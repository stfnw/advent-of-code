// https://adventofcode.com/2023/day/20

use std::collections::{HashMap, VecDeque};
use std::fs;

#[derive(Debug)]
struct Day20Error;

pub fn run() {
    run_().unwrap();
}

fn run_() -> Result<(), Day20Error> {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day20.txt").unwrap();

    // Adjacency list of the circuit graph.
    let mut circuit: HashMap<String, Module> = HashMap::new();
    let mut flipflop_states: HashMap<String, bool> = HashMap::new();
    let mut conjunction_states: HashMap<String, Vec<(String, bool)>> = HashMap::new();

    for line in content.lines() {
        let tmp: Vec<_> = line.split(" -> ").collect();
        let outputs: Vec<_> = tmp[1].split(", ").map(str::to_string).collect();

        let type_name = if tmp[0] == "broadcaster" {
            Ok((Type::Broadcast, "broadcaster"))
        } else if let Some(name) = tmp[0].strip_prefix('%') {
            // Remember previous state: initially off.
            flipflop_states.insert(name.to_string(), false);
            Ok((Type::FlipFlop, name))
        } else if let Some(name) = tmp[0].strip_prefix('&') {
            conjunction_states.insert(name.to_string(), Vec::new());
            Ok((Type::Conjunction, name))
        } else {
            Err(Day20Error)
        };

        let type_name = type_name?;

        circuit.insert(
            type_name.1.to_string(),
            Module {
                mtype: type_name.0,
                outputs,
            },
        );
    }

    // Conjunction: Remember previous pulse from each input (initially low)
    for (name, module) in circuit.iter() {
        for output in module.outputs.iter() {
            if let Some(nmodule) = circuit.get(output) {
                if nmodule.mtype == Type::Conjunction {
                    conjunction_states
                        .get_mut(output)
                        .unwrap()
                        .push((name.to_string(), false));
                }
            }
        }
    }

    let (mut nlowpulses, mut nhighpulses) = (0, 0);

    // Queue of open commands/pulses that still need to be processed.
    let mut queue = VecDeque::new();

    // One thousand button presses.
    for _ in 0..1000 {
        // A button press sends a single low pulse to the broadcaster.
        queue.push_back(Cmd {
            from: "button".to_string(),
            pulse: false,
            to: "broadcaster".to_string(),
        });

        while let Some(cmd) = queue.pop_front() {
            match cmd.pulse {
                false => nlowpulses += 1,
                true => nhighpulses += 1,
            }

            if let Some(module) = circuit.get(&cmd.to) {
                // Update module state and determine pulse it sends out.
                let npulse = match module.mtype {
                    Type::Broadcast => Some(cmd.pulse),
                    Type::FlipFlop => {
                        match (cmd.pulse, flipflop_states.get_mut(&cmd.to).unwrap()) {
                            (false, state) => {
                                *state ^= true;
                                Some(*state)
                            }
                            _ => None,
                        }
                    }
                    Type::Conjunction => {
                        let state = conjunction_states.get_mut(&cmd.to).unwrap();
                        let mut npulse = true;
                        for (frommodule, lastpulse) in state.iter_mut() {
                            if *frommodule == cmd.from {
                                *lastpulse = cmd.pulse;
                            }
                            npulse &= *lastpulse;
                        }
                        Some(!npulse)
                    }
                };

                // Send new pulse to all the outputs of the module.
                if let Some(npulse) = npulse {
                    for output in module.outputs.iter() {
                        queue.push_back(Cmd {
                            from: cmd.to.clone(),
                            pulse: npulse,
                            to: output.to_string(),
                        });
                    }
                }
            }
        }
    }

    println!("Day20: {}", nlowpulses * nhighpulses);

    Ok(())
}

#[derive(Debug)]
struct Module {
    mtype: Type,
    outputs: Vec<String>,
}

#[derive(Debug, Eq, PartialEq)]
enum Type {
    Broadcast,
    FlipFlop,
    Conjunction,
}

#[derive(Debug)]
struct Cmd {
    from: String,
    pulse: bool,
    to: String,
}
