// https://adventofcode.com/2023/day/19

use std::collections::HashMap;
use std::fs;

#[derive(Debug)]
struct Workflow {
    name: String,
    rules: Vec<Rule>,
}

#[derive(Debug)]
enum Rule {
    Cond {
        rating: Rating,
        op: Op,
        val: u64,
        nworkflow: String,
    },
    Uncond {
        nworkflow: String,
    },
}

#[derive(Clone, Debug)]
enum Rating {
    X,
    M,
    A,
    S,
}

#[derive(Debug)]
enum Op {
    Lt,
    Gt,
}

impl Op {
    fn satisfies(&self, val1: u64, val2: u64) -> bool {
        match self {
            Op::Lt => val1 < val2,
            Op::Gt => val1 > val2,
        }
    }
}

#[derive(Debug)]
struct Part {
    x: u64,
    m: u64,
    a: u64,
    s: u64,
}

impl Part {
    fn get(&self, rating: Rating) -> u64 {
        match rating {
            Rating::X => self.x,
            Rating::M => self.m,
            Rating::A => self.a,
            Rating::S => self.s,
        }
    }

    fn val_sum(&self) -> u64 {
        self.x + self.m + self.a + self.s
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ParseError(String);

impl std::str::FromStr for Workflow {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let i = s
            .find('{')
            .ok_or(ParseError(format!("Workflow: {}: Doesn't contain '{{'", s)))?;
        let name = s[..i].to_string();

        if !s.ends_with('}') {
            return Err(ParseError(format!(
                "Workflow: {}: Doesn't end with '}}'",
                s
            )));
        }

        let mut rules: Vec<_> = Vec::new();
        for rule in s[(i + 1)..s.len() - 1].split(',') {
            rules.push(str::parse(rule)?);
        }

        Ok(Workflow { name, rules })
    }
}

impl std::str::FromStr for Rule {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.contains(':') {
            // parse conditional form
            let rating = s[0..1].parse()?;
            let op = s[1..2].parse()?;
            let tmp: Vec<_> = s.split(':').collect();
            let val = tmp[0][2..]
                .parse()
                .map_err(|_| ParseError(format!("Rule: {}: Can't parse as int {}", s, tmp[0])))?;
            let nworkflow = tmp[1].to_string();
            Ok(Rule::Cond {
                rating,
                op,
                val,
                nworkflow,
            })
        } else {
            // parse unconditional form
            Ok(Rule::Uncond {
                nworkflow: s.to_string(),
            })
        }
    }
}

impl std::str::FromStr for Rating {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "x" => Ok(Rating::X),
            "m" => Ok(Rating::M),
            "a" => Ok(Rating::A),
            "s" => Ok(Rating::S),
            _ => Err(ParseError(format!("Rating: {}: Invalid attribute", s))),
        }
    }
}

impl std::str::FromStr for Op {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "<" => Ok(Op::Lt),
            ">" => Ok(Op::Gt),
            _ => Err(ParseError(format!("Op: {}: Invalid operator", s))),
        }
    }
}

impl std::str::FromStr for Part {
    type Err = ParseError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if !s.starts_with('{') || !s.ends_with('}') {
            return Err(ParseError(format!(
                "Part: {}: Doesn't start with '{{' or end with '}}'",
                s
            )));
        }

        let s = &s[1..(s.len() - 1)];

        let tmp: Vec<_> = s.split(',').collect();

        for (i, item) in tmp.iter().enumerate().take(4) {
            let a = item.chars().next();
            if a != Some(['x', 'm', 'a', 's'][i]) {
                return Err(ParseError(format!(
                    "Part: {}: Invalid attribute {:?}",
                    s, a
                )));
            }

            let b = item.chars().nth(1);
            if b != Some('=') {
                return Err(ParseError(format!(
                    "Part: {}: Expected '=', got {:?}",
                    s, b
                )));
            }
        }

        Ok(Part {
            x: tmp[0][2..]
                .parse()
                .map_err(|_| ParseError(format!("Part: {}: Can't parse int {}", s, tmp[0])))?,
            m: tmp[1][2..]
                .parse()
                .map_err(|_| ParseError(format!("Part: {}: Can't parse int {}", s, tmp[1])))?,
            a: tmp[2][2..]
                .parse()
                .map_err(|_| ParseError(format!("Part: {}: Can't parse int {}", s, tmp[2])))?,
            s: tmp[3][2..]
                .parse()
                .map_err(|_| ParseError(format!("Part: {}: Can't parse int {}", s, tmp[3])))?,
        })
    }
}

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day19.txt").unwrap();

    let tmp: Vec<_> = content.split("\n\n").collect();

    let mut workflows = HashMap::new();
    for Workflow { name, rules } in tmp[0].lines().map(|w| w.trim().parse().unwrap()) {
        workflows.insert(name, rules);
    }

    let parts: Vec<Part> = tmp[1].lines().map(|p| p.parse().unwrap()).collect();

    let mut sum = 0;

    for part in parts.iter() {
        let mut workflow = "in".to_string();
        while let Some(rules) = workflows.get(&workflow) {
            if workflow == "A" || workflow == "R" {
                break;
            }

            for rule in rules.iter() {
                match rule {
                    Rule::Cond {
                        rating,
                        op,
                        val,
                        nworkflow,
                    } => {
                        if op.satisfies(part.get(rating.clone()), *val) {
                            workflow = nworkflow.clone();
                            break;
                        }
                    }
                    Rule::Uncond { nworkflow } => {
                        workflow = nworkflow.clone();
                        break;
                    }
                }
            }
        }

        if workflow == "A" {
            sum += part.val_sum();
        }
    }

    println!("Day19: {}", sum);
}
