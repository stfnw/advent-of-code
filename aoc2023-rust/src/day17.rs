// https://adventofcode.com/2023/day/17

use std::collections::{BinaryHeap, HashSet};
use std::fs;

#[derive(Clone, Debug)]
struct Arr<T> {
    arr: Vec<T>,
    rows: usize,
    cols: usize,
}

impl<T> Arr<T> {
    fn get2(&self, r: usize, c: usize) -> &T {
        &self.arr[self.idx2(r, c)]
    }
    fn idx2(&self, r: usize, c: usize) -> usize {
        r * self.cols + c
    }
}

impl<T: std::fmt::Display> std::fmt::Display for Arr<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for r in 0..self.rows {
            for c in 0..self.cols {
                write!(f, "{}", self.get2(r, c))?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, PartialEq, Eq)]
struct ParseArrError;

impl std::str::FromStr for Arr<u8> {
    type Err = ParseArrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let arr: Vec<_> = s
            .chars()
            .filter(|&c| c != '\n')
            .map(|c| c.to_digit(10))
            .map(Option::unwrap)
            .map(|x| x as u8)
            .collect();

        let (rows, cols) = (
            s.lines().count(),
            s.chars().position(|c| c == '\n').ok_or(ParseArrError)?,
        );

        Ok(Arr { arr, rows, cols })
    }
}

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day17.txt").unwrap();

    let arr: Arr<u8> = content.parse().unwrap();
    let res = shortest_path(&arr).unwrap();

    println!("Day17: {}", res);
}

// https://doc.rust-lang.org/std/collections/binary_heap/index.html
#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
struct Node {
    position: (usize, usize),
    last_dir: (i8, i8),
    last_dir_count: usize,
}

#[derive(Copy, Clone, Eq, PartialEq)]
struct State {
    cost: u64,
    node: Node,
}

// Min-Heap by cost.
impl Ord for State {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.node.cmp(&other.node))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

// Dijkstra's algorithm on a grid.
fn shortest_path(grid: &Arr<u8>) -> Option<u64> {
    let mut queue = BinaryHeap::new();
    queue.push(State {
        cost: 0,
        node: Node {
            position: (0, 0),
            last_dir: (0, 0),
            last_dir_count: 0,
        },
    });

    let mut visited = HashSet::new();

    while let Some(State { cost, node }) = queue.pop() {
        if node.position.0 == grid.rows - 1 && node.position.1 == grid.cols - 1 {
            return Some(cost);
        }

        if visited.contains(&node) {
            continue;
        }
        visited.insert(node);

        for nlast_dir in [(0, -1), (0, 1), (-1, 0), (1, 0)] {
            // Check whether move is valid based on last_dir: can't go reversed.
            match (node.last_dir, nlast_dir) {
                ((0, -1), (0, 1)) => continue,
                ((0, 1), (0, -1)) => continue,
                ((-1, 0), (1, 0)) => continue,
                ((1, 0), (-1, 0)) => continue,
                _ => (),
            }

            // Check whether move is valid based on boundaries of the grid.
            let (nr, nc) = (
                node.position.0 as i32 + nlast_dir.0 as i32,
                node.position.1 as i32 + nlast_dir.1 as i32,
            );
            if !(0 <= nr && nr < grid.rows as i32 && 0 <= nc && nc < grid.cols as i32) {
                continue;
            }
            let (nr, nc) = (nr as usize, nc as usize);

            let nposition = (nr, nc);
            let nlast_dir_count = if nlast_dir == node.last_dir {
                node.last_dir_count + 1
            } else {
                1
            };
            let ncost = cost.saturating_add(*grid.get2(nr, nc) as u64);

            // Check whether move is valid based on being able to move at most
            // three blocks in a single direction.
            if nlast_dir_count >= 4 {
                continue;
            }

            // Note that at this point we unconditionally add the new state to
            // the priority queue. We do not compare against the current best
            // cost/distance, since we do not search for the absolute best path,
            // but the best path that satisfies some history/path-dependent
            // condition.

            queue.push(State {
                cost: ncost,
                node: Node {
                    position: nposition,
                    last_dir: nlast_dir,
                    last_dir_count: nlast_dir_count,
                },
            });
        }
    }

    None
}
