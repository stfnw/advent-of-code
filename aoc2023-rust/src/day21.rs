// https://adventofcode.com/2023/day/21

use std::collections::HashSet;
use std::fs;

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day21.txt").unwrap();

    let grid: Arr<char> = content.parse().unwrap();
    let (r, c) = grid.idx1(grid.arr.iter().position(|&x| x == 'S').unwrap());

    let mut stack = Vec::new();
    stack.push((r, c));

    for _ in 0..64 {
        let mut nstack = HashSet::new();
        while let Some((r, c)) = stack.pop() {
            let dirs = [(0, -1), (0, 1), (-1, 0), (1, 0)];
            let npos: Vec<_> = dirs
                .iter()
                .filter(|(i, j)| {
                    0 <= r as i64 + i
                        && r as i64 + i < grid.rows as i64
                        && 0 <= c as i64 + j
                        && c as i64 + j < grid.cols as i64
                })
                .map(|(i, j)| ((r as i64 + i) as usize, (c as i64 + j) as usize))
                .filter(|(i, j)| *grid.get2(*i, *j) != '#')
                .collect();
            nstack.extend(npos);
        }

        stack = nstack.drain().collect();
    }

    let res = stack.len();

    println!("Day21: {}", res);
}

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
    fn idx1(&self, i: usize) -> (usize, usize) {
        (i / self.cols, i % self.cols)
    }
}

impl std::fmt::Display for Arr<char> {
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

impl std::str::FromStr for Arr<char> {
    type Err = ParseArrError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let arr: Vec<_> = s.chars().filter(|&c| c != '\n').collect();

        let (rows, cols) = (
            s.lines().count(),
            s.chars().position(|c| c == '\n').ok_or(ParseArrError)?,
        );

        Ok(Arr { arr, rows, cols })
    }
}
