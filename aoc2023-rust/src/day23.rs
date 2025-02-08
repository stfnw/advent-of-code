// https://adventofcode.com/2023/day/23

use std::collections::HashSet;
use std::fs;

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day23.txt").unwrap();

    let grid: Arr<char> = content.parse().unwrap();

    let mut max_len = 0;
    let start = (0, 1);

    let mut visited = HashSet::new();
    visited.insert(start);

    let mut stack = Vec::new();
    stack.push((start, visited));

    while let Some(((r, c), visited)) = stack.pop() {
        if r == grid.rows - 1 && c == grid.cols - 1 - 1 {
            // let mut tmp = grid.clone();
            // for (r, c) in visited.iter() {
            //     tmp.set2(*r, *c, 'O');
            // }
            // println!("visualize path of len {}\n{}", visited.len(), tmp);

            max_len = std::cmp::max(max_len, visited.len());
        }

        let dirs = match grid.get2(r, c) {
            '<' => vec![(0, -1)],
            '>' => vec![(0, 1)],
            '^' => vec![(-1, 0)],
            'v' => vec![(1, 0)],
            _ => vec![(0, -1), (0, 1), (-1, 0), (1, 0)],
        };

        for (i, j) in dirs.iter() {
            if !(0 <= r as i64 + i
                && r as i64 + i < grid.rows as i64
                && 0 <= c as i64 + j
                && c as i64 + j < grid.cols as i64)
            {
                continue;
            }

            let (nr, nc) = ((r as i64 + i) as usize, (c as i64 + j) as usize);

            if visited.contains(&(nr, nc)) {
                continue;
            }

            if *grid.get2(nr, nc) == '#' {
                continue;
            }

            let mut nvisited = visited.clone();
            nvisited.insert((nr, nc));
            stack.push(((nr, nc), nvisited));
        }
    }

    println!("Day23: {}", max_len - 1);
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

        assert!(arr[1] == '.');
        assert!(arr[arr.len() - 1 - 1] == '.');

        Ok(Arr { arr, rows, cols })
    }
}
