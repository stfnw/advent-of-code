// https://adventofcode.com/2023/day/16

use std::collections::HashSet;
use std::fs;

#[derive(Clone, Debug)]
struct Arr {
    arr: Vec<char>,
    rows: usize,
    cols: usize,
}

impl Arr {
    fn get2(&self, r: usize, c: usize) -> char {
        self.arr[self.idx2(r, c)]
    }
    fn idx2(&self, r: usize, c: usize) -> usize {
        r * self.cols + c
    }
}

impl std::fmt::Display for Arr {
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

impl std::str::FromStr for Arr {
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

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day16.txt").unwrap();

    let arr: Arr = content.parse().unwrap();
    let res = count_energized(&arr);

    println!("Day16: {}", res);
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
enum Dir {
    L,
    R,
    U,
    D,
}

impl std::convert::From<Dir> for (i32, i32) {
    fn from(dir: Dir) -> Self {
        match dir {
            Dir::L => (0, -1),
            Dir::R => (0, 1),
            Dir::U => (-1, 0),
            Dir::D => (1, 0),
        }
    }
}

fn count_energized(arr: &Arr) -> usize {
    // Keep track of visited nodes to prevent loops. We also have to keep track
    // of the moving direction since different moving directions may lead to
    // differen paths in the future.
    // (pos, direction of moving)
    let mut visited: HashSet<(usize, usize, Dir)> = HashSet::new();

    let mut stack = Vec::new();
    stack.push((0, 0, Dir::R));

    while let Some((r, c, dir)) = stack.pop() {
        if visited.contains(&(r, c, dir.clone())) {
            continue;
        }
        visited.insert((r, c, dir.clone()));

        let nextdirs: Vec<Dir> = match (arr.get2(r, c), dir) {
            ('.', d) => vec![d],
            //
            ('|', Dir::L) => vec![Dir::U, Dir::D],
            ('|', Dir::R) => vec![Dir::U, Dir::D],
            ('|', Dir::U) => vec![Dir::U],
            ('|', Dir::D) => vec![Dir::D],
            //
            ('-', Dir::L) => vec![Dir::L],
            ('-', Dir::R) => vec![Dir::R],
            ('-', Dir::D) => vec![Dir::L, Dir::R],
            ('-', Dir::U) => vec![Dir::L, Dir::R],
            //
            ('\\', Dir::L) => vec![Dir::U],
            ('\\', Dir::R) => vec![Dir::D],
            ('\\', Dir::D) => vec![Dir::R],
            ('\\', Dir::U) => vec![Dir::L],
            //
            ('/', Dir::L) => vec![Dir::D],
            ('/', Dir::R) => vec![Dir::U],
            ('/', Dir::D) => vec![Dir::L],
            ('/', Dir::U) => vec![Dir::R],
            //
            (chr, d) => panic!(
                "Unexpected char {} at position {:?} while moving to {:?}",
                chr,
                (r, c),
                d
            ),
        };

        for ndir in nextdirs.into_iter() {
            let diff: (i32, i32) = ndir.clone().into();
            let (nr, nc) = (r as i32 + diff.0, c as i32 + diff.1);

            if !(0 <= nr && nr < arr.rows as i32 && 0 <= nc && nc < arr.cols as i32) {
                continue;
            }

            stack.push((nr as usize, nc as usize, ndir));
        }
    }

    // Project down the visited nodes to only positions (regardless of direction
    // we were moving in).
    let energized: HashSet<(usize, usize)> = visited.into_iter().map(|(r, c, _)| (r, c)).collect();

    energized.len()
}
