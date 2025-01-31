// https://adventofcode.com/2023/day/14

use std::fs;

#[derive(Debug)]
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
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day14.txt").unwrap();

    let sum: usize = content
        .split("\n\n")
        .map(str::parse::<Arr>)
        .map(Result::unwrap)
        .map(process_arr)
        .sum();

    println!("Day14: {}", sum);
}

fn process_arr(arr: Arr) -> usize {
    let mut res = 0;

    for c in 0..arr.cols {
        let mut nextfree = 0;
        for r in 0..arr.rows {
            match arr.get2(r, c) {
                'O' => {
                    res += arr.rows - nextfree;
                    nextfree += 1;
                }
                '#' => nextfree = r + 1,
                '.' => (),
                chr => panic!("Unexpected char {}", chr),
            }
        }
    }

    res
}
