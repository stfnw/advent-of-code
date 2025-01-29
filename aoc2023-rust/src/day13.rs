// https://adventofcode.com/2023/day/13

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

    fn transpose(&self) -> Self {
        let mut arr = Vec::new();

        for c in 0..self.cols {
            for r in 0..self.rows {
                arr.push(self.get2(r, c));
            }
        }

        Self {
            arr,
            rows: self.cols,
            cols: self.rows,
        }
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

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day13.txt").unwrap();

    let mut sum = 0;

    for arr in content.split("\n\n") {
        let (rows, cols) = (
            arr.lines().count(),
            arr.chars().position(|c| c == '\n').unwrap(),
        );

        let arr = Arr {
            arr: arr.chars().filter(|&c| c != '\n').collect(),
            rows,
            cols,
        };

        // The trick with transposing and using the same check-function twice is from
        // https://github.com/Hamatti/adventofcode-2023/blob/main/src/day_13.ipynb
        let left_cols = find_vmirror(&arr, 0).map(|x| x + 1);
        let top_rows = find_vmirror(&arr.transpose(), 0).map(|x| x + 1);

        assert!( !(left_cols.is_some() && top_rows.is_some()),
                "Found both vertical mirror with left offset {:?} and horizontal mirror with top offset {:?}; don't know which to incorporate into the sum",
                left_cols, top_rows
        );

        assert!(
            left_cols.is_some() || top_rows.is_some(),
            "Could not find a mirror line in the following block:\n{}",
            arr
        );

        sum += left_cols.unwrap_or(0);
        sum += top_rows.unwrap_or(0) * 100;
    }

    println!("Day13: {}", sum);
}

fn find_vmirror(block: &Arr, errthreshold: usize) -> Option<usize> {
    (0..block.cols).find(|&c| is_vmirror(block, c, errthreshold))
}

fn is_vmirror(block: &Arr, c: usize, errthreshold: usize) -> bool {
    if c == block.cols - 1 {
        return false;
    }

    let mut nerr = 0;

    for r in 0..block.rows {
        let mut i = 0;
        while i <= c && c + i + 1 < block.cols {
            nerr += if block.get2(r, c - i) != block.get2(r, c + i + 1) {
                1
            } else {
                0
            };
            i += 1;
        }
    }

    nerr == errthreshold
}
