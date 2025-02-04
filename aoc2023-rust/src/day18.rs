// https://adventofcode.com/2023/day/18

use std::fs;

struct Cmd {
    dir: Dir,
    n: usize,
    _color: String,
}

enum Dir {
    L,
    R,
    U,
    D,
}

#[derive(Debug, PartialEq, Eq)]
struct ParseError;

impl std::str::FromStr for Dir {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "L" => Ok(Dir::L),
            "R" => Ok(Dir::R),
            "U" => Ok(Dir::U),
            "D" => Ok(Dir::D),
            _ => Err(ParseError),
        }
    }
}

impl std::str::FromStr for Cmd {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let tmp: Vec<_> = s.split(' ').map(str::trim).collect();

        let dir: Dir = tmp[0].parse()?;
        let n: usize = tmp[1].parse().map_err(|_| ParseError)?;
        let _color = tmp[2].to_string();

        Ok(Self { dir, n, _color })
    }
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
    fn set2(&mut self, r: usize, c: usize, val: T) {
        let idx = self.idx2(r, c);
        self.arr[idx] = val;
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

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day18.txt").unwrap();

    let cmds: Vec<_> = content
        .lines()
        .map(str::parse::<Cmd>)
        .map(Result::unwrap)
        .collect();

    let mut arr = draw_grid(&cmds);
    let res = flood_fill(&mut arr);

    println!("Day18: {}", res);
}

/// Determine dimensions and draw grid of the trench dug into the ground with 1
/// unit of padding in each direction so that the later flood-fill
/// implementation becomes easier.
fn draw_grid(cmds: &[Cmd]) -> Arr<char> {
    let (mut r, mut c): (i64, i64) = (0, 0);
    let (mut minr, mut maxr, mut minc, mut maxc) = (0, 0, 0, 0);

    // All points in the path / closed loop (the start point is not again
    // included at the end).
    let mut points = Vec::new();

    for cmd in cmds.iter() {
        for _ in 0..cmd.n {
            points.push((r, c));
            match cmd.dir {
                Dir::L => c -= 1,
                Dir::R => c += 1,
                Dir::U => r -= 1,
                Dir::D => r += 1,
            }
        }

        minr = std::cmp::min(r, minr);
        maxr = std::cmp::max(r, maxr);
        minc = std::cmp::min(c, minc);
        maxc = std::cmp::max(c, maxc);
    }

    assert!(
        (r, c) == (0, 0),
        "Trench is not a closed loop! {:?}",
        (r, c)
    );

    let padding = 1;
    let (rows, cols) = (
        (maxr + 1 - minr) as usize + 2 * padding,
        (maxc + 1 - minc) as usize + 2 * padding,
    );
    assert!(
        minr <= 0 && minc <= 0,
        "Unexpected value of minr={} and minc={}",
        minr,
        minc,
    );

    let points: Vec<_> = points
        .into_iter()
        .map(|(r, c)| {
            (
                (r + (-minr)) as usize + padding,
                (c + (-minc)) as usize + padding,
            )
        })
        .collect();

    let mut arr = Arr {
        arr: vec!['.'; rows * cols],
        rows,
        cols,
    };

    for (r, c) in points.into_iter() {
        arr.set2(r, c, '#')
    }

    arr
}

fn flood_fill(arr: &mut Arr<char>) -> usize {
    // Because of the padding the value (0,0) will always be outside of the loop
    // and is a suitable starting point for the flood-fill of the outer area.

    let mut stack: Vec<(usize, usize)> = Vec::new();
    stack.push((0, 0));

    // First we flood-fill the outer area with '-'.
    while let Some((r, c)) = stack.pop() {
        if *arr.get2(r, c) != '.' {
            continue;
        }

        let dirs = [(0, -1), (0, 1), (-1, 0), (1, 0)];
        let valid_npos: Vec<_> = dirs
            .iter()
            .filter(|(i, j)| {
                0 <= r as i64 + i
                    && r as i64 + i < arr.rows as i64
                    && 0 <= c as i64 + j
                    && c as i64 + j < arr.cols as i64
            })
            .map(|(i, j)| ((r as i64 + i) as usize, (c as i64 + j) as usize))
            .collect();

        arr.set2(r, c, '-');
        for (nr, nc) in valid_npos.into_iter() {
            stack.push((nr, nc));
        }
    }

    // Then we fill the inner area with the same value as the border '#'.
    for i in 0..(arr.rows * arr.cols) {
        if arr.arr[i] == '.' {
            arr.arr[i] = '#';
        }
    }

    // Then we count the values of the inner area.
    arr.arr.iter().filter(|c| **c == '#').count()
}
