// https://adventofcode.com/2023/day/11

use std::fs;

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day11.txt").unwrap();

    let (rows, cols) = (
        content.split('\n').collect::<Vec<_>>().len() - 1,
        content.chars().position(|c| c == '\n').unwrap(),
    );

    let idx2 = |r: usize, c: usize| r * cols + c;
    let idx1 = |i: usize| (i / cols, i % cols);

    let arr: Vec<_> = content.chars().filter(|&c| c != '\n').collect();

    let galaxies: Vec<_> = arr
        .iter()
        .enumerate()
        .filter_map(|(i, &c)| if c == '#' { Some(idx1(i)) } else { None })
        .collect();

    let mut empty_rows = Vec::new();
    for i in 0..rows {
        let mut is_empty = true;
        for j in 0..cols {
            is_empty &= arr[idx2(i, j)] == '.';
        }
        if is_empty {
            empty_rows.push(i);
        }
    }

    let mut empty_cols = Vec::new();
    for i in 0..cols {
        let mut is_empty = true;
        for j in 0..rows {
            is_empty &= arr[idx2(j, i)] == '.';
        }
        if is_empty {
            empty_cols.push(i);
        }
    }

    let mut dists = Vec::new();

    for g1i in 0..galaxies.len() {
        for g2i in (g1i + 1)..galaxies.len() {
            let (g1, g2) = (galaxies[g1i], galaxies[g2i]);
            let mut d = dist(g1, g2);

            // apply stretches in rows
            let (minr, maxr) = (std::cmp::min(g1.0, g2.0), std::cmp::max(g1.0, g2.0));
            for er in empty_rows.iter() {
                if minr < *er && *er < maxr {
                    d += 1;
                }
            }

            // apply stretches in cols
            let (minc, maxc) = (std::cmp::min(g1.1, g2.1), std::cmp::max(g1.1, g2.1));
            for ec in empty_cols.iter() {
                if minc < *ec && *ec < maxc {
                    d += 1;
                }
            }

            dists.push(d);
        }
    }

    let sum: usize = dists.into_iter().sum();
    println!("Day11: {}", sum);
}

fn dist((r1, c1): (usize, usize), (r2, c2): (usize, usize)) -> usize {
    ((r2 as i64 - r1 as i64).abs() + (c2 as i64 - c1 as i64).abs()) as usize
}
