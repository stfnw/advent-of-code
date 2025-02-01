// https://adventofcode.com/2023/day/15

use std::fs;

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day15.txt").unwrap();

    let sum: usize = content
        .split(',')
        .map(str::trim)
        .map(hash_str)
        .map(|x| x as usize)
        .sum();

    println!("Day15: {}", sum);
}

fn hash_str(s: &str) -> u8 {
    let mut res: u8 = 0;
    for c in s.chars() {
        res = res.wrapping_add(c as u8);
        res = res.wrapping_mul(17);
    }
    res
}
