// https://adventofcode.com/2023/day/1

use std::fs;

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day1.txt").unwrap();

    let mut sum = 0;

    for line in content.lines() {
        let first: u32 = line.chars().find_map(|x| x.to_digit(10)).unwrap();
        let last: u32 = line.chars().rev().find_map(|x| x.to_digit(10)).unwrap();

        sum += first * 10 + last;
    }

    println!("Day1: {}", sum);
}
