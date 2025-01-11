// https://adventofcode.com/2023/day/12

use std::fs;

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day12.txt").unwrap();

    let mut sum = 0;
    for line in content.lines() {
        let tmp: Vec<_> = line.split(' ').collect();
        let (mut arrangement, groups): (Vec<_>, Vec<_>) = (
            tmp[0].chars().collect(),
            tmp[1]
                .split(',')
                .map(str::parse::<u64>)
                .map(Result::unwrap)
                .collect(),
        );

        sum += count_arrangements(&mut arrangement, &groups);
    }

    println!("Day12: {}", sum);
}

fn count_arrangements(partial: &mut [char], groups: &[u64]) -> u64 {
    backtrack(partial, groups, 0)
}

fn backtrack(partial: &mut [char], groups: &[u64], idx: usize) -> u64 {
    if idx == partial.len() {
        return if is_valid(partial, groups) { 1 } else { 0 };
    }

    if !can_be_valid(partial, idx, groups) {
        return 0;
    }

    let mut res = 0;
    let c = partial[idx];
    if c == '.' || c == '?' {
        partial[idx] = '.';
        res += backtrack(partial, groups, idx + 1);
    }
    if c == '#' || c == '?' {
        partial[idx] = '#';
        res += backtrack(partial, groups, idx + 1);
    }
    partial[idx] = c;
    res
}

fn is_valid(arrangement: &[char], groups: &[u64]) -> bool {
    get_groups(arrangement, None) == groups
}

fn get_groups(arrangement: &[char], upto: Option<usize>) -> Vec<u64> {
    let upto = upto.unwrap_or(arrangement.len());

    let mut res = Vec::new();
    let mut curgroup = 0;

    for &c in arrangement[..upto].iter() {
        assert!(c == '#' || c == '.');

        if c == '#' {
            curgroup += 1;
        }

        if c == '.' && curgroup > 0 {
            res.push(curgroup);
            curgroup = 0;
        }
    }

    if curgroup > 0 {
        res.push(curgroup);
    }

    res
}

fn can_be_valid(arrangement: &[char], idx: usize, groups: &[u64]) -> bool {
    let g = get_groups(arrangement, Some(idx));

    if g.len() > groups.len() {
        return false;
    }

    for (c1, c2) in g.iter().zip(groups.iter()) {
        if c1 > c2 {
            return false;
        }
    }

    true
}
