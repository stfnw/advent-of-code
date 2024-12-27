// https://adventofcode.com/2023/day/2

use std::fs;

struct Cube {
    red: u32,
    green: u32,
    blue: u32,
}

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day2.txt").unwrap();

    let mut sum_ids = 0;

    for line in content.lines() {
        let fields: Vec<_> = line.split(": ").collect();
        let gameid: u32 = fields[0].split(" ").collect::<Vec<_>>()[1].parse().unwrap();

        if is_valid_game(fields[1]) {
            sum_ids += gameid;
        }
    }

    println!("Day2: {}", sum_ids);
}

fn is_valid_game(inp: &str) -> bool {
    let mut is_valid = true;

    for cs in inp.split("; ") {
        let mut cube = Cube {
            red: 0,
            green: 0,
            blue: 0,
        };

        for c in cs.split(", ") {
            let mut it = c.split(" ");
            let n: u32 = it.next().unwrap().parse().unwrap();

            let color = it.next().unwrap();
            match color {
                "red" => cube.red += n,
                "green" => cube.green += n,
                "blue" => cube.blue += n,
                _ => panic!("Invalid color"),
            }
        }

        is_valid &= cube.red <= 12 && cube.green <= 13 && cube.blue <= 14;
    }

    is_valid
}
