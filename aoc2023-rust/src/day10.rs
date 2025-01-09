// https://adventofcode.com/2023/day/10

use std::collections::HashSet;
use std::fmt;
use std::fs;

#[derive(Clone, Debug)]
enum Dir {
    Up,
    Down,
    Left,
    Right,
}

#[derive(Debug)]
struct Grid {
    grid: Vec<char>,
    xs: usize,
    ys: usize,
}

impl Grid {
    fn get(&self, pos: &Pos) -> Option<char> {
        if pos.x < self.xs && pos.y < self.ys {
            Some(self.grid[pos.y * self.xs + pos.x])
        } else {
            None
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct Pos {
    x: usize,
    y: usize,
}

impl Pos {
    fn add(&self, dir: &Dir) -> Option<Self> {
        #[rustfmt::skip]
        let (x, y): (i32, i32) = match dir {
            Dir::Up    => (self.x as i32,     self.y as i32 - 1),
            Dir::Down  => (self.x as i32,     self.y as i32 + 1),
            Dir::Left  => (self.x as i32 - 1, self.y as i32    ),
            Dir::Right => (self.x as i32 + 1, self.y as i32    ),
        };

        if 0 <= x && 0 <= y {
            Some(Self {
                x: x as usize,
                y: y as usize,
            })
        } else {
            None
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}/{})", self.x, self.y)
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day10.txt").unwrap();

    let (cols, rows) = (content.find('\n').unwrap(), content.lines().count());

    let grid = Grid {
        grid: content.replace("\n", "").chars().collect(),
        xs: cols,
        ys: rows,
    };
    assert!(grid.grid.len() == grid.xs * grid.ys, "Index mismatch");

    let start_idx = grid.grid.iter().position(|&x| x == 'S').unwrap();

    let pos = Pos {
        x: start_idx % cols,
        y: start_idx / cols,
    };

    let mut visited = HashSet::new();
    visited.insert(pos.clone());
    dfs(&grid, &pos, &mut visited);

    println!("Day10: {}", (visited.len() + 1) / 2);
}

fn dfs(grid: &Grid, pos: &Pos, visited: &mut HashSet<Pos>) {
    for dir in [Dir::Up, Dir::Down, Dir::Left, Dir::Right] {
        if can_go(grid, visited, pos, &dir) {
            let new_pos = pos.add(&dir).unwrap();

            visited.insert(new_pos.clone());
            dfs(grid, &new_pos, visited);
        }
    }
}

fn can_go(grid: &Grid, visited: &mut HashSet<Pos>, pos: &Pos, dir: &Dir) -> bool {
    let Some(new_pos) = pos.add(dir) else {
        return false;
    };

    if visited.contains(&new_pos) {
        return false;
    }

    let Some(c) = grid.get(pos) else {
        return false;
    };
    let Some(new_c) = grid.get(&new_pos) else {
        return false;
    };

    match (c, dir, new_c) {
        ('.', _, _) /***********/ => false,
        (_, _, '.') /***********/ => false,
        ('|', Dir::Up /*****/, _) => true,
        ('|', Dir::Down /***/, _) => true,
        ('-', Dir::Left /***/, _) => true,
        ('-', Dir::Right /**/, _) => true,
        ('L', Dir::Up /*****/, _) => true,
        ('L', Dir::Right /**/, _) => true,
        ('J', Dir::Up /*****/, _) => true,
        ('J', Dir::Left /***/, _) => true,
        ('7', Dir::Down /***/, _) => true,
        ('7', Dir::Left /***/, _) => true,
        ('F', Dir::Down /***/, _) => true,
        ('F', Dir::Right /**/, _) => true,
        ('S', Dir::Up /*****/, _) => true,
        ('S', Dir::Down /***/, _) => true,
        ('S', Dir::Left /***/, _) => true,
        ('S', Dir::Right /**/, _) => true,
        _ /*********************/ => false,
    }
}
