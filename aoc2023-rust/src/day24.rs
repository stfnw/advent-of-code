// https://adventofcode.com/2023/day/24

use std::fs;

pub fn run() {
    let content = fs::read_to_string("../../advent-of-code-data/aoc2023/Day24.txt").unwrap();

    let mut lines: Vec<Line> = Vec::new();
    for line in content.lines() {
        lines.push(line.parse().unwrap());
    }

    let (min, max): (f64, f64) = (200_000_000_000_000.0, 400_000_000_000_000.0);
    let mut n = 0;
    for i in 0..lines.len() {
        for j in (i + 1)..lines.len() {
            n += if lines[i].intersects(&lines[j], min, max) {
                1
            } else {
                0
            };
        }
    }

    println!("Day24: {}", n);
}

#[derive(Debug)]
struct Line {
    p: Point,
    v: Point,
}

impl Line {
    fn new(p: Point, v: Point) -> Line {
        Line { p, v }
    }

    // Solve system of linear equations:
    // (https://en.wikipedia.org/wiki/Intersection_(geometry)#Two_lines)
    // p1 + v1 * s = p2 + v2 * t
    //
    // v1 * s + (-v2) * t = p2 - p1
    // __       ____        ______
    //  a         b            c
    fn intersects(&self, other: &Line, min: f64, max: f64) -> bool {
        let a = self.v;
        let b = -other.v;
        let c = other.p - self.p;

        let tmp = a.y * b.x - a.x * b.y;
        if approx_equal(tmp, 0.0) {
            // Lines are parallel.

            // Check whether arbitrary point on self is also on other:
            let tx = (self.p.x - other.p.x) / other.v.x;
            let ty = (self.p.y - other.p.y) / other.v.y;
            // Either lines coincide / intersect everywhere,
            // or they intersect nowhere.
            return approx_equal(tx, ty);
        }

        let s = (b.x * c.y - b.y * c.x) / tmp;
        let t = (a.y * c.x - a.x * c.y) / tmp;

        if t < 0.0 || s < 0.0 {
            // The lines intersect, but it happened in the past.
            return false;
        }

        min - self.p.x <= self.v.x * s
            && self.v.x * s <= max - self.p.x
            && min - self.p.y <= self.v.y * s
            && self.v.y * s <= max - self.p.y
    }
}

fn approx_equal(a: f64, b: f64) -> bool {
    (a - b).abs() < std::f64::EPSILON
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Point {
    x: f64,
    y: f64,
    z: f64,
}

impl Point {
    fn new(x: f64, y: f64, z: f64) -> Point {
        Point { x, y, z }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct ParseError(String);

impl std::str::FromStr for Line {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = ParseError(s.to_string());

        let tmp: Vec<_> = s.split(" @ ").collect();
        let p: Point = tmp.get(0).ok_or(err.clone())?.parse()?;
        let v: Point = tmp.get(1).ok_or(err.clone())?.parse()?;

        Ok(Line::new(p, v))
    }
}

impl std::str::FromStr for Point {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let err = ParseError(s.to_string());

        let coords: Vec<_> = s
            .split(",")
            .map(|coord| {
                coord
                    .trim()
                    .parse::<i64>()
                    .map_err(|_| ParseError(coord.to_string()))
            })
            .collect::<Result<Vec<_>, ParseError>>()?;
        let x: f64 = *coords.get(0).ok_or(err.clone())? as f64;
        let y: f64 = *coords.get(1).ok_or(err.clone())? as f64;
        let z: f64 = *coords.get(2).ok_or(err.clone())? as f64;
        Ok(Point::new(x, y, z))
    }
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
}

impl std::ops::Neg for Point {
    type Output = Point;
    fn neg(self) -> Point {
        Point::new(-self.x, -self.y, -self.z)
    }
}

impl std::ops::Add<Point> for Point {
    type Output = Point;
    fn add(self, other: Point) -> Point {
        Point::new(self.x + other.x, self.y + other.y, self.z + other.z)
    }
}

impl std::ops::Sub<Point> for Point {
    type Output = Point;
    fn sub(self, other: Point) -> Point {
        self + (-other)
    }
}
