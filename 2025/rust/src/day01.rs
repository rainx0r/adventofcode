use std::{fs, path::Path};

const MODULUS: i32 = 100;
const START: i32 = 50;

fn parse_input() -> Vec<i32> {
    let file_path = Path::new("data/day01.txt");
    let contents = fs::read_to_string(file_path).expect("Input for this day should exist.");

    return contents
        .replace("R", "")
        .replace("L", "-")
        .split_whitespace()
        .filter_map(|x| x.parse().ok())
        .collect();
}

pub fn part1() {
    let mut number: i32 = START;
    let mut zero_count: u32 = 0;

    for offset in parse_input() {
        number = (number + offset).rem_euclid(MODULUS);
        if number == 0 {
            zero_count += 1;
        }
    }
    println!("{zero_count}");
}

pub fn part2() {
    let mut number: i32 = START;
    let mut zero_count: u32 = 0;

    for offset in parse_input() {
        let full_rotations = (offset.abs() / MODULUS) as u32;
        zero_count += full_rotations;

        let offset = (offset.abs() % MODULUS) * offset.signum();
        if offset.signum() == 1 {
            zero_count += (number + offset.abs() > MODULUS) as u32;
        } else {
            zero_count += (number - offset.abs() < 0 && number != 0) as u32;
        }

        number = (number + offset).rem_euclid(MODULUS);
        if number == 0 {
            zero_count += 1;
        }
    }

    println!("{zero_count}");
}
