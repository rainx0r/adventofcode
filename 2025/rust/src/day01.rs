use std::{fs, path::Path};

pub fn part1() {
    let file_path = Path::new("data/day01.txt");
    let contents = fs::read_to_string(file_path).expect("Input for this day should exist.");

    let mut number: i32 = 50;
    let modulus = 100;
    let mut zero_count: i32 = 0;

    for line in contents.lines() {
        let (instruction, value_str) = line.split_at(1);
        let offset: i32 = value_str.parse().expect("Should be a number");
        match instruction {
            "R" => {
                number = (number + offset).rem_euclid(modulus);
            }
            "L" => {
                number = (number - offset).rem_euclid(modulus);
            }
            _ => panic!("Unknown instruction: {}", instruction),
        }
        zero_count += if number == 0 { 1 } else { 0 };
    }

    println!("{zero_count}");
}

pub fn part2() {
    let file_path = Path::new("data/day01.txt");
    let contents = fs::read_to_string(file_path).expect("Input for this day should exist.");

    let mut number: i32 = 50;
    let modulus = 100;
    let mut zero_count: i32 = 0;

    for line in contents.lines() {
        let (instruction, value_str) = line.split_at(1);
        let offset: i32 = value_str.parse().expect("Should be a number");

        let full_rotations = offset / modulus;
        zero_count += full_rotations;

        let rest = offset % modulus;

        match instruction {
            "R" => {
                zero_count += (number + rest > modulus) as i32;
                number = (number + rest).rem_euclid(modulus);
            }
            "L" => {
                zero_count += (number - rest < 0 && number != 0) as i32;
                number = (number - rest).rem_euclid(modulus);
            }
            _ => panic!("Unknown instruction: {}", instruction),
        }
        zero_count += if number == 0 { 1 } else { 0 };
    }

    println!("{zero_count}");
}
