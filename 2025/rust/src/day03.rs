use std::{fs, path::Path};

fn parse_input() -> Vec<String> {
    let file_path = Path::new("data/day03.txt");
    let contents = fs::read_to_string(file_path).expect("Input for this day should exist.");

    return contents.trim().lines().map(|x| x.to_string()).collect();
}

pub fn part1() {
    let sum: u64 = parse_input()
        .into_iter()
        .map(|x| max_joltage::<2>(x.as_bytes()))
        .sum();
    println!("{sum}");
}

pub fn part2() {
    let sum: u64 = parse_input()
        .into_iter()
        .map(|x| max_joltage::<12>(x.as_bytes()))
        .sum();
    println!("{sum}");
}

fn max_joltage<const DIGITS: usize>(batteries: &[u8]) -> u64 {
    // https://github.com/AhmedYassineMaalej/AoC-2025/blob/main/src/problems/day3.rs
    let (mut acc, mut start) = (0, 0);

    for remaining_digits in (0..DIGITS).rev() {
        let end = batteries.len() - remaining_digits;

        let slice = &batteries[start..end];
        let (mut max_idx, mut max_digit) = (0, 0);

        for (idx, &digit) in slice.iter().enumerate() {
            if digit > max_digit {
                max_idx = idx;
                max_digit = digit;
            }
        }

        acc = acc * 10 + u64::from(max_digit - b'0');
        start += max_idx + 1;
    }

    acc
}
