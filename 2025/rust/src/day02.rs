use std::{fs, path::Path};

fn parse_input() -> Vec<(i64, i64)> {
    let file_path = Path::new("data/day02.txt");
    let contents = fs::read_to_string(file_path).expect("Input for this day should exist.");

    return contents
        .trim()
        .split(",")
        .map(|x| {
            let (start_str, end_str) = x
                .split_once('-')
                .expect("each range must contain exactly one '-'");
            let start = start_str
                .parse()
                .expect("start of range should be a number");
            let end = end_str.parse().expect("end of range should be a number");

            (start, end)
        })
        .collect();
}

pub fn part1() {
    let mut sum: i64 = 0;

    for range in parse_input() {
        for number in range.0..=range.1 {
            let number_str = number.to_string();
            if number_str.len() % 2 == 0 {
                let (first_half_str, second_half_str) = number_str.split_at(number_str.len() / 2);
                if first_half_str == second_half_str {
                    sum += number as i64;
                }
            }
        }
    }

    println!("{}", sum);
}

pub fn part2() {
    let mut sum: i64 = 0;

    for range in parse_input() {
        for number in range.0..=range.1 {
            let s = number.to_string();
            let bytes = s.as_bytes();
            let len = bytes.len();
            let mut is_invalid = false;

            for block_len in 1..=len / 2 {
                if len % block_len != 0 {
                    continue;
                }

                let block = &bytes[..block_len];
                let repeats = len / block_len;

                let mut ok = true;
                for i in 1..repeats {
                    let start = i * block_len;
                    let end = start + block_len;
                    if &bytes[start..end] != block {
                        ok = false;
                        break;
                    }
                }

                if ok && repeats >= 2 {
                    is_invalid = true;
                    break;
                }
            }

            if is_invalid {
                sum += number;
            }
        }
    }

    println!("{sum}");
}
