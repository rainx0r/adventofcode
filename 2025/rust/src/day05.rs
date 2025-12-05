use std::{
    cmp::{max, min},
    fs,
    path::Path,
};

fn parse_input() -> (Vec<(u64, u64)>, Vec<u64>) {
    let file_path = Path::new("data/day05.txt");
    let contents = fs::read_to_string(file_path).expect("Input for this day should exist.");
    let (id_ranges, ids) = contents
        .split_once("\n\n")
        .expect("Input format should be correct");

    let mut fresh_id_ranges: Vec<(u64, u64)> = Vec::new();
    for range in id_ranges.trim().lines() {
        let (start_str, end_str) = range.split_once("-").expect("Should be a range");
        let start: u64 = start_str.parse().expect("Should be a number");
        let end: u64 = end_str.parse().expect("Should be a number");
        fresh_id_ranges.push((start, end));
    }

    (
        fresh_id_ranges,
        ids.trim()
            .lines()
            .map(|line| line.parse().expect("should be an integer"))
            .collect(),
    )
}

pub fn part1() {
    let (fresh_id_ranges, ids) = parse_input();
    let mut total_fresh = 0;
    for id in ids {
        'inner: for (start, end) in fresh_id_ranges.iter() {
            if (start..end).contains(&&id) {
                total_fresh += 1;
                break 'inner;
            }
        }
    }

    println!("{total_fresh}");
}

pub fn part2() {
    let (mut fresh_id_ranges, _) = parse_input();
    fresh_id_ranges.sort();

    let mut non_overlapping_fresh_id_ranges: Vec<(u64, u64)> = Vec::new();
    let mut current_range = fresh_id_ranges[0];

    for (start, end) in fresh_id_ranges.iter().skip(1) {
        if end >= &current_range.0 && &current_range.1 >= start {
            current_range = (min(*start, current_range.0), max(*end, current_range.1));
        } else {
            non_overlapping_fresh_id_ranges.push(current_range);
            current_range = (*start, *end);
        }
    }
    non_overlapping_fresh_id_ranges.push(current_range);

    let total_fresh_ids: u64 = non_overlapping_fresh_id_ranges
        .iter()
        .map(|(start, end)| end - start + 1)
        .sum();
    println!("{total_fresh_ids}");
}
