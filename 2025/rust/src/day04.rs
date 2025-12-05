use std::{fs, path::Path};

#[derive(PartialEq, Eq)]
enum Item {
    Roll,
    None,
}

fn parse_input() -> Vec<Vec<Item>> {
    let file_path = Path::new("data/day04.txt");
    let contents = fs::read_to_string(file_path).expect("Input for this day should exist.");

    contents
        .trim()
        .lines()
        .map(|x| {
            x.chars()
                .map(|c| match c {
                    '@' => Item::Roll,
                    _ => Item::None,
                })
                .collect()
        })
        .collect()
}

fn check_position(map: &Vec<Vec<Item>>, x: i32, y: i32) -> bool {
    let mut sum = 0;
    for x_offset in -1..=1 {
        for y_offset in -1..=1 {
            if x_offset == 0 && y_offset == 0 {
                continue;
            };

            if x + x_offset < 0
                || x + x_offset >= (map.len() as i32)
                || y + y_offset < 0
                || y + y_offset >= (map[0].len() as i32)
            {
                continue;
            }

            if map[(x + x_offset) as usize][(y + y_offset) as usize] == Item::Roll {
                sum += 1;
            }
        }
    }

    sum < 4
}

pub fn part1() {
    let map = parse_input();
    let mut rolls = 0;

    for x in 0..map.len() {
        for y in 0..map[x].len() {
            if map[x][y] == Item::Roll {
                if check_position(&map, x as i32, y as i32) {
                    rolls += 1;
                }
            }
        }
    }

    println!("{rolls}");
}

pub fn part2() {
    let mut map = parse_input();
    let mut rolls = 0;
    let mut cont = true;

    while cont {
        let mut positions: Vec<(usize, usize)> = Vec::new();
        for x in 0..map.len() {
            for y in 0..map[x].len() {
                if map[x][y] == Item::Roll {
                    if check_position(&map, x as i32, y as i32) {
                        positions.push((x, y));
                        rolls += 1;
                    }
                }
            }
        }

        if positions.len() == 0 {
            cont = false;
        } else {
            for position in positions {
                map[position.0][position.1] = Item::None;
            }
        }
    }

    println!("{rolls}");
}
