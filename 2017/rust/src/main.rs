// imports

use std::env;
use std::fs::File;
use std::io::Read;

mod day_01;



// constants

const DAYS: [[fn(&str) -> String; 2]; 1] = [
    [day_01::part1, day_01::part2]
];



// helpers

fn do_day(input_dir: &str, day: i32) {
    let filename = format!("{}/{:02}.in", input_dir, day);
    let mut f = File::open(&filename).expect(&format!("error: {} file not found", &filename));
    let mut contents = String::new();
    f.read_to_string(&mut contents).unwrap();
    for (i, &f) in DAYS[(day-1) as usize].into_iter().enumerate() {
        println!("day {:2} part {}: {}", day, i+1, f(contents.trim()));
    }
}



// main

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        panic!("missing INPUT_DIR argument")
    }

    let input_dir = &args[1];
    if args.len() > 2 {
        for arg in args[2..].iter() {
            let day: i32 = arg.parse().expect(&format!("error: {} is not a valid day", arg));
            if day < 1 || day > 25 {
                panic!("error: {} is not a valid day", arg);
            }
            do_day(input_dir, day);
        }
    } else {
        for day in 1..25 {
            do_day(&input_dir, day);
        }
    }
}
