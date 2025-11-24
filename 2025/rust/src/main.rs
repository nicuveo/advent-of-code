use anyhow::{Context, Result};
use clap::Parser;
use std::fs;
use std::path::PathBuf;

// mod day01;
// mod day02;
// mod day03;
// mod day04;
mod utils;

#[derive(Debug, Parser)]
#[clap(name = "aoc", version)]
struct Aoc {
    /// Path to directory containing input files
    input_path: PathBuf,

    /// Days to run
    #[arg(value_parser = clap::value_parser!(u8).range(1..=12))]
    days: Vec<u8>,
}

fn main() -> Result<()> {
    let aoc = Aoc::parse();
    for day in aoc.days {
        let file_path = aoc.input_path.join(format!("{:02}.in", day));
        let input =
            fs::read_to_string(&file_path).context(format!("failed to read {:?}", file_path))?;
        println!("# day {:02}", day);
        match day {
            // 1 => day01::run(&input)?,
            // 2 => day02::run(&input)?,
            // 3 => day03::run(&input)?,
            // 4 => day04::run(&input)?,
            _ => unreachable!(),
        };
    }
    Ok(())
}
