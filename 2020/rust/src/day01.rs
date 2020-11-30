use std::fs::File;
use std::io::prelude::*;

fn read_input() -> std::io::Result<String> {
    let mut file = File::open("../input/01.in")?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() -> std::io::Result<()> {
    let mut result: i64 = 0;
    let input = read_input()?;
    for _line in input.lines() {
    }
    println!("part 1: {}", result);
    println!("part 2: {}", result);
    Ok(())
}
