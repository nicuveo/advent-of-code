// imports

use common::parse_int;



// solutions

pub fn part1(input: &str) -> String {
    let mut steps: Vec<i32> = input.split_whitespace().map(parse_int).collect();
    let res = jumps(&mut steps, |s| s + 1);
    return format!("{}", res);
}

pub fn part2(input: &str) -> String {
    let mut steps: Vec<i32> = input.split_whitespace().map(parse_int).collect();
    let res = jumps(&mut steps, |s| if s > 2 { s-1 } else { s+1 });
    return format!("{}", res);
}



// helpers

fn jumps(steps: &mut Vec<i32>, update: fn(i32) -> i32) -> i32 {
    let mut current: i32 = 0;
    let mut jumps: i32 = 0;

    while current >= 0 && current < (steps.len() as i32) {
        let jump: &mut i32 = steps.get_mut(current as usize).unwrap();
        current += *jump;
        jumps += 1;
        *jump = update(*jump);
    }

    return jumps;
}
