// imports

use std::collections::HashMap;

use common::parse_int;



// solutions

pub fn part1(input: &str) -> String {
    let (x, y) = get_pos(parse_int(input));
    return format!("{}", x.abs() + y.abs());
}

pub fn part2(input: &str) -> String {
    let threshold = parse_int(input);
    let mut cache = HashMap::<i32, i32>::new();
    let mut res: i32 = 0;

    for p in 1 .. 999 {
        let val = get_part2_val(&mut cache, p);
        if val > threshold {
            res = val;
            break;
        }
    }
    return format!("{}", res);
}



// helpers

type Point = (i32, i32);

fn get_pos(index: i32) -> Point {
    assert!(index > 0);
    if index == 1 { return (0, 0); }

    let size  = (1 .. 999).filter(|n| n % 2 == 1 && n*n >= index).next().unwrap();
    let start = (size - 2 as i32).pow(2) + 1;
    let side  = (index - start) / (size - 1);
    let dist  = (index - start) % (size - 1);
    let u     = size / 2;
    let v     = dist - (size - 3) / 2;

    match side {
        0 => ( u,  v),
        1 => (-v,  u),
        2 => (-u, -v),
        3 => ( v, -u),
        _ => panic!("day 03: logic error, side {} not in 0 .. 4", side),
    }
}

fn get_val((x, y): Point) -> i32 {
    fn compute(k: i32, s: i32, d: i32) -> i32 {
        (2 * k - 1 as i32).pow(2) + 1 + s * (2 * k) + d + k - 1
    }
    if x.abs() > y.abs() {
        if x > 0 {
            compute(x, 0, y)
        } else {
            compute(-x, 2, -y)
        }
    } else {
        if y > 0 {
            compute(y, 1, -x)
        } else {
            compute(-y, 3, x)
        }
    }
}

fn get_part2_val(cache: &mut HashMap<i32, i32>, index: i32) -> i32 {
    if index == 1 { return 1; }
    match cache.get(&index) {
        Some(r) => return *r,
        None    => {}
    }
    let (x, y) = get_pos(index);
    let mut res: i32 = 0;
    for p in [get_val((x-1, y+1)),
              get_val((x,   y+1)),
              get_val((x+1, y+1)),
              get_val((x-1, y  )),
              get_val((x+1, y  )),
              get_val((x-1, y-1)),
              get_val((x,   y-1)),
              get_val((x+1, y-1))].into_iter() {
        if *p < index {
            res += get_part2_val(cache, *p);
        }
    }
    cache.insert(index, res);
    return res;
}
