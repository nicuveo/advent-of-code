pub fn part1(input: &str) -> String {
    let mut res: i32 = 0;
    let bytes = input.as_bytes();
    let length = bytes.len();
    for i in 0 .. length {
        if bytes[i] == bytes[(i+1) % length] {
            res += bytes[i] as i32 - 48;
        }
    }
    return format!("{}", res);
}

pub fn part2(input: &str) -> String {
    let mut res: i32 = 0;
    let bytes = input.as_bytes();
    let length = bytes.len();
    for i in 0 .. length {
        if bytes[i] == bytes[(i+length/2) % length] {
            res += bytes[i] as i32 - 48;
        }
    }
    return format!("{}", res);
}
