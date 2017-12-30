pub fn part1(input: &str) -> String {
    let mut res: i32 = 0;
    for line in input.lines() {
        let mut lmin = i32::max_value();
        let mut lmax = i32::min_value();
        for word in line.split_whitespace() {
            let num = parse_int(word);
            if num < lmin { lmin = num; }
            if num > lmax { lmax = num; }
        }
        res += lmax - lmin;
    }
    return format!("{}", res);
}

pub fn part2(input: &str) -> String {
    let mut res: i32 = 0;
    for line in input.lines() {
        let nums: Vec<i32> = line.split_whitespace().map(parse_int).collect();
        for i in 0 .. nums.len() {
            for j in 0 .. nums.len() {
                if nums[i] > nums[j] && nums[i] % nums[j] == 0 {
                    res += nums[i] / nums[j]
                }
            }
        }
    }
    return format!("{}", res);
}

fn parse_int(word: &str) -> i32 {
    return word.parse().expect(&format!("day02: {} is not a valid int", word));
}
