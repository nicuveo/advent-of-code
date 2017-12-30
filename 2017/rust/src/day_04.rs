// solutions

pub fn part1(input: &str) -> String {
    let mut res: i32 = 0;
    for line in input.lines() {
        let words: Vec<&str> = line.split_whitespace().collect();
        if check(&words) {
            res += 1
        }
    }
    return format!("{}", res);
}

pub fn part2(input: &str) -> String {
    let mut res: i32 = 0;
    for line in input.lines() {
        let words: Vec<Vec<char>> = line.split_whitespace().map(sorted_chars).collect();
        if check(&words) {
            res += 1
        }
    }
    return format!("{}", res);
}



// helpers

fn check<T: PartialEq>(words: &Vec<T>) -> bool {
    for i in 0 .. words.len() {
        for j in 0 .. i {
            if words[i] == words[j] {
                return false;
            }
        }
    }
    return true;
}

fn sorted_chars(word: &str) -> Vec<char> {
    let mut res: Vec<char> = word.chars().collect();
    res.sort();
    return res;
}
