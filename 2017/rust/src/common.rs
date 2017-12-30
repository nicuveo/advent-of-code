pub fn parse_int(word: &str) -> i32 {
    return word.parse().expect(&format!("error: {} is not a valid int", word));
}
