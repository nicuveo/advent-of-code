use anyhow::Result;
use scan_fmt;

const TEST_DATA: &str = "";

fn parse(input: &str) -> Result<Vec<&str>> {
    Ok(input.lines().collect())
}

fn go(prefix: &str, input: &str) -> Result<()> {
    let parsed = parse(input)?;

    println!("{} part 1: {}", prefix, 0);
    println!("{} part 2: {}", prefix, 0);
    Ok(())
}

pub fn run(real_data: &str) -> Result<()> {
    go("test data", TEST_DATA)?;
    go("real data", real_data)?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
}
