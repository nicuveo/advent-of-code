use std::iter::Zip;
use std::slice::Iter;
use std::str::FromStr;

pub fn parse_lines<F: FromStr>(data: &str) -> Result<Vec<Vec<F>>, <F as FromStr>::Err> {
    data.lines()
        .map(|l| l.split_whitespace().map(|x| x.parse::<F>()).collect())
        .collect()
}

pub fn pairs<T>(xs: &[T]) -> Zip<Iter<T>, Iter<T>> {
    let head = xs.iter();
    let mut tail = xs.iter();
    tail.next();
    std::iter::zip(head, tail)
}

#[macro_export]
macro_rules! short_circuit_true {
    ($x: expr) => {
        if $x {
            return true;
        }
    };
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_pairs() {
        assert_eq!(
            pairs(&vec![1, 2, 3, 4]).collect::<Vec<_>>(),
            vec![(&1, &2), (&2, &3), (&3, &4)]
        );
    }
}
