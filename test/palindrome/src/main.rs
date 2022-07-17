trait PalindromeCheckable {
    fn is_palindrome(&self) -> bool;
}

impl PalindromeCheckable for [u8] {
    fn is_palindrome(&self) -> bool {
        let mut front = 0usize;
        let mut end = self.len() - 1;

        while front < end && self[front] == self[end] {
            front += 1;
            end -= 1;
        }

        front >= end
    }
}

impl PalindromeCheckable for &str {
    fn is_palindrome(&self) -> bool {
        self.as_bytes().is_palindrome()
    }
}

fn main() {
    let text = "hiih";
    println!("{}.is_palindrome() = {}", text, text.is_palindrome());
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn even_short_t() {
        assert_eq!("hiih".is_palindrome(), true);
    }

    #[test]
    fn even_short_f() {
        assert_eq!("haih".is_palindrome(), false);
    }

    #[test]
    fn odd_short_f() {
        assert_eq!("aih".is_palindrome(), false);
    }

    #[test]
    fn odd_short_t() {
        assert_eq!("hih".is_palindrome(), true);
    }

    #[test]
    fn even_long_t() {
        let bytes = "wiojosdfoijeroigjoiwjeroiweortwer".as_bytes();
        assert_eq!(
            bytes
                .iter()
                .chain(bytes.iter().rev())
                .map(|val| *val)
                .collect::<Vec<_>>()
                .is_palindrome(),
            true
        );
    }
}
