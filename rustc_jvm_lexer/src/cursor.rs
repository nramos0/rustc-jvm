use std::str::Chars;
pub struct Cursor<'a> {
    // iterator that updates after every char read
    input: Chars<'a>,
    // iterator that updates after every token read
    token_input: Chars<'a>,
    // slice of the current token
    pub token_str: &'a str,
    // input remaining length, updates after every token read
    initial_len: usize,
    // current location
    pub line: usize,
    pub col: usize,
}

pub const EOF: char = '\0';

impl<'a> Cursor<'a> {
    pub fn new(input_str: &'a str) -> Self {
        Cursor {
            input: input_str.chars(),
            token_input: input_str.chars(),
            token_str: "",
            initial_len: input_str.len(),
            line: 1,
            col: 1,
        }
    }

    pub fn first(&self) -> char {
        self.input.clone().next().unwrap_or(EOF)
    }

    pub fn second(&self) -> char {
        let mut iter = self.input.clone();
        iter.next();
        iter.next().unwrap_or(EOF)
    }

    pub fn is_eof(&self) -> bool {
        self.input.as_str().is_empty()
    }

    pub fn len_consumed(&self) -> usize {
        self.initial_len - self.input.as_str().len()
    }

    pub fn reset_len_consumed(&mut self) {
        self.token_str = "";
        self.token_input = self.input.clone();
        self.initial_len = self.input.as_str().len();
    }

    pub fn next(&mut self) -> Option<char> {
        self.token_str = &self.token_input.as_str()[..self.len_consumed() + 1];
        self.input.next()
    }

    pub fn skip_while(&mut self, mut predicate: impl FnMut(char) -> bool) {
        while predicate(self.first()) && !self.is_eof() {
            self.next();
        }
    }
}
