mod cursor;
pub mod literal_kind;
pub mod token_kind;

use std::str::FromStr;

use cursor::{Cursor, EOF};

use token_kind::{DocStyle, KeywordKind, TokenKind, TypeKind};
use TokenKind::*;

use literal_kind::{Base, LiteralKind};
use LiteralKind::*;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct Token<'a> {
    pub kind: TokenKind,
    pub val: &'a str,
    pub len: usize,
    pub pos: Position,
}

impl<'a> Token<'a> {
    fn new(kind: TokenKind, val: &str, len: usize, pos: Position) -> Token {
        Token {
            kind,
            val,
            len,
            pos,
        }
    }
}

/// Creates an iterator that produces tokens from the input string.
pub fn tokenize<'a>(input: &'a str) -> impl Iterator<Item = Token<'a>> + '_ {
    let mut cursor = Cursor::new(input);
    std::iter::from_fn(move || {
        if cursor.is_eof() {
            None
        } else {
            cursor.reset_len_consumed();
            Some(cursor.advance_token())
        }
    })
}

/// True if `c` is considered a whitespace according to Rust language definition.
/// See [Rust language reference](https://doc.rust-lang.org/reference/whitespace.html)
/// for definitions of these classes.
pub fn is_whitespace(c: char) -> bool {
    // This is Pattern_White_Space.
    //
    // Note that this set is stable (ie, it doesn't change with different
    // Unicode versions), so it's ok to just hard-code the values.

    matches!(
        c,
        // Usual ASCII suspects
        '\u{0009}'   // \t
        | '\u{000A}' // \n
        | '\u{000B}' // vertical tab
        | '\u{000C}' // form feed
        | '\u{000D}' // \r
        | '\u{0020}' // space

        // NEXT LINE from latin1
        | '\u{0085}'

        // Bidi markers
        | '\u{200E}' // LEFT-TO-RIGHT MARK
        | '\u{200F}' // RIGHT-TO-LEFT MARK

        // Dedicated whitespace characters from Unicode
        | '\u{2028}' // LINE SEPARATOR
        | '\u{2029}' // PARAGRAPH SEPARATOR
    )
}

/// True if `c` is valid as a first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_start(c: char) -> bool {
    // This is XID_Start OR '_' (which formally is not a XID_Start).
    c == '_' || unicode_xid::UnicodeXID::is_xid_start(c)
}

/// True if `c` is valid as a non-first character of an identifier.
/// See [Rust language reference](https://doc.rust-lang.org/reference/identifiers.html) for
/// a formal definition of valid identifier name.
pub fn is_id_continue(c: char) -> bool {
    unicode_xid::UnicodeXID::is_xid_continue(c)
}

/// The passed string is lexically an identifier.
pub fn is_ident(string: &str) -> bool {
    let mut chars = string.chars();
    if let Some(start) = chars.next() {
        is_id_start(start) && chars.all(is_id_continue)
    } else {
        false
    }
}

impl<'a> Cursor<'a> {
    /// Parses a token from the input string.
    fn advance_token(&mut self) -> Token<'a> {
        let first_char = self.next().unwrap();

        let init_line = self.line;
        let init_col = self.col;

        let token_kind = match first_char {
            // Slash, comment or block comment.
            '/' => match self.first() {
                '/' => self.line_comment(),
                '*' => self.block_comment(),
                _ => Slash,
            },

            // Whitespace sequence.
            c if is_whitespace(c) => self.whitespace(c),

            // Identifier (this should be checked after other variant that can
            // start as identifier).
            c if is_id_start(c) => self.ident_or_unknown_prefix(),

            // Numeric literal.
            c @ '0'..='9' => {
                let literal_kind = self.number(c);
                let suffix_start = self.len_consumed();
                self.eat_literal_suffix();
                TokenKind::Literal {
                    kind: literal_kind,
                    suffix_start,
                }
            }

            // One-symbol tokens.
            ';' => Semi,
            ',' => Comma,
            '.' => Dot,
            '(' => OpenParen,
            ')' => CloseParen,
            '{' => OpenBrace,
            '}' => CloseBrace,
            '[' => OpenBracket,
            ']' => CloseBracket,
            '@' => At,
            '#' => Pound,
            '~' => Tilde,
            '?' => Question,
            ':' => Colon,
            '$' => Dollar,
            '=' => self.eq(),
            '!' => self.bang(),
            '<' => self.lt(),
            '>' => self.gt(),
            '-' => Minus,
            '&' => self.and(),
            '|' => self.or(),
            '+' => Plus,
            '*' => Star,
            '^' => Caret,
            '%' => Percent,

            // Lifetime or character literal.
            '\'' => self.lifetime_or_char(),

            // String literal.
            '"' => {
                let terminated = self.double_quoted_string();
                let suffix_start = self.len_consumed();
                if terminated {
                    self.eat_literal_suffix();
                }
                let kind = Str { terminated };
                Literal { kind, suffix_start }
            }
            _ => Unknown,
        };

        let mut line = self.line;
        let mut col = self.col;

        let len = self.token_str.len();
        self.col += len;

        if let TokenKind::Whitespace = token_kind {
            col = init_col;
            line = init_line;

            self.col -= self.line - init_line;
        }

        Token::new(
            token_kind,
            self.token_str,
            self.len_consumed(),
            Position { line, col },
        )
    }

    fn eq(&mut self) -> TokenKind {
        match self.first() {
            '=' => {
                self.next();
                EqEq
            }
            _ => Eq,
        }
    }

    fn bang(&mut self) -> TokenKind {
        match self.first() {
            '=' => {
                self.next();
                Ne
            }
            _ => Eq,
        }
    }

    fn lt(&mut self) -> TokenKind {
        match self.first() {
            '<' => {
                self.next();
                LtLt
            }
            '=' => {
                self.next();
                Le
            }
            _ => Lt,
        }
    }

    fn gt(&mut self) -> TokenKind {
        match self.first() {
            '>' => {
                self.next();
                GtGt
            }
            '=' => {
                self.next();
                Ge
            }
            _ => Gt,
        }
    }

    fn and(&mut self) -> TokenKind {
        match self.first() {
            '&' => {
                self.next();
                AndAnd
            }
            _ => And,
        }
    }

    fn or(&mut self) -> TokenKind {
        match self.first() {
            '|' => {
                self.next();
                OrOr
            }
            _ => Or,
        }
    }

    fn line_comment(&mut self) -> TokenKind {
        self.next();

        let doc_style = match self.first() {
            // `//!` is an inner line doc comment.
            '!' => Some(DocStyle::Inner),
            // `////` (more than 3 slashes) is not considered a doc comment.
            '/' if self.second() != '/' => Some(DocStyle::Outer),
            _ => None,
        };

        self.skip_while(|c| c != '\n');
        LineComment { doc_style }
    }

    fn block_comment(&mut self) -> TokenKind {
        self.next();

        let doc_style = match self.first() {
            // `/*!` is an inner block doc comment.
            '!' => Some(DocStyle::Inner),
            // `/***` (more than 2 stars) is not considered a doc comment.
            // `/**/` is not considered a doc comment.
            '*' if !matches!(self.second(), '*' | '/') => Some(DocStyle::Outer),
            _ => None,
        };

        let mut depth = 1usize;
        while let Some(c) = self.next() {
            match c {
                '/' if self.first() == '*' => {
                    self.next();
                    depth += 1;
                }
                '*' if self.first() == '/' => {
                    self.next();
                    depth -= 1;
                    if depth == 0 {
                        // This block comment is closed, so for a construction like "/* */ */"
                        // there will be a successfully parsed block comment "/* */"
                        // and " */" will be processed separately.
                        break;
                    }
                }
                _ => (),
            }
        }

        BlockComment {
            doc_style,
            terminated: depth == 0,
        }
    }

    fn whitespace(&mut self, first_char: char) -> TokenKind {
        let mut curr_char = first_char;
        loop {
            if curr_char == '\n' {
                self.line += 1;
                self.col = 1;
            }

            if is_whitespace(self.first()) && !self.is_eof() {
                curr_char = self.next().unwrap();
            } else {
                break;
            }
        }

        Whitespace
    }

    fn ident_or_unknown_prefix(&mut self) -> TokenKind {
        // Start is already eaten, eat the rest of identifier.
        self.skip_while(is_id_continue);
        // Known prefixes must have been handled earlier. So if
        // we see a prefix here, it is definitely an unknown prefix.
        match self.first() {
            '#' | '"' | '\'' => UnknownPrefix,
            c if !c.is_ascii() && unic_emoji_char::is_emoji(c) => {
                self.fake_ident_or_unknown_prefix()
            }
            _ => match KeywordKind::from_str(self.token_str) {
                Ok(keyword_kind) => Keyword { kind: keyword_kind },
                Err(_) => match TypeKind::from_str(self.token_str) {
                    Ok(type_kind) => Type { kind: type_kind },
                    Err(_) => Ident,
                },
            },
        }
    }

    fn fake_ident_or_unknown_prefix(&mut self) -> TokenKind {
        // Start is already eaten, eat the rest of identifier.
        self.skip_while(|c| {
            unicode_xid::UnicodeXID::is_xid_continue(c)
                || (!c.is_ascii() && unic_emoji_char::is_emoji(c))
                || c == '\u{200d}'
        });
        // Known prefixes must have been handled earlier. So if
        // we see a prefix here, it is definitely an unknown prefix.
        match self.first() {
            '#' | '"' | '\'' => UnknownPrefix,
            _ => InvalidIdent,
        }
    }

    fn number(&mut self, first_digit: char) -> LiteralKind {
        let mut base = Base::Decimal;
        if first_digit == '0' {
            // Attempt to parse encoding base.
            let has_digits = match self.first() {
                'b' => {
                    base = Base::Binary;
                    self.next();
                    self.eat_decimal_digits()
                }
                'o' => {
                    base = Base::Octal;
                    self.next();
                    self.eat_decimal_digits()
                }
                'x' => {
                    base = Base::Hexadecimal;
                    self.next();
                    self.eat_hexadecimal_digits()
                }
                // Not a base prefix.
                '0'..='9' | '_' | '.' | 'e' | 'E' => {
                    self.eat_decimal_digits();
                    true
                }
                // Just a 0.
                _ => {
                    return Int {
                        base,
                        empty_int: false,
                    }
                }
            };
            // Base prefix was provided, but there were no digits
            // after it, e.g. "0x".
            if !has_digits {
                return Int {
                    base,
                    empty_int: true,
                };
            }
        } else {
            // No base prefix, parse number in the usual way.
            self.eat_decimal_digits();
        };

        match self.first() {
            // Don't be greedy if this is actually an
            // integer literal followed by field/method access or a range pattern
            // (`0..2` and `12.foo()`)
            '.' if self.second() != '.' && !is_id_start(self.second()) => {
                // might have stuff after the ., and if it does, it needs to start
                // with a number
                self.next();
                let mut empty_exponent = false;
                if self.first().is_digit(10) {
                    self.eat_decimal_digits();
                    match self.first() {
                        'e' | 'E' => {
                            self.next();
                            empty_exponent = !self.eat_float_exponent();
                        }
                        _ => (),
                    }
                }
                Float {
                    base,
                    empty_exponent,
                }
            }
            'e' | 'E' => {
                self.next();
                let empty_exponent = !self.eat_float_exponent();
                Float {
                    base,
                    empty_exponent,
                }
            }
            _ => Int {
                base,
                empty_int: false,
            },
        }
    }

    fn lifetime_or_char(&mut self) -> TokenKind {
        let can_be_a_lifetime = if self.second() == '\'' {
            // It's surely not a lifetime.
            false
        } else {
            // If the first symbol is valid for identifier, it can be a lifetime.
            // Also check if it's a number for a better error reporting (so '0 will
            // be reported as invalid lifetime and not as unterminated char literal).
            is_id_start(self.first()) || self.first().is_digit(10)
        };

        if !can_be_a_lifetime {
            let terminated = self.single_quoted_string();
            let suffix_start = self.len_consumed();
            if terminated {
                self.eat_literal_suffix();
            }
            let kind = Char { terminated };
            return Literal { kind, suffix_start };
        }

        // Either a lifetime or a character literal with
        // length greater than 1.

        let starts_with_number = self.first().is_digit(10);

        // Skip the literal contents.
        // First symbol can be a number (which isn't a valid identifier start),
        // so skip it without any checks.
        self.next();
        self.skip_while(is_id_continue);

        // Check if after skipping literal contents we've met a closing
        // single quote (which means that user attempted to create a
        // string with single quotes).
        if self.first() == '\'' {
            self.next();
            let kind = Char { terminated: true };
            Literal {
                kind,
                suffix_start: self.len_consumed(),
            }
        } else {
            Lifetime { starts_with_number }
        }
    }

    fn single_quoted_string(&mut self) -> bool {
        // Check if it's a one-symbol literal.
        if self.second() == '\'' && self.first() != '\\' {
            self.next();
            self.next();
            return true;
        }

        // Literal has more than one symbol.

        // Parse until either quotes are terminated or error is detected.
        loop {
            match self.first() {
                // Quotes are terminated, finish parsing.
                '\'' => {
                    self.next();
                    return true;
                }
                // Probably beginning of the comment, which we don't want to include
                // to the error report.
                '/' => break,
                // Newline without following '\'' means unclosed quote, stop parsing.
                '\n' if self.second() != '\'' => break,
                // End of file, stop parsing.
                EOF if self.is_eof() => break,
                // Escaped slash is considered one character, so bump twice.
                '\\' => {
                    self.next();
                    self.next();
                }
                // Skip the character.
                _ => {
                    self.next();
                }
            }
        }
        // String was not terminated.
        false
    }

    /// Eats double-quoted string and returns true
    /// if string is terminated.
    fn double_quoted_string(&mut self) -> bool {
        while let Some(c) = self.next() {
            match c {
                '"' => {
                    return true;
                }
                '\\' if self.first() == '\\' || self.first() == '"' => {
                    // Bump again to skip escaped character.
                    self.next();
                }
                _ => (),
            }
        }
        // End of file reached.
        false
    }

    fn eat_decimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.next();
                }
                '0'..='9' => {
                    has_digits = true;
                    self.next();
                }
                _ => break,
            }
        }
        has_digits
    }

    fn eat_hexadecimal_digits(&mut self) -> bool {
        let mut has_digits = false;
        loop {
            match self.first() {
                '_' => {
                    self.next();
                }
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    has_digits = true;
                    self.next();
                }
                _ => break,
            }
        }
        has_digits
    }

    /// Eats the float exponent. Returns true if at least one digit was met,
    /// and returns false otherwise.
    fn eat_float_exponent(&mut self) -> bool {
        if self.first() == '-' || self.first() == '+' {
            self.next();
        }
        self.eat_decimal_digits()
    }

    // Eats the suffix of the literal, e.g. "_u8".
    fn eat_literal_suffix(&mut self) {
        self.eat_identifier();
    }

    // Eats the identifier.
    fn eat_identifier(&mut self) {
        if !is_id_start(self.first()) {
            return;
        }
        self.next();

        self.skip_while(is_id_continue);
    }
}
