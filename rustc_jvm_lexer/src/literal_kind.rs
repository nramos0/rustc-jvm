#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum LiteralKind {
    /// "12_u8", "0o100", "0b120i99"
    Int { base: Base, empty_int: bool },
    /// "12.34f32", "0b100.100"
    Float { base: Base, empty_exponent: bool },
    /// "'a'", "'\\'", "'''", "';"
    Char { terminated: bool },
    /// "b'a'", "b'\\'", "b'''", "b';"
    Byte { terminated: bool },
    /// ""abc"", ""abc"
    Str { terminated: bool },
    /// "b"abc"", "b"abc"
    ByteStr { terminated: bool },
    /// "r"abc"", "r#"abc"#", "r####"ab"###"c"####", "r#"a"
    RawStr {
        n_hashes: u16,
        err: Option<RawStrError>,
    },
    /// "br"abc"", "br#"abc"#", "br####"ab"###"c"####", "br#"a"
    RawByteStr {
        n_hashes: u16,
        err: Option<RawStrError>,
    },
}

/// Base of numeric literal encoding according to its prefix.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Base {
    /// Literal starts with "0b".
    Binary,
    /// Literal starts with "0o".
    Octal,
    /// Literal starts with "0x".
    Hexadecimal,
    /// Literal doesn't contain a prefix.
    Decimal,
}

/// Error produced validating a raw string. Represents cases like:
/// - `r##~"abcde"##`: `InvalidStarter`
/// - `r###"abcde"##`: `NoTerminator { expected: 3, found: 2, possible_terminator_offset: Some(11)`
/// - Too many `#`s (>65535): `TooManyDelimiters`
// perf note: It doesn't matter that this makes `Token` 36 bytes bigger. See #77629
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum RawStrError {
    /// Non `#` characters exist between `r` and `"` eg. `r#~"..`
    InvalidStarter { bad_char: char },
    /// The string was never terminated. `possible_terminator_offset` is the number of characters after `r` or `br` where they
    /// may have intended to terminate it.
    NoTerminator {
        expected: usize,
        found: usize,
        possible_terminator_offset: Option<usize>,
    },
    /// More than 65535 `#`s exist.
    TooManyDelimiters { found: usize },
}
