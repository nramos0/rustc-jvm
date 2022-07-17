use std::str::FromStr;

use crate::literal_kind::LiteralKind;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum DocStyle {
    Outer,
    Inner,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum KeywordKind {
    Break,
    Continue,
    Else,
    Enum,
    False,
    Fn,
    For,
    If,
    Impl,
    In,
    Let,
    Loop,
    Match,
    Mut,
    Pub,
    Ref,
    Return,
    SelfLower,
    SelfUpper,
    Struct,
    Trait,
    True,
    Type,
    Use,
    Where,
    While,
}

impl FromStr for KeywordKind {
    type Err = ();
    /// Converts a string to a KeywordKind if it represents a keyword
    fn from_str(string: &str) -> Result<Self, Self::Err> {
        Ok(match string {
            "break" => KeywordKind::Break,
            "continue" => KeywordKind::Continue,
            "else" => KeywordKind::Else,
            "enum" => KeywordKind::Enum,
            "false" => KeywordKind::False,
            "fn" => KeywordKind::Fn,
            "for" => KeywordKind::For,
            "if" => KeywordKind::If,
            "impl" => KeywordKind::Impl,
            "in" => KeywordKind::In,
            "let" => KeywordKind::Let,
            "loop" => KeywordKind::Loop,
            "match" => KeywordKind::Match,
            "mut" => KeywordKind::Mut,
            "pub" => KeywordKind::Pub,
            "ref" => KeywordKind::Ref,
            "return" => KeywordKind::Return,
            "self" => KeywordKind::SelfLower,
            "Self" => KeywordKind::SelfUpper,
            "struct" => KeywordKind::Struct,
            "trait" => KeywordKind::Trait,
            "true" => KeywordKind::True,
            "type" => KeywordKind::Type,
            "use" => KeywordKind::Use,
            "where" => KeywordKind::Where,
            "while" => KeywordKind::While,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TypeKind {
    U8,
    U16,
    U32,
    U64,
    U128,
    I8,
    I16,
    I32,
    I64,
    I128,
    F32,
    F64,
    Usize,
    Isize,
    Bool,
}

impl FromStr for TypeKind {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(match s {
            "u8" => TypeKind::U8,
            "u16" => TypeKind::U16,
            "u32" => TypeKind::U32,
            "u64" => TypeKind::U64,
            "u128" => TypeKind::U128,
            "i8" => TypeKind::I8,
            "i16" => TypeKind::I16,
            "i32" => TypeKind::I32,
            "i64" => TypeKind::I64,
            "i128" => TypeKind::I128,
            "f32" => TypeKind::F32,
            "f64" => TypeKind::F64,
            "usize" => TypeKind::Usize,
            "isize" => TypeKind::Isize,
            "bool" => TypeKind::Bool,
            _ => return Err(()),
        })
    }
}

/// Enum representing common lexeme types.
// perf note: Changing all `usize` to `u32` doesn't change performance. See #77629
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum TokenKind {
    // Multi-char tokens:
    /// "// comment"
    LineComment { doc_style: Option<DocStyle> },
    /// `/* block comment */`
    ///
    /// Block comments can be recursive, so the sequence like `/* /* */`
    /// will not be considered terminated and will result in a parsing error.
    BlockComment {
        doc_style: Option<DocStyle>,
        terminated: bool,
    },
    /// Any whitespace characters sequence.
    Whitespace,
    /// "ident" or "continue"
    Ident,
    /// A keyword
    Keyword { kind: KeywordKind },
    /// A type
    Type { kind: TypeKind },
    /// Like the above, but containing invalid unicode codepoints.
    InvalidIdent,
    /// "r#ident"
    RawIdent,
    /// An unknown prefix like `foo#`, `foo'`, `foo"`. Note that only the
    /// prefix (`foo`) is included in the token, not the separator (which is
    /// lexed as its own distinct token). In Rust 2021 and later, reserved
    /// prefixes are reported as errors; in earlier editions, they result in a
    /// (allowed by default) lint, and are treated as regular identifier
    /// tokens.
    UnknownPrefix,
    /// "12_u8", "1.0e-40", "b"123"". See `LiteralKind` for more details.
    Literal {
        kind: LiteralKind,
        suffix_start: usize,
    },
    /// "'a"
    Lifetime { starts_with_number: bool },

    // One-char tokens:
    /// ";"
    Semi,
    /// ","
    Comma,
    /// "."
    Dot,
    /// "("
    OpenParen,
    /// ")"
    CloseParen,
    /// "{"
    OpenBrace,
    /// "}"
    CloseBrace,
    /// "["
    OpenBracket,
    /// "]"
    CloseBracket,
    /// "@"
    At,
    /// "#"
    Pound,
    /// "~"
    Tilde,
    /// "?"
    Question,
    /// ":"
    Colon,
    /// "$"
    Dollar,
    /// "="
    Eq,
    /// "!"
    Bang,
    /// "<"
    Lt,
    /// ">"
    Gt,
    /// "-"
    Minus,
    /// "&"
    And,
    /// "|"
    Or,
    /// "+"
    Plus,
    /// "*"
    Star,
    /// "/"
    Slash,
    /// "^"
    Caret,
    /// "%"
    Percent,

    // Two-char tokens:
    /// "=="
    EqEq,
    /// "!="
    Ne,
    /// "<<"
    LtLt,
    /// ">>"
    GtGt,
    /// "&&"
    AndAnd,
    /// "||"
    OrOr,
    /// ">="
    Ge,
    /// "<="
    Le,

    /// Unknown token, not expected by the lexer, e.g. "№"
    Unknown,
}
