mod production_handlers;
mod symbol;

use std::rc::Rc;

use jvm_bytecode::class_file::ClassFileBuilderMethods as ClassFile;
use production_handlers::*;
use rustc_jvm_lexer::{self, token_kind::TokenKind, Token};
use symbol::get_root_env;

pub fn parse<'a>(tokens: impl Iterator<Item = Token<'a>>, class_file: &mut ClassFile) {
    let env = get_root_env();
    let mut tokens = tokens
        .filter(|token| {
            !matches!(
                token.kind,
                TokenKind::Whitespace
                    | TokenKind::LineComment { .. }
                    | TokenKind::BlockComment { .. }
            )
        })
        .peekable();

    let mut items: Vec<rustc_jvm_ast::Item> = vec![];
    while tokens.peek().is_some() {
        items.push(p_item(&mut tokens, Rc::clone(&env), class_file));
    }
    // println!("{:#?}", items);
}
