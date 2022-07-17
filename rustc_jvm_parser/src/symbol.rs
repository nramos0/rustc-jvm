use lazy_static::lazy_static;
// use rustc_jvm_lexer::{token_kind::TokenKind, Token};
use rustc_jvm_ast::{Ident, IdentKind};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub type EnvPtr = Rc<RefCell<Env>>;

#[derive(Debug)]
pub struct Env {
    table: HashMap<String, Ident>,
    prev_table: Option<EnvPtr>,
}

impl Env {
    pub fn new(prev_table: Option<EnvPtr>) -> Self {
        Env {
            table: HashMap::new(),
            prev_table,
        }
    }

    pub fn get(mut env_opt: Option<EnvPtr>, name: String) -> Option<Ident> {
        while let Some(env_cell) = env_opt {
            let env = env_cell.borrow();
            if let Some(id) = env.table.get(&name) {
                return Some(id.clone());
            }
            env_opt = env.prev_table.as_ref().map(Rc::clone);
        }
        None
    }

    pub fn put(&mut self, name: String, ident: Ident) {
        self.table.insert(name, ident);
    }
}

lazy_static! {
    static ref KEYWORDS: Vec<&'static str> = vec![
        "break", "continue", "else", "enum", "false", "fn", "for", "if", "impl", "in", "let",
        "loop", "match", "mut", "pub", "ref", "return", "self", "Self", "struct", "trait", "use",
        "where", "while", "u8", "u16", "u32", "u64", "u128", "i8", "i16", "i32", "i64", "i128",
        "f32", "f64", "usize", "isize", "bool"
    ];
}

pub fn get_root_env() -> EnvPtr {
    let mut env = Env::new(None);
    KEYWORDS
        .iter()
        .map(|keyword| keyword.to_string())
        .for_each(|keyword| {
            env.table.insert(
                keyword.clone(),
                Ident {
                    name: keyword,
                    kind: IdentKind::Keyword,
                },
            );
        });
    Rc::new(RefCell::new(env))
}

// pub fn parse_tokens(string: &str, tokens: Vec<Token>, mut env: EnvPtr) {
//     let mut dist = 0usize;
//     let mut offset = 0usize;
//     tokens.iter().for_each(|token| {
//         let text = &string[dist..dist + token.len];
//         dist += token.len;
//         match token.kind {
//             TokenKind::OpenBrace => {
//                 let mut new_env = Env::new();
//                 new_env.prev_table = Some(Rc::clone(&env));
//                 env = Rc::new(RefCell::new(new_env));

//                 println!("Creating new scope...");
//             }
//             TokenKind::CloseBrace => {
//                 println!("Releasing scope...");
//                 println!("{:?}", env);

//                 let clone = Rc::clone(env.borrow().prev_table.as_ref().unwrap());
//                 env = clone;
//             }
//             TokenKind::Ident => {
//                 let table = &mut RefCell::borrow_mut(&env).table;
//                 let text_owned = text.to_string();
//                 if !table.contains_key(&text_owned) {
//                     table.insert(
//                         text_owned.clone(),
//                         Ident {
//                             name: text_owned,
//                             kind: IdentKind::Ident(offset)
//                         },
//                     );
//                     offset += 1;
//                 }
//             }
//             _ => {}
//         };
//     });

//     println!("Final scope...");
//     println!("{:?}", env);
// }
