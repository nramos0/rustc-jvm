use crate::symbol::{Env, EnvPtr};
use jvm_bytecode::class_file::ClassFileBuilderMethods as ClassFile;
use rustc_jvm_ast::FnSig;
use rustc_jvm_ast::{
    translation::{translate_fn, StackFrameContext},
    *,
};
use rustc_jvm_lexer::{
    literal_kind::LiteralKind,
    token_kind::{KeywordKind, TokenKind, TypeKind},
    Token,
};
use std::cell::RefCell;
use std::{iter::Peekable, rc::Rc};

pub fn p_item<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    class_file: &mut ClassFile,
) -> Item {
    match iter.peek().unwrap().kind {
        TokenKind::Keyword {
            kind: KeywordKind::Fn,
        } => Item {
            kind: ItemKind::Fn(Box::new(p_fn(iter, env, class_file))),
        },
        kind => unimplemented!("{:?}", kind),
    }
}

pub fn p_fn<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    class_file: &mut ClassFile,
) -> Fn {
    iter.read_token(TokenKind::Keyword {
        kind: KeywordKind::Fn,
    });

    let mut stack_frame = StackFrameContext::new();

    let name = iter.read_token(TokenKind::Ident).val.to_string();
    let ident = Ident {
        name: name.clone(),
        kind: IdentKind::Fn,
    };
    env.borrow_mut().put(name.clone(), ident);

    iter.read_token(TokenKind::OpenParen);

    let new_env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&env)))));
    let params = p_fn_params(iter, Rc::clone(&new_env), &mut stack_frame);

    iter.read_token(TokenKind::CloseParen);
    let ret_type = p_fn_ret_type(iter);

    let func = Fn {
        name,
        sig: FnSig {
            decl: FnDecl {
                inputs: params,
                output: ret_type,
            }
            .p(),
        },
        body: Some(p_block(iter, Rc::clone(&new_env), &mut stack_frame).p()),
    };
    translate_fn(&func, &mut stack_frame, class_file);

    func
}

pub fn p_fn_ret_type<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>) -> FnRetTy {
    if let TokenKind::Minus = iter.peek().unwrap().kind {
        // read "->"
        iter.read_token(TokenKind::Minus);
        iter.read_token(TokenKind::Gt);
        // read identifer / type
        FnRetTy::Ty(p_ty(iter).p())
    } else {
        FnRetTy::Default
    }
}

pub fn p_ty<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>) -> Ty {
    let (is_ref, is_mut) = p_opt_amp_mut(iter);
    let mut ty = match iter.peek().unwrap().kind {
        TokenKind::Type { kind } => {
            iter.read_token(TokenKind::Type { kind });
            match kind {
                TypeKind::U8 => Ty::u8(),
                TypeKind::U16 => Ty::u16(),
                TypeKind::U32 => Ty::u32(),
                TypeKind::U64 => Ty::u64(),
                TypeKind::U128 => Ty::u128(),
                TypeKind::I8 => Ty::i8(),
                TypeKind::I16 => Ty::i16(),
                TypeKind::I32 => Ty::i32(),
                TypeKind::I64 => Ty::i64(),
                TypeKind::I128 => Ty::i128(),
                TypeKind::Usize => Ty::usize(),
                TypeKind::Isize => Ty::isize(),
                TypeKind::F32 => Ty::f32(),
                TypeKind::F64 => Ty::f64(),
                TypeKind::Bool => Ty::bool(),
            }
        }
        TokenKind::OpenBracket => p_ty_arr(iter),
        kind => unimplemented!("{:#?}", kind),
    };

    if is_ref {
        ty = Ty {
            kind: TyKind::Rptr(MutTy {
                ty: ty.p(),
                mutbl: if is_mut {
                    Mutability::Mut
                } else {
                    Mutability::Not
                },
            }),
            size: 8,
        }
    }

    ty
}

pub fn p_ty_arr<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>) -> Ty {
    iter.read_token(TokenKind::OpenBracket);
    let ty = p_ty(iter);
    let amt = p_ty_arr_len(iter);
    iter.read_token(TokenKind::CloseBracket);

    let size = ty.size;
    Ty {
        kind: TyKind::Array(ty.p(), amt),
        size: size * amt,
    }
}

pub fn p_ty_arr_len<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>) -> usize {
    iter.read_token(TokenKind::Semi);
    match iter.read_token_fn(|token_kind| matches!(token_kind, TokenKind::Literal { .. })) {
        Token {
            kind:
                TokenKind::Literal {
                    kind: LiteralKind::Int { .. },
                    ..
                },
            val,
            ..
        } => val.parse::<usize>().unwrap(),
        _ => panic!(),
    }
}

pub fn p_fn_params<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    _: &mut StackFrameContext,
) -> Vec<Param> {
    let mut params = vec![];

    p_fn_param_self(iter, &mut params);
    p_opt_comma(iter);

    loop {
        if let TokenKind::CloseParen = iter.peek().unwrap().kind {
            return params;
        }
        params.push(p_fn_param(iter, Rc::clone(&env)));
        p_opt_comma(iter);
    }
}

pub fn p_fn_param<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Param {
    if let TokenKind::Keyword {
        kind: KeywordKind::Mut,
    } = iter.peek().unwrap().kind
    {
        iter.read_token(TokenKind::Keyword {
            kind: KeywordKind::Mut,
        });
    }

    let name = iter.read_token(TokenKind::Ident).val.to_string();
    iter.read_token(TokenKind::Colon);

    let ty = p_ty(iter);
    let size = ty.size;

    let ident = Ident {
        name: name.clone(),
        kind: IdentKind::Var(get_and_inc_symbol_addr(size)),
    };

    // add identifier to symbol table
    env.borrow_mut().put(name, ident.clone());

    Param { ty: ty.p(), ident }
}

pub fn p_fn_param_self<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    params: &mut Vec<Param>,
) {
    let mut ty_kind: Option<TyKind> = match p_opt_amp_mut(iter) {
        (true, true) => Some(TyKind::SelfRefMut),
        (true, false) => Some(TyKind::SelfRef),
        (false, false) => None,
        (false, true) => unreachable!(),
    };

    if ty_kind.is_some() {
        iter.read_token(TokenKind::Keyword {
            kind: KeywordKind::SelfLower,
        });
    } else if let TokenKind::Keyword {
        kind: KeywordKind::SelfLower,
    } = iter.peek().unwrap().kind
    {
        iter.read_token(TokenKind::Keyword {
            kind: KeywordKind::SelfLower,
        });
        ty_kind = Some(TyKind::SelfVal);
    }

    if let Some(kind) = ty_kind {
        params.push(Param {
            ty: Ty { kind, size: 0 }.p(),
            ident: Ident {
                name: "self".to_string(),
                kind: IdentKind::Keyword,
            },
        });
    }
}

pub fn p_opt_amp_mut<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>) -> (bool, bool) {
    if let TokenKind::And = iter.peek().unwrap().kind {
        iter.read_token(TokenKind::And);

        if let TokenKind::Keyword {
            kind: KeywordKind::Mut,
        } = iter.peek().unwrap().kind
        {
            iter.read_token(TokenKind::Keyword {
                kind: KeywordKind::Mut,
            });
            (true, true)
        } else {
            (true, false)
        }
    } else {
        (false, false)
    }
}

pub fn p_opt_comma<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>) {
    if let TokenKind::Comma = iter.peek().unwrap().kind {
        iter.read_token(TokenKind::Comma);
    }
}

pub fn p_block<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    stack: &mut StackFrameContext,
) -> Block {
    iter.read_token(TokenKind::OpenBrace);

    let new_env = Rc::new(RefCell::new(Env::new(Some(Rc::clone(&env)))));
    let stmts = p_stmts(iter, new_env, stack);
    iter.read_token(TokenKind::CloseBrace);

    Block { stmts }
}

pub fn p_stmts<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    stack: &mut StackFrameContext,
) -> Vec<Stmt> {
    let mut stmts = vec![];
    loop {
        if let TokenKind::CloseBrace = iter.peek().unwrap().kind {
            break;
        }

        stmts.push(p_stmt(iter, Rc::clone(&env), stack));
    }
    stmts
}

pub fn p_stmt<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    stack: &mut StackFrameContext,
) -> Stmt {
    if let TokenKind::Semi = iter.peek().unwrap().kind {
        iter.read_token(TokenKind::Semi);
        return Stmt {
            kind: StmtKind::Empty,
        };
    }
    match iter.peek().unwrap().kind {
        TokenKind::Semi => {
            iter.read_token(TokenKind::Semi);
            Stmt {
                kind: StmtKind::Empty,
            }
        }
        TokenKind::Keyword {
            kind: KeywordKind::While,
        } => Stmt {
            kind: StmtKind::Expr(p_while(iter, env, stack).p()),
        },
        TokenKind::Keyword {
            kind: KeywordKind::If,
        } => Stmt {
            kind: StmtKind::Expr(p_if(iter, env, stack).p()),
        },
        TokenKind::Keyword {
            kind: KeywordKind::For,
        } => Stmt {
            kind: StmtKind::Expr(p_for(iter, env, stack).p()),
        },
        TokenKind::Keyword {
            kind: KeywordKind::Let,
        } => p_decl(iter, env),
        TokenKind::Ident => Stmt {
            kind: StmtKind::Expr(p_assign_or_fn_call(iter, env).p()),
        },
        _ => Stmt {
            kind: StmtKind::Empty,
        },
    }
}

fn get_and_inc_symbol_addr(size: usize) -> usize {
    SYMBOL_ADDR_OFFSET_KEY.with(|f| {
        let mut ref_val = f.borrow_mut();
        let original = *ref_val;
        *ref_val = original + size;
        original
    })
}

// pub fn p_decls<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Vec<Stmt> {
//     let mut stmts = vec![];
//     while matches!(
//         iter.peek().unwrap().kind,
//         TokenKind::Keyword {
//             kind: KeywordKind::Let
//         }
//     ) {
//         stmts.push(p_decl(iter, Rc::clone(&env)));
//     }
//     stmts
// }

pub fn p_assign_or_fn_call<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
) -> Expr {
    let next_token = *iter.peek().unwrap();
    if let TokenKind::Ident = next_token.kind {
        match p_factor(iter, Rc::clone(&env)) {
            Expr {
                kind: ExprKind::Path(Path { mut segments }),
            } => {
                let ident = segments.remove(0).ident;
                let next_kind = iter.peek().unwrap().kind;
                if let TokenKind::Eq = next_kind {
                    iter.read_token(TokenKind::Eq);
                    let right = p_bool(iter, env);
                    Expr {
                        kind: ExprKind::Assign(
                            Expr {
                                kind: ExprKind::Path(Path {
                                    segments: vec![PathSegment { ident }],
                                }),
                            }
                            .p(),
                            right.p(),
                        ),
                    }
                } else if let TokenKind::OpenParen = next_kind {
                    p_fn_call(iter, env, ident)
                } else {
                    unimplemented!()
                }
            }
            Expr {
                kind: ExprKind::Index(ident, index),
            } => {
                let next_kind = iter.peek().unwrap().kind;
                if let TokenKind::Eq = next_kind {
                    iter.read_token(TokenKind::Eq);
                    let right = p_bool(iter, env);
                    Expr {
                        kind: ExprKind::Assign(
                            Expr {
                                kind: ExprKind::Index(ident, index),
                            }
                            .p(),
                            right.p(),
                        ),
                    }
                } else {
                    unimplemented!()
                }
            }
            expr => unreachable!("token: {:#?}\nExpr: {:#?}", next_token, expr),
        }
    } else {
        panic!("left side of assignment is not an identifier");
    }
}

pub fn p_fn_call<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    ident: Ident,
) -> Expr {
    iter.read_token(TokenKind::OpenParen);

    let mut params = vec![];

    while !matches!(iter.peek().unwrap().kind, TokenKind::CloseParen) {
        params.push(p_bool(iter, Rc::clone(&env)).p());
    }

    iter.read_token(TokenKind::CloseParen);

    Expr {
        kind: ExprKind::Call(ident, params),
    }
}

pub fn p_decl<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Stmt {
    iter.read_token(TokenKind::Keyword {
        kind: KeywordKind::Let,
    });

    // optional "mut"
    if let TokenKind::Keyword {
        kind: KeywordKind::Mut,
    } = iter.peek().unwrap().kind
    {
        iter.read_token_fn(|token_kind| {
            matches!(
                token_kind,
                TokenKind::Keyword {
                    kind: KeywordKind::Mut
                }
            )
        });
    }

    // identifer
    let id = iter.read_token(TokenKind::Ident);

    // ":"
    iter.read_token(TokenKind::Colon);

    // type
    let ty = p_ty(iter);

    // optional initialization
    let init_expr = if let TokenKind::Eq = iter.peek().unwrap().kind {
        iter.read_token(TokenKind::Eq);

        Some(if let TokenKind::OpenBracket = iter.peek().unwrap().kind {
            p_arr(iter, Rc::clone(&env))
        } else {
            p_bool(iter, Rc::clone(&env))
        })
    } else {
        None
    };

    // ";"
    iter.read_token(TokenKind::Semi);

    let ident = Ident {
        name: id.val.to_string(),
        kind: IdentKind::Var(get_and_inc_symbol_addr(ty.size)),
    };

    // add identifier to symbol table
    env.borrow_mut().put(id.val.to_string(), ident.clone());

    Stmt {
        kind: StmtKind::Local(
            Local {
                kind: match init_expr {
                    Some(expr) => LocalKind::Init(expr.p()),
                    None => LocalKind::Decl,
                },
                ident,
                ty,
            }
            .p(),
        ),
    }
}

pub fn p_arr<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    iter.read_token(TokenKind::OpenBracket);

    let mut vals = vec![];

    loop {
        if let TokenKind::CloseBracket = iter.peek().unwrap().kind {
            break;
        }
        vals.push(p_expr(iter, Rc::clone(&env)).p());
        p_opt_comma(iter);
    }

    iter.read_token(TokenKind::CloseBracket);

    Expr {
        kind: ExprKind::Array(vals),
    }
}

pub fn p_for<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    stack: &mut StackFrameContext,
) -> Expr {
    iter.read_token(TokenKind::Keyword {
        kind: KeywordKind::For,
    });

    // create intermediate environment to inject ident
    let env = Rc::new(RefCell::new(Env::new(Some(env))));

    let var_name = iter.read_token(TokenKind::Ident).val.to_string();
    let ident = Ident {
        name: var_name.clone(),
        kind: IdentKind::Var(get_and_inc_symbol_addr(8)),
    };
    env.borrow_mut().put(var_name, ident.clone());

    iter.read_token(TokenKind::Keyword {
        kind: KeywordKind::In,
    });

    let start = p_factor(iter, Rc::clone(&env)).p();
    iter.read_token(TokenKind::Dot);
    iter.read_token(TokenKind::Dot);
    let end = p_factor(iter, Rc::clone(&env)).p();

    Expr {
        kind: ExprKind::For(
            ident,
            Range { start, end }.p(),
            p_block(iter, env, stack).p(),
        ),
    }
}

pub fn p_while<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    stack: &mut StackFrameContext,
) -> Expr {
    iter.read_token(TokenKind::Keyword {
        kind: KeywordKind::While,
    });
    let expr = p_bool(iter, Rc::clone(&env)).p();
    let block = p_block(iter, env, stack).p();
    Expr {
        kind: ExprKind::While(expr, block),
    }
}

pub fn p_if<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    stack: &mut StackFrameContext,
) -> Expr {
    iter.read_token(TokenKind::Keyword {
        kind: KeywordKind::If,
    });
    let expr = p_bool(iter, Rc::clone(&env)).p();
    let block = p_block(iter, env, stack).p();
    Expr {
        kind: ExprKind::If(expr, block),
    }
}

trait Matchable<'a>: Iterator {
    fn read_token(&mut self, token: TokenKind) -> Token<'a>;
    fn read_token_fn<F>(&mut self, predicate: F) -> Token<'a>
    where
        F: std::ops::Fn(TokenKind) -> bool;
}

impl<'a, T> Matchable<'a> for Peekable<T>
where
    T: Iterator<Item = Token<'a>>,
{
    #[inline]
    fn read_token(&mut self, kind: TokenKind) -> Token<'a> {
        let token = self.next().unwrap();
        // actual: left
        // expected: right
        assert_eq!(token.kind, kind);
        token
    }

    fn read_token_fn<F>(&mut self, predicate: F) -> Token<'a>
    where
        F: std::ops::Fn(TokenKind) -> bool,
    {
        let token = self.next().unwrap();
        assert!(predicate(token.kind));
        token
    }
}

fn p_bool<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    let mut expr = p_join(iter, Rc::clone(&env));
    while let TokenKind::OrOr = iter.peek().unwrap().kind {
        iter.read_token(TokenKind::OrOr);
        expr = Expr {
            kind: ExprKind::Binary(BinOpKind::Or, expr.p(), p_join(iter, Rc::clone(&env)).p()),
        };
    }
    expr
}

fn p_join<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    let mut expr = p_equality(iter, Rc::clone(&env));
    while let TokenKind::AndAnd = iter.peek().unwrap().kind {
        iter.read_token(TokenKind::AndAnd);
        expr = Expr {
            kind: ExprKind::Binary(
                BinOpKind::And,
                expr.p(),
                p_equality(iter, Rc::clone(&env)).p(),
            ),
        };
    }
    expr
}

fn p_equality<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    let mut expr = p_rel(iter, Rc::clone(&env));
    while let TokenKind::EqEq | TokenKind::Ne = iter.peek().unwrap().kind {
        let token_kind = iter
            .read_token_fn(|token_kind| matches!(token_kind, TokenKind::EqEq | TokenKind::Ne))
            .kind;
        expr = Expr {
            kind: ExprKind::Binary(
                match token_kind {
                    TokenKind::EqEq => BinOpKind::Eq,
                    TokenKind::Ne => BinOpKind::Ne,
                    _ => unreachable!(),
                },
                expr.p(),
                p_rel(iter, Rc::clone(&env)).p(),
            ),
        };
    }
    expr
}

fn p_rel<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    let expr = p_expr(iter, Rc::clone(&env));
    match iter.peek().unwrap().kind {
        TokenKind::Lt | TokenKind::Gt | TokenKind::Le | TokenKind::Ge => {
            let token_kind = iter.next().unwrap().kind;
            Expr {
                kind: ExprKind::Binary(
                    match token_kind {
                        TokenKind::Lt => BinOpKind::Lt,
                        TokenKind::Gt => BinOpKind::Gt,
                        TokenKind::Le => BinOpKind::Le,
                        TokenKind::Ge => BinOpKind::Ge,
                        _ => unreachable!(),
                    },
                    expr.p(),
                    p_expr(iter, env).p(),
                ),
            }
        }
        _ => expr,
    }
}

fn p_expr<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    let mut expr = p_term(iter, Rc::clone(&env));
    while let TokenKind::Plus | TokenKind::Minus = iter.peek().unwrap().kind {
        let token_kind = iter
            .read_token_fn(|token_kind| matches!(token_kind, TokenKind::Plus | TokenKind::Minus))
            .kind;
        expr = Expr {
            kind: ExprKind::Binary(
                match token_kind {
                    TokenKind::Plus => BinOpKind::Add,
                    TokenKind::Minus => BinOpKind::Sub,
                    _ => unreachable!(),
                },
                expr.p(),
                p_term(iter, Rc::clone(&env)).p(),
            ),
        };
    }
    expr
}

fn p_term<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    let mut expr = p_unary(iter, Rc::clone(&env));
    while let TokenKind::Star | TokenKind::Slash = iter.peek().unwrap().kind {
        let token_kind = iter
            .read_token_fn(|token_kind| matches!(token_kind, TokenKind::Star | TokenKind::Slash))
            .kind;
        expr = Expr {
            kind: ExprKind::Binary(
                match token_kind {
                    TokenKind::Star => BinOpKind::Mul,
                    TokenKind::Slash => BinOpKind::Div,
                    _ => unreachable!(),
                },
                expr.p(),
                p_unary(iter, Rc::clone(&env)).p(),
            ),
        };
    }
    expr
}

fn p_unary<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    match iter.peek().unwrap().kind {
        TokenKind::Minus | TokenKind::Bang => {
            let token_kind = iter
                .read_token_fn(|token_kind| {
                    matches!(token_kind, TokenKind::Minus | TokenKind::Bang)
                })
                .kind;
            Expr {
                kind: ExprKind::Unary(
                    match token_kind {
                        TokenKind::Minus => UnOpKind::Neg,
                        TokenKind::Bang => UnOpKind::Not,
                        _ => unreachable!(),
                    },
                    p_unary(iter, env).p(),
                ),
            }
        }
        _ => p_factor(iter, env),
    }
}

fn p_factor<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>, env: EnvPtr) -> Expr {
    match iter.peek().unwrap().kind {
        TokenKind::OpenParen => {
            iter.read_token(TokenKind::OpenParen);
            let expr = Some(p_bool(iter, env));
            iter.read_token(TokenKind::CloseParen);
            expr.unwrap()
        }
        TokenKind::Literal { .. } => p_literal_expr(iter),
        TokenKind::Keyword {
            kind: KeywordKind::True | KeywordKind::False,
        } => {
            let val = matches!(
                iter.read_token_fn(|token_kind| matches!(
                    token_kind,
                    TokenKind::Keyword {
                        kind: KeywordKind::True | KeywordKind::False
                    }
                ))
                .kind,
                TokenKind::Keyword {
                    kind: KeywordKind::True
                }
            );
            Expr {
                kind: ExprKind::Lit(Lit {
                    kind: LitKind::Bool(val),
                    val: if val {
                        "true".to_string()
                    } else {
                        "false".to_string()
                    },
                }),
            }
        }
        TokenKind::Ident => {
            let name = iter.read_token(TokenKind::Ident).val;
            let id = Env::get(Some(Rc::clone(&env)), name.to_string());
            if id.is_none() {
                panic!("undefined identifier: {}", name);
            }
            let id = id.unwrap();

            if let TokenKind::OpenBracket = iter.peek().unwrap().kind {
                p_offset(iter, env, id)
            } else {
                Expr {
                    kind: ExprKind::Path(Path {
                        segments: vec![PathSegment { ident: id }],
                    }),
                }
            }
        }
        TokenKind::And => Expr {
            kind: ExprKind::Unary(
                {
                    iter.read_token(TokenKind::And);
                    if let TokenKind::Keyword {
                        kind: KeywordKind::Mut,
                    } = iter.peek().unwrap().kind
                    {
                        iter.read_token(TokenKind::Keyword {
                            kind: KeywordKind::Mut,
                        });
                        UnOpKind::Ref
                    } else {
                        UnOpKind::Ref
                    }
                },
                p_factor(iter, env).p(),
            ),
        },
        kind => unimplemented!("{:#?}", kind),
    }
}

fn p_offset<'a>(
    iter: &mut Peekable<impl Iterator<Item = Token<'a>>>,
    env: EnvPtr,
    ident: Ident,
) -> Expr {
    iter.read_token(TokenKind::OpenBracket);
    let expr = p_expr(iter, Rc::clone(&env)).p();
    iter.read_token(TokenKind::CloseBracket);
    Expr {
        kind: ExprKind::Index(ident, expr),
    }
}

fn p_literal_expr<'a>(iter: &mut Peekable<impl Iterator<Item = Token<'a>>>) -> Expr {
    let token = iter.read_token_fn(|token_kind| matches!(token_kind, TokenKind::Literal { .. }));
    match token.kind {
        TokenKind::Literal { kind, .. } => Expr {
            kind: ExprKind::Lit(Lit {
                kind: match kind {
                    LiteralKind::Int { .. } => LitKind::Integer(IntegerKind::USize),
                    LiteralKind::Float { .. } => LitKind::Float(FloatKind::F32),
                    LiteralKind::Char { .. } => LitKind::Char,
                    LiteralKind::Byte { .. } => LitKind::Byte,
                    LiteralKind::Str { .. } => LitKind::Str,
                    LiteralKind::RawStr { n_hashes, .. } => LitKind::StrRaw(n_hashes),
                    LiteralKind::ByteStr { .. } => LitKind::ByteStr,
                    LiteralKind::RawByteStr { n_hashes, .. } => LitKind::ByteStrRaw(n_hashes),
                },
                val: token.val.to_string(),
            }),
        },
        _ => unreachable!(),
    }
}
