use core::panic;
use std::collections::HashMap;

use crate::*;
use jvm_bytecode::{
    class_file::{
        access_flag::AccessFlagBuilder,
        const_pool::{ConstantKind, MethodRefConstant, NameAndTypeConstant},
        descriptor::ToDescriptor,
        *,
    },
    instructions::{ArrayType, Instruction, InstructionVec},
};

use std::collections::hash_map;

use jvm_bytecode::class_file::ClassFileBuilderMethods as ClassFile;

pub struct StackFrameContext {
    table: HashMap<String, (usize, Ty)>,
    top: usize,
    temp: usize,
}

pub enum LocalSize {
    Normal,
    Wide,
}

impl StackFrameContext {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
            top: 0,
            temp: 0,
        }
    }

    pub fn store_temporary(&mut self, ty: Ty) -> &(usize, Ty) {
        self.temp += 1;
        self.store_local(&(self.temp - 1).to_string(), ty)
    }

    pub fn get_local(&mut self, s: &str) -> &(usize, Ty) {
        self.table.get(&s.to_string()).unwrap()
    }

    pub fn store_local(&mut self, s: &str, ty: Ty) -> &(usize, Ty) {
        // println!("storing local {} with type {:#?}", self.top, ty);
        match self.table.entry(s.to_string()) {
            hash_map::Entry::Occupied(_) => panic!("local \"{}\" exists, cannot redeclare", s),
            hash_map::Entry::Vacant(e) => {
                let size = match ty.kind {
                    TyKind::Integer(IntegerKind::USize) | TyKind::Float(FloatKind::F64) => 2,
                    _ => 1,
                };
                let val = e.insert((self.top, ty));
                self.top += size;
                val
            }
        }
    }
}

fn generate_main_method(class_file: &mut ClassFile) {
    let Method {
        name_index,
        descriptor_index,
        ..
    } = class_file.methods.last().unwrap();
    let const_pool = &mut class_file.constant_pool;

    let name_and_type_index = const_pool.push(ConstantKind::NameAndType(NameAndTypeConstant {
        name_index: *name_index,
        descriptor_index: *descriptor_index,
    }));

    let rust_main_method_ref_index = const_pool
        .push(ConstantKind::MethodRef(MethodRefConstant {
            class_index: class_file.this_class,
            name_and_type_index,
        }))
        .to_be_bytes();

    class_file.method_by_ref(|class_file| {
        MethodBuilder::main_method(class_file)
            .code(|_| CodeAttribute {
                max_stack: 2,
                max_locals: 1,
                code: InstructionVec(vec![
                    Instruction::invokestatic(
                        rust_main_method_ref_index[0],
                        rust_main_method_ref_index[1],
                    ),
                    Instruction::_return,
                ])
                .to_byte_vec(),
                exception_table: vec![],
                attributes: vec![],
            })
            .to_method()
    });
}

pub fn translate_fn(func: &Fn, ctx: &mut StackFrameContext, class_file: &mut ClassFile) {
    let java_main_name = "main";
    let rust_main_name = "rust_main";

    if let Some(_) = &func.body {
        let name = &func.name[..];
        if name == rust_main_name {
            panic!("reserved function name {} cannot be used", rust_main_name);
        }
        let name = if name == java_main_name {
            rust_main_name
        } else {
            name
        };

        let mut instructions = translate_fn_params(&func.sig.decl, ctx, class_file);
        instructions
            .0
            .extend(translate_block(func.body.as_ref().unwrap(), ctx, class_file, None).0);
        instructions.0.push(Instruction::_return);

        class_file.method_by_ref(|class_file| {
            MethodBuilder::build(class_file)
                .access_flags(AccessFlagBuilder::method().acc_public().acc_static())
                .name(name)
                .descriptor(&func.sig.decl.to_descriptor())
                .attributes()
                .code(|_| CodeAttribute {
                    max_stack: 100,
                    max_locals: ctx.top as u16,
                    code: instructions.to_byte_vec(),
                    exception_table: vec![],
                    attributes: vec![],
                })
                .to_method()
        });

        // this is the rust main method, generate a java main method
        // and invoke the rust main method from it
        if name == rust_main_name {
            generate_main_method(class_file);
        }
    }
}

pub fn translate_fn_params(
    decl: &FnDecl,
    ctx: &mut StackFrameContext,
    _: &mut ClassFile,
) -> InstructionVec {
    for param in &decl.inputs {
        ctx.store_temporary(Ty {
            kind: param.ty.kind.clone(),
            size: 0,
        });
    }
    InstructionVec(vec![])
}

pub fn translate_block(
    block: &P<Block>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
    type_req: Option<&TyKind>,
) -> InstructionVec {
    let mut instructions = vec![];
    for stmt in &block.stmts {
        instructions.extend(translate_stmt(stmt, ctx, class_file, type_req));
    }
    InstructionVec(instructions)
}

pub fn translate_stmt(
    stmt: &Stmt,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
    type_req: Option<&TyKind>,
) -> Vec<Instruction> {
    match stmt.kind {
        StmtKind::Local(ref local) => translate_local(local, ctx, class_file, type_req),
        StmtKind::Item(_) => todo!(),
        StmtKind::Expr(ref expr) | StmtKind::Semi(ref expr) => {
            translate_expr(expr, ctx, class_file, type_req).0
        }
        StmtKind::Empty => vec![],
    }
}

pub fn auto_convert_types(expr_type: &TyKind, expected_type: &TyKind) -> Vec<Instruction> {
    let mut instr = vec![];

    match (expr_type, expected_type) {
        (TyKind::Integer(IntegerKind::I32), TyKind::Integer(IntegerKind::USize)) => {
            // expected usize, got i32
            // insert conversion
            // println!("converting i32 to usize");
            instr.push(Instruction::i2l);
            instr
        }
        (TyKind::Integer(IntegerKind::USize), TyKind::Integer(IntegerKind::I32)) => {
            // expected i32, got usize
            // insert conversion
            // println!("converting usize to i32");
            instr.push(Instruction::l2i);
            instr
        }
        (TyKind::Integer(kind1), TyKind::Integer(kind2))
            if std::mem::discriminant(kind1) == std::mem::discriminant(kind2) =>
        {
            instr
        }
        (TyKind::Array(ty1, _), TyKind::Array(ty2, _))
            if std::mem::discriminant(&ty1.kind) == std::mem::discriminant(&ty2.kind) =>
        {
            instr
        }
        _ => panic!(
            "unsupported type conversion: {:#?} -> {:#?}",
            expr_type, expected_type
        ),
    }
}

pub fn translate_local(
    stmt: &Local,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
    type_req: Option<&TyKind>,
) -> Vec<Instruction> {
    let local = ctx.store_local(&stmt.ident.name, stmt.ty.clone()).0 as u8;
    match &stmt.kind {
        LocalKind::Init(expr) => {
            let (mut instr, ty) = translate_expr(expr, ctx, class_file, type_req);
            instr.extend(auto_convert_types(&ty, &stmt.ty.kind));

            // support only ints, longs, and arrays
            instr.push(match stmt.ty.kind {
                TyKind::Array(_, _) => Instruction::astore(local),
                TyKind::Integer(IntegerKind::USize | IntegerKind::I64) => {
                    Instruction::lstore(local)
                }
                TyKind::Integer(IntegerKind::I32) => Instruction::istore(local),
                _ => todo!(),
            });
            instr
        }
        _ => vec![],
    }
}

/// Emits instructions to push the value of the expression onto the stack
pub fn translate_expr(
    expr: &Expr,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
    type_req: Option<&TyKind>,
) -> (Vec<Instruction>, TyKind) {
    match &expr.kind {
        ExprKind::Assign(left, right) => translate_assign(left, right, ctx, class_file),
        ExprKind::Binary(kind, left, right) => translate_binary(kind, left, right, ctx, class_file),
        ExprKind::While(cond, block) => translate_while(cond, block, ctx, class_file),
        ExprKind::For(ident, range, block) => translate_for(ident, range, block, ctx, class_file),
        ExprKind::Path(path) => translate_path(path, ctx, class_file),
        ExprKind::Lit(lit) => translate_lit(lit, ctx, class_file, type_req),
        ExprKind::Array(arr) => translate_array(arr, ctx, class_file, type_req),
        ExprKind::Index(ident, index) => translate_index(ident, index, ctx, class_file),
        ExprKind::Call(ident, params) => translate_call(ident, params, ctx, class_file),
        ExprKind::If(cond, block) => translate_if(cond, block, ctx, class_file),
        _ => unimplemented!("expr: {:#?}", expr),
    }
}

pub fn translate_if(
    cond: &P<Expr>,
    block: &P<Block>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    let (mut expr_instr, _) = translate_expr(cond, ctx, class_file, None);

    let block_instr = translate_block(block, ctx, class_file, None);
    let jump_byte_offset_bytes = i16::try_from(3 + block_instr.byte_len())
        .unwrap()
        .to_be_bytes();

    // jump if condition is 0
    expr_instr.push(Instruction::ifeq(
        jump_byte_offset_bytes[0],
        jump_byte_offset_bytes[1],
    ));
    expr_instr.extend(block_instr.0);

    (expr_instr, TyKind::Never)
}

pub fn translate_call(
    ident: &Ident,
    params: &Vec<P<Expr>>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    let mut instr = vec![];

    if ident.name != "print_arr" {
        panic!("function call only supported for compiler-known print_arr function");
    }

    // push System.out onto stack
    let system_out_index_bytes = class_file
        .constant_pool
        .include_java_system_out()
        .to_be_bytes();
    instr.push(Instruction::getstatic(
        system_out_index_bytes[0],
        system_out_index_bytes[1],
    ));

    for param in params {
        instr.extend(translate_expr(param, ctx, class_file, None).0);
    }

    // invoke Arrays.toString
    let arr_long_to_string_index_bytes = class_file
        .constant_pool
        .include_java_arr_long_to_string()
        .to_be_bytes();
    instr.push(Instruction::invokestatic(
        arr_long_to_string_index_bytes[0],
        arr_long_to_string_index_bytes[1],
    ));
    // invoke System.out.println
    let system_out_println_index_bytes = class_file
        .constant_pool
        .include_java_println_string()
        .to_be_bytes();
    instr.push(Instruction::invokevirtual(
        system_out_println_index_bytes[0],
        system_out_println_index_bytes[1],
    ));

    (instr, TyKind::Never)
}

pub fn translate_for(
    ident: &Ident,
    range: &P<Range>,
    block: &P<Block>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    let (mut instr, ty) = translate_expr(
        &range.start,
        ctx,
        class_file,
        Some(&TyKind::Integer(IntegerKind::I32)),
    );
    // range start is now on stack

    let ident_local = ctx
        .store_local(
            &ident.name,
            Ty {
                kind: TyKind::Integer(IntegerKind::I32),
                size: 0,
            },
        )
        .0 as u8;
    instr.push(Instruction::istore(ident_local)); // store range start in local ident

    instr.extend(
        translate_expr(
            &range.end,
            ctx,
            class_file,
            Some(&TyKind::Integer(IntegerKind::I32)),
        )
        .0,
    );
    // range end is now on stack, store it in a variable

    let range_end_local = ctx
        .store_temporary(Ty {
            kind: TyKind::Integer(IntegerKind::I32),
            size: 0,
        })
        .0 as u8;
    instr.push(Instruction::istore(range_end_local));

    // run block
    let block_instr = translate_block(block, ctx, class_file, None);
    let block_instr_byte_len = block_instr.byte_len();
    instr.extend(block_instr.0);

    // increment local ident
    instr.push(Instruction::iinc(ident_local, 1));
    // check if local ident is less than range end
    // if it is, jump back to start of loop
    instr.push(Instruction::iload(ident_local));
    instr.push(Instruction::iload(range_end_local));
    let offset_bytes = (-i16::try_from(block_instr_byte_len + 3 + 2 + 2).unwrap()).to_be_bytes();
    instr.push(Instruction::if_icmplt(offset_bytes[0], offset_bytes[1]));

    (instr, ty)
}

pub fn translate_index(
    ident: &Ident,
    index: &P<Expr>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    let (arr_local, ty) = ctx.get_local(&ident.name).clone();
    let arr_local = arr_local as u8;

    if let TyKind::Array(inner_ty, _) = ty.kind {
        // load array onto stack
        let mut instr = vec![Instruction::aload(arr_local)];

        // load index onto stack
        let (index_instr, index_ty) = translate_expr(index, ctx, class_file, None);
        instr.extend(index_instr);
        let conv = auto_convert_types(&index_ty, &TyKind::Integer(IntegerKind::I32));
        instr.extend(conv);

        // load arr[index] onto stack
        instr.push(match inner_ty.kind {
            TyKind::Integer(IntegerKind::USize) => Instruction::laload,
            TyKind::Integer(IntegerKind::I32) => Instruction::iaload,
            _ => unimplemented!(),
        });

        return (instr, inner_ty.kind.clone());
    }
    panic!("indexing non-array ident!");
}

pub fn translate_path(
    path: &Path,
    ctx: &mut StackFrameContext,
    _: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    let (local, ty) = ctx.get_local(&path.segments[0].ident.name);
    let local = *local as u8;
    // push local onto stack
    (
        vec![match &ty.kind {
            TyKind::Integer(IntegerKind::USize) => Instruction::lload(local),
            TyKind::Integer(IntegerKind::I32) => Instruction::iload(local),
            TyKind::Array(_, _) => Instruction::aload(local),
            kind => unimplemented!("{:#?}", kind),
        }],
        ty.kind.clone(),
    )
}

pub fn translate_array(
    arr: &Vec<P<Expr>>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
    type_req: Option<&TyKind>,
) -> (Vec<Instruction>, TyKind) {
    let len_index = class_file.constant_pool.get_int_index(arr.len() as u32);
    let (arr_local, ty) = {
        let (local, ty) = ctx.store_temporary(Ty {
            kind: TyKind::Array(
                Ty {
                    kind: TyKind::Integer(IntegerKind::USize),
                    size: 0,
                }
                .p(),
                0,
            ),
            size: 0,
        });
        (*local as u8, ty.clone())
    };

    let (i_local, _) = ctx.store_temporary(Ty {
        kind: TyKind::Integer(IntegerKind::I32),
        size: 0,
    });
    let i_local = *i_local as u8;

    let mut instr = vec![
        Instruction::ldc(len_index as u8), // push array length onto stack
        Instruction::newarray(ArrayType::Long as u8), // allocate new array
        Instruction::astore(arr_local),    // store array into variable
        Instruction::iconst_0,             // store i = 0 onto stack
        Instruction::istore(i_local),      // store i = 0 into local
    ];

    for expr in arr {
        instr.push(Instruction::aload(arr_local)); // load array onto stack
        instr.push(Instruction::iload(i_local)); // load i onto stack
        instr.extend(translate_expr(expr, ctx, class_file, type_req).0); // store expression result onto stack
        instr.push(Instruction::lastore); // arr[i] = val
        instr.push(Instruction::iinc(i_local, 1)); // ++i
    }

    instr.push(Instruction::aload(arr_local)); // load array onto stack

    (instr, ty.kind)
}

pub fn translate_lit(
    lit: &Lit,
    _: &mut StackFrameContext,
    class_file: &mut ClassFile,
    type_req: Option<&TyKind>,
) -> (Vec<Instruction>, TyKind) {
    match type_req {
        Some(kind) if matches!(kind, TyKind::Integer(IntegerKind::I32)) => {
            let index_bytes = class_file
                .constant_pool
                .get_int_index(lit.val.parse::<u32>().unwrap())
                .to_be_bytes();
            (
                vec![Instruction::ldc_w(index_bytes[0], index_bytes[1])],
                kind.clone(),
            )
        }
        Some(_) | None => {
            // store literal in const pool
            let index_bytes = class_file
                .constant_pool
                .get_long_index(lit.val.parse::<usize>().unwrap())
                .to_be_bytes();

            // push literal onto stack
            (
                vec![Instruction::ldc2_w(index_bytes[0], index_bytes[1])],
                TyKind::Integer(IntegerKind::USize),
            )
        }
    }
}

pub fn translate_while(
    cond: &P<Expr>,
    block: &P<Block>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    let mut instr = vec![];

    let (expr_instr, ty) = translate_expr(cond, ctx, class_file, None);
    let expr_instr = InstructionVec(expr_instr);
    let expr_instr_byte_len = expr_instr.byte_len();

    instr.extend(expr_instr.0);

    // the value of the boolean expression is now on the top of the stack, save in temporary
    // let temp = ctx
    //     .store_temporary(Ty {
    //         kind: TyKind::Bool,
    //         size: 4,
    //     })
    //     .0 as u8;
    // instr.push(Instruction::istore(temp));

    // // reload temp local back onto stack
    // instr.push(Instruction::iload(temp));

    let block_instr = translate_block(block, ctx, class_file, None);
    let block_instr_byte_len = block_instr.byte_len();

    // check if the boolean expression is true or false
    // ifeq jumps if the value on the stack is 0
    // we jump to the end of the block if 0, by jumping
    // a byte offset equal to the block's instruction byte length + 1
    // (1) added three is to account for the space taken by the ifeq instruction
    // (2) added three is to account for the extra goto added to the end of the block
    // instructions for the while loop jump
    let ifeq_skip_block_byte_offset = i16::try_from(3 + block_instr_byte_len + 3)
        .unwrap()
        .to_be_bytes();
    instr.push(Instruction::ifeq(
        ifeq_skip_block_byte_offset[0],
        ifeq_skip_block_byte_offset[1],
    ));
    instr.extend(block_instr.0);

    // need signed value for negative byte offset
    // block_instr_byte_len: skip to start of block
    // 3: skip to start of ifeq
    // expr_instr_byte_len: skip to start bool instructions
    let goto_loop_byte_offset = block_instr_byte_len + 3 + expr_instr_byte_len;
    let goto_loop_byte_offset = -i16::try_from(goto_loop_byte_offset).unwrap();
    let goto_loop_byte_offset = goto_loop_byte_offset.to_be_bytes();

    instr.push(Instruction::goto(
        goto_loop_byte_offset[0],
        goto_loop_byte_offset[1],
    ));

    (instr, ty)
}

pub fn translate_binary_and_or(
    binop_kind: &BinOpKind,
    left: &P<Expr>,
    right: &P<Expr>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    match binop_kind {
        BinOpKind::And => {
            let mut instr = vec![];

            let (left_instr, _) = translate_expr(left, ctx, class_file, None);
            let (right_instr, _) = translate_expr(right, ctx, class_file, None);

            let right_instr = InstructionVec(right_instr);
            let right_instr_byte_len = right_instr.byte_len() as u8;

            // do left comparison
            instr.extend(left_instr);
            // jump to push 0 if false
            instr.push(Instruction::ifeq(0, 3 + right_instr_byte_len + 3 + 1 + 3));

            // do right comparison
            instr.extend(right_instr.0);
            // jump to push 0 if false
            instr.push(Instruction::ifeq(0, 3 + 1 + 3));

            // push 1
            instr.push(Instruction::iconst_1);
            // skip over push 0
            instr.push(Instruction::goto(0, 3 + 1));

            // push 0
            instr.push(Instruction::iconst_0);

            (instr, TyKind::Bool)
        }
        BinOpKind::Or => {
            let mut instr = vec![];

            let (left_instr, _) = translate_expr(left, ctx, class_file, None);
            let (right_instr, _) = translate_expr(right, ctx, class_file, None);

            let right_instr = InstructionVec(right_instr);
            let right_instr_byte_len = right_instr.byte_len() as u8;

            // do left comparison
            instr.extend(left_instr);
            // jump to push 1 if true
            instr.push(Instruction::ifne(0, 3 + right_instr_byte_len + 3 + 1 + 3));

            // do right comparison
            instr.extend(right_instr.0);
            // jump to push 1 if true
            instr.push(Instruction::ifne(0, 3 + 1 + 3));

            // push 0
            instr.push(Instruction::iconst_0);
            // skip over push 1
            instr.push(Instruction::goto(0, 3 + 1));

            // push 1
            instr.push(Instruction::iconst_1);

            (instr, TyKind::Bool)
        }
        _ => panic!(
            "translate_binary_and_or called with binop: {:#?}",
            binop_kind
        ),
    }
}

pub fn translate_binary(
    binop_kind: &BinOpKind,
    left: &P<Expr>,
    right: &P<Expr>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    if let BinOpKind::And | BinOpKind::Or = binop_kind {
        return translate_binary_and_or(binop_kind, left, right, ctx, class_file);
    }

    let (mut left_instr, left_ty) = translate_expr(left, ctx, class_file, None);
    let (right_instr, right_ty) = translate_expr(right, ctx, class_file, None);
    left_instr.extend(right_instr);
    left_instr.extend(auto_convert_types(&right_ty, &left_ty));

    left_instr.extend(match left_ty {
        TyKind::Integer(ref kind) => match kind {
            IntegerKind::USize => match binop_kind {
                BinOpKind::Add => vec![Instruction::ladd],
                BinOpKind::Sub => vec![Instruction::lsub],
                BinOpKind::Mul => vec![Instruction::lmul],
                BinOpKind::Div => vec![Instruction::ldiv],
                BinOpKind::Rem => unimplemented!(),
                BinOpKind::And => unimplemented!(),
                BinOpKind::Or => unimplemented!(),
                BinOpKind::BitXor => unimplemented!(),
                BinOpKind::BitAnd => unimplemented!(),
                BinOpKind::BitOr => unimplemented!(),
                BinOpKind::Shl => unimplemented!(),
                BinOpKind::Shr => unimplemented!(),
                BinOpKind::Eq => unimplemented!(),
                BinOpKind::Ne => unimplemented!(),
                BinOpKind::Le | BinOpKind::Lt | BinOpKind::Ge | BinOpKind::Gt => vec![
                    Instruction::lcmp,
                    match binop_kind {
                        BinOpKind::Lt => Instruction::ifge(0, 3 + 1 + 3),
                        BinOpKind::Le => Instruction::ifgt(0, 3 + 1 + 3),
                        BinOpKind::Ge => Instruction::iflt(0, 3 + 1 + 3),
                        BinOpKind::Gt => Instruction::ifle(0, 3 + 1 + 3),
                        _ => unreachable!(),
                    },
                    Instruction::iconst_1,
                    Instruction::goto(0, 3 + 1),
                    Instruction::iconst_0,
                ],
            },
            IntegerKind::I32 => match binop_kind {
                BinOpKind::Add => vec![Instruction::iadd],
                BinOpKind::Sub => vec![Instruction::isub],
                BinOpKind::Mul => vec![Instruction::imul],
                BinOpKind::Div => vec![Instruction::idiv],
                BinOpKind::Le | BinOpKind::Lt | BinOpKind::Ge | BinOpKind::Gt => vec![
                    match binop_kind {
                        BinOpKind::Lt => Instruction::if_icmpge(0, 3 + 1 + 3),
                        BinOpKind::Le => Instruction::if_icmpgt(0, 3 + 1 + 3),
                        BinOpKind::Ge => Instruction::if_icmplt(0, 3 + 1 + 3),
                        BinOpKind::Gt => Instruction::if_icmple(0, 3 + 1 + 3),
                        _ => unreachable!(),
                    },
                    Instruction::iconst_1,
                    Instruction::goto(0, 3 + 1),
                    Instruction::iconst_0,
                ],
                _ => unimplemented!(),
            },
            kind => unimplemented!("{:#?}", kind),
        },
        ty => unimplemented!("{:#?}", ty),
    });

    (left_instr, left_ty)
}

pub fn translate_assign(
    left: &P<Expr>,
    right: &P<Expr>,
    ctx: &mut StackFrameContext,
    class_file: &mut ClassFile,
) -> (Vec<Instruction>, TyKind) {
    match &left.kind {
        ExprKind::Path(Path { segments }) => {
            let ident = &segments[0].ident;
            let (id, ty) = ctx.get_local(&ident.name).clone();
            let id = id as u8;
            let (mut instr, _) = translate_expr(right, ctx, class_file, None);
            instr.push(match ty.kind {
                TyKind::Integer(ref kind) => match kind {
                    IntegerKind::USize => Instruction::lstore(id),
                    IntegerKind::I32 => Instruction::istore(id),
                    _ => unimplemented!(),
                },
                _ => unimplemented!(),
            });
            (instr, ty.kind)
        }
        ExprKind::Index(ident, index_expr) => {
            let (id, ty) = ctx.get_local(&ident.name).clone();
            let mut instr = vec![Instruction::aload(id as u8)]; // array_ref

            let (index_expr_instr, index_expr_ty) = translate_expr(
                &index_expr,
                ctx,
                class_file,
                None, // indices must be ints
            );
            instr.extend(index_expr_instr); // index
            instr.extend(auto_convert_types(
                &index_expr_ty,
                &TyKind::Integer(IntegerKind::I32),
            ));

            let (assign_right_instr, assign_right_ty) =
                translate_expr(right, ctx, class_file, None);
            instr.extend(assign_right_instr); // value
            instr.extend(auto_convert_types(
                &assign_right_ty,
                &TyKind::Integer(IntegerKind::USize),
            ));

            instr.push(Instruction::lastore); // array_ref[index] = value

            (instr, ty.kind)
        }
        _ => unimplemented!(),
    }
}
