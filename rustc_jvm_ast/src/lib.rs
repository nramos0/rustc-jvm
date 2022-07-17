pub mod translation;

use std::cell::RefCell;
use std::fmt::{self, Debug, Display};
use std::ops::{Deref, DerefMut};

pub struct P<T: ?Sized> {
    pub ptr: Box<T>,
}

impl<T> P<T> {
    pub fn new(inner: T) -> Self {
        P {
            ptr: Box::new(inner),
        }
    }
}

pub trait IntoPtr
where
    Self: Sized,
{
    fn p(self) -> P<Self> {
        P::new(self)
    }
}
impl IntoPtr for Expr {}
impl IntoPtr for Block {}
impl IntoPtr for Stmt {}
impl IntoPtr for Local {}
impl IntoPtr for Item {}
impl IntoPtr for FnDecl {}
impl IntoPtr for Ty {}
impl IntoPtr for Range {}

impl<T: ?Sized> Deref for P<T> {
    type Target = T;

    fn deref(&self) -> &T {
        &self.ptr
    }
}

impl<T: ?Sized> DerefMut for P<T> {
    fn deref_mut(&mut self) -> &mut T {
        &mut self.ptr
    }
}
impl<T: 'static + Clone> Clone for P<T> {
    fn clone(&self) -> P<T> {
        P {
            ptr: Box::new((**self).clone()),
        }
    }
}

impl<T: ?Sized + Debug> Debug for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Debug::fmt(&self.ptr, f)
    }
}

impl<T: Display> Display for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Display::fmt(&**self, f)
    }
}

impl<T> fmt::Pointer for P<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Pointer::fmt(&self.ptr, f)
    }
}

thread_local! {
    pub static SYMBOL_ADDR_OFFSET_KEY: RefCell<usize> = RefCell::new(0);
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub struct Range {
    pub start: P<Expr>,
    pub end: P<Expr>,
}

#[derive(Debug, Clone)]
pub enum LocalKind {
    /// Local declaration.
    /// Example: `let x;`
    Decl,
    /// Local declaration with an initializer.
    /// Example: `let x = y;`
    Init(P<Expr>),
    /// Local declaration with an initializer and an `else` clause.
    /// Example: `let Some(x) = y else { return };`
    InitElse(P<Expr>, P<Block>),
}

/// Local represents a `let` statement, e.g., `let <pat>:<ty> = <expr>;`.
#[derive(Debug, Clone)]
pub struct Local {
    pub ident: Ident,
    pub ty: Ty,
    pub kind: LocalKind,
}

#[derive(Debug, Clone)]
pub struct Ident {
    pub name: String,
    pub kind: IdentKind,
}

#[derive(Debug, Clone)]
pub enum IdentKind {
    Keyword,
    Var(usize),
    Lit(LitKind),
    Fn,
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// A local (let) binding.
    Local(P<Local>),
    /// An item definition.
    Item(P<Item>),
    /// Expr without trailing semi-colon.
    Expr(P<Expr>),
    /// Expr with a trailing semi-colon.
    Semi(P<Expr>),
    /// Just a trailing semi-colon.
    Empty,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub struct FnSig {
    pub decl: P<FnDecl>,
}

#[derive(Debug, Clone)]
pub enum FnRetTy {
    /// Returns type is not specified.
    ///
    /// Functions default to `()` and closures default to inference.
    Default,
    /// Everything else.
    Ty(P<Ty>),
}

#[derive(Debug, Clone)]
pub struct Param {
    pub ty: P<Ty>,
    pub ident: Ident,
}

#[derive(Debug, Clone)]
pub struct FnDecl {
    pub inputs: Vec<Param>,
    pub output: FnRetTy,
}

use jvm_bytecode::class_file::descriptor::{
    DescriptorBuilder, FieldTypeKind, MethodDescriptor, ToDescriptor,
};
impl ToDescriptor<MethodDescriptor> for FnDecl {
    fn to_descriptor(&self) -> String {
        let mut builder = DescriptorBuilder::method().params();

        fn get_field_type_kind(param: &P<Ty>) -> FieldTypeKind {
            match param.kind {
                TyKind::Slice(_) => todo!(),
                TyKind::Array(ref ty, _) => {
                    FieldTypeKind::ArrRef(Box::new(get_field_type_kind(ty)))
                }
                TyKind::Rptr(_) => todo!(),
                TyKind::Never => todo!(),
                TyKind::Tup(_) => todo!(),
                TyKind::Path(_) => todo!(),
                TyKind::Integer(ref kind) => match kind {
                    IntegerKind::I8 => FieldTypeKind::Char,
                    IntegerKind::I16 => FieldTypeKind::Short,
                    IntegerKind::I32 => FieldTypeKind::Int,
                    IntegerKind::I64 => FieldTypeKind::Long,
                    IntegerKind::I128 => todo!(),
                    IntegerKind::ISize => todo!(),
                    IntegerKind::U8 => FieldTypeKind::Char,
                    IntegerKind::U16 => FieldTypeKind::Short,
                    IntegerKind::U32 => FieldTypeKind::Int,
                    IntegerKind::U64 => FieldTypeKind::Long,
                    IntegerKind::U128 => todo!(),
                    IntegerKind::USize => FieldTypeKind::Long,
                },
                TyKind::Float(_) => todo!(),
                TyKind::Bool => todo!(),
                TyKind::Paren(_) => todo!(),
                TyKind::Typeof(_) => todo!(),
                TyKind::Infer => todo!(),
                TyKind::ImplicitSelf => todo!(),
                TyKind::SelfVal => todo!(),
                TyKind::SelfRef => todo!(),
                TyKind::SelfRefMut => todo!(),
                TyKind::Err => todo!(),
            }
        }

        for param in &self.inputs {
            builder = match param.ty.kind {
                TyKind::Slice(_) => todo!(),
                TyKind::Array(ref ty, _) => builder.arr_ref(get_field_type_kind(ty)),
                TyKind::Rptr(_) => todo!(),
                TyKind::Never => todo!(),
                TyKind::Tup(_) => todo!(),
                TyKind::Path(_) => todo!(),
                TyKind::Integer(ref kind) => match kind {
                    IntegerKind::I8 => builder.char(),
                    IntegerKind::I16 => builder.short(),
                    IntegerKind::I32 => builder.int(),
                    IntegerKind::I64 => todo!(),
                    IntegerKind::I128 => todo!(),
                    IntegerKind::ISize => todo!(),
                    IntegerKind::U8 => builder.char(),
                    IntegerKind::U16 => builder.short(),
                    IntegerKind::U32 => builder.int(),
                    IntegerKind::U64 => builder.long(),
                    IntegerKind::U128 => todo!(),
                    IntegerKind::USize => builder.long(),
                },
                TyKind::Float(ref kind) => match kind {
                    FloatKind::F32 => builder.float(),
                    FloatKind::F64 => builder.double(),
                },
                TyKind::Bool => builder.bool(),
                TyKind::Paren(_) => todo!(),
                TyKind::Typeof(_) => todo!(),
                TyKind::Infer => todo!(),
                TyKind::ImplicitSelf => todo!(),
                TyKind::SelfVal => todo!(),
                TyKind::SelfRef => todo!(),
                TyKind::SelfRefMut => todo!(),
                TyKind::Err => todo!(),
            };
        }

        let builder = builder.ret_type();

        match self.output {
            FnRetTy::Default => builder.void(),
            FnRetTy::Ty(_) => todo!(),
        }
        .descriptor()
    }
}

#[derive(Debug, Clone)]
pub struct Fn {
    pub name: String,
    pub sig: FnSig,
    pub body: Option<P<Block>>,
}

#[derive(Debug, Clone)]
pub struct Ty {
    pub kind: TyKind,
    pub size: usize,
}

impl Ty {
    pub fn u8() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::U8),
            size: 1,
        }
    }
    pub fn u16() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::U16),
            size: 2,
        }
    }
    pub fn u32() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::U32),
            size: 4,
        }
    }
    pub fn u64() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::U64),
            size: 8,
        }
    }
    pub fn u128() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::U128),
            size: 16,
        }
    }
    pub fn i8() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::I8),
            size: 1,
        }
    }
    pub fn i16() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::I16),
            size: 2,
        }
    }
    pub fn i32() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::I32),
            size: 4,
        }
    }
    pub fn i64() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::I64),
            size: 8,
        }
    }
    pub fn i128() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::I128),
            size: 16,
        }
    }
    pub fn usize() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::USize),
            size: 8,
        }
    }
    pub fn isize() -> Self {
        Self {
            kind: TyKind::Integer(IntegerKind::ISize),
            size: 8,
        }
    }
    pub fn f32() -> Self {
        Self {
            kind: TyKind::Float(FloatKind::F32),
            size: 4,
        }
    }
    pub fn f64() -> Self {
        Self {
            kind: TyKind::Float(FloatKind::F64),
            size: 8,
        }
    }
    pub fn bool() -> Self {
        Self {
            kind: TyKind::Bool,
            size: 4,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AnonConst {
    pub value: P<Expr>,
}

#[derive(Debug, Clone)]
pub enum Mutability {
    Mut,
    Not,
}

#[derive(Debug, Clone)]
pub struct MutTy {
    pub ty: P<Ty>,
    pub mutbl: Mutability,
}

#[derive(Debug, Clone)]
pub enum TyKind {
    /// A variable-length slice (`[T]`).
    Slice(P<Ty>),
    /// A fixed length array (`[T; n]`).
    Array(P<Ty>, usize),
    /// A reference (`&'a T` or `&'a mut T`).
    Rptr(MutTy),
    /// The never type (`!`).
    Never,
    /// A tuple (`(A, B, C, D,...)`).
    Tup(Vec<P<Ty>>),
    /// A path (`module::module::...::Type`), optionally
    /// "qualified", e.g., `<Vec<T> as SomeTrait>::SomeType`.
    ///
    /// Type parameters are stored in the `Path` itself.
    Path(Path),
    /// An integer
    Integer(IntegerKind),
    /// A float
    Float(FloatKind),
    /// A bool
    Bool,
    /// No-op; kept solely so that we can pretty-print faithfully.
    Paren(P<Ty>),
    /// Unused for now.
    Typeof(AnonConst),
    /// This means the type should be inferred instead of it having been
    /// specified. This can appear anywhere in a type.
    Infer,
    /// Inferred type of a `self` or `&self` argument in a method.
    ImplicitSelf,
    SelfVal,
    SelfRef,
    SelfRefMut,
    /// Placeholder for a kind that has failed to be defined.
    Err,
}

#[derive(Debug, Clone)]
pub struct Item<K = ItemKind> {
    pub kind: K,
}

pub type AssocItem = Item<AssocItemKind>;

#[derive(Debug, Clone)]
pub enum AssocItemKind {
    /// An associated function.
    Fn(Box<Fn>),
}

#[derive(Debug, Clone)]
pub enum VariantData {
    /// Struct variant.
    ///
    /// E.g., `Bar { .. }` as in `enum Foo { Bar { .. } }`.
    Struct(Vec<FieldDef>, bool),
}

#[derive(Debug, Clone)]
pub struct FieldDef {
    pub ident: Ident,
    pub ty: P<Ty>,
}

#[derive(Debug, Clone)]
pub enum ItemKind {
    /// A function declaration (`fn`).
    ///
    /// E.g., `fn foo(bar: usize) -> usize { .. }`.
    Fn(Box<Fn>),
    /// A struct definition (`struct`).
    ///
    /// E.g., `struct Foo<A> { x: A }`.
    Struct(VariantData),
    /// A trait declaration (`trait`).
    ///
    /// E.g., `trait Foo { .. }`, `trait Foo<T> { .. }` or `auto trait Foo {}`.
    Trait(Box<Trait>),
    /// An implementation.
    ///
    /// E.g., `impl<A> Foo<A> { .. }` or `impl<A> Trait for Foo<A> { .. }`.
    Impl(Box<Impl>),
}

#[derive(Debug, Clone)]
pub struct Trait {
    pub items: Vec<P<AssocItem>>,
}

#[derive(Debug, Clone)]
pub struct Impl {
    pub self_ty: P<Ty>,
    pub items: Vec<P<AssocItem>>,
}

#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
}

#[derive(Debug, Clone)]
pub enum BinOpKind {
    /// The `+` operator (addition)
    Add,
    /// The `-` operator (subtraction)
    Sub,
    /// The `*` operator (multiplication)
    Mul,
    /// The `/` operator (division)
    Div,
    /// The `%` operator (modulus)
    Rem,
    /// The `&&` operator (logical and)
    And,
    /// The `||` operator (logical or)
    Or,
    /// The `^` operator (bitwise xor)
    BitXor,
    /// The `&` operator (bitwise and)
    BitAnd,
    /// The `|` operator (bitwise or)
    BitOr,
    /// The `<<` operator (shift left)
    Shl,
    /// The `>>` operator (shift right)
    Shr,
    /// The `==` operator (equality)
    Eq,
    /// The `<` operator (less than)
    Lt,
    /// The `<=` operator (less than or equal to)
    Le,
    /// The `!=` operator (not equal to)
    Ne,
    /// The `>=` operator (greater than or equal to)
    Ge,
    /// The `>` operator (greater than)
    Gt,
}

#[derive(Debug, Clone)]
pub enum UnOpKind {
    /// The `*` operator for dereferencing
    Deref,
    /// The `!` operator for logical inversion
    Not,
    /// The `-` operator for negation
    Neg,
    /// The `&` operator for taking a shared reference
    Ref,
    /// The `& mut` operator for taking an exclusive reference
    RefMut,
}

#[derive(Debug, Clone)]
pub enum IntegerKind {
    I8,
    I16,
    I32,
    I64,
    I128,
    ISize,
    U8,
    U16,
    U32,
    U64,
    U128,
    USize,
}

#[derive(Debug, Clone)]
pub enum FloatKind {
    F32,
    F64,
}

#[derive(Debug, Clone)]
pub enum IntegerKindWithVal {
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    I128(i128),
    ISize(isize),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    U128(u128),
    USize(usize),
}

#[derive(Debug, Clone)]
pub enum FloatKindWithVal {
    F32(f32),
    F64(f64),
}

#[derive(Debug, Clone)]
pub enum LitKind {
    Bool(bool), // AST only, must never appear in a `Token`
    Byte,
    Char,
    Integer(IntegerKind),
    Float(FloatKind),
    Str,
    StrRaw(u16), // raw string delimited by `n` hash symbols
    ByteStr,
    ByteStrRaw(u16), // raw byte string delimited by `n` hash symbols
    Err,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub val: String,
    pub kind: LitKind,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub segments: Vec<PathSegment>,
}

#[derive(Debug, Clone)]
pub struct PathSegment {
    /// The identifier portion of this path segment.
    pub ident: Ident,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    /// A function call
    ///
    /// The first field resolves to the function itself,
    /// and the second field is the list of arguments.
    /// This also represents calling the constructor of
    /// tuple-like ADTs such as tuple structs and enum variants.
    Call(Ident, Vec<P<Expr>>),
    /// An array (`[a, b, c, d]`)
    Array(Vec<P<Expr>>),
    /// A while loop.
    ///
    /// `while expr { block }`
    While(P<Expr>, P<Block>),

    If(P<Expr>, P<Block>),
    /// A for .. in .. loop
    ///
    /// `for ident in expr { block }`
    For(Ident, P<Range>, P<Block>),
    /// An assignment (`a = foo()`).
    Assign(P<Expr>, P<Expr>),
    /// A binary operation (e.g., `a + b`, `a * b`).
    Binary(BinOpKind, P<Expr>, P<Expr>),
    /// A unary operation (e.g., `+a`, `-a`).
    Unary(UnOpKind, P<Expr>),
    /// A literal (e.g., `1`, `"foo"`).
    Lit(Lit),
    /// An indexing operation (e.g., `foo[2]`).
    Index(Ident, P<Expr>),
    /// Variable reference, possibly containing `::` and/or type
    /// parameters (e.g., `foo::bar::<baz>`).
    ///
    /// Optionally "qualified" (e.g., `<Vec<T> as SomeTrait>::SomeType`).
    Path(Path),
    Empty,
}
