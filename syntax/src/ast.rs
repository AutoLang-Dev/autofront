use std::fmt::Debug;

use macros::{AstPrint, Span};

use crate::{Tok, token::*};

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeInfer(pub Tok![_]);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeIdent(pub Ident);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeFn {
   pub fn_tok: Tok![fn],
   pub mut_tok: Option<Tok![mut]>,
   pub params: TypeTuple,
   pub ret: Ret,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeRef {
   pub ref_tok: Tok![&],
   pub mut_tok: Option<Tok![mut]>,
   pub pointee: Box<Type>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypePtr {
   pub ptr_tok: Tok![*],
   pub mut_tok: Option<Tok![mut]>,
   pub pointee: Box<Type>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeParen(pub Tok![(Box<Type>)]);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeTuple(pub Tok![(Type,)]);

#[derive(Debug, Clone, Span)]
pub struct ArrayInner {
   pub elem: Box<Type>,
   pub semi_tok: Tok![;],
   pub lens: Tok![Expr,],
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeArray(pub Tok![[ArrayInner]]);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeSlice(pub Tok![[Box<Type>]]);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct StructField {
   pub name: Ident,
   pub colon_tok: Tok![:],
   pub lens: Box<Type>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeStruct(pub Tok![{StructField,}]);

#[derive(Debug, Clone, AstPrint, Span)]
pub enum Type {
   Infer(TypeInfer),
   Ident(TypeIdent),
   Paren(TypeParen),
   Fn(TypeFn),
   Ref(TypeRef),
   Ptr(TypePtr),
   Tuple(TypeTuple),
   Array(TypeArray),
   Slice(TypeSlice),
   Struct(TypeStruct),

   Error(Error),
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprRange {
   pub lhs: Box<Expr>,
   pub op: RangeOp,
   pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprTuple(pub Tok![(Expr,)]);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprArray(pub Tok![[Expr,]]);

#[derive(Debug, Clone, Span)]
pub struct RepeatInner {
   pub elem: Box<Expr>,
   pub semi_tok: Tok![;],
   pub lens: Tok![Expr,],
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprRepeat(pub Tok![[RepeatInner]]);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct FieldInit {
   pub colon_tok: Tok![:],
   pub value: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint)]
pub struct FieldValue {
   pub name: Ident,
   pub init: Option<FieldInit>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprStruct(pub Tok![{FieldValue,}]);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprFn {
   pub sign: FnSign,
   pub eq_tok: Tok![=],
   pub body: FnBody,
}

#[derive(Debug, Clone, AstPrint)]
pub struct FnSign {
   pub fn_tok: Tok![fn],
   pub mut_tok: Option<Tok![mut]>,
   pub params: Option<Params>,
   pub ret: Option<Ret>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Params(pub Tok![(Param,)]);

#[derive(Debug, Clone, AstPrint)]
pub struct Param {
   pub mut_tok: Option<Tok![mut]>,
   pub name: Ident,
   pub colon_tok: Tok![:],
   pub ty: Box<Type>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Ret {
   pub arrow_tok: Tok![->],
   pub ty: Box<Type>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub enum FnBody {
   Expr(Box<Expr>),
   Ffi(Ffi),
   Asm(Asm),
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Ffi {
   pub extern_tok: Tok![extern],
   pub abi: Tok![(Abi)],
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Abi {
   pub abi: LitStr,
   pub comma_tok: Tok![,],
   pub symbol: LitStr,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Asm {
   pub extern_tok: Tok![asm],
   pub ir: Tok![{ LitStr }],
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprLit {
   pub lit: Lit,
   pub suffix: Option<Suffix>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub enum Member {
   Ident(Ident),
   Index(LitInt),
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprIdent(pub Ident);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprParen(pub Tok![(Box<Expr>)]);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprField {
   pub base: Box<Expr>,
   pub dot_tok: Tok![.],
   pub member: Member,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprAssign {
   pub lhs: Box<Expr>,
   pub op: AssignOp,
   pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprCmp(pub Separated<Expr, CmpOp>);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprShift {
   pub lhs: Box<Expr>,
   pub op: ShiftOp,
   pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprAdd {
   pub lhs: Box<Expr>,
   pub op: AddOp,
   pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprMul {
   pub lhs: Box<Expr>,
   pub op: MulOp,
   pub rhs: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprLogical(pub Separated<Expr, LogicalOp>);

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprRef {
   pub ref_tok: Tok![&],
   pub mut_tok: Option<Tok![mut]>,
   pub pointee: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprDeref {
   pub deref_tok: Tok![*],
   pub pointee: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprPrefix {
   pub op: PrefixOp,
   pub operand: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprSuffix {
   pub operand: Box<Expr>,
   pub op: SuffixOp,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprCast {
   pub operand: Box<Expr>,
   pub as_tok: Tok![as],
   pub ty: Box<Type>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprCall {
   pub callee: Box<Expr>,
   pub args: ExprTuple,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ExprIndex {
   pub base: Box<Expr>,
   pub indices: Tok![[Expr,]],
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprCase {
   pub label: Option<Labelled>,
   pub case_tok: Tok![case],
   pub arms: Tok![{CaseArm,}],
}

#[derive(Debug, Clone, AstPrint)]
pub struct CaseArm {
   pub label: Option<Labelled>,
   pub cond: Cond,
   pub eq_tok: Tok![=],
   pub value: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprIf {
   pub label: Option<Labelled>,
   pub if_tok: Tok![if],
   pub cond: Box<Expr>,
   pub then_branch: Block,
   pub else_branch: Option<ElseBranch>,
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprWhile {
   pub label: Option<Labelled>,
   pub while_tok: Tok![while],
   pub cond: Box<Expr>,
   pub body: Block,
   pub exit: Option<ElseBranch>,
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprFor {
   pub label: Option<Labelled>,
   pub for_tok: Tok![for],
   pub cond: Box<Expr>,
   pub in_tok: Tok![in],
   pub range: Box<Expr>,
   pub body: Block,
   pub exit: Option<ElseBranch>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct ElseBranch {
   pub else_tok: Tok![else],
   pub body: Block,
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprBlock {
   pub label: Option<Labelled>,
   pub block: Block,
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprReturn {
   pub return_tok: Tok![return],
   pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprBreak {
   pub break_tok: Tok![break],
   pub label: Option<Label>,
   pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, AstPrint)]
pub struct ExprCont {
   pub cont_tok: Tok![cont],
   pub label: Option<Label>,
   pub value: Option<Box<Expr>>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Block(
   pub  Tok![{
      Stmt;
   }],
);

#[derive(Debug, Clone, AstPrint, Span)]
pub enum Stmt {
   Def(Def),
   Expr(Expr),

   Error(Error),
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Labelled {
   pub label: Label,
   pub colon: Tok![:],
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Cond(pub Box<Expr>);

#[derive(Debug, Clone, AstPrint, Span)]
pub enum Expr {
   Range(ExprRange),
   Tuple(ExprTuple),
   Array(ExprArray),
   Repeat(ExprRepeat),
   Struct(ExprStruct),
   Fn(ExprFn),
   Lit(ExprLit),

   Ident(ExprIdent),
   Paren(ExprParen),
   Field(ExprField),
   Assign(ExprAssign),
   Cmp(ExprCmp),
   Shift(ExprShift),
   Add(ExprAdd),
   Mul(ExprMul),
   And(ExprLogical),
   Ref(ExprRef),
   Deref(ExprDeref),
   Prefix(ExprPrefix),
   Suffix(ExprSuffix),
   Cast(ExprCast),
   Call(ExprCall),
   Index(ExprIndex),

   Case(ExprCase),
   If(ExprIf),
   While(ExprWhile),
   For(ExprFor),
   Block(ExprBlock),
   Return(ExprReturn),
   Break(ExprBreak),
   Cont(ExprCont),

   Error(Error),
}

#[derive(Debug, Clone, AstPrint)]
pub struct LocalDef {
   pub mut_tok: Option<Tok![mut]>,
   pub name: Ident,
   pub colon_tok: Tok![:],
   pub ty: Option<Box<Type>>,
   pub init: Option<LocalInit>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct LocalInit {
   pub eq_tok: Tok![=],
   pub value: Box<Expr>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct FnDef {
   pub name: Ident,
   pub colon_tok: Tok![:],
   pub function: ExprFn,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct TypeDef {
   pub name: Ident,
   pub colon_tok: Tok![:],
   pub type_tok: Tok![type],
   pub eq_tok: Tok![=],
   pub ty: Box<Type>,
}

#[derive(Debug, Clone, AstPrint, Span)]
pub enum Def {
   Local(LocalDef),
   Fn(FnDef),
   Type(TypeDef),

   Error(Error),
}

#[derive(Debug, Clone, AstPrint, Span)]
pub enum GlobalItem {
   Def(Box<Def>),
   Asm(Asm),

   Error(Error),
}

#[derive(Debug, Clone, AstPrint, Span)]
pub struct Root(pub Separated<GlobalItem, Tok![;]>);

#[derive(Clone, Span)]
pub struct Ast {
   pub root: Root,
}
