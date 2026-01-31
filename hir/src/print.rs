use std::fmt::{self, Debug, Formatter, Write};

use num_bigint::BigInt;

use crate::*;

const IDENT_WIDTH: usize = 3;

pub struct HirPrinter<'a> {
   indent: usize,
   written: bool,
   buf: &'a mut (dyn Write + 'a),
}

impl<'a> HirPrinter<'a> {
   fn new(buf: &'a mut dyn Write) -> Self {
      Self::with_indent(0, buf)
   }

   fn with_indent(indent: usize, buf: &'a mut dyn Write) -> Self {
      Self {
         indent,
         written: false,
         buf,
      }
   }

   pub fn write_indent(&mut self) -> fmt::Result {
      if self.written {
         return Ok(());
      }

      self.written = true;

      let spaces = IDENT_WIDTH * self.indent;
      for _ in 0..spaces {
         self.buf.write_char(' ')?;
      }
      Ok(())
   }

   pub fn indent(&mut self) {
      self.indent += 1;
   }

   pub fn dedent(&mut self) {
      if self.indent > 0 {
         self.indent -= 1;
      }
   }

   pub fn print<T: HirPrint + ?Sized>(&mut self, x: &T) -> fmt::Result {
      x.print(self)
   }

   pub fn println<T: HirPrint + ?Sized>(&mut self, x: &T) -> fmt::Result {
      x.print(self)?;
      writeln!(self)
   }
}

impl Write for HirPrinter<'_> {
   fn write_char(&mut self, c: char) -> fmt::Result {
      self.write_indent()?;
      self.buf.write_char(c)?;
      if c == '\n' {
         self.written = false;
      }
      Ok(())
   }

   fn write_str(&mut self, s: &str) -> fmt::Result {
      for seg in s.split_inclusive('\n') {
         self.write_indent()?;
         self.buf.write_str(seg)?;
         if seg.ends_with('\n') {
            self.written = false;
         }
      }
      Ok(())
   }

   fn write_fmt(&mut self, args: fmt::Arguments<'_>) -> fmt::Result {
      self.write_indent()?;
      if let Some(s) = args.as_str() {
         self.write_str(s)
      } else {
         self.write_str(&args.to_string())
      }
   }
}

impl<T: HirPrint> HirPrint for Box<T> {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      self.as_ref().print(f)
   }
}

impl HirPrint for u8 {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      if self.is_ascii() {
         let c = *self as char;
         f.print(&c)
      } else {
         f.print(&format!("\\x{self:2X}"))
      }
   }
}

impl HirPrint for bool {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      f.write_indent()?;
      write!(f.buf, "{self}")
   }
}

impl HirPrint for char {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      f.write_char(*self)
   }
}

impl HirPrint for str {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      f.write_str(self)
   }
}

impl HirPrint for String {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      f.write_str(self)
   }
}

impl HirPrint for BigInt {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      f.write_indent()?;
      write!(f.buf, "{self}")
   }
}

pub trait HirPrint: Debug {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      write!(f, "{self:#?}")
   }
}

impl HirPrint for Ident {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      write!(f, "{}", self.name)
   }
}

impl HirPrint for SymId {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      write!(f, "@{}", self.as_usize())
   }
}

impl HirPrint for DestId {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      write!(f, "'{}", self.as_usize())
   }
}

impl HirPrint for Item {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      match &self.kind {
         ItemKind::FnDef(func) => f.print(func),
         ItemKind::TyDef(ty) => f.print(ty),
         ItemKind::Error => f.print("<error>"),
      }
   }
}

const PAREN: (char, char) = ('(', ')');
const BRACKET: (char, char) = ('[', ']');
const BRACE: (char, char) = ('{', '}');

fn print_group(
   f: &mut HirPrinter,
   (l, r): (char, char),
   cb: impl FnOnce(&mut HirPrinter) -> fmt::Result,
) -> fmt::Result {
   f.println(&l)?;
   f.indent();
   cb(f)?;
   f.dedent();
   f.print(&r)
}

fn print_list<T: HirPrint>(f: &mut HirPrinter, (l, r): (char, char), xs: &[T]) -> fmt::Result {
   match xs {
      [] | [_] => {
         f.print(&l)?;
         if let Some(x) = xs.first() {
            f.print(x)?;
         }
         f.print(&r)
      }
      xs => print_group(f, (l, r), |f| {
         for x in xs {
            f.print(x)?;
            f.write_str(",\n")?;
         }
         Ok(())
      }),
   }
}

fn print_fn_sign<Param: HirPrint>(
   f: &mut HirPrinter,
   side_effect: bool,
   params: &[Param],
   ret: &Ty,
) -> fmt::Result {
   f.print("fn ")?;
   if side_effect {
      f.print("mut ")?;
   }
   print_list(f, PAREN, params)?;
   f.print(" -> ")?;
   f.print(ret)
}

impl HirPrint for Body {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      match &self.kind {
         BodyKind::Expr(expr) => f.print(expr),
         BodyKind::CFfi(symbol) => {
            f.print("extern(\"C\", ")?;
            f.print(&format!("{symbol:?}"))?;
            f.print(")")
         }
         BodyKind::Error => f.print("<error>"),
      }
   }
}

impl HirPrint for FnDef {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      if let Some(name) = &self.name {
         f.print(name)?;
      }
      f.print(&self.sym_id)?;
      f.print(": ")?;
      print_fn_sign(f, self.side_effect, &self.params, &self.ret)?;
      f.print(" = ")?;
      f.print(&self.body)
   }
}

impl HirPrint for Param {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      if self.mutable {
         f.print("mut ")?;
      }
      f.print(&self.name)?;
      f.print(&self.sym_id)?;
      f.print(": ")?;
      f.print(&self.ty)
   }
}

impl HirPrint for TyDef {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      if let Some(name) = &self.name {
         f.print(name)?;
      }
      f.print(&self.sym_id)?;
      f.print(": type = ")?;
      f.print(&self.ty)
   }
}

impl HirPrint for Ty {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      match &self.kind {
         TyKind::Never => f.print("Never"),
         TyKind::Use(ident, sym_id) => {
            if let Some(ident) = ident {
               f.print(ident)?;
            }
            f.print(sym_id)
         }
         TyKind::Ref(pointee) => {
            f.print("&")?;
            f.print(pointee)
         }
         TyKind::Ptr(pointee) => {
            f.print("*")?;
            f.print(pointee)
         }
         TyKind::FnPtr(side_effect, params, ret) => {
            print_fn_sign(f, *side_effect, params, ret)?;
            Ok(())
         }
         TyKind::Tuple(tys) => print_list(f, PAREN, tys),
         TyKind::Struct(fields) => {
            f.print("{")?;
            for (i, field) in fields.iter().enumerate() {
               if i > 0 {
                  f.print(", ")?;
               }
               f.print(&field.ident)?;
               f.print(": ")?;
               f.print(&*field.ty)?;
            }
            f.print("}")
         }
         TyKind::Slice(ty) => {
            f.print("[")?;
            f.print(ty)?;
            f.print("]")
         }
         TyKind::Array(ty, len) => {
            f.print("[")?;
            f.print(ty)?;
            f.print("; ")?;
            f.print(len)?;
            f.print("]")
         }
         TyKind::Infer => f.print("_"),
         TyKind::Error => f.print("<error>"),
      }
   }
}

impl HirPrint for Pointee {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      if self.mutable {
         f.print("mut ")?;
      }
      f.print(&self.ty)
   }
}

impl HirPrint for Expr {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      match &self.kind {
         ExprKind::Use(ident, sym_id) => {
            if let Some(ident) = ident {
               f.print(ident)?;
            }
            f.print(sym_id)
         }
         ExprKind::Const(constant) => f.print(constant),
         ExprKind::Array(arr) => print_list(f, BRACKET, arr),
         ExprKind::Repeat(expr, len) => {
            f.print("[")?;
            expr.print_atom(f)?;
            f.print("; ")?;
            f.print(len)?;
            f.print("]")
         }
         ExprKind::Tuple(tup) => print_list(f, PAREN, tup),
         ExprKind::Struct(st) => print_list(f, BRACE, st),
         ExprKind::Cont(dest_id) => {
            f.print("cont ")?;
            f.print(dest_id)
         }
         ExprKind::Break(dest_id, expr) => {
            f.print("break ")?;
            f.print(dest_id)?;
            f.print(" ")?;
            expr.print_atom(f)
         }
         ExprKind::Block(block) => f.print(block),
         ExprKind::If(cond, then_branch, else_branch) => {
            f.print("if ")?;
            f.print(cond)?;
            f.print(" ")?;
            then_branch.print_branch(f)?;
            f.print(" else ")?;
            else_branch.print_branch(f)
         }
         ExprKind::Binary(op, lhs, rhs) => {
            lhs.print_atom(f)?;
            f.print(" ")?;
            f.print(op)?;
            f.print(" ")?;
            rhs.print_atom(f)
         }
         ExprKind::Unary(op, operand) => match op.kind.pos() {
            UnOpPos::Prefix => {
               f.print(op)?;
               operand.print_atom(f)
            }
            UnOpPos::Suffix => {
               operand.print_atom(f)?;
               f.print(op)
            }
         },
         ExprKind::Cast(expr, ty) => {
            expr.print_atom(f)?;
            f.print(" as ")?;
            f.print(ty)
         }
         ExprKind::Field(expr, field) => {
            expr.print_atom(f)?;
            f.print(".")?;
            f.print(field)
         }
         ExprKind::Index(expr, index) => {
            expr.print_atom(f)?;
            f.print("[")?;
            f.print(index)?;
            f.print("]")
         }
         ExprKind::RefOf(mutable, expr) => {
            f.print("&")?;
            if *mutable {
               f.print("mut ")?;
            }
            expr.print_atom(f)
         }
         ExprKind::Call(callee, args) => {
            callee.print_atom(f)?;
            print_list(f, PAREN, args)
         }
         ExprKind::Begin(expr) => {
            f.print("@begin(")?;
            f.print(expr)?;
            f.print(")")
         }
         ExprKind::End(expr) => {
            f.print("@end(")?;
            f.print(expr)?;
            f.print(")")
         }
         ExprKind::Error => f.print("<error>"),
      }
   }
}

impl Expr {
   pub fn is_atom(&self) -> bool {
      match &self.kind {
         ExprKind::Use(_, _)
         | ExprKind::Array(_)
         | ExprKind::Repeat(_, _)
         | ExprKind::Tuple(_)
         | ExprKind::Struct(_)
         | ExprKind::If(_, _, _)
         | ExprKind::Field(_, _)
         | ExprKind::Begin(_)
         | ExprKind::End(_)
         | ExprKind::Error => true,

         ExprKind::Cont(_)
         | ExprKind::Break(_, _)
         | ExprKind::Binary(_, _, _)
         | ExprKind::Unary(_, _)
         | ExprKind::Cast(_, _)
         | ExprKind::Index(_, _)
         | ExprKind::RefOf(_, _)
         | ExprKind::Call(_, _) => false,

         ExprKind::Const(constant) => constant.is_atom(),
         ExprKind::Block(block) => block.dest_id.is_none(),
      }
   }

   pub fn print_atom(&self, f: &mut HirPrinter) -> fmt::Result {
      if self.is_atom() {
         f.print(self)
      } else {
         f.print("(")?;
         f.print(self)?;
         f.print(")")
      }
   }

   pub fn print_branch(&self, f: &mut HirPrinter) -> fmt::Result {
      match &self.kind {
         ExprKind::Block(block) if block.dest_id.is_none() => f.print(self),
         ExprKind::Tuple(tup) if tup.is_empty() => f.print("{ () }"),
         _ => print_group(f, BRACE, |f| f.println(self)),
      }
   }
}

impl HirPrint for Block {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      if let Some(dest_id) = &self.dest_id {
         f.print(dest_id)?;
         f.print(": ")?;
      }
      print_group(f, BRACE, |f| {
         for stmt in &self.stmts {
            f.print(stmt)?;
            f.println("")?;
         }
         f.print(&self.expr)?;
         f.println("")
      })
   }
}

impl HirPrint for ExprField {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      f.print(&self.ident)?;
      f.print(": ")?;
      f.print(&self.expr)
   }
}

impl HirPrint for Stmt {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      match &self.kind {
         StmtKind::LocalDef(def) => f.print(def)?,
         StmtKind::Item(item) => f.print(item)?,
         StmtKind::Expr(expr) => f.print(expr)?,
         StmtKind::Error => f.print("<error>")?,
      }
      f.print(";")
   }
}

impl HirPrint for LocalDef {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      if self.mutable {
         f.print("mut ")?;
      }
      if let Some(name) = &self.name {
         f.print(name)?;
      }
      f.print(&self.sym_id)?;
      f.print(": ")?;
      f.print(&self.ty)?;
      if let Some(init) = &self.init {
         f.print(" = ")?;
         f.print(init)?;
      }
      Ok(())
   }
}

impl HirPrint for BinOp {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      use BinOpKind::*;
      let op = match self.kind {
         Add => "+",
         Sub => "-",
         Mul => "*",
         Div => "/",
         Rem => "%",
         Shl => "<<",
         Shr => ">>",
         Eq => "==",
         Lt => "<",
         Le => "<=",
         Ne => "!=",
         Gt => ">",
         Ge => ">=",
         Way3 => "<=>",
         Ass => "=",
         AddAss => "+=",
         SubAss => "-=",
         MulAss => "*=",
         DivAss => "/=",
         RemAss => "%=",
         ShlAss => "<<=",
         ShrAss => ">>=",
         Range => "~",
         RangeInc => "~=",
      };
      f.print(op)
   }
}

impl HirPrint for UnOp {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      use UnOpKind::*;
      let op = match self.kind {
         Deref => "*",
         Not => "!",
         Neg => "-",
         Inc => "++",
         Dec => "--",
      };
      f.print(op)
   }
}

impl HirPrint for Constant {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      match &self.kind {
         ConstantKind::Char(char) => {
            f.print("'")?;
            f.print(char)?;
            f.print("'")
         }
         ConstantKind::Str(str) => {
            f.print("\"")?;
            f.print(str)?;
            f.print("\"")
         }
         ConstantKind::Byte(byte) => {
            f.print("b\"")?;
            f.print(byte)?;
            f.print("\"")
         }
         ConstantKind::Bytes(bytes) => {
            f.print("b\"")?;
            for byte in bytes {
               f.print(byte)?;
            }
            f.print("\"")
         }
         ConstantKind::Int(int) => f.print(int),
         ConstantKind::Bool(bool) => f.print(bool),
         ConstantKind::Expr(expr) => f.print(expr),
      }
   }
}

impl Constant {
   pub fn is_atom(&self) -> bool {
      match &self.kind {
         ConstantKind::Expr(expr) => expr.is_atom(),
         _ => true,
      }
   }
}

impl HirPrint for Vec<Item> {
   fn print(&self, f: &mut HirPrinter) -> fmt::Result {
      for (i, item) in self.iter().enumerate() {
         if i != 0 {
            f.println("")?;
         }
         f.print(item)?;
         f.println(";")?;
      }
      Ok(())
   }
}

impl Debug for Hir {
   fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
      if f.alternate() {
         let mut printer = HirPrinter::new(f);
         printer.print(&self.items)
      } else {
         Debug::fmt(&self.items, f)
      }
   }
}
