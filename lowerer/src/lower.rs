use crate::*;
use common::span::Span;
use hir::*;
use syntax::{ast, span::Spanned, token};

pub fn lower_ast(ctx: &mut LoweringContext, ast: &ast::Ast) -> Hir {
   let mut items = Vec::new();

   for item in ast.root.0.vals() {
      if let ast::GlobalItem::Def(def) = item {
         match def.as_ref() {
            ast::Def::Fn(def) => {
               ctx.define_local(Some(def.name.ident.clone()), SymKind::Fn, def.name.span());
            }
            ast::Def::Type(def) => {
               ctx.define_local(Some(def.name.ident.clone()), SymKind::Type, def.name.span());
            }
            _ => (),
         }
      }
   }

   for item in ast.root.0.vals() {
      let item = lower_global_item(ctx, item);
      items.push(item);
   }

   Hir { items }
}

fn lower_global_item(ctx: &mut LoweringContext, item: &ast::GlobalItem) -> Item {
   match item {
      ast::GlobalItem::Def(def) => {
         return lower_def_as_item(ctx, def);
      }
      ast::GlobalItem::Asm(asm) => {
         ctx.diag(UnimplementedAsm::new(asm.span()));
      }
      _ => (),
   }

   Item {
      kind: ItemKind::Error,
      span: item.span(),
   }
}

fn lower_type(ctx: &mut LoweringContext, ty: &ast::Type) -> Ty {
   let span = ty.span();
   let kind = match ty {
      ast::Type::Infer(_) => TyKind::Infer,

      ast::Type::Ident(ident) => {
         let name = ident.0.ident.clone();
         if let Some(symbol) = ctx.lookup_across(&name) {
            if symbol.kind != SymKind::Type {
               ctx.diag(NotAType::new(name.clone(), span));
               TyKind::Infer
            } else {
               TyKind::Use(Some(Ident { name, span }), symbol.id)
            }
         } else {
            ctx.diag(NotFound::new(name, span));
            TyKind::Infer
         }
      }

      ast::Type::Paren(paren) => {
         return lower_type(ctx, &paren.0.inner);
      }

      ast::Type::Tuple(tuple) => {
         let types = tuple.0.inner.vals();
         let types = types.map(|t| lower_type(ctx, t)).collect();
         TyKind::Tuple(types)
      }

      ast::Type::Ref(ref_ty) => {
         let mutable = ref_ty.mut_tok.is_some();
         let pointee = lower_type(ctx, &ref_ty.pointee);
         TyKind::Ref(Pointee {
            mutable,
            ty: Box::new(pointee),
         })
      }

      ast::Type::Ptr(ptr_ty) => {
         let mutable = ptr_ty.mut_tok.is_some();
         let pointee = lower_type(ctx, &ptr_ty.pointee);
         TyKind::Ptr(Pointee {
            mutable,
            ty: Box::new(pointee),
         })
      }

      ast::Type::Slice(slice) => {
         let elem = lower_type(ctx, &slice.0.inner);
         TyKind::Slice(Box::new(elem))
      }

      ast::Type::Array(array) => {
         let inner = &array.0.inner;
         let mut elem_ty = lower_type(ctx, &inner.elem);

         let lens: Vec<_> = inner.lens.vals().collect();
         for len_expr in lens.iter().rev() {
            let len = lower_const_expr(ctx, len_expr);
            elem_ty = Ty {
               kind: TyKind::Array(Box::new(elem_ty), len),
               span,
            };
         }

         return elem_ty;
      }

      ast::Type::Fn(fn_ty) => {
         let sign = lower_fn_sign_type(ctx, fn_ty);
         TyKind::FnPtr(sign.0, sign.1, sign.2)
      }

      ast::Type::Struct(struct_ty) => {
         let fields = struct_ty
            .0
            .inner
            .vals()
            .map(|field| TyField {
               ident: Ident {
                  name: field.name.ident.clone(),
                  span: field.name.span(),
               },
               ty: Box::new(lower_type(ctx, &field.lens)),
            })
            .collect();
         TyKind::Struct(fields)
      }

      ast::Type::Error(_) => TyKind::Error,
   };

   Ty { kind, span }
}

fn lower_fn_sign_type(ctx: &mut LoweringContext, fn_ty: &ast::TypeFn) -> (bool, Vec<Ty>, Box<Ty>) {
   let side_effect = fn_ty.mut_tok.is_some();
   let params = fn_ty
      .params
      .0
      .inner
      .vals()
      .map(|p| lower_type(ctx, p))
      .collect();
   let ret = lower_type(ctx, &fn_ty.ret.ty);

   (side_effect, params, Box::new(ret))
}

fn lower_const_expr(ctx: &mut LoweringContext, expr: &ast::Expr) -> Constant {
   let span = expr.span();
   let kind = match expr {
      ast::Expr::Lit(lit) => lower_literal(&lit.lit),
      _ => {
         let hir_expr = lower_expr(ctx, expr);
         ConstantKind::Expr(Box::new(hir_expr))
      }
   };

   Constant { kind, span }
}

fn lower_literal(lit: &token::Lit) -> ConstantKind {
   use token::Lit;
   match lit {
      Lit::Char(c) => ConstantKind::Char(c.char),
      Lit::Str(s) => ConstantKind::Str(s.str.clone()),
      Lit::Byte(b) => ConstantKind::Byte(b.byte),
      Lit::Bytes(bs) => ConstantKind::Bytes(bs.bytes.clone()),
      Lit::Int(i) => ConstantKind::Int(i.int.clone()),
      Lit::Bool(b) => ConstantKind::Bool(b.bool),
   }
}

fn lower_expr(ctx: &mut LoweringContext, expr: &ast::Expr) -> Expr {
   let span = expr.span();
   let kind = match expr {
      ast::Expr::Lit(lit) => {
         let constant = Constant {
            kind: lower_literal(&lit.lit),
            span: lit.lit.span(),
         };
         ExprKind::Const(constant)
      }

      ast::Expr::Ident(ident) => 'blk: {
         let name = ident.0.ident.clone();
         if let Some(symbol) = ctx.lookup_across(&name) {
            if symbol.kind == SymKind::Type {
               ctx.diag(TypeAsValue::new(name.clone(), span));
            } else {
               let ident = Ident { name, span };
               break 'blk ExprKind::Use(Some(ident), symbol.id);
            }
         } else {
            ctx.diag(NotFound::new(name, span));
         }
         ExprKind::Error
      }

      ast::Expr::Paren(paren) => {
         return lower_expr(ctx, &paren.0.inner);
      }

      ast::Expr::Tuple(tuple) => {
         let exprs = tuple.0.inner.vals();
         let exprs = exprs.map(|e| lower_expr(ctx, e)).collect();
         ExprKind::Tuple(exprs)
      }

      ast::Expr::Array(array) => {
         let exprs = array.0.inner.vals();
         let exprs = exprs.map(|e| lower_expr(ctx, e)).collect();
         ExprKind::Array(exprs)
      }

      ast::Expr::Repeat(repeat) => {
         let inner = &repeat.0.inner;
         let elem = lower_expr(ctx, &inner.elem);
         let first = inner.lens.vals().next();
         let len = lower_const_expr(ctx, first.unwrap());
         ExprKind::Repeat(Box::new(elem), len)
      }

      ast::Expr::Struct(struct_expr) => {
         let fields = struct_expr
            .0
            .inner
            .vals()
            .map(|f| lower_field_value(ctx, f))
            .collect();
         ExprKind::Struct(fields)
      }

      ast::Expr::Field(field) => {
         let base = lower_expr(ctx, &field.base);
         let ident = lower_member(&field.member);
         ExprKind::Field(Box::new(base), ident)
      }

      ast::Expr::Index(index) => {
         let base = lower_expr(ctx, &index.base);
         let first = index.indices.inner.vals().next();
         let idx = lower_expr(ctx, first.unwrap());
         ExprKind::Index(Box::new(base), Box::new(idx))
      }

      ast::Expr::Cast(cast) => {
         let operand = lower_expr(ctx, &cast.operand);
         let ty = lower_type(ctx, &cast.ty);
         ExprKind::Cast(Box::new(operand), Box::new(ty))
      }

      ast::Expr::Ref(ref_expr) => {
         let mutable = ref_expr.mut_tok.is_some();
         let pointee = lower_expr(ctx, &ref_expr.pointee);
         ExprKind::RefOf(mutable, Box::new(pointee))
      }

      ast::Expr::Deref(deref) => {
         let pointee = lower_expr(ctx, &deref.pointee);
         ExprKind::Unary(
            UnOp {
               kind: UnOpKind::Deref,
               span,
            },
            Box::new(pointee),
         )
      }

      ast::Expr::Prefix(prefix) => {
         let operand = lower_expr(ctx, &prefix.operand);
         let op = lower_prefix_op(&prefix.op);
         ExprKind::Unary(op, Box::new(operand))
      }

      ast::Expr::Suffix(suffix) => {
         let operand = lower_expr(ctx, &suffix.operand);
         let op = lower_suffix_op(&suffix.op);
         ExprKind::Unary(op, Box::new(operand))
      }

      ast::Expr::Add(add) => {
         let lhs = lower_expr(ctx, &add.lhs);
         let rhs = lower_expr(ctx, &add.rhs);
         let op = lower_add_op(&add.op);
         ExprKind::Binary(op, Box::new(lhs), Box::new(rhs))
      }

      ast::Expr::Mul(mul) => {
         let lhs = lower_expr(ctx, &mul.lhs);
         let rhs = lower_expr(ctx, &mul.rhs);
         let op = lower_mul_op(&mul.op);
         ExprKind::Binary(op, Box::new(lhs), Box::new(rhs))
      }

      ast::Expr::Shift(shift) => {
         let lhs = lower_expr(ctx, &shift.lhs);
         let rhs = lower_expr(ctx, &shift.rhs);
         let op = lower_shift_op(&shift.op);
         ExprKind::Binary(op, Box::new(lhs), Box::new(rhs))
      }

      ast::Expr::Assign(assign) => {
         let lhs = lower_expr(ctx, &assign.lhs);
         let rhs = lower_expr(ctx, &assign.rhs);
         let op = lower_assign_op(&assign.op);
         ExprKind::Binary(op, Box::new(lhs), Box::new(rhs))
      }

      ast::Expr::If(if_expr) => {
         return lower_if_expr(ctx, if_expr);
      }

      ast::Expr::Block(block_expr) => {
         if let Some(label) = &block_expr.label {
            let mut guard = ctx.new_dest(
               Some(label.label.label.clone()),
               block_expr.block.span(),
               DestKind::Block,
            );
            let dest_id = Some(guard.dest_id());
            return lower_block(&mut guard, &block_expr.block, dest_id);
         } else {
            return lower_block(ctx, &block_expr.block, None);
         }
      }

      ast::Expr::Return(ret_expr) => {
         return lower_return_expr(ctx, ret_expr);
      }

      ast::Expr::Break(break_expr) => {
         return lower_break_expr(ctx, break_expr);
      }

      ast::Expr::Cont(cont_expr) => {
         return lower_cont_expr(ctx, cont_expr);
      }

      ast::Expr::While(while_expr) => {
         return lower_while_expr(ctx, while_expr);
      }

      ast::Expr::For(for_expr) => {
         return lower_for_expr(ctx, for_expr);
      }

      ast::Expr::Case(case_expr) => {
         return lower_case_expr(ctx, case_expr);
      }

      ast::Expr::Call(call_expr) => {
         let callee = lower_expr(ctx, &call_expr.callee);
         let args = call_expr
            .args
            .0
            .inner
            .vals()
            .map(|e| lower_expr(ctx, e))
            .collect();
         ExprKind::Call(Box::new(callee), args)
      }

      ast::Expr::Range(range_expr) => {
         let lhs = lower_expr(ctx, &range_expr.lhs);
         let rhs = lower_expr(ctx, &range_expr.rhs);
         let op = lower_range_op(&range_expr.op);
         ExprKind::Binary(op, Box::new(lhs), Box::new(rhs))
      }

      ast::Expr::Cmp(cmp_expr) => {
         return lower_cmp_expr(ctx, cmp_expr);
      }

      ast::Expr::And(and_expr) => {
         return lower_and_expr(ctx, and_expr);
      }

      ast::Expr::Fn(fn_expr) => {
         return lower_fn_expr(ctx, fn_expr);
      }

      ast::Expr::Error(_) => ExprKind::Error,
   };

   Expr { kind, span }
}

fn lower_field_value(ctx: &mut LoweringContext, field: &ast::FieldValue) -> ExprField {
   let expr = if let Some(init) = &field.init {
      lower_expr(ctx, &init.value)
   } else {
      lower_expr(ctx, &ast::Expr::Ident(ast::ExprIdent(field.name.clone())))
   };

   ExprField {
      ident: Ident {
         name: field.name.ident.clone(),
         span: field.name.span(),
      },
      expr: Box::new(expr),
      span: field.span(),
   }
}

fn lower_member(member: &ast::Member) -> Ident {
   let name = match member {
      ast::Member::Ident(ident) => ident.ident.clone(),
      ast::Member::Index(idx) => idx.int.to_string(),
   };

   Ident {
      name,
      span: member.span(),
   }
}

fn lower_prefix_op(op: &token::PrefixOp) -> hir::UnOp {
   use token::PrefixOp;
   let span = op.span();
   let kind = match op {
      PrefixOp::Not(_) => hir::UnOpKind::Not,
      PrefixOp::Neg(_) => hir::UnOpKind::Neg,
   };
   hir::UnOp { kind, span }
}

fn lower_suffix_op(op: &token::SuffixOp) -> hir::UnOp {
   use token::SuffixOp;
   let span = op.span();
   let kind = match op {
      SuffixOp::Inc(_) => UnOpKind::Inc,
      SuffixOp::Dec(_) => UnOpKind::Dec,
   };
   hir::UnOp { kind, span }
}

fn lower_add_op(op: &token::AddOp) -> hir::BinOp {
   use token::AddOp;
   let span = op.span();
   let kind = match op {
      AddOp::Add(_) => BinOpKind::Add,
      AddOp::Sub(_) => BinOpKind::Sub,
   };
   hir::BinOp { kind, span }
}

fn lower_mul_op(op: &token::MulOp) -> hir::BinOp {
   use token::MulOp;
   let span = op.span();
   let kind = match op {
      MulOp::Mul(_) => BinOpKind::Mul,
      MulOp::Div(_) => BinOpKind::Div,
      MulOp::Rem(_) => BinOpKind::Rem,
   };
   hir::BinOp { kind, span }
}

fn lower_shift_op(op: &token::ShiftOp) -> hir::BinOp {
   use token::ShiftOp;
   let span = op.span();
   let kind = match op {
      ShiftOp::Shl(_) => BinOpKind::Shl,
      ShiftOp::Shr(_) => BinOpKind::Shr,
   };
   hir::BinOp { kind, span }
}

fn lower_assign_op(op: &token::AssignOp) -> hir::BinOp {
   use token::AssignOp;
   let span = op.span();
   let kind = match op {
      AssignOp::Assign(_) => BinOpKind::Ass,
      AssignOp::Add(_) => BinOpKind::AddAss,
      AssignOp::Sub(_) => BinOpKind::SubAss,
      AssignOp::Mul(_) => BinOpKind::MulAss,
      AssignOp::Div(_) => BinOpKind::DivAss,
      AssignOp::Rem(_) => BinOpKind::RemAss,
      AssignOp::Shl(_) => BinOpKind::ShlAss,
      AssignOp::Shr(_) => BinOpKind::ShrAss,
   };
   hir::BinOp { kind, span }
}

fn lower_range_op(op: &token::RangeOp) -> hir::BinOp {
   use token::RangeOp;
   let span = op.span();
   let kind = match op {
      RangeOp::Lcro(_) => BinOpKind::Range,
      RangeOp::Lcrc(_) => BinOpKind::RangeInc,
   };
   hir::BinOp { kind, span }
}

fn lower_cmp_op(op: &token::CmpOp) -> hir::BinOp {
   use token::CmpOp;
   let span = op.span();
   let kind = match op {
      CmpOp::Lt(_) => BinOpKind::Lt,
      CmpOp::Le(_) => BinOpKind::Le,
      CmpOp::Gt(_) => BinOpKind::Gt,
      CmpOp::Ge(_) => BinOpKind::Ge,
      CmpOp::Eq(_) => BinOpKind::Eq,
      CmpOp::Ne(_) => BinOpKind::Ne,
      CmpOp::Way3(_) => BinOpKind::Way3,
   };
   hir::BinOp { kind, span }
}

fn lower_def_as_item(ctx: &mut LoweringContext, def: &ast::Def) -> Item {
   match def {
      ast::Def::Fn(def) => {
         let name = def.name.ident.clone();
         let sym_id = ctx.lookup_across(&name).unwrap().id;

         let mut ctx = ctx.new_fn();
         let sign = lower_fn_sign(&mut ctx, &def.function.sign);
         let body = lower_fn_body(&mut ctx, &def.function.body);

         return Item {
            kind: ItemKind::FnDef(FnDef {
               sym_id,
               name: Some(Ident {
                  name,
                  span: def.name.span(),
               }),
               side_effect: sign.0,
               params: sign.1,
               ret: sign.2,
               body,
            }),
            span: def.span(),
         };
      }

      ast::Def::Type(def) => {
         let name = def.name.ident.clone();
         let sym_id = ctx.lookup_across(&name).unwrap().id;

         return Item {
            kind: ItemKind::TyDef(TyDef {
               sym_id,
               name: Some(Ident {
                  name,
                  span: def.name.span(),
               }),
               ty: Box::new(lower_type(ctx, &def.ty)),
            }),
            span: def.span(),
         };
      }

      ast::Def::Local(local) => {
         ctx.diag(UnimplementedGlobal::new(local.span()));
      }

      ast::Def::Error(_) => (),
   }

   Item {
      kind: ItemKind::Error,
      span: def.span(),
   }
}

fn lower_fn_sign(ctx: &mut LoweringContext, sign: &ast::FnSign) -> (bool, Vec<Param>, Box<Ty>) {
   let side_effect = sign.mut_tok.is_some();

   let params = if let Some(params) = &sign.params {
      params
         .0
         .inner
         .vals()
         .map(|p| {
            let name = p.name.ident.clone();
            let span = p.name.span();

            let sym_id = ctx.define_local(Some(name.clone()), SymKind::Local, span);

            let ty = lower_type(ctx, &p.ty);

            Param {
               sym_id,
               mutable: p.mut_tok.is_some(),
               name: Ident { name, span },
               ty: Box::new(ty),
               span: p.span(),
            }
         })
         .collect()
   } else {
      vec![]
   };

   let ret = if let Some(r) = &sign.ret {
      lower_type(ctx, &r.ty)
   } else {
      Ty {
         kind: TyKind::Tuple(vec![]),
         span: sign.span(),
      }
   };

   (side_effect, params, Box::new(ret))
}

fn lower_fn_body(ctx: &mut LoweringContext, body: &ast::FnBody) -> Body {
   let kind = match body {
      ast::FnBody::Expr(expr) => {
         let body_expr = match expr.as_ref() {
            ast::Expr::Block(block) => {
               let label = block.label.as_ref().map(|l| l.label.label.clone());
               let mut guard = ctx.new_dest(label, block.span(), DestKind::Body);
               let dest_id = guard.dest_id();
               lower_block(&mut guard, &block.block, Some(dest_id))
            }

            _ => {
               let mut guard = ctx.new_dest(None, expr.span(), DestKind::Body);
               let dest_id = Some(guard.dest_id());
               let result_expr = lower_expr(&mut guard, expr);

               Expr {
                  kind: ExprKind::Block(Block {
                     dest_id,
                     stmts: vec![],
                     expr: Box::new(result_expr),
                     source: BlockSource::FnBody,
                  }),
                  span: expr.span(),
               }
            }
         };

         BodyKind::Expr(Box::new(body_expr))
      }

      ast::FnBody::Ffi(ffi) => {
         if ffi.abi.inner.abi.str.to_uppercase() != "C" {
            ctx.diag(UnsupportedFfi::new(ffi.abi.inner.abi.span));
            BodyKind::Error
         } else {
            let symbol = ffi.abi.inner.symbol.str.clone();
            BodyKind::CFfi(symbol)
         }
      }

      ast::FnBody::Asm(asm) => {
         ctx.diag(UnimplementedAsm::new(asm.span()));
         BodyKind::Error
      }
   };

   Body {
      kind,
      span: body.span(),
   }
}

fn lower_if_expr(ctx: &mut LoweringContext, if_expr: &ast::ExprIf) -> Expr {
   let span = if_expr.span();
   let cond = lower_expr(ctx, &if_expr.cond);
   let then_branch = lower_block(ctx, &if_expr.then_branch, None);

   let else_branch = if let Some(else_br) = &if_expr.else_branch {
      lower_block(ctx, &else_br.body, None)
   } else {
      Expr {
         kind: ExprKind::Tuple(vec![]),
         span,
      }
   };

   Expr {
      kind: ExprKind::If(Box::new(cond), Box::new(then_branch), Box::new(else_branch)),
      span,
   }
}

fn lower_block(ctx: &mut LoweringContext, block: &ast::Block, dest_id: Option<DestId>) -> Expr {
   let mut ctx = ctx.new_scope();

   let inner = &block.0.inner;

   for stmt in inner.vals() {
      if let ast::Stmt::Def(def) = stmt {
         match def {
            ast::Def::Fn(def) => {
               let name = def.name.ident.clone();
               ctx.define_local(Some(name), SymKind::Fn, def.span());
            }

            ast::Def::Type(def) => {
               let name = def.name.ident.clone();
               ctx.define_local(Some(name), SymKind::Type, def.span());
            }

            _ => {}
         }
      }
   }

   let mut stmts = Vec::new();
   for (stmt, _) in &inner.inner {
      if let Some(hir_stmt) = lower_stmt(&mut ctx, stmt) {
         stmts.push(hir_stmt);
      }
   }

   let expr = 'blk: {
      let span = if let Some(last) = &inner.last {
         match last.as_ref() {
            ast::Stmt::Expr(expr) => {
               break 'blk lower_expr(&mut ctx, expr);
            }
            _ => {
               if let Some(hir_stmt) = lower_stmt(&mut ctx, last) {
                  stmts.push(hir_stmt);
               }
               last.span()
            }
         }
      } else {
         block.span()
      };

      Expr {
         kind: ExprKind::Tuple(vec![]),
         span,
      }
   };

   Expr {
      kind: ExprKind::Block(Block {
         dest_id,
         stmts,
         expr: Box::new(expr),
         source: BlockSource::Block,
      }),
      span: block.span(),
   }
}

fn lower_stmt(ctx: &mut LoweringContext, stmt: &ast::Stmt) -> Option<Stmt> {
   let kind = match stmt {
      ast::Stmt::Def(def) => match def {
         ast::Def::Local(local) => {
            let name = local.name.ident.clone();

            let ty = match local.ty.as_ref() {
               Some(ty) => lower_type(ctx, ty),
               None => {
                  let start = local.name.span().end;
                  let end = local.init.as_ref().unwrap().eq_tok.span().start;
                  Ty {
                     kind: TyKind::Infer,
                     span: Span { start, end },
                  }
               }
            };

            let init = local.init.as_ref().map(|i| lower_expr(ctx, &i.value));

            let sym_id = ctx.define_local(Some(name.clone()), SymKind::Local, local.span());

            StmtKind::LocalDef(LocalDef {
               sym_id,
               mutable: local.mut_tok.is_some(),
               name: Some(Ident {
                  name,
                  span: local.name.span(),
               }),
               ty: Box::new(ty),
               init: init.map(Box::new),
               span: local.span(),
            })
         }

         ast::Def::Fn(def) => {
            let name = def.name.ident.clone();

            let sym_id = ctx.lookup_local(&name).unwrap().id;

            let mut ctx = ctx.new_fn();
            let sign = lower_fn_sign(&mut ctx, &def.function.sign);
            let body = lower_fn_body(&mut ctx, &def.function.body);

            StmtKind::Item(Item {
               kind: ItemKind::FnDef(FnDef {
                  sym_id,
                  name: Some(Ident {
                     name,
                     span: def.name.span(),
                  }),
                  side_effect: sign.0,
                  params: sign.1,
                  ret: sign.2,
                  body,
               }),
               span: def.span(),
            })
         }

         ast::Def::Type(def) => {
            let name_str = def.name.ident.clone();

            let sym_id = ctx.lookup_local(&name_str).unwrap().id;

            let ty_value = lower_type(ctx, &def.ty);

            StmtKind::Item(Item {
               kind: ItemKind::TyDef(TyDef {
                  sym_id,
                  name: Some(Ident {
                     name: name_str,
                     span: def.name.span(),
                  }),
                  ty: Box::new(ty_value),
               }),
               span: def.span(),
            })
         }

         ast::Def::Error(_) => StmtKind::Error,
      },

      ast::Stmt::Expr(expr) => StmtKind::Expr(Box::new(lower_expr(ctx, expr))),

      ast::Stmt::Error(_) => StmtKind::Error,
   };

   Some(Stmt {
      kind,
      span: stmt.span(),
   })
}

fn lower_return_expr(ctx: &mut LoweringContext, ret_expr: &ast::ExprReturn) -> Expr {
   let span = ret_expr.span();

   let dest_id = if let Some(dest) = ctx.find_fn_body_dest() {
      dest
   } else {
      ctx.diag(RetOutsideFn::new(ret_expr.span()));
      return Expr {
         kind: ExprKind::Error,
         span,
      };
   };

   let value = if let Some(val) = &ret_expr.value {
      lower_expr(ctx, val)
   } else {
      Expr {
         kind: ExprKind::Tuple(vec![]),
         span,
      }
   };

   Expr {
      kind: ExprKind::Break(dest_id, Box::new(value)),
      span,
   }
}

fn lower_break_expr(ctx: &mut LoweringContext, break_expr: &ast::ExprBreak) -> Expr {
   let span = break_expr.span();
   let error = Expr {
      kind: ExprKind::Error,
      span,
   };

   let dest_id = if let Some(label) = &break_expr.label {
      if let Some(dest) = ctx.lookup_label(&label.label) {
         dest
      } else {
         let label_name = format!("'{}", label.label);
         ctx.diag(NotFound::new(label_name, label.span()));
         return error;
      }
   } else if let Some(dest) = ctx.find_loop_dest() {
      dest
   } else {
      ctx.diag(BreakOutsideLoop::new(span));
      return error;
   };

   let value = if let Some(val) = &break_expr.value {
      lower_expr(ctx, val)
   } else {
      Expr {
         kind: ExprKind::Tuple(vec![]),
         span,
      }
   };

   Expr {
      kind: ExprKind::Break(dest_id, Box::new(value)),
      span,
   }
}

fn lower_cont_expr(ctx: &mut LoweringContext, cont_expr: &ast::ExprCont) -> Expr {
   let span = cont_expr.span();
   let error = Expr {
      kind: ExprKind::Error,
      span,
   };

   let dest_id = if let Some(label) = &cont_expr.label {
      if let Some(dest) = ctx.lookup_label(&label.label) {
         dest
      } else {
         let label_name = format!("'{}", label.label);
         ctx.diag(NotFound::new(label_name, label.span()));
         return error;
      }
   } else if let Some(dest) = ctx.find_loop_dest() {
      dest
   } else {
      ctx.diag(ContOutsideLoop::new(span));
      return error;
   };

   Expr {
      kind: ExprKind::Cont(dest_id),
      span,
   }
}

fn lower_while_expr(ctx: &mut LoweringContext, while_expr: &ast::ExprWhile) -> Expr {
   let span = while_expr.span();
   let label = while_expr.label.as_ref().map(|l| l.label.label.clone());
   let mut ctx = ctx.new_dest(label, span, DestKind::Loop);
   let dest_id = ctx.dest_id();

   let cond = lower_expr(&mut ctx, &while_expr.cond);

   let body = lower_block(&mut ctx, &while_expr.body, None);
   let body_span = while_expr.body.span();
   let body_sym_id = ctx.define_local(None, SymKind::Local, body_span);
   let body = LocalDef {
      sym_id: body_sym_id,
      mutable: false,
      name: None,
      ty: Box::new(Ty {
         kind: TyKind::Tuple(vec![]),
         span: body_span,
      }),
      init: Some(Box::new(body)),
      span: body_span,
   };

   let cont_expr = Expr {
      kind: ExprKind::Cont(dest_id),
      span,
   };

   let body = Expr {
      kind: ExprKind::Block(Block {
         dest_id: None,
         stmts: vec![Stmt {
            kind: StmtKind::LocalDef(body),
            span: body_span,
         }],
         expr: Box::new(cont_expr),
         source: BlockSource::While,
      }),
      span,
   };

   let else_branch = if let Some(else_branch) = &while_expr.exit {
      let block = lower_block(&mut ctx, &else_branch.body, None);
      match block.kind {
         ExprKind::Block(block) if block.stmts.is_empty() => block.expr,
         _ => Box::new(block),
      }
   } else {
      Box::new(Expr {
         kind: ExprKind::Tuple(vec![]),
         span,
      })
   };

   let dest_id = Some(dest_id);

   Expr {
      kind: ExprKind::Block(Block {
         dest_id,
         stmts: vec![],
         expr: Box::new(Expr {
            kind: ExprKind::If(Box::new(cond), Box::new(body), else_branch),
            span,
         }),
         source: BlockSource::While,
      }),
      span,
   }
}

fn lower_for_expr(ctx: &mut LoweringContext, for_expr: &ast::ExprFor) -> Expr {
   let span = for_expr.span();
   let label = for_expr.label.as_ref().map(|l| l.label.label.clone());

   let range_span = for_expr.range.span();
   let range_expr = lower_expr(ctx, &for_expr.range);
   let range_sym_id = ctx.define_local(None, SymKind::Local, range_span);
   let range_def = LocalDef {
      sym_id: range_sym_id,
      mutable: false,
      name: None,
      ty: Box::new(Ty {
         kind: TyKind::Infer,
         span: range_span,
      }),
      init: Some(Box::new(range_expr)),
      span: range_span,
   };
   let range_use = Expr {
      kind: ExprKind::Use(None, range_sym_id),
      span,
   };

   let begin_init = Expr {
      kind: ExprKind::Begin(Box::new(range_use.clone())),
      span,
   };
   let begin_sym_id = ctx.define_local(None, SymKind::Local, span);
   let begin_def = LocalDef {
      sym_id: begin_sym_id,
      mutable: false,
      name: None,
      ty: Box::new(Ty {
         kind: TyKind::Infer,
         span,
      }),
      init: Some(Box::new(begin_init)),
      span,
   };
   let begin_use = Expr {
      kind: ExprKind::Use(None, begin_sym_id),
      span,
   };

   let end_init = Expr {
      kind: ExprKind::End(Box::new(range_use.clone())),
      span,
   };
   let end_sym_id = ctx.define_local(None, SymKind::Local, span);
   let end_def = LocalDef {
      sym_id: end_sym_id,
      mutable: false,
      name: None,
      ty: Box::new(Ty {
         kind: TyKind::Infer,
         span,
      }),
      init: Some(Box::new(end_init)),
      span,
   };
   let end_use = Expr {
      kind: ExprKind::Use(None, end_sym_id),
      span,
   };

   let mut ctx = ctx.new_dest(label, span, DestKind::Loop);
   let dest_id = ctx.dest_id();

   let cond = Expr {
      kind: ExprKind::Binary(
         BinOp {
            kind: BinOpKind::Lt,
            span,
         },
         Box::new(begin_use.clone()),
         Box::new(end_use.clone()),
      ),
      span,
   };

   let elem_name = for_expr.name.ident.clone();
   let elem_span = for_expr.name.span();
   let elem_init = Expr {
      kind: ExprKind::Unary(
         UnOp {
            kind: UnOpKind::Deref,
            span: elem_span,
         },
         Box::new(begin_use.clone()),
      ),
      span: elem_span,
   };
   let elem_sym_id = ctx.define_local(Some(elem_name.clone()), SymKind::Local, elem_span);
   let elem_def = LocalDef {
      sym_id: elem_sym_id,
      mutable: for_expr.mutable.is_some(),
      name: Some(Ident {
         name: elem_name,
         span: elem_span,
      }),
      ty: Box::new(Ty {
         kind: TyKind::Infer,
         span: elem_span,
      }),
      init: Some(Box::new(elem_init)),
      span: elem_span,
   };

   let user_body = lower_block(&mut ctx, &for_expr.body, None);
   let body_span = for_expr.body.span();
   let body_sym_id = ctx.define_local(None, SymKind::Local, body_span);
   let body_def = LocalDef {
      sym_id: body_sym_id,
      mutable: false,
      name: None,
      ty: Box::new(Ty {
         kind: TyKind::Tuple(vec![]),
         span: body_span,
      }),
      init: Some(Box::new(user_body)),
      span: body_span,
   };

   let increment = Expr {
      kind: ExprKind::Unary(
         UnOp {
            kind: UnOpKind::Inc,
            span,
         },
         Box::new(begin_use.clone()),
      ),
      span,
   };

   let cont_expr = Expr {
      kind: ExprKind::Cont(dest_id),
      span,
   };

   let loop_body = Expr {
      kind: ExprKind::Block(Block {
         dest_id: None,
         stmts: vec![
            Stmt {
               kind: StmtKind::LocalDef(elem_def),
               span: elem_span,
            },
            Stmt {
               kind: StmtKind::LocalDef(body_def),
               span: body_span,
            },
            Stmt {
               kind: StmtKind::Expr(Box::new(increment)),
               span,
            },
         ],
         expr: Box::new(cont_expr),
         source: BlockSource::For,
      }),
      span,
   };

   let else_branch = if let Some(else_branch) = &for_expr.exit {
      let block = lower_block(&mut ctx, &else_branch.body, None);
      match block.kind {
         ExprKind::Block(block) if block.stmts.is_empty() => block.expr,
         _ => Box::new(block),
      }
   } else {
      Box::new(Expr {
         kind: ExprKind::Tuple(vec![]),
         span,
      })
   };

   let loop_expr = Expr {
      kind: ExprKind::Block(Block {
         dest_id: Some(dest_id),
         stmts: vec![],
         expr: Box::new(Expr {
            kind: ExprKind::If(Box::new(cond), Box::new(loop_body), else_branch),
            span,
         }),
         source: BlockSource::For,
      }),
      span,
   };

   Expr {
      kind: ExprKind::Block(Block {
         dest_id: None,
         stmts: vec![
            Stmt {
               kind: StmtKind::LocalDef(range_def),
               span: range_span,
            },
            Stmt {
               kind: StmtKind::LocalDef(begin_def),
               span,
            },
            Stmt {
               kind: StmtKind::LocalDef(end_def),
               span,
            },
         ],
         expr: Box::new(loop_expr),
         source: BlockSource::For,
      }),
      span,
   }
}

fn lower_case_expr(ctx: &mut LoweringContext, case_expr: &ast::ExprCase) -> Expr {
   let span = case_expr.span();

   let mut ctx = ctx.new_dest(None, span, DestKind::Block);
   let dest_id = ctx.dest_id();

   let arms = &case_expr.arms.inner;
   let arm_list: Vec<_> = arms.vals().collect();

   let mut stmts = Vec::new();

   for arm in arm_list {
      // Ignore arm.label for now because it's hard to desugar.
      // I am considering making case a primitive operation instead of if.

      let cond = lower_expr(&mut ctx, &arm.cond.0);
      let value = lower_expr(&mut ctx, &arm.value);
      let break_stmt = Expr {
         kind: ExprKind::Break(dest_id, Box::new(value)),
         span: arm.value.span(),
      };

      let arm_span = arm.span();

      let if_expr = Expr {
         kind: ExprKind::If(
            Box::new(cond),
            Box::new(break_stmt),
            Box::new(Expr {
               kind: ExprKind::Tuple(vec![]),
               span,
            }),
         ),
         span: arm_span,
      };

      stmts.push(Stmt {
         kind: StmtKind::Expr(Box::new(if_expr)),
         span: arm_span,
      });
   }

   let else_expr = if let Some(else_arm) = &case_expr.else_arm {
      lower_expr(&mut ctx, &else_arm.value)
   } else {
      Expr {
         kind: ExprKind::Tuple(vec![]),
         span,
      }
   };

   Expr {
      kind: ExprKind::Block(Block {
         dest_id: Some(dest_id),
         stmts,
         expr: Box::new(else_expr),
         source: BlockSource::Case,
      }),
      span,
   }
}

fn lower_cmp_expr(ctx: &mut LoweringContext, cmp_expr: &ast::ExprCmp) -> Expr {
   let span = cmp_expr.span();

   let exprs: Vec<_> = cmp_expr.0.vals().collect();
   let ops: Vec<_> = cmp_expr.0.seps().collect();

   assert!(!ops.is_empty());
   assert_eq!(exprs.len() - 1, ops.len());

   if exprs.len() == 2 {
      let lhs_expr = lower_expr(ctx, exprs[0]);
      let lhs_span = lhs_expr.span;
      let lhs = Expr {
         kind: ExprKind::RefOf(false, Box::new(lhs_expr)),
         span: lhs_span,
      };

      let rhs_expr = lower_expr(ctx, exprs[1]);
      let rhs_span = rhs_expr.span;
      let rhs = Expr {
         kind: ExprKind::RefOf(false, Box::new(rhs_expr)),
         span: rhs_span,
      };

      let op = lower_cmp_op(ops[0]);

      return Expr {
         kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
         span,
      };
   }

   let mut ctx = ctx.new_dest(None, span, DestKind::Block);
   let dest_id = ctx.dest_id();

   let first_node = exprs.first().unwrap();
   let first_expr = lower_expr(&mut ctx, first_node);
   let first_span = first_node.span();
   let first_sym_id = ctx.define_local(None, SymKind::Local, first_span);

   let first_init = Expr {
      kind: ExprKind::RefOf(false, Box::new(first_expr)),
      span: first_span,
   };

   let first_def = LocalDef {
      sym_id: first_sym_id,
      mutable: false,
      name: None,
      ty: Box::new(Ty {
         kind: TyKind::Infer,
         span: first_span,
      }),
      init: Some(Box::new(first_init)),
      span: first_span,
   };

   let mut stmts = vec![Stmt {
      kind: StmtKind::LocalDef(first_def),
      span: first_span,
   }];

   let mut lhs_span = first_span;
   let mut lhs_use = Expr {
      kind: ExprKind::Use(None, first_sym_id),
      span: lhs_span,
   };

   for i in 0..ops.len() {
      let rhs_node = exprs[i + 1];
      let rhs_expr = lower_expr(&mut ctx, rhs_node);
      let rhs_span = rhs_node.span();
      let rhs_sym_id = ctx.define_local(None, SymKind::Local, rhs_span);
      let rhs_init = Expr {
         kind: ExprKind::RefOf(false, Box::new(rhs_expr)),
         span: rhs_span,
      };
      let rhs_def = LocalDef {
         sym_id: rhs_sym_id,
         mutable: false,
         name: None,
         ty: Box::new(Ty {
            kind: TyKind::Infer,
            span: rhs_span,
         }),
         init: Some(Box::new(rhs_init)),
         span: rhs_span,
      };
      let rhs_use = Expr {
         kind: ExprKind::Use(None, rhs_sym_id),
         span: rhs_span,
      };

      let op = lower_cmp_op(ops[i]);
      let cmp_span = lhs_span.merge(rhs_span);

      let cmp = Expr {
         kind: ExprKind::Binary(op, Box::new(lhs_use), Box::new(rhs_use.clone())),
         span: cmp_span,
      };

      let negated = Expr {
         kind: ExprKind::Unary(
            UnOp {
               kind: UnOpKind::Not,
               span: cmp_span,
            },
            Box::new(cmp),
         ),
         span: cmp_span,
      };

      let false_const = Expr {
         kind: ExprKind::Const(Constant {
            kind: ConstantKind::Bool(false),
            span,
         }),
         span: cmp_span,
      };

      let break_false = Expr {
         kind: ExprKind::Break(dest_id, Box::new(false_const)),
         span: cmp_span,
      };

      let if_stmt = Expr {
         kind: ExprKind::If(
            Box::new(negated),
            Box::new(break_false),
            Box::new(Expr {
               kind: ExprKind::Tuple(vec![]),
               span: cmp_span,
            }),
         ),
         span: cmp_span,
      };

      stmts.push(Stmt {
         kind: StmtKind::LocalDef(rhs_def),
         span: rhs_span,
      });
      stmts.push(Stmt {
         kind: StmtKind::Expr(Box::new(if_stmt)),
         span: cmp_span,
      });

      lhs_span = rhs_span;
      lhs_use = rhs_use;
   }

   let true_const = Expr {
      kind: ExprKind::Const(Constant {
         kind: ConstantKind::Bool(true),
         span,
      }),
      span,
   };

   Expr {
      kind: ExprKind::Block(Block {
         dest_id: Some(dest_id),
         stmts,
         expr: Box::new(true_const),
         source: BlockSource::Block,
      }),
      span,
   }
}

fn lower_and_expr(ctx: &mut LoweringContext, and_expr: &ast::ExprLogical) -> Expr {
   let span = and_expr.span();

   let exprs: Vec<_> = and_expr.0.vals().collect();

   assert!(exprs.len() > 1);
   let short_circuit = matches!(and_expr.0.seps().next().unwrap(), token::LogicalOp::Or(_));

   let mut ctx = ctx.new_dest(None, span, DestKind::Block);
   let dest_id = ctx.dest_id();
   let mut stmts = Vec::new();

   for expr in &exprs {
      let expr_span = expr.span();
      let mut cond = lower_expr(&mut ctx, expr);

      if !short_circuit {
         cond = Expr {
            kind: ExprKind::Unary(
               UnOp {
                  kind: UnOpKind::Not,
                  span: expr_span,
               },
               Box::new(cond),
            ),
            span: expr_span,
         };
      }

      let value = Expr {
         kind: ExprKind::Const(Constant {
            kind: ConstantKind::Bool(short_circuit),
            span: expr_span,
         }),
         span: expr_span,
      };

      let break_expr = Expr {
         kind: ExprKind::Break(dest_id, Box::new(value)),
         span: expr_span,
      };

      let if_stmt = Expr {
         kind: ExprKind::If(
            Box::new(cond),
            Box::new(break_expr),
            Box::new(Expr {
               kind: ExprKind::Tuple(vec![]),
               span: expr_span,
            }),
         ),
         span: expr_span,
      };

      stmts.push(Stmt {
         kind: StmtKind::Expr(Box::new(if_stmt)),
         span: expr_span,
      });
   }

   let value = Expr {
      kind: ExprKind::Const(Constant {
         kind: ConstantKind::Bool(!short_circuit),
         span,
      }),
      span,
   };

   Expr {
      kind: ExprKind::Block(Block {
         dest_id: Some(dest_id),
         stmts,
         expr: Box::new(value),
         source: BlockSource::Block,
      }),
      span,
   }
}

fn lower_fn_expr(ctx: &mut LoweringContext, fn_expr: &ast::ExprFn) -> Expr {
   let span = fn_expr.span();

   let sym_id = ctx.define_local(None, SymKind::Fn, span);

   let mut ctx = ctx.new_fn();
   let sign = lower_fn_sign(&mut ctx, &fn_expr.sign);
   let body = lower_fn_body(&mut ctx, &fn_expr.body);

   let fn_item = Item {
      kind: ItemKind::FnDef(FnDef {
         sym_id,
         name: None,
         side_effect: sign.0,
         params: sign.1,
         ret: sign.2,
         body,
      }),
      span,
   };

   let use_expr = Expr {
      kind: ExprKind::Use(None, sym_id),
      span,
   };

   Expr {
      kind: ExprKind::Block(Block {
         dest_id: None,
         stmts: vec![Stmt {
            kind: StmtKind::Item(fn_item),
            span,
         }],
         expr: Box::new(use_expr),
         source: BlockSource::Block,
      }),
      span,
   }
}
