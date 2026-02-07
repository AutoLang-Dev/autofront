use crate::*;
use common::span::Span;
use diag::{DiagSink, Diagnostics};
use hir::{DestId, SymId};

pub struct LoweringContext<'a> {
   diag: &'a mut DiagSink,
   scope: SymbolTable,
   next_sym_id: SymId,
   next_dest_id: DestId,
   dest_stack: Vec<Vec<Destination>>,
}

impl<'a> LoweringContext<'a> {
   pub fn new(diag: &'a mut DiagSink) -> Self {
      let mut ctx = Self {
         diag,
         scope: SymbolTable::new(),
         next_sym_id: SymId::new(0),
         next_dest_id: DestId::new(0),
         dest_stack: vec![Vec::new()],
      };

      ctx.register_prelude();

      ctx
   }

   pub fn diag(&mut self, diag: impl Diagnostics + 'static) {
      self.diag.diag(diag);
   }

   fn register_prelude(&mut self) {
      let dummy_span = Span { start: 0, end: 0 };

      let builtin_types = [
         "Never", "Bool", "Char", "Byte", "Int8", "Int16", "Int32", "Int64", "Int128", "UInt8",
         "UInt16", "UInt32", "UInt64", "UInt128", "ISize", "USize",
      ];

      for ty_name in builtin_types {
         self.define_global(ty_name.to_string(), SymKind::Type, dummy_span);
      }
   }

   pub fn fresh_sym_id(&mut self) -> SymId {
      let id = self.next_sym_id;
      self.next_sym_id += 1;
      id
   }

   pub fn fresh_dest_id(&mut self) -> DestId {
      let id = self.next_dest_id;
      self.next_dest_id += 1;
      id
   }

   pub fn define_global(&mut self, name: String, kind: SymKind, span: Span) -> SymId {
      let id = self.fresh_sym_id();
      let symbol = Symbol {
         id,
         name: Some(name.clone()),
         kind,
         span,
      };
      self.scope.define_global(name, symbol);
      id
   }

   pub fn define_local(&mut self, name: Option<String>, kind: SymKind, span: Span) -> SymId {
      let id = self.fresh_sym_id();
      let symbol = Symbol {
         id,
         name: name.clone(),
         kind,
         span,
      };

      if let Some(name) = name {
         match kind {
            SymKind::Local => {
               self.scope.define_local(name, symbol);
            }
            SymKind::Fn | SymKind::Type => {
               if self.scope.contains_in_scope(&name) {
                  let prev = self.scope.get_local(&name).unwrap();
                  self.diag.diag(crate::errors::DuplicateDefinition::new(
                     name.clone(),
                     span,
                     prev.span,
                  ));
               } else {
                  self.scope.define_local(name, symbol);
               }
            }
         }
      }

      id
   }

   pub fn lookup_local(&self, name: &str) -> Option<&Symbol> {
      self.scope.get_local(name)
   }

   pub fn lookup_across(&self, name: &str) -> Option<&Symbol> {
      self.scope.get_across(name)
   }

   pub fn lookup_global(&self, name: &str) -> Option<&Symbol> {
      self.scope.get_global(name)
   }

   pub fn current_dest(&self) -> Option<DestId> {
      self.dest_stack.last()?.last().map(|d| d.id)
   }

   pub fn lookup_label(&self, label: &str) -> Option<DestId> {
      self.scope.get_label(label)
   }

   pub fn find_loop_dest(&self) -> Option<DestId> {
      self
         .dest_stack
         .last()?
         .iter()
         .rev()
         .find(|d| d.kind == DestKind::Loop)
         .map(|d| d.id)
   }

   pub fn find_fn_body_dest(&self) -> Option<DestId> {
      self
         .dest_stack
         .last()?
         .iter()
         .rev()
         .find(|d| d.kind == DestKind::Body)
         .map(|d| d.id)
   }

   pub fn new_scope<'b>(&'b mut self) -> ScopeGuard<'b, 'a> {
      self.scope.enter_scope();
      ScopeGuard { ctx: self }
   }

   pub fn new_fn<'b>(&'b mut self) -> FnGuard<'b, 'a> {
      self.scope.enter_fn();
      self.dest_stack.push(Vec::new());
      FnGuard { ctx: self }
   }

   pub fn new_dest<'b>(
      &'b mut self,
      label: Option<String>,
      span: Span,
      kind: DestKind,
   ) -> DestGuard<'b, 'a> {
      let dest_id = self.fresh_dest_id();
      let dest = Destination {
         id: dest_id,
         label: label.clone(),
         span,
         kind,
      };

      if let Some(label) = label {
         self.scope.define_label(label, dest_id);
      }

      if let Some(current_fn) = self.dest_stack.last_mut() {
         current_fn.push(dest);
      }

      DestGuard { ctx: self, dest_id }
   }
}

pub struct ScopeGuard<'b, 'a> {
   ctx: &'b mut LoweringContext<'a>,
}

impl<'b, 'a> Drop for ScopeGuard<'b, 'a> {
   fn drop(&mut self) {
      self.ctx.scope.exit_scope();
   }
}

impl<'b, 'a> std::ops::Deref for ScopeGuard<'b, 'a> {
   type Target = LoweringContext<'a>;

   fn deref(&self) -> &Self::Target {
      self.ctx
   }
}

impl<'b, 'a> std::ops::DerefMut for ScopeGuard<'b, 'a> {
   fn deref_mut(&mut self) -> &mut Self::Target {
      self.ctx
   }
}

pub struct FnGuard<'b, 'a> {
   ctx: &'b mut LoweringContext<'a>,
}

impl<'b, 'a> Drop for FnGuard<'b, 'a> {
   fn drop(&mut self) {
      self.ctx.scope.exit_fn();
      if self.ctx.dest_stack.len() > 1 {
         self.ctx.dest_stack.pop();
      }
   }
}

impl<'b, 'a> std::ops::Deref for FnGuard<'b, 'a> {
   type Target = LoweringContext<'a>;

   fn deref(&self) -> &Self::Target {
      self.ctx
   }
}

impl<'b, 'a> std::ops::DerefMut for FnGuard<'b, 'a> {
   fn deref_mut(&mut self) -> &mut Self::Target {
      self.ctx
   }
}

pub struct DestGuard<'b, 'a> {
   ctx: &'b mut LoweringContext<'a>,
   dest_id: DestId,
}

impl<'b, 'a> DestGuard<'b, 'a> {
   pub fn dest_id(&self) -> DestId {
      self.dest_id
   }
}

impl<'b, 'a> Drop for DestGuard<'b, 'a> {
   fn drop(&mut self) {
      if let Some(current_fn) = self.ctx.dest_stack.last_mut() {
         current_fn.pop();
      }
   }
}

impl<'b, 'a> std::ops::Deref for DestGuard<'b, 'a> {
   type Target = LoweringContext<'a>;

   fn deref(&self) -> &Self::Target {
      self.ctx
   }
}

impl<'b, 'a> std::ops::DerefMut for DestGuard<'b, 'a> {
   fn deref_mut(&mut self) -> &mut Self::Target {
      self.ctx
   }
}
