use std::borrow::Borrow;
use std::collections::HashMap;
use std::hash::Hash;

use common::span::Span;
use hir::{DestId, SymId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SymKind {
   Type,
   Fn,
   Local,
}

#[derive(Debug, Clone)]
pub struct Symbol {
   pub id: SymId,
   pub name: Option<String>,
   pub kind: SymKind,
   pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DestKind {
   Block,
   Loop,
   Body,
}

#[derive(Debug, Clone)]
pub struct Destination {
   pub id: DestId,
   pub label: Option<String>,
   pub span: Span,
   pub kind: DestKind,
}

#[derive(Debug, Clone, Default)]
struct SymbolScope(HashMap<String, Symbol>);

#[derive(Debug, Clone, Default)]
struct LocalScope {
   symbols: SymbolScope,
   labels: HashMap<String, DestId>,
}

#[derive(Debug, Clone, Default)]
pub struct SymbolTable {
   global: SymbolScope,
   local: Vec<Vec<LocalScope>>,
}

impl SymbolTable {
   pub fn new() -> Self {
      Self {
         global: SymbolScope::default(),
         local: vec![],
      }
   }

   pub fn is_global(&self) -> bool {
      self.local.is_empty()
   }

   pub fn enter_scope(&mut self) {
      if let Some(scopes) = self.local.last_mut() {
         scopes.push(LocalScope::default());
      }
   }

   pub fn exit_scope(&mut self) {
      if let Some(scopes) = self.local.last_mut()
         && scopes.len() > 1
      {
         scopes.pop();
      }
   }

   pub fn enter_fn(&mut self) {
      self.local.push(vec![LocalScope::default()]);
   }

   pub fn exit_fn(&mut self) {
      self.local.pop();
   }

   pub fn contains_in_scope<Q>(&self, key: &Q) -> bool
   where
      String: Borrow<Q>,
      Q: Hash + Eq + ?Sized,
   {
      if let Some(scopes) = self.local.last() {
         scopes.last().unwrap().symbols.0.contains_key(key)
      } else {
         self.global.0.contains_key(key)
      }
   }

   pub fn define_label(&mut self, label: String, id: DestId) {
      let f = self.local.last_mut().unwrap();
      let scope = f.last_mut().unwrap();
      scope.labels.insert(label, id);
   }

   pub fn get_label<Q>(&self, label: &Q) -> Option<DestId>
   where
      String: Borrow<Q>,
      Q: Hash + Eq + ?Sized,
   {
      let Some(scopes) = self.local.last() else {
         unreachable!("get_label called outside of any function")
      };
      for scope in scopes.iter().rev() {
         if let Some(id) = scope.labels.get(label) {
            return Some(*id);
         }
      }
      None
   }

   pub fn define_local(&mut self, key: String, value: Symbol) {
      if let Some(scopes) = self.local.last_mut() {
         scopes.last_mut().unwrap().symbols.0.insert(key, value);
      } else {
         self.global.0.insert(key, value);
      }
   }

   pub fn define_global(&mut self, key: String, value: Symbol) {
      self.global.0.insert(key, value);
   }

   pub fn get_global<Q>(&self, key: &Q) -> Option<&Symbol>
   where
      String: Borrow<Q>,
      Q: Hash + Eq + ?Sized,
   {
      self.global.0.get(key)
   }

   pub fn get_local<Q>(&self, key: &Q) -> Option<&Symbol>
   where
      String: Borrow<Q>,
      Q: Hash + Eq + ?Sized,
   {
      if let Some(scopes) = self.local.last() {
         for scope in scopes.iter().rev() {
            if let Some(value) = scope.symbols.0.get(key) {
               return Some(value);
            }
         }
      }
      self.global.0.get(key)
   }

   pub fn get_across<Q>(&self, key: &Q) -> Option<&Symbol>
   where
      String: Borrow<Q>,
      Q: Hash + Eq + ?Sized,
   {
      for function_scopes in self.local.iter().rev() {
         for scope in function_scopes.iter().rev() {
            if let Some(value) = scope.symbols.0.get(key) {
               return Some(value);
            }
         }
      }
      self.global.0.get(key)
   }
}
