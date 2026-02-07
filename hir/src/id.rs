use std::ops::{Add, AddAssign};

macro_rules! define_id {
   ($id:ident) => {
      #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
      pub struct $id(usize);

      impl $id {
         pub fn new(value: usize) -> Self {
            Self(value)
         }

         pub fn as_usize(self) -> usize {
            self.0
         }
      }

      impl Add<usize> for $id {
         type Output = Self;
         fn add(self, rhs: usize) -> Self::Output {
            Self(self.0 + rhs)
         }
      }

      impl AddAssign<usize> for $id {
         fn add_assign(&mut self, rhs: usize) {
            self.0 += rhs
         }
      }
   };
}

define_id!(SymId);

define_id!(DestId);
