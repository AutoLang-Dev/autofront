use std::env::{VarError, var};

use fluent_i18n::set_locale;

#[macro_export]
macro_rules! tr {
   ($key:ident $(,)?) => {{
      $crate::__t!(stringify!($key))
   }};

   ($key:ident, $($rest:tt)+) => {{
      $crate::tr!(@munch stringify!($key), [] ; $($rest)+ ,)
   }};

   (@munch $key:expr, [ $($items:tt)* ] ; ) => {{
      $crate::__t!($key, { $($items)* })
   }};

   (@munch $key:expr, [ $($items:tt)* ] ; ,) => {{
      $crate::__t!($key, { $($items)* })
   }};

   (@munch $key:expr, [ $($items:tt)* ] ; $name:ident = $value:expr , $($rest:tt)*) => {
      {
         $crate::tr!(@munch
         $key,
         [ $($items)* stringify!($name) => $value, ]
         ; $($rest)*
      )
   }
   };

   (@munch $key:expr, [ $($items:tt)* ] ; $name:ident , $($rest:tt)*) => {
      $crate::tr!(@munch
         $key,
         [ $($items)* stringify!($name) => $name, ]
         ; $($rest)*
      )
   };
}

#[macro_export]
macro_rules! eptr {
   ($key:ident $(, $($rest:tt)+)?) => {
      $crate::__anstream::eprint!("{}", $crate::tr!($key $(, $($rest)+)?))
   };
}

#[macro_export]
macro_rules! eplntr {
   ($key:ident $(, $($rest:tt)+)?) => {
      $crate::__anstream::eprintln!("{}", $crate::tr!($key $(, $($rest)+)?))
   };
}

#[macro_export]
macro_rules! ptr {
   ($key:ident $(, $($rest:tt)+)?) => {
      $crate::__anstream::print!("{}", $crate::tr!($key $(, $($rest)+)?))
   };
}

#[macro_export]
macro_rules! plntr {
   ($key:ident $(, $($rest:tt)+)?) => {
      $crate::__anstream::println!("{}", $crate::tr!($key $(, $($rest)+)?))
   };
}

#[macro_export]
macro_rules! wtr {
   ($f:expr, $key:ident $(, $($rest:tt)+)?) => {
      write!($f, "{}", $crate::tr!($key $(, $($rest)+)?))
   };
}

#[macro_export]
macro_rules! wlntr {
   ($f:expr, $key:ident $(, $($rest:tt)+)?) => {
      writeln!($f, "{}", $crate::tr!($key $(, $($rest)+)?))
   };
}

#[macro_export]
macro_rules! tre {
   ($key:ident $(, $($rest:tt)+)?) => {{
      use std::fmt::Write;

      let s = $crate::tr!($key $(, $($rest)+)?);

      let mut buf = std::string::String::new();

      for c in s.chars() {
         match c {
            '\n' => write!(buf, "\\n"),
            '\r' => write!(buf, "\\r"),
            _ => write!(buf, "{c}"),
         }
         .unwrap();
      }

      buf
   }};
}

pub fn init_locale() -> bool {
   let locale = {
      match var("AUTOFRONT_LANG") {
         Ok(locale) => Some(locale),
         Err(VarError::NotPresent) => None,
         Err(err) => {
            anstream::eprintln!("VarError::NotUnicode: {err}");
            return false;
         }
      }
   };

   if let Err(err) = set_locale(locale.as_deref()) {
      set_locale(None).unwrap();
      eplntr!(bad_locale, err = err.to_string());
      return false;
   };

   true
}
