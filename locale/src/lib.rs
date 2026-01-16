mod i18n;

pub use i18n::*;

pub use anstream as __anstream;
pub use fluent_i18n::{i18n, t as __t};

i18n!("locale", fallback = "en-US");
