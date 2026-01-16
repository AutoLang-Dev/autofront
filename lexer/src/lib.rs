mod lexer;
mod tokentree;

pub use lexer::lex;
pub use tokentree::parse_token_tree;

locale::i18n!("locale", fallback = "en-US");
