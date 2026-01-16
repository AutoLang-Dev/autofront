mod sink;

pub use sink::*;

use annotate_snippets::Snippet;
use common::source::Source;

locale::i18n!("locale", fallback = "en-US");

pub trait SourceSnippet {
   fn snippet<T: Clone>(&self) -> Snippet<'_, T>;
}

impl SourceSnippet for Source {
   fn snippet<T: Clone>(&self) -> Snippet<'_, T> {
      Snippet::source(&self.code).path(&self.file)
   }
}
