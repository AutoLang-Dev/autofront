mod cli;
mod driver;
mod pipelines;
mod utils;

use std::process::ExitCode;

use driver::Driver;

locale::i18n!("locale", fallback = "en-US");

fn main() -> ExitCode {
   match locale::init_locale() {
      false => ExitCode::FAILURE,
      true => Driver::new().run(),
   }
}
