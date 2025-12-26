mod cli;
mod driver;
mod pipelines;
mod utils;

use std::process::ExitCode;

use {driver::Driver, utils::init_locale};

fluent_i18n::i18n!("locale", fallback = "en-US");

fn main() -> ExitCode {
   match init_locale() {
      false => ExitCode::FAILURE,
      true => Driver::new().run(),
   }
}
