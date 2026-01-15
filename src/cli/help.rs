use crate::{cli::cmd::CliError, tr};

pub fn print_help(topic: Option<&str>) -> Result<(), CliError> {
   match topic {
      None => print_general_help(),
      Some("help") => print_help_help(),
      Some("version") => print_version_help(),
      Some("lex") => print_lex_help(),
      Some("tt") => print_tt_help(),
      Some("parse") => print_parse_help(),
      Some(cmd) => {
         return Err(CliError::UnknownCommand(cmd.to_string()));
      }
   }
   Ok(())
}

fn print_general_help() {
   anstream::println!("{}", tr!(cli_help));
}

fn print_help_help() {
   anstream::println!("{}", tr!(cli_help_help));
}

fn print_version_help() {
   anstream::println!("{}", tr!(cli_help_version));
}

fn print_lex_help() {
   anstream::println!("{}", tr!(cli_help_lex));
}

fn print_tt_help() {
   anstream::println!("{}", tr!(cli_help_tt));
}

fn print_parse_help() {
   anstream::println!("{}", tr!(cli_help_parse));
}
