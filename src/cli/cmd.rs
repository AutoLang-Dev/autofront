use std::{error::Error, fmt::Display};

use crate::wtr;

#[derive(Debug, Clone)]
pub struct DebugSubcommand {
   pub file: String,
   pub output: Option<String>,
   pub show_recovery: bool,
}

#[derive(Debug, Clone)]
pub enum Command {
   None,
   Help(Option<String>),
   Version,
   Lex(DebugSubcommand),
   Tt(DebugSubcommand),
   Parse(DebugSubcommand),
}

#[derive(Debug, Clone)]
pub enum CliError {
   UnknownCommand(String),
   MissingArgument(String),
   UnknownOption(String),
}

impl Display for CliError {
   fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      use CliError::*;
      match self {
         UnknownCommand(cmd) => wtr!(f, cli_unknown_command, cmd),
         MissingArgument(arg) => wtr!(f, cli_missing_argument, arg),
         UnknownOption(opt) => wtr!(f, cli_unknown_option, opt),
      }
   }
}

impl Error for CliError {}

pub type Result<T> = std::result::Result<T, CliError>;
