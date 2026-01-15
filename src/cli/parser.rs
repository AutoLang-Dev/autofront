use crate::cli::{
   DebugSubcommand,
   cmd::{CliError, Command, Result},
};
use locale::tr;

impl DebugSubcommand {
   fn parse(args: &[String]) -> Result<DebugSubcommand> {
      let mut file = None;
      let mut output = None;
      let mut show_recovery = false;

      for i in 1.. {
         let Some(arg) = args.get(i).cloned() else {
            break;
         };

         match &arg as &str {
            "--show-recovery" => show_recovery = true,
            "-o" => continue,
            _ => {
               if arg.starts_with("-") {
                  return Err(CliError::UnknownOption(arg));
               }

               if i > 1 || args.get(i - 1).is_some_and(|p| p == "-o") {
                  output = Some(arg)
               } else {
                  file = Some(arg)
               }
            }
         }
      }

      let Some(file) = file else {
         return Err(CliError::MissingArgument(tr!(cli_file)));
      };

      Ok(DebugSubcommand {
         file,
         output,
         show_recovery,
      })
   }
}

pub fn parse_args(args: &[String]) -> Result<Command> {
   if args.is_empty() {
      return Ok(Command::None);
   }

   let cmd = args[0].as_str();

   match cmd {
      "help" => {
         let topic = match args.get(1) {
            Some(topic) => {
               let topic = topic.clone();
               match topic.as_str() {
                  "help" | "version" | "lex" | "tt" | "parse" => Some(topic),
                  _ => Err(CliError::UnknownCommand(topic))?,
               }
            }
            None => None,
         };
         Ok(Command::Help(topic))
      }

      "version" => Ok(Command::Version),

      "lex" => Ok(Command::Lex(DebugSubcommand::parse(args)?)),
      "tt" => Ok(Command::Tt(DebugSubcommand::parse(args)?)),
      "parse" => Ok(Command::Parse(DebugSubcommand::parse(args)?)),

      other => Err(CliError::UnknownCommand(other.into())),
   }
}
