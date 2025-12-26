use crate::{
   cli::cmd::{CliError, Command, Result},
   tr,
};

struct FromTo {
   file: String,
   output: Option<String>,
}

impl FromTo {
   fn parse(args: &[String]) -> Result<FromTo> {
      let file;
      let output;

      let first = args
         .get(1)
         .ok_or(CliError::MissingArgument(tr!(cli_file)))?;

      if first.starts_with("-") {
         if first != "-o" {
            return Err(CliError::UnknownOption(first.into()));
         }

         let second = args
            .get(2)
            .ok_or(CliError::MissingArgument(tr!(cli_file)))?;

         output = Some(second.into());

         let third = args
            .get(3)
            .ok_or(CliError::MissingArgument(tr!(cli_file)))?;

         file = third.into()
      } else {
         file = first.into();

         match args.get(2) {
            Some(second) => {
               if second != "-o" {
                  return Err(CliError::UnknownOption(second.into()));
               }

               let third = args
                  .get(3)
                  .ok_or(CliError::MissingArgument(tr!(cli_file)))?;

               output = Some(third.into())
            }
            None => output = None,
         }
      }

      Ok(FromTo { file, output })
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
                  "help" | "version" | "lex" | "tt" => Some(topic),
                  _ => Err(CliError::UnknownCommand(topic))?,
               }
            }
            None => None,
         };
         Ok(Command::Help(topic))
      }

      "version" => Ok(Command::Version),

      "lex" => {
         let FromTo { file, output } = FromTo::parse(args)?;
         Ok(Command::Lex { file, output })
      }

      "tt" => {
         let FromTo { file, output } = FromTo::parse(args)?;
         Ok(Command::Tt { file, output })
      }

      other => Err(CliError::UnknownCommand(other.into())),
   }
}
