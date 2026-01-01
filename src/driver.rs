use anstream::StripStream;
use std::{
   env::args,
   error::Error,
   fs::{self, File},
   io::Write,
   path::Path,
   process::ExitCode,
};
use unicode_width::UnicodeWidthStr;

use crate::{
   cli::{Command, DebugSubcommand, parse_args, print_help},
   pipelines::{
      lexer::{Source, lex},
      tokentree::parse_token_tree,
   },
   tr,
   utils::{DiagPrinter, DiagSink},
};

macro_rules! get_out {
   ($output:expr) => {{
      match $output {
         None => &mut anstream::stdout() as &mut dyn Write,
         Some(file) => {
            let file = File::create(file)?;
            &mut StripStream::new(file)
         }
      }
   }};
}

pub struct Driver {}

impl Default for Driver {
   fn default() -> Self {
      Self::new()
   }
}

impl Driver {
   pub fn new() -> Self {
      Self {}
   }

   fn load(&self, file: String) -> Result<Source, Box<dyn Error>> {
      let path = Path::new(&file);
      let code = fs::read_to_string(path)?;
      let file = path.file_name().unwrap().to_str().unwrap().to_string();
      Ok(Source { file, code })
   }

   fn lex(&mut self, args: DebugSubcommand) -> Result<(), Box<dyn Error>> {
      let DebugSubcommand {
         file,
         output,
         show_recovery,
      } = args;

      let mut sink = DiagSink::default();
      let src = self.load(file)?;

      let tokens = lex(&src, &mut sink);

      let printer = DiagPrinter::new(&src);
      printer.print(sink, show_recovery)?;

      let mut wid = 0;
      for token in &tokens {
         wid = wid.max(format!("{}", token.span).width())
      }

      let out = get_out!(output);

      for token in &tokens {
         let span = format!("{}", token.span);
         writeln!(out, "{span:wid$} | {token:#}",)?;
      }

      Ok(())
   }

   fn tt(&mut self, args: DebugSubcommand) -> Result<(), Box<dyn Error>> {
      let DebugSubcommand {
         file,
         output,
         show_recovery,
      } = args;

      let mut sink = DiagSink::default();
      let src = self.load(file)?;

      let tokens = lex(&src, &mut sink);
      let tt = parse_token_tree(&tokens, &mut sink);

      let printer = DiagPrinter::new(&src);
      printer.print(sink, show_recovery)?;

      let out = get_out!(output);
      writeln!(out, "{tt}")?;

      Ok(())
   }

   fn drive(mut self) -> Result<(), Box<dyn Error>> {
      use Command::*;

      let args: Vec<String> = args().collect();
      let cmd = parse_args(&args[1..])?;

      match cmd {
         None => anstream::println!("{}", tr!(cli_welcome)),
         Help(cmd) => print_help(cmd.as_deref())?,
         Version => anstream::println!("{}", env!("CARGO_PKG_VERSION")),
         Lex(args) => self.lex(args)?,
         Tt(args) => self.tt(args)?,
      }

      Ok(())
   }

   pub fn run(self) -> ExitCode {
      match self.drive() {
         Ok(()) => ExitCode::SUCCESS,
         Err(err) => {
            anstream::eprintln!("{err}");
            ExitCode::FAILURE
         }
      }
   }
}
