cli_welcome = Welcome to AutoLang and its compiler, AutoFront!
    AutoFront is currently unavailable; more features are coming soon.
    Tip: Use `autofront help` for assistance.

cli_checking = Checking { $file }
cli_compiling = Compiling { $file }

cli_unknown_command = Unknown command: { $cmd }
cli_missing_argument = Missing argument: { $arg }
cli_unknown_option = Unknown option: { $opt }

cli_help = Usage: autofront <COMMAND> [OPTIONS]
    Commands:
      help [<COMMAND>]  Display help messgae
      version           Print version info
      lex <FILE>        Lexical analysis (unstable)
      tt <FILE>         Build TokenTree (unstable)
      parse <FILE>      Parse file (unstable)
    
    Options:
      -o <FILE>         Write output to FILE
      --show-recovery   Print the results even if it fails

cli_help_help = help [<COMMAND>]: Display help message
cli_help_version = version: Print version info
cli_help_lex = lex <FILE>: Perform lexical analysis and print the results
cli_help_tt = tt <FILE>: Build a TokenTree
cli_help_parse = parse <FILE>: Parse file and print AST

cli_file = file
cli_option = option
