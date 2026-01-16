todo = todo

diag_print_fail = failed to print diagnostic: {$msg}
diag_io_err = IO error: {$msg}

here = here

error = error
warning = warning
info = info
note = note
help = help

summary_fail = compilation failed: { $errors ->
        [one]   1 error
       *[other] {$errors} errors
    } and { $warnings ->
        [0]     no warnings
        [1]     1 warning
       *[other] {$warnings} warnings
    }
summary_success =
    { $warnings -> 
        [0]     compilation successful
        [1]     compilation successful with 1 warning
       *[other] compilation successful with {$warnings} warnings
    }
