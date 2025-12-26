bad_locale = 错误的语言代码：{ $err }

todo = 等待实现

diag_print_fail = 诊断打印失败：{$msg}
diag_io_err = IO 错误：{$msg}

here = 这里

error = 错误
warning = 警告
info = 信息
note = 注释
help = 帮助

summary_fail =
    编译失败：{$errors} 个错误，{ $warnings ->
        [0]     没有警告
       *[other] {$warnings} 个警告
    }
summary_success =
    { $warnings -> 
        [0]     编译成功
       *[other] 编译成功，但有 {$warnings} 个警告
    }
