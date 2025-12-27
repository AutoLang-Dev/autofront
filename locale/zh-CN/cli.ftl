cli_welcome = 欢迎使用 AutoLang 及其编译器 AutoFront！
    AutoFront 目前还处于不可用状态，更多功能敬请期待
    提示：使用 `autofront help` 查看帮助

cli_checking = 正在检查 { $file }
cli_compiling = 正在编译 { $file }

cli_unknown_command = 未知命令：{ $cmd }
cli_missing_argument = 需要参数：{ $arg }
cli_unknown_option = 未知选项：{ $opt }

cli_help = 用法：autofront <命令> [选项]
    命令：
      help [<命令>]   显示帮助信息
      version         显示版本
      lex <文件>      词法分析（不稳定）
      tt <文件>       构建 TokenTree（不稳定）

    选项：
      -o <文件>       写入输出到文件
      --show-recovery 失败时也打印结果

cli_help_help = help [<命令>]：显示帮助信息
cli_help_version = version：显示版本信息
cli_help_lex = lex <文件>：对文件进行词法分析并打印结果
cli_help_tt = tt <文件>：构建 ToenTree

cli_file = 文件
cli_option = 选项
