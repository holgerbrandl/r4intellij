if (is.character(help.statet)
                && length(help.statet) == 1 && !is.na(help.statet)) {
        .showHelp(help.statet)
}
if (length(x) == 0
              || is.null(.rj.tmp$help) ) {
        # NextMethod ?
        return (utils:::print.help_files_with_topic(x, ...))
}