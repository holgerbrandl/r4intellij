##
## Exported symobls in package `readr`
##

## Exported package methods

read_delim_chunked <- function (file, callback, chunk_size = 10000, delim, quote = "\"", 
    escape_backslash = FALSE, escape_double = TRUE, col_names = TRUE, 
    col_types = NULL, locale = default_locale(), na = c("", "NA"), 
    quoted_na = TRUE, comment = "", trim_ws = FALSE, skip = 0, 
    guess_max = min(1000, chunk_size), progress = show_progress()) 
{
    if (!nzchar(delim)) {
        stop("`delim` must be at least one character, ", "use `read_table()` for whitespace delimited input.", 
            call. = FALSE)
    }
    tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, 
        escape_double = escape_double, na = na, quoted_na = quoted_na, 
        comment = comment, trim_ws = trim_ws)
    read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, 
        tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, guess_max = guess_max, 
        progress = progress)
}


parse_logical <- function (x, na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, col_logical(), na = na, locale = locale)
}


col_date <- function (format = "") 
{
    collector("date", format = format)
}


fwf_widths <- function (widths, col_names = NULL) 
{
    pos <- cumsum(c(1L, abs(widths)))
    fwf_positions(pos[-length(pos)], pos[-1] - 1L, col_names)
}


readr_example <- function (path) 
{
    system.file("extdata", path, package = "readr", mustWork = TRUE)
}


parse_datetime <- function (x, format = "", na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, col_datetime(format), na = na, locale = locale)
}


write_csv <- function (x, path, na = "NA", append = FALSE, col_names = !append) 
{
    write_delim(x, path, delim = ",", na = na, append = append, 
        col_names = col_names)
}


write_rds <- function (x, path, compress = c("none", "gz", "bz2", "xz"), ...) 
{
    compress <- match.arg(compress)
    con <- switch(compress, none = file(path, ...), gz = gzfile(path, 
        ...), bz2 = bzfile(path, ...), xz = xzfile(path, ...))
    on.exit(close(con), add = TRUE)
    saveRDS(x, con)
    invisible(x)
}


problems <- function (x) 
{
    probs <- probs(x)
    if (is.null(probs)) {
        structure(data.frame(row = integer(), col = integer(), 
            expected = character(), actual = character(), stringsAsFactors = FALSE), 
            class = c("tbl_df", "data.frame"))
    }
    else {
        probs
    }
}


count_fields <- function (file, tokenizer, skip = 0, n_max = -1L) 
{
    ds <- datasource(file, skip = skip)
    count_fields_(ds, tokenizer, n_max)
}


SideEffectChunkCallback <- "<environment>"

write_file <- function (x, path, append = FALSE) 
{
    path <- standardise_path(path, input = FALSE)
    if (!isOpen(path)) {
        on.exit(close(path), add = TRUE)
        if (isTRUE(append)) {
            open(path, "ab")
        }
        else {
            open(path, "wb")
        }
    }
    if (is.raw(x)) {
        write_file_raw_(x, path)
    }
    else {
        write_file_(x, path)
    }
    invisible(x)
}


format_delim <- function (x, delim, na = "NA", append = FALSE, col_names = !append) 
{
    stopifnot(is.data.frame(x))
    x <- lapply(x, output_column)
    stream_delim(x, NULL, delim, col_names = col_names, append = append, 
        na = na)
}


tokenizer_delim <- function (delim, quote = "\"", na = "NA", quoted_na = TRUE, comment = "", 
    trim_ws = TRUE, escape_double = TRUE, escape_backslash = FALSE) 
{
    structure(list(delim = delim, quote = quote, na = na, quoted_na = quoted_na, 
        comment = comment, trim_ws = trim_ws, escape_double = escape_double, 
        escape_backslash = escape_backslash), class = "tokenizer_delim")
}


col_double <- function () 
{
    collector("double")
}


fwf_empty <- function (file, skip = 0, col_names = NULL, comment = "", n = 100L) 
{
    ds <- datasource(file, skip = skip)
    out <- whitespaceColumns(ds, comment = comment, n = n)
    out$end[length(out$end)] <- NA
    col_names <- fwf_col_names(col_names, length(out$begin))
    out$col_names <- col_names
    out
}


parse_factor <- function (x, levels, ordered = FALSE, na = c("", "NA"), locale = default_locale(), 
    include_na = TRUE) 
{
    parse_vector(x, col_factor(levels, ordered, include_na), 
        na = na, locale = locale)
}


DataFrameCallback <- "<environment>"

type_convert <- function (df, col_types = NULL, na = c("", "NA"), trim_ws = TRUE, 
    locale = default_locale()) 
{
    stopifnot(is.data.frame(df))
    is_character <- vapply(df, is.character, logical(1))
    char_cols <- df[is_character]
    guesses <- lapply(char_cols, function(x) {
        x[x %in% na] <- NA
        guess_parser(x, locale)
    })
    if (is.character(col_types)) {
        stop("`col_types` must be `NULL` or a `cols` specification for `type_convert()`.", 
            call. = FALSE)
    }
    specs <- col_spec_standardise(col_types = col_types, col_names = names(char_cols), 
        guessed_types = guesses)
    if (is.null(col_types)) {
        show_cols_spec(specs)
    }
    df[is_character] <- lapply(seq_along(char_cols), function(i) {
        type_convert_col(char_cols[[i]], specs$cols[[i]], which(is_character)[i], 
            locale_ = locale, na = na, trim_ws = trim_ws)
    })
    df
}


format_csv <- function (x, na = "NA", append = FALSE, col_names = !append) 
{
    format_delim(x, delim = ",", na = na, append = append, col_names = col_names)
}


parse_character <- function (x, na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, col_character(), na = na, locale = locale)
}


parse_vector <- function (x, collector, na = c("", "NA"), locale = default_locale()) 
{
    if (is.character(collector)) {
        collector <- collector_find(collector)
    }
    warn_problems(parse_vector_(x, collector, na = na, locale_ = locale))
}


tokenizer_csv <- function (na = "NA", quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE) 
{
    tokenizer_delim(delim = ",", na = na, quoted_na = quoted_na, 
        quote = quote, comment = comment, trim_ws = trim_ws, 
        escape_double = TRUE, escape_backslash = FALSE)
}


output_column <- function (x) 
{
    UseMethod("output_column")
}


spec_csv <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress()) 
attr((function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress()) 
{
    tokenizer <- tokenizer_csv(na = na, quoted_na = TRUE, quote = quote, 
        comment = comment, trim_ws = trim_ws)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
})(file = file, col_names = col_names, col_types = col_types, 
    locale = locale, na = na, quoted_na = quoted_na, quote = quote, 
    comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, 
    guess_max = guess_max, progress = progress), "spec")


tokenizer_ws <- function (na = "NA", comment = "") 
{
    structure(list(na = na, comment = comment), class = "tokenizer_ws")
}


cols <- function (..., .default = col_guess()) 
{
    col_types <- list(...)
    is_character <- vapply(col_types, is.character, logical(1))
    col_types[is_character] <- lapply(col_types[is_character], 
        col_concise)
    if (is.character(.default)) {
        .default <- col_concise(.default)
    }
    col_spec(col_types, .default)
}


parse_guess <- function (x, na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, guess_parser(x, locale), na = na, locale = locale)
}


col_logical <- function () 
{
    collector("logical")
}


tokenizer_line <- function (na = character()) 
{
    structure(list(na = na), class = "tokenizer_line")
}


cols_condense <- function (x) 
{
    types <- vapply(x$cols, function(xx) class(xx)[[1]], character(1))
    counts <- table(types)
    most_common <- names(counts)[counts == max(counts)][[1]]
    x$default <- x$cols[types == most_common][[1]]
    x$cols <- x$cols[types != most_common]
    x
}


col_factor <- function (levels, ordered = FALSE, include_na = FALSE) 
{
    collector("factor", levels = levels, ordered = ordered, include_na = include_na)
}


read_rds <- function (path) 
{
    readRDS(path)
}


fwf_cols <- function (...) 
{
    x <- lapply(list(...), as.integer)
    names(x) <- fwf_col_names(names(x), length(x))
    x <- tibble::as_tibble(x)
    if (nrow(x) == 2) {
        fwf_positions(as.integer(x[1, ]), as.integer(x[2, ]), 
            names(x))
    }
    else if (nrow(x) == 1) {
        fwf_widths(as.integer(x[1, ]), names(x))
    }
    else {
        stop("All variables must have either one (width) two (start, end) values.", 
            call. = FALSE)
    }
}


fwf_positions <- function (start, end = NULL, col_names = NULL) 
{
    stopifnot(length(start) == length(end))
    col_names <- fwf_col_names(col_names, length(start))
    tibble(begin = start - 1L, end = end, col_names = col_names)
}


read_tsv_chunked <- function (file, callback, chunk_size = 10000, col_names = TRUE, 
    col_types = NULL, locale = default_locale(), na = c("", "NA"), 
    quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, 
    skip = 0, guess_max = min(1000, chunk_size), progress = show_progress()) 
{
    tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, 
        quote = quote, comment = comment, trim_ws = trim_ws)
    read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, 
        tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, guess_max = guess_max, 
        progress = progress)
}


read_csv <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, 
        n_max), progress = show_progress()) 
{
    tokenizer <- tokenizer_csv(na = na, quoted_na = TRUE, quote = quote, 
        comment = comment, trim_ws = trim_ws)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
}


read_table2 <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 
        1000), progress = show_progress(), comment = "") 
{
    tokenizer <- tokenizer_ws(na = na, comment = comment)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
}


parse_integer <- function (x, na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, col_integer(), na = na, locale = locale)
}


read_lines_raw <- function (file, skip = 0, n_max = -1L, progress = show_progress()) 
{
    if (empty_file(file)) {
        return(list())
    }
    ds <- datasource(file, skip = skip)
    read_lines_raw_(ds, n_max = n_max, progress = progress)
}


ListCallback <- "<environment>"

read_file <- function (file, locale = default_locale()) 
{
    if (empty_file(file)) {
        return("")
    }
    ds <- datasource(file)
    read_file_(ds, locale)
}


col_character <- function () 
{
    collector("character")
}


write_lines <- function (x, path, na = "NA", append = FALSE) 
{
    is_raw <- is.list(x) && inherits(x[[1]], "raw")
    if (!is_raw) {
        x <- as.character(x)
    }
    path <- standardise_path(path, input = FALSE)
    if (!isOpen(path)) {
        on.exit(close(path), add = TRUE)
        open(path, if (isTRUE(append)) 
            "ab"
        else "wb")
    }
    if (is_raw) {
        write_lines_raw_(x, path)
    }
    else {
        write_lines_(x, path, na)
    }
    invisible(x)
}


col_skip <- function () 
{
    collector("skip")
}


parse_double <- function (x, na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, col_double(), na = na, locale = locale)
}


ChunkCallback <- "<environment>"

read_file_raw <- function (file) 
{
    if (empty_file(file)) {
        return(raw())
    }
    ds <- datasource(file)
    read_file_raw_(ds)
}


date_names <- function (mon, mon_ab = mon, day, day_ab = day, am_pm = c("AM", 
    "PM")) 
{
    stopifnot(is.character(mon), length(mon) == 12)
    stopifnot(is.character(mon_ab), length(mon_ab) == 12)
    stopifnot(is.character(day), length(day) == 7)
    stopifnot(is.character(day_ab), length(day_ab) == 7)
    structure(list(mon = enc2utf8(mon), mon_ab = enc2utf8(mon_ab), 
        day = enc2utf8(day), day_ab = enc2utf8(day_ab), am_pm = enc2utf8(am_pm)), 
        class = "date_names")
}


write_delim <- function (x, path, delim = " ", na = "NA", append = FALSE, col_names = !append) 
{
    stopifnot(is.data.frame(x))
    x_out <- lapply(x, output_column)
    stream_delim(x_out, path, delim, col_names = col_names, append = append, 
        na = na)
    invisible(x)
}


stop_for_problems <- function (x) 
{
    n <- n_problems(x)
    if (n == 0) 
        return(invisible(x))
    stop(n, " parsing failure", if (n > 1) 
        "s", call. = FALSE)
}


guess_encoding <- function (file, n_max = 10000, threshold = 0.2) 
{
    if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("stringi package required for encoding operations", 
            call. = FALSE)
    }
    if (is.character(file)) {
        lines <- unlist(read_lines_raw(file, n_max = n_max))
    }
    else if (is.raw(file)) {
        lines <- file
    }
    else if (is.list(file)) {
        lines <- unlist(file)
    }
    else {
        stop("Unknown input to `file`", call. = FALSE)
    }
    if (stringi::stri_enc_isascii(lines)) {
        return(tibble::tibble(encoding = "ASCII", confidence = 1))
    }
    guess <- stringi::stri_enc_detect(lines)
    df <- tibble::as_tibble(guess[[1]])
    names(df) <- tolower(names(df))
    df[df$confidence > threshold, c("encoding", "confidence")]
}


parse_time <- function (x, format = "", na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, col_time(format), na = na, locale = locale)
}


parse_date <- function (x, format = "", na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, col_date(format), na = na, locale = locale)
}


write_tsv <- function (x, path, na = "NA", append = FALSE, col_names = !append) 
{
    write_delim(x, path, delim = "\t", na = na, append = append, 
        col_names = col_names)
}


col_integer <- function () 
{
    collector("integer")
}


tokenizer_fwf <- function (begin, end, na = "NA", comment = "") 
{
    structure(list(begin = begin, end = end, na = na, comment = comment), 
        class = "tokenizer_fwf")
}


col_number <- function () 
{
    collector("number")
}


read_csv2 <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, 
        n_max), progress = show_progress()) 
{
    if (locale$decimal_mark == ".") {
        message("Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.")
        locale$decimal_mark <- ","
        locale$grouping_mark <- "."
    }
    tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, 
        quote = quote, comment = comment, trim_ws = trim_ws)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
}


datasource <- function (file, skip = 0, comment = "") 
{
    if (inherits(file, "source")) {
        if (!missing(skip)) {
            file$skip <- skip
        }
        if (!missing(comment)) {
            file$comment <- comment
        }
        file
    }
    else if (is.connection(file)) {
        datasource_connection(file, skip, comment)
    }
    else if (is.raw(file)) {
        datasource_raw(file, skip, comment)
    }
    else if (is.character(file)) {
        if (grepl("\n", file)) {
            datasource_string(file, skip, comment)
        }
        else {
            file <- standardise_path(file)
            if (is.connection(file)) {
                datasource_connection(file, skip, comment)
            }
            else {
                datasource_file(file, skip, comment)
            }
        }
    }
    else {
        stop("`file` must be a string, raw vector or a connection.", 
            call. = FALSE)
    }
}


read_delim <- function (file, delim, quote = "\"", escape_backslash = FALSE, 
    escape_double = TRUE, col_names = TRUE, col_types = NULL, 
    locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, 
    comment = "", trim_ws = FALSE, skip = 0, n_max = Inf, guess_max = min(1000, 
        n_max), progress = show_progress()) 
{
    if (!nzchar(delim)) {
        stop("`delim` must be at least one character, ", "use `read_table()` for whitespace delimited input.", 
            call. = FALSE)
    }
    tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, 
        escape_double = escape_double, na = na, quoted_na = quoted_na, 
        comment = comment, trim_ws = trim_ws)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
}


spec_delim <- function (file, delim, quote = "\"", escape_backslash = FALSE, 
    escape_double = TRUE, col_names = TRUE, col_types = NULL, 
    locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, 
    comment = "", trim_ws = FALSE, skip = 0, n_max = 0, guess_max = 1000, 
    progress = show_progress()) 
attr((function (file, delim, quote = "\"", escape_backslash = FALSE, 
    escape_double = TRUE, col_names = TRUE, col_types = NULL, 
    locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, 
    comment = "", trim_ws = FALSE, skip = 0, n_max = 0, guess_max = 1000, 
    progress = show_progress()) 
{
    if (!nzchar(delim)) {
        stop("`delim` must be at least one character, ", "use `read_table()` for whitespace delimited input.", 
            call. = FALSE)
    }
    tokenizer <- tokenizer_delim(delim, quote = quote, escape_backslash = escape_backslash, 
        escape_double = escape_double, na = na, quoted_na = quoted_na, 
        comment = comment, trim_ws = trim_ws)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
})(file = file, delim = delim, quote = quote, escape_backslash = escape_backslash, 
    escape_double = escape_double, col_names = col_names, col_types = col_types, 
    locale = locale, na = na, quoted_na = quoted_na, comment = comment, 
    trim_ws = trim_ws, skip = skip, n_max = n_max, guess_max = guess_max, 
    progress = progress), "spec")


locale <- function (date_names = "en", date_format = "%AD", time_format = "%AT", 
    decimal_mark = ".", grouping_mark = ",", tz = "UTC", encoding = "UTF-8", 
    asciify = FALSE) 
{
    if (is.character(date_names)) {
        date_names <- date_names_lang(date_names)
    }
    stopifnot(is.date_names(date_names))
    if (asciify) {
        date_names[] <- lapply(date_names, stringi::stri_trans_general, 
            id = "latin-ascii")
    }
    if (missing(grouping_mark) && !missing(decimal_mark)) {
        grouping_mark <- if (decimal_mark == ".") 
            ","
        else "."
    }
    else if (missing(decimal_mark) && !missing(grouping_mark)) {
        decimal_mark <- if (grouping_mark == ".") 
            ","
        else "."
    }
    stopifnot(decimal_mark %in% c(".", ","))
    stopifnot(is.character(grouping_mark), length(grouping_mark) == 
        1)
    if (decimal_mark == grouping_mark) {
        stop("`decimal_mark` and `grouping_mark` must be different", 
            call. = FALSE)
    }
    check_tz(tz)
    check_encoding(encoding)
    structure(list(date_names = date_names, date_format = date_format, 
        time_format = time_format, decimal_mark = decimal_mark, 
        grouping_mark = grouping_mark, tz = tz, encoding = encoding), 
        class = "locale")
}


write_excel_csv <- function (x, path, na = "NA", append = FALSE, col_names = !append) 
{
    stopifnot(is.data.frame(x))
    x_out <- lapply(x, output_column)
    stream_delim(x_out, path, ",", col_names = col_names, append = append, 
        na = na, bom = TRUE)
    invisible(x)
}


cols_only <- function (...) 
{
    cols(..., .default = col_skip())
}


format_tsv <- function (x, na = "NA", append = FALSE, col_names = !append) 
{
    format_delim(x, delim = "\t", na = na, append = append, col_names = col_names)
}


tokenizer_tsv <- function (na = "NA", quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE) 
{
    tokenizer_delim(delim = "\t", na = na, quoted_na = quoted_na, 
        quote = quote, comment = comment, trim_ws = trim_ws, 
        escape_double = TRUE, escape_backslash = FALSE)
}


spec_tsv <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress()) 
attr((function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress()) 
{
    tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, 
        quote = quote, comment = comment, trim_ws = trim_ws)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
})(file = file, col_names = col_names, col_types = col_types, 
    locale = locale, na = na, quoted_na = quoted_na, quote = quote, 
    comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, 
    guess_max = guess_max, progress = progress), "spec")


tokenizer_log <- function () 
{
    structure(list(), class = "tokenizer_log")
}


date_names_langs <- function () 
{
    names(date_symbols)
}


read_fwf <- function (file, col_positions, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), comment = "", skip = 0, n_max = Inf, guess_max = min(n_max, 
        1000), progress = show_progress()) 
{
    ds <- datasource(file, skip = skip)
    if (inherits(ds, "source_file") && empty_file(file)) {
        return(tibble::tibble())
    }
    tokenizer <- tokenizer_fwf(col_positions$begin, col_positions$end, 
        na = na, comment = comment)
    spec <- col_spec_standardise(file, skip = skip, guess_max = guess_max, 
        tokenizer = tokenizer, locale = locale, col_names = col_positions$col_names, 
        col_types = col_types, drop_skipped_names = TRUE)
    if (is.null(col_types) && !inherits(ds, "source_string")) {
        show_cols_spec(spec)
    }
    out <- read_tokens(ds, tokenizer, spec$cols, names(spec$cols), 
        locale_ = locale, n_max = if (n_max == Inf) 
            -1
        else n_max, progress = progress)
    out <- name_problems(out, names(spec$cols), source_name(file))
    attr(out, "spec") <- spec
    warn_problems(out)
}


read_csv_chunked <- function (file, callback, chunk_size = 10000, col_names = TRUE, 
    col_types = NULL, locale = default_locale(), na = c("", "NA"), 
    quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, 
    skip = 0, guess_max = min(1000, chunk_size), progress = show_progress()) 
{
    tokenizer <- tokenizer_csv(na = na, quoted_na = TRUE, quote = quote, 
        comment = comment, trim_ws = trim_ws)
    read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, 
        tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, guess_max = guess_max, 
        progress = progress)
}


read_table <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = "NA", skip = 0, n_max = Inf, guess_max = min(n_max, 
        1000), progress = show_progress(), comment = "") 
{
    ds <- datasource(file, skip = skip)
    columns <- fwf_empty(ds, skip = skip, n = guess_max, comment = comment)
    skip <- skip + columns$skip
    tokenizer <- tokenizer_fwf(columns$begin, columns$end, na = na, 
        comment = comment)
    spec <- col_spec_standardise(file = ds, skip = skip, guess_max = guess_max, 
        col_names = col_names, col_types = col_types, locale = locale, 
        tokenizer = tokenizer)
    ds <- datasource(file = ds, skip = skip + isTRUE(col_names))
    if (is.null(col_types) && !inherits(ds, "source_string")) {
        show_cols_spec(spec)
    }
    res <- read_tokens(ds, tokenizer, spec$cols, names(spec$cols), 
        locale_ = locale, n_max = n_max, progress = progress)
    attr(res, "spec") <- spec
    res
}


col_time <- function (format = "") 
{
    collector("time", format = format)
}


col_datetime <- function (format = "") 
{
    collector("datetime", format = format)
}


parse_number <- function (x, na = c("", "NA"), locale = default_locale()) 
{
    parse_vector(x, col_number(), na = na, locale = locale)
}


read_tsv <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = Inf, guess_max = min(1000, 
        n_max), progress = show_progress()) 
{
    tokenizer <- tokenizer_tsv(na = na, quoted_na = quoted_na, 
        quote = quote, comment = comment, trim_ws = trim_ws)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
}


guess_parser <- function (x, locale = default_locale()) 
{
    stopifnot(is.locale(locale))
    collectorGuess(x, locale)
}


read_lines <- function (file, skip = 0, n_max = -1L, locale = default_locale(), 
    na = character(), progress = show_progress()) 
{
    if (empty_file(file)) {
        return(character())
    }
    ds <- datasource(file, skip = skip)
    read_lines_(ds, locale_ = locale, na = na, n_max = n_max, 
        progress = progress)
}


date_names_lang <- function (language) 
{
    stopifnot(is.character(language), length(language) == 1)
    symbols <- date_symbols[[language]]
    if (is.null(symbols)) {
        stop("Unknown language '", language, "'", call. = FALSE)
    }
    symbols
}


read_log <- function (file, col_names = FALSE, col_types = NULL, skip = 0, 
    n_max = Inf, progress = show_progress()) 
{
    tokenizer <- tokenizer_log()
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        skip = skip, n_max = n_max, progress = progress)
}


read_lines_chunked <- function (file, callback, chunk_size = 10000, skip = 0, locale = default_locale(), 
    na = character(), progress = show_progress()) 
{
    if (empty_file(file)) {
        return(character())
    }
    ds <- datasource(file, skip = skip)
    callback <- as_chunk_callback(callback)
    on.exit(callback$finally(), add = TRUE)
    read_lines_chunked_(ds, locale, na, chunk_size, callback, 
        progress)
    return(callback$result())
}


default_locale <- function () 
{
    loc <- getOption("readr.default_locale")
    if (is.null(loc)) {
        loc <- locale()
        options(readr.default_locale = loc)
    }
    loc
}


spec <- function (x) 
{
    stopifnot(inherits(x, "tbl_df"))
    attr(x, "spec")
}


col_guess <- function () 
{
    collector("guess")
}


read_csv2_chunked <- function (file, callback, chunk_size = 10000, col_names = TRUE, 
    col_types = NULL, locale = default_locale(), na = c("", "NA"), 
    quoted_na = TRUE, quote = "\"", comment = "", trim_ws = TRUE, 
    skip = 0, guess_max = min(1000, chunk_size), progress = show_progress()) 
{
    if (locale$decimal_mark == ".") {
        message("Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.")
        locale$decimal_mark <- ","
        locale$grouping_mark <- "."
    }
    tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, 
        quote = quote, comment = comment, trim_ws = trim_ws)
    read_delimited_chunked(file, callback = callback, chunk_size = chunk_size, 
        tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, guess_max = guess_max, 
        progress = progress)
}


spec_table <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = "NA", skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), 
    comment = "") 
attr((function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = "NA", skip = 0, n_max = 0, guess_max = 1000, progress = show_progress(), 
    comment = "") 
{
    ds <- datasource(file, skip = skip)
    columns <- fwf_empty(ds, skip = skip, n = guess_max, comment = comment)
    skip <- skip + columns$skip
    tokenizer <- tokenizer_fwf(columns$begin, columns$end, na = na, 
        comment = comment)
    spec <- col_spec_standardise(file = ds, skip = skip, guess_max = guess_max, 
        col_names = col_names, col_types = col_types, locale = locale, 
        tokenizer = tokenizer)
    ds <- datasource(file = ds, skip = skip + isTRUE(col_names))
    if (is.null(col_types) && !inherits(ds, "source_string")) {
        show_cols_spec(spec)
    }
    res <- read_tokens(ds, tokenizer, spec$cols, names(spec$cols), 
        locale_ = locale, n_max = n_max, progress = progress)
    attr(res, "spec") <- spec
    res
})(file = file, col_names = col_names, col_types = col_types, 
    locale = locale, na = na, skip = skip, n_max = n_max, guess_max = guess_max, 
    progress = progress, comment = comment), "spec")


tokenize <- function (file, tokenizer = tokenizer_csv(), skip = 0, n_max = -1L) 
{
    ds <- datasource(file, skip = skip)
    tokenize_(ds, tokenizer, n_max)
}


spec_csv2 <- function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress()) 
attr((function (file, col_names = TRUE, col_types = NULL, locale = default_locale(), 
    na = c("", "NA"), quoted_na = TRUE, quote = "\"", comment = "", 
    trim_ws = TRUE, skip = 0, n_max = 0, guess_max = 1000, progress = show_progress()) 
{
    if (locale$decimal_mark == ".") {
        message("Using ',' as decimal and '.' as grouping mark. Use read_delim() for more control.")
        locale$decimal_mark <- ","
        locale$grouping_mark <- "."
    }
    tokenizer <- tokenizer_delim(delim = ";", na = na, quoted_na = quoted_na, 
        quote = quote, comment = comment, trim_ws = trim_ws)
    read_delimited(file, tokenizer, col_names = col_names, col_types = col_types, 
        locale = locale, skip = skip, comment = comment, n_max = n_max, 
        guess_max = guess_max, progress = progress)
})(file = file, col_names = col_names, col_types = col_types, 
    locale = locale, na = na, quoted_na = quoted_na, quote = quote, 
    comment = comment, trim_ws = trim_ws, skip = skip, n_max = n_max, 
    guess_max = guess_max, progress = progress), "spec")




## Package Data

# none


## Package Info

.skeleton_package_title = "Read Rectangular Text Data"

.skeleton_package_version = "1.1.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "Rcpp,tibble,hms,R6"


## Internal

.skeleton_version = 5


## EOF