##
## Exported symobls in package `stringi`
##

## Exported package methods

stri_width <- function (str) 
{
    .Call(C_stri_width, str)
}


stri_replace_last_charclass <- function (str, pattern, replacement) 
{
    .Call(C_stri_replace_last_charclass, str, pattern, replacement)
}


stri_replace_all_charclass <- function (str, pattern, replacement, merge = FALSE, vectorize_all = TRUE) 
{
    .Call(C_stri_replace_all_charclass, str, pattern, replacement, 
        merge, vectorize_all)
}


`stri_sub<-` <- function (str, from = 1L, to = -1L, length, omit_na = FALSE, 
    value) 
{
    if (missing(length)) {
        if (is.matrix(from) && !missing(to)) 
            warning("argument `to` is ignored in the current context")
        .Call(C_stri_sub_replacement, str, from, to, NULL, omit_na, 
            value)
    }
    else {
        if (!missing(to)) 
            warning("argument `to` is ignored in the current context")
        if (is.matrix(from)) 
            warning("argument `length` is ignored in the current context")
        .Call(C_stri_sub_replacement, str, from, NULL, length, 
            omit_na, value)
    }
}


stri_numbytes <- function (str) 
{
    .Call(C_stri_numbytes, str)
}


stri_extract_first_fixed <- function (str, pattern, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_extract_first_fixed, str, pattern, opts_fixed)
}


stri_enc_isutf32be <- function (str) 
{
    .Call(C_stri_enc_isutf32be, str)
}


stri_join_list <- function (x, sep = "", collapse = NULL) 
{
    .Call(C_stri_join_list, x, sep, collapse)
}


stri_replace_first_charclass <- function (str, pattern, replacement) 
{
    .Call(C_stri_replace_first_charclass, str, pattern, replacement)
}


stri_enc_isutf16be <- function (str) 
{
    .Call(C_stri_enc_isutf16be, str)
}


stri_trans_list <- function () 
{
    .Call(C_stri_trans_list)
}


stri_extract_all_fixed <- function (str, pattern, simplify = FALSE, omit_no_match = FALSE, 
    ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_extract_all_fixed, str, pattern, simplify, omit_no_match, 
        opts_fixed)
}


`%s===%` <- function (e1, e2) 
{
    stri_cmp_eq(e1, e2)
}


stri_locate_first_boundaries <- function (str, ..., opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_locate_first_boundaries, str, opts_brkiter)
}


stri_read_lines <- function (fname, encoding = "auto", locale = NA, fallback_encoding = stri_enc_get()) 
{
    stopifnot(is.character(encoding), length(encoding) == 1)
    txt <- stri_read_raw(fname)
    if (identical(encoding, "auto")) {
        encoding <- stri_enc_detect2(txt, locale)[[1]]$Encoding[1]
        if (is.na(encoding)) {
            if (is.na(locale)) 
                encoding <- fallback_encoding
            else stop("could not auto-detect encoding")
        }
    }
    txt <- stri_encode(txt, encoding, "UTF-8")
    stri_split_lines1(txt)
}


stri_opts_regex <- function (case_insensitive, comments, dotall, literal, multiline, 
    unix_lines, uword, error_on_unknown_escapes, ...) 
{
    opts <- list()
    if (!missing(case_insensitive)) 
        opts["case_insensitive"] <- case_insensitive
    if (!missing(comments)) 
        opts["comments"] <- comments
    if (!missing(dotall)) 
        opts["dotall"] <- dotall
    if (!missing(literal)) 
        opts["literal"] <- literal
    if (!missing(multiline)) 
        opts["multiline"] <- multiline
    if (!missing(unix_lines)) 
        opts["unix_lines"] <- unix_lines
    if (!missing(uword)) 
        opts["uword"] <- uword
    if (!missing(error_on_unknown_escapes)) 
        opts["error_on_unknown_escapes"] <- error_on_unknown_escapes
    opts
}


stri_subset_fixed <- function (str, pattern, omit_na = FALSE, negate = FALSE, ..., 
    opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_subset_fixed, str, pattern, omit_na, negate, 
        opts_fixed)
}


stri_trans_nfkc <- function (str) 
{
    .Call(C_stri_trans_nfkc, str)
}


stri_endswith <- function (str, ..., fixed, coll, charclass) 
{
    providedarg <- c(fixed = !missing(fixed), coll = !missing(coll), 
        charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `fixed`, `coll`, or `charclass`")
    if (providedarg["fixed"]) 
        stri_endswith_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_endswith_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_endswith_charclass(str, charclass, ...)
}


stri_trans_nfkd <- function (str) 
{
    .Call(C_stri_trans_nfkd, str)
}


stri_replace_all_fixed <- function (str, pattern, replacement, vectorize_all = TRUE, ..., 
    opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_replace_all_fixed, str, pattern, replacement, 
        vectorize_all, opts_fixed)
}


stri_cmp_nequiv <- function (e1, e2, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_cmp_nequiv, e1, e2, opts_collator)
}


stri_match_all_regex <- function (str, pattern, omit_no_match = FALSE, cg_missing = NA_character_, 
    ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_match_all_regex, str, pattern, omit_no_match, 
        cg_missing, opts_regex)
}


stri_replace_last_fixed <- function (str, pattern, replacement, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_replace_last_fixed, str, pattern, replacement, 
        opts_fixed)
}


stri_stats_general <- function (str) 
{
    .Call(C_stri_stats_general, str)
}


stri_replace_all_coll <- function (str, pattern, replacement, vectorize_all = TRUE, ..., 
    opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_replace_all_coll, str, pattern, replacement, 
        vectorize_all, opts_collator)
}


stri_count_boundaries <- function (str, ..., opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_count_boundaries, str, opts_brkiter)
}


stri_locale_list <- function () 
{
    .Call(C_stri_locale_list)
}


stri_locale_set <- function (locale) 
{
    previous <- stri_locale_get()
    .Call(C_stri_locale_set, locale)
    message(stri_paste("You are now working with ", stri_info(short = TRUE)))
    invisible(previous)
}


stri_enc_set <- function (enc) 
{
    previous <- stri_enc_get()
    .Call(C_stri_enc_set, enc)
    message(stri_paste("You are now working with ", stri_info(short = TRUE)))
    invisible(previous)
}


stri_trim_both <- function (str, pattern = "\\P{Wspace}") 
{
    .Call(C_stri_trim_both, str, pattern)
}


stri_timezone_list <- function (region = NA_character_, offset = NA_integer_) 
{
    .Call(C_stri_timezone_list, region, offset)
}


stri_install_check <- function (silent = FALSE) 
{
    stopifnot(is.logical(silent), length(silent) == 1)
    warning("THIS FUNCTION IS DEPRECATED")
    allok <- tryCatch({
        if (!silent) 
            message(stri_info(TRUE))
        if (length(stri_enc_list()) <= 0) 
            stop("encodings unsupported")
        if (length(stri_locale_list()) <= 0) 
            stop("locales unsupported")
        if (length(stri_trans_list()) <= 0) 
            stop("transliterators unsupported")
        TRUE
    }, error = function(e) {
        FALSE
    })
    if (!silent) {
        if (allok) 
            message("All tests completed successfully.")
        else {
            message("It seems that the ICU data library has not been installed properly.")
            message("Call stri_install_icudt() to fix this problem.")
        }
    }
    invisible(allok)
}


stri_extract_first_coll <- function (str, pattern, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_extract_first_coll, str, pattern, opts_collator)
}


`%stri!==%` <- function (e1, e2) 
{
    stri_cmp_neq(e1, e2)
}


stri_enc_detect2 <- function (str, locale = NULL) 
{
    suppressWarnings(.Call(C_stri_enc_detect2, str, locale))
}


`%s+%` <- function (e1, e2) 
{
    .Call(C_stri_join2, e1, e2)
}


stri_rand_strings <- function (n, length, pattern = "[A-Za-z0-9]") 
{
    .Call(C_stri_rand_strings, n, length, pattern)
}


stri_escape_unicode <- function (str) 
{
    .Call(C_stri_escape_unicode, str)
}


stri_pad_right <- function (str, width = floor(0.9 * getOption("width")), pad = " ", 
    use_length = FALSE) 
{
    .Call(C_stri_pad, str, width, 1L, pad, use_length)
}


stri_locate_last_coll <- function (str, pattern, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_locate_last_coll, str, pattern, opts_collator)
}


stri_trans_tolower <- function (str, locale = NULL) 
{
    .Call(C_stri_trans_tolower, str, locale)
}


stri_extract_last_coll <- function (str, pattern, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_extract_last_coll, str, pattern, opts_collator)
}


stri_timezone_set <- function (tz) 
{
    previous <- stri_timezone_get()
    .Call(C_stri_timezone_set, tz)
    invisible(previous)
}


stri_reverse <- function (str) 
{
    .Call(C_stri_reverse, str)
}


stri_enc_tonative <- function (str) 
{
    stri_encode(str, NULL, NULL)
}


stri_extract_last_boundaries <- function (str, ..., opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_extract_last_boundaries, str, opts_brkiter)
}


stri_compare <- function (e1, e2, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_cmp, e1, e2, opts_collator)
}


stri_write_lines <- function (str, fname, encoding = "UTF-8", sep = ifelse(.Platform$OS.type == 
    "windows", "\r\n", "\n")) 
{
    str <- stri_join(str, sep, collapse = "")
    str <- stri_encode(str, "", encoding, to_raw = TRUE)[[1]]
    writeBin(str, fname, useBytes = TRUE)
    invisible(NULL)
}


stri_split_lines <- function (str, omit_empty = FALSE) 
{
    .Call(C_stri_split_lines, str, omit_empty)
}


stri_trans_isnfkc_casefold <- function (str) 
{
    .Call(C_stri_trans_isnfkc_casefold, str)
}


stri_unescape_unicode <- function (str) 
{
    .Call(C_stri_unescape_unicode, str)
}


stri_split_lines1 <- function (str) 
{
    .Call(C_stri_split_lines1, str)
}


stri_stats_latex <- function (str) 
{
    .Call(C_stri_stats_latex, str)
}


stri_extract_last_regex <- function (str, pattern, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_extract_last_regex, str, pattern, opts_regex)
}


stri_trim <- function (str, side = c("both", "left", "right"), pattern = "\\P{Wspace}") 
{
    side <- match.arg(side)
    switch(side, both = stri_trim_both(str, pattern), left = stri_trim_left(str, 
        pattern), right = stri_trim_right(str, pattern))
}


stri_locate_all_words <- function (str, omit_no_match = FALSE, locale = NULL) 
{
    stri_locate_all_boundaries(str, omit_no_match, opts_brkiter = stri_opts_brkiter(type = "word", 
        skip_word_none = TRUE, locale = locale))
}


`%stri<=%` <- function (e1, e2) 
{
    stri_cmp_le(e1, e2)
}


stri_extract_all_words <- function (str, simplify = FALSE, omit_no_match = FALSE, locale = NULL) 
{
    stri_extract_all_boundaries(str, simplify, omit_no_match, 
        opts_brkiter = stri_opts_brkiter(type = "word", skip_word_none = TRUE, 
            locale = locale))
}


stri_enc_toascii <- function (str) 
{
    .Call(C_stri_enc_toascii, str)
}


stri_trim_right <- function (str, pattern = "\\P{Wspace}") 
{
    .Call(C_stri_trim_right, str, pattern)
}


stri_duplicated <- function (str, fromLast = FALSE, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_duplicated, str, fromLast, opts_collator)
}


stri_rand_lipsum <- function (nparagraphs, start_lipsum = TRUE) 
{
    nparagraphs <- as.integer(nparagraphs)
    stopifnot(is.finite(nparagraphs), nparagraphs >= 1)
    start_lipsum <- identical(start_lipsum, TRUE)
    rwords <- function(n) {
        words <- c("SED", "IN", "UT", "ET", "AC", "EU", "NON", 
            "NEC", "AMET", "SIT", "VEL", "AT", "MAURIS", "A", 
            "VITAE", "EGET", "QUIS", "NUNC", "NULLA", "ID", "VESTIBULUM", 
            "PELLENTESQUE", "TINCIDUNT", "ALIQUAM", "IPSUM", 
            "DONEC", "TURPIS", "LIGULA", "EGESTAS", "NIBH", "SAPIEN", 
            "ANTE", "NISL", "VELIT", "ERAT", "EROS", "LEO", "MAGNA", 
            "JUSTO", "ENIM", "MI", "PURUS", "EST", "LACUS", "LOREM", 
            "QUAM", "DIAM", "RISUS", "DOLOR", "SEM", "AUGUE", 
            "NEQUE", "TEMPOR", "DUI", "ARCU", "METUS", "TORTOR", 
            "URNA", "LIBERO", "PHARETRA", "TEMPUS", "FAUCIBUS", 
            "LECTUS", "SUSPENDISSE", "FELIS", "ODIO", "ORCI", 
            "VARIUS", "MASSA", "TELLUS", "VOLUTPAT", "BLANDIT", 
            "INTERDUM", "LOBORTIS", "MAXIMUS", "NISI", "LUCTUS", 
            "PORTTITOR", "AUCTOR", "ELEMENTUM", "EX", "MAECENAS", 
            "MALESUADA", "TRISTIQUE", "ULLAMCORPER", "ULTRICES", 
            "NULLAM", "CONSEQUAT", "LACINIA", "PHASELLUS", "ACCUMSAN", 
            "DAPIBUS", "ELEIFEND", "COMMODO", "DUIS", "EFFICITUR", 
            "ELIT", "IMPERDIET", "AENEAN", "IACULIS", "NAM", 
            "CONSECTETUR", "FERMENTUM", "PORTA", "SCELERISQUE", 
            "SODALES", "FEUGIAT", "LAOREET", "VULPUTATE", "DICTUM", 
            "QUISQUE", "FACILISIS", "FINIBUS", "ORNARE", "PULVINAR", 
            "RHONCUS", "CONDIMENTUM", "MOLLIS", "PRETIUM", "ALIQUET", 
            "CONGUE", "POSUERE", "SUSCIPIT", "ULTRICIES", "CURABITUR", 
            "GRAVIDA", "MATTIS", "VIVERRA", "CURSUS", "EUISMOD", 
            "RUTRUM", "VENENATIS", "CONVALLIS", "PROIN", "VEHICULA", 
            "PLACERAT", "SAGITTIS", "CRAS", "INTEGER", "MORBI", 
            "VIVAMUS", "PRAESENT", "BIBENDUM", "MOLESTIE", "SEMPER", 
            "FRINGILLA", "FUSCE", "DIGNISSIM", "ETIAM", "HENDRERIT", 
            "SOLLICITUDIN", "PER", "FAMES", "POTENTI", "AD", 
            "APTENT", "CLASS", "CONUBIA", "HIMENAEOS", "INCEPTOS", 
            "LITORA", "NOSTRA", "SOCIOSQU", "TACITI", "TORQUENT", 
            "HABITANT", "NETUS", "SENECTUS", "PRIMIS", "CUM", 
            "DIS", "MAGNIS", "MONTES", "MUS", "NASCETUR", "NATOQUE", 
            "PARTURIENT", "PENATIBUS", "RIDICULUS", "SOCIIS", 
            "ADIPISCING", "FACILISI", "CUBILIA", "CURAE", "DICTUMST", 
            "HABITASSE", "HAC", "PLATEA")
        dzipf <- function(k, N, s) 1/k^s/sum(1/(1:N)^s)
        pzipf.y <- c(0, cumsum(dzipf(1:length(words), length(words), 
            0.5)))
        robs <- findInterval(runif(n), pzipf.y)
        words[robs]
    }
    rtruncnorm <- function(n, a, b, mu, sd) {
        x <- round(rnorm(n, mu, sd))
        while (any(x < a | x > b)) x[x < a | x > b] <- round(rnorm(sum(x < 
            a | x > b), mu, sd))
        x
    }
    sent_para <- rtruncnorm(nparagraphs, 7, 20, 11, 3)
    word_sent <- lapply(sent_para, function(numsent) rtruncnorm(numsent, 
        2, Inf, 8, 3))
    totwords <- sum(unlist(word_sent))
    words <- rwords(totwords)
    seps <- sample(c(" ", ", "), replace = TRUE, size = totwords, 
        prob = c(0.9, 0.1))
    seps[cumsum(unlist(word_sent))] <- sample(c(". ", "? ", "! "), 
        size = length(unlist(word_sent)), replace = TRUE, prob = c(0.95, 
            0.025, 0.025))
    seps[cumsum(sapply(word_sent, sum))] <- ".\n"
    seps[totwords] <- "."
    if (start_lipsum) {
        words <- c("LOREM", "IPSUM", "DOLOR", "SIT", "AMET", 
            words)
        seps <- c(" ", " ", " ", " ", ", ", seps)
    }
    ret <- stri_split_charclass(stri_paste(words, seps, collapse = ""), 
        "[\\n]")[[1]]
    ret <- stri_trans_totitle(ret, opts_brkiter = stri_opts_brkiter(type = "sentence"))
    ret
}


`stri_subset_fixed<-` <- function (str, pattern, negate = FALSE, ..., opts_fixed = NULL, 
    value) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_subset_fixed_replacement, str, pattern, negate, 
        opts_fixed, value)
}


stri_unique <- function (str, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_unique, str, opts_collator)
}


stri_enc_list <- function (simplify = FALSE) 
{
    simplify <- !identical(simplify, FALSE)
    ret <- .Call(C_stri_enc_list)
    if (simplify) 
        return(stri_sort(unique(unlist(ret))))
    else return(ret)
}


stri_enc_mark <- function (str) 
{
    .Call(C_stri_enc_mark, str)
}


`%stri+%` <- function (e1, e2) 
{
    .Call(C_stri_join2, e1, e2)
}


stri_locate_last_charclass <- function (str, pattern) 
{
    .Call(C_stri_locate_last_charclass, str, pattern)
}


stri_datetime_add <- function (time, value = 1L, units = "seconds", tz = NULL, locale = NULL) 
{
    .Call(C_stri_datetime_add, time, value, units, tz, locale)
}


stri_enc_fromutf32 <- function (vec) 
{
    .Call(C_stri_enc_fromutf32, vec)
}


stri_duplicated_any <- function (str, fromLast = FALSE, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_duplicated_any, str, fromLast, opts_collator)
}


stri_extract_first_charclass <- function (str, pattern) 
{
    .Call(C_stri_extract_first_charclass, str, pattern)
}


stri_list2matrix <- function (x, byrow = FALSE, fill = NA_character_, n_min = 0) 
{
    .Call(C_stri_list2matrix, x, byrow, stri_enc_toutf8(fill), 
        n_min)
}


stri_replace_na <- function (str, replacement = "NA") 
{
    .Call(C_stri_replace_na, str, replacement)
}


stri_dup <- function (str, times) 
{
    .Call(C_stri_dup, str, times)
}


stri_trans_nfc <- function (str) 
{
    .Call(C_stri_trans_nfc, str)
}


stri_wrap <- function (str, width = floor(0.9 * getOption("width")), cost_exponent = 2, 
    simplify = TRUE, normalize = TRUE, indent = 0, exdent = 0, 
    prefix = "", initial = prefix, whitespace_only = FALSE, use_length = FALSE, 
    locale = NULL) 
{
    simplify <- as.logical(simplify)
    normalize <- as.logical(normalize)
    if (normalize) {
        str <- sapply(stri_split_lines(str), function(s) stri_flatten(s, 
            collapse = " "))
        str <- stri_trim(stri_replace_all_charclass(str, "[\\u0020\\r\\n\\t]", 
            " ", merge = TRUE))
        str <- stri_trans_nfc(str)
    }
    ret <- .Call(C_stri_wrap, str, width, cost_exponent, indent, 
        exdent, prefix, initial, whitespace_only, use_length, 
        locale)
    if (simplify) 
        as.character(unlist(ret))
    else ret
}


stri_trans_nfd <- function (str) 
{
    .Call(C_stri_trans_nfd, str)
}


stri_pad <- function (str, width = floor(0.9 * getOption("width")), side = c("left", 
    "right", "both"), pad = " ", use_length = FALSE) 
{
    side <- match.arg(side)
    switch(side, both = stri_pad_both(str, width, pad, use_length), 
        left = stri_pad_left(str, width, pad, use_length), right = stri_pad_right(str, 
            width, pad, use_length))
}


stri_cmp_equiv <- function (e1, e2, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_cmp_equiv, e1, e2, opts_collator)
}


`%stri<%` <- function (e1, e2) 
{
    stri_cmp_lt(e1, e2)
}


stri_startswith_fixed <- function (str, pattern, from = 1L, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_startswith_fixed, str, pattern, from, opts_fixed)
}


stri_extract_all_charclass <- function (str, pattern, merge = TRUE, simplify = FALSE, omit_no_match = FALSE) 
{
    .Call(C_stri_extract_all_charclass, str, pattern, merge, 
        simplify, omit_no_match)
}


stri_trans_toupper <- function (str, locale = NULL) 
{
    .Call(C_stri_trans_toupper, str, locale)
}


stri_cmp_neq <- function (e1, e2) 
{
    .Call(C_stri_cmp_neq, e1, e2)
}


stri_extract_all_coll <- function (str, pattern, simplify = FALSE, omit_no_match = FALSE, 
    ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_extract_all_coll, str, pattern, simplify, omit_no_match, 
        opts_collator)
}


stri_datetime_now <- function () 
{
    .Call(C_stri_datetime_now)
}


stri_endswith_fixed <- function (str, pattern, to = -1L, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_endswith_fixed, str, pattern, to, opts_fixed)
}


stri_split_boundaries <- function (str, n = -1L, tokens_only = FALSE, simplify = FALSE, 
    ..., opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_split_boundaries, str, n, tokens_only, simplify, 
        opts_brkiter)
}


`%s<=%` <- function (e1, e2) 
{
    stri_cmp_le(e1, e2)
}


stri_length <- function (str) 
{
    .Call(C_stri_length, str)
}


stri_enc_get <- function () 
{
    stri_enc_info(NULL)$Name.friendly
}


stri_extract_first_regex <- function (str, pattern, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_extract_first_regex, str, pattern, opts_regex)
}


stri_extract_first_boundaries <- function (str, ..., opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_extract_first_boundaries, str, opts_brkiter)
}


stri_enc_toutf32 <- function (str) 
{
    .Call(C_stri_enc_toutf32, str)
}


stri_opts_brkiter <- function (type, locale, skip_word_none, skip_word_number, skip_word_letter, 
    skip_word_kana, skip_word_ideo, skip_line_soft, skip_line_hard, 
    skip_sentence_term, skip_sentence_sep, ...) 
{
    opts <- list()
    if (!missing(type)) 
        opts["type"] <- type
    if (!missing(locale)) 
        opts["locale"] <- locale
    if (!missing(skip_word_none)) 
        opts["skip_word_none"] <- skip_word_none
    if (!missing(skip_word_number)) 
        opts["skip_word_number"] <- skip_word_number
    if (!missing(skip_word_letter)) 
        opts["skip_word_letter"] <- skip_word_letter
    if (!missing(skip_word_kana)) 
        opts["skip_word_kana"] <- skip_word_kana
    if (!missing(skip_word_ideo)) 
        opts["skip_word_ideo"] <- skip_word_ideo
    if (!missing(skip_line_soft)) 
        opts["skip_line_soft"] <- skip_line_soft
    if (!missing(skip_line_hard)) 
        opts["skip_line_hard"] <- skip_line_hard
    if (!missing(skip_sentence_term)) 
        opts["skip_sentence_term"] <- skip_sentence_term
    if (!missing(skip_sentence_sep)) 
        opts["skip_sentence_sep"] <- skip_sentence_sep
    opts
}


stri_count_fixed <- function (str, pattern, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_count_fixed, str, pattern, opts_fixed)
}


stri_count_charclass <- function (str, pattern) 
{
    .Call(C_stri_count_charclass, str, pattern)
}


stri_locate_last_boundaries <- function (str, ..., opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_locate_last_boundaries, str, opts_brkiter)
}


stri_trans_isnfc <- function (str) 
{
    .Call(C_stri_trans_isnfc, str)
}


stri_trans_isnfd <- function (str) 
{
    .Call(C_stri_trans_isnfd, str)
}


stri_timezone_info <- function (tz = NULL, locale = NULL, display_type = "long") 
{
    .Call(C_stri_timezone_info, tz, locale, display_type)
}


stri_locate_all_regex <- function (str, pattern, omit_no_match = FALSE, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_locate_all_regex, str, pattern, omit_no_match, 
        opts_regex)
}


stri_locate_first_words <- function (str, locale = NULL) 
{
    stri_locate_first_boundaries(str, opts_brkiter = stri_opts_brkiter(type = "word", 
        skip_word_none = TRUE, locale = locale))
}


stri_extract <- function (str, ..., regex, fixed, coll, charclass, mode = c("first", 
    "all", "last")) 
{
    mode <- match.arg(mode)
    switch(mode, first = stri_extract_first(str, ..., regex = regex, 
        fixed = fixed, coll = coll, charclass = charclass), last = stri_extract_last(str, 
        ..., regex = regex, fixed = fixed, coll = coll, charclass = charclass), 
        all = stri_extract_all(str, ..., regex = regex, fixed = fixed, 
            coll = coll, charclass = charclass))
}


stri_conv <- function (str, from = NULL, to = NULL, to_raw = FALSE) 
{
    .Call(C_stri_encode, str, from, to, to_raw)
}


`stri_subset_charclass<-` <- function (str, pattern, negate = FALSE, value) 
{
    .Call(C_stri_subset_charclass_replacement, str, pattern, 
        negate, value)
}


stri_trans_totitle <- function (str, ..., opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_trans_totitle, str, opts_brkiter)
}


stri_locate_all_fixed <- function (str, pattern, omit_no_match = FALSE, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_locate_all_fixed, str, pattern, omit_no_match, 
        opts_fixed)
}


`%stri===%` <- function (e1, e2) 
{
    stri_cmp_eq(e1, e2)
}


stri_locate_last_regex <- function (str, pattern, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_locate_last_regex, str, pattern, opts_regex)
}


stri_timezone_get <- function () 
{
    stri_timezone_info()$ID
}


stri_c_list <- function (x, sep = "", collapse = NULL) 
{
    .Call(C_stri_join_list, x, sep, collapse)
}


stri_endswith_coll <- function (str, pattern, to = -1L, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_endswith_coll, str, pattern, to, opts_collator)
}


stri_encode <- function (str, from = NULL, to = NULL, to_raw = FALSE) 
{
    .Call(C_stri_encode, str, from, to, to_raw)
}


stri_match <- function (str, ..., regex, mode = c("first", "all", "last")) 
{
    mode <- match.arg(mode)
    switch(mode, first = stri_match_first_regex(str, regex, ...), 
        last = stri_match_last_regex(str, regex, ...), all = stri_match_all_regex(str, 
            regex, ...))
}


stri_endswith_charclass <- function (str, pattern, to = -1L) 
{
    .Call(C_stri_endswith_charclass, str, pattern, to)
}


`%s<%` <- function (e1, e2) 
{
    stri_cmp_lt(e1, e2)
}


stri_datetime_parse <- function (str, format = "uuuu-MM-dd HH:mm:ss", lenient = FALSE, 
    tz = NULL, locale = NULL) 
{
    .Call(C_stri_datetime_parse, str, format, lenient, tz, locale)
}


stri_match_first_regex <- function (str, pattern, cg_missing = NA_character_, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_match_first_regex, str, pattern, cg_missing, 
        opts_regex)
}


stri_detect_coll <- function (str, pattern, negate = FALSE, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_detect_coll, str, pattern, negate, opts_collator)
}


stri_extract_all_boundaries <- function (str, simplify = FALSE, omit_no_match = FALSE, ..., 
    opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_extract_all_boundaries, str, simplify, omit_no_match, 
        opts_brkiter)
}


stri_datetime_fstr <- function (x) 
{
    warn <- c("%U", "%V", "%x", "%X", "%u", "%w", "%r", "%g", 
        "%G", "%c")
    search <- c("%U", "%W", "%g", "%G")
    needle <- c("ww", "ww", "yy", "Y")
    search <- c(search, "%a", "%A", "%b", "%B")
    needle <- c(needle, "ccc", "cccc", "LLL", "LLLL")
    search <- c(search, "%c", "%d", "%D")
    needle <- c(needle, "eee LLL d HH:mm:ss yyyy", "dd", "MM/dd/yy")
    search <- c(search, "%e", "%F", "%h", "%H")
    needle <- c(needle, "d", "yyyy-MM-dd", "MMM", "HH")
    search <- c(search, "%I", "%j", "%m", "%M", "%n", "%p")
    needle <- c(needle, "hh", "D", "MM", "mm", "\n", "a")
    search <- c(search, "%r", "%R", "%S", "%t", "%T", "%u")
    needle <- c(needle, "hh:mm:ss", "HH:mm", "ss", "\t", "HH:mm:ss", 
        "c")
    search <- c(search, "%V", "%w", "%x", "%X", "%y", "%Y", "%z", 
        "%Z")
    needle <- c(needle, "ww", "c", "yy/MM/dd", "HH:mm:ss", "yy", 
        "yyyy", "Z", "z")
    x <- stri_replace_all_fixed(x, "'", "\\'")
    x <- stri_replace_all_fixed(x, "%%", "%!")
    x <- stri_replace_all_regex(x, "(?:(?<=[%][A-Za-z])|^(?![%][A-Za-z]))(.+?)(?:(?<![%][A-Za-z])$|(?=[%][A-Za-z]))", 
        "'$1'")
    if (any(stri_detect_regex(x, stri_flatten(warn, collapse = "|")))) 
        warning(sprintf("Formatters %s might not be 100%% compatible with ICU", 
            stri_flatten(warn, collapse = ", ")))
    x <- stri_replace_all_fixed(x, search, needle, vectorize_all = FALSE)
    if (any(stri_detect_regex(x, "%[A-Za-z]"))) {
        warning("Unsupported date/time format specifier. Ignoring")
        x <- stri_replace_all_regex(x, "%[A-Za-z]", "%?")
    }
    x <- stri_replace_all_fixed(x, "%!", "%")
    x
}


stri_pad_both <- function (str, width = floor(0.9 * getOption("width")), pad = " ", 
    use_length = FALSE) 
{
    .Call(C_stri_pad, str, width, 2L, pad, use_length)
}


stri_enc_isascii <- function (str) 
{
    .Call(C_stri_enc_isascii, str)
}


stri_replace <- function (str, replacement, ..., regex, fixed, coll, charclass, 
    mode = c("first", "all", "last")) 
{
    mode <- match.arg(mode)
    switch(mode, first = stri_replace_first(str, replacement, 
        ..., regex = regex, fixed = fixed, coll = coll, charclass = charclass), 
        last = stri_replace_last(str, replacement, ..., regex = regex, 
            fixed = fixed, coll = coll, charclass = charclass), 
        all = stri_replace_all(str, replacement, ..., regex = regex, 
            fixed = fixed, coll = coll, charclass = charclass))
}


stri_c <- function (..., sep = "", collapse = NULL, ignore_null = FALSE) 
{
    .Call(C_stri_join, list(...), sep, collapse, ignore_null)
}


stri_locate <- function (str, ..., regex, fixed, coll, charclass, mode = c("first", 
    "all", "last")) 
{
    mode <- match.arg(mode)
    switch(mode, first = stri_locate_first(str, ..., regex = regex, 
        fixed = fixed, coll = coll, charclass = charclass), last = stri_locate_last(str, 
        ..., regex = regex, fixed = fixed, coll = coll, charclass = charclass), 
        all = stri_locate_all(str, ..., regex = regex, fixed = fixed, 
            coll = coll, charclass = charclass))
}


stri_pad_left <- function (str, width = floor(0.9 * getOption("width")), pad = " ", 
    use_length = FALSE) 
{
    .Call(C_stri_pad, str, width, 0L, pad, use_length)
}


stri_locale_info <- function (locale = NULL) 
{
    .Call(C_stri_locale_info, locale)
}


stri_flatten <- function (str, collapse = "") 
{
    .Call(C_stri_flatten, str, collapse)
}


stri_trans_char <- function (str, pattern, replacement) 
{
    .Call(C_stri_trans_char, str, pattern, replacement)
}


stri_replace_first_regex <- function (str, pattern, replacement, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_replace_first_regex, str, pattern, replacement, 
        opts_regex)
}


stri_paste <- function (..., sep = "", collapse = NULL, ignore_null = FALSE) 
{
    .Call(C_stri_join, list(...), sep, collapse, ignore_null)
}


stri_locate_last <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_locate_last_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_locate_last_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_locate_last_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_locate_last_charclass(str, charclass, ...)
}


stri_locate_last_fixed <- function (str, pattern, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_locate_last_fixed, str, pattern, opts_fixed)
}


stri_replace_all <- function (str, replacement, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_replace_all_regex(str, regex, replacement, ...)
    else if (providedarg["fixed"]) 
        stri_replace_all_fixed(str, fixed, replacement, ...)
    else if (providedarg["coll"]) 
        stri_replace_all_coll(str, coll, replacement, ...)
    else if (providedarg["charclass"]) 
        stri_replace_all_charclass(str, charclass, replacement, 
            ...)
}


stri_detect_charclass <- function (str, pattern, negate = FALSE) 
{
    .Call(C_stri_detect_charclass, str, pattern, negate)
}


stri_cmp_gt <- function (e1, e2, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_cmp_gt, e1, e2, opts_collator)
}


stri_split_fixed <- function (str, pattern, n = -1L, omit_empty = FALSE, tokens_only = FALSE, 
    simplify = FALSE, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_split_fixed, str, pattern, n, omit_empty, tokens_only, 
        simplify, opts_fixed)
}


stri_datetime_format <- function (time, format = "uuuu-MM-dd HH:mm:ss", tz = NULL, locale = NULL) 
{
    .Call(C_stri_datetime_format, time, format, tz, locale)
}


stri_count <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_count_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_count_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_count_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_count_charclass(str, charclass, ...)
}


stri_join <- function (..., sep = "", collapse = NULL, ignore_null = FALSE) 
{
    .Call(C_stri_join, list(...), sep, collapse, ignore_null)
}


stri_sort <- function (str, decreasing = FALSE, na_last = NA, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_sort, str, decreasing, na_last, opts_collator)
}


stri_trans_isnfkc <- function (str) 
{
    .Call(C_stri_trans_isnfkc, str)
}


`%s!=%` <- function (e1, e2) 
{
    stri_cmp_nequiv(e1, e2)
}


stri_trans_isnfkd <- function (str) 
{
    .Call(C_stri_trans_isnfkd, str)
}


stri_opts_collator <- function (locale = NULL, strength = 3L, alternate_shifted = FALSE, 
    french = FALSE, uppercase_first = NA, case_level = FALSE, 
    normalization = FALSE, numeric = FALSE, ...) 
{
    opts <- list()
    if (!missing(locale)) 
        opts["locale"] <- locale
    if (!missing(strength)) 
        opts["strength"] <- strength
    if (!missing(alternate_shifted)) 
        opts["alternate_shifted"] <- alternate_shifted
    if (!missing(french)) 
        opts["french"] <- french
    if (!missing(uppercase_first)) 
        opts["uppercase_first"] <- uppercase_first
    if (!missing(case_level)) 
        opts["case_level"] <- case_level
    if (!missing(normalization)) 
        opts["normalization"] <- normalization
    if (!missing(numeric)) 
        opts["numeric"] <- numeric
    opts
}


`%stri!=%` <- function (e1, e2) 
{
    stri_cmp_nequiv(e1, e2)
}


`%s>%` <- function (e1, e2) 
{
    stri_cmp_gt(e1, e2)
}


`stri_subset_coll<-` <- function (str, pattern, negate = FALSE, ..., opts_collator = NULL, 
    value) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_subset_coll_replacement, str, pattern, negate, 
        opts_collator, value)
}


stri_replace_last <- function (str, replacement, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_replace_last_regex(str, regex, replacement, ...)
    else if (providedarg["fixed"]) 
        stri_replace_last_fixed(str, fixed, replacement, ...)
    else if (providedarg["coll"]) 
        stri_replace_last_coll(str, coll, replacement, ...)
    else if (providedarg["charclass"]) 
        stri_replace_last_charclass(str, charclass, replacement, 
            ...)
}


`%s==%` <- function (e1, e2) 
{
    stri_cmp_equiv(e1, e2)
}


stri_subset <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_subset_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_subset_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_subset_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_subset_charclass(str, charclass, ...)
}


stri_startswith_charclass <- function (str, pattern, from = 1L) 
{
    .Call(C_stri_startswith_charclass, str, pattern, from)
}


`stri_datetime_add<-` <- function (time, units = "seconds", tz = NULL, locale = NULL, 
    value) 
{
    .Call(C_stri_datetime_add, time, value, units, tz, locale)
}


stri_detect_regex <- function (str, pattern, negate = FALSE, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_detect_regex, str, pattern, negate, opts_regex)
}


stri_extract_first_words <- function (str, locale = NULL) 
{
    stri_extract_first_boundaries(str, opts_brkiter = stri_opts_brkiter(type = "word", 
        skip_word_none = TRUE, locale = locale))
}


`%stri==%` <- function (e1, e2) 
{
    stri_cmp_equiv(e1, e2)
}


stri_split_coll <- function (str, pattern, n = -1L, omit_empty = FALSE, tokens_only = FALSE, 
    simplify = FALSE, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_split_coll, str, pattern, n, omit_empty, tokens_only, 
        simplify, opts_collator)
}


stri_locate_first_charclass <- function (str, pattern) 
{
    .Call(C_stri_locate_first_charclass, str, pattern)
}


stri_isempty <- function (str) 
{
    .Call(C_stri_isempty, str)
}


stri_paste_list <- function (x, sep = "", collapse = NULL) 
{
    .Call(C_stri_join_list, x, sep, collapse)
}


stri_cmp_ge <- function (e1, e2, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_cmp_ge, e1, e2, opts_collator)
}


`%s!==%` <- function (e1, e2) 
{
    stri_cmp_neq(e1, e2)
}


stri_trim_left <- function (str, pattern = "\\P{Wspace}") 
{
    .Call(C_stri_trim_left, str, pattern)
}


stri_locale_get <- function () 
{
    stri_locale_info(NULL)$Name
}


stri_order <- function (str, decreasing = FALSE, na_last = TRUE, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_order, str, decreasing, na_last, opts_collator)
}


`stri_subset<-` <- function (str, ..., regex, fixed, coll, charclass, value) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        `stri_subset_regex<-`(str, regex, ..., value = value)
    else if (providedarg["fixed"]) 
        `stri_subset_fixed<-`(str, fixed, ..., value = value)
    else if (providedarg["coll"]) 
        `stri_subset_coll<-`(str, coll, ..., value = value)
    else if (providedarg["charclass"]) 
        `stri_subset_charclass<-`(str, charclass, ..., value = value)
}


stri_datetime_fields <- function (time, tz = attr(time, "tzone"), locale = NULL) 
{
    as.data.frame(.Call(C_stri_datetime_fields, time, tz, locale))
}


stri_locate_all_charclass <- function (str, pattern, merge = TRUE, omit_no_match = FALSE) 
{
    .Call(C_stri_locate_all_charclass, str, pattern, merge, omit_no_match)
}


stri_enc_isutf8 <- function (str) 
{
    .Call(C_stri_enc_isutf8, str)
}


stri_locate_first_coll <- function (str, pattern, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_locate_first_coll, str, pattern, opts_collator)
}


stri_locate_all_boundaries <- function (str, omit_no_match = FALSE, ..., opts_brkiter = NULL) 
{
    if (!missing(...)) 
        opts_brkiter <- do.call(stri_opts_brkiter, as.list(c(opts_brkiter, 
            ...)))
    .Call(C_stri_locate_all_boundaries, str, omit_no_match, opts_brkiter)
}


stri_enc_detect <- function (str, filter_angle_brackets = FALSE) 
{
    .Call(C_stri_enc_detect, str, filter_angle_brackets)
}


stri_locate_all <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_locate_all_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_locate_all_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_locate_all_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_locate_all_charclass(str, charclass, ...)
}


stri_cmp_eq <- function (e1, e2) 
{
    .Call(C_stri_cmp_eq, e1, e2)
}


stri_extract_last_words <- function (str, locale = NULL) 
{
    stri_extract_last_boundaries(str, opts_brkiter = stri_opts_brkiter(type = "word", 
        skip_word_none = TRUE, locale = locale))
}


stri_replace_first_fixed <- function (str, pattern, replacement, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_replace_first_fixed, str, pattern, replacement, 
        opts_fixed)
}


stri_match_all <- function (str, ..., regex) 
{
    stri_match_all_regex(str, regex, ...)
}


stri_trans_general <- function (str, id) 
{
    .Call(C_stri_trans_general, str, id)
}


stri_replace_last_regex <- function (str, pattern, replacement, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_replace_last_regex, str, pattern, replacement, 
        opts_regex)
}


stri_startswith <- function (str, ..., fixed, coll, charclass) 
{
    providedarg <- c(fixed = !missing(fixed), coll = !missing(coll), 
        charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `fixed`, `coll`, or `charclass`")
    if (providedarg["fixed"]) 
        stri_startswith_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_startswith_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_startswith_charclass(str, charclass, ...)
}


stri_enc_isutf32le <- function (str) 
{
    .Call(C_stri_enc_isutf32le, str)
}


stri_cmp_lt <- function (e1, e2, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_cmp_lt, e1, e2, opts_collator)
}


stri_subset_regex <- function (str, pattern, omit_na = FALSE, negate = FALSE, ..., 
    opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_subset_regex, str, pattern, omit_na, negate, 
        opts_regex)
}


stri_match_last_regex <- function (str, pattern, cg_missing = NA_character_, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_match_last_regex, str, pattern, cg_missing, 
        opts_regex)
}


`stri_subset_regex<-` <- function (str, pattern, negate = FALSE, ..., opts_regex = NULL, 
    value) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_subset_regex_replacement, str, pattern, negate, 
        opts_regex, value)
}


stri_enc_toutf8 <- function (str, is_unknown_8bit = FALSE, validate = FALSE) 
{
    .Call(C_stri_enc_toutf8, str, is_unknown_8bit, validate)
}


stri_extract_first <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_extract_first_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_extract_first_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_extract_first_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_extract_first_charclass(str, charclass, ...)
}


stri_enc_isutf16le <- function (str) 
{
    .Call(C_stri_enc_isutf16le, str)
}


stri_locate_first_regex <- function (str, pattern, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_locate_first_regex, str, pattern, opts_regex)
}


stri_opts_fixed <- function (case_insensitive = FALSE, overlap = FALSE, ...) 
{
    opts <- list()
    if (!missing(case_insensitive)) 
        opts["case_insensitive"] <- case_insensitive
    if (!missing(overlap)) 
        opts["overlap"] <- overlap
    opts
}


stri_sub <- function (str, from = 1L, to = -1L, length) 
{
    if (missing(length)) {
        if (is.matrix(from) && !missing(to)) 
            warning("argument `to` is ignored in the current context")
        .Call(C_stri_sub, str, from, to, NULL)
    }
    else {
        if (!missing(to)) 
            warning("argument `to` is ignored in the current context")
        if (is.matrix(from)) 
            warning("argument `length` is ignored in the current context")
        .Call(C_stri_sub, str, from, NULL, length)
    }
}


stri_split_regex <- function (str, pattern, n = -1L, omit_empty = FALSE, tokens_only = FALSE, 
    simplify = FALSE, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_split_regex, str, pattern, n, omit_empty, tokens_only, 
        simplify, opts_regex)
}


stri_enc_info <- function (enc = NULL) 
{
    .Call(C_stri_enc_info, enc)
}


stri_extract_all <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_extract_all_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_extract_all_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_extract_all_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_extract_all_charclass(str, charclass, ...)
}


stri_match_first <- function (str, ..., regex) 
{
    stri_match_first_regex(str, regex, ...)
}


stri_read_raw <- function (fname) 
{
    stopifnot(is.character(fname), length(fname) == 1, file.exists(fname))
    fsize <- file.info(fname)$size
    readBin(fname, what = "raw", size = 1, n = fsize)
}


stri_cmp <- function (e1, e2, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_cmp, e1, e2, opts_collator)
}


`%stri>%` <- function (e1, e2) 
{
    stri_cmp_gt(e1, e2)
}


stri_detect <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_detect_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_detect_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_detect_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_detect_charclass(str, charclass, ...)
}


stri_trans_nfkc_casefold <- function (str) 
{
    .Call(C_stri_trans_nfkc_casefold, str)
}


stri_rand_shuffle <- function (str) 
{
    .Call(C_stri_rand_shuffle, str)
}


stri_count_coll <- function (str, pattern, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_count_coll, str, pattern, opts_collator)
}


stri_info <- function (short = FALSE) 
{
    stopifnot(is.logical(short), length(short) == 1)
    info <- .Call(C_stri_info)
    if (info$Charset.native$Name.friendly != "UTF-8") {
        if (!identical(info$Charset.native$ASCII.subset, TRUE)) 
            warning(stri_paste("Your native charset is not a superset of US-ASCII. ", 
                "This may cause serious problems. Consider switching to UTF-8."))
        else if (!identical(info$Charset.native$Unicode.1to1, 
            TRUE)) 
            warning(stri_paste("Your native charset does not map to Unicode well. ", 
                "This may cause serious problems. Consider switching to UTF-8."))
    }
    loclist <- stri_locale_list()
    if (!(info$Locale$Name %in% loclist)) 
        warning(stri_paste("Your current locale is not in the list of available ", 
            "locales. Some functions may not work properly. ", 
            "Refer to stri_locale_list() for more details ", 
            "on known locale specifiers."))
    if (!short) 
        return(info)
    else {
        locale <- info$Locale$Name
        charset <- info$Charset.native$Name.friendly
        return(sprintf("stringi_%s (%s.%s; ICU4C %s [%s]; Unicode %s)", 
            as.character(packageVersion("stringi")), locale, 
            charset, info$ICU.version, if (info$ICU.system) "system" else "bundle", 
            info$Unicode.version))
    }
}


stri_locate_last_words <- function (str, locale = NULL) 
{
    stri_locate_last_boundaries(str, opts_brkiter = stri_opts_brkiter(type = "word", 
        skip_word_none = TRUE, locale = locale))
}


stri_match_last <- function (str, ..., regex) 
{
    stri_match_last_regex(str, regex, ...)
}


stri_datetime_create <- function (year, month, day, hour = 12L, minute = 0L, second = 0, 
    lenient = FALSE, tz = NULL, locale = NULL) 
{
    .Call(C_stri_datetime_create, year, month, day, hour, minute, 
        second, lenient, tz, locale)
}


stri_subset_charclass <- function (str, pattern, omit_na = FALSE, negate = FALSE) 
{
    .Call(C_stri_subset_charclass, str, pattern, omit_na, negate)
}


stri_cmp_le <- function (e1, e2, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_cmp_le, e1, e2, opts_collator)
}


stri_extract_last_fixed <- function (str, pattern, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_extract_last_fixed, str, pattern, opts_fixed)
}


stri_replace_last_coll <- function (str, pattern, replacement, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_replace_last_coll, str, pattern, replacement, 
        opts_collator)
}


stri_detect_fixed <- function (str, pattern, negate = FALSE, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_detect_fixed, str, pattern, negate, opts_fixed)
}


stri_count_words <- function (str, locale = NULL) 
{
    stri_count_boundaries(str, opts_brkiter = stri_opts_brkiter(type = "word", 
        skip_word_none = TRUE, locale = locale))
}


stri_datetime_symbols <- function (locale = NULL, context = "standalone", width = "wide") 
{
    .Call(C_stri_datetime_symbols, locale, context, width)
}


stri_install_icudt <- function (check = TRUE, outpath = NULL, inpath = NULL) 
{
    stopifnot(is.logical(check), length(check) == 1, !is.na(check))
    if (check && stri_install_check(TRUE)) {
        message("icudt has been already installed.")
        return(invisible(TRUE))
    }
    if (check) 
        warning("THIS FUNCTION IS DEPRECATED")
    if (is.null(outpath)) 
        outpath <- file.path(path.package("stringi"), "libs")
    stopifnot(is.character(outpath), length(outpath) == 1, file.exists(outpath))
    fname <- if (.Platform$endian == "little") 
        "icudt55l.zip"
    else "icudt55b.zip"
    md5ex <- if (.Platform$endian == "little") 
        "ff345529f230cc39bb8d450af0607708"
    else "1194f0dd879d3c1c1f189cde5fd90efe"
    mirrors <- c("http://static.rexamine.com/packages/", "http://www.mini.pw.edu.pl/~gagolews/stringi/", 
        "http://www.ibspan.waw.pl/~gagolews/stringi/")
    if (!is.null(inpath)) {
        stopifnot(is.character(inpath), length(inpath) > 0, !is.na(inpath))
        mirrors <- c(inpath, mirrors)
    }
    outfname <- tempfile(fileext = ".zip")
    download_from_mirror <- function(href, fname, outfname) {
        tryCatch({
            suppressWarnings(file.remove(outfname))
            if (!grepl("^https?://", href)) {
                if (!file.exists(file.path(href, fname))) 
                  return("no icudt in a local repo")
                message("icudt has been found in a local repo")
                file.copy(file.path(href, fname), outfname)
            }
            else {
                if (download.file(paste(href, fname, sep = ""), 
                  outfname, mode = "wb") != 0) 
                  return("download error")
            }
            if (!file.exists(outfname)) 
                return("download error")
            md5ob <- tools::md5sum(outfname)
            if (is.na(md5ob)) 
                return("error checking md5sum")
            if (md5ob != md5ex) 
                return("md5sum mismatch")
            TRUE
        }, error = function(e) as.character(e))
    }
    message("downloading ICU data library (icudt)")
    message("the files will be extracted to: ", outpath)
    allok <- FALSE
    for (m in mirrors) {
        if (identical(status <- download_from_mirror(m, fname, 
            outfname), TRUE)) {
            allok <- TRUE
            break
        }
        else message(status)
    }
    if (!allok) {
        message("icudt download failed")
        return(invisible(FALSE))
    }
    message("icudt fetch OK")
    message("decompressing downloaded archive")
    res <- unzip(outfname, exdir = outpath, overwrite = TRUE)
    if (!is.character(res) || length(res) <= 0) {
        message("error decompressing archive")
        return(invisible(FALSE))
    }
    suppressWarnings(file.remove(outfname))
    message("icudt has been installed successfully")
    invisible(TRUE)
}


stri_split_charclass <- function (str, pattern, n = -1L, omit_empty = FALSE, tokens_only = FALSE, 
    simplify = FALSE) 
{
    .Call(C_stri_split_charclass, str, pattern, n, omit_empty, 
        tokens_only, simplify)
}


stri_locate_first_fixed <- function (str, pattern, ..., opts_fixed = NULL) 
{
    if (!missing(...)) 
        opts_fixed <- do.call(stri_opts_fixed, as.list(c(opts_fixed, 
            ...)))
    .Call(C_stri_locate_first_fixed, str, pattern, opts_fixed)
}


stri_extract_all_regex <- function (str, pattern, simplify = FALSE, omit_no_match = FALSE, 
    ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_extract_all_regex, str, pattern, simplify, omit_no_match, 
        opts_regex)
}


stri_replace_all_regex <- function (str, pattern, replacement, vectorize_all = TRUE, ..., 
    opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_replace_all_regex, str, pattern, replacement, 
        vectorize_all, opts_regex)
}


stri_replace_first <- function (str, replacement, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_replace_first_regex(str, regex, replacement, ...)
    else if (providedarg["fixed"]) 
        stri_replace_first_fixed(str, fixed, replacement, ...)
    else if (providedarg["coll"]) 
        stri_replace_first_coll(str, coll, replacement, ...)
    else if (providedarg["charclass"]) 
        stri_replace_first_charclass(str, charclass, replacement, 
            ...)
}


stri_extract_last <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_extract_last_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_extract_last_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_extract_last_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_extract_last_charclass(str, charclass, ...)
}


stri_subset_coll <- function (str, pattern, omit_na = FALSE, negate = FALSE, ..., 
    opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_subset_coll, str, pattern, omit_na, negate, 
        opts_collator)
}


stri_count_regex <- function (str, pattern, ..., opts_regex = NULL) 
{
    if (!missing(...)) 
        opts_regex <- do.call(stri_opts_regex, as.list(c(opts_regex, 
            ...)))
    .Call(C_stri_count_regex, str, pattern, opts_regex)
}


stri_locate_all_coll <- function (str, pattern, omit_no_match = FALSE, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_locate_all_coll, str, pattern, omit_no_match, 
        opts_collator)
}


stri_replace_first_coll <- function (str, pattern, replacement, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_replace_first_coll, str, pattern, replacement, 
        opts_collator)
}


`%s>=%` <- function (e1, e2) 
{
    stri_cmp_ge(e1, e2)
}


stri_startswith_coll <- function (str, pattern, from = 1L, ..., opts_collator = NULL) 
{
    if (!missing(...)) 
        opts_collator <- do.call(stri_opts_collator, as.list(c(opts_collator, 
            ...)))
    .Call(C_stri_startswith_coll, str, pattern, from, opts_collator)
}


stri_split <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_split_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_split_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_split_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_split_charclass(str, charclass, ...)
}


`%stri>=%` <- function (e1, e2) 
{
    stri_cmp_ge(e1, e2)
}


stri_extract_last_charclass <- function (str, pattern) 
{
    .Call(C_stri_extract_last_charclass, str, pattern)
}


stri_locate_first <- function (str, ..., regex, fixed, coll, charclass) 
{
    providedarg <- c(regex = !missing(regex), fixed = !missing(fixed), 
        coll = !missing(coll), charclass = !missing(charclass))
    if (sum(providedarg) != 1) 
        stop("you have to specify either `regex`, `fixed`, `coll`, or `charclass`")
    if (providedarg["regex"]) 
        stri_locate_first_regex(str, regex, ...)
    else if (providedarg["fixed"]) 
        stri_locate_first_fixed(str, fixed, ...)
    else if (providedarg["coll"]) 
        stri_locate_first_coll(str, coll, ...)
    else if (providedarg["charclass"]) 
        stri_locate_first_charclass(str, charclass, ...)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Character String Processing Facilities"

.skeleton_package_version = "1.1.2"

.skeleton_package_depends = ""

.skeleton_package_imports = "tools,utils,stats"


## Internal

.skeleton_version = 5


## EOF