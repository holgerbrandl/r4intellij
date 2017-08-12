##
## Exported symobls in package `stringr`
##

## Exported package methods

str_length <- function (string) 
{
    stri_length(string)
}


invert_match <- function (loc) 
{
    cbind(start = c(0L, loc[, "end"] + 1L), end = c(loc[, "start"] - 
        1L, -1L))
}


str_to_upper <- function (string, locale = "en") 
{
    stri_trans_toupper(string, locale = locale)
}


word <- function (string, start = 1L, end = start, sep = fixed(" ")) 
{
    n <- max(length(string), length(start), length(end))
    string <- rep(string, length.out = n)
    start <- rep(start, length.out = n)
    end <- rep(end, length.out = n)
    breaks <- str_locate_all(string, sep)
    words <- lapply(breaks, invert_match)
    len <- vapply(words, nrow, integer(1))
    neg_start <- !is.na(start) & start < 0L
    start[neg_start] <- start[neg_start] + len[neg_start] + 1L
    neg_end <- !is.na(end) & end < 0L
    end[neg_end] <- end[neg_end] + len[neg_end] + 1L
    start[start > len] <- NA
    end[end > len] <- NA
    starts <- mapply(function(word, loc) word[loc, "start"], 
        words, start)
    ends <- mapply(function(word, loc) word[loc, "end"], words, 
        end)
    str_sub(string, starts, ends)
}


str_extract_all <- function (string, pattern, simplify = FALSE) 
{
    switch(type(pattern), empty = stri_extract_all_boundaries(string, 
        pattern, simplify = simplify, omit_no_match = TRUE, opts_brkiter = opts(pattern)), 
        bound = stri_extract_all_boundaries(string, pattern, 
            simplify = simplify, omit_no_match = TRUE, opts_brkiter = opts(pattern)), 
        fixed = stri_extract_all_fixed(string, pattern, simplify = simplify, 
            omit_no_match = TRUE, opts_fixed = opts(pattern)), 
        coll = stri_extract_all_coll(string, pattern, simplify = simplify, 
            omit_no_match = TRUE, opts_collator = opts(pattern)), 
        regex = stri_extract_all_regex(string, pattern, simplify = simplify, 
            omit_no_match = TRUE, opts_regex = opts(pattern)))
}


str_count <- function (string, pattern = "") 
{
    switch(type(pattern), empty = stri_count_boundaries(string, 
        opts_brkiter = opts(pattern)), bound = stri_count_boundaries(string, 
        opts_brkiter = opts(pattern)), fixed = stri_count_fixed(string, 
        pattern, opts_fixed = opts(pattern)), coll = stri_count_coll(string, 
        pattern, opts_collator = opts(pattern)), regex = stri_count_regex(string, 
        pattern, opts_regex = opts(pattern)))
}


str_detect <- function (string, pattern) 
{
    switch(type(pattern), empty = , bound = str_count(string, 
        pattern) > 0, fixed = stri_detect_fixed(string, pattern, 
        opts_fixed = opts(pattern)), coll = stri_detect_coll(string, 
        pattern, opts_collator = opts(pattern)), regex = stri_detect_regex(string, 
        pattern, opts_regex = opts(pattern)))
}


str_to_title <- function (string, locale = "en") 
{
    stri_trans_totitle(string, opts_brkiter = stri_opts_brkiter(locale = locale))
}


str_wrap <- function (string, width = 80, indent = 0, exdent = 0) 
{
    if (width <= 0) 
        width <- 1
    out <- stri_wrap(string, width = width, indent = indent, 
        exdent = exdent, simplify = FALSE)
    vapply(out, str_c, collapse = "\n", character(1))
}


str_which <- function (string, pattern) 
{
    which(str_detect(string, pattern))
}


str_interp <- function (string, env = parent.frame()) 
{
    if (!is.character(string)) {
        stop("string argument is not character.", call. = FALSE)
    }
    string <- str_c(string, collapse = "")
    matches <- interp_placeholders(string)
    if (matches$indices[1] <= 0) {
        string
    }
    else {
        replacements <- eval_interp_matches(matches$matches, 
            env)
        `regmatches<-`(string, list(matches$indices), FALSE, 
            list(replacements))
    }
}


str_dup <- function (string, times) 
{
    stri_dup(string, times)
}


str_replace_na <- function (string, replacement = "NA") 
{
    stri_replace_na(string, replacement)
}


str_trunc <- function (string, width, side = c("right", "left", "center"), 
    ellipsis = "...") 
{
    side <- match.arg(side)
    too_long <- str_length(string) > width
    width... <- width - str_length(ellipsis)
    string[too_long] <- switch(side, right = str_c(str_sub(string[too_long], 
        1, width...), ellipsis), left = str_c(ellipsis, str_sub(string[too_long], 
        -width..., -1)), center = str_c(str_sub(string[too_long], 
        1, ceiling(width.../2)), ellipsis, str_sub(string[too_long], 
        -floor(width.../2), -1)))
    string
}


str_c <- function (..., sep = "", collapse = NULL) 
{
    stri_c(..., sep = sep, collapse = collapse, ignore_null = TRUE)
}


str_split_fixed <- function (string, pattern, n) 
{
    out <- str_split(string, pattern, n = n, simplify = TRUE)
    out[is.na(out)] <- ""
    out
}


str_pad <- function (string, width, side = c("left", "right", "both"), pad = " ") 
{
    side <- match.arg(side)
    switch(side, left = stri_pad_left(string, width, pad = pad), 
        right = stri_pad_right(string, width, pad = pad), both = stri_pad_both(string, 
            width, pad = pad))
}


str_split <- function (string, pattern, n = Inf, simplify = FALSE) 
{
    if (identical(n, Inf)) 
        n <- -1L
    switch(type(pattern), empty = stri_split_boundaries(string, 
        n = n, simplify = simplify, opts_brkiter = opts(pattern)), 
        bound = stri_split_boundaries(string, n = n, simplify = simplify, 
            opts_brkiter = opts(pattern)), fixed = stri_split_fixed(string, 
            pattern, n = n, simplify = simplify, opts_fixed = opts(pattern)), 
        regex = stri_split_regex(string, pattern, n = n, simplify = simplify, 
            opts_regex = opts(pattern)), coll = stri_split_coll(string, 
            pattern, n = n, simplify = simplify, opts_collator = opts(pattern)))
}


perl <- function (pattern) 
{
    message("perl is deprecated. Please use regex() instead")
    regex(pattern)
}


`%>%` <- magrittr::`%>%` # re-exported from magrittr package

str_replace <- function (string, pattern, replacement) 
{
    if (!missing(replacement) && is.function(replacement)) {
        return(str_transform(string, pattern, replacement))
    }
    switch(type(pattern), empty = , bound = stop("Not implemented", 
        call. = FALSE), fixed = stri_replace_first_fixed(string, 
        pattern, replacement, opts_fixed = opts(pattern)), coll = stri_replace_first_coll(string, 
        pattern, replacement, opts_collator = opts(pattern)), 
        regex = stri_replace_first_regex(string, pattern, fix_replacement(replacement), 
            opts_regex = opts(pattern)))
}


ignore.case <- function (string) 
{
    message("Please use (fixed|coll|regex)(x, ignore_case = TRUE) instead of ignore.case(x)")
    fixed(string, ignore_case = TRUE)
}


str_join <- function (..., sep = "", collapse = NULL) 
{
    .Deprecated("str_c")
    stri_c(..., sep = sep, collapse = collapse, ignore_null = TRUE)
}


str_view <- function (string, pattern, match = NA) 
{
    if (identical(match, TRUE)) {
        string <- string[str_detect(string, pattern)]
    }
    else if (identical(match, FALSE)) {
        string <- string[!str_detect(string, pattern)]
    }
    loc <- str_locate(string, pattern)
    has_match <- !is.na(loc[, "start"])
    str_sub(string[has_match], loc[has_match, , drop = FALSE]) <- paste0("<span class='match'>", 
        str_sub(string[has_match], loc[has_match, , drop = FALSE]), 
        "</span>")
    bullets <- htmltools::HTML(str_c("<ul>\n", str_c("  <li>", 
        string, "</li>", collapse = "\n"), "\n</ul>"))
    htmlwidgets::createWidget("str_view", list(html = bullets), 
        sizingPolicy = htmlwidgets::sizingPolicy(knitr.figure = FALSE, 
            defaultHeight = "auto"), package = "stringr")
}


regex <- function (pattern, ignore_case = FALSE, multiline = FALSE, comments = FALSE, 
    dotall = FALSE, ...) 
{
    if (!is_bare_character(pattern)) {
        stop("Can only modify plain character vectors.", call. = FALSE)
    }
    options <- stri_opts_regex(case_insensitive = ignore_case, 
        multiline = multiline, comments = comments, dotall = dotall, 
        ...)
    structure(pattern, options = options, class = c("regex", 
        "pattern", "character"))
}


boundary <- function (type = c("character", "line_break", "sentence", "word"), 
    skip_word_none = NA, ...) 
{
    type <- match.arg(type)
    if (identical(skip_word_none, NA)) {
        skip_word_none <- type == "word"
    }
    options <- stri_opts_brkiter(type = type, skip_word_none = skip_word_none, 
        ...)
    structure(character(), options = options, class = c("boundary", 
        "pattern", "character"))
}


str_view_all <- function (string, pattern, match = NA) 
{
    if (identical(match, TRUE)) {
        string <- string[str_detect(string, pattern)]
    }
    else if (identical(match, FALSE)) {
        string <- string[!str_detect(string, pattern)]
    }
    loc <- str_locate_all(string, pattern)
    string_list <- Map(loc = loc, string = string, function(loc, 
        string) {
        if (nrow(loc) == 0) 
            return(string)
        for (i in rev(seq_len(nrow(loc)))) {
            str_sub(string, loc[i, , drop = FALSE]) <- paste0("<span class='match'>", 
                str_sub(string, loc[i, , drop = FALSE]), "</span>")
        }
        string
    })
    string <- unlist(string_list)
    bullets <- htmltools::HTML(str_c("<ul>\n", str_c("  <li>", 
        string, "</li>", collapse = "\n"), "\n</ul>"))
    htmlwidgets::createWidget("str_view", list(html = bullets), 
        sizingPolicy = htmlwidgets::sizingPolicy(knitr.figure = FALSE, 
            defaultHeight = "auto"), package = "stringr")
}


str_sort <- function (x, decreasing = FALSE, na_last = TRUE, locale = "en", 
    numeric = FALSE, ...) 
{
    stri_sort(x, decreasing = decreasing, na_last = na_last, 
        opts_collator = stri_opts_collator(locale, numeric = numeric, 
            ...))
}


fixed <- function (pattern, ignore_case = FALSE) 
{
    if (!is_bare_character(pattern)) {
        stop("Can only modify plain character vectors.", call. = FALSE)
    }
    options <- stri_opts_fixed(case_insensitive = ignore_case)
    structure(pattern, options = options, class = c("fixed", 
        "pattern", "character"))
}


str_locate_all <- function (string, pattern) 
{
    opts <- opts(pattern)
    switch(type(pattern), empty = stri_locate_all_boundaries(string, 
        omit_no_match = TRUE, opts_brkiter = opts), bound = stri_locate_all_boundaries(string, 
        omit_no_match = TRUE, opts_brkiter = opts), fixed = stri_locate_all_fixed(string, 
        pattern, omit_no_match = TRUE, opts_fixed = opts), regex = stri_locate_all_regex(string, 
        pattern, omit_no_match = TRUE, opts_regex = opts), coll = stri_locate_all_coll(string, 
        pattern, omit_no_match = TRUE, opts_collator = opts))
}


str_extract <- function (string, pattern) 
{
    switch(type(pattern), empty = stri_extract_first_boundaries(string, 
        pattern, opts_brkiter = opts(pattern)), bound = stri_extract_first_boundaries(string, 
        pattern, opts_brkiter = opts(pattern)), fixed = stri_extract_first_fixed(string, 
        pattern, opts_fixed = opts(pattern)), coll = stri_extract_first_coll(string, 
        pattern, opts_collator = opts(pattern)), regex = stri_extract_first_regex(string, 
        pattern, opts_regex = opts(pattern)))
}


str_trim <- function (string, side = c("both", "left", "right")) 
{
    side <- match.arg(side)
    switch(side, left = stri_trim_left(string), right = stri_trim_right(string), 
        both = stri_trim_both(string))
}


str_locate <- function (string, pattern) 
{
    switch(type(pattern), empty = stri_locate_first_boundaries(string, 
        opts_brkiter = opts(pattern)), bound = stri_locate_first_boundaries(string, 
        opts_brkiter = opts(pattern)), fixed = stri_locate_first_fixed(string, 
        pattern, opts_fixed = opts(pattern)), coll = stri_locate_first_coll(string, 
        pattern, opts_collator = opts(pattern)), regex = stri_locate_first_regex(string, 
        pattern, opts_regex = opts(pattern)))
}


`str_sub<-` <- function (string, start = 1L, end = -1L, value) 
{
    if (is.matrix(start)) {
        stri_sub(string, from = start) <- value
    }
    else {
        stri_sub(string, from = start, to = end) <- value
    }
    string
}


str_match <- function (string, pattern) 
{
    if (type(pattern) != "regex") {
        stop("Can only match regular expressions", call. = FALSE)
    }
    stri_match_first_regex(string, pattern, opts_regex = opts(pattern))
}


str_to_lower <- function (string, locale = "en") 
{
    stri_trans_tolower(string, locale = locale)
}


str_order <- function (x, decreasing = FALSE, na_last = TRUE, locale = "en", 
    numeric = FALSE, ...) 
{
    stri_order(x, decreasing = decreasing, na_last = na_last, 
        opts_collator = stri_opts_collator(locale, numeric = numeric, 
            ...))
}


str_sub <- function (string, start = 1L, end = -1L) 
{
    if (is.matrix(start)) {
        stri_sub(string, from = start)
    }
    else {
        stri_sub(string, from = start, to = end)
    }
}


str_subset <- function (string, pattern) 
{
    switch(type(pattern), empty = , bound = string[str_detect(string, 
        pattern)], fixed = stri_subset_fixed(string, pattern, 
        omit_na = TRUE, opts_fixed = opts(pattern)), coll = stri_subset_coll(string, 
        pattern, omit_na = TRUE, opts_collator = opts(pattern)), 
        regex = stri_subset_regex(string, pattern, omit_na = TRUE, 
            opts_regex = opts(pattern)))
}


str_match_all <- function (string, pattern) 
{
    if (type(pattern) != "regex") {
        stop("Can only match regular expressions", call. = FALSE)
    }
    stri_match_all_regex(string, pattern, omit_no_match = TRUE, 
        opts_regex = opts(pattern))
}


str_replace_all <- function (string, pattern, replacement) 
{
    if (!missing(replacement) && is.function(replacement)) {
        return(str_transform_all(string, pattern, replacement))
    }
    if (!is.null(names(pattern))) {
        vec <- FALSE
        replacement <- unname(pattern)
        pattern <- names(pattern)
    }
    else {
        vec <- TRUE
    }
    switch(type(pattern), empty = , bound = stop("Not implemented", 
        call. = FALSE), fixed = stri_replace_all_fixed(string, 
        pattern, replacement, vectorize_all = vec, opts_fixed = opts(pattern)), 
        coll = stri_replace_all_coll(string, pattern, replacement, 
            vectorize_all = vec, opts_collator = opts(pattern)), 
        regex = stri_replace_all_regex(string, pattern, fix_replacement(replacement), 
            vectorize_all = vec, opts_regex = opts(pattern)))
}


coll <- function (pattern, ignore_case = FALSE, locale = "en", ...) 
{
    if (!is_bare_character(pattern)) {
        stop("Can only modify plain character vectors.", call. = FALSE)
    }
    options <- stri_opts_collator(strength = if (ignore_case) 
        2L
    else 3L, locale = locale, ...)
    structure(pattern, options = options, class = c("coll", "pattern", 
        "character"))
}


str_conv <- function (string, encoding) 
{
    stri_conv(string, encoding, "UTF-8")
}




## Package Data

fruit <- stringr::fruit		## Sample character vectors for practicing string manipulations.

sentences <- stringr::sentences		## Sample character vectors for practicing string manipulations.

words <- stringr::words		## Sample character vectors for practicing string manipulations.



## Package Info

.skeleton_package_title = "Simple, Consistent Wrappers for Common String Operations"

.skeleton_package_version = "1.2.0"

.skeleton_package_depends = ""

.skeleton_package_imports = "stringi,magrittr"


## Internal

.skeleton_version = 5


## EOF