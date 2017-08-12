##
## Exported symobls in package `tibble`
##

## Exported package methods

knit_print.trunc_mat <- function (x, options) 
{
    header <- format_header(x)
    if (length(header) > 0L) {
        header[names2(header) != ""] <- paste0(names2(header), 
            ": ", header)
        summary <- header
    }
    else {
        summary <- character()
    }
    kable <- knitr::kable(x$table, row.names = FALSE)
    extra <- format_footer(x)
    if (length(extra) > 0) {
        extra <- wrap("(", collapse(extra), ")", width = x$width)
    }
    else {
        extra <- "\n"
    }
    res <- paste(c("", "", summary, "", kable, "", extra), collapse = "\n")
    knitr::asis_output(res, cacheable = TRUE)
}


type_sum <- function (x) 
UseMethod("type_sum")


obj_sum <- function (x) 
UseMethod("obj_sum")


add_column <- function (.data, ..., .before = NULL, .after = NULL) 
{
    df <- tibble(...)
    if (ncol(df) == 0L) {
        return(.data)
    }
    if (nrow(df) != nrow(.data)) {
        if (nrow(df) == 1) {
            df <- df[rep(1L, nrow(.data)), ]
        }
        else {
            stopc("`.data` must have ", nrow(.data), pluralise_n(" row(s)", 
                nrow(.data)), ", not ", nrow(df))
        }
    }
    extra_vars <- intersect(names(df), names(.data))
    if (length(extra_vars) > 0) {
        stopc(pluralise_msg("Column(s) ", extra_vars), pluralise(" already exist[s]", 
            extra_vars))
    }
    pos <- pos_from_before_after_names(.before, .after, colnames(.data))
    if (pos <= 0L) {
        out <- cbind(df, .data)
    }
    else if (pos >= ncol(.data)) {
        out <- cbind(.data, df)
    }
    else {
        indexes <- seq_len(pos)
        out <- cbind(.data[indexes], df, .data[-indexes])
    }
    set_class(remove_rownames(out), class(.data))
}


lst_ <- function (xs) 
{
    xs <- compat_lazy_dots(xs, caller_env())
    lst(!(!(!xs)))
}


is.tibble <- function (x) 
{
    "tbl_df" %in% class(x)
}


trunc_mat <- function (x, n = NULL, width = NULL, n_extra = NULL) 
{
    rows <- nrow(x)
    if (is_null(n)) {
        if (is.na(rows) || rows > tibble_opt("print_max")) {
            n <- tibble_opt("print_min")
        }
        else {
            n <- rows
        }
    }
    n_extra <- n_extra %||% tibble_opt("max_extra_cols")
    df <- as.data.frame(head(x, n))
    width <- tibble_width(width)
    shrunk <- shrink_mat(df, width, rows, n, star = has_rownames(x))
    trunc_info <- list(width = width, rows_total = rows, rows_min = nrow(df), 
        n_extra = n_extra, summary = tbl_sum(x))
    structure(c(shrunk, trunc_info), class = c(paste0("trunc_mat_", 
        class(x)), "trunc_mat"))
}


set_tidy_names <- function (x, syntactic = FALSE, quiet = FALSE) 
{
    new_names <- tidy_names(names2(x), syntactic, quiet)
    set_names(x, new_names)
}


as_tibble <- function (x, ...) 
{
    UseMethod("as_tibble")
}


rownames_to_column <- function (df, var = "rowname") 
{
    stopifnot(is.data.frame(df))
    if (has_name(df, var)) {
        stopc("Column `", var, "` already exists")
    }
    new_df <- add_column(df, `:=`(!(!(var)), rownames(df)), .before = 1)
    new_df
}


has_rownames <- function (df) 
{
    .row_names_info(df) > 0L
}


tibble_ <- function (xs) 
{
    as_tibble(lst_(xs))
}


as_data_frame <- function (x, ...) 
{
    UseMethod("as_data_frame")
}


as.tibble <- function (x, ...) 
{
    UseMethod("as_tibble")
}


is_vector_s3 <- function (x) 
UseMethod("is_vector_s3")


add_row <- function (.data, ..., .before = NULL, .after = NULL) 
{
    if (inherits(.data, "grouped_df")) {
        stop("Can't add rows to grouped data frames", call. = FALSE)
    }
    df <- tibble(...)
    attr(df, "row.names") <- .set_row_names(max(1L, nrow(df)))
    extra_vars <- setdiff(names(df), names(.data))
    if (length(extra_vars) > 0) {
        stopc(pluralise_msg("Can't add row with new variable(s) ", 
            extra_vars))
    }
    missing_vars <- setdiff(names(.data), names(df))
    df[missing_vars] <- map(.data[missing_vars], na_value)
    df <- df[names(.data)]
    pos <- pos_from_before_after(.before, .after, nrow(.data))
    out <- rbind_at(.data, df, pos)
    set_class(remove_rownames(out), class(.data))
}


tribble <- function (...) 
{
    data <- extract_frame_data_from_dots(...)
    turn_frame_data_into_tibble(data$frame_names, data$frame_rest)
}


column_to_rownames <- function (df, var = "rowname") 
{
    stopifnot(is.data.frame(df))
    if (has_rownames(df)) {
        stopc("`df` already has row names")
    }
    if (!has_name(df, var)) {
        stopc("Column `num2` not found")
    }
    rownames(df) <- df[[var]]
    df[[var]] <- NULL
    df
}


glimpse <- function (x, width = NULL, ...) 
{
    UseMethod("glimpse")
}


frame_data <- function (...) 
{
    data <- extract_frame_data_from_dots(...)
    turn_frame_data_into_tibble(data$frame_names, data$frame_rest)
}


enframe <- function (x, name = "name", value = "value") 
{
    if (is_null(names(x))) {
        df <- tibble(seq_along(x), x)
    }
    else {
        df <- tibble(names(x), unname(x))
    }
    names(df) <- c(name, value)
    df
}


tidy_names <- function (name, syntactic = FALSE, quiet = FALSE) 
{
    name[is.na(name)] <- ""
    orig_name <- name
    name <- make_syntactic(name, syntactic)
    name <- append_pos(name)
    describe_tidying(orig_name, name, quiet)
    name
}


data_frame <- function (...) 
{
    as_tibble(lst(...))
}


lst <- function (...) 
{
    xs <- quos(..., .named = 500L)
    n <- length(xs)
    if (n == 0) {
        return(list())
    }
    col_names <- names2(xs)
    output <- list_len(n)
    names(output) <- character(n)
    for (i in seq_len(n)) {
        res <- eval_tidy(xs[[i]], output)
        if (!is_null(res)) {
            output[[i]] <- res
        }
        names(output)[i] <- col_names[[i]]
    }
    output
}


tbl_sum <- function (x) 
UseMethod("tbl_sum", x)


data_frame_ <- function (xs) 
{
    as_tibble(lst_(xs))
}


rowid_to_column <- function (df, var = "rowid") 
{
    stopifnot(is.data.frame(df))
    if (has_name(df, var)) {
        stopc("Column `", var, "` already exists")
    }
    new_df <- add_column(df, `:=`(!(!(var)), seq_len(nrow(df))), 
        .before = 1)
    new_df
}


has_name <- function (x, name) 
rlang::has_name(x, name)


deframe <- function (x) 
{
    value <- x[[2L]]
    name <- x[[1L]]
    names(value) <- name
    value
}


remove_rownames <- function (df) 
{
    stopifnot(is.data.frame(df))
    rownames(df) <- NULL
    df
}


repair_names <- function (x, prefix = "V", sep = "") 
{
    if (length(x) == 0) {
        names(x) <- character()
        return(x)
    }
    new_names <- make_unique(names2(x), prefix = prefix, sep = sep)
    set_names(x, new_names)
}


frame_matrix <- function (...) 
{
    data <- extract_frame_data_from_dots(...)
    turn_frame_data_into_frame_matrix(data$frame_names, data$frame_rest)
}


tibble <- function (...) 
{
    as_tibble(lst(...))
}


is_tibble <- function (x) 
{
    "tbl_df" %in% class(x)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Simple Data Frames"

.skeleton_package_version = "1.3.3"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods,rlang,Rcpp,utils"


## Internal

.skeleton_version = 5


## EOF