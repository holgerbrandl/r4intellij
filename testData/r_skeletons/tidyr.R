##
## Exported symobls in package `tidyr`
##

## Exported package methods

nest_ <- function (data, key_col, nest_cols = character()) 
{
    UseMethod("nest_")
}


unnest <- function (data, ..., .drop = NA, .id = NULL, .sep = NULL) 
{
    dots <- lazyeval::lazy_dots(...)
    if (length(dots) == 0) {
        list_cols <- names(data)[vapply(data, is.list, logical(1))]
        list_col_names <- lapply(list_cols, as.name)
        dots <- lazyeval::as.lazy_dots(list_col_names, env = parent.frame())
    }
    unnest_(data, dots, .drop = .drop, .id = .id, .sep = .sep)
}


full_seq <- function (x, period, tol = 1e-06) 
{
    UseMethod("full_seq")
}


separate_ <- function (data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, 
    convert = FALSE, extra = "warn", fill = "warn", ...) 
{
    UseMethod("separate_")
}


spread <- function (data, key, value, fill = NA, convert = FALSE, drop = TRUE, 
    sep = NULL) 
{
    key_col <- col_name(substitute(key))
    value_col <- col_name(substitute(value))
    spread_(data, key_col, value_col, fill = fill, convert = convert, 
        drop = drop, sep = sep)
}


complete <- Rcpp::complete # re-exported from Rcpp package

drop_na_ <- function (data, vars) 
{
    UseMethod("drop_na_")
}


fill_ <- function (data, fill_cols, .direction = c("down", "up")) 
{
    UseMethod("fill_")
}


drop_na <- function (data, ...) 
{
    relevant_cols <- unname(dplyr::select_vars(colnames(data), 
        ...))
    drop_na_(data, relevant_cols)
}


crossing <- function (...) 
{
    crossing_(tibble::lst(...))
}


complete_ <- function (data, cols, fill = list(), ...) 
{
    UseMethod("complete_")
}


separate_rows <- function (data, ..., sep = "[^[:alnum:].]+", convert = FALSE) 
{
    cols <- unname(dplyr::select_vars(names(data), ...))
    separate_rows_(data, cols, sep, convert)
}


gather <- function (data, key, value, ..., na.rm = FALSE, convert = FALSE, 
    factor_key = FALSE) 
{
    key_col <- col_name(substitute(key), "key")
    value_col <- col_name(substitute(value), "value")
    if (n_dots(...) == 0) {
        gather_cols <- setdiff(colnames(data), c(key_col, value_col))
    }
    else {
        gather_cols <- unname(dplyr::select_vars(colnames(data), 
            ...))
    }
    gather_(data, key_col, value_col, gather_cols, na.rm = na.rm, 
        convert = convert, factor_key = factor_key)
}


unite_ <- function (data, col, from, sep = "_", remove = TRUE) 
{
    UseMethod("unite_")
}


extract <- magrittr::extract # re-exported from magrittr package

crossing_ <- function (x) 
{
    stopifnot(is.list(x))
    x <- drop_empty(x)
    is_atomic <- vapply(x, is.atomic, logical(1))
    is_df <- vapply(x, is.data.frame, logical(1))
    if (any(!is_df & !is_atomic)) {
        bad <- names(x)[!is_df & !is_atomic]
        stop("Each element must be either an atomic vector or a data frame\n.", 
            "Problems: ", paste(bad, collapse = ", "), ".\n", 
            call. = FALSE)
    }
    col_df <- lapply(x[is_atomic], function(x) data_frame(x = ulevels(x)))
    col_df <- Map(setNames, col_df, names(x)[is_atomic])
    x[is_atomic] <- col_df
    Reduce(cross_df, x)
}


fill <- function (data, ..., .direction = c("down", "up")) 
{
    fill_cols <- unname(dplyr::select_vars(colnames(data), ...))
    fill_(data, fill_cols, .direction = .direction)
}


nesting_ <- function (x) 
{
    stopifnot(is.list(x))
    x <- drop_empty(x)
    df <- as_data_frame(x)
    df <- dplyr::distinct(df)
    df[do.call(order, df), , drop = FALSE]
}


separate <- function (data, col, into, sep = "[^[:alnum:]]+", remove = TRUE, 
    convert = FALSE, extra = "warn", fill = "warn", ...) 
{
    col <- col_name(substitute(col))
    separate_(data, col, into, sep = sep, remove = remove, convert = convert, 
        extra = extra, fill = fill, ...)
}


spread_ <- function (data, key_col, value_col, fill = NA, convert = FALSE, 
    drop = TRUE, sep = NULL) 
{
    if (!(key_col %in% names(data))) {
        stop("Key column '", key_col, "' does not exist in input.", 
            call. = FALSE)
    }
    if (!(value_col %in% names(data))) {
        stop("Value column '", value_col, "' does not exist in input.", 
            call. = FALSE)
    }
    UseMethod("spread_")
}


nest <- function (data, ..., .key = data) 
{
    key_col <- col_name(substitute(.key))
    nest_cols <- unname(dplyr::select_vars(colnames(data), ...))
    nest_(data, key_col, nest_cols)
}


unnest_ <- function (data, unnest_cols, .drop = NA, .id = NULL, .sep = NULL) 
{
    UseMethod("unnest_")
}


extract_ <- function (data, col, into, regex = "([[:alnum:]]+)", remove = TRUE, 
    convert = FALSE, ...) 
{
    UseMethod("extract_")
}


gather_ <- function (data, key_col, value_col, gather_cols, na.rm = FALSE, 
    convert = FALSE, factor_key = FALSE) 
{
    UseMethod("gather_")
}


extract_numeric <- function (x) 
{
    message("extract_numeric() is deprecated: please use readr::parse_number() instead")
    as.numeric(gsub("[^0-9.-]+", "", as.character(x)))
}


expand <- function (data, ...) 
{
    dots <- lazyeval::lazy_dots(...)
    expand_(data, dots)
}


unite <- function (data, col, ..., sep = "_", remove = TRUE) 
{
    col <- col_name(substitute(col))
    from <- dplyr::select_vars(colnames(data), ...)
    unite_(data, col, from, sep = sep, remove = remove)
}


nesting <- function (...) 
{
    nesting_(tibble::lst(...))
}


`%>%` <- magrittr::`%>%` # re-exported from magrittr package

replace_na <- function (data, replace = list(), ...) 
{
    UseMethod("replace_na")
}


separate_rows_ <- function (data, cols, sep = "[^[:alnum:].]+", convert = FALSE) 
{
    UseMethod("separate_rows_")
}


expand_ <- function (data, dots, ...) 
{
    UseMethod("expand_")
}




## Package Data

population <- tidyr::population		## World Health Organization TB data

smiths <- tidyr::smiths		## Some data about the Smith family.

table1 <- tidyr::table1		## Example tabular representations

table2 <- tidyr::table2		## Example tabular representations

table3 <- tidyr::table3		## Example tabular representations

table4a <- tidyr::table4a		## Example tabular representations

table4b <- tidyr::table4b		## Example tabular representations

table5 <- tidyr::table5		## Example tabular representations

who <- tidyr::who		## World Health Organization TB data



## Package Info

.skeleton_package_title = "Easily Tidy Data with 'spread()' and 'gather()' Functions"

.skeleton_package_version = "0.6.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "tibble,dplyr,stringi,lazyeval,magrittr,Rcpp"


## Internal

.skeleton_version = 5


## EOF