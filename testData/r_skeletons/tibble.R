##
## Exported symobls in package `tibble`
##

## Exported package methods

knit_print.trunc_mat <- function (x, options) 
{
    summary <- format_summary(x)
    kable <- knitr::kable(x$table, row.names = FALSE)
    extra <- format_extra(x)
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
        stopc("Expected ", nrow(.data), " rows, got ", nrow(df))
    }
    extra_vars <- intersect(names(df), names(.data))
    if (length(extra_vars) > 0) {
        stopc("Columns already in data frame: ", format_n(extra_vars))
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
    n <- length(xs)
    if (n == 0) 
        return(list())
    col_names <- names2(xs)
    missing_names <- col_names == ""
    if (any(missing_names)) {
        deparse2 <- function(x) paste(deparse(x$expr, 500L), 
            collapse = "")
        defaults <- vapply(xs[missing_names], deparse2, character(1), 
            USE.NAMES = FALSE)
        col_names[missing_names] <- defaults
    }
    output <- vector("list", n)
    names(output) <- character(n)
    for (i in seq_len(n)) {
        res <- lazyeval::lazy_eval(xs[[i]], output)
        if (!is.null(res)) {
            output[[i]] <- res
        }
        names(output)[i] <- col_names[[i]]
    }
    output
}


is.tibble <- function (x) 
{
    "tbl_df" %in% class(x)
}


trunc_mat <- function (x, n = NULL, width = NULL, n_extra = NULL) 
{
    rows <- nrow(x)
    if (is.null(n)) {
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
    structure(c(shrunk, trunc_info), class = "trunc_mat")
}


as_tibble <- function (x, ...) 
{
    UseMethod("as_tibble")
}


rownames_to_column <- function (df, var = "rowname") 
{
    stopifnot(is.data.frame(df))
    if (has_name(df, var)) 
        stopc("There is a column named ", var, " already!")
    rn <- tibble(rownames(df))
    names(rn) <- var
    attribs <- attributes(df)
    new_df <- c(rn, df)
    attribs[["names"]] <- names(new_df)
    attributes(new_df) <- attribs[names(attribs) != "row.names"]
    attr(new_df, "row.names") <- .set_row_names(nrow(df))
    new_df
}


has_rownames <- function (df) 
{
    .row_names_info(df) > 0L
}


as_data_frame <- function (x, ...) 
{
    UseMethod("as_data_frame")
}


is_vector_s3 <- function (x) 
UseMethod("is_vector_s3")


tibble_ <- function (xs) 
{
    as_tibble(lst_(xs))
}


add_row <- function (.data, ..., .before = NULL, .after = NULL) 
{
    df <- tibble(...)
    attr(df, "row.names") <- .set_row_names(max(1L, nrow(df)))
    extra_vars <- setdiff(names(df), names(.data))
    if (length(extra_vars) > 0) {
        stopc("This row would add new variables: ", format_n(extra_vars))
    }
    missing_vars <- setdiff(names(.data), names(df))
    df[missing_vars] <- lapply(.data[missing_vars], na_value)
    df <- df[names(.data)]
    pos <- pos_from_before_after(.before, .after, nrow(.data))
    if (pos <= 0L) {
        out <- rbind(df, .data)
    }
    else if (pos >= nrow(.data)) {
        out <- rbind(.data, df)
    }
    else {
        indexes <- seq_len(pos)
        out <- rbind(.data[indexes, ], df, .data[-indexes, ])
    }
    set_class(remove_rownames(out), class(.data))
}


tribble <- function (...) 
{
    dots <- list(...)
    frame_names <- character()
    i <- 1
    while (TRUE) {
        if (i > length(dots)) {
            out <- rep(list(logical()), length(frame_names))
            names(out) <- frame_names
            return(as_tibble(out))
        }
        el <- dots[[i]]
        if (!is.call(el)) 
            break
        if (!identical(el[[1]], as.name("~"))) 
            break
        if (length(el) != 2) {
            stopc("expected a column name with a single argument; e.g. '~ name'")
        }
        candidate <- el[[2]]
        if (!(is.symbol(candidate) || is.character(candidate))) {
            stopc("expected a symbol or string denoting a column name")
        }
        frame_names <- c(frame_names, as.character(el[[2]]))
        i <- i + 1
    }
    if (!length(frame_names)) {
        stopc("no column names detected in 'tribble()' call")
    }
    frame_rest <- dots[i:length(dots)]
    n_elements <- length(frame_rest)
    frame_ncol <- length(frame_names)
    if (n_elements%%frame_ncol != 0) {
        stopc(sprintf("invalid 'tribble()' specification: had %s elements and %s columns", 
            n_elements, frame_ncol))
    }
    frame_mat <- matrix(frame_rest, ncol = frame_ncol, byrow = TRUE)
    frame_col <- lapply(seq_len(ncol(frame_mat)), function(i) {
        col <- frame_mat[, i]
        if (any(vapply(col, needs_list_col, logical(1L)))) {
            col
        }
        else {
            unlist(col)
        }
    })
    names(frame_col) <- frame_names
    as_tibble(frame_col)
}


column_to_rownames <- function (df, var = "rowname") 
{
    stopifnot(is.data.frame(df))
    if (has_rownames(df)) 
        stopc("This data frame already has row names.")
    if (!has_name(df, var)) 
        stopc("This data frame has no column named ", var, ".")
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
    dots <- list(...)
    frame_names <- character()
    i <- 1
    while (TRUE) {
        if (i > length(dots)) {
            out <- rep(list(logical()), length(frame_names))
            names(out) <- frame_names
            return(as_tibble(out))
        }
        el <- dots[[i]]
        if (!is.call(el)) 
            break
        if (!identical(el[[1]], as.name("~"))) 
            break
        if (length(el) != 2) {
            stopc("expected a column name with a single argument; e.g. '~ name'")
        }
        candidate <- el[[2]]
        if (!(is.symbol(candidate) || is.character(candidate))) {
            stopc("expected a symbol or string denoting a column name")
        }
        frame_names <- c(frame_names, as.character(el[[2]]))
        i <- i + 1
    }
    if (!length(frame_names)) {
        stopc("no column names detected in 'tribble()' call")
    }
    frame_rest <- dots[i:length(dots)]
    n_elements <- length(frame_rest)
    frame_ncol <- length(frame_names)
    if (n_elements%%frame_ncol != 0) {
        stopc(sprintf("invalid 'tribble()' specification: had %s elements and %s columns", 
            n_elements, frame_ncol))
    }
    frame_mat <- matrix(frame_rest, ncol = frame_ncol, byrow = TRUE)
    frame_col <- lapply(seq_len(ncol(frame_mat)), function(i) {
        col <- frame_mat[, i]
        if (any(vapply(col, needs_list_col, logical(1L)))) {
            col
        }
        else {
            unlist(col)
        }
    })
    names(frame_col) <- frame_names
    as_tibble(frame_col)
}


enframe <- function (x, name = "name", value = "value") 
{
    if (is.null(names(x))) {
        df <- tibble(seq_along(x), x)
    }
    else {
        df <- tibble(names(x), unname(x))
    }
    names(df) <- c(name, value)
    df
}


data_frame <- function (...) 
{
    as_tibble(lst(...))
}


lst <- function (...) 
{
    lst_(lazyeval::lazy_dots(...))
}


data_frame_ <- function (xs) 
{
    as_tibble(lst_(xs))
}


tbl_sum <- function (x) 
UseMethod("tbl_sum", x)


has_name <- assertthat::has_name # re-exported from assertthat package

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
    setNames(x, new_names)
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

.skeleton_package_version = "1.2"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods,utils,lazyeval,Rcpp"


## Internal

.skeleton_version = 5


## EOF