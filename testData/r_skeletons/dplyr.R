##
## Exported symobls in package `dplyr`
##

## Exported package methods

sql_set_op <- function (con, x, y, method) 
{
    UseMethod("sql_set_op")
}


`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

first <- function (x, order_by = NULL, default = default_missing(x)) 
{
    nth(x, 1L, order_by = order_by, default = default)
}


eval_tbls <- function (tbls, op) 
{
    lapply(tbls, function(x) as.data.frame(op(x)))
}


partial_eval <- function (call, tbl = NULL, env = parent.frame()) 
{
    if (is.atomic(call)) 
        return(call)
    if (inherits(call, "lazy_dots")) {
        lapply(call, function(l) partial_eval(l$expr, tbl, l$env))
    }
    else if (is.list(call)) {
        lapply(call, partial_eval, tbl = tbl, env = env)
    }
    else if (is.symbol(call)) {
        name <- as.character(call)
        if (!is.null(tbl) && name %in% tbl_vars(tbl)) {
            call
        }
        else if (exists(name, env)) {
            eval(call, env)
        }
        else {
            call
        }
    }
    else if (is.call(call)) {
        name <- as.character(call[[1]])
        if (name == "local") {
            eval(call[[2]], env)
        }
        else if (name %in% c("$", "[[", "[")) {
            eval(call, env)
        }
        else if (name == "remote") {
            call[[2]]
        }
        else {
            call[-1] <- lapply(call[-1], partial_eval, tbl = tbl, 
                env = env)
            call
        }
    }
    else {
        stop("Unknown input type: ", class(call), call. = FALSE)
    }
}


row_number <- function (x) 
rank(x, ties.method = "first", na.last = "keep")


named_commas <- function (...) 
{
    x <- c(...)
    if (is.null(names(x))) {
        paste0(x, collapse = ", ")
    }
    else {
        paste0(names(x), " = ", x, collapse = ", ")
    }
}


sql_infix <- function (f) 
{
    assert_that(is.string(f))
    f <- toupper(f)
    function(x, y) {
        build_sql(x, " ", sql(f), " ", y)
    }
}


lahman_sqlite <- function (path = NULL) 
{
    path <- db_location(path, "lahman.sqlite")
    copy_lahman(src_sqlite(path = path, create = TRUE))
}


mutate_at <- function (.tbl, .cols, .funs, ...) 
{
    cols <- select_colwise_names(.tbl, .cols)
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, cols)
    mutate_(.tbl, .dots = vars)
}


do_ <- function (.data, ..., .dots) 
{
    UseMethod("do_")
}


rename_vars_ <- function (vars, args) 
{
    if (any(names2(args) == "")) {
        stop("All arguments to `rename()` must be named.", call. = FALSE)
    }
    args <- lazyeval::as.lazy_dots(args)
    is_name <- vapply(args, function(x) is.name(x$expr), logical(1))
    if (!all(is_name)) {
        n <- sum(!is_name)
        bad <- paste0("`", names(args)[!is_name], "`", collapse = ", ")
        stop("Arguments to `rename()` must be unquoted variable names.\n", 
            sprintf(ngettext(n, "Argument %s is not.", "Arguments %s are not."), 
                bad), call. = FALSE)
    }
    old_vars <- vapply(args, function(x) as.character(x$expr), 
        character(1))
    new_vars <- names(args)
    unknown_vars <- setdiff(old_vars, vars)
    if (length(unknown_vars) > 0) {
        stop("Unknown variables: ", paste0(unknown_vars, collapse = ", "), 
            ".", call. = FALSE)
    }
    select <- setNames(vars, vars)
    names(select)[match(old_vars, vars)] <- new_vars
    select
}


slice <- function (.data, ...) 
{
    slice_(.data, .dots = lazyeval::lazy_dots(...))
}


explain <- function (x, ...) 
{
    UseMethod("explain")
}


sql_escape_ident <- function (con, x) 
UseMethod("sql_escape_ident")


semi_join_query <- function (x, y, anti = FALSE, by = NULL) 
{
    structure(list(x = x, y = y, anti = anti, by = by), class = c("semi_join_query", 
        "query"))
}


add_rownames <- function (df, var = "rowname") 
{
    warning("Deprecated, use tibble::rownames_to_column() instead.", 
        call. = FALSE)
    stopifnot(is.data.frame(df))
    rn <- as_data_frame(setNames(list(rownames(df)), var))
    rownames(df) <- NULL
    bind_cols(rn, df)
}


db_query_rows <- function (con, sql, ...) 
{
    UseMethod("db_query_rows")
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

matches <- function (match, ignore.case = TRUE, vars = current_vars()) 
{
    stopifnot(is.string(match), nchar(match) > 0)
    grep_vars(match, vars, ignore.case = ignore.case)
}


summarize_each_ <- function (tbl, funs, vars) 
{
    if (is.character(funs)) {
        funs <- funs_(funs)
    }
    vars <- colwise_(tbl, funs, vars)
    summarise_(tbl, .dots = vars)
}


everything <- function (vars = current_vars()) 
{
    seq_along(vars)
}


src_tbls <- function (x) 
{
    UseMethod("src_tbls")
}


between <- function (x, left, right) 
{
    .Call("dplyr_between", PACKAGE = "dplyr", x, left, right)
}


test_load <- function (df, name = random_table_name(), srcs = test_srcs$get(), 
    ignore = character()) 
{
    stopifnot(is.data.frame(df))
    stopifnot(is.character(ignore))
    srcs <- srcs[setdiff(names(srcs), ignore)]
    lapply(srcs, copy_to, df, name = name)
}


tbl_cube <- function (dimensions, measures) 
{
    if (!is.list(dimensions) || any_apply(dimensions, Negate(is.atomic)) || 
        is.null(names(dimensions))) {
        stop("Dimensions must be a named list of vectors", call. = FALSE)
    }
    if (!is.list(measures) || any_apply(measures, Negate(is.array)) || 
        is.null(names(measures))) {
        stop("Measures must be a named list of arrays", call. = FALSE)
    }
    dims <- vapply(dimensions, length, integer(1), USE.NAMES = FALSE)
    dims_ok <- vapply(measures, function(x) identical(unname(dim(x)), 
        dims), logical(1))
    if (any(!dims_ok)) {
        bad <- names(measures)[!dims_ok]
        stop("Measures ", paste0(bad, collapse = ", "), " don't have correct ", 
            "dimensions (", paste0(dims, collapse = " x "), ")", 
            call. = FALSE)
    }
    structure(list(dims = dimensions, mets = measures), class = "tbl_cube")
}


mutate <- function (.data, ...) 
{
    mutate_(.data, .dots = lazyeval::lazy_dots(...))
}


n_groups <- function (x) 
UseMethod("n_groups")


src_sqlite <- function (path, create = FALSE) 
{
    if (!requireNamespace("RSQLite", quietly = TRUE)) {
        stop("RSQLite package required to connect to sqlite db", 
            call. = FALSE)
    }
    if (!create && !file.exists(path)) {
        stop("Path does not exist and create = FALSE", call. = FALSE)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    RSQLite::initExtension(con)
    src_sql("sqlite", con, path = path)
}


mutate_ <- function (.data, ..., .dots) 
{
    UseMethod("mutate_")
}


coalesce <- function (x, ...) 
{
    values <- list(...)
    for (i in seq_along(values)) {
        x <- replace_with(x, is.na(x), values[[i]], paste0("Vector ", 
            i))
    }
    x
}


group_by_prepare <- function (.data, ..., .dots, add = FALSE) 
{
    new_groups <- lazyeval::all_dots(.dots, ...)
    new_groups <- resolve_vars(new_groups, tbl_vars(.data))
    is_name <- vapply(new_groups, function(x) is.name(x$expr), 
        logical(1))
    has_name <- names2(new_groups) != ""
    needs_mutate <- has_name | !is_name
    if (any(needs_mutate)) {
        .data <- mutate_(.data, .dots = new_groups[needs_mutate])
    }
    new_groups <- lazyeval::auto_name(new_groups)
    groups <- lapply(names(new_groups), as.name)
    if (add) {
        groups <- c(groups(.data), groups)
    }
    groups <- groups[!duplicated(groups)]
    list(data = .data, groups = groups)
}


percent_rank <- function (x) 
{
    (min_rank(x) - 1)/(sum(!is.na(x)) - 1)
}


grouped_df <- function (data, vars, drop = TRUE) 
{
    if (length(vars) == 0) {
        return(tbl_df(data))
    }
    assert_that(is.data.frame(data), is.list(vars), all(sapply(vars, 
        is.name)), is.flag(drop))
    grouped_df_impl(data, unname(vars), drop)
}


rename_vars <- function (vars, ...) 
{
    rename_vars_(vars, lazyeval::lazy_dots(...))
}


summarize_each <- function (tbl, funs, ...) 
{
    summarise_each_(tbl, funs, lazyeval::lazy_dots(...))
}


cumall <- function (x) 
{
    .Call("dplyr_cumall", PACKAGE = "dplyr", x)
}


count_ <- function (x, vars, wt = NULL, sort = FALSE) 
{
    grouped <- group_by_(x, .dots = vars, add = TRUE)
    tally_(grouped, wt = wt, sort = sort)
}


base_scalar <- "<environment>"

sql_translator <- function (..., .funs = list(), .parent = new.env(parent = emptyenv())) 
{
    funs <- c(list(...), .funs)
    if (length(funs) == 0) 
        return(.parent)
    list2env(funs, copy_env(.parent))
}


op_double <- function (name, x, y, args = list()) 
{
    structure(list(name = name, x = x, y = y, args = args), class = c(paste0("op_", 
        name), "op_double", "op"))
}


sql_select <- function (con, select, from, where = NULL, group_by = NULL, having = NULL, 
    order_by = NULL, limit = NULL, distinct = FALSE, ...) 
{
    UseMethod("sql_select")
}


dim_desc <- function (x) 
{
    d <- dim(x)
    d2 <- big_mark(d)
    d2[is.na(d)] <- "??"
    paste0("[", paste0(d2, collapse = " x "), "]")
}


lead <- function (x, n = 1L, default = NA, order_by = NULL, ...) 
{
    if (!is.null(order_by)) {
        return(with_order(order_by, lead, x, n = n, default = default))
    }
    if (n == 0) 
        return(x)
    if (n < 0 || length(n) > 1) 
        stop("n must be a single positive integer")
    xlen <- length(x)
    n <- pmin(n, xlen)
    out <- c(x[-seq_len(n)], rep(default, n))
    attributes(out) <- attributes(x)
    out
}


db_insert_into <- function (con, table, values, ...) 
{
    UseMethod("db_insert_into")
}


with_order <- function (order_by, fun, x, ...) 
{
    ord <- order(order_by)
    undo <- match(seq_along(order_by), ord)
    out <- fun(x[ord], ...)
    out[undo]
}


join_query <- function (x, y, type = "inner", by = NULL, suffix = c(".x", ".y")) 
{
    structure(list(x = x, y = y, type = type, by = by, suffix = suffix), 
        class = c("join_query", "query"))
}


src_local <- function (tbl, pkg = NULL, env = NULL) 
{
    if (!xor(is.null(pkg), is.null(env))) {
        stop("Must supply exactly one of pkg and env", call. = FALSE)
    }
    if (!is.null(pkg)) {
        env <- getNamespaceInfo(pkg, "lazydata")
        name <- paste0("<package: ", pkg, ">")
    }
    else {
        name <- utils::capture.output(print(env))
    }
    structure(list(tbl_f = match.fun(tbl), name = name, env = env), 
        class = c("src_local", "src"))
}


base_agg <- "<environment>"

anti_join <- function (x, y, by = NULL, copy = FALSE, ...) 
{
    UseMethod("anti_join")
}


changes <- function (x, y) 
{
    x <- location(x)
    y <- location(y)
    if (x$df == y$df) {
        cat("<identical>\n")
        return(invisible())
    }
    vars <- match_up(x$vars, y$vars)
    attr <- match_up(x$attr, y$attr)
    width <- max(nchar(rownames(vars)), nchar(rownames(attr)))
    if (nrow(vars) > 0) 
        rownames(vars) <- format(rownames(vars), width = width)
    if (nrow(attr) > 0) 
        rownames(attr) <- format(rownames(attr), width = width)
    if (nrow(vars) > 0) {
        cat("Changed variables:\n")
        print(vars, quote = FALSE)
    }
    if (nrow(vars) > 0 && nrow(attr)) 
        cat("\n")
    if (nrow(attr) > 0) {
        cat("Changed attributes:\n")
        print(attr, quote = FALSE)
    }
}


transmute_ <- function (.data, ..., .dots) 
{
    UseMethod("transmute_")
}


src_memdb <- function () 
{
    cache_computation("src_memdb", src_sqlite(":memory:", TRUE))
}


summarise_all <- function (.tbl, .funs, ...) 
{
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, list())
    summarise_(.tbl, .dots = vars)
}


summarize <- function (.data, ...) 
{
    summarise_(.data, .dots = lazyeval::lazy_dots(...))
}


filter <- function (.data, ...) 
{
    filter_(.data, .dots = lazyeval::lazy_dots(...))
}


compare_tbls <- function (tbls, op, ref = NULL, compare = equal_data_frame, ...) 
{
    if (length(tbls) < 2 && is.null(ref)) {
        testthat::skip("Need at least two srcs to compare")
    }
    if (!requireNamespace("testthat", quietly = TRUE)) {
        stop("Please install the testthat package", call. = FALSE)
    }
    results <- eval_tbls(tbls, op)
    if (is.null(ref)) {
        ref <- results[[1]]
        ref_name <- names(results)[1]
        rest <- results[-1]
    }
    else {
        rest <- results
        ref_name <- "supplied comparison"
    }
    for (i in seq_along(rest)) {
        ok <- compare(ref, rest[[i]], ...)
        msg <- paste0(names(rest)[[i]], " not equal to ", ref_name, 
            "\n", attr(ok, "comment"))
        testthat::expect_true(ok, info = msg)
    }
    invisible(TRUE)
}


lahman_postgres <- function (dbname = "lahman", ...) 
{
    copy_lahman(src_postgres(dbname, ...))
}


cummean <- function (x) 
{
    .Call("dplyr_cummean", PACKAGE = "dplyr", x)
}


lag <- function (x, n = 1L, default = NA, order_by = NULL, ...) 
{
    if (!is.null(order_by)) {
        return(with_order(order_by, lag, x, n = n, default = default))
    }
    if (n == 0) 
        return(x)
    if (n < 0 || length(n) > 1) 
        stop("n must be a single positive integer")
    xlen <- length(x)
    n <- pmin(n, xlen)
    out <- c(rep(default, n), x[seq_len(xlen - n)])
    attributes(out) <- attributes(x)
    out
}


set_op_query <- function (x, y, type = type) 
{
    structure(list(x = x, y = y, type = type), class = c("set_op_query", 
        "query"))
}


translate_sql <- function (..., con = NULL, vars = character(), vars_group = NULL, 
    vars_order = NULL, window = TRUE) 
{
    dots <- lazyeval::lazy_dots(...)
    translate_sql_(dots, con = con, vars = vars, vars_group = vars_group, 
        vars_order = vars_order, window = window)
}


collapse <- function (x, ...) 
{
    UseMethod("collapse")
}


data_frame_ <- tibble::data_frame_ # re-exported from tibble package

src_postgres <- function (dbname = NULL, host = NULL, port = NULL, user = NULL, 
    password = NULL, ...) 
{
    if (!requireNamespace("RPostgreSQL", quietly = TRUE)) {
        stop("RPostgreSQL package required to connect to postgres db", 
            call. = FALSE)
    }
    user <- user %||% if (in_travis()) 
        "postgres"
    else ""
    con <- dbConnect(RPostgreSQL::PostgreSQL(), host = host %||% 
        "", dbname = dbname %||% "", user = user, password = password %||% 
        "", port = port %||% "", ...)
    info <- dbGetInfo(con)
    src_sql("postgres", con, info = info, disco = db_disconnector(con, 
        "postgres"))
}


contains <- function (match, ignore.case = TRUE, vars = current_vars()) 
{
    stopifnot(is.string(match), nchar(match) > 0)
    if (ignore.case) {
        vars <- tolower(vars)
        match <- tolower(match)
    }
    grep_vars(match, vars, fixed = TRUE)
}


transmute <- function (.data, ...) 
{
    transmute_(.data, .dots = lazyeval::lazy_dots(...))
}


tbl_sql <- function (subclass, src, from, ..., vars = attr(from, "vars")) 
{
    make_tbl(c(subclass, "sql", "lazy"), src = src, ops = op_base_remote(src, 
        from, vars))
}


select_vars_ <- function (vars, args, include = character(), exclude = character()) 
{
    if (length(args) == 0) {
        vars <- setdiff(include, exclude)
        return(setNames(vars, vars))
    }
    set_current_vars(vars)
    on.exit(reset_current_vars(), add = TRUE)
    args <- lazyeval::as.lazy_dots(args)
    names_list <- setNames(as.list(seq_along(vars)), vars)
    ind_list <- lazyeval::lazy_eval(args, names_list)
    names(ind_list) <- names2(args)
    is_numeric <- vapply(ind_list, is.numeric, logical(1))
    if (any(!is_numeric)) {
        bad_inputs <- lapply(args[!is_numeric], `[[`, "expr")
        labels <- vapply(bad_inputs, deparse_trunc, character(1))
        stop("All select() inputs must resolve to integer column positions.\n", 
            "The following do not:\n", paste("* ", labels, collapse = "\n"), 
            call. = FALSE)
    }
    incl <- combine_vars(vars, ind_list)
    sel <- setNames(vars[incl], names(incl))
    sel <- c(setdiff2(include, sel), sel)
    sel <- setdiff2(sel, exclude)
    if (length(sel) == 0) {
        names(sel) <- sel
    }
    else {
        unnamed <- names2(sel) == ""
        names(sel)[unnamed] <- sel[unnamed]
    }
    sel
}


vars <- function (...) 
{
    structure(lazyeval::lazy_dots(...), class = c("col_list", 
        "lazy_dots"))
}


arrange <- function (.data, ...) 
{
    arrange_(.data, .dots = lazyeval::lazy_dots(...))
}


left_join <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
    ...) 
{
    UseMethod("left_join")
}


progress_estimated <- function (n, min_time = 0) 
{
    Progress$new(n, min_time = min_time)
}


groups <- function (x) 
{
    UseMethod("groups")
}


failwith <- function (default = NULL, f, quiet = FALSE) 
{
    function(...) {
        out <- default
        try(out <- f(...), silent = quiet)
        out
    }
}


copy_to <- function (dest, df, name = deparse(substitute(df)), ...) 
{
    UseMethod("copy_to")
}


`.__T__$:base` <- Rcpp::`.__T__$:base` # re-exported from Rcpp package

base_no_win <- "<environment>"

is.grouped_df <- function (x) 
inherits(x, "grouped_df")


nycflights13_sqlite <- function (path = NULL) 
{
    cache_computation("nycflights_sqlite", {
        path <- db_location(path, "nycflights13.sqlite")
        message("Caching nycflights db at ", path)
        src <- src_sqlite(path, create = TRUE)
        copy_nycflights13(src)
    })
}


db_analyze <- function (con, table, ...) 
UseMethod("db_analyze")


db_commit <- function (con, ...) 
UseMethod("db_commit")


sample_n <- function (tbl, size, replace = FALSE, weight = NULL, .env = parent.frame()) 
{
    UseMethod("sample_n")
}


compute <- function (x, name = random_table_name(), ...) 
{
    UseMethod("compute")
}


src_sql <- function (subclass, con, ...) 
{
    subclass <- paste0("src_", subclass)
    structure(list(con = con, ...), class = c(subclass, "src_sql", 
        "src"))
}


summarise_ <- function (.data, ..., .dots) 
{
    UseMethod("summarise_")
}


nth <- function (x, n, order_by = NULL, default = default_missing(x)) 
{
    stopifnot(length(n) == 1, is.numeric(n))
    n <- trunc(n)
    if (n == 0 || n > length(x) || n < -length(x)) {
        return(default)
    }
    if (n < 0) {
        n <- length(x) + n + 1
    }
    if (is.null(order_by)) {
        x[[n]]
    }
    else {
        x[[order(order_by)[n]]]
    }
}


current_vars <- function () 
cur_vars_env$selected


group_indices <- function (.data, ...) 
{
    group_indices_(.data, .dots = lazyeval::lazy_dots(...))
}


add_op_single <- function (name, .data, dots = list(), args = list()) 
{
    .data$ops <- op_single(name, x = .data$ops, dots = dots, 
        args = args)
    .data
}


test_register_src <- function (name, src) 
{
    message("Registering testing src: ", name, " ", appendLF = FALSE)
    tryCatch({
        test_srcs$add(name, src)
        message("OK")
    }, error = function(e) message(conditionMessage(e)))
}


intersect <- function (x, y, ...) 
UseMethod("intersect")


cumany <- function (x) 
{
    .Call("dplyr_cumany", PACKAGE = "dplyr", x)
}


summarize_if <- function (.tbl, .predicate, .funs, ...) 
{
    if (inherits(.tbl, "tbl_lazy")) {
        stop("Conditional colwise operations currently require local sources", 
            call. = FALSE)
    }
    cols <- probe_colwise_names(.tbl, .predicate)
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, cols)
    summarise_(.tbl, .dots = vars)
}


common_by <- function (by = NULL, x, y) 
{
    if (is.list(by)) 
        return(by)
    if (!is.null(by)) {
        by <- by[!duplicated(by)]
        x <- names(by) %||% by
        y <- unname(by)
        x[x == ""] <- y[x == ""]
        return(list(x = x, y = y))
    }
    by <- intersect(tbl_vars(x), tbl_vars(y))
    if (length(by) == 0) {
        stop("No common variables. Please specify `by` param.", 
            call. = FALSE)
    }
    message("Joining, by = ", utils::capture.output(dput(by)))
    list(x = by, y = by)
}


summarise <- function (.data, ...) 
{
    summarise_(.data, .dots = lazyeval::lazy_dots(...))
}


funs <- function (...) 
funs_(lazyeval::lazy_dots(...))


ends_with <- function (match, ignore.case = TRUE, vars = current_vars()) 
{
    stopifnot(is.string(match), !is.na(match), nchar(match) > 
        0)
    if (ignore.case) 
        match <- tolower(match)
    n <- nchar(match)
    if (ignore.case) 
        vars <- tolower(vars)
    length <- nchar(vars)
    which_vars(match, substr(vars, pmax(1, length - n + 1), length))
}


trunc_mat <- tibble::trunc_mat # re-exported from tibble package

has_nycflights13 <- function (type = c("sqlite", "postgresql"), ...) 
{
    if (!requireNamespace("nycflights13", quietly = TRUE)) 
        return(FALSE)
    type <- match.arg(type)
    succeeds(switch(type, sqlite = nycflights13_sqlite(...), 
        quiet = TRUE, postgres = nycflights13_postgres(...), 
        quiet = TRUE))
}


select_ <- function (.data, ..., .dots) 
{
    UseMethod("select_")
}


sql_quote <- function (x, quote) 
{
    y <- gsub(quote, paste0(quote, quote), x, fixed = TRUE)
    y <- paste0(quote, y, quote)
    y[is.na(x)] <- "NULL"
    names(y) <- names(x)
    y
}


escape <- function (x, parens = NA, collapse = " ", con = NULL) 
{
    UseMethod("escape")
}


sql_prefix <- function (f, n = NULL) 
{
    assert_that(is.string(f))
    f <- toupper(f)
    function(..., na.rm) {
        if (!missing(na.rm)) {
            message("na.rm not needed in SQL: NULL are always dropped", 
                call. = FALSE)
        }
        args <- list(...)
        if (!is.null(n) && length(args) != n) {
            stop("Invalid number of args to SQL ", f, ". Expecting ", 
                n, call. = FALSE)
        }
        if (any(names2(args) != "")) {
            warning("Named arguments ignored for SQL ", f, call. = FALSE)
        }
        build_sql(sql(f), args)
    }
}


has_lahman <- function (type, ...) 
{
    if (!requireNamespace("Lahman", quietly = TRUE)) 
        return(FALSE)
    if (missing(type)) 
        return(TRUE)
    succeeds(lahman(type, ...), quiet = TRUE)
}


select <- function (.data, ...) 
{
    select_(.data, .dots = lazyeval::lazy_dots(...))
}


near <- function (x, y, tol = .Machine$double.eps^0.5) 
{
    abs(x - y) < tol
}


summarize_all <- function (.tbl, .funs, ...) 
{
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, list())
    summarise_(.tbl, .dots = vars)
}


filter_ <- function (.data, ..., .dots) 
{
    UseMethod("filter_")
}


select_if <- function (.data, .predicate, ...) 
{
    if (inherits(.data, "tbl_lazy")) {
        stop("Selection with predicate currently require local sources", 
            call. = FALSE)
    }
    vars <- probe_colwise_names(.data, .predicate, ...)
    vars <- ensure_grouped_vars(vars, .data, notify = FALSE)
    select_(.data, .dots = vars)
}


slice_ <- function (.data, ..., .dots) 
{
    UseMethod("slice_")
}


sql_not_supported <- function (f) 
{
    assert_that(is.string(f))
    f <- toupper(f)
    function(...) {
        stop(f, " is not available in this SQL variant", call. = FALSE)
    }
}


op_vars <- function (op) 
UseMethod("op_vars")


db_create_table <- function (con, table, types, temporary = FALSE, ...) 
{
    UseMethod("db_create_table")
}


op_grps <- function (op) 
UseMethod("op_grps")


summarise_at <- function (.tbl, .cols, .funs, ...) 
{
    cols <- select_colwise_names(.tbl, .cols)
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, cols)
    summarise_(.tbl, .dots = vars)
}


lahman_dt <- function () 
{
    dtplyr::src_dt("Lahman")
}


memdb_frame <- function (..., .name = random_table_name()) 
{
    copy_to(src_memdb(), data_frame(...), name = .name)
}


is.ident <- function (x) 
inherits(x, "ident")


sql_subquery <- function (con, from, name = random_table_name(), ...) 
{
    UseMethod("sql_subquery")
}


db_query_fields <- function (con, sql, ...) 
{
    UseMethod("db_query_fields")
}


mutate_all <- function (.tbl, .funs, ...) 
{
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, list())
    mutate_(.tbl, .dots = vars)
}


op_single <- function (name, x, dots = list(), args = list()) 
{
    structure(list(name = name, x = x, dots = dots, args = args), 
        class = c(paste0("op_", name), "op_single", "op"))
}


summarise_each_q <- function (...) 
{
    .Deprecated("summarise_all")
    summarise_each_(...)
}


select_query <- function (from, select = sql("*"), where = character(), group_by = character(), 
    having = character(), order_by = character(), limit = NULL, 
    distinct = FALSE) 
{
    stopifnot(is.character(select))
    stopifnot(is.character(where))
    stopifnot(is.character(group_by))
    stopifnot(is.character(having))
    stopifnot(is.character(order_by))
    stopifnot(is.null(limit) || (is.numeric(limit) && length(limit) == 
        1L))
    stopifnot(is.logical(distinct), length(distinct) == 1L)
    structure(list(from = from, select = select, where = where, 
        group_by = group_by, having = having, order_by = order_by, 
        distinct = distinct, limit = limit), class = c("select_query", 
        "query"))
}


sql_semi_join <- function (con, x, y, anti = FALSE, by = NULL, ...) 
{
    UseMethod("sql_semi_join")
}


select_vars <- function (vars, ..., include = character(), exclude = character()) 
{
    args <- lazyeval::lazy_dots(...)
    select_vars_(vars, args, include = include, exclude = exclude)
}


tbl_vars <- function (x) 
UseMethod("tbl_vars")


sql_variant <- function (scalar = sql_translator(), aggregate = sql_translator(), 
    window = sql_translator()) 
{
    stopifnot(is.environment(scalar))
    stopifnot(is.environment(aggregate))
    stopifnot(is.environment(window))
    structure(list(scalar = scalar, aggregate = aggregate, window = window), 
        class = "sql_variant")
}


build_sql <- function (..., .env = parent.frame(), con = NULL) 
{
    escape_expr <- function(x) {
        if (is.character(x)) 
            return(x)
        val <- eval(x, .env)
        if (is.null(val)) 
            return("")
        escape(val, con = con)
    }
    pieces <- vapply(dots(...), escape_expr, character(1))
    sql(paste0(pieces, collapse = ""))
}


distinct <- function (.data, ..., .keep_all = FALSE) 
{
    distinct_(.data, .dots = lazyeval::lazy_dots(...), .keep_all = .keep_all)
}


rbind_list <- function (...) 
{
    warning("`rbind_list()` is deprecated. Please use `bind_rows()` instead.", 
        call. = FALSE)
    rbind_list__impl(environment())
}


same_src <- function (x, y) 
{
    UseMethod("same_src")
}


sql_vector <- function (x, parens = NA, collapse = " ", con = NULL) 
{
    if (is.na(parens)) {
        parens <- length(x) > 1L
    }
    x <- names_to_as(x, con = con)
    x <- paste(x, collapse = collapse)
    if (parens) 
        x <- paste0("(", x, ")")
    sql(x)
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

data_frame <- tibble::data_frame # re-exported from tibble package

recode_factor <- function (.x, ..., .default = NULL, .missing = NULL, .ordered = FALSE) 
{
    recoded <- recode(.x, ..., .default = .default, .missing = .missing)
    all_levels <- unique(c(..., recode_default(.x, .default, 
        recoded), .missing))
    recoded_levels <- if (is.factor(recoded)) 
        levels(recoded)
    else unique(recoded)
    levels <- intersect(all_levels, recoded_levels)
    factor(recoded, levels, ordered = .ordered)
}


one_of <- function (..., vars = current_vars()) 
{
    keep <- c(...)
    if (!is.character(keep)) {
        stop("`c(...)` must be a character vector", call. = FALSE)
    }
    if (!all(keep %in% vars)) {
        bad <- setdiff(keep, vars)
        warning("Unknown variables: ", paste0("`", bad, "`", 
            collapse = ", "))
    }
    match_vars(keep, vars)
}


n <- function () 
{
    stop("This function should not be called directly")
}


lahman_df <- function () 
{
    src_df("Lahman")
}


frame_data <- tibble::frame_data # re-exported from tibble package

sql <- function (...) 
{
    x <- c(...)
    if (length(x) == 0) {
        structure(character(), class = c("sql", "character"))
    }
    else {
        stopifnot(is.character(x))
        structure(x, class = c("sql", "character"))
    }
}


last <- function (x, order_by = NULL, default = default_missing(x)) 
{
    nth(x, -1L, order_by = order_by, default = default)
}


show_query <- function (x) 
{
    message("<SQL>\n", sql_render(x))
}


starts_with <- function (match, ignore.case = TRUE, vars = current_vars()) 
{
    stopifnot(is.string(match), !is.na(match), nchar(match) > 
        0)
    if (ignore.case) 
        match <- tolower(match)
    n <- nchar(match)
    if (ignore.case) 
        vars <- tolower(vars)
    which_vars(match, substr(vars, 1, n))
}


as_data_frame <- tibble::as_data_frame # re-exported from tibble package

setdiff <- function (x, y, ...) 
UseMethod("setdiff")


db_list_tables <- function (con) 
UseMethod("db_list_tables")


mutate_each <- function (tbl, funs, ...) 
{
    if (is.character(funs)) {
        funs <- funs_(funs)
    }
    mutate_each_(tbl, funs, lazyeval::lazy_dots(...))
}


do <- function (.data, ...) 
{
    do_(.data, .dots = lazyeval::lazy_dots(...))
}


src <- function (subclass, ...) 
{
    subclass <- paste0("src_", subclass)
    structure(list(...), class = c(subclass, "src"))
}


db_create_indexes <- function (con, table, indexes = NULL, unique = FALSE, ...) 
{
    UseMethod("db_create_indexes")
}


rename_ <- function (.data, ..., .dots) 
{
    UseMethod("rename_")
}


order_by <- function (order_by, call) 
{
    call <- substitute(call)
    stopifnot(is.call(call))
    new_call <- as.call(c(quote(with_order), list(substitute(order_by)), 
        as.list(call)))
    eval(new_call, parent.frame())
}


semi_join <- function (x, y, by = NULL, copy = FALSE, ...) 
{
    UseMethod("semi_join")
}


sql_render <- function (query, con = NULL, ...) 
{
    UseMethod("sql_render")
}


db_has_table <- function (con, table) 
UseMethod("db_has_table")


na_if <- function (x, y) 
{
    if (length(y) != length(x) && length(y) != 1) {
        stop("`y` must be length 1 or same length as `x`", call. = FALSE)
    }
    x[x == y] <- NA
    x
}


tbl <- function (src, ...) 
{
    UseMethod("tbl")
}


group_indices_ <- function (.data, ..., .dots) 
{
    UseMethod("group_indices_")
}


tally <- function (x, wt, sort = FALSE) 
{
    if (missing(wt)) {
        if ("n" %in% names(x)) {
            message("Using n as weighting variable")
            wt <- quote(n)
        }
        else {
            wt <- NULL
        }
    }
    else {
        wt <- substitute(wt)
    }
    tally_(x, wt, sort = sort)
}


mutate_each_ <- function (tbl, funs, vars) 
{
    vars <- colwise_(tbl, funs, vars)
    mutate_(tbl, .dots = vars)
}


bind_cols <- function (...) 
{
    x <- list_or_dots(...)
    cbind_all(x)
}


sql_translate_env <- function (con) 
UseMethod("sql_translate_env")


all_equal <- function (target, current, ignore_col_order = TRUE, ignore_row_order = TRUE, 
    convert = FALSE, ...) 
{
    res <- equal_data_frame(target, current, ignore_col_order = ignore_col_order, 
        ignore_row_order = ignore_row_order, convert = convert)
    if (res) {
        TRUE
    }
    else {
        attr(res, "comment")
    }
}


sample_frac <- function (tbl, size = 1, replace = FALSE, weight = NULL, .env = parent.frame()) 
{
    UseMethod("sample_frac")
}


is.sql <- function (x) 
inherits(x, "sql")


db_data_type <- function (con, fields) 
UseMethod("db_data_type")


location <- function (df) 
{
    assert_that(is.data.frame(df))
    structure(list(df = loc(df), vars = dfloc(df), attr = plfloc(attributes(df))), 
        class = "location")
}


lst_ <- tibble::lst_ # re-exported from tibble package

lahman_srcs <- function (..., quiet = NULL) 
{
    load_srcs(lahman, c(...), quiet = quiet)
}


if_else <- function (condition, true, false, missing = NULL) 
{
    if (!is.logical(condition)) {
        stop("`condition` must be logical", call. = FALSE)
    }
    out <- true[rep(NA_integer_, length(condition))]
    out <- replace_with(out, condition & !is.na(condition), true, 
        "`true`")
    out <- replace_with(out, !condition & !is.na(condition), 
        false, "`false`")
    out <- replace_with(out, is.na(condition), missing, "`missing`")
    out
}


arrange_ <- function (.data, ..., .dots) 
{
    UseMethod("arrange_")
}


src_df <- function (pkg = NULL, env = NULL) 
{
    src_local("tbl_df", pkg, env)
}


op_sort <- function (op) 
UseMethod("op_sort")


db_begin <- function (con, ...) 
UseMethod("db_begin")


is.src <- function (x) 
inherits(x, "src")


case_when <- function (...) 
{
    formulas <- list(...)
    n <- length(formulas)
    if (n == 0) {
        stop("No cases provided", call. = FALSE)
    }
    query <- vector("list", n)
    value <- vector("list", n)
    for (i in seq_len(n)) {
        f <- formulas[[i]]
        if (!inherits(f, "formula") || length(f) != 3) {
            non_formula_arg <- substitute(list(...))[[i + 1]]
            stop("Case ", i, " (", deparse_trunc(non_formula_arg), 
                ") is not a two-sided formula", call. = FALSE)
        }
        env <- environment(f)
        query[[i]] <- eval(f[[2]], envir = env)
        if (!is.logical(query[[i]])) {
            stop("LHS of case ", i, " (", deparse_trunc(f_lhs(f)), 
                ") is ", typeof(query[[i]]), ", not logical", 
                call. = FALSE)
        }
        value[[i]] <- eval(f[[3]], envir = env)
    }
    m <- max(vapply(query, length, integer(1)))
    out <- value[[1]][rep(NA_integer_, m)]
    replaced <- rep(FALSE, m)
    for (i in seq_len(n)) {
        check_length(query[[i]], out, paste0("LHS of case ", 
            i, " (", deparse_trunc(f_lhs(formulas[[i]])), ")"))
        out <- replace_with(out, query[[i]] & !replaced, value[[i]], 
            paste0("RHS of case ", i, " (", deparse_trunc(f_rhs(formulas[[i]])), 
                ")"))
        replaced <- replaced | query[[i]]
    }
    out
}


ungroup <- function (x, ...) 
{
    UseMethod("ungroup")
}


inner_join <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
    ...) 
{
    UseMethod("inner_join")
}


funs_ <- function (dots, args = list(), env = baseenv()) 
{
    dots <- lazyeval::as.lazy_dots(dots, env)
    env <- lazyeval::common_env(dots)
    names(dots) <- names2(dots)
    dots[] <- lapply(dots, function(x) {
        x$expr <- make_call(x$expr, args)
        x
    })
    missing_names <- names(dots) == ""
    default_names <- vapply(dots[missing_names], function(x) make_name(x$expr), 
        character(1))
    names(dots)[missing_names] <- default_names
    class(dots) <- c("fun_list", "lazy_dots")
    attr(dots, "has_names") <- any(!missing_names)
    dots
}


lst <- tibble::lst # re-exported from tibble package

is.tbl <- function (x) 
inherits(x, "tbl")


`%>%` <- magrittr::`%>%` # re-exported from magrittr package

group_by_ <- function (.data, ..., .dots, add = FALSE) 
{
    UseMethod("group_by_")
}


db_create_index <- function (con, table, columns, name = NULL, unique = FALSE, ...) 
{
    UseMethod("db_create_index")
}


dense_rank <- function (x) 
{
    r <- rank(x, na.last = "keep")
    match(r, sort(unique(r)))
}


db_rollback <- function (con, ...) 
UseMethod("db_rollback")


lahman_mysql <- function (dbname = "lahman", ...) 
{
    copy_lahman(src_mysql(dbname, ...))
}


type_sum <- tibble::type_sum # re-exported from tibble package

num_range <- function (prefix, range, width = NULL, vars = current_vars()) 
{
    if (!is.null(width)) {
        range <- sprintf(paste0("%0", width, "d"), range)
    }
    match_vars(paste0(prefix, range), vars)
}


op_base <- function (name, src, x, vars) 
{
    stopifnot(is.character(vars))
    structure(list(src = src, x = x, vars = vars), class = c(paste0("op_base_", 
        name), "op_base", "op"))
}


rowwise <- function (data) 
{
    stopifnot(is.data.frame(data))
    assert_all_white_list(data)
    structure(data, class = c("rowwise_df", "tbl_df", "tbl", 
        "data.frame"))
}


group_by <- function (.data, ..., add = FALSE) 
{
    group_by_(.data, .dots = lazyeval::lazy_dots(...), add = add)
}


group_size <- function (x) 
UseMethod("group_size")


as.tbl_cube <- function (x, ...) 
UseMethod("as.tbl_cube")


ntile <- function (x, n) 
{
    floor((n * (row_number(x) - 1)/length(x)) + 1)
}


query <- function (con, sql, .vars) 
UseMethod("query")


recode <- function (.x, ..., .default = NULL, .missing = NULL) 
{
    UseMethod("recode")
}


collect <- function (x, ...) 
{
    UseMethod("collect")
}


auto_copy <- function (x, y, copy = FALSE, ...) 
{
    if (same_src(x, y)) 
        return(y)
    if (!copy) {
        stop("x and y don't share the same src. Set copy = TRUE to copy y into ", 
            "x's source (this may be time consuming).", call. = FALSE)
    }
    UseMethod("auto_copy")
}


sql_build <- function (op, con, ...) 
{
    UseMethod("sql_build")
}


src_desc <- function (x) 
UseMethod("src_desc")


right_join <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
    ...) 
{
    UseMethod("right_join")
}


base_win <- "<environment>"

regroup <- function (x, value) 
{
    .Deprecated("group_by_")
    group_by_(x, .dots = value)
}


top_n <- function (x, n, wt) 
{
    if (missing(wt)) {
        vars <- tbl_vars(x)
        message("Selecting by ", vars[length(vars)])
        wt <- as.name(vars[length(vars)])
    }
    else {
        wt <- substitute(wt)
    }
    stopifnot(is.numeric(n), length(n) == 1)
    if (n > 0) {
        call <- substitute(filter(x, min_rank(desc(wt)) <= n), 
            list(n = n, wt = wt))
    }
    else {
        call <- substitute(filter(x, min_rank(wt) <= n), list(n = abs(n), 
            wt = wt))
    }
    eval(call)
}


rbind_all <- function (x, id = NULL) 
{
    warning("`rbind_all()` is deprecated. Please use `bind_rows()` instead.", 
        call. = FALSE)
    bind_rows_(x, id = id)
}


sql_escape_string <- function (con, x) 
UseMethod("sql_escape_string")


min_rank <- function (x) 
rank(x, ties.method = "min", na.last = "keep")


summarise_if <- function (.tbl, .predicate, .funs, ...) 
{
    if (inherits(.tbl, "tbl_lazy")) {
        stop("Conditional colwise operations currently require local sources", 
            call. = FALSE)
    }
    cols <- probe_colwise_names(.tbl, .predicate)
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, cols)
    summarise_(.tbl, .dots = vars)
}


bind_rows <- function (..., .id = NULL) 
{
    x <- list_or_dots(...)
    if (!is.null(.id)) {
        if (!(is.character(.id) && length(.id) == 1)) {
            stop(".id is not a string", call. = FALSE)
        }
        names(x) <- names(x) %||% seq_along(x)
    }
    bind_rows_(x, .id)
}


setequal <- function (x, y, ...) 
UseMethod("setequal")


union_all <- function (x, y, ...) 
UseMethod("union_all")


union <- function (x, y, ...) 
UseMethod("union")


n_distinct <- function (..., na.rm = FALSE) 
{
    n_distinct_multi(list(...), na.rm)
}


summarise_each <- function (tbl, funs, ...) 
{
    summarise_each_(tbl, funs, lazyeval::lazy_dots(...))
}


src_mysql <- function (dbname, host = NULL, port = 0L, user = "root", password = "", 
    ...) 
{
    if (!requireNamespace("RMySQL", quietly = TRUE)) {
        stop("RMySQL package required to connect to mysql/mariadb", 
            call. = FALSE)
    }
    con <- dbConnect(RMySQL::MySQL(), dbname = dbname, host = host, 
        port = port, username = user, password = password, ...)
    info <- dbGetInfo(con)
    src_sql("mysql", con, info = info, disco = db_disconnector(con, 
        "mysql"))
}


db_drop_table <- function (con, table, force = FALSE, ...) 
{
    UseMethod("db_drop_table")
}


glimpse <- tibble::glimpse # re-exported from tibble package

bench_tbls <- function (tbls, op, ..., times = 10) 
{
    if (!requireNamespace("microbenchmark")) {
        stop("Please install the microbenchmark package", call. = FALSE)
    }
    calls <- lapply(seq_along(tbls), function(i) {
        substitute(op(tbls[[i]]), list(i = i))
    })
    names(calls) <- names(tbls)
    mb <- as.call(c(quote(microbenchmark::microbenchmark), calls, 
        dots(...), list(times = times)))
    eval(mb)
}


tibble <- tibble::tibble # re-exported from tibble package

distinct_ <- function (.data, ..., .dots, .keep_all = FALSE) 
{
    UseMethod("distinct_")
}


cume_dist <- function (x) 
{
    rank(x, ties.method = "max", na.last = "keep")/sum(!is.na(x))
}


test_frame <- function (..., srcs = test_srcs$get(), ignore = character()) 
{
    df <- data_frame(...)
    test_load(df, srcs = srcs, ignore = ignore)
}


make_tbl <- function (subclass, ...) 
{
    subclass <- paste0("tbl_", subclass)
    structure(list(...), class = c(subclass, "tbl"))
}


copy_nycflights13 <- function (src, ...) 
{
    all <- utils::data(package = "nycflights13")$results[, 3]
    unique_index <- list(airlines = list("carrier"), planes = list("tailnum"))
    index <- list(airports = list("faa"), flights = list(c("year", 
        "month", "day"), "carrier", "tailnum", "origin", "dest"), 
        weather = list(c("year", "month", "day"), "origin"))
    tables <- setdiff(all, src_tbls(src))
    for (table in tables) {
        df <- getExportedValue("nycflights13", table)
        message("Creating table: ", table)
        copy_to(src, df, table, unique_indexes = unique_index[[table]], 
            indexes = index[[table]], temporary = FALSE)
    }
    src
}


summarize_ <- function (.data, ..., .dots) 
{
    UseMethod("summarise_")
}


summarise_each_ <- function (tbl, funs, vars) 
{
    if (is.character(funs)) {
        funs <- funs_(funs)
    }
    vars <- colwise_(tbl, funs, vars)
    summarise_(tbl, .dots = vars)
}


id <- function (.variables, drop = FALSE) 
{
    warning("id() is deprecated", call. = FALSE)
    lengths <- vapply(.variables, length, integer(1))
    .variables <- .variables[lengths != 0]
    if (length(.variables) == 0) {
        n <- nrow(.variables) %||% 0L
        return(structure(seq_len(n), n = n))
    }
    if (length(.variables) == 1) {
        return(id_var(.variables[[1]], drop = drop))
    }
    ids <- rev(lapply(.variables, id_var, drop = drop))
    p <- length(ids)
    ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1), 
        USE.NAMES = FALSE)
    n <- prod(ndistinct)
    if (n > 2^31) {
        char_id <- do.call("paste", c(ids, sep = "\r"))
        res <- match(char_id, unique(char_id))
    }
    else {
        combs <- c(1, cumprod(ndistinct[-p]))
        mat <- do.call("cbind", ids)
        res <- c((mat - 1L) %*% combs + 1L)
    }
    attr(res, "n") <- n
    if (drop) {
        id_var(res, drop = TRUE)
    }
    else {
        structure(as.integer(res), n = attr(res, "n"))
    }
}


rename <- function (.data, ...) 
{
    rename_(.data, .dots = lazyeval::lazy_dots(...))
}


ident <- function (...) 
{
    x <- c(...)
    if (length(x) == 0) 
        return(sql())
    stopifnot(is.character(x))
    structure(x, class = c("ident", "sql", "character"))
}


combine <- function (...) 
{
    args <- list(...)
    if (length(args) == 1 && is.list(args[[1]])) {
        combine_all(args[[1]])
    }
    else {
        combine_all(args)
    }
}


full_join <- function (x, y, by = NULL, copy = FALSE, suffix = c(".x", ".y"), 
    ...) 
{
    UseMethod("full_join")
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

db_save_query <- function (con, sql, name, temporary = TRUE, ...) 
{
    UseMethod("db_save_query")
}


tbl_df <- function (data) 
{
    as_data_frame(data)
}


as.tbl <- function (x, ...) 
UseMethod("as.tbl")


desc <- function (x) 
-xtfrm(x)


copy_lahman <- function (src, ...) 
{
    tables <- setdiff(lahman_tables(), src_tbls(src))
    for (table in tables) {
        df <- getExportedValue("Lahman", table)
        message("Creating table: ", table)
        ids <- as.list(names(df)[grepl("ID$", names(df))])
        copy_to(src, df, table, indexes = ids, temporary = FALSE)
    }
    src
}


add_row <- tibble::add_row # re-exported from tibble package

summarize_at <- function (.tbl, .cols, .funs, ...) 
{
    cols <- select_colwise_names(.tbl, .cols)
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, cols)
    summarise_(.tbl, .dots = vars)
}


mutate_each_q <- function (...) 
{
    .Deprecated("mutate_all")
    mutate_each_(...)
}


db_explain <- function (con, sql, ...) 
{
    UseMethod("db_explain")
}


mutate_if <- function (.tbl, .predicate, .funs, ...) 
{
    if (inherits(.tbl, "tbl_lazy")) {
        stop("Conditional colwise operations currently require local sources", 
            call. = FALSE)
    }
    cols <- probe_colwise_names(.tbl, .predicate)
    funs <- as.fun_list(.funs, .env = parent.frame(), ...)
    vars <- colwise_(.tbl, funs, cols)
    mutate_(.tbl, .dots = vars)
}


nycflights13_postgres <- function (dbname = "nycflights13", ...) 
{
    cache_computation("nycflights_postgres", {
        message("Caching nycflights db in postgresql db ", dbname)
        copy_nycflights13(src_postgres(dbname, ...))
    })
}


count <- function (x, ..., wt = NULL, sort = FALSE) 
{
    vars <- lazyeval::lazy_dots(...)
    wt <- substitute(wt)
    count_(x, vars, wt, sort = sort)
}


sql_join <- function (con, x, y, type = "inner", by = NULL, ...) 
{
    UseMethod("sql_join")
}


translate_sql_ <- function (dots, con = NULL, vars = character(), vars_group = NULL, 
    vars_order = NULL, window = TRUE) 
{
    expr <- lazyeval::as.lazy_dots(dots, env = parent.frame())
    if (!any(has_names(expr))) {
        names(expr) <- NULL
    }
    if (length(vars) > 0) {
        expr <- partial_eval2(expr, vars)
    }
    else {
        expr <- lapply(expr, "[[", "expr")
    }
    variant <- sql_translate_env(con)
    if (window) {
        old_con <- set_partition_con(con)
        on.exit(set_partition_con(old_con), add = TRUE)
        old_group <- set_partition_group(vars_group)
        on.exit(set_partition_group(old_group), add = TRUE)
        old_order <- set_partition_order(vars_order)
        on.exit(set_partition_order(old_order), add = TRUE)
    }
    pieces <- lapply(expr, function(x) {
        if (is.atomic(x)) 
            return(escape(x, con = con))
        env <- sql_env(x, variant, con, window = window)
        escape(eval(x, envir = env))
    })
    sql(unlist(pieces))
}




## Package Data

nasa <- dplyr::nasa		## NASA spatio-temporal data



## Package Info

.skeleton_package_title = "A Grammar of Data Manipulation"

.skeleton_package_version = "0.5.0"

.skeleton_package_depends = ""

.skeleton_package_imports = "assertthat,utils,R6,Rcpp,tibble,magrittr,lazyeval,DBI"


## Internal

.skeleton_version = 5


## EOF