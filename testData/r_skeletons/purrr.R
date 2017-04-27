##
## Exported symobls in package `purrr`
##

## Exported package methods

keep <- function (.x, .p, ...) 
{
    sel <- probe(.x, .p, ...)
    .x[!is.na(sel) & sel]
}


discard <- function (.x, .p, ...) 
{
    sel <- probe(.x, .p, ...)
    .x[is.na(sel) | !sel]
}


pmap_df <- function (.l, .f, ..., .id = NULL) 
{
    .f <- as_function(.f, ...)
    res <- pmap(.l, .f, ...)
    dplyr::bind_rows(res, .id = .id)
}


is_scalar_vector <- function (x) 
{
    is_vector(x) && length(x) == 1
}


cross_n <- function (.l, .filter = NULL) 
{
    if (is_empty(.l)) {
        return(.l)
    }
    if (!is.null(.filter)) {
        .filter <- as_function(.filter)
    }
    n <- length(.l)
    lengths <- lapply(.l, length)
    names <- names(.l)
    factors <- cumprod(lengths)
    total_length <- factors[n]
    factors <- c(1, factors[-n])
    out <- replicate(total_length, vector("list", n), simplify = FALSE)
    for (i in seq_along(out)) {
        for (j in seq_len(n)) {
            index <- floor((i - 1)/factors[j])%%length(.l[[j]]) + 
                1
            out[[i]][[j]] <- .l[[j]][[index]]
        }
        names(out[[i]]) <- names
        if (!is.null(.filter)) {
            is_to_filter <- do.call(".filter", unname(out[[i]]))
            if (!is.logical(is_to_filter) || !length(is_to_filter) == 
                1) {
                stop("The filter function must return TRUE or FALSE", 
                  call. = FALSE)
            }
            if (is_to_filter) {
                out[i] <- list(NULL)
            }
        }
    }
    compact(out)
}


map_int <- function (.x, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map_impl, environment(), ".x", ".f", "integer")
}


rerun <- function (.n, ...) 
{
    dots <- lazyeval::lazy_dots(...)
    if (length(dots) == 1 && !has_names(dots)) {
        dots <- dots[[1]]
    }
    out <- vector("list", .n)
    for (i in seq_len(.n)) {
        out[[i]] <- lazyeval::lazy_eval(dots)
    }
    out
}


invoke_map_chr <- function (.f, .x = list(NULL), ..., .env = NULL) 
{
    .env <- .env %||% lazyeval::expr_env(.f, parent.frame())
    .f <- as_invoke_function(.f)
    map2_chr(.f, .x, invoke, ..., .env = .env)
}


rdunif <- function (n, b, a = 1) 
{
    sample(b - a + 1, n, replace = TRUE) + a - 1
}


map_call <- function (.x, .f, ...) 
{
    warning("`map_call()` is deprecated. Please use `invoke()` instead.")
    invoke(.f, .x, ...)
}


is_bare_integer <- function (x) 
{
    !is.object(x) && is_integer(x)
}


dmap_if <- function (.d, .p, .f, ...) 
{
    sel <- map_lgl(.d, .p)
    partial_dmap(.d, sel, .f, ...)
}


by_row <- function (.d, ..f, ..., .collate = c("list", "rows", "cols"), 
    .to = ".out", .labels = TRUE) 
{
    check_df_consistency(.d)
    if (nrow(.d) < 1) {
        return(.d)
    }
    ..f <- as_rows_function(..f)
    .collate <- match.arg(.collate)
    indices <- seq(0, nrow(.d) - 1)
    attr(.d, "indices") <- as.list(indices)
    .unique_labels <- 0
    .labels_cols <- .d
    .slicing_cols <- .d
    .Call(by_slice_impl, environment(), ".d", "..f")
}


prepend <- function (x, values, before = 1) 
{
    n <- length(x)
    stopifnot(before > 0 && before <= n)
    if (before == 1) {
        c(values, x)
    }
    else {
        c(x[1:(before - 1)], values, x[before:n])
    }
}


detect_index <- function (.x, .p, ..., .right = FALSE) 
{
    .p <- as_function(.p, ...)
    for (i in index(.x, .right)) {
        if (isTRUE(.p(.x[[i]], ...))) 
            return(i)
    }
    0
}


accumulate <- function (.x, .f, ..., .init) 
{
    .f <- as_function(.f, ...)
    f <- function(x, y) {
        .f(x, y, ...)
    }
    Reduce(f, .x, init = .init, accumulate = TRUE)
}


is_bare_vector <- function (x) 
{
    is_bare_atomic(x) || is_bare_list(x)
}


is_bare_atomic <- function (x) 
{
    !is.object(x) && is_atomic(x)
}


is_empty <- function (x) 
length(x) == 0


is_formula <- function (x) 
inherits(x, "formula")


is_bare_character <- function (x) 
{
    !is.object(x) && is_character(x)
}


invoke_map_df <- function (.f, .x = list(NULL), ..., .env = NULL) 
{
    .env <- .env %||% lazyeval::expr_env(.f, parent.frame())
    .f <- as_invoke_function(.f)
    map2_df(.f, .x, invoke, ..., .env = .env)
}


invoke_map_dbl <- function (.f, .x = list(NULL), ..., .env = NULL) 
{
    .env <- .env %||% lazyeval::expr_env(.f, parent.frame())
    .f <- as_invoke_function(.f)
    map2_dbl(.f, .x, invoke, ..., .env = .env)
}


invoke_map_int <- function (.f, .x = list(NULL), ..., .env = NULL) 
{
    .env <- .env %||% lazyeval::expr_env(.f, parent.frame())
    .f <- as_invoke_function(.f)
    map2_int(.f, .x, invoke, ..., .env = .env)
}


sort_by <- function (.x, .f, ...) 
{
    .x[order_by(.x, .f, ...)]
}


is_character <- function (x) 
{
    typeof(x) == "character"
}


flatten <- function (.x) 
{
    .Call(flatten_impl, .x)
}


is_bare_numeric <- function (x) 
{
    !is.object(x) && is_numeric(x)
}


contains <- function (.x, .y) 
{
    some(.x, identical, .y)
}


compact <- function (.x, .p = identity) 
{
    .f <- as_function(.p)
    .x %>% discard(function(x) is_empty(.p(x)))
}


pmap_lgl <- function (.l, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(pmap_impl, environment(), ".l", ".f", "logical")
}


flatten_int <- function (.x) 
{
    .Call(vflatten_impl, .x, "integer")
}


array_branch <- function (array, margin = NULL) 
{
    dim(array) <- dim(array) %||% length(array)
    margin <- margin %||% seq_along(dim(array))
    if (length(margin) == 0) {
        list(array)
    }
    else {
        apply(array, margin, list) %>% flatten()
    }
}


rbernoulli <- function (n, p = 0.5) 
{
    sample(c(TRUE, FALSE), n, replace = TRUE, prob = c(p, 1 - 
        p))
}


map_if <- function (.x, .p, .f, ...) 
{
    .x <- c(.x)
    sel <- probe(.x, .p)
    .x[sel] <- map(.x[sel], .f, ...)
    .x
}


is_numeric <- function (x) 
{
    typeof(x) %in% c("integer", "double")
}


lift <- function (..f, ..., .unnamed = FALSE) 
{
    force(..f)
    defaults <- list(...)
    function(.x = list(), ...) {
        if (.unnamed) {
            .x <- unname(.x)
        }
        do.call("..f", c(.x, defaults, list(...)))
    }
}


map2_dbl <- function (.x, .y, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map2_impl, environment(), ".x", ".y", ".f", "double")
}


pmap_dbl <- function (.l, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(pmap_impl, environment(), ".l", ".f", "double")
}


is_logical <- function (x) 
{
    typeof(x) == "logical"
}


map_at <- function (.x, .at, .f, ...) 
{
    .x <- c(.x)
    sel <- inv_which(.x, .at)
    .x[sel] <- map(.x[sel], .f, ...)
    .x
}


when <- function (., ...) 
{
    dots <- list(...)
    names <- names(dots)
    named <- if (is.null(names)) 
        rep(FALSE, length(dots))
    else names != ""
    if (sum(!named) == 0) 
        stop("At least one matching condition is needed.", call. = FALSE)
    is_formula <- vapply(dots, function(dot) identical(class(dot), 
        "formula"), logical(1L))
    env <- new.env(parent = parent.frame())
    env[["."]] <- .
    if (sum(named) > 0) 
        for (i in which(named)) env[[names[i]]] <- dots[[i]]
    result <- NULL
    for (i in which(!named)) {
        if (is_formula[i]) {
            action <- length(dots[[i]])
            if (action == 2 || isTRUE(eval(dots[[i]][[2]], env, 
                env))) {
                result <- eval(dots[[i]][[action]], env, env)
                break
            }
        }
        else {
            result <- dots[[i]]
        }
    }
    result
}


flatten_df <- function (.x, .id = NULL) 
{
    res <- .Call(flatten_impl, .x)
    dplyr::bind_rows(res, .id = .id)
}


as_function <- function (.f, ...) 
{
    UseMethod("as_function")
}


map2_df <- function (.x, .y, .f, ..., .id = NULL) 
{
    .f <- as_function(.f, ...)
    res <- map2(.x, .y, .f, ...)
    dplyr::bind_rows(res, .id = .id)
}


is_scalar_numeric <- function (x) 
{
    is_numeric(x) && length(x) == 1
}


slice_rows <- function (.d, .cols = NULL) 
{
    stopifnot(is.data.frame(.d))
    if (is.null(.cols)) {
        return(unslice(.d))
    }
    if (is.numeric(.cols)) {
        .cols <- names(.d)[.cols]
    }
    stopifnot(is.character(.cols))
    dplyr::group_by_(.d, .dots = .cols)
}


map2_lgl <- function (.x, .y, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map2_impl, environment(), ".x", ".y", ".f", "logical")
}


reduce <- function (.x, .f, ..., .init) 
{
    .f <- as_function(.f, ...)
    f <- function(x, y) {
        .f(x, y, ...)
    }
    Reduce(f, .x, init = .init)
}


lmap <- function (.x, .f, ...) 
{
    .x %>% lmap_at(seq_along(.x), .f, ...)
}


quietly <- function (.f) 
{
    .f <- as_function(.f)
    function(...) capture_output(.f(...))
}


map_chr <- function (.x, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map_impl, environment(), ".x", ".f", "character")
}


is_integer <- function (x) 
{
    typeof(x) == "integer"
}


flatten_chr <- function (.x) 
{
    .Call(vflatten_impl, .x, "character")
}


is_vector <- function (x) 
{
    is_atomic(x) || is.list(x)
}


is_scalar_logical <- function (x) 
{
    is_logical(x) && length(x) == 1
}


splice <- function (...) 
{
    dots <- list(...)
    if (length(dots) == 0) {
        return(dots)
    }
    names <- Map(function(dot, name) {
        if (is_bare_list(dot)) 
            names2(dot)
        else name
    }, dots, names2(dots))
    names <- flatten(names)
    is_not_list <- vapply(dots, negate(is_bare_list), logical(1))
    dots[is_not_list] <- lapply(dots[is_not_list], list)
    dots %>% flatten() %>% stats::setNames(names)
}


by_slice <- function (.d, ..f, ..., .collate = c("list", "rows", "cols"), 
    .to = ".out", .labels = TRUE) 
{
    ..f <- as_rows_function(..f)
    if (!dplyr::is.grouped_df(.d)) {
        stop(".d must be a sliced data frame", call. = FALSE)
    }
    if (length(.d) <= length(attr(.d, "labels"))) {
        stop("Mappable part of data frame is empty", call. = FALSE)
    }
    .collate <- match.arg(.collate)
    set_sliced_env(.d, .labels, .collate, .to, environment(), 
        ".d")
    .Call(by_slice_impl, environment(), ".d", "..f")
}


zip2 <- function (.x, .y, .fields = NULL) 
{
    warning("`zip2(x, y)` is deprecated, please use `transpose(list(x, y))` instead.", 
        call. = FALSE)
    transpose(list(.x, .y))
}


as_vector <- function (.x, .type = NULL) 
{
    if (can_simplify(.x, .type)) {
        unlist(.x)
    }
    else {
        stop("Cannot coerce .x to a vector", call. = FALSE)
    }
}


pmap_chr <- function (.l, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(pmap_impl, environment(), ".l", ".f", "character")
}


lift_dv <- function (..f, ..., .unnamed = FALSE) 
{
    force(..f)
    defaults <- list(...)
    function(.x, ...) {
        if (.unnamed) {
            .x <- unname(.x)
        }
        .x <- as.list(.x)
        do.call("..f", c(.x, defaults, list(...)))
    }
}


zip3 <- function (.x, .y, .z, .fields = NULL) 
{
    warning("`zip2(x, y, z)` is deprecated, please use `transpose(list(x, y, z))` instead.", 
        call. = FALSE)
    transpose(list(.x, .y, .z))
}


is_atomic <- function (x) 
{
    typeof(x) %in% c("logical", "integer", "double", "complex", 
        "character", "raw")
}


order_by <- function (.x, .f, ...) 
{
    vals <- map(.x, .f, ...)
    do.call("order", simplify_all(transpose(vals)))
}


invoke <- function (.f, .x = NULL, ..., .env = NULL) 
{
    .env <- .env %||% lazyeval::expr_env(.f, parent.frame())
    do.call(.f, c(.x, list(...)), envir = .env)
}


is_function <- magrittr::is_function # re-exported from magrittr package

lift_vd <- function (..f, ..., .type) 
{
    force(..f)
    defaults <- list(...)
    if (missing(.type)) 
        .type <- NULL
    function(...) {
        x <- as_vector(list(...), .type)
        do.call("..f", c(list(x), defaults))
    }
}


is_bare_list <- function (x) 
{
    !is.object(x) && is_list(x)
}


`%>%` <- magrittr::`%>%` # re-exported from magrittr package

is_scalar_double <- function (x) 
{
    is_double(x) && length(x) == 1
}


walk2 <- function (.x, .y, .f, ...) 
{
    pwalk(list(.x, .y), .f, ...)
    invisible(.x)
}


walk3 <- function (.x, .y, .z, .f, ...) 
{
    warning("`walk3(x, y, z)` is deprecated. Please use `pwalk(list(x, y, z))` ", 
        "instead", call. = FALSE)
    pwalk(list(.x, .y, .z), .f, ...)
    invisible(.x)
}


split_by <- function (.x, .f, ...) 
{
    vals <- map(.x, .f, ...)
    split(.x, simplify_all(transpose(vals)))
}


`%||%` <- function (x, y) 
{
    if (is.null(x)) {
        y
    }
    else {
        x
    }
}


some <- function (.x, .p, ...) 
{
    .p <- as_function(.p, ...)
    for (i in seq_along(.x)) {
        if (isTRUE(.p(.x[[i]], ...))) 
            return(TRUE)
    }
    FALSE
}


map_n <- function (...) 
{
    warning("`map_n()` is deprecated; please use `pmap()` instead.", 
        call. = FALSE)
    pmap(...)
}


lift_lv <- function (..f, ...) 
{
    force(..f)
    defaults <- list(...)
    function(.x, ...) {
        do.call("..f", c(list(as.list(.x)), defaults, list(...)))
    }
}


map2_chr <- function (.x, .y, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map2_impl, environment(), ".x", ".y", ".f", "character")
}


update_list <- function (`_data`, ...) 
{
    new_values <- list(...)
    is_formula <- map_lgl(new_values, ~inherits(., "formula"))
    new_values[is_formula] <- lapply(new_values[is_formula], 
        function(f) {
            stopifnot(length(f) == 2)
            eval(f[[2]], `_data`, environment(f))
        })
    utils::modifyList(`_data`, new_values)
}


flatten_dbl <- function (.x) 
{
    .Call(vflatten_impl, .x, "double")
}


accumulate_right <- function (.x, .f, ..., .init) 
{
    .f <- as_function(.f, ...)
    f <- function(x, y) {
        .f(y, x, ...)
    }
    Reduce(f, .x, init = .init, right = TRUE, accumulate = TRUE)
}


lmap_at <- function (.x, .at, .f, ...) 
{
    if (is_formula(.f)) {
        .f <- as_function(.f, ...)
    }
    sel <- inv_which(.x, .at)
    out <- vector("list", length(.x))
    for (i in seq_along(.x)) {
        res <- if (sel[[i]]) {
            .f(.x[i], ...)
        }
        else {
            .x[i]
        }
        stopifnot(is.list(res))
        out[[i]] <- res
    }
    flatten(out) %>% maybe_as_data_frame(.x)
}


is_list <- function (x) 
{
    typeof(x) == "list"
}


lift_vl <- function (..f, ..., .type) 
{
    force(..f)
    defaults <- list(...)
    if (missing(.type)) 
        .type <- NULL
    function(.x = list(), ...) {
        x <- as_vector(.x, .type)
        do.call("..f", c(list(x), defaults, list(...)))
    }
}


zip_n <- function (...) 
{
    warning("`zip_n()` is deprecated; please use `transpose()` instead.", 
        call. = FALSE)
    transpose(...)
}


map_df <- function (.x, .f, ..., .id = NULL) 
{
    .f <- as_function(.f, ...)
    res <- map(.x, .f, ...)
    dplyr::bind_rows(res, .id = .id)
}


simplify_all <- function (.x, .type = NULL) 
{
    map(.x, simplify, .type = .type)
}


pmap_int <- function (.l, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(pmap_impl, environment(), ".l", ".f", "integer")
}


partial <- function (...f, ..., .env = parent.frame(), .lazy = TRUE, .first = TRUE) 
{
    stopifnot(is.function(...f))
    if (.lazy) {
        fcall <- substitute(...f(...))
    }
    else {
        fcall <- make_call(substitute(...f), .args = list(...))
    }
    n <- length(fcall)
    if (!.first && n > 1) {
        tmp <- fcall[1]
        tmp[[2]] <- quote(...)
        tmp[seq(3, n + 1)] <- fcall[seq(2, n)]
        names(tmp)[seq(3, n + 1)] <- names2(fcall)[seq(2, n)]
        fcall <- tmp
    }
    else {
        fcall[[n + 1]] <- quote(...)
    }
    args <- list("..." = quote(expr = ))
    make_function(args, fcall, .env)
}


is_bare_double <- function (x) 
{
    !is.object(x) && is_double(x)
}


lmap_if <- function (.x, .p, .f, ...) 
{
    sel <- probe(.x, .p) %>% which()
    .x %>% lmap_at(sel, .f, ...)
}


lift_dl <- function (..f, ..., .unnamed = FALSE) 
{
    force(..f)
    defaults <- list(...)
    function(.x = list(), ...) {
        if (.unnamed) {
            .x <- unname(.x)
        }
        do.call("..f", c(.x, defaults, list(...)))
    }
}


map_lgl <- function (.x, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map_impl, environment(), ".x", ".f", "logical")
}


pmap <- function (.l, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(pmap_impl, environment(), ".l", ".f", "list")
}


is_null <- function (x) 
{
    typeof(x) == "NULL"
}


unslice <- function (.d) 
{
    dplyr::group_by_(.d, .dots = list())
}


map_rows <- function (.d, .f, ..., .labels = TRUE) 
{
    message("`map_rows()` is deprecated; please use `invoke_rows()` instead.", 
        call. = FALSE)
    invoke_rows(.f, .d, ..., .labels = .labels)
}


set_names <- magrittr::set_names # re-exported from magrittr package

map_dbl <- function (.x, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map_impl, environment(), ".x", ".f", "double")
}


lift_ld <- function (..f, ...) 
{
    force(..f)
    defaults <- list(...)
    function(...) {
        do.call("..f", c(list(list(...)), defaults))
    }
}


is_scalar_list <- function (x) 
{
    is_list(x) && length(x) == 1
}


safely <- function (.f, otherwise = NULL, quiet = TRUE) 
{
    .f <- as_function(.f)
    function(...) capture_error(.f(...), otherwise)
}


array_tree <- function (array, margin = NULL) 
{
    dim(array) <- dim(array) %||% length(array)
    margin <- margin %||% seq_along(dim(array))
    if (length(margin) > 1) {
        new_margin <- ifelse(margin[-1] > margin[[1]], margin[-1] - 
            1, margin[-1])
        apply(array, margin[[1]], . %>% array_tree(new_margin))
    }
    else {
        array_branch(array, margin)
    }
}


walk_n <- function (...) 
{
    warning("`walk_n()` is deprecated; please use `pwalk()` instead.", 
        call. = FALSE)
    pwalk(...)
}


map2_int <- function (.x, .y, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map2_impl, environment(), ".x", ".y", ".f", "integer")
}


negate <- function (.p) 
{
    .p <- as_function(.p)
    function(...) !.p(...)
}


is_double <- function (x) 
{
    typeof(x) == "double"
}


transpose <- function (.l) 
{
    .Call(transpose_impl, .l)
}


map2 <- function (.x, .y, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map2_impl, environment(), ".x", ".y", ".f", "list")
}


map3 <- function (.x, .y, .z, .f, ...) 
{
    warning("`map3(x, y, z)` is deprecated. Please use `pmap(list(x, y, z))` ", 
        "instead", call. = FALSE)
    pmap(list(.x, .y, .z), .f, ...)
}


list_along <- function (x) 
{
    vector("list", length(x))
}


invoke_map <- function (.f, .x = list(NULL), ..., .env = NULL) 
{
    .env <- .env %||% lazyeval::expr_env(.f, parent.frame())
    .f <- as_invoke_function(.f)
    map2(.f, .x, invoke, ..., .env = .env)
}


invoke_map_lgl <- function (.f, .x = list(NULL), ..., .env = NULL) 
{
    .env <- .env %||% lazyeval::expr_env(.f, parent.frame())
    .f <- as_invoke_function(.f)
    map2_lgl(.f, .x, invoke, ..., .env = .env)
}


every <- function (.x, .p, ...) 
{
    .p <- as_function(.p, ...)
    for (i in seq_along(.x)) {
        if (!isTRUE(.p(.x[[i]], ...))) 
            return(FALSE)
    }
    TRUE
}


compose <- function (...) 
{
    fs <- lapply(list(...), match.fun)
    n <- length(fs)
    last <- fs[[n]]
    rest <- fs[-n]
    function(...) {
        out <- last(...)
        for (f in rev(rest)) {
            out <- f(out)
        }
        out
    }
}


possibly <- function (.f, otherwise, quiet = TRUE) 
{
    .f <- as_function(.f)
    force(otherwise)
    function(...) {
        tryCatch(.f(...), error = function(e) {
            if (!quiet) 
                message("Error: ", e$message)
            otherwise
        })
    }
}


`%@%` <- function (x, name) 
attr(x, name, exact = TRUE)


head_while <- function (.x, .p, ...) 
{
    loc <- detect_index(.x, negate(.p), ...)
    if (loc == 0) 
        return(.x)
    .x[seq_len(loc - 1)]
}


walk <- function (.x, .f, ...) 
{
    .f <- as_function(.f, ...)
    for (i in seq_along(.x)) {
        .f(.x[[i]], ...)
    }
    invisible(.x)
}


is_scalar_atomic <- function (x) 
{
    is_atomic(x) && length(x) == 1
}


flatmap <- function (.x, .f, ..., .type) 
{
    warning("`flatmap()` has been deprecated. Please use `map()` + `flatten()", 
        call. = FALSE)
    out <- map(.x, .f = .f, ...)
    flatten(out)
}


dmap_at <- function (.d, .at, .f, ...) 
{
    sel <- inv_which(.d, .at)
    partial_dmap(.d, sel, .f, ...)
}


simplify <- function (.x, .type = NULL) 
{
    if (can_simplify(.x, .type)) {
        unlist(.x)
    }
    else {
        .x
    }
}


map <- function (.x, .f, ...) 
{
    .f <- as_function(.f, ...)
    .Call(map_impl, environment(), ".x", ".f", "list")
}


at_depth <- function (.x, .depth, .f, ...) 
{
    .f <- as_function(.f, ...)
    recurse <- function(x, depth) {
        if (depth > 1) {
            if (is.atomic(x)) {
                stop("List not deep enough", call. = FALSE)
            }
            lapply(x, recurse, depth = depth - 1)
        }
        else {
            lapply(x, .f, ...)
        }
    }
    if (.depth == 0) {
        .f(.x, ...)
    }
    else if (.depth > 0) {
        recurse(.x, .depth)
    }
    else {
        stop(".depth cannot be negative", call. = FALSE)
    }
}


detect <- function (.x, .p, ..., .right = FALSE) 
{
    .p <- as_function(.p, ...)
    for (i in index(.x, .right)) {
        if (isTRUE(.p(.x[[i]], ...))) 
            return(.x[[i]])
    }
    NULL
}


tail_while <- function (.x, .p, ...) 
{
    loc <- detect_index(.x, negate(.p), ..., .right = TRUE)
    if (loc == 0) 
        return(.x)
    .x[-seq_len(loc)]
}


rep_along <- function (x, y) 
{
    rep(y, length.out = length(x))
}


is_bare_logical <- function (x) 
{
    !is.object(x) && is_logical(x)
}


dmap <- function (.d, .f, ...) 
{
    .f <- as_function(.f, ...)
    if (dplyr::is.grouped_df(.d)) {
        sliced_dmap(.d, .f, ...)
    }
    else {
        res <- .Call(map_impl, environment(), ".d", ".f", "list")
        dplyr::as_data_frame(res)
    }
}


reduce_right <- function (.x, .f, ..., .init) 
{
    .f <- as_function(.f, ...)
    f <- function(x, y) {
        .f(y, x, ...)
    }
    Reduce(f, .x, init = .init, right = TRUE)
}


is_scalar_character <- function (x) 
{
    is_character(x) && length(x) == 1
}


is_scalar_integer <- function (x) 
{
    is_integer(x) && length(x) == 1
}


pwalk <- function (.l, .f, ...) 
{
    .f <- as_function(.f, ...)
    args_list <- recycle_args(.l) %>% transpose()
    for (args in args_list) {
        do.call(".f", c(args, list(...)))
    }
    invisible(.l)
}


cross_d <- function (.l, .filter = NULL) 
{
    cross_n(.l, .filter = .filter) %>% transpose() %>% simplify_all() %>% 
        dplyr::as_data_frame()
}


flatten_lgl <- function (.x) 
{
    .Call(vflatten_impl, .x, "logical")
}


cross2 <- function (.x, .y, .filter = NULL) 
{
    cross_n(list(.x, .y), .filter = .filter)
}


cross3 <- function (.x, .y, .z, .filter = NULL) 
{
    cross_n(list(.x, .y, .z), .filter = .filter)
}


invoke_rows <- function (.f, .d, ..., .collate = c("list", "rows", "cols"), 
    .to = ".out", .labels = TRUE) 
{
    check_df_consistency(.d)
    .collate <- match.arg(.collate)
    .unique_labels <- 0
    .labels_cols <- .d
    .slicing_cols <- .d
    .Call(invoke_rows_impl, environment(), ".d", ".f")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Functional Programming Tools"

.skeleton_package_version = "0.2.2"

.skeleton_package_depends = ""

.skeleton_package_imports = "magrittr,dplyr,Rcpp,lazyeval"


## Internal

.skeleton_version = 5


## EOF