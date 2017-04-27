##
## Exported symobls in package `lazyeval`
##

## Exported package methods

f_interp <- function (f, data = NULL) 
{
    f_rhs(f) <- .Call(interp_, f_rhs(f), f_env(f), data)
    f
}


uqs <- function (x) 
{
    if (!is_vector(x)) {
        stop("`x` must be a vector")
    }
    as.pairlist(x)
}


as.lazy_dots <- function (x, env) 
UseMethod("as.lazy_dots")


f_eval_rhs <- function (f, data = NULL) 
{
    if (!is_formula(f)) {
        stop("`f` is not a formula", call. = FALSE)
    }
    expr <- f_rhs(f_interp(f, data = data))
    eval_expr(expr, f_env(f), data)
}


all_dots <- function (.dots, ..., all_named = FALSE) 
{
    dots <- as.lazy_dots(list(...))
    if (!missing(.dots)) {
        dots2 <- as.lazy_dots(.dots)
        dots <- c(dots, dots2)
    }
    if (all_named) {
        dots <- auto_name(dots)
    }
    dots
}


is_atomic <- function (x) 
{
    typeof(x) %in% c("logical", "integer", "double", "complex", 
        "character", "raw")
}


f_eval_lhs <- function (f, data = NULL) 
{
    if (!is_formula(f)) {
        stop("`f` is not a formula", call. = FALSE)
    }
    expr <- f_lhs(f_interp(f, data = data))
    eval_expr(expr, f_env(f), data)
}


as_f_list <- function (x) 
{
    .Call(lhs_name, x)
}


ast <- function (x) 
ast_(expr_find(x))


is_pairlist <- function (x) 
{
    typeof(x) == "pairlist"
}


expr_text <- function (x, width = 60L, nlines = Inf) 
{
    expr_text_(expr_find(x), width = width, nlines = nlines)
}


expr_find <- function (x) 
{
    .Call(expr_find_, quote(x), environment())
}


missing_arg <- function () 
{
    quote(expr = )
}


function_new <- function (args, body, env = parent.frame()) 
{
    stopifnot(all(has_names(args)), is_lang(body), is.environment(env))
    args <- as.pairlist(args)
    eval(call("function", args, body), env)
}


f_eval <- function (f, data = NULL) 
{
    if (!is_formula(f)) {
        stop("`f` is not a formula", call. = FALSE)
    }
    expr <- f_rhs(f_interp(f, data = data))
    eval_expr(expr, f_env(f), data)
}


is_call <- function (x) 
{
    typeof(x) == "language"
}


expr_label <- function (x) 
{
    expr_label_(expr_find(x))
}


f_label <- function (x) 
{
    expr_label_(f_rhs(x))
}


lazy_eval <- function (x, data = NULL) 
{
    if (is.lazy_dots(x)) {
        return(lapply(x, lazy_eval, data = data))
    }
    x <- as.lazy(x)
    if (!is.null(data)) {
        eval(x$expr, data, x$env)
    }
    else {
        eval(x$expr, x$env, emptyenv())
    }
}


`f_env<-` <- function (x, value) 
{
    stopifnot(is_formula(x))
    f_new(f_rhs(x), f_lhs(x), value)
}


ast_ <- function (x, width = getOption("width")) 
{
    if (is.expression(x) || is.list(x)) {
        trees <- vapply(x, tree, character(1), width = width)
        out <- paste0(trees, collapse = "\n\n")
    }
    else {
        out <- tree(x, width = width)
    }
    cat(out, "\n")
}


lazy_ <- function (expr, env) 
{
    stopifnot(is.call(expr) || is.name(expr) || is.atomic(expr))
    structure(list(expr = expr, env = env), class = "lazy")
}


interp <- function (`_obj`, ..., .values) 
{
    UseMethod("interp")
}


f_unwrap <- function (f) 
{
    stopifnot(is_formula(f))
    e <- environment(f)
    if (identical(e, emptyenv())) {
        f
    }
    else {
        f_new(substitute_(f_rhs(f), e), f_lhs(f), parent.env(e))
    }
}


uq <- function (x, data = NULL) 
{
    if (is_formula(x)) {
        if (is.null(data)) {
            f_rhs(f_interp(x))
        }
        else {
            f_eval(x, data = data)
        }
    }
    else {
        x
    }
}


is_formula <- function (x) 
{
    typeof(x) == "language" && inherits(x, "formula")
}


find_data <- function (x) 
UseMethod("find_data")


f_list <- function (...) 
{
    .Call(lhs_name, list(...))
}


f_text <- function (x, width = 60L, nlines = Inf) 
{
    expr_text_(f_rhs(x), width = width, nlines = nlines)
}


dots_capture <- function (..., .ignore_empty = TRUE) 
{
    lazies <- .Call(make_lazy_dots, environment(), TRUE, .ignore_empty)
    lapply(lazies, function(x) f_new(x$expr, env = x$env))
}


is_name <- function (x) 
{
    typeof(x) == "symbol"
}


as_name <- function (x) 
UseMethod("as_name")


call_standardise <- function (call, env = parent.frame()) 
{
    stopifnot(is_call(call))
    f <- eval(call[[1]], env)
    if (is.primitive(f)) 
        return(call)
    match.call(f, call)
}


auto_name <- function (x, max_width = 40) 
{
    names(x) <- auto_names(x, max_width = max_width)
    x
}


common_env <- function (dots) 
{
    if (!is.list(dots)) 
        stop("dots must be a list", call. = FALSE)
    if (length(dots) == 0) 
        return(baseenv())
    dots <- as.lazy_dots(dots)
    env <- dots[[1]]$env
    if (length(dots) == 1) 
        return(env)
    for (i in 2:length(dots)) {
        if (!identical(env, dots[[i]]$env)) {
            return(baseenv())
        }
    }
    env
}


expr_env <- function (x, default_env) 
{
    env <- .Call(expr_env_, quote(x), environment())
    if (is.null(env)) {
        if (missing(default_env)) {
            stop("Promise has already been forced")
        }
        else {
            default_env
        }
    }
    else {
        env
    }
}


f_env <- function (f) 
{
    .Call(env, f)
}


is_lang <- function (x) 
{
    is_call(x) || is_pairlist(x) || is_atomic(x) || is_name(x) || 
        is.null(x)
}


f_new <- function (rhs, lhs = NULL, env = parent.frame()) 
{
    if (!is_lang(rhs)) {
        stop("`rhs` must be a language object", call. = FALSE)
    }
    if (!is_lang(lhs) && !is.null(lhs)) {
        stop("`lhs` must be a language object", call. = FALSE)
    }
    if (!is.environment(env)) {
        stop("`env` must be an environment", call. = FALSE)
    }
    if (is.null(lhs)) {
        f <- call_new("~", rhs)
    }
    else {
        f <- call_new("~", lhs, rhs)
    }
    structure(f, class = "formula", .Environment = env)
}


lazy <- function (expr, env = parent.frame(), .follow_symbols = TRUE) 
{
    .Call(make_lazy, quote(expr), environment(), .follow_symbols)
}


as.lazy <- function (x, env = baseenv()) 
UseMethod("as.lazy")


make_call <- function (fun, args) 
{
    stopifnot(is.call(fun) || is.name(fun))
    args <- as.lazy_dots(args)
    expr <- lapply(args, `[[`, "expr")
    lazy_(as.call(c(fun, expr)), common_env(args))
}


as_call <- function (x) 
{
    UseMethod("as_call")
}


`f_lhs<-` <- function (x, value) 
{
    stopifnot(is_formula(x))
    f_new(f_rhs(x), value, f_env(x))
}


`f_rhs<-` <- function (x, value) 
{
    stopifnot(is_formula(x))
    f_new(value, f_lhs(x), f_env(x))
}


f_rhs <- function (f) 
{
    .Call(rhs, f)
}


f_lhs <- function (f) 
{
    .Call(lhs, f)
}


lazy_dots <- function (..., .follow_symbols = FALSE, .ignore_empty = FALSE) 
{
    .Call(make_lazy_dots, environment(), .follow_symbols, .ignore_empty)
}


f_capture <- function (x) 
{
    lazy <- .Call(make_lazy, quote(x), environment(), TRUE)
    f_new(lazy$expr, env = lazy$env)
}


uqf <- function (x) 
{
    if (!is_formula(x)) 
        stop("`x` must be a formula", call. = FALSE)
    x
}


call_new <- function (f, ..., .args = list()) 
{
    if (is.character(f)) {
        if (length(f) != 1) {
            stop("Character `f` must be length 1", call. = FALSE)
        }
        f <- as.name(f)
    }
    args <- c(list(...), as.list(.args))
    as.call(c(f, args))
}


call_modify <- function (call, new_args, env = parent.frame()) 
{
    stopifnot(is.call(call), is.list(new_args))
    call <- call_standardise(call, env)
    if (!all(has_names(new_args))) {
        stop("All new arguments must be named", call. = FALSE)
    }
    for (nm in names(new_args)) {
        call[[nm]] <- new_args[[nm]]
    }
    call
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Lazy (Non-Standard) Evaluation"

.skeleton_package_version = "0.2.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF