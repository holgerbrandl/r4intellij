##
## Exported symobls in package `memoise`
##

## Exported package methods

forget <- function (f) 
{
    if (!is.memoised(f)) {
        return(FALSE)
    }
    env <- environment(f)
    if (!exists("_cache", env, inherits = FALSE)) 
        return(FALSE)
    cache <- get("_cache", env)
    cache$reset()
    TRUE
}


timeout <- function (seconds, current = as.numeric(Sys.time())) 
{
    (current - current%%seconds)%/%seconds
}


memoize <- function (f, ..., envir = environment(f)) 
{
    f_formals <- formals(args(f))
    if (is.memoised(f)) {
        stop("`f` must not be memoised.", call. = FALSE)
    }
    f_formal_names <- names(f_formals)
    f_formal_name_list <- lapply(f_formal_names, as.name)
    list_call <- make_call(quote(list), f_formal_name_list)
    init_call_args <- setNames(f_formal_name_list, f_formal_names)
    init_call <- make_call(quote(`_f`), init_call_args)
    cache <- new_cache()
    validate_formulas(...)
    additional <- list(...)
    memo_f <- eval(bquote(function(...) {
        hash <- `_digest`(c(.(list_call), lapply(`_additional`, 
            function(x) eval(x[[2L]], environment(x)))), algo = "sha512")
        if (`_cache`$has_key(hash)) {
            res <- `_cache`$get(hash)
        }
        else {
            res <- withVisible(.(init_call))
            `_cache`$set(hash, res)
        }
        if (res$visible) {
            res$value
        }
        else {
            invisible(res$value)
        }
    }, as.environment(list(list_call = list_call, init_call = init_call))))
    formals(memo_f) <- f_formals
    attr(memo_f, "memoised") <- TRUE
    if (is.null(envir)) {
        envir <- baseenv()
    }
    memo_f_env <- new.env(parent = envir)
    memo_f_env$`_cache` <- cache
    memo_f_env$`_f` <- f
    memo_f_env$`_digest` <- digest
    memo_f_env$`_additional` <- additional
    environment(memo_f) <- memo_f_env
    class(memo_f) <- c("memoised", "function")
    memo_f
}


is.memoised <- function (f) 
{
    is.function(f) && inherits(f, "memoised")
}


memoise <- function (f, ..., envir = environment(f)) 
{
    f_formals <- formals(args(f))
    if (is.memoised(f)) {
        stop("`f` must not be memoised.", call. = FALSE)
    }
    f_formal_names <- names(f_formals)
    f_formal_name_list <- lapply(f_formal_names, as.name)
    list_call <- make_call(quote(list), f_formal_name_list)
    init_call_args <- setNames(f_formal_name_list, f_formal_names)
    init_call <- make_call(quote(`_f`), init_call_args)
    cache <- new_cache()
    validate_formulas(...)
    additional <- list(...)
    memo_f <- eval(bquote(function(...) {
        hash <- `_digest`(c(.(list_call), lapply(`_additional`, 
            function(x) eval(x[[2L]], environment(x)))), algo = "sha512")
        if (`_cache`$has_key(hash)) {
            res <- `_cache`$get(hash)
        }
        else {
            res <- withVisible(.(init_call))
            `_cache`$set(hash, res)
        }
        if (res$visible) {
            res$value
        }
        else {
            invisible(res$value)
        }
    }, as.environment(list(list_call = list_call, init_call = init_call))))
    formals(memo_f) <- f_formals
    attr(memo_f, "memoised") <- TRUE
    if (is.null(envir)) {
        envir <- baseenv()
    }
    memo_f_env <- new.env(parent = envir)
    memo_f_env$`_cache` <- cache
    memo_f_env$`_f` <- f
    memo_f_env$`_digest` <- digest
    memo_f_env$`_additional` <- additional
    environment(memo_f) <- memo_f_env
    class(memo_f) <- c("memoised", "function")
    memo_f
}


has_cache <- function (f, ...) 
{
    if (!is.memoised(f)) 
        stop("`f` is not a memoised function!", call. = FALSE)
    body <- body(f)
    body[[3]] <- quote(if (`_cache`$has_key(hash)) return(TRUE) else return(FALSE))
    body(f) <- body
    f
}


is.memoized <- function (f) 
{
    is.function(f) && inherits(f, "memoised")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Memoisation of Functions"

.skeleton_package_version = "1.0.0"

.skeleton_package_depends = ""

.skeleton_package_imports = "digest"


## Internal

.skeleton_version = 5


## EOF