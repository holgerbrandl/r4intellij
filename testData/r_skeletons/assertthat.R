##
## Exported symobls in package `assertthat`
##

## Exported package methods

is.error <- function (x) 
inherits(x, "try-error")


`%has_args%` <- function (f, args) 
has_args(f, args)


are_equal <- function (x, y, ...) 
{
    isTRUE(all.equal(x, y, ...))
}


noNA <- function (x) 
{
    !(any(is.na(x)))
}


is.date <- function (x) 
inherits(x, "Date")


has_extension <- function (path, ext) 
{
    file_ext(path) == ext
}


has_attr <- function (x, which) 
!is.null(attr(x, which, exact = TRUE))


`on_failure<-` <- function (x, value) 
{
    stopifnot(is.function(x), identical(names(formals(value)), 
        c("call", "env")))
    attr(x, "fail") <- value
    x
}


is.number <- function (x) 
is.numeric(x) && length(x) == 1


on_failure <- function (x) 
attr(x, "fail")


is.writeable <- function (path) 
{
    assert_that(is.string(path), file.exists(path))
    file.access(path, mode = 2)[[1]] == 0
}


is.flag <- function (x) 
is.logical(x) && length(x) == 1


has_args <- function (f, args, exact = FALSE) 
{
    assert_that(is.function(f))
    if (exact) {
        identical(args, names(formals(f)))
    }
    else {
        all(args %in% names(formals(f)))
    }
}


assert_that <- function (..., env = parent.frame()) 
{
    res <- see_if(..., env = env)
    if (res) 
        return(TRUE)
    stop(assertError(attr(res, "msg")))
}


is.count <- function (x) 
{
    if (length(x) != 1) 
        return(FALSE)
    if (!is.integerish(x)) 
        return(FALSE)
    x > 0
}


validate_that <- function (..., env = parent.frame()) 
{
    res <- see_if(..., env = env)
    if (res) 
        return(TRUE)
    return(attr(res, "msg"))
}


is.dir <- function (path) 
{
    assert_that(is.string(path), file.exists(path))
    file.info(path)$isdir
}


is.time <- function (x) 
inherits(x, "POSIXt")


see_if <- function (..., env = parent.frame()) 
{
    asserts <- eval(substitute(alist(...)))
    for (assertion in asserts) {
        res <- tryCatch({
            eval(assertion, env)
        }, assertError = function(e) {
            structure(FALSE, msg = e$message)
        })
        check_result(res)
        if (!res) {
            msg <- get_message(res, assertion, env)
            return(structure(FALSE, msg = msg))
        }
    }
    res
}


is.scalar <- function (x) 
{
    length(x) == 1L
}


has_name <- function (x, which) 
which %in% names(x)


`%has_attr%` <- function (x, which) 
!is.null(attr(x, which, exact = TRUE))


is.readable <- function (path) 
{
    assert_that(is.string(path), file.exists(path))
    file.access(path, mode = 4)[[1]] == 0
}


is.string <- function (x) 
is.character(x) && length(x) == 1


not_empty <- function (x) 
{
    all((dim(x) %||% length(x)) != 0)
}


`%has_name%` <- function (x, which) 
which %in% names(x)




## Package Data

# none


## Package Info

.skeleton_package_title = "Easy pre and post assertions."

.skeleton_package_version = "0.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "tools"


## Internal

.skeleton_version = 5


## EOF