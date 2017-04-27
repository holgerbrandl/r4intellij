##
## Exported symobls in package `magrittr`
##

## Exported package methods

multiply_by_matrix <- function (x, y)  .Primitive("%*%")


raise_to_power <- function (e1, e2)  .Primitive("^")


inset2 <- .Primitive("[[<-")


extract <- .Primitive("[")


divide_by_int <- function (e1, e2)  .Primitive("%/%")


`%$%` <- function (lhs, rhs) 
{
    parent <- parent.frame()
    env <- new.env(parent = parent)
    chain_parts <- split_chain(match.call(), env = env)
    pipes <- chain_parts[["pipes"]]
    rhss <- chain_parts[["rhss"]]
    lhs <- chain_parts[["lhs"]]
    env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
        pipes[[i]], parent))
    env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value, 
        `_function_list`)), env, env), c("fseq", "function"))
    env[["freduce"]] <- freduce
    if (is_placeholder(lhs)) {
        env[["_fseq"]]
    }
    else {
        env[["_lhs"]] <- eval(lhs, parent, parent)
        result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, 
            env))
        if (is_compound_pipe(pipes[[1L]])) {
            eval(call("<-", lhs, result[["value"]]), parent, 
                parent)
        }
        else {
            if (result[["visible"]]) 
                result[["value"]]
            else invisible(result[["value"]])
        }
    }
}


`%T>%` <- function (lhs, rhs) 
{
    parent <- parent.frame()
    env <- new.env(parent = parent)
    chain_parts <- split_chain(match.call(), env = env)
    pipes <- chain_parts[["pipes"]]
    rhss <- chain_parts[["rhss"]]
    lhs <- chain_parts[["lhs"]]
    env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
        pipes[[i]], parent))
    env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value, 
        `_function_list`)), env, env), c("fseq", "function"))
    env[["freduce"]] <- freduce
    if (is_placeholder(lhs)) {
        env[["_fseq"]]
    }
    else {
        env[["_lhs"]] <- eval(lhs, parent, parent)
        result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, 
            env))
        if (is_compound_pipe(pipes[[1L]])) {
            eval(call("<-", lhs, result[["value"]]), parent, 
                parent)
        }
        else {
            if (result[["visible"]]) 
                result[["value"]]
            else invisible(result[["value"]])
        }
    }
}


is_less_than <- function (e1, e2)  .Primitive("<")


undebug_fseq <- function (fseq) 
{
    for (i in 1:length(functions(fseq))) if (isdebugged(functions(fseq)[[i]])) 
        undebug(functions(fseq)[[i]])
}


multiply_by <- function (e1, e2)  .Primitive("*")


or <- function (e1, e2)  .Primitive("|")


use_series <- .Primitive("$")


equals <- function (e1, e2)  .Primitive("==")


set_rownames <- function (x, value) 
{
    if (is.data.frame(x)) {
        row.names(x) <- value
    }
    else {
        dn <- dimnames(x)
        if (is.null(dn)) {
            if (is.null(value)) 
                return(x)
            if ((nd <- length(dim(x))) < 1L) 
                stop("attempt to set 'rownames' on an object with no dimensions")
            dn <- vector("list", nd)
        }
        if (length(dn) < 1L) 
            stop("attempt to set 'rownames' on an object with no dimensions")
        if (is.null(value)) 
            dn[1L] <- list(NULL)
        else dn[[1L]] <- value
        dimnames(x) <- dn
    }
    x
}


not <- function (x)  .Primitive("!")


add <- function (e1, e2)  .Primitive("+")


inset <- .Primitive("[<-")


and <- function (e1, e2)  .Primitive("&")


debug_pipe <- function (x) 
{
    browser()
    x
}


divide_by <- function (e1, e2)  .Primitive("/")


debug_fseq <- function (fseq, ...) 
{
    is_valid_index <- function(i) i %in% 1:length(functions(fseq))
    indices <- list(...)
    if (!any(vapply(indices, is.numeric, logical(1))) || !any(vapply(indices, 
        is_valid_index, logical(1)))) 
        stop("Index or indices invalid.", call. = FALSE)
    invisible(lapply(indices, function(i) debug(functions(fseq)[[i]])))
}


set_names <- function (x, value)  .Primitive("names<-")


mod <- function (e1, e2)  .Primitive("%%")


`%>%` <- function (lhs, rhs) 
{
    parent <- parent.frame()
    env <- new.env(parent = parent)
    chain_parts <- split_chain(match.call(), env = env)
    pipes <- chain_parts[["pipes"]]
    rhss <- chain_parts[["rhss"]]
    lhs <- chain_parts[["lhs"]]
    env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
        pipes[[i]], parent))
    env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value, 
        `_function_list`)), env, env), c("fseq", "function"))
    env[["freduce"]] <- freduce
    if (is_placeholder(lhs)) {
        env[["_fseq"]]
    }
    else {
        env[["_lhs"]] <- eval(lhs, parent, parent)
        result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, 
            env))
        if (is_compound_pipe(pipes[[1L]])) {
            eval(call("<-", lhs, result[["value"]]), parent, 
                parent)
        }
        else {
            if (result[["visible"]]) 
                result[["value"]]
            else invisible(result[["value"]])
        }
    }
}


`%<>%` <- function (lhs, rhs) 
{
    parent <- parent.frame()
    env <- new.env(parent = parent)
    chain_parts <- split_chain(match.call(), env = env)
    pipes <- chain_parts[["pipes"]]
    rhss <- chain_parts[["rhss"]]
    lhs <- chain_parts[["lhs"]]
    env[["_function_list"]] <- lapply(1:length(rhss), function(i) wrap_function(rhss[[i]], 
        pipes[[i]], parent))
    env[["_fseq"]] <- `class<-`(eval(quote(function(value) freduce(value, 
        `_function_list`)), env, env), c("fseq", "function"))
    env[["freduce"]] <- freduce
    if (is_placeholder(lhs)) {
        env[["_fseq"]]
    }
    else {
        env[["_lhs"]] <- eval(lhs, parent, parent)
        result <- withVisible(eval(quote(`_fseq`(`_lhs`)), env, 
            env))
        if (is_compound_pipe(pipes[[1L]])) {
            eval(call("<-", lhs, result[["value"]]), parent, 
                parent)
        }
        else {
            if (result[["visible"]]) 
                result[["value"]]
            else invisible(result[["value"]])
        }
    }
}


extract2 <- .Primitive("[[")


`n'est pas` <- function (x)  .Primitive("!")


set_colnames <- function (x, value) 
{
    if (is.data.frame(x)) {
        names(x) <- value
    }
    else {
        dn <- dimnames(x)
        if (is.null(dn)) {
            if (is.null(value)) 
                return(x)
            if ((nd <- length(dim(x))) < 2L) 
                stop("attempt to set 'colnames' on an object with less than two dimensions")
            dn <- vector("list", nd)
        }
        if (length(dn) < 2L) 
            stop("attempt to set 'colnames' on an object with less than two dimensions")
        if (is.null(value)) 
            dn[2L] <- list(NULL)
        else dn[[2L]] <- value
        dimnames(x) <- dn
    }
    x
}


subtract <- function (e1, e2)  .Primitive("-")


freduce <- function (value, function_list) 
{
    k <- length(function_list)
    if (k > 1) {
        for (i in 1:(k - 1L)) {
            value <- function_list[[i]](value)
        }
    }
    value <- withVisible(function_list[[k]](value))
    if (value[["visible"]]) 
        value[["value"]]
    else invisible(value[["value"]])
}


is_weakly_less_than <- function (e1, e2)  .Primitive("<=")


is_in <- function (x, table) 
match(x, table, nomatch = 0L) > 0L


is_weakly_greater_than <- function (e1, e2)  .Primitive(">=")


functions <- function (fseq) 
{
    if (!"fseq" %in% class(fseq)) 
        stop("Object is not a functional sequence.", call. = FALSE)
    environment(fseq)[["_function_list"]]
}


is_greater_than <- function (e1, e2)  .Primitive(">")




## Package Data

# none


## Package Info

.skeleton_package_title = "A Forward-Pipe Operator for R"

.skeleton_package_version = "1.5"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF