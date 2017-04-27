##
## Exported symobls in package `foreach`
##

## Exported package methods

getResult <- function (obj, ...) 
{
    UseMethod("getResult")
}


getDoParVersion <- function () 
{
    if (exists("info", where = .foreachGlobals, inherits = FALSE)) 
        .foreachGlobals$info(.foreachGlobals$data, "version")
    else NULL
}


registerDoSEQ <- function () 
{
    setDoPar(doSEQ, NULL, info)
    setDoSeq(doSEQ, NULL, info)
}


`%dopar%` <- function (obj, ex) 
{
    e <- getDoPar()
    e$fun(obj, substitute(ex), parent.frame(), e$data)
}


getDoParName <- function () 
{
    if (exists("info", where = .foreachGlobals, inherits = FALSE)) 
        .foreachGlobals$info(.foreachGlobals$data, "name")
    else NULL
}


getDoSeqVersion <- function () 
{
    if (exists("seqInfo", where = .foreachGlobals, inherits = FALSE)) 
        .foreachGlobals$seqInfo(.foreachGlobals$seqData, "version")
    else NULL
}


`%do%` <- function (obj, ex) 
{
    e <- getDoSeq()
    e$fun(obj, substitute(ex), parent.frame(), e$data)
}


times <- function (n) 
{
    if (!is.numeric(n) || length(n) != 1) 
        stop("n must be a numeric value")
    foreach(icount(n), .combine = "c")
}


getDoSeqRegistered <- function () 
{
    exists("seqFun", where = .foreachGlobals, inherits = FALSE)
}


getErrorValue <- function (obj, ...) 
{
    UseMethod("getErrorValue")
}


when <- function (cond) 
{
    obj <- list(qcond = substitute(cond), evalenv = parent.frame())
    class(obj) <- "foreachCondition"
    obj
}


setDoSeq <- function (fun, data = NULL, info = function(data, item) NULL) 
{
    tryCatch({
        assign("seqFun", fun, pos = .foreachGlobals, inherits = FALSE)
        assign("seqData", data, pos = .foreachGlobals, inherits = FALSE)
        assign("seqInfo", info, pos = .foreachGlobals, inherits = FALSE)
    }, error = function(e) {
        if (exists("fun", where = .foreachGlobals, inherits = FALSE)) 
            remove("fun", envir = .foreachGlobals)
        if (exists("data", where = .foreachGlobals, inherits = FALSE)) 
            remove("data", envir = .foreachGlobals)
        if (exists("info", where = .foreachGlobals, inherits = FALSE)) 
            remove("info", envir = .foreachGlobals)
        e
    })
}


getDoSeqName <- function () 
{
    if (exists("seqInfo", where = .foreachGlobals, inherits = FALSE)) 
        .foreachGlobals$seqInfo(.foreachGlobals$seqData, "name")
    else NULL
}


getexports <- function (ex, e, env, good = character(0), bad = character(0)) 
{
    syms <- getsyms(ex)
    syms <- expandsyms(syms, env, good, bad)
    for (s in syms) {
        if (s != "...") {
            val <- get(s, env, inherits = FALSE)
            fenv <- environment(val)
            if (is.function(val) && (identical(fenv, env) || 
                identical(fenv, .GlobalEnv))) 
                environment(val) <- e
            assign(s, val, e)
        }
    }
    invisible(NULL)
}


setDoPar <- function (fun, data = NULL, info = function(data, item) NULL) 
{
    tryCatch({
        assign("fun", fun, pos = .foreachGlobals, inherits = FALSE)
        assign("data", data, pos = .foreachGlobals, inherits = FALSE)
        assign("info", info, pos = .foreachGlobals, inherits = FALSE)
    }, error = function(e) {
        if (exists("fun", where = .foreachGlobals, inherits = FALSE)) 
            remove("fun", envir = .foreachGlobals)
        if (exists("data", where = .foreachGlobals, inherits = FALSE)) 
            remove("data", envir = .foreachGlobals)
        if (exists("info", where = .foreachGlobals, inherits = FALSE)) 
            remove("info", envir = .foreachGlobals)
        e
    })
}


getDoParWorkers <- function () 
{
    wc <- if (exists("info", where = .foreachGlobals, inherits = FALSE)) 
        .foreachGlobals$info(.foreachGlobals$data, "workers")
    else NULL
    if (is.null(wc)) 
        1L
    else wc
}


getDoSeqWorkers <- function () 
{
    wc <- if (exists("seqInfo", where = .foreachGlobals, inherits = FALSE)) 
        .foreachGlobals$seqInfo(.foreachGlobals$seqData, "workers")
    else NULL
    if (is.null(wc)) 
        1L
    else wc
}


foreach <- function (..., .combine, .init, .final = NULL, .inorder = TRUE, 
    .multicombine = FALSE, .maxcombine = if (.multicombine) 100 else 2, 
    .errorhandling = c("stop", "remove", "pass"), .packages = NULL, 
    .export = NULL, .noexport = NULL, .verbose = FALSE) 
{
    if (missing(.combine)) {
        if (!missing(.init)) 
            stop("if .init is specified, then .combine must also be specified")
        .combine <- defcombine
        hasInit <- TRUE
        init <- quote(list())
    }
    else {
        .combine <- match.fun(.combine)
        if (missing(.init)) {
            hasInit <- FALSE
            init <- NULL
        }
        else {
            hasInit <- TRUE
            init <- substitute(.init)
        }
    }
    if (missing(.multicombine) && (identical(.combine, cbind) || 
        identical(.combine, rbind) || identical(.combine, c) || 
        identical(.combine, defcombine))) 
        .multicombine <- TRUE
    if (!is.null(.final) && !is.function(.final)) 
        stop(".final must be a function")
    if (!is.logical(.inorder) || length(.inorder) > 1) 
        stop(".inorder must be a logical value")
    if (!is.logical(.multicombine) || length(.multicombine) > 
        1) 
        stop(".multicombine must be a logical value")
    if (!is.numeric(.maxcombine) || length(.maxcombine) > 1 || 
        .maxcombine < 2) 
        stop(".maxcombine must be a numeric value >= 2")
    if (!is.character(.errorhandling)) 
        stop(".errorhandling must be a character string")
    if (!is.null(.packages) && !is.character(.packages)) 
        stop(".packages must be a character vector")
    if (!is.null(.export) && !is.character(.export)) 
        stop(".export must be a character vector")
    if (!is.null(.noexport) && !is.character(.noexport)) 
        stop(".noexport must be a character vector")
    if (!is.logical(.verbose) || length(.verbose) > 1) 
        stop(".verbose must be a logical value")
    specified <- c("errorHandling", "verbose")
    specified <- specified[c(!missing(.errorhandling), !missing(.verbose))]
    args <- substitute(list(...))[-1]
    if (length(args) == 0) 
        stop("no iteration arguments specified")
    argnames <- names(args)
    if (is.null(argnames)) 
        argnames <- rep("", length(args))
    options <- list()
    opts <- grep("^\\.options\\.[A-Za-z][A-Za-z]*$", argnames)
    if (length(opts) > 0) {
        for (i in opts) {
            bname <- substr(argnames[i], 10, 100)
            options[[bname]] <- list(...)[[i]]
        }
        args <- args[-opts]
        argnames <- argnames[-opts]
    }
    unrecog <- grep("^\\.", argnames)
    if (length(unrecog) > 0) 
        stop(sprintf("unrecognized argument(s): %s", paste(argnames[unrecog], 
            collapse = ", ")))
    oldargs <- c("COMBINE", "INIT", "INORDER", "MULTICOMBINE", 
        "MAXCOMBINE", "ERRORHANDLING", "PACKAGES", "VERBOSE", 
        "EXPORT", "NOEXPORT", "LOADFACTOR", "CHUNKSIZE")
    oldused <- argnames %in% oldargs
    if (any(oldused)) 
        stop(sprintf("old style argument(s) specified: %s", paste(argnames[oldused], 
            collapse = ", ")))
    .errorhandling <- match.arg(.errorhandling)
    combineInfo <- list(fun = .combine, in.order = .inorder, 
        has.init = hasInit, init = init, final = .final, multi.combine = .multicombine, 
        max.combine = .maxcombine)
    iterable <- list(args = args, argnames = argnames, evalenv = parent.frame(), 
        specified = specified, combineInfo = combineInfo, errorHandling = .errorhandling, 
        packages = .packages, export = .export, noexport = .noexport, 
        options = options, verbose = .verbose)
    class(iterable) <- "foreach"
    iterable
}


accumulate <- function (obj, result, tag, ...) 
{
    UseMethod("accumulate")
}


getErrorIndex <- function (obj, ...) 
{
    UseMethod("getErrorIndex")
}


makeAccum <- function (it) 
{
    function(results, tags) {
        if (identical(it$error.handling, "stop") && !is.null(it$state$errorValue)) 
            return(invisible(NULL))
        for (i in seq(along = tags)) {
            if (it$verbose) 
                cat(sprintf("got results for task %d\n", tags[i]))
            accumulate(it, results[[i]], tags[i])
        }
    }
}


`%:%` <- function (e1, e2) 
{
    if (!inherits(e1, "foreach")) 
        stop("\"%:%\" was passed an illegal right operand")
    if (inherits(e2, "foreach")) 
        makeMerged(e1, e2)
    else if (inherits(e2, "foreachCondition")) 
        makeFiltered(e1, e2)
    else stop("\"%:%\" was passed an illegal right operand")
}


getDoParRegistered <- function () 
{
    exists("fun", where = .foreachGlobals, inherits = FALSE)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Provides Foreach Looping Construct for R"

.skeleton_package_version = "1.4.3"

.skeleton_package_depends = ""

.skeleton_package_imports = "codetools,utils,iterators"


## Internal

.skeleton_version = 5


## EOF