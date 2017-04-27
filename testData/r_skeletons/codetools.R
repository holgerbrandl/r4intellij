##
## Exported symobls in package `codetools`
##

## Exported package methods

collectLocals <- function (e, collect) 
{
    w <- makeLocalsCollector(collect = collect)
    walkCode(e, w)
}


findFuncLocals <- function (formals, body) 
findLocalsList(c(list(body), dropMissings(formals)))


walkCode <- function (e, w = makeCodeWalker()) 
{
    if (typeof(e) == "language") {
        if (typeof(e[[1]]) %in% c("symbol", "character")) {
            h <- w$handler(as.character(e[[1]]), w)
            if (!is.null(h)) 
                h(e, w)
            else w$call(e, w)
        }
        else w$call(e, w)
    }
    else w$leaf(e, w)
}


checkUsagePackage <- function (pack, ...) 
{
    pname <- paste("package", pack, sep = ":")
    if (!pname %in% search()) 
        stop("package must be loaded")
    if (pack %in% loadedNamespaces()) 
        checkUsageEnv(getNamespace(pack), ...)
    else checkUsageEnv(as.environment(pname), ...)
}


makeConstantFolder <- function (..., leaf = foldLeaf, handler = function(v, w) if (w$foldable(v, 
    w)) foldCall, call = function(e, w) exitFolder(e, w), exit = function(e, 
    w) stop0(paste("not a foldable expression:", deparse(e, width.cutoff = 500))), 
    isLocal = function(v, w) FALSE, foldable = isFoldable, isConstant = isConstantValue, 
    signal = function(e, msg, w) warning0(msg)) 
list(handler = handler, call = call, exit = exit, leaf = leaf, 
    isLocal = isLocal, foldable = isFoldable, isConstant = isConstant, 
    signal = signal, ...)


getAssignedVar <- function (e) 
{
    v <- e[[2]]
    if (missing(v)) 
        stop0(paste("bad assignment:", pasteExpr(e)))
    else if (typeof(v) %in% c("symbol", "character")) 
        as.character(v)
    else {
        while (typeof(v) == "language") {
            if (length(v) < 2) 
                stop0(paste("bad assignment:", pasteExpr(e)))
            v <- v[[2]]
            if (missing(v)) 
                stop0(paste("bad assignment:", pasteExpr(e)))
        }
        if (typeof(v) != "symbol") 
            stop0(paste("bad assignment:", pasteExpr(e)))
        as.character(v)
    }
}


collectUsage <- function (fun, name = "<anonymous>", ...) 
{
    w <- makeUsageCollector(fun, ...)
    collectUsageFun(name, formals(fun), body(fun), w)
}


makeCodeWalker <- function (..., handler = function(v, w) NULL, call = function(e, 
    w) for (ee in as.list(e)) if (!missing(ee)) walkCode(ee, 
    w), leaf = function(e, w) print(e)) 
list(handler = handler, call = call, leaf = leaf, ...)


makeUsageCollector <- function (fun, ..., name = NULL, enterLocal = doNothing, enterGlobal = doNothing, 
    enterInternal = doNothing, startCollectLocals = doNothing, 
    finishCollectLocals = doNothing, warn = warning0, signal = signalUsageIssue) 
{
    if (typeof(fun) != "closure") 
        stop("only works for closures")
    makeCodeWalker(..., name = name, enterLocal = enterLocal, 
        enterGlobal = enterGlobal, enterInternal = enterInternal, 
        startCollectLocals = startCollectLocals, finishCollectLocals = finishCollectLocals, 
        warn = warn, signal = signal, leaf = collectUsageLeaf, 
        call = collectUsageCall, handler = getCollectUsageHandler, 
        globalenv = environment(fun), env = environment(fun), 
        name = NULL, srcfile = NULL, frow = NULL, lrow = NULL, 
        isLocal = collectUsageIsLocal)
}


makeLocalsCollector <- function (..., leaf = function(e, w) character(0), handler = getCollectLocalsHandler, 
    isLocal = function(v, w) FALSE, exit = function(e, msg, w) stop0(msg), 
    collect = function(v, e, w) print(v)) 
makeCodeWalker(leaf = leaf, handler = handler, collect = collect, 
    isLocal = isLocal)


flattenAssignment <- function (e) 
{
    if (typeof(e) == "language") 
        list(evalseq(e[[2]]), apdef(e))
    else list(NULL, NULL)
}


checkUsageEnv <- function (env, ...) 
{
    for (n in ls(env, all.names = TRUE)) {
        v <- get(n, envir = env)
        if (typeof(v) == "closure") 
            checkUsage(v, name = n, ...)
    }
}


checkUsage <- function (fun, name = "<anonymous>", report = cat, all = FALSE, 
    suppressLocal = FALSE, suppressParamAssigns = !all, suppressParamUnused = !all, 
    suppressFundefMismatch = FALSE, suppressLocalUnused = FALSE, 
    suppressNoLocalFun = !all, skipWith = FALSE, suppressUndefined = dfltSuppressUndefined, 
    suppressPartialMatchArgs = TRUE) 
{
    if (is.null(getOption("warnPartialMatchArgs"))) 
        options(warnPartialMatchArgs = FALSE)
    if (!suppressPartialMatchArgs) {
        oldOpts <- options(warnPartialMatchArgs = TRUE)
        on.exit(options(oldOpts))
    }
    tryCatch(collectUsage(fun, name = name, warn = report, suppressLocal = suppressLocal, 
        suppressParamAssigns = suppressParamAssigns, suppressParamUnused = suppressParamUnused, 
        suppressFundefMismatch = suppressFundefMismatch, suppressLocalUnused = suppressLocalUnused, 
        suppressNoLocalFun = suppressNoLocalFun, skipWith = skipWith, 
        enterGlobal = checkUsageEnterGlobal, enterLocal = checkUsageEnterLocal, 
        startCollectLocals = checkUsageStartLocals, finishCollectLocals = checkUsageFinishLocals, 
        suppressUndefined = suppressUndefined, suppressPartialMatchArgs = suppressPartialMatchArgs), 
        error = function(e) {
            report(paste0(name, ": Error while checking: ", conditionMessage(e), 
                "\n"))
        })
    invisible(NULL)
}


constantFold <- function (e, env = NULL, fail = NULL) 
{
    job <- function(exit) {
        isLocal <- function(v, w) as.character(v) %in% env
        doExit <- function(e, w) exit(fail)
        w <- makeConstantFolder(isLocal = isLocal, exit = doExit)
        walkCode(e, w)
    }
    callCC(job)
}


findLocals <- function (e, envir = .BaseEnv) 
findLocalsList(list(e), envir)


isConstantValue <- function (v, w) 
is.null(v) || (is.null(attributes(v)) && is.atomic(v)) || (is.list(v) && 
    (identical(v, .Platform) || identical(v, .Machine)))


showTree <- function (e, write = cat) 
{
    w <- makeCodeWalker(call = showTreeCall, leaf = showTreeLeaf, 
        write = write)
    walkCode(e, w)
    w$write("\n")
}


findLocalsList <- function (elist, envir = .BaseEnv) 
{
    localStopFuns <- c("expression", "quote", "Quote", "local")
    if (is.character(envir)) 
        locals <- envir
    else locals <- localStopFuns[!sapply(localStopFuns, isBaseVar, 
        envir)]
    specialSyntaxFuns <- c("~", "<-", "=", "for", "function")
    sf <- unique(c(locals, localStopFuns))
    nsf <- length(sf)
    collect <- function(v, e, w) assign(v, TRUE, envir = env)
    isLocal <- function(v, w) as.character(v) %in% sf
    w <- makeLocalsCollector(collect = collect, isLocal = isLocal)
    repeat {
        env <- mkHash()
        for (e in elist) walkCode(e, w)
        isloc <- sapply(sf, exists, envir = env, inherits = FALSE)
        last.nsf <- nsf
        sf <- unique(c(locals, sf[isloc]))
        nsf <- length(sf)
        if (last.nsf == nsf) {
            vals <- ls(env, all.names = TRUE)
            rdsf <- vals %in% specialSyntaxFuns
            if (any(rdsf)) 
                warning0(paste("local assignments to syntactic functions:", 
                  vals[rdsf]))
            return(vals)
        }
    }
}


findGlobals <- function (fun, merge = TRUE) 
{
    vars <- mkHash()
    funs <- mkHash()
    enter <- function(type, v, e, w) if (type == "function") 
        assign(v, TRUE, funs)
    else assign(v, TRUE, vars)
    collectUsage(fun, enterGlobal = enter)
    fnames <- ls(funs, all.names = TRUE)
    vnames <- ls(vars, all.names = TRUE)
    if (merge) 
        sort(unique(c(vnames, fnames)))
    else list(functions = fnames, variables = vnames)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Code Analysis Tools for R"

.skeleton_package_version = "0.2-14"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF