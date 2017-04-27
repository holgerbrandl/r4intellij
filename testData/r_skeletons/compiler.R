##
## Exported symobls in package `compiler`
##

## Exported package methods

compile <- function (e, env = .GlobalEnv, options = NULL) 
{
    cenv <- makeCenv(env)
    cntxt <- make.toplevelContext(cenv, options)
    cntxt$env <- addCenvVars(cenv, findLocals(e, cntxt))
    genCode(e, cntxt)
}


cmpfun <- function (f, options = NULL) 
{
    type <- typeof(f)
    if (type == "closure") {
        cntxt <- make.toplevelContext(makeCenv(environment(f)), 
            options)
        ncntxt <- make.functionContext(cntxt, formals(f), body(f))
        b <- genCode(body(f), ncntxt)
        val <- .Internal(bcClose(formals(f), b, environment(f)))
        attrs <- attributes(f)
        if (!is.null(attrs)) 
            attributes(val) <- attrs
        if (isS4(f)) 
            val <- asS4(val)
        val
    }
    else if (type == "builtin" || type == "special") 
        f
    else stop("cannot compile a non-function")
}


loadcmp <- function (file, envir = .GlobalEnv, chdir = FALSE) 
{
    if (!(is.character(file) && file.exists(file))) 
        stop(gettextf("file '%s' does not exist", file), domain = NA)
    exprs <- .Internal(load.from.file(file))
    if (length(exprs) == 0) 
        return(invisible())
    if (chdir && (path <- dirname(file)) != ".") {
        owd <- getwd()
        on.exit(setwd(owd), add = TRUE)
        setwd(path)
    }
    for (i in exprs) {
        yy <- eval(i, envir)
    }
    invisible()
}


enableJIT <- function (level) 
.Internal(enableJIT(level))


getCompilerOption <- function (name, options = NULL) 
{
    if (name %in% names(options)) 
        options[[name]]
    else get(name, compilerOptions)
}


disassemble <- function (code) 
{
    .CodeSym <- as.name(".Code")
    disasm.const <- function(x) if (typeof(x) == "list" && length(x) > 
        0 && identical(x[[1]], .CodeSym)) 
        disasm(x)
    else x
    disasm <- function(code) {
        code[[2]] <- bcDecode(code[[2]])
        code[[3]] <- lapply(code[[3]], disasm.const)
        code
    }
    if (typeof(code) == "closure") {
        code <- .Internal(bodyCode(code))
        if (typeof(code) != "bytecode") 
            stop("function is not compiled")
    }
    dput(disasm(.Internal(disassemble(code))))
}


cmpfile <- function (infile, outfile, ascii = FALSE, env = .GlobalEnv, verbose = FALSE, 
    options = NULL) 
{
    if (!is.environment(env) || !identical(env, topenv(env))) 
        stop("'env' must be a top level environment")
    if (missing(outfile)) {
        basename <- sub("\\.[a-zA-Z0-9]$", "", infile)
        outfile <- paste0(basename, ".Rc")
    }
    if (infile == outfile) 
        stop("input and output file names are the same")
    forms <- parse(infile)
    nforms <- length(forms)
    if (nforms > 0) {
        expr.needed <- 1000
        expr.old <- getOption("expressions")
        if (expr.old < expr.needed) {
            options(expressions = expr.needed)
            on.exit(options(expressions = expr.old))
        }
        cforms <- vector("list", nforms)
        cenv <- makeCenv(env)
        cntxt <- make.toplevelContext(cenv, options)
        cntxt$env <- addCenvVars(cenv, findLocalsList(forms, 
            cntxt))
        for (i in 1:nforms) {
            e <- forms[[i]]
            if (verbose) {
                if (typeof(e) == "language" && e[[1]] == "<-" && 
                  typeof(e[[3]]) == "language" && e[[3]][[1]] == 
                  "function") 
                  cat(paste0("compiling function \"", e[[2]], 
                    "\"\n"))
                else cat(paste("compiling expression", deparse(e, 
                  20)[1], "...\n"))
            }
            cforms[[i]] <- genCode(e, cntxt)
        }
        cat(gettextf("saving to file \"%s\" ... ", outfile))
        .Internal(save.to.file(cforms, outfile, ascii))
        cat(gettext("done"), "\n", sep = "")
    }
    else warning("empty input file; no output written")
    invisible(NULL)
}


setCompilerOptions <- function (...) 
{
    options <- list(...)
    nm <- names(options)
    for (n in nm) if (!exists(n, compilerOptions)) 
        stop(gettextf("'%s' is not a valid compiler option", 
            n), domain = NA)
    old <- list()
    newOptions <- as.list(compilerOptions)
    for (n in nm) {
        op <- options[[n]]
        switch(n, optimize = {
            op <- as.integer(op)
            if (length(op) == 1 && 0 <= op && op <= 3) {
                old <- c(old, list(optimize = compilerOptions$optimize))
                newOptions$optimize <- op
            }
        }, suppressAll = {
            if (identical(op, TRUE) || identical(op, FALSE)) {
                old <- c(old, list(suppressAll = compilerOptions$suppressAll))
                newOptions$suppressAll <- op
            }
        }, suppressUndefined = {
            if (identical(op, TRUE) || identical(op, FALSE) || 
                is.character(op)) {
                old <- c(old, list(suppressUndefined = compilerOptions$suppressUndefined))
                newOptions$suppressUndefined <- op
            }
        })
    }
    jitEnabled <- enableJIT(-1)
    if (checkCompilerOptions(jitEnabled, newOptions)) 
        for (n in names(newOptions)) assign(n, newOptions[[n]], 
            compilerOptions)
    invisible(old)
}


compilePKGS <- function (enable) 
.Internal(compilePKGS(enable))




## Package Data

# none


## Package Info

.skeleton_package_title = "The R Compiler Package"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF