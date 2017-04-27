##
## Exported symobls in package `pkgmaker`
##

## Exported package methods

isPackageInstalled <- function (..., lib.loc = NULL) 
{
    inst <- utils::installed.packages(lib.loc = lib.loc)
    pattern <- "^([a-zA-Z.]+)(_([0-9.]+)?)?$"
    res <- sapply(list(...), function(p) {
        vers <- gsub(pattern, "\\3", p)
        print(vers)
        pkg <- gsub(pattern, "\\1", p)
        print(pkg)
        if (!(pkg %in% rownames(inst))) 
            return(FALSE)
        p.desc <- inst[pkg, ]
        if ((vers != "") && compareVersion(vers, p.desc["Version"]) > 
            0) 
            return(FALSE)
        TRUE
    })
    all(res)
}


ExposeAttribute <- function (object, ..., .MODE = "rw", .VALUE = FALSE) 
{
    args <- list(...)
    if (length(args)) {
        if (isString(.MODE) == 1L) 
            .MODE <- rep(.MODE, length(args))
        else if (length(.MODE) != length(args)) {
            stop("Argument .MODE must provide an access mode for each argument in `...`.")
        }
        if (is.null(names(args))) 
            args <- setNames(args, rep("", length(args)))
        un <- names(args) == ""
        if (any(!sapply(args[un], isString))) 
            stop("All unnamed argument must be the name of an attribute, i.e. a character string.")
        if (.VALUE) {
            sapply(names(args)[!un], function(x) {
                attr(object, x) <<- args[[x]]
            })
        }
        else {
            .MODE[!un] <- args[!un]
        }
        eargs <- ifelse(un, args, names(args))
        eargs <- as.list(setNames(.MODE, eargs))
        names(eargs) <- paste("^", names(eargs), "$", sep = "")
    }
    else {
        eargs <- .MODE
    }
    attr(object, ".ExposeAttribute") <- eargs
    class(object) <- c(class(object), "ExposeAttribute")
    object
}


attr_mode <- function (x) 
{
    .getEAmode(x, RAW.. = TRUE)
}


packageReference <- function (key, short = FALSE) 
{
    bibs <- bibtex::read.bib(file = packageReferenceFile())
    k <- sapply(bibs, function(x) x$key)
    mk <- match(key, k)
    sel <- mk[!is.na(mk)]
    if (!length(sel)) 
        return("")
    if (!short) {
        paste(format(bibs[sel]), collapse = "\n\n")
    }
    else {
        sapply(bibs[sel], function(x) {
            if (length(x$author$family) <= 1L) 
                paste(x$author$family, "(", x$year, ")", sep = "")
            else {
                paste(x$author$family[[1]], " et al. (", x$year, 
                  ")", sep = "")
            }
        })
    }
}


`.__T__utest:pkgmaker` <- "<environment>"

runPostponedAction <- function (group = NULL, verbose = getOption("verbose")) 
{
    ns <- topns(strict = FALSE)
    taskObj <- simpleRegistry(".__delayedTasks__", envir = ns)
    if (verbose) {
        message("# Executing postponed ", if (!is.null(group)) 
            paste("'", group, "' ", sep = ""), "action(s) in package '", 
            packageName(ns, .Global = TRUE), "' ... ", appendLF = FALSE)
    }
    isRunningPostponedAction(TRUE)
    on.exit(isRunningPostponedAction(FALSE))
    t <- taskObj$names()
    if (!is.null(group)) 
        t <- grep(str_c("^", group), t, value = TRUE)
    if (verbose > 1 && length(t)) 
        message()
    sapply(t, function(x) {
        act <- taskObj$get(x)
        if (verbose > 1) {
            message("** Action '", x, "' [", packageName(act$envir, 
                .Global = TRUE), "]")
        }
        act$action()
        taskObj$set(x, NULL)
    })
    if (verbose) 
        message("OK [", length(t), "]")
    invisible(length(t))
}


extractLocalFun <- function (f) 
{
    bf <- body(f)
    txt <- as.character(bf)[2]
    if (!grepl("\\{", txt)) {
        sf <- capture.output(print(bf))
        w <- tail(grep("^\\s*\\.local\\(", sf), 1L)
        txt <- paste(sf[-w], collapse = "\n")
    }
    expr <- parse(text = txt)
    e <- new.env()
    eval(expr, e)
}


getBiocRepos <- function (url = "http://www.bioconductor.org", version = NULL) 
{
    if (is.null(url)) {
        url <- getBiocMirror()
        if (is.null(url)) 
            stop("No Bioconductor mirror was setup. Use `setBiocMirror`.")
    }
    biocParts <- c(bioc = "bioc", biocData = "data/annotation", 
        biocExp = "data/experiment", biocExtra = "extra")
    if (is.null(version)) {
        assoc <- list(`2` = c(7L, 2L))
        Rv <- as.integer(sub("([0-9]+).*", "\\1", R.version$minor))
        offset <- assoc[[R.version$major]]
        version <- paste(R.version$major, offset[2L] + Rv - offset[1L], 
            sep = ".")
    }
    setNames(paste(url, "packages", version, biocParts, sep = "/"), 
        names(biocParts))
}


utestFramework <- function (x, eval = FALSE) 
{
    expr <- if (missing(eval) || !eval) 
        substitute(x)
    else if (is.function(x)) 
        body(x)
    if (!is.null(expr)) {
        cw <- makeCodeWalker(leaf = function(e, w) if (is.symbol(e)) 
            cat(e, "\n"))
        s <- str_trim(capture.output(walkCode(expr, cw)))
        if (length(s) > 1L) {
            for (f in names(.UFdata)) {
                if (any(s %in% .UFdata[[f]]$check_functions)) {
                  return(f)
                }
            }
        }
        if (!missing(eval) && !eval) 
            return()
        if (missing(eval)) {
            return(utestFramework(x, eval = TRUE))
        }
    }
    if (!is.character(x)) 
        stop("Invalid argument `x`: expecting a character string")
    path <- x
    framework <- NULL
    tf <- if (is.dir(path)) 
        list.files(path, "\\.[rR]$")
    else path
    for (f in names(.UFdata)) {
        if (any(grepl(.UFdata[[f]]$file_pattern, tf))) {
            return(f)
        }
    }
    if (is.null(framework)) 
        stop("Could not determine unit test framework used in directory: '", 
            path, "'")
    framework
}


mkoptions <- function (...) 
{
    .DATA <- new.env(parent = emptyenv())
    .defaults <- list(...)
    .DATA$.options <- list(...)
    function(...) {
        .options(..., .DATA = .DATA)
    }
}


str_out <- function (x, max = 3L, quote = is.character(x), use.names = FALSE, 
    sep = ", ", total = FALSE) 
{
    if (is_NA(max)) 
        max <- Inf
    suffix <- NULL
    nTotal <- length(x)
    if (max > 2 && length(x) > max) {
        suffix <- "..."
        x <- c(head(x, max - 1), tail(x, 1))
    }
    x <- head(x, max)
    quote <- if (isTRUE(quote)) 
        "'"
    else if (is.character(quote)) 
        quote
    if (!is.null(quote)) 
        x <- unlist(lapply(x, function(v) paste(quote, v, quote, 
            sep = "")))
    else if (all(sapply(x, isInteger))) 
        x <- unlist(lapply(x, function(v) str_c(v, "L")))
    if (use.names && !is.null(names(x))) {
        nm <- str_c(names(x), "=")
        x <- paste(ifelse(nm == "=", "", nm), x, sep = "")
    }
    if (!is.null(suffix)) {
        x <- c(head(x, length(x) - 1L), suffix, tail(x, 1L))
    }
    s <- paste(paste(x, collapse = sep), sep = "")
    if (total) 
        s <- paste0(s, " (", nTotal, " total)")
    s
}


is_NA <- function (x) 
{
    identical(x, NA) || identical(x, as.character(NA)) || identical(x, 
        as.numeric(NA)) || identical(x, as.integer(NA))
}


postponeAction <- function (expr, key = digest(tempfile()), group = NULL, envir = topns(strict = FALSE), 
    verbose = getOption("verbose")) 
{
    if (isRunningPostponedAction()) 
        return()
    ns <- topns(strict = FALSE)
    taskObj <- simpleRegistry(".__delayedTasks__", envir = ns)
    if (!missing(expr)) {
        if (missing(key)) {
            stop("Missing required argument `key` for registering/cancelling delayed action.")
        }
        if (!is.null(group)) 
            key <- str_c(group, "::", key)
        qe <- expr
        if (verbose) {
            if (!is.null(qe)) 
                message("# Postponing action '", key, "'")
            else {
                message("# Cancelling postponed action '", key, 
                  "'")
            }
        }
        taskObj$set(key, list(action = qe, envir = envir))
    }
    else {
        taskObj$names()
    }
}


ns_get <- function (x, ns) 
{
    if (!isNamespace(ns)) 
        ns <- asNamespace(ns)
    get(x, ns)
}


setPackageExtra <- function (handler, extra, ...) 
{
    fhandler <- packageExtraHandler(handler, exact = TRUE, error = FALSE)
    if (is.null(fhandler)) {
        handlers <- packageExtraHandler()
        stop("Could not register action '", extra, "': handler '", 
            handler, "' is not defined", if (length(handlers)) {
                str_c(".\n  Available handlers are: ", str_out(handlers, 
                  Inf))
            }
            else " [handler registry is empty].")
    }
    args <- list(...)
    pkg <- packageName(topenv(parent.frame()), .Global = TRUE)
    setPackageRegistryEntry("extra_action", key = extra, handler = handler, 
        args = args, package = pkg, msg = str_c(" for handler '", 
            handler, "'"))
}


setCRANMirror <- function (url = CRAN, unique = TRUE) 
{
    repos <- c(CRAN = url, getOption("repos"))
    if (unique) {
        nam <- names(repos)
        repos <- repos[!duplicated(repos) & (!duplicated(nam) | 
            nam == "")]
    }
    options(repos = repos)
}


str_bs <- function (x) 
{
    x <- gsub("^\b+", "", x)
    x <- gsub("\n\b+", "\n", x)
    while (length(grep("\b", x, fixed = TRUE))) x <- gsub("[^\n\b][\b]", 
        "", x)
    x
}


parseCMD <- function (parser, ARGS = commandArgs(TRUE), debug = FALSE, envir = parent.frame()) 
{
    if (isString(ARGS) == 1L) {
        ARGS <- strsplit(ARGS, " ")[[1]]
    }
    ARGS <- gsub("'", "\"", ARGS)
    library(pkgmaker, quietly = TRUE)
    prog <- parser$prog
    if (!length(ARGS)) {
        parser$print_usage()
        return(invisible(parser))
    }
    else if (!grepl("^-", ARGS[1L])) {
        command <- ARGS[1L]
        if (!command %in% names(parser$command)) {
            stop("unknown ", prog, " command '", command, "'\n", 
                "  Available commands: ", paste0(names(parser$command), 
                  collapse = ", "))
        }
    }
    else if (any(ARGS %in% c("-h", "--help"))) {
        parser$print_help()
        return(invisible(parser))
    }
    else {
        if (nzchar(parser$command_default())) 
            ARGS <- c(parser$command_default(), ARGS)
        else {
            stop("Missing command:\n  ", paste(capture.output(parser$print_usage()), 
                collapse = "\n"), "\n  Available command(s): ", 
                str_out(names(parser$command), Inf, quote = FALSE), 
                call. = FALSE)
        }
    }
    command <- ARGS[1L]
    cmd_funame <- paste0("CLI_", command)
    if (!exists(cmd_funame, envir, inherits = TRUE)) {
        stop("Could not execute ", prog, " command ", command, 
            ": did not find CLI entry point '", cmd_funame, "'")
    }
    cmd_fun <- get(cmd_funame, envir, inherits = TRUE)
    cmd_parser <- cmd_fun(ARGS = NULL)
    ARGS <- ARGS[-1L]
    if (!length(ARGS)) {
        cmd_parser$print_usage()
        invisible(cmd_parser)
    }
    else if (any(ARGS %in% c("-h", "--help"))) {
        cmd_parser$print_help()
        return(invisible(cmd_parser))
    }
    else {
        args <- cmd_parser$parse_args(ARGS)
        if (debug) {
            message("Call: ", parser$call_string(ARGS))
            message("Parsed arguments:")
            str(args)
        }
        cmd_fun(ARGS = args)
    }
}


alphacol <- function (col, alpha = FALSE) 
{
    apply(as.character(as.hexmode(col2rgb(col, alpha))), 2, function(x) paste("#", 
        paste(x, collapse = ""), sep = ""))
}


unit.test <- function (x, expr, framework = NULL, envir = parent.frame()) 
{
    sid <- as.character(deparse(substitute(x)))
    hash <- suppressWarnings(digest(x))
    eTest <- packageTestEnv()
    f <- function() {
    }
    environment(f) <- eTest
    body(f) <- substitute({
        expr
    })
    if (!grepl("\"", sid)) {
        lmessage("Creating unit test for object: `", sid, "`")
        eval(substitute(attr(x, "test") <- f, list(x = substitute(x), 
            f = f)), envir)
    }
    else lmessage("Creating unit test: ", sid)
    eTest[[str_c(sid, ":", hash)]] <- list(test = f, name = sid, 
        object = is.name(x))
    f
}


packageRegistry <- function (regname = NULL, quiet = FALSE, entry = FALSE, update = !entry, 
    package = topenv(parent.frame())) 
{
    metaregname <- ".packageRegistry"
    name <- regname
    e <- packageEnv(package)
    nm <- packageName(e)
    pkgreg <- .packageMetaRegistry(package, quiet)
    if (is.null(name)) 
        return(pkgreg)
    else {
        if (is.null(pkgreg)) {
            if (quiet) 
                return(NULL)
            stop("Could not find registry '", name, "' in package `", 
                nm, "`: meta registry does not exist.")
        }
        nm <- packageSlot(pkgreg)
        reg <- regfetch(pkgreg, key = name, exact = TRUE, error = FALSE)
        if (is.null(reg)) {
            if (quiet) 
                return(NULL)
            stop("Could not find registry `", name, "` in package `", 
                nm, "`.")
        }
        else {
            if (update) 
                reg <- .update_pkgreg(reg)
            if (entry) 
                return(reg)
            reg$regobj
        }
    }
}


oneoffVariable <- function (default = NULL) 
{
    .var <- default
    function(value) {
        if (missing(value)) {
            res <- .var
            .var <<- default
            res
        }
        else .var <<- value
    }
}


R.SHLIB <- function (libname, ...) 
{
    R.CMD("SHLIB", "-o ", libname, .Platform$dynlib.ext, ...)
}


exitCheck <- function () 
{
    .success <- FALSE
    function(x) {
        if (nargs() == 0L) 
            .success
        else {
            .success <<- TRUE
            x
        }
    }
}


str_desc <- function (object, exdent = 0L) 
{
    p <- sapply(object, function(x) {
        if (is.atomic(x) && length(x) == 1L) 
            x
        else paste("<", class(x), ">", sep = "")
    })
    str_wrap(str_out(p, NA, use.names = TRUE, quote = FALSE), 
        exdent = exdent)
}


R.exec <- function (..., lib.loc = NULL) 
{
    cmd <- paste(file.path(R.home("bin"), "R"), " ", ..., sep = "", 
        collapse = "")
    ol <- set_libPaths(lib.loc)
    on.exit(set_libPaths(ol))
    message(cmd)
    system(cmd, intern = interactive())
}


hook_try <- function (before, options, envir) 
{
    if (!before) {
        if (.try_defined && exists("try", envir = envir, inherits = FALSE)) {
            remove(list = "try", envir = envir)
        }
        .try_defined <<- FALSE
        return(invisible())
    }
    if (!is.null(options$try)) {
        do.signal <- isFALSE(options$try)
        if (isManualVignette() && isTRUE(options$try)) {
            do.signal <- TRUE
        }
        .try <- try_message(do.signal)
        assign("try", .try, envir)
        .try_defined <<- TRUE
    }
}


hasPackageRegistry <- function (regname = NULL, package) 
{
    isNamespaceLoaded(package) && !is.null(packageRegistry(regname, 
        package = package, quiet = TRUE, entry = TRUE))
}


`attr_mode<-` <- function (x, value) 
{
    if (is.null(value)) {
        attr(x, ".ExposeAttribute") <- NULL
        class(x) <- class(x)[!class(x) %in% "ExposeAttribute"]
    }
    else if (isString(value)) {
        x <- ExposeAttribute(x, .MODE = value)
    }
    else if (is.list(value)) {
        args <- c(list(x), names(value), list(.MODE = setNames(value, 
            NULL), .VALUE = FALSE))
        x <- do.call("ExposeAttribute", args)
    }
    else {
        stop("Invalid value: a character string or a list is expected")
    }
    x
}


Sys.getenv_value <- function (name, raw = FALSE) 
{
    val <- Sys.getenv(name, unset = NA, names = FALSE)
    if (raw) 
        return(val)
    if (is.na(val) || !nchar(val) || identical(tolower(val), 
        "false") || val == "0") {
        val <- FALSE
    }
    val
}


latex_preamble <- function (PACKAGE, R = TRUE, CRAN = TRUE, Bioconductor = TRUE, 
    GEO = TRUE, ArrayExpress = TRUE, biblatex = FALSE, only = FALSE, 
    file = "") 
{
    cmd <- "%%%% PKGMAKER COMMANDS %%%%%%\n\\usepackage{xspace}\n"
    inc <- function(arg) {
        e <- parent.frame()
        (!only || eval(substitute(hasArg(arg), list(arg = substitute(arg))), 
            e)) && arg
    }
    if (inc(R)) {
        cmd <- c(cmd, "% R\n\\let\\proglang=\\textit\n\\let\\code=\\texttt \n\\newcommand{\\Rcode}{\\code}\n\\newcommand{\\pkgname}[1]{\\textit{#1}\\xspace}\n\\newcommand{\\Rpkg}[1]{\\pkgname{#1} package\\xspace}\n\\newcommand{\\citepkg}[1]{\\cite{#1}}\n")
    }
    if (inc(CRAN)) {
        cmd <- c(cmd, "% CRAN\n\\newcommand{\\CRANurl}[1]{\\url{http://cran.r-project.org/package=#1}}\n%% CRANpkg\n\\makeatletter\n\\def\\CRANpkg{\\@ifstar\\@CRANpkg\\@@CRANpkg}\n\\def\\@CRANpkg#1{\\href{http://cran.r-project.org/package=#1}{\\pkgname{#1}}\\footnote{\\CRANurl{#1}}}\n\\def\\@@CRANpkg#1{\\href{http://cran.r-project.org/package=#1}{\\pkgname{#1}} package\\footnote{\\CRANurl{#1}}}\n\\makeatother\n%% citeCRANpkg\n\\makeatletter\n\\def\\citeCRANpkg{\\@ifstar\\@citeCRANpkg\\@@citeCRANpkg}\n\\def\\@citeCRANpkg#1{\\CRANpkg{#1}\\cite*{Rpackage:#1}}\n\\def\\@@citeCRANpkg#1{\\CRANpkg{#1}~\\cite{Rpackage:#1}}\n\\makeatother\n\\newcommand{\\CRANnmf}{\\href{http://cran.r-project.org/package=NMF}{CRAN}}\n\\newcommand{\\CRANnmfURL}{\\url{http://cran.r-project.org/package=NMF}}\n")
    }
    if (inc(Bioconductor)) {
        cmd <- c(cmd, "% Bioconductor\n\\newcommand{\\BioCurl}[1]{\\url{http://www.bioconductor.org/packages/release/bioc/html/#1.html}}\n\\newcommand{\\BioCpkg}[1]{\\href{http://www.bioconductor.org/packages/release/bioc/html/#1.html}{\\pkgname{#1}} package\\footnote{\\BioCurl{#1}}}\n\\newcommand{\\citeBioCpkg}[1]{\\BioCpkg{#1}~\\cite{Rpackage:#1}}\n% Bioconductor annotation\n\\newcommand{\\BioCAnnurl}[1]{\\url{http://www.bioconductor.org/packages/release/data/annotation/html/#1.html}}\n\\newcommand{\\BioCAnnpkg}[1]{\\href{http://www.bioconductor.org/packages/release/data/annotation/html/#1.html}{\\Rcode{#1}} annotation package\\footnote{\\BioCAnnurl{#1}}}\n\\newcommand{\\citeBioCAnnpkg}[1]{\\BioCAnnpkg{#1}~\\cite{Rpackage:#1}}\n")
    }
    if (inc(GEO)) {
        cmd <- c(cmd, "% GEO\n\\newcommand{\\GEOurl}[1]{\\href{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=#1}{#1}\\xspace}\n\\newcommand{\\GEOhref}[1]{\\GEOurl{#1}\\footnote{\\url{http://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=#1}}}\n")
    }
    if (inc(ArrayExpress)) 
        cmd <- c(cmd, "% ArrayExpress\n\\newcommand{\\ArrayExpressurl}[1]{\\href{http://www.ebi.ac.uk/arrayexpress/experiments/#1}{#1}\\xspace}\n\\newcommand{\\ArrayExpresshref}[1]{\\ArrayExpressurl{#1}\\footnote{\\url{http://www.ebi.ac.uk/arrayexpress/experiments/#1}}}\n")
    if (biblatex) {
        if (missing(PACKAGE)) 
            stop("Argument `PACKAGE` is required when specifying `biblatex=TRUE`.")
        cmd <- c(cmd, latex_bibliography(PACKAGE, file = NULL))
    }
    cmd <- c(cmd, "%%%% END: PKGMAKER COMMANDS %%%%%%\n")
    cmd <- str_c(cmd, collapse = "\n")
    if (!is.null(file)) 
        cat(cmd, file, sep = "")
    else cmd
}


packageData <- function (list, envir = .GlobalEnv, ...) 
{
    if (missing(list)) 
        return(data(..., envir = envir))
    data(list = list, ..., envir = envir)
    if (length(list) == 1L) 
        get(list, envir = envir)
    else sapply(list, get, envir = envir, simplify = FALSE)
}


packageRegistries <- function (regname = NULL, package = NULL, primary = FALSE) 
{
    lns <- loadedNamespaces()
    if (!is.null(package)) 
        lns <- lns[lns %in% package]
    if (!length(lns)) 
        return(character())
    res <- lapply(lns, function(ns) {
        reg <- packageRegistry(package = ns, quiet = TRUE)
        if (is.null(reg)) 
            return(character())
        regnames <- reg$get_entry_names()
        res <- setNames(regnames, rep(ns, length(regnames)))
        if (primary) {
            pr <- sapply(res, function(n) reg$get_entry(n)$parent)
            res <- res[nchar(pr) == 0L]
        }
        res
    })
    res <- unlist(res)
    if (!is.null(regname)) {
        res <- res[res == regname]
        if (primary && length(res) > 1L) {
            warning("Package registry - Found multiple primary registries '", 
                regname, "' in packages ", str_out(res, Inf), 
                " [using first one only]")
            res <- res[1L]
        }
    }
    res
}


isDevNamespace <- function (ns) 
{
    if (missing(ns)) {
        e <- parent.frame()
        ns <- methods::getPackageName(topenv(e))
    }
    if (!isNamespaceLoaded(ns)) 
        return(FALSE)
    if (isString(ns)) 
        ns <- asNamespace(ns)
    exists(".__DEVTOOLS__", where = ns)
}


ldata <- function (list, ...) 
{
    e <- parent.frame()
    packageData(list = list, ..., envir = e)
}


cgetAnywhere <- function (x) 
{
    do.call("getAnywhere", list(x))
}


packageOptions <- function (..., PACKAGE = packageName()) 
{
    optobj <- as.package_options(PACKAGE)
    optobj <- getOption(optobj$name)
    optobj$options(...)
}


requireRUnit <- function (...) 
{
    if (!is.null(.cache)) 
        return(.cache)
    has_pkg <- function(x) length(find.package(x, quiet = TRUE)) > 
        0L
    ruf <- c("RUnit", "svUnit")
    runit <- NULL
    for (pkg in ruf) {
        if (require.quiet(pkg, character.only = TRUE)) {
            runit <- pkg
            break
        }
    }
    if (is.null(runit)) 
        stop("Cannot find any package providing RUnit framework.")
    message("Using RUnit framework provider: ", runit)
    .cache <<- runit
    invisible(runit)
}


onUnload <- function (libpath) 
{
    dlls <- base::getLoadedDLLs()
    pname <- packageName()
    if (pname %in% names(dlls)) {
        if (!missing(libpath)) 
            library.dynam.unload(pname, libpath)
        else dyn.unload(dlls[[pname]][["path"]])
    }
}


hook_backspace <- function () 
{
    .hook_bkp <- NULL
    function(before, options, envir) {
        if (is.null(options[[name]])) 
            return()
        if (before) {
            if (is.null(.hook_bkp)) 
                .hook_bkp <<- knit_hooks$get(type)
            hook_wrapper <- function(x, options) {
                res <- .hook_bkp(x, options)
                hook(res, options)
            }
            args <- list()
            args[[type]] <- hook_wrapper
            do.call(knit_hooks$set, args)
        }
        else {
            args <- list()
            args[[type]] <- .hook_bkp
            do.call(knit_hooks$set, args)
            .hook_bkp <<- NULL
        }
    }
}


libname <- function (x) 
{
    sub(str_c("\\", .Platform$dynlib.ext, "$"), "", basename(x))
}


option_symlink_target <- function (x, opts) 
{
    if (!is.list(opts)) 
        stop("invalid argument `opts`: must be a list object")
    n <- 0
    track <- NULL
    while (is_option_symlink(x, opts)) {
        if (x %in% track) 
            stop("cycling symbolic link options: ", str_out(c(track, 
                x), Inf, sep = " -> "))
        track <- c(track, x)
        x <- opts[[x]]
        n <- n + 1
    }
    x
}


packageCLI <- function (package, altfile = NULL, local = TRUE, ARGS = commandArgs(TRUE), 
    ...) 
{
    master_cli <- if (!is.null(package)) 
        system.file("scripts", "CLI.R", package = package)
    else if (is.null(altfile)) {
        stop("Could not load CLI definition: argument `package` or `altfile` is required")
    }
    if (!length(master_cli) || !nzchar(master_cli)) {
        master_cli <- altfile
    }
    source(master_cli, keep.source = TRUE, chdir = TRUE, local = local)
    if (!exists("CLI", inherits = FALSE)) {
        stop("Could not start command line interface for package '", 
            package, "': main entry point function CLI() not found.")
    }
    CLI <- get("CLI", inherits = !local)
    CLI(ARGS, ...)
}


CLIArgumentParser <- function (prog = CLIfile(), description = "", ..., epilog = "", 
    show.defaults = TRUE) 
{
    suppressMessages(library(argparse, quietly = TRUE))
    .flag_newlines <- function(x) {
        gsub("\n", "", x)
    }
    .special <- "__@@##@@__"
    epilog <- paste0(.special, epilog)
    p <- ArgumentParser(prog = prog, description = .flag_newlines(description), 
        ..., epilog = .flag_newlines(epilog))
    if (show.defaults) {
        i <- grep("argparse\\.ArgumentParser", p$python_code)
        inst <- p$python_code[i]
        p$python_code[i] <- paste0(substr(inst, 1, nchar(inst) - 
            1), ", formatter_class=argparse.ArgumentDefaultsHelpFormatter)")
    }
    p <- proto(p)
    p$command_loc <- .special
    p$prog <- prog
    p$exec <- if (nchar(exec_path <- CLIfile(full = TRUE))) 
        normalizePath(CLIfile(full = TRUE))
    else ""
    p$command <- list()
    p$command_idefault <- 0L
    p$command_default <- function(.) {
        if (.$command_idefault) 
            names(.$command)[.$command_idefault]
        else ""
    }
    p$add_command <- function(., command, help = "", ..., default = FALSE) {
        if (!length(.$command)) {
            .$.super$add_argument("command", help = paste0(.$prog, 
                " command to run"))
        }
        .$command[command] <- help
        if (default) 
            .$command_idefault <- length(.$command)
    }
    p$add_argument <- function(., ..., help = "") {
        .flag_newlines <- function(x) {
            gsub("\n", "", x)
        }
        help <- .flag_newlines(help)
        .$.super$add_argument(..., help = help)
    }
    p$print_usage <- function(.) {
        .$.super$print_usage()
        if (length(.$command)) {
            cat("\n  Use --help for listing all available commands\n")
        }
    }
    p$print_help <- function(.) {
        h <- paste(capture.output(.$.super$print_help()), collapse = "\n")
        cmds <- ""
        if (length(.$command)) {
            lm <- max(nchar(names(.$command)))
            fmt <- paste0("  %-", lm, "s")
            cmds <- strwrap(.$command, indent = 4, exdent = 2 + 
                lm + 4, width = 80, simplify = FALSE)
            cmds <- sapply(cmds, paste, collapse = "\n")
            cmds <- paste0(sprintf(fmt, names(.$command)), cmds)
            cmds <- paste0("Commands:\n", paste(cmds, collapse = "\n"))
        }
        h <- gsub(.$command_loc, cmds, h, fixed = TRUE)
        cat(h, sep = "\n")
    }
    p$call_string <- function(., args = commandArgs(TRUE)) {
        paste(.$prog, paste0(args, collapse = " "))
    }
    e <- parent.frame()
    p$locenvir <- parent.env(e)
    p$parse_cmd <- function(., ...) {
        pkgmaker::parseCMD(., ..., envir = .$locenvir)
    }
    p
}


setBiocMirror <- function (url = "http://www.bioconductor.org", version = NULL, 
    unique = TRUE) 
{
    biocRepos <- getBiocRepos(url, version)
    repos <- c(biocRepos, getOption("repos"))
    if (unique) {
        nam <- names(repos)
        repos <- repos[!duplicated(repos) & (!duplicated(nam) | 
            nam == "")]
    }
    options(repos = repos)
}


install.extras <- function (package, extra = NULL, handler = NULL, ..., .verbose = getOption("verbose")) 
{
    if (missing(handler)) 
        handler <- .handler
    .local <- function(p, ...) {
        extras <- packageExtra(handler = handler, extra = extra, 
            package = p)
        sapply(extras, function(def, ...) {
            e <- def$key
            h <- def$handler
            f <- packageExtra(handler = h, extra = e, package = p, 
                .wrap = TRUE)
            if (.verbose) {
                message("# Running extra action '", h, ":", e, 
                  "' ...")
                message("# Action: ", str_fun(f))
                on.exit(message("# ERROR [", e, "]\n"))
            }
            res <- f(...)
            if (.verbose) {
                on.exit()
                message("# OK [", e, "]\n")
            }
            res
        }, ...)
    }
    invisible(sapply(package, .local, ...))
}


allFormals <- function (f) 
{
    if (is(f, "MethodDefinition")) {
        f <- f@.Data
        lf <- try(codetools::getAssignedVar(body(f)), silent = TRUE)
        if (!identical(lf, ".local")) 
            return(formals(f))
        lfun <- extractLocalFun(f)
        res <- formals(lfun)
        generic_args <- formals(f)
        meth_no_default <- sapply(res, is.symbol)
        gen_no_default <- sapply(generic_args, is.symbol)
        generic_args <- generic_args[!gen_no_default]
        generic_args <- generic_args[names(generic_args) %in% 
            names(res[meth_no_default])]
        if (length(generic_args)) {
            res[names(generic_args)] <- generic_args
        }
        res
    }
    else if (is.function(f)) 
        formals(f)
}


knit_ex <- function (x, ..., quiet = TRUE, open = FALSE) 
{
    library(knitr)
    x <- gsub("^^^", "```", x, fixed = TRUE)
    if (!(html_chunks <- any(grepl("```{", x, fixed = TRUE)))) {
        if (all(!grepl(">>=", x, fixed = TRUE))) {
            x <- c("```{r}", x, "```")
            html_chunks <- TRUE
        }
    }
    x <- paste0(x, collapse = "\n")
    if (any(html_chunks)) {
        res <- knit2html(text = x, ..., fragment.only = TRUE, 
            quiet = quiet)
        if (open) {
            tmp <- tempfile("knit_ex", fileext = ".html")
            cat(res, file = tmp, sep = "\n")
            if (interactive()) 
                browseURL(tmp)
            return(invisible(res))
        }
    }
    else {
        res <- knit(text = x, ..., quiet = quiet)
    }
    cat(res)
}


checkPlot <- function (expr, msg = NULL, width = 1000, height = NULL) 
{
    uf <- requireRUnit()
    if (is.null(uf) || uf != "RUnit") 
        return(TRUE)
    .testLogger <- if (.existsTestLogger()) 
        .GlobalEnv$.testLogger
    if (missing(expr)) {
        stop("'expr' is missing.")
    }
    plotfile <- if (.existsTestLogger()) {
        .testLogger$incrementCheckNum()
        if (is.null(.testLogger$setPlot)) {
            addToLogger(".plots", NULL)
            .plots <- NULL
            addToLogger("setPlot", function(name, msg = "") {
                if (is.null(.plots)) 
                  .plots <<- list()
                .plots[[name]] <<- msg
            })
            .getTestData <- .currentTestSuiteName <- .currentSourceFileName <- .getCheckNum <- NULL
            addToLogger("getPlotfile", function(name, msg = "") {
                td <- .getTestData()
                fname <- basename(tempfile(paste(.currentTestSuiteName, 
                  "_", .currentSourceFileName, "_", sep = "")))
                paste(fname, .getCheckNum(), sep = "_")
            })
            .testLogger <- .GlobalEnv$.testLogger
        }
        .testLogger$getPlotfile()
    }
    else tempfile(tmpdir = ".")
    plotfile <- paste(plotfile, "png", sep = ".")
    if (is.null(msg)) 
        msg <- plotfile
    png(filename = plotfile, width = width)
    res <- try(eval(expr, envir = parent.frame()))
    dev.off()
    fileinfo <- file.info(plotfile)
    if (inherits(res, "try-error") || is.na(fileinfo$size[1]) || 
        fileinfo$size[1] == 0) {
        unlink(plotfile)
        if (.existsTestLogger()) {
            .testLogger$setFailure()
        }
        stop("Problem when generating plot:", res, msg)
    }
    if (.existsTestLogger()) {
        .testLogger$setPlot(plotfile, msg)
    }
    return(TRUE)
}


quickinstall <- function (path, destdir = NULL, vignettes = FALSE, force = TRUE, 
    ..., lib.loc = if (!is.null(destdir)) TRUE) 
{
    npath <- normalizePath(path)
    pkg <- as.package(path)
    nlib <- if (!is.null(destdir)) 
        destdir
    else if (is_NA(destdir)) 
        tempfile("pkglib_")
    if (!is.null(nlib)) {
        if (!is.dir(nlib)) 
            dir.create(nlib, recursive = TRUE)
        nlib <- normalizePath(nlib)
        if (!is.dir(nlib)) {
            stop("Could not install package '", pkg$package, 
                "': installation directory '", nlib, "' does not exist.")
        }
        if (isTRUE(lib.loc)) 
            lib.loc <- unique(c(nlib, .libPaths()))
    }
    res <- invisible(if (!is.null(destdir)) nlib else .libPaths()[1L])
    message("# Check for previous package installation ... ", 
        appendLF = FALSE)
    if (!is.null(destdir) && is.dir(file.path(nlib, pkg$package))) {
        if (!force) {
            message("YES (skip)")
            return(res)
        }
        message("YES (replace)")
    }
    else message("NO")
    ol <- set_libPaths(lib.loc)
    on.exit(set_libPaths(ol), add = TRUE)
    message("Using R Libraries: ", str_out(.libPaths(), Inf))
    owd <- setwd(tempdir())
    on.exit(setwd(owd), add = TRUE)
    message("# Building package `", pkg$package, "` in '", getwd(), 
        "'")
    opts <- "--no-manual --no-resave-data "
    if (!vignettes) {
        vflag <- if (testRversion(">= 3.0")) 
            "--no-build-vignettes "
        else "--no-vignettes "
        opts <- str_c(opts, vflag)
    }
    R.CMD("build", opts, path.protect(npath), ...)
    spkg <- paste(pkg$package, "_", pkg$version, ".tar.gz", sep = "")
    if (!file.exists(spkg)) 
        stop("Error in building package `", pkg$package, "`")
    message("# Installing package `", pkg$package, "`", if (!is.null(destdir)) {
        tmp <- if (is_NA(destdir)) 
            "temporary "
        str_c("in ", tmp, "'", nlib, "'")
    })
    opts_inst <- " --no-multiarch --no-demo --with-keep.source "
    if (!vignettes) 
        opts_inst <- str_c(opts_inst, "--no-docs ")
    R.CMD("INSTALL", if (!is.null(destdir)) 
        paste("-l", path.protect(nlib)), opts_inst, path.protect(spkg), 
        ...)
    invisible(res)
}


is.file <- function (x) 
file_test("-f", x)


requirePackage <- function (pkg, ...) 
{
    if (!require(pkg, character.only = TRUE)) {
        if (nargs() > 1L) 
            stop(..., " requires package(s) ", str_out(pkg))
        else stop("Could not find required package(s) ", str_out(pkg))
    }
}


rnwIncludes <- function (x) 
{
    x <- rnwObject(x)
    l <- readLines(x$file)
    dr <- suppressWarnings(str_match(l, "^\\s*\\\\((include)|(includegraphics)|(input))\\{([^}]*)\\}"))
    w <- which(!is.na(dr[, 1L]))
    rnw_message("Detected includes: ", appendLF = FALSE)
    if (length(w) > 0L) {
        inc <- str_trim(dr[w, 6L])
        message(str_out(inc))
        inc
    }
    else message("NONE")
}


vignetteMakefile <- function (package = NULL, skip = NULL, print = TRUE, template = NULL, 
    temp = FALSE, checkMode = isCHECK() || vignetteCheckMode(), 
    user = NULL, tests = TRUE) 
{
    if (is.null(template)) 
        template <- packagePath("vignette.mk", package = "pkgmaker")
    l <- paste(readLines(template), collapse = "\n")
    l <- subMakeVar("R_BIN", R.home("bin"), l)
    if (checkMode) {
        oldCM <- vignetteCheckMode(TRUE)
        on.exit(vignetteCheckMode(oldCM))
    }
    localMode <- !checkMode
    cuser <- Sys.info()["user"]
    l <- subMakeVar("VIGNETTE_USER", cuser, l)
    maintainers <- "-"
    if (!is.null(user)) {
        maintainers <- str_c(user, collapse = ", ")
        if (cuser %in% user) {
            localMode <- TRUE
        }
    }
    l <- subMakeVar("VIGNETTE_MAINTAINERS", maintainers, l)
    if (localMode) {
        l <- defMakeVar("LOCAL_MODE", cuser, l)
    }
    pkg_dir <- dirname(getwd())
    loc_package <- if (is.file(df <- file.path(pkg_dir, "DESCRIPTION"))) {
        d <- try(read.dcf(df), silent = TRUE)
        d <- as.list(as.data.frame(d, stringsAsFactors = FALSE))
        d$Package
    }
    if (!is.null(loc_package) && (is.null(package) || identical(loc_package, 
        package))) 
        package <- loc_package
    else if (!identical(loc_package, package) && length(pkg_dir <- find.package(package, 
        quiet = TRUE))) {
        d <- packageDescription(package)
    }
    else {
        stop("Could not load DESCRIPTION file for package '", 
            package, "'.")
    }
    l <- defMakeVar("MAKE_R_PACKAGE", package, l)
    l <- subMakeVar("R_PACKAGE_DESCRIPTION", pkg_dir, l)
    Rlibs <- NULL
    if (localMode && is.dir(devlib <- file.path(getwd(), "..", 
        "..", "lib"))) {
        Rlibs <- devlib
    }
    Rlibs <- paste(c(Rlibs, "$(TMP_INSTALL_DIR)", "$(R_LIBS)"), 
        collapse = .Platform$path.sep)
    l <- subMakeVar("R_LIBS_DEV", Rlibs, l)
    l <- subMakeVar("TMP_INSTALL_DIR", file.path(dirname(tempdir()), 
        basename(tempfile("Rpkglib_"))), l)
    rnwFiles <- NULL
    if (is.dir("src")) 
        rnwFiles <- list.files("src", pattern = "\\.Rnw$")
    if (tests && is.dir("../tests")) 
        rnwFiles <- c(rnwFiles, str_c(package, "-unitTests.Rnw"))
    rnwFiles <- c(rnwFiles, list.files(".", pattern = "\\.Rnw$"))
    rnwFiles <- unique(rnwFiles)
    if (!is.null(skip)) 
        rnwFiles <- setdiff(rnwFiles, skip)
    l <- subMakeVar("RNW_SRCS", paste(rnwFiles, collapse = " "), 
        l)
    noBuildVignettes <- if (!is.null(d$BuildVignettes)) 
        tolower(d$BuildVignettes) == "no"
    else FALSE
    if (localMode && noBuildVignettes) {
        l <- defMakeVar("INST_TARGET", 1, l)
        l <- defMakeVar("PDF_OBJS", paste(file.path("../inst/doc", 
            sub("\\.Rnw$", ".pdf", rnwFiles)), collapse = " "), 
            l)
    }
    l <- defMakeVar("PDF_OBJS", paste(file.path("../inst/doc", 
        sub("\\.Rnw$", ".pdf", rnwFiles)), collapse = " "), l)
    mk <- if (temp) 
        tempfile("vignette_", tmpdir = ".", fileext = ".mk")
    else "vignette.mk"
    cat(l, file = mk)
    if (print) {
        cat(mk)
    }
    invisible(l)
}


isManualVignette <- function () 
{
    isTRUE(getOption("R_RUNNING_MANUAL_VIGNETTE"))
}


getBiocMirror <- function () 
{
    getOption("BioC_mirror")
}


install.dependencies <- function (pkg = NULL, all = FALSE, ..., dryrun = FALSE) 
{
    pkg <- as.package(pkg, extract = TRUE)
    deps <- c(parse_deps(pkg$depends), parse_deps(pkg$imports), 
        parse_deps(pkg$linkingto), if (isTRUE(all)) parse_deps(pkg$suggests))
    not.installed <- function(x) length(find.package(x, quiet = TRUE)) == 
        0
    message("Package dependencies for ", pkg$package, ": ", str_out(deps, 
        Inf))
    deps <- Filter(not.installed, deps)
    if (length(deps) == 0) {
        message("Missing: none")
        return(invisible())
    }
    message("Missing: ", str_out(deps, Inf))
    message("Installing ", length(deps), " dependencies for ", 
        pkg$package)
    if (!dryrun) {
        .biocLite(deps, ...)
    }
    invisible(deps)
}


is_option_symlink <- function (x, opts) 
{
    if (missing(opts)) 
        is(x, "option_symlink")
    else is(opts[[x]], "option_symlink")
}


packageExtraHandler <- function (handler = NULL, ...) 
{
    pkgreg_fetch("extra_handler", key = handler, ...)
}


str_fun <- function (object) 
{
    s <- capture.output(args(object))
    paste(s[-length(s)], collapse = "\n")
}


CRAN <- "http://cran.r-project.org"


isCRAN_timing <- function () 
isCRANcheck("timing")


expand_list <- function (x, ..., .exact = TRUE, .names = !.exact) 
{
    defaults <- list(...)
    if (length(defaults) == 1L && is.null(names(defaults))) {
        defaults <- defaults[[1L]]
    }
    if (!length(defaults)) 
        return(x)
    x_ex <- x
    if (!.exact) {
        i <- pmatch(names(x), names(defaults))
        if (length(w <- which(!is.na(i)))) {
            names(x_ex)[w] <- names(defaults)[i[w]]
            if (.names) 
                names(x)[w] <- names(defaults)[i[w]]
        }
    }
    i <- match(names(defaults), names(x_ex))
    if (length(w <- which(is.na(i)))) {
        n <- names(defaults)[w]
        lapply(n, function(m) {
            if (is.null(defaults[[m]])) 
                x[m] <<- list(NULL)
            else x[[m]] <<- defaults[[m]]
        })
    }
    x
}


latex_bibliography <- function (PACKAGE, file = "") 
{
    rpkg.bib <- "%\\bibliography{Rpackages}\n"
    cmd <- rpkg.bib
    reffile <- packageReferenceFile(PACKAGE = PACKAGE)
    if (is.file(reffile)) {
        cmd <- paste0(cmd, "\\bibliography{", gsub("\\.bib$", 
            "", reffile), "}\n")
    }
    library(knitr)
    knit_hooks$set(document = function(x) {
        if (length(pkgs <- parsePackageCitation(x))) {
            write.pkgbib(gsub("^Rpackage:", "", pkgs), file = "Rpackages.bib", 
                prefix = "Rpackage:")
            x <- gsub("%\\bibliography{Rpackages}", "\\bibliography{Rpackages}", 
                x, fixed = TRUE)
        }
        x
    })
    if (!is.null(file)) 
        cat(cmd, file = file)
    else cmd
}


packageEnv <- function (pkg, skip = FALSE, verbose = FALSE) 
{
    if (!missing(pkg) && !is.null(pkg)) {
        env <- if (is.environment(pkg)) 
            topenv(pkg)
        else if (!is.null(path.package(pkg, quiet = TRUE))) 
            asNamespace(pkg)
        else if (isLoadingNamespace(pkg)) 
            getLoadingNamespace(env = TRUE)
        else if (isNamespaceLoaded(pkg)) 
            asNamespace(pkg)
        else if (pkg %in% search()) 
            as.environment(pkg)
        else as.environment(str_c("package:", pkg))
        return(env)
    }
    envir = parent.frame()
    pkgmakerEnv <- topenv()
    n <- 1
    skipEnv <- pkgmakerEnv
    while (identical(e <- topenv(envir), skipEnv) && !identical(e, 
        emptyenv()) && !identical(e, .GlobalEnv)) {
        if (verbose > 1) 
            print(e)
        n <- n + 1
        envir <- parent.frame(n)
    }
    if (!skip) {
        if (identical(e, .BaseNamespaceEnv)) {
            if (verbose) 
                message("packageEnv - Inferred ", str_ns(skipEnv))
            return(skipEnv)
        }
        if (verbose) 
            message("packageEnv - Detected ", str_ns(e))
        return(e)
    }
    if (verbose > 1) 
        message("Skipping ", str_ns(skipEnv))
    skipEnv <- e
    while (identical(e <- topenv(envir), skipEnv) && !identical(e, 
        emptyenv()) && !identical(e, .GlobalEnv)) {
        if (verbose > 1) 
            print(e)
        n <- n + 1
        envir <- parent.frame(n)
    }
    if (identical(e, .BaseNamespaceEnv)) {
        if (verbose) 
            message("packageEnv - Inferred ", str_ns(skipEnv))
        return(skipEnv)
    }
    if (verbose) 
        message("packageEnv - Detected ", str_ns(e))
    return(e)
}


rnwChildren <- function (x) 
{
    x <- rnwObject(x)
    l <- readLines(x$file)
    dr <- str_match(l, "^\\s*\\\\SweaveInput\\{([^}]*)\\}")
    w <- which(!is.na(dr[, 1L]))
    if (length(w) > 0L) {
        inc <- dr[w, 2L]
        rnw_message("Detected children: ", str_out(inc, Inf))
        owd <- setwd(dirname(x$file))
        on.exit(setwd(owd))
        mapply(as.rnw, inc, line = w, SIMPLIFY = FALSE)
    }
}


isString <- function (x, y, ignore.case = FALSE) 
{
    if (res <- is.character(x) && length(x) == 1L) {
        if (!missing(y)) {
            if (!isString(y)) 
                stop("Invalid argument 'y': must be a string itself.")
            if (ignore.case) {
                x <- toupper(x)
                y <- toupper(y)
            }
            res <- x == y
        }
    }
    res
}


utestPath <- function (...) 
{
    packagePath("tests-results", ...)
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

isLoadingNamespace <- function (ns, nodev = FALSE) 
{
    if (missing(ns)) 
        !is.null(getLoadingNamespace(nodev = nodev))
    else {
        nspkg <- getLoadingNamespace(nodev = nodev, env = is.environment(ns))
        if (is.null(nspkg)) 
            FALSE
        else identical(nspkg, ns)
    }
}


writeUnitVignette <- function (pkg, file, results = NULL, check = FALSE) 
{
    Rnw.template <- "\n\\documentclass[10pt]{article}\n%\\VignetteDepends{knitr}\n%\\VignetteIndexEntry{@pkg@-unitTests}\n%\\VignetteCompiler{knitr}\n%\\VignetteEngine{knitr::knitr}\n\\usepackage{vmargin}\n\\setmargrb{0.75in}{0.75in}{0.75in}{0.75in}\n\n<<setup, include=FALSE>>=\npkg <- '@pkg@'\nrequire( pkg, character.only=TRUE )\nprettyVersion <- packageDescription(pkg)$Version\nprettyDate <- format(Sys.Date(), '%B %e, %Y')\nauthors <- packageDescription(pkg)$Author\n@\n\n\\usepackage[colorlinks]{hyperref}\n\\author{\\Sexpr{authors}}\n\\title{\\texttt{\\Sexpr{pkg}}: Unit testing results@resNote@}\n\\date{\\texttt{\\Sexpr{pkg}} version \\Sexpr{prettyVersion} as of \\Sexpr{prettyDate}}\n\\begin{document}\n\\maketitle\n\n@results@\n\n\\section*{Session Information}\n@sessionInfo@\n\n\\end{document}\n"
    verbatim_wrap <- function(...) {
        c("\\\\begin{verbatim}\n", ..., "\n\\\\end{verbatim}")
    }
    if (is.null(results)) {
        upath <- utestPath(package = pkg)
        results <- list.files(upath, pattern = "\\.txt$", full.names = TRUE)
        if (!length(results)) {
            results <- verbatim_wrap("Could not find any unit test result in \"", 
                upath, "\"")
        }
    }
    if (is.file(results[1L])) {
        resFile <- results[1L]
        name <- str_match(resFile, "([^.]+)\\.[^.]+$")[, 2L]
        results <- c(str_c("\\\\section{", name, "}"), verbatim_wrap(readLines(resFile)))
    }
    else {
        resFile <- NULL
    }
    results <- paste(results, collapse = "\n")
    contents <- Rnw.template
    contents <- gsub("@pkg@", pkg, contents)
    contents <- gsub("@results@", results, contents)
    contents <- gsub("@sessionInfo@", gsub("\\", "\\\\", paste(toLatex(sessionInfo()), 
        collapse = "\n"), fixed = TRUE), contents)
    resnote <- str_c("\\footnote{Vignette computed ", if (check) 
        " via R CMD check/build ", " on ", date(), "}")
    if (check) {
        lfile <- gsub("([_$])", "\\\\\\1", paste(resFile, collapse = "\\\\"))
        resnote <- str_c(resnote, " \\footnote{File: '", lfile, 
            "'}")
    }
    contents <- gsub("@resNote@", gsub("\\", "\\\\", resnote, 
        fixed = TRUE), contents)
    fileext <- toupper(file_extension(file))
    fileext <- charmatch(fileext, c("RNW", "TEX", "PDF"))
    if (is_NA(fileext)) 
        stop("Invalid output file extension [", fileext, "] from file '", 
            file, "'")
    fileRNW <- if (fileext == 1L) 
        file
    else str_c(pkg, "-unitTests.Rnw")
    fileTEX <- if (fileext == 2L) 
        file
    else str_c(pkg, "-unitTests.tex")
    filePDF <- if (fileext == 3L) 
        file
    else str_c(pkg, "-unitTests.pdf")
    writeLines(contents, fileRNW)
    if (fileext == 1L) 
        return()
    rnw(fileRNW, fileTEX)
    if (fileext == 2L) 
        return()
    res <- tools::texi2dvi(fileTEX, pdf = TRUE, clean = TRUE)
    if (check) 
        file.copy(filePDF, "../../..")
    res
}


topns_name <- function (n = 1L, strict = TRUE, unique = TRUE) 
{
    if (n == 1L && !is.null(ns <- getLoadingNamespace())) {
        return(ns)
    }
    nf <- sys.nframe()
    i <- 0
    res <- character()
    while (i <= nf && length(res) < n) {
        e <- sys.frame(i)
        if (!strict || !identical(e, .GlobalEnv)) {
            pkg <- methods::getPackageName(e, create = FALSE)
            if (pkg != "") {
                res <- c(res, pkg)
            }
        }
        i <- i + 1
    }
    if (!length(res)) {
        e <- packageEnv(skip = TRUE)
        if (isNamespace(e)) {
            res <- methods::getPackageName(e)
        }
        else {
            return("")
        }
    }
    if (unique || n == 1L) 
        res <- match.fun("unique")(res)
    if (length(res) || n > 1L) 
        res
    else ""
}


simpleRegistry <- function (name, envir = topenv(parent.frame()), verbose = FALSE) 
{
    if (exists(name, envir = envir)) {
        return(invisible(get(name, envir = envir)))
    }
    if (verbose) 
        message("# Setup simple registry '", name, "' in ", packageName(envir, 
            .Global = TRUE))
    .name <- name
    .envir <- envir
    .data <- list()
    .get <- function(x) {
        if (.has(x)) {
            .data[[x]]
        }
    }
    .set <- function(x, value) {
        if (is.null(value)) {
            if (.has(x)) {
                .data[[x]] <<- NULL
            }
        }
        else {
            .data[[x]] <<- value
        }
    }
    .has <- function(x) {
        x %in% names(.data)
    }
    .cleanup <- function() {
        rm(.name, envir = .envir)
    }
    .names <- function() {
        names(.data)
    }
    .length <- function() {
        length(.data)
    }
    .obj <- list(get = .get, set = .set, has = .has, cleanup = .cleanup, 
        names = .names, length = .length)
    assign(.name, .obj, envir = .envir)
    invisible(.obj)
}


pkgreg_remove <- function (regname, ..., msg = NULL, where = topenv(parent.frame()), 
    quiet = FALSE) 
{
    regentry <- packageRegistry(regname, package = where, entry = TRUE, 
        update = TRUE)
    if (missing(msg) && !is_NA(regentry$entrydesc)) 
        msg <- regentry$entrydesc
    entry <- regfetch(regentry$regobj, ..., exact = TRUE, error = FALSE, 
        all = TRUE, msg = msg)
    res <- if (!is.null(entry)) {
        name <- names(entry)
        if (!quiet) {
            msg <- paste0("Removing ", msg, " '", name, "' from registry '", 
                regname, "'")
            message(msg, " ... ", appendLF = FALSE)
        }
        regentry$regobj$delete_entry(name)
        if (!quiet) 
            message("OK")
        TRUE
    }
    else {
        if (!quiet) {
            name <- str_out(list(...), Inf, use.names = TRUE)
            warning("Could not remove ", msg, " '", name, "': no matching registry entry.", 
                call. = FALSE)
        }
        FALSE
    }
    if (quiet) 
        invisible(res)
    else res
}


rnwWrapper <- function (x, verbose = TRUE) 
{
    x <- rnwObject(x)
    l <- readLines(x$file)
    dr <- str_match(l, str_c("^\\s*", if (commented) 
        "%", "\\s*\\\\", tag, if (options) 
        "(\\[[^]]*\\])?", "\\{([^}]*)\\}"))
    w <- which(!is.na(dr[, 1L]))
    if (length(w) > 0L) {
        if (first) 
            w <- w[1L]
        s <- dr[w, if (options) 
            3L
        else 2L]
        if (trim) 
            s <- str_trim(s)
        if (verbose) 
            rnw_message("Detected ", name, ": ", paste("'", s, 
                "'", sep = "", collapse = ", "))
        s
    }
}


userIs <- function (user) 
{
    setNames(Sys.info()["user"], NULL) %in% user
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

makeUnitVignette <- function (pkg, file = paste(pkg, "-unitTests.pdf", sep = ""), 
    ..., check = FALSE) 
{
    package <- pkg
    pkg <- sub("^package:", "", pkg)
    if (!is.null(file)) 
        on.exit(writeUnitVignette(pkg, file, check = check))
    if (!require(pkg, character.only = TRUE)) {
        stop("Could not load package '", pkg, "' for testing [libPath= ", 
            str_out(.libPaths(), Inf), "]")
    }
    if (!check) {
        utestCheckMode(FALSE)
        tests <- utest(package, ...)
        err <- getErrors(tests)
        errMsg <- NULL
        if (err$nFail > 0) {
            errMsg <- c(errMsg, sprintf("unit test problems: %d failures\n", 
                err$nFail))
        }
        if (err$nErr > 0) {
            errMsg <- c(errMsg, sprintf("unit test problems: %d errors\n", 
                err$nErr))
        }
        if (length(errMsg) > 0L) 
            stop(errMsg)
        err
    }
    else {
    }
}


orderVersion <- function (x, decreasing = FALSE) 
{
    tx <- gsub("[^0-9]+", ".", paste("_", x, sep = ""))
    stx <- strsplit(tx, ".", fixed = TRUE)
    mtx <- max(sapply(stx, length))
    tx <- sapply(stx, function(v) paste(sprintf("%06i", c(as.integer(v[-1]), 
        rep(0, mtx - length(v) + 1))), collapse = "."))
    order(tx, decreasing = decreasing)
}


parsePackageCitation <- function (x) 
{
    if (length(x) > 1L) 
        x <- paste(x, collapse = "\n")
    .parse <- function(x, pattern, idx) {
        dr <- str_match_all(x, pattern)
        dr <- dr[sapply(dr, length) > 0L]
        unlist(lapply(dr, "[", , idx))
    }
    x <- gsub(".*[^%]* *\\\\begin\\{document\\}(.*)", "\\1", 
        x)
    cite <- .parse(x, "\\\\cite((CRAN)|(BioC)|(BioCAnn))?pkg[*]?\\{([^}]*)\\}", 
        6L)
    cite2 <- .parse(x, "\\\\cite[^{ ]*\\{([^}]*)\\}", 2L)
    if (length(cite2)) {
        cite2 <- .parse(cite2, ".*Rpackage:([^,}]+).*", 2L)
        cite <- c(cite, cite2)
    }
    if (length(cite)) {
        cite <- unlist(strsplit(cite, ","))
        cite <- gsub("^Rpackage:", "", cite)
    }
    inc <- character()
    if (length(cite) > 0L) {
        inc <- unique(str_trim(unlist(strsplit(cite, ","))))
    }
    inc
}


option_symlink <- function (x) 
{
    if (!is.character(x)) 
        stop("Symbolic link options must be character strings")
    structure(x, class = "option_symlink")
}


R.CMD <- function (cmd, ...) 
{
    R.exec("CMD ", cmd, " ", ...)
}


write.pkgbib <- function (entry = NULL, file = "Rpackages.bib", prefix = "", 
    append = FALSE, verbose = TRUE) 
{
    if (is.null(file)) {
        file <- stdout()
        verbose <- FALSE
    }
    if (is.null(entry)) {
        if (verbose) 
            message("Generating Bibtex entries for all installed packages ", 
                appendLF = FALSE)
        entry <- unique(installed.packages()[, 1])
        if (verbose) 
            message("[", length(entry), "]")
    }
    bibs <- if (is(entry, "bibentry")) 
        entry
    else if (is.character(entry)) {
        if (length(entry) == 0) {
            if (verbose) 
                message("Empty package list: nothing to be done.")
            return(invisible())
        }
        pkgs <- entry
        bibs <- sapply(pkgs, function(x) try(citation(x)), simplify = FALSE)
        n.installed <- length(bibs)
        ok <- sapply(bibs, is, "bibentry")
        pkgs <- pkgs[ok]
        bibs <- bibs[ok]
        n.converted <- sum(ok)
        pkgs <- lapply(seq_along(pkgs), function(i) {
            if (length(bibs[[i]]) > 1) 
                paste(prefix, pkgs[i], c("", 2:length(bibs[[i]])), 
                  sep = "")
            else paste(prefix, pkgs[i], sep = "")
        })
        pkgs <- do.call("c", pkgs)
        bibs <- do.call("c", bibs)
        as.bibkey <- function(x) {
            i <- grep("[.]", x)
            if (length(i) > 0) 
                x[i] <- paste("{", x[i], "}", sep = "")
            x
        }
        bibs <- mapply(function(b, k) {
            if (is.null(b$key)) 
                b$key <- k
            b
        }, bibs, pkgs, SIMPLIFY = FALSE)
        bibs <- do.call("c", bibs)
        if (verbose) 
            message("Converted ", n.converted, " of ", n.installed, 
                " package citations to BibTeX")
        bibs
    }
    else stop("Invalid argument `entry`: expected a bibentry object or a character vector of package names.")
    if (length(bibs) == 0) {
        if (verbose) 
            message("Empty bibentry list: nothing to be done.")
        return(invisible())
    }
    not_anonymous <- !identical(file, "")
    fh <- if (is.character(file)) {
        if (not_anonymous && !grepl("\\.bib$", file)) 
            file <- paste(file, ".bib", sep = "")
        fh <- file(file, open = if (append && not_anonymous) 
            "a+"
        else "w+")
        if (not_anonymous) 
            on.exit(if (isOpen(fh)) close(fh))
        fh
    }
    else if (is(file, "connection")) 
        file
    else stop("Invalid argument `file`: expected a filename, NULL, or a connection [", 
        class(file), "]")
    if (!is(fh, "connection")) 
        stop("Invalid connection: ", fh)
    file.desc <- summary(fh)["description"]
    if (verbose) 
        message(if (append) 
            "Adding "
        else "Writing ", length(bibs), " Bibtex entries ... ", 
            appendLF = FALSE)
    writeLines(toBibtex(bibs), fh)
    if (verbose) 
        message("OK\nResults written to file '", file.desc, "'")
    if (!not_anonymous) 
        attr(bibs, "connection") <- fh
    invisible(bibs)
}


is.dir <- function (x) 
file_test("-d", x)


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

testRversion <- function (x, test = 1L) 
{
    rv <- Rversion()
    op <- "=="
    if (grepl("^[=<>]", str_trim(x))) {
        m <- str_match(x, "^([<>=]=?)(.*)")
        if (is.na(m[, 1])) 
            stop("Invalid version specification: ", x)
        op <- m[, 2]
        if (op == "=") 
            op <- "=="
        x <- str_trim(m[, 3L])
        if (!missing(test)) 
            warning("Ignoring argument `test`: comparison operator was passed in argument `x`")
        test <- 0L
    }
    do.call(op, list(utils::compareVersion(rv, x), test))
}


hasArg2 <- function (name) 
{
    name <- as.name(name)
    aname <- as.character(substitute(name))
    fnames <- names(formals(sys.function(sys.parent())))
    if (is.na(match(aname, fnames))) {
        if (is.na(match("...", fnames))) 
            FALSE
        else {
            dotsCall <- eval(quote(substitute(list(...))), sys.parent())
            !is.na(match(aname, names(dotsCall)))
        }
    }
    else eval(substitute(!missing(name)), sys.frame(sys.parent()))
}


hasEnvar <- function (x) 
{
    is.na(Sys.getenv(x, unset = NA, names = FALSE))
}


as.rnw <- function (x, ..., load = TRUE) 
{
    if (is.rnw(x)) 
        return(x)
    checkRnwFile(x)
    obj <- list()
    class(obj) <- "rnw"
    obj$file <- normalizePath(x)
    obj$line <- NA
    if (!load) 
        return(obj)
    obj$compiler <- rnwCompiler(obj) %||% "Sweave"
    cl <- if (obj$compiler == "knitr") 
        "knitr"
    else "sweave"
    class(obj) <- c(paste("rnw_", cl, sep = ""), class(obj))
    obj$driver <- rnwDriver(obj) %||% RweaveLatex()
    obj$wrapper <- rnwWrapper(obj)
    obj$includes <- rnwIncludes(obj)
    obj$latexPackages <- rnwLatexPackages(obj)
    obj$children <- rnwChildren(obj)
    obj$cite <- rnwCite(obj)
    if (nargs() > 1L) {
        dots <- list(...)
        obj[names(dots)] <- dots
    }
    obj
}


addnames <- function (x, ...) 
{
    UseMethod("addnames")
}


isCHECK <- function () 
{
    isCRANcheck() || !isFALSE(utestCheckMode()) || isTRUE(getOption("R_CHECK_RUNNING_EXAMPLES_"))
}


citecmd <- function (key, ..., REFERENCES = NULL) 
{
    if (is.null(REFERENCES)) {
        if (hasArg(REFERENCES)) 
            .cache <<- .init
        if (is.null(.cache$REFERENCES)) {
            pkg <- Sys.getenv("R_PACKAGE_NAME")
            if (!nchar(pkg)) 
                pkg <- Sys.getenv("R_INSTALL_PKG")
            if (!nchar(pkg)) 
                pkg <- Sys.getenv("MAKE_R_PACKAGE")
            if (!nchar(pkg)) 
                stop("Could not identify package")
            .cache$REFERENCES <<- bibtex::read.bib(package = pkg)
        }
        REFERENCES <- .cache$REFERENCES
    }
    REFERENCES <- if (is(REFERENCES, "bibentry")) 
        REFERENCES
    else if (is.character(REFERENCES)) {
        p <- str_match(REFERENCES, "^package:(.*)")[, 2]
        if (is.na(p)) 
            bibtex::read.bib(file = REFERENCES)
        else bibtex::read.bib(package = p)
    }
    else stop("Invalid argument `REFERENCES`: expected bibentry object or character string [", 
        class(REFERENCES), "]")
    if (missing(key)) {
        .cache$REFERENCES <<- REFERENCES
        if (hasArg(REFERENCES)) 
            return(invisible(.cache$KEYS))
        else return(.cache$KEYS)
    }
    if (!is.character(key)) 
        stop("Invalid argument `key`: must be a character vector.")
    refkey <- sapply(REFERENCES, function(x) x$key)
    pkgs <- str_match(key, "^package:(.*)")[, 2]
    nokey <- !key %in% refkey
    i_pkgs <- which(nokey && !is.na(pkgs))
    if (length(i_pkgs) > 0L) {
        .cache$KEYS <<- unique(c(.cache$KEYS, key[i_pkgs]))
        key[i_pkgs] <- pkgs[i_pkgs]
    }
    paste("\\cite{", key, "}", sep = "")
}


packagePath <- function (..., package = NULL, lib.loc = NULL) 
{
    pname <- packageName(package)
    path <- NULL
    if (!is.null(info <- getLoadingNamespace(info = TRUE)) && 
        info$pkgname == pname) {
        path <- info$path
    }
    else {
        path <- find.package(package = pname, lib.loc = lib.loc, 
            quiet = TRUE)
    }
    if (!length(path) || path == "") {
        if (!is.null(info <- getLoadingNamespace(info = TRUE))) {
            path <- info$path
        }
    }
    stopifnot(!is.null(path) && path != "")
    if (isDevNamespace(pname)) {
        dots <- list(...)
        Rdirs <- c("data", "R", "src", "exec", "tests", "demo", 
            "exec", "libs", "man", "help", "html", "Meta")
        if (length(dots) && !sub("^/?([^/]+).*", "\\1", ..1) %in% 
            Rdirs) 
            path <- file.path(path, "inst")
    }
    file.path(path, ...)
}


checkWarning <- function (expr, expected = TRUE, msg = NULL) 
{
    uf <- requireRUnit()
    .testLogger <- if (.existsTestLogger()) 
        .GlobalEnv$.testLogger
    if (missing(expr)) {
        stop("'expr' is missing")
    }
    if (.existsTestLogger()) {
        .testLogger$incrementCheckNum()
    }
    pf <- parent.frame()
    warns <- NULL
    withCallingHandlers(eval(expr, envir = pf), warning = function(w) {
        warns <<- c(warns, w$message)
    })
    if (length(warns) == 0L) {
        if (isFALSE(expected)) 
            return(TRUE)
        if (.existsTestLogger()) {
            .testLogger$setFailure()
        }
        stop("Warning not generated as expected\n", msg)
    }
    if (isFALSE(expected)) {
        if (.existsTestLogger()) {
            .testLogger$setFailure()
        }
        stop("Warning generated while none was expected:\n", 
            "  - Warning(s): ", if (length(warns) > 1) 
                "\n    * ", str_out(warns, Inf, sep = "\n    * "), 
            "\n", msg)
    }
    if (is.null(expected) || isTRUE(expected)) 
        return(TRUE)
    if (any(grepl(expected, warns))) 
        return(TRUE)
    if (.existsTestLogger()) {
        .testLogger$setFailure()
    }
    stop("Warning does not match expected pattern:\n", "  - Warning(s): ", 
        if (length(warns) > 1) 
            "\n    * ", str_out(warns, Inf, sep = "\n    * "), 
        "\n", "  - Pattern: '", expected, "'\n", msg)
    TRUE
}


write_PACKAGES_index <- function (path = ".", output = "index.html", pattern = NULL, 
    title = "Packages", robots.file = TRUE) 
{
    dir <- path
    sel <- .PACKAGES_fields
    repo_dir <- normalizePath(dir)
    contrib_dir <- contrib.url(repo_dir)
    repo <- paste0("file://", repo_dir)
    contrib <- contrib.url(repo)
    contrib_path <- contrib.url(".")
    od <- setwd(repo_dir)
    on.exit(setwd(od))
    makePACKAGES <- function(dir = ".") {
        od <- setwd(dir)
        on.exit(setwd(od))
        smessage("Generating PACKAGES file for ", dir, " ... ")
        n <- tools::write_PACKAGES(".", fields = sel)
        message("OK [", n, "]")
        n
    }
    makePACKAGES(contrib_path)
    smessage("Generating HTML page in ", repo_dir, appendLF = TRUE)
    if (robots.file) {
        write("User-agent: *\nDisallow: /\n\n", file = file.path(repo_dir, 
            "robots.txt"))
    }
    smessage("Reading PACKAGES file in ", contrib_path, " ... ")
    p <- available.packages(contrib, fields = sel)
    message("OK [", nrow(p), "]")
    if (!is.null(pattern)) {
        smessage("Selecting packages matching pattern \"", pattern, 
            "\" only ... ")
        i <- grep(pattern, p[, "Package"])
        message("OK [", length(i), "/", nrow(p), "]")
        p <- p[i, , drop = FALSE]
    }
    df <- as.data.frame(p[, sel, drop = FALSE], stringsAsFactors = FALSE)
    smessage("Loading required packages ... ")
    qlibrary("ReportingTools")
    qlibrary("hwriter")
    message("OK")
    smessage("Generating ", output, " ... ")
    index <- HTMLReport(shortName = tools::file_path_sans_ext(output), 
        title = title)
    linkPackage <- function(df, ...) {
        pkg_src <- file.path(sub(file.path(repo, ""), "", contrib, 
            fixed = TRUE), as.character(df$Package))
        df$Package <- hwrite(as.character(df$Package), link = sprintf("%s_%s.tar.gz", 
            pkg_src, df$Version), table = FALSE)
        df
    }
    emailMaintainer <- function(df, ...) {
        if (!is.null(df$Maintainer)) {
            df$Maintainer <- gsub(".*<([^>]+)> *$", "\\1", df$Maintainer)
        }
        df
    }
    publish(knit2html(quiet = TRUE, text = "Install packages from this repository as follows (in an R console):\n                            \n```{r, eval = FALSE}\n# install BiocInstaller (only once)\nsource('http://bioiconductor.org/biocLite.R')\n                            \n# install package\nBiocInstaller::biocLite('<pkgname>', siteRepos = '<URL>')\n```", 
        fragment.only = TRUE), index)
    publish(df, index, name = title, .modifyDF = list(emailMaintainer, 
        linkPackage))
    finish(index)
    message("OK")
    message()
    invisible(normalizePath(output))
}


sVariable <- function (default = NULL) 
{
    .val <- default
    function(value) {
        if (missing(value)) 
            .val
        else {
            old <- .val
            .val <<- value
            old
        }
    }
}


install.extrapackages <- function (package, extra = NULL, handler = NULL, ..., .verbose = getOption("verbose")) 
{
    if (missing(handler)) 
        handler <- .handler
    .local <- function(p, ...) {
        extras <- packageExtra(handler = handler, extra = extra, 
            package = p)
        sapply(extras, function(def, ...) {
            e <- def$key
            h <- def$handler
            f <- packageExtra(handler = h, extra = e, package = p, 
                .wrap = TRUE)
            if (.verbose) {
                message("# Running extra action '", h, ":", e, 
                  "' ...")
                message("# Action: ", str_fun(f))
                on.exit(message("# ERROR [", e, "]\n"))
            }
            res <- f(...)
            if (.verbose) {
                on.exit()
                message("# OK [", e, "]\n")
            }
            res
        }, ...)
    }
    invisible(sapply(package, .local, ...))
}


isCRANcheck <- function (...) 
{
    tests <- list(...)
    if (!length(tests)) {
        tests <- list("timing", "cran")
    }
    test_sets <- c(timing = "_R_CHECK_TIMINGS_", cran = "_R_CHECK_CRAN_INCOMING_")
    tests <- sapply(tests, function(x) {
        if (length(i <- which(x %in% names(test_sets)))) {
            y <- test_sets[x[i]]
            x <- x[-i]
            x <- c(x, y)
        }
        evar <- unlist(sapply(x, Sys.getenv))
        all(nchar(as.character(evar)) > 0)
    })
    any(tests)
}


hasNames <- function (x, all = FALSE) 
{
    nm <- names(x)
    if (length(x) == 0L) 
        TRUE
    else !is.null(nm) && (!all || !is.element("", nm))
}


packageExtraRunner <- function (handler) 
{
    .handler <- handler
    function(package, extra = NULL, handler = NULL, ..., .verbose = getOption("verbose")) {
        if (missing(handler)) 
            handler <- .handler
        .local <- function(p, ...) {
            extras <- packageExtra(handler = handler, extra = extra, 
                package = p)
            sapply(extras, function(def, ...) {
                e <- def$key
                h <- def$handler
                f <- packageExtra(handler = h, extra = e, package = p, 
                  .wrap = TRUE)
                if (.verbose) {
                  message("# Running extra action '", h, ":", 
                    e, "' ...")
                  message("# Action: ", str_fun(f))
                  on.exit(message("# ERROR [", e, "]\n"))
                }
                res <- f(...)
                if (.verbose) {
                  on.exit()
                  message("# OK [", e, "]\n")
                }
                res
            }, ...)
        }
        invisible(sapply(package, .local, ...))
    }
}


RdSection2latex <- function (topic, package, i = 1L, notitle = TRUE) 
{
    rdsec <- getRdTag(topic, tag = "\\section", package = package)
    if (!length(rdsec)) 
        return()
    ltx <- capture.output(tools::Rd2latex(rdsec[i], fragment = TRUE))
    if (notitle) {
        parts <- stringr::str_match(ltx, "\\{Section\\}")
        w <- which(!is.na(parts[, 1]))
        ltx <- ltx[seq(w[1] + 1, tail(w, 1) - 1)]
    }
    ltx <- paste(ltx, collapse = "\n")
    ltx <- gsub("\\\\LinkA\\{([^}]+)\\}\\{([^}]+)\\}", "\\2", 
        ltx)
    cat(ltx)
    invisible()
}


setPackageRegistry <- function (regname, regobj, description = "", entrydesc = NA, 
    ..., package = topenv(parent.frame()), overwrite = FALSE) 
{
    if (missing(overwrite) && isDevNamespace(package)) {
        overwrite <- TRUE
    }
    oldreg <- packageRegistry(regname, quiet = TRUE, package = package)
    if (!is.null(oldreg) && !overwrite) {
        return(oldreg)
    }
    regenv <- .packageMetaRegistry(package, create = TRUE)
    nm <- packageSlot(regenv)
    ns_str <- str_c("package '", nm, "'")
    if (!is.null(oldreg)) {
        if (!overwrite) {
            if (isLoadingNamespace()) {
                message("NOTE: Did not create registry '", regname, 
                  "' in ", ns_str, ": registry already exists.")
                return(oldreg)
            }
            stop("Could not create registry '", regname, "' in ", 
                ns_str, ": registry already exists")
        }
        else {
            message("Remove registry '", regname, "' from ", 
                ns_str)
            regenv$delete_entry(regname)
        }
    }
    message("Creating registry '", regname, "' in ", ns_str, 
        " ... ", appendLF = FALSE)
    .add_regclass <- function(x, newcl, before) {
        cl <- class(x)
        ir <- which(cl == before)
        class(x) <- c(if (ir > 1) cl[1:(ir - 1)], newcl, cl[ir:length(cl)])
        x
    }
    pkgregclass <- c(paste(regname, "package_registry", sep = "_"), 
        "package_registry")
    if (is.character(regobj)) {
        objtype <- regobj[1]
        regobj <- registry(entry_class = paste(regname, "entry", 
            sep = "_"), registry_class = c(pkgregclass, "object_subregistry"))
        regobj$set_field("key", type = "character", is_key = TRUE, 
            index_FUN = match_partial_ignorecase)
        regobj$set_field("object", type = objtype, is_mandatory = TRUE, 
            validity_FUN = validObject)
    }
    else if (is(regobj, "registry")) {
        if (!is(regobj, "package_registry")) {
            regobj <- .add_regclass(regobj, pkgregclass, "registry")
        }
    }
    else {
        message("ERROR")
        stop("Invalid argument 'regobj': must be a class name or a registry object.")
    }
    if (!"REGISTERINGpackage" %in% regobj$get_field_names()) 
        regobj$set_field("REGISTERINGpackage", type = "character", 
            is_mandatory = TRUE, index_FUN = match_exact)
    regobj <- fix_registry(regobj)
    attr(regobj, "package") <- nm
    regenv$set_entry(key = regname, regobj = regobj, description = description, 
        entrydesc = entrydesc, ...)
    message("OK")
    regenv$get_entry(regname)$regobj
}


utest <- function (x, ...) 
standardGeneric("utest")


str_ns <- function (envir = packageEnv()) 
{
    if (!is.environment(envir)) 
        stop("Invalid argument: must be an environment [", class(envir), 
            "]")
    str_c(if (isNamespace(envir)) 
        "namespace"
    else "environment", " '", packageName(envir, rm.prefix = FALSE), 
        "'")
}


compile_src <- function (pkg = NULL, load = TRUE) 
{
    if (!is.null(pkg)) {
        library(devtools)
        p <- as.package(pkg)
        path <- p$path
    }
    else {
        pkg <- packageName()
        path <- packagePath(lib = NA)
    }
    owd <- getwd()
    on.exit(setwd(owd))
    srcdir <- file.path(path, "src")
    message("# Checking '", srcdir, "' ... ", appendLF = FALSE)
    if (!file.exists(srcdir)) {
        message("NO")
    }
    else {
        message("YES")
        message("## Compiling '", srcdir, "' ##")
        setwd(srcdir)
        Sys.setenv(R_PACKAGE_DIR = path)
        R.SHLIB(pkg, " *.cpp ")
        message("## DONE")
        if (load) {
            if (existsFunction("load_dll", where = "package:devtools")) {
                f <- getFunction("load_dll", where = "package:devtools")
                f(pkg)
            }
            else {
                f <- getFunction("load_c", where = "package:devtools")
                f(pkg)
            }
        }
    }
}


onLoad <- function (libname = NULL, pkgname, chname = packageName()) 
{
    if (!is.null(libname)) {
        if (file.exists(packagePath("libs"))) {
            sapply(chname, library.dynam, package = pkgname, 
                lib.loc = libname)
        }
    }
    else {
        compile_src()
    }
}


isReal <- function (x) 
{
    isNumber(x) && !is.integer(x)
}


packageExtra <- function (handler = NULL, extra = NULL, package = NULL, .wrap = FALSE) 
{
    extras <- pkgreg_fetch("extra_action", key = extra, handler = handler, 
        package = package, exact = TRUE, all = !.wrap)
    if (missing(handler) || is.null(extra) || !.wrap) 
        return(extras)
    args <- extras$args
    fhandler <- packageExtraHandler(handler, package = "pkgmaker")
    if (is.null(fhandler)) {
        handlers <- packageExtraHandler(package = "pkgmaker")
        stop("Could not find action handler '", handler, "' in pkgmaker global handler registry.\n", 
            "  Available handlers are: ", str_out(handlers, Inf))
    }
    .wrapExtra(fhandler, args)
}


inSweave <- function () 
{
    if ((n.parents <- length(sys.parents())) >= 3) {
        for (i in seq_len(n.parents) - 1) {
            if ("chunkopts" %in% ls(envir = sys.frame(i))) {
                chunkopts = get("chunkopts", envir = sys.frame(i))
                if (all(c("prefix.string", "label") %in% names(chunkopts))) {
                  return(TRUE)
                  break
                }
            }
        }
    }
    FALSE
}


sortVersion <- function (x, ...) 
{
    x[orderVersion(x, ...)]
}


file_extension <- function (x, ext = NULL) 
{
    if (is.null(ext)) 
        sub(".*\\.([^.]{3})$", "\\1", x)
    else str_c(sub("(.*)(\\.([^.]{3}))$", "\\1", x), ".", sub("^.", 
        "", ext))
}


rnw <- function (x, file = NULL, ..., raw = FALSE) 
{
    x <- as.rnw(x, ...)
    opts <- options()
    gpar <- par(no.readonly = TRUE)
    on.exit({
        options(opts)
        par(gpar)
    })
    if ("cleveref" %in% x$latexPackages) {
        clv <- packagePath("cleveref.sty", package = "pkgmaker")
        message("# Copying package 'cleveref.sty' from ", dirname(clv), 
            " ... ", appendLF = FALSE)
        wd <- if (!is.null(file)) 
            dirname(file)
        else getwd()
        file.copy(clv, wd)
        if (file.exists(file.path(wd, basename(clv)))) 
            message("OK")
        else message("ERROR")
    }
    res <- runVignette(x, file = file, ...)
    if (!is.null(keys <- x$cite)) {
        message("# Writing package bibtex file [", length(keys), 
            " key(s)] ... ", appendLF = FALSE)
        write.pkgbib(keys, file = "Rpackages.bib", prefix = "Rpackage:", 
            verbose = FALSE)
        message("OK")
    }
    if (raw) 
        return(res)
    if (!is.null(x$wrapper)) {
        res <- x$wrapper
    }
    invisible(res)
}


topns <- function (strict = TRUE) 
{
    ns <- topns_name(n = 1L, strict = strict)
    if (ns == ".GlobalEnv") 
        return(.GlobalEnv)
    else if (nchar(ns)) 
        asNamespace(ns)
}


require.quiet <- function (package, character.only = FALSE, ...) 
{
    if (!character.only) 
        package <- as.character(substitute(package))
    utils::capture.output(suppressMessages(suppressWarnings(res <- do.call("require", 
        list(package = package, ..., character.only = TRUE, quietly = TRUE)))))
    res
}


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

mfrow <- function (n) 
{
    if (n == 1) 
        c(1, 1)
    else if (n == 2) 
        c(1, 2)
    else if (n <= 4) 
        c(2, 2)
    else if (n <= 6) 
        c(3, 2)
    else if (n <= 9) 
        c(3, 3)
    else {
        sn <- floor(n/3)
        c(sn + if (sn%%3) 1 else 0, 3)
    }
}


source_files <- function (x, pattern = NULL, ...) 
{
    if (length(x) == 1L && is.dir(x)) 
        x <- list.files(x, pattern = pattern, full.names = TRUE)
    invisible(sapply(x, source, ...))
}


as.package_options <- function (..., defaults = NULL) 
{
    args <- .list_or_named_dots(...)
    x <- if (is.null(names(args))) 
        args[[1]]
    if (!is.null(names(args))) 
        defaults <- args
    if (is.null(x)) 
        x <- basename(tempfile(""))
    if (is.package_options(x)) {
        if (!missing(defaults) && is.list(defaults)) {
            optname <- basename(tempfile(str_c(x$name, "_")))
            x <- as.package_options(x$.options, defaults)
            x$name <- optname
        }
        return(x)
    }
    .OPTOBJ <- structure(list2env(list(name = NULL, .options = NULL, 
        .defaults = defaults)), class = "package_options")
    if (is.character(x)) {
        x <- sub("^package:", "", x)
        .OPTOBJ$name <- paste("package:", x[1L], sep = "")
    }
    else if (is.list(x)) {
        .OPTOBJ$name <- tempfile("package:")
        .OPTOBJ$.options <- x
    }
    else stop("Invalid argument `x`: must be a character string or a list.")
    .OPTOBJ$options <- function(...) {
        .options(..., .DATA = .OPTOBJ)
    }
    .OPTOBJ$getOption <- function(x, default = NULL) {
        options <- .OPTOBJ$options
        if (missing(default)) 
            return(options(x)[[1L]])
        if (x %in% names(options())) 
            options(x)[[1L]]
        else default
    }
    .OPTOBJ$newOptions <- function(...) {
        defs <- .list_or_named_dots(..., named.only = TRUE)
        lapply(seq_along(defs), function(i) {
            name <- names(defs)[i]
            value <- defs[[i]]
            in_opts <- name %in% names(.OPTOBJ$.defaults) && 
                !identical(.OPTOBJ$.defaults[[name]], value)
            if (in_opts && !isLoadingNamespace()) {
                message("Skipping option ", .OPTOBJ$name, "::`", 
                  name, "`: already defined with another default value")
            }
            else {
                if (in_opts) 
                  message("Overwriting option ", .OPTOBJ$name, 
                    "::`", name, "` : already defined with another default value")
                .OPTOBJ$.defaults[[name]] <- value
                .OPTOBJ$.options[[name]] <- value
            }
        })
        invisible()
    }
    .OPTOBJ$resetOptions <- function(..., ALL = FALSE) {
        defaults <- .OPTOBJ$.defaults
        if (ALL) {
            .OPTOBJ$.options <- NULL
        }
        if (length(list(...)) > 0L) {
            onames <- c(...)
            if (!is.character(onames)) 
                stop("character strings expected for resetting option names")
            defaults <- defaults[names(defaults) %in% onames]
            if (length(not_default <- onames[!onames %in% names(defaults)])) {
                .OPTOBJ$.options[not_default] <- NULL
            }
        }
        if (length(defaults)) {
            .OPTOBJ$options(defaults)
        }
    }
    .OPTOBJ$printOptions <- function() print(.OPTOBJ)
    .OPTOBJ$resetOptions()
    .OPTOBJ
}


expand_dots <- function (..., .exclude = NULL) 
{
    dotsCall <- as.list(eval(quote(substitute(list(...))), sys.parent()))
    if (length(dotsCall) >= 1L) 
        dotsCall <- dotsCall[-1L]
    defaults <- list(...)
    if (length(defaults) == 1L && is.null(names(defaults))) {
        defaults <- defaults[[1L]]
    }
    if (length(defaults)) {
        excl <- names(allFormals(sys.function(sys.parent())))
        if (!is.null(.exclude)) 
            excl <- c(excl, .exclude)
        defaults <- defaults[!names(defaults) %in% excl]
        dotsCall <- expand_list(dotsCall, defaults, .exact = FALSE)
    }
    dotsCall
}


hook_toggle <- function () 
{
    .init <- TRUE
    .last_label <- NULL
    fn <- chunkOutputHook("toggle", type = "source", function(x, 
        options) {
        opt <- options$toggle
        label <- options$label
        if (!isTRUE(opt) && !isFALSE(opt)) 
            return(x)
        if (.init) {
            x <- paste0(.js_toggle_fun, x)
            .init <<- FALSE
        }
        disp <- if (opt) 
            "Chunk_block"
        else "Chunk_none"
        id <- paste0("Rcode_", label)
        subst <- paste0("```{", id, " \\1 ", disp, "}\n")
        if (!identical(label, .last_label)) {
            .last_label <<- label
            subst <- paste0("<a href=\"\" onclick=\"toggle_vis2('", 
                id, "'); return false;\">Show/Hide R code</a>\n", 
                subst)
        }
        sub("```([^\n]*)\n", sprintf(subst, "block"), x)
    })
    fn()
}


add_lib <- function (..., append = FALSE) 
{
    p <- if (append) 
        c(.libPaths(), ...)
    else c(..., .libPaths())
    .libPaths(p)
}


new2 <- function (class, ...) 
{
    sl <- getSlots(class)
    if (nargs() == 1L) 
        return(new(class))
    dots <- list(...)
    if (nargs() == 2L && is.null(names(dots))) {
        l <- dots[[1]]
        if (!is.list(l)) 
            stop("Invalid call: single unnamed argument must be a list")
        dots <- l
    }
    if (is.null(names(dots)) || any(names(dots) == "")) 
        stop("Invalid call: all slot arguments must be named")
    dots <- dots[names(dots) %in% names(sl)]
    do.call("new", c(list(class), dots))
}


isInteger <- function (x) 
{
    is.integer(x) && length(x) == 1
}


listPackageOptions <- function () 
{
    grep("^package:", names(options()), value = TRUE)
}


compactVignettes <- function (paths, ...) 
{
    td <- tempfile(basename(paths))
    file.copy(paths, td)
    res <- tools::compactPDF(td, gs_quality = "none", ...)
    diff_none <- format(res, diff = 1e+05)
    res <- tools::compactPDF(td, gs_quality = "ebook", ...)
    diff_ebook <- format(res, diff = 250000)
    if (length(diff_ebook)) {
        tools::compactPDF(paths, gs_quality = "ebook", ...)
        invisible("ebook")
    }
    else {
        tools::compactPDF(paths, gs_quality = "none", ...)
        invisible("none")
    }
}


pkgreg_fetch <- function (regname, ..., msg = NULL, where = topenv(parent.frame())) 
{
    regentry <- packageRegistry(regname, package = where, entry = TRUE, 
        update = TRUE)
    if (missing(msg) && !is_NA(regentry$entrydesc)) 
        msg <- regentry$entrydesc
    regfetch(regentry$regobj, ..., msg = msg)
}


isFALSE <- function (x) 
identical(x, FALSE)


getLoadingNamespace <- function (env = FALSE, info = FALSE, nodev = FALSE) 
{
    is.loading <- try(nsInfo <- loadingNamespaceInfo(), silent = TRUE)
    if (!is(is.loading, "try-error")) {
        if (env) 
            asNamespace(as.name(nsInfo$pkgname))
        else if (info) {
            nsInfo$path <- file.path(nsInfo$libname, nsInfo$pkgname)
            nsInfo
        }
        else nsInfo$pkgname
    }
    else if (!nodev) {
        if (is_pkgcall("devtools") && (i <- is_funcall(devtools::load_all))) {
            e <- sys.frame(i)
            pkg <- e$pkg
            if (env) 
                asNamespace(pkg$package)
            else if (info) {
                list(pkgname = pkg$package, path = pkg$path, 
                  libname = dirname(pkg$path))
            }
            else pkg$package
        }
    }
    else NULL
}


packageTestEnv <- function (pkg) 
{
    if (!missing(pkg) && !is.null(pkg)) {
        e <- packageEnv(pkg)
        return(e$.packageTest)
    }
    e <- packageEnv()
    if (is.null(e$.packageTest)) 
        e$.packageTest <- new.env(parent = e)
    e$.packageTest
}


isNumber <- function (x) 
{
    is.numeric(x) && length(x) == 1
}


regfetch <- function (regobj, ..., all = FALSE, error = TRUE, exact = FALSE, 
    KEYS = NULL, verbose = FALSE, entry = FALSE, msg = NULL) 
{
    keylist <- allkeys <- regobj$get_entry_names()
    if (!all) 
        keylist <- grep("^[^.]", keylist, value = TRUE)
    index_fields <- if (!is.null(KEYS)) {
        if (!is.list(KEYS)) 
            stop("Invalid argument <KEYS>: must be a list of field values.")
        KEYS
    }
    else list(...)
    key <- if (length(index_fields)) {
        if (!is.null(names(index_fields))) 
            index_fields <- regkeys(regobj, index_fields)
        if (length(index_fields)) {
            paste(unlist(index_fields), collapse = "_")
            str_out(index_fields, Inf, use.names = TRUE)
        }
    }
    if (is.null(key)) {
        return(keylist)
    }
    if (!missing(verbose)) {
        ol <- lverbose(verbose)
        on.exit(lverbose(ol))
    }
    if (!is.null(msg)) 
        msg <- str_c(msg, " - ")
    if (regobj$n_of_entries() == 0L) {
        if (error) 
            stop(msg, "Registry is empty: no matching entry for key ", 
                dQuote(key), ".")
        else return(NULL)
    }
    d <- do.call(regobj$get_entries, index_fields)
    if (is.null(d)) {
        if (error) {
            stop(msg, "No matching entry for key ", dQuote(key), 
                " in the registry.", "\n  Use one of: ", str_wrap(str_out(sort(allkeys), 
                  Inf), exdent = 2), ".")
        }
        else return(NULL)
    }
    if (is.list(index_fields)) {
        ex <- sapply(d, function(x) all(mapply(identical, index_fields, 
            x[names(index_fields)])))
    }
    else {
        ex <- names(d) == index_fields
    }
    if (length(i <- which(ex))) {
        d <- d[i]
    }
    else if (exact) {
        if (error) {
            stop(msg, "No exact match for key '", key, "' in the registry.", 
                "\n  Use one of: ", str_wrap(str_out(allkeys, 
                  Inf), exdent = 2), ".")
        }
        else return(NULL)
    }
    if (all) 
        return(d)
    if (length(d) > 1L) {
        if (error) {
            stop(msg, "Multiple entries found for key ", dQuote(key), 
                ": ", str_out(sort(names(d)), Inf), ".")
        }
        else return(NA)
    }
    if (length(d) != 1L) 
        stop("Unexpected error: more than one entry was selected.")
    d <- d[[1L]]
    if (!entry && is(regobj, "object_subregistry")) 
        d$object
    else d
}


isNamespaceLoaded <- function (ns) 
{
    if (is.environment(ns)) {
        if (!isNamespace(ns)) 
            return(FALSE)
        else ns <- getPackageName(ns)
    }
    if (isString(ns)) 
        ns %in% loadedNamespaces()
    else stop("Invalid argument `ns`: only support strings and environments.")
}


str_diff <- function (x, y) 
{
    sx <- strsplit(x, "")[[1]]
    sy <- strsplit(y, "")[[1]]
    n <- min(length(sx), length(sy))
    res <- mapply("!=", head(sx, n), head(sy, n))
    wres <- which(res)
    if (length(sx) > length(sy)) 
        wres <- c(wres, (n + 1):length(sx))
    attr(wres, "str") <- list(x = x, y = y)
    class(wres) <- "str_diff"
    wres
}


setupPackageOptions <- function (..., NAME = NULL, ENVIR = topenv(parent.frame()), RESET = isLoadingNamespace()) 
{
    defaults <- .list_or_named_dots(...)
    e <- parent.frame()
    if (missing(ENVIR) && identical(e, .GlobalEnv)) {
        ENVIR <- NULL
    }
    pkg <- packageName(.Global = TRUE)
    fprefix <- if (is.null(NAME)) 
        tolower(pkg)
    else NAME
    optname <- pkg
    if (!is.null(NAME)) 
        optname <- paste(optname, NAME, sep = ":")
    optobj <- as.package_options(optname, defaults = defaults)
    OLD <- getOption(optobj$name)
    if (!is.null(OLD) && !RESET) 
        stop("Package specific options '", OLD$name, "' already exist: ", 
            " (", length(OLD$options()), " default option(s))")
    message(if (is.null(OLD)) 
        "Setting"
    else "Resetting", " package specific options: ", optobj$name, 
        " (", length(optobj$options()), " default option(s))")
    options(setNames(list(optobj), optobj$name))
    optobj <- getOption(optobj$name)
    stopifnot(!is.null(optobj))
    if (!is.null(ENVIR)) {
        isfun <- unlist(eapply(optobj, is.function))
        isfun <- isfun[names(isfun) != "newOptions"]
        ifun <- which(isfun)
        lapply(names(isfun)[ifun], function(x) {
            f <- get(x, envir = optobj)
            assign(paste(fprefix, x, sep = "."), f, envir = ENVIR)
        })
    }
    optobj
}


addNamespaceExport <- function (x) 
{
    ns <- pkgmaker::getLoadingNamespace(env = TRUE)
    if (!is.null(ns)) {
        namespaceExport(ns, x)
    }
}


makeFakeVignette <- function (src, out, PACKAGE = NULL) 
{
    if (!is.null(PACKAGE)) {
        src <- str_c(, src)
    }
    if (identical(normalizePath(dirname(src)), normalizePath(dirname(out)))) {
        cat("# NOTE: skipped fake vignette [source in root directory]\n")
        return(invisible())
    }
    l <- readLines(src)
    vign <- l[grep("^%\\s*\\\\Vignette", l)]
    cat(c("\\documentclass[10pt]{article}", vign, "\\usepackage{url}\n\\usepackage[colorlinks]{hyperref}\n\n\\begin{document}\n\\end{document}"), 
        file = out, sep = "\n")
}


setPackageRegistryEntry <- function (regname, key, ..., overwrite = FALSE, verbose = FALSE, 
    where = topenv(parent.frame()), msg = NULL) 
{
    if (isLoadingNamespace()) {
        verbose <- TRUE
        if (missing(overwrite)) 
            overwrite <- TRUE
    }
    registry <- regname
    package <- where
    if (nchar(key) == 0) 
        stop("Invalid argument <key>: cannot be an empty string.")
    fullkey <- key
    top_ns <- topns(strict = FALSE)
    package <- packageEnv(package)
    subregentry <- packageRegistry(registry, package = package, 
        entry = TRUE, update = TRUE)
    regobj <- subregentry$regobj
    fields <- list(...)
    objdesc <- if (!is_NA(subregentry$entrydesc)) 
        subregentry$entrydesc
    else paste(registry, "object")
    objdesc <- paste(objdesc, " '", key, "'", sep = "")
    if (length(fields) == 1L) {
        objdesc <- paste(objdesc, " [", class(fields[[1L]]), 
            "]", sep = "")
        if (is.null(names(fields)) && is(regobj, "object_subregistry")) 
            names(fields) <- "object"
    }
    fields$key <- key
    regpkg <- packageName(top_ns, .Global = TRUE)
    fields$REGISTERINGpackage <- regpkg
    oldentry <- regfetch(regobj, KEYS = fields, exact = TRUE, 
        error = FALSE, all = TRUE)
    if (!is.null(oldentry) && !overwrite) {
        if (verbose) 
            message("ERROR")
        stop("Cannot register ", objdesc, ": key already exists.")
    }
    if (verbose) {
        action <- if (is.null(oldentry)) 
            "Registering"
        else "Replacing"
        message(action, " ", objdesc, msg, " ... ", appendLF = FALSE)
    }
    if (!is.null(oldentry)) {
        regobj$delete_entry(names(oldentry)[1L])
    }
    do.call(regobj$set_entry, fields)
    if (verbose) 
        message("OK")
    lns <- getLoadingNamespace(env = TRUE)
    if (!is.null(lns <- getLoadingNamespace(env = TRUE)) && !identical(lns, 
        package)) {
        if (nchar(subregentry$parent)) {
            warning("Deriving package registry '", registry, 
                "' in package ", lns, " from ", subregentry$parent, 
                " instead of ", subregentry$package, immediate. = TRUE)
            parent <- subregentry$parent
        }
        else parent <- subregentry$package
        fullregistry <- str_c(parent, "::", registry)
        if (is.null(locregobj <- packageRegistry(fullregistry, 
            package = lns, quiet = TRUE))) {
            locregobj <- clone_regobj(regobj, empty = TRUE)
            locregobj <- setPackageRegistry(fullregistry, locregobj, 
                description = subregentry$description, entrydesc = subregentry$entrydesc, 
                parent = parent, package = lns)
        }
        action <- "Adding"
        if (!is.null(locentry <- regfetch(locregobj, KEYS = fields, 
            exact = TRUE, error = FALSE, all = TRUE))) {
            action <- "Overwriting"
            locregobj$delete_entry(names(locentry)[1L])
        }
        if (verbose) 
            message(action, " entry '", key, "' in registry '", 
                fullregistry, "' ... ", appendLF = FALSE)
        do.call(locregobj$set_entry, fields)
        if (verbose) 
            message("OK")
    }
    regfetch(regobj, KEYS = fields, exact = TRUE)
}


rnwCompiler <- function (x, verbose = TRUE) 
{
    x <- rnwObject(x)
    l <- readLines(x$file)
    dr <- str_match(l, str_c("^\\s*", if (commented) 
        "%", "\\s*\\\\", tag, if (options) 
        "(\\[[^]]*\\])?", "\\{([^}]*)\\}"))
    w <- which(!is.na(dr[, 1L]))
    if (length(w) > 0L) {
        if (first) 
            w <- w[1L]
        s <- dr[w, if (options) 
            3L
        else 2L]
        if (trim) 
            s <- str_trim(s)
        if (verbose) 
            rnw_message("Detected ", name, ": ", paste("'", s, 
                "'", sep = "", collapse = ", "))
        s
    }
}


rnwDriver <- function (x) 
{
    parse_driver <- rnwVignetteParser("Driver", trim = FALSE)
    if (!is.null(s <- parse_driver(x))) {
        eval(parse(text = s))
    }
}


Rversion <- function () 
{
    paste(R.version$major, R.version$minor, sep = ".")
}


setPackageExtraHandler <- function (handler, fun, ...) 
{
    setPackageRegistryEntry("extra_handler", handler, fun, ...)
    runner <- packageExtraRunner(handler)
}


list.libs <- function (dir, ..., all.platforms = FALSE) 
{
    p <- if (!all.platforms) {
        str_c("\\", .Platform$dynlib.ext, "$")
    }
    else {
        p <- str_c("(\\.", c("so", "dll"), , ")", collapse = "|")
        str_c(p, "$")
    }
    list.files(dir, pattern = p, ...)
}


packageDependencies <- function (x, all = TRUE, as.list = FALSE, available = NULL) 
{
    if (is.null(available)) 
        x <- as.package(x, extract = TRUE)
    else {
        p <- available[, "Package"]
        if (!x %in% p) 
            return(NA)
        x <- available[p == x, , drop = FALSE][1L, ]
        names(x) <- tolower(names(x))
    }
    d <- lapply(x[c("depends", "imports", "linkingto", "suggests")], 
        parse_deps)
    d <- unlist(d)
    d <- d[!is.na(d)]
    if (!length(d)) 
        return()
    names(d) <- gsub("[0-9]+$", "", names(d))
    if (!all) 
        d <- d[!names(d) %in% c("suggests")]
    if (as.list) 
        d <- split(unname(d), names(d))
    d
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Package development utilities"

.skeleton_package_version = "0.22"

.skeleton_package_depends = "stats,registry"

.skeleton_package_imports = "methods,tools,codetools,digest,stringr,xtable,grDevices"


## Internal

.skeleton_version = 5


## EOF