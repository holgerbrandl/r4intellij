# This file contains skeletons for base package.


## @type recursive : logical
## @rule (... : T, recursive = FALSE) -> max(T)
c <- function (..., recursive = FALSE)  .Primitive("c")

## @return numeric
length <- function (x)  .Primitive("length")

## @optional x
invisible <- function (x)  .Primitive("invisible")

## @optional env
substitute <- function (expr, env)  .Primitive("substitute")

## @optional pattern, name
ls <- function (name, pos = -1L, envir = as.environment(pos), all.names = FALSE,
    pattern)
{
    if (!missing(name)) {
        pos <- tryCatch(name, error = function(e) e)
        if (inherits(pos, "error")) {
            name <- substitute(name)
            if (!is.character(name))
                name <- deparse(name)
            warning(gettextf("%s converted to character string",
                sQuote(name)), domain = NA)
            pos <- name
        }
    }
    all.names <- .Internal(ls(envir, all.names))
    if (!missing(pattern)) {
        if ((ll <- length(grep("[", pattern, fixed = TRUE))) &&
            ll != length(grep("]", pattern, fixed = TRUE))) {
            if (pattern == "[") {
                pattern <- "\\["
                warning("replaced regular expression pattern '[' by  '\\\\['")
            }
            else if (length(grep("[^\\\\]\\[<-", pattern))) {
                pattern <- sub("\\[<-", "\\\\\\[<-", pattern)
                warning("replaced '[<-' by '\\\\[<-' in regular expression pattern")
            }
        }
        grep(pattern, all.names, value = TRUE)
    }
    else all.names
}

## @type generic : character
## @optional object
UseMethod <- function (generic, object)  .Primitive("UseMethod")

## @type command : character
## @type stdout : character | logical
## @type wait : logical
## @type args : character
## @type input : character
## @type stderr : character | logical
## @type env : character
## @type stdin : character
system2 <- function (command, args = character(), stdout = "", stderr = "",
    stdin = "", input = NULL, env = character(), wait = TRUE,
    minimized = FALSE, invisible = TRUE)
{
    if (!missing(minimized) || !missing(invisible))
        message("arguments 'minimized' and 'invisible' are for Windows only")
    if (!is.logical(wait) || is.na(wait))
        stop("'wait' must be TRUE or FALSE")
    intern <- FALSE
    command <- paste(c(env, shQuote(command), args), collapse = " ")
    if (is.null(stdout))
        stdout <- FALSE
    if (is.null(stderr))
        stderr <- FALSE
    if (isTRUE(stderr)) {
        if (!isTRUE(stdout))
            warning("setting stdout = TRUE")
        stdout <- TRUE
    }
    if (identical(stdout, FALSE))
        command <- paste(command, ">/dev/null")
    else if (isTRUE(stdout))
        intern <- TRUE
    else if (is.character(stdout)) {
        if (length(stdout) != 1L)
            stop("'stdout' must be of length 1")
        if (nzchar(stdout)) {
            command <- if (identical(stdout, stderr))
                paste(command, ">", shQuote(stdout), "2>&1")
            else command <- paste(command, ">", shQuote(stdout))
        }
    }
    if (identical(stderr, FALSE))
        command <- paste(command, "2>/dev/null")
    else if (isTRUE(stderr)) {
        command <- paste(command, "2>&1")
    }
    else if (is.character(stderr)) {
        if (length(stderr) != 1L)
            stop("'stderr' must be of length 1")
        if (nzchar(stderr) && !identical(stdout, stderr))
            command <- paste(command, "2>", shQuote(stderr))
    }
    if (!is.null(input)) {
        if (!is.character(input))
            stop("'input' must be a character vector or 'NULL'")
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(input, f)
        command <- paste(command, "<", shQuote(f))
    }
    else if (nzchar(stdin))
        command <- paste(command, "<", stdin)
    if (!wait && !intern)
        command <- paste(command, "&")
    .Internal(system(command, intern))
}

## @optional frame
exists <- function (x, where = -1, envir = if (missing(frame)) as.environment(where) else sys.frame(frame),
    frame, mode = "any", inherits = TRUE)
.Internal(exists(x, envir, mode, inherits))

## @optional text
scan <- function (file = "", what = double(), nmax = -1L, n = -1L, sep = "",
    quote = if (identical(sep, "\n")) "" else "'\"", dec = ".",
    skip = 0L, nlines = 0L, na.strings = "NA", flush = FALSE,
    fill = FALSE, strip.white = FALSE, quiet = FALSE, blank.lines.skip = TRUE,
    multi.line = TRUE, comment.char = "", allowEscapes = FALSE,
    fileEncoding = "", encoding = "unknown", text, skipNul = FALSE)
{
    na.strings <- as.character(na.strings)
    if (!missing(n)) {
        if (missing(nmax))
            nmax <- n/pmax(length(what), 1L)
        else stop("either specify 'nmax' or 'n', but not both.")
    }
    if (missing(file) && !missing(text)) {
        file <- textConnection(text, encoding = "UTF-8")
        encoding <- "UTF-8"
        on.exit(close(file))
    }
    if (is.character(file))
        if (file == "")
            file <- stdin()
        else {
            file <- if (nzchar(fileEncoding))
                file(file, "r", encoding = fileEncoding)
            else file(file, "r")
            on.exit(close(file))
        }
    if (!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    .Internal(scan(file, what, nmax, sep, dec, quote, skip, nlines,
        na.strings, flush, fill, strip.white, quiet, blank.lines.skip,
        multi.line, comment.char, allowEscapes, encoding, skipNul))
}

## @optional finally
tryCatch <- function (expr, ..., finally)
{
    tryCatchList <- function(expr, names, parentenv, handlers) {
        nh <- length(names)
        if (nh > 1L)
            tryCatchOne(tryCatchList(expr, names[-nh], parentenv,
                handlers[-nh]), names[nh], parentenv, handlers[[nh]])
        else if (nh == 1L)
            tryCatchOne(expr, names, parentenv, handlers[[1L]])
        else expr
    }
    tryCatchOne <- function(expr, name, parentenv, handler) {
        doTryCatch <- function(expr, name, parentenv, handler) {
            .Internal(.addCondHands(name, list(handler), parentenv,
                environment(), FALSE))
            expr
        }
        value <- doTryCatch(return(expr), name, parentenv, handler)
        if (is.null(value[[1L]])) {
            msg <- .Internal(geterrmessage())
            call <- value[[2L]]
            cond <- simpleError(msg, call)
        }
        else cond <- value[[1L]]
        value[[3L]](cond)
    }
    if (!missing(finally))
        on.exit(finally)
    handlers <- list(...)
    classes <- names(handlers)
    parentenv <- parent.frame()
    if (length(classes) != length(handlers))
        stop("bad handler specification")
    tryCatchList(expr, classes, parentenv, handlers)
}

## @optional help
## @return logical
library <- function (package, help, pos = 2, lib.loc = NULL, character.only = FALSE,
    logical.return = FALSE, warn.conflicts = TRUE, quietly = FALSE,
    verbose = getOption("verbose"))
{
    testRversion <- function(pkgInfo, pkgname, pkgpath) {
        if (is.null(built <- pkgInfo$Built))
            stop(gettextf("package %s has not been installed properly\n",
                sQuote(pkgname)), call. = FALSE, domain = NA)
        R_version_built_under <- as.numeric_version(built$R)
        if (R_version_built_under < "3.0.0")
            stop(gettextf("package %s was built before R 3.0.0: please re-install it",
                sQuote(pkgname)), call. = FALSE, domain = NA)
        current <- getRversion()
        if (length(Rdeps <- pkgInfo$Rdepends2)) {
            for (dep in Rdeps) if (length(dep) > 1L) {
                target <- dep$version
                res <- if (is.character(target)) {
                  do.call(dep$op, list(as.numeric(R.version[["svn rev"]]),
                    as.numeric(sub("^r", "", dep$version))))
                }
                else {
                  do.call(dep$op, list(current, as.numeric_version(target)))
                }
                if (!res)
                  stop(gettextf("This is R %s, package %s needs %s %s",
                    current, sQuote(pkgname), dep$op, target),
                    call. = FALSE, domain = NA)
            }
        }
        if (R_version_built_under > current)
            warning(gettextf("package %s was built under R version %s",
                sQuote(pkgname), as.character(built$R)), call. = FALSE,
                domain = NA)
        platform <- built$Platform
        r_arch <- .Platform$r_arch
        if (.Platform$OS.type == "unix") {
            if (!nzchar(r_arch) && length(grep("\\w", platform)) &&
                !testPlatformEquivalence(platform, R.version$platform))
                stop(gettextf("package %s was built for %s",
                  sQuote(pkgname), platform), call. = FALSE,
                  domain = NA)
        }
        else {
            if (nzchar(platform) && !grepl("mingw", platform))
                stop(gettextf("package %s was built for %s",
                  sQuote(pkgname), platform), call. = FALSE,
                  domain = NA)
        }
        if (nzchar(r_arch) && file.exists(file.path(pkgpath,
            "libs")) && !file.exists(file.path(pkgpath, "libs",
            r_arch)))
            stop(gettextf("package %s is not installed for 'arch = %s'",
                sQuote(pkgname), r_arch), call. = FALSE, domain = NA)
    }
    checkLicense <- function(pkg, pkgInfo, pkgPath) {
        L <- tools:::analyze_license(pkgInfo$DESCRIPTION["License"])
        if (!L$is_empty && !L$is_verified) {
            site_file <- path.expand(file.path(R.home("etc"),
                "licensed.site"))
            if (file.exists(site_file) && pkg %in% readLines(site_file))
                return()
            personal_file <- path.expand("~/.R/licensed")
            if (file.exists(personal_file)) {
                agreed <- readLines(personal_file)
                if (pkg %in% agreed)
                  return()
            }
            else agreed <- character()
            if (!interactive())
                stop(gettextf("package %s has a license that you need to accept in an interactive session",
                  sQuote(pkg)), domain = NA)
            lfiles <- file.path(pkgpath, c("LICENSE", "LICENCE"))
            lfiles <- lfiles[file.exists(lfiles)]
            if (length(lfiles)) {
                message(gettextf("package %s has a license that you need to accept after viewing",
                  sQuote(pkg)), domain = NA)
                readline("press RETURN to view license")
                encoding <- pkgInfo$DESCRIPTION["Encoding"]
                if (is.na(encoding))
                  encoding <- ""
                if (encoding == "latin1")
                  encoding <- "cp1252"
                file.show(lfiles[1L], encoding = encoding)
            }
            else {
                message(gettextf("package %s has a license that you need to accept:\naccording to the DESCRIPTION file it is",
                  sQuote(pkg)), domain = NA)
                message(pkgInfo$DESCRIPTION["License"], domain = NA)
            }
            choice <- menu(c("accept", "decline"), title = paste("License for",
                sQuote(pkg)))
            if (choice != 1)
                stop(gettextf("license for package %s not accepted",
                  sQuote(package)), domain = NA, call. = FALSE)
            dir.create(dirname(personal_file), showWarnings = FALSE)
            writeLines(c(agreed, pkg), personal_file)
        }
    }
    checkNoGenerics <- function(env, pkg) {
        nenv <- env
        ns <- .getNamespace(as.name(pkg))
        if (!is.null(ns))
            nenv <- asNamespace(ns)
        if (exists(".noGenerics", envir = nenv, inherits = FALSE))
            TRUE
        else {
            length(objects(env, pattern = "^\\.__T", all.names = TRUE)) ==
                0L
        }
    }
    checkConflicts <- function(package, pkgname, pkgpath, nogenerics,
        env) {
        dont.mind <- c("last.dump", "last.warning", ".Last.value",
            ".Random.seed", ".Last.lib", ".onDetach", ".packageName",
            ".noGenerics", ".required", ".no_S3_generics", ".Depends",
            ".requireCachedGenerics")
        sp <- search()
        lib.pos <- match(pkgname, sp)
        ob <- objects(lib.pos, all.names = TRUE)
        if (!nogenerics) {
            these <- ob[substr(ob, 1L, 6L) == ".__T__"]
            gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
            from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
            gen <- gen[from != package]
            ob <- ob[!(ob %in% gen)]
        }
        fst <- TRUE
        ipos <- seq_along(sp)[-c(lib.pos, match(c("Autoloads",
            "CheckExEnv"), sp, 0L))]
        for (i in ipos) {
            obj.same <- match(objects(i, all.names = TRUE), ob,
                nomatch = 0L)
            if (any(obj.same > 0)) {
                same <- ob[obj.same]
                same <- same[!(same %in% dont.mind)]
                Classobjs <- grep("^\\.__", same)
                if (length(Classobjs))
                  same <- same[-Classobjs]
                same.isFn <- function(where) vapply(same, exists,
                  NA, where = where, mode = "function", inherits = FALSE)
                same <- same[same.isFn(i) == same.isFn(lib.pos)]
                not.Ident <- function(ch, TRAFO = identity, ...) vapply(ch,
                  function(.) !identical(TRAFO(get(., i)), TRAFO(get(.,
                    lib.pos)), ...), NA)
                if (length(same))
                  same <- same[not.Ident(same)]
                if (length(same) && identical(sp[i], "package:base"))
                  same <- same[not.Ident(same, ignore.environment = TRUE)]
                if (length(same)) {
                  if (fst) {
                    fst <- FALSE
                    packageStartupMessage(gettextf("\nAttaching package: %s\n",
                      sQuote(package)), domain = NA)
                  }
                  msg <- .maskedMsg(same, pkg = sQuote(sp[i]),
                    by = i < lib.pos)
                  packageStartupMessage(msg, domain = NA)
                }
            }
        }
    }
    if (verbose && quietly)
        message("'verbose' and 'quietly' are both true; being verbose then ..")
    if (!missing(package)) {
        if (is.null(lib.loc))
            lib.loc <- .libPaths()
        lib.loc <- lib.loc[file.info(lib.loc)$isdir %in% TRUE]
        if (!character.only)
            package <- as.character(substitute(package))
        if (length(package) != 1L)
            stop("'package' must be of length 1")
        if (is.na(package) || (package == ""))
            stop("invalid package name")
        pkgname <- paste("package", package, sep = ":")
        newpackage <- is.na(match(pkgname, search()))
        if (newpackage) {
            pkgpath <- find.package(package, lib.loc, quiet = TRUE,
                verbose = verbose)
            if (length(pkgpath) == 0L) {
                txt <- if (length(lib.loc))
                  gettextf("there is no package called %s", sQuote(package))
                else gettext("no library trees found in 'lib.loc'")
                if (logical.return) {
                  warning(txt, domain = NA)
                  return(FALSE)
                }
                else stop(txt, domain = NA)
            }
            which.lib.loc <- normalizePath(dirname(pkgpath),
                "/", TRUE)
            pfile <- system.file("Meta", "package.rds", package = package,
                lib.loc = which.lib.loc)
            if (!nzchar(pfile))
                stop(gettextf("%s is not a valid installed package",
                  sQuote(package)), domain = NA)
            pkgInfo <- readRDS(pfile)
            testRversion(pkgInfo, package, pkgpath)
            if (!package %in% c("datasets", "grDevices", "graphics",
                "methods", "splines", "stats", "stats4", "tcltk",
                "tools", "utils") && isTRUE(getOption("checkPackageLicense",
                FALSE)))
                checkLicense(package, pkgInfo, pkgpath)
            if (is.character(pos)) {
                npos <- match(pos, search())
                if (is.na(npos)) {
                  warning(gettextf("%s not found on search path, using pos = 2",
                    sQuote(pos)), domain = NA)
                  pos <- 2
                }
                else pos <- npos
            }
            .getRequiredPackages2(pkgInfo, quietly = quietly)
            deps <- unique(names(pkgInfo$Depends))
            if (packageHasNamespace(package, which.lib.loc)) {
                tt <- try({
                  ns <- loadNamespace(package, c(which.lib.loc,
                    lib.loc))
                  env <- attachNamespace(ns, pos = pos, deps)
                })
                if (inherits(tt, "try-error"))
                  if (logical.return)
                    return(FALSE)
                  else stop(gettextf("package or namespace load failed for %s",
                    sQuote(package)), call. = FALSE, domain = NA)
                else {
                  on.exit(detach(pos = pos))
                  nogenerics <- !.isMethodsDispatchOn() || checkNoGenerics(env,
                    package)
                  if (warn.conflicts && !exists(".conflicts.OK",
                    envir = env, inherits = FALSE))
                    checkConflicts(package, pkgname, pkgpath,
                      nogenerics, ns)
                  on.exit()
                  if (logical.return)
                    return(TRUE)
                  else return(invisible(.packages()))
                }
            }
            else stop(gettextf("package %s does not have a namespace and should be re-installed",
                sQuote(package)), domain = NA)
        }
        if (verbose && !newpackage)
            warning(gettextf("package %s already present in search()",
                sQuote(package)), domain = NA)
    }
    else if (!missing(help)) {
        if (!character.only)
            help <- as.character(substitute(help))
        pkgName <- help[1L]
        pkgPath <- find.package(pkgName, lib.loc, verbose = verbose)
        docFiles <- c(file.path(pkgPath, "Meta", "package.rds"),
            file.path(pkgPath, "INDEX"))
        if (file.exists(vignetteIndexRDS <- file.path(pkgPath,
            "Meta", "vignette.rds")))
            docFiles <- c(docFiles, vignetteIndexRDS)
        pkgInfo <- vector("list", 3L)
        readDocFile <- function(f) {
            if (basename(f) %in% "package.rds") {
                txt <- readRDS(f)$DESCRIPTION
                if ("Encoding" %in% names(txt)) {
                  to <- if (Sys.getlocale("LC_CTYPE") == "C")
                    "ASCII//TRANSLIT"
                  else ""
                  tmp <- try(iconv(txt, from = txt["Encoding"],
                    to = to))
                  if (!inherits(tmp, "try-error"))
                    txt <- tmp
                  else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible",
                    call. = FALSE)
                }
                nm <- paste0(names(txt), ":")
                formatDL(nm, txt, indent = max(nchar(nm, "w")) +
                  3)
            }
            else if (basename(f) %in% "vignette.rds") {
                txt <- readRDS(f)
                if (is.data.frame(txt) && nrow(txt))
                  cbind(basename(gsub("\\.[[:alpha:]]+$", "",
                    txt$File)), paste(txt$Title, paste0(rep.int("(source",
                    NROW(txt)), ifelse(txt$PDF != "", ", pdf",
                    ""), ")")))
                else NULL
            }
            else readLines(f)
        }
        for (i in which(file.exists(docFiles))) pkgInfo[[i]] <- readDocFile(docFiles[i])
        y <- list(name = pkgName, path = pkgPath, info = pkgInfo)
        class(y) <- "packageInfo"
        return(y)
    }
    else {
        if (is.null(lib.loc))
            lib.loc <- .libPaths()
        db <- matrix(character(), nrow = 0L, ncol = 3L)
        nopkgs <- character()
        for (lib in lib.loc) {
            a <- .packages(all.available = TRUE, lib.loc = lib)
            for (i in sort(a)) {
                file <- system.file("Meta", "package.rds", package = i,
                  lib.loc = lib)
                title <- if (file != "") {
                  txt <- readRDS(file)
                  if (is.list(txt))
                    txt <- txt$DESCRIPTION
                  if ("Encoding" %in% names(txt)) {
                    to <- if (Sys.getlocale("LC_CTYPE") == "C")
                      "ASCII//TRANSLIT"
                    else ""
                    tmp <- try(iconv(txt, txt["Encoding"], to,
                      "?"))
                    if (!inherits(tmp, "try-error"))
                      txt <- tmp
                    else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible",
                      call. = FALSE)
                  }
                  txt["Title"]
                }
                else NA
                if (is.na(title))
                  title <- " ** No title available ** "
                db <- rbind(db, cbind(i, lib, title))
            }
            if (length(a) == 0L)
                nopkgs <- c(nopkgs, lib)
        }
        dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
        if (length(nopkgs) && !missing(lib.loc)) {
            pkglist <- paste(sQuote(nopkgs), collapse = ", ")
            msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages",
                "libraries %s contain no packages"), pkglist)
            warning(msg, domain = NA)
        }
        y <- list(header = NULL, results = db, footer = NULL)
        class(y) <- "libraryIQR"
        return(y)
    }
    if (logical.return)
        TRUE
    else invisible(.packages())
}


## @type length : numeric
## @type mode : character
## @rule (mode="logical")->logical
## @rule (mode="numeric")->numeric
## @rule (mode="integer")->integer
## @rule (mode="double")->numeric
## @rule (mode="complex")->complex
## @rule (mode="character")->character
## @rule (mode="raw")->raw
## @rule (mode=string) -> error(Wrong mode)
vector <- function (mode = "logical", length = 0L)
.Internal(vector(mode, length))

## @return character
## @type length : numeric
character <- function (length = 0L)
.Internal(vector("character", length))


## @return raw
## @type length : numeric
raw <- function (length = 0L)
.Internal(vector("raw", length))

## @return numeric
## @type length : numeric
double <- function (length = 0L)
.Internal(vector("double", length))

## @type imaginary : numeric
## @type length.out : numeric
## @type modulus : numeric
## @type argument : numeric
## @type real : numeric
## @return complex
complex <- function (length.out = 0L, real = numeric(), imaginary = numeric(),
    modulus = 1, argument = 0)
{
    if (missing(modulus) && missing(argument)) {
        .Internal(complex(length.out, real, imaginary))
    }
    else {
        n <- max(length.out, length(argument), length(modulus))
        rep_len(modulus, n) * exp((0+1i) * rep_len(argument,
            n))
    }
}

## @return logical
## @type length : numeric
logical <- function (length = 0L)
.Internal(vector("logical", length))

## @return integer
## @type length : numeric
integer <- function (length = 0L)
.Internal(vector("integer", length))

## @rule (data : T) -> T[matrix]
matrix <- function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL)
{
    if (is.object(data) || !is.atomic(data))
        data <- as.vector(data)
    .Internal(matrix(data, nrow, ncol, byrow, dimnames, missing(nrow),
        missing(ncol)))
}

## @optional e2
## @type e1 : complex
## @type e2 : complex
"+" <- function (e1, e2)  .Primitive("+")

## @optional e2
## @type e1 : complex
## @type e2 : complex
"-" <- function (e1, e2)  .Primitive("-")

## @optional fdef
standardGeneric <- function (f, fdef)  .Primitive("standardGeneric")

isTRUE <- function (x)
identical(TRUE, x)