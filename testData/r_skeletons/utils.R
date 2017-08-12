##
## Exported symobls in package `utils`
##

## Exported package methods

aspell_package_Rd_files <- function (dir, drop = c("\\author", "\\references"), control = list(), 
    program = NULL, dictionaries = character()) 
{
    dir <- normalizePath(dir, "/")
    subdir <- file.path(dir, "man")
    files <- if (dir.exists(subdir)) 
        tools::list_files_with_type(subdir, "docs", OS_subdirs = c("unix", 
            "windows"))
    else character()
    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if (is.na(encoding <- meta["Encoding"])) 
        encoding <- "unknown"
    defaults <- .aspell_package_defaults(dir, encoding)$Rd_files
    if (!is.null(defaults)) {
        if (!is.null(d <- defaults$drop)) 
            drop <- d
        if (!is.null(d <- defaults$control)) 
            control <- d
        if (!is.null(d <- defaults$program)) 
            program <- d
        if (!is.null(d <- defaults$dictionaries)) {
            dictionaries <- aspell_find_dictionaries(d, file.path(dir, 
                ".aspell"))
        }
        if (!is.null(d <- defaults$personal)) 
            control <- c(control, sprintf("-p %s", shQuote(file.path(dir, 
                ".aspell", d))))
    }
    aspell(files, filter = list("Rd", drop = drop), control = control, 
        encoding = encoding, program = program, dictionaries = dictionaries)
}


vi <- function (name = NULL, file = "") 
edit.default(name, file, editor = "vi")


read.table <- function (file, header = FALSE, sep = "", quote = "\"'", dec = ".", 
    numerals = c("allow.loss", "warn.loss", "no.loss"), row.names, 
    col.names, as.is = !stringsAsFactors, na.strings = "NA", 
    colClasses = NA, nrows = -1, skip = 0, check.names = TRUE, 
    fill = !blank.lines.skip, strip.white = FALSE, blank.lines.skip = TRUE, 
    comment.char = "#", allowEscapes = FALSE, flush = FALSE, 
    stringsAsFactors = default.stringsAsFactors(), fileEncoding = "", 
    encoding = "unknown", text, skipNul = FALSE) 
{
    if (missing(file) && !missing(text)) {
        file <- textConnection(text, encoding = "UTF-8")
        encoding <- "UTF-8"
        on.exit(close(file))
    }
    if (is.character(file)) {
        file <- if (nzchar(fileEncoding)) 
            file(file, "rt", encoding = fileEncoding)
        else file(file, "rt")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    if (!isOpen(file, "rt")) {
        open(file, "rt")
        on.exit(close(file))
    }
    pbEncoding <- if (encoding %in% c("", "bytes", "UTF-8")) 
        encoding
    else "bytes"
    numerals <- match.arg(numerals)
    if (skip > 0L) 
        readLines(file, skip)
    nlines <- n0lines <- if (nrows < 0L) 
        5
    else min(5L, (header + nrows))
    lines <- .External(C_readtablehead, file, nlines, comment.char, 
        blank.lines.skip, quote, sep, skipNul)
    if (encoding %in% c("UTF-8", "latin1")) 
        Encoding(lines) <- encoding
    nlines <- length(lines)
    if (!nlines) {
        if (missing(col.names)) 
            stop("no lines available in input")
        rlabp <- FALSE
        cols <- length(col.names)
    }
    else {
        if (all(!nzchar(lines))) 
            stop("empty beginning of file")
        if (nlines < n0lines && file == 0L) {
            pushBack(c(lines, lines, ""), file, encoding = pbEncoding)
            on.exit((clearPushBack(stdin())))
        }
        else pushBack(c(lines, lines), file, encoding = pbEncoding)
        first <- scan(file, what = "", sep = sep, quote = quote, 
            nlines = 1, quiet = TRUE, skip = 0, strip.white = TRUE, 
            blank.lines.skip = blank.lines.skip, comment.char = comment.char, 
            allowEscapes = allowEscapes, encoding = encoding, 
            skipNul = skipNul)
        col1 <- if (missing(col.names)) 
            length(first)
        else length(col.names)
        col <- numeric(nlines - 1L)
        if (nlines > 1L) 
            for (i in seq_along(col)) col[i] <- length(scan(file, 
                what = "", sep = sep, quote = quote, nlines = 1, 
                quiet = TRUE, skip = 0, strip.white = strip.white, 
                blank.lines.skip = blank.lines.skip, comment.char = comment.char, 
                allowEscapes = allowEscapes, encoding = encoding, 
                skipNul = skipNul))
        cols <- max(col1, col)
        rlabp <- (cols - col1) == 1L
        if (rlabp && missing(header)) 
            header <- TRUE
        if (!header) 
            rlabp <- FALSE
        if (header) {
            .External(C_readtablehead, file, 1L, comment.char, 
                blank.lines.skip, quote, sep, skipNul)
            if (missing(col.names)) 
                col.names <- first
            else if (length(first) != length(col.names)) 
                warning("header and 'col.names' are of different lengths")
        }
        else if (missing(col.names)) 
            col.names <- paste0("V", 1L:cols)
        if (length(col.names) + rlabp < cols) 
            stop("more columns than column names")
        if (fill && length(col.names) > cols) 
            cols <- length(col.names)
        if (!fill && cols > 0L && length(col.names) > cols) 
            stop("more column names than columns")
        if (cols == 0L) 
            stop("first five rows are empty: giving up")
    }
    if (check.names) 
        col.names <- make.names(col.names, unique = TRUE)
    if (rlabp) 
        col.names <- c("row.names", col.names)
    nmColClasses <- names(colClasses)
    if (is.null(nmColClasses)) {
        if (length(colClasses) < cols) 
            colClasses <- rep_len(colClasses, cols)
    }
    else {
        tmp <- rep_len(NA_character_, cols)
        names(tmp) <- col.names
        i <- match(nmColClasses, col.names, 0L)
        if (any(i <= 0L)) 
            warning("not all columns named in 'colClasses' exist")
        tmp[i[i > 0L]] <- colClasses[i > 0L]
        colClasses <- tmp
    }
    what <- rep.int(list(""), cols)
    names(what) <- col.names
    colClasses[colClasses %in% c("real", "double")] <- "numeric"
    known <- colClasses %in% c("logical", "integer", "numeric", 
        "complex", "character", "raw")
    what[known] <- sapply(colClasses[known], do.call, list(0))
    what[colClasses %in% "NULL"] <- list(NULL)
    keep <- !sapply(what, is.null)
    data <- scan(file = file, what = what, sep = sep, quote = quote, 
        dec = dec, nmax = nrows, skip = 0, na.strings = na.strings, 
        quiet = TRUE, fill = fill, strip.white = strip.white, 
        blank.lines.skip = blank.lines.skip, multi.line = FALSE, 
        comment.char = comment.char, allowEscapes = allowEscapes, 
        flush = flush, encoding = encoding, skipNul = skipNul)
    nlines <- length(data[[which.max(keep)]])
    if (cols != length(data)) {
        warning("cols = ", cols, " != length(data) = ", length(data), 
            domain = NA)
        cols <- length(data)
    }
    if (is.logical(as.is)) {
        as.is <- rep_len(as.is, cols)
    }
    else if (is.numeric(as.is)) {
        if (any(as.is < 1 | as.is > cols)) 
            stop("invalid numeric 'as.is' expression")
        i <- rep.int(FALSE, cols)
        i[as.is] <- TRUE
        as.is <- i
    }
    else if (is.character(as.is)) {
        i <- match(as.is, col.names, 0L)
        if (any(i <= 0L)) 
            warning("not all columns named in 'as.is' exist")
        i <- i[i > 0L]
        as.is <- rep.int(FALSE, cols)
        as.is[i] <- TRUE
    }
    else if (length(as.is) != cols) 
        stop(gettextf("'as.is' has the wrong length %d  != cols = %d", 
            length(as.is), cols), domain = NA)
    do <- keep & !known
    if (rlabp) 
        do[1L] <- FALSE
    for (i in (1L:cols)[do]) {
        data[[i]] <- if (is.na(colClasses[i])) 
            type.convert(data[[i]], as.is = as.is[i], dec = dec, 
                numerals = numerals, na.strings = character(0L))
        else if (colClasses[i] == "factor") 
            as.factor(data[[i]])
        else if (colClasses[i] == "Date") 
            as.Date(data[[i]])
        else if (colClasses[i] == "POSIXct") 
            as.POSIXct(data[[i]])
        else methods::as(data[[i]], colClasses[i])
    }
    compactRN <- TRUE
    if (missing(row.names)) {
        if (rlabp) {
            row.names <- data[[1L]]
            data <- data[-1L]
            keep <- keep[-1L]
            compactRN <- FALSE
        }
        else row.names <- .set_row_names(as.integer(nlines))
    }
    else if (is.null(row.names)) {
        row.names <- .set_row_names(as.integer(nlines))
    }
    else if (is.character(row.names)) {
        compactRN <- FALSE
        if (length(row.names) == 1L) {
            rowvar <- (1L:cols)[match(col.names, row.names, 0L) == 
                1L]
            row.names <- data[[rowvar]]
            data <- data[-rowvar]
            keep <- keep[-rowvar]
        }
    }
    else if (is.numeric(row.names) && length(row.names) == 1L) {
        compactRN <- FALSE
        rlabp <- row.names
        row.names <- data[[rlabp]]
        data <- data[-rlabp]
        keep <- keep[-rlabp]
    }
    else stop("invalid 'row.names' specification")
    data <- data[keep]
    if (is.object(row.names) || !(is.integer(row.names))) 
        row.names <- as.character(row.names)
    if (!compactRN) {
        if (length(row.names) != nlines) 
            stop("invalid 'row.names' length")
        if (anyDuplicated(row.names)) 
            stop("duplicate 'row.names' are not allowed")
        if (anyNA(row.names)) 
            stop("missing values in 'row.names' are not allowed")
    }
    class(data) <- "data.frame"
    attr(data, "row.names") <- row.names
    data
}


URLdecode <- function (URL) 
{
    x <- charToRaw(URL)
    pc <- charToRaw("%")
    out <- raw(0L)
    i <- 1L
    while (i <= length(x)) {
        if (x[i] != pc) {
            out <- c(out, x[i])
            i <- i + 1L
        }
        else {
            y <- as.integer(x[i + 1L:2L])
            y[y > 96L] <- y[y > 96L] - 32L
            y[y > 57L] <- y[y > 57L] - 7L
            y <- sum((y - 48L) * c(16L, 1L))
            out <- c(out, as.raw(as.character(y)))
            i <- i + 3L
        }
    }
    rawToChar(out)
}


rc.status <- function () 
{
    as.list(.CompletionEnv)
}


write.csv <- function (...) 
{
    Call <- match.call(expand.dots = TRUE)
    for (argname in c("append", "col.names", "sep", "dec", "qmethod")) if (!is.null(Call[[argname]])) 
        warning(gettextf("attempt to set '%s' ignored", argname), 
            domain = NA)
    rn <- eval.parent(Call$row.names)
    Call$append <- NULL
    Call$col.names <- if (is.logical(rn) && !rn) 
        TRUE
    else NA
    Call$sep <- ","
    Call$dec <- "."
    Call$qmethod <- "double"
    Call[[1L]] <- as.name("write.table")
    eval.parent(Call)
}


RweaveLatexOptions <- function (options) 
{
    defaults <- options[[".defaults"]]
    c2l <- function(x) if (is.null(x)) 
        FALSE
    else suppressWarnings(as.logical(x))
    NUMOPTS <- c("width", "height", "resolution")
    CHAROPTS <- c("results", "prefix.string", "engine", "label", 
        "strip.white", "pdf.version", "pdf.encoding", "grdevice")
    for (opt in names(options)) {
        if (opt == ".defaults") 
            next
        oldval <- options[[opt]]
        defval <- defaults[[opt]]
        if (opt %in% CHAROPTS || is.character(defval)) {
        }
        else if (is.logical(defval)) 
            options[[opt]] <- c2l(oldval)
        else if (opt %in% NUMOPTS || is.numeric(defval)) 
            options[[opt]] <- as.numeric(oldval)
        else if (!is.na(newval <- c2l(oldval))) 
            options[[opt]] <- newval
        else if (!is.na(newval <- suppressWarnings(as.numeric(oldval)))) 
            options[[opt]] <- newval
        if (is.na(options[[opt]])) 
            stop(gettextf("invalid value for %s : %s", sQuote(opt), 
                oldval), domain = NA)
    }
    if (!is.null(options$results)) {
        res <- as.character(options$results)
        if (tolower(res) != res) 
            warning("value of 'results' option should be lowercase", 
                call. = FALSE)
        options$results <- tolower(res)
    }
    options$results <- match.arg(options$results, c("verbatim", 
        "tex", "hide"))
    if (!is.null(options$strip.white)) {
        res <- as.character(options$strip.white)
        if (tolower(res) != res) 
            warning("value of 'strip.white' option should be lowercase", 
                call. = FALSE)
        options$strip.white <- tolower(res)
    }
    options$strip.white <- match.arg(options$strip.white, c("true", 
        "false", "all"))
    options
}


formatUL <- function (x, label = "*", offset = 0, width = 0.9 * getOption("width")) 
{
    if (!length(x)) 
        return(character())
    .format_rl_table(label, x, offset, width)
}


prompt <- function (object, filename = NULL, name = NULL, ...) 
UseMethod("prompt")


upgrade <- function (object, ...) 
UseMethod("upgrade")


RShowDoc <- function (what, type = c("pdf", "html", "txt"), package) 
{
    paste. <- function(x, ext) paste(x, ext, sep = ".")
    pdf_viewer <- function(path) {
        pdfviewer <- getOption("pdfviewer")
        if (identical(pdfviewer, "false")) {
        }
        else if (.Platform$OS.type == "windows" && identical(pdfviewer, 
            file.path(R.home("bin"), "open.exe"))) 
            shell.exec(path)
        else system2(pdfviewer, shQuote(path), wait = FALSE)
    }
    html_viewer <- function(path) {
        browser <- getOption("browser")
        if (is.null(browser) && .Platform$OS.type == "windows") 
            shell.exec(chartr("/", "\\", path))
        else browseURL(paste0("file://", URLencode(path)))
    }
    type <- match.arg(type)
    if (missing(what) || length(what) != 1L || !is.character(what)) {
        message("   RShowDoc() should be used with a character string argument specifying\n   a documentation file")
        return(invisible())
    }
    if (!missing(package)) {
        pkgpath <- find.package(package)
        if (type == "pdf") {
            path <- file.path(pkgpath, "doc", paste.(what, "pdf"))
            if (file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            path <- file.path(pkgpath, paste.(what, "pdf"))
            if (file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            type <- "html"
        }
        if (type == "html") {
            path <- file.path(pkgpath, "doc", paste.(what, "html"))
            if (file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
            path <- file.path(pkgpath, paste.(what, "html"))
            if (file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        path <- file.path(pkgpath, "doc", what)
        if (file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        path <- file.path(pkgpath, what)
        if (file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        stop(gettextf("no documentation for %s found in package %s", 
            sQuote(what), sQuote(package)), domain = NA)
    }
    if (what == "FAQ") 
        what <- "R-FAQ"
    if (what == "NEWS") {
        if (type == "pdf") {
            path <- file.path(R.home("doc"), paste.(what, "pdf"))
            if (file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            type <- "html"
        }
        if (type == "html") {
            path <- file.path(R.home("doc"), "html", paste.(what, 
                "html"))
            if (file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        path <- file.path(R.home("doc"), what)
        tf <- tempfile()
        tmp <- readLines(path)
        tmp[1] <- ""
        writeLines(tmp, tf)
        file.show(tf, delete.file = TRUE, encoding = "UTF-8")
        return(invisible(path))
    }
    else if (what == "COPYING") {
        path <- file.path(R.home("doc"), what)
        file.show(path)
        return(invisible(path))
    }
    else if (what %in% dir(file.path(R.home("share"), "licenses"))) {
        path <- file.path(R.home("share"), "licenses", what)
        file.show(path)
        return(invisible(path))
    }
    else if (what %in% c("R-admin", "R-data", "R-exts", "R-FAQ", 
        "R-intro", "R-ints", "R-lang")) {
        if (type == "pdf") {
            path <- file.path(R.home("doc"), "manual", paste.(what, 
                "pdf"))
            if (file.exists(path)) {
                pdf_viewer(path)
                return(invisible(path))
            }
            type <- "html"
        }
        if (type == "html") {
            path <- file.path(R.home("doc"), "manual", paste.(what, 
                "html"))
            if (file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        if (what == "R-FAQ" && file.exists(path <- file.path(R.home("doc"), 
            "FAQ"))) {
            file.show(path)
            return(invisible(path))
        }
    }
    else if (.Platform$OS.type == "windows" && what %in% "rw-FAQ") {
        if (type == "pdf") 
            type <- "html"
        if (type == "html") {
            path <- file.path(R.home("doc"), "html", paste.(what, 
                "html"))
            if (file.exists(path)) {
                html_viewer(path)
                return(invisible(path))
            }
        }
        path <- file.path(R.home("doc"), what)
        if (file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
        path <- file.path(R.home(), "src", "gnuwin32", what)
        if (file.exists(path)) {
            file.show(path)
            return(invisible(path))
        }
    }
    else {
        rdocdir <- R.home("doc")
        docs <- dir(rdocdir, full.names = TRUE)
        docs <- docs[sapply(docs, function(x) file_test("-f", 
            x))]
        m <- match(what, basename(docs), 0L)
        if (m > 0L) {
            file.show(docs[m])
            return(invisible(docs[m]))
        }
    }
    stop("document not found")
}


argsAnywhere <- function (x) 
{
    if (tryCatch(!is.character(x), error = function(e) TRUE)) 
        x <- as.character(substitute(x))
    fs <- getAnywhere(x)
    if (sum(!fs$dups) == 0L) 
        return(NULL)
    if (sum(!fs$dups) > 1L) 
        sapply(fs$objs[!fs$dups], function(f) if (is.function(f)) 
            args(f))
    else args(fs$objs[[1L]])
}


read.delim <- function (file, header = TRUE, sep = "\t", quote = "\"", dec = ".", 
    fill = TRUE, comment.char = "", ...) 
read.table(file = file, header = header, sep = sep, quote = quote, 
    dec = dec, fill = fill, comment.char = comment.char, ...)


de.ncols <- function (inlist) 
{
    ncols <- matrix(0, nrow = length(inlist), ncol = 2L)
    i <- 1L
    for (telt in inlist) {
        if (is.matrix(telt)) {
            ncols[i, 1L] <- ncol(telt)
            ncols[i, 2L] <- 2L
        }
        else if (is.list(telt)) {
            for (telt2 in telt) if (!is.vector(telt2)) 
                stop("wrong argument to 'dataentry'")
            ncols[i, 1L] <- length(telt)
            ncols[i, 2L] <- 3L
        }
        else if (is.vector(telt)) {
            ncols[i, 1L] <- 1L
            ncols[i, 2L] <- 1L
        }
        else stop("wrong argument to 'dataentry'")
        i <- i + 1L
    }
    return(ncols)
}


SweaveSyntConv <- function (file, syntax, output = NULL) 
{
    if (is.character(syntax)) 
        syntax <- get(syntax)
    if (!identical(class(syntax), "SweaveSyntax")) 
        stop(gettextf("target syntax not of class %s", dQuote("SweaveSyntax")), 
            domain = NA)
    if (is.null(syntax$trans)) 
        stop("target syntax contains no translation table")
    insynt <- SweaveGetSyntax(file)
    text <- readLines(file)
    if (is.null(output)) 
        output <- sub(insynt$extension, syntax$trans$extension, 
            basename(file))
    TN <- names(syntax$trans)
    for (n in TN) if (n != "extension") 
        text <- gsub(insynt[[n]], syntax$trans[[n]], text)
    cat(text, file = output, sep = "\n")
    cat("Wrote file", output, "\n")
}


new.packages <- function (lib.loc = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
    type), instPkgs = installed.packages(lib.loc = lib.loc), 
    method, available = NULL, ask = FALSE, ..., type = getOption("pkgType")) 
{
    ask
    if (type == "both" && (!missing(contriburl) || !is.null(available))) {
        stop("specifying 'contriburl' or 'available' requires a single type, not type = \"both\"")
    }
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    if (!is.matrix(instPkgs)) 
        stop(gettextf("no installed packages for (invalid?) 'lib.loc=%s'", 
            lib.loc), domain = NA)
    if (is.null(available)) 
        available <- available.packages(contriburl = contriburl, 
            method = method)
    installed <- unique(instPkgs[, "Package"])
    poss <- sort(unique(available[, "Package"]))
    res <- setdiff(poss, installed)
    update <- character()
    graphics <- FALSE
    if (is.character(ask) && ask == "graphics") {
        ask <- TRUE
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA" || (capabilities("tcltk") && capabilities("X11"))) 
            graphics <- TRUE
    }
    if (isTRUE(ask)) {
        if (length(res)) 
            update <- res[match(select.list(res, multiple = TRUE, 
                title = "New packages to be installed", graphics = graphics), 
                res)]
        else message("no new packages are available")
    }
    if (length(update)) {
        if (type == "both") 
            install.packages(update, lib = lib.loc[1L], method = method, 
                type = type, ...)
        else install.packages(update, lib = lib.loc[1L], contriburl = contriburl, 
            method = method, available = available, type = type, 
            ...)
        dirs <- list.files(lib.loc[1L])
        updated <- update[update %in% dirs]
        res <- res[!res %in% updated]
    }
    res
}


read.socket <- function (socket, maxlen = 256L, loop = FALSE) 
{
    repeat {
        rval <- .Call(C_sockread, socket$socket, maxlen)
        if (nzchar(rval) || !loop) 
            break
    }
    rval
}


personList <- function (...) 
{
    z <- list(...)
    if (!all(sapply(z, inherits, "person"))) 
        stop(gettextf("all arguments must be of class %s", dQuote("person")), 
            domain = NA)
    do.call("c", z)
}


write.table <- function (x, file = "", append = FALSE, quote = TRUE, sep = " ", 
    eol = "\n", na = "NA", dec = ".", row.names = TRUE, col.names = TRUE, 
    qmethod = c("escape", "double"), fileEncoding = "") 
{
    qmethod <- match.arg(qmethod)
    if (is.logical(quote) && (length(quote) != 1L || is.na(quote))) 
        stop("'quote' must be 'TRUE', 'FALSE' or numeric")
    quoteC <- if (is.logical(quote)) 
        quote
    else TRUE
    qset <- is.logical(quote) && quote
    if (!is.data.frame(x) && !is.matrix(x)) 
        x <- data.frame(x)
    makeRownames <- isTRUE(row.names)
    makeColnames <- is.logical(col.names) && !identical(FALSE, 
        col.names)
    if (is.matrix(x)) {
        p <- ncol(x)
        d <- dimnames(x)
        if (is.null(d)) 
            d <- list(NULL, NULL)
        if (is.null(d[[1L]]) && makeRownames) 
            d[[1L]] <- seq_len(nrow(x))
        if (is.null(d[[2L]]) && makeColnames && p > 0L) 
            d[[2L]] <- paste0("V", 1L:p)
        if (qset) 
            quote <- if (is.character(x)) 
                seq_len(p)
            else numeric()
    }
    else {
        if (qset) 
            quote <- if (length(x)) 
                which(unlist(lapply(x, function(x) is.character(x) || 
                  is.factor(x))))
            else numeric()
        if (any(sapply(x, function(z) length(dim(z)) == 2 && 
            dim(z)[2L] > 1))) {
            c1 <- names(x)
            x <- as.matrix(x, rownames.force = makeRownames)
            d <- dimnames(x)
            if (qset) {
                ord <- match(c1, d[[2L]], 0L)
                quote <- ord[quote]
                quote <- quote[quote > 0L]
            }
        }
        else d <- list(if (makeRownames) row.names(x), if (makeColnames) names(x))
        p <- ncol(x)
    }
    nocols <- p == 0L
    if (is.logical(quote)) 
        quote <- NULL
    else if (is.numeric(quote)) {
        if (any(quote < 1L | quote > p)) 
            stop("invalid numbers in 'quote'")
    }
    else stop("invalid 'quote' specification")
    rn <- FALSE
    rnames <- NULL
    if (is.logical(row.names)) {
        if (row.names) {
            rnames <- as.character(d[[1L]])
            rn <- TRUE
        }
    }
    else {
        rnames <- as.character(row.names)
        rn <- TRUE
        if (length(rnames) != nrow(x)) 
            stop("invalid 'row.names' specification")
    }
    if (!is.null(quote) && rn) 
        quote <- c(0, quote)
    if (is.logical(col.names)) {
        if (!rn && is.na(col.names)) 
            stop("'col.names = NA' makes no sense when 'row.names = FALSE'")
        col.names <- if (is.na(col.names) && rn) 
            c("", d[[2L]])
        else if (col.names) 
            d[[2L]]
        else NULL
    }
    else {
        col.names <- as.character(col.names)
        if (length(col.names) != p) 
            stop("invalid 'col.names' specification")
    }
    if (file == "") 
        file <- stdout()
    else if (is.character(file)) {
        file <- if (nzchar(fileEncoding)) 
            file(file, ifelse(append, "a", "w"), encoding = fileEncoding)
        else file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
    }
    else if (!isOpen(file, "w")) {
        open(file, "w")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    qstring <- switch(qmethod, escape = "\\\\\"", double = "\"\"")
    if (!is.null(col.names)) {
        if (append) 
            warning("appending column names to file")
        if (quoteC) 
            col.names <- paste("\"", gsub("\"", qstring, col.names), 
                "\"", sep = "")
        writeLines(paste(col.names, collapse = sep), file, sep = eol)
    }
    if (nrow(x) == 0L) 
        return(invisible())
    if (nocols && !rn) 
        return(cat(rep.int(eol, NROW(x)), file = file, sep = ""))
    if (is.matrix(x) && !is.atomic(x)) 
        mode(x) <- "character"
    if (is.data.frame(x)) {
        x[] <- lapply(x, function(z) {
            if (is.object(z) && !is.factor(z)) 
                as.character(z)
            else z
        })
    }
    invisible(.External2(C_writetable, x, file, nrow(x), p, rnames, 
        sep, eol, na, dec, as.integer(quote), qmethod != "double"))
}


read.delim2 <- function (file, header = TRUE, sep = "\t", quote = "\"", dec = ",", 
    fill = TRUE, comment.char = "", ...) 
read.table(file = file, header = header, sep = sep, quote = quote, 
    dec = dec, fill = fill, comment.char = comment.char, ...)


aspell_package_vignettes <- function (dir, control = list(), program = NULL, dictionaries = character()) 
{
    dir <- tools::file_path_as_absolute(dir)
    vinfo <- tools::pkgVignettes(dir = dir)
    files <- vinfo$docs
    if (!length(files)) 
        return(aspell(character()))
    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if (is.na(encoding <- meta["Encoding"])) 
        encoding <- "unknown"
    defaults <- .aspell_package_defaults(dir, encoding)$vignettes
    if (!is.null(defaults)) {
        if (!is.null(d <- defaults$control)) 
            control <- d
        if (!is.null(d <- defaults$program)) 
            program <- d
        if (!is.null(d <- defaults$dictionaries)) {
            dictionaries <- aspell_find_dictionaries(d, file.path(dir, 
                ".aspell"))
        }
        if (!is.null(d <- defaults$personal)) 
            control <- c(control, sprintf("-p %s", shQuote(file.path(dir, 
                ".aspell", d))))
    }
    program <- aspell_find_program(program)
    fgroups <- split(files, vinfo$engines)
    egroups <- split(vinfo$encodings, vinfo$engines)
    do.call(rbind, Map(function(fgroup, egroup, engine) {
        engine <- tools::vignetteEngine(engine)
        aspell(fgroup, filter = engine$aspell$filter, control = c(engine$aspell$control, 
            aspell_control_package_vignettes[[names(program)]], 
            control), encoding = egroup, program = program, dictionaries = dictionaries)
    }, fgroups, egroups, names(fgroups)))
}


SweaveHooks <- function (options, run = FALSE, envir = .GlobalEnv) 
{
    if (is.null(SweaveHooks <- getOption("SweaveHooks"))) 
        return(NULL)
    z <- character()
    for (k in names(SweaveHooks)) if (nzchar(k) && is.logical(options[[k]]) && 
        options[[k]]) 
        if (is.function(SweaveHooks[[k]])) {
            z <- c(z, k)
            if (run) 
                eval(SweaveHooks[[k]](), envir = envir)
        }
    z
}


hasName <- function (x, name) 
match(name, names(x), nomatch = 0L) > 0L


assignInNamespace <- function (x, value, ns, pos = -1, envir = as.environment(pos)) 
{
    nf <- sys.nframe()
    if (missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if (is.null(nm) || substr(nm, 1L, 8L) != "package:") 
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    }
    else ns <- asNamespace(ns)
    ns_name <- getNamespaceName(ns)
    if (nf > 1L) {
        if (ns_name %in% tools:::.get_standard_package_names()$base) 
            stop("locked binding of ", sQuote(x), " cannot be changed", 
                domain = NA)
    }
    if (bindingIsLocked(x, ns)) {
        in_load <- Sys.getenv("_R_NS_LOAD_")
        if (nzchar(in_load)) {
            if (in_load != ns_name) {
                msg <- gettextf("changing locked binding for %s in %s whilst loading %s", 
                  sQuote(x), sQuote(ns_name), sQuote(in_load))
                if (!in_load %in% c("Matrix", "SparseM")) 
                  warning(msg, call. = FALSE, domain = NA, immediate. = TRUE)
            }
        }
        else if (nzchar(Sys.getenv("_R_WARN_ON_LOCKED_BINDINGS_"))) {
            warning(gettextf("changing locked binding for %s in %s", 
                sQuote(x), sQuote(ns_name)), call. = FALSE, domain = NA, 
                immediate. = TRUE)
        }
        unlockBinding(x, ns)
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(x, ns)
    }
    else {
        assign(x, value, envir = ns, inherits = FALSE)
    }
    if (!isBaseNamespace(ns)) {
        S3 <- .getNamespaceInfo(ns, "S3methods")
        if (!length(S3)) 
            return(invisible(NULL))
        S3names <- S3[, 3L]
        if (x %in% S3names) {
            i <- match(x, S3names)
            genfun <- get(S3[i, 1L], mode = "function", envir = parent.frame())
            if (.isMethodsDispatchOn() && methods::is(genfun, 
                "genericFunction")) 
                genfun <- methods::slot(genfun, "default")@methods$ANY
            defenv <- if (typeof(genfun) == "closure") 
                environment(genfun)
            else .BaseNamespaceEnv
            S3Table <- get(".__S3MethodsTable__.", envir = defenv)
            remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
            if (exists(remappedName, envir = S3Table, inherits = FALSE)) 
                assign(remappedName, value, S3Table)
        }
    }
    invisible(NULL)
}


is.relistable <- function (x) 
inherits(x, "relistable")


.DollarNames <- function (x, pattern) 
UseMethod(".DollarNames")


toBibtex <- function (object, ...) 
UseMethod("toBibtex")


alarm <- function () 
{
    cat("\a")
    flush.console()
}


RweaveLatexSetup <- function (file, syntax, output = NULL, quiet = FALSE, debug = FALSE, 
    stylepath, ...) 
{
    dots <- list(...)
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste0(prefix.string, ".tex")
    }
    else prefix.string <- basename(sub("\\.tex$", "", output))
    if (!quiet) 
        cat("Writing to file ", output, "\n", "Processing code chunks with options ...\n", 
            sep = "")
    encoding <- attr(file, "encoding")
    if (encoding %in% c("ASCII", "bytes")) 
        encoding <- ""
    output <- file(output, open = "w", encoding = encoding)
    if (missing(stylepath)) {
        p <- Sys.getenv("SWEAVE_STYLEPATH_DEFAULT")
        stylepath <- if (length(p) >= 1L && nzchar(p[1L])) 
            identical(p, "TRUE")
        else FALSE
    }
    if (stylepath) {
        styfile <- file.path(R.home("share"), "texmf", "tex", 
            "latex", "Sweave")
        if (.Platform$OS.type == "windows") 
            styfile <- chartr("\\", "/", styfile)
        if (length(grep(" ", styfile))) 
            warning(gettextf("path to %s contains spaces,\n", 
                sQuote(styfile)), gettext("this may cause problems when running LaTeX"), 
                domain = NA)
    }
    else styfile <- "Sweave"
    options <- list(prefix = TRUE, prefix.string = prefix.string, 
        engine = "R", print = FALSE, eval = TRUE, fig = FALSE, 
        pdf = TRUE, eps = FALSE, png = FALSE, jpeg = FALSE, grdevice = "", 
        width = 6, height = 6, resolution = 300, term = TRUE, 
        echo = TRUE, keep.source = TRUE, results = "verbatim", 
        split = FALSE, strip.white = "true", include = TRUE, 
        pdf.version = grDevices::pdf.options()$version, pdf.encoding = grDevices::pdf.options()$encoding, 
        pdf.compress = grDevices::pdf.options()$compress, expand = TRUE, 
        concordance = FALSE, figs.only = TRUE)
    options$.defaults <- options
    options[names(dots)] <- dots
    options <- RweaveLatexOptions(options)
    list(output = output, styfile = styfile, havesty = FALSE, 
        haveconcordance = FALSE, debug = debug, quiet = quiet, 
        syntax = syntax, options = options, chunkout = list(), 
        srclines = integer())
}


checkCRAN <- function (method) 
{
    master <- available.packages(contrib.url("http://CRAN.R-project.org"), 
        method = method)
    m <- getCRANmirrors()
    z <- list()
    for (url in as.character(m$URL)) z[[url]] <- available.packages(contrib.url(url), 
        method = method)
    lapply(z, function(a) all.equal(a, master))
}


setRepositories <- function (graphics = getOption("menu.graphics"), ind = NULL, 
    addURLs = character()) 
{
    if (is.null(ind) && !interactive()) 
        stop("cannot set repositories non-interactively")
    a <- tools:::.get_repositories()
    pkgType <- getOption("pkgType")
    if (pkgType == "both") 
        pkgType <- "source"
    if (pkgType == "binary") 
        pkgType <- .Platform$pkgType
    if (length(grep("^mac\\.binary", pkgType))) 
        pkgType <- "mac.binary"
    thisType <- a[[pkgType]]
    a <- a[thisType, 1L:3L]
    repos <- getOption("repos")
    if ("CRAN" %in% row.names(a) && !is.na(CRAN <- repos["CRAN"])) 
        a["CRAN", "URL"] <- CRAN
    a[(a[["URL"]] %in% repos), "default"] <- TRUE
    new <- !(repos %in% a[["URL"]])
    if (any(new)) {
        aa <- names(repos[new])
        if (is.null(aa)) 
            aa <- rep("", length(repos[new]))
        aa[aa == ""] <- repos[new][aa == ""]
        newa <- data.frame(menu_name = aa, URL = repos[new], 
            default = TRUE)
        row.names(newa) <- aa
        a <- rbind(a, newa)
    }
    default <- a[["default"]]
    res <- if (length(ind)) 
        as.integer(ind)
    else {
        title <- if (graphics) 
            "Repositories"
        else gettext("--- Please select repositories for use in this session ---\n")
        match(select.list(a[, 1L], a[default, 1L], multiple = TRUE, 
            title, graphics = graphics), a[, 1L])
    }
    if (length(res) || length(addURLs)) {
        repos <- a[["URL"]]
        names(repos) <- row.names(a)
        repos <- c(repos[res], addURLs)
        options(repos = repos)
    }
}


dump.frames <- function (dumpto = "last.dump", to.file = FALSE, include.GlobalEnv = FALSE) 
{
    calls <- sys.calls()
    last.dump <- sys.frames()
    names(last.dump) <- limitedLabels(calls)
    if (include.GlobalEnv) {
        last.dump <- c(.GlobalEnv = as.environment(as.list(.GlobalEnv, 
            all.names = TRUE)), last.dump)
    }
    last.dump <- last.dump[-length(last.dump)]
    attr(last.dump, "error.message") <- geterrmessage()
    class(last.dump) <- "dump.frames"
    if (dumpto != "last.dump") 
        assign(dumpto, last.dump)
    if (to.file) 
        save(list = dumpto, file = paste0(dumpto, ".rda"))
    else assign(dumpto, last.dump, envir = .GlobalEnv)
    invisible()
}


Rprof <- function (filename = "Rprof.out", append = FALSE, interval = 0.02, 
    memory.profiling = FALSE, gc.profiling = FALSE, line.profiling = FALSE, 
    numfiles = 100L, bufsize = 10000L) 
{
    if (is.null(filename)) 
        filename <- ""
    invisible(.External(C_Rprof, filename, append, interval, 
        memory.profiling, gc.profiling, line.profiling, numfiles, 
        bufsize))
}


sessionInfo <- function (package = NULL) 
{
    z <- list()
    z$R.version <- R.Version()
    z$platform <- z$R.version$platform
    if (nzchar(.Platform$r_arch)) 
        z$platform <- paste(z$platform, .Platform$r_arch, sep = "/")
    z$platform <- paste0(z$platform, " (", 8 * .Machine$sizeof.pointer, 
        "-bit)")
    z$locale <- Sys.getlocale()
    if (.Platform$OS.type == "windows") {
        z$running <- win.version()
    }
    else if (nzchar(Sys.which("uname"))) {
        uname <- system("uname -a", intern = TRUE)
        os <- sub(" .*", "", uname)
        z$running <- switch(os, Linux = if (file.exists("/etc/os-release")) {
            tmp <- readLines("/etc/os-release")
            t2 <- if (any(startsWith(tmp, "PRETTY_NAME="))) sub("^PRETTY_NAME=", 
                "", grep("^PRETTY_NAME=", tmp, value = TRUE)[1L]) else if (any(startsWith(tmp, 
                "NAME"))) sub("^NAME=", "", grep("^NAME=", tmp, 
                value = TRUE)[1L]) else "Linux (unknown distro)"
            sub("\"(.*)\"", "\\1", t2)
        } else if (file.exists("/etc/system-release")) {
            readLines("/etc/system-release")
        }, Darwin = {
            ver <- readLines("/System/Library/CoreServices/SystemVersion.plist")
            ind <- grep("ProductUserVisibleVersion", ver)
            ver <- ver[ind + 1L]
            ver <- sub(".*<string>", "", ver)
            ver <- sub("</string>$", "", ver)
            ver1 <- strsplit(ver, ".", fixed = TRUE)[[1L]][2L]
            sprintf("%s %s %s", ifelse(as.numeric(ver1) < 12, 
                "OS X", "macOS"), switch(ver1, `6` = "Snow Leopard", 
                `7` = "Lion", `8` = "Mountain Lion", `9` = "Mavericks", 
                `10` = "Yosemite", `11` = "El Capitan", `12` = "Sierra", 
                ""), ver)
        }, SunOS = {
            ver <- system("uname -r", intern = TRUE)
            paste("Solaris", strsplit(ver, ".", fixed = TRUE)[[1L]][2L])
        }, uname)
    }
    if (is.null(package)) {
        package <- grep("^package:", search(), value = TRUE)
        keep <- vapply(package, function(x) x == "package:base" || 
            !is.null(attr(as.environment(x), "path")), NA)
        package <- .rmpkg(package[keep])
    }
    pkgDesc <- lapply(package, packageDescription, encoding = NA)
    if (length(package) == 0) 
        stop("no valid packages were specified")
    basePkgs <- sapply(pkgDesc, function(x) !is.null(x$Priority) && 
        x$Priority == "base")
    z$basePkgs <- package[basePkgs]
    if (any(!basePkgs)) {
        z$otherPkgs <- pkgDesc[!basePkgs]
        names(z$otherPkgs) <- package[!basePkgs]
    }
    loadedOnly <- loadedNamespaces()
    loadedOnly <- loadedOnly[!(loadedOnly %in% package)]
    if (length(loadedOnly)) {
        names(loadedOnly) <- loadedOnly
        pkgDesc <- c(pkgDesc, lapply(loadedOnly, packageDescription))
        z$loadedOnly <- pkgDesc[loadedOnly]
    }
    z$matprod <- as.character(options("matprod"))
    es <- extSoftVersion()
    z$BLAS <- as.character(es["BLAS"])
    z$LAPACK <- La_library()
    class(z) <- "sessionInfo"
    z
}


count.fields <- function (file, sep = "", quote = "\"'", skip = 0, blank.lines.skip = TRUE, 
    comment.char = "#") 
{
    if (is.character(file)) {
        file <- file(file)
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    .External(C_countfields, file, sep, quote, skip, blank.lines.skip, 
        comment.char)
}


create.post <- function (instructions = character(), description = "post", subject = "", 
    method = getOption("mailer"), address = "the relevant mailing list", 
    ccaddress = getOption("ccaddress", ""), filename = "R.post", 
    info = character()) 
{
    method <- if (is.null(method)) 
        "none"
    else match.arg(method, c("mailto", "mailx", "gnudoit", "none", 
        "ess"))
    open_prog <- if (grepl("-apple-darwin", R.version$platform)) 
        "open"
    else "xdg-open"
    if (method == "mailto") 
        if (!nzchar(Sys.which(open_prog))) {
            browser <- Sys.getenv("R_BROWSER", "")
            if (!nzchar(browser)) {
                warning("cannot find program to open 'mailto:' URIs: reverting to 'method=\"none\"'")
                flush.console()
                Sys.sleep(5)
            }
            else {
                message("Using the browser to open a mailto: URI")
                open_prog <- browser
            }
        }
    body <- c(instructions, "--please do not edit the information below--", 
        "", info)
    none_method <- function() {
        disclaimer <- paste0("# Your mailer is set to \"none\",\n", 
            "# hence we cannot send the, ", description, " directly from R.\n", 
            "# Please copy the ", description, " (after finishing it) to\n", 
            "# your favorite email program and send it to\n#\n", 
            "#       ", address, "\n#\n", "######################################################\n", 
            "\n\n")
        cat(c(disclaimer, body), file = filename, sep = "\n")
        cat("The", description, "is being opened for you to edit.\n")
        flush.console()
        file.edit(filename)
        cat("The unsent ", description, " can be found in file ", 
            sQuote(filename), "\n", sep = "")
    }
    if (method == "none") {
        none_method()
    }
    else if (method == "mailx") {
        if (missing(address)) 
            stop("must specify 'address'")
        if (!nzchar(subject)) 
            stop("'subject' is missing")
        if (length(ccaddress) != 1L) 
            stop("'ccaddress' must be of length 1")
        cat(body, file = filename, sep = "\n")
        cat("The", description, "is being opened for you to edit.\n")
        file.edit(filename)
        if (is.character(ccaddress) && nzchar(ccaddress)) {
            cmdargs <- paste("-s", shQuote(subject), "-c", shQuote(ccaddress), 
                shQuote(address), "<", filename, "2>/dev/null")
        }
        else cmdargs <- paste("-s", shQuote(subject), shQuote(address), 
            "<", filename, "2>/dev/null")
        status <- 1L
        answer <- readline(paste0("Email the ", description, 
            " now? (yes/no) "))
        answer <- grep("yes", answer, ignore.case = TRUE)
        if (length(answer)) {
            cat("Sending email ...\n")
            status <- system(paste("mailx", cmdargs), , TRUE, 
                TRUE)
            if (status) 
                status <- system(paste("Mail", cmdargs), , TRUE, 
                  TRUE)
            if (status) 
                status <- system(paste("/usr/ucb/mail", cmdargs), 
                  , TRUE, TRUE)
            if (status == 0L) 
                unlink(filename)
            else {
                cat("Sending email failed!\n")
                cat("The unsent", description, "can be found in file", 
                  sQuote(filename), "\n")
            }
        }
        else cat("The unsent", description, "can be found in file", 
            filename, "\n")
    }
    else if (method == "ess") {
        cat(body, sep = "\n")
    }
    else if (method == "gnudoit") {
        cmd <- paste0("gnudoit -q '", "(mail nil \"", address, 
            "\")", "(insert \"", paste(body, collapse = "\\n"), 
            "\")", "(search-backward \"Subject:\")", "(end-of-line)'")
        system(cmd)
    }
    else if (method == "mailto") {
        if (missing(address)) 
            stop("must specify 'address'")
        if (!nzchar(subject)) 
            subject <- "<<Enter Meaningful Subject>>"
        if (length(ccaddress) != 1L) 
            stop("'ccaddress' must be of length 1")
        cat("The", description, "is being opened in your default mail program\nfor you to complete and send.\n")
        arg <- paste0("mailto:", address, "?subject=", subject, 
            if (is.character(ccaddress) && nzchar(ccaddress)) 
                paste0("&cc=", ccaddress), "&body=", paste(body, 
                collapse = "\r\n"))
        if (system2(open_prog, shQuote(URLencode(arg)), FALSE, 
            FALSE)) {
            cat("opening the mailer failed, so reverting to 'mailer=\"none\"'\n")
            flush.console()
            none_method()
        }
    }
    invisible()
}


tail.matrix <- function (x, n = 6L, addrownums = TRUE, ...) 
{
    stopifnot(length(n) == 1L)
    nrx <- nrow(x)
    n <- if (n < 0L) 
        max(nrx + n, 0L)
    else min(n, nrx)
    sel <- as.integer(seq.int(to = nrx, length.out = n))
    ans <- x[sel, , drop = FALSE]
    if (addrownums && is.null(rownames(x))) 
        rownames(ans) <- format(sprintf("[%d,]", sel), justify = "right")
    ans
}


person <- function (given = NULL, family = NULL, middle = NULL, email = NULL, 
    role = NULL, comment = NULL, first = NULL, last = NULL) 
{
    args <- list(given = given, family = family, middle = middle, 
        email = email, role = role, comment = comment, first = first, 
        last = last)
    if (all(sapply(args, is.null))) {
        return(structure(list(), class = "person"))
    }
    args <- lapply(args, .listify)
    args_length <- lengths(args)
    if (!all(args_length_ok <- args_length %in% c(1L, max(args_length)))) 
        warning(gettextf("Not all arguments are of the same length, the following need to be recycled: %s", 
            paste(names(args)[!args_length_ok], collapse = ", ")), 
            domain = NA)
    args <- lapply(args, function(x) rep_len(x, max(args_length)))
    person1 <- function(given = NULL, family = NULL, middle = NULL, 
        email = NULL, role = NULL, comment = NULL, first = NULL, 
        last = NULL) {
        if (!.is_not_nonempty_text(first)) {
            if (!.is_not_nonempty_text(given)) 
                stop(gettextf("Use either %s or %s/%s but not both.", 
                  sQuote("given"), sQuote("first"), sQuote("middle")), 
                  domain = NA)
            message(gettextf("It is recommended to use %s instead of %s.", 
                sQuote("given"), sQuote("first")), domain = NA)
            given <- first
        }
        if (!.is_not_nonempty_text(middle)) {
            message(gettextf("It is recommended to use %s instead of %s.", 
                sQuote("given"), sQuote("middle")), domain = NA)
            given <- c(given, unlist(strsplit(middle, "[[:space:]]+")))
        }
        if (!.is_not_nonempty_text(last)) {
            if (!.is_not_nonempty_text(family)) 
                stop(gettextf("Use either %s or %s but not both.", 
                  sQuote("family"), sQuote("last")), domain = NA)
            message(gettextf("It is recommended to use %s instead of %s.", 
                sQuote("family"), sQuote("last")), domain = NA)
            family <- last
        }
        if (.is_not_nonempty_text(given)) 
            given <- NULL
        if (.is_not_nonempty_text(family)) 
            family <- NULL
        if (.is_not_nonempty_text(email)) 
            email <- NULL
        if (.is_not_nonempty_text(role)) {
            if (!is.null(role)) 
                warning(sprintf(ngettext(length(role), "Invalid role specification: %s.", 
                  "Invalid role specifications: %s."), paste(sQuote(role), 
                  collapse = ", ")), domain = NA)
            role <- NULL
        }
        if (.is_not_nonempty_text(comment)) 
            comment <- NULL
        if (length(role)) 
            role <- .canonicalize_person_role(role)
        rval <- list(given = given, family = family, role = role, 
            email = email, comment = comment)
        if (any(ind <- (lengths(rval) == 0L))) 
            rval[ind] <- vector("list", length = sum(ind))
        if (all(vapply(rval, is.null, NA))) 
            NULL
        else rval
    }
    rval <- lapply(seq_along(args$given), function(i) with(args, 
        person1(given = given[[i]], family = family[[i]], middle = middle[[i]], 
            email = email[[i]], role = role[[i]], comment = comment[[i]], 
            first = first[[i]], last = last[[i]])))
    structure(rval[!vapply(rval, is.null, NA)], class = "person")
}


promptPackage <- function (package, lib.loc = NULL, filename = NULL, name = NULL, 
    final = FALSE) 
{
    insert1 <- function(field, new) {
        prev <- Rdtxt[[field]]
        Rdtxt[[field]] <<- c(prev[-length(prev)], new, prev[length(prev)])
    }
    insert2 <- function(field, new) insert1(field, paste("~~", 
        new, "~~"))
    if (missing(name)) 
        name <- paste0(package, "-package")
    if (is.null(filename)) 
        filename <- paste0(name, ".Rd")
    Rdtxt <- list(name = paste0("\\name{", name, "}"), aliases = c(paste0("\\alias{", 
        name, "}"), c(paste0("\\alias{", package, "}"))), docType = "\\docType{package}", 
        title = c("\\title{", "}"), description = c("\\description{", 
            "}"), details = c("\\details{", "}"), author = c("\\author{", 
            "}"), references = character(0L), keywords = c("\\keyword{ package }"))
    insert1("title", paste0("\\packageTitle{", package, "}"))
    insert1("description", paste0("\\packageDescription{", package, 
        "}"))
    insert1("author", c(paste0("\\packageAuthor{", package, "}"), 
        "", paste("Maintainer:", paste0("\\packageMaintainer{", 
            package, "}"))))
    insert1("details", c("", "The DESCRIPTION file:"))
    insert1("details", paste0("\\packageDESCRIPTION{", package, 
        "}"))
    insert1("details", paste0("\\packageIndices{", package, "}"))
    if (!final) {
        insert2("details", strwrap("An overview of how to use the package, including the most important functions"))
        Rdtxt$references <- c("\\references{", paste("~~", "Literature or other references for background information", 
            "~~"), "}")
        Rdtxt$seealso <- c("\\seealso{", "}")
        insert2("seealso", c("Optional links to other man pages, e.g.", 
            "\\code{\\link[<pkg>:<pkg>-package]{<pkg>}}"))
        Rdtxt$examples <- c("\\examples{", "}")
        insert2("examples", "simple examples of the most important functions")
        insert2("keywords", strwrap("Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory"))
    }
    if (is.na(filename)) 
        return(Rdtxt)
    cat(unlist(Rdtxt), file = filename, sep = "\n")
    message(gettextf("Created file named %s.", sQuote(filename)), 
        "\n", gettext("Edit the file and move it to the appropriate directory."), 
        domain = NA)
    invisible(filename)
}


install.packages <- function (pkgs, lib, repos = getOption("repos"), contriburl = contrib.url(repos, 
    type), method, available = NULL, destdir = NULL, dependencies = NA, 
    type = getOption("pkgType"), configure.args = getOption("configure.args"), 
    configure.vars = getOption("configure.vars"), clean = FALSE, 
    Ncpus = getOption("Ncpus", 1L), verbose = getOption("verbose"), 
    libs_only = FALSE, INSTALL_opts, quiet = FALSE, keep_outputs = FALSE, 
    ...) 
{
    type2 <- .Platform$pkgType
    if (type == "binary") {
        if (type2 == "source") 
            stop("type 'binary' is not supported on this platform")
        else type <- type2
        if (type == "both" && (!missing(contriburl) || !is.null(available))) 
            stop("specifying 'contriburl' or 'available' requires a single type, not type = \"both\"")
    }
    if (is.logical(clean) && clean) 
        clean <- "--clean"
    if (is.logical(dependencies) && is.na(dependencies)) 
        dependencies <- if (!missing(lib) && length(lib) > 1L) 
            FALSE
        else c("Depends", "Imports", "LinkingTo")
    get_package_name <- function(pkg) {
        gsub("_[.](zip|tar[.]gz|tar[.]bzip2|tar[.]xz)", "", gsub(.standard_regexps()$valid_package_version, 
            "", basename(pkg)))
    }
    getConfigureArgs <- function(pkg) {
        if (.Platform$OS.type == "windows") 
            return(character())
        if (length(pkgs) == 1L && length(configure.args) && length(names(configure.args)) == 
            0L) 
            return(paste0("--configure-args=", shQuote(paste(configure.args, 
                collapse = " "))))
        pkg <- get_package_name(pkg)
        if (length(configure.args) && length(names(configure.args)) && 
            pkg %in% names(configure.args)) 
            config <- paste0("--configure-args=", shQuote(paste(configure.args[[pkg]], 
                collapse = " ")))
        else config <- character()
        config
    }
    getConfigureVars <- function(pkg) {
        if (.Platform$OS.type == "windows") 
            return(character())
        if (length(pkgs) == 1L && length(configure.vars) && length(names(configure.vars)) == 
            0L) 
            return(paste0("--configure-vars=", shQuote(paste(configure.vars, 
                collapse = " "))))
        pkg <- get_package_name(pkg)
        if (length(configure.vars) && length(names(configure.vars)) && 
            pkg %in% names(configure.vars)) 
            config <- paste0("--configure-vars=", shQuote(paste(configure.vars[[pkg]], 
                collapse = " ")))
        else config <- character()
        config
    }
    get_install_opts <- function(pkg) {
        if (!length(INSTALL_opts)) 
            character()
        else paste(INSTALL_opts[[get_package_name(pkg)]], collapse = " ")
    }
    if (missing(pkgs) || !length(pkgs)) {
        if (!interactive()) 
            stop("no packages were specified")
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA" || (capabilities("tcltk") && capabilities("X11") && 
            suppressWarnings(tcltk::.TkUp))) {
        }
        else stop("no packages were specified")
        if (is.null(available)) {
            av <- available.packages(contriburl = contriburl, 
                method = method)
            if (missing(repos)) 
                repos <- getOption("repos")
            if (type != "both") 
                available <- av
        }
        else av <- available
        if (NROW(av)) {
            pkgs <- select.list(sort(unique(rownames(av))), multiple = TRUE, 
                title = "Packages", graphics = TRUE)
        }
        if (!length(pkgs)) 
            stop("no packages were specified")
    }
    if (missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1L]
        if (!quiet && length(.libPaths()) > 1L) 
            message(sprintf(ngettext(length(pkgs), "Installing package into %s\n(as %s is unspecified)", 
                "Installing packages into %s\n(as %s is unspecified)"), 
                sQuote(lib), sQuote("lib")), domain = NA)
    }
    ok <- dir.exists(lib) & (file.access(lib, 2) == 0L)
    if (length(lib) > 1 && any(!ok)) 
        stop(sprintf(ngettext(sum(!ok), "'lib' element %s is not a writable directory", 
            "'lib' elements %s are not writable directories"), 
            paste(sQuote(lib[!ok]), collapse = ", ")), domain = NA)
    if (length(lib) == 1L && .Platform$OS.type == "windows") {
        ok <- dir.exists(lib)
        if (ok) {
            fn <- file.path(lib, paste0("_test_dir_", Sys.getpid()))
            unlink(fn, recursive = TRUE)
            res <- try(dir.create(fn, showWarnings = FALSE))
            if (inherits(res, "try-error") || !res) 
                ok <- FALSE
            else unlink(fn, recursive = TRUE)
        }
    }
    if (length(lib) == 1L && !ok) {
        warning(gettextf("'lib = \"%s\"' is not writable", lib), 
            domain = NA, immediate. = TRUE)
        userdir <- unlist(strsplit(Sys.getenv("R_LIBS_USER"), 
            .Platform$path.sep))[1L]
        if (interactive()) {
            ask.yes.no <- function(msg) {
                msg <- gettext(msg)
                if (.Platform$OS.type == "windows") {
                  flush.console()
                  ans <- winDialog("yesno", sprintf(msg, sQuote(userdir)))
                  if (ans != "YES") 
                    "no"
                  else ans
                }
                else {
                  ans <- readline(paste(sprintf(msg, userdir), 
                    " (y/n) "))
                  if (substr(ans, 1L, 1L) == "n") 
                    "no"
                  else ans
                }
            }
            ans <- ask.yes.no("Would you like to use a personal library instead?")
            if (identical(ans, "no")) 
                stop("unable to install packages")
            lib <- userdir
            if (!file.exists(userdir)) {
                ans <- ask.yes.no("Would you like to create a personal library\n%s\nto install packages into?")
                if (identical(ans, "no")) 
                  stop("unable to install packages")
                if (!dir.create(userdir, recursive = TRUE)) 
                  stop(gettextf("unable to create %s", sQuote(userdir)), 
                    domain = NA)
                .libPaths(c(userdir, .libPaths()))
            }
        }
        else stop("unable to install packages")
    }
    lib <- normalizePath(lib)
    if (length(pkgs) == 1L && missing(repos) && missing(contriburl)) {
        if ((type == "source" && any(grepl("[.]tar[.](gz|bz2|xz)$", 
            pkgs))) || (type %in% "win.binary" && length(grep("[.]zip$", 
            pkgs))) || (substr(type, 1L, 10L) == "mac.binary" && 
            grepl("[.]tgz$", pkgs))) {
            repos <- NULL
            message("inferring 'repos = NULL' from 'pkgs'")
        }
        if (type == "both") {
            if (type2 %in% "win.binary" && grepl("[.]zip$", pkgs)) {
                repos <- NULL
                type <- type2
                message("inferring 'repos = NULL' from 'pkgs'")
            }
            else if (substr(type2, 1L, 10L) == "mac.binary" && 
                grepl("[.]tgz$", pkgs)) {
                repos <- NULL
                type <- type2
                message("inferring 'repos = NULL' from 'pkgs'")
            }
            else if (grepl("[.]tar[.](gz|bz2|xz)$", pkgs)) {
                repos <- NULL
                type <- "source"
                message("inferring 'repos = NULL' from 'pkgs'")
            }
        }
    }
    if (length(pkgs) == 1L && is.null(repos) && type == "both") {
        if ((type2 %in% "win.binary" && grepl("[.]zip$", pkgs)) || 
            (substr(type2, 1L, 10L) == "mac.binary" && grepl("[.]tgz$", 
                pkgs))) {
            type <- type2
        }
        else if (grepl("[.]tar[.](gz|bz2|xz)$", pkgs)) {
            type <- "source"
        }
    }
    if (is.null(repos) && missing(contriburl)) {
        tmpd <- destdir
        nonlocalrepos <- any(web <- grepl("^(http|https|ftp)://", 
            pkgs))
        if (is.null(destdir) && nonlocalrepos) {
            tmpd <- file.path(tempdir(), "downloaded_packages")
            if (!file.exists(tmpd) && !dir.create(tmpd)) 
                stop(gettextf("unable to create temporary directory %s", 
                  sQuote(tmpd)), domain = NA)
        }
        if (nonlocalrepos) {
            urls <- pkgs[web]
            for (p in unique(urls)) {
                this <- pkgs == p
                destfile <- file.path(tmpd, basename(p))
                res <- try(download.file(p, destfile, method, 
                  mode = "wb", ...))
                if (!inherits(res, "try-error") && res == 0L) 
                  pkgs[this] <- destfile
                else {
                  pkgs[this] <- NA
                }
            }
        }
    }
    if (type == "both") {
        if (type2 == "source") 
            stop("type == \"both\" can only be used on Windows or a CRAN build for macOS")
        if (!missing(contriburl) || !is.null(available)) 
            type <- type2
    }
    getDeps <- TRUE
    if (type == "both") {
        if (is.null(repos)) 
            stop("type == \"both\" cannot be used with 'repos = NULL'")
        type <- "source"
        contriburl <- contrib.url(repos, "source")
        if (missing(repos)) 
            repos <- getOption("repos")
        available <- available.packages(contriburl = contriburl, 
            method = method, fields = "NeedsCompilation")
        pkgs <- getDependencies(pkgs, dependencies, available, 
            lib)
        getDeps <- FALSE
        av2 <- available.packages(contriburl = contrib.url(repos, 
            type2), method = method)
        bins <- row.names(av2)
        bins <- pkgs[pkgs %in% bins]
        srcOnly <- pkgs[!pkgs %in% bins]
        binvers <- av2[bins, "Version"]
        hasSrc <- !is.na(av2[bins, "Archs"])
        srcvers <- available[bins, "Version"]
        later <- as.numeric_version(binvers) < srcvers
        action <- getOption("install.packages.compile.from.source", 
            "interactive")
        if (!nzchar(Sys.which(Sys.getenv("MAKE", "make")))) 
            action <- "never"
        if (any(later)) {
            msg <- ngettext(sum(later), "There is a binary version available but the source version is later", 
                "There are binary versions available but the source versions are later")
            cat("\n", paste(strwrap(msg, indent = 2, exdent = 2), 
                collapse = "\n"), ":\n", sep = "")
            out <- data.frame(binary = binvers, source = srcvers, 
                needs_compilation = hasSrc, row.names = bins, 
                check.names = FALSE)[later, ]
            print(out)
            cat("\n")
            if (any(later & hasSrc)) {
                if (action == "interactive" && interactive()) {
                  msg <- ngettext(sum(later & hasSrc), "Do you want to install from sources the package which needs compilation?", 
                    "Do you want to install from sources the packages which need compilation?")
                  message(msg, domain = NA)
                  res <- readline("y/n: ")
                  if (res != "y") 
                    later <- later & !hasSrc
                }
                else if (action == "never") {
                  cat("  Binaries will be installed\n")
                  later <- later & !hasSrc
                }
            }
        }
        bins <- bins[!later]
        if (length(srcOnly)) {
            s2 <- srcOnly[!(available[srcOnly, "NeedsCompilation"] %in% 
                "no")]
            if (length(s2)) {
                msg <- ngettext(length(s2), "Package which is only available in source form, and may need compilation of C/C++/Fortran", 
                  "Packages which are only available in source form, and may need compilation of C/C++/Fortran")
                msg <- c(paste0(msg, ": "), sQuote(s2))
                msg <- strwrap(paste(msg, collapse = " "), exdent = 2)
                message(paste(msg, collapse = "\n"), domain = NA)
                if (action == "interactive" && interactive()) {
                  message("Do you want to attempt to install these from sources?")
                  res <- readline("y/n: ")
                  if (res != "y") 
                    pkgs <- setdiff(pkgs, s2)
                }
                else if (action == "never") {
                  cat("  These will not be installed\n")
                  pkgs <- setdiff(pkgs, s2)
                }
            }
        }
        if (length(bins)) {
            if (type2 == "win.binary") 
                .install.winbinary(pkgs = bins, lib = lib, contriburl = contrib.url(repos, 
                  type2), method = method, available = av2, destdir = destdir, 
                  dependencies = NULL, libs_only = libs_only, 
                  quiet = quiet, ...)
            else .install.macbinary(pkgs = bins, lib = lib, contriburl = contrib.url(repos, 
                type2), method = method, available = av2, destdir = destdir, 
                dependencies = NULL, quiet = quiet, ...)
        }
        pkgs <- setdiff(pkgs, bins)
        if (!length(pkgs)) 
            return(invisible())
        message(sprintf(ngettext(length(pkgs), "installing the source package %s", 
            "installing the source packages %s"), paste(sQuote(pkgs), 
            collapse = ", ")), "\n", domain = NA)
        flush.console()
    }
    else if (getOption("install.packages.check.source", "yes") %in% 
        "yes" && (type %in% "win.binary" || substr(type, 1L, 
        10L) == "mac.binary")) {
        if (missing(contriburl) && is.null(available) && !is.null(repos)) {
            contriburl2 <- contrib.url(repos, "source")
            if (missing(repos)) 
                repos <- getOption("repos")
            av1 <- tryCatch(suppressWarnings(available.packages(contriburl = contriburl2, 
                method = method)), error = function(e) e)
            if (inherits(av1, "error")) {
                message("source repository is unavailable to check versions")
                available <- available.packages(contriburl = contrib.url(repos, 
                  type), method = method)
            }
            else {
                srcpkgs <- pkgs[pkgs %in% row.names(av1)]
                available <- available.packages(contriburl = contrib.url(repos, 
                  type), method = method)
                bins <- pkgs[pkgs %in% row.names(available)]
                na <- srcpkgs[!srcpkgs %in% bins]
                if (length(na)) {
                  msg <- sprintf(ngettext(length(na), "package %s is available as a source package but not as a binary", 
                    "packages %s are available as source packages but not as binaries"), 
                    paste(sQuote(na), collapse = ", "))
                  cat("\n   ", msg, "\n\n", sep = "")
                }
                binvers <- available[bins, "Version"]
                srcvers <- binvers
                OK <- bins %in% srcpkgs
                srcvers[OK] <- av1[bins[OK], "Version"]
                later <- as.numeric_version(binvers) < srcvers
                if (any(later)) {
                  msg <- ngettext(sum(later), "There is a binary version available (and will be installed) but the source version is later", 
                    "There are binary versions available (and will be installed) but the source versions are later")
                  cat("\n", paste(strwrap(msg, indent = 2, exdent = 2), 
                    collapse = "\n"), ":\n", sep = "")
                  print(data.frame(binary = binvers, source = srcvers, 
                    row.names = bins, check.names = FALSE)[later, 
                    ])
                  cat("\n")
                }
            }
        }
    }
    if (.Platform$OS.type == "windows") {
        if (substr(type, 1L, 10L) == "mac.binary") 
            stop("cannot install macOS binary packages on Windows")
        if (type %in% "win.binary") {
            .install.winbinary(pkgs = pkgs, lib = lib, contriburl = contriburl, 
                method = method, available = available, destdir = destdir, 
                dependencies = dependencies, libs_only = libs_only, 
                quiet = quiet, ...)
            return(invisible())
        }
        have_spaces <- grep(" ", pkgs)
        if (length(have_spaces)) {
            p <- pkgs[have_spaces]
            dirs <- shortPathName(dirname(p))
            pkgs[have_spaces] <- file.path(dirs, basename(p))
        }
        pkgs <- gsub("\\\\", "/", pkgs)
    }
    else {
        if (substr(type, 1L, 10L) == "mac.binary") {
            if (!grepl("darwin", R.version$platform)) 
                stop("cannot install macOS binary packages on this platform")
            .install.macbinary(pkgs = pkgs, lib = lib, contriburl = contriburl, 
                method = method, available = available, destdir = destdir, 
                dependencies = dependencies, quiet = quiet, ...)
            return(invisible())
        }
        if (type %in% "win.binary") 
            stop("cannot install Windows binary packages on this platform")
        if (!file.exists(file.path(R.home("bin"), "INSTALL"))) 
            stop("This version of R is not set up to install source packages\nIf it was installed from an RPM, you may need the R-devel RPM")
    }
    libpath <- .libPaths()
    libpath <- libpath[!libpath %in% .Library]
    if (length(libpath)) 
        libpath <- paste(libpath, collapse = .Platform$path.sep)
    cmd0 <- file.path(R.home("bin"), "R")
    args0 <- c("CMD", "INSTALL")
    output <- if (quiet) 
        FALSE
    else ""
    env <- character()
    outdir <- getwd()
    if (is.logical(keep_outputs)) {
        if (is.na(keep_outputs)) 
            keep_outputs <- FALSE
    }
    else if (is.character(keep_outputs) && (length(keep_outputs) == 
        1L)) {
        if (!dir.exists(keep_outputs) && !dir.create(keep_outputs, 
            recursive = TRUE)) 
            stop(gettextf("unable to create %s", sQuote(keep_outputs)), 
                domain = NA)
        outdir <- normalizePath(keep_outputs)
        keep_outputs <- TRUE
    }
    else stop(gettextf("invalid %s argument", sQuote("keep_outputs")), 
        domain = NA)
    if (length(libpath)) {
        if (.Platform$OS.type == "windows") {
            oldrlibs <- Sys.getenv("R_LIBS")
            Sys.setenv(R_LIBS = libpath)
            on.exit(Sys.setenv(R_LIBS = oldrlibs))
        }
        else env <- paste0("R_LIBS=", shQuote(libpath))
    }
    if (is.character(clean)) 
        args0 <- c(args0, clean)
    if (libs_only) 
        args0 <- c(args0, "--libs-only")
    if (!missing(INSTALL_opts)) {
        if (!is.list(INSTALL_opts)) {
            args0 <- c(args0, paste(INSTALL_opts, collapse = " "))
            INSTALL_opts <- list()
        }
    }
    else {
        INSTALL_opts <- list()
    }
    if (verbose) 
        message(gettextf("system (cmd0): %s", paste(c(cmd0, args0), 
            collapse = " ")), domain = NA)
    if (is.null(repos) & missing(contriburl)) {
        update <- cbind(path.expand(pkgs), lib)
        for (i in seq_len(nrow(update))) {
            if (is.na(update[i, 1L])) 
                next
            args <- c(args0, get_install_opts(update[i, 1L]), 
                "-l", shQuote(update[i, 2L]), getConfigureArgs(update[i, 
                  1L]), getConfigureVars(update[i, 1L]), shQuote(update[i, 
                  1L]))
            status <- system2(cmd0, args, env = env, stdout = output, 
                stderr = output)
            if (status > 0L) 
                warning(gettextf("installation of package %s had non-zero exit status", 
                  sQuote(update[i, 1L])), domain = NA)
            else if (verbose) {
                cmd <- paste(c(cmd0, args), collapse = " ")
                message(sprintf("%d): succeeded '%s'", i, cmd), 
                  domain = NA)
            }
        }
        return(invisible())
    }
    tmpd <- destdir
    nonlocalrepos <- length(grep("^file:", contriburl)) < length(contriburl)
    if (is.null(destdir) && nonlocalrepos) {
        tmpd <- file.path(tempdir(), "downloaded_packages")
        if (!file.exists(tmpd) && !dir.create(tmpd)) 
            stop(gettextf("unable to create temporary directory %s", 
                sQuote(tmpd)), domain = NA)
    }
    if (is.null(available)) 
        available <- available.packages(contriburl = contriburl, 
            method = method)
    if (getDeps) 
        pkgs <- getDependencies(pkgs, dependencies, available, 
            lib)
    foundpkgs <- download.packages(pkgs, destdir = tmpd, available = available, 
        contriburl = contriburl, method = method, type = "source", 
        quiet = quiet, ...)
    if (length(foundpkgs)) {
        if (verbose) 
            message(gettextf("foundpkgs: %s", paste(foundpkgs, 
                collapse = ", ")), domain = NA)
        update <- unique(cbind(pkgs, lib))
        colnames(update) <- c("Package", "LibPath")
        found <- pkgs %in% foundpkgs[, 1L]
        files <- foundpkgs[match(pkgs[found], foundpkgs[, 1L]), 
            2L]
        if (verbose) 
            message(gettextf("files: %s", paste(files, collapse = ", \n\t")), 
                domain = NA)
        update <- cbind(update[found, , drop = FALSE], file = files)
        if (nrow(update) > 1L) {
            upkgs <- unique(pkgs <- update[, 1L])
            DL <- .make_dependency_list(upkgs, available)
            p0 <- .find_install_order(upkgs, DL)
            update <- update[sort.list(match(pkgs, p0)), ]
        }
        if (Ncpus > 1L && nrow(update) > 1L) {
            args0 <- c(args0, "--pkglock")
            tmpd <- file.path(tempdir(), "make_packages")
            if (!file.exists(tmpd) && !dir.create(tmpd)) 
                stop(gettextf("unable to create temporary directory %s", 
                  sQuote(tmpd)), domain = NA)
            mfile <- file.path(tmpd, "Makefile")
            conn <- file(mfile, "wt")
            deps <- paste(paste0(update[, 1L], ".ts"), collapse = " ")
            deps <- strwrap(deps, width = 75, exdent = 2)
            deps <- paste(deps, collapse = " \\\n")
            cat("all: ", deps, "\n", sep = "", file = conn)
            aDL <- .make_dependency_list(upkgs, available, recursive = TRUE)
            for (i in seq_len(nrow(update))) {
                pkg <- update[i, 1L]
                args <- c(args0, get_install_opts(update[i, 3L]), 
                  "-l", shQuote(update[i, 2L]), getConfigureArgs(update[i, 
                    3L]), getConfigureVars(update[i, 3L]), shQuote(update[i, 
                    3L]), ">", paste0(pkg, ".out"), "2>&1")
                cmd <- paste(c("MAKEFLAGS=", shQuote(cmd0), args), 
                  collapse = " ")
                deps <- aDL[[pkg]]
                deps <- deps[deps %in% upkgs]
                deps <- if (length(deps)) 
                  paste(paste0(deps, ".ts"), collapse = " ")
                else ""
                cat(paste0(pkg, ".ts: ", deps), paste("\t@echo begin installing package", 
                  sQuote(pkg)), paste0("\t@", cmd, " && touch ", 
                  pkg, ".ts"), paste0("\t@cat ", pkg, ".out"), 
                  "", sep = "\n", file = conn)
            }
            close(conn)
            cwd <- setwd(tmpd)
            on.exit(setwd(cwd))
            status <- system2(Sys.getenv("MAKE", "make"), c("-k -j", 
                Ncpus), stdout = output, stderr = output, env = env)
            if (status > 0L) {
                pkgs <- update[, 1L]
                tss <- sub("[.]ts$", "", dir(".", pattern = "[.]ts$"))
                failed <- pkgs[!pkgs %in% tss]
                for (pkg in failed) system(paste0("cat ", pkg, 
                  ".out"))
                warning(gettextf("installation of one or more packages failed,\n  probably %s", 
                  paste(sQuote(failed), collapse = ", ")), domain = NA)
            }
            if (keep_outputs) 
                file.copy(paste0(update[, 1L], ".out"), outdir)
            setwd(cwd)
            on.exit()
            unlink(tmpd, recursive = TRUE)
        }
        else {
            outfiles <- paste0(update[, 1L], ".out")
            for (i in seq_len(nrow(update))) {
                outfile <- if (keep_outputs) 
                  outfiles[i]
                else output
                args <- c(args0, get_install_opts(update[i, 3L]), 
                  "-l", shQuote(update[i, 2L]), getConfigureArgs(update[i, 
                    3L]), getConfigureVars(update[i, 3L]), update[i, 
                    3L])
                status <- system2(cmd0, args, env = env, stdout = outfile, 
                  stderr = outfile)
                if (!quiet && keep_outputs) 
                  writeLines(readLines(outfile))
                if (status > 0L) 
                  warning(gettextf("installation of package %s had non-zero exit status", 
                    sQuote(update[i, 1L])), domain = NA)
                else if (verbose) {
                  cmd <- paste(c(cmd0, args), collapse = " ")
                  message(sprintf("%d): succeeded '%s'", i, cmd), 
                    domain = NA)
                }
            }
            if (keep_outputs && (outdir != getwd())) {
                file.copy(outfiles, outdir)
                file.remove(outfiles)
            }
        }
        if (!quiet && nonlocalrepos && !is.null(tmpd) && is.null(destdir)) 
            cat("\n", gettextf("The downloaded source packages are in\n\t%s", 
                sQuote(normalizePath(tmpd, mustWork = FALSE))), 
                "\n", sep = "", file = stderr())
        libs_used <- unique(update[, 2L])
        if (.Platform$OS.type == "unix" && .Library %in% libs_used) {
            message("Updating HTML index of packages in '.Library'")
            make.packages.html(.Library)
        }
    }
    else if (!is.null(tmpd) && is.null(destdir)) 
        unlink(tmpd, TRUE)
    invisible()
}


news <- function (query, package = "R", lib.loc = NULL, format = NULL, 
    reader = NULL, db = NULL) 
{
    if (new.db <- is.null(db)) {
        db <- if (package == "R") 
            tools:::.build_news_db_from_R_NEWS_Rd()
        else tools:::.build_news_db(package, lib.loc, format, 
            reader)
    }
    if (is.null(db)) 
        return(NULL)
    if (new.db) 
        attr(db, "package") <- package
    if (missing(query)) 
        return(db)
    has_bad_attr <- !is.null(bad <- attr(db, "bad")) && (length(bad) == 
        NROW(db))
    db1 <- db
    version <- db$Version
    pos <- regexpr(sprintf("^%s", .standard_regexps()$valid_numeric_version), 
        version)
    if (any(ind <- (pos > -1L))) 
        version[ind] <- substring(version[ind], 1L, attr(pos, 
            "match.length")[ind])
    db1$Version <- numeric_version(version, strict = FALSE)
    db1$Date <- as.Date(db$Date)
    r <- eval(substitute(query), db1, parent.frame())
    if (!is.null(r)) {
        if (!is.logical(r) || length(r) != length(version)) 
            stop("invalid query")
        r <- r & !is.na(r)
        db <- db[r, ]
        if (!all(r)) 
            db <- structure(db, subset = r)
        if (has_bad_attr) 
            db <- structure(db, bad = bad[r])
    }
    db
}


URLencode <- function (URL, reserved = FALSE, repeated = FALSE) 
{
    if (!repeated && grepl("%[[:xdigit:]]{2}", URL, useBytes = TRUE)) 
        return(URL)
    OK <- paste0("[^", if (!reserved) 
        "][!$&'()*+,;=:/?@#", "ABCDEFGHIJKLMNOPQRSTUVWXYZ", "abcdefghijklmnopqrstuvwxyz0123456789._~-", 
        "]")
    x <- strsplit(URL, "")[[1L]]
    z <- grep(OK, x)
    if (length(z)) {
        y <- sapply(x[z], function(x) paste0("%", toupper(as.character(charToRaw(x))), 
            collapse = ""))
        x[z] <- y
    }
    paste(x, collapse = "")
}


type.convert <- function (x, na.strings = "NA", as.is = FALSE, dec = ".", numerals = c("allow.loss", 
    "warn.loss", "no.loss")) 
.External2(C_typeconvert, x, na.strings, as.is, dec, match.arg(numerals))


help.start <- function (update = FALSE, gui = "irrelevant", browser = getOption("browser"), 
    remote = NULL) 
{
    WINDOWS <- .Platform$OS.type == "windows"
    if (!WINDOWS) {
        if (!is.function(browser) && (length(browser) != 1L || 
            !is.character(browser) || !nzchar(browser))) 
            stop("invalid browser name, check options(\"browser\").")
    }
    home <- if (is.null(remote)) {
        port <- tools::startDynamicHelp(NA)
        if (port > 0L) {
            if (update) 
                make.packages.html(temp = TRUE)
            paste0("http://127.0.0.1:", port)
        }
        else stop("help.start() requires the HTTP server to be running", 
            call. = FALSE)
    }
    else remote
    url <- paste0(home, "/doc/html/index.html")
    if (WINDOWS) {
        cat(gettextf("If nothing happens, you should open\n%s yourself\n", 
            sQuote(url)))
    }
    else if (is.character(browser)) {
        writeLines(strwrap(gettextf("If the browser launched by '%s' is already running, it is *not* restarted, and you must switch to its window.", 
            browser), exdent = 4L))
        writeLines(gettext("Otherwise, be patient ..."))
    }
    browseURL(url, browser = browser)
    invisible()
}


de.setup <- function (ilist, list.names, incols) 
{
    ilen <- sum(incols)
    ivec <- vector("list", ilen)
    inames <- vector("list", ilen)
    i <- 1L
    k <- 0L
    for (telt in ilist) {
        k <- k + 1L
        if (is.list(telt)) {
            y <- names(telt)
            for (j in seq_along(telt)) {
                ivec[[i]] <- telt[[j]]
                if (is.null(y) || y[j] == "") 
                  inames[[i]] <- paste0("var", i)
                else inames[[i]] <- y[j]
                i <- i + 1L
            }
        }
        else if (is.vector(telt)) {
            ivec[[i]] <- telt
            inames[[i]] <- list.names[[k]]
            i <- i + 1
        }
        else if (is.matrix(telt)) {
            y <- dimnames(telt)[[2L]]
            for (j in seq_len(ncol(telt))) {
                ivec[[i]] <- telt[, j]
                if (is.null(y) || y[j] == "") 
                  inames[[i]] <- paste0("var", i)
                else inames[[i]] <- y[j]
                i <- i + 1L
            }
        }
        else stop("wrong argument to 'dataentry'")
    }
    names(ivec) <- inames
    return(ivec)
}


aregexec <- function (pattern, text, max.distance = 0.1, costs = NULL, ignore.case = FALSE, 
    fixed = FALSE, useBytes = FALSE) 
{
    costs <- as.integer(.amatch_costs(costs))
    bounds <- .amatch_bounds(max.distance)
    .Internal(aregexec(as.character(pattern), as.character(text), 
        bounds, costs, ignore.case, fixed, useBytes))
}


rc.getOption <- function (name) 
{
    get("options", envir = .CompletionEnv)[[name]]
}


de.restore <- function (inlist, ncols, coltypes, argnames, args) 
{
    p <- length(ncols)
    rlist <- vector("list", length = p)
    rnames <- vector("character", length = p)
    j <- 1L
    lnames <- names(inlist)
    if (p) 
        for (i in seq_len(p)) {
            if (coltypes[i] == 2) {
                tlen <- length(inlist[[j]])
                x <- matrix(0, nrow = tlen, ncol = ncols[i])
                cnames <- vector("character", ncol(x))
                for (ind1 in seq_len(ncols[i])) {
                  if (tlen != length(inlist[[j]])) {
                    warning("could not restore type information")
                    return(inlist)
                  }
                  x[, ind1] <- inlist[[j]]
                  cnames[ind1] <- lnames[j]
                  j <- j + 1L
                }
                if (nrow(x) == nrow(args[[i]])) 
                  rn <- dimnames(args[[i]])[[1L]]
                else rn <- NULL
                if (any(cnames != "")) 
                  dimnames(x) <- list(rn, cnames)
                rlist[[i]] <- x
                rnames[i] <- argnames[i]
            }
            else if (coltypes[i] == 3) {
                x <- vector("list", length = ncols[i])
                cnames <- vector("character", ncols[i])
                for (ind1 in seq_len(ncols[i])) {
                  x[[ind1]] <- inlist[[j]]
                  cnames[ind1] <- lnames[j]
                  j <- j + 1L
                }
                if (any(cnames != "")) 
                  names(x) <- cnames
                rlist[[i]] <- x
                rnames[i] <- argnames[i]
            }
            else {
                rlist[[i]] <- inlist[[j]]
                j <- j + 1
                rnames[i] <- argnames[i]
            }
        }
    names(rlist) <- rnames
    return(rlist)
}


file.edit <- function (..., title = file, editor = getOption("editor"), fileEncoding = "") 
{
    file <- path.expand(c(...))
    title <- rep_len(as.character(title), length(file))
    if (nzchar(fileEncoding) && fileEncoding != "native.enc") {
        tfile <- file
        for (i in seq_along(file)) {
            tfile <- tempfile()
            con <- file(file[i], encoding = fileEncoding)
            writeLines(readLines(con), tfile)
            close(con)
            file[i] <- tfile
        }
    }
    if (is.function(editor)) 
        invisible(editor(file = file, title = title))
    else invisible(.External2(C_fileedit, file, title, editor))
}


citHeader <- function (...) 
{
    rval <- paste(...)
    class(rval) <- "citationHeader"
    rval
}


fix <- function (x, ...) 
{
    subx <- substitute(x)
    if (is.name(subx)) 
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1L) 
        stop("'fix' requires a name")
    parent <- parent.frame()
    if (exists(subx, envir = parent, inherits = TRUE)) 
        x <- edit(get(subx, envir = parent), title = subx, ...)
    else {
        x <- edit(function() {
        }, title = subx, ...)
        environment(x) <- .GlobalEnv
    }
    assign(subx, x, envir = .GlobalEnv)
}


makeRweaveLatexCodeRunner <- function (evalFunc = RweaveEvalWithOpt) 
{
    function(object, chunk, options) {
        pdf.Swd <- function(name, width, height, ...) grDevices::pdf(file = paste0(chunkprefix, 
            ".pdf"), width = width, height = height, version = options$pdf.version, 
            encoding = options$pdf.encoding, compress = options$pdf.compress)
        eps.Swd <- function(name, width, height, ...) grDevices::postscript(file = paste0(name, 
            ".eps"), width = width, height = height, paper = "special", 
            horizontal = FALSE)
        png.Swd <- function(name, width, height, options, ...) grDevices::png(filename = paste0(chunkprefix, 
            ".png"), width = width, height = height, res = options$resolution, 
            units = "in")
        jpeg.Swd <- function(name, width, height, options, ...) grDevices::jpeg(filename = paste0(chunkprefix, 
            ".jpeg"), width = width, height = height, res = options$resolution, 
            units = "in")
        if (!(options$engine %in% c("R", "S"))) 
            return(object)
        devs <- devoffs <- list()
        if (options$fig && options$eval) {
            if (options$pdf) {
                devs <- c(devs, list(pdf.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$eps) {
                devs <- c(devs, list(eps.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$png) {
                devs <- c(devs, list(png.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$jpeg) {
                devs <- c(devs, list(jpeg.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (nzchar(grd <- options$grdevice)) {
                grdo <- paste0(grd, ".off")
                if (grepl("::", grd, fixed = TRUE)) {
                  devs <- c(devs, eval(parse(text = grd)))
                  devoffs <- c(devoffs, if (!inherits(grdo <- tryCatch(eval(parse(text = grdo)), 
                    error = identity), "error")) list(grdo) else list(grDevices::dev.off))
                }
                else {
                  devs <- c(devs, list(get(grd, envir = .GlobalEnv)))
                  devoffs <- c(devoffs, if (exists(grdo, envir = .GlobalEnv)) list(get(grdo, 
                    envir = .GlobalEnv)) else list(grDevices::dev.off))
                }
            }
        }
        if (!object$quiet) {
            cat(formatC(options$chunknr, width = 2), ":")
            if (options$echo) 
                cat(" echo")
            if (options$keep.source) 
                cat(" keep.source")
            if (options$eval) {
                if (options$print) 
                  cat(" print")
                if (options$term) 
                  cat(" term")
                cat("", options$results)
                if (options$fig) {
                  if (options$eps) 
                    cat(" eps")
                  if (options$pdf) 
                    cat(" pdf")
                  if (options$png) 
                    cat(" png")
                  if (options$jpeg) 
                    cat(" jpeg")
                  if (!is.null(options$grdevice)) 
                    cat("", options$grdevice)
                }
            }
            cat(" (")
            if (!is.null(options$label)) 
                cat("label = ", options$label, ", ", sep = "")
            filenum <- attr(chunk, "srcFilenum")[1]
            filename <- attr(chunk, "srcFilenames")[filenum]
            cat(basename(filename), ":", attr(chunk, "srclines")[1], 
                ")", sep = "")
            cat("\n")
        }
        chunkprefix <- RweaveChunkPrefix(options)
        if (options$split) {
            chunkout <- object$chunkout[chunkprefix][[1L]]
            if (is.null(chunkout)) {
                chunkout <- file(paste0(chunkprefix, ".tex"), 
                  "w")
                if (!is.null(options$label)) 
                  object$chunkout[[chunkprefix]] <- chunkout
                if (!grepl(.SweaveValidFilenameRegexp, chunkout)) 
                  warning("file stem ", sQuote(chunkout), " is not portable", 
                    call. = FALSE, domain = NA)
            }
        }
        else chunkout <- object$output
        srcfile <- srcfilecopy(object$filename, chunk, isFile = TRUE)
        chunkexps <- try(parse(text = chunk, srcfile = srcfile), 
            silent = TRUE)
        if (inherits(chunkexps, "try-error")) 
            chunkexps[1L] <- sub(" parse(text = chunk, srcfile = srcfile) : \n ", 
                "", chunkexps[1L], fixed = TRUE)
        RweaveTryStop(chunkexps, options)
        putSinput <- function(dce, leading) {
            if (!openSinput) {
                if (!openSchunk) {
                  cat("\\begin{Schunk}\n", file = chunkout)
                  linesout[thisline + 1L] <<- srcline
                  filenumout[thisline + 1L] <<- srcfilenum
                  thisline <<- thisline + 1L
                  openSchunk <<- TRUE
                }
                cat("\\begin{Sinput}", file = chunkout)
                openSinput <<- TRUE
            }
            leading <- max(leading, 1L)
            cat("\n", paste0(getOption("prompt"), dce[seq_len(leading)], 
                collapse = "\n"), file = chunkout, sep = "")
            if (length(dce) > leading) 
                cat("\n", paste0(getOption("continue"), dce[-seq_len(leading)], 
                  collapse = "\n"), file = chunkout, sep = "")
            linesout[thisline + seq_along(dce)] <<- srcline
            filenumout[thisline + seq_along(dce)] <<- srcfilenum
            thisline <<- thisline + length(dce)
        }
        trySrcLines <- function(srcfile, showfrom, showto, ce) {
            tryCatch(suppressWarnings(getSrcLines(srcfile, showfrom, 
                showto)), error = function(e) {
                if (is.null(ce)) 
                  character()
                else deparse(ce, width.cutoff = 0.75 * getOption("width"))
            })
        }
        echoComments <- function(showto) {
            if (options$echo && !is.na(lastshown) && lastshown < 
                showto) {
                dce <- trySrcLines(srcfile, lastshown + 1L, showto, 
                  NULL)
                linedirs <- startsWith(dce, "#line ")
                dce <- dce[!linedirs]
                if (length(dce)) 
                  putSinput(dce, length(dce))
                lastshown <<- showto
            }
        }
        openSinput <- FALSE
        openSchunk <- FALSE
        srclines <- attr(chunk, "srclines")
        srcfilenums <- attr(chunk, "srcFilenum")
        linesout <- integer()
        filenumout <- integer()
        srcline <- srclines[1L]
        srcfilenum <- srcfilenums[1L]
        thisline <- 0L
        lastshown <- 0L
        leading <- 1L
        srcrefs <- attr(chunkexps, "srcref")
        if (length(devs)) {
            if (!grepl(.SweaveValidFilenameRegexp, chunkprefix)) 
                warning("file stem ", sQuote(chunkprefix), " is not portable", 
                  call. = FALSE, domain = NA)
            if (options$figs.only) 
                devs[[1L]](name = chunkprefix, width = options$width, 
                  height = options$height, options)
        }
        SweaveHooks(options, run = TRUE)
        for (nce in seq_along(chunkexps)) {
            ce <- chunkexps[[nce]]
            if (options$keep.source && nce <= length(srcrefs) && 
                !is.null(srcref <- srcrefs[[nce]])) {
                showfrom <- srcref[7L]
                showto <- srcref[8L]
                dce <- trySrcLines(srcfile, lastshown + 1L, showto, 
                  ce)
                leading <- showfrom - lastshown
                lastshown <- showto
                srcline <- srcref[3L]
                linedirs <- startsWith(dce, "#line ")
                dce <- dce[!linedirs]
                leading <- leading - sum(linedirs[seq_len(leading)])
                while (length(dce) && length(grep("^[[:blank:]]*$", 
                  dce[1L]))) {
                  dce <- dce[-1L]
                  leading <- leading - 1L
                }
            }
            else {
                dce <- deparse(ce, width.cutoff = 0.75 * getOption("width"))
                leading <- 1L
            }
            if (object$debug) 
                cat("\nRnw> ", paste(dce, collapse = "\n+  "), 
                  "\n")
            if (options$echo && length(dce)) 
                putSinput(dce, leading)
            if (options$eval) {
                tmpcon <- file()
                sink(file = tmpcon)
                err <- tryCatch(evalFunc(ce, options), finally = {
                  cat("\n")
                  sink()
                })
                output <- readLines(tmpcon)
                close(tmpcon)
                if (length(output) == 1L && !nzchar(output[1L])) 
                  output <- NULL
                RweaveTryStop(err, options)
            }
            else output <- NULL
            if (length(output) && object$debug) 
                cat(paste(output, collapse = "\n"))
            if (length(output) && (options$results != "hide")) {
                if (openSinput) {
                  cat("\n\\end{Sinput}\n", file = chunkout)
                  linesout[thisline + 1L:2L] <- srcline
                  filenumout[thisline + 1L:2L] <- srcfilenum
                  thisline <- thisline + 2L
                  openSinput <- FALSE
                }
                if (options$results == "verbatim") {
                  if (!openSchunk) {
                    cat("\\begin{Schunk}\n", file = chunkout)
                    linesout[thisline + 1L] <- srcline
                    filenumout[thisline + 1L] <- srcfilenum
                    thisline <- thisline + 1L
                    openSchunk <- TRUE
                  }
                  cat("\\begin{Soutput}\n", file = chunkout)
                  linesout[thisline + 1L] <- srcline
                  filenumout[thisline + 1L] <- srcfilenum
                  thisline <- thisline + 1L
                }
                output <- paste(output, collapse = "\n")
                if (options$strip.white %in% c("all", "true")) {
                  output <- sub("^[[:space:]]*\n", "", output)
                  output <- sub("\n[[:space:]]*$", "", output)
                  if (options$strip.white == "all") 
                    output <- sub("\n[[:space:]]*\n", "\n", output)
                }
                cat(output, file = chunkout)
                count <- sum(strsplit(output, NULL)[[1L]] == 
                  "\n")
                if (count > 0L) {
                  linesout[thisline + 1L:count] <- srcline
                  filenumout[thisline + 1L:count] <- srcfilenum
                  thisline <- thisline + count
                }
                remove(output)
                if (options$results == "verbatim") {
                  cat("\n\\end{Soutput}\n", file = chunkout)
                  linesout[thisline + 1L:2L] <- srcline
                  filenumout[thisline + 1L:2L] <- srcfilenum
                  thisline <- thisline + 2L
                }
            }
        }
        if (options$keep.source) 
            echoComments(length(srcfile$lines))
        if (openSinput) {
            cat("\n\\end{Sinput}\n", file = chunkout)
            linesout[thisline + 1L:2L] <- srcline
            filenumout[thisline + 1L:2L] <- srcfilenum
            thisline <- thisline + 2L
        }
        if (openSchunk) {
            cat("\\end{Schunk}\n", file = chunkout)
            linesout[thisline + 1L] <- srcline
            filenumout[thisline + 1L] <- srcfilenum
            thisline <- thisline + 1L
        }
        if (is.null(options$label) && options$split) 
            close(chunkout)
        if (options$split && options$include) {
            cat("\\input{", chunkprefix, "}\n", sep = "", file = object$output)
            linesout[thisline + 1L] <- srcline
            filenumout[thisline + 1L] <- srcfilenum
            thisline <- thisline + 1L
        }
        if (length(devs)) {
            if (options$figs.only) 
                devoffs[[1L]]()
            for (i in seq_along(devs)) {
                if (options$figs.only && i == 1) 
                  next
                devs[[i]](name = chunkprefix, width = options$width, 
                  height = options$height, options)
                err <- tryCatch({
                  SweaveHooks(options, run = TRUE)
                  eval(chunkexps, envir = .GlobalEnv)
                }, error = function(e) {
                  devoffs[[i]]()
                  stop(conditionMessage(e), call. = FALSE, domain = NA)
                })
                devoffs[[i]]()
            }
            if (options$include) {
                cat("\\includegraphics{", chunkprefix, "}\n", 
                  sep = "", file = object$output)
                linesout[thisline + 1L] <- srcline
                filenumout[thisline + 1L] <- srcfilenum
                thisline <- thisline + 1L
            }
        }
        object$linesout <- c(object$linesout, linesout)
        object$filenumout <- c(object$filenumout, filenumout)
        object
    }
}


.RtangleCodeLabel <- function (chunk) 
{
    if (length(lnos <- grep("^#line ", chunk, value = TRUE))) {
        srclines <- attr(chunk, "srclines")
        lno <- if (length(srclines)) 
            paste(min(srclines), max(srclines), sep = "-")
        else srclines
        fn <- sub("[^\"]*\"([^\"]+).*", "\\1", lnos[1L])
        paste(fn, lno, sep = ":")
    }
    else "(missing #line/file info)"
}


write.socket <- function (socket, string) 
invisible(.Call(C_sockwrite, socket$socket, string))


unzip <- function (zipfile, files = NULL, list = FALSE, overwrite = TRUE, 
    junkpaths = FALSE, exdir = ".", unzip = "internal", setTimes = FALSE) 
{
    if (identical(unzip, "internal")) {
        if (!list && !missing(exdir)) 
            dir.create(exdir, showWarnings = FALSE, recursive = TRUE)
        res <- .External(C_unzip, zipfile, files, exdir, list, 
            overwrite, junkpaths, setTimes)
        if (list) {
            dates <- as.POSIXct(res[[3]], "%Y-%m-%d %H:%M", tz = "UTC")
            data.frame(Name = res[[1]], Length = res[[2]], Date = dates, 
                stringsAsFactors = FALSE)
        }
        else invisible(attr(res, "extracted"))
    }
    else {
        WINDOWS <- .Platform$OS.type == "windows"
        if (!is.character(unzip) || length(unzip) != 1L || !nzchar(unzip)) 
            stop("'unzip' must be a single character string")
        zipfile <- path.expand(zipfile)
        if (list) {
            res <- if (WINDOWS) 
                system2(unzip, c("-l", shQuote(zipfile)), stdout = TRUE)
            else system2(unzip, c("-l", shQuote(zipfile)), stdout = TRUE, 
                env = c("TZ=UTC"))
            l <- length(res)
            res2 <- res[-c(1, 3, l - 1, l)]
            con <- textConnection(res2)
            on.exit(close(con))
            z <- read.table(con, header = TRUE, as.is = TRUE)
            dt <- paste(z$Date, z$Time)
            formats <- if (max(nchar(z$Date) > 8)) 
                c("%Y-%m-%d", "%d-%m-%Y", "%m-%d-%Y")
            else c("%m-%d-%y", "%d-%m-%y", "%y-%m-%d")
            slash <- any(grepl("/", z$Date))
            if (slash) 
                formats <- gsub("-", "/", formats)
            formats <- paste(formats, "%H:%M")
            for (f in formats) {
                zz <- as.POSIXct(dt, tz = "UTC", format = f)
                if (all(!is.na(zz))) 
                  break
            }
            z[, "Date"] <- zz
            z[c("Name", "Length", "Date")]
        }
        else {
            args <- c("-oq", shQuote(zipfile))
            if (length(files)) 
                args <- c(args, shQuote(files))
            if (exdir != ".") 
                args <- c(args, "-d", shQuote(exdir))
            system2(unzip, args, stdout = NULL, stderr = NULL, 
                invisible = TRUE)
            invisible(NULL)
        }
    }
}


Sweave <- function (file, driver = RweaveLatex(), syntax = getOption("SweaveSyntax"), 
    encoding = "", ...) 
{
    if (is.character(driver)) 
        driver <- get(driver, mode = "function")()
    else if (is.function(driver)) 
        driver <- driver()
    if (is.null(syntax)) 
        syntax <- SweaveGetSyntax(file)
    if (is.character(syntax)) 
        syntax <- get(syntax, mode = "list")
    if (.Platform$OS.type == "windows") 
        file <- chartr("\\", "/", file)
    text <- SweaveReadFile(file, syntax, encoding = encoding)
    attr(file, "encoding") <- encoding <- attr(text, "encoding")
    srcFilenames <- attr(text, "files")
    srcFilenum <- attr(text, "srcFilenum")
    srcLinenum <- attr(text, "srcLinenum")
    drobj <- driver$setup(file = file, syntax = syntax, ...)
    on.exit(driver$finish(drobj, error = TRUE))
    syntax <- attr(text, "syntax")
    if (!is.na(envopts <- Sys.getenv("SWEAVE_OPTIONS", NA))) 
        drobj$options <- SweaveParseOptions(envopts, drobj$options, 
            driver$checkopts)
    drobj$filename <- file
    mode <- "doc"
    chunknr <- 0L
    chunk <- NULL
    chunkopts <- NULL
    namedchunks <- list()
    prevfilenum <- 0L
    prevlinediff <- 0L
    for (linenum in seq_along(text)) {
        line <- text[linenum]
        filenum <- srcFilenum[linenum]
        linediff <- srcLinenum[linenum] - linenum
        if (nzchar(Sys.getenv("R_DEBUG_Sweave"))) {
            cat(sprintf("l.%3d: %30s -'%4s'- ", linenum, substr(line, 
                1, 30), mode))
            cat(sprintf("%16s\n", system(paste("ls -s", summary(drobj$output)$description), 
                intern = TRUE)))
        }
        if (length(grep(syntax$doc, line))) {
            if (mode == "doc") {
                if (!is.null(chunk)) 
                  drobj <- driver$writedoc(drobj, chunk)
            }
            else {
                if (!is.null(chunkopts$label)) 
                  namedchunks[[chunkopts$label]] <- chunk
                if (!is.null(chunk)) 
                  drobj <- driver$runcode(drobj, chunk, chunkopts)
                mode <- "doc"
            }
            chunk <- NULL
        }
        else if (length(grep(syntax$code, line))) {
            if (mode == "doc") {
                if (!is.null(chunk)) 
                  drobj <- driver$writedoc(drobj, chunk)
            }
            else {
                if (!is.null(chunkopts$label)) 
                  namedchunks[[chunkopts$label]] <- chunk
                if (!is.null(chunk)) 
                  drobj <- driver$runcode(drobj, chunk, chunkopts)
            }
            mode <- "code"
            chunkopts <- sub(syntax$code, "\\1", line)
            chunkopts <- SweaveParseOptions(chunkopts, drobj$options, 
                driver$checkopts)
            file <- srcFilenames[filenum]
            chunk <- paste0("#line ", linenum + linediff + 1L, 
                " \"", basename(file), "\"")
            attr(chunk, "srclines") <- linenum + linediff
            attr(chunk, "srcFilenum") <- filenum
            attr(chunk, "srcFilenames") <- srcFilenames
            chunknr <- chunknr + 1L
            chunkopts$chunknr <- chunknr
        }
        else {
            if (mode == "code" && length(grep(syntax$coderef, 
                line))) {
                chunkref <- sub(syntax$coderef, "\\1", line)
                if (!(chunkref %in% names(namedchunks))) {
                  warning(gettextf("reference to unknown chunk %s", 
                    sQuote(chunkref)), call. = TRUE, domain = NA)
                  next
                }
                else {
                  file <- srcFilenames[filenum]
                  line <- c(namedchunks[[chunkref]], paste0("#line ", 
                    linenum + linediff + 1L, " \"", basename(file), 
                    "\""))
                }
            }
            if (mode == "code" && (prevfilenum != filenum || 
                prevlinediff != linediff)) {
                file <- srcFilenames[filenum]
                line <- c(paste0("#line ", linenum + linediff, 
                  " \"", basename(file), "\""), line)
            }
            srclines <- c(attr(chunk, "srclines"), rep(linenum + 
                linediff, length(line)))
            srcfilenum <- c(attr(chunk, "srcFilenum"), rep(filenum, 
                length(line)))
            chunk <- c(chunk, line)
            attr(chunk, "srclines") <- srclines
            attr(chunk, "srcFilenum") <- srcfilenum
            attr(chunk, "srcFilenames") <- srcFilenames
        }
        prevfilenum <- filenum
        prevlinediff <- linediff
    }
    if (!is.null(chunk)) {
        drobj <- if (mode == "doc") 
            driver$writedoc(drobj, chunk)
        else driver$runcode(drobj, chunk, chunkopts)
    }
    on.exit()
    drobj$srcFilenames <- srcFilenames
    driver$finish(drobj)
}


nsl <- function (hostname) 
.Call(C_nsl, hostname)


read.fwf <- function (file, widths, header = FALSE, sep = "\t", skip = 0L, 
    row.names, col.names, n = -1L, buffersize = 2000, fileEncoding = "", 
    ...) 
{
    doone <- function(x) {
        x <- substring(x, first, last)
        x[!nzchar(x)] <- NA_character_
        x
    }
    if (is.list(widths)) {
        recordlength <- length(widths)
        widths <- do.call("c", widths)
    }
    else recordlength <- 1L
    drop <- (widths < 0L)
    widths <- abs(widths)
    buffersize <- (buffersize%/%recordlength) * recordlength
    FILENAME <- tempfile("Rfwf.")
    on.exit(unlink(FILENAME))
    FILE <- file(FILENAME, "a")
    on.exit(close(FILE), add = TRUE)
    if (is.character(file)) {
        file <- if (nzchar(fileEncoding)) 
            file(file, "rt", encoding = fileEncoding)
        else file(file, "rt")
        on.exit(close(file), add = TRUE)
    }
    else if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file), add = TRUE)
    }
    if (skip) 
        readLines(file, n = skip)
    if (header) {
        headerline <- readLines(file, n = 1L)
        cat(file = FILE, headerline, "\n")
    }
    repeat ({
        if (n == 0L) 
            break
        if (n == -1L) 
            thisblock <- buffersize
        else thisblock <- min(buffersize, n * recordlength)
        raw <- readLines(file, n = thisblock)
        nread <- length(raw)
        if (recordlength > 1L && nread%%recordlength) {
            raw <- raw[1L:(nread - nread%%recordlength)]
            warning(sprintf(ngettext(nread%%recordlength, "last record incomplete, %d line discarded", 
                "last record incomplete, %d lines discarded"), 
                nread%%recordlength), domain = NA)
        }
        if (recordlength > 1L) {
            raw <- matrix(raw, nrow = recordlength)
            raw <- apply(raw, 2L, paste, collapse = "")
        }
        st <- c(1L, 1L + cumsum(widths))
        first <- st[-length(st)][!drop]
        last <- cumsum(widths)[!drop]
        cat(file = FILE, sapply(raw, doone), sep = c(rep_len(sep, 
            length(first) - 1L), "\n"))
        if (nread < thisblock) 
            break
        if (n > 0L) 
            n <- n - length(raw)
    })
    close(FILE)
    FILE <- file(FILENAME, "r")
    read.table(file = FILE, header = header, sep = sep, row.names = row.names, 
        col.names = col.names, quote = "", ...)
}


CRAN.packages <- function (CRAN = getOption("repos"), method, contriburl = contrib.url(CRAN)) 
.Defunct("available.packages")


RweaveEvalWithOpt <- function (expr, options) 
{
    if (options$eval) {
        res <- try(withVisible(eval(expr, .GlobalEnv)), silent = TRUE)
        if (inherits(res, "try-error")) 
            return(res)
        if (options$print || (options$term && res$visible)) {
            if (.isMethodsDispatchOn() && isS4(res$value)) 
                methods::show(res$value)
            else print(res$value)
        }
    }
    res
}


limitedLabels <- function (value, maxwidth = getOption("width") - 5L) 
{
    srcrefs <- sapply(value, function(v) if (!is.null(srcref <- attr(v, 
        "srcref"))) {
        srcfile <- attr(srcref, "srcfile")
        paste0(basename(srcfile$filename), "#", srcref[1L], ": ")
    }
    else "")
    value <- paste0(srcrefs, as.character(value))
    if (is.null(maxwidth) || maxwidth < 40L) 
        maxwidth <- 40L
    maxwidth <- min(maxwidth, 1000L)
    strtrim(value, maxwidth)
}


readCitationFile <- function (file, meta = NULL) 
{
    meta <- as.list(meta)
    exprs <- tools:::.parse_CITATION_file(file, meta$Encoding)
    rval <- list()
    mheader <- NULL
    mfooter <- NULL
    envir <- new.env(hash = TRUE)
    assign("meta", meta, envir = envir)
    for (expr in exprs) {
        x <- eval(expr, envir = envir)
        if (inherits(x, "bibentry")) 
            rval <- c(rval, list(x))
        else if (identical(class(x), "citationHeader")) 
            mheader <- c(mheader, x)
        else if (identical(class(x), "citationFooter")) 
            mfooter <- c(mfooter, x)
    }
    rval <- if (length(rval) == 1L) 
        rval[[1L]]
    else do.call("c", rval)
    if (!.is_not_nonempty_text(mheader)) 
        attr(rval, "mheader") <- paste(mheader, collapse = "\n")
    if (!.is_not_nonempty_text(mfooter)) 
        attr(rval, "mfooter") <- paste(mfooter, collapse = "\n")
    .citation(rval)
}


`?` <- function (e1, e2) 
{
    if (missing(e2)) {
        type <- NULL
        topicExpr <- substitute(e1)
    }
    else {
        type <- substitute(e1)
        topicExpr <- substitute(e2)
    }
    search <- (is.call(topicExpr) && topicExpr[[1L]] == "?")
    if (search) {
        topicExpr <- topicExpr[[2L]]
        if (is.call(te <- topicExpr) && te[[1L]] == "?" && is.call(te <- topicExpr[[2L]]) && 
            te[[1L]] == "?") {
            cat("Contacting Delphi...")
            flush.console()
            Sys.sleep(2 + stats::rpois(1, 2))
            cat("the oracle is unavailable.\nWe apologize for any inconvenience.\n")
            return(invisible())
        }
    }
    if (is.call(topicExpr) && (topicExpr[[1L]] == "::" || topicExpr[[1L]] == 
        ":::")) {
        package <- as.character(topicExpr[[2L]])
        topicExpr <- topicExpr[[3L]]
    }
    else package <- NULL
    if (search) {
        if (is.null(type)) 
            return(eval(substitute(help.search(TOPIC, package = PACKAGE), 
                list(TOPIC = as.character(topicExpr), PACKAGE = package))))
        else return(eval(substitute(help.search(TOPIC, fields = FIELD, 
            package = PACKAGE), list(TOPIC = as.character(topicExpr), 
            FIELD = as.character(type), PACKAGE = package))))
    }
    else {
        if (is.null(type)) {
            if (is.call(topicExpr)) 
                return(.helpForCall(topicExpr, parent.frame()))
            topic <- if (is.name(topicExpr)) 
                as.character(topicExpr)
            else e1
            return(eval(substitute(help(TOPIC, package = PACKAGE), 
                list(TOPIC = topic, PACKAGE = package))))
        }
        else {
            type <- if (is.name(type)) 
                as.character(type)
            else e1
            topic <- if (is.name(topicExpr)) 
                as.character(topicExpr)
            else {
                if (is.call(topicExpr) && identical(type, "method")) 
                  return(.helpForCall(topicExpr, parent.frame(), 
                    FALSE))
                e2
            }
            if (type == "package") 
                package <- topic
            h <- .tryHelp(topicName(type, topic), package = package)
            if (is.null(h)) {
                if (is.language(topicExpr)) 
                  topicExpr <- deparse(topicExpr)
                stop(gettextf("no documentation of type %s and topic %s (or error in processing help)", 
                  sQuote(type), sQuote(topicExpr)), domain = NA)
            }
            h
        }
    }
}


process.events <- function () 
invisible(.Call(C_processevents))


as.relistable <- function (x) 
{
    if (!inherits(x, "relistable")) 
        class(x) <- c("relistable", class(x))
    x
}


getParseText <- function (parseData, id) 
{
    srcfile <- attr(parseData, "srcfile")
    d <- parseData[as.character(id), ]
    text <- d$text
    if (is.null(text)) {
        text <- character(nrow(d))
        blank <- seq_along(text)
    }
    else blank <- which(!nzchar(text) | (d$token == "STR_CONST" & 
        startsWith(text, "[")))
    for (i in blank) {
        lines <- getSrcLines(srcfile, d$line1[i], d$line2[i])
        n <- length(lines)
        lines[n] <- substr_with_tabs(lines[n], 1, d$col2[i])
        lines[1] <- substr_with_tabs(lines[1], d$col1[i], Inf)
        text[i] <- paste(lines, collapse = "\n")
    }
    text
}


isS3stdGeneric <- function (f) 
{
    bdexpr <- body(f)
    while (as.character(bdexpr[[1L]]) == "{") bdexpr <- bdexpr[[2L]]
    ret <- is.call(bdexpr) && identical(bdexpr[[1L]], as.name("UseMethod"))
    if (ret) 
        names(ret) <- bdexpr[[2L]]
    ret
}


modifyList <- function (x, val, keep.null = FALSE) 
{
    stopifnot(is.list(x), is.list(val))
    xnames <- names(x)
    vnames <- names(val)
    vnames <- vnames[nzchar(vnames)]
    if (keep.null) {
        for (v in vnames) {
            x[v] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
                list(modifyList(x[[v]], val[[v]], keep.null = keep.null))
            else val[v]
        }
    }
    else {
        for (v in vnames) {
            x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && 
                is.list(val[[v]])) 
                modifyList(x[[v]], val[[v]], keep.null = keep.null)
            else val[[v]]
        }
    }
    x
}


maintainer <- function (pkg) 
{
    force(pkg)
    desc <- packageDescription(pkg)
    if (is.list(desc)) 
        gsub("\n", " ", desc$Maintainer, fixed = TRUE)
    else NA_character_
}


available.packages <- function (contriburl = contrib.url(repos, type), method, fields = NULL, 
    type = getOption("pkgType"), filters = NULL, repos = getOption("repos")) 
{
    requiredFields <- c(tools:::.get_standard_repository_db_fields(), 
        "File")
    if (is.null(fields)) 
        fields <- requiredFields
    else {
        stopifnot(is.character(fields))
        fields <- unique(c(requiredFields, fields))
    }
    res <- matrix(NA_character_, 0L, length(fields) + 1L, dimnames = list(NULL, 
        c(fields, "Repository")))
    for (repos in contriburl) {
        localcran <- length(grep("^file:", repos)) > 0L
        if (localcran) {
            if (substring(repos, 1L, 8L) == "file:///") {
                tmpf <- paste0(substring(repos, 8L), "/PACKAGES")
                if (.Platform$OS.type == "windows") {
                  if (length(grep("^/[A-Za-z]:", tmpf))) 
                    tmpf <- substring(tmpf, 2L)
                }
            }
            else {
                tmpf <- paste0(substring(repos, 6L), "/PACKAGES")
            }
            res0 <- if (file.exists(dest <- paste0(tmpf, ".rds"))) 
                readRDS(dest)
            else read.dcf(file = tmpf)
            if (length(res0)) 
                rownames(res0) <- res0[, "Package"]
        }
        else {
            dest <- file.path(tempdir(), paste0("repos_", URLencode(repos, 
                TRUE), ".rds"))
            if (file.exists(dest)) {
                res0 <- readRDS(dest)
                if (length(res0)) 
                  rownames(res0) <- res0[, "Package"]
            }
            else {
                need_dest <- FALSE
                op <- options(warn = -1L)
                z <- tryCatch({
                  download.file(url = paste0(repos, "/PACKAGES.rds"), 
                    destfile = dest, method = method, cacheOK = FALSE, 
                    quiet = TRUE, mode = "wb")
                }, error = identity)
                options(op)
                if (!inherits(z, "error")) 
                  z <- res0 <- tryCatch(readRDS(dest), error = identity)
                if (inherits(z, "error")) {
                  need_dest <- TRUE
                  tmpf <- tempfile()
                  on.exit(unlink(tmpf))
                  op <- options(warn = -1L)
                  z <- tryCatch({
                    download.file(url = paste0(repos, "/PACKAGES.gz"), 
                      destfile = tmpf, method = method, cacheOK = FALSE, 
                      quiet = TRUE, mode = "wb")
                  }, error = identity)
                  if (inherits(z, "error")) 
                    z <- tryCatch({
                      download.file(url = paste0(repos, "/PACKAGES"), 
                        destfile = tmpf, method = method, cacheOK = FALSE, 
                        quiet = TRUE, mode = "wb")
                    }, error = identity)
                  options(op)
                  if (!inherits(z, "error")) 
                    z <- res0 <- tryCatch(read.dcf(file = tmpf), 
                      error = identity)
                  unlink(tmpf)
                  on.exit()
                }
                if (inherits(z, "error")) {
                  warning(gettextf("unable to access index for repository %s", 
                    repos), ":\n  ", conditionMessage(z), call. = FALSE, 
                    immediate. = TRUE, domain = NA)
                  next
                }
                if (length(res0)) {
                  rownames(res0) <- res0[, "Package"]
                  if (need_dest) 
                    saveRDS(res0, dest, compress = TRUE)
                }
                else if (!need_dest) {
                  unlink(dest)
                }
            }
        }
        if (length(res0)) {
            missingFields <- fields[!(fields %in% colnames(res0))]
            if (length(missingFields)) {
                toadd <- matrix(NA_character_, nrow = nrow(res0), 
                  ncol = length(missingFields), dimnames = list(NULL, 
                    missingFields))
                res0 <- cbind(res0, toadd)
            }
            if ("Path" %in% colnames(res0)) {
                rp <- rep.int(repos, nrow(res0))
                path <- res0[, "Path"]
                rp[!is.na(path)] <- paste(repos, path[!is.na(path)], 
                  sep = "/")
            }
            else rp <- repos
            res0 <- cbind(res0[, fields, drop = FALSE], Repository = rp)
            res <- rbind(res, res0)
        }
    }
    if (!length(res)) 
        return(res)
    if (is.null(filters)) {
        filters <- getOption("available_packages_filters")
        if (is.null(filters)) 
            filters <- available_packages_filters_default
    }
    if (is.list(filters)) {
        if (identical(filters$add, TRUE)) {
            filters$add <- NULL
            filters <- c(available_packages_filters_default, 
                filters)
        }
    }
    for (f in filters) {
        if (!length(res)) 
            break
        if (is.character(f)) {
            f <- available_packages_filters_db[[f[1L]]]
        }
        if (!is.function(f)) 
            stop("invalid 'filters' argument.")
        res <- f(res)
    }
    res
}


citeNatbib <- function (keys, bib, textual = FALSE, before = NULL, after = NULL, 
    mode = c("authoryear", "numbers", "super"), abbreviate = TRUE, 
    longnamesfirst = TRUE, bibpunct = c("(", ")", ";", "a", "", 
        ","), previous) 
{
    shortName <- function(person) {
        if (length(person$family)) 
            paste(tools:::cleanupLatex(person$family), collapse = " ")
        else paste(tools:::cleanupLatex(person$given), collapse = " ")
    }
    authorList <- function(paper) sapply(paper$author, shortName)
    if (!missing(previous)) 
        cited <<- previous
    if (!missing(mode)) 
        mode <- match.arg(mode)
    else mode <- switch(bibpunct[4L], n = "numbers", s = "super", 
        "authoryear")
    numeric <- mode %in% c("numbers", "super")
    if (numeric) 
        bib <- sort(bib)
    keys <- unlist(strsplit(keys, " *, *"))
    if (!length(keys)) 
        return("")
    n <- length(keys)
    first <- !(keys %in% cited)
    cited <<- unique(c(cited, keys))
    bibkeys <- unlist(bib$key)
    year <- match(keys, bibkeys)
    papers <- bib[year]
    if (textual || !numeric) {
        auth <- character(n)
        if (!numeric) 
            year <- unlist(papers$year)
        authorLists <- lapply(papers, authorList)
        lastAuthors <- NULL
        for (i in seq_along(keys)) {
            authors <- authorLists[[i]]
            if (identical(lastAuthors, authors)) 
                auth[i] <- ""
            else {
                if (length(authors) > 1L) 
                  authors[length(authors)] <- paste("and", authors[length(authors)])
                if (length(authors) > 2L) {
                  if (!abbreviate || (first[i] && longnamesfirst)) 
                    auth[i] <- paste(authors, collapse = ", ")
                  else auth[i] <- paste(authors[1L], "et al.")
                }
                else auth[i] <- paste(authors, collapse = " ")
            }
            lastAuthors <- authors
        }
        suppressauth <- which(!nzchar(auth))
        if (length(suppressauth)) {
            for (i in suppressauth) year[i - 1L] <- paste0(year[i - 
                1L], bibpunct[6L], " ", year[i])
            auth <- auth[-suppressauth]
            year <- year[-suppressauth]
        }
    }
    if (!is.null(before)) 
        before <- paste0(before, " ")
    if (!is.null(after)) 
        after <- paste0(" ", after)
    if (textual) {
        result <- paste0(bibpunct[1L], before, year, after, bibpunct[2L])
        if (mode == "super") 
            result <- paste0(auth, "^{", result, "}")
        else result <- paste0(auth, " ", result)
        result <- paste(result, collapse = paste0(bibpunct[3L], 
            " "))
    }
    else if (numeric) {
        result <- paste(year, collapse = paste0(bibpunct[3L], 
            " "))
        result <- paste0(bibpunct[1L], before, result, after, 
            bibpunct[2L])
        if (mode == "super") 
            result <- paste0("^{", result, "}")
    }
    else {
        result <- paste0(auth, bibpunct[5L], " ", year)
        result <- paste(result, collapse = paste0(bibpunct[3L], 
            " "))
        result <- paste0(bibpunct[1L], before, result, after, 
            bibpunct[2L])
    }
    result
}


rc.options <- function (...) 
{
    new <- list(...)
    if (is.null(names(new)) && length(new) == 1L && is.list(new[[1L]])) 
        new <- new[[1L]]
    old <- .CompletionEnv$options
    if (length(new) == 0L) 
        return(old)
    nm <- names(new)
    if (is.null(nm)) 
        return(old[unlist(new)])
    isNamed <- nzchar(nm)
    if (any(!isNamed)) 
        nm[!isNamed] <- unlist(new[!isNamed])
    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]
    .CompletionEnv$options <- modifyList(old, new[nm])
    invisible(retVal)
}


setBreakpoint <- function (srcfile, line, nameonly = TRUE, envir = parent.frame(), 
    lastenv, verbose = TRUE, tracer, print = FALSE, clear = FALSE, 
    ...) 
{
    if (missing(lastenv)) {
        if (missing(envir)) 
            lastenv <- globalenv()
        else lastenv <- emptyenv()
    }
    locations <- findLineNum(srcfile, line, nameonly, envir, 
        lastenv)
    if (verbose) 
        print(locations, steps = !clear)
    breakpoint <- missing(tracer)
    while (length(locations)) {
        what <- locations[[1]]$name
        where <- locations[[1]]$env
        at <- list(locations[[1]]$at)
        signature <- locations[[1]]$signature
        if (breakpoint) {
            filename <- basename(locations[[1]]$filename)
            linenum <- locations[[1]]$line
            tracer <- bquote({
                cat(paste0(.(filename), "#", .(linenum), "\n"))
                browser(skipCalls = 4L)
            })
        }
        locations[[1]] <- NULL
        i <- 1
        while (i <= length(locations)) {
            if (what == locations[[i]]$name && identical(where, 
                locations[[i]]$env) && identical(signature, locations[[i]]$signature)) {
                at <- c(at, list(locations[[i]]))
                locations[[i]] <- NULL
            }
            else i <- i + 1
        }
        if (clear) {
            if (is.null(signature)) 
                untrace(what, where = where)
            else untrace(what, signature = signature, where = where)
        }
        else if (is.null(signature)) 
            trace(what, tracer, at = at, where = where, print = print, 
                ...)
        else trace(what, signature = signature, tracer, at = at, 
            where = where, ...)
    }
}


Rtangle <- function () 
{
    list(setup = RtangleSetup, runcode = RtangleRuncode, writedoc = RtangleWritedoc, 
        finish = RtangleFinish, checkopts = RweaveLatexOptions)
}


head.matrix <- function (x, n = 6L, ...) 
{
    stopifnot(length(n) == 1L)
    n <- if (n < 0L) 
        max(nrow(x) + n, 0L)
    else min(n, nrow(x))
    x[seq_len(n), , drop = FALSE]
}


getTxtProgressBar <- function (pb) 
{
    if (!inherits(pb, "txtProgressBar")) 
        stop(gettextf("'pb' is not from class %s", dQuote("txtProgressBar")), 
            domain = NA)
    pb$getVal()
}


tail <- function (x, ...) 
UseMethod("tail")


changedFiles <- function (before, after, path = before$path, timestamp = before$timestamp, 
    check.file.info = c("size", "isdir", "mode", "mtime"), md5sum = before$md5sum, 
    digest = before$digest, full.names = before$full.names, ...) 
{
    stopifnot(inherits(before, "fileSnapshot"))
    if (missing(after)) {
        get.file.info <- length(check.file.info) > 0 && before$file.info
        args <- before$args
        newargs <- list(...)
        args[names(newargs)] <- newargs
        after <- do.call(fileSnapshot, c(list(path = path, timestamp = NULL, 
            file.info = get.file.info, md5sum = md5sum, digest = digest, 
            full.names = full.names), args))
    }
    stopifnot(inherits(after, "fileSnapshot"))
    preinfo <- before$info
    postinfo <- after$info
    prenames <- rownames(preinfo)
    postnames <- rownames(postinfo)
    added <- setdiff(postnames, prenames)
    deleted <- setdiff(prenames, postnames)
    common <- intersect(prenames, postnames)
    if (!before$file.info || !after$file.info) 
        check.file.info <- NULL
    if (length(check.file.info)) {
        pre <- preinfo[common, check.file.info, drop = FALSE]
        post <- postinfo[common, check.file.info, drop = FALSE]
        changes <- pre != post
    }
    else changes <- matrix(logical(0), nrow = length(common), 
        ncol = 0, dimnames = list(common, character(0)))
    if (length(timestamp)) 
        if (file.exists(timestamp)) {
            fullnames <- if (after$full.names) 
                common
            else file.path(after$path, common)
            changes <- cbind(changes, Newer = file_test("-nt", 
                fullnames, timestamp))
        }
        else warning("Timestamp file no longer exists.")
    if (md5sum) {
        pre <- preinfo[common, "md5sum"]
        post <- postinfo[common, "md5sum"]
        changes <- cbind(changes, md5sum = pre != post)
    }
    if (!is.null(digest)) {
        pre <- preinfo[common, "digest"]
        post <- postinfo[common, "digest"]
        changes <- cbind(changes, digest = pre != post)
    }
    changed <- rownames(changes)[rowSums(changes, na.rm = TRUE) > 
        0]
    structure(list(added = added, deleted = deleted, changed = changed, 
        unchanged = setdiff(common, changed), changes = changes), 
        class = "changedFiles")
}


RweaveLatexFinish <- function (object, error = FALSE) 
{
    outputname <- summary(object$output)$description
    if (!object$quiet && !error) {
        if (!file.exists(outputname)) 
            stop(gettextf("the output file '%s' has disappeared", 
                outputname))
        cat("\n", sprintf("You can now run (pdf)latex on %s", 
            sQuote(outputname)), "\n", sep = "")
    }
    close(object$output)
    if (length(object$chunkout)) 
        for (con in object$chunkout) close(con)
    if (object$haveconcordance) {
        linesout <- object$linesout
        filenumout <- object$filenumout
        filenames <- object$srcFilenames[filenumout]
        if (!is.null(filenames)) {
            filegps <- rle(filenames)
            offset <- 0L
            for (i in seq_along(filegps$lengths)) {
                len <- filegps$lengths[i]
                inputname <- filegps$values[i]
                vals <- rle(diff(linesout[offset + seq_len(len)]))
                vals <- c(linesout[offset + 1L], as.numeric(rbind(vals$lengths, 
                  vals$values)))
                concordance <- paste(strwrap(paste(vals, collapse = " ")), 
                  collapse = " %\n")
                special <- paste0("\\Sconcordance{concordance:", 
                  outputname, ":", inputname, ":", if (offset) 
                    paste0("ofs ", offset, ":")
                  else "", "%\n", concordance, "}\n")
                cat(special, file = object$concordfile, append = offset > 
                  0L)
                offset <- offset + len
            }
        }
    }
    invisible(outputname)
}


getAnywhere <- function (x) 
{
    if (tryCatch(!is.character(x), error = function(e) TRUE)) 
        x <- as.character(substitute(x))
    objs <- list()
    where <- character()
    visible <- logical()
    if (length(pos <- find(x, numeric = TRUE))) {
        objs <- lapply(pos, function(pos, x) get(x, pos = pos), 
            x = x)
        where <- names(pos)
        visible <- rep.int(TRUE, length(pos))
    }
    if (length(grep(".", x, fixed = TRUE))) {
        np <- length(parts <- strsplit(x, ".", fixed = TRUE)[[1L]])
        for (i in 2:np) {
            gen <- paste(parts[1L:(i - 1)], collapse = ".")
            cl <- paste(parts[i:np], collapse = ".")
            if (gen == "" || cl == "") 
                next
            Call <- substitute(getS3method(gen, cl, TRUE), list(gen = gen, 
                cl = cl))
            f <- eval.parent(Call)
            if (!is.null(f) && !is.null(environment(f))) {
                ev <- topenv(environment(f), baseenv())
                nmev <- if (isNamespace(ev)) 
                  getNamespaceName(ev)
                else NULL
                objs <- c(objs, list(f))
                msg <- paste("registered S3 method for", gen)
                if (!is.null(nmev)) 
                  msg <- paste(msg, "from namespace", nmev)
                where <- c(where, msg)
                visible <- c(visible, FALSE)
            }
        }
    }
    for (i in loadedNamespaces()) {
        ns <- asNamespace(i)
        if (exists(x, envir = ns, inherits = FALSE)) {
            f <- get(x, envir = ns, inherits = FALSE)
            objs <- c(objs, list(f))
            where <- c(where, paste0("namespace:", i))
            visible <- c(visible, FALSE)
        }
    }
    ln <- length(objs)
    dups <- rep.int(FALSE, ln)
    if (ln > 1L) 
        for (i in 2L:ln) for (j in 1L:(i - 1L)) if (identical(objs[[i]], 
            objs[[j]], ignore.environment = TRUE)) {
            dups[i] <- TRUE
            break
        }
    structure(list(name = x, objs = objs, where = where, visible = visible, 
        dups = dups), class = "getAnywhere")
}


browseEnv <- function (envir = .GlobalEnv, pattern, excludepatt = "^last\\.warning", 
    html = .Platform$GUI != "AQUA", expanded = TRUE, properties = NULL, 
    main = NULL, debugMe = FALSE) 
{
    objlist <- ls(envir = envir, pattern = pattern)
    if (length(iX <- grep(excludepatt, objlist))) 
        objlist <- objlist[-iX]
    if (debugMe) {
        cat("envir= ")
        print(envir)
        cat("objlist =\n")
        print(objlist)
    }
    n <- length(objlist)
    if (n == 0L) {
        cat("Empty environment, nothing to do!\n")
        return(invisible())
    }
    str1 <- function(obj) {
        md <- mode(obj)
        lg <- length(obj)
        objdim <- dim(obj)
        if (length(objdim) == 0L) 
            dim.field <- paste("length:", lg)
        else {
            dim.field <- "dim:"
            for (i in seq_along(objdim)) dim.field <- paste(dim.field, 
                objdim[i])
            if (is.matrix(obj)) 
                md <- "matrix"
        }
        obj.class <- oldClass(obj)
        if (!is.null(obj.class)) {
            md <- obj.class[1L]
            if (inherits(obj, "factor")) 
                dim.field <- paste("levels:", length(levels(obj)))
        }
        list(type = md, dim.field = dim.field)
    }
    N <- 0L
    M <- n
    IDS <- rep.int(NA, n)
    NAMES <- rep.int(NA, n)
    TYPES <- rep.int(NA, n)
    DIMS <- rep.int(NA, n)
    IsRoot <- rep.int(TRUE, n)
    Container <- rep.int(FALSE, n)
    ItemsPerContainer <- rep.int(0, n)
    ParentID <- rep.int(-1, n)
    for (objNam in objlist) {
        N <- N + 1L
        if (debugMe) 
            cat("  ", N, ":", objNam)
        obj <- get(objNam, envir = envir)
        sOb <- str1(obj)
        if (debugMe) 
            cat(", type=", sOb$type, ",", sOb$dim.field, "\n")
        IDS[N] <- N
        NAMES[N] <- objNam
        TYPES[N] <- sOb$type
        DIMS[N] <- sOb$dim.field
        if (is.recursive(obj) && !is.function(obj) && !is.environment(obj) && 
            (lg <- length(obj))) {
            Container[N] <- TRUE
            ItemsPerContainer[N] <- lg
            nm <- names(obj)
            if (is.null(nm)) 
                nm <- paste0("[[", format(1L:lg), "]]")
            for (i in 1L:lg) {
                M <- M + 1
                ParentID[M] <- N
                if (nm[i] == "") 
                  nm[i] <- paste0("[[", i, "]]")
                s.l <- str1(obj[[i]])
                IDS <- c(IDS, M)
                NAMES <- c(NAMES, nm[i])
                TYPES <- c(TYPES, s.l$type)
                DIMS <- c(DIMS, s.l$dim.field)
            }
        }
        else if (!is.null(class(obj))) {
            if (inherits(obj, "table")) {
                obj.nms <- attr(obj, "dimnames")
                lg <- length(obj.nms)
                if (length(names(obj.nms)) > 0) 
                  nm <- names(obj.nms)
                else nm <- rep.int("", lg)
                Container[N] <- TRUE
                ItemsPerContainer[N] <- lg
                for (i in seq_len(lg)) {
                  M <- M + 1L
                  ParentID[M] <- N
                  if (nm[i] == "") 
                    nm[i] = paste0("[[", i, "]]")
                  md.l <- mode(obj.nms[[i]])
                  objdim.l <- dim(obj.nms[[i]])
                  if (length(objdim.l) == 0L) 
                    dim.field.l <- paste("length:", length(obj.nms[[i]]))
                  else {
                    dim.field.l <- "dim:"
                    for (j in seq_along(objdim.l)) dim.field.l <- paste(dim.field.l, 
                      objdim.l[i])
                  }
                  IDS <- c(IDS, M)
                  NAMES <- c(NAMES, nm[i])
                  TYPES <- c(TYPES, md.l)
                  DIMS <- c(DIMS, dim.field.l)
                }
            }
            else if (inherits(obj, "mts")) {
                nm <- dimnames(obj)[[2L]]
                lg <- length(nm)
                Container[N] <- TRUE
                ItemsPerContainer[N] <- lg
                for (i in seq_len(lg)) {
                  M <- M + 1L
                  ParentID[M] <- N
                  md.l <- mode(obj[[i]])
                  dim.field.l <- paste("length:", dim(obj)[1L])
                  md.l <- "ts"
                  IDS <- c(IDS, M)
                  NAMES <- c(NAMES, nm[i])
                  TYPES <- c(TYPES, md.l)
                  DIMS <- c(DIMS, dim.field.l)
                }
            }
        }
    }
    if (debugMe) 
        cat(" __end {for}\n ")
    Container <- c(Container, rep.int(FALSE, M - N))
    IsRoot <- c(IsRoot, rep.int(FALSE, M - N))
    ItemsPerContainer <- c(ItemsPerContainer, rep.int(0, M - 
        N))
    if (is.null(main)) 
        main <- paste("R objects in", deparse(substitute(envir)))
    if (is.null(properties)) {
        properties <- as.list(c(date = format(Sys.time(), "%Y-%b-%d %H:%M"), 
            local({
                si <- Sys.info()
                si[c("user", "nodename", "sysname")]
            })))
    }
    if (html) 
        wsbrowser(IDS, IsRoot, Container, ItemsPerContainer, 
            ParentID, NAMES, TYPES, DIMS, kind = "HTML", main = main, 
            properties = properties, expanded)
    else if (.Platform$GUI == "AQUA") {
        awsbrowser <- get("wsbrowser", envir = as.environment("tools:RGUI"))
        awsbrowser(as.integer(IDS), IsRoot, Container, as.integer(ItemsPerContainer), 
            as.integer(ParentID), NAMES, TYPES, DIMS)
    }
    else stop("only 'html = TRUE' is supported on this platform")
}


object.size <- function (x) 
structure(.Call(C_objectSize, x), class = "object_size")


globalVariables <- function (names, package, add = TRUE) 
registerNames(names, package, ".__global__", add)


packageVersion <- function (pkg, lib.loc = NULL) 
{
    res <- suppressWarnings(packageDescription(pkg, lib.loc = lib.loc, 
        fields = "Version"))
    if (!is.na(res)) 
        package_version(res)
    else stop(gettextf("package %s not found", sQuote(pkg)), 
        domain = NA)
}


help.search <- function (pattern, fields = c("alias", "concept", "title"), apropos, 
    keyword, whatis, ignore.case = TRUE, package = NULL, lib.loc = NULL, 
    help.db = getOption("help.db"), verbose = getOption("verbose"), 
    rebuild = FALSE, agrep = NULL, use_UTF8 = FALSE, types = getOption("help.search.types")) 
{
    .wrong_args <- function(args) gettextf("argument %s must be a single character string", 
        sQuote(args))
    if (is.logical(verbose)) 
        verbose <- 2 * as.integer(verbose)
    fuzzy <- agrep
    if (!missing(pattern)) {
        if (!is.character(pattern) || (length(pattern) > 1L)) 
            stop(.wrong_args("pattern"), domain = NA)
        i <- pmatch(fields, hsearch_db_fields)
        if (anyNA(i)) 
            stop("incorrect field specification")
        else fields <- hsearch_db_fields[i]
    }
    else if (!missing(apropos)) {
        if (!is.character(apropos) || (length(apropos) > 1L)) 
            stop(.wrong_args("apropos"), domain = NA)
        else {
            pattern <- apropos
            fields <- c("alias", "title")
        }
    }
    else if (!missing(keyword)) {
        if (!is.character(keyword) || (length(keyword) > 1L)) 
            stop(.wrong_args("keyword"), domain = NA)
        else {
            pattern <- keyword
            fields <- "keyword"
            if (is.null(fuzzy)) 
                fuzzy <- FALSE
        }
    }
    else if (!missing(whatis)) {
        if (!is.character(whatis) || (length(whatis) > 1)) 
            stop(.wrong_args("whatis"), domain = NA)
        else {
            pattern <- whatis
            fields <- "alias"
        }
    }
    else {
        stop("do not know what to search")
    }
    if (!missing(help.db)) 
        warning("argument 'help.db' is deprecated")
    i <- pmatch(types, hsearch_db_types)
    if (anyNA(i)) 
        stop("incorrect type specification")
    else types <- hsearch_db_types[i]
    db <- hsearch_db(package, lib.loc, types, verbose, rebuild, 
        use_UTF8)
    lib.loc <- attr(db, "LibPaths")
    if (!identical(sort(types), sort(attr(db, "Types")))) {
        db$Base <- db$Base[!is.na(match(db$Base$Type, types)), 
            ]
        db[-1L] <- lapply(db[-1L], function(e) {
            e[!is.na(match(e$ID, db$Base$ID)), ]
        })
    }
    if (!is.null(package)) {
        pos_in_hsearch_db <- match(package, unique(db$Base[, 
            "Package"]), nomatch = 0L)
        if (any(pos_in_hsearch_db) == 0L) 
            stop(gettextf("no information in the database for package %s: need 'rebuild = TRUE'?", 
                sQuote(package[pos_in_hsearch_db == 0][1L])), 
                domain = NA)
        db[] <- lapply(db, function(e) {
            e[!is.na(match(e$Package, package)), ]
        })
    }
    if (verbose >= 2L) {
        message("Database of ", NROW(db$Base), " help objects (", 
            NROW(db$Aliases), " aliases, ", NROW(db$Concepts), 
            " concepts, ", NROW(db$Keywords), " keywords)", domain = NA)
        flush.console()
    }
    if (!length(db$Base)) 
        return(invisible())
    if (is.null(fuzzy) || is.na(fuzzy)) 
        fuzzy <- (grepl("^([[:alnum:]]|[[:space:]]|-)+$", pattern) && 
            (nchar(pattern, type = "c") > 4L))
    if (is.logical(fuzzy)) {
        if (fuzzy) 
            max.distance <- 0.1
    }
    else if (is.numeric(fuzzy) || is.list(fuzzy)) {
        max.distance <- fuzzy
        fuzzy <- TRUE
    }
    else stop("incorrect 'agrep' specification")
    dbBase <- db$Base
    search_fun <- if (fuzzy) {
        function(x) {
            agrep(pattern, x, ignore.case = ignore.case, max.distance = max.distance)
        }
    }
    else {
        function(x) {
            grep(pattern, x, ignore.case = ignore.case, perl = use_UTF8)
        }
    }
    search_db_results <- function(p, f, e) data.frame(Position = p, 
        Field = f, Entry = e, stringsAsFactors = FALSE)
    search_db_field <- function(field) {
        switch(field, alias = {
            aliases <- db$Aliases$Alias
            matched <- search_fun(aliases)
            search_db_results(match(db$Aliases$ID[matched], dbBase$ID), 
                rep.int(field, length(matched)), aliases[matched])
        }, concept = {
            concepts <- db$Concepts$Concept
            matched <- search_fun(concepts)
            search_db_results(match(db$Concepts$ID[matched], 
                dbBase$ID), rep.int(field, length(matched)), 
                concepts[matched])
        }, keyword = {
            keywords <- db$Keywords$Keyword
            matched <- search_fun(keywords)
            search_db_results(match(db$Keywords$ID[matched], 
                dbBase$ID), rep.int(field, length(matched)), 
                keywords[matched])
        }, name = {
            matched <- search_fun(dbBase$Name)
            search_db_results(matched, rep.int("Name", length(matched)), 
                dbBase$Name[matched])
        }, title = {
            matched <- search_fun(dbBase$Title)
            search_db_results(matched, rep.int("Title", length(matched)), 
                dbBase$Title[matched])
        })
    }
    matches <- NULL
    for (f in fields) matches <- rbind(matches, search_db_field(f))
    matches <- matches[order(matches$Position), ]
    db <- cbind(dbBase[matches$Position, c("Topic", "Title", 
        "Name", "ID", "Package", "LibPath", "Type"), drop = FALSE], 
        matches[c("Field", "Entry")])
    rownames(db) <- NULL
    if (verbose >= 2L) {
        n_of_objects_matched <- length(unique(db[, "ID"]))
        message(sprintf(ngettext(n_of_objects_matched, "matched %d object.", 
            "matched %d objects."), n_of_objects_matched), domain = NA)
        flush.console()
    }
    y <- list(pattern = pattern, fields = fields, type = if (fuzzy) "fuzzy" else "regexp", 
        agrep = agrep, ignore.case = ignore.case, types = types, 
        package = package, lib.loc = lib.loc, matches = db)
    class(y) <- "hsearch"
    y
}


package.skeleton <- function (name = "anRpackage", list = character(), environment = .GlobalEnv, 
    path = ".", force = FALSE, code_files = character()) 
{
    safe.dir.create <- function(path) {
        if (!dir.exists(path) && !dir.create(path)) 
            stop(gettextf("cannot create directory '%s'", path), 
                domain = NA)
    }
    if (!is.character(code_files)) 
        stop("'code_files' must be a character vector")
    use_code_files <- length(code_files) > 0L
    envIsMissing <- missing(environment)
    if (missing(list)) {
        if (use_code_files) {
            environment <- new.env(hash = TRUE, parent = globalenv())
            methods::setPackageName(name, environment)
            for (cf in code_files) sys.source(cf, envir = environment)
        }
        list <- ls(environment, all.names = TRUE)
    }
    if (!is.character(list)) 
        stop("'list' must be a character vector naming R objects")
    if (use_code_files || !envIsMissing) {
        classesList <- methods::getClasses(environment)
        classes0 <- .fixPackageFileNames(classesList)
        names(classes0) <- classesList
        methodsList <- methods::getGenerics(environment)
        methods0 <- .fixPackageFileNames(methodsList)
        names(methods0) <- methodsList
    }
    else {
        classesList <- methodsList <- character()
    }
    usingS4 <- length(classesList) > 0L || length(methodsList) > 
        0L
    curLocale <- Sys.getlocale("LC_CTYPE")
    on.exit(Sys.setlocale("LC_CTYPE", curLocale), add = TRUE)
    if (Sys.setlocale("LC_CTYPE", "C") != "C") 
        warning("cannot turn off locale-specific chars via LC_CTYPE", 
            domain = NA)
    have <- vapply(list, exists, NA, envir = environment)
    if (any(!have)) 
        warning(sprintf(ngettext(sum(!have), "object '%s' not found", 
            "objects '%s' not found"), paste(sQuote(list[!have]), 
            collapse = ", ")), domain = NA)
    list <- list[have]
    if (!length(list)) 
        stop("no R objects specified or available")
    message("Creating directories ...", domain = NA)
    dir <- file.path(path, name)
    if (file.exists(dir) && !force) 
        stop(gettextf("directory '%s' already exists", dir), 
            domain = NA)
    safe.dir.create(dir)
    safe.dir.create(code_dir <- file.path(dir, "R"))
    safe.dir.create(docs_dir <- file.path(dir, "man"))
    safe.dir.create(data_dir <- file.path(dir, "data"))
    message("Creating DESCRIPTION ...", domain = NA)
    description <- file(file.path(dir, "DESCRIPTION"), "wt")
    cat("Package: ", name, "\n", "Type: Package\n", "Title: What the package does (short line)\n", 
        "Version: 1.0\n", "Date: ", format(Sys.time(), format = "%Y-%m-%d"), 
        "\n", "Author: Who wrote it\n", "Maintainer: Who to complain to <yourfault@somewhere.net>\n", 
        "Description: More about what it does (maybe more than one line)\n", 
        "License: What license is it under?\n", if (usingS4) 
            "Depends: methods\n", file = description, sep = "")
    close(description)
    message("Creating NAMESPACE ...", domain = NA)
    out <- file(file.path(dir, "NAMESPACE"), "wt")
    writeLines("exportPattern(\"^[[:alpha:]]+\")", out)
    if (length(methodsList)) {
        cat("exportMethods(\n    ", file = out)
        cat(paste0("\"", methodsList, "\"", collapse = ",\n    "), 
            "\n)\n", file = out)
    }
    if (length(classesList)) {
        cat("exportClasses(\n    ", file = out)
        cat(paste0("\"", classesList, "\"", collapse = ",\n     "), 
            "\n)\n", file = out)
    }
    close(out)
    message("Creating Read-and-delete-me ...", domain = NA)
    out <- file(file.path(dir, "Read-and-delete-me"), "wt")
    msg <- c("* Edit the help file skeletons in 'man', possibly combining help files for multiple functions.", 
        "* Edit the exports in 'NAMESPACE', and add necessary imports.", 
        "* Put any C/C++/Fortran code in 'src'.", "* If you have compiled code, add a useDynLib() directive to 'NAMESPACE'.", 
        "* Run R CMD build to build the package tarball.", "* Run R CMD check to check the package tarball.", 
        "", "Read \"Writing R Extensions\" for more information.")
    writeLines(strwrap(msg, exdent = 2), out)
    close(out)
    internalObjInds <- grep("^\\.", list)
    internalObjs <- list[internalObjInds]
    if (length(internalObjInds)) 
        list <- list[-internalObjInds]
    list0 <- .fixPackageFileNames(list)
    names(list0) <- list
    if (!use_code_files) {
        message("Saving functions and data ...", domain = NA)
        if (length(internalObjInds)) 
            dump(internalObjs, file = file.path(code_dir, sprintf("%s-internal.R", 
                name)), envir = environment)
        for (item in list) {
            objItem <- get(item, envir = environment)
            if (is.function(objItem)) {
                if (isS4(objItem)) 
                  stop(gettextf("generic functions and other S4 objects (e.g., '%s') cannot be dumped; use the 'code_files' argument", 
                    item), domain = NA)
                dump(item, file = file.path(code_dir, sprintf("%s.R", 
                  list0[item])), envir = environment)
            }
            else try(save(list = item, envir = environment, file = file.path(data_dir, 
                sprintf("%s.rda", item))))
        }
    }
    else {
        message("Copying code files ...", domain = NA)
        file.copy(code_files, code_dir)
        R_files <- tools::list_files_with_type(code_dir, "code", 
            full.names = FALSE, OS_subdirs = "")
        code_files <- basename(code_files)
        wrong <- code_files[is.na(match(code_files, R_files))]
        if (length(wrong)) {
            warning("Invalid file name(s) for R code in ", code_dir, 
                ":\n", strwrap(paste(sQuote(wrong), collapse = ", "), 
                  indent = 2), "\n are now renamed to 'z<name>.R'", 
                domain = NA)
            file.rename(from = file.path(code_dir, wrong), to = file.path(code_dir, 
                paste0("z", sub("(\\.[^.]*)?$", ".R", wrong))))
        }
    }
    message("Making help files ...", domain = NA)
    yy <- try(suppressMessages({
        promptPackage(name, filename = file.path(docs_dir, sprintf("%s-package.Rd", 
            name)), lib.loc = path)
        sapply(list, function(item) {
            prompt(get(item, envir = environment), name = item, 
                filename = file.path(docs_dir, sprintf("%s.Rd", 
                  list0[item])))
        })
        sapply(classesList, function(item) {
            methods::promptClass(item, filename = file.path(docs_dir, 
                sprintf("%s-class.Rd", classes0[item])), where = environment)
        })
        sapply(methodsList, function(item) {
            methods::promptMethods(item, filename = file.path(docs_dir, 
                sprintf("%s-methods.Rd", methods0[item])), methods::findMethods(item, 
                where = environment))
        })
    }))
    for (item in methodsList) {
        if (exists(item, envir = environment, inherits = FALSE)) {
            ff <- get(item, envir = environment)
            if (methods::is(ff, "genericFunction") && !identical(ff@package, 
                name)) 
                file.remove(file.path(docs_dir, sprintf("%s.Rd", 
                  list0[item])))
        }
    }
    if (inherits(yy, "try-error")) 
        stop(yy)
    if (length(list.files(code_dir)) == 0L) 
        unlink(code_dir, recursive = TRUE)
    if (length(list.files(data_dir)) == 0L) 
        unlink(data_dir, recursive = TRUE)
    message("Done.", domain = NA)
    message(sprintf("Further steps are described in '%s'.", file.path(dir, 
        "Read-and-delete-me")), domain = NA)
}


summaryRprof <- function (filename = "Rprof.out", chunksize = 5000, memory = c("none", 
    "both", "tseries", "stats"), lines = c("hide", "show", "both"), 
    index = 2, diff = TRUE, exclude = NULL, basenames = 1) 
{
    con <- file(filename, "rt")
    on.exit(close(con))
    firstline <- readLines(con, n = 1L)
    if (!length(firstline)) 
        stop(gettextf("no lines found in %s", sQuote(filename)), 
            domain = NA)
    sample.interval <- as.numeric(strsplit(firstline, "=")[[1L]][2L])/1e+06
    memory.profiling <- substr(firstline, 1L, 6L) == "memory"
    line.profiling <- grepl("line profiling", firstline)
    if (line.profiling) 
        filenames <- character(0)
    memory <- match.arg(memory)
    if (memory != "none" && !memory.profiling) 
        stop("profile does not contain memory information")
    if (memory == "tseries") 
        return(Rprof_memory_summary(filename = con, chunksize = chunksize, 
            label = index, diff = diff, exclude = exclude, sample.interval = sample.interval))
    else if (memory == "stats") 
        return(Rprof_memory_summary(filename = con, chunksize = chunksize, 
            aggregate = index, diff = diff, exclude = exclude, 
            sample.interval = sample.interval))
    lines <- match.arg(lines)
    if (lines != "hide" && !line.profiling) 
        stop("profile does not contain line information")
    fnames <- NULL
    ucounts <- NULL
    fcounts <- NULL
    memcounts <- NULL
    umem <- NULL
    repeat ({
        chunk <- readLines(con, n = chunksize)
        if (line.profiling) {
            filenamelines <- grep("^#File [0-9]+: ", chunk)
            if (length(filenamelines)) {
                fnum <- as.integer(sub("^#File ([0-9]+): .*", 
                  "\\1", chunk[filenamelines]))
                filenames[fnum] <- sub("^#File [0-9]+: ", "", 
                  chunk[filenamelines])
                if (basenames) {
                  dirnames <- dirname(filenames[fnum])
                  filenames[fnum] <- basename(filenames[fnum])
                  for (i in seq_len(basenames - 1)) {
                    tail <- basename(dirnames)
                    filenames[fnum] <- ifelse(tail == ".", filenames[fnum], 
                      paste0(tail, "/", filenames[fnum]))
                    parent <- dirname(dirnames)
                    dirnames <- ifelse(dirnames == parent, ".", 
                      parent)
                  }
                }
                chunk <- chunk[-filenamelines]
            }
        }
        if (length(chunk) == 0L) 
            break
        if (memory.profiling) {
            memprefix <- attr(regexpr(":[0-9]+:[0-9]+:[0-9]+:[0-9]+:", 
                chunk), "match.length")
            if (memory == "both") {
                memstuff <- substr(chunk, 2L, memprefix - 1L)
                memcounts <- pmax(apply(sapply(strsplit(memstuff, 
                  ":"), as.numeric), 1, diff), 0)
                if (!is.matrix(memcounts)) 
                  memcounts <- matrix(memcounts, nrow = 1)
                memcounts <- c(0, rowSums(cbind(memcounts[, 1L:2L, 
                  drop = FALSE] * 8, memcounts[, 3L, drop = FALSE])))
                rm(memstuff)
            }
            chunk <- substr(chunk, memprefix + 1L, nchar(chunk, 
                "c"))
            if (any((nc <- nchar(chunk, "c")) == 0L)) {
                chunk <- chunk[nc > 0L]
                memcounts <- memcounts[nc > 0L]
            }
        }
        chunk <- strsplit(chunk, " ")
        if (line.profiling) 
            chunk <- lapply(chunk, function(x) {
                locations <- !startsWith(x, "\"")
                if (lines != "hide") {
                  fnum <- sub("#.*", "", x[locations])
                  lnum <- sub(".*#", "", x[locations])
                  x[locations] <- paste0(filenames[as.integer(fnum)], 
                    "#", lnum)
                }
                switch(lines, hide = x <- x[!locations], show = x <- x[locations])
                if (length(x)) 
                  x
                else "<no location>"
            })
        newfirsts <- sapply(chunk, "[[", 1L)
        newuniques <- lapply(chunk, unique)
        ulen <- lengths(newuniques)
        newuniques <- unlist(newuniques)
        new.utable <- table(newuniques)
        new.ftable <- table(factor(newfirsts, levels = names(new.utable)))
        if (memory == "both") 
            new.umem <- rowsum(memcounts[rep.int(seq_along(memcounts), 
                ulen)], newuniques)
        fcounts <- rowsum(c(as.vector(new.ftable), fcounts), 
            c(names(new.ftable), fnames))
        ucounts <- rowsum(c(as.vector(new.utable), ucounts), 
            c(names(new.utable), fnames))
        if (memory == "both") 
            umem <- rowsum(c(new.umem, umem), c(names(new.utable), 
                fnames))
        fnames <- sort(unique(c(fnames, names(new.utable))))
    })
    firstnum <- fcounts * sample.interval
    uniquenum <- ucounts * sample.interval
    index1 <- order(-firstnum, -uniquenum)
    index2 <- order(-uniquenum, -firstnum)
    if (lines == "show") {
        filename <- sub("#.*$", "", fnames)
        linenum <- rep(0, length(filename))
        hasline <- filename != fnames
        linenum[hasline] <- as.numeric(sub("^.*#", "", fnames[hasline]))
        index3 <- order(filename, linenum)
    }
    firstpct <- round(100 * firstnum/sum(firstnum), 2)
    uniquepct <- round(100 * uniquenum/sum(firstnum), 2)
    digits <- ifelse(sample.interval < 0.01, 3L, 2L)
    firstnum <- round(firstnum, digits)
    uniquenum <- round(uniquenum, digits)
    if (memory == "both") 
        memtotal <- round(umem/1048576, 1)
    rval <- data.frame(firstnum, firstpct, uniquenum, uniquepct)
    names(rval) <- c("self.time", "self.pct", "total.time", "total.pct")
    rownames(rval) <- fnames
    if (memory == "both") 
        rval$mem.total <- memtotal
    by.self <- rval[index1, ]
    by.self <- by.self[by.self[, 1L] > 0, ]
    by.total <- rval[index2, c(3L, 4L, if (memory == "both") 5L, 
        1L, 2L)]
    result <- list(by.self = by.self, by.total = by.total)
    if (lines == "show") 
        result <- c(result, list(by.line = rval[index3, ]))
    c(result, sample.interval = sample.interval, sampling.time = sum(fcounts) * 
        sample.interval)
}


setTxtProgressBar <- function (pb, value, title = NULL, label = NULL) 
{
    if (!inherits(pb, "txtProgressBar")) 
        stop(gettextf("'pb' is not from class %s", dQuote("txtProgressBar")), 
            domain = NA)
    oldval <- pb$getVal()
    pb$up(value)
    invisible(oldval)
}


timestamp <- function (stamp = date(), prefix = "##------ ", suffix = " ------##", 
    quiet = FALSE) 
{
    stamp <- paste0(prefix, stamp, suffix)
    .External2(C_addhistory, stamp)
    if (!quiet) 
        cat(stamp, sep = "\n")
    invisible(stamp)
}


getSrcref <- function (x) 
{
    if (inherits(x, "srcref")) 
        return(x)
    if (!is.null(srcref <- attr(x, "srcref"))) 
        return(srcref)
    if (is.function(x) && !is.null(srcref <- getSrcref(body(x)))) 
        return(srcref)
    if (methods::is(x, "MethodDefinition")) 
        return(getSrcref(unclass(methods::unRematchDefinition(x))))
    NULL
}


stack <- function (x, ...) 
UseMethod("stack")


capture.output <- function (..., file = NULL, append = FALSE, type = c("output", 
    "message"), split = FALSE) 
{
    args <- substitute(list(...))[-1L]
    type <- match.arg(type)
    rval <- NULL
    closeit <- TRUE
    if (is.null(file)) 
        file <- textConnection("rval", "w", local = TRUE)
    else if (is.character(file)) 
        file <- file(file, if (append) 
            "a"
        else "w")
    else if (inherits(file, "connection")) {
        if (!isOpen(file)) 
            open(file, if (append) 
                "a"
            else "w")
        else closeit <- FALSE
    }
    else stop("'file' must be NULL, a character string or a connection")
    sink(file, type = type, split = split)
    on.exit({
        sink(type = type, split = split)
        if (closeit) close(file)
    })
    pf <- parent.frame()
    evalVis <- function(expr) withVisible(eval(expr, pf))
    for (i in seq_along(args)) {
        expr <- args[[i]]
        tmp <- switch(mode(expr), expression = lapply(expr, evalVis), 
            call = , name = list(evalVis(expr)), stop("bad argument"))
        for (item in tmp) if (item$visible) 
            print(item$value)
    }
    on.exit()
    sink(type = type, split = split)
    if (closeit) 
        close(file)
    if (is.null(rval)) 
        invisible(NULL)
    else rval
}


bug.report <- function (subject = "", address, file = "R.bug.report", package = NULL, 
    lib.loc = NULL, ...) 
{
    baseR <- function() {
        writeLines(c("  Bug reports on R and the base packages need to be submitted", 
            "  to the tracker at <https://bugs.R-project.org/>.", 
            "", "  We will now try to open that website in a browser"))
        flush.console()
        Sys.sleep(2)
        browseURL("https://bugs.r-project.org/bugzilla3/index.cgi")
    }
    if (is.null(package)) 
        return(baseR())
    DESC <- packageDescription(package, lib.loc)
    if (!inherits(DESC, "packageDescription")) 
        stop(gettextf("Package %s: DESCRIPTION file not found", 
            sQuote(package)), domain = NA)
    info <- paste0(c("Package", " Version", " Maintainer", " Built"), 
        ": ", c(DESC$Package, DESC$Version, DESC$Maintainer, 
            DESC$Built))
    info <- c(info, "", bug.report.info())
    if (identical(DESC$Priority, "base")) 
        return(baseR())
    findEmail2 <- function(x) {
        x <- paste(x, collapse = " ")
        if (grepl("mailto:", x)) 
            sub(".*mailto:([^ ]+).*", "\\1", x)
        else if (grepl("[^<]*<([^>]+)", x)) 
            sub("[^<]*<([^>]+)>.*", "\\1", x)
        else if (grepl("(^|.* )[^ ]+@[[:alnum:]._]+", x)) 
            sub("(^|.* )([^ ]+@[[:alnum:]._]+).*", "\\2", x)
        else NA_character_
    }
    BR <- DESC$BugReports
    if (!is.null(BR) && nzchar(BR)) {
        BR <- trimws(BR)
        if (grepl("^https?://", BR)) {
            writeLines(info)
            cat("\nThis package has a bug submission web page, which we will now attempt\n", 
                "to open.  The information above may be useful in your report.\n", 
                "If the web page does not work, you should send email to the maintainer,\n", 
                DESC$Maintainer, ".\n", sep = "")
            flush.console()
            Sys.sleep(2)
            browseURL(BR)
            return(invisible())
        }
        else {
            cat("This package has a BugReports field which is not the URL of a web page:\n\n", 
                "  BugReports: ", BR, "\n\n", sep = "")
            em <- findEmail2(BR)
            if (!is.na(em)) {
                cat("It appears to contain an email address, so we will try that.\n\n")
                address <- em
            }
            else cat("We will ignore it and email the maintainer.\n\n")
            flush.console()
            Sys.sleep(2)
        }
    }
    CT <- DESC$Contact
    if (!is.null(CT) && nzchar(CT)) {
        cat("This package has a Contact field:\n\n", "  Contact: ", 
            CT, "\n\n", sep = "")
        em <- findEmail2(CT)
        if (!is.na(em)) {
            cat("That appears to contain an email address, so we will try that\n")
            address <- em
        }
        else cat("We cannot make sense of that, so will ignore it.\n\n")
        flush.console()
        Sys.sleep(2)
    }
    if (missing(address)) {
        findEmail <- function(x) {
            x <- paste(x, collapse = " ")
            sub("[^<]*<([^>]+)>.*", "\\1", x)
        }
        address <- findEmail(DESC$Maintainer)
    }
    create.post(instructions = c("", "<<insert bug report here>>", 
        rep("", 3)), description = "bug report", subject = subject, 
        address = address, filename = file, info = info, ...)
}


rc.settings <- function (ops, ns, args, func, ipck, S3, data, help, argdb, fuzzy, 
    quotes, files) 
{
    if (length(match.call()) == 1) 
        return(unlist(.CompletionEnv[["settings"]]))
    checkAndChange <- function(what, value) {
        if ((length(value) == 1L) && is.logical(value) && !is.na(value)) 
            .CompletionEnv$settings[[what]] <- value
    }
    if (!missing(ops)) 
        checkAndChange("ops", ops)
    if (!missing(ns)) 
        checkAndChange("ns", ns)
    if (!missing(args)) 
        checkAndChange("args", args)
    if (!missing(func)) 
        checkAndChange("func", func)
    if (!missing(ipck)) 
        checkAndChange("ipck", ipck)
    if (!missing(S3)) 
        checkAndChange("S3", S3)
    if (!missing(data)) 
        checkAndChange("data", data)
    if (!missing(help)) 
        checkAndChange("help", help)
    if (!missing(argdb)) 
        checkAndChange("argdb", argdb)
    if (!missing(files)) 
        checkAndChange("files", files)
    if (!missing(quotes)) 
        checkAndChange("quotes", quotes)
    if (!missing(fuzzy)) 
        checkAndChange("fuzzy", fuzzy)
    invisible()
}


fileSnapshot <- function (path = ".", file.info = TRUE, timestamp = NULL, md5sum = FALSE, 
    digest = NULL, full.names = length(path) > 1, ...) 
{
    if (length(path) > 1 && !full.names) 
        stop("'full.names' must be TRUE for multiple paths.")
    if (length(timestamp) == 1) 
        file.create(timestamp)
    path <- normalizePath(path)
    args <- list(...)
    fullnames <- names <- character(0)
    for (i in seq_along(path)) {
        newnames <- do.call(list.files, c(path = path[i], full.names = full.names, 
            args))
        names <- c(names, newnames)
        if (full.names) 
            fullnames <- names
        else fullnames <- c(fullnames, file.path(path[i], newnames))
    }
    if (file.info) {
        info <- file.info(fullnames)
        if (!full.names) 
            rownames(info) <- names
    }
    else info <- data.frame(row.names = names)
    if (md5sum) 
        info <- data.frame(info, md5sum = suppressWarnings(tools::md5sum(fullnames)), 
            stringsAsFactors = FALSE)
    if (!is.null(digest)) 
        info <- data.frame(info, digest = digest(fullnames), 
            stringsAsFactors = FALSE)
    structure(list(info = info, path = path, timestamp = timestamp, 
        file.info = file.info, md5sum = md5sum, digest = digest, 
        full.names = full.names, args = args), class = "fileSnapshot")
}


hsearch_db_keywords <- function (db = hsearch_db()) 
{
    pos <- match(db$Keywords[, "ID"], db$Base[, "ID"])
    entries <- split(as.data.frame(db$Base[pos, ], stringsAsFactors = FALSE), 
        db$Keywords[, "Keyword"])
    enums <- sapply(entries, NROW)
    pnums <- sapply(entries, function(e) length(unique(e$Package)))
    standard <- .get_standard_Rd_keywords_with_descriptions()
    concepts <- standard$Descriptions[match(names(entries), standard$Keywords)]
    pos <- order(enums, pnums, decreasing = TRUE)
    data.frame(Keyword = names(entries)[pos], Concept = concepts[pos], 
        Frequency = enums[pos], Packages = pnums[pos], stringsAsFactors = FALSE, 
        row.names = NULL)
}


rtags <- function (path = ".", pattern = "\\.[RrSs]$", recursive = FALSE, 
    src = list.files(path = path, pattern = pattern, full.names = TRUE, 
        recursive = recursive), keep.re = NULL, ofile = "", append = FALSE, 
    verbose = getOption("verbose")) 
{
    if (nzchar(ofile) && !append) {
        if (!file.create(ofile, showWarnings = FALSE)) 
            stop(gettextf("Could not create file %s, aborting", 
                ofile), domain = NA)
    }
    if (!missing(keep.re)) 
        src <- grep(keep.re, src, value = TRUE)
    for (s in src) {
        if (verbose) 
            message(gettextf("Processing file %s", s), domain = NA)
        tryCatch(rtags.file(s, ofile = ofile, append = TRUE), 
            error = function(e) NULL)
    }
    invisible()
}


update.packageStatus <- function (object, lib.loc = levels(object$inst$LibPath), repositories = levels(object$avail$Repository), 
    ...) 
{
    packageStatus(lib.loc = lib.loc, repositories = repositories)
}


browseVignettes <- function (package = NULL, lib.loc = NULL, all = TRUE) 
{
    vinfo <- tools::getVignetteInfo(package, lib.loc, all)
    pkgs <- unique(vinfo[, "Package"])
    db <- lapply(pkgs, function(p) vinfo[vinfo[, "Package"] == 
        p, , drop = FALSE])
    names(db) <- pkgs
    attr(db, "call") <- sys.call()
    attr(db, "footer") <- if (all) 
        ""
    else sprintf(gettext("Use <code> %s </code> \n to list the vignettes in all <strong>available</strong> packages."), 
        "browseVignettes(all = TRUE)")
    class(db) <- "browseVignettes"
    return(db)
}


contrib.url <- function (repos, type = getOption("pkgType")) 
{
    type <- resolvePkgType(type)
    if (is.null(repos)) 
        return(NULL)
    if ("@CRAN@" %in% repos && interactive()) {
        cat(gettext("--- Please select a CRAN mirror for use in this session ---"), 
            "\n", sep = "")
        flush.console()
        chooseCRANmirror()
        m <- match("@CRAN@", repos)
        nm <- names(repos)
        repos[m] <- getOption("repos")["CRAN"]
        if (is.null(nm)) 
            nm <- rep("", length(repos))
        nm[m] <- "CRAN"
        names(repos) <- nm
    }
    if ("@CRAN@" %in% repos) 
        stop("trying to use CRAN without setting a mirror")
    ver <- paste(R.version$major, strsplit(R.version$minor, ".", 
        fixed = TRUE)[[1L]][1L], sep = ".")
    mac.path <- "macosx"
    if (substr(type, 1L, 11L) == "mac.binary.") {
        mac.path <- paste(mac.path, substring(type, 12L), sep = "/")
        type <- "mac.binary"
    }
    res <- switch(type, source = paste(gsub("/$", "", repos), 
        "src", "contrib", sep = "/"), mac.binary = paste(gsub("/$", 
        "", repos), "bin", mac.path, "contrib", ver, sep = "/"), 
        win.binary = paste(gsub("/$", "", repos), "bin", "windows", 
            "contrib", ver, sep = "/"))
    res
}


strOptions <- function (strict.width = "no", digits.d = 3, vec.len = 4, drop.deparse.attr = TRUE, 
    formatNum = function(x, ...) format(x, trim = TRUE, drop0trailing = TRUE, 
        ...)) 
list(strict.width = strict.width, digits.d = digits.d, vec.len = vec.len, 
    drop.deparse.attr = drop.deparse.attr, formatNum = match.fun(formatNum))


packageDescription <- function (pkg, lib.loc = NULL, fields = NULL, drop = TRUE, encoding = "") 
{
    retval <- list()
    if (!is.null(fields)) {
        fields <- as.character(fields)
        retval[fields] <- NA
    }
    pkgpath <- if (is.null(lib.loc)) {
        if (pkg == "base") 
            file.path(.Library, "base")
        else if (isNamespaceLoaded(pkg)) 
            getNamespaceInfo(pkg, "path")
        else if ((envname <- paste0("package:", pkg)) %in% search()) {
            attr(as.environment(envname), "path")
        }
    }
    if (is.null(pkgpath)) 
        pkgpath <- ""
    if (pkgpath == "") {
        libs <- if (is.null(lib.loc)) 
            .libPaths()
        else lib.loc
        for (lib in libs) if (file.access(file.path(lib, pkg), 
            5) == 0L) {
            pkgpath <- file.path(lib, pkg)
            break
        }
    }
    if (pkgpath == "") {
        warning(gettextf("no package '%s' was found", pkg), domain = NA)
        return(NA)
    }
    if (file.exists(file <- file.path(pkgpath, "Meta", "package.rds"))) {
        desc <- readRDS(file)$DESCRIPTION
        if (length(desc) < 1) 
            stop(gettextf("metadata of package '%s' is corrupt", 
                pkg), domain = NA)
        desc <- as.list(desc)
    }
    else if (file.exists(file <- file.path(pkgpath, "DESCRIPTION"))) {
        dcf <- read.dcf(file = file)
        if (NROW(dcf) < 1L) 
            stop(gettextf("DESCRIPTION file of package '%s' is corrupt", 
                pkg), domain = NA)
        desc <- as.list(dcf[1, ])
    }
    else file <- ""
    if (nzchar(file)) {
        enc <- desc[["Encoding"]]
        if (!is.null(enc) && !is.na(encoding)) {
            if (missing(encoding) && Sys.getlocale("LC_CTYPE") == 
                "C") 
                encoding <- "ASCII//TRANSLIT"
            newdesc <- try(lapply(desc, iconv, from = enc, to = encoding))
            if (!inherits(newdesc, "try-error")) 
                desc <- newdesc
            else warning("'DESCRIPTION' file has an 'Encoding' field and re-encoding is not possible", 
                call. = FALSE)
        }
        if (!is.null(fields)) {
            ok <- names(desc) %in% fields
            retval[names(desc)[ok]] <- desc[ok]
        }
        else retval[names(desc)] <- desc
    }
    if ((file == "") || (length(retval) == 0)) {
        warning(gettextf("DESCRIPTION file of package '%s' is missing or broken", 
            pkg), domain = NA)
        return(NA)
    }
    if (drop & length(fields) == 1L) 
        return(retval[[1L]])
    class(retval) <- "packageDescription"
    if (!is.null(fields)) 
        attr(retval, "fields") <- fields
    attr(retval, "file") <- file
    retval
}


RweaveLatex <- function () 
{
    list(setup = RweaveLatexSetup, runcode = RweaveLatexRuncode, 
        writedoc = RweaveLatexWritedoc, finish = RweaveLatexFinish, 
        checkopts = RweaveLatexOptions)
}


SweaveSyntaxNoweb <- structure(list(doc = "^@", code = "^<<(.*)>>=.*", coderef = "^<<(.*)>>.*", 
    docopt = "^[[:space:]]*\\\\SweaveOpts\\{([^}]*)\\}", docexpr = "\\\\Sexpr\\{([^}]*)\\}", 
    extension = "\\.[rsRS]?nw$", syntaxname = "^[[:space:]]*\\\\SweaveSyntax\\{([^}]*)\\}", 
    input = "^[[:space:]]*\\\\SweaveInput\\{([^}]*)\\}", trans = structure(list(
        doc = "@", code = "<<\\1>>=", coderef = "<<\\1>>", docopt = "\\\\SweaveOpts{\\1}", 
        docexpr = "\\\\Sexpr{\\1}", extension = ".Snw", syntaxname = "\\\\SweaveSyntax{SweaveSyntaxNoweb}", 
        input = "\\\\SweaveInput{\\1}"), .Names = c("doc", "code", 
    "coderef", "docopt", "docexpr", "extension", "syntaxname", 
    "input"))), .Names = c("doc", "code", "coderef", "docopt", 
"docexpr", "extension", "syntaxname", "input", "trans"), class = "SweaveSyntax")


flush.console <- function () 
invisible(.Call(C_flushconsole))


Rprofmem <- function (filename = "Rprofmem.out", append = FALSE, threshold = 0) 
{
    if (is.null(filename)) 
        filename <- ""
    invisible(.External(C_Rprofmem, filename, append, as.double(threshold)))
}


compareVersion <- function (a, b) 
{
    if (is.na(a)) 
        return(-1L)
    if (is.na(b)) 
        return(1L)
    a <- as.integer(strsplit(a, "[.-]")[[1L]])
    b <- as.integer(strsplit(b, "[.-]")[[1L]])
    for (k in seq_along(a)) if (k <= length(b)) {
        if (a[k] > b[k]) 
            return(1)
        else if (a[k] < b[k]) 
            return(-1L)
    }
    else return(1L)
    if (length(b) > length(a)) 
        return(-1L)
    else return(0L)
}


as.personList <- function (x) 
UseMethod("as.personList")


getFromNamespace <- function (x, ns, pos = -1, envir = as.environment(pos)) 
{
    if (missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if (is.null(nm) || substr(nm, 1L, 8L) != "package:") 
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    }
    else ns <- asNamespace(ns)
    get(x, envir = ns, inherits = FALSE)
}


recover <- function () 
{
    if (.isMethodsDispatchOn()) {
        tState <- tracingState(FALSE)
        on.exit(tracingState(tState))
    }
    calls <- sys.calls()
    from <- 0L
    n <- length(calls)
    if (identical(sys.function(n), recover)) 
        n <- n - 1L
    for (i in rev(seq_len(n))) {
        calli <- calls[[i]]
        fname <- calli[[1L]]
        if (!is.na(match(deparse(fname)[1L], c("methods::.doTrace", 
            ".doTrace")))) {
            from <- i - 1L
            break
        }
    }
    if (from == 0L) 
        for (i in rev(seq_len(n))) {
            calli <- calls[[i]]
            fname <- calli[[1L]]
            if (!is.name(fname) || is.na(match(as.character(fname), 
                c("recover", "stop", "Stop")))) {
                from <- i
                break
            }
        }
    if (from > 0L) {
        if (!interactive()) {
            try(dump.frames())
            cat(gettext("recover called non-interactively; frames dumped, use debugger() to view\n"))
            return(NULL)
        }
        else if (identical(getOption("show.error.messages"), 
            FALSE)) 
            return(NULL)
        calls <- limitedLabels(calls[1L:from])
        repeat {
            which <- menu(calls, title = "\nEnter a frame number, or 0 to exit  ")
            if (which) 
                eval(substitute(browser(skipCalls = skip), list(skip = 7 - 
                  which)), envir = sys.frame(which))
            else break
        }
    }
    else cat(gettext("No suitable frames for recover()\n"))
}


packageStatus <- function (lib.loc = NULL, repositories = NULL, method, type = getOption("pkgType")) 
{
    newestVersion <- function(x) {
        vers <- package_version(x)
        max <- vers[1L]
        for (i in seq_along(vers)) if (max < vers[i]) 
            max <- vers[i]
        which.max(vers == max)
    }
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    if (is.null(repositories)) 
        repositories <- contrib.url(getOption("repos"), type = type)
    char2df <- function(x) {
        y <- list()
        for (k in 1L:ncol(x)) y[[k]] <- x[, k]
        attr(y, "names") <- colnames(x)
        attr(y, "row.names") <- make.unique(y[[1L]])
        class(y) <- "data.frame"
        y
    }
    y <- char2df(installed.packages(lib.loc = lib.loc))
    y[, "Status"] <- "ok"
    z <- available.packages(repositories, method)
    ztab <- table(z[, "Package"])
    for (pkg in names(ztab)[ztab > 1]) {
        zrow <- which(z[, "Package"] == pkg)
        znewest <- newestVersion(z[zrow, "Version"])
        z <- z[-zrow[-znewest], ]
    }
    z <- cbind(z, Status = "not installed")
    z[z[, "Package"] %in% y$Package, "Status"] <- "installed"
    z <- char2df(z)
    attr(z, "row.names") <- z$Package
    for (k in 1L:nrow(y)) {
        pkg <- y[k, "Package"]
        if (pkg %in% z$Package) {
            if (package_version(y[k, "Version"]) < package_version(z[pkg, 
                "Version"])) {
                y[k, "Status"] <- "upgrade"
            }
        }
        else {
            if (!(y[k, "Priority"] %in% "base")) 
                y[k, "Status"] <- "unavailable"
        }
    }
    y$LibPath <- factor(y$LibPath, levels = lib.loc)
    y$Status <- factor(y$Status, levels = c("ok", "upgrade", 
        "unavailable"))
    z$Repository <- factor(z$Repository, levels = repositories)
    z$Status <- factor(z$Status, levels = c("installed", "not installed"))
    retval <- list(inst = y, avail = z)
    class(retval) <- "packageStatus"
    retval
}


read.csv2 <- function (file, header = TRUE, sep = ";", quote = "\"", dec = ",", 
    fill = TRUE, comment.char = "", ...) 
read.table(file = file, header = header, sep = sep, quote = quote, 
    dec = dec, fill = fill, comment.char = comment.char, ...)


demo <- function (topic, package = NULL, lib.loc = NULL, character.only = FALSE, 
    verbose = getOption("verbose"), echo = TRUE, ask = getOption("demo.ask"), 
    encoding = getOption("encoding")) 
{
    paths <- find.package(package, lib.loc, verbose = verbose)
    paths <- paths[dir.exists(file.path(paths, "demo"))]
    if (missing(topic)) {
        db <- matrix(character(), nrow = 0L, ncol = 4L)
        for (path in paths) {
            entries <- NULL
            if (file_test("-f", INDEX <- file.path(path, "Meta", 
                "demo.rds"))) {
                entries <- readRDS(INDEX)
            }
            if (NROW(entries)) {
                db <- rbind(db, cbind(basename(path), dirname(path), 
                  entries))
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")
        footer <- if (missing(package)) 
            paste0("Use ", sQuote(paste("demo(package =", ".packages(all.available = TRUE))")), 
                "\n", "to list the demos in all *available* packages.")
        else NULL
        y <- list(title = "Demos", header = NULL, results = db, 
            footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }
    if (!character.only) {
        topic <- substitute(topic)
        if (is.call(topic) && (topic[[1L]] == "::" || topic[[1L]] == 
            ":::")) {
            package <- as.character(topic[[2L]])
            topic <- as.character(topic[[3L]])
        }
        else topic <- as.character(topic)
    }
    available <- character()
    paths <- file.path(paths, "demo")
    for (p in paths) {
        files <- basename(tools::list_files_with_type(p, "demo"))
        files <- files[topic == tools::file_path_sans_ext(files)]
        if (length(files)) 
            available <- c(available, file.path(p, files))
    }
    if (length(available) == 0L) 
        stop(gettextf("No demo found for topic %s", sQuote(topic)), 
            domain = NA)
    if (length(available) > 1L) {
        available <- available[1L]
        warning(gettextf("Demo for topic %s' found more than once,\nusing the one found in %s", 
            sQuote(topic), sQuote(dirname(available[1L]))), domain = NA)
    }
    pkgpath <- dirname(dirname(available))
    if (file.exists(file <- file.path(pkgpath, "Meta", "package.rds"))) {
        desc <- readRDS(file)$DESCRIPTION
        if (length(desc) == 1L) {
            enc <- as.list(desc)[["Encoding"]]
            !if (!is.null(enc)) 
                encoding <- enc
        }
    }
    if (ask == "default") 
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)
    if (.Device != "null device") {
        oldask <- grDevices::devAskNewPage(ask = ask)
        on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
    }
    op <- options(device.ask.default = ask)
    on.exit(options(op), add = TRUE)
    if (echo) {
        cat("\n\n", "\tdemo(", topic, ")\n", "\t---- ", rep.int("~", 
            nchar(topic, type = "w")), "\n", sep = "")
        if (ask && interactive()) 
            readline("\nType  <Return>\t to start : ")
    }
    source(available, echo = echo, max.deparse.length = Inf, 
        keep.source = TRUE, encoding = encoding)
}


edit <- function (name, ...) 
UseMethod("edit")


aspell_package_R_files <- function (dir, ignore = character(), control = list(), program = NULL, 
    dictionaries = character()) 
{
    dir <- tools::file_path_as_absolute(dir)
    subdir <- file.path(dir, "R")
    files <- if (dir.exists(subdir)) 
        tools::list_files_with_type(subdir, "code", OS_subdirs = c("unix", 
            "windows"))
    else character()
    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if (is.na(encoding <- meta["Encoding"])) 
        encoding <- "unknown"
    defaults <- .aspell_package_defaults(dir, encoding)$R_files
    if (!is.null(defaults)) {
        if (!is.null(d <- defaults$ignore)) 
            ignore <- d
        if (!is.null(d <- defaults$control)) 
            control <- d
        if (!is.null(d <- defaults$program)) 
            program <- d
        if (!is.null(d <- defaults$dictionaries)) {
            dictionaries <- aspell_find_dictionaries(d, file.path(dir, 
                ".aspell"))
        }
    }
    program <- aspell_find_program(program)
    aspell(files, filter = list("R", ignore = ignore), control = control, 
        encoding = encoding, program = program, dictionaries = dictionaries)
}


isS3method <- function (method, f, class, envir = parent.frame()) 
{
    if (missing(method)) {
        method <- paste(f, class, sep = ".")
    }
    else {
        f.c <- strsplit(method, ".", fixed = TRUE)[[1]]
        nfc <- length(f.c)
        if (nfc < 2 || !is.character(f.c)) 
            return(FALSE)
        if (nfc == 2) {
            f <- f.c[[1L]]
            class <- f.c[[2L]]
        }
        else {
            for (j in 2:nfc) if (isS3method(f = paste(f.c[1:(j - 
                1)], collapse = "."), class = paste(f.c[j:nfc], 
                collapse = "."), envir = envir)) 
                return(TRUE)
            return(FALSE)
        }
    }
    if (!any(f == getKnownS3generics())) {
        if (!nzchar(f <- findGeneric(f, envir))) 
            return(FALSE)
    }
    if (!is.null(m <- get0(method, envir = envir, mode = "function"))) {
        pkg <- if (isNamespace(em <- environment(m))) 
            environmentName(em)
        else if (is.primitive(m)) 
            "base"
        return(is.na(match(method, tools::nonS3methods(pkg))))
    }
    defenv <- if (!is.na(w <- .knownS3Generics[f])) 
        asNamespace(w)
    else if (f %in% tools:::.get_internal_S3_generics()) 
        .BaseNamespaceEnv
    else {
        genfun <- get(f, mode = "function", envir = envir)
        if (.isMethodsDispatchOn() && methods::is(genfun, "genericFunction")) 
            genfun <- methods::selectMethod(genfun, "ANY")
        if (typeof(genfun) == "closure") 
            environment(genfun)
        else .BaseNamespaceEnv
    }
    S3Table <- get(".__S3MethodsTable__.", envir = defenv)
    exists(method, envir = S3Table, inherits = FALSE)
}


de <- function (..., Modes = list(), Names = NULL) 
{
    sdata <- list(...)
    snames <- as.character(substitute(list(...))[-1L])
    if (is.null(sdata)) {
        if (is.null(Names)) {
            odata <- vector("list", length = max(1, length(Modes)))
        }
        else {
            if ((length(Names) != length(Modes)) && length(Modes)) {
                warning("'modes' argument ignored")
                Modes <- list()
            }
            odata <- vector("list", length = length(Names))
            names(odata) <- Names
        }
        ncols <- rep.int(1, length(odata))
        coltypes <- rep.int(1, length(odata))
    }
    else {
        ncols <- de.ncols(sdata)
        coltypes <- ncols[, 2L]
        ncols <- ncols[, 1]
        odata <- de.setup(sdata, snames, ncols)
        if (length(Names)) 
            if (length(Names) != length(odata)) 
                warning("'names' argument ignored")
            else names(odata) <- Names
        if (length(Modes)) 
            if (length(Modes) != length(odata)) {
                warning("'modes' argument ignored")
                Modes <- list()
            }
    }
    rdata <- dataentry(odata, as.list(Modes))
    if (any(coltypes != 1L)) {
        if (length(rdata) == sum(ncols)) 
            rdata <- de.restore(rdata, ncols, coltypes, snames, 
                sdata)
        else warning("could not restore variables properly")
    }
    return(rdata)
}


RSiteSearch <- function (string, restrict = c("functions", "vignettes", "views"), 
    format = c("normal", "short"), sortby = c("score", "date:late", 
        "date:early", "subject", "subject:descending", "from", 
        "from:descending", "size", "size:descending"), matchesPerPage = 20) 
{
    string <- paste0("http://search.r-project.org/cgi-bin/namazu.cgi?query=", 
        URLencode(gsub(" ", "+", string), reserved = TRUE))
    mpp <- paste0("max=", matchesPerPage)
    format <- paste0("result=", match.arg(format))
    restrictVALS <- c("functions", "vignettes", "views")
    restr <- match.arg(restrict, choices = restrictVALS, several.ok = TRUE)
    restr <- paste(paste0("idxname=", restr), collapse = "&")
    sortby <- match.arg(sortby)
    sortby <- paste0("sort=", switch(sortby, score = , `date:late` = , 
        `date:early` = sortby, subject = "field:subject:ascending", 
        `subject:descending` = "field:subject:descending", from = "field:from:ascending", 
        `from:descending` = "field:from:descending", size = "field:size:ascending", 
        `size:descending` = "field:size:descending"))
    qstring <- paste(string, mpp, format, sortby, restr, sep = "&")
    browseURL(qstring)
    cat(gettextf("A search query has been submitted to %s", "http://search.r-project.org"), 
        "\n", sep = "")
    cat(gettext("The results page should open in your browser shortly\n"))
    invisible(qstring)
}


read.DIF <- function (file, header = FALSE, dec = ".", numerals = c("allow.loss", 
    "warn.loss", "no.loss"), row.names, col.names, as.is = !stringsAsFactors, 
    na.strings = "NA", colClasses = NA, nrows = -1, skip = 0, 
    check.names = TRUE, blank.lines.skip = TRUE, stringsAsFactors = default.stringsAsFactors(), 
    transpose = FALSE, fileEncoding = "") 
{
    if (.Platform$OS.type == "windows" && identical(file, "clipboard")) {
        if (!(5 %in% getClipboardFormats(numeric = TRUE))) 
            stop("No DIF data on clipboard")
        lines <- readClipboard(5)
    }
    else if (nzchar(fileEncoding)) {
        con <- file(file, "rt", encoding = fileEncoding)
        lines <- readLines(con)
        close(con)
    }
    else {
        lines <- readLines(file)
    }
    if (length(lines) < 1L) 
        stop("file had no lines")
    topic <- ""
    nrow <- NA
    ncol <- NA
    i <- 1L
    while (topic != "DATA") {
        topic <- lines[i]
        vnum <- lines[i + 1]
        num <- as.numeric(sub("^.*,", "", vnum))
        i <- i + 3L
        if (topic == "VECTORS") 
            if (transpose) 
                nrow <- num
            else ncol <- num
        else if (topic == "TUPLES") 
            if (transpose) 
                ncol <- num
            else nrow <- num
    }
    if (is.na(nrow) || is.na(ncol)) 
        stop("row and column counts not found")
    data <- matrix("", nrow, ncol)
    types <- matrix(NA_character_, nrow, ncol)
    row <- 0L
    while (i < length(lines)) {
        typenum <- lines[i]
        type <- as.numeric(sub(",.*$", "", typenum))
        num <- as.numeric(sub("^.*,", "", typenum))
        stringval <- lines[i + 1]
        i <- i + 2L
        if (type == -1L) {
            if (stringval == "BOT") {
                row <- row + 1L
                if (row > nrow) 
                  stop("More rows than specified in header; maybe use 'transpose=TRUE'")
                col <- 0L
            }
            else if (stringval == "EOD") 
                break
            else stop("Unrecognized special data value")
        }
        else {
            col <- col + 1L
            if (col > ncol) 
                stop("More columns than specified in header; maybe use 'transpose=TRUE'")
            if (type == 0L) {
                types[row, col] <- "numeric"
                if (stringval == "V") 
                  data[row, col] <- num
                else if (stringval == "NA") 
                  data[row, col] <- NA
                else if (stringval == "ERROR") 
                  data[row, col] <- NA
                else if (stringval == "TRUE") {
                  data[row, col] <- "TRUE"
                  types[row, col] <- "logical"
                }
                else if (stringval == "FALSE") {
                  data[row, col] <- "FALSE"
                  types[row, col] <- "logical"
                }
                else stop("Unrecognized value indicator")
            }
            else if (type == 1L) {
                types[row, col] <- "character"
                stringval <- sub("^\"", "", stringval)
                stringval <- sub("\"$", "", stringval)
                data[row, col] <- stringval
            }
        }
    }
    if (skip > 0L) 
        data <- data[-(1L:skip), , drop = FALSE]
    nlines <- nrow(data)
    if (!nlines) {
        if (missing(col.names)) 
            stop("no lines available in input")
        else {
            tmp <- vector("list", length(col.names))
            names(tmp) <- col.names
            class(tmp) <- "data.frame"
            return(tmp)
        }
    }
    first <- data[1L, ]
    if (first[1L] == "") 
        first <- first[-1L]
    cols <- ncol
    rlabp <- all(types[1L, ][-1L] == "character") && data[1L, 
        1L] == ""
    if (rlabp && missing(header)) 
        header <- TRUE
    if (!header) 
        rlabp <- FALSE
    if (header) {
        data <- data[-1L, , drop = FALSE]
        types <- types[-1L, , drop = FALSE]
        if (missing(col.names)) 
            col.names <- first
        else if (length(first) != length(col.names)) 
            warning("header and 'col.names' are of different lengths")
    }
    else if (missing(col.names)) 
        col.names <- paste0("V", 1L:cols)
    if (length(col.names) + rlabp < cols) 
        stop("more columns than column names")
    if (cols > 0L && length(col.names) > cols) 
        stop("more column names than columns")
    if (cols == 0L) 
        stop("rows are empty: giving up")
    if (check.names) 
        col.names <- make.names(col.names, unique = TRUE)
    if (rlabp) 
        col.names <- c("row.names", col.names)
    nmColClasses <- names(colClasses)
    if (length(colClasses) < cols) 
        if (is.null(nmColClasses)) {
            colClasses <- rep_len(colClasses, cols)
        }
        else {
            tmp <- rep_len(NA_character_, cols)
            names(tmp) <- col.names
            i <- match(nmColClasses, col.names, 0L)
            if (any(i <= 0L)) 
                warning("not all columns named in 'colClasses' exist")
            tmp[i[i > 0L]] <- colClasses
            colClasses <- tmp
        }
    colClasses[colClasses %in% c("real", "double")] <- "numeric"
    known <- colClasses %in% c("logical", "integer", "numeric", 
        "complex", "character")
    keep <- !(colClasses %in% "NULL")
    if (blank.lines.skip) 
        data <- data[apply(data, 1L, function(x) !all(x == "")), 
            , drop = FALSE]
    if (nrows > -1 && nrows < nrow(data)) 
        data <- data[seq_len(nrows), , drop = FALSE]
    nlines <- nrow(data)
    data[data %in% na.strings] <- NA
    data <- as.data.frame(data, stringsAsFactors = FALSE)
    names(data) <- col.names
    if (cols != length(data)) {
        warning("cols = ", cols, " != length(data) = ", length(data), 
            domain = NA)
        cols <- length(data)
    }
    if (is.logical(as.is)) {
        as.is <- rep_len(as.is, cols)
    }
    else if (is.numeric(as.is)) {
        if (any(as.is < 1 | as.is > cols)) 
            stop("invalid numeric 'as.is' expression")
        i <- rep.int(FALSE, cols)
        i[as.is] <- TRUE
        as.is <- i
    }
    else if (is.character(as.is)) {
        i <- match(as.is, col.names, 0L)
        if (any(i <= 0L)) 
            warning("not all columns named in 'as.is' exist")
        i <- i[i > 0L]
        as.is <- rep.int(FALSE, cols)
        as.is[i] <- TRUE
    }
    else if (length(as.is) != cols) 
        stop(gettextf("'as.is' has the wrong length %d  != cols = %d", 
            length(as.is), cols), domain = NA)
    do <- keep & !known
    if (rlabp) 
        do[1L] <- FALSE
    for (i in (1L:cols)[do]) {
        data[[i]] <- if (is.na(colClasses[i])) {
            if (any(types[, i] == "character")) {
                if (stringsAsFactors && !as.is[i]) 
                  as.factor(data[[i]])
                else data[[i]]
            }
            else type.convert(data[[i]], as.is = as.is[i], dec = dec, 
                na.strings = character(0L), numerals = numerals)
        }
        else if (colClasses[i] == "factor") 
            as.factor(data[[i]])
        else if (colClasses[i] == "Date") 
            as.Date(data[[i]])
        else if (colClasses[i] == "POSIXct") 
            as.POSIXct(data[[i]])
        else methods::as(data[[i]], colClasses[i])
    }
    compactRN <- TRUE
    if (missing(row.names)) {
        if (rlabp) {
            row.names <- data[[1L]]
            data <- data[-1L]
            keep <- keep[-1L]
            compactRN <- FALSE
        }
        else row.names <- .set_row_names(as.integer(nlines))
    }
    else if (is.null(row.names)) {
        row.names <- .set_row_names(as.integer(nlines))
    }
    else if (is.character(row.names)) {
        compactRN <- FALSE
        if (length(row.names) == 1L) {
            rowvar <- (1L:cols)[match(col.names, row.names, 0L) == 
                1L]
            row.names <- data[[rowvar]]
            data <- data[-rowvar]
            keep <- keep[-rowvar]
        }
    }
    else if (is.numeric(row.names) && length(row.names) == 1L) {
        compactRN <- FALSE
        rlabp <- row.names
        row.names <- data[[rlabp]]
        data <- data[-rlabp]
        keep <- keep[-rlabp]
    }
    else stop("invalid 'row.names' specification")
    data <- data[keep]
    if (is.object(row.names) || !(is.integer(row.names))) 
        row.names <- as.character(row.names)
    if (!compactRN) {
        if (length(row.names) != nlines) 
            stop("invalid 'row.names' length")
        if (anyDuplicated(row.names)) 
            stop("duplicate 'row.names' are not allowed")
        if (anyNA(row.names)) 
            stop("missing values in 'row.names' are not allowed")
    }
    attr(data, "row.names") <- row.names
    data
}


hsearch_db_concepts <- function (db = hsearch_db()) 
{
    pos <- match(db$Concepts[, "ID"], db$Base[, "ID"])
    entries <- split(as.data.frame(db$Base[pos, ], stringsAsFactors = FALSE), 
        db$Concepts[, "Concept"])
    enums <- sapply(entries, NROW)
    pnums <- sapply(entries, function(e) length(unique(e$Package)))
    pos <- order(enums, pnums, decreasing = TRUE)
    data.frame(Concept = names(entries)[pos], Frequency = enums[pos], 
        Packages = pnums[pos], stringsAsFactors = FALSE, row.names = NULL)
}


getSrcLocation <- function (x, which = c("line", "column", "byte", "parse"), first = TRUE) 
{
    srcref <- getSrcref(x)
    if (is.null(srcref)) 
        return(NULL)
    if (is.list(srcref)) 
        sapply(srcref, getSrcLocation, which, first)
    else {
        if (length(srcref) == 6L) 
            srcref <- c(srcref, srcref[c(1L, 3L)])
        which <- match.arg(which)
        if (first) 
            index <- c(line = 1L, column = 5L, byte = 2L, parse = 7L)[which]
        else index <- c(line = 3L, column = 6L, byte = 4L, parse = 8L)[which]
        srcref[index]
    }
}


lsf.str <- function (pos = -1, envir, ...) 
{
    if (missing(envir)) 
        envir <- as.environment(pos)
    ls.str(pos = pos, envir = envir, mode = "function", ...)
}


tar <- function (tarfile, files = NULL, compression = c("none", "gzip", 
    "bzip2", "xz"), compression_level = 6, tar = Sys.getenv("tar"), 
    extra_flags = "") 
{
    files <- if (is.null(files)) 
        list.files(recursive = TRUE, all.files = TRUE, full.names = TRUE, 
            include.dirs = TRUE)
    else list.files(files, recursive = TRUE, all.files = TRUE, 
        full.names = TRUE, include.dirs = TRUE)
    if (is.character(tarfile)) {
        if (nzchar(tar) && tar != "internal") {
            flags <- switch(match.arg(compression), none = "-cf", 
                gzip = "-zcf", bzip2 = "-jcf", xz = "-Jcf")
            if (grepl("darwin", R.version$os)) {
                tar <- paste("COPYFILE_DISABLE=1", tar)
                if (grepl("darwin8", R.version$os)) 
                  tar <- paste("COPY_EXTENDED_ATTRIBUTES_DISABLE=1", 
                    tar)
            }
            if (is.null(extra_flags)) 
                extra_flags <- ""
            cmd <- paste(tar, extra_flags, flags, shQuote(tarfile), 
                paste(shQuote(files), collapse = " "))
            return(invisible(system(cmd)))
        }
        con <- switch(match.arg(compression), none = file(tarfile, 
            "wb"), gzip = gzfile(tarfile, "wb", compression = compression_level), 
            bzip2 = bzfile(tarfile, "wb", compression = compression_level), 
            xz = xzfile(tarfile, "wb", compression = compression_level))
        on.exit(close(con))
    }
    else if (inherits(tarfile, "connection")) 
        con <- tarfile
    else stop("'tarfile' must be a character string or a connection")
    GNUname <- function(name, link = FALSE) {
        header <- raw(512L)
        n1 <- charToRaw("ExtendedName")
        header[seq_along(n1)] <- n1
        header[157L] <- charToRaw(ifelse(link, "K", "L"))
        size <- length(name)
        header[125:135] <- charToRaw(sprintf("%011o", as.integer(size)))
        header[149:156] <- charToRaw(" ")
        checksum <- sum(as.integer(header))%%2^24
        header[149:154] <- charToRaw(sprintf("%06o", as.integer(checksum)))
        header[155L] <- as.raw(0L)
        writeBin(header, con)
        writeBin(name, con)
        ssize <- 512L * ceiling(size/512L)
        if (ssize > size) 
            writeBin(raw(ssize - size), con)
    }
    warn1 <- character()
    invalid_uid <- invalid_gid <- FALSE
    for (f in unique(files)) {
        info <- file.info(f)
        if (is.na(info$size)) {
            warning(gettextf("file '%s' not found", f), domain = NA)
            next
        }
        header <- raw(512L)
        if (info$isdir && !grepl("/$", f)) 
            f <- paste0(f, "/")
        name <- charToRaw(f)
        if (length(name) > 100L) {
            OK <- TRUE
            if (length(name) > 256L) 
                OK <- FALSE
            else {
                m <- length(name)
                s <- max(which(name[1:min(156, m - 1L)] == charToRaw("/")))
                if (is.infinite(s) || s + 100L < length(name)) 
                  OK <- FALSE
            }
            warning("storing paths of more than 100 bytes is not portable:\n  ", 
                sQuote(f), domain = NA)
            if (OK) {
                prefix <- name[1:(s - 1L)]
                name <- name[-(1:s)]
                header[345L + seq_along(prefix)] <- prefix
            }
            else {
                GNUname(name)
                name <- charToRaw("dummy")
                warn1 <- c(warn1, "using GNU extension for long pathname")
            }
        }
        header[seq_along(name)] <- name
        mode <- info$mode
        if (is.null(extra_flags) && grepl("/(configure|cleanup)$", 
            f) && (mode & "111") != as.octmode("111")) {
            warning(gettextf("file '%s' did not have execute permissions: corrected", 
                f), domain = NA, call. = FALSE)
            mode <- mode | "111"
        }
        header[101:107] <- charToRaw(sprintf("%07o", mode))
        uid <- info$uid
        if (!is.null(uid) && !is.na(uid)) {
            if (uid < 0L || uid > 32767L) {
                invalid_uid <- TRUE
                uid <- 32767L
            }
            header[109:115] <- charToRaw(sprintf("%07o", uid))
        }
        gid <- info$gid
        if (!is.null(gid) && !is.na(gid)) {
            if (gid < 0L || gid > 32767L) {
                invalid_gid <- TRUE
                gid <- 32767L
            }
            header[117:123] <- charToRaw(sprintf("%07o", gid))
        }
        header[137:147] <- charToRaw(sprintf("%011o", as.integer(info$mtime)))
        if (info$isdir) 
            header[157L] <- charToRaw("5")
        else {
            lnk <- Sys.readlink(f)
            if (is.na(lnk)) 
                lnk <- ""
            header[157L] <- charToRaw(ifelse(nzchar(lnk), "2", 
                "0"))
            if (nzchar(lnk)) {
                if (nchar(lnk, "b") > 100L) {
                  GNUname(charToRaw(lnk), TRUE)
                  warn1 <- c(warn1, "using GNU extension for long linkname")
                  lnk <- "dummy"
                }
                header[157L + seq_len(nchar(lnk))] <- charToRaw(lnk)
                size <- 0
            }
        }
        size <- ifelse(info$isdir, 0, info$size)
        if (size >= 8^11) 
            stop("file size is limited to 8GB")
        header[125:135] <- .Call(C_octsize, size)
        header[258:262] <- charToRaw("ustar")
        header[264:265] <- charToRaw("0")
        s <- info$uname
        if (!is.null(s) && !is.na(s)) {
            ns <- nchar(s, "b")
            header[265L + (1:ns)] <- charToRaw(s)
        }
        s <- info$grname
        if (!is.null(s) && !is.na(s)) {
            ns <- nchar(s, "b")
            header[297L + (1:ns)] <- charToRaw(s)
        }
        header[149:156] <- charToRaw(" ")
        checksum <- sum(as.integer(header))%%2^24
        header[149:154] <- charToRaw(sprintf("%06o", as.integer(checksum)))
        header[155L] <- as.raw(0L)
        writeBin(header, con)
        if (info$isdir || nzchar(lnk)) 
            next
        inf <- file(f, "rb")
        for (i in seq_len(ceiling(info$size/512L))) {
            block <- readBin(inf, "raw", 512L)
            writeBin(block, con)
            if ((n <- length(block)) < 512L) 
                writeBin(raw(512L - n), con)
        }
        close(inf)
    }
    if (invalid_uid) 
        warning(gettextf("invalid uid value replaced by that for user 'nobody'", 
            uid), domain = NA, call. = FALSE)
    if (invalid_gid) 
        warning(gettextf("invalid gid value replaced by that for user 'nobody'", 
            uid), domain = NA, call. = FALSE)
    block <- raw(512L)
    writeBin(block, con)
    writeBin(block, con)
    if (length(warn1)) {
        warn1 <- unique(warn1)
        for (w in warn1) warning(w, domain = NA)
    }
    invisible(0L)
}


help.request <- function (subject = "", address = "r-help@R-project.org", file = "R.help.request", 
    ...) 
{
    no <- function(answer) answer == "n"
    yes <- function(answer) answer == "y"
    webpage <- "corresponding web page"
    catPlease <- function() cat("Please do this first - the", 
        webpage, "has been loaded in your web browser\n")
    go <- function(url) {
        catPlease()
        browseURL(url)
    }
    readMyLine <- function(..., .A. = "(y/n)") readline(paste(paste(strwrap(paste(...)), 
        collapse = "\n"), .A., ""))
    checkPkgs <- function(pkgDescs, pkgtxt = paste("packages", 
        paste(names(pkgDescs), collapse = ", "))) {
        cat("Checking if", pkgtxt, "are up-to-date; may take some time...\n")
        stopifnot(sapply(pkgDescs, inherits, what = "packageDescription"))
        fields <- .instPkgFields(NULL)
        n <- length(pkgDescs)
        iPkgs <- matrix(NA_character_, n, 2L + length(fields), 
            dimnames = list(NULL, c("Package", "LibPath", fields)))
        for (i in seq_len(n)) {
            desc <- c(unlist(pkgDescs[[i]]), LibPath = dirname(dirname(dirname(attr(pkgDescs[[i]], 
                "file")))))
            nms <- intersect(names(desc), colnames(iPkgs))
            iPkgs[i, nms] <- desc[nms]
        }
        old <- old.packages(instPkgs = iPkgs)
        if (!is.null(old)) {
            update <- readMyLine("The following installed packages are out-of-date:\n", 
                paste(strwrap(rownames(old), width = 0.7 * getOption("width"), 
                  indent = 0.15 * getOption("width")), collapse = "\n"), 
                "would you like to update now?")
            if (yes(update)) 
                update.packages(oldPkgs = old, ask = FALSE)
        }
    }
    cat("Checklist:\n")
    post <- readline("Have you read the posting guide? (y/n) ")
    if (no(post)) 
        return(go("https://www.r-project.org/posting-guide.html"))
    FAQ <- readline("Have you checked the FAQ? (y/n) ")
    if (no(FAQ)) 
        return(go("https://cran.r-project.org/faqs.html"))
    intro <- readline("Have you checked An Introduction to R? (y/n) ")
    if (no(intro)) 
        return(go("https://cran.r-project.org/manuals.html"))
    NEWS <- readMyLine("Have you checked the NEWS of the latest development release?")
    if (no(NEWS)) 
        return(go("https://cran.r-project.org/doc/manuals/r-devel/NEWS.html"))
    rsitesearch <- readline("Have you looked on RSiteSearch? (y/n) ")
    if (no(rsitesearch)) {
        catPlease()
        return(RSiteSearch(subject))
    }
    inf <- sessionInfo()
    if ("otherPkgs" %in% names(inf)) {
        oPkgs <- names(inf$otherPkgs)
        other <- readMyLine("You have packages", paste0("(", 
            paste(sQuote(oPkgs), collapse = ", "), ")"), "other than the base packages loaded. ", 
            "If your query relates to one of these, have you ", 
            "checked any corresponding books/manuals and", "considered contacting the package maintainer?", 
            .A. = "(y/n/NA)")
        if (no(other)) 
            return("Please do this first.")
    }
    page <- url("https://cran.r-project.org/bin/windows/base")
    title <- grep("<title>", readLines(page, 10L), fixed = TRUE, 
        value = TRUE)
    ver <- sub("^.*R-([^ ]*) for Windows.*$", "\\1", title)
    if (getRversion() < numeric_version(ver)) {
        update <- readMyLine("Your R version is out-of-date,", 
            "would you like to update now?")
        if (yes(update)) 
            return(go(getOption("repos")))
    }
    if ("otherPkgs" %in% names(inf)) {
        checkPkgs(inf$otherPkgs)
    }
    cat("Have you written example code that is\n", "- minimal\n - reproducible\n - self-contained\n - commented", 
        "\nusing data that is either\n", "- constructed by the code\n - loaded by data()\n", 
        "- reproduced using dump(\"mydata\", file = \"\")\n")
    code <- readMyLine("have you checked this code in a fresh R session", 
        "(invoking R with the --vanilla option if possible)", 
        "and is this code copied to the clipboard?")
    if (no(code)) 
        return(cat("\nIf your query is not directly related to code", 
            "(e.g. a general query \nabout R's capabilities),", 
            "email R-help@r-project.org directly. ", "\nOtherwise prepare some example code first.\n"))
    change <- readline(paste("Would you like to change your subject line:", 
        subject, "to something more meaningful? (y/n) ", sep = "\n"))
    if (yes(change)) 
        subject <- readline("Enter subject: \n")
    create.post(instructions = paste("\\n<<SEND AS PLAIN TEXT!>>\\n\\n", 
        "\\n<<Write your query here, using your example code to illustrate>>", 
        "\\n<<End with your name and affiliation>>\\n\\n\\n\\n"), 
        description = "help request", subject = subject, address = address, 
        filename = file, info = bug.report.info(), ...)
}


cite <- function (keys, bib, ...) 
{
    fn <- tools::bibstyle()$cite
    if (is.null(fn)) 
        fn <- citeNatbib
    fn(keys, bib, ...)
}


.S3methods <- function (generic.function, class, envir = parent.frame()) 
{
    rbindSome <- function(df, nms, msg) {
        nms <- unique(nms)
        n2 <- length(nms)
        dnew <- data.frame(visible = rep.int(FALSE, n2), from = rep.int(msg, 
            n2), row.names = nms)
        n <- nrow(df)
        if (n == 0L) 
            return(dnew)
        keep <- !duplicated(c(rownames(df), rownames(dnew)))
        rbind(df[keep[1L:n], ], dnew[keep[(n + 1L):(n + n2)], 
            ])
    }
    S3MethodsStopList <- tools::nonS3methods(NULL)
    knownGenerics <- getKnownS3generics()
    sp <- search()
    methods.called <- identical(sys.call(-1)[[1]], as.symbol("methods"))
    an <- lapply(seq_along(sp), ls)
    lens <- lengths(an)
    an <- unlist(an, use.names = FALSE)
    names(an) <- rep(sp, lens)
    an <- an[!duplicated(an)]
    info <- data.frame(visible = rep.int(TRUE, length(an)), from = .rmpkg(names(an)), 
        row.names = an)
    if (!missing(generic.function)) {
        if (!is.character(generic.function)) 
            generic.function <- deparse(substitute(generic.function))
        if (!exists(generic.function, mode = "function", envir = envir) && 
            !any(generic.function == c("Math", "Ops", "Complex", 
                "Summary"))) 
            stop(gettextf("no function '%s' is visible", generic.function), 
                domain = NA)
        warn.not.generic <- FALSE
        if (!any(generic.function == knownGenerics)) {
            truegf <- findGeneric(generic.function, envir, warnS4only = !methods.called)
            if (truegf == "") 
                warn.not.generic <- TRUE
            else if (truegf != generic.function) {
                warning(gettextf("generic function '%s' dispatches methods for generic '%s'", 
                  generic.function, truegf), domain = NA)
                generic.function <- truegf
            }
        }
        info <- info[startsWith(row.names(info), paste0(generic.function, 
            ".")), ]
        info <- info[!row.names(info) %in% S3MethodsStopList, 
            ]
        if (nrow(info)) {
            keep <- vapply(row.names(info), exists, logical(1), 
                mode = "function")
            info <- info[keep, ]
        }
        if (warn.not.generic && nrow(info)) 
            warning(gettextf("function '%s' appears not to be S3 generic; found functions that look like S3 methods", 
                generic.function), domain = NA)
        defenv <- if (!is.na(w <- .knownS3Generics[generic.function])) 
            asNamespace(w)
        else {
            genfun <- get(generic.function, mode = "function", 
                envir = envir)
            if (.isMethodsDispatchOn() && methods::is(genfun, 
                "genericFunction")) 
                genfun <- methods::finalDefaultMethod(genfun@default)
            if (typeof(genfun) == "closure") 
                environment(genfun)
            else .BaseNamespaceEnv
        }
        S3reg <- names(get(".__S3MethodsTable__.", envir = defenv))
        S3reg <- S3reg[startsWith(S3reg, paste0(generic.function, 
            "."))]
        if (length(S3reg)) 
            info <- rbindSome(info, S3reg, msg = paste("registered S3method for", 
                generic.function))
        if (generic.function == "all") 
            info <- info[-grep("^all\\.equal", row.names(info)), 
                ]
    }
    else if (!missing(class)) {
        if (!is.character(class)) 
            class <- paste(deparse(substitute(class)))
        name <- paste0(".", class, "$")
        name <- gsub("([.[])", "\\\\\\1", name)
        info <- info[grep(name, row.names(info)), ]
        info <- info[!row.names(info) %in% S3MethodsStopList, 
            ]
        if (nrow(info)) {
            possible.generics <- gsub(name, "", row.names(info))
            keep <- vapply(possible.generics, function(nm) {
                if (nm %in% knownGenerics) 
                  return(TRUE)
                where <- find(nm, mode = "function")
                if (length(where)) 
                  any(vapply(where, function(w) nzchar(findGeneric(nm, 
                    envir = as.environment(w))), logical(1)))
                else FALSE
            }, logical(1))
            info <- info[keep, ]
        }
        S3reg <- unlist(lapply(loadedNamespaces(), function(i) ls(get(".__S3MethodsTable__.", 
            envir = asNamespace(i)), pattern = name)))
        if (length(S3reg)) 
            S3reg <- S3reg[vapply(gsub(name, "", S3reg), exists, 
                NA)]
        if (length(S3reg)) 
            info <- rbindSome(info, S3reg, msg = "registered S3method")
    }
    else stop("must supply 'generic.function' or 'class'")
    info$generic <- if (!missing(generic.function)) 
        rep(generic.function, nrow(info))
    else sub(paste0("\\.", class, "$"), "", row.names(info))
    info$isS4 <- rep(FALSE, nrow(info))
    info <- info[sort.list(row.names(info)), , drop = FALSE]
    res <- row.names(info)
    class(res) <- "MethodsFunction"
    attr(res, "info") <- info
    attr(res, "byclass") <- missing(generic.function)
    res
}


select.list <- function (choices, preselect = NULL, multiple = FALSE, title = NULL, 
    graphics = getOption("menu.graphics")) 
{
    if (!interactive()) 
        stop("select.list() cannot be used non-interactively")
    if (!is.null(title) && (!is.character(title) || length(title) != 
        1)) 
        stop("'title' must be NULL or a length-1 character vector")
    if (isTRUE(graphics)) {
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA") 
            return(.External2(C_selectlist, choices, preselect, 
                multiple, title))
        else if (graphics && capabilities("tcltk") && capabilities("X11") && 
            suppressWarnings(tcltk::.TkUp)) 
            return(tcltk::tk_select.list(choices, preselect, 
                multiple, title))
    }
    if (!multiple) {
        res <- menu(choices, FALSE, title)
        if (res < 1L || res > length(choices)) 
            return("")
        else return(choices[res])
    }
    else {
        nc <- length(choices)
        if (length(title) && nzchar(title[1L])) 
            cat(title, "\n", sep = "")
        def <- if (is.null(preselect)) 
            rep(FALSE, nc)
        else choices %in% preselect
        op <- paste0(format(seq_len(nc)), ": ", ifelse(def, "+", 
            " "), " ", choices)
        if (nc > 10L) {
            fop <- format(op)
            nw <- nchar(fop[1L], "w") + 2L
            ncol <- getOption("width")%/%nw
            if (ncol > 1L) 
                op <- paste(fop, c(rep("  ", ncol - 1L), "\n"), 
                  sep = "", collapse = "")
            cat("", op, sep = "\n")
        }
        else cat("", op, "", sep = "\n")
        cat(gettext("Enter one or more numbers separated by spaces, or an empty line to cancel\n"))
        repeat {
            res <- tryCatch(scan("", what = 0, quiet = TRUE, 
                nlines = 1), error = identity)
            if (!inherits(res, "error")) 
                break
            cat(gettext("Invalid input, please try again\n"))
        }
        if (!length(res) || (length(res) == 1L && !res[1L])) 
            return(character())
        res <- sort(res[1 <= res && res <= nc])
        return(choices[res])
    }
}


.romans <- structure(c(1000L, 900L, 500L, 400L, 100L, 90L, 50L, 40L, 10L, 
9L, 5L, 4L, 1L), .Names = c("M", "CM", "D", "CD", "C", "XC", 
"L", "XL", "X", "IX", "V", "IV", "I"))


getCRANmirrors <- function (all = FALSE, local.only = FALSE) 
{
    .getMirrors("https://cran.r-project.org/CRAN_mirrors.csv", 
        file.path(R.home("doc"), "CRAN_mirrors.csv"), all = all, 
        local.only = local.only)
}


relist <- function (flesh, skeleton = attr(flesh, "skeleton")) 
{
    if (is.null(skeleton)) {
        stop("The 'flesh' argument does not contain a skeleton attribute.\n", 
            "Either ensure you unlist a relistable object, or specify the skeleton separately.")
    }
    UseMethod("relist", skeleton)
}


ls.str <- function (pos = -1, name, envir, all.names = FALSE, pattern, 
    mode = "any") 
{
    if (missing(envir)) 
        envir <- as.environment(pos)
    nms <- ls(name, envir = envir, all.names = all.names, pattern = pattern)
    r <- unlist(lapply(nms, function(n) exists(n, envir = envir, 
        mode = mode, inherits = FALSE)))
    structure(nms[r], envir = envir, mode = mode, class = "ls_str")
}


localeToCharset <- function (locale = Sys.getlocale("LC_CTYPE")) 
{
    guess <- function(en) {
        if (en %in% c("aa", "af", "an", "br", "ca", "da", "de", 
            "en", "es", "et", "eu", "fi", "fo", "fr", "ga", "gl", 
            "gv", "id", "is", "it", "kl", "kw", "ml", "ms", "nb", 
            "nn", "no", "oc", "om", "pt", "so", "sq", "st", "sv", 
            "tl", "uz", "wa", "xh", "zu")) 
            return("ISO8859-1")
        if (en %in% c("bs", "cs", "hr", "hu", "pl", "ro", "sk", 
            "sl")) 
            return("ISO8859-2")
        if (en %in% "mt") 
            return("ISO8859-3")
        if (en %in% c("mk", "ru")) 
            return("ISO8859-5")
        if (en %in% "ar") 
            return("ISO8859-6")
        if (en %in% "el") 
            return("ISO8859-7")
        if (en %in% c("he", "iw", "yi")) 
            return("ISO8859-8")
        if (en %in% "tr") 
            return("ISO8859-9")
        if (en %in% "lg") 
            return("ISO8859-10")
        if (en %in% c("lt", "lv", "mi")) 
            return("ISO8859-13")
        if (en %in% "cy") 
            return("ISO8859-14")
        if (en %in% "uk") 
            return("KOI8-U")
        if (en %in% "ja") 
            return("EUC-JP")
        if (en %in% "ko") 
            return("EUC-KR")
        if (en %in% "th") 
            return("TIS-620")
        if (en %in% "tg") 
            return("KOI8-T")
        if (en %in% "ka") 
            return("GEORGIAN-PS")
        if (en %in% "kk") 
            return("PT154")
        return(NA_character_)
    }
    if (locale %in% c("C", "POSIX")) 
        return("ASCII")
    if (.Platform$OS.type == "windows") {
        x <- strsplit(locale, ".", fixed = TRUE)[[1L]]
        if (length(x) != 2) 
            return(NA_character_)
        switch(x[2L], `1252` = return("ISO8859-1"), `1257` = return("ISO8859-13"))
        return(paste0("CP", x[2L]))
    }
    else {
        x <- strsplit(locale, ".", fixed = TRUE)[[1L]]
        enc <- if (length(x) == 2) 
            gsub("@.*$o", "", x[2L])
        else ""
        if (toupper(enc) == "UTF-8") 
            enc <- "utf8"
        if (nzchar(enc) && enc != "utf8") {
            enc <- tolower(enc)
            known <- c("ISO8859-1", "ISO8859-2", "ISO8859-3", 
                "ISO8859-6", "ISO8859-7", "ISO8859-8", "ISO8859-9", 
                "ISO8859-10", "ISO8859-13", "ISO8859-14", "ISO8859-15", 
                "CP1251", "CP1255", "EUC-JP", "EUC-KR", "EUC-TW", 
                "GEORGIAN-PS", "KOI8-R", "KOI8-U", "TCVN", "BIG5", 
                "GB2312", "GB18030", "GBK", "TIS-620", "SHIFT_JIS", 
                "GB2312", "BIG5-HKSCS")
            names(known) <- c("iso88591", "iso88592", "iso88593", 
                "iso88596", "iso88597", "iso88598", "iso88599", 
                "iso885910", "iso885913", "iso885914", "iso885915", 
                "cp1251", "cp1255", "eucjp", "euckr", "euctw", 
                "georgianps", "koi8r", "koi8u", "tcvn", "big5", 
                "gb2312", "gb18030", "gbk", "tis-620", "sjis", 
                "eucn", "big5-hkscs")
            if (grepl("darwin", R.version$os)) {
                k <- c(known, "ISO8859-1", "ISO8859-2", "ISO8859-4", 
                  "ISO8859-7", "ISO8859-9", "ISO8859-13", "ISO8859-15", 
                  "KOI8-U", "KOI8-R", "PT154", "ASCII", "ARMSCII-8", 
                  "ISCII-DEV", "BIG5-HKCSC")
                names(k) <- c(names(known), "iso8859-1", "iso8859-2", 
                  "iso8859-4", "iso8859-7", "iso8859-9", "iso8859-13", 
                  "iso8859-15", "koi8-u", "koi8-r", "pt154", 
                  "us-ascii", "armscii-8", "iscii-dev", "big5hkscs")
                known <- k
            }
            if (enc %in% names(known)) 
                return(unname(known[enc]))
            if (length(grep("^cp-", enc))) 
                return(sub("cp-([0-9]+)", "CP\\1", enc))
            if (enc == "EUC") {
                if (length(grep("^[[:alpha:]]{2}_", x[1L], perl = TRUE))) {
                  ll <- substr(x[1L], 1L, 2L)
                  return(switch(ll, jp = "EUC-JP", kr = "EUC-KR", 
                    zh = "GB2312"))
                }
            }
        }
        if (grepl("darwin", R.version$os)) 
            return("UTF-8")
        if (length(grep("^[[:alpha:]]{2}_", x[1L], perl = TRUE))) {
            ll <- substr(x[1L], 1L, 2L)
            if (enc == "utf8") 
                return(c("UTF-8", guess(ll)))
            else return(guess(ll))
        }
        return(NA_character_)
    }
}


download.packages <- function (pkgs, destdir, available = NULL, repos = getOption("repos"), 
    contriburl = contrib.url(repos, type), method, type = getOption("pkgType"), 
    ...) 
{
    nonlocalcran <- length(grep("^file:", contriburl)) < length(contriburl)
    if (nonlocalcran && !dir.exists(destdir)) 
        stop("'destdir' is not a directory")
    type <- resolvePkgType(type)
    if (is.null(available)) 
        available <- available.packages(contriburl = contriburl, 
            method = method)
    retval <- matrix(character(), 0L, 2L)
    for (p in unique(pkgs)) {
        ok <- (available[, "Package"] == p)
        ok <- ok & !is.na(ok)
        if (!any(ok)) 
            warning(gettextf("no package %s at the repositories", 
                sQuote(p)), domain = NA, immediate. = TRUE)
        else {
            if (sum(ok) > 1L) {
                vers <- package_version(available[ok, "Version"])
                keep <- vers == max(vers)
                keep[duplicated(keep)] <- FALSE
                ok[ok][!keep] <- FALSE
            }
            if (substr(type, 1L, 10L) == "mac.binary") 
                type <- "mac.binary"
            File <- available[ok, "File"]
            fn <- paste0(p, "_", available[ok, "Version"], switch(type, 
                source = ".tar.gz", mac.binary = ".tgz", win.binary = ".zip"))
            have_fn <- !is.na(File)
            fn[have_fn] <- File[have_fn]
            repos <- available[ok, "Repository"]
            if (length(grep("^file:", repos)) > 0L) {
                if (substring(repos, 1L, 8L) == "file:///") {
                  fn <- paste(substring(repos, 8L), fn, sep = "/")
                  if (.Platform$OS.type == "windows") {
                    if (length(grep("^/[A-Za-z]:", fn))) 
                      fn <- substring(fn, 2L)
                  }
                }
                else {
                  fn <- paste(substring(repos, 6L), fn, sep = "/")
                }
                if (file.exists(fn)) 
                  retval <- rbind(retval, c(p, fn))
                else warning(gettextf("package %s does not exist on the local repository", 
                  sQuote(p)), domain = NA, immediate. = TRUE)
            }
            else {
                url <- paste(repos, fn, sep = "/")
                destfile <- file.path(destdir, fn)
                res <- try(download.file(url, destfile, method, 
                  mode = "wb", ...))
                if (!inherits(res, "try-error") && res == 0L) 
                  retval <- rbind(retval, c(p, destfile))
                else warning(gettextf("download of package %s failed", 
                  sQuote(p)), domain = NA, immediate. = TRUE)
            }
        }
    }
    retval
}


debugger <- function (dump = last.dump) 
{
    debugger.look <- function(.selection) {
        for (.obj in ls(envir = dump[[.selection]], all.names = TRUE)) tryCatch(assign(.obj, 
            get(.obj, envir = dump[[.selection]])), error = function(e) {
        })
        cat(gettext("Browsing in the environment with call:\n   "), 
            calls[.selection], "\n", sep = "")
        rm(.obj, .selection)
        browser()
    }
    if (!inherits(dump, "dump.frames")) {
        cat(gettextf("'dump' is not an object of class %s\n", 
            dQuote("dump.frames")))
        return(invisible())
    }
    err.action <- getOption("error")
    on.exit(options(error = err.action))
    if (length(msg <- attr(dump, "error.message"))) 
        cat(gettext("Message: "), msg)
    n <- length(dump)
    if (!n) {
        cat(gettextf("'dump' is empty\n"))
        return(invisible())
    }
    calls <- names(dump)
    repeat {
        cat(gettext("Available environments had calls:\n"))
        cat(paste0(1L:n, ": ", calls), sep = "\n")
        cat(gettext("\nEnter an environment number, or 0 to exit  "))
        repeat {
            ind <- .Call(C_menu, as.character(calls))
            if (ind <= n) 
                break
        }
        if (ind == 0L) 
            return(invisible())
        debugger.look(ind)
    }
}


chooseCRANmirror <- function (graphics = getOption("menu.graphics"), ind = NULL, 
    useHTTPS = getOption("useHTTPS", TRUE), local.only = FALSE) 
{
    m <- getCRANmirrors(all = FALSE, local.only = local.only)
    url <- .chooseMirror(m, "CRAN", graphics, ind, useHTTPS)
    if (length(url)) {
        repos <- getOption("repos")
        repos["CRAN"] <- url
        options(repos = repos)
    }
    invisible()
}


savehistory <- function (file = ".Rhistory") 
invisible(.External2(C_savehistory, file))


citEntry <- function (entry, textVersion, header = NULL, footer = NULL, ...) 
bibentry(bibtype = entry, textVersion = textVersion, header = header, 
    footer = footer, ...)


RweaveTryStop <- function (err, options) 
{
    if (inherits(err, "try-error")) {
        cat("\n")
        msg <- paste(" chunk", options$chunknr)
        if (!is.null(options$label)) 
            msg <- paste0(msg, " (label = ", options$label, ")")
        msg <- paste(msg, "\n")
        stop(msg, err, call. = FALSE)
    }
}


close.socket <- function (socket, ...) 
.Call(C_sockclose, socket$socket)


vignette <- function (topic, package = NULL, lib.loc = NULL, all = TRUE) 
{
    vinfo <- tools::getVignetteInfo(package, lib.loc, all)
    if (!missing(topic)) {
        topic <- topic[1L]
        vinfo <- vinfo[vinfo[, "Topic"] == topic, , drop = FALSE]
        if (length(vinfo)) {
            pos <- which(file_test("-f", file.path(vinfo[, "Dir"], 
                "doc", vinfo[, "PDF"])))
            if (!length(pos)) {
                z <- as.list(vinfo[1L, ])
                z$PDF <- ""
            }
            else {
                if (length(pos) > 1L) {
                  pos <- pos[1L]
                  warning(gettextf("vignette %s found more than once,\nusing the one found in %s", 
                    sQuote(topic), sQuote(file.path(vinfo[pos, 
                      "Dir"], "doc"))), call. = FALSE, domain = NA)
                }
                z <- as.list(vinfo[pos, ])
            }
            if (!file_test("-f", file.path(z$Dir, "doc", z$R))) 
                z$R <- ""
            class(z) <- "vignette"
            return(z)
        }
        else warning(gettextf("vignette %s not found", sQuote(topic)), 
            call. = FALSE, domain = NA)
    }
    else {
        title <- if (nrow(vinfo)) {
            paste(vinfo[, "Title"], paste0(rep.int("(source", 
                nrow(vinfo)), ifelse(nzchar(vinfo[, "PDF"]), 
                paste0(", ", tools::file_ext(vinfo[, "PDF"])), 
                ""), ")"))
        }
        else character()
        db <- cbind(Package = basename(vinfo[, "Dir"]), LibPath = dirname(vinfo[, 
            "Dir"]), Item = vinfo[, "Topic"], Title = title)
        footer <- if (all) 
            NULL
        else paste0("Use ", sQuote("vignette(all = TRUE)"), "\n", 
            "to list the vignettes in all *available* packages.")
        structure(class = "packageIQR", list(type = "vignette", 
            title = "Vignettes", header = NULL, results = db, 
            footer = footer))
    }
}


getS3method <- function (f, class, optional = FALSE, envir = parent.frame()) 
{
    if (!any(f == getKnownS3generics())) {
        truegf <- findGeneric(f, envir)
        if (nzchar(truegf)) 
            f <- truegf
        else {
            if (optional) 
                return(NULL)
            else stop(gettextf("no function '%s' could be found", 
                f), domain = NA)
        }
    }
    method <- paste(f, class, sep = ".")
    if (!is.null(m <- get0(method, envir = envir, mode = "function"))) 
        return(m)
    defenv <- if (!is.na(w <- .knownS3Generics[f])) 
        asNamespace(w)
    else if (f %in% tools:::.get_internal_S3_generics()) 
        .BaseNamespaceEnv
    else {
        genfun <- get(f, mode = "function", envir = envir)
        if (.isMethodsDispatchOn() && methods::is(genfun, "genericFunction")) 
            genfun <- methods::selectMethod(genfun, "ANY")
        if (typeof(genfun) == "closure") 
            environment(genfun)
        else .BaseNamespaceEnv
    }
    S3Table <- get(".__S3MethodsTable__.", envir = defenv)
    if (!is.null(m <- get0(method, envir = S3Table, inherits = FALSE))) 
        m
    else if (optional) 
        NULL
    else stop(gettextf("S3 method '%s' not found", method), domain = NA)
}


combn <- function (x, m, FUN = NULL, simplify = TRUE, ...) 
{
    stopifnot(length(m) == 1L, is.numeric(m))
    if (m < 0) 
        stop("m < 0", domain = NA)
    if (is.numeric(x) && length(x) == 1L && x > 0 && trunc(x) == 
        x) 
        x <- seq_len(x)
    n <- length(x)
    if (n < m) 
        stop("n < m", domain = NA)
    x0 <- x
    if (simplify) {
        if (is.factor(x)) 
            x <- as.integer(x)
    }
    m <- as.integer(m)
    e <- 0
    h <- m
    a <- seq_len(m)
    nofun <- is.null(FUN)
    if (!nofun && !is.function(FUN)) 
        stop("'FUN' must be a function or NULL")
    len.r <- length(r <- if (nofun) x[a] else FUN(x[a], ...))
    count <- as.integer(round(choose(n, m)))
    if (simplify) {
        dim.use <- if (nofun) 
            c(m, count)
        else {
            d <- dim(r)
            if (length(d) > 1L) 
                c(d, count)
            else if (len.r > 1L) 
                c(len.r, count)
            else c(d, count)
        }
    }
    if (simplify) 
        out <- matrix(r, nrow = len.r, ncol = count)
    else {
        out <- vector("list", count)
        out[[1L]] <- r
    }
    if (m > 0) {
        i <- 2L
        nmmp1 <- n - m + 1L
        while (a[1L] != nmmp1) {
            if (e < n - h) {
                h <- 1L
                e <- a[m]
                j <- 1L
            }
            else {
                e <- a[m - h]
                h <- h + 1L
                j <- 1L:h
            }
            a[m - h + j] <- e + j
            r <- if (nofun) 
                x[a]
            else FUN(x[a], ...)
            if (simplify) 
                out[, i] <- r
            else out[[i]] <- r
            i <- i + 1L
        }
    }
    if (simplify) {
        if (is.factor(x0)) {
            levels(out) <- levels(x0)
            class(out) <- class(x0)
        }
        dim(out) <- dim.use
    }
    out
}


loadhistory <- function (file = ".Rhistory") 
invisible(.External2(C_loadhistory, file))


update.packages <- function (lib.loc = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
    type), method, instlib = NULL, ask = TRUE, available = NULL, 
    oldPkgs = NULL, ..., checkBuilt = FALSE, type = getOption("pkgType")) 
{
    force(ask)
    text.select <- function(old) {
        update <- NULL
        for (k in seq_len(nrow(old))) {
            cat(old[k, "Package"], ":\n", "Version", old[k, "Installed"], 
                "installed in", old[k, "LibPath"], if (checkBuilt) 
                  paste("built under R", old[k, "Built"]), "\n", 
                "Version", old[k, "ReposVer"], "available at", 
                simplifyRepos(old[k, "Repository"], type))
            cat("\n")
            answer <- substr(readline("Update (y/N/c)?  "), 1L, 
                1L)
            if (answer == "c" | answer == "C") {
                cat("cancelled by user\n")
                return(invisible())
            }
            if (answer == "y" | answer == "Y") 
                update <- rbind(update, old[k, ])
        }
        update
    }
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    if (type == "both" && (!missing(contriburl) || !is.null(available))) {
        stop("specifying 'contriburl' or 'available' requires a single type, not type = \"both\"")
    }
    if (is.null(available)) {
        available <- available.packages(contriburl = contriburl, 
            method = method)
        if (missing(repos)) 
            repos <- getOption("repos")
    }
    if (!is.matrix(oldPkgs) && is.character(oldPkgs)) {
        subset <- oldPkgs
        oldPkgs <- NULL
    }
    else subset <- NULL
    if (is.null(oldPkgs)) {
        oldPkgs <- old.packages(lib.loc = lib.loc, contriburl = contriburl, 
            method = method, available = available, checkBuilt = checkBuilt)
        if (missing(repos)) 
            repos <- getOption("repos")
        if (!is.null(oldPkgs)) {
            pkg <- 0L
            while (pkg < nrow(oldPkgs)) {
                pkg <- pkg + 1L
                if (find.package(oldPkgs[pkg], lib.loc = lib.loc) != 
                  find.package(oldPkgs[pkg], lib.loc = oldPkgs[pkg, 
                    2])) {
                  warning(sprintf("package '%s' in library '%s' will not be updated", 
                    oldPkgs[pkg], oldPkgs[pkg, 2]), call. = FALSE, 
                    immediate. = TRUE)
                  oldPkgs <- oldPkgs[-pkg, , drop = FALSE]
                  pkg <- pkg - 1L
                }
            }
        }
        if (is.null(oldPkgs)) 
            return(invisible())
    }
    else if (!(is.matrix(oldPkgs) && is.character(oldPkgs))) 
        stop("invalid 'oldPkgs'; must be a character vector or a result from old.packages()")
    if (!is.null(subset)) {
        oldPkgs <- oldPkgs[rownames(oldPkgs) %in% subset, , drop = FALSE]
        if (nrow(oldPkgs) == 0) 
            return(invisible())
    }
    update <- if (is.character(ask) && ask == "graphics") {
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA" || (capabilities("tcltk") && capabilities("X11"))) {
            k <- select.list(oldPkgs[, 1L], oldPkgs[, 1L], multiple = TRUE, 
                title = "Packages to be updated", graphics = TRUE)
            oldPkgs[match(k, oldPkgs[, 1L]), , drop = FALSE]
        }
        else text.select(oldPkgs)
    }
    else if (isTRUE(ask)) 
        text.select(oldPkgs)
    else oldPkgs
    if (length(update)) {
        if (is.null(instlib)) 
            instlib <- update[, "LibPath"]
        libs <- unique(instlib)
        for (l in libs) if (type == "both") 
            install.packages(update[instlib == l, "Package"], 
                l, repos = repos, method = method, ..., type = type)
        else install.packages(update[instlib == l, "Package"], 
            l, contriburl = contriburl, method = method, available = available, 
            ..., type = type)
    }
}


glob2rx <- function (pattern, trim.head = FALSE, trim.tail = TRUE) 
{
    if (!length(pattern)) 
        return(character())
    p <- gsub("\\.", "\\\\.", paste0("^", pattern, "$"))
    p <- gsub("\\?", ".", gsub("\\*", ".*", p))
    p <- gsub("([^\\])\\(", "\\1\\\\(", p)
    p <- gsub("([^\\])\\[", "\\1\\\\[", p)
    p <- gsub("([^\\])\\{", "\\1\\\\{", p)
    if (trim.tail) 
        p <- sub("\\.\\*\\$$", "", p)
    if (trim.head) 
        p <- sub("\\^\\.\\*", "", p)
    p
}


old.packages <- function (lib.loc = NULL, repos = getOption("repos"), contriburl = contrib.url(repos, 
    type), instPkgs = installed.packages(lib.loc = lib.loc), 
    method, available = NULL, checkBuilt = FALSE, type = getOption("pkgType")) 
{
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    if (!missing(instPkgs)) {
        if (!is.matrix(instPkgs) || !is.character(instPkgs[, 
            "Package"])) 
            stop("ill-formed 'instPkgs' matrix")
    }
    if (NROW(instPkgs) == 0L) 
        return(NULL)
    available <- if (is.null(available)) 
        available.packages(contriburl = contriburl, method = method)
    else tools:::.remove_stale_dups(available)
    update <- NULL
    currentR <- minorR <- getRversion()
    minorR[[c(1L, 3L)]] <- 0L
    for (k in 1L:nrow(instPkgs)) {
        if (instPkgs[k, "Priority"] %in% "base") 
            next
        z <- match(instPkgs[k, "Package"], available[, "Package"])
        if (is.na(z)) 
            next
        onRepos <- available[z, ]
        if ((!checkBuilt || package_version(instPkgs[k, "Built"]) >= 
            minorR) && package_version(onRepos["Version"]) <= 
            package_version(instPkgs[k, "Version"])) 
            next
        deps <- onRepos["Depends"]
        if (!is.na(deps)) {
            Rdeps <- tools:::.split_dependencies(deps)[["R", 
                exact = TRUE]]
            if (length(Rdeps) > 1L) {
                target <- Rdeps$version
                res <- do.call(Rdeps$op, list(currentR, target))
                if (!res) 
                  next
            }
        }
        update <- rbind(update, c(instPkgs[k, c("Package", "LibPath", 
            "Version", "Built")], onRepos["Version"], onRepos["Repository"]))
    }
    if (!is.null(update)) 
        colnames(update) <- c("Package", "LibPath", "Installed", 
            "Built", "ReposVer", "Repository")
    rownames(update) <- update[, "Package"]
    update[!duplicated(update), , drop = FALSE]
}


remove.packages <- function (pkgs, lib) 
{
    updateIndices <- function(lib) {
        if (lib == .Library && .Platform$OS.type == "unix") {
            message("Updating HTML index of packages in '.Library'")
            make.packages.html(.Library)
        }
        Rcss <- file.path(lib, "R.css")
        if (file.exists(Rcss)) {
            pkgs <- Sys.glob(file.path(lib, "*", "Meta", "package.rds"))
            if (!length(pkgs)) 
                unlink(Rcss)
        }
    }
    if (!length(pkgs)) 
        return(invisible())
    if (missing(lib) || is.null(lib)) {
        lib <- .libPaths()[1L]
        message(sprintf(ngettext(length(pkgs), "Removing package from %s\n(as %s is unspecified)", 
            "Removing packages from %s\n(as %s is unspecified)"), 
            sQuote(lib), sQuote("lib")), domain = NA)
    }
    paths <- find.package(pkgs, lib)
    if (length(paths)) {
        unlink(paths, TRUE)
        for (lib in unique(dirname(paths))) updateIndices(lib)
    }
    invisible()
}


getParseData <- function (x, includeText = NA) 
{
    if (inherits(x, "srcfile")) 
        srcfile <- x
    else srcfile <- getSrcfile(x)
    if (is.null(srcfile)) 
        return(NULL)
    else data <- srcfile$parseData
    if (!is.null(data)) {
        tokens <- attr(data, "tokens")
        data <- t(unclass(data))
        colnames(data) <- c("line1", "col1", "line2", "col2", 
            "terminal", "token.num", "id", "parent")
        data <- data.frame(data[, -c(5, 6), drop = FALSE], token = tokens, 
            terminal = as.logical(data[, "terminal"]), text = attr(data, 
                "text"), stringsAsFactors = FALSE)
        o <- order(data[, 1], data[, 2], -data[, 3], -data[, 
            4])
        data <- data[o, ]
        rownames(data) <- data$id
        attr(data, "srcfile") <- srcfile
        if (isTRUE(includeText)) 
            gettext <- which(!nzchar(data$text))
        else if (is.na(includeText)) 
            gettext <- which(!nzchar(data$text) & data$terminal)
        else {
            gettext <- integer(0)
            data$text <- NULL
        }
        if (length(gettext)) 
            data$text[gettext] <- getParseText(data, data$id[gettext])
    }
    data
}


xedit <- function (name = NULL, file = "") 
edit.default(name, file, editor = "xedit")


SweaveSyntaxLatex <- structure(list(doc = "^[[:space:]]*\\\\end\\{Scode\\}", code = "^[[:space:]]*\\\\begin\\{Scode\\}\\{?([^}]*)\\}?.*", 
    coderef = "^[[:space:]]*\\\\Scoderef\\{([^}]*)\\}.*", docopt = "^[[:space:]]*\\\\SweaveOpts\\{([^}]*)\\}", 
    docexpr = "\\\\Sexpr\\{([^}]*)\\}", extension = "\\.[rsRS]tex$", 
    syntaxname = "^[[:space:]]*\\\\SweaveSyntax\\{([^}]*)\\}", 
    input = "^[[:space:]]*\\\\SweaveInput\\{([^}]*)\\}", trans = structure(list(
        doc = "\\\\end{Scode}", code = "\\\\begin{Scode}{\\1}", 
        coderef = "\\\\Scoderef{\\1}", docopt = "\\\\SweaveOpts{\\1}", 
        docexpr = "\\\\Sexpr{\\1}", extension = ".Stex", syntaxname = "\\\\SweaveSyntax{SweaveSyntaxLatex}", 
        input = "\\\\SweaveInput{\\1}"), .Names = c("doc", "code", 
    "coderef", "docopt", "docexpr", "extension", "syntaxname", 
    "input"))), .Names = c("doc", "code", "coderef", "docopt", 
"docexpr", "extension", "syntaxname", "input", "trans"), class = "SweaveSyntax")


packageName <- function (env = parent.frame()) 
{
    if (!is.environment(env)) 
        stop("'env' must be an environment")
    env <- topenv(env)
    if (!is.null(pn <- get0(".packageName", envir = env, inherits = FALSE))) 
        pn
    else if (identical(env, .BaseNamespaceEnv)) 
        "base"
}


bibentry <- function (bibtype, textVersion = NULL, header = NULL, footer = NULL, 
    key = NULL, ..., other = list(), mheader = NULL, mfooter = NULL) 
{
    BibTeX_names <- names(tools:::BibTeX_entry_field_db)
    args <- c(list(...), other)
    if (!length(args)) 
        return(structure(list(), class = "bibentry"))
    if (any(sapply(names(args), .is_not_nonempty_text))) 
        stop("all fields have to be named")
    args <- c(list(bibtype = bibtype, textVersion = textVersion, 
        header = header, footer = footer, key = key), list(...))
    args <- lapply(args, .listify)
    other <- lapply(other, .listify)
    max_length <- max(lengths(c(args, other)))
    args_length <- lengths(args)
    if (!all(args_length_ok <- args_length %in% c(1L, max_length))) 
        warning(gettextf("Not all arguments are of the same length, the following need to be recycled: %s", 
            paste(names(args)[!args_length_ok], collapse = ", ")), 
            domain = NA)
    args <- lapply(args, function(x) rep_len(x, max_length))
    other_length <- lengths(other)
    if (!all(other_length_ok <- other_length %in% c(1L, max_length))) 
        warning(gettextf("Not all arguments are of the same length, the following need to be recycled: %s", 
            paste(names(other)[!other_length_ok], collapse = ", ")), 
            domain = NA)
    other <- lapply(other, function(x) rep_len(x, max_length))
    bibentry1 <- function(bibtype, textVersion, header = NULL, 
        footer = NULL, key = NULL, ..., other = list()) {
        bibtype <- as.character(bibtype)
        stopifnot(length(bibtype) == 1L)
        pos <- match(tolower(bibtype), tolower(BibTeX_names))
        if (is.na(pos)) 
            stop(gettextf("%s has to be one of %s", sQuote("bibtype"), 
                paste(BibTeX_names, collapse = ", ")), domain = NA)
        bibtype <- BibTeX_names[pos]
        rval <- c(list(...), other)
        rval <- rval[!sapply(rval, .is_not_nonempty_text)]
        fields <- tolower(names(rval))
        names(rval) <- fields
        attr(rval, "bibtype") <- bibtype
        .bibentry_check_bibentry1(rval)
        pos <- fields %in% c("author", "editor")
        if (any(pos)) {
            for (i in which(pos)) rval[[i]] <- as.person(rval[[i]])
        }
        if (any(!pos)) {
            for (i in which(!pos)) rval[[i]] <- as.character(rval[[i]])
        }
        attr(rval, "key") <- if (is.null(key)) 
            NULL
        else as.character(key)
        if (!is.null(textVersion)) 
            attr(rval, "textVersion") <- as.character(textVersion)
        if (!.is_not_nonempty_text(header)) 
            attr(rval, "header") <- paste(header, collapse = "\n")
        if (!.is_not_nonempty_text(footer)) 
            attr(rval, "footer") <- paste(footer, collapse = "\n")
        return(rval)
    }
    rval <- lapply(seq_along(args$bibtype), function(i) do.call(bibentry1, 
        c(lapply(args, "[[", i), list(other = lapply(other, "[[", 
            i)))))
    if (!.is_not_nonempty_text(mheader)) 
        attr(rval, "mheader") <- paste(mheader, collapse = "\n")
    if (!.is_not_nonempty_text(mfooter)) 
        attr(rval, "mfooter") <- paste(mfooter, collapse = "\n")
    class(rval) <- "bibentry"
    rval
}


RtangleSetup <- function (file, syntax, output = NULL, annotate = TRUE, split = FALSE, 
    quiet = FALSE, drop.evalFALSE = FALSE, ...) 
{
    dots <- list(...)
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste0(prefix.string, ".R")
    }
    else prefix.string <- basename(sub("\\.[rsRS]$", "", output))
    if (!split) {
        if (identical(output, "stdout")) 
            output <- stdout()
        else if (identical(output, "stderr")) 
            output <- stderr()
        else {
            if (!quiet) 
                cat("Writing to file", output, "\n")
            output <- file(output, open = "w")
        }
        lines <- c(sprintf("R code from vignette source '%s'", 
            file), if (attr(file, "encoding") != "ASCII") sprintf("Encoding: %s", 
            localeToCharset()[1L]))
        lines <- c(paste("###", lines), "")
        writeLines(lines, output)
    }
    else {
        if (!quiet) 
            cat("Writing chunks to files ...\n")
        output <- NULL
    }
    options <- list(split = split, prefix = TRUE, prefix.string = prefix.string, 
        engine = "R", eval = TRUE, show.line.nos = FALSE)
    options$.defaults <- options
    options[names(dots)] <- dots
    options <- RweaveLatexOptions(options)
    list(output = output, annotate = annotate, options = options, 
        chunkout = list(), quiet = quiet, syntax = syntax, drop.evalFALSE = drop.evalFALSE)
}


findLineNum <- function (srcfile, line, nameonly = TRUE, envir = parent.frame(), 
    lastenv) 
{
    count <- 0
    result <- list()
    if (!inherits(srcfile, "srcfile")) {
        if (missing(line)) {
            line <- as.numeric(sub(".*#", "", srcfile))
            if (is.na(line)) 
                stop("Line number missing")
            srcfile <- sub("#[^#]*", "", srcfile)
        }
        srcfile <- srcfile(srcfile)
    }
    if (missing(lastenv)) {
        if (missing(envir)) 
            lastenv <- globalenv()
        else lastenv <- emptyenv()
    }
    if (!is.environment(envir)) 
        envir <- environment(envir)
    fns <- character()
    envirs <- list()
    e <- envir
    repeat {
        fns <- c(fns, lsf.str(envir = e, all = TRUE))
        oldlen <- length(envirs)
        length(envirs) <- length(fns)
        if (length(envirs) > oldlen) 
            for (i in seq.int(oldlen + 1, length(envirs))) envirs[[i]] <- e
        if (identical(e, lastenv) || identical(e, emptyenv())) 
            break
        e <- parent.env(e)
    }
    for (i in seq_along(fns)) {
        functionName <- fns[i]
        fn <- get(functionName, envir = envirs[[i]])
        loc <- fnLineNum(fn, srcfile = srcfile, line = line, 
            nameonly = nameonly)
        if (!is.null(loc)) {
            count <- count + 1
            result[[count]] <- c(list(name = functionName, env = envirs[[i]]), 
                loc)
        }
        gen <- tryCatch(methods::isGeneric(functionName, envirs[[i]], 
            fdef = fn), error = identity)
        if (isTRUE(gen)) {
            e1 <- environment(fn)$.AllMTable
            if (!is.null(e1)) {
                sigs <- ls(e1)
                for (j in seq_along(sigs)) {
                  sig <- sigs[j]
                  fn <- get(sig, e1)
                  if (typeof(fn) != "closure") 
                    next
                  loc <- fnLineNum(fn, srcfile = srcfile, line = line, 
                    nameonly = nameonly)
                  if (is.null(loc) && length(body(fn)) > 1 && 
                    length(body(fn)[[2]]) > 2 && typeof(body(fn)[[c(2, 
                    3)]]) == "closure") {
                    fn2 <- body(fn)[[c(2, 3)]]
                    loc <- fnLineNum(fn2, srcfile = srcfile, 
                      line = line, nameonly = nameonly)
                    if (!is.null(loc)) 
                      loc$at <- c(2, 3)
                  }
                  if (!is.null(loc)) {
                    count <- count + 1
                    result[[count]] <- c(list(name = functionName, 
                      env = envirs[[i]], signature = strsplit(sig, 
                        "#")[[1]]), loc)
                  }
                }
            }
        }
    }
    return(structure(result, class = "findLineNumResult"))
}


RtangleWritedoc <- function (object, chunk) 
{
    while (length(pos <- grep(object$syntax$docopt, chunk))) {
        opts <- sub(paste0(".*", object$syntax$docopt, ".*"), 
            "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options, 
            RweaveLatexOptions)
        chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }
    object
}


txtProgressBar <- function (min = 0, max = 1, initial = 0, char = "=", width = NA, 
    title, label, style = 1, file = "") 
{
    if (!identical(file, "") && !(inherits(file, "connection") && 
        isOpen(file))) 
        stop("'file' must be \"\" or an open connection object")
    if (!style %in% 1L:3L) 
        style <- 1
    .val <- initial
    .killed <- FALSE
    .nb <- 0L
    .pc <- -1L
    nw <- nchar(char, "w")
    if (is.na(width)) {
        width <- getOption("width")
        if (style == 3L) 
            width <- width - 10L
        width <- trunc(width/nw)
    }
    if (max <= min) 
        stop("must have 'max' > 'min'")
    up1 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb < nb) {
            cat(strrep(char, nb - .nb), file = file)
            flush.console()
        }
        else if (.nb > nb) {
            cat("\r", strrep(" ", .nb * nw), "\r", strrep(char, 
                nb), sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }
    up2 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        if (.nb <= nb) {
            cat("\r", strrep(char, nb), sep = "", file = file)
            flush.console()
        }
        else {
            cat("\r", strrep(" ", .nb * nw), "\r", strrep(char, 
                nb), sep = "", file = file)
            flush.console()
        }
        .nb <<- nb
    }
    up3 <- function(value) {
        if (!is.finite(value) || value < min || value > max) 
            return()
        .val <<- value
        nb <- round(width * (value - min)/(max - min))
        pc <- round(100 * (value - min)/(max - min))
        if (nb == .nb && pc == .pc) 
            return()
        cat(paste0("\r  |", strrep(" ", nw * width + 6)), file = file)
        cat(paste(c("\r  |", rep.int(char, nb), rep.int(" ", 
            nw * (width - nb)), sprintf("| %3d%%", pc)), collapse = ""), 
            file = file)
        flush.console()
        .nb <<- nb
        .pc <<- pc
    }
    getVal <- function() .val
    kill <- function() if (!.killed) {
        cat("\n", file = file)
        flush.console()
        .killed <<- TRUE
    }
    up <- switch(style, up1, up2, up3)
    up(initial)
    structure(list(getVal = getVal, up = up, kill = kill), class = "txtProgressBar")
}


promptData <- function (object, filename = NULL, name = NULL) 
{
    if (missing(name)) 
        name <- if (is.character(object)) 
            object
        else {
            name <- substitute(object)
            if (is.name(name)) 
                as.character(name)
            else stop("cannot determine a usable name")
        }
    if (is.null(filename)) 
        filename <- paste0(name, ".Rd")
    x <- if (!missing(object)) 
        object
    else {
        x <- get(name, envir = parent.frame())
    }
    if (is.data.frame(x)) {
        make_item_tag <- function(s) {
            if (grepl("^([[:alpha:]]|[.][[:alpha:]._])[[:alnum:]._]*$", 
                s)) {
                paste0("\\code{", s, "}")
            }
            else {
                paste0("\\samp{", gsub("([%{}])", "\\\\\\1", 
                  s), "}")
            }
        }
        fmt <- c("\\format{", paste("  A data frame with", nrow(x), 
            "observations on the following", ifelse(ncol(x) == 
                1, "variable.", paste(ncol(x), "variables."))), 
            "  \\describe{")
        for (i in names(x)) {
            xi <- x[[i]]
            fmt <- c(fmt, paste0("    \\item{", make_item_tag(i), 
                "}{", if (inherits(xi, "ordered")) {
                  paste("an", data.class(xi), "factor with levels", 
                    paste0("\\code{", levels(xi), "}", collapse = " < "), 
                    collapse = " ")
                } else if (inherits(xi, "factor")) {
                  paste("a factor with levels", paste0("\\code{", 
                    levels(xi), "}", collapse = " "), collapse = " ")
                } else if (is.vector(xi)) {
                  paste("a", data.class(xi), "vector")
                } else if (is.matrix(xi)) {
                  paste("a matrix with", ncol(xi), "columns")
                } else {
                  paste("a", data.class(xi))
                }, "}"))
        }
        fmt <- c(fmt, "  }", "}")
    }
    else {
        tf <- tempfile()
        on.exit(unlink(tf))
        sink(tf)
        str(object)
        sink()
        fmt <- c("\\format{", "  The format is:", scan(tf, "", 
            quiet = !getOption("verbose"), sep = "\n"), "}")
    }
    Rdtxt <- list(name = paste0("\\name{", name, "}"), aliases = paste0("\\alias{", 
        name, "}"), docType = "\\docType{data}", title = "\\title{\n%%   ~~ data name/kind ... ~~\n}", 
        description = c("\\description{", "%%  ~~ A concise (1-5 lines) description of the dataset. ~~", 
            "}"), usage = paste0("\\usage{data(\"", name, "\")}"), 
        format = fmt, details = c("\\details{", paste("%%  ~~ If necessary, more details than the", 
            "__description__ above ~~"), "}"), source = c("\\source{", 
            paste("%%  ~~ reference to a publication or URL", 
                "from which the data were obtained ~~"), "}"), 
        references = c("\\references{", "%%  ~~ possibly secondary sources and usages ~~", 
            "}"), examples = c("\\examples{", paste0("data(", 
            name, ")"), paste0("## maybe str(", name, ") ; plot(", 
            name, ") ..."), "}"), keywords = "\\keyword{datasets}")
    if (is.na(filename)) 
        return(Rdtxt)
    cat(unlist(Rdtxt), file = filename, sep = "\n")
    message(gettextf("Created file named %s.", sQuote(filename)), 
        "\n", gettext("Edit the file and move it to the appropriate directory."), 
        domain = NA)
    invisible(filename)
}


file_test <- tools::file_test # re-exported from tools package

getSrcFilename <- function (x, full.names = FALSE, unique = TRUE) 
{
    srcref <- getSrcref(x)
    if (is.list(srcref)) 
        result <- sapply(srcref, getSrcFilename, full.names, 
            unique)
    else {
        srcfile <- attr(srcref, "srcfile")
        if (is.null(srcfile)) 
            result <- character()
        else result <- srcfile$filename
    }
    result <- if (full.names) 
        result
    else basename(result)
    if (unique) 
        unique(result)
    else result
}


help <- function (topic, package = NULL, lib.loc = NULL, verbose = getOption("verbose"), 
    try.all.packages = getOption("help.try.all.packages"), help_type = getOption("help_type")) 
{
    types <- c("text", "html", "pdf")
    help_type <- if (!length(help_type)) 
        "text"
    else match.arg(tolower(help_type), types)
    if (!missing(package)) 
        if (is.name(y <- substitute(package))) 
            package <- as.character(y)
    if (missing(topic)) {
        if (!is.null(package)) {
            if (interactive() && help_type == "html") {
                port <- tools::startDynamicHelp(NA)
                if (port <= 0L) 
                  return(library(help = package, lib.loc = lib.loc, 
                    character.only = TRUE))
                browser <- if (.Platform$GUI == "AQUA") {
                  get("aqua.browser", envir = as.environment("tools:RGUI"))
                }
                else getOption("browser")
                browseURL(paste0("http://127.0.0.1:", port, "/library/", 
                  package, "/html/00Index.html"), browser)
                return(invisible())
            }
            else return(library(help = package, lib.loc = lib.loc, 
                character.only = TRUE))
        }
        if (!is.null(lib.loc)) 
            return(library(lib.loc = lib.loc))
        topic <- "help"
        package <- "utils"
        lib.loc <- .Library
    }
    ischar <- tryCatch(is.character(topic) && length(topic) == 
        1L, error = identity)
    if (inherits(ischar, "error")) 
        ischar <- FALSE
    if (!ischar) {
        reserved <- c("TRUE", "FALSE", "NULL", "Inf", "NaN", 
            "NA", "NA_integer_", "NA_real_", "NA_complex_", "NA_character_")
        stopic <- deparse(substitute(topic))
        if (!is.name(substitute(topic)) && !stopic %in% reserved) 
            stop("'topic' should be a name, length-one character vector or reserved word")
        topic <- stopic
    }
    paths <- index.search(topic, find.package(if (is.null(package)) 
        loadedNamespaces()
    else package, lib.loc, verbose = verbose))
    try.all.packages <- !length(paths) && is.logical(try.all.packages) && 
        !is.na(try.all.packages) && try.all.packages && is.null(package) && 
        is.null(lib.loc)
    if (try.all.packages) {
        for (lib in .libPaths()) {
            packages <- .packages(TRUE, lib)
            packages <- packages[is.na(match(packages, .packages()))]
            paths <- c(paths, index.search(topic, file.path(lib, 
                packages)))
        }
        paths <- paths[nzchar(paths)]
    }
    structure(unique(paths), call = match.call(), topic = topic, 
        tried_all_packages = try.all.packages, type = help_type, 
        class = "help_files_with_topic")
}


toLatex <- function (object, ...) 
UseMethod("toLatex")


installed.packages <- function (lib.loc = NULL, priority = NULL, noCache = FALSE, fields = NULL, 
    subarch = .Platform$r_arch) 
{
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    if (!is.null(priority)) {
        if (!is.character(priority)) 
            stop("'priority' must be character or NULL")
        if (any(b <- priority %in% "high")) 
            priority <- c(priority[!b], "recommended", "base")
    }
    fields <- .instPkgFields(fields)
    retval <- matrix(character(), 0L, 2L + length(fields))
    for (lib in lib.loc) {
        if (noCache) {
            ret0 <- .readPkgDesc(lib, fields)
            if (length(ret0)) 
                retval <- rbind(retval, ret0)
        }
        else {
            base <- paste(c(lib, fields), collapse = ",")
            enc <- sprintf("%d_%s", nchar(base), .Call(C_crc64, 
                base))
            dest <- file.path(tempdir(), paste0("libloc_", enc, 
                ".rds"))
            if (file.exists(dest) && file.mtime(dest) > file.mtime(lib) && 
                (val <- readRDS(dest))$base == base) 
                retval <- rbind(retval, val$value)
            else {
                ret0 <- .readPkgDesc(lib, fields)
                if (length(ret0)) {
                  retval <- rbind(retval, ret0)
                  saveRDS(list(base = base, value = ret0), dest)
                }
            }
        }
    }
    .fixupPkgMat(retval, fields, priority, subarch)
}


citFooter <- function (...) 
{
    rval <- paste(...)
    class(rval) <- "citationFooter"
    rval
}


unstack <- function (x, ...) 
UseMethod("unstack")


mirror2html <- function (mirrors = NULL, file = "mirrors.html", head = "mirrors-head.html", 
    foot = "mirrors-foot.html") 
{
    if (is.null(mirrors)) {
        mirrors <- getCRANmirrors(all = FALSE, local.only = TRUE)
    }
    mirrors$Host <- gsub("&", "&amp;", mirrors$Host)
    z <- NULL
    if (file.exists(head)) 
        z <- readLines(head)
    z <- c(z, "<dl>")
    for (country in unique(mirrors$Country)) {
        m <- mirrors[mirrors$Country == country, ]
        z <- c(z, paste0("<dt>", country, "</dt>"), "<dd>", sprintf("<table border=0 width=\"90%%\" summary=\"%s\">", 
            country))
        for (k in seq_len(nrow(m))) {
            z <- c(z, "<tr>", "<td width=\"45%\">", sprintf("<a href=\"%s\" target=\"_top\">%s</a>", 
                m$URL[k], m$URL[k]), "</td>\n", "<td>", m$Host[k], 
                "</td>", "</tr>")
        }
        z <- c(z, "</table>", "</dd>")
    }
    z <- c(z, "</dl>")
    if (file.exists(foot)) 
        z <- c(z, readLines(foot))
    if (file != "") 
        writeLines(z, file)
    invisible(z)
}


debugcall <- function (call, once = FALSE) 
{
    stopifnot(length(once) == 1L, is.logical(once), !is.na(once))
    call <- substitute(call)
    .debugcall(call, if (once) 
        debugonce
    else debug)
    invisible(call)
}


data.entry <- function (..., Modes = NULL, Names = NULL) 
{
    tmp1 <- de(..., Modes = Modes, Names = Names)
    j <- 1L
    nn <- names(tmp1)
    for (i in nn) {
        assign(i, tmp1[[j]], envir = .GlobalEnv)
        j <- j + 1L
    }
    if (j == 1L) 
        warning("did not assign() anything")
    invisible(nn)
}


str <- function (object, ...) 
UseMethod("str")


read.fortran <- function (file, format, ..., as.is = TRUE, colClasses = NA) 
{
    processFormat <- function(format) {
        format <- toupper(format)
        template <- "^([0-9]*)([FXAI])([0-9]*)\\.?([0-9]*)"
        reps <- as.numeric(sub(template, "\\1", format))
        types <- sub(template, "\\2", format)
        lengths <- as.numeric(sub(template, "\\3", format))
        decimals <- as.numeric(sub(template, "\\4", format))
        reps[is.na(reps)] <- 1L
        lengths[is.na(lengths) & types == "X"] <- 1L
        charskip <- types == "X"
        lengths[charskip] <- reps[charskip] * lengths[charskip]
        reps[charskip] <- 1
        if (anyNA(lengths)) 
            stop("missing lengths for some fields")
        lengths <- rep(lengths, reps)
        types <- rep(types, reps)
        decimals <- rep(decimals, reps)
        types <- match(types, c("F", "D", "X", "A", "I"))
        if (any(!is.na(decimals) & types > 2L)) 
            stop("invalid format")
        colClasses <- c("numeric", "numeric", NA, if (as.is) "character" else NA, 
            "integer")[types]
        colClasses <- colClasses[!(types == 3L)]
        decimals <- decimals[!(types == 3L)]
        lengths[types == 3] <- -lengths[types == 3L]
        list(lengths, colClasses, decimals)
    }
    if (is.list(format)) {
        ff <- lapply(format, processFormat)
        widths <- lapply(ff, "[[", 1L)
        if (is.na(colClasses)) 
            colClasses <- do.call("c", lapply(ff, "[[", 2L))
        decimals <- do.call("c", lapply(ff, "[[", 3L))
    }
    else {
        ff <- processFormat(format)
        widths <- ff[[1L]]
        if (is.na(colClasses)) 
            colClasses <- ff[[2L]]
        decimals <- ff[[3L]]
    }
    rval <- read.fwf(file, widths = widths, ..., colClasses = colClasses)
    for (i in which(!is.na(decimals))) rval[, i] <- rval[, i] * 
        (10^-decimals[i])
    rval
}


dataentry <- function (data, modes) 
{
    check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
    msg <- "dataentry() should not be used in examples etc"
    if (identical(check, "stop")) 
        stop(msg, domain = NA)
    else if (identical(check, "warn")) 
        warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)
    if (!is.list(data) || !length(data) || !all(sapply(data, 
        is.vector))) 
        stop("invalid 'data' argument")
    if (!is.list(modes) || (length(modes) && !all(sapply(modes, 
        is.character)))) 
        stop("invalid 'modes' argument")
    if (grepl("darwin", R.version$os)) 
        check_for_XQuartz()
    .External2(C_dataentry, data, modes)
}


citation <- function (package = "base", lib.loc = NULL, auto = NULL) 
{
    if (!is.null(auto) && !is.logical(auto) && !any(is.na(match(c("Package", 
        "Version", "Title"), names(meta <- as.list(auto))))) && 
        !all(is.na(match(c("Authors@R", "Author"), names(meta))))) {
        auto_was_meta <- TRUE
        package <- meta$Package
    }
    else {
        auto_was_meta <- FALSE
        dir <- system.file(package = package, lib.loc = lib.loc)
        if (dir == "") 
            stop(gettextf("package %s not found", sQuote(package)), 
                domain = NA)
        meta <- packageDescription(pkg = package, lib.loc = dirname(dir))
        citfile <- file.path(dir, "CITATION")
        test <- file_test("-f", citfile)
        if (!test) {
            citfile <- file.path(dir, "inst", "CITATION")
            test <- file_test("-f", citfile)
        }
        if (is.null(auto)) 
            auto <- !test
        if (!auto) {
            return(readCitationFile(citfile, meta))
        }
    }
    if ((!is.null(meta$Priority)) && (meta$Priority == "base")) {
        cit <- citation("base", auto = FALSE)
        attr(cit, "mheader")[1L] <- paste0("The ", sQuote(package), 
            " package is part of R.  ", attr(cit, "mheader")[1L])
        return(.citation(cit))
    }
    year <- sub("-.*", "", meta$`Date/Publication`)
    if (!length(year)) {
        if (is.null(meta$Date)) {
            warning(gettextf("no date field in DESCRIPTION file of package %s", 
                sQuote(package)), domain = NA)
        }
        else {
            date <- trimws(as.vector(meta$Date))[1L]
            date <- strptime(date, "%Y-%m-%d", tz = "GMT")
            if (!is.na(date)) 
                year <- format(date, "%Y")
        }
    }
    if (!length(year)) {
        date <- as.POSIXlt(sub(";.*", "", trimws(meta$Packaged)[1L]))
        if (!is.na(date)) 
            year <- format(date, "%Y")
    }
    if (!length(year)) {
        warning(gettextf("could not determine year for %s from package DESCRIPTION file", 
            sQuote(package)), domain = NA)
        year <- NA_character_
    }
    author <- meta$`Authors@R`
    if (length(author)) {
        aar <- .read_authors_at_R_field(author)
        author <- Filter(function(e) {
            !(is.null(e$given) && is.null(e$family)) && !is.na(match("aut", 
                e$role))
        }, aar)
        if (!length(author)) 
            author <- Filter(function(e) {
                !(is.null(e$given) && is.null(e$family)) && !is.na(match("cre", 
                  e$role))
            }, aar)
    }
    if (length(author)) {
        has_authors_at_R_field <- TRUE
    }
    else {
        has_authors_at_R_field <- FALSE
        author <- as.personList(meta$Author)
    }
    z <- list(title = paste0(package, ": ", meta$Title), author = author, 
        year = year, note = paste("R package version", meta$Version))
    if (identical(meta$Repository, "CRAN")) 
        z$url <- sprintf("https://CRAN.R-project.org/package=%s", 
            package)
    if (identical(meta$Repository, "R-Forge")) {
        z$url <- if (!is.null(rfp <- meta$"Repository/R-Forge/Project")) 
            sprintf("https://R-Forge.R-project.org/projects/%s/", 
                rfp)
        else "https://R-Forge.R-project.org/"
        if (!is.null(rfr <- meta$"Repository/R-Forge/Revision")) 
            z$note <- paste(z$note, rfr, sep = "/r")
    }
    if (!length(z$url) && !is.null(url <- meta$URL)) {
        if (grepl("[, ]", url)) 
            z$note <- url
        else z$url <- url
    }
    header <- if (!auto_was_meta) {
        gettextf("To cite package %s in publications use:", sQuote(package))
    }
    else NULL
    footer <- if (!has_authors_at_R_field && !auto_was_meta) {
        gettextf("ATTENTION: This citation information has been auto-generated from the package DESCRIPTION file and may need manual editing, see %s.", 
            sQuote("help(\"citation\")"))
    }
    else NULL
    author <- format(z$author, include = c("given", "family"))
    if (length(author) > 1L) 
        author <- paste(paste(head(author, -1L), collapse = ", "), 
            tail(author, 1L), sep = " and ")
    rval <- bibentry(bibtype = "Manual", textVersion = paste0(author, 
        " (", z$year, "). ", z$title, ". ", z$note, ". ", z$url), 
        header = header, footer = footer, other = z)
    .citation(rval)
}


emacs <- function (name = NULL, file = "") 
edit.default(name, file, editor = "emacs")


head <- function (x, ...) 
UseMethod("head")


promptImport <- function (object, filename = NULL, name = NULL, importedFrom = NULL, 
    importPage = name, ...) 
{
    if (missing(name)) 
        name <- if (is.character(object)) 
            object
        else {
            name <- substitute(object)
            if (is.name(name)) 
                as.character(name)
            else if (is.language(name) && length(name) == 3 && 
                identical(name[[1]], as.name("::"))) 
                as.character(name[[3]])
            else stop("cannot determine a usable name")
        }
    if (is.null(filename)) 
        filename <- paste0(name, ".Rd")
    x <- if (!missing(object)) 
        object
    else {
        x <- get(name, envir = parent.frame())
    }
    if (is.null(importedFrom)) {
        if (is.function(x)) 
            importedFrom <- getNamespaceName(environment(x))
        else stop("cannot determine import name")
    }
    Rdtxt <- list(name = paste0("\\name{", name, "}"), aliases = paste0("\\alias{", 
        name, "}"), docType = "\\docType{import}", title = paste0("\\title{Import from package \\pkg{", 
        importedFrom, "}}"), description = c("\\description{", 
        paste0("The \\code{", name, "} object is imported from package \\pkg{", 
            importedFrom, "}."), paste0("Help is available here:  \\code{\\link[", 
            importedFrom, ":", importPage, "]{", importedFrom, 
            "::", importPage, "}}."), "}"))
    if (is.na(filename)) 
        return(Rdtxt)
    cat(unlist(Rdtxt), file = filename, sep = "\n")
    message(gettextf("Created file named %s.", sQuote(filename)), 
        "\n", gettext("Edit the file and move it to the appropriate directory."), 
        domain = NA)
}


removeSource <- function (fn) 
{
    stopifnot(is.function(fn))
    if (is.primitive(fn)) 
        return(fn)
    attr(fn, "srcref") <- NULL
    attr(body(fn), "wholeSrcref") <- NULL
    attr(body(fn), "srcfile") <- NULL
    recurse <- function(part) {
        if (is.name(part)) 
            return(part)
        attr(part, "srcref") <- NULL
        attr(part, "wholeSrcref") <- NULL
        attr(part, "srcfile") <- NULL
        if (is.language(part) && is.recursive(part)) {
            for (i in seq_along(part)) part[[i]] <- recurse(part[[i]])
        }
        part
    }
    body(fn) <- recurse(body(fn))
    fn
}


make.packages.html <- function (lib.loc = .libPaths(), temp = FALSE, verbose = TRUE, 
    docdir = R.home("doc")) 
{
    add_lib_index <- function(libs) {
        cat("<div align=\"left\">\n<ul>\n", file = out)
        for (i in seq_along(libs)) {
            nm <- libs[i]
            if (nm == .Library) {
                cat("<li>Contents of the <a href=\"#lib-", i, 
                  "\">", "standard</a> library</li>\n", sep = "", 
                  file = out)
            }
            else {
                cat("<li>Contents of <a href=\"#lib-", i, "\">", 
                  nm, "</a></li>\n", sep = "", file = out)
            }
        }
        cat("</ul>\n</div>\n", file = out)
    }
    WINDOWS <- .Platform$OS.type == "windows"
    f.tg <- if (temp) {
        dir.create(file.path(tempdir(), ".R/doc/html"), recursive = TRUE, 
            showWarnings = FALSE)
        file.path(tempdir(), ".R/doc/html/packages.html")
    }
    else file.path(docdir, "html", "packages.html")
    op <- file.path(tempdir(), ".R/doc/html/libPaths.rds")
    if (temp && file.exists(f.tg) && file.exists(op)) {
        if (identical(lib.loc, readRDS(op))) {
            dates <- file.mtime(c(f.tg, lib.loc))
            if (which.max(dates) == 1L) 
                return(TRUE)
        }
    }
    if (!file.create(f.tg)) {
        warning("cannot update HTML package index")
        return(FALSE)
    }
    if (verbose) {
        message("Making 'packages.html' ...", appendLF = FALSE, 
            domain = NA)
        flush.console()
    }
    file.append(f.tg, file.path(R.home("doc"), "html", "packages-head-utf8.html"))
    out <- file(f.tg, open = "a")
    on.exit(close(out))
    if (WINDOWS) {
        rh <- chartr("\\", "/", R.home())
        drive <- substring(rh, 1L, 2L)
    }
    pkgs <- vector("list", length(lib.loc))
    names(pkgs) <- lib.loc
    for (lib in lib.loc) {
        pg <- .packages(all.available = TRUE, lib.loc = lib)
        pkgs[[lib]] <- pg[order(toupper(pg), pg)]
    }
    if (WINDOWS) {
        tot <- sum(lengths(pkgs))
        if (verbose) {
            pb <- winProgressBar("R: creating packages.html", 
                max = tot)
            on.exit(close(pb), add = TRUE)
        }
        npkgs <- 0L
    }
    if (length(lib.loc) > 1L) 
        add_lib_index(lib.loc)
    for (ii in seq_along(lib.loc)) {
        lib <- lib.loc[ii]
        libname <- if (identical(lib, .Library)) 
            "the standard library"
        else if (WINDOWS) 
            chartr("/", "\\", lib)
        else lib
        cat("<p><h3 id=\"lib-", ii, "\">Packages in ", libname, 
            "</h3>\n", sep = "", file = out)
        lib0 <- "../../library"
        if (!temp) {
            if (WINDOWS) {
                if (is.na(pmatch(rh, lib))) {
                  lib0 <- if (substring(lib, 2L, 2L) != ":") 
                    paste0(drive, lib)
                  else lib
                  lib0 <- paste0("file:///", URLencode(lib0))
                }
            }
            else {
                if (lib != .Library) 
                  lib0 <- paste0("file:///", URLencode(lib))
            }
        }
        pg <- pkgs[[lib]]
        use_alpha <- (length(pg) > 100)
        first <- toupper(substr(pg, 1, 1))
        nm <- sort(names(table(first)))
        if (use_alpha) {
            writeLines("<p align=\"center\">", out)
            writeLines(paste0("<a href=\"#pkgs-", nm, "\">", 
                nm, "</a>"), out)
            writeLines("</p>\n", out)
        }
        cat("<p><table width=\"100%\" summary=\"R Package list\">\n", 
            file = out)
        for (a in nm) {
            if (use_alpha) 
                cat("<tr id=\"pkgs-", a, "\"> <td></td>\n", sep = "", 
                  file = out)
            for (i in pg[first == a]) {
                title <- packageDescription(i, lib.loc = lib, 
                  fields = "Title", encoding = "UTF-8")
                if (is.na(title)) 
                  title <- "-- Title is missing --"
                cat("<tr align=\"left\" valign=\"top\" id=\"lib-", 
                  i, "\">\n", "<td width=\"25%\"><a href=\"", 
                  lib0, "/", i, "/html/00Index.html\">", i, "</a></td><td>", 
                  title, "</td></tr>\n", file = out, sep = "")
                if (WINDOWS) {
                  npkgs <- npkgs + 1L
                  if (verbose) 
                    setWinProgressBar(pb, npkgs)
                }
            }
        }
        cat("</table>\n\n", file = out)
    }
    if (length(lib.loc) > 1L) 
        add_lib_index(lib.loc)
    cat("</body></html>\n", file = out)
    if (verbose) {
        message(" ", "done")
        flush.console()
    }
    if (temp) 
        saveRDS(lib.loc, op)
    invisible(TRUE)
}


RweaveChunkPrefix <- function (options) 
{
    if (!is.null(options$label)) {
        if (options$prefix) 
            paste0(options$prefix.string, "-", options$label)
        else options$label
    }
    else paste0(options$prefix.string, "-", formatC(options$chunknr, 
        flag = "0", width = 3))
}


page <- function (x, method = c("dput", "print"), ...) 
{
    local.file.show <- function(file, title = subx, delete.file = TRUE, 
        pager = getOption("pager"), ...) file.show(file, title = title, 
        delete.file = delete.file, pager = pager)
    local.dput <- function(x, file, title, delete.file, pager, 
        ...) dput(x, file, ...)
    local.print <- function(x, title, delete.file, pager, ...) print(x, 
        ...)
    if (is.character(x) && length(x) == 1L) {
        subx <- x
        parent <- parent.frame()
        if (exists(subx, envir = parent)) 
            x <- get(subx, envir = parent)
        else stop(gettextf("no object named '%s' to show", x), 
            domain = NA)
    }
    else {
        subx <- deparse(substitute(x))
    }
    file <- tempfile("Rpage.")
    if (match.arg(method) == "dput") 
        local.dput(x, file, ...)
    else {
        sink(file)
        local.print(x, ...)
        sink()
    }
    local.file.show(file, ...)
}


aspell_write_personal_dictionary_file <- function (x, out, language = "en", program = NULL) 
{
    if (inherits(x, "aspell")) 
        x <- sort(unique(x$Original))
    program <- aspell_find_program(program)
    if (is.na(program)) 
        stop("No suitable spell check program found.")
    if (names(program) == "aspell") {
        header <- sprintf("personal_ws-1.1 %s %d UTF-8", language, 
            length(x))
        x <- enc2utf8(x)
    }
    else {
        header <- NULL
    }
    writeLines(c(header, x), out, useBytes = TRUE)
}


formatOL <- function (x, type = "arabic", offset = 0, start = 1, width = 0.9 * 
    getOption("width")) 
{
    if (!length(x)) 
        return(character())
    type_tokens <- c("1", "A", "a", "I", "i")
    type_full_names <- c("arabic", "Alph", "alph", "Roman", "roman")
    type <- match.arg(type, c(type_tokens, type_full_names))
    if (nchar(type, "b") > 1L) 
        type <- type_tokens[match(type, type_full_names)]
    len <- length(x)
    labels <- seq.int(start[1L], length.out = len)
    upper <- labels[len]
    if (type %in% c("A", "a")) {
        if (upper > 26L) 
            stop(gettextf("too many list items (at most up to %d)", 
                26L), domain = NA)
        labels <- if (type == "A") 
            LETTERS[labels]
        else letters[labels]
    }
    else if (type %in% c("I", "i")) {
        if (upper > 3899L) 
            stop(gettextf("too many list items (at most up to %d)", 
                3899L), domain = NA)
        labels <- as.character(as.roman(labels))
        if (type == "i") 
            labels <- tolower(labels)
    }
    .format_rl_table(sprintf("%s.", labels), x, offset, width)
}


RweaveLatexWritedoc <- function (object, chunk) 
{
    linesout <- attr(chunk, "srclines")
    filenumout <- attr(chunk, "srcFilenum")
    if (length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk))) 
        object$havesty <- TRUE
    if (!object$havesty) {
        begindoc <- "^[[:space:]]*\\\\begin\\{document\\}"
        which <- grep(begindoc, chunk)
        if (length(which)) {
            chunk[which] <- sub(begindoc, paste("\\\\usepackage{", 
                object$styfile, "}\n\\\\begin{document}", sep = ""), 
                chunk[which])
            idx <- c(1L:which, which, seq(from = which + 1L, 
                length.out = length(linesout) - which))
            linesout <- linesout[idx]
            filenumout <- filenumout[idx]
            object$havesty <- TRUE
        }
    }
    while (length(pos <- grep(object$syntax$docexpr, chunk))) {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc, cmdloc + attr(cmdloc, 
            "match.length") - 1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if (object$options$eval) {
            val <- tryCatch(as.character(eval(parse(text = cmd), 
                envir = .GlobalEnv)), error = function(e) {
                filenum <- attr(chunk, "srcFilenum")[pos[1L]]
                filename <- attr(chunk, "srcFilenames")[filenum]
                location <- paste0(basename(filename), ":", attr(chunk, 
                  "srclines")[pos[1L]])
                stop("at ", location, ", ", conditionMessage(e), 
                  domain = NA, call. = FALSE)
            })
            if (length(val) == 0L) 
                val <- ""
        }
        else val <- paste0("\\\\verb#<<", cmd, ">>#")
        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }
    while (length(pos <- grep(object$syntax$docopt, chunk))) {
        opts <- sub(paste0(".*", object$syntax$docopt, ".*"), 
            "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options, 
            RweaveLatexOptions)
        if (isTRUE(object$options$concordance) && !object$haveconcordance) {
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste0(prefix, ".tex")
            chunk[pos[1L]] <- sub(object$syntax$docopt, paste0("\\\\input{", 
                prefix, "}"), chunk[pos[1L]])
            object$haveconcordance <- TRUE
        }
        else chunk[pos[1L]] <- sub(object$syntax$docopt, "", 
            chunk[pos[1L]])
    }
    cat(chunk, sep = "\n", file = object$output)
    object$linesout <- c(object$linesout, linesout)
    object$filenumout <- c(object$filenumout, filenumout)
    object
}


memory.limit <- function (size = NA) 
{
    warning("'memory.limit()' is Windows-specific", call. = FALSE)
    Inf
}


find <- function (what, mode = "any", numeric = FALSE, simple.words = TRUE) 
{
    stopifnot(is.character(what))
    if (length(what) > 1L) {
        warning("elements of 'what' after the first will be ignored")
        what <- what[1L]
    }
    len.s <- length(sp <- search())
    ind <- logical(len.s)
    check.mode <- mode != "any"
    for (i in 1L:len.s) {
        if (simple.words) {
            found <- what %in% ls(pos = i, all.names = TRUE)
            if (found && check.mode) 
                found <- exists(what, where = i, mode = mode, 
                  inherits = FALSE)
            ind[i] <- found
        }
        else {
            li <- ls(pos = i, pattern = what, all.names = TRUE)
            li <- grep("^[.](__|C_|F_)", li, invert = TRUE, value = TRUE)
            if (sp[i] == "package:base") 
                li <- li[!li %in% .dot_internals]
            ll <- length(li)
            if (ll > 0 && check.mode) {
                mode.ok <- sapply(li, exists, where = i, mode = mode, 
                  inherits = FALSE)
                ll <- sum(mode.ok)
                if (ll >= 2) 
                  warning(sprintf(ngettext(ll, "%d occurrence in %s", 
                    "%d occurrences in %s"), ll, sp[i]), domain = NA)
            }
            ind[i] <- ll > 0L
        }
    }
    if (numeric) 
        structure(which(ind), names = sp[ind])
    else sp[ind]
}


pico <- function (name = NULL, file = "") 
edit.default(name, file, editor = "pico")


fixInNamespace <- function (x, ns, pos = -1, envir = as.environment(pos), ...) 
{
    subx <- substitute(x)
    if (is.name(subx)) 
        subx <- deparse(subx)
    if (!is.character(subx) || length(subx) != 1L) 
        stop("'fixInNamespace' requires a name")
    if (missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if (is.null(nm) || substr(nm, 1L, 8L) != "package:") 
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    }
    else ns <- asNamespace(ns)
    x <- edit(get(subx, envir = ns, inherits = FALSE), ...)
    assignInNamespace(subx, x, ns)
}


adist <- function (x, y = NULL, costs = NULL, counts = FALSE, fixed = TRUE, 
    partial = !fixed, ignore.case = FALSE, useBytes = FALSE) 
{
    bytesToInt <- function(x) {
        if (is.na(x)) 
            return(NA_integer_)
        as.integer(charToRaw(x))
    }
    costs <- .amatch_costs(costs)
    nmx <- names(x)
    x <- as.character(x)
    names(x) <- nmx
    if (!is.null(y)) {
        nmy <- names(y)
        y <- as.character(y)
        names(y) <- nmy
    }
    if (!identical(fixed, FALSE) && !identical(partial, TRUE)) {
        ex <- Encoding(x)
        useBytes <- identical(useBytes, TRUE) || any(ex == "bytes")
        if (!is.null(y)) {
            ey <- Encoding(y)
            useBytes <- useBytes || any(ey == "bytes")
        }
        if (useBytes) {
            x <- lapply(x, bytesToInt)
            y <- if (is.null(y)) {
                x
            }
            else {
                lapply(y, bytesToInt)
            }
        }
        else {
            ignore.case <- identical(ignore.case, TRUE)
            x <- if (ignore.case) {
                lapply(tolower(enc2utf8(x)), utf8ToInt)
            }
            else {
                lapply(enc2utf8(x), utf8ToInt)
            }
            y <- if (is.null(y)) {
                x
            }
            else if (ignore.case) {
                lapply(tolower(enc2utf8(y)), utf8ToInt)
            }
            else {
                lapply(enc2utf8(y), utf8ToInt)
            }
        }
    }
    else {
        if (is.null(y)) {
            y <- x
        }
        costs <- as.integer(costs)
    }
    .Internal(adist(x, y, costs, counts, fixed, partial, ignore.case, 
        useBytes))
}


example <- function (topic, package = NULL, lib.loc = NULL, character.only = FALSE, 
    give.lines = FALSE, local = FALSE, echo = TRUE, verbose = getOption("verbose"), 
    setRNG = FALSE, ask = getOption("example.ask"), prompt.prefix = abbreviate(topic, 
        6), run.dontrun = FALSE, run.donttest = interactive()) 
{
    if (!character.only) {
        topic <- substitute(topic)
        if (!is.character(topic)) 
            topic <- deparse(topic)[1L]
    }
    pkgpaths <- find.package(package, lib.loc, verbose = verbose)
    file <- index.search(topic, pkgpaths, TRUE)
    if (!length(file)) {
        warning(gettextf("no help found for %s", sQuote(topic)), 
            domain = NA)
        return(invisible())
    }
    packagePath <- dirname(dirname(file))
    pkgname <- basename(packagePath)
    lib <- dirname(packagePath)
    tf <- tempfile("Rex")
    tools::Rd2ex(.getHelpFile(file), tf, commentDontrun = !run.dontrun, 
        commentDonttest = !run.donttest)
    if (!file.exists(tf)) {
        if (give.lines) 
            return(character())
        warning(gettextf("%s has a help file but no examples", 
            sQuote(topic)), domain = NA)
        return(invisible())
    }
    on.exit(unlink(tf))
    if (give.lines) 
        return(readLines(tf))
    if (pkgname != "base") 
        library(pkgname, lib.loc = lib, character.only = TRUE)
    if (!is.logical(setRNG) || setRNG) {
        if ((exists(".Random.seed", envir = .GlobalEnv))) {
            oldSeed <- get(".Random.seed", envir = .GlobalEnv)
            on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv), 
                add = TRUE)
        }
        else {
            oldRNG <- RNGkind()
            on.exit(RNGkind(oldRNG[1L], oldRNG[2L]), add = TRUE)
        }
        if (is.logical(setRNG)) {
            RNGkind("default", "default")
            set.seed(1)
        }
        else eval(setRNG)
    }
    zz <- readLines(tf, n = 1L)
    skips <- 0L
    if (echo) {
        zcon <- file(tf, open = "rt")
        while (length(zz) && !length(grep("^### \\*\\*", zz))) {
            skips <- skips + 1L
            zz <- readLines(zcon, n = 1L)
        }
        close(zcon)
    }
    if (ask == "default") 
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)
    if (ask) {
        if (.Device != "null device") {
            oldask <- grDevices::devAskNewPage(ask = TRUE)
            if (!oldask) 
                on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
        }
        op <- options(device.ask.default = TRUE)
        on.exit(options(op), add = TRUE)
    }
    source(tf, local, echo = echo, prompt.echo = paste0(prompt.prefix, 
        getOption("prompt")), continue.echo = paste0(prompt.prefix, 
        getOption("continue")), verbose = verbose, max.deparse.length = Inf, 
        encoding = "UTF-8", skip.echo = skips, keep.source = TRUE)
}


data <- function (..., list = character(), package = NULL, lib.loc = NULL, 
    verbose = getOption("verbose"), envir = .GlobalEnv) 
{
    fileExt <- function(x) {
        db <- grepl("\\.[^.]+\\.(gz|bz2|xz)$", x)
        ans <- sub(".*\\.", "", x)
        ans[db] <- sub(".*\\.([^.]+\\.)(gz|bz2|xz)$", "\\1\\2", 
            x[db])
        ans
    }
    names <- c(as.character(substitute(list(...))[-1L]), list)
    if (!is.null(package)) {
        if (!is.character(package)) 
            stop("'package' must be a character string or NULL")
        if (any(package %in% "base")) 
            warning("datasets have been moved from package 'base' to package 'datasets'")
        if (any(package %in% "stats")) 
            warning("datasets have been moved from package 'stats' to package 'datasets'")
        package[package %in% c("base", "stats")] <- "datasets"
    }
    paths <- find.package(package, lib.loc, verbose = verbose)
    if (is.null(lib.loc)) 
        paths <- c(path.package(package, TRUE), if (!length(package)) getwd(), 
            paths)
    paths <- unique(normalizePath(paths[file.exists(paths)]))
    paths <- paths[dir.exists(file.path(paths, "data"))]
    dataExts <- tools:::.make_file_exts("data")
    if (length(names) == 0L) {
        db <- matrix(character(), nrow = 0L, ncol = 4L)
        for (path in paths) {
            entries <- NULL
            packageName <- if (file_test("-f", file.path(path, 
                "DESCRIPTION"))) 
                basename(path)
            else "."
            if (file_test("-f", INDEX <- file.path(path, "Meta", 
                "data.rds"))) {
                entries <- readRDS(INDEX)
            }
            else {
                dataDir <- file.path(path, "data")
                entries <- tools::list_files_with_type(dataDir, 
                  "data")
                if (length(entries)) {
                  entries <- unique(tools::file_path_sans_ext(basename(entries)))
                  entries <- cbind(entries, "")
                }
            }
            if (NROW(entries)) {
                if (is.matrix(entries) && ncol(entries) == 2L) 
                  db <- rbind(db, cbind(packageName, dirname(path), 
                    entries))
                else warning(gettextf("data index for package %s is invalid and will be ignored", 
                  sQuote(packageName)), domain = NA, call. = FALSE)
            }
        }
        colnames(db) <- c("Package", "LibPath", "Item", "Title")
        footer <- if (missing(package)) 
            paste0("Use ", sQuote(paste("data(package =", ".packages(all.available = TRUE))")), 
                "\n", "to list the data sets in all *available* packages.")
        else NULL
        y <- list(title = "Data sets", header = NULL, results = db, 
            footer = footer)
        class(y) <- "packageIQR"
        return(y)
    }
    paths <- file.path(paths, "data")
    for (name in names) {
        found <- FALSE
        for (p in paths) {
            if (file_test("-f", file.path(p, "Rdata.rds"))) {
                rds <- readRDS(file.path(p, "Rdata.rds"))
                if (name %in% names(rds)) {
                  found <- TRUE
                  if (verbose) 
                    message(sprintf("name=%s:\t found in Rdata.rds", 
                      name), domain = NA)
                  thispkg <- sub(".*/([^/]*)/data$", "\\1", p)
                  thispkg <- sub("_.*$", "", thispkg)
                  thispkg <- paste0("package:", thispkg)
                  objs <- rds[[name]]
                  lazyLoad(file.path(p, "Rdata"), envir = envir, 
                    filter = function(x) x %in% objs)
                  break
                }
                else if (verbose) 
                  message(sprintf("name=%s:\t NOT found in names() of Rdata.rds, i.e.,\n\t%s\n", 
                    name, paste(names(rds), collapse = ",")), 
                    domain = NA)
            }
            if (file_test("-f", file.path(p, "Rdata.zip"))) {
                warning("zipped data found for package ", sQuote(basename(dirname(p))), 
                  ".\nThat is defunct, so please re-install the package.", 
                  domain = NA)
                if (file_test("-f", fp <- file.path(p, "filelist"))) 
                  files <- file.path(p, scan(fp, what = "", quiet = TRUE))
                else {
                  warning(gettextf("file 'filelist' is missing for directory %s", 
                    sQuote(p)), domain = NA)
                  next
                }
            }
            else {
                files <- list.files(p, full.names = TRUE)
            }
            files <- files[grep(name, files, fixed = TRUE)]
            if (length(files) > 1L) {
                o <- match(fileExt(files), dataExts, nomatch = 100L)
                paths0 <- dirname(files)
                paths0 <- factor(paths0, levels = unique(paths0))
                files <- files[order(paths0, o)]
            }
            if (length(files)) {
                for (file in files) {
                  if (verbose) 
                    message("name=", name, ":\t file= ...", .Platform$file.sep, 
                      basename(file), "::\t", appendLF = FALSE, 
                      domain = NA)
                  ext <- fileExt(file)
                  if (basename(file) != paste0(name, ".", ext)) 
                    found <- FALSE
                  else {
                    found <- TRUE
                    zfile <- file
                    zipname <- file.path(dirname(file), "Rdata.zip")
                    if (file.exists(zipname)) {
                      Rdatadir <- tempfile("Rdata")
                      dir.create(Rdatadir, showWarnings = FALSE)
                      topic <- basename(file)
                      rc <- .External(C_unzip, zipname, topic, 
                        Rdatadir, FALSE, TRUE, FALSE, FALSE)
                      if (rc == 0L) 
                        zfile <- file.path(Rdatadir, topic)
                    }
                    if (zfile != file) 
                      on.exit(unlink(zfile))
                    switch(ext, R = , r = {
                      library("utils")
                      sys.source(zfile, chdir = TRUE, envir = envir)
                    }, RData = , rdata = , rda = load(zfile, 
                      envir = envir), TXT = , txt = , tab = , 
                      tab.gz = , tab.bz2 = , tab.xz = , txt.gz = , 
                      txt.bz2 = , txt.xz = assign(name, read.table(zfile, 
                        header = TRUE, as.is = FALSE), envir = envir), 
                      CSV = , csv = , csv.gz = , csv.bz2 = , 
                      csv.xz = assign(name, read.table(zfile, 
                        header = TRUE, sep = ";", as.is = FALSE), 
                        envir = envir), found <- FALSE)
                  }
                  if (found) 
                    break
                }
                if (verbose) 
                  message(if (!found) 
                    "*NOT* ", "found", domain = NA)
            }
            if (found) 
                break
        }
        if (!found) 
            warning(gettextf("data set %s not found", sQuote(name)), 
                domain = NA)
    }
    invisible(names)
}


write.csv2 <- function (...) 
{
    Call <- match.call(expand.dots = TRUE)
    for (argname in c("append", "col.names", "sep", "dec", "qmethod")) if (!is.null(Call[[argname]])) 
        warning(gettextf("attempt to set '%s' ignored", argname), 
            domain = NA)
    rn <- eval.parent(Call$row.names)
    Call$append <- NULL
    Call$col.names <- if (is.logical(rn) && !rn) 
        TRUE
    else NA
    Call$sep <- ";"
    Call$dec <- ","
    Call$qmethod <- "double"
    Call[[1L]] <- as.name("write.table")
    eval.parent(Call)
}


memory.size <- function (max = FALSE) 
{
    warning("'memory.size()' is Windows-specific", call. = FALSE)
    Inf
}


menu <- function (choices, graphics = FALSE, title = NULL) 
{
    if (!interactive()) 
        stop("menu() cannot be used non-interactively")
    if (isTRUE(graphics)) {
        if (.Platform$OS.type == "windows" || .Platform$GUI == 
            "AQUA" || (capabilities("tcltk") && capabilities("X11") && 
            suppressWarnings(tcltk::.TkUp))) {
            res <- select.list(choices, multiple = FALSE, title = title, 
                graphics = TRUE)
            return(match(res, choices, nomatch = 0L))
        }
    }
    nc <- length(choices)
    if (length(title) && nzchar(title[1L])) 
        cat(title[1L], "\n")
    op <- paste0(format(seq_len(nc)), ": ", choices)
    if (nc > 10L) {
        fop <- format(op)
        nw <- nchar(fop[1L], "w") + 2
        ncol <- getOption("width")%/%nw
        if (ncol > 1L) 
            op <- paste0(fop, c(rep("  ", ncol - 1), "\n"), collapse = "")
        cat("", op, "", sep = "\n")
    }
    else cat("", op, "", sep = "\n")
    repeat {
        ind <- .Call(C_menu, as.character(choices))
        if (ind <= nc) 
            return(ind)
        cat(gettext("Enter an item from the menu, or 0 to exit\n"))
    }
}


read.csv <- function (file, header = TRUE, sep = ",", quote = "\"", dec = ".", 
    fill = TRUE, comment.char = "", ...) 
read.table(file = file, header = header, sep = sep, quote = quote, 
    dec = dec, fill = fill, comment.char = comment.char, ...)


url.show <- function (url, title = url, file = tempfile(), delete.file = TRUE, 
    method, ...) 
{
    if (download.file(url, destfile = file, method = method, 
        mode = "w")) 
        stop("transfer failure")
    file.show(file, delete.file = delete.file, title = title, 
        ...)
}


history <- function (max.show = 25, reverse = FALSE, pattern, ...) 
{
    file1 <- tempfile("Rrawhist")
    savehistory(file1)
    rawhist <- readLines(file1)
    unlink(file1)
    if (!missing(pattern)) 
        rawhist <- unique(grep(pattern, rawhist, value = TRUE, 
            ...))
    nlines <- length(rawhist)
    if (nlines) {
        inds <- max(1, nlines - max.show):nlines
        if (reverse) 
            inds <- rev(inds)
    }
    else inds <- integer()
    file2 <- tempfile("hist")
    writeLines(rawhist[inds], file2)
    file.show(file2, title = "R History", delete.file = TRUE)
}


chooseBioCmirror <- function (graphics = getOption("menu.graphics"), ind = NULL, 
    useHTTPS = getOption("useHTTPS", TRUE), local.only = FALSE) 
{
    m <- .getMirrors("https://bioconductor.org/BioC_mirrors.csv", 
        file.path(R.home("doc"), "BioC_mirrors.csv"), all = FALSE, 
        local.only = local.only)
    url <- .chooseMirror(m, "BioC", graphics, ind, useHTTPS)
    if (length(url)) 
        options(BioC_mirror = url)
    invisible()
}


View <- function (x, title) 
{
    check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
    msg <- "View() should not be used in examples etc"
    if (identical(check, "stop")) 
        stop(msg, domain = NA)
    else if (identical(check, "warn")) 
        warning(msg, immediate. = TRUE, noBreaks. = TRUE, domain = NA)
    if (missing(title)) 
        title <- paste("Data:", deparse(substitute(x))[1])
    as.num.or.char <- function(x) {
        if (is.character(x)) 
            x
        else if (is.numeric(x)) {
            storage.mode(x) <- "double"
            x
        }
        else as.character(x)
    }
    x0 <- as.data.frame(x)
    x <- as.list(format.data.frame(x0))
    rn <- row.names(x0)
    if (any(rn != seq_along(rn))) 
        x <- c(list(row.names = rn), x)
    if (!is.list(x) || !length(x) || !all(sapply(x, is.atomic)) || 
        !max(lengths(x))) 
        stop("invalid 'x' argument")
    if (grepl("darwin", R.version$os)) 
        check_for_XQuartz()
    invisible(.External2(C_dataviewer, x, title))
}


assignInMyNamespace <- function (x, value) 
{
    f <- sys.function(-1)
    ns <- environment(f)
    if (isS4(f)) 
        while (!isNamespace(ns)) ns <- parent.env(ns)
    if (bindingIsLocked(x, ns)) {
        unlockBinding(x, ns)
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(x, ns)
    }
    else assign(x, value, envir = ns, inherits = FALSE)
    if (!isBaseNamespace(ns)) {
        S3 <- getNamespaceInfo(ns, "S3methods")
        if (!length(S3)) 
            return(invisible(NULL))
        S3names <- S3[, 3L]
        if (x %in% S3names) {
            i <- match(x, S3names)
            genfun <- get(S3[i, 1L], mode = "function", envir = parent.frame())
            if (.isMethodsDispatchOn() && methods::is(genfun, 
                "genericFunction")) 
                genfun <- methods::slot(genfun, "default")@methods$ANY
            defenv <- if (typeof(genfun) == "closure") 
                environment(genfun)
            else .BaseNamespaceEnv
            S3Table <- get(".__S3MethodsTable__.", envir = defenv)
            remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
            if (exists(remappedName, envir = S3Table, inherits = FALSE)) 
                assign(remappedName, value, S3Table)
        }
    }
    invisible(NULL)
}


hsearch_db <- function (package = NULL, lib.loc = NULL, types = getOption("help.search.types"), 
    verbose = getOption("verbose"), rebuild = FALSE, use_UTF8 = FALSE) 
{
    WINDOWS <- .Platform$OS.type == "windows"
    if (is.logical(verbose)) 
        verbose <- 2 * as.integer(verbose)
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    i <- pmatch(types, hsearch_db_types)
    if (anyNA(i)) 
        stop("incorrect type specification")
    else types <- hsearch_db_types[i]
    db <- eval(.hsearch_db())
    if (is.null(db)) 
        rebuild <- TRUE
    else if (!rebuild) {
        if (!identical(lib.loc, attr(db, "LibPaths")) || any(is.na(match(types, 
            attr(db, "Types")))) || any(attr(db, "mtime") < file.mtime(lib.loc[file.exists(lib.loc)])) || 
            !identical(attr(db, "ctype"), Sys.getlocale("LC_CTYPE"))) 
            rebuild <- TRUE
        if (!is.null(package) && any(is.na(match(package, db$Base[, 
            "Package"])))) 
            rebuild <- TRUE
    }
    if (rebuild) {
        if (verbose > 0L) {
            message("Rebuilding the help.search() database", 
                " ", "...", if (verbose > 1L) 
                  "...", domain = NA)
            flush.console()
        }
        want_type_help <- any(types == "help")
        want_type_demo <- any(types == "demo")
        want_type_vignette <- any(types == "vignette")
        if (!is.null(package)) {
            packages_in_hsearch_db <- package
            package_paths <- NULL
        }
        else {
            ans <- character(0L)
            paths <- character(0L)
            lib.loc <- lib.loc[file.exists(lib.loc)]
            valid_package_version_regexp <- .standard_regexps()$valid_package_version
            for (lib in lib.loc) {
                a <- list.files(lib, all.files = FALSE, full.names = FALSE)
                for (nam in a) {
                  pfile <- file.path(lib, nam, "Meta", "package.rds")
                  if (file.exists(pfile)) 
                    info <- readRDS(pfile)$DESCRIPTION[c("Package", 
                      "Version")]
                  else next
                  if ((length(info) != 2L) || anyNA(info)) 
                    next
                  if (!grepl(valid_package_version_regexp, info["Version"])) 
                    next
                  ans <- c(ans, nam)
                  paths <- c(paths, file.path(lib, nam))
                }
            }
            un <- !duplicated(ans)
            packages_in_hsearch_db <- ans[un]
            package_paths <- paths[un]
            names(package_paths) <- ans[un]
        }
        np <- 0L
        if (verbose >= 2L) {
            message("Packages {readRDS() sequentially}:", domain = NA)
            flush.console()
        }
        tot <- length(package_paths)
        incr <- 0L
        if (verbose && WINDOWS) {
            pb <- winProgressBar("R: creating the help.search() DB", 
                max = tot)
            on.exit(close(pb))
        }
        else if (verbose == 1L) 
            incr <- ifelse(tot > 500L, 100L, 10L)
        dbMat <- vector("list", length(packages_in_hsearch_db) * 
            4L)
        dim(dbMat) <- c(length(packages_in_hsearch_db), 4L)
        hDB0 <- tools:::.build_hsearch_index(NULL)
        for (p in packages_in_hsearch_db) {
            if (incr && np%%incr == 0L) {
                message(".", appendLF = FALSE, domain = NA)
                flush.console()
            }
            np <- np + 1L
            if (verbose && WINDOWS) 
                setWinProgressBar(pb, np)
            if (verbose >= 2L) {
                message(" ", p, appendLF = ((np%%5L) == 0L), 
                  domain = NA)
                flush.console()
            }
            path <- if (!is.null(package_paths)) 
                package_paths[p]
            else find.package(p, lib.loc, quiet = TRUE)
            if (length(path) == 0L) {
                if (is.null(package)) 
                  next
                else stop(gettextf("could not find package %s", 
                  sQuote(p)), domain = NA)
            }
            hDB <- NULL
            if (want_type_help) {
                if (file.exists(hs_file <- file.path(path, "Meta", 
                  "hsearch.rds"))) {
                  hDB <- readRDS(hs_file)
                  if (!is.null(hDB)) {
                    if (is.na(match("Encoding", colnames(hDB[[1L]])))) 
                      hDB[[1L]] <- cbind(hDB[[1L]], Encoding = "")
                    for (i in seq_along(hDB)) {
                      colnames(hDB[[i]]) <- tools:::hsearch_index_colnames[[i]]
                    }
                  }
                  else if (verbose >= 2L) {
                    message(gettextf("package %s has empty hsearch data - strangely", 
                      sQuote(p)), domain = NA)
                    flush.console()
                  }
                }
                else if (!is.null(package)) 
                  warning("no hsearch.rds meta data for package ", 
                    p, domain = NA)
            }
            if (is.null(hDB)) 
                hDB <- hDB0
            nh <- NROW(hDB[[1L]])
            hDB[[1L]] <- cbind(hDB[[1L]], Type = rep("help", 
                nh))
            if (nh) 
                hDB[[1L]][, "LibPath"] <- path
            if (want_type_vignette) 
                hDB <- merge_vignette_index(hDB, path, p)
            if (want_type_demo) 
                hDB <- merge_demo_index(hDB, path, p)
            dbMat[np, seq_along(hDB)] <- hDB
        }
        if (verbose >= 2L) {
            message(ifelse(np%%5L == 0L, "\n", "\n\n"), sprintf("Built dbMat[%d,%d]", 
                nrow(dbMat), ncol(dbMat)), domain = NA)
            flush.console()
        }
        if (.isMethodsDispatchOn()) {
            bind_was_on <- methods:::bind_activation(FALSE)
            if (bind_was_on) 
                on.exit(methods:::bind_activation(TRUE))
        }
        db <- list(Base = do.call("rbind", dbMat[, 1]), Aliases = do.call("rbind", 
            dbMat[, 2]), Keywords = do.call("rbind", dbMat[, 
            3]), Concepts = do.call("rbind", dbMat[, 4]))
        rownames(db$Base) <- NULL
        if (is.null(db$Concepts)) {
            db$Concepts <- matrix(character(), ncol = 3L, dimnames = list(NULL, 
                tools:::hsearch_index_colnames$Concepts))
        }
        for (i in which(sapply(db, NROW) > 0L)) {
            db[[i]][, "ID"] <- paste(rep.int(seq_along(packages_in_hsearch_db), 
                sapply(dbMat[, i], NROW)), db[[i]][, "ID"], sep = "/")
        }
        if (!identical(Sys.getlocale("LC_CTYPE"), "C")) {
            if (verbose >= 2L) {
                message("reencoding ...", appendLF = FALSE, domain = NA)
                flush.console()
            }
            encoding <- db$Base[, "Encoding"]
            target <- ifelse(use_UTF8 && !l10n_info()$`UTF-8`, 
                "UTF-8", "")
            for (enc in unique(encoding)) {
                if (enc != target) 
                  next
                IDs <- db$Base[encoding == enc, "ID"]
                for (i in seq_along(db)) {
                  ind <- db[[i]][, "ID"] %in% IDs
                  db[[i]][ind, ] <- iconv(db[[i]][ind, ], enc, 
                    "")
                }
            }
            if (verbose >= 2L) {
                message(" ", "done", domain = NA)
                flush.console()
            }
        }
        bad_IDs <- unlist(sapply(db, function(u) u[rowSums(is.na(nchar(u, 
            "chars", allowNA = TRUE, keepNA = FALSE))) > 0, "ID"]))
        if (length(bad_IDs)) {
            for (i in seq_along(db)) {
                ind <- db[[i]][, "ID"] %in% bad_IDs
                db[[i]][ind, ] <- iconv(db[[i]][ind, ], "latin1", 
                  "")
            }
            bad_IDs <- unlist(sapply(db, function(u) u[rowSums(is.na(nchar(u, 
                "chars", allowNA = TRUE, keepNA = FALSE))) > 
                0, "ID"]))
        }
        if (length(bad_IDs)) {
            warning("removing all entries with invalid multi-byte character data")
            for (i in seq_along(db)) {
                ind <- db[[i]][, "ID"] %in% bad_IDs
                db[[i]] <- db[[i]][!ind, ]
            }
        }
        bad_IDs <- db$Base[is.na(db$Base[, "Topic"]), "ID"]
        if (length(bad_IDs)) {
            for (i in seq_along(db)) {
                ind <- db[[i]][, "ID"] %in% bad_IDs
                db[[i]] <- db[[i]][!ind, ]
            }
        }
        ind <- is.na(match(db$Keywords[, "Keyword"], c("", "~kwd1", 
            "~kwd2", "~~ other possible keyword(s) ~~")))
        db$Keywords <- db$Keywords[ind, , drop = FALSE]
        ind <- nzchar(db$Concepts[, "Concept"])
        db$Concepts <- db$Concepts[ind, , drop = FALSE]
        standard <- .get_standard_Rd_keywords_with_descriptions()
        keywords <- standard$Keywords
        concepts <- standard$Descriptions
        pos <- match(db$Keywords[, "Keyword"], keywords)
        ind <- !is.na(pos) & (keywords[pos] != "internal")
        db$Concepts <- rbind(db$Concepts, db$Keywords[is.na(pos), 
            , drop = FALSE], cbind(concepts[pos[ind]], db$Keywords[ind, 
            -1L, drop = FALSE]))
        db$Keywords <- db$Keywords[!is.na(pos), , drop = FALSE]
        db <- lapply(db, as.data.frame, stringsAsFactors = FALSE, 
            row.names = NULL)
        if (verbose >= 2L) {
            message("saving the database ...", appendLF = FALSE, 
                domain = NA)
            flush.console()
        }
        attr(db, "LibPaths") <- lib.loc
        attr(db, "mtime") <- Sys.time()
        attr(db, "ctype") <- Sys.getlocale("LC_CTYPE")
        attr(db, "Types") <- unique(c("help", types))
        class(db) <- "hsearch_db"
        .hsearch_db(db)
        if (verbose >= 2L) {
            message(" ", "done", domain = NA)
            flush.console()
        }
        if (verbose > 0L) {
            message("... database rebuilt", domain = NA)
            if (WINDOWS) {
                close(pb)
                on.exit()
            }
            flush.console()
        }
    }
    db
}


browseURL <- function (url, browser = getOption("browser"), encodeIfNeeded = FALSE) 
{
    WINDOWS <- .Platform$OS.type == "windows"
    if (!is.character(url) || length(url) != 1L || !nzchar(url)) 
        stop("'url' must be a non-empty character string")
    if (identical(browser, "false")) 
        return(invisible())
    if (WINDOWS && is.null(browser)) 
        return(shell.exec(url))
    if (is.function(browser)) 
        return(invisible(browser(if (encodeIfNeeded) URLencode(url) else url)))
    if (!is.character(browser) || length(browser) != 1L || !nzchar(browser)) 
        stop("'browser' must be a non-empty character string")
    if (WINDOWS) {
        return(system(paste0("\"", browser, "\" ", if (encodeIfNeeded) URLencode(url) else url), 
            wait = FALSE))
    }
    if (.Platform$GUI == "AQUA" || length(grep("^(localhost|):", 
        Sys.getenv("DISPLAY")))) 
        isLocal <- TRUE
    else isLocal <- FALSE
    .shQuote <- function(string) paste0("\"", gsub("\\$", "\\\\$", 
        string), "\"")
    quotedUrl <- .shQuote(if (encodeIfNeeded) 
        URLencode(url)
    else url)
    remoteCmd <- if (isLocal) 
        switch(basename(browser), `gnome-moz-remote` = , open = quotedUrl, 
            galeon = paste("-x", quotedUrl), kfmclient = paste("openURL", 
                quotedUrl), mozilla = , opera = {
                paste0("-remote \"openURL(", gsub("([,)$])", 
                  "%\\1", url), ")\"")
            }, quotedUrl)
    else quotedUrl
    system(paste(browser, remoteCmd, "> /dev/null 2>&1 ||", browser, 
        quotedUrl, "&"))
}


untar <- function (tarfile, files = NULL, list = FALSE, exdir = ".", compressed = NA, 
    extras = NULL, verbose = FALSE, restore_times = TRUE, tar = Sys.getenv("TAR")) 
{
    if (inherits(tarfile, "connection") || identical(tar, "internal")) 
        return(untar2(tarfile, files, list, exdir, restore_times))
    if (!(is.character(tarfile) && length(tarfile) == 1L)) 
        stop("invalid 'tarfile' argument")
    TAR <- tar
    if (!nzchar(TAR) && .Platform$OS.type == "windows" && nzchar(Sys.which("tar.exe"))) 
        TAR <- "tar.exe"
    if (!nzchar(TAR) || TAR == "internal") 
        return(untar2(tarfile, files, list, exdir))
    cflag <- ""
    if (is.character(compressed)) {
        switch(match.arg(compressed, c("gzip", "bzip2", "xz")), 
            gzip = "z", bzip2 = "j", xz = "J")
    }
    else if (is.logical(compressed)) {
        if (is.na(compressed)) {
            magic <- readBin(tarfile, "raw", n = 3L)
            if (all(magic[1:2] == c(31, 139))) 
                cflag <- "z"
            else if (all(magic[1:2] == c(31, 157))) 
                cflag <- "z"
            else if (rawToChar(magic[1:3]) == "BZh") 
                cflag <- "j"
            else if (rawToChar(magic[1:5]) == "\xfd7zXZ") 
                cflag <- "J"
        }
        else if (compressed) 
            cflag <- "z"
    }
    else stop("'compressed' must be logical or character")
    if (!restore_times) 
        cflag <- paste0(cflag, "m")
    gzOK <- .Platform$OS.type == "windows"
    if (!gzOK) {
        tf <- tempfile()
        cmd <- paste0(TAR, " -", cflag, "tf ", shQuote(tarfile))
        system(paste(TAR, "--version >", tf, "2>&1"))
        if (file.exists(tf)) {
            gzOK <- any(grepl("GNU", readLines(tf), fixed = TRUE))
            unlink(tf)
        }
    }
    tarfile <- path.expand(tarfile)
    if (!gzOK && cflag == "z" && nzchar(ZIP <- Sys.getenv("R_GZIPCMD"))) {
        TAR <- paste(ZIP, "-dc", shQuote(tarfile), "|", TAR)
        tarfile <- "-"
        cflag <- ""
    }
    if (!gzOK && cflag == "j" && nzchar(ZIP <- Sys.getenv("R_BZIPCMD"))) {
        TAR <- paste(ZIP, "-dc", shQuote(tarfile), "|", TAR)
        tarfile < "-"
        cflag <- ""
    }
    if (cflag == "J") {
        TAR <- paste("xz -dc", shQuote(tarfile), "|", TAR)
        tarfile < "-"
        cflag <- ""
    }
    if (list) {
        cmd <- paste0(TAR, " -", cflag, "tf ", shQuote(tarfile))
        if (length(extras)) 
            cmd <- paste(cmd, extras, collapse = " ")
        if (verbose) 
            message("untar: using cmd = ", sQuote(cmd), domain = NA)
        system(cmd, intern = TRUE)
    }
    else {
        cmd <- paste0(TAR, " -", cflag, "xf ", shQuote(tarfile))
        if (!missing(exdir)) {
            if (!dir.exists(exdir)) {
                if (!dir.create(exdir, showWarnings = TRUE, recursive = TRUE)) 
                  stop(gettextf("failed to create directory %s", 
                    sQuote(exdir)), domain = NA)
            }
            cmd <- if (.Platform$OS.type == "windows") 
                paste(cmd, "-C", shQuote(gsub("\\", "/", exdir, 
                  fixed = TRUE)))
            else paste(cmd, "-C", shQuote(exdir))
        }
        if (length(extras)) 
            cmd <- paste(cmd, extras, collapse = " ")
        if (length(files)) 
            cmd <- paste(cmd, paste(shQuote(files), collapse = " "))
        if (verbose) 
            message("untar: using cmd = ", sQuote(cmd), domain = NA)
        res <- system(cmd)
        if (res) 
            warning(sQuote(cmd), " returned error code ", res, 
                domain = NA)
        invisible(res)
    }
}


aspell <- function (files, filter, control = list(), encoding = "unknown", 
    program = NULL, dictionaries = character()) 
{
    program <- aspell_find_program(program)
    if (is.na(program)) 
        stop("No suitable spell-checker program found")
    if (inherits(files, "Rd")) 
        files <- list(files)
    files_are_names <- is.character(files)
    filter_args <- list()
    if (missing(filter) || is.null(filter)) {
        filter <- if (!files_are_names) {
            function(ifile, encoding) {
                if (inherits(ifile, "srcfile")) 
                  readLines(ifile$filename, encoding = encoding, 
                    warn = FALSE)
                else if (inherits(ifile, "connection")) 
                  readLines(ifile, encoding = encoding, warn = FALSE)
                else {
                  as.character(ifile)
                }
            }
        }
        else NULL
    }
    else if (is.character(filter)) {
        filter_name <- filter[1L]
        filter <- aspell_filter_db[[filter_name]]
        if (is.null(filter)) 
            warning(gettextf("Filter '%s' is not available.", 
                filter_name), domain = NA)
    }
    else if (is.list(filter)) {
        filter_name <- filter[[1L]][1L]
        filter_args <- filter[-1L]
        filter <- aspell_filter_db[[filter_name]]
        if (is.null(filter)) 
            warning(gettextf("Filter '%s' is not available.", 
                filter_name), domain = NA)
    }
    else if (!is.function(filter)) 
        stop("Invalid 'filter' argument.")
    encoding <- rep_len(encoding, length(files))
    verbose <- getOption("verbose")
    db <- data.frame(Original = character(), File = character(), 
        Line = integer(), Column = integer(), stringsAsFactors = FALSE)
    db$Suggestions <- list()
    tfile <- tempfile("aspell")
    on.exit(unlink(tfile))
    if (length(dictionaries)) {
        paths <- aspell_find_dictionaries(dictionaries)
        ind <- paths == ""
        if (any(ind)) {
            warning(gettextf("The following dictionaries were not found:\n%s", 
                paste(sprintf("  %s", dictionaries[ind]), collapse = "\n")), 
                domain = NA)
            paths <- paths[!ind]
        }
        if (length(paths)) {
            words <- unlist(lapply(paths, readRDS), use.names = FALSE)
            personal <- tempfile("aspell_personal")
            on.exit(unlink(personal), add = TRUE)
            aspell_write_personal_dictionary_file(words, personal, 
                program = program)
            control <- c(control, "-p", shQuote(personal))
        }
    }
    control <- as.character(control)
    fnames <- names(files)
    files <- as.list(files)
    for (i in seq_along(files)) {
        file <- files[[i]]
        if (files_are_names) 
            fname <- file
        else {
            fname <- if (inherits(file, "srcfile")) 
                file$filename
            else attr(attr(file, "srcref"), "srcfile")$filename
            if (is.null(fname)) 
                fname <- fnames[i]
            if (is.null(fname)) 
                fname <- "<unknown>"
        }
        enc <- encoding[i]
        if (verbose) 
            message(gettextf("Processing file %s", fname), domain = NA)
        lines <- if (is.null(filter)) 
            readLines(file, encoding = enc, warn = FALSE)
        else {
            do.call(filter, c(list(file, encoding = enc), filter_args))
        }
        control <- c(control, attr(lines, "control"))
        writeLines(paste0("^", lines), tfile)
        out <- tools:::.system_with_capture(program, c("-a", 
            control), stdin = tfile)
        if (out$status != 0L) 
            stop(gettextf("Running aspell failed with diagnostics:\n%s", 
                paste(out$stderr, collapse = "\n")), domain = NA)
        lines <- out$stdout[-1L]
        pos <- cumsum(lines == "") + 1L
        if (any(ind <- startsWith(lines, "&"))) {
            info <- strsplit(lines[ind], ": ", fixed = TRUE)
            one <- strsplit(sapply(info, `[`, 1L), " ", fixed = TRUE)
            two <- strsplit(sapply(info, `[`, 2L), ", ", fixed = TRUE)
            db1 <- data.frame(Original = as.character(sapply(one, 
                `[`, 2L)), File = fname, Line = pos[ind], Column = as.integer(sapply(one, 
                `[`, 4L)), stringsAsFactors = FALSE)
            db1$Suggestions <- two
            db <- rbind(db, db1)
        }
        if (any(ind <- startsWith(lines, "#"))) {
            one <- strsplit(lines[ind], " ", fixed = TRUE)
            db1 <- data.frame(Original = as.character(sapply(one, 
                `[`, 2L)), File = fname, Line = pos[ind], Column = as.integer(sapply(one, 
                `[`, 3L)), stringsAsFactors = FALSE)
            db1$Suggestions <- vector("list", length(one))
            db <- rbind(db, db1)
        }
    }
    class(db) <- c("aspell", "data.frame")
    db
}


zip <- function (zipfile, files, flags = "-r9X", extras = "", zip = Sys.getenv("R_ZIPCMD", 
    "zip")) 
{
    if (missing(flags) && (!is.character(files) || !length(files))) 
        stop("'files' must a character vector specifying one or more filepaths")
    args <- c(flags, shQuote(path.expand(zipfile)), shQuote(files), 
        extras)
    if (.Platform$OS.type == "windows") 
        invisible(system2(zip, args, invisible = TRUE))
    else invisible(system2(zip, args))
}


make.socket <- function (host = "localhost", port, fail = TRUE, server = FALSE) 
{
    if (length(port <- as.integer(port)) != 1L) 
        stop("'port' must be integer of length 1")
    if (length(host <- as.character(host)) != 1L) 
        stop("'host' must be character of length 1")
    if (!server) {
        socket <- .Call(C_sockconnect, port, host)
    }
    else {
        if (host != "localhost") 
            stop("can only receive calls on local machine")
        tmp <- .Call(C_sockopen, port)
        socket <- .Call(C_socklisten, tmp)
        host <- attr(socket, "host")
        .Call(C_sockclose, tmp)
    }
    if (socket <= 0) {
        if (fail) 
            stop("socket not established")
        else warning("socket not established")
    }
    rval <- list(socket = socket, host, port = port)
    class(rval) <- "socket"
    rval
}


getSrcDirectory <- function (x, unique = TRUE) 
{
    result <- dirname(getSrcFilename(x, full.names = TRUE, unique = unique))
    if (unique) 
        unique(result)
    else result
}


suppressForeignCheck <- function (names, package, add = TRUE) 
registerNames(names, package, ".__suppressForeign__", add)


undebugcall <- function (call) 
{
    call <- substitute(call)
    .debugcall(call, undebug)
    invisible(NULL)
}


Stangle <- function (file, driver = Rtangle(), syntax = getOption("SweaveSyntax"), 
    encoding = "", ...) 
Sweave(file = file, driver = driver, encoding = encoding, ...)


aspell_package_C_files <- function (dir, ignore = character(), control = list(), program = NULL, 
    dictionaries = character()) 
{
    dir <- tools::file_path_as_absolute(dir)
    files <- file.path(dir, "po", paste(basename(dir), "pot", 
        collapse = "."))
    files <- files[file_test("-f", files)]
    meta <- tools:::.get_package_metadata(dir, installed = FALSE)
    if (is.na(encoding <- meta["Encoding"])) 
        encoding <- "unknown"
    defaults <- .aspell_package_defaults(dir, encoding)$C_files
    if (!is.null(defaults)) {
        if (!is.null(d <- defaults$ignore)) 
            ignore <- d
        if (!is.null(d <- defaults$control)) 
            control <- d
        if (!is.null(d <- defaults$program)) 
            program <- d
        if (!is.null(d <- defaults$dictionaries)) {
            dictionaries <- aspell_find_dictionaries(d, file.path(dir, 
                ".aspell"))
        }
    }
    program <- aspell_find_program(program)
    aspell(files, filter = list("pot", ignore = ignore), control = control, 
        encoding = encoding, program = program, dictionaries = dictionaries)
}


download.file <- function (url, destfile, method, quiet = FALSE, mode = "w", cacheOK = TRUE, 
    extra = getOption("download.file.extra")) 
{
    destfile
    method <- if (missing(method)) 
        getOption("download.file.method", default = "auto")
    else match.arg(method, c("auto", "internal", "libcurl", "wget", 
        "curl", "lynx"))
    if (method == "auto") {
        if (length(url) != 1L || typeof(url) != "character") 
            stop("'url' must be a length-one character vector")
        method <- if (grepl("^file:", url)) 
            "internal"
        else "libcurl"
    }
    switch(method, internal = {
        status <- .External(C_download, url, destfile, quiet, 
            mode, cacheOK)
        if (!quiet) flush.console()
    }, libcurl = {
        status <- .Internal(curlDownload(url, destfile, quiet, 
            mode, cacheOK))
        if (!quiet) flush.console()
    }, wget = {
        if (length(url) != 1L || typeof(url) != "character") stop("'url' must be a length-one character vector")
        if (length(destfile) != 1L || typeof(destfile) != "character") stop("'destfile' must be a length-one character vector")
        if (quiet) extra <- c(extra, "--quiet")
        if (!cacheOK) extra <- c(extra, "--cache=off")
        status <- system(paste("wget", paste(extra, collapse = " "), 
            shQuote(url), "-O", shQuote(path.expand(destfile))))
        if (status) stop("'wget' call had nonzero exit status")
    }, curl = {
        if (length(url) != 1L || typeof(url) != "character") stop("'url' must be a length-one character vector")
        if (length(destfile) != 1L || typeof(url) != "character") stop("'destfile' must be a length-one character vector")
        if (quiet) extra <- c(extra, "-s -S")
        if (!cacheOK) extra <- c(extra, "-H 'Pragma: no-cache'")
        status <- system(paste("curl", paste(extra, collapse = " "), 
            shQuote(url), " -o", shQuote(path.expand(destfile))))
        if (status) stop("'curl' call had nonzero exit status")
    }, lynx = stop("method 'lynx' is defunct", domain = NA))
    if (status) 
        warning("download had nonzero exit status")
    invisible(status)
}


apropos <- function (what, where = FALSE, ignore.case = TRUE, mode = "any") 
{
    stopifnot(is.character(what))
    x <- character(0L)
    check.mode <- mode != "any"
    for (i in seq_along(sp <- search())) {
        li <- if (ignore.case) 
            grep(what, ls(pos = i, all.names = TRUE), ignore.case = TRUE, 
                value = TRUE)
        else ls(pos = i, pattern = what, all.names = TRUE)
        li <- grep("^[.](__|C_|F_)", li, invert = TRUE, value = TRUE)
        if (sp[i] == "package:base") 
            li <- li[!li %in% .dot_internals]
        if (length(li)) {
            if (check.mode) 
                li <- li[sapply(li, exists, where = i, mode = mode, 
                  inherits = FALSE)]
            x <- c(x, if (where) structure(li, names = rep.int(i, 
                length(li))) else li)
        }
    }
    sort(x)
}


as.roman <- function (x) 
.as.roman(x, check.range = TRUE)


xemacs <- function (name = NULL, file = "") 
edit.default(name, file, editor = "xemacs")


strcapture <- function (pattern, x, proto, perl = FALSE, useBytes = FALSE) 
{
    m <- regexec(pattern, x, perl = perl, useBytes = useBytes)
    str <- regmatches(x, m)
    ntokens <- length(proto) + 1L
    nomatch <- lengths(str) == 0L
    str[nomatch] <- list(rep(NA_character_, ntokens))
    if (length(str) > 0L && length(str[[1L]]) != ntokens) {
        stop("The number of captures in 'pattern' != 'length(proto)'")
    }
    mat <- matrix(as.character(unlist(str)), ncol = ntokens, 
        byrow = TRUE)[, -1L, drop = FALSE]
    ans <- lapply(seq_along(proto), function(i) {
        if (isS4(proto[[i]])) {
            methods::as(mat[, i], class(proto[[i]]))
        }
        else {
            fun <- match.fun(paste0("as.", class(proto[[i]])))
            fun(mat[, i])
        }
    })
    names(ans) <- names(proto)
    if (isS4(proto)) {
        methods::as(ans, class(proto))
    }
    else {
        as.data.frame(ans, optional = TRUE, stringsAsFactors = FALSE)
    }
}


methods <- function (generic.function, class) 
{
    if (!missing(generic.function) && !is.character(generic.function)) 
        generic.function <- deparse(substitute(generic.function))
    if (!missing(class) && !is.character(class)) 
        class <- paste(deparse(substitute(class)))
    s3 <- .S3methods(generic.function, class, parent.frame())
    s4 <- if (.isMethodsDispatchOn()) {
        methods::.S4methods(generic.function, class)
    }
    else NULL
    .MethodsFunction(s3, s4, missing(generic.function))
}


as.person <- function (x) 
UseMethod("as.person")




## Package Data

# none


## Package Info

.skeleton_package_title = "The R Utils Package"

.skeleton_package_version = "3.4.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF