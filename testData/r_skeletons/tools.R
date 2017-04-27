##
## Exported symobls in package `tools`
##

## Exported package methods

vignetteDepends <- function (vignette, recursive = TRUE, reduce = TRUE, local = TRUE, 
    lib.loc = NULL) 
{
    if (length(vignette) != 1L) 
        stop("argument 'vignette' must be of length 1")
    if (!nzchar(vignette)) 
        return(invisible())
    if (!file.exists(vignette)) 
        stop(gettextf("file '%s' not found", vignette), domain = NA)
    vigDeps <- vignetteInfo(vignette)$depends
    depMtrx <- getVigDepMtrx(vigDeps)
    instPkgs <- utils::installed.packages(lib.loc = lib.loc)
    getDepList(depMtrx, instPkgs, recursive, local, reduce)
}


SIGHUP <- 1L


checkTnF <- function (package, dir, file, lib.loc = NULL) 
{
    code_files <- docs_files <- character()
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        if (file.exists(file.path(dir, "R", "all.rda"))) {
            warning("cannot check R code installed as image")
        }
        code_file <- file.path(dir, "R", package)
        if (file.exists(code_file)) 
            code_files <- code_file
        example_dir <- file.path(dir, "R-ex")
        if (dir.exists(example_dir)) {
            code_files <- c(code_files, list_files_with_exts(example_dir, 
                "R"))
        }
    }
    else if (!missing(dir)) {
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        else dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if (dir.exists(code_dir)) 
            code_files <- list_files_with_type(code_dir, "code")
        docs_dir <- file.path(dir, "man")
        if (dir.exists(docs_dir)) 
            docs_files <- list_files_with_type(docs_dir, "docs")
    }
    else if (!missing(file)) {
        if (!file_test("-f", file)) 
            stop(gettextf("file '%s' does not exist", file), 
                domain = NA)
        else code_files <- file
    }
    else stop("you must specify 'package', 'dir' or 'file'")
    find_TnF_in_code <- function(file, txt) {
        matches <- list()
        TnF <- c("T", "F")
        find_bad_exprs <- function(e, p) {
            if (is.name(e) && (as.character(e) %in% TnF) && !is.null(p)) {
                matches <<- c(matches, list(p))
            }
            else if (is.recursive(e)) {
                for (i in seq_along(e)) Recall(e[[i]], e)
            }
        }
        exprs <- if (missing(txt)) 
            tryCatch(parse(file = file, n = -1L), error = function(e) stop(gettextf("parse error in file '%s':\n", 
                file, .massage_file_parse_error_message(conditionMessage(e))), 
                domain = NA, call. = FALSE))
        else tryCatch(parse(text = txt), error = function(e) stop(gettextf("parse error in examples from file '%s':\n", 
            file, conditionMessage(e)), domain = NA, call. = FALSE))
        for (i in seq_along(exprs)) find_bad_exprs(exprs[[i]], 
            NULL)
        matches
    }
    bad_exprs <- list()
    for (file in code_files) {
        exprs <- find_TnF_in_code(file)
        if (length(exprs)) {
            exprs <- list(exprs)
            names(exprs) <- file
            bad_exprs <- c(bad_exprs, exprs)
        }
    }
    for (file in docs_files) {
        Rd <- prepare_Rd(file, defines = .Platform$OS.type)
        txt <- .Rd_get_example_code(Rd)
        exprs <- find_TnF_in_code(file, txt)
        if (length(exprs)) {
            exprs <- list(exprs)
            names(exprs) <- file
            bad_exprs <- c(bad_exprs, exprs)
        }
    }
    class(bad_exprs) <- "checkTnF"
    bad_exprs
}


SIGSTOP <- 19L


xgettext <- function (dir, verbose = FALSE, asCall = TRUE) 
{
    dir <- file_path_as_absolute(dir)
    bn <- basename(dir)
    dir <- file.path(dir, "R")
    exts <- .make_file_exts("code")
    R_files <- list_files_with_exts(dir, exts)
    for (d in c("unix", "windows")) {
        OSdir <- file.path(dir, d)
        if (dir.exists(OSdir)) 
            R_files <- c(R_files, list_files_with_exts(OSdir, 
                exts))
    }
    if (bn == "base") {
        shdir <- file.path(dir, "../../../../share/R")
        R_files <- c(R_files, list_files_with_exts(shdir, exts))
    }
    out <- vector("list", length = length(R_files))
    names(out) <- R_files
    find_strings <- function(e) {
        find_strings2 <- function(e, suppress) {
            if (is.character(e)) {
                if (!suppress) 
                  strings <<- c(strings, e)
            }
            else if (is.call(e)) {
                if (is.name(e[[1L]]) && (as.character(e[[1L]]) %in% 
                  c("gettext", "gettextf"))) {
                  domain <- e[["domain"]]
                  suppress <- !is.null(domain) && !is.name(domain) && 
                    is.na(domain)
                  if (as.character(e[[1L]]) == "gettextf") {
                    e <- match.call(gettextf, e)
                    e <- e["fmt"]
                  }
                  else if (as.character(e[[1L]]) == "gettext" && 
                    !is.null(names(e))) {
                    e <- e[!(names(e) == "domain")]
                  }
                }
                for (i in seq_along(e)) find_strings2(e[[i]], 
                  suppress)
            }
        }
        if (is.call(e) && is.name(e[[1L]]) && (as.character(e[[1L]]) %in% 
            c("warning", "stop", "message", "packageStartupMessage", 
                "gettext", "gettextf"))) {
            domain <- e[["domain"]]
            suppress <- !is.null(domain) && !is.name(domain) && 
                is.na(domain)
            if (!is.null(names(e))) 
                e <- e[!names(e) %in% c("call.", "immediate.", 
                  "domain")]
            if (asCall) {
                if (!suppress) 
                  strings <<- c(strings, as.character(e)[-1L])
            }
            else {
                if (as.character(e[[1L]]) == "gettextf") {
                  e <- match.call(gettextf, e)
                  e <- e["fmt"]
                }
                for (i in seq_along(e)) find_strings2(e[[i]], 
                  suppress)
            }
        }
        else if (is.recursive(e)) 
            for (i in seq_along(e)) Recall(e[[i]])
    }
    for (f in R_files) {
        if (verbose) 
            message(gettextf("parsing '%s'", f), domain = NA)
        strings <- character()
        for (e in parse(file = f)) find_strings(e)
        strings <- sub("^[ \t\n]*", "", strings)
        strings <- sub("[ \t\n]*$", "", strings)
        out[[f]] <- structure(unique(strings), class = "xgettext")
    }
    out[lengths(out) > 0L]
}


parseLatex <- function (text, filename = deparse(substitute(text)), verbose = FALSE, 
    verbatim = c("verbatim", "verbatim*", "Sinput", "Soutput")) 
{
    srcfile <- srcfilecopy(filename, text, file.mtime(filename))
    text <- paste(text, collapse = "\n")
    .External2("C_parseLatex", text, srcfile, verbose, as.character(verbatim), 
        PACKAGE = "tools")
}


texi2pdf <- function (file, clean = FALSE, quiet = TRUE, texi2dvi = getOption("texi2dvi"), 
    texinputs = NULL, index = TRUE) 
texi2dvi(file = file, pdf = TRUE, clean = clean, quiet = quiet, 
    texi2dvi = texi2dvi, texinputs = texinputs, index = index)


checkRdaFiles <- function (paths) 
{
    if (length(paths) == 1L && dir.exists(paths)) {
        paths <- Sys.glob(c(file.path(paths, "*.rda"), file.path(paths, 
            "*.RData")))
        paths <- grep("/[.]RData$", paths, value = TRUE, invert = TRUE)
    }
    res <- data.frame(size = NA_real_, ASCII = NA, compress = NA_character_, 
        version = NA_integer_, stringsAsFactors = FALSE)
    res <- res[rep_len(1L, length(paths)), ]
    row.names(res) <- paths
    keep <- file.exists(paths)
    res$size[keep] <- file.size(paths)[keep]
    for (p in paths[keep]) {
        magic <- readBin(p, "raw", n = 5)
        res[p, "compress"] <- if (all(magic[1:2] == c(31, 139))) 
            "gzip"
        else if (rawToChar(magic[1:3]) == "BZh") 
            "bzip2"
        else if (magic[1L] == 253 && rawToChar(magic[2:5]) == 
            "7zXZ") 
            "xz"
        else if (grepl("RD[ABX][12]", rawToChar(magic), useBytes = TRUE)) 
            "none"
        else "unknown"
        con <- gzfile(p)
        magic <- readChar(con, 5L, useBytes = TRUE)
        close(con)
        res[p, "ASCII"] <- if (grepl("RD[ABX][12]", magic, useBytes = TRUE)) 
            substr(magic, 3, 3) == "A"
        else NA
        ver <- sub("(RD[ABX])([12]*)", "\\2", magic, useBytes = TRUE)
        res$version <- as.integer(ver)
    }
    res
}


SIGCONT <- 18L


read.00Index <- function (file) 
{
    if (is.character(file)) {
        if (file == "") 
            file <- stdin()
        else {
            file <- file(file, "r")
            on.exit(close(file))
        }
    }
    if (!inherits(file, "connection")) 
        stop(gettextf("argument '%s' must be a character string or connection", 
            file), domain = NA)
    y <- matrix("", nrow = 0L, ncol = 2L)
    x <- paste(readLines(file), collapse = "\n")
    for (chunk in unlist(strsplit(x, "\n[ \t\n]*\n"))) {
        entries <- tryCatch({
            if (!grepl("(   |\t)", chunk)) 
                NULL
            else {
                chunk <- gsub("\n[ \t]+", "\t", chunk)
                x <- strsplit(unlist(strsplit(chunk, "\n")), 
                  "[ \t]")
                cbind(unlist(lapply(x, "[[", 1L)), unlist(lapply(x, 
                  function(t) {
                    paste(t[-c(1L, which(!nzchar(t)))], collapse = " ")
                  })))
            }
        }, error = identity)
        if (!inherits(entries, "error") && NCOL(entries) == 2L) 
            y <- rbind(y, entries)
    }
    colnames(y) <- c("Item", "Description")
    y
}


makevars_user <- function () 
{
    m <- character()
    if (.Platform$OS.type == "windows") {
        if (!is.na(f <- Sys.getenv("R_MAKEVARS_USER", NA_character_))) {
            if (file.exists(f)) 
                m <- f
        }
        else if ((Sys.getenv("R_ARCH") == "/x64") && file.exists(f <- path.expand("~/.R/Makevars.win64"))) 
            m <- f
        else if (file.exists(f <- path.expand("~/.R/Makevars.win"))) 
            m <- f
        else if (file.exists(f <- path.expand("~/.R/Makevars"))) 
            m <- f
    }
    else {
        if (!is.na(f <- Sys.getenv("R_MAKEVARS_USER", NA_character_))) {
            if (file.exists(f)) 
                m <- f
        }
        else if (file.exists(f <- path.expand(paste("~/.R/Makevars", 
            Sys.getenv("R_PLATFORM"), sep = "-")))) 
            m <- f
        else if (file.exists(f <- path.expand("~/.R/Makevars"))) 
            m <- f
    }
    m
}


codocData <- function (package, lib.loc = NULL) 
{
    bad_Rd_objects <- structure(NULL, class = "codocData")
    if (length(package) != 1L) 
        stop("argument 'package' must be of length 1")
    dir <- find.package(package, lib.loc)
    db <- Rd_db(package, lib.loc = dirname(dir))
    is_base <- basename(dir) == "base"
    has_namespace <- !is_base && packageHasNamespace(package, 
        dirname(dir))
    if (!is_base) 
        .load_package_quietly(package, lib.loc)
    code_env <- .package_env(package)
    if (has_namespace) 
        ns_env <- asNamespace(package)
    aliases <- lapply(db, .Rd_get_metadata, "alias")
    idx <- lengths(aliases) == 1L
    if (!any(idx)) 
        return(bad_Rd_objects)
    db <- db[idx]
    aliases <- aliases[idx]
    names(db) <- .Rd_get_names_from_Rd_db(db)
    .get_var_names_from_item_tags <- function(s, nice = TRUE) {
        if (!length(s)) 
            return(character())
        nms <- character()
        s <- sub("^ *", "", sub("( *:)? *$", "", s))
        re <- "\\\\samp\\{(([^\\}]|[\\].)*)\\}( *, *)?"
        m <- gregexpr(re, s)
        if (any(unlist(m) > -1)) {
            nms <- sub(re, "\\1", unlist(regmatches(s, m)))
            nms <- gsub("\\\\([{}%])", "\\1", nms)
            regmatches(s, m) <- ""
        }
        re <- "\\\\code\\{([^}]*)\\}( *, *)?"
        m <- gregexpr(re, s)
        add <- regmatches(s, m)
        lens <- lengths(add)
        add <- sub(re, "\\1", unlist(add))
        if (nice) {
            ind <- rep.int(lens == 1L, lens)
            add[ind] <- trimws(add[ind])
        }
        nms <- c(nms, add)
        regmatches(s, m) <- ""
        nms <- c(nms, unlist(strsplit(s, " *, *")))
        nms
    }
    .get_data_frame_var_names <- function(x) {
        x <- x[RdTags(x) == "\\format"]
        if (length(x) != 1L) 
            return(character())
        x <- .Rd_drop_comments(x[[1L]])
        if (!grepl("^[ \n\t]*(A|This) data frame", .Rd_deparse(x, 
            tag = FALSE))) 
            return(character())
        x <- .Rd_get_section(x, "describe")
        x <- .Rd_get_item_tags(x)
        .get_var_names_from_item_tags(x)
    }
    Rd_var_names <- lapply(db, .get_data_frame_var_names)
    idx <- (lengths(Rd_var_names) > 0L)
    if (!length(idx)) 
        return(bad_Rd_objects)
    aliases <- unlist(aliases[idx])
    Rd_var_names <- Rd_var_names[idx]
    db_names <- names(db)[idx]
    data_env <- new.env(hash = TRUE)
    data_dir <- file.path(dir, "data")
    has_data <- dir.exists(data_dir) && !file_test("-f", file.path(data_dir, 
        "Rdata.rdb"))
    data_exts <- .make_file_exts("data")
    data_frames_checked <- character()
    for (i in seq_along(aliases)) {
        var_names_in_docs <- sort(Rd_var_names[[i]])
        al <- aliases[i]
        if (!is.null(A <- get0(al, envir = code_env, mode = "list", 
            inherits = FALSE))) 
            al <- A
        else if (has_namespace && !is.null(A <- get0(al, envir = ns_env, 
            mode = "list", inherits = FALSE))) 
            al <- A
        else if (has_data) {
            if (!length(dir(data_dir) %in% paste(al, data_exts, 
                sep = "."))) {
                next
            }
            utils::data(list = al, envir = data_env)
            if (!is.null(A <- get0(al, envir = data_env, mode = "list", 
                inherits = FALSE))) 
                al <- A
            rm(list = ls(envir = data_env, all.names = TRUE), 
                envir = data_env)
        }
        if (!is.data.frame(al)) 
            next
        data_frames_checked <- c(data_frames_checked, aliases[i])
        var_names_in_code <- sort(names(al))
        if (!identical(var_names_in_code, var_names_in_docs)) 
            bad_Rd_objects[[db_names[i]]] <- list(name = aliases[i], 
                code = var_names_in_code, docs = var_names_in_docs)
    }
    attr(bad_Rd_objects, "data_frames_checked") <- as.character(data_frames_checked)
    bad_Rd_objects
}


buildVignette <- function (file, dir = ".", weave = TRUE, latex = TRUE, tangle = TRUE, 
    quiet = TRUE, clean = TRUE, keep = character(), engine = NULL, 
    buildPkg = NULL, encoding = getVignetteEncoding(file), ...) 
{
    if (!file_test("-f", file)) 
        stop(gettextf("file '%s' not found", file), domain = NA)
    if (!dir.exists(dir)) 
        stop(gettextf("directory '%s' does not exist", dir), 
            domain = NA)
    if (!is.null(buildPkg)) 
        for (pkg in buildPkg) loadNamespace(pkg)
    if (is.null(engine)) 
        engine <- getVignetteEngine(file)
    if (is.character(engine)) 
        engine <- vignetteEngine(engine, package = buildPkg)
    names <- sapply(engine$pattern, FUN = sub, "", file)
    name <- basename(names[(names != file)][1L])
    if (is.na(name)) 
        stop(gettextf("vignette filename '%s' does not match any of the '%s' filename patterns", 
            file, paste(engine$package, engine$name, sep = "::")), 
            domain = NA)
    if (encoding == "non-ASCII") 
        stop(gettextf("Vignette '%s' is non-ASCII but has no declared encoding", 
            name))
    file <- file_path_as_absolute(file)
    olddir <- setwd(dir)
    if (!is.null(olddir)) 
        on.exit(setwd(olddir))
    if (is.na(clean) || clean) {
        file.create(".build.timestamp")
    }
    tdir <- getwd()
    output <- NULL
    final <- if (weave) {
        engine$weave(file, quiet = quiet, encoding = encoding, 
            ...)
        setwd(tdir)
        output <- find_vignette_product(name, by = "weave", engine = engine)
        if (latex && vignette_is_tex(output)) {
            texi2pdf(file = output, clean = FALSE, quiet = quiet)
            find_vignette_product(name, by = "texi2pdf", engine = engine)
        }
        else output
    }
    sources <- if (tangle) {
        engine$tangle(file, quiet = quiet, encoding = encoding, 
            ...)
        setwd(tdir)
        find_vignette_product(name, by = "tangle", main = FALSE, 
            engine = engine)
    }
    keep <- c(sources, final, keep)
    if (is.na(clean)) {
        keep <- c(keep, output)
        clean <- TRUE
    }
    if (clean) {
        f <- setdiff(list.files(all.files = TRUE, no.. = TRUE), 
            keep)
        newer <- file_test("-nt", f, ".build.timestamp")
        unlink(f[newer], recursive = TRUE)
    }
    if ((is.na(clean) || clean) && file.exists(".build.timestamp")) {
        file.remove(".build.timestamp")
    }
    unique(keep)
}


Rd2ex <- function (Rd, out = "", defines = .Platform$OS.type, stages = "render", 
    outputEncoding = "UTF-8", commentDontrun = TRUE, commentDonttest = FALSE, 
    ...) 
{
    WriteLines <- function(x, con, outputEncoding, ...) {
        if (outputEncoding != "UTF-8") {
            x <- iconv(x, "UTF-8", outputEncoding, mark = FALSE)
            if (anyNA(x)) 
                x <- iconv(x, "UTF-8", outputEncoding, sub = "byte", 
                  mark = FALSE)
        }
        writeLines(x, con, useBytes = TRUE, ...)
    }
    dropNewline <- FALSE
    of0 <- function(...) of1(paste0(...))
    of1 <- function(text) {
        if (dropNewline && length(text)) {
            text[1L] <- psub("^\n", "", text[1L])
            dropNewline <<- FALSE
        }
        WriteLines(text, con, outputEncoding, sep = "")
    }
    wr <- function(x) paste0("###", strwrap(remap(x), 73L, indent = 1L, 
        exdent = 3L), collapse = "\n")
    remap <- function(x) {
        if (!length(x)) 
            return(x)
        x <- psub("\\\\(link|var)\\{([^}]+)\\}", "\\2", x)
        x <- psub("(?<!\\\\)\\\\([%{])", "\\1", x)
        x <- psub("\\\\(l|)dots", "...", x)
        x
    }
    render <- function(x, prefix = "") {
        renderDont <- function(txt, comment, label = TRUE, xtra1 = comment) {
            if (label) 
                of0("## ", txt, ": ")
            if (xtra1 && length(x) == 1L) {
                render(x[[1L]], prefix)
            }
            else {
                if (!grepl("^\n", x[[1L]][1L], perl = TRUE) && 
                  RdTags(x)[1L] != "COMMENT") {
                  writeLines("", con)
                  render(x[[1L]], paste0(if (comment) 
                    "##D ", prefix))
                }
                else render(x[[1L]], prefix)
                for (i in seq_along(x)[-1]) render(x[[i]], paste0(if (comment) 
                  "##D ", prefix))
                last <- x[[length(x)]]
                if (!grepl("\n$", last[length(last)], perl = TRUE)) 
                  writeLines("", con)
                if (label) 
                  of0("## End(", txt, ")")
            }
        }
        tag <- attr(x, "Rd_tag")
        if (tag %in% c("\\dontshow", "\\testonly")) {
            renderDont("Don't show", comment = FALSE)
        }
        else if (tag == "\\dontrun") {
            renderDont("Not run", commentDontrun, label = commentDontrun)
        }
        else if (tag == "\\donttest") {
            renderDont("No test", commentDonttest, xtra1 = FALSE)
        }
        else if (tag == "COMMENT") {
            if (attr(x, "srcref")[2L] == 1L) 
                dropNewline <<- TRUE
        }
        else if (tag %in% c("\\dots", "\\ldots")) {
            of1("...")
        }
        else if (tag == "\\if" || tag == "\\ifelse") {
            if (testRdConditional("example", x, Rdfile)) 
                for (i in seq_along(x[[2L]])) render(x[[2L]][[i]], 
                  prefix)
            else if (tag == "\\ifelse") 
                for (i in seq_along(x[[3L]])) render(x[[3L]][[i]], 
                  prefix)
        }
        else if (tag == "\\out") {
            for (i in seq_along(x)) of1(x[[i]])
        }
        else if (tag %in% c("USERMACRO", "\\newcommand", "\\renewcommand")) {
        }
        else {
            txt <- unlist(x)
            of0(prefix, remap(txt))
        }
    }
    Rd <- prepare_Rd(Rd, defines = defines, stages = stages, 
        ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)
    where <- which(sections == "\\examples")
    if (length(where)) {
        if (is.character(out)) {
            if (out == "") {
                con <- stdout()
            }
            else {
                con <- file(out, "wt")
                on.exit(close(con))
            }
        }
        else {
            con <- out
            out <- summary(con)$description
        }
        if (length(where) > 1L) 
            warning("more than one \\examples section, using the first")
        ex <- Rd[[where[1L]]]
        exl <- unlist(ex)
        if (length(exl) && any(Encoding(exl) != "unknown")) {
            if (any(f <- sections == "\\encoding")) {
                encoding <- unlist(Rd[which(f)])[1L]
                if (nzchar(outputEncoding)) 
                  encoding <- outputEncoding
                else outputEncoding <- encoding
                of0("### Encoding: ", encoding, "\n\n")
            }
        }
        nameblk <- sections == "\\name"
        if (any(nameblk)) {
            name <- as.character(Rd[[which.max(nameblk)]])
            of0("### Name: ", name, "\n")
        }
        title <- .Rd_format_title(.Rd_get_title(Rd))
        if (!length(title)) 
            title <- "No title found"
        of0(wr(paste0("Title: ", title)), "\n")
        aliasblks <- sections == "\\alias"
        if (any(aliasblks)) {
            aliases <- unlist(Rd[aliasblks])
            sp <- grep(" ", aliases, fixed = TRUE)
            aliases[sp] <- paste0("'", aliases[sp], "'")
            of0(wr(paste0("Aliases: ", paste(aliases, collapse = " "))), 
                "\n")
        }
        keyblks <- sections == "\\keyword"
        if (any(keyblks)) {
            keys <- unlist(Rd[keyblks])
            if (length(keys)) {
                keys <- psub("^\\s+", "", keys)
                of0(wr(paste("Keywords: ", paste0(keys, collapse = " "))), 
                  "\n")
            }
        }
        writeLines(c("", "### ** Examples"), con)
        for (i in seq_along(ex)) render(ex[[i]])
        of1("\n\n\n")
    }
    invisible(out)
}


Adobe_glyphs <- structure(list(adobe = c("controlSTX", "controlSOT", "controlETX", 
"controlEOT", "controlENQ", "controlACK", "controlBEL", "controlBS", 
"controlHT", "controlLF", "controlVT", "controlFF", "controlCR", 
"controlSO", "controlSI", "controlDLE", "controlDC1", "controlDC2", 
"controlDC3", "controlDC4", "controlNAK", "controlSYN", "controlETB", 
"controlCAN", "controlEM", "controlSUB", "controlESC", "controlFS", 
"controlGS", "controlRS", "controlUS", "space", "spacehackarabic", 
"exclam", "quotedbl", "numbersign", "dollar", "percent", "ampersand", 
"quotesingle", "parenleft", "parenright", "asterisk", "plus", 
"comma", "hyphen", "period", "slash", "zero", "one", "two", "three", 
"four", "five", "six", "seven", "eight", "nine", "colon", "semicolon", 
"less", "equal", "greater", "question", "at", "A", "B", "C", 
"D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", 
"Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "bracketleft", 
"backslash", "bracketright", "asciicircum", "underscore", "grave", 
"a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", 
"n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y", "z", 
"braceleft", "bar", "verticalbar", "braceright", "asciitilde", 
"controlDEL", "nbspace", "nonbreakingspace", "exclamdown", "cent", 
"sterling", "currency", "yen", "brokenbar", "section", "dieresis", 
"copyright", "ordfeminine", "guillemotleft", "logicalnot", "sfthyphen", 
"softhyphen", "registered", "macron", "overscore", "degree", 
"plusminus", "twosuperior", "threesuperior", "acute", "mu", "mu1", 
"paragraph", "middot", "periodcentered", "cedilla", "onesuperior", 
"ordmasculine", "guillemotright", "onequarter", "onehalf", "threequarters", 
"questiondown", "Agrave", "Aacute", "Acircumflex", "Atilde", 
"Adieresis", "Aring", "AE", "Ccedilla", "Egrave", "Eacute", "Ecircumflex", 
"Edieresis", "Igrave", "Iacute", "Icircumflex", "Idieresis", 
"Eth", "Ntilde", "Ograve", "Oacute", "Ocircumflex", "Otilde", 
"Odieresis", "multiply", "Oslash", "Ugrave", "Uacute", "Ucircumflex", 
"Udieresis", "Yacute", "Thorn", "germandbls", "agrave", "aacute", 
"acircumflex", "atilde", "adieresis", "aring", "ae", "ccedilla", 
"egrave", "eacute", "ecircumflex", "edieresis", "igrave", "iacute", 
"icircumflex", "idieresis", "eth", "ntilde", "ograve", "oacute", 
"ocircumflex", "otilde", "odieresis", "divide", "oslash", "ugrave", 
"uacute", "ucircumflex", "udieresis", "yacute", "thorn", "ydieresis", 
"Amacron", "amacron", "Abreve", "abreve", "Aogonek", "aogonek", 
"Cacute", "cacute", "Ccircumflex", "ccircumflex", "Cdot", "Cdotaccent", 
"cdot", "cdotaccent", "Ccaron", "ccaron", "Dcaron", "dcaron", 
"Dcroat", "Dslash", "dcroat", "dmacron", "Emacron", "emacron", 
"Ebreve", "ebreve", "Edot", "Edotaccent", "edot", "edotaccent", 
"Eogonek", "eogonek", "Ecaron", "ecaron", "Gcircumflex", "gcircumflex", 
"Gbreve", "gbreve", "Gdot", "Gdotaccent", "gdot", "gdotaccent", 
"Gcedilla", "Gcommaaccent", "gcedilla", "gcommaaccent", "Hcircumflex", 
"hcircumflex", "Hbar", "hbar", "Itilde", "itilde", "Imacron", 
"imacron", "Ibreve", "ibreve", "Iogonek", "iogonek", "Idot", 
"Idotaccent", "dotlessi", "IJ", "ij", "Jcircumflex", "jcircumflex", 
"Kcedilla", "Kcommaaccent", "kcedilla", "kcommaaccent", "kgreenlandic", 
"Lacute", "lacute", "Lcedilla", "Lcommaaccent", "lcedilla", "lcommaaccent", 
"Lcaron", "lcaron", "Ldot", "Ldotaccent", "ldot", "ldotaccent", 
"Lslash", "lslash", "Nacute", "nacute", "Ncedilla", "Ncommaaccent", 
"ncedilla", "ncommaaccent", "Ncaron", "ncaron", "napostrophe", 
"quoterightn", "Eng", "eng", "Omacron", "omacron", "Obreve", 
"obreve", "Odblacute", "Ohungarumlaut", "odblacute", "ohungarumlaut", 
"OE", "oe", "Racute", "racute", "Rcedilla", "Rcommaaccent", "rcedilla", 
"rcommaaccent", "Rcaron", "rcaron", "Sacute", "sacute", "Scircumflex", 
"scircumflex", "Scedilla", "scedilla", "Scaron", "scaron", "Tcedilla", 
"Tcommaaccent", "tcedilla", "tcommaaccent", "Tcaron", "tcaron", 
"Tbar", "tbar", "Utilde", "utilde", "Umacron", "umacron", "Ubreve", 
"ubreve", "Uring", "uring", "Udblacute", "Uhungarumlaut", "udblacute", 
"uhungarumlaut", "Uogonek", "uogonek", "Wcircumflex", "wcircumflex", 
"Ycircumflex", "ycircumflex", "Ydieresis", "Zacute", "zacute", 
"Zdot", "Zdotaccent", "zdot", "zdotaccent", "Zcaron", "zcaron", 
"longs", "slong", "bstroke", "Bhook", "Btopbar", "btopbar", "Tonesix", 
"tonesix", "Oopen", "Chook", "chook", "Dafrican", "Dhook", "Dtopbar", 
"dtopbar", "deltaturned", "Ereversed", "Schwa", "Eopen", "Fhook", 
"florin", "Ghook", "Gammaafrican", "hv", "Iotaafrican", "Istroke", 
"Khook", "khook", "lbar", "lambdastroke", "Mturned", "Nhookleft", 
"nlegrightlong", "Ocenteredtilde", "Ohorn", "ohorn", "Oi", "oi", 
"Phook", "phook", "yr", "Tonetwo", "tonetwo", "Esh", "eshreversedloop", 
"tpalatalhook", "Thook", "thook", "Tretroflexhook", "Uhorn", 
"uhorn", "Upsilonafrican", "Vhook", "Yhook", "yhook", "Zstroke", 
"zstroke", "Ezh", "Ezhreversed", "ezhreversed", "ezhtail", "twostroke", 
"Tonefive", "tonefive", "glottalinvertedstroke", "wynn", "clickdental", 
"clicklateral", "clickalveolar", "clickretroflex", "DZcaron", 
"Dzcaron", "dzcaron", "LJ", "Lj", "lj", "NJ", "Nj", "nj", "Acaron", 
"acaron", "Icaron", "icaron", "Ocaron", "ocaron", "Ucaron", "ucaron", 
"Udieresismacron", "udieresismacron", "Udieresisacute", "udieresisacute", 
"Udieresiscaron", "udieresiscaron", "Udieresisgrave", "udieresisgrave", 
"eturned", "Adieresismacron", "adieresismacron", "Adotmacron", 
"adotmacron", "AEmacron", "aemacron", "Gstroke", "gstroke", "Gcaron", 
"gcaron", "Kcaron", "kcaron", "Oogonek", "oogonek", "Oogonekmacron", 
"oogonekmacron", "Ezhcaron", "ezhcaron", "jcaron", "DZ", "Dz", 
"dz", "Gacute", "gacute", "Aringacute", "aringacute", "AEacute", 
"aeacute", "Oslashacute", "Ostrokeacute", "oslashacute", "ostrokeacute", 
"Adblgrave", "adblgrave", "Ainvertedbreve", "ainvertedbreve", 
"Edblgrave", "edblgrave", "Einvertedbreve", "einvertedbreve", 
"Idblgrave", "idblgrave", "Iinvertedbreve", "iinvertedbreve", 
"Odblgrave", "odblgrave", "Oinvertedbreve", "oinvertedbreve", 
"Rdblgrave", "rdblgrave", "Rinvertedbreve", "rinvertedbreve", 
"Udblgrave", "udblgrave", "Uinvertedbreve", "uinvertedbreve", 
"Scommaaccent", "scommaaccent", "aturned", "ascript", "ascriptturned", 
"bhook", "oopen", "ccurl", "dtail", "dhook", "ereversed", "schwa", 
"schwahook", "eopen", "eopenreversed", "eopenreversedhook", "eopenreversedclosed", 
"jdotlessstroke", "ghook", "gscript", "gammalatinsmall", "ramshorn", 
"hturned", "hhook", "henghook", "istroke", "iotalatin", "lmiddletilde", 
"lbelt", "lhookretroflex", "lezh", "mturned", "mlonglegturned", 
"mhook", "nhookleft", "nhookretroflex", "obarred", "omegalatinclosed", 
"philatin", "rturned", "rlonglegturned", "rhookturned", "rlongleg", 
"rhook", "rfishhook", "rfishhookreversed", "Rsmallinverted", 
"shook", "esh", "dotlessjstrokehook", "eshsquatreversed", "eshcurl", 
"tturned", "tretroflexhook", "ubar", "upsilonlatin", "vhook", 
"vturned", "wturned", "yturned", "zretroflexhook", "zcurl", "ezh", 
"ezhcurl", "glottalstop", "glottalstopreversed", "glottalstopinverted", 
"cstretched", "bilabialclick", "eopenclosed", "Gsmallhook", "jcrossedtail", 
"kturned", "qhook", "glottalstopstroke", "glottalstopstrokereversed", 
"dzaltone", "dezh", "dzcurl", "ts", "tesh", "tccurl", "hsuperior", 
"hhooksuperior", "jsuperior", "rturnedsuperior", "rhookturnedsuperior", 
"Rsmallinvertedsuperior", "wsuperior", "ysuperior", "primemod", 
"dblprimemod", "commaturnedmod", "afii57929", "apostrophemod", 
"afii64937", "commareversedmod", "ringhalfright", "ringhalfleft", 
"glottalstopmod", "glottalstopreversedmod", "arrowheadleftmod", 
"arrowheadrightmod", "arrowheadupmod", "arrowheaddownmod", "circumflex", 
"caron", "verticallinemod", "firsttonechinese", "secondtonechinese", 
"fourthtonechinese", "verticallinelowmod", "macronlowmod", "gravelowmod", 
"acutelowmod", "colontriangularmod", "colontriangularhalfmod", 
"ringhalfrightcentered", "ringhalfleftcentered", "uptackmod", 
"downtackmod", "plusmod", "minusmod", "breve", "dotaccent", "ring", 
"ogonek", "ilde", "tilde", "hungarumlaut", "rhotichookmod", "gammasuperior", 
"xsuperior", "glottalstopreversedsuperior", "tonebarextrahighmod", 
"tonebarhighmod", "tonebarmidmod", "tonebarlowmod", "tonebarextralowmod", 
"gravecmb", "gravecomb", "acutecmb", "acutecomb", "circumflexcmb", 
"tildecmb", "tildecomb", "macroncmb", "overlinecmb", "brevecmb", 
"dotaccentcmb", "dieresiscmb", "hookabovecomb", "hookcmb", "ringcmb", 
"hungarumlautcmb", "caroncmb", "verticallineabovecmb", "dblverticallineabovecmb", 
"dblgravecmb", "candrabinducmb", "breveinvertedcmb", "commaturnedabovecmb", 
"commaabovecmb", "commareversedabovecmb", "commaaboverightcmb", 
"gravebelowcmb", "acutebelowcmb", "lefttackbelowcmb", "righttackbelowcmb", 
"leftangleabovecmb", "horncmb", "ringhalfleftbelowcmb", "uptackbelowcmb", 
"downtackbelowcmb", "plusbelowcmb", "minusbelowcmb", "hookpalatalizedbelowcmb", 
"hookretroflexbelowcmb", "dotbelowcmb", "dotbelowcomb", "dieresisbelowcmb", 
"ringbelowcmb", "cedillacmb", "ogonekcmb", "verticallinebelowcmb", 
"bridgebelowcmb", "dblarchinvertedbelowcmb", "caronbelowcmb", 
"circumflexbelowcmb", "brevebelowcmb", "breveinvertedbelowcmb", 
"tildebelowcmb", "macronbelowcmb", "lowlinecmb", "dbllowlinecmb", 
"tildeoverlaycmb", "strokeshortoverlaycmb", "strokelongoverlaycmb", 
"solidusshortoverlaycmb", "soliduslongoverlaycmb", "ringhalfrightbelowcmb", 
"bridgeinvertedbelowcmb", "squarebelowcmb", "seagullbelowcmb", 
"xabovecmb", "tildeverticalcmb", "dbloverlinecmb", "gravetonecmb", 
"acutetonecmb", "perispomenigreekcmb", "koroniscmb", "dialytikatonoscmb", 
"ypogegrammenigreekcmb", "tildedoublecmb", "breveinverteddoublecmb", 
"numeralsigngreek", "numeralsignlowergreek", "ypogegrammeni", 
"questiongreek", "tonos", "dialytikatonos", "dieresistonos", 
"Alphatonos", "anoteleia", "Epsilontonos", "Etatonos", "Iotatonos", 
"Omicrontonos", "Upsilontonos", "Omegatonos", "iotadieresistonos", 
"Alpha", "Beta", "Gamma", "Deltagreek", "Epsilon", "Zeta", "Eta", 
"Theta", "Iota", "Kappa", "Lambda", "Mu", "Nu", "Xi", "Omicron", 
"Pi", "Rho", "Sigma", "Tau", "Upsilon", "Phi", "Chi", "Psi", 
"Omegagreek", "Iotadieresis", "Upsilondieresis", "alphatonos", 
"epsilontonos", "etatonos", "iotatonos", "upsilondieresistonos", 
"alpha", "beta", "gamma", "delta", "epsilon", "zeta", "eta", 
"theta", "iota", "kappa", "lambda", "mugreek", "nu", "xi", "omicron", 
"pi", "rho", "sigma1", "sigmafinal", "sigma", "tau", "upsilon", 
"phi", "chi", "psi", "omega", "iotadieresis", "upsilondieresis", 
"omicrontonos", "upsilontonos", "omegatonos", "betasymbolgreek", 
"theta1", "thetasymbolgreek", "Upsilon1", "Upsilonhooksymbol", 
"Upsilonacutehooksymbolgreek", "Upsilondieresishooksymbolgreek", 
"phi1", "phisymbolgreek", "omega1", "pisymbolgreek", "Stigmagreek", 
"Digammagreek", "Koppagreek", "Sampigreek", "Sheicoptic", "sheicoptic", 
"Feicoptic", "feicoptic", "Kheicoptic", "kheicoptic", "Horicoptic", 
"horicoptic", "Gangiacoptic", "gangiacoptic", "Shimacoptic", 
"shimacoptic", "Deicoptic", "deicoptic", "kappasymbolgreek", 
"rhosymbolgreek", "sigmalunatesymbolgreek", "yotgreek", "Iocyrillic", 
"afii10023", "Djecyrillic", "afii10051", "Gjecyrillic", "afii10052", 
"Ecyrillic", "afii10053", "Dzecyrillic", "afii10054", "Icyrillic", 
"afii10055", "Yicyrillic", "afii10056", "Jecyrillic", "afii10057", 
"Ljecyrillic", "afii10058", "Njecyrillic", "afii10059", "Tshecyrillic", 
"afii10060", "Kjecyrillic", "afii10061", "Ushortcyrillic", "afii10062", 
"Dzhecyrillic", "afii10145", "Acyrillic", "afii10017", "Becyrillic", 
"afii10018", "Vecyrillic", "afii10019", "Gecyrillic", "afii10020", 
"Decyrillic", "afii10021", "Iecyrillic", "afii10022", "Zhecyrillic", 
"afii10024", "Zecyrillic", "afii10025", "Iicyrillic", "afii10026", 
"Iishortcyrillic", "afii10027", "Kacyrillic", "afii10028", "Elcyrillic", 
"afii10029", "Emcyrillic", "afii10030", "Encyrillic", "afii10031", 
"Ocyrillic", "afii10032", "Pecyrillic", "afii10033", "Ercyrillic", 
"afii10034", "Escyrillic", "afii10035", "Tecyrillic", "afii10036", 
"Ucyrillic", "afii10037", "Efcyrillic", "afii10038", "Khacyrillic", 
"afii10039", "Tsecyrillic", "afii10040", "Checyrillic", "afii10041", 
"Shacyrillic", "afii10042", "Shchacyrillic", "afii10043", "Hardsigncyrillic", 
"afii10044", "Yericyrillic", "afii10045", "Softsigncyrillic", 
"afii10046", "Ereversedcyrillic", "afii10047", "IUcyrillic", 
"afii10048", "IAcyrillic", "afii10049", "acyrillic", "afii10065", 
"afii10066", "becyrillic", "afii10067", "vecyrillic", "afii10068", 
"gecyrillic", "afii10069", "decyrillic", "afii10070", "iecyrillic", 
"afii10072", "zhecyrillic", "afii10073", "zecyrillic", "afii10074", 
"iicyrillic", "afii10075", "iishortcyrillic", "afii10076", "kacyrillic", 
"afii10077", "elcyrillic", "afii10078", "emcyrillic", "afii10079", 
"encyrillic", "afii10080", "ocyrillic", "afii10081", "pecyrillic", 
"afii10082", "ercyrillic", "afii10083", "escyrillic", "afii10084", 
"tecyrillic", "afii10085", "ucyrillic", "afii10086", "efcyrillic", 
"afii10087", "khacyrillic", "afii10088", "tsecyrillic", "afii10089", 
"checyrillic", "afii10090", "shacyrillic", "afii10091", "shchacyrillic", 
"afii10092", "hardsigncyrillic", "afii10093", "yericyrillic", 
"afii10094", "softsigncyrillic", "afii10095", "ereversedcyrillic", 
"afii10096", "iucyrillic", "afii10097", "iacyrillic", "afii10071", 
"iocyrillic", "afii10099", "djecyrillic", "afii10100", "gjecyrillic", 
"afii10101", "ecyrillic", "afii10102", "dzecyrillic", "afii10103", 
"icyrillic", "afii10104", "yicyrillic", "afii10105", "jecyrillic", 
"afii10106", "ljecyrillic", "afii10107", "njecyrillic", "afii10108", 
"tshecyrillic", "afii10109", "kjecyrillic", "afii10110", "ushortcyrillic", 
"afii10193", "dzhecyrillic", "Omegacyrillic", "omegacyrillic", 
"Yatcyrillic", "afii10146", "afii10194", "yatcyrillic", "Eiotifiedcyrillic", 
"eiotifiedcyrillic", "Yuslittlecyrillic", "yuslittlecyrillic", 
"Yuslittleiotifiedcyrillic", "yuslittleiotifiedcyrillic", "Yusbigcyrillic", 
"yusbigcyrillic", "Yusbigiotifiedcyrillic", "yusbigiotifiedcyrillic", 
"Ksicyrillic", "ksicyrillic", "Psicyrillic", "psicyrillic", "Fitacyrillic", 
"afii10147", "afii10195", "fitacyrillic", "Izhitsacyrillic", 
"afii10148", "afii10196", "izhitsacyrillic", "Izhitsadblgravecyrillic", 
"izhitsadblgravecyrillic", "Ukcyrillic", "ukcyrillic", "Omegaroundcyrillic", 
"omegaroundcyrillic", "Omegatitlocyrillic", "omegatitlocyrillic", 
"Otcyrillic", "otcyrillic", "Koppacyrillic", "koppacyrillic", 
"thousandcyrillic", "titlocyrilliccmb", "palatalizationcyrilliccmb", 
"dasiapneumatacyrilliccmb", "psilipneumatacyrilliccmb", "Gheupturncyrillic", 
"afii10050", "afii10098", "gheupturncyrillic", "Ghestrokecyrillic", 
"ghestrokecyrillic", "Ghemiddlehookcyrillic", "ghemiddlehookcyrillic", 
"Zhedescendercyrillic", "zhedescendercyrillic", "Zedescendercyrillic", 
"zedescendercyrillic", "Kadescendercyrillic", "kadescendercyrillic", 
"Kaverticalstrokecyrillic", "kaverticalstrokecyrillic", "Kastrokecyrillic", 
"kastrokecyrillic", "Kabashkircyrillic", "kabashkircyrillic", 
"Endescendercyrillic", "endescendercyrillic", "Enghecyrillic", 
"enghecyrillic", "Pemiddlehookcyrillic", "pemiddlehookcyrillic", 
"Haabkhasiancyrillic", "haabkhasiancyrillic", "Esdescendercyrillic", 
"esdescendercyrillic", "Tedescendercyrillic", "tedescendercyrillic", 
"Ustraightcyrillic", "ustraightcyrillic", "Ustraightstrokecyrillic", 
"ustraightstrokecyrillic", "Hadescendercyrillic", "hadescendercyrillic", 
"Tetsecyrillic", "tetsecyrillic", "Chedescendercyrillic", "chedescendercyrillic", 
"Cheverticalstrokecyrillic", "cheverticalstrokecyrillic", "Shhacyrillic", 
"shhacyrillic", "Cheabkhasiancyrillic", "cheabkhasiancyrillic", 
"Chedescenderabkhasiancyrillic", "chedescenderabkhasiancyrillic", 
"palochkacyrillic", "Zhebrevecyrillic", "zhebrevecyrillic", "Kahookcyrillic", 
"kahookcyrillic", "Enhookcyrillic", "enhookcyrillic", "Chekhakassiancyrillic", 
"chekhakassiancyrillic", "Abrevecyrillic", "abrevecyrillic", 
"Adieresiscyrillic", "adieresiscyrillic", "Aiecyrillic", "aiecyrillic", 
"Iebrevecyrillic", "iebrevecyrillic", "Schwacyrillic", "afii10846", 
"schwacyrillic", "Schwadieresiscyrillic", "schwadieresiscyrillic", 
"Zhedieresiscyrillic", "zhedieresiscyrillic", "Zedieresiscyrillic", 
"zedieresiscyrillic", "Dzeabkhasiancyrillic", "dzeabkhasiancyrillic", 
"Imacroncyrillic", "imacroncyrillic", "Idieresiscyrillic", "idieresiscyrillic", 
"Odieresiscyrillic", "odieresiscyrillic", "Obarredcyrillic", 
"obarredcyrillic", "Obarreddieresiscyrillic", "obarreddieresiscyrillic", 
"Umacroncyrillic", "umacroncyrillic", "Udieresiscyrillic", "udieresiscyrillic", 
"Uhungarumlautcyrillic", "uhungarumlautcyrillic", "Chedieresiscyrillic", 
"chedieresiscyrillic", "Yerudieresiscyrillic", "yerudieresiscyrillic", 
"Aybarmenian", "Benarmenian", "Gimarmenian", "Daarmenian", "Echarmenian", 
"Zaarmenian", "Eharmenian", "Etarmenian", "Toarmenian", "Zhearmenian", 
"Iniarmenian", "Liwnarmenian", "Xeharmenian", "Caarmenian", "Kenarmenian", 
"Hoarmenian", "Jaarmenian", "Ghadarmenian", "Cheharmenian", "Menarmenian", 
"Yiarmenian", "Nowarmenian", "Shaarmenian", "Voarmenian", "Chaarmenian", 
"Peharmenian", "Jheharmenian", "Raarmenian", "Seharmenian", "Vewarmenian", 
"Tiwnarmenian", "Reharmenian", "Coarmenian", "Yiwnarmenian", 
"Piwrarmenian", "Keharmenian", "Oharmenian", "Feharmenian", "ringhalfleftarmenian", 
"apostrophearmenian", "emphasismarkarmenian", "exclamarmenian", 
"commaarmenian", "questionarmenian", "abbreviationmarkarmenian", 
"aybarmenian", "benarmenian", "gimarmenian", "daarmenian", "echarmenian", 
"zaarmenian", "eharmenian", "etarmenian", "toarmenian", "zhearmenian", 
"iniarmenian", "liwnarmenian", "xeharmenian", "caarmenian", "kenarmenian", 
"hoarmenian", "jaarmenian", "ghadarmenian", "cheharmenian", "menarmenian", 
"yiarmenian", "nowarmenian", "shaarmenian", "voarmenian", "chaarmenian", 
"peharmenian", "jheharmenian", "raarmenian", "seharmenian", "vewarmenian", 
"tiwnarmenian", "reharmenian", "coarmenian", "yiwnarmenian", 
"piwrarmenian", "keharmenian", "oharmenian", "feharmenian", "echyiwnarmenian", 
"periodarmenian", "etnahtafoukhhebrew", "etnahtafoukhlefthebrew", 
"etnahtahebrew", "etnahtalefthebrew", "segoltahebrew", "shalshelethebrew", 
"zaqefqatanhebrew", "zaqefgadolhebrew", "tipehahebrew", "tipehalefthebrew", 
"reviahebrew", "reviamugrashhebrew", "zarqahebrew", "pashtahebrew", 
"yetivhebrew", "tevirhebrew", "tevirlefthebrew", "gereshaccenthebrew", 
"gereshmuqdamhebrew", "gershayimaccenthebrew", "qarneyparahebrew", 
"telishagedolahebrew", "pazerhebrew", "munahhebrew", "munahlefthebrew", 
"mahapakhhebrew", "mahapakhlefthebrew", "merkhahebrew", "merkhalefthebrew", 
"merkhakefulahebrew", "merkhakefulalefthebrew", "dargahebrew", 
"dargalefthebrew", "qadmahebrew", "telishaqetanahebrew", "yerahbenyomohebrew", 
"yerahbenyomolefthebrew", "olehebrew", "iluyhebrew", "dehihebrew", 
"zinorhebrew", "masoracirclehebrew", "afii57799", "sheva", "sheva115", 
"sheva15", "sheva22", "sheva2e", "shevahebrew", "shevanarrowhebrew", 
"shevaquarterhebrew", "shevawidehebrew", "afii57801", "hatafsegol", 
"hatafsegol17", "hatafsegol24", "hatafsegol30", "hatafsegolhebrew", 
"hatafsegolnarrowhebrew", "hatafsegolquarterhebrew", "hatafsegolwidehebrew", 
"afii57800", "hatafpatah", "hatafpatah16", "hatafpatah23", "hatafpatah2f", 
"hatafpatahhebrew", "hatafpatahnarrowhebrew", "hatafpatahquarterhebrew", 
"hatafpatahwidehebrew", "afii57802", "hatafqamats", "hatafqamats1b", 
"hatafqamats28", "hatafqamats34", "hatafqamatshebrew", "hatafqamatsnarrowhebrew", 
"hatafqamatsquarterhebrew", "hatafqamatswidehebrew", "afii57793", 
"hiriq", "hiriq14", "hiriq21", "hiriq2d", "hiriqhebrew", "hiriqnarrowhebrew", 
"hiriqquarterhebrew", "hiriqwidehebrew", "afii57794", "tsere", 
"tsere12", "tsere1e", "tsere2b", "tserehebrew", "tserenarrowhebrew", 
"tserequarterhebrew", "tserewidehebrew", "afii57795", "segol", 
"segol13", "segol1f", "segol2c", "segolhebrew", "segolnarrowhebrew", 
"segolquarterhebrew", "segolwidehebrew", "afii57798", "patah", 
"patah11", "patah1d", "patah2a", "patahhebrew", "patahnarrowhebrew", 
"patahquarterhebrew", "patahwidehebrew", "afii57797", "qamats", 
"qamats10", "qamats1a", "qamats1c", "qamats27", "qamats29", "qamats33", 
"qamatsde", "qamatshebrew", "qamatsnarrowhebrew", "qamatsqatanhebrew", 
"qamatsqatannarrowhebrew", "qamatsqatanquarterhebrew", "qamatsqatanwidehebrew", 
"qamatsquarterhebrew", "qamatswidehebrew", "afii57806", "holam", 
"holam19", "holam26", "holam32", "holamhebrew", "holamnarrowhebrew", 
"holamquarterhebrew", "holamwidehebrew", "afii57796", "qubuts", 
"qubuts18", "qubuts25", "qubuts31", "qubutshebrew", "qubutsnarrowhebrew", 
"qubutsquarterhebrew", "qubutswidehebrew", "afii57807", "dagesh", 
"dageshhebrew", "afii57839", "siluqhebrew", "siluqlefthebrew", 
"afii57645", "maqafhebrew", "afii57841", "rafe", "rafehebrew", 
"afii57842", "paseqhebrew", "afii57804", "shindothebrew", "afii57803", 
"sindothebrew", "afii57658", "sofpasuqhebrew", "upperdothebrew", 
"afii57664", "alef", "alefhebrew", "afii57665", "bet", "bethebrew", 
"afii57666", "gimel", "gimelhebrew", "afii57667", "dalet", "afii57668", 
"he", "hehebrew", "afii57669", "vav", "vavhebrew", "afii57670", 
"zayin", "zayinhebrew", "afii57671", "het", "hethebrew", "afii57672", 
"tet", "tethebrew", "afii57673", "yod", "yodhebrew", "afii57674", 
"finalkaf", "finalkafhebrew", "afii57675", "kaf", "kafhebrew", 
"afii57676", "lamed", "lamedhebrew", "afii57677", "finalmem", 
"finalmemhebrew", "afii57678", "mem", "memhebrew", "afii57679", 
"finalnun", "finalnunhebrew", "afii57680", "nun", "nunhebrew", 
"afii57681", "samekh", "samekhhebrew", "afii57682", "ayin", "ayinhebrew", 
"afii57683", "finalpe", "finalpehebrew", "afii57684", "pe", "pehebrew", 
"afii57685", "finaltsadi", "finaltsadihebrew", "afii57686", "tsadi", 
"tsadihebrew", "afii57687", "qof", "qofhebrew", "afii57688", 
"resh", "reshhebrew", "afii57689", "shin", "shinhebrew", "afii57690", 
"tav", "tavhebrew", "afii57716", "vavvavhebrew", "afii57717", 
"vavyodhebrew", "afii57718", "yodyodhebrew", "gereshhebrew", 
"gershayimhebrew", "afii57388", "commaarabic", "afii57403", "semicolonarabic", 
"afii57407", "questionarabic", "afii57409", "hamzaarabic", "hamzalowarabic", 
"afii57410", "alefmaddaabovearabic", "afii57411", "alefhamzaabovearabic", 
"afii57412", "wawhamzaabovearabic", "afii57413", "alefhamzabelowarabic", 
"afii57414", "yehhamzaabovearabic", "afii57415", "alefarabic", 
"afii57416", "beharabic", "afii57417", "tehmarbutaarabic", "afii57418", 
"teharabic", "afii57419", "theharabic", "afii57420", "jeemarabic", 
"afii57421", "haharabic", "afii57422", "khaharabic", "afii57423", 
"dalarabic", "afii57424", "thalarabic", "afii57425", "reharabic", 
"afii57426", "zainarabic", "afii57427", "seenarabic", "afii57428", 
"sheenarabic", "afii57429", "sadarabic", "afii57430", "dadarabic", 
"afii57431", "taharabic", "afii57432", "zaharabic", "afii57433", 
"ainarabic", "afii57434", "ghainarabic", "afii57440", "kashidaautoarabic", 
"kashidaautonosidebearingarabic", "tatweelarabic", "afii57441", 
"feharabic", "afii57442", "qafarabic", "afii57443", "kafarabic", 
"afii57444", "lamarabic", "afii57445", "meemarabic", "afii57446", 
"noonarabic", "afii57470", "heharabic", "afii57448", "wawarabic", 
"afii57449", "alefmaksuraarabic", "afii57450", "yeharabic", "afii57451", 
"fathatanarabic", "afii57452", "dammatanaltonearabic", "dammatanarabic", 
"afii57453", "kasratanarabic", "afii57454", "fathaarabic", "fathalowarabic", 
"afii57455", "dammaarabic", "dammalowarabic", "afii57456", "kasraarabic", 
"afii57457", "shaddaarabic", "afii57458", "sukunarabic", "afii57392", 
"zeroarabic", "zerohackarabic", "afii57393", "onearabic", "onehackarabic", 
"afii57394", "twoarabic", "twohackarabic", "afii57395", "threearabic", 
"threehackarabic", "afii57396", "fourarabic", "fourhackarabic", 
"afii57397", "fivearabic", "fivehackarabic", "afii57398", "sixarabic", 
"sixhackarabic", "afii57399", "sevenarabic", "sevenhackarabic", 
"afii57400", "eightarabic", "eighthackarabic", "afii57401", "ninearabic", 
"ninehackarabic", "afii57381", "percentarabic", "decimalseparatorarabic", 
"decimalseparatorpersian", "thousandsseparatorarabic", "thousandsseparatorpersian", 
"afii63167", "asteriskaltonearabic", "asteriskarabic", "afii57511", 
"tteharabic", "afii57506", "peharabic", "afii57507", "tcheharabic", 
"afii57512", "ddalarabic", "afii57513", "rreharabic", "afii57508", 
"jeharabic", "afii57505", "veharabic", "afii57509", "gafarabic", 
"afii57514", "noonghunnaarabic", "haaltonearabic", "hehaltonearabic", 
"yehthreedotsbelowarabic", "afii57519", "yehbarreearabic", "afii57534", 
"zeropersian", "onepersian", "twopersian", "threepersian", "fourpersian", 
"fivepersian", "sixpersian", "sevenpersian", "eightpersian", 
"ninepersian", "candrabindudeva", "anusvaradeva", "visargadeva", 
"adeva", "aadeva", "ideva", "iideva", "udeva", "uudeva", "rvocalicdeva", 
"lvocalicdeva", "ecandradeva", "eshortdeva", "edeva", "aideva", 
"ocandradeva", "oshortdeva", "odeva", "audeva", "kadeva", "khadeva", 
"gadeva", "ghadeva", "ngadeva", "cadeva", "chadeva", "jadeva", 
"jhadeva", "nyadeva", "ttadeva", "tthadeva", "ddadeva", "ddhadeva", 
"nnadeva", "tadeva", "thadeva", "dadeva", "dhadeva", "nadeva", 
"nnnadeva", "padeva", "phadeva", "badeva", "bhadeva", "madeva", 
"yadeva", "radeva", "rradeva", "ladeva", "lladeva", "llladeva", 
"vadeva", "shadeva", "ssadeva", "sadeva", "hadeva", "nuktadeva", 
"avagrahadeva", "aavowelsigndeva", "ivowelsigndeva", "iivowelsigndeva", 
"uvowelsigndeva", "uuvowelsigndeva", "rvocalicvowelsigndeva", 
"rrvocalicvowelsigndeva", "ecandravowelsigndeva", "eshortvowelsigndeva", 
"evowelsigndeva", "aivowelsigndeva", "ocandravowelsigndeva", 
"oshortvowelsigndeva", "ovowelsigndeva", "auvowelsigndeva", "viramadeva", 
"omdeva", "udattadeva", "anudattadeva", "gravedeva", "acutedeva", 
"qadeva", "khhadeva", "ghhadeva", "zadeva", "dddhadeva", "rhadeva", 
"fadeva", "yyadeva", "rrvocalicdeva", "llvocalicdeva", "lvocalicvowelsigndeva", 
"llvocalicvowelsigndeva", "danda", "dbldanda", "zerodeva", "onedeva", 
"twodeva", "threedeva", "fourdeva", "fivedeva", "sixdeva", "sevendeva", 
"eightdeva", "ninedeva", "abbreviationsigndeva", "candrabindubengali", 
"anusvarabengali", "visargabengali", "abengali", "aabengali", 
"ibengali", "iibengali", "ubengali", "uubengali", "rvocalicbengali", 
"lvocalicbengali", "ebengali", "aibengali", "obengali", "aubengali", 
"kabengali", "khabengali", "gabengali", "ghabengali", "ngabengali", 
"cabengali", "chabengali", "jabengali", "jhabengali", "nyabengali", 
"ttabengali", "tthabengali", "ddabengali", "ddhabengali", "nnabengali", 
"tabengali", "thabengali", "dabengali", "dhabengali", "nabengali", 
"pabengali", "phabengali", "babengali", "bhabengali", "mabengali", 
"yabengali", "rabengali", "labengali", "shabengali", "ssabengali", 
"sabengali", "habengali", "nuktabengali", "aavowelsignbengali", 
"ivowelsignbengali", "iivowelsignbengali", "uvowelsignbengali", 
"uuvowelsignbengali", "rvocalicvowelsignbengali", "rrvocalicvowelsignbengali", 
"evowelsignbengali", "aivowelsignbengali", "ovowelsignbengali", 
"auvowelsignbengali", "viramabengali", "aulengthmarkbengali", 
"rrabengali", "rhabengali", "yyabengali", "rrvocalicbengali", 
"llvocalicbengali", "lvocalicvowelsignbengali", "llvocalicvowelsignbengali", 
"zerobengali", "onebengali", "twobengali", "threebengali", "fourbengali", 
"fivebengali", "sixbengali", "sevenbengali", "eightbengali", 
"ninebengali", "ramiddlediagonalbengali", "ralowerdiagonalbengali", 
"rupeemarkbengali", "rupeesignbengali", "onenumeratorbengali", 
"twonumeratorbengali", "threenumeratorbengali", "fournumeratorbengali", 
"denominatorminusonenumeratorbengali", "sixteencurrencydenominatorbengali", 
"issharbengali", "bindigurmukhi", "agurmukhi", "aagurmukhi", 
"igurmukhi", "iigurmukhi", "ugurmukhi", "uugurmukhi", "eegurmukhi", 
"aigurmukhi", "oogurmukhi", "augurmukhi", "kagurmukhi", "khagurmukhi", 
"gagurmukhi", "ghagurmukhi", "ngagurmukhi", "cagurmukhi", "chagurmukhi", 
"jagurmukhi", "jhagurmukhi", "nyagurmukhi", "ttagurmukhi", "tthagurmukhi", 
"ddagurmukhi", "ddhagurmukhi", "nnagurmukhi", "tagurmukhi", "thagurmukhi", 
"dagurmukhi", "dhagurmukhi", "nagurmukhi", "pagurmukhi", "phagurmukhi", 
"bagurmukhi", "bhagurmukhi", "magurmukhi", "yagurmukhi", "ragurmukhi", 
"lagurmukhi", "vagurmukhi", "shagurmukhi", "sagurmukhi", "hagurmukhi", 
"nuktagurmukhi", "aamatragurmukhi", "imatragurmukhi", "iimatragurmukhi", 
"umatragurmukhi", "uumatragurmukhi", "eematragurmukhi", "aimatragurmukhi", 
"oomatragurmukhi", "aumatragurmukhi", "halantgurmukhi", "khhagurmukhi", 
"ghhagurmukhi", "zagurmukhi", "rragurmukhi", "fagurmukhi", "zerogurmukhi", 
"onegurmukhi", "twogurmukhi", "threegurmukhi", "fourgurmukhi", 
"fivegurmukhi", "sixgurmukhi", "sevengurmukhi", "eightgurmukhi", 
"ninegurmukhi", "tippigurmukhi", "addakgurmukhi", "irigurmukhi", 
"uragurmukhi", "ekonkargurmukhi", "candrabindugujarati", "anusvaragujarati", 
"visargagujarati", "agujarati", "aagujarati", "igujarati", "iigujarati", 
"ugujarati", "uugujarati", "rvocalicgujarati", "ecandragujarati", 
"egujarati", "aigujarati", "ocandragujarati", "ogujarati", "augujarati", 
"kagujarati", "khagujarati", "gagujarati", "ghagujarati", "ngagujarati", 
"cagujarati", "chagujarati", "jagujarati", "jhagujarati", "nyagujarati", 
"ttagujarati", "tthagujarati", "ddagujarati", "ddhagujarati", 
"nnagujarati", "tagujarati", "thagujarati", "dagujarati", "dhagujarati", 
"nagujarati", "pagujarati", "phagujarati", "bagujarati", "bhagujarati", 
"magujarati", "yagujarati", "ragujarati", "lagujarati", "llagujarati", 
"vagujarati", "shagujarati", "ssagujarati", "sagujarati", "hagujarati", 
"nuktagujarati", "aavowelsigngujarati", "ivowelsigngujarati", 
"iivowelsigngujarati", "uvowelsigngujarati", "uuvowelsigngujarati", 
"rvocalicvowelsigngujarati", "rrvocalicvowelsigngujarati", "ecandravowelsigngujarati", 
"evowelsigngujarati", "aivowelsigngujarati", "ocandravowelsigngujarati", 
"ovowelsigngujarati", "auvowelsigngujarati", "viramagujarati", 
"omgujarati", "rrvocalicgujarati", "zerogujarati", "onegujarati", 
"twogujarati", "threegujarati", "fourgujarati", "fivegujarati", 
"sixgujarati", "sevengujarati", "eightgujarati", "ninegujarati", 
"kokaithai", "khokhaithai", "khokhuatthai", "khokhwaithai", "khokhonthai", 
"khorakhangthai", "ngonguthai", "chochanthai", "chochingthai", 
"chochangthai", "sosothai", "chochoethai", "yoyingthai", "dochadathai", 
"topatakthai", "thothanthai", "thonangmonthothai", "thophuthaothai", 
"nonenthai", "dodekthai", "totaothai", "thothungthai", "thothahanthai", 
"thothongthai", "nonuthai", "bobaimaithai", "poplathai", "phophungthai", 
"fofathai", "phophanthai", "fofanthai", "phosamphaothai", "momathai", 
"yoyakthai", "roruathai", "ruthai", "lolingthai", "luthai", "wowaenthai", 
"sosalathai", "sorusithai", "sosuathai", "hohipthai", "lochulathai", 
"oangthai", "honokhukthai", "paiyannoithai", "saraathai", "maihanakatthai", 
"saraaathai", "saraamthai", "saraithai", "saraiithai", "sarauethai", 
"saraueethai", "sarauthai", "sarauuthai", "phinthuthai", "bahtthai", 
"saraethai", "saraaethai", "saraothai", "saraaimaimuanthai", 
"saraaimaimalaithai", "lakkhangyaothai", "maiyamokthai", "maitaikhuthai", 
"maiekthai", "maithothai", "maitrithai", "maichattawathai", "thanthakhatthai", 
"nikhahitthai", "yamakkanthai", "fongmanthai", "zerothai", "onethai", 
"twothai", "threethai", "fourthai", "fivethai", "sixthai", "seventhai", 
"eightthai", "ninethai", "angkhankhuthai", "khomutthai", "Aringbelow", 
"aringbelow", "Bdotaccent", "bdotaccent", "Bdotbelow", "bdotbelow", 
"Blinebelow", "blinebelow", "Ccedillaacute", "ccedillaacute", 
"Ddotaccent", "ddotaccent", "Ddotbelow", "ddotbelow", "Dlinebelow", 
"dlinebelow", "Dcedilla", "dcedilla", "Dcircumflexbelow", "dcircumflexbelow", 
"Emacrongrave", "emacrongrave", "Emacronacute", "emacronacute", 
"Ecircumflexbelow", "ecircumflexbelow", "Etildebelow", "etildebelow", 
"Ecedillabreve", "ecedillabreve", "Fdotaccent", "fdotaccent", 
"Gmacron", "gmacron", "Hdotaccent", "hdotaccent", "Hdotbelow", 
"hdotbelow", "Hdieresis", "hdieresis", "Hcedilla", "hcedilla", 
"Hbrevebelow", "hbrevebelow", "Itildebelow", "itildebelow", "Idieresisacute", 
"idieresisacute", "Kacute", "kacute", "Kdotbelow", "kdotbelow", 
"Klinebelow", "klinebelow", "Ldotbelow", "ldotbelow", "Ldotbelowmacron", 
"ldotbelowmacron", "Llinebelow", "llinebelow", "Lcircumflexbelow", 
"lcircumflexbelow", "Macute", "macute", "Mdotaccent", "mdotaccent", 
"Mdotbelow", "mdotbelow", "Ndotaccent", "ndotaccent", "Ndotbelow", 
"ndotbelow", "Nlinebelow", "nlinebelow", "Ncircumflexbelow", 
"ncircumflexbelow", "Otildeacute", "otildeacute", "Otildedieresis", 
"otildedieresis", "Omacrongrave", "omacrongrave", "Omacronacute", 
"omacronacute", "Pacute", "pacute", "Pdotaccent", "pdotaccent", 
"Rdotaccent", "rdotaccent", "Rdotbelow", "rdotbelow", "Rdotbelowmacron", 
"rdotbelowmacron", "Rlinebelow", "rlinebelow", "Sdotaccent", 
"sdotaccent", "Sdotbelow", "sdotbelow", "Sacutedotaccent", "sacutedotaccent", 
"Scarondotaccent", "scarondotaccent", "Sdotbelowdotaccent", "sdotbelowdotaccent", 
"Tdotaccent", "tdotaccent", "Tdotbelow", "tdotbelow", "Tlinebelow", 
"tlinebelow", "Tcircumflexbelow", "tcircumflexbelow", "Udieresisbelow", 
"udieresisbelow", "Utildebelow", "utildebelow", "Ucircumflexbelow", 
"ucircumflexbelow", "Utildeacute", "utildeacute", "Umacrondieresis", 
"umacrondieresis", "Vtilde", "vtilde", "Vdotbelow", "vdotbelow", 
"Wgrave", "wgrave", "Wacute", "wacute", "Wdieresis", "wdieresis", 
"Wdotaccent", "wdotaccent", "Wdotbelow", "wdotbelow", "Xdotaccent", 
"xdotaccent", "Xdieresis", "xdieresis", "Ydotaccent", "ydotaccent", 
"Zcircumflex", "zcircumflex", "Zdotbelow", "zdotbelow", "Zlinebelow", 
"zlinebelow", "hlinebelow", "tdieresis", "wring", "yring", "arighthalfring", 
"slongdotaccent", "Adotbelow", "adotbelow", "Ahookabove", "ahookabove", 
"Acircumflexacute", "acircumflexacute", "Acircumflexgrave", "acircumflexgrave", 
"Acircumflexhookabove", "acircumflexhookabove", "Acircumflextilde", 
"acircumflextilde", "Acircumflexdotbelow", "acircumflexdotbelow", 
"Abreveacute", "abreveacute", "Abrevegrave", "abrevegrave", "Abrevehookabove", 
"abrevehookabove", "Abrevetilde", "abrevetilde", "Abrevedotbelow", 
"abrevedotbelow", "Edotbelow", "edotbelow", "Ehookabove", "ehookabove", 
"Etilde", "etilde", "Ecircumflexacute", "ecircumflexacute", "Ecircumflexgrave", 
"ecircumflexgrave", "Ecircumflexhookabove", "ecircumflexhookabove", 
"Ecircumflextilde", "ecircumflextilde", "Ecircumflexdotbelow", 
"ecircumflexdotbelow", "Ihookabove", "ihookabove", "Idotbelow", 
"idotbelow", "Odotbelow", "odotbelow", "Ohookabove", "ohookabove", 
"Ocircumflexacute", "ocircumflexacute", "Ocircumflexgrave", "ocircumflexgrave", 
"Ocircumflexhookabove", "ocircumflexhookabove", "Ocircumflextilde", 
"ocircumflextilde", "Ocircumflexdotbelow", "ocircumflexdotbelow", 
"Ohornacute", "ohornacute", "Ohorngrave", "ohorngrave", "Ohornhookabove", 
"ohornhookabove", "Ohorntilde", "ohorntilde", "Ohorndotbelow", 
"ohorndotbelow", "Udotbelow", "udotbelow", "Uhookabove", "uhookabove", 
"Uhornacute", "uhornacute", "Uhorngrave", "uhorngrave", "Uhornhookabove", 
"uhornhookabove", "Uhorntilde", "uhorntilde", "Uhorndotbelow", 
"uhorndotbelow", "Ygrave", "ygrave", "Ydotbelow", "ydotbelow", 
"Yhookabove", "yhookabove", "Ytilde", "ytilde", "enspace", "zerowidthspace", 
"afii61664", "zerowidthnonjoiner", "afii301", "afii299", "afii300", 
"hyphentwo", "figuredash", "endash", "emdash", "afii00208", "horizontalbar", 
"dblverticalbar", "dbllowline", "underscoredbl", "quoteleft", 
"quoteright", "quotesinglbase", "quoteleftreversed", "quotereversed", 
"quotedblleft", "quotedblright", "quotedblbase", "dagger", "daggerdbl", 
"bullet", "onedotenleader", "twodotenleader", "twodotleader", 
"ellipsis", "afii61573", "afii61574", "afii61575", "perthousand", 
"minute", "second", "primereversed", "guilsinglleft", "guilsinglright", 
"referencemark", "exclamdbl", "overline", "asterism", "fraction", 
"zerosuperior", "foursuperior", "fivesuperior", "sixsuperior", 
"sevensuperior", "eightsuperior", "ninesuperior", "plussuperior", 
"equalsuperior", "parenleftsuperior", "parenrightsuperior", "nsuperior", 
"zeroinferior", "oneinferior", "twoinferior", "threeinferior", 
"fourinferior", "fiveinferior", "sixinferior", "seveninferior", 
"eightinferior", "nineinferior", "parenleftinferior", "parenrightinferior", 
"colonmonetary", "colonsign", "cruzeiro", "franc", "afii08941", 
"lira", "peseta", "won", "afii57636", "newsheqelsign", "sheqel", 
"sheqelhebrew", "dong", "Euro", "euro", "centigrade", "afii61248", 
"careof", "fahrenheit", "Ifraktur", "afii61289", "lsquare", "afii61352", 
"numero", "weierstrass", "Rfraktur", "prescription", "telephone", 
"trademark", "Ohm", "Omega", "angstrom", "estimated", "aleph", 
"onethird", "twothirds", "oneeighth", "threeeighths", "fiveeighths", 
"seveneighths", "Oneroman", "Tworoman", "Threeroman", "Fourroman", 
"Fiveroman", "Sixroman", "Sevenroman", "Eightroman", "Nineroman", 
"Tenroman", "Elevenroman", "Twelveroman", "oneroman", "tworoman", 
"threeroman", "fourroman", "fiveroman", "sixroman", "sevenroman", 
"eightroman", "nineroman", "tenroman", "elevenroman", "twelveroman", 
"arrowleft", "arrowup", "arrowright", "arrowdown", "arrowboth", 
"arrowupdn", "arrowupleft", "arrowupright", "arrowdownright", 
"arrowdownleft", "arrowupdnbse", "arrowupdownbase", "carriagereturn", 
"harpoonleftbarbup", "harpoonrightbarbup", "arrowrightoverleft", 
"arrowupleftofdown", "arrowleftoverright", "arrowleftdblstroke", 
"arrowrightdblstroke", "arrowdblleft", "arrowleftdbl", "arrowdblup", 
"arrowdblright", "dblarrowright", "arrowdbldown", "arrowdblboth", 
"dblarrowleft", "pageup", "pagedown", "arrowdashleft", "arrowdashup", 
"arrowdashright", "arrowdashdown", "arrowtableft", "arrowtabright", 
"arrowleftwhite", "arrowupwhite", "arrowrightwhite", "arrowdownwhite", 
"capslock", "forall", "universal", "partialdiff", "existential", 
"thereexists", "emptyset", "Delta", "increment", "gradient", 
"nabla", "element", "notelement", "notelementof", "suchthat", 
"notcontains", "product", "summation", "minus", "minusplus", 
"divisionslash", "asteriskmath", "bulletoperator", "radical", 
"proportional", "infinity", "orthogonal", "rightangle", "angle", 
"divides", "parallel", "notparallel", "logicaland", "logicalor", 
"intersection", "union", "integral", "dblintegral", "contourintegral", 
"therefore", "because", "ratio", "proportion", "similar", "tildeoperator", 
"reversedtilde", "asymptoticallyequal", "approximatelyequal", 
"congruent", "approxequal", "allequal", "approaches", "geometricallyequal", 
"approxequalorimage", "imageorapproximatelyequal", "notequal", 
"equivalence", "notidentical", "lessequal", "greaterequal", "lessoverequal", 
"greateroverequal", "muchless", "muchgreater", "notless", "notgreater", 
"notlessnorequal", "notgreaternorequal", "lessorequivalent", 
"greaterorequivalent", "lessorgreater", "greaterorless", "notgreaternorless", 
"precedes", "succeeds", "notprecedes", "notsucceeds", "propersubset", 
"subset", "propersuperset", "superset", "notsubset", "notsuperset", 
"reflexsubset", "subsetorequal", "reflexsuperset", "supersetorequal", 
"subsetnotequal", "supersetnotequal", "circleplus", "pluscircle", 
"minuscircle", "circlemultiply", "timescircle", "circleot", "tackleft", 
"tackdown", "perpendicular", "righttriangle", "dotmath", "curlyor", 
"curlyand", "lessequalorgreater", "greaterequalorless", "ellipsisvertical", 
"house", "control", "projective", "logicalnotreversed", "revlogicalnot", 
"arc", "propellor", "integraltop", "integraltp", "integralbottom", 
"integralbt", "option", "deleteright", "clear", "angleleft", 
"angleright", "deleteleft", "blank", "onecircle", "twocircle", 
"threecircle", "fourcircle", "fivecircle", "sixcircle", "sevencircle", 
"eightcircle", "ninecircle", "tencircle", "elevencircle", "twelvecircle", 
"thirteencircle", "fourteencircle", "fifteencircle", "sixteencircle", 
"seventeencircle", "eighteencircle", "nineteencircle", "twentycircle", 
"oneparen", "twoparen", "threeparen", "fourparen", "fiveparen", 
"sixparen", "sevenparen", "eightparen", "nineparen", "tenparen", 
"elevenparen", "twelveparen", "thirteenparen", "fourteenparen", 
"fifteenparen", "sixteenparen", "seventeenparen", "eighteenparen", 
"nineteenparen", "twentyparen", "oneperiod", "twoperiod", "threeperiod", 
"fourperiod", "fiveperiod", "sixperiod", "sevenperiod", "eightperiod", 
"nineperiod", "tenperiod", "elevenperiod", "twelveperiod", "thirteenperiod", 
"fourteenperiod", "fifteenperiod", "sixteenperiod", "seventeenperiod", 
"eighteenperiod", "nineteenperiod", "twentyperiod", "aparen", 
"bparen", "cparen", "dparen", "eparen", "fparen", "gparen", "hparen", 
"iparen", "jparen", "kparen", "lparen", "mparen", "nparen", "oparen", 
"pparen", "qparen", "rparen", "sparen", "tparen", "uparen", "vparen", 
"wparen", "xparen", "yparen", "zparen", "Acircle", "Bcircle", 
"Ccircle", "Dcircle", "Ecircle", "Fcircle", "Gcircle", "Hcircle", 
"Icircle", "Jcircle", "Kcircle", "Lcircle", "Mcircle", "Ncircle", 
"Ocircle", "Pcircle", "Qcircle", "Rcircle", "Scircle", "Tcircle", 
"Ucircle", "Vcircle", "Wcircle", "Xcircle", "Ycircle", "Zcircle", 
"acircle", "bcircle", "ccircle", "dcircle", "ecircle", "fcircle", 
"gcircle", "hcircle", "icircle", "jcircle", "kcircle", "lcircle", 
"mcircle", "ncircle", "ocircle", "pcircle", "qcircle", "rcircle", 
"scircle", "tcircle", "ucircle", "vcircle", "wcircle", "xcircle", 
"ycircle", "zcircle", "SF100000", "SF110000", "SF010000", "SF030000", 
"SF020000", "SF040000", "SF080000", "SF090000", "SF060000", "SF070000", 
"SF050000", "SF430000", "SF240000", "SF510000", "SF520000", "SF390000", 
"SF220000", "SF210000", "SF250000", "SF500000", "SF490000", "SF380000", 
"SF280000", "SF270000", "SF260000", "SF360000", "SF370000", "SF420000", 
"SF190000", "SF200000", "SF230000", "SF470000", "SF480000", "SF410000", 
"SF450000", "SF460000", "SF400000", "SF540000", "SF530000", "SF440000", 
"upblock", "dnblock", "block", "lfblock", "rtblock", "ltshade", 
"shadelight", "shade", "shademedium", "dkshade", "shadedark", 
"blacksquare", "filledbox", "H22073", "whitesquare", "squarewhitewithsmallblack", 
"squarehorizontalfill", "squareverticalfill", "squareorthogonalcrosshatchfill", 
"squareupperlefttolowerrightfill", "squareupperrighttolowerleftfill", 
"squarediagonalcrosshatchfill", "H18543", "blacksmallsquare", 
"H18551", "whitesmallsquare", "blackrectangle", "filledrect", 
"blackuppointingtriangle", "triagup", "whiteuppointingtriangle", 
"blackuppointingsmalltriangle", "whiteuppointingsmalltriangle", 
"blackrightpointingtriangle", "whiterightpointingtriangle", "whiterightpointingsmalltriangle", 
"blackrightpointingpointer", "triagrt", "blackdownpointingtriangle", 
"triagdn", "whitedownpointingtriangle", "whitedownpointingsmalltriangle", 
"blackleftpointingtriangle", "whiteleftpointingtriangle", "whiteleftpointingsmalltriangle", 
"blackleftpointingpointer", "triaglf", "blackdiamond", "whitediamond", 
"whitediamondcontainingblacksmalldiamond", "fisheye", "lozenge", 
"circle", "whitecircle", "dottedcircle", "bullseye", "H18533", 
"blackcircle", "circlewithlefthalfblack", "circlewithrighthalfblack", 
"bulletinverse", "invbullet", "invcircle", "whitecircleinverse", 
"blacklowerrighttriangle", "blacklowerlefttriangle", "blackupperlefttriangle", 
"blackupperrighttriangle", "openbullet", "whitebullet", "largecircle", 
"blackstar", "whitestar", "telephoneblack", "whitetelephone", 
"pointingindexleftwhite", "pointingindexupwhite", "pointingindexrightwhite", 
"pointingindexdownwhite", "yinyang", "smileface", "whitesmilingface", 
"blacksmilingface", "invsmileface", "compass", "sun", "female", 
"venus", "earth", "male", "mars", "spade", "spadesuitblack", 
"heartsuitwhite", "diamondsuitwhite", "club", "clubsuitblack", 
"spadesuitwhite", "heart", "heartsuitblack", "diamond", "clubsuitwhite", 
"hotsprings", "quarternote", "musicalnote", "eighthnotebeamed", 
"musicalnotedbl", "beamedsixteenthnotes", "musicflatsign", "musicsharpsign", 
"checkmark", "onecircleinversesansserif", "twocircleinversesansserif", 
"threecircleinversesansserif", "fourcircleinversesansserif", 
"fivecircleinversesansserif", "sixcircleinversesansserif", "sevencircleinversesansserif", 
"eightcircleinversesansserif", "ninecircleinversesansserif", 
"arrowrightheavy", "ideographicspace", "ideographiccomma", "ideographicperiod", 
"dittomark", "jis", "ideographiciterationmark", "ideographicclose", 
"ideographiczero", "anglebracketleft", "anglebracketright", "dblanglebracketleft", 
"dblanglebracketright", "cornerbracketleft", "cornerbracketright", 
"whitecornerbracketleft", "whitecornerbracketright", "blacklenticularbracketleft", 
"blacklenticularbracketright", "postalmark", "getamark", "tortoiseshellbracketleft", 
"tortoiseshellbracketright", "whitelenticularbracketleft", "whitelenticularbracketright", 
"whitetortoiseshellbracketleft", "whitetortoiseshellbracketright", 
"wavedash", "quotedblprimereversed", "quotedblprime", "postalmarkface", 
"onehangzhou", "twohangzhou", "threehangzhou", "fourhangzhou", 
"fivehangzhou", "sixhangzhou", "sevenhangzhou", "eighthangzhou", 
"ninehangzhou", "circlepostalmark", "asmallhiragana", "ahiragana", 
"ismallhiragana", "ihiragana", "usmallhiragana", "uhiragana", 
"esmallhiragana", "ehiragana", "osmallhiragana", "ohiragana", 
"kahiragana", "gahiragana", "kihiragana", "gihiragana", "kuhiragana", 
"guhiragana", "kehiragana", "gehiragana", "kohiragana", "gohiragana", 
"sahiragana", "zahiragana", "sihiragana", "zihiragana", "suhiragana", 
"zuhiragana", "sehiragana", "zehiragana", "sohiragana", "zohiragana", 
"tahiragana", "dahiragana", "tihiragana", "dihiragana", "tusmallhiragana", 
"tuhiragana", "duhiragana", "tehiragana", "dehiragana", "tohiragana", 
"dohiragana", "nahiragana", "nihiragana", "nuhiragana", "nehiragana", 
"nohiragana", "hahiragana", "bahiragana", "pahiragana", "hihiragana", 
"bihiragana", "pihiragana", "huhiragana", "buhiragana", "puhiragana", 
"hehiragana", "behiragana", "pehiragana", "hohiragana", "bohiragana", 
"pohiragana", "mahiragana", "mihiragana", "muhiragana", "mehiragana", 
"mohiragana", "yasmallhiragana", "yahiragana", "yusmallhiragana", 
"yuhiragana", "yosmallhiragana", "yohiragana", "rahiragana", 
"rihiragana", "ruhiragana", "rehiragana", "rohiragana", "wasmallhiragana", 
"wahiragana", "wihiragana", "wehiragana", "wohiragana", "nhiragana", 
"vuhiragana", "voicedmarkkana", "semivoicedmarkkana", "iterationhiragana", 
"voicediterationhiragana", "asmallkatakana", "akatakana", "ismallkatakana", 
"ikatakana", "usmallkatakana", "ukatakana", "esmallkatakana", 
"ekatakana", "osmallkatakana", "okatakana", "kakatakana", "gakatakana", 
"kikatakana", "gikatakana", "kukatakana", "gukatakana", "kekatakana", 
"gekatakana", "kokatakana", "gokatakana", "sakatakana", "zakatakana", 
"sikatakana", "zikatakana", "sukatakana", "zukatakana", "sekatakana", 
"zekatakana", "sokatakana", "zokatakana", "takatakana", "dakatakana", 
"tikatakana", "dikatakana", "tusmallkatakana", "tukatakana", 
"dukatakana", "tekatakana", "dekatakana", "tokatakana", "dokatakana", 
"nakatakana", "nikatakana", "nukatakana", "nekatakana", "nokatakana", 
"hakatakana", "bakatakana", "pakatakana", "hikatakana", "bikatakana", 
"pikatakana", "hukatakana", "bukatakana", "pukatakana", "hekatakana", 
"bekatakana", "pekatakana", "hokatakana", "bokatakana", "pokatakana", 
"makatakana", "mikatakana", "mukatakana", "mekatakana", "mokatakana", 
"yasmallkatakana", "yakatakana", "yusmallkatakana", "yukatakana", 
"yosmallkatakana", "yokatakana", "rakatakana", "rikatakana", 
"rukatakana", "rekatakana", "rokatakana", "wasmallkatakana", 
"wakatakana", "wikatakana", "wekatakana", "wokatakana", "nkatakana", 
"vukatakana", "kasmallkatakana", "kesmallkatakana", "vakatakana", 
"vikatakana", "vekatakana", "vokatakana", "dotkatakana", "prolongedkana", 
"iterationkatakana", "voicediterationkatakana", "bbopomofo", 
"pbopomofo", "mbopomofo", "fbopomofo", "dbopomofo", "tbopomofo", 
"nbopomofo", "lbopomofo", "gbopomofo", "kbopomofo", "hbopomofo", 
"jbopomofo", "qbopomofo", "xbopomofo", "zhbopomofo", "chbopomofo", 
"shbopomofo", "rbopomofo", "zbopomofo", "cbopomofo", "sbopomofo", 
"abopomofo", "obopomofo", "ebopomofo", "ehbopomofo", "aibopomofo", 
"eibopomofo", "aubopomofo", "oubopomofo", "anbopomofo", "enbopomofo", 
"angbopomofo", "engbopomofo", "erbopomofo", "ibopomofo", "ubopomofo", 
"iubopomofo", "kiyeokkorean", "ssangkiyeokkorean", "kiyeoksioskorean", 
"nieunkorean", "nieuncieuckorean", "nieunhieuhkorean", "tikeutkorean", 
"ssangtikeutkorean", "rieulkorean", "rieulkiyeokkorean", "rieulmieumkorean", 
"rieulpieupkorean", "rieulsioskorean", "rieulthieuthkorean", 
"rieulphieuphkorean", "rieulhieuhkorean", "mieumkorean", "pieupkorean", 
"ssangpieupkorean", "pieupsioskorean", "sioskorean", "ssangsioskorean", 
"ieungkorean", "cieuckorean", "ssangcieuckorean", "chieuchkorean", 
"khieukhkorean", "thieuthkorean", "phieuphkorean", "hieuhkorean", 
"akorean", "aekorean", "yakorean", "yaekorean", "eokorean", "ekorean", 
"yeokorean", "yekorean", "okorean", "wakorean", "waekorean", 
"oekorean", "yokorean", "ukorean", "weokorean", "wekorean", "wikorean", 
"yukorean", "eukorean", "yikorean", "ikorean", "hangulfiller", 
"ssangnieunkorean", "nieuntikeutkorean", "nieunsioskorean", "nieunpansioskorean", 
"rieulkiyeoksioskorean", "rieultikeutkorean", "rieulpieupsioskorean", 
"rieulpansioskorean", "rieulyeorinhieuhkorean", "mieumpieupkorean", 
"mieumsioskorean", "mieumpansioskorean", "kapyeounmieumkorean", 
"pieupkiyeokkorean", "pieuptikeutkorean", "pieupsioskiyeokkorean", 
"pieupsiostikeutkorean", "pieupcieuckorean", "pieupthieuthkorean", 
"kapyeounpieupkorean", "kapyeounssangpieupkorean", "sioskiyeokkorean", 
"siosnieunkorean", "siostikeutkorean", "siospieupkorean", "sioscieuckorean", 
"pansioskorean", "ssangieungkorean", "yesieungkorean", "yesieungsioskorean", 
"yesieungpansioskorean", "kapyeounphieuphkorean", "ssanghieuhkorean", 
"yeorinhieuhkorean", "yoyakorean", "yoyaekorean", "yoikorean", 
"yuyeokorean", "yuyekorean", "yuikorean", "araeakorean", "araeaekorean", 
"kiyeokparenkorean", "nieunparenkorean", "tikeutparenkorean", 
"rieulparenkorean", "mieumparenkorean", "pieupparenkorean", "siosparenkorean", 
"ieungparenkorean", "cieucparenkorean", "chieuchparenkorean", 
"khieukhparenkorean", "thieuthparenkorean", "phieuphparenkorean", 
"hieuhparenkorean", "kiyeokaparenkorean", "nieunaparenkorean", 
"tikeutaparenkorean", "rieulaparenkorean", "mieumaparenkorean", 
"pieupaparenkorean", "siosaparenkorean", "ieungaparenkorean", 
"cieucaparenkorean", "chieuchaparenkorean", "khieukhaparenkorean", 
"thieuthaparenkorean", "phieuphaparenkorean", "hieuhaparenkorean", 
"cieucuparenkorean", "oneideographicparen", "twoideographicparen", 
"threeideographicparen", "fourideographicparen", "fiveideographicparen", 
"sixideographicparen", "sevenideographicparen", "eightideographicparen", 
"nineideographicparen", "tenideographicparen", "ideographicmoonparen", 
"ideographicfireparen", "ideographicwaterparen", "ideographicwoodparen", 
"ideographicmetalparen", "ideographicearthparen", "ideographicsunparen", 
"ideographicstockparen", "ideographichaveparen", "ideographicsocietyparen", 
"ideographicnameparen", "ideographicspecialparen", "ideographicfinancialparen", 
"ideographiccongratulationparen", "ideographiclaborparen", "ideographicrepresentparen", 
"ideographiccallparen", "ideographicstudyparen", "ideographicsuperviseparen", 
"ideographicenterpriseparen", "ideographicresourceparen", "ideographicallianceparen", 
"ideographicfestivalparen", "ideographicselfparen", "ideographicreachparen", 
"kiyeokcirclekorean", "nieuncirclekorean", "tikeutcirclekorean", 
"rieulcirclekorean", "mieumcirclekorean", "pieupcirclekorean", 
"sioscirclekorean", "ieungcirclekorean", "cieuccirclekorean", 
"chieuchcirclekorean", "khieukhcirclekorean", "thieuthcirclekorean", 
"phieuphcirclekorean", "hieuhcirclekorean", "kiyeokacirclekorean", 
"nieunacirclekorean", "tikeutacirclekorean", "rieulacirclekorean", 
"mieumacirclekorean", "pieupacirclekorean", "siosacirclekorean", 
"ieungacirclekorean", "cieucacirclekorean", "chieuchacirclekorean", 
"khieukhacirclekorean", "thieuthacirclekorean", "phieuphacirclekorean", 
"hieuhacirclekorean", "koreanstandardsymbol", "ideographmooncircle", 
"ideographfirecircle", "ideographwatercircle", "ideographwoodcircle", 
"ideographmetalcircle", "ideographearthcircle", "ideographsuncircle", 
"ideographnamecircle", "ideographicfinancialcircle", "ideographiclaborcircle", 
"ideographicsecretcircle", "ideographicexcellentcircle", "ideographicprintcircle", 
"ideographiccorrectcircle", "ideographichighcircle", "ideographiccentrecircle", 
"ideographiclowcircle", "ideographicleftcircle", "ideographicrightcircle", 
"ideographicmedicinecircle", "apaatosquare", "aarusquare", "intisquare", 
"karoriisquare", "kirosquare", "kiroguramusquare", "kiromeetorusquare", 
"guramusquare", "kooposquare", "sentisquare", "sentosquare", 
"dorusquare", "tonsquare", "haitusquare", "paasentosquare", "birusquare", 
"huiitosquare", "hekutaarusquare", "herutusquare", "peezisquare", 
"hoonsquare", "mansyonsquare", "mirisquare", "miribaarusquare", 
"meetorusquare", "yaadosquare", "rittorusquare", "wattosquare", 
"heiseierasquare", "syouwaerasquare", "taisyouerasquare", "meizierasquare", 
"corporationsquare", "paampssquare", "nasquare", "muasquare", 
"masquare", "kasquare", "KBsquare", "MBsquare", "GBsquare", "calsquare", 
"kcalsquare", "pfsquare", "nfsquare", "mufsquare", "mugsquare", 
"squaremg", "squarekg", "Hzsquare", "khzsquare", "mhzsquare", 
"ghzsquare", "thzsquare", "mulsquare", "mlsquare", "dlsquare", 
"klsquare", "fmsquare", "nmsquare", "mumsquare", "squaremm", 
"squarecm", "squarekm", "mmsquaredsquare", "cmsquaredsquare", 
"squaremsquared", "kmsquaredsquare", "mmcubedsquare", "cmcubedsquare", 
"mcubedsquare", "kmcubedsquare", "moverssquare", "moverssquaredsquare", 
"pasquare", "kpasquare", "mpasquare", "gpasquare", "radsquare", 
"radoverssquare", "radoverssquaredsquare", "pssquare", "nssquare", 
"mussquare", "mssquare", "pvsquare", "nvsquare", "muvsquare", 
"mvsquare", "kvsquare", "mvmegasquare", "pwsquare", "nwsquare", 
"muwsquare", "mwsquare", "kwsquare", "mwmegasquare", "kohmsquare", 
"mohmsquare", "amsquare", "bqsquare", "squarecc", "cdsquare", 
"coverkgsquare", "cosquare", "dbsquare", "gysquare", "hasquare", 
"HPsquare", "KKsquare", "squarekmcapital", "ktsquare", "lmsquare", 
"squareln", "squarelog", "lxsquare", "mbsquare", "squaremil", 
"molsquare", "pmsquare", "srsquare", "svsquare", "wbsquare", 
"twentyhangzhou", "dotlessj", "LL", "ll", "commaaccent", "afii10063", 
"afii10064", "afii10192", "afii10831", "afii10832", "Acute", 
"Caron", "Dieresis", "DieresisAcute", "DieresisGrave", "Grave", 
"Hungarumlaut", "Macron", "cyrBreve", "cyrFlex", "dblGrave", 
"cyrbreve", "cyrflex", "dblgrave", "dieresisacute", "dieresisgrave", 
"copyrightserif", "registerserif", "trademarkserif", "onefitted", 
"rupiah", "threequartersemdash", "centinferior", "centsuperior", 
"commainferior", "commasuperior", "dollarinferior", "dollarsuperior", 
"hypheninferior", "hyphensuperior", "periodinferior", "periodsuperior", 
"asuperior", "bsuperior", "dsuperior", "esuperior", "isuperior", 
"lsuperior", "msuperior", "osuperior", "rsuperior", "ssuperior", 
"tsuperior", "Brevesmall", "Caronsmall", "Circumflexsmall", "Dotaccentsmall", 
"Hungarumlautsmall", "Lslashsmall", "OEsmall", "Ogoneksmall", 
"Ringsmall", "Scaronsmall", "Tildesmall", "Zcaronsmall", "exclamsmall", 
"dollaroldstyle", "ampersandsmall", "zerooldstyle", "oneoldstyle", 
"twooldstyle", "threeoldstyle", "fouroldstyle", "fiveoldstyle", 
"sixoldstyle", "sevenoldstyle", "eightoldstyle", "nineoldstyle", 
"questionsmall", "Gravesmall", "Asmall", "Bsmall", "Csmall", 
"Dsmall", "Esmall", "Fsmall", "Gsmall", "Hsmall", "Ismall", "Jsmall", 
"Ksmall", "Lsmall", "Msmall", "Nsmall", "Osmall", "Psmall", "Qsmall", 
"Rsmall", "Ssmall", "Tsmall", "Usmall", "Vsmall", "Wsmall", "Xsmall", 
"Ysmall", "Zsmall", "exclamdownsmall", "centoldstyle", "Dieresissmall", 
"Macronsmall", "Acutesmall", "Cedillasmall", "questiondownsmall", 
"Agravesmall", "Aacutesmall", "Acircumflexsmall", "Atildesmall", 
"Adieresissmall", "Aringsmall", "AEsmall", "Ccedillasmall", "Egravesmall", 
"Eacutesmall", "Ecircumflexsmall", "Edieresissmall", "Igravesmall", 
"Iacutesmall", "Icircumflexsmall", "Idieresissmall", "Ethsmall", 
"Ntildesmall", "Ogravesmall", "Oacutesmall", "Ocircumflexsmall", 
"Otildesmall", "Odieresissmall", "Oslashsmall", "Ugravesmall", 
"Uacutesmall", "Ucircumflexsmall", "Udieresissmall", "Yacutesmall", 
"Thornsmall", "Ydieresissmall", "maihanakatleftthai", "saraileftthai", 
"saraiileftthai", "saraueleftthai", "saraueeleftthai", "maitaikhuleftthai", 
"maiekupperleftthai", "maieklowrightthai", "maieklowleftthai", 
"maithoupperleftthai", "maitholowrightthai", "maitholowleftthai", 
"maitriupperleftthai", "maitrilowrightthai", "maitrilowleftthai", 
"maichattawaupperleftthai", "maichattawalowrightthai", "maichattawalowleftthai", 
"thanthakhatupperleftthai", "thanthakhatlowrightthai", "thanthakhatlowleftthai", 
"nikhahitleftthai", "radicalex", "arrowvertex", "arrowhorizex", 
"registersans", "copyrightsans", "trademarksans", "parenlefttp", 
"parenleftex", "parenleftbt", "bracketlefttp", "bracketleftex", 
"bracketleftbt", "bracelefttp", "braceleftmid", "braceleftbt", 
"braceex", "integralex", "parenrighttp", "parenrightex", "parenrightbt", 
"bracketrighttp", "bracketrightex", "bracketrightbt", "bracerighttp", 
"bracerightmid", "bracerightbt", "apple", "ff", "fi", "fl", "ffi", 
"ffl", "afii57705", "doubleyodpatah", "doubleyodpatahhebrew", 
"yodyodpatahhebrew", "ayinaltonehebrew", "afii57694", "shinshindot", 
"shinshindothebrew", "afii57695", "shinsindot", "shinsindothebrew", 
"shindageshshindot", "shindageshshindothebrew", "shindageshsindot", 
"shindageshsindothebrew", "alefpatahhebrew", "alefqamatshebrew", 
"alefdageshhebrew", "betdagesh", "betdageshhebrew", "gimeldagesh", 
"gimeldageshhebrew", "daletdagesh", "daletdageshhebrew", "hedagesh", 
"hedageshhebrew", "afii57723", "vavdagesh", "vavdagesh65", "vavdageshhebrew", 
"zayindagesh", "zayindageshhebrew", "tetdagesh", "tetdageshhebrew", 
"yoddagesh", "yoddageshhebrew", "finalkafdagesh", "finalkafdageshhebrew", 
"kafdagesh", "kafdageshhebrew", "lameddagesh", "lameddageshhebrew", 
"memdagesh", "memdageshhebrew", "nundagesh", "nundageshhebrew", 
"samekhdagesh", "samekhdageshhebrew", "pefinaldageshhebrew", 
"pedagesh", "pedageshhebrew", "tsadidagesh", "tsadidageshhebrew", 
"qofdagesh", "qofdageshhebrew", "reshdageshhebrew", "shindagesh", 
"shindageshhebrew", "tavdages", "tavdagesh", "tavdageshhebrew", 
"afii57700", "vavholam", "vavholamhebrew", "betrafehebrew", "kafrafehebrew", 
"perafehebrew", "aleflamedhebrew", "pehfinalarabic", "pehinitialarabic", 
"pehmedialarabic", "ttehfinalarabic", "ttehinitialarabic", "ttehmedialarabic", 
"vehfinalarabic", "vehinitialarabic", "vehmedialarabic", "tchehfinalarabic", 
"tchehinitialarabic", "tchehmedialarabic", "ddalfinalarabic", 
"jehfinalarabic", "rrehfinalarabic", "gaffinalarabic", "gafinitialarabic", 
"gafmedialarabic", "noonghunnafinalarabic", "hehhamzaaboveisolatedarabic", 
"hehhamzaabovefinalarabic", "hehfinalaltonearabic", "hehinitialaltonearabic", 
"hehmedialaltonearabic", "yehbarreefinalarabic", "behmeemisolatedarabic", 
"tehjeemisolatedarabic", "tehhahisolatedarabic", "tehmeemisolatedarabic", 
"meemmeemisolatedarabic", "noonjeemisolatedarabic", "noonmeemisolatedarabic", 
"yehmeemisolatedarabic", "shaddadammatanarabic", "shaddakasratanarabic", 
"shaddafathaarabic", "shaddadammaarabic", "shaddakasraarabic", 
"behnoonfinalarabic", "tehnoonfinalarabic", "noonnoonfinalarabic", 
"yehnoonfinalarabic", "behmeeminitialarabic", "tehjeeminitialarabic", 
"tehhahinitialarabic", "tehmeeminitialarabic", "lamjeeminitialarabic", 
"lamhahinitialarabic", "lamkhahinitialarabic", "lammeeminitialarabic", 
"meemmeeminitialarabic", "noonjeeminitialarabic", "noonmeeminitialarabic", 
"yehmeeminitialarabic", "parenleftaltonearabic", "parenrightaltonearabic", 
"lammeemhahinitialarabic", "lamlamhehisolatedarabic", "sallallahoualayhewasallamarabic", 
"twodotleadervertical", "emdashvertical", "endashvertical", "underscorevertical", 
"wavyunderscorevertical", "parenleftvertical", "parenrightvertical", 
"braceleftvertical", "bracerightvertical", "tortoiseshellbracketleftvertical", 
"tortoiseshellbracketrightvertical", "blacklenticularbracketleftvertical", 
"blacklenticularbracketrightvertical", "dblanglebracketleftvertical", 
"dblanglebracketrightvertical", "anglebracketleftvertical", "anglebracketrightvertical", 
"cornerbracketleftvertical", "cornerbracketrightvertical", "whitecornerbracketleftvertical", 
"whitecornerbracketrightvertical", "overlinedashed", "overlinecenterline", 
"overlinewavy", "overlinedblwavy", "lowlinedashed", "lowlinecenterline", 
"underscorewavy", "commasmall", "periodsmall", "semicolonsmall", 
"colonsmall", "parenleftsmall", "parenrightsmall", "braceleftsmall", 
"bracerightsmall", "tortoiseshellbracketleftsmall", "tortoiseshellbracketrightsmall", 
"numbersignsmall", "asterisksmall", "plussmall", "hyphensmall", 
"lesssmall", "greatersmall", "equalsmall", "dollarsmall", "percentsmall", 
"atsmall", "alefmaddaabovefinalarabic", "alefhamzaabovefinalarabic", 
"wawhamzaabovefinalarabic", "alefhamzabelowfinalarabic", "yehhamzaabovefinalarabic", 
"yehhamzaaboveinitialarabic", "yehhamzaabovemedialarabic", "aleffinalarabic", 
"behfinalarabic", "behinitialarabic", "behmedialarabic", "tehmarbutafinalarabic", 
"tehfinalarabic", "tehinitialarabic", "tehmedialarabic", "thehfinalarabic", 
"thehinitialarabic", "thehmedialarabic", "jeemfinalarabic", "jeeminitialarabic", 
"jeemmedialarabic", "hahfinalarabic", "hahinitialarabic", "hahmedialarabic", 
"khahfinalarabic", "khahinitialarabic", "khahmedialarabic", "dalfinalarabic", 
"thalfinalarabic", "rehfinalarabic", "zainfinalarabic", "seenfinalarabic", 
"seeninitialarabic", "seenmedialarabic", "sheenfinalarabic", 
"sheeninitialarabic", "sheenmedialarabic", "sadfinalarabic", 
"sadinitialarabic", "sadmedialarabic", "dadfinalarabic", "dadinitialarabic", 
"dadmedialarabic", "tahfinalarabic", "tahinitialarabic", "tahmedialarabic", 
"zahfinalarabic", "zahinitialarabic", "zahmedialarabic", "ainfinalarabic", 
"aininitialarabic", "ainmedialarabic", "ghainfinalarabic", "ghaininitialarabic", 
"ghainmedialarabic", "fehfinalarabic", "fehinitialarabic", "fehmedialarabic", 
"qaffinalarabic", "qafinitialarabic", "qafmedialarabic", "kaffinalarabic", 
"kafinitialarabic", "kafmedialarabic", "lamfinalarabic", "laminitialarabic", 
"lammedialarabic", "meemfinalarabic", "meeminitialarabic", "meemmedialarabic", 
"noonfinalarabic", "nooninitialarabic", "noonmedialarabic", "hehfinalalttwoarabic", 
"hehfinalarabic", "hehinitialarabic", "hehmedialarabic", "wawfinalarabic", 
"alefmaksurafinalarabic", "yehfinalarabic", "alefmaksurainitialarabic", 
"yehinitialarabic", "alefmaksuramedialarabic", "yehmedialarabic", 
"lamalefmaddaaboveisolatedarabic", "lamalefmaddaabovefinalarabic", 
"lamalefhamzaaboveisolatedarabic", "lamalefhamzaabovefinalarabic", 
"lamalefhamzabelowisolatedarabic", "lamalefhamzabelowfinalarabic", 
"lamalefisolatedarabic", "lamaleffinalarabic", "zerowidthjoiner", 
"exclammonospace", "quotedblmonospace", "numbersignmonospace", 
"dollarmonospace", "percentmonospace", "ampersandmonospace", 
"quotesinglemonospace", "parenleftmonospace", "parenrightmonospace", 
"asteriskmonospace", "plusmonospace", "commamonospace", "hyphenmonospace", 
"periodmonospace", "slashmonospace", "zeromonospace", "onemonospace", 
"twomonospace", "threemonospace", "fourmonospace", "fivemonospace", 
"sixmonospace", "sevenmonospace", "eightmonospace", "ninemonospace", 
"colonmonospace", "semicolonmonospace", "lessmonospace", "equalmonospace", 
"greatermonospace", "questionmonospace", "atmonospace", "Amonospace", 
"Bmonospace", "Cmonospace", "Dmonospace", "Emonospace", "Fmonospace", 
"Gmonospace", "Hmonospace", "Imonospace", "Jmonospace", "Kmonospace", 
"Lmonospace", "Mmonospace", "Nmonospace", "Omonospace", "Pmonospace", 
"Qmonospace", "Rmonospace", "Smonospace", "Tmonospace", "Umonospace", 
"Vmonospace", "Wmonospace", "Xmonospace", "Ymonospace", "Zmonospace", 
"bracketleftmonospace", "backslashmonospace", "bracketrightmonospace", 
"asciicircummonospace", "underscoremonospace", "gravemonospace", 
"amonospace", "bmonospace", "cmonospace", "dmonospace", "emonospace", 
"fmonospace", "gmonospace", "hmonospace", "imonospace", "jmonospace", 
"kmonospace", "lmonospace", "mmonospace", "nmonospace", "omonospace", 
"pmonospace", "qmonospace", "rmonospace", "smonospace", "tmonospace", 
"umonospace", "vmonospace", "wmonospace", "xmonospace", "ymonospace", 
"zmonospace", "braceleftmonospace", "barmonospace", "bracerightmonospace", 
"asciitildemonospace", "periodhalfwidth", "cornerbracketlefthalfwidth", 
"cornerbracketrighthalfwidth", "ideographiccommaleft", "middledotkatakanahalfwidth", 
"wokatakanahalfwidth", "asmallkatakanahalfwidth", "ismallkatakanahalfwidth", 
"usmallkatakanahalfwidth", "esmallkatakanahalfwidth", "osmallkatakanahalfwidth", 
"yasmallkatakanahalfwidth", "yusmallkatakanahalfwidth", "yosmallkatakanahalfwidth", 
"tusmallkatakanahalfwidth", "katahiraprolongmarkhalfwidth", "akatakanahalfwidth", 
"ikatakanahalfwidth", "ukatakanahalfwidth", "ekatakanahalfwidth", 
"okatakanahalfwidth", "kakatakanahalfwidth", "kikatakanahalfwidth", 
"kukatakanahalfwidth", "kekatakanahalfwidth", "kokatakanahalfwidth", 
"sakatakanahalfwidth", "sikatakanahalfwidth", "sukatakanahalfwidth", 
"sekatakanahalfwidth", "sokatakanahalfwidth", "takatakanahalfwidth", 
"tikatakanahalfwidth", "tukatakanahalfwidth", "tekatakanahalfwidth", 
"tokatakanahalfwidth", "nakatakanahalfwidth", "nikatakanahalfwidth", 
"nukatakanahalfwidth", "nekatakanahalfwidth", "nokatakanahalfwidth", 
"hakatakanahalfwidth", "hikatakanahalfwidth", "hukatakanahalfwidth", 
"hekatakanahalfwidth", "hokatakanahalfwidth", "makatakanahalfwidth", 
"mikatakanahalfwidth", "mukatakanahalfwidth", "mekatakanahalfwidth", 
"mokatakanahalfwidth", "yakatakanahalfwidth", "yukatakanahalfwidth", 
"yokatakanahalfwidth", "rakatakanahalfwidth", "rikatakanahalfwidth", 
"rukatakanahalfwidth", "rekatakanahalfwidth", "rokatakanahalfwidth", 
"wakatakanahalfwidth", "nkatakanahalfwidth", "voicedmarkkanahalfwidth", 
"semivoicedmarkkanahalfwidth", "centmonospace", "sterlingmonospace", 
"macronmonospace", "yenmonospace", "wonmonospace"), unicode = c("0001", 
"0002", "0003", "0004", "0005", "0006", "0007", "0008", "0009", 
"000A", "000B", "000C", "000D", "000E", "000F", "0010", "0011", 
"0012", "0013", "0014", "0015", "0016", "0017", "0018", "0019", 
"001A", "001B", "001C", "001D", "001E", "001F", "0020", "0020", 
"0021", "0022", "0023", "0024", "0025", "0026", "0027", "0028", 
"0029", "002A", "002B", "002C", "002D", "002E", "002F", "0030", 
"0031", "0032", "0033", "0034", "0035", "0036", "0037", "0038", 
"0039", "003A", "003B", "003C", "003D", "003E", "003F", "0040", 
"0041", "0042", "0043", "0044", "0045", "0046", "0047", "0048", 
"0049", "004A", "004B", "004C", "004D", "004E", "004F", "0050", 
"0051", "0052", "0053", "0054", "0055", "0056", "0057", "0058", 
"0059", "005A", "005B", "005C", "005D", "005E", "005F", "0060", 
"0061", "0062", "0063", "0064", "0065", "0066", "0067", "0068", 
"0069", "006A", "006B", "006C", "006D", "006E", "006F", "0070", 
"0071", "0072", "0073", "0074", "0075", "0076", "0077", "0078", 
"0079", "007A", "007B", "007C", "007C", "007D", "007E", "007F", 
"00A0", "00A0", "00A1", "00A2", "00A3", "00A4", "00A5", "00A6", 
"00A7", "00A8", "00A9", "00AA", "00AB", "00AC", "00AD", "00AD", 
"00AE", "00AF", "00AF", "00B0", "00B1", "00B2", "00B3", "00B4", 
"00B5", "00B5", "00B6", "00B7", "00B7", "00B8", "00B9", "00BA", 
"00BB", "00BC", "00BD", "00BE", "00BF", "00C0", "00C1", "00C2", 
"00C3", "00C4", "00C5", "00C6", "00C7", "00C8", "00C9", "00CA", 
"00CB", "00CC", "00CD", "00CE", "00CF", "00D0", "00D1", "00D2", 
"00D3", "00D4", "00D5", "00D6", "00D7", "00D8", "00D9", "00DA", 
"00DB", "00DC", "00DD", "00DE", "00DF", "00E0", "00E1", "00E2", 
"00E3", "00E4", "00E5", "00E6", "00E7", "00E8", "00E9", "00EA", 
"00EB", "00EC", "00ED", "00EE", "00EF", "00F0", "00F1", "00F2", 
"00F3", "00F4", "00F5", "00F6", "00F7", "00F8", "00F9", "00FA", 
"00FB", "00FC", "00FD", "00FE", "00FF", "0100", "0101", "0102", 
"0103", "0104", "0105", "0106", "0107", "0108", "0109", "010A", 
"010A", "010B", "010B", "010C", "010D", "010E", "010F", "0110", 
"0110", "0111", "0111", "0112", "0113", "0114", "0115", "0116", 
"0116", "0117", "0117", "0118", "0119", "011A", "011B", "011C", 
"011D", "011E", "011F", "0120", "0120", "0121", "0121", "0122", 
"0122", "0123", "0123", "0124", "0125", "0126", "0127", "0128", 
"0129", "012A", "012B", "012C", "012D", "012E", "012F", "0130", 
"0130", "0131", "0132", "0133", "0134", "0135", "0136", "0136", 
"0137", "0137", "0138", "0139", "013A", "013B", "013B", "013C", 
"013C", "013D", "013E", "013F", "013F", "0140", "0140", "0141", 
"0142", "0143", "0144", "0145", "0145", "0146", "0146", "0147", 
"0148", "0149", "0149", "014A", "014B", "014C", "014D", "014E", 
"014F", "0150", "0150", "0151", "0151", "0152", "0153", "0154", 
"0155", "0156", "0156", "0157", "0157", "0158", "0159", "015A", 
"015B", "015C", "015D", "015E", "015F", "0160", "0161", "0162", 
"0162", "0163", "0163", "0164", "0165", "0166", "0167", "0168", 
"0169", "016A", "016B", "016C", "016D", "016E", "016F", "0170", 
"0170", "0171", "0171", "0172", "0173", "0174", "0175", "0176", 
"0177", "0178", "0179", "017A", "017B", "017B", "017C", "017C", 
"017D", "017E", "017F", "017F", "0180", "0181", "0182", "0183", 
"0184", "0185", "0186", "0187", "0188", "0189", "018A", "018B", 
"018C", "018D", "018E", "018F", "0190", "0191", "0192", "0193", 
"0194", "0195", "0196", "0197", "0198", "0199", "019A", "019B", 
"019C", "019D", "019E", "019F", "01A0", "01A1", "01A2", "01A3", 
"01A4", "01A5", "01A6", "01A7", "01A8", "01A9", "01AA", "01AB", 
"01AC", "01AD", "01AE", "01AF", "01B0", "01B1", "01B2", "01B3", 
"01B4", "01B5", "01B6", "01B7", "01B8", "01B9", "01BA", "01BB", 
"01BC", "01BD", "01BE", "01BF", "01C0", "01C1", "01C2", "01C3", 
"01C4", "01C5", "01C6", "01C7", "01C8", "01C9", "01CA", "01CB", 
"01CC", "01CD", "01CE", "01CF", "01D0", "01D1", "01D2", "01D3", 
"01D4", "01D5", "01D6", "01D7", "01D8", "01D9", "01DA", "01DB", 
"01DC", "01DD", "01DE", "01DF", "01E0", "01E1", "01E2", "01E3", 
"01E4", "01E5", "01E6", "01E7", "01E8", "01E9", "01EA", "01EB", 
"01EC", "01ED", "01EE", "01EF", "01F0", "01F1", "01F2", "01F3", 
"01F4", "01F5", "01FA", "01FB", "01FC", "01FD", "01FE", "01FE", 
"01FF", "01FF", "0200", "0201", "0202", "0203", "0204", "0205", 
"0206", "0207", "0208", "0209", "020A", "020B", "020C", "020D", 
"020E", "020F", "0210", "0211", "0212", "0213", "0214", "0215", 
"0216", "0217", "0218", "0219", "0250", "0251", "0252", "0253", 
"0254", "0255", "0256", "0257", "0258", "0259", "025A", "025B", 
"025C", "025D", "025E", "025F", "0260", "0261", "0263", "0264", 
"0265", "0266", "0267", "0268", "0269", "026B", "026C", "026D", 
"026E", "026F", "0270", "0271", "0272", "0273", "0275", "0277", 
"0278", "0279", "027A", "027B", "027C", "027D", "027E", "027F", 
"0281", "0282", "0283", "0284", "0285", "0286", "0287", "0288", 
"0289", "028A", "028B", "028C", "028D", "028E", "0290", "0291", 
"0292", "0293", "0294", "0295", "0296", "0297", "0298", "029A", 
"029B", "029D", "029E", "02A0", "02A1", "02A2", "02A3", "02A4", 
"02A5", "02A6", "02A7", "02A8", "02B0", "02B1", "02B2", "02B4", 
"02B5", "02B6", "02B7", "02B8", "02B9", "02BA", "02BB", "02BC", 
"02BC", "02BD", "02BD", "02BE", "02BF", "02C0", "02C1", "02C2", 
"02C3", "02C4", "02C5", "02C6", "02C7", "02C8", "02C9", "02CA", 
"02CB", "02CC", "02CD", "02CE", "02CF", "02D0", "02D1", "02D2", 
"02D3", "02D4", "02D5", "02D6", "02D7", "02D8", "02D9", "02DA", 
"02DB", "02DC", "02DC", "02DD", "02DE", "02E0", "02E3", "02E4", 
"02E5", "02E6", "02E7", "02E8", "02E9", "0300", "0300", "0301", 
"0301", "0302", "0303", "0303", "0304", "0305", "0306", "0307", 
"0308", "0309", "0309", "030A", "030B", "030C", "030D", "030E", 
"030F", "0310", "0311", "0312", "0313", "0314", "0315", "0316", 
"0317", "0318", "0319", "031A", "031B", "031C", "031D", "031E", 
"031F", "0320", "0321", "0322", "0323", "0323", "0324", "0325", 
"0327", "0328", "0329", "032A", "032B", "032C", "032D", "032E", 
"032F", "0330", "0331", "0332", "0333", "0334", "0335", "0336", 
"0337", "0338", "0339", "033A", "033B", "033C", "033D", "033E", 
"033F", "0340", "0341", "0342", "0343", "0344", "0345", "0360", 
"0361", "0374", "0375", "037A", "037E", "0384", "0385", "0385", 
"0386", "0387", "0388", "0389", "038A", "038C", "038E", "038F", 
"0390", "0391", "0392", "0393", "0394", "0395", "0396", "0397", 
"0398", "0399", "039A", "039B", "039C", "039D", "039E", "039F", 
"03A0", "03A1", "03A3", "03A4", "03A5", "03A6", "03A7", "03A8", 
"03A9", "03AA", "03AB", "03AC", "03AD", "03AE", "03AF", "03B0", 
"03B1", "03B2", "03B3", "03B4", "03B5", "03B6", "03B7", "03B8", 
"03B9", "03BA", "03BB", "03BC", "03BD", "03BE", "03BF", "03C0", 
"03C1", "03C2", "03C2", "03C3", "03C4", "03C5", "03C6", "03C7", 
"03C8", "03C9", "03CA", "03CB", "03CC", "03CD", "03CE", "03D0", 
"03D1", "03D1", "03D2", "03D2", "03D3", "03D4", "03D5", "03D5", 
"03D6", "03D6", "03DA", "03DC", "03DE", "03E0", "03E2", "03E3", 
"03E4", "03E5", "03E6", "03E7", "03E8", "03E9", "03EA", "03EB", 
"03EC", "03ED", "03EE", "03EF", "03F0", "03F1", "03F2", "03F3", 
"0401", "0401", "0402", "0402", "0403", "0403", "0404", "0404", 
"0405", "0405", "0406", "0406", "0407", "0407", "0408", "0408", 
"0409", "0409", "040A", "040A", "040B", "040B", "040C", "040C", 
"040E", "040E", "040F", "040F", "0410", "0410", "0411", "0411", 
"0412", "0412", "0413", "0413", "0414", "0414", "0415", "0415", 
"0416", "0416", "0417", "0417", "0418", "0418", "0419", "0419", 
"041A", "041A", "041B", "041B", "041C", "041C", "041D", "041D", 
"041E", "041E", "041F", "041F", "0420", "0420", "0421", "0421", 
"0422", "0422", "0423", "0423", "0424", "0424", "0425", "0425", 
"0426", "0426", "0427", "0427", "0428", "0428", "0429", "0429", 
"042A", "042A", "042B", "042B", "042C", "042C", "042D", "042D", 
"042E", "042E", "042F", "042F", "0430", "0430", "0431", "0431", 
"0432", "0432", "0433", "0433", "0434", "0434", "0435", "0435", 
"0436", "0436", "0437", "0437", "0438", "0438", "0439", "0439", 
"043A", "043A", "043B", "043B", "043C", "043C", "043D", "043D", 
"043E", "043E", "043F", "043F", "0440", "0440", "0441", "0441", 
"0442", "0442", "0443", "0443", "0444", "0444", "0445", "0445", 
"0446", "0446", "0447", "0447", "0448", "0448", "0449", "0449", 
"044A", "044A", "044B", "044B", "044C", "044C", "044D", "044D", 
"044E", "044E", "044F", "044F", "0451", "0451", "0452", "0452", 
"0453", "0453", "0454", "0454", "0455", "0455", "0456", "0456", 
"0457", "0457", "0458", "0458", "0459", "0459", "045A", "045A", 
"045B", "045B", "045C", "045C", "045E", "045E", "045F", "045F", 
"0460", "0461", "0462", "0462", "0463", "0463", "0464", "0465", 
"0466", "0467", "0468", "0469", "046A", "046B", "046C", "046D", 
"046E", "046F", "0470", "0471", "0472", "0472", "0473", "0473", 
"0474", "0474", "0475", "0475", "0476", "0477", "0478", "0479", 
"047A", "047B", "047C", "047D", "047E", "047F", "0480", "0481", 
"0482", "0483", "0484", "0485", "0486", "0490", "0490", "0491", 
"0491", "0492", "0493", "0494", "0495", "0496", "0497", "0498", 
"0499", "049A", "049B", "049C", "049D", "049E", "049F", "04A0", 
"04A1", "04A2", "04A3", "04A4", "04A5", "04A6", "04A7", "04A8", 
"04A9", "04AA", "04AB", "04AC", "04AD", "04AE", "04AF", "04B0", 
"04B1", "04B2", "04B3", "04B4", "04B5", "04B6", "04B7", "04B8", 
"04B9", "04BA", "04BB", "04BC", "04BD", "04BE", "04BF", "04C0", 
"04C1", "04C2", "04C3", "04C4", "04C7", "04C8", "04CB", "04CC", 
"04D0", "04D1", "04D2", "04D3", "04D4", "04D5", "04D6", "04D7", 
"04D8", "04D9", "04D9", "04DA", "04DB", "04DC", "04DD", "04DE", 
"04DF", "04E0", "04E1", "04E2", "04E3", "04E4", "04E5", "04E6", 
"04E7", "04E8", "04E9", "04EA", "04EB", "04EE", "04EF", "04F0", 
"04F1", "04F2", "04F3", "04F4", "04F5", "04F8", "04F9", "0531", 
"0532", "0533", "0534", "0535", "0536", "0537", "0538", "0539", 
"053A", "053B", "053C", "053D", "053E", "053F", "0540", "0541", 
"0542", "0543", "0544", "0545", "0546", "0547", "0548", "0549", 
"054A", "054B", "054C", "054D", "054E", "054F", "0550", "0551", 
"0552", "0553", "0554", "0555", "0556", "0559", "055A", "055B", 
"055C", "055D", "055E", "055F", "0561", "0562", "0563", "0564", 
"0565", "0566", "0567", "0568", "0569", "056A", "056B", "056C", 
"056D", "056E", "056F", "0570", "0571", "0572", "0573", "0574", 
"0575", "0576", "0577", "0578", "0579", "057A", "057B", "057C", 
"057D", "057E", "057F", "0580", "0581", "0582", "0583", "0584", 
"0585", "0586", "0587", "0589", "0591", "0591", "0591", "0591", 
"0592", "0593", "0594", "0595", "0596", "0596", "0597", "0597", 
"0598", "0599", "059A", "059B", "059B", "059C", "059D", "059E", 
"059F", "05A0", "05A1", "05A3", "05A3", "05A4", "05A4", "05A5", 
"05A5", "05A6", "05A6", "05A7", "05A7", "05A8", "05A9", "05AA", 
"05AA", "05AB", "05AC", "05AD", "05AE", "05AF", "05B0", "05B0", 
"05B0", "05B0", "05B0", "05B0", "05B0", "05B0", "05B0", "05B0", 
"05B1", "05B1", "05B1", "05B1", "05B1", "05B1", "05B1", "05B1", 
"05B1", "05B2", "05B2", "05B2", "05B2", "05B2", "05B2", "05B2", 
"05B2", "05B2", "05B3", "05B3", "05B3", "05B3", "05B3", "05B3", 
"05B3", "05B3", "05B3", "05B4", "05B4", "05B4", "05B4", "05B4", 
"05B4", "05B4", "05B4", "05B4", "05B5", "05B5", "05B5", "05B5", 
"05B5", "05B5", "05B5", "05B5", "05B5", "05B6", "05B6", "05B6", 
"05B6", "05B6", "05B6", "05B6", "05B6", "05B6", "05B7", "05B7", 
"05B7", "05B7", "05B7", "05B7", "05B7", "05B7", "05B7", "05B8", 
"05B8", "05B8", "05B8", "05B8", "05B8", "05B8", "05B8", "05B8", 
"05B8", "05B8", "05B8", "05B8", "05B8", "05B8", "05B8", "05B8", 
"05B9", "05B9", "05B9", "05B9", "05B9", "05B9", "05B9", "05B9", 
"05B9", "05BB", "05BB", "05BB", "05BB", "05BB", "05BB", "05BB", 
"05BB", "05BB", "05BC", "05BC", "05BC", "05BD", "05BD", "05BD", 
"05BE", "05BE", "05BF", "05BF", "05BF", "05C0", "05C0", "05C1", 
"05C1", "05C2", "05C2", "05C3", "05C3", "05C4", "05D0", "05D0", 
"05D0", "05D1", "05D1", "05D1", "05D2", "05D2", "05D2", "05D3", 
"05D3", "05D4", "05D4", "05D4", "05D5", "05D5", "05D5", "05D6", 
"05D6", "05D6", "05D7", "05D7", "05D7", "05D8", "05D8", "05D8", 
"05D9", "05D9", "05D9", "05DA", "05DA", "05DA", "05DB", "05DB", 
"05DB", "05DC", "05DC", "05DC", "05DD", "05DD", "05DD", "05DE", 
"05DE", "05DE", "05DF", "05DF", "05DF", "05E0", "05E0", "05E0", 
"05E1", "05E1", "05E1", "05E2", "05E2", "05E2", "05E3", "05E3", 
"05E3", "05E4", "05E4", "05E4", "05E5", "05E5", "05E5", "05E6", 
"05E6", "05E6", "05E7", "05E7", "05E7", "05E8", "05E8", "05E8", 
"05E9", "05E9", "05E9", "05EA", "05EA", "05EA", "05F0", "05F0", 
"05F1", "05F1", "05F2", "05F2", "05F3", "05F4", "060C", "060C", 
"061B", "061B", "061F", "061F", "0621", "0621", "0621", "0622", 
"0622", "0623", "0623", "0624", "0624", "0625", "0625", "0626", 
"0626", "0627", "0627", "0628", "0628", "0629", "0629", "062A", 
"062A", "062B", "062B", "062C", "062C", "062D", "062D", "062E", 
"062E", "062F", "062F", "0630", "0630", "0631", "0631", "0632", 
"0632", "0633", "0633", "0634", "0634", "0635", "0635", "0636", 
"0636", "0637", "0637", "0638", "0638", "0639", "0639", "063A", 
"063A", "0640", "0640", "0640", "0640", "0641", "0641", "0642", 
"0642", "0643", "0643", "0644", "0644", "0645", "0645", "0646", 
"0646", "0647", "0647", "0648", "0648", "0649", "0649", "064A", 
"064A", "064B", "064B", "064C", "064C", "064C", "064D", "064D", 
"064E", "064E", "064E", "064F", "064F", "064F", "0650", "0650", 
"0651", "0651", "0652", "0652", "0660", "0660", "0660", "0661", 
"0661", "0661", "0662", "0662", "0662", "0663", "0663", "0663", 
"0664", "0664", "0664", "0665", "0665", "0665", "0666", "0666", 
"0666", "0667", "0667", "0667", "0668", "0668", "0668", "0669", 
"0669", "0669", "066A", "066A", "066B", "066B", "066C", "066C", 
"066D", "066D", "066D", "0679", "0679", "067E", "067E", "0686", 
"0686", "0688", "0688", "0691", "0691", "0698", "0698", "06A4", 
"06A4", "06AF", "06AF", "06BA", "06BA", "06C1", "06C1", "06D1", 
"06D2", "06D2", "06D5", "06F0", "06F1", "06F2", "06F3", "06F4", 
"06F5", "06F6", "06F7", "06F8", "06F9", "0901", "0902", "0903", 
"0905", "0906", "0907", "0908", "0909", "090A", "090B", "090C", 
"090D", "090E", "090F", "0910", "0911", "0912", "0913", "0914", 
"0915", "0916", "0917", "0918", "0919", "091A", "091B", "091C", 
"091D", "091E", "091F", "0920", "0921", "0922", "0923", "0924", 
"0925", "0926", "0927", "0928", "0929", "092A", "092B", "092C", 
"092D", "092E", "092F", "0930", "0931", "0932", "0933", "0934", 
"0935", "0936", "0937", "0938", "0939", "093C", "093D", "093E", 
"093F", "0940", "0941", "0942", "0943", "0944", "0945", "0946", 
"0947", "0948", "0949", "094A", "094B", "094C", "094D", "0950", 
"0951", "0952", "0953", "0954", "0958", "0959", "095A", "095B", 
"095C", "095D", "095E", "095F", "0960", "0961", "0962", "0963", 
"0964", "0965", "0966", "0967", "0968", "0969", "096A", "096B", 
"096C", "096D", "096E", "096F", "0970", "0981", "0982", "0983", 
"0985", "0986", "0987", "0988", "0989", "098A", "098B", "098C", 
"098F", "0990", "0993", "0994", "0995", "0996", "0997", "0998", 
"0999", "099A", "099B", "099C", "099D", "099E", "099F", "09A0", 
"09A1", "09A2", "09A3", "09A4", "09A5", "09A6", "09A7", "09A8", 
"09AA", "09AB", "09AC", "09AD", "09AE", "09AF", "09B0", "09B2", 
"09B6", "09B7", "09B8", "09B9", "09BC", "09BE", "09BF", "09C0", 
"09C1", "09C2", "09C3", "09C4", "09C7", "09C8", "09CB", "09CC", 
"09CD", "09D7", "09DC", "09DD", "09DF", "09E0", "09E1", "09E2", 
"09E3", "09E6", "09E7", "09E8", "09E9", "09EA", "09EB", "09EC", 
"09ED", "09EE", "09EF", "09F0", "09F1", "09F2", "09F3", "09F4", 
"09F5", "09F6", "09F7", "09F8", "09F9", "09FA", "0A02", "0A05", 
"0A06", "0A07", "0A08", "0A09", "0A0A", "0A0F", "0A10", "0A13", 
"0A14", "0A15", "0A16", "0A17", "0A18", "0A19", "0A1A", "0A1B", 
"0A1C", "0A1D", "0A1E", "0A1F", "0A20", "0A21", "0A22", "0A23", 
"0A24", "0A25", "0A26", "0A27", "0A28", "0A2A", "0A2B", "0A2C", 
"0A2D", "0A2E", "0A2F", "0A30", "0A32", "0A35", "0A36", "0A38", 
"0A39", "0A3C", "0A3E", "0A3F", "0A40", "0A41", "0A42", "0A47", 
"0A48", "0A4B", "0A4C", "0A4D", "0A59", "0A5A", "0A5B", "0A5C", 
"0A5E", "0A66", "0A67", "0A68", "0A69", "0A6A", "0A6B", "0A6C", 
"0A6D", "0A6E", "0A6F", "0A70", "0A71", "0A72", "0A73", "0A74", 
"0A81", "0A82", "0A83", "0A85", "0A86", "0A87", "0A88", "0A89", 
"0A8A", "0A8B", "0A8D", "0A8F", "0A90", "0A91", "0A93", "0A94", 
"0A95", "0A96", "0A97", "0A98", "0A99", "0A9A", "0A9B", "0A9C", 
"0A9D", "0A9E", "0A9F", "0AA0", "0AA1", "0AA2", "0AA3", "0AA4", 
"0AA5", "0AA6", "0AA7", "0AA8", "0AAA", "0AAB", "0AAC", "0AAD", 
"0AAE", "0AAF", "0AB0", "0AB2", "0AB3", "0AB5", "0AB6", "0AB7", 
"0AB8", "0AB9", "0ABC", "0ABE", "0ABF", "0AC0", "0AC1", "0AC2", 
"0AC3", "0AC4", "0AC5", "0AC7", "0AC8", "0AC9", "0ACB", "0ACC", 
"0ACD", "0AD0", "0AE0", "0AE6", "0AE7", "0AE8", "0AE9", "0AEA", 
"0AEB", "0AEC", "0AED", "0AEE", "0AEF", "0E01", "0E02", "0E03", 
"0E04", "0E05", "0E06", "0E07", "0E08", "0E09", "0E0A", "0E0B", 
"0E0C", "0E0D", "0E0E", "0E0F", "0E10", "0E11", "0E12", "0E13", 
"0E14", "0E15", "0E16", "0E17", "0E18", "0E19", "0E1A", "0E1B", 
"0E1C", "0E1D", "0E1E", "0E1F", "0E20", "0E21", "0E22", "0E23", 
"0E24", "0E25", "0E26", "0E27", "0E28", "0E29", "0E2A", "0E2B", 
"0E2C", "0E2D", "0E2E", "0E2F", "0E30", "0E31", "0E32", "0E33", 
"0E34", "0E35", "0E36", "0E37", "0E38", "0E39", "0E3A", "0E3F", 
"0E40", "0E41", "0E42", "0E43", "0E44", "0E45", "0E46", "0E47", 
"0E48", "0E49", "0E4A", "0E4B", "0E4C", "0E4D", "0E4E", "0E4F", 
"0E50", "0E51", "0E52", "0E53", "0E54", "0E55", "0E56", "0E57", 
"0E58", "0E59", "0E5A", "0E5B", "1E00", "1E01", "1E02", "1E03", 
"1E04", "1E05", "1E06", "1E07", "1E08", "1E09", "1E0A", "1E0B", 
"1E0C", "1E0D", "1E0E", "1E0F", "1E10", "1E11", "1E12", "1E13", 
"1E14", "1E15", "1E16", "1E17", "1E18", "1E19", "1E1A", "1E1B", 
"1E1C", "1E1D", "1E1E", "1E1F", "1E20", "1E21", "1E22", "1E23", 
"1E24", "1E25", "1E26", "1E27", "1E28", "1E29", "1E2A", "1E2B", 
"1E2C", "1E2D", "1E2E", "1E2F", "1E30", "1E31", "1E32", "1E33", 
"1E34", "1E35", "1E36", "1E37", "1E38", "1E39", "1E3A", "1E3B", 
"1E3C", "1E3D", "1E3E", "1E3F", "1E40", "1E41", "1E42", "1E43", 
"1E44", "1E45", "1E46", "1E47", "1E48", "1E49", "1E4A", "1E4B", 
"1E4C", "1E4D", "1E4E", "1E4F", "1E50", "1E51", "1E52", "1E53", 
"1E54", "1E55", "1E56", "1E57", "1E58", "1E59", "1E5A", "1E5B", 
"1E5C", "1E5D", "1E5E", "1E5F", "1E60", "1E61", "1E62", "1E63", 
"1E64", "1E65", "1E66", "1E67", "1E68", "1E69", "1E6A", "1E6B", 
"1E6C", "1E6D", "1E6E", "1E6F", "1E70", "1E71", "1E72", "1E73", 
"1E74", "1E75", "1E76", "1E77", "1E78", "1E79", "1E7A", "1E7B", 
"1E7C", "1E7D", "1E7E", "1E7F", "1E80", "1E81", "1E82", "1E83", 
"1E84", "1E85", "1E86", "1E87", "1E88", "1E89", "1E8A", "1E8B", 
"1E8C", "1E8D", "1E8E", "1E8F", "1E90", "1E91", "1E92", "1E93", 
"1E94", "1E95", "1E96", "1E97", "1E98", "1E99", "1E9A", "1E9B", 
"1EA0", "1EA1", "1EA2", "1EA3", "1EA4", "1EA5", "1EA6", "1EA7", 
"1EA8", "1EA9", "1EAA", "1EAB", "1EAC", "1EAD", "1EAE", "1EAF", 
"1EB0", "1EB1", "1EB2", "1EB3", "1EB4", "1EB5", "1EB6", "1EB7", 
"1EB8", "1EB9", "1EBA", "1EBB", "1EBC", "1EBD", "1EBE", "1EBF", 
"1EC0", "1EC1", "1EC2", "1EC3", "1EC4", "1EC5", "1EC6", "1EC7", 
"1EC8", "1EC9", "1ECA", "1ECB", "1ECC", "1ECD", "1ECE", "1ECF", 
"1ED0", "1ED1", "1ED2", "1ED3", "1ED4", "1ED5", "1ED6", "1ED7", 
"1ED8", "1ED9", "1EDA", "1EDB", "1EDC", "1EDD", "1EDE", "1EDF", 
"1EE0", "1EE1", "1EE2", "1EE3", "1EE4", "1EE5", "1EE6", "1EE7", 
"1EE8", "1EE9", "1EEA", "1EEB", "1EEC", "1EED", "1EEE", "1EEF", 
"1EF0", "1EF1", "1EF2", "1EF3", "1EF4", "1EF5", "1EF6", "1EF7", 
"1EF8", "1EF9", "2002", "200B", "200C", "200C", "200D", "200E", 
"200F", "2010", "2012", "2013", "2014", "2015", "2015", "2016", 
"2017", "2017", "2018", "2019", "201A", "201B", "201B", "201C", 
"201D", "201E", "2020", "2021", "2022", "2024", "2025", "2025", 
"2026", "202C", "202D", "202E", "2030", "2032", "2033", "2035", 
"2039", "203A", "203B", "203C", "203E", "2042", "2044", "2070", 
"2074", "2075", "2076", "2077", "2078", "2079", "207A", "207C", 
"207D", "207E", "207F", "2080", "2081", "2082", "2083", "2084", 
"2085", "2086", "2087", "2088", "2089", "208D", "208E", "20A1", 
"20A1", "20A2", "20A3", "20A4", "20A4", "20A7", "20A9", "20AA", 
"20AA", "20AA", "20AA", "20AB", "20AC", "20AC", "2103", "2105", 
"2105", "2109", "2111", "2113", "2113", "2116", "2116", "2118", 
"211C", "211E", "2121", "2122", "2126", "2126", "212B", "212E", 
"2135", "2153", "2154", "215B", "215C", "215D", "215E", "2160", 
"2161", "2162", "2163", "2164", "2165", "2166", "2167", "2168", 
"2169", "216A", "216B", "2170", "2171", "2172", "2173", "2174", 
"2175", "2176", "2177", "2178", "2179", "217A", "217B", "2190", 
"2191", "2192", "2193", "2194", "2195", "2196", "2197", "2198", 
"2199", "21A8", "21A8", "21B5", "21BC", "21C0", "21C4", "21C5", 
"21C6", "21CD", "21CF", "21D0", "21D0", "21D1", "21D2", "21D2", 
"21D3", "21D4", "21D4", "21DE", "21DF", "21E0", "21E1", "21E2", 
"21E3", "21E4", "21E5", "21E6", "21E7", "21E8", "21E9", "21EA", 
"2200", "2200", "2202", "2203", "2203", "2205", "2206", "2206", 
"2207", "2207", "2208", "2209", "2209", "220B", "220C", "220F", 
"2211", "2212", "2213", "2215", "2217", "2219", "221A", "221D", 
"221E", "221F", "221F", "2220", "2223", "2225", "2226", "2227", 
"2228", "2229", "222A", "222B", "222C", "222E", "2234", "2235", 
"2236", "2237", "223C", "223C", "223D", "2243", "2245", "2245", 
"2248", "224C", "2250", "2251", "2252", "2253", "2260", "2261", 
"2262", "2264", "2265", "2266", "2267", "226A", "226B", "226E", 
"226F", "2270", "2271", "2272", "2273", "2276", "2277", "2279", 
"227A", "227B", "2280", "2281", "2282", "2282", "2283", "2283", 
"2284", "2285", "2286", "2286", "2287", "2287", "228A", "228B", 
"2295", "2295", "2296", "2297", "2297", "2299", "22A3", "22A4", 
"22A5", "22BF", "22C5", "22CE", "22CF", "22DA", "22DB", "22EE", 
"2302", "2303", "2305", "2310", "2310", "2312", "2318", "2320", 
"2320", "2321", "2321", "2325", "2326", "2327", "2329", "232A", 
"232B", "2423", "2460", "2461", "2462", "2463", "2464", "2465", 
"2466", "2467", "2468", "2469", "246A", "246B", "246C", "246D", 
"246E", "246F", "2470", "2471", "2472", "2473", "2474", "2475", 
"2476", "2477", "2478", "2479", "247A", "247B", "247C", "247D", 
"247E", "247F", "2480", "2481", "2482", "2483", "2484", "2485", 
"2486", "2487", "2488", "2489", "248A", "248B", "248C", "248D", 
"248E", "248F", "2490", "2491", "2492", "2493", "2494", "2495", 
"2496", "2497", "2498", "2499", "249A", "249B", "249C", "249D", 
"249E", "249F", "24A0", "24A1", "24A2", "24A3", "24A4", "24A5", 
"24A6", "24A7", "24A8", "24A9", "24AA", "24AB", "24AC", "24AD", 
"24AE", "24AF", "24B0", "24B1", "24B2", "24B3", "24B4", "24B5", 
"24B6", "24B7", "24B8", "24B9", "24BA", "24BB", "24BC", "24BD", 
"24BE", "24BF", "24C0", "24C1", "24C2", "24C3", "24C4", "24C5", 
"24C6", "24C7", "24C8", "24C9", "24CA", "24CB", "24CC", "24CD", 
"24CE", "24CF", "24D0", "24D1", "24D2", "24D3", "24D4", "24D5", 
"24D6", "24D7", "24D8", "24D9", "24DA", "24DB", "24DC", "24DD", 
"24DE", "24DF", "24E0", "24E1", "24E2", "24E3", "24E4", "24E5", 
"24E6", "24E7", "24E8", "24E9", "2500", "2502", "250C", "2510", 
"2514", "2518", "251C", "2524", "252C", "2534", "253C", "2550", 
"2551", "2552", "2553", "2554", "2555", "2556", "2557", "2558", 
"2559", "255A", "255B", "255C", "255D", "255E", "255F", "2560", 
"2561", "2562", "2563", "2564", "2565", "2566", "2567", "2568", 
"2569", "256A", "256B", "256C", "2580", "2584", "2588", "258C", 
"2590", "2591", "2591", "2592", "2592", "2593", "2593", "25A0", 
"25A0", "25A1", "25A1", "25A3", "25A4", "25A5", "25A6", "25A7", 
"25A8", "25A9", "25AA", "25AA", "25AB", "25AB", "25AC", "25AC", 
"25B2", "25B2", "25B3", "25B4", "25B5", "25B6", "25B7", "25B9", 
"25BA", "25BA", "25BC", "25BC", "25BD", "25BF", "25C0", "25C1", 
"25C3", "25C4", "25C4", "25C6", "25C7", "25C8", "25C9", "25CA", 
"25CB", "25CB", "25CC", "25CE", "25CF", "25CF", "25D0", "25D1", 
"25D8", "25D8", "25D9", "25D9", "25E2", "25E3", "25E4", "25E5", 
"25E6", "25E6", "25EF", "2605", "2606", "260E", "260F", "261C", 
"261D", "261E", "261F", "262F", "263A", "263A", "263B", "263B", 
"263C", "263C", "2640", "2640", "2641", "2642", "2642", "2660", 
"2660", "2661", "2662", "2663", "2663", "2664", "2665", "2665", 
"2666", "2667", "2668", "2669", "266A", "266B", "266B", "266C", 
"266D", "266F", "2713", "278A", "278B", "278C", "278D", "278E", 
"278F", "2790", "2791", "2792", "279E", "3000", "3001", "3002", 
"3003", "3004", "3005", "3006", "3007", "3008", "3009", "300A", 
"300B", "300C", "300D", "300E", "300F", "3010", "3011", "3012", 
"3013", "3014", "3015", "3016", "3017", "3018", "3019", "301C", 
"301D", "301E", "3020", "3021", "3022", "3023", "3024", "3025", 
"3026", "3027", "3028", "3029", "3036", "3041", "3042", "3043", 
"3044", "3045", "3046", "3047", "3048", "3049", "304A", "304B", 
"304C", "304D", "304E", "304F", "3050", "3051", "3052", "3053", 
"3054", "3055", "3056", "3057", "3058", "3059", "305A", "305B", 
"305C", "305D", "305E", "305F", "3060", "3061", "3062", "3063", 
"3064", "3065", "3066", "3067", "3068", "3069", "306A", "306B", 
"306C", "306D", "306E", "306F", "3070", "3071", "3072", "3073", 
"3074", "3075", "3076", "3077", "3078", "3079", "307A", "307B", 
"307C", "307D", "307E", "307F", "3080", "3081", "3082", "3083", 
"3084", "3085", "3086", "3087", "3088", "3089", "308A", "308B", 
"308C", "308D", "308E", "308F", "3090", "3091", "3092", "3093", 
"3094", "309B", "309C", "309D", "309E", "30A1", "30A2", "30A3", 
"30A4", "30A5", "30A6", "30A7", "30A8", "30A9", "30AA", "30AB", 
"30AC", "30AD", "30AE", "30AF", "30B0", "30B1", "30B2", "30B3", 
"30B4", "30B5", "30B6", "30B7", "30B8", "30B9", "30BA", "30BB", 
"30BC", "30BD", "30BE", "30BF", "30C0", "30C1", "30C2", "30C3", 
"30C4", "30C5", "30C6", "30C7", "30C8", "30C9", "30CA", "30CB", 
"30CC", "30CD", "30CE", "30CF", "30D0", "30D1", "30D2", "30D3", 
"30D4", "30D5", "30D6", "30D7", "30D8", "30D9", "30DA", "30DB", 
"30DC", "30DD", "30DE", "30DF", "30E0", "30E1", "30E2", "30E3", 
"30E4", "30E5", "30E6", "30E7", "30E8", "30E9", "30EA", "30EB", 
"30EC", "30ED", "30EE", "30EF", "30F0", "30F1", "30F2", "30F3", 
"30F4", "30F5", "30F6", "30F7", "30F8", "30F9", "30FA", "30FB", 
"30FC", "30FD", "30FE", "3105", "3106", "3107", "3108", "3109", 
"310A", "310B", "310C", "310D", "310E", "310F", "3110", "3111", 
"3112", "3113", "3114", "3115", "3116", "3117", "3118", "3119", 
"311A", "311B", "311C", "311D", "311E", "311F", "3120", "3121", 
"3122", "3123", "3124", "3125", "3126", "3127", "3128", "3129", 
"3131", "3132", "3133", "3134", "3135", "3136", "3137", "3138", 
"3139", "313A", "313B", "313C", "313D", "313E", "313F", "3140", 
"3141", "3142", "3143", "3144", "3145", "3146", "3147", "3148", 
"3149", "314A", "314B", "314C", "314D", "314E", "314F", "3150", 
"3151", "3152", "3153", "3154", "3155", "3156", "3157", "3158", 
"3159", "315A", "315B", "315C", "315D", "315E", "315F", "3160", 
"3161", "3162", "3163", "3164", "3165", "3166", "3167", "3168", 
"3169", "316A", "316B", "316C", "316D", "316E", "316F", "3170", 
"3171", "3172", "3173", "3174", "3175", "3176", "3177", "3178", 
"3179", "317A", "317B", "317C", "317D", "317E", "317F", "3180", 
"3181", "3182", "3183", "3184", "3185", "3186", "3187", "3188", 
"3189", "318A", "318B", "318C", "318D", "318E", "3200", "3201", 
"3202", "3203", "3204", "3205", "3206", "3207", "3208", "3209", 
"320A", "320B", "320C", "320D", "320E", "320F", "3210", "3211", 
"3212", "3213", "3214", "3215", "3216", "3217", "3218", "3219", 
"321A", "321B", "321C", "3220", "3221", "3222", "3223", "3224", 
"3225", "3226", "3227", "3228", "3229", "322A", "322B", "322C", 
"322D", "322E", "322F", "3230", "3231", "3232", "3233", "3234", 
"3235", "3236", "3237", "3238", "3239", "323A", "323B", "323C", 
"323D", "323E", "323F", "3240", "3242", "3243", "3260", "3261", 
"3262", "3263", "3264", "3265", "3266", "3267", "3268", "3269", 
"326A", "326B", "326C", "326D", "326E", "326F", "3270", "3271", 
"3272", "3273", "3274", "3275", "3276", "3277", "3278", "3279", 
"327A", "327B", "327F", "328A", "328B", "328C", "328D", "328E", 
"328F", "3290", "3294", "3296", "3298", "3299", "329D", "329E", 
"32A3", "32A4", "32A5", "32A6", "32A7", "32A8", "32A9", "3300", 
"3303", "3305", "330D", "3314", "3315", "3316", "3318", "331E", 
"3322", "3323", "3326", "3327", "332A", "332B", "3331", "3333", 
"3336", "3339", "333B", "3342", "3347", "3349", "334A", "334D", 
"334E", "3351", "3357", "337B", "337C", "337D", "337E", "337F", 
"3380", "3381", "3382", "3383", "3384", "3385", "3386", "3387", 
"3388", "3389", "338A", "338B", "338C", "338D", "338E", "338F", 
"3390", "3391", "3392", "3393", "3394", "3395", "3396", "3397", 
"3398", "3399", "339A", "339B", "339C", "339D", "339E", "339F", 
"33A0", "33A1", "33A2", "33A3", "33A4", "33A5", "33A6", "33A7", 
"33A8", "33A9", "33AA", "33AB", "33AC", "33AD", "33AE", "33AF", 
"33B0", "33B1", "33B2", "33B3", "33B4", "33B5", "33B6", "33B7", 
"33B8", "33B9", "33BA", "33BB", "33BC", "33BD", "33BE", "33BF", 
"33C0", "33C1", "33C2", "33C3", "33C4", "33C5", "33C6", "33C7", 
"33C8", "33C9", "33CA", "33CB", "33CD", "33CE", "33CF", "33D0", 
"33D1", "33D2", "33D3", "33D4", "33D5", "33D6", "33D8", "33DB", 
"33DC", "33DD", "5344", "F6BE", "F6BF", "F6C0", "F6C3", "F6C4", 
"F6C5", "F6C6", "F6C7", "F6C8", "F6C9", "F6CA", "F6CB", "F6CC", 
"F6CD", "F6CE", "F6CF", "F6D0", "F6D1", "F6D2", "F6D3", "F6D4", 
"F6D5", "F6D6", "F6D7", "F6D8", "F6D9", "F6DA", "F6DB", "F6DC", 
"F6DD", "F6DE", "F6DF", "F6E0", "F6E1", "F6E2", "F6E3", "F6E4", 
"F6E5", "F6E6", "F6E7", "F6E8", "F6E9", "F6EA", "F6EB", "F6EC", 
"F6ED", "F6EE", "F6EF", "F6F0", "F6F1", "F6F2", "F6F3", "F6F4", 
"F6F5", "F6F6", "F6F7", "F6F8", "F6F9", "F6FA", "F6FB", "F6FC", 
"F6FD", "F6FE", "F6FF", "F721", "F724", "F726", "F730", "F731", 
"F732", "F733", "F734", "F735", "F736", "F737", "F738", "F739", 
"F73F", "F760", "F761", "F762", "F763", "F764", "F765", "F766", 
"F767", "F768", "F769", "F76A", "F76B", "F76C", "F76D", "F76E", 
"F76F", "F770", "F771", "F772", "F773", "F774", "F775", "F776", 
"F777", "F778", "F779", "F77A", "F7A1", "F7A2", "F7A8", "F7AF", 
"F7B4", "F7B8", "F7BF", "F7E0", "F7E1", "F7E2", "F7E3", "F7E4", 
"F7E5", "F7E6", "F7E7", "F7E8", "F7E9", "F7EA", "F7EB", "F7EC", 
"F7ED", "F7EE", "F7EF", "F7F0", "F7F1", "F7F2", "F7F3", "F7F4", 
"F7F5", "F7F6", "F7F8", "F7F9", "F7FA", "F7FB", "F7FC", "F7FD", 
"F7FE", "F7FF", "F884", "F885", "F886", "F887", "F888", "F889", 
"F88A", "F88B", "F88C", "F88D", "F88E", "F88F", "F890", "F891", 
"F892", "F893", "F894", "F895", "F896", "F897", "F898", "F899", 
"F8E5", "F8E6", "F8E7", "F8E8", "F8E9", "F8EA", "F8EB", "F8EC", 
"F8ED", "F8EE", "F8EF", "F8F0", "F8F1", "F8F2", "F8F3", "F8F4", 
"F8F5", "F8F6", "F8F7", "F8F8", "F8F9", "F8FA", "F8FB", "F8FC", 
"F8FD", "F8FE", "F8FF", "FB00", "FB01", "FB02", "FB03", "FB04", 
"FB1F", "FB1F", "FB1F", "FB1F", "FB20", "FB2A", "FB2A", "FB2A", 
"FB2B", "FB2B", "FB2B", "FB2C", "FB2C", "FB2D", "FB2D", "FB2E", 
"FB2F", "FB30", "FB31", "FB31", "FB32", "FB32", "FB33", "FB33", 
"FB34", "FB34", "FB35", "FB35", "FB35", "FB35", "FB36", "FB36", 
"FB38", "FB38", "FB39", "FB39", "FB3A", "FB3A", "FB3B", "FB3B", 
"FB3C", "FB3C", "FB3E", "FB3E", "FB40", "FB40", "FB41", "FB41", 
"FB43", "FB44", "FB44", "FB46", "FB46", "FB47", "FB47", "FB48", 
"FB49", "FB49", "FB4A", "FB4A", "FB4A", "FB4B", "FB4B", "FB4B", 
"FB4C", "FB4D", "FB4E", "FB4F", "FB57", "FB58", "FB59", "FB67", 
"FB68", "FB69", "FB6B", "FB6C", "FB6D", "FB7B", "FB7C", "FB7D", 
"FB89", "FB8B", "FB8D", "FB93", "FB94", "FB95", "FB9F", "FBA4", 
"FBA5", "FBA7", "FBA8", "FBA9", "FBAF", "FC08", "FC0B", "FC0C", 
"FC0E", "FC48", "FC4B", "FC4E", "FC58", "FC5E", "FC5F", "FC60", 
"FC61", "FC62", "FC6D", "FC73", "FC8D", "FC94", "FC9F", "FCA1", 
"FCA2", "FCA4", "FCC9", "FCCA", "FCCB", "FCCC", "FCD1", "FCD2", 
"FCD5", "FCDD", "FD3E", "FD3F", "FD88", "FDF2", "FDFA", "FE30", 
"FE31", "FE32", "FE33", "FE34", "FE35", "FE36", "FE37", "FE38", 
"FE39", "FE3A", "FE3B", "FE3C", "FE3D", "FE3E", "FE3F", "FE40", 
"FE41", "FE42", "FE43", "FE44", "FE49", "FE4A", "FE4B", "FE4C", 
"FE4D", "FE4E", "FE4F", "FE50", "FE52", "FE54", "FE55", "FE59", 
"FE5A", "FE5B", "FE5C", "FE5D", "FE5E", "FE5F", "FE61", "FE62", 
"FE63", "FE64", "FE65", "FE66", "FE69", "FE6A", "FE6B", "FE82", 
"FE84", "FE86", "FE88", "FE8A", "FE8B", "FE8C", "FE8E", "FE90", 
"FE91", "FE92", "FE94", "FE96", "FE97", "FE98", "FE9A", "FE9B", 
"FE9C", "FE9E", "FE9F", "FEA0", "FEA2", "FEA3", "FEA4", "FEA6", 
"FEA7", "FEA8", "FEAA", "FEAC", "FEAE", "FEB0", "FEB2", "FEB3", 
"FEB4", "FEB6", "FEB7", "FEB8", "FEBA", "FEBB", "FEBC", "FEBE", 
"FEBF", "FEC0", "FEC2", "FEC3", "FEC4", "FEC6", "FEC7", "FEC8", 
"FECA", "FECB", "FECC", "FECE", "FECF", "FED0", "FED2", "FED3", 
"FED4", "FED6", "FED7", "FED8", "FEDA", "FEDB", "FEDC", "FEDE", 
"FEDF", "FEE0", "FEE2", "FEE3", "FEE4", "FEE6", "FEE7", "FEE8", 
"FEEA", "FEEA", "FEEB", "FEEC", "FEEE", "FEF0", "FEF2", "FEF3", 
"FEF3", "FEF4", "FEF4", "FEF5", "FEF6", "FEF7", "FEF8", "FEF9", 
"FEFA", "FEFB", "FEFC", "FEFF", "FF01", "FF02", "FF03", "FF04", 
"FF05", "FF06", "FF07", "FF08", "FF09", "FF0A", "FF0B", "FF0C", 
"FF0D", "FF0E", "FF0F", "FF10", "FF11", "FF12", "FF13", "FF14", 
"FF15", "FF16", "FF17", "FF18", "FF19", "FF1A", "FF1B", "FF1C", 
"FF1D", "FF1E", "FF1F", "FF20", "FF21", "FF22", "FF23", "FF24", 
"FF25", "FF26", "FF27", "FF28", "FF29", "FF2A", "FF2B", "FF2C", 
"FF2D", "FF2E", "FF2F", "FF30", "FF31", "FF32", "FF33", "FF34", 
"FF35", "FF36", "FF37", "FF38", "FF39", "FF3A", "FF3B", "FF3C", 
"FF3D", "FF3E", "FF3F", "FF40", "FF41", "FF42", "FF43", "FF44", 
"FF45", "FF46", "FF47", "FF48", "FF49", "FF4A", "FF4B", "FF4C", 
"FF4D", "FF4E", "FF4F", "FF50", "FF51", "FF52", "FF53", "FF54", 
"FF55", "FF56", "FF57", "FF58", "FF59", "FF5A", "FF5B", "FF5C", 
"FF5D", "FF5E", "FF61", "FF62", "FF63", "FF64", "FF65", "FF66", 
"FF67", "FF68", "FF69", "FF6A", "FF6B", "FF6C", "FF6D", "FF6E", 
"FF6F", "FF70", "FF71", "FF72", "FF73", "FF74", "FF75", "FF76", 
"FF77", "FF78", "FF79", "FF7A", "FF7B", "FF7C", "FF7D", "FF7E", 
"FF7F", "FF80", "FF81", "FF82", "FF83", "FF84", "FF85", "FF86", 
"FF87", "FF88", "FF89", "FF8A", "FF8B", "FF8C", "FF8D", "FF8E", 
"FF8F", "FF90", "FF91", "FF92", "FF93", "FF94", "FF95", "FF96", 
"FF97", "FF98", "FF99", "FF9A", "FF9B", "FF9C", "FF9D", "FF9E", 
"FF9F", "FFE0", "FFE1", "FFE3", "FFE5", "FFE6")), .Names = c("adobe", 
"unicode"), row.names = c(1455L, 1454L, 1444L, 1441L, 1440L, 
1428L, 1429L, 1430L, 1448L, 1449L, 1459L, 1445L, 1432L, 1453L, 
1452L, 1438L, 1433L, 1434L, 1435L, 1436L, 1450L, 1457L, 1443L, 
1431L, 1439L, 1456L, 1442L, 1446L, 1447L, 1451L, 1458L, 3498L, 
3499L, 1774L, 3133L, 2778L, 1595L, 2991L, 1078L, 3146L, 2939L, 
2949L, 1164L, 3043L, 1410L, 2126L, 2995L, 3480L, 4165L, 2859L, 
3790L, 3666L, 1858L, 1829L, 3456L, 3359L, 1669L, 2697L, 1403L, 
3351L, 2462L, 1736L, 1970L, 3125L, 1173L, 1L, 52L, 65L, 94L, 
128L, 193L, 204L, 233L, 257L, 302L, 310L, 338L, 360L, 373L, 394L, 
460L, 475L, 479L, 500L, 571L, 604L, 659L, 669L, 679L, 687L, 712L, 
1285L, 1195L, 1290L, 1155L, 3862L, 1962L, 735L, 1193L, 1313L, 
1482L, 1630L, 1787L, 1886L, 1988L, 2132L, 2274L, 2302L, 2416L, 
2511L, 2648L, 2794L, 2921L, 3083L, 3148L, 3279L, 3556L, 3816L, 
3902L, 3951L, 4020L, 4031L, 4132L, 1271L, 1203L, 3925L, 1278L, 
1157L, 1437L, 2661L, 2736L, 1777L, 1343L, 3536L, 1477L, 4068L, 
1303L, 3333L, 1572L, 1460L, 2894L, 1981L, 2491L, 3383L, 3487L, 
3181L, 2513L, 2917L, 1552L, 3046L, 3813L, 3688L, 765L, 2621L, 
2622L, 2937L, 2583L, 2997L, 1341L, 2883L, 2895L, 1982L, 2881L, 
2871L, 3685L, 3128L, 34L, 6L, 17L, 49L, 28L, 44L, 2L, 71L, 152L, 
129L, 136L, 146L, 280L, 261L, 266L, 270L, 185L, 391L, 420L, 397L, 
405L, 456L, 415L, 2633L, 450L, 625L, 605L, 610L, 616L, 688L, 
587L, 1918L, 1031L, 737L, 759L, 1174L, 776L, 1112L, 781L, 1333L, 
1662L, 1631L, 1645L, 1655L, 2207L, 2133L, 2140L, 2196L, 1761L, 
2769L, 2826L, 2795L, 2809L, 2907L, 2819L, 1581L, 2899L, 3839L, 
3817L, 3824L, 3831L, 4034L, 3658L, 4050L, 41L, 1076L, 8L, 750L, 
43L, 1098L, 67L, 1316L, 75L, 1336L, 76L, 77L, 1338L, 1339L, 70L, 
1332L, 99L, 1531L, 103L, 120L, 1535L, 1588L, 161L, 1707L, 131L, 
1635L, 148L, 149L, 1656L, 1657L, 171L, 1726L, 132L, 1640L, 214L, 
1907L, 210L, 1903L, 216L, 217L, 1909L, 1910L, 212L, 215L, 1905L, 
1908L, 246L, 2041L, 242L, 2036L, 298L, 2265L, 286L, 2229L, 263L, 
2137L, 291L, 2249L, 274L, 275L, 1608L, 259L, 2223L, 305L, 2283L, 
322L, 324L, 2343L, 2345L, 2353L, 341L, 2418L, 344L, 347L, 2452L, 
2455L, 343L, 2451L, 348L, 349L, 2456L, 2457L, 357L, 2501L, 375L, 
2651L, 377L, 380L, 2663L, 2666L, 376L, 2662L, 2658L, 3144L, 168L, 
1721L, 434L, 2844L, 401L, 2802L, 413L, 431L, 2816L, 2837L, 395L, 
2822L, 481L, 3151L, 483L, 485L, 3171L, 3173L, 482L, 3170L, 541L, 
3281L, 552L, 3325L, 547L, 3319L, 544L, 3317L, 575L, 578L, 3582L, 
3589L, 574L, 3580L, 573L, 3578L, 656L, 3888L, 638L, 3857L, 607L, 
3821L, 651L, 3881L, 614L, 634L, 3828L, 3850L, 642L, 3869L, 672L, 
3969L, 692L, 4049L, 693L, 714L, 4134L, 719L, 720L, 4157L, 4158L, 
715L, 4153L, 2495L, 3482L, 1304L, 59L, 64L, 1306L, 596L, 3725L, 
449L, 89L, 1374L, 98L, 110L, 122L, 1620L, 1560L, 176L, 548L, 
172L, 198L, 1851L, 223L, 208L, 2125L, 293L, 297L, 330L, 2376L, 
2448L, 2433L, 371L, 383L, 2724L, 403L, 425L, 2831L, 432L, 2838L, 
468L, 3022L, 4111L, 597L, 3726L, 180L, 1751L, 3737L, 586L, 3656L, 
598L, 628L, 3844L, 646L, 664L, 700L, 4081L, 734L, 4197L, 190L, 
192L, 1785L, 1786L, 3812L, 595L, 3724L, 1946L, 4019L, 1393L, 
1394L, 1392L, 1395L, 96L, 124L, 1625L, 339L, 353L, 2475L, 374L, 
385L, 2720L, 15L, 757L, 264L, 2138L, 402L, 2807L, 608L, 3822L, 
622L, 3837L, 617L, 3832L, 619L, 3834L, 621L, 3836L, 1768L, 30L, 
778L, 33L, 780L, 4L, 784L, 232L, 1979L, 211L, 1904L, 321L, 2342L, 
447L, 2886L, 448L, 2887L, 191L, 1783L, 2281L, 95L, 123L, 1623L, 
206L, 1888L, 45L, 1113L, 3L, 782L, 451L, 454L, 2900L, 2904L, 
27L, 773L, 38L, 1047L, 145L, 1653L, 157L, 1693L, 269L, 2142L, 
284L, 2218L, 414L, 2817L, 433L, 2839L, 486L, 3174L, 494L, 3240L, 
615L, 3829L, 636L, 3852L, 553L, 3326L, 1177L, 1159L, 1160L, 1233L, 
2890L, 1337L, 1619L, 1567L, 1743L, 3320L, 3323L, 1728L, 1730L, 
1732L, 1731L, 2285L, 1936L, 1978L, 1899L, 3167L, 2118L, 2072L, 
2068L, 2261L, 2253L, 2486L, 2449L, 2472L, 2470L, 2620L, 2603L, 
2580L, 2680L, 2681L, 2797L, 2851L, 3019L, 3264L, 3244L, 3203L, 
3243L, 3202L, 3197L, 3198L, 498L, 3434L, 1747L, 1610L, 1752L, 
1748L, 3775L, 3742L, 3818L, 3876L, 3931L, 3948L, 4018L, 4115L, 
4196L, 4156L, 1782L, 1784L, 1947L, 1950L, 1948L, 1474L, 1236L, 
1729L, 231L, 2284L, 2410L, 3109L, 1953L, 1954L, 1624L, 1562L, 
1626L, 3747L, 3620L, 3581L, 2117L, 2073L, 2301L, 3265L, 3204L, 
499L, 4017L, 4113L, 3065L, 1526L, 1423L, 1021L, 1102L, 1030L, 
1419L, 3237L, 3233L, 1949L, 1951L, 1130L, 1131L, 1132L, 1129L, 
1388L, 1327L, 3929L, 1826L, 3332L, 1882L, 3928L, 2516L, 1967L, 
770L, 1409L, 1408L, 3239L, 3236L, 3879L, 1616L, 3047L, 2598L, 
1295L, 1603L, 3230L, 2824L, 2227L, 3699L, 2123L, 3206L, 1900L, 
4030L, 1952L, 3719L, 3721L, 3723L, 3722L, 3720L, 1964L, 1965L, 
767L, 768L, 1390L, 3701L, 3702L, 2515L, 2913L, 1297L, 1604L, 
1575L, 2106L, 2107L, 3232L, 2124L, 1329L, 3926L, 1528L, 1521L, 
1322L, 1299L, 1422L, 1411L, 1418L, 1412L, 1963L, 766L, 2461L, 
3225L, 2460L, 2113L, 3235L, 3878L, 1615L, 3044L, 2596L, 2108L, 
2109L, 1605L, 1606L, 1574L, 3231L, 1342L, 2825L, 3927L, 1301L, 
1516L, 1328L, 1389L, 1296L, 1298L, 3700L, 2514L, 2497L, 1524L, 
3705L, 3539L, 3538L, 3493L, 3492L, 3238L, 1302L, 3504L, 3330L, 
4021L, 3706L, 1525L, 1969L, 771L, 3003L, 2405L, 1569L, 4110L, 
3703L, 1300L, 2781L, 2782L, 4109L, 3130L, 3727L, 1568L, 1577L, 
40L, 1093L, 174L, 184L, 295L, 444L, 650L, 442L, 2252L, 39L, 58L, 
207L, 109L, 173L, 725L, 182L, 585L, 292L, 318L, 342L, 372L, 393L, 
684L, 443L, 469L, 492L, 565L, 572L, 643L, 467L, 88L, 472L, 439L, 
294L, 647L, 1075L, 1735L, 1760L, 2254L, 3875L, 1074L, 1223L, 
1898L, 1559L, 1734L, 4181L, 1758L, 3644L, 2250L, 2324L, 2432L, 
2627L, 2770L, 4027L, 2856L, 3026L, 3201L, 3436L, 3437L, 3435L, 
3572L, 3873L, 3012L, 1364L, 3075L, 2848L, 2251L, 3874L, 2857L, 
3877L, 2854L, 1224L, 3645L, 3646L, 644L, 649L, 645L, 648L, 3013L, 
3021L, 2849L, 3041L, 570L, 115L, 335L, 543L, 562L, 3407L, 197L, 
1802L, 329L, 2363L, 252L, 2111L, 209L, 1901L, 564L, 3420L, 107L, 
1555L, 2325L, 3205L, 3438L, 4103L, 290L, 793L, 116L, 821L, 225L, 
822L, 144L, 823L, 126L, 824L, 268L, 825L, 703L, 826L, 306L, 827L, 
354L, 828L, 386L, 829L, 600L, 830L, 331L, 831L, 652L, 832L, 127L, 
881L, 26L, 787L, 56L, 788L, 662L, 789L, 218L, 790L, 106L, 791L, 
278L, 792L, 728L, 794L, 722L, 795L, 283L, 796L, 285L, 797L, 315L, 
798L, 159L, 799L, 164L, 800L, 166L, 801L, 412L, 802L, 464L, 803L, 
175L, 804L, 178L, 805L, 581L, 806L, 613L, 807L, 151L, 808L, 328L, 
809L, 599L, 810L, 81L, 811L, 560L, 812L, 561L, 813L, 241L, 814L, 
697L, 815L, 568L, 816L, 177L, 817L, 260L, 818L, 258L, 819L, 772L, 
835L, 836L, 1211L, 837L, 3918L, 838L, 1911L, 839L, 1551L, 840L, 
2201L, 842L, 4185L, 843L, 4160L, 844L, 2213L, 845L, 2219L, 846L, 
2306L, 847L, 1699L, 848L, 1710L, 849L, 1717L, 850L, 2815L, 851L, 
2976L, 852L, 1742L, 853L, 1745L, 854L, 3593L, 855L, 3826L, 856L, 
1661L, 857L, 2355L, 858L, 3752L, 859L, 1357L, 860L, 3386L, 861L, 
3402L, 862L, 2008L, 863L, 4074L, 864L, 3488L, 865L, 1744L, 866L, 
2268L, 867L, 2134L, 841L, 2248L, 869L, 1584L, 870L, 1945L, 871L, 
1652L, 872L, 1628L, 873L, 2141L, 874L, 4084L, 875L, 2286L, 876L, 
2476L, 877L, 2721L, 878L, 3761L, 879L, 2391L, 880L, 3882L, 886L, 
1629L, 438L, 2850L, 690L, 882L, 887L, 4047L, 158L, 1694L, 710L, 
4123L, 711L, 4124L, 708L, 4121L, 709L, 4122L, 336L, 2408L, 473L, 
3076L, 199L, 883L, 888L, 1828L, 300L, 884L, 889L, 2272L, 301L, 
2273L, 637L, 3855L, 440L, 2852L, 441L, 2853L, 455L, 2906L, 334L, 
2403L, 3663L, 3711L, 2934L, 1510L, 3077L, 222L, 820L, 868L, 1933L, 
221L, 1932L, 220L, 1931L, 729L, 4186L, 723L, 4161L, 316L, 2307L, 
320L, 2339L, 319L, 2337L, 313L, 2303L, 167L, 1720L, 169L, 1723L, 
466L, 2989L, 239L, 1989L, 179L, 1746L, 582L, 3594L, 654L, 3886L, 
655L, 3887L, 240L, 1992L, 584L, 3625L, 83L, 1359L, 87L, 1363L, 
563L, 3419L, 80L, 1355L, 82L, 1358L, 2935L, 727L, 4184L, 317L, 
2321L, 170L, 1724L, 86L, 1362L, 10L, 752L, 29L, 777L, 37L, 1039L, 
277L, 2200L, 549L, 892L, 3321L, 550L, 3322L, 730L, 4187L, 724L, 
4162L, 125L, 1627L, 287L, 2230L, 272L, 2198L, 416L, 2820L, 399L, 
2798L, 400L, 2799L, 639L, 3858L, 620L, 3835L, 635L, 3851L, 84L, 
1360L, 698L, 4075L, 51L, 57L, 224L, 97L, 134L, 713L, 154L, 183L, 
594L, 726L, 289L, 352L, 683L, 66L, 327L, 251L, 303L, 219L, 85L, 
368L, 702L, 389L, 559L, 666L, 79L, 465L, 307L, 480L, 557L, 663L, 
591L, 490L, 92L, 704L, 470L, 326L, 422L, 196L, 3234L, 1101L, 
1714L, 1775L, 1415L, 3127L, 746L, 1189L, 1221L, 1940L, 1483L, 
1642L, 4133L, 1664L, 1759L, 3715L, 4183L, 2236L, 2474L, 4026L, 
1314L, 2351L, 2092L, 2275L, 1923L, 1361L, 2575L, 4083L, 2765L, 
3384L, 3940L, 1349L, 2982L, 2297L, 3149L, 3347L, 3930L, 3712L, 
3185L, 1402L, 4087L, 3042L, 2347L, 2828L, 1798L, 1643L, 2996L, 
1764L, 1765L, 1766L, 1767L, 3345L, 3400L, 4146L, 4145L, 3708L, 
3709L, 3194L, 3195L, 4147L, 2961L, 4079L, 3626L, 3627L, 1915L, 
1917L, 1919L, 3106L, 3613L, 2971L, 2635L, 2636L, 2522L, 2523L, 
2576L, 2579L, 2577L, 2578L, 1508L, 1509L, 3085L, 3614L, 4072L, 
4073L, 2843L, 2228L, 1553L, 4190L, 2552L, 1010L, 3410L, 3411L, 
3412L, 3413L, 3414L, 3415L, 3416L, 3417L, 3418L, 1012L, 2028L, 
2029L, 2030L, 2031L, 2032L, 2033L, 2034L, 2035L, 1011L, 2012L, 
2013L, 2014L, 2015L, 2016L, 2017L, 2018L, 2019L, 1013L, 2020L, 
2021L, 2022L, 2023L, 2024L, 2025L, 2026L, 2027L, 1004L, 2082L, 
2083L, 2084L, 2085L, 2086L, 2087L, 2088L, 2089L, 1005L, 3753L, 
3754L, 3755L, 3756L, 3757L, 3758L, 3759L, 3760L, 1006L, 3338L, 
3339L, 3340L, 3341L, 3342L, 3343L, 3344L, 3346L, 1009L, 2963L, 
2964L, 2965L, 2966L, 2967L, 2968L, 2969L, 2970L, 1008L, 3090L, 
3091L, 3092L, 3093L, 3094L, 3095L, 3096L, 3097L, 3098L, 3099L, 
3100L, 3101L, 3102L, 3103L, 3104L, 3105L, 1016L, 2097L, 2098L, 
2099L, 2100L, 2101L, 2102L, 2103L, 2104L, 1007L, 3117L, 3118L, 
3119L, 3120L, 3121L, 3122L, 3123L, 3124L, 1017L, 1490L, 1491L, 
1018L, 3442L, 3443L, 967L, 2550L, 1019L, 3158L, 3159L, 1020L, 
2960L, 1015L, 3428L, 1014L, 3445L, 968L, 3486L, 3872L, 969L, 
1054L, 1062L, 970L, 1222L, 1227L, 971L, 1941L, 1944L, 972L, 1499L, 
973L, 2045L, 2053L, 974L, 3907L, 3911L, 975L, 4148L, 4151L, 976L, 
2070L, 2071L, 977L, 3621L, 3624L, 978L, 4089L, 4092L, 979L, 1814L, 
1817L, 980L, 2309L, 2314L, 981L, 2434L, 2437L, 982L, 1818L, 1819L, 
983L, 2571L, 2574L, 984L, 1820L, 1821L, 985L, 2784L, 2787L, 986L, 
3294L, 3297L, 987L, 1190L, 1192L, 988L, 1822L, 1823L, 989L, 2975L, 
2983L, 990L, 1824L, 1825L, 991L, 3748L, 3751L, 992L, 3111L, 3114L, 
993L, 3190L, 3192L, 994L, 3421L, 3429L, 995L, 3573L, 3577L, 1000L, 
3914L, 1001L, 3915L, 1002L, 4093L, 1916L, 1920L, 897L, 1414L, 
908L, 3352L, 909L, 3126L, 910L, 2005L, 2006L, 911L, 1064L, 912L, 
1058L, 913L, 3965L, 914L, 1060L, 915L, 4057L, 916L, 1055L, 917L, 
1212L, 918L, 3603L, 919L, 3595L, 920L, 3638L, 921L, 2287L, 922L, 
1996L, 923L, 2359L, 924L, 1498L, 925L, 3632L, 926L, 3184L, 927L, 
4142L, 928L, 3334L, 929L, 3403L, 930L, 3283L, 931L, 1485L, 932L, 
3563L, 933L, 4137L, 934L, 1043L, 935L, 1927L, 936L, 2331L, 2332L, 
3571L, 937L, 1797L, 938L, 3086L, 939L, 2310L, 940L, 2431L, 941L, 
2560L, 942L, 2739L, 954L, 2052L, 943L, 3963L, 944L, 1066L, 945L, 
4053L, 946L, 1793L, 947L, 1505L, 1506L, 948L, 2336L, 949L, 1791L, 
1792L, 950L, 1503L, 1504L, 951L, 2335L, 952L, 3387L, 953L, 3548L, 
898L, 4166L, 4171L, 899L, 2860L, 2870L, 900L, 3791L, 3801L, 901L, 
3667L, 3675L, 902L, 1859L, 1866L, 903L, 1830L, 1838L, 904L, 3457L, 
3464L, 905L, 3360L, 3368L, 906L, 1670L, 1680L, 907L, 2698L, 2705L, 
896L, 2992L, 1549L, 1550L, 3664L, 3665L, 1029L, 1165L, 1166L, 
960L, 3767L, 956L, 2981L, 957L, 3583L, 961L, 1540L, 962L, 3254L, 
958L, 2291L, 955L, 3919L, 959L, 1890L, 963L, 2741L, 1990L, 2051L, 
4066L, 964L, 4054L, 965L, 4175L, 2880L, 3810L, 3684L, 1875L, 
1846L, 3472L, 3376L, 1689L, 2713L, 1323L, 1096L, 3937L, 775L, 
738L, 2195L, 2214L, 3830L, 3892L, 3274L, 2507L, 1636L, 1749L, 
1654L, 1038L, 2803L, 2897L, 2818L, 1180L, 2308L, 2356L, 1889L, 
1924L, 2675L, 1317L, 1351L, 2277L, 2294L, 2791L, 3764L, 3772L, 
1537L, 1544L, 2729L, 3560L, 3629L, 1486L, 1564L, 2652L, 2732L, 
2926L, 3009L, 1197L, 1230L, 2519L, 4035L, 3152L, 3252L, 2419L, 
2478L, 2481L, 3903L, 3397L, 3525L, 3284L, 1993L, 2775L, 1188L, 
744L, 2270L, 2221L, 3900L, 3897L, 3277L, 3260L, 1638L, 1750L, 
1772L, 1049L, 2805L, 2898L, 2919L, 1186L, 3934L, 2847L, 3827L, 
1094L, 1966L, 769L, 3084L, 2364L, 1934L, 4135L, 1542L, 3200L, 
1788L, 4131L, 3257L, 2483L, 2509L, 2485L, 1507L, 1519L, 4168L, 
2864L, 3795L, 3671L, 1863L, 1834L, 3461L, 3364L, 1674L, 2702L, 
747L, 1321L, 1095L, 3936L, 748L, 736L, 2135L, 2212L, 3819L, 3891L, 
3273L, 2506L, 1633L, 1036L, 2800L, 1178L, 2304L, 2354L, 1887L, 
1922L, 2674L, 1315L, 1350L, 2276L, 2293L, 2790L, 3763L, 3771L, 
1536L, 1543L, 2728L, 3557L, 3628L, 1484L, 1563L, 2649L, 2924L, 
3008L, 1194L, 1229L, 2512L, 4033L, 3150L, 2417L, 3385L, 3524L, 
3280L, 1991L, 2774L, 743L, 2269L, 2220L, 3899L, 3896L, 3276L, 
3259L, 1771L, 1048L, 2918L, 1185L, 3933L, 1183L, 3251L, 3199L, 
4130L, 3256L, 2482L, 2508L, 2484L, 4167L, 2861L, 3792L, 3668L, 
1860L, 1831L, 3458L, 3361L, 1671L, 2699L, 3166L, 3165L, 3269L, 
3270L, 2876L, 3806L, 3680L, 1871L, 1561L, 3476L, 2260L, 1237L, 
1033L, 740L, 2209L, 2216L, 3841L, 3894L, 1659L, 1041L, 2888L, 
1182L, 2319L, 2358L, 1895L, 1926L, 2677L, 1319L, 1353L, 2279L, 
2296L, 2793L, 3766L, 3774L, 1539L, 1546L, 2731L, 3562L, 3631L, 
1495L, 1566L, 2654L, 2930L, 3011L, 1199L, 1232L, 2521L, 4038L, 
3161L, 2421L, 3905L, 3399L, 3289L, 1995L, 2777L, 741L, 2232L, 
2217L, 3860L, 3895L, 1660L, 1042L, 2889L, 1184L, 2004L, 2365L, 
1935L, 4136L, 3253L, 1789L, 4170L, 2869L, 3800L, 3674L, 1865L, 
1837L, 3463L, 3367L, 1679L, 2704L, 3710L, 774L, 2256L, 3880L, 
1697L, 1324L, 1097L, 3938L, 1032L, 739L, 2208L, 2215L, 3840L, 
3893L, 3275L, 1637L, 1663L, 1040L, 2804L, 2827L, 1181L, 2318L, 
2357L, 1894L, 1925L, 2676L, 1318L, 1352L, 2278L, 2295L, 2792L, 
3765L, 3773L, 1538L, 1545L, 2730L, 3561L, 3630L, 1494L, 1565L, 
2653L, 2929L, 3010L, 1198L, 1231L, 2520L, 4037L, 3160L, 2420L, 
2479L, 3904L, 3398L, 3526L, 3288L, 1994L, 2776L, 745L, 2271L, 
2222L, 3901L, 3898L, 3278L, 3261L, 1639L, 1773L, 1050L, 2806L, 
2920L, 1187L, 3935L, 2855L, 3258L, 4169L, 2868L, 3799L, 3673L, 
1864L, 1836L, 3462L, 3366L, 1678L, 2703L, 2399L, 2371L, 2373L, 
2374L, 2372L, 2377L, 2678L, 1371L, 1372L, 1370L, 3496L, 1373L, 
4107L, 1591L, 3729L, 3660L, 3655L, 3657L, 2737L, 1592L, 3736L, 
3662L, 3659L, 3661L, 2738L, 1265L, 3059L, 3024L, 1855L, 3023L, 
1854L, 3025L, 2613L, 4106L, 3249L, 3272L, 2494L, 2505L, 4014L, 
3495L, 3494L, 3497L, 2093L, 2489L, 2796L, 2105L, 2932L, 3303L, 
2534L, 3298L, 3302L, 3308L, 3306L, 3313L, 3311L, 3314L, 3315L, 
3020L, 1201L, 3304L, 3299L, 3309L, 3301L, 3300L, 2422L, 2545L, 
2536L, 2531L, 2539L, 2543L, 2527L, 3636L, 2696L, 4043L, 1856L, 
4177L, 2884L, 3814L, 3689L, 1881L, 1849L, 3479L, 3382L, 1692L, 
2719L, 1084L, 2375L, 46L, 1114L, 54L, 1207L, 55L, 1208L, 60L, 
1262L, 72L, 1334L, 104L, 1547L, 105L, 1548L, 117L, 1586L, 100L, 
1532L, 102L, 1534L, 163L, 1709L, 162L, 1708L, 138L, 1647L, 188L, 
1763L, 133L, 1641L, 195L, 1796L, 226L, 1955L, 248L, 2043L, 249L, 
2044L, 247L, 2042L, 244L, 2039L, 243L, 2038L, 299L, 2266L, 271L, 
2197L, 314L, 2305L, 325L, 2346L, 332L, 2392L, 350L, 2458L, 351L, 
2459L, 355L, 2480L, 346L, 2454L, 364L, 2518L, 366L, 2558L, 367L, 
2559L, 381L, 2667L, 382L, 2668L, 387L, 2725L, 379L, 2665L, 457L, 
2908L, 458L, 2909L, 436L, 2846L, 435L, 2845L, 461L, 2925L, 463L, 
2974L, 487L, 3175L, 488L, 3176L, 489L, 3177L, 495L, 3242L, 554L, 
3327L, 555L, 3328L, 542L, 3282L, 545L, 3318L, 556L, 3329L, 579L, 
3591L, 580L, 3592L, 592L, 3713L, 577L, 3588L, 618L, 3833L, 658L, 
3890L, 611L, 3825L, 657L, 3889L, 640L, 3859L, 668L, 3947L, 661L, 
3917L, 676L, 3978L, 670L, 3952L, 673L, 3970L, 674L, 3971L, 675L, 
3972L, 682L, 4025L, 681L, 4024L, 695L, 4051L, 718L, 4155L, 721L, 
4159L, 731L, 4191L, 2090L, 3590L, 4016L, 4112L, 1111L, 3483L, 
32L, 779L, 36L, 1035L, 18L, 760L, 20L, 762L, 21L, 763L, 23L, 
764L, 19L, 761L, 9L, 751L, 12L, 754L, 13L, 755L, 14L, 756L, 11L, 
753L, 150L, 1658L, 155L, 1667L, 187L, 1762L, 137L, 1646L, 140L, 
1649L, 141L, 1650L, 143L, 1651L, 139L, 1648L, 282L, 2211L, 276L, 
2199L, 418L, 2821L, 424L, 2830L, 406L, 2810L, 408L, 2812L, 409L, 
2813L, 411L, 2814L, 407L, 2811L, 426L, 2832L, 428L, 2834L, 429L, 
2835L, 430L, 2836L, 427L, 2833L, 624L, 3838L, 627L, 3843L, 629L, 
3845L, 631L, 3847L, 632L, 3848L, 633L, 3849L, 630L, 3846L, 699L, 
4080L, 696L, 4052L, 701L, 4082L, 707L, 4114L, 1725L, 4180L, 1028L, 
4179L, 895L, 893L, 894L, 2131L, 1811L, 1718L, 1711L, 785L, 2112L, 
1527L, 1523L, 3863L, 3140L, 3143L, 3145L, 3141L, 3142L, 3135L, 
3139L, 3134L, 1492L, 1493L, 1309L, 2865L, 3796L, 3797L, 1705L, 
1025L, 1026L, 1027L, 3005L, 2600L, 3331L, 3066L, 1983L, 1984L, 
3178L, 1776L, 2911L, 1170L, 1884L, 4176L, 1877L, 1848L, 3474L, 
3378L, 1691L, 2715L, 3050L, 1739L, 2946L, 2956L, 2768L, 4172L, 
2874L, 3804L, 3678L, 1869L, 1841L, 3467L, 3371L, 1684L, 2708L, 
2943L, 2953L, 1404L, 1406L, 1473L, 1885L, 786L, 2473L, 3006L, 
4012L, 966L, 2672L, 3408L, 3409L, 1601L, 189L, 1770L, 1344L, 
1022L, 1326L, 1790L, 279L, 1023L, 2502L, 1024L, 2783L, 3974L, 
491L, 3064L, 3611L, 3739L, 423L, 437L, 1092L, 1756L, 1072L, 2885L, 
3815L, 2866L, 3672L, 1835L, 3365L, 446L, 603L, 589L, 202L, 200L, 
566L, 558L, 156L, 384L, 583L, 160L, 602L, 2882L, 3811L, 3687L, 
1876L, 1847L, 3473L, 3377L, 1690L, 2714L, 3619L, 1704L, 3785L, 
1134L, 1146L, 1139L, 1125L, 1115L, 1147L, 1150L, 1152L, 1127L, 
1126L, 1148L, 1149L, 1330L, 2009L, 2010L, 1142L, 1151L, 1137L, 
1136L, 1140L, 1122L, 1135L, 1124L, 1123L, 1518L, 1121L, 1120L, 
1517L, 2928L, 2927L, 1117L, 1119L, 1118L, 1116L, 1144L, 1145L, 
1138L, 1153L, 1143L, 1128L, 1325L, 1857L, 3868L, 2959L, 1781L, 
3642L, 1715L, 108L, 2234L, 1961L, 2650L, 1700L, 2751L, 2752L, 
3544L, 2750L, 3067L, 3549L, 2595L, 2599L, 1583L, 1167L, 1311L, 
3153L, 3074L, 2235L, 2896L, 3224L, 1085L, 1582L, 2938L, 2760L, 
2490L, 2493L, 2243L, 3867L, 2237L, 1522L, 1426L, 3643L, 1210L, 
3168L, 3073L, 3444L, 3704L, 3193L, 1172L, 1107L, 1425L, 1105L, 
1073L, 1104L, 1914L, 1106L, 2231L, 2753L, 1740L, 2757L, 2463L, 
1971L, 2468L, 1976L, 2625L, 2624L, 2758L, 2754L, 2759L, 2755L, 
2466L, 1974L, 2467L, 1975L, 2756L, 3063L, 3543L, 2761L, 2763L, 
3071L, 3540L, 3072L, 3551L, 2762L, 2764L, 3179L, 3542L, 3180L, 
3553L, 3541L, 3552L, 1384L, 3045L, 2597L, 1382L, 3707L, 1383L, 
3559L, 3558L, 3004L, 3226L, 1611L, 1476L, 1475L, 2464L, 1972L, 
1706L, 2115L, 1427L, 3068L, 2492L, 3196L, 1110L, 3070L, 2241L, 
2242L, 2238L, 2239L, 2893L, 1558L, 1391L, 1090L, 1091L, 1557L, 
1261L, 2862L, 3793L, 3669L, 1861L, 1832L, 3459L, 3362L, 1672L, 
2700L, 3615L, 1701L, 3782L, 3652L, 1878L, 1808L, 3475L, 3379L, 
1675L, 2716L, 3786L, 2878L, 3808L, 3682L, 1873L, 1844L, 3470L, 
3374L, 1687L, 2711L, 3617L, 1702L, 3783L, 3653L, 1879L, 1809L, 
3477L, 3380L, 1676L, 2717L, 3788L, 2879L, 3809L, 3683L, 1874L, 
1845L, 3471L, 3375L, 1688L, 2712L, 3618L, 1703L, 3784L, 3654L, 
1880L, 1810L, 3478L, 3381L, 1677L, 2718L, 3789L, 1100L, 1268L, 
1472L, 1617L, 1733L, 1883L, 1959L, 2116L, 2255L, 2300L, 2406L, 
2500L, 2616L, 2766L, 2891L, 3062L, 3115L, 3250L, 3503L, 3738L, 
3870L, 3946L, 4015L, 4029L, 4108L, 4195L, 16L, 53L, 74L, 101L, 
135L, 194L, 213L, 245L, 265L, 304L, 323L, 345L, 365L, 378L, 404L, 
462L, 476L, 484L, 551L, 576L, 609L, 660L, 671L, 680L, 691L, 717L, 
758L, 1206L, 1335L, 1533L, 1644L, 1795L, 1906L, 2040L, 2139L, 
2282L, 2344L, 2453L, 2556L, 2664L, 2808L, 2973L, 3108L, 3172L, 
3324L, 3587L, 3823L, 3916L, 3968L, 4023L, 4048L, 4154L, 510L, 
511L, 501L, 503L, 502L, 504L, 508L, 509L, 506L, 507L, 505L, 529L, 
517L, 537L, 538L, 525L, 515L, 514L, 518L, 536L, 535L, 524L, 521L, 
520L, 519L, 522L, 523L, 528L, 512L, 513L, 516L, 533L, 534L, 527L, 
531L, 532L, 526L, 540L, 539L, 530L, 3871L, 1590L, 1263L, 2471L, 
3263L, 2504L, 3395L, 3393L, 3396L, 1585L, 3394L, 1255L, 1812L, 
237L, 3998L, 3522L, 3508L, 3521L, 3518L, 3519L, 3520L, 3507L, 
235L, 1253L, 236L, 3996L, 1250L, 1813L, 1260L, 3746L, 4004L, 
1259L, 4003L, 1252L, 3995L, 3994L, 1251L, 3745L, 1241L, 3743L, 
3989L, 3988L, 1243L, 3991L, 3990L, 1242L, 3744L, 1240L, 3986L, 
3987L, 1827L, 2499L, 1381L, 3980L, 1612L, 1312L, 234L, 1239L, 
1386L, 1387L, 1310L, 2245L, 2246L, 3981L, 1249L, 1248L, 1257L, 
1258L, 2892L, 3979L, 2447L, 1256L, 3999L, 3612L, 4000L, 3055L, 
3057L, 3056L, 3054L, 4086L, 3484L, 3997L, 1254L, 2247L, 1424L, 
3550L, 1803L, 3924L, 1632L, 2548L, 2551L, 3500L, 3501L, 2048L, 
1571L, 1396L, 1397L, 3502L, 2046L, 2047L, 1570L, 1398L, 2114L, 
3116L, 2637L, 1682L, 2638L, 1209L, 2639L, 2640L, 1356L, 2863L, 
3794L, 3670L, 1862L, 1833L, 3460L, 3363L, 1673L, 2701L, 1141L, 
2180L, 2149L, 2171L, 1580L, 2298L, 2162L, 2148L, 2188L, 1086L, 
1088L, 1512L, 1514L, 1463L, 1466L, 3982L, 3984L, 1244L, 1246L, 
3060L, 1921L, 3730L, 3733L, 3992L, 3993L, 4001L, 4002L, 3961L, 
3138L, 3137L, 3061L, 2872L, 3802L, 3676L, 1867L, 1839L, 3465L, 
3369L, 1681L, 2706L, 1385L, 1161L, 1034L, 2257L, 2210L, 3883L, 
3842L, 1753L, 1666L, 2901L, 2829L, 2320L, 1896L, 2379L, 1938L, 
2411L, 1980L, 2348L, 1912L, 2397L, 1957L, 3290L, 4140L, 3439L, 
4188L, 3545L, 4198L, 3348L, 4163L, 3489L, 4193L, 3566L, 1496L, 
3691L, 1578L, 3779L, 3776L, 1621L, 3600L, 1554L, 3716L, 1593L, 
2655L, 2692L, 2771L, 2669L, 2733L, 1999L, 1200L, 2931L, 2079L, 
1234L, 3039L, 2119L, 1307L, 3079L, 2061L, 1215L, 2986L, 2094L, 
1266L, 3053L, 2524L, 2592L, 2629L, 2567L, 2608L, 4044L, 4039L, 
4125L, 4116L, 4100L, 4095L, 3162L, 3227L, 3266L, 3187L, 3246L, 
3958L, 3954L, 4005L, 3973L, 4009L, 2679L, 3949L, 3943L, 3355L, 
2263L, 3941L, 1162L, 1051L, 2258L, 2224L, 3884L, 3853L, 1754L, 
1695L, 2902L, 2840L, 2322L, 1897L, 2380L, 1939L, 2412L, 1985L, 
2349L, 1913L, 2400L, 1958L, 3291L, 4144L, 3440L, 4189L, 3546L, 
4199L, 3349L, 4164L, 3490L, 4194L, 3569L, 1497L, 3692L, 1579L, 
3780L, 3777L, 1622L, 3609L, 1556L, 3717L, 1594L, 2656L, 2693L, 
2772L, 2670L, 2734L, 2002L, 1202L, 2933L, 2080L, 1235L, 3040L, 
2121L, 1308L, 3080L, 2065L, 1220L, 2988L, 2095L, 1267L, 3058L, 
2546L, 2593L, 2630L, 2569L, 2610L, 4045L, 4040L, 4126L, 4118L, 
4101L, 4097L, 3163L, 3228L, 3267L, 3188L, 3247L, 3959L, 3955L, 
4006L, 3975L, 4010L, 2722L, 3950L, 2333L, 2352L, 3906L, 3932L, 
3923L, 3945L, 1607L, 3069L, 2264L, 3942L, 1205L, 2972L, 2554L, 
1794L, 1529L, 3579L, 2660L, 2450L, 1902L, 2340L, 2037L, 2280L, 
3107L, 4022L, 4182L, 1354L, 3401L, 3169L, 4152L, 1331L, 3316L, 
749L, 2801L, 1634L, 1665L, 1037L, 1668L, 1179L, 2910L, 1082L, 
1716L, 1083L, 1722L, 1741L, 2136L, 3820L, 2267L, 2388L, 3530L, 
2390L, 2687L, 2684L, 2686L, 3697L, 3534L, 3213L, 3211L, 3214L, 
3218L, 3220L, 3221L, 3217L, 3210L, 2587L, 3032L, 3532L, 3035L, 
3451L, 3533L, 2205L, 1378L, 3527L, 1368L, 2369L, 3650L, 3017L, 
2077L, 1053L, 783L, 4042L, 4036L, 1727L, 1698L, 4070L, 4067L, 
2842L, 3957L, 3953L, 2823L, 4099L, 3856L, 3977L, 3976L, 4007L, 
4120L, 1769L, 4085L, 2226L, 2007L, 3531L, 2691L, 2690L, 2688L, 
3212L, 3222L, 3219L, 3215L, 3223L, 2590L, 2591L, 2588L, 2326L, 
3031L, 3038L, 3034L, 3036L, 3029L, 3037L, 2328L, 2329L, 3450L, 
3452L, 3455L, 3454L, 3448L, 2936L, 3529L, 4076L, 4078L, 4077L, 
2327L, 3528L, 4071L, 4105L, 4104L, 4096L, 4129L, 4128L, 4117L, 
1109L, 1108L, 2389L, 2689L, 3698L, 3216L, 2589L, 3033L, 3453L, 
2206L, 1379L, 1369L, 2370L, 3651L, 3018L, 2078L, 2386L, 2683L, 
3695L, 3208L, 2585L, 3028L, 3447L, 2203L, 1376L, 1366L, 2367L, 
3648L, 3015L, 2075L, 1380L, 2873L, 3803L, 3677L, 1868L, 1840L, 
3466L, 3370L, 1683L, 2707L, 3616L, 2169L, 2159L, 2186L, 2187L, 
2168L, 2153L, 2184L, 2182L, 2160L, 2179L, 2170L, 2181L, 2158L, 
2151L, 2164L, 2174L, 2146L, 2183L, 2185L, 2154L, 2175L, 2145L, 
2156L, 2178L, 2173L, 2387L, 2685L, 3696L, 3209L, 2586L, 3030L, 
3449L, 2204L, 1377L, 1367L, 2368L, 3649L, 3016L, 2076L, 2385L, 
2682L, 3694L, 3207L, 2584L, 3027L, 3446L, 2202L, 1375L, 1365L, 
2366L, 3647L, 3014L, 2074L, 2404L, 2190L, 2144L, 2193L, 2194L, 
2189L, 2143L, 2192L, 2191L, 2157L, 2163L, 2177L, 2155L, 2172L, 
2152L, 2161L, 2147L, 2166L, 2165L, 2176L, 2167L, 1099L, 742L, 
2244L, 2330L, 2384L, 2382L, 2383L, 1986L, 2402L, 3357L, 3358L, 
1602L, 3728L, 2001L, 2923L, 1238L, 2120L, 2067L, 2069L, 2979L, 
2110L, 2549L, 2602L, 2601L, 2566L, 4032L, 3241L, 3960L, 2064L, 
3555L, 3568L, 2568L, 1469L, 2922L, 2659L, 2623L, 2553L, 2334L, 
311L, 361L, 205L, 1320L, 2341L, 3007L, 2673L, 2626L, 2628L, 3514L, 
3509L, 256L, 2378L, 2581L, 1937L, 3690L, 2632L, 2604L, 1587L, 
2393L, 1853L, 2727L, 2634L, 3516L, 3506L, 3510L, 2607L, 1401L, 
3517L, 2396L, 2605L, 1399L, 2557L, 2394L, 2614L, 2615L, 2962L, 
2407L, 2617L, 1960L, 3157L, 3155L, 3156L, 3078L, 2767L, 2641L, 
2618L, 3081L, 2788L, 2642L, 2645L, 2414L, 2644L, 3082L, 2789L, 
2643L, 2647L, 2415L, 2646L, 2398L, 2609L, 1081L, 1269L, 3505L, 
1340L, 1471L, 1470L, 1530L, 1987L, 2011L, 238L, 312L, 3511L, 
2409L, 2488L, 3512L, 3513L, 2510L, 2555L, 3515L, 2612L, 3052L, 
3523L, 3554L, 3967L, 3787L, 1609L, 340L, 2477L, 1413L, 833L, 
834L, 885L, 890L, 891L, 24L, 68L, 111L, 112L, 113L, 228L, 254L, 
362L, 1478L, 1479L, 1511L, 1480L, 1481L, 1520L, 1573L, 1576L, 
1462L, 3183L, 3741L, 2867L, 3271L, 3686L, 1345L, 1348L, 1416L, 
1421L, 1596L, 1600L, 2127L, 2130L, 2999L, 3002L, 1171L, 1305L, 
1618L, 1757L, 2262L, 2503L, 2619L, 2905L, 3262L, 3535L, 3762L, 
62L, 69L, 90L, 119L, 255L, 358L, 396L, 419L, 493L, 546L, 590L, 
716L, 1780L, 1598L, 1080L, 4174L, 2877L, 3807L, 3681L, 1872L, 
1843L, 3469L, 3373L, 1686L, 2710L, 3132L, 229L, 48L, 63L, 93L, 
121L, 181L, 203L, 230L, 253L, 296L, 309L, 337L, 359L, 370L, 390L, 
453L, 474L, 478L, 497L, 569L, 601L, 653L, 667L, 678L, 686L, 706L, 
733L, 1778L, 1347L, 114L, 363L, 25L, 78L, 3129L, 35L, 7L, 22L, 
50L, 31L, 47L, 5L, 73L, 153L, 130L, 142L, 147L, 281L, 262L, 267L, 
273L, 186L, 392L, 421L, 398L, 410L, 459L, 417L, 452L, 626L, 606L, 
612L, 623L, 689L, 588L, 694L, 2533L, 3307L, 3305L, 3312L, 3310L, 
2535L, 2532L, 2530L, 2529L, 2540L, 2538L, 2537L, 2544L, 2542L, 
2541L, 2528L, 2526L, 2525L, 3637L, 3635L, 3634L, 2695L, 3154L, 
1154L, 1133L, 3182L, 1461L, 3740L, 2947L, 2942L, 2941L, 1289L, 
1287L, 1286L, 1276L, 1273L, 1272L, 1270L, 2240L, 2957L, 2952L, 
2951L, 1294L, 1292L, 1291L, 1283L, 1280L, 1279L, 1103L, 1804L, 
1807L, 1850L, 1805L, 1806L, 999L, 1613L, 1614L, 4094L, 1191L, 
996L, 3430L, 3431L, 997L, 3432L, 3433L, 3424L, 3425L, 3426L, 
3427L, 1070L, 1071L, 1056L, 1225L, 1226L, 1942L, 1943L, 1500L, 
1501L, 2049L, 2050L, 1003L, 3908L, 3909L, 3910L, 4149L, 4150L, 
3622L, 3623L, 4090L, 4091L, 1815L, 1816L, 2311L, 2312L, 2435L, 
2436L, 2572L, 2573L, 2785L, 2786L, 3295L, 3296L, 2980L, 2977L, 
2978L, 3749L, 3750L, 3112L, 3113L, 3191L, 3422L, 3423L, 3574L, 
3575L, 3576L, 998L, 3912L, 3913L, 1228L, 2317L, 2990L, 1063L, 
2984L, 2985L, 2987L, 3768L, 3769L, 3770L, 3920L, 3921L, 3922L, 
3584L, 3585L, 3586L, 1541L, 2292L, 3255L, 1891L, 1892L, 1893L, 
2742L, 2058L, 2057L, 2054L, 2059L, 2062L, 4055L, 1218L, 3602L, 
3598L, 3607L, 2565L, 2745L, 2748L, 4064L, 3389L, 3392L, 3390L, 
3388L, 3391L, 1219L, 3608L, 2749L, 4065L, 1217L, 3601L, 3597L, 
3606L, 2441L, 2439L, 2442L, 2446L, 2564L, 2744L, 2747L, 4063L, 
2940L, 2950L, 2445L, 2443L, 3293L, 3798L, 1712L, 1719L, 3865L, 
3962L, 2948L, 2958L, 1277L, 1284L, 3732L, 3735L, 1245L, 1247L, 
1513L, 1515L, 1087L, 1089L, 1465L, 1468L, 3983L, 3985L, 2914L, 
2912L, 2916L, 2915L, 2498L, 2496L, 3866L, 1420L, 3001L, 3354L, 
1407L, 2945L, 2955L, 1275L, 1282L, 3731L, 3734L, 2780L, 1169L, 
3049L, 2129L, 2469L, 1977L, 1738L, 1599L, 2994L, 1176L, 1065L, 
1059L, 3966L, 1061L, 4058L, 4059L, 4060L, 1057L, 1213L, 1214L, 
1216L, 3604L, 3596L, 3599L, 3605L, 3639L, 3640L, 3641L, 2288L, 
2289L, 2290L, 1997L, 1998L, 2000L, 2360L, 2361L, 2362L, 1502L, 
3633L, 3186L, 4143L, 3335L, 3336L, 3337L, 3404L, 3405L, 3406L, 
3285L, 3286L, 3287L, 1487L, 1488L, 1489L, 3564L, 3565L, 3567L, 
4138L, 4139L, 4141L, 1044L, 1045L, 1046L, 1928L, 1929L, 1930L, 
1799L, 1800L, 1801L, 3087L, 3088L, 3089L, 2313L, 2315L, 2316L, 
2438L, 2440L, 2444L, 2561L, 2562L, 2563L, 2740L, 2743L, 2746L, 
2055L, 2056L, 2060L, 2063L, 3964L, 1067L, 4056L, 1068L, 4061L, 
1069L, 4062L, 2430L, 2429L, 2425L, 2424L, 2427L, 2426L, 2428L, 
2423L, 4178L, 1779L, 3136L, 2779L, 1597L, 2993L, 1079L, 3147L, 
2944L, 2954L, 1168L, 3048L, 1417L, 2128L, 3000L, 3481L, 4173L, 
2875L, 3805L, 3679L, 1870L, 1842L, 3468L, 3372L, 1685L, 2709L, 
1405L, 3353L, 2465L, 1737L, 1973L, 3131L, 1175L, 42L, 61L, 91L, 
118L, 165L, 201L, 227L, 250L, 288L, 308L, 333L, 356L, 369L, 388L, 
445L, 471L, 477L, 496L, 567L, 593L, 641L, 665L, 677L, 685L, 705L, 
732L, 1288L, 1196L, 1293L, 1156L, 3864L, 1968L, 1077L, 1264L, 
1400L, 1589L, 1713L, 1852L, 1956L, 2091L, 2233L, 2299L, 2395L, 
2487L, 2606L, 2726L, 2858L, 3051L, 3110L, 3245L, 3485L, 3714L, 
3861L, 3939L, 4008L, 4028L, 4088L, 4192L, 1274L, 1204L, 1281L, 
1158L, 2998L, 1464L, 1467L, 2150L, 2582L, 4011L, 1163L, 2259L, 
3885L, 1755L, 2903L, 4046L, 4127L, 4102L, 3781L, 2338L, 1052L, 
2225L, 3854L, 1696L, 2841L, 2323L, 2381L, 2413L, 2350L, 2401L, 
3292L, 3441L, 3547L, 3350L, 3491L, 3570L, 3693L, 3778L, 3610L, 
3718L, 2657L, 2694L, 2773L, 2671L, 2735L, 2003L, 2081L, 2122L, 
2066L, 2096L, 2547L, 2594L, 2631L, 2570L, 2611L, 4041L, 4119L, 
4098L, 3164L, 3229L, 3268L, 3189L, 3248L, 3956L, 2723L, 3944L, 
3356L, 1346L, 3537L, 2517L, 4069L, 4013L), class = "data.frame")


SIGTERM <- 15L


package.dependencies <- function (x, check = FALSE, depLevel = c("Depends", "Imports", 
    "Suggests")) 
{
    depLevel <- match.arg(depLevel)
    .Deprecated("package_dependencies")
    if (!is.matrix(x)) 
        x <- matrix(x, nrow = 1L, dimnames = list(NULL, names(x)))
    deps <- list()
    for (k in 1L:nrow(x)) {
        z <- x[k, depLevel]
        if (!is.na(z) & z != "") {
            z <- unlist(strsplit(z, ",", fixed = TRUE))
            z <- sub("^[[:space:]]*(.*)", "\\1", z)
            z <- sub("(.*)[[:space:]]*$", "\\1", z)
            pat <- "^([^\\([:space:]]+)[[:space:]]*\\(([^\\)]+)\\).*"
            deps[[k]] <- cbind(sub(pat, "\\1", z), sub(pat, "\\2", 
                z), NA)
            noversion <- deps[[k]][, 1] == deps[[k]][, 2]
            deps[[k]][noversion, 2] <- NA
            pat <- "[[:space:]]*([[<>=]+)[[:space:]]+(.*)"
            deps[[k]][!noversion, 2:3] <- c(sub(pat, "\\1", deps[[k]][!noversion, 
                2]), sub(pat, "\\2", deps[[k]][!noversion, 2]))
        }
        else deps[[k]] <- NA
    }
    if (check) {
        z <- rep.int(TRUE, nrow(x))
        for (k in 1L:nrow(x)) {
            if (!is.na(deps[[k]]) && any(ok <- deps[[k]][, 1] == 
                "R")) {
                if (!is.na(deps[[k]][ok, 2]) && deps[[k]][ok, 
                  2] %in% c("<=", ">=")) {
                  op <- deps[[k]][ok, 2]
                  x1 <- rep.int(0, 6)
                  y <- c(R.version$major, strsplit(R.version$minor, 
                    ".", fixed = TRUE)[[1L]])
                  x1[seq_along(y)] <- y
                  y <- strsplit(deps[[k]][ok, 3], ".", fixed = TRUE)[[1L]]
                  x1[3 + seq_along(y)] <- y
                  x1 <- format(x1, justify = "right")
                  x2 <- paste(x1[4:6], collapse = ".")
                  x1 <- paste(x1[1L:3], collapse = ".")
                  comptext <- paste0("'", x1, "' ", op, " '", 
                    x2, "'")
                  compres <- try(eval(parse(text = comptext)))
                  if (!inherits(compres, "try-error")) {
                    z[k] <- compres
                  }
                }
            }
        }
        names(z) <- x[, "Package"]
        return(z)
    }
    else {
        names(deps) <- x[, "Package"]
        return(deps)
    }
}


checkPoFiles <- function (language, dir = ".") 
{
    files <- list.files(path = dir, pattern = paste0(language, 
        "[.]po$"), full.names = TRUE, recursive = TRUE)
    result <- matrix(character(), ncol = 5L, nrow = 0L)
    for (f in files) {
        errs <- checkPoFile(f, strictPlural = startsWith(basename(f), 
            "R-"))
        if (nrow(errs)) 
            result <- rbind(result, errs)
    }
    structure(result, class = "check_po_files")
}


assertCondition <- function (expr, ..., .exprString = .deparseTrim(substitute(expr), 
    cutoff = 30L), verbose = FALSE) 
{
    getConds <- function(expr) {
        conds <- list()
        tryCatch(withCallingHandlers(expr, warning = function(w) {
            conds <<- c(conds, list(w))
            invokeRestart("muffleWarning")
        }, condition = function(cond) conds <<- c(conds, list(cond))), 
            error = function(e) conds <<- c(conds, list(e)))
        conds
    }
    conds <- if (nargs() > 1) 
        c(...)
    .Wanted <- if (nargs() > 1) 
        paste(c(...), collapse = " or ")
    else "any condition"
    res <- getConds(expr)
    if (length(res)) {
        if (is.null(conds)) {
            if (verbose) 
                message("assertConditon: Successfully caught a condition\n")
            invisible(res)
        }
        else {
            ii <- sapply(res, function(cond) any(class(cond) %in% 
                conds))
            if (any(ii)) {
                if (verbose) {
                  found <- unique(sapply(res, function(cond) class(cond)[class(cond) %in% 
                    conds]))
                  message(sprintf("assertCondition: caught %s", 
                    paste(dQuote(found), collapse = ", ")), domain = NA)
                }
                invisible(res)
            }
            else {
                .got <- paste(unique((sapply(res, function(obj) class(obj)[[1]]))), 
                  collapse = ", ")
                stop(gettextf("Got %s in evaluating %s; wanted %s", 
                  .got, .exprString, .Wanted))
            }
        }
    }
    else stop(gettextf("Failed to get %s in evaluating %s", .Wanted, 
        .exprString))
}


update_pkg_po <- function (pkgdir, pkg = NULL, version = NULL, copyright, bugs) 
{
    same <- function(a, b) {
        tmpa <- readLines(a)
        tmpb <- readLines(b)
        tmpa <- grep("^\"POT-Creation-Date:", tmpa, invert = TRUE, 
            value = TRUE)
        tmpb <- grep("^\"POT-Creation-Date:", tmpb, invert = TRUE, 
            value = TRUE)
        identical(tmpa, tmpb)
    }
    pwd <- getwd()
    coll <- Sys.getlocale("LC_COLLATE")
    on.exit({
        Sys.setlocale("LC_COLLATE", coll)
        setwd(pwd)
    })
    Sys.setlocale("LC_COLLATE", "C")
    setwd(pkgdir)
    dir.create("po", FALSE)
    files <- dir("po")
    desc <- "DESCRIPTION"
    if (file.exists(desc)) {
        desc <- read.dcf(desc, fields = c("Package", "Version"))
        pkg <- name <- desc[1L]
        version <- desc[2L]
        if (missing(copyright)) 
            copyright <- NULL
        if (missing(bugs)) 
            bugs <- NULL
        stem <- file.path("inst", "po")
    }
    else {
        pkg <- basename(pkgdir)
        name <- "R"
        version <- as.character(getRversion())
        copyright <- "The R Core Team"
        bugs <- "bugs.r-project.org"
        stem <- file.path("..", "translations", "inst")
    }
    is_base <- (pkg == "base")
    have_src <- paste0(pkg, ".pot") %in% files
    ofile <- tempfile()
    xgettext2pot(".", ofile, name, version, bugs)
    potfile <- file.path("po", paste0("R-", pkg, ".pot"))
    if (file.exists(potfile) && same(potfile, ofile)) {
    }
    else file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "R-.*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/R-en@quot.po"]
    for (f in pofiles) {
        lang <- sub("^R-(.*)[.]po$", "\\1", basename(f))
        message("  R-", lang, ":", appendLF = FALSE, domain = NA)
        cmd <- paste("msgmerge --update", f, shQuote(potfile))
        if (system(cmd) != 0L) {
            warning("running msgmerge on ", sQuote(f), " failed", 
                domain = NA)
            next
        }
        res <- checkPoFile(f, TRUE)
        if (nrow(res)) {
            print(res)
            message("not installing", domain = NA)
            next
        }
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), 
            shQuote(f))
        if (system(cmd) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), 
                domain = NA, immediate. = TRUE)
    }
    if (l10n_info()[["UTF-8"]]) {
        lang <- "en@quot"
        message("  R-", lang, ":", domain = NA)
        f <- tempfile()
        en_quote(potfile, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("R-%s.mo", pkg))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), 
            shQuote(f))
        if (system(cmd) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), 
                domain = NA, immediate. = TRUE)
    }
    if (!(is_base || have_src)) 
        return(invisible())
    ofile <- tempfile()
    if (!is_base) {
        dom <- pkg
        od <- setwd("src")
        exts <- "[.](c|cc|cpp|m|mm)$"
        cfiles <- dir(".", pattern = exts)
        if (file.exists("windows")) 
            cfiles <- c(cfiles, dir("windows", pattern = exts, 
                full.names = TRUE))
    }
    else {
        dom <- "R"
        od <- setwd("../../..")
        cfiles <- grep("^#", readLines("po/POTFILES"), value = TRUE, 
            invert = TRUE)
    }
    cmd <- sprintf("xgettext --keyword=_ --keyword=N_ -o %s", 
        shQuote(ofile))
    cmd <- c(cmd, paste("--package-name", name, sep = "="), paste("--package-version", 
        version, sep = "="), "--add-comments=TRANSLATORS:", if (!is.null(copyright)) sprintf("--copyright-holder=\"%s\"", 
        copyright), if (!is.null(bugs)) sprintf("--msgid-bugs-address=\"%s\"", 
        bugs), if (is_base) "-C")
    cmd <- paste(c(cmd, cfiles), collapse = " ")
    if (system(cmd) != 0L) 
        stop("running xgettext failed", domain = NA)
    setwd(od)
    potfile <- file.path("po", paste0(dom, ".pot"))
    if (!same(potfile, ofile)) 
        file.copy(ofile, potfile, overwrite = TRUE)
    pofiles <- dir("po", pattern = "^[^R].*[.]po$", full.names = TRUE)
    pofiles <- pofiles[pofiles != "po/en@quot.po"]
    for (f in pofiles) {
        lang <- sub("[.]po", "", basename(f))
        message("  ", lang, ":", appendLF = FALSE, domain = NA)
        cmd <- paste("msgmerge --update", shQuote(f), shQuote(potfile))
        if (system(cmd) != 0L) {
            warning("running msgmerge on ", f, " failed", domain = NA)
            next
        }
        res <- checkPoFile(f, TRUE)
        if (nrow(res)) {
            print(res)
            message("not installing", domain = NA)
            next
        }
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", dom))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), 
            shQuote(f))
        if (system(cmd) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), 
                domain = NA)
    }
    if (l10n_info()[["UTF-8"]]) {
        lang <- "en@quot"
        message("  ", lang, ":", domain = NA)
        f <- tempfile()
        en_quote(potfile, f)
        dest <- file.path(stem, lang, "LC_MESSAGES")
        dir.create(dest, FALSE, TRUE)
        dest <- file.path(dest, sprintf("%s.mo", dom))
        cmd <- paste("msgfmt -c --statistics -o", shQuote(dest), 
            shQuote(f))
        if (system(cmd) != 0L) 
            warning(sprintf("running msgfmt on %s failed", basename(f)), 
                domain = NA)
    }
    invisible()
}


buildVignettes <- function (package, dir, lib.loc = NULL, quiet = TRUE, clean = TRUE, 
    tangle = FALSE) 
{
    vigns <- pkgVignettes(package = package, dir = dir, lib.loc = lib.loc, 
        check = TRUE)
    if (is.null(vigns)) 
        return(invisible())
    if (length(vigns$msg)) 
        warning(paste(vigns$msg, collapse = "\n"), domain = NA)
    dups <- duplicated(vigns$names)
    if (any(dups)) {
        names <- unique(vigns$names[dups])
        docs <- sort(basename(vigns$docs[vigns$names %in% names]))
        stop(gettextf("Detected vignette source files (%s) with shared names (%s) and therefore risking overwriting each others output files", 
            paste(sQuote(docs), collapse = ", "), paste(sQuote(names), 
                collapse = ", ")), domain = NA)
    }
    Sys.unsetenv("SWEAVE_STYLEPATH_DEFAULT")
    op <- options(warn = 1)
    wd <- getwd()
    if (is.null(wd)) 
        stop("current working directory cannot be ascertained")
    on.exit({
        setwd(wd)
        options(op)
    })
    setwd(vigns$dir)
    origfiles <- list.files(all.files = TRUE)
    have.makefile <- "Makefile" %in% origfiles
    WINDOWS <- .Platform$OS.type == "windows"
    file.create(".build.timestamp")
    loadVignetteBuilder(vigns$pkgdir)
    outputs <- NULL
    sourceList <- list()
    startdir <- getwd()
    for (i in seq_along(vigns$docs)) {
        file <- basename(vigns$docs[i])
        name <- vigns$names[i]
        engine <- vignetteEngine(vigns$engine[i])
        enc <- vigns$encodings[i]
        if (enc == "non-ASCII") 
            stop(gettextf("Vignette '%s' is non-ASCII but has no declared encoding", 
                file), domain = NA, call. = FALSE)
        output <- tryCatch({
            engine$weave(file, quiet = quiet, encoding = enc)
            setwd(startdir)
            find_vignette_product(name, by = "weave", engine = engine)
        }, error = function(e) {
            stop(gettextf("processing vignette '%s' failed with diagnostics:\n%s", 
                file, conditionMessage(e)), domain = NA, call. = FALSE)
        })
        if (!have.makefile && vignette_is_tex(output)) {
            texi2pdf(file = output, clean = FALSE, quiet = quiet)
            output <- find_vignette_product(name, by = "texi2pdf", 
                engine = engine)
        }
        outputs <- c(outputs, output)
        if (tangle) {
            output <- tryCatch({
                engine$tangle(file, quiet = quiet, encoding = enc)
                setwd(startdir)
                find_vignette_product(name, by = "tangle", main = FALSE, 
                  engine = engine)
            }, error = function(e) {
                stop(gettextf("tangling vignette '%s' failed with diagnostics:\n%s", 
                  file, conditionMessage(e)), domain = NA, call. = FALSE)
            })
            sourceList[[file]] <- output
        }
    }
    if (have.makefile) {
        if (WINDOWS) {
            rhome <- chartr("\\", "/", R.home())
            Sys.setenv(R_HOME = rhome)
        }
        make <- Sys.getenv("MAKE", "make")
        if (!nzchar(make)) 
            make <- "make"
        yy <- system(make)
        if (yy > 0) 
            stop("running 'make' failed")
        if (clean && any(startsWith(readLines("Makefile", warn = FALSE), 
            "clean:"))) 
            system(paste(make, "clean"))
    }
    else {
        grDevices::graphics.off()
        keep <- c(outputs, unlist(sourceList))
        if (clean) {
            f <- setdiff(list.files(all.files = TRUE, no.. = TRUE), 
                keep)
            newer <- file_test("-nt", f, ".build.timestamp")
            unlink(f[newer], recursive = TRUE)
            f <- setdiff(list.files(all.files = TRUE, no.. = TRUE), 
                c(keep, origfiles))
            f <- f[file_test("-f", f)]
            file.remove(f)
        }
    }
    stopifnot(length(outputs) == length(vigns$docs))
    vigns$outputs <- outputs
    vigns$sources <- sourceList
    if (file.exists(".build.timestamp")) 
        file.remove(".build.timestamp")
    invisible(vigns)
}


list_files_with_type <- function (dir, type, all.files = FALSE, full.names = TRUE, OS_subdirs = .OStype()) 
{
    exts <- .make_file_exts(type)
    files <- list_files_with_exts(dir, exts, all.files = all.files, 
        full.names = full.names)
    if (type %in% c("code", "docs")) {
        for (os in OS_subdirs) {
            os_dir <- file.path(dir, os)
            if (dir.exists(os_dir)) {
                os_files <- list_files_with_exts(os_dir, exts, 
                  all.files = all.files, full.names = FALSE)
                os_files <- file.path(if (full.names) 
                  os_dir
                else os, os_files)
                files <- c(files, os_files)
            }
        }
    }
    if (type %in% c("code", "docs")) {
        files <- files[grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789]", 
            basename(files))]
    }
    if (type %in% "demo") {
        files <- files[grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]", 
            basename(files))]
    }
    files
}


getDepList <- function (depMtrx, instPkgs, recursive = TRUE, local = TRUE, 
    reduce = TRUE, lib.loc = NULL) 
{
    .Deprecated("dependsOnPkgs() or package_dependencies()")
    if (!missing(lib.loc)) 
        warning("the 'lib.loc' argument has always been unused and is deprecated now")
    out <- structure(list(Depends = character(), Installed = character(), 
        Found = list(), NotFound = character(), R = character()), 
        class = "DependsList")
    if (!is.matrix(depMtrx) && is.na(depMtrx)) 
        return(out)
    mtrxList <- buildDepList(depMtrx, instPkgs, recursive)
    if (!local) {
        toFind <- mtrxList$Depends[!apply(mtrxList$Depends, 1L, 
            isSatisfied, mtrxList$Installed), , drop = FALSE]
        if (reduce) 
            toFind <- reduceDepends(toFind)
        if (length(toFind)) {
            found <- foundDepends(toFind)
            out$Found <- found$Found
            mtrxList$NotFound <- found$NotFound
        }
    }
    if (reduce) {
        mtrxList$R <- reduceDepends(mtrxList$R)
        mtrxList$Depends <- reduceDepends(mtrxList$Depends)
        mtrxList$Installed <- reduceDepends(mtrxList$Installed)
    }
    out$R <- depMtrxToStrings(mtrxList$R)
    out$Depends <- depMtrxToStrings(mtrxList$Depends)
    out$Installed <- depMtrxToStrings(mtrxList$Installed)
    out$NotFound <- depMtrxToStrings(mtrxList$NotFound)
    out
}


pkgVignettes <- function (package, dir, subdirs = NULL, lib.loc = NULL, output = FALSE, 
    source = FALSE, check = FALSE) 
{
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
    }
    if (missing(dir)) 
        stop("you must specify 'package' or 'dir'")
    if (!dir.exists(dir)) 
        stop(gettextf("directory '%s' does not exist", dir), 
            domain = NA)
    else {
        dir <- file_path_as_absolute(dir)
        if (is.null(subdirs)) 
            subdirs <- if (missing(package)) 
                "vignettes"
            else "doc"
        for (subdir in subdirs) {
            docdir <- file.path(dir, subdir)
            if (dir.exists(docdir)) 
                break
        }
    }
    if (!dir.exists(docdir)) 
        return(NULL)
    buildPkgs <- loadVignetteBuilder(dir, mustwork = FALSE)
    engineList <- vignetteEngine(package = buildPkgs)
    docs <- names <- engines <- patterns <- character()
    allFiles <- list.files(docdir, all.files = FALSE, full.names = TRUE)
    matchedPattern <- rep.int(FALSE, length(allFiles))
    msg <- character()
    if (length(allFiles) > 0L) {
        for (name in names(engineList)) {
            engine <- engineList[[name]]
            for (pattern in engine$pattern) {
                idxs <- grep(pattern, allFiles)
                matchedPattern[idxs] <- TRUE
                keep <- vapply(allFiles[idxs], function(.d.) engineMatches(name, 
                  getVignetteEngine(.d.)), NA)
                if (any(keep)) {
                  idxs <- idxs[keep]
                  if (is.function(engine$weave)) {
                    docsT <- allFiles[idxs]
                    docs <- c(docs, docsT)
                    names <- c(names, gsub(pattern, "", basename(docsT)))
                    engines <- c(engines, rep.int(name, length(idxs)))
                    patterns <- c(patterns, rep.int(pattern, 
                      length(idxs)))
                  }
                  matchedPattern <- matchedPattern[-idxs]
                  allFiles <- allFiles[-idxs]
                  if (length(allFiles) == 0L) 
                    break
                }
            }
        }
        if (check && any(matchedPattern)) {
            files <- substring(allFiles[matchedPattern], nchar(dir) + 
                2)
            msg <- c("Files named as vignettes but with no recognized vignette engine:", 
                paste("  ", sQuote(files)), "(Is a VignetteBuilder field missing?)")
        }
    }
    stopifnot(length(names) == length(docs), length(engines) == 
        length(docs), length(patterns) == length(docs), !anyDuplicated(docs))
    defaultEncoding <- .get_package_metadata(dir)["Encoding"]
    encodings <- vapply(docs, getVignetteEncoding, "", default = defaultEncoding)
    z <- list(docs = docs, names = names, engines = engines, 
        patterns = patterns, encodings = encodings, dir = docdir, 
        pkgdir = dir, msg = msg)
    if (output) {
        outputs <- character(length(docs))
        for (i in seq_along(docs)) {
            file <- docs[i]
            name <- names[i]
            outputI <- find_vignette_product(name, by = "weave", 
                dir = docdir, engine = engine)
            outputs[i] <- outputI
        }
        z$outputs <- outputs
    }
    if (source) {
        sources <- list()
        for (i in seq_along(docs)) {
            file <- docs[i]
            name <- names[i]
            sourcesI <- find_vignette_product(name, by = "tangle", 
                main = FALSE, dir = docdir, engine = engine)
            sources[[file]] <- sourcesI
        }
        z$sources <- sources
    }
    class(z) <- "pkgVignettes"
    z
}


findHTMLlinks <- function (pkgDir = "", lib.loc = NULL, level = 0:2) 
{
    if (is.null(lib.loc)) 
        lib.loc <- .libPaths()
    Links <- list()
    if (2 %in% level) 
        Links <- c(Links, lapply(rev(lib.loc), .find_HTML_links_in_library))
    if (1 %in% level) {
        base <- unlist(.get_standard_package_names()[c("base", 
            "recommended")], use.names = FALSE)
        Links <- c(Links, lapply(file.path(.Library, base), .find_HTML_links_in_package))
    }
    if (0 %in% level && nzchar(pkgDir)) 
        Links <- c(Links, list(.find_HTML_links_in_package(pkgDir)))
    Links <- unlist(Links)
    Links <- rev(Links)
    Links <- Links[!duplicated(names(Links))]
    gsub("[Rr]d$", "html", Links)
}


Rdiff <- function (from, to, useDiff = FALSE, forEx = FALSE, nullPointers = TRUE, 
    Log = FALSE) 
{
    clean <- function(txt) {
        if (!length(txt)) 
            return(txt)
        if (length(top <- grep("^(R version|R : Copyright|R Under development)", 
            txt, perl = TRUE, useBytes = TRUE)) && length(bot <- grep("quit R.$", 
            txt, perl = TRUE, useBytes = TRUE))) 
            txt <- txt[-(top[1L]:bot[1L])]
        ll <- grep("</HEADER>", txt, fixed = TRUE, useBytes = TRUE)
        if (length(ll)) 
            txt <- txt[-seq_len(max(ll))]
        ll <- grep("<FOOTER>", txt, fixed = TRUE, useBytes = TRUE)
        if (length(ll)) 
            txt <- txt[seq_len(max(ll) - 1L)]
        nl <- length(txt)
        if (nl > 3L && startsWith(txt[nl - 2L], "> proc.time()")) 
            txt <- txt[1:(nl - 3L)]
        if (nullPointers) 
            txt <- gsub("<(environment|bytecode|pointer|promise): [x[:xdigit:]]+>", 
                "<\\1: 0>", txt)
        txt <- .canonicalize_quotes(txt)
        if (.Platform$OS.type == "windows") {
            txt <- gsub("(\x91|\x92)", "'", txt, perl = TRUE, 
                useBytes = TRUE)
            txt <- gsub("(\x93|\x94)", "\"", txt, perl = TRUE, 
                useBytes = TRUE)
            txt <- txt[!grepl("options(pager = \"console\")", 
                txt, fixed = TRUE, useBytes = TRUE)]
        }
        pat <- "(^Time |^Loading required package|^Package [A-Za-z][A-Za-z0-9]+ loaded|^<(environment|promise|pointer|bytecode):|^/CreationDate |^/ModDate |^/Producer |^End.Don't show)"
        txt[!grepl(pat, txt, perl = TRUE, useBytes = TRUE)]
    }
    clean2 <- function(txt) {
        eoh <- grep("^> options\\(warn = 1\\)$", txt)
        if (length(eoh)) 
            txt[-(1L:eoh[1L])]
        else txt
    }
    left <- clean(readLines(from))
    right <- clean(readLines(to))
    if (forEx) {
        left <- clean2(left)
        left <- grep("[.](format_|)ptime", left, value = TRUE, 
            invert = TRUE, useBytes = TRUE)
        right <- clean2(right)
    }
    if (!useDiff && (length(left) == length(right))) {
        bleft <- gsub("[[:space:]]*$", "", left)
        bright <- gsub("[[:space:]]*$", "", right)
        bleft <- gsub("[[:space:]]+", " ", bleft)
        bright <- gsub("[[:space:]]+", " ", bright)
        if (all(bleft == bright)) 
            return(if (Log) list(status = 0L, out = character()) else 0L)
        cat("\n")
        diff <- bleft != bright
        for (i in which(diff)) cat(i, "c", i, "\n< ", left[i], 
            "\n", "---\n> ", right[i], "\n", sep = "")
        if (Log) {
            i <- which(diff)
            out <- paste0(i, "c", i, "\n< ", left[i], "\n", "---\n> ", 
                right[i])
            list(status = 1L, out = out)
        }
        else 1L
    }
    else {
        out <- character()
        if (!useDiff) {
            cat("\nfiles differ in number of lines:\n")
            out <- "files differ in number of lines"
        }
        a <- tempfile("Rdiffa")
        writeLines(left, a)
        b <- tempfile("Rdiffb")
        writeLines(right, b)
        if (Log) {
            tf <- tempfile()
            status <- system2("diff", c("-bw", shQuote(a), shQuote(b)), 
                stdout = tf, stderr = tf)
            list(status = status, out = c(out, readLines(tf)))
        }
        else system(paste("diff -bw", shQuote(a), shQuote(b)))
    }
}


SIGQUIT <- 3L


file_path_sans_ext <- function (x, compression = FALSE) 
{
    if (compression) 
        x <- sub("[.](gz|bz2|xz)$", "", x)
    sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}


xngettext <- function (dir, verbose = FALSE) 
{
    dir <- file_path_as_absolute(dir)
    dir <- file.path(dir, "R")
    exts <- .make_file_exts("code")
    R_files <- list_files_with_exts(dir, exts)
    for (d in c("unix", "windows", "aqua")) {
        OSdir <- file.path(dir, d)
        if (dir.exists(OSdir)) 
            R_files <- c(R_files, list_files_with_exts(OSdir, 
                exts))
    }
    out <- vector("list", length = length(R_files))
    names(out) <- R_files
    find_strings <- function(e) {
        if (is.call(e) && is.name(e[[1L]]) && as.character(e[[1L]]) %in% 
            "ngettext") {
            e <- match.call(ngettext, e)
            domain <- e[["domain"]]
            suppress <- !is.null(domain) && !is.name(domain) && 
                is.na(domain)
            if (!suppress && is.character(e[["msg1"]]) && is.character(e[["msg2"]])) 
                strings <<- c(strings, list(c(msg1 = e[["msg1"]], 
                  msg2 = e[["msg2"]])))
        }
        else if (is.recursive(e)) 
            for (i in seq_along(e)) Recall(e[[i]])
    }
    for (f in R_files) {
        if (verbose) 
            message(gettextf("parsing '%s'", f), domain = NA)
        strings <- list()
        for (e in parse(file = f)) find_strings(e)
        out[[f]] <- structure(strings, class = "xngettext")
    }
    out[lengths(out) > 0L]
}


checkMD5sums <- function (package, dir) 
{
    if (missing(dir)) 
        dir <- find.package(package, quiet = TRUE)
    if (!length(dir)) 
        return(NA)
    md5file <- file.path(dir, "MD5")
    if (!file.exists(md5file)) 
        return(NA)
    inlines <- readLines(md5file)
    xx <- sub("^([0-9a-fA-F]*)(.*)", "\\1", inlines)
    nmxx <- names(xx) <- sub("^[0-9a-fA-F]* [ |*](.*)", "\\1", 
        inlines)
    dot <- getwd()
    if (is.null(dot)) 
        stop("current working directory cannot be ascertained")
    setwd(dir)
    x <- md5sum(dir(dir, recursive = TRUE))
    setwd(dot)
    x <- x[names(x) != "MD5"]
    nmx <- names(x)
    res <- TRUE
    not.here <- !(nmxx %in% nmx)
    if (any(not.here)) {
        res <- FALSE
        if (sum(not.here) > 1L) 
            cat("files", paste(sQuote(nmxx[not.here]), collapse = ", "), 
                "are missing\n", sep = " ")
        else cat("file", sQuote(nmxx[not.here]), "is missing\n", 
            sep = " ")
    }
    nmxx <- nmxx[!not.here]
    diff <- xx[nmxx] != x[nmxx]
    if (any(diff)) {
        res <- FALSE
        files <- nmxx[diff]
        if (length(files) > 1L) 
            cat("files", paste(sQuote(files), collapse = ", "), 
                "have the wrong MD5 checksums\n", sep = " ")
        else cat("file", sQuote(files), "has the wrong MD5 checksum\n")
    }
    res
}


deparseLatex <- function (x, dropBraces = FALSE) 
{
    result <- character()
    lastTag <- "TEXT"
    for (i in seq_along(x)) {
        a <- x[[i]]
        tag <- attr(a, "latex_tag")
        if (is.null(tag)) 
            tag <- "NULL"
        switch(tag, VERB = , TEXT = , MACRO = , COMMENT = result <- c(result, 
            a), BLOCK = result <- c(result, if (dropBraces && 
            lastTag == "TEXT") deparseLatex(a) else c("{", deparseLatex(a), 
            "}")), ENVIRONMENT = result <- c(result, "\\begin{", 
            a[[1L]], "}", deparseLatex(a[[2L]]), "\\end{", a[[1L]], 
            "}"), MATH = result <- c(result, "$", deparseLatex(a), 
            "$"), `NULL` = stop("Internal error, no tag", domain = NA))
        lastTag <- tag
    }
    paste(result, collapse = "")
}


codocClasses <- function (package, lib.loc = NULL) 
{
    bad_Rd_objects <- structure(NULL, class = "codocClasses")
    if (length(package) != 1L) 
        stop("argument 'package' must be of length 1")
    dir <- find.package(package, lib.loc)
    if (!dir.exists(file.path(dir, "R"))) 
        stop(gettextf("directory '%s' does not contain R code", 
            dir), domain = NA)
    if (!.haveRds(dir)) 
        stop(gettextf("directory '%s' does not contain Rd objects", 
            dir), domain = NA)
    is_base <- basename(dir) == "base"
    if (!is_base) 
        .load_package_quietly(package, lib.loc)
    code_env <- .package_env(package)
    if (!.isMethodsDispatchOn()) 
        return(bad_Rd_objects)
    S4_classes <- methods::getClasses(code_env)
    if (!length(S4_classes)) 
        return(bad_Rd_objects)
    sApply <- function(X, FUN, ...) unlist(lapply(X = X, FUN = FUN, 
        ...), recursive = FALSE, use.names = FALSE)
    db <- Rd_db(package, lib.loc = dirname(dir))
    idx <- sApply(lapply(db, .Rd_get_doc_type), identical, "class")
    if (!any(idx)) 
        return(bad_Rd_objects)
    db <- db[idx]
    stats <- c(n.S4classes = length(S4_classes), n.db = length(db))
    aliases <- lapply(db, .Rd_get_metadata, "alias")
    named_class <- lapply(aliases, endsWith, suffix = "-class")
    nClass <- sApply(named_class, sum)
    oneAlias <- lengths(aliases, use.names = FALSE) == 1L
    idx <- oneAlias | nClass == 1L
    if (!any(idx)) 
        return(bad_Rd_objects)
    db <- db[idx]
    stats["n.cl"] <- length(db)
    multi <- idx & !oneAlias
    aliases[multi] <- mapply(`[`, aliases[multi], named_class[multi], 
        SIMPLIFY = FALSE, USE.NAMES = FALSE)
    aliases <- unlist(aliases[idx], use.names = FALSE)
    Rd_slots <- lapply(db, .Rd_get_section, "Slots", FALSE)
    idx <- sapply(Rd_slots, length) > 0L
    if (!any(idx)) 
        return(bad_Rd_objects)
    db <- db[idx]
    aliases <- aliases[idx]
    Rd_slots <- Rd_slots[idx]
    stats["n.final"] <- length(db)
    db_names <- .Rd_get_names_from_Rd_db(db)
    .get_slot_names <- function(x) {
        x <- .Rd_get_section(x, "describe")
        txt <- .Rd_get_item_tags(x)
        if (!length(txt)) 
            return(character())
        txt <- gsub("\\\\l?dots", "...", txt)
        txt <- gsub("\\\\code\\{([^}]*)\\}:?", "\\1", as.character(txt))
        txt <- unlist(strsplit(txt, ", *"))
        trimws(txt)
    }
    .inheritedSlotNames <- function(ext) {
        supcl <- methods::.selectSuperClasses(ext)
        unique(unlist(lapply(lapply(supcl, methods::getClassDef), 
            methods::slotNames), use.names = FALSE))
    }
    S4topics <- sApply(S4_classes, utils:::topicName, type = "class")
    S4_checked <- S4_classes[has.a <- S4topics %in% aliases]
    idx <- match(S4topics[has.a], aliases)
    for (icl in seq_along(S4_checked)) {
        cl <- S4_checked[icl]
        cld <- methods::getClass(cl, where = code_env)
        ii <- idx[icl]
        scld <- methods::slotNames(cld)
        codeSlots <- if (!is.null(scld)) 
            sort(scld)
        else character()
        docSlots <- sort(.get_slot_names(Rd_slots[[ii]]))
        superSlots <- .inheritedSlotNames(cld@contains)
        if (length(superSlots)) 
            docSlots <- docSlots[is.na(match(docSlots, c("...", 
                "\\dots")))]
        if (!all(docSlots %in% codeSlots) || !all(setdiff(codeSlots, 
            superSlots) %in% docSlots)) {
            bad_Rd_objects[[db_names[ii]]] <- list(name = cl, 
                code = codeSlots, inherited = superSlots, docs = docSlots)
        }
    }
    attr(bad_Rd_objects, "S4_classes_checked") <- S4_checked
    attr(bad_Rd_objects, "stats") <- stats
    bad_Rd_objects
}


makevars_site <- function () 
{
    m <- character()
    if (is.na(f <- Sys.getenv("R_MAKEVARS_SITE", NA_character_))) 
        f <- file.path(paste0(R.home("etc"), Sys.getenv("R_ARCH")), 
            "Makevars.site")
    if (file.exists(f)) 
        m <- f
    m
}


bibstyle <- function (style, envir, ..., .init = FALSE, .default = TRUE) 
{
    newfns <- list(...)
    if (missing(style) || is.null(style)) {
        if (!missing(envir) || length(newfns) || .init) 
            stop("Changes require specified 'style'")
        style <- default
    }
    else {
        if (!missing(envir)) {
            stopifnot(!.init)
            styles[[style]] <<- envir
        }
        if (.init) 
            styles[[style]] <<- makeJSS()
        if (length(newfns) && style == "JSS") 
            stop("The default JSS style may not be modified.")
        for (n in names(newfns)) assign(n, newfns[[n]], envir = styles[[style]])
        if (.default) 
            default <<- style
    }
    styles[[style]]
}


delimMatch <- function (x, delim = c("{", "}"), syntax = "Rd") 
{
    if (!is.character(x)) 
        stop("argument 'x' must be a character vector")
    if ((length(delim) != 2L) || any(nchar(delim) != 1L)) 
        stop("argument 'delim' must specify two characters")
    if (syntax != "Rd") 
        stop("only Rd syntax is currently supported")
    .Call(delim_match, x, delim)
}


package_dependencies <- function (packages = NULL, db = NULL, which = c("Depends", "Imports", 
    "LinkingTo"), recursive = FALSE, reverse = FALSE, verbose = getOption("verbose")) 
{
    if (is.null(db)) 
        db <- utils::available.packages()
    out_of_db_packages <- character()
    if (!recursive && !reverse) {
        if (!is.null(packages)) {
            ind <- match(packages, db[, "Package"], nomatch = 0L)
            db <- db[ind, , drop = FALSE]
            out_of_db_packages <- packages[ind == 0L]
        }
    }
    if (identical(which, "all")) 
        which <- c("Depends", "Imports", "LinkingTo", "Suggests", 
            "Enhances")
    else if (identical(which, "most")) 
        which <- c("Depends", "Imports", "LinkingTo", "Suggests")
    depends <- do.call(Map, c(list("c"), lapply(which, function(f) {
        if (is.list(d <- db[, f])) d else lapply(d, .extract_dependency_package_names)
    }), list(USE.NAMES = FALSE)))
    depends <- lapply(depends, unique)
    if (!recursive && !reverse) {
        names(depends) <- db[, "Package"]
        if (length(out_of_db_packages)) {
            depends <- c(depends, structure(vector("list", length(out_of_db_packages)), 
                names = out_of_db_packages))
        }
        return(depends)
    }
    all_packages <- sort(unique(c(db[, "Package"], unlist(depends))))
    if (!recursive) {
        depends <- split(rep.int(db[, "Package"], lengths(depends)), 
            factor(unlist(depends), levels = all_packages))
        if (!is.null(packages)) {
            depends <- depends[match(packages, names(depends))]
            names(depends) <- packages
        }
        return(depends)
    }
    matchP <- match(rep.int(db[, "Package"], lengths(depends)), 
        all_packages)
    matchD <- match(unlist(depends), all_packages)
    tab <- if (reverse) 
        split(matchP, factor(matchD, levels = seq_along(all_packages)))
    else split(matchD, factor(matchP, levels = seq_along(all_packages)))
    if (is.null(packages)) {
        if (reverse) {
            packages <- all_packages
            p_L <- seq_along(all_packages)
        }
        else {
            packages <- db[, "Package"]
            p_L <- match(packages, all_packages)
        }
    }
    else {
        p_L <- match(packages, all_packages, nomatch = 0L)
        if (any(ind <- (p_L == 0L))) {
            out_of_db_packages <- packages[ind]
            packages <- packages[!ind]
            p_L <- p_L[!ind]
        }
    }
    p_R <- tab[p_L]
    pos <- cbind(rep.int(p_L, lengths(p_R)), unlist(p_R))
    ctr <- 0L
    repeat {
        if (verbose) 
            cat("Cycle:", (ctr <- ctr + 1L))
        p_L <- split(pos[, 1L], pos[, 2L])
        new <- do.call(rbind, Map(function(i, k) cbind(rep.int(i, 
            length(k)), rep(k, each = length(i))), p_L, tab[as.integer(names(p_L))]))
        npos <- unique(rbind(pos, new))
        nnew <- nrow(npos) - nrow(pos)
        if (verbose) 
            cat(" NNew:", nnew, "\n")
        if (!nnew) 
            break
        pos <- npos
    }
    depends <- split(all_packages[pos[, 2L]], factor(all_packages[pos[, 
        1L]], levels = unique(packages)))
    if (length(out_of_db_packages)) {
        depends <- c(depends, structure(vector("list", length(out_of_db_packages)), 
            names = out_of_db_packages))
    }
    depends
}


checkRd <- function (Rd, defines = .Platform$OS.type, stages = "render", 
    unknownOK = TRUE, listOK = TRUE, ..., def_enc = FALSE) 
{
    warnRd <- function(block, Rdfile, ..., level = 0) {
        srcref <- attr(block, "srcref")
        msg <- if (is.null(srcref)) 
            paste0("file '", Rdfile, "': ", ...)
        else {
            loc <- paste0(Rdfile, ":", srcref[1L])
            if (srcref[1L] != srcref[3L]) 
                loc <- paste0(loc, "-", srcref[3L])
            paste0(loc, ": ", ...)
        }
        msg <- sprintf("checkRd: (%d) %s", level, msg)
        .messages <<- c(.messages, msg)
    }
    checkLink <- function(tag, block) {
        option <- attr(block, "Rd_option")
        if (!is.null(option)) 
            checkContent(option, tag)
        checkContent(block, tag)
        get_link(block, tag, Rdfile)
    }
    checkBlock <- function(block, tag, blocktag) {
        switch(tag, UNKNOWN = if (!unknownOK) stopRd(block, Rdfile, 
            "Unrecognized macro ", block[[1L]]), VERB = , RCODE = , 
            TEXT = {
                if (!def_enc) {
                  msg2 <- if (inEnc2) "in second part of \\enc" else "without declared encoding"
                  if (Encoding(block) == "UTF-8") warnRd(block, 
                    Rdfile, level = -1, "Non-ASCII contents ", 
                    msg2)
                  if (grepl("<[0123456789abcdef][0123456789abcdef]>", 
                    block)) warnRd(block, Rdfile, level = -3, 
                    "Apparent non-ASCII contents ", msg2)
                }
                if (!grepl("^[[:space:]]*$", block)) has_text <<- TRUE
            }, USERMACRO = , `\\newcommand` = , `\\renewcommand` = , 
            COMMENT = {
            }, LIST = if (length(block)) {
                deparse <- sQuote(paste(as.character.Rd(block), 
                  collapse = ""))
                if (!listOK) stopRd(block, Rdfile, "Unnecessary braces at ", 
                  deparse) else warnRd(block, Rdfile, level = -3, 
                  "Unnecessary braces at ", deparse)
                checkContent(block, tag)
            }, `\\describe` = , `\\enumerate` = , `\\itemize` = , 
            `\\bold` = , `\\cite` = , `\\command` = , `\\dfn` = , 
            `\\emph` = , `\\kbd` = checkContent(block, tag), 
            `\\code` = , `\\preformatted` = checkCodeBlock(block, 
                tag), `\\Sexpr` = , `\\special` = , `\\strong` = , 
            `\\var` = , `\\verb` = checkContent(block, tag), 
            `\\linkS4class` = , `\\link` = checkLink(tag, block), 
            `\\email` = , `\\url` = has_text <<- TRUE, `\\cr` = {
            }, `\\dots` = , `\\ldots` = , `\\R` = has_text <<- TRUE, 
            `\\acronym` = , `\\env` = , `\\file` = , `\\option` = , 
            `\\pkg` = , `\\samp` = , `\\sQuote` = , `\\dQuote` = checkContent(block, 
                tag), `\\method` = , `\\S3method` = , `\\S4method` = warnRd(block, 
                Rdfile, level = 7, "Tag ", tag, " not valid outside a code block"), 
            `\\enc` = {
                checkContent(block[[1L]], tag)
                save_enc <- def_enc
                def_enc <<- FALSE
                inEnc2 <<- TRUE
                checkContent(block[[2L]], tag)
                def_enc <<- save_enc
                inEnc2 <<- FALSE
            }, `\\eqn` = , `\\deqn` = , `\\figure` = {
                checkContent(block[[1L]])
                if (length(block) > 1L) checkContent(block[[2L]])
            }, `\\tabular` = checkTabular(block), `\\subsection` = checkSection(block, 
                tag), `\\if` = , `\\ifelse` = {
                condition <- block[[1L]]
                tags <- RdTags(condition)
                if (!all(tags %in% c("TEXT", "\\Sexpr"))) stopRd(block, 
                  Rdfile, "Condition must be \\Sexpr or plain text")
                condition <- condition[tags == "TEXT"]
                allow <- trimws(strsplit(paste(condition, collapse = ""), 
                  ",")[[1L]])
                unknown <- allow[!(allow %in% c("", "latex", 
                  "example", "text", "html", "TRUE", "FALSE"))]
                if (length(unknown)) warnRd(block, Rdfile, "Unrecognized format: ", 
                  unknown)
                checkContent(block[[2L]])
                if (tag == "\\ifelse") checkContent(block[[3L]])
            }, `\\href` = {
                if (!identical(RdTags(block[[1L]]), "VERB")) stopRd(block, 
                  Rdfile, "First argument to \\href must be verbatim URL")
                checkContent(block[[2L]], tag)
            }, `\\out` = {
                tags <- RdTags(block)
                if (!all(tags == "VERB")) stopRd(block, Rdfile, 
                  "Must contain verbatim text")
            }, warnRd(block, Rdfile, level = 7, "Tag ", tag, 
                " not recognized"))
    }
    checkCodeBlock <- function(blocks, blocktag) {
        for (block in blocks) {
            tag <- attr(block, "Rd_tag")
            switch(tag, UNKNOWN = if (!unknownOK) stopRd(block, 
                Rdfile, "Unrecognized macro ", block[[1L]]), 
                VERB = , RCODE = , TEXT = {
                  if (!def_enc) {
                    msg2 <- if (inEnc2) "in second part of \\enc" else "without declared encoding"
                    if (Encoding(block) == "UTF-8") warnRd(block, 
                      Rdfile, level = -1, "Non-ASCII contents ", 
                      msg2)
                    if (grepl("<[0123456789abcdef][0123456789abcdef]>", 
                      block)) warnRd(block, Rdfile, level = -3, 
                      "Apparent non-ASCII contents ", msg2)
                  }
                  if (!grepl("^[[:space:]]*$", block)) has_text <<- TRUE
                }, USERMACRO = , `\\newcommand` = , `\\renewcommand` = , 
                COMMENT = {
                }, `\\var` = checkCodeBlock(block, blocktag), 
                `\\special` = checkCodeBlock(block, blocktag), 
                `\\dots` = has_text <<- TRUE, `\\ldots` = {
                  warnRd(block, Rdfile, level = -3, "Tag ", tag, 
                    " is invalid in a code block")
                  has_text <<- TRUE
                }, `\\linkS4class` = , `\\link` = checkLink(tag, 
                  block), `\\method` = , `\\S3method` = , `\\S4method` = if (blocktag == 
                  "\\usage") {
                  checkContent(block[[1L]], tag)
                  checkContent(block[[2L]], tag)
                } else warnRd(block, Rdfile, level = 7, "Tag ", 
                  tag, " is only valid in \\usage"), `\\dontrun` = , 
                `\\donttest` = , `\\dontshow` = , `\\testonly` = if (blocktag == 
                  "\\examples") checkCodeBlock(block, blocktag) else warnRd(block, 
                  Rdfile, level = 7, "Tag ", tag, " is only valid in \\examples"), 
                {
                  warnRd(block, Rdfile, level = 7, "Tag ", tag, 
                    " is invalid in a ", blocktag, " block")
                  has_text <<- TRUE
                })
        }
    }
    checkTabular <- function(table) {
        has_text <<- TRUE
        format <- table[[1L]]
        content <- table[[2L]]
        if (length(format) != 1 || RdTags(format) != "TEXT") 
            warnRd(table, Rdfile, level = 7, "\\tabular format must be simple text")
        format <- strsplit(format[[1L]], "", fixed = TRUE)[[1L]]
        if (!all(format %in% c("l", "c", "r"))) 
            warnRd(table, Rdfile, level = 7, "Unrecognized \\tabular format: ", 
                table[[1L]][[1L]])
        tags <- RdTags(content)
        newrow <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
                newrow <- FALSE
                col <- 0
                newcol <- TRUE
            }
            if (newcol) {
                col <- col + 1
                if (col > length(format)) 
                  warnRd(table, Rdfile, level = 7, "Only ", length(format), 
                    " columns allowed in this table")
                newcol <- FALSE
            }
            switch(tags[i], `\\tab` = {
                newcol <- TRUE
            }, `\\cr` = {
                newrow <- TRUE
            }, checkBlock(content[[i]], tags[i], "\\tabular"))
        }
    }
    checkContent <- function(blocks, blocktag) {
        inlist <- FALSE
        tags <- RdTags(blocks)
        for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag, `\\item` = {
                if (!inlist) inlist <- TRUE
                switch(blocktag, `\\arguments` = {
                  checkContent(block[[1L]], tag)
                  checkContent(block[[2L]], tag)
                }, `\\value` = , `\\describe` = {
                  checkContent(block[[1L]], tag)
                  checkContent(block[[2L]], tag)
                }, `\\enumerate` = , `\\itemize` = {
                })
            }, {
                if (inlist && !(blocktag %in% c("\\itemize", 
                  "\\enumerate")) && !(tag == "TEXT" && isBlankRd(block))) {
                  inlist <- FALSE
                }
                checkBlock(block, tag, blocktag)
            })
        }
    }
    has_text <- FALSE
    checkSection <- function(section, tag) {
        if (tag == "\\section" || tag == "\\subsection") {
            title <- section[[1L]]
            checkContent(title, tag)
            section <- section[[2L]]
            tagtitle <- sQuote(as.character(title))
        }
        else tagtitle <- tag
        has_text <<- FALSE
        if (tag == "\\synopsis") 
            stopRd(section, Rdfile, "\\synopsis was removed in R 3.1.0")
        if (tag %in% c("\\usage", "\\examples")) 
            checkCodeBlock(section, tag)
        else checkContent(section, tag)
        if (!has_text) 
            warnRd(section, Rdfile, level = 3, "Empty section ", 
                tagtitle)
    }
    checkUnique <- function(tag) {
        which <- which(sections == tag)
        if (length(which) < 1L) 
            warnRd(Rd, Rdfile, level = 5, "Must have a ", tag)
        else {
            if (length(which) > 1L) 
                warnRd(Rd[[which[2L]]], Rdfile, level = 5, "Only one ", 
                  tag, " is allowed")
            empty <- TRUE
            for (block in Rd[which]) {
                switch(attr(block, "Rd_tag"), TEXT = if (!grepl("^[[:space:]]*$", 
                  block)) empty <- FALSE, empty <- FALSE)
            }
            if (empty) 
                warnRd(Rd[[which[1L]]], Rdfile, level = 5, "Tag ", 
                  tag, " must not be empty")
        }
    }
    dt <- which(RdTags(Rd) == "\\docType")
    docTypes <- character(length(dt))
    if (length(dt)) {
        for (i in dt) {
            docType <- Rd[[i]]
            if (!identical(RdTags(docType), "TEXT")) 
                warnRd(docType, Rdfile, level = 7, "'docType' must be plain text")
            docTypes[i] <- sub("^ *", "", sub(" *$", "", docType[[1L]]))
        }
    }
    .messages <- character()
    .whandler <- function(e) {
        .messages <<- c(.messages, paste("prepare_Rd:", conditionMessage(e)))
        invokeRestart("muffleWarning")
    }
    Rd <- withCallingHandlers({
        prepare_Rd(Rd, defines = defines, stages = stages, warningCalls = FALSE, 
            ..., msglevel = 1)
    }, warning = .whandler)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)
    enc <- which(sections == "\\encoding")
    if (length(enc)) 
        def_enc <- TRUE
    inEnc2 <- FALSE
    if (!identical("package", docTypes)) 
        checkUnique("\\description")
    unique_tags <- paste("\\", c("name", "title", "usage", "arguments", 
        "synopsis", "format", "details", "value", "references", 
        "source", "seealso", "examples", "author", "encoding"), 
        sep = "")
    for (tag in intersect(sections[duplicated(sections)], unique_tags)) warnRd(Rd, 
        Rdfile, level = 5, sprintf("multiple sections named '%s' are not allowed", 
            tag))
    for (i in seq_along(sections)) checkSection(Rd[[i]], sections[i])
    structure(.messages, class = "checkRd")
}


Rd2txt_options <- function (...) 
{
    args <- list(...)
    if (!length(args)) 
        return(opts)
    else {
        if (is.list(args[[1L]])) 
            args <- args[[1L]]
        result <- opts[names(args)]
        opts[names(args)] <<- args
        invisible(result)
    }
}


latexToUtf8 <- function (x) 
{
    i <- 0L
    whitespace <- c(" ", "\t", "\n")
    while (i < length(x)) {
        i <- i + 1L
        a <- x[[i]]
        tag <- attr(a, "latex_tag")
        if (tag == "MACRO") {
            numargs <- latexArgCount[a]
            if (!is.na(numargs)) {
                args <- vector("list", numargs)
                j <- i
                getNext <- TRUE
                k <- 1L
                while (k <= numargs) {
                  if (getNext) {
                    j <- j + 1L
                    if (j > length(x)) {
                      warning("argument for ", c(a), " not found", 
                        domain = NA)
                      nextobj <- latex_tag("", "TEXT")
                      nexttag <- "TEXT"
                      nextchars <- ""
                    }
                    else {
                      nextobj <- x[[j]]
                      nexttag <- attr(nextobj, "latex_tag")
                      if (nexttag == "TEXT") 
                        nextchars <- strsplit(nextobj, "")[[1L]]
                    }
                    getNext <- FALSE
                  }
                  switch(nexttag, TEXT = {
                    args[[k]] <- latex_tag(nextchars[1L], "TEXT")
                    nextchars <- nextchars[-1L]
                    if (!length(nextchars)) getNext <- TRUE
                    if (args[[k]] %in% whitespace) next
                    k <- k + 1L
                  }, COMMENT = getNext <- TRUE, BLOCK = , ENVIRONMENT = , 
                    MATH = {
                      args[[k]] <- latexToUtf8(nextobj)
                      k <- k + 1L
                      getNext <- TRUE
                    }, `NULL` = stop("Internal error:  NULL tag", 
                      domain = NA))
                }
                index <- a
                for (i1 in seq_along(args)) {
                  if (is.null(latexTable[[index]])) 
                    break
                  nextobj1 <- args[[i1]]
                  nexttag1 <- attr(nextobj1, "latex_tag")
                  index <- c(index, switch(nexttag1, MACRO = , 
                    TEXT = nextobj1, BLOCK = deparseLatex(nextobj1, 
                      dropBraces = TRUE)))
                }
                subst <- latex_tag(latexTable[[index]], "TEXT")
                if (!is.null(subst) && !is.list(subst)) {
                  x[[i]] <- subst
                  if (numargs) {
                    if (nexttag == "TEXT" && length(nextchars)) {
                      nextobj[1L] <- paste(nextchars, collapse = "")
                      x[[j]] <- nextobj
                      j <- j - 1L
                    }
                    while (j > i) {
                      x[[j]] <- NULL
                      j <- j - 1L
                    }
                  }
                }
                else i <- j
            }
        }
        else if (tag == "BLOCK") 
            x[[i]] <- latexToUtf8(a)
    }
    x
}


xgettext2pot <- function (dir, potFile, name = "R", version, bugs) 
{
    dir <- file_path_as_absolute(dir)
    if (missing(potFile)) 
        potFile <- paste0("R-", basename(dir), ".pot")
    tmp <- unique(unlist(xgettext(dir, asCall = FALSE)))
    tmp <- tmp[nzchar(tmp)]
    if (length(tmp) > 0L) 
        tmp <- shQuote(encodeString(tmp), type = "cmd")
    con <- file(potFile, "wt")
    on.exit(close(con))
    if (missing(version)) 
        version <- paste(R.version$major, R.version$minor, sep = ".")
    if (missing(bugs)) 
        bugs <- "bugs.r-project.org"
    writeLines(con = con, c("msgid \"\"", "msgstr \"\"", sprintf("\"Project-Id-Version: %s %s\\n\"", 
        name, version), sprintf("\"Report-Msgid-Bugs-To: %s\\n\"", 
        bugs), paste0("\"POT-Creation-Date: ", format(Sys.time(), 
        "%Y-%m-%d %H:%M"), "\\n\""), "\"PO-Revision-Date: YEAR-MO-DA HO:MI+ZONE\\n\"", 
        "\"Last-Translator: FULL NAME <EMAIL@ADDRESS>\\n\"", 
        "\"Language-Team: LANGUAGE <LL@li.org>\\n\"", "\"MIME-Version: 1.0\\n\"", 
        "\"Content-Type: text/plain; charset=CHARSET\\n\"", "\"Content-Transfer-Encoding: 8bit\\n\"", 
        ""))
    for (e in tmp) writeLines(con = con, c("", paste("msgid", 
        e), "msgstr \"\""))
    tmp <- xngettext(dir)
    un <- unique(unlist(tmp, recursive = TRUE))
    for (ee in tmp) for (e in ee) if (e[1L] %in% un) {
        writeLines(con = con, c("", paste("msgid       ", shQuote(encodeString(e[1L]), 
            type = "cmd")), paste("msgid_plural", shQuote(encodeString(e[2L]), 
            type = "cmd")), "msgstr[0]    \"\"", "msgstr[1]    \"\""))
        un <- un[-match(e, un)]
    }
}


nonS3methods <- function (package) 
{
    stopList <- list(base = c("all.equal", "all.names", "all.vars", 
        "expand.grid", "format.char", "format.info", "format.pval", 
        "max.col", "print.atomic", "print.coefmat", "qr.Q", "qr.R", 
        "qr.X", "qr.coef", "qr.fitted", "qr.qty", "qr.qy", "qr.resid", 
        "qr.solve", "rep.int", "seq.int", "sort.int", "sort.list"), 
        AMORE = "sim.MLPnet", BSDA = "sign.test", ChemometricsWithR = "lda.loofun", 
        ElectoGraph = "plot.wedding.cake", FrF2 = "all.2fis.clear.catlg", 
        GLDEX = c("hist.su", "pretty.su"), Hmisc = c("abs.error.pred", 
            "all.digits", "all.is.numeric", "format.df", "format.pval", 
            "t.test.cluster"), HyperbolicDist = "log.hist", MASS = c("frequency.polygon", 
            "gamma.dispersion", "gamma.shape", "hist.FD", "hist.scott"), 
        LinearizedSVR = "sigma.est", Matrix = c("qr.Q", "qr.R", 
            "qr.coef", "qr.fitted", "qr.qty", "qr.qy", "qr.resid"), 
        RCurl = "merge.list", RNetCDF = c("close.nc", "dim.def.nc", 
            "dim.inq.nc", "dim.rename.nc", "open.nc", "print.nc"), 
        Rmpfr = c("mpfr.is.0", "mpfr.is.integer"), SMPracticals = "exp.gibbs", 
        TANOVA = "sigma.hat", TeachingDemos = "sigma.test", XML = "text.SAX", 
        ape = "sort.index", arm = "sigma.hat", assist = "chol.new", 
        boot = "exp.tilt", car = "scatterplot.matrix", calibrator = "t.fun", 
        clusterfly = "ggobi.som", coda = "as.mcmc.list", crossdes = "all.combn", 
        ctv = "update.views", deSolve = "plot.1D", effects = "all.effects", 
        elliptic = "sigma.laurent", equivalence = "sign.boot", 
        fields = c("qr.q2ty", "qr.yq2"), gbm = c("pretty.gbm.tree", 
            "quantile.rug"), gpclib = "scale.poly", grDevices = "boxplot.stats", 
        graphics = c("close.screen", "plot.design", "plot.new", 
            "plot.window", "plot.xy", "split.screen"), ic.infer = "all.R2", 
        hier.part = "all.regs", lasso2 = "qr.rtr.inv", latticeExtra = "xyplot.list", 
        locfit = c("density.lf", "plot.eval"), moments = c("all.cumulants", 
            "all.moments"), mosaic = "t.test", mratios = c("t.test.ration", 
            "t.test.ratio.default", "t.test.ratio.formula"), 
        ncdf = c("open.ncdf", "close.ncdf", "dim.create.ncdf", 
            "dim.def.ncdf", "dim.inq.ncdf", "dim.same.ncdf"), 
        quadprog = c("solve.QP", "solve.QP.compact"), reposTools = "update.packages2", 
        rgeos = "scale.poly", sac = "cumsum.test", sm = "print.graph", 
        splusTimeDate = "sort.list", splusTimeSeries = "sort.list", 
        stats = c("anova.lmlist", "expand.model.frame", "fitted.values", 
            "influence.measures", "lag.plot", "t.test", "plot.spec.phase", 
            "plot.spec.coherency"), stremo = "sigma.hat", supclust = c("sign.change", 
            "sign.flip"), tensorA = "chol.tensor", utils = c("close.socket", 
            "flush.console", "update.packages"))
    if (is.null(package)) 
        return(unlist(stopList))
    thisPkg <- stopList[[package]]
    if (!length(thisPkg)) 
        character()
    else thisPkg
}


checkFF <- function (package, dir, file, lib.loc = NULL, registration = FALSE, 
    check_DUP = FALSE, verbose = getOption("verbose")) 
{
    allow_suppress <- !nzchar(Sys.getenv("_R_CHECK_FF_AS_CRAN_"))
    suppressCheck <- function(e) allow_suppress && length(e) == 
        2L && is.call(e) && is.symbol(e[[1L]]) && as.character(e[[1L]]) == 
        "dontCheck"
    has_namespace <- FALSE
    is_installed_msg <- is_installed <- FALSE
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        dfile <- file.path(dir, "DESCRIPTION")
        db <- .read_description(dfile)
        pkg <- pkgDLL <- basename(dir)
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        have_registration <- FALSE
        if (basename(dir) != "base") {
            .load_package_quietly(package, lib.loc)
            code_env <- asNamespace(package)
            if (!is.null(DLLs <- get0("DLLs", envir = code_env$.__NAMESPACE__.))) {
                if (length(DLLs)) 
                  has_namespace <- TRUE
                if (length(DLLs) && inherits(DLLs[[1L]], "DLLInfo")) {
                  pkgDLL <- unclass(DLLs[[1L]])$name
                  if (registration) {
                    reg <- getDLLRegisteredRoutines(DLLs[[1L]])
                    have_registration <- sum(lengths(reg)) > 
                      0L
                  }
                }
            }
        }
        else {
            has_namespace <- have_registration <- TRUE
            code_env <- .package_env(package)
        }
        is_installed <- TRUE
    }
    else if (!missing(dir)) {
        have_registration <- FALSE
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        else dir <- file_path_as_absolute(dir)
        pkg <- pkgDLL <- basename(dir)
        dfile <- file.path(dir, "DESCRIPTION")
        enc <- NA
        db <- NULL
        if (file.exists(dfile)) {
            db <- .read_description(dfile)
            enc <- db["Encoding"]
        }
        if (pkg == "base") 
            has_namespace <- TRUE
        if (file.exists(file.path(dir, "NAMESPACE"))) {
            nm <- parseNamespaceFile(basename(dir), dirname(dir))
            has_namespace <- length(nm$dynlibs) > 0L
        }
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        file <- tempfile()
        on.exit(unlink(file))
        if (!file.create(file)) 
            stop("unable to create ", file, domain = NA)
        if (!all(.file_append_ensuring_LFs(file, list_files_with_type(code_dir, 
            "code")))) 
            stop("unable to write code files", domain = NA)
    }
    else if (!missing(file)) {
        pkg <- enc <- NA
    }
    else stop("you must specify 'package', 'dir' or 'file'")
    if (missing(package) && !file_test("-f", file)) 
        stop(gettextf("file '%s' does not exist", file), domain = NA)
    bad_exprs <- empty_exprs <- wrong_pkg <- other_problem <- list()
    other_desc <- character()
    bad_pkg <- character()
    dup_false <- list()
    FF_funs <- FF_fun_names <- c(".C", ".Fortran", ".Call", ".External", 
        ".Call.graphics", ".External.graphics")
    if (!missing(package)) {
        is_FF_fun_from_base <- sapply(FF_funs, function(f) {
            e <- .find_owner_env(f, code_env)
            (identical(e, baseenv()) || identical(e, .BaseNamespaceEnv))
        })
        FF_funs <- FF_funs[is_FF_fun_from_base]
    }
    FF_funs <- c(FF_funs, sprintf("base::%s", FF_fun_names))
    check_registration <- function(e, fr) {
        sym <- e[[2L]]
        name <- deparse(sym, nlines = 1L)
        if (name == "...") 
            return("SYMBOL OK")
        if (is.character(sym)) {
            if (!have_registration) 
                return("SYMBOL OK")
            FF_fun <- as.character(e[[1L]])
            sym <- reg[[FF_fun]][[sym]]
            if (is.null(sym)) 
                return("SYMBOL OK")
        }
        if (!is_installed) {
            if (!is_installed_msg) {
                other_problem <<- c(other_problem, e)
                other_desc <<- c(other_desc, "foreign function registration not tested, as package was not installed")
                is_installed_msg <<- TRUE
            }
            return("OTHER")
        }
        if (is.symbol(sym)) {
            if (!exists(name, code_env, inherits = FALSE)) {
                if (allow_suppress && name %in% utils::suppressForeignCheck(, 
                  package)) 
                  return("SYMBOL OK")
                if (have_registration) {
                  if (name %in% fr) {
                    other_problem <<- c(other_problem, e)
                    other_desc <<- c(other_desc, sprintf("symbol %s in the local frame", 
                      sQuote(name)))
                  }
                  else {
                    other_problem <<- c(other_problem, e)
                    other_desc <<- c(other_desc, sprintf("symbol %s not in namespace", 
                      sQuote(name)))
                  }
                }
                return("OTHER")
            }
        }
        else if (suppressCheck(sym)) 
            return("SKIPPED")
        sym <- tryCatch(eval(sym, code_env), error = function(e) e)
        if (inherits(sym, "error")) {
            if (have_registration || !allow_suppress) {
                other_problem <<- c(other_problem, e)
                other_desc <<- c(other_desc, sprintf("Evaluating %s during check gives error\n%s", 
                  sQuote(name), sQuote(sym$message)))
            }
            return("OTHER")
        }
        FF_fun <- as.character(e[[1L]])
        if (is.character(sym)) {
            if (!have_registration) 
                return("SYMBOL OK")
            sym <- reg[[FF_fun]][[sym]]
            if (is.null(sym)) 
                return("SYMBOL OK")
        }
        if (inherits(sym, "RegisteredNativeSymbol") || inherits(sym, 
            "NativeSymbol")) 
            return("SYMBOL OK")
        if (!inherits(sym, "NativeSymbolInfo")) {
            other_problem <<- c(other_problem, e)
            other_desc <<- c(other_desc, sprintf("%s is of class \"%s\"", 
                sQuote(name), class(sym)))
            return("OTHER")
        }
        parg <- unclass(sym$dll)$name
        if (length(parg) == 1L && !parg %in% c("Rcpp", pkgDLL)) {
            wrong_pkg <<- c(wrong_pkg, e)
            bad_pkg <<- c(bad_pkg, parg)
        }
        numparms <- sym$numParameters
        if (length(numparms) && numparms >= 0) {
            if (any(as.character(e) == "...")) {
                other_problem <<- c(other_problem, e)
                other_desc <<- c(other_desc, sprintf("call includes ..., expected %d %s", 
                  numparms, if (numparms > 1L) "parameters" else "parameter"))
            }
            else {
                callparms <- length(e) - 2L
                if ("PACKAGE" %in% names(e)) 
                  callparms <- callparms - 1L
                if (FF_fun %in% c(".C", ".Fortran")) 
                  callparms <- callparms - length(intersect(names(e), 
                    c("NAOK", "DUP", "ENCODING")))
                if (!is.null(numparms) && numparms >= 0L && numparms != 
                  callparms) {
                  other_problem <<- c(other_problem, e)
                  other_desc <<- c(other_desc, sprintf("call to %s with %d %s, expected %d", 
                    sQuote(name), callparms, if (callparms > 
                      1L) "parameters" else "parameter", numparms))
                  return("OTHER")
                }
            }
        }
        if (inherits(sym, "CallRoutine") && !(FF_fun %in% c(".Call", 
            ".Call.graphics"))) {
            other_problem <<- c(other_problem, e)
            other_desc <<- c(other_desc, sprintf("%s registered as %s, but called with %s", 
                sQuote(name), ".Call", FF_fun))
            return("OTHER")
        }
        if (inherits(sym, "ExternalRoutine") && !(FF_fun %in% 
            c(".External", ".External.graphics"))) {
            other_problem <<- c(other_problem, e)
            other_desc <<- c(other_desc, sprintf("%s registered as %s, but called with %s", 
                sQuote(name), ".External", FF_fun))
            return("OTHER")
        }
        "SYMBOL OK"
    }
    find_bad_exprs <- function(e) {
        if (is.call(e) || is.expression(e)) {
            if (deparse(e[[1L]])[1L] %in% FF_funs) {
                if (registration) 
                  check_registration(e, fr)
                dup <- e[["DUP"]]
                if (!is.null(dup) && !identical(dup, TRUE)) 
                  dup_false <<- c(dup_false, e)
                this <- ""
                this <- parg <- e[["PACKAGE"]]
                if (!is.na(pkg) && is.character(parg) && nzchar(parg) && 
                  parg != pkgDLL) {
                  wrong_pkg <<- c(wrong_pkg, e)
                  bad_pkg <<- c(bad_pkg, this)
                }
                parg <- if (!is.null(parg) && (nzchar(parg))) 
                  "OK"
                else if (identical(parg, "")) {
                  empty_exprs <<- c(empty_exprs, e)
                  "EMPTY"
                }
                else if (!is.character(sym <- e[[2L]])) {
                  if (!registration) {
                    sym <- tryCatch(eval(sym, code_env), error = function(e) e)
                    if (inherits(sym, "NativeSymbolInfo")) {
                      parg <- unclass(sym$dll)$name
                      if (length(parg) == 1L && !parg %in% c("Rcpp", 
                        pkgDLL)) {
                        wrong_pkg <<- c(wrong_pkg, e)
                        bad_pkg <<- c(bad_pkg, parg)
                      }
                    }
                  }
                  "Called with symbol"
                }
                else if (!has_namespace) {
                  bad_exprs <<- c(bad_exprs, e)
                  "MISSING"
                }
                else "MISSING but in a function in a namespace"
                if (verbose) 
                  if (is.null(this)) 
                    cat(deparse(e[[1L]]), "(", deparse(e[[2L]]), 
                      ", ... ): ", parg, "\n", sep = "")
                  else cat(deparse(e[[1L]]), "(", deparse(e[[2L]]), 
                    ", ..., PACKAGE = \"", this, "\"): ", parg, 
                    "\n", sep = "")
            }
            else if (deparse(e[[1L]])[1L] %in% "<-") {
                fr <<- c(fr, as.character(e[[2L]]))
            }
            for (i in seq_along(e)) Recall(e[[i]])
        }
    }
    if (!missing(package)) {
        checkFFmy <- function(f) if (typeof(f) == "closure") {
            env <- environment(f)
            if (isNamespace(env)) {
                nm <- getNamespaceName(env)
                if (nm == package) 
                  body(f)
                else NULL
            }
            else body(f)
        }
        else NULL
        exprs <- lapply(ls(envir = code_env, all.names = TRUE), 
            function(f) {
                f <- get(f, envir = code_env)
                checkFFmy(f)
            })
        if (.isMethodsDispatchOn()) {
            for (f in .get_S4_generics(code_env)) {
                mlist <- .get_S4_methods_list(f, code_env)
                exprs <- c(exprs, lapply(mlist, body))
            }
            refs <- .get_ref_classes(code_env)
            if (length(refs)) {
                exprs2 <- lapply(unlist(refs, FALSE), checkFFmy)
                exprs <- c(exprs, exprs2)
            }
        }
    }
    else {
        if (!is.na(enc) && !(Sys.getlocale("LC_CTYPE") %in% c("C", 
            "POSIX"))) {
            con <- file(file, encoding = enc)
            on.exit(close(con))
        }
        else con <- file
        exprs <- tryCatch(parse(file = con, n = -1L), error = function(e) stop(gettextf("parse error in file '%s':\n%s", 
            file, .massage_file_parse_error_message(conditionMessage(e))), 
            domain = NA, call. = FALSE))
    }
    for (i in seq_along(exprs)) {
        fr <- character()
        find_bad_exprs(exprs[[i]])
    }
    attr(bad_exprs, "wrong_pkg") <- wrong_pkg
    attr(bad_exprs, "bad_pkg") <- bad_pkg
    attr(bad_exprs, "empty") <- empty_exprs
    attr(bad_exprs, "other_problem") <- other_problem
    attr(bad_exprs, "other_desc") <- other_desc
    if (check_DUP) 
        attr(bad_exprs, "dup_false") <- dup_false
    if (length(bad_pkg)) {
        bases <- .get_standard_package_names()$base
        bad <- bad_pkg[!bad_pkg %in% bases]
        if (length(bad)) {
            depends <- .get_requires_from_package_db(db, "Depends")
            imports <- .get_requires_from_package_db(db, "Imports")
            suggests <- .get_requires_from_package_db(db, "Suggests")
            enhances <- .get_requires_from_package_db(db, "Enhances")
            bad <- bad[!bad %in% c(depends, imports, suggests, 
                enhances)]
            attr(bad_exprs, "undeclared") <- bad
        }
    }
    class(bad_exprs) <- "checkFF"
    if (verbose) 
        invisible(bad_exprs)
    else bad_exprs
}


checkPoFile <- function (f, strictPlural = FALSE) 
{
    getfmts <- function(s) .Call(C_getfmts, s)
    lines <- readLines(f, encoding = "bytes")
    i <- 0
    noCformat <- FALSE
    f1_plural <- NULL
    ref <- NA
    fuzzy <- FALSE
    result <- matrix(character(), ncol = 5L, nrow = 0L)
    while (i < length(lines)) {
        i <- i + 1L
        if (startsWith(lines[i], "#,")) {
            noCformat <- noCformat || grepl("no-c-format", lines[i], 
                useBytes = TRUE)
            fuzzy <- fuzzy || grepl("fuzzy", lines[i], useBytes = TRUE)
        }
        else if (startsWith(lines[i], "#:")) {
            if (!is.na(ref)) 
                ref <- paste(ref, "etc.")
            else ref <- sub("^#:[[:blank:]]*", "", lines[i])
        }
        else if (startsWith(lines[i], "msgid ")) {
            s1 <- sub("^msgid[[:blank:]]+[\"](.*)[\"][[:blank:]]*$", 
                "\\1", lines[i])
            while (startsWith(lines[i + 1L], "\"")) {
                i <- i + 1L
                s1 <- paste0(s1, sub("^[\"](.*)[\"][[:blank:]]*$", 
                  "\\1", lines[i]))
            }
            f1 <- tryCatch(getfmts(s1), error = function(e) e)
            j <- i + 1L
            if (noCformat || inherits(f1, "error")) {
                noCformat <- FALSE
                next
            }
            while (j <= length(lines)) {
                if (grepl("^msgid_plural[[:blank:]]", lines[j], 
                  useBytes = TRUE)) 
                  statement <- "msgid_plural"
                else if (grepl("^msgstr[[:blank:]]", lines[j], 
                  useBytes = TRUE)) 
                  statement <- "msgstr"
                else if (grepl("^msgstr\\[[[:digit:]]+\\][[:blank:]]", 
                  lines[j], useBytes = TRUE)) 
                  statement <- sub("^(msgstr)\\[([[:digit:]]+)\\].*$", 
                    "\\1\\\\[\\2\\\\]", lines[j])
                else break
                s2 <- sub(paste0("^", statement, "[[:blank:]]+[\"](.*)[\"][[:blank:]]*$"), 
                  "\\1", lines[j])
                while (!is.na(lines[j + 1L]) && startsWith(lines[j + 
                  1L], "\"")) {
                  j <- j + 1L
                  s2 <- paste0(s2, sub("^[\"](.*)[\"][[:blank:]]*$", 
                    "\\1", lines[j]))
                }
                if (s1 == "") {
                  encoding <- sub(".*Content-Type:[^\\]*charset=([^\\[:space:]]*)[[:space:]]*\\\\n.*", 
                    "\\1", s2)
                  lines <- iconv(lines, encoding, "UTF-8")
                  break
                }
                f2 <- tryCatch(getfmts(s2), error = function(e) e)
                if (statement == "msgid_plural") {
                  if (!strictPlural) {
                    f1_plural <- f2
                    j <- j + 1L
                    next
                  }
                }
                if (nzchar(s2) && !(identical(f1, f2) || identical(f1_plural, 
                  f2))) {
                  location <- paste0(f, ":", j)
                  if (inherits(f2, "error")) 
                    diff <- conditionMessage(f2)
                  else {
                    if (length(f1) < length(f2)) {
                      diff <- "too many entries"
                      length(f2) <- length(f1)
                    }
                    else if (length(f1) > length(f2)) {
                      diff <- "too few entries"
                      length(f1) <- length(f2)
                    }
                    else diff <- ""
                    diffs <- which(f1 != f2)
                    if (length(diffs)) {
                      if (nzchar(diff)) 
                        diff <- paste0(diff, ", ")
                      if (length(diffs) > 1) 
                        diff <- paste(paste0(diff, "differences in entries"), 
                          paste(diffs, collapse = ","))
                      else diff <- paste(paste0(diff, "difference in entry"), 
                        diffs)
                    }
                    if (grepl("٪", s2, fixed = TRUE)) 
                      diff <- paste0(diff, ", translation contains arabic percent sign U+066A")
                    if (grepl("﹪", s2, fixed = TRUE)) 
                      diff <- paste0(diff, ", translation contains small percent sign U+FE6A")
                    if (grepl("％", s2, fixed = TRUE)) 
                      diff <- paste0(diff, ", translation contains wide percent sign U+FF05")
                  }
                  if (!fuzzy) 
                    result <- rbind(result, c(location, ref, 
                      diff, s1, s2))
                }
                j <- j + 1L
            }
            i <- j - 1L
            noCformat <- FALSE
            f1_plural <- NULL
            ref <- NA
            fuzzy <- FALSE
        }
    }
    structure(result, class = "check_po_files")
}


encoded_text_to_latex <- function (x, encoding = c("latin1", "latin2", "latin9", "UTF-8", 
    "utf8")) 
{
    encoding <- match.arg(encoding)
    do_latin1 <- function(x) {
        xx <- charToRaw(x)
        paste(latin1table[as.integer(xx)], collapse = "")
    }
    do_latin2 <- function(x) {
        xx <- charToRaw(x)
        paste(latin2table[as.integer(xx)], collapse = "")
    }
    do_latin9 <- function(x) {
        xx <- charToRaw(x)
        paste(latin9table[as.integer(xx)], collapse = "")
    }
    do_utf8 <- function(x) {
        xx <- utf8ToInt(x)
        y <- rep("?", length(x))
        y[xx < 512] <- utf8table[xx]
        y[xx == 710] <- "{\\textasciicircum}"
        y[xx == 711] <- "{\\textasciicaron}"
        y[xx == 714] <- "{\\textasciitilde}"
        y[xx == 728] <- "{\\textasciibreve}"
        y[xx == 729] <- "{\\textperiodcentered}"
        y[xx == 733] <- "{\\textacutedbl}"
        y[xx == 8204] <- "{\\textcompwordmark}"
        y[xx == 8216] <- "{\\textquoteleft}"
        y[xx == 8217] <- "{\\textquoteright}"
        y[xx == 8220] <- "{\\textquotedblleft}"
        y[xx == 8221] <- "{\\textquotedblright}"
        y[xx == 8224] <- "{\\textdagger}"
        y[xx == 8226] <- "{\\textbullet}"
        y[xx == 8230] <- "{\\textellipsis}"
        y[xx == 8364] <- "{\\texteuro}"
        paste(y, collapse = "")
    }
    as.vector(switch(encoding, latin1 = sapply(x, do_latin1), 
        latin2 = sapply(x, do_latin2), latin9 = sapply(x, do_latin9), 
        `UTF-8` = sapply(x, do_utf8), utf8 = sapply(x, do_utf8), 
        stop("unimplemented encoding")))
}


SIGCHLD <- 17L


SIGTSTP <- 20L


charset_to_Unicode <- structure(c(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 
12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 
25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 
38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 
51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 
64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 
77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 
90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 
102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 
113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 
124L, 125L, 126L, 127L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 160L, 161L, 162L, 163L, 164L, 165L, 166L, 
167L, 168L, 169L, 170L, 171L, 172L, 173L, 174L, 175L, 176L, 177L, 
178L, 179L, 180L, 181L, 182L, 183L, 184L, 185L, 186L, 187L, 188L, 
189L, 190L, 191L, 192L, 193L, 194L, 195L, 196L, 197L, 198L, 199L, 
200L, 201L, 202L, 203L, 204L, 205L, 206L, 207L, 208L, 209L, 210L, 
211L, 212L, 213L, 214L, 215L, 216L, 217L, 218L, 219L, 220L, 221L, 
222L, 223L, 224L, 225L, 226L, 227L, 228L, 229L, 230L, 231L, 232L, 
233L, 234L, 235L, 236L, 237L, 238L, 239L, 240L, 241L, 242L, 243L, 
244L, 245L, 246L, 247L, 248L, 249L, 250L, 251L, 252L, 253L, 254L, 
255L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 
13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 
26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 
39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 
52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 
65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 77L, 
78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 
91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 102L, 
103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 
114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 
125L, 126L, 127L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 160L, 260L, 728L, 321L, 164L, 317L, 346L, 
167L, 168L, 352L, 350L, 356L, 377L, 173L, 381L, 379L, 176L, 261L, 
731L, 322L, 180L, 318L, 347L, 711L, 184L, 353L, 351L, 357L, 378L, 
733L, 382L, 380L, 340L, 193L, 194L, 258L, 196L, 313L, 262L, 199L, 
268L, 201L, 280L, 203L, 282L, 205L, 206L, 270L, 272L, 323L, 327L, 
211L, 212L, 336L, 214L, 215L, 344L, 366L, 218L, 368L, 220L, 221L, 
354L, 223L, 341L, 225L, 226L, 259L, 228L, 314L, 263L, 231L, 269L, 
233L, 281L, 235L, 283L, 237L, 238L, 271L, 273L, 324L, 328L, 243L, 
244L, 337L, 246L, 247L, 345L, 367L, 250L, 369L, 252L, 253L, 355L, 
729L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 
13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 
26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 
39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 
52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 
65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 77L, 
78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 
91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 102L, 
103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 
114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 
125L, 126L, 127L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 160L, 8221L, 162L, 163L, 164L, 8222L, 166L, 
167L, 216L, 169L, 342L, 171L, 172L, 173L, 174L, 198L, 176L, 177L, 
178L, 179L, 8220L, 181L, 182L, 183L, 248L, 185L, 343L, 187L, 
188L, 189L, 190L, 230L, 260L, 302L, 256L, 262L, 196L, 197L, 280L, 
274L, 268L, 201L, 377L, 278L, 290L, 310L, 298L, 315L, 352L, 323L, 
325L, 211L, 332L, 213L, 214L, 215L, 370L, 321L, 346L, 362L, 220L, 
379L, 381L, 223L, 261L, 303L, 257L, 263L, 228L, 229L, 281L, 275L, 
269L, 233L, 378L, 279L, 291L, 311L, 299L, 316L, 353L, 324L, 326L, 
243L, 333L, 245L, 246L, 247L, 371L, 322L, 347L, 363L, 252L, 380L, 
382L, 8217L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 
12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 
25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 
38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 
51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 
64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 
77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 
90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 
102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 
113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 
124L, 125L, 126L, 127L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 160L, 161L, 162L, 163L, 8364L, 165L, 
352L, 167L, 353L, 169L, 170L, 171L, 172L, 173L, 174L, 175L, 176L, 
177L, 178L, 179L, 381L, 181L, 182L, 183L, 382L, 185L, 186L, 187L, 
338L, 339L, 376L, 191L, 192L, 193L, 194L, 195L, 196L, 197L, 198L, 
199L, 200L, 201L, 202L, 203L, 204L, 205L, 206L, 207L, 208L, 209L, 
210L, 211L, 212L, 213L, 214L, 215L, 216L, 217L, 218L, 219L, 220L, 
221L, 222L, 223L, 224L, 225L, 226L, 227L, 228L, 229L, 230L, 231L, 
232L, 233L, 234L, 235L, 236L, 237L, 238L, 239L, 240L, 241L, 242L, 
243L, 244L, 245L, 246L, 247L, 248L, 249L, 250L, 251L, 252L, 253L, 
254L, 255L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 
12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 
25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 
38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 
51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 
64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 
77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 
90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 
102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 
113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 
124L, 125L, 126L, 127L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 160L, 1025L, 1026L, 1027L, 1028L, 1029L, 
1030L, 1031L, 1032L, 1033L, 1034L, 1035L, 1036L, 173L, 1038L, 
1039L, 1040L, 1041L, 1042L, 1043L, 1044L, 1045L, 1046L, 1047L, 
1048L, 1049L, 1050L, 1051L, 1052L, 1053L, 1054L, 1055L, 1056L, 
1057L, 1058L, 1059L, 1060L, 1061L, 1062L, 1063L, 1064L, 1065L, 
1066L, 1067L, 1068L, 1069L, 1070L, 1071L, 1072L, 1073L, 1074L, 
1075L, 1076L, 1077L, 1078L, 1079L, 1080L, 1081L, 1082L, 1083L, 
1084L, 1085L, 1086L, 1087L, 1088L, 1089L, 1090L, 1091L, 1092L, 
1093L, 1094L, 1095L, 1096L, 1097L, 1098L, 1099L, 1100L, 1101L, 
1102L, 1103L, 8470L, 1105L, 1106L, 1107L, 1108L, 1109L, 1110L, 
1111L, 1112L, 1113L, 1114L, 1115L, 1116L, 167L, 1118L, 1119L, 
0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 
15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 
28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 
41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 
54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 
67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 77L, 78L, 79L, 
80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 91L, 92L, 
93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 102L, 103L, 104L, 
105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L, 
116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 125L, 126L, 
127L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 160L, 8216L, 8217L, 163L, 8364L, 8367L, 166L, 167L, 168L, 
169L, 890L, 171L, 172L, 173L, 65533L, 8213L, 176L, 177L, 178L, 
179L, 900L, 901L, 902L, 183L, 904L, 905L, 906L, 187L, 908L, 189L, 
910L, 911L, 912L, 913L, 914L, 915L, 916L, 917L, 918L, 919L, 920L, 
921L, 922L, 923L, 924L, 925L, 926L, 927L, 928L, 929L, 65533L, 
931L, 932L, 933L, 934L, 935L, 936L, 937L, 938L, 939L, 940L, 941L, 
942L, 943L, 944L, 945L, 946L, 947L, 948L, 949L, 950L, 951L, 952L, 
953L, 954L, 955L, 956L, 957L, 958L, 959L, 960L, 961L, 962L, 963L, 
964L, 965L, 966L, 967L, 968L, 969L, 970L, 971L, 972L, 973L, 974L, 
65533L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 
13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 
26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 
39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 
52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 
65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 77L, 
78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 
91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 102L, 
103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 
114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 
125L, 126L, 127L, 9472L, 9474L, 9484L, 9488L, 9492L, 9496L, 9500L, 
9508L, 9516L, 9524L, 9532L, 9600L, 9604L, 9608L, 9612L, 9616L, 
9617L, 9618L, 9619L, 8992L, 9632L, 8729L, 8730L, 8776L, 8804L, 
8805L, 160L, 8993L, 176L, 178L, 183L, 247L, 9552L, 9553L, 9554L, 
1105L, 9555L, 9556L, 9557L, 9558L, 9559L, 9560L, 9561L, 9562L, 
9563L, 9564L, 9565L, 9566L, 9567L, 9568L, 9569L, 1025L, 9570L, 
9571L, 9572L, 9573L, 9574L, 9575L, 9576L, 9577L, 9578L, 9579L, 
9580L, 169L, 1102L, 1072L, 1073L, 1094L, 1076L, 1077L, 1092L, 
1075L, 1093L, 1080L, 1081L, 1082L, 1083L, 1084L, 1085L, 1086L, 
1087L, 1103L, 1088L, 1089L, 1090L, 1091L, 1078L, 1074L, 1100L, 
1099L, 1079L, 1096L, 1101L, 1097L, 1095L, 1098L, 1070L, 1040L, 
1041L, 1062L, 1044L, 1045L, 1060L, 1043L, 1061L, 1048L, 1049L, 
1050L, 1051L, 1052L, 1053L, 1054L, 1055L, 1071L, 1056L, 1057L, 
1058L, 1059L, 1046L, 1042L, 1068L, 1067L, 1047L, 1064L, 1069L, 
1065L, 1063L, 1066L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 
10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 
23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 
36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 
49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 
62L, 63L, 64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 
75L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 
88L, 89L, 90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 
101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 
112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 
123L, 124L, 125L, 126L, 127L, 9472L, 9474L, 9484L, 9488L, 9492L, 
9496L, 9500L, 9508L, 9516L, 9524L, 9532L, 9600L, 9604L, 9608L, 
9612L, 9616L, 9617L, 9618L, 9619L, 8992L, 9632L, 8729L, 8730L, 
8776L, 8804L, 8805L, 160L, 8993L, 176L, 178L, 183L, 247L, 9552L, 
9553L, 9554L, 1105L, 1108L, 9556L, 1110L, 1111L, 9559L, 9560L, 
9561L, 9562L, 9563L, 1169L, 9565L, 9566L, 9567L, 9568L, 9569L, 
1025L, 1028L, 9571L, 1030L, 1031L, 9574L, 9575L, 9576L, 9577L, 
9578L, 1168L, 9580L, 169L, 1102L, 1072L, 1073L, 1094L, 1076L, 
1077L, 1092L, 1075L, 1093L, 1080L, 1081L, 1082L, 1083L, 1084L, 
1085L, 1086L, 1087L, 1103L, 1088L, 1089L, 1090L, 1091L, 1078L, 
1074L, 1100L, 1099L, 1079L, 1096L, 1101L, 1097L, 1095L, 1098L, 
1070L, 1040L, 1041L, 1062L, 1044L, 1045L, 1060L, 1043L, 1061L, 
1048L, 1049L, 1050L, 1051L, 1052L, 1053L, 1054L, 1055L, 1071L, 
1056L, 1057L, 1058L, 1059L, 1046L, 1042L, 1068L, 1067L, 1047L, 
1064L, 1069L, 1065L, 1063L, 1066L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 
7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 
20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 
33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 
46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 
59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 
72L, 73L, 74L, 75L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 
85L, 86L, 87L, 88L, 89L, 90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 
98L, 99L, 100L, 101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 
109L, 110L, 111L, 112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 
120L, 121L, 122L, 123L, 124L, 125L, 126L, 127L, 8364L, 65533L, 
8218L, 65533L, 8222L, 8230L, 8224L, 8225L, 65533L, 8240L, 352L, 
8249L, 346L, 356L, 381L, 377L, 65533L, 8216L, 8217L, 8220L, 8221L, 
8226L, 8211L, 8212L, 65533L, 8482L, 353L, 8250L, 347L, 357L, 
382L, 378L, 160L, 711L, 728L, 321L, 164L, 260L, 166L, 167L, 168L, 
169L, 350L, 171L, 172L, 173L, 174L, 379L, 176L, 177L, 731L, 322L, 
180L, 181L, 182L, 183L, 184L, 261L, 351L, 187L, 317L, 733L, 318L, 
380L, 340L, 193L, 194L, 258L, 196L, 313L, 262L, 199L, 268L, 201L, 
280L, 203L, 282L, 205L, 206L, 270L, 272L, 323L, 327L, 211L, 212L, 
336L, 214L, 215L, 344L, 366L, 218L, 368L, 220L, 221L, 354L, 223L, 
341L, 225L, 226L, 259L, 228L, 314L, 263L, 231L, 269L, 233L, 281L, 
235L, 283L, 237L, 238L, 271L, 273L, 324L, 328L, 243L, 244L, 337L, 
246L, 247L, 345L, 367L, 250L, 369L, 252L, 253L, 355L, 729L, 0L, 
1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 
15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 
28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 39L, 40L, 
41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 52L, 53L, 
54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 65L, 66L, 
67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 77L, 78L, 79L, 
80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 91L, 92L, 
93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 102L, 103L, 104L, 
105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 114L, 115L, 
116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 125L, 126L, 
127L, 1026L, 1027L, 8218L, 1107L, 8222L, 8230L, 8224L, 8225L, 
8364L, 8240L, 1033L, 8249L, 1034L, 1036L, 1035L, 1039L, 1106L, 
8216L, 8217L, 8220L, 8221L, 8226L, 8211L, 8212L, 65533L, 8482L, 
1113L, 8250L, 1114L, 1116L, 1115L, 1119L, 160L, 1038L, 1118L, 
1032L, 164L, 1168L, 166L, 167L, 1025L, 169L, 1028L, 171L, 172L, 
173L, 174L, 1031L, 176L, 177L, 1030L, 1110L, 1169L, 181L, 182L, 
183L, 1105L, 8470L, 1108L, 187L, 1112L, 1029L, 1109L, 1111L, 
1040L, 1041L, 1042L, 1043L, 1044L, 1045L, 1046L, 1047L, 1048L, 
1049L, 1050L, 1051L, 1052L, 1053L, 1054L, 1055L, 1056L, 1057L, 
1058L, 1059L, 1060L, 1061L, 1062L, 1063L, 1064L, 1065L, 1066L, 
1067L, 1068L, 1069L, 1070L, 1071L, 1072L, 1073L, 1074L, 1075L, 
1076L, 1077L, 1078L, 1079L, 1080L, 1081L, 1082L, 1083L, 1084L, 
1085L, 1086L, 1087L, 1088L, 1089L, 1090L, 1091L, 1092L, 1093L, 
1094L, 1095L, 1096L, 1097L, 1098L, 1099L, 1100L, 1101L, 1102L, 
1103L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 
13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 
26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 37L, 38L, 
39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 50L, 51L, 
52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 64L, 
65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 76L, 77L, 
78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 89L, 90L, 
91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 101L, 102L, 
103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 112L, 113L, 
114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 123L, 124L, 
125L, 126L, 127L, 8364L, 65533L, 8218L, 402L, 8222L, 8230L, 8224L, 
8225L, 710L, 8240L, 352L, 8249L, 338L, 65533L, 381L, 65533L, 
65533L, 8216L, 8217L, 8220L, 8221L, 8226L, 8211L, 8212L, 732L, 
8482L, 353L, 8250L, 339L, 65533L, 382L, 376L, 160L, 161L, 162L, 
163L, 164L, 165L, 166L, 167L, 168L, 169L, 170L, 171L, 172L, 173L, 
174L, 175L, 176L, 177L, 178L, 179L, 180L, 181L, 182L, 183L, 184L, 
185L, 186L, 187L, 188L, 189L, 190L, 191L, 192L, 193L, 194L, 195L, 
196L, 197L, 198L, 199L, 200L, 201L, 202L, 203L, 204L, 205L, 206L, 
207L, 208L, 209L, 210L, 211L, 212L, 213L, 214L, 215L, 216L, 217L, 
218L, 219L, 220L, 221L, 222L, 223L, 224L, 225L, 226L, 227L, 228L, 
229L, 230L, 231L, 232L, 233L, 234L, 235L, 236L, 237L, 238L, 239L, 
240L, 241L, 242L, 243L, 244L, 245L, 246L, 247L, 248L, 249L, 250L, 
251L, 252L, 253L, 254L, 255L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 
8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 
21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 
34L, 35L, 36L, 37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 
47L, 48L, 49L, 50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 
60L, 61L, 62L, 63L, 64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 
73L, 74L, 75L, 76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 
86L, 87L, 88L, 89L, 90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 
99L, 100L, 101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 
110L, 111L, 112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 
121L, 122L, 123L, 124L, 125L, 126L, 127L, 8364L, 65533L, 8218L, 
402L, 8222L, 8230L, 8224L, 8225L, 65533L, 8240L, 65533L, 8249L, 
65533L, 65533L, 65533L, 65533L, 65533L, 8216L, 8217L, 8220L, 
8221L, 8226L, 8211L, 8212L, 65533L, 8482L, 65533L, 8250L, 65533L, 
65533L, 65533L, 65533L, 160L, 901L, 902L, 163L, 164L, 165L, 166L, 
167L, 168L, 169L, 65533L, 171L, 172L, 173L, 174L, 8213L, 176L, 
177L, 178L, 179L, 900L, 181L, 182L, 183L, 904L, 905L, 906L, 187L, 
908L, 189L, 910L, 911L, 912L, 913L, 914L, 915L, 916L, 917L, 918L, 
919L, 920L, 921L, 922L, 923L, 924L, 925L, 926L, 927L, 928L, 929L, 
65533L, 931L, 932L, 933L, 934L, 935L, 936L, 937L, 938L, 939L, 
940L, 941L, 942L, 943L, 944L, 945L, 946L, 947L, 948L, 949L, 950L, 
951L, 952L, 953L, 954L, 955L, 956L, 957L, 958L, 959L, 960L, 961L, 
962L, 963L, 964L, 965L, 966L, 967L, 968L, 969L, 970L, 971L, 972L, 
973L, 974L, 65533L, 0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
11L, 12L, 13L, 14L, 15L, 16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 
24L, 25L, 26L, 27L, 28L, 29L, 30L, 31L, 32L, 33L, 34L, 35L, 36L, 
37L, 38L, 39L, 40L, 41L, 42L, 43L, 44L, 45L, 46L, 47L, 48L, 49L, 
50L, 51L, 52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 
63L, 64L, 65L, 66L, 67L, 68L, 69L, 70L, 71L, 72L, 73L, 74L, 75L, 
76L, 77L, 78L, 79L, 80L, 81L, 82L, 83L, 84L, 85L, 86L, 87L, 88L, 
89L, 90L, 91L, 92L, 93L, 94L, 95L, 96L, 97L, 98L, 99L, 100L, 
101L, 102L, 103L, 104L, 105L, 106L, 107L, 108L, 109L, 110L, 111L, 
112L, 113L, 114L, 115L, 116L, 117L, 118L, 119L, 120L, 121L, 122L, 
123L, 124L, 125L, 126L, 127L, 8364L, 65533L, 8218L, 65533L, 8222L, 
8230L, 8224L, 8225L, 65533L, 8240L, 65533L, 8249L, 65533L, 168L, 
711L, 184L, 65533L, 8216L, 8217L, 8220L, 8221L, 8226L, 8211L, 
8212L, 65533L, 8482L, 65533L, 8250L, 65533L, 175L, 731L, 65533L, 
160L, 65533L, 162L, 163L, 164L, 65533L, 166L, 167L, 216L, 169L, 
342L, 171L, 172L, 173L, 174L, 198L, 176L, 177L, 178L, 179L, 180L, 
181L, 182L, 183L, 248L, 185L, 343L, 187L, 188L, 189L, 190L, 230L, 
260L, 302L, 256L, 262L, 196L, 197L, 280L, 274L, 268L, 201L, 377L, 
278L, 290L, 310L, 298L, 315L, 352L, 323L, 325L, 211L, 332L, 213L, 
214L, 215L, 370L, 321L, 346L, 362L, 220L, 379L, 381L, 223L, 261L, 
303L, 257L, 263L, 228L, 229L, 281L, 275L, 269L, 233L, 378L, 279L, 
291L, 311L, 299L, 316L, 353L, 324L, 326L, 243L, 333L, 245L, 246L, 
247L, 371L, 322L, 347L, 363L, 252L, 380L, 382L, 729L, 0L, 1L, 
2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 
16L, 17L, 18L, 19L, 20L, 21L, 22L, 23L, 24L, 25L, 26L, 27L, 28L, 
29L, 30L, 31L, 32L, 33L, 8704L, 35L, 8707L, 37L, 38L, 8717L, 
40L, 41L, 8727L, 43L, 44L, 8722L, 46L, 47L, 48L, 49L, 50L, 51L, 
52L, 53L, 54L, 55L, 56L, 57L, 58L, 59L, 60L, 61L, 62L, 63L, 8773L, 
913L, 914L, 935L, 916L, 917L, 934L, 915L, 919L, 921L, 977L, 922L, 
923L, 924L, 925L, 927L, 928L, 920L, 929L, 931L, 932L, 933L, 962L, 
937L, 926L, 936L, 918L, 91L, 8756L, 93L, 8869L, 95L, 63717L, 
945L, 946L, 967L, 948L, 949L, 966L, 947L, 951L, 953L, 981L, 954L, 
955L, 956L, 957L, 959L, 960L, 952L, 961L, 963L, 964L, 965L, 982L, 
969L, 958L, 968L, 950L, 123L, 124L, 125L, 8764L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 
0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 8364L, 
978L, 8242L, 8804L, 8260L, 8734L, 402L, 9827L, 9830L, 9829L, 
9824L, 8596L, 8592L, 8593L, 8594L, 8595L, 176L, 177L, 8243L, 
8805L, 215L, 8733L, 8706L, 8226L, 247L, 8800L, 8801L, 8776L, 
8230L, 63718L, 63719L, 8629L, 8501L, 8465L, 8476L, 8472L, 8855L, 
8853L, 8709L, 8745L, 8746L, 8835L, 8839L, 8836L, 8834L, 8838L, 
8712L, 8713L, 8736L, 8711L, 63194L, 63193L, 63195L, 8719L, 8730L, 
8901L, 172L, 8743L, 8744L, 8660L, 8656L, 8657L, 8658L, 8659L, 
9674L, 9001L, 63720L, 63721L, 63722L, 8721L, 63723L, 63724L, 
63725L, 63726L, 63727L, 63728L, 63729L, 63730L, 63731L, 63732L, 
0L, 9002L, 8747L, 8992L, 63733L, 8993L, 63734L, 63735L, 63736L, 
63737L, 63738L, 63739L, 63740L, 63741L, 63742L, 0L), .Dim = c(256L, 
14L), .Dimnames = list(c("00", "01", "02", "03", "04", "05", 
"06", "07", "08", "09", "0a", "0b", "0c", "0d", "0e", "0f", "10", 
"11", "12", "13", "14", "15", "16", "17", "18", "19", "1a", "1b", 
"1c", "1d", "1e", "1f", "20", "21", "22", "23", "24", "25", "26", 
"27", "28", "29", "2a", "2b", "2c", "2d", "2e", "2f", "30", "31", 
"32", "33", "34", "35", "36", "37", "38", "39", "3a", "3b", "3c", 
"3d", "3e", "3f", "40", "41", "42", "43", "44", "45", "46", "47", 
"48", "49", "4a", "4b", "4c", "4d", "4e", "4f", "50", "51", "52", 
"53", "54", "55", "56", "57", "58", "59", "5a", "5b", "5c", "5d", 
"5e", "5f", "60", "61", "62", "63", "64", "65", "66", "67", "68", 
"69", "6a", "6b", "6c", "6d", "6e", "6f", "70", "71", "72", "73", 
"74", "75", "76", "77", "78", "79", "7a", "7b", "7c", "7d", "7e", 
"7f", "80", "81", "82", "83", "84", "85", "86", "87", "88", "89", 
"8a", "8b", "8c", "8d", "8e", "8f", "90", "91", "92", "93", "94", 
"95", "96", "97", "98", "99", "9a", "9b", "9c", "9d", "9e", "9f", 
"a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8", "a9", "aa", 
"ab", "ac", "ad", "ae", "af", "b0", "b1", "b2", "b3", "b4", "b5", 
"b6", "b7", "b8", "b9", "ba", "bb", "bc", "bd", "be", "bf", "c0", 
"c1", "c2", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "ca", "cb", 
"cc", "cd", "ce", "cf", "d0", "d1", "d2", "d3", "d4", "d5", "d6", 
"d7", "d8", "d9", "da", "db", "dc", "dd", "de", "df", "e0", "e1", 
"e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "ea", "eb", "ec", 
"ed", "ee", "ef", "f0", "f1", "f2", "f3", "f4", "f5", "f6", "f7", 
"f8", "f9", "fa", "fb", "fc", "fd", "fe", "ff"), c("ISOLatin1", 
"ISOLatin2", "ISOLatin7", "ISOLatin9", "Cyrillic", "Greek", "KOI8R", 
"KOI8U", "CP1250", "CP1251", "CP1252", "CP1253", "CP1257", "AdobeSymbol"
)), class = c("noquote", "hexmode"))


readNEWS <- function (file = file.path(R.home(), "NEWS"), trace = FALSE, 
    chop = c("first", "1", "par1", "keepAll")) 
.Defunct()


Rd2txt <- function (Rd, out = "", package = "", defines = .Platform$OS.type, 
    stages = "render", outputEncoding = "", fragment = FALSE, 
    options, ...) 
{
    buffer <- character()
    linestart <- TRUE
    indent <- 0L
    wrapping <- TRUE
    keepFirstIndent <- FALSE
    dropBlank <- FALSE
    haveBlanks <- 0L
    enumItem <- 0L
    inEqn <- FALSE
    sectionLevel <- 0
    saveOpts <- Rd2txt_options()
    on.exit(Rd2txt_options(saveOpts))
    if (!missing(options)) 
        Rd2txt_options(options)
    WIDTH <- 0.9 * Rd2txt_options()$width
    HDR_WIDTH <- WIDTH - 2L
    startCapture <- function() {
        save <- list(buffer = buffer, linestart = linestart, 
            indent = indent, wrapping = wrapping, keepFirstIndent = keepFirstIndent, 
            dropBlank = dropBlank, haveBlanks = haveBlanks, enumItem = enumItem, 
            inEqn = inEqn)
        buffer <<- character()
        linestart <<- TRUE
        indent <<- 0L
        wrapping <<- TRUE
        keepFirstIndent <<- FALSE
        dropBlank <<- FALSE
        haveBlanks <<- 0L
        enumItem <<- 0L
        inEqn <<- FALSE
        save
    }
    endCapture <- function(saved) {
        result <- buffer
        buffer <<- saved$buffer
        linestart <<- saved$linestart
        indent <<- saved$indent
        wrapping <<- saved$wrapping
        keepFirstIndent <<- saved$keepFirstIndent
        dropBlank <<- saved$dropBlank
        haveBlanks <<- saved$haveBlanks
        enumItem <<- saved$enumItem
        inEqn <<- saved$inEqn
        result
    }
    WriteLines <- if (outputEncoding == "UTF-8" || (outputEncoding == 
        "" && l10n_info()[["UTF-8"]])) {
        function(x, con, outputEncoding, ...) writeLines(x, con, 
            useBytes = TRUE, ...)
    }
    else {
        function(x, con, outputEncoding, ...) {
            x <- iconv(x, "UTF-8", outputEncoding, sub = "byte", 
                mark = FALSE)
            writeLines(x, con, useBytes = TRUE, ...)
        }
    }
    frmt <- function(x, justify = "left", width = 0L) {
        justify <- match.arg(justify, c("left", "right", "centre", 
            "none"))
        w <- sum(nchar(x, "width"))
        if (w < width && justify != "none") {
            excess <- width - w
            left <- right <- 0L
            if (justify == "left") 
                right <- excess
            else if (justify == "right") 
                left <- excess
            else if (justify == "centre") {
                left <- excess%/%2
                right <- excess - left
            }
            paste(c(rep_len(" ", left), x, rep_len(" ", right)), 
                collapse = "")
        }
        else x
    }
    wrap <- function(doWrap = TRUE) if (doWrap != wrapping) {
        flushBuffer()
        wrapping <<- doWrap
    }
    putw <- function(...) {
        wrap(TRUE)
        put(...)
    }
    putf <- function(...) {
        wrap(FALSE)
        put(...)
    }
    put <- function(...) {
        txt <- paste0(..., collapse = "")
        trail <- grepl("\n$", txt)
        txt <- strsplit(txt, "\n", fixed = TRUE)[[1L]]
        if (dropBlank) {
            while (length(txt) && grepl("^[[:space:]]*$", txt[1L])) txt <- txt[-1L]
            if (length(txt)) 
                dropBlank <<- FALSE
        }
        if (!length(txt)) 
            return()
        haveBlanks <<- 0
        if (linestart) 
            buffer <<- c(buffer, txt)
        else if (length(buffer)) {
            buffer[length(buffer)] <<- paste0(buffer[length(buffer)], 
                txt[1L])
            buffer <<- c(buffer, txt[-1L])
        }
        else buffer <<- txt
        linestart <<- trail
    }
    flushBuffer <- function() {
        if (!length(buffer)) 
            return()
        if (wrapping) {
            if (keepFirstIndent) {
                first <- nchar(psub1("[^ ].*", "", buffer[1L]))
                keepFirstIndent <<- FALSE
            }
            else first <- indent
            buffer <<- c(buffer, "")
            blankLines <- grep("^[[:space:]]*$", buffer)
            result <- character()
            start <- 1L
            for (i in seq_along(blankLines)) {
                if (blankLines[i] > start) {
                  result <- c(result, strwrap(paste(buffer[start:(blankLines[i] - 
                    1L)], collapse = " "), WIDTH, indent = first, 
                    exdent = indent))
                  first <- indent
                }
                result <- c(result, "")
                start <- blankLines[i] + 1L
            }
            buffer <<- result[-length(result)]
            empty <- !nzchar(buffer)
            drop <- empty & c(FALSE, empty[-length(empty)])
            buffer <<- buffer[!drop]
        }
        else {
            if (keepFirstIndent) {
                if (length(buffer) > 1L) 
                  buffer[-1L] <<- paste0(strrep(" ", indent), 
                    buffer[-1L])
                keepFirstIndent <- FALSE
            }
            else buffer <<- paste0(strrep(" ", indent), buffer)
        }
        if (length(buffer)) 
            WriteLines(buffer, con, outputEncoding)
        buffer <<- character()
        linestart <<- TRUE
    }
    encoding <- "unknown"
    li <- l10n_info()
    use_fancy_quotes <- (.Platform$OS.type == "windows" && ((li$codepage >= 
        1250 && li$codepage <= 1258) || li$codepage == 874)) || 
        li[["UTF-8"]]
    if (!identical(getOption("useFancyQuotes"), FALSE) && use_fancy_quotes) {
        LSQM <- intToUtf8("0x2018")
        RSQM <- intToUtf8("0x2019")
        LDQM <- intToUtf8("0x201c")
        RDQM <- intToUtf8("0x201d")
    }
    else {
        LSQM <- RSQM <- "'"
        LDQM <- RDQM <- "\""
    }
    trim <- function(x) {
        x <- psub1("^\\s*", "", x)
        psub1("\\s*$", "", x)
    }
    txt_header <- function(header) {
        opts <- Rd2txt_options()
        header <- paste(strwrap(header, WIDTH), collapse = "\n")
        if (opts$underline_titles) {
            letters <- strsplit(header, "", fixed = TRUE)[[1L]]
            isaln <- grep("[[:alnum:]]", letters)
            letters[isaln] <- paste0("_\b", letters[isaln])
            paste(letters, collapse = "")
        }
        else header
    }
    unescape <- function(x) {
        x <- psub("(---|--)", "-", x)
        x
    }
    writeCode <- function(x) {
        txt <- as.character(x)
        if (inEqn) 
            txt <- txt_eqn(txt)
        txt <- fsub("\"\\{\"", "\"{\"", txt)
        txt <- fsub("\\dots", "...", txt)
        put(txt)
    }
    blankLine <- function(n = 1L) {
        while (length(buffer) && grepl("^[[:blank:]]*$", buffer[length(buffer)])) buffer <<- buffer[-length(buffer)]
        flushBuffer()
        if (n > haveBlanks) {
            buffer <<- rep_len("", n - haveBlanks)
            flushBuffer()
            haveBlanks <<- n
        }
        dropBlank <<- TRUE
    }
    txt_eqn <- function(x) {
        x <- psub("\\\\(Alpha|Beta|Gamma|Delta|Epsilon|Zeta|Eta|Theta|Iota|Kappa|Lambda|Mu|Nu|Xi|Omicron|Pi|Rho|Sigma|Tau|Upsilon|Phi|Chi|Psi|Omega|alpha|beta|gamma|delta|epsilon|zeta|eta|theta|iota|kappa|lambda|mu|nu|xi|omicron|pi|rho|sigma|tau|upsilon|phi|chi|psi|omega|sum|prod|sqrt)", 
            "\\1", x)
        x <- psub("\\\\(dots|ldots)", "...", x)
        x <- fsub("\\le", "<=", x)
        x <- fsub("\\ge", ">=", x)
        x <- fsub("\\infty", "Inf", x)
        x <- psub("\\\\(bold|strong|emph|var)\\{([^}]*)\\}", 
            "\\2", x)
        x <- psub("\\\\(code|samp)\\{([^}]*)\\}", "'\\2'", x)
        x
    }
    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            putf("## Not run:\n")
            writeCodeBlock(block, tag)
            blankLine(0L)
            putf("## End(Not run)\n")
        }
        else {
            putf("## Not run: ")
            writeCodeBlock(block, tag)
            blankLine(0L)
        }
    }
    writeQ <- function(block, tag, quote = tag) {
        if (use_fancy_quotes) {
            if (quote == "\\sQuote") {
                put(LSQM)
                writeContent(block, tag)
                put(RSQM)
            }
            else {
                put(LDQM)
                writeContent(block, tag)
                put(RDQM)
            }
        }
        else {
            if (quote == "\\sQuote") {
                put("'")
                writeContent(block, tag)
                put("'")
            }
            else {
                put("\"")
                writeContent(block, tag)
                put("\"")
            }
        }
    }
    writeBlock <- function(block, tag, blocktag) {
        switch(tag, UNKNOWN = , VERB = , RCODE = writeCode(tabExpand(block)), 
            TEXT = if (blocktag == "\\command") putw(block) else putw(unescape(tabExpand(block))), 
            USERMACRO = , `\\newcommand` = , `\\renewcommand` = , 
            COMMENT = {
            }, LIST = writeContent(block, tag), `\\describe` = {
                blankLine(0L)
                writeContent(block, tag)
                blankLine()
            }, `\\itemize` = , `\\enumerate` = {
                blankLine(0L)
                enumItem0 <- enumItem
                enumItem <<- 0L
                indent0 <- indent
                opts <- Rd2txt_options()
                indent <<- max(opts$minIndent, indent + opts$extraIndent)
                dropBlank <<- TRUE
                writeContent(block, tag)
                blankLine()
                indent <<- indent0
                enumItem <<- enumItem0
            }, `\\code` = , `\\command` = , `\\env` = , `\\file` = , 
            `\\kbd` = , `\\option` = , `\\pkg` = , `\\samp` = {
                opts <- Rd2txt_options()
                if (opts$code_quote) writeQ(block, tag, quote = "\\sQuote") else writeContent(block, 
                  tag)
            }, `\\email` = {
                put("<email: ", trimws(gsub("\n", "", paste(as.character(block), 
                  collapse = ""))), ">")
            }, `\\url` = {
                put("<URL: ", trimws(gsub("\n", "", paste(as.character(block), 
                  collapse = ""))), ">")
            }, `\\href` = {
                opts <- Rd2txt_options()
                writeContent(block[[2L]], tag)
                if (opts$showURLs) put(" (URL: ", trimws(gsub("\n", 
                  "", paste(as.character(block[[1L]]), collapse = ""))), 
                  ")")
            }, `\\Sexpr` = put(as.character.Rd(block, deparse = TRUE)), 
            `\\acronym` = , `\\cite` = , `\\dfn` = , `\\special` = , 
            `\\var` = writeContent(block, tag), `\\bold` = , 
            `\\strong` = {
                put("*")
                writeContent(block, tag)
                put("*")
            }, `\\emph` = {
                put("_")
                writeContent(block, tag)
                put("_")
            }, `\\sQuote` = , `\\dQuote` = writeQ(block, tag), 
            `\\preformatted` = {
                putf("\n")
                writeCodeBlock(block, tag)
            }, `\\verb` = put(block), `\\linkS4class` = , `\\link` = writeContent(block, 
                tag), `\\cr` = {
                flushBuffer()
                dropBlank <<- TRUE
            }, `\\dots` = , `\\ldots` = put("..."), `\\R` = put("R"), 
            `\\enc` = {
                txt <- as.character(block[[1L]])
                test <- iconv(txt, "UTF-8", outputEncoding, mark = FALSE)
                txt <- if (!is.na(test)) txt else as.character(block[[2L]])
                put(txt)
            }, `\\eqn` = {
                block <- block[[length(block)]]
                inEqn0 <- inEqn
                inEqn <<- TRUE
                writeContent(block, tag)
                inEqn <<- inEqn0
            }, `\\deqn` = {
                blankLine()
                block <- block[[length(block)]]
                save <- startCapture()
                inEqn <<- TRUE
                writeContent(block, tag)
                eqn <- endCapture(save)
                eqn <- frmt(eqn, justify = "centre", width = WIDTH - 
                  indent)
                putf(paste(eqn, collapse = "\n"))
                blankLine()
            }, `\\figure` = {
                blankLine()
                save <- startCapture()
                writeContent(block[[length(block)]], tag)
                alt <- endCapture(save)
                if (length(alt)) {
                  alt <- frmt(alt, justify = "centre", width = WIDTH - 
                    indent)
                  putf(paste(alt, collapse = "\n"))
                  blankLine()
                }
            }, `\\tabular` = writeTabular(block), `\\subsection` = writeSection(block, 
                tag), `\\if` = , `\\ifelse` = if (testRdConditional("text", 
                block, Rdfile)) writeContent(block[[2L]], tag) else if (tag == 
                "\\ifelse") writeContent(block[[3L]], tag), `\\out` = for (i in seq_along(block)) put(block[[i]]), 
            stopRd(block, Rdfile, "Tag ", tag, " not recognized"))
    }
    writeTabular <- function(table) {
        formats <- table[[1L]]
        content <- table[[2L]]
        if (length(formats) != 1L || RdTags(formats) != "TEXT") 
            stopRd(table, Rdfile, "\\tabular format must be simple text")
        formats <- strsplit(formats[[1L]], "", fixed = TRUE)[[1L]]
        tags <- RdTags(content)
        entries <- list()
        row <- 1L
        col <- 1L
        save <- startCapture()
        dropBlank <<- TRUE
        newEntry <- function() {
            entries <<- c(entries, list(list(text = trim(endCapture(save)), 
                row = row, col = col)))
            save <<- startCapture()
            dropBlank <<- TRUE
        }
        for (i in seq_along(tags)) {
            switch(tags[i], `\\tab` = {
                newEntry()
                col <- col + 1
                if (col > length(formats)) stopRd(content[[i]], 
                  Rdfile, sprintf("too many columns for format '%s'", 
                    table[[1L]]))
            }, `\\cr` = {
                newEntry()
                row <- row + 1L
                col <- 1L
            }, writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        newEntry()
        endCapture(save)
        entries <- with(entries[[length(entries)]], {
            if (!length(text) && col == 1L) 
                entries[-length(entries)]
            else entries
        })
        rows <- entries[[length(entries)]]$row
        cols <- max(sapply(entries, function(e) e$col))
        widths <- rep_len(0L, cols)
        lines <- rep_len(1L, rows)
        for (i in seq_along(entries)) {
            e <- entries[[i]]
            while (length(e$text) && !nzchar(e$text[length(e$text)])) {
                e$text <- e$text[-length(e$text)]
                entries[[i]] <- e
            }
            if (any(nzchar(e$text))) 
                widths[e$col] <- max(widths[e$col], max(nchar(e$text, 
                  "w")))
            lines[e$row] <- max(lines[e$row], length(e$text))
        }
        result <- matrix("", sum(lines), cols)
        for (i in seq_len(cols)) result[, i] <- strrep(" ", widths[i])
        firstline <- c(1L, 1L + cumsum(lines))
        for (i in seq_along(entries)) {
            e <- entries[[i]]
            if (!length(e$text)) 
                next
            text <- frmt(e$text, justify = formats[e$col], width = widths[e$col])
            for (j in seq_along(text)) result[firstline[e$row] + 
                j - 1L, e$col] <- text[j]
        }
        blankLine()
        indent0 <- indent
        indent <<- indent + 1L
        for (i in seq_len(nrow(result))) {
            putf(paste0(" ", result[i, ], " ", collapse = ""))
            putf("\n")
        }
        blankLine()
        indent <<- indent0
    }
    writeCodeBlock <- function(blocks, blocktag) {
        tags <- RdTags(blocks)
        i <- 0
        while (i < length(tags)) {
            i <- i + 1
            block <- blocks[[i]]
            tag <- tags[i]
            switch(tag, `\\method` = , `\\S3method` = , `\\S4method` = {
                blocks <- transformMethod(i, blocks, Rdfile)
                tags <- RdTags(blocks)
                i <- i - 1
            }, UNKNOWN = , VERB = , RCODE = , TEXT = writeCode(tabExpand(block)), 
                `\\donttest` = , `\\special` = , `\\var` = writeCodeBlock(block, 
                  tag), `\\dots` = , `\\ldots` = put("..."), 
                `\\dontrun` = writeDR(block, tag), USERMACRO = , 
                `\\newcommand` = , `\\renewcommand` = , COMMENT = , 
                `\\dontshow` = , `\\testonly` = {
                }, stopRd(block, Rdfile, "Tag ", tag, " not expected in code block"))
        }
    }
    writeContent <- function(blocks, blocktag) {
        itemskip <- FALSE
        tags <- RdTags(blocks)
        for (i in seq_along(tags)) {
            tag <- tags[i]
            block <- blocks[[i]]
            switch(tag, `\\item` = {
                switch(blocktag, `\\describe` = {
                  blankLine()
                  save <- startCapture()
                  dropBlank <<- TRUE
                  writeContent(block[[1L]], tag)
                  DLlab <- endCapture(save)
                  indent0 <- indent
                  opts <- Rd2txt_options()
                  indent <<- max(opts$minIndent, indent + opts$extraIndent)
                  keepFirstIndent <<- TRUE
                  putw(strrep(" ", indent0), frmt(paste0(DLlab), 
                    justify = "left", width = indent), " ")
                  writeContent(block[[2L]], tag)
                  blankLine(0L)
                  indent <<- indent0
                }, `\\value` = , `\\arguments` = {
                  blankLine()
                  save <- startCapture()
                  dropBlank <<- TRUE
                  writeContent(block[[1L]], tag)
                  DLlab <- endCapture(save)
                  indent0 <- indent
                  opts <- Rd2txt_options()
                  indent <<- max(opts$minIndent, indent + opts$extraIndent)
                  keepFirstIndent <<- TRUE
                  putw(frmt(paste0(DLlab, ": "), justify = "right", 
                    width = indent))
                  writeContent(block[[2L]], tag)
                  blankLine(0L)
                  indent <<- indent0
                }, `\\itemize` = , `\\enumerate` = {
                  blankLine()
                  keepFirstIndent <<- TRUE
                  opts <- Rd2txt_options()
                  if (blocktag == "\\itemize") label <- opts$itemBullet else {
                    enumItem <<- enumItem + 1L
                    label <- opts$enumFormat(enumItem)
                  }
                  putw(frmt(label, justify = "right", width = indent))
                })
                itemskip <- TRUE
            }, {
                if (itemskip) {
                  itemskip <- FALSE
                  if (tag == "TEXT") {
                    txt <- psub("^ ", "", as.character(tabExpand(block)))
                    put(txt)
                  } else writeBlock(block, tag, blocktag)
                } else writeBlock(block, tag, blocktag)
            })
        }
    }
    writeSection <- function(section, tag) {
        if (tag %in% c("\\alias", "\\concept", "\\encoding", 
            "\\keyword")) 
            return()
        save <- c(indent, sectionLevel, keepFirstIndent, dropBlank, 
            wrapping)
        blankLine(min(sectionLevel, 1L))
        titlePrefix <- strrep("  ", sectionLevel)
        opts <- Rd2txt_options()
        indent <<- opts$sectionIndent + opts$sectionExtra * sectionLevel
        sectionLevel <<- sectionLevel + 1
        keepFirstIndent <<- TRUE
        if (tag == "\\section" || tag == "\\subsection") {
            title <- .Rd_format_title(.Rd_get_text(section[[1L]]))
            putf(titlePrefix, txt_header(title), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- TRUE
            keepFirstIndent <<- FALSE
            writeContent(section[[2L]], tag)
        }
        else if (tag %in% c("\\usage", "\\examples")) {
            putf(txt_header(sectionTitles[tag]), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- FALSE
            keepFirstIndent <<- FALSE
            writeCodeBlock(section, tag)
        }
        else {
            putf(txt_header(sectionTitles[tag]), ":")
            blankLine()
            dropBlank <<- TRUE
            wrapping <<- TRUE
            keepFirstIndent <<- FALSE
            writeContent(section, tag)
        }
        blankLine()
        indent <<- save[1L]
        sectionLevel <<- save[2L]
        keepFirstIndent <<- save[3L]
        dropBlank <<- save[4L]
        wrapping <<- save[5L]
    }
    if (is.character(out)) {
        if (out == "") {
            con <- stdout()
        }
        else {
            con <- file(out, "wt")
            on.exit(close(con), add = TRUE)
        }
    }
    else {
        con <- out
        out <- summary(con)$description
    }
    Rd <- prepare_Rd(Rd, defines = defines, stages = stages, 
        fragment = fragment, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)
    if (fragment) {
        if (sections[1L] %in% names(sectionOrder)) 
            for (i in seq_along(sections)) writeSection(Rd[[i]], 
                sections[i])
        else for (i in seq_along(sections)) writeBlock(Rd[[i]], 
            sections[i], "")
    }
    else {
        title <- .Rd_format_title(.Rd_get_title(Rd))
        name <- trim(Rd[[2L]][[1L]])
        if (nzchar(package)) {
            left <- name
            mid <- if (nzchar(package)) 
                paste0("package:", package)
            else ""
            right <- "R Documentation"
            if (encoding != "unknown") 
                right <- paste0(right, "(", encoding, ")")
            pad <- max(HDR_WIDTH - nchar(left, "w") - nchar(mid, 
                "w") - nchar(right, "w"), 0)
            pad0 <- pad%/%2L
            pad1 <- strrep(" ", pad0)
            pad2 <- strrep(" ", pad - pad0)
            putf(paste0(left, pad1, mid, pad2, right, "\n\n"))
        }
        putf(txt_header(title))
        blankLine()
        for (i in seq_along(sections)[-(1:2)]) writeSection(Rd[[i]], 
            sections[i])
    }
    blankLine(0L)
    invisible(out)
}


summarize_check_packages_in_dir_timings <- function (dir, all = FALSE, full = FALSE) 
{
    dir <- normalizePath(dir)
    tfile <- file.path(dir, "timings.tab")
    if (file_test("-f", tfile)) {
        timings <- utils::read.table(tfile)
        if (!all) {
            rdepends <- Sys.glob(file.path(dir, "rdepends_*.Rcheck"))
            timings <- timings[is.na(match(rownames(timings), 
                sub("rdepends_(.*).Rcheck", "\\1", basename(rdepends)))), 
                ]
        }
        print(timings)
    }
    if (full) {
        tfiles <- Sys.glob(file.path(R_check_outdirs(dir, all = all), 
            "*-Ex.timings"))
        if (length(tfiles)) 
            message("")
        timings <- lapply(tfiles, utils::read.table, header = TRUE)
        timings <- lapply(timings, function(x) x[order(x$user, 
            decreasing = TRUE), ])
        timings <- split(as.data.frame(lapply(do.call(rbind, 
            timings), format)), rep.int(sub("\\.Rcheck$", "", 
            basename(dirname(tfiles))), sapply(timings, nrow)))
        invisible(Map(function(x, y) {
            writeLines(sprintf("Example timings for package '%s':", 
                x))
            cat(rbind(" ", t(as.matrix(y))), sep = c(" ", " ", 
                " ", " ", "\n"))
        }, names(timings), timings))
    }
    invisible()
}


pkgDepends <- function (pkg, recursive = TRUE, local = TRUE, reduce = TRUE, 
    lib.loc = NULL) 
{
    if (length(pkg) != 1L) 
        stop("argument 'pkg' must be of length 1")
    .Deprecated("package_dependencies()")
    instPkgs <- utils::installed.packages(lib.loc = lib.loc)
    depMtrx <- getDepMtrx(pkg, instPkgs, local)
    if (is.null(depMtrx)) 
        stop(gettextf("package '%s' was not found", pkg), domain = NA)
    getDepList(depMtrx, instPkgs, recursive, local, reduce)
}


Rdindex <- function (RdFiles, outFile = "", type = NULL, width = 0.9 * getOption("width"), 
    indent = NULL) 
{
    if ((length(RdFiles) == 1L) && dir.exists(RdFiles)) {
        docsDir <- RdFiles
        if (dir.exists(file.path(docsDir, "man"))) 
            docsDir <- file.path(docsDir, "man")
        RdFiles <- list_files_with_type(docsDir, "docs")
    }
    if (outFile == "") 
        outFile <- stdout()
    else if (is.character(outFile)) {
        outFile <- file(outFile, "w")
        on.exit(close(outFile))
    }
    if (!inherits(outFile, "connection")) 
        stop("argument 'outFile' must be a character string or connection")
    db <- .build_Rd_db(files = RdFiles, stages = "build")
    index <- .build_Rd_index(Rd_contents(db), type = type)
    writeLines(formatDL(index, width = width, indent = indent), 
        outFile)
}


getVignetteInfo <- function (package = NULL, lib.loc = NULL, all = TRUE) 
{
    if (is.null(package)) {
        package <- .packages(all.available = all, lib.loc)
        paths <- find.package(package, lib.loc, quiet = TRUE)
    }
    else paths <- find.package(package, lib.loc)
    paths <- paths[dir.exists(file.path(paths, "doc"))]
    empty <- cbind(Package = character(0), Dir = character(0), 
        Topic = character(0), File = character(0), Title = character(0), 
        R = character(0), PDF = character(0))
    getVinfo <- function(dir) {
        entries <- NULL
        if (file.exists(INDEX <- file.path(dir, "Meta", "vignette.rds"))) 
            entries <- readRDS(INDEX)
        if (NROW(entries) > 0) {
            if (is.null(entries$R)) 
                R <- rep("", NROW(entries))
            else R <- entries$R
            file <- basename(entries$File)
            pdf <- entries$PDF
            topic <- file_path_sans_ext(ifelse(R == "", ifelse(pdf == 
                "", file, pdf), R))
            cbind(Package = basename(dir), Dir = dir, Topic = topic, 
                File = file, Title = entries$Title, R = R, PDF = pdf)[order(entries$Title), 
                , drop = FALSE]
        }
        else empty
    }
    if (length(paths)) 
        do.call(rbind, lapply(paths, getVinfo))
    else empty
}


vignetteEngine <- function (name, weave, tangle, pattern = NULL, package = NULL, 
    aspell = list()) 
{
    if (missing(weave)) {
        getEngine(name, package)
    }
    else {
        if (is.element(name, c("Sweave", "utils::Sweave"))) {
            stop(gettextf("Cannot change the %s engine or use an engine of that name", 
                sQuote("Sweave")), domain = NA)
        }
        if (missing(package)) 
            package <- utils::packageName(parent.frame())
        result <- setEngine(name, package, pattern = pattern, 
            weave = weave, tangle = tangle, aspell = aspell)
        invisible(result)
    }
}


Rd_db <- function (package, dir, lib.loc = NULL) 
{
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        docs_dir <- file.path(dir, "man")
        db_file <- file.path(dir, "help", package)
        if (file_test("-f", paste(db_file, "rdx", sep = "."))) {
            db <- fetchRdDB(db_file)
            pathfile <- file.path(dir, "help", "paths.rds")
            if (file.exists(pathfile)) {
                paths <- readRDS(pathfile)
                if (!is.null(first <- attr(paths, "first"))) 
                  paths <- substring(paths, first)
                names(db) <- paths
            }
            return(db)
        }
        db_file <- file.path(docs_dir, sprintf("%s.Rd.gz", package))
        if (file_test("-f", db_file)) {
            lines <- .read_Rd_lines_quietly(db_file)
            eof_pos <- grep("^\\\\eof$", lines, perl = TRUE, 
                useBytes = TRUE)
            db <- split(lines[-eof_pos], rep(seq_along(eof_pos), 
                times = diff(c(0, eof_pos)))[-eof_pos])
        }
        else return(structure(list(), names = character()))
        paths <- as.character(sapply(db, "[", 1L))
        names(db) <- if (length(paths) && all(grepl("^% --- Source file: (.+) ---$", 
            paths))) 
            sub("^% --- Source file: (.+) ---$", "\\1", paths)
        else NULL
        encoding <- .get_package_metadata(dir, TRUE)["Encoding"]
        if (is.na(encoding)) 
            encoding <- "unknown"
        db <- suppressWarnings(lapply(db, prepare_Rd_from_Rd_lines, 
            encoding = encoding, defines = .Platform$OS.type, 
            stages = "install"))
    }
    else {
        if (missing(dir)) 
            stop("you must specify 'package' or 'dir'")
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        else dir <- file_path_as_absolute(dir)
        built_file <- file.path(dir, "build", "partial.rdb")
        db <- .build_Rd_db(dir, stages = "build", built_file = built_file)
        if (length(db)) {
            first <- nchar(file.path(dir, "man")) + 2L
            names(db) <- substring(names(db), first)
        }
    }
    db
}


toHTML <- function (x, ...) 
UseMethod("toHTML")


make_translations_pkg <- function (srcdir, outDir = ".", append = "-1") 
{
    src <- file.path(srcdir, "src/library/translations")
    dest <- file.path(tempdir(), "translations")
    dir.create(dest, FALSE)
    file.copy(file.path(src, "inst"), dest, recursive = TRUE)
    lines <- readLines(file.path(src, "DESCRIPTION.in"))
    ver <- getRversion()
    lines <- gsub("@VERSION@", ver, lines, fixed = TRUE)
    lines[2] <- paste0(lines[2], append)
    ver <- unclass(getRversion())[[1]]
    deps <- sprintf("Depends: R (>= %s.%d.0), R (< %d.%d.0)", 
        ver[1], ver[2], ver[1], ver[2] + 1)
    lines <- c(lines, deps)
    writeLines(lines, file.path(dest, "DESCRIPTION"))
    cmd <- file.path(R.home(), "bin", "R")
    cmd <- paste(cmd, "CMD", "build", shQuote(dest))
    if (system(cmd) != 0L) 
        stop("R CMD build failed")
    tarball <- Sys.glob(file.path(tempdir(), "translations_*.tar.gz"))
    file.rename(tarball, file.path(outDir, basename(tarball)))
    invisible()
}


checkDocStyle <- function (package, dir, lib.loc = NULL) 
{
    has_namespace <- FALSE
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if (file_test("-f", dfile)) 
            .read_description(dfile)
        else character()
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        if (!.haveRds(dir)) 
            stop(gettextf("directory '%s' does not contain Rd objects", 
                dir), domain = NA)
        package_name <- package
        is_base <- package_name == "base"
        if (!is_base) 
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)
        objects_in_code <- sort(names(code_env))
        if (packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            ns_S3_methods_db <- getNamespaceInfo(package, "S3methods")
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }
    }
    else {
        if (missing(dir)) 
            stop("you must specify 'package' or 'dir'")
        package_name <- basename(dir)
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        else dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        if (!.haveRds(dir)) 
            stop(gettextf("directory '%s' does not contain Rd objects", 
                dir), domain = NA)
        is_base <- package_name == "base"
        code_env <- new.env(hash = TRUE)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if (file_test("-f", dfile)) 
            .read_description(dfile)
        else character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if (file_test("-f", sys_data_file)) 
            load(sys_data_file, code_env)
        objects_in_code <- sort(names(code_env))
        if (file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(package_name, dirname(dir))
            OK <- intersect(objects_in_code, nsInfo$exports)
            for (p in nsInfo$exportPatterns) OK <- c(OK, grep(p, 
                objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }
    }
    functions_in_code <- Filter(function(f) is.function(get(f, 
        envir = code_env)), objects_in_code)
    all_S3_generics <- unique(c(Filter(function(f) .is_S3_generic(f, 
        envir = code_env), functions_in_code), .get_S3_generics_as_seen_from_package(dir, 
        !missing(package), TRUE), .get_S3_group_generics()))
    code_env <- .make_S3_group_generic_env(parent = code_env)
    methods_stop_list <- nonS3methods(package_name)
    methods_in_package <- sapply(all_S3_generics, function(g) {
        if (!exists(g, envir = code_env)) 
            return(character())
        name <- paste0(g, ".")
        methods <- functions_in_code[substr(functions_in_code, 
            1L, nchar(name, type = "c")) == name]
        methods <- setdiff(methods, methods_stop_list)
        if (has_namespace) {
            methods2 <- ns_S3_methods[ns_S3_generics == g]
            OK <- substr(methods2, 1L, nchar(name, type = "c")) == 
                name
            methods <- c(methods, methods2[OK])
        }
        methods
    })
    all_methods_in_package <- unlist(methods_in_package)
    if (package_name == "cluster") 
        all_methods_in_package <- setdiff(all_methods_in_package, 
            functions_in_code)
    db <- if (!missing(package)) 
        Rd_db(package, lib.loc = dirname(dir))
    else Rd_db(dir = dir)
    names(db) <- db_names <- .Rd_get_names_from_Rd_db(db)
    ind <- db_names %in% paste(package_name, c("deprecated", 
        "defunct"), sep = "-")
    db <- db[!ind]
    db_names <- db_names[!ind]
    db_usages <- lapply(db, function(Rd) {
        Rd <- .Rd_get_section(Rd, "usage")
        .parse_usage_as_much_as_possible(Rd)
    })
    ind <- as.logical(sapply(db_usages, function(x) !is.null(attr(x, 
        "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")
    bad_doc_objects <- list()
    for (docObj in db_names) {
        exprs <- db_usages[[docObj]]
        exprs <- exprs[lengths(exprs) > 1L]
        functions <- as.character(sapply(exprs, function(e) as.character(e[[1L]])))
        ind <- as.logical(sapply(exprs, .is_call_from_replacement_function_usage))
        if (any(ind)) {
            replace_funs <- paste0(sapply(exprs[ind], function(e) as.character(e[[2L]][[1L]])), 
                "<-")
            functions <- c(functions, replace_funs)
        }
        methods_with_full_name <- intersect(functions, all_methods_in_package)
        functions <- .transform_S3_method_markup(functions)
        methods_with_generic <- sapply(intersect(functions, all_S3_generics), 
            function(g) intersect(functions, methods_in_package[[g]]), 
            simplify = FALSE)
        if ((length(methods_with_generic)) || (length(methods_with_full_name))) 
            bad_doc_objects[[docObj]] <- list(withGeneric = methods_with_generic, 
                withFullName = methods_with_full_name)
    }
    attr(bad_doc_objects, "bad_lines") <- bad_lines
    class(bad_doc_objects) <- "checkDocStyle"
    bad_doc_objects
}


assertWarning <- function (expr, verbose = FALSE) 
{
    d.expr <- .deparseTrim(substitute(expr), cutoff = 30L)
    res <- assertCondition(expr, "warning", .exprString = d.expr)
    if (any(sapply(res, function(cond) "error" %in% class(cond)))) 
        stop(gettextf("Got warning in evaluating %s, but also an error", 
            d.expr))
    if (verbose) {
        warning <- res[sapply(res, function(cond) "warning" %in% 
            class(cond))]
        message(sprintf("Asserted warning: %s", warning[[1]]$message))
    }
    invisible(res)
}


SIGINT <- 2L


showNonASCIIfile <- function (file) 
showNonASCII(readLines(file, warn = FALSE))


pskill <- function (pid, signal = SIGTERM) 
invisible(.Call(ps_kill, pid, signal))


.print.via.format <- function (x, ...) 
{
    writeLines(format(x, ...))
    invisible(x)
}


checkReplaceFuns <- function (package, dir, lib.loc = NULL) 
{
    has_namespace <- FALSE
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        is_base <- basename(dir) == "base"
        if (!is_base) 
            .load_package_quietly(package, lib.loc)
        if (packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            code_env <- asNamespace(package)
            ns_S3_methods_db <- .getNamespaceInfo(code_env, "S3methods")
        }
        else code_env <- .package_env(package)
    }
    else {
        if (missing(dir)) 
            stop("you must specify 'package' or 'dir'")
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        else dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        is_base <- basename(dir) == "base"
        code_env <- new.env(hash = TRUE)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if (file_test("-f", dfile)) 
            .read_description(dfile)
        else character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if (file_test("-f", sys_data_file)) 
            load(sys_data_file, code_env)
        if (file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
        }
    }
    objects_in_code <- sort(names(code_env))
    replace_funs <- character()
    if (has_namespace) {
        ns_S3_generics <- ns_S3_methods_db[, 1L]
        ns_S3_methods <- ns_S3_methods_db[, 3L]
        idx <- grep("<-$", ns_S3_generics)
        if (length(idx)) 
            replace_funs <- ns_S3_methods[idx]
        objects_in_code <- setdiff(objects_in_code, ns_S3_methods)
    }
    replace_funs <- c(replace_funs, grep("<-", objects_in_code, 
        value = TRUE))
    .check_last_formal_arg <- function(f) {
        arg_names <- names(formals(f))
        if (!length(arg_names)) 
            TRUE
        else identical(arg_names[length(arg_names)], "value")
    }
    bad_replace_funs <- if (length(replace_funs)) {
        Filter(function(f) {
            f <- get(f, envir = code_env)
            if (!is.function(f)) 
                return(FALSE)
            !.check_last_formal_arg(f)
        }, replace_funs)
    }
    else character()
    if (.isMethodsDispatchOn()) {
        S4_generics <- .get_S4_generics(code_env)
        S4_generics <- S4_generics[grepl("<-$", names(S4_generics))]
        bad_S4_replace_methods <- sapply(S4_generics, function(f) {
            mlist <- .get_S4_methods_list(f, code_env)
            ind <- !as.logical(sapply(mlist, .check_last_formal_arg))
            if (!any(ind)) 
                character()
            else {
                sigs <- .make_siglist(mlist[ind])
                sprintf("\\S4method{%s}{%s}", f, sigs)
            }
        })
        bad_replace_funs <- c(bad_replace_funs, unlist(bad_S4_replace_methods, 
            use.names = FALSE))
    }
    class(bad_replace_funs) <- "checkReplaceFuns"
    bad_replace_funs
}


check_packages_in_dir <- function (dir, check_args = character(), check_args_db = list(), 
    reverse = NULL, check_env = character(), xvfb = FALSE, Ncpus = getOption("Ncpus", 
        1L), clean = TRUE, ...) 
{
    owd <- getwd()
    dir <- normalizePath(dir)
    setwd(dir)
    on.exit(setwd(owd))
    .check_packages_in_dir_retval <- function(dir, pfiles, pnames = character(), 
        rnames = character()) {
        structure(pfiles, dir = dir, pnames = pnames, rnames = rnames, 
            class = "check_packages_in_dir")
    }
    pfiles <- Sys.glob("*.tar.gz")
    if (!length(pfiles)) {
        message("no packages to check")
        return(.check_packages_in_dir_retval(dir, pfiles))
    }
    pnames <- sub("_.*", "", pfiles)
    os_type <- .Platform$OS.type
    xvfb_options <- "-screen 0 1280x1024x24"
    if (os_type == "windows") 
        xvfb <- FALSE
    else if (is.logical(xvfb)) {
        if (!identical(xvfb, TRUE)) 
            xvfb <- FALSE
    }
    else {
        xvfb_options <- as.character(xvfb)
        xvfb <- TRUE
    }
    curl <- if (os_type == "windows") 
        sprintf("file:///%s", dir)
    else sprintf("file://%s", dir)
    libdir <- file.path(dir, "Library")
    dir.create(libdir, showWarnings = FALSE)
    outdir <- file.path(dir, "Outputs")
    dir.create(outdir, showWarnings = FALSE)
    pnames_using_install_no <- character()
    pnames_using_install_fake <- character()
    check_args_db <- as.list(check_args_db)
    if (length(check_args_db) && !is.null(nms <- names(check_args_db))) {
        args <- lapply(check_args_db, function(e) scan(text = e, 
            what = character(), quiet = TRUE))
        pnames_using_install_no <- nms[sapply(args, function(e) any(e == 
            "--install=no"))]
        pnames_using_install_fake <- nms[sapply(args, function(e) any(e == 
            "--install=fake"))]
    }
    else {
        check_args_db <- list()
    }
    write_PACKAGES(dir, type = "source")
    if (dir.exists(depdir <- file.path(dir, "Depends"))) {
        write_PACKAGES(depdir, type = "source")
        curl <- c(curl, paste0(curl, "/Depends"))
    }
    localones <- utils::available.packages(contriburl = curl, 
        type = "source")
    curls <- utils::contrib.url(getOption("repos"), type = "source")
    available <- utils::available.packages(contriburl = curls, 
        type = "source")
    available <- rbind(localones, available)
    available <- available[!duplicated(available[, "Package"]), 
        , drop = FALSE]
    curls <- c(curl, curls)
    pnames_using_install_no <- c(pnames_using_install_no, setdiff(pnames_using_install_fake, 
        available[, "Package"]))
    pnames_using_install_fake <- intersect(pnames_using_install_fake, 
        available[, "Package"])
    if (!is.null(reverse) && !identical(reverse, FALSE)) {
        reverse <- as.list(reverse)
        defaults <- list(which = c("Depends", "Imports", "LinkingTo"), 
            recursive = FALSE, repos = getOption("repos"))
        pos <- pmatch(names(reverse), names(defaults), nomatch = 0L)
        defaults[pos] <- reverse[pos > 0L]
        rnames <- if (is.list(defaults$which)) {
            defaults$recursive <- rep_len(as.list(defaults$recursive), 
                length(defaults$which))
            unlist(Map(function(w, r) package_dependencies(setdiff(pnames, 
                pnames_using_install_no), available, which = w, 
                recursive = r, reverse = TRUE), defaults$which, 
                defaults$recursive), use.names = FALSE)
        }
        else {
            package_dependencies(setdiff(pnames, pnames_using_install_no), 
                available, which = defaults$which, recursive = defaults$recursive, 
                reverse = TRUE)
        }
        rnames <- intersect(unlist(rnames, use.names = FALSE), 
            available[, "Package"])
        rnames <- setdiff(rnames, pnames)
        pos <- match(rnames, available[, "Package"], nomatch = 0L)
        if (!identical(defaults$repos, getOption("repos"))) {
            pos <- split(pos[pos > 0L], available[pos, "Repository"])
            nms <- names(pos)
            pos <- unlist(pos[unique(c(outer(defaults$repos, 
                nms, pmatch, nomatch = 0L)))], use.names = FALSE)
        }
        rnames <- available[pos, "Package"]
        rfiles <- sprintf("%s_%s.tar.gz", rnames, available[pos, 
            "Version"])
        if (length(rfiles)) {
            message("downloading reverse dependencies ...")
            rfurls <- sprintf("%s/%s", available[pos, "Repository"], 
                rfiles)
            for (i in seq_along(rfiles)) {
                message(sprintf("downloading %s ... ", rfiles[i]), 
                  appendLF = FALSE)
                status <- if (!utils::download.file(rfurls[i], 
                  rfiles[i], quiet = TRUE)) 
                  "ok"
                else "failed"
                message(status)
            }
            message("")
        }
    }
    else {
        rfiles <- rnames <- character()
    }
    pnames <- c(pnames, rnames)
    if (xvfb) {
        pid <- start_virtual_X11_fb(xvfb_options)
        on.exit(close_virtual_X11_db(pid), add = TRUE)
    }
    depends <- package_dependencies(pnames, available, which = "most")
    depends <- setdiff(unique(unlist(depends, use.names = FALSE)), 
        .get_standard_package_names()$base)
    libs <- c(libdir, .libPaths())
    installed <- utils::installed.packages(libs)[, "Package"]
    depends <- c(setdiff(depends, installed), intersect(intersect(depends, 
        installed), utils::old.packages(libs, available = available)[, 
        "Package"]))
    if (length(depends)) {
        message(paste(strwrap(sprintf("installing dependencies %s", 
            paste(sQuote(sort(depends)), collapse = ", ")), exdent = 2L), 
            collapse = "\n"), domain = NA)
        message("")
        iflags <- as.list(rep.int("--fake", length(pnames_using_install_fake)))
        names(iflags) <- pnames_using_install_fake
        tmpdir <- tempfile(tmpdir = outdir)
        dir.create(tmpdir)
        utils::install.packages(depends, lib = libdir, contriburl = curls, 
            available = available, dependencies = NA, INSTALL_opts = iflags, 
            keep_outputs = tmpdir, Ncpus = Ncpus, type = "source")
        outfiles <- Sys.glob(file.path(tmpdir, "*.out"))
        file.rename(outfiles, file.path(outdir, sprintf("install_%s", 
            basename(outfiles))))
        unlink(tmpdir, recursive = TRUE)
        message("")
    }
    check_args <- if (is.list(check_args)) {
        c(rep.int(list(check_args[[1L]]), length(pfiles)), rep.int(list(check_args[[2L]]), 
            length(rfiles)))
    }
    else {
        rep.int(list(check_args), length(pnames))
    }
    check_args_db <- check_args_db[pnames]
    check_args_db <- Map(c, check_args, check_args_db)
    names(check_args_db) <- pnames
    check_env <- if (is.list(check_env)) {
        c(rep.int(list(check_env[[1L]]), length(pfiles)), rep.int(list(check_env[[2L]]), 
            length(rfiles)))
    }
    else {
        rep.int(list(check_env), length(pnames))
    }
    check_env_db <- as.list(check_env)
    names(check_env_db) <- pnames
    pfiles <- c(pfiles, rfiles)
    check_package <- function(pfile, args_db = NULL, env_db = NULL) {
        message(sprintf("checking %s ...", pfile))
        pname <- sub("_.*", "", basename(pfile))
        out <- file.path(outdir, sprintf("check_%s_stdout.txt", 
            pname))
        err <- file.path(outdir, sprintf("check_%s_stderr.txt", 
            pname))
        env <- c(check_env_db[[pname]], sprintf("R_LIBS=%s", 
            shQuote(libdir)))
        system.time(system2(file.path(R.home("bin"), "R"), c("CMD", 
            "check", "--timings", args_db[[pname]], pfile), stdout = out, 
            stderr = err, env = env))
    }
    if (Ncpus > 1L) {
        if (os_type != "windows") {
            timings <- parallel::mclapply(pfiles, check_package, 
                check_args_db, check_env_db, mc.cores = Ncpus)
        }
        else {
            cl <- parallel::makeCluster(Ncpus)
            timings <- parallel::parLapply(cl, pfiles, check_package, 
                check_args_db, check_env_db)
            parallel::stopCluster(cl)
        }
    }
    else {
        timings <- lapply(pfiles, check_package, check_args_db, 
            check_env_db)
    }
    timings <- do.call(rbind, lapply(timings, summary))
    rownames(timings) <- pnames
    utils::write.table(timings, "timings.tab")
    file.rename(sprintf("%s.Rcheck", rnames), sprintf("rdepends_%s.Rcheck", 
        rnames))
    if (clean) {
        file.remove(rfiles)
    }
    else {
        file.rename(rfiles, sprintf("rdepends_%s", rfiles))
    }
    .check_packages_in_dir_retval(dir, pfiles, setdiff(pnames, 
        rnames), rnames)
}


checkS3methods <- function (package, dir, lib.loc = NULL) 
{
    has_namespace <- FALSE
    S3_reg <- character()
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        is_base <- basename(dir) == "base"
        if (!is_base) 
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)
        objects_in_code <- sort(names(code_env))
        if (packageHasNamespace(package, dirname(dir))) {
            has_namespace <- TRUE
            ns_S3_methods_db <- getNamespaceInfo(package, "S3methods")
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- paste(ns_S3_generics, ns_S3_methods_db[, 
                2L], sep = ".")
            S3_reg <- setdiff(ns_S3_methods, objects_in_code)
        }
    }
    else {
        if (missing(dir)) 
            stop("you must specify 'package' or 'dir'")
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        else dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        is_base <- basename(dir) == "base"
        code_env <- new.env(hash = TRUE)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if (file_test("-f", dfile)) 
            .read_description(dfile)
        else character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if (file_test("-f", sys_data_file)) 
            load(sys_data_file, code_env)
        objects_in_code <- sort(names(code_env))
        if (file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            nsInfo <- parseNamespaceFile(basename(dir), dirname(dir))
            OK <- intersect(objects_in_code, nsInfo$exports)
            for (p in nsInfo$exportPatterns) OK <- c(OK, grep(p, 
                objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
            ns_S3_methods_db <- .get_namespace_S3_methods_db(nsInfo)
            ns_S3_generics <- ns_S3_methods_db[, 1L]
            ns_S3_methods <- ns_S3_methods_db[, 3L]
        }
    }
    functions_in_code <- Filter(function(f) is.function(code_env[[f]]), 
        objects_in_code)
    S3_group_generics <- .get_S3_group_generics()
    S3_primitive_generics <- .get_S3_primitive_generics()
    checkArgs <- function(g, m) {
        if (identical(g, "round") && m == "round.POSIXt") 
            return()
        genfun <- get(g, envir = code_env)
        gArgs <- names(formals(genfun))
        if (identical(g, "plot")) 
            gArgs <- gArgs[-2L]
        ogArgs <- gArgs
        gm <- if (m %in% S3_reg) {
            defenv <- if (g %in% S3_group_generics || g %in% 
                S3_primitive_generics) 
                .BaseNamespaceEnv
            else {
                if (.isMethodsDispatchOn() && methods::is(genfun, 
                  "genericFunction")) 
                  genfun <- methods::finalDefaultMethod(genfun@default)
                if (typeof(genfun) == "closure") 
                  environment(genfun)
                else .BaseNamespaceEnv
            }
            if (is.null(S3Table <- get0(".__S3MethodsTable__.", 
                envir = defenv, inherits = FALSE))) {
                return(NULL)
            }
            if (is.null(mm <- get0(m, envir = S3Table))) {
                warning(gettextf("declared S3 method '%s' not found", 
                  m), domain = NA, call. = FALSE)
                return(NULL)
            }
            else mm
        }
        else get(m, envir = code_env)
        mArgs <- omArgs <- names(formals(gm))
        if (length(grep("\\.formula$", m))) {
            if (gArgs[1L] != "...") 
                gArgs <- gArgs[-1L]
            mArgs <- mArgs[-1L]
        }
        dotsPos <- which(gArgs == "...")
        ipos <- if (length(dotsPos)) 
            seq_len(dotsPos[1L] - 1L)
        else seq_along(gArgs)
        dotsPos <- which(mArgs == "...")
        if (length(dotsPos)) 
            ipos <- ipos[seq_len(dotsPos[1L] - 1L)]
        posMatchOK <- identical(gArgs[ipos], mArgs[ipos])
        argMatchOK <- all(gArgs %in% mArgs) || length(dotsPos) > 
            0L
        margMatchOK <- all(mArgs %in% c("...", gArgs)) || "..." %in% 
            ogArgs
        if (posMatchOK && argMatchOK && margMatchOK) 
            NULL
        else if (g %in% c("+", "-", "*", "/", "^", "%%", "%/%", 
            "&", "|", "!", "==", "!=", "<", "<=", ">=", ">") && 
            (length(ogArgs) == length(omArgs))) 
            NULL
        else {
            l <- list(ogArgs, omArgs)
            names(l) <- c(g, m)
            list(l)
        }
    }
    all_S3_generics <- unique(c(Filter(function(f) .is_S3_generic(f, 
        envir = code_env), functions_in_code), .get_S3_generics_as_seen_from_package(dir, 
        !missing(package), FALSE), S3_group_generics, S3_primitive_generics))
    code_env <- .make_S3_group_generic_env(parent = code_env)
    code_env <- .make_S3_primitive_generic_env(parent = code_env)
    bad_methods <- list()
    methods_stop_list <- nonS3methods(basename(dir))
    methods_stop_list <- c(methods_stop_list, "all.equal", "all.names", 
        "all.vars", "fitted.values", "qr.Q", "qr.R", "qr.X", 
        "qr.coef", "qr.fitted", "qr.qty", "qr.qy", "qr.resid", 
        "qr.solve", "rep.int", "seq.int", "sort.int", "sort.list", 
        "t.test")
    methods_not_registered_but_exported <- character()
    methods_not_registered_not_exported <- character()
    for (g in all_S3_generics) {
        if (!exists(g, envir = code_env)) 
            next
        name <- paste0(g, ".")
        methods <- functions_in_code[substr(functions_in_code, 
            1L, nchar(name, type = "c")) == name]
        methods <- setdiff(methods, methods_stop_list)
        if (has_namespace) {
            methods <- c(methods, ns_S3_methods[ns_S3_generics == 
                g])
            if (length(delta <- setdiff(methods, ns_S3_methods))) {
                methods_not_registered_but_exported <- c(methods_not_registered_but_exported, 
                  intersect(delta, objects_in_code))
                methods_not_registered_not_exported <- c(methods_not_registered_not_exported, 
                  setdiff(delta, objects_in_code))
            }
        }
        for (m in methods) bad_methods <- if (g == "all") {
            m1 <- m[-grep("^all\\.equal", m)]
            c(bad_methods, if (length(m1)) checkArgs(g, m1))
        }
        else c(bad_methods, checkArgs(g, m))
    }
    if (length(methods_not_registered_but_exported)) 
        attr(bad_methods, "methods_not_registered_but_exported") <- methods_not_registered_but_exported
    if (length(methods_not_registered_not_exported)) 
        attr(bad_methods, "methods_not_registered_not_exported") <- methods_not_registered_not_exported
    class(bad_methods) <- "checkS3methods"
    bad_methods
}


getBibstyle <- function (all = FALSE) 
{
    if (all) 
        names(environment(bibstyle)$styles)
    else environment(bibstyle)$default
}


md5sum <- function (files) 
structure(.Call(Rmd5, files), names = files)


Rd2HTML <- function (Rd, out = "", package = "", defines = .Platform$OS.type, 
    Links = NULL, Links2 = NULL, stages = "render", outputEncoding = "UTF-8", 
    dynamic = FALSE, no_links = FALSE, fragment = FALSE, stylesheet = "R.css", 
    ...) 
{
    if (missing(no_links) && is.null(Links) && !dynamic) 
        no_links <- TRUE
    version <- ""
    if (!identical(package, "")) {
        if (length(package) > 1L) {
            version <- package[2L]
            package <- package[1L]
        }
        else {
            dir <- dirname(package)
            if (nzchar(dir) && file_test("-f", dfile <- file.path(package, 
                "DESCRIPTION"))) {
                version <- .read_description(dfile)["Version"]
                package <- basename(package)
            }
            else {
                version <- utils::packageDescription(package, 
                  fields = "Version")
            }
        }
        if (is.na(version)) 
            version <- ""
    }
    writeLinesUTF8 <- if (outputEncoding == "UTF-8" || (outputEncoding == 
        "" && l10n_info()[["UTF-8"]])) {
        function(x, con, outputEncoding, ...) writeLines(x, con, 
            useBytes = TRUE, ...)
    }
    else {
        function(x, con, outputEncoding, ...) {
            x <- iconv(x, "UTF-8", outputEncoding, sub = "byte", 
                mark = FALSE)
            writeLines(x, con, useBytes = TRUE, ...)
        }
    }
    of0 <- function(...) writeLinesUTF8(paste0(...), con, outputEncoding, 
        sep = "")
    of1 <- function(text) writeLinesUTF8(text, con, outputEncoding, 
        sep = "")
    pendingClose <- pendingOpen <- character()
    inEqn <- FALSE
    sectionLevel <- 0L
    inPara <- FALSE
    inAsIs <- FALSE
    HTMLTags <- c(`\\bold` = "b", `\\cite` = "cite", `\\code` = "code", 
        `\\command` = "code", `\\dfn` = "dfn", `\\emph` = "em", 
        `\\kbd` = "kbd", `\\preformatted` = "pre", `\\strong` = "strong", 
        `\\var` = "var")
    HTMLEscapes <- c(`\\R` = "<span style=\"font-family: Courier New, Courier; color: #666666;\"><b>R</b></span>", 
        `\\cr` = "<br />", `\\dots` = "...", `\\ldots` = "...")
    HTMLLeft <- c(`\\acronym` = "<acronym><span class=\"acronym\">", 
        `\\donttest` = "", `\\env` = "<span class=\"env\">", 
        `\\file` = "&lsquo;<span class=\"file\">", `\\option` = "<span class=\"option\">", 
        `\\pkg` = "<span class=\"pkg\">", `\\samp` = "<span class=\"samp\">", 
        `\\sQuote` = "&lsquo;", `\\dQuote` = "&ldquo;", `\\verb` = "<code style=\"white-space: pre;\">")
    HTMLRight <- c(`\\acronym` = "</span></acronym>", `\\donttest` = "", 
        `\\env` = "</span>", `\\file` = "</span>&rsquo;", `\\option` = "</span>", 
        `\\pkg` = "</span>", `\\samp` = "</span>", `\\sQuote` = "&rsquo;", 
        `\\dQuote` = "&rdquo;", `\\verb` = "</code>")
    addParaBreaks <- function(x) {
        if (isBlankLineRd(x) && isTRUE(inPara)) {
            inPara <<- FALSE
            return("</p>\n")
        }
        start <- attr(x, "srcref")[2L]
        if (start == 1) 
            x <- psub("^\\s+", "", x)
        if (isTRUE(!inPara) && !all(grepl("^[[:blank:]\n]*$", 
            x, perl = TRUE))) {
            x <- c("<p>", x)
            inPara <<- TRUE
        }
        x
    }
    enterPara <- function(enter = TRUE) {
        if (enter && isTRUE(!inPara)) {
            of0("<p>")
            inPara <<- TRUE
        }
    }
    leavePara <- function(newval) {
        if (isTRUE(inPara)) 
            of0("</p>\n")
        inPara <<- newval
    }
    writeWrapped <- function(tag, block, doParas) {
        if (!doParas || HTMLTags[tag] == "pre") 
            leavePara(NA)
        else enterPara()
        saveAsIs <- inAsIs
        asis <- !is.na(match(tag, "\\command"))
        if (asis) 
            inAsIs <<- TRUE
        if (!isBlankRd(block)) {
            of0("<", HTMLTags[tag], ">")
            writeContent(block, tag)
            of0("</", HTMLTags[tag], ">")
        }
        if (HTMLTags[tag] == "pre") 
            inPara <<- FALSE
        if (asis) 
            inAsIs <<- saveAsIs
    }
    writeLink <- function(tag, block, doParas) {
        parts <- get_link(block, tag, Rdfile)
        writeHref <- function() {
            enterPara(doParas)
            savePara <- inPara
            inPara <<- NA
            if (!no_links) 
                of0("<a href=\"", htmlfile, "\">")
            writeContent(block, tag)
            if (!no_links) 
                of1("</a>")
            inPara <<- savePara
        }
        if (is.null(parts$targetfile)) {
            topic <- parts$dest
            if (dynamic) {
                htmlfile <- paste0("../../", urlify(package), 
                  "/help/", urlify(topic))
                writeHref()
                return()
            }
            else {
                htmlfile <- NA_character_
                if (!is.null(Links)) {
                  tmp <- Links[topic]
                  if (!is.na(tmp)) 
                    htmlfile <- tmp
                  else {
                    tmp <- Links2[topic]
                    if (!is.na(tmp)) 
                      htmlfile <- tmp
                  }
                }
            }
            if (is.na(htmlfile)) {
                if (!no_links) 
                  warnRd(block, Rdfile, "missing link ", sQuote(topic))
                writeContent(block, tag)
            }
            else {
                pkg_regexp <- paste0("^../../", urlify(package), 
                  "/html/")
                if (grepl(pkg_regexp, htmlfile)) {
                  htmlfile <- sub(pkg_regexp, "", htmlfile)
                }
                writeHref()
            }
        }
        else {
            htmlfile <- paste0(urlify(parts$targetfile), ".html")
            if (!dynamic && !no_links && nzchar(pkgpath <- system.file(package = parts$pkg))) {
                OK <- FALSE
                if (!file.exists(file.path(pkgpath, "html", htmlfile))) {
                  f <- file.path(pkgpath, "help", "paths.rds")
                  if (file.exists(f)) {
                    paths <- sub("\\.[Rr]d$", "", basename(readRDS(f)))
                    OK <- parts$targetfile %in% paths
                  }
                }
                else OK <- TRUE
                if (!OK) {
                  file <- utils:::index.search(parts$targetfile, 
                    pkgpath)
                  if (!length(file)) {
                    warnRd(block, Rdfile, "file link ", sQuote(parts$targetfile), 
                      " in package ", sQuote(parts$pkg), " does not exist and so has been treated as a topic")
                    parts$targetfile <- basename(file)
                  }
                  else {
                    warnRd(block, Rdfile, "missing file link ", 
                      sQuote(parts$targetfile))
                  }
                }
            }
            if (parts$pkg == package) {
                writeHref()
            }
            else {
                htmlfile <- paste0("../../", urlify(parts$pkg), 
                  "/html/", htmlfile)
                writeHref()
            }
        }
    }
    writeLR <- function(block, tag, doParas) {
        enterPara(doParas)
        of1(HTMLLeft[tag])
        writeContent(block, tag)
        of1(HTMLRight[tag])
    }
    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            of1("## Not run: ")
            writeContent(block, tag)
            of1("\n## End(Not run)")
        }
        else {
            of1("## Not run: ")
            writeContent(block, tag)
        }
    }
    writeBlock <- function(block, tag, blocktag) {
        doParas <- !(blocktag %in% c("\\tabular"))
        switch(tag, UNKNOWN = , VERB = of1(vhtmlify(block, inEqn)), 
            RCODE = of1(vhtmlify(block)), TEXT = of1(if (doParas && 
                !inAsIs) addParaBreaks(htmlify(block)) else vhtmlify(block)), 
            USERMACRO = , `\\newcommand` = , `\\renewcommand` = , 
            COMMENT = {
            }, LIST = writeContent(block, tag), `\\describe` = , 
            `\\enumerate` = , `\\itemize` = {
                leavePara(FALSE)
                writeContent(block, tag)
            }, `\\bold` = , `\\cite` = , `\\code` = , `\\command` = , 
            `\\dfn` = , `\\emph` = , `\\kbd` = , `\\preformatted` = , 
            `\\strong` = , `\\var` = writeWrapped(tag, block, 
                doParas), `\\special` = writeContent(block, tag), 
            `\\linkS4class` = , `\\link` = writeLink(tag, block, 
                doParas), `\\email` = if (length(block)) {
                url <- paste(as.character(block), collapse = "")
                url <- gsub("\n", "", url)
                enterPara(doParas)
                of0("<a href=\"mailto:", urlify(url), "\">", 
                  htmlify(url), "</a>")
            }, `\\url` = if (length(block)) {
                url <- paste(as.character(block), collapse = "")
                url <- trimws(gsub("\n", "", url, fixed = TRUE, 
                  useBytes = TRUE))
                enterPara(doParas)
                of0("<a href=\"", urlify(url), "\">", htmlify(url), 
                  "</a>")
            }, `\\href` = {
                if (length(block[[1L]])) {
                  url <- paste(as.character(block[[1L]]), collapse = "")
                  url <- trimws(gsub("\n", "", url, fixed = TRUE, 
                    useBytes = TRUE))
                  enterPara(doParas)
                  of0("<a href=\"", urlify(url), "\">")
                  closing <- "</a>"
                } else closing <- ""
                savePara <- inPara
                inPara <<- NA
                writeContent(block[[2L]], tag)
                of0(closing)
                inPara <<- savePara
            }, `\\Sexpr` = of0(as.character.Rd(block, deparse = TRUE)), 
            `\\cr` = , `\\dots` = , `\\ldots` = , `\\R` = {
                enterPara(doParas)
                of1(HTMLEscapes[tag])
            }, `\\acronym` = , `\\donttest` = , `\\env` = , `\\file` = , 
            `\\option` = , `\\pkg` = , `\\samp` = , `\\sQuote` = , 
            `\\dQuote` = , `\\verb` = writeLR(block, tag, doParas), 
            `\\dontrun` = writeDR(block, tag), `\\enc` = writeContent(block[[1L]], 
                tag), `\\eqn` = {
                enterPara(doParas)
                inEqn <<- TRUE
                of1("<i>")
                block <- block[[length(block)]]
                writeContent(block, tag)
                of1("</i>")
                inEqn <<- FALSE
            }, `\\deqn` = {
                inEqn <<- TRUE
                leavePara(TRUE)
                of1("<p style=\"text-align: center;\"><i>")
                block <- block[[length(block)]]
                writeContent(block, tag)
                of0("</i>")
                leavePara(FALSE)
                inEqn <<- FALSE
            }, `\\figure` = {
                enterPara(doParas)
                if (dynamic) of1("<img src=\"figures/") else of1("<img src=\"../help/figures/")
                writeContent(block[[1]], tag)
                of1("\" ")
                if (length(block) > 1L && length(imgoptions <- .Rd_get_latex(block[[2]])) && 
                  startsWith(imgoptions, "options: ")) {
                  imgoptions <- gsub("\\%", "%", imgoptions, 
                    fixed = TRUE)
                  of1(sub("^options: ", "", imgoptions))
                } else {
                  of1("alt=\"")
                  writeContent(block[[length(block)]], tag)
                  of1("\"")
                }
                of1(" />")
            }, `\\dontshow` = , `\\testonly` = {
            }, `\\method` = , `\\S3method` = , `\\S4method` = {
            }, `\\tabular` = writeTabular(block), `\\subsection` = writeSection(block, 
                tag), `\\if` = , `\\ifelse` = if (testRdConditional("html", 
                block, Rdfile)) writeContent(block[[2L]], tag) else if (tag == 
                "\\ifelse") writeContent(block[[3L]], tag), `\\out` = for (i in seq_along(block)) of1(block[[i]]), 
            stopRd(block, Rdfile, "Tag ", tag, " not recognized"))
    }
    writeTabular <- function(table) {
        format <- table[[1L]]
        content <- table[[2L]]
        if (length(format) != 1 || RdTags(format) != "TEXT") 
            stopRd(table, Rdfile, "\\tabular format must be simple text")
        format <- strsplit(format[[1L]], "", fixed = TRUE)[[1L]]
        if (!all(format %in% c("l", "c", "r"))) 
            stopRd(table, Rdfile, "Unrecognized \\tabular format: ", 
                table[[1L]][[1L]])
        format <- c(l = "left", c = "center", r = "right")[format]
        tags <- RdTags(content)
        leavePara(NA)
        of1("\n<table summary=\"Rd table\">\n")
        newrow <- TRUE
        newcol <- TRUE
        for (i in seq_along(tags)) {
            if (newrow) {
                of1("<tr>\n ")
                newrow <- FALSE
                col <- 0
            }
            if (newcol) {
                col <- col + 1L
                if (col > length(format)) 
                  stopRd(table, Rdfile, "Only ", length(format), 
                    " columns allowed in this table")
                of0("<td style=\"text-align: ", format[col], 
                  ";\">")
                newcol <- FALSE
            }
            switch(tags[i], `\\tab` = {
                of1("</td>")
                newcol <- TRUE
            }, `\\cr` = {
                if (!newcol) of1("</td>")
                of1("\n</tr>\n")
                newrow <- TRUE
                newcol <- TRUE
            }, writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        if (!newcol) 
            of1("</td>")
        if (!newrow) 
            of1("\n</tr>\n")
        of1("\n</table>\n")
        inPara <<- FALSE
    }
    writeContent <- function(blocks, blocktag) {
        inlist <- FALSE
        itemskip <- FALSE
        tags <- RdTags(blocks)
        i <- 0
        while (i < length(tags)) {
            i <- i + 1
            tag <- tags[i]
            block <- blocks[[i]]
            if (length(pendingOpen)) {
                if (tag == "RCODE" && startsWith(block, "(")) {
                  block <- sub("^\\(", "", block)
                  arg1 <- sub("[,)[:space:]].*", "", block)
                  block <- sub(paste0(arg1, "[[:space:]]*,[[:space:]]*"), 
                    "", block)
                  of0(arg1, pendingOpen)
                  if (pendingOpen == "$") 
                    pendingClose <<- ""
                  else pendingClose <<- chartr("[", "]", pendingOpen)
                }
                else of0("`", pendingOpen, "`")
                pendingOpen <<- character()
            }
            if (length(pendingClose) && tag == "RCODE" && grepl("\\)", 
                block)) {
                of0(sub("\\).*", "", block), pendingClose)
                block <- sub("[^)]*\\)", "", block)
                pendingClose <<- character()
            }
            switch(tag, `\\method` = , `\\S3method` = , `\\S4method` = {
                blocks <- transformMethod(i, blocks, Rdfile)
                tags <- RdTags(blocks)
                i <- i - 1
            }, `\\item` = {
                leavePara(FALSE)
                if (!inlist) {
                  switch(blocktag, `\\value` = of1("<table summary=\"R valueblock\">\n"), 
                    `\\arguments` = of1("<table summary=\"R argblock\">\n"), 
                    `\\itemize` = of1("<ul>\n"), `\\enumerate` = of1("<ol>\n"), 
                    `\\describe` = of1("<dl>\n"))
                  inlist <- TRUE
                } else {
                  if (blocktag %in% c("\\itemize", "\\enumerate")) {
                    of1("</li>\n")
                    itemskip <- TRUE
                  }
                }
                switch(blocktag, `\\value` = , `\\arguments` = {
                  of1("<tr valign=\"top\"><td><code>")
                  inPara <<- NA
                  writeContent(block[[1L]], tag)
                  of1("</code></td>\n<td>\n")
                  inPara <<- FALSE
                  writeContent(block[[2L]], tag)
                  leavePara(FALSE)
                  of1("</td></tr>")
                }, `\\describe` = {
                  of1("<dt>")
                  inPara <<- NA
                  writeContent(block[[1L]], tag)
                  of1("</dt><dd>")
                  inPara <<- FALSE
                  writeContent(block[[2L]], tag)
                  leavePara(FALSE)
                  of1("</dd>")
                }, `\\enumerate` = , `\\itemize` = {
                  inPara <<- FALSE
                  of1("<li>")
                })
            }, {
                if (inlist && !(blocktag %in% c("\\itemize", 
                  "\\enumerate")) && !(tag == "TEXT" && isBlankRd(block))) {
                  switch(blocktag, `\\arguments` = , `\\value` = of1("</table>\n"), 
                    `\\describe` = of1("</dl>\n"))
                  inlist <- FALSE
                  inPara <<- FALSE
                }
                if (itemskip) {
                  itemskip <- FALSE
                  if (tag == "TEXT") {
                    txt <- addParaBreaks(htmlify(block))
                    of1(txt)
                  } else writeBlock(block, tag, blocktag)
                } else writeBlock(block, tag, blocktag)
            })
        }
        if (inlist) {
            leavePara(FALSE)
            switch(blocktag, `\\value` = , `\\arguments` = of1("</table>\n"), 
                `\\itemize` = of1("</li></ul>\n"), `\\enumerate` = of1("</li></ol>\n"), 
                `\\describe` = of1("</dl>\n"))
        }
    }
    writeSection <- function(section, tag) {
        if (tag %in% c("\\alias", "\\concept", "\\encoding", 
            "\\keyword")) 
            return()
        leavePara(NA)
        save <- sectionLevel
        sectionLevel <<- sectionLevel + 1L
        of1(paste0("\n\n<h", sectionLevel + 2L, ">"))
        if (tag == "\\section" || tag == "\\subsection") {
            title <- section[[1L]]
            section <- section[[2L]]
            writeContent(title, tag)
        }
        else of1(sectionTitles[tag])
        of1(paste0("</h", sectionLevel + 2L, ">\n\n"))
        if (tag %in% c("\\examples", "\\usage")) {
            of1("<pre>")
            inPara <<- NA
            pre <- TRUE
        }
        else {
            inPara <<- FALSE
            pre <- FALSE
        }
        if (length(section)) {
            s1 <- section[[1L]][1L]
            if (RdTags(section)[1] == "TEXT" && s1 == "\n") 
                section <- section[-1L]
            writeContent(section, tag)
        }
        leavePara(FALSE)
        if (pre) 
            of0("</pre>\n")
        sectionLevel <<- save
    }
    if (is.character(out)) {
        if (out == "") {
            con <- stdout()
        }
        else {
            con <- file(out, "wt")
            on.exit(close(con))
        }
    }
    else {
        con <- out
        out <- summary(con)$description
    }
    Rd <- prepare_Rd(Rd, defines = defines, stages = stages, 
        fragment = fragment, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)
    if (fragment) {
        if (sections[1L] %in% names(sectionOrder)) 
            for (i in seq_along(sections)) writeSection(Rd[[i]], 
                sections[i])
        else for (i in seq_along(sections)) writeBlock(Rd[[i]], 
            sections[i], "")
    }
    else {
        name <- htmlify(Rd[[2L]][[1L]])
        of0("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">", 
            "<html xmlns=\"http://www.w3.org/1999/xhtml\">", 
            "<head><title>")
        headtitle <- strwrap(.Rd_format_title(.Rd_get_title(Rd)), 
            width = 65, initial = "R: ")
        if (length(headtitle) > 1) 
            headtitle <- paste0(headtitle[1], "...")
        of1(htmlify(headtitle))
        of0("</title>\n", "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=", 
            mime_canonical_encoding(outputEncoding), "\" />\n")
        of0("<link rel=\"stylesheet\" type=\"text/css\" href=\"", 
            urlify(stylesheet), "\" />\n", "</head><body>\n\n", 
            "<table width=\"100%\" summary=\"page for ", htmlify(name))
        if (nchar(package)) 
            of0(" {", package, "}\"><tr><td>", name, " {", package, 
                "}")
        else of0("\"><tr><td>", name)
        of0("</td><td style=\"text-align: right;\">R Documentation</td></tr></table>\n\n")
        of1("<h2>")
        inPara <- NA
        title <- Rd[[1L]]
        writeContent(title, sections[1])
        of1("</h2>")
        inPara <- FALSE
        for (i in seq_along(sections)[-(1:2)]) writeSection(Rd[[i]], 
            sections[i])
        if (nzchar(version)) 
            version <- paste0("Package <em>", package, "</em> version ", 
                version, " ")
        of0("\n")
        if (nzchar(version)) 
            of0("<hr /><div style=\"text-align: center;\">[", 
                version, if (!no_links) 
                  "<a href=\"00Index.html\">Index</a>", "]</div>")
        of0("\n", "</body></html>\n")
    }
    invisible(out)
}


SweaveTeXFilter <- function (ifile, encoding = "unknown") 
{
    if (inherits(ifile, "srcfile")) 
        ifile <- ifile$filename
    syntax <- utils:::SweaveGetSyntax(ifile)
    lines <- readLines(ifile, warn = FALSE)
    if (encoding != "unknown") {
        if (encoding == "UTF-8") 
            Encoding(lines) <- "UTF-8"
        else lines <- iconv(lines, encoding, "", sub = "byte")
    }
    TEXT <- 1L
    CODE <- 0L
    dpos <- grep(syntax$doc, lines)
    cpos <- grep(syntax$code, lines)
    recs <- rbind(data.frame(line = dpos, type = rep.int(TEXT, 
        length(dpos))), data.frame(line = cpos, type = rep.int(CODE, 
        length(cpos))))
    recs <- recs[order(recs$line), ]
    last <- 0L
    state <- TEXT
    for (i in seq_len(nrow(recs))) {
        line <- recs$line[i]
        if (state == CODE) 
            lines[(last + 1L):line] <- ""
        else lines[line] <- ""
        state <- recs$type[i]
        last <- line
    }
    lines
}


SIGUSR1 <- 10L


testInstalledPackage <- function (pkg, lib.loc = NULL, outDir = ".", types = c("examples", 
    "tests", "vignettes"), srcdir = NULL, Ropts = "", ...) 
{
    types <- match.arg(types, c("examples", "tests", "vignettes"), 
        several.ok = TRUE)
    pkgdir <- find.package(pkg, lib.loc)
    owd <- setwd(outDir)
    on.exit(setwd(owd))
    strict <- as.logical(Sys.getenv("R_STRICT_PACKAGE_CHECK", 
        "FALSE"))
    if ("examples" %in% types) {
        message(gettextf("Testing examples for package %s", sQuote(pkg)), 
            domain = NA)
        Rfile <- .createExdotR(pkg, pkgdir, silent = TRUE, ...)
        if (length(Rfile)) {
            outfile <- paste0(pkg, "-Ex.Rout")
            failfile <- paste(outfile, "fail", sep = ".")
            savefile <- paste(outfile, "prev", sep = ".")
            if (file.exists(outfile)) 
                file.rename(outfile, savefile)
            unlink(failfile)
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")), 
                "CMD BATCH --vanilla --no-timing", Ropts, shQuote(Rfile), 
                shQuote(failfile))
            if (.Platform$OS.type == "windows") 
                Sys.setenv(R_LIBS = "")
            else cmd <- paste("R_LIBS=", cmd)
            res <- system(cmd)
            if (res) 
                return(invisible(1L))
            else file.rename(failfile, outfile)
            savefile <- paste(outfile, "save", sep = ".")
            if (!is.null(srcdir)) 
                savefile <- file.path(srcdir, savefile)
            else {
                tfile <- file.path(pkgdir, "tests", "Examples", 
                  savefile)
                if (!file.exists(savefile) && file.exists(tfile)) 
                  savefile <- tfile
            }
            if (file.exists(savefile)) {
                if (file.exists(savefile)) {
                  message(gettextf("  comparing %s to %s ...", 
                    sQuote(outfile), sQuote(basename(savefile))), 
                    appendLF = FALSE, domain = NA)
                  res <- Rdiff(outfile, savefile)
                  if (!res) 
                    message(" OK")
                  else if (strict) 
                    stop("  ", "results differ from reference results")
                }
            }
            else {
                prevfile <- paste(outfile, "prev", sep = ".")
                if (file.exists(prevfile)) {
                  message(gettextf("  comparing %s to %s ...", 
                    sQuote(outfile), sQuote(basename(prevfile))), 
                    appendLF = FALSE, domain = NA)
                  res <- Rdiff(outfile, prevfile)
                  if (!res) 
                    message(" OK")
                }
            }
        }
        else warning(gettextf("no examples found for package %s", 
            sQuote(pkg)), call. = FALSE, domain = NA)
    }
    if ("tests" %in% types && dir.exists(d <- file.path(pkgdir, 
        "tests"))) {
        this <- paste(pkg, "tests", sep = "-")
        unlink(this, recursive = TRUE)
        dir.create(this)
        file.copy(Sys.glob(file.path(d, "*")), this, recursive = TRUE)
        setwd(this)
        message(gettextf("Running specific tests for package %s", 
            sQuote(pkg)), domain = NA)
        Rfiles <- dir(".", pattern = "\\.R$")
        for (f in Rfiles) {
            message(gettextf("  Running %s", sQuote(f)), domain = NA)
            outfile <- paste0(f, "out")
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")), 
                "CMD BATCH --vanilla --no-timing", Ropts, shQuote(f), 
                shQuote(outfile))
            cmd <- if (.Platform$OS.type == "windows") 
                paste(cmd, "LANGUAGE=C")
            else paste("LANGUAGE=C", cmd)
            res <- system(cmd)
            if (res) {
                file.rename(outfile, paste(outfile, "fail", sep = "."))
                return(invisible(1L))
            }
            savefile <- paste(outfile, "save", sep = ".")
            if (file.exists(savefile)) {
                message(gettextf("  comparing %s to %s ...", 
                  sQuote(outfile), sQuote(savefile)), appendLF = FALSE, 
                  domain = NA)
                res <- Rdiff(outfile, savefile)
                if (!res) 
                  message(" OK")
            }
        }
        setwd(owd)
    }
    if ("vignettes" %in% types && dir.exists(file.path(pkgdir, 
        "doc"))) {
        message(gettextf("Running vignettes for package %s", 
            sQuote(pkg)), domain = NA)
        checkVignettes(pkg, lib.loc, latex = FALSE, weave = TRUE)
    }
    invisible(0L)
}


psnice <- function (pid = Sys.getpid(), value = NA_integer_) 
{
    res <- .Call(ps_priority, pid, value)
    if (is.na(value)) 
        res
    else invisible(res)
}


SIGUSR2 <- 12L


codoc <- function (package, dir, lib.loc = NULL, use.values = NULL, verbose = getOption("verbose")) 
{
    has_namespace <- FALSE
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        if (!.haveRds(dir)) 
            stop(gettextf("directory '%s' does not contain Rd objects", 
                dir), domain = NA)
        is_base <- basename(dir) == "base"
        if (!is_base) 
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)
        objects_in_code <- sort(names(code_env))
        dirdir <- dirname(dir)
        if (packageHasNamespace(package, dirdir)) {
            has_namespace <- TRUE
            ns_env <- asNamespace(package)
            S3Table <- get(".__S3MethodsTable__.", envir = ns_env)
            functions_in_S3Table <- ls(S3Table, all.names = TRUE)
            objects_in_ns <- setdiff(sort(names(ns_env)), c(".__NAMESPACE__.", 
                ".__S3MethodsTable__."))
            ns_S3_methods_db <- ns_env$.__NAMESPACE__.$S3methods
            ns_S3_methods <- if (is.null(ns_S3_methods_db)) 
                character()
            else paste(ns_S3_methods_db[, 1L], ns_S3_methods_db[, 
                2L], sep = ".")
            objects_in_code_or_namespace <- unique(c(objects_in_code, 
                objects_in_ns, ns_S3_methods))
            objects_in_ns <- setdiff(objects_in_ns, objects_in_code)
        }
        else objects_in_code_or_namespace <- objects_in_code
        package_name <- package
    }
    else {
        if (missing(dir)) 
            stop("you must specify 'package' or 'dir'")
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        package_name <- basename(dir)
        dirdir <- dirname(dir)
        dir <- file_path_as_absolute(dir)
        code_dir <- file.path(dir, "R")
        if (!dir.exists(code_dir)) 
            stop(gettextf("directory '%s' does not contain R code", 
                dir), domain = NA)
        if (!.haveRds(dir)) 
            stop(gettextf("directory '%s' does not contain Rd objects", 
                dir), domain = NA)
        is_base <- package_name == "base"
        code_env <- new.env(hash = TRUE)
        dfile <- file.path(dir, "DESCRIPTION")
        meta <- if (file_test("-f", dfile)) 
            .read_description(dfile)
        else character()
        .source_assignments_in_code_dir(code_dir, code_env, meta)
        sys_data_file <- file.path(code_dir, "sysdata.rda")
        if (file_test("-f", sys_data_file)) 
            load(sys_data_file, code_env)
        objects_in_code <- sort(names(code_env))
        objects_in_code_or_namespace <- objects_in_code
        if (file.exists(file.path(dir, "NAMESPACE"))) {
            has_namespace <- TRUE
            objects_in_ns <- objects_in_code
            functions_in_S3Table <- character()
            ns_env <- code_env
            nsInfo <- parseNamespaceFile(package_name, dirdir)
            OK <- intersect(objects_in_code, nsInfo$exports)
            for (p in nsInfo$exportPatterns) OK <- c(OK, grep(p, 
                objects_in_code, value = TRUE))
            objects_in_code <- unique(OK)
        }
    }
    data_dir <- file.path(dir, "data")
    data_sets_in_code <- if (dir.exists(data_dir)) 
        names(.try_quietly(list_data_in_pkg(package_name, dataDir = data_dir)))
    else character()
    functions_in_code <- Filter(function(f) {
        f <- get(f, envir = code_env)
        typeof(f) == "closure"
    }, objects_in_code)
    if (is_base) {
        objects_in_base <- sort(names(baseenv()))
        objects_in_code <- c(objects_in_code, Filter(.is_primitive_in_base, 
            objects_in_base), c(".First.lib", ".Last.lib", ".Random.seed", 
            ".onLoad", ".onAttach", ".onDetach", ".onUnload"))
        objects_in_code_or_namespace <- objects_in_code
        known_env <- .make_S3_primitive_generic_env(code_env, 
            fixup = TRUE)
        extras <- ls(known_env, all.names = TRUE)
        functions_in_code <- c(functions_in_code, extras)
        code_env <- known_env
        known_env <- .make_S3_primitive_nongeneric_env(code_env)
        extras <- ls(known_env, all.names = TRUE)
        functions_in_code <- c(functions_in_code, extras)
        code_env <- known_env
    }
    function_args_in_code <- lapply(functions_in_code, function(f) formals(get(f, 
        envir = code_env)))
    names(function_args_in_code) <- functions_in_code
    if (has_namespace) {
        functions_in_ns <- Filter(function(f) {
            f <- get(f, envir = ns_env)
            is.function(f) && (length(formals(f)) > 0L)
        }, objects_in_ns)
        function_args_in_ns <- lapply(functions_in_ns, function(f) formals(get(f, 
            envir = ns_env)))
        names(function_args_in_ns) <- functions_in_ns
        function_args_in_S3Table <- lapply(functions_in_S3Table, 
            function(f) formals(get(f, envir = S3Table)))
        names(function_args_in_S3Table) <- functions_in_S3Table
        tmp <- c(function_args_in_code, function_args_in_S3Table, 
            function_args_in_ns)
        keep <- !duplicated(names(tmp))
        function_args_in_code <- tmp[keep]
        functions_in_code <- names(function_args_in_code)
    }
    if (.isMethodsDispatchOn()) {
        check_S4_methods <- !identical(as.logical(Sys.getenv("_R_CHECK_CODOC_S4_METHODS_")), 
            FALSE)
        if (check_S4_methods) {
            get_formals_from_method_definition <- function(m) formals(methods::unRematchDefinition(m))
            lapply(.get_S4_generics(code_env), function(f) {
                mlist <- .get_S4_methods_list(f, code_env)
                sigs <- .make_siglist(mlist)
                if (!length(sigs)) 
                  return()
                nm <- sprintf("\\S4method{%s}{%s}", f, sigs)
                args <- lapply(mlist, get_formals_from_method_definition)
                names(args) <- nm
                functions_in_code <<- c(functions_in_code, nm)
                function_args_in_code <<- c(function_args_in_code, 
                  args)
            })
        }
    }
    check_codoc <- function(fName, ffd) {
        ffc <- function_args_in_code[[fName]]
        if (identical(use.values, FALSE)) {
            ffc <- names(ffc)
            ffd <- names(ffd)
            ok <- identical(ffc, ffd)
        }
        else {
            if (!identical(names(ffc), names(ffd))) 
                ok <- FALSE
            else {
                vffc <- as.character(ffc)
                vffd <- as.character(ffd)
                if (!identical(use.values, TRUE)) {
                  ind <- nzchar(as.character(ffd))
                  vffc <- vffc[ind]
                  vffd <- vffd[ind]
                }
                ok <- identical(vffc, vffd)
            }
        }
        if (ok) 
            NULL
        else list(list(name = fName, code = ffc, docs = ffd))
    }
    db <- if (!missing(package)) 
        Rd_db(package, lib.loc = dirdir)
    else Rd_db(dir = dir)
    names(db) <- db_names <- .Rd_get_names_from_Rd_db(db)
    ind <- db_names %in% paste(package_name, "defunct", sep = "-")
    db <- db[!ind]
    db_names <- db_names[!ind]
    db_usages <- lapply(db, .Rd_get_section, "usage")
    db_usages <- lapply(db_usages, .parse_usage_as_much_as_possible)
    ind <- as.logical(sapply(db_usages, function(x) !is.null(attr(x, 
        "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")
    bad_doc_objects <- list()
    functions_in_usages <- character()
    variables_in_usages <- character()
    data_sets_in_usages <- character()
    functions_in_usages_not_in_code <- list()
    data_sets_in_usages_not_in_code <- list()
    for (docObj in db_names) {
        exprs <- db_usages[[docObj]]
        if (!length(exprs)) 
            next
        ind <- !sapply(exprs, is.call)
        if (any(ind)) {
            variables_in_usages <- c(variables_in_usages, sapply(exprs[ind], 
                deparse))
            exprs <- exprs[!ind]
        }
        ind <- as.logical(sapply(exprs, function(e) (length(e) == 
            2L) && e[[1L]] == as.symbol("data")))
        if (any(ind)) {
            data_sets <- sapply(exprs[ind], function(e) as.character(e[[2L]]))
            data_sets_in_usages <- c(data_sets_in_usages, data_sets)
            data_sets <- setdiff(data_sets, data_sets_in_code)
            if (length(data_sets)) 
                data_sets_in_usages_not_in_code[[docObj]] <- data_sets
            exprs <- exprs[!ind]
        }
        ind <- as.logical(sapply(exprs, .is_call_from_replacement_function_usage))
        replace_exprs <- exprs[ind]
        exprs <- exprs[!ind]
        functions <- sapply(exprs, function(e) as.character(e[[1L]]))
        ind <- !(functions %in% c("<-", "=", "+", "-"))
        exprs <- exprs[ind]
        functions <- functions[ind]
        functions <- .transform_S3_method_markup(as.character(functions))
        ind <- functions %in% functions_in_code
        bad_functions <- mapply(functions[ind], exprs[ind], FUN = function(x, 
            y) check_codoc(x, as.pairlist(as.alist.call(y[-1L]))), 
            SIMPLIFY = FALSE)
        if (length(replace_exprs)) {
            replace_funs <- paste0(sapply(replace_exprs, function(e) as.character(e[[2L]][[1L]])), 
                "<-")
            replace_funs <- .transform_S3_method_markup(replace_funs)
            functions <- c(functions, replace_funs)
            ind <- (replace_funs %in% functions_in_code)
            if (any(ind)) {
                bad_replace_funs <- mapply(replace_funs[ind], 
                  replace_exprs[ind], FUN = function(x, y) check_codoc(x, 
                    as.pairlist(c(as.alist.call(y[[2L]][-1L]), 
                      as.alist.symbol(y[[3L]])))), SIMPLIFY = FALSE)
                bad_functions <- c(bad_functions, bad_replace_funs)
            }
        }
        bad_functions <- do.call("c", bad_functions)
        if (length(bad_functions)) 
            bad_doc_objects[[docObj]] <- bad_functions
        ind <- grep(.S4_method_markup_regexp, functions)
        if (any(ind)) 
            functions <- functions[!ind]
        bad_functions <- setdiff(functions, objects_in_code_or_namespace)
        if (length(bad_functions)) 
            functions_in_usages_not_in_code[[docObj]] <- bad_functions
        functions_in_usages <- c(functions_in_usages, functions)
    }
    objects_in_code_not_in_usages <- setdiff(objects_in_code, 
        c(functions_in_usages, variables_in_usages))
    functions_in_code_not_in_usages <- intersect(functions_in_code, 
        objects_in_code_not_in_usages)
    functions_missing_from_usages <- if (!has_namespace) 
        character()
    else {
        functions <- functions_in_code_not_in_usages
        if (.isMethodsDispatchOn()) {
            functions <- setdiff(functions, names(.get_S4_generics(code_env)))
        }
        is_defunct <- function(f) {
            f <- get(f, envir = code_env)
            if (!is.function(f)) 
                return(FALSE)
            (is.call(b <- body(f)) && identical(as.character(b[[1L]]), 
                ".Defunct"))
        }
        functions[!sapply(functions, is_defunct)]
    }
    objects_missing_from_usages <- if (!has_namespace) 
        character()
    else {
        c(functions_missing_from_usages, setdiff(objects_in_code_not_in_usages, 
            c(functions_in_code, data_sets_in_code)))
    }
    attr(bad_doc_objects, "objects_in_code_not_in_usages") <- objects_in_code_not_in_usages
    attr(bad_doc_objects, "functions_in_code_not_in_usages") <- functions_in_code_not_in_usages
    attr(bad_doc_objects, "functions_in_usages_not_in_code") <- functions_in_usages_not_in_code
    attr(bad_doc_objects, "function_args_in_code") <- function_args_in_code
    attr(bad_doc_objects, "data_sets_in_usages_not_in_code") <- data_sets_in_usages_not_in_code
    attr(bad_doc_objects, "objects_missing_from_usages") <- objects_missing_from_usages
    attr(bad_doc_objects, "functions_missing_from_usages") <- functions_missing_from_usages
    attr(bad_doc_objects, "has_namespace") <- has_namespace
    attr(bad_doc_objects, "bad_lines") <- bad_lines
    class(bad_doc_objects) <- "codoc"
    bad_doc_objects
}


checkNEWS <- function (file = file.path(R.home(), "NEWS")) 
.Defunct()


resaveRdaFiles <- function (paths, compress = c("auto", "gzip", "bzip2", "xz"), 
    compression_level) 
{
    if (length(paths) == 1L && dir.exists(paths)) 
        paths <- Sys.glob(c(file.path(paths, "*.rda"), file.path(paths, 
            "*.RData")))
    compress <- match.arg(compress)
    if (missing(compression_level)) 
        compression_level <- switch(compress, gzip = 6, 9)
    for (p in paths) {
        env <- new.env(hash = TRUE)
        suppressPackageStartupMessages(load(p, envir = env))
        if (compress == "auto") {
            f1 <- tempfile()
            save(file = f1, list = ls(env, all.names = TRUE), 
                envir = env)
            f2 <- tempfile()
            save(file = f2, list = ls(env, all.names = TRUE), 
                envir = env, compress = "bzip2")
            ss <- file.size(c(f1, f2)) * c(0.9, 1)
            names(ss) <- c(f1, f2)
            if (ss[1L] > 10240) {
                f3 <- tempfile()
                save(file = f3, list = ls(env, all.names = TRUE), 
                  envir = env, compress = "xz")
                ss <- c(ss, file.size(f3))
                names(ss) <- c(f1, f2, f3)
            }
            nm <- names(ss)
            ind <- which.min(ss)
            file.copy(nm[ind], p, overwrite = TRUE)
            unlink(nm)
        }
        else save(file = p, list = ls(env, all.names = TRUE), 
            envir = env, compress = compress, compression_level = compression_level)
    }
}


add_datalist <- function (pkgpath, force = FALSE) 
{
    dlist <- file.path(pkgpath, "data", "datalist")
    if (!force && file.exists(dlist)) 
        return()
    size <- sum(file.size(Sys.glob(file.path(pkgpath, "data", 
        "*"))))
    if (size <= 1024^2) 
        return()
    z <- suppressPackageStartupMessages(list_data_in_pkg(dataDir = file.path(pkgpath, 
        "data")))
    if (!length(z)) 
        return()
    con <- file(dlist, "w")
    for (nm in names(z)) {
        zz <- z[[nm]]
        if (length(zz) == 1L && zz == nm) 
            writeLines(nm, con)
        else cat(nm, ": ", paste(zz, collapse = " "), "\n", sep = "", 
            file = con)
    }
    close(con)
    invisible()
}


startDynamicHelp <- function (start = TRUE) 
{
    if (nzchar(Sys.getenv("R_DISABLE_HTTPD"))) {
        httpdPort(-1L)
        warning("httpd server disabled by R_DISABLE_HTTPD", immediate. = TRUE)
        utils::flush.console()
        return(invisible(httpdPort()))
    }
    port <- httpdPort()
    if (is.na(start)) {
        if (port <= 0L) 
            return(startDynamicHelp(TRUE))
        return(invisible(port))
    }
    if (start && port) {
        if (port > 0L) 
            stop("server already running")
        else stop("server could not be started on an earlier attempt")
    }
    if (!start && (port <= 0L)) 
        stop("no running server to stop")
    if (start) {
        utils::flush.console()
        OK <- FALSE
        ports <- getOption("help.ports")
        if (is.null(ports)) {
            ports <- 10000 + 22000 * ((stats::runif(10) + unclass(Sys.time())/300)%%1)
        }
        ports <- as.integer(ports)
        if (all(ports == 0)) 
            return(invisible(0))
        message("starting httpd help server ...", appendLF = FALSE)
        for (i in seq_along(ports)) {
            status <- .Call(startHTTPD, "127.0.0.1", ports[i])
            if (status == 0L) {
                OK <- TRUE
                httpdPort(ports[i])
                break
            }
            if (status != -2L) 
                break
        }
        if (OK) {
            message(" done")
            utils::flush.console()
        }
        else {
            warning("failed to start the httpd server", immediate. = TRUE)
            utils::flush.console()
            httpdPort(-1L)
        }
    }
    else {
        .Call(stopHTTPD)
        httpdPort(0L)
    }
    invisible(httpdPort())
}


summarize_check_packages_in_dir_depends <- function (dir, all = FALSE, which = c("Depends", "Imports", "LinkingTo")) 
{
    if (identical(which, "all")) 
        which <- c("Depends", "Imports", "LinkingTo", "Suggests", 
            "Enhances")
    else if (identical(which, "most")) 
        which <- c("Depends", "Imports", "LinkingTo", "Suggests")
    for (d in R_check_outdirs(dir, all = all)) {
        dfile <- Sys.glob(file.path(d, "00_pkg_src", "*", "DESCRIPTION"))[1L]
        if (file_test("-f", dfile)) {
            meta <- .read_description(dfile)
            package <- meta["Package"]
            meta <- meta[match(which, names(meta), nomatch = 0L)]
            if (length(meta)) {
                writeLines(c(sprintf("Package: %s", package), 
                  unlist(Map(function(tag, val) {
                    strwrap(sprintf("%s: %s", tag, val), indent = 2L, 
                      exdent = 4L)
                  }, names(meta), meta))))
            }
        }
    }
    invisible()
}


write_PACKAGES <- function (dir = ".", fields = NULL, type = c("source", "mac.binary", 
    "win.binary"), verbose = FALSE, unpacked = FALSE, subdirs = FALSE, 
    latestOnly = TRUE, addFiles = FALSE) 
{
    if (missing(type) && .Platform$OS.type == "windows") 
        type <- "win.binary"
    type <- match.arg(type)
    nfields <- 0
    out <- file(file.path(dir, "PACKAGES"), "wt")
    outgz <- gzfile(file.path(dir, "PACKAGES.gz"), "wt")
    paths <- ""
    if (is.logical(subdirs) && subdirs) {
        owd <- setwd(dir)
        paths <- list.dirs(".")
        setwd(owd)
        paths <- c("", paths[paths != "."])
    }
    else if (is.character(subdirs)) 
        paths <- c("", subdirs)
    for (path in paths) {
        this <- if (nzchar(path)) 
            file.path(dir, path)
        else dir
        desc <- .build_repository_package_db(this, fields, type, 
            verbose, unpacked)
        desc <- Filter(length, desc)
        if (length(desc)) {
            Files <- names(desc)
            fields <- names(desc[[1L]])
            desc <- matrix(unlist(desc), ncol = length(fields), 
                byrow = TRUE)
            colnames(desc) <- fields
            if (addFiles) 
                desc <- cbind(desc, File = Files)
            if (latestOnly) 
                desc <- .remove_stale_dups(desc)
            license_info <- analyze_licenses(desc[, "License"])
            desc[, "License"] <- ifelse(license_info$is_standardizable, 
                license_info$standardization, NA)
            for (i in seq_len(nrow(desc))) {
                desci <- desc[i, !(is.na(desc[i, ]) | (desc[i, 
                  ] == "")), drop = FALSE]
                write.dcf(desci, file = out)
                if (nzchar(path)) 
                  cat("Path: ", path, "\n", sep = "", file = out)
                cat("\n", file = out)
                write.dcf(desci, file = outgz)
                if (nzchar(path)) 
                  cat("Path: ", path, "\n", sep = "", file = outgz)
                cat("\n", file = outgz)
            }
            nfields <- nfields + nrow(desc)
        }
    }
    close(out)
    close(outgz)
    invisible(nfields)
}


undoc <- function (package, dir, lib.loc = NULL) 
{
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dirdir <- dirname(dir <- find.package(package, lib.loc))
        is_base <- package == "base"
        all_doc_topics <- Rd_aliases(package, lib.loc = dirdir)
        if (!is_base) 
            .load_package_quietly(package, lib.loc)
        code_env <- .package_env(package)
        code_objs <- ls(envir = code_env, all.names = TRUE)
        pkgname <- package
    }
    else {
        if (missing(dir)) 
            stop("you must specify 'package' or 'dir'")
        pkgname <- basename(dir)
        dirdir <- dirname(dir)
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        else dir <- file_path_as_absolute(dir)
        is_base <- pkgname == "base"
        all_doc_topics <- Rd_aliases(dir = dir)
        code_env <- new.env(hash = TRUE)
        code_dir <- file.path(dir, "R")
        if (dir.exists(code_dir)) {
            dfile <- file.path(dir, "DESCRIPTION")
            meta <- if (file_test("-f", dfile)) 
                .read_description(dfile)
            else character()
            .source_assignments_in_code_dir(code_dir, code_env, 
                meta)
            sys_data_file <- file.path(code_dir, "sysdata.rda")
            if (file_test("-f", sys_data_file)) 
                load(sys_data_file, code_env)
        }
        code_objs <- ls(envir = code_env, all.names = TRUE)
        if (file.exists(file.path(dir, "NAMESPACE"))) {
            nsInfo <- parseNamespaceFile(pkgname, dirdir)
            OK <- intersect(code_objs, nsInfo$exports)
            for (p in nsInfo$exportPatterns) OK <- c(OK, grep(p, 
                code_objs, value = TRUE))
            code_objs <- unique(OK)
        }
    }
    data_dir <- file.path(dir, "data")
    data_objs <- if (dir.exists(data_dir)) 
        unlist(.try_quietly(list_data_in_pkg(pkgname, dataDir = data_dir)), 
            use.names = FALSE)
    else character()
    if (!missing(package) && !length(code_objs) && !length(data_objs) && 
        getOption("verbose")) 
        message("neither code nor data objects found")
    if (!is_base) {
        code_objs <- grep("^[^.].*", code_objs, value = TRUE)
        if (.isMethodsDispatchOn()) {
            code_objs <- Filter(function(f) {
                fdef <- get(f, envir = code_env)
                if (methods::is(fdef, "genericFunction")) 
                  fdef@package == pkgname
                else TRUE
            }, code_objs)
        }
        code_objs <- setdiff(code_objs, c("Arith", "Compare", 
            "Complex", "Logic", "Math", "Math2", "Ops", "Summary"))
    }
    undoc_things <- list(`code objects` = unique(setdiff(code_objs, 
        all_doc_topics)), `data sets` = unique(setdiff(data_objs, 
        all_doc_topics)))
    if (.isMethodsDispatchOn()) {
        S4_classes <- methods::getClasses(code_env)
        S4_classes <- S4_classes[!sapply(S4_classes, function(u) utils:::topicName("class", 
            u)) %in% all_doc_topics]
        undoc_things <- c(undoc_things, list(`S4 classes` = unique(S4_classes)))
    }
    if (.isMethodsDispatchOn()) {
        .make_S4_method_siglist <- function(g) {
            mlist <- .get_S4_methods_list(g, code_env)
            sigs <- .make_siglist(mlist)
            if (length(sigs)) 
                paste0(g, ",", sigs)
            else character()
        }
        S4_methods <- lapply(.get_S4_generics(code_env), .make_S4_method_siglist)
        S4_methods <- as.character(unlist(S4_methods, use.names = FALSE))
        S4_methods <- S4_methods[!vapply(S4_methods, utils:::topicName, 
            " ", type = "method", USE.NAMES = FALSE) %in% all_doc_topics]
        undoc_things <- c(undoc_things, list(`S4 methods` = unique(sub("([^,]*),(.*)", 
            "generic '\\1' and siglist '\\2'", S4_methods))))
    }
    if (is_base) {
        ff <- as.list(baseenv(), all.names = TRUE)
        prims <- names(ff)[vapply(ff, is.primitive, logical(1L))]
        prototypes <- sort(c(names(.ArgsEnv), names(.GenericArgsEnv)))
        extras <- setdiff(prototypes, prims)
        if (length(extras)) 
            undoc_things <- c(undoc_things, list(prim_extra = extras))
        miss <- setdiff(prims, c(langElts, prototypes))
        if (length(miss)) 
            undoc_things <- c(undoc_things, list(primitives = miss))
    }
    class(undoc_things) <- "undoc"
    undoc_things
}


HTMLheader <- function (title = "R", logo = TRUE, up = NULL, top = file.path(Rhome, 
    "doc/html/index.html"), Rhome = "", css = file.path(Rhome, 
    "doc/html/R.css"), headerTitle = paste("R:", title), outputEncoding = "UTF-8") 
{
    result <- c("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">", 
        "<html xmlns=\"http://www.w3.org/1999/xhtml\">", paste0("<head><title>", 
            headerTitle, "</title>"), paste0("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=", 
            mime_canonical_encoding(outputEncoding), "\" />"), 
        paste0("<link rel=\"stylesheet\" type=\"text/css\" href=\"", 
            css, "\" />"), "</head><body>", paste("<h1>", title))
    if (logo) 
        result <- c(result, paste0("<img class=\"toplogo\" src=\"", 
            file.path(Rhome, "doc/html/Rlogo.svg"), "\" alt=\"[R logo]\" />"))
    result <- c(result, "</h1>", "<hr/>")
    if (!is.null(up) || !is.null(top)) {
        result <- c(result, "<div style=\"text-align: center;\">")
        if (!is.null(up)) 
            result <- c(result, paste0("<a href=\"", up, "\"><img class=\"arrow\" src=\"", 
                file.path(Rhome, "doc/html/left.jpg"), "\" alt=\"[Up]\" /></a>"))
        if (!is.null(top)) 
            result <- c(result, paste0("<a href=\"", top, "\"><img class=\"arrow\" src=\"", 
                file.path(Rhome, "doc/html/up.jpg"), "\" alt=\"[Top]\" /></a>"))
        result <- c(result, "</div>")
    }
    result
}


installFoundDepends <- function (depPkgList, ...) 
{
    .Deprecated(msg = "'installFoundDepends()' is deprecated: It is misleading nowadays\n since it has never taken 'Imports' into consideration")
    urls <- names(depPkgList)
    for (i in seq_along(depPkgList)) {
        if (length(depPkgList[[i]])) 
            utils::install.packages(depPkgList[[i]], contriburl = urls[i], 
                ...)
    }
    NULL
}


loadPkgRdMacros <- function (pkgdir, macros = NULL) 
{
    pkglist <- try(.read_description(file.path(pkgdir, "DESCRIPTION")), 
        silent = TRUE)
    if (inherits(pkglist, "try-error")) 
        pkglist <- try(.read_description(file.path(pkgdir, "DESCRIPTION.in")), 
            silent = TRUE)
    if (inherits(pkglist, "try-error")) 
        return(macros)
    pkglist <- pkglist["RdMacros"]
    if (is.na(pkglist)) 
        pkglist <- NULL
    if (is.null(macros)) 
        macros <- initialRdMacros(pkglist)
    else macros <- initialRdMacros(pkglist, macros)
    files <- c(list.files(file.path(pkgdir, "man", "macros"), 
        pattern = "\\.Rd$", full.names = TRUE), list.files(file.path(pkgdir, 
        "help", "macros"), pattern = "\\.Rd$", full.names = TRUE))
    for (f in files) macros <- loadRdMacros(f, macros)
    macros
}


toRd <- function (obj, ...) 
UseMethod("toRd")


find_gs_cmd <- function (gs_cmd = "") 
{
    if (!nzchar(gs_cmd)) {
        if (.Platform$OS.type == "windows") {
            gsexe <- Sys.getenv("R_GSCMD")
            if (!nzchar(gsexe)) 
                gsexe <- Sys.getenv("GSC")
            gs_cmd <- Sys.which(gsexe)
            if (!nzchar(gs_cmd)) 
                gs_cmd <- Sys.which("gswin64c")
            if (!nzchar(gs_cmd)) 
                gs_cmd <- Sys.which("gswin32c")
            gs_cmd
        }
        else Sys.which(Sys.getenv("R_GSCMD", "gs"))
    }
    else Sys.which(gs_cmd)
}


dependsOnPkgs <- function (pkgs, dependencies = c("Depends", "Imports", "LinkingTo"), 
    recursive = TRUE, lib.loc = NULL, installed = utils::installed.packages(lib.loc, 
        fields = "Enhances")) 
{
    if (identical(dependencies, "all")) 
        dependencies <- c("Depends", "Imports", "LinkingTo", 
            "Suggests", "Enhances")
    else if (identical(dependencies, "most")) 
        dependencies <- c("Depends", "Imports", "LinkingTo", 
            "Suggests")
    av <- installed[, dependencies, drop = FALSE]
    rn <- row.names(installed)
    need <- apply(av, 1L, function(x) any(pkgs %in% utils:::.clean_up_dependencies(x)))
    uses <- rn[need]
    if (recursive) {
        p <- pkgs
        repeat {
            p <- unique(c(p, uses))
            need <- apply(av, 1L, function(x) any(p %in% utils:::.clean_up_dependencies(x)))
            uses <- unique(c(p, rn[need]))
            if (length(uses) <= length(p)) 
                break
        }
    }
    setdiff(uses, pkgs)
}


checkVignettes <- function (package, dir, lib.loc = NULL, tangle = TRUE, weave = TRUE, 
    latex = FALSE, workdir = c("tmp", "src", "cur"), keepfiles = FALSE) 
{
    vigns <- pkgVignettes(package = package, dir = dir, lib.loc = lib.loc)
    if (is.null(vigns)) 
        return(NULL)
    workdir <- match.arg(workdir)
    wd <- getwd()
    if (is.null(wd)) 
        stop("current working directory cannot be ascertained")
    if (workdir == "tmp") {
        tmpd <- tempfile("Sweave")
        if (!dir.create(tmpd)) 
            stop(gettextf("unable to create temp directory %s ", 
                sQuote(tmpd)), domain = NA)
        setwd(tmpd)
    }
    else {
        keepfiles <- TRUE
        if (workdir == "src") 
            setwd(vigns$dir)
    }
    on.exit({
        setwd(wd)
        if (!keepfiles) unlink(tmpd, recursive = TRUE)
    })
    file.create(".check.timestamp")
    result <- list(tangle = list(), weave = list(), source = list(), 
        latex = list())
    loadVignetteBuilder(vigns$pkgdir)
    startdir <- getwd()
    for (i in seq_along(vigns$docs)) {
        file <- vigns$docs[i]
        file <- basename(file)
        name <- vigns$names[i]
        engine <- vignetteEngine(vigns$engines[i])
        enc <- vigns$encodings[i]
        if (enc == "non-ASCII") 
            stop(gettextf("Vignette '%s' is non-ASCII but has no declared encoding", 
                name), domain = NA)
        if (tangle) {
            message("  Running ", sQuote(file))
            .eval_with_capture({
                result$tangle[[file]] <- tryCatch({
                  engine$tangle(file, quiet = TRUE, encoding = enc)
                  setwd(startdir)
                  find_vignette_product(name, by = "tangle", 
                    main = FALSE, engine = engine)
                }, error = function(e) e)
            })
        }
        if (weave) {
            setwd(startdir)
            .eval_with_capture({
                result$weave[[file]] <- tryCatch({
                  engine$weave(file, quiet = TRUE, encoding = enc)
                  setwd(startdir)
                  find_vignette_product(name, by = "weave", engine = engine)
                }, error = function(e) e)
            })
        }
        setwd(startdir)
    }
    for (name in c("weave", "tangle")) {
        resultsT <- result[[name]]
        if (length(resultsT) <= 1L) 
            next
        for (i in 1L:(length(resultsT) - 1L)) {
            outputsI <- resultsT[[i]]
            if (inherits(outputsI, "error")) 
                next
            outputsI <- normalizePath(outputsI)
            for (j in (i + 1L):length(resultsT)) {
                outputsJ <- resultsT[[j]]
                if (inherits(outputsJ, "error")) 
                  next
                outputsJ <- normalizePath(outputsJ)
                bad <- intersect(outputsJ, outputsI)
                if (length(bad) > 0L) {
                  stop(gettextf("Vignette %s overwrites the following %s output by vignette %s: %s", 
                    sQuote(basename(names(resultsT)[j])), sQuote(name), 
                    sQuote(basename(names(resultsT)[i])), paste(basename(bad), 
                      collapse = ", ")), domain = NA)
                }
            }
        }
    }
    if (tangle) {
        cwd <- getwd()
        if (is.null(cwd)) 
            stop("current working directory cannot be ascertained")
        for (i in seq_along(result$tangle)) {
            sources <- result$tangle[[i]]
            if (inherits(sources, "error")) 
                next
            sources <- sources[file_test("-nt", sources, ".check.timestamp")]
            for (file in sources) {
                .eval_with_capture({
                  result$source[[file]] <- tryCatch({
                    source(file)
                  }, error = function(e) e)
                })
                setwd(startdir)
            }
        }
    }
    if (weave && latex) {
        if (!("Makefile" %in% list.files(vigns$dir))) {
            for (i in seq_along(result$weave)) {
                file <- names(result$weave)[i]
                output <- result$weave[i]
                if (inherits(output, "error")) 
                  next
                if (!vignette_is_tex(output)) 
                  next
                .eval_with_capture({
                  result$latex[[file]] <- tryCatch({
                    texi2pdf(file = output, clean = FALSE, quiet = TRUE)
                    find_vignette_product(name, by = "texi2pdf", 
                      engine = engine)
                  }, error = function(e) e)
                })
            }
        }
    }
    for (name in c("tangle", "weave", "source", "latex")) {
        resultsT <- result[[name]]
        resultsT <- lapply(resultsT, FUN = function(res) {
            if (inherits(res, "error")) 
                conditionMessage(res)
            else NULL
        })
        resultsT <- resultsT[!sapply(resultsT, FUN = is.null)]
        result[[name]] <- resultsT
    }
    file.remove(".check.timestamp")
    class(result) <- "checkVignettes"
    result
}


texi2dvi <- function (file, pdf = FALSE, clean = FALSE, quiet = TRUE, texi2dvi = getOption("texi2dvi"), 
    texinputs = NULL, index = TRUE) 
{
    if (clean) 
        pre_files <- list.files(all.files = TRUE)
    do_cleanup <- function(clean) if (clean) {
        out_file <- paste(basename(file_path_sans_ext(file)), 
            if (pdf) 
                "pdf"
            else "dvi", sep = ".")
        files <- setdiff(list.files(all.files = TRUE), c(".", 
            "..", out_file, pre_files))
        file.remove(files)
    }
    if (identical(texi2dvi, "emulation")) 
        texi2dvi <- ""
    else {
        if (is.null(texi2dvi) || !nzchar(texi2dvi) || texi2dvi == 
            "texi2dvi") 
            texi2dvi <- Sys.which("texi2dvi")
        if (.Platform$OS.type == "windows" && !nzchar(texi2dvi)) 
            texi2dvi <- Sys.which("texify")
    }
    envSep <- .Platform$path.sep
    texinputs0 <- texinputs
    Rtexmf <- file.path(R.home("share"), "texmf")
    Rtexinputs <- file.path(Rtexmf, "tex", "latex")
    texinputs <- paste(c(texinputs0, Rtexinputs, ""), collapse = envSep)
    if (.Platform$OS.type == "windows") 
        texinputs <- gsub("\\", "/", texinputs, fixed = TRUE)
    Rbibinputs <- file.path(Rtexmf, "bibtex", "bib")
    bibinputs <- paste(c(texinputs0, Rbibinputs, ""), collapse = envSep)
    Rbstinputs <- file.path(Rtexmf, "bibtex", "bst")
    bstinputs <- paste(c(texinputs0, Rbstinputs, ""), collapse = envSep)
    otexinputs <- Sys.getenv("TEXINPUTS", unset = NA_character_)
    if (is.na(otexinputs)) {
        on.exit(Sys.unsetenv("TEXINPUTS"))
        otexinputs <- "."
    }
    else on.exit(Sys.setenv(TEXINPUTS = otexinputs))
    Sys.setenv(TEXINPUTS = paste(otexinputs, texinputs, sep = envSep))
    obibinputs <- Sys.getenv("BIBINPUTS", unset = NA_character_)
    if (is.na(obibinputs)) {
        on.exit(Sys.unsetenv("BIBINPUTS"), add = TRUE)
        obibinputs <- "."
    }
    else on.exit(Sys.setenv(BIBINPUTS = obibinputs, add = TRUE))
    Sys.setenv(BIBINPUTS = paste(obibinputs, bibinputs, sep = envSep))
    obstinputs <- Sys.getenv("BSTINPUTS", unset = NA_character_)
    if (is.na(obstinputs)) {
        on.exit(Sys.unsetenv("BSTINPUTS"), add = TRUE)
        obstinputs <- "."
    }
    else on.exit(Sys.setenv(BSTINPUTS = obstinputs), add = TRUE)
    Sys.setenv(BSTINPUTS = paste(obstinputs, bstinputs, sep = envSep))
    if (index && nzchar(texi2dvi) && .Platform$OS.type != "windows") {
        Sys.setenv(TEXINDY = "false")
        on.exit(Sys.unsetenv("TEXINDY"), add = TRUE)
        opt_pdf <- if (pdf) 
            "--pdf"
        else ""
        opt_quiet <- if (quiet) 
            "--quiet"
        else ""
        opt_extra <- ""
        out <- .system_with_capture(texi2dvi, "--help")
        if (length(grep("--no-line-error", out$stdout))) 
            opt_extra <- "--no-line-error"
        if (any(grepl("--max-iterations=N", out$stdout))) 
            opt_extra <- c(opt_extra, "--max-iterations=20")
        env0 <- "LC_COLLATE=C"
        if (grepl(" ", Sys.getenv("TMPDIR"))) 
            env0 <- paste(env0, "TMPDIR=/tmp")
        out <- .system_with_capture(texi2dvi, c(opt_pdf, opt_quiet, 
            opt_extra, shQuote(file)), env = env0)
        log <- paste(file_path_sans_ext(file), "log", sep = ".")
        if (out$status && file_test("-f", log) && any(grepl("(Rerun to get|biblatex.*\\(re\\)run)", 
            readLines(log, warn = FALSE)))) {
            out <- .system_with_capture(texi2dvi, c(opt_pdf, 
                opt_quiet, opt_extra, shQuote(file)), env = env0)
        }
        errors <- character()
        log <- paste(file_path_sans_ext(file), "log", sep = ".")
        if (file_test("-f", log)) {
            lines <- .get_LaTeX_errors_from_log_file(log)
            if (length(lines)) 
                errors <- paste("LaTeX errors:", paste(lines, 
                  collapse = "\n"), sep = "\n")
        }
        log <- paste(file_path_sans_ext(file), "blg", sep = ".")
        if (file_test("-f", log)) {
            lines <- .get_BibTeX_errors_from_blg_file(log)
            if (length(lines)) 
                errors <- paste("BibTeX errors:", paste(lines, 
                  collapse = "\n"), sep = "\n")
        }
        msg <- ""
        if (out$status) {
            msg <- gettextf("Running 'texi2dvi' on '%s' failed.", 
                file)
            if (length(errors)) 
                msg <- paste(msg, errors, sep = "\n")
            else if (length(out$stderr)) 
                msg <- paste(msg, "Messages:", paste(out$stderr, 
                  collapse = "\n"), sep = "\n")
            if (!quiet) 
                msg <- paste(msg, "Output:", paste(out$stdout, 
                  collapse = "\n"), sep = "\n")
        }
        do_cleanup(clean)
        if (nzchar(msg)) 
            stop(msg, domain = NA)
        else if (!quiet) 
            message(paste(paste(out$stderr, collapse = "\n"), 
                paste(out$stdout, collapse = "\n"), sep = "\n"))
    }
    else if (index && nzchar(texi2dvi)) {
        extra <- ""
        ver <- system(paste(shQuote(texi2dvi), "--version"), 
            intern = TRUE)
        if (length(grep("MiKTeX", ver[1L]))) {
            texinputs <- c(texinputs0, Rtexinputs, Rbstinputs)
            texinputs <- gsub("\\", "/", texinputs, fixed = TRUE)
            paths <- paste("-I", shQuote(texinputs))
            extra <- "--max-iterations=20"
            extra <- paste(extra, paste(paths, collapse = " "))
        }
        base <- basename(file_path_sans_ext(file))
        system(paste(shQuote(texi2dvi), if (quiet) 
            "--quiet"
        else "", if (pdf) 
            "--pdf"
        else "", shQuote(file), extra), intern = TRUE, ignore.stderr = TRUE)
        msg <- ""
        logfile <- paste(base, "log", sep = ".")
        if (file_test("-f", logfile)) {
            lines <- .get_LaTeX_errors_from_log_file(logfile)
            if (length(lines)) 
                msg <- paste(msg, "LaTeX errors:", paste(lines, 
                  collapse = "\n"), sep = "\n")
        }
        logfile <- paste(base, "blg", sep = ".")
        if (file_test("-f", logfile)) {
            lines <- .get_BibTeX_errors_from_blg_file(logfile)
            if (length(lines)) 
                msg <- paste(msg, "BibTeX errors:", paste(lines, 
                  collapse = "\n"), sep = "\n")
        }
        do_cleanup(clean)
        if (nzchar(msg)) {
            msg <- paste(gettextf("running 'texi2dvi' on '%s' failed", 
                file), msg, "", sep = "\n")
            stop(msg, call. = FALSE, domain = NA)
        }
    }
    else {
        texfile <- shQuote(file)
        base <- basename(file_path_sans_ext(file))
        idxfile <- paste0(base, ".idx")
        latex <- if (pdf) 
            Sys.getenv("PDFLATEX", "pdflatex")
        else Sys.getenv("LATEX", "latex")
        if (!nzchar(Sys.which(latex))) 
            stop(if (pdf) 
                "pdflatex"
            else "latex", " is not available", domain = NA)
        sys2 <- if (quiet) 
            function(...) system2(..., stdout = FALSE, stderr = FALSE)
        else system2
        bibtex <- Sys.getenv("BIBTEX", "bibtex")
        makeindex <- Sys.getenv("MAKEINDEX", "makeindex")
        ltxargs <- c("-interaction=nonstopmode", texfile)
        if (sys2(latex, ltxargs)) 
            stop(gettextf("unable to run '%s' on '%s'", latex, 
                file), domain = NA)
        nmiss <- length(grep("Warning:.*Citation.*undefined", 
            readLines(paste0(base, ".log"))))
        for (iter in 1L:10L) {
            if (nmiss) 
                sys2(bibtex, shQuote(base))
            nmiss_prev <- nmiss
            if (index && file.exists(idxfile)) {
                if (sys2(makeindex, shQuote(idxfile))) 
                  stop(gettextf("unable to run '%s' on '%s'", 
                    makeindex, idxfile), domain = NA)
            }
            if (sys2(latex, ltxargs)) {
                lines <- .get_LaTeX_errors_from_log_file(paste0(base, 
                  ".log"))
                errors <- if (length(lines)) 
                  paste("LaTeX errors:", paste(lines, collapse = "\n"), 
                    sep = "\n")
                else character()
                stop(paste(gettextf("unable to run %s on '%s'", 
                  latex, file), errors, sep = "\n"), domain = NA)
            }
            Log <- readLines(paste0(base, ".log"))
            nmiss <- length(grep("Warning:.*Citation.*undefined", 
                Log))
            if (nmiss == nmiss_prev && !any(grepl("(Rerun to get|biblatex.*\\(re\\)run)", 
                Log))) 
                break
        }
        do_cleanup(clean)
    }
    invisible(NULL)
}


toTitleCase <- function (text) 
{
    alone <- c("2D", "3D", "AIC", "BayesX", "GoF", "HTML", "LaTeX", 
        "MonetDB", "OpenBUGS", "TeX", "U.S.", "U.S.A.", "WinBUGS", 
        "aka", "et", "al.", "ggplot2", "i.e.", "jar", "jars", 
        "ncdf", "netCDF", "rgl", "rpart", "xls", "xlsx")
    lpat <- "^(a|an|and|are|as|at|be|but|by|en|for|if|in|is|nor|not|of|on|or|per|so|the|to|v[.]?|via|vs[.]?|from|into|than|that|with)$"
    either <- c("all", "above", "after", "along", "also", "among", 
        "any", "both", "can", "few", "it", "less", "log", "many", 
        "may", "more", "over", "some", "their", "then", "this", 
        "under", "until", "using", "von", "when", "where", "which", 
        "will", "without", "yet", "you", "your")
    titleCase1 <- function(x) {
        do1 <- function(x) {
            x1 <- substring(x, 1L, 1L)
            if (nchar(x) >= 3L && x1 %in% c("'", "\"")) 
                paste0(x1, toupper(substring(x, 2L, 2L)), tolower(substring(x, 
                  3L)))
            else paste0(toupper(x1), tolower(substring(x, 2L)))
        }
        xx <- .Call(splitString, x, " -/\"()\n")
        alone <- xx %in% c(alone, either)
        alone <- alone | grepl("^'.*'$", xx)
        havecaps <- grepl("^[[:alpha:]].*[[:upper:]]+", xx)
        l <- grepl(lpat, xx, ignore.case = TRUE)
        l[1L] <- FALSE
        ind <- grep("[-:]$", xx)
        ind <- ind[ind + 2L <= length(l)]
        ind <- ind[(xx[ind + 1L] == " ") & grepl("^['[:alnum:]]", 
            xx[ind + 2L])]
        l[ind + 2L] <- FALSE
        ind <- which(xx == "\"")
        ind <- ind[ind + 1L <= length(l)]
        l[ind + 1L] <- FALSE
        xx[l] <- tolower(xx[l])
        keep <- havecaps | l | (nchar(xx) == 1L) | alone
        xx[!keep] <- sapply(xx[!keep], do1)
        paste(xx, collapse = "")
    }
    if (typeof(text) != "character") 
        stop("'text' must be a character vector")
    sapply(text, titleCase1, USE.NAMES = FALSE)
}


summarize_check_packages_in_dir_results <- function (dir, all = TRUE, full = FALSE) 
{
    dir <- normalizePath(dir)
    outdirs <- R_check_outdirs(dir, all = all)
    logs <- file.path(outdirs, "00check.log")
    logs <- logs[file_test("-f", logs)]
    results <- check_packages_in_dir_results(logs = logs)
    writeLines("Check status summary:")
    tab <- check_packages_in_dir_results_summary(results)
    rownames(tab) <- paste0("  ", rownames(tab))
    print(tab)
    writeLines("")
    writeLines("Check results summary:")
    Map(function(p, r) {
        writeLines(c(sprintf("%s ... %s", p, r$status), r$lines))
    }, names(results), results)
    if (full && !all(as.character(unlist(lapply(results, `[[`, 
        "status"))) == "OK")) {
        writeLines(c("", "Check results details:"))
        details <- check_packages_in_dir_details(logs = logs)
        writeLines(paste(format(details), collapse = "\n\n"))
        invisible(TRUE)
    }
    else {
        invisible(FALSE)
    }
}


langElts <- c("(", "{", ":", "~", "<-", "<<-", "=", "[", "[[", "[[<-", "[<-", 
"@", "@<-", "$", "$<-", "&&", "||", "break", "for", "function", 
"if", "next", "repeat", "return", "while")


Rd2latex <- function (Rd, out = "", defines = .Platform$OS.type, stages = "render", 
    outputEncoding = "ASCII", fragment = FALSE, ..., writeEncoding = TRUE) 
{
    encode_warn <- FALSE
    WriteLines <- if (outputEncoding == "UTF-8" || (outputEncoding == 
        "" && l10n_info()[["UTF-8"]])) {
        function(x, con, outputEncoding, ...) writeLines(x, con, 
            useBytes = TRUE, ...)
    }
    else {
        function(x, con, outputEncoding, ...) {
            x <- iconv(x, "UTF-8", outputEncoding, mark = FALSE)
            if (anyNA(x)) {
                x <- iconv(x, "UTF-8", outputEncoding, sub = "byte", 
                  mark = FALSE)
                encode_warn <<- TRUE
            }
            writeLines(x, con, useBytes = TRUE, ...)
        }
    }
    last_char <- ""
    of0 <- function(...) of1(paste0(...))
    of1 <- function(text) {
        nc <- nchar(text)
        last_char <<- substr(text, nc, nc)
        WriteLines(text, con, outputEncoding, sep = "")
    }
    trim <- function(x) {
        x <- psub1("^\\s*", "", as.character(x))
        psub1("\\s*$", "", x)
    }
    envTitles <- c(`\\description` = "Description", `\\usage` = "Usage", 
        `\\arguments` = "Arguments", `\\format` = "Format", `\\details` = "Details", 
        `\\note` = "Note", `\\section` = "", `\\author` = "Author", 
        `\\references` = "References", `\\source` = "Source", 
        `\\seealso` = "SeeAlso", `\\examples` = "Examples", `\\value` = "Value")
    sectionExtras <- c(`\\usage` = "verbatim", `\\arguments` = "ldescription", 
        `\\examples` = "ExampleCode")
    inCodeBlock <- FALSE
    inCode <- FALSE
    inEqn <- FALSE
    inPre <- FALSE
    sectionLevel <- 0
    hasFigures <- FALSE
    startByte <- function(x) {
        srcref <- attr(x, "srcref")
        if (is.null(srcref)) 
            NA
        else srcref[2L]
    }
    addParaBreaks <- function(x, tag) {
        start <- startByte(x)
        if (isBlankLineRd(x)) 
            "\n"
        else if (identical(start, 1L)) 
            psub("^\\s+", "", x)
        else x
    }
    texify <- function(x, code = inCodeBlock) {
        if (inEqn) 
            return(x)
        if (!code) {
            x <- fsub("\\", "\\bsl", x)
            x <- psub("([&$%_#])", "\\\\\\1", x)
            x <- fsub("{", "\\{", x)
            x <- fsub("}", "\\}", x)
            x <- fsub("^", "\\textasciicircum{}", x)
            x <- fsub("~", "\\textasciitilde{}", x)
            x <- fsub("\\bsl", "\\bsl{}", x)
        }
        else {
            x <- psub("\\\\[l]{0,1}dots", "...", as.character(x))
            x <- psub("\\\\([$^&~_#])", "\\1", x)
            if (inCodeBlock) {
                x <- fsub1("\"\\{\"", "\"{\"", x)
            }
            else if (inPre) {
                BSL = "@BSL@"
                x <- fsub("\\", BSL, x)
                x <- psub("(?<!\\\\)\\{", "\\\\{", x)
                x <- psub("(?<!\\\\)}", "\\\\}", x)
                x <- fsub(BSL, "\\bsl{}", x)
                x <- psub("\\\\\\\\var\\\\\\{([^\\\\]*)\\\\}", 
                  "\\\\var{\\1}", x)
            }
            else {
                BSL = "@BSL@"
                x <- fsub("\\", BSL, x)
                x <- psub("(?<!\\\\)\\{", "\\\\{", x)
                x <- psub("(?<!\\\\)}", "\\\\}", x)
                x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
                x <- fsub("^", "\\textasciicircum{}", x)
                x <- fsub("~", "\\textasciitilde{}", x)
                x <- fsub(BSL, "\\bsl{}", x)
                x <- fsub("<<", "<{}<", x)
                x <- fsub(">>", ">{}>", x)
                x <- fsub(",,", ",{},", x)
            }
        }
        x
    }
    wrappers <- list(`\\dQuote` = c("``", "''"), `\\sQuote` = c("`", 
        "'"), `\\cite` = c("\\Cite{", "}"))
    writeWrapped <- function(block, tag) {
        wrapper <- wrappers[[tag]]
        if (is.null(wrapper)) 
            wrapper <- c(paste0(tag, "{"), "}")
        of1(wrapper[1L])
        writeContent(block, tag)
        of1(wrapper[2L])
    }
    writeURL <- function(block, tag) {
        if (tag == "\\url") 
            url <- as.character(block)
        else {
            url <- as.character(block[[1L]])
            tag <- "\\Rhref"
        }
        url <- trimws(gsub("\n", "", paste(as.character(url), 
            collapse = ""), fixed = TRUE, useBytes = TRUE))
        url <- gsub("%", "\\%", url, fixed = TRUE, useBytes = TRUE)
        of0(tag, "{", url, "}")
        if (tag == "\\Rhref") {
            of1("{")
            writeContent(block[[2L]], tag)
            of1("}")
        }
    }
    writeLink <- function(tag, block) {
        parts <- get_link(block, tag)
        of0("\\LinkA{", latex_escape_link(parts$topic), "}{", 
            latex_link_trans0(parts$dest), "}")
    }
    writeDR <- function(block, tag) {
        if (length(block) > 1L) {
            of1("## Not run: ")
            writeContent(block, tag)
            of1("\n## End(Not run)")
        }
        else {
            of1("## Not run: ")
            writeContent(block, tag)
        }
    }
    ltxstriptitle <- function(x) {
        x <- fsub("\\R", "\\R{}", x)
        x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x
    }
    latex_escape_name <- function(x) {
        x <- psub("([$#~_&])", "\\\\\\1", x)
        x <- fsub("{", "\\textbraceleft{}", x)
        x <- fsub("}", "\\textbraceright{}", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x <- fsub("%", "\\Rpercent{}", x)
        x <- fsub("\\\\", "\\textbackslash{}", x)
        x <- fsub("<<", "<{}<", x)
        x <- fsub(">>", ">{}>", x)
        x
    }
    latex_escape_link <- function(x) {
        x <- fsub("\\_", "_", x)
        latex_escape_name(x)
    }
    latex_link_trans0 <- function(x) {
        x <- fsub("\\Rdash", ".Rdash.", x)
        x <- fsub("-", ".Rdash.", x)
        x <- fsub("\\_", ".Rul.", x)
        x <- fsub("\\$", ".Rdol.", x)
        x <- fsub("\\^", ".Rcaret.", x)
        x <- fsub("^", ".Rcaret.", x)
        x <- fsub("_", ".Rul.", x)
        x <- fsub("$", ".Rdol.", x)
        x <- fsub("\\#", ".Rhash.", x)
        x <- fsub("#", ".Rhash.", x)
        x <- fsub("\\&", ".Ramp.", x)
        x <- fsub("&", ".Ramp.", x)
        x <- fsub("\\~", ".Rtilde.", x)
        x <- fsub("~", ".Rtilde.", x)
        x <- fsub("\\%", ".Rpcent.", x)
        x <- fsub("%", ".Rpcent.", x)
        x <- fsub("\\\\", ".Rbl.", x)
        x <- fsub("{", ".Rlbrace.", x)
        x <- fsub("}", ".Rrbrace.", x)
        x
    }
    latex_code_alias <- function(x) {
        x <- fsub("{", "\\{", x)
        x <- fsub("}", "\\}", x)
        x <- psub("(?<!\\\\)([&$%_#])", "\\\\\\1", x)
        x <- fsub("^", "\\textasciicircum{}", x)
        x <- fsub("~", "\\textasciitilde{}", x)
        x <- fsub("<-", "<\\Rdash", x)
        x <- psub("([!|])", "\"\\1", x)
        x
    }
    currentAlias <- NA_character_
    writeAlias <- function(block, tag) {
        alias <- as.character(block)
        aa <- "\\aliasA{"
        if (grepl("[|{(]", alias)) 
            aa <- "\\aliasB{"
        if (is.na(currentAlias)) 
            currentAlias <<- name
        if (pmatch(paste0(currentAlias, "."), alias, 0L)) {
            aa <- "\\methaliasA{"
        }
        else currentAlias <<- alias
        if (alias == name) 
            return()
        alias2 <- latex_link_trans0(alias)
        of0(aa, latex_code_alias(alias), "}{", latex_escape_name(name), 
            "}{", alias2, "}\n")
    }
    writeBlock <- function(block, tag, blocktag) {
        switch(tag, UNKNOWN = , VERB = of1(texify(block, TRUE)), 
            RCODE = of1(texify(block, TRUE)), TEXT = of1(addParaBreaks(texify(block), 
                blocktag)), USERMACRO = , `\\newcommand` = , 
            `\\renewcommand` = , COMMENT = {
            }, LIST = writeContent(block, tag), `\\describe` = {
                of1("\\begin{description}\n")
                writeContent(block, tag)
                of1("\n\\end{description}\n")
            }, `\\enumerate` = {
                of1("\\begin{enumerate}\n")
                writeContent(block, tag)
                of1("\n\\end{enumerate}\n")
            }, `\\itemize` = {
                of1("\\begin{itemize}\n")
                writeContent(block, tag)
                of1("\n\\end{itemize}\n")
            }, `\\command` = , `\\env` = , `\\kbd` = , `\\option` = , 
            `\\samp` = writeWrapped(block, tag), `\\url` = , 
            `\\href` = writeURL(block, tag), `\\code` = {
                inCode <<- TRUE
                writeWrapped(block, tag)
                inCode <<- FALSE
            }, `\\acronym` = , `\\bold` = , `\\dfn` = , `\\dQuote` = , 
            `\\email` = , `\\emph` = , `\\file` = , `\\pkg` = , 
            `\\sQuote` = , `\\strong` = , `\\var` = , `\\cite` = if (inCodeBlock) writeContent(block, 
                tag) else writeWrapped(block, tag), `\\preformatted` = {
                inPre <<- TRUE
                of1("\\begin{alltt}")
                writeContent(block, tag)
                of1("\\end{alltt}\n")
                inPre <<- FALSE
            }, `\\Sexpr` = {
                of1("\\begin{verbatim}\n")
                of0(as.character.Rd(block, deparse = TRUE))
                of1("\n\\end{verbatim}\n")
            }, `\\verb` = {
                of0("\\AsIs{")
                writeContent(block, tag)
                of1("}")
            }, `\\special` = writeContent(block, tag), `\\linkS4class` = , 
            `\\link` = writeLink(tag, block), `\\cr` = of1("\\\\{}"), 
            `\\dots` = , `\\ldots` = of1(if (inCode || inCodeBlock) "..." else tag), 
            `\\R` = of0(tag, "{}"), `\\donttest` = writeContent(block, 
                tag), `\\dontrun` = writeDR(block, tag), `\\enc` = {
                if (outputEncoding == "ASCII") writeContent(block[[2L]], 
                  tag) else writeContent(block[[1L]], tag)
            }, `\\eqn` = , `\\deqn` = {
                of0(tag, "{")
                inEqn <<- TRUE
                writeContent(block[[1L]], tag)
                inEqn <<- FALSE
                of0("}{}")
            }, `\\figure` = {
                of0("\\Figure{")
                writeContent(block[[1L]], tag)
                of0("}{")
                if (length(block) > 1L) {
                  includeoptions <- .Rd_get_latex(block[[2]])
                  if (length(includeoptions) && startsWith(includeoptions, 
                    "options: ")) of0(sub("^options: ", "", includeoptions))
                }
                of0("}")
                hasFigures <<- TRUE
            }, `\\dontshow` = , `\\testonly` = {
            }, `\\method` = , `\\S3method` = , `\\S4method` = {
            }, `\\tabular` = writeTabular(block), `\\subsection` = writeSection(block, 
                tag), `\\if` = , `\\ifelse` = if (testRdConditional("latex", 
                block, Rdfile)) writeContent(block[[2L]], tag) else if (tag == 
                "\\ifelse") writeContent(block[[3L]], tag), `\\out` = for (i in seq_along(block)) of1(block[[i]]), 
            stopRd(block, Rdfile, "Tag ", tag, " not recognized"))
    }
    writeTabular <- function(table) {
        format <- table[[1L]]
        content <- table[[2L]]
        if (length(format) != 1L || RdTags(format) != "TEXT") 
            stopRd(table, Rdfile, "\\tabular format must be simple text")
        tags <- RdTags(content)
        of0("\n\\Tabular{", format, "}{")
        for (i in seq_along(tags)) {
            switch(tags[i], `\\tab` = of1("&"), `\\cr` = of1("\\\\{}"), 
                writeBlock(content[[i]], tags[i], "\\tabular"))
        }
        of1("}")
    }
    writeContent <- function(blocks, blocktag) {
        inList <- FALSE
        itemskip <- FALSE
        tags <- RdTags(blocks)
        i <- 0
        while (i < length(tags)) {
            i <- i + 1
            block <- blocks[[i]]
            tag <- attr(block, "Rd_tag")
            if (!is.null(tag)) 
                switch(tag, `\\method` = , `\\S3method` = , `\\S4method` = {
                  blocks <- transformMethod(i, blocks, Rdfile)
                  tags <- RdTags(blocks)
                  i <- i - 1
                }, `\\item` = {
                  if (blocktag == "\\value" && !inList) {
                    of1("\\begin{ldescription}\n")
                    inList <- TRUE
                  }
                  switch(blocktag, `\\describe` = {
                    of1("\\item[")
                    writeContent(block[[1L]], tag)
                    of1("] ")
                    writeContent(block[[2L]], tag)
                  }, `\\value` = , `\\arguments` = {
                    of1("\\item[\\code{")
                    inCode <<- TRUE
                    writeContent(block[[1L]], tag)
                    inCode <<- FALSE
                    of1("}] ")
                    writeContent(block[[2L]], tag)
                  }, `\\enumerate` = , `\\itemize` = {
                    of1("\\item ")
                    itemskip <- TRUE
                  })
                  itemskip <- TRUE
                }, `\\cr` = of1("\\\\{}"), {
                  if (inList && !(tag == "TEXT" && isBlankRd(block))) {
                    of1("\\end{ldescription}\n")
                    inList <- FALSE
                  }
                  if (itemskip) {
                    itemskip <- FALSE
                    if (tag == "TEXT") {
                      txt <- psub("^ ", "", as.character(block))
                      of1(texify(txt))
                    } else writeBlock(block, tag, blocktag)
                  } else writeBlock(block, tag, blocktag)
                })
        }
        if (inList) 
            of1("\\end{ldescription}\n")
    }
    writeSectionInner <- function(section, tag) {
        if (length(section)) {
            nxt <- section[[1L]]
            if (!attr(nxt, "Rd_tag") %in% c("TEXT", "RCODE") || 
                substr(as.character(nxt), 1L, 1L) != "\n") 
                of1("\n")
            writeContent(section, tag)
            inCodeBlock <<- FALSE
            if (last_char != "\n") 
                of1("\n")
        }
    }
    writeSection <- function(section, tag) {
        if (tag %in% c("\\encoding", "\\concept")) 
            return()
        save <- sectionLevel
        sectionLevel <<- sectionLevel + 1
        if (tag == "\\alias") 
            writeAlias(section, tag)
        else if (tag == "\\keyword") {
            key <- trim(section)
            of0("\\keyword{", latex_escape_name(key), "}{", ltxname, 
                "}\n")
        }
        else if (tag == "\\section" || tag == "\\subsection") {
            macro <- c("Section", "SubSection", "SubSubSection")[min(sectionLevel, 
                3)]
            of0("%\n\\begin{", macro, "}{")
            writeContent(section[[1L]], tag)
            of1("}")
            writeSectionInner(section[[2L]], tag)
            of0("\\end{", macro, "}\n")
        }
        else {
            title <- envTitles[tag]
            of0("%\n\\begin{", title, "}")
            if (tag %in% c("\\author", "\\description", "\\details", 
                "\\note", "\\references", "\\seealso", "\\source")) 
                of1("\\relax")
            extra <- sectionExtras[tag]
            if (!is.na(extra)) 
                of0("\n\\begin{", extra, "}")
            if (tag %in% c("\\usage", "\\examples")) 
                inCodeBlock <<- TRUE
            writeSectionInner(section, tag)
            inCodeBlock <<- FALSE
            if (!is.na(extra)) 
                of0("\\end{", extra, "}\n")
            of0("\\end{", title, "}\n")
        }
        sectionLevel <<- save
    }
    Rd <- prepare_Rd(Rd, defines = defines, stages = stages, 
        fragment = fragment, ...)
    Rdfile <- attr(Rd, "Rdfile")
    sections <- RdTags(Rd)
    if (is.character(out)) {
        if (out == "") {
            con <- stdout()
        }
        else {
            con <- file(out, "wt")
            on.exit(close(con))
        }
    }
    else {
        con <- out
        out <- summary(con)$description
    }
    if (outputEncoding != "ASCII") {
        latexEncoding <- latex_canonical_encoding(outputEncoding)
        if (writeEncoding) 
            of0("\\inputencoding{", latexEncoding, "}\n")
    }
    else latexEncoding <- NA
    if (fragment) {
        if (sections[1L] %in% names(sectionOrder)) 
            for (i in seq_along(sections)) writeSection(Rd[[i]], 
                sections[i])
        else for (i in seq_along(sections)) writeBlock(Rd[[i]], 
            sections[i], "")
    }
    else {
        nm <- character(length(Rd))
        isAlias <- sections == "\\alias"
        sortorder <- if (any(isAlias)) {
            nm[isAlias] <- sapply(Rd[isAlias], as.character)
            order(sectionOrder[sections], toupper(nm), nm)
        }
        else order(sectionOrder[sections])
        Rd <- Rd[sortorder]
        sections <- sections[sortorder]
        title <- .Rd_get_latex(.Rd_get_section(Rd, "title"))
        title <- paste(title[nzchar(title)], collapse = " ")
        name <- Rd[[2L]]
        name <- trim(as.character(Rd[[2L]][[1L]]))
        ltxname <- latex_escape_name(name)
        of0("\\HeaderA{", ltxname, "}{", ltxstriptitle(title), 
            "}{", latex_link_trans0(name), "}\n")
        for (i in seq_along(sections)[-(1:2)]) writeSection(Rd[[i]], 
            sections[i])
    }
    if (encode_warn) 
        warnRd(Rd, Rdfile, "Some input could not be re-encoded to ", 
            outputEncoding)
    invisible(structure(out, latexEncoding = latexEncoding, hasFigures = hasFigures))
}


Rcmd <- function (args, ...) 
{
    if (.Platform$OS.type == "windows") 
        system2(file.path(R.home("bin"), "Rcmd.exe"), args, ...)
    else system2(file.path(R.home("bin"), "R"), c("CMD", args), 
        ...)
}


parse_Rd <- function (file, srcfile = NULL, encoding = "unknown", verbose = FALSE, 
    fragment = FALSE, warningCalls = TRUE, macros = file.path(R.home("share"), 
        "Rd", "macros", "system.Rd"), permissive = FALSE) 
{
    if (is.character(file)) {
        file0 <- file
        if (file == "") {
            file <- stdin()
        }
        else {
            if (missing(srcfile)) 
                srcfile <- srcfile(file)
        }
    }
    else file0 <- "<connection>"
    lines <- readLines(file, warn = FALSE)
    if (is.character(macros)) 
        macros <- initialRdMacros(macros = macros)
    lines[lines == "\\non_function{}"] <- ""
    enc <- grep("\\encoding{", lines, fixed = TRUE, useBytes = TRUE)
    enc <- grep("^[[:space:]]*\\\\encoding\\{([^}]*)\\}.*", lines[enc], 
        value = TRUE)
    if (length(enc)) {
        if (length(enc) > 1L) 
            warning(file0, ": multiple \\encoding lines, using the first", 
                domain = NA, call. = warningCalls)
        enc <- enc[1L]
        enc <- sub("^[[:space:]]*\\\\encoding\\{([^}]*)\\}.*", 
            "\\1", enc)
        if (verbose) 
            message("found encoding ", enc, domain = NA)
        encoding <- if (enc %in% c("UTF-8", "utf-8", "utf8")) 
            "UTF-8"
        else enc
    }
    if (encoding == "unknown") 
        encoding <- ""
    if (!inherits(srcfile, "srcfile")) 
        srcfile <- srcfile(file0)
    basename <- basename(srcfile$filename)
    srcfile$encoding <- encoding
    srcfile$Enc <- "UTF-8"
    if (encoding == "ASCII") {
        if (any(is.na(iconv(lines, "", "ASCII")))) 
            stop(file0, ": non-ASCII input and no declared encoding", 
                domain = NA, call. = warningCalls)
    }
    else if (encoding != "UTF-8") 
        lines <- iconv(lines, encoding, "UTF-8", sub = "byte")
    tcon <- file()
    writeLines(lines, tcon, useBytes = TRUE)
    on.exit(close(tcon))
    warndups <- config_val_to_logical(Sys.getenv("_R_WARN_DUPLICATE_RD_MACROS_", 
        "FALSE"))
    if (permissive) 
        result <- withCallingHandlers(.External2(C_parseRd, tcon, 
            srcfile, "UTF-8", verbose, basename, fragment, warningCalls, 
            macros, warndups), warning = function(w) if (grepl("unknown macro", 
            conditionMessage(w))) 
            invokeRestart("muffleWarning"))
    else result <- .External2(C_parseRd, tcon, srcfile, "UTF-8", 
        verbose, basename, fragment, warningCalls, macros, warndups)
    result <- expandDynamicFlags(result)
    if (permissive) 
        result <- permissify(result)
    result
}


testInstalledPackages <- function (outDir = ".", errorsAreFatal = TRUE, scope = c("both", 
    "base", "recommended"), types = c("examples", "tests", "vignettes"), 
    srcdir = NULL, Ropts = "", ...) 
{
    ow <- options(warn = 1)
    on.exit(ow)
    scope <- match.arg(scope)
    status <- 0L
    pkgs <- character()
    known_packages <- .get_standard_package_names()
    if (scope %in% c("both", "base")) 
        pkgs <- known_packages$base
    if (scope %in% c("both", "recommended")) 
        pkgs <- c(pkgs, known_packages$recommended)
    mc.cores <- as.integer(Sys.getenv("TEST_MC_CORES", "1"))
    if (.Platform$OS.type != "windows" && !is.na(mc.cores) && 
        mc.cores > 1L) {
        do_one <- function(pkg) {
            if (is.null(srcdir) && pkg %in% known_packages$base) 
                srcdir <- R.home("tests/Examples")
            testInstalledPackage(pkg, .Library, outDir, types, 
                srcdir, Ropts, ...)
        }
        res <- parallel::mclapply(pkgs, do_one, mc.cores = mc.cores, 
            mc.preschedule = FALSE)
        res <- unlist(res) != 0L
        if (any(res)) {
            for (i in which(res)) warning(gettextf("testing '%s' failed", 
                pkgs[i]), domain = NA, call. = FALSE, immediate. = TRUE)
            if (errorsAreFatal) 
                stop(sprintf(ngettext(sum(res), "%d of the package tests failed", 
                  "%d of the package tests failed", domain = "R-tools"), 
                  sum(res)), domain = NA, call. = FALSE)
        }
    }
    else {
        for (pkg in pkgs) {
            if (is.null(srcdir) && pkg %in% known_packages$base) 
                srcdir <- R.home("tests/Examples")
            res <- testInstalledPackage(pkg, .Library, outDir, 
                types, srcdir, Ropts, ...)
            if (res) {
                status <- 1L
                msg <- gettextf("testing '%s' failed", pkg)
                if (errorsAreFatal) 
                  stop(msg, domain = NA, call. = FALSE)
                else warning(msg, domain = NA, call. = FALSE, 
                  immediate. = TRUE)
            }
        }
    }
    invisible(status)
}


RdTextFilter <- function (ifile, encoding = "unknown", keepSpacing = TRUE, drop = character(), 
    keep = character(), macros = file.path(R.home("share"), "Rd", 
        "macros", "system.Rd")) 
{
    if (inherits(ifile, "srcfile")) 
        ifile <- ifile$filename
    if (inherits(ifile, "Rd")) {
        srcrefs <- sapply(ifile, function(s) attr(s, "srcref"))
        p <- ifile[order(srcrefs[1, ], srcrefs[2, ])]
        class(p) <- class(ifile)
    }
    else p <- parse_Rd(ifile, encoding = encoding, macros = macros)
    tags <- RdTags(p)
    if ("\\encoding" %in% tags) {
        encoding <- p[[which.max(tags == "\\encoding")]][[1L]]
        if (encoding %in% c("UTF-8", "utf-8", "utf8")) 
            encoding <- "UTF-8"
        if (!inherits(ifile, "Rd")) 
            p <- parse_Rd(ifile, encoding = encoding, macros = macros)
    }
    else encoding <- ""
    mycon <- textConnection("myval", open = "w", local = TRUE, 
        encoding = "UTF-8")
    on.exit(close(mycon))
    mycat <- function(...) cat(..., file = mycon)
    prevline <- 1L
    prevcol <- 0L
    doPartialMarkup <- function(x, tags, i) {
        result <- FALSE
        if (i < length(tags) && tags[i + 1L] == "TEXT" && length(x[[i]]) == 
            1L && tags[i] %in% c("\\bold", "\\emph", "\\strong", 
            "\\link") && !(tags[i] %in% drop) && RdTags(x[[i]]) == 
            "TEXT") {
            text1 <- x[[i]][[1L]]
            if (length(grep("[^[:space:]]$", text1))) {
                text2 <- x[[i + 1L]]
                if (length(grep("^[^[:space:]]", text2))) {
                  show(text1)
                  prevcol <<- prevcol + 1L
                  saveline <- prevline
                  show(text2)
                  if (prevline == saveline) 
                    prevcol <<- prevcol - 1L
                  result <- TRUE
                }
            }
        }
        result
    }
    show <- function(x) {
        srcref <- attr(x, "srcref")
        firstline <- srcref[1L]
        lastline <- srcref[3L]
        firstcol <- srcref[5L]
        lastcol <- srcref[6L]
        tag <- attr(x, "Rd_tag")
        if (is.null(tag)) 
            tag <- "NULL"
        if (tag %in% drop) 
            tag <- "DROP"
        else if (tag %in% keep) 
            tag <- "KEEPLIST"
        switch(tag, KEEP = , TEXT = {
            if (prevline < firstline) {
                prevcol <<- 0L
                mycat(rep.int("\n", if (keepSpacing) firstline - 
                  prevline else 1L))
            }
            if (keepSpacing) mycat(rep.int(" ", firstcol - prevcol - 
                1L), sep = "")
            x <- as.character(srcref)
            mycat(x, sep = "")
            prevcol <<- lastcol
            prevline <<- lastline
        }, `\\S3method` = , `\\S4method` = , `\\command` = , 
            `\\docType` = , `\\email` = , `\\encoding` = , `\\file` = , 
            `\\keyword` = , `\\link` = , `\\linkS4class` = , 
            `\\method` = , `\\pkg` = , `\\var` = , DROP = {
            }, `\\tabular` = , `#ifdef` = , `#ifndef` = {
                show(x[[2L]])
            }, `\\item` = {
                if (length(x) == 2L) show(x[[2L]])
            }, {
                if (is.list(x)) {
                  tags <- RdTags(x)
                  i <- 0L
                  while (i < length(x)) {
                    i <- i + 1L
                    if (doPartialMarkup(x, tags, i)) i <- i + 
                      1L else show(x[[i]])
                  }
                } else if (tag == "KEEPLIST") {
                  attr(x, "Rd_tag") <- "KEEP"
                  show(x)
                }
            })
    }
    show(p)
    mycat("\n")
    out <- textConnectionValue(mycon)
    if (encoding == "latin1") 
        out <- iconv(out, "UTF-8", "latin1")
    out
}


compactPDF <- function (paths, qpdf = Sys.which(Sys.getenv("R_QPDF", "qpdf")), 
    gs_cmd = Sys.getenv("R_GSCMD", ""), gs_quality = Sys.getenv("GS_QUALITY", 
        "none"), gs_extras = character()) 
{
    use_qpdf <- nzchar(qpdf)
    gs_quality <- match.arg(gs_quality, c("none", "printer", 
        "ebook", "screen"))
    use_gs <- if (gs_quality != "none") 
        nzchar(gs_cmd <- find_gs_cmd(gs_cmd))
    else FALSE
    if (!use_gs && !use_qpdf) 
        return()
    if (length(paths) == 1L && dir.exists(paths)) 
        paths <- Sys.glob(file.path(paths, "*.pdf"))
    dummy <- rep.int(NA_real_, length(paths))
    ans <- data.frame(old = dummy, new = dummy, row.names = paths)
    tf <- tempfile("pdf")
    tf2 <- tempfile("pdf")
    for (p in paths) {
        res <- 0
        if (use_gs) {
            res <- system2(gs_cmd, c("-q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite", 
                sprintf("-dPDFSETTINGS=/%s", gs_quality), "-dCompatibilityLevel=1.5", 
                "-dAutoRotatePages=/None", sprintf("-sOutputFile=%s", 
                  tf), gs_extras, p), FALSE, FALSE)
            if (!res && use_qpdf) {
                unlink(tf2)
                file.rename(tf, tf2)
                res <- system2(qpdf, c("--stream-data=compress", 
                  "--object-streams=generate", tf2, tf), FALSE, 
                  FALSE)
                unlink(tf2)
            }
        }
        else if (use_qpdf) {
            res <- system2(qpdf, c("--stream-data=compress", 
                "--object-streams=generate", p, tf), FALSE, FALSE)
        }
        if (!res && file.exists(tf)) {
            old <- file.size(p)
            new <- file.size(tf)
            if (new/old < 0.9 && new < old - 10000) {
                file.copy(tf, p, overwrite = TRUE)
                ans[p, ] <- c(old, new)
            }
        }
        unlink(tf)
    }
    structure(stats::na.omit(ans), class = c("compactPDF", "data.frame"))
}


assertError <- function (expr, verbose = FALSE) 
{
    d.expr <- .deparseTrim(substitute(expr), cutoff = 30L)
    tryCatch(res <- assertCondition(expr, "error", .exprString = d.expr), 
        error = function(e) stop(gettextf("Failed to get error in evaluating %s", 
            d.expr), call. = FALSE))
    if (verbose) {
        error <- res[sapply(res, function(cond) "error" %in% 
            class(cond))]
        message(sprintf("Asserted error: %s", error[[1]]$message))
    }
    invisible(res)
}


checkDocFiles <- function (package, dir, lib.loc = NULL) 
{
    if (!missing(package)) {
        if (length(package) != 1L) 
            stop("argument 'package' must be of length 1")
        dir <- find.package(package, lib.loc)
    }
    else {
        if (missing(dir)) 
            stop("you must specify 'package' or 'dir'")
        if (!dir.exists(dir)) 
            stop(gettextf("directory '%s' does not exist", dir), 
                domain = NA)
        else dir <- file_path_as_absolute(dir)
    }
    db <- if (!missing(package)) 
        Rd_db(package, lib.loc = dirname(dir))
    else Rd_db(dir = dir)
    db_aliases <- lapply(db, .Rd_get_metadata, "alias")
    db_keywords <- lapply(db, .Rd_get_metadata, "keyword")
    db_names <- .Rd_get_names_from_Rd_db(db)
    names(db) <- names(db_aliases) <- db_names
    db_usages <- lapply(db, .Rd_get_section, "usage")
    db_usage_texts <- lapply(db_usages, function(e) .Rd_deparse(.Rd_drop_comments(e)))
    db_usages <- lapply(db_usages, .parse_usage_as_much_as_possible)
    ind <- as.logical(sapply(db_usages, function(x) !is.null(attr(x, 
        "bad_lines"))))
    bad_lines <- lapply(db_usages[ind], attr, "bad_lines")
    ind <- sapply(db_keywords, function(x) length(grep("^ *internal *$", 
        x)) > 0L)
    if (any(ind)) {
        db <- db[!ind]
        db_names <- db_names[!ind]
        db_aliases <- db_aliases[!ind]
    }
    db_argument_names <- lapply(db, .Rd_get_argument_names)
    bad_doc_objects <- list()
    for (docObj in db_names) {
        exprs <- db_usages[[docObj]]
        if (!length(exprs)) 
            next
        aliases <- db_aliases[[docObj]]
        arg_names_in_arg_list <- db_argument_names[[docObj]]
        ind <- as.logical(sapply(exprs, function(e) ((length(e) > 
            1L) && !((length(e) == 2L) && e[[1L]] == as.symbol("data")))))
        exprs <- exprs[ind]
        ind <- as.logical(sapply(exprs, .is_call_from_replacement_function_usage))
        replace_exprs <- exprs[ind]
        exprs <- exprs[!ind]
        functions <- as.character(sapply(exprs, function(e) as.character(e[[1L]])))
        ind <- functions %in% c("<-", "=")
        assignments <- exprs[ind]
        if (any(ind)) {
            exprs <- exprs[!ind]
            functions <- functions[!ind]
        }
        arg_names_in_usage <- unlist(sapply(exprs, function(e) .arg_names_from_call(e[-1L])))
        if (length(replace_exprs)) {
            replace_funs <- paste0(vapply(replace_exprs, function(e) as.character(e[[2L]][[1L]]), 
                ""), "<-")
            functions <- c(functions, replace_funs)
            arg_names_in_usage <- c(arg_names_in_usage, unlist(sapply(replace_exprs, 
                function(e) c(.arg_names_from_call(e[[2L]][-1L]), 
                  .arg_names_from_call(e[[3L]])))))
        }
        functions <- .transform_S3_method_markup(functions)
        functions <- .transform_S4_method_markup(functions)
        arg_names_in_usage_missing_in_arg_list <- setdiff(arg_names_in_usage, 
            arg_names_in_arg_list)
        arg_names_in_arg_list_missing_in_usage <- setdiff(arg_names_in_arg_list, 
            arg_names_in_usage)
        if (length(arg_names_in_arg_list_missing_in_usage)) {
            usage_text <- db_usage_texts[[docObj]]
            bad_args <- character()
            bad <- !grepl("^[[:alnum:]._]+$", arg_names_in_arg_list_missing_in_usage)
            if (any(bad)) {
                bad_args <- arg_names_in_arg_list_missing_in_usage[bad]
                arg_names_in_arg_list_missing_in_usage <- arg_names_in_arg_list_missing_in_usage[!bad]
            }
            bad <- sapply(arg_names_in_arg_list_missing_in_usage, 
                function(x) !grepl(paste0("\\b", x, "\\b"), usage_text))
            arg_names_in_arg_list_missing_in_usage <- c(bad_args, 
                arg_names_in_arg_list_missing_in_usage[as.logical(bad)])
        }
        if (!length(grep("-deprecated$", aliases))) {
            aliases <- sub("([^,]+),(.+)-method$", "\\\\S4method{\\1}{\\2}", 
                aliases)
            aliases <- gsub("\\\\%", "%", aliases)
            functions_not_in_aliases <- setdiff(functions, aliases)
        }
        else functions_not_in_aliases <- character()
        if ((length(arg_names_in_usage_missing_in_arg_list)) || 
            anyDuplicated(arg_names_in_arg_list) || (length(arg_names_in_arg_list_missing_in_usage)) || 
            (length(functions_not_in_aliases)) || (length(assignments))) 
            bad_doc_objects[[docObj]] <- list(missing = arg_names_in_usage_missing_in_arg_list, 
                duplicated = arg_names_in_arg_list[duplicated(arg_names_in_arg_list)], 
                overdoc = arg_names_in_arg_list_missing_in_usage, 
                unaliased = functions_not_in_aliases, assignments = assignments)
    }
    structure(bad_doc_objects, class = "checkDocFiles", bad_lines = bad_lines)
}


file_path_as_absolute <- function (x) 
{
    if (length(x) != 1L) 
        stop("'x' must be a single character string")
    if (!file.exists(epath <- path.expand(x))) 
        stop(gettextf("file '%s' does not exist", x), domain = NA)
    normalizePath(epath, "/", TRUE)
}


file_ext <- function (x) 
{
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}


testInstalledBasic <- function (scope = c("basic", "devel", "both", "internet")) 
{
    scope <- match.arg(scope)
    Sys.setlocale("LC_COLLATE", "C")
    tests1 <- c("eval-etc", "simple-true", "arith-true", "lm-tests", 
        "ok-errors", "method-dispatch", "array-subset", "any-all", 
        "d-p-q-r-tests")
    tests2 <- c("complex", "print-tests", "lapack", "datasets", 
        "datetime", "iec60559")
    tests3 <- c("reg-tests-1a", "reg-tests-1b", "reg-tests-1c", 
        "reg-tests-2", "reg-examples1", "reg-examples2", "reg-packages", 
        "p-qbeta-strict-tst", "reg-IO", "reg-IO2", "reg-plot", 
        "reg-S4", "reg-BLAS")
    runone <- function(f, diffOK = FALSE, inC = TRUE) {
        f <- paste(f, "R", sep = ".")
        if (!file.exists(f)) {
            if (!file.exists(fin <- paste0(f, "in"))) 
                stop("file ", sQuote(f), " not found", domain = NA)
            message("creating ", sQuote(f), domain = NA)
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")), 
                "--vanilla --slave -f", fin)
            if (system(cmd)) 
                stop("creation of ", sQuote(f), " failed", domain = NA)
            cat("\n", file = f, append = TRUE)
            on.exit(unlink(f))
        }
        message("  running code in ", sQuote(f), domain = NA)
        outfile <- paste0(f, "out")
        cmd <- paste(shQuote(file.path(R.home("bin"), "R")), 
            "CMD BATCH --vanilla --no-timing", shQuote(f), shQuote(outfile))
        extra <- paste("LANGUAGE=en", "LC_COLLATE=C", "R_DEFAULT_PACKAGES=", 
            "SRCDIR=.")
        if (inC) 
            extra <- paste(extra, "LC_ALL=C")
        if (.Platform$OS.type == "windows") {
            Sys.setenv(LANGUAGE = "C")
            Sys.setenv(R_DEFAULT_PACKAGES = "")
            Sys.setenv(LC_COLLATE = "C")
            Sys.setenv(SRCDIR = ".")
        }
        else cmd <- paste(extra, cmd)
        res <- system(cmd)
        if (res) {
            file.rename(outfile, paste(outfile, "fail", sep = "."))
            message("FAILED")
            return(1L)
        }
        savefile <- paste(outfile, "save", sep = ".")
        if (file.exists(savefile)) {
            message(gettextf("  comparing %s to %s ...", sQuote(outfile), 
                sQuote(savefile)), appendLF = FALSE, domain = NA)
            res <- Rdiff(outfile, savefile, TRUE)
            if (!res) 
                message(" OK")
            else if (!diffOK) 
                return(1L)
        }
        0L
    }
    owd <- setwd(file.path(R.home(), "tests"))
    on.exit(setwd(owd))
    if (scope %in% c("basic", "both")) {
        message("running strict specific tests", domain = NA)
        for (f in tests1) if (runone(f)) 
            return(1L)
        message("running sloppy specific tests", domain = NA)
        for (f in tests2) runone(f, TRUE)
        message("running regression tests", domain = NA)
        for (f in tests3) {
            if (runone(f)) 
                return(invisible(1L))
            if (f == "reg-plot") {
                message("  comparing 'reg-plot.pdf' to 'reg-plot.pdf.save' ...", 
                  appendLF = FALSE, domain = NA)
                res <- Rdiff("reg-plot.pdf", "reg-plot.pdf.save")
                if (res != 0L) 
                  message("DIFFERED")
                else message("OK")
            }
        }
        runone("reg-tests-3", TRUE)
        runone("reg-examples3", TRUE)
        message("running tests of plotting Latin-1", domain = NA)
        message("  expect failure or some differences if not in a Latin or UTF-8 locale", 
            domain = NA)
        runone("reg-plot-latin1", TRUE, FALSE)
        message("  comparing 'reg-plot-latin1.pdf' to 'reg-plot-latin1.pdf.save' ...", 
            appendLF = FALSE, domain = NA)
        res <- Rdiff("reg-plot-latin1.pdf", "reg-plot-latin1.pdf.save")
        if (res != 0L) 
            message("DIFFERED")
        else message("OK")
    }
    if (scope %in% c("devel", "both")) {
        message("running tests of date-time printing\n expect platform-specific differences", 
            domain = NA)
        runone("datetime2")
        message("running tests of consistency of as/is.*", domain = NA)
        runone("isas-tests")
        message("running tests of random deviate generation -- fails occasionally")
        runone("p-r-random-tests", TRUE)
        message("running tests demos from base and stats", domain = NA)
        if (runone("demos")) 
            return(invisible(1L))
        if (runone("demos2")) 
            return(invisible(1L))
        message("running tests of primitives", domain = NA)
        if (runone("primitives")) 
            return(invisible(1L))
        message("running regexp regression tests", domain = NA)
        if (runone("utf8-regex", inC = FALSE)) 
            return(invisible(1L))
        message("running tests to possibly trigger segfaults", 
            domain = NA)
        if (runone("no-segfault")) 
            return(invisible(1L))
    }
    if (scope %in% "internet") {
        message("running tests of Internet functions - expect some differences", 
            domain = NA)
        runone("internet")
        message("running more Internet and socket tests", domain = NA)
        runone("internet2")
        runone("libcurl")
    }
    invisible(0L)
}


SIGKILL <- 9L


list_files_with_exts <- function (dir, exts, all.files = FALSE, full.names = TRUE) 
{
    if (file.exists(file.path(dir, "filelist")) && any(file.exists(file.path(dir, 
        c("Rdata.zip", "Rex.zip", "Rhelp.zip"))))) {
        files <- readLines(file.path(dir, "filelist"))
        if (!all.files) 
            files <- grep("^[^.]", files, value = TRUE)
    }
    else {
        files <- list.files(dir, all.files = all.files)
    }
    patt <- paste0("\\.(", paste(exts, collapse = "|"), ")$")
    files <- grep(patt, files, value = TRUE)
    if (full.names) 
        files <- if (length(files)) 
            file.path(dir, files)
        else character()
    files
}


check_packages_in_dir_changes <- function (dir, old, outputs = FALSE, sources = FALSE) 
{
    check_packages_in_dir_changes_classes <- c("check_packages_in_dir_changes", 
        "data.frame")
    dir <- if (inherits(dir, "check_packages_in_dir")) 
        dir <- attr(dir, "dir")
    else normalizePath(dir)
    outdirs <- R_check_outdirs(dir, all = sources, invert = TRUE)
    logs <- file.path(outdirs, "00check.log")
    logs <- logs[file_test("-f", logs)]
    new <- check_packages_in_dir_details(logs = logs, drop_ok = FALSE)
    if (!inherits(old, "check_details")) 
        old <- check_packages_in_dir_details(old, drop_ok = FALSE)
    packages <- intersect(old$Package, new$Package)
    if (!length(packages)) {
        db <- data.frame(Package = character(), Check = character(), 
            Old = character(), New = character(), stringsAsFactors = FALSE)
        class(db) <- check_packages_in_dir_changes_classes
        return(db)
    }
    db <- merge(old[!is.na(match(old$Package, packages)), ], 
        new[!is.na(match(new$Package, packages)), ], by = c("Package", 
            "Check"), all = TRUE)
    chunks <- lapply(split(db, db$Package), function(e) {
        len <- nrow(e)
        if (length(pos <- which(!is.na(e$Version.x)))) 
            e$Version.x <- rep.int(e[pos[1L], "Version.x"], len)
        if (length(pos <- which(!is.na(e$Version.y)))) 
            e$Version.y <- rep.int(e[pos[1L], "Version.y"], len)
        e
    })
    db <- do.call(rbind, chunks)
    x.issue <- !is.na(match(db$Status.x, c("NOTE", "ERROR", "WARNING")))
    y.issue <- !is.na(match(db$Status.y, c("NOTE", "ERROR", "WARNING")))
    db <- db[x.issue | y.issue, ]
    sx <- as.character(db$Status.x)
    sy <- as.character(db$Status.y)
    if (outputs) {
        sx <- sprintf("%s\n  %s", sx, gsub("\n", "\n  ", db$Output.x, 
            fixed = TRUE))
        sy <- sprintf("%s\n  %s", sy, gsub("\n", "\n  ", db$Output.y, 
            fixed = TRUE))
    }
    sx[is.na(db$Status.x)] <- ""
    sy[is.na(db$Status.y)] <- ""
    ind <- if (outputs) 
        (.canonicalize_quotes(sx) != .canonicalize_quotes(sy))
    else (sx != sy)
    db <- cbind(db[ind, ], Old = sx[ind], New = sy[ind], stringsAsFactors = FALSE)
    ind <- (db$Version.x != db$Version.y)
    if (any(ind)) 
        db$Package[ind] <- sprintf("%s [Old version: %s, New version: %s]", 
            db$Package[ind], db$Version.x[ind], db$Version.y[ind])
    db <- db[c("Package", "Check", "Old", "New")]
    class(db) <- check_packages_in_dir_changes_classes
    db
}


showNonASCII <- function (x) 
{
    asc <- iconv(x, "latin1", "ASCII")
    ind <- is.na(asc) | asc != x
    if (any(ind)) 
        message(paste0(which(ind), ": ", iconv(x[ind], "latin1", 
            "ASCII", sub = "byte"), collapse = "\n"), domain = NA)
    invisible(x[ind])
}


loadRdMacros <- function (file, macros = TRUE) 
{
    if (is.logical(macros) && !macros) 
        stop("'macros' must be TRUE or must specify existing macros")
    Rd <- parse_Rd(file, fragment = TRUE, macros = macros, warningCalls = FALSE)
    for (entry in Rd) {
        bad <- TRUE
        if (is.list(entry)) 
            break
        tag <- attr(entry, "Rd_tag")
        switch(tag, TEXT = if (any(grepl("[^[:space:]]", entry, 
            perl = TRUE, useBytes = TRUE))) break else bad <- FALSE, 
            USERMACRO = , `\\newcommand` = , `\\renewcommand` = , 
            COMMENT = bad <- FALSE, break)
    }
    if (bad) 
        warning(gettextf("Macro file %s should only contain Rd macro definitions and comments", 
            file))
    attr(Rd, "macros")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Tools for Package Development"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF