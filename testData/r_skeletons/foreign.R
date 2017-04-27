##
## Exported symobls in package `foreign`
##

## Exported package methods

write.foreign <- function (df, datafile, codefile, package = c("SPSS", "Stata", 
    "SAS"), ...) 
{
    do.call(paste("writeForeign", package, sep = ""), c(list(df = df, 
        datafile = datafile, codefile = codefile), ...))
    invisible(NULL)
}


read.dbf <- function (file, as.is = FALSE) 
{
    df <- .Call(Rdbfread, as.character(path.expand(file)))
    onames <- names(df)
    inames <- make.names(onames, unique = TRUE)
    names(df) <- inames
    if (!(identical(onames, inames))) {
        for (i in seq_along(onames)) if (!(identical(onames[i], 
            inames[i]))) 
            message(gettextf("Field name: %s changed to: %s", 
                sQuote(onames[i]), sQuote(inames[i])), domain = NA)
    }
    data_types <- attr(df, "data_types")
    for (i in seq_along(onames)) if (data_types[i] == "D") 
        df[[i]] <- as.Date(df[[i]], format = "%Y%m%d")
    if (!as.is) {
        df <- data.frame(lapply(df, function(x) if (is.character(x)) 
            factor(x)
        else x))
        attr(df, "data_types") <- data_types
    }
    df
}


lookup.xport <- function (file) 
.Call(xport_info, file)


read.dta <- function (file, convert.dates = TRUE, convert.factors = TRUE, 
    missing.type = FALSE, convert.underscore = FALSE, warn.missing.labels = TRUE) 
{
    if (length(grep("^(http|ftp|https)://", file))) {
        tmp <- tempfile()
        download.file(file, tmp, quiet = TRUE, mode = "wb")
        file <- tmp
        on.exit(unlink(file))
    }
    rval <- .External(do_readStata, file)
    if (convert.underscore) 
        names(rval) <- gsub("_", ".", names(rval))
    types <- attr(rval, "types")
    stata.na <- data.frame(type = 251L:255L, min = c(101, 32741, 
        2147483621, 2^127, 2^1023), inc = c(1, 1, 1, 2^115, 2^1011))
    if (!missing.type) {
        if (abs(attr(rval, "version")) >= 8L) {
            for (v in which(types > 250L)) {
                this.type <- types[v] - 250L
                rval[[v]][rval[[v]] >= stata.na$min[this.type]] <- NA
            }
        }
    }
    else {
        if (abs(attr(rval, "version")) >= 8L) {
            missings <- vector("list", length(rval))
            names(missings) <- names(rval)
            for (v in which(types > 250L)) {
                this.type <- types[v] - 250L
                nas <- is.na(rval[[v]]) | rval[[v]] >= stata.na$min[this.type]
                natype <- (rval[[v]][nas] - stata.na$min[this.type])/stata.na$inc[this.type]
                natype[is.na(natype)] <- 0L
                missings[[v]] <- rep(NA, NROW(rval))
                missings[[v]][nas] <- natype
                rval[[v]][nas] <- NA
            }
            attr(rval, "missing") <- missings
        }
        else warning("'missing.type' only applicable to version >= 8 files")
    }
    convert_dt_c <- function(x) as.POSIXct((x + 0.1)/1000, origin = "1960-01-01")
    convert_dt_C <- function(x) {
        ls <- .leap.seconds + seq_along(.leap.seconds) + 315619200
        z <- (x + 0.1)/1000
        z <- z - rowSums(outer(z, ls, ">="))
        as.POSIXct(z, origin = "1960-01-01")
    }
    if (convert.dates) {
        ff <- attr(rval, "formats")
        dates <- if (attr(rval, "version") >= 8L) 
            grep("^%(-|)(d|td)", ff)
        else grep("%-*d", ff)
        base <- structure(-3653L, class = "Date")
        for (v in dates) rval[[v]] <- structure(base + rval[[v]], 
            class = "Date")
        for (v in grep("%tc", ff)) rval[[v]] <- convert_dt_c(rval[[v]])
        for (v in grep("%tC", ff)) rval[[v]] <- convert_dt_C(rval[[v]])
    }
    if (convert.factors %in% c(TRUE, NA)) {
        if (attr(rval, "version") == 5L) 
            warning("cannot read factor labels from Stata 5 files")
        else {
            ll <- attr(rval, "val.labels")
            tt <- attr(rval, "label.table")
            factors <- which(ll != "")
            for (v in factors) {
                labels <- tt[[ll[v]]]
                if (warn.missing.labels && is.null(labels)) {
                  warning(gettextf("value labels (%s) for %s are missing", 
                    sQuote(ll[v]), sQuote(names(rval)[v])), domain = NA)
                  next
                }
                if (!is.na(convert.factors)) {
                  if (!all(rval[[v]] %in% c(NA, NaN, tt[[ll[v]]]))) 
                    next
                }
                rval[[v]] <- factor(rval[[v]], levels = tt[[ll[v]]], 
                  labels = names(tt[[ll[v]]]))
            }
        }
    }
    att <- attributes(rval)
    class(rval) <- "data.frame"
    newatt <- attributes(rval)
    newatt <- c(newatt, att[!(names(att) %in% names(newatt))])
    attributes(rval) <- newatt
    rval
}


write.dta <- function (dataframe, file, version = 7L, convert.dates = TRUE, 
    tz = "GMT", convert.factors = c("labels", "string", "numeric", 
        "codes")) 
{
    if (!is.data.frame(dataframe)) 
        stop("The object \"dataframe\" must have class data.frame")
    if (version < 6L) 
        stop("Version must be 6-12")
    if (version == 9L) 
        version <- 8L
    if (version == 11L) 
        version <- 10L
    if (version == 12L) 
        version <- 10L
    if (version > 12L) {
        warning("Version must be 6-12: using 7")
        version <- 7L
    }
    namelength <- if (version == 6L) 
        8L
    else 31L
    oldn <- names(dataframe)
    nn <- abbreviate(oldn, namelength)
    if (any(nchar(nn) > namelength)) 
        stop("cannot uniquely abbreviate variable names")
    if (any(nchar(oldn) > namelength)) 
        warning("abbreviating variable names")
    names(dataframe) <- nn
    attr(dataframe, "orig.names") <- oldn
    if (convert.dates) {
        dates <- which(sapply(dataframe, function(x) inherits(x, 
            "Date")))
        for (v in dates) dataframe[[v]] <- as.vector(julian(dataframe[[v]], 
            as.Date("1960-1-1", tz = "GMT")))
        dates <- which(sapply(dataframe, function(x) inherits(x, 
            "POSIXt")))
        for (v in dates) dataframe[[v]] <- as.vector(round(julian(dataframe[[v]], 
            ISOdate(1960, 1, 1, tz = tz))))
    }
    convert.factors <- match.arg(convert.factors)
    factors <- which(sapply(dataframe, is.factor))
    if (convert.factors == "string") {
        for (v in factors) dataframe[[v]] <- I(as.character(dataframe[[v]]))
    }
    else if (convert.factors == "numeric") {
        for (v in factors) dataframe[[v]] <- as.numeric(as.character(dataframe[[v]]))
    }
    else if (convert.factors == "codes") {
        for (v in factors) dataframe[[v]] <- as.numeric(dataframe[[v]])
    }
    shortlevels <- function(f) {
        ll <- levels(f)
        if (is.null(ll)) 
            return(NULL)
        if (all(nchar(ll, "bytes") <= 80L)) 
            ll
        else abbreviate(ll, 80L)
    }
    leveltable <- lapply(dataframe, shortlevels)
    if (any(sapply(dataframe, function(x) {
        d <- dim(x)
        !is.null(d) && d[1L] < length(x)
    }))) 
        stop("cannot handle multicolumn columns")
    invisible(.External(do_writeStata, file, dataframe, version, 
        leveltable))
}


data.restore <- function (file, print = FALSE, verbose = FALSE, env = .GlobalEnv) 
{
    dump <- file(file, open = "rt")
    on.exit(close(dump))
    ReadSdump <- function(top = FALSE, prefix) {
        name <- readLines(dump, 1L)
        if (length(name) == 0L) 
            return(NULL)
        code <- readLines(dump, 1L)
        len <- as.integer(readLines(dump, 1L))
        if (top && print) 
            cat("\"", name, "\": ", code, "\n", sep = "")
        if (verbose) 
            cat(prefix, summary(dump)$position, name, code, len, 
                "\n")
        if (code %in% c("logical", "numeric", "integer", "single", 
            "double", "character", "name", "missing", "complex")) {
            value <- readLines(dump, len)
            value[value == "N"] <- as.character(NA)
            if (code != "character") 
                value <- if (code == "logical") 
                  as.logical(as.integer(value))
                else if (code %in% c("integer", "double", "name", 
                  "complex")) 
                  methods::as(value, code)
                else if (code %in% c("numeric", "single")) 
                  as.numeric(value)
                else if (code == "missing") 
                  call("stop", paste("Argument ", sQuote(name), 
                    " is missing", sep = ""))
        }
        else if (code %in% c("list", "structure", "NULL", SModeNames)) {
            value <- list()
            for (i in seq_len(len)) {
                temp <- ReadSdump(FALSE, c(prefix, " "))
                value[[nam.or.i(temp$name, i)]] <- temp$value
            }
            if (code == "structure") {
                thelist <- value
                value <- thelist[[".Data"]]
                attributes(value) <- thelist[-match(c(".Data", 
                  ".Dim", ".Dimnames", ".Label"), names(thelist), 
                  nomatch = 0L)]
                dim(value) <- thelist[[".Dim"]]
                names(value) <- names(thelist[[".Data"]])
                if (!is.null(thelist[[".Label"]])) 
                  levels(value) <- thelist[[".Label"]]
                if (!is.null(thelist[[".Dimnames"]])) 
                  try(dimnames(value) <- thelist[[".Dimnames"]])
                if (!is.null(tsp <- thelist[[".Tsp"]])) 
                  try(value <- stats::ts(c(value), tsp[1L], tsp[2L], 
                    tsp[3L]))
            }
            else if (code == "function") 
                try(value <- as.function(value, env = env))
            else if (code %in% c("break", "if", "for", "return", 
                "S.call", "while", "<-", "<<-", "(", "{")) 
                value <- as.call(c(as.name(code), value))
            else if (code == "NULL") {
                value <- if (name != "") 
                  as.name(name)
            }
            else if (code == "call(...)") 
                value <- value[[1L]]
            else if (code == "comment") 
                value <- NULL
            else if (code == "comment.expression") 
                value <- value[unlist(lapply(value, function(y) !is.null(y)))][[1L]]
            else if (code == "internal") 
                value <- as.call(list(as.name(".Internal"), value[[1L]]))
            else try(mode(value) <- code)
        }
        else {
            stop(gettextf("S mode %s (near byte offset %s) not supported", 
                sQuote(code), seek(dump)), domain = NA)
        }
        list(name = name, value = value)
    }
    repeat {
        temp <- ReadSdump(TRUE, " ")
        if (is.null(temp)) 
            break
        assign(temp$name, temp$value, envir = env)
    }
    file
}


read.mtp <- function (file) 
{
    clean <- function(x) if (is.numeric(x)) 
        signif(x, 6L)
    else x
    val <- .Call(read_mtp, file)
    lapply(val, clean)
}


read.arff <- function (file) 
{
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("Argument 'file' must be a character string or connection.")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    col_names <- NULL
    col_types <- NULL
    col_dfmts <- character()
    line <- readLines(file, n = 1L)
    while (length(line) && regexpr("^[[:space:]]*@(?i)data", 
        line, perl = TRUE) == -1L) {
        if (regexpr("^[[:space:]]*@(?i)attribute", line, perl = TRUE) > 
            0L) {
            con <- textConnection(line)
            line <- scan(con, character(), quiet = TRUE)
            close(con)
            if (length(line) < 3L) 
                stop("Invalid attribute specification.")
            col_names <- c(col_names, line[2L])
            if ((type <- tolower(line[3L])) == "date") {
                col_types <- c(col_types, "character")
                col_dfmts <- c(col_dfmts, if (length(line) > 
                  3L) ISO_8601_to_POSIX_datetime_format(line[4L]) else "%Y-%m-%d %H:%M:%S")
            }
            else if (type == "relational") 
                stop("Type 'relational' currently not implemented.")
            else {
                type <- sub("\\{.*", "factor", type)
                type <- sub("string", "character", type)
                type <- sub("real", "numeric", type)
                col_types <- c(col_types, type)
                col_dfmts <- c(col_dfmts, NA)
            }
        }
        line <- readLines(file, n = 1L)
    }
    if (length(line) == 0L) 
        stop("Missing data section.")
    if (is.null(col_names)) 
        stop("Missing attribute section.")
    if (length(col_names) != length(grep("factor|numeric|character", 
        col_types))) 
        stop("Invalid type specification.")
    data <- read.table(file, sep = ",", na.strings = "?", colClasses = col_types, 
        comment.char = "%")
    if (any(ind <- which(!is.na(col_dfmts)))) 
        for (i in ind) data[i] <- as.data.frame(strptime(data[[i]], 
            col_dfmts[i]))
    for (i in seq_len(length(data))) if (is.factor(data[[i]])) 
        levels(data[[i]]) <- gsub("\\\\", "", levels(data[[i]]))
    names(data) <- col_names
    data
}


write.dbf <- function (dataframe, file, factor2char = TRUE, max_nchar = 254) 
{
    allowed_classes <- c("logical", "integer", "numeric", "character", 
        "factor", "Date")
    if (!is.data.frame(dataframe)) 
        dataframe <- as.data.frame(dataframe)
    if (any(sapply(dataframe, function(x) !is.null(dim(x))))) 
        stop("cannot handle matrix/array columns")
    cl <- sapply(dataframe, function(x) class(x[1L]))
    asis <- cl == "AsIs"
    cl[asis & sapply(dataframe, mode) == "character"] <- "character"
    if (length(cl0 <- setdiff(cl, allowed_classes))) 
        stop(sprintf(ngettext(length(cl0), "data frame contains columns of unsupported class %s", 
            "data frame contains columns of unsupported classes %s"), 
            paste(dQuote(cl0), collapse = ",")), domain = NA)
    m <- ncol(dataframe)
    DataTypes <- c(logical = "L", integer = "N", numeric = "F", 
        character = "C", factor = if (factor2char) "C" else "N", 
        Date = "D")[cl]
    for (i in seq_len(m)) {
        x <- dataframe[[i]]
        if (is.factor(x)) 
            dataframe[[i]] <- if (factor2char) 
                as.character(x)
            else as.integer(x)
        else if (inherits(x, "Date")) 
            dataframe[[i]] <- format(x, "%Y%m%d")
    }
    precision <- integer(m)
    scale <- integer(m)
    dfnames <- names(dataframe)
    for (i in seq_len(m)) {
        nlen <- nchar(dfnames[i], "b")
        x <- dataframe[, i]
        if (is.logical(x)) {
            precision[i] <- 1L
            scale[i] <- 0L
        }
        else if (is.integer(x)) {
            rx <- range(x, na.rm = TRUE)
            rx[!is.finite(rx)] <- 0
            if (any(rx == 0)) 
                rx <- rx + 1
            mrx <- as.integer(max(ceiling(log10(abs(rx)))) + 
                3L)
            precision[i] <- min(max(nlen, mrx), 19L)
            scale[i] <- 0L
        }
        else if (is.double(x)) {
            precision[i] <- 19L
            rx <- range(x, na.rm = TRUE)
            rx[!is.finite(rx)] <- 0
            mrx <- max(ceiling(log10(abs(rx))))
            scale[i] <- min(precision[i] - ifelse(mrx > 0L, mrx + 
                3L, 3L), 15L)
        }
        else if (is.character(x)) {
            mf <- max(nchar(x[!is.na(x)], "b"))
            p <- max(nlen, mf)
            if (p > max_nchar) 
                warning(gettextf("character column %d will be truncated to %d bytes", 
                  i, max_nchar), domain = NA)
            precision[i] <- min(p, max_nchar)
            scale[i] <- 0L
        }
        else stop("unknown column type in data frame")
    }
    if (any(is.na(precision))) 
        stop("NA in precision")
    if (any(is.na(scale))) 
        stop("NA in scale")
    invisible(.Call(DoWritedbf, as.character(file), dataframe, 
        as.integer(precision), as.integer(scale), as.character(DataTypes)))
}


read.octave <- function (file) 
{
    skip_lines_to_next_item <- function(con) {
        looking <- TRUE
        while (looking) {
            line <- readLines(con, n = 1L)
            if (length(grep("^# name: ", line)) == 1L) {
                pushBack(line, con)
                looking <- FALSE
            }
            else if (length(line) == 0L) {
                looking <- FALSE
            }
        }
    }
    read_octave_matrix <- function(con) {
        line <- readLines(con, 1L)
        if (regexpr("^# rows:", line) > 0L) {
            nr <- as.integer(gsub("# rows: ", "", line))
            nc <- as.integer(gsub("# columns: ", "", readLines(con, 
                1L)))
            data <- scan(con, nlines = nr, quiet = TRUE)
            matrix(data, nrow = nr, ncol = nc, byrow = TRUE)
        }
        else {
            dims <- scan(con, nlines = 1L, quiet = TRUE)
            data <- scan(con, n = prod(dims), quiet = TRUE)
            array(data, dim = dims)
        }
    }
    read_octave_complex_matrix <- function(con) {
        line <- readLines(con, 1L)
        if (regexpr("^# rows:", line) > 0L) {
            nr <- as.integer(gsub("# rows: ", "", line))
            nc <- as.integer(gsub("# columns: ", "", readLines(con, 
                1L)))
            data <- readLines(con, n = nr)
            cl <- paste(data, sep = "", collapse = "")
            c1 <- gsub("\\(", "", cl)
            c1 <- gsub("\\)", "", c1)
            c1 <- gsub(",", " ", c1)
            s <- unlist(strsplit(c1, " "))
            nums <- as.numeric(s[-1L])
            reals <- nums[seq.int(from = 1L, by = 2L, length.out = length(nums)/2)]
            imags <- nums[seq.int(from = 2L, by = 2L, length.out = length(nums)/2)]
            matrix(data = complex(real = reals, imaginary = imags), 
                nrow = nr, ncol = nc, byrow = TRUE)
        }
        else {
            dims <- scan(con, nlines = 1L, quiet = TRUE)
            data <- readLines(con, n = prod(dims))
            data <- gsub("\\(", "", data)
            data <- gsub("\\)", "", data)
            nums <- strsplit(data, ",")
            stopifnot(all(sapply(nums, length) == 2))
            array(complex(real = as.numeric(sapply(nums, "[", 
                1L)), imaginary = as.numeric(sapply(nums, "[", 
                2L))), dim = dims)
        }
    }
    read_octave_string_array <- function(con) {
        elements <- as.numeric(gsub("# elements: ", "", readLines(con, 
            1L)))
        d <- readLines(con, n = 2L * elements)
        d[seq.int(from = 2L, by = 2L, length.out = length(d)/2L)]
    }
    read_octave_scalar <- function(con) {
        as.numeric(scan(con, nlines = 1L, quiet = TRUE))
    }
    read_octave_complex_scalar <- function(con) {
        d <- readLines(con, n = 1L)
        str <- gsub("\\(", "", d)
        str <- gsub("\\)", "", str)
        nums <- as.numeric(unlist(strsplit(str, ",")))
        stopifnot(length(nums) == 2L)
        complex(real = nums[1L], imaginary = nums[2L])
    }
    read_octave_range <- function(con) {
        d <- readLines(con, n = 1L)
        d <- as.numeric(scan(con, nlines = 1L, quiet = TRUE))
        stopifnot(length(d) == 3L)
        seq.int(from = d[1L], to = d[2L], by = d[3L])
    }
    read_octave_unknown <- function(con, type) {
        warning(gettextf("cannot handle unknown type %s", sQuote(type)), 
            domain = NA)
        skip_lines_to_next_item(con)
        NULL
    }
    read_octave_list <- function(con) {
        n <- as.numeric(gsub("# length: ", "", readLines(con, 
            1L)))
        out <- vector("list", n)
        for (i in seq_len(n)) {
            readLines(con, 1L)
            out[[i]] <- read_item(con)
        }
        out
    }
    read_octave_cell <- function(con) {
        nr <- as.numeric(gsub("# rows: ", "", readLines(con, 
            1L)))
        nc <- as.numeric(gsub("# columns: ", "", readLines(con, 
            1L)))
        out <- vector("list", nr * nc)
        dim(out) <- c(nr, nc)
        for (j in seq_len(nc)) {
            for (i in seq_len(nr)) {
                readLines(con, 1L)
                out[[i, j]] <- read_item(con)
            }
            skip_lines_to_next_item(con)
        }
        out
    }
    read_octave_struct <- function(con) {
        n <- as.numeric(gsub("# length: ", "", readLines(con, 
            1L)))
        out <- vector("list", n)
        for (i in seq_len(n)) {
            name <- gsub("# name: ", "", readLines(con, 1L))
            out[[i]] <- read_item(con)
            names(out)[i] <- name
        }
        out
    }
    read_octave_bool <- function(con) {
        as.logical(scan(con, nlines = 1L, quiet = TRUE))
    }
    read_octave_bool_matrix <- function(con) {
        nr <- as.integer(gsub("# rows: ", "", readLines(con, 
            1L)))
        nc <- as.integer(gsub("# columns: ", "", readLines(con, 
            1L)))
        data <- scan(con, nlines = nr, quiet = TRUE)
        matrix(as.logical(data), nrow = nr, ncol = nc, byrow = TRUE)
    }
    read_item <- function(con) {
        type <- gsub("# type: ", "", readLines(con, 1L))
        switch(type, matrix = read_octave_matrix(con), scalar = read_octave_scalar(con), 
            string = read_octave_string_array(con), `string array` = read_octave_string_array(con), 
            range = read_octave_range(con), `complex matrix` = read_octave_complex_matrix(con), 
            `complex scalar` = read_octave_complex_scalar(con), 
            list = read_octave_list(con), cell = read_octave_cell(con), 
            struct = read_octave_struct(con), bool = read_octave_bool(con), 
            `bool matrix` = read_octave_bool_matrix(con), read_octave_unknown(con, 
                type))
    }
    zz <- file(file, "r")
    on.exit(close(zz))
    readLines(zz, n = 1L)
    items <- list()
    names <- character()
    reading <- TRUE
    while (reading) {
        line <- readLines(zz, 1L, ok = TRUE)
        if (length(line) == 0L) {
            reading <- FALSE
        }
        else {
            items <- c(items, list(read_item(zz)))
            names <- c(names, gsub("# name: ", "", line))
        }
    }
    names(items) <- names
    items
}


read.epiinfo <- function (file, read.deleted = FALSE, guess.broken.dates = FALSE, 
    thisyear = NULL, lower.case.names = FALSE) 
{
    if (is.character(file)) {
        file <- file(file, "rt")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("argument 'file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "rt")
        on.exit(close(file))
    }
    line <- readLines(file, 1L, ok = TRUE)
    headerlength <- na.omit(sapply(strsplit(line, " ")[[1L]], 
        as.numeric))[1L]
    if (headerlength <= 0L) 
        stop("file has zero or fewer variables: probably not an EpiInfo file")
    headerlines <- readLines(file, n = headerlength)
    pushBack(headerlines, file)
    comments <- sapply(headerlines, function(s) substring(s, 
        46L, 46L + 80L))
    header <- scan(file, nlines = headerlength, what = list(name = "", 
        x = 0, y = 0, color = 0, x1 = 0, y1 = 0, type = 0, len = 0, 
        color = 0), flush = TRUE, quiet = TRUE, comment.char = "")
    header <- as.data.frame(lapply(header, I))
    header$start <- cumsum(c(1L, header$len))[1L:headerlength]
    header$stop <- cumsum(header$len)
    multiline <- ceiling(max(header$stop)/78L)
    really.variables <- header$len != 0
    header <- header[really.variables, ]
    entrychar <- substr(header$name, 1L, 1L)
    if (all(entrychar %in% c("#", "_"))) 
        header$name <- substr(header$name, 2L, 12L)
    comments <- comments[really.variables]
    numbers <- (header$len > 0L) & ((header$type %in% c(0L, 6L, 
        12L)) | (header$type > 12L)) & !(header$type %in% c(16L, 
        17L))
    datalines <- scan(file, what = "", sep = "\n", quote = "", 
        quiet = TRUE, blank.lines.skip = TRUE, comment.char = "")
    if (length(datalines) == 0L) 
        stop("no records in file")
    if (length(datalines)%%multiline) 
        warning("wrong number of records")
    datalines <- matrix(datalines, nrow = multiline)
    if (multiline > 1L) 
        datalines[-multiline, ] <- substr(datalines[-multiline, 
            ], 1L, 78L)
    datalines <- apply(datalines, 2L, paste, collapse = "")
    deleted <- substr(datalines, nchar(datalines), nchar(datalines)) == 
        "?"
    nvars <- NROW(header)
    data <- as.data.frame(lapply(1L:nvars, function(i) I(substring(datalines, 
        header$start[i], header$stop[i]))))
    names(data) <- header$name
    names(comments) <- header$name
    if (is.na(read.deleted)) 
        data[deleted, ] <- NA
    else if (!read.deleted) 
        data <- data[!deleted, ]
    if (guess.broken.dates && is.null(thisyear)) 
        thisyear <- format(Sys.time(), format = "%Y")
    for (i in 1L:nvars) {
        if (numbers[i]) 
            data[[i]] <- as.numeric(data[[i]])
        else if (header$type[i] == 5L) 
            data[[i]] <- ifelse(data[[i]] %in% c("Y", "N"), data[[i]] == 
                "Y", NA)
        else if (header$type[i] %in% c(11L, 16L) && header$len[i] == 
            5L && guess.broken.dates) 
            data[[i]] <- as.Date(strptime(paste(data[[i]], thisyear, 
                sep = "/"), format = "%d/%m/%Y"))
        else if (header$type[i] %in% c(11L, 16L) && header$len[i] == 
            8L && guess.broken.dates) 
            data[[i]] <- as.Date(strptime(data[[i]], format = "%d/%m/%y"))
        else if (header$type[i] %in% c(11L, 16L) && header$len[i] == 
            10L) 
            data[[i]] <- as.Date(strptime(data[[i]], format = "%d/%m/%Y"))
        else if (header$type[i] %in% c(2L, 10L) && header$len[i] == 
            5L && guess.broken.dates) 
            data[[i]] <- as.Date(strptime(paste(data[[i]], thisyear, 
                sep = "/"), format = "%m/%d/%Y"))
        else if (header$type[i] %in% c(2L, 10L) && header$len[i] == 
            8L && guess.broken.dates) 
            data[[i]] <- as.Date(strptime(data[[i]], format = "%m/%d/%y"))
        else if (header$type[i] %in% c(2L, 10L) && header$len[i] == 
            10L) 
            data[[i]] <- as.Date(strptime(data[[i]], format = "%m/%d/%Y"))
        else if (header$type[i] == 17L) {
            data[[i]][substr(data[[i]], 1L, 1L) == " "] <- NA
            data[[i]] <- substr(data[[i]], 1L, 5L)
        }
        else {
            blanks <- grep("^[[:blank:]]*$", data[[i]])
            data[[i]][blanks] <- NA
        }
    }
    if (!is.na(read.deleted) && read.deleted) 
        attr(data, "deleted") <- deleted
    attr(data, "prompts") <- comments
    if (lower.case.names) 
        names(data) <- tolower(names(data))
    data
}


read.ssd <- function (libname, sectionnames, tmpXport = tempfile(), tmpProgLoc = tempfile(), 
    sascmd = "sas") 
{
    tmpFiles <- tmpXport
    on.exit(unlink(tmpFiles))
    logGuess <- function(x) {
        expl <- strsplit(x, "")[[1L]]
        rex <- rev(expl)
        br <- match("/", rex)[1L]
        if (is.na(br)) 
            return(x)
        return(paste(rev(rex[1L:(br - 1L)]), sep = "", collapse = ""))
    }
    fileExtension <- function(string) {
        n <- nchar(string)
        chars <- substring(string, 1L:n, 1L:n)
        lastDot <- n + 1L - match(".", rev(chars), nomatch = n + 
            1L)
        substring(string, lastDot + 1L, n)
    }
    sn <- sectionnames
    if (any(nchar(sn) > 8L)) {
        oldDir <- libname
        libname <- tempdir()
        allFiles <- list.files(oldDir)
        oldNames <- character(0L)
        for (i in 1L:length(sn)) {
            fName <- grep(sn[i], allFiles, value = TRUE)
            if (length(fName) == 0L) 
                stop(gettextf("sectionname %s not found", sn[i]), 
                  domain = NA)
            oldNames <- c(oldNames, fName)
        }
        sectionnames <- linkNames <- character(length(oldNames))
        for (i in 1L:length(oldNames)) {
            sectionnames[i] <- paste("sn", i, sep = "")
            linkNames[i] <- paste(sectionnames[i], fileExtension(oldNames[i]), 
                sep = ".")
            oldPath <- file.path(oldDir, oldNames[i])
            linkPath <- file.path(libname, linkNames[i])
            file.symlink(oldPath, linkPath)
            tmpFiles <- c(tmpFiles, linkPath)
        }
    }
    st0 <- "option validvarname = v6;"
    st1 <- paste("libname src2rd '", libname, "';\n", sep = "")
    st2 <- paste("libname rd xport '", tmpXport, "';\n", sep = "")
    st3 <- paste("proc copy in=src2rd out=rd;\n")
    st4 <- paste("select", sectionnames, ";\n", sep = " ")
    tmpProg <- paste(tmpProgLoc, ".sas", sep = "")
    tmpProgLogBase <- logGuess(tmpProgLoc)
    tmpProgLog <- paste(tmpProgLogBase, ".log", sep = "")
    cat(st0, file = tmpProg)
    cat(st1, file = tmpProg, append = TRUE)
    cat(st2, file = tmpProg, append = TRUE)
    cat(st3, file = tmpProg, append = TRUE)
    cat(st4, file = tmpProg, append = TRUE)
    if (.Platform$OS.type == "windows") 
        sascmd <- paste(shQuote(sascmd), "-sysin")
    sasrun <- try(sysret <- system(paste(sascmd, tmpProg)))
    if (!inherits(sasrun, "try-error") & sysret == 0L) {
        unlink(tmpProg)
        unlink(tmpProgLog)
        if (length(sectionnames) == 1L) 
            return(read.xport(tmpXport))
        else {
            zz <- read.xport(tmpXport)
            names(zz) <- sn
            return(zz)
        }
    }
    else {
        cat("SAS failed.  SAS program at", tmpProg, "\n")
        if (.Platform$OS.type == "unix") {
            cat("a log and other error products should be in the vicinity\n")
            system(paste("ls -l ", tmpProgLog))
        }
        else {
            cat("The log file will be ", paste(basename(tmpProgLoc), 
                ".log", sep = ""), " in the current directory\n", 
                sep = "")
        }
        warning(gettextf("SAS return code was %d", sysret), domain = NA)
        return(NULL)
    }
}


read.systat <- function (file, to.data.frame = TRUE) 
{
    if (length(file) != 1L) 
        stop("only one file")
    if (!is.character(file)) 
        stop("'file' must be character")
    res <- .Call(readSystat, as.character(file))
    if (to.data.frame) {
        comment <- NULL
        if (!is.null(attr(res, "comment")) && nzchar(attr(res, 
            "comment"))) 
            comment <- attr(res, "comment")
        res <- as.data.frame(res)
        if (!is.null(comment)) 
            comment(res) <- comment
    }
    res
}


read.spss <- function (file, use.value.labels = TRUE, to.data.frame = FALSE, 
    max.value.labels = Inf, trim.factor.names = FALSE, trim_values = TRUE, 
    reencode = NA, use.missings = to.data.frame) 
{
    trim <- function(strings, trim = TRUE) if (trim) 
        sub(" +$", "", strings)
    else strings
    knownCP <- c(`UCS-2LE` = 1200, `UCS-2BE` = 1201, macroman = 10000, 
        ` UCS-4LE` = 12000, `UCS-4BE` = 12001, `koi8-r` = 20866, 
        `koi8-u` = 21866, latin1 = 28591, latin2 = 28592, latin3 = 28593, 
        latin4 = 28594, `latin-9` = 28605, `ISO-2022-JP` = 50221, 
        `euc-jp` = 51932, `UTF-8` = 65001, ASCII = 20127, CP1250 = 1250, 
        CP1251 = 1251, CP1252 = 1252, CP1253 = 1253, CP1254 = 1254, 
        CP1255 = 1255, CP1256 = 1256, CP1257 = 1257, CP1258 = 1258, 
        CP874 = 874, CP936 = 936)
    if (length(grep("^(http|ftp|https)://", file))) {
        tmp <- tempfile()
        download.file(file, tmp, quiet = TRUE, mode = "wb")
        file <- tmp
        on.exit(unlink(file))
    }
    rval <- .Call(do_read_SPSS, file)
    codepage <- attr(rval, "codepage")
    if (is.null(codepage)) 
        codepage <- 2
    if (!capabilities("iconv")) 
        reencode <- FALSE
    if (!identical(reencode, FALSE)) {
        cp <- "unknown"
        if (is.character(reencode)) {
            cp <- reencode
            reencode <- TRUE
        }
        else if (codepage == 20127) {
            reencode <- FALSE
        }
        else if (m <- match(codepage, knownCP, 0L)) {
            cp <- names(knownCP)[m]
        }
        else if (codepage < 200) {
            attr(rval, "codepage") <- NULL
            reencode <- FALSE
        }
        else cp <- paste("CP", codepage, sep = "")
        if (is.na(reencode)) 
            reencode <- l10n_info()[["UTF-8"]] && (codepage != 
                65001)
        if (reencode) {
            message(gettextf("re-encoding from %s", cp), domain = NA)
            names(rval) <- iconv(names(rval), cp, "")
            vl <- attr(rval, "variable.labels")
            nm <- names(vl)
            vl <- iconv(vl, cp, "")
            names(vl) <- iconv(nm, cp, "")
            attr(rval, "variable.labels") <- vl
            for (i in seq_along(rval)) {
                xi <- rval[[i]]
                if (is.character(xi)) 
                  rval[[i]] <- iconv(xi, cp, "")
            }
        }
    }
    miss <- attr(rval, "missings")
    vl <- attr(rval, "label.table")
    if (!is.null(miss)) {
        if (reencode) {
            nm <- names(miss)
            names(miss) <- iconv(nm, cp, "")
            for (i in seq_along(miss)) if (is.character(miss[[i]]$value)) 
                miss[[i]]$value <- iconv(miss[[i]]$value, cp, 
                  "")
            attr(rval, "missings") <- miss
        }
        if (use.missings) 
            for (v in names(rval)) {
                tp <- miss[[v]]$type
                xi <- rval[[v]]
                z <- miss[[v]]$value
                if (tp %in% "none") 
                  next
                if (tp %in% c("one", "two", "three")) {
                  other <- miss[[v]]$value
                  xi[xi %in% other] <- NA
                  vl[[v]] <- vl[[v]][!(vl[[v]] %in% other)]
                }
                else if (tp == "low" || tp == "low+1") {
                  xi[xi <= z[1L]] <- NA
                  vl[[v]] <- vl[[v]][as.numeric(vl[[v]]) > z[1L]]
                  if (tp == "low+1") {
                    xi[xi == z[2L]] <- NA
                    vl[[v]] <- vl[[v]][as.numeric(vl[[v]]) != 
                      z[2L]]
                  }
                }
                else if (tp == "high" || tp == "high+1") {
                  xi[xi >= z[1L]] <- NA
                  vl[[v]] <- vl[[v]][as.numeric(vl[[v]]) < z[1L]]
                  if (tp == "high+1") {
                    xi[xi == z[2L]] <- NA
                    vl[[v]] <- vl[[v]][as.numeric(vl[[v]]) != 
                      z[2L]]
                  }
                }
                else if (tp == "range" || tp == "range+1") {
                  xi[xi >= z[1L] & xi <= z[2L]] <- NA
                  vl[[v]] <- vl[[v]][as.numeric(vl[[v]]) < z[1L] | 
                    as.numeric(vl[[v]]) > z[2L]]
                  if (tp == "range+1") {
                    xi[xi == z[3L]] <- NA
                    vl[[v]] <- vl[[v]][as.numeric(vl[[v]]) != 
                      z[3L]]
                  }
                }
                else warning(gettextf("missingness type %s is not handled", 
                  tp), domain = NA)
                rval[[v]] <- xi
            }
    }
    else use.missings <- FALSE
    if (reencode) 
        names(vl) <- iconv(names(vl), cp, "")
    has.vl <- which(!sapply(vl, is.null))
    for (v in has.vl) {
        nm <- names(vl)[[v]]
        nvalues <- length(na.omit(unique(rval[[nm]])))
        nlabels <- length(vl[[v]])
        if (reencode && nlabels) {
            nm2 <- names(vl[[v]])
            vl[[v]] <- iconv(vl[[v]], cp, "")
            names(vl[[v]]) <- iconv(nm2, cp, "")
        }
        if (use.value.labels && (!is.finite(max.value.labels) || 
            nvalues <= max.value.labels) && nlabels >= nvalues) {
            rval[[nm]] <- factor(trim(rval[[nm]], trim_values), 
                levels = rev(trim(vl[[v]], trim_values)), labels = rev(trim(names(vl[[v]]), 
                  trim.factor.names)))
        }
        else attr(rval[[nm]], "value.labels") <- vl[[v]]
    }
    if (reencode) 
        attr(rval, "label.table") <- vl
    if (to.data.frame) {
        varlab <- attr(rval, "variable.labels")
        rval <- as.data.frame(rval)
        attr(rval, "variable.labels") <- varlab
        if (codepage > 500) 
            attr(rval, "codepage") <- codepage
    }
    rval
}


read.S <- function (file) 
{
    endian <- .Platform$endian
    s <- file(file, open = "rb")
    on.exit(close(s))
    readheader <- function(s) {
        head <- readBin(s, "int", 8L, 1L)
        all(head == c(0L, 83L, 32L, 100L, 97L, 116L, 97L, 1L))
    }
    ReadSObj <- function(code, len) {
        if (code == 1L) 
            result <- as.logical(readBin(s, "int", len, endian = endian))
        else if (code == 2L) 
            result <- readBin(s, "int", len, endian = endian)
        else if (code == 3L) 
            result <- readBin(s, "numeric", len, size = 4L, endian = endian)
        else if (code == 4L) 
            result <- readBin(s, "numeric", len, endian = endian)
        else if (code == 5L) {
            charsize <- readBin(s, "int", endian = endian)
            newpos <- charsize + seek(s, NA)
            result <- readBin(s, "character", len)
            seek(s, newpos)
        }
        else if (code == 6L) {
            result <- list()
            if (len) {
                names <- ReadSObj(5L, len)
                codes <- ReadSObj(2L, len)
                lens <- ReadSObj(2L, len)
                offsets <- ReadSObj(2, len)
                for (i in 1L:len) {
                  seek(s, offsets[i])
                  temp <- if (codes[i] > 0L) 
                    ReadSObj(codes[i], lens[i])
                  else as.name(names[i])
                  result[[nam.or.i(names[i], i)]] <- temp
                }
            }
        }
        else if (code == 7L) 
            result <- readBin(s, "complex", len, endian = endian)
        else if (code == 21L) {
            temp <- ReadSObj(6L, len)
            result <- temp[[".Data"]]
            attributes(result) <- temp[-match(c(".Data", ".Dim", 
                ".Dimnames", ".Label"), names(temp), nomatch = 0L)]
            dim(result) <- temp[[".Dim"]]
            names(result) <- names(temp[[".Data"]])
            if (!is.null(temp[[".Label"]])) 
                levels(result) <- temp[[".Label"]]
            if (!is.null(temp[[".Dimnames"]])) 
                dimnames(result) <- temp[[".Dimnames"]]
        }
        else if (code %in% 257L:321L) {
            code <- SModeNames[code - 256L]
            if (code %in% c("name", "missing")) 
                result <- ReadSObj(5L, len)
            else result <- ReadSObj(6L, len)
            if (code == "function") 
                try(result <- as.function(result, env = .GlobalEnv))
            else if (code %in% c("break", "if", "for", "return", 
                "S.call", "while", "<-", "<<-", "(", "{")) 
                result <- as.call(c(as.name(code), result))
            else if (code == "call(...)") 
                result <- result[[1L]]
            else if (code == "comment") 
                result <- NULL
            else if (code == "comment.expression") 
                result <- result[unlist(lapply(result, function(y) !is.null(y)))][[1L]]
            else if (code == "internal") 
                result <- as.call(list(as.name(".Internal"), 
                  result[[1L]]))
            else if (code == "missing") 
                result <- call("stop", "Argument is missing")
            else try(mode(result) <- code)
        }
        else {
            return(paste("Unrecognized S mode", code, "not supported"))
        }
        result
    }
    if (readheader(s)) {
        code <- readBin(s, "int", endian = endian)
        if (code < 0L | code > 65535L) {
            endian <- switch(endian, big = "little", little = "big")
            seek(s, seek(s, NA) - 4)
            code <- readBin(s, "int", endian = endian)
            if (code < 0L | code > 65535L) 
                stop("internal error - illegal S code value")
        }
        len <- readBin(s, "int", endian = endian)
        return(ReadSObj(code, len))
    }
    else stop("not an S object")
}


write.arff <- function (x, file, eol = "\n", relation = deparse(substitute(x))) 
{
    if (file == "") 
        file <- stdout()
    else if (is.character(file)) {
        file <- file(file, "wb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("Argument 'file' must be a character string or connection.")
    if (!is.data.frame(x) && !is.matrix(x)) 
        x <- data.frame(x)
    squote <- function(s) {
        ifelse(is.na(s), s, sprintf("'%s'", gsub("(['\\])", "\\\\\\1", 
            s)))
    }
    spquote <- function(s) {
        if (length(grep("^[[:alpha:]]", s)) == 0L) 
            s <- paste("X", s, sep = "")
        if (length(grep(" ", s))) 
            s <- paste("\"", s, "\"", sep = "")
        s
    }
    text <- paste("@relation", spquote(make.names(relation)))
    writeLines(text, file, sep = eol)
    for (name in colnames(x)) {
        text <- paste("@attribute", spquote(name))
        if (is.data.frame(x) && is.factor(x[[name]])) {
            lev <- squote(levels(x[[name]]))
            levels(x[[name]]) <- lev
            text <- paste(text, " {", paste(lev, collapse = ","), 
                "}", sep = "")
        }
        else if (is.character(x[, name])) {
            text <- paste(text, "string")
            x[, name] <- squote((x[, name]))
        }
        else if (inherits(x[, name], "Date")) {
            text <- paste(text, "date \"yyyy-MM-dd\"")
            x[, name] <- squote(format(x[, name]))
        }
        else if (inherits(x[, name], "POSIXt")) {
            text <- paste(text, "date \"yyyy-MM-dd HH:mm:ss\"")
            x[, name] <- squote(format(x[, name]))
        }
        else text <- paste(text, "numeric")
        writeLines(text, file, sep = eol)
    }
    writeLines("@data", file)
    write.table(x, file = file, na = "?", sep = ",", eol = eol, 
        quote = FALSE, row.names = FALSE, col.names = FALSE)
}


read.xport <- function (file) 
{
    data.info <- lookup.xport(file)
    ans <- .Call(xport_read, file, data.info)
    if (length(ans) == 1L) 
        as.data.frame(ans[[1L]])
    else lapply(ans, as.data.frame)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Read Data Stored by Minitab, S, SAS, SPSS, Stata, Systat, Weka,dBase, ..."

.skeleton_package_version = "0.8-66"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods,utils,stats"


## Internal

.skeleton_version = 5


## EOF