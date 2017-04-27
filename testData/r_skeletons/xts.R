##
## Exported symobls in package `xts`
##

## Exported package methods

`dimnames<-.xts` <- function (x, value) 
{
    oclass <- class(x)
    x <- unclass(x)
    if (is.null(value)) {
        dimnames(x) <- NULL
        class(x) <- oclass
    }
    else {
        if (!is.list(value) || length(value) != 2L) 
            stop("invalid 'dimnames' given for xts")
        value[[1L]] <- list(NULL)
        value[[2L]] <- as.character(value[[2L]])
        rownames(x) <- NULL
        colnames(x) <- value[[2L]]
        class(x) <- oclass
    }
    x
}


to.weekly <- function (x, drop.time = TRUE, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    x <- to.period(x, "weeks", name = name, ...)
    if (drop.time) 
        x <- .drop.time(x)
    return(x)
}


last <- function (x, ...) 
{
    UseMethod("last")
}


timeBasedSeq <- function (x, retclass = NULL, length.out = NULL) 
{
    if (!is.character(x)) 
        x <- deparse(match.call()$x)
    x <- gsub("::", "/", x, perl = TRUE)
    x <- gsub("[-:]", "", x, perl = TRUE)
    x <- gsub("[ ]", "", x, perl = TRUE)
    x <- unlist(strsplit(x, "/"))
    from <- x[1]
    to <- x[2]
    BY <- x[3]
    if (from == "") 
        from <- NA
    if (!is.na(from)) {
        year <- as.numeric(substr(from, 1, 4))
        month <- as.numeric(substr(from, 5, 6))
        day <- as.numeric(substr(from, 7, 8))
        hour <- as.numeric(substr(from, 9, 10))
        mins <- as.numeric(substr(from, 11, 12))
        secs <- as.numeric(substr(from, 13, 14))
        time.args.from <- as.list(unlist(sapply(c(year, month, 
            day, hour, mins, secs), function(x) if (!is.na(x)) 
            x)))
        from <- do.call("firstof", time.args.from)
    }
    else time.args.from <- list()
    if (!is.na(to)) {
        year <- as.numeric(substr(to, 1, 4))
        month <- as.numeric(substr(to, 5, 6))
        day <- as.numeric(substr(to, 7, 8))
        hour <- as.numeric(substr(to, 9, 10))
        mins <- as.numeric(substr(to, 11, 12))
        secs <- as.numeric(substr(to, 13, 14))
        time.args.to <- as.list(unlist(sapply(c(year, month, 
            day, hour, mins, secs), function(x) if (!is.na(x)) 
            x)))
        to <- do.call("lastof", time.args.to)
    }
    else time.args.to <- list()
    max.resolution <- max(length(time.args.from), length(time.args.to))
    if (max.resolution == 0) 
        max.resolution <- 1
    resolution <- c("year", "month", "day", "hour", "mins", "secs")[max.resolution]
    if (!is.na(BY)) 
        resolution <- names(match.arg(BY, list(year = "Y", month = "m", 
            day = "d", hour = "H", mins = "M", secs = "S")))
    convert.to <- "Date"
    if (max.resolution == 2 || resolution == "month") 
        convert.to <- "yearmon"
    if (max.resolution > 3 || resolution %in% c("H", "M", "S")) 
        convert.to <- "POSIXct"
    if (is.na(to) && missing(length.out)) 
        length.out <- 1L
    if (((!missing(retclass) && is.null(retclass)) || any(is.na(to), 
        is.na(from)))) {
        return(list(from = from, to = to, by = resolution, length.out = length.out))
    }
    if (is.null(length.out)) {
        SEQ <- seq(from, to, by = resolution)
    }
    else {
        SEQ <- seq(from, by = resolution, length.out = length.out)
    }
    if (!is.null(retclass)) 
        convert.to <- retclass
    if (convert.to == "POSIXct") {
        structure(SEQ, class = c("POSIXt", "POSIXct"))
    }
    else do.call(paste("as", convert.to, sep = "."), list(SEQ))
}


xtsAttributes <- function (x, user = NULL) 
{
    rm.attr <- c("dim", "dimnames", "index", "class", "names")
    x.attr <- attributes(x)
    if (is.null(user)) {
        rm.attr <- c(rm.attr, ".CLASS", ".CLASSnames", ".ROWNAMES", 
            ".indexCLASS", ".indexFORMAT", ".indexTZ", "tzone", 
            "tclass")
        xa <- x.attr[!names(x.attr) %in% rm.attr]
    }
    else if (user) {
        rm.attr <- c(rm.attr, ".CLASS", ".CLASSnames", ".ROWNAMES", 
            ".indexCLASS", ".indexFORMAT", ".indexTZ", "tzone", 
            "tclass", x.attr$.CLASSnames)
        xa <- x.attr[!names(x.attr) %in% rm.attr]
    }
    else {
        xa <- x.attr[names(x.attr) %in% x.attr$.CLASSnames]
    }
    if (length(xa) == 0) 
        return(NULL)
    xa
}


is.timeBased <- function (x) 
{
    if (!any(sapply(c("Date", "POSIXt", "chron", "dates", "times", 
        "timeDate", "yearmon", "yearqtr", "xtime"), function(xx) inherits(x, 
        xx)))) {
        FALSE
    }
    else TRUE
}


is.time.unique <- function (x) 
{
    UseMethod("is.time.unique")
}


`indexFormat<-` <- function (x, value) 
{
    UseMethod("indexFormat<-")
}


reclass <- function (x, match.to, error = FALSE, ...) 
{
    if (!missing(match.to) && is.xts(match.to)) {
        if (NROW(x) != length(.index(match.to))) 
            if (error) {
                stop("incompatible match.to attibutes")
            }
            else return(x)
        if (!is.xts(x)) 
            x <- .xts(coredata(x), .index(match.to), .indexCLASS = indexClass(match.to), 
                tzone = indexTZ(match.to))
        CLASS(x) <- CLASS(match.to)
        xtsAttributes(x) <- xtsAttributes(match.to)
    }
    oldCLASS <- CLASS(x)
    if (length(oldCLASS) > 0 && !inherits(oldClass, "xts")) {
        if (!is.null(dim(x))) {
            if (!is.null(attr(x, ".ROWNAMES"))) {
                rownames(x) <- attr(x, ".ROWNAMES")[1:NROW(x)]
            }
        }
        attr(x, ".ROWNAMES") <- NULL
        if (isTRUE(attr(x, ".RECLASS"))) {
            do.call(paste("re", oldCLASS, sep = "."), list(x))
        }
        else {
            x
        }
    }
    else {
        x
    }
}


tzone <- function (x, ...) 
{
    UseMethod("tzone")
}


xcoredata <- function (x, ...) 
{
    UseMethod("xcoredata")
}


convertIndex <- function (x, value) 
{
    indexClass(x) <- value
    x
}


`indexTZ<-` <- function (x, value) 
{
    UseMethod("indexTZ<-")
}


merge.xts <- function (..., all = TRUE, fill = NA, suffixes = NULL, join = "outer", 
    retside = TRUE, retclass = "xts", tzone = NULL, drop = NULL, 
    check.names = NULL) 
{
    if (is.logical(retclass) && !retclass) {
        setclass = FALSE
    }
    else setclass <- TRUE
    fill.fun <- NULL
    if (is.function(fill)) {
        fill.fun <- fill
        fill <- NA
    }
    mc <- match.call(expand.dots = FALSE)
    dots <- mc$...
    if (is.null(suffixes)) {
        syms <- names(dots)
        syms[nchar(syms) == 0] <- as.character(dots)[nchar(syms) == 
            0]
        if (is.null(syms)) 
            syms <- as.character(dots)
    }
    else if (length(suffixes) != length(dots)) {
        warning("length of suffixes and does not match number of merged objects")
        syms <- as.character(dots)
    }
    else {
        syms <- as.character(suffixes)
        sfx <- as.character(suffixes)
    }
    .times <- .External("number_of_cols", ..., PACKAGE = "xts")
    if (all(.times == 0)) 
        return(xts())
    symnames <- rep(syms, .times)
    if (length(dots) == 1) {
        if (!is.null(names(dots))) {
            x <- list(...)[[1]]
            if (is.null(colnames(x))) 
                colnames(x) <- symnames
            return(x)
        }
    }
    if (!missing(join)) {
        all <- switch(pmatch(join, c("outer", "left", "right", 
            "inner")), c(TRUE, TRUE), c(TRUE, FALSE), c(FALSE, 
            TRUE), c(FALSE, FALSE))
        if (length(dots) > 2) {
            all <- all[1]
            warning("'join' only applicable to two object merges")
        }
    }
    if (length(all) != 2) {
        if (length(all) > 2) 
            warning("'all' must be of length two")
        all <- rep(all[1], 2)
    }
    if (length(dots) > 2) 
        retside <- TRUE
    if (length(retside) != 2) 
        retside <- rep(retside[1], 2)
    x <- .External("mergeXts", all = all[1:2], fill = fill, setclass = setclass, 
        symnames = symnames, suffixes = suffixes, retside = retside, 
        env = new.env(), tzone = tzone, ..., PACKAGE = "xts")
    if (!is.logical(retclass) && retclass != "xts") {
        asFun <- paste("as", retclass, sep = ".")
        if (!exists(asFun)) {
            warning(paste("could not locate", asFun, "returning 'xts' object instead"))
            return(x)
        }
        xx <- try(do.call(asFun, list(x)))
        if (!inherits(xx, "try-error")) {
            return(xx)
        }
    }
    if (!is.null(fill.fun)) {
        fill.fun(x)
    }
    else return(x)
}


`CLASS<-` <- function (x, value) 
{
    UseMethod("CLASS<-")
}


timeBasedRange <- function (x, ...) 
{
    if (!is.character(x)) 
        x <- deparse(match.call()$x)
    tblist <- timeBasedSeq(x, NULL)
    c(as.numeric(tblist$from), as.numeric(tblist$to))
}


ndays <- function (x) 
{
    length(endpoints(x, on = "days")) - 1
}


indexTZ <- function (x, ...) 
{
    UseMethod("indexTZ")
}


align.time <- function (x, ...) 
{
    UseMethod("align.time")
}


make.index.unique <- function (x, eps = 1e-06, drop = FALSE, fromLast = FALSE, ...) 
{
    UseMethod("make.index.unique")
}


cbind.xts <- function (..., all = TRUE, fill = NA, suffixes = NULL) 
{
    merge.xts(..., all = all, fill = fill, suffixes = suffixes)
}


apply.quarterly <- function (x, FUN, ...) 
{
    ep <- endpoints(x, "quarters")
    period.apply(x, ep, FUN, ...)
}


to.minutes30 <- function (x, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    to.period(x, "minutes", k = 30, name = name, ...)
}


as.timeSeries.xts <- function (x, ...) 
{
    if (!require("timeSeries", quietly = TRUE)) 
        timeSeries <- function(...) message("package 'timeSeries' is required")
    timeSeries(data = coredata(x), charvec = as.character(index(x)), 
        ...)
}


period.prod <- function (x, INDEX) 
{
    if (NCOL(x) > 1) 
        stop("single column data only")
    if (min(INDEX) < 0 || max(INDEX) > NROW(x)) 
        stop("INDEX must be >= 0 and <= nrow(x)")
    ep <- INDEX
    if (ep[1] != 0) 
        ep <- c(0, ep)
    if (ep[length(ep)] != NROW(x)) 
        ep <- c(ep, NROW(x))
    xx <- as.double(as.matrix(x))
    q <- .Fortran("pprodz", ep = as.integer(ep), lep = as.integer(length(ep)), 
        ia = as.double(xx), lia = as.integer(length(xx)), ret = as.double(rep(0, 
            (length(ep) - 1))), PACKAGE = "xts")
    if (timeBased(index(x))) {
        tz <- xts(q$ret, index(x)[ep[-1]])
    }
    else {
        tz <- zoo(q$ret, index(x)[ep[-1]])
    }
    tz
}


apply.weekly <- function (x, FUN, ...) 
{
    ep <- endpoints(x, "weeks")
    period.apply(x, ep, FUN, ...)
}


indexFormat <- function (x) 
{
    attr(x, ".indexFORMAT")
}


nquarters <- function (x) 
{
    length(endpoints(x, on = "quarters")) - 1
}


c.xts <- function (...) 
{
    .External("rbindXts", dup = FALSE, ..., PACKAGE = "xts")
}


to.quarterly <- function (x, indexAt = "yearqtr", drop.time = TRUE, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    x <- to.period(x, "quarters", indexAt = indexAt, name = name, 
        ...)
    if (drop.time) 
        x <- .drop.time(x)
    return(x)
}


use.xts <- function (x, ..., error = TRUE) 
{
    if (is.xts(x)) {
        return(x)
    }
    xx <- try(as.xts(x, ..., .RECLASS = TRUE), silent = TRUE)
    if (inherits(xx, "try-error")) {
        if (is.character(error)) {
            stop(error)
        }
        else if (is.function(error)) {
            return(error(x, ...))
        }
        else if (error) {
            stop(gsub("\n", "", xx))
        }
        else {
            return(x)
        }
    }
    else {
        structure(xx, .RECLASS = TRUE)
    }
}


.indexweek <- function (x) 
{
    (.index(x) + (3 * 86400))%/%86400%/%7
}


period.sum <- function (x, INDEX) 
{
    if (NCOL(x) > 1) 
        stop("single column data only")
    if (min(INDEX) < 0 || max(INDEX) > NROW(x)) 
        stop("INDEX must be >= 0 and <= nrow(x)")
    ep <- INDEX
    if (ep[1] != 0) 
        ep <- c(0, ep)
    if (ep[length(ep)] != NROW(x)) 
        ep <- c(ep, NROW(x))
    xx <- as.double(as.matrix(x))
    q <- .Fortran("psumz", ep = as.integer(ep), lep = as.integer(length(ep)), 
        ia = as.double(xx), lia = as.integer(length(xx)), ret = as.double(rep(0, 
            (length(ep) - 1))), PACKAGE = "xts")
    if (timeBased(index(x))) {
        tz <- xts(q$ret, index(x)[ep[-1]])
    }
    else {
        tz <- zoo(q$ret, index(x)[ep[-1]])
    }
    tz
}


.indexsec <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$sec
}


split.xts <- function (x, f = "months", drop = FALSE, k = 1, ...) 
{
    if (is.character(f)) {
        ep <- endpoints(x, on = f, k = k)
        sp <- (ep + 1)[-length(ep)]
        ep <- ep[-1]
        lapply(1:length(ep), function(X) x[sp[X]:ep[X]])
    }
    else NextMethod("split")
}


.indexhour <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$hour
}


use.reclass <- function (x) 
{
    xx <- match.call()
    xxObj <- eval.parent(parse(text = all.vars(xx)[1]), 1)
    inObj <- try.xts(xxObj, error = FALSE)
    xx <- eval(match.call()[[-1]])
    reclass(xx, inObj)
}


.indexmin <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$min
}


nyears <- function (x) 
{
    length(endpoints(x, on = "years")) - 1
}


lag.xts <- function (x, k = 1, na.pad = TRUE, ...) 
{
    zooCompat <- getOption("xts.compat.zoo.lag")
    if (is.logical(zooCompat) && zooCompat) {
        k <- -k
        if (missing(na.pad)) 
            na.pad <- FALSE
    }
    if (length(k) > 1) {
        if (is.null(names(k))) 
            names(k) <- paste("lag", k, sep = "")
        return(do.call("merge.xts", lapply(k, lag.xts, x = x, 
            na.pad = na.pad, ...)))
    }
    .Call("lag_xts", x, as.integer(k), as.logical(na.pad), PACKAGE = "xts")
}


to_period <- function (x, period = "months", k = 1, indexAt = NULL, name = NULL, 
    OHLC = TRUE, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    xo <- x
    x <- try.xts(x)
    if (NROW(x) == 0 || NCOL(x) == 0) 
        stop(sQuote("x"), " contains no data")
    if (any(is.na(x))) {
        x <- na.omit(x)
        warning("missing values removed from data")
    }
    if (!OHLC) {
        xx <- x[endpoints(x, period, k), ]
    }
    else {
        if (!is.null(indexAt)) {
            index_at <- switch(indexAt, startof = TRUE, endof = FALSE, 
                FALSE)
        }
        else index_at <- FALSE
        cnames <- c("Open", "High", "Low", "Close")
        if (has.Vo(x)) 
            cnames <- c(cnames, "Volume")
        if (has.Ad(x) && is.OHLC(x)) 
            cnames <- c(cnames, "Adjusted")
        cnames <- paste(name, cnames, sep = ".")
        if (is.null(name)) 
            cnames <- NULL
        xx <- .Call("toPeriod", x, endpoints(x, period, k), has.Vo(x), 
            has.Vo(x, which = TRUE), has.Ad(x) && is.OHLC(x), 
            index_at, cnames, PACKAGE = "xts")
    }
    if (!is.null(indexAt)) {
        if (indexAt == "yearmon" || indexAt == "yearqtr") 
            indexClass(xx) <- indexAt
        if (indexAt == "firstof") {
            ix <- as.POSIXlt(c(.index(xx)), tz = indexTZ(xx))
            if (period %in% c("years", "months", "quarters", 
                "days")) 
                index(xx) <- firstof(ix$year + 1900, ix$mon + 
                  1)
            else index(xx) <- firstof(ix$year + 1900, ix$mon + 
                1, ix$mday, ix$hour, ix$min, ix$sec)
        }
        if (indexAt == "lastof") {
            ix <- as.POSIXlt(c(.index(xx)), tz = indexTZ(xx))
            if (period %in% c("years", "months", "quarters", 
                "days")) 
                index(xx) <- as.Date(lastof(ix$year + 1900, ix$mon + 
                  1))
            else index(xx) <- lastof(ix$year + 1900, ix$mon + 
                1, ix$mday, ix$hour, ix$min, ix$sec)
        }
    }
    reclass(xx, xo)
}


indexClass <- function (x) 
{
    class <- attr(attr(x, "index"), "tclass")
    if (is.null(class)) 
        attr(x, ".indexCLASS")
    else class
}


Reclass <- function (x) 
{
    xx <- match.call()
    xxObj <- eval.parent(parse(text = all.vars(xx)[1]), 1)
    inObj <- try.xts(xxObj, error = FALSE)
    xx <- eval(match.call()[[-1]])
    reclass(xx, inObj)
}


`tclass<-` <- function (x, value) 
{
    UseMethod("tclass<-")
}


firstof <- function (year = 1970, month = 1, day = 1, hour = 0, min = 0, 
    sec = 0, tz = "") 
{
    ISOdatetime(year, month, day, hour, min, sec, tz)
}


period.max <- function (x, INDEX) 
{
    if (NCOL(x) > 1) 
        stop("single column data only")
    if (min(INDEX) < 0 || max(INDEX) > NROW(x)) 
        stop("INDEX must be >= 0 and <= nrow(x)")
    ep <- INDEX
    if (ep[1] != 0) 
        ep <- c(0, ep)
    if (ep[length(ep)] != NROW(x)) 
        ep <- c(ep, NROW(x))
    xx <- as.double(as.matrix(x))
    q <- .Fortran("pmaxz", ep = as.integer(ep), lep = as.integer(length(ep)), 
        ia = as.double(xx), lia = as.integer(length(xx)), ret = as.double(rep(0, 
            (length(ep) - 1))), PACKAGE = "xts")
    if (timeBased(index(x))) {
        tz <- xts(q$ret, index(x)[ep[-1]])
    }
    else {
        tz <- zoo(q$ret, index(x)[ep[-1]])
    }
    tz
}


try.xts <- function (x, ..., error = TRUE) 
{
    if (is.xts(x)) {
        return(x)
    }
    xx <- try(as.xts(x, ..., .RECLASS = TRUE), silent = TRUE)
    if (inherits(xx, "try-error")) {
        if (is.character(error)) {
            stop(error)
        }
        else if (is.function(error)) {
            return(error(x, ...))
        }
        else if (error) {
            stop(gsub("\n", "", xx))
        }
        else {
            return(x)
        }
    }
    else {
        structure(xx, .RECLASS = TRUE)
    }
}


.indexwday <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$wday
}


apply.monthly <- function (x, FUN, ...) 
{
    ep <- endpoints(x, "months")
    period.apply(x, ep, FUN, ...)
}


as.environment.xts <- function (x) 
{
    e <- new.env()
    lapply(1:NCOL(x), function(.) assign(colnames(x)[.], x[, 
        .], envir = e))
    e
}


periodicity <- function (x, ...) 
{
    if (timeBased(x) || !is.xts(x)) 
        x <- try.xts(x, error = "'x' needs to be timeBased or xtsible")
    p <- median(diff(.index(x)))
    if (is.na(p)) 
        stop("can not calculate periodicity of 1 observation")
    units <- "days"
    scale <- "yearly"
    label <- "year"
    if (p < 60) {
        units <- "secs"
        scale <- "seconds"
        label <- "second"
    }
    else if (p < 3600) {
        units <- "mins"
        scale <- "minute"
        label <- "minute"
        p <- p/60L
    }
    else if (p < 86400) {
        units <- "hours"
        scale <- "hourly"
        label <- "hour"
    }
    else if (p == 86400) {
        scale <- "daily"
        label <- "day"
    }
    else if (p <= 604800) {
        scale <- "weekly"
        label <- "week"
    }
    else if (p <= 2678400) {
        scale <- "monthly"
        label <- "month"
    }
    else if (p <= 7948800) {
        scale <- "quarterly"
        label <- "quarter"
    }
    structure(list(difftime = structure(p, units = units, class = "difftime"), 
        frequency = p, start = start(x), end = end(x), units = units, 
        scale = scale, label = label), class = "periodicity")
}


nweeks <- function (x) 
{
    length(endpoints(x, on = "weeks")) - 1
}


make.time.unique <- function (x, eps = 1e-06, drop = FALSE, fromLast = FALSE, ...) 
{
    UseMethod("make.index.unique")
}


`xcoredata<-` <- function (x, value) 
{
    UseMethod("xcoredata<-")
}


.indexmday <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$mday
}


period.min <- function (x, INDEX) 
{
    if (NCOL(x) > 1) 
        stop("single column data only")
    if (min(INDEX) < 0 || max(INDEX) > NROW(x)) 
        stop("INDEX must be >= 0 and <= nrow(x)")
    ep <- INDEX
    if (ep[1] != 0) 
        ep <- c(0, ep)
    if (ep[length(ep)] != NROW(x)) 
        ep <- c(ep, NROW(x))
    xx <- as.double(as.matrix(x))
    q <- .Fortran("pminz", ep = as.integer(ep), lep = as.integer(length(ep)), 
        ia = as.double(xx), lia = as.integer(length(xx)), ret = as.double(rep(0, 
            (length(ep) - 1))), PACKAGE = "xts")
    if (timeBased(index(x))) {
        tz <- xts(q$ret, index(x)[ep[-1]])
    }
    else {
        tz <- zoo(q$ret, index(x)[ep[-1]])
    }
    tz
}


timeBased <- function (x) 
{
    if (!any(sapply(c("Date", "POSIXt", "chron", "dates", "times", 
        "timeDate", "yearmon", "yearqtr", "xtime"), function(xx) inherits(x, 
        xx)))) {
        FALSE
    }
    else TRUE
}


xts <- function (x = NULL, order.by = index(x), frequency = NULL, unique = TRUE, 
    tzone = Sys.getenv("TZ"), ...) 
{
    if (is.null(x) && missing(order.by)) 
        return(structure(.xts(, 0), index = integer()))
    if (!timeBased(order.by)) 
        stop("order.by requires an appropriate time-based object")
    if (inherits(order.by, "dates")) 
        tzone <- ""
    if (inherits(order.by, "Date")) {
        if (!missing(tzone)) 
            warning(paste(sQuote("tzone"), "setting ignored for Date indexes"))
        tzone <- "UTC"
    }
    if (NROW(x) > 0 && NROW(x) != length(order.by)) 
        stop("NROW(x) must match length(order.by)")
    orderBy <- class(order.by)
    if (inherits(order.by, "Date")) {
        order.by <- .POSIXct(unclass(order.by) * 86400, tz = tzone)
    }
    if (!is.null(x) && !isOrdered(order.by, strictly = !unique)) {
        indx <- order(order.by)
        if (NCOL(x) > 1 || is.matrix(x) || is.data.frame(x)) {
            x <- x[indx, , drop = FALSE]
        }
        else x <- x[indx]
        order.by <- order.by[indx]
    }
    if (!is.null(x) || length(x) != 0) {
        x <- as.matrix(x)
    }
    else x <- numeric(0)
    if (orderBy == "timeDate" && missing(tzone)) {
        tzone <- order.by@FinCenter
    }
    else if (!is.null(attr(order.by, "tzone")) && missing(tzone)) 
        tzone <- attr(order.by, "tzone")
    if (inherits(order.by, "dates")) 
        index <- as.numeric(as.POSIXct(strptime(as.character(order.by), 
            "(%m/%d/%y %H:%M:%S)")))
    else index <- as.numeric(as.POSIXct(order.by))
    x <- structure(.Data = x, index = structure(index, tzone = tzone, 
        tclass = orderBy), class = c("xts", "zoo"), .indexCLASS = orderBy, 
        tclass = orderBy, .indexTZ = tzone, tzone = tzone, ...)
    if (!is.null(attributes(x)$dimnames[[1]])) 
        dimnames(x) <- dimnames(x)
    x
}


endpoints <- function (x, on = "months", k = 1) 
{
    if (timeBased(x)) {
        NR <- length(x)
        x <- xts(, order.by = x)
    }
    else NR <- NROW(x)
    addlast <- TRUE
    if (!is.xts(x)) 
        x <- try.xts(x, error = "must be either xts-coercible or timeBased")
    if (on %in% c("years", "quarters", "months", "weeks", "days")) 
        posixltindex <- as.POSIXlt(.POSIXct(.index(x)), tz = indexTZ(x))
    if (on == "years") {
        as.integer(c(0, which(diff(posixltindex$year%/%k + 1) != 
            0), NR))
    }
    else if (on == "quarters") {
        xi <- (posixltindex$mon%/%3) + 1
        as.integer(c(0, which(diff(xi) != 0), NR))
    }
    else if (on == "months") {
        ep <- .Call("endpoints", posixltindex$mon, 1L, 1L, addlast, 
            PACKAGE = "xts")
        if (k > 1) 
            ep[seq(1, length(ep), k)]
        else ep
    }
    else if (on == "weeks") {
        .Call("endpoints", .index(x) + 3L * 86400L, 604800L, 
            k, addlast, PACKAGE = "xts")
    }
    else if (on == "days") {
        .Call("endpoints", posixltindex$yday, 1L, k, addlast, 
            PACKAGE = "xts")
    }
    else if (on == "hours") {
        .Call("endpoints", .index(x), 3600L, k, addlast, PACKAGE = "xts")
    }
    else if (on == "minutes" || on == "mins") {
        .Call("endpoints", .index(x), 60L, k, addlast, PACKAGE = "xts")
    }
    else if (on == "seconds" || on == "secs") {
        .Call("endpoints", .index(x), 1L, k, addlast, PACKAGE = "xts")
    }
    else if (on == "milliseconds" || on == "ms") {
        .Call("endpoints", .index(x)%/%0.001, 1L, k, addlast, 
            PACKAGE = "xts")
    }
    else if (on == "microseconds" || on == "us") {
        .Call("endpoints", .index(x)%/%1e-06, 1L, k, addlast, 
            PACKAGE = "xts")
    }
    else {
        stop("unsupported \"on\" argument")
    }
}


.indexmon <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$mon
}


apply.yearly <- function (x, FUN, ...) 
{
    ep <- endpoints(x, "years")
    period.apply(x, ep, FUN, ...)
}


.indexyday <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$yday
}


.index <- function (x, ...) 
{
    if (is.list(attr(x, "index"))) {
        attr(x, "index")[[1]]
    }
    else attr(x, "index")
}


nminutes <- function (x) 
{
    length(endpoints(x, on = "minutes")) - 1
}


`xtsAttributes<-` <- function (x, value) 
{
    UseMethod("xtsAttributes<-")
}


to.period <- function (x, period = "months", k = 1, indexAt = NULL, name = NULL, 
    OHLC = TRUE, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    xo <- x
    x <- try.xts(x)
    if (NROW(x) == 0 || NCOL(x) == 0) 
        stop(sQuote("x"), " contains no data")
    if (any(is.na(x))) {
        x <- na.omit(x)
        warning("missing values removed from data")
    }
    if (!OHLC) {
        xx <- x[endpoints(x, period, k), ]
    }
    else {
        if (!is.null(indexAt)) {
            index_at <- switch(indexAt, startof = TRUE, endof = FALSE, 
                FALSE)
        }
        else index_at <- FALSE
        cnames <- c("Open", "High", "Low", "Close")
        if (has.Vo(x)) 
            cnames <- c(cnames, "Volume")
        if (has.Ad(x) && is.OHLC(x)) 
            cnames <- c(cnames, "Adjusted")
        cnames <- paste(name, cnames, sep = ".")
        if (is.null(name)) 
            cnames <- NULL
        xx <- .Call("toPeriod", x, endpoints(x, period, k), has.Vo(x), 
            has.Vo(x, which = TRUE), has.Ad(x) && is.OHLC(x), 
            index_at, cnames, PACKAGE = "xts")
    }
    if (!is.null(indexAt)) {
        if (indexAt == "yearmon" || indexAt == "yearqtr") 
            indexClass(xx) <- indexAt
        if (indexAt == "firstof") {
            ix <- as.POSIXlt(c(.index(xx)), tz = indexTZ(xx))
            if (period %in% c("years", "months", "quarters", 
                "days")) 
                index(xx) <- firstof(ix$year + 1900, ix$mon + 
                  1)
            else index(xx) <- firstof(ix$year + 1900, ix$mon + 
                1, ix$mday, ix$hour, ix$min, ix$sec)
        }
        if (indexAt == "lastof") {
            ix <- as.POSIXlt(c(.index(xx)), tz = indexTZ(xx))
            if (period %in% c("years", "months", "quarters", 
                "days")) 
                index(xx) <- as.Date(lastof(ix$year + 1900, ix$mon + 
                  1))
            else index(xx) <- lastof(ix$year + 1900, ix$mon + 
                1, ix$mday, ix$hour, ix$min, ix$sec)
        }
    }
    reclass(xx, xo)
}


.indexyear <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$year
}


.xts <- function (x = NULL, index, tclass = c("POSIXt", "POSIXct"), tzone = Sys.getenv("TZ"), 
    check = TRUE, unique = FALSE, .indexCLASS = tclass, ...) 
{
    if (check) {
        if (!isOrdered(index, increasing = TRUE, strictly = unique)) 
            stop("index is not in ", ifelse(unique, "strictly", 
                ""), " increasing order")
    }
    if (!is.numeric(index) && timeBased(index)) 
        index <- as.numeric(as.POSIXct(index))
    if (!is.null(x) && NROW(x) != length(index)) 
        stop("index length must match number of observations")
    if (!is.null(x)) {
        if (!is.matrix(x)) 
            x <- as.matrix(x)
    }
    else if (length(x) == 0 && !is.null(x)) {
        x <- vector(storage.mode(x))
    }
    else x <- numeric(0)
    structure(.Data = x, index = structure(index, tzone = tzone, 
        tclass = .indexCLASS), .indexCLASS = .indexCLASS, .indexTZ = tzone, 
        tclass = .indexCLASS, tzone = tzone, class = c("xts", 
            "zoo"), ...)
}


period.apply <- function (x, INDEX, FUN, ...) 
{
    x <- try.xts(x, error = FALSE)
    FUN <- match.fun(FUN)
    xx <- sapply(1:(length(INDEX) - 1), function(y) {
        FUN(x[(INDEX[y] + 1):INDEX[y + 1]], ...)
    })
    if (is.vector(xx)) 
        xx <- t(xx)
    xx <- t(xx)
    if (is.null(colnames(xx)) && NCOL(x) == NCOL(xx)) 
        colnames(xx) <- colnames(x)
    reclass(xx, x[INDEX])
}


nmonths <- function (x) 
{
    length(endpoints(x, on = "months")) - 1
}


diff.xts <- function (x, lag = 1, differences = 1, arithmetic = TRUE, log = FALSE, 
    na.pad = TRUE, ...) 
{
    if (is.logical(x)) 
        x <- .xts(matrix(as.integer(x), ncol = NCOL(x)), .index(x))
    if (lag < 1 || differences < 1) 
        stop("'diff.xts' defined only for positive lag and differences arguments")
    zooCompat <- getOption("xts.compat.zoo.lag")
    if (is.logical(zooCompat) && zooCompat) {
        lag <- -lag
        if (missing(na.pad)) 
            na.pad <- FALSE
    }
    if (differences > 1) {
        if (arithmetic && !log) {
            x <- x - lag.xts(x, k = lag, na.pad = na.pad)
        }
        else {
            if (log) {
                x <- log(x/lag.xts(x, k = lag, na.pad = na.pad))
            }
            else x <- x/lag.xts(x, k = lag, na.pad = na.pad)
        }
        diff(x, lag, differences = differences - 1, arithmetic = arithmetic, 
            log = log, na.pad = na.pad, ...)
    }
    else {
        if (arithmetic && !log) {
            x - lag.xts(x, k = lag, na.pad = na.pad)
        }
        else {
            if (log) {
                log(x/lag.xts(x, k = lag, na.pad = na.pad))
            }
            else x/lag.xts(x, k = lag, na.pad = na.pad)
        }
    }
}


to.minutes <- function (x, k, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    if (missing(k)) 
        k <- 1
    to.period(x, "minutes", k = k, name = name, ...)
}


.indexDate <- function (x) 
{
    .index(x)%/%86400L
}


to.daily <- function (x, drop.time = TRUE, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    x <- to.period(x, "days", name = name, ...)
    if (drop.time) 
        x <- .drop.time(x)
    return(x)
}


to.yearly <- function (x, drop.time = TRUE, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    x <- to.period(x, "years", name = name, ...)
    if (drop.time) 
        x <- .drop.time(x)
    return(x)
}


adj.time <- function (x, ...) 
{
    tr <- match.call(expand.dots = FALSE)$...
    if (length(tr) < 1) 
        return(x)
    oClass <- class(x)
    x <- as.POSIXlt(x)
    ntime <- as.environment(unclass(x))
    lapply(tr, function(T) {
        assign(all.vars(T), with(x, eval(T)), envir = ntime)
    })
    x <- structure(list(sec = ntime$sec, min = ntime$min, hour = ntime$hour, 
        mday = ntime$mday, mon = ntime$mon, year = ntime$year, 
        wday = ntime$wday, yday = ntime$yday, isdst = ntime$isdst), 
        tzone = attr(x, "tzone"), class = c("POSIXt", "POSIXlt"))
    do.call(paste("as", oClass[1], sep = "."), list(x))
}


dimnames.xts <- function (x) 
{
    .Call("dimnames_zoo", x)
}


as.xts <- function (x, ...) 
{
    UseMethod("as.xts")
}


.subset.xts <- function (x, i, j, drop = FALSE, which.i = FALSE, ...) 
{
    USE_EXTRACT <- FALSE
    if (is.null(dim(x))) {
        nr <- length(x)
        if (nr == 0) 
            return(xts(rep(NA, length(index(x))), index(x))[i])
        nc <- 1L
    }
    else {
        nr <- nrow(x)
        nc <- ncol(x)
    }
    if (!missing(i)) {
        if (is.numeric(i)) {
            if (.Call("any_negative", i, PACKAGE = "xts")) {
                if (!all(i <= 0)) 
                  stop("only zeros may be mixed with negative subscripts")
                i <- (1:nr)[i]
            }
            if (length(i) > 0 && max(i) > nr) 
                stop("subscript out of bounds")
        }
        else if (inherits(i, "AsIs") && is.character(i)) {
            i <- MATCH(i, format(index(x)))
        }
        else if (timeBased(i)) {
            if (inherits(i, "POSIXct")) {
                i <- which(!is.na(match(.index(x), i)))
            }
            else if (inherits(i, "Date")) {
                i <- which(!is.na(match(.index(x), as.POSIXct(as.character(i), 
                  tz = indexTZ(x)))))
            }
            else {
                i <- which(!is.na(match(.index(x), as.POSIXct(i, 
                  tz = indexTZ(x)))))
            }
            i[is.na(i)] <- 0
        }
        else if (is.logical(i)) {
            i <- which(i)
        }
        else if (is.character(i)) {
            if (length(i) == 1 && !identical(integer(), grep("^T.*?/T", 
                i[1]))) {
                i <- gsub("T|:", "", i)
                i <- strsplit(i, "/")[[1]]
                i <- .makeISO8601TT(x, i[1], i[2])
            }
            i.tmp <- NULL
            tz <- as.character(indexTZ(x))
            i_len <- length(i)
            for (ii in i) {
                adjusted.times <- .parseISO8601(ii, .index(x)[1], 
                  .index(x)[nr], tz = tz)
                if (length(adjusted.times) > 1) {
                  firstlast <- c(seq.int(binsearch(adjusted.times$first.time, 
                    .index(x), TRUE), binsearch(adjusted.times$last.time, 
                    .index(x), FALSE)))
                  if (isOrdered(firstlast, strictly = FALSE)) 
                    i.tmp <- c(i.tmp, firstlast)
                }
            }
            i <- i.tmp
            if (i_len == 1L) 
                USE_EXTRACT <- TRUE
        }
        if (!isOrdered(i, strictly = FALSE)) {
            i <- sort(i)
        }
        zero.index <- binsearch(0, i, NULL)
        if (!is.na(zero.index)) 
            i <- i[-zero.index]
        if (length(i) <= 0 && USE_EXTRACT) 
            USE_EXTRACT <- FALSE
        if (which.i) 
            return(i)
    }
    if (missing(j)) {
        if (missing(i)) 
            i <- seq_len(nr)
        if (length(x) == 0) {
            x.tmp <- .xts(rep(NA, length(i)), .index(x)[i])
            return((colnames(x.tmp) <- colnames(x)))
        }
        else {
            if (USE_EXTRACT) {
                return(.Call("extract_col", x, as.integer(1:nc), 
                  drop, as.integer(i[1]), as.integer(i[length(i)]), 
                  PACKAGE = "xts"))
            }
            else {
                return(.Call("_do_subset_xts", x, as.integer(i), 
                  as.integer(1:nc), drop, PACKAGE = "xts"))
            }
        }
    }
    else if (is.numeric(j)) {
        if (min(j, na.rm = TRUE) < 0) {
            if (max(j, na.rm = TRUE) > 0) 
                stop("only zeros may be mixed with negative subscripts")
            j <- (1:nc)[j]
        }
        if (max(j, na.rm = TRUE) > nc) 
            stop("subscript out of bounds")
    }
    else if (is.logical(j)) {
        if (length(j) == 1) {
            j <- (1:nc)[rep(j, nc)]
        }
        else if (length(j) > nc) {
            stop("(subscript) logical subscript too long")
        }
        else j <- (1:nc)[j]
    }
    else if (is.character(j)) {
        j <- match(j, colnames(x), nomatch = 0L)
    }
    j0 <- which(!as.logical(j))
    if (length(j0)) 
        j <- j[-j0]
    if (length(j) == 0 || (length(j) == 1 && j == 0)) {
        if (missing(i)) 
            i <- seq_len(nr)
        return(.xts(coredata(x)[i, j, drop = FALSE], index = .index(x)[i], 
            .indexCLASS = indexClass(x), .indexTZ = indexTZ(x)))
    }
    if (missing(i)) 
        return(.Call("extract_col", x, as.integer(j), drop, 1, 
            nr, PACKAGE = "xts"))
    if (USE_EXTRACT) {
        return(.Call("extract_col", x, as.integer(j), drop, as.integer(i[1]), 
            as.integer(i[length(i)]), PACKAGE = "xts"))
    }
    else return(.Call("_do_subset_xts", x, as.integer(i), as.integer(j), 
        drop, PACKAGE = "xts"))
}


to.minutes3 <- function (x, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    to.period(x, "minutes", k = 3, name = name, ...)
}


plot.xts <- function (x, y = NULL, type = "l", auto.grid = TRUE, major.ticks = "auto", 
    minor.ticks = TRUE, major.format = TRUE, bar.col = "grey", 
    candle.col = "white", ann = TRUE, axes = TRUE, ...) 
{
    series.title <- deparse(substitute(x))
    ep <- axTicksByTime(x, major.ticks, format.labels = major.format)
    otype <- type
    if (is.OHLC(x) && type %in% c("candles", "bars")) {
        x <- x[, has.OHLC(x, TRUE)]
        xycoords <- list(x = .index(x), y = seq(min(x), max(x), 
            length.out = NROW(x)))
        type <- "n"
    }
    else {
        if (NCOL(x) > 1) 
            warning("only the univariate series will be plotted")
        if (is.null(y)) 
            xycoords <- xy.coords(.index(x), x[, 1])
    }
    plot(xycoords$x, xycoords$y, type = type, axes = FALSE, ann = FALSE, 
        ...)
    if (auto.grid) {
        abline(v = xycoords$x[ep], col = "grey", lty = 4)
        grid(NA, NULL)
    }
    if (is.OHLC(x) && otype == "candles") 
        plot.ohlc.candles(x, bar.col = bar.col, candle.col = candle.col, 
            ...)
    dots <- list(...)
    if (axes) {
        if (minor.ticks) 
            axis(1, at = xycoords$x, labels = FALSE, col = "#BBBBBB", 
                ...)
        axis(1, at = xycoords$x[ep], labels = names(ep), las = 1, 
            lwd = 1, mgp = c(3, 2, 0), ...)
        axis(2, ...)
    }
    box()
    if (!"main" %in% names(dots)) 
        title(main = series.title)
    do.call("title", list(...))
    assign(".plot.xts", recordPlot(), .xtsEnv)
}


CLASS <- function (x) 
{
    cl <- attr(x, ".CLASS")
    if (!is.null(cl)) 
        return(structure(cl, class = "CLASS"))
    return(NULL)
}


`indexClass<-` <- function (x, value) 
{
    UseMethod("indexClass<-")
}


to.minutes5 <- function (x, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    to.period(x, "minutes", k = 5, name = name, ...)
}


rbind.xts <- function (..., deparse.level = 1) 
{
    .External("rbindXts", dup = FALSE, ..., PACKAGE = "xts")
}


lastof <- function (year = 1970, month = 12, day = 31, hour = 23, min = 59, 
    sec = 59, subsec = 0.99999, tz = "") 
{
    if (!missing(sec) && sec%%1 != 0) 
        subsec <- 0
    sec <- ifelse(year < 1970, sec, sec + subsec)
    mon.lengths <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 
        30, 31)
    if (missing(day)) {
        day <- ifelse(month %in% 2, ifelse(((year%%4 %in% 0 & 
            !year%%100 %in% 0) | (year%%400 %in% 0)), 29, 28), 
            mon.lengths[month])
    }
    if (length(c(year, month, day, hour, min, sec)) == 6 && c(year, 
        month, day, hour, min, sec) == c(1969, 12, 31, 23, 59, 
        59) && Sys.getenv("TZ") %in% c("", "GMT", "UTC")) 
        sec <- sec - 1
    ISOdatetime(year, month, day, hour, min, sec, tz)
}


`.index<-` <- function (x, value) 
{
    if (timeBased(value)) {
        if (inherits(value, "Date")) {
            attr(x, "index") <- as.numeric(value)
        }
        else {
            attr(x, "index") <- as.numeric(as.POSIXct(value))
        }
    }
    else if (is.numeric(value)) {
        attr(x, "index") <- value
    }
    else stop(".index is used for low level operations - data must be numeric or timeBased")
    return(x)
}


.indexisdst <- function (x) 
{
    as.POSIXlt(.POSIXct(.index(x)))$isdst
}


to.minutes10 <- function (x, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    to.period(x, "minutes", k = 10, name = name, ...)
}


isOrdered <- function (x, increasing = TRUE, strictly = TRUE) 
{
    if (is.character(x)) 
        stop("character ordering unsupported")
    if (!is.numeric(x)) 
        x = as.numeric(x)
    .Call("do_is_ordered", x = x, increasing = as.logical(increasing), 
        strictly = as.logical(strictly), PACKAGE = "xts")
}


nhours <- function (x) 
{
    length(endpoints(x, on = "hours")) - 1
}


is.xts <- function (x) 
{
    inherits(x, "xts") && is.numeric(.index(x)) && !is.null(indexClass(x))
}


.indexday <- function (x) 
{
    .index(x)%/%86400L
}


xtsible <- function (x) 
{
    if (inherits(try(as.xts(x), silent = TRUE), "try-error")) {
        FALSE
    }
    else TRUE
}


.subset_xts <- function (x, i, j, ...) 
{
    if (missing(i)) {
        i <- 1:NROW(x)
    }
    if (missing(j)) {
        j <- 1:NCOL(x)
    }
    .Call("_do_subset_xts", x, i, j, FALSE, PACKAGE = "xts")
}


`tzone<-` <- function (x, value) 
{
    UseMethod("tzone<-")
}


nseconds <- function (x) 
{
    length(endpoints(x, on = "seconds")) - 1
}


first <- function (x, ...) 
{
    UseMethod("first")
}


to.minutes15 <- function (x, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    to.period(x, "minutes", k = 15, name = name, ...)
}


to.monthly <- function (x, indexAt = "yearmon", drop.time = TRUE, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    x <- to.period(x, "months", indexAt = indexAt, name = name, 
        ...)
    if (drop.time) 
        x <- .drop.time(x)
    return(x)
}


shift.time <- function (x, n = 60, ...) 
{
    UseMethod("shift.time")
}


.parseISO8601 <- function (x, start, end, tz = "") 
{
    as_numeric <- function(.x) {
        if (gsub(" ", "", .x) == "") 
            NULL
        else as.numeric(.x)
    }
    x <- gsub("NOW", format(Sys.time(), "%Y%m%dT%H%M%S"), x)
    x <- gsub("TODAY", format(Sys.Date(), "%Y%m%d"), x)
    if (identical(grep("/|(--)|(::)", x), integer(0))) {
        x <- paste(x, x, sep = "/")
    }
    intervals <- unlist(strsplit(x, "/|(--)|(::)"))
    DURATION <- ""
    if (length(intervals) == 2L) {
        if (substr(intervals[1], 0, 1) == "P") {
            DURATION <- intervals[1]
            DURATION_LHS <- TRUE
            intervals[1] <- ""
        }
        if (substr(intervals[2], 0, 1) == "P") {
            DURATION <- intervals[2]
            DURATION_LHS <- FALSE
            intervals <- intervals[1]
        }
    }
    parse.side <- function(x, startof) {
        if (is.na(x) || !nzchar(x)) 
            return(c(NULL))
        basic <- gsub(":|-", "", x, perl = TRUE)
        date.time <- unlist(strsplit(basic, " |T"))
        date <- date.time[1]
        if (!missing(startof) && nchar(basic) == 2L) {
            startof <- gsub(":|-", "", startof, perl = TRUE)
            if (nchar(startof) - nchar(date) >= 4) {
                sstartof <- substr(startof, 0, nchar(startof) - 
                  nchar(date))
                date <- paste(sstartof, date, sep = "")
            }
        }
        date <- sprintf("%-8s", date)
        YYYY <- substr(date, 0, 4)
        MM <- substr(date, 5, 6)
        DD <- substr(date, 7, 8)
        time <- date.time[2]
        if (!is.na(time)) {
            time <- sprintf("%-6s", time)
            H <- substr(time, 0, 2)
            M <- substr(time, 3, 4)
            S <- substr(time, 5, 10000L)
        }
        else H <- M <- S <- ""
        c(as.list(c(year = as_numeric(YYYY), mon = as_numeric(MM), 
            day = as_numeric(DD), hour = as_numeric(H), min = as_numeric(M), 
            sec = as_numeric(S))), tz = tz)
    }
    s <- e <- NA
    if (nzchar(intervals[1])) 
        s <- as.POSIXlt(do.call(firstof, parse.side(intervals[1])))
    if (length(intervals) == 2L) {
        e <- as.POSIXlt(do.call(lastof, parse.side(intervals[2], 
            intervals[1])))
        if (is.na(e)) 
            e <- as.POSIXlt(do.call(lastof, parse.side(intervals[2])))
    }
    if (!missing(start)) {
        start <- as.numeric(start)
        s <- as.POSIXlt(.POSIXct(max(start, as.numeric(s), na.rm = TRUE), 
            tz = tz))
    }
    if (!missing(end)) {
        end <- as.numeric(end)
        e <- as.POSIXlt(.POSIXct(min(end, as.numeric(e), na.rm = TRUE), 
            tz = tz))
    }
    if (nzchar(DURATION)) {
        parse_duration <- function(P) {
            P <- gsub("P", "", P)
            P <- gsub("T(.*)M", "\\1m", P)
            n <- unlist(strsplit(P, "[[:alpha:]]"))
            d <- unlist(strsplit(gsub("[[:digit:]]", "", P), 
                ""))
            dur.vec <- list(as.numeric(n), unname(c(Y = 6, M = 5, 
                D = 4, H = 3, m = 2, S = 1)[d]))
            init.vec <- rep(0, 9)
            init.vec[dur.vec[[2]]] <- dur.vec[[1]]
            init.vec
        }
        if (DURATION_LHS) {
            s <- as.POSIXct(structure(as.list(mapply(`-`, e, 
                parse_duration(DURATION))), class = c("POSIXt", 
                "POSIXlt"), tzone = attr(e, "tzone")))
        }
        else {
            e <- as.POSIXct(structure(as.list(mapply(`+`, s, 
                parse_duration(DURATION))), class = c("POSIXt", 
                "POSIXlt"), tzone = attr(e, "tzone")))
        }
    }
    list(first.time = as.POSIXct(s), last.time = as.POSIXct(e))
}


is.index.unique <- function (x) 
{
    UseMethod("is.time.unique")
}


to.hourly <- function (x, name, ...) 
{
    if (missing(name)) 
        name <- deparse(substitute(x))
    to.period(x, "hours", name = name, ...)
}


axTicksByTime <- function (x, ticks.on = "auto", k = 1, labels = TRUE, format.labels = TRUE, 
    ends = TRUE, gt = 2, lt = 30) 
{
    if (timeBased(x)) 
        x <- xts(rep(1, length(x)), x)
    tick.opts <- c("years", "months", "weeks", "days", "hours", 
        "minutes", "seconds")
    tick.k.opts <- c(10, 5, 2, 1, 6, 1, 1, 1, 4, 2, 1, 30, 15, 
        1, 1)
    if (ticks.on %in% tick.opts) {
        cl <- ticks.on[1]
        ck <- k
    }
    else {
        tick.opts <- paste(rep(tick.opts, c(4, 2, 1, 1, 3, 3, 
            1)), tick.k.opts)
        is <- structure(rep(0, length(tick.opts)), .Names = tick.opts)
        for (i in 1:length(tick.opts)) {
            y <- strsplit(tick.opts[i], " ")[[1]]
            ep <- endpoints(x, y[1], as.numeric(y[2]))
            is[i] <- length(ep) - 1
            if (is[i] > lt) 
                break
        }
        nms <- rev(names(is)[which(is > gt & is < lt)])[1]
        cl <- strsplit(nms, " ")[[1]][1]
        ck <- as.numeric(strsplit(nms, " ")[[1]][2])
    }
    if (is.null(cl)) {
        ep <- NULL
    }
    else ep <- endpoints(x, cl, ck)
    if (ends) 
        ep <- ep + c(rep(1, length(ep) - 1), 0)
    if (labels) {
        if (is.logical(format.labels) || is.character(format.labels)) {
            unix <- ifelse(.Platform$OS.type == "unix", TRUE, 
                FALSE)
            time.scale <- periodicity(x)$scale
            fmt <- ifelse(unix, "%n%b%n%Y", "%b %Y")
            if (time.scale == "weekly" | time.scale == "daily") 
                fmt <- ifelse(unix, "%b %d%n%Y", "%b %d %Y")
            if (time.scale == "minute" | time.scale == "hourly") 
                fmt <- ifelse(unix, "%b %d%n%H:%M", "%b %d %H:%M")
            if (time.scale == "seconds") 
                fmt <- ifelse(unix, "%b %d%n%H:%M:%S", "%b %d %H:%M:%S")
            if (is.character(format.labels)) 
                fmt <- format.labels
            names(ep) <- format(index(x)[ep], fmt)
        }
        else names(ep) <- as.character(index(x)[ep])
    }
    ep
}


apply.daily <- function (x, FUN, ...) 
{
    ep <- endpoints(x, "days")
    period.apply(x, ep, FUN, ...)
}


as.fts.xts <- function (x) 
{
    if (!require("fts", quietly = TRUE)) 
        fts <- function(...) message("package 'fts' is required")
    fts(coredata(x), structure(.index(x), class = c("POSIXt", 
        "POSIXct")))
}


tclass <- function (x) 
{
    class <- attr(attr(x, "index"), "tclass")
    if (is.null(class)) 
        attr(x, ".indexCLASS")
    else class
}




## Package Data

sample_matrix <- xts::sample_matrix		## Sample Data Matrix For xts Example and Unit Testing



## Package Info

.skeleton_package_title = "eXtensible Time Series"

.skeleton_package_version = "0.9-7"

.skeleton_package_depends = "zoo"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF