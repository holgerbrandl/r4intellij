##
## Exported symobls in package `zoo`
##

## Exported package methods

panel.rect.tis <- function (x0, x1, ...) 
{
    x0 <- as.zoo(x0)
    x1 <- as.zoo(x1)
    panel.rect(time(x0), coredata(x0), time(x1), coredata(x1), 
        ...)
}


as.Date.ts <- function (x, offset = 0, ...) 
{
    time.x <- unclass(time(x)) + offset
    if (frequency(x) == 1) 
        as.Date(paste(time.x, 1, 1, sep = "-"))
    else if (frequency(x) == 4) 
        as.Date(paste((time.x + 0.001)%/%1, 3 * (cycle(x) - 1) + 
            1, 1, sep = "-"))
    else if (frequency(x) == 12) 
        as.Date(paste((time.x + 0.001)%/%1, cycle(x), 1, sep = "-"))
    else stop("unable to convert ts time to Date class")
}


rollmean.default <- function (x, k, fill = if (na.pad) NA, na.pad = FALSE, align = c("center", 
    "left", "right"), ...) 
{
    coredata(rollmean(zoo(x), k, fill = fill, align = align, 
        ...))
}


make.par.list <- function (nams, x, n, m, def, recycle = sum(unnamed) > 0) 
{
    if (!is.list(x)) 
        x <- if (m == 1) 
            list(x)
        else as.list(x)
    y <- vector(mode = "list", length = length(nams))
    names(y) <- nams
    in.x <- nams %in% names(x)
    unnamed <- if (is.null(names(x))) 
        rep(TRUE, length(x))
    else names(x) == ""
    if (!recycle) 
        y[] <- def
    y[in.x] <- x[nams[in.x]]
    if (recycle) {
        stopifnot(sum(unnamed) > 0)
        y[!in.x] <- rep(x[unnamed], length.out = sum(!in.x))
    }
    else {
        y[which(!in.x)[seq_len(sum(unnamed))]] <- x[unnamed]
    }
    lapply(y, function(y) if (length(y) == 1) 
        y
    else rep(y, length.out = n))
}


na.trim.default <- function (object, sides = c("both", "left", "right"), is.na = c("any", 
    "all"), ...) 
{
    is.na <- match.arg(is.na)
    nisna <- if (is.na == "any" || length(dim(object)) < 2) {
        complete.cases(object)
    }
    else rowSums(!is.na(object)) > 0
    idx <- switch(match.arg(sides), left = cumsum(nisna) > 0, 
        right = rev(cumsum(rev(nisna) > 0) > 0), both = (cumsum(nisna) > 
            0) & rev(cumsum(rev(nisna)) > 0))
    if (length(dim(object)) < 2) 
        object[idx]
    else object[idx, , drop = FALSE]
}


panel.rect.zoo <- function (x0, x1, ...) 
{
    x0 <- as.zoo(x0)
    x1 <- as.zoo(x1)
    panel.rect(time(x0), coredata(x0), time(x1), coredata(x1), 
        ...)
}


write.zoo <- function (x, file = "", index.name = "Index", row.names = FALSE, 
    col.names = NULL, ...) 
{
    if (is.null(col.names)) 
        col.names <- !is.null(colnames(x))
    dx <- as.data.frame(x)
    stopifnot(all(names(dx) != index.name))
    dx[[index.name]] <- index(x)
    dx <- dx[, c(ncol(dx), 1:(ncol(dx) - 1))]
    write.table(dx, file = file, row.names = row.names, col.names = col.names, 
        ...)
}


cbind.zoo <- function (..., all = TRUE, fill = NA, suffixes = NULL, drop = FALSE) 
{
    merge.zoo(..., all = all, fill = fill, suffixes = suffixes, 
        retclass = "zoo", drop = drop)
}


autoplot.zoo <- function (object, geom = "line", facets, ...) 
{
    lab <- deparse(substitute(object))
    if (NCOL(object) == 1L) {
        if (is.null(dim(object))) 
            dim(object) <- c(NROW(object), 1L)
        if (is.null(colnames(object))) 
            colnames(object) <- lab
    }
    if (is.null(colnames(object))) 
        colnames(object) <- paste(lab, 1:NCOL(object), sep = ".")
    df <- fortify.zoo(object, melt = TRUE)
    single <- nlevels(df$Series) == 1L
    if (missing(facets)) {
        auto <- TRUE
        facets <- if (single) 
            NULL
        else Series ~ .
    }
    else {
        auto <- FALSE
    }
    Index <- Value <- Series <- NULL
    gg <- if (single | (!is.null(facets) & auto)) {
        ggplot2::qplot(Index, Value, data = df, geom = geom, 
            facets = facets, ...) + ggplot2::ylab(if (single) 
            levels(df$Series)
        else "") + ggplot2::xlab("Index")
    }
    else {
        ggplot2::qplot(Index, Value, data = df, group = Series, 
            geom = geom, facets = facets, colour = Series, ...) + 
            ggplot2::ylab("") + ggplot2::xlab("Index")
    }
    return(gg)
}


scale_y_yearmon <- function (..., format = "%b %Y", n = 5) 
{
    ggplot2::scale_y_continuous(..., trans = yearmon_trans(format, 
        n))
}


as.zoo.default <- function (x, ...) 
{
    zoo(structure(x, dim = dim(x)), index(x), ...)
}


na.trim.ts <- function (object, ...) 
{
    as.ts(na.trim(as.zoo(object), ...))
}


zooreg <- function (data, start = 1, end = numeric(), frequency = 1, deltat = 1, 
    ts.eps = getOption("ts.eps"), order.by = NULL) 
{
    if (missing(frequency)) 
        frequency <- 1/deltat
    else if (missing(deltat)) 
        deltat <- 1/frequency
    if (frequency > 1 && abs(frequency - round(frequency)) < 
        ts.eps) 
        frequency <- round(frequency)
    if (missing(data) || is.null(data)) 
        data <- NA
    if (!(is.vector(data) || is.factor(data) || is.atomic(data) || 
        is.matrix(data) || is.data.frame(data))) 
        stop(paste(dQuote("data"), ": attempt to define invalid zoo object"))
    if (is.matrix(data) || is.data.frame(data)) 
        data <- as.matrix(data)
    if (is.null(order.by)) {
        if (!any(c(is.vector(data), is.factor(data), is.atomic(data), 
            is.matrix(data), is.data.frame(data)))) 
            stop(paste(dQuote("data"), ": attempt to define invalid zoo object"))
        ndata <- NROW(data)
        numORint <- function(x) identical(class(x), "numeric") | 
            identical(class(x), "integer")
        if (length(start) > 1) 
            start <- start[1] + (start[2] - 1)/frequency
        if (length(end) > 1) 
            end <- end[1] + (end[2] - 1)/frequency
        if (missing(end)) {
            ostart <- start
            oend <- NULL
            start <- as.numeric(start)
            end <- start + (ndata - 1)/frequency
        }
        else if (missing(start)) {
            ostart <- NULL
            oend <- end
            end <- as.numeric(end)
            start <- end - (ndata - 1)/frequency
        }
        else {
            ostart <- start
            oend <- NULL
            start <- as.numeric(start)
            end <- as.numeric(end)
        }
        if (start > end) 
            stop("start cannot be after end")
        order.by <- start + seq(0, length.out = ndata) * deltat
        if (identical(all.equal(start * frequency, round(start * 
            frequency)), TRUE)) {
            order.by <- floor(frequency * order.by + 1e-04)/frequency
        }
        if (!is.null(ostart) && !numORint(ostart)) 
            order.by <- ostart + (order.by - start)
        if (!is.null(oend) && !numORint(oend)) 
            order.by <- oend + (order.by - end)
        nobs <- length(order.by)
        if (nobs != ndata) {
            if (is.vector(data)) 
                data <- rep(data, length.out = nobs)
            else if (is.factor(data)) 
                data <- factor(rep(as.character(data), length.out = nobs), 
                  labels = levels(data))
            else if (is.matrix(data) || is.data.frame(data)) 
                data <- data[rep(1:ndata, length.out = nobs), 
                  , drop = FALSE]
        }
        attr(data, "oclass") <- attr(data, "class")
        attr(data, "index") <- order.by
        attr(data, "frequency") <- frequency
        class(data) <- c("zooreg", "zoo")
        return(data)
    }
    else {
        return(zoo(data, order.by, frequency))
    }
}


panel.segments.zoo <- function (x0, x1, ...) 
{
    x0 <- as.zoo(x0)
    x1 <- as.zoo(x1)
    panel.segments(time(x0), coredata(x0), time(x1), coredata(x1), 
        ...)
}


panel.text.ts <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.text(time(x), coredata(x), ...)
    .Deprecated("panel.text")
}


na.approx.default <- function (object, x = index(object), xout = x, ..., na.rm = TRUE, 
    maxgap = Inf, along) 
{
    if (!missing(along)) {
        warning("along to be deprecated - use x instead")
        if (missing(x)) 
            x <- along
    }
    na.approx.vec <- function(x, y, xout = x, ...) {
        na <- is.na(y)
        if (sum(!na) < 2L) {
            yf <- rep.int(NA, length(xout))
            if (any(!na)) {
                if (x[!na] %in% xout) {
                  yf[xout == x[!na]] <- y[!na]
                }
            }
            return(yf)
        }
        if (all(!na) && (length(xout) > maxgap) && !all(xout %in% 
            x)) {
            xf <- sort(unique(c(x, xout)))
            yf <- rep.int(NA, length(xf))
            yf[MATCH(x, xf)] <- y
            x <- xf
            y <- yf
        }
        yf <- approx(x[!na], y[!na], xout, ...)$y
        if (maxgap < length(y)) {
            ygap <- .fill_short_gaps(y, seq_along(y), maxgap = maxgap)
            ix <- approx(x, seq_along(y), xout, ...)$y
            yx <- ifelse(is.na(ygap[floor(ix)] + ygap[ceiling(ix)]), 
                NA, yf)
            yx
        }
        else {
            yf
        }
    }
    if (!identical(length(x), length(index(object)))) {
        stop("x and index must have the same length")
    }
    x. <- as.numeric(x)
    if (missing(xout) || is.null(xout)) 
        xout <- x.
    xout. <- as.numeric(xout)
    object. <- coredata(object)
    result <- if (length(dim(object.)) < 2) {
        na.approx.vec(x., coredata(object.), xout = xout., ...)
    }
    else {
        apply(coredata(object.), 2, na.approx.vec, x = x., xout = xout., 
            ...)
    }
    if (na.rm) {
        result <- na.trim(result, is.na = "all")
    }
    result
}


panel.points.tis <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.points(time(x), coredata(x), ...)
    .Deprecated("panel.points")
}


as.zooreg.default <- function (x, frequency = NULL, ...) 
{
    as.zooreg(as.zoo(x, ...), frequency = frequency)
}


scale_x_yearqtr <- function (..., format = "%Y-%q", n = 5) 
{
    ggplot2::scale_x_continuous(..., trans = yearqtr_trans(format, 
        n))
}


as.zoo <- function (x, ...) 
{
    UseMethod("as.zoo")
}


panel.polygon.its <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.polygon(time(x), coredata(x), ...)
}


rbind.zoo <- function (..., deparse.level = 1) 
{
    args <- Filter(Negate(is.null), list(...))
    indexes <- do.call("c", unname(lapply(args, index)))
    my.table <- function(x) {
        ix <- ORDER(x)
        x <- x[ix]
        table(MATCH(x, x))
    }
    if (max(my.table(indexes)) > 1L) 
        stop("indexes overlap")
    if (any(sapply(args, function(x) is.null(dim(x)) && length(x) == 
        0L && length(index(x)) > 0L))) 
        stop("zero-length vectors with non-zero-length index are not allowed")
    ncols <- sapply(args, NCOL)
    if (!all(ncols == ncols[1L])) 
        stop("number of columns differ")
    nams <- lapply(args, colnames)
    namsNULL <- sapply(nams, is.null)
    if (all(namsNULL)) 
        namsOK <- TRUE
    else {
        if (sum(namsNULL) > 0L) 
            namsOK <- FALSE
        else {
            nam1 <- nams[[1L]]
            namsID <- sapply(nams, function(x) identical(x, nam1))
            if (all(namsID)) 
                namsOK <- TRUE
            else {
                namsSORT <- sapply(nams, function(x) identical(sort(x), 
                  sort(nam1)))
                if (!all(namsSORT)) 
                  namsOK <- FALSE
                else {
                  namsOK <- TRUE
                  for (i in which(!namsID)) args[[i]] <- args[[i]][, 
                    nam1]
                }
            }
        }
    }
    if (!namsOK) 
        warning("column names differ")
    argsdata <- lapply(args, coredata)
    nulldim <- sapply(argsdata, function(a) is.null(dim(a)))
    if (ncols[1L] == 1L) {
        if (nulldim[1] & any(!nulldim)) {
            argsdata <- lapply(argsdata, function(a) if (is.null(dim(a))) 
                a
            else a[, 1, drop = TRUE])
            nulldim <- rep(TRUE, length(nulldim))
        }
        if (!nulldim[1] & any(nulldim)) {
            argsdata <- lapply(argsdata, function(a) if (is.null(dim(a))) 
                as.matrix(a)
            else a)
            nulldim <- rep(FALSE, length(nulldim))
        }
    }
    if ((ncols[1L] > 1L) | !all(nulldim)) 
        rval <- zoo(do.call("rbind", argsdata), indexes)
    else rval <- zoo(do.call("c", argsdata), indexes)
    freq <- if (!("zooreg" %in% unlist(sapply(args, class)))) 
        NULL
    else {
        freq <- c(frequency(rval), unlist(sapply(args, frequency)))
        if ((length(freq) == (length(args) + 1L)) && identical(all.equal(max(freq)/freq, 
            round(max(freq)/freq)), TRUE)) 
            max(freq)
        else NULL
    }
    if (!is.null(freq)) {
        attr(rval, "frequency") <- freq
        class(rval) <- c("zooreg", class(rval))
    }
    return(rval)
}


ifelse.zoo <- function (test, yes, no) 
{
    if (!is.zoo(test)) 
        test <- zoo(test, index(yes))
    merge(test, yes, no, retclass = NULL)
    ifelse(test, yes, no)
}


median.zoo <- function (x, na.rm = FALSE) 
median(coredata(x), na.rm = na.rm)


rollapply <- function (data, ...) 
UseMethod("rollapply")


na.StructTS <- function (object, ...) 
UseMethod("na.StructTS")


rollmaxr <- function (..., align = "right") 
{
    rollmax(..., align = align)
}


as.Date.yearqtr <- function (x, frac = 0, ...) 
{
    x <- unclass(x)
    if (all(is.na(x))) 
        return(as.Date(x))
    year <- floor(x + 0.001)
    ix <- !is.na(year)
    month <- floor(12 * (x - year) + 1 + 0.5 + 0.001)
    dd.start <- as.Date(rep(NA, length(year)))
    dd.start[ix] <- as.Date(paste(year[ix], month[ix], 1, sep = "-"))
    dd.end <- dd.start + 100 - as.numeric(format(dd.start + 100, 
        "%d"))
    as.Date((1 - frac) * as.numeric(dd.start) + frac * as.numeric(dd.end), 
        origin = "1970-01-01")
}


panel.lines.zoo <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.lines(time(x), coredata(x), ...)
    .Deprecated("panel.lines")
}


rollmax <- function (x, k, fill = if (na.pad) NA, na.pad = FALSE, align = c("center", 
    "left", "right"), ...) 
{
    UseMethod("rollmax")
}


as.Date.numeric <- function (x, origin, ...) 
{
    if (missing(origin)) 
        origin <- "1970-01-01"
    if (identical(origin, "0000-00-00")) 
        origin <- as.Date("0000-01-01", ...) - 1
    as.Date(origin, ...) + x
}


as.yearqtr.default <- function (x, ...) 
as.yearqtr(as.numeric(x))


read.delim2.zoo <- function (file, format = "", tz = "", FUN = NULL, regular = FALSE, 
    index.column = 1, drop = TRUE, FUN2 = NULL, split = NULL, 
    aggregate = FALSE, ...) 
{
    file <- read.delim2(file, ...)
    read.zoo(file, format = format, tz = tz, FUN = FUN, regular = regular, 
        index.column = index.column, drop = drop, FUN2 = FUN2, 
        split = split, aggregate = aggregate)
}


quantile.zoo <- function (x, ...) 
quantile(coredata(x), ...)


rollsum <- function (x, k, fill = if (na.pad) NA, na.pad = FALSE, align = c("center", 
    "left", "right"), ...) 
{
    UseMethod("rollsum")
}


yearmon_trans <- function (format = "%b %Y", n = 5) 
{
    breaks. <- function(x) as.yearmon((scales::pretty_breaks(n))(x))
    format. <- function(x) format(x, format = format)
    scales::trans_new("yearmon", transform = as.numeric, inverse = as.yearmon, 
        breaks = breaks., format = format.)
}


na.locf.default <- function (object, na.rm = TRUE, fromLast, rev, maxgap = Inf, 
    rule = 2, ...) 
{
    L <- list(...)
    if ("x" %in% names(L) || "xout" %in% names(L)) {
        if (!missing(fromLast)) {
            stop("fromLast not supported if x or xout is specified")
        }
        return(na.approx(object, na.rm = na.rm, maxgap = maxgap, 
            method = "constant", rule = rule, ...))
    }
    na.locf.0 <- function(x) {
        L <- !is.na(x)
        idx <- if (fromLast) 
            rev(c(NA, rev(which(L)))[cumsum(rev(L)) + 1])
        else c(NA, which(L))[cumsum(L) + 1]
        na.index <- function(x, i) {
            L <- !is.na(i)
            x[!L] <- NA
            x[L] <- coredata(x)[i[L]]
            x
        }
        xf <- na.index(x, idx)
        .fill_short_gaps(x, xf, maxgap = maxgap)
    }
    if (!missing(rev)) {
        warning("na.locf.default: rev= deprecated. Use fromLast= instead.")
        if (missing(fromLast)) 
            fromLast <- rev
    }
    else if (missing(fromLast)) 
        fromLast <- FALSE
    rev <- base::rev
    object[] <- if (length(dim(object)) == 0) 
        na.locf.0(object)
    else apply(object, length(dim(object)), na.locf.0)
    if (na.rm) 
        na.trim(object, is.na = "all")
    else object
}


panel.plot.default <- function (x, y, subscripts, groups, panel = panel.xyplot, col = 1, 
    type = "p", pch = 20, lty = 1, lwd = 1, ...) 
{
    col <- rep(as.list(col), length.out = nlevels(groups))
    type <- rep(as.list(type), length.out = nlevels(groups))
    pch <- rep(as.list(pch), length.out = nlevels(groups))
    lty <- rep(as.list(lty), length.out = nlevels(groups))
    lwd <- rep(as.list(lwd), length.out = nlevels(groups))
    for (g in 1:nlevels(groups)) {
        idx <- g == unclass(groups[subscripts])
        if (any(idx)) 
            panel(x[idx], y[idx], ..., col = col[[g]], type = type[[g]], 
                pch = pch[[g]], lty = lty[[g]], lwd = lwd[[g]])
    }
    .Deprecated(msg = "panel.plot.default is no longer needed, just use panel.xyplot etc")
}


na.trim <- function (object, ...) 
UseMethod("na.trim")


na.aggregate.default <- function (object, by = 1, ..., FUN = mean, na.rm = FALSE, maxgap = Inf) 
{
    if (is.function(by)) 
        by <- by(time(object), ...)
    f <- function(x) replace(x, is.na(x), FUN(x[!is.na(x)]))
    na.aggregate.0 <- function(y) {
        yf <- ave(y, by, FUN = f)
        .fill_short_gaps(y, yf, maxgap = maxgap)
    }
    object[] <- if (length(dim(object)) == 0) 
        na.aggregate.0(object)
    else apply(object, 2, na.aggregate.0)
    if (na.rm) 
        na.trim(object, is.na = "all")
    else object
}


zoo <- function (x = NULL, order.by = index(x), frequency = NULL) 
{
    if (length(unique(MATCH(order.by, order.by))) < length(order.by)) 
        warning(paste("some methods for", dQuote("zoo"), "objects do not work if the index entries in", 
            sQuote("order.by"), "are not unique"))
    index <- ORDER(order.by)
    order.by <- order.by[index]
    if (is.matrix(x) || is.data.frame(x)) 
        x <- as.matrix(x)
    if (is.matrix(x) && sum(dim(x)) < 1L) 
        x <- NULL
    if (missing(x) || is.null(x)) 
        x <- numeric()
    else if (is.factor(x)) 
        x <- factor(rep(as.character(x), length.out = length(index))[index], 
            levels = levels(x), ordered = is.ordered(x))
    else if (is.matrix(x) || is.data.frame(x)) 
        x <- (x[rep(1:NROW(x), length.out = length(index)), , 
            drop = FALSE])[index, , drop = FALSE]
    else if (is.atomic(x)) 
        x <- rep(x, length.out = length(index))[index]
    else stop(paste(dQuote("x"), ": attempt to define invalid zoo object"))
    if (!is.null(frequency)) {
        delta <- suppressWarnings(try(diff(as.numeric(order.by)), 
            silent = TRUE))
        freqOK <- if (class(delta) == "try-error" || any(is.na(delta))) 
            FALSE
        else if (length(delta) < 1) 
            TRUE
        else identical(all.equal(delta * frequency, round(delta * 
            frequency)), TRUE)
        if (!freqOK) {
            warning(paste(dQuote("order.by"), "and", dQuote("frequency"), 
                "do not match:", dQuote("frequency"), "ignored"))
            frequency <- NULL
        }
        else {
            if (frequency > 1 && identical(all.equal(frequency, 
                round(frequency)), TRUE)) 
                frequency <- round(frequency)
        }
        if (!is.null(frequency) && identical(class(order.by), 
            "numeric") | identical(class(order.by), "integer")) {
            orig.order.by <- order.by
            order.by <- floor(frequency * order.by + 1e-04)/frequency
            if (!isTRUE(all.equal(order.by, orig.order.by))) 
                order.by <- orig.order.by
        }
    }
    attr(x, "oclass") <- attr(x, "class")
    attr(x, "index") <- order.by
    attr(x, "frequency") <- frequency
    class(x) <- if (is.null(frequency)) 
        "zoo"
    else c("zooreg", "zoo")
    return(x)
}


panel.segments.its <- function (x0, x1, ...) 
{
    x0 <- as.zoo(x0)
    x1 <- as.zoo(x1)
    panel.segments(time(x0), coredata(x0), time(x1), coredata(x1), 
        ...)
}


format.yearqtr <- function (x, format = "%Y Q%q", ...) 
{
    if (length(x) == 0) 
        return(character(0))
    gsub.vec <- function(pattern, replacement, x, ...) {
        y <- x
        for (i in seq_along(x)) {
            y[i] <- gsub(pattern, replacement[i], x[i], ...)
        }
        y
    }
    x <- as.yearqtr(x)
    x <- unclass(x)
    year <- floor(x + 0.001)
    qtr <- floor(4 * (x - year) + 1 + 0.5 + 0.001)
    if (format == "%Y Q%q") 
        return(paste(year, " Q", qtr, sep = ""))
    xx <- gsub.vec("%q", qtr, rep(format, length(qtr)))
    xx <- gsub.vec("%Y", year, xx)
    xx <- gsub.vec("%y", sprintf("%02d", as.integer(year%%100)), 
        xx)
    xx <- gsub.vec("%C", year%/%100, xx)
    names(xx) <- names(x)
    xx
}


rollapplyr <- function (..., align = "right") 
{
    rollapply(..., align = align)
}


rollmedian.default <- function (x, k, fill = if (na.pad) NA, na.pad = FALSE, align = c("center", 
    "left", "right"), ...) 
{
    coredata(rollmedian(zoo(x), k, fill = fill, align = align, 
        ...))
}


panel.text.tis <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.text(time(x), coredata(x), ...)
    .Deprecated("panel.text")
}


panel.plot.custom <- function (...) 
{
    args <- list(...)
    function(...) {
        dots <- list(...)
        do.call("panel.plot.default", modifyList(dots, args))
    }
}


na.spline.default <- function (object, x = index(object), xout = x, ..., na.rm = TRUE, 
    maxgap = Inf, along) 
{
    if (!missing(along)) {
        warning("along to be deprecated - use x instead")
        if (missing(x)) 
            x <- along
    }
    na.spline.vec <- function(x, y, xout = x, ...) {
        na <- is.na(y)
        yf <- splinefun(x[!na], y[!na], ...)(xout)
        if (maxgap < length(y)) {
            ygap <- .fill_short_gaps(y, seq_along(y), maxgap = maxgap)
            ix <- splinefun(x, seq_along(y), ...)(xout)
            yx <- ifelse(is.na(ygap[floor(ix)] + ygap[ceiling(ix)]), 
                NA, yf)
            yx
        }
        else {
            yf
        }
    }
    if (!identical(length(x), length(index(object)))) {
        stop("x and index must have the same length")
    }
    x. <- as.numeric(x)
    if (missing(xout) || is.null(xout)) 
        xout <- x.
    xout. <- as.numeric(xout)
    object. <- coredata(object)
    result <- if (length(dim(object.)) < 2) {
        na.spline.vec(x., coredata(object.), xout = xout., ...)
    }
    else {
        apply(coredata(object.), 2, na.spline.vec, x = x., xout = xout., 
            ...)
    }
    if (na.rm) {
        result <- na.trim(result, is.na = "all")
    }
    result
}


panel.text.zoo <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.text(time(x), coredata(x), ...)
    .Deprecated("panel.text")
}


panel.rect.its <- function (x0, x1, ...) 
{
    x0 <- as.zoo(x0)
    x1 <- as.zoo(x1)
    panel.rect(time(x0), coredata(x0), time(x1), coredata(x1), 
        ...)
}


read.zoo <- function (file, format = "", tz = "", FUN = NULL, regular = FALSE, 
    index.column = 1, drop = TRUE, FUN2 = NULL, split = NULL, 
    aggregate = FALSE, ..., text) 
{
    if (missing(file) && !missing(text)) {
        file <- textConnection(text)
        on.exit(close(file))
    }
    if (is.character(file) && length(file) > 1) {
        mc <- match.call()
        pf <- parent.frame()
        L <- sapply(file, function(file) eval(replace(mc, 2, 
            file), pf), simplify = FALSE)
        return(do.call("merge", L))
    }
    rval <- if (is.data.frame(file)) 
        file
    else read.table(file, ...)
    if (is.data.frame(file) && length(index.column) == 1 && !is.character(rval[[index.column]]) && 
        !is.factor(rval[[index.column]]) && missing(tz) && missing(format) && 
        missing(FUN)) 
        FUN <- identity
    if (is.data.frame(file) && length(index.column) == 1 && inherits(rval[[index.column]], 
        "POSIXlt")) 
        rval[[index.column]] <- as.POSIXct(rval[[index.column]])
    no.default <- function(x) typeof(x) %in% c("symbol", "language")
    if (is.null(FUN) && is.null(FUN2)) {
        index.column <- as.list(index.column)
    }
    else if (identical(FUN, paste)) {
        index.column <- as.list(index.column)
    }
    else if (is.null(FUN) && identical(FUN2, paste)) {
        index.column <- as.list(index.column)
    }
    else if (!is.null(FUN) && !is.list(index.column) && length(index.column) <= 
        length(sapply(formals(match.fun(FUN)), no.default))) {
        index.column <- as.list(index.column)
    }
    else if (is.null(FUN) && !is.null(FUN2) && length(index.column) <= 
        length(sapply(formals(match.fun(FUN2)), no.default))) {
        index.column <- as.list(index.column)
    }
    if (is.list(index.column) && length(index.column) == 1 && 
        index.column[[1]] == 1) 
        index.column <- unlist(index.column)
    is.index.column <- seq_along(rval) %in% unname(unlist(index.column)) | 
        names(rval) %in% unname(unlist(index.column))
    name.to.num <- function(x) if (is.character(x)) 
        match(x, names(rval), nomatch = 0)
    else x
    index.column <- if (is.list(index.column)) 
        index.column <- lapply(index.column, name.to.num)
    else name.to.num(index.column)
    is.fac <- sapply(rval, is.factor)
    is.fac.index <- is.fac & is.index.column
    if (any(is.fac.index)) 
        rval[is.fac.index] <- lapply(rval[is.fac.index], as.character)
    if (NROW(rval) < 1) {
        if (is.data.frame(rval)) 
            rval <- as.matrix(rval)
        if (NCOL(rval) > 1) 
            rval <- rval[, !is.index.column, drop = drop]
        rval2 <- zoo(rval)
        return(rval2)
    }
    if (NCOL(rval) < 1) 
        stop("data file must specify at least one column")
    ix <- if (identical(index.column, 0) || identical(index.column, 
        list(0)) || identical(index.column, 0L) || identical(index.column, 
        list(0L))) {
        attributes(rval)$row.names
    }
    else if (is.list(index.column)) {
        sapply(index.column, function(j) rval[, j], simplify = FALSE)
    }
    else rval[, index.column]
    split. <- if (is.character(split)) 
        match(split, colnames(rval), nomatch = 0)
    else split
    rval2 <- if (is.null(split.)) {
        rval[, !is.index.column, drop = FALSE]
    }
    else {
        split.values <- if (is.character(split) || is.finite(split)) 
            rval[, split]
        else {
            if (identical(split, Inf)) 
                ave(ix, ix, FUN = seq_along)
            else if (identical(split, -Inf)) 
                ave(ix, ix, FUN = function(x) rev(seq_along(x)))
            else ix
        }
        if (0 %in% split.) {
            stop(paste("split:", split, "not found in colnames:", 
                colnames(rval)))
        }
        rval[, -c(if (is.finite(split.)) split. else 0, which(is.index.column)), 
            drop = FALSE]
    }
    if (is.factor(ix)) 
        ix <- as.character(ix)
    rval3 <- if (is.data.frame(rval2)) 
        as.matrix(rval2)
    else if (is.list(rval2)) 
        t(rval2)
    else rval2
    if (NCOL(rval3) == 1 && drop) 
        rval3 <- drop(rval3)
    toDate <- if (missing(format) || is.null(format)) {
        function(x, ...) as.Date(format(x, scientific = FALSE))
    }
    else {
        function(x, format) {
            if (any(sapply(c("%H", "%M", "%S"), function(y) grepl(y, 
                format, fixed = TRUE)))) {
                warning("the 'format' appears to be for a date/time, please specify 'tz' if you want to create a POSIXct time index")
            }
            as.Date(format(x, scientific = FALSE), format = format)
        }
    }
    toPOSIXct <- if (missing(format) || is.null(format)) {
        function(x, tz) as.POSIXct(format(x, scientific = FALSE), 
            tz = tz)
    }
    else function(x, format, tz) {
        as.POSIXct(strptime(format(x, scientific = FALSE), tz = tz, 
            format = format))
    }
    toDefault <- function(x, ...) {
        rval. <- try(toPOSIXct(x, tz = ""), silent = TRUE)
        if (inherits(rval., "try-error")) 
            rval. <- try(toDate(x), silent = TRUE)
        else {
            hms <- as.POSIXlt(rval.)
            hms <- hms$sec + 60 * hms$min + 3600 * hms$hour
            if (isTRUE(all.equal(hms, rep.int(hms[1], length(hms))))) {
                rval2. <- try(toDate(x), silent = TRUE)
                if (!inherits(rval2., "try-error")) 
                  rval. <- rval2.
            }
        }
        if (inherits(rval., "try-error")) 
            rval. <- rep(NA, length(x))
        return(rval.)
    }
    toNumeric <- function(x, ...) x
    if ((missing(FUN) || is.null(FUN)) && !missing(FUN2) && !is.null(FUN2)) {
        FUN <- FUN2
        FUN2 <- NULL
    }
    FUN0 <- NULL
    if (is.null(FUN)) {
        if (is.list(index.column)) 
            FUN0 <- paste
        FUN <- if (!missing(tz) && !is.null(tz)) 
            toPOSIXct
        else if (!missing(format) && !is.null(format)) 
            toDate
        else if (is.numeric(ix)) 
            toNumeric
        else toDefault
    }
    FUN <- match.fun(FUN)
    processFUN <- function(...) {
        if (is.data.frame(..1)) 
            FUN(...)
        else if (is.list(..1)) {
            if (is.null(FUN0)) 
                do.call(FUN, c(...))
            else {
                L <- list(...)
                L[[1]] <- do.call(FUN0, L[[1]])
                do.call(FUN, L)
            }
        }
        else FUN(...)
    }
    ix <- if (missing(format) || is.null(format)) {
        if (missing(tz) || is.null(tz)) 
            processFUN(ix)
        else processFUN(ix, tz = tz)
    }
    else {
        if (missing(tz) || is.null(tz)) 
            processFUN(ix, format = format)
        else processFUN(ix, format = format, tz = tz)
    }
    if (!is.null(FUN2)) {
        FUN2 <- match.fun(FUN2)
        ix <- FUN2(ix)
    }
    if (any(is.na(ix))) {
        idx <- which(is.na(ix))
        msg <- if (length(idx) == 1) 
            paste("index has bad entry at data row", idx)
        else if (length(idx) <= 100) 
            paste("index has bad entries at data rows:", paste(idx, 
                collapse = " "))
        else paste("index has", length(idx), "bad entries at data rows:", 
            paste(head(idx, 100), collapse = " "), "...")
        stop(msg)
    }
    if (length(ix) != NROW(rval3)) 
        stop("index does not match data")
    if (identical(aggregate, TRUE)) {
        agg.fun <- mean
    }
    else if (identical(aggregate, FALSE)) {
        agg.fun <- NULL
    }
    else {
        agg.fun <- match.fun(aggregate)
        if (!is.function(agg.fun)) 
            stop(paste("invalid specification of", sQuote("aggregate")))
    }
    remove(list = "aggregate")
    if (is.null(split)) {
        rval4 <- if (!is.null(agg.fun)) 
            aggregate(zoo(rval3), ix, agg.fun)
        else zoo(rval3, ix)
        rval8 <- if (regular && is.regular(rval4)) 
            as.zooreg(rval4)
        else rval4
    }
    else {
        split.matrix <- split.data.frame
        rval4 <- split(rval3, split.values)
        ix <- split(ix, split.values)
        rval5 <- if (!is.null(agg.fun)) {
            lapply(seq_along(rval4), function(i) {
                aggregate(zoo(rval4[[i]]), ix[[i]], agg.fun)
            })
        }
        else lapply(seq_along(rval4), function(i) zoo(rval4[[i]], 
            ix[[i]]))
        names(rval5) <- names(rval4)
        rval6 <- if (regular) {
            lapply(rval5, function(x) if (is.regular(x)) 
                as.zooreg(x)
            else x)
        }
        else rval5
        rval8 <- do.call(merge, rval6)
    }
    return(rval8)
}


panel.lines.tis <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.lines(time(x), coredata(x), ...)
    .Deprecated("panel.lines")
}


as.yearmon <- function (x, ...) 
UseMethod("as.yearmon")


yearqtr_trans <- function (format = "%Y-%q", n = 5) 
{
    breaks. <- function(x) as.yearqtr((scales::pretty_breaks(n))(x))
    format. <- function(x) zoo::format.yearqtr(x, format = format)
    scales::trans_new("yearqtr", transform = as.numeric, inverse = as.yearqtr, 
        breaks = breaks., format = format.)
}


na.approx <- function (object, ...) 
UseMethod("na.approx")


coredata <- function (x, ...) 
UseMethod("coredata")


facet_free <- function (facets = Series ~ ., margins = FALSE, scales = "free_y", 
    ...) 
{
    ggplot2::facet_grid(facets, margins = margins, scales = scales, 
        ...)
}


coredata.default <- function (x, ...) 
x


ORDER.default <- function (x, ..., na.last = TRUE, decreasing = FALSE) 
order(x, ..., na.last = na.last, decreasing = decreasing)


na.locf <- function (object, na.rm = TRUE, ...) 
UseMethod("na.locf")


panel.segments.ts <- function (x0, x1, ...) 
{
    x0 <- as.zoo(x0)
    x1 <- as.zoo(x1)
    panel.segments(time(x0), coredata(x0), time(x1), coredata(x1), 
        ...)
}


rollsum.default <- function (x, k, fill = if (na.pad) NA, na.pad = FALSE, align = c("center", 
    "left", "right"), ...) 
{
    coredata(rollsum(zoo(x), k, fill = fill, align = align, ...))
}


na.fill <- function (object, fill, ...) 
UseMethod("na.fill")


MATCH.default <- function (x, table, nomatch = NA, ...) 
match(x, table, nomatch = nomatch, ...)


Sys.yearmon <- function () 
as.yearmon(Sys.Date())


read.csv.zoo <- function (file, format = "", tz = "", FUN = NULL, regular = FALSE, 
    index.column = 1, drop = TRUE, FUN2 = NULL, split = NULL, 
    aggregate = FALSE, ...) 
{
    file <- read.csv(file, ...)
    read.zoo(file, format = format, tz = tz, FUN = FUN, regular = regular, 
        index.column = index.column, drop = drop, FUN2 = FUN2, 
        split = split, aggregate = aggregate)
}


merge.zoo <- function (..., all = TRUE, fill = NA, suffixes = NULL, check.names = FALSE, 
    retclass = c("zoo", "list", "data.frame"), drop = TRUE) 
{
    if (!is.null(retclass)) 
        retclass <- match.arg(retclass)
    cl <- as.list(match.call())
    cl[[1]] <- cl$all <- cl$fill <- cl$retclass <- cl$suffixes <- cl$check.names <- cl$drop <- NULL
    args <- list(...)
    parent <- parent.frame()
    is.plain <- function(x) all(class(x) %in% c("array", "integer", 
        "numeric", "factor", "matrix", "logical"))
    is.scalar <- function(x) is.plain(x) && length(x) == 1
    stopifnot(all(sapply(args, function(x) is.zoo(x) || !is.plain(x) || 
        (is.plain(x) && (NROW(x) == NROW(args[[1]]) || is.scalar(x))))))
    scalars <- sapply(args, is.scalar)
    if (!is.zoo(args[[1]])) 
        args[[1]] <- as.zoo(args[[1]])
    for (i in seq_along(args)) if (is.plain(args[[i]])) 
        args[[i]] <- zoo(args[[i]], index(args[[1]]), attr(args[[1]], 
            "frequency"))
    else if (!is.zoo(args[[i]])) 
        args[[i]] <- as.zoo(args[[i]])
    freq <- if (!("zooreg" %in% unlist(sapply(args, class)))) 
        NULL
    else {
        freq <- unlist(sapply(args, frequency))
        if ((length(freq) == length(args)) && identical(all.equal(max(freq)/freq, 
            round(max(freq)/freq)), TRUE)) 
            max(freq)
        else NULL
    }
    if (is.null(suffixes)) {
        makeNames <- function(l) {
            nm <- names(l)
            fixup <- if (is.null(nm)) 
                seq_along(l)
            else nm == ""
            dep <- sapply(l[fixup], function(x) deparse(x)[1])
            if (is.null(nm)) 
                return(dep)
            if (any(fixup)) 
                nm[fixup] <- dep
            nm
        }
        suffixes <- makeNames(as.list(substitute(list(...)))[-1])
    }
    if (length(suffixes) != length(cl)) {
        warning("length of suffixes and does not match number of merged objects")
        suffixes <- rep(suffixes, length.out = length(cl))
    }
    all <- rep(as.logical(all), length.out = length(cl))
    indexlist <- lapply(args, index)
    index_duplicates <- function(x) length(unique(MATCH(x, x))) < 
        length(x)
    if (any(sapply(indexlist, index_duplicates))) 
        stop("series cannot be merged with non-unique index entries in a series")
    indexclasses <- sapply(indexlist, function(x) class(x)[1])
    if (!all(indexclasses == indexclasses[1])) 
        warning(paste("Index vectors are of different classes:", 
            paste(indexclasses, collapse = " ")))
    sort.unique <- function(x) {
        ix <- MATCH(x, x) == seq_len(length(x))
        x <- x[ix]
        ix <- ORDER(x)
        x[ix]
    }
    intersect.list <- function(list) {
        my.table <- function(x) {
            ix <- ORDER(x)
            x <- x[ix]
            table(MATCH(x, x))
        }
        union <- do.call("c", unname(list))
        sort.unique(union)[which(my.table(union) == length(list))]
    }
    indexintersect <- intersect.list(indexlist)
    indexunion <- do.call("c", unname(indexlist[all]))
    indexes <- if (is.null(indexunion)) 
        indexintersect
    else sort.unique(c(indexunion, indexintersect))
    if (!is.null(freq)) {
        freq <- c(frequency(zoo(, indexes)), freq)
        freq <- if ((length(freq) == 2) && identical(all.equal(max(freq)/freq, 
            round(max(freq)/freq)), TRUE)) 
            max(freq)
        else NULL
    }
    match0 <- function(a, b, nomatch = 0, ...) MATCH(a, b, nomatch = nomatch, 
        ...)
    f <- if (any(all)) {
        function(a, ret.zoo = TRUE) {
            if (length(a) == 0 && length(dim(a)) == 0) 
                return(if (ret.zoo) {
                  rval <- zoo(, indexes)
                  attr(rval, "frequency") <- freq
                  if (!is.null(freq)) class(rval) <- c("zooreg", 
                    class(rval))
                  rval
                } else numeric())
            z <- matrix(fill, length(indexes), NCOL(a))
            if (length(dim(a)) > 0) 
                z[match0(index(a), indexes), ] <- a[match0(indexes, 
                  index(a)), , drop = FALSE]
            else {
                z[match0(index(a), indexes), ] <- a[match0(indexes, 
                  index(a))]
                z <- z[, 1, drop = TRUE]
            }
            if (ret.zoo) {
                z <- zoo(z, indexes)
                attr(z, "oclass") <- attr(a, "oclass")
                attr(z, "levels") <- attr(a, "levels")
                attr(z, "frequency") <- freq
                if (!is.null(freq)) 
                  class(z) <- c("zooreg", class(z))
            }
            return(z)
        }
    }
    else {
        function(a, ret.zoo = TRUE) {
            if (!ret.zoo) 
                class(a) <- NULL
            if (length(dim(a)) == 0) {
                if (length(a) == 0) {
                  rval <- if (ret.zoo) 
                    zoo(, indexes)
                  else numeric()
                }
                else rval <- as.zoo(a[match0(indexes, attr(a, 
                  "index"))])
            }
            else rval <- as.zoo(a[match0(indexes, attr(a, "index")), 
                , drop = FALSE])
            if (is.zoo(rval) && !is.null(freq)) {
                attr(rval, "frequency") <- freq
                class(rval) <- unique(c("zooreg", class(rval)))
            }
            return(rval)
        }
    }
    if (is.null(retclass)) {
        for (vn in cl) {
            if (is.name(vn)) 
                tryCatch(eval(substitute(v <- f(v), list(f = f, 
                  v = vn)), parent), condition = function(x) {
                })
        }
        invisible(return(NULL))
    }
    rval <- lapply(args, f, ret.zoo = retclass %in% c("list", 
        "data.frame"))
    names(rval) <- suffixes
    if (retclass == "list") {
        return(rval)
    }
    if (retclass == "data.frame") {
        charindex <- index2char(index(rval[[1]]), frequency = freq)
        nam1 <- names(rval)
        rval <- lapply(rval, as.list)
        todf <- function(x) {
            class(x) <- "data.frame"
            attr(x, "row.names") <- charindex
            return(x)
        }
        rval <- lapply(rval, todf)
        nam2 <- sapply(rval, function(z) 1:NCOL(z))
        for (i in 1:length(nam2)) nam2[[i]] <- paste(names(nam2)[i], 
            nam2[[i]], sep = ".")
        nam1 <- unlist(ifelse(sapply(rval, NCOL) > 1, nam2, nam1))
        rval <- do.call("cbind", rval)
        names(rval) <- nam1
        is.zoofactor <- function(x) !is.null(attr(x, "oclass")) && 
            attr(x, "oclass") == "factor"
        for (i in 1:NCOL(rval)) if (is.zoofactor(rval[, i])) 
            rval[, i] <- coredata(rval[, i])
        return(rval)
    }
    rval <- rval[sapply(rval, function(x) length(x) > 0)]
    rval <- if (length(rval) > 1L | (length(rval) == 1L & !drop)) 
        do.call("cbind", rval)
    else if (length(rval) > 0L) 
        rval[[1]]
    if (length(dim(rval)) == 0L) {
        rval <- zoo(coredata(rval), indexes)
        attr(rval, "frequency") <- freq
        if (!is.null(freq)) 
            class(rval) <- c("zooreg", class(rval))
        return(rval)
    }
    if (length(unlist(sapply(args, colnames))) > 0) {
        fixcolnames <- function(a) {
            if (length(dim(a)) == 0) {
                if (length(a) == 0) 
                  return(NULL)
                else return("")
            }
            else {
                if (ncol(a) == 0) 
                  return(NULL)
                rval <- colnames(a)
                if (is.null(rval)) {
                  rval <- paste(1:NCOL(a), suffixes[i], sep = ".")
                }
                else {
                  rval[rval == ""] <- as.character(which(rval == 
                    ""))
                }
                return(rval)
            }
        }
        zoocolnames <- lapply(args, fixcolnames)
        zcn <- unlist(zoocolnames)
        fixme <- lapply(zoocolnames, function(x) x %in% zcn[duplicated(zcn)])
        f <- function(i) {
            rval <- zoocolnames[[i]]
            rval[rval == ""] <- suffixes[i]
            rval
        }
        zoocolnames <- lapply(seq_along(args), f)
        f <- function(i) ifelse(fixme[[i]], paste(zoocolnames[[i]], 
            suffixes[i], sep = "."), zoocolnames[[i]])
        if (any(duplicated(unlist(zoocolnames)))) 
            zoocolnames <- lapply(seq_along(args), f)
    }
    else {
        fixcolnames <- function(a) {
            if (length(a) == 0) 
                return(NULL)
            if (NCOL(a) < 2) 
                return("")
            else return(paste(".", 1:NCOL(a), sep = ""))
        }
        zoocolnames <- lapply(args, fixcolnames)
        zoocolnames <- lapply(seq_along(args), function(i) if (!is.null(zoocolnames[[i]])) 
            paste(suffixes[i], zoocolnames[[i]], sep = ""))
    }
    zoocolnames <- unlist(zoocolnames)
    colnames(rval) <- if (check.names) 
        make.names(make.unique(zoocolnames))
    else if (ncol(rval) == length(zoocolnames)) 
        zoocolnames
    else colnames(rval)
    rval <- zoo(coredata(rval), indexes)
    attr(rval, "frequency") <- freq
    if (!is.null(freq)) 
        class(rval) <- c("zooreg", class(rval))
    return(rval)
}


panel.text.its <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.text(time(x), coredata(x), ...)
    .Deprecated("panel.text")
}


yearmon <- function (x) 
structure(floor(12 * x + 1e-04)/12, class = "yearmon")


panel.points.ts <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.points(time(x), coredata(x), ...)
    .Deprecated("panel.points")
}


MATCH <- function (x, table, nomatch = NA, ...) 
UseMethod("MATCH")


panel.lines.its <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.lines(time(x), coredata(x), ...)
    .Deprecated("panel.lines")
}


panel.polygon.ts <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.polygon(time(x), coredata(x), ...)
}


xblocks <- function (x, ...) 
UseMethod("xblocks")


is.zoo <- function (object) 
inherits(object, "zoo")


plot.zoo <- function (x, y = NULL, screens, plot.type, panel = lines, xlab = "Index", 
    ylab = NULL, main = NULL, xlim = NULL, ylim = NULL, xy.labels = FALSE, 
    xy.lines = NULL, yax.flip = FALSE, oma = c(6, 0, 5, 0), mar = c(0, 
        5.1, 0, if (yax.flip) 5.1 else 2.1), col = 1, lty = 1, 
    lwd = 1, pch = 1, type = "l", log = "", nc, widths = 1, heights = 1, 
    ...) 
{
    if (!is.null(y)) {
        if (NCOL(x) > 1 || NCOL(y) > 1) 
            stop("scatter plots only for univariate zoo series")
        xyzoo <- merge.zoo(x, y, all = FALSE)
        xy <- coredata(xyzoo)
        xy <- xy.coords(xy[, 1], xy[, 2])
        xlab <- if (missing(xlab)) 
            deparse(substitute(x))
        else xlab
        ylab <- if (missing(ylab)) 
            deparse(substitute(y))
        else ylab
        xlim <- if (is.null(xlim)) 
            range(xy$x[is.finite(xy$x)])
        else xlim
        ylim <- if (is.null(ylim)) 
            range(xy$y[is.finite(xy$y)])
        else ylim
        if (is.null(main)) 
            main <- ""
        do.lab <- if (is.logical(xy.labels)) 
            xy.labels
        else TRUE
        if (is.null(xy.lines)) 
            xy.lines <- do.lab
        ptype <- if (do.lab) 
            "n"
        else if (missing(type)) 
            "p"
        else type
        plot.default(xy, type = ptype, col = col, pch = pch, 
            main = main, xlab = xlab, ylab = ylab, xlim = xlim, 
            ylim = ylim, log = log, ...)
        if (do.lab) 
            text(xy, col = col, labels = if (!is.logical(xy.labels)) 
                xy.labels
            else index2char(index(xyzoo)), ...)
        if (xy.lines) 
            lines(xy, col = col, lty = lty, lwd = lwd, type = if (do.lab) 
                "c"
            else "l", ...)
        return(invisible(xyzoo))
    }
    recycle <- function(a, len, nser) rep(lapply(as.list(a), 
        rep, length.out = len), length.out = nser)
    range2 <- function(x, ...) if (length(x) == 2) 
        x
    else range(x, ...)
    if (missing(plot.type)) {
        plot.type <- if (missing(screens)) 
            "multiple"
        else if (length(unique(screens) == 1)) 
            "single"
        else "multiple"
    }
    plot.type <- match.arg(plot.type, c("multiple", "single"))
    nser <- NCOL(x)
    if (missing(screens)) {
        screens <- if (plot.type == "single") 
            1
        else seq_len(nser)
    }
    dots <- list(...)
    x.index <- index(x)
    if (is.ts(x.index)) 
        x.index <- as.vector(x.index)
    cn <- if (is.null(colnames(x))) 
        paste("V", seq_len(nser), sep = "")
    else colnames(x)
    screens <- make.par.list(cn, screens, NROW(x), nser, 1)
    screens <- as.factor(unlist(screens))[drop = TRUE]
    ngraph <- length(levels(screens))
    if (nser > 1 && (plot.type == "multiple" || ngraph > 1)) {
        if (ngraph == 1) {
            screens <- as.factor(seq(nser))
            ngraph <- nser
        }
        if (is.null(main)) 
            main <- deparse(substitute(x))
        main.outer <- TRUE
        if (is.null(ylab)) 
            ylab <- colnames(x)[!duplicated(screens)]
        if (is.null(ylab)) 
            ylab <- paste("Series", which(!duplicated(screens)))
        if (is.call(ylab)) 
            ylab <- as.expression(ylab)
        ylab <- rep(ylab, length.out = ngraph)
        if (!is.list(ylab)) 
            ylab <- as.list(ylab)
        lty <- rep(lty, length.out = nser)
        lwd <- rep(lwd, length.out = nser)
        col <- make.par.list(cn, col, NROW(x), nser, 1)
        pch <- make.par.list(cn, pch, NROW(x), nser, par("pch"))
        type <- make.par.list(cn, type, NROW(x), nser, "l")
        if (!is.null(ylim)) {
            if (is.list(ylim)) 
                ylim <- lapply(ylim, range2, na.rm = TRUE)
            else ylim <- list(range2(ylim, na.rm = TRUE))
            ylim <- lapply(make.par.list(cn, ylim, 2, nser, NULL), 
                function(x) if (is.null(x) || length(na.omit(x)) == 
                  0) 
                  NULL
                else range2(x, na.rm = TRUE))
        }
        panel <- match.fun(panel)
        if (missing(nc)) 
            nc <- if (ngraph > 4) 
                2
            else 1
        oldpar <- par(no.readonly = TRUE)
        on.exit({
            par(oldpar)
        })
        nr <- ceiling(ngraph/nc)
        layout(matrix(seq(nr * nc), nr), widths = widths, heights = heights)
        par(mar = mar, oma = oma)
        allsame <- function(L) {
            f <- function(x, y) if (identical(x, y)) 
                x
            !is.null(Reduce(f, L))
        }
        f <- function(idx) if (allsame(ylim)) 
            ylim[idx][[1]]
        else if (!is.null(ylim) && length(idx) > 0 && length(unlist(ylim[idx])) > 
            0) 
            range(ylim[idx], finite = TRUE)
        else range(x[, idx], na.rm = TRUE)
        ranges <- tapply(1:ncol(x), screens, f)
        for (j in seq_along(levels(screens))) {
            panel.number <- j
            y.side <- if (j%%2 || !yax.flip) 
                2
            else 4
            range. <- rep(ranges[[j]], length.out = length(time(x)))
            if (j%%nr == 0 || j == length(levels(screens))) {
                args <- list(x.index, range., xlab = "", ylab = "", 
                  yaxt = "n", xlim = xlim, ylim = ylim[[j]], 
                  log = log, ...)
                args$type <- "n"
                do.call("plot", args)
                mtext(xlab, side = 1, line = 3)
            }
            else {
                args <- list(x.index, range., xaxt = "n", yaxt = "n", 
                  xlab = "", ylab = "", xlim = xlim, ylim = ylim[[j]], 
                  log = log, ...)
                args$type <- "n"
                do.call("plot", args)
                if ("bty" %in% names(args) && args$bty == "n") {
                }
                else box()
            }
            do.call("axis", c(list(side = y.side, xpd = NA), 
                dots))
            mtext(ylab[[j]], y.side, line = 3)
            for (i in which(screens == levels(screens)[j])) {
                series.number <- i
                series.within.screen <- ave(seq_along(screens), 
                  screens, FUN = seq_along)[series.number]
                panel(x.index, x[, i], col = col[[i]], pch = pch[[i]], 
                  lty = lty[i], lwd = lwd[i], type = type[[i]], 
                  ...)
            }
        }
    }
    else {
        if (is.null(ylab)) 
            ylab <- deparse(substitute(x))
        if (is.call(ylab)) 
            ylab <- as.expression(ylab)
        if (is.null(main)) 
            main <- ""
        main.outer <- FALSE
        if (is.null(ylim)) 
            ylim <- range(x, na.rm = TRUE)
        else ylim <- range2(c(ylim, recursive = TRUE), na.rm = TRUE)
        lty <- rep(lty, length.out = nser)
        lwd <- rep(lwd, length.out = nser)
        col <- make.par.list(cn, col, NROW(x), nser, 1)
        pch <- make.par.list(cn, pch, NROW(x), nser, par("pch"))
        type <- make.par.list(cn, type, NROW(x), nser, "l")
        dummy <- rep(range(x, na.rm = TRUE), length.out = length(index(x)))
        args <- list(x.index, dummy, xlab = xlab, ylab = ylab[1], 
            ylim = ylim, xlim = xlim, log = log, ...)
        args$type <- "n"
        do.call("plot", args)
        if ("bty" %in% names(args) && args$bty == "n") {
        }
        else box()
        y <- as.matrix(x)
        for (i in 1:nser) {
            panel(x.index, y[, i], col = col[[i]], pch = pch[[i]], 
                lty = lty[i], lwd = lwd[i], type = type[[i]], 
                ...)
        }
    }
    dots <- list(...)
    title.args <- c(list(main = main, outer = main.outer), dots[grep("[.]main$", 
        names(dots))])
    do.call("title", title.args)
    return(invisible(x))
}


is.regular <- function (x, strict = FALSE) 
{
    UseMethod("is.regular")
}


yearqtr <- function (x) 
structure(floor(4 * x + 0.001)/4, class = "yearqtr")


`coredata<-` <- function (x, value) 
{
    UseMethod("coredata<-")
}


rollsumr <- function (..., align = "right") 
{
    rollsum(..., align = align)
}


panel.segments.tis <- function (x0, x1, ...) 
{
    x0 <- as.zoo(x0)
    x1 <- as.zoo(x1)
    panel.segments(time(x0), coredata(x0), time(x1), coredata(x1), 
        ...)
}


index2char <- function (x, ...) 
UseMethod("index2char")


read.delim.zoo <- function (file, format = "", tz = "", FUN = NULL, regular = FALSE, 
    index.column = 1, drop = TRUE, FUN2 = NULL, split = NULL, 
    aggregate = FALSE, ...) 
{
    file <- read.delim(file, ...)
    read.zoo(file, format = format, tz = tz, FUN = FUN, regular = regular, 
        index.column = index.column, drop = drop, FUN2 = FUN2, 
        split = split, aggregate = aggregate)
}


index <- function (x, ...) 
{
    UseMethod("index")
}


rev.zoo <- function (x) 
{
    ix <- rev(ORDER(time(x)))
    zoo(coredata(x), time(x)[ix])
}


rollmean <- function (x, k, fill = if (na.pad) NA, na.pad = FALSE, align = c("center", 
    "left", "right"), ...) 
{
    UseMethod("rollmean")
}


na.spline <- function (object, ...) 
UseMethod("na.spline")


as.yearmon.default <- function (x, ...) 
as.yearmon(as.numeric(x))


as.yearqtr <- function (x, ...) 
UseMethod("as.yearqtr")


`time<-` <- function (x, value) 
{
    UseMethod("time<-")
}


rollmedianr <- function (..., align = "right") 
{
    rollmedian(..., align = align)
}


panel.polygon.tis <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.polygon(time(x), coredata(x), ...)
}


fortify.zoo <- function (model, data, melt = FALSE, ...) 
{
    n <- NROW(model)
    k <- NCOL(model)
    lab <- colnames(model)
    if (is.null(lab)) 
        lab <- rep.int(deparse(substitute(model)), k)
    lab <- make.unique(lab)
    if (melt) {
        df <- if (k == 1L) {
            data.frame(index(model), factor(rep.int(1, n), labels = lab), 
                coredata(model))
        }
        else {
            data.frame(index(model)[rep.int(1:n, k)], factor(rep(1:k, 
                each = n), levels = 1:k, labels = lab), as.vector(coredata(model)))
        }
        names(df) <- c("Index", "Series", "Value")
    }
    else {
        df <- cbind(data.frame(index(model)), coredata(model))
        names(df) <- c("Index", lab)
    }
    return(df)
}


rollmax.default <- function (x, k, fill = if (na.pad) NA, na.pad = FALSE, align = c("center", 
    "left", "right"), ...) 
{
    coredata(rollmax(zoo(x), k, fill = fill, align = align, ...))
}


panel.points.zoo <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.points(time(x), coredata(x), ...)
    .Deprecated("panel.points")
}


scale_y_yearqtr <- function (..., format = "%Y-%q", n = 5) 
{
    ggplot2::scale_y_continuous(..., trans = yearqtr_trans(format, 
        n))
}


rollmeanr <- function (..., align = "right") 
{
    rollmean(..., align = align)
}


read.table.zoo <- function (file, format = "", tz = "", FUN = NULL, regular = FALSE, 
    index.column = 1, drop = TRUE, FUN2 = NULL, split = NULL, 
    aggregate = FALSE, ...) 
{
    file <- read.table(file, ...)
    read.zoo(file, format = format, tz = tz, FUN = FUN, regular = regular, 
        index.column = index.column, drop = drop, FUN2 = FUN2, 
        split = split, aggregate = aggregate)
}


panel.polygon.zoo <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.polygon(time(x), coredata(x), ...)
}


na.fill.default <- function (object, fill, ix, ...) 
{
    coredata(na.fill(zoo(object), fill, ix, ...))
}


`index<-` <- function (x, value) 
{
    UseMethod("index<-")
}


scale_x_yearmon <- function (..., format = "%b %Y", n = 5) 
{
    ggplot2::scale_x_continuous(..., trans = yearmon_trans(format, 
        n))
}


na.aggregate <- function (object, ...) 
UseMethod("na.aggregate")


Sys.yearqtr <- function () 
as.yearqtr(Sys.Date())


panel.rect.ts <- function (x0, x1, ...) 
{
    x0 <- as.zoo(x0)
    x1 <- as.zoo(x1)
    panel.rect(time(x0), coredata(x0), time(x1), coredata(x1), 
        ...)
}


rollmedian <- function (x, k, fill = if (na.pad) NA, na.pad = FALSE, align = c("center", 
    "left", "right"), ...) 
{
    UseMethod("rollmedian")
}


MATCH.times <- function (x, table, nomatch = NA, units = "sec", eps = 1e-10, 
    ...) 
{
    match(trunc(x, units, eps), trunc(table, units, eps), nomatch = nomatch, 
        ...)
}


as.Date <- function (x, ...) 
{
    if (!is.object(x) && is.numeric(x)) 
        as.Date.numeric(x, ...)
    else UseMethod("as.Date")
}


xblocks.default <- function (x, y, ..., col = NULL, border = NA, ybottom = par("usr")[3], 
    ytop = ybottom + height, height = diff(par("usr")[3:4]), 
    last.step = median(diff(tail(x)))) 
{
    if (is.function(y)) 
        y <- y(x)
    x <- as.numeric(x)
    if (length(x) == 0) 
        return()
    if (is.unsorted(x, na.rm = TRUE)) 
        stop("'x' should be ordered (increasing)")
    if (is.na(last.step)) 
        last.step <- 0
    if (is.logical(y)) {
        y <- y
    }
    else if (is.numeric(y)) {
        y <- !is.na(y)
    }
    else {
        y <- as.character(y)
    }
    NAval <- if (is.character(y)) 
        ""
    else FALSE
    y[is.na(y)] <- NAval
    yrle <- rle(y)
    blockCol <- yrle$values
    blockCol[blockCol == NAval] <- NA
    if (is.logical(y) && is.null(col)) 
        col <- palette()[1]
    if (length(col) > 0) {
        if (is.character(col)) 
            col[col == ""] <- NA
        ok <- !is.na(blockCol)
        blockCol[ok] <- rep(col, length.out = sum(ok))
    }
    idxBounds <- cumsum(c(1, yrle$lengths))
    idxStart <- head(idxBounds, -1)
    idxEnd <- tail(idxBounds, -1)
    idxEnd[length(idxEnd)] <- length(y)
    blockStart <- x[idxStart]
    blockEnd <- x[idxEnd]
    blockEnd[length(blockEnd)] <- tail(blockEnd, 1) + last.step
    blockWidth <- blockEnd - blockStart
    if (par("ylog")) {
        ybottom <- 10^ybottom
        ytop <- 10^ytop
    }
    rect(xleft = blockStart, xright = blockEnd, ybottom = ybottom, 
        ytop = ytop, col = blockCol, border = border, ...)
}


xtfrm.zoo <- function (x) 
coredata(x)


panel.lines.ts <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.lines(time(x), coredata(x), ...)
    .Deprecated("panel.lines")
}


ORDER <- function (x, ...) 
UseMethod("ORDER")


read.csv2.zoo <- function (file, format = "", tz = "", FUN = NULL, regular = FALSE, 
    index.column = 1, drop = TRUE, FUN2 = NULL, split = NULL, 
    aggregate = FALSE, ...) 
{
    file <- read.csv2(file, ...)
    read.zoo(file, format = format, tz = tz, FUN = FUN, regular = regular, 
        index.column = index.column, drop = drop, FUN2 = FUN2, 
        split = split, aggregate = aggregate)
}


as.Date.yearmon <- function (x, frac = 0, ...) 
{
    x <- unclass(x)
    if (all(is.na(x))) 
        return(as.Date(x))
    year <- floor(x + 0.001)
    ix <- !is.na(year)
    month <- floor(12 * (x - year) + 1 + 0.5 + 0.001)
    dd.start <- as.Date(rep(NA, length(year)))
    dd.start[ix] <- as.Date(paste(year[ix], month[ix], 1, sep = "-"))
    dd.end <- dd.start + 32 - as.numeric(format(dd.start + 32, 
        "%d"))
    as.Date((1 - frac) * as.numeric(dd.start) + frac * as.numeric(dd.end), 
        origin = "1970-01-01")
}


`frequency<-` <- function (x, value) 
UseMethod("frequency<-")


panel.points.its <- function (x, ...) 
{
    x <- as.zoo(x)
    panel.points(time(x), coredata(x), ...)
    .Deprecated("panel.points")
}


as.zooreg <- function (x, frequency = NULL, ...) 
{
    UseMethod("as.zooreg")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "S3 Infrastructure for Regular and Irregular Time Series (Z'sOrdered Observations)"

.skeleton_package_version = "1.7-14"

.skeleton_package_depends = "stats"

.skeleton_package_imports = "utils,graphics,grDevices,lattice"


## Internal

.skeleton_version = 5


## EOF