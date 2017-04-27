##
## Exported symobls in package `quantmod`
##

## Exported package methods

getOptionChain <- function (Symbols, Exp = NULL, src = "yahoo", ...) 
{
    Call <- paste("getOptionChain", src, sep = ".")
    if (missing(Exp)) {
        do.call(Call, list(Symbols = Symbols, ...))
    }
    else {
        do.call(Call, list(Symbols = Symbols, Exp = Exp, ...))
    }
}


unsetDefaults <- function (name, confirm = TRUE) 
{
    importDefaults(calling.fun = "unsetDefaults")
    if (is.function(name)) 
        name <- deparse(substitute(name))
    if (is.null(getDefaults(name))) 
        invisible(return())
    remove.yes <- TRUE
    if (confirm) {
        CONFIRMATION <- readline(prompt = paste("Are you sure you want to remove", 
            sQuote(name), "defaults? (N): "))
        if (toupper(substr(CONFIRMATION, 1, 1)) != "Y") {
            remove.yes <- FALSE
            cat(paste(sQuote(name), "Defaults NOT removed\n"))
        }
        else {
            if (confirm) 
                cat(paste(sQuote(name), "Defaults removed!\n"))
        }
    }
    if (remove.yes) {
        default.name <- paste(name, "Default", sep = ".")
        env <- as.environment(-1)
        eval(parse(text = paste("options(", default.name, "=NULL)", 
            sep = "")), envir = env)
    }
}


`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

allReturns <- function (x, subset = NULL, type = "arithmetic", leading = TRUE) 
{
    x.orig <- x
    x <- try.xts(x)
    all.ret <- cbind(periodReturn(x, "daily", type = type, leading = FALSE), 
        periodReturn(x, "weekly", type = type), periodReturn(x, 
            "monthly", type = type, indexAt = "endof"), periodReturn(x, 
            "quarterly", type = type, indexAt = "endof"), periodReturn(x, 
            "yearly", type = type))
    colnames(all.ret) <- c("daily", "weekly", "monthly", "quarterly", 
        "yearly")
    reclass(all.ret, x.orig)
}


attachSymbols <- function (DB = DDB_Yahoo(), pos = 2, prefix = NULL, postfix = NULL, 
    mem.cache = TRUE, file.cache = !mem.cache, cache.dir = tempdir()) 
{
    if (!inherits(DB, "DDB")) 
        stop("DB must be of class 'DDB'")
    do.call(paste("attachSymbols", DB$src, sep = "."), list(DB = DB, 
        pos = pos, prefix = prefix, postfix = postfix, mem.cache = mem.cache, 
        file.cache = file.cache, cache.dir = cache.dir))
}


weeklyReturn <- function (x, subset = NULL, type = "arithmetic", leading = TRUE, 
    ...) 
{
    periodReturn(x, "weekly", subset, type, leading, ...)
}


LoCl <- function (x) 
{
    xx <- Delt(Lo(x), Cl(x))
    colnames(xx) <- paste("LoCl", deparse(substitute(x)), sep = ".")
    xx
}


saveChart <- function (.type = "pdf", ..., dev = dev.cur()) 
{
    dev <- as.numeric(dev)
    gchob <- get.chob()[[dev]]
    dim.inches <- par("din")
    resolution <- 1
    if (.type %in% c("png", "jpeg")) 
        resolution <- 72
    width <- dim.inches[1] * resolution
    height <- dim.inches[2] * resolution
    export.pars <- c(list(...), list(file = paste(gchob@name, 
        .type, sep = "."), width = width, height = height))[unique(names(c(list(file = 1, 
        width = width, height = height), list(...))))]
    do.call(.type, export.pars)
    chartSeries.chob(gchob)
    invisible(dev.off())
    release.chob(length(get.chob()))
    message(paste("chart saved to", export.pars$file))
}


getSymbols.yahooj <- function (Symbols, env = parent.frame(), return.class = "xts", 
    index.class = "Date", from = "2007-01-01", to = Sys.Date(), 
    ...) 
{
    importDefaults("getSymbols.yahooj")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg(adjust)) 
        adjust <- FALSE
    default.return.class <- return.class
    default.from <- from
    default.to <- to
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    if (!requireNamespace("XML", quietly = TRUE)) 
        stop("package:", dQuote("XML"), "cannot be loaded.")
    yahoo.URL <- "http://info.finance.yahoo.co.jp/history/"
    for (i in 1:length(Symbols)) {
        symname <- toupper(Symbols[[i]])
        symbol <- symname
        if (grepl("^YJ", symname)) {
            return.class <- getSymbolLookup()[[symname]]$return.class
            return.class <- ifelse(is.null(return.class), default.return.class, 
                return.class)
            from <- getSymbolLookup()[[symname]]$from
            from <- if (is.null(from)) 
                default.from
            else from
            to <- getSymbolLookup()[[symname]]$to
            to <- if (is.null(to)) 
                default.to
            else to
            symbol <- substring(symname, 3)
        }
        else {
            return.class <- default.return.class
            from <- default.from
            to <- default.to
            symname <- paste("YJ", symbol, sep = "")
        }
        from.y <- as.numeric(strsplit(as.character(as.Date(from, 
            origin = "1970-01-01")), "-", )[[1]][1])
        from.m <- as.numeric(strsplit(as.character(as.Date(from, 
            origin = "1970-01-01")), "-", )[[1]][2])
        from.d <- as.numeric(strsplit(as.character(as.Date(from, 
            origin = "1970-01-01")), "-", )[[1]][3])
        to.y <- as.numeric(strsplit(as.character(as.Date(to, 
            origin = "1970-01-01")), "-", )[[1]][1])
        to.m <- as.numeric(strsplit(as.character(as.Date(to, 
            origin = "1970-01-01")), "-", )[[1]][2])
        to.d <- as.numeric(strsplit(as.character(as.Date(to, 
            origin = "1970-01-01")), "-", )[[1]][3])
        Symbols.name <- getSymbolLookup()[[symname]]$name
        Symbols.name <- ifelse(is.null(Symbols.name), symbol, 
            Symbols.name)
        if (verbose) 
            cat("downloading ", Symbols.name, ".....\n\n")
        page <- 1
        totalrows <- c()
        tmp <- tempfile()
        on.exit(unlink(tmp))
        while (TRUE) {
            download.file(paste(yahoo.URL, "?code=", Symbols.name, 
                "&sm=", from.m, "&sd=", sprintf("%.2d", from.d), 
                "&sy=", from.y, "&em=", to.m, "&ed=", sprintf("%.2d", 
                  to.d), "&ey=", to.y, "&tm=d", "&p=", page, 
                sep = ""), destfile = tmp, quiet = !verbose)
            fdoc <- XML::htmlParse(tmp)
            rows <- XML::xpathApply(fdoc, "//table[@class='boardFin yjSt marB6']//tr")
            if (length(rows) == 1) 
                break
            totalrows <- c(totalrows, rows)
            page <- page + 1
        }
        if (verbose) 
            cat("done.\n")
        cols <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
        firstrow <- totalrows[[1]]
        cells <- XML::getNodeSet(firstrow, "th")
        if (length(cells) == 5) 
            cols <- cols[-(5:6)]
        totalrows <- rev(totalrows)
        mat <- matrix(0, ncol = length(cols) + 1, nrow = 0, byrow = TRUE)
        for (row in totalrows) {
            cells <- XML::getNodeSet(row, "td")
            if (length(cells) == 2 & length(cols) == 6 & nrow(mat) > 
                1) {
                ss.data <- as.numeric(na.omit(as.numeric(unlist(strsplit(XML::xmlValue(cells[[2]]), 
                  "[^0-9]+")))))
                factor <- ss.data[2]/ss.data[1]
                mat <- rbind(t(apply(mat[-nrow(mat), ], 1, function(x) {
                  x * c(1, rep(1/factor, 4), factor, 1)
                })), mat[nrow(mat), ])
            }
            if (length(cells) != length(cols) + 1) 
                next
            date <- as.Date(XML::xmlValue(cells[[1]]), format = "%Y年%m月%d日")
            entry <- c(date)
            for (n in 2:length(cells)) {
                entry <- cbind(entry, as.numeric(gsub(",", "", 
                  XML::xmlValue(cells[[n]]))))
            }
            mat <- rbind(mat, entry)
        }
        fr <- xts(mat[, -1], as.Date(mat[, 1]), src = "yahooj", 
            updated = Sys.time())
        colnames(fr) <- paste(symname, cols, sep = ".")
        fr <- convert.time.series(fr = fr, return.class = return.class)
        if (is.xts(fr)) 
            indexClass(fr) <- index.class
        Symbols[[i]] <- symname
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
        if (i >= 5 && length(Symbols) > 5) {
            message("pausing 1 second between requests for more than 5 symbols")
            Sys.sleep(1)
        }
    }
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


addBBands <- function (n = 20, sd = 2, maType = "SMA", draw = "bands", on = -1) 
{
    draw.options <- c("bands", "percent", "width")
    draw <- draw.options[pmatch(draw, draw.options)]
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    if (draw == "bands") {
        chobTA@new <- FALSE
    }
    else {
        chobTA@new <- TRUE
        on <- NULL
    }
    xx <- if (is.OHLC(x)) {
        cbind(Hi(x), Lo(x), Cl(x))
    }
    else x
    bb <- BBands(xx, n = n, maType = maType, sd = sd)
    chobTA@TA.values <- bb[lchob@xsubset, ]
    chobTA@name <- "chartBBands"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, ma = maType, sd = sd, draw = draw)
    return(chobTA)
}


add_EMA <- function (n = 10, on = 1, col = "blue", ...) 
{
    lenv <- new.env()
    lenv$add_ema <- function(x, n, col, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        ema <- EMA(Cl(xdata), n = n)[xsubset]
        lines(1:NROW(xdata[xsubset]), ema, col = col, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, ...)), list(n = n, col = col, ...))
    exp <- parse(text = gsub("list", "add_ema", as.expression(substitute(list(x = current.chob(), 
        n = n, col = col, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- EMA(Cl(plot_object$Env$xdata), n = n)
    plot_object$set_frame(sign(on) * (abs(on) + 1L))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


add_SMA <- function (n = 10, on = 1, col = "brown", ...) 
{
    lenv <- new.env()
    lenv$add_sma <- function(x, n, col, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        ema <- SMA(Cl(xdata), n = n)[xsubset]
        lines(1:NROW(xdata[xsubset]), ema, col = col, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, ...)), list(n = n, col = col, ...))
    exp <- parse(text = gsub("list", "add_sma", as.expression(substitute(list(x = current.chob(), 
        n = n, col = col, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- SMA(Cl(plot_object$Env$xdata), n = n)
    plot_object$set_frame(sign(on) * (abs(on) + 1L))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


OpOp <- function (x) 
{
    xx <- Delt(Op(x))
    colnames(xx) <- paste("OpOp", deparse(substitute(x)), sep = ".")
    xx
}


chartShading <- function (x) 
{
    spacing <- x@params$spacing
    width <- x@params$width
    x.range <- x@params$xrange
    x.range <- seq(x.range[1], x.range[2] * spacing)
    y.range <- x@params$yrange
    xstart <- x@params$xstart
    xend <- x@params$xend
    rect(((xstart - 1) * spacing + 1) - width/2, rep(y.range[1] * 
        0.95, length(xstart)), ((xend - 1) * spacing + 1) + width/2, 
        rep(y.range[2] * 1.05, length(xend)), col = c(x@params$colors$BBands$fill), 
        border = NA)
}


setSymbolLookup <- function (...) 
{
    new.symbols <- list(...)
    if (length(new.symbols) == 1 && is.null(names(new.symbols)) && 
        is.list(new.symbols[[1]])) 
        new.symbols <- new.symbols[[1]]
    all.symbols <- getOption("getSymbols.sources")
    for (each.symbol in names(new.symbols)) {
        if (length(new.symbols[[each.symbol]]) == 1 & !is.list(new.symbols[[each.symbol]])) {
            all.symbols[[each.symbol]] <- list(src = new.symbols[[each.symbol]])
        }
        else {
            all.symbols[[each.symbol]] <- new.symbols[[each.symbol]]
        }
    }
    options(getSymbols.sources = all.symbols)
}


loadSymbols <- function (Symbols = NULL, env = parent.frame(), reload.Symbols = FALSE, 
    verbose = FALSE, warnings = TRUE, src = "yahoo", symbol.lookup = TRUE, 
    auto.assign = getOption("loadSymbols.auto.assign", TRUE), 
    ...) 
{
    if (getOption("getSymbols.warning4.0", TRUE)) {
        message(paste("    As of 0.4-0,", sQuote("getSymbols"), 
            "uses env=parent.frame() and\n", "auto.assign=TRUE by default.\n\n", 
            "This  behavior  will be  phased out in 0.5-0  when the call  will\n", 
            "default to use auto.assign=FALSE. getOption(\"getSymbols.env\") and \n", 
            "getOptions(\"getSymbols.auto.assign\") are now checked for alternate defaults\n\n", 
            "This message is shown once per session and may be disabled by setting \n", 
            "options(\"getSymbols.warning4.0\"=FALSE). See ?getSymbols for more details."))
        options(getSymbols.warning4.0 = FALSE)
    }
    importDefaults("getSymbols")
    if (missing(env) && !is.null(getOption("getSymbols.env"))) 
        env <- getOption("getSymbols.env")
    if (is.null(env)) 
        auto.assign <- FALSE
    if (!auto.assign && length(Symbols) > 1) 
        stop("must use auto.assign=TRUE for multiple Symbols requests")
    force(Symbols)
    if (symbol.lookup && missing(src)) {
        symbols.src <- getOption("getSymbols.sources")
    }
    else {
        symbols.src <- src[1]
    }
    if (is.character(Symbols)) {
        Symbols <- unlist(strsplit(Symbols, ";"))
        tmp.Symbols <- vector("list")
        for (each.symbol in Symbols) {
            if (each.symbol %in% names(symbols.src)) {
                tmp.src <- symbols.src[[each.symbol]]$src[1]
                if (is.null(tmp.src)) {
                  tmp.Symbols[[each.symbol]] <- src[1]
                }
                else {
                  tmp.Symbols[[each.symbol]] <- tmp.src
                }
            }
            else {
                tmp.Symbols[[each.symbol]] <- src[1]
            }
        }
        Symbols <- tmp.Symbols
    }
    old.Symbols <- NULL
    if (auto.assign && exists(".getSymbols", env, inherits = FALSE)) {
        old.Symbols <- get(".getSymbols", env)
    }
    if (reload.Symbols) {
        Symbols <- c(Symbols, old.Symbols)[unique(names(c(Symbols, 
            old.Symbols)))]
    }
    if (!auto.assign && length(Symbols) > 1) 
        stop("must use auto.assign=TRUE when reloading multiple Symbols")
    if (!is.null(Symbols)) {
        Symbols <- as.list(unlist(lapply(unique(as.character(Symbols)), 
            FUN = function(x) {
                Symbols[Symbols == x]
            })))
        all.symbols <- list()
        for (symbol.source in unique(as.character(Symbols))) {
            current.symbols <- names(Symbols[Symbols == symbol.source])
            symbols.returned <- do.call(paste("getSymbols.", 
                symbol.source, sep = ""), list(Symbols = current.symbols, 
                env = env, verbose = verbose, warnings = warnings, 
                auto.assign = auto.assign, ...))
            if (!auto.assign) 
                return(symbols.returned)
            for (each.symbol in symbols.returned) all.symbols[[each.symbol]] <- symbol.source
        }
        req.symbols <- names(all.symbols)
        all.symbols <- c(all.symbols, old.Symbols)[unique(names(c(all.symbols, 
            old.Symbols)))]
        if (auto.assign) {
            assign(".getSymbols", all.symbols, env)
            return(req.symbols)
        }
    }
    else {
        warning("no Symbols specified")
    }
}


listTA <- function (dev) 
{
    if (missing(dev)) 
        dev <- dev.cur()
    sapply(get.chob()[[dev]]@passed.args$TA, function(x) x@call)
}


OpCl <- function (x) 
{
    xx <- Delt(Op(x), Cl(x))
    colnames(xx) <- paste("OpCl", deparse(substitute(x)), sep = ".")
    xx
}


addROC <- function (n = 1, type = c("discrete", "continuous"), col = "red") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        Cl(x)
    }
    else x
    type <- match.arg(type)
    roc <- ROC(xx, n = n, type = type, na.pad = TRUE)
    chobTA@TA.values <- roc[lchob@xsubset]
    chobTA@name <- "chartROC"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        multi.col = lchob@multi.col, spacing = lchob@spacing, 
        width = lchob@width, bp = lchob@bp, x.labels = lchob@x.labels, 
        time.scale = lchob@time.scale, n = n, type = type, col = col)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


HLC <- xts::HLC # re-exported from xts package

has.Lo <- xts::has.Lo # re-exported from xts package

`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

add_SMI <- function (n = 13, nFast = 25, nSlow = 2, nSig = 9, maType = "EMA", 
    bounded = TRUE, ...) 
{
    lenv <- new.env()
    lenv$plot_smi <- function(x, n, nFast, nSlow, nSig, maType, 
        bounded, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        smi <- SMI(HLC(xdata), n = n, nFast = nFast, nSlow = nSlow, 
            nSig = nSig, maType = maType, bounded = bounded)
        x.pos <- 1:NROW(xdata[xsubset])
        segments(axTicksByTime2(xdata[xsubset]), range(na.omit(smi))[1], 
            axTicksByTime2(xdata[xsubset]), range(na.omit(smi))[2], 
            col = x$Env$theme$grid)
        lines(x.pos, smi[xsubset, 1], col = x$Env$theme$smi$col$smi, 
            lwd = 2, ...)
        lines(x.pos, smi[xsubset, 2], col = x$Env$theme$smi$col$signal, 
            ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, nFast = nFast, nSlow = nSlow, nSig = nSig, 
        maType = maType, bounded = bounded, ...)), list(n = n, 
        nFast = nFast, nSlow = nSlow, nSig = nSig, maType = maType, 
        bounded = bounded, ...))
    exp <- parse(text = gsub("list", "plot_smi", as.expression(substitute(list(x = current.chob(), 
        n = n, nFast = nFast, nSlow = nSlow, nSig = nSig, maType = maType, 
        bounded = bounded, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    if (is.null(plot_object$Env$theme$smi)) {
        plot_object$Env$theme$smi$col$smi <- "orange"
        plot_object$Env$theme$smi$col$signal <- "darkgrey"
    }
    xsubset <- plot_object$Env$xsubset
    smi <- SMI(HLC(plot_object$Env$xdata), n = n, nFast = nFast, 
        nSlow = nSlow, nSig = nSig, maType = maType, bounded = bounded)
    plot_object$add_frame(ylim = c(0, 1), asp = 0.2)
    plot_object$next_frame()
    lenv$xdata <- structure(smi, .Dimnames = list(NULL, c("smi", 
        "signal")))
    text.exp <- expression(text(c(1, 1 + strwidth(paste("SMI(", 
        paste(n, nFast, nSlow, nSig, sep = ","), "):", sep = "")), 
        1 + strwidth(paste("SMI(", paste(n, nFast, nSlow, nSig, 
            sep = ","), "):", sep = "")) + strwidth("-22.22222")), 
        0.3, c(paste("SMI(", paste(n, nFast, nSlow, nSig, sep = ","), 
            "):", sep = ""), round(last(xdata[xsubset, 1]), 5), 
            round(last(xdata[xsubset, 2]), 5)), col = c(1, theme$smi$col$smi, 
            theme$smi$col$signal), adj = c(0, 0), cex = 0.9, 
        offset = 0, pos = 4))
    plot_object$add(text.exp, env = c(lenv, plot_object$Env), 
        expr = TRUE)
    plot_object$add_frame(ylim = range(na.omit(smi)), fixed = TRUE, 
        asp = 1)
    plot_object$next_frame()
    lenv$grid_lines <- function(xdata, x) {
        seq(-50, 50, 50)
    }
    exp <- c(expression(abline(h = grid_lines(xdata, xsubset), 
        col = theme$grid)), exp, expression(text(1 - 1/3 - max(strwidth(grid_lines(xdata, 
        xsubset))), grid_lines(xdata, xsubset), noquote(format(grid_lines(xdata, 
        xsubset), justify = "right")), col = theme$labels, offset = 0, 
        pos = 4, cex = 0.9)), expression(text(NROW(xdata[xsubset]) + 
        1/3, grid_lines(xdata, xsubset), noquote(format(grid_lines(xdata, 
        xsubset), justify = "right")), col = theme$labels, offset = 0, 
        pos = 4, cex = 0.9)))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


axTicksByTime2 <- function (x, ticks.on = "auto", k = 1, labels = TRUE, format.labels = TRUE, 
    ends = TRUE, gt = 2, lt = 25) 
{
    if (timeBased(x)) 
        x <- xts(rep(1, length(x)), x)
    tick.opts <- c("years", "months", "weeks", "days")
    tick.k.opts <- c(1, 1, 1, 1)
    if (ticks.on %in% tick.opts) {
        cl <- ticks.on[1]
        ck <- k
    }
    else {
        tick.opts <- paste(tick.opts, tick.k.opts)
        is <- structure(rep(0, length(tick.opts)), .Names = tick.opts)
        for (i in 1:length(tick.opts)) {
            y <- strsplit(tick.opts[i], " ")[[1]]
            ep <- endpoints(x, y[1], as.numeric(y[2]))
            if (i > 1 && is[i - 1] == length(ep) - 1) 
                break
            is[i] <- length(ep) - 1
            if (is[i] > lt) 
                break
        }
        nms <- rev(names(is)[which(is > gt & is < lt)])[1]
        cl <- strsplit(nms, " ")[[1]][1]
        ck <- as.numeric(strsplit(nms, " ")[[1]][2])
    }
    if (is.na(cl) || is.na(ck) || is.null(cl)) {
        return(c(1, NROW(x)))
    }
    else ep <- endpoints(x, cl, ck)
    if (ends) 
        ep <- ep + c(rep(1, length(ep) - 1), 0)
    if (labels) {
        if (is.logical(format.labels) || is.character(format.labels)) {
            unix <- ifelse(.Platform$OS.type == "unix", TRUE, 
                FALSE)
            fmt <- switch(cl, years = "%Y", months = "%b", days = "%d", 
                weeks = "W%W", hours = "%H:%M", minutes = "%H:%M:%S", 
                seconds = "%H:%M:%S")
            if (ndays(x) > 1 && cl %in% c("hours", "minutes", 
                "seconds")) {
                fmt <- paste("%b-%d", fmt)
            }
            names(ep) <- format(index(x)[ep], fmt)
        }
        else names(ep) <- as.character(index(x)[ep])
    }
    ep
}


Lag <- function (x, k = 1) 
{
    UseMethod("Lag")
}


addVo <- function (log.scale = FALSE) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    if (!lchob@show.vol || !has.Vo(x)) 
        return(invisible(new("chobTA", new = FALSE, name = "chartNULL", 
            call = match.call())))
    Volumes <- Vo(x)
    max.vol <- max(Volumes, na.rm = TRUE)
    vol.scale <- list(100, "100s")
    if (max.vol > 10000) 
        vol.scale <- list(1000, "1000s")
    if (max.vol > 1e+05) 
        vol.scale <- list(10000, "10,000s")
    if (max.vol > 1e+06) 
        vol.scale <- list(1e+05, "100,000s")
    if (max.vol > 1e+07) 
        vol.scale <- list(1e+06, "millions")
    if (lchob@color.vol & is.OHLC(x)) {
        Opens <- Op(x)
        Closes <- Cl(x)
        if (lchob@multi.col) {
            last.Closes <- as.numeric(Lag(Closes))
            last.Closes[1] <- Closes[1]
            bar.col <- ifelse(Opens < Closes, ifelse(Opens < 
                last.Closes, lchob@colors$dn.up.col, lchob@colors$up.up.col), 
                ifelse(Opens < last.Closes, lchob@colors$dn.dn.col, 
                  lchob@colors$up.dn.col))
        }
        else {
            bar.col <- ifelse(Opens < Closes, lchob@colors$up.col, 
                lchob@colors$dn.col)
        }
    }
    else bar.col <- ifelse(!is.null(lchob@colors$Vo.bar.col), 
        lchob@colors$Vo.bar.col, lchob@colors$border)
    border.col <- ifelse(is.null(lchob@colors$border), bar.col, 
        lchob@colors$border)
    bar.col <- bar.col[lchob@xsubset]
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    chobTA@TA.values <- (Volumes/vol.scale[[1]])[lchob@xsubset]
    chobTA@name <- "chartVo"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        vol.scale = vol.scale, x.labels = lchob@x.labels, log.scale = log.scale, 
        bar.col = bar.col, border.col = border.col, time.scale = lchob@time.scale)
    chobTA@params$thin <- ifelse(lchob@type %in% c("bars", "matchsticks"), 
        TRUE, FALSE)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


chart_Series <- function (x, name = deparse(substitute(x)), type = "candlesticks", 
    subset = "", TA = "", pars = chart_pars(), theme = chart_theme(), 
    clev = 0, ...) 
{
    cs <- new.replot()
    line.col <- theme$col$line.col
    up.col <- theme$col$up.col
    dn.col <- theme$col$dn.col
    up.border <- theme$col$up.border
    dn.border <- theme$col$dn.border
    format.labels <- theme$format.labels
    if (is.null(theme$grid.ticks.on)) {
        xs <- x[subset]
        major.grid <- c(years = nyears(xs), months = nmonths(xs), 
            days = ndays(xs))
        grid.ticks.on <- names(major.grid)[rev(which(major.grid < 
            30))[1]]
    }
    else grid.ticks.on <- theme$grid.ticks.on
    label.bg <- theme$col$label.bg
    cs$subset <- function(x) {
        if (FALSE) {
            set_ylim <- get_ylim <- set_xlim <- Env <- function() {
            }
        }
        if (missing(x)) {
            x <- ""
        }
        Env$xsubset <<- x
        set_xlim(c(1, NROW(Env$xdata[Env$xsubset])))
        ylim <- get_ylim()
        for (y in seq(2, length(ylim), by = 2)) {
            if (!attr(ylim[[y]], "fixed")) 
                ylim[[y]] <- structure(c(Inf, -Inf), fixed = FALSE)
        }
        lapply(Env$actions, function(x) {
            frame <- abs(attr(x, "frame"))
            fixed <- attr(ylim[[frame]], "fixed")
            if (frame%%2 == 0 && !fixed) {
                lenv <- attr(x, "env")
                if (is.list(lenv)) 
                  lenv <- lenv[[1]]
                min.tmp <- min(ylim[[frame]][1], range(na.omit(lenv$xdata[Env$xsubset]))[1], 
                  na.rm = TRUE)
                max.tmp <- max(ylim[[frame]][2], range(na.omit(lenv$xdata[Env$xsubset]))[2], 
                  na.rm = TRUE)
                ylim[[frame]] <<- structure(c(min.tmp, max.tmp), 
                  fixed = fixed)
            }
        })
        set_ylim(ylim)
    }
    environment(cs$subset) <- environment(cs$get_asp)
    if (is.character(x)) 
        stop("'x' must be a time-series object")
    if (is.OHLC(x)) {
        cs$Env$xdata <- OHLC(x)
        if (has.Vo(x)) 
            cs$Env$vo <- Vo(x)
    }
    else cs$Env$xdata <- x
    cs$Env$xsubset <- subset
    cs$Env$cex <- pars$cex
    cs$Env$mar <- pars$mar
    cs$set_asp(3)
    cs$set_xlim(c(1, NROW(cs$Env$xdata[subset])))
    cs$set_ylim(list(structure(range(na.omit(cs$Env$xdata[subset])), 
        fixed = FALSE)))
    cs$set_frame(1, FALSE)
    cs$Env$clev = min(clev + 0.01, 1)
    cs$Env$theme$bbands <- theme$bbands
    cs$Env$theme$shading <- theme$shading
    cs$Env$theme$line.col <- theme$col$line.col
    cs$Env$theme$up.col <- up.col
    cs$Env$theme$dn.col <- dn.col
    cs$Env$theme$up.border <- up.border
    cs$Env$theme$dn.border <- dn.border
    cs$Env$theme$rylab <- theme$rylab
    cs$Env$theme$lylab <- theme$lylab
    cs$Env$theme$bg <- theme$col$bg
    cs$Env$theme$grid <- theme$col$grid
    cs$Env$theme$grid2 <- theme$col$grid2
    cs$Env$theme$labels <- "#333333"
    cs$Env$theme$label.bg <- label.bg
    cs$Env$format.labels <- format.labels
    cs$Env$ticks.on <- grid.ticks.on
    cs$Env$grid.ticks.lwd <- theme$grid.ticks.lwd
    cs$Env$type <- type
    cs$Env$axis_ticks <- function(xdata, xsubset) {
        ticks <- diff(axTicksByTime2(xdata[xsubset], labels = FALSE))/2 + 
            last(axTicksByTime2(xdata[xsubset], labels = TRUE), 
                -1)
        if (!theme$coarse.time || length(ticks) == 1) 
            return(unname(ticks))
        if (min(diff(ticks)) < max(strwidth(names(ticks)))) {
            ticks <- unname(ticks)
        }
        ticks
    }
    cs$add(expression(atbt <- axTicksByTime2(xdata[xsubset]), 
        segments(atbt, get_ylim()[[2]][1], atbt, get_ylim()[[2]][2], 
            col = theme$grid, lwd = grid.ticks.lwd), axt <- axis_ticks(xdata, 
            xsubset), text(as.numeric(axt), par("usr")[3] - 0.2 * 
            min(strheight(axt)), names(axt), xpd = TRUE, cex = 0.9, 
            pos = 3)), clip = FALSE, expr = TRUE)
    cs$set_frame(-1)
    cs$add_frame(0, ylim = c(0, 1), asp = 0.2)
    cs$set_frame(1)
    cs$add(expression(if (NROW(xdata[xsubset]) < 400) {
        axis(1, at = 1:NROW(xdata[xsubset]), labels = FALSE, 
            col = theme$grid2, tcl = 0.3)
    }), expr = TRUE)
    cs$add(expression(axt <- axTicksByTime(xdata[xsubset], format.labels = format.labels), 
        axis(1, at = axt, labels = names(axt), las = 1, lwd.ticks = 1, 
            mgp = c(3, 1.5, 0), tcl = -0.4, cex.axis = 0.9)), 
        expr = TRUE)
    cs$Env$name <- name
    text.exp <- c(expression(text(1 - 1/3, 0.5, name, font = 2, 
        col = "#444444", offset = 0, cex = 1.1, pos = 4)), expression(text(NROW(xdata[xsubset]), 
        0.5, paste(start(xdata[xsubset]), end(xdata[xsubset]), 
            sep = " / "), col = 1, adj = c(0, 0), pos = 2)))
    cs$add(text.exp, env = cs$Env, expr = TRUE)
    cs$set_frame(2)
    cs$Env$axis_labels <- function(xdata, xsubset, scale = 5) {
        axTicksByValue(na.omit(xdata[xsubset]))
    }
    cs$Env$make_pretty_labels <- function(ylim) {
        p <- pretty(ylim, 10)
        p[p > ylim[1] & p < ylim[2]]
    }
    cs$add(expression(assign("alabels", make_pretty_labels(get_ylim(get_frame())[[2]]))), 
        expr = TRUE)
    cs$set_frame(-2)
    cs$add(expression(if (diff(range(xdata[xsubset], na.rm = TRUE)) < 
        50) segments(1, seq(min(xdata[xsubset]%/%1, na.rm = TRUE), 
        max(xdata[xsubset]%/%1, na.rm = TRUE), 1), length(xsubset), 
        seq(min(xdata[xsubset]%/%1, na.rm = TRUE), max(xdata[xsubset]%/%1, 
            na.rm = TRUE), 1), col = theme$grid2, lty = "dotted")), 
        expr = TRUE)
    cs$set_frame(2)
    cs$add(expression(segments(1, alabels, NROW(xdata[xsubset]), 
        alabels, col = theme$grid)), expr = TRUE)
    if (theme$lylab) {
        cs$add(expression(text(1 - 1/3 - max(strwidth(alabels)), 
            alabels, noquote(format(alabels, justify = "right")), 
            col = theme$labels, offset = 0, cex = 0.9, pos = 4, 
            xpd = TRUE)), expr = TRUE)
    }
    if (theme$rylab) {
        cs$add(expression(text(NROW(xdata[xsubset]) + 1/3, alabels, 
            noquote(format(alabels, justify = "right")), col = theme$labels, 
            offset = 0, cex = 0.9, pos = 4, xpd = TRUE)), expr = TRUE)
    }
    cs$set_frame(2)
    cs$add(expression(range.bars(xdata[xsubset], type, 1, fade(theme$line.col, 
        clev), fade(theme$up.col, clev), fade(theme$dn.col, clev), 
        fade(theme$up.border, clev), fade(theme$dn.border, clev))), 
        expr = TRUE)
    assign(".chob", cs, .plotEnv)
    if (!is.null(TA) && nchar(TA) > 0) {
        TA <- parse(text = TA, srcfile = NULL)
        for (ta in 1:length(TA)) {
            if (length(TA[ta][[1]][-1]) > 0) {
                cs <- eval(TA[ta])
            }
            else {
                cs <- eval(TA[ta])
            }
        }
    }
    assign(".chob", cs, .plotEnv)
    cs
}


standardQuote <- function (src = "yahoo") 
{
    do.call(paste("standardQuote", src, sep = "."), list())
}


getDividends <- function (Symbol, from = "1970-01-01", to = Sys.Date(), env = parent.frame(), 
    src = "yahoo", auto.assign = FALSE, auto.update = FALSE, 
    verbose = FALSE, ...) 
{
    if (missing(env)) 
        env <- parent.frame(1)
    if (is.null(env)) 
        auto.assign <- FALSE
    Symbol.name <- ifelse(!is.character(Symbol), deparse(substitute(Symbol)), 
        as.character(Symbol))
    yahoo.URL <- "http://ichart.finance.yahoo.com/table.csv?s="
    from.y <- as.numeric(strsplit(as.character(from), "-", )[[1]][1])
    from.m <- as.numeric(strsplit(as.character(from), "-", )[[1]][2]) - 
        1
    from.d <- as.numeric(strsplit(as.character(from), "-", )[[1]][3])
    to.y <- as.numeric(strsplit(as.character(to), "-", )[[1]][1])
    to.m <- as.numeric(strsplit(as.character(to), "-", )[[1]][2]) - 
        1
    to.d <- as.numeric(strsplit(as.character(to), "-", )[[1]][3])
    tmp <- tempfile()
    on.exit(unlink(tmp))
    download.file(paste(yahoo.URL, Symbol.name, "&a=", from.m, 
        "&b=", sprintf("%.2d", from.d), "&c=", from.y, "&d=", 
        to.m, "&e=", sprintf("%.2d", to.d), "&f=", to.y, "&g=v&ignore=.csv", 
        sep = ""), destfile = tmp, quiet = !verbose)
    fr <- read.csv(tmp)
    fr <- xts(fr[, 2], as.Date(fr[, 1]))
    if (is.xts(Symbol)) {
        if (auto.update) {
            xtsAttributes(Symbol) <- list(dividends = fr)
            assign(Symbol.name, Symbol, envir = env)
        }
    }
    else if (auto.assign) {
        assign(paste(Symbol.name, "div", sep = "."), fr, envir = env)
    }
    else fr
}


modelData <- function (x, data.window = NULL, exclude.training = FALSE) 
{
    model.data <- x@model.data
    if (!is.null(data.window)) {
        if (length(data.window) > 2) {
            model.data <- model.data[index(model.data) %in% data.window]
        }
        else {
            start.date.index <- index(model.data[which(index(model.data) >= 
                as.Date(data.window[1], origin = "1970-01-01"))])
            end.date.index <- index(model.data[which(index(model.data) <= 
                as.Date(data.window[2], origin = "1970-01-01"))])
            date.range <- as.Date(intersect(start.date.index, 
                end.date.index), origin = "1970-01-01")
            model.data <- model.data[date.range]
        }
    }
    if (exclude.training == TRUE) {
        model.data <- model.data[!index(model.data) %in% x@training.data]
    }
    return(model.data)
}


OHLC <- xts::OHLC # re-exported from xts package

.__C__chobTA <- new("classRepresentation"
    , slots = structure(list(call = structure("call", package = "methods"), 
    on = structure("ANY", package = "methods"), new = structure("logical", package = "methods"), 
    TA.values = structure("ANY", package = "methods"), name = structure("character", package = "methods"), 
    params = structure("ANY", package = "methods")), .Names = c("call", 
"on", "new", "TA.values", "name", "params"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("chobTA", package = "quantmod")
    , package = "quantmod"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


chart_theme <- function () 
{
    theme <- list(col = list(bg = "#FFFFFF", label.bg = "#F0F0F0", 
        grid = "#F0F0F0", grid2 = "#F5F5F5", ticks = "#999999", 
        labels = "#333333", line.col = "darkorange", dn.col = "red", 
        up.col = NA, dn.border = "#333333", up.border = "#333333"), 
        shading = 1, format.labels = TRUE, coarse.time = TRUE, 
        rylab = TRUE, lylab = TRUE, grid.ticks.lwd = 1, grid.ticks.on = "months")
    theme$bbands <- list(col = list(fill = "whitesmoke", upper = "#D5D5D5", 
        lower = "#D5D5D5", ma = "#D5D5D5"), lty = list(upper = "dashed", 
        lower = "dashed", ma = "dotted"))
    theme
}


has.Ad <- xts::has.Ad # re-exported from xts package

has.Ask <- function (x, which = FALSE) 
{
    colAttr <- attr(x, "Ask")
    if (!is.null(colAttr)) 
        return(if (which) colAttr else TRUE)
    loc <- grep("(ask|offer).*price", colnames(x), ignore.case = TRUE)
    if (identical(loc, integer(0))) 
        loc <- grep("(ask|offer|ofr)", colnames(x), ignore.case = TRUE)
    if (!identical(loc, integer(0))) {
        return(if (which) loc else TRUE)
    }
    else FALSE
}


seriesDecel <- function (x) 
{
    diff(x, diff = 2L, na.pad = TRUE) < 0
}


addChAD <- function (..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- chaikinAD(HLC = HLC(x), volume = Vo(x))
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^.*[(]", " Chaikin Acc/Dist (", deparse(match.call()))
    gpars <- c(list(...), list(col = 11))[unique(names(c(list(col = 11), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


current.chob <- function () 
invisible(get(".chob", .plotEnv))


add_DEMA <- function (n = 10, on = 1, col = "pink", ...) 
{
    lenv <- new.env()
    lenv$add_dema <- function(x, n, col, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        dema <- DEMA(Cl(xdata), n = n)[xsubset]
        lines(1:NROW(xdata[xsubset]), dema, col = col, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, col = col, ...)), list(n = n, col = col, 
        ...))
    exp <- parse(text = gsub("list", "add_dema", as.expression(substitute(list(x = current.chob(), 
        n = n, col = col, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- DEMA(Cl(plot_object$Env$xdata), n = n)
    plot_object$set_frame(sign(on) * (abs(on) + 1L))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


futures.expiry <- function (x) 
{
    which(format(index(x), "%d") > 14 & format(index(x), "%d") < 
        22 & format(index(x), "%w") == 5 & as.numeric(months(x, 
        TRUE)) %in% c(3, 6, 9, 12))
}


.__C__chob <- new("classRepresentation"
    , slots = structure(list(device = structure("ANY", package = "methods"), 
    call = structure("call", package = "methods"), xdata = structure("ANY", package = "methods"), 
    xsubset = structure("ANY", package = "methods"), name = structure("character", package = "methods"), 
    type = structure("character", package = "methods"), passed.args = structure("ANY", package = "methods"), 
    windows = structure("numeric", package = "methods"), xrange = structure("numeric", package = "methods"), 
    yrange = structure("numeric", package = "methods"), log.scale = structure("logical", package = "methods"), 
    length = structure("numeric", package = "methods"), color.vol = structure("logical", package = "methods"), 
    multi.col = structure("logical", package = "methods"), show.vol = structure("logical", package = "methods"), 
    show.grid = structure("logical", package = "methods"), line.type = structure("character", package = "methods"), 
    bar.type = structure("character", package = "methods"), xlab = structure("character", package = "methods"), 
    ylab = structure("character", package = "methods"), spacing = structure("numeric", package = "methods"), 
    width = structure("numeric", package = "methods"), bp = structure("numeric", package = "methods"), 
    x.labels = structure("character", package = "methods"), colors = structure("ANY", package = "methods"), 
    layout = structure("ANY", package = "methods"), time.scale = structure("ANY", package = "methods"), 
    minor.ticks = structure("logical", package = "methods"), 
    major.ticks = structure("ANY", package = "methods")), .Names = c("device", 
"call", "xdata", "xsubset", "name", "type", "passed.args", "windows", 
"xrange", "yrange", "log.scale", "length", "color.vol", "multi.col", 
"show.vol", "show.grid", "line.type", "bar.type", "xlab", "ylab", 
"spacing", "width", "bp", "x.labels", "colors", "layout", "time.scale", 
"minor.ticks", "major.ticks"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("chob", package = "quantmod")
    , package = "quantmod"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


addEVWMA <- function (n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "yellow") 
{
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    if (on == 1) {
        x <- as.matrix(lchob@xdata)
        if (!is.OHLC(x) && missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- cbind(do.call(with.col, list(x)), Vo(x))
        }
        else x.tmp <- x[, with.col]
    }
    else {
        which.TA <- which(sapply(lchob@passed.args$TA, function(x) x@new))
        target.TA <- eval(lchob@passed.args$TA[which.TA][on - 
            1])[[1]]
        x <- as.matrix(target.TA@TA.values)
        if (missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    if (!has.Vo(x)) 
        return()
    chobTA@TA.values <- cbind(x.tmp, Vo(x))[lchob@xsubset, ]
    chobTA@name <- "chartEVWMA"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        col = col, n = n)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addEMA <- function (n = 10, wilder = FALSE, ratio = NULL, on = 1, with.col = Cl, 
    overlay = TRUE, col = "blue") 
{
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    if (on == 1) {
        x <- as.matrix(lchob@xdata)
        if (!is.OHLC(x) && missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    else {
        which.TA <- which(sapply(lchob@passed.args$TA, function(x) x@new))
        target.TA <- eval(lchob@passed.args$TA[which.TA][on - 
            1])[[1]]
        x <- as.matrix(target.TA@TA.values)
        if (missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    ma.tmp <- NULL
    for (i in 1:length(n)) {
        ma <- EMA(x.tmp, n = n[i], wilder = wilder[1], ratio = ratio[1])
        ma.tmp <- cbind(ma.tmp, ma)
    }
    chobTA@TA.values <- matrix(ma.tmp[lchob@xsubset, ], ncol = NCOL(ma.tmp))
    chobTA@name <- "chartEMA"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        col = col, n = n, wilder = wilder, ratio = ratio)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


zoomChart <- function (subset, yrange = NULL) 
{
    if (missing(subset) || is.null(subset)) 
        subset <- "::"
    reChart(subset = subset, yrange = yrange)
}


removeSymbols <- function (Symbols = NULL, env = parent.frame()) 
{
    if (exists(".getSymbols", env, inherits = FALSE)) {
        getSymbols <- get(".getSymbols", env, inherits = FALSE)
        if (is.null(Symbols)) {
            Symbols <- names(getSymbols)
        }
        else {
            Symbols <- Symbols[Symbols %in% names(getSymbols)]
        }
        remove(list = as.character(Symbols), envir = env)
        Symbols.remaining <- getSymbols[!names(getSymbols) %in% 
            Symbols]
        if (length(Symbols.remaining) == 0) {
            remove(list = c(".getSymbols"), envir = env)
        }
        else {
            assign(".getSymbols", Symbols.remaining, env)
        }
    }
}


addTRIX <- function (n = 20, signal = 9, maType = "EMA", percent = TRUE) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        Cl(x)
    }
    else x
    trix <- TRIX(xx, n = n, nSig = signal, maType = maType, percent = percent)
    chobTA@TA.values <- trix[lchob@xsubset, ]
    chobTA@name <- "chartTRIX"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, signal = signal, maType = maType, percent = percent)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addSMA <- function (n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "brown") 
{
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    if (on == 1) {
        x <- as.matrix(lchob@xdata)
        if (!is.OHLC(x) && missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    else {
        which.TA <- which(sapply(lchob@passed.args$TA, function(x) x@new))
        target.TA <- eval(lchob@passed.args$TA[which.TA][on - 
            1])[[1]]
        x <- as.matrix(target.TA@TA.values)
        if (missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    ma.tmp <- NULL
    for (i in 1:length(n)) {
        ma <- SMA(x.tmp, n = n[i])
        ma.tmp <- cbind(ma.tmp, ma)
    }
    chobTA@TA.values <- matrix(ma.tmp[lchob@xsubset, ], ncol = NCOL(ma.tmp))
    chobTA@name <- "chartSMA"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        col = col, n = n)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


add_TA <- function (x, order = NULL, on = NA, legend = "auto", yaxis = list(NULL, 
    NULL), col = 1, taType = NULL, ...) 
{
    lenv <- new.env()
    lenv$name <- deparse(substitute(x))
    lenv$plot_ta <- function(x, ta, on, taType, col = col, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        if (all(is.na(on))) {
            segments(axTicksByTime2(xdata[xsubset]), par("usr")[3], 
                axTicksByTime2(xdata[xsubset]), par("usr")[4], 
                col = x$Env$theme$grid)
        }
        if (is.logical(ta)) {
            ta <- merge(ta, xdata, join = "right", retside = c(TRUE, 
                FALSE))[xsubset]
            shade <- shading(as.logical(ta, drop = FALSE))
            if (length(shade$start) > 0) 
                rect(shade$start - 1/3, par("usr")[3], shade$end + 
                  1/3, par("usr")[4], col = col, ...)
        }
        else {
            subset.range <- paste(start(x$Env$xdata[x$Env$xsubset]), 
                end(x$Env$xdata[x$Env$xsubset]), sep = "/")
            ta.adj <- merge(n = .xts(1:NROW(x$Env$xdata[x$Env$xsubset]), 
                .index(x$Env$xdata[x$Env$xsubset]), tzone = indexTZ(x$Env$xdata)), 
                ta)[subset.range]
            ta.x <- as.numeric(na.approx(ta.adj[, 1], rule = 2))
            ta.y <- ta.adj[, -1]
            for (i in 1:NCOL(ta.y)) lines(ta.x, as.numeric(ta.y[, 
                i]), col = col, ...)
        }
    }
    lenv$xdata <- x
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(x = x, order = order, on = on, legend = legend, 
        taType = taType, col = col, ...)), list(x = x, order = order, 
        on = on, legend = legend, taType = taType, col = col, 
        ...))
    exp <- parse(text = gsub("list", "plot_ta", as.expression(substitute(list(x = current.chob(), 
        ta = get("x"), on = on, taType = taType, col = col, ...)))), 
        srcfile = NULL)
    plot_object <- current.chob()
    xdata <- plot_object$Env$xdata
    xsubset <- plot_object$Env$xsubset
    if (is.logical(x)) 
        no.update <- TRUE
    else no.update <- FALSE
    lenv$col <- col
    lenv$xdata <- merge(x, xdata, retside = c(TRUE, FALSE))
    if (is.na(on)) {
        plot_object$add_frame(ylim = c(0, 1), asp = 0.15)
        plot_object$next_frame()
        text.exp <- expression(text(x = c(1, 1 + strwidth(name)), 
            y = 0.3, labels = c(name, round(last(xdata[xsubset]), 
                5)), col = c(1, col), adj = c(0, 0), cex = 0.9, 
            offset = 0, pos = 4))
        plot_object$add(text.exp, env = c(lenv, plot_object$Env), 
            expr = TRUE)
        plot_object$add_frame(ylim = range(na.omit(xdata)), asp = 1)
        plot_object$next_frame()
        lenv$grid_lines <- function(xdata, xsubset) {
            pretty(xdata[xsubset])
        }
        exp <- c(expression(segments(1, grid_lines(xdata, xsubset), 
            NROW(xdata[xsubset]), grid_lines(xdata, xsubset), 
            col = theme$grid)), exp, expression(text(1 - 1/3 - 
            max(strwidth(grid_lines(xdata, xsubset))), grid_lines(xdata, 
            xsubset), noquote(format(grid_lines(xdata, xsubset), 
            justify = "right")), col = theme$labels, offset = 0, 
            pos = 4, cex = 0.9)), expression(text(NROW(xdata[xsubset]) + 
            1/3, grid_lines(xdata, xsubset), noquote(format(grid_lines(xdata, 
            xsubset), justify = "right")), col = theme$labels, 
            offset = 0, pos = 4, cex = 0.9)))
        plot_object$add(exp, env = c(lenv, plot_object$Env), 
            expr = TRUE, no.update = no.update)
    }
    else {
        for (i in 1:length(on)) {
            plot_object$set_frame(2 * on[i])
            lenv$grid_lines <- function(xdata, xsubset) {
                pretty(xdata[xsubset])
            }
            exp <- c(exp, expression(text(NROW(xdata[xsubset]) + 
                1/3, grid_lines(xdata, xsubset), noquote(format(grid_lines(xdata, 
                xsubset), justify = "right")), col = theme$labels, 
                offset = 0, pos = 4, cex = 0.9)))
            plot_object$add(exp, env = c(lenv, plot_object$Env), 
                expr = TRUE, no.update = no.update)
        }
    }
    plot_object
}


addZLEMA <- function (n = 10, ratio = NULL, on = 1, with.col = Cl, overlay = TRUE, 
    col = "red") 
{
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    if (on == 1) {
        x <- as.matrix(lchob@xdata)
        if (!is.OHLC(x) && missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    else {
        which.TA <- which(sapply(lchob@passed.args$TA, function(x) x@new))
        target.TA <- eval(lchob@passed.args$TA[which.TA][on - 
            1])[[1]]
        if (missing(with.col)) 
            with.col <- 1
        x <- as.matrix(target.TA@TA.values)
        if (missing(with.col)) {
            warning("missing \"with.col\" argument")
            invisible(return())
        }
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    chobTA@TA.values <- x.tmp[lchob@xsubset]
    chobTA@name <- "chartZLEMA"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        col = col, n = n, ratio = ratio)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


is.HLC <- xts::is.HLC # re-exported from xts package

add_WMA <- function (n = 10, wts = 1:n, on = 1, col = "green", ...) 
{
    lenv <- new.env()
    lenv$add_wma <- function(x, n, wts, col, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        ema <- WMA(Cl(xdata), n = n, wts = wts)[xsubset]
        lines(1:NROW(xdata[xsubset]), ema, col = col, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, wts = wts, col = col, ...)), list(n = n, 
        wts = wts, col = col, ...))
    exp <- parse(text = gsub("list", "add_wma", as.expression(substitute(list(x = current.chob(), 
        n = n, wts = wts, col = col, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- WMA(Cl(plot_object$Env$xdata), n = n, wts = wts)
    plot_object$set_frame(sign(on) * (abs(on) + 1L))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


add_GMMA <- function (short = c(3, 5, 8, 10, 12, 15), long = c(30, 35, 40, 
    45, 50, 60), on = 1, col = c("yellow", "brown"), ...) 
{
    lenv <- new.env()
    lenv$add_gmma <- function(x, short, long, col, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        gmma <- GMMA(Cl(xdata), short, long, maType = "EMA")[xsubset]
        col <- colorRampPalette(col)(length(short) + length(long))
        for (i in 1:(length(short) + length(long))) lines(1:NROW(xdata[xsubset]), 
            gmma[, i], col = col[i], ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(short = short, long = long, col = col, ...)), 
        list(short = short, long = long, col = col, ...))
    exp <- parse(text = gsub("list", "add_gmma", as.expression(substitute(list(x = current.chob(), 
        short = short, long = long, col = col, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- GMMA(Cl(plot_object$Env$xdata), short = short, 
        long = long)
    plot_object$set_frame(sign(on) * (abs(on) + 1L))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


addDPO <- function (n = 10, maType = "EMA", shift = n/2 + 1, percent = FALSE) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        Cl(x)
    }
    else x
    dpo <- DPO(xx, n = n, maType = maType, shift = shift, percent = percent)
    chobTA@TA.values <- dpo[lchob@xsubset]
    chobTA@name <- "chartDPO"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, maType = maType, shift = shift, percent = percent)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


is.quantmodResults <- function (x) 
{
    (class(x) == "quantmodResults")
}


add_MACD <- function (fast = 12, slow = 26, signal = 9, maType = "EMA", histogram = TRUE, 
    ...) 
{
    lenv <- new.env()
    lenv$plot_macd <- function(x, fast, slow, signal, maType, 
        histogram, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        macd <- macd[xsubset]
        segments(axTicksByTime2(xdata[xsubset]), par("usr")[3], 
            axTicksByTime2(xdata[xsubset]), par("usr")[4], col = x$Env$theme$grid)
        x.pos <- 1:NROW(macd)
        if (histogram) {
            macd.hist <- macd[, 1] - macd[, 2]
            bar.col <- ifelse(macd.hist > 0, x$Env$theme$macd$up.col, 
                x$Env$theme$macd$dn.col)
            rect(x.pos - 1/3, 0, x.pos + 1/3, macd.hist, col = bar.col, 
                border = "grey", lwd = 0.2, ...)
        }
        lines(x.pos, macd[, 1], col = x$Env$theme$macd$macd, 
            lwd = 2, , lty = 1, ...)
        lines(x.pos, macd[, 2], col = x$Env$theme$macd$signal, 
            lty = 3, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(fast = fast, slow = slow, signal = signal, 
        maType = maType, histogram = histogram, ...)), list(fast = fast, 
        slow = slow, signal = signal, maType = maType, histogram = histogram, 
        ...))
    exp <- parse(text = gsub("list", "plot_macd", as.expression(substitute(list(x = current.chob(), 
        fast = fast, slow = slow, signal = signal, maType = maType, 
        histogram = histogram, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    if (is.null(plot_object$Env$theme$macd)) {
        plot_object$Env$theme$macd$macd <- "#555555"
        plot_object$Env$theme$macd$signal <- "black"
        plot_object$Env$theme$macd$up.col <- "green"
        plot_object$Env$theme$macd$dn.col <- "red"
    }
    xdata <- plot_object$Env$xdata
    xsubset <- plot_object$Env$xsubset
    macd <- MACD(Cl(xdata), fast, slow, signal, maType)
    lenv$xdata <- structure(cbind(macd, macd[, 1] - macd[, 2]), 
        .Dimnames = list(NULL, c("macd", "signal", "histogram")))
    lenv$macd <- cbind(macd, macd[, 1] - macd[, 2])
    plot_object$add_frame(ylim = c(0, 1), asp = 0.15)
    plot_object$next_frame()
    text.exp <- expression(text(x = c(1, 1 + strwidth(paste("MACD(", 
        paste(fast, slow, signal, sep = ","), "):", sep = "")), 
        1 + strwidth(paste("MACD(", paste(fast, slow, signal, 
            sep = ","), "):", sep = "")) + strwidth("5") * 7), 
        y = 0.3, labels = c(paste("MACD(", paste(fast, slow, 
            signal, sep = ","), "):", sep = ""), round(last(xdata[xsubset, 
            1]), 5), round(last(xdata[xsubset, 2]), 5)), col = c(1, 
            theme$macd$macd, theme$macd$signal), adj = c(0, 0), 
        cex = 0.9, offset = 0, pos = 4))
    plot_object$add(text.exp, env = c(lenv, plot_object$Env), 
        expr = TRUE)
    plot_object$add_frame(ylim = range(na.omit(lenv$macd[xsubset])), 
        fixed = FALSE, asp = 1)
    plot_object$next_frame()
    lenv$grid_lines <- function(xdata, xsubset) {
        axTicksByValue(xdata[xsubset], c(5, 4, 3, 2, 1), gt = 3)
    }
    exp <- c(expression(segments(1, grid_lines(xdata, xsubset), 
        length(xsubset), grid_lines(xdata, xsubset), col = theme$grid)), 
        exp, expression(text(1 - 1/3 - max(strwidth(grid_lines(xdata, 
            xsubset))), grid_lines(xdata, xsubset), noquote(format(grid_lines(xdata, 
            xsubset), justify = "right")), col = theme$labels, 
            offset = 0, pos = 4, cex = 0.9)), expression(text(NROW(xdata[xsubset]) + 
            1/3, grid_lines(xdata, xsubset), noquote(format(grid_lines(xdata, 
            xsubset), justify = "right")), col = theme$labels, 
            offset = 0, pos = 4, cex = 0.9)))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


addSMI <- function (n = 13, slow = 25, fast = 2, signal = 9, ma.type = "EMA") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        cbind(Hi(x), Lo(x), Cl(x))
    }
    else if (is.null(dim(x))) {
        x
    }
    else {
        x[, 1]
    }
    smi <- SMI(xx, n = n, nFast = fast, nSlow = slow, nSig = signal, 
        maType = ma.type)
    chobTA@TA.values <- smi[lchob@xsubset, ]
    chobTA@name <- "chartSMI"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, slow = slow, fast = fast, signal = signal, ma.type = ma.type)
    return(chobTA)
}


addAroon <- function (n = 20, ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- cbind(Hi(x), Lo(x))
    x <- aroon(HL = x, n = n)[, -3]
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^add", "", deparse(match.call()))
    gpars <- c(list(...), list(col = 3:4))[unique(names(c(list(col = 3:4), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    return(chobTA)
}


has.Price <- function (x, which = FALSE) 
{
    colAttr <- attr(x, "Price")
    if (!is.null(colAttr)) 
        return(if (which) colAttr else TRUE)
    locBidAsk <- c(has.Bid(x, which = TRUE), has.Ask(x, which = TRUE))
    loc <- grep("price", colnames(x), ignore.case = TRUE)
    loc <- loc[!(loc %in% locBidAsk)]
    if (!identical(loc, integer(0))) {
        return(if (which) loc else TRUE)
    }
    else FALSE
}


chartTheme <- function (theme = "black", ...) 
{
    ctheme <- get(".chart.theme", as.environment("package:quantmod"))
    attr(ctheme, ".Environment") <- NULL
    current.theme <- ctheme[[theme]]
    ll <- list(...)
    for (i in names(ll)) {
        current.theme[[i]] <- ll[[i]]
    }
    return(structure(current.theme, class = "chart.theme"))
}


addOBV <- function (..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- try.xts(lchob@xdata, error = FALSE)
    x <- OBV(price = Cl(x), volume = Vo(x))
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^.*[(]", " On Balance Volume (", deparse(match.call()))
    gpars <- c(list(...), list(col = 4))[unique(names(c(list(col = 4), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        chartSeries.chob <- chartSeries.chob
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


unsetTA <- function (type = c("chartSeries", "barChart", "candleChart")) 
{
    if ("chartSeries" %in% type) 
        setDefaults(chartSeries, TA = NULL)
    if ("barChart" %in% type) 
        setDefaults(barChart, TA = NULL)
    if ("candleChart" %in% type) 
        setDefaults(candleChart, TA = NULL)
}


addEMV <- function (volume, n = 9, maType, vol.divisor = 10000, ..., on = NA, 
    legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- EMV(HL = HLC(x)[, -3], volume = Vo(x), n = n, maType = maType, 
        vol.divisor = vol.divisor)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^.*[(]", " Ease of Movement (", deparse(match.call()))
    gpars <- c(list(...), list(col = 6:7))[unique(names(c(list(col = 6:7), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        chartSeries.chob <- chartSeries.chob
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addPoints <- function (x, y = NULL, type = "p", pch = 20, offset = 1, col = 2, 
    bg = 2, cex = 1, on = 1, overlay = TRUE) 
{
    lchob <- get.current.chob()
    xdata <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    chobTA@TA.values <- xdata[lchob@xsubset, ]
    chobTA@name <- "chartPoints"
    chobTA@call <- match.call()
    chobTA@on <- on
    if (missing(bg)) 
        bg <- col
    xsubset <- x %in% lchob@xsubset
    if (NROW(x) != NROW(y)) 
        stop("x and y must be of equal lengths")
    x <- x[xsubset]
    if (!is.null(y)) 
        y <- y[xsubset]
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, subset = lchob@xsubset, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        x = x, y = y, type = type, offset = offset, pch = pch, 
        col = col, bg = bg, cex = cex)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


viewFin <- function (x, type = c("BS", "IS", "CF"), period = c("A", "Q"), 
    subset = NULL) 
{
    if (!inherits(x, "financials")) 
        stop(paste(sQuote("x"), "must be of type", sQuote("financials")))
    type <- match.arg(toupper(type[1]), c("BS", "IS", "CF"))
    period <- match.arg(toupper(period[1]), c("A", "Q"))
    statements <- list(BS = "Balance Sheet", IS = "Income Statement", 
        CF = "Cash Flow Statement", A = "Annual", Q = "Quarterly")
    if (is.null(subset)) {
        message(paste(statements[[period]], statements[[type]], 
            "for", attr(x, "symbol")))
        return(x[[type]][[period]])
    }
    else {
        tmp.table <- as.matrix(as.xts(t(x[[type]][[period]]), 
            dateFormat = "Date")[subset])
        dn1 <- rownames(tmp.table)
        dn2 <- colnames(tmp.table)
        tmp.table <- t(tmp.table)[, NROW(tmp.table):1]
        if (is.null(dim(tmp.table))) {
            dim(tmp.table) <- c(NROW(tmp.table), 1)
            dimnames(tmp.table) <- list(dn2, dn1)
        }
        message(paste(statements[[period]], statements[[type]], 
            "for", attr(x, "symbol")))
        return(tmp.table)
    }
}


chartTA <- function (x) 
{
    spacing <- x@params$spacing
    width <- x@params$width
    x.range <- x@params$xrange
    x.range <- seq(x.range[1], x.range[2] * spacing)
    tav <- x@TA.values
    if (x@new) {
        y.range <- if (is.null(x@params$yrange) || length(x@params$yrange) != 
            2) {
            seq(min(tav * 0.975, na.rm = TRUE), max(tav * 1.05, 
                na.rm = TRUE), length.out = length(x.range))
        }
        else seq(x@params$yrange[1], x@params$yrange[2], length.out = length(x.range))
        plot(x.range, y.range, type = "n", axes = FALSE, ann = FALSE)
        coords <- par("usr")
        rect(coords[1], coords[3], coords[2], coords[4], col = x@params$colors$area)
        grid(NA, NULL, col = x@params$colors$grid.col)
    }
    pars <- x@params$pars[[1]]
    pars <- lapply(pars, function(x) {
        len <- NCOL(tav)
        if (length(x) < len) {
            rep(list(x), length.out = len)
        }
        else rep(list(x), length.out = len)
    })
    col.order <- if (is.null(x@params$order)) {
        1:NCOL(tav)
    }
    else x@params$order
    if (is.null(x@params$legend)) 
        legend <- function(legend, text.col, ...) {
        }
    if (is.character(x@params$legend) && x@params$legend != "auto") {
        legend("topleft", legend = x@params$legend, bty = "n", 
            y.inter = 0.95)
        legend <- function(legend, text.col, ...) {
        }
    }
    if (!x@new) {
        legend <- function(legend, text.col, ...) {
            list(legend = legend, text.col = text.col)
        }
    }
    legend.text <- list()
    if (is.null(x@params$legend.name)) 
        x@params$legend.name <- deparse(x@call[-1][[1]])
    x.pos <- 1 + spacing * (1:length(x.range))
    if (NCOL(tav) == 1) {
        tmp.pars <- lapply(pars, function(x) x[[1]][[1]])
        if (x@params$isLogical) {
            do.call("rect", c(list(x.pos[shading(tav)$start - 
                1] - spacing/3), list(par("usr")[3]), list(x.pos[shading(tav)$end - 
                1] + spacing/3), list(par("usr")[4]), tmp.pars))
        }
        else {
            do.call("lines", c(list(seq(1, length(x.range), by = spacing)), 
                list(tav), tmp.pars))
            legend.text[[1]] <- legend("topleft", legend = c(paste(x@params$legend.name, 
                ":"), sprintf("%.3f", last(na.omit(tav)))), text.col = c(x@params$colors$fg.col, 
                last(pars$col[[1]])), bty = "n", y.inter = 0.95)
        }
    }
    else {
        for (cols in col.order) {
            tmp.pars <- lapply(pars, function(x) {
                p <- try(x[[cols]][[cols]], silent = TRUE)
                if (inherits(p, "try-error")) {
                  stop("TA parameter length must equal number of columns", 
                    call. = FALSE)
                }
                else p
            })
            do.call("lines", c(list(seq(1, length(x.range), by = spacing)), 
                list(tav[, cols]), tmp.pars))
            if (cols == 1) {
                legend.text[[cols]] <- legend("topleft", legend = c(paste(x@params$legend.name, 
                  ":")), text.col = c(x@params$colors$fg.col, 
                  last(pars$col[[cols]])), bty = "n", y.inter = 0.95)
            }
            Col.title <- colnames(tav)[cols]
            legend.text[[cols]] <- legend("topleft", legend = c(rep("", 
                cols), paste(Col.title, ":", sprintf("%.3f", 
                last(na.omit(tav[, cols]))))), text.col = pars$col[[cols]][cols], 
                bty = "n", y.inter = 0.95)
        }
    }
    axis(2)
    box(col = x@params$colors$fg.col)
    invisible(legend.text)
}


adjustOHLC <- function (x, adjust = c("split", "dividend"), use.Adjusted = FALSE, 
    ratio = NULL, symbol.name = deparse(substitute(x))) 
{
    if (is.null(ratio)) {
        if (use.Adjusted) {
            if (!has.Ad(x)) 
                stop("no Adjusted column in 'x'")
            ratio <- Ad(x)/Cl(x)
        }
        else {
            div <- getDividends(symbol.name, from = "1900-01-01")
            splits <- getSplits(symbol.name, from = "1900-01-01")
            if (is.xts(splits) && is.xts(div) && nrow(splits) > 
                0 && nrow(div) > 0) 
                div <- div * 1/adjRatios(splits = merge(splits, 
                  index(div)))[, 1]
            ratios <- adjRatios(splits, div, Cl(x))
            if (length(adjust) == 1 && adjust == "split") {
                ratio <- ratios[, 1]
            }
            else if (length(adjust) == 1 && adjust == "dividend") {
                ratio <- ratios[, 2]
            }
            else ratio <- ratios[, 1] * ratios[, 2]
        }
    }
    Adjusted <- Cl(x) * ratio
    structure(cbind((ratio * (Op(x) - Cl(x)) + Adjusted), (ratio * 
        (Hi(x) - Cl(x)) + Adjusted), (ratio * (Lo(x) - Cl(x)) + 
        Adjusted), Adjusted, if (has.Vo(x)) 
        Vo(x)
    else NULL, if (has.Ad(x)) 
        Ad(x)
    else NULL), .Dimnames = list(NULL, colnames(x)))
}


addDEMA <- function (n = 10, on = 1, with.col = Cl, overlay = TRUE, col = "pink") 
{
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    if (on == 1) {
        x <- as.matrix(lchob@xdata)
        if (!is.OHLC(x) && missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    else {
        which.TA <- which(sapply(lchob@passed.args$TA, function(x) x@new))
        target.TA <- eval(lchob@passed.args$TA[which.TA][on - 
            1])[[1]]
        x <- as.matrix(target.TA@TA.values)
        if (missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    chobTA@TA.values <- x.tmp[lchob@xsubset]
    chobTA@name <- "chartDEMA"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        col = col, n = n)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addSAR <- function (accel = c(0.02, 0.2), col = "blue") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- FALSE
    if (!is.OHLC(x)) 
        stop("SAR requires HL series")
    sar <- SAR(cbind(Hi(x), Lo(x)), accel = accel)
    chobTA@TA.values <- sar[lchob@xsubset]
    chobTA@name <- "chartSAR"
    chobTA@call <- match.call()
    chobTA@on <- 1
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        accel = accel, col = col)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


is.quantmod <- function (x) 
{
    (class(x) == "quantmod")
}


.__C__quantmodReturn <- new("classRepresentation"
    , slots = structure(list(results = structure("xtsORzoo", package = "quantmod"), 
    returns = structure("xtsORzoo", package = "quantmod"), CAGR = structure("numeric", package = "methods"), 
    HPR = structure("numeric", package = "methods"), accuracy = structure("xtsORzoo", package = "quantmod"), 
    directional.accuracy = structure("list", package = "methods"), 
    dist.of.returns = structure("list", package = "methods"), 
    returnsBy = structure("ANY", package = "methods")), .Names = c("results", 
"returns", "CAGR", "HPR", "accuracy", "directional.accuracy", 
"dist.of.returns", "returnsBy"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("quantmodReturn", package = "quantmod")
    , package = "quantmod"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


add_RSI <- function (n = 14, maType = "EMA", wilder = TRUE, ..., RSIup = 70, 
    RSIdn = 30) 
{
    lenv <- new.env()
    lenv$plot_rsi <- function(x, n, maType, wilder, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        rsi <- RSI(Cl(xdata), n = n, maType = maType, wilder = wilder)[xsubset]
        x.pos <- 1:NROW(rsi)
        theme <- x$Env$theme$rsi
        segments(axTicksByTime2(xdata[xsubset]), par("usr")[3], 
            axTicksByTime2(xdata[xsubset]), par("usr")[4], col = x$Env$theme$grid)
        lines(x.pos, rep(RSIdn, length(x.pos)), col = theme$col$lines, 
            lwd = 1, lty = 2, lend = 2, ...)
        lines(x.pos, rep(RSIup, length(x.pos)), col = theme$col$lines, 
            lwd = 1, lty = 2, lend = 2, ...)
        lines(x.pos, rsi[, 1], col = x$Env$theme$rsi$col$rsi, 
            lwd = 1.5, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, maType = maType, wilder = wilder, ...)), 
        list(n = n, maType = maType, wilder = wilder, ...))
    exp <- parse(text = gsub("list", "plot_rsi", as.expression(substitute(list(x = current.chob(), 
        n = n, maType = maType, wilder = wilder, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    if (is.null(plot_object$Env$theme$rsi)) {
        plot_object$Env$theme$rsi$col$rsi <- "saddlebrown"
        plot_object$Env$theme$rsi$col$lines <- "orange2"
    }
    xsubset <- plot_object$Env$xsubset
    rsi <- RSI(Cl(plot_object$Env$xdata), n = n, maType = maType, 
        wilder = wilder)
    plot_object$add_frame(ylim = c(0, 1), asp = 0.2)
    plot_object$next_frame()
    lenv$xdata <- structure(rsi, .Dimnames = list(NULL, "rsi"))
    text.exp <- expression(text(c(1, 1 + strwidth(paste("RSI(", 
        n, "):", sep = ""))), 0.3, c(paste("RSI(", n, "):", sep = ""), 
        round(last(xdata[xsubset]), 5)), col = c(1, theme$rsi$col$rsi), 
        adj = c(0, 0), cex = 0.9, offset = 0, pos = 4))
    plot_object$add(text.exp, env = c(lenv, plot_object$Env), 
        expr = TRUE)
    plot_object$add_frame(ylim = c(0, 100), asp = 1, fixed = TRUE)
    plot_object$next_frame()
    lenv$grid_lines <- function(xdata, x) {
        c(RSIdn, RSIup)
    }
    exp <- c(expression(segments(1, grid_lines(xdata, xsubset), 
        NROW(xdata[xsubset]), grid_lines(xdata, xsubset), col = theme$grid)), 
        exp, expression(text(1 - 1/3 - max(strwidth(grid_lines(xdata, 
            xsubset))), grid_lines(xdata, xsubset), noquote(format(grid_lines(xdata, 
            xsubset), justify = "right")), col = theme$labels, 
            offset = 0, pos = 4, cex = 0.9)), expression(text(NROW(xdata[xsubset]) + 
            1/3, grid_lines(xdata, xsubset), noquote(format(grid_lines(xdata, 
            xsubset), justify = "right")), col = theme$labels, 
            offset = 0, pos = 4, cex = 0.9)))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


add_EVWMA <- function (n = 10, on = 1, col = "darkgrey", ...) 
{
    lenv <- new.env()
    lenv$add_evwma <- function(x, n, col, ...) {
        xdata <- x$Env$xdata
        xvo <- x$Env$vo
        xsubset <- x$Env$xsubset
        evwma <- EVWMA(Cl(xdata), xvo, n = n)[xsubset]
        lines(1:NROW(xdata[xsubset]), evwma, col = col, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, col = col, ...)), list(n = n, col = col, 
        ...))
    exp <- parse(text = gsub("list", "add_evwma", as.expression(substitute(list(x = current.chob(), 
        n = n, col = col, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- EVWMA(Cl(plot_object$Env$xdata), plot_object$Env$vo, 
        n = n)
    plot_object$set_frame(sign(on) * (abs(on) + 1L))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


Hi <- xts::Hi # re-exported from xts package

getModelData <- function (x, na.rm = TRUE) 
{
    model <- x
    if (!is.quantmod(model)) 
        stop(sQuote("x"), "must be of class", dQuote("quantmod"), 
            "\n")
    if (length(model@model.inputs) == 0) {
        build.vars <- c(model@model.target, model@build.inputs)
    }
    else {
        build.vars <- c(model@model.target, model@model.inputs)
    }
    model.symbols <- vars <- all.vars(model@model.spec)
    env <- new.env()
    lapply(vars, function(V) {
        if (!exists(V)) {
            getSymbols(V, env = env)
        }
        else {
            assign(V, get(V), env)
        }
    })
    target.data <- get(model.symbols[[1]], env)
    total.columns = NULL
    for (j in 1:length(model.symbols)) {
        if (j == 1) {
            m <- as.xts(target.data)
        }
        else {
            m <- merge(m, as.xts(get(model.symbols[[j]], env)), 
                join = "inner")
        }
        total.columns[j] <- ncol(m)
    }
    fullIndex <- index(m)
    from.col = 1
    for (i in 1:length(model.symbols)) {
        assign(model.symbols[[i]], m[, from.col:(total.columns[i])], 
            env)
        from.col = total.columns[i] + 1
    }
    mf <- xts(model.frame(model@model.spec, data = env, na.action = NULL), 
        fullIndex)
    if (na.rm) 
        mf <- rbind(na.exclude(mf[-nrow(mf), ]), mf[nrow(mf), 
            ])
    colnames(mf) <- lapply(colnames(mf), function(x) {
        gsub("[) ]", "", gsub("[(,=^:'\"]", ".", x))
    })
    model@model.data <- mf
    model@build.inputs <- colnames(mf)[-1]
    model@model.formula = as.formula(paste(colnames(mf)[1], "~", 
        paste(colnames(mf)[-1], collapse = "+"), sep = ""))
    return(model)
}


has.OHLC <- xts::has.OHLC # re-exported from xts package

getPrice <- function (x, symbol = NULL, prefer = NULL, ...) 
{
    if (!is.null(symbol)) {
        loc <- grep(symbol, colnames(x))
        if (!identical(loc, integer(0))) {
            x <- x[, loc]
        }
        else {
            stop(paste("subscript out of bounds: no column name containing", 
                symbol))
        }
    }
    if (is.null(prefer)) {
        if (has.Price(x)) 
            prefer = "price"
        else if (has.Trade(x)) 
            prefer = "trade"
        else if (has.Cl(x)) 
            prefer = "close"
        else stop("subscript out of bounds, no price was discernible from the data")
    }
    if (!is.null(prefer)) {
        loc <- NULL
        switch(prefer, Op = , open = , Open = {
            loc <- has.Op(x, which = TRUE)
        }, Hi = , high = , High = {
            loc <- has.Hi(x, which = TRUE)
        }, Lo = , low = , Low = {
            loc <- has.Lo(x, which = TRUE)
        }, Cl = , close = , Close = {
            loc <- has.Cl(x, which = TRUE)
        }, Bid = , bid = {
            loc <- has.Bid(x, which = TRUE)
        }, Ask = , ask = , Offer = , offer = {
            loc <- has.Ask(x, which = TRUE)
        }, Mid = , mid = , Midpoint = , midpoint = {
            loc <- has.Mid(x, which = TRUE)
        }, Trade = , trade = {
            loc <- has.Trade(x, which = TRUE)
        }, Price = , price = {
            loc <- has.Price(x, which = TRUE)
        }, {
            loc <- grep(prefer, colnames(x))
        })
        if (!identical(loc, integer(0))) 
            return(x[, loc])
        else stop("subscript out of bounds, no price was discernible from the data")
    }
}


has.Op <- xts::has.Op # re-exported from xts package

addWMA <- function (n = 10, wts = 1:n, on = 1, with.col = Cl, overlay = TRUE, 
    col = "green") 
{
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    if (on == 1) {
        x <- as.matrix(lchob@xdata)
        if (!is.OHLC(x) && missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    else {
        which.TA <- which(sapply(lchob@passed.args$TA, function(x) x@new))
        target.TA <- eval(lchob@passed.args$TA[which.TA][on - 
            1])[[1]]
        x <- as.matrix(target.TA@TA.values)
        if (missing(with.col)) 
            with.col <- 1
        if (is.function(with.col)) {
            x.tmp <- do.call(with.col, list(x))
        }
        else x.tmp <- x[, with.col]
    }
    chobTA@TA.values <- x.tmp[lchob@xsubset]
    chobTA@name <- "chartWMA"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        col = col, n = n, wts = wts)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


getSplits <- function (Symbol, from = "1970-01-01", to = Sys.Date(), env = parent.frame(), 
    src = "yahoo", auto.assign = FALSE, auto.update = FALSE, 
    verbose = FALSE, ...) 
{
    if (missing(env)) 
        env <- parent.frame(1)
    if (is.null(env)) 
        auto.assign <- FALSE
    Symbol.name <- ifelse(!is.character(Symbol), deparse(substitute(Symbol)), 
        as.character(Symbol))
    yahoo.URL <- "http://ichart.finance.yahoo.com/x?s="
    from.y <- as.numeric(strsplit(as.character(from), "-", )[[1]][1])
    from.m <- as.numeric(strsplit(as.character(from), "-", )[[1]][2]) - 
        1
    from.d <- as.numeric(strsplit(as.character(from), "-", )[[1]][3])
    to.y <- as.numeric(strsplit(as.character(to), "-", )[[1]][1])
    to.m <- as.numeric(strsplit(as.character(to), "-", )[[1]][2]) - 
        1
    to.d <- as.numeric(strsplit(as.character(to), "-", )[[1]][3])
    tmp <- tempfile()
    on.exit(unlink(tmp))
    download.file(paste(yahoo.URL, Symbol.name, "&a=", from.m, 
        "&b=", sprintf("%.2d", from.d), "&c=", from.y, "&d=", 
        to.m, "&e=", sprintf("%.2d", to.d), "&f=", to.y, "&g=v&y=0&z=30000", 
        sep = ""), destfile = tmp, quiet = !verbose)
    fr <- read.table(tmp, skip = 1, fill = TRUE, as.is = TRUE, 
        sep = ",")
    fr <- fr[fr$V1 == "SPLIT", ]
    if (NROW(fr) == 0) {
        fr <- NA
    }
    else {
        fr$V3 <- sub(":", "/", fr$V3)
        fr$V3 <- 1/sapply(parse(text = fr$V3), eval)
        fr <- xts(fr$V3, as.Date(as.character(fr$V2), "%Y%m%d"))
        colnames(fr) <- paste(Symbol.name, "spl", sep = ".")
    }
    if (is.xts(Symbol)) {
        if (auto.update) {
            xtsAttributes(Symbol) <- list(splits = fr)
            assign(Symbol.name, Symbol, envir = env)
        }
    }
    else if (auto.assign) {
        assign(paste(Symbol.name, "spl", sep = "."), fr, envir = env)
    }
    else fr
}


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

has.Cl <- xts::has.Cl # re-exported from xts package

show <- methods::show # re-exported from methods package

specifyModel <- function (formula, na.rm = TRUE) 
{
    new.quantmod <- new("quantmod")
    formula <- as.formula(formula)
    dot.vars <- all.vars(formula)
    convert.vars <- function(vars) {
        v <- unlist(strsplit(vars, "[.]"))
        v <- paste(v[1], "(", v[2], if (length(v) > 2) 
            paste(",", v[3], sep = ""), ")", sep = "")
        return(v)
    }
    new.quantmod@model.spec <- formula
    new.quantmod@model.formula <- as.formula(gsub("[) ]", "", 
        gsub("[(,=:^'\"]", ".", deparse(formula))))
    new.quantmod@model.target <- as.character(new.quantmod@model.formula[[2]])
    new.quantmod@build.inputs <- as.character(attr(terms(new.quantmod@model.formula), 
        "term.labels"))
    vars <- all.vars(formula)
    new.quantmod@symbols <- vars
    new.quantmod@product <- vars[1]
    new.quantmod <- getModelData(new.quantmod, na.rm = na.rm)
    return(new.quantmod)
}


OHLCV <- xts::OHLCV # re-exported from xts package

seriesLo <- function (x) 
{
    UseMethod("seriesLo")
}


findPeaks <- function (x, thresh = 0) 
{
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) < 
        0) + 2
    if (!missing(thresh)) {
        if (sign(thresh) < 0) 
            thresh <- -thresh
        pks[x[pks - 1] - coredata(x[pks]) > thresh]
    }
    else pks
}


chart_pars <- function () 
{
    list(cex = 0.6, mar = c(3, 1, 0, 1))
}


is.OHLC <- xts::is.OHLC # re-exported from xts package

yahooQuote.EOD <- structure(list("ohgl1v", c("Open", "High", "Low", "Close", "Volume"
)), class = "quoteFormat")


summary <- function (object, ...) 
standardGeneric("summary")


LoHi <- function (x) 
{
    xx <- Delt(Lo(x), Hi(x))
    colnames(xx) <- paste("LoHi", deparse(substitute(x)), sep = ".")
    xx
}


addADX <- function (n = 14, maType = "EMA", wilder = TRUE) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    if (!is.OHLC(x)) 
        stop("only applicable to HLC series")
    adx <- ADX(cbind(Hi(x), Lo(x), Cl(x)), n = n, maType = maType, 
        wilder = wilder)
    chobTA@TA.values <- adx[lchob@xsubset, ]
    chobTA@name <- "chartADX"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, maType = maType, wilder = wilder)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addRSI <- function (n = 14, maType = "EMA", wilder = TRUE) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        Cl(x)
    }
    else x
    rsi <- RSI(xx, n = n, maType = maType, wilder = wilder)
    chobTA@TA.values <- rsi[lchob@xsubset]
    chobTA@name <- "chartRSI"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, wilder = wilder, maType = maType)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addExpiry <- function (type = "options", lty = "dotted") 
{
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- FALSE
    x <- lchob@xdata
    if (type == "options") {
        index.of.exp <- options.expiry(x)
    }
    else index.of.exp <- futures.expiry(x)
    chobTA@TA.values <- index.of.exp[index.of.exp %in% lchob@xsubset]
    chobTA@name <- "chartExpiry"
    chobTA@call <- match.call()
    chobTA@on <- 1
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        col = col, lty = lty)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addShading <- function (when, on = -1, overlay = TRUE, col = "blue") 
{
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    x <- lchob@xdata
    i <- when
    indexClass(x) <- "POSIXct"
    POSIXindex <- index(x)
    if (missing(i)) 
        i <- 1:NROW(x)
    if (timeBased(i)) 
        i <- as.character(as.POSIXct(i))
    if (is.character(i)) {
        i <- strsplit(i, ";")[[1]]
        i.tmp <- NULL
        for (ii in i) {
            if (!identical(grep("::", ii), integer(0))) {
                dates <- strsplit(ii, "::")[[1]]
                first.time <- ifelse(dates[1] == "", POSIXindex[1], 
                  do.call("firstof", as.list(as.numeric(strsplit(dates[1], 
                    ":|-|/| ")[[1]]))))
                last.time <- ifelse(length(dates) == 1, POSIXindex[length(POSIXindex)], 
                  do.call("lastof", as.list(as.numeric(strsplit(dates[2], 
                    ":|-|/| ")[[1]]))))
            }
            else {
                dates <- ii
                first.time <- do.call("firstof", as.list(as.numeric(strsplit(dates, 
                  ":|-|/| ")[[1]])))
                last.time <- do.call("lastof", as.list(as.numeric(strsplit(dates, 
                  ":|-|/| ")[[1]])))
            }
            i.tmp <- c(i.tmp, which(POSIXindex <= last.time & 
                POSIXindex >= first.time))
        }
        i <- i.tmp
    }
    xstart <- unique(c(i[1], i[which(diff(i) != 1) + 1]))
    xend <- unique(c(i[which(diff(i) != 1) - 1], rev(i)[1]))
    chobTA@TA.values <- x
    chobTA@name <- "chartShading"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, yrange = lchob@yrange, 
        colors = lchob@colors, spacing = lchob@spacing, width = lchob@width, 
        xsubset = lchob@xsubset, time.scale = lchob@time.scale, 
        xstart = xstart, xend = xend)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


fittedModel <- function (object) 
{
    object@fitted.model
}


OpHi <- function (x) 
{
    xx <- Delt(Op(x), Hi(x))
    colnames(xx) <- paste("OpHi", deparse(substitute(x)), sep = ".")
    xx
}


add_Series <- function (x, type = "candlesticks", order = NULL, on = NA, legend = "auto", 
    theme = NULL, ...) 
{
    lenv <- new.env()
    lenv$name <- deparse(substitute(x))
    lenv$plot_series <- function(x, series, type, ...) {
        if (FALSE) 
            theme <- NULL
        segments(axTicksByTime2(xdata[xsubset]), par("usr")[3], 
            axTicksByTime2(xdata[xsubset]), par("usr")[4], col = theme$grid)
        series <- merge(series, x$Env$xdata, join = "outer", 
            retside = c(TRUE, FALSE))[x$Env$xsubset]
        range.bars(series, type = type)
    }
    lenv$xdata <- x
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(x = x, type = type, order = order, on = on, 
        legend = legend, ...)), list(x = x, type = type, order = order, 
        on = on, legend = legend, ...))
    exp <- parse(text = gsub("list", "plot_series", as.expression(substitute(list(x = current.chob(), 
        type = type, series = get("x"), ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$theme <- if (is.null(theme)) 
        plot_object$Env$theme
    else theme
    xdata <- plot_object$Env$xdata
    xsubset <- plot_object$Env$xsubset
    tav <- merge(x, xdata, join = "left", retside = c(TRUE, FALSE))
    lenv$upper.env <- plot_object$Env
    lenv$xdata <- x
    x <- x[xsubset]
    if (is.na(on)) {
        plot_object$add_frame(ylim = c(0, 1), asp = 0.15)
        plot_object$next_frame()
        text.exp <- expression(text(x = c(1), y = 0.3, name, 
            col = c(1), adj = c(0, 0), cex = 0.9, offset = 0, 
            pos = 4))
        plot_object$add(text.exp, env = c(lenv, plot_object$Env), 
            expr = TRUE)
        plot_object$add_frame(ylim = range(na.omit(OHLC(x))), 
            asp = 1)
        plot_object$next_frame()
        plot_object$add(expression(assign("alabels", axTicksByValue(na.omit(xdata[xsubset])))), 
            expr = TRUE)
        plot_object$add(expression(segments(1, alabels, NROW(xdata[xsubset]), 
            alabels, col = theme$grid)), expr = TRUE)
        exp <- c(expression(text(1 - 1/3 - max(strwidth(alabels)), 
            alabels, noquote(format(alabels, justify = "right")), 
            col = theme$labels, offset = 0, cex = 0.9, pos = 4)), 
            expression(text(NROW(upper.env$xdata[xsubset]) + 
                1/3, alabels, noquote(format(alabels, justify = "right")), 
                col = theme$labels, offset = 0, cex = 0.9, pos = 4)), 
            exp)
    }
    else {
        plot_object$set_frame(sign(on) * (abs(on) + 1L))
    }
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


addKST <- function (n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 30), nSig = 9, 
    maType, wts = 1:NROW(n), ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- coredata(Cl(x))
    x <- KST(price = x, n = n, nROC = nROC, nSig = nSig, maType = maType, 
        wts = wts)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^addKST", "Know Sure Thing ", deparse(match.call()))
    gpars <- c(list(...), list(col = 6:7))[unique(names(c(list(col = 6:7), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        chartSeries.chob <- chartSeries.chob
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


`.__T__fittedModel<-:quantmod` <- "<environment>"

Vo <- xts::Vo # re-exported from xts package

add_BBands <- function (n = 20, maType = "SMA", sd = 2, on = -1, ...) 
{
    lenv <- new.env()
    lenv$plot_bbands <- function(x, n, maType, sd, on, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        col <- x$Env$theme$bbands$col
        lty <- x$Env$theme$bbands$lty
        bbands <- coredata(BBands(Cl(xdata), n = n, maType, sd)[xsubset])
        if (on < 0) {
            xx <- do.call("seq", as.list(x$get_xlim()))
            polygon(c(xx, rev(xx)), c(bbands[, 1], rev(bbands[, 
                3])), col = col$fill, border = NA)
            lines(1:NROW(xdata[xsubset]), bbands[, 1], lty = lty$upper, 
                col = col$upper, ...)
            lines(1:NROW(xdata[xsubset]), bbands[, 3], lty = lty$lower, 
                col = col$lower, ...)
            lines(1:NROW(xdata[xsubset]), bbands[, 2], lty = lty$ma, 
                col = col$ma, ...)
        }
        else {
            lines(1:NROW(xdata[xsubset]), bbands[, 1], lty = lty$upper, 
                ...)
            lines(1:NROW(xdata[xsubset]), bbands[, 3], lty = lty$lower, 
                ...)
            lines(1:NROW(xdata[xsubset]), bbands[, 2], lty = lty$ma, 
                ...)
        }
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, maType = maType, sd = sd, on = on, ...)), 
        list(n = n, maType = maType, sd = sd, on = on, ...))
    exp <- parse(text = gsub("list", "plot_bbands", as.expression(substitute(list(x = current.chob(), 
        n = n, maType = maType, sd = sd, on = on, ...)))), srcfile = NULL)
    chob <- current.chob()
    xdata <- chob$Env$xdata
    lenv$xdata <- BBands(Cl(xdata), n = n, maType, sd)[, -4]
    chob$set_frame(sign(on) * (abs(on) + 1L))
    chob$add(exp, env = c(lenv, chob$Env), expr = TRUE)
    chob
}


getFinancials <- function (Symbol, env = parent.frame(), src = "google", auto.assign = TRUE, 
    ...) 
{
    if (missing(env)) 
        env <- parent.frame(1)
    if (is.null(env)) 
        auto.assign <- FALSE
    Symbol <- strsplit(Symbol, ";")[[1]]
    if (length(Symbol) > 1) 
        return(unlist(lapply(Symbol, getFin, env = env, src = src, 
            auto.assign = auto.assign)))
    Symbol.name <- Symbol
    google.fin <- "http://finance.google.com/finance?fstype=ii&q="
    tmp <- tempfile()
    on.exit(unlink(tmp))
    download.file(paste(google.fin, Symbol, sep = ""), quiet = TRUE, 
        destfile = tmp)
    Symbol <- readLines(tmp, warn = FALSE)
    thead <- grep("thead", Symbol)
    tbody <- grep("tbody", Symbol)
    c1 <- lapply(seq(1, 11, 2), function(x) Symbol[thead[x]:thead[x + 
        1]])
    c2 <- lapply(c1, gsub, pattern = "<.*?>", replacement = "")
    cnames <- lapply(c2, function(x) x[-which(x == "")][-1])
    d1 <- lapply(seq(1, 11, 2), function(x) {
        Symbol[tbody[x]:tbody[x + 1]]
    })
    d2 <- lapply(d1, gsub, pattern = "<.*?>", replacement = "", 
        perl = TRUE)
    d3 <- lapply(d2, function(x) x[-which(x == "")])
    fnames <- lapply(d3, function(x) {
        gsub("&amp;", "&", x[grep("[A-Za-z]", x)])
    })
    vals <- lapply(d3, function(x) {
        as.numeric(gsub(",", "", gsub("^-$", NA, x[-grep("[A-Za-z]", 
            x)])))
    })
    make_col_names <- function(name) {
        substr(name, nchar(name) - 9, nchar(name))
    }
    fin <- lapply(1:6, function(x) {
        structure(matrix(vals[[x]], nrow = length(fnames[[x]]), 
            byrow = TRUE), .Dimnames = list(fnames[[x]], make_col_names(cnames[[x]])), 
            col_desc = cnames[[x]])
    })
    fin <- list(IS = list(Q = fin[[1]], A = fin[[2]]), BS = list(Q = fin[[3]], 
        A = fin[[4]]), CF = list(Q = fin[[5]], A = fin[[6]]))
    if (auto.assign) {
        assign(paste(gsub(":", ".", Symbol.name), "f", sep = "."), 
            structure(fin, symbol = Symbol.name, class = "financials", 
                src = "google", updated = Sys.time()), env)
        return(paste(gsub(":", ".", Symbol.name), "f", sep = "."))
    }
    else {
        return(structure(fin, symbol = Symbol.name, class = "financials", 
            src = "google", updated = Sys.time()))
    }
}


annualReturn <- function (x, subset = NULL, type = "arithmetic", leading = TRUE, 
    ...) 
{
    periodReturn(x, "yearly", subset, type, leading, ...)
}


getSymbols.RData <- function (Symbols, env, dir = "", return.class = "xts", extension = "rda", 
    col.names = c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
    ...) 
{
    importDefaults("getSymbols.rda")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    default.return.class <- return.class
    default.dir <- dir
    default.extension <- extension
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    for (i in 1:length(Symbols)) {
        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class), default.return.class, 
            return.class)
        dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
        dir <- ifelse(is.null(dir), default.dir, dir)
        extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
        extension <- ifelse(is.null(extension), default.extension, 
            extension)
        if (verbose) 
            cat("loading ", Symbols[[i]], ".....")
        if (dir == "") {
            sym.file <- paste(Symbols[[i]], extension, sep = ".")
        }
        else {
            sym.file <- file.path(dir, paste(Symbols[[i]], extension, 
                sep = "."))
        }
        if (!file.exists(sym.file)) {
            cat("\nfile ", paste(Symbols[[i]], extension, sep = "."), 
                " does not exist ", "in ", dir, "....skipping\n")
            next
        }
        local.name <- load(sym.file)
        assign("fr", get(local.name))
        if (verbose) 
            cat("done.\n")
        if (!is.xts(fr)) 
            fr <- xts(fr[, -1], as.Date(fr[, 1], origin = "1970-01-01"), 
                src = "rda", updated = Sys.time())
        colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols[[i]])), 
            col.names, sep = ".")
        fr <- convert.time.series(fr = fr, return.class = return.class)
        Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


chartSeries <- function (x, type = c("auto", "candlesticks", "matchsticks", 
    "bars", "line"), subset = NULL, show.grid = TRUE, name = NULL, 
    time.scale = NULL, log.scale = FALSE, TA = "addVo()", TAsep = ";", 
    line.type = "l", bar.type = "ohlc", theme = chartTheme("black"), 
    layout = NA, major.ticks = "auto", minor.ticks = TRUE, yrange = NULL, 
    plot = TRUE, up.col, dn.col, color.vol = TRUE, multi.col = FALSE, 
    ...) 
{
    x <- try.xts(x, error = "chartSeries requires an xtsible object")
    x <- na.omit(x)
    indexClass(x) <- "POSIXct"
    if (!is.null(subset) & is.character(subset)) {
        if (strsplit(subset, " ")[[1]][1] %in% c("first", "last")) {
            subsetvec <- strsplit(subset, " ")[[1]]
            if (length(subsetvec) < 3) {
                subset.n <- ifelse(length(subsetvec) == 1, 1L, 
                  as.numeric(subsetvec[2]))
            }
            else {
                subset.n <- paste(subsetvec[2:3], collapse = " ")
            }
            sub.index <- index(do.call(subsetvec[1], list(x, 
                subset.n)))
            xsubset <- which(index(x) %in% sub.index)
        }
        else xsubset <- which(index(x) %in% index(x[subset]))
    }
    else xsubset <- 1:NROW(x)
    xdata <- x
    x <- x[xsubset]
    if (is.OHLC(x)) {
        Opens <- as.numeric(Op(x))
        Highs <- as.numeric(Hi(x))
        Lows <- as.numeric(Lo(x))
        Closes <- as.numeric(Cl(x))
    }
    else {
        Lows <- min(x[, 1])
        Highs <- max(x[, 1])
        Closes <- as.numeric(x[, 1])
        type <- "line"
        color.vol <- FALSE
    }
    if (has.Vo(x)) {
        Volumes <- as.numeric(Vo(x))
        show.vol <- TRUE
    }
    else show.vol <- FALSE
    if (is.null(time.scale)) {
        time.scale <- periodicity(x)$scale
    }
    if (is.character(theme)) 
        theme <- chartTheme(theme)
    if (!missing(up.col)) 
        theme$up.col <- up.col
    if (!missing(dn.col)) 
        theme$dn.col <- dn.col
    if (missing(multi.col) | !multi.col) {
        multi.col <- FALSE
        theme$dn.up.col <- theme$up.col
        theme$up.up.col <- theme$up.col
        theme$dn.dn.col <- theme$dn.col
        theme$up.dn.col <- theme$dn.col
    }
    else {
        if (is.character(multi.col)) {
            theme$dn.up.col <- multi.col[1]
            theme$up.up.col <- multi.col[2]
            theme$dn.dn.col <- multi.col[3]
            theme$up.dn.col <- multi.col[4]
        }
        theme$up.col <- theme$up.up.col
        theme$dn.col <- theme$dn.dn.col
        multi.col <- TRUE
    }
    chart.options <- c("auto", "candlesticks", "matchsticks", 
        "line", "bars")
    chart <- chart.options[pmatch(type, chart.options)]
    if (chart[1] == "auto") {
        chart <- ifelse(NROW(x) > 300, "matchsticks", "candlesticks")
    }
    if (chart[1] == "candlesticks") {
        spacing <- 3
        width <- 3
    }
    else if (chart[1] == "matchsticks" || chart[1] == "line") {
        spacing <- 1
        width <- 1
    }
    else if (chart[1] == "bars") {
        spacing <- 4
        width <- 3
        if (NROW(x) > 60) 
            width <- 1
    }
    ep <- axTicksByTime(x, major.ticks)
    x.labels <- names(ep)
    chob <- new("chob")
    chob@call <- match.call(expand.dots = TRUE)
    if (is.null(name)) 
        name <- as.character(match.call()$x)
    chob@xdata <- xdata
    chob@xsubset <- xsubset
    chob@name <- name
    chob@type <- chart[1]
    chob@xrange <- c(1, NROW(x))
    if (is.OHLC(x)) {
        chob@yrange <- c(min(Lo(x), na.rm = TRUE), max(Hi(x), 
            na.rm = TRUE))
    }
    else chob@yrange <- range(x[, 1], na.rm = TRUE)
    if (!is.null(yrange) && length(yrange) == 2) 
        chob@yrange <- yrange
    chob@log.scale <- log.scale
    chob@color.vol <- color.vol
    chob@multi.col <- multi.col
    chob@show.vol <- show.vol
    chob@bar.type <- bar.type
    chob@line.type <- line.type
    chob@spacing <- spacing
    chob@width <- width
    chob@bp <- ep
    chob@x.labels <- x.labels
    chob@colors <- theme
    chob@layout <- layout
    chob@time.scale <- time.scale
    chob@minor.ticks <- minor.ticks
    chob@major.ticks <- major.ticks
    chob@length <- NROW(x)
    chob@passed.args <- as.list(match.call(expand.dots = TRUE)[-1])
    if (!is.null(TA)) {
        thisEnv <- environment()
        if (is.character(TA)) 
            TA <- as.list(strsplit(TA, TAsep)[[1]])
        chob@passed.args$TA <- list()
        for (ta in 1:length(TA)) {
            if (is.character(TA[[ta]])) {
                chob@passed.args$TA[[ta]] <- eval(parse(text = TA[[ta]]), 
                  envir = thisEnv)
            }
            else chob@passed.args$TA[[ta]] <- eval(TA[[ta]], 
                envir = thisEnv)
        }
        poss.new <- sapply(chob@passed.args$TA, function(x) {
            if (isS4(x) && is(x, "chobTA")) 
                return(x@new)
            stop("improper TA argument/call in chartSeries", 
                call. = FALSE)
        })
        if (length(poss.new) > 0) 
            poss.new <- which(poss.new)
        chob@windows <- length(poss.new) + 1
        chob@passed.args$show.vol <- any(sapply(chob@passed.args$TA, 
            function(x) x@name == "chartVo"))
    }
    else chob@windows <- 1
    chob@passed.args$TA <- sapply(chob@passed.args$TA, function(x) {
        eval(x@call)
    })
    if (plot) 
        do.call("chartSeries.chob", list(chob))
    chob@device <- as.numeric(dev.cur())
    write.chob(chob, chob@device)
    invisible(chob)
}


barChart <- function (x, subset = NULL, type = "bars", show.grid = TRUE, 
    name = deparse(substitute(x)), time.scale = NULL, log.scale = FALSE, 
    TA = "addVo()", bar.type = "ohlc", theme = chartTheme("black"), 
    major.ticks = "auto", minor.ticks = TRUE, color.vol = TRUE, 
    multi.col = FALSE, ...) 
{
    do.call("chartSeries", list(x, subset = subset, name = name, 
        type = "bars", show.grid = show.grid, time.scale = time.scale, 
        log.scale = log.scale, TA = substitute(TA), bar.type = bar.type, 
        theme = theme, major.ticks = major.ticks, minor.ticks = minor.ticks, 
        color.vol = color.vol, multi.col = multi.col, ...))
}


showSymbols <- function (env = parent.frame()) 
{
    if (exists(".getSymbols", env, inherits = FALSE)) {
        return(unlist(get(".getSymbols", env)))
    }
    else {
        return(NULL)
    }
}


reChart <- function (type = c("auto", "candlesticks", "matchsticks", "bars", 
    "line"), subset = NULL, show.grid = TRUE, name = NULL, time.scale = NULL, 
    line.type = "l", bar.type = "ohlc", theme = chartTheme("black"), 
    major.ticks = "auto", minor.ticks = TRUE, yrange = NULL, 
    up.col, dn.col, color.vol = TRUE, multi.col = FALSE, ...) 
{
    chob <- get.current.chob()
    x <- chob@xdata
    if (!missing(name)) 
        chob@name <- name
    if (!missing(type)) {
        chart.options <- c("auto", "candlesticks", "matchsticks", 
            "line", "bars")
        chart <- chart.options[pmatch(type, chart.options)]
        if (chart[1] == "auto") {
            chart <- ifelse(NROW(x) > 300, "matchsticks", "candlesticks")
        }
        if (chart[1] == "candlesticks") {
            spacing <- 3
            width <- 3
        }
        else if (chart[1] == "matchsticks" || chart[1] == "line") {
            spacing <- 1
            width <- 1
        }
        else if (chart[1] == "bars") {
            spacing <- 4
            width <- 3
            if (NROW(x) > 60) 
                width <- 1
        }
        chob@spacing <- spacing
        chob@width <- width
        chob@type <- chart[1]
    }
    if (!missing(subset)) {
        if (!is.null(subset) & is.character(subset)) {
            if (strsplit(subset, " ")[[1]][1] %in% c("first", 
                "last")) {
                subsetvec <- strsplit(subset, " ")[[1]]
                if (length(subsetvec) < 3) {
                  subset.n <- ifelse(length(subsetvec) == 1, 
                    1L, as.numeric(subsetvec[2]))
                }
                else {
                  subset.n <- paste(subsetvec[2:3], collapse = " ")
                }
                sub.index <- index(do.call(subsetvec[1], list(x, 
                  subset.n)))
                xsubset <- which(index(x) %in% sub.index)
            }
            else xsubset <- which(index(x) %in% index(x[subset]))
        }
        else xsubset <- 1:NROW(x)
        if (!is.null(subset)) {
            chob@xsubset <- xsubset
            x <- x[xsubset, ]
            chob@xrange <- c(1, NROW(x))
            if (is.OHLC(x)) {
                chob@yrange <- c(min(Lo(x), na.rm = TRUE), max(Hi(x), 
                  na.rm = TRUE))
            }
            else chob@yrange <- range(x[, 1], na.rm = TRUE)
            if (!is.null(yrange) && length(yrange) == 2) 
                chob@yrange <- yrange
        }
        chob@xsubset <- xsubset
        if (missing(major.ticks)) {
            majorticks <- chob@major.ticks
        }
        else majorticks <- major.ticks
        chob@bp <- axTicksByTime(x, majorticks)
        chob@x.labels <- names(chob@bp)
        chob@length <- NROW(x)
    }
    if (!missing(major.ticks)) {
        chob@bp <- axTicksByTime(x[chob@xsubset], major.ticks)
        chob@x.labels <- names(chob@bp)
        chob@major.ticks <- major.ticks
    }
    if (!missing(minor.ticks)) 
        chob@minor.ticks = minor.ticks
    if (!missing(theme)) {
        if (inherits(theme, "chart.theme")) {
            chob@colors <- theme
        }
        else chob@colors <- chartTheme(theme)
    }
    if (missing(theme) & !missing(multi.col)) 
        stop(paste(sQuote("theme"), "must be specified in conjunction with", 
            sQuote("multi.col")))
    theme <- chob@colors
    if (missing(multi.col)) 
        multi.col <- chob@multi.col
    if (is.OHLC(x)) {
        Opens <- as.numeric(Op(x))
        Highs <- as.numeric(Hi(x))
        Lows <- as.numeric(Lo(x))
        Closes <- as.numeric(Cl(x))
    }
    else {
        Lows <- min(x[, 1])
        Highs <- max(x[, 1])
        Closes <- as.numeric(x[, 1])
        type <- "line"
        color.vol <- FALSE
    }
    if (has.Vo(x)) {
        Volumes <- as.numeric(Vo(x))
        show.vol <- TRUE
    }
    else show.vol <- FALSE
    if (missing(time.scale)) {
        time.scale <- chob@time.scale
    }
    if (!missing(up.col)) 
        theme$up.col <- up.col
    if (!missing(dn.col)) 
        theme$dn.col <- dn.col
    if (!multi.col) {
        theme$dn.up.col <- theme$up.col
        theme$up.up.col <- theme$up.col
        theme$dn.dn.col <- theme$dn.col
        theme$up.dn.col <- theme$dn.col
    }
    else {
        if (is.character(multi.col)) {
            theme$dn.up.col <- multi.col[1]
            theme$up.up.col <- multi.col[2]
            theme$dn.dn.col <- multi.col[3]
            theme$up.dn.col <- multi.col[4]
        }
        theme$up.col <- theme$up.up.col
        theme$dn.col <- theme$dn.dn.col
        multi.col <- TRUE
    }
    chob@colors <- theme
    chob@multi.col <- multi.col
    chob@color.vol <- color.vol
    chob@passed.args$TA <- sapply(chob@passed.args$TA, function(x) eval(x@call))
    chartSeries.chob(chob)
    chob@device <- as.numeric(dev.cur())
    write.chob(chob, chob@device)
    invisible(chob)
}


new.replot <- function (frame = 1, asp = 1, xlim = c(1, 10), ylim = list(structure(c(1, 
    10), fixed = FALSE))) 
{
    Env <- new.env()
    Env$frame <- frame
    Env$asp <- asp
    Env$xlim <- xlim
    Env$ylim <- ylim
    Env$pad1 <- -0
    Env$pad3 <- 0
    if (length(asp) != length(ylim)) 
        stop("'ylim' and 'asp' must be the same length")
    set_frame <- function(frame, clip = TRUE) {
        Env$frame <<- frame
        set_window(clip)
    }
    set_asp <- function(asp) {
        Env$asp <<- asp
    }
    set_xlim <- function(xlim) {
        Env$xlim <<- xlim
    }
    set_ylim <- function(ylim) {
        Env$ylim <<- ylim
    }
    set_pad <- function(pad) {
        Env$pad1 <<- pad[1]
        Env$pad3 <<- pad[2]
    }
    reset_ylim <- function() {
        ylim <- get_ylim()
        ylim <- rep(list(c(Inf, -Inf)), length(ylim))
        lapply(Env$actions, function(x) {
            frame <- attr(x, "frame")
            if (frame > 0) {
                lenv <- attr(x, "env")
                if (is.list(lenv)) 
                  lenv <- lenv[[1]]
                ylim[[frame]][1] <<- min(ylim[[frame]][1], range(na.omit(lenv$xdata[Env$xsubset]))[1], 
                  na.rm = TRUE)
                ylim[[frame]][2] <<- max(ylim[[frame]][2], range(na.omit(lenv$xdata[Env$xsubset]))[2], 
                  na.rm = TRUE)
            }
        })
        set_ylim(ylim)
    }
    get_frame <- function(frame) {
        Env$frame
    }
    get_asp <- function(asp) {
        Env$asp
    }
    get_xlim <- function(xlim) {
        Env$xlim
    }
    get_ylim <- function(ylim) {
        Env$ylim
    }
    get_pad <- function() c(Env$pad1, Env$pad3)
    scale_ranges <- function(frame, asp, ranges) {
        asp/asp[frame] * abs(diff(ranges[[frame]]))
    }
    set_window <- function(clip = TRUE, set = TRUE) {
        frame <- Env$frame
        frame <- abs(frame)
        asp <- Env$asp
        xlim <- Env$xlim
        ylim <- lapply(Env$ylim, function(x) structure(x + (diff(x) * 
            c(Env$pad1, Env$pad3)), fixed = attr(x, "fixed")))
        sr <- scale_ranges(frame, asp, ylim)
        if (frame == 1) {
            win <- list(xlim, c((ylim[[frame]][1] - sum(sr[-1])), 
                ylim[[frame]][2]))
        }
        else if (frame == length(ylim)) {
            win <- list(xlim, c(ylim[[frame]][1], ylim[[frame]][2] + 
                sum(sr[-length(sr)])))
        }
        else {
            win <- list(xlim, c(ylim[[frame]][1] - sum(sr[-(1:frame)]), 
                ylim[[frame]][2] + sum(sr[-(frame:length(sr))])))
        }
        if (!set) 
            return(win)
        do.call("plot.window", win)
        if (clip) 
            clip(par("usr")[1], par("usr")[2], ylim[[frame]][1], 
                ylim[[frame]][2])
    }
    get_actions <- function(frame) {
        actions <- NULL
        for (i in 1:length(Env$actions)) {
            if (abs(attr(Env$actions[[i]], "frame")) == frame) 
                actions <- c(actions, Env$actions[i])
        }
        actions
    }
    add_frame <- function(after, ylim = c(0, 0), asp = 0, fixed = FALSE) {
        if (missing(after)) 
            after <- max(abs(sapply(Env$actions, function(x) attr(x, 
                "frame"))))
        for (i in 1:length(Env$actions)) {
            cframe <- attr(Env$actions[[i]], "frame")
            if (cframe > 0 && cframe > after) 
                attr(Env$actions[[i]], "frame") <- cframe + 1L
            if (cframe < 0 && cframe < -after) 
                attr(Env$actions[[i]], "frame") <- cframe - 1L
        }
        Env$ylim <- append(Env$ylim, list(structure(ylim, fixed = fixed)), 
            after)
        Env$asp <- append(Env$asp, asp, after)
    }
    update_frames <- function(headers = TRUE) {
        from_by <- ifelse(headers, 2, 1)
        ylim <- get_ylim()
        for (y in seq(from_by, length(ylim), by = from_by)) {
            if (!attr(ylim[[y]], "fixed")) 
                ylim[[y]] <- structure(c(Inf, -Inf), fixed = FALSE)
        }
        lapply(Env$actions, function(x) {
            if (!is.null(attr(x, "no.update")) && attr(x, "no.update")) 
                return(NULL)
            frame <- abs(attr(x, "frame"))
            fixed <- attr(ylim[[frame]], "fixed")
            if (frame%%from_by == 0 && !fixed) {
                lenv <- attr(x, "env")
                if (is.list(lenv)) 
                  lenv <- lenv[[1]]
                dat.range <- range(na.omit(lenv$xdata[Env$xsubset]))
                min.tmp <- min(ylim[[frame]][1], dat.range, na.rm = TRUE)
                max.tmp <- max(ylim[[frame]][2], dat.range, na.rm = TRUE)
                ylim[[frame]] <<- structure(c(min.tmp, max.tmp), 
                  fixed = fixed)
            }
        })
        set_ylim(ylim)
    }
    remove_frame <- function(frame) {
        rm.frames <- NULL
        max.frame <- max(abs(sapply(Env$actions, function(x) attr(x, 
            "frame"))))
        for (i in 1:length(Env$actions)) {
            cframe <- attr(Env$actions[[i]], "frame")
            if (abs(attr(Env$actions[[i]], "frame")) == frame) 
                rm.frames <- c(rm.frames, i)
            if (cframe > 0 && cframe > frame) {
                attr(Env$actions[[i]], "frame") <- cframe - 1L
            }
            if (cframe < 0 && cframe < -frame) {
                attr(Env$actions[[i]], "frame") <- cframe + 1L
            }
        }
        if (frame > max.frame) {
            Env$frame <- max.frame
        }
        else Env$frame <- max.frame - 1
        Env$ylim <- Env$ylim[-frame]
        Env$asp <- Env$asp[-frame]
        if (!is.null(rm.frames)) 
            Env$actions <- Env$actions[-rm.frames]
    }
    next_frame <- function() {
        set_frame(max(abs(sapply(Env$actions, function(x) attr(x, 
            "frame")))) + 1L)
    }
    move_frame <- function() {
    }
    Env$actions <- list()
    add <- replot <- function(x, env = Env, expr = FALSE, clip = TRUE, 
        ...) {
        if (!expr) {
            x <- match.call()$x
        }
        a <- structure(x, frame = Env$frame, clip = clip, env = env, 
            ...)
        Env$actions[[length(Env$actions) + 1]] <<- a
    }
    replot_env <- new.env()
    class(replot_env) <- c("replot", "environment")
    replot_env$Env <- Env
    replot_env$set_window <- set_window
    replot_env$add <- add
    replot_env$replot <- replot
    replot_env$get_actions <- get_actions
    replot_env$subset <- subset
    replot_env$update_frames <- update_frames
    replot_env$set_frame <- set_frame
    replot_env$get_frame <- get_frame
    replot_env$next_frame <- next_frame
    replot_env$add_frame <- add_frame
    replot_env$remove_frame <- remove_frame
    replot_env$set_asp <- set_asp
    replot_env$get_asp <- get_asp
    replot_env$set_xlim <- set_xlim
    replot_env$get_xlim <- get_xlim
    replot_env$reset_ylim <- reset_ylim
    replot_env$set_ylim <- set_ylim
    replot_env$get_ylim <- get_ylim
    replot_env$set_pad <- set_pad
    return(replot_env)
}


setDefaults <- function (name, ...) 
{
    if (is.function(name)) 
        name <- deparse(substitute(name))
    if (!is.function(eval(parse(text = name)))) 
        stop("argument 'name' must be a function")
    default.name <- paste(name, "Default", sep = ".")
    old.defaults <- getDefaults(name)
    new.defaults <- list(...)
    avail.defaults <- formals(name)
    matched.defaults <- list()
    for (arg in names(new.defaults)) {
        if (!is.na(pmatch(arg, names(avail.defaults)))) {
            arg.name <- match.arg(arg, names(avail.defaults))
            mc <- match.call()[[arg]]
            if (typeof(mc) == "language") 
                mc <- eval(mc)
            if (is.character(mc)) 
                new.defaults[[arg]] <- paste("'", mc, "'", sep = "")
            if (is.name(mc)) 
                new.defaults[[arg]] <- as.character(mc)
            matched.defaults[[arg.name]] <- new.defaults[[arg]]
            if (is.null(new.defaults[[arg]])) 
                old.defaults[[arg.name]] <- NULL
        }
        else {
            warning(paste(sQuote(arg), "was not set, possibly not a formal arg for", 
                sQuote(name)))
        }
    }
    all.and.matched <- c(matched.defaults, old.defaults)
    all.and.matched <- all.and.matched[unique(names(all.and.matched))]
    if (length(all.and.matched) == 0) {
        if (!is.null(getDefaults(name))) 
            unsetDefaults(name, confirm = FALSE)
    }
    else {
        env <- as.environment(-1)
        default.deparse <- function(x) {
            if (is.character(x)) 
                paste(deparse(x), sep = "", collapse = "")
            else x
        }
        default.values <- lapply(all.and.matched, default.deparse)
        default.list <- paste(names(all.and.matched), "=", default.values)
        eval(parse(text = paste("options(", default.name, "=list(", 
            paste(default.list, collapse = ","), "))", sep = "")), 
            envir = env)
    }
}


seriesIncr <- function (x, thresh = 0, diff. = 1L) 
{
    diff(x, diff = diff., na.pad = TRUE) > thresh
}


getMetals <- function (Metals, from = Sys.Date() - 500, to = Sys.Date(), base.currency = "USD", 
    env = parent.frame(), verbose = FALSE, warning = TRUE, auto.assign = TRUE, 
    ...) 
{
    importDefaults("getMetals")
    if (missing(env)) 
        env <- parent.frame(1)
    if (is.null(env)) 
        auto.assign <- FALSE
    metals <- c("XAU-GOLD", "XPD-PALLADIUM", "XPT-PLATINUM", 
        "XAG-SILVER")
    metals <- metals[sapply(Metals, function(x) grep(x, metals, 
        ignore.case = TRUE))]
    metals <- as.character(sapply(metals, function(x) {
        paste(strsplit(x, "-")[[1]][1], base.currency, sep = "/")
    }))
    getSymbols.oanda(Symbols = metals, from = from, to = to, 
        auto.assign = auto.assign, env = env, verbose = verbose, 
        warning = warning, ...)
}


periodReturn <- function (x, period = "monthly", subset = NULL, type = "arithmetic", 
    leading = TRUE, ...) 
{
    xx <- try.xts(x)
    if (inherits(x, "ts")) {
        x <- na.omit(try.xts(x))
        xtsAttributes(x) <- CLASS(x) <- NULL
        xx <- x
        TS <- TRUE
    }
    else TS <- FALSE
    if (has.Op(xx) & has.Cl(xx)) {
        getFirst <- function(X) Op(X)
        getLast <- function(X) Cl(X)
    }
    else getFirst <- getLast <- function(X) X[, 1]
    on.opts <- list(daily = "days", weekly = "weeks", monthly = "months", 
        quarterly = "quarters", yearly = "years", annually = "years")
    ep <- endpoints(xx, on = on.opts[[period]])
    ret <- Delt_(Cl(to_period(xx, period = on.opts[[period]], 
        ...)), type = type)
    if (leading) {
        firstval <- as.numeric(Delt_(getFirst(xx[1]), getLast(xx[ep[2]]), 
            type = type))
        ret[1, ] <- firstval
    }
    colnames(ret) <- paste(period, "returns", sep = ".")
    if (TS) 
        xx <- 1
    tmp.ret <- reclass(ret, xx[ep[-1]])
    if (is.null(subset)) 
        subset <- "/"
    reclass(as.xts(tmp.ret)[subset])
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

peak <- function (x) 
{
    .Deprecated("findPeaks", package = "quantmod")
    findPeaks(x)
}


has.Qty <- function (x, which = FALSE) 
{
    colAttr <- attr(x, "Qty")
    if (!is.null(colAttr)) 
        return(if (which) colAttr else TRUE)
    locBidAsk <- c(has.Bid(x, which = TRUE), has.Ask(x, which = TRUE))
    loc <- grep("qty", colnames(x), ignore.case = TRUE)
    loc <- loc[!(loc %in% locBidAsk)]
    if (!identical(loc, integer(0))) {
        return(if (which) loc else TRUE)
    }
    else FALSE
}


yearlyReturn <- function (x, subset = NULL, type = "arithmetic", leading = TRUE, 
    ...) 
{
    periodReturn(x, "yearly", subset, type, leading, ...)
}


add_Vo <- function (...) 
{
    lenv <- new.env()
    lenv$plot_vo <- function(x, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        vo <- x$Env$vo[xsubset]
        if (is.OHLC(xdata[xsubset])) {
            Opens <- as.numeric(Op(xdata[xsubset]))
            Highs <- as.numeric(Hi(xdata[xsubset]))
            Lows <- as.numeric(Lo(xdata[xsubset]))
            Closes <- as.numeric(Cl(xdata[xsubset]))
        }
        bar.col <- ifelse(Opens < Closes, x$Env$theme$up.col, 
            x$Env$theme$dn.col)
        bar.border <- ifelse(Opens < Closes, x$Env$theme$up.border, 
            x$Env$theme$dn.border)
        x.pos <- 1:NROW(vo)
        min.vol <- min(vo)
        segments(axTicksByTime(xdata[xsubset], ticks.on = x$Env$ticks.on), 
            range(na.omit(vo))[1], axTicksByTime(xdata[xsubset], 
                ticks.on = x$Env$ticks.on), range(na.omit(vo))[2], 
            col = x$Env$theme$grid)
        rect(x.pos - 1/3, min.vol, x.pos + 1/3, vo, col = bar.col, 
            border = bar.border, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(...)), list(...))
    exp <- parse(text = gsub("list", "plot_vo", as.expression(substitute(list(x = current.chob(), 
        ...)))), srcfile = NULL)
    plot_object <- current.chob()
    xdata <- plot_object$Env$vo
    xsubset <- plot_object$Env$xsubset
    theme <- plot_object$theme
    vo <- xdata[xsubset]
    lenv$xdata <- xdata
    plot_object$add_frame(ylim = c(0, 1), asp = 0.15)
    plot_object$next_frame()
    text.exp <- expression(text(c(0, 0 + strwidth(paste("Volume:", 
        sep = ""))), 0.5, c(paste("Volume:", sep = ""), prettyNum(last(xdata[xsubset]), 
        big.mark = ",")), col = ifelse(diff(last(xdata[xsubset], 
        2)) > 0, theme$up.col, theme$dn.col), adj = c(0, 0), 
        cex = 0.9, offset = 0, pos = 4))
    plot_object$add(rect(par("usr")[1], 0, par("usr")[2], 1, 
        col = theme$grid, border = NA))
    plot_object$add(text.exp, env = c(lenv, plot_object$Env), 
        expr = TRUE)
    lenv$grid_lines <- function(xdata, x) {
        seq(0, 1)
    }
    exp <- c(expression(abline(h = grid_lines(xdata, xsubset), 
        col = theme$grid)), expression(text(0, grid_lines(xdata, 
        xsubset), sprintf("%+d", grid_lines(xdata, xsubset)), 
        col = theme$labels, offset = 0, pos = 2)), expression(text(length(xsubset), 
        grid_lines(xdata, xsubset), sprintf("%+d", grid_lines(xdata, 
            xsubset)), col = theme$labels, offset = 0, pos = 4)), 
        exp)
    plot_object$add_frame(ylim = range(vo), asp = 1)
    plot_object$next_frame()
    plot_object$replot(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


seriesAccel <- function (x) 
{
    diff(x, diff = 2L, na.pad = TRUE) > 0
}


swapTA <- function (ta1, ta2, occ1 = 1, occ2 = 1, dev) 
{
    if (missing(ta1) | missing(ta2)) 
        stop("two TA indicator required")
    if (missing(dev)) 
        dev <- dev.cur()
    ta.list <- listTA(dev)
    lchob <- get.chob()[[dev]]
    if (regexpr("^add", ta1) == -1) 
        ta1 <- paste("add", ta1, sep = "")
    if (regexpr("^add", ta2) == -1) 
        ta2 <- paste("add", ta2, sep = "")
    which.ta1 <- which(ta1 == sapply(ta.list, function(x) deparse(x[[1]])))[occ1]
    which.ta2 <- which(ta2 == sapply(ta.list, function(x) deparse(x[[1]])))[occ2]
    tmp.ta1 <- lchob@passed.args$TA[[which.ta1]]
    tmp.ta2 <- lchob@passed.args$TA[[which.ta2]]
    lchob@passed.args$TA[[which.ta1]] <- tmp.ta2
    lchob@passed.args$TA[[which.ta2]] <- tmp.ta1
    do.call("chartSeries.chob", list(lchob))
    write.chob(lchob, lchob@device)
}


modelSignal <- function (x) 
{
    if (!is.quantmodResults(x)) 
        stop(paste(dQuote("x"), "must be of class", dQuote("quantmodResults")))
    x@signal
}


addZigZag <- function (change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE, 
    ..., on = -1, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- cbind(Hi(x), Lo(x))
    x <- ZigZag(HL = x, change = change, percent = percent, retrace = retrace, 
        lastExtreme = lastExtreme)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^add", "", deparse(match.call()))
    gpars <- c(list(...), list(col = 4, lwd = 3))[unique(names(c(list(col = 4, 
        lwd = 3), list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


findValleys <- function (x, thresh = 0) 
{
    pks <- which(diff(sign(diff(x, na.pad = FALSE)), na.pad = FALSE) > 
        0) + 2
    if (!missing(thresh)) {
        if (sign(thresh) > 0) 
            thresh <- -thresh
        pks[x[pks - 1] - coredata(x[pks]) < thresh]
    }
    else pks
}


options.expiry <- function (x) 
{
    which(format(index(x), "%d") > 14 & format(index(x), "%d") < 
        22 & format(index(x), "%w") == 5)
}


getSymbols.oanda <- function (Symbols, env, return.class = "xts", from = Sys.Date() - 
    499, to = Sys.Date(), ...) 
{
    importDefaults("getSymbols.oanda")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!auto.assign && length(Symbols) > 1) 
        stop("must use auto.assign=TRUE for multiple Symbols requests")
    default.return.class <- return.class
    default.from <- from
    default.to <- to
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    daySpans <- c(7, 30, 60, 90, 180, 364, 728, 1820)
    dateStr <- c("d7", "d30", "d60", "d90", "d180", "y1", "y2", 
        "y5")
    tmp <- tempfile()
    on.exit(unlink(tmp))
    for (i in 1:length(Symbols)) {
        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class), default.return.class, 
            return.class)
        from <- getSymbolLookup()[[Symbols[[i]]]]$from
        from <- ifelse(is.null(from), default.from, from)
        from <- as.Date(from, origin = "1970-01-01")
        to <- getSymbolLookup()[[Symbols[[i]]]]$to
        to <- ifelse(is.null(to), default.to, to)
        to <- as.Date(to, origin = "1970-01-01")
        Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
        Symbols.name <- ifelse(is.null(Symbols.name), Symbols[[i]], 
            Symbols.name)
        currency.pair <- strsplit(toupper(Symbols.name), "/")[[1]]
        if (length(currency.pair) != 2) {
            warning(paste("incorrectly specified currency pair", 
                Symbols.name))
            next
        }
        if (verbose) 
            cat("downloading ", Symbols.name, ".....")
        dateDiff <- difftime(to, from, units = "days")
        dateLoc <- which(daySpans >= dateDiff)
        if (!length(dateLoc)) {
            warning("Oanda limits data to 5years. Symbol: ", 
                Symbols[[i]])
            dateLoc <- length(dateStr)
        }
        data_range <- dateStr[dateLoc[1]]
        oanda.URL <- paste("https://www.oanda.com/currency/historical-rates/download?", 
            "quote_currency=", currency.pair[1], "&end_date=", 
            to, "&start_date=", from, "&period=daily&display=absolute&rate=0", 
            "&data_range=", data_range, "&price=mid&view=table", 
            "&base_currency_0=", currency.pair[2], "&base_currency_1=&base_currency_2=&base_currency_3=&base_currency_4=&download=csv", 
            sep = "")
        download.file(oanda.URL, destfile = tmp, quiet = !verbose)
        fr <- read.csv(tmp, skip = 4, as.is = TRUE, header = TRUE)
        fr[, 1L] <- as.Date(fr[, 1L], origin = "1970-01-01")
        fr <- na.omit(fr[, 1:2])
        if (is.character(fr[, 2L])) 
            fr[, 2L] <- as.numeric(gsub(",", "", fr[, 2L], fixed = TRUE))
        if (verbose) 
            cat("done.\n")
        fr <- xts(fr[, -1L], fr[, 1L], src = "oanda", updated = Sys.time())
        fr <- fr[paste(from, to, sep = "/")]
        colnames(fr) <- gsub("/", ".", Symbols[[i]])
        fr <- convert.time.series(fr = fr, return.class = return.class)
        Symbols[[i]] <- toupper(gsub("\\^|/", "", Symbols[[i]]))
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


yahooQF <- function (names) 
{
    optnames <- c("Ask", "Average Daily Volume", "Ask Size", 
        "Bid", "Ask (Real-time)", "Bid (Real-time)", "Book Value", 
        "Bid Size", "Change & Percent Change", "Change", "Commission", 
        "Change (Real-time)", "After Hours Change (Real-time)", 
        "Dividend/Share", "Last Trade Date", "Trade Date", "Earnings/Share", 
        "Error Indication (returned for symbol changed / invalid)", 
        "EPS Estimate Current Year", "EPS Estimate Next Year", 
        "EPS Estimate Next Quarter", "Float Shares", "Days Low", 
        "Days High", "52-week Low", "52-week High", "Holdings Gain Percent", 
        "Annualized Gain", "Holdings Gain", "Holdings Gain Percent (Real-time)", 
        "Holdings Gain (Real-time)", "More Info", "Order Book (Real-time)", 
        "Market Capitalization", "Market Cap (Real-time)", "EBITDA", 
        "Change From 52-week Low", "Percent Change From 52-week Low", 
        "Last Trade (Real-time) With Time", "Change Percent (Real-time)", 
        "Last Trade Size", "Change From 52-week High", "Percent Change From 52-week High", 
        "Last Trade (With Time)", "Last Trade (Price Only)", 
        "High Limit", "Low Limit", "Days Range", "Days Range (Real-time)", 
        "50-day Moving Average", "200-day Moving Average", "Change From 200-day Moving Average", 
        "Percent Change From 200-day Moving Average", "Change From 50-day Moving Average", 
        "Percent Change From 50-day Moving Average", "Name", 
        "Notes", "Open", "Previous Close", "Price Paid", "Change in Percent", 
        "Price/Sales", "Price/Book", "Ex-Dividend Date", "P/E Ratio", 
        "Dividend Pay Date", "P/E Ratio (Real-time)", "PEG Ratio", 
        "Price/EPS Estimate Current Year", "Price/EPS Estimate Next Year", 
        "Symbol", "Shares Owned", "Short Ratio", "Last Trade Time", 
        "Trade Links", "Ticker Trend", "1 yr Target Price", "Volume", 
        "Holdings Value", "Holdings Value (Real-time)", "52-week Range", 
        "Days Value Change", "Days Value Change (Real-time)", 
        "Stock Exchange", "Dividend Yield")
    optshort <- c("Ask", "Ave. Daily Volume", "Ask Size", "Bid", 
        "Ask (RT)", "Bid (RT)", "Book Value", "Bid Size", "Change & % Change", 
        "Change", "Commission", "Change (RT)", "After Hours Change (RT)", 
        "Dividend/Share", "Last Trade Date", "Trade Date", "Earnings/Share", 
        "Error Indication (returned for symbol changed / invalid)", 
        "EPS Estimate Current Year", "EPS Estimate Next Year", 
        "EPS Estimate Next Quarter", "Float Shares", "Low", "High", 
        "52-week Low", "52-week High", "Holdings Gain %", "Annualized Gain", 
        "Holdings Gain", "Holdings Gain % (RT)", "Holdings Gain (RT)", 
        "More Info", "Order Book (RT)", "Market Capitalization", 
        "Market Cap (RT)", "EBITDA", "Change From 52-week Low", 
        "% Change From 52-week Low", "Last Trade (RT) With Time", 
        "%Change (RT)", "Last Size", "Change From 52-week High", 
        "% Change From 52-week High", "Last", "Last", "High Limit", 
        "Low Limit", "Days Range", "Days Range (RT)", "50-day MA", 
        "200-day MA", "Change From 200-day MA", "% Change From 200-day MA", 
        "Change From 50-day MA", "% Change From 50-day MA", "Name", 
        "Notes", "Open", "P. Close", "Price Paid", "% Change", 
        "Price/Sales", "Price/Book", "Ex-Dividend Date", "P/E Ratio", 
        "Dividend Pay Date", "P/E Ratio (RT)", "PEG Ratio", "Price/EPS Estimate Current Year", 
        "Price/EPS Estimate Next Year", "Symbol", "Shares Owned", 
        "Short Ratio", "Last Trade Time", "Trade Links", "Ticker Trend", 
        "1 yr Target Price", "Volume", "Holdings Value", "Holdings Value (RT)", 
        "52-week Range", "Days Value Change", "Days Value Change (RT)", 
        "Stock Exchange", "Dividend Yield")
    optcodes <- c("a", "a2", "a5", "b", "b2", "b3", "b4", "b6", 
        "c", "c1", "c3", "c6", "c8", "d", "d1", "d2", "e", "e1", 
        "e7", "e8", "e9", "f6", "g", "h", "j", "k", "g1", "g3", 
        "g4", "g5", "g6", "i", "i5", "j1", "j3", "j4", "j5", 
        "j6", "k1", "k2", "k3", "k4", "k5", "l", "l1", "l2", 
        "l3", "m", "m2", "m3", "m4", "m5", "m6", "m7", "m8", 
        "n", "n4", "o", "p", "p1", "p2", "p5", "p6", "q", "r", 
        "r1", "r2", "r5", "r6", "r7", "s", "s1", "s7", "t1", 
        "t6", "t7", "t8", "v", "v1", "v7", "w", "w1", "w4", "x", 
        "y")
    w <- NULL
    if (!missing(names)) {
        names <- unlist(strsplit(names, ";"))
        for (n in names) {
            w <- c(w, which(optnames %in% n))
        }
    }
    else {
        names <- select.list(optnames, multiple = TRUE)
        for (n in names) {
            w <- c(w, which(optnames %in% n))
        }
    }
    str <- paste(optcodes[w], collapse = "")
    nms <- optshort[w]
    return(structure(list(str, nms), class = "quoteFormat"))
}


lineChart <- function (x, subset = NULL, type = "line", show.grid = TRUE, 
    name = deparse(substitute(x)), time.scale = NULL, log.scale = FALSE, 
    TA = "addVo()", line.type = "l", theme = chartTheme("black"), 
    major.ticks = "auto", minor.ticks = TRUE, color.vol = TRUE, 
    multi.col = FALSE, ...) 
{
    do.call("chartSeries", list(x, subset = subset, name = name, 
        type = "line", show.grid = show.grid, time.scale = time.scale, 
        log.scale = log.scale, TA = substitute(TA), line.type = line.type, 
        theme = theme, major.ticks = major.ticks, minor.ticks = minor.ticks, 
        color.vol = color.vol, multi.col = multi.col, ...))
}


`fittedModel<-` <- function (object, value) 
{
    standardGeneric("fittedModel<-")
}


getDefaults <- function (name = NULL, arg = NULL) 
{
    if (is.function(name)) 
        name <- deparse(substitute(name))
    if (!is.null(name)) {
        if (length(name) > 1) {
            if (!is.character(name)) 
                stop(paste(sQuote("name"), "must be a character vector", 
                  "or visible function"))
            all.names = list()
        }
        for (each.name in name) {
            default.name <- paste(each.name, "Default", sep = ".")
            if (is.null(arg)) {
                if (exists("all.names", inherits = FALSE)) {
                  all.names[[each.name]] <- options(default.name)[[1]]
                }
                else {
                  return(options(default.name)[[1]])
                }
            }
            else {
                default.list <- list()
                for (each.arg in arg) {
                  default.list[[each.arg]] <- options(default.name)[[1]][[each.arg]]
                }
                if (exists("all.names", inherits = FALSE)) {
                  all.names[[each.name]] <- default.list
                }
                else {
                  return(default.list)
                }
            }
        }
        return(all.names)
    }
    else {
        all.options <- names(options())
        all.Defaults <- as.character(sapply(all.options[grep(".Default$", 
            all.options)], FUN = function(x) {
            gsub(".Default$", "", x)
        }))
        if (identical(all.Defaults, character(0))) 
            return(NULL)
        return(all.Defaults)
    }
}


loadSymbolLookup <- function (file, dir = "") 
{
    if (missing(file)) 
        file <- ".quantmod.SymbolLookup.rda"
    if (dir != "") {
        file <- file.path(dir, file)
    }
    if (file.exists(file)) {
        load(file)
        options(getSymbols.sources = get("lookup.list"))
    }
    else {
        stop("no SymbolLookup file exists in this directory")
    }
}


getSymbols <- function (Symbols = NULL, env = parent.frame(), reload.Symbols = FALSE, 
    verbose = FALSE, warnings = TRUE, src = "yahoo", symbol.lookup = TRUE, 
    auto.assign = getOption("getSymbols.auto.assign", TRUE), 
    ...) 
{
    if (getOption("getSymbols.warning4.0", TRUE)) {
        message(paste("    As of 0.4-0,", sQuote("getSymbols"), 
            "uses env=parent.frame() and\n", "auto.assign=TRUE by default.\n\n", 
            "This  behavior  will be  phased out in 0.5-0  when the call  will\n", 
            "default to use auto.assign=FALSE. getOption(\"getSymbols.env\") and \n", 
            "getOptions(\"getSymbols.auto.assign\") are now checked for alternate defaults\n\n", 
            "This message is shown once per session and may be disabled by setting \n", 
            "options(\"getSymbols.warning4.0\"=FALSE). See ?getSymbols for more details."))
        options(getSymbols.warning4.0 = FALSE)
    }
    importDefaults("getSymbols")
    if (missing(env) && !is.null(getOption("getSymbols.env"))) 
        env <- getOption("getSymbols.env")
    if (is.null(env)) 
        auto.assign <- FALSE
    if (!auto.assign && length(Symbols) > 1) 
        stop("must use auto.assign=TRUE for multiple Symbols requests")
    force(Symbols)
    if (symbol.lookup && missing(src)) {
        symbols.src <- getOption("getSymbols.sources")
    }
    else {
        symbols.src <- src[1]
    }
    if (is.character(Symbols)) {
        Symbols <- unlist(strsplit(Symbols, ";"))
        tmp.Symbols <- vector("list")
        for (each.symbol in Symbols) {
            if (each.symbol %in% names(symbols.src)) {
                tmp.src <- symbols.src[[each.symbol]]$src[1]
                if (is.null(tmp.src)) {
                  tmp.Symbols[[each.symbol]] <- src[1]
                }
                else {
                  tmp.Symbols[[each.symbol]] <- tmp.src
                }
            }
            else {
                tmp.Symbols[[each.symbol]] <- src[1]
            }
        }
        Symbols <- tmp.Symbols
    }
    old.Symbols <- NULL
    if (auto.assign && exists(".getSymbols", env, inherits = FALSE)) {
        old.Symbols <- get(".getSymbols", env)
    }
    if (reload.Symbols) {
        Symbols <- c(Symbols, old.Symbols)[unique(names(c(Symbols, 
            old.Symbols)))]
    }
    if (!auto.assign && length(Symbols) > 1) 
        stop("must use auto.assign=TRUE when reloading multiple Symbols")
    if (!is.null(Symbols)) {
        Symbols <- as.list(unlist(lapply(unique(as.character(Symbols)), 
            FUN = function(x) {
                Symbols[Symbols == x]
            })))
        all.symbols <- list()
        for (symbol.source in unique(as.character(Symbols))) {
            current.symbols <- names(Symbols[Symbols == symbol.source])
            symbols.returned <- do.call(paste("getSymbols.", 
                symbol.source, sep = ""), list(Symbols = current.symbols, 
                env = env, verbose = verbose, warnings = warnings, 
                auto.assign = auto.assign, ...))
            if (!auto.assign) 
                return(symbols.returned)
            for (each.symbol in symbols.returned) all.symbols[[each.symbol]] <- symbol.source
        }
        req.symbols <- names(all.symbols)
        all.symbols <- c(all.symbols, old.Symbols)[unique(names(c(all.symbols, 
            old.Symbols)))]
        if (auto.assign) {
            assign(".getSymbols", all.symbols, env)
            return(req.symbols)
        }
    }
    else {
        warning("no Symbols specified")
    }
}


importDefaults <- function (calling.fun = NULL) 
{
    sc <- sys.call(-1)
    if (is.null(calling.fun)) 
        calling.fun <- as.character(sc[[1]])
    if (is.function(calling.fun)) 
        calling.fun <- deparse(substitute(calling.fun))
    if (is.null(sc)) 
        stop("importDefaults is only valid inside a function call")
    if (as.character(sc[[1]]) != calling.fun) 
        return()
    all.defaults <- getDefaults(calling.fun)
    if (is.null(all.defaults)) 
        return()
    envir <- as.environment(-1)
    passed.args <- names(as.list(match.call(definition = eval(parse(text = calling.fun)), 
        call = as.call(sys.call(-1)))))[-1]
    formal.args <- names(formals(as.character(sys.call(-1))))
    default.args <- names(which(sapply(all.defaults, function(x) !is.null(x)) == 
        TRUE))
    for (arg in formal.args) {
        if (!arg %in% passed.args) {
            if (arg %in% default.args) {
                if (typeof(all.defaults[arg][[1]]) == "list") {
                  assign(arg, as.vector(all.defaults[arg][[1]]), 
                    envir = envir)
                }
                else if (typeof(all.defaults[arg][[1]]) %in% 
                  c("symbol", "language")) {
                  assign(arg, all.defaults[arg][[1]], envir = envir)
                }
                else if (typeof(all.defaults[arg][[1]]) == "character") {
                  if (length(all.defaults[arg][[1]]) == 1) {
                    assign(arg, eval(parse(text = all.defaults[arg][[1]])), 
                      envir = envir)
                  }
                  else {
                    assign(arg, as.character(parse(text = all.defaults[arg][[1]])), 
                      envir = envir)
                  }
                }
                else {
                  assign(arg, as.vector(unlist(all.defaults[arg][[1]])), 
                    envir = envir)
                }
            }
        }
    }
}


getSymbolLookup <- function (Symbols = NULL) 
{
    all.symbols <- getOption("getSymbols.sources")
    if (is.null(Symbols)) 
        Symbols <- names(all.symbols)
    all.symbols[Symbols]
}


is.OHLCV <- xts::is.OHLCV # re-exported from xts package

addCMF <- function (n = 20) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        cbind(Hi(x), Lo(x), Cl(x))
    }
    else stop("CMF only applicaple to HLC series")
    cmf <- CMF(xx, Vo(x), n = n)
    chobTA@TA.values <- cmf[lchob@xsubset]
    chobTA@name <- "chartCMF"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addCLV <- function (..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- HLC(x)
    x <- CLV(HLC = x)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^.*[(]", " Close Location Value (", 
        deparse(match.call()))
    gpars <- c(list(...), list(col = 5, type = "h"))[unique(names(c(list(col = 5, 
        type = "h"), list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    return(chobTA)
}


has.Bid <- function (x, which = FALSE) 
{
    colAttr <- attr(x, "Bid")
    if (!is.null(colAttr)) 
        return(if (which) colAttr else TRUE)
    loc <- grep("bid.*price", colnames(x), ignore.case = TRUE)
    if (identical(loc, integer(0))) 
        loc <- grep("bid", colnames(x), ignore.case = TRUE)
    if (!identical(loc, integer(0))) {
        return(if (which) loc else TRUE)
    }
    else FALSE
}


`.__T__summary:base` <- "<environment>"

.__C__tradeLog <- new("classRepresentation"
    , slots = structure(list(date = structure("Date", package = "methods"), 
    trade.id = structure("numeric", package = "methods"), action = structure("character", package = "methods"), 
    underlying = structure("character", package = "methods"), 
    price = structure("numeric", package = "methods"), quantity = structure("numeric", package = "methods"), 
    trade.value = structure("numeric", package = "methods"), 
    gain.loss = structure("numeric", package = "methods"), account.value = structure("numeric", package = "methods"), 
    currency = structure("character", package = "methods"), currency.symbol = structure("character", package = "methods"), 
    start.date = structure("Date", package = "methods"), exch = structure("character", package = "methods")), .Names = c("date", 
"trade.id", "action", "underlying", "price", "quantity", "trade.value", 
"gain.loss", "account.value", "currency", "currency.symbol", 
"start.date", "exch"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("tradeLog", package = "quantmod")
    , package = "quantmod"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Next <- function (x, k = 1) 
{
    UseMethod("Next")
}


getSymbols.SQLite <- function (Symbols, env, return.class = "xts", db.fields = c("row_names", 
    "Open", "High", "Low", "Close", "Volume", "Adjusted"), field.names = NULL, 
    dbname = NULL, POSIX = TRUE, ...) 
{
    importDefaults("getSymbols.SQLite")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    if (!requireNamespace("DBI", quietly = TRUE)) 
        stop("package:", dQuote("DBI"), "cannot be loaded.")
    if (!requireNamespace("RSQLite", quietly = TRUE)) 
        stop("package:", dQuote("RSQLite"), "cannot be loaded.")
    drv <- DBI::dbDriver("SQLite")
    con <- DBI::dbConnect(drv, dbname = dbname)
    db.Symbols <- DBI::dbListTables(con)
    if (length(Symbols) != sum(Symbols %in% db.Symbols)) {
        missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
        warning(paste("could not load symbol(s): ", paste(missing.db.symbol, 
            collapse = ", ")))
        Symbols <- Symbols[Symbols %in% db.Symbols]
    }
    for (i in 1:length(Symbols)) {
        if (verbose) {
            cat(paste("Loading ", Symbols[[i]], paste(rep(".", 
                10 - nchar(Symbols[[i]])), collapse = ""), sep = ""))
        }
        query <- paste("SELECT ", paste(db.fields, collapse = ","), 
            " FROM ", Symbols[[i]], " ORDER BY row_names")
        rs <- DBI::dbSendQuery(con, query)
        fr <- DBI::fetch(rs, n = -1)
        if (POSIX) {
            d <- as.numeric(fr[, 1])
            class(d) <- c("POSIXt", "POSIXct")
            fr <- xts(fr[, -1], order.by = d)
        }
        else {
            fr <- xts(fr[, -1], order.by = as.Date(as.numeric(fr[, 
                1]), origin = "1970-01-01"))
        }
        colnames(fr) <- paste(Symbols[[i]], c("Open", "High", 
            "Low", "Close", "Volume", "Adjusted"), sep = ".")
        fr <- convert.time.series(fr = fr, return.class = return.class)
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
        if (verbose) 
            cat("done\n")
    }
    DBI::dbDisconnect(con)
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


addMACD <- function (fast = 12, slow = 26, signal = 9, type = "EMA", histogram = TRUE, 
    col) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    col <- if (missing(col)) 
        col <- c("#999999", "#777777", "#BBBBBB", "#FF0000")
    xx <- if (is.OHLC(x)) {
        Cl(x)
    }
    else x
    macd <- MACD(xx, nFast = fast, nSlow = slow, nSig = signal, 
        maType = type)
    chobTA@TA.values <- macd[lchob@xsubset, ]
    chobTA@name <- "chartMACD"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        fast = fast, slow = slow, signal = signal, col = col, 
        histo = histogram)
    return(chobTA)
}


getSymbols.FRED <- function (Symbols, env, return.class = "xts", ...) 
{
    importDefaults("getSymbols.FRED")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    FRED.URL <- "https://research.stlouisfed.org/fred2/series"
    tmp <- tempfile()
    on.exit(unlink(tmp))
    for (i in 1:length(Symbols)) {
        if (verbose) 
            cat("downloading ", Symbols[[i]], ".....\n\n")
        URL <- paste(FRED.URL, "/", Symbols[[i]], "/downloaddata/", 
            Symbols[[i]], ".csv", sep = "")
        try.download.file(URL, destfile = tmp, quiet = !verbose, 
            ...)
        fr <- read.csv(tmp, na.string = ".")
        if (verbose) 
            cat("done.\n")
        fr <- xts(as.matrix(fr[, -1]), as.Date(fr[, 1], origin = "1970-01-01"), 
            src = "FRED", updated = Sys.time())
        dim(fr) <- c(NROW(fr), 1)
        colnames(fr) <- as.character(toupper(Symbols[[i]]))
        fr <- convert.time.series(fr = fr, return.class = return.class)
        Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


getQuote <- function (Symbols, src = "yahoo", what, ...) 
{
    args <- list(Symbols = Symbols, ...)
    if (!missing(what)) 
        args$what <- what
    do.call(paste("getQuote", src, sep = "."), args)
}


addWPR <- function (n = 14) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        cbind(Hi(x), Lo(x), Cl(x))
    }
    else if (is.null(dim(x))) {
        x
    }
    else {
        x[, 1]
    }
    wpr <- WPR(xx, n = n)
    chobTA@TA.values <- as.numeric(wpr)[lchob@xsubset]
    chobTA@name <- "chartWPR"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


getSymbols.mysql <- function (Symbols, env, return.class = "xts", db.fields = c("date", 
    "o", "h", "l", "c", "v", "a"), field.names = NULL, user = NULL, 
    password = NULL, dbname = NULL, host = "localhost", port = 3306, 
    ...) 
{
    importDefaults("getSymbols.MySQL")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    if (!requireNamespace("DBI", quietly = TRUE)) 
        stop("package:", dQuote("DBI"), "cannot be loaded.")
    if (!requireNamespace("RMySQL", quietly = TRUE)) 
        stop("package:", dQuote("RMySQL"), "cannot be loaded.")
    if (is.null(user) || is.null(password) || is.null(dbname)) {
        stop(paste("At least one connection argument (", sQuote("user"), 
            sQuote("password"), sQuote("dbname"), ") is not set"))
    }
    con <- DBI::dbConnect(RMySQL::MySQL(), user = user, password = password, 
        dbname = dbname, host = host, port = port)
    db.Symbols <- DBI::dbListTables(con)
    if (length(Symbols) != sum(Symbols %in% db.Symbols)) {
        missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
        warning(paste("could not load symbol(s): ", paste(missing.db.symbol, 
            collapse = ", ")))
        Symbols <- Symbols[Symbols %in% db.Symbols]
    }
    for (i in 1:length(Symbols)) {
        if (verbose) {
            cat(paste("Loading ", Symbols[[i]], paste(rep(".", 
                10 - nchar(Symbols[[i]])), collapse = ""), sep = ""))
        }
        query <- paste("SELECT ", paste(db.fields, collapse = ","), 
            " FROM ", Symbols[[i]], " ORDER BY date")
        rs <- DBI::dbSendQuery(con, query)
        fr <- DBI::fetch(rs, n = -1)
        fr <- xts(as.matrix(fr[, -1]), order.by = as.Date(fr[, 
            1], origin = "1970-01-01"), src = dbname, updated = Sys.time())
        colnames(fr) <- paste(Symbols[[i]], c("Open", "High", 
            "Low", "Close", "Volume", "Adjusted"), sep = ".")
        fr <- convert.time.series(fr = fr, return.class = return.class)
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
        if (verbose) 
            cat("done\n")
    }
    DBI::dbDisconnect(con)
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


Lo <- xts::Lo # re-exported from xts package

addLines <- function (x, h, v, on = 1, overlay = TRUE, col = "blue") 
{
    if (missing(x)) 
        x <- NULL
    if (missing(h)) 
        h <- NULL
    if (missing(v)) 
        v <- NULL
    lchob <- get.current.chob()
    chobTA <- new("chobTA")
    chobTA@new <- !overlay
    chobTA@TA.values <- NULL
    chobTA@name <- "chartLines"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        col = col, h = h, x = x, v = v)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addCMO <- function (n = 14) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (has.Cl(x)) {
        Cl(x)
    }
    else if (is.null(dim(x))) {
        x
    }
    else {
        x[, 1]
    }
    cmo <- CMO(xx, n = n)
    chobTA@TA.values <- cmo[lchob@xsubset]
    chobTA@name <- "chartCMO"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


saveSymbols <- function (Symbols = NULL, file.path = stop("must specify 'file.path'"), 
    env = parent.frame()) 
{
    if (exists(".getSymbols", env, inherits = FALSE)) {
        getSymbols <- get(".getSymbols", env, inherits = FALSE)
        if (is.null(Symbols)) {
            Symbols <- names(getSymbols)
        }
        else {
            Symbols <- Symbols[Symbols %in% names(getSymbols)]
        }
        for (each.symbol in Symbols) {
            save(list = each.symbol, file = paste(file.path, 
                "/", each.symbol, ".RData", sep = ""), envir = env)
        }
    }
}


addATR <- function (n = 14, maType = "EMA", ...) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    if (!is.OHLC(x)) 
        stop("only applicable to HLC series")
    atr <- ATR(cbind(Hi(x), Lo(x), Cl(x)), n = n, maType = maType, 
        ...)
    chobTA@TA.values <- atr[lchob@xsubset, ]
    chobTA@name <- "chartATR"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, maType = maType)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addMomentum <- function (n = 1) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        Cl(x)
    }
    else x
    mom <- momentum(xx, n = n)
    chobTA@TA.values <- mom[lchob@xsubset]
    chobTA@name <- "chartMomentum"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


`.__T__show:methods` <- "<environment>"

addEnvelope <- function (n = 20, p = 2.5, maType = "SMA", ..., on = 1) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- FALSE
    xx <- if (is.OHLC(x)) {
        Cl(x)
    }
    else x
    ma <- do.call(maType, list(xx, n = n, ...))
    mae <- cbind(ma * (1 - p/100), ma, ma * (1 + p/100))
    chobTA@TA.values <- mae[lchob@xsubset, ]
    chobTA@name <- "chartEnvelope"
    chobTA@call <- match.call()
    chobTA@on <- on
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, p = p, maType = maType)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


matchChart <- function (x, subset = NULL, type = "matchsticks", show.grid = TRUE, 
    name = deparse(substitute(x)), time.scale = NULL, log.scale = FALSE, 
    TA = "addVo()", theme = chartTheme("black"), major.ticks = "auto", 
    minor.ticks = TRUE, color.vol = TRUE, multi.col = FALSE, 
    ...) 
{
    do.call("chartSeries", list(x, subset = subset, name = name, 
        type = "matchsticks", show.grid = show.grid, time.scale = time.scale, 
        log.scale = log.scale, TA = substitute(TA), theme = theme, 
        major.ticks = major.ticks, minor.ticks = minor.ticks, 
        color.vol = color.vol, multi.col = multi.col, ...))
}


quarterlyReturn <- function (x, subset = NULL, type = "arithmetic", leading = TRUE, 
    ...) 
{
    periodReturn(x, "quarterly", subset, type, leading, ...)
}


moveTA <- function (ta, pos, occ = 1, dev) 
{
    pos <- pos - 1
    if (missing(ta)) 
        stop("no TA indicator specified")
    if (missing(dev)) 
        dev <- dev.cur()
    ta.list <- listTA(dev)
    lchob <- get.chob()[[dev]]
    if (regexpr("^add", ta) == -1) 
        ta <- paste("add", ta, sep = "")
    which.ta <- which(ta == sapply(ta.list, function(x) deparse(x[[1]])))[occ]
    if (is.na(which.ta)) 
        stop("no TA")
    lchob@passed.args$TA <- append(lchob@passed.args$TA[-which.ta], 
        lchob@passed.args$TA[which.ta], after = pos)
    do.call("chartSeries.chob", list(lchob))
    write.chob(lchob, lchob@device)
}


addTA <- function (ta, order = NULL, on = NA, legend = "auto", yrange = NULL, 
    ...) 
{
    if (is.character(ta)) {
        if (exists(ta)) {
            plot(do.call(paste("add", ta, sep = ""), list(...)))
        }
        else stop(paste("no TA method found for", paste("add", 
            ta, sep = "")))
    }
    else {
        lchob <- get.current.chob()
        chobTA <- new("chobTA")
        if (any(is.na(on))) {
            chobTA@new <- TRUE
        }
        else {
            chobTA@new <- FALSE
            chobTA@on <- on
        }
        nrc <- NROW(lchob@xdata)
        ta <- try.xts(ta, error = FALSE)
        if (is.xts(ta)) {
            x <- merge(lchob@xdata, ta, fill = ifelse(is.logical(ta), 
                0, NA), join = "left", retside = c(FALSE, TRUE))
        }
        else {
            if (NROW(ta) != nrc) 
                stop("non-xtsible data must match the length of the underlying series")
            x <- merge(lchob@xdata, ta, join = "left", retside = c(FALSE, 
                TRUE))
        }
        if (is.logical(ta)) 
            x <- as.logical(x, drop = FALSE)
        chobTA@TA.values <- coredata(x)[lchob@xsubset, ]
        chobTA@name <- "chartTA"
        chobTA@call <- match.call()
        chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
            colors = lchob@colors, spacing = lchob@spacing, width = lchob@width, 
            bp = lchob@bp, isLogical = is.logical(ta), x.labels = lchob@x.labels, 
            order = order, legend = legend, pars = list(list(...)), 
            time.scale = lchob@time.scale)
        return(chobTA)
    }
}


dropTA <- function (ta, occ = 1, dev, all = FALSE) 
{
    if (all) 
        return(do.call("dropTA", list(1:length(listTA()))))
    if (missing(ta)) 
        stop("no TA indicator specified")
    if (missing(dev)) 
        dev <- dev.cur()
    ta.list <- listTA(dev)
    lchob <- get.chob()[[dev]]
    sel.ta <- NULL
    for (cta in 1:length(ta)) {
        if (is.character(ta[cta])) {
            if (regexpr("^add", ta[cta]) == -1) 
                ta[cta] <- paste("add", ta[cta], sep = "")
            which.ta <- which(ta[cta] == sapply(ta.list, function(x) deparse(x[[1]])))[occ]
        }
        else which.ta <- cta
        if (!is.na(which.ta)) {
            if (lchob@passed.args$TA[[which.ta]]@new) 
                lchob@windows <- lchob@windows - 1
            sel.ta <- c(sel.ta, which.ta)
        }
    }
    if (is.null(sel.ta)) 
        stop("nothing to remove")
    lchob@passed.args$TA <- lchob@passed.args$TA[-sel.ta]
    if (length(lchob@passed.args$TA) < 1) 
        lchob@passed.args$TA <- list()
    do.call("chartSeries.chob", list(lchob))
    write.chob(lchob, lchob@device)
}


Ad <- xts::Ad # re-exported from xts package

has.Hi <- xts::has.Hi # re-exported from xts package

seriesHi <- function (x) 
{
    UseMethod("seriesHi")
}


is.BBO <- function (x) 
{
    if (all(has.Bid(x), has.Ask(x))) {
        TRUE
    }
    else FALSE
}


setTA <- function (type = c("chartSeries", "barChart", "candleChart")) 
{
    if ("chartSeries" %in% type) 
        setDefaults(chartSeries, TA = listTA())
    if ("barChart" %in% type) 
        setDefaults(barChart, TA = listTA())
    if ("candleChart" %in% type) 
        setDefaults(candleChart, TA = listTA())
}


candleChart <- function (x, subset = NULL, type = "candlesticks", show.grid = TRUE, 
    name = deparse(substitute(x)), time.scale = NULL, log.scale = FALSE, 
    TA = "addVo()", theme = chartTheme("black"), major.ticks = "auto", 
    minor.ticks = TRUE, color.vol = TRUE, multi.col = FALSE, 
    ...) 
{
    do.call("chartSeries", list(x, subset = subset, name = name, 
        type = "candlesticks", show.grid = show.grid, time.scale = time.scale, 
        log.scale = log.scale, TA = substitute(TA), theme = theme, 
        major.ticks = major.ticks, minor.ticks = minor.ticks, 
        color.vol = color.vol, multi.col = multi.col, ...))
}


saveSymbolLookup <- function (file, dir = "") 
{
    if (missing(file)) 
        file <- ".quantmod.SymbolLookup.rda"
    if (dir != "") {
        file <- file.path(dir, file)
    }
    lookup.list <- getSymbolLookup()
    save(lookup.list, file = file)
}


monthlyReturn <- function (x, subset = NULL, type = "arithmetic", leading = TRUE, 
    ...) 
{
    periodReturn(x, "monthly", subset, type, leading, ...)
}


.__C__quantmod <- new("classRepresentation"
    , slots = structure(list(model.id = structure("character", package = "methods"), 
    model.spec = structure("formula", package = "methods"), model.formula = structure("formula", package = "methods"), 
    model.target = structure("character", package = "methods"), 
    model.inputs = structure("character", package = "methods"), 
    build.inputs = structure("character", package = "methods"), 
    symbols = structure("character", package = "methods"), product = structure("character", package = "methods"), 
    price.levels = structure("ANY", package = "methods"), training.data = structure("ANY", package = "methods"), 
    build.date = structure("character", package = "methods"), 
    fitted.model = structure("ANY", package = "methods"), model.data = structure("ANY", package = "methods"), 
    quantmod.version = structure("numeric", package = "methods")), .Names = c("model.id", 
"model.spec", "model.formula", "model.target", "model.inputs", 
"build.inputs", "symbols", "product", "price.levels", "training.data", 
"build.date", "fitted.model", "model.data", "quantmod.version"
))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("quantmod", package = "quantmod")
    , package = "quantmod"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


getSymbols.MySQL <- function (Symbols, env, return.class = "xts", db.fields = c("date", 
    "o", "h", "l", "c", "v", "a"), field.names = NULL, user = NULL, 
    password = NULL, dbname = NULL, host = "localhost", port = 3306, 
    ...) 
{
    importDefaults("getSymbols.MySQL")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    if (!requireNamespace("DBI", quietly = TRUE)) 
        stop("package:", dQuote("DBI"), "cannot be loaded.")
    if (!requireNamespace("RMySQL", quietly = TRUE)) 
        stop("package:", dQuote("RMySQL"), "cannot be loaded.")
    if (is.null(user) || is.null(password) || is.null(dbname)) {
        stop(paste("At least one connection argument (", sQuote("user"), 
            sQuote("password"), sQuote("dbname"), ") is not set"))
    }
    con <- DBI::dbConnect(RMySQL::MySQL(), user = user, password = password, 
        dbname = dbname, host = host, port = port)
    db.Symbols <- DBI::dbListTables(con)
    if (length(Symbols) != sum(Symbols %in% db.Symbols)) {
        missing.db.symbol <- Symbols[!Symbols %in% db.Symbols]
        warning(paste("could not load symbol(s): ", paste(missing.db.symbol, 
            collapse = ", ")))
        Symbols <- Symbols[Symbols %in% db.Symbols]
    }
    for (i in 1:length(Symbols)) {
        if (verbose) {
            cat(paste("Loading ", Symbols[[i]], paste(rep(".", 
                10 - nchar(Symbols[[i]])), collapse = ""), sep = ""))
        }
        query <- paste("SELECT ", paste(db.fields, collapse = ","), 
            " FROM ", Symbols[[i]], " ORDER BY date")
        rs <- DBI::dbSendQuery(con, query)
        fr <- DBI::fetch(rs, n = -1)
        fr <- xts(as.matrix(fr[, -1]), order.by = as.Date(fr[, 
            1], origin = "1970-01-01"), src = dbname, updated = Sys.time())
        colnames(fr) <- paste(Symbols[[i]], c("Open", "High", 
            "Low", "Close", "Volume", "Adjusted"), sep = ".")
        fr <- convert.time.series(fr = fr, return.class = return.class)
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
        if (verbose) 
            cat("done\n")
    }
    DBI::dbDisconnect(con)
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


addTDI <- function (n = 20, multiple = 2, ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- Cl(x)
    x <- TDI(price = x, n = n, multiple = multiple)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^addTDI", "Trend Detection Index ", 
        deparse(match.call()))
    gpars <- c(list(...), list(col = 5:6))[unique(names(c(list(col = 5:6), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


addAroonOsc <- function (n = 20, ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- cbind(Hi(x), Lo(x))
    x <- aroon(HL = x, n = n)[, 3]
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^addAroonOsc", "Aroon Oscillator ", 
        deparse(match.call()))
    gpars <- c(list(...), list(col = 3:4))[unique(names(c(list(col = 3:4), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


OpLo <- function (x) 
{
    xx <- Delt(Op(x), Lo(x))
    colnames(xx) <- paste("OpLo", deparse(substitute(x)), sep = ".")
    xx
}


has.HLC <- xts::has.HLC # re-exported from xts package

getFX <- function (Currencies, from = Sys.Date() - 499, to = Sys.Date(), 
    env = parent.frame(), verbose = FALSE, warning = TRUE, auto.assign = TRUE, 
    ...) 
{
    importDefaults("getFX")
    if (missing(env)) 
        env <- parent.frame(1)
    if (is.null(env)) 
        auto.assign <- FALSE
    if (!auto.assign && length(Currencies) > 1) 
        stop("must use auto.assign=TRUE for multiple currency requests")
    getSymbols.oanda(Symbols = Currencies, from = from, to = to, 
        env = env, verbose = verbose, warning = warning, auto.assign = auto.assign, 
        ...)
}


zooom <- function (n = 1, eps = 2) 
{
    for (i in 1:n) {
        cat("select left and right extremes by clicking the chart\n")
        points <- locator(2)
        if (abs(diff(points$x)) < eps) {
            zoomChart()
        }
        else {
            usr <- par("usr")
            xdata <- get.chob()[[2]]@xdata
            xsubset <- get.chob()[[2]]@xsubset
            sq <- floor(seq(usr[1], usr[2], 1))
            st <- which(floor(points$x[1]) == sq)/length(sq) * 
                NROW(xdata[xsubset])
            en <- which(floor(points$x[2]) == sq)/length(sq) * 
                NROW(xdata[xsubset])
            sorted <- sort(c(st, en))
            st <- sorted[1]
            en <- sorted[2] * 1.05
            zoomChart(paste(index(xdata[xsubset])[max(1, floor(st), 
                na.rm = TRUE)], index(xdata[xsubset])[min(ceiling(en), 
                NROW(xdata[xsubset]), na.rm = TRUE)], sep = "::"))
        }
    }
    cat("done\n")
}


has.OHLCV <- xts::has.OHLCV # re-exported from xts package

Delt <- function (x1, x2 = NULL, k = 0, type = c("arithmetic", "log")) 
{
    x1 <- try.xts(x1, error = FALSE)
    type <- match.arg(type[1], c("log", "arithmetic"))
    if (length(x2) != length(x1) && !is.null(x2)) 
        stop("x1 and x2 must be of same length")
    if (is.null(x2)) {
        x2 <- x1
        if (length(k) < 2) {
            k <- max(1, k)
        }
    }
    dim(x2) <- NULL
    if (type == "log") {
        xx <- lapply(k, function(K.) {
            log(unclass(x2)/Lag(x1, K.))
        })
    }
    else {
        xx <- lapply(k, function(K.) {
            unclass(x2)/Lag(x1, K.) - 1
        })
    }
    xx <- do.call("cbind", xx)
    colnames(xx) <- paste("Delt", k, type, sep = ".")
    reclass(xx, x1)
}


tradeModel <- function (x, signal.threshold = c(0, 0), leverage = 1, return.model = TRUE, 
    plot.model = FALSE, trade.dates = NULL, exclude.training = TRUE, 
    ret.type = c("weeks", "months", "quarters", "years"), ...) 
{
    trade.offset = 0
    quantmod <- getModelData(x)
    if (class(quantmod) != "quantmod") 
        stop("model must be of class quantmod")
    if (!is.null(trade.dates) & length(trade.dates) < 2) 
        stop("trade.dates must be of length 2")
    model.data <- modelData(quantmod, trade.dates, exclude.training = exclude.training)
    fitted.zoo <- predictModel(quantmod@fitted.model, model.data, 
        ...)
    if (class(fitted.zoo) != "zoo") {
        fitted.zoo <- zoo(as.vector(fitted.zoo), index(model.data))
    }
    signal.zoo <- ifelse(fitted.zoo < signal.threshold[1] | fitted.zoo > 
        signal.threshold[2], ifelse(fitted.zoo > 0, 1, -1), 0)
    tmp.index <- index(signal.zoo)[-(1 + trade.offset)]
    market.zoo <- model.data[-(nrow(model.data) + trade.offset), 
        1]
    signal.zoo <- signal.zoo[-c(length(index(signal.zoo)) - trade.offset, 
        length(index(signal.zoo)))]
    signal.zoo = merge(market.zoo, signal.zoo)
    index(signal.zoo) <- tmp.index
    quantmodResults <- list(model = quantmod, signal = signal.zoo)
    model.returns <- modelReturn(quantmodResults, trade.dates = trade.dates, 
        leverage = leverage, ret.type = ret.type)
    quantmodResults$return <- model.returns
    quantmodResults$model <- stripModelData(quantmodResults$model)
    return(structure(quantmodResults, class = "quantmodResults"))
}


addCCI <- function (n = 20, maType = "SMA", c = 0.015) 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    chobTA <- new("chobTA")
    chobTA@new <- TRUE
    xx <- if (is.OHLC(x)) {
        cbind(Hi(x), Lo(x), Cl(x))
    }
    else x
    cci <- CCI(xx, n = n, maType = maType, c = c)
    chobTA@TA.values <- cci[lchob@xsubset]
    chobTA@name <- "chartCCI"
    chobTA@call <- match.call()
    chobTA@params <- list(xrange = lchob@xrange, colors = lchob@colors, 
        color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        n = n, maType = maType, c = c)
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


buildData <- function (formula, na.rm = TRUE, return.class = "zoo") 
{
    if (is.quantmod(formula)) {
        fr <- modelData(formula)
    }
    else {
        fr <- modelData(specifyModel(formula, na.rm = na.rm))
    }
    fr <- convert.time.series(fr = fr, return.class = return.class)
}


zoom_Chart <- function (subset) 
{
    chob <- current.chob()
    chob$subset(subset)
    chob
}


as.quantmod.OHLC <- function (x, col.names = c("Open", "High", "Low", "Close", "Volume", 
    "Adjusted"), name = NULL, ...) 
{
    if (ncol(x) != length(col.names)) 
        stop("'col.names' must match number of columns of 'x'")
    UseMethod("as.quantmod.OHLC")
}


addVolatility <- function (n = 10, calc = "close", N = 260, ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- OHLC(x)
    x <- volatility(OHLC = x, n = n, calc = calc, N = N)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^add", "", deparse(match.call()))
    gpars <- c(list(...), list(col = 8))[unique(names(c(list(col = 8), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


axTicksByValue <- function (x, match.to = c(1e+08, 1e+07, 1e+06, 1e+05, 10000, 
    1000, 500, 300, 200, 150, 100, 50, 20, 10, 5, 2, 1, 0.5, 
    0.25, 0.2, 0.1, 0.05, 0.02, 0.01), lt = 20, gt = 3, secondary = FALSE) 
{
    x <- na.omit(x)
    diff_range <- diff(range(x))
    if (diff_range > 1) 
        diff_range <- diff(range(x%/%1))
    by <- match.to[which(diff_range%/%match.to > gt & diff_range%/%match.to < 
        lt)[1]]
    if (is.na(by)) {
        by <- 1L
    }
    ticks1 <- do.call("seq.int", as.list(c(range(x)[1]%/%by * 
        by, range(x)[2]%/%by * by, by)))
    ticks1
}


flushSymbols <- function (DB = DDB_Yahoo()) 
{
    pos = match(DB$name, search())
    detach(pos = pos)
    attachSymbols(DB = DB, pos = pos)
}


is.TBBO <- function (x) 
{
    if (all(has.Trade(x), has.Qty(x), has.Bid(x), has.Ask(x))) {
        TRUE
    }
    else FALSE
}


addChVol <- function (n = 10, maType, ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    x <- chaikinVolatility(HL = HLC(x)[, -3], n = n, maType = maType)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^.*[(]", " Chaikin Volatility (", deparse(match.call()))
    gpars <- c(list(...), list(col = 8))[unique(names(c(list(col = 8), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


has.Vo <- xts::has.Vo # re-exported from xts package

oanda.currencies <- structure(list(oanda.df.1.length.oanda.df...2....1. = c("US Dollar", 
"Afghanistan Afghani", "Albanian Lek", "Algerian Dinar", "Andorran Franc", 
"Andorran Peseta", "Angolan Kwanza", "Angolan New Kwanza", "Argentine Peso", 
"Armenian Dram", "Aruban Florin", "Australian Dollar", "Austrian Schilling", 
"Azerbaijan Manat", "Azerbaijan New Manat", "Bahamian Dollar", 
"Bahraini Dinar", "Bangladeshi Taka", "Barbados Dollar", "Belarusian Ruble", 
"Belgian Franc", "Belize Dollar", "Bermudian Dollar", "Bhutan Ngultrum", 
"Bolivian Boliviano", "Bosnian Mark", "Botswana Pula", "Brazilian Real", 
"British Pound", "Brunei Dollar", "Bulgarian Lev", "Burundi Franc", 
"CFA Franc BCEAO", "CFA Franc BEAC", "CFP Franc", "Cambodian Riel", 
"Canadian Dollar", "Cape Verde Escudo", "Cayman Islands Dollar", 
"Chilean Peso", "Chinese Yuan Renminbi", "Colombian Peso", "Comoros Franc", 
"Congolese Franc", "Costa Rican Colon", "Croatian Kuna", "Cuban Convertible Peso", 
"Cuban Peso", "Cyprus Pound", "Czech Koruna", "Danish Krone", 
"Djibouti Franc", "Dominican R. Peso", "Dutch Guilder", "ECU", 
"East Caribbean Dollar", "Ecuador Sucre", "Egyptian Pound", "El Salvador Colon", 
"Estonian Kroon", "Ethiopian Birr", "Euro", "Falkland Islands Pound", 
"Fiji Dollar", "Finnish Markka", "French Franc", "Gambian Dalasi", 
"Georgian Lari", "German Mark", "Ghanaian Cedi", "Ghanaian New Cedi", 
"Gibraltar Pound", "Gold (oz.)", "Greek Drachma", "Guatemalan Quetzal", 
"Guinea Franc", "Guyanese Dollar", "Haitian Gourde", "Honduran Lempira", 
"Hong Kong Dollar", "Hungarian Forint", "Iceland Krona", "Indian Rupee", 
"Indonesian Rupiah", "Iranian Rial", "Iraqi Dinar", "Irish Punt", 
"Israeli New Shekel", "Italian Lira", "Jamaican Dollar", "Japanese Yen", 
"Jordanian Dinar", "Kazakhstan Tenge", "Kenyan Shilling", "Kuwaiti Dinar", 
"Kyrgyzstanian Som", "Lao Kip", "Latvian Lats", "Lebanese Pound", 
"Lesotho Loti", "Liberian Dollar", "Libyan Dinar", "Lithuanian Litas", 
"Luxembourg Franc", "Macau Pataca", "Macedonian Denar", "Malagasy Ariary", 
"Malagasy Franc", "Malawi Kwacha", "Malaysian Ringgit", "Maldive Rufiyaa", 
"Maltese Lira", "Mauritanian Ouguiya", "Mauritius Rupee", "Mexican Peso", 
"Moldovan Leu", "Mongolian Tugrik", "Moroccan Dirham", "Mozambique Metical", 
"Mozambique New Metical", "Myanmar Kyat", "NL Antillian Guilder", 
"Namibia Dollar", "Nepalese Rupee", "New Zealand Dollar", "Nicaraguan Cordoba Oro", 
"Nigerian Naira", "North Korean Won", "Norwegian Kroner", "Omani Rial", 
"Pakistan Rupee", "Palladium (oz.)", "Panamanian Balboa", "Papua New Guinea Kina", 
"Paraguay Guarani", "Peruvian Nuevo Sol", "Philippine Peso", 
"Platinum (oz.)", "Polish Zloty", "Portuguese Escudo", "Qatari Rial", 
"Romanian Lei", "Romanian New Lei", "Russian Rouble", "Rwandan Franc", 
"Samoan Tala", "Sao Tome/Principe Dobra", "Saudi Riyal", "Serbian Dinar", 
"Seychelles Rupee", "Sierra Leone Leone", "Silver (oz.)", "Singapore Dollar", 
"Slovak Koruna", "Slovenian Tolar", "Solomon Islands Dollar", 
"Somali Shilling", "South African Rand", "South-Korean Won", 
"Spanish Peseta", "Sri Lanka Rupee", "St. Helena Pound", "Sudanese Dinar", 
"Sudanese Old Pound", "Sudanese Pound", "Suriname Dollar", "Suriname Guilder", 
"Swaziland Lilangeni", "Swedish Krona", "Swiss Franc", "Syrian Pound", 
"Taiwan Dollar", "Tanzanian Shilling", "Thai Baht", "Tonga Pa'anga", 
"Trinidad/Tobago Dollar", "Tunisian Dinar", "Turkish Lira", "Turkish New Lira", 
"Turkmenistan Manat", "Uganda Shilling", "Ukraine Hryvnia", "Uruguayan Peso", 
"Utd. Arab Emir. Dirham", "Vanuatu Vatu", "Venezuelan Bolivar", 
"Vietnamese Dong", "Yemeni Rial", "Yugoslav Dinar", "Zambian Kwacha", 
"Zimbabwe Dollar")), .Names = "oanda.df.1.length.oanda.df...2....1.", row.names = c("USD", 
"AFN", "ALL", "DZD", "ADF", "ADP", "AOA", "AON", "ARS", "AMD", 
"AWG", "AUD", "ATS", "AZM", "AZN", "BSD", "BHD", "BDT", "BBD", 
"BYR", "BEF", "BZD", "BMD", "BTN", "BOB", "BAM", "BWP", "BRL", 
"GBP", "BND", "BGN", "BIF", "XOF", "XAF", "XPF", "KHR", "CAD", 
"CVE", "KYD", "CLP", "CNY", "COP", "KMF", "CDF", "CRC", "HRK", 
"CUC", "CUP", "CYP", "CZK", "DKK", "DJF", "DOP", "NLG", "XEU", 
"XCD", "ECS", "EGP", "SVC", "EEK", "ETB", "EUR", "FKP", "FJD", 
"FIM", "FRF", "GMD", "GEL", "DEM", "GHC", "GHS", "GIP", "XAU", 
"GRD", "GTQ", "GNF", "GYD", "HTG", "HNL", "HKD", "HUF", "ISK", 
"INR", "IDR", "IRR", "IQD", "IEP", "ILS", "ITL", "JMD", "JPY", 
"JOD", "KZT", "KES", "KWD", "KGS", "LAK", "LVL", "LBP", "LSL", 
"LRD", "LYD", "LTL", "LUF", "MOP", "MKD", "MGA", "MGF", "MWK", 
"MYR", "MVR", "MTL", "MRO", "MUR", "MXN", "MDL", "MNT", "MAD", 
"MZM", "MZN", "MMK", "ANG", "NAD", "NPR", "NZD", "NIO", "NGN", 
"KPW", "NOK", "OMR", "PKR", "XPD", "PAB", "PGK", "PYG", "PEN", 
"PHP", "XPT", "PLN", "PTE", "QAR", "ROL", "RON", "RUB", "RWF", 
"WST", "STD", "SAR", "RSD", "SCR", "SLL", "XAG", "SGD", "SKK", 
"SIT", "SBD", "SOS", "ZAR", "KRW", "ESP", "LKR", "SHP", "SDD", 
"SDP", "SDG", "SRD", "SRG", "SZL", "SEK", "CHF", "SYP", "TWD", 
"TZS", "THB", "TOP", "TTD", "TND", "TRL", "TRY", "TMM", "UGX", 
"UAH", "UYU", "AED", "VUV", "VEB", "VND", "YER", "YUN", "ZMK", 
"ZWD"), class = "data.frame")


buildModel <- function (x, method, training.per, ...) 
{
    as.POSIXorDate <- function(x) {
        class.of.index <- class(index(model.data))
        if ("POSIXt" %in% class.of.index) {
            if ("POSIXlt" %in% class.of.index) {
                x <- as.POSIXlt(x)
            }
            else {
                x <- as.POSIXct(x)
            }
        }
        else {
            x <- as.Date(x)
        }
        x
    }
    model.id = deparse(substitute(x))
    if (length(training.per) != 2) 
        stop("training.per must be of length 2")
    model.data <- x@model.data
    start.date.index <- index(model.data[which(index(model.data) >= 
        as.POSIXorDate(training.per[1]))])
    end.date.index <- index(model.data[which(index(model.data) <= 
        as.POSIXorDate(training.per[2]))])
    training.dates <- as.POSIXorDate(intersect(as.character(start.date.index), 
        as.character(end.date.index)))
    method <- as.character(paste("buildModel.", method, sep = ""))
    training.data <- model.data[training.dates]
    formula <- x@model.formula
    mcall <- do.call(method, list(quantmod = x, training.data = training.data, 
        ...))
    x@fitted.model <- mcall$fitted
    x@model.inputs <- as.character(mcall$inputs)
    x@build.date = as.character(Sys.time())
    x@model.id <- paste(class(mcall$fitted)[length(class(mcall$fitted))], 
        as.numeric(Sys.time()), sep = "")
    x@training.data <- (training.dates)
    invisible(x)
}


.chart.theme <- structure(list(white = structure(list(fg.col = "#000000", bg.col = "#F0F0F0", 
    grid.col = "#CCCCCC", border = "#444444", minor.tick = "#888888", 
    major.tick = "#000000", up.col = "#00CC00", dn.col = "#FF7700", 
    dn.up.col = "#888888", up.up.col = "#FFFFFF", dn.dn.col = "#FF0000", 
    up.dn.col = "#000000", up.border = "#444444", dn.border = "#444444", 
    dn.up.border = "#444444", up.up.border = "#444444", dn.dn.border = "#444444", 
    up.dn.border = "#444444", main.col = "#555555", sub.col = "#555555", 
    area = "#FFFFFF", fill = "#F7F7F7", Expiry = "#C9C9C9", theme.name = "white"), .Names = c("fg.col", 
"bg.col", "grid.col", "border", "minor.tick", "major.tick", "up.col", 
"dn.col", "dn.up.col", "up.up.col", "dn.dn.col", "up.dn.col", 
"up.border", "dn.border", "dn.up.border", "up.up.border", "dn.dn.border", 
"up.dn.border", "main.col", "sub.col", "area", "fill", "Expiry", 
"theme.name")), white.mono = structure(list(fg.col = "#666666", 
    bg.col = "#FFFFFF", grid.col = "#CCCCCC", border = "#666666", 
    minor.tick = "#CCCCCC", major.tick = "#888888", up.col = "#000000", 
    dn.col = "#000000", dn.up.col = "#888888", up.up.col = "#FFFFFF", 
    dn.dn.col = "#4D4D4D", up.dn.col = "#000000", up.border = "#666666", 
    dn.border = "#666666", dn.up.border = "#666666", up.up.border = "#666666", 
    dn.dn.border = "#666666", up.dn.border = "#666666", main.col = "#555555", 
    sub.col = "#555555", fill = "#F7F7F7", Expiry = "#C9C9C9", 
    BBands.col = "#666666", BBands.fill = "#F7F7F7", BBands = structure(list(
        col = "#666666", fill = "#F7F7F7"), .Names = c("col", 
    "fill")), theme.name = "white.mono"), .Names = c("fg.col", 
"bg.col", "grid.col", "border", "minor.tick", "major.tick", "up.col", 
"dn.col", "dn.up.col", "up.up.col", "dn.dn.col", "up.dn.col", 
"up.border", "dn.border", "dn.up.border", "up.up.border", "dn.dn.border", 
"up.dn.border", "main.col", "sub.col", "fill", "Expiry", "BBands.col", 
"BBands.fill", "BBands", "theme.name")), black = structure(list(
    fg.col = "#666666", bg.col = "#222222", grid.col = "#303030", 
    border = "#666666", minor.tick = "#303030", major.tick = "#AAAAAA", 
    up.col = "#00FF00", dn.col = "#FF9900", dn.up.col = "#888888", 
    up.up.col = "#FFFFFF", dn.dn.col = "#FF0000", up.dn.col = "#000000", 
    up.border = "#666666", dn.border = "#666666", dn.up.border = "#666666", 
    up.up.border = "#666666", dn.dn.border = "#666666", up.dn.border = "#666666", 
    main.col = "#999999", sub.col = "#999999", area = "#252525", 
    fill = "#282828", Expiry = "#383838", BBands.col = "red", 
    BBands.fill = "#282828", BBands = structure(list(col = "red", 
        fill = "#282828"), .Names = c("col", "fill")), theme.name = "black"), .Names = c("fg.col", 
"bg.col", "grid.col", "border", "minor.tick", "major.tick", "up.col", 
"dn.col", "dn.up.col", "up.up.col", "dn.dn.col", "up.dn.col", 
"up.border", "dn.border", "dn.up.border", "up.up.border", "dn.dn.border", 
"up.dn.border", "main.col", "sub.col", "area", "fill", "Expiry", 
"BBands.col", "BBands.fill", "BBands", "theme.name")), black.mono = structure(list(
    fg.col = "#666666", bg.col = "#222222", grid.col = "#303030", 
    border = "#666666", minor.tick = "#303030", major.tick = "#AAAAAA", 
    up.col = "#FFFFFF", dn.col = "#FFFFFF", dn.up.col = "#888888", 
    up.up.col = "#FFFFFF", dn.dn.col = "#4D4D4D", up.dn.col = "#000000", 
    up.border = "#666666", dn.border = "#666666", dn.up.border = "#666666", 
    up.up.border = "#666666", dn.dn.border = "#666666", up.dn.border = "#666666", 
    main.col = "#999999", sub.col = "#999999", fill = "#777777", 
    Expiry = "#383838", BBands = structure(list(col = "#DDDDDD", 
        fill = "#777777"), .Names = c("col", "fill")), BBands.col = "#DDDDDD", 
    BBands.fill = "#777777", theme.name = "black.mono"), .Names = c("fg.col", 
"bg.col", "grid.col", "border", "minor.tick", "major.tick", "up.col", 
"dn.col", "dn.up.col", "up.up.col", "dn.dn.col", "up.dn.col", 
"up.border", "dn.border", "dn.up.border", "up.up.border", "dn.dn.border", 
"up.dn.border", "main.col", "sub.col", "fill", "Expiry", "BBands", 
"BBands.col", "BBands.fill", "theme.name")), beige = structure(list(
    fg.col = "#888888", bg.col = "#F5F5D0", grid.col = "#CCCCCC", 
    border = "#666666", minor.tick = "#CCCCCC", major.tick = "#AAAAAA", 
    up.col = "#00FF00", dn.col = "#AA0000", dn.up.col = "#888888", 
    up.up.col = "#FFFFFF", dn.dn.col = "#FF0000", up.dn.col = "#000000", 
    up.border = "#666666", dn.border = "#666666", dn.up.border = "#666666", 
    up.up.border = "#666666", dn.dn.border = "#666666", up.dn.border = "#666666", 
    main.col = "#555555", sub.col = "#555555", fill = "#F5F5F5", 
    Expiry = "#C9C9C9", BBands.col = "orange", BBands.fill = "#F5F5DF", 
    BBands = structure(list(col = "orange", fill = "#F5F5DF"), .Names = c("col", 
    "fill")), theme.name = "beige"), .Names = c("fg.col", "bg.col", 
"grid.col", "border", "minor.tick", "major.tick", "up.col", "dn.col", 
"dn.up.col", "up.up.col", "dn.dn.col", "up.dn.col", "up.border", 
"dn.border", "dn.up.border", "up.up.border", "dn.dn.border", 
"up.dn.border", "main.col", "sub.col", "fill", "Expiry", "BBands.col", 
"BBands.fill", "BBands", "theme.name")), wsj = structure(list(
    fg.col = "#000000", bg.col = "#F0F0F0", grid.col = "#ffffff", 
    border = "#444444", minor.tick = "#888888", major.tick = "#000000", 
    up.col = "#FFFFFF", dn.col = "#666666", dn.up.col = "#888888", 
    up.up.col = "#FFFFFF", dn.dn.col = "#FF0000", up.dn.col = "#000000", 
    up.border = "#444444", dn.border = "#666666", dn.up.border = "#444444", 
    up.up.border = "#444444", dn.dn.border = "#444444", up.dn.border = "#444444", 
    main.col = "#555555", sub.col = "#555555", area = "#d3d0af", 
    fill = "#F7F7F7", Expiry = "#C9C9C9", theme.name = "wsj"), .Names = c("fg.col", 
"bg.col", "grid.col", "border", "minor.tick", "major.tick", "up.col", 
"dn.col", "dn.up.col", "up.up.col", "dn.dn.col", "up.dn.col", 
"up.border", "dn.border", "dn.up.border", "up.up.border", "dn.dn.border", 
"up.dn.border", "main.col", "sub.col", "area", "fill", "Expiry", 
"theme.name"))), .Names = c("white", "white.mono", "black", "black.mono", 
"beige", "wsj"), class = "chart.theme")


add_axis <- function (side, at = NULL, labels = TRUE, tick = TRUE, line = NA, 
    pos = NA, font = NA, col = NULL) 
{
    lenv <- new.env()
    lenv$plot_axis <- function(x, side, at, labels, tick, font, 
        pos, col) {
        xdata <- x$Env$xdata
        if (is.OHLC(xdata)) 
            xdata <- OHLC(xdata)
        xsubset <- x$Env$xsubset
        nr <- NROW(x$Env$xdata[x$Env$xsubset])
        if (is.logical(labels) && labels == TRUE) {
            labels <- pretty(xdata[xsubset])
            dropped_label <- which(labels < min(xdata[xsubset], 
                na.rm = TRUE))
            labels <- labels[-dropped_label]
        }
        if (is.null(at)) 
            at <- labels
        if (side == 2) {
            nr <- 0
        }
        text(nr, at, labels, col = col, cex = 0.9, font = font, 
            pos = pos, xpd = TRUE)
        if (tick) 
            segments(nr - (1/8 * max(strwidth(labels))), at, 
                nr + (1/8 * max(strwidth(labels))), at)
    }
    if (missing(pos)) 
        pos <- side
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(side = side, at = at, labels = labels, font = font, 
        tick = tick, pos = pos, col = col)), list(side = side, 
        at = at, labels = labels, font = font, tick = tick, pos = pos, 
        col = col))
    exp <- parse(text = gsub("list", "plot_axis", as.expression(substitute(list(x = current.chob(), 
        side = side, at = get("at"), labels = get("labels"), 
        tick = tick, font = font, pos = pos, col = col)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- plot_object$Env$xdata
    plot_object$set_frame(2)
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


has.Trade <- function (x, which = FALSE) 
{
    colAttr <- attr(x, "Trade")
    if (!is.null(colAttr)) 
        return(if (which) colAttr else TRUE)
    loc <- grep("trade", colnames(x), ignore.case = TRUE)
    if (!identical(loc, integer(0))) {
        return(if (which) loc else TRUE)
    }
    else FALSE
}


add_VMA <- function (w, ratio = 1, on = 1, col = "green", ...) 
{
    lenv <- new.env()
    lenv$add_wma <- function(x, w, ratio, col, ...) {
        xdata <- x$Env$xdata
        xsubset <- x$Env$xsubset
        vma <- VMA(Cl(xdata), w = w, ratio = ratio)[xsubset]
        lines(1:NROW(xdata[xsubset]), vma, col = col, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(w = w, ratio = ratio, col = col, ...)), list(w = w, 
        ratio = ratio, col = col, ...))
    exp <- parse(text = gsub("list", "add_wma", as.expression(substitute(list(x = current.chob(), 
        w = w, ratio = ratio, col = col, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- VMA(Cl(plot_object$Env$xdata), w = w, ratio = ratio)
    plot_object$set_frame(sign(on) * (abs(on) + 1L))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


seriesDecr <- function (x, thresh = 0, diff. = 1L) 
{
    diff(x, diff = diff., na.pad = TRUE) < thresh
}


Op <- xts::Op # re-exported from xts package

ClCl <- function (x) 
{
    xx <- Delt(Cl(x))
    colnames(xx) <- paste("ClCl", deparse(substitute(x)), sep = ".")
    xx
}


getSymbols.google <- function (Symbols, env, return.class = "xts", from = "2007-01-01", 
    to = Sys.Date(), ...) 
{
    fix.google.bug <- TRUE
    importDefaults("getSymbols.google")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    google.URL <- "http://finance.google.com/finance/historical?"
    from.y <- as.numeric(strsplit(as.character(from), "-", )[[1]][1])
    from.m <- as.numeric(strsplit(as.character(from), "-", )[[1]][2])
    from.d <- as.numeric(strsplit(as.character(from), "-", )[[1]][3])
    to.y <- as.numeric(strsplit(as.character(to), "-", )[[1]][1])
    to.m <- as.numeric(strsplit(as.character(to), "-", )[[1]][2])
    to.d <- as.numeric(strsplit(as.character(to), "-", )[[1]][3])
    tmp <- tempfile()
    on.exit(unlink(tmp))
    for (i in 1:length(Symbols)) {
        Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
        Symbols.name <- ifelse(is.null(Symbols.name), Symbols[[i]], 
            Symbols.name)
        if (verbose) 
            cat("downloading ", Symbols.name, ".....\n\n")
        download.file(paste(google.URL, "q=", Symbols.name, "&startdate=", 
            month.abb[from.m], "+", sprintf("%.2d", from.d), 
            ",+", from.y, "&enddate=", month.abb[to.m], "+", 
            sprintf("%.2d", to.d), ",+", to.y, "&output=csv", 
            sep = ""), destfile = tmp, quiet = !verbose)
        fr <- read.csv(tmp)
        if (verbose) 
            cat("done.\n")
        fr <- fr[nrow(fr):1, ]
        if (fix.google.bug) {
            bad.dates <- c("29-Dec-04", "30-Dec-04", "31-Dec-04")
            if (as.Date(from, origin = "1970-01-01") < as.Date("2003-12-28", 
                origin = "1970-01-01") && as.Date(to, origin = "1970-01-01") > 
                as.Date("2003-12-30", origin = "1970-01-01")) {
                dup.dates <- which(fr[, 1] %in% bad.dates)[(1:3)]
                fr <- fr[-dup.dates, ]
                warning("google duplicate bug - missing Dec 28,29,30 of 2003")
            }
        }
        fr <- xts(as.matrix(fr[, -1]), as.Date(strptime(fr[, 
            1], "%d-%B-%y"), origin = "1970-01-01"), src = "google", 
            updated = Sys.time())
        colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols.name)), 
            c("Open", "High", "Low", "Close", "Volume"), sep = ".")
        suppressWarnings(storage.mode(fr) <- "numeric")
        fr <- convert.time.series(fr = fr, return.class = return.class)
        Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


dailyReturn <- function (x, subset = NULL, type = "arithmetic", leading = TRUE, 
    ...) 
{
    periodReturn(x, "daily", subset, type, leading, ...)
}


Cl <- xts::Cl # re-exported from xts package

.quantmodEnv <- "<environment>"

quantmodenv <- function () 
as.environment(".quantmodEnv")


.chob <- "<environment>"

addMFI <- function (n = 14, ..., on = NA, legend = "auto") 
{
    lchob <- get.current.chob()
    x <- as.matrix(lchob@xdata)
    volume <- Vo(x)
    x <- HLC(x)
    x <- MFI(HLC = x, volume = volume, n = n)
    yrange <- NULL
    chobTA <- new("chobTA")
    if (NCOL(x) == 1) {
        chobTA@TA.values <- x[lchob@xsubset]
    }
    else chobTA@TA.values <- x[lchob@xsubset, ]
    chobTA@name <- "chartTA"
    if (any(is.na(on))) {
        chobTA@new <- TRUE
    }
    else {
        chobTA@new <- FALSE
        chobTA@on <- on
    }
    chobTA@call <- match.call()
    legend.name <- gsub("^addMFI", "Money Flow Index ", deparse(match.call()))
    gpars <- c(list(...), list(col = 8))[unique(names(c(list(col = 8), 
        list(...))))]
    chobTA@params <- list(xrange = lchob@xrange, yrange = yrange, 
        colors = lchob@colors, color.vol = lchob@color.vol, multi.col = lchob@multi.col, 
        spacing = lchob@spacing, width = lchob@width, bp = lchob@bp, 
        x.labels = lchob@x.labels, time.scale = lchob@time.scale, 
        isLogical = is.logical(x), legend = legend, legend.name = legend.name, 
        pars = list(gpars))
    if (is.null(sys.call(-1))) {
        TA <- lchob@passed.args$TA
        lchob@passed.args$TA <- c(TA, chobTA)
        lchob@windows <- lchob@windows + ifelse(chobTA@new, 1, 
            0)
        do.call("chartSeries.chob", list(lchob))
        invisible(chobTA)
    }
    else {
        return(chobTA)
    }
}


getFin <- function (Symbol, env = parent.frame(), src = "google", auto.assign = TRUE, 
    ...) 
{
    if (missing(env)) 
        env <- parent.frame(1)
    if (is.null(env)) 
        auto.assign <- FALSE
    Symbol <- strsplit(Symbol, ";")[[1]]
    if (length(Symbol) > 1) 
        return(unlist(lapply(Symbol, getFin, env = env, src = src, 
            auto.assign = auto.assign)))
    Symbol.name <- Symbol
    google.fin <- "http://finance.google.com/finance?fstype=ii&q="
    tmp <- tempfile()
    on.exit(unlink(tmp))
    download.file(paste(google.fin, Symbol, sep = ""), quiet = TRUE, 
        destfile = tmp)
    Symbol <- readLines(tmp, warn = FALSE)
    thead <- grep("thead", Symbol)
    tbody <- grep("tbody", Symbol)
    c1 <- lapply(seq(1, 11, 2), function(x) Symbol[thead[x]:thead[x + 
        1]])
    c2 <- lapply(c1, gsub, pattern = "<.*?>", replacement = "")
    cnames <- lapply(c2, function(x) x[-which(x == "")][-1])
    d1 <- lapply(seq(1, 11, 2), function(x) {
        Symbol[tbody[x]:tbody[x + 1]]
    })
    d2 <- lapply(d1, gsub, pattern = "<.*?>", replacement = "", 
        perl = TRUE)
    d3 <- lapply(d2, function(x) x[-which(x == "")])
    fnames <- lapply(d3, function(x) {
        gsub("&amp;", "&", x[grep("[A-Za-z]", x)])
    })
    vals <- lapply(d3, function(x) {
        as.numeric(gsub(",", "", gsub("^-$", NA, x[-grep("[A-Za-z]", 
            x)])))
    })
    make_col_names <- function(name) {
        substr(name, nchar(name) - 9, nchar(name))
    }
    fin <- lapply(1:6, function(x) {
        structure(matrix(vals[[x]], nrow = length(fnames[[x]]), 
            byrow = TRUE), .Dimnames = list(fnames[[x]], make_col_names(cnames[[x]])), 
            col_desc = cnames[[x]])
    })
    fin <- list(IS = list(Q = fin[[1]], A = fin[[2]]), BS = list(Q = fin[[3]], 
        A = fin[[4]]), CF = list(Q = fin[[5]], A = fin[[6]]))
    if (auto.assign) {
        assign(paste(gsub(":", ".", Symbol.name), "f", sep = "."), 
            structure(fin, symbol = Symbol.name, class = "financials", 
                src = "google", updated = Sys.time()), env)
        return(paste(gsub(":", ".", Symbol.name), "f", sep = "."))
    }
    else {
        return(structure(fin, symbol = Symbol.name, class = "financials", 
            src = "google", updated = Sys.time()))
    }
}


getSymbols.rda <- function (Symbols, env, dir = "", return.class = "xts", extension = "rda", 
    col.names = c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
    ...) 
{
    importDefaults("getSymbols.rda")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    default.return.class <- return.class
    default.dir <- dir
    default.extension <- extension
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    for (i in 1:length(Symbols)) {
        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class), default.return.class, 
            return.class)
        dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
        dir <- ifelse(is.null(dir), default.dir, dir)
        extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
        extension <- ifelse(is.null(extension), default.extension, 
            extension)
        if (verbose) 
            cat("loading ", Symbols[[i]], ".....")
        if (dir == "") {
            sym.file <- paste(Symbols[[i]], extension, sep = ".")
        }
        else {
            sym.file <- file.path(dir, paste(Symbols[[i]], extension, 
                sep = "."))
        }
        if (!file.exists(sym.file)) {
            cat("\nfile ", paste(Symbols[[i]], extension, sep = "."), 
                " does not exist ", "in ", dir, "....skipping\n")
            next
        }
        local.name <- load(sym.file)
        assign("fr", get(local.name))
        if (verbose) 
            cat("done.\n")
        if (!is.xts(fr)) 
            fr <- xts(fr[, -1], as.Date(fr[, 1], origin = "1970-01-01"), 
                src = "rda", updated = Sys.time())
        colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols[[i]])), 
            col.names, sep = ".")
        fr <- convert.time.series(fr = fr, return.class = return.class)
        Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

getSymbols.yahoo <- function (Symbols, env, return.class = "xts", index.class = "Date", 
    from = "2007-01-01", to = Sys.Date(), ...) 
{
    importDefaults("getSymbols.yahoo")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    if (!hasArg(adjust)) 
        adjust <- FALSE
    default.return.class <- return.class
    default.from <- from
    default.to <- to
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    yahoo.URL <- "http://ichart.finance.yahoo.com/table.csv?"
    tmp <- tempfile()
    on.exit(unlink(tmp))
    for (i in 1:length(Symbols)) {
        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class), default.return.class, 
            return.class)
        from <- getSymbolLookup()[[Symbols[[i]]]]$from
        from <- if (is.null(from)) 
            default.from
        else from
        to <- getSymbolLookup()[[Symbols[[i]]]]$to
        to <- if (is.null(to)) 
            default.to
        else to
        from.y <- as.numeric(strsplit(as.character(as.Date(from, 
            origin = "1970-01-01")), "-", )[[1]][1])
        from.m <- as.numeric(strsplit(as.character(as.Date(from, 
            origin = "1970-01-01")), "-", )[[1]][2]) - 1
        from.d <- as.numeric(strsplit(as.character(as.Date(from, 
            origin = "1970-01-01")), "-", )[[1]][3])
        to.y <- as.numeric(strsplit(as.character(as.Date(to, 
            origin = "1970-01-01")), "-", )[[1]][1])
        to.m <- as.numeric(strsplit(as.character(as.Date(to, 
            origin = "1970-01-01")), "-", )[[1]][2]) - 1
        to.d <- as.numeric(strsplit(as.character(as.Date(to, 
            origin = "1970-01-01")), "-", )[[1]][3])
        Symbols.name <- getSymbolLookup()[[Symbols[[i]]]]$name
        Symbols.name <- ifelse(is.null(Symbols.name), Symbols[[i]], 
            Symbols.name)
        if (verbose) 
            cat("downloading ", Symbols.name, ".....\n\n")
        download.file(paste(yahoo.URL, "s=", Symbols.name, "&a=", 
            from.m, "&b=", sprintf("%.2d", from.d), "&c=", from.y, 
            "&d=", to.m, "&e=", sprintf("%.2d", to.d), "&f=", 
            to.y, "&g=d&q=q&y=0", "&z=", Symbols.name, "&x=.csv", 
            sep = ""), destfile = tmp, quiet = !verbose)
        fr <- read.csv(tmp)
        if (verbose) 
            cat("done.\n")
        fr <- xts(as.matrix(fr[, -1]), as.Date(fr[, 1]), src = "yahoo", 
            updated = Sys.time())
        colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols.name)), 
            c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
            sep = ".")
        if (adjust) {
            fr <- adjustOHLC(fr, symbol.name = Symbols.name)
        }
        fr <- convert.time.series(fr = fr, return.class = return.class)
        if (is.xts(fr)) 
            indexClass(fr) <- index.class
        Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
        if (i >= 5 && length(Symbols) > 5) {
            message("pausing 1 second between requests for more than 5 symbols")
            Sys.sleep(1)
        }
    }
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


getSymbols.csv <- function (Symbols, env, dir = "", return.class = "xts", extension = "csv", 
    col.names = c("Open", "High", "Low", "Close", "Volume", "Adjusted"), 
    ...) 
{
    importDefaults("getSymbols.csv")
    this.env <- environment()
    for (var in names(list(...))) {
        assign(var, list(...)[[var]], this.env)
    }
    default.return.class <- return.class
    default.dir <- dir
    default.extension <- extension
    if (!hasArg(verbose)) 
        verbose <- FALSE
    if (!hasArg(auto.assign)) 
        auto.assign <- TRUE
    for (i in 1:length(Symbols)) {
        return.class <- getSymbolLookup()[[Symbols[[i]]]]$return.class
        return.class <- ifelse(is.null(return.class), default.return.class, 
            return.class)
        dir <- getSymbolLookup()[[Symbols[[i]]]]$dir
        dir <- ifelse(is.null(dir), default.dir, dir)
        extension <- getSymbolLookup()[[Symbols[[i]]]]$extension
        extension <- ifelse(is.null(extension), default.extension, 
            extension)
        if (verbose) 
            cat("loading ", Symbols[[i]], ".....")
        if (dir == "") {
            sym.file <- paste(Symbols[[i]], extension, sep = ".")
        }
        else {
            sym.file <- file.path(dir, paste(Symbols[[i]], extension, 
                sep = "."))
        }
        if (!file.exists(sym.file)) {
            cat("\nfile ", paste(Symbols[[i]], "csv", sep = "."), 
                " does not exist ", "in ", dir, "....skipping\n")
            next
        }
        fr <- read.csv(sym.file)
        if (verbose) 
            cat("done.\n")
        asDateArgs <- list(x = as.character(fr[, 1]))
        if (hasArg("format")) 
            asDateArgs$format <- format
        if (!is.null(getSymbolLookup()[[Symbols[[i]]]]$format)) 
            asDateArgs$format <- getSymbolLookup()[[Symbols[[i]]]]$format
        fr <- xts(fr[, -1], do.call("as.Date", asDateArgs), src = "csv", 
            updated = Sys.time())
        colnames(fr) <- paste(toupper(gsub("\\^", "", Symbols[[i]])), 
            col.names, sep = ".")
        fr <- convert.time.series(fr = fr, return.class = return.class)
        Symbols[[i]] <- toupper(gsub("\\^", "", Symbols[[i]]))
        if (auto.assign) 
            assign(Symbols[[i]], fr, env)
    }
    if (auto.assign) 
        return(Symbols)
    return(fr)
}


add_VWAP <- function (n = 10, on = 1, col = "darkgrey", ...) 
{
    lenv <- new.env()
    lenv$add_vwap <- function(x, n, col, ...) {
        xdata <- x$Env$xdata
        xvo <- x$Env$vo
        xsubset <- x$Env$xsubset
        vwap <- VWAP(Cl(xdata), xvo, n = n)[xsubset]
        lines(1:NROW(xdata[xsubset]), vwap, col = col, ...)
    }
    mapply(function(name, value) {
        assign(name, value, envir = lenv)
    }, names(list(n = n, col = col, ...)), list(n = n, col = col, 
        ...))
    exp <- parse(text = gsub("list", "add_vwap", as.expression(substitute(list(x = current.chob(), 
        n = n, col = col, ...)))), srcfile = NULL)
    plot_object <- current.chob()
    lenv$xdata <- VWAP(Cl(plot_object$Env$xdata), plot_object$Env$vo, 
        n = n)
    plot_object$set_frame(sign(on) * (abs(on) + 1L))
    plot_object$add(exp, env = c(lenv, plot_object$Env), expr = TRUE)
    plot_object
}


viewFinancials <- function (x, type = c("BS", "IS", "CF"), period = c("A", "Q"), 
    subset = NULL) 
{
    if (!inherits(x, "financials")) 
        stop(paste(sQuote("x"), "must be of type", sQuote("financials")))
    type <- match.arg(toupper(type[1]), c("BS", "IS", "CF"))
    period <- match.arg(toupper(period[1]), c("A", "Q"))
    statements <- list(BS = "Balance Sheet", IS = "Income Statement", 
        CF = "Cash Flow Statement", A = "Annual", Q = "Quarterly")
    if (is.null(subset)) {
        message(paste(statements[[period]], statements[[type]], 
            "for", attr(x, "symbol")))
        return(x[[type]][[period]])
    }
    else {
        tmp.table <- as.matrix(as.xts(t(x[[type]][[period]]), 
            dateFormat = "Date")[subset])
        dn1 <- rownames(tmp.table)
        dn2 <- colnames(tmp.table)
        tmp.table <- t(tmp.table)[, NROW(tmp.table):1]
        if (is.null(dim(tmp.table))) {
            dim(tmp.table) <- c(NROW(tmp.table), 1)
            dimnames(tmp.table) <- list(dn2, dn1)
        }
        message(paste(statements[[period]], statements[[type]], 
            "for", attr(x, "symbol")))
        return(tmp.table)
    }
}


newTA <- function (FUN, preFUN, postFUN, on = NA, yrange = NULL, legend.name, 
    fdots = TRUE, cdots = TRUE, data.at = 1, ...) 
{
    if (is.character(FUN)) {
        if (exists(FUN) && is.function(get(FUN))) {
            FUN.name <- FUN
            FUN <- get(FUN)
        }
    }
    else if (is.function(FUN)) {
        FUN.name <- deparse(substitute(FUN))
    }
    else stop("FUN required to be a function object")
    funToFun <- function(x, fun.name, drop.arg = 1, dots = TRUE) {
        drop.arg <- if (any(drop.arg < 1)) {
            1:length(formals(x))
        }
        else -drop.arg
        fnames <- names(formals(x))
        if (!dots && ("..." %in% fnames)) 
            fnames <- fnames[-which("..." == fnames)]
        fun.args <- paste(fnames, "=", c("x", fnames[drop.arg]), 
            sep = "")
        fun.args <- paste(gsub("=\\.\\.\\.", "", fun.args), collapse = ",")
        paste(fun.name, "(", fun.args, ")", collapse = "", sep = "")
    }
    .formals <- formals(FUN)[-data.at]
    .body <- deparse(body(skeleton.TA))
    gpars <- list(...)
    if (!missing(legend.name) && is.character(legend.name)) {
        .body[22] <- paste("legend.name <- gsub('^.*[(]',", paste("'", 
            legend.name, "('"), ",deparse(match.call()))")
    }
    if (missing(fdots) && !("..." %in% .formals)) 
        fdots <- FALSE
    if (fdots) 
        cdots <- TRUE
    if (!cdots) {
        .formals <- .formals[-which("..." == names(.formals))]
        .body[23] <- paste("gpars <-", list(gpars))
    }
    else {
        if (!"..." %in% names(.formals)) {
            .formals <- c(.formals, alist("..." = ))
        }
        .body[23] <- paste("gpars <- c(list(...),", list(gpars), 
            ")[unique(names(c(", list(gpars), ",list(...))))]")
    }
    .formals <- eval(parse(text = paste("c(.formals,alist(on=", 
        on, ", legend=\"auto\"))")))
    if (!missing(preFUN)) {
        if (is.character(preFUN)) {
            if (exists(preFUN) && is.function(get(preFUN))) {
                preFUN <- preFUN
            }
        }
        else if (is.function(preFUN)) {
            preFUN <- deparse(substitute(preFUN))
        }
        else stop("preFUN required to be a function object")
        .body[4] <- paste("x <-", preFUN, "(x)", sep = "")
    }
    else .body[4] <- "preFUN <- \"\""
    if (!missing(postFUN)) {
        if (is.character(postFUN)) {
            if (exists(postFUN) && is.function(get(postFUN))) {
                postFUN <- postFUN
            }
        }
        else if (is.function(postFUN)) {
            postFUN <- deparse(substitute(postFUN))
        }
        else stop("postFUN required to be a function object")
        .body[6] <- paste("x <-", postFUN, "(x)", sep = "")
    }
    else .body[6] <- "postFUN <- \"\""
    if (!is.null(yrange)) {
        .body[7] <- paste("yrange <-", deparse(yrange))
    }
    .body[5] <- paste("x <-", funToFun(FUN, FUN.name, data.at, 
        dots = fdots))
    if (.body[6] == "postFUN <- \"\"") 
        .body[6] <- ""
    if (.body[4] == "preFUN <- \"\"") 
        .body[4] <- ""
    as.function(c(.formals, as.call(parse(text = .body))[[1]]), 
        envir = asNamespace("quantmod"))
}


HiCl <- function (x) 
{
    xx <- Delt(Hi(x), Cl(x))
    colnames(xx) <- paste("HiCl", deparse(substitute(x)), sep = ".")
    xx
}


valley <- function (x) 
{
    .Deprecated("findValleys", package = "quantmod")
    findValleys(x)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Quantitative Financial Modelling Framework"

.skeleton_package_version = "0.4-7"

.skeleton_package_depends = "xts,zoo,TTR,methods"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF