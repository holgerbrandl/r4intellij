##
## Exported symobls in package `graphics`
##

## Exported package methods

assocplot <- function (x, col = c("black", "red"), space = 0.3, main = NULL, 
    xlab = NULL, ylab = NULL) 
{
    if (length(dim(x)) != 2L) 
        stop("'x' must be a 2-d contingency table")
    if (any(x < 0) || anyNA(x)) 
        stop("all entries of 'x' must be nonnegative and finite")
    if ((n <- sum(x)) == 0L) 
        stop("at least one entry of 'x' must be positive")
    if (length(col) != 2L) 
        stop("incorrect 'col': must be length 2")
    f <- x[, rev(1L:NCOL(x))]
    e <- outer(rowSums(f), colSums(f), "*")/n
    d <- (f - e)/sqrt(e)
    e <- sqrt(e)
    x.w <- apply(e, 1L, max)
    y.h <- apply(d, 2L, max) - apply(d, 2L, min)
    x.delta <- mean(x.w) * space
    y.delta <- mean(y.h) * space
    xlim <- c(0, sum(x.w) + NROW(f) * x.delta)
    ylim <- c(0, sum(y.h) + NCOL(f) * y.delta)
    dev.hold()
    on.exit(dev.flush())
    plot.new()
    plot.window(xlim, ylim, log = "")
    x.r <- cumsum(x.w + x.delta)
    x.m <- (c(0, x.r[-NROW(f)]) + x.r)/2
    y.u <- cumsum(y.h + y.delta)
    y.m <- y.u - apply(pmax(d, 0), 2L, max) - y.delta/2
    z <- expand.grid(x.m, y.m)
    rect(z[, 1] - e/2, z[, 2], z[, 1] + e/2, z[, 2] + d, col = col[1 + 
        (d < 0)])
    axis(1, at = x.m, labels = rownames(f), tick = FALSE)
    axis(2, at = y.m, labels = colnames(f), tick = FALSE)
    abline(h = y.m, lty = 2)
    ndn <- names(dimnames(f))
    if (length(ndn) == 2L) {
        if (is.null(xlab)) 
            xlab <- ndn[1L]
        if (is.null(ylab)) 
            ylab <- ndn[2L]
    }
    title(main = main, xlab = xlab, ylab = ylab)
}


title <- function (main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    line = NA, outer = FALSE, ...) 
{
    main <- as.graphicsAnnot(main)
    sub <- as.graphicsAnnot(sub)
    xlab <- as.graphicsAnnot(xlab)
    ylab <- as.graphicsAnnot(ylab)
    .External.graphics(C_title, main, sub, xlab, ylab, line, 
        outer, ...)
    invisible()
}


axis.Date <- function (side, x, at, format, labels = TRUE, ...) 
{
    mat <- missing(at) || is.null(at)
    if (!mat) 
        x <- as.Date(at)
    else x <- as.Date(x)
    range <- par("usr")[if (side%%2) 
        1L:2L
    else 3:4L]
    range[1L] <- ceiling(range[1L])
    range[2L] <- floor(range[2L])
    d <- range[2L] - range[1L]
    z <- c(range, x[is.finite(x)])
    class(z) <- "Date"
    if (d < 7) 
        if (missing(format)) 
            format <- "%a"
    if (d < 100) {
        z <- structure(pretty(z), class = "Date")
        if (missing(format)) 
            format <- "%b %d"
    }
    else if (d < 1.1 * 365) {
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        m <- rep.int(zz$year[1L], m)
        zz$year <- c(m, m + 1)
        z <- as.Date(zz)
        if (missing(format)) 
            format <- "%b"
    }
    else {
        zz <- as.POSIXlt(z)
        zz$mday <- 1
        zz$mon <- 0
        zz$year <- pretty(zz$year)
        z <- as.Date(zz)
        if (missing(format)) 
            format <- "%Y"
    }
    if (!mat) 
        z <- x[is.finite(x)]
    keep <- z >= range[1L] & z <= range[2L]
    z <- z[keep]
    z <- sort(unique(z))
    class(z) <- "Date"
    if (!is.logical(labels)) 
        labels <- labels[keep]
    else if (identical(labels, TRUE)) 
        labels <- format.Date(z, format = format)
    else if (identical(labels, FALSE)) 
        labels <- rep("", length(z))
    axis(side, at = z, labels = labels, ...)
}


points <- function (x, ...) 
UseMethod("points")


pairs.default <- function (x, labels, panel = points, ..., horInd = 1:nc, verInd = 1:nc, 
    lower.panel = panel, upper.panel = panel, diag.panel = NULL, 
    text.panel = textPanel, label.pos = 0.5 + has.diag/3, line.main = 3, 
    cex.labels = NULL, font.labels = 1, row1attop = TRUE, gap = 1, 
    log = "") 
{
    if (doText <- missing(text.panel) || is.function(text.panel)) 
        textPanel <- function(x = 0.5, y = 0.5, txt, cex, font) text(x, 
            y, txt, cex = cex, font = font)
    localAxis <- function(side, x, y, xpd, bg, col = NULL, main, 
        oma, ...) {
        xpd <- NA
        if (side%%2L == 1L && xl[j]) 
            xpd <- FALSE
        if (side%%2L == 0L && yl[i]) 
            xpd <- FALSE
        if (side%%2L == 1L) 
            Axis(x, side = side, xpd = xpd, ...)
        else Axis(y, side = side, xpd = xpd, ...)
    }
    localPlot <- function(..., main, oma, font.main, cex.main) plot(...)
    localLowerPanel <- function(..., main, oma, font.main, cex.main) lower.panel(...)
    localUpperPanel <- function(..., main, oma, font.main, cex.main) upper.panel(...)
    localDiagPanel <- function(..., main, oma, font.main, cex.main) diag.panel(...)
    dots <- list(...)
    nmdots <- names(dots)
    if (!is.matrix(x)) {
        x <- as.data.frame(x)
        for (i in seq_along(names(x))) {
            if (is.factor(x[[i]]) || is.logical(x[[i]])) 
                x[[i]] <- as.numeric(x[[i]])
            if (!is.numeric(unclass(x[[i]]))) 
                stop("non-numeric argument to 'pairs'")
        }
    }
    else if (!is.numeric(x)) 
        stop("non-numeric argument to 'pairs'")
    panel <- match.fun(panel)
    if ((has.lower <- !is.null(lower.panel)) && !missing(lower.panel)) 
        lower.panel <- match.fun(lower.panel)
    if ((has.upper <- !is.null(upper.panel)) && !missing(upper.panel)) 
        upper.panel <- match.fun(upper.panel)
    if ((has.diag <- !is.null(diag.panel)) && !missing(diag.panel)) 
        diag.panel <- match.fun(diag.panel)
    if (row1attop) {
        tmp <- lower.panel
        lower.panel <- upper.panel
        upper.panel <- tmp
        tmp <- has.lower
        has.lower <- has.upper
        has.upper <- tmp
    }
    nc <- ncol(x)
    if (nc < 2L) 
        stop("only one column in the argument to 'pairs'")
    if (!all(horInd >= 1L && horInd <= nc)) 
        stop("invalid argument 'horInd'")
    if (!all(verInd >= 1L && verInd <= nc)) 
        stop("invalid argument 'verInd'")
    if (doText) {
        if (missing(labels)) {
            labels <- colnames(x)
            if (is.null(labels)) 
                labels <- paste("var", 1L:nc)
        }
        else if (is.null(labels)) 
            doText <- FALSE
    }
    oma <- if ("oma" %in% nmdots) 
        dots$oma
    main <- if ("main" %in% nmdots) 
        dots$main
    if (is.null(oma)) 
        oma <- c(4, 4, if (!is.null(main)) 6 else 4, 4)
    opar <- par(mfrow = c(length(horInd), length(verInd)), mar = rep.int(gap/2, 
        4), oma = oma)
    on.exit(par(opar))
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    xl <- yl <- logical(nc)
    if (is.numeric(log)) 
        xl[log] <- yl[log] <- TRUE
    else {
        xl[] <- grepl("x", log)
        yl[] <- grepl("y", log)
    }
    for (i in if (row1attop) 
        verInd
    else rev(verInd)) for (j in horInd) {
        l <- paste0(ifelse(xl[j], "x", ""), ifelse(yl[i], "y", 
            ""))
        localPlot(x[, j], x[, i], xlab = "", ylab = "", axes = FALSE, 
            type = "n", ..., log = l)
        if (i == j || (i < j && has.lower) || (i > j && has.upper)) {
            box()
            if (i == 1 && (!(j%%2L) || !has.upper || !has.lower)) 
                localAxis(1L + 2L * row1attop, x[, j], x[, i], 
                  ...)
            if (i == nc && (j%%2L || !has.upper || !has.lower)) 
                localAxis(3L - 2L * row1attop, x[, j], x[, i], 
                  ...)
            if (j == 1 && (!(i%%2L) || !has.upper || !has.lower)) 
                localAxis(2L, x[, j], x[, i], ...)
            if (j == nc && (i%%2L || !has.upper || !has.lower)) 
                localAxis(4L, x[, j], x[, i], ...)
            mfg <- par("mfg")
            if (i == j) {
                if (has.diag) 
                  localDiagPanel(as.vector(x[, i]), ...)
                if (doText) {
                  par(usr = c(0, 1, 0, 1))
                  if (is.null(cex.labels)) {
                    l.wid <- strwidth(labels, "user")
                    cex.labels <- max(0.8, min(2, 0.9/max(l.wid)))
                  }
                  xlp <- if (xl[i]) 
                    10^0.5
                  else 0.5
                  ylp <- if (yl[j]) 
                    10^label.pos
                  else label.pos
                  text.panel(xlp, ylp, labels[i], cex = cex.labels, 
                    font = font.labels)
                }
            }
            else if (i < j) 
                localLowerPanel(as.vector(x[, j]), as.vector(x[, 
                  i]), ...)
            else localUpperPanel(as.vector(x[, j]), as.vector(x[, 
                i]), ...)
            if (any(par("mfg") != mfg)) 
                stop("the 'panel' function made a new plot")
        }
        else par(new = FALSE)
    }
    if (!is.null(main)) {
        font.main <- if ("font.main" %in% nmdots) 
            dots$font.main
        else par("font.main")
        cex.main <- if ("cex.main" %in% nmdots) 
            dots$cex.main
        else par("cex.main")
        mtext(main, 3, line.main, outer = TRUE, at = 0.5, cex = cex.main, 
            font = font.main)
    }
    invisible(NULL)
}


strheight <- function (s, units = "user", cex = NULL, font = NULL, vfont = NULL, 
    ...) 
{
    if (!is.null(vfont)) 
        vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface), 
            fontindex = pmatch(vfont[2L], Hershey$fontindex))
    .External.graphics(C_strHeight, as.graphicsAnnot(s), pmatch(units, 
        c("user", "figure", "inches")), cex, font, vfont, ...)
}


rect <- function (xleft, ybottom, xright, ytop, density = NULL, angle = 45, 
    col = NA, border = NULL, lty = par("lty"), lwd = par("lwd"), 
    ...) 
{
    if (is.numeric(density) && all(is.na(density) | density < 
        0)) 
        density <- NULL
    if (!is.null(density) && !is.null(angle)) {
        if (is.logical(border) && !is.na(border)) {
            if (border) 
                border <- col
            else border <- NA
        }
        n <- range(length(xleft), length(xright), length(ybottom), 
            length(ytop))
        if (n[1L] == 0) 
            stop("invalid rectangle specification")
        n <- n[2L]
        x <- rbind(rep.int(NA, n), xleft, xright, xright, xleft)[-1L]
        y <- rbind(rep.int(NA, n), ybottom, ybottom, ytop, ytop)[-1L]
        polygon(x, y, col = col, border = border, lty = lty, 
            lwd = lwd, density = density, angle = angle, ...)
    }
    else .External.graphics(C_rect, as.double(xleft), as.double(ybottom), 
        as.double(xright), as.double(ytop), col = col, border = border, 
        lty = lty, lwd = lwd, ...)
    invisible()
}


frame <- function () 
{
    for (fun in getHook("before.plot.new")) {
        if (is.character(fun)) 
            fun <- get(fun)
        try(fun())
    }
    .External2(C_plot_new)
    grDevices:::recordPalette()
    for (fun in getHook("plot.new")) {
        if (is.character(fun)) 
            fun <- get(fun)
        try(fun())
    }
    invisible()
}


identify <- function (x, ...) 
UseMethod("identify")


plot.xy <- function (xy, type, pch = par("pch"), lty = par("lty"), col = par("col"), 
    bg = NA, cex = 1, lwd = par("lwd"), ...) 
invisible(.External.graphics(C_plotXY, xy, type, pch, lty, col, 
    bg, cex, lwd, ...))


stem <- function (x, scale = 1, width = 80, atom = 1e-08) 
{
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    x <- x[is.finite(x)]
    n <- as.integer(length(x))
    if (is.na(n)) 
        stop("invalid length(x)")
    if (n == 0) 
        stop("no finite and non-missing values")
    if (scale <= 0) 
        stop("'scale' must be positive")
    .Call(C_StemLeaf, as.double(x), scale, width, atom)
    invisible(NULL)
}


par <- function (..., no.readonly = FALSE) 
{
    .Pars.readonly <- c("cin", "cra", "csi", "cxy", "din", "page")
    single <- FALSE
    args <- list(...)
    if (!length(args)) 
        args <- as.list(if (no.readonly) 
            .Pars[-match(.Pars.readonly, .Pars)]
        else .Pars)
    else {
        if (all(unlist(lapply(args, is.character)))) 
            args <- as.list(unlist(args))
        if (length(args) == 1) {
            if (is.list(args[[1L]]) | is.null(args[[1L]])) 
                args <- args[[1L]]
            else if (is.null(names(args))) 
                single <- TRUE
        }
    }
    value <- .External2(C_par, args)
    if (single) 
        value <- value[[1L]]
    if (!is.null(names(args))) 
        invisible(value)
    else value
}


plot.default <- function (x, y = NULL, type = "p", xlim = NULL, ylim = NULL, 
    log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL, 
    ann = par("ann"), axes = TRUE, frame.plot = axes, panel.first = NULL, 
    panel.last = NULL, asp = NA, ...) 
{
    localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
    localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
    localWindow <- function(..., col, bg, pch, cex, lty, lwd) plot.window(...)
    localTitle <- function(..., col, bg, pch, cex, lty, lwd) title(...)
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    ylabel <- if (!missing(y)) 
        deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel, log)
    xlab <- if (is.null(xlab)) 
        xy$xlab
    else xlab
    ylab <- if (is.null(ylab)) 
        xy$ylab
    else ylab
    xlim <- if (is.null(xlim)) 
        range(xy$x[is.finite(xy$x)])
    else xlim
    ylim <- if (is.null(ylim)) 
        range(xy$y[is.finite(xy$y)])
    else ylim
    dev.hold()
    on.exit(dev.flush())
    plot.new()
    localWindow(xlim, ylim, log, asp, ...)
    panel.first
    plot.xy(xy, type, ...)
    panel.last
    if (axes) {
        localAxis(if (is.null(y)) 
            xy$x
        else x, side = 1, ...)
        localAxis(if (is.null(y)) 
            x
        else y, side = 2, ...)
    }
    if (frame.plot) 
        localBox(...)
    if (ann) 
        localTitle(main = main, sub = sub, xlab = xlab, ylab = ylab, 
            ...)
    invisible()
}


fourfoldplot <- function (x, color = c("#99CCFF", "#6699CC"), conf.level = 0.95, 
    std = c("margins", "ind.max", "all.max"), margin = c(1, 2), 
    space = 0.2, main = NULL, mfrow = NULL, mfcol = NULL) 
{
    if (!is.array(x)) 
        stop("'x' must be an array")
    if (length(dim(x)) == 2L) {
        x <- if (is.null(dimnames(x))) 
            array(x, c(dim(x), 1L))
        else array(x, c(dim(x), 1L), c(dimnames(x), list(NULL)))
    }
    if (length(dim(x)) != 3L) 
        stop("'x' must be 2- or 3-dimensional")
    if (any(dim(x)[1L:2L] != 2L)) 
        stop("table for each stratum must be 2 by 2")
    dnx <- dimnames(x)
    if (is.null(dnx)) 
        dnx <- vector("list", 3L)
    for (i in which(sapply(dnx, is.null))) dnx[[i]] <- LETTERS[seq_len(dim(x)[i])]
    if (is.null(names(dnx))) 
        i <- 1L:3L
    else i <- which(is.null(names(dnx)))
    if (any(i)) 
        names(dnx)[i] <- c("Row", "Col", "Strata")[i]
    dimnames(x) <- dnx
    k <- dim(x)[3L]
    if (!((length(conf.level) == 1) && is.finite(conf.level) && 
        (conf.level >= 0) && (conf.level < 1))) 
        stop("'conf.level' must be a single number between 0 and 1")
    if (conf.level == 0) 
        conf.level <- FALSE
    std <- match.arg(std)
    findTableWithOAM <- function(or, tab) {
        m <- rowSums(tab)[1L]
        n <- rowSums(tab)[2L]
        t <- colSums(tab)[1L]
        if (or == 1) 
            x <- t * n/(m + n)
        else if (or == Inf) 
            x <- max(0, t - m)
        else {
            A <- or - 1
            B <- or * (m - t) + (n + t)
            C <- -t * n
            x <- (-B + sqrt(B^2 - 4 * A * C))/(2 * A)
        }
        matrix(c(t - x, x, m - t + x, n - x), nrow = 2)
    }
    drawPie <- function(r, from, to, n = 500, col = NA) {
        p <- 2 * pi * seq.int(from, to, length.out = n)/360
        x <- c(cos(p), 0) * r
        y <- c(sin(p), 0) * r
        polygon(x, y, col = col)
        invisible(NULL)
    }
    stdize <- function(tab, std, x) {
        if (std == "margins") {
            if (all(sort(margin) == c(1L, 2L))) {
                u <- sqrt(odds(tab)$or)
                u <- u/(1 + u)
                y <- matrix(c(u, 1 - u, 1 - u, u), nrow = 2L)
            }
            else if (margin %in% c(1, 2)) 
                y <- prop.table(tab, margin)
            else stop("incorrect 'margin' specification")
        }
        else if (std == "ind.max") 
            y <- tab/max(tab)
        else if (std == "all.max") 
            y <- tab/max(x)
        y
    }
    odds <- function(x) {
        if (length(dim(x)) == 2L) {
            dim(x) <- c(dim(x), 1L)
            k <- 1
        }
        else k <- dim(x)[3L]
        or <- double(k)
        se <- double(k)
        for (i in 1:k) {
            f <- x[, , i]
            if (any(f == 0)) 
                f <- f + 0.5
            or[i] <- (f[1L, 1L] * f[2L, 2L])/(f[1L, 2L] * f[2L, 
                1L])
            se[i] <- sqrt(sum(1/f))
        }
        list(or = or, se = se)
    }
    gamma <- 1.25
    debug <- FALSE
    angle.f <- c(90, 180, 0, 270)
    angle.t <- c(180, 270, 90, 360)
    opar <- par(mar = c(0, 0, if (is.null(main)) 0 else 2.5, 
        0))
    on.exit(par(opar))
    byrow <- FALSE
    if (!is.null(mfrow)) {
        nr <- mfrow[1L]
        nc <- mfrow[2L]
    }
    else if (!is.null(mfcol)) {
        nr <- mfcol[1L]
        nc <- mfcol[2L]
        byrow <- TRUE
    }
    else {
        nr <- ceiling(sqrt(k))
        nc <- ceiling(k/nr)
    }
    if (nr * nc < k) 
        stop("incorrect geometry specification")
    if (byrow) 
        indexMatrix <- expand.grid(1:nc, 1:nr)[, c(2, 1)]
    else indexMatrix <- expand.grid(1:nr, 1:nc)
    totalWidth <- nc * 2 * (1 + space) + (nc - 1L) * space
    totalHeight <- if (k == 1) 
        2 * (1 + space)
    else nr * (2 + (2 + gamma) * space) + (nr - 1L) * space
    xlim <- c(0, totalWidth)
    ylim <- c(0, totalHeight)
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    plot.new()
    plot.window(xlim = xlim, ylim = ylim, asp = 1)
    o <- odds(x)
    scale <- space/(2 * strheight("Ag"))
    v <- 0.95 - max(strwidth(as.character(c(x)), cex = scale))/2
    for (i in 1:k) {
        tab <- x[, , i]
        fit <- stdize(tab, std, x)
        xInd <- indexMatrix[i, 2L]
        xOrig <- 2 * xInd - 1 + (3 * xInd - 2) * space
        yInd <- indexMatrix[i, 1L]
        yOrig <- if (k == 1) 
            (1 + space)
        else (totalHeight - (2 * yInd - 1 + ((3 + gamma) * yInd - 
            2) * space))
        plot.window(xlim - xOrig, ylim - yOrig, asp = 1)
        if (debug) {
            abline(h = -1 - space)
            abline(h = 1 + space)
            abline(h = 1 + (1 + gamma) * space)
            abline(v = -1 - space)
            abline(v = 1 + space)
        }
        u <- 1 + space/2
        adjCorr <- 0.2
        text(0, u, paste(names(dimnames(x))[1L], dimnames(x)[[1L]][1L], 
            sep = ": "), adj = c(0.5, 0.5 - adjCorr), cex = scale)
        text(-u, 0, paste(names(dimnames(x))[2L], dimnames(x)[[2L]][1L], 
            sep = ": "), adj = c(0.5, 0.5 - adjCorr), cex = scale, 
            srt = 90)
        text(0, -u, paste(names(dimnames(x))[1L], dimnames(x)[[1L]][2L], 
            sep = ": "), adj = c(0.5, 0.5 + adjCorr), cex = scale)
        text(u, 0, paste(names(dimnames(x))[2L], dimnames(x)[[2L]][2L], 
            sep = ": "), adj = c(0.5, 0.5 + adjCorr), cex = scale, 
            srt = 90)
        if (k > 1) {
            text(0, 1 + (1 + gamma/2) * space, paste(names(dimnames(x))[3L], 
                dimnames(x)[[3L]][i], sep = ": "), cex = gamma * 
                scale)
        }
        d <- odds(tab)$or
        drawPie(sqrt(fit[1, 1]), 90, 180, col = color[1 + (d > 
            1)])
        drawPie(sqrt(fit[2, 1]), 180, 270, col = color[2 - (d > 
            1)])
        drawPie(sqrt(fit[1, 2]), 0, 90, col = color[2 - (d > 
            1)])
        drawPie(sqrt(fit[2, 2]), 270, 360, col = color[1 + (d > 
            1)])
        u <- 1 - space/2
        text(c(-v, -v, v, v), c(u, -u, u, -u), as.character(c(tab)), 
            cex = scale)
        if (is.numeric(conf.level)) {
            or <- o$or[i]
            se <- o$se[i]
            theta <- or * exp(stats::qnorm((1 - conf.level)/2) * 
                se)
            tau <- findTableWithOAM(theta, tab)
            r <- sqrt(c(stdize(tau, std, x)))
            for (j in 1:4) drawPie(r[j], angle.f[j], angle.t[j])
            theta <- or * exp(stats::qnorm((1 + conf.level)/2) * 
                se)
            tau <- findTableWithOAM(theta, tab)
            r <- sqrt(c(stdize(tau, std, x)))
            for (j in 1:4) drawPie(r[j], angle.f[j], angle.t[j])
        }
        polygon(c(-1, 1, 1, -1), c(-1, -1, 1, 1))
        lines(c(-1, 1), c(0, 0))
        for (j in seq.int(from = -0.8, to = 0.8, by = 0.2)) lines(c(j, 
            j), c(-0.02, 0.02))
        for (j in seq.int(from = -0.9, to = 0.9, by = 0.2)) lines(c(j, 
            j), c(-0.01, 0.01))
        lines(c(0, 0), c(-1, 1))
        for (j in seq.int(from = -0.8, to = 0.8, by = 0.2)) lines(c(-0.02, 
            0.02), c(j, j))
        for (j in seq.int(from = -0.9, to = 0.9, by = 0.2)) lines(c(-0.01, 
            0.01), c(j, j))
    }
    if (!is.null(main)) 
        mtext(main, cex = 1.5, adj = 0.5)
    return(invisible())
}


yinch <- function (y = 1, warn.log = TRUE) 
{
    if (warn.log && par("ylog")) 
        warning("y log scale:  yinch() is nonsense")
    y * diff(par("usr")[3:4])/par("pin")[2L]
}


layout <- function (mat, widths = rep.int(1, ncol(mat)), heights = rep.int(1, 
    nrow(mat)), respect = FALSE) 
{
    storage.mode(mat) <- "integer"
    mat <- as.matrix(mat)
    if (!is.logical(respect)) {
        respect <- as.matrix(respect)
        if (!is.matrix(respect) || any(dim(respect) != dim(mat))) 
            stop("'respect' must be logical or matrix with same dimension as 'mat'")
    }
    num.figures <- as.integer(max(mat))
    for (i in 1L:num.figures) if (match(i, mat, nomatch = 0L) == 
        0L) 
        stop(gettextf("layout matrix must contain at least one reference\nto each of the values {1 ... %d}\n", 
            num.figures), domain = NA)
    dm <- dim(mat)
    num.rows <- dm[1L]
    num.cols <- dm[2L]
    cm.widths <- if (is.character(widths)) 
        grep("cm", widths, fixed = TRUE)
    cm.heights <- if (is.character(heights)) 
        grep("cm", heights, fixed = TRUE)
    pad1.rm.cm <- function(v, cm.v, len) {
        if ((ll <- length(v)) < len) 
            v <- c(v, rep.int(1, len - ll))
        if (is.character(v)) {
            wcm <- v[cm.v]
            v[cm.v] <- substring(wcm, 1L, nchar(wcm, type = "c") - 
                3)
            v <- chartr(getOption("OutDec"), ".", v)
        }
        as.numeric(v)
    }
    widths <- pad1.rm.cm(widths, cm.widths, len = num.cols)
    heights <- pad1.rm.cm(heights, cm.heights, len = num.rows)
    if (is.matrix(respect)) {
        respect.mat <- as.integer(respect)
        respect <- 2
    }
    else {
        respect.mat <- matrix(0L, num.rows, num.cols)
    }
    .External.graphics(C_layout, num.rows, num.cols, mat, as.integer(num.figures), 
        col.widths = widths, row.heights = heights, cm.widths, 
        cm.heights, respect = as.integer(respect), respect.mat)
    invisible(num.figures)
}


panel.smooth <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), 
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) 
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)
}


split.screen <- function (figs, screen, erase = TRUE) 
{
    first.split <- !.SSexists("sp.screens")
    if (missing(screen)) 
        screen <- if (!first.split) 
            .SSget("sp.cur.screen")
        else 0
    if (!first.split) 
        .valid.screens <- .SSget("sp.valid.screens")
    if (missing(figs)) 
        if (first.split) 
            return(FALSE)
        else return(.valid.screens)
    if ((first.split && screen != 0) || (!first.split && !(screen %in% 
        .valid.screens))) 
        stop("invalid screen number")
    if (!is.matrix(figs)) {
        if (!is.vector(figs)) 
            stop("'figs' must be a vector or a matrix with 4 columns")
        nr <- figs[1L]
        nc <- figs[2L]
        x <- seq.int(0, 1, length.out = nc + 1)
        y <- seq.int(1, 0, length.out = nr + 1)
        figs <- matrix(c(rep.int(x[-(nc + 1)], nr), rep.int(x[-1L], 
            nr), rep.int(y[-1L], rep.int(nc, nr)), rep.int(y[-(nr + 
            1)], rep.int(nc, nr))), ncol = 4)
    }
    num.screens <- nrow(figs)
    if (num.screens < 1) 
        stop("'figs' must specify at least one screen")
    new.screens <- valid.screens <- cur.screen <- 0
    if (first.split) {
        if (erase) 
            plot.new()
        split.saved.pars <- par(get("par.list", envir = .SSenv))
        split.saved.pars$fig <- NULL
        split.saved.pars$omi <- par(omi = rep.int(0, 4))$omi
        .SSassign("sp.saved.pars", split.saved.pars)
        split.screens <- vector(mode = "list", length = num.screens)
        new.screens <- 1L:num.screens
        for (i in new.screens) {
            split.screens[[i]] <- par(get("par.list", envir = .SSenv))
            split.screens[[i]]$fig <- figs[i, ]
        }
        valid.screens <- new.screens
        cur.screen <- 1
    }
    else {
        if (erase) 
            erase.screen(screen)
        max.screen <- max(.valid.screens)
        new.max.screen <- max.screen + num.screens
        split.screens <- .SSget("sp.screens")
        total <- c(0, 1, 0, 1)
        if (screen > 0) 
            total <- split.screens[[screen]]$fig
        for (i in 1L:num.screens) figs[i, ] <- total[c(1, 1, 
            3, 3)] + figs[i, ] * rep.int(c(total[2L] - total[1L], 
            total[4L] - total[3L]), c(2, 2))
        new.screens <- (max.screen + 1):new.max.screen
        for (i in new.screens) {
            split.screens[[i]] <- par(get("par.list", envir = .SSenv))
            split.screens[[i]]$fig <- figs[i - max.screen, ]
        }
        valid.screens <- c(.valid.screens, new.screens)
        cur.screen <- max.screen + 1
    }
    .SSassign("sp.screens", split.screens)
    .SSassign("sp.cur.screen", cur.screen)
    .SSassign("sp.valid.screens", valid.screens)
    if (first.split) 
        on.exit(close.screen(all.screens = TRUE))
    par(split.screens[[cur.screen]])
    on.exit()
    return(new.screens)
}


screen <- function (n = cur.screen, new = TRUE) 
{
    if (!.SSexists("sp.screens")) 
        return(FALSE)
    cur.screen <- .SSget("sp.cur.screen")
    if (missing(n) && missing(new)) 
        return(cur.screen)
    if (!(n %in% .SSget("sp.valid.screens"))) 
        stop("invalid screen number")
    split.screens <- .SSget("sp.screens")
    split.screens[[cur.screen]] <- par(get("par.list", envir = .SSenv))
    .SSassign("sp.screens", split.screens)
    .SSassign("sp.cur.screen", n)
    par(split.screens[[n]])
    if (new) 
        erase.screen(n)
    invisible(n)
}


rug <- function (x, ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"), 
    quiet = getOption("warn") < 0, ...) 
{
    x <- as.vector(x)
    ok <- is.finite(x)
    x <- x[ok]
    if (!quiet) {
        u <- par("usr")
        u <- if (side%%2 == 1) {
            if (par("xlog")) 
                10^u[1L:2]
            else u[1L:2]
        }
        else {
            if (par("ylog")) 
                10^u[3:4]
            else u[3:4]
        }
        if (any(x < u[1L] | x > u[2L])) 
            warning("some values will be clipped")
    }
    Axis(side = side, at = x, labels = FALSE, lwd = 0, lwd.ticks = lwd, 
        col.ticks = col, tck = ticksize, ...)
}


plot.new <- function () 
{
    for (fun in getHook("before.plot.new")) {
        if (is.character(fun)) 
            fun <- get(fun)
        try(fun())
    }
    .External2(C_plot_new)
    grDevices:::recordPalette()
    for (fun in getHook("plot.new")) {
        if (is.character(fun)) 
            fun <- get(fun)
        try(fun())
    }
    invisible()
}


image.default <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, zlim = range(z[is.finite(z)]), 
    xlim = range(x), ylim = range(y), col = heat.colors(12), 
    add = FALSE, xaxs = "i", yaxs = "i", xlab, ylab, breaks, 
    oldstyle = FALSE, useRaster, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                if (is.null(dim(x))) 
                  stop("argument must be matrix-like")
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
            if (missing(xlab)) 
                xlab <- ""
            if (missing(ylab)) 
                ylab <- ""
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        xn <- deparse(substitute(x))
        if (missing(xlab)) 
            xlab <- paste(xn, "x", sep = "$")
        if (missing(ylab)) 
            ylab <- paste(xn, "y", sep = "$")
        y <- x$y
        x <- x$x
    }
    else {
        if (missing(xlab)) 
            xlab <- if (missing(x)) 
                ""
            else deparse(substitute(x))
        if (missing(ylab)) 
            ylab <- if (missing(y)) 
                ""
            else deparse(substitute(y))
    }
    if (any(!is.finite(x)) || any(!is.finite(y))) 
        stop("'x' and 'y' values must be finite and non-missing")
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z)) 
        stop("'z' must be a matrix")
    if (!typeof(z) %in% c("logical", "integer", "double")) 
        stop("'z' must be numeric or logical")
    if (length(x) > 1 && length(x) == nrow(z)) {
        dx <- 0.5 * diff(x)
        x <- c(x[1L] - dx[1L], x[-length(x)] + dx, x[length(x)] + 
            dx[length(x) - 1])
    }
    if (length(y) > 1 && length(y) == ncol(z)) {
        dy <- 0.5 * diff(y)
        y <- c(y[1L] - dy[1L], y[-length(y)] + dy, y[length(y)] + 
            dy[length(y) - 1L])
    }
    if (missing(breaks)) {
        nc <- length(col)
        if (!missing(zlim) && (any(!is.finite(zlim)) || diff(zlim) < 
            0)) 
            stop("invalid z limits")
        if (diff(zlim) == 0) 
            zlim <- if (zlim[1L] == 0) 
                c(-1, 1)
            else zlim[1L] + c(-0.4, 0.4) * abs(zlim[1L])
        z <- (z - zlim[1L])/diff(zlim)
        zi <- if (oldstyle) 
            floor((nc - 1) * z + 0.5)
        else floor((nc - 1e-05) * z + 1e-07)
        zi[zi < 0 | zi >= nc] <- NA
    }
    else {
        if (length(breaks) != length(col) + 1) 
            stop("must have one more break than colour")
        if (any(!is.finite(breaks))) 
            stop("'breaks' must all be finite")
        if (is.unsorted(breaks)) {
            warning("unsorted 'breaks' will be sorted before use")
            breaks <- sort(breaks)
        }
        zi <- .bincode(z, breaks, TRUE, TRUE) - 1L
    }
    if (!add) 
        plot(xlim, ylim, xlim = xlim, ylim = ylim, type = "n", 
            xaxs = xaxs, yaxs = yaxs, xlab = xlab, ylab = ylab, 
            ...)
    if (length(x) <= 1) 
        x <- par("usr")[1L:2]
    if (length(y) <= 1) 
        y <- par("usr")[3:4]
    if (length(x) != nrow(z) + 1 || length(y) != ncol(z) + 1) 
        stop("dimensions of z are not length(x)(-1) times length(y)(-1)")
    check_irregular <- function(x, y) {
        dx <- diff(x)
        dy <- diff(y)
        (length(dx) && !isTRUE(all.equal(dx, rep(dx[1], length(dx))))) || 
            (length(dy) && !isTRUE(all.equal(dy, rep(dy[1], length(dy)))))
    }
    if (missing(useRaster)) {
        useRaster <- getOption("preferRaster", FALSE)
        if (useRaster && check_irregular(x, y)) 
            useRaster <- FALSE
        if (useRaster) {
            useRaster <- FALSE
            ras <- dev.capabilities("rasterImage")$rasterImage
            if (identical(ras, "yes")) 
                useRaster <- TRUE
            if (identical(ras, "non-missing")) 
                useRaster <- all(!is.na(zi))
        }
    }
    if (useRaster) {
        if (check_irregular(x, y)) 
            stop(gettextf("%s can only be used with a regular grid", 
                sQuote("useRaster = TRUE")), domain = NA)
        if (!is.character(col)) {
            col <- as.integer(col)
            if (any(!is.na(col) & col < 0L)) 
                stop("integer colors must be non-negative")
            col[col < 1L] <- NA_integer_
            p <- palette()
            col <- p[((col - 1L)%%length(p)) + 1L]
        }
        zc <- col[zi + 1L]
        dim(zc) <- dim(z)
        zc <- t(zc)[ncol(zc):1L, , drop = FALSE]
        rasterImage(as.raster(zc), min(x), min(y), max(x), max(y), 
            interpolate = FALSE)
    }
    else .External.graphics(C_image, x, y, zi, col)
    invisible()
}


boxplot.matrix <- function (x, use.cols = TRUE, ...) 
{
    groups <- if (use.cols) {
        split(c(x), rep.int(1L:ncol(x), rep.int(nrow(x), ncol(x))))
    }
    else split(c(x), seq(nrow(x)))
    if (length(nam <- dimnames(x)[[1 + use.cols]])) 
        names(groups) <- nam
    invisible(boxplot(groups, ...))
}


clip <- function (x1, x2, y1, y2) 
invisible(.External.graphics(C_clip, x1, x2, y1, y2))


pairs <- function (x, ...) 
UseMethod("pairs")


points.default <- function (x, y = NULL, type = "p", ...) 
plot.xy(xy.coords(x, y), type = type, ...)


lines.default <- function (x, y = NULL, type = "l", ...) 
plot.xy(xy.coords(x, y), type = type, ...)


strwidth <- function (s, units = "user", cex = NULL, font = NULL, vfont = NULL, 
    ...) 
{
    if (!is.null(vfont)) 
        vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface), 
            fontindex = pmatch(vfont[2L], Hershey$fontindex))
    .External.graphics(C_strWidth, as.graphicsAnnot(s), pmatch(units, 
        c("user", "figure", "inches")), cex, font, vfont, ...)
}


persp <- function (x, ...) 
UseMethod("persp")


plot.design <- function (x, y = NULL, fun = mean, data = NULL, ..., ylim = NULL, 
    xlab = "Factors", ylab = NULL, main = NULL, ask = NULL, xaxt = par("xaxt"), 
    axes = TRUE, xtick = FALSE) 
{
    .plot.des <- function(x, y, fun, ylab, ylim = NULL, ...) {
        if (!is.numeric(y)) 
            stop("'y' must be a numeric vector")
        if (!is.data.frame(x)) 
            stop("'x' must be a data frame")
        if (!all(sapply(x, is.factor)) & !is.factor(x)) 
            stop("all columns/components of 'x' must be factors")
        k <- ncol(x)
        if (anyNA(y)) {
            FUN <- fun
            fun <- function(u) FUN(u[!is.na(u)])
        }
        tot <- fun(y)
        stats <- lapply(x, function(xc) tapply(y, xc, fun))
        if (any(is.na(unlist(stats)))) 
            warning("some levels of the factors are empty", call. = FALSE)
        if (is.null(ylim)) 
            ylim <- range(c(sapply(stats, range, na.rm = TRUE), 
                tot))
        plot(c(0, k + 1), ylim, type = "n", axes = axes, xaxt = "n", 
            xlab = xlab, ylab = ylab, main = main, adj = 0.5, 
            ...)
        segments(0.5, tot, k + 0.5, tot, ...)
        for (i in 1L:k) {
            si <- stats[[i]]
            segments(i, min(si, na.rm = TRUE), i, max(si, na.rm = TRUE), 
                ...)
            for (j in 1L:(length(si))) {
                sij <- si[j]
                segments(i - 0.05, sij, i + 0.05, sij, ...)
                text(i - 0.1, sij, labels = names(sij), adj = 1, 
                  ...)
            }
        }
        if (axes && xaxt != "n") 
            axis(1, at = 1L:k, names(stats), xaxt = xaxt, tick = xtick, 
                mgp = {
                  p <- par("mgp")
                  c(p[1L], if (xtick) p[2L] else 0, 0)
                }, ...)
    }
    fname <- deparse(substitute(fun))
    fun <- match.fun(fun)
    if (!(is.data.frame(x) | inherits(x, "formula"))) 
        stop("'x' must be a dataframe or a formula")
    if (is.data.frame(x)) {
        if (is.null(y)) {
        }
        else if (inherits(y, "formula")) {
            x <- stats::model.frame(y, data = x)
        }
        else if (is.numeric(y)) {
            x <- cbind(y, x[, sapply(x, is.factor)])
            tmpname <- match.call()
            names(x) <- as.character(c(tmpname[[3L]], names(x[, 
                -1])))
        }
        else if (is.character(y)) {
            ynames <- y
            y <- data.frame(x[, y])
            if (sum(sapply(y, is.numeric)) != ncol(y)) {
                stop("a variable in 'y' is not numeric")
            }
            x <- x[, sapply(x, is.factor)]
            xnames <- names(x)
            x <- cbind(x, y)
            names(x) <- c(xnames, ynames)
        }
    }
    else if (is.data.frame(data)) {
        x <- stats::model.frame(x, data = data)
    }
    else {
        x <- stats::model.frame(x)
    }
    i.fac <- sapply(x, is.factor)
    i.num <- sapply(x, is.numeric)
    nResp <- sum(i.num)
    if (nResp == 0) 
        stop("there must be at least one numeric variable!")
    yname <- names(x)[i.num]
    if (is.null(ylab)) 
        ylab <- paste(fname, "of", yname)
    ydata <- as.matrix(x[, i.num])
    if (!any(i.fac)) {
        x <- data.frame(Intercept = rep.int(" ", nrow(x)))
        i.fac <- 1
    }
    xf <- x[, i.fac, drop = FALSE]
    if (is.null(ask)) 
        ask <- prod(par("mfcol")) < nResp && dev.interactive(orNone = TRUE)
    if (ask) {
        oask <- devAskNewPage(ask)
        on.exit(devAskNewPage(oask))
    }
    for (j in 1L:nResp) .plot.des(xf, ydata[, j], fun = fun, 
        ylab = ylab[j], ylim = ylim, ...)
    invisible()
}


matpoints <- function (x, y, type = "p", lty = 1:5, lwd = 1, pch = NULL, col = 1:6, 
    ...) 
matplot(x = x, y = y, type = type, lty = lty, lwd = lwd, pch = pch, 
    col = col, add = TRUE, ...)


dotchart <- function (x, labels = NULL, groups = NULL, gdata = NULL, cex = par("cex"), 
    pt.cex = cex, pch = 21, gpch = 21, bg = par("bg"), color = par("fg"), 
    gcolor = par("fg"), lcolor = "gray", xlim = range(x[is.finite(x)]), 
    main = NULL, xlab = NULL, ylab = NULL, ...) 
{
    opar <- par("mai", "mar", "cex", "yaxs")
    on.exit(par(opar))
    par(cex = cex, yaxs = "i")
    if (!is.numeric(x)) 
        stop("'x' must be a numeric vector or matrix")
    n <- length(x)
    if (is.matrix(x)) {
        if (is.null(labels)) 
            labels <- rownames(x)
        if (is.null(labels)) 
            labels <- as.character(1L:nrow(x))
        labels <- rep_len(labels, n)
        if (is.null(groups)) 
            groups <- col(x, as.factor = TRUE)
        glabels <- levels(groups)
    }
    else {
        if (is.null(labels)) 
            labels <- names(x)
        glabels <- if (!is.null(groups)) 
            levels(groups)
        if (!is.vector(x)) {
            warning("'x' is neither a vector nor a matrix: using as.numeric(x)")
            x <- as.numeric(x)
        }
    }
    plot.new()
    linch <- if (!is.null(labels)) 
        max(strwidth(labels, "inch"), na.rm = TRUE)
    else 0
    if (is.null(glabels)) {
        ginch <- 0
        goffset <- 0
    }
    else {
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- 0.4
    }
    if (!(is.null(labels) && is.null(glabels))) {
        nmai <- par("mai")
        nmai[2L] <- nmai[4L] + max(linch + goffset, ginch) + 
            0.1
        par(mai = nmai)
    }
    if (is.null(groups)) {
        o <- 1L:n
        y <- o
        ylim <- c(0, n + 1)
    }
    else {
        o <- sort.list(as.numeric(groups), decreasing = TRUE)
        x <- x[o]
        groups <- groups[o]
        color <- rep_len(color, length(groups))[o]
        lcolor <- rep_len(lcolor, length(groups))[o]
        offset <- cumsum(c(0, diff(as.numeric(groups)) != 0))
        y <- 1L:n + 2 * offset
        ylim <- range(0, y + 2)
    }
    plot.window(xlim = xlim, ylim = ylim, log = "")
    lheight <- par("csi")
    if (!is.null(labels)) {
        linch <- max(strwidth(labels, "inch"), na.rm = TRUE)
        loffset <- (linch + 0.1)/lheight
        labs <- labels[o]
        mtext(labs, side = 2, line = loffset, at = y, adj = 0, 
            col = color, las = 2, cex = cex, ...)
    }
    abline(h = y, lty = "dotted", col = lcolor)
    points(x, y, pch = pch, col = color, bg = bg, cex = pt.cex/cex)
    if (!is.null(groups)) {
        gpos <- rev(cumsum(rev(tapply(groups, groups, length)) + 
            2) - 1)
        ginch <- max(strwidth(glabels, "inch"), na.rm = TRUE)
        goffset <- (max(linch + 0.2, ginch, na.rm = TRUE) + 0.1)/lheight
        mtext(glabels, side = 2, line = goffset, at = gpos, adj = 0, 
            col = gcolor, las = 2, cex = cex, ...)
        if (!is.null(gdata)) {
            abline(h = gpos, lty = "dotted")
            points(gdata, gpos, pch = gpch, col = gcolor, bg = bg, 
                cex = pt.cex/cex, ...)
        }
    }
    axis(1)
    box()
    title(main = main, xlab = xlab, ylab = ylab, ...)
    invisible()
}


sunflowerplot <- function (x, ...) 
UseMethod("sunflowerplot")


.filled.contour <- function (x, y, z, levels, col) 
{
    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
        stop("no proper 'z' matrix specified")
    .External.graphics(C_filledcontour, x, y, z, levels, col)
    invisible()
}


arrows <- function (x0, y0, x1 = x0, y1 = y0, length = 0.25, angle = 30, 
    code = 2, col = par("fg"), lty = par("lty"), lwd = par("lwd"), 
    ...) 
{
    if (missing(x1) && missing(y1)) 
        stop("one of 'x1' and 'y1' must be given")
    .External.graphics(C_arrows, x0, y0, x1, y1, length = length, 
        angle = angle, code = code, col = col, lty = lty, lwd = lwd, 
        ...)
    invisible()
}


symbols <- function (x, y = NULL, circles, squares, rectangles, stars, thermometers, 
    boxplots, inches = TRUE, add = FALSE, fg = par("col"), bg = NA, 
    xlab = NULL, ylab = NULL, main = NULL, xlim = NULL, ylim = NULL, 
    ...) 
{
    count <- 0
    if (!missing(circles)) {
        count <- count + 1
        data <- circles
        type <- 1
    }
    if (!missing(squares)) {
        count <- count + 1
        data <- squares
        type <- 2
    }
    if (!missing(rectangles)) {
        count <- count + 1
        data <- rectangles
        type <- 3
    }
    if (!missing(stars)) {
        count <- count + 1
        data <- stars
        type <- 4
    }
    if (!missing(thermometers)) {
        count <- count + 1
        data <- thermometers
        type <- 5
    }
    if (!missing(boxplots)) {
        count <- count + 1
        data <- boxplots
        type <- 6
    }
    if (count != 1) 
        stop("exactly one symbol type must be specified")
    xy <- xy.coords(x, y, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)))
    x <- xy$x
    y <- xy$y
    if (!add) {
        if (is.null(xlab)) 
            xlab <- xy$xlab
        if (is.null(ylab)) 
            ylab <- xy$ylab
        if (is.null(xlim)) 
            xlim <- extendrange(x, f = 0.1)
        if (is.null(ylim)) 
            ylim <- extendrange(y, f = 0.1)
        plot(NA, NA, type = "n", xlim = xlim, ylim = ylim, xlab = xlab, 
            ylab = ylab, main = main, ...)
    }
    invisible(.External.graphics(C_symbols, x, y, type, data, 
        inches, bg, fg, ...))
}


hist.default <- function (x, breaks = "Sturges", freq = NULL, probability = !freq, 
    include.lowest = TRUE, right = TRUE, density = NULL, angle = 45, 
    col = NULL, border = NULL, main = paste("Histogram of", xname), 
    xlim = range(breaks), ylim = NULL, xlab = xname, ylab, axes = TRUE, 
    plot = TRUE, labels = FALSE, nclass = NULL, warn.unused = TRUE, 
    ...) 
{
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    xname <- paste(deparse(substitute(x), 500), collapse = "\n")
    n <- length(x <- x[is.finite(x)])
    n <- as.integer(n)
    if (is.na(n)) 
        stop("invalid length(x)")
    use.br <- !missing(breaks)
    if (use.br) {
        if (!missing(nclass)) 
            warning("'nclass' not used when 'breaks' is specified")
    }
    else if (!is.null(nclass) && length(nclass) == 1L) 
        breaks <- nclass
    use.br <- use.br && (nB <- length(breaks)) > 1L
    if (use.br) 
        breaks <- sort(breaks)
    else {
        if (!include.lowest) {
            include.lowest <- TRUE
            warning("'include.lowest' ignored as 'breaks' is not a vector")
        }
        if (is.character(breaks)) {
            breaks <- match.arg(tolower(breaks), c("sturges", 
                "fd", "freedman-diaconis", "scott"))
            breaks <- switch(breaks, sturges = nclass.Sturges(x), 
                `freedman-diaconis` = , fd = nclass.FD(x), scott = nclass.scott(x), 
                stop("unknown 'breaks' algorithm"))
        }
        else if (is.function(breaks)) {
            breaks <- breaks(x)
        }
        if (length(breaks) == 1) {
            if (!is.numeric(breaks) || !is.finite(breaks) || 
                breaks < 1L) 
                stop("invalid number of 'breaks'")
            breaks <- pretty(range(x), n = breaks, min.n = 1)
            nB <- length(breaks)
            if (nB <= 1) 
                stop(gettextf("hist.default: pretty() error, breaks=%s", 
                  format(breaks)), domain = NA)
        }
        else {
            if (!is.numeric(breaks) || length(breaks) <= 1) 
                stop(gettextf("Invalid breakpoints produced by 'breaks(x)': %s", 
                  format(breaks)), domain = NA)
            breaks <- sort(breaks)
            nB <- length(breaks)
            use.br <- TRUE
        }
    }
    nB <- as.integer(nB)
    if (is.na(nB)) 
        stop("invalid length(breaks)")
    h <- diff(breaks)
    equidist <- !use.br || diff(range(h)) < 1e-07 * mean(h)
    if (!use.br && any(h <= 0)) 
        stop("'breaks' are not strictly increasing")
    freq1 <- freq
    if (is.null(freq)) {
        freq1 <- if (!missing(probability)) 
            !as.logical(probability)
        else equidist
    }
    else if (!missing(probability) && any(probability == freq)) 
        stop("'probability' is an alias for '!freq', however they differ.")
    diddle <- 1e-07 * if (nB > 5) 
        stats::median(h)
    else if (nB <= 3) 
        diff(range(x))
    else min(h[h > 0])
    fuzz <- if (right) 
        c(if (include.lowest) -diddle else diddle, rep.int(diddle, 
            nB - 1L))
    else c(rep.int(-diddle, nB - 1L), if (include.lowest) diddle else -diddle)
    fuzzybreaks <- breaks + fuzz
    counts <- .Call(C_BinCount, x, fuzzybreaks, right, include.lowest)
    if (any(counts < 0L)) 
        stop("negative 'counts'. Internal Error.", domain = NA)
    if (sum(counts) < n) 
        stop("some 'x' not counted; maybe 'breaks' do not span range of 'x'")
    dens <- counts/(n * h)
    mids <- 0.5 * (breaks[-1L] + breaks[-nB])
    r <- structure(list(breaks = breaks, counts = counts, density = dens, 
        mids = mids, xname = xname, equidist = equidist), class = "histogram")
    if (plot) {
        plot(r, freq = freq1, col = col, border = border, angle = angle, 
            density = density, main = main, xlim = xlim, ylim = ylim, 
            xlab = xlab, ylab = ylab, axes = axes, labels = labels, 
            ...)
        invisible(r)
    }
    else {
        if (warn.unused) {
            nf <- names(formals())
            nf <- nf[is.na(match(nf, c("x", "breaks", "nclass", 
                "plot", "include.lowest", "right")))]
            missE <- lapply(nf, function(n) substitute(missing(.), 
                list(. = as.name(n))))
            not.miss <- !sapply(missE, eval, envir = environment())
            if (any(not.miss)) 
                warning(sprintf(ngettext(sum(not.miss), "argument %s is not made use of", 
                  "arguments %s are not made use of"), paste(sQuote(nf[not.miss]), 
                  collapse = ", ")), domain = NA)
        }
        r
    }
}


stripchart <- function (x, ...) 
UseMethod("stripchart")


xyinch <- function (xy = 1, warn.log = TRUE) 
{
    if (warn.log && (par("xlog") || par("ylog"))) 
        warning("log scale:  xyinch() is nonsense")
    u <- par("usr")
    xy * c(u[2L] - u[1L], u[4L] - u[3L])/par("pin")
}


xinch <- function (x = 1, warn.log = TRUE) 
{
    if (warn.log && par("xlog")) 
        warning("x log scale:  xinch() is nonsense")
    x * diff(par("usr")[1L:2])/par("pin")[1L]
}


contour <- function (x, ...) 
UseMethod("contour")


erase.screen <- function (n = cur.screen) 
{
    if (!.SSexists("sp.screens")) 
        return(FALSE)
    cur.screen <- .SSget("sp.cur.screen")
    if (!(n %in% .SSget("sp.valid.screens")) && n != 0) 
        stop("invalid screen number")
    old <- par(usr = c(0, 1, 0, 1), mar = c(0, 0, 0, 0), fig = if (n > 
        0) 
        .SSget("sp.screens")[[n]]$fig
    else c(0, 1, 0, 1), xaxs = "i", yaxs = "i")
    on.exit(par(old))
    par(new = TRUE)
    plot.new()
    polygon(c(0, 1, 1, 0), c(0, 0, 1, 1), border = NA, col = 0)
    par(new = TRUE)
    invisible()
}


Axis <- function (x = NULL, at = NULL, ..., side, labels = NULL) 
{
    if (!is.null(x)) 
        UseMethod("Axis", x)
    else if (!is.null(at)) 
        UseMethod("Axis", at)
    else axis(side = side, at = at, labels = labels, ...)
}


text.default <- function (x, y = NULL, labels = seq_along(x), adj = NULL, pos = NULL, 
    offset = 0.5, vfont = NULL, cex = 1, col = NULL, font = NULL, 
    ...) 
{
    if (!missing(y) && (is.character(y) || is.expression(y))) {
        labels <- y
        y <- NULL
    }
    labels <- as.graphicsAnnot(labels)
    if (!is.null(vfont)) 
        vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface), 
            fontindex = pmatch(vfont[2L], Hershey$fontindex))
    .External.graphics(C_text, xy.coords(x, y, recycle = TRUE), 
        labels, adj, pos, offset, vfont, cex, col, font, ...)
    invisible()
}


grconvertX <- function (x, from = "user", to = "user") 
{
    from <- pmatch(from, .units)
    to <- pmatch(to, .units)
    .External(C_convertX, as.double(x), from, to)
}


legend <- function (x, y = NULL, legend, fill = NULL, col = par("col"), 
    border = "black", lty, lwd, pch, angle = 45, density = NULL, 
    bty = "o", bg = par("bg"), box.lwd = par("lwd"), box.lty = par("lty"), 
    box.col = par("fg"), pt.bg = NA, cex = 1, pt.cex = cex, pt.lwd = lwd, 
    xjust = 0, yjust = 1, x.intersp = 1, y.intersp = 1, adj = c(0, 
        0.5), text.width = NULL, text.col = par("col"), text.font = NULL, 
    merge = do.lines && has.pch, trace = FALSE, plot = TRUE, 
    ncol = 1, horiz = FALSE, title = NULL, inset = 0, xpd, title.col = text.col, 
    title.adj = 0.5, seg.len = 2) 
{
    if (missing(legend) && !missing(y) && (is.character(y) || 
        is.expression(y))) {
        legend <- y
        y <- NULL
    }
    mfill <- !missing(fill) || !missing(density)
    if (!missing(xpd)) {
        op <- par("xpd")
        on.exit(par(xpd = op))
        par(xpd = xpd)
    }
    title <- as.graphicsAnnot(title)
    if (length(title) > 1) 
        stop("invalid 'title'")
    legend <- as.graphicsAnnot(legend)
    n.leg <- if (is.call(legend)) 
        1
    else length(legend)
    if (n.leg == 0) 
        stop("'legend' is of length 0")
    auto <- if (is.character(x)) 
        match.arg(x, c("bottomright", "bottom", "bottomleft", 
            "left", "topleft", "top", "topright", "right", "center"))
    else NA
    if (is.na(auto)) {
        xy <- xy.coords(x, y)
        x <- xy$x
        y <- xy$y
        nx <- length(x)
        if (nx < 1 || nx > 2) 
            stop("invalid coordinate lengths")
    }
    else nx <- 0
    xlog <- par("xlog")
    ylog <- par("ylog")
    rect2 <- function(left, top, dx, dy, density = NULL, angle, 
        ...) {
        r <- left + dx
        if (xlog) {
            left <- 10^left
            r <- 10^r
        }
        b <- top - dy
        if (ylog) {
            top <- 10^top
            b <- 10^b
        }
        rect(left, top, r, b, angle = angle, density = density, 
            ...)
    }
    segments2 <- function(x1, y1, dx, dy, ...) {
        x2 <- x1 + dx
        if (xlog) {
            x1 <- 10^x1
            x2 <- 10^x2
        }
        y2 <- y1 + dy
        if (ylog) {
            y1 <- 10^y1
            y2 <- 10^y2
        }
        segments(x1, y1, x2, y2, ...)
    }
    points2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        points(x, y, ...)
    }
    text2 <- function(x, y, ...) {
        if (xlog) 
            x <- 10^x
        if (ylog) 
            y <- 10^y
        text(x, y, ...)
    }
    if (trace) 
        catn <- function(...) do.call("cat", c(lapply(list(...), 
            formatC), list("\n")))
    cin <- par("cin")
    Cex <- cex * par("cex")
    if (is.null(text.width)) 
        text.width <- max(abs(strwidth(legend, units = "user", 
            cex = cex, font = text.font)))
    else if (!is.numeric(text.width) || text.width < 0) 
        stop("'text.width' must be numeric, >= 0")
    xc <- Cex * xinch(cin[1L], warn.log = FALSE)
    yc <- Cex * yinch(cin[2L], warn.log = FALSE)
    if (xc < 0) 
        text.width <- -text.width
    xchar <- xc
    xextra <- 0
    yextra <- yc * (y.intersp - 1)
    ymax <- yc * max(1, strheight(legend, units = "user", cex = cex)/yc)
    ychar <- yextra + ymax
    if (trace) 
        catn("  xchar=", xchar, "; (yextra,ychar)=", c(yextra, 
            ychar))
    if (mfill) {
        xbox <- xc * 0.8
        ybox <- yc * 0.5
        dx.fill <- xbox
    }
    do.lines <- (!missing(lty) && (is.character(lty) || any(lty > 
        0))) || !missing(lwd)
    n.legpercol <- if (horiz) {
        if (ncol != 1) 
            warning(gettextf("horizontal specification overrides: Number of columns := %d", 
                n.leg), domain = NA)
        ncol <- n.leg
        1
    }
    else ceiling(n.leg/ncol)
    has.pch <- !missing(pch) && length(pch) > 0
    if (do.lines) {
        x.off <- if (merge) 
            -0.7
        else 0
    }
    else if (merge) 
        warning("'merge = TRUE' has no effect when no line segments are drawn")
    if (has.pch) {
        if (is.character(pch) && !is.na(pch[1L]) && nchar(pch[1L], 
            type = "c") > 1) {
            if (length(pch) > 1) 
                warning("not using pch[2..] since pch[1L] has multiple chars")
            np <- nchar(pch[1L], type = "c")
            pch <- substr(rep.int(pch[1L], np), 1L:np, 1L:np)
        }
        if (!is.character(pch)) 
            pch <- as.integer(pch)
    }
    if (is.na(auto)) {
        if (xlog) 
            x <- log10(x)
        if (ylog) 
            y <- log10(y)
    }
    if (nx == 2) {
        x <- sort(x)
        y <- sort(y)
        left <- x[1L]
        top <- y[2L]
        w <- diff(x)
        h <- diff(y)
        w0 <- w/ncol
        x <- mean(x)
        y <- mean(y)
        if (missing(xjust)) 
            xjust <- 0.5
        if (missing(yjust)) 
            yjust <- 0.5
    }
    else {
        h <- (n.legpercol + (!is.null(title))) * ychar + yc
        w0 <- text.width + (x.intersp + 1) * xchar
        if (mfill) 
            w0 <- w0 + dx.fill
        if (do.lines) 
            w0 <- w0 + (seg.len + x.off) * xchar
        w <- ncol * w0 + 0.5 * xchar
        if (!is.null(title) && (abs(tw <- strwidth(title, units = "user", 
            cex = cex) + 0.5 * xchar)) > abs(w)) {
            xextra <- (tw - w)/2
            w <- tw
        }
        if (is.na(auto)) {
            left <- x - xjust * w
            top <- y + (1 - yjust) * h
        }
        else {
            usr <- par("usr")
            inset <- rep_len(inset, 2)
            insetx <- inset[1L] * (usr[2L] - usr[1L])
            left <- switch(auto, bottomright = , topright = , 
                right = usr[2L] - w - insetx, bottomleft = , 
                left = , topleft = usr[1L] + insetx, bottom = , 
                top = , center = (usr[1L] + usr[2L] - w)/2)
            insety <- inset[2L] * (usr[4L] - usr[3L])
            top <- switch(auto, bottomright = , bottom = , bottomleft = usr[3L] + 
                h + insety, topleft = , top = , topright = usr[4L] - 
                insety, left = , right = , center = (usr[3L] + 
                usr[4L] + h)/2)
        }
    }
    if (plot && bty != "n") {
        if (trace) 
            catn("  rect2(", left, ",", top, ", w=", w, ", h=", 
                h, ", ...)", sep = "")
        rect2(left, top, dx = w, dy = h, col = bg, density = NULL, 
            lwd = box.lwd, lty = box.lty, border = box.col)
    }
    xt <- left + xchar + xextra + (w0 * rep.int(0:(ncol - 1), 
        rep.int(n.legpercol, ncol)))[1L:n.leg]
    yt <- top - 0.5 * yextra - ymax - (rep.int(1L:n.legpercol, 
        ncol)[1L:n.leg] - 1 + (!is.null(title))) * ychar
    if (mfill) {
        if (plot) {
            if (!is.null(fill)) 
                fill <- rep_len(fill, n.leg)
            rect2(left = xt, top = yt + ybox/2, dx = xbox, dy = ybox, 
                col = fill, density = density, angle = angle, 
                border = border)
        }
        xt <- xt + dx.fill
    }
    if (plot && (has.pch || do.lines)) 
        col <- rep_len(col, n.leg)
    if (missing(lwd) || is.null(lwd)) 
        lwd <- par("lwd")
    if (do.lines) {
        if (missing(lty) || is.null(lty)) 
            lty <- 1
        lty <- rep_len(lty, n.leg)
        lwd <- rep_len(lwd, n.leg)
        ok.l <- !is.na(lty) & (is.character(lty) | lty > 0) & 
            !is.na(lwd)
        if (trace) 
            catn("  segments2(", xt[ok.l] + x.off * xchar, ",", 
                yt[ok.l], ", dx=", seg.len * xchar, ", dy=0, ...)")
        if (plot) 
            segments2(xt[ok.l] + x.off * xchar, yt[ok.l], dx = seg.len * 
                xchar, dy = 0, lty = lty[ok.l], lwd = lwd[ok.l], 
                col = col[ok.l])
        xt <- xt + (seg.len + x.off) * xchar
    }
    if (has.pch) {
        pch <- rep_len(pch, n.leg)
        pt.bg <- rep_len(pt.bg, n.leg)
        pt.cex <- rep_len(pt.cex, n.leg)
        pt.lwd <- rep_len(pt.lwd, n.leg)
        ok <- !is.na(pch)
        if (!is.character(pch)) {
            ok <- ok & (pch >= 0 | pch <= -32)
        }
        else {
            ok <- ok & nzchar(pch)
        }
        x1 <- (if (merge && do.lines) 
            xt - (seg.len/2) * xchar
        else xt)[ok]
        y1 <- yt[ok]
        if (trace) 
            catn("  points2(", x1, ",", y1, ", pch=", pch[ok], 
                ", ...)")
        if (plot) 
            points2(x1, y1, pch = pch[ok], col = col[ok], cex = pt.cex[ok], 
                bg = pt.bg[ok], lwd = pt.lwd[ok])
    }
    xt <- xt + x.intersp * xchar
    if (plot) {
        if (!is.null(title)) 
            text2(left + w * title.adj, top - ymax, labels = title, 
                adj = c(title.adj, 0), cex = cex, col = title.col)
        text2(xt, yt, labels = legend, adj = adj, cex = cex, 
            col = text.col, font = text.font)
    }
    invisible(list(rect = list(w = w, h = h, left = left, top = top), 
        text = list(x = xt, y = yt)))
}


text <- function (x, ...) 
UseMethod("text")


grconvertY <- function (y, from = "user", to = "user") 
{
    from <- pmatch(from, .units)
    to <- pmatch(to, .units)
    .External(C_convertY, as.double(y), from, to)
}


lcm <- function (x) 
paste(x, "cm")


cdplot <- function (x, ...) 
{
    UseMethod("cdplot")
}


hist <- function (x, ...) 
UseMethod("hist")


smoothScatter <- function (x, y = NULL, nbin = 128, bandwidth, colramp = colorRampPalette(c("white", 
    blues9)), nrpoints = 100, ret.selection = FALSE, pch = ".", 
    cex = 1, col = "black", transformation = function(x) x^0.25, 
    postPlotHook = box, xlab = NULL, ylab = NULL, xlim, ylim, 
    xaxs = par("xaxs"), yaxs = par("yaxs"), ...) 
{
    if (!is.numeric(nrpoints) || nrpoints < 0 || length(nrpoints) != 
        1) 
        stop("'nrpoints' should be numeric scalar with value >= 0.")
    nrpoints <- round(nrpoints)
    ret.selection <- ret.selection && nrpoints > 0
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    ylabel <- if (!missing(y)) 
        deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel)
    xlab <- if (is.null(xlab)) 
        xy$xlab
    else xlab
    ylab <- if (is.null(ylab)) 
        xy$ylab
    else ylab
    x <- cbind(xy$x, xy$y)[I <- is.finite(xy$x) & is.finite(xy$y), 
        , drop = FALSE]
    if (ret.selection) 
        iS <- which(I)
    if (!missing(xlim)) {
        stopifnot(is.numeric(xlim), length(xlim) == 2, is.finite(xlim))
        x <- x[I <- min(xlim) <= x[, 1] & x[, 1] <= max(xlim), 
            , drop = FALSE]
        if (ret.selection) 
            iS <- iS[I]
    }
    else {
        xlim <- range(x[, 1])
    }
    if (!missing(ylim)) {
        stopifnot(is.numeric(ylim), length(ylim) == 2, is.finite(ylim))
        x <- x[I <- min(ylim) <= x[, 2] & x[, 2] <= max(ylim), 
            , drop = FALSE]
        if (ret.selection) 
            iS <- iS[I]
    }
    else {
        ylim <- range(x[, 2])
    }
    map <- grDevices:::.smoothScatterCalcDensity(x, nbin, bandwidth)
    xm <- map$x1
    ym <- map$x2
    dens <- map$fhat
    dens[] <- transformation(dens)
    image(xm, ym, z = dens, col = colramp(256), xlab = xlab, 
        ylab = ylab, xlim = xlim, ylim = ylim, xaxs = xaxs, yaxs = yaxs, 
        ...)
    if (!is.null(postPlotHook)) 
        postPlotHook()
    if (nrpoints > 0) {
        nrpoints <- min(nrow(x), ceiling(nrpoints))
        stopifnot((nx <- length(xm)) == nrow(dens), (ny <- length(ym)) == 
            ncol(dens))
        ixm <- 1L + as.integer((nx - 1) * (x[, 1] - xm[1])/(xm[nx] - 
            xm[1]))
        iym <- 1L + as.integer((ny - 1) * (x[, 2] - ym[1])/(ym[ny] - 
            ym[1]))
        sel <- order(dens[cbind(ixm, iym)])[seq_len(nrpoints)]
        x <- x[sel, , drop = FALSE]
        points(x, pch = pch, cex = cex, col = col)
        if (ret.selection) 
            iS[sel]
    }
}


layout.show <- function (n = 1) 
{
    oma.saved <- par("oma")
    par(oma = rep.int(0, 4))
    par(oma = oma.saved)
    o.par <- par(mar = rep.int(0, 4))
    on.exit(par(o.par))
    for (i in seq_len(n)) {
        plot.new()
        box()
        text(0.5, 0.5, i)
    }
    invisible()
}


grid <- function (nx = NULL, ny = nx, col = "lightgray", lty = "dotted", 
    lwd = par("lwd"), equilogs = TRUE) 
{
    if (is.null(nx) || (!is.na(nx) && nx >= 1)) {
        log <- par("xlog")
        if (is.null(nx)) {
            ax <- par("xaxp")
            if (log && equilogs && ax[3L] > 0) 
                ax[3L] <- 1
            at <- axTicks(1, axp = ax, log = log)
        }
        else {
            U <- par("usr")
            at <- seq.int(U[1L], U[2L], length.out = nx + 1)
            at <- (if (log) 
                10^at
            else at)[-c(1, nx + 1)]
        }
        abline(v = at, col = col, lty = lty, lwd = lwd)
    }
    if (is.null(ny) || (!is.na(ny) && ny >= 1)) {
        log <- par("ylog")
        if (is.null(ny)) {
            ax <- par("yaxp")
            if (log && equilogs && ax[3L] > 0) 
                ax[3L] <- 1
            at <- axTicks(2, axp = ax, log = log)
        }
        else {
            U <- par("usr")
            at <- seq.int(U[3L], U[4L], length.out = ny + 1)
            at <- (if (log) 
                10^at
            else at)[-c(1, ny + 1)]
        }
        abline(h = at, col = col, lty = lty, lwd = lwd)
    }
}


polypath <- function (x, y = NULL, border = NULL, col = NA, lty = par("lty"), 
    rule = "winding", ...) 
{
    xy <- xy.coords(x, y)
    if (is.logical(border)) {
        if (!is.na(border) && border) 
            border <- par("fg")
        else border <- NA
    }
    rule <- match(rule, c("winding", "evenodd"))
    if (is.na(rule)) 
        stop("Invalid fill rule for graphics path")
    breaks <- which(is.na(xy$x) | is.na(xy$y))
    if (length(breaks) == 0) {
        .External.graphics(C_path, xy$x, xy$y, as.integer(length(xy$x)), 
            as.integer(rule), col, border, lty, ...)
    }
    else {
        nb <- length(breaks)
        lengths <- c(breaks[1] - 1, diff(breaks) - 1, length(xy$x) - 
            breaks[nb])
        .External.graphics(C_path, xy$x[-breaks], xy$y[-breaks], 
            as.integer(lengths), as.integer(rule), col, border, 
            lty, ...)
    }
    invisible()
}


close.screen <- function (n, all.screens = FALSE) 
{
    if (!.SSexists("sp.screens")) 
        return(FALSE)
    if (missing(n) && missing(all.screens)) 
        return(.SSget("sp.valid.screens"))
    valid.screens <- .SSget("sp.valid.screens")
    if (all.screens || all(valid.screens %in% n)) {
        par(.SSget("sp.saved.pars"))
        par(mfrow = c(1, 1), new = FALSE)
        rm(list = paste(c("sp.screens", "sp.cur.screen", "sp.saved.pars", 
            "sp.valid.screens"), dev.cur(), sep = ":"), envir = .SSenv)
        invisible()
    }
    else {
        valid.screens <- valid.screens[-sort(match(n, valid.screens))]
        .SSassign("sp.valid.screens", valid.screens)
        temp <- .SSget("sp.cur.screen")
        if (temp %in% n) {
            poss <- valid.screens[valid.screens > temp]
            temp <- if (length(poss)) 
                min(poss)
            else min(valid.screens)
        }
        screen(temp, new = FALSE)
        valid.screens
    }
}


xspline <- function (x, y = NULL, shape = 0, open = TRUE, repEnds = TRUE, 
    draw = TRUE, border = par("fg"), col = NA, ...) 
{
    xy <- xy.coords(x, y)
    s <- rep.int(shape, length(xy$x))
    if (open) 
        s[1L] <- s[length(x)] <- 0
    invisible(.External.graphics(C_xspline, xy$x, xy$y, s, open, 
        repEnds, draw, col, border, ...))
}


curve <- function (expr, from = NULL, to = NULL, n = 101, add = FALSE, 
    type = "l", xname = "x", xlab = xname, ylab = NULL, log = NULL, 
    xlim = NULL, ...) 
{
    sexpr <- substitute(expr)
    if (is.name(sexpr)) {
        expr <- call(as.character(sexpr), as.name(xname))
    }
    else {
        if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% 
            all.vars(sexpr))) 
            stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", 
                xname), domain = NA)
        expr <- sexpr
    }
    if (dev.cur() == 1L && !identical(add, FALSE)) {
        warning("'add' will be ignored as there is no existing plot")
        add <- FALSE
    }
    addF <- identical(add, FALSE)
    if (is.null(ylab)) 
        ylab <- deparse(expr)
    if (is.null(from) || is.null(to)) {
        xl <- if (!is.null(xlim)) 
            xlim
        else if (!addF) {
            pu <- par("usr")[1L:2L]
            if (par("xaxs") == "r") 
                pu <- extendrange(pu, f = -1/27)
            if (par("xlog")) 
                10^pu
            else pu
        }
        else c(0, 1)
        if (is.null(from)) 
            from <- xl[1L]
        if (is.null(to)) 
            to <- xl[2L]
    }
    lg <- if (length(log)) 
        log
    else if (!addF && par("xlog")) 
        "x"
    else ""
    if (length(lg) == 0) 
        lg <- ""
    if (grepl("x", lg, fixed = TRUE)) {
        if (from <= 0 || to <= 0) 
            stop("'from' and 'to' must be > 0 with log=\"x\"")
        x <- exp(seq.int(log(from), log(to), length.out = n))
    }
    else x <- seq.int(from, to, length.out = n)
    ll <- list(x = x)
    names(ll) <- xname
    y <- eval(expr, envir = ll, enclos = parent.frame())
    if (length(y) != length(x)) 
        stop("'expr' did not evaluate to an object of length 'n'")
    if (isTRUE(add)) 
        lines(x = x, y = y, type = type, ...)
    else plot(x = x, y = y, type = type, xlab = xlab, ylab = ylab, 
        xlim = xlim, log = lg, ...)
    invisible(list(x = x, y = y))
}


filled.contour <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    levels = pretty(zlim, nlevels), nlevels = 20, color.palette = cm.colors, 
    col = color.palette(length(levels) - 1), plot.title, plot.axes, 
    key.title, key.axes, asp = NA, xaxs = "i", yaxs = "i", las = 1, 
    axes = TRUE, frame.plot = axes, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    mar.orig <- (par.orig <- par(c("mar", "las", "mfrow")))$mar
    on.exit(par(par.orig))
    w <- (3 + mar.orig[2L]) * par("csi") * 2.54
    layout(matrix(c(2, 1), ncol = 2L), widths = c(1, lcm(w)))
    par(las = las)
    mar <- mar.orig
    mar[4L] <- mar[2L]
    mar[2L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim = c(0, 1), ylim = range(levels), xaxs = "i", 
        yaxs = "i")
    rect(0, levels[-length(levels)], 1, levels[-1L], col = col)
    if (missing(key.axes)) {
        if (axes) 
            axis(4)
    }
    else key.axes
    box()
    if (!missing(key.title)) 
        key.title
    mar <- mar.orig
    mar[4L] <- 1
    par(mar = mar)
    plot.new()
    plot.window(xlim, ylim, "", xaxs = xaxs, yaxs = yaxs, asp = asp)
    .filled.contour(x, y, z, levels, col)
    if (missing(plot.axes)) {
        if (axes) {
            title(main = "", xlab = "", ylab = "")
            Axis(x, side = 1)
            Axis(y, side = 2)
        }
    }
    else plot.axes
    if (frame.plot) 
        box()
    if (missing(plot.title)) 
        title(...)
    else plot.title
    invisible()
}


co.intervals <- function (x, number = 6, overlap = 0.5) 
{
    x <- sort(x[!is.na(x)])
    n <- length(x)
    r <- n/(number * (1 - overlap) + overlap)
    ii <- 0:(number - 1) * (1 - overlap) * r
    x1 <- x[round(1 + ii)]
    xr <- x[round(r + ii)]
    keep <- c(TRUE, diff(x1) > 0 | diff(xr) > 0)
    j.gt.0 <- 0 < (jump <- diff(x))
    eps <- 0.5 * if (any(j.gt.0)) 
        min(jump[j.gt.0])
    else 0
    cbind(x1[keep] - eps, xr[keep] + eps)
}


contour.default <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, nlevels = 10, levels = pretty(zlim, 
    nlevels), labels = NULL, xlim = range(x, finite = TRUE), 
    ylim = range(y, finite = TRUE), zlim = range(z, finite = TRUE), 
    labcex = 0.6, drawlabels = TRUE, method = "flattest", vfont, 
    axes = TRUE, frame.plot = axes, col = par("fg"), lty = par("lty"), 
    lwd = par("lwd"), add = FALSE, ...) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z) || nrow(z) <= 1L || ncol(z) <= 1L) 
        stop("no proper 'z' matrix specified")
    if (!add) {
        localPlotWindow <- function(xlim, ylim, ..., main, sub, 
            xlab, ylab, outer, line) plot.window(xlim, ylim, 
            ...)
        localTitle <- function(..., log) title(...)
        plot.new()
        localPlotWindow(xlim, ylim, ...)
        localTitle(...)
    }
    method <- pmatch(method[1L], c("simple", "edge", "flattest"))
    if (missing(vfont)) 
        vfont <- if (.Call(C_contourDef)) 
            NULL
        else c("sans serif", "plain")
    if (!is.null(vfont)) 
        vfont <- c(typeface = pmatch(vfont[1L], Hershey$typeface), 
            fontindex = pmatch(vfont[2L], Hershey$fontindex))
    if (!is.null(labels)) 
        labels <- as.character(labels)
    .External.graphics(C_contour, x, y, z, levels, labels, labcex, 
        drawlabels, method, vfont, col, lty, lwd)
    if (!add) {
        localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
        localBox <- function(..., col, bg, pch, cex, lty, lwd) box(...)
        if (axes) {
            localAxis(x, side = 1, ...)
            localAxis(y, side = 2, ...)
        }
        if (frame.plot) 
            localBox(...)
    }
    invisible()
}


boxplot.default <- function (x, ..., range = 1.5, width = NULL, varwidth = FALSE, 
    notch = FALSE, outline = TRUE, names, plot = TRUE, border = par("fg"), 
    col = NULL, log = "", pars = list(boxwex = 0.8, staplewex = 0.5, 
        outwex = 0.5), horizontal = FALSE, add = FALSE, at = NULL) 
{
    args <- list(x, ...)
    namedargs <- if (!is.null(attributes(args)$names)) 
        attributes(args)$names != ""
    else rep_len(FALSE, length(args))
    groups <- if (is.list(x)) 
        x
    else args[!namedargs]
    if (0L == (n <- length(groups))) 
        stop("invalid first argument")
    if (length(class(groups))) 
        groups <- unclass(groups)
    if (!missing(names)) 
        attr(groups, "names") <- names
    else {
        if (is.null(attr(groups, "names"))) 
            attr(groups, "names") <- 1L:n
        names <- attr(groups, "names")
    }
    cls <- sapply(groups, function(x) class(x)[1L])
    cl <- if (all(cls == cls[1L])) 
        cls[1L]
    else NULL
    for (i in 1L:n) groups[i] <- list(boxplot.stats(unclass(groups[[i]]), 
        range))
    stats <- matrix(0, nrow = 5L, ncol = n)
    conf <- matrix(0, nrow = 2L, ncol = n)
    ng <- out <- group <- numeric(0L)
    ct <- 1
    for (i in groups) {
        stats[, ct] <- i$stats
        conf[, ct] <- i$conf
        ng <- c(ng, i$n)
        if ((lo <- length(i$out))) {
            out <- c(out, i$out)
            group <- c(group, rep.int(ct, lo))
        }
        ct <- ct + 1
    }
    if (length(cl) && cl != "numeric") 
        oldClass(stats) <- cl
    z <- list(stats = stats, n = ng, conf = conf, out = out, 
        group = group, names = names)
    if (plot) {
        if (is.null(pars$boxfill) && is.null(args$boxfill)) 
            pars$boxfill <- col
        do.call("bxp", c(list(z, notch = notch, width = width, 
            varwidth = varwidth, log = log, border = border, 
            pars = pars, outline = outline, horizontal = horizontal, 
            add = add, at = at), args[namedargs]))
        invisible(z)
    }
    else z
}


plot.function <- function (x, y = 0, to = 1, from = y, xlim = NULL, ylab = NULL, 
    ...) 
{
    if (!missing(y) && missing(from)) 
        from <- y
    if (is.null(xlim)) {
        if (is.null(from)) 
            from <- 0
    }
    else {
        if (missing(from)) 
            from <- xlim[1L]
        if (missing(to)) 
            to <- xlim[2L]
    }
    if (is.null(ylab)) {
        sx <- substitute(x)
        ylab <- if (mode(x) != "name") 
            deparse(sx)[1L]
        else {
            xname <- list(...)[["xname"]]
            if (is.null(xname)) 
                xname <- "x"
            paste0(sx, "(", xname, ")")
        }
    }
    curve(expr = x, from = from, to = to, xlim = xlim, ylab = ylab, 
        ...)
}


spineplot <- function (x, ...) 
{
    UseMethod("spineplot")
}


plot <- function (x, y, ...) 
UseMethod("plot")


boxplot <- function (x, ...) 
UseMethod("boxplot")


rasterImage <- function (image, xleft, ybottom, xright, ytop, angle = 0, interpolate = TRUE, 
    ...) 
{
    .External.graphics(C_raster, if (inherits(image, "nativeRaster")) image else as.raster(image), 
        as.double(xleft), as.double(ybottom), as.double(xright), 
        as.double(ytop), as.double(angle), as.logical(interpolate), 
        ...)
    invisible()
}


axis.POSIXct <- function (side, x, at, format, labels = TRUE, ...) 
{
    mat <- missing(at) || is.null(at)
    if (!mat) 
        x <- as.POSIXct(at)
    else x <- as.POSIXct(x)
    range <- par("usr")[if (side%%2) 
        1L:2L
    else 3L:4L]
    d <- range[2L] - range[1L]
    z <- c(range, x[is.finite(x)])
    attr(z, "tzone") <- attr(x, "tzone")
    if (d < 1.1 * 60) {
        sc <- 1
        if (missing(format)) 
            format <- "%S"
    }
    else if (d < 1.1 * 60 * 60) {
        sc <- 60
        if (missing(format)) 
            format <- "%M:%S"
    }
    else if (d < 1.1 * 60 * 60 * 24) {
        sc <- 60 * 60
        if (missing(format)) 
            format <- "%H:%M"
    }
    else if (d < 2 * 60 * 60 * 24) {
        sc <- 60 * 60
        if (missing(format)) 
            format <- "%a %H:%M"
    }
    else if (d < 7 * 60 * 60 * 24) {
        sc <- 60 * 60 * 24
        if (missing(format)) 
            format <- "%a"
    }
    else {
        sc <- 60 * 60 * 24
    }
    if (d < 60 * 60 * 24 * 50) {
        zz <- pretty(z/sc)
        z <- zz * sc
        z <- .POSIXct(z, attr(x, "tzone"))
        if (sc == 60 * 60 * 24) 
            z <- as.POSIXct(round(z, "days"))
        if (missing(format)) 
            format <- "%b %d"
    }
    else if (d < 1.1 * 60 * 60 * 24 * 365) {
        z <- .POSIXct(z, attr(x, "tzone"))
        zz <- as.POSIXlt(z)
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1
        zz$hour <- zz$min <- zz$sec <- 0
        zz$mon <- pretty(zz$mon)
        m <- length(zz$mon)
        M <- 2 * m
        m <- rep.int(zz$year[1L], m)
        zz$year <- c(m, m + 1)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        zz <- .POSIXlt(zz, attr(x, "tzone"))
        z <- as.POSIXct(zz)
        if (missing(format)) 
            format <- "%b"
    }
    else {
        z <- .POSIXct(z, attr(x, "tzone"))
        zz <- as.POSIXlt(z)
        zz$mday <- zz$wday <- zz$yday <- 1
        zz$isdst <- -1
        zz$mon <- zz$hour <- zz$min <- zz$sec <- 0
        zz$year <- pretty(zz$year)
        M <- length(zz$year)
        zz <- lapply(zz, function(x) rep(x, length.out = M))
        z <- as.POSIXct(.POSIXlt(zz))
        if (missing(format)) 
            format <- "%Y"
    }
    if (!mat) 
        z <- x[is.finite(x)]
    keep <- z >= range[1L] & z <= range[2L]
    z <- z[keep]
    if (!is.logical(labels)) 
        labels <- labels[keep]
    else if (identical(labels, TRUE)) 
        labels <- format(z, format = format)
    else if (identical(labels, FALSE)) 
        labels <- rep("", length(z))
    axis(side, at = z, labels = labels, ...)
}


bxp <- function (z, notch = FALSE, width = NULL, varwidth = FALSE, outline = TRUE, 
    notch.frac = 0.5, log = "", border = par("fg"), pars = NULL, 
    frame.plot = axes, horizontal = FALSE, add = FALSE, at = NULL, 
    show.names = NULL, ...) 
{
    pars <- c(list(...), pars)
    pars <- pars[unique(names(pars))]
    bplt <- function(x, wid, stats, out, conf, notch, xlog, i) {
        ok <- TRUE
        if (!anyNA(stats)) {
            xP <- if (xlog) 
                function(x, w) x * exp(w)
            else function(x, w) x + w
            wid <- wid/2
            if (notch) {
                ok <- stats[2L] <= conf[1L] && conf[2L] <= stats[4L]
                xx <- xP(x, wid * c(-1, 1, 1, notch.frac, 1, 
                  1, -1, -1, -notch.frac, -1))
                yy <- c(stats[c(2, 2)], conf[1L], stats[3L], 
                  conf[2L], stats[c(4, 4)], conf[2L], stats[3L], 
                  conf[1L])
            }
            else {
                xx <- xP(x, wid * c(-1, 1, 1, -1))
                yy <- stats[c(2, 2, 4, 4)]
            }
            if (!notch) 
                notch.frac <- 1
            wntch <- notch.frac * wid
            xypolygon(xx, yy, lty = "blank", col = boxfill[i])
            xysegments(xP(x, -wntch), stats[3L], xP(x, +wntch), 
                stats[3L], lty = medlty[i], lwd = medlwd[i], 
                col = medcol[i], lend = 1)
            xypoints(x, stats[3L], pch = medpch[i], cex = medcex[i], 
                col = medcol[i], bg = medbg[i])
            xysegments(rep.int(x, 2), stats[c(1, 5)], rep.int(x, 
                2), stats[c(2, 4)], lty = whisklty[i], lwd = whisklwd[i], 
                col = whiskcol[i])
            xysegments(rep.int(xP(x, -wid * staplewex[i]), 2), 
                stats[c(1, 5)], rep.int(xP(x, +wid * staplewex[i]), 
                  2), stats[c(1, 5)], lty = staplelty[i], lwd = staplelwd[i], 
                col = staplecol[i])
            xypolygon(xx, yy, lty = boxlty[i], lwd = boxlwd[i], 
                border = boxcol[i])
            if ((nout <- length(out))) {
                xysegments(rep(x - wid * outwex, nout), out, 
                  rep(x + wid * outwex, nout), out, lty = outlty[i], 
                  lwd = outlwd[i], col = outcol[i])
                xypoints(rep.int(x, nout), out, pch = outpch[i], 
                  lwd = outlwd[i], cex = outcex[i], col = outcol[i], 
                  bg = outbg[i])
            }
            if (any(inf <- !is.finite(out))) {
                warning(sprintf(ngettext(length(unique(out[inf])), 
                  "Outlier (%s) in boxplot %d is not drawn", 
                  "Outliers (%s) in boxplot %d are not drawn"), 
                  paste(unique(out[inf]), collapse = ", "), i), 
                  domain = NA)
            }
        }
        return(ok)
    }
    if (!is.list(z) || 0L == (n <- length(z$n))) 
        stop("invalid first argument")
    if (is.null(at)) 
        at <- 1L:n
    else if (length(at) != n) 
        stop(gettextf("'at' must have same length as 'z$n', i.e. %d", 
            n), domain = NA)
    if (is.null(z$out)) 
        z$out <- numeric()
    if (is.null(z$group) || !outline) 
        z$group <- integer()
    if (is.null(pars$ylim)) 
        ylim <- range(z$stats[is.finite(z$stats)], if (outline) z$out[is.finite(z$out)], 
            if (notch) z$conf[is.finite(z$conf)])
    else {
        ylim <- pars$ylim
        pars$ylim <- NULL
    }
    if (length(border) == 0L) 
        border <- par("fg")
    dev.hold()
    on.exit(dev.flush())
    if (!add) {
        if (is.null(pars$xlim)) 
            xlim <- range(at, finite = TRUE) + c(-0.5, 0.5)
        else {
            xlim <- pars$xlim
            pars$xlim <- NULL
        }
        plot.new()
        if (horizontal) 
            plot.window(ylim = xlim, xlim = ylim, log = log, 
                xaxs = pars$yaxs)
        else plot.window(xlim = xlim, ylim = ylim, log = log, 
            yaxs = pars$yaxs)
    }
    xlog <- (par("ylog") && horizontal) || (par("xlog") && !horizontal)
    pcycle <- function(p, def1, def2 = NULL) rep(if (length(p)) p else if (length(def1)) def1 else def2, 
        length.out = n)
    p <- function(sym) pars[[sym, exact = TRUE]]
    boxlty <- pcycle(pars$boxlty, p("lty"), par("lty"))
    boxlwd <- pcycle(pars$boxlwd, p("lwd"), par("lwd"))
    boxcol <- pcycle(pars$boxcol, border)
    boxfill <- pcycle(pars$boxfill, par("bg"))
    boxwex <- pcycle(pars$boxwex, 0.8 * {
        if (n <= 1) 
            1
        else stats::quantile(diff(sort(if (xlog) 
            log(at)
        else at)), 0.1)
    })
    medlty <- pcycle(pars$medlty, p("lty"), par("lty"))
    medlwd <- pcycle(pars$medlwd, 3 * p("lwd"), 3 * par("lwd"))
    medpch <- pcycle(pars$medpch, NA_integer_)
    medcex <- pcycle(pars$medcex, p("cex"), par("cex"))
    medcol <- pcycle(pars$medcol, border)
    medbg <- pcycle(pars$medbg, p("bg"), par("bg"))
    whisklty <- pcycle(pars$whisklty, p("lty"), "dashed")
    whisklwd <- pcycle(pars$whisklwd, p("lwd"), par("lwd"))
    whiskcol <- pcycle(pars$whiskcol, border)
    staplelty <- pcycle(pars$staplelty, p("lty"), par("lty"))
    staplelwd <- pcycle(pars$staplelwd, p("lwd"), par("lwd"))
    staplecol <- pcycle(pars$staplecol, border)
    staplewex <- pcycle(pars$staplewex, 0.5)
    outlty <- pcycle(pars$outlty, "blank")
    outlwd <- pcycle(pars$outlwd, p("lwd"), par("lwd"))
    outpch <- pcycle(pars$outpch, p("pch"), par("pch"))
    outcex <- pcycle(pars$outcex, p("cex"), par("cex"))
    outcol <- pcycle(pars$outcol, border)
    outbg <- pcycle(pars$outbg, p("bg"), par("bg"))
    outwex <- pcycle(pars$outwex, 0.5)
    width <- if (!is.null(width)) {
        if (length(width) != n | anyNA(width) | any(width <= 
            0)) 
            stop("invalid boxplot widths")
        boxwex * width/max(width)
    }
    else if (varwidth) 
        boxwex * sqrt(z$n/max(z$n))
    else if (n == 1) 
        0.5 * boxwex
    else rep.int(boxwex, n)
    if (horizontal) {
        xypoints <- function(x, y, ...) points(y, x, ...)
        xypolygon <- function(x, y, ...) polygon(y, x, ...)
        xysegments <- function(x0, y0, x1, y1, ...) segments(y0, 
            x0, y1, x1, ...)
    }
    else {
        xypoints <- points
        xypolygon <- polygon
        xysegments <- segments
    }
    ok <- TRUE
    for (i in 1L:n) ok <- ok & bplt(at[i], wid = width[i], stats = z$stats[, 
        i], out = z$out[z$group == i], conf = z$conf[, i], notch = notch, 
        xlog = xlog, i = i)
    if (!ok) 
        warning("some notches went outside hinges ('box'): maybe set notch=FALSE")
    axes <- is.null(pars$axes)
    if (!axes) {
        axes <- pars$axes
        pars$axes <- NULL
    }
    if (axes) {
        ax.pars <- pars[names(pars) %in% c("xaxt", "yaxt", "xaxp", 
            "yaxp", "las", "cex.axis", "col.axis", "format")]
        if (is.null(show.names)) 
            show.names <- n > 1
        if (show.names) 
            do.call("axis", c(list(side = 1 + horizontal, at = at, 
                labels = z$names), ax.pars))
        do.call("Axis", c(list(x = z$stats, side = 2 - horizontal), 
            ax.pars))
    }
    do.call("title", pars[names(pars) %in% c("main", "cex.main", 
        "col.main", "sub", "cex.sub", "col.sub", "xlab", "ylab", 
        "cex.lab", "col.lab")])
    if (frame.plot) 
        box()
    invisible(at)
}


box <- function (which = "plot", lty = "solid", ...) 
{
    which <- pmatch(which[1L], c("plot", "figure", "inner", "outer"))
    .External.graphics(C_box, which = which, lty = lty, ...)
    invisible()
}


mtext <- function (text, side = 3, line = 0, outer = FALSE, at = NA, adj = NA, 
    padj = NA, cex = NA, col = NA, font = NA, ...) 
invisible(.External.graphics(C_mtext, as.graphicsAnnot(text), 
    side, line, outer, at, adj, padj, cex, col, font, ...))


matplot <- function (x, y, type = "p", lty = 1:5, lwd = 1, lend = par("lend"), 
    pch = NULL, col = 1:6, cex = NULL, bg = NA, xlab = NULL, 
    ylab = NULL, xlim = NULL, ylim = NULL, ..., add = FALSE, 
    verbose = getOption("verbose")) 
{
    paste.ch <- function(chv) paste0("\"", chv, "\"", collapse = " ")
    str2vec <- function(string) {
        if (nchar(string, type = "c")[1L] > 1L) 
            strsplit(string[1L], NULL)[[1L]]
        else string
    }
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    ylabel <- if (!missing(y)) 
        deparse(substitute(y))
    if (missing(x)) {
        if (missing(y)) 
            stop("must specify at least one of 'x' and 'y'")
        else x <- seq_len(NROW(y))
    }
    else if (missing(y)) {
        y <- x
        ylabel <- xlabel
        x <- seq_len(NROW(y))
        xlabel <- ""
    }
    kx <- ncol(x <- as.matrix(x))
    ky <- ncol(y <- as.matrix(y))
    n <- nrow(x)
    if (n != nrow(y)) 
        stop("'x' and 'y' must have same number of rows")
    if (kx > 1L && ky > 1L && kx != ky) 
        stop("'x' and 'y' must have only 1 or the same number of columns")
    if (kx == 1L) 
        x <- matrix(x, nrow = n, ncol = ky)
    if (ky == 1L) 
        y <- matrix(y, nrow = n, ncol = kx)
    k <- max(kx, ky)
    type <- str2vec(type)
    if (is.null(pch)) {
        pch <- c(1L:9L, 0L, letters, LETTERS)
        if (k > length(pch) && any(type %in% c("p", "o", "b"))) 
            warning("default 'pch' is smaller than number of columns and hence recycled")
    }
    else if (is.character(pch)) 
        pch <- str2vec(pch)
    if (verbose) 
        message("matplot: doing ", k, " plots with ", paste0(" col= (", 
            paste.ch(col), ")"), paste0(" pch= (", paste.ch(pch), 
            ")"), " ...\n", domain = NA)
    ii <- match("log", names(xargs <- list(...)), nomatch = 0L)
    log <- if (ii != 0) 
        xargs[[ii]]
    xy <- xy.coords(x, y, xlabel, ylabel, log = log)
    xlab <- if (is.null(xlab)) 
        xy$xlab
    else xlab
    ylab <- if (is.null(ylab)) 
        xy$ylab
    else ylab
    xlim <- if (is.null(xlim)) 
        range(xy$x[is.finite(xy$x)])
    else xlim
    ylim <- if (is.null(ylim)) 
        range(xy$y[is.finite(xy$y)])
    else ylim
    if (length(type) < k) 
        type <- rep_len(type, k)
    if (length(lty) < k) 
        lty <- rep_len(lty, k)
    if (length(lend) < k) 
        lend <- rep_len(lend, k)
    if (length(lwd) < k && !is.null(lwd)) 
        lwd <- rep_len(lwd, k)
    if (length(pch) < k) 
        pch <- rep_len(pch, k)
    if (length(col) < k) 
        col <- rep_len(col, k)
    if (length(bg) < k) 
        bg <- rep_len(bg, k)
    if (is.null(cex)) 
        cex <- 1
    if (length(cex) < k) 
        cex <- rep_len(cex, k)
    ii <- seq_len(k)
    dev.hold()
    on.exit(dev.flush())
    if (!add) {
        ii <- ii[-1L]
        plot(x[, 1L], y[, 1L], type = type[1L], xlab = xlab, 
            ylab = ylab, xlim = xlim, ylim = ylim, lty = lty[1L], 
            lwd = lwd[1L], lend = lend[1L], pch = pch[1L], col = col[1L], 
            cex = cex[1L], bg = bg[1L], ...)
    }
    for (i in ii) lines(x[, i], y[, i], type = type[i], lty = lty[i], 
        lwd = lwd[i], lend = lend[i], pch = pch[i], col = col[i], 
        cex = cex[i], bg = bg[i])
    invisible()
}


abline <- function (a = NULL, b = NULL, h = NULL, v = NULL, reg = NULL, 
    coef = NULL, untf = FALSE, ...) 
{
    int_abline <- function(a, b, h, v, untf, col = par("col"), 
        lty = par("lty"), lwd = par("lwd"), ...) .External.graphics(C_abline, 
        a, b, h, v, untf, col, lty, lwd, ...)
    if (!is.null(reg)) {
        if (!is.null(a)) 
            warning("'a' is overridden by 'reg'")
        a <- reg
    }
    if (is.object(a) || is.list(a)) {
        p <- length(coefa <- as.vector(coef(a)))
        if (p > 2) 
            warning(gettextf("only using the first two of %d regression coefficients", 
                p), domain = NA)
        islm <- inherits(a, "lm")
        noInt <- if (islm) 
            !as.logical(attr(stats::terms(a), "intercept"))
        else p == 1
        if (noInt) {
            a <- 0
            b <- coefa[1L]
        }
        else {
            a <- coefa[1L]
            b <- if (p >= 2) 
                coefa[2L]
            else 0
        }
    }
    if (!is.null(coef)) {
        if (!is.null(a)) 
            warning("'a' and 'b' are overridden by 'coef'")
        a <- coef[1L]
        b <- coef[2L]
    }
    int_abline(a = a, b = b, h = h, v = v, untf = untf, ...)
    invisible()
}


pie <- function (x, labels = names(x), edges = 200, radius = 0.8, clockwise = FALSE, 
    init.angle = if (clockwise) 90 else 0, density = NULL, angle = 45, 
    col = NULL, border = NULL, lty = NULL, main = NULL, ...) 
{
    if (!is.numeric(x) || any(is.na(x) | x < 0)) 
        stop("'x' values must be positive.")
    if (is.null(labels)) 
        labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L]) 
        xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    dev.hold()
    on.exit(dev.flush())
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col)) 
        col <- if (is.null(density)) 
            c("white", "lightblue", "mistyrose", "lightcyan", 
                "lavender", "cornsilk")
        else par("fg")
    if (!is.null(col)) 
        col <- rep_len(col, nx)
    if (!is.null(border)) 
        border <- rep_len(border, nx)
    if (!is.null(lty)) 
        lty <- rep_len(lty, nx)
    angle <- rep(angle, nx)
    if (!is.null(density)) 
        density <- rep_len(density, nx)
    twopi <- if (clockwise) 
        -2 * pi
    else 2 * pi
    t2xy <- function(t) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n))
        polygon(c(P$x, 0), c(P$y, 0), density = density[i], angle = angle[i], 
            border = border[i], col = col[i], lty = lty[i])
        P <- t2xy(mean(x[i + 0:1]))
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * P$x, c(1, 1.05) * P$y)
            text(1.1 * P$x, 1.1 * P$y, labels[i], xpd = TRUE, 
                adj = ifelse(P$x < 0, 1, 0), ...)
        }
    }
    title(main = main, ...)
    invisible(NULL)
}


image <- function (x, ...) 
UseMethod("image")


matlines <- function (x, y, type = "l", lty = 1:5, lwd = 1, pch = NULL, col = 1:6, 
    ...) 
matplot(x = x, y = y, type = type, lty = lty, lwd = lwd, pch = pch, 
    col = col, add = TRUE, ...)


segments <- function (x0, y0, x1 = x0, y1 = y0, col = par("fg"), lty = par("lty"), 
    lwd = par("lwd"), ...) 
{
    if (missing(x1) && missing(y1)) 
        stop("one of 'x1' and 'y1' must be given")
    .External.graphics(C_segments, x0, y0, x1, y1, col = col, 
        lty = lty, lwd = lwd, ...)
    invisible()
}


lines <- function (x, ...) 
UseMethod("lines")


barplot.default <- function (height, width = 1, space = NULL, names.arg = NULL, 
    legend.text = NULL, beside = FALSE, horiz = FALSE, density = NULL, 
    angle = 45, col = NULL, border = par("fg"), main = NULL, 
    sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, 
    xpd = TRUE, log = "", axes = TRUE, axisnames = TRUE, cex.axis = par("cex.axis"), 
    cex.names = par("cex.axis"), inside = TRUE, plot = TRUE, 
    axis.lty = 0, offset = 0, add = FALSE, args.legend = NULL, 
    ...) 
{
    if (!missing(inside)) 
        .NotYetUsed("inside", error = FALSE)
    if (is.null(space)) 
        space <- if (is.matrix(height) && beside) 
            c(0, 1)
        else 0.2
    space <- space * mean(width)
    if (plot && axisnames && is.null(names.arg)) 
        names.arg <- if (is.matrix(height)) 
            colnames(height)
        else names(height)
    if (is.vector(height) || (is.array(height) && (length(dim(height)) == 
        1))) {
        height <- cbind(height)
        beside <- TRUE
        if (is.null(col)) 
            col <- "grey"
    }
    else if (is.matrix(height)) {
        if (is.null(col)) 
            col <- gray.colors(nrow(height))
    }
    else stop("'height' must be a vector or a matrix")
    if (is.logical(legend.text)) 
        legend.text <- if (legend.text && is.matrix(height)) 
            rownames(height)
    stopifnot(is.character(log))
    logx <- logy <- FALSE
    if (log != "") {
        logx <- length(grep("x", log)) > 0L
        logy <- length(grep("y", log)) > 0L
    }
    if ((logx || logy) && !is.null(density)) 
        stop("Cannot use shading lines in bars when log scale is used")
    NR <- nrow(height)
    NC <- ncol(height)
    if (beside) {
        if (length(space) == 2) 
            space <- rep.int(c(space[2L], rep.int(space[1L], 
                NR - 1)), NC)
        width <- rep_len(width, NR)
    }
    else {
        width <- rep_len(width, NC)
    }
    offset <- rep_len(as.vector(offset), length(width))
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    log.dat <- (logx && horiz) || (logy && !horiz)
    if (log.dat) {
        if (min(height + offset, na.rm = TRUE) <= 0) 
            stop("log scale error: at least one 'height + offset' value <= 0")
        if (logx && !is.null(xlim) && min(xlim) <= 0) 
            stop("log scale error: 'xlim' <= 0")
        if (logy && !is.null(ylim) && min(ylim) <= 0) 
            stop("log scale error: 'ylim' <= 0")
        rectbase <- if (logy && !horiz && !is.null(ylim)) 
            ylim[1L]
        else if (logx && horiz && !is.null(xlim)) 
            xlim[1L]
        else 0.9 * min(height, na.rm = TRUE)
    }
    else rectbase <- 0
    if (!beside) 
        height <- rbind(rectbase, apply(height, 2L, cumsum))
    rAdj <- offset + (if (log.dat) 
        0.9 * height
    else -0.01 * height)
    delta <- width/2
    w.r <- cumsum(space + width)
    w.m <- w.r - delta
    w.l <- w.m - delta
    if (horiz) {
        if (is.null(xlim)) 
            xlim <- range(rAdj, height + offset, na.rm = TRUE)
        if (is.null(ylim)) 
            ylim <- c(min(w.l), max(w.r))
    }
    else {
        if (is.null(xlim)) 
            xlim <- c(min(w.l), max(w.r))
        if (is.null(ylim)) 
            ylim <- range(rAdj, height + offset, na.rm = TRUE)
    }
    if (beside) 
        w.m <- matrix(w.m, ncol = NC)
    if (plot) {
        dev.hold()
        opar <- if (horiz) 
            par(xaxs = "i", xpd = xpd)
        else par(yaxs = "i", xpd = xpd)
        on.exit({
            dev.flush()
            par(opar)
        })
        if (!add) {
            plot.new()
            plot.window(xlim, ylim, log = log, ...)
        }
        xyrect <- function(x1, y1, x2, y2, horizontal = TRUE, 
            ...) {
            if (horizontal) 
                rect(x1, y1, x2, y2, ...)
            else rect(y1, x1, y2, x2, ...)
        }
        if (beside) 
            xyrect(rectbase + offset, w.l, c(height) + offset, 
                w.r, horizontal = horiz, angle = angle, density = density, 
                col = col, border = border)
        else {
            for (i in 1L:NC) {
                xyrect(height[1L:NR, i] + offset[i], w.l[i], 
                  height[-1, i] + offset[i], w.r[i], horizontal = horiz, 
                  angle = angle, density = density, col = col, 
                  border = border)
            }
        }
        if (axisnames && !is.null(names.arg)) {
            at.l <- if (length(names.arg) != length(w.m)) {
                if (length(names.arg) == NC) 
                  colMeans(w.m)
                else stop("incorrect number of names")
            }
            else w.m
            axis(if (horiz) 
                2
            else 1, at = at.l, labels = names.arg, lty = axis.lty, 
                cex.axis = cex.names, ...)
        }
        if (!is.null(legend.text)) {
            legend.col <- rep_len(col, length(legend.text))
            if ((horiz & beside) || (!horiz & !beside)) {
                legend.text <- rev(legend.text)
                legend.col <- rev(legend.col)
                density <- rev(density)
                angle <- rev(angle)
            }
            xy <- par("usr")
            if (is.null(args.legend)) {
                legend(xy[2L] - xinch(0.1), xy[4L] - yinch(0.1), 
                  legend = legend.text, angle = angle, density = density, 
                  fill = legend.col, xjust = 1, yjust = 1)
            }
            else {
                args.legend1 <- list(x = xy[2L] - xinch(0.1), 
                  y = xy[4L] - yinch(0.1), legend = legend.text, 
                  angle = angle, density = density, fill = legend.col, 
                  xjust = 1, yjust = 1)
                args.legend1[names(args.legend)] <- args.legend
                do.call("legend", args.legend1)
            }
        }
        title(main = main, sub = sub, xlab = xlab, ylab = ylab, 
            ...)
        if (axes) 
            axis(if (horiz) 
                1
            else 2, cex.axis = cex.axis, ...)
        invisible(w.m)
    }
    else w.m
}


barplot <- function (height, ...) 
UseMethod("barplot")


locator <- function (n = 512, type = "n", ...) 
{
    if (length(extras <- list(...))) {
        opar <- par(extras)
        on.exit(par(opar))
    }
    z <- .External2(C_locator, n, type = type)
    x <- z[[1L]]
    y <- z[[2L]]
    if ((n <- z[[3L]]) > 0) 
        list(x = x[1L:n], y = y[1L:n])
}


stars <- function (x, full = TRUE, scale = TRUE, radius = TRUE, labels = dimnames(x)[[1L]], 
    locations = NULL, nrow = NULL, ncol = NULL, len = 1, key.loc = NULL, 
    key.labels = dimnames(x)[[2L]], key.xpd = TRUE, xlim = NULL, 
    ylim = NULL, flip.labels = NULL, draw.segments = FALSE, col.segments = 1L:n.seg, 
    col.stars = NA, col.lines = NA, axes = FALSE, frame.plot = axes, 
    main = NULL, sub = NULL, xlab = "", ylab = "", cex = 0.8, 
    lwd = 0.25, lty = par("lty"), xpd = FALSE, mar = pmin(par("mar"), 
        1.1 + c(2 * axes + (xlab != ""), 2 * axes + (ylab != 
            ""), 1, 0)), add = FALSE, plot = TRUE, ...) 
{
    if (is.data.frame(x)) 
        x <- data.matrix(x)
    else if (!is.matrix(x)) 
        stop("'x' must be a matrix or a data frame")
    if (!is.numeric(x)) 
        stop("data in 'x' must be numeric")
    n.loc <- nrow(x)
    n.seg <- ncol(x)
    if (is.null(locations)) {
        if (is.null(nrow)) 
            nrow <- ceiling(if (!is.numeric(ncol)) sqrt(n.loc) else n.loc/ncol)
        if (is.null(ncol)) 
            ncol <- ceiling(n.loc/nrow)
        if (nrow * ncol < n.loc) 
            stop("'nrow * ncol' is less than the number of observations")
        ff <- if (!is.null(labels)) 
            2.3
        else 2.1
        locations <- expand.grid(ff * 1L:ncol, ff * nrow:1)[1L:n.loc, 
            ]
        if (!is.null(labels) && (missing(flip.labels) || !is.logical(flip.labels))) 
            flip.labels <- ncol * mean(nchar(labels, type = "c")) > 
                30
    }
    else {
        if (is.numeric(locations) && length(locations) == 2) {
            locations <- cbind(rep.int(locations[1L], n.loc), 
                rep.int(locations[2L], n.loc))
            if (!missing(labels) && n.loc > 1) 
                warning("labels do not make sense for a single location")
            else labels <- NULL
        }
        else {
            if (is.data.frame(locations)) 
                locations <- data.matrix(locations)
            if (!is.matrix(locations) || ncol(locations) != 2) 
                stop("'locations' must be a 2-column matrix.")
            if (n.loc != nrow(locations)) 
                stop("number of rows of 'locations' and 'x' must be equal.")
        }
        if (missing(flip.labels) || !is.logical(flip.labels)) 
            flip.labels <- FALSE
    }
    xloc <- locations[, 1]
    yloc <- locations[, 2]
    angles <- if (full) 
        seq.int(0, 2 * pi, length.out = n.seg + 1)[-(n.seg + 
            1)]
    else if (draw.segments) 
        seq.int(0, pi, length.out = n.seg + 1)[-(n.seg + 1)]
    else seq.int(0, pi, length.out = n.seg)
    if (length(angles) != n.seg) 
        stop("length of 'angles' must equal 'ncol(x)'")
    if (scale) {
        x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/diff(range(x, 
            na.rm = TRUE)))
    }
    x[is.na(x)] <- 0
    mx <- max(x <- x * len)
    if (is.null(xlim)) 
        xlim <- range(xloc) + c(-mx, mx)
    if (is.null(ylim)) 
        ylim <- range(yloc) + c(-mx, mx)
    deg <- pi/180
    op <- par(mar = mar, xpd = xpd)
    on.exit(par(op))
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    if (plot && !add) 
        plot(0, type = "n", ..., xlim = xlim, ylim = ylim, main = main, 
            sub = sub, xlab = xlab, ylab = ylab, asp = 1, axes = axes)
    if (!plot) 
        return(locations)
    s.x <- xloc + x * rep.int(cos(angles), rep.int(n.loc, n.seg))
    s.y <- yloc + x * rep.int(sin(angles), rep.int(n.loc, n.seg))
    if (draw.segments) {
        aangl <- c(angles, if (full) 2 * pi else pi)
        for (i in 1L:n.loc) {
            px <- py <- numeric()
            for (j in 1L:n.seg) {
                k <- seq.int(from = aangl[j], to = aangl[j + 
                  1], by = 1 * deg)
                px <- c(px, xloc[i], s.x[i, j], x[i, j] * cos(k) + 
                  xloc[i], NA)
                py <- c(py, yloc[i], s.y[i, j], x[i, j] * sin(k) + 
                  yloc[i], NA)
            }
            polygon(px, py, col = col.segments, lwd = lwd, lty = lty)
        }
    }
    else {
        for (i in 1L:n.loc) {
            polygon(s.x[i, ], s.y[i, ], lwd = lwd, lty = lty, 
                col = col.stars[i])
            polygon(s.x[i, ], s.y[i, ], lwd = lwd, lty = lty, 
                border = col.lines[i], col = col.stars[i])
            if (radius) 
                segments(rep.int(xloc[i], n.seg), rep.int(yloc[i], 
                  n.seg), s.x[i, ], s.y[i, ], lwd = lwd, lty = lty)
        }
    }
    if (!is.null(labels)) {
        y.off <- mx * (if (full) 
            1
        else 0.1)
        if (flip.labels) 
            y.off <- y.off + cex * par("cxy")[2L] * ((1L:n.loc)%%2 - 
                if (full) 
                  0.4
                else 0)
        text(xloc, yloc - y.off, labels, cex = cex, adj = c(0.5, 
            1))
    }
    if (!is.null(key.loc)) {
        par(xpd = key.xpd)
        key.x <- len * cos(angles) + key.loc[1L]
        key.y <- len * sin(angles) + key.loc[2L]
        if (draw.segments) {
            px <- py <- numeric()
            for (j in 1L:n.seg) {
                k <- seq.int(from = aangl[j], to = aangl[j + 
                  1], by = 1 * deg)
                px <- c(px, key.loc[1L], key.x[j], len * cos(k) + 
                  key.loc[1L], NA)
                py <- c(py, key.loc[2L], key.y[j], len * sin(k) + 
                  key.loc[2L], NA)
            }
            polygon(px, py, col = col.segments, lwd = lwd, lty = lty)
        }
        else {
            polygon(key.x, key.y, lwd = lwd, lty = lty)
            if (radius) 
                segments(rep.int(key.loc[1L], n.seg), rep.int(key.loc[2L], 
                  n.seg), key.x, key.y, lwd = lwd, lty = lty)
        }
        lab.angl <- angles + if (draw.segments) 
            (angles[2L] - angles[1L])/2
        else 0
        label.x <- 1.1 * len * cos(lab.angl) + key.loc[1L]
        label.y <- 1.1 * len * sin(lab.angl) + key.loc[2L]
        for (k in 1L:n.seg) {
            text.adj <- c(if (lab.angl[k] < 90 * deg || lab.angl[k] > 
                270 * deg) 0 else if (lab.angl[k] > 90 * deg && 
                lab.angl[k] < 270 * deg) 1 else 0.5, if (lab.angl[k] <= 
                90 * deg) (1 - lab.angl[k]/(90 * deg))/2 else if (lab.angl[k] <= 
                270 * deg) (lab.angl[k] - 90 * deg)/(180 * deg) else 1 - 
                (lab.angl[k] - 270 * deg)/(180 * deg))
            text(label.x[k], label.y[k], labels = key.labels[k], 
                cex = cex, adj = text.adj)
        }
    }
    if (frame.plot) 
        box(...)
    invisible(locations)
}


coplot <- function (formula, data, given.values, panel = points, rows, 
    columns, show.given = TRUE, col = par("fg"), pch = par("pch"), 
    bar.bg = c(num = gray(0.8), fac = gray(0.95)), xlab = c(x.name, 
        paste("Given :", a.name)), ylab = c(y.name, paste("Given :", 
        b.name)), subscripts = FALSE, axlabels = function(f) abbreviate(levels(f)), 
    number = 6, overlap = 0.5, xlim, ylim, ...) 
{
    deparen <- function(expr) {
        while (is.language(expr) && !is.name(expr) && deparse(expr[[1L]])[1L] == 
            "(") expr <- expr[[2L]]
        expr
    }
    bad.formula <- function() stop("invalid conditioning formula")
    bad.lengths <- function() stop("incompatible variable lengths")
    getOp <- function(call) deparse(call[[1L]], backtick = FALSE)[[1L]]
    formula <- deparen(formula)
    if (!inherits(formula, "formula")) 
        bad.formula()
    y <- deparen(formula[[2L]])
    rhs <- deparen(formula[[3L]])
    if (getOp(rhs) != "|") 
        bad.formula()
    x <- deparen(rhs[[2L]])
    rhs <- deparen(rhs[[3L]])
    if (is.language(rhs) && !is.name(rhs) && getOp(rhs) %in% 
        c("*", "+")) {
        have.b <- TRUE
        a <- deparen(rhs[[2L]])
        b <- deparen(rhs[[3L]])
    }
    else {
        have.b <- FALSE
        a <- rhs
    }
    if (missing(data)) 
        data <- parent.frame()
    x.name <- deparse(x)
    x <- eval(x, data, parent.frame())
    nobs <- length(x)
    y.name <- deparse(y)
    y <- eval(y, data, parent.frame())
    if (length(y) != nobs) 
        bad.lengths()
    a.name <- deparse(a)
    a <- eval(a, data, parent.frame())
    if (length(a) != nobs) 
        bad.lengths()
    if (is.character(a)) 
        a <- as.factor(a)
    a.is.fac <- is.factor(a)
    if (have.b) {
        b.name <- deparse(b)
        b <- eval(b, data, parent.frame())
        if (length(b) != nobs) 
            bad.lengths()
        if (is.character(b)) 
            b <- as.factor(b)
        b.is.fac <- is.factor(b)
        missingrows <- which(is.na(x) | is.na(y) | is.na(a) | 
            is.na(b))
    }
    else {
        missingrows <- which(is.na(x) | is.na(y) | is.na(a))
        b <- NULL
        b.name <- ""
    }
    number <- as.integer(number)
    if (length(number) == 0L || any(number < 1)) 
        stop("'number' must be integer >= 1")
    if (any(overlap >= 1)) 
        stop("'overlap' must be < 1 (and typically >= 0).")
    bad.givens <- function() stop("invalid 'given.values'")
    if (missing(given.values)) {
        a.intervals <- if (a.is.fac) {
            i <- seq_along(a.levels <- levels(a))
            a <- as.numeric(a)
            cbind(i - 0.5, i + 0.5)
        }
        else co.intervals(unclass(a), number = number[1L], overlap = overlap[1L])
        b.intervals <- if (have.b) {
            if (b.is.fac) {
                i <- seq_along(b.levels <- levels(b))
                b <- as.numeric(b)
                cbind(i - 0.5, i + 0.5)
            }
            else {
                if (length(number) == 1L) 
                  number <- rep.int(number, 2)
                if (length(overlap) == 1L) 
                  overlap <- rep.int(overlap, 2)
                co.intervals(unclass(b), number = number[2L], 
                  overlap = overlap[2L])
            }
        }
    }
    else {
        if (!is.list(given.values)) 
            given.values <- list(given.values)
        if (length(given.values) != (if (have.b) 
            2L
        else 1L)) 
            bad.givens()
        a.intervals <- given.values[[1L]]
        if (a.is.fac) {
            a.levels <- levels(a)
            if (is.character(a.intervals)) 
                a.intervals <- match(a.intervals, a.levels)
            a.intervals <- cbind(a.intervals - 0.5, a.intervals + 
                0.5)
            a <- as.numeric(a)
        }
        else if (is.numeric(a)) {
            if (!is.numeric(a.intervals)) 
                bad.givens()
            if (!is.matrix(a.intervals) || ncol(a.intervals) != 
                2) 
                a.intervals <- cbind(a.intervals - 0.5, a.intervals + 
                  0.5)
        }
        if (have.b) {
            b.intervals <- given.values[[2L]]
            if (b.is.fac) {
                b.levels <- levels(b)
                if (is.character(b.intervals)) 
                  b.intervals <- match(b.intervals, b.levels)
                b.intervals <- cbind(b.intervals - 0.5, b.intervals + 
                  0.5)
                b <- as.numeric(b)
            }
            else if (is.numeric(b)) {
                if (!is.numeric(b.intervals)) 
                  bad.givens()
                if (!is.matrix(b.intervals) || ncol(b.intervals) != 
                  2) 
                  b.intervals <- cbind(b.intervals - 0.5, b.intervals + 
                    0.5)
            }
        }
    }
    if (anyNA(a.intervals) || (have.b && anyNA(b.intervals))) 
        bad.givens()
    if (have.b) {
        rows <- nrow(b.intervals)
        columns <- nrow(a.intervals)
        nplots <- rows * columns
        if (length(show.given) < 2L) 
            show.given <- rep.int(show.given, 2L)
    }
    else {
        nplots <- nrow(a.intervals)
        if (missing(rows)) {
            if (missing(columns)) {
                rows <- ceiling(round(sqrt(nplots)))
                columns <- ceiling(nplots/rows)
            }
            else rows <- ceiling(nplots/columns)
        }
        else if (missing(columns)) 
            columns <- ceiling(nplots/rows)
        if (rows * columns < nplots) 
            stop("rows * columns too small")
    }
    total.columns <- columns
    total.rows <- rows
    f.col <- f.row <- 1
    if (show.given[1L]) {
        total.rows <- rows + 1
        f.row <- rows/total.rows
    }
    if (have.b && show.given[2L]) {
        total.columns <- columns + 1
        f.col <- columns/total.columns
    }
    mar <- if (have.b) 
        rep.int(0, 4)
    else c(0.5, 0, 0.5, 0)
    oma <- c(5, 6, 5, 4)
    if (have.b) {
        oma[2L] <- 5
        if (!b.is.fac) 
            oma[4L] <- 5
    }
    if (a.is.fac && show.given[1L]) 
        oma[3L] <- oma[3L] - 1
    opar <- par(mfrow = c(total.rows, total.columns), oma = oma, 
        mar = mar, xaxs = "r", yaxs = "r")
    on.exit(par(opar))
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    plot.new()
    if (missing(xlim)) 
        xlim <- range(as.numeric(x), finite = TRUE)
    if (missing(ylim)) 
        ylim <- range(as.numeric(y), finite = TRUE)
    pch <- rep_len(pch, nobs)
    col <- rep_len(col, nobs)
    do.panel <- function(index, subscripts = FALSE, id) {
        Paxis <- function(side, x) {
            if (nlevels(x)) {
                lab <- axlabels(x)
                axis(side, labels = lab, at = seq(lab), xpd = NA)
            }
            else Axis(x, side = side, xpd = NA)
        }
        istart <- (total.rows - rows) + 1
        i <- total.rows - ((index - 1)%/%columns)
        j <- (index - 1)%%columns + 1
        par(mfg = c(i, j, total.rows, total.columns))
        plot.new()
        plot.window(xlim, ylim)
        if (anyNA(id)) 
            id[is.na(id)] <- FALSE
        if (any(id)) {
            grid(lty = "solid")
            if (subscripts) 
                panel(x[id], y[id], subscripts = id, col = col[id], 
                  pch = pch[id], ...)
            else panel(x[id], y[id], col = col[id], pch = pch[id], 
                ...)
        }
        if ((i == total.rows) && (j%%2 == 0)) 
            Paxis(1, x)
        else if ((i == istart || index + columns > nplots) && 
            (j%%2 == 1)) 
            Paxis(3, x)
        if ((j == 1) && ((total.rows - i)%%2 == 0)) 
            Paxis(2, y)
        else if ((j == columns || index == nplots) && ((total.rows - 
            i)%%2 == 1)) 
            Paxis(4, y)
        box()
    }
    if (have.b) {
        count <- 1
        for (i in 1L:rows) {
            for (j in 1L:columns) {
                id <- ((a.intervals[j, 1] <= a) & (a <= a.intervals[j, 
                  2]) & (b.intervals[i, 1] <= b) & (b <= b.intervals[i, 
                  2]))
                do.panel(count, subscripts, id)
                count <- count + 1
            }
        }
    }
    else {
        for (i in 1L:nplots) {
            id <- ((a.intervals[i, 1] <= a) & (a <= a.intervals[i, 
                2]))
            do.panel(i, subscripts, id)
        }
    }
    mtext(xlab[1L], side = 1, at = 0.5 * f.col, outer = TRUE, 
        line = 3.5, xpd = NA, font = par("font.lab"), cex = par("cex.lab"))
    mtext(ylab[1L], side = 2, at = 0.5 * f.row, outer = TRUE, 
        line = 3.5, xpd = NA, font = par("font.lab"), cex = par("cex.lab"))
    if (length(xlab) == 1L) 
        xlab <- c(xlab, paste("Given :", a.name))
    if (show.given[1L]) {
        par(fig = c(0, f.col, f.row, 1), mar = mar + c(3 + (!a.is.fac), 
            0, 0, 0), new = TRUE)
        plot.new()
        nint <- nrow(a.intervals)
        a.range <- range(a.intervals, finite = TRUE)
        plot.window(a.range + c(0.03, -0.03) * diff(a.range), 
            0.5 + c(0, nint))
        rect(a.intervals[, 1], 1L:nint - 0.3, a.intervals[, 2], 
            1L:nint + 0.3, col = bar.bg[if (a.is.fac) 
                "fac"
            else "num"])
        if (a.is.fac) {
            text(apply(a.intervals, 1L, mean), 1L:nint, a.levels)
        }
        else {
            Axis(a, side = 3, xpd = NA)
            axis(1, labels = FALSE)
        }
        box()
        mtext(xlab[2L], 3, line = 3 - a.is.fac, at = mean(par("usr")[1L:2]), 
            xpd = NA, font = par("font.lab"), cex = par("cex.lab"))
    }
    else {
        mtext(xlab[2L], 3, line = 3.25, outer = TRUE, at = 0.5 * 
            f.col, xpd = NA, font = par("font.lab"), cex = par("cex.lab"))
    }
    if (have.b) {
        if (length(ylab) == 1L) 
            ylab <- c(ylab, paste("Given :", b.name))
        if (show.given[2L]) {
            par(fig = c(f.col, 1, 0, f.row), mar = mar + c(0, 
                3 + (!b.is.fac), 0, 0), new = TRUE)
            plot.new()
            nint <- nrow(b.intervals)
            b.range <- range(b.intervals, finite = TRUE)
            plot.window(0.5 + c(0, nint), b.range + c(0.03, -0.03) * 
                diff(b.range))
            rect(1L:nint - 0.3, b.intervals[, 1], 1L:nint + 0.3, 
                b.intervals[, 2], col = bar.bg[if (b.is.fac) 
                  "fac"
                else "num"])
            if (b.is.fac) {
                text(1L:nint, apply(b.intervals, 1L, mean), b.levels, 
                  srt = 90)
            }
            else {
                Axis(b, side = 4, xpd = NA)
                axis(2, labels = FALSE)
            }
            box()
            mtext(ylab[2L], 4, line = 3 - b.is.fac, at = mean(par("usr")[3:4]), 
                xpd = NA, font = par("font.lab"), cex = par("cex.lab"))
        }
        else {
            mtext(ylab[2L], 4, line = 3.25, at = 0.5 * f.row, 
                outer = TRUE, xpd = NA, font = par("font.lab"), 
                cex = par("cex.lab"))
        }
    }
    if (length(missingrows)) {
        cat("\n", gettextf("Missing rows: %s", paste0(missingrows, 
            collapse = ", ")), "\n")
        invisible(missingrows)
    }
    else invisible()
}


mosaicplot <- function (x, ...) 
UseMethod("mosaicplot")


polygon <- function (x, y = NULL, density = NULL, angle = 45, border = NULL, 
    col = NA, lty = par("lty"), ..., fillOddEven = FALSE) 
{
    ..debug.hatch <- FALSE
    xy <- xy.coords(x, y)
    if (is.numeric(density) && all(is.na(density) | density < 
        0)) 
        density <- NULL
    if (!is.null(angle) && !is.null(density)) {
        polygon.onehatch <- function(x, y, x0, y0, xd, yd, ..debug.hatch = FALSE, 
            ...) {
            if (..debug.hatch) {
                points(x0, y0)
                arrows(x0, y0, x0 + xd, y0 + yd)
            }
            halfplane <- as.integer(xd * (y - y0) - yd * (x - 
                x0) <= 0)
            cross <- halfplane[-1L] - halfplane[-length(halfplane)]
            does.cross <- cross != 0
            if (!any(does.cross)) 
                return()
            x1 <- x[-length(x)][does.cross]
            y1 <- y[-length(y)][does.cross]
            x2 <- x[-1L][does.cross]
            y2 <- y[-1L][does.cross]
            t <- (((x1 - x0) * (y2 - y1) - (y1 - y0) * (x2 - 
                x1))/(xd * (y2 - y1) - yd * (x2 - x1)))
            o <- order(t)
            tsort <- t[o]
            crossings <- cumsum(cross[does.cross][o])
            if (fillOddEven) 
                crossings <- crossings%%2
            drawline <- crossings != 0
            lx <- x0 + xd * tsort
            ly <- y0 + yd * tsort
            lx1 <- lx[-length(lx)][drawline]
            ly1 <- ly[-length(ly)][drawline]
            lx2 <- lx[-1L][drawline]
            ly2 <- ly[-1L][drawline]
            segments(lx1, ly1, lx2, ly2, ...)
        }
        polygon.fullhatch <- function(x, y, density, angle, ..debug.hatch = FALSE, 
            ...) {
            x <- c(x, x[1L])
            y <- c(y, y[1L])
            angle <- angle%%180
            if (par("xlog") || par("ylog")) {
                warning("cannot hatch with logarithmic scale active")
                return()
            }
            usr <- par("usr")
            pin <- par("pin")
            upi <- c(usr[2L] - usr[1L], usr[4L] - usr[3L])/pin
            if (upi[1L] < 0) 
                angle <- 180 - angle
            if (upi[2L] < 0) 
                angle <- 180 - angle
            upi <- abs(upi)
            xd <- cos(angle/180 * pi) * upi[1L]
            yd <- sin(angle/180 * pi) * upi[2L]
            if (angle < 45 || angle > 135) {
                if (angle < 45) {
                  first.x <- max(x)
                  last.x <- min(x)
                }
                else {
                  first.x <- min(x)
                  last.x <- max(x)
                }
                y.shift <- upi[2L]/density/abs(cos(angle/180 * 
                  pi))
                x0 <- 0
                y0 <- floor((min(y) - first.x * yd/xd)/y.shift) * 
                  y.shift
                y.end <- max(y) - last.x * yd/xd
                while (y0 < y.end) {
                  polygon.onehatch(x, y, x0, y0, xd, yd, ..debug.hatch = ..debug.hatch, 
                    ...)
                  y0 <- y0 + y.shift
                }
            }
            else {
                if (angle < 90) {
                  first.y <- max(y)
                  last.y <- min(y)
                }
                else {
                  first.y <- min(y)
                  last.y <- max(y)
                }
                x.shift <- upi[1L]/density/abs(sin(angle/180 * 
                  pi))
                x0 <- floor((min(x) - first.y * xd/yd)/x.shift) * 
                  x.shift
                y0 <- 0
                x.end <- max(x) - last.y * xd/yd
                while (x0 < x.end) {
                  polygon.onehatch(x, y, x0, y0, xd, yd, ..debug.hatch = ..debug.hatch, 
                    ...)
                  x0 <- x0 + x.shift
                }
            }
        }
        if (missing(col) || is.null(col) || is.na(col)) 
            col <- par("fg")
        if (is.null(border)) 
            border <- col
        if (is.logical(border)) {
            if (!is.na(border) && border) 
                border <- col
            else border <- NA
        }
        start <- 1
        ends <- c(seq_along(xy$x)[is.na(xy$x) | is.na(xy$y)], 
            length(xy$x) + 1)
        num.polygons <- length(ends)
        col <- rep_len(col, num.polygons)
        if (length(border)) 
            border <- rep_len(border, num.polygons)
        if (length(lty)) 
            lty <- rep_len(lty, num.polygons)
        if (length(density)) 
            density <- rep_len(density, num.polygons)
        angle <- rep_len(angle, num.polygons)
        i <- 1L
        for (end in ends) {
            if (end > start) {
                if (is.null(density) || is.na(density[i]) || 
                  density[i] < 0) 
                  .External.graphics(C_polygon, xy$x[start:(end - 
                    1)], xy$y[start:(end - 1)], col[i], NA, lty[i], 
                    ...)
                else if (density[i] > 0) {
                  polygon.fullhatch(xy$x[start:(end - 1)], xy$y[start:(end - 
                    1)], col = col[i], lty = lty[i], density = density[i], 
                    angle = angle[i], ..debug.hatch = ..debug.hatch, 
                    ...)
                }
                i <- i + 1
            }
            start <- end + 1
        }
        .External.graphics(C_polygon, xy$x, xy$y, NA, border, 
            lty, ...)
    }
    else {
        if (is.logical(border)) {
            if (!is.na(border) && border) 
                border <- par("fg")
            else border <- NA
        }
        .External.graphics(C_polygon, xy$x, xy$y, col, border, 
            lty, ...)
    }
    invisible()
}


plot.window <- function (xlim, ylim, log = "", asp = NA, ...) 
{
    .External.graphics(C_plot_window, xlim, ylim, log, asp, ...)
    invisible()
}


axis <- function (side, at = NULL, labels = TRUE, tick = TRUE, line = NA, 
    pos = NA, outer = FALSE, font = NA, lty = "solid", lwd = 1, 
    lwd.ticks = lwd, col = NULL, col.ticks = NULL, hadj = NA, 
    padj = NA, ...) 
{
    if (is.null(col) && !missing(...) && !is.null(fg <- list(...)$fg)) 
        col <- fg
    invisible(.External.graphics(C_axis, side, at, as.graphicsAnnot(labels), 
        tick, line, pos, outer, font, lty, lwd, lwd.ticks, col, 
        col.ticks, hadj, padj, ...))
}


axTicks <- function (side, axp = NULL, usr = NULL, log = NULL, nintLog = NULL) 
{
    if (!(side <- as.integer(side)) %in% 1L:4L) 
        stop("'side' must be in {1:4}")
    is.x <- side%%2 == 1
    XY <- function(ch) paste0(if (is.x) 
        "x"
    else "y", ch)
    if (is.null(axp)) 
        axp <- par(XY("axp"))
    else if (!is.numeric(axp) || length(axp) != 3) 
        stop("invalid 'axp'")
    if (is.null(log)) 
        log <- par(XY("log"))
    else if (!is.logical(log) || anyNA(log)) 
        stop("invalid 'log'")
    if (log && axp[3L] > 0) {
        if (!any((iC <- as.integer(axp[3L])) == 1L:3L)) 
            stop("invalid positive 'axp[3]'")
        if (is.null(usr)) 
            usr <- par("usr")[if (is.x) 
                1:2
            else 3:4]
        else if (!is.numeric(usr) || length(usr) != 2) 
            stop("invalid 'usr'")
        if (is.null(nintLog)) 
            nintLog <- par("lab")[2L - is.x]
        if (is.finite(nintLog)) {
            axisTicks(usr, log = log, axp = axp, nint = nintLog)
        }
        else {
            if (needSort <- is.unsorted(usr)) {
                usr <- usr[2:1]
                axp <- axp[2:1]
            }
            else axp <- axp[1:2]
            ii <- round(log10(axp))
            x10 <- 10^((ii[1L] - (iC >= 2L)):ii[2L])
            r <- switch(iC, x10, c(outer(c(1, 5), x10))[-1L], 
                c(outer(c(1, 2, 5), x10))[-1L])
            if (needSort) 
                r <- rev(r)
            r[usr[1L] <= log10(r) & log10(r) <= usr[2L]]
        }
    }
    else {
        seq.int(axp[1L], axp[2L], length.out = 1L + abs(axp[3L]))
    }
}




## Package Data

# none


## Package Info

.skeleton_package_title = "The R Graphics Package"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF