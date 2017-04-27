##
## Exported symobls in package `spatial`
##

## Exported package methods

gaucov <- function (r, d, alpha = 0, se = 1) 
{
    se^2 * (alpha * (r < d/10000) + (1 - alpha) * exp(-(r/d)^2))
}


predict.trls <- function (object, x, y, ...) 
{
    if (!inherits(object, "trls")) 
        stop("'object' is not a fitted trend surface")
    n <- length(x)
    if (length(y) != n) 
        stop("'x' and 'y' differ in length")
    .C(VR_frset, as.double(object$rx[1L]), as.double(object$rx[2L]), 
        as.double(object$ry[1L]), as.double(object$ry[2L]))
    invisible(.trval(object, x, y))
}


surf.ls <- function (np, x, y, z) 
{
    if (np > 6L) 
        stop("'np' exceeds 6")
    if (is.data.frame(x)) {
        if (any(is.na(match(c("x", "y", "z"), names(x))))) 
            stop("'x' does not have columns 'x', 'y' and 'z'")
        if (missing(y)) 
            y <- x$y
        if (missing(z)) 
            z <- x$z
        x <- x$x
    }
    rx <- range(x)
    ry <- range(y)
    .C(VR_frset, as.double(rx[1L]), as.double(rx[2L]), as.double(ry[1L]), 
        as.double(ry[2L]))
    n <- length(x)
    npar <- ((np + 1) * (np + 2))/2
    f <- .spfmat(x, y, np)
    Z <- .C(VR_ls, as.double(x), as.double(y), as.double(z), 
        as.integer(n), as.integer(np), as.integer(npar), f = f, 
        r = double((npar * (npar + 1))/2), beta = double(npar), 
        wz = double(n), ifail = as.integer(0L))
    res <- list(x = x, y = y, z = z, np = np, f = f, r = Z$r, 
        beta = Z$beta, wz = Z$wz, rx = rx, ry = ry, call = match.call())
    class(res) <- "trls"
    res
}


anova.trls <- function (object, ...) 
{
    if (length(list(object, ...)) > 1L) 
        return(anovalist.trls(object, ...))
    if (!inherits(object, "trls")) 
        stop("'object' is not a fitted trend surface")
    rss <- deviance(object)
    rdf <- df.residual.trls(object)
    n <- length(object$z)
    edf <- n - rdf - 1L
    tss <- var(object$z) * (n - 1L)
    ess <- tss - rss
    ems <- ess/edf
    rms <- rss/rdf
    f <- ems/rms
    p <- 1 - pf(f, edf, rdf)
    table <- data.frame(format(c(ess, rss, tss)), format(c(edf, 
        rdf, edf + rdf)), c(format(c(ems, rms)), ""), c(format(f), 
        "", ""), c(format.pval(p), "", ""))
    dimnames(table) <- list(c("Regression", "Deviation", "Total"), 
        c("Sum Sq", "Df", "Mean Sq", "F value", "Pr(>F)"))
    cat("Analysis of Variance Table\n", "Model: ")
    cat(deparse(object$call), "\n", sep = "")
    table
}


Psim <- function (n) 
{
    z <- .C(VR_pdata, as.integer(n), x = double(n), y = double(n))
    invisible(list(x = z$x, y = z$y, call = match.call()))
}


SSI <- function (n, r) 
{
    z <- .C(VR_simmat, as.integer(n), x = double(n), y = double(n), 
        as.double(r))
    invisible(list(x = z$x, y = z$y, call = match.call()))
}


Kenvl <- function (fs, nsim, ...) 
{
    dot.expression <- as.expression(substitute(...))
    h <- Kfn(pp = eval(dot.expression), fs)
    hx <- h$x
    hu <- h$y
    hl <- h$y
    ha <- h$y^2
    for (i in 2:nsim) {
        h <- Kfn(pp = eval(dot.expression), fs)$y
        hu <- pmax(hu, h)
        hl <- pmin(hl, h)
        ha <- ha + h^2
    }
    list(x = hx, lower = hl, upper = hu, aver = sqrt(ha/nsim), 
        call = match.call())
}


semat <- function (obj, xl, xu, yl, yu, n, se) 
{
    seval <- function(obj, xp, yp) {
        npt <- length(xp)
        np <- obj$np
        npar <- ((np + 1) * (np + 2))/2
        .C(VR_prvar, z = double(npt), as.double(xp), as.double(yp), 
            as.integer(npt), as.double(obj$x), as.double(obj$y), 
            as.double(obj$l), as.double(obj$r), as.integer(length(obj$x)), 
            as.integer(np), as.integer(npar), as.double(obj$l1f))$z
    }
    if (!inherits(obj, "trgls")) 
        stop("object not from kriging")
    if (n > 999) 
        stop("'n' is too large")
    .C(VR_frset, as.double(obj$rx[1L]), as.double(obj$rx[2L]), 
        as.double(obj$ry[1L]), as.double(obj$ry[2L]))
    alph <- obj$alph
    if (length(alph) <= 1L) {
        mm <- 1.5 * sqrt((obj$rx[2L] - obj$rx[1L])^2 + (obj$ry[2L] - 
            obj$ry[1L])^2)
        alph <- c(alph[1L], obj$covmod(seq(0, mm, alph[1L])))
    }
    .C(VR_alset, as.double(alph), as.integer(length(alph)))
    dx <- (xu - xl)/n
    dy <- (yu - yl)/n
    xs <- seq(xl, xu, dx)
    ys <- seq(yl, yu, dy)
    z <- matrix(nrow = length(xs), ncol = length(ys))
    np <- obj$np
    npar <- ((np + 1) * (np + 2))/2
    if (missing(se)) 
        se <- sqrt(sum(obj$W^2)/(length(obj$x) - npar))
    for (i in seq_along(ys)) z[, i] <- se * sqrt(seval(obj, xs, 
        rep(ys[i], length(xs))))
    invisible(list(x = xs, y = ys, z = z))
}


variogram <- function (krig, nint, plotit = TRUE, ...) 
{
    z <- .C(VR_variogram, xp = double(nint), yp = double(nint), 
        nint = as.integer(nint), as.double(krig$x), as.double(krig$y), 
        if (krig$np > 0) as.double(krig$wz) else as.double(krig$z), 
        as.integer(length(krig$x)), cnt = integer(nint))
    xp <- z$xp[1L:z$nint]
    yp <- z$yp[1L:z$nint]
    if (xp[1L] > 0) {
        xp <- c(0, xp)
        yp <- c(0, yp)
    }
    z <- list(x = xp, y = yp, cnt = z$cnt[1L:z$nint])
    if (plotit) 
        if (exists(".Device")) {
            plot(xp, yp, type = "p", ...)
            invisible(z)
        }
        else {
            warning("Device not active")
            return(z)
        }
    else z
}


anovalist.trls <- function (object, ...) 
{
    objs <- list(object, ...)
    nmodels <- length(objs)
    for (i in 1L:nmodels) {
        if (!inherits(objs[[i]], "trls")) 
            stop("'object' is not a fitted trend surface")
    }
    if (nmodels == 1L) 
        return(anova.trls(object))
    models <- as.character(lapply(objs, function(x) x$call))
    df.r <- unlist(lapply(objs, df.residual.trls))
    ss.r <- unlist(lapply(objs, deviance.trls))
    df <- c(NA, -diff(df.r))
    ss <- c(NA, -diff(ss.r))
    ms <- ss/df
    f <- p <- rep(NA, nmodels)
    for (i in 2L:nmodels) {
        if (df[i] > 0) {
            f[i] <- ms[i]/(ss.r[i]/df.r[i])
            p[i] <- 1 - pf(f[i], df[i], df.r[i])
        }
        else if (df[i] < 0) {
            f[i] <- ms[i]/(ss.r[i - 1]/df.r[i - 1])
            p[i] <- 1 - pf(f[i], -df[i], df.r[i - 1])
        }
        else {
            ss[i] <- 0
        }
    }
    table <- data.frame(df.r, ss.r, df, ss, f, p)
    dimnames(table) <- list(1L:nmodels, c("Res.Df", "Res.Sum Sq", 
        "Df", "Sum Sq", "F value", "Pr(>F)"))
    title <- "Analysis of Variance Table\n"
    topnote <- paste("Model ", format(1L:nmodels), ": ", models, 
        sep = "", collapse = "\n")
    sss <- getOption("show.signif.stars")
    if (sss) 
        options(show.signif.stars = FALSE)
    print(structure(table, heading = c(title, topnote), class = c("anova", 
        "data.frame")))
    if (sss) 
        options(show.signif.stars = TRUE)
    invisible(structure(table, heading = c(title, topnote), class = c("anova", 
        "data.frame")))
}


plot.trls <- function (x, border = "red", col = NA, pch = 4, cex = 0.6, add = FALSE, 
    div = 8, ...) 
{
    if (!inherits(x, "trls")) 
        stop("'x' not a fitted trend surface")
    infl <- trls.influence(x)
    dx <- diff(range(x$x))
    dy <- diff(range(x$y))
    dxy <- (dx + dy)/2
    mDi <- max(infl$Di)
    sc <- (mDi * dxy)/div
    if (!add) 
        plot(x$x, x$y, type = "n", xlab = "", ylab = "")
    symbols(x$x, x$y, circles = sc * infl$Di, add = TRUE, fg = border, 
        inches = FALSE)
    points(x$x, x$y, pch = pch)
}


correlogram <- function (krig, nint, plotit = TRUE, ...) 
{
    z <- .C(VR_correlogram, xp = double(nint), yp = double(nint), 
        nint = as.integer(nint), as.double(krig$x), as.double(krig$y), 
        if (krig$np > 0) as.double(krig$wz) else as.double(krig$z), 
        as.integer(length(krig$x)), cnt = integer(nint))
    xp <- z$xp[1L:z$nint]
    yp <- z$yp[1L:z$nint]
    z <- list(x = xp, y = yp, cnt = z$cnt[1L:z$nint])
    if (plotit) 
        if (exists(".Device")) {
            plot(xp, yp, type = "p", ylim = c(-1, 1), ...)
            abline(0, 0)
            invisible(z)
        }
        else {
            warning("Device not active")
            return(z)
        }
    else z
}


trls.influence <- function (object) 
{
    if (!inherits(object, "trls")) 
        stop("'object' is not a fitted trend surface")
    nr <- length(object$z)
    nc <- length(object$beta)
    X <- matrix(object$f, nrow = nr, ncol = nc)
    hii <- stats::hat(X, FALSE)
    s <- sqrt(deviance(object)/df.residual.trls(object))
    r <- residuals(object)
    stresid <- r/(s * sqrt(1 - hii))
    Di <- ((stresid^2) * hii)/(nc * (1 - hii))
    invisible(list(r = r, hii = hii, stresid = stresid, Di = Di))
}


ppinit <- function (file) 
{
    tfile <- file
    t1file <- system.file("ppdata", file, package = "spatial")
    if (nzchar(t1file)) 
        tfile <- t1file
    h <- scan(tfile, list(xl = 0, xu = 0, yl = 0, yu = 0, fac = 0), 
        n = 5L, skip = 2L, quiet = TRUE)
    pp <- scan(tfile, list(x = 0, y = 0), skip = 3, quiet = TRUE)
    pp$x <- pp$x/h$fac
    pp$y <- pp$y/h$fac
    pp$area <- c(xl = h$xl/h$fac, xu = h$xu/h$fac, yl = h$yl/h$fac, 
        yu = h$yu/h$fac)
    ppregion(pp)
    invisible(pp)
}


ppregion <- function (xl = 0, xu = 1, yl = 0, yu = 1) 
{
    if (is.null(xl)) 
        stop("invalid input")
    if (is.numeric(xl)) 
        if (length(xl) != 1L || length(xu) != 1L || length(yl) != 
            1L || length(yu) != 1L) 
            stop("invalid input")
    if (is.list(xl)) {
        if (is.null(xl$area) && any(is.na(match(c("xl", "xu", 
            "yl", "yu"), names(xl))))) 
            stop("invalid input")
    }
    if (is.list(xl)) {
        if (length(xl$area)) 
            .C(VR_ppset, as.double(xl$area))
        else .C(VR_ppset, as.double(c(xl$xl, xl$xu, xl$yl, xl$yu)))
    }
    else .C(VR_ppset, as.double(c(xl, xu, yl, yu)))
    invisible()
}


sphercov <- function (r, d, alpha = 0, se = 1, D = 2) 
{
    r <- r/d
    if (D == 2) {
        t <- 1 - (2/pi) * (r * sqrt(1 - r^2) + asin(r))
    }
    else {
        t <- 1 - 1.5 * r + r^3/2
    }
    se^2 * (alpha * (r < 1/10000) + (1 - alpha) * ifelse(r < 
        1, t, 0))
}


ppgetregion <- function () 
{
    xx <- .C(VR_ppget, z = double(4))$z
    names(xx) <- c("xl", "xu", "yl", "yu")
    xx
}


prmat <- function (obj, xl, xu, yl, yu, n) 
{
    predval <- function(obj, xp, yp) {
        npt <- length(xp)
        .C(VR_krpred, z = double(npt), as.double(xp), as.double(yp), 
            as.double(obj$x), as.double(obj$y), as.integer(npt), 
            as.integer(length(obj$x)), as.double(obj$yy))$z
    }
    if (!inherits(obj, "trgls")) 
        stop("'object' not from kriging")
    if (n > 999) 
        stop("'n' is too large")
    .C(VR_frset, as.double(obj$rx[1L]), as.double(obj$rx[2L]), 
        as.double(obj$ry[1L]), as.double(obj$ry[2L]))
    alph <- obj$alph
    if (length(alph) <= 1L) {
        mm <- 1.5 * sqrt((obj$rx[2L] - obj$rx[1L])^2 + (obj$ry[2L] - 
            obj$ry[1L])^2)
        alph <- c(alph[1L], obj$covmod(seq(0, mm, alph[1L])))
    }
    .C(VR_alset, as.double(alph), as.integer(length(alph)))
    dx <- (xu - xl)/n
    dy <- (yu - yl)/n
    xs <- seq(xl, xu, dx)
    ys <- seq(yl, yu, dy)
    z <- matrix(nrow = length(xs), ncol = length(ys))
    for (i in seq_along(ys)) z[, i] <- .trval(obj, xs, rep(ys[i], 
        length(xs))) + predval(obj, xs, rep(ys[i], length(xs)))
    invisible(list(x = xs, y = ys, z = z))
}


Kaver <- function (fs, nsim, ...) 
{
    dot.expression <- as.expression(substitute(...))
    h <- Kfn(pp = eval(dot.expression), fs)
    hx <- h$x
    ha <- h$y^2
    for (i in 2:nsim) {
        h <- Kfn(pp = eval(dot.expression), fs)$y
        ha <- ha + h^2
    }
    list(x = hx, y = sqrt(ha/nsim), call = match.call())
}


pplik <- function (pp, R, ng = 50, trace = FALSE) 
{
    pplikfn <- function(cc, R, n, x, y, ng, target, trace = FALSE) {
        z <- .C(VR_plike, as.double(x), as.double(y), as.integer(n), 
            as.double(cc), as.double(R), as.integer(ng), as.double(target), 
            res = double(1))
        if (trace) {
            print(c(cc, z$res))
            utils::flush.console()
        }
        z$res
    }
    n <- length(pp$x)
    ar <- pp$area
    target <- n * (Kfn(pp, R, 1)$y)^2 * pi/((ar["xu"] - ar["xl"]) * 
        (ar["yu"] - ar["yl"]))
    if (target == 0) 
        return(0)
    tmp <- pplikfn(1, R, n, pp$x, pp$y, ng, target, FALSE)
    if (tmp <= 0) 
        return(1)
    stats::uniroot(pplikfn, interval = c(0, 1), R = R, n = n, 
        x = pp$x, y = pp$y, ng = ng, target = target, trace = trace)$root
}


trmat <- function (obj, xl, xu, yl, yu, n) 
{
    if (!inherits(obj, "trls")) 
        stop("'object' is not a fitted trend surface")
    dx <- (xu - xl)/n
    dy <- (yu - yl)/n
    x <- seq(xl, xu, dx)
    y <- seq(yl, yu, dy)
    z <- matrix(nrow = length(x), ncol = length(y))
    for (i in seq_along(y)) z[, i] <- predict(obj, x, rep(y[i], 
        length(x)))
    invisible(list(x = x, y = y, z = z))
}


expcov <- function (r, d, alpha = 0, se = 1) 
{
    se^2 * (alpha * (r < d/10000) + (1 - alpha) * exp(-r/d))
}


surf.gls <- function (np, covmod, x, y, z, nx = 1000, ...) 
{
    if (np > 6) 
        stop("'np' exceeds 6")
    if (is.data.frame(x)) {
        if (any(is.na(match(c("x", "y", "z"), names(x))))) 
            stop("'x' does not have columns 'x', 'y' and 'z'")
        if (missing(y)) 
            y <- x$y
        if (missing(z)) 
            z <- x$z
        x <- x$x
    }
    rx <- range(x)
    ry <- range(y)
    .C(VR_frset, as.double(rx[1L]), as.double(rx[2L]), as.double(ry[1L]), 
        as.double(ry[2L]))
    covmod <- covmod
    arguments <- list(...)
    if (length(arguments)) {
        onames <- names(formals(covmod))
        pm <- pmatch(names(arguments), onames, nomatch = 0L)
        if (any(pm == 0L)) 
            warning("some of '...' do not match")
        names(arguments[pm > 0L]) <- onames[pm]
        oargs <- formals(covmod)
        oargs[pm] <- arguments[pm > 0L]
        formals(covmod) <- oargs
    }
    mm <- 1.5 * sqrt((rx[2L] - rx[1L])^2 + (ry[2L] - ry[1L])^2)
    alph <- c(mm/nx, covmod(seq(0, mm, mm/nx)))
    .C(VR_alset, as.double(alph), as.integer(length(alph)))
    n <- length(x)
    npar <- ((np + 1) * (np + 2))/2
    f <- .spfmat(x, y, np)
    Z <- .C(VR_gls, as.double(x), as.double(y), as.double(z), 
        as.integer(n), as.integer(np), as.integer(npar), as.double(f), 
        l = double((n * (n + 1))/2), r = double((npar * (npar + 
            1))/2), beta = double(npar), wz = double(n), yy = double(n), 
        W = double(n), ifail = 0L, l1f = double(n * npar))
    if (Z$ifail > 0L) 
        stop("rank failure in Choleski decomposition")
    if (nx > 1000L) 
        alph <- alph[1L]
    res <- list(x = x, y = y, z = z, np = np, f = f, alph = alph, 
        l = Z$l, r = Z$r, beta = Z$beta, wz = Z$wz, yy = Z$yy, 
        W = Z$W, l1f = Z$l1f, rx = rx, ry = ry, covmod = covmod, 
        call = match.call())
    class(res) <- c("trgls", "trls")
    res
}


Kfn <- function (pp, fs, k = 100) 
{
    zz <- (c(range(pp$x), range(pp$y)) - ppgetregion()) * c(1, 
        -1, 1, -1)
    if (any(zz < 0)) 
        stop("some points outside region")
    z <- .C(VR_sp_pp2, as.double(pp$x), as.double(pp$y), as.integer(length(pp$x)), 
        k1 = as.integer(k), h = double(k), dmin = double(1L), 
        lm = double(1L), as.double(fs))
    list(y = z$h[1L:z$k1], x = (seq(1L:z$k1) * fs)/k, k = k, 
        dmin = z$dmin, lm = max(z$dmin, z$lm), call = match.call())
}


Strauss <- function (n, c = 0, r) 
{
    init <- 0
    if (!exists(".ppx")) {
        init <- 1
        z <- .C(VR_pdata, as.integer(n), x = double(n), y = double(n))
        assign(".ppx", z$x)
        assign(".ppy", z$y)
    }
    z <- .C(VR_simpat, as.integer(n), x = as.double(.ppx), y = as.double(.ppy), 
        as.double(c), as.double(r), as.integer(init))
    assign(".ppx", z$x)
    assign(".ppy", z$y)
    invisible(list(x = z$x, y = z$y, call = match.call()))
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Functions for Kriging and Point Pattern Analysis"

.skeleton_package_version = "7.3-11"

.skeleton_package_depends = "graphics,stats,utils"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF