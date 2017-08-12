##
## Exported symobls in package `stats`
##

## Exported package methods

confint.default <- function (object, parm, level = 0.95, ...) 
{
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    pct <- format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
        pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}


mahalanobis <- function (x, center, cov, inverted = FALSE, ...) 
{
    x <- if (is.vector(x)) 
        matrix(x, ncol = length(x))
    else as.matrix(x)
    if (!identical(center, FALSE)) 
        x <- sweep(x, 2L, center)
    if (!inverted) 
        cov <- solve(cov, ...)
    setNames(rowSums(x %*% cov * x), rownames(x))
}


optimize <- function (f, interval, ..., lower = min(interval), upper = max(interval), 
    maximum = FALSE, tol = .Machine$double.eps^0.25) 
{
    if (maximum) {
        val <- .External2(C_do_fmin, function(arg) -f(arg, ...), 
            lower, upper, tol)
        list(maximum = val, objective = f(val, ...))
    }
    else {
        val <- .External2(C_do_fmin, function(arg) f(arg, ...), 
            lower, upper, tol)
        list(minimum = val, objective = f(val, ...))
    }
}


hat <- function (x, intercept = TRUE) 
{
    if (is.qr(x)) 
        n <- nrow(x$qr)
    else {
        if (intercept) 
            x <- cbind(1, x)
        n <- nrow(x)
        x <- qr(x)
    }
    rowSums(qr.qy(x, diag(1, nrow = n, ncol = x$rank))^2)
}


Box.test <- function (x, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0) 
{
    if (NCOL(x) > 1) 
        stop("x is not a vector or univariate time series")
    DNAME <- deparse(substitute(x))
    type <- match.arg(type)
    cor <- acf(x, lag.max = lag, plot = FALSE, na.action = na.pass)
    n <- sum(!is.na(x))
    PARAMETER <- c(df = lag - fitdf)
    obs <- cor$acf[2:(lag + 1)]
    if (type == "Box-Pierce") {
        METHOD <- "Box-Pierce test"
        STATISTIC <- n * sum(obs^2)
        PVAL <- 1 - pchisq(STATISTIC, lag - fitdf)
    }
    else {
        METHOD <- "Box-Ljung test"
        STATISTIC <- n * (n + 2) * sum(1/seq.int(n - 1, n - lag) * 
            obs^2)
        PVAL <- 1 - pchisq(STATISTIC, lag - fitdf)
    }
    names(STATISTIC) <- "X-squared"
    structure(list(statistic = STATISTIC, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME), 
        class = "htest")
}


reorder <- function (x, ...) 
UseMethod("reorder")


phyper <- function (q, m, n, k, lower.tail = TRUE, log.p = FALSE) 
.Call(C_phyper, q, m, n, k, lower.tail, log.p)


ar.mle <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail, 
    demean = TRUE, series = NULL, ...) 
{
    if (is.null(series)) 
        series <- deparse(substitute(x))
    ists <- is.ts(x)
    if (!is.null(dim(x))) 
        stop("MLE only implemented for univariate series")
    x <- na.action(as.ts(x))
    if (anyNA(x)) 
        stop("NAs in 'x'")
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    if (ists) 
        xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.vector(x)
    n.used <- length(x)
    order.max <- if (is.null(order.max)) 
        min(n.used - 1L, 12L, floor(10 * log10(n.used)))
    else round(order.max)
    if (order.max < 0L) 
        stop("'order.max' must be >= 0")
    else if (order.max >= n.used) 
        stop("'order.max' must be < 'n.used'")
    if (aic) {
        coefs <- matrix(NA, order.max + 1L, order.max + 1L)
        var.pred <- numeric(order.max + 1L)
        xaic <- numeric(order.max + 1L)
        xm <- if (demean) 
            mean(x)
        else 0
        coefs[1, 1L] <- xm
        var0 <- sum((x - xm)^2)/n.used
        var.pred[1L] <- var0
        xaic[1L] <- n.used * log(var0) + 2 * demean + 2 + n.used + 
            n.used * log(2 * pi)
        for (i in seq_len(order.max)) {
            fit <- arima0(x, order = c(i, 0L, 0L), include.mean = demean)
            coefs[i + 1L, seq_len(i + demean)] <- fit$coef[seq_len(i + 
                demean)]
            xaic[i + 1L] <- fit$aic
            var.pred[i + 1L] <- fit$sigma2
        }
        xaic <- setNames(xaic - min(xaic), 0L:order.max)
        order <- (0L:order.max)[xaic == 0L]
        ar <- coefs[order + 1L, seq_len(order)]
        x.mean <- coefs[order + 1L, order + 1L]
        var.pred <- var.pred[order + 1L]
    }
    else {
        order <- order.max
        fit <- arima0(x, order = c(order, 0L, 0L), include.mean = demean)
        coefs <- fit$coef
        if (demean) {
            ar <- coefs[-length(coefs)]
            x.mean <- coefs[length(coefs)]
        }
        else {
            ar <- coefs
            x.mean <- 0
        }
        var.pred <- fit$sigma2
        xaic <- structure(0, names = order)
    }
    resid <- if (order) 
        c(rep(NA, order), embed(x - x.mean, order + 1L) %*% c(1, 
            -ar))
    else x - x.mean
    if (ists) {
        attr(resid, "tsp") <- xtsp
        attr(resid, "class") <- "ts"
    }
    res <- list(order = order, ar = ar, var.pred = var.pred, 
        x.mean = x.mean, aic = xaic, n.used = n.used, order.max = order.max, 
        partialacf = NULL, resid = resid, method = "MLE", series = series, 
        frequency = xfreq, call = match.call())
    if (order) {
        xacf <- acf(x, type = "covariance", lag.max = order, 
            plot = FALSE)$acf
        res$asy.var.coef <- solve(toeplitz(drop(xacf)[seq_len(order)])) * 
            var.pred/n.used
    }
    class(res) <- "ar"
    res
}


rhyper <- function (nn, m, n, k) 
.Call(C_rhyper, nn, m, n, k)


contr.SAS <- function (n, contrasts = TRUE, sparse = FALSE) 
{
    contr.treatment(n, base = if (is.numeric(n) && length(n) == 
        1L) 
        n
    else length(n), contrasts = contrasts, sparse = sparse)
}


na.exclude <- function (object, ...) 
UseMethod("na.exclude")


qgeom <- function (p, prob, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qgeom, p, prob, lower.tail, log.p)


is.ts <- function (x) 
inherits(x, "ts") && length(x)


plogis <- function (q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_plogis, q, location, scale, lower.tail, log.p)


drop.terms <- function (termobj, dropx = NULL, keep.response = FALSE) 
{
    if (is.null(dropx)) 
        termobj
    else {
        if (!inherits(termobj, "terms")) 
            stop(gettextf("'termobj' must be a object of class %s", 
                dQuote("terms")), domain = NA)
        newformula <- reformulate(attr(termobj, "term.labels")[-dropx], 
            if (keep.response) 
                termobj[[2L]]
            else NULL, attr(termobj, "intercept"))
        environment(newformula) <- environment(termobj)
        result <- terms(newformula, specials = names(attr(termobj, 
            "specials")))
        response <- attr(termobj, "response")
        if (response && !keep.response) 
            dropOpt <- c(response, dropx + length(response))
        else dropOpt <- dropx + max(response)
        if (!is.null(predvars <- attr(termobj, "predvars"))) {
            attr(result, "predvars") <- predvars[-(dropOpt + 
                1)]
        }
        if (!is.null(dataClasses <- attr(termobj, "dataClasses"))) {
            attr(result, "dataClasses") <- dataClasses[-dropOpt]
        }
        result
    }
}


rlogis <- function (n, location = 0, scale = 1) 
.Call(C_rlogis, n, location, scale)


convolve <- function (x, y, conj = TRUE, type = c("circular", "open", "filter")) 
{
    type <- match.arg(type)
    n <- length(x)
    ny <- length(y)
    Real <- is.numeric(x) && is.numeric(y)
    if (type == "circular") {
        if (ny != n) 
            stop("length mismatch in convolution")
    }
    else {
        n1 <- ny - 1
        x <- c(rep.int(0, n1), x)
        n <- length(y <- c(y, rep.int(0, n - 1)))
    }
    x <- fft(fft(x) * (if (conj) 
        Conj(fft(y))
    else fft(y)), inverse = TRUE)
    if (type == "filter") 
        (if (Real) 
            Re(x)
        else x)[-c(1L:n1, (n - n1 + 1L):n)]/n
    else (if (Real) 
        Re(x)
    else x)/n
}


ts.intersect <- function (..., dframe = FALSE) 
.cbind.ts(list(...), .makeNamesTs(...), dframe = dframe, union = FALSE)


KalmanSmooth <- function (y, mod, nit = 0L) 
{
    z <- .Call(C_KalmanSmooth, y, mod, as.integer(nit))
    dn <- dim(z$smooth)
    dim(z$var) <- dn[c(1L, 2L, 2L)]
    z
}


ts.union <- function (..., dframe = FALSE) 
.cbind.ts(list(...), .makeNamesTs(...), dframe = dframe, union = TRUE)


smoothEnds <- function (y, k = 3) 
{
    med3 <- function(a, b, c) {
        m <- b
        if (a < b) {
            if (c < b) 
                m <- if (a >= c) 
                  a
                else c
        }
        else {
            if (c > b) 
                m <- if (a <= c) 
                  a
                else c
        }
        m
    }
    med.odd <- function(x, n = length(x)) {
        half <- (n + 1)%/%2
        sort(x, partial = half)[half]
    }
    k <- as.integer(k)
    if (k < 0L || k%%2L == 0L) 
        stop("bandwidth 'k' must be >= 1 and odd!")
    k <- k%/%2L
    if (k < 1L) 
        return(y)
    n <- length(y)
    sm <- y
    if (k >= 2L) {
        sm[2L] <- med3(y[1L], y[2L], y[3L])
        sm[n - 1L] <- med3(y[n], y[n - 1L], y[n - 2L])
        if (k >= 3L) {
            for (i in 3:k) {
                j <- 2L * i - 1L
                sm[i] <- med.odd(y[1L:j], j)
                sm[n - i + 1L] <- med.odd(y[(n + 1L - j):n], 
                  j)
            }
        }
    }
    sm[1L] <- med3(y[1L], sm[2L], 3 * sm[2L] - 2 * sm[3L])
    sm[n] <- med3(y[n], sm[n - 1L], 3 * sm[n - 1L] - 2 * sm[n - 
        2L])
    sm
}


is.tskernel <- function (k) 
{
    inherits(k, "tskernel")
}


as.formula <- function (object, env = parent.frame()) 
{
    if (inherits(object, "formula")) 
        object
    else {
        rval <- formula(object, env = baseenv())
        if (identical(environment(rval), baseenv()) || !missing(env)) 
            environment(rval) <- env
        rval
    }
}


dexp <- function (x, rate = 1, log = FALSE) 
.Call(C_dexp, x, 1/rate, log)


inverse.gaussian <- function (link = "1/mu^2") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    okLinks <- c("inverse", "log", "identity", "1/mu^2")
    if (linktemp %in% okLinks) 
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    }
    else {
        if (inherits(link, "link-glm")) {
            stats <- link
            if (!is.null(stats$name)) 
                linktemp <- stats$name
        }
        else {
            stop(gettextf("link \"%s\" not available for inverse.gaussian family; available links are %s", 
                linktemp, paste(sQuote(okLinks), collapse = ", ")), 
                domain = NA)
        }
    }
    variance <- function(mu) mu^3
    dev.resids <- function(y, mu, wt) wt * ((y - mu)^2)/(y * 
        mu^2)
    aic <- function(y, n, mu, wt, dev) sum(wt) * (log(dev/sum(wt) * 
        2 * pi) + 1) + 3 * sum(log(y) * wt) + 2
    initialize <- expression({
        if (any(y <= 0)) stop("positive values only are allowed for the 'inverse.gaussian' family")
        n <- rep.int(1, nobs)
        mustart <- y
    })
    validmu <- function(mu) TRUE
    simfun <- function(object, nsim) {
        if (!requireNamespace("SuppDists", quietly = TRUE)) 
            stop("need CRAN package 'SuppDists' for simulation from the 'inverse.gaussian' family")
        wts <- object$prior.weights
        if (any(wts != 1)) 
            message("using weights as inverse variances")
        ftd <- fitted(object)
        SuppDists::rinvGauss(nsim * length(ftd), nu = ftd, lambda = wts/summary(object)$dispersion)
    }
    structure(list(family = "inverse.gaussian", link = linktemp, 
        linkfun = stats$linkfun, linkinv = stats$linkinv, variance = variance, 
        dev.resids = dev.resids, aic = aic, mu.eta = stats$mu.eta, 
        initialize = initialize, validmu = validmu, valideta = stats$valideta, 
        simulate = simfun), class = "family")
}


na.contiguous <- function (object, ...) 
UseMethod("na.contiguous")


line <- function (x, y = NULL) 
{
    xy <- xy.coords(x, y, setLab = FALSE)
    ok <- complete.cases(xy$x, xy$y)
    Call <- sys.call()
    structure(.Call(C_tukeyline, as.double(xy$x[ok]), as.double(xy$y[ok]), 
        Call), class = "tukeyline")
}


dgeom <- function (x, prob, log = FALSE) 
.Call(C_dgeom, x, prob, log)


qbirthday <- function (prob = 0.5, classes = 365, coincident = 2) 
{
    k <- coincident
    c <- classes
    p <- prob
    if (p <= 0) 
        return(1)
    if (p >= 1) 
        return(c * (k - 1) + 1)
    N <- exp(((k - 1) * log(c) + lgamma(k + 1) + log(-log1p(-p)))/k)
    N <- ceiling(N)
    if (pbirthday(N, c, k) < prob) {
        N <- N + 1
        while (pbirthday(N, c, k) < prob) N <- N + 1
    }
    else if (pbirthday(N - 1, c, k) >= prob) {
        N <- N - 1
        while (pbirthday(N - 1, c, k) >= prob) N <- N - 1
    }
    N
}


confint.lm <- function (object, parm, level = 0.95, ...) 
{
    cf <- coef(object)
    pnames <- names(cf)
    if (missing(parm)) 
        parm <- pnames
    else if (is.numeric(parm)) 
        parm <- pnames[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qt(a, object$df.residual)
    pct <- format.perc(a, 3)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
        pct))
    ses <- sqrt(diag(vcov(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}


cophenetic <- function (x) 
UseMethod("cophenetic")


var.test <- function (x, ...) 
UseMethod("var.test")


diffinv <- function (x, ...) 
{
    UseMethod("diffinv")
}


ar <- function (x, aic = TRUE, order.max = NULL, method = c("yule-walker", 
    "burg", "ols", "mle", "yw"), na.action = na.fail, series = deparse(substitute(x)), 
    ...) 
{
    res <- switch(match.arg(method), yw = , `yule-walker` = ar.yw(x, 
        aic = aic, order.max = order.max, na.action = na.action, 
        series = series, ...), burg = ar.burg(x, aic = aic, order.max = order.max, 
        na.action = na.action, series = series, ...), ols = ar.ols(x, 
        aic = aic, order.max = order.max, na.action = na.action, 
        series = series, ...), mle = ar.mle(x, aic = aic, order.max = order.max, 
        na.action = na.action, series = series, ...))
    res$call <- match.call()
    res
}


dummy.coef <- function (object, ...) 
UseMethod("dummy.coef")


cooks.distance <- function (model, ...) 
UseMethod("cooks.distance")


density <- function (x, ...) 
UseMethod("density")


ptukey <- function (q, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_ptukey, q, nranges, nmeans, df, lower.tail, log.p)


as.dist <- function (m, diag = FALSE, upper = FALSE) 
UseMethod("as.dist")


drop1 <- function (object, scope, ...) 
UseMethod("drop1")


rbeta <- function (n, shape1, shape2, ncp = 0) 
{
    if (is.na(ncp)) {
        warning("NAs produced")
        rep(NaN, n)
    }
    else if (ncp == 0) 
        .Call(C_rbeta, n, shape1, shape2)
    else {
        X <- rchisq(n, 2 * shape1, ncp = ncp)
        X/(X + rchisq(n, 2 * shape2))
    }
}


fligner.test <- function (x, ...) 
UseMethod("fligner.test")


naprint <- function (x, ...) 
UseMethod("naprint")


qqnorm <- function (y, ...) 
UseMethod("qqnorm")


is.stepfun <- function (x) 
is.function(x) && inherits(x, "stepfun")


lsfit <- function (x, y, wt = NULL, intercept = TRUE, tolerance = 1e-07, 
    yname = NULL) 
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    xnames <- colnames(x)
    if (is.null(xnames)) {
        if (ncol(x) == 1L) 
            xnames <- "X"
        else xnames <- paste0("X", 1L:ncol(x))
    }
    if (intercept) {
        x <- cbind(1, x)
        xnames <- c("Intercept", xnames)
    }
    if (is.null(yname) && ncol(y) > 1) 
        yname <- paste0("Y", 1L:ncol(y))
    good <- complete.cases(x, y, wt)
    dimy <- dim(as.matrix(y))
    if (any(!good)) {
        warning(sprintf(ngettext(sum(!good), "%d missing value deleted", 
            "%d missing values deleted"), sum(!good)), domain = NA)
        x <- as.matrix(x)[good, , drop = FALSE]
        y <- as.matrix(y)[good, , drop = FALSE]
        wt <- wt[good]
    }
    nrx <- NROW(x)
    ncx <- NCOL(x)
    nry <- NROW(y)
    ncy <- NCOL(y)
    nwts <- length(wt)
    if (nry != nrx) 
        stop(sprintf(paste0(ngettext(nrx, "'X' matrix has %d case (row)", 
            "'X' matrix has %d cases (rows)"), ", ", ngettext(nry, 
            "'Y' has %d case (row)", "'Y' has %d cases (rows)")), 
            nrx, nry), domain = NA)
    if (nry < ncx) 
        stop(sprintf(paste0(ngettext(nry, "only %d case", "only %d cases"), 
            ", ", ngettext(ncx, "but %d variable", "but %d variables")), 
            nry, ncx), domain = NA)
    if (!is.null(wt)) {
        if (any(wt < 0)) 
            stop("negative weights not allowed")
        if (nwts != nry) 
            stop(gettextf("number of weights = %d should equal %d (number of responses)", 
                nwts, nry), domain = NA)
        wtmult <- sqrt(wt)
        if (any(wt == 0)) {
            xzero <- as.matrix(x)[wt == 0, ]
            yzero <- as.matrix(y)[wt == 0, ]
        }
        x <- x * wtmult
        y <- y * wtmult
        invmult <- 1/ifelse(wt == 0, 1, wtmult)
    }
    z <- .Call(C_Cdqrls, x, y, tolerance, FALSE)
    resids <- array(NA, dim = dimy)
    dim(z$residuals) <- c(nry, ncy)
    if (!is.null(wt)) {
        if (any(wt == 0)) {
            if (ncx == 1L) 
                fitted.zeros <- xzero * z$coefficients
            else fitted.zeros <- xzero %*% z$coefficients
            z$residuals[wt == 0, ] <- yzero - fitted.zeros
        }
        z$residuals <- z$residuals * invmult
    }
    resids[good, ] <- z$residuals
    if (dimy[2L] == 1 && is.null(yname)) {
        resids <- drop(resids)
        names(z$coefficients) <- xnames
    }
    else {
        colnames(resids) <- yname
        colnames(z$effects) <- yname
        dim(z$coefficients) <- c(ncx, ncy)
        dimnames(z$coefficients) <- list(xnames, yname)
    }
    z$qr <- as.matrix(z$qr)
    colnames(z$qr) <- xnames
    output <- list(coefficients = z$coefficients, residuals = resids)
    if (z$rank != ncx) {
        xnames <- xnames[z$pivot]
        dimnames(z$qr) <- list(NULL, xnames)
        warning("'X' matrix was collinear")
    }
    if (!is.null(wt)) {
        weights <- rep.int(NA, dimy[1L])
        weights[good] <- wt
        output <- c(output, list(wt = weights))
    }
    rqr <- list(qt = drop(z$effects), qr = z$qr, qraux = z$qraux, 
        rank = z$rank, pivot = z$pivot, tol = z$tol)
    oldClass(rqr) <- "qr"
    output <- c(output, list(intercept = intercept, qr = rqr))
    return(output)
}


mad <- function (x, center = median(x), constant = 1.4826, na.rm = FALSE, 
    low = FALSE, high = FALSE) 
{
    if (na.rm) 
        x <- x[!is.na(x)]
    n <- length(x)
    constant * if ((low || high) && n%%2 == 0) {
        if (low && high) 
            stop("'low' and 'high' cannot be both TRUE")
        n2 <- n%/%2 + as.integer(high)
        sort(abs(x - center), partial = n2)[n2]
    }
    else median(abs(x - center))
}


shapiro.test <- function (x) 
{
    DNAME <- deparse(substitute(x))
    stopifnot(is.numeric(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if (is.na(n) || n < 3L || n > 5000L) 
        stop("sample size must be between 3 and 5000")
    rng <- x[n] - x[1L]
    if (rng == 0) 
        stop("all 'x' values are identical")
    if (rng < 1e-10) 
        x <- x/rng
    res <- .Call(C_SWilk, x)
    RVAL <- list(statistic = c(W = res[1]), p.value = res[2], 
        method = "Shapiro-Wilk normality test", data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}


qqplot <- function (x, y, plot.it = TRUE, xlab = deparse(substitute(x)), 
    ylab = deparse(substitute(y)), ...) 
{
    sx <- sort(x)
    sy <- sort(y)
    lenx <- length(sx)
    leny <- length(sy)
    if (leny < lenx) 
        sx <- approx(1L:lenx, sx, n = leny)$y
    if (leny > lenx) 
        sy <- approx(1L:leny, sy, n = lenx)$y
    if (plot.it) 
        plot(sx, sy, xlab = xlab, ylab = ylab, ...)
    invisible(list(x = sx, y = sy))
}


eff.aovlist <- function (aovlist) 
{
    Terms <- terms(aovlist)
    if (names(aovlist)[[1L]] == "(Intercept)") 
        aovlist <- aovlist[-1L]
    pure.error.strata <- sapply(aovlist, function(x) is.null(x$qr))
    aovlist <- aovlist[!pure.error.strata]
    s.labs <- names(aovlist)
    s.terms <- lapply(aovlist, function(x) {
        asgn <- x$assign[x$qr$pivot[1L:x$rank]]
        attr(terms(x), "term.labels")[asgn]
    })
    t.labs <- attr(Terms, "term.labels")
    t.labs <- t.labs[t.labs %in% unlist(s.terms)]
    eff <- matrix(0, ncol = length(t.labs), nrow = length(s.labs), 
        dimnames = list(s.labs, t.labs))
    for (i in names(s.terms)) eff[i, s.terms[[i]]] <- 1
    cs <- colSums(eff)
    if (all(cs <= 1)) 
        return(eff[, cs > 0, drop = FALSE])
    nm <- t.labs[cs > 1]
    pl <- lapply(aovlist, function(x) {
        asgn <- x$assign[x$qr$pivot[1L:x$rank]]
        sp <- split(seq_along(asgn), attr(terms(x), "term.labels")[asgn])
        sp <- sp[names(sp) %in% nm]
        sapply(sp, function(x, y) {
            y <- y[x, x, drop = FALSE]
            res <- sum(diag(y)^2)
            if (nrow(y) > 1 && sum(y^2) > 1.01 * res) 
                stop("eff.aovlist: non-orthogonal contrasts would give an incorrect answer")
            res
        }, y = x$qr$qr)
    })
    for (i in names(pl)) eff[i, names(pl[[i]])] <- pl[[i]]
    cs <- colSums(eff)
    eff <- eff/rep(cs, each = nrow(eff))
    eff[, cs != 0, drop = FALSE]
}


runif <- function (n, min = 0, max = 1) 
.Call(C_runif, n, min, max)


isoreg <- function (x, y = NULL) 
{
    xy <- xy.coords(x, y)
    x <- xy$x
    if (anyNA(x) || any(is.na(xy$y))) 
        stop("missing values not allowed")
    isOrd <- ((!is.null(xy$xlab) && xy$xlab == "Index") || !is.unsorted(x, 
        strictly = TRUE))
    if (!isOrd) {
        y <- xy$y
        ord <- order(x, -y)
        y <- y[ord]
    }
    z <- .Call(C_isoreg, if (isOrd) xy$y else y)
    structure(c(xy[c("x", "y")], z[c("yf", "yc", "iKnots")], 
        list(isOrd = isOrd, ord = if (!isOrd) ord, call = match.call())), 
        class = "isoreg")
}


profile <- function (fitted, ...) 
UseMethod("profile")


is.leaf <- function (object) 
(is.logical(L <- attr(object, "leaf"))) && L


dchisq <- function (x, df, ncp = 0, log = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_dchisq, x, df, log)
    else .Call(C_dnchisq, x, df, ncp, log)
}


contr.poly <- function (n, scores = 1:n, contrasts = TRUE, sparse = FALSE) 
{
    make.poly <- function(n, scores) {
        y <- scores - mean(scores)
        X <- outer(y, seq_len(n) - 1, "^")
        QR <- qr(X)
        z <- QR$qr
        z <- z * (row(z) == col(z))
        raw <- qr.qy(QR, z)
        Z <- sweep(raw, 2L, apply(raw, 2L, function(x) sqrt(sum(x^2))), 
            "/", check.margin = FALSE)
        colnames(Z) <- paste0("^", 1L:n - 1L)
        Z
    }
    if (is.numeric(n) && length(n) == 1L) 
        levs <- seq_len(n)
    else {
        levs <- n
        n <- length(levs)
    }
    if (n < 2) 
        stop(gettextf("contrasts not defined for %d degrees of freedom", 
            n - 1), domain = NA)
    if (n > 95) 
        stop(gettextf("orthogonal polynomials cannot be represented accurately enough for %d degrees of freedom", 
            n - 1), domain = NA)
    if (length(scores) != n) 
        stop("'scores' argument is of the wrong length")
    if (!is.numeric(scores) || anyDuplicated(scores)) 
        stop("'scores' must all be different numbers")
    contr <- make.poly(n, scores)
    if (sparse) 
        contr <- .asSparse(contr)
    if (contrasts) {
        dn <- colnames(contr)
        dn[2:min(4, n)] <- c(".L", ".Q", ".C")[1:min(3, n - 1)]
        colnames(contr) <- dn
        contr[, -1, drop = FALSE]
    }
    else {
        contr[, 1] <- 1
        contr
    }
}


stepfun <- function (x, y, f = as.numeric(right), ties = "ordered", right = FALSE) 
{
    if (is.unsorted(x)) 
        stop("stepfun: 'x' must be ordered increasingly")
    n <- length(x)
    if (n < 1) 
        stop("'x' must have length >= 1")
    n1 <- n + 1L
    if (length(y) != n1) 
        stop("'y' must be one longer than 'x'")
    rval <- approxfun(x, y[-if (right) 
        n1
    else 1], method = "constant", yleft = y[1L], yright = y[n1], 
        f = f, ties = ties)
    class(rval) <- c("stepfun", class(rval))
    attr(rval, "call") <- sys.call()
    rval
}


quasipoisson <- function (link = "log") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    okLinks <- c("log", "identity", "sqrt")
    if (linktemp %in% okLinks) 
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    }
    else {
        if (inherits(link, "link-glm")) {
            stats <- link
            if (!is.null(stats$name)) 
                linktemp <- stats$name
        }
        else {
            stop(gettextf("link \"%s\" not available for quasipoisson family; available links are %s", 
                linktemp, paste(sQuote(okLinks), collapse = ", ")), 
                domain = NA)
        }
    }
    variance <- function(mu) mu
    validmu <- function(mu) all(is.finite(mu)) && all(mu > 0)
    dev.resids <- function(y, mu, wt) {
        r <- mu * wt
        p <- which(y > 0)
        r[p] <- (wt * (y * log(y/mu) - (y - mu)))[p]
        2 * r
    }
    aic <- function(y, n, mu, wt, dev) NA
    initialize <- expression({
        if (any(y < 0)) stop("negative values not allowed for the 'quasiPoisson' family")
        n <- rep.int(1, nobs)
        mustart <- y + 0.1
    })
    structure(list(family = "quasipoisson", link = linktemp, 
        linkfun = stats$linkfun, linkinv = stats$linkinv, variance = variance, 
        dev.resids = dev.resids, aic = aic, mu.eta = stats$mu.eta, 
        initialize = initialize, validmu = validmu, valideta = stats$valideta), 
        class = "family")
}


na.omit <- function (object, ...) 
UseMethod("na.omit")


pnbinom <- function (q, size, prob, mu, lower.tail = TRUE, log.p = FALSE) 
{
    if (!missing(mu)) {
        if (!missing(prob)) 
            stop("'prob' and 'mu' both specified")
        .Call(C_pnbinom_mu, q, size, mu, lower.tail, log.p)
    }
    else .Call(C_pnbinom, q, size, prob, lower.tail, log.p)
}


ppois <- function (q, lambda, lower.tail = TRUE, log.p = FALSE) 
.Call(C_ppois, q, lambda, lower.tail, log.p)


model.matrix.default <- function (object, data = environment(object), contrasts.arg = NULL, 
    xlev = NULL, ...) 
{
    t <- if (missing(data)) 
        terms(object)
    else terms(object, data = data)
    if (is.null(attr(data, "terms"))) 
        data <- model.frame(object, data, xlev = xlev)
    else {
        deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), 
            collapse = " ")
        reorder <- match(sapply(attr(t, "variables"), deparse2)[-1L], 
            names(data))
        if (anyNA(reorder)) 
            stop("model frame and formula mismatch in model.matrix()")
        if (!identical(reorder, seq_len(ncol(data)))) 
            data <- data[, reorder, drop = FALSE]
    }
    int <- attr(t, "response")
    if (length(data)) {
        contr.funs <- as.character(getOption("contrasts"))
        namD <- names(data)
        for (i in namD) if (is.character(data[[i]])) 
            data[[i]] <- factor(data[[i]])
        isF <- vapply(data, function(x) is.factor(x) || is.logical(x), 
            NA)
        isF[int] <- FALSE
        isOF <- vapply(data, is.ordered, NA)
        for (nn in namD[isF]) if (is.null(attr(data[[nn]], "contrasts"))) 
            contrasts(data[[nn]]) <- contr.funs[1 + isOF[nn]]
        if (!is.null(contrasts.arg) && is.list(contrasts.arg)) {
            if (is.null(namC <- names(contrasts.arg))) 
                stop("invalid 'contrasts.arg' argument")
            for (nn in namC) {
                if (is.na(ni <- match(nn, namD))) 
                  warning(gettextf("variable '%s' is absent, its contrast will be ignored", 
                    nn), domain = NA)
                else {
                  ca <- contrasts.arg[[nn]]
                  if (is.matrix(ca)) 
                    contrasts(data[[ni]], ncol(ca)) <- ca
                  else contrasts(data[[ni]]) <- contrasts.arg[[nn]]
                }
            }
        }
    }
    else {
        isF <- FALSE
        data <- data.frame(x = rep(0, nrow(data)))
    }
    ans <- .External2(C_modelmatrix, t, data)
    cons <- if (any(isF)) 
        lapply(data[isF], attr, "contrasts")
    attr(ans, "contrasts") <- cons
    ans
}


family <- function (object, ...) 
UseMethod("family")


plot.ecdf <- function (x, ..., ylab = "Fn(x)", verticals = FALSE, col.01line = "gray70", 
    pch = 19) 
{
    plot.stepfun(x, ..., ylab = ylab, verticals = verticals, 
        pch = pch)
    abline(h = c(0, 1), col = col.01line, lty = 2)
}


cor.test <- function (x, ...) 
UseMethod("cor.test")


pchisq <- function (q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_pchisq, q, df, lower.tail, log.p)
    else .Call(C_pnchisq, q, df, ncp, lower.tail, log.p)
}


rchisq <- function (n, df, ncp = 0) 
{
    if (missing(ncp)) 
        .Call(C_rchisq, n, df)
    else .Call(C_rnchisq, n, df, ncp)
}


na.fail <- function (object, ...) 
UseMethod("na.fail")


heatmap <- function (x, Rowv = NULL, Colv = if (symm) "Rowv" else NULL, 
    distfun = dist, hclustfun = hclust, reorderfun = function(d, 
        w) reorder(d, w), add.expr, symm = FALSE, revC = identical(Colv, 
        "Rowv"), scale = c("row", "column", "none"), na.rm = TRUE, 
    margins = c(5, 5), ColSideColors, RowSideColors, cexRow = 0.2 + 
        1/log10(nr), cexCol = 0.2 + 1/log10(nc), labRow = NULL, 
    labCol = NULL, main = NULL, xlab = NULL, ylab = NULL, keep.dendro = FALSE, 
    verbose = getOption("verbose"), ...) 
{
    scale <- if (symm && missing(scale)) 
        "none"
    else match.arg(scale)
    if (length(di <- dim(x)) != 2 || !is.numeric(x)) 
        stop("'x' must be a numeric matrix")
    nr <- di[1L]
    nc <- di[2L]
    if (nr <= 1 || nc <= 1) 
        stop("'x' must have at least 2 rows and 2 columns")
    if (!is.numeric(margins) || length(margins) != 2L) 
        stop("'margins' must be a numeric vector of length 2")
    doRdend <- !identical(Rowv, NA)
    doCdend <- !identical(Colv, NA)
    if (!doRdend && identical(Colv, "Rowv")) 
        doCdend <- FALSE
    if (is.null(Rowv)) 
        Rowv <- rowMeans(x, na.rm = na.rm)
    if (is.null(Colv)) 
        Colv <- colMeans(x, na.rm = na.rm)
    if (doRdend) {
        if (inherits(Rowv, "dendrogram")) 
            ddr <- Rowv
        else {
            hcr <- hclustfun(distfun(x))
            ddr <- as.dendrogram(hcr)
            if (!is.logical(Rowv) || Rowv) 
                ddr <- reorderfun(ddr, Rowv)
        }
        if (nr != length(rowInd <- order.dendrogram(ddr))) 
            stop("row dendrogram ordering gave index of wrong length")
    }
    else rowInd <- 1L:nr
    if (doCdend) {
        if (inherits(Colv, "dendrogram")) 
            ddc <- Colv
        else if (identical(Colv, "Rowv")) {
            if (nr != nc) 
                stop("Colv = \"Rowv\" but nrow(x) != ncol(x)")
            ddc <- ddr
        }
        else {
            hcc <- hclustfun(distfun(if (symm) 
                x
            else t(x)))
            ddc <- as.dendrogram(hcc)
            if (!is.logical(Colv) || Colv) 
                ddc <- reorderfun(ddc, Colv)
        }
        if (nc != length(colInd <- order.dendrogram(ddc))) 
            stop("column dendrogram ordering gave index of wrong length")
    }
    else colInd <- 1L:nc
    x <- x[rowInd, colInd]
    labRow <- if (is.null(labRow)) 
        if (is.null(rownames(x))) 
            (1L:nr)[rowInd]
        else rownames(x)
    else labRow[rowInd]
    labCol <- if (is.null(labCol)) 
        if (is.null(colnames(x))) 
            (1L:nc)[colInd]
        else colnames(x)
    else labCol[colInd]
    if (scale == "row") {
        x <- sweep(x, 1L, rowMeans(x, na.rm = na.rm), check.margin = FALSE)
        sx <- apply(x, 1L, sd, na.rm = na.rm)
        x <- sweep(x, 1L, sx, "/", check.margin = FALSE)
    }
    else if (scale == "column") {
        x <- sweep(x, 2L, colMeans(x, na.rm = na.rm), check.margin = FALSE)
        sx <- apply(x, 2L, sd, na.rm = na.rm)
        x <- sweep(x, 2L, sx, "/", check.margin = FALSE)
    }
    lmat <- rbind(c(NA, 3), 2:1)
    lwid <- c(if (doRdend) 1 else 0.05, 4)
    lhei <- c((if (doCdend) 1 else 0.05) + if (!is.null(main)) 0.2 else 0, 
        4)
    if (!missing(ColSideColors)) {
        if (!is.character(ColSideColors) || length(ColSideColors) != 
            nc) 
            stop("'ColSideColors' must be a character vector of length ncol(x)")
        lmat <- rbind(lmat[1, ] + 1, c(NA, 1), lmat[2, ] + 1)
        lhei <- c(lhei[1L], 0.2, lhei[2L])
    }
    if (!missing(RowSideColors)) {
        if (!is.character(RowSideColors) || length(RowSideColors) != 
            nr) 
            stop("'RowSideColors' must be a character vector of length nrow(x)")
        lmat <- cbind(lmat[, 1] + 1, c(rep(NA, nrow(lmat) - 1), 
            1), lmat[, 2] + 1)
        lwid <- c(lwid[1L], 0.2, lwid[2L])
    }
    lmat[is.na(lmat)] <- 0
    if (verbose) {
        cat("layout: widths = ", lwid, ", heights = ", lhei, 
            "; lmat=\n")
        print(lmat)
    }
    dev.hold()
    on.exit(dev.flush())
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    layout(lmat, widths = lwid, heights = lhei, respect = TRUE)
    if (!missing(RowSideColors)) {
        par(mar = c(margins[1L], 0, 0, 0.5))
        image(rbind(if (revC) 
            nr:1L
        else 1L:nr), col = RowSideColors[rowInd], axes = FALSE)
    }
    if (!missing(ColSideColors)) {
        par(mar = c(0.5, 0, 0, margins[2L]))
        image(cbind(1L:nc), col = ColSideColors[colInd], axes = FALSE)
    }
    par(mar = c(margins[1L], 0, 0, margins[2L]))
    if (!symm || scale != "none") 
        x <- t(x)
    if (revC) {
        iy <- nr:1
        if (doRdend) 
            ddr <- rev(ddr)
        x <- x[, iy]
    }
    else iy <- 1L:nr
    image(1L:nc, 1L:nr, x, xlim = 0.5 + c(0, nc), ylim = 0.5 + 
        c(0, nr), axes = FALSE, xlab = "", ylab = "", ...)
    axis(1, 1L:nc, labels = labCol, las = 2, line = -0.5, tick = 0, 
        cex.axis = cexCol)
    if (!is.null(xlab)) 
        mtext(xlab, side = 1, line = margins[1L] - 1.25)
    axis(4, iy, labels = labRow, las = 2, line = -0.5, tick = 0, 
        cex.axis = cexRow)
    if (!is.null(ylab)) 
        mtext(ylab, side = 4, line = margins[2L] - 1.25)
    if (!missing(add.expr)) 
        eval.parent(substitute(add.expr))
    par(mar = c(margins[1L], 0, 0, 0))
    if (doRdend) 
        plot(ddr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none")
    else frame()
    par(mar = c(0, 0, if (!is.null(main)) 1 else 0, margins[2L]))
    if (doCdend) 
        plot(ddc, axes = FALSE, xaxs = "i", leaflab = "none")
    else if (!is.null(main)) 
        frame()
    if (!is.null(main)) {
        par(xpd = NA)
        title(main, cex.main = 1.5 * op[["cex.main"]])
    }
    invisible(list(rowInd = rowInd, colInd = colInd, Rowv = if (keep.dendro && 
        doRdend) ddr, Colv = if (keep.dendro && doCdend) ddc))
}


rmultinom <- function (n, size, prob) 
.Call(C_rmultinom, n, size, prob)


qweibull <- function (p, shape, scale = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qweibull, p, shape, scale, lower.tail, log.p)


power.t.test <- function (n = NULL, delta = NULL, sd = 1, sig.level = 0.05, power = NULL, 
    type = c("two.sample", "one.sample", "paired"), alternative = c("two.sided", 
        "one.sided"), strict = FALSE, tol = .Machine$double.eps^0.25) 
{
    if (sum(sapply(list(n, delta, sd, power, sig.level), is.null)) != 
        1) 
        stop("exactly one of 'n', 'delta', 'sd', 'power', and 'sig.level' must be NULL")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
        sig.level | sig.level > 1)) 
        stop("'sig.level' must be numeric in [0, 1]")
    type <- match.arg(type)
    alternative <- match.arg(alternative)
    tsample <- switch(type, one.sample = 1, two.sample = 2, paired = 1)
    tside <- switch(alternative, one.sided = 1, two.sided = 2)
    if (tside == 2 && !is.null(delta)) 
        delta <- abs(delta)
    p.body <- if (strict && tside == 2) 
        quote({
            nu <- (n - 1) * tsample
            qu <- qt(sig.level/tside, nu, lower.tail = FALSE)
            pt(qu, nu, ncp = sqrt(n/tsample) * delta/sd, lower.tail = FALSE) + 
                pt(-qu, nu, ncp = sqrt(n/tsample) * delta/sd, 
                  lower.tail = TRUE)
        })
    else quote({
        nu <- (n - 1) * tsample
        pt(qt(sig.level/tside, nu, lower.tail = FALSE), nu, ncp = sqrt(n/tsample) * 
            delta/sd, lower.tail = FALSE)
    })
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(n)) 
        n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+07), 
            tol = tol, extendInt = "upX")$root
    else if (is.null(sd)) 
        sd <- uniroot(function(sd) eval(p.body) - power, delta * 
            c(1e-07, 1e+07), tol = tol, extendInt = "downX")$root
    else if (is.null(delta)) 
        delta <- uniroot(function(delta) eval(p.body) - power, 
            sd * c(1e-07, 1e+07), tol = tol, extendInt = "upX")$root
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10), tol = tol, extendInt = "yes")$root
    else stop("internal error", domain = NA)
    NOTE <- switch(type, paired = "n is number of *pairs*, sd is std.dev. of *differences* within pairs", 
        two.sample = "n is number in *each* group", NULL)
    METHOD <- paste(switch(type, one.sample = "One-sample", two.sample = "Two-sample", 
        paired = "Paired"), "t test power calculation")
    structure(list(n = n, delta = delta, sd = sd, sig.level = sig.level, 
        power = power, alternative = alternative, note = NOTE, 
        method = METHOD), class = "power.htest")
}


complete.cases <- function (...) 
.External(C_compcases, ...)


loess.control <- function (surface = c("interpolate", "direct"), statistics = c("approximate", 
    "exact", "none"), trace.hat = c("exact", "approximate"), 
    cell = 0.2, iterations = 4L, iterTrace = FALSE, ...) 
{
    stopifnot(length(iterations) == 1L, !is.na(iterations), as.integer(iterations) > 
        0L, length(iterTrace) == 1L, !is.na(iterTrace), as.integer(iterTrace) >= 
        0L)
    list(surface = match.arg(surface), statistics = match.arg(statistics), 
        trace.hat = match.arg(trace.hat), cell = cell, iterations = iterations, 
        iterTrace = iterTrace)
}


qnbinom <- function (p, size, prob, mu, lower.tail = TRUE, log.p = FALSE) 
{
    if (!missing(mu)) {
        if (!missing(prob)) 
            stop("'prob' and 'mu' both specified")
        .Call(C_qnbinom_mu, p, size, mu, lower.tail, log.p)
    }
    else .Call(C_qnbinom, p, size, prob, lower.tail, log.p)
}


df <- function (x, df1, df2, ncp, log = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_df, x, df1, df2, log)
    else .Call(C_dnf, x, df1, df2, ncp, log)
}


`tsp<-` <- function (x, value) 
{
    cl <- oldClass(x)
    attr(x, "tsp") <- value
    if (is.null(value)) {
        if (inherits(x, "ts")) 
            cl <- cl["ts" != cl]
        if (inherits(x, "mts")) 
            cl <- cl["mts" != cl]
        class(x) <- cl
    }
    x
}


rnorm <- function (n, mean = 0, sd = 1) 
.Call(C_rnorm, n, mean, sd)


addmargins <- function (A, margin = seq_along(dim(A)), FUN = sum, quiet = FALSE) 
{
    if (is.null(dim(A))) 
        stop("'A' must be an array or table")
    n.sid <- length(margin)
    miss.FUN <- missing(FUN)
    if (length(FUN) == 1 && !is.list(FUN)) {
        fname <- if (!miss.FUN) 
            deparse(substitute(FUN))
        else "Sum"
        FUN <- setNames(list(FUN), fname)
    }
    if (!miss.FUN) {
        add.names <- function(thelist) {
            n <- names(thelist)
            if (is.null(n)) 
                n <- rep("", length(thelist))
            for (i in seq_along(thelist)[-1L]) {
                if (!is.call(thelist[[i]])) {
                  if (n[i] == "") 
                    n[i] <- as.character(thelist[[i]])
                }
                else if (as.character(thelist[[i]][[1L]]) == 
                  "list") 
                  thelist[[i]] <- add.names(thelist[[i]])
            }
            names(thelist) <- n
            thelist
        }
        if (mode(substitute(FUN)) == "call") 
            FUN <- eval(add.names(substitute(FUN)))
        if (is.null(names(FUN))) 
            names(FUN) <- rep("", length(FUN))
    }
    if (length(FUN) != n.sid) {
        if (length(FUN) == 1L) 
            FUN <- rep(FUN, n.sid)
        else stop(gettextf("length of FUN, %d,\n does not match the length of the margins, %d", 
            length(FUN), n.sid), domain = NA)
    }
    fnames <- vector("list", n.sid)
    for (i in seq_along(FUN)) {
        fnames[[i]] <- names(FUN)[i]
        if (is.list(FUN[[i]])) {
            topname <- fnames[[i]]
            fnames[[i]] <- names(FUN[[i]])
            blank <- fnames[[i]] == ""
            fnames[[i]][blank] <- seq_along(blank)[blank]
            if (topname == "") {
                fnames[[i]][blank] <- paste0("Margin ", margin[i], 
                  ".", fnames[[i]][blank])
            }
            else {
                fnames[[i]] <- paste0(topname, ".", fnames[[i]])
            }
        }
        else if (fnames[[i]] == "") 
            fnames[[i]] <- paste("Margin", margin[i])
    }
    expand.one <- function(A, margin, FUN, fnames) {
        if (!inherits(FUN, "list")) 
            FUN <- list(FUN)
        d <- dim(A)
        n.dim <- length(d)
        n.mar <- length(FUN)
        newdim <- d
        newdim[margin] <- newdim[margin] + n.mar
        if (is.null(dnA <- dimnames(A))) 
            dnA <- vector("list", n.dim)
        dnA[[margin]] <- c(if (is.null(dnA[[margin]])) rep("", 
            d[[margin]]) else dnA[[margin]], fnames)
        n.new <- prod(newdim)
        skip <- prod(d[1L:margin])
        runl <- skip/d[margin]
        apos <- rep(c(rep_len(TRUE, skip), rep_len(FALSE, n.mar * 
            runl)), n.new/(skip + n.mar * runl))
        values <- double(length(apos))
        values[apos] <- as.vector(A)
        for (i in 1L:n.mar) {
            mtab <- if (n.dim > 1) 
                apply(A, (1L:n.dim)[-margin], FUN[[i]])
            else FUN[[i]](A)
            select <- rep_len(FALSE, n.mar)
            select[i] <- TRUE
            mpos <- rep(c(rep_len(FALSE, skip), rep(select, each = runl)), 
                prod(dim(A))/skip)
            values[mpos] <- as.vector(mtab)
        }
        array(values, dim = newdim, dimnames = dnA)
    }
    new.A <- A
    for (i in 1L:n.sid) new.A <- expand.one(A = new.A, margin = margin[i], 
        FUN = FUN[[i]], fnames = fnames[[i]])
    if (inherits(A, "table")) 
        class(new.A) <- c("table", class(new.A))
    if (!quiet && !miss.FUN && n.sid > 1) {
        cat("Margins computed over dimensions\nin the following order:\n")
        for (i in seq_len(n.sid)) cat(paste(i), ": ", names(dimnames(A))[margin[i]], 
            "\n", sep = "")
    }
    new.A
}


asOneSidedFormula <- function (object) 
{
    if ((mode(object) == "call") && (object[[1L]] == "~")) {
        object <- eval(object)
    }
    if (inherits(object, "formula")) {
        if (length(object) != 2L) {
            stop(gettextf("formula '%s' must be of the form '~expr'", 
                deparse(as.vector(object))), domain = NA)
        }
        return(object)
    }
    do.call("~", list(switch(mode(object), name = , numeric = , 
        call = object, character = as.name(object), expression = object[[1L]], 
        stop(gettextf("'%s' cannot be of mode '%s'", substitute(object), 
            mode(object)), domain = NA))))
}


nlm <- function (f, p, ..., hessian = FALSE, typsize = rep(1, length(p)), 
    fscale = 1, print.level = 0, ndigit = 12, gradtol = 1e-06, 
    stepmax = max(1000 * sqrt(sum((p/typsize)^2)), 1000), steptol = 1e-06, 
    iterlim = 100, check.analyticals = TRUE) 
{
    print.level <- as.integer(print.level)
    if (print.level < 0 || print.level > 2) 
        stop("'print.level' must be in {0,1,2}")
    msg <- (1 + c(8, 0, 16))[1 + print.level]
    if (!check.analyticals) 
        msg <- msg + (2 + 4)
    .External2(C_nlm, function(x) f(x, ...), p, hessian, typsize, 
        fscale, msg, ndigit, gradtol, stepmax, steptol, iterlim)
}


covratio <- function (model, infl = lm.influence(model, do.coef = FALSE), 
    res = weighted.residuals(model)) 
{
    n <- nrow(qr.lm(model)$qr)
    p <- model$rank
    omh <- 1 - infl$hat
    e.star <- res/(infl$sigma * sqrt(omh))
    e.star[is.infinite(e.star)] <- NaN
    1/(omh * (((n - p - 1) + e.star^2)/(n - p))^p)
}


C <- function (object, contr, how.many, ...) 
{
    if (!nlevels(object)) 
        stop("object not interpretable as a factor")
    if (!missing(contr) && is.name(Xcontr <- substitute(contr))) 
        contr <- switch(as.character(Xcontr), poly = "contr.poly", 
            helmert = "contr.helmert", sum = "contr.sum", treatment = "contr.treatment", 
            SAS = "contr.SAS", contr)
    if (missing(contr)) {
        oc <- getOption("contrasts")
        contr <- if (length(oc) < 2L) 
            if (is.ordered(object)) 
                contr.poly
            else contr.treatment
        else oc[1 + is.ordered(object)]
    }
    if (missing(how.many) && missing(...)) 
        contrasts(object) <- contr
    else {
        if (is.character(contr)) 
            contr <- get(contr, mode = "function")
        if (is.function(contr)) 
            contr <- contr(nlevels(object), ...)
        contrasts(object, how.many) <- contr
    }
    object
}


as.stepfun <- function (x, ...) 
UseMethod("as.stepfun")


D <- function (expr, name) 
.External(C_doD, expr, name)


ar.burg <- function (x, ...) 
UseMethod("ar.burg")


cov2cor <- function (V) 
{
    p <- (d <- dim(V))[1L]
    if (!is.numeric(V) || length(d) != 2L || p != d[2L]) 
        stop("'V' is not a square numeric matrix")
    Is <- sqrt(1/diag(V))
    if (any(!is.finite(Is))) 
        warning("diag(.) had 0 or NA entries; non-finite result is doubtful")
    r <- V
    r[] <- Is * V * rep(Is, each = p)
    r[cbind(1L:p, 1L:p)] <- 1
    r
}


pgeom <- function (q, prob, lower.tail = TRUE, log.p = FALSE) 
.Call(C_pgeom, q, prob, lower.tail, log.p)


chisq.test <- function (x, y = NULL, correct = TRUE, p = rep(1/length(x), length(x)), 
    rescale.p = FALSE, simulate.p.value = FALSE, B = 2000) 
{
    DNAME <- deparse(substitute(x))
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (is.matrix(x)) {
        if (min(dim(x)) == 1L) 
            x <- as.vector(x)
    }
    if (!is.matrix(x) && !is.null(y)) {
        if (length(x) != length(y)) 
            stop("'x' and 'y' must have the same length")
        DNAME2 <- deparse(substitute(y))
        xname <- if (length(DNAME) > 1L || nchar(DNAME, "w") > 
            30) 
            ""
        else DNAME
        yname <- if (length(DNAME2) > 1L || nchar(DNAME2, "w") > 
            30) 
            ""
        else DNAME2
        OK <- complete.cases(x, y)
        x <- factor(x[OK])
        y <- factor(y[OK])
        if ((nlevels(x) < 2L) || (nlevels(y) < 2L)) 
            stop("'x' and 'y' must have at least 2 levels")
        x <- table(x, y)
        names(dimnames(x)) <- c(xname, yname)
        DNAME <- paste(paste(DNAME, collapse = "\n"), "and", 
            paste(DNAME2, collapse = "\n"))
    }
    if (any(x < 0) || anyNA(x)) 
        stop("all entries of 'x' must be nonnegative and finite")
    if ((n <- sum(x)) == 0) 
        stop("at least one entry of 'x' must be positive")
    if (simulate.p.value) {
        setMETH <- function() METHOD <<- paste(METHOD, "with simulated p-value\n\t (based on", 
            B, "replicates)")
        almost.1 <- 1 - 64 * .Machine$double.eps
    }
    if (is.matrix(x)) {
        METHOD <- "Pearson's Chi-squared test"
        nr <- as.integer(nrow(x))
        nc <- as.integer(ncol(x))
        if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
            stop("invalid nrow(x) or ncol(x)", domain = NA)
        sr <- rowSums(x)
        sc <- colSums(x)
        E <- outer(sr, sc, "*")/n
        v <- function(r, c, n) c * r * (n - r) * (n - c)/n^3
        V <- outer(sr, sc, v, n)
        dimnames(E) <- dimnames(x)
        if (simulate.p.value && all(sr > 0) && all(sc > 0)) {
            setMETH()
            tmp <- .Call(C_chisq_sim, sr, sc, B, E)
            STATISTIC <- sum(sort((x - E)^2/E, decreasing = TRUE))
            PARAMETER <- NA
            PVAL <- (1 + sum(tmp >= almost.1 * STATISTIC))/(B + 
                1)
        }
        else {
            if (simulate.p.value) 
                warning("cannot compute simulated p-value with zero marginals")
            if (correct && nrow(x) == 2L && ncol(x) == 2L) {
                YATES <- min(0.5, abs(x - E))
                if (YATES > 0) 
                  METHOD <- paste(METHOD, "with Yates' continuity correction")
            }
            else YATES <- 0
            STATISTIC <- sum((abs(x - E) - YATES)^2/E)
            PARAMETER <- (nr - 1L) * (nc - 1L)
            PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
        }
    }
    else {
        if (length(dim(x)) > 2L) 
            stop("invalid 'x'")
        if (length(x) == 1L) 
            stop("'x' must at least have 2 elements")
        if (length(x) != length(p)) 
            stop("'x' and 'p' must have the same number of elements")
        if (any(p < 0)) 
            stop("probabilities must be non-negative.")
        if (abs(sum(p) - 1) > sqrt(.Machine$double.eps)) {
            if (rescale.p) 
                p <- p/sum(p)
            else stop("probabilities must sum to 1.")
        }
        METHOD <- "Chi-squared test for given probabilities"
        E <- n * p
        V <- n * p * (1 - p)
        STATISTIC <- sum((x - E)^2/E)
        names(E) <- names(x)
        if (simulate.p.value) {
            setMETH()
            nx <- length(x)
            sm <- matrix(sample.int(nx, B * n, TRUE, prob = p), 
                nrow = n)
            ss <- apply(sm, 2L, function(x, E, k) {
                sum((table(factor(x, levels = 1L:k)) - E)^2/E)
            }, E = E, k = nx)
            PARAMETER <- NA
            PVAL <- (1 + sum(ss >= almost.1 * STATISTIC))/(B + 
                1)
        }
        else {
            PARAMETER <- length(x) - 1
            PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
        }
    }
    names(STATISTIC) <- "X-squared"
    names(PARAMETER) <- "df"
    if (any(E < 5) && is.finite(PARAMETER)) 
        warning("Chi-squared approximation may be incorrect")
    structure(list(statistic = STATISTIC, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME, observed = x, 
        expected = E, residuals = (x - E)/sqrt(E), stdres = (x - 
            E)/sqrt(V)), class = "htest")
}


p.adjust <- function (p, method = p.adjust.methods, n = length(p)) 
{
    method <- match.arg(method)
    if (method == "fdr") 
        method <- "BH"
    nm <- names(p)
    p <- as.numeric(p)
    p0 <- setNames(p, nm)
    if (all(nna <- !is.na(p))) 
        nna <- TRUE
    p <- p[nna]
    lp <- length(p)
    stopifnot(n >= lp)
    if (n <= 1) 
        return(p0)
    if (n == 2 && method == "hommel") 
        method <- "hochberg"
    p0[nna] <- switch(method, bonferroni = pmin(1, n * p), holm = {
        i <- seq_len(lp)
        o <- order(p)
        ro <- order(o)
        pmin(1, cummax((n - i + 1L) * p[o]))[ro]
    }, hommel = {
        if (n > lp) p <- c(p, rep.int(1, n - lp))
        i <- seq_len(n)
        o <- order(p)
        p <- p[o]
        ro <- order(o)
        q <- pa <- rep.int(min(n * p/i), n)
        for (j in (n - 1):2) {
            ij <- seq_len(n - j + 1)
            i2 <- (n - j + 2):n
            q1 <- min(j * p[i2]/(2:j))
            q[ij] <- pmin(j * p[ij], q1)
            q[i2] <- q[n - j + 1]
            pa <- pmax(pa, q)
        }
        pmax(pa, p)[if (lp < n) ro[1:lp] else ro]
    }, hochberg = {
        i <- lp:1L
        o <- order(p, decreasing = TRUE)
        ro <- order(o)
        pmin(1, cummin((n - i + 1L) * p[o]))[ro]
    }, BH = {
        i <- lp:1L
        o <- order(p, decreasing = TRUE)
        ro <- order(o)
        pmin(1, cummin(n/i * p[o]))[ro]
    }, BY = {
        i <- lp:1L
        o <- order(p, decreasing = TRUE)
        ro <- order(o)
        q <- sum(1L/(1L:n))
        pmin(1, cummin(q * n/i * p[o]))[ro]
    }, none = p)
    p0
}


nls <- function (formula, data = parent.frame(), start, control = nls.control(), 
    algorithm = c("default", "plinear", "port"), trace = FALSE, 
    subset, weights, na.action, model = FALSE, lower = -Inf, 
    upper = Inf, ...) 
{
    formula <- as.formula(formula)
    algorithm <- match.arg(algorithm)
    if (!is.list(data) && !is.environment(data)) 
        stop("'data' must be a list or an environment")
    mf <- cl <- match.call()
    varNames <- all.vars(formula)
    if (length(formula) == 2L) {
        formula[[3L]] <- formula[[2L]]
        formula[[2L]] <- 0
    }
    form2 <- formula
    form2[[2L]] <- 0
    varNamesRHS <- all.vars(form2)
    mWeights <- missing(weights)
    pnames <- if (missing(start)) {
        if (!is.null(attr(data, "parameters"))) {
            names(attr(data, "parameters"))
        }
        else {
            cll <- formula[[length(formula)]]
            fn <- as.character(cll[[1L]])
            if (is.null(func <- tryCatch(get(fn), error = function(e) NULL))) 
                func <- get(fn, envir = parent.frame())
            if (!is.null(pn <- attr(func, "pnames"))) 
                as.character(as.list(match.call(func, call = cll))[-1L][pn])
        }
    }
    else names(start)
    env <- environment(formula)
    if (is.null(env)) 
        env <- parent.frame()
    if (length(pnames)) 
        varNames <- varNames[is.na(match(varNames, pnames))]
    lenVar <- function(var) tryCatch(length(eval(as.name(var), 
        data, env)), error = function(e) -1L)
    if (length(varNames)) {
        n <- vapply(varNames, lenVar, 0)
        if (any(not.there <- n == -1L)) {
            nnn <- names(n[not.there])
            if (missing(start)) {
                if (algorithm == "plinear") 
                  stop("no starting values specified")
                warning("No starting values specified for some parameters.\n", 
                  "Initializing ", paste(sQuote(nnn), collapse = ", "), 
                  " to '1.'.\n", "Consider specifying 'start' or using a selfStart model", 
                  domain = NA)
                start <- setNames(as.list(rep_len(1, length(nnn))), 
                  nnn)
                varNames <- varNames[i <- is.na(match(varNames, 
                  nnn))]
                n <- n[i]
            }
            else stop(gettextf("parameters without starting value in 'data': %s", 
                paste(nnn, collapse = ", ")), domain = NA)
        }
    }
    else {
        if (length(pnames) && any((np <- sapply(pnames, lenVar)) == 
            -1)) {
            message(sprintf(ngettext(sum(np == -1), "fitting parameter %s without any variables", 
                "fitting parameters %s without any variables"), 
                paste(sQuote(pnames[np == -1]), collapse = ", ")), 
                domain = NA)
            n <- integer()
        }
        else stop("no parameters to fit")
    }
    respLength <- length(eval(formula[[2L]], data, env))
    if (length(n) > 0L) {
        varIndex <- n%%respLength == 0
        if (is.list(data) && diff(range(n[names(n) %in% names(data)])) > 
            0) {
            mf <- data
            if (!missing(subset)) 
                warning("argument 'subset' will be ignored")
            if (!missing(na.action)) 
                warning("argument 'na.action' will be ignored")
            if (missing(start)) 
                start <- getInitial(formula, mf)
            startEnv <- new.env(hash = FALSE, parent = environment(formula))
            for (i in names(start)) assign(i, start[[i]], envir = startEnv)
            rhs <- eval(formula[[3L]], data, startEnv)
            n <- NROW(rhs)
            wts <- if (mWeights) 
                rep_len(1, n)
            else eval(substitute(weights), data, environment(formula))
        }
        else {
            mf$formula <- as.formula(paste("~", paste(varNames[varIndex], 
                collapse = "+")), env = environment(formula))
            mf$start <- mf$control <- mf$algorithm <- mf$trace <- mf$model <- NULL
            mf$lower <- mf$upper <- NULL
            mf[[1L]] <- quote(stats::model.frame)
            mf <- eval.parent(mf)
            n <- nrow(mf)
            mf <- as.list(mf)
            wts <- if (!mWeights) 
                model.weights(mf)
            else rep_len(1, n)
        }
        if (any(wts < 0 | is.na(wts))) 
            stop("missing or negative weights not allowed")
    }
    else {
        varIndex <- logical()
        mf <- list(0)
        wts <- numeric()
    }
    if (missing(start)) 
        start <- getInitial(formula, mf)
    for (var in varNames[!varIndex]) mf[[var]] <- eval(as.name(var), 
        data, env)
    varNamesRHS <- varNamesRHS[varNamesRHS %in% varNames[varIndex]]
    m <- switch(algorithm, plinear = nlsModel.plinear(formula, 
        mf, start, wts), port = nlsModel(formula, mf, start, 
        wts, upper), nlsModel(formula, mf, start, wts))
    ctrl <- nls.control()
    if (!missing(control)) {
        control <- as.list(control)
        ctrl[names(control)] <- control
    }
    if (algorithm != "port") {
        if (!identical(lower, -Inf) || !identical(upper, +Inf)) {
            warning("upper and lower bounds ignored unless algorithm = \"port\"")
            cl$lower <- NULL
            cl$upper <- NULL
        }
        convInfo <- .Call(C_nls_iter, m, ctrl, trace)
        nls.out <- list(m = m, convInfo = convInfo, data = substitute(data), 
            call = cl)
    }
    else {
        pfit <- nls_port_fit(m, start, lower, upper, control, 
            trace, give.v = TRUE)
        iv <- pfit[["iv"]]
        msg.nls <- port_msg(iv[1L])
        conv <- (iv[1L] %in% 3:6)
        if (!conv) {
            msg <- paste("Convergence failure:", msg.nls)
            if (ctrl$warnOnly) 
                warning(msg)
            else stop(msg)
        }
        v. <- port_get_named_v(pfit[["v"]])
        cInfo <- list(isConv = conv, finIter = iv[31L], finTol = v.[["NREDUC"]], 
            nEval = c(`function` = iv[6L], gradient = iv[30L]), 
            stopCode = iv[1L], stopMessage = msg.nls)
        cl$lower <- lower
        cl$upper <- upper
        nls.out <- list(m = m, data = substitute(data), call = cl, 
            convInfo = cInfo, convergence = as.integer(!conv), 
            message = msg.nls)
    }
    nls.out$call$algorithm <- algorithm
    nls.out$call$control <- ctrl
    nls.out$call$trace <- trace
    nls.out$na.action <- attr(mf, "na.action")
    nls.out$dataClasses <- attr(attr(mf, "terms"), "dataClasses")[varNamesRHS]
    if (model) 
        nls.out$model <- mf
    if (!mWeights) 
        nls.out$weights <- wts
    nls.out$control <- control
    class(nls.out) <- "nls"
    nls.out
}


integrate <- function (f, lower, upper, ..., subdivisions = 100L, rel.tol = .Machine$double.eps^0.25, 
    abs.tol = rel.tol, stop.on.error = TRUE, keep.xy = FALSE, 
    aux = NULL) 
{
    f <- match.fun(f)
    ff <- function(x) f(x, ...)
    limit <- as.integer(subdivisions)
    if (limit < 1L || (abs.tol <= 0 && rel.tol < max(50 * .Machine$double.eps, 
        5e-29))) 
        stop("invalid parameter values")
    if (is.finite(lower) && is.finite(upper)) {
        wk <- .External(C_call_dqags, ff, rho = environment(), 
            as.double(lower), as.double(upper), as.double(abs.tol), 
            as.double(rel.tol), limit = limit)
    }
    else {
        if (anyNA(lower) || anyNA(upper)) 
            stop("a limit is NA or NaN")
        if (is.finite(lower)) {
            inf <- 1L
            bound <- lower
        }
        else if (is.finite(upper)) {
            inf <- -1L
            bound <- upper
        }
        else {
            inf <- 2L
            bound <- 0
        }
        wk <- .External(C_call_dqagi, ff, rho = environment(), 
            as.double(bound), inf, as.double(abs.tol), as.double(rel.tol), 
            limit = limit)
    }
    res <- wk[c("value", "abs.error", "subdivisions")]
    res$message <- switch(wk$ierr + 1L, "OK", "maximum number of subdivisions reached", 
        "roundoff error was detected", "extremely bad integrand behaviour", 
        "roundoff error is detected in the extrapolation table", 
        "the integral is probably divergent", "the input is invalid")
    if (wk$ierr == 6L || (wk$ierr > 0L && stop.on.error)) 
        stop(res$message)
    res$call <- match.call()
    class(res) <- "integrate"
    res
}


fivenum <- function (x, na.rm = TRUE) 
{
    xna <- is.na(x)
    if (any(xna)) {
        if (na.rm) 
            x <- x[!xna]
        else return(rep.int(NA, 5))
    }
    x <- sort(x)
    n <- length(x)
    if (n == 0) 
        rep.int(NA, 5)
    else {
        n4 <- floor((n + 3)/2)/2
        d <- c(1, n4, (n + 1)/2, n + 1 - n4, n)
        0.5 * (x[floor(d)] + x[ceiling(d)])
    }
}


weighted.mean <- function (x, w, ...) 
UseMethod("weighted.mean")


rweibull <- function (n, shape, scale = 1) 
.Call(C_rweibull, n, shape, scale)


qqline <- function (y, datax = FALSE, distribution = qnorm, probs = c(0.25, 
    0.75), qtype = 7, ...) 
{
    stopifnot(length(probs) == 2, is.function(distribution))
    y <- quantile(y, probs, names = FALSE, type = qtype, na.rm = TRUE)
    x <- distribution(probs)
    if (datax) {
        slope <- diff(x)/diff(y)
        int <- x[1L] - slope * y[1L]
    }
    else {
        slope <- diff(y)/diff(x)
        int <- y[1L] - slope * x[1L]
    }
    abline(int, slope, ...)
}


as.dendrogram <- function (object, ...) 
UseMethod("as.dendrogram")


runmed <- function (x, k, endrule = c("median", "keep", "constant"), algorithm = NULL, 
    print.level = 0) 
{
    n <- as.integer(length(x))
    if (is.na(n)) 
        stop(gettextf("invalid value of %s", "length(x)"), domain = NA)
    k <- as.integer(k)
    if (is.na(k)) 
        stop(gettextf("invalid value of %s", "'k'"), domain = NA)
    if (k < 0L) 
        stop("'k' must be positive")
    if (k%%2L == 0L) 
        warning(gettextf("'k' must be odd!  Changing 'k' to %d", 
            k <- as.integer(1 + 2 * (k%/%2))), domain = NA)
    if (n == 0L) {
        x <- double()
        attr(x, "k") <- k
        return(x)
    }
    if (k > n) 
        warning(gettextf("'k' is bigger than 'n'!  Changing 'k' to %d", 
            k <- as.integer(1 + 2 * ((n - 1)%/%2))), domain = NA)
    algorithm <- if (missing(algorithm)) {
        if (k < 20L || n < 300L) 
            "Stuetzle"
        else "Turlach"
    }
    else match.arg(algorithm, c("Stuetzle", "Turlach"))
    endrule <- match.arg(endrule)
    iend <- switch(endrule, median = , keep = 0L, constant = 1L)
    if (print.level) 
        cat("runmed(*, endrule=", endrule, ", algorithm=", algorithm, 
            ", iend=", iend, ")\n")
    res <- switch(algorithm, Turlach = .Call(C_runmed, as.double(x), 
        1, k, iend, print.level), Stuetzle = .Call(C_runmed, 
        as.double(x), 0, k, iend, print.level))
    if (endrule == "median") 
        res <- smoothEnds(res, k = k)
    attr(res, "k") <- k
    res
}


aov <- function (formula, data = NULL, projections = FALSE, qr = TRUE, 
    contrasts = NULL, ...) 
{
    Terms <- if (missing(data)) 
        terms(formula, "Error")
    else terms(formula, "Error", data = data)
    indError <- attr(Terms, "specials")$Error
    if (length(indError) > 1L) 
        stop(sprintf(ngettext(length(indError), "there are %d Error terms: only 1 is allowed", 
            "there are %d Error terms: only 1 is allowed"), length(indError)), 
            domain = NA)
    lmcall <- Call <- match.call()
    lmcall[[1L]] <- quote(stats::lm)
    lmcall$singular.ok <- TRUE
    if (projections) 
        qr <- lmcall$qr <- TRUE
    lmcall$projections <- NULL
    if (is.null(indError)) {
        fit <- eval(lmcall, parent.frame())
        fit$call <- Call
        structure(fit, class = c(if (inherits(fit, "mlm")) "maov", 
            "aov", oldClass(fit)), projections = if (projections) 
            proj(fit))
    }
    else {
        if (pmatch("weights", names(Call), 0L)) 
            stop("weights are not supported in a multistratum aov() fit")
        opcons <- options("contrasts")
        options(contrasts = c("contr.helmert", "contr.poly"))
        on.exit(options(opcons))
        allTerms <- Terms
        errorterm <- attr(Terms, "variables")[[1L + indError]]
        eTerm <- deparse(errorterm[[2L]], width.cutoff = 500L, 
            backtick = TRUE)
        intercept <- attr(Terms, "intercept")
        ecall <- lmcall
        ecall$formula <- as.formula(paste(deparse(formula[[2L]], 
            width.cutoff = 500L, backtick = TRUE), "~", eTerm, 
            if (!intercept) 
                "- 1"), env = environment(formula))
        ecall$method <- "qr"
        ecall$qr <- TRUE
        ecall$contrasts <- NULL
        er.fit <- eval(ecall, parent.frame())
        options(opcons)
        nmstrata <- attr(terms(er.fit), "term.labels")
        nmstrata <- sub("^`(.*)`$", "\\1", nmstrata)
        nmstrata <- c("(Intercept)", nmstrata)
        qr.e <- er.fit$qr
        rank.e <- er.fit$rank
        if (rank.e < NROW(er.fit$coefficients)) 
            warning("Error() model is singular")
        qty <- er.fit$residuals
        maov <- is.matrix(qty)
        asgn.e <- er.fit$assign[qr.e$pivot[1L:rank.e]]
        maxasgn <- length(nmstrata) - 1L
        nobs <- NROW(qty)
        len <- if (nobs > rank.e) {
            asgn.e[(rank.e + 1L):nobs] <- maxasgn + 1L
            nmstrata <- c(nmstrata, "Within")
            maxasgn + 2L
        }
        else maxasgn + 1L
        result <- setNames(vector("list", len), nmstrata)
        lmcall$formula <- form <- update(formula, paste(". ~ .-", 
            deparse(errorterm, width.cutoff = 500L, backtick = TRUE)))
        Terms <- terms(form)
        lmcall$method <- "model.frame"
        mf <- eval(lmcall, parent.frame())
        xlev <- .getXlevels(Terms, mf)
        resp <- model.response(mf)
        qtx <- model.matrix(Terms, mf, contrasts)
        cons <- attr(qtx, "contrasts")
        dnx <- colnames(qtx)
        asgn.t <- attr(qtx, "assign")
        if (length(wts <- model.weights(mf))) {
            wts <- sqrt(wts)
            resp <- resp * wts
            qtx <- qtx * wts
        }
        qty <- as.matrix(qr.qty(qr.e, resp))
        if ((nc <- ncol(qty)) > 1L) {
            dny <- colnames(resp)
            if (is.null(dny)) 
                dny <- paste0("Y", 1L:nc)
            dimnames(qty) <- list(seq(nrow(qty)), dny)
        }
        else dimnames(qty) <- list(seq(nrow(qty)), NULL)
        qtx <- qr.qty(qr.e, qtx)
        dimnames(qtx) <- list(seq(nrow(qtx)), dnx)
        for (i in seq_along(nmstrata)) {
            select <- asgn.e == (i - 1L)
            ni <- sum(select)
            if (!ni) 
                next
            xi <- qtx[select, , drop = FALSE]
            cols <- colSums(xi^2) > 1e-05
            if (any(cols)) {
                xi <- xi[, cols, drop = FALSE]
                attr(xi, "assign") <- asgn.t[cols]
                fiti <- lm.fit(xi, qty[select, , drop = FALSE])
                fiti$terms <- Terms
            }
            else {
                y <- qty[select, , drop = FALSE]
                fiti <- list(coefficients = numeric(), residuals = y, 
                  fitted.values = 0 * y, weights = wts, rank = 0L, 
                  df.residual = NROW(y))
            }
            if (projections) 
                fiti$projections <- proj(fiti)
            class(fiti) <- c(if (maov) "maov", "aov", oldClass(er.fit))
            result[[i]] <- fiti
        }
        structure(class = c("aovlist", "listof"), result[!vapply(result, 
            is.null, NA)], error.qr = if (qr) 
            qr.e, call = Call, weights = if (length(wts)) 
            wts, terms = allTerms, contrasts = cons, xlevels = xlev)
    }
}


dt <- function (x, df, ncp, log = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_dt, x, df, log)
    else .Call(C_dnt, x, df, ncp, log)
}


influence <- function (model, ...) 
UseMethod("influence")


rnbinom <- function (n, size, prob, mu) 
{
    if (!missing(mu)) {
        if (!missing(prob)) 
            stop("'prob' and 'mu' both specified")
        .Call(C_rnbinom_mu, n, size, mu)
    }
    else .Call(C_rnbinom, n, size, prob)
}


coefficients <- function (object, ...) 
UseMethod("coef")


tsp <- function (x) 
attr(x, "tsp")


dfbeta <- function (model, ...) 
UseMethod("dfbeta")


hasTsp <- function (x) 
{
    if (is.null(attr(x, "tsp"))) 
        attr(x, "tsp") <- c(1, NROW(x), 1)
    x
}


dbinom <- function (x, size, prob, log = FALSE) 
.Call(C_dbinom, x, size, prob, log)


wilcox.test <- function (x, ...) 
UseMethod("wilcox.test")


scatter.smooth <- function (x, y = NULL, span = 2/3, degree = 1, family = c("symmetric", 
    "gaussian"), xlab = NULL, ylab = NULL, ylim = range(y, pred$y, 
    na.rm = TRUE), evaluation = 50, ..., lpars = list()) 
{
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    ylabel <- if (!missing(y)) 
        deparse(substitute(y))
    xy <- xy.coords(x, y, xlabel, ylabel)
    x <- xy$x
    y <- xy$y
    xlab <- if (is.null(xlab)) 
        xy$xlab
    else xlab
    ylab <- if (is.null(ylab)) 
        xy$ylab
    else ylab
    pred <- loess.smooth(x, y, span, degree, family, evaluation)
    plot(x, y, ylim = ylim, xlab = xlab, ylab = ylab, ...)
    do.call(lines, c(list(pred), lpars))
    invisible()
}


start <- function (x, ...) 
UseMethod("start")


delete.response <- function (termobj) 
{
    a <- attributes(termobj)
    y <- a$response
    if (!is.null(y) && y) {
        termobj[[2L]] <- NULL
        a$response <- 0
        a$variables <- a$variables[-(1 + y)]
        a$predvars <- a$predvars[-(1 + y)]
        if (length(a$factors)) 
            a$factors <- a$factors[-y, , drop = FALSE]
        if (length(a$offset)) 
            a$offset <- ifelse(a$offset > y, a$offset - 1, a$offset)
        if (length(a$specials)) 
            for (i in seq_along(a$specials)) {
                b <- a$specials[[i]]
                a$specials[[i]] <- ifelse(b > y, b - 1, b)
            }
        attributes(termobj) <- a
    }
    termobj
}


qsignrank <- function (p, n, lower.tail = TRUE, log.p = FALSE) 
{
    on.exit(.External(C_signrank_free))
    .Call(C_qsignrank, p, n, lower.tail, log.p)
}


is.mts <- function (x) 
inherits(x, "mts")


.getXlevels <- function (Terms, m) 
{
    deparse2 <- function(x) paste(deparse(x, width.cutoff = 500L), 
        collapse = " ")
    xvars <- sapply(attr(Terms, "variables"), deparse2)[-1L]
    if ((yvar <- attr(Terms, "response")) > 0) 
        xvars <- xvars[-yvar]
    if (length(xvars)) {
        xlev <- lapply(m[xvars], function(x) if (is.factor(x)) 
            levels(x)
        else if (is.character(x)) 
            levels(as.factor(x))
        else NULL)
        xlev[!vapply(xlev, is.null, NA)]
    }
    else NULL
}


BIC <- function (object, ...) 
UseMethod("BIC")


ks.test <- function (x, y, ..., alternative = c("two.sided", "less", "greater"), 
    exact = NULL) 
{
    alternative <- match.arg(alternative)
    DNAME <- deparse(substitute(x))
    x <- x[!is.na(x)]
    n <- length(x)
    if (n < 1L) 
        stop("not enough 'x' data")
    PVAL <- NULL
    if (is.numeric(y)) {
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        y <- y[!is.na(y)]
        n.x <- as.double(n)
        n.y <- length(y)
        if (n.y < 1L) 
            stop("not enough 'y' data")
        if (is.null(exact)) 
            exact <- (n.x * n.y < 10000)
        METHOD <- "Two-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        n <- n.x * n.y/(n.x + n.y)
        w <- c(x, y)
        z <- cumsum(ifelse(order(w) <= n.x, 1/n.x, -1/n.y))
        if (length(unique(w)) < (n.x + n.y)) {
            if (exact) {
                warning("cannot compute exact p-value with ties")
                exact <- FALSE
            }
            else warning("p-value will be approximate in the presence of ties")
            z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
            TIES <- TRUE
        }
        STATISTIC <- switch(alternative, two.sided = max(abs(z)), 
            greater = max(z), less = -min(z))
        nm_alternative <- switch(alternative, two.sided = "two-sided", 
            less = "the CDF of x lies below that of y", greater = "the CDF of x lies above that of y")
        if (exact && (alternative == "two.sided") && !TIES) 
            PVAL <- 1 - .Call(C_pSmirnov2x, STATISTIC, n.x, n.y)
    }
    else {
        if (is.character(y)) 
            y <- get(y, mode = "function", envir = parent.frame())
        if (!is.function(y)) 
            stop("'y' must be numeric or a function or a string naming a valid function")
        METHOD <- "One-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        if (length(unique(x)) < n) {
            warning("ties should not be present for the Kolmogorov-Smirnov test")
            TIES <- TRUE
        }
        if (is.null(exact)) 
            exact <- (n < 100) && !TIES
        x <- y(sort(x), ...) - (0:(n - 1))/n
        STATISTIC <- switch(alternative, two.sided = max(c(x, 
            1/n - x)), greater = max(1/n - x), less = max(x))
        if (exact) {
            PVAL <- 1 - if (alternative == "two.sided") 
                .Call(C_pKolmogorov2x, STATISTIC, n)
            else {
                pkolmogorov1x <- function(x, n) {
                  if (x <= 0) 
                    return(0)
                  if (x >= 1) 
                    return(1)
                  j <- seq.int(from = 0, to = floor(n * (1 - 
                    x)))
                  1 - x * sum(exp(lchoose(n, j) + (n - j) * log(1 - 
                    x - j/n) + (j - 1) * log(x + j/n)))
                }
                pkolmogorov1x(STATISTIC, n)
            }
        }
        nm_alternative <- switch(alternative, two.sided = "two-sided", 
            less = "the CDF of x lies below the null hypothesis", 
            greater = "the CDF of x lies above the null hypothesis")
    }
    names(STATISTIC) <- switch(alternative, two.sided = "D", 
        greater = "D^+", less = "D^-")
    if (is.null(PVAL)) {
        pkstwo <- function(x, tol = 1e-06) {
            if (is.numeric(x)) 
                x <- as.double(x)
            else stop("argument 'x' must be numeric")
            p <- rep(0, length(x))
            p[is.na(x)] <- NA
            IND <- which(!is.na(x) & (x > 0))
            if (length(IND)) 
                p[IND] <- .Call(C_pKS2, p = x[IND], tol)
            p
        }
        PVAL <- ifelse(alternative == "two.sided", 1 - pkstwo(sqrt(n) * 
            STATISTIC), exp(-2 * n * STATISTIC^2))
    }
    PVAL <- min(1, max(0, PVAL))
    RVAL <- list(statistic = STATISTIC, p.value = PVAL, alternative = nm_alternative, 
        method = METHOD, data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}


.nknots.smspl <- function (n) 
{
    if (n < 50L) 
        n
    else trunc({
        a1 <- log2(50)
        a2 <- log2(100)
        a3 <- log2(140)
        a4 <- log2(200)
        if (n < 200L) 2^(a1 + (a2 - a1) * (n - 50)/150) else if (n < 
            800L) 2^(a2 + (a3 - a2) * (n - 200)/600) else if (n < 
            3200L) 2^(a3 + (a4 - a3) * (n - 800)/2400) else 200 + 
            (n - 3200)^0.2
    })
}


order.dendrogram <- function (x) 
{
    if (!inherits(x, "dendrogram")) 
        stop("'order.dendrogram' requires a dendrogram")
    if (is.list(x)) 
        unlist(x)
    else as.vector(x)
}


SSD <- function (object, ...) 
UseMethod("SSD")


HoltWinters <- function (x, alpha = NULL, beta = NULL, gamma = NULL, seasonal = c("additive", 
    "multiplicative"), start.periods = 2, l.start = NULL, b.start = NULL, 
    s.start = NULL, optim.start = c(alpha = 0.3, beta = 0.1, 
        gamma = 0.1), optim.control = list()) 
{
    x <- as.ts(x)
    seasonal <- match.arg(seasonal)
    f <- frequency(x)
    if (!is.null(alpha) && (alpha == 0)) 
        stop("cannot fit models without level ('alpha' must not be 0 or FALSE)")
    if (!all(is.null(c(alpha, beta, gamma))) && any(c(alpha, 
        beta, gamma) < 0 || c(alpha, beta, gamma) > 1)) 
        stop("'alpha', 'beta' and 'gamma' must be within the unit interval")
    if ((is.null(gamma) || gamma > 0)) {
        if (seasonal == "multiplicative" && any(x == 0)) 
            stop("data must be non-zero for multiplicative Holt-Winters")
        if (start.periods < 2) 
            stop("need at least 2 periods to compute seasonal start values")
    }
    if (!is.null(gamma) && is.logical(gamma) && !gamma) {
        expsmooth <- !is.null(beta) && is.logical(beta) && !beta
        if (is.null(l.start)) 
            l.start <- if (expsmooth) 
                x[1L]
            else x[2L]
        if (is.null(b.start)) 
            if (is.null(beta) || !is.logical(beta) || beta) 
                b.start <- x[2L] - x[1L]
        start.time <- 3 - expsmooth
        s.start <- 0
    }
    else {
        start.time <- f + 1
        wind <- start.periods * f
        st <- decompose(ts(x[1L:wind], start = start(x), frequency = f), 
            seasonal)
        if (is.null(l.start) || is.null(b.start)) {
            dat <- na.omit(st$trend)
            cf <- coef(.lm.fit(x = cbind(1, seq_along(dat)), 
                y = dat))
            if (is.null(l.start)) 
                l.start <- cf[1L]
            if (is.null(b.start)) 
                b.start <- cf[2L]
        }
        if (is.null(s.start)) 
            s.start <- st$figure
    }
    lenx <- as.integer(length(x))
    if (is.na(lenx)) 
        stop("invalid length(x)")
    len <- lenx - start.time + 1
    hw <- function(alpha, beta, gamma) .C(C_HoltWinters, as.double(x), 
        lenx, as.double(max(min(alpha, 1), 0)), as.double(max(min(beta, 
            1), 0)), as.double(max(min(gamma, 1), 0)), as.integer(start.time), 
        as.integer(!+(seasonal == "multiplicative")), as.integer(f), 
        as.integer(!is.logical(beta) || beta), as.integer(!is.logical(gamma) || 
            gamma), a = as.double(l.start), b = as.double(b.start), 
        s = as.double(s.start), SSE = as.double(0), level = double(len + 
            1L), trend = double(len + 1L), seasonal = double(len + 
            f))
    if (is.null(gamma)) {
        if (is.null(alpha)) {
            if (is.null(beta)) {
                error <- function(p) hw(p[1L], p[2L], p[3L])$SSE
                sol <- optim(optim.start, error, method = "L-BFGS-B", 
                  lower = c(0, 0, 0), upper = c(1, 1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                  1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                      sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                beta <- sol$par[2L]
                gamma <- sol$par[3L]
            }
            else {
                error <- function(p) hw(p[1L], beta, p[2L])$SSE
                sol <- optim(c(optim.start["alpha"], optim.start["gamma"]), 
                  error, method = "L-BFGS-B", lower = c(0, 0), 
                  upper = c(1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                  1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                      sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                gamma <- sol$par[2L]
            }
        }
        else {
            if (is.null(beta)) {
                error <- function(p) hw(alpha, p[1L], p[2L])$SSE
                sol <- optim(c(optim.start["beta"], optim.start["gamma"]), 
                  error, method = "L-BFGS-B", lower = c(0, 0), 
                  upper = c(1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                  1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                      sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                beta <- sol$par[1L]
                gamma <- sol$par[2L]
            }
            else {
                error <- function(p) hw(alpha, beta, p)$SSE
                gamma <- optimize(error, lower = 0, upper = 1)$minimum
            }
        }
    }
    else {
        if (is.null(alpha)) {
            if (is.null(beta)) {
                error <- function(p) hw(p[1L], p[2L], gamma)$SSE
                sol <- optim(c(optim.start["alpha"], optim.start["beta"]), 
                  error, method = "L-BFGS-B", lower = c(0, 0), 
                  upper = c(1, 1), control = optim.control)
                if (sol$convergence || any(sol$par < 0 | sol$par > 
                  1)) {
                  if (sol$convergence > 50) {
                    warning(gettextf("optimization difficulties: %s", 
                      sol$message), domain = NA)
                  }
                  else stop("optimization failure")
                }
                alpha <- sol$par[1L]
                beta <- sol$par[2L]
            }
            else {
                error <- function(p) hw(p, beta, gamma)$SSE
                alpha <- optimize(error, lower = 0, upper = 1)$minimum
            }
        }
        else {
            if (is.null(beta)) {
                error <- function(p) hw(alpha, p, gamma)$SSE
                beta <- optimize(error, lower = 0, upper = 1)$minimum
            }
        }
    }
    final.fit <- hw(alpha, beta, gamma)
    fitted <- ts(cbind(xhat = final.fit$level[-len - 1], level = final.fit$level[-len - 
        1], trend = if (!is.logical(beta) || beta) 
        final.fit$trend[-len - 1], season = if (!is.logical(gamma) || 
        gamma) 
        final.fit$seasonal[1L:len]), start = start(lag(x, k = 1 - 
        start.time)), frequency = frequency(x))
    if (!is.logical(beta) || beta) 
        fitted[, 1] <- fitted[, 1] + fitted[, "trend"]
    if (!is.logical(gamma) || gamma) 
        fitted[, 1] <- if (seasonal == "multiplicative") 
            fitted[, 1] * fitted[, "season"]
        else fitted[, 1] + fitted[, "season"]
    structure(list(fitted = fitted, x = x, alpha = alpha, beta = beta, 
        gamma = gamma, coefficients = c(a = final.fit$level[len + 
            1], b = if (!is.logical(beta) || beta) final.fit$trend[len + 
            1], s = if (!is.logical(gamma) || gamma) final.fit$seasonal[len + 
            1L:f]), seasonal = seasonal, SSE = final.fit$SSE, 
        call = match.call()), class = "HoltWinters")
}


qbeta <- function (p, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_qbeta, p, shape1, shape2, lower.tail, log.p)
    else .Call(C_qnbeta, p, shape1, shape2, ncp, lower.tail, 
        log.p)
}


pbinom <- function (q, size, prob, lower.tail = TRUE, log.p = FALSE) 
.Call(C_pbinom, q, size, prob, lower.tail, log.p)


effects <- function (object, ...) 
UseMethod("effects")


rbinom <- function (n, size, prob) 
.Call(C_rbinom, n, size, prob)


monthplot <- function (x, ...) 
UseMethod("monthplot")


acf2AR <- function (acf) 
{
    r <- as.double(drop(acf))
    order.max <- length(r) - 1
    if (order.max <= 0) 
        stop("'acf' must be of length two or more")
    z <- .Fortran(C_eureka, as.integer(order.max), r, r, coefs = double(order.max^2), 
        vars = double(order.max), double(order.max))
    nm <- paste0("ar(", 1L:order.max, ")")
    matrix(z$coefs, order.max, order.max, dimnames = list(nm, 
        1L:order.max))
}


splinefunH <- function (x, y, m) 
{
    n <- length(x)
    stopifnot(is.numeric(x), is.numeric(y), is.numeric(m), length(y) == 
        n, length(m) == n)
    if (is.unsorted(x)) {
        i <- sort.list(x)
        x <- x[i]
        y <- y[i]
        m <- m[i]
    }
    dx <- x[-1L] - x[-n]
    if (anyNA(dx) || any(dx == 0)) 
        stop("'x' must be *strictly* increasing (non - NA)")
    splinefunH0(x, y, m, dx = dx)
}


splinefun <- function (x, y = NULL, method = c("fmm", "periodic", "natural", 
    "monoH.FC", "hyman"), ties = mean) 
{
    x <- regularize.values(x, y, ties)
    y <- x$y
    x <- x$x
    nx <- as.integer(length(x))
    if (is.na(nx)) 
        stop(gettextf("invalid value of %s", "length(x)"), domain = NA)
    if (nx == 0) 
        stop("zero non-NA points")
    method <- match.arg(method)
    if (method == "periodic" && y[1L] != y[nx]) {
        warning("spline: first and last y values differ - using y[1L] for both")
        y[nx] <- y[1L]
    }
    if (method == "monoH.FC") {
        n1 <- nx - 1L
        dy <- y[-1L] - y[-nx]
        dx <- x[-1L] - x[-nx]
        Sx <- dy/dx
        m <- c(Sx[1L], (Sx[-1L] + Sx[-n1])/2, Sx[n1])
        m <- .Call(C_monoFC_m, m, Sx)
        return(splinefunH0(x0 = x, y0 = y, m = m, dx = dx))
    }
    iMeth <- match(method, c("periodic", "natural", "fmm", "monoH.FC", 
        "hyman"))
    if (iMeth == 5L) {
        dy <- diff(y)
        if (!(all(dy >= 0) || all(dy <= 0))) 
            stop("'y' must be increasing or decreasing")
    }
    z <- .Call(C_SplineCoef, min(3L, iMeth), x, y)
    if (iMeth == 5L) 
        z <- spl_coef_conv(hyman_filter(z))
    rm(x, y, nx, method, iMeth, ties)
    function(x, deriv = 0L) {
        deriv <- as.integer(deriv)
        if (deriv < 0L || deriv > 3L) 
            stop("'deriv' must be between 0 and 3")
        if (deriv > 0L) {
            z0 <- double(z$n)
            z[c("y", "b", "c")] <- switch(deriv, list(y = z$b, 
                b = 2 * z$c, c = 3 * z$d), list(y = 2 * z$c, 
                b = 6 * z$d, c = z0), list(y = 6 * z$d, b = z0, 
                c = z0))
            z[["d"]] <- z0
        }
        res <- .splinefun(x, z)
        if (deriv > 0 && z$method == 2 && any(ind <- x <= z$x[1L])) 
            res[ind] <- ifelse(deriv == 1, z$y[1L], 0)
        res
    }
}


qunif <- function (p, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qunif, p, min, max, lower.tail, log.p)


nls.control <- function (maxiter = 50, tol = 1e-05, minFactor = 1/1024, printEval = FALSE, 
    warnOnly = FALSE) 
list(maxiter = maxiter, tol = tol, minFactor = minFactor, printEval = printEval, 
    warnOnly = warnOnly)


dbeta <- function (x, shape1, shape2, ncp = 0, log = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_dbeta, x, shape1, shape2, log)
    else .Call(C_dnbeta, x, shape1, shape2, ncp, log)
}


influence.measures <- function (model) 
{
    is.influential <- function(infmat, n) {
        k <- ncol(infmat) - 4
        if (n <= k) 
            stop("too few cases, n < k")
        absmat <- abs(infmat)
        result <- cbind(absmat[, 1L:k] > 1, absmat[, k + 1] > 
            3 * sqrt(k/(n - k)), abs(1 - infmat[, k + 2]) > (3 * 
            k)/(n - k), pf(infmat[, k + 3], k, n - k) > 0.5, 
            infmat[, k + 4] > (3 * k)/n)
        dimnames(result) <- dimnames(infmat)
        result
    }
    infl <- influence(model)
    p <- model$rank
    e <- weighted.residuals(model)
    s <- sqrt(sum(e^2, na.rm = TRUE)/df.residual(model))
    mqr <- qr.lm(model)
    xxi <- chol2inv(mqr$qr, mqr$rank)
    si <- infl$sigma
    h <- infl$hat
    dfbetas <- infl$coefficients/outer(infl$sigma, sqrt(diag(xxi)))
    vn <- variable.names(model)
    vn[vn == "(Intercept)"] <- "1_"
    colnames(dfbetas) <- paste0("dfb.", abbreviate(vn))
    dffits <- e * sqrt(h)/(si * (1 - h))
    if (any(ii <- is.infinite(dffits))) 
        dffits[ii] <- NaN
    cov.ratio <- (si/s)^(2 * p)/(1 - h)
    cooks.d <- if (inherits(model, "glm")) 
        (infl$pear.res/(1 - h))^2 * h/(summary(model)$dispersion * 
            p)
    else ((e/(s * (1 - h)))^2 * h)/p
    infmat <- cbind(dfbetas, dffit = dffits, cov.r = cov.ratio, 
        cook.d = cooks.d, hat = h)
    infmat[is.infinite(infmat)] <- NaN
    is.inf <- is.influential(infmat, sum(h > 0))
    ans <- list(infmat = infmat, is.inf = is.inf, call = model$call)
    class(ans) <- "infl"
    ans
}


as.ts <- function (x, ...) 
UseMethod("as.ts")


prop.trend.test <- function (x, n, score = seq_along(x)) 
{
    method <- "Chi-squared Test for Trend in Proportions"
    dname <- paste(deparse(substitute(x)), "out of", deparse(substitute(n)))
    dname <- paste(dname, ",\n using scores:", paste(score, collapse = " "))
    x <- as.vector(x)
    n <- as.vector(n)
    p <- sum(x)/sum(n)
    w <- n/p/(1 - p)
    a <- anova(lm(freq ~ score, data = list(freq = x/n, score = as.vector(score)), 
        weights = w))
    chisq <- c(`X-squared` = a["score", "Sum Sq"])
    structure(list(statistic = chisq, parameter = c(df = 1), 
        p.value = pchisq(as.numeric(chisq), 1, lower.tail = FALSE), 
        method = method, data.name = dname), class = "htest")
}


summary.glm <- function (object, dispersion = NULL, correlation = FALSE, symbolic.cor = FALSE, 
    ...) 
{
    est.disp <- FALSE
    df.r <- object$df.residual
    if (is.null(dispersion)) 
        dispersion <- if (object$family$family %in% c("poisson", 
            "binomial")) 
            1
        else if (df.r > 0) {
            est.disp <- TRUE
            if (any(object$weights == 0)) 
                warning("observations with zero weight not used for calculating dispersion")
            sum((object$weights * object$residuals^2)[object$weights > 
                0])/df.r
        }
        else {
            est.disp <- TRUE
            NaN
        }
    aliased <- is.na(coef(object))
    p <- object$rank
    if (p > 0) {
        p1 <- 1L:p
        Qr <- qr.lm(object)
        coef.p <- object$coefficients[Qr$pivot[p1]]
        covmat.unscaled <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
        dimnames(covmat.unscaled) <- list(names(coef.p), names(coef.p))
        covmat <- dispersion * covmat.unscaled
        var.cf <- diag(covmat)
        s.err <- sqrt(var.cf)
        tvalue <- coef.p/s.err
        dn <- c("Estimate", "Std. Error")
        if (!est.disp) {
            pvalue <- 2 * pnorm(-abs(tvalue))
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
            dimnames(coef.table) <- list(names(coef.p), c(dn, 
                "z value", "Pr(>|z|)"))
        }
        else if (df.r > 0) {
            pvalue <- 2 * pt(-abs(tvalue), df.r)
            coef.table <- cbind(coef.p, s.err, tvalue, pvalue)
            dimnames(coef.table) <- list(names(coef.p), c(dn, 
                "t value", "Pr(>|t|)"))
        }
        else {
            coef.table <- cbind(coef.p, NaN, NaN, NaN)
            dimnames(coef.table) <- list(names(coef.p), c(dn, 
                "t value", "Pr(>|t|)"))
        }
        df.f <- NCOL(Qr$qr)
    }
    else {
        coef.table <- matrix(, 0L, 4L)
        dimnames(coef.table) <- list(NULL, c("Estimate", "Std. Error", 
            "t value", "Pr(>|t|)"))
        covmat.unscaled <- covmat <- matrix(, 0L, 0L)
        df.f <- length(aliased)
    }
    keep <- match(c("call", "terms", "family", "deviance", "aic", 
        "contrasts", "df.residual", "null.deviance", "df.null", 
        "iter", "na.action"), names(object), 0L)
    ans <- c(object[keep], list(deviance.resid = residuals(object, 
        type = "deviance"), coefficients = coef.table, aliased = aliased, 
        dispersion = dispersion, df = c(object$rank, df.r, df.f), 
        cov.unscaled = covmat.unscaled, cov.scaled = covmat))
    if (correlation && p > 0) {
        dd <- sqrt(diag(covmat.unscaled))
        ans$correlation <- covmat.unscaled/outer(dd, dd)
        ans$symbolic.cor <- symbolic.cor
    }
    class(ans) <- "summary.glm"
    return(ans)
}


get_all_vars <- function (formula, data = NULL, ...) 
{
    if (missing(formula)) {
        if (!missing(data) && inherits(data, "data.frame") && 
            length(attr(data, "terms"))) 
            return(data)
        formula <- as.formula(data)
    }
    else if (missing(data) && inherits(formula, "data.frame")) {
        if (length(attr(formula, "terms"))) 
            return(formula)
        data <- formula
        formula <- as.formula(data)
    }
    formula <- as.formula(formula)
    if (missing(data)) 
        data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data) && 
        !is.null(attr(data, "class"))) 
        data <- as.data.frame(data)
    else if (is.array(data)) 
        stop("'data' must be a data.frame, not a matrix or an array")
    if (!inherits(formula, "terms")) 
        formula <- terms(formula, data = data)
    env <- environment(formula)
    rownames <- .row_names_info(data, 0L)
    varnames <- all.vars(formula)
    inp <- parse(text = paste("list(", paste(varnames, collapse = ","), 
        ")"), keep.source = FALSE)
    variables <- eval(inp, data, env)
    if (is.null(rownames) && (resp <- attr(formula, "response")) > 
        0) {
        lhs <- variables[[resp]]
        rownames <- if (is.matrix(lhs)) 
            rownames(lhs)
        else names(lhs)
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1L])
    extras <- eval(extras, data, env)
    x <- setNames(as.data.frame(c(variables, extras), optional = TRUE), 
        c(varnames, extranames))
    if (!is.null(rownames)) 
        attr(x, "row.names") <- rownames
    x
}


preplot <- function (object, ...) 
UseMethod("preplot")


mood.test <- function (x, ...) 
UseMethod("mood.test")


dunif <- function (x, min = 0, max = 1, log = FALSE) 
.Call(C_dunif, x, min, max, log)


vcov <- function (object, ...) 
UseMethod("vcov")


glm.control <- function (epsilon = 1e-08, maxit = 25, trace = FALSE) 
{
    if (!is.numeric(epsilon) || epsilon <= 0) 
        stop("value of 'epsilon' must be > 0")
    if (!is.numeric(maxit) || maxit <= 0) 
        stop("maximum number of iterations must be > 0")
    list(epsilon = epsilon, maxit = maxit, trace = trace)
}


ar.ols <- function (x, aic = TRUE, order.max = NULL, na.action = na.fail, 
    demean = TRUE, intercept = demean, series = NULL, ...) 
{
    if (is.null(series)) 
        series <- deparse(substitute(x))
    rescale <- TRUE
    ists <- is.ts(x)
    x <- na.action(as.ts(x))
    if (anyNA(x)) 
        stop("NAs in 'x'")
    if (ists) 
        xtsp <- tsp(x)
    xfreq <- frequency(x)
    x <- as.matrix(x)
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    n.used <- nrow(x)
    nser <- ncol(x)
    iser <- seq_len(nser)
    if (rescale) {
        sc <- sqrt(drop(apply(x, 2L, var)))
        sc[sc == 0] <- 1
        x <- x/rep.int(sc, rep.int(n.used, nser))
    }
    else sc <- rep.int(1, nser)
    order.max <- if (is.null(order.max)) 
        min(n.used - 1L, floor(10 * log10(n.used)))
    else round(order.max)
    if (order.max < 0L) 
        stop("'order.max' must be >= 0")
    if (order.max >= n.used) 
        stop("'order.max' must be < 'n.used'")
    order.min <- if (aic) 
        0L
    else order.max
    varE <- seA <- A <- vector("list", order.max - order.min + 
        1L)
    xaic <- rep.int(Inf, order.max - order.min + 1L)
    det <- function(x) max(0, prod(diag(qr(x)$qr)) * (-1)^(ncol(x) - 
        1))
    if (demean) {
        xm <- colMeans(x)
        x <- sweep(x, 2L, xm, check.margin = FALSE)
    }
    else xm <- rep.int(0, nser)
    for (m in order.min:order.max) {
        y <- embed(x, m + 1L)
        if (intercept) {
            if (m) 
                X <- cbind(rep.int(1, nrow(y)), y[, (nser + 1L):ncol(y)])
            else X <- as.matrix(rep.int(1, nrow(y)))
        }
        else {
            if (m) 
                X <- y[, (nser + 1L):ncol(y)]
            else X <- matrix(0, nrow(y), 0)
        }
        Y <- t(y[, iser])
        N <- ncol(Y)
        XX <- t(X) %*% X
        rank <- qr(XX)$rank
        if (rank != nrow(XX)) {
            warning(paste("model order: ", m, "singularities in the computation of the projection matrix", 
                "results are only valid up to model order", m - 
                  1L), domain = NA)
            break
        }
        P <- if (ncol(XX) > 0) 
            solve(XX)
        else XX
        A[[m - order.min + 1L]] <- Y %*% X %*% P
        YH <- A[[m - order.min + 1L]] %*% t(X)
        E <- (Y - YH)
        varE[[m - order.min + 1L]] <- tcrossprod(E)/N
        varA <- P %x% (varE[[m - order.min + 1L]])
        seA[[m - order.min + 1L]] <- if (ncol(varA) > 0) 
            sqrt(diag(varA))
        else numeric()
        xaic[m - order.min + 1L] <- n.used * log(det(varE[[m - 
            order.min + 1L]])) + 2 * nser * (nser * m + intercept)
    }
    m <- if (aic) 
        which.max(xaic == min(xaic)) + order.min - 1L
    else order.max
    y <- embed(x, m + 1L)
    AA <- A[[m - order.min + 1L]]
    if (intercept) {
        xint <- AA[, 1L]
        ar <- AA[, -1L]
        X <- if (m) 
            cbind(rep.int(1, nrow(y)), y[, (nser + 1L):ncol(y)])
        else as.matrix(rep.int(1, nrow(y)))
    }
    else {
        X <- if (m) 
            y[, (nser + 1L):ncol(y)]
        else matrix(0, nrow(y), 0L)
        xint <- NULL
        ar <- AA
    }
    Y <- t(y[, iser, drop = FALSE])
    YH <- AA %*% t(X)
    E <- drop(rbind(matrix(NA, m, nser), t(Y - YH)))
    maic <- min(aic)
    xaic <- setNames(if (is.finite(maic)) 
        xaic - min(xaic)
    else ifelse(xaic == maic, 0, Inf), order.min:order.max)
    dim(ar) <- c(nser, nser, m)
    ar <- aperm(ar, c(3L, 1L, 2L))
    ses <- seA[[m - order.min + 1L]]
    if (intercept) {
        sem <- ses[iser]
        ses <- ses[-iser]
    }
    else sem <- rep.int(0, nser)
    dim(ses) <- c(nser, nser, m)
    ses <- aperm(ses, c(3L, 1L, 2L))
    var.pred <- varE[[m - order.min + 1L]]
    if (nser > 1L) {
        snames <- colnames(x)
        dimnames(ses) <- dimnames(ar) <- list(seq_len(m), snames, 
            snames)
        dimnames(var.pred) <- list(snames, snames)
        names(sem) <- colnames(E) <- snames
    }
    if (ists) {
        attr(E, "tsp") <- xtsp
        attr(E, "class") <- "ts"
    }
    if (rescale) {
        xm <- xm * sc
        if (!is.null(xint)) 
            xint <- xint * sc
        aa <- outer(sc, 1/sc)
        if (nser > 1L && m) 
            for (i in seq_len(m)) ar[i, , ] <- ar[i, , ] * aa
        var.pred <- var.pred * drop(outer(sc, sc))
        E <- E * rep.int(sc, rep.int(NROW(E), nser))
        sem <- sem * sc
        if (m) 
            for (i in seq_len(m)) ses[i, , ] <- ses[i, , ] * 
                aa
    }
    res <- list(order = m, ar = ar, var.pred = var.pred, x.mean = xm, 
        x.intercept = xint, aic = xaic, n.used = n.used, order.max = order.max, 
        partialacf = NULL, resid = E, method = "Unconstrained LS", 
        series = series, frequency = xfreq, call = match.call(), 
        asy.se.coef = list(x.mean = sem, ar = drop(ses)))
    class(res) <- "ar"
    res
}


qgamma <- function (p, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, 
    log.p = FALSE) 
{
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    .Call(C_qgamma, p, shape, scale, lower.tail, log.p)
}


dendrapply <- function (X, FUN, ...) 
{
    FUN <- match.fun(FUN)
    if (!inherits(X, "dendrogram")) 
        stop("'X' is not a dendrogram")
    Napply <- function(d) {
        r <- FUN(d, ...)
        if (!is.leaf(d)) {
            if (!is.list(r)) 
                r <- as.list(r)
            if (length(r) < (n <- length(d))) 
                r[seq_len(n)] <- vector("list", n)
            r[] <- lapply(d, Napply)
        }
        r
    }
    Napply(X)
}


arima0.diag <- function (...) 
.Defunct()


dnbinom <- function (x, size, prob, mu, log = FALSE) 
{
    if (!missing(mu)) {
        if (!missing(prob)) 
            stop("'prob' and 'mu' both specified")
        .Call(C_dnbinom_mu, x, size, mu, log)
    }
    else .Call(C_dnbinom, x, size, prob, log)
}


qnorm <- function (p, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qnorm, p, mean, sd, lower.tail, log.p)


`contrasts<-` <- function (x, how.many, value) 
{
    if (is.logical(x)) 
        x <- factor(x, levels = c(FALSE, TRUE))
    if (!is.factor(x)) 
        stop("contrasts apply only to factors")
    if (nlevels(x) < 2L) 
        stop("contrasts can be applied only to factors with 2 or more levels")
    if (is.function(value)) 
        value <- value(nlevels(x))
    if ((is.n <- is.numeric(value)) || (isS4(value) && methods::is(value, 
        "Matrix"))) {
        if (is.n) 
            value <- as.matrix(value)
        nlevs <- nlevels(x)
        if (nrow(value) != nlevs) 
            stop("wrong number of contrast matrix rows")
        n1 <- if (missing(how.many)) 
            nlevs - 1L
        else how.many
        nc <- ncol(value)
        rownames(value) <- levels(x)
        if (nc < n1) {
            if (!is.n) 
                value <- as.matrix(value)
            cm <- qr(cbind(1, value))
            if (cm$rank != nc + 1) 
                stop("singular contrast matrix")
            cm <- qr.qy(cm, diag(nlevs))[, 2L:nlevs]
            cm[, 1L:nc] <- value
            dimnames(cm) <- list(levels(x), NULL)
            if (!is.null(nmcol <- dimnames(value)[[2L]])) 
                dimnames(cm)[[2L]] <- c(nmcol, rep.int("", n1 - 
                  nc))
        }
        else cm <- value[, 1L:n1, drop = FALSE]
    }
    else if (is.character(value)) 
        cm <- value
    else if (is.null(value)) 
        cm <- NULL
    else stop("numeric contrasts or contrast name expected")
    attr(x, "contrasts") <- cm
    x
}


SSbiexp <- function (input, A1, lrc1, A2, lrc2) 
{
    .expr1 <- exp(lrc1)
    .expr4 <- exp(((-.expr1) * input))
    .expr6 <- exp(lrc2)
    .expr9 <- exp(((-.expr6) * input))
    .value <- (A1 * .expr4) + (A2 * .expr9)
    .actualArgs <- as.list(match.call()[c("A1", "lrc1", "A2", 
        "lrc2")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .grad <- array(0, c(length(.value), 4L), list(NULL, c("A1", 
            "lrc1", "A2", "lrc2")))
        .grad[, "A1"] <- .expr4
        .grad[, "lrc1"] <- -(A1 * (.expr4 * (.expr1 * input)))
        .grad[, "A2"] <- .expr9
        .grad[, "lrc2"] <- -(A2 * (.expr9 * (.expr6 * input)))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


poisson.test <- function (x, T = 1, r = 1, alternative = c("two.sided", "less", 
    "greater"), conf.level = 0.95) 
{
    DNAME <- deparse(substitute(x))
    DNAME <- paste(DNAME, "time base:", deparse(substitute(T)))
    if ((l <- length(x)) != length(T)) 
        if (length(T) == 1L) 
            T <- rep(T, l)
        else stop("'x' and 'T' have incompatible length")
    xr <- round(x)
    if (any(!is.finite(x) | (x < 0)) || max(abs(x - xr)) > 1e-07) 
        stop("'x' must be finite, nonnegative, and integer")
    x <- xr
    if (any(is.na(T) | (T < 0))) 
        stop("'T' must be nonnegative")
    if ((k <- length(x)) < 1L) 
        stop("not enough data")
    if (k > 2L) 
        stop("the case k > 2 is unimplemented")
    if (!missing(r) && (length(r) > 1 || is.na(r) || r < 0)) 
        stop("'r' must be a single positive number")
    alternative <- match.arg(alternative)
    if (k == 2) {
        RVAL <- binom.test(x, sum(x), r * T[1L]/(r * T[1L] + 
            T[2L]), alternative = alternative, conf.level = conf.level)
        RVAL$data.name <- DNAME
        RVAL$statistic <- c(count1 = x[1L])
        RVAL$parameter <- c(`expected count1` = sum(x) * r * 
            T[1L]/sum(T * c(1, r)))
        RVAL$estimate <- c(`rate ratio` = (x[1L]/T[1L])/(x[2L]/T[2L]))
        pp <- RVAL$conf.int
        RVAL$conf.int <- pp/(1 - pp) * T[2L]/T[1L]
        names(r) <- "rate ratio"
        RVAL$null.value <- r
        RVAL$method <- "Comparison of Poisson rates"
        return(RVAL)
    }
    else {
        m <- r * T
        PVAL <- switch(alternative, less = ppois(x, m), greater = ppois(x - 
            1, m, lower.tail = FALSE), two.sided = {
            if (m == 0) (x == 0) else {
                relErr <- 1 + 1e-07
                d <- dpois(x, r * T)
                if (x == m) 1 else if (x < m) {
                  N <- ceiling(2 * m - x)
                  while (dpois(N, m) > d) N <- 2 * N
                  i <- seq.int(from = ceiling(m), to = N)
                  y <- sum(dpois(i, m) <= d * relErr)
                  ppois(x, m) + ppois(N - y, m, lower.tail = FALSE)
                } else {
                  i <- seq.int(from = 0, to = floor(m))
                  y <- sum(dpois(i, m) <= d * relErr)
                  ppois(y - 1, m) + ppois(x - 1, m, lower.tail = FALSE)
                }
            }
        })
        p.L <- function(x, alpha) {
            if (x == 0) 
                0
            else qgamma(alpha, x)
        }
        p.U <- function(x, alpha) qgamma(1 - alpha, x + 1)
        CINT <- switch(alternative, less = c(0, p.U(x, 1 - conf.level)), 
            greater = c(p.L(x, 1 - conf.level), Inf), two.sided = {
                alpha <- (1 - conf.level)/2
                c(p.L(x, alpha), p.U(x, alpha))
            })/T
        attr(CINT, "conf.level") <- conf.level
        ESTIMATE <- x/T
        names(x) <- "number of events"
        names(T) <- "time base"
        names(ESTIMATE) <- names(r) <- "event rate"
        structure(list(statistic = x, parameter = T, p.value = PVAL, 
            conf.int = CINT, estimate = ESTIMATE, null.value = r, 
            alternative = alternative, method = "Exact Poisson test", 
            data.name = DNAME), class = "htest")
    }
}


add.scope <- function (terms1, terms2) 
{
    terms1 <- terms(terms1)
    terms2 <- terms(terms2)
    factor.scope(attr(terms1, "factors"), list(add = attr(terms2, 
        "factors")))$add
}


plot.spec.phase <- function (x, ci = 0.95, xlab = "frequency", ylab = "phase", ylim = c(-pi, 
    pi), type = "l", main = NULL, ci.col = "blue", ci.lty = 3, 
    ...) 
{
    nser <- NCOL(x$spec)
    gg <- 2/x$df
    if (is.null(main)) 
        main <- paste(paste("Series:", x$series), "Phase spectrum", 
            sep = "  -- ")
    if (nser == 2) {
        plot(x$freq, x$phase, type = type, xlab = xlab, ylab = ylab, 
            ylim = ylim, ...)
        coh <- sqrt(x$coh)
        cl <- asin(pmin(0.9999, qt(ci, 2/gg - 2) * sqrt(gg * 
            (coh^{
                -2
            } - 1)/(2 * (1 - gg)))))
        lines(x$freq, x$phase + cl, lty = ci.lty, col = ci.col)
        lines(x$freq, x$phase - cl, lty = ci.lty, col = ci.col)
        title(main)
    }
    else {
        dev.hold()
        on.exit(dev.flush())
        opar <- par(mfrow = c(nser - 1, nser - 1), mar = c(1.5, 
            1.5, 0.5, 0.5), oma = c(4, 4, 6, 4))
        on.exit(par(opar), add = TRUE)
        plot.new()
        for (j in 2:nser) for (i in 1L:(j - 1)) {
            par(mfg = c(j - 1, i, nser - 1, nser - 1))
            ind <- i + (j - 1) * (j - 2)/2
            plot(x$freq, x$phase[, ind], type = type, ylim = ylim, 
                axes = FALSE, xlab = "", ylab = "", ...)
            coh <- sqrt(x$coh[, ind])
            cl <- asin(pmin(0.9999, qt(ci, 2/gg - 2) * sqrt(gg * 
                (coh^{
                  -2
                } - 1)/(2 * (1 - gg)))))
            lines(x$freq, x$phase[, ind] + cl, lty = ci.lty, 
                col = ci.col)
            lines(x$freq, x$phase[, ind] - cl, lty = ci.lty, 
                col = ci.col)
            box()
            if (i == 1) {
                axis(2, xpd = NA)
                title(ylab = x$snames[j], xpd = NA)
            }
            if (j == nser) {
                axis(1, xpd = NA)
                title(xlab = x$snames[i], xpd = NA)
            }
            mtext(main, 3, 3, TRUE, 0.5, cex = par("cex.main"), 
                font = par("font.main"))
        }
    }
    invisible()
}


dlnorm <- function (x, meanlog = 0, sdlog = 1, log = FALSE) 
.Call(C_dlnorm, x, meanlog, sdlog, log)


makeARIMA <- function (phi, theta, Delta, kappa = 1e+06, SSinit = c("Gardner1980", 
    "Rossignol2011"), tol = .Machine$double.eps) 
{
    if (anyNA(phi)) 
        warning(gettextf("NAs in '%s'", "phi"), domain = NA)
    if (anyNA(theta)) 
        warning(gettextf("NAs in '%s'", "theta"), domain = NA)
    p <- length(phi)
    q <- length(theta)
    r <- max(p, q + 1L)
    d <- length(Delta)
    rd <- r + d
    Z <- c(1, rep.int(0, r - 1L), Delta)
    T <- matrix(0, rd, rd)
    if (p > 0) 
        T[1L:p, 1L] <- phi
    if (r > 1L) {
        ind <- 2:r
        T[cbind(ind - 1L, ind)] <- 1
    }
    if (d > 0L) {
        T[r + 1L, ] <- Z
        if (d > 1L) {
            ind <- r + 2:d
            T[cbind(ind, ind - 1)] <- 1
        }
    }
    if (q < r - 1L) 
        theta <- c(theta, rep.int(0, r - 1L - q))
    R <- c(1, theta, rep.int(0, d))
    V <- R %o% R
    h <- 0
    a <- rep(0, rd)
    Pn <- P <- matrix(0, rd, rd)
    if (r > 1L) 
        Pn[1L:r, 1L:r] <- switch(match.arg(SSinit), Gardner1980 = .Call(C_getQ0, 
            phi, theta), Rossignol2011 = .Call(C_getQ0bis, phi, 
            theta, tol), stop("invalid 'SSinit'"))
    else Pn[1L, 1L] <- if (p > 0) 
        1/(1 - phi^2)
    else 1
    if (d > 0L) 
        Pn[cbind(r + 1L:d, r + 1L:d)] <- kappa
    list(phi = phi, theta = theta, Delta = Delta, Z = Z, a = a, 
        P = P, T = T, V = V, h = h, Pn = Pn)
}


tsSmooth <- function (object, ...) 
UseMethod("tsSmooth")


dnorm <- function (x, mean = 0, sd = 1, log = FALSE) 
.Call(C_dnorm, x, mean, sd, log)


nobs <- function (object, ...) 
UseMethod("nobs")


knots <- function (Fn, ...) 
UseMethod("knots")


fitted <- function (object, ...) 
UseMethod("fitted")


spline <- function (x, y = NULL, n = 3 * length(x), method = "fmm", xmin = min(x), 
    xmax = max(x), xout, ties = mean) 
{
    method <- pmatch(method, c("periodic", "natural", "fmm", 
        "hyman"))
    if (is.na(method)) 
        stop("invalid interpolation method")
    x <- regularize.values(x, y, ties)
    y <- x$y
    x <- x$x
    nx <- as.integer(length(x))
    if (is.na(nx)) 
        stop(gettextf("invalid value of %s", "length(x)"), domain = NA)
    if (nx == 0) 
        stop("zero non-NA points")
    if (method == 1L && y[1L] != y[nx]) {
        warning("spline: first and last y values differ - using y[1] for both")
        y[nx] <- y[1L]
    }
    if (method == 4L) {
        dy <- diff(y)
        if (!(all(dy >= 0) || all(dy <= 0))) 
            stop("'y' must be increasing or decreasing")
    }
    if (missing(xout)) 
        xout <- seq.int(xmin, xmax, length.out = n)
    else n <- length(xout)
    if (n <= 0L) 
        stop("'spline' requires n >= 1")
    xout <- as.double(xout)
    z <- .Call(C_SplineCoef, min(3L, method), x, y)
    if (method == 4L) 
        z <- spl_coef_conv(hyman_filter(z))
    list(x = xout, y = .Call(C_SplineEval, xout, z))
}


symnum <- function (x, cutpoints = c(0.3, 0.6, 0.8, 0.9, 0.95), symbols = if (numeric.x) c(" ", 
    ".", ",", "+", "*", "B") else c(".", "|"), legend = length(symbols) >= 
    3, na = "?", eps = 1e-05, numeric.x = is.numeric(x), corr = missing(cutpoints) && 
    numeric.x, show.max = if (corr) "1", show.min = NULL, abbr.colnames = has.colnames, 
    lower.triangular = corr && is.numeric(x) && is.matrix(x), 
    diag.lower.tri = corr && !is.null(show.max)) 
{
    if (length(x) == 0L) 
        return(noquote(if (is.null(d <- dim(x))) character() else array("", 
            dim = d)))
    has.na <- any(nax <- is.na(x))
    if (numeric.x) {
        force(corr)
        cutpoints <- sort(cutpoints)
        if (corr) 
            cutpoints <- c(0, cutpoints, 1)
        if (anyDuplicated(cutpoints) || (corr && (any(cutpoints > 
            1) || any(cutpoints < 0)))) 
            stop(if (corr) 
                gettext("'cutpoints' must be unique in 0 < cuts < 1, but are = ")
            else gettext("'cutpoints' must be unique, but are = "), 
                paste(format(cutpoints), collapse = "|"), domain = NA)
        nc <- length(cutpoints)
        minc <- cutpoints[1L]
        maxc <- cutpoints[nc]
        range.msg <- if (corr) 
            gettext("'x' must be between -1 and 1")
        else gettextf("'x' must be between %s and %s", format(minc), 
            format(maxc))
        if (corr) 
            x <- abs(x)
        else if (any(x < minc - eps, na.rm = TRUE)) 
            stop(range.msg, domain = NA)
        if (any(x > maxc + eps, na.rm = TRUE)) 
            stop(range.msg, domain = NA)
        ns <- length(symbols)
        symbols <- as.character(symbols)
        if (anyDuplicated(symbols)) 
            stop("'symbols' must be unique, but are = ", paste(symbols, 
                collapse = "|"), domain = NA)
        if (nc != ns + 1) 
            if (corr) 
                stop("number of 'cutpoints' must be one less than number of symbols")
            else stop("number of 'cutpoints' must be one more than number of symbols")
        iS <- cut(x, breaks = cutpoints, include.lowest = TRUE, 
            labels = FALSE)
        if (any(ii <- is.na(iS))) {
            iS[which(ii)[!is.na(x[ii]) & (abs(x[ii] - minc) < 
                eps)]] <- 1
        }
    }
    else {
        if (!missing(symbols) && length(symbols) != 2L) 
            stop("must have 2 'symbols' for logical 'x' argument")
        iS <- x + 1
    }
    if (has.na) {
        ans <- character(length(iS))
        if ((has.na <- is.character(na))) 
            ans[nax] <- na
        ans[!nax] <- symbols[iS[!nax]]
    }
    else ans <- symbols[iS]
    if (numeric.x) {
        if (!is.null(show.max)) 
            ans[x >= maxc - eps] <- if (is.character(show.max)) 
                show.max
            else format(maxc, dig = 1)
        if (!is.null(show.min)) 
            ans[x <= minc + eps] <- if (is.character(show.min)) 
                show.min
            else format(minc, dig = 1)
    }
    if (lower.triangular && is.matrix(x)) 
        ans[!lower.tri(x, diag = diag.lower.tri)] <- ""
    attributes(ans) <- attributes(x)
    if (is.array(ans) && (rank <- length(dim(x))) >= 2L) {
        has.colnames <- !is.null(dimnames(ans))
        if (!has.colnames) {
            dimnames(ans) <- vector("list", rank)
        }
        else {
            has.colnames <- length(dimnames(ans)[[2L]]) > 0L
        }
        if ((is.logical(abbr.colnames) || is.numeric(abbr.colnames)) && 
            abbr.colnames) {
            dimnames(ans)[[2L]] <- abbreviate(dimnames(ans)[[2L]], 
                minlength = abbr.colnames)
        }
        else if (is.null(abbr.colnames) || is.null(dimnames(ans)[[2L]])) 
            dimnames(ans)[[2L]] <- rep("", dim(ans)[2L])
        else if (!is.logical(abbr.colnames)) 
            stop("invalid 'abbr.colnames'")
    }
    if (legend) {
        legend <- c(rbind(sapply(cutpoints, format), c(sQuote(symbols), 
            "")), if (has.na) paste("\t    ## NA:", sQuote(na)))
        attr(ans, "legend") <- paste(legend[-2 * (ns + 1)], collapse = " ")
    }
    noquote(ans)
}


prcomp <- function (x, ...) 
UseMethod("prcomp")


friedman.test <- function (y, ...) 
UseMethod("friedman.test")


KalmanForecast <- function (n.ahead = 10L, mod, update = FALSE) 
.Call(C_KalmanFore, as.integer(n.ahead), mod, update)


aggregate.data.frame <- function (x, by, FUN, ..., simplify = TRUE, drop = TRUE) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    FUN <- match.fun(FUN)
    if (NROW(x) == 0L) 
        stop("no rows to aggregate")
    if (NCOL(x) == 0L) {
        x <- data.frame(x = rep(1, NROW(x)))
        return(aggregate.data.frame(x, by, function(x) 0L)[seq_along(by)])
    }
    if (!is.list(by)) 
        stop("'by' must be a list")
    if (is.null(names(by)) && length(by)) 
        names(by) <- paste0("Group.", seq_along(by))
    else {
        nam <- names(by)
        ind <- which(!nzchar(nam))
        names(by)[ind] <- paste0("Group.", ind)
    }
    nrx <- NROW(x)
    if (any(lengths(by) != nrx)) 
        stop("arguments must have same length")
    y <- as.data.frame(by, stringsAsFactors = FALSE)
    keep <- complete.cases(by)
    y <- y[keep, , drop = FALSE]
    x <- x[keep, , drop = FALSE]
    nrx <- NROW(x)
    ident <- function(x) {
        y <- as.factor(x)
        l <- length(levels(y))
        s <- as.character(seq_len(l))
        n <- nchar(s)
        levels(y) <- paste0(strrep("0", n[l] - n), s)
        as.character(y)
    }
    grp <- if (ncol(y)) {
        grp <- lapply(rev(y), ident)
        names(grp) <- NULL
        do.call(paste, c(grp, list(sep = ".")))
    }
    else integer(nrx)
    if (!drop && ncol(y)) {
        y <- expand.grid(lapply(y, function(e) sort(unique(e))))
        lev <- lapply(rev(y), ident)
        names(lev) <- NULL
        lev <- do.call(paste, c(lev, list(sep = ".")))
        grp <- factor(grp, levels = lev)
    }
    else y <- y[match(sort(unique(grp)), grp, 0L), , drop = FALSE]
    nry <- NROW(y)
    z <- lapply(x, function(e) {
        ans <- lapply(X = split(e, grp), FUN = FUN, ...)
        if (simplify && length(len <- unique(lengths(ans))) == 
            1L) {
            if (len == 1L) {
                cl <- lapply(ans, oldClass)
                cl1 <- cl[[1L]]
                ans <- unlist(ans, recursive = FALSE)
                if (!is.null(cl1) && all(sapply(cl, function(x) identical(x, 
                  cl1)))) 
                  class(ans) <- cl1
            }
            else if (len > 1L) 
                ans <- matrix(unlist(ans, recursive = FALSE), 
                  nrow = nry, ncol = len, byrow = TRUE, dimnames = {
                    if (!is.null(nms <- names(ans[[1L]]))) 
                      list(NULL, nms)
                    else NULL
                  })
        }
        ans
    })
    len <- length(y)
    for (i in seq_along(z)) y[[len + i]] <- z[[i]]
    names(y) <- c(names(by), names(x))
    row.names(y) <- NULL
    y
}


plot.spec.coherency <- function (x, ci = 0.95, xlab = "frequency", ylab = "squared coherency", 
    ylim = c(0, 1), type = "l", main = NULL, ci.col = "blue", 
    ci.lty = 3, ...) 
{
    nser <- NCOL(x$spec)
    gg <- 2/x$df
    se <- sqrt(gg/2)
    z <- -qnorm((1 - ci)/2)
    if (is.null(main)) 
        main <- paste(paste("Series:", x$series), "Squared Coherency", 
            sep = " --  ")
    if (nser == 2) {
        plot(x$freq, x$coh, type = type, xlab = xlab, ylab = ylab, 
            ylim = ylim, ...)
        coh <- pmin(0.99999, sqrt(x$coh))
        lines(x$freq, (tanh(atanh(coh) + z * se))^2, lty = ci.lty, 
            col = ci.col)
        lines(x$freq, (pmax(0, tanh(atanh(coh) - z * se)))^2, 
            lty = ci.lty, col = ci.col)
        title(main)
    }
    else {
        dev.hold()
        on.exit(dev.flush())
        opar <- par(mfrow = c(nser - 1, nser - 1), mar = c(1.5, 
            1.5, 0.5, 0.5), oma = c(4, 4, 6, 4))
        on.exit(par(opar), add = TRUE)
        plot.new()
        for (j in 2:nser) for (i in 1L:(j - 1)) {
            par(mfg = c(j - 1, i, nser - 1, nser - 1))
            ind <- i + (j - 1) * (j - 2)/2
            plot(x$freq, x$coh[, ind], type = type, ylim = ylim, 
                axes = FALSE, xlab = "", ylab = "", ...)
            coh <- pmin(0.99999, sqrt(x$coh[, ind]))
            lines(x$freq, (tanh(atanh(coh) + z * se))^2, lty = ci.lty, 
                col = ci.col)
            lines(x$freq, (pmax(0, tanh(atanh(coh) - z * se)))^2, 
                lty = ci.lty, col = ci.col)
            box()
            if (i == 1) {
                axis(2, xpd = NA)
                title(ylab = x$snames[j], xpd = NA)
            }
            if (j == nser) {
                axis(1, xpd = NA)
                title(xlab = x$snames[i], xpd = NA)
            }
            mtext(main, 3, 3, TRUE, 0.5, cex = par("cex.main"), 
                font = par("font.main"))
        }
    }
    invisible()
}


frequency <- function (x, ...) 
UseMethod("frequency")


decompose <- function (x, type = c("additive", "multiplicative"), filter = NULL) 
{
    type <- match.arg(type)
    l <- length(x)
    f <- frequency(x)
    if (f <= 1 || length(na.omit(x)) < 2 * f) 
        stop("time series has no or less than 2 periods")
    if (is.null(filter)) 
        filter <- if (!f%%2) 
            c(0.5, rep_len(1, f - 1), 0.5)/f
        else rep_len(1, f)/f
    trend <- filter(x, filter)
    season <- if (type == "additive") 
        x - trend
    else x/trend
    periods <- l%/%f
    index <- seq.int(1L, l, by = f) - 1L
    figure <- numeric(f)
    for (i in 1L:f) figure[i] <- mean(season[index + i], na.rm = TRUE)
    figure <- if (type == "additive") 
        figure - mean(figure)
    else figure/mean(figure)
    seasonal <- ts(rep(figure, periods + 1)[seq_len(l)], start = start(x), 
        frequency = f)
    structure(list(x = x, seasonal = seasonal, trend = trend, 
        random = if (type == "additive") x - seasonal - trend else x/seasonal/trend, 
        figure = figure, type = type), class = "decomposed.ts")
}


dsignrank <- function (x, n, log = FALSE) 
{
    on.exit(.External(C_signrank_free))
    .Call(C_dsignrank, x, n, log)
}


pcauchy <- function (q, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_pcauchy, q, location, scale, lower.tail, log.p)


dist <- function (x, method = "euclidean", diag = FALSE, upper = FALSE, 
    p = 2) 
{
    if (!is.na(pmatch(method, "euclidian"))) 
        method <- "euclidean"
    METHODS <- c("euclidean", "maximum", "manhattan", "canberra", 
        "binary", "minkowski")
    method <- pmatch(method, METHODS)
    if (is.na(method)) 
        stop("invalid distance method")
    if (method == -1) 
        stop("ambiguous distance method")
    x <- as.matrix(x)
    N <- nrow(x)
    attrs <- if (method == 6L) 
        list(Size = N, Labels = dimnames(x)[[1L]], Diag = diag, 
            Upper = upper, method = METHODS[method], p = p, call = match.call(), 
            class = "dist")
    else list(Size = N, Labels = dimnames(x)[[1L]], Diag = diag, 
        Upper = upper, method = METHODS[method], call = match.call(), 
        class = "dist")
    .Call(C_Cdist, x, method, attrs, p)
}


SSmicmen <- function (input, Vm, K) 
{
    .expr1 <- Vm * input
    .expr2 <- K + input
    .value <- .expr1/.expr2
    .actualArgs <- as.list(match.call()[c("Vm", "K")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .grad <- array(0, c(length(.value), 2L), list(NULL, c("Vm", 
            "K")))
        .grad[, "Vm"] <- input/.expr2
        .grad[, "K"] <- -(.expr1/.expr2^2)
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


predict <- function (object, ...) 
UseMethod("predict")


lag.plot <- function (x, lags = 1, layout = NULL, set.lags = 1L:lags, main = NULL, 
    asp = 1, diag = TRUE, diag.col = "gray", type = "p", oma = NULL, 
    ask = NULL, do.lines = (n <= 150), labels = do.lines, ...) 
{
    lAxis <- function(side, ..., mgp, xpd, panel, Mgp) if (missing(Mgp)) 
        axis(side, ..., xpd = NA)
    else axis(side, ..., xpd = NA, mgp = Mgp)
    xnam <- deparse(substitute(x))
    is.mat <- !is.null(ncol(x))
    nser <- ncol(x <- as.ts(as.matrix(x)))
    n <- nrow(x)
    if (missing(lags) && !missing(set.lags)) 
        lags <- length(set.lags <- as.integer(set.lags))
    tot.lags <- nser * lags
    if (is.null(ask)) {
        if (.Device == "null device") 
            dev.new()
        ask <- if (is.null(layout)) 
            par("ask")
        else (dev.interactive() && prod(layout) < tot.lags)
    }
    if (is.null(layout)) 
        layout <- if (prod(pmf <- par("mfrow")) >= tot.lags) 
            pmf
        else n2mfrow(tot.lags)
    mlayout <- any(layout > 1)
    if (mlayout) {
        dots <- list(...)
        cex.main <- dots$cex.main
        if (is.null(cex.main)) 
            cex.main <- par("cex.main")
        if (is.null(oma)) {
            oma <- rep(2, 4)
            if (!is.null(main)) 
                oma[3L] <- oma[3L] + 3 * cex.main
        }
        opar <- par(mfrow = layout, mar = c(1.1, 1.1, 0.5, 0.5) + 
            is.mat * c(0, 0.5, 0, 0.5), oma = oma, ask = ask)
        on.exit(par(opar))
    }
    nR <- layout[1L]
    nC <- layout[2L]
    ii <- jj <- 0
    for (i in 1L:nser) {
        X <- x[, i]
        xl <- range(X)
        nam <- if (is.mat) 
            dimnames(x)[[2L]][i]
        else xnam
        newX <- is.mat
        for (ll in set.lags) {
            jj <- 1 + jj%%nC
            if (jj == 1) 
                ii <- 1 + ii%%nR
            if (mlayout) {
                plot(lag(X, ll), X, xlim = xl, ylim = xl, asp = asp, 
                  xlab = paste("lag", ll), ylab = nam, mgp = if (mlayout) 
                    c(0, 0, 0), axes = FALSE, type = type, xy.lines = do.lines, 
                  xy.labels = labels, col.lab = if (newX) 
                    "red", font.lab = if (newX) 
                    2, ...)
                box(...)
                if (jj == 1 && ii%%2 == 1 && !newX) 
                  lAxis(2, ...)
                if (ii == 1 && jj%%2 == 1) 
                  lAxis(3, ...)
                do.4 <- (ii%%2 == 0 && (jj == nC || (i == nser && 
                  ll == set.lags[lags])))
                if (do.4) 
                  lAxis(4, ...)
                if (jj%%2 == 0 && ii == nR) 
                  lAxis(1, ...)
                if (newX) {
                  newX <- FALSE
                  if (!do.4) 
                    lAxis(4, Mgp = c(0, 0.6, 0), ...)
                }
            }
            else {
                plot(lag(X, ll), X, xlim = xl, ylim = xl, asp = asp, 
                  xlab = paste("lag", ll), ylab = nam, type = type, 
                  xy.lines = do.lines, xy.labels = labels, main = main, 
                  ...)
            }
            if (diag) 
                abline(c(0, 1), lty = 2, col = diag.col)
            if (mlayout && !is.null(main)) {
                font.main <- dots$font.main
                if (is.null(font.main)) 
                  font.main <- par("font.main")
                if ((jj == nC && ii == nR) || ll == set.lags[lags]) 
                  mtext(main, 3, 3, outer = TRUE, at = 0.5, cex = cex.main, 
                    font = font.main)
            }
        }
    }
    invisible(NULL)
}


plnorm <- function (q, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_plnorm, q, meanlog, sdlog, lower.tail, log.p)


write.ftable <- function (x, file = "", quote = TRUE, append = FALSE, digits = getOption("digits"), 
    ...) 
{
    r <- format.ftable(x, quote = quote, digits = digits, ...)
    cat(t(r), file = file, append = append, sep = c(rep(" ", 
        ncol(r) - 1), "\n"))
    invisible(x)
}


factanal <- function (x, factors, data = NULL, covmat = NULL, n.obs = NA, 
    subset, na.action, start = NULL, scores = c("none", "regression", 
        "Bartlett"), rotation = "varimax", control = NULL, ...) 
{
    sortLoadings <- function(Lambda) {
        cn <- colnames(Lambda)
        Phi <- attr(Lambda, "covariance")
        ssq <- apply(Lambda, 2L, function(x) -sum(x^2))
        Lambda <- Lambda[, order(ssq), drop = FALSE]
        colnames(Lambda) <- cn
        neg <- colSums(Lambda) < 0
        Lambda[, neg] <- -Lambda[, neg]
        if (!is.null(Phi)) {
            unit <- ifelse(neg, -1, 1)
            attr(Lambda, "covariance") <- unit %*% Phi[order(ssq), 
                order(ssq)] %*% unit
        }
        Lambda
    }
    cl <- match.call()
    na.act <- NULL
    if (is.list(covmat)) {
        if (any(is.na(match(c("cov", "n.obs"), names(covmat))))) 
            stop("'covmat' is not a valid covariance list")
        cv <- covmat$cov
        n.obs <- covmat$n.obs
        have.x <- FALSE
    }
    else if (is.matrix(covmat)) {
        cv <- covmat
        have.x <- FALSE
    }
    else if (is.null(covmat)) {
        if (missing(x)) 
            stop("neither 'x' nor 'covmat' supplied")
        have.x <- TRUE
        if (inherits(x, "formula")) {
            mt <- terms(x, data = data)
            if (attr(mt, "response") > 0) 
                stop("response not allowed in formula")
            attr(mt, "intercept") <- 0
            mf <- match.call(expand.dots = FALSE)
            names(mf)[names(mf) == "x"] <- "formula"
            mf$factors <- mf$covmat <- mf$scores <- mf$start <- mf$rotation <- mf$control <- mf$... <- NULL
            mf[[1L]] <- quote(stats::model.frame)
            mf <- eval.parent(mf)
            na.act <- attr(mf, "na.action")
            if (.check_vars_numeric(mf)) 
                stop("factor analysis applies only to numerical variables")
            z <- model.matrix(mt, mf)
        }
        else {
            z <- as.matrix(x)
            if (!is.numeric(z)) 
                stop("factor analysis applies only to numerical variables")
            if (!missing(subset)) 
                z <- z[subset, , drop = FALSE]
        }
        covmat <- cov.wt(z)
        cv <- covmat$cov
        n.obs <- covmat$n.obs
    }
    else stop("'covmat' is of unknown type")
    scores <- match.arg(scores)
    if (scores != "none" && !have.x) 
        stop("requested scores without an 'x' matrix")
    p <- ncol(cv)
    if (p < 3) 
        stop("factor analysis requires at least three variables")
    dof <- 0.5 * ((p - factors)^2 - p - factors)
    if (dof < 0) 
        stop(sprintf(ngettext(factors, "%d factor is too many for %d variables", 
            "%d factors are too many for %d variables"), factors, 
            p), domain = NA)
    sds <- sqrt(diag(cv))
    cv <- cv/(sds %o% sds)
    cn <- list(nstart = 1, trace = FALSE, lower = 0.005)
    cn[names(control)] <- control
    more <- list(...)[c("nstart", "trace", "lower", "opt", "rotate")]
    if (length(more)) 
        cn[names(more)] <- more
    if (is.null(start)) {
        start <- (1 - 0.5 * factors/p)/diag(solve(cv))
        if ((ns <- cn$nstart) > 1) 
            start <- cbind(start, matrix(runif(ns - 1), p, ns - 
                1, byrow = TRUE))
    }
    start <- as.matrix(start)
    if (nrow(start) != p) 
        stop(sprintf(ngettext(p, "'start' must have %d row", 
            "'start' must have %d rows"), p), domain = NA)
    nc <- ncol(start)
    if (nc < 1) 
        stop("no starting values supplied")
    best <- Inf
    for (i in 1L:nc) {
        nfit <- factanal.fit.mle(cv, factors, start[, i], max(cn$lower, 
            0), cn$opt)
        if (cn$trace) 
            cat("start", i, "value:", format(nfit$criteria[1L]), 
                "uniqs:", format(as.vector(round(nfit$uniquenesses, 
                  4))), "\n")
        if (nfit$converged && nfit$criteria[1L] < best) {
            fit <- nfit
            best <- fit$criteria[1L]
        }
    }
    if (best == Inf) 
        stop(ngettext(nc, "unable to optimize from this starting value", 
            "unable to optimize from these starting values"), 
            domain = NA)
    load <- fit$loadings
    if (rotation != "none") {
        rot <- do.call(rotation, c(list(load), cn$rotate))
        load <- if (is.list(rot)) {
            load <- rot$loadings
            fit$rotmat <- if (inherits(rot, "GPArotation")) 
                t(solve(rot$Th))
            else rot$rotmat
            rot$loadings
        }
        else rot
    }
    fit$loadings <- sortLoadings(load)
    class(fit$loadings) <- "loadings"
    fit$na.action <- na.act
    if (have.x && scores != "none") {
        Lambda <- fit$loadings
        zz <- scale(z, TRUE, TRUE)
        switch(scores, regression = {
            sc <- zz %*% solve(cv, Lambda)
            if (!is.null(Phi <- attr(Lambda, "covariance"))) sc <- sc %*% 
                Phi
        }, Bartlett = {
            d <- 1/fit$uniquenesses
            tmp <- t(Lambda * d)
            sc <- t(solve(tmp %*% Lambda, tmp %*% t(zz)))
        })
        rownames(sc) <- rownames(z)
        colnames(sc) <- colnames(Lambda)
        if (!is.null(na.act)) 
            sc <- napredict(na.act, sc)
        fit$scores <- sc
    }
    if (!is.na(n.obs) && dof > 0) {
        fit$STATISTIC <- (n.obs - 1 - (2 * p + 5)/6 - (2 * factors)/3) * 
            fit$criteria["objective"]
        fit$PVAL <- pchisq(fit$STATISTIC, dof, lower.tail = FALSE)
    }
    fit$n.obs <- n.obs
    fit$call <- cl
    fit
}


resid <- function (object, ...) 
UseMethod("residuals")


rlnorm <- function (n, meanlog = 0, sdlog = 1) 
.Call(C_rlnorm, n, meanlog, sdlog)


rstandard <- function (model, ...) 
UseMethod("rstandard")


plot.ts <- function (x, y = NULL, plot.type = c("multiple", "single"), xy.labels, 
    xy.lines, panel = lines, nc, yax.flip = FALSE, mar.multi = c(0, 
        5.1, 0, if (yax.flip) 5.1 else 2.1), oma.multi = c(6, 
        0, 5, 0), axes = TRUE, ...) 
{
    plotts <- function(x, y = NULL, plot.type = c("multiple", 
        "single"), xy.labels, xy.lines, panel = lines, nc, xlabel, 
        ylabel, type = "l", xlim = NULL, ylim = NULL, xlab = "Time", 
        ylab, log = "", col = par("col"), bg = NA, pch = par("pch"), 
        cex = par("cex"), lty = par("lty"), lwd = par("lwd"), 
        axes = TRUE, frame.plot = axes, ann = par("ann"), cex.lab = par("cex.lab"), 
        col.lab = par("col.lab"), font.lab = par("font.lab"), 
        cex.axis = par("cex.axis"), col.axis = par("col.axis"), 
        font.axis = par("font.axis"), main = NULL, ...) {
        plot.type <- match.arg(plot.type)
        nser <- NCOL(x)
        if (plot.type == "multiple" && nser > 1) {
            addmain <- function(main, cex.main = par("cex.main"), 
                font.main = par("font.main"), col.main = par("col.main"), 
                ...) mtext(main, side = 3, line = 3, cex = cex.main, 
                font = font.main, col = col.main, ...)
            panel <- match.fun(panel)
            nser <- NCOL(x)
            if (nser > 10) 
                stop("cannot plot more than 10 series as \"multiple\"")
            if (is.null(main)) 
                main <- xlabel
            nm <- colnames(x)
            if (is.null(nm)) 
                nm <- paste("Series", 1L:nser)
            if (missing(nc)) 
                nc <- if (nser > 4) 
                  2
                else 1
            nr <- ceiling(nser/nc)
            oldpar <- par(mar = mar.multi, oma = oma.multi, mfcol = c(nr, 
                nc))
            on.exit(par(oldpar))
            for (i in 1L:nser) {
                plot.default(x[, i], axes = FALSE, xlab = "", 
                  ylab = "", log = log, col = col, bg = bg, pch = pch, 
                  ann = ann, type = "n", ...)
                panel(x[, i], col = col, bg = bg, pch = pch, 
                  cex = cex, lwd = lwd, lty = lty, type = type, 
                  ...)
                if (frame.plot) 
                  box(...)
                y.side <- if (i%%2 || !yax.flip) 
                  2
                else 4
                do.xax <- i%%nr == 0 || i == nser
                if (axes) {
                  axis(y.side, xpd = NA, cex.axis = cex.axis, 
                    col.axis = col.axis, font.axis = font.axis, 
                    ...)
                  if (do.xax) 
                    axis(1, xpd = NA, cex.axis = cex.axis, col.axis = col.axis, 
                      font.axis = font.axis, ...)
                }
                if (ann) {
                  mtext(nm[i], y.side, line = 3, cex = cex.lab, 
                    col = col.lab, font = font.lab, ...)
                  if (do.xax) 
                    mtext(xlab, side = 1, line = 3, cex = cex.lab, 
                      col = col.lab, font = font.lab, ...)
                }
            }
            if (ann && !is.null(main)) {
                par(mfcol = c(1, 1))
                addmain(main, ...)
            }
            return(invisible())
        }
        x <- as.ts(x)
        if (!is.null(y)) {
            y <- hasTsp(y)
            if (NCOL(x) > 1 || NCOL(y) > 1) 
                stop("scatter plots only for univariate time series")
            if (is.ts(x) && is.ts(y)) {
                xy <- ts.intersect(x, y)
                xy <- xy.coords(xy[, 1], xy[, 2], xlabel, ylabel, 
                  log)
            }
            else xy <- xy.coords(x, y, xlabel, ylabel, log)
            xlab <- if (missing(xlab)) 
                xy$xlab
            else xlab
            ylab <- if (missing(ylab)) 
                xy$ylab
            else ylab
            xlim <- if (is.null(xlim)) 
                range(xy$x[is.finite(xy$x)])
            else xlim
            ylim <- if (is.null(ylim)) 
                range(xy$y[is.finite(xy$y)])
            else ylim
            n <- length(xy$x)
            if (missing(xy.labels)) 
                xy.labels <- (n <= 150)
            do.lab <- if (is.logical(xy.labels)) 
                xy.labels
            else {
                if (!is.character(xy.labels)) 
                  stop("'xy.labels' must be logical or character")
                TRUE
            }
            ptype <- if (do.lab) 
                "n"
            else if (missing(type)) 
                "p"
            else type
            dev.hold()
            on.exit(dev.flush())
            plot.default(xy, type = ptype, xlab = xlab, ylab = ylab, 
                xlim = xlim, ylim = ylim, log = log, col = col, 
                bg = bg, pch = pch, cex = cex, lty = lty, lwd = lwd, 
                axes = axes, frame.plot = frame.plot, ann = ann, 
                main = main, ...)
            if (missing(xy.lines)) 
                xy.lines <- do.lab
            if (do.lab) 
                text(xy, labels = if (is.character(xy.labels)) 
                  xy.labels
                else if (all(tsp(x) == tsp(y))) 
                  formatC(unclass(time(x)), width = 1)
                else seq_along(xy$x), col = col, cex = cex)
            if (xy.lines) 
                lines(xy, col = col, lty = lty, lwd = lwd, type = if (do.lab) 
                  "c"
                else "l")
            return(invisible())
        }
        if (missing(ylab)) {
            ylab <- colnames(x)
            if (length(ylab) != 1L) 
                ylab <- xlabel
        }
        if (is.matrix(x)) {
            k <- ncol(x)
            tx <- time(x)
            xy <- xy.coords(x = matrix(rep.int(tx, k), ncol = k), 
                y = x, log = log, setLab = FALSE)
            xy$x <- tx
        }
        else xy <- xy.coords(x, NULL, log = log, setLab = FALSE)
        if (is.null(xlim)) 
            xlim <- range(xy$x)
        if (is.null(ylim)) 
            ylim <- range(xy$y[is.finite(xy$y)])
        plot.new()
        plot.window(xlim, ylim, log, ...)
        if (is.matrix(x)) {
            for (i in seq_len(k)) lines.default(xy$x, x[, i], 
                col = col[(i - 1L)%%length(col) + 1L], lty = lty[(i - 
                  1L)%%length(lty) + 1L], lwd = lwd[(i - 1L)%%length(lwd) + 
                  1L], bg = bg[(i - 1L)%%length(bg) + 1L], pch = pch[(i - 
                  1L)%%length(pch) + 1L], cex = cex[(i - 1L)%%length(cex) + 
                  1L], type = type)
        }
        else {
            lines.default(xy$x, x, col = col[1L], bg = bg, lty = lty[1L], 
                lwd = lwd[1L], pch = pch[1L], cex = cex[1L], 
                type = type)
        }
        if (ann) 
            title(main = main, xlab = xlab, ylab = ylab, ...)
        if (axes) {
            axis(1, ...)
            axis(2, ...)
        }
        if (frame.plot) 
            box(...)
    }
    xlabel <- if (!missing(x)) 
        deparse(substitute(x))
    ylabel <- if (!missing(y)) 
        deparse(substitute(y))
    plotts(x = x, y = y, plot.type = plot.type, xy.labels = xy.labels, 
        xy.lines = xy.lines, panel = panel, nc = nc, xlabel = xlabel, 
        ylabel = ylabel, axes = axes, ...)
}


optim <- function (par, fn, gr = NULL, ..., method = c("Nelder-Mead", 
    "BFGS", "CG", "L-BFGS-B", "SANN", "Brent"), lower = -Inf, 
    upper = Inf, control = list(), hessian = FALSE) 
{
    fn1 <- function(par) fn(par, ...)
    gr1 <- if (!is.null(gr)) 
        function(par) gr(par, ...)
    method <- match.arg(method)
    if ((length(lower) > 1L || length(upper) > 1L || lower[1L] != 
        -Inf || upper[1L] != Inf) && !any(method == c("L-BFGS-B", 
        "Brent"))) {
        warning("bounds can only be used with method L-BFGS-B (or Brent)")
        method <- "L-BFGS-B"
    }
    npar <- length(par)
    con <- list(trace = 0, fnscale = 1, parscale = rep.int(1, 
        npar), ndeps = rep.int(0.001, npar), maxit = 100L, abstol = -Inf, 
        reltol = sqrt(.Machine$double.eps), alpha = 1, beta = 0.5, 
        gamma = 2, REPORT = 10, type = 1, lmm = 5, factr = 1e+07, 
        pgtol = 0, tmax = 10, temp = 10)
    nmsC <- names(con)
    if (method == "Nelder-Mead") 
        con$maxit <- 500
    if (method == "SANN") {
        con$maxit <- 10000
        con$REPORT <- 100
    }
    con[(namc <- names(control))] <- control
    if (length(noNms <- namc[!namc %in% nmsC])) 
        warning("unknown names in control: ", paste(noNms, collapse = ", "))
    if (con$trace < 0) 
        warning("read the documentation for 'trace' more carefully")
    else if (method == "SANN" && con$trace && as.integer(con$REPORT) == 
        0) 
        stop("'trace != 0' needs 'REPORT >= 1'")
    if (method == "L-BFGS-B" && any(!is.na(match(c("reltol", 
        "abstol"), namc)))) 
        warning("method L-BFGS-B uses 'factr' (and 'pgtol') instead of 'reltol' and 'abstol'")
    if (npar == 1 && method == "Nelder-Mead") 
        warning("one-dimensional optimization by Nelder-Mead is unreliable:\nuse \"Brent\" or optimize() directly")
    if (npar > 1 && method == "Brent") 
        stop("method = \"Brent\" is only available for one-dimensional optimization")
    lower <- as.double(rep_len(lower, npar))
    upper <- as.double(rep_len(upper, npar))
    res <- if (method == "Brent") {
        if (any(!is.finite(c(upper, lower)))) 
            stop("'lower' and 'upper' must be finite values")
        res <- optimize(function(par) fn(par, ...)/con$fnscale, 
            lower = lower, upper = upper, tol = con$reltol)
        names(res)[names(res) == c("minimum", "objective")] <- c("par", 
            "value")
        res$value <- res$value * con$fnscale
        c(res, list(counts = c(`function` = NA, gradient = NA), 
            convergence = 0L, message = NULL))
    }
    else .External2(C_optim, par, fn1, gr1, method, con, lower, 
        upper)
    if (hessian) 
        res$hessian <- .External2(C_optimhess, res$par, fn1, 
            gr1, con)
    res
}


glm.fit <- function (x, y, weights = rep(1, nobs), start = NULL, etastart = NULL, 
    mustart = NULL, offset = rep(0, nobs), family = gaussian(), 
    control = list(), intercept = TRUE) 
{
    control <- do.call("glm.control", control)
    x <- as.matrix(x)
    xnames <- dimnames(x)[[2L]]
    ynames <- if (is.matrix(y)) 
        rownames(y)
    else names(y)
    conv <- FALSE
    nobs <- NROW(y)
    nvars <- ncol(x)
    EMPTY <- nvars == 0
    if (is.null(weights)) 
        weights <- rep.int(1, nobs)
    if (is.null(offset)) 
        offset <- rep.int(0, nobs)
    variance <- family$variance
    linkinv <- family$linkinv
    if (!is.function(variance) || !is.function(linkinv)) 
        stop("'family' argument seems not to be a valid family object", 
            call. = FALSE)
    dev.resids <- family$dev.resids
    aic <- family$aic
    mu.eta <- family$mu.eta
    unless.null <- function(x, if.null) if (is.null(x)) 
        if.null
    else x
    valideta <- unless.null(family$valideta, function(eta) TRUE)
    validmu <- unless.null(family$validmu, function(mu) TRUE)
    if (is.null(mustart)) {
        eval(family$initialize)
    }
    else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
    }
    if (EMPTY) {
        eta <- rep.int(0, nobs) + offset
        if (!valideta(eta)) 
            stop("invalid linear predictor values in empty model", 
                call. = FALSE)
        mu <- linkinv(eta)
        if (!validmu(mu)) 
            stop("invalid fitted means in empty model", call. = FALSE)
        dev <- sum(dev.resids(y, mu, weights))
        w <- sqrt((weights * mu.eta(eta)^2)/variance(mu))
        residuals <- (y - mu)/mu.eta(eta)
        good <- rep_len(TRUE, length(residuals))
        boundary <- conv <- TRUE
        coef <- numeric()
        iter <- 0L
    }
    else {
        coefold <- NULL
        eta <- if (!is.null(etastart)) 
            etastart
        else if (!is.null(start)) 
            if (length(start) != nvars) 
                stop(gettextf("length of 'start' should equal %d and correspond to initial coefs for %s", 
                  nvars, paste(deparse(xnames), collapse = ", ")), 
                  domain = NA)
            else {
                coefold <- start
                offset + as.vector(if (NCOL(x) == 1L) 
                  x * start
                else x %*% start)
            }
        else family$linkfun(mustart)
        mu <- linkinv(eta)
        if (!(validmu(mu) && valideta(eta))) 
            stop("cannot find valid starting values: please specify some", 
                call. = FALSE)
        devold <- sum(dev.resids(y, mu, weights))
        boundary <- conv <- FALSE
        for (iter in 1L:control$maxit) {
            good <- weights > 0
            varmu <- variance(mu)[good]
            if (anyNA(varmu)) 
                stop("NAs in V(mu)")
            if (any(varmu == 0)) 
                stop("0s in V(mu)")
            mu.eta.val <- mu.eta(eta)
            if (any(is.na(mu.eta.val[good]))) 
                stop("NAs in d(mu)/d(eta)")
            good <- (weights > 0) & (mu.eta.val != 0)
            if (all(!good)) {
                conv <- FALSE
                warning(gettextf("no observations informative at iteration %d", 
                  iter), domain = NA)
                break
            }
            z <- (eta - offset)[good] + (y - mu)[good]/mu.eta.val[good]
            w <- sqrt((weights[good] * mu.eta.val[good]^2)/variance(mu)[good])
            fit <- .Call(C_Cdqrls, x[good, , drop = FALSE] * 
                w, z * w, min(1e-07, control$epsilon/1000), check = FALSE)
            if (any(!is.finite(fit$coefficients))) {
                conv <- FALSE
                warning(gettextf("non-finite coefficients at iteration %d", 
                  iter), domain = NA)
                break
            }
            if (nobs < fit$rank) 
                stop(sprintf(ngettext(nobs, "X matrix has rank %d, but only %d observation", 
                  "X matrix has rank %d, but only %d observations"), 
                  fit$rank, nobs), domain = NA)
            start[fit$pivot] <- fit$coefficients
            eta <- drop(x %*% start)
            mu <- linkinv(eta <- eta + offset)
            dev <- sum(dev.resids(y, mu, weights))
            if (control$trace) 
                cat("Deviance = ", dev, " Iterations - ", iter, 
                  "\n", sep = "")
            boundary <- FALSE
            if (!is.finite(dev)) {
                if (is.null(coefold)) 
                  stop("no valid set of coefficients has been found: please supply starting values", 
                    call. = FALSE)
                warning("step size truncated due to divergence", 
                  call. = FALSE)
                ii <- 1
                while (!is.finite(dev)) {
                  if (ii > control$maxit) 
                    stop("inner loop 1; cannot correct step size", 
                      call. = FALSE)
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- drop(x %*% start)
                  mu <- linkinv(eta <- eta + offset)
                  dev <- sum(dev.resids(y, mu, weights))
                }
                boundary <- TRUE
                if (control$trace) 
                  cat("Step halved: new deviance = ", dev, "\n", 
                    sep = "")
            }
            if (!(valideta(eta) && validmu(mu))) {
                if (is.null(coefold)) 
                  stop("no valid set of coefficients has been found: please supply starting values", 
                    call. = FALSE)
                warning("step size truncated: out of bounds", 
                  call. = FALSE)
                ii <- 1
                while (!(valideta(eta) && validmu(mu))) {
                  if (ii > control$maxit) 
                    stop("inner loop 2; cannot correct step size", 
                      call. = FALSE)
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- drop(x %*% start)
                  mu <- linkinv(eta <- eta + offset)
                }
                boundary <- TRUE
                dev <- sum(dev.resids(y, mu, weights))
                if (control$trace) 
                  cat("Step halved: new deviance = ", dev, "\n", 
                    sep = "")
            }
            if (abs(dev - devold)/(0.1 + abs(dev)) < control$epsilon) {
                conv <- TRUE
                coef <- start
                break
            }
            else {
                devold <- dev
                coef <- coefold <- start
            }
        }
        if (!conv) 
            warning("glm.fit: algorithm did not converge", call. = FALSE)
        if (boundary) 
            warning("glm.fit: algorithm stopped at boundary value", 
                call. = FALSE)
        eps <- 10 * .Machine$double.eps
        if (family$family == "binomial") {
            if (any(mu > 1 - eps) || any(mu < eps)) 
                warning("glm.fit: fitted probabilities numerically 0 or 1 occurred", 
                  call. = FALSE)
        }
        if (family$family == "poisson") {
            if (any(mu < eps)) 
                warning("glm.fit: fitted rates numerically 0 occurred", 
                  call. = FALSE)
        }
        if (fit$rank < nvars) 
            coef[fit$pivot][seq.int(fit$rank + 1, nvars)] <- NA
        xxnames <- xnames[fit$pivot]
        residuals <- (y - mu)/mu.eta(eta)
        fit$qr <- as.matrix(fit$qr)
        nr <- min(sum(good), nvars)
        if (nr < nvars) {
            Rmat <- diag(nvars)
            Rmat[1L:nr, 1L:nvars] <- fit$qr[1L:nr, 1L:nvars]
        }
        else Rmat <- fit$qr[1L:nvars, 1L:nvars]
        Rmat <- as.matrix(Rmat)
        Rmat[row(Rmat) > col(Rmat)] <- 0
        names(coef) <- xnames
        colnames(fit$qr) <- xxnames
        dimnames(Rmat) <- list(xxnames, xxnames)
    }
    names(residuals) <- ynames
    names(mu) <- ynames
    names(eta) <- ynames
    wt <- rep.int(0, nobs)
    wt[good] <- w^2
    names(wt) <- ynames
    names(weights) <- ynames
    names(y) <- ynames
    if (!EMPTY) 
        names(fit$effects) <- c(xxnames[seq_len(fit$rank)], rep.int("", 
            sum(good) - fit$rank))
    wtdmu <- if (intercept) 
        sum(weights * y)/sum(weights)
    else linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    n.ok <- nobs - sum(weights == 0)
    nulldf <- n.ok - as.integer(intercept)
    rank <- if (EMPTY) 
        0
    else fit$rank
    resdf <- n.ok - rank
    aic.model <- aic(y, n, mu, weights, dev) + 2 * rank
    list(coefficients = coef, residuals = residuals, fitted.values = mu, 
        effects = if (!EMPTY) fit$effects, R = if (!EMPTY) Rmat, 
        rank = rank, qr = if (!EMPTY) structure(fit[c("qr", "rank", 
            "qraux", "pivot", "tol")], class = "qr"), family = family, 
        linear.predictors = eta, deviance = dev, aic = aic.model, 
        null.deviance = nulldev, iter = iter, weights = wt, prior.weights = weights, 
        df.residual = resdf, df.null = nulldf, y = y, converged = conv, 
        boundary = boundary)
}


predict.lm <- function (object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
    interval = c("none", "confidence", "prediction"), level = 0.95, 
    type = c("response", "terms"), terms = NULL, na.action = na.pass, 
    pred.var = res.var/weights, weights = 1, ...) 
{
    tt <- terms(object)
    if (!inherits(object, "lm")) 
        warning("calling predict.lm(<fake-lm-object>) ...")
    if (missing(newdata) || is.null(newdata)) {
        mm <- X <- model.matrix(object)
        mmDone <- TRUE
        offset <- object$offset
    }
    else {
        Terms <- delete.response(tt)
        m <- model.frame(Terms, newdata, na.action = na.action, 
            xlev = object$xlevels)
        if (!is.null(cl <- attr(Terms, "dataClasses"))) 
            .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        offset <- rep(0, nrow(X))
        if (!is.null(off.num <- attr(tt, "offset"))) 
            for (i in off.num) offset <- offset + eval(attr(tt, 
                "variables")[[i + 1]], newdata)
        if (!is.null(object$call$offset)) 
            offset <- offset + eval(object$call$offset, newdata)
        mmDone <- FALSE
    }
    n <- length(object$residuals)
    p <- object$rank
    p1 <- seq_len(p)
    piv <- if (p) 
        qr.lm(object)$pivot[p1]
    if (p < ncol(X) && !(missing(newdata) || is.null(newdata))) 
        warning("prediction from a rank-deficient fit may be misleading")
    beta <- object$coefficients
    predictor <- drop(X[, piv, drop = FALSE] %*% beta[piv])
    if (!is.null(offset)) 
        predictor <- predictor + offset
    interval <- match.arg(interval)
    if (interval == "prediction") {
        if (missing(newdata)) 
            warning("predictions on current data refer to _future_ responses\n")
        if (missing(newdata) && missing(weights)) {
            w <- weights.default(object)
            if (!is.null(w)) {
                weights <- w
                warning("assuming prediction variance inversely proportional to weights used for fitting\n")
            }
        }
        if (!missing(newdata) && missing(weights) && !is.null(object$weights) && 
            missing(pred.var)) 
            warning("Assuming constant prediction variance even though model fit is weighted\n")
        if (inherits(weights, "formula")) {
            if (length(weights) != 2L) 
                stop("'weights' as formula should be one-sided")
            d <- if (missing(newdata) || is.null(newdata)) 
                model.frame(object)
            else newdata
            weights <- eval(weights[[2L]], d, environment(weights))
        }
    }
    type <- match.arg(type)
    if (se.fit || interval != "none") {
        w <- object$weights
        res.var <- if (is.null(scale)) {
            r <- object$residuals
            rss <- sum(if (is.null(w)) r^2 else r^2 * w)
            df <- object$df.residual
            rss/df
        }
        else scale^2
        if (type != "terms") {
            if (p > 0) {
                XRinv <- if (missing(newdata) && is.null(w)) 
                  qr.Q(qr.lm(object))[, p1, drop = FALSE]
                else X[, piv] %*% qr.solve(qr.R(qr.lm(object))[p1, 
                  p1])
                ip <- drop(XRinv^2 %*% rep(res.var, p))
            }
            else ip <- rep(0, n)
        }
    }
    if (type == "terms") {
        if (!mmDone) {
            mm <- model.matrix(object)
            mmDone <- TRUE
        }
        aa <- attr(mm, "assign")
        ll <- attr(tt, "term.labels")
        hasintercept <- attr(tt, "intercept") > 0L
        if (hasintercept) 
            ll <- c("(Intercept)", ll)
        aaa <- factor(aa, labels = ll)
        asgn <- split(order(aa), aaa)
        if (hasintercept) {
            asgn$"(Intercept)" <- NULL
            avx <- colMeans(mm)
            termsconst <- sum(avx[piv] * beta[piv])
        }
        nterms <- length(asgn)
        if (nterms > 0) {
            predictor <- matrix(ncol = nterms, nrow = NROW(X))
            dimnames(predictor) <- list(rownames(X), names(asgn))
            if (se.fit || interval != "none") {
                ip <- matrix(ncol = nterms, nrow = NROW(X))
                dimnames(ip) <- list(rownames(X), names(asgn))
                Rinv <- qr.solve(qr.R(qr.lm(object))[p1, p1])
            }
            if (hasintercept) 
                X <- sweep(X, 2L, avx, check.margin = FALSE)
            unpiv <- rep.int(0L, NCOL(X))
            unpiv[piv] <- p1
            for (i in seq.int(1L, nterms, length.out = nterms)) {
                iipiv <- asgn[[i]]
                ii <- unpiv[iipiv]
                iipiv[ii == 0L] <- 0L
                predictor[, i] <- if (any(iipiv > 0L)) 
                  X[, iipiv, drop = FALSE] %*% beta[iipiv]
                else 0
                if (se.fit || interval != "none") 
                  ip[, i] <- if (any(iipiv > 0L)) 
                    as.matrix(X[, iipiv, drop = FALSE] %*% Rinv[ii, 
                      , drop = FALSE])^2 %*% rep.int(res.var, 
                      p)
                  else 0
            }
            if (!is.null(terms)) {
                predictor <- predictor[, terms, drop = FALSE]
                if (se.fit) 
                  ip <- ip[, terms, drop = FALSE]
            }
        }
        else {
            predictor <- ip <- matrix(0, n, 0L)
        }
        attr(predictor, "constant") <- if (hasintercept) 
            termsconst
        else 0
    }
    if (interval != "none") {
        tfrac <- qt((1 - level)/2, df)
        hwid <- tfrac * switch(interval, confidence = sqrt(ip), 
            prediction = sqrt(ip + pred.var))
        if (type != "terms") {
            predictor <- cbind(predictor, predictor + hwid %o% 
                c(1, -1))
            colnames(predictor) <- c("fit", "lwr", "upr")
        }
        else {
            if (!is.null(terms)) 
                hwid <- hwid[, terms, drop = FALSE]
            lwr <- predictor + hwid
            upr <- predictor - hwid
        }
    }
    if (se.fit || interval != "none") {
        se <- sqrt(ip)
        if (type == "terms" && !is.null(terms) && !se.fit) 
            se <- se[, terms, drop = FALSE]
    }
    if (missing(newdata) && !is.null(na.act <- object$na.action)) {
        predictor <- napredict(na.act, predictor)
        if (se.fit) 
            se <- napredict(na.act, se)
    }
    if (type == "terms" && interval != "none") {
        if (missing(newdata) && !is.null(na.act)) {
            lwr <- napredict(na.act, lwr)
            upr <- napredict(na.act, upr)
        }
        list(fit = predictor, se.fit = se, lwr = lwr, upr = upr, 
            df = df, residual.scale = sqrt(res.var))
    }
    else if (se.fit) 
        list(fit = predictor, se.fit = se, df = df, residual.scale = sqrt(res.var))
    else predictor
}


dmultinom <- function (x, size = NULL, prob, log = FALSE) 
{
    K <- length(prob)
    if (length(x) != K) 
        stop("x[] and prob[] must be equal length vectors.")
    if (any(!is.finite(prob)) || any(prob < 0) || (s <- sum(prob)) == 
        0) 
        stop("probabilities must be finite, non-negative and not all 0")
    prob <- prob/s
    x <- as.integer(x + 0.5)
    if (any(x < 0)) 
        stop("'x' must be non-negative")
    N <- sum(x)
    if (is.null(size)) 
        size <- N
    else if (size != N) 
        stop("size != sum(x), i.e. one is wrong")
    i0 <- prob == 0
    if (any(i0)) {
        if (any(x[i0] != 0)) 
            return(if (log) -Inf else 0)
        if (all(i0)) 
            return(if (log) 0 else 1)
        x <- x[!i0]
        prob <- prob[!i0]
    }
    r <- lgamma(size + 1) + sum(x * log(prob) - lgamma(x + 1))
    if (log) 
        r
    else exp(r)
}


getCall <- function (x, ...) 
UseMethod("getCall")


ansari.test <- function (x, ...) 
UseMethod("ansari.test")


loadings <- function (x, ...) 
x$loadings


NLSstAsymptotic <- function (xy) 
UseMethod("NLSstAsymptotic")


`window<-` <- function (x, ..., value) 
UseMethod("window<-")


residuals.glm <- function (object, type = c("deviance", "pearson", "working", 
    "response", "partial"), ...) 
{
    type <- match.arg(type)
    y <- object$y
    r <- object$residuals
    mu <- object$fitted.values
    wts <- object$prior.weights
    switch(type, deviance = , pearson = , response = if (is.null(y)) {
        mu.eta <- object$family$mu.eta
        eta <- object$linear.predictors
        y <- mu + r * mu.eta(eta)
    })
    res <- switch(type, deviance = if (object$df.residual > 0) {
        d.res <- sqrt(pmax((object$family$dev.resids)(y, mu, 
            wts), 0))
        ifelse(y > mu, d.res, -d.res)
    } else rep.int(0, length(mu)), pearson = (y - mu) * sqrt(wts)/sqrt(object$family$variance(mu)), 
        working = r, response = y - mu, partial = r)
    if (!is.null(object$na.action)) 
        res <- naresid(object$na.action, res)
    if (type == "partial") 
        res <- res + predict(object, type = "terms")
    res
}


window <- function (x, ...) 
UseMethod("window")


pbeta <- function (q, shape1, shape2, ncp = 0, lower.tail = TRUE, log.p = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_pbeta, q, shape1, shape2, lower.tail, log.p)
    else .Call(C_pnbeta, q, shape1, shape2, ncp, lower.tail, 
        log.p)
}


pairwise.t.test <- function (x, g, p.adjust.method = p.adjust.methods, pool.sd = !paired, 
    paired = FALSE, alternative = c("two.sided", "less", "greater"), 
    ...) 
{
    if (paired & pool.sd) 
        stop("pooling of SD is incompatible with paired tests")
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    g <- factor(g)
    p.adjust.method <- match.arg(p.adjust.method)
    alternative <- match.arg(alternative)
    if (pool.sd) {
        METHOD <- "t tests with pooled SD"
        xbar <- tapply(x, g, mean, na.rm = TRUE)
        s <- tapply(x, g, sd, na.rm = TRUE)
        n <- tapply(!is.na(x), g, sum)
        degf <- n - 1
        total.degf <- sum(degf)
        pooled.sd <- sqrt(sum(s^2 * degf)/total.degf)
        compare.levels <- function(i, j) {
            dif <- xbar[i] - xbar[j]
            se.dif <- pooled.sd * sqrt(1/n[i] + 1/n[j])
            t.val <- dif/se.dif
            if (alternative == "two.sided") 
                2 * pt(-abs(t.val), total.degf)
            else pt(t.val, total.degf, lower.tail = (alternative == 
                "less"))
        }
    }
    else {
        METHOD <- if (paired) 
            "paired t tests"
        else "t tests with non-pooled SD"
        compare.levels <- function(i, j) {
            xi <- x[as.integer(g) == i]
            xj <- x[as.integer(g) == j]
            t.test(xi, xj, paired = paired, alternative = alternative, 
                ...)$p.value
        }
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
        p.adjust.method = p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}


mantelhaen.test <- function (x, y = NULL, z = NULL, alternative = c("two.sided", 
    "less", "greater"), correct = TRUE, exact = FALSE, conf.level = 0.95) 
{
    DNAME <- deparse(substitute(x))
    if (is.array(x)) {
        if (length(dim(x)) == 3L) {
            if (anyNA(x)) 
                stop("NAs are not allowed")
            if (any(dim(x) < 2L)) 
                stop("each dimension in table must be >= 2")
        }
        else stop("'x' must be a 3-dimensional array")
    }
    else {
        if (is.null(y)) 
            stop("if 'x' is not an array, 'y' must be given")
        if (is.null(z)) 
            stop("if 'x' is not an array, 'z' must be given")
        if (any(diff(c(length(x), length(y), length(z))) != 0L)) 
            stop("'x', 'y', and 'z' must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)), 
            "and", deparse(substitute(z)))
        OK <- complete.cases(x, y, z)
        x <- factor(x[OK])
        y <- factor(y[OK])
        if ((nlevels(x) < 2L) || (nlevels(y) < 2L)) 
            stop("'x' and 'y' must have at least 2 levels")
        else x <- table(x, y, z[OK])
    }
    if (any(apply(x, 3L, sum) < 2)) 
        stop("sample size in each stratum must be > 1")
    I <- dim(x)[1L]
    J <- dim(x)[2L]
    K <- dim(x)[3L]
    if ((I == 2) && (J == 2)) {
        alternative <- match.arg(alternative)
        if (!missing(conf.level) && (length(conf.level) != 1 || 
            !is.finite(conf.level) || conf.level < 0 || conf.level > 
            1)) 
            stop("'conf.level' must be a single number between 0 and 1")
        NVAL <- c(`common odds ratio` = 1)
        if (!exact) {
            s.x <- apply(x, c(1L, 3L), sum)
            s.y <- apply(x, c(2L, 3L), sum)
            n <- as.double(apply(x, 3L, sum))
            DELTA <- sum(x[1, 1, ] - s.x[1, ] * s.y[1, ]/n)
            YATES <- if (correct && (abs(DELTA) >= 0.5)) 
                0.5
            else 0
            STATISTIC <- ((abs(DELTA) - YATES)^2/sum(apply(rbind(s.x, 
                s.y), 2L, prod)/(n^2 * (n - 1))))
            PARAMETER <- 1
            if (alternative == "two.sided") 
                PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
            else {
                z <- sign(DELTA) * sqrt(STATISTIC)
                PVAL <- pnorm(z, lower.tail = (alternative == 
                  "less"))
            }
            names(STATISTIC) <- "Mantel-Haenszel X-squared"
            names(PARAMETER) <- "df"
            METHOD <- paste("Mantel-Haenszel chi-squared test", 
                if (YATES) 
                  "with"
                else "without", "continuity correction")
            s.diag <- sum(x[1L, 1L, ] * x[2L, 2L, ]/n)
            s.offd <- sum(x[1L, 2L, ] * x[2L, 1L, ]/n)
            ESTIMATE <- s.diag/s.offd
            sd <- sqrt(sum((x[1L, 1L, ] + x[2L, 2L, ]) * x[1L, 
                1L, ] * x[2L, 2L, ]/n^2)/(2 * s.diag^2) + sum(((x[1L, 
                1L, ] + x[2L, 2L, ]) * x[1L, 2L, ] * x[2L, 1L, 
                ] + (x[1L, 2L, ] + x[2L, 1L, ]) * x[1L, 1L, ] * 
                x[2L, 2L, ])/n^2)/(2 * s.diag * s.offd) + sum((x[1L, 
                2L, ] + x[2L, 1L, ]) * x[1L, 2L, ] * x[2L, 1L, 
                ]/n^2)/(2 * s.offd^2))
            CINT <- switch(alternative, less = c(0, ESTIMATE * 
                exp(qnorm(conf.level) * sd)), greater = c(ESTIMATE * 
                exp(qnorm(conf.level, lower.tail = FALSE) * sd), 
                Inf), two.sided = {
                ESTIMATE * exp(c(1, -1) * qnorm((1 - conf.level)/2) * 
                  sd)
            })
            RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
                p.value = PVAL)
        }
        else {
            METHOD <- paste("Exact conditional test of independence", 
                "in 2 x 2 x k tables")
            mn <- apply(x, c(2L, 3L), sum)
            m <- mn[1L, ]
            n <- mn[2L, ]
            t <- apply(x, c(1L, 3L), sum)[1L, ]
            s <- sum(x[1L, 1L, ])
            lo <- sum(pmax(0, t - n))
            hi <- sum(pmin(m, t))
            support <- lo:hi
            dc <- .Call(C_d2x2xk, K, m, n, t, hi - lo + 1L)
            logdc <- log(dc)
            dn2x2xk <- function(ncp) {
                if (ncp == 1) 
                  return(dc)
                d <- logdc + log(ncp) * support
                d <- exp(d - max(d))
                d/sum(d)
            }
            mn2x2xk <- function(ncp) {
                if (ncp == 0) 
                  return(lo)
                if (ncp == Inf) 
                  return(hi)
                sum(support * dn2x2xk(ncp))
            }
            pn2x2xk <- function(q, ncp = 1, upper.tail = FALSE) {
                if (ncp == 0) {
                  if (upper.tail) 
                    return(as.numeric(q <= lo))
                  else return(as.numeric(q >= lo))
                }
                if (ncp == Inf) {
                  if (upper.tail) 
                    return(as.numeric(q <= hi))
                  else return(as.numeric(q >= hi))
                }
                d <- dn2x2xk(ncp)
                if (upper.tail) 
                  sum(d[support >= q])
                else sum(d[support <= q])
            }
            PVAL <- switch(alternative, less = pn2x2xk(s, 1), 
                greater = pn2x2xk(s, 1, upper.tail = TRUE), two.sided = {
                  relErr <- 1 + 10^(-7)
                  d <- dc
                  sum(d[d <= d[s - lo + 1] * relErr])
                })
            mle <- function(x) {
                if (x == lo) 
                  return(0)
                if (x == hi) 
                  return(Inf)
                mu <- mn2x2xk(1)
                if (mu > x) 
                  uniroot(function(t) mn2x2xk(t) - x, c(0, 1))$root
                else if (mu < x) 
                  1/uniroot(function(t) mn2x2xk(1/t) - x, c(.Machine$double.eps, 
                    1))$root
                else 1
            }
            ESTIMATE <- mle(s)
            ncp.U <- function(x, alpha) {
                if (x == hi) 
                  return(Inf)
                p <- pn2x2xk(x, 1)
                if (p < alpha) 
                  uniroot(function(t) pn2x2xk(x, t) - alpha, 
                    c(0, 1))$root
                else if (p > alpha) 
                  1/uniroot(function(t) pn2x2xk(x, 1/t) - alpha, 
                    c(.Machine$double.eps, 1))$root
                else 1
            }
            ncp.L <- function(x, alpha) {
                if (x == lo) 
                  return(0)
                p <- pn2x2xk(x, 1, upper.tail = TRUE)
                if (p > alpha) 
                  uniroot(function(t) pn2x2xk(x, t, upper.tail = TRUE) - 
                    alpha, c(0, 1))$root
                else if (p < alpha) 
                  1/uniroot(function(t) pn2x2xk(x, 1/t, upper.tail = TRUE) - 
                    alpha, c(.Machine$double.eps, 1))$root
                else 1
            }
            CINT <- switch(alternative, less = c(0, ncp.U(s, 
                1 - conf.level)), greater = c(ncp.L(s, 1 - conf.level), 
                Inf), two.sided = {
                alpha <- (1 - conf.level)/2
                c(ncp.L(s, alpha), ncp.U(s, alpha))
            })
            STATISTIC <- c(S = s)
            RVAL <- list(statistic = STATISTIC, p.value = PVAL)
        }
        names(ESTIMATE) <- names(NVAL)
        attr(CINT, "conf.level") <- conf.level
        RVAL <- c(RVAL, list(conf.int = CINT, estimate = ESTIMATE, 
            null.value = NVAL, alternative = alternative))
    }
    else {
        df <- (I - 1) * (J - 1)
        n <- m <- double(length = df)
        V <- matrix(0, nrow = df, ncol = df)
        for (k in 1:K) {
            f <- x[, , k]
            ntot <- sum(f)
            rowsums <- apply(f, 1L, sum)[-I]
            colsums <- apply(f, 2L, sum)[-J]
            n <- n + c(f[-I, -J])
            m <- m + c(outer(rowsums, colsums, "*"))/ntot
            V <- V + (kronecker(diag(ntot * colsums, nrow = J - 
                1) - outer(colsums, colsums), diag(ntot * rowsums, 
                nrow = I - 1) - outer(rowsums, rowsums))/(ntot^2 * 
                (ntot - 1)))
        }
        n <- n - m
        STATISTIC <- c(crossprod(n, qr.solve(V, n)))
        PARAMETER <- df
        PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
        names(STATISTIC) <- "Cochran-Mantel-Haenszel M^2"
        names(PARAMETER) <- "df"
        METHOD <- "Cochran-Mantel-Haenszel test"
        RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
            p.value = PVAL)
    }
    RVAL <- c(RVAL, list(method = METHOD, data.name = DNAME))
    class(RVAL) <- "htest"
    return(RVAL)
}


aggregate <- function (x, ...) 
UseMethod("aggregate")


qcauchy <- function (p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qcauchy, p, location, scale, lower.tail, log.p)


loess.smooth <- function (x, y, span = 2/3, degree = 1, family = c("symmetric", 
    "gaussian"), evaluation = 50, ...) 
{
    notna <- !(is.na(x) | is.na(y))
    x <- x[notna]
    y <- y[notna]
    new.x <- seq.int(min(x), max(x), length.out = evaluation)
    control <- loess.control(...)
    w <- rep_len(1, length(y))
    family <- match.arg(family)
    iterations <- if (family == "gaussian") 
        1L
    else control$iterations
    kd <- simpleLoess(y, x, w, span, degree = degree, parametric = FALSE, 
        drop.square = FALSE, normalize = FALSE, statistics = "none", 
        surface = "interpolate", cell = control$cell, iterations = iterations, 
        iterTrace = control$iterTrace, trace.hat = control$trace.hat)$kd
    z <- .C(C_loess_ifit, as.integer(kd$parameter), as.integer(kd$a), 
        as.double(kd$xi), as.double(kd$vert), as.double(kd$vval), 
        as.integer(evaluation), as.double(new.x), fit = double(evaluation))$fit
    list(x = new.x, y = z)
}


predict.glm <- function (object, newdata = NULL, type = c("link", "response", 
    "terms"), se.fit = FALSE, dispersion = NULL, terms = NULL, 
    na.action = na.pass, ...) 
{
    type <- match.arg(type)
    na.act <- object$na.action
    object$na.action <- NULL
    if (!se.fit) {
        if (missing(newdata)) {
            pred <- switch(type, link = object$linear.predictors, 
                response = object$fitted.values, terms = predict.lm(object, 
                  se.fit = se.fit, scale = 1, type = "terms", 
                  terms = terms))
            if (!is.null(na.act)) 
                pred <- napredict(na.act, pred)
        }
        else {
            pred <- predict.lm(object, newdata, se.fit, scale = 1, 
                type = ifelse(type == "link", "response", type), 
                terms = terms, na.action = na.action)
            switch(type, response = {
                pred <- family(object)$linkinv(pred)
            }, link = , terms = )
        }
    }
    else {
        if (inherits(object, "survreg")) 
            dispersion <- 1
        if (is.null(dispersion) || dispersion == 0) 
            dispersion <- summary(object, dispersion = dispersion)$dispersion
        residual.scale <- as.vector(sqrt(dispersion))
        pred <- predict.lm(object, newdata, se.fit, scale = residual.scale, 
            type = ifelse(type == "link", "response", type), 
            terms = terms, na.action = na.action)
        fit <- pred$fit
        se.fit <- pred$se.fit
        switch(type, response = {
            se.fit <- se.fit * abs(family(object)$mu.eta(fit))
            fit <- family(object)$linkinv(fit)
        }, link = , terms = )
        if (missing(newdata) && !is.null(na.act)) {
            fit <- napredict(na.act, fit)
            se.fit <- napredict(na.act, se.fit)
        }
        pred <- list(fit = fit, se.fit = se.fit, residual.scale = residual.scale)
    }
    pred
}


extractAIC <- function (fit, scale, k = 2, ...) 
UseMethod("extractAIC")


punif <- function (q, min = 0, max = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_punif, q, min, max, lower.tail, log.p)


approxfun <- function (x, y = NULL, method = "linear", yleft, yright, rule = 1, 
    f = 0, ties = mean) 
{
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method)) 
        stop("invalid interpolation method")
    stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1L, 
        lenR <= 2L)
    if (lenR == 1) 
        rule <- rule[c(1, 1)]
    x <- regularize.values(x, y, ties)
    y <- x$y
    x <- x$x
    n <- as.integer(length(x))
    if (is.na(n)) 
        stop("invalid length(x)")
    if (n <= 1) {
        if (method == 1) 
            stop("need at least two non-NA values to interpolate")
        if (n == 0) 
            stop("zero non-NA points")
    }
    if (missing(yleft)) 
        yleft <- if (rule[1L] == 1) 
            NA
        else y[1L]
    if (missing(yright)) 
        yright <- if (rule[2L] == 1) 
            NA
        else y[length(y)]
    force(f)
    stopifnot(length(yleft) == 1L, length(yright) == 1L, length(f) == 
        1L)
    rm(rule, ties, lenR, n)
    x <- as.double(x)
    y <- as.double(y)
    .Call(C_ApproxTest, x, y, method, f)
    function(v) .approxfun(x, y, v, method, yleft, yright, f)
}


napredict <- function (omit, x, ...) 
UseMethod("napredict")


weights <- function (object, ...) 
UseMethod("weights")


quasibinomial <- function (link = "logit") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
    if (linktemp %in% okLinks) 
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    }
    else {
        if (inherits(link, "link-glm")) {
            stats <- link
            if (!is.null(stats$name)) 
                linktemp <- stats$name
        }
        else {
            stop(gettextf("link \"%s\" not available for quasibinomial family; available links are %s", 
                linktemp, paste(sQuote(okLinks), collapse = ", ")), 
                domain = NA)
        }
    }
    variance <- function(mu) mu * (1 - mu)
    validmu <- function(mu) all(is.finite(mu)) && all(mu > 0 & 
        mu < 1)
    dev.resids <- function(y, mu, wt) .Call(C_binomial_dev_resids, 
        y, mu, wt)
    aic <- function(y, n, mu, wt, dev) NA
    initialize <- expression({
        if (NCOL(y) == 1) {
            if (is.factor(y)) y <- y != levels(y)[1L]
            n <- rep.int(1, nobs)
            if (any(y < 0 | y > 1)) stop("y values must be 0 <= y <= 1")
            mustart <- (weights * y + 0.5)/(weights + 1)
        } else if (NCOL(y) == 2) {
            n <- y[, 1] + y[, 2]
            y <- ifelse(n == 0, 0, y[, 1]/n)
            weights <- weights * n
            mustart <- (n * y + 0.5)/(n + 1)
        } else stop("for the 'quasibinomial' family, y must be a vector of 0 and 1's\nor a 2 column matrix where col 1 is no. successes and col 2 is no. failures")
    })
    structure(list(family = "quasibinomial", link = linktemp, 
        linkfun = stats$linkfun, linkinv = stats$linkinv, variance = variance, 
        dev.resids = dev.resids, aic = aic, mu.eta = stats$mu.eta, 
        initialize = initialize, validmu = validmu, valideta = stats$valideta), 
        class = "family")
}


SSweibull <- function (x, Asym, Drop, lrc, pwr) 
{
    .expr1 <- exp(lrc)
    .expr3 <- x^pwr
    .expr5 <- exp(-(ee <- .expr1 * .expr3))
    .value <- Asym - (De <- Drop * .expr5)
    .actualArgs <- as.list(match.call()[c("Asym", "Drop", "lrc", 
        "pwr")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .grad <- array(0, c(length(.value), 4L), list(NULL, c("Asym", 
            "Drop", "lrc", "pwr")))
        .grad[, "Asym"] <- 1
        .grad[, "Drop"] <- -.expr5
        .grad[, "lrc"] <- lrc <- De * ee
        .grad[, "pwr"] <- lrc * log(x)
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


ave <- function (x, ..., FUN = mean) 
{
    if (missing(...)) 
        x[] <- FUN(x)
    else {
        g <- interaction(...)
        split(x, g) <- lapply(split(x, g), FUN)
    }
    x
}


ARMAacf <- function (ar = numeric(), ma = numeric(), lag.max = r, pacf = FALSE) 
{
    p <- length(ar)
    q <- length(ma)
    if (!p && !q) 
        stop("empty model supplied")
    r <- max(p, q + 1)
    if (p > 0) {
        if (r > 1) {
            if (r > p) {
                ar <- c(ar, rep(0, r - p))
                p <- r
            }
            p1 <- p + 1L
            p2.1 <- p + p1
            A <- matrix(0, p1, p2.1)
            ind <- seq_len(p1)
            ind <- as.matrix(expand.grid(ind, ind))[, 2L:1L]
            ind[, 2] <- ind[, 1L] + ind[, 2L] - 1L
            A[ind] <- c(1, -ar)
            A[, 1L:p] <- A[, 1L:p] + A[, p2.1:(p + 2L)]
            rhs <- c(1, rep(0, p))
            if (q > 0) {
                psi <- c(1, ARMAtoMA(ar, ma, q))
                theta <- c(1, ma, rep(0, q + 1L))
                for (k in 1L + 0:q) rhs[k] <- sum(psi * theta[k + 
                  0:q])
            }
            ind <- p1:1
            Acf <- solve(A[ind, ind], rhs)
            Acf <- Acf[-1L]/Acf[1L]
        }
        else Acf <- ar
        if (lag.max > p) {
            xx <- rep(0, lag.max - p)
            Acf <- c(Acf, filter(xx, ar, "recursive", init = rev(Acf)))
        }
        Acf <- c(1, Acf[1L:lag.max])
    }
    else if (q > 0) {
        x <- c(1, ma)
        Acf <- filter(c(x, rep(0, q)), rev(x), sides = 1)[-(1L:q)]
        if (lag.max > q) 
            Acf <- c(Acf, rep(0, lag.max - q))
        Acf <- Acf/Acf[1L]
    }
    names(Acf) <- 0:lag.max
    if (pacf) 
        drop(.Call(C_pacf1, Acf, lag.max))
    else Acf
}


bw.ucv <- function (x, nb = 1000L, lower = 0.1 * hmax, upper = hmax, tol = 0.1 * 
    lower) 
{
    if ((n <- length(x)) < 2L) 
        stop("need at least 2 data points")
    n <- as.integer(n)
    if (is.na(n)) 
        stop("invalid length(x)")
    if (!is.numeric(x)) 
        stop("invalid 'x'")
    nb <- as.integer(nb)
    if (is.na(nb) || nb <= 0L) 
        stop("invalid 'nb'")
    storage.mode(x) <- "double"
    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    Z <- bw_pair_cnts(x, nb, n > nb/2)
    d <- Z[[1L]]
    cnt <- Z[[2L]]
    fucv <- function(h) .Call(C_bw_ucv, n, d, cnt, h)
    h <- optimize(fucv, c(lower, upper), tol = tol)$minimum
    if (h < lower + tol | h > upper - tol) 
        warning("minimum occurred at one end of the range")
    h
}


estVar <- function (object, ...) 
UseMethod("estVar")


cor <- function (x, y = NULL, use = "everything", method = c("pearson", 
    "kendall", "spearman")) 
{
    na.method <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs", 
        "everything", "na.or.complete"))
    if (is.na(na.method)) 
        stop("invalid 'use' argument")
    method <- match.arg(method)
    if (is.data.frame(y)) 
        y <- as.matrix(y)
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (!is.matrix(x) && is.null(y)) 
        stop("supply both 'x' and 'y' or a matrix-like 'x'")
    if (!(is.numeric(x) || is.logical(x))) 
        stop("'x' must be numeric")
    stopifnot(is.atomic(x))
    if (!is.null(y)) {
        if (!(is.numeric(y) || is.logical(y))) 
            stop("'y' must be numeric")
        stopifnot(is.atomic(y))
    }
    Rank <- function(u) {
        if (length(u) == 0L) 
            u
        else if (is.matrix(u)) {
            if (nrow(u) > 1L) 
                apply(u, 2L, rank, na.last = "keep")
            else row(u)
        }
        else rank(u, na.last = "keep")
    }
    if (method == "pearson") 
        .Call(C_cor, x, y, na.method, FALSE)
    else if (na.method %in% c(2L, 5L)) {
        if (is.null(y)) {
            .Call(C_cor, Rank(na.omit(x)), NULL, na.method, method == 
                "kendall")
        }
        else {
            nas <- attr(na.omit(cbind(x, y)), "na.action")
            dropNA <- function(x, nas) {
                if (length(nas)) {
                  if (is.matrix(x)) 
                    x[-nas, , drop = FALSE]
                  else x[-nas]
                }
                else x
            }
            .Call(C_cor, Rank(dropNA(x, nas)), Rank(dropNA(y, 
                nas)), na.method, method == "kendall")
        }
    }
    else if (na.method != 3L) {
        x <- Rank(x)
        if (!is.null(y)) 
            y <- Rank(y)
        .Call(C_cor, x, y, na.method, method == "kendall")
    }
    else {
        if (is.null(y)) {
            ncy <- ncx <- ncol(x)
            if (ncx == 0) 
                stop("'x' is empty")
            r <- matrix(0, nrow = ncx, ncol = ncy)
            for (i in seq_len(ncx)) {
                for (j in seq_len(i)) {
                  x2 <- x[, i]
                  y2 <- x[, j]
                  ok <- complete.cases(x2, y2)
                  x2 <- rank(x2[ok])
                  y2 <- rank(y2[ok])
                  r[i, j] <- if (any(ok)) 
                    .Call(C_cor, x2, y2, 1L, method == "kendall")
                  else NA
                }
            }
            r <- r + t(r) - diag(diag(r))
            rownames(r) <- colnames(x)
            colnames(r) <- colnames(x)
            r
        }
        else {
            if (length(x) == 0L || length(y) == 0L) 
                stop("both 'x' and 'y' must be non-empty")
            matrix_result <- is.matrix(x) || is.matrix(y)
            if (!is.matrix(x)) 
                x <- matrix(x, ncol = 1L)
            if (!is.matrix(y)) 
                y <- matrix(y, ncol = 1L)
            ncx <- ncol(x)
            ncy <- ncol(y)
            r <- matrix(0, nrow = ncx, ncol = ncy)
            for (i in seq_len(ncx)) {
                for (j in seq_len(ncy)) {
                  x2 <- x[, i]
                  y2 <- y[, j]
                  ok <- complete.cases(x2, y2)
                  x2 <- rank(x2[ok])
                  y2 <- rank(y2[ok])
                  r[i, j] <- if (any(ok)) 
                    .Call(C_cor, x2, y2, 1L, method == "kendall")
                  else NA
                }
            }
            rownames(r) <- colnames(x)
            colnames(r) <- colnames(y)
            if (matrix_result) 
                r
            else drop(r)
        }
    }
}


update.formula <- function (old, new, ...) 
{
    tmp <- .Call(C_updateform, as.formula(old), as.formula(new))
    out <- formula(terms.formula(tmp, simplify = TRUE))
    return(out)
}


pbirthday <- function (n, classes = 365, coincident = 2) 
{
    k <- coincident
    c <- classes
    if (k < 2) 
        return(1)
    if (k == 2) 
        return(1 - prod((c:(c - n + 1))/rep(c, n)))
    if (k > n) 
        return(0)
    if (n > c * (k - 1)) 
        return(1)
    LHS <- n * exp(-n/(c * k))/(1 - n/(c * (k + 1)))^(1/k)
    lxx <- k * log(LHS) - (k - 1) * log(c) - lgamma(k + 1)
    -expm1(-exp(lxx))
}


se.contrast <- function (object, ...) 
UseMethod("se.contrast")


alias <- function (object, ...) 
UseMethod("alias")


rcauchy <- function (n, location = 0, scale = 1) 
.Call(C_rcauchy, n, location, scale)


cov <- function (x, y = NULL, use = "everything", method = c("pearson", 
    "kendall", "spearman")) 
{
    na.method <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs", 
        "everything", "na.or.complete"))
    if (is.na(na.method)) 
        stop("invalid 'use' argument")
    method <- match.arg(method)
    if (is.data.frame(y)) 
        y <- as.matrix(y)
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (!is.matrix(x) && is.null(y)) 
        stop("supply both 'x' and 'y' or a matrix-like 'x'")
    stopifnot(is.numeric(x) || is.logical(x), is.atomic(x))
    if (!is.null(y)) 
        stopifnot(is.numeric(y) || is.logical(y), is.atomic(y))
    Rank <- function(u) {
        if (length(u) == 0L) 
            u
        else if (is.matrix(u)) {
            if (nrow(u) > 1L) 
                apply(u, 2L, rank, na.last = "keep")
            else row(u)
        }
        else rank(u, na.last = "keep")
    }
    if (method == "pearson") 
        .Call(C_cov, x, y, na.method, method == "kendall")
    else if (na.method %in% c(2L, 5L)) {
        if (is.null(y)) {
            .Call(C_cov, Rank(na.omit(x)), NULL, na.method, method == 
                "kendall")
        }
        else {
            nas <- attr(na.omit(cbind(x, y)), "na.action")
            dropNA <- function(x, nas) {
                if (length(nas)) {
                  if (is.matrix(x)) 
                    x[-nas, , drop = FALSE]
                  else x[-nas]
                }
                else x
            }
            .Call(C_cov, Rank(dropNA(x, nas)), Rank(dropNA(y, 
                nas)), na.method, method == "kendall")
        }
    }
    else if (na.method != 3L) {
        x <- Rank(x)
        if (!is.null(y)) 
            y <- Rank(y)
        .Call(C_cov, x, y, na.method, method == "kendall")
    }
    else stop("cannot handle 'pairwise.complete.obs'")
}


case.names <- function (object, ...) 
UseMethod("case.names")


ls.print <- function (ls.out, digits = 4L, print.it = TRUE) 
{
    resids <- as.matrix(ls.out$residuals)
    if (!is.null(ls.out$wt)) {
        if (any(ls.out$wt == 0)) 
            warning("observations with 0 weights not used")
        resids <- resids * sqrt(ls.out$wt)
    }
    n <- apply(resids, 2L, length) - colSums(is.na(resids))
    lsqr <- ls.out$qr
    p <- lsqr$rank
    if (ls.out$intercept) {
        if (is.matrix(lsqr$qt)) 
            totss <- colSums(lsqr$qt[-1L, ]^2)
        else totss <- sum(lsqr$qt[-1L]^2)
        degfree <- p - 1
    }
    else {
        totss <- colSums(as.matrix(lsqr$qt^2))
        degfree <- p
    }
    resss <- colSums(resids^2, na.rm = TRUE)
    resse <- (resss/(n - p))^0.5
    regss <- totss - resss
    rsquared <- regss/totss
    fstat <- (regss/degfree)/(resss/(n - p))
    pvalue <- pf(fstat, degfree, (n - p), lower.tail = FALSE)
    Ynames <- colnames(resids)
    summary <- cbind(format(round(resse, digits)), format(round(rsquared, 
        digits)), format(round(fstat, digits)), format(degfree), 
        format(n - p), format(round(pvalue, digits)))
    dimnames(summary) <- list(Ynames, c("Mean Sum Sq", "R Squared", 
        "F-value", "Df 1", "Df 2", "Pr(>F)"))
    mat <- as.matrix(lsqr$qr[1L:p, 1L:p])
    mat[row(mat) > col(mat)] <- 0
    qrinv <- solve(mat)
    m.y <- ncol(resids)
    coef.table <- as.list(1L:m.y)
    if (m.y == 1) 
        coef <- matrix(ls.out$coefficients, ncol = 1)
    else coef <- ls.out$coefficients
    for (i in 1L:m.y) {
        covmat <- (resss[i]/(n[i] - p)) * (qrinv %*% t(qrinv))
        se <- diag(covmat)^0.5
        coef.table[[i]] <- cbind(coef[, i], se, coef[, i]/se, 
            2 * pt(abs(coef[, i]/se), n[i] - p, lower.tail = FALSE))
        dimnames(coef.table[[i]]) <- list(colnames(lsqr$qr), 
            c("Estimate", "Std.Err", "t-value", "Pr(>|t|)"))
        if (print.it) {
            if (m.y > 1) 
                cat("Response:", Ynames[i], "\n\n")
            cat(paste("Residual Standard Error=", format(round(resse[i], 
                digits)), "\nR-Square=", format(round(rsquared[i], 
                digits)), "\nF-statistic (df=", format(degfree), 
                ", ", format(n[i] - p), ")=", format(round(fstat[i], 
                  digits)), "\np-value=", format(round(pvalue[i], 
                  digits)), "\n\n", sep = ""))
            print(round(coef.table[[i]], digits))
            cat("\n\n")
        }
    }
    names(coef.table) <- Ynames
    invisible(list(summary = summary, coef.table = coef.table))
}


nlminb <- function (start, objective, gradient = NULL, hessian = NULL, 
    ..., scale = 1, control = list(), lower = -Inf, upper = Inf) 
{
    par <- setNames(as.double(start), names(start))
    n <- length(par)
    iv <- integer(78 + 3 * n)
    v <- double(130 + (n * (n + 27))/2)
    .Call(C_port_ivset, 2, iv, v)
    if (length(control)) {
        nms <- names(control)
        if (!is.list(control) || is.null(nms)) 
            stop("'control' argument must be a named list")
        pos <- pmatch(nms, names(port_cpos))
        if (any(nap <- is.na(pos))) {
            warning(sprintf(ngettext(length(nap), "unrecognized control element named %s ignored", 
                "unrecognized control elements named %s ignored"), 
                paste(sQuote(nms[nap]), collapse = ", ")), domain = NA)
            pos <- pos[!nap]
            control <- control[!nap]
        }
        ivpars <- pos <= 4
        vpars <- !ivpars
        if (any(ivpars)) 
            iv[port_cpos[pos[ivpars]]] <- as.integer(unlist(control[ivpars]))
        if (any(vpars)) 
            v[port_cpos[pos[vpars]]] <- as.double(unlist(control[vpars]))
    }
    obj <- quote(objective(.par, ...))
    rho <- new.env(parent = environment())
    assign(".par", par, envir = rho)
    grad <- hess <- low <- upp <- NULL
    if (!is.null(gradient)) {
        grad <- quote(gradient(.par, ...))
        if (!is.null(hessian)) {
            if (is.logical(hessian)) 
                stop("logical 'hessian' argument not allowed.  See documentation.")
            hess <- quote(hessian(.par, ...))
        }
    }
    if (any(lower != -Inf) || any(upper != Inf)) {
        low <- rep_len(as.double(lower), length(par))
        upp <- rep_len(as.double(upper), length(par))
    }
    else low <- upp <- numeric()
    .Call(C_port_nlminb, obj, grad, hess, rho, low, upp, d = rep_len(as.double(scale), 
        length(par)), iv, v)
    iv1 <- iv[1L]
    list(par = get(".par", envir = rho), objective = v[10L], 
        convergence = (if (iv1 %in% 3L:6L) 0L else 1L), iterations = iv[31L], 
        evaluations = c(`function` = iv[6L], gradient = iv[30L]), 
        message = if (19 <= iv1 && iv1 <= 43) {
            if (any(B <- iv1 == port_cpos)) sprintf("'control' component '%s' = %g, is out of range", 
                names(port_cpos)[B], v[iv1]) else sprintf("V[IV[1]] = V[%d] = %g is out of range (see PORT docu.)", 
                iv1, v[iv1])
        } else port_msg(iv1))
}


TukeyHSD <- function (x, which, ordered = FALSE, conf.level = 0.95, ...) 
UseMethod("TukeyHSD")


hatvalues <- function (model, ...) 
UseMethod("hatvalues")


contr.sum <- function (n, contrasts = TRUE, sparse = FALSE) 
{
    if (length(n) <= 1L) {
        if (is.numeric(n) && length(n) == 1L && n > 1L) 
            levels <- seq_len(n)
        else stop("not enough degrees of freedom to define contrasts")
    }
    else levels <- n
    levels <- as.character(levels)
    cont <- .Diag(levels, sparse = sparse)
    if (contrasts) {
        cont <- cont[, -length(levels), drop = FALSE]
        cont[length(levels), ] <- -1
        colnames(cont) <- NULL
    }
    cont
}


ppoints <- function (n, a = if (n <= 10) 3/8 else 1/2) 
{
    if (length(n) > 1L) 
        n <- length(n)
    if (n > 0) 
        (1L:n - a)/(n + 1 - 2 * a)
    else numeric()
}


filter <- function (x, filter, method = c("convolution", "recursive"), 
    sides = 2L, circular = FALSE, init = NULL) 
{
    method <- match.arg(method)
    x <- as.ts(x)
    storage.mode(x) <- "double"
    xtsp <- tsp(x)
    n <- as.integer(NROW(x))
    if (is.na(n)) 
        stop(gettextf("invalid value of %s", "NROW(x)"), domain = NA)
    nser <- NCOL(x)
    filter <- as.double(filter)
    nfilt <- as.integer(length(filter))
    if (is.na(nfilt)) 
        stop(gettextf("invalid value of %s", "length(filter)"), 
            domain = NA)
    if (anyNA(filter)) 
        stop("missing values in 'filter'")
    if (method == "convolution") {
        if (nfilt > n) 
            stop("'filter' is longer than time series")
        sides <- as.integer(sides)
        if (is.na(sides) || (sides != 1L && sides != 2L)) 
            stop("argument 'sides' must be 1 or 2")
        circular <- as.logical(circular)
        if (is.na(circular)) 
            stop("'circular' must be logical and not NA")
        if (is.matrix(x)) {
            y <- matrix(NA, n, nser)
            for (i in seq_len(nser)) y[, i] <- .Call(C_cfilter, 
                x[, i], filter, sides, circular)
        }
        else y <- .Call(C_cfilter, x, filter, sides, circular)
    }
    else {
        if (missing(init)) {
            init <- matrix(0, nfilt, nser)
        }
        else {
            ni <- NROW(init)
            if (ni != nfilt) 
                stop("length of 'init' must equal length of 'filter'")
            if (NCOL(init) != 1L && NCOL(init) != nser) {
                stop(sprintf(ngettext(nser, "'init' must have %d column", 
                  "'init' must have 1 or %d columns", domain = "R-stats"), 
                  nser), domain = NA)
            }
            if (!is.matrix(init)) 
                dim(init) <- c(nfilt, nser)
        }
        ind <- seq_len(nfilt)
        if (is.matrix(x)) {
            y <- matrix(NA, n, nser)
            for (i in seq_len(nser)) y[, i] <- .Call(C_rfilter, 
                x[, i], filter, c(rev(init[, i]), double(n)))[-ind]
        }
        else y <- .Call(C_rfilter, x, filter, c(rev(init[, 1L]), 
            double(n)))[-ind]
    }
    tsp(y) <- xtsp
    class(y) <- if (nser > 1L) 
        c("mts", "ts")
    else "ts"
    y
}


deviance <- function (object, ...) 
UseMethod("deviance")


KalmanRun <- function (y, mod, nit = 0L, update = FALSE) 
{
    z <- .Call(C_KalmanLike, y, mod, nit, TRUE, update)
    x <- z$values
    z[[1L]] <- c(Lik = 0.5 * (log(x[1L]) + x[2L]), s2 = x[1L])
    z
}


pnorm <- function (q, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_pnorm, q, mean, sd, lower.tail, log.p)


lag <- function (x, ...) 
UseMethod("lag")


ar.yw <- function (x, ...) 
UseMethod("ar.yw")


lm <- function (formula, data, subset, weights, na.action, method = "qr", 
    model = TRUE, x = FALSE, y = FALSE, qr = TRUE, singular.ok = TRUE, 
    contrasts = NULL, offset, ...) 
{
    ret.x <- x
    ret.y <- y
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (method == "model.frame") 
        return(mf)
    else if (method != "qr") 
        warning(gettextf("method = '%s' is not supported. Using 'qr'", 
            method), domain = NA)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- as.vector(model.weights(mf))
    if (!is.null(w) && !is.numeric(w)) 
        stop("'weights' must be a numeric vector")
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(y)) 
            stop(gettextf("number of offsets is %d, should equal %d (number of observations)", 
                length(offset), NROW(y)), domain = NA)
    }
    if (is.empty.model(mt)) {
        x <- NULL
        z <- list(coefficients = if (is.matrix(y)) matrix(, 0, 
            3) else numeric(), residuals = y, fitted.values = 0 * 
            y, weights = w, rank = 0L, df.residual = if (!is.null(w)) sum(w != 
            0) else if (is.matrix(y)) nrow(y) else length(y))
        if (!is.null(offset)) {
            z$fitted.values <- offset
            z$residuals <- y - offset
        }
    }
    else {
        x <- model.matrix(mt, mf, contrasts)
        z <- if (is.null(w)) 
            lm.fit(x, y, offset = offset, singular.ok = singular.ok, 
                ...)
        else lm.wfit(x, y, w, offset = offset, singular.ok = singular.ok, 
            ...)
    }
    class(z) <- c(if (is.matrix(y)) "mlm", "lm")
    z$na.action <- attr(mf, "na.action")
    z$offset <- offset
    z$contrasts <- attr(x, "contrasts")
    z$xlevels <- .getXlevels(mt, mf)
    z$call <- cl
    z$terms <- mt
    if (model) 
        z$model <- mf
    if (ret.x) 
        z$x <- x
    if (ret.y) 
        z$y <- y
    if (!qr) 
        z$qr <- NULL
    z
}


r2dtable <- function (n, r, c) 
{
    if (length(n) == 0L || (n < 0) || is.na(n)) 
        stop("invalid argument 'n'")
    if ((length(r) <= 1L) || any(r < 0) || anyNA(r)) 
        stop("invalid argument 'r'")
    if ((length(c) <= 1L) || any(c < 0) || anyNA(c)) 
        stop("invalid argument 'c'")
    if (sum(r) != sum(c)) 
        stop("arguments 'r' and 'c' must have the same sums")
    .Call(C_r2dtable, as.integer(n), as.integer(r), as.integer(c))
}


cpgram <- function (ts, taper = 0.1, main = paste("Series: ", deparse(substitute(ts))), 
    ci.col = "blue") 
{
    main
    if (NCOL(ts) > 1) 
        stop("only implemented for univariate time series")
    x <- as.vector(ts)
    x <- x[!is.na(x)]
    x <- spec.taper(scale(x, TRUE, FALSE), p = taper)
    y <- Mod(fft(x))^2/length(x)
    y[1L] <- 0
    n <- length(x)
    x <- (0:(n/2)) * frequency(ts)/n
    if (length(x)%%2 == 0) {
        n <- length(x) - 1
        y <- y[1L:n]
        x <- x[1L:n]
    }
    else y <- y[seq_along(x)]
    xm <- frequency(ts)/2
    mp <- length(x) - 1
    crit <- 1.358/(sqrt(mp) + 0.12 + 0.11/sqrt(mp))
    oldpty <- par(pty = "s")
    on.exit(par(oldpty))
    plot(x, cumsum(y)/sum(y), type = "s", xlim = c(0, xm), ylim = c(0, 
        1), xaxs = "i", yaxs = "i", xlab = "frequency", ylab = "")
    lines(c(0, xm * (1 - crit)), c(crit, 1), col = ci.col, lty = 2)
    lines(c(xm * crit, xm), c(0, 1 - crit), col = ci.col, lty = 2)
    title(main = main)
    invisible()
}


mcnemar.test <- function (x, y = NULL, correct = TRUE) 
{
    if (is.matrix(x)) {
        r <- nrow(x)
        if ((r < 2) || (ncol(x) != r)) 
            stop("'x' must be square with at least two rows and columns")
        if (any(x < 0) || anyNA(x)) 
            stop("all entries of 'x' must be nonnegative and finite")
        DNAME <- deparse(substitute(x))
    }
    else {
        if (is.null(y)) 
            stop("if 'x' is not a matrix, 'y' must be given")
        if (length(x) != length(y)) 
            stop("'x' and 'y' must have the same length")
        DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
        OK <- complete.cases(x, y)
        x <- as.factor(x[OK])
        y <- as.factor(y[OK])
        r <- nlevels(x)
        if ((r < 2) || (nlevels(y) != r)) 
            stop("'x' and 'y' must have the same number of levels (minimum 2)")
        x <- table(x, y)
    }
    PARAMETER <- r * (r - 1)/2
    METHOD <- "McNemar's Chi-squared test"
    if (correct && (r == 2) && any(x - t(x) != 0)) {
        y <- (abs(x - t(x)) - 1)
        METHOD <- paste(METHOD, "with continuity correction")
    }
    else y <- x - t(x)
    x <- x + t(x)
    STATISTIC <- sum(y[upper.tri(x)]^2/x[upper.tri(x)])
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    names(STATISTIC) <- "McNemar's chi-squared"
    names(PARAMETER) <- "df"
    RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}


NLSstClosestX <- function (xy, yval) 
UseMethod("NLSstClosestX")


makepredictcall <- function (var, call) 
UseMethod("makepredictcall")


NLSstLfAsymptote <- function (xy) 
UseMethod("NLSstLfAsymptote")


simulate <- function (object, nsim = 1, seed = NULL, ...) 
UseMethod("simulate")


toeplitz <- function (x) 
{
    if (!is.vector(x)) 
        stop("'x' is not a vector")
    n <- length(x)
    A <- matrix(raw(), n, n)
    matrix(x[abs(col(A) - row(A)) + 1L], n, n)
}


SSasympOrig <- function (input, Asym, lrc) 
{
    .expr1 <- exp(lrc)
    .expr4 <- exp(((-.expr1) * input))
    .expr5 <- 1 - .expr4
    .value <- Asym * .expr5
    .actualArgs <- as.list(match.call()[c("Asym", "lrc")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .grad <- array(0, c(length(.value), 2L), list(NULL, c("Asym", 
            "lrc")))
        .grad[, "Asym"] <- .expr5
        .grad[, "lrc"] <- Asym * (.expr4 * (.expr1 * input))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


summary.manova <- function (object, test = c("Pillai", "Wilks", "Hotelling-Lawley", 
    "Roy"), intercept = FALSE, tol = 1e-07, ...) 
{
    if (!inherits(object, "maov")) 
        stop(gettextf("object must be of class %s or %s", dQuote("manova"), 
            dQuote("maov")), domain = NA)
    test <- match.arg(test)
    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
    uasgn <- unique(asgn)
    nterms <- length(uasgn)
    effects <- object$effects
    if (!is.null(effects)) 
        effects <- as.matrix(effects)[seq_along(asgn), , drop = FALSE]
    rdf <- object$df.residual
    nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if (!is.null(wt)) 
        resid <- resid * sqrt(wt)
    nresp <- NCOL(resid)
    if (nresp <= 1) 
        stop("need multiple responses")
    if (is.null(effects)) {
        df <- nterms <- 0
        ss <- list(0)
        nmrows <- character()
    }
    else {
        df <- numeric(nterms)
        ss <- list(nterms)
        nmrows <- character(nterms)
        for (i in seq(nterms)) {
            ai <- (asgn == uasgn[i])
            nmrows[i] <- nmeffect[1 + uasgn[i]]
            df[i] <- sum(ai)
            ss[[i]] <- crossprod(effects[ai, , drop = FALSE])
        }
    }
    pm <- pmatch("(Intercept)", nmrows, 0L)
    if (!intercept && pm > 0) {
        nterms <- nterms - 1
        df <- df[-pm]
        nmrows <- nmrows[-pm]
        ss <- ss[-pm]
    }
    names(ss) <- nmrows
    nt <- nterms
    if (rdf > 0) {
        nt <- nterms + 1
        df[nt] <- rdf
        ss[[nt]] <- crossprod(resid)
        names(ss)[nt] <- nmrows[nt] <- "Residuals"
        ok <- df[-nt] > 0
        eigs <- array(NA, c(nterms, nresp), dimnames = list(nmrows[-nt], 
            NULL))
        stats <- matrix(NA, nt, 5, dimnames = list(nmrows, c(test, 
            "approx F", "num Df", "den Df", "Pr(>F)")))
        sc <- sqrt(sss <- diag(ss[[nt]]))
        for (i in seq_len(nterms)[ok]) sss <- sss + diag(ss[[i]])
        sc[sc < sqrt(sss) * 1e-06] <- 1
        D <- diag(1/sc)
        rss.qr <- qr(D %*% ss[[nt]] %*% D, tol = tol)
        if (rss.qr$rank < ncol(resid)) 
            stop(gettextf("residuals have rank %d < %d", rss.qr$rank, 
                ncol(resid)), domain = NA)
        if (!is.null(rss.qr)) 
            for (i in seq_len(nterms)[ok]) {
                A1 <- qr.coef(rss.qr, D %*% ss[[i]] %*% D)
                eigs[i, ] <- Re(eigen(A1, symmetric = FALSE, 
                  only.values = TRUE)$values)
                stats[i, 1L:4L] <- switch(test, Pillai = Pillai(eigs[i, 
                  ], df[i], df[nt]), Wilks = Wilks(eigs[i, ], 
                  df[i], df[nt]), `Hotelling-Lawley` = HL(eigs[i, 
                  ], df[i], df[nt]), Roy = Roy(eigs[i, ], df[i], 
                  df[nt]))
                ok <- stats[, 2L] >= 0 & stats[, 3L] > 0 & stats[, 
                  4L] > 0
                ok <- !is.na(ok) & ok
                stats[ok, 5L] <- pf(stats[ok, 2L], stats[ok, 
                  3L], stats[ok, 4L], lower.tail = FALSE)
            }
        x <- list(row.names = nmrows, SS = ss, Eigenvalues = eigs, 
            stats = cbind(Df = df, stats = stats))
    }
    else x <- list(row.names = nmrows, SS = ss, Df = df)
    class(x) <- "summary.manova"
    x
}


model.extract <- function (frame, component) 
{
    component <- as.character(substitute(component))
    rval <- switch(component, response = model.response(frame), 
        offset = model.offset(frame), frame[[paste0("(", component, 
            ")")]])
    if (!is.null(rval)) {
        if (length(rval) == nrow(frame)) 
            names(rval) <- attr(frame, "row.names")
        else if (is.matrix(rval) && nrow(rval) == nrow(frame)) {
            t1 <- dimnames(rval)
            dimnames(rval) <- list(attr(frame, "row.names"), 
                t1[[2L]])
        }
    }
    rval
}


qhyper <- function (p, m, n, k, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qhyper, p, m, n, k, lower.tail, log.p)


model.tables <- function (x, ...) 
UseMethod("model.tables")


add1 <- function (object, scope, ...) 
UseMethod("add1")


SSfol <- function (Dose, input, lKe, lKa, lCl) 
{
    .expr4 <- Dose * exp((lKe + lKa) - lCl)
    .expr5 <- exp(lKe)
    .expr8 <- exp(-.expr5 * input)
    .expr9 <- exp(lKa)
    .expr12 <- exp(-.expr9 * input)
    .expr14 <- .expr4 * (.expr8 - .expr12)
    .expr15 <- .expr9 - .expr5
    .expr16 <- .expr14/.expr15
    .expr23 <- .expr15^2
    .value <- .expr16
    .actualArgs <- as.list(match.call()[c("lKe", "lKa", "lCl")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .grad <- array(0, c(length(.value), 3L), list(NULL, c("lKe", 
            "lKa", "lCl")))
        .grad[, "lKe"] <- (.expr14 - .expr4 * (.expr8 * (.expr5 * 
            input)))/.expr15 + .expr14 * .expr5/.expr23
        .grad[, "lKa"] <- (.expr14 + .expr4 * (.expr12 * (.expr9 * 
            input)))/.expr15 - .expr14 * .expr9/.expr23
        .grad[, "lCl"] <- -.expr16
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


SSasymp <- function (input, Asym, R0, lrc) 
{
    .expr1 <- R0 - Asym
    .expr2 <- exp(lrc)
    .expr5 <- exp(((-.expr2) * input))
    .value <- Asym + (.expr1 * .expr5)
    .actualArgs <- as.list(match.call()[c("Asym", "R0", "lrc")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .grad <- array(0, c(length(.value), 3L), list(NULL, c("Asym", 
            "R0", "lrc")))
        .grad[, "Asym"] <- 1 - .expr5
        .grad[, "R0"] <- .expr5
        .grad[, "lrc"] <- -(.expr1 * (.expr5 * (.expr2 * input)))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


ts.plot <- function (..., gpars = list()) 
{
    dots <- list(...)
    pars <- c("xlab", "ylab", "xlim", "ylim", "col", "lty", "lwd", 
        "type", "main", "sub", "log")
    m <- names(dots) %in% pars
    if (length(m)) {
        gpars <- c(gpars, dots[m])
        dots <- dots[!m]
    }
    sers <- do.call("ts.union", dots)
    if (is.null(gpars$ylab)) 
        gpars$ylab <- if (NCOL(sers) > 1) 
            ""
        else deparse(substitute(...))
    do.call("plot.ts", c(list(sers, plot.type = "single"), gpars))
}


Gamma <- function (link = "inverse") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    okLinks <- c("inverse", "log", "identity")
    if (linktemp %in% okLinks) 
        stats <- make.link(linktemp)
    else if (is.character(link)) 
        stats <- make.link(link)
    else {
        if (inherits(link, "link-glm")) {
            stats <- link
            if (!is.null(stats$name)) 
                linktemp <- stats$name
        }
        else {
            stop(gettextf("link \"%s\" not available for gamma family; available links are %s", 
                linktemp, paste(sQuote(okLinks), collapse = ", ")), 
                domain = NA)
        }
    }
    variance <- function(mu) mu^2
    validmu <- function(mu) all(is.finite(mu)) && all(mu > 0)
    dev.resids <- function(y, mu, wt) -2 * wt * (log(ifelse(y == 
        0, 1, y/mu)) - (y - mu)/mu)
    aic <- function(y, n, mu, wt, dev) {
        n <- sum(wt)
        disp <- dev/n
        -2 * sum(dgamma(y, 1/disp, scale = mu * disp, log = TRUE) * 
            wt) + 2
    }
    initialize <- expression({
        if (any(y <= 0)) stop("non-positive values not allowed for the 'gamma' family")
        n <- rep.int(1, nobs)
        mustart <- y
    })
    simfun <- function(object, nsim) {
        wts <- object$prior.weights
        if (any(wts != 1)) 
            message("using weights as shape parameters")
        ftd <- fitted(object)
        shape <- MASS::gamma.shape(object)$alpha * wts
        rgamma(nsim * length(ftd), shape = shape, rate = shape/ftd)
    }
    structure(list(family = "Gamma", link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
        validmu = validmu, valideta = stats$valideta, simulate = simfun), 
        class = "family")
}


plot.stepfun <- function (x, xval, xlim, ylim = range(c(y, Fn.kn)), xlab = "x", 
    ylab = "f(x)", main = NULL, add = FALSE, verticals = TRUE, 
    do.points = (n < 1000), pch = par("pch"), col = par("col"), 
    col.points = col, cex.points = par("cex"), col.hor = col, 
    col.vert = col, lty = par("lty"), lwd = par("lwd"), ...) 
{
    if (!is.stepfun(x)) {
        if (is.numeric(x)) {
            sarg <- substitute(x)
            x <- ecdf(x)
            attr(x, "call") <- call("ecdf", sarg)
        }
        else stop("'plot.stepfun' called with wrong type of argument 'x'")
    }
    if (missing(main)) 
        main <- {
            cl <- attr(x, "call")
            deparse(if (!is.null(cl)) 
                cl
            else sys.call())
        }
    knF <- knots(x)
    xval <- if (missing(xval)) 
        knF
    else sort(xval)
    if (missing(xlim)) {
        rx <- range(xval)
        dr <- if (length(xval) > 1L) 
            max(0.08 * diff(rx), median(diff(xval)))
        else abs(xval)/16
        xlim <- rx + dr * c(-1, 1)
    }
    else dr <- diff(xlim)
    xval <- xval[xlim[1L] - dr <= xval & xval <= xlim[2L] + dr]
    ti <- c(xlim[1L] - dr, xval, xlim[2L] + dr)
    ti.l <- ti[-length(ti)]
    ti.r <- ti[-1L]
    y <- x(0.5 * (ti.l + ti.r))
    n <- length(y)
    Fn.kn <- x(xval)
    dev.hold()
    on.exit(dev.flush())
    if (add) 
        segments(ti.l, y, ti.r, y, col = col.hor, lty = lty, 
            lwd = lwd, ...)
    else {
        if (missing(ylim)) 
            ylim <- range(c(y, Fn.kn))
        plot(NA, NA, type = "n", xlim = xlim, ylim = ylim, xlab = xlab, 
            ylab = ylab, main = main, ...)
        segments(ti.l, y, ti.r, y, col = col.hor, lty = lty, 
            lwd = lwd)
    }
    if (do.points) 
        points(xval, Fn.kn, pch = pch, col = col.points, cex = cex.points)
    if (verticals) 
        segments(xval, y[-n], xval, y[-1L], col = col.vert, lty = lty, 
            lwd = lwd)
    invisible(list(t = ti, y = y))
}


termplot <- function (model, data = NULL, envir = environment(formula(model)), 
    partial.resid = FALSE, rug = FALSE, terms = NULL, se = FALSE, 
    xlabs = NULL, ylabs = NULL, main = NULL, col.term = 2, lwd.term = 1.5, 
    col.se = "orange", lty.se = 2, lwd.se = 1, col.res = "gray", 
    cex = 1, pch = par("pch"), col.smth = "darkred", lty.smth = 2, 
    span.smth = 2/3, ask = dev.interactive() && nb.fig < n.tms, 
    use.factor.levels = TRUE, smooth = NULL, ylim = "common", 
    plot = TRUE, transform.x = FALSE, ...) 
{
    which.terms <- terms
    terms <- if (is.null(terms)) 
        predict(model, type = "terms", se.fit = se)
    else predict(model, type = "terms", se.fit = se, terms = terms)
    n.tms <- ncol(tms <- as.matrix(if (se) 
        terms$fit
    else terms))
    transform.x <- rep_len(transform.x, n.tms)
    mf <- model.frame(model)
    if (is.null(data)) 
        data <- eval(model$call$data, envir)
    if (is.null(data)) 
        data <- mf
    use.rows <- if (NROW(tms) < NROW(data)) 
        match(rownames(tms), rownames(data))
    nmt <- colnames(tms)
    if (any(grepl(":", nmt, fixed = TRUE))) 
        warning("'model' appears to involve interactions: see the help page", 
            domain = NA, immediate. = TRUE)
    cn <- parse(text = nmt, keep.source = FALSE)
    if (!is.null(smooth)) 
        smooth <- match.fun(smooth)
    if (is.null(ylabs)) 
        ylabs <- paste("Partial for", nmt)
    if (is.null(main)) 
        main <- ""
    else if (is.logical(main)) 
        main <- if (main) 
            deparse(model$call, 500)
        else ""
    else if (!is.character(main)) 
        stop("'main' must be TRUE, FALSE, NULL or character (vector).")
    main <- rep_len(main, n.tms)
    pf <- envir
    carrier <- function(term, transform) {
        if (length(term) > 1L) {
            if (transform) 
                tms[, i]
            else carrier(term[[2L]], transform)
        }
        else eval(term, data, enclos = pf)
    }
    carrier.name <- function(term) {
        if (length(term) > 1L) 
            carrier.name(term[[2L]])
        else as.character(term)
    }
    in.mf <- nmt %in% names(mf)
    is.fac <- sapply(nmt, function(i) i %in% names(mf) && is.factor(mf[, 
        i]))
    if (!plot) {
        outlist <- vector("list", sum(in.mf))
        for (i in 1L:n.tms) {
            if (!in.mf[i]) 
                next
            if (is.fac[i]) {
                xx <- mf[, nmt[i]]
                if (!is.null(use.rows)) 
                  xx <- xx[use.rows]
                ww <- match(levels(xx), xx, nomatch = 0L)
            }
            else {
                xx <- carrier(cn[[i]], transform.x[i])
                if (!is.null(use.rows)) 
                  xx <- xx[use.rows]
                ww <- match(sort(unique(xx)), xx)
            }
            outlist[[i]] <- if (se) 
                data.frame(x = xx[ww], y = tms[ww, i], se = terms$se.fit[ww, 
                  i], row.names = NULL)
            else data.frame(x = xx[ww], y = tms[ww, i], row.names = NULL)
        }
        attr(outlist, "constant") <- attr(terms, "constant")
        if (se && is.null(attr(outlist, "constant"))) 
            attr(outlist, "constant") <- attr(terms$fit, "constant")
        names(outlist) <- sapply(cn, carrier.name)[in.mf]
        return(outlist)
    }
    if (!is.null(smooth)) 
        smooth <- match.fun(smooth)
    if (is.null(ylabs)) 
        ylabs <- paste("Partial for", nmt)
    if (is.null(main)) 
        main <- ""
    else if (is.logical(main)) 
        main <- if (main) 
            deparse(model$call, 500)
        else ""
    else if (!is.character(main)) 
        stop("'main' must be TRUE, FALSE, NULL or character (vector).")
    main <- rep_len(main, n.tms)
    if (is.null(xlabs)) {
        xlabs <- unlist(lapply(cn, carrier.name))
        if (any(transform.x)) 
            xlabs <- ifelse(transform.x, lapply(cn, deparse), 
                xlabs)
    }
    if (partial.resid || !is.null(smooth)) {
        pres <- residuals(model, "partial")
        if (!is.null(which.terms)) 
            pres <- pres[, which.terms, drop = FALSE]
    }
    se.lines <- function(x, iy, i, ff = 2) {
        tt <- ff * terms$se.fit[iy, i]
        lines(x, tms[iy, i] + tt, lty = lty.se, lwd = lwd.se, 
            col = col.se)
        lines(x, tms[iy, i] - tt, lty = lty.se, lwd = lwd.se, 
            col = col.se)
    }
    nb.fig <- prod(par("mfcol"))
    if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    ylims <- ylim
    if (identical(ylims, "common")) {
        ylims <- if (!se) 
            range(tms, na.rm = TRUE)
        else range(tms + 1.05 * 2 * terms$se.fit, tms - 1.05 * 
            2 * terms$se.fit, na.rm = TRUE)
        if (partial.resid) 
            ylims <- range(ylims, pres, na.rm = TRUE)
        if (rug) 
            ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
    }
    for (i in 1L:n.tms) {
        if (identical(ylim, "free")) {
            ylims <- range(tms[, i], na.rm = TRUE)
            if (se) 
                ylims <- range(ylims, tms[, i] + 1.05 * 2 * terms$se.fit[, 
                  i], tms[, i] - 1.05 * 2 * terms$se.fit[, i], 
                  na.rm = TRUE)
            if (partial.resid) 
                ylims <- range(ylims, pres[, i], na.rm = TRUE)
            if (rug) 
                ylims[1L] <- ylims[1L] - 0.07 * diff(ylims)
        }
        if (!in.mf[i]) 
            next
        if (is.fac[i]) {
            ff <- mf[, nmt[i]]
            if (!is.null(model$na.action)) 
                ff <- naresid(model$na.action, ff)
            ll <- levels(ff)
            xlims <- range(seq_along(ll)) + c(-0.5, 0.5)
            xx <- as.numeric(ff)
            if (rug) {
                xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
                xlims[2L] <- xlims[2L] + 0.03 * diff(xlims)
            }
            plot(1, 0, type = "n", xlab = xlabs[i], ylab = ylabs[i], 
                xlim = xlims, ylim = ylims, main = main[i], xaxt = "n", 
                ...)
            if (use.factor.levels) 
                axis(1, at = seq_along(ll), labels = ll, ...)
            else axis(1)
            for (j in seq_along(ll)) {
                ww <- which(ff == ll[j])[c(1, 1)]
                jf <- j + c(-0.4, 0.4)
                lines(jf, tms[ww, i], col = col.term, lwd = lwd.term, 
                  ...)
                if (se) 
                  se.lines(jf, iy = ww, i = i)
            }
        }
        else {
            xx <- carrier(cn[[i]], transform.x[i])
            if (!is.null(use.rows)) 
                xx <- xx[use.rows]
            xlims <- range(xx, na.rm = TRUE)
            if (rug) 
                xlims[1L] <- xlims[1L] - 0.07 * diff(xlims)
            oo <- order(xx)
            plot(xx[oo], tms[oo, i], type = "l", xlab = xlabs[i], 
                ylab = ylabs[i], xlim = xlims, ylim = ylims, 
                main = main[i], col = col.term, lwd = lwd.term, 
                ...)
            if (se) 
                se.lines(xx[oo], iy = oo, i = i)
        }
        if (partial.resid) {
            if (!is.fac[i] && !is.null(smooth)) {
                smooth(xx, pres[, i], lty = lty.smth, cex = cex, 
                  pch = pch, col = col.res, col.smooth = col.smth, 
                  span = span.smth)
            }
            else points(xx, pres[, i], cex = cex, pch = pch, 
                col = col.res)
        }
        if (rug) {
            n <- length(xx)
            lines(rep.int(jitter(xx), rep.int(3, n)), rep.int(ylims[1L] + 
                c(0, 0.05, NA) * diff(ylims), n))
            if (partial.resid) 
                lines(rep.int(xlims[1L] + c(0, 0.05, NA) * diff(xlims), 
                  n), rep.int(pres[, i], rep.int(3, n)))
        }
    }
    invisible(n.tms)
}


bartlett.test <- function (x, ...) 
UseMethod("bartlett.test")


oneway.test <- function (formula, data, subset, na.action, var.equal = FALSE) 
{
    if (missing(formula) || (length(formula) != 3L)) 
        stop("'formula' missing or incorrect")
    dp <- as.character(formula)
    if (length(dp) != 3L) 
        stop("a two-sided formula is required")
    DNAME <- paste(dp[[2L]], "and", dp[[3L]])
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$var.equal <- NULL
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    response <- attr(attr(mf, "terms"), "response")
    y <- mf[[response]]
    if (length(mf[-response]) > 1L) 
        g <- factor(do.call("interaction", mf[-response]))
    else g <- factor(mf[[-response]])
    k <- nlevels(g)
    if (k < 2L) 
        stop("not enough groups")
    n.i <- tapply(y, g, length)
    if (any(n.i < 2)) 
        stop("not enough observations")
    m.i <- tapply(y, g, mean)
    v.i <- tapply(y, g, var)
    w.i <- n.i/v.i
    sum.w.i <- sum(w.i)
    tmp <- sum((1 - w.i/sum.w.i)^2/(n.i - 1))/(k^2 - 1)
    METHOD <- "One-way analysis of means"
    if (var.equal) {
        n <- sum(n.i)
        STATISTIC <- ((sum(n.i * (m.i - mean(y))^2)/(k - 1))/(sum((n.i - 
            1) * v.i)/(n - k)))
        PARAMETER <- c(k - 1, n - k)
        PVAL <- pf(STATISTIC, k - 1, n - k, lower.tail = FALSE)
    }
    else {
        m <- sum(w.i * m.i)/sum.w.i
        STATISTIC <- sum(w.i * (m.i - m)^2)/((k - 1) * (1 + 2 * 
            (k - 2) * tmp))
        PARAMETER <- c(k - 1, 1/(3 * tmp))
        PVAL <- pf(STATISTIC, k - 1, 1/(3 * tmp), lower.tail = FALSE)
        METHOD <- paste(METHOD, "(not assuming equal variances)")
    }
    names(STATISTIC) <- "F"
    names(PARAMETER) <- c("num df", "denom df")
    RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
        p.value = PVAL, method = METHOD, data.name = DNAME)
    class(RVAL) <- "htest"
    RVAL
}


step <- function (object, scope, scale = 0, direction = c("both", "backward", 
    "forward"), trace = 1, keep = NULL, steps = 1000, k = 2, 
    ...) 
{
    mydeviance <- function(x, ...) {
        dev <- deviance(x)
        if (!is.null(dev)) 
            dev
        else extractAIC(x, k = 0)[2L]
    }
    cut.string <- function(string) {
        if (length(string) > 1L) 
            string[-1L] <- paste0("\n", string[-1L])
        string
    }
    re.arrange <- function(keep) {
        namr <- names(k1 <- keep[[1L]])
        namc <- names(keep)
        nc <- length(keep)
        nr <- length(k1)
        array(unlist(keep, recursive = FALSE), c(nr, nc), list(namr, 
            namc))
    }
    step.results <- function(models, fit, object, usingCp = FALSE) {
        change <- sapply(models, "[[", "change")
        rd <- sapply(models, "[[", "deviance")
        dd <- c(NA, abs(diff(rd)))
        rdf <- sapply(models, "[[", "df.resid")
        ddf <- c(NA, diff(rdf))
        AIC <- sapply(models, "[[", "AIC")
        heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
            "\nInitial Model:", deparse(formula(object)), "\nFinal Model:", 
            deparse(formula(fit)), "\n")
        aod <- data.frame(Step = I(change), Df = ddf, Deviance = dd, 
            `Resid. Df` = rdf, `Resid. Dev` = rd, AIC = AIC, 
            check.names = FALSE)
        if (usingCp) {
            cn <- colnames(aod)
            cn[cn == "AIC"] <- "Cp"
            colnames(aod) <- cn
        }
        attr(aod, "heading") <- heading
        fit$anova <- aod
        fit
    }
    Terms <- terms(object)
    object$call$formula <- object$formula <- Terms
    md <- missing(direction)
    direction <- match.arg(direction)
    backward <- direction == "both" | direction == "backward"
    forward <- direction == "both" | direction == "forward"
    if (missing(scope)) {
        fdrop <- numeric()
        fadd <- attr(Terms, "factors")
        if (md) 
            forward <- FALSE
    }
    else {
        if (is.list(scope)) {
            fdrop <- if (!is.null(fdrop <- scope$lower)) 
                attr(terms(update.formula(object, fdrop)), "factors")
            else numeric()
            fadd <- if (!is.null(fadd <- scope$upper)) 
                attr(terms(update.formula(object, fadd)), "factors")
        }
        else {
            fadd <- if (!is.null(fadd <- scope)) 
                attr(terms(update.formula(object, scope)), "factors")
            fdrop <- numeric()
        }
    }
    models <- vector("list", steps)
    if (!is.null(keep)) 
        keep.list <- vector("list", steps)
    n <- nobs(object, use.fallback = TRUE)
    fit <- object
    bAIC <- extractAIC(fit, scale, k = k, ...)
    edf <- bAIC[1L]
    bAIC <- bAIC[2L]
    if (is.na(bAIC)) 
        stop("AIC is not defined for this model, so 'step' cannot proceed")
    if (bAIC == -Inf) 
        stop("AIC is -infinity for this model, so 'step' cannot proceed")
    nm <- 1
    if (trace) {
        cat("Start:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(formula(fit))), 
            "\n\n", sep = "")
        flush.console()
    }
    models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
        edf, change = "", AIC = bAIC)
    if (!is.null(keep)) 
        keep.list[[nm]] <- keep(fit, bAIC)
    usingCp <- FALSE
    while (steps > 0) {
        steps <- steps - 1
        AIC <- bAIC
        ffac <- attr(Terms, "factors")
        scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
        aod <- NULL
        change <- NULL
        if (backward && length(scope$drop)) {
            aod <- drop1(fit, scope$drop, scale = scale, trace = trace, 
                k = k, ...)
            rn <- row.names(aod)
            row.names(aod) <- c(rn[1L], paste("-", rn[-1L]))
            if (any(aod$Df == 0, na.rm = TRUE)) {
                zdf <- aod$Df == 0 & !is.na(aod$Df)
                change <- rev(rownames(aod)[zdf])[1L]
            }
        }
        if (is.null(change)) {
            if (forward && length(scope$add)) {
                aodf <- add1(fit, scope$add, scale = scale, trace = trace, 
                  k = k, ...)
                rn <- row.names(aodf)
                row.names(aodf) <- c(rn[1L], paste("+", rn[-1L]))
                aod <- if (is.null(aod)) 
                  aodf
                else rbind(aod, aodf[-1, , drop = FALSE])
            }
            attr(aod, "heading") <- NULL
            nzdf <- if (!is.null(aod$Df)) 
                aod$Df != 0 | is.na(aod$Df)
            aod <- aod[nzdf, ]
            if (is.null(aod) || ncol(aod) == 0) 
                break
            nc <- match(c("Cp", "AIC"), names(aod))
            nc <- nc[!is.na(nc)][1L]
            o <- order(aod[, nc])
            if (trace) 
                print(aod[o, ])
            if (o[1L] == 1) 
                break
            change <- rownames(aod)[o[1L]]
        }
        usingCp <- match("Cp", names(aod), 0L) > 0L
        fit <- update(fit, paste("~ .", change), evaluate = FALSE)
        fit <- eval.parent(fit)
        nnew <- nobs(fit, use.fallback = TRUE)
        if (all(is.finite(c(n, nnew))) && nnew != n) 
            stop("number of rows in use has changed: remove missing values?")
        Terms <- terms(fit)
        bAIC <- extractAIC(fit, scale, k = k, ...)
        edf <- bAIC[1L]
        bAIC <- bAIC[2L]
        if (trace) {
            cat("\nStep:  AIC=", format(round(bAIC, 2)), "\n", 
                cut.string(deparse(formula(fit))), "\n\n", sep = "")
            flush.console()
        }
        if (bAIC >= AIC + 1e-07) 
            break
        nm <- nm + 1
        models[[nm]] <- list(deviance = mydeviance(fit), df.resid = n - 
            edf, change = change, AIC = bAIC)
        if (!is.null(keep)) 
            keep.list[[nm]] <- keep(fit, bAIC)
    }
    if (!is.null(keep)) 
        fit$keep <- re.arrange(keep.list[seq(nm)])
    step.results(models = models[seq(nm)], fit, object, usingCp)
}


qlogis <- function (p, location = 0, scale = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qlogis, p, location, scale, lower.tail, log.p)


df.kernel <- function (k) 
{
    2/sum(k[-k$m:k$m]^2)
}


quasi <- function (link = "identity", variance = "constant") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    if (linktemp %in% c("logit", "probit", "cloglog", "identity", 
        "inverse", "log", "1/mu^2", "sqrt")) 
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    }
    else {
        stats <- link
        linktemp <- if (!is.null(stats$name)) 
            stats$name
        else deparse(linktemp)
    }
    vtemp <- substitute(variance)
    if (!is.character(vtemp)) 
        vtemp <- deparse(vtemp)
    variance_nm <- vtemp
    switch(vtemp, constant = {
        varfun <- function(mu) rep.int(1, length(mu))
        dev.resids <- function(y, mu, wt) wt * ((y - mu)^2)
        validmu <- function(mu) TRUE
        initialize <- expression({
            n <- rep.int(1, nobs)
            mustart <- y
        })
    }, `mu(1-mu)` = {
        varfun <- function(mu) mu * (1 - mu)
        validmu <- function(mu) all(mu > 0) && all(mu < 1)
        dev.resids <- function(y, mu, wt) .Call(C_binomial_dev_resids, 
            y, mu, wt)
        initialize <- expression({
            n <- rep.int(1, nobs)
            mustart <- pmax(0.001, pmin(0.999, y))
        })
    }, mu = {
        varfun <- function(mu) mu
        validmu <- function(mu) all(mu > 0)
        dev.resids <- function(y, mu, wt) 2 * wt * (y * log(ifelse(y == 
            0, 1, y/mu)) - (y - mu))
        initialize <- expression({
            n <- rep.int(1, nobs)
            mustart <- y + 0.1 * (y == 0)
        })
    }, `mu^2` = {
        varfun <- function(mu) mu^2
        validmu <- function(mu) all(mu > 0)
        dev.resids <- function(y, mu, wt) pmax(-2 * wt * (log(ifelse(y == 
            0, 1, y)/mu) - (y - mu)/mu), 0)
        initialize <- expression({
            n <- rep.int(1, nobs)
            mustart <- y + 0.1 * (y == 0)
        })
    }, `mu^3` = {
        varfun <- function(mu) mu^3
        validmu <- function(mu) all(mu > 0)
        dev.resids <- function(y, mu, wt) wt * ((y - mu)^2)/(y * 
            mu^2)
        initialize <- expression({
            n <- rep.int(1, nobs)
            mustart <- y + 0.1 * (y == 0)
        })
    }, variance_nm <- NA)
    if (is.na(variance_nm)) {
        if (is.character(variance)) 
            stop(gettextf("'variance' \"%s\" is invalid: possible values are \"mu(1-mu)\", \"mu\", \"mu^2\", \"mu^3\" and \"constant\"", 
                variance_nm), domain = NA)
        varfun <- variance$varfun
        validmu <- variance$validmu
        dev.resids <- variance$dev.resids
        initialize <- variance$initialize
        variance_nm <- variance$name
    }
    aic <- function(y, n, mu, wt, dev) NA
    structure(list(family = "quasi", link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, variance = varfun, dev.resids = dev.resids, 
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
        validmu = validmu, valideta = stats$valideta, varfun = variance_nm), 
        class = "family")
}


coef <- function (object, ...) 
UseMethod("coef")


promax <- function (x, m = 4) 
{
    if (ncol(x) < 2) 
        return(x)
    dn <- dimnames(x)
    xx <- varimax(x)
    x <- xx$loadings
    Q <- x * abs(x)^(m - 1)
    U <- lm.fit(x, Q)$coefficients
    d <- diag(solve(t(U) %*% U))
    U <- U %*% diag(sqrt(d))
    dimnames(U) <- NULL
    z <- x %*% U
    U <- xx$rotmat %*% U
    dimnames(z) <- dn
    class(z) <- "loadings"
    list(loadings = z, rotmat = U)
}


factor.scope <- function (factor, scope) 
{
    drop <- scope$drop
    add <- scope$add
    if (length(factor) && !is.null(drop)) {
        nmdrop <- colnames(drop)
        facs <- factor
        if (length(drop)) {
            nmfac <- colnames(factor)
            nmfac0 <- sapply(strsplit(nmfac, ":", fixed = TRUE), 
                function(x) paste(sort(x), collapse = ":"))
            nmdrop0 <- sapply(strsplit(nmdrop, ":", fixed = TRUE), 
                function(x) paste(sort(x), collapse = ":"))
            where <- match(nmdrop0, nmfac0, 0L)
            if (any(!where)) 
                stop(sprintf(ngettext(sum(where == 0), "lower scope has term %s not included in model", 
                  "lower scope has terms %s not included in model"), 
                  paste(sQuote(nmdrop[where == 0]), collapse = ", ")), 
                  domain = NA)
            facs <- factor[, -where, drop = FALSE]
            nmdrop <- nmfac[-where]
        }
        else nmdrop <- colnames(factor)
        if (ncol(facs) > 1) {
            keep <- rep.int(TRUE, ncol(facs))
            f <- crossprod(facs > 0)
            for (i in seq(keep)) keep[i] <- max(f[i, -i]) != 
                f[i, i]
            nmdrop <- nmdrop[keep]
        }
    }
    else nmdrop <- character()
    if (!length(add)) 
        nmadd <- character()
    else {
        nmfac <- colnames(factor)
        nmadd <- colnames(add)
        if (!is.null(nmfac)) {
            nmfac0 <- sapply(strsplit(nmfac, ":", fixed = TRUE), 
                function(x) paste(sort(x), collapse = ":"))
            nmadd0 <- sapply(strsplit(nmadd, ":", fixed = TRUE), 
                function(x) paste(sort(x), collapse = ":"))
            where <- match(nmfac0, nmadd0, 0L)
            if (any(!where)) 
                stop(sprintf(ngettext(sum(where == 0), "upper scope has term %s not included in model", 
                  "upper scope has terms %s not included in model"), 
                  paste(sQuote(nmdrop[where == 0]), collapse = ", ")), 
                  domain = NA)
            nmadd <- nmadd[-where]
            add <- add[, -where, drop = FALSE]
        }
        if (ncol(add) > 1) {
            keep <- rep.int(TRUE, ncol(add))
            f <- crossprod(add > 0)
            for (i in seq(keep)) keep[-i] <- keep[-i] & (f[i, 
                -i] < f[i, i])
            nmadd <- nmadd[keep]
        }
    }
    list(drop = nmdrop, add = nmadd)
}


poisson <- function (link = "log") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    okLinks <- c("log", "identity", "sqrt")
    if (linktemp %in% okLinks) 
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    }
    else {
        if (inherits(link, "link-glm")) {
            stats <- link
            if (!is.null(stats$name)) 
                linktemp <- stats$name
        }
        else {
            stop(gettextf("link \"%s\" not available for poisson family; available links are %s", 
                linktemp, paste(sQuote(okLinks), collapse = ", ")), 
                domain = NA)
        }
    }
    variance <- function(mu) mu
    validmu <- function(mu) all(is.finite(mu)) && all(mu > 0)
    dev.resids <- function(y, mu, wt) {
        r <- mu * wt
        p <- which(y > 0)
        r[p] <- (wt * (y * log(y/mu) - (y - mu)))[p]
        2 * r
    }
    aic <- function(y, n, mu, wt, dev) -2 * sum(dpois(y, mu, 
        log = TRUE) * wt)
    initialize <- expression({
        if (any(y < 0)) stop("negative values not allowed for the 'Poisson' family")
        n <- rep.int(1, nobs)
        mustart <- y + 0.1
    })
    simfun <- function(object, nsim) {
        wts <- object$prior.weights
        if (any(wts != 1)) 
            warning("ignoring prior weights")
        ftd <- fitted(object)
        rpois(nsim * length(ftd), ftd)
    }
    structure(list(family = "poisson", link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
        validmu = validmu, valideta = stats$valideta, simulate = simfun), 
        class = "family")
}


embed <- function (x, dimension = 1) 
{
    if (is.matrix(x)) {
        n <- nrow(x)
        m <- ncol(x)
        if ((dimension < 1) | (dimension > n)) 
            stop("wrong embedding dimension")
        y <- matrix(0, n - dimension + 1L, dimension * m)
        for (i in seq_len(m)) y[, seq.int(i, by = m, length.out = dimension)] <- Recall(as.vector(x[, 
            i]), dimension)
        return(y)
    }
    else if (is.vector(x) || is.ts(x)) {
        n <- length(x)
        if ((dimension < 1) | (dimension > n)) 
            stop("wrong embedding dimension")
        m <- n - dimension + 1L
        data <- x[1L:m + rep.int(dimension:1L, rep.int(m, dimension)) - 
            1L]
        dim(data) <- c(m, dimension)
        return(data)
    }
    else stop("'x' is not a vector or matrix")
}


dcauchy <- function (x, location = 0, scale = 1, log = FALSE) 
.Call(C_dcauchy, x, location, scale, log)


na.action <- function (object, ...) 
UseMethod("na.action")


rect.hclust <- function (tree, k = NULL, which = NULL, x = NULL, h = NULL, border = 2, 
    cluster = NULL) 
{
    if (length(h) > 1L | length(k) > 1L) 
        stop("'k' and 'h' must be a scalar")
    if (!is.null(h)) {
        if (!is.null(k)) 
            stop("specify exactly one of 'k' and 'h'")
        k <- min(which(rev(tree$height) < h))
        k <- max(k, 2)
    }
    else if (is.null(k)) 
        stop("specify exactly one of 'k' and 'h'")
    if (k < 2 | k > length(tree$height)) 
        stop(gettextf("k must be between 2 and %d", length(tree$height)), 
            domain = NA)
    if (is.null(cluster)) 
        cluster <- cutree(tree, k = k)
    clustab <- table(cluster)[unique(cluster[tree$order])]
    m <- c(0, cumsum(clustab))
    if (!is.null(x)) {
        if (!is.null(which)) 
            stop("specify exactly one of 'which' and 'x'")
        which <- x
        for (n in seq_along(x)) which[n] <- max(which(m < x[n]))
    }
    else if (is.null(which)) 
        which <- 1L:k
    if (any(which > k)) 
        stop(gettextf("all elements of 'which' must be between 1 and %d", 
            k), domain = NA)
    border <- rep_len(border, length(which))
    retval <- list()
    for (n in seq_along(which)) {
        rect(m[which[n]] + 0.66, par("usr")[3L], m[which[n] + 
            1] + 0.33, mean(rev(tree$height)[(k - 1):k]), border = border[n])
        retval[[n]] <- which(cluster == as.integer(names(clustab)[which[n]]))
    }
    invisible(retval)
}


SSfpl <- function (input, A, B, xmid, scal) 
{
    .expr1 <- B - A
    .expr2 <- xmid - input
    .expr4 <- exp(.e2 <- .expr2/scal)
    .expr5 <- 1 + .expr4
    .value <- A + .expr1/.expr5
    .actualArgs <- as.list(match.call()[c("A", "B", "xmid", "scal")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .expr8 <- 1/.expr5
        .expr13 <- .expr5^2
        .grad <- array(0, c(length(.value), 4L), list(NULL, c("A", 
            "B", "xmid", "scal")))
        .grad[, "A"] <- 1 - .expr8
        .grad[, "B"] <- .expr8
        .grad[, "xmid"] <- -(xm <- .expr1 * .expr4/scal/.expr13)
        .grad[, "scal"] <- xm * .e2
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


bw.bcv <- function (x, nb = 1000L, lower = 0.1 * hmax, upper = hmax, tol = 0.1 * 
    lower) 
{
    if ((n <- length(x)) < 2L) 
        stop("need at least 2 data points")
    n <- as.integer(n)
    if (is.na(n)) 
        stop("invalid length(x)")
    if (!is.numeric(x)) 
        stop("invalid 'x'")
    nb <- as.integer(nb)
    if (is.na(nb) || nb <= 0L) 
        stop("invalid 'nb'")
    storage.mode(x) <- "double"
    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    Z <- bw_pair_cnts(x, nb, n > nb/2)
    d <- Z[[1L]]
    cnt <- Z[[2L]]
    fbcv <- function(h) .Call(C_bw_bcv, n, d, cnt, h)
    h <- optimize(fbcv, c(lower, upper), tol = tol)$minimum
    if (h < lower + tol | h > upper - tol) 
        warning("minimum occurred at one end of the range")
    h
}


as.hclust <- function (x, ...) 
UseMethod("as.hclust")


varimax <- function (x, normalize = TRUE, eps = 1e-05) 
{
    nc <- ncol(x)
    if (nc < 2) 
        return(x)
    if (normalize) {
        sc <- sqrt(drop(apply(x, 1L, function(x) sum(x^2))))
        x <- x/sc
    }
    p <- nrow(x)
    TT <- diag(nc)
    d <- 0
    for (i in 1L:1000L) {
        z <- x %*% TT
        B <- t(x) %*% (z^3 - z %*% diag(drop(rep(1, p) %*% z^2))/p)
        sB <- La.svd(B)
        TT <- sB$u %*% sB$vt
        dpast <- d
        d <- sum(sB$d)
        if (d < dpast * (1 + eps)) 
            break
    }
    z <- x %*% TT
    if (normalize) 
        z <- z * sc
    dimnames(z) <- dimnames(x)
    class(z) <- "loadings"
    list(loadings = z, rotmat = TT)
}


model.offset <- function (x) 
{
    offsets <- attr(attr(x, "terms"), "offset")
    if (length(offsets)) {
        ans <- x$"(offset)"
        if (is.null(ans)) 
            ans <- 0
        for (i in offsets) ans <- ans + x[[i]]
        ans
    }
    else ans <- x$"(offset)"
    if (!is.null(ans) && !is.numeric(ans)) 
        stop("'offset' must be numeric")
    ans
}


df.residual <- function (object, ...) 
UseMethod("df.residual")


nextn <- function (n, factors = c(2, 3, 5)) 
.Call(C_nextn, n, factors)


hclust <- function (d, method = "complete", members = NULL) 
{
    METHODS <- c("ward.D", "single", "complete", "average", "mcquitty", 
        "median", "centroid", "ward.D2")
    if (method == "ward") {
        message("The \"ward\" method has been renamed to \"ward.D\"; note new \"ward.D2\"")
        method <- "ward.D"
    }
    i.meth <- pmatch(method, METHODS)
    if (is.na(i.meth)) 
        stop("invalid clustering method", paste("", method))
    if (i.meth == -1) 
        stop("ambiguous clustering method", paste("", method))
    n <- as.integer(attr(d, "Size"))
    if (is.null(n)) 
        stop("invalid dissimilarities")
    if (is.na(n) || n > 65536L) 
        stop("size cannot be NA nor exceed 65536")
    if (n < 2) 
        stop("must have n >= 2 objects to cluster")
    len <- as.integer(n * (n - 1)/2)
    if (length(d) != len) 
        (if (length(d) < len) 
            stop
        else warning)("dissimilarities of improper length")
    if (is.null(members)) 
        members <- rep(1, n)
    else if (length(members) != n) 
        stop("invalid length of members")
    storage.mode(d) <- "double"
    hcl <- .Fortran(C_hclust, n = n, len = len, method = as.integer(i.meth), 
        ia = integer(n), ib = integer(n), crit = double(n), members = as.double(members), 
        nn = integer(n), disnn = double(n), flag = logical(n), 
        diss = d)
    hcass <- .Fortran(C_hcass2, n = n, ia = hcl$ia, ib = hcl$ib, 
        order = integer(n), iia = integer(n), iib = integer(n))
    structure(list(merge = cbind(hcass$iia[1L:(n - 1)], hcass$iib[1L:(n - 
        1)]), height = hcl$crit[1L:(n - 1)], order = hcass$order, 
        labels = attr(d, "Labels"), method = METHODS[i.meth], 
        call = match.call(), dist.method = attr(d, "method")), 
        class = "hclust")
}


smooth.spline <- function (x, y = NULL, w = NULL, df, spar = NULL, lambda = NULL, 
    cv = FALSE, all.knots = FALSE, nknots = .nknots.smspl, keep.data = TRUE, 
    df.offset = 0, penalty = 1, control.spar = list(), tol = 1e-06 * 
        IQR(x), keep.stuff = FALSE) 
{
    contr.sp <- list(low = -1.5, high = 1.5, tol = 1e-04, eps = 2e-08, 
        maxit = 500, trace = getOption("verbose"))
    contr.sp[names(control.spar)] <- control.spar
    ctrl.Num <- contr.sp[1:4]
    if (!all(vapply(ctrl.Num, is.numeric, NA)) || contr.sp$tol < 
        0 || contr.sp$eps <= 0 || contr.sp$maxit <= 0) 
        stop("invalid 'control.spar'")
    xy <- xy.coords(x, y, setLab = FALSE)
    y <- xy$y
    x <- xy$x
    if (!all(is.finite(c(x, y)))) 
        stop("missing or infinite values in inputs are not allowed")
    n <- length(x)
    if (is.na(n)) 
        stop("invalid number of points")
    no.wgts <- is.null(w)
    w <- if (no.wgts) 
        1
    else {
        if (n != length(w)) 
            stop("lengths of 'x' and 'w' must match")
        if (any(w < 0)) 
            stop("all weights should be non-negative")
        if (all(w == 0)) 
            stop("some weights should be positive")
        (w * sum(w > 0))/sum(w)
    }
    if (!is.finite(tol) || tol <= 0) 
        stop("'tol' must be strictly positive and finite")
    if (!match(keep.stuff, c(FALSE, TRUE))) 
        stop("invalid 'keep.stuff'")
    xx <- round((x - mean(x))/tol)
    nd <- !duplicated(xx)
    ux <- sort(x[nd])
    uxx <- sort(xx[nd])
    nx <- length(ux)
    if (nx <= 3L) 
        stop("need at least four unique 'x' values")
    if (nx == n) {
        ox <- TRUE
        tmp <- cbind(w, w * y, w * y^2)[order(x), ]
    }
    else {
        ox <- match(xx, uxx)
        tapply1 <- function(X, INDEX, FUN = NULL, ..., simplify = TRUE) {
            sapply(X = unname(split(X, INDEX)), FUN = FUN, ..., 
                simplify = simplify, USE.NAMES = FALSE)
        }
        tmp <- matrix(unlist(tapply1(seq_len(n), ox, if (length(w) == 
            1L) 
            function(i) c(length(i), sum(y[i]), sum(y[i]^2))
        else function(i) c(sum(w[i]), sum(w[i] * y[i]), sum(w[i] * 
            y[i]^2))), use.names = FALSE), ncol = 3, byrow = TRUE)
    }
    wbar <- tmp[, 1L]
    ybar <- tmp[, 2L]/ifelse(wbar > 0, wbar, 1)
    yssw <- sum(tmp[, 3L] - wbar * ybar^2)
    if (is.na(cv) && !missing(df)) 
        stop("'cv' must not be NA when 'df' is specified")
    CV <- !is.na(cv) && cv
    if (CV && nx < n) 
        warning("cross-validation with non-unique 'x' values seems doubtful")
    r.ux <- ux[nx] - ux[1L]
    xbar <- (ux - ux[1L])/r.ux
    if (is.numeric(all.knots)) {
        if (is.unsorted(all.knots, strictly = TRUE)) 
            stop("Numeric 'all.knots' must be strictly increasing")
        if (!missing(nknots) && !is.null(nknots)) 
            warning("'all.knots' is vector of knots; 'nknots' specification is disregarded")
        nknots <- length(all.knots)
        if (0 < all.knots[1] || all.knots[nknots] < 1) 
            stop("numeric 'all.knots' must cover [0,1] (= the transformed data-range)")
        knot <- c(rep(all.knots[1], 3), all.knots, rep(all.knots[nknots], 
            3))
    }
    else {
        if (all.knots) {
            if (!missing(nknots) && !is.null(nknots)) 
                warning("'all.knots' is TRUE; 'nknots' specification is disregarded")
            nknots <- nx
        }
        else if (is.null(nknots)) 
            nknots <- .nknots.smspl(nx)
        else {
            if (is.function(nknots)) 
                nknots <- nknots(nx)
            else if (!is.numeric(nknots)) 
                stop("'nknots' must be numeric (in {1,..,n})")
            if (nknots < 1) 
                stop("'nknots' must be at least 1")
            else if (nknots > nx) 
                stop("cannot use more inner knots than unique 'x' values")
        }
        knot <- c(rep(xbar[1], 3), if (all.knots) xbar else xbar[seq.int(1, 
            nx, length.out = nknots)], rep(xbar[nx], 3))
    }
    nk <- nknots + 2L
    spar.is.lambda <- !missing(lambda)
    if (spar.is.lambda <- !missing(lambda)) {
        if (!missing(spar)) 
            stop("must not specify both 'spar' and 'lambda'")
        ispar <- 1L
    }
    else ispar <- if (is.null(spar) || missing(spar)) {
        if (contr.sp$trace) 
            -1L
        else 0L
    }
    else 1L
    spar <- if (spar.is.lambda) 
        as.double(lambda)
    else if (ispar == 1L) 
        as.double(spar)
    else double(1)
    if (length(spar) != 1) 
        stop("'spar' must be of length 1")
    icrit <- if (is.na(cv)) 
        0L
    else if (cv) 
        2L
    else 1L
    dofoff <- df.offset
    if (!missing(df)) {
        if (df > 1 && df <= nx) {
            icrit <- 3L
            dofoff <- df
        }
        else warning("not using invalid df; must have 1 < df <= n := #{unique x} = ", 
            nx)
    }
    iparms <- c(icrit = icrit, ispar = ispar, iter = as.integer(contr.sp$maxit), 
        spar.is.lambda)
    ans.names <- c("coef", "ty", "lev", "spar", "parms", "crit", 
        "iparms", "ier", if (keep.stuff) "scratch")
    fit <- .Fortran(C_rbart, as.double(penalty), as.double(dofoff), 
        x = as.double(xbar), y = as.double(ybar), w = as.double(wbar), 
        ssw = as.double(yssw), as.integer(nx), as.double(knot), 
        as.integer(nk), coef = double(nk), ty = double(nx), lev = double(if (is.na(cv)) 1L else nx), 
        crit = double(1), iparms = iparms, spar = spar, parms = c(unlist(ctrl.Num), 
            ratio = -1), scratch = double((17L + 1L) * nk + 1L), 
        ld4 = 4L, ldnk = 1L, ier = integer(1L))[ans.names]
    if (is.na(cv)) 
        lev <- df <- NA
    else {
        lev <- fit$lev
        df <- sum(lev)
        if (is.na(df)) 
            stop("NA lev[]; probably smoothing parameter 'spar' way too large!")
    }
    if (fit$ier > 0L) {
        offKind <- if (spar.is.lambda) 
            "extreme"
        else if (sml <- fit$spar < 0.5) 
            "small"
        else "large"
        wtxt <- paste("smoothing parameter value too", offKind)
        if (spar.is.lambda || sml) {
            stop(wtxt)
        }
        else {
            fit$ty <- rep(mean(y), nx)
            df <- 1
            warning(wtxt, "\nsetting df = 1  __use with care!__")
        }
    }
    cv.crit <- if (is.na(cv)) 
        NA
    else {
        r <- y - fit$ty[ox]
        if (cv) {
            ww <- wbar
            ww[ww == 0] <- 1
            r <- r/(1 - (lev[ox] * w)/ww[ox])
            if (no.wgts) 
                mean(r^2)
            else weighted.mean(r^2, w)
        }
        else (if (no.wgts) 
            mean(r^2)
        else weighted.mean(r^2, w))/(1 - (df.offset + penalty * 
            df)/n)^2
    }
    structure(list(x = ux, y = fit$ty, w = wbar, yin = ybar, 
        tol = tol, data = if (keep.data) list(x = x, y = y, w = w), 
        no.weights = no.wgts, lev = lev, cv.crit = cv.crit, pen.crit = sum(wbar * 
            (ybar - fit$ty)^2), crit = fit$crit, df = df, spar = if (spar.is.lambda) NA else fit$spar, 
        ratio = if (spar.is.lambda) NA else fit$parms[["ratio"]], 
        lambda = fit$parms[["low"]], iparms = c(fit$iparms, errorI = if (fit$ier) fit$ier else NA), 
        auxM = if (keep.stuff) list(XWy = fit$scratch[seq_len(nk)], 
            XWX = fit$scratch[nk + seq_len(4 * nk)], Sigma = fit$scratch[5 * 
                nk + seq_len(4 * nk)], R = fit$scratch[9 * nk + 
                seq_len(4 * nk)]), fit = structure(list(knot = knot, 
            nk = nk, min = ux[1L], range = r.ux, coef = fit$coef), 
            class = "smooth.spline.fit"), call = match.call()), 
        class = "smooth.spline")
}


qtukey <- function (p, nmeans, df, nranges = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qtukey, p, nranges, nmeans, df, lower.tail, log.p)


bw.nrd0 <- function (x) 
{
    if (length(x) < 2L) 
        stop("need at least 2 data points")
    hi <- sd(x)
    if (!(lo <- min(hi, IQR(x)/1.34))) 
        (lo <- hi) || (lo <- abs(x[1L])) || (lo <- 1)
    0.9 * lo * length(x)^(-0.2)
}


ppr <- function (x, ...) 
UseMethod("ppr")


pacf <- function (x, lag.max, plot, na.action, ...) 
UseMethod("pacf")


power.prop.test <- function (n = NULL, p1 = NULL, p2 = NULL, sig.level = 0.05, power = NULL, 
    alternative = c("two.sided", "one.sided"), strict = FALSE, 
    tol = .Machine$double.eps^0.25) 
{
    if (sum(sapply(list(n, p1, p2, power, sig.level), is.null)) != 
        1) 
        stop("exactly one of 'n', 'p1', 'p2', 'power', and 'sig.level' must be NULL")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
        sig.level | sig.level > 1)) 
        stop("'sig.level' must be numeric in [0, 1]")
    alternative <- match.arg(alternative)
    tside <- switch(alternative, one.sided = 1, two.sided = 2)
    p.body <- if (strict && tside == 2) 
        quote({
            qu <- qnorm(sig.level/tside, lower.tail = FALSE)
            d <- abs(p1 - p2)
            q1 <- 1 - p1
            q2 <- 1 - p2
            pbar <- (p1 + p2)/2
            qbar <- 1 - pbar
            v1 <- p1 * q1
            v2 <- p2 * q2
            vbar <- pbar * qbar
            pnorm((sqrt(n) * d - qu * sqrt(2 * vbar))/sqrt(v1 + 
                v2)) + pnorm((sqrt(n) * d + qu * sqrt(2 * vbar))/sqrt(v1 + 
                v2), lower.tail = FALSE)
        })
    else quote(pnorm((sqrt(n) * abs(p1 - p2) - (qnorm(sig.level/tside, 
        lower.tail = FALSE) * sqrt((p1 + p2) * (1 - (p1 + p2)/2))))/sqrt(p1 * 
        (1 - p1) + p2 * (1 - p2))))
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(n)) 
        n <- uniroot(function(n) eval(p.body) - power, c(1, 1e+07), 
            tol = tol, extendInt = "upX")$root
    else if (is.null(p1)) 
        p1 <- uniroot(function(p1) eval(p.body) - power, c(0, 
            p2), tol = tol, extendInt = "yes")$root
    else if (is.null(p2)) 
        p2 <- uniroot(function(p2) eval(p.body) - power, c(p1, 
            1), tol = tol, extendInt = "yes")$root
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10), tol = tol, extendInt = "upX")$root
    else stop("internal error", domain = NA)
    NOTE <- "n is number in *each* group"
    METHOD <- "Two-sample comparison of proportions power calculation"
    structure(list(n = n, p1 = p1, p2 = p2, sig.level = sig.level, 
        power = power, alternative = alternative, note = NOTE, 
        method = METHOD), class = "power.htest")
}


pwilcox <- function (q, m, n, lower.tail = TRUE, log.p = FALSE) 
{
    on.exit(.External(C_wilcox_free))
    .Call(C_pwilcox, q, m, n, lower.tail, log.p)
}


bw.nrd <- function (x) 
{
    if (length(x) < 2L) 
        stop("need at least 2 data points")
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2L] - r[1L])/1.34
    1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
}


rsignrank <- function (nn, n) 
.Call(C_rsignrank, nn, n)


quantile <- function (x, ...) 
UseMethod("quantile")


stl <- function (x, s.window, s.degree = 0, t.window = NULL, t.degree = 1, 
    l.window = nextodd(period), l.degree = t.degree, s.jump = ceiling(s.window/10), 
    t.jump = ceiling(t.window/10), l.jump = ceiling(l.window/10), 
    robust = FALSE, inner = if (robust) 1 else 2, outer = if (robust) 15 else 0, 
    na.action = na.fail) 
{
    nextodd <- function(x) {
        x <- round(x)
        if (x%%2 == 0) 
            x <- x + 1
        as.integer(x)
    }
    deg.check <- function(deg) {
        degname <- deparse(substitute(deg))
        deg <- as.integer(deg)
        if (deg < 0 || deg > 1) 
            stop(gettextf("%s must be 0 or 1", degname), domain = NA)
        deg
    }
    x <- na.action(as.ts(x))
    if (is.matrix(x)) 
        stop("only univariate series are allowed")
    n <- as.integer(length(x))
    if (is.na(n)) 
        stop("invalid length(x)")
    period <- frequency(x)
    if (period < 2 || n <= 2 * period) 
        stop("series is not periodic or has less than two periods")
    periodic <- FALSE
    if (is.character(s.window)) {
        if (is.na(pmatch(s.window, "periodic"))) 
            stop("unknown string value for s.window")
        else {
            periodic <- TRUE
            s.window <- 10 * n + 1
            s.degree <- 0
        }
    }
    s.degree <- deg.check(s.degree)
    t.degree <- deg.check(t.degree)
    l.degree <- deg.check(l.degree)
    if (is.null(t.window)) 
        t.window <- nextodd(ceiling(1.5 * period/(1 - 1.5/s.window)))
    storage.mode(x) <- "double"
    z <- .Fortran(C_stl, x, n, as.integer(period), as.integer(s.window), 
        as.integer(t.window), as.integer(l.window), s.degree, 
        t.degree, l.degree, nsjump = as.integer(s.jump), ntjump = as.integer(t.jump), 
        nljump = as.integer(l.jump), ni = as.integer(inner), 
        no = as.integer(outer), weights = double(n), seasonal = double(n), 
        trend = double(n), double((n + 2 * period) * 5))
    if (periodic) {
        which.cycle <- cycle(x)
        z$seasonal <- tapply(z$seasonal, which.cycle, mean)[which.cycle]
    }
    remainder <- as.vector(x) - z$seasonal - z$trend
    y <- cbind(seasonal = z$seasonal, trend = z$trend, remainder = remainder)
    res <- list(time.series = ts(y, start = start(x), frequency = period), 
        weights = z$weights, call = match.call(), win = c(s = s.window, 
            t = t.window, l = l.window), deg = c(s = s.degree, 
            t = t.degree, l = l.degree), jump = c(s = s.jump, 
            t = t.jump, l = l.jump), inner = z$ni, outer = z$no)
    class(res) <- "stl"
    res
}


AIC <- function (object, ..., k = 2) 
UseMethod("AIC")


end <- function (x, ...) 
UseMethod("end")


sortedXyData <- function (x, y, data) 
UseMethod("sortedXyData")


lowess <- function (x, y = NULL, f = 2/3, iter = 3L, delta = 0.01 * diff(range(x))) 
{
    xy <- xy.coords(x, y, setLab = FALSE)
    o <- order(xy$x)
    x <- as.double(xy$x[o])
    list(x = x, y = .Call(C_lowess, x, as.double(xy$y[o]), f, 
        iter, delta))
}


lm.wfit <- function (x, y, w, offset = NULL, method = "qr", tol = 1e-07, 
    singular.ok = TRUE, ...) 
{
    if (is.null(n <- nrow(x))) 
        stop("'x' must be a matrix")
    if (n == 0) 
        stop("0 (non-NA) cases")
    ny <- NCOL(y)
    if (is.matrix(y) && ny == 1L) 
        y <- drop(y)
    if (!is.null(offset)) 
        y <- y - offset
    if (NROW(y) != n | length(w) != n) 
        stop("incompatible dimensions")
    if (any(w < 0 | is.na(w))) 
        stop("missing or negative weights not allowed")
    if (method != "qr") 
        warning(gettextf("method = '%s' is not supported. Using 'qr'", 
            method), domain = NA)
    chkDots(...)
    x.asgn <- attr(x, "assign")
    zero.weights <- any(w == 0)
    if (zero.weights) {
        save.r <- y
        save.f <- y
        save.w <- w
        ok <- w != 0
        nok <- !ok
        w <- w[ok]
        x0 <- x[!ok, , drop = FALSE]
        x <- x[ok, , drop = FALSE]
        n <- nrow(x)
        y0 <- if (ny > 1L) 
            y[!ok, , drop = FALSE]
        else y[!ok]
        y <- if (ny > 1L) 
            y[ok, , drop = FALSE]
        else y[ok]
    }
    p <- ncol(x)
    if (p == 0) {
        return(list(coefficients = numeric(), residuals = y, 
            fitted.values = 0 * y, weights = w, rank = 0L, df.residual = length(y)))
    }
    if (n == 0) {
        return(list(coefficients = rep(NA_real_, p), residuals = y, 
            fitted.values = 0 * y, weights = w, rank = 0L, df.residual = 0L))
    }
    wts <- sqrt(w)
    z <- .Call(C_Cdqrls, x * wts, y * wts, tol, FALSE)
    if (!singular.ok && z$rank < p) 
        stop("singular fit encountered")
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- seq_len(z$rank)
    dn <- colnames(x)
    if (is.null(dn)) 
        dn <- paste0("x", 1L:p)
    nmeffects <- c(dn[pivot[r1]], rep.int("", n - z$rank))
    r2 <- if (z$rank < p) 
        (z$rank + 1L):p
    else integer()
    if (is.matrix(y)) {
        coef[r2, ] <- NA
        if (z$pivoted) 
            coef[pivot, ] <- coef
        dimnames(coef) <- list(dn, colnames(y))
        dimnames(z$effects) <- list(nmeffects, colnames(y))
    }
    else {
        coef[r2] <- NA
        if (z$pivoted) 
            coef[pivot] <- coef
        names(coef) <- dn
        names(z$effects) <- nmeffects
    }
    z$coefficients <- coef
    z$residuals <- z$residuals/wts
    z$fitted.values <- y - z$residuals
    z$weights <- w
    if (zero.weights) {
        coef[is.na(coef)] <- 0
        f0 <- x0 %*% coef
        if (ny > 1) {
            save.r[ok, ] <- z$residuals
            save.r[nok, ] <- y0 - f0
            save.f[ok, ] <- z$fitted.values
            save.f[nok, ] <- f0
        }
        else {
            save.r[ok] <- z$residuals
            save.r[nok] <- y0 - f0
            save.f[ok] <- z$fitted.values
            save.f[nok] <- f0
        }
        z$residuals <- save.r
        z$fitted.values <- save.f
        z$weights <- save.w
    }
    if (!is.null(offset)) 
        z$fitted.values <- z$fitted.values + offset
    if (z$pivoted) 
        colnames(z$qr) <- colnames(x)[z$pivot]
    qr <- z[c("qr", "qraux", "pivot", "tol", "rank")]
    c(z[c("coefficients", "residuals", "fitted.values", "effects", 
        "weights", "rank")], list(assign = x.asgn, qr = structure(qr, 
        class = "qr"), df.residual = n - z$rank))
}


pf <- function (q, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_pf, q, df1, df2, lower.tail, log.p)
    else .Call(C_pnf, q, df1, df2, ncp, lower.tail, log.p)
}


princomp <- function (x, ...) 
UseMethod("princomp")


terms <- function (x, ...) 
UseMethod("terms")


lm.influence <- function (model, do.coef = TRUE) 
{
    wt.res <- weighted.residuals(model)
    e <- na.omit(wt.res)
    if (model$rank == 0) {
        n <- length(wt.res)
        sigma <- sqrt(deviance(model)/df.residual(model))
        res <- list(hat = rep(0, n), coefficients = matrix(0, 
            n, 0), sigma = rep(sigma, n), wt.res = e)
    }
    else {
        e[abs(e) < 100 * .Machine$double.eps * median(abs(e))] <- 0
        mqr <- qr.lm(model)
        n <- as.integer(nrow(mqr$qr))
        if (is.na(n)) 
            stop("invalid model QR matrix")
        if (NROW(e) != n) 
            stop("non-NA residual length does not match cases used in fitting")
        do.coef <- as.logical(do.coef)
        tol <- 10 * .Machine$double.eps
        res <- .Call(C_influence, mqr, do.coef, e, tol)
        if (!is.null(model$na.action)) {
            hat <- naresid(model$na.action, res$hat)
            hat[is.na(hat)] <- 0
            res$hat <- hat
            if (do.coef) {
                coefficients <- naresid(model$na.action, res$coefficients)
                coefficients[is.na(coefficients)] <- 0
                res$coefficients <- coefficients
            }
            sigma <- naresid(model$na.action, res$sigma)
            sigma[is.na(sigma)] <- sqrt(deviance(model)/df.residual(model))
            res$sigma <- sigma
        }
    }
    res$wt.res <- naresid(model$na.action, res$wt.res)
    res$hat[res$hat > 1 - 10 * .Machine$double.eps] <- 1
    names(res$hat) <- names(res$sigma) <- names(res$wt.res)
    if (do.coef) {
        rownames(res$coefficients) <- names(res$wt.res)
        colnames(res$coefficients) <- names(coef(model))[!is.na(coef(model))]
    }
    res
}


model.frame <- function (formula, ...) 
UseMethod("model.frame")


qwilcox <- function (p, m, n, lower.tail = TRUE, log.p = FALSE) 
{
    on.exit(.External(C_wilcox_free))
    .Call(C_qwilcox, p, m, n, lower.tail, log.p)
}


stat.anova <- function (table, test = c("Rao", "LRT", "Chisq", "F", "Cp"), 
    scale, df.scale, n) 
{
    test <- match.arg(test)
    dev.col <- match("Deviance", colnames(table))
    if (test == "Rao") 
        dev.col <- match("Rao", colnames(table))
    if (is.na(dev.col)) 
        dev.col <- match("Sum of Sq", colnames(table))
    switch(test, Rao = , LRT = , Chisq = {
        dfs <- table[, "Df"]
        vals <- table[, dev.col]/scale * sign(dfs)
        vals[dfs %in% 0] <- NA
        vals[!is.na(vals) & vals < 0] <- NA
        cbind(table, `Pr(>Chi)` = pchisq(vals, abs(dfs), lower.tail = FALSE))
    }, F = {
        dfs <- table[, "Df"]
        Fvalue <- (table[, dev.col]/dfs)/scale
        Fvalue[dfs %in% 0] <- NA
        Fvalue[!is.na(Fvalue) & Fvalue < 0] <- NA
        cbind(table, F = Fvalue, `Pr(>F)` = pf(Fvalue, abs(dfs), 
            df.scale, lower.tail = FALSE))
    }, Cp = {
        if ("RSS" %in% names(table)) {
            cbind(table, Cp = table[, "RSS"] + 2 * scale * (n - 
                table[, "Res.Df"]))
        } else {
            cbind(table, Cp = table[, "Resid. Dev"] + 2 * scale * 
                (n - table[, "Resid. Df"]))
        }
    })
}


qchisq <- function (p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_qchisq, p, df, lower.tail, log.p)
    else .Call(C_qnchisq, p, df, ncp, lower.tail, log.p)
}


update.default <- function (object, formula., ..., evaluate = TRUE) 
{
    if (is.null(call <- getCall(object))) 
        stop("need an object with call component")
    extras <- match.call(expand.dots = FALSE)$...
    if (!missing(formula.)) 
        call$formula <- update.formula(formula(object), formula.)
    if (length(extras)) {
        existing <- !is.na(match(names(extras), names(call)))
        for (a in names(extras)[existing]) call[[a]] <- extras[[a]]
        if (any(!existing)) {
            call <- c(as.list(call), extras[!existing])
            call <- as.call(call)
        }
    }
    if (evaluate) 
        eval(call, parent.frame())
    else call
}


model.response <- function (data, type = "any") 
{
    if (attr(attr(data, "terms"), "response")) {
        if (is.list(data) | is.data.frame(data)) {
            v <- data[[1L]]
            if (type == "numeric" && is.factor(v)) {
                warning("using type = \"numeric\" with a factor response will be ignored")
            }
            else if (type == "numeric" | type == "double") 
                storage.mode(v) <- "double"
            else if (type != "any") 
                stop("invalid response type")
            if (is.matrix(v) && ncol(v) == 1L) 
                dim(v) <- NULL
            rows <- attr(data, "row.names")
            if (nrows <- length(rows)) {
                if (length(v) == nrows) 
                  names(v) <- rows
                else if (length(dd <- dim(v)) == 2L) 
                  if (dd[1L] == nrows && !length((dn <- dimnames(v))[[1L]])) 
                    dimnames(v) <- list(rows, dn[[2L]])
            }
            return(v)
        }
        else stop("invalid 'data' argument")
    }
    else return(NULL)
}


drop.scope <- function (terms1, terms2) 
{
    terms1 <- terms(terms1)
    f2 <- if (missing(terms2)) 
        numeric()
    else attr(terms(terms2), "factors")
    factor.scope(attr(terms1, "factors"), list(drop = f2))$drop
}


residuals <- function (object, ...) 
UseMethod("residuals")


median <- function (x, na.rm = FALSE, ...) 
UseMethod("median")


cycle <- function (x, ...) 
UseMethod("cycle")


constrOptim <- function (theta, f, grad, ui, ci, mu = 1e-04, control = list(), 
    method = if (is.null(grad)) "Nelder-Mead" else "BFGS", outer.iterations = 100, 
    outer.eps = 1e-05, ..., hessian = FALSE) 
{
    if (!is.null(control$fnscale) && control$fnscale < 0) 
        mu <- -mu
    R <- function(theta, theta.old, ...) {
        ui.theta <- ui %*% theta
        gi <- ui.theta - ci
        if (any(gi < 0)) 
            return(NaN)
        gi.old <- ui %*% theta.old - ci
        bar <- sum(gi.old * log(gi) - ui.theta)
        if (!is.finite(bar)) 
            bar <- -Inf
        f(theta, ...) - mu * bar
    }
    dR <- function(theta, theta.old, ...) {
        ui.theta <- ui %*% theta
        gi <- drop(ui.theta - ci)
        gi.old <- drop(ui %*% theta.old - ci)
        dbar <- colSums(ui * gi.old/gi - ui)
        grad(theta, ...) - mu * dbar
    }
    if (any(ui %*% theta - ci <= 0)) 
        stop("initial value is not in the interior of the feasible region")
    obj <- f(theta, ...)
    r <- R(theta, theta, ...)
    fun <- function(theta, ...) R(theta, theta.old, ...)
    gradient <- if (method == "SANN") {
        if (missing(grad)) 
            NULL
        else grad
    }
    else function(theta, ...) dR(theta, theta.old, ...)
    totCounts <- 0
    s.mu <- sign(mu)
    for (i in seq_len(outer.iterations)) {
        obj.old <- obj
        r.old <- r
        theta.old <- theta
        a <- optim(theta.old, fun, gradient, control = control, 
            method = method, hessian = hessian, ...)
        r <- a$value
        if (is.finite(r) && is.finite(r.old) && abs(r - r.old) < 
            (0.001 + abs(r)) * outer.eps) 
            break
        theta <- a$par
        totCounts <- totCounts + a$counts
        obj <- f(theta, ...)
        if (s.mu * obj > s.mu * obj.old) 
            break
    }
    if (i == outer.iterations) {
        a$convergence <- 7
        a$message <- gettext("Barrier algorithm ran out of iterations and did not converge")
    }
    if (mu > 0 && obj > obj.old) {
        a$convergence <- 11
        a$message <- gettextf("Objective function increased at outer iteration %d", 
            i)
    }
    if (mu < 0 && obj < obj.old) {
        a$convergence <- 11
        a$message <- gettextf("Objective function decreased at outer iteration %d", 
            i)
    }
    a$outer.iterations <- i
    a$counts <- totCounts
    a$barrier.value <- a$value
    a$value <- f(a$par, ...)
    a$barrier.value <- a$barrier.value - a$value
    a
}


pt <- function (q, df, ncp, lower.tail = TRUE, log.p = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_pt, q, df, lower.tail, log.p)
    else .Call(C_pnt, q, df, ncp, lower.tail, log.p)
}


deriv3 <- function (expr, ...) 
UseMethod("deriv3")


variable.names <- function (object, ...) 
UseMethod("variable.names")


qf <- function (p, df1, df2, ncp, lower.tail = TRUE, log.p = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_qf, p, df1, df2, lower.tail, log.p)
    else .Call(C_qnf, p, df1, df2, ncp, lower.tail, log.p)
}


summary.aov <- function (object, intercept = FALSE, split, expand.split = TRUE, 
    keep.zero.df = TRUE, ...) 
{
    splitInteractions <- function(split, factors, names, asgn, 
        df.names) {
        ns <- names(split)
        for (i in unique(asgn)) {
            if (i == 0 || names[i + 1L] %in% ns) 
                next
            f <- rownames(factors)[factors[, i] > 0]
            sp <- f %in% ns
            if (any(sp)) {
                if (sum(sp) > 1L) {
                  old <- split[f[sp]]
                  nn <- setNames(nm = f[sp])
                  marg <- lapply(nn, function(x) df.names[asgn == 
                    (match(x, names) - 1L)])
                  term.coefs <- strsplit(df.names[asgn == i], 
                    ":", fixed = TRUE)
                  ttc <- sapply(term.coefs, function(x) x[sp])
                  rownames(ttc) <- nn
                  splitnames <- setNames(nm = apply(expand.grid(lapply(old, 
                    names)), 1L, function(x) paste(x, collapse = ".")))
                  tmp <- sapply(nn, function(i) names(old[[i]])[match(ttc[i, 
                    ], marg[[i]])])
                  tmp <- apply(tmp, 1L, function(x) paste(x, 
                    collapse = "."))
                  new <- lapply(splitnames, function(x) match(x, 
                    tmp))
                  split[[names[i + 1L]]] <- new[sapply(new, function(x) length(x) > 
                    0L)]
                }
                else {
                  old <- split[[f[sp]]]
                  marg.coefs <- df.names[asgn == (match(f[sp], 
                    names) - 1L)]
                  term.coefs <- strsplit(df.names[asgn == i], 
                    ":", fixed = TRUE)
                  ttc <- sapply(term.coefs, function(x) x[sp])
                  new <- lapply(old, function(x) seq_along(ttc)[ttc %in% 
                    marg.coefs[x]])
                  split[[names[i + 1L]]] <- new
                }
            }
        }
        split
    }
    asgn <- object$assign[object$qr$pivot[1L:object$rank]]
    uasgn <- unique(asgn)
    nterms <- length(uasgn)
    effects <- object$effects
    if (!is.null(effects)) 
        effects <- as.matrix(effects)[seq_along(asgn), , drop = FALSE]
    rdf <- object$df.residual
    nmeffect <- c("(Intercept)", attr(object$terms, "term.labels"))
    coef <- as.matrix(object$coefficients)
    resid <- as.matrix(object$residuals)
    wt <- object$weights
    if (!is.null(wt)) 
        resid <- resid * sqrt(wt)
    nresp <- NCOL(resid)
    ans <- vector("list", nresp)
    if (nresp > 1) {
        names(ans) <- character(nresp)
        for (y in 1L:nresp) {
            cn <- colnames(resid)[y]
            if (is.null(cn) || cn == "") 
                cn <- y
            names(ans)[y] <- paste(" Response", cn)
        }
    }
    if (!is.null(effects) && !missing(split)) {
        ns <- names(split)
        if (!is.null(Terms <- object$terms)) {
            if (!is.list(split)) 
                stop("the 'split' argument must be a list")
            if (!all(ns %in% nmeffect)) {
                na <- sum(!ns %in% nmeffect)
                stop(sprintf(ngettext(na, "unknown name %s in the 'split' list", 
                  "unknown names %s in the 'split' list"), paste(sQuote(ns[na]), 
                  collapse = ", ")), domain = NA)
            }
        }
        if (expand.split) {
            df.names <- names(coef(object))
            split <- splitInteractions(split, attr(Terms, "factors"), 
                nmeffect, asgn, df.names)
            ns <- names(split)
        }
    }
    for (y in 1L:nresp) {
        if (is.null(effects)) {
            nterms <- 0L
            df <- ss <- ms <- numeric()
            nmrows <- character()
        }
        else {
            df <- ss <- numeric()
            nmrows <- character()
            for (i in seq(nterms)) {
                ai <- (asgn == uasgn[i])
                df <- c(df, sum(ai))
                ss <- c(ss, sum(effects[ai, y]^2))
                nmi <- nmeffect[1 + uasgn[i]]
                nmrows <- c(nmrows, nmi)
                if (!missing(split) && !is.na(int <- match(nmi, 
                  ns))) {
                  df <- c(df, lengths(split[[int]]))
                  if (is.null(nms <- names(split[[int]]))) 
                    nms <- paste0("C", seq_along(split[[int]]))
                  ss <- c(ss, unlist(lapply(split[[int]], function(i, 
                    e) sum(e[i]^2), effects[ai, y])))
                  nmrows <- c(nmrows, paste0("  ", nmi, ": ", 
                    nms))
                }
            }
        }
        if (rdf > 0L) {
            df <- c(df, rdf)
            ss <- c(ss, sum(resid[, y]^2))
            nmrows <- c(nmrows, "Residuals")
        }
        nt <- length(df)
        ms <- ifelse(df > 0L, ss/df, NA)
        x <- list(Df = df, `Sum Sq` = ss, `Mean Sq` = ms)
        if (rdf > 0L) {
            TT <- ms/ms[nt]
            TP <- pf(TT, df, rdf, lower.tail = FALSE)
            TT[nt] <- TP[nt] <- NA
            x$"F value" <- TT
            x$"Pr(>F)" <- TP
        }
        class(x) <- c("anova", "data.frame")
        attr(x, "row.names") <- format(nmrows)
        if (!keep.zero.df) 
            x <- x[df > 0L, ]
        pm <- pmatch("(Intercept)", row.names(x), 0L)
        if (!intercept && pm > 0L) 
            x <- x[-pm, ]
        ans[[y]] <- x
    }
    class(ans) <- c("summary.aov", "listof")
    attr(ans, "na.action") <- object$na.action
    ans
}


update <- function (object, ...) 
UseMethod("update")


rwilcox <- function (nn, m, n) 
.Call(C_rwilcox, nn, m, n)


pairwise.table <- function (compare.levels, level.names, p.adjust.method) 
{
    ix <- setNames(seq_along(level.names), level.names)
    pp <- outer(ix[-1L], ix[-length(ix)], function(ivec, jvec) sapply(seq_along(ivec), 
        function(k) {
            i <- ivec[k]
            j <- jvec[k]
            if (i > j) 
                compare.levels(i, j)
            else NA
        }))
    pp[lower.tri(pp, TRUE)] <- p.adjust(pp[lower.tri(pp, TRUE)], 
        p.adjust.method)
    pp
}


model.matrix.lm <- function (object, ...) 
{
    if (n_match <- match("x", names(object), 0L)) 
        object[[n_match]]
    else {
        data <- model.frame(object, xlev = object$xlevels, ...)
        NextMethod("model.matrix", data = data, contrasts.arg = object$contrasts)
    }
}


expand.model.frame <- function (model, extras, envir = environment(formula(model)), 
    na.expand = FALSE) 
{
    f <- formula(model)
    data <- eval(model$call$data, envir)
    ff <- foo ~ bar + baz
    if (is.call(extras)) 
        gg <- extras
    else gg <- parse(text = paste("~", paste(extras, collapse = "+")))[[1L]]
    ff[[2L]] <- f[[2L]]
    ff[[3L]][[2L]] <- f[[3L]]
    ff[[3L]][[3L]] <- gg[[2L]]
    if (!na.expand) {
        naa <- model$call$na.action
        subset <- model$call$subset
        rval <- eval(call("model.frame", ff, data = data, subset = subset, 
            na.action = naa), envir)
    }
    else {
        subset <- model$call$subset
        rval <- eval(call("model.frame", ff, data = data, subset = subset, 
            na.action = I), envir)
        oldmf <- model.frame(model)
        keep <- match(rownames(oldmf), rownames(rval))
        rval <- rval[keep, ]
        class(rval) <- "data.frame"
    }
    return(rval)
}


acf <- function (x, lag.max = NULL, type = c("correlation", "covariance", 
    "partial"), plot = TRUE, na.action = na.fail, demean = TRUE, 
    ...) 
{
    type <- match.arg(type)
    if (type == "partial") {
        m <- match.call()
        m[[1L]] <- quote(stats::pacf)
        m$type <- NULL
        return(eval(m, parent.frame()))
    }
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    x.freq <- frequency(x)
    x <- as.matrix(x)
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    sampleT <- as.integer(nrow(x))
    nser <- as.integer(ncol(x))
    if (is.na(sampleT) || is.na(nser)) 
        stop("'sampleT' and 'nser' must be integer")
    if (is.null(lag.max)) 
        lag.max <- floor(10 * (log10(sampleT) - log10(nser)))
    lag.max <- as.integer(min(lag.max, sampleT - 1L))
    if (is.na(lag.max) || lag.max < 0) 
        stop("'lag.max' must be at least 0")
    if (demean) 
        x <- sweep(x, 2, colMeans(x, na.rm = TRUE), check.margin = FALSE)
    lag <- matrix(1, nser, nser)
    lag[lower.tri(lag)] <- -1
    acf <- .Call(C_acf, x, lag.max, type == "correlation")
    lag <- outer(0:lag.max, lag/x.freq)
    acf.out <- structure(list(acf = acf, type = type, n.used = sampleT, 
        lag = lag, series = series, snames = colnames(x)), class = "acf")
    if (plot) {
        plot.acf(acf.out, ...)
        invisible(acf.out)
    }
    else acf.out
}


proj <- function (object, ...) 
UseMethod("proj")


bw.SJ <- function (x, nb = 1000L, lower = 0.1 * hmax, upper = hmax, method = c("ste", 
    "dpi"), tol = 0.1 * lower) 
{
    if ((n <- length(x)) < 2L) 
        stop("need at least 2 data points")
    n <- as.integer(n)
    if (is.na(n)) 
        stop("invalid length(x)")
    if (!is.numeric(x)) 
        stop("invalid 'x'")
    nb <- as.integer(nb)
    if (is.na(nb) || nb <= 0L) 
        stop("invalid 'nb'")
    storage.mode(x) <- "double"
    method <- match.arg(method)
    SDh <- function(h) .Call(C_bw_phi4, n, d, cnt, h)
    TDh <- function(h) .Call(C_bw_phi6, n, d, cnt, h)
    Z <- bw_pair_cnts(x, nb, n > nb/2)
    d <- Z[[1L]]
    cnt <- Z[[2L]]
    scale <- min(sd(x), IQR(x)/1.349)
    a <- 1.24 * scale * n^(-1/7)
    b <- 1.23 * scale * n^(-1/9)
    c1 <- 1/(2 * sqrt(pi) * n)
    TD <- -TDh(b)
    if (!is.finite(TD) || TD <= 0) 
        stop("sample is too sparse to find TD", domain = NA)
    if (method == "dpi") 
        res <- (c1/SDh((2.394/(n * TD))^(1/7)))^(1/5)
    else {
        if (bnd.Miss <- missing(lower) || missing(upper)) {
            hmax <- 1.144 * scale * n^(-1/5)
        }
        alph2 <- 1.357 * (SDh(a)/TD)^(1/7)
        if (!is.finite(alph2)) 
            stop("sample is too sparse to find alph2", domain = NA)
        itry <- 1L
        fSD <- function(h) (c1/SDh(alph2 * h^(5/7)))^(1/5) - 
            h
        while (fSD(lower) * fSD(upper) > 0) {
            if (itry > 99L || !bnd.Miss) 
                stop("no solution in the specified range of bandwidths")
            if (itry%%2) 
                upper <- upper * 1.2
            else lower <- lower/1.2
            if (getOption("verbose")) 
                message(gettextf("increasing bw.SJ() search interval (%d) to [%.4g,%.4g]", 
                  itry, lower, upper), domain = NA)
            itry <- itry + 1L
        }
        res <- uniroot(fSD, c(lower, upper), tol = tol)$root
    }
    res
}


numericDeriv <- function (expr, theta, rho = parent.frame(), dir = 1) 
{
    dir <- rep_len(dir, length(theta))
    val <- .Call(C_numeric_deriv, expr, theta, rho, dir)
    valDim <- dim(val)
    if (!is.null(valDim)) {
        if (valDim[length(valDim)] == 1) 
            valDim <- valDim[-length(valDim)]
        if (length(valDim) > 1L) 
            dim(attr(val, "gradient")) <- c(valDim, dim(attr(val, 
                "gradient"))[-1L])
    }
    val
}


uniroot <- function (f, interval, ..., lower = min(interval), upper = max(interval), 
    f.lower = f(lower, ...), f.upper = f(upper, ...), extendInt = c("no", 
        "yes", "downX", "upX"), check.conv = FALSE, tol = .Machine$double.eps^0.25, 
    maxiter = 1000, trace = 0) 
{
    if (!missing(interval) && length(interval) != 2L) 
        stop("'interval' must be a vector of length 2")
    if (!is.numeric(lower) || !is.numeric(upper) || lower >= 
        upper) 
        stop("lower < upper  is not fulfilled")
    if (is.na(f.lower)) 
        stop("f.lower = f(lower) is NA")
    if (is.na(f.upper)) 
        stop("f.upper = f(upper) is NA")
    Sig <- switch(match.arg(extendInt), yes = NULL, downX = -1, 
        no = 0, upX = 1, stop("invalid 'extendInt'; please report"))
    truncate <- function(x) pmax.int(pmin(x, .Machine$double.xmax), 
        -.Machine$double.xmax)
    f.low. <- truncate(f.lower)
    f.upp. <- truncate(f.upper)
    doX <- (is.null(Sig) && f.low. * f.upp. > 0 || is.numeric(Sig) && 
        (Sig * f.low. > 0 || Sig * f.upp. < 0))
    if (doX) {
        if (trace) 
            cat(sprintf("search in [%g,%g]%s", lower, upper, 
                if (trace >= 2) 
                  "\n"
                else " ... "))
        Delta <- function(u) 0.01 * pmax(1e-04, abs(u))
        it <- 0L
        if (is.null(Sig)) {
            delta <- Delta(c(lower, upper))
            while (isTRUE(f.lower * f.upper > 0) && any(iF <- is.finite(c(lower, 
                upper)))) {
                if ((it <- it + 1L) > maxiter) 
                  stop(gettextf("no sign change found in %d iterations", 
                    it - 1), domain = NA)
                if (iF[1]) {
                  ol <- lower
                  of <- f.lower
                  if (is.na(f.lower <- f(lower <- lower - delta[1], 
                    ...))) {
                    lower <- ol
                    f.lower <- of
                    delta[1] <- delta[1]/4
                  }
                }
                if (iF[2]) {
                  ol <- upper
                  of <- f.upper
                  if (is.na(f.upper <- f(upper <- upper + delta[2], 
                    ...))) {
                    upper <- ol
                    f.upper <- of
                    delta[2] <- delta[2]/4
                  }
                }
                if (trace >= 2) 
                  cat(sprintf(" .. modified lower,upper: (%15g,%15g)\n", 
                    lower, upper))
                delta <- 2 * delta
            }
        }
        else {
            delta <- Delta(lower)
            while (isTRUE(Sig * f.lower > 0)) {
                if ((it <- it + 1L) > maxiter) 
                  stop(gettextf("no sign change found in %d iterations", 
                    it - 1), domain = NA)
                f.lower <- f(lower <- lower - delta, ...)
                if (trace >= 2) 
                  cat(sprintf(" .. modified lower: %g\n", lower))
                delta <- 2 * delta
            }
            delta <- Delta(upper)
            while (isTRUE(Sig * f.upper < 0)) {
                if ((it <- it + 1L) > maxiter) 
                  stop(gettextf("no sign change found in %d iterations", 
                    it - 1), domain = NA)
                f.upper <- f(upper <- upper + delta, ...)
                if (trace >= 2) 
                  cat(sprintf(" .. modified upper: %g\n", upper))
                delta <- 2 * delta
            }
        }
        if (trace && trace < 2) 
            cat(sprintf("extended to [%g, %g] in %d steps\n", 
                lower, upper, it))
    }
    if (!isTRUE(as.vector(sign(f.lower) * sign(f.upper) <= 0))) 
        stop(if (doX) 
            "did not succeed extending the interval endpoints for f(lower) * f(upper) <= 0"
        else "f() values at end points not of opposite sign")
    if (check.conv) {
        val <- tryCatch(.External2(C_zeroin2, function(arg) f(arg, 
            ...), lower, upper, f.lower, f.upper, tol, as.integer(maxiter)), 
            warning = function(w) w)
        if (inherits(val, "warning")) 
            stop("convergence problem in zero finding: ", conditionMessage(val))
    }
    else {
        val <- .External2(C_zeroin2, function(arg) f(arg, ...), 
            lower, upper, f.lower, f.upper, tol, as.integer(maxiter))
    }
    iter <- as.integer(val[2L])
    if (iter < 0) {
        (if (check.conv) 
            stop
        else warning)(sprintf(ngettext(maxiter, "_NOT_ converged in %d iteration", 
            "_NOT_ converged in %d iterations"), maxiter), domain = NA)
        iter <- maxiter
    }
    if (doX) 
        iter <- iter + it
    else it <- NA_integer_
    list(root = val[1L], f.root = f(val[1L], ...), iter = iter, 
        init.it = it, estim.prec = val[3L])
}


ARMAtoMA <- function (ar = numeric(), ma = numeric(), lag.max) 
.Call(C_ARMAtoMA, as.double(ar), as.double(ma), as.integer(lag.max))


ksmooth <- function (x, y, kernel = c("box", "normal"), bandwidth = 0.5, 
    range.x = range(x), n.points = max(100L, length(x)), x.points) 
{
    if (missing(y) || is.null(y)) 
        stop("numeric y must be supplied.\nFor density estimation use density()")
    kernel <- match.arg(kernel)
    krn <- switch(kernel, box = 1L, normal = 2L)
    x.points <- if (missing(x.points)) 
        seq.int(range.x[1L], range.x[2L], length.out = n.points)
    else {
        n.points <- length(x.points)
        sort(x.points)
    }
    ord <- order(x)
    .Call(C_ksmooth, x[ord], y[ord], x.points, krn, bandwidth)
}


qt <- function (p, df, ncp, lower.tail = TRUE, log.p = FALSE) 
{
    if (missing(ncp)) 
        .Call(C_qt, p, df, lower.tail, log.p)
    else .Call(C_qnt, p, df, ncp, lower.tail, log.p)
}


dffits <- function (model, infl = lm.influence(model, do.coef = FALSE), 
    res = weighted.residuals(model)) 
{
    res <- res * sqrt(infl$hat)/(infl$sigma * (1 - infl$hat))
    res[is.infinite(res)] <- NaN
    res
}


.checkMFClasses <- function (cl, m, ordNotOK = FALSE) 
{
    new <- vapply(m, .MFclass, "")
    new <- new[names(new) %in% names(cl)]
    if (length(new) == 0L) 
        return()
    old <- cl[names(new)]
    if (!ordNotOK) {
        old[old == "ordered"] <- "factor"
        new[new == "ordered"] <- "factor"
    }
    new[new == "ordered" & old == "factor"] <- "factor"
    new[new == "factor" & old == "character"] <- "character"
    if (!identical(old, new)) {
        wrong <- old != new
        if (sum(wrong) == 1) 
            stop(gettextf("variable '%s' was fitted with type \"%s\" but type \"%s\" was supplied", 
                names(old)[wrong], old[wrong], new[wrong]), call. = FALSE, 
                domain = NA)
        else stop(gettextf("variables %s were specified with different types from the fit", 
            paste(sQuote(names(old)[wrong]), collapse = ", ")), 
            call. = FALSE, domain = NA)
    }
}


rstudent <- function (model, ...) 
UseMethod("rstudent")


rf <- function (n, df1, df2, ncp) 
{
    if (missing(ncp)) 
        .Call(C_rf, n, df1, df2)
    else if (is.na(ncp)) {
        warning("NAs produced")
        rep(NaN, n)
    }
    else (rchisq(n, df1, ncp = ncp)/df1)/(rchisq(n, df2)/df2)
}


plclust <- function (tree, hang = 0.1, unit = FALSE, level = FALSE, hmin = 0, 
    square = TRUE, labels = NULL, plot. = TRUE, axes = TRUE, 
    frame.plot = FALSE, ann = TRUE, main = "", sub = NULL, xlab = NULL, 
    ylab = "Height") 
{
    .Deprecated("plot")
    if (!missing(level) && level) 
        .NotYetUsed("level", error = FALSE)
    if (!missing(hmin) && hmin != 0) 
        .NotYetUsed("hmin", error = FALSE)
    if (!missing(square) && !square) 
        .NotYetUsed("square", error = FALSE)
    if (!missing(plot.) && !plot.) 
        .NotYetUsed("plot.", error = TRUE)
    if (!missing(hmin)) 
        tree$height <- pmax(tree$height, hmin)
    if (unit) 
        tree$height <- rank(tree$height)
    plot.hclust(x = tree, labels = labels, hang = hang, axes = axes, 
        frame.plot = frame.plot, ann = ann, main = main, sub = sub, 
        xlab = xlab, ylab = ylab)
}


manova <- function (...) 
{
    Call <- fcall <- match.call()
    fcall[[1L]] <- quote(stats::aov)
    result <- eval(fcall, parent.frame())
    if (inherits(result, "aovlist")) {
        for (i in seq_along(result)) {
            if (!inherits(result[[i]], "maov")) 
                stop("need multiple responses")
            class(result[[i]]) <- c("manova", oldClass(result[[i]]))
        }
        attr(result, "call") <- Call
    }
    else {
        if (!inherits(result, "maov")) 
            stop("need multiple responses")
        class(result) <- c("manova", oldClass(result))
        result$call <- Call
    }
    result
}


dwilcox <- function (x, m, n, log = FALSE) 
{
    on.exit(.External(C_wilcox_free))
    .Call(C_dwilcox, x, m, n, log)
}


kernapply <- function (x, ...) 
{
    UseMethod("kernapply")
}


.MFclass <- function (x) 
{
    if (is.logical(x)) 
        return("logical")
    if (is.ordered(x)) 
        return("ordered")
    if (is.factor(x)) 
        return("factor")
    if (is.character(x)) 
        return("character")
    if (is.matrix(x) && is.numeric(x)) 
        return(paste0("nmatrix.", ncol(x)))
    if (is.numeric(x)) 
        return("numeric")
    return("other")
}


cutree <- function (tree, k = NULL, h = NULL) 
{
    if (is.null(n1 <- nrow(tree$merge)) || n1 < 1) 
        stop("invalid 'tree' ('merge' component)")
    n <- n1 + 1
    if (is.null(k) && is.null(h)) 
        stop("either 'k' or 'h' must be specified")
    if (is.null(k)) {
        if (is.unsorted(tree$height)) 
            stop("the 'height' component of 'tree' is not sorted (increasingly)")
        k <- n + 1L - apply(outer(c(tree$height, Inf), h, ">"), 
            2, which.max)
        if (getOption("verbose")) 
            message("cutree(): k(h) = ", k, domain = NA)
    }
    else {
        k <- as.integer(k)
        if (min(k) < 1 || max(k) > n) 
            stop(gettextf("elements of 'k' must be between 1 and %d", 
                n), domain = NA)
    }
    ans <- .Call(C_cutree, tree$merge, k)
    if (length(k) == 1L) {
        ans <- setNames(as.vector(ans), tree$labels)
    }
    else {
        colnames(ans) <- if (!is.null(h)) 
            h
        else k
        rownames(ans) <- tree$labels
    }
    return(ans)
}


spec.ar <- function (x, n.freq, order = NULL, plot = TRUE, na.action = na.fail, 
    method = "yule-walker", ...) 
{
    if (!is.list(x)) {
        series <- deparse(substitute(x))
        x <- na.action(as.ts(x))
        xfreq <- frequency(x)
        nser <- NCOL(x)
        x <- ar(x, is.null(order), order, na.action = na.action, 
            method = method)
    }
    else {
        cn <- match(c("ar", "var.pred", "order"), names(x))
        if (anyNA(cn)) 
            stop("'x' must be a time series or an ar() fit")
        series <- x$series
        xfreq <- x$frequency
        if (is.array(x$ar)) 
            nser <- dim(x$ar)[2L]
        else nser <- 1
    }
    order <- x$order
    if (missing(n.freq)) 
        n.freq <- 500
    freq <- seq.int(0, 0.5, length.out = n.freq)
    if (nser == 1) {
        coh <- phase <- NULL
        var.p <- as.vector(x$var.pred)
        spec <- if (order >= 1) {
            cs <- outer(freq, 1L:order, function(x, y) cos(2 * 
                pi * x * y)) %*% x$ar
            sn <- outer(freq, 1L:order, function(x, y) sin(2 * 
                pi * x * y)) %*% x$ar
            var.p/(xfreq * ((1 - cs)^2 + sn^2))
        }
        else rep.int(var.p/xfreq, length(freq))
    }
    else .NotYetImplemented()
    spg.out <- list(freq = freq * xfreq, spec = spec, coh = coh, 
        phase = phase, n.used = nrow(x), series = series, method = paste0("AR (", 
            order, ") spectrum "))
    class(spg.out) <- "spec"
    if (plot) {
        plot(spg.out, ci = 0, ...)
        invisible(spg.out)
    }
    else spg.out
}


cmdscale <- function (d, k = 2, eig = FALSE, add = FALSE, x.ret = FALSE, 
    list. = eig || add || x.ret) 
{
    if (anyNA(d)) 
        stop("NA values not allowed in 'd'")
    if (!list.) {
        if (eig) 
            warning("eig=TRUE is disregarded when list.=FALSE")
        if (x.ret) 
            warning("x.ret=TRUE is disregarded when list.=FALSE")
    }
    if (is.null(n <- attr(d, "Size"))) {
        if (add) 
            d <- as.matrix(d)
        x <- as.matrix(d^2)
        storage.mode(x) <- "double"
        if ((n <- nrow(x)) != ncol(x)) 
            stop("distances must be result of 'dist' or a square matrix")
        rn <- rownames(x)
    }
    else {
        rn <- attr(d, "Labels")
        x <- matrix(0, n, n)
        if (add) 
            d0 <- x
        x[row(x) > col(x)] <- d^2
        x <- x + t(x)
        if (add) {
            d0[row(x) > col(x)] <- d
            d <- d0 + t(d0)
        }
    }
    n <- as.integer(n)
    if (is.na(n) || n > 46340) 
        stop(gettextf("invalid value of %s", "'n'"), domain = NA)
    if ((k <- as.integer(k)) > n - 1 || k < 1) 
        stop("'k' must be in {1, 2, ..  n - 1}")
    x <- .Call(C_DoubleCentre, x)
    if (add) {
        i2 <- n + (i <- 1L:n)
        Z <- matrix(0, 2L * n, 2L * n)
        Z[cbind(i2, i)] <- -1
        Z[i, i2] <- -x
        Z[i2, i2] <- .Call(C_DoubleCentre, 2 * d)
        e <- eigen(Z, symmetric = FALSE, only.values = TRUE)$values
        add.c <- max(Re(e))
        x <- matrix(double(n * n), n, n)
        non.diag <- row(d) != col(d)
        x[non.diag] <- (d[non.diag] + add.c)^2
        x <- .Call(C_DoubleCentre, x)
    }
    e <- eigen(-x/2, symmetric = TRUE)
    ev <- e$values[seq_len(k)]
    evec <- e$vectors[, seq_len(k), drop = FALSE]
    k1 <- sum(ev > 0)
    if (k1 < k) {
        warning(gettextf("only %d of the first %d eigenvalues are > 0", 
            k1, k), domain = NA)
        evec <- evec[, ev > 0, drop = FALSE]
        ev <- ev[ev > 0]
    }
    points <- evec * rep(sqrt(ev), each = n)
    dimnames(points) <- list(rn, NULL)
    if (list.) {
        evalus <- e$values
        list(points = points, eig = if (eig) evalus, x = if (x.ret) x, 
            ac = if (add) add.c else 0, GOF = sum(ev)/c(sum(abs(evalus)), 
                sum(pmax(evalus, 0))))
    }
    else points
}


poly <- function (x, ..., degree = 1, coefs = NULL, raw = FALSE, simple = FALSE) 
{
    dots <- list(...)
    if (nd <- length(dots)) {
        if (nd == 1 && length(dots[[1L]]) == 1L) 
            degree <- dots[[1L]]
        else return(polym(x, ..., degree = degree, coefs = coefs, 
            raw = raw))
    }
    if (is.matrix(x)) {
        m <- unclass(as.data.frame(cbind(x, ...)))
        return(do.call(polym, c(m, degree = degree, raw = raw, 
            list(coefs = coefs))))
    }
    if (degree < 1) 
        stop("'degree' must be at least 1")
    if (raw) {
        Z <- outer(x, 1L:degree, "^")
        colnames(Z) <- 1L:degree
    }
    else {
        if (is.null(coefs)) {
            if (anyNA(x)) 
                stop("missing values are not allowed in 'poly'")
            if (degree >= length(unique(x))) 
                stop("'degree' must be less than number of unique points")
            xbar <- mean(x)
            x <- x - xbar
            X <- outer(x, 0L:degree, "^")
            QR <- qr(X)
            if (QR$rank < degree) 
                stop("'degree' must be less than number of unique points")
            z <- QR$qr
            z <- z * (row(z) == col(z))
            Z <- qr.qy(QR, z)
            norm2 <- colSums(Z^2)
            alpha <- (colSums(x * Z^2)/norm2 + xbar)[1L:degree]
            norm2 <- c(1, norm2)
        }
        else {
            alpha <- coefs$alpha
            norm2 <- coefs$norm2
            Z <- matrix(1, length(x), degree + 1L)
            Z[, 2] <- x - alpha[1L]
            if (degree > 1) 
                for (i in 2:degree) Z[, i + 1] <- (x - alpha[i]) * 
                  Z[, i] - (norm2[i + 1]/norm2[i]) * Z[, i - 
                  1]
        }
        Z <- Z/rep(sqrt(norm2[-1L]), each = length(x))
        colnames(Z) <- 0L:degree
        Z <- Z[, -1, drop = FALSE]
        if (!simple) 
            attr(Z, "coefs") <- list(alpha = alpha, norm2 = norm2)
    }
    if (simple) 
        Z
    else structure(Z, degree = 1L:degree, class = c("poly", "matrix"))
}


dweibull <- function (x, shape, scale = 1, log = FALSE) 
.Call(C_dweibull, x, shape, scale, log)


optimise <- function (f, interval, ..., lower = min(interval), upper = max(interval), 
    maximum = FALSE, tol = .Machine$double.eps^0.25) 
{
    if (maximum) {
        val <- .External2(C_do_fmin, function(arg) -f(arg, ...), 
            lower, upper, tol)
        list(maximum = val, objective = f(val, ...))
    }
    else {
        val <- .External2(C_do_fmin, function(arg) f(arg, ...), 
            lower, upper, tol)
        list(minimum = val, objective = f(val, ...))
    }
}


formula <- function (x, ...) 
UseMethod("formula")


sd <- function (x, na.rm = FALSE) 
sqrt(var(if (is.vector(x) || is.factor(x)) x else as.double(x), 
    na.rm = na.rm))


rt <- function (n, df, ncp) 
{
    if (missing(ncp)) 
        .Call(C_rt, n, df)
    else if (is.na(ncp)) {
        warning("NAs produced")
        rep(NaN, n)
    }
    else rnorm(n, ncp)/sqrt(rchisq(n, df)/df)
}


loglin <- function (table, margin, start = rep(1, length(table)), fit = FALSE, 
    eps = 0.1, iter = 20L, param = FALSE, print = TRUE) 
{
    rfit <- fit
    dtab <- dim(table)
    nvar <- length(dtab)
    ncon <- length(margin)
    conf <- matrix(0L, nrow = nvar, ncol = ncon)
    nmar <- 0
    varnames <- names(dimnames(table))
    for (k in seq_along(margin)) {
        tmp <- margin[[k]]
        if (is.character(tmp)) {
            tmp <- match(tmp, varnames)
            margin[[k]] <- tmp
        }
        if (!is.numeric(tmp) || any(is.na(tmp) | tmp <= 0)) 
            stop("'margin' must contain names or numbers corresponding to 'table'")
        conf[seq_along(tmp), k] <- tmp
        nmar <- nmar + prod(dtab[tmp])
    }
    ntab <- length(table)
    if (length(start) != ntab) 
        stop("'start' and 'table' must be same length")
    z <- .Call(C_LogLin, dtab, conf, table, start, nmar, eps, 
        iter)
    if (print) 
        cat(z$nlast, "iterations: deviation", z$dev[z$nlast], 
            "\n")
    fit <- z$fit
    attributes(fit) <- attributes(table)
    observed <- as.vector(table[start > 0])
    expected <- as.vector(fit[start > 0])
    pearson <- sum((observed - expected)^2/expected)
    observed <- as.vector(table[table * fit > 0])
    expected <- as.vector(fit[table * fit > 0])
    lrt <- 2 * sum(observed * log(observed/expected))
    subsets <- function(x) {
        y <- list(vector(mode(x), length = 0))
        for (i in seq_along(x)) {
            y <- c(y, lapply(y, "c", x[i]))
        }
        y[-1L]
    }
    df <- rep.int(0, 2^nvar)
    for (k in seq_along(margin)) {
        terms <- subsets(margin[[k]])
        for (j in seq_along(terms)) df[sum(2^(terms[[j]] - 1))] <- prod(dtab[terms[[j]]] - 
            1)
    }
    if (!is.null(varnames) && all(nzchar(varnames))) {
        for (k in seq_along(margin)) margin[[k]] <- varnames[margin[[k]]]
    }
    else {
        varnames <- as.character(1:ntab)
    }
    y <- list(lrt = lrt, pearson = pearson, df = ntab - sum(df) - 
        1, margin = margin)
    if (rfit) 
        y$fit <- fit
    if (param) {
        fit <- log(fit)
        terms <- seq_along(df)[df > 0]
        parlen <- length(terms) + 1
        parval <- list(parlen)
        parnam <- character(parlen)
        parval[[1L]] <- mean(fit)
        parnam[1L] <- "(Intercept)"
        fit <- fit - parval[[1L]]
        dyadic <- NULL
        while (any(terms > 0)) {
            dyadic <- cbind(dyadic, terms%%2)
            terms <- terms%/%2
        }
        dyadic <- dyadic[order(rowSums(dyadic)), , drop = FALSE]
        for (i in 2:parlen) {
            vars <- which(dyadic[i - 1, ] > 0)
            parval[[i]] <- apply(fit, vars, mean)
            parnam[i] <- paste(varnames[vars], collapse = ".")
            fit <- sweep(fit, vars, parval[[i]], check.margin = FALSE)
        }
        names(parval) <- parnam
        y$param <- parval
    }
    return(y)
}


qbinom <- function (p, size, prob, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qbinom, p, size, prob, lower.tail, log.p)


median.default <- function (x, na.rm = FALSE, ...) 
{
    if (is.factor(x) || is.data.frame(x)) 
        stop("need numeric data")
    if (length(names(x))) 
        names(x) <- NULL
    if (na.rm) 
        x <- x[!is.na(x)]
    else if (any(is.na(x))) 
        return(x[FALSE][NA])
    n <- length(x)
    if (n == 0L) 
        return(x[FALSE][NA])
    half <- (n + 1L)%/%2L
    if (n%%2L == 1L) 
        sort(x, partial = half)[half]
    else mean(sort(x, partial = half + 0L:1L)[half + 0L:1L])
}


gaussian <- function (link = "identity") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    okLinks <- c("inverse", "log", "identity")
    if (linktemp %in% okLinks) 
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    }
    else {
        if (inherits(link, "link-glm")) {
            stats <- link
            if (!is.null(stats$name)) 
                linktemp <- stats$name
        }
        else {
            stop(gettextf("link \"%s\" not available for gaussian family; available links are %s", 
                linktemp, paste(sQuote(okLinks), collapse = ", ")), 
                domain = NA)
        }
    }
    structure(list(family = "gaussian", link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, variance = function(mu) rep.int(1, 
            length(mu)), dev.resids = function(y, mu, wt) wt * 
            ((y - mu)^2), aic = function(y, n, mu, wt, dev) {
            nobs <- length(y)
            nobs * (log(dev/nobs * 2 * pi) + 1) + 2 - sum(log(wt))
        }, mu.eta = stats$mu.eta, initialize = expression({
            n <- rep.int(1, nobs)
            if (is.null(etastart) && is.null(start) && is.null(mustart) && 
                ((family$link == "inverse" && any(y == 0)) || 
                  (family$link == "log" && any(y <= 0)))) stop("cannot find valid starting values: please specify some")
            mustart <- y
        }), validmu = function(mu) TRUE, valideta = stats$valideta), 
        class = "family")
}


model.matrix <- function (object, ...) 
UseMethod("model.matrix")


fitted.values <- function (object, ...) 
UseMethod("fitted")


var <- function (x, y = NULL, na.rm = FALSE, use) 
{
    if (missing(use)) 
        use <- if (na.rm) 
            "na.or.complete"
        else "everything"
    na.method <- pmatch(use, c("all.obs", "complete.obs", "pairwise.complete.obs", 
        "everything", "na.or.complete"))
    if (is.na(na.method)) 
        stop("invalid 'use' argument")
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    else stopifnot(is.atomic(x))
    if (is.data.frame(y)) 
        y <- as.matrix(y)
    else stopifnot(is.atomic(y))
    .Call(C_cov, x, y, na.method, FALSE)
}


binomial <- function (link = "logit") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    okLinks <- c("logit", "probit", "cloglog", "cauchit", "log")
    if (linktemp %in% okLinks) 
        stats <- make.link(linktemp)
    else if (is.character(link)) {
        stats <- make.link(link)
        linktemp <- link
    }
    else {
        if (inherits(link, "link-glm")) {
            stats <- link
            if (!is.null(stats$name)) 
                linktemp <- stats$name
        }
        else {
            stop(gettextf("link \"%s\" not available for binomial family; available links are %s", 
                linktemp, paste(sQuote(okLinks), collapse = ", ")), 
                domain = NA)
        }
    }
    variance <- function(mu) mu * (1 - mu)
    validmu <- function(mu) all(is.finite(mu)) && all(mu > 0 & 
        mu < 1)
    dev.resids <- function(y, mu, wt) .Call(C_binomial_dev_resids, 
        y, mu, wt)
    aic <- function(y, n, mu, wt, dev) {
        m <- if (any(n > 1)) 
            n
        else wt
        -2 * sum(ifelse(m > 0, (wt/m), 0) * dbinom(round(m * 
            y), round(m), mu, log = TRUE))
    }
    initialize <- expression({
        if (NCOL(y) == 1) {
            if (is.factor(y)) y <- y != levels(y)[1L]
            n <- rep.int(1, nobs)
            y[weights == 0] <- 0
            if (any(y < 0 | y > 1)) stop("y values must be 0 <= y <= 1")
            mustart <- (weights * y + 0.5)/(weights + 1)
            m <- weights * y
            if (any(abs(m - round(m)) > 0.001)) warning("non-integer #successes in a binomial glm!")
        } else if (NCOL(y) == 2) {
            if (any(abs(y - round(y)) > 0.001)) warning("non-integer counts in a binomial glm!")
            n <- y[, 1] + y[, 2]
            y <- ifelse(n == 0, 0, y[, 1]/n)
            weights <- weights * n
            mustart <- (n * y + 0.5)/(n + 1)
        } else stop("for the 'binomial' family, y must be a vector of 0 and 1's\nor a 2 column matrix where col 1 is no. successes and col 2 is no. failures")
    })
    simfun <- function(object, nsim) {
        ftd <- fitted(object)
        n <- length(ftd)
        ntot <- n * nsim
        wts <- object$prior.weights
        if (any(wts%%1 != 0)) 
            stop("cannot simulate from non-integer prior.weights")
        if (!is.null(m <- object$model)) {
            y <- model.response(m)
            if (is.factor(y)) {
                yy <- factor(1 + rbinom(ntot, size = 1, prob = ftd), 
                  labels = levels(y))
                split(yy, rep(seq_len(nsim), each = n))
            }
            else if (is.matrix(y) && ncol(y) == 2) {
                yy <- vector("list", nsim)
                for (i in seq_len(nsim)) {
                  Y <- rbinom(n, size = wts, prob = ftd)
                  YY <- cbind(Y, wts - Y)
                  colnames(YY) <- colnames(y)
                  yy[[i]] <- YY
                }
                yy
            }
            else rbinom(ntot, size = wts, prob = ftd)/wts
        }
        else rbinom(ntot, size = wts, prob = ftd)/wts
    }
    structure(list(family = "binomial", link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
        validmu = validmu, valideta = stats$valideta, simulate = simfun), 
        class = "family")
}


arima.sim <- function (model, n, rand.gen = rnorm, innov = rand.gen(n, ...), 
    n.start = NA, start.innov = rand.gen(n.start, ...), ...) 
{
    if (!is.list(model)) 
        stop("'model' must be list")
    if (n <= 0L) 
        stop("'n' must be strictly positive")
    p <- length(model$ar)
    if (p) {
        minroots <- min(Mod(polyroot(c(1, -model$ar))))
        if (minroots <= 1) 
            stop("'ar' part of model is not stationary")
    }
    q <- length(model$ma)
    if (is.na(n.start)) 
        n.start <- p + q + ifelse(p > 0, ceiling(6/log(minroots)), 
            0)
    if (n.start < p + q) 
        stop("burn-in 'n.start' must be as long as 'ar + ma'")
    d <- 0
    if (!is.null(ord <- model$order)) {
        if (length(ord) != 3L) 
            stop("'model$order' must be of length 3")
        if (p != ord[1L]) 
            stop("inconsistent specification of 'ar' order")
        if (q != ord[3L]) 
            stop("inconsistent specification of 'ma' order")
        d <- ord[2L]
        if (d != round(d) || d < 0) 
            stop("number of differences must be a positive integer")
    }
    if (!missing(start.innov) && length(start.innov) < n.start) 
        stop(sprintf(ngettext(n.start, "'start.innov' is too short: need %d point", 
            "'start.innov' is too short: need %d points"), n.start), 
            domain = NA)
    x <- ts(c(start.innov[seq_len(n.start)], innov[1L:n]), start = 1 - 
        n.start)
    if (length(model$ma)) {
        x <- filter(x, c(1, model$ma), sides = 1L)
        x[seq_along(model$ma)] <- 0
    }
    if (length(model$ar)) 
        x <- filter(x, model$ar, method = "recursive")
    if (n.start > 0) 
        x <- x[-(seq_len(n.start))]
    if (d > 0) 
        x <- diffinv(x, differences = d)
    as.ts(x)
}


logLik <- function (object, ...) 
UseMethod("logLik")


selfStart <- function (model, initial, parameters, template) 
UseMethod("selfStart")


reformulate <- function (termlabels, response = NULL, intercept = TRUE) 
{
    if (!is.character(termlabels) || !length(termlabels)) 
        stop("'termlabels' must be a character vector of length at least one")
    has.resp <- !is.null(response)
    termtext <- paste(if (has.resp) 
        "response", "~", paste(termlabels, collapse = "+"), collapse = "")
    if (!intercept) 
        termtext <- paste(termtext, "- 1")
    rval <- eval(parse(text = termtext, keep.source = FALSE)[[1L]])
    if (has.resp) 
        rval[[2L]] <- if (is.character(response)) 
            as.symbol(response)
        else response
    environment(rval) <- parent.frame()
    rval
}


naresid <- function (omit, x, ...) 
UseMethod("naresid")


StructTS <- function (x, type = c("level", "trend", "BSM"), init = NULL, 
    fixed = NULL, optim.control = NULL) 
{
    makeLevel <- function(x) {
        T <- matrix(1, 1L, 1L)
        Z <- 1
        xm <- if (is.na(x[1L])) 
            mean(x, na.rm = TRUE)
        else x[1L]
        if (is.na(xm)) 
            stop("the series is entirely NA")
        a <- xm
        P <- Pn <- matrix(0, 1L, 1L)
        h <- 1
        V <- diag(1L)
        return(list(Z = Z, a = a, P = P, T = T, V = V, h = h, 
            Pn = Pn))
    }
    makeTrend <- function(x) {
        T <- matrix(c(1, 0, 1, 1), 2L, 2L)
        Z <- c(1, 0)
        xm <- if (is.na(x[1L])) 
            mean(x, na.rm = TRUE)
        else x[1L]
        if (is.na(xm)) 
            stop("the series is entirely NA")
        a <- c(xm, 0)
        P <- Pn <- matrix(0, 2L, 2L)
        h <- 1
        V <- diag(2L)
        return(list(Z = Z, a = a, P = P, T = T, V = V, h = h, 
            Pn = Pn))
    }
    makeBSM <- function(x, nf) {
        if (nf <= 1L) 
            stop("frequency must be a positive integer >= 2 for BSM")
        T <- matrix(0, nf + 1L, nf + 1L)
        T[1L:2L, 1L:2L] <- c(1, 0, 1, 1)
        T[3L, ] <- c(0, 0, rep(-1, nf - 1L))
        if (nf >= 3L) {
            ind <- 3:nf
            T[cbind(ind + 1L, ind)] <- 1
        }
        Z <- c(1, 0, 1, rep(0, nf - 2L))
        xm <- if (is.na(x[1L])) 
            mean(x, na.rm = TRUE)
        else x[1L]
        if (is.na(xm)) 
            stop("the series is entirely NA")
        a <- c(xm, rep(0, nf))
        P <- Pn <- matrix(0, nf + 1L, nf + 1L)
        h <- 1
        V <- diag(c(1, 1, 1, rep(0, nf - 2L)))
        return(list(Z = Z, a = a, P = P, T = T, V = V, h = h, 
            Pn = Pn))
    }
    getLike <- function(par) {
        p <- cf
        p[mask] <- par
        if (all(p == 0)) 
            return(1000)
        Z$V[cbind(1L:np, 1L:np)] <- p[-(np + 1L)] * vx
        Z$h <- p[np + 1L] * vx
        z <- .Call(C_KalmanLike, y, Z, -1L, FALSE, FALSE)
        0.5 * sum(z)
    }
    series <- deparse(substitute(x))
    if (NCOL(x) > 1L) 
        stop("only implemented for univariate time series")
    x <- as.ts(x)
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    storage.mode(x) <- "double"
    if (is.na(x[1L])) 
        stop("the first value of the time series must not be missing")
    type <- if (missing(type)) 
        if (frequency(x) > 1) 
            "BSM"
        else "trend"
    else match.arg(type)
    dim(x) <- NULL
    xtsp <- tsp(x)
    nf <- frequency(x)
    Z <- switch(type, level = makeLevel(x), trend = makeTrend(x), 
        BSM = makeBSM(x, nf))
    vx <- var(x, na.rm = TRUE)/100
    Z$P[] <- 1e+06 * vx
    np <- switch(type, level = 1L, trend = 2L, BSM = 3L)
    if (is.null(fixed)) 
        fixed <- rep(NA_real_, np + 1L)
    mask <- is.na(fixed)
    if (!any(mask)) 
        stop("all parameters were fixed")
    cf <- fixed/vx
    if (is.null(init)) 
        init <- rep(1, np + 1L)
    else init <- init/vx
    y <- x
    res <- optim(init[mask], getLike, method = "L-BFGS-B", lower = rep(0, 
        np + 1L), upper = rep(Inf, np + 1L), control = optim.control)
    if (res$convergence > 0) 
        warning(gettextf("possible convergence problem: 'optim' gave code = %d and message %s", 
            res$convergence, sQuote(res$message)), domain = NA)
    coef <- cf
    coef[mask] <- res$par
    Z$V[cbind(1L:np, 1L:np)] <- coef[1L:np] * vx
    Z$h <- coef[np + 1L] * vx
    z <- KalmanRun(y, Z, -1, update = TRUE)
    resid <- ts(z$resid)
    tsp(resid) <- xtsp
    cn <- switch(type, level = c("level"), trend = c("level", 
        "slope"), BSM = c("level", "slope", "sea"))
    states <- z$states
    if (type == "BSM") 
        states <- states[, 1L:3L]
    dimnames(states) <- list(time(x), cn)
    states <- ts(states, start = xtsp[1L], frequency = nf)
    coef <- pmax(coef * vx, 0)
    names(coef) <- switch(type, level = c("level", "epsilon"), 
        trend = c("level", "slope", "epsilon"), BSM = c("level", 
            "slope", "seas", "epsilon"))
    loglik <- -length(y) * res$value - 0.5 * sum(!is.na(y)) * 
        log(2 * pi)
    loglik0 <- -length(y) * res$value + length(y) * log(2 * pi)
    res <- list(coef = coef, loglik = loglik, loglik0 = loglik0, 
        data = y, residuals = resid, fitted = states, call = match.call(), 
        series = series, code = res$convergence, model = attr(z, 
            "mod"), model0 = Z, xtsp = xtsp)
    class(res) <- "StructTS"
    res
}


interaction.plot <- function (x.factor, trace.factor, response, fun = mean, type = c("l", 
    "p", "b", "o", "c"), legend = TRUE, trace.label = deparse(substitute(trace.factor)), 
    fixed = FALSE, xlab = deparse(substitute(x.factor)), ylab = ylabel, 
    ylim = range(cells, na.rm = TRUE), lty = nc:1, col = 1, pch = c(1L:9, 
        0, letters), xpd = NULL, leg.bg = par("bg"), leg.bty = "n", 
    xtick = FALSE, xaxt = par("xaxt"), axes = TRUE, ...) 
{
    ylabel <- paste(deparse(substitute(fun)), "of ", deparse(substitute(response)))
    type <- match.arg(type)
    cells <- tapply(response, list(x.factor, trace.factor), fun)
    nr <- nrow(cells)
    nc <- ncol(cells)
    xvals <- 1L:nr
    if (is.ordered(x.factor)) {
        wn <- getOption("warn")
        options(warn = -1)
        xnm <- as.numeric(levels(x.factor))
        options(warn = wn)
        if (!anyNA(xnm)) 
            xvals <- xnm
    }
    xlabs <- rownames(cells)
    ylabs <- colnames(cells)
    nch <- max(sapply(ylabs, nchar, type = "width"))
    if (is.null(xlabs)) 
        xlabs <- as.character(xvals)
    if (is.null(ylabs)) 
        ylabs <- as.character(1L:nc)
    xlim <- range(xvals)
    xleg <- xlim[2L] + 0.05 * diff(xlim)
    xlim <- xlim + c(-0.2/nr, if (legend) 0.2 + 0.02 * nch else 0.2/nr) * 
        diff(xlim)
    dev.hold()
    on.exit(dev.flush())
    matplot(xvals, cells, ..., type = type, xlim = xlim, ylim = ylim, 
        xlab = xlab, ylab = ylab, axes = axes, xaxt = "n", col = col, 
        lty = lty, pch = pch)
    if (axes && xaxt != "n") {
        axisInt <- function(x, main, sub, lwd, bg, log, asp, 
            ...) axis(1, x, ...)
        mgp. <- par("mgp")
        if (!xtick) 
            mgp.[2L] <- 0
        axisInt(1, at = xvals, labels = xlabs, tick = xtick, 
            mgp = mgp., xaxt = xaxt, ...)
    }
    if (legend) {
        yrng <- diff(ylim)
        yleg <- ylim[2L] - 0.1 * yrng
        if (!is.null(xpd) || {
            xpd. <- par("xpd")
            !is.na(xpd.) && !xpd. && (xpd <- TRUE)
        }) {
            op <- par(xpd = xpd)
            on.exit(par(op), add = TRUE)
        }
        text(xleg, ylim[2L] - 0.05 * yrng, paste("  ", trace.label), 
            adj = 0)
        if (!fixed) {
            ord <- sort.list(cells[nr, ], decreasing = TRUE)
            ylabs <- ylabs[ord]
            lty <- lty[1 + (ord - 1)%%length(lty)]
            col <- col[1 + (ord - 1)%%length(col)]
            pch <- pch[ord]
        }
        legend(xleg, yleg, legend = ylabs, col = col, pch = if (type %in% 
            c("p", "b")) 
            pch, lty = if (type %in% c("l", "b")) 
            lty, bty = leg.bty, bg = leg.bg)
    }
    invisible()
}


SSlogis <- function (input, Asym, xmid, scal) 
{
    .expr1 <- xmid - input
    .expr3 <- exp(.e2 <- .expr1/scal)
    .expr4 <- 1 + .expr3
    .value <- Asym/.expr4
    .actualArgs <- as.list(match.call()[c("Asym", "xmid", "scal")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .expr10 <- .expr4^2
        .grad <- array(0, c(length(.value), 3L), list(NULL, c("Asym", 
            "xmid", "scal")))
        .grad[, "Asym"] <- 1/.expr4
        .grad[, "xmid"] <- -(xm <- Asym * .expr3/scal/.expr10)
        .grad[, "scal"] <- xm * .e2
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


terms.formula <- function (x, specials = NULL, abb = NULL, data = NULL, neg.out = TRUE, 
    keep.order = FALSE, simplify = FALSE, ..., allowDotAsName = FALSE) 
{
    fixFormulaObject <- function(object) {
        Terms <- terms(object)
        tmp <- attr(Terms, "term.labels")
        ind <- grep("|", tmp, fixed = TRUE)
        if (length(ind)) 
            tmp[ind] <- paste("(", tmp[ind], ")")
        if (length(ind <- attr(Terms, "offset"))) {
            tmp2 <- as.character(attr(Terms, "variables"))[-1L]
            tmp <- c(tmp, tmp2[ind])
        }
        rhs <- if (length(tmp)) 
            paste(tmp, collapse = " + ")
        else "1"
        if (!attr(terms(object), "intercept")) 
            rhs <- paste(rhs, "- 1")
        if (length(form <- formula(object)) > 2L) {
            res <- formula(paste("lhs ~", rhs))
            res[[2L]] <- form[[2L]]
            res
        }
        else formula(paste("~", rhs))
    }
    if (!is.null(data) && !is.environment(data) && !is.data.frame(data)) 
        data <- as.data.frame(data, optional = TRUE)
    terms <- .External(C_termsform, x, specials, data, keep.order, 
        allowDotAsName)
    if (simplify) {
        a <- attributes(terms)
        terms <- fixFormulaObject(terms)
        attributes(terms) <- a
    }
    environment(terms) <- environment(x)
    if (!inherits(terms, "formula")) 
        class(terms) <- c(oldClass(terms), "formula")
    terms
}


supsmu <- function (x, y, wt = rep(1, n), span = "cv", periodic = FALSE, 
    bass = 0, trace = FALSE) 
{
    if (span == "cv") 
        span <- 0
    else if (span < 0 || span > 1) 
        stop("'span' must be between 0 and 1.")
    n <- length(y)
    if (!n || !is.numeric(y)) 
        stop("'y' must be numeric vector")
    if (length(x) != n) 
        stop("number of observations in 'x' and 'y' must match.")
    if (length(wt) != n) 
        stop("number of weights must match number of observations.")
    if (periodic) {
        iper <- 2L
        xrange <- range(x)
        if (xrange[1L] < 0 || xrange[2L] > 1) 
            stop("'x' must be between 0 and 1 for periodic smooth")
    }
    else iper <- 1L
    okay <- is.finite(x + y + wt)
    ord <- order(x[okay], y[okay])
    ord <- cumsum(!okay)[okay][ord] + ord
    xo <- x[ord]
    leno <- length(ord)
    if (leno == 0L) 
        stop("no finite observations")
    if (diff <- n - leno) 
        warning(sprintf(ngettext(diff, "%d observation with NA, NaN or Inf deleted", 
            "%d observations with NAs, NaNs and/or Infs deleted"), 
            diff), domain = NA)
    .Fortran(C_setsmu, as.integer(trace))
    smo <- .Fortran(C_supsmu, as.integer(leno), as.double(xo), 
        as.double(y[ord]), as.double(wt[ord]), as.integer(iper), 
        as.double(span), as.double(bass), smo = double(leno), 
        double(n * 7L), double(1L))$smo
    dupx <- duplicated(xo)
    list(x = xo[!dupx], y = smo[!dupx])
}


dgamma <- function (x, shape, rate = 1, scale = 1/rate, log = FALSE) 
{
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    .Call(C_dgamma, x, shape, scale, log)
}


weighted.residuals <- function (obj, drop0 = TRUE) 
{
    w <- weights(obj)
    r <- residuals(obj, type = "deviance")
    if (drop0 && !is.null(w)) {
        if (is.matrix(r)) 
            r[w != 0, , drop = FALSE]
        else r[w != 0]
    }
    else r
}


smooth <- function (x, kind = c("3RS3R", "3RSS", "3RSR", "3R", "3", "S"), 
    twiceit = FALSE, endrule = c("Tukey", "copy"), do.ends = FALSE) 
{
    if (!is.numeric(x)) 
        stop("attempt to smooth non-numeric values")
    if (anyNA(x)) 
        stop("attempt to smooth NA values")
    endrule <- match.arg(endrule)
    rules <- c("copy", "Tukey")
    if (is.na(iend <- pmatch(endrule, rules))) 
        stop("invalid 'endrule' argument")
    kind <- match.arg(kind)
    if (substr(kind, 1L, 3L) == "3RS" && !do.ends) 
        iend <- -iend
    else if (kind == "S") 
        iend <- as.logical(do.ends)
    type <- match(kind, c("3RS3R", "3RSS", "3RSR", "3R", "3", 
        "S"))
    smo <- .Call(C_Rsm, as.double(x), type, iend)
    if (twiceit) {
        r <- smooth(x - smo$y, kind = kind, twiceit = FALSE, 
            endrule = endrule, do.ends = do.ends)
        smo$y <- smo$y + r
        if (!is.null(smo$iter)) 
            smo$iter <- smo$iter + attr(r, "iter")
        if (!is.null(smo$changed)) 
            smo$changed <- smo$changed || attr(r, "changed")
    }
    if (is.ts(x)) 
        smo$y <- ts(smo$y, start = start(x), frequency = frequency(x))
    structure(smo$y, kind = kind, twiced = twiceit, iter = smo$iter, 
        changed = smo$changed, endrule = if (substr(kind, 1L, 
            1L) == "3") 
            rules[iend], call = match.call(), class = c("tukeysmooth", 
            if (is.ts(x)) "ts"))
}


kernel <- function (coef, m = 2, r, name = "unknown") 
{
    mkName <- function(name, args) paste0(name, "(", paste(args, 
        collapse = ","), ")")
    modified.daniell.kernel <- function(m) {
        if (length(m) == 1L) 
            k <- kernel(c(rep_len(1, m), 0.5)/(2 * m), m)
        else {
            k <- Recall(m[1L])
            for (i in 2L:length(m)) k <- kernapply(k, Recall(m[i]))
        }
        attr(k, "name") <- mkName("mDaniell", m)
        k
    }
    daniell.kernel <- function(m) {
        if (length(m) == 1L) 
            k <- kernel(rep_len(1/(2 * m + 1), m + 1), m)
        else {
            k <- Recall(m[1L])
            for (i in 2L:length(m)) k <- kernapply(k, Recall(m[i]))
        }
        attr(k, "name") <- mkName("Daniell", m)
        k
    }
    fejer.kernel <- function(m, r) {
        if (r < 1L) 
            stop("'r' is less than 1")
        if (m < 1L) 
            stop("'m' is less than 1")
        n <- 2L * m + 1L
        wn <- double(m + 1L)
        wj <- 2 * pi * (1L:m)/n
        wn[2L:(m + 1L)] <- sin(r * wj/2)^2/sin(wj/2)^2/r
        wn[1L] <- r
        wn <- wn/(wn[1L] + 2 * sum(wn[2L:(m + 1L)]))
        kernel(wn, m, name = mkName("Fejer", c(m, r)))
    }
    dirichlet.kernel <- function(m, r) {
        if (r < 0) 
            stop("'r' is less than 0")
        if (m < 1) 
            stop("'m' is less than 1")
        n <- 2L * m + 1L
        wn <- double(m + 1L)
        wj <- 2 * pi * (1L:m)/n
        wn[2L:(m + 1)] <- sin((r + 0.5) * wj)/sin(wj/2)
        wn[1L] <- 2 * r + 1
        wn <- wn/(wn[1L] + 2 * sum(wn[2L:(m + 1L)]))
        kernel(wn, m, name = mkName("Dirichlet", c(m, r)))
    }
    if (!missing(m)) 
        if (!is.numeric(m) || length(m) < 1L || m != round(m) || 
            any(m < 0L)) 
            stop("'m' must be numeric with non-negative integers")
    if (is.character(coef)) {
        switch(coef, daniell = daniell.kernel(m), dirichlet = dirichlet.kernel(m, 
            r), fejer = fejer.kernel(m, r), modified.daniell = modified.daniell.kernel(m), 
            stop("unknown named kernel"))
    }
    else {
        if (!is.numeric(coef)) 
            stop("'coef' must be a vector")
        if (length(coef) < 1L) 
            stop("'coef' does not have the correct length")
        m <- length(coef) - 1L
        kernel <- list(coef = coef, m = m)
        attr(kernel, "name") <- name
        class(kernel) <- "tskernel"
        sk <- sum(kernel[-m:m])
        if (abs(sk - 1) > getOption("ts.eps")) 
            stop("coefficients do not add to 1")
        kernel
    }
}


confint <- function (object, parm, level = 0.95, ...) 
UseMethod("confint")


mvfft <- function (z, inverse = FALSE) 
.Call(C_mvfft, z, inverse)


kruskal.test <- function (x, ...) 
UseMethod("kruskal.test")


spec.pgram <- function (x, spans = NULL, kernel = NULL, taper = 0.1, pad = 0, 
    fast = TRUE, demean = FALSE, detrend = TRUE, plot = TRUE, 
    na.action = na.fail, ...) 
{
    series <- deparse(substitute(x))
    x <- na.action(as.ts(x))
    xfreq <- frequency(x)
    x <- as.matrix(x)
    N <- N0 <- nrow(x)
    nser <- ncol(x)
    if (!is.null(spans)) 
        kernel <- {
            if (is.tskernel(spans)) 
                spans
            else kernel("modified.daniell", spans%/%2)
        }
    if (!is.null(kernel) && !is.tskernel(kernel)) 
        stop("must specify 'spans' or a valid kernel")
    if (detrend) {
        t <- 1L:N - (N + 1)/2
        sumt2 <- N * (N^2 - 1)/12
        for (i in 1L:ncol(x)) x[, i] <- x[, i] - mean(x[, i]) - 
            sum(x[, i] * t) * t/sumt2
    }
    else if (demean) {
        x <- sweep(x, 2, colMeans(x), check.margin = FALSE)
    }
    x <- spec.taper(x, taper)
    u2 <- (1 - (5/8) * taper * 2)
    u4 <- (1 - (93/128) * taper * 2)
    if (pad > 0) {
        x <- rbind(x, matrix(0, nrow = N * pad, ncol = ncol(x)))
        N <- nrow(x)
    }
    NewN <- if (fast) 
        nextn(N)
    else N
    x <- rbind(x, matrix(0, nrow = (NewN - N), ncol = ncol(x)))
    N <- nrow(x)
    Nspec <- floor(N/2)
    freq <- seq.int(from = xfreq/N, by = xfreq/N, length.out = Nspec)
    xfft <- mvfft(x)
    pgram <- array(NA, dim = c(N, ncol(x), ncol(x)))
    for (i in 1L:ncol(x)) {
        for (j in 1L:ncol(x)) {
            pgram[, i, j] <- xfft[, i] * Conj(xfft[, j])/(N0 * 
                xfreq)
            pgram[1, i, j] <- 0.5 * (pgram[2, i, j] + pgram[N, 
                i, j])
        }
    }
    if (!is.null(kernel)) {
        for (i in 1L:ncol(x)) for (j in 1L:ncol(x)) pgram[, i, 
            j] <- kernapply(pgram[, i, j], kernel, circular = TRUE)
        df <- df.kernel(kernel)
        bandwidth <- bandwidth.kernel(kernel)
    }
    else {
        df <- 2
        bandwidth <- sqrt(1/12)
    }
    df <- df/(u4/u2^2)
    df <- df * (N0/N)
    bandwidth <- bandwidth * xfreq/N
    pgram <- pgram[2:(Nspec + 1), , , drop = FALSE]
    spec <- matrix(NA, nrow = Nspec, ncol = nser)
    for (i in 1L:nser) spec[, i] <- Re(pgram[1L:Nspec, i, i])
    if (nser == 1) {
        coh <- phase <- NULL
    }
    else {
        coh <- phase <- matrix(NA, nrow = Nspec, ncol = nser * 
            (nser - 1)/2)
        for (i in 1L:(nser - 1)) {
            for (j in (i + 1):nser) {
                coh[, i + (j - 1) * (j - 2)/2] <- Mod(pgram[, 
                  i, j])^2/(spec[, i] * spec[, j])
                phase[, i + (j - 1) * (j - 2)/2] <- Arg(pgram[, 
                  i, j])
            }
        }
    }
    for (i in 1L:nser) spec[, i] <- spec[, i]/u2
    spec <- drop(spec)
    spg.out <- list(freq = freq, spec = spec, coh = coh, phase = phase, 
        kernel = kernel, df = df, bandwidth = bandwidth, n.used = N, 
        orig.n = N0, series = series, snames = colnames(x), method = ifelse(!is.null(kernel), 
            "Smoothed Periodogram", "Raw Periodogram"), taper = taper, 
        pad = pad, detrend = detrend, demean = demean)
    class(spg.out) <- "spec"
    if (plot) {
        plot(spg.out, ...)
        return(invisible(spg.out))
    }
    else return(spg.out)
}


glm <- function (formula, family = gaussian, data, weights, subset, 
    na.action, start = NULL, etastart, mustart, offset, control = list(...), 
    model = TRUE, method = "glm.fit", x = FALSE, y = TRUE, contrasts = NULL, 
    ...) 
{
    call <- match.call()
    if (is.character(family)) 
        family <- get(family, mode = "function", envir = parent.frame())
    if (is.function(family)) 
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }
    if (missing(data)) 
        data <- environment(formula)
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "etastart", "mustart", "offset"), names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (identical(method, "model.frame")) 
        return(mf)
    if (!is.character(method) && !is.function(method)) 
        stop("invalid 'method' argument")
    if (identical(method, "glm.fit")) 
        control <- do.call("glm.control", control)
    mt <- attr(mf, "terms")
    Y <- model.response(mf, "any")
    if (length(dim(Y)) == 1L) {
        nm <- rownames(Y)
        dim(Y) <- NULL
        if (!is.null(nm)) 
            names(Y) <- nm
    }
    X <- if (!is.empty.model(mt)) 
        model.matrix(mt, mf, contrasts)
    else matrix(, NROW(Y), 0L)
    weights <- as.vector(model.weights(mf))
    if (!is.null(weights) && !is.numeric(weights)) 
        stop("'weights' must be a numeric vector")
    if (!is.null(weights) && any(weights < 0)) 
        stop("negative weights not allowed")
    offset <- as.vector(model.offset(mf))
    if (!is.null(offset)) {
        if (length(offset) != NROW(Y)) 
            stop(gettextf("number of offsets is %d should equal %d (number of observations)", 
                length(offset), NROW(Y)), domain = NA)
    }
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")
    fit <- eval(call(if (is.function(method)) "method" else method, 
        x = X, y = Y, weights = weights, start = start, etastart = etastart, 
        mustart = mustart, offset = offset, family = family, 
        control = control, intercept = attr(mt, "intercept") > 
            0L))
    if (length(offset) && attr(mt, "intercept") > 0L) {
        fit2 <- eval(call(if (is.function(method)) "method" else method, 
            x = X[, "(Intercept)", drop = FALSE], y = Y, weights = weights, 
            offset = offset, family = family, control = control, 
            intercept = TRUE))
        if (!fit2$converged) 
            warning("fitting to calculate the null deviance did not converge -- increase 'maxit'?")
        fit$null.deviance <- fit2$deviance
    }
    if (model) 
        fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if (x) 
        fit$x <- X
    if (!y) 
        fit$y <- NULL
    fit <- c(fit, list(call = call, formula = formula, terms = mt, 
        data = data, offset = offset, control = control, method = method, 
        contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, 
            mf)))
    class(fit) <- c(fit$class, c("glm", "lm"))
    fit
}


ls.diag <- function (ls.out) 
{
    resids <- as.matrix(ls.out$residuals)
    d0 <- dim(resids)
    xnames <- colnames(ls.out$qr$qr)
    yname <- colnames(resids)
    good <- complete.cases(resids, ls.out$wt)
    if (any(!good)) {
        warning("missing observations deleted")
        resids <- resids[good, , drop = FALSE]
    }
    if (!is.null(ls.out$wt)) {
        if (any(ls.out$wt[good] == 0)) 
            warning("observations with 0 weight not used in calculating standard deviation")
        resids <- resids * sqrt(ls.out$wt[good])
    }
    p <- ls.out$qr$rank
    n <- nrow(resids)
    hatdiag <- rep.int(NA, n)
    stats <- array(NA, dim = d0)
    colnames(stats) <- yname
    stdres <- studres <- dfits <- Cooks <- stats
    q <- qr.qy(ls.out$qr, rbind(diag(p), matrix(0, nrow = n - 
        p, ncol = p)))
    hatdiag[good] <- rowSums(as.matrix(q^2))
    stddev <- sqrt(colSums(as.matrix(resids^2))/(n - p))
    stddevmat <- matrix(stddev, nrow = sum(good), ncol = ncol(resids), 
        byrow = TRUE)
    stdres[good, ] <- resids/(sqrt(1 - hatdiag[good]) * stddevmat)
    studres[good, ] <- (stdres[good, ] * stddevmat)/sqrt(((n - 
        p) * stddevmat^2 - resids^2/(1 - hatdiag[good]))/(n - 
        p - 1))
    dfits[good, ] <- sqrt(hatdiag[good]/(1 - hatdiag[good])) * 
        studres[good, ]
    Cooks[good, ] <- ((stdres[good, ]^2 * hatdiag[good])/p)/(1 - 
        hatdiag[good])
    if (ncol(resids) == 1 && is.null(yname)) {
        stdres <- as.vector(stdres)
        Cooks <- as.vector(Cooks)
        studres <- as.vector(studres)
        dfits <- as.vector(dfits)
    }
    qr <- as.matrix(ls.out$qr$qr[1L:p, 1L:p])
    qr[row(qr) > col(qr)] <- 0
    qrinv <- solve(qr)
    covmat.unscaled <- qrinv %*% t(qrinv)
    dimnames(covmat.unscaled) <- list(xnames, xnames)
    covmat.scaled <- sum(stddev^2) * covmat.unscaled
    cormat <- covmat.scaled/sqrt(outer(diag(covmat.scaled), diag(covmat.scaled)))
    stderr <- outer(diag(covmat.unscaled)^0.5, stddev)
    dimnames(stderr) <- list(xnames, yname)
    return(list(std.dev = stddev, hat = hatdiag, std.res = stdres, 
        stud.res = studres, cooks = Cooks, dfits = dfits, correlation = cormat, 
        std.err = stderr, cov.scaled = covmat.scaled, cov.unscaled = covmat.unscaled))
}


pairwise.wilcox.test <- function (x, g, p.adjust.method = p.adjust.methods, paired = FALSE, 
    ...) 
{
    p.adjust.method <- match.arg(p.adjust.method)
    DNAME <- paste(deparse(substitute(x)), "and", deparse(substitute(g)))
    g <- factor(g)
    METHOD <- if (paired) 
        "Wilcoxon signed rank test"
    else "Wilcoxon rank sum test"
    compare.levels <- function(i, j) {
        xi <- x[as.integer(g) == i]
        xj <- x[as.integer(g) == j]
        wilcox.test(xi, xj, paired = paired, ...)$p.value
    }
    PVAL <- pairwise.table(compare.levels, levels(g), p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
        p.adjust.method = p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}


NLSstRtAsymptote <- function (xy) 
UseMethod("NLSstRtAsymptote")


pgamma <- function (q, shape, rate = 1, scale = 1/rate, lower.tail = TRUE, 
    log.p = FALSE) 
{
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    .Call(C_pgamma, q, shape, scale, lower.tail, log.p)
}


rpois <- function (n, lambda) 
.Call(C_rpois, n, lambda)


arima <- function (x, order = c(0L, 0L, 0L), seasonal = list(order = c(0L, 
    0L, 0L), period = NA), xreg = NULL, include.mean = TRUE, 
    transform.pars = TRUE, fixed = NULL, init = NULL, method = c("CSS-ML", 
        "ML", "CSS"), n.cond, SSinit = c("Gardner1980", "Rossignol2011"), 
    optim.method = "BFGS", optim.control = list(), kappa = 1e+06) 
{
    "%+%" <- function(a, b) .Call(C_TSconv, a, b)
    SSinit <- match.arg(SSinit)
    SS.G <- SSinit == "Gardner1980"
    upARIMA <- function(mod, phi, theta) {
        p <- length(phi)
        q <- length(theta)
        mod$phi <- phi
        mod$theta <- theta
        r <- max(p, q + 1L)
        if (p > 0) 
            mod$T[1L:p, 1L] <- phi
        if (r > 1L) 
            mod$Pn[1L:r, 1L:r] <- if (SS.G) 
                .Call(C_getQ0, phi, theta)
            else .Call(C_getQ0bis, phi, theta, tol = 0)
        else mod$Pn[1L, 1L] <- if (p > 0) 
            1/(1 - phi^2)
        else 1
        mod$a[] <- 0
        mod
    }
    arimaSS <- function(y, mod) {
        .Call(C_ARIMA_Like, y, mod, 0L, TRUE)
    }
    armafn <- function(p, trans) {
        par <- coef
        par[mask] <- p
        trarma <- .Call(C_ARIMA_transPars, par, arma, trans)
        if (is.null(Z <- tryCatch(upARIMA(mod, trarma[[1L]], 
            trarma[[2L]]), error = function(e) NULL))) 
            return(.Machine$double.xmax)
        if (ncxreg > 0) 
            x <- x - xreg %*% par[narma + (1L:ncxreg)]
        res <- .Call(C_ARIMA_Like, x, Z, 0L, FALSE)
        s2 <- res[1L]/res[3L]
        0.5 * (log(s2) + res[2L]/res[3L])
    }
    armaCSS <- function(p) {
        par <- as.double(fixed)
        par[mask] <- p
        trarma <- .Call(C_ARIMA_transPars, par, arma, FALSE)
        if (ncxreg > 0) 
            x <- x - xreg %*% par[narma + (1L:ncxreg)]
        res <- .Call(C_ARIMA_CSS, x, arma, trarma[[1L]], trarma[[2L]], 
            as.integer(ncond), FALSE)
        0.5 * log(res)
    }
    arCheck <- function(ar) {
        p <- max(which(c(1, -ar) != 0)) - 1
        if (!p) 
            return(TRUE)
        all(Mod(polyroot(c(1, -ar[1L:p]))) > 1)
    }
    maInvert <- function(ma) {
        q <- length(ma)
        q0 <- max(which(c(1, ma) != 0)) - 1L
        if (!q0) 
            return(ma)
        roots <- polyroot(c(1, ma[1L:q0]))
        ind <- Mod(roots) < 1
        if (all(!ind)) 
            return(ma)
        if (q0 == 1) 
            return(c(1/ma[1L], rep.int(0, q - q0)))
        roots[ind] <- 1/roots[ind]
        x <- 1
        for (r in roots) x <- c(x, 0) - c(0, x)/r
        c(Re(x[-1L]), rep.int(0, q - q0))
    }
    series <- deparse(substitute(x))
    if (NCOL(x) > 1L) 
        stop("only implemented for univariate time series")
    method <- match.arg(method)
    x <- as.ts(x)
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    storage.mode(x) <- "double"
    dim(x) <- NULL
    n <- length(x)
    if (!missing(order)) 
        if (!is.numeric(order) || length(order) != 3L || any(order < 
            0)) 
            stop("'order' must be a non-negative numeric vector of length 3")
    if (!missing(seasonal)) 
        if (is.list(seasonal)) {
            if (is.null(seasonal$order)) 
                stop("'seasonal' must be a list with component 'order'")
            if (!is.numeric(seasonal$order) || length(seasonal$order) != 
                3L || any(seasonal$order < 0L)) 
                stop("'seasonal$order' must be a non-negative numeric vector of length 3")
        }
        else if (is.numeric(order)) {
            if (length(order) == 3L) 
                seasonal <- list(order = seasonal)
            else ("'seasonal' is of the wrong length")
        }
        else stop("'seasonal' must be a list with component 'order'")
    if (is.null(seasonal$period) || is.na(seasonal$period) || 
        seasonal$period == 0) 
        seasonal$period <- frequency(x)
    arma <- as.integer(c(order[-2L], seasonal$order[-2L], seasonal$period, 
        order[2L], seasonal$order[2L]))
    narma <- sum(arma[1L:4L])
    xtsp <- tsp(x)
    tsp(x) <- NULL
    Delta <- 1
    for (i in seq_len(order[2L])) Delta <- Delta %+% c(1, -1)
    for (i in seq_len(seasonal$order[2L])) Delta <- Delta %+% 
        c(1, rep.int(0, seasonal$period - 1), -1)
    Delta <- -Delta[-1L]
    nd <- order[2L] + seasonal$order[2L]
    n.used <- sum(!is.na(x)) - length(Delta)
    if (is.null(xreg)) {
        ncxreg <- 0L
    }
    else {
        nmxreg <- deparse(substitute(xreg))
        if (NROW(xreg) != n) 
            stop("lengths of 'x' and 'xreg' do not match")
        ncxreg <- NCOL(xreg)
        xreg <- as.matrix(xreg)
        storage.mode(xreg) <- "double"
    }
    class(xreg) <- NULL
    if (ncxreg > 0L && is.null(colnames(xreg))) 
        colnames(xreg) <- if (ncxreg == 1L) 
            nmxreg
        else paste0(nmxreg, 1L:ncxreg)
    if (include.mean && (nd == 0L)) {
        xreg <- cbind(intercept = rep(1, n), xreg = xreg)
        ncxreg <- ncxreg + 1L
    }
    if (method == "CSS-ML") {
        anyna <- anyNA(x)
        if (ncxreg) 
            anyna <- anyna || anyNA(xreg)
        if (anyna) 
            method <- "ML"
    }
    if (method == "CSS" || method == "CSS-ML") {
        ncond <- order[2L] + seasonal$order[2L] * seasonal$period
        ncond1 <- order[1L] + seasonal$period * seasonal$order[1L]
        ncond <- ncond + if (!missing(n.cond)) 
            max(n.cond, ncond1)
        else ncond1
    }
    else ncond <- 0
    if (is.null(fixed)) 
        fixed <- rep(NA_real_, narma + ncxreg)
    else if (length(fixed) != narma + ncxreg) 
        stop("wrong length for 'fixed'")
    mask <- is.na(fixed)
    no.optim <- !any(mask)
    if (no.optim) 
        transform.pars <- FALSE
    if (transform.pars) {
        ind <- arma[1L] + arma[2L] + seq_len(arma[3L])
        if (any(!mask[seq_len(arma[1L])]) || any(!mask[ind])) {
            warning("some AR parameters were fixed: setting transform.pars = FALSE")
            transform.pars <- FALSE
        }
    }
    init0 <- rep.int(0, narma)
    parscale <- rep(1, narma)
    if (ncxreg) {
        cn <- colnames(xreg)
        orig.xreg <- (ncxreg == 1L) || any(!mask[narma + 1L:ncxreg])
        if (!orig.xreg) {
            S <- svd(na.omit(xreg))
            xreg <- xreg %*% S$v
        }
        dx <- x
        dxreg <- xreg
        if (order[2L] > 0L) {
            dx <- diff(dx, 1L, order[2L])
            dxreg <- diff(dxreg, 1L, order[2L])
        }
        if (seasonal$period > 1L & seasonal$order[2L] > 0) {
            dx <- diff(dx, seasonal$period, seasonal$order[2L])
            dxreg <- diff(dxreg, seasonal$period, seasonal$order[2L])
        }
        fit <- if (length(dx) > ncol(dxreg)) 
            lm(dx ~ dxreg - 1, na.action = na.omit)
        else list(rank = 0L)
        if (fit$rank == 0L) {
            fit <- lm(x ~ xreg - 1, na.action = na.omit)
        }
        isna <- is.na(x) | apply(xreg, 1L, anyNA)
        n.used <- sum(!isna) - length(Delta)
        init0 <- c(init0, coef(fit))
        ses <- summary(fit)$coefficients[, 2L]
        parscale <- c(parscale, 10 * ses)
    }
    if (n.used <= 0) 
        stop("too few non-missing observations")
    if (!is.null(init)) {
        if (length(init) != length(init0)) 
            stop("'init' is of the wrong length")
        if (any(ind <- is.na(init))) 
            init[ind] <- init0[ind]
        if (method == "ML") {
            if (arma[1L] > 0) 
                if (!arCheck(init[1L:arma[1L]])) 
                  stop("non-stationary AR part")
            if (arma[3L] > 0) 
                if (!arCheck(init[sum(arma[1L:2L]) + 1L:arma[3L]])) 
                  stop("non-stationary seasonal AR part")
            if (transform.pars) 
                init <- .Call(C_ARIMA_Invtrans, as.double(init), 
                  arma)
        }
    }
    else init <- init0
    coef <- as.double(fixed)
    if (!("parscale" %in% names(optim.control))) 
        optim.control$parscale <- parscale[mask]
    if (method == "CSS") {
        res <- if (no.optim) 
            list(convergence = 0L, par = numeric(), value = armaCSS(numeric()))
        else optim(init[mask], armaCSS, method = optim.method, 
            hessian = TRUE, control = optim.control)
        if (res$convergence > 0) 
            warning(gettextf("possible convergence problem: optim gave code = %d", 
                res$convergence), domain = NA)
        coef[mask] <- res$par
        trarma <- .Call(C_ARIMA_transPars, coef, arma, FALSE)
        mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa, 
            SSinit)
        if (ncxreg > 0) 
            x <- x - xreg %*% coef[narma + (1L:ncxreg)]
        arimaSS(x, mod)
        val <- .Call(C_ARIMA_CSS, x, arma, trarma[[1L]], trarma[[2L]], 
            as.integer(ncond), TRUE)
        sigma2 <- val[[1L]]
        var <- if (no.optim) 
            numeric()
        else solve(res$hessian * n.used)
    }
    else {
        if (method == "CSS-ML") {
            res <- if (no.optim) 
                list(convergence = 0L, par = numeric(), value = armaCSS(numeric()))
            else optim(init[mask], armaCSS, method = optim.method, 
                hessian = FALSE, control = optim.control)
            if (res$convergence == 0) 
                init[mask] <- res$par
            if (arma[1L] > 0) 
                if (!arCheck(init[1L:arma[1L]])) 
                  stop("non-stationary AR part from CSS")
            if (arma[3L] > 0) 
                if (!arCheck(init[sum(arma[1L:2L]) + 1L:arma[3L]])) 
                  stop("non-stationary seasonal AR part from CSS")
            ncond <- 0L
        }
        if (transform.pars) {
            init <- .Call(C_ARIMA_Invtrans, init, arma)
            if (arma[2L] > 0) {
                ind <- arma[1L] + 1L:arma[2L]
                init[ind] <- maInvert(init[ind])
            }
            if (arma[4L] > 0) {
                ind <- sum(arma[1L:3L]) + 1L:arma[4L]
                init[ind] <- maInvert(init[ind])
            }
        }
        trarma <- .Call(C_ARIMA_transPars, init, arma, transform.pars)
        mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa, 
            SSinit)
        res <- if (no.optim) 
            list(convergence = 0, par = numeric(), value = armafn(numeric(), 
                as.logical(transform.pars)))
        else optim(init[mask], armafn, method = optim.method, 
            hessian = TRUE, control = optim.control, trans = as.logical(transform.pars))
        if (res$convergence > 0) 
            warning(gettextf("possible convergence problem: optim gave code = %d", 
                res$convergence), domain = NA)
        coef[mask] <- res$par
        if (transform.pars) {
            if (arma[2L] > 0L) {
                ind <- arma[1L] + 1L:arma[2L]
                if (all(mask[ind])) 
                  coef[ind] <- maInvert(coef[ind])
            }
            if (arma[4L] > 0L) {
                ind <- sum(arma[1L:3L]) + 1L:arma[4L]
                if (all(mask[ind])) 
                  coef[ind] <- maInvert(coef[ind])
            }
            if (any(coef[mask] != res$par)) {
                oldcode <- res$convergence
                res <- optim(coef[mask], armafn, method = optim.method, 
                  hessian = TRUE, control = list(maxit = 0L, 
                    parscale = optim.control$parscale), trans = TRUE)
                res$convergence <- oldcode
                coef[mask] <- res$par
            }
            A <- .Call(C_ARIMA_Gradtrans, as.double(coef), arma)
            A <- A[mask, mask]
            var <- crossprod(A, solve(res$hessian * n.used, A))
            coef <- .Call(C_ARIMA_undoPars, coef, arma)
        }
        else var <- if (no.optim) 
            numeric()
        else solve(res$hessian * n.used)
        trarma <- .Call(C_ARIMA_transPars, coef, arma, FALSE)
        mod <- makeARIMA(trarma[[1L]], trarma[[2L]], Delta, kappa, 
            SSinit)
        val <- if (ncxreg > 0L) 
            arimaSS(x - xreg %*% coef[narma + (1L:ncxreg)], mod)
        else arimaSS(x, mod)
        sigma2 <- val[[1L]][1L]/n.used
    }
    value <- 2 * n.used * res$value + n.used + n.used * log(2 * 
        pi)
    aic <- if (method != "CSS") 
        value + 2 * sum(mask) + 2
    else NA
    nm <- NULL
    if (arma[1L] > 0L) 
        nm <- c(nm, paste0("ar", 1L:arma[1L]))
    if (arma[2L] > 0L) 
        nm <- c(nm, paste0("ma", 1L:arma[2L]))
    if (arma[3L] > 0L) 
        nm <- c(nm, paste0("sar", 1L:arma[3L]))
    if (arma[4L] > 0L) 
        nm <- c(nm, paste0("sma", 1L:arma[4L]))
    if (ncxreg > 0L) {
        nm <- c(nm, cn)
        if (!orig.xreg) {
            ind <- narma + 1L:ncxreg
            coef[ind] <- S$v %*% coef[ind]
            A <- diag(narma + ncxreg)
            A[ind, ind] <- S$v
            A <- A[mask, mask]
            var <- A %*% var %*% t(A)
        }
    }
    names(coef) <- nm
    if (!no.optim) 
        dimnames(var) <- list(nm[mask], nm[mask])
    resid <- val[[2L]]
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    structure(list(coef = coef, sigma2 = sigma2, var.coef = var, 
        mask = mask, loglik = -0.5 * value, aic = aic, arma = arma, 
        residuals = resid, call = match.call(), series = series, 
        code = res$convergence, n.cond = ncond, nobs = n.used, 
        model = mod), class = "Arima")
}


rgamma <- function (n, shape, rate = 1, scale = 1/rate) 
{
    if (!missing(rate) && !missing(scale)) {
        if (abs(rate * scale - 1) < 1e-15) 
            warning("specify 'rate' or 'scale' but not both")
        else stop("specify 'rate' or 'scale' but not both")
    }
    .Call(C_rgamma, n, shape, scale)
}


t.test <- function (x, ...) 
UseMethod("t.test")


biplot <- function (x, ...) 
UseMethod("biplot")


xtabs <- function (formula = ~., data = parent.frame(), subset, sparse = FALSE, 
    na.action, addNA = FALSE, exclude = if (!addNA) c(NA, NaN), 
    drop.unused.levels = FALSE) 
{
    if (missing(formula) && missing(data)) 
        stop("must supply either 'formula' or 'data'")
    if (!missing(formula)) {
        formula <- as.formula(formula)
        if (!inherits(formula, "formula")) 
            stop("'formula' missing or incorrect")
    }
    if (any(attr(terms(formula, data = data), "order") > 1)) 
        stop("interactions are not allowed")
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval(m$data, parent.frame()))) 
        m$data <- as.data.frame(data)
    m$... <- m$exclude <- m$drop.unused.levels <- m$sparse <- m$addNA <- NULL
    if (addNA && missing(na.action)) 
        m$na.action <- quote(na.pass)
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval(m, parent.frame())
    if (length(formula) == 2L) {
        by <- mf
        y <- NULL
    }
    else {
        i <- attr(attr(mf, "terms"), "response")
        by <- mf[-i]
        y <- mf[[i]]
    }
    has.exclude <- !missing(exclude)
    by <- lapply(by, function(u) {
        if (!is.factor(u)) 
            u <- factor(u, exclude = exclude)
        else if (has.exclude) 
            u <- factor(as.character(u), levels = setdiff(levels(u), 
                exclude), exclude = NULL)
        if (addNA) 
            u <- addNA(u, ifany = TRUE)
        u[, drop = drop.unused.levels]
    })
    naAct <- if (!is.null(m$na.action)) 
        m$na.action
    else getOption("na.action", default = quote(na.omit))
    na.rm <- identical(naAct, quote(na.omit)) || identical(naAct, 
        na.omit) || identical(naAct, "na.omit")
    if (!sparse) {
        x <- if (is.null(y)) 
            table(by, dnn = names(by))
        else if (NCOL(y) == 1L) 
            tapply(y, by, sum, na.rm = na.rm, default = 0L)
        else {
            z <- lapply(as.data.frame(y), tapply, by, sum, na.rm = na.rm, 
                default = 0L)
            array(unlist(z), dim = c(dim(z[[1L]]), length(z)), 
                dimnames = c(dimnames(z[[1L]]), list(names(z))))
        }
        class(x) <- c("xtabs", "table")
        attr(x, "call") <- match.call()
        x
    }
    else {
        if (length(by) != 2L) 
            stop(gettextf("%s applies only to two-way tables", 
                "xtabs(*, sparse=TRUE)"), domain = NA)
        if (is.null(tryCatch(loadNamespace("Matrix"), error = function(e) NULL))) 
            stop(gettextf("%s needs package 'Matrix' correctly installed", 
                "xtabs(*, sparse=TRUE)"), domain = NA)
        if (length(i.ex <- unique(unlist(lapply(by, function(f) which(is.na(f))))))) {
            by <- lapply(by, `[`, -i.ex)
            if (!is.null(y)) 
                y <- y[-i.ex]
        }
        if (na.rm && !is.null(y) && any(isN <- is.na(y))) {
            ok <- !isN
            by <- lapply(by, `[`, ok)
            y <- y[ok]
        }
        rows <- by[[1L]]
        cols <- by[[2L]]
        dnms <- lapply(by, levels)
        x <- if (is.null(y)) 
            rep.int(1, length(rows))
        else as.double(y)
        methods::as(methods::new("dgTMatrix", x = x, Dimnames = dnms, 
            i = as.integer(rows) - 1L, j = as.integer(cols) - 
                1L, Dim = lengths(dnms, use.names = FALSE)), 
            "CsparseMatrix")
    }
}


ts <- function (data = NA, start = 1, end = numeric(), frequency = 1, 
    deltat = 1, ts.eps = getOption("ts.eps"), class = if (nseries > 
        1) c("mts", "ts", "matrix") else "ts", names = if (!is.null(dimnames(data))) colnames(data) else paste("Series", 
        seq(nseries))) 
{
    if (is.data.frame(data)) 
        data <- data.matrix(data)
    if (is.matrix(data)) {
        nseries <- ncol(data)
        ndata <- nrow(data)
        dimnames(data) <- list(NULL, names)
    }
    else {
        nseries <- 1
        ndata <- length(data)
    }
    if (ndata == 0) 
        stop("'ts' object must have one or more observations")
    if (missing(frequency)) 
        frequency <- 1/deltat
    else if (missing(deltat)) 
        deltat <- 1/frequency
    if (frequency > 1 && abs(frequency - round(frequency)) < 
        ts.eps) 
        frequency <- round(frequency)
    if (length(start) > 1L) {
        start <- start[1L] + (start[2L] - 1)/frequency
    }
    if (length(end) > 1L) {
        end <- end[1L] + (end[2L] - 1)/frequency
    }
    if (missing(end)) 
        end <- start + (ndata - 1)/frequency
    else if (missing(start)) 
        start <- end - (ndata - 1)/frequency
    if (start > end) 
        stop("'start' cannot be after 'end'")
    nobs <- floor((end - start) * frequency + 1.01)
    if (nobs != ndata) 
        data <- if (NCOL(data) == 1) {
            if (ndata < nobs) 
                rep_len(data, nobs)
            else if (ndata > nobs) 
                data[1L:nobs]
        }
        else {
            if (ndata < nobs) 
                data[rep_len(1L:ndata, nobs), ]
            else if (ndata > nobs) 
                data[1L:nobs, ]
        }
    attr(data, "tsp") <- c(start, end, frequency)
    if (!is.null(class) && class != "none") 
        attr(data, "class") <- class
    data
}


SSasympOff <- function (input, Asym, lrc, c0) 
{
    .expr1 <- exp(lrc)
    .expr3 <- input - c0
    .expr5 <- exp(((-.expr1) * .expr3))
    .expr6 <- 1 - .expr5
    .value <- Asym * .expr6
    .actualArgs <- as.list(match.call()[c("Asym", "lrc", "c0")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .grad <- array(0, c(length(.value), 3L), list(NULL, c("Asym", 
            "lrc", "c0")))
        .grad[, "Asym"] <- .expr6
        .grad[, "lrc"] <- Asym * (.expr5 * (.expr1 * .expr3))
        .grad[, "c0"] <- -(Asym * (.expr5 * .expr1))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


is.empty.model <- function (x) 
{
    tt <- terms(x)
    (length(attr(tt, "factors")) == 0L) & (attr(tt, "intercept") == 
        0L)
}


summary.lm <- function (object, correlation = FALSE, symbolic.cor = FALSE, 
    ...) 
{
    z <- object
    p <- z$rank
    rdf <- z$df.residual
    if (p == 0) {
        r <- z$residuals
        n <- length(r)
        w <- z$weights
        if (is.null(w)) {
            rss <- sum(r^2)
        }
        else {
            rss <- sum(w * r^2)
            r <- sqrt(w) * r
        }
        resvar <- rss/rdf
        ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
        class(ans) <- "summary.lm"
        ans$aliased <- is.na(coef(object))
        ans$residuals <- r
        ans$df <- c(0L, n, length(ans$aliased))
        ans$coefficients <- matrix(NA, 0L, 4L)
        dimnames(ans$coefficients) <- list(NULL, c("Estimate", 
            "Std. Error", "t value", "Pr(>|t|)"))
        ans$sigma <- sqrt(resvar)
        ans$r.squared <- ans$adj.r.squared <- 0
        return(ans)
    }
    if (is.null(z$terms)) 
        stop("invalid 'lm' object:  no 'terms' component")
    if (!inherits(object, "lm")) 
        warning("calling summary.lm(<fake-lm-object>) ...")
    Qr <- qr.lm(object)
    n <- NROW(Qr$qr)
    if (is.na(z$df.residual) || n - p != z$df.residual) 
        warning("residual degrees of freedom in object suggest this is not an \"lm\" fit")
    r <- z$residuals
    f <- z$fitted.values
    w <- z$weights
    if (is.null(w)) {
        mss <- if (attr(z$terms, "intercept")) 
            sum((f - mean(f))^2)
        else sum(f^2)
        rss <- sum(r^2)
    }
    else {
        mss <- if (attr(z$terms, "intercept")) {
            m <- sum(w * f/sum(w))
            sum(w * (f - m)^2)
        }
        else sum(w * f^2)
        rss <- sum(w * r^2)
        r <- sqrt(w) * r
    }
    resvar <- rss/rdf
    if (is.finite(resvar) && resvar < (mean(f)^2 + var(f)) * 
        1e-30) 
        warning("essentially perfect fit: summary may be unreliable")
    p1 <- 1L:p
    R <- chol2inv(Qr$qr[p1, p1, drop = FALSE])
    se <- sqrt(diag(R) * resvar)
    est <- z$coefficients[Qr$pivot[p1]]
    tval <- est/se
    ans <- z[c("call", "terms", if (!is.null(z$weights)) "weights")]
    ans$residuals <- r
    ans$coefficients <- cbind(Estimate = est, `Std. Error` = se, 
        `t value` = tval, `Pr(>|t|)` = 2 * pt(abs(tval), rdf, 
            lower.tail = FALSE))
    ans$aliased <- is.na(z$coefficients)
    ans$sigma <- sqrt(resvar)
    ans$df <- c(p, rdf, NCOL(Qr$qr))
    if (p != attr(z$terms, "intercept")) {
        df.int <- if (attr(z$terms, "intercept")) 
            1L
        else 0L
        ans$r.squared <- mss/(mss + rss)
        ans$adj.r.squared <- 1 - (1 - ans$r.squared) * ((n - 
            df.int)/rdf)
        ans$fstatistic <- c(value = (mss/(p - df.int))/resvar, 
            numdf = p - df.int, dendf = rdf)
    }
    else ans$r.squared <- ans$adj.r.squared <- 0
    ans$cov.unscaled <- R
    dimnames(ans$cov.unscaled) <- dimnames(ans$coefficients)[c(1, 
        1)]
    if (correlation) {
        ans$correlation <- (R * resvar)/outer(se, se)
        dimnames(ans$correlation) <- dimnames(ans$cov.unscaled)
        ans$symbolic.cor <- symbolic.cor
    }
    if (!is.null(z$na.action)) 
        ans$na.action <- z$na.action
    class(ans) <- "summary.lm"
    ans
}


dummy.coef.lm <- function (object, use.na = FALSE, ...) 
{
    xl <- object$xlevels
    if (!length(xl)) 
        return(as.list(coef(object)))
    Terms <- terms(object)
    tl <- attr(Terms, "term.labels")
    int <- attr(Terms, "intercept")
    facs <- attr(Terms, "factors")[-1, , drop = FALSE]
    Terms <- delete.response(Terms)
    mf <- object$model
    if (is.null(mf)) 
        mf <- model.frame(object)
    vars <- dimnames(facs)[[1]]
    xtlv <- lapply(mf[, vars, drop = FALSE], levels)
    nxl <- pmax(lengths(xtlv), 1L)
    lterms <- apply(facs, 2L, function(x) prod(nxl[x > 0]))
    nl <- sum(lterms)
    args <- sapply(vars, function(i) if (nxl[i] == 1) 
        rep.int(1, nl)
    else factor(rep.int(xtlv[[i]][1L], nl), levels = xtlv[[i]]), 
        simplify = FALSE)
    dummy <- do.call(data.frame, args)
    names(dummy) <- vars
    pos <- 0L
    rn <- rep.int(tl, lterms)
    rnn <- character(nl)
    for (j in tl) {
        i <- vars[facs[, j] > 0]
        ifac <- i[nxl[i] > 1]
        lt.j <- lterms[[j]]
        if (length(ifac) == 0L) {
            rnn[pos + 1L] <- j
        }
        else {
            p.j <- pos + seq_len(lt.j)
            if (length(ifac) == 1L) {
                dummy[p.j, ifac] <- x.i <- xtlv[[ifac]]
                rnn[p.j] <- as.character(x.i)
            }
            else {
                tmp <- expand.grid(xtlv[ifac], KEEP.OUT.ATTRS = FALSE)
                dummy[p.j, ifac] <- tmp
                rnn[p.j] <- apply(as.matrix(tmp), 1L, paste, 
                  collapse = ":")
            }
        }
        pos <- pos + lt.j
    }
    attr(dummy, "terms") <- attr(mf, "terms")
    lcontr <- object$contrasts
    lci <- vapply(dummy, is.factor, NA)
    lcontr <- lcontr[names(lci)[lci]]
    mm <- model.matrix(Terms, dummy, lcontr, xl)
    if (anyNA(mm)) {
        warning("some terms will have NAs due to the limits of the method")
        mm[is.na(mm)] <- NA
    }
    coef <- object$coefficients
    if (!use.na) 
        coef[is.na(coef)] <- 0
    asgn <- attr(mm, "assign")
    res <- setNames(vector("list", length(tl)), tl)
    if (isM <- is.matrix(coef)) {
        for (j in seq_along(tl)) {
            keep <- which(asgn == j)
            cf <- coef[keep, , drop = FALSE]
            ij <- rn == tl[j]
            cf <- if (any(na <- is.na(cf))) {
                if (ncol(cf) >= 2) 
                  stop("multivariate case with missing coefficients is not yet implemented")
                rj <- t(mm[ij, keep[!na], drop = FALSE] %*% cf[!na])
                rj[apply(mm[ij, keep[na], drop = FALSE] != 0, 
                  1L, any)] <- NA
                rj
            }
            else t(mm[ij, keep, drop = FALSE] %*% cf)
            dimnames(cf) <- list(colnames(coef), rnn[ij])
            res[[j]] <- cf
        }
    }
    else {
        for (j in seq_along(tl)) {
            keep <- which(asgn == j)
            cf <- coef[keep]
            ij <- rn == tl[j]
            res[[j]] <- if (any(na <- is.na(cf))) {
                rj <- setNames(drop(mm[ij, keep[!na], drop = FALSE] %*% 
                  cf[!na]), rnn[ij])
                rj[apply(mm[ij, keep[na], drop = FALSE] != 0, 
                  1L, any)] <- NA
                rj
            }
            else setNames(drop(mm[ij, keep, drop = FALSE] %*% 
                cf), rnn[ij])
        }
    }
    if (int > 0) 
        res <- c(list(`(Intercept)` = if (isM) coef[int, ] else coef[int]), 
            res)
    structure(res, class = "dummy_coef", matrix = isM)
}


binom.test <- function (x, n, p = 0.5, alternative = c("two.sided", "less", 
    "greater"), conf.level = 0.95) 
{
    DNAME <- deparse(substitute(x))
    xr <- round(x)
    if (any(is.na(x) | (x < 0)) || max(abs(x - xr)) > 1e-07) 
        stop("'x' must be nonnegative and integer")
    x <- xr
    if (length(x) == 2L) {
        n <- sum(x)
        x <- x[1L]
    }
    else if (length(x) == 1L) {
        nr <- round(n)
        if ((length(n) > 1L) || is.na(n) || (n < 1) || abs(n - 
            nr) > 1e-07 || (x > nr)) 
            stop("'n' must be a positive integer >= 'x'")
        DNAME <- paste(DNAME, "and", deparse(substitute(n)))
        n <- nr
    }
    else stop("incorrect length of 'x'")
    if (!missing(p) && (length(p) > 1L || is.na(p) || p < 0 || 
        p > 1)) 
        stop("'p' must be a single number between 0 and 1")
    alternative <- match.arg(alternative)
    if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
        (conf.level > 0) && (conf.level < 1))) 
        stop("'conf.level' must be a single number between 0 and 1")
    PVAL <- switch(alternative, less = pbinom(x, n, p), greater = pbinom(x - 
        1, n, p, lower.tail = FALSE), two.sided = {
        if (p == 0) (x == 0) else if (p == 1) (x == n) else {
            relErr <- 1 + 1e-07
            d <- dbinom(x, n, p)
            m <- n * p
            if (x == m) 1 else if (x < m) {
                i <- seq.int(from = ceiling(m), to = n)
                y <- sum(dbinom(i, n, p) <= d * relErr)
                pbinom(x, n, p) + pbinom(n - y, n, p, lower.tail = FALSE)
            } else {
                i <- seq.int(from = 0, to = floor(m))
                y <- sum(dbinom(i, n, p) <= d * relErr)
                pbinom(y - 1, n, p) + pbinom(x - 1, n, p, lower.tail = FALSE)
            }
        }
    })
    p.L <- function(x, alpha) {
        if (x == 0) 
            0
        else qbeta(alpha, x, n - x + 1)
    }
    p.U <- function(x, alpha) {
        if (x == n) 
            1
        else qbeta(1 - alpha, x + 1, n - x)
    }
    CINT <- switch(alternative, less = c(0, p.U(x, 1 - conf.level)), 
        greater = c(p.L(x, 1 - conf.level), 1), two.sided = {
            alpha <- (1 - conf.level)/2
            c(p.L(x, alpha), p.U(x, alpha))
        })
    attr(CINT, "conf.level") <- conf.level
    ESTIMATE <- x/n
    names(x) <- "number of successes"
    names(n) <- "number of trials"
    names(ESTIMATE) <- names(p) <- "probability of success"
    structure(list(statistic = x, parameter = n, p.value = PVAL, 
        conf.int = CINT, estimate = ESTIMATE, null.value = p, 
        alternative = alternative, method = "Exact binomial test", 
        data.name = DNAME), class = "htest")
}


sigma <- function (object, ...) 
UseMethod("sigma")


prop.test <- function (x, n, p = NULL, alternative = c("two.sided", "less", 
    "greater"), conf.level = 0.95, correct = TRUE) 
{
    DNAME <- deparse(substitute(x))
    if (is.table(x) && length(dim(x)) == 1L) {
        if (dim(x) != 2L) 
            stop("table 'x' should have 2 entries")
        l <- 1
        n <- sum(x)
        x <- x[1L]
    }
    else if (is.matrix(x)) {
        if (ncol(x) != 2L) 
            stop("'x' must have 2 columns")
        l <- nrow(x)
        n <- rowSums(x)
        x <- x[, 1L]
    }
    else {
        DNAME <- paste(DNAME, "out of", deparse(substitute(n)))
        if ((l <- length(x)) != length(n)) 
            stop("'x' and 'n' must have the same length")
    }
    OK <- complete.cases(x, n)
    x <- x[OK]
    n <- n[OK]
    if ((k <- length(x)) < 1L) 
        stop("not enough data")
    if (any(n <= 0)) 
        stop("elements of 'n' must be positive")
    if (any(x < 0)) 
        stop("elements of 'x' must be nonnegative")
    if (any(x > n)) 
        stop("elements of 'x' must not be greater than those of 'n'")
    if (is.null(p) && (k == 1)) 
        p <- 0.5
    if (!is.null(p)) {
        DNAME <- paste0(DNAME, ", null ", if (k == 1) 
            "probability "
        else "probabilities ", deparse(substitute(p)))
        if (length(p) != l) 
            stop("'p' must have the same length as 'x' and 'n'")
        p <- p[OK]
        if (any((p <= 0) | (p >= 1))) 
            stop("elements of 'p' must be in (0,1)")
    }
    alternative <- match.arg(alternative)
    if (k > 2 || (k == 2) && !is.null(p)) 
        alternative <- "two.sided"
    if ((length(conf.level) != 1L) || is.na(conf.level) || (conf.level <= 
        0) || (conf.level >= 1)) 
        stop("'conf.level' must be a single number between 0 and 1")
    correct <- as.logical(correct)
    ESTIMATE <- setNames(x/n, if (k == 1) 
        "p"
    else paste("prop", 1L:l)[OK])
    NVAL <- p
    CINT <- NULL
    YATES <- if (correct && (k <= 2)) 
        0.5
    else 0
    if (k == 1) {
        z <- qnorm(if (alternative == "two.sided") 
            (1 + conf.level)/2
        else conf.level)
        YATES <- min(YATES, abs(x - n * p))
        z22n <- z^2/(2 * n)
        p.c <- ESTIMATE + YATES/n
        p.u <- if (p.c >= 1) 
            1
        else (p.c + z22n + z * sqrt(p.c * (1 - p.c)/n + z22n/(2 * 
            n)))/(1 + 2 * z22n)
        p.c <- ESTIMATE - YATES/n
        p.l <- if (p.c <= 0) 
            0
        else (p.c + z22n - z * sqrt(p.c * (1 - p.c)/n + z22n/(2 * 
            n)))/(1 + 2 * z22n)
        CINT <- switch(alternative, two.sided = c(max(p.l, 0), 
            min(p.u, 1)), greater = c(max(p.l, 0), 1), less = c(0, 
            min(p.u, 1)))
    }
    else if ((k == 2) & is.null(p)) {
        DELTA <- ESTIMATE[1L] - ESTIMATE[2L]
        YATES <- min(YATES, abs(DELTA)/sum(1/n))
        WIDTH <- (switch(alternative, two.sided = qnorm((1 + 
            conf.level)/2), qnorm(conf.level)) * sqrt(sum(ESTIMATE * 
            (1 - ESTIMATE)/n)) + YATES * sum(1/n))
        CINT <- switch(alternative, two.sided = c(max(DELTA - 
            WIDTH, -1), min(DELTA + WIDTH, 1)), greater = c(max(DELTA - 
            WIDTH, -1), 1), less = c(-1, min(DELTA + WIDTH, 1)))
    }
    if (!is.null(CINT)) 
        attr(CINT, "conf.level") <- conf.level
    METHOD <- paste(if (k == 1) 
        "1-sample proportions test"
    else paste0(k, "-sample test for ", if (is.null(p)) 
        "equality of"
    else "given", " proportions"), if (YATES) 
        "with"
    else "without", "continuity correction")
    if (is.null(p)) {
        p <- sum(x)/sum(n)
        PARAMETER <- k - 1
    }
    else {
        PARAMETER <- k
        names(NVAL) <- names(ESTIMATE)
    }
    names(PARAMETER) <- "df"
    x <- cbind(x, n - x)
    E <- cbind(n * p, n * (1 - p))
    if (any(E < 5)) 
        warning("Chi-squared approximation may be incorrect")
    STATISTIC <- sum((abs(x - E) - YATES)^2/E)
    names(STATISTIC) <- "X-squared"
    if (alternative == "two.sided") 
        PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
    else {
        if (k == 1) 
            z <- sign(ESTIMATE - p) * sqrt(STATISTIC)
        else z <- sign(DELTA) * sqrt(STATISTIC)
        PVAL <- pnorm(z, lower.tail = (alternative == "less"))
    }
    RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
        p.value = as.numeric(PVAL), estimate = ESTIMATE, null.value = NVAL, 
        conf.int = CINT, alternative = alternative, method = METHOD, 
        data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}


na.pass <- function (object, ...) 
object


make.link <- function (link) 
{
    switch(link, logit = {
        linkfun <- function(mu) .Call(C_logit_link, mu)
        linkinv <- function(eta) .Call(C_logit_linkinv, eta)
        mu.eta <- function(eta) .Call(C_logit_mu_eta, eta)
        valideta <- function(eta) TRUE
    }, probit = {
        linkfun <- function(mu) qnorm(mu)
        linkinv <- function(eta) {
            thresh <- -qnorm(.Machine$double.eps)
            eta <- pmin(pmax(eta, -thresh), thresh)
            pnorm(eta)
        }
        mu.eta <- function(eta) pmax(dnorm(eta), .Machine$double.eps)
        valideta <- function(eta) TRUE
    }, cauchit = {
        linkfun <- function(mu) qcauchy(mu)
        linkinv <- function(eta) {
            thresh <- -qcauchy(.Machine$double.eps)
            eta <- pmin(pmax(eta, -thresh), thresh)
            pcauchy(eta)
        }
        mu.eta <- function(eta) pmax(dcauchy(eta), .Machine$double.eps)
        valideta <- function(eta) TRUE
    }, cloglog = {
        linkfun <- function(mu) log(-log(1 - mu))
        linkinv <- function(eta) pmax(pmin(-expm1(-exp(eta)), 
            1 - .Machine$double.eps), .Machine$double.eps)
        mu.eta <- function(eta) {
            eta <- pmin(eta, 700)
            pmax(exp(eta) * exp(-exp(eta)), .Machine$double.eps)
        }
        valideta <- function(eta) TRUE
    }, identity = {
        linkfun <- function(mu) mu
        linkinv <- function(eta) eta
        mu.eta <- function(eta) rep.int(1, length(eta))
        valideta <- function(eta) TRUE
    }, log = {
        linkfun <- function(mu) log(mu)
        linkinv <- function(eta) pmax(exp(eta), .Machine$double.eps)
        mu.eta <- function(eta) pmax(exp(eta), .Machine$double.eps)
        valideta <- function(eta) TRUE
    }, sqrt = {
        linkfun <- function(mu) sqrt(mu)
        linkinv <- function(eta) eta^2
        mu.eta <- function(eta) 2 * eta
        valideta <- function(eta) all(is.finite(eta)) && all(eta > 
            0)
    }, `1/mu^2` = {
        linkfun <- function(mu) 1/mu^2
        linkinv <- function(eta) 1/sqrt(eta)
        mu.eta <- function(eta) -1/(2 * eta^1.5)
        valideta <- function(eta) all(is.finite(eta)) && all(eta > 
            0)
    }, inverse = {
        linkfun <- function(mu) 1/mu
        linkinv <- function(eta) 1/eta
        mu.eta <- function(eta) -1/(eta^2)
        valideta <- function(eta) all(is.finite(eta)) && all(eta != 
            0)
    }, stop(gettextf("%s link not recognised", sQuote(link)), 
        domain = NA))
    environment(linkfun) <- environment(linkinv) <- environment(mu.eta) <- environment(valideta) <- asNamespace("stats")
    structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, 
        valideta = valideta, name = link), class = "link-glm")
}


summary.stepfun <- function (object, ...) 
{
    n <- length(eval(expression(x), envir = environment(object)))
    if (!is.integer(n) || n < 1L) 
        stop("not a valid step function")
    cat("Step function with continuity 'f'=", format(eval(expression(f), 
        envir = environment(object))), ", ", n, if (n <= 6L) 
        "knots at\n"
    else "knots with summary\n")
    summ <- if (n > 6L) 
        summary
    else function(x) x
    print(summ(knots(object)))
    cat(if (n > 6L) 
        "\n"
    else "  ", "and\t", n + 1L, " plateau levels (y) ", if (n <= 
        6L) 
        "at\n"
    else "with summary\n", sep = "")
    print(summ(eval(expression(c(yleft, y)), envir = environment(object))))
    invisible()
}


reshape <- function (data, varying = NULL, v.names = NULL, timevar = "time", 
    idvar = "id", ids = 1L:NROW(data), times = seq_along(varying[[1L]]), 
    drop = NULL, direction, new.row.names = NULL, sep = ".", 
    split = if (sep == "") {
        list(regexp = "[A-Za-z][0-9]", include = TRUE)
    } else {
        list(regexp = sep, include = FALSE, fixed = TRUE)
    }) 
{
    if (!is.character(sep) || length(sep) != 1L) 
        stop("'sep' must be a character string")
    ix2names <- function(ix) if (is.character(ix)) 
        ix
    else names(data)[ix]
    guess <- function(nms, re = split$regexp, drop = !split$include, 
        fixed = if (is.null(split$fixed)) 
            FALSE
        else split$fixed) {
        if (drop) 
            nn <- do.call("rbind", strsplit(nms, re, fixed = fixed))
        else nn <- cbind(substr(nms, 1L, regexpr(re, nms)), substr(nms, 
            regexpr(re, nms) + 1L, 10000L))
        if (ncol(nn) != 2L) 
            stop("failed to guess time-varying variables from their names")
        vn <- unique(nn[, 1])
        v.names <- split(nms, factor(nn[, 1L], levels = vn))
        times <- unique(nn[, 2L])
        attr(v.names, "v.names") <- vn
        tt <- tryCatch(as.numeric(times), warning = function(w) times)
        attr(v.names, "times") <- tt
        v.names
    }
    reshapeLong <- function(data, varying, v.names = NULL, timevar, 
        idvar, ids = 1L:NROW(data), times, drop = NULL, new.row.names = NULL) {
        ll <- unlist(lapply(varying, length))
        if (any(ll != ll[1L])) 
            stop("'varying' arguments must be the same length")
        if (ll[1L] != length(times)) 
            stop("'lengths(varying)' must all match 'length(times)'")
        if (!is.null(drop)) {
            if (is.character(drop)) 
                drop <- names(data) %in% drop
            data <- data[, if (is.logical(drop)) 
                !drop
            else -drop, drop = FALSE]
        }
        undoInfo <- list(varying = varying, v.names = v.names, 
            idvar = idvar, timevar = timevar)
        if (length(idvar) > 1L) {
            ids <- interaction(data[, idvar], drop = TRUE)
        }
        else if (idvar %in% names(data)) {
            ids <- data[, idvar]
        }
        d <- data
        all.varying <- unlist(varying)
        d <- d[, !(names(data) %in% all.varying), drop = FALSE]
        if (is.null(v.names)) 
            v.names <- vapply(varying, `[`, 1L, FUN.VALUE = character(1L))
        rval <- do.call(rbind, lapply(seq_along(times), function(i) {
            d[, timevar] <- times[i]
            varying.i <- vapply(varying, `[`, i, FUN.VALUE = character(1L))
            d[, v.names] <- data[, varying.i]
            if (is.null(new.row.names)) 
                row.names(d) <- paste(ids, times[i], sep = ".")
            else row.names(d) <- new.row.names[(i - 1L) * NROW(d) + 
                1L:NROW(d)]
            d
        }))
        if (length(idvar) == 1L && !(idvar %in% names(data))) {
            rval[, idvar] <- ids
        }
        attr(rval, "reshapeLong") <- undoInfo
        return(rval)
    }
    reshapeWide <- function(data, timevar, idvar, varying = NULL, 
        v.names = NULL, drop = NULL, new.row.names = NULL) {
        if (!is.null(drop)) {
            if (is.character(drop)) 
                drop <- names(data) %in% drop
            data <- data[, if (is.logical(drop)) 
                !drop
            else -drop, drop = FALSE]
        }
        undoInfo <- list(v.names = v.names, timevar = timevar, 
            idvar = idvar)
        orig.idvar <- idvar
        if (length(idvar) > 1L) {
            repeat ({
                tempidname <- basename(tempfile("tempID"))
                if (!(tempidname %in% names(data))) 
                  break
            })
            data[, tempidname] <- interaction(data[, idvar], 
                drop = TRUE)
            idvar <- tempidname
            drop.idvar <- TRUE
        }
        else drop.idvar <- FALSE
        times <- unique(data[, timevar])
        if (anyNA(times)) 
            warning("there are records with missing times, which will be dropped.")
        undoInfo$times <- times
        if (is.null(v.names)) 
            v.names <- names(data)[!(names(data) %in% c(timevar, 
                idvar, orig.idvar))]
        if (is.null(varying)) 
            varying <- outer(v.names, times, paste, sep = sep)
        if (is.list(varying)) 
            varying <- do.call("rbind", varying)
        undoInfo$varying <- varying
        keep <- !(names(data) %in% c(timevar, v.names, idvar, 
            orig.idvar))
        if (any(keep)) {
            rval <- data[keep]
            tmp <- data[, idvar]
            really.constant <- unlist(lapply(rval, function(a) all(tapply(a, 
                as.vector(tmp), function(b) length(unique(b)) == 
                  1L))))
            if (!all(really.constant)) 
                warning(gettextf("some constant variables (%s) are really varying", 
                  paste(names(rval)[!really.constant], collapse = ",")), 
                  domain = NA)
        }
        rval <- data[!duplicated(data[, idvar]), !(names(data) %in% 
            c(timevar, v.names)), drop = FALSE]
        for (i in seq_along(times)) {
            thistime <- data[data[, timevar] %in% times[i], ]
            tab <- table(thistime[, idvar])
            if (any(tab > 1L)) 
                warning(sprintf("multiple rows match for %s=%s: first taken", 
                  timevar, times[i]), domain = NA)
            rval[, varying[, i]] <- thistime[match(rval[, idvar], 
                thistime[, idvar]), v.names]
        }
        if (!is.null(new.row.names)) 
            row.names(rval) <- new.row.names
        if (drop.idvar) 
            rval[, idvar] <- NULL
        attr(rval, "reshapeWide") <- undoInfo
        rval
    }
    if (missing(direction)) {
        undo <- c("wide", "long")[c("reshapeLong", "reshapeWide") %in% 
            names(attributes(data))]
        if (length(undo) == 1L) 
            direction <- undo
    }
    direction <- match.arg(direction, c("wide", "long"))
    switch(direction, wide = {
        back <- attr(data, "reshapeLong")
        if (missing(timevar) && missing(idvar) && !is.null(back)) {
            reshapeWide(data, idvar = back$idvar, timevar = back$timevar, 
                varying = back$varying, v.names = back$v.names, 
                new.row.names = new.row.names)
        } else {
            reshapeWide(data, idvar = idvar, timevar = timevar, 
                varying = varying, v.names = v.names, drop = drop, 
                new.row.names = new.row.names)
        }
    }, long = {
        if (missing(varying)) {
            back <- attr(data, "reshapeWide")
            if (is.null(back)) stop("no 'reshapeWide' attribute, must specify 'varying'")
            varying <- back$varying
            idvar <- back$idvar
            timevar <- back$timevar
            v.names <- back$v.names
            times <- back$times
        }
        if (is.matrix(varying)) {
            varying <- split(c(varying), row(varying))
        }
        if (is.null(varying)) stop("'varying' must be nonempty list or vector")
        if (is.atomic(varying)) {
            varying <- ix2names(varying)
            if (missing(v.names)) varying <- guess(varying) else {
                if (length(varying)%%length(v.names)) stop("length of 'v.names' does not evenly divide length of 'varying'")
                ntimes <- length(varying)%/%length(v.names)
                if (missing(times)) times <- seq_len(ntimes) else if (length(times) != 
                  ntimes) stop("length of 'varying' must be the product of length of 'v.names' and length of 'times'")
                varying <- split(varying, rep(v.names, ntimes))
                attr(varying, "v.names") <- v.names
                attr(varying, "times") <- times
            }
        } else varying <- lapply(varying, ix2names)
        if (missing(v.names) && !is.null(attr(varying, "v.names"))) {
            v.names <- attr(varying, "v.names")
            times <- attr(varying, "times")
        }
        reshapeLong(data, idvar = idvar, timevar = timevar, varying = varying, 
            v.names = v.names, drop = drop, times = times, ids = ids, 
            new.row.names = new.row.names)
    })
}


ftable <- function (x, ...) 
UseMethod("ftable")


density.default <- function (x, bw = "nrd0", adjust = 1, kernel = c("gaussian", 
    "epanechnikov", "rectangular", "triangular", "biweight", 
    "cosine", "optcosine"), weights = NULL, window = kernel, 
    width, give.Rkern = FALSE, n = 512, from, to, cut = 3, na.rm = FALSE, 
    ...) 
{
    chkDots(...)
    if (!missing(window) && missing(kernel)) 
        kernel <- window
    kernel <- match.arg(kernel)
    if (give.Rkern) 
        return(switch(kernel, gaussian = 1/(2 * sqrt(pi)), rectangular = sqrt(3)/6, 
            triangular = sqrt(6)/9, epanechnikov = 3/(5 * sqrt(5)), 
            biweight = 5 * sqrt(7)/49, cosine = 3/4 * sqrt(1/3 - 
                2/pi^2), optcosine = sqrt(1 - 8/pi^2) * pi^2/16))
    if (!is.numeric(x)) 
        stop("argument 'x' must be numeric")
    name <- deparse(substitute(x))
    x <- as.vector(x)
    x.na <- is.na(x)
    if (any(x.na)) {
        if (na.rm) 
            x <- x[!x.na]
        else stop("'x' contains missing values")
    }
    N <- nx <- as.integer(length(x))
    if (is.na(N)) 
        stop(gettextf("invalid value of %s", "length(x)"), domain = NA)
    x.finite <- is.finite(x)
    if (any(!x.finite)) {
        x <- x[x.finite]
        nx <- length(x)
    }
    if (is.null(weights)) {
        weights <- rep.int(1/nx, nx)
        totMass <- nx/N
    }
    else {
        if (length(weights) != N) 
            stop("'x' and 'weights' have unequal length")
        if (!all(is.finite(weights))) 
            stop("'weights' must all be finite")
        if (any(weights < 0)) 
            stop("'weights' must not be negative")
        wsum <- sum(weights)
        if (any(!x.finite)) {
            weights <- weights[x.finite]
            totMass <- sum(weights)/wsum
        }
        else totMass <- 1
        if (!isTRUE(all.equal(1, wsum))) 
            warning("sum(weights) != 1  -- will not get true density")
    }
    n.user <- n
    n <- max(n, 512)
    if (n > 512) 
        n <- 2^ceiling(log2(n))
    if (missing(bw) && !missing(width)) {
        if (is.numeric(width)) {
            fac <- switch(kernel, gaussian = 4, rectangular = 2 * 
                sqrt(3), triangular = 2 * sqrt(6), epanechnikov = 2 * 
                sqrt(5), biweight = 2 * sqrt(7), cosine = 2/sqrt(1/3 - 
                2/pi^2), optcosine = 2/sqrt(1 - 8/pi^2))
            bw <- width/fac
        }
        if (is.character(width)) 
            bw <- width
    }
    if (is.character(bw)) {
        if (nx < 2) 
            stop("need at least 2 points to select a bandwidth automatically")
        bw <- switch(tolower(bw), nrd0 = bw.nrd0(x), nrd = bw.nrd(x), 
            ucv = bw.ucv(x), bcv = bw.bcv(x), sj = , `sj-ste` = bw.SJ(x, 
                method = "ste"), `sj-dpi` = bw.SJ(x, method = "dpi"), 
            stop("unknown bandwidth rule"))
    }
    if (!is.finite(bw)) 
        stop("non-finite 'bw'")
    bw <- adjust * bw
    if (bw <= 0) 
        stop("'bw' is not positive.")
    if (missing(from)) 
        from <- min(x) - cut * bw
    if (missing(to)) 
        to <- max(x) + cut * bw
    if (!is.finite(from)) 
        stop("non-finite 'from'")
    if (!is.finite(to)) 
        stop("non-finite 'to'")
    lo <- from - 4 * bw
    up <- to + 4 * bw
    y <- .Call(C_BinDist, x, weights, lo, up, n) * totMass
    kords <- seq.int(0, 2 * (up - lo), length.out = 2L * n)
    kords[(n + 2):(2 * n)] <- -kords[n:2]
    kords <- switch(kernel, gaussian = dnorm(kords, sd = bw), 
        rectangular = {
            a <- bw * sqrt(3)
            ifelse(abs(kords) < a, 0.5/a, 0)
        }, triangular = {
            a <- bw * sqrt(6)
            ax <- abs(kords)
            ifelse(ax < a, (1 - ax/a)/a, 0)
        }, epanechnikov = {
            a <- bw * sqrt(5)
            ax <- abs(kords)
            ifelse(ax < a, 3/4 * (1 - (ax/a)^2)/a, 0)
        }, biweight = {
            a <- bw * sqrt(7)
            ax <- abs(kords)
            ifelse(ax < a, 15/16 * (1 - (ax/a)^2)^2/a, 0)
        }, cosine = {
            a <- bw/sqrt(1/3 - 2/pi^2)
            ifelse(abs(kords) < a, (1 + cos(pi * kords/a))/(2 * 
                a), 0)
        }, optcosine = {
            a <- bw/sqrt(1 - 8/pi^2)
            ifelse(abs(kords) < a, pi/4 * cos(pi * kords/(2 * 
                a))/a, 0)
        })
    kords <- fft(fft(y) * Conj(fft(kords)), inverse = TRUE)
    kords <- pmax.int(0, Re(kords)[1L:n]/length(y))
    xords <- seq.int(lo, up, length.out = n)
    x <- seq.int(from, to, length.out = n.user)
    structure(list(x = x, y = approx(xords, kords, x)$y, bw = bw, 
        n = N, call = match.call(), data.name = name, has.na = FALSE), 
        class = "density")
}


dfbetas <- function (model, ...) 
UseMethod("dfbetas")


contrasts <- function (x, contrasts = TRUE, sparse = FALSE) 
{
    if (is.logical(x)) 
        x <- factor(x, levels = c(FALSE, TRUE))
    if (!is.factor(x)) 
        stop("contrasts apply only to factors")
    if (!contrasts) 
        return(.Diag(levels(x), sparse = sparse))
    ctr <- attr(x, "contrasts")
    if ((NL <- is.null(ctr)) || is.character(ctr)) {
        if (NL) 
            ctr <- getOption("contrasts")[[if (is.ordered(x)) 
                2L
            else 1L]]
        ctrfn <- get(ctr, mode = "function", envir = parent.frame())
        if (useSparse <- isTRUE(sparse)) {
            if (!(useSparse <- any("sparse" == names(formals(ctrfn))))) 
                warning(sprintf("contrast function '%s' does not support 'sparse = TRUE'", 
                  ctr), domain = NA)
        }
        ctr <- if (useSparse) 
            ctrfn(levels(x), contrasts = contrasts, sparse = sparse)
        else ctrfn(levels(x), contrasts = contrasts)
    }
    ctr
}


psignrank <- function (q, n, lower.tail = TRUE, log.p = FALSE) 
{
    on.exit(.External(C_signrank_free))
    .Call(C_psignrank, q, n, lower.tail, log.p)
}


polym <- function (..., degree = 1, coefs = NULL, raw = FALSE) 
{
    dots <- list(...)
    nd <- length(if (is.null(coefs)) dots else coefs)
    if (nd == 0) 
        stop("must supply one or more vectors")
    z <- do.call(expand.grid, c(rep.int(list(0:degree), nd), 
        KEEP.OUT.ATTRS = FALSE))
    s <- rowSums(z)
    ind <- 0 < s & s <= degree
    z <- z[ind, , drop = FALSE]
    s <- s[ind]
    if (is.null(coefs)) {
        aPoly <- poly(dots[[1L]], degree, raw = raw, simple = raw && 
            nd > 1)
        if (nd == 1) 
            return(aPoly)
        n <- lengths(dots)
        if (any(n != n[1L])) 
            stop("arguments must have the same length")
        res <- cbind(1, aPoly)[, 1L + z[, 1]]
        if (!raw) 
            coefs <- list(attr(aPoly, "coefs"))
        for (i in 2:nd) {
            aPoly <- poly(dots[[i]], degree, raw = raw, simple = raw)
            res <- res * cbind(1, aPoly)[, 1L + z[, i]]
            if (!raw) 
                coefs <- c(coefs, list(attr(aPoly, "coefs")))
        }
        colnames(res) <- apply(z, 1L, function(x) paste(x, collapse = "."))
        structure(res, degree = as.vector(s), coefs = if (!raw) 
            coefs, class = c("poly", "matrix"))
    }
    else {
        newdata <- as.data.frame(dots)
        if (nd != ncol(newdata)) 
            stop("wrong number of columns in new data: ", deparse(substitute(...)))
        res <- cbind(1, poly(newdata[[1]], degree = degree, coefs = coefs[[1]], 
            simple = TRUE))[, 1L + z[, 1]]
        if (nd > 1) 
            for (i in 2:nd) res <- res * cbind(1, poly(newdata[[i]], 
                degree = degree, coefs = coefs[[i]], simple = TRUE))[, 
                1L + z[, i]]
        colnames(res) <- apply(z, 1L, function(x) paste(x, collapse = "."))
        res
    }
}


model.frame.default <- function (formula, data = NULL, subset = NULL, na.action = na.fail, 
    drop.unused.levels = FALSE, xlev = NULL, ...) 
{
    possible_newdata <- !missing(data) && is.data.frame(data) && 
        identical(substitute(data), quote(newdata)) && (nr <- nrow(data)) > 
        0
    if (!missing(formula) && nargs() == 1 && is.list(formula) && 
        !is.null(m <- formula$model)) 
        return(m)
    if (!missing(formula) && nargs() == 1 && is.list(formula) && 
        all(c("terms", "call") %in% names(formula))) {
        fcall <- formula$call
        m <- match(c("formula", "data", "subset", "weights", 
            "na.action"), names(fcall), 0)
        fcall <- fcall[c(1, m)]
        fcall[[1L]] <- quote(stats::model.frame)
        env <- environment(formula$terms)
        if (is.null(env)) 
            env <- parent.frame()
        return(eval(fcall, env))
    }
    if (missing(formula)) {
        if (!missing(data) && inherits(data, "data.frame") && 
            length(attr(data, "terms"))) 
            return(data)
        formula <- as.formula(data)
    }
    else if (missing(data) && inherits(formula, "data.frame")) {
        if (length(attr(formula, "terms"))) 
            return(formula)
        data <- formula
        formula <- as.formula(data)
    }
    formula <- as.formula(formula)
    if (missing(na.action)) {
        if (!is.null(naa <- attr(data, "na.action")) & mode(naa) != 
            "numeric") 
            na.action <- naa
        else if (!is.null(naa <- getOption("na.action"))) 
            na.action <- naa
    }
    if (missing(data)) 
        data <- environment(formula)
    else if (!is.data.frame(data) && !is.environment(data) && 
        !is.null(attr(data, "class"))) 
        data <- as.data.frame(data)
    else if (is.array(data)) 
        stop("'data' must be a data.frame, not a matrix or an array")
    if (!inherits(formula, "terms")) 
        formula <- terms(formula, data = data)
    env <- environment(formula)
    rownames <- .row_names_info(data, 0L)
    vars <- attr(formula, "variables")
    predvars <- attr(formula, "predvars")
    if (is.null(predvars)) 
        predvars <- vars
    varnames <- sapply(vars, function(x) paste(deparse(x, width.cutoff = 500), 
        collapse = " "))[-1L]
    variables <- eval(predvars, data, env)
    resp <- attr(formula, "response")
    if (is.null(rownames) && resp > 0L) {
        lhs <- variables[[resp]]
        rownames <- if (is.matrix(lhs)) 
            rownames(lhs)
        else names(lhs)
    }
    if (possible_newdata && length(variables)) {
        nr2 <- max(sapply(variables, NROW))
        if (nr2 != nr) 
            warning(sprintf(paste0(ngettext(nr, "'newdata' had %d row", 
                "'newdata' had %d rows"), " ", ngettext(nr2, 
                "but variable found had %d row", "but variables found have %d rows")), 
                nr, nr2), call. = FALSE, domain = NA)
    }
    if (is.null(attr(formula, "predvars"))) {
        for (i in seq_along(varnames)) predvars[[i + 1L]] <- makepredictcall(variables[[i]], 
            vars[[i + 1L]])
        attr(formula, "predvars") <- predvars
    }
    extras <- substitute(list(...))
    extranames <- names(extras[-1L])
    extras <- eval(extras, data, env)
    subset <- eval(substitute(subset), data, env)
    data <- .External2(C_modelframe, formula, rownames, variables, 
        varnames, extras, extranames, subset, na.action)
    if (length(xlev)) {
        for (nm in names(xlev)) if (!is.null(xl <- xlev[[nm]])) {
            xi <- data[[nm]]
            if (is.character(xi)) 
                xi <- as.factor(xi)
            if (!is.factor(xi) || is.null(nxl <- levels(xi))) 
                warning(gettextf("variable '%s' is not a factor", 
                  nm), domain = NA)
            else {
                ctr <- attr(xi, "contrasts")
                xi <- xi[, drop = TRUE]
                nxl <- levels(xi)
                if (any(m <- is.na(match(nxl, xl)))) 
                  stop(sprintf(ngettext(length(m), "factor %s has new level %s", 
                    "factor %s has new levels %s"), nm, paste(nxl[m], 
                    collapse = ", ")), domain = NA)
                data[[nm]] <- factor(xi, levels = xl, exclude = NULL)
                if (!identical(attr(data[[nm]], "contrasts"), 
                  ctr)) 
                  warning(gettext(sprintf("contrasts dropped from factor %s", 
                    nm), domain = NA), call. = FALSE)
            }
        }
    }
    else if (drop.unused.levels) {
        for (nm in names(data)) {
            x <- data[[nm]]
            if (is.factor(x) && length(unique(x[!is.na(x)])) < 
                length(levels(x))) {
                ctr <- attr(x, "contrasts")
                data[[nm]] <- x[, drop = TRUE]
                if (!identical(attr(data[[nm]], "contrasts"), 
                  ctr)) 
                  warning(gettext(sprintf("contrasts dropped from factor %s due to missing levels", 
                    nm), domain = NA), call. = FALSE)
            }
        }
    }
    attr(formula, "dataClasses") <- vapply(data, .MFclass, "")
    attr(data, "terms") <- formula
    data
}


rgeom <- function (n, prob) 
.Call(C_rgeom, n, prob)


contr.treatment <- function (n, base = 1, contrasts = TRUE, sparse = FALSE) 
{
    if (is.numeric(n) && length(n) == 1L) {
        if (n > 1L) 
            levels <- as.character(seq_len(n))
        else stop("not enough degrees of freedom to define contrasts")
    }
    else {
        levels <- as.character(n)
        n <- length(n)
    }
    contr <- .Diag(levels, sparse = sparse)
    if (contrasts) {
        if (n < 2L) 
            stop(gettextf("contrasts not defined for %d degrees of freedom", 
                n - 1L), domain = NA)
        if (base < 1L | base > n) 
            stop("baseline group number out of range")
        contr <- contr[, -base, drop = FALSE]
    }
    contr
}


qlnorm <- function (p, meanlog = 0, sdlog = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qlnorm, p, meanlog, sdlog, lower.tail, log.p)


lm.fit <- function (x, y, offset = NULL, method = "qr", tol = 1e-07, singular.ok = TRUE, 
    ...) 
{
    if (is.null(n <- nrow(x))) 
        stop("'x' must be a matrix")
    if (n == 0L) 
        stop("0 (non-NA) cases")
    p <- ncol(x)
    if (p == 0L) {
        return(list(coefficients = numeric(), residuals = y, 
            fitted.values = 0 * y, rank = 0, df.residual = length(y)))
    }
    ny <- NCOL(y)
    if (is.matrix(y) && ny == 1) 
        y <- drop(y)
    if (!is.null(offset)) 
        y <- y - offset
    if (NROW(y) != n) 
        stop("incompatible dimensions")
    if (method != "qr") 
        warning(gettextf("method = '%s' is not supported. Using 'qr'", 
            method), domain = NA)
    chkDots(...)
    z <- .Call(C_Cdqrls, x, y, tol, FALSE)
    if (!singular.ok && z$rank < p) 
        stop("singular fit encountered")
    coef <- z$coefficients
    pivot <- z$pivot
    r1 <- seq_len(z$rank)
    dn <- colnames(x)
    if (is.null(dn)) 
        dn <- paste0("x", 1L:p)
    nmeffects <- c(dn[pivot[r1]], rep.int("", n - z$rank))
    r2 <- if (z$rank < p) 
        (z$rank + 1L):p
    else integer()
    if (is.matrix(y)) {
        coef[r2, ] <- NA
        if (z$pivoted) 
            coef[pivot, ] <- coef
        dimnames(coef) <- list(dn, colnames(y))
        dimnames(z$effects) <- list(nmeffects, colnames(y))
    }
    else {
        coef[r2] <- NA
        if (z$pivoted) 
            coef[pivot] <- coef
        names(coef) <- dn
        names(z$effects) <- nmeffects
    }
    z$coefficients <- coef
    r1 <- y - z$residuals
    if (!is.null(offset)) 
        r1 <- r1 + offset
    if (z$pivoted) 
        colnames(z$qr) <- colnames(x)[z$pivot]
    qr <- z[c("qr", "qraux", "pivot", "tol", "rank")]
    c(z[c("coefficients", "residuals", "effects", "rank")], list(fitted.values = r1, 
        assign = attr(x, "assign"), qr = structure(qr, class = "qr"), 
        df.residual = n - z$rank))
}


arima0 <- function (x, order = c(0, 0, 0), seasonal = list(order = c(0, 
    0, 0), period = NA), xreg = NULL, include.mean = TRUE, delta = 0.01, 
    transform.pars = TRUE, fixed = NULL, init = NULL, method = c("ML", 
        "CSS"), n.cond, optim.control = list()) 
{
    arma0f <- function(p) {
        par <- as.double(fixed)
        par[mask] <- p
        .Call(C_arma0fa, G, par)
    }
    arCheck <- function(ar) {
        p <- max(which(c(1, -ar) != 0)) - 1
        if (!p) 
            return(TRUE)
        all(Mod(polyroot(c(1, -ar[1L:p]))) > 1)
    }
    maInvert <- function(ma) {
        q <- length(ma)
        q0 <- max(which(c(1, ma) != 0)) - 1
        if (!q0) 
            return(ma)
        roots <- polyroot(c(1, ma[1L:q0]))
        ind <- Mod(roots) < 1
        if (all(!ind)) 
            return(ma)
        warning("converting non-invertible initial MA values")
        if (q0 == 1) 
            return(c(1/ma[1L], rep(0, q - q0)))
        roots[ind] <- 1/roots[ind]
        x <- 1
        for (r in roots) x <- c(x, 0) - c(0, x)/r
        c(Re(x[-1L]), rep(0, q - q0))
    }
    series <- deparse(substitute(x))
    if (NCOL(x) > 1) 
        stop("only implemented for univariate time series")
    method <- match.arg(method)
    x <- as.ts(x)
    if (!is.numeric(x)) 
        stop("'x' must be numeric")
    dim(x) <- NULL
    n <- length(x)
    if (!missing(order)) 
        if (!is.numeric(order) || length(order) != 3L || any(order < 
            0)) 
            stop("'order' must be a non-negative numeric vector of length 3")
    if (!missing(seasonal)) 
        if (is.list(seasonal)) {
            if (is.null(seasonal$order)) 
                stop("'seasonal' must be a list with component 'order'")
            if (!is.numeric(seasonal$order) || length(seasonal$order) != 
                3L || any(seasonal$order < 0)) 
                stop("'seasonal$order' must be a non-negative numeric vector of length 3")
        }
        else if (is.numeric(order)) {
            if (length(order) == 3) 
                seasonal <- list(order = seasonal)
            else ("'seasonal' is of the wrong length")
        }
        else stop("'seasonal' must be a list with component 'order'")
    if (is.null(seasonal$period) || is.na(seasonal$period) || 
        seasonal$period == 0) 
        seasonal$period <- frequency(x)
    arma <- c(order[-2L], seasonal$order[-2L], seasonal$period, 
        order[2L], seasonal$order[2L])
    narma <- sum(arma[1L:4L])
    if (d <- order[2L]) 
        x <- diff(x, 1, d)
    if (d <- seasonal$order[2L]) 
        x <- diff(x, seasonal$period, d)
    xtsp <- tsp(x)
    tsp(x) <- NULL
    nd <- order[2L] + seasonal$order[2L]
    n.used <- length(x)
    ncond <- n - n.used
    if (method == "CSS") {
        ncond1 <- order[1L] + seasonal$period * seasonal$order[1L]
        ncond <- if (!missing(n.cond)) 
            ncond + max(n.cond, ncond1)
        else ncond + ncond1
    }
    if (is.null(xreg)) {
        ncxreg <- 0
    }
    else {
        if (NROW(xreg) != n) 
            stop("lengths of 'x' and 'xreg' do not match")
        ncxreg <- NCOL(xreg)
    }
    class(xreg) <- NULL
    if (include.mean && (nd == 0)) {
        if (is.matrix(xreg) && is.null(colnames(xreg))) 
            colnames(xreg) <- paste0("xreg", 1L:ncxreg)
        xreg <- cbind(intercept = rep_len(1, n), xreg = xreg)
        ncxreg <- ncxreg + 1
    }
    if (is.null(fixed)) 
        fixed <- rep_len(NA_real_, narma + ncxreg)
    else if (length(fixed) != narma + ncxreg) 
        stop("wrong length for 'fixed'")
    mask <- is.na(fixed)
    if (!any(mask)) 
        stop("all parameters were fixed")
    if (transform.pars && any(!mask[1L:narma])) {
        warning("some ARMA parameters were fixed: setting transform.pars = FALSE")
        transform.pars <- FALSE
    }
    if (ncxreg) {
        if (d <- order[2L]) 
            xreg <- diff(xreg, 1, d)
        if (d <- seasonal$order[2L]) 
            xreg <- diff(xreg, seasonal$period, d)
        xreg <- as.matrix(xreg)
        if (qr(na.omit(xreg))$rank < ncol(xreg)) 
            stop("'xreg' is collinear")
        if (is.null(cn <- colnames(xreg))) 
            cn <- paste0("xreg", 1L:ncxreg)
    }
    if (anyNA(x) || (ncxreg && anyNA(xreg))) 
        if (method == "ML" && delta >= 0) {
            warning("NAs present: setting 'delta' to -1")
            delta <- -1
        }
    init0 <- rep_len(0, narma)
    parscale <- rep_len(1, narma)
    if (ncxreg) {
        orig.xreg <- (ncxreg == 1) || any(!mask[narma + 1L:ncxreg])
        if (!orig.xreg) {
            S <- svd(na.omit(xreg))
            xreg <- xreg %*% S$v
        }
        fit <- lm(x ~ xreg - 1, na.action = na.omit)
        init0 <- c(init0, coef(fit))
        ses <- summary(fit)$coefficients[, 2]
        parscale <- c(parscale, ses)
    }
    storage.mode(x) <- storage.mode(xreg) <- "double"
    if (method == "CSS") 
        transform.pars <- 0
    G <- .Call(C_setup_starma, as.integer(arma), x, n.used, xreg, 
        ncxreg, delta, transform.pars > 0, ncond - (n - n.used))
    on.exit(.Call(C_free_starma, G))
    if (!is.null(init)) {
        if (length(init) != length(init0)) 
            stop("'init' is of the wrong length")
        if (any(ind <- is.na(init))) 
            init[ind] <- init0[ind]
        if (transform.pars) {
            if (any(!mask[1L:narma])) 
                warning("transformed ARMA parameters were fixed")
            if (arma[1L] > 0) 
                if (!arCheck(init[1L:arma[1L]])) 
                  stop("non-stationary AR part")
            if (arma[3L] > 0) 
                if (!arCheck(init[sum(arma[1L:2]) + 1L:arma[3L]])) 
                  stop("non-stationary seasonal AR part")
            if (arma[2L] > 0) {
                ind <- arma[1L] + 1L:arma[2L]
                init[ind] <- maInvert(init[ind])
            }
            if (arma[4L] > 0) {
                ind <- sum(arma[1L:3]) + 1L:arma[4L]
                init[ind] <- maInvert(init[ind])
            }
            init <- .Call(C_Invtrans, G, as.double(init))
        }
    }
    else init <- init0
    .Call(C_Starma_method, G, method == "CSS")
    if (!("parscale" %in% names(optim.control))) 
        optim.control$parscale <- parscale[mask]
    res <- optim(init[mask], arma0f, method = "BFGS", hessian = TRUE, 
        control = optim.control)
    if ((code <- res$convergence) > 0) 
        warning(gettextf("possible convergence problem: optim gave code = %d", 
            code), domain = NA)
    coef <- res$par
    if (transform.pars) {
        cf <- fixed
        cf[mask] <- coef
        A <- .Call(C_Gradtrans, G, as.double(cf))[mask, mask]
        var <- t(A) %*% solve(res$hessian * length(x)) %*% A
        coef <- .Call(C_Dotrans, G, as.double(cf))[mask]
        .Call(C_set_trans, G, 0)
    }
    else var <- solve(res$hessian * length(x))
    arma0f(coef)
    sigma2 <- .Call(C_get_s2, G)
    resid <- .Call(C_get_resid, G)
    tsp(resid) <- xtsp
    class(resid) <- "ts"
    n.used <- sum(!is.na(resid))
    nm <- NULL
    if (arma[1L] > 0) 
        nm <- c(nm, paste0("ar", 1L:arma[1L]))
    if (arma[2L] > 0) 
        nm <- c(nm, paste0("ma", 1L:arma[2L]))
    if (arma[3L] > 0) 
        nm <- c(nm, paste0("sar", 1L:arma[3L]))
    if (arma[4L] > 0) 
        nm <- c(nm, paste0("sma", 1L:arma[4L]))
    fixed[mask] <- coef
    if (ncxreg > 0) {
        nm <- c(nm, cn)
        if (!orig.xreg) {
            ind <- narma + 1L:ncxreg
            fixed[ind] <- S$v %*% fixed[ind]
            A <- diag(narma + ncxreg)
            A[ind, ind] <- S$v
            A <- A[mask, mask]
            var <- A %*% var %*% t(A)
        }
    }
    names(fixed) <- nm
    names(arma) <- c("ar", "ma", "sar", "sma", "period", "diff", 
        "sdiff")
    dimnames(var) <- list(nm[mask], nm[mask])
    value <- 2 * n.used * res$value + n.used + n.used * log(2 * 
        pi)
    aic <- if (method != "CSS") 
        value + 2 * length(coef) + 2
    else NA
    res <- list(coef = fixed, sigma2 = sigma2, var.coef = var, 
        mask = mask, loglik = -0.5 * value, aic = aic, arma = arma, 
        residuals = resid, call = match.call(), series = series, 
        code = code, n.cond = ncond)
    class(res) <- "arima0"
    res
}


offset <- function (object) 
object


power.anova.test <- function (groups = NULL, n = NULL, between.var = NULL, within.var = NULL, 
    sig.level = 0.05, power = NULL) 
{
    if (sum(sapply(list(groups, n, between.var, within.var, power, 
        sig.level), is.null)) != 1) 
        stop("exactly one of 'groups', 'n', 'between.var', 'within.var', 'power', and 'sig.level' must be NULL")
    if (!is.null(groups) && groups < 2) 
        stop("number of groups must be at least 2")
    if (!is.null(n) && n < 2) 
        stop("number of observations in each group must be at least 2")
    if (!is.null(sig.level) && !is.numeric(sig.level) || any(0 > 
        sig.level | sig.level > 1)) 
        stop("'sig.level' must be numeric in [0, 1]")
    p.body <- quote({
        lambda <- (groups - 1) * n * (between.var/within.var)
        pf(qf(sig.level, groups - 1, (n - 1) * groups, lower.tail = FALSE), 
            groups - 1, (n - 1) * groups, lambda, lower.tail = FALSE)
    })
    if (is.null(power)) 
        power <- eval(p.body)
    else if (is.null(groups)) 
        groups <- uniroot(function(groups) eval(p.body) - power, 
            c(2, 100))$root
    else if (is.null(n)) 
        n <- uniroot(function(n) eval(p.body) - power, c(2, 1e+05))$root
    else if (is.null(within.var)) 
        within.var <- uniroot(function(within.var) eval(p.body) - 
            power, between.var * c(1e-07, 1e+07))$root
    else if (is.null(between.var)) 
        between.var <- uniroot(function(between.var) eval(p.body) - 
            power, within.var * c(1e-07, 1e+07))$root
    else if (is.null(sig.level)) 
        sig.level <- uniroot(function(sig.level) eval(p.body) - 
            power, c(1e-10, 1 - 1e-10))$root
    else stop("internal error", domain = NA)
    NOTE <- "n is number in each group"
    METHOD <- "Balanced one-way analysis of variance power calculation"
    structure(list(groups = groups, n = n, between.var = between.var, 
        within.var = within.var, sig.level = sig.level, power = power, 
        note = NOTE, method = METHOD), class = "power.htest")
}


loess <- function (formula, data, weights, subset, na.action, model = FALSE, 
    span = 0.75, enp.target, degree = 2L, parametric = FALSE, 
    drop.square = FALSE, normalize = TRUE, family = c("gaussian", 
        "symmetric"), method = c("loess", "model.frame"), control = loess.control(...), 
    ...) 
{
    family <- match.arg(family)
    method <- match.arg(method)
    mf <- match.call(expand.dots = FALSE)
    mf$model <- mf$span <- mf$enp.target <- mf$degree <- mf$parametric <- mf$drop.square <- mf$normalize <- mf$family <- mf$method <- mf$control <- mf$... <- NULL
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval(mf, parent.frame())
    if (match.arg(method) == "model.frame") 
        return(mf)
    mt <- attr(mf, "terms")
    y <- model.response(mf, "numeric")
    w <- model.weights(mf)
    if (is.null(w)) 
        w <- rep_len(1, length(y))
    nmx <- as.character(attr(mt, "variables"))[-(1L:2)]
    x <- mf[, nmx, drop = FALSE]
    if (any(sapply(x, is.factor))) 
        stop("predictors must all be numeric")
    x <- as.matrix(x)
    D <- ncol(x)
    nmx <- setNames(nm = colnames(x))
    drop.square <- match(nmx, nmx[drop.square], 0L) > 0L
    parametric <- match(nmx, nmx[parametric], 0L) > 0L
    if (!match(degree, 0L:2L, 0L)) 
        stop("'degree' must be 0, 1 or 2")
    iterations <- if (family == "gaussian") 
        1L
    else control$iterations
    if (!missing(enp.target)) 
        if (!missing(span)) 
            warning("both 'span' and 'enp.target' specified: 'span' will be used")
        else {
            tau <- switch(degree + 1L, 1, D + 1, (D + 1) * (D + 
                2)/2) - sum(drop.square)
            span <- 1.2 * tau/enp.target
        }
    if (!is.list(control) || !is.character(control$surface) || 
        !is.character(control$statistics) || !is.character(control$trace.hat) || 
        !is.numeric(control$cell) || !is.numeric(iterations)) 
        stop("invalid 'control' argument")
    fit <- simpleLoess(y, x, w, span, degree = degree, parametric = parametric, 
        drop.square = drop.square, normalize = normalize, statistics = control$statistics, 
        surface = control$surface, cell = control$cell, iterations = iterations, 
        iterTrace = control$iterTrace, trace.hat = control$trace.hat)
    fit$call <- match.call()
    fit$terms <- mt
    fit$xnames <- nmx
    fit$x <- x
    fit$y <- y
    fit$weights <- w
    if (model) 
        fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    fit
}


pairwise.prop.test <- function (x, n, p.adjust.method = p.adjust.methods, ...) 
{
    p.adjust.method <- match.arg(p.adjust.method)
    METHOD <- "Pairwise comparison of proportions"
    DNAME <- deparse(substitute(x))
    if (is.matrix(x)) {
        if (ncol(x) != 2) 
            stop("'x' must have 2 columns")
        n <- rowSums(x)
        x <- x[, 1]
    }
    else {
        DNAME <- paste(DNAME, "out of", deparse(substitute(n)))
        if (length(x) != length(n)) 
            stop("'x' and 'n' must have the same length")
    }
    OK <- complete.cases(x, n)
    x <- x[OK]
    n <- n[OK]
    if (length(x) < 2L) 
        stop("too few groups")
    compare.levels <- function(i, j) {
        prop.test(x[c(i, j)], n[c(i, j)], ...)$p.value
    }
    level.names <- names(x)
    if (is.null(level.names)) 
        level.names <- seq_along(x)
    PVAL <- pairwise.table(compare.levels, level.names, p.adjust.method)
    ans <- list(method = METHOD, data.name = DNAME, p.value = PVAL, 
        p.adjust.method = p.adjust.method)
    class(ans) <- "pairwise.htest"
    ans
}


kmeans <- function (x, centers, iter.max = 10L, nstart = 1L, algorithm = c("Hartigan-Wong", 
    "Lloyd", "Forgy", "MacQueen"), trace = FALSE) 
{
    .Mimax <- .Machine$integer.max
    do_one <- function(nmeth) {
        switch(nmeth, {
            isteps.Qtran <- as.integer(min(.Mimax, 50 * m))
            iTran <- c(isteps.Qtran, integer(max(0, k - 1)))
            Z <- .Fortran(C_kmns, x, m, p, centers = centers, 
                as.integer(k), c1 = integer(m), c2 = integer(m), 
                nc = integer(k), double(k), double(k), ncp = integer(k), 
                D = double(m), iTran = iTran, live = integer(k), 
                iter = iter.max, wss = double(k), ifault = as.integer(trace))
            switch(Z$ifault, stop("empty cluster: try a better set of initial centers", 
                call. = FALSE), Z$iter <- max(Z$iter, iter.max + 
                1L), stop("number of cluster centres must lie between 1 and nrow(x)", 
                call. = FALSE), warning(gettextf("Quick-TRANSfer stage steps exceeded maximum (= %d)", 
                isteps.Qtran), call. = FALSE))
        }, {
            Z <- .C(C_kmeans_Lloyd, x, m, p, centers = centers, 
                k, c1 = integer(m), iter = iter.max, nc = integer(k), 
                wss = double(k))
        }, {
            Z <- .C(C_kmeans_MacQueen, x, m, p, centers = as.double(centers), 
                k, c1 = integer(m), iter = iter.max, nc = integer(k), 
                wss = double(k))
        })
        if (m23 <- any(nmeth == c(2L, 3L))) {
            if (any(Z$nc == 0)) 
                warning("empty cluster: try a better set of initial centers", 
                  call. = FALSE)
        }
        if (Z$iter > iter.max) {
            warning(sprintf(ngettext(iter.max, "did not converge in %d iteration", 
                "did not converge in %d iterations"), iter.max), 
                call. = FALSE, domain = NA)
            if (m23) 
                Z$ifault <- 2L
        }
        if (nmeth %in% c(2L, 3L)) {
            if (any(Z$nc == 0)) 
                warning("empty cluster: try a better set of initial centers", 
                  call. = FALSE)
        }
        Z
    }
    x <- as.matrix(x)
    m <- as.integer(nrow(x))
    if (is.na(m)) 
        stop("invalid nrow(x)")
    p <- as.integer(ncol(x))
    if (is.na(p)) 
        stop("invalid ncol(x)")
    if (missing(centers)) 
        stop("'centers' must be a number or a matrix")
    nmeth <- switch(match.arg(algorithm), `Hartigan-Wong` = 1L, 
        Lloyd = 2L, Forgy = 2L, MacQueen = 3L)
    storage.mode(x) <- "double"
    if (length(centers) == 1L) {
        k <- centers
        if (nstart == 1L) 
            centers <- x[sample.int(m, k), , drop = FALSE]
        if (nstart >= 2L || any(duplicated(centers))) {
            cn <- unique(x)
            mm <- nrow(cn)
            if (mm < k) 
                stop("more cluster centers than distinct data points.")
            centers <- cn[sample.int(mm, k), , drop = FALSE]
        }
    }
    else {
        centers <- as.matrix(centers)
        if (any(duplicated(centers))) 
            stop("initial centers are not distinct")
        cn <- NULL
        k <- nrow(centers)
        if (m < k) 
            stop("more cluster centers than data points")
    }
    k <- as.integer(k)
    if (is.na(k)) 
        stop(gettextf("invalid value of %s", "'k'"), domain = NA)
    if (k == 1L) 
        nmeth <- 3L
    iter.max <- as.integer(iter.max)
    if (is.na(iter.max) || iter.max < 1L) 
        stop("'iter.max' must be positive")
    if (ncol(x) != ncol(centers)) 
        stop("must have same number of columns in 'x' and 'centers'")
    storage.mode(centers) <- "double"
    Z <- do_one(nmeth)
    best <- sum(Z$wss)
    if (nstart >= 2L && !is.null(cn)) 
        for (i in 2:nstart) {
            centers <- cn[sample.int(mm, k), , drop = FALSE]
            ZZ <- do_one(nmeth)
            if ((z <- sum(ZZ$wss)) < best) {
                Z <- ZZ
                best <- z
            }
        }
    centers <- matrix(Z$centers, k)
    dimnames(centers) <- list(1L:k, dimnames(x)[[2L]])
    cluster <- Z$c1
    if (!is.null(rn <- rownames(x))) 
        names(cluster) <- rn
    totss <- sum(scale(x, scale = FALSE)^2)
    structure(list(cluster = cluster, centers = centers, totss = totss, 
        withinss = Z$wss, tot.withinss = best, betweenss = totss - 
            best, size = Z$nc, iter = Z$iter, ifault = Z$ifault), 
        class = "kmeans")
}


aggregate.ts <- function (x, nfrequency = 1, FUN = sum, ndeltat = 1, ts.eps = getOption("ts.eps"), 
    ...) 
{
    x <- as.ts(x)
    ofrequency <- tsp(x)[3L]
    FUN <- match.fun(FUN)
    if (missing(nfrequency)) 
        nfrequency <- 1/ndeltat
    if ((nfrequency > 1) && (abs(nfrequency - round(nfrequency)) < 
        ts.eps)) 
        nfrequency <- round(nfrequency)
    if (nfrequency == ofrequency) 
        return(x)
    ratio <- ofrequency/nfrequency
    if (abs(ratio - round(ratio)) > ts.eps) 
        stop(gettextf("cannot change frequency from %g to %g", 
            ofrequency, nfrequency), domain = NA)
    len <- trunc((ofrequency/nfrequency) + ts.eps)
    mat <- is.matrix(x)
    if (mat) 
        cn <- colnames(x)
    nstart <- tsp(x)[1L]
    x <- as.matrix(x)
    nend <- floor(nrow(x)/len) * len
    x <- apply(array(c(x[1:nend, ]), dim = c(len, nend/len, ncol(x))), 
        MARGIN = c(2L, 3L), FUN = FUN, ...)
    if (!mat) 
        x <- as.vector(x)
    else colnames(x) <- cn
    ts(x, start = nstart, frequency = nfrequency)
}


relevel <- function (x, ref, ...) 
UseMethod("relevel")


spectrum <- function (x, ..., method = c("pgram", "ar")) 
{
    switch(match.arg(method), pgram = spec.pgram(x, ...), ar = spec.ar(x, 
        ...))
}


model.weights <- function (x) 
x$"(weights)"


rexp <- function (n, rate = 1) 
.Call(C_rexp, n, 1/rate)


.lm.fit <- function (x, y, tol = 1e-07) 
.Call(C_Cdqrls, x, y, tol, check = TRUE)


printCoefmat <- function (x, digits = max(3L, getOption("digits") - 2L), signif.stars = getOption("show.signif.stars"), 
    signif.legend = signif.stars, dig.tst = max(1L, min(5L, digits - 
        1L)), cs.ind = 1:k, tst.ind = k + 1, zap.ind = integer(), 
    P.values = NULL, has.Pvalue = nc >= 4L && substr(colnames(x)[nc], 
        1L, 3L) %in% c("Pr(", "p-v"), eps.Pvalue = .Machine$double.eps, 
    na.print = "NA", ...) 
{
    if (is.null(d <- dim(x)) || length(d) != 2L) 
        stop("'x' must be coefficient matrix/data frame")
    nc <- d[2L]
    if (is.null(P.values)) {
        scp <- getOption("show.coef.Pvalues")
        if (!is.logical(scp) || is.na(scp)) {
            warning("option \"show.coef.Pvalues\" is invalid: assuming TRUE")
            scp <- TRUE
        }
        P.values <- has.Pvalue && scp
    }
    else if (P.values && !has.Pvalue) 
        stop("'P.values' is TRUE, but 'has.Pvalue' is not")
    if (has.Pvalue && !P.values) {
        d <- dim(xm <- data.matrix(x[, -nc, drop = FALSE]))
        nc <- nc - 1
        has.Pvalue <- FALSE
    }
    else xm <- data.matrix(x)
    k <- nc - has.Pvalue - (if (missing(tst.ind)) 
        1
    else length(tst.ind))
    if (!missing(cs.ind) && length(cs.ind) > k) 
        stop("wrong k / cs.ind")
    Cf <- array("", dim = d, dimnames = dimnames(xm))
    ok <- !(ina <- is.na(xm))
    for (i in zap.ind) xm[, i] <- zapsmall(xm[, i], digits)
    if (length(cs.ind)) {
        acs <- abs(coef.se <- xm[, cs.ind, drop = FALSE])
        if (any(ia <- is.finite(acs))) {
            digmin <- 1 + if (length(acs <- acs[ia & acs != 0])) 
                floor(log10(range(acs[acs != 0], finite = TRUE)))
            else 0
            Cf[, cs.ind] <- format(round(coef.se, max(1L, digits - 
                digmin)), digits = digits)
        }
    }
    if (length(tst.ind)) 
        Cf[, tst.ind] <- format(round(xm[, tst.ind], digits = dig.tst), 
            digits = digits)
    if (any(r.ind <- !((1L:nc) %in% c(cs.ind, tst.ind, if (has.Pvalue) nc)))) 
        for (i in which(r.ind)) Cf[, i] <- format(xm[, i], digits = digits)
    ok[, tst.ind] <- FALSE
    okP <- if (has.Pvalue) 
        ok[, -nc]
    else ok
    x1 <- Cf[okP]
    dec <- getOption("OutDec")
    if (dec != ".") 
        x1 <- chartr(dec, ".", x1)
    x0 <- (xm[okP] == 0) != (as.numeric(x1) == 0)
    if (length(not.both.0 <- which(x0 & !is.na(x0)))) {
        Cf[okP][not.both.0] <- format(xm[okP][not.both.0], digits = max(1L, 
            digits - 1L))
    }
    if (any(ina)) 
        Cf[ina] <- na.print
    if (P.values) {
        if (!is.logical(signif.stars) || is.na(signif.stars)) {
            warning("option \"show.signif.stars\" is invalid: assuming TRUE")
            signif.stars <- TRUE
        }
        if (any(okP <- ok[, nc])) {
            pv <- as.vector(xm[, nc])
            Cf[okP, nc] <- format.pval(pv[okP], digits = dig.tst, 
                eps = eps.Pvalue)
            signif.stars <- signif.stars && any(pv[okP] < 0.1)
            if (signif.stars) {
                Signif <- symnum(pv, corr = FALSE, na = FALSE, 
                  cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), 
                  symbols = c("***", "**", "*", ".", " "))
                Cf <- cbind(Cf, format(Signif))
            }
        }
        else signif.stars <- FALSE
    }
    else signif.stars <- FALSE
    print.default(Cf, quote = FALSE, right = TRUE, na.print = na.print, 
        ...)
    if (signif.stars && signif.legend) {
        if ((w <- getOption("width")) < nchar(sleg <- attr(Signif, 
            "legend"))) 
            sleg <- strwrap(sleg, width = w - 2, prefix = "  ")
        cat("---\nSignif. codes:  ", sleg, sep = "", fill = w + 
            4 + max(nchar(sleg, "bytes") - nchar(sleg)))
    }
    invisible(x)
}


PP.test <- function (x, lshort = TRUE) 
{
    if (NCOL(x) > 1) 
        stop("x is not a vector or univariate time series")
    DNAME <- deparse(substitute(x))
    z <- embed(x, 2)
    yt <- z[, 1]
    yt1 <- z[, 2]
    n <- length(yt)
    u <- (1L:n) - n/2
    res <- lm(yt ~ 1 + u + yt1)
    if (res$rank < 3) 
        stop("singularities in regression")
    cf <- coef(summary(res))
    tstat <- (cf[3, 1] - 1)/cf[3, 2]
    u <- residuals(res)
    ssqru <- sum(u^2)/n
    l <- if (lshort) 
        trunc(4 * (n/100)^0.25)
    else trunc(12 * (n/100)^0.25)
    ssqrtl <- ssqru + .Call(C_pp_sum, u, l)
    n2 <- n^2
    trm1 <- n2 * (n2 - 1) * sum(yt1^2)/12
    trm2 <- n * sum(yt1 * (1L:n))^2
    trm3 <- n * (n + 1) * sum(yt1 * (1L:n)) * sum(yt1)
    trm4 <- (n * (n + 1) * (2 * n + 1) * sum(yt1)^2)/6
    Dx <- trm1 - trm2 + trm3 - trm4
    STAT <- sqrt(ssqru)/sqrt(ssqrtl) * tstat - (n^3)/(4 * sqrt(3) * 
        sqrt(Dx) * sqrt(ssqrtl)) * (ssqrtl - ssqru)
    table <- cbind(c(4.38, 4.15, 4.04, 3.99, 3.98, 3.96), c(3.95, 
        3.8, 3.73, 3.69, 3.68, 3.66), c(3.6, 3.5, 3.45, 3.43, 
        3.42, 3.41), c(3.24, 3.18, 3.15, 3.13, 3.13, 3.12), c(1.14, 
        1.19, 1.22, 1.23, 1.24, 1.25), c(0.8, 0.87, 0.9, 0.92, 
        0.93, 0.94), c(0.5, 0.58, 0.62, 0.64, 0.65, 0.66), c(0.15, 
        0.24, 0.28, 0.31, 0.32, 0.33))
    table <- -table
    tablen <- dim(table)[2L]
    tableT <- c(25, 50, 100, 250, 500, 1e+05)
    tablep <- c(0.01, 0.025, 0.05, 0.1, 0.9, 0.95, 0.975, 0.99)
    tableipl <- numeric(tablen)
    for (i in (1L:tablen)) tableipl[i] <- approx(tableT, table[, 
        i], n, rule = 2)$y
    PVAL <- approx(tableipl, tablep, STAT, rule = 2)$y
    PARAMETER <- l
    METHOD <- "Phillips-Perron Unit Root Test"
    names(STAT) <- "Dickey-Fuller"
    names(PARAMETER) <- "Truncation lag parameter"
    structure(list(statistic = STAT, parameter = PARAMETER, p.value = PVAL, 
        method = METHOD, data.name = DNAME), class = "htest")
}


cov.wt <- function (x, wt = rep(1/nrow(x), nrow(x)), cor = FALSE, center = TRUE, 
    method = c("unbiased", "ML")) 
{
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    else if (!is.matrix(x)) 
        stop("'x' must be a matrix or a data frame")
    if (!all(is.finite(x))) 
        stop("'x' must contain finite values only")
    n <- nrow(x)
    if (with.wt <- !missing(wt)) {
        if (length(wt) != n) 
            stop("length of 'wt' must equal the number of rows in 'x'")
        if (any(wt < 0) || (s <- sum(wt)) == 0) 
            stop("weights must be non-negative and not all zero")
        wt <- wt/s
    }
    if (is.logical(center)) {
        center <- if (center) 
            colSums(wt * x)
        else 0
    }
    else {
        if (length(center) != ncol(x)) 
            stop("length of 'center' must equal the number of columns in 'x'")
    }
    x <- sqrt(wt) * sweep(x, 2, center, check.margin = FALSE)
    cov <- switch(match.arg(method), unbiased = crossprod(x)/(1 - 
        sum(wt^2)), ML = crossprod(x))
    y <- list(cov = cov, center = center, n.obs = n)
    if (with.wt) 
        y$wt <- wt
    if (cor) {
        Is <- 1/sqrt(diag(cov))
        R <- cov
        R[] <- Is * cov * rep(Is, each = nrow(cov))
        y$cor <- R
    }
    y
}


residuals.lm <- function (object, type = c("working", "response", "deviance", 
    "pearson", "partial"), ...) 
{
    type <- match.arg(type)
    r <- object$residuals
    res <- switch(type, working = , response = r, deviance = , 
        pearson = if (is.null(object$weights)) r else r * sqrt(object$weights), 
        partial = r)
    res <- naresid(object$na.action, res)
    if (type == "partial") 
        res <- res + predict(object, type = "terms")
    res
}


replications <- function (formula, data = NULL, na.action) 
{
    if (missing(data) && inherits(formula, "data.frame")) {
        data <- formula
        formula <- ~.
    }
    if (!inherits(formula, "terms")) {
        formula <- as.formula(formula)
        if (length(formula) < 3L) {
            f <- y ~ x
            f[[3L]] <- formula[[2L]]
            formula <- f
        }
        formula <- terms(formula, data = data)
    }
    if (missing(na.action)) 
        if (!is.null(tj <- attr(data, "na.action")) && is.function(tj)) 
            na.action <- tj
        else {
            naa <- getOption("na.action")
            if (!is.null(naa)) 
                na.action <- match.fun(naa)
            else na.action <- na.fail
        }
    f <- attr(formula, "factors")
    o <- attr(formula, "order")
    labels <- attr(formula, "term.labels")
    vars <- as.character(attr(formula, "variables"))[-1L]
    if (is.null(data)) {
        v <- c(quote(data.frame), attr(formula, "variables"))
        data <- eval(as.call(v), parent.frame())
    }
    if (!is.function(na.action)) 
        stop("na.action must be a function")
    data <- na.action(data)
    class(data) <- NULL
    n <- length(o)
    z <- setNames(vector("list", n), labels)
    dummy <- numeric(.row_names_info(data, 2L))
    data <- lapply(data, function(x) if (is.character(x)) 
        as.factor(x)
    else x)
    notfactor <- !sapply(data, function(x) inherits(x, "factor"))
    balance <- TRUE
    for (i in seq_len(n)) {
        l <- labels[i]
        if (o[i] < 1 || substring(l, 1L, 5L) == "Error") {
            z[[l]] <- NULL
            next
        }
        select <- vars[f[, i] > 0]
        if (any(nn <- notfactor[select])) {
            warning(gettextf("non-factors ignored: %s", paste(names(nn), 
                collapse = ", ")), domain = NA)
            next
        }
        if (length(select)) 
            tble <- tapply(dummy, unclass(data[select]), length)
        nrep <- unique(as.vector(tble))
        if (length(nrep) > 1L) {
            balance <- FALSE
            tble[is.na(tble)] <- 0
            z[[l]] <- tble
        }
        else z[[l]] <- as.vector(nrep)
    }
    if (balance) 
        unlist(z)
    else z
}


fisher.test <- function (x, y = NULL, workspace = 2e+05, hybrid = FALSE, control = list(), 
    or = 1, alternative = "two.sided", conf.int = TRUE, conf.level = 0.95, 
    simulate.p.value = FALSE, B = 2000) 
{
    DNAME <- deparse(substitute(x))
    METHOD <- "Fisher's Exact Test for Count Data"
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    if (is.matrix(x)) {
        if (any(dim(x) < 2L)) 
            stop("'x' must have at least 2 rows and columns")
        if (!is.numeric(x) || any(x < 0) || anyNA(x)) 
            stop("all entries of 'x' must be nonnegative and finite")
        if (!is.integer(x)) {
            xo <- x
            x <- round(x)
            if (any(x > .Machine$integer.max)) 
                stop("'x' has entries too large to be integer")
            if (!identical(TRUE, (ax <- all.equal(xo, x)))) 
                warning(gettextf("'x' has been rounded to integer: %s", 
                  ax), domain = NA)
            storage.mode(x) <- "integer"
        }
    }
    else {
        if (is.null(y)) 
            stop("if 'x' is not a matrix, 'y' must be given")
        if (length(x) != length(y)) 
            stop("'x' and 'y' must have the same length")
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        OK <- complete.cases(x, y)
        x <- as.factor(x[OK])
        y <- as.factor(y[OK])
        if ((nlevels(x) < 2L) || (nlevels(y) < 2L)) 
            stop("'x' and 'y' must have at least 2 levels")
        x <- table(x, y)
    }
    con <- list(mult = 30)
    con[names(control)] <- control
    if ((mult <- as.integer(con$mult)) < 2) 
        stop("'mult' must be integer >= 2, typically = 30")
    nr <- nrow(x)
    nc <- ncol(x)
    if ((nr == 2) && (nc == 2)) {
        alternative <- char.expand(alternative, c("two.sided", 
            "less", "greater"))
        if (length(alternative) > 1L || is.na(alternative)) 
            stop("alternative must be \"two.sided\", \"less\" or \"greater\"")
        if (!((length(conf.level) == 1L) && is.finite(conf.level) && 
            (conf.level > 0) && (conf.level < 1))) 
            stop("'conf.level' must be a single number between 0 and 1")
        if (!missing(or) && (length(or) > 1L || is.na(or) || 
            or < 0)) 
            stop("'or' must be a single number between 0 and Inf")
    }
    PVAL <- NULL
    if (nr != 2 || nc != 2) {
        if (simulate.p.value) {
            sr <- rowSums(x)
            sc <- colSums(x)
            x <- x[sr > 0, sc > 0, drop = FALSE]
            nr <- as.integer(nrow(x))
            nc <- as.integer(ncol(x))
            if (is.na(nr) || is.na(nc) || is.na(nr * nc)) 
                stop("invalid nrow(x) or ncol(x)", domain = NA)
            if (nr <= 1L) 
                stop("need 2 or more non-zero row marginals")
            if (nc <= 1L) 
                stop("need 2 or more non-zero column marginals")
            METHOD <- paste(METHOD, "with simulated p-value\n\t (based on", 
                B, "replicates)")
            STATISTIC <- -sum(lfactorial(x))
            tmp <- .Call(C_Fisher_sim, rowSums(x), colSums(x), 
                B)
            almost.1 <- 1 + 64 * .Machine$double.eps
            PVAL <- (1 + sum(tmp <= STATISTIC/almost.1))/(B + 
                1)
        }
        else if (hybrid) {
            PVAL <- .Call(C_Fexact, x, c(5, 180, 1), workspace, 
                mult)
        }
        else {
            PVAL <- .Call(C_Fexact, x, c(-1, 100, 0), workspace, 
                mult)
        }
        RVAL <- list(p.value = max(0, min(1, PVAL)))
    }
    if ((nr == 2) && (nc == 2)) {
        if (hybrid) 
            warning("'hybrid' is ignored for a 2 x 2 table")
        m <- sum(x[, 1L])
        n <- sum(x[, 2L])
        k <- sum(x[1L, ])
        x <- x[1L, 1L]
        lo <- max(0L, k - n)
        hi <- min(k, m)
        NVAL <- c(`odds ratio` = or)
        support <- lo:hi
        logdc <- dhyper(support, m, n, k, log = TRUE)
        dnhyper <- function(ncp) {
            d <- logdc + log(ncp) * support
            d <- exp(d - max(d))
            d/sum(d)
        }
        mnhyper <- function(ncp) {
            if (ncp == 0) 
                return(lo)
            if (ncp == Inf) 
                return(hi)
            sum(support * dnhyper(ncp))
        }
        pnhyper <- function(q, ncp = 1, upper.tail = FALSE) {
            if (ncp == 1) {
                return(if (upper.tail) phyper(x - 1, m, n, k, 
                  lower.tail = FALSE) else phyper(x, m, n, k))
            }
            if (ncp == 0) {
                return(as.numeric(if (upper.tail) q <= lo else q >= 
                  lo))
            }
            if (ncp == Inf) {
                return(as.numeric(if (upper.tail) q <= hi else q >= 
                  hi))
            }
            sum(dnhyper(ncp)[if (upper.tail) support >= q else support <= 
                q])
        }
        if (is.null(PVAL)) {
            PVAL <- switch(alternative, less = pnhyper(x, or), 
                greater = pnhyper(x, or, upper.tail = TRUE), 
                two.sided = {
                  if (or == 0) as.numeric(x == lo) else if (or == 
                    Inf) as.numeric(x == hi) else {
                    relErr <- 1 + 10^(-7)
                    d <- dnhyper(or)
                    sum(d[d <= d[x - lo + 1] * relErr])
                  }
                })
            RVAL <- list(p.value = PVAL)
        }
        mle <- function(x) {
            if (x == lo) 
                return(0)
            if (x == hi) 
                return(Inf)
            mu <- mnhyper(1)
            if (mu > x) 
                uniroot(function(t) mnhyper(t) - x, c(0, 1))$root
            else if (mu < x) 
                1/uniroot(function(t) mnhyper(1/t) - x, c(.Machine$double.eps, 
                  1))$root
            else 1
        }
        ESTIMATE <- c(`odds ratio` = mle(x))
        if (conf.int) {
            ncp.U <- function(x, alpha) {
                if (x == hi) 
                  return(Inf)
                p <- pnhyper(x, 1)
                if (p < alpha) 
                  uniroot(function(t) pnhyper(x, t) - alpha, 
                    c(0, 1))$root
                else if (p > alpha) 
                  1/uniroot(function(t) pnhyper(x, 1/t) - alpha, 
                    c(.Machine$double.eps, 1))$root
                else 1
            }
            ncp.L <- function(x, alpha) {
                if (x == lo) 
                  return(0)
                p <- pnhyper(x, 1, upper.tail = TRUE)
                if (p > alpha) 
                  uniroot(function(t) pnhyper(x, t, upper.tail = TRUE) - 
                    alpha, c(0, 1))$root
                else if (p < alpha) 
                  1/uniroot(function(t) pnhyper(x, 1/t, upper.tail = TRUE) - 
                    alpha, c(.Machine$double.eps, 1))$root
                else 1
            }
            CINT <- switch(alternative, less = c(0, ncp.U(x, 
                1 - conf.level)), greater = c(ncp.L(x, 1 - conf.level), 
                Inf), two.sided = {
                alpha <- (1 - conf.level)/2
                c(ncp.L(x, alpha), ncp.U(x, alpha))
            })
            attr(CINT, "conf.level") <- conf.level
        }
        RVAL <- c(RVAL, list(conf.int = if (conf.int) CINT, estimate = ESTIMATE, 
            null.value = NVAL))
    }
    RVAL <- c(RVAL, alternative = alternative, method = METHOD, 
        data.name = DNAME)
    attr(RVAL, "class") <- "htest"
    return(RVAL)
}


qexp <- function (p, rate = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qexp, p, 1/rate, lower.tail, log.p)


screeplot <- function (x, ...) 
UseMethod("screeplot")


contr.helmert <- function (n, contrasts = TRUE, sparse = FALSE) 
{
    if (length(n) <= 1L) {
        if (is.numeric(n) && length(n) == 1L && n > 1L) 
            levels <- seq_len(n)
        else stop("not enough degrees of freedom to define contrasts")
    }
    else levels <- n
    levels <- as.character(levels)
    if (contrasts) {
        n <- length(levels)
        cont <- array(-1, c(n, n - 1L), list(levels, NULL))
        cont[col(cont) <= row(cont) - 2L] <- 0
        cont[col(cont) == row(cont) - 1L] <- seq_len(n - 1L)
        colnames(cont) <- NULL
        if (sparse) 
            .asSparse(cont)
        else cont
    }
    else .Diag(levels, sparse = sparse)
}


deltat <- function (x, ...) 
UseMethod("deltat")


medpolish <- function (x, eps = 0.01, maxiter = 10L, trace.iter = TRUE, na.rm = FALSE) 
{
    z <- as.matrix(x)
    nr <- nrow(z)
    nc <- ncol(z)
    t <- 0
    r <- numeric(nr)
    c <- numeric(nc)
    oldsum <- 0
    for (iter in 1L:maxiter) {
        rdelta <- apply(z, 1L, median, na.rm = na.rm)
        z <- z - matrix(rdelta, nrow = nr, ncol = nc)
        r <- r + rdelta
        delta <- median(c, na.rm = na.rm)
        c <- c - delta
        t <- t + delta
        cdelta <- apply(z, 2L, median, na.rm = na.rm)
        z <- z - matrix(cdelta, nrow = nr, ncol = nc, byrow = TRUE)
        c <- c + cdelta
        delta <- median(r, na.rm = na.rm)
        r <- r - delta
        t <- t + delta
        newsum <- sum(abs(z), na.rm = na.rm)
        converged <- newsum == 0 || abs(newsum - oldsum) < eps * 
            newsum
        if (converged) 
            break
        oldsum <- newsum
        if (trace.iter) 
            cat(iter, ": ", newsum, "\n", sep = "")
    }
    if (converged) {
        if (trace.iter) 
            cat("Final: ", newsum, "\n", sep = "")
    }
    else warning(sprintf(ngettext(maxiter, "medpolish() did not converge in %d iteration", 
        "medpolish() did not converge in %d iterations"), maxiter), 
        domain = NA)
    names(r) <- rownames(z)
    names(c) <- colnames(z)
    ans <- list(overall = t, row = r, col = c, residuals = z, 
        name = deparse(substitute(x)))
    class(ans) <- "medpolish"
    ans
}


pexp <- function (q, rate = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_pexp, q, 1/rate, lower.tail, log.p)


rWishart <- function (n, df, Sigma) 
.Call(C_rWishart, n, df, Sigma)


ccf <- function (x, y, lag.max = NULL, type = c("correlation", "covariance"), 
    plot = TRUE, na.action = na.fail, ...) 
{
    type <- match.arg(type)
    if (is.matrix(x) || is.matrix(y)) 
        stop("univariate time series only")
    X <- ts.intersect(as.ts(x), as.ts(y))
    colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
    acf.out <- acf(X, lag.max = lag.max, plot = FALSE, type = type, 
        na.action = na.action)
    lag <- c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
    y <- c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
    acf.out$acf <- array(y, dim = c(length(y), 1L, 1L))
    acf.out$lag <- array(lag, dim = c(length(y), 1L, 1L))
    acf.out$snames <- paste(acf.out$snames, collapse = " & ")
    if (plot) {
        plot(acf.out, ...)
        return(invisible(acf.out))
    }
    else return(acf.out)
}


time <- function (x, ...) 
UseMethod("time")


anova <- function (object, ...) 
UseMethod("anova")


IQR <- function (x, na.rm = FALSE, type = 7) 
diff(quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm, names = FALSE, 
    type = type))


KalmanLike <- function (y, mod, nit = 0L, update = FALSE) 
{
    x <- .Call(C_KalmanLike, y, mod, nit, FALSE, update)
    z <- list(Lik = 0.5 * (log(x[1L]) + x[2L]), s2 = x[1L])
    if (update) 
        attr(z, "mod") <- attr(x, "mod")
    z
}


optimHess <- function (par, fn, gr = NULL, ..., control = list()) 
{
    fn1 <- function(par) fn(par, ...)
    gr1 <- if (!is.null(gr)) 
        function(par) gr(par, ...)
    npar <- length(par)
    con <- list(fnscale = 1, parscale = rep.int(1, npar), ndeps = rep.int(0.001, 
        npar))
    con[(names(control))] <- control
    .External2(C_optimhess, par, fn1, gr1, con)
}


pweibull <- function (q, shape, scale = 1, lower.tail = TRUE, log.p = FALSE) 
.Call(C_pweibull, q, shape, scale, lower.tail, log.p)


p.adjust.methods <- c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", 
"none")


read.ftable <- function (file, sep = "", quote = "\"", row.var.names, col.vars, 
    skip = 0) 
{
    if (is.character(file)) {
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    if (!isSeekable(file)) {
        tmpf <- tempfile()
        cat(readLines(file), file = tmpf, sep = "\n")
        file <- file(tmpf, "r")
        on.exit({
            close(file)
            unlink(tmpf)
        }, add = TRUE)
    }
    z <- count.fields(file, sep, quote, skip)
    n.row.vars <- z[max(which(z == max(z)))] - z[length(z)] + 
        1
    seek(file, where = 0)
    if (skip > 0) 
        readLines(file, skip)
    lines <- readLines(file)
    seek(file, where = 0)
    if (skip > 0) 
        readLines(file, skip)
    i <- which(z == n.row.vars)
    j <- i[grep("^[^[:space:]]", lines[i])]
    if ((length(j) == 1L) && (j > 1)) {
        n.col.vars <- j - 1
        col.vars <- vector("list", length = n.col.vars)
        n <- c(1, z[1:n.col.vars] - 1)
        for (k in seq.int(from = 1, to = n.col.vars)) {
            s <- scan(file, what = "", sep = sep, quote = quote, 
                nlines = 1, quiet = TRUE)
            col.vars[[k]] <- s[-1L]
            names(col.vars)[k] <- s[1L]
        }
        row.vars <- setNames(vector("list", length = n.row.vars), 
            scan(file, what = "", sep = sep, quote = quote, nlines = 1, 
                quiet = TRUE))
        z <- z[-(1:(n.col.vars + 1))]
    }
    else {
        if ((z[1L] == 1) && z[2L] == max(z)) {
            n.col.vars <- 1
            col.vars <- vector("list", length = n.col.vars)
            s <- scan(file, what = "", sep = sep, quote = quote, 
                nlines = 2, quiet = TRUE)
            names(col.vars) <- s[1L]
            s <- s[-1L]
            row.vars <- vector("list", length = n.row.vars)
            i <- 1:n.row.vars
            names(row.vars) <- s[i]
            col.vars[[1L]] <- s[-i]
            z <- z[-(1:2)]
        }
        else {
            if (missing(row.var.names)) {
                stop("'row.var.names' missing")
            }
            n.row.vars <- length(row.var.names)
            row.vars <- setNames(vector("list", length = n.row.vars), 
                as.character(row.var.names))
            if (missing(col.vars) || !is.list(col.vars)) {
                stop("'col.vars' missing or incorrect")
            }
            col.vars <- lapply(col.vars, as.character)
            n.col.vars <- length(col.vars)
            if (is.null(names(col.vars))) 
                names(col.vars) <- paste0("Factor.", seq_along(col.vars))
            else {
                nam <- names(col.vars)
                ind <- which(!nzchar(nam))
                names(col.vars)[ind] <- paste0("Factor.", ind)
            }
        }
    }
    p <- 1
    n <- integer(n.row.vars)
    for (k in seq.int(from = 1, to = n.row.vars)) {
        n[k] <- sum(z >= max(z) - k + 1)/p
        p <- p * n[k]
    }
    is.row.lab <- rep(rep(c(TRUE, FALSE), length(z)), c(rbind(z - 
        min(z) + 1, min(z) - 1)))
    s <- scan(file, what = "", sep = sep, quote = quote, quiet = TRUE)
    values <- as.numeric(s[!is.row.lab])
    tmp <- s[is.row.lab]
    len <- length(tmp)
    for (k in seq.int(from = 1, to = n.row.vars)) {
        i <- seq.int(from = 1, to = len, by = len/n[k])
        row.vars[[k]] <- unique(tmp[i])
        tmp <- tmp[seq.int(from = 2, to = len/n[k])]
        len <- length(tmp)
    }
    values <- matrix(values, nrow = prod(lengths(row.vars)), 
        ncol = prod(lengths(col.vars)), byrow = TRUE)
    structure(values, row.vars = row.vars, col.vars = col.vars, 
        class = "ftable")
}


cancor <- function (x, y, xcenter = TRUE, ycenter = TRUE) 
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    if ((nr <- nrow(x)) != nrow(y)) 
        stop("unequal number of rows in 'cancor'")
    ncx <- ncol(x)
    ncy <- ncol(y)
    if (!nr || !ncx || !ncy) 
        stop("dimension 0 in 'x' or 'y'")
    if (is.logical(xcenter)) {
        if (xcenter) {
            xcenter <- colMeans(x, )
            x <- x - rep(xcenter, rep.int(nr, ncx))
        }
        else xcenter <- rep.int(0, ncx)
    }
    else {
        xcenter <- rep_len(xcenter, ncx)
        x <- x - rep(xcenter, rep.int(nr, ncx))
    }
    if (is.logical(ycenter)) {
        if (ycenter) {
            ycenter <- colMeans(y)
            y <- y - rep(ycenter, rep.int(nr, ncy))
        }
        else ycenter <- rep.int(0, ncy)
    }
    else {
        ycenter <- rep_len(ycenter, ncy)
        y <- y - rep(ycenter, rep.int(nr, ncy))
    }
    qx <- qr(x)
    qy <- qr(y)
    dx <- qx$rank
    if (!dx) 
        stop("'x' has rank 0")
    dy <- qy$rank
    if (!dy) 
        stop("'y' has rank 0")
    z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1L:dx, , 
        drop = FALSE], dx, dy)
    xcoef <- backsolve((qx$qr)[1L:dx, 1L:dx, drop = FALSE], z$u)
    rownames(xcoef) <- colnames(x)[qx$pivot][1L:dx]
    ycoef <- backsolve((qy$qr)[1L:dy, 1L:dy, drop = FALSE], z$v)
    rownames(ycoef) <- colnames(y)[qy$pivot][1L:dy]
    list(cor = z$d, xcoef = xcoef, ycoef = ycoef, xcenter = xcenter, 
        ycenter = ycenter)
}


fft <- function (z, inverse = FALSE) 
.Call(C_fft, z, inverse)


qpois <- function (p, lambda, lower.tail = TRUE, log.p = FALSE) 
.Call(C_qpois, p, lambda, lower.tail, log.p)


tsdiag <- function (object, gof.lag, ...) 
UseMethod("tsdiag")


power <- function (lambda = 1) 
{
    if (!is.numeric(lambda) || is.na(lambda)) 
        stop("invalid argument 'lambda'")
    if (lambda <= 0) 
        return(make.link("log"))
    if (lambda == 1) 
        return(make.link("identity"))
    linkfun <- function(mu) mu^lambda
    linkinv <- function(eta) pmax(eta^(1/lambda), .Machine$double.eps)
    mu.eta <- function(eta) pmax((1/lambda) * eta^(1/lambda - 
        1), .Machine$double.eps)
    valideta <- function(eta) all(is.finite(eta)) && all(eta > 
        0)
    link <- paste0("mu^", round(lambda, 3))
    structure(list(linkfun = linkfun, linkinv = linkinv, mu.eta = mu.eta, 
        valideta = valideta, name = link), class = "link-glm")
}


getInitial <- function (object, data, ...) 
UseMethod("getInitial")


SSgompertz <- function (x, Asym, b2, b3) 
{
    .expr2 <- b3^x
    .expr4 <- exp(-b2 * .expr2)
    .value <- Asym * .expr4
    .actualArgs <- as.list(match.call()[c("Asym", "b2", "b3")])
    if (all(unlist(lapply(.actualArgs, is.name)))) {
        .grad <- array(0, c(length(.value), 3L), list(NULL, c("Asym", 
            "b2", "b3")))
        .grad[, "Asym"] <- .expr4
        .grad[, "b2"] <- -Asym * (.expr4 * .expr2)
        .grad[, "b3"] <- -Asym * (.expr4 * (b2 * (b3^(x - 1) * 
            x)))
        dimnames(.grad) <- list(NULL, .actualArgs)
        attr(.value, "gradient") <- .grad
    }
    .value
}


bandwidth.kernel <- function (k) 
{
    i <- -k$m:k$m
    sqrt(sum((1/12 + i^2) * k[i]))
}


quade.test <- function (y, ...) 
UseMethod("quade.test")


mauchly.test <- function (object, ...) 
UseMethod("mauchly.test", object)


approx <- function (x, y = NULL, xout, method = "linear", n = 50, yleft, 
    yright, rule = 1, f = 0, ties = mean) 
{
    method <- pmatch(method, c("linear", "constant"))
    if (is.na(method)) 
        stop("invalid interpolation method")
    stopifnot(is.numeric(rule), (lenR <- length(rule)) >= 1L, 
        lenR <= 2L)
    if (lenR == 1) 
        rule <- rule[c(1, 1)]
    x <- regularize.values(x, y, ties)
    y <- x$y
    x <- x$x
    nx <- as.integer(length(x))
    if (is.na(nx)) 
        stop("invalid length(x)")
    if (nx <= 1) {
        if (method == 1) 
            stop("need at least two non-NA values to interpolate")
        if (nx == 0) 
            stop("zero non-NA points")
    }
    if (missing(yleft)) 
        yleft <- if (rule[1L] == 1) 
            NA
        else y[1L]
    if (missing(yright)) 
        yright <- if (rule[2L] == 1) 
            NA
        else y[length(y)]
    stopifnot(length(yleft) == 1L, length(yright) == 1L, length(f) == 
        1L)
    if (missing(xout)) {
        if (n <= 0) 
            stop("'approx' requires n >= 1")
        xout <- seq.int(x[1L], x[nx], length.out = n)
    }
    x <- as.double(x)
    y <- as.double(y)
    .Call(C_ApproxTest, x, y, method, f)
    yout <- .Call(C_Approx, x, y, xout, method, yleft, yright, 
        f)
    list(x = xout, y = yout)
}


.preformat.ts <- function (x, calendar, ...) 
{
    fr.x <- frequency(x)
    if (missing(calendar)) 
        calendar <- any(fr.x == c(4, 12)) && length(start(x)) == 
            2L
    Tsp <- tsp(x)
    if (is.null(Tsp)) 
        stop("series is corrupt, with no 'tsp' attribute")
    nn <- 1 + round((Tsp[2L] - Tsp[1L]) * Tsp[3L])
    if (NROW(x) != nn) {
        warning(gettextf("series is corrupt: length %d with 'tsp' implying %d", 
            NROW(x), nn), domain = NA, call. = FALSE)
        calendar <- FALSE
    }
    if (NCOL(x) == 1) {
        if (calendar) {
            if (fr.x > 1) {
                dn2 <- if (fr.x == 12) 
                  month.abb
                else if (fr.x == 4) {
                  c("Qtr1", "Qtr2", "Qtr3", "Qtr4")
                }
                else paste0("p", 1L:fr.x)
                if (NROW(x) <= fr.x && start(x)[1L] == end(x)[1L]) {
                  dn1 <- start(x)[1L]
                  dn2 <- dn2[1 + (start(x)[2L] - 2 + seq_along(x))%%fr.x]
                  x <- matrix(format(x, ...), nrow = 1L, byrow = TRUE, 
                    dimnames = list(dn1, dn2))
                }
                else {
                  start.pad <- start(x)[2L] - 1
                  end.pad <- fr.x - end(x)[2L]
                  dn1 <- start(x)[1L]:end(x)[1L]
                  x <- matrix(c(rep.int("", start.pad), format(x, 
                    ...), rep.int("", end.pad)), ncol = fr.x, 
                    byrow = TRUE, dimnames = list(dn1, dn2))
                }
            }
            else {
                tx <- time(x)
                attributes(x) <- NULL
                names(x) <- tx
            }
        }
        else {
            attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
        }
    }
    else {
        rownames(x) <- if (calendar && fr.x > 1) {
            tm <- time(x)
            t2 <- 1 + round(fr.x * ((tm + 0.001)%%1))
            p1 <- format(floor(zapsmall(tm, digits = 7)))
            if (fr.x == 12) 
                paste(month.abb[t2], p1)
            else paste(p1, if (fr.x == 4) 
                c("Q1", "Q2", "Q3", "Q4")[t2]
            else format(t2))
        }
        else format(time(x))
        attr(x, "class") <- attr(x, "tsp") <- attr(x, "na.action") <- NULL
    }
    x
}


deriv <- function (expr, ...) 
UseMethod("deriv")


spec.taper <- function (x, p = 0.1) 
{
    if (any(p < 0) || any(p > 0.5)) 
        stop("'p' must be between 0 and 0.5")
    a <- attributes(x)
    x <- as.matrix(x)
    nc <- ncol(x)
    if (length(p) == 1L) 
        p <- rep(p, nc)
    else if (length(p) != nc) 
        stop("length of 'p' must be 1 or equal the number of columns of 'x'")
    nr <- nrow(x)
    for (i in 1L:nc) {
        m <- floor(nr * p[i])
        if (m == 0) 
            next
        w <- 0.5 * (1 - cos(pi * seq.int(1, 2 * m - 1, by = 2)/(2 * 
            m)))
        x[, i] <- c(w, rep_len(1, nr - 2 * m), rev(w)) * x[, 
            i]
    }
    attributes(x) <- a
    x
}


ecdf <- function (x) 
{
    x <- sort(x)
    n <- length(x)
    if (n < 1) 
        stop("'x' must have 1 or more non-missing values")
    vals <- unique(x)
    rval <- approxfun(vals, cumsum(tabulate(match(x, vals)))/n, 
        method = "constant", yleft = 0, yright = 1, f = 0, ties = "ordered")
    class(rval) <- c("ecdf", "stepfun", class(rval))
    assign("nobs", n, envir = environment(rval))
    attr(rval, "call") <- sys.call()
    rval
}


dhyper <- function (x, m, n, k, log = FALSE) 
.Call(C_dhyper, x, m, n, k, log)


dpois <- function (x, lambda, log = FALSE) 
.Call(C_dpois, x, lambda, log)


setNames <- function (object = nm, nm) 
{
    names(object) <- nm
    object
}


dlogis <- function (x, location = 0, scale = 1, log = FALSE) 
.Call(C_dlogis, x, location, scale, log)





## Package Info

.skeleton_package_title = "The R Stats Package"

.skeleton_package_version = "3.4.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF