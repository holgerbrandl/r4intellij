##
## Exported symobls in package `MASS`
##

## Exported package methods

Shepard <- function (d, x, p = 2) 
{
    n <- as.integer(nrow(x))
    if (is.na(n)) 
        stop("invalid row(x)")
    k <- ncol(x)
    y <- dist(x, method = "minkowski", p = p)
    ord <- order(d)
    y <- y[ord]
    nd <- length(ord)
    if (is.na(nd)) 
        stop("invalid length(d)")
    Z <- .C(VR_mds_fn, as.double(y), yf = as.double(y), as.integer(nd), 
        ssq = double(1), as.integer(order(ord) - 1), as.double(x), 
        as.integer(n), as.integer(k), g = double(n * k), as.integer(1L), 
        as.double(2))
    list(x = d[ord], y = y, yf = Z$yf)
}


cov.mcd <- function (...) 
{
    oc <- sys.call()
    oc$method <- "mcd"
    oc[[1L]] <- quote(MASS::cov.rob)
    eval.parent(oc)
}


fbeta <- function (x, alpha, beta) 
{
    x^(alpha - 1) * (1 - x)^(beta - 1)
}


lmwork <- function (object) 
{
    resid <- object$residuals
    hat <- lm.influence(object, do.coef = FALSE)$hat
    hat <- hat[hat > 0]
    ok <- !(is.na(resid))
    n.miss <- sum(!ok)
    if (n.miss) 
        warning(sprintf(ngettext(n.miss, "%d missing observation deleted", 
            "%d missing observations deleted"), n.miss), domain = NA)
    resid <- resid[ok]
    n <- length(resid)
    p <- object$rank
    rdf <- object$df.residual
    if (is.null(rdf)) 
        rdf <- n - p
    if (!is.null(object$weights)) {
        wt <- object$weights[ok]
        resid <- resid * wt^0.5
        excl <- wt == 0
        if (any(excl)) {
            warning(sprintf(ngettext(sum(excl), "%d row with zero weights not counted", 
                "%d rows with zero weights not counted"), sum(excl)), 
                domain = NA)
            resid <- resid[!excl]
            if (is.null(object$df.resid)) 
                rdf <- rdf - sum(excl)
        }
    }
    stdres <- studres <- resid
    if (n > p) {
        stddev <- sqrt(sum(resid^2)/rdf)
        sr <- resid/(sqrt(1 - hat) * stddev)
        stdres <- sr
        studres <- sr/sqrt((n - p - sr^2)/(n - p - 1))
        if (!is.null(object$na.action)) {
            stdres <- naresid(object$na.action, stdres)
            studres <- naresid(object$na.action, studres)
        }
    }
    else stddev <- stdres[] <- studres[] <- NA
    list(stddev = stddev, stdres = stdres, studres = studres)
}


truehist <- function (data, nbins = "Scott", h, x0 = -h/1000, breaks, prob = TRUE, 
    xlim = range(breaks), ymax = max(est), col = "cyan", xlab = deparse(substitute(data)), 
    bty = "n", ...) 
{
    plot.truehist <- function(breaks, est, xlim, ymax, bty, xlab, 
        ylab = "", density = NULL, angle = 45, col = NULL, border = NULL, 
        lty = NULL, lwd = par("lwd"), ...) {
        n <- length(breaks)
        dev.hold()
        on.exit(dev.flush())
        plot(xlim, c(0, ymax), type = "n", xlab = xlab, ylab = ylab, 
            bty = bty, ...)
        rect(breaks[-n], 0, breaks[-1L], est, density = density, 
            angle = angle, col = col, border = border, lty = lty, 
            lwd = lwd)
    }
    xlab
    data <- data[is.finite(data)]
    if (missing(breaks)) {
        if (missing(h)) {
            if (is.character(nbins)) 
                nbins <- switch(casefold(nbins), scott = nclass.scott(data), 
                  `freedman-diaconis` = , fd = nclass.FD(data))
            if (!is.finite(nbins) || nbins <= 0) 
                stop("'nbins' must result in a positive integer")
            h <- diff(pretty(data, nbins))[1L]
        }
        if (!is.finite(h) || h <= 0) 
            stop("'h' must be strictly positive")
        first <- floor((min(data) - x0)/h)
        last <- ceiling((max(data) - x0)/h)
        breaks <- x0 + h * c(first:last)
    }
    if (any(diff(breaks) <= 0)) 
        stop("'breaks' must be strictly increasing")
    if (min(data) < min(breaks) || max(data) > max(breaks)) 
        stop("'breaks' do not cover the data")
    db <- diff(breaks)
    if (!prob && sqrt(var(db)) > mean(db)/1000) 
        warning("uneven breaks with 'prob = FALSE' will give a misleading plot")
    bin <- cut(data, breaks, include.lowest = TRUE)
    est <- tabulate(bin, length(levels(bin)))
    if (prob) 
        est <- est/(diff(breaks) * length(data))
    plot.truehist(breaks, est, xlim, ymax, bty = bty, xlab = xlab, 
        col = col, ...)
    invisible()
}


mca <- function (df, nf = 2, abbrev = FALSE) 
{
    class.ind <- function(cl) {
        n <- length(cl)
        cl <- as.factor(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (unclass(cl) - 1)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    if (!all(unlist(lapply(df, is.factor)))) 
        stop("all variables must be factors")
    Call <- match.call()
    n <- nrow(df)
    p <- length(df)
    G <- as.matrix(do.call("data.frame", c(lapply(df, class.ind), 
        check.names = FALSE)))
    Dc <- drop((rep(1, n)) %*% G)
    X <- t(t(G)/(sqrt(p * Dc)))
    X.svd <- svd(X)
    sec <- 1 + (1L:nf)
    rs <- X %*% X.svd$v[, sec]/p
    cs <- diag(1/(sqrt(p * Dc))) %*% X.svd$v[, sec]
    fs <- X.svd$u[, sec]/rep(p * X.svd$d[sec], rep(n, nf))
    dimnames(rs) <- list(row.names(df), as.character(1L:nf))
    dimnames(fs) <- dimnames(rs)
    varnames <- if (abbrev) 
        unlist(lapply(df, levels))
    else colnames(G)
    dimnames(cs) <- list(varnames, as.character(1L:nf))
    structure(list(rs = rs, cs = cs, fs = fs, d = X.svd$d[sec], 
        p = p, call = Call), class = "mca")
}


psi.huber <- function (u, k = 1.345, deriv = 0) 
{
    if (!deriv) 
        return(pmin(1, k/abs(u)))
    abs(u) <= k
}


rational <- function (x, cycles = 10, max.denominator = 2000, ...) 
{
    ans <- .rat(x, cycles, max.denominator)$rat
    do.call("structure", c(list(ans[, 1]/ans[, 2]), attributes(x)))
}


psi.hampel <- function (u, a = 2, b = 4, c = 8, deriv = 0) 
{
    U <- pmin(abs(u) + 1e-50, c)
    if (!deriv) 
        return(ifelse(U <= a, U, ifelse(U <= b, a, a * (c - U)/(c - 
            b)))/U)
    ifelse(abs(u) <= c, ifelse(U <= a, 1, ifelse(U <= b, 0, -a/(c - 
        b))), 0)
}


lm.ridge <- function (formula, data, subset, na.action, lambda = 0, model = FALSE, 
    x = FALSE, y = FALSE, contrasts = NULL, ...) 
{
    m <- match.call(expand.dots = FALSE)
    m$model <- m$x <- m$y <- m$contrasts <- m$... <- m$lambda <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    Y <- model.response(m)
    X <- model.matrix(Terms, m, contrasts)
    n <- nrow(X)
    p <- ncol(X)
    offset <- model.offset(m)
    if (!is.null(offset)) 
        Y <- Y - offset
    if (Inter <- attr(Terms, "intercept")) {
        Xm <- colMeans(X[, -Inter])
        Ym <- mean(Y)
        p <- p - 1
        X <- X[, -Inter] - rep(Xm, rep(n, p))
        Y <- Y - Ym
    }
    else Ym <- Xm <- NA
    Xscale <- drop(rep(1/n, n) %*% X^2)^0.5
    X <- X/rep(Xscale, rep(n, p))
    Xs <- svd(X)
    rhs <- t(Xs$u) %*% Y
    d <- Xs$d
    lscoef <- Xs$v %*% (rhs/d)
    lsfit <- X %*% lscoef
    resid <- Y - lsfit
    s2 <- sum(resid^2)/(n - p - Inter)
    HKB <- (p - 2) * s2/sum(lscoef^2)
    LW <- (p - 2) * s2 * n/sum(lsfit^2)
    k <- length(lambda)
    dx <- length(d)
    div <- d^2 + rep(lambda, rep(dx, k))
    a <- drop(d * rhs)/div
    dim(a) <- c(dx, k)
    coef <- Xs$v %*% a
    dimnames(coef) <- list(names(Xscale), format(lambda))
    GCV <- colSums((Y - X %*% coef)^2)/(n - colSums(matrix(d^2/div, 
        dx)))^2
    res <- list(coef = drop(coef), scales = Xscale, Inter = Inter, 
        lambda = lambda, ym = Ym, xm = Xm, GCV = GCV, kHKB = HKB, 
        kLW = LW)
    class(res) <- "ridgelm"
    res
}


addterm <- function (object, ...) 
UseMethod("addterm")


cov.trob <- function (x, wt = rep(1, n), cor = FALSE, center = TRUE, nu = 5, 
    maxit = 25, tol = 0.01) 
{
    test.values <- function(x) {
        if (any(is.na(x)) || any(is.infinite(x))) 
            stop("missing or infinite values in 'x'")
    }
    scale.simp <- function(x, center, n, p) x - rep(center, rep(n, 
        p))
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    dn <- colnames(x)
    test.values(x)
    if (!(miss.wt <- missing(wt))) {
        wt0 <- wt
        test.values(wt)
        if (length(wt) != n) 
            stop("length of 'wt' must equal number of observations")
        if (any(wt < 0)) 
            stop("negative weights not allowed")
        if (!sum(wt)) 
            stop("no positive weights")
        x <- x[wt > 0, , drop = FALSE]
        wt <- wt[wt > 0]
        n <- nrow(x)
    }
    loc <- colSums(wt * x)/sum(wt)
    if (is.numeric(center)) {
        if (length(center) != p) 
            stop("'center' is not the right length")
        loc <- center
    }
    else if (is.logical(center) && !center) 
        loc <- rep(0, p)
    use.loc <- is.logical(center) && center
    w <- wt * (1 + p/nu)
    endit <- 0
    for (iter in 1L:maxit) {
        w0 <- w
        X <- scale.simp(x, loc, n, p)
        sX <- svd(sqrt(w/sum(w)) * X, nu = 0)
        wX <- X %*% sX$v %*% diag(1/sX$d, , p)
        Q <- drop(wX^2 %*% rep(1, p))
        w <- (wt * (nu + p))/(nu + Q)
        if (use.loc) 
            loc <- colSums(w * x)/sum(w)
        if (all(abs(w - w0) < tol)) 
            break
        endit <- iter
    }
    if (endit == maxit || abs(mean(w) - mean(wt)) > tol || abs(mean(w * 
        Q)/p - 1) > tol) 
        warning("Probable convergence failure")
    cov <- crossprod(sqrt(w) * X)/sum(wt)
    if (length(dn)) {
        dimnames(cov) <- list(dn, dn)
        names(loc) <- dn
    }
    if (miss.wt) 
        ans <- list(cov = cov, center = loc, n.obs = n)
    else ans <- list(cov = cov, center = loc, wt = wt0, n.obs = n)
    if (cor) {
        sd <- sqrt(diag(cov))
        cor <- (cov/sd)/rep(sd, rep.int(p, p))
        if (length(dn)) 
            dimnames(cor) <- list(dn, dn)
        ans <- c(ans, list(cor = cor))
    }
    ans$call <- match.call()
    ans$iter <- endit
    ans
}


ginv <- function (X, tol = sqrt(.Machine$double.eps)) 
{
    if (length(dim(X)) > 2L || !(is.numeric(X) || is.complex(X))) 
        stop("'X' must be a numeric or complex matrix")
    if (!is.matrix(X)) 
        X <- as.matrix(X)
    Xsvd <- svd(X)
    if (is.complex(X)) 
        Xsvd$u <- Conj(Xsvd$u)
    Positive <- Xsvd$d > max(tol * Xsvd$d[1L], 0)
    if (all(Positive)) 
        Xsvd$v %*% (1/Xsvd$d * t(Xsvd$u))
    else if (!any(Positive)) 
        array(0, dim(X)[2L:1L])
    else Xsvd$v[, Positive, drop = FALSE] %*% ((1/Xsvd$d[Positive]) * 
        t(Xsvd$u[, Positive, drop = FALSE]))
}


hist.FD <- function (x, prob = TRUE, xlab = deparse(substitute(x)), ...) 
invisible(hist(x, nclass.FD(x), prob = prob, xlab = xlab, ...))


parcoord <- function (x, col = 1, lty = 1, var.label = FALSE, ...) 
{
    rx <- apply(x, 2L, range, na.rm = TRUE)
    x <- apply(x, 2L, function(x) (x - min(x, na.rm = TRUE))/(max(x, 
        na.rm = TRUE) - min(x, na.rm = TRUE)))
    matplot(1L:ncol(x), t(x), type = "l", col = col, lty = lty, 
        xlab = "", ylab = "", axes = FALSE, ...)
    axis(1, at = 1L:ncol(x), labels = colnames(x))
    for (i in 1L:ncol(x)) {
        lines(c(i, i), c(0, 1), col = "grey70")
        if (var.label) 
            text(c(i, i), c(0, 1), labels = format(rx[, i], digits = 3), 
                xpd = NA, offset = 0.3, pos = c(1, 3), cex = 0.7)
    }
    invisible()
}


negative.binomial <- function (theta = stop("'theta' must be specified"), link = "log") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    if (linktemp %in% c("log", "identity", "sqrt")) 
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
        else stop(gettextf("\"%s\" link not available for negative binomial family; available links are \"identity\", \"log\" and \"sqrt\"", 
            linktemp))
    }
    .Theta <- theta
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", theta, envir = env)
    variance <- function(mu) mu + mu^2/.Theta
    validmu <- function(mu) all(mu > 0)
    dev.resids <- function(y, mu, wt) 2 * wt * (y * log(pmax(1, 
        y)/mu) - (y + .Theta) * log((y + .Theta)/(mu + .Theta)))
    aic <- function(y, n, mu, wt, dev) {
        term <- (y + .Theta) * log(mu + .Theta) - y * log(mu) + 
            lgamma(y + 1) - .Theta * log(.Theta) + lgamma(.Theta) - 
            lgamma(.Theta + y)
        2 * sum(term * wt)
    }
    initialize <- expression({
        if (any(y < 0)) stop("negative values not allowed for the negative binomial family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0)/6
    })
    simfun <- function(object, nsim) {
        ftd <- fitted(object)
        rnegbin(nsim * length(ftd), ftd, .Theta)
    }
    environment(variance) <- environment(validmu) <- environment(dev.resids) <- environment(aic) <- environment(simfun) <- env
    famname <- paste("Negative Binomial(", format(round(theta, 
        4)), ")", sep = "")
    structure(list(family = famname, link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, variance = variance, dev.resids = dev.resids, 
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
        validmu = validmu, valideta = stats$valideta, simulate = simfun), 
        class = "family")
}


dose.p <- function (obj, cf = 1:2, p = 0.5) 
{
    eta <- family(obj)$linkfun(p)
    b <- coef(obj)[cf]
    x.p <- (eta - b[1L])/b[2L]
    names(x.p) <- paste("p = ", format(p), ":", sep = "")
    pd <- -cbind(1, x.p)/b[2L]
    SE <- sqrt(((pd %*% vcov(obj)[cf, cf]) * pd) %*% c(1, 1))
    res <- structure(x.p, SE = SE, p = p)
    class(res) <- "glm.dose"
    res
}


area <- function (f, a, b, ..., fa = f(a, ...), fb = f(b, ...), limit = 10, 
    eps = 1e-05) 
{
    h <- b - a
    d <- (a + b)/2
    fd <- f(d, ...)
    a1 <- ((fa + fb) * h)/2
    a2 <- ((fa + 4 * fd + fb) * h)/6
    if (abs(a1 - a2) < eps) 
        return(a2)
    if (limit == 0) {
        warning(gettextf("iteration limit reached near 'x = %f'", 
            d), doman = NA)
        return(a2)
    }
    Recall(f, a, d, ..., fa = fa, fb = fd, limit = limit - 1, 
        eps = eps) + Recall(f, d, b, ..., fa = fd, fb = fb, limit = limit - 
        1, eps = eps)
}


rlm <- function (x, ...) 
UseMethod("rlm")


contr.sdif <- function (n, contrasts = TRUE, sparse = FALSE) 
{
    if (is.numeric(n) && length(n) == 1L) {
        if (n%%1L || n < 2L) 
            stop("invalid number of levels")
        lab <- as.character(seq(n))
    }
    else {
        lab <- as.character(n)
        n <- length(n)
        if (n < 2L) 
            stop("invalid number of levels")
    }
    if (contrasts) {
        cont <- col(matrix(nrow = n, ncol = n - 1L))
        upper.tri <- !lower.tri(cont)
        cont[upper.tri] <- cont[upper.tri] - n
        structure(cont/n, dimnames = list(lab, paste(lab[-1L], 
            lab[-n], sep = "-")))
    }
    else .Diag(lab, sparse = sparse)
}


as.fractions <- function (x) 
if (is.fractions(x)) x else fractions(x)


gamma.dispersion <- function (object, ...) 
1/gamma.shape(object, ...)[[1L]]


con2tr <- function (obj) 
{
    data.frame(expand.grid(x = obj$x, y = obj$y), z = as.vector(obj$z))
}


lmsreg <- function (...) 
{
    oc <- sys.call()
    oc$method <- "lms"
    oc[[1L]] <- quote(MASS::lqs)
    eval.parent(oc)
}


lqs.formula <- function (formula, data, ..., method = c("lts", "lqs", "lms", 
    "S", "model.frame"), subset, na.action, model = TRUE, x.ret = FALSE, 
    y.ret = FALSE, contrasts = NULL) 
{
    method <- match.arg(method)
    mf <- match.call(expand.dots = FALSE)
    mf$method <- mf$contrasts <- mf$model <- mf$x.ret <- mf$y.ret <- mf$... <- NULL
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(mf)
    if (method == "model.frame") 
        return(mf)
    mt <- attr(mf, "terms")
    y <- model.extract(mf, "response")
    offset <- model.offset(mf)
    if (!is.null(offset)) 
        y <- y - offset
    x <- model.matrix(mt, mf, contrasts)
    contr <- attr(x, "contrasts")
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    if (xint) 
        x <- x[, -xint, drop = FALSE]
    fit <- lqs.default(x, y, intercept = (xint > 0), method = method, 
        ...)
    fit$terms <- mt
    fit$call <- match.call()
    fit$contrasts <- contr
    fit$xlevels <- .getXlevels(mt, mf)
    fit$na.action <- attr(mf, "na.action")
    if (model) 
        fit$model <- mf
    if (x.ret) 
        fit$x <- x
    if (y.ret) 
        fit$y <- y
    fit
}


psi.bisquare <- function (u, c = 4.685, deriv = 0) 
{
    if (!deriv) 
        return((1 - pmin(1, abs(u/c))^2)^2)
    t <- (u/c)^2
    ifelse(t < 1, (1 - t) * (1 - 5 * t), 0)
}


polr <- function (formula, data, weights, start, ..., subset, na.action, 
    contrasts = NULL, Hess = FALSE, model = TRUE, method = c("logistic", 
        "probit", "loglog", "cloglog", "cauchit")) 
{
    m <- match.call(expand.dots = FALSE)
    method <- match.arg(method)
    if (is.matrix(eval.parent(m$data))) 
        m$data <- as.data.frame(data)
    m$start <- m$Hess <- m$method <- m$model <- m$... <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    n <- nrow(x)
    pc <- ncol(x)
    cons <- attr(x, "contrasts")
    if (xint > 0L) {
        x <- x[, -xint, drop = FALSE]
        pc <- pc - 1L
    }
    else warning("an intercept is needed and assumed")
    wt <- model.weights(m)
    if (!length(wt)) 
        wt <- rep(1, n)
    offset <- model.offset(m)
    if (length(offset) <= 1L) 
        offset <- rep(0, n)
    y <- model.response(m)
    if (!is.factor(y)) 
        stop("response must be a factor")
    lev <- levels(y)
    llev <- length(lev)
    if (llev <= 2L) 
        stop("response must have 3 or more levels")
    y <- unclass(y)
    q <- llev - 1L
    if (missing(start)) {
        q1 <- llev%/%2L
        y1 <- (y > q1)
        X <- cbind(Intercept = rep(1, n), x)
        fit <- switch(method, logistic = glm.fit(X, y1, wt, family = binomial(), 
            offset = offset), probit = glm.fit(X, y1, wt, family = binomial("probit"), 
            offset = offset), loglog = glm.fit(X, y1, wt, family = binomial("probit"), 
            offset = offset), cloglog = glm.fit(X, y1, wt, family = binomial("probit"), 
            offset = offset), cauchit = glm.fit(X, y1, wt, family = binomial("cauchit"), 
            offset = offset))
        if (!fit$converged) 
            stop("attempt to find suitable starting values failed")
        coefs <- fit$coefficients
        if (any(is.na(coefs))) {
            warning("design appears to be rank-deficient, so dropping some coefs")
            keep <- names(coefs)[!is.na(coefs)]
            coefs <- coefs[keep]
            x <- x[, keep[-1L], drop = FALSE]
            pc <- ncol(x)
        }
        logit <- function(p) log(p/(1 - p))
        spacing <- logit((1L:q)/(q + 1L))
        if (method != "logistic") 
            spacing <- spacing/1.7
        gammas <- -coefs[1L] + spacing - spacing[q1]
        start <- c(coefs[-1L], gammas)
    }
    else if (length(start) != pc + q) 
        stop("'start' is not of the correct length")
    ans <- polr.fit(x, y, wt, start, offset, method, hessian = Hess, 
        ...)
    beta <- ans$coefficients
    zeta <- ans$zeta
    deviance <- ans$deviance
    res <- ans$res
    niter <- c(f.evals = res$counts[1L], g.evals = res$counts[2L])
    eta <- if (pc) 
        offset + drop(x %*% beta)
    else offset + rep(0, n)
    pfun <- switch(method, logistic = plogis, probit = pnorm, 
        loglog = pgumbel, cloglog = pGumbel, cauchit = pcauchy)
    cumpr <- matrix(pfun(matrix(zeta, n, q, byrow = TRUE) - eta), 
        , q)
    fitted <- t(apply(cumpr, 1L, function(x) diff(c(0, x, 1))))
    dimnames(fitted) <- list(row.names(m), lev)
    fit <- list(coefficients = beta, zeta = zeta, deviance = deviance, 
        fitted.values = fitted, lev = lev, terms = Terms, df.residual = sum(wt) - 
            pc - q, edf = pc + q, n = sum(wt), nobs = sum(wt), 
        call = match.call(), method = method, convergence = res$convergence, 
        niter = niter, lp = eta)
    if (Hess) {
        dn <- c(names(beta), names(zeta))
        H <- res$hessian
        dimnames(H) <- list(dn, dn)
        fit$Hessian <- H
    }
    if (model) 
        fit$model <- m
    fit$na.action <- attr(m, "na.action")
    fit$contrasts <- cons
    fit$xlevels <- .getXlevels(Terms, m)
    class(fit) <- "polr"
    fit
}


stepAIC <- function (object, scope, scale = 0, direction = c("both", "backward", 
    "forward"), trace = 1, keep = NULL, steps = 1000, use.start = FALSE, 
    k = 2, ...) 
{
    mydeviance <- function(x, ...) {
        dev <- deviance(x)
        if (!is.null(dev)) 
            dev
        else extractAIC(x, k = 0)[2L]
    }
    cut.string <- function(string) {
        if (length(string) > 1L) 
            string[-1L] <- paste("\n", string[-1L], sep = "")
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
        ddf <- c(NA, abs(diff(rdf)))
        AIC <- sapply(models, "[[", "AIC")
        heading <- c("Stepwise Model Path \nAnalysis of Deviance Table", 
            "\nInitial Model:", deparse(formula(object)), "\nFinal Model:", 
            deparse(formula(fit)), "\n")
        aod <- if (usingCp) 
            data.frame(Step = change, Df = ddf, Deviance = dd, 
                `Resid. Df` = rdf, `Resid. Dev` = rd, Cp = AIC, 
                check.names = FALSE)
        else data.frame(Step = change, Df = ddf, Deviance = dd, 
            `Resid. Df` = rdf, `Resid. Dev` = rd, AIC = AIC, 
            check.names = FALSE)
        attr(aod, "heading") <- heading
        class(aod) <- c("Anova", "data.frame")
        fit$anova <- aod
        fit
    }
    Terms <- terms(object)
    object$formula <- Terms
    if (inherits(object, "lme")) 
        object$call$fixed <- Terms
    else if (inherits(object, "gls")) 
        object$call$model <- Terms
    else object$call$formula <- Terms
    if (use.start) 
        warning("'use.start' cannot be used with R's version of 'glm'")
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
        stop("AIC is not defined for this model, so 'stepAIC' cannot proceed")
    if (bAIC == -Inf) 
        stop("AIC is -infinity for this model, so 'stepAIC' cannot proceed")
    nm <- 1
    Terms <- terms(fit)
    if (trace) {
        cat("Start:  AIC=", format(round(bAIC, 2)), "\n", cut.string(deparse(formula(fit))), 
            "\n\n", sep = "")
        utils::flush.console()
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
        if (!is.null(sp <- attr(Terms, "specials")) && !is.null(st <- sp$strata)) 
            ffac <- ffac[-st, ]
        scope <- factor.scope(ffac, list(add = fadd, drop = fdrop))
        aod <- NULL
        change <- NULL
        if (backward && length(scope$drop)) {
            aod <- dropterm(fit, scope$drop, scale = scale, trace = max(0, 
                trace - 1), k = k, ...)
            rn <- row.names(aod)
            row.names(aod) <- c(rn[1L], paste("-", rn[-1L], sep = " "))
            if (any(aod$Df == 0, na.rm = TRUE)) {
                zdf <- aod$Df == 0 & !is.na(aod$Df)
                nc <- match(c("Cp", "AIC"), names(aod))
                nc <- nc[!is.na(nc)][1L]
                ch <- abs(aod[zdf, nc] - aod[1, nc]) > 0.01
                if (any(is.finite(ch) & ch)) {
                  warning("0 df terms are changing AIC")
                  zdf <- zdf[!ch]
                }
                if (length(zdf) > 0L) 
                  change <- rev(rownames(aod)[zdf])[1L]
            }
        }
        if (is.null(change)) {
            if (forward && length(scope$add)) {
                aodf <- addterm(fit, scope$add, scale = scale, 
                  trace = max(0, trace - 1), k = k, ...)
                rn <- row.names(aodf)
                row.names(aodf) <- c(rn[1L], paste("+", rn[-1L], 
                  sep = " "))
                aod <- if (is.null(aod)) 
                  aodf
                else rbind(aod, aodf[-1, , drop = FALSE])
            }
            attr(aod, "heading") <- NULL
            if (is.null(aod) || ncol(aod) == 0) 
                break
            nzdf <- if (!is.null(aod$Df)) 
                aod$Df != 0 | is.na(aod$Df)
            aod <- aod[nzdf, ]
            if (is.null(aod) || ncol(aod) == 0) 
                break
            nc <- match(c("Cp", "AIC"), names(aod))
            nc <- nc[!is.na(nc)][1L]
            o <- order(aod[, nc])
            if (trace) {
                print(aod[o, ])
                utils::flush.console()
            }
            if (o[1L] == 1) 
                break
            change <- rownames(aod)[o[1L]]
        }
        usingCp <- match("Cp", names(aod), 0) > 0
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
            utils::flush.console()
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


is.fractions <- function (f) 
inherits(f, "fractions")


enlist <- function (vec) 
{
    x <- as.list(vec)
    names(x) <- names(vec)
    x
}


theta.md <- function (y, mu, dfr, weights, limit = 20, eps = .Machine$double.eps^0.25) 
{
    if (inherits(y, "lm")) {
        mu <- y$fitted.values
        dfr <- y$df.residual
        y <- if (is.null(y$y)) 
            mu + residuals(y)
        else y$y
    }
    if (missing(weights)) 
        weights <- rep(1, length(y))
    n <- sum(weights)
    t0 <- n/sum(weights * (y/mu - 1)^2)
    a <- 2 * sum(weights * y * log(pmax(1, y)/mu)) - dfr
    it <- 0
    del <- 1
    while ((it <- it + 1) < limit && abs(del) > eps) {
        t0 <- abs(t0)
        tmp <- log((y + t0)/(mu + t0))
        top <- a - 2 * sum(weights * (y + t0) * tmp)
        bot <- 2 * sum(weights * ((y - mu)/(mu + t0) - tmp))
        del <- top/bot
        t0 <- t0 - del
    }
    if (t0 < 0) {
        t0 <- 0
        warning("estimate truncated at zero")
        attr(t0, "warn") <- gettext("estimate truncated at zero")
    }
    t0
}


qda <- function (x, ...) 
UseMethod("qda")


fitdistr <- function (x, densfun, start, ...) 
{
    myfn <- function(parm, ...) -sum(log(dens(parm, ...)))
    mylogfn <- function(parm, ...) -sum(dens(parm, ..., log = TRUE))
    mydt <- function(x, m, s, df, log) dt((x - m)/s, df, log = TRUE) - 
        log(s)
    Call <- match.call(expand.dots = TRUE)
    if (missing(start)) 
        start <- NULL
    dots <- names(list(...))
    dots <- dots[!is.element(dots, c("upper", "lower"))]
    if (missing(x) || length(x) == 0L || mode(x) != "numeric") 
        stop("'x' must be a non-empty numeric vector")
    if (any(!is.finite(x))) 
        stop("'x' contains missing or infinite values")
    if (missing(densfun) || !(is.function(densfun) || is.character(densfun))) 
        stop("'densfun' must be supplied as a function or name")
    control <- list()
    n <- length(x)
    if (is.character(densfun)) {
        distname <- tolower(densfun)
        densfun <- switch(distname, beta = dbeta, cauchy = dcauchy, 
            `chi-squared` = dchisq, exponential = dexp, f = df, 
            gamma = dgamma, geometric = dgeom, `log-normal` = dlnorm, 
            lognormal = dlnorm, logistic = dlogis, `negative binomial` = dnbinom, 
            normal = dnorm, poisson = dpois, t = mydt, weibull = dweibull, 
            NULL)
        if (is.null(densfun)) 
            stop("unsupported distribution")
        if (distname %in% c("lognormal", "log-normal")) {
            if (!is.null(start)) 
                stop(gettextf("supplying pars for the %s distribution is not supported", 
                  "log-Normal"), domain = NA)
            if (any(x <= 0)) 
                stop("need positive values to fit a log-Normal")
            lx <- log(x)
            sd0 <- sqrt((n - 1)/n) * sd(lx)
            mx <- mean(lx)
            estimate <- c(mx, sd0)
            sds <- c(sd0/sqrt(n), sd0/sqrt(2 * n))
            names(estimate) <- names(sds) <- c("meanlog", "sdlog")
            vc <- matrix(c(sds[1]^2, 0, 0, sds[2]^2), ncol = 2, 
                dimnames = list(names(sds), names(sds)))
            names(estimate) <- names(sds) <- c("meanlog", "sdlog")
            return(structure(list(estimate = estimate, sd = sds, 
                vcov = vc, n = n, loglik = sum(dlnorm(x, mx, 
                  sd0, log = TRUE))), class = "fitdistr"))
        }
        if (distname == "normal") {
            if (!is.null(start)) 
                stop(gettextf("supplying pars for the %s distribution is not supported", 
                  "Normal"), domain = NA)
            sd0 <- sqrt((n - 1)/n) * sd(x)
            mx <- mean(x)
            estimate <- c(mx, sd0)
            sds <- c(sd0/sqrt(n), sd0/sqrt(2 * n))
            names(estimate) <- names(sds) <- c("mean", "sd")
            vc <- matrix(c(sds[1]^2, 0, 0, sds[2]^2), ncol = 2, 
                dimnames = list(names(sds), names(sds)))
            return(structure(list(estimate = estimate, sd = sds, 
                vcov = vc, n = n, loglik = sum(dnorm(x, mx, sd0, 
                  log = TRUE))), class = "fitdistr"))
        }
        if (distname == "poisson") {
            if (!is.null(start)) 
                stop(gettextf("supplying pars for the %s distribution is not supported", 
                  "Poisson"), domain = NA)
            estimate <- mean(x)
            sds <- sqrt(estimate/n)
            names(estimate) <- names(sds) <- "lambda"
            vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("lambda", 
                "lambda"))
            return(structure(list(estimate = estimate, sd = sds, 
                vcov = vc, n = n, loglik = sum(dpois(x, estimate, 
                  log = TRUE))), class = "fitdistr"))
        }
        if (distname == "exponential") {
            if (any(x < 0)) 
                stop("Exponential values must be >= 0")
            if (!is.null(start)) 
                stop(gettextf("supplying pars for the %s distribution is not supported", 
                  "exponential"), domain = NA)
            estimate <- 1/mean(x)
            sds <- estimate/sqrt(n)
            vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("rate", 
                "rate"))
            names(estimate) <- names(sds) <- "rate"
            return(structure(list(estimate = estimate, sd = sds, 
                vcov = vc, n = n, loglik = sum(dexp(x, estimate, 
                  log = TRUE))), class = "fitdistr"))
        }
        if (distname == "geometric") {
            if (!is.null(start)) 
                stop(gettextf("supplying pars for the %s distribution is not supported", 
                  "geometric"), domain = NA)
            estimate <- 1/(1 + mean(x))
            sds <- estimate * sqrt((1 - estimate)/n)
            vc <- matrix(sds^2, ncol = 1, nrow = 1, dimnames = list("prob", 
                "prob"))
            names(estimate) <- names(sds) <- "prob"
            return(structure(list(estimate = estimate, sd = sds, 
                vcov = vc, n = n, loglik = sum(dgeom(x, estimate, 
                  log = TRUE))), class = "fitdistr"))
        }
        if (distname == "weibull" && is.null(start)) {
            if (any(x <= 0)) 
                stop("Weibull values must be > 0")
            lx <- log(x)
            m <- mean(lx)
            v <- var(lx)
            shape <- 1.2/sqrt(v)
            scale <- exp(m + 0.572/shape)
            start <- list(shape = shape, scale = scale)
            start <- start[!is.element(names(start), dots)]
        }
        if (distname == "gamma" && is.null(start)) {
            if (any(x < 0)) 
                stop("gamma values must be >= 0")
            m <- mean(x)
            v <- var(x)
            start <- list(shape = m^2/v, rate = m/v)
            start <- start[!is.element(names(start), dots)]
            control <- list(parscale = c(1, start$rate))
        }
        if (distname == "negative binomial" && is.null(start)) {
            m <- mean(x)
            v <- var(x)
            size <- if (v > m) 
                m^2/(v - m)
            else 100
            start <- list(size = size, mu = m)
            start <- start[!is.element(names(start), dots)]
        }
        if (is.element(distname, c("cauchy", "logistic")) && 
            is.null(start)) {
            start <- list(location = median(x), scale = IQR(x)/2)
            start <- start[!is.element(names(start), dots)]
        }
        if (distname == "t" && is.null(start)) {
            start <- list(m = median(x), s = IQR(x)/2, df = 10)
            start <- start[!is.element(names(start), dots)]
        }
    }
    if (is.null(start) || !is.list(start)) 
        stop("'start' must be a named list")
    nm <- names(start)
    f <- formals(densfun)
    args <- names(f)
    m <- match(nm, args)
    if (any(is.na(m))) 
        stop("'start' specifies names which are not arguments to 'densfun'")
    formals(densfun) <- c(f[c(1, m)], f[-c(1, m)])
    dens <- function(parm, x, ...) densfun(x, parm, ...)
    if ((l <- length(nm)) > 1L) 
        body(dens) <- parse(text = paste("densfun(x,", paste("parm[", 
            1L:l, "]", collapse = ", "), ", ...)"))
    Call[[1L]] <- quote(stats::optim)
    Call$densfun <- Call$start <- NULL
    Call$x <- x
    Call$par <- start
    Call$fn <- if ("log" %in% args) 
        mylogfn
    else myfn
    Call$hessian <- TRUE
    if (length(control)) 
        Call$control <- control
    if (is.null(Call$method)) {
        if (any(c("lower", "upper") %in% names(Call))) 
            Call$method <- "L-BFGS-B"
        else if (length(start) > 1L) 
            Call$method <- "BFGS"
        else Call$method <- "Nelder-Mead"
    }
    res <- eval.parent(Call)
    if (res$convergence > 0L) 
        stop("optimization failed")
    vc <- solve(res$hessian)
    sds <- sqrt(diag(vc))
    structure(list(estimate = res$par, sd = sds, vcov = vc, loglik = -res$value, 
        n = n), class = "fitdistr")
}


rms.curv <- function (obj) 
{
    fit.val <- obj$m$fitted()
    v <- attr(fit.val, "gradient")
    if (is.null(v)) 
        stop("\"gradient\" attribute missing")
    a <- attr(fit.val, "hessian")
    if (is.null(a)) 
        stop("\"hessian\" attribute missing")
    p <- ncol(v)
    n <- nrow(v)
    s <- sqrt(deviance(obj)/(n - p))
    sp <- s * sqrt(p)
    D <- v
    for (j in 1L:p) D <- cbind(D, a[, 1L:j, j])
    qrd <- qr(D)
    Q <- qr.Q(qrd)
    rnk <- qrd$rank
    if (rnk <= p) 
        warning("regression apparently linear")
    Q1 <- Q[, 1L:rnk]
    C <- array(0, c(rnk, p, p))
    for (j in 1L:p) C[, , j] <- crossprod(Q1, a[, , j])
    C <- aperm(C, c(2, 3, 1))
    r11i <- solve(qr.R(qrd)[1L:p, 1L:p])
    ct <- 0
    for (j in 1L:p) {
        C[, , j] <- crossprod(r11i, C[, , j]) %*% r11i * sp
        ct <- ct + 2 * sum(C[, , j]^2) + sum(diag(C[, , j]))^2
    }
    ci <- 0
    for (j in (p + 1):rnk) {
        C[, , j] <- crossprod(r11i, C[, , j]) %*% r11i * sp
        ci <- ci + 2 * sum(C[, , j]^2) + sum(diag(C[, , j]))^2
    }
    ct <- sqrt(ct/(p * (p + 2)))
    ci <- sqrt(ci/(p * (p + 2)))
    pe <- ct * sqrt(qf(19/20, p, n - p))
    ic <- ci * sqrt(qf(19/20, p, n - p))
    val <- list(pe = pe, ic = ic, ct = ct, ci = ci, C = C)
    class(val) <- "rms.curv"
    val
}


bandwidth.nrd <- function (x) 
{
    r <- quantile(x, c(0.25, 0.75))
    h <- (r[2L] - r[1L])/1.34
    4 * 1.06 * min(sqrt(var(x)), h) * length(x)^(-1/5)
}


negexp.SSival <- function (mCall, data, LHS) 
{
    x <- eval(mCall[["x"]], data)
    if (length(x) < 3L) 
        stop("at least 3 distinct 'x' values are needed")
    y <- eval(LHS, data)
    mx <- mean(x)
    b <- as.vector(lsfit(cbind(x - mx, -(x - mx)^2/2), y)$coef)
    rx <- range(x)
    xh <- mx + b[2L]/b[3L]
    if (prod(xh - rx) < 0) 
        if (xh - rx[1L] > rx[2L] - xh) 
            rx[2L] <- xh
        else rx[1L] <- xh
    x0 <- c(rx[1L], sum(rx)/2, rx[2L])
    dy <- diff(b[1L] + b[2L] * (x0 - mx) - (b[3L] * (x0 - mx)^2)/2)
    th <- (x0[2L] - x0[1L])/log(dy[1L]/dy[2L])
    b <- as.vector(lsfit(exp(-x/th), y)$coef)
    pars <- list(b[1L], b[2L], th)
    names(pars) <- mCall[c("b0", "b1", "th")]
    print(unlist(pars))
    pars
}


neg.bin <- function (theta = stop("'theta' must be given")) 
{
    .Theta <- theta
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", theta, envir = env)
    stats <- make.link("log")
    variance <- function(mu) mu + mu^2/.Theta
    validmu <- function(mu) all(mu > 0)
    dev.resids <- function(y, mu, wt) 2 * wt * (y * log(pmax(1, 
        y)/mu) - (y + .Theta) * log((y + .Theta)/(mu + .Theta)))
    aic <- function(y, n, mu, wt, dev) {
        term <- (y + .Theta) * log(mu + .Theta) - y * log(mu) + 
            lgamma(y + 1) - .Theta * log(.Theta) + lgamma(.Theta) - 
            lgamma(.Theta + y)
        2 * sum(term * wt)
    }
    initialize <- expression({
        if (any(y < 0)) stop("negative values not allowed for the negative binomial family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0)/6
    })
    simfun <- function(object, nsim) {
        ftd <- fitted(object)
        rnegbin(nsim * length(ftd), ftd, .Theta)
    }
    environment(variance) <- environment(validmu) <- environment(dev.resids) <- environment(aic) <- environment(simfun) <- env
    structure(list(family = "Negative Binomial", link = "log", 
        linkfun = stats$linkfun, linkinv = stats$linkinv, variance = variance, 
        dev.resids = dev.resids, aic = aic, mu.eta = stats$mu.eta, 
        initialize = initialize, validmu = validmu, valideta = stats$valideta, 
        simulate = simfun), class = "family")
}


huber <- function (y, k = 1.5, tol = 1e-06) 
{
    y <- y[!is.na(y)]
    n <- length(y)
    mu <- median(y)
    s <- mad(y)
    if (s == 0) 
        stop("cannot estimate scale: MAD is zero for this sample")
    repeat {
        yy <- pmin(pmax(mu - k * s, y), mu + k * s)
        mu1 <- sum(yy)/n
        if (abs(mu - mu1) < tol * s) 
            break
        mu <- mu1
    }
    list(mu = mu, s = s)
}


lm.gls <- function (formula, data, W, subset, na.action, inverse = FALSE, 
    method = "qr", model = FALSE, x = FALSE, y = FALSE, contrasts = NULL, 
    ...) 
{
    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    m$W <- m$inverse <- m$method <- m$model <- m$x <- m$y <- m$contrasts <- m$... <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m <- eval.parent(m)
    if (method == "model.frame") 
        return(m)
    Terms <- attr(m, "terms")
    Y <- model.response(m)
    X <- model.matrix(Terms, m, contrasts)
    n <- nrow(X)
    if (any(dim(W) != c(n, n))) 
        stop("dim(W) is not correct")
    eW <- eigen(W, TRUE)
    d <- eW$values
    if (any(d <= 0)) 
        stop("'W' is not positive definite")
    A <- diag(d^ifelse(inverse, -0.5, 0.5)) %*% t(eW$vector)
    Ainv <- eW$vector %*% diag(d^ifelse(inverse, 0.5, -0.5))
    fit <- lm.fit(A %*% X, A %*% Y, method = method, ...)
    fit$fitted.values <- drop(Ainv %*% fit$fitted.values)
    fit$residuals <- drop(Ainv %*% fit$residuals)
    fit$terms <- Terms
    fit$call <- call
    if (model) 
        fit$model <- m
    if (x) 
        fit$x <- X
    if (y) 
        fit$y <- Y
    fit$na.action <- attr(m, "na.action")
    class(fit) <- "lm.gls"
    fit$xlevels <- .getXlevels(Terms, m)
    fit$contrasts <- attr(X, "contrasts")
    fit
}


theta.ml <- function (y, mu, n = sum(weights), weights, limit = 10, eps = .Machine$double.eps^0.25, 
    trace = FALSE) 
{
    score <- function(n, th, mu, y, w) sum(w * (digamma(th + 
        y) - digamma(th) + log(th) + 1 - log(th + mu) - (y + 
        th)/(mu + th)))
    info <- function(n, th, mu, y, w) sum(w * (-trigamma(th + 
        y) + trigamma(th) - 1/th + 2/(mu + th) - (y + th)/(mu + 
        th)^2))
    if (inherits(y, "lm")) {
        mu <- y$fitted.values
        y <- if (is.null(y$y)) 
            mu + residuals(y)
        else y$y
    }
    if (missing(weights)) 
        weights <- rep(1, length(y))
    t0 <- n/sum(weights * (y/mu - 1)^2)
    it <- 0
    del <- 1
    if (trace) 
        message(sprintf("theta.ml: iter %d 'theta = %f'", it, 
            signif(t0)), domain = NA)
    while ((it <- it + 1) < limit && abs(del) > eps) {
        t0 <- abs(t0)
        del <- score(n, t0, mu, y, weights)/(i <- info(n, t0, 
            mu, y, weights))
        t0 <- t0 + del
        if (trace) 
            message("theta.ml: iter", it, " theta =", signif(t0))
    }
    if (t0 < 0) {
        t0 <- 0
        warning("estimate truncated at zero")
        attr(t0, "warn") <- gettext("estimate truncated at zero")
    }
    if (it == limit) {
        warning("iteration limit reached")
        attr(t0, "warn") <- gettext("iteration limit reached")
    }
    attr(t0, "SE") <- sqrt(1/i)
    t0
}


mvrnorm <- function (n = 1, mu, Sigma, tol = 1e-06, empirical = FALSE, EISPACK = FALSE) 
{
    p <- length(mu)
    if (!all(dim(Sigma) == c(p, p))) 
        stop("incompatible arguments")
    if (EISPACK) 
        stop("'EISPACK' is no longer supported by R", domain = NA)
    eS <- eigen(Sigma, symmetric = TRUE)
    ev <- eS$values
    if (!all(ev >= -tol * abs(ev[1L]))) 
        stop("'Sigma' is not positive definite")
    X <- matrix(rnorm(p * n), n)
    if (empirical) {
        X <- scale(X, TRUE, FALSE)
        X <- X %*% svd(X, nu = 0)$v
        X <- scale(X, FALSE, TRUE)
    }
    X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% 
        t(X)
    nm <- names(mu)
    if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) 
        nm <- dn[[1L]]
    dimnames(X) <- list(nm, NULL)
    if (n == 1) 
        drop(X)
    else t(X)
}


loglm <- function (formula, data, subset, na.action, ...) 
{
    .call <- match.call()
    if (missing(data) || inherits(data, "data.frame")) {
        m <- match.call(expand.dots = FALSE)
        m$... <- NULL
        m[[1L]] <- quote(stats::model.frame)
        data <- eval.parent(m)
        .formula <- as.formula(attr(data, "terms"))
    }
    else {
        trms <- attr(data, "terms") <- terms(formula <- denumerate(formula))
        .formula <- renumerate(as.formula(trms))
    }
    loglm1(formula, data, ..., .call = .call, .formula = .formula)
}


loglm1 <- function (formula, data, ...) 
UseMethod("loglm1", data)


theta.mm <- function (y, mu, dfr, weights, limit = 10, eps = .Machine$double.eps^0.25) 
{
    if (inherits(y, "lm")) {
        mu <- y$fitted.values
        dfr <- y$df.residual
        y <- if (is.null(y$y)) 
            mu + residuals(y)
        else y$y
    }
    if (missing(weights)) 
        weights <- rep(1, length(y))
    n <- sum(weights)
    t0 <- n/sum(weights * (y/mu - 1)^2)
    it <- 0
    del <- 1
    while ((it <- it + 1) < limit && abs(del) > eps) {
        t0 <- abs(t0)
        del <- (sum(weights * ((y - mu)^2/(mu + mu^2/t0))) - 
            dfr)/sum(weights * (y - mu)^2/(mu + t0)^2)
        t0 <- t0 - del
    }
    if (t0 < 0) {
        t0 <- 0
        warning("estimate truncated at zero")
        attr(t0, "warn") <- gettext("estimate truncated at zero")
    }
    t0
}


ucv <- function (x, nb = 1000, lower = 0.1 * hmax, upper = hmax) 
{
    fucv <- function(h, x, n, d) .C(VR_ucv_bin, as.integer(n), 
        as.integer(length(x)), as.double(d), x, as.double(h), 
        u = double(1))$u
    n <- length(x)
    if (!n) 
        stop("'x' has length zero")
    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5) * 4
    storage.mode(x) <- "double"
    Z <- .C(VR_den_bin, as.integer(n), as.integer(nb), d = double(1), 
        x, cnt = integer(nb))
    d <- Z$d
    cnt <- as.integer(Z$cnt)
    h <- optimize(fucv, c(lower, upper), tol = 0.1 * lower, x = cnt, 
        n = n, d = d)$minimum
    if (h < 1.1 * lower | h > upper - 0.1 * lower) 
        warning("minimum occurred at one end of the range")
    h
}


rnegbin <- function (n, mu = n, theta = stop("'theta' must be specified")) 
{
    k <- if (length(n) > 1L) 
        length(n)
    else n
    rpois(k, (mu * rgamma(k, theta))/theta)
}


hist.scott <- function (x, prob = TRUE, xlab = deparse(substitute(x)), ...) 
invisible(hist(x, nclass.scott(x), prob = prob, xlab = xlab, 
    ...))


lda <- function (x, ...) 
UseMethod("lda")


dropterm <- function (object, ...) 
UseMethod("dropterm")


cov.mve <- function (...) 
{
    oc <- sys.call()
    oc$method <- "mve"
    oc[[1L]] <- quote(MASS::cov.rob)
    eval.parent(oc)
}


nclass.freq <- function (x) 
{
    h <- 2.15 * sqrt(var(x)) * length(x)^(-1/5)
    ceiling(diff(range(x))/h)
}


ltsreg <- function (...) 
{
    oc <- sys.call()
    oc$method <- "lts"
    oc[[1L]] <- quote(MASS::lqs)
    eval.parent(oc)
}


logtrans <- function (object, ...) 
UseMethod("logtrans")


glmmPQL <- function (fixed, random, family, data, correlation, weights, 
    control, niter = 10, verbose = TRUE, ...) 
{
    if (!requireNamespace("nlme", quietly = TRUE)) 
        stop("package 'nlme' is essential")
    if (is.character(family)) 
        family <- get(family)
    if (is.function(family)) 
        family <- family()
    if (is.null(family$family)) {
        print(family)
        stop("'family' not recognized")
    }
    m <- mcall <- Call <- match.call()
    nm <- names(m)[-1L]
    keep <- is.element(nm, c("weights", "data", "subset", "na.action"))
    for (i in nm[!keep]) m[[i]] <- NULL
    allvars <- if (is.list(random)) 
        allvars <- c(all.vars(fixed), names(random), unlist(lapply(random, 
            function(x) all.vars(formula(x)))))
    else c(all.vars(fixed), all.vars(random))
    Terms <- if (missing(data)) 
        terms(fixed)
    else terms(fixed, data = data)
    off <- attr(Terms, "offset")
    if (length(off <- attr(Terms, "offset"))) 
        allvars <- c(allvars, as.character(attr(Terms, "variables"))[off + 
            1])
    if (!missing(correlation) && !is.null(attr(correlation, "formula"))) 
        allvars <- c(allvars, all.vars(attr(correlation, "formula")))
    Call$fixed <- eval(fixed)
    Call$random <- eval(random)
    m$formula <- as.formula(paste("~", paste(allvars, collapse = "+")))
    environment(m$formula) <- environment(fixed)
    m$drop.unused.levels <- TRUE
    m[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(m)
    off <- model.offset(mf)
    if (is.null(off)) 
        off <- 0
    wts <- model.weights(mf)
    if (is.null(wts)) 
        wts <- rep(1, nrow(mf))
    mf$wts <- wts
    fit0 <- glm(formula = fixed, family = family, data = mf, 
        weights = wts, ...)
    w <- fit0$prior.weights
    eta <- fit0$linear.predictors
    zz <- eta + fit0$residuals - off
    wz <- fit0$weights
    fam <- family
    nm <- names(mcall)[-1L]
    keep <- is.element(nm, c("fixed", "random", "data", "subset", 
        "na.action", "control"))
    for (i in nm[!keep]) mcall[[i]] <- NULL
    fixed[[2L]] <- quote(zz)
    mcall[["fixed"]] <- fixed
    mcall[[1L]] <- quote(nlme::lme)
    mcall$random <- random
    mcall$method <- "ML"
    if (!missing(correlation)) 
        mcall$correlation <- correlation
    mcall$weights <- quote(nlme::varFixed(~invwt))
    mf$zz <- zz
    mf$invwt <- 1/wz
    mcall$data <- mf
    for (i in seq_len(niter)) {
        if (verbose) 
            message(gettextf("iteration %d", i), domain = NA)
        fit <- eval(mcall)
        etaold <- eta
        eta <- fitted(fit) + off
        if (sum((eta - etaold)^2) < 1e-06 * sum(eta^2)) 
            break
        mu <- fam$linkinv(eta)
        mu.eta.val <- fam$mu.eta(eta)
        mf$zz <- eta + (fit0$y - mu)/mu.eta.val - off
        wz <- w * mu.eta.val^2/fam$variance(mu)
        mf$invwt <- 1/wz
        mcall$data <- mf
    }
    attributes(fit$logLik) <- NULL
    fit$call <- Call
    fit$family <- family
    fit$logLik <- as.numeric(NA)
    oldClass(fit) <- c("glmmPQL", oldClass(fit))
    fit
}


gamma.shape <- function (object, ...) 
UseMethod("gamma.shape")


denumerate <- function (x) 
UseMethod("denumerate")


fractions <- function (x, cycles = 10, max.denominator = 2000, ...) 
{
    ans <- .rat(x, cycles, max.denominator)
    ndc <- paste(ans$rat[, 1], ans$rat[, 2], sep = "/")
    int <- ans$rat[, 2] == 1
    ndc[int] <- as.character(ans$rat[int, 1])
    structure(ans$x, fracs = ndc, class = c("fractions", class(ans$x)))
}


cov.rob <- function (x, cor = FALSE, quantile.used = floor((n + p + 1)/2), 
    method = c("mve", "mcd", "classical"), nsamp = "best", seed) 
{
    method <- match.arg(method)
    x <- as.matrix(x)
    if (any(is.na(x)) || any(is.infinite(x))) 
        stop("missing or infinite values are not allowed")
    n <- nrow(x)
    p <- ncol(x)
    if (n < p + 1) 
        stop(gettextf("at least %d cases are needed", p + 1), 
            domain = NA)
    if (method == "classical") {
        ans <- list(center = colMeans(x), cov = var(x))
    }
    else {
        if (quantile.used < p + 1) 
            stop(gettextf("'quantile' must be at least %d", p + 
                1), domain = NA)
        if (quantile.used > n - 1) 
            stop(gettextf("'quantile' must be at most %d", n - 
                1), domain = NA)
        divisor <- apply(x, 2, IQR)
        if (any(divisor == 0)) 
            stop("at least one column has IQR 0")
        x <- x/rep(divisor, rep(n, p))
        qn <- quantile.used
        ps <- p + 1
        nexact <- choose(n, ps)
        if (is.character(nsamp) && nsamp == "best") 
            nsamp <- if (nexact < 5000) 
                "exact"
            else "sample"
        if (is.numeric(nsamp) && nsamp > nexact) {
            warning(sprintf(ngettext(nexact, "only %d set, so all sets will be tried", 
                "only %d sets, so all sets will be tried"), nexact), 
                domain = NA)
            nsamp <- "exact"
        }
        samp <- nsamp != "exact"
        if (samp) {
            if (nsamp == "sample") 
                nsamp <- min(500 * ps, 3000)
        }
        else nsamp <- nexact
        if (samp && !missing(seed)) {
            if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
                seed.keep <- get(".Random.seed", envir = .GlobalEnv, 
                  inherits = FALSE)
                on.exit(assign(".Random.seed", seed.keep, envir = .GlobalEnv))
            }
            assign(".Random.seed", seed, envir = .GlobalEnv)
        }
        z <- .C(mve_fitlots, as.double(x), as.integer(n), as.integer(p), 
            as.integer(qn), as.integer(method == "mcd"), as.integer(samp), 
            as.integer(ps), as.integer(nsamp), crit = double(1), 
            sing = integer(1L), bestone = integer(n))
        z$sing <- paste(z$sing, "singular samples of size", ps, 
            "out of", nsamp)
        crit <- z$crit + 2 * sum(log(divisor)) + if (method == 
            "mcd") 
            -p * log(qn - 1)
        else 0
        best <- seq(n)[z$bestone != 0]
        if (!length(best)) 
            stop("'x' is probably collinear")
        means <- colMeans(x[best, , drop = FALSE])
        rcov <- var(x[best, , drop = FALSE]) * (1 + 15/(n - p))^2
        dist <- mahalanobis(x, means, rcov)
        cut <- qchisq(0.975, p) * quantile(dist, qn/n)/qchisq(qn/n, 
            p)
        cov <- divisor * var(x[dist < cut, , drop = FALSE]) * 
            rep(divisor, rep(p, p))
        attr(cov, "names") <- NULL
        ans <- list(center = colMeans(x[dist < cut, , drop = FALSE]) * 
            divisor, cov = cov, msg = z$sing, crit = crit, best = best)
    }
    if (cor) {
        sd <- sqrt(diag(ans$cov))
        ans <- c(ans, list(cor = (ans$cov/sd)/rep(sd, rep(p, 
            p))))
    }
    ans$n.obs <- n
    ans
}


hubers <- function (y, k = 1.5, mu, s, initmu = median(y), tol = 1e-06) 
{
    mmu <- missing(mu)
    ms <- missing(s)
    y <- y[!is.na(y)]
    n <- length(y)
    if (mmu) {
        mu0 <- initmu
        n1 <- n - 1
    }
    else {
        mu0 <- mu
        mu1 <- mu
        n1 <- n
    }
    if (ms) {
        s0 <- mad(y)
        if (s0 == 0) 
            return(list(mu = mu0, s = 0))
    }
    else {
        s0 <- s
        s1 <- s
    }
    th <- 2 * pnorm(k) - 1
    beta <- th + k^2 * (1 - th) - 2 * k * dnorm(k)
    for (i in 1:30) {
        yy <- pmin(pmax(mu0 - k * s0, y), mu0 + k * s0)
        if (mmu) 
            mu1 <- sum(yy)/n
        if (ms) {
            ss <- sum((yy - mu1)^2)/n1
            s1 <- sqrt(ss/beta)
        }
        if ((abs(mu0 - mu1) < tol * s0) && abs(s0 - s1) < tol * 
            s0) 
            break
        mu0 <- mu1
        s0 <- s1
    }
    list(mu = mu0, s = s0)
}


boxcox <- function (object, ...) 
UseMethod("boxcox")


eqscplot <- function (x, y, ratio = 1, tol = 0.04, uin, ...) 
{
    dots <- list(...)
    nmdots <- names(dots)
    Call <- match.call()
    Call$ratio <- Call$tol <- Call$uin <- NULL
    if (is.matrix(x)) {
        y <- x[, 2]
        x <- x[, 1]
        if (!is.null(dn <- colnames(x))) {
            xlab0 <- dn[1L]
            ylab0 <- dn[2L]
        }
        else {
            xlab0 <- ""
            ylab0 <- ""
        }
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
        xlab0 <- "x"
        ylab0 <- "y"
    }
    else {
        xlab0 <- deparse(substitute(x))
        ylab0 <- deparse(substitute(y))
    }
    Call$x <- x
    Call$y <- y
    Call$xlab <- if ("xlab" %in% nmdots) 
        dots$xlab
    else xlab0
    Call$ylab <- if ("ylab" %in% nmdots) 
        dots$ylab
    else ylab0
    xlim <- if ("xlim" %in% nmdots) 
        dots$xlim
    else range(x[is.finite(x)])
    ylim <- if ("ylim" %in% nmdots) 
        dots$ylim
    else range(y[is.finite(y)])
    midx <- 0.5 * (xlim[2L] + xlim[1L])
    xlim <- midx + (1 + tol) * 0.5 * c(-1, 1) * (xlim[2L] - xlim[1L])
    midy <- 0.5 * (ylim[2L] + ylim[1L])
    ylim <- midy + (1 + tol) * 0.5 * c(-1, 1) * (ylim[2L] - ylim[1L])
    oldpin <- par("pin")
    xuin <- oxuin <- oldpin[1L]/abs(diff(xlim))
    yuin <- oyuin <- oldpin[2L]/abs(diff(ylim))
    if (missing(uin)) {
        if (yuin > xuin * ratio) 
            yuin <- xuin * ratio
        else xuin <- yuin/ratio
    }
    else {
        if (length(uin) == 1L) 
            uin <- uin * c(1, ratio)
        if (any(c(xuin, yuin) < uin)) 
            stop("'uin' is too large to fit plot in")
        xuin <- uin[1L]
        yuin <- uin[2L]
    }
    xlim <- midx + oxuin/xuin * c(-1, 1) * diff(xlim) * 0.5
    ylim <- midy + oyuin/yuin * c(-1, 1) * diff(ylim) * 0.5
    Call$xlim <- xlim
    Call$ylim <- ylim
    Call$xaxs <- Call$yaxs <- "i"
    Call[[1L]] <- as.name("plot")
    eval.parent(Call)
}


write.matrix <- function (x, file = "", sep = " ", blocksize) 
{
    x <- as.matrix(x)
    p <- ncol(x)
    cn <- colnames(x)
    if (!missing(blocksize) && blocksize > 0L) {
        cat(cn, file = file, sep = c(rep(sep, p - 1L), "\n"))
        nlines <- 0
        nr <- nrow(x)
        while (nlines < nr) {
            nb <- min(blocksize, nr - nlines)
            cat(format(t(x[nlines + (1L:nb), ])), file = file, 
                append = TRUE, sep = c(rep(sep, p - 1L), "\n"))
            nlines <- nlines + nb
        }
    }
    else cat(c(cn, format(t(x))), file = file, sep = c(rep(sep, 
        p - 1L), "\n"))
}


Null <- function (M) 
{
    tmp <- qr(M)
    set <- if (tmp$rank == 0L) 
        seq_len(ncol(M))
    else -seq_len(tmp$rank)
    qr.Q(tmp, complete = TRUE)[, set, drop = FALSE]
}


corresp <- function (x, ...) 
UseMethod("corresp")


glm.convert <- function (object) 
{
    object$call[[1L]] <- quote(stats::glm)
    if (is.null(object$link)) 
        object$link <- as.name("log")
    object$call$family <- call("negative.binomial", theta = object$theta, 
        link = object$link)
    object$call$init.theta <- object$call$link <- NULL
    class(object) <- c("glm", "lm")
    object
}


kde2d <- function (x, y, h, n = 25, lims = c(range(x), range(y))) 
{
    nx <- length(x)
    if (length(y) != nx) 
        stop("data vectors must be the same length")
    if (any(!is.finite(x)) || any(!is.finite(y))) 
        stop("missing or infinite values in the data are not allowed")
    if (any(!is.finite(lims))) 
        stop("only finite values are allowed in 'lims'")
    n <- rep(n, length.out = 2L)
    gx <- seq.int(lims[1L], lims[2L], length.out = n[1L])
    gy <- seq.int(lims[3L], lims[4L], length.out = n[2L])
    h <- if (missing(h)) 
        c(bandwidth.nrd(x), bandwidth.nrd(y))
    else rep(h, length.out = 2L)
    if (any(h <= 0)) 
        stop("bandwidths must be strictly positive")
    h <- h/4
    ax <- outer(gx, x, "-")/h[1L]
    ay <- outer(gy, y, "-")/h[2L]
    z <- tcrossprod(matrix(dnorm(ax), , nx), matrix(dnorm(ay), 
        , nx))/(nx * h[1L] * h[2L])
    list(x = gx, y = gy, z = z)
}


width.SJ <- function (x, nb = 1000, lower = 0.1 * hmax, upper = hmax, method = c("ste", 
    "dpi")) 
{
    fSD <- function(h, x, alph2, c1, n, d) (c1/SDh(x, alph2 * 
        h^(5/7), n, d))^(1/5) - h
    SDh <- function(x, h, n, d) .C(VR_phi4_bin, as.integer(n), 
        as.integer(length(x)), as.double(d), x, as.double(h), 
        u = double(1))$u
    TDh <- function(x, h, n, d) .C(VR_phi6_bin, as.integer(n), 
        as.integer(length(x)), as.double(d), x, as.double(h), 
        u = double(1))$u
    method <- match.arg(method)
    n <- length(x)
    if (!n) 
        stop("'x' has length zero")
    storage.mode(x) <- "double"
    Z <- .C(VR_den_bin, as.integer(n), as.integer(nb), d = double(1), 
        x, cnt = integer(nb))
    d <- Z$d
    cnt <- as.integer(Z$cnt)
    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5)
    scale <- min(sqrt(var(x)), IQR(x)/1.349)
    a <- 1.24 * scale * n^(-1/7)
    b <- 1.23 * scale * n^(-1/9)
    c1 <- 1/(2 * sqrt(pi) * n)
    TD <- -TDh(cnt, b, n, d)
    alph2 <- 1.357 * (SDh(cnt, a, n, d)/TD)^(1/7)
    if (method == "dpi") 
        res <- (c1/SDh(cnt, (2.394/(n * TD))^(1/7), n, d))^(1/5)
    else {
        if (fSD(lower, cnt, alph2, c1, n, d) * fSD(upper, cnt, 
            alph2, c1, n, d) > 0) 
            stop("no solution in the specified range of bandwidths")
        res <- uniroot(fSD, c(lower, upper), tol = 0.1 * lower, 
            x = cnt, alph2 = alph2, c1 = c1, n = n, d = d)$root
    }
    4 * res
}


isoMDS <- function (d, y = cmdscale(d, k), k = 2, maxit = 50, trace = TRUE, 
    tol = 0.001, p = 2) 
{
    if (any(!is.finite(d)) && missing(y)) 
        stop("an initial configuration must be supplied with NA/Infs in 'd'")
    if (!is.matrix(y)) 
        stop("'y' must be a matrix")
    if (is.null(n <- attr(d, "Size"))) {
        x <- as.matrix(d)
        if ((n <- nrow(x)) != ncol(x)) 
            stop("distances must be result of 'dist' or a square matrix")
        rn <- rownames(x)
    }
    else {
        x <- matrix(0, n, n)
        x[row(x) > col(x)] <- d
        x <- x + t(x)
        rn <- attr(d, "Labels")
    }
    n <- as.integer(n)
    if (is.na(n)) 
        stop("invalid size")
    ab <- x[row(x) < col(x)] <= 0
    if (any(ab, na.rm = TRUE)) {
        ab <- !is.na(ab) & ab
        aa <- cbind(as.vector(row(x)), as.vector(col(x)))[row(x) < 
            col(x), ]
        aa <- aa[ab, , drop = FALSE]
        stop(gettextf("zero or negative distance between objects %d and %d", 
            aa[1, 1], aa[1, 2]), domain = NA)
    }
    nas <- is.na(x)
    diag(nas) <- FALSE
    if (any(rowSums(!nas) < 2)) 
        stop("not enough non-missing data")
    if (any(dim(y) != c(n, k))) 
        stop("invalid initial configuration")
    if (any(!is.finite(y))) 
        stop("initial configuration must be complete")
    dis <- x[row(x) > col(x)]
    ord <- order(dis)
    nd <- sum(!is.na(ord))
    on.exit(.C(VR_mds_unload))
    .C(VR_mds_init_data, as.integer(nd), as.integer(k), n, as.integer(ord - 
        1), as.integer(order(ord) - 1), as.double(y), as.double(p))
    tmp <- .C(VR_mds_dovm, val = double(1), as.integer(maxit), 
        as.integer(trace), y = as.double(y), as.double(tol))
    points <- matrix(tmp$y, , k)
    dimnames(points) <- list(rn, NULL)
    list(points = points, stress = tmp$val)
}


lqs <- function (x, ...) 
UseMethod("lqs")


ldahist <- function (data, g, nbins = 25, h, x0 = -h/1000, breaks, xlim = range(breaks), 
    ymax = 0, width, type = c("histogram", "density", "both"), 
    sep = (type != "density"), col = 5L, xlab = deparse(substitute(data)), 
    bty = "n", ...) 
{
    xlab
    type <- match.arg(type)
    data <- data[!is.na(data)]
    g <- g[!is.na(data)]
    counts <- table(g)
    groups <- names(counts)[counts > 0L]
    if (missing(breaks)) {
        if (missing(h)) 
            h <- diff(pretty(data, nbins))[1L]
        first <- floor((min(data) - x0)/h)
        last <- ceiling((max(data) - x0)/h)
        breaks <- x0 + h * c(first:last)
    }
    if (type == "histogram" || type == "both") {
        if (any(diff(breaks) <= 0)) 
            stop("'breaks' must be strictly increasing")
        if (min(data) < min(breaks) || max(data) > max(breaks)) 
            stop("'breaks' do not cover the data")
        est <- vector("list", length(groups))
        names(est) <- groups
        for (grp in groups) {
            bin <- cut(data[g == grp], breaks, include.lowest = TRUE)
            est1 <- tabulate(bin, length(levels(bin)))
            est1 <- est1/(diff(breaks) * length(data[g == grp]))
            ymax <- max(ymax, est1)
            est[[grp]] <- est1
        }
    }
    if (type == "density" || type == "both") {
        xd <- vector("list", length(groups))
        for (grp in groups) {
            if (missing(width)) 
                width <- width.SJ(data[g == grp])
            xd1 <- density(data[g == grp], n = 200L, width = width, 
                from = xlim[1L], to = xlim[2L])
            ymax <- max(ymax, xd1$y)
            xd[[grp]] <- xd1
        }
    }
    dev.hold()
    on.exit(dev.flush())
    if (!sep) 
        plot(xlim, c(0, ymax), type = "n", xlab = xlab, ylab = "", 
            bty = bty)
    else {
        oldpar <- par(mfrow = c(length(groups), 1L))
        on.exit(par(oldpar), add = TRUE)
    }
    for (grp in groups) {
        if (sep) 
            plot(xlim, c(0, ymax), type = "n", xlab = paste("group", 
                grp), ylab = "", bty = bty)
        if (type == "histogram" || type == "both") {
            n <- length(breaks)
            rect(breaks[-n], 0, breaks[-1L], est[[grp]], col = col, 
                ...)
        }
        if (type == "density" || type == "both") 
            lines(xd[[grp]])
    }
    invisible()
}


frequency.polygon <- function (x, nclass = nclass.freq(x), xlab = "", ylab = "", ...) 
{
    hst <- hist(x, nclass, probability = TRUE, plot = FALSE, 
        ...)
    midpoints <- 0.5 * (hst$breaks[-length(hst$breaks)] + hst$breaks[-1L])
    plot(midpoints, hst$counts, type = "l", xlab = xlab, ylab = ylab)
}


stdres <- function (object) 
lmwork(object)$stdres


select <- function (obj) 
UseMethod("select")


renumerate <- function (x) 
UseMethod("renumerate")


glm.nb <- function (formula, data, weights, subset, na.action, start = NULL, 
    etastart, mustart, control = glm.control(...), method = "glm.fit", 
    model = TRUE, x = FALSE, y = TRUE, contrasts = NULL, ..., 
    init.theta, link = log) 
{
    loglik <- function(n, th, mu, y, w) sum(w * (lgamma(th + 
        y) - lgamma(th) - lgamma(y + 1) + th * log(th) + y * 
        log(mu + (y == 0)) - (th + y) * log(th + mu)))
    link <- substitute(link)
    fam0 <- if (missing(init.theta)) 
        do.call("poisson", list(link = link))
    else do.call("negative.binomial", list(theta = init.theta, 
        link = link))
    mf <- Call <- match.call()
    m <- match(c("formula", "data", "subset", "weights", "na.action", 
        "etastart", "mustart", "offset"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- quote(stats::model.frame)
    mf <- eval.parent(mf)
    Terms <- attr(mf, "terms")
    if (method == "model.frame") 
        return(mf)
    Y <- model.response(mf, "numeric")
    X <- if (!is.empty.model(Terms)) 
        model.matrix(Terms, mf, contrasts)
    else matrix(, NROW(Y), 0)
    w <- model.weights(mf)
    if (!length(w)) 
        w <- rep(1, nrow(mf))
    else if (any(w < 0)) 
        stop("negative weights not allowed")
    offset <- model.offset(mf)
    mustart <- model.extract(mf, "mustart")
    etastart <- model.extract(mf, "etastart")
    n <- length(Y)
    if (!missing(method)) {
        if (!exists(method, mode = "function")) 
            stop(gettextf("unimplemented method: %s", sQuote(method)), 
                domain = NA)
        glm.fitter <- get(method)
    }
    else {
        method <- "glm.fit"
        glm.fitter <- stats::glm.fit
    }
    if (control$trace > 1) 
        message("Initial fit:")
    fit <- glm.fitter(x = X, y = Y, w = w, start = start, etastart = etastart, 
        mustart = mustart, offset = offset, family = fam0, control = list(maxit = control$maxit, 
            epsilon = control$epsilon, trace = control$trace > 
                1), intercept = attr(Terms, "intercept") > 0)
    class(fit) <- c("glm", "lm")
    mu <- fit$fitted.values
    th <- as.vector(theta.ml(Y, mu, sum(w), w, limit = control$maxit, 
        trace = control$trace > 2))
    if (control$trace > 1) 
        message(gettextf("Initial value for 'theta': %f", signif(th)), 
            domain = NA)
    fam <- do.call("negative.binomial", list(theta = th, link = link))
    iter <- 0
    d1 <- sqrt(2 * max(1, fit$df.residual))
    d2 <- del <- 1
    g <- fam$linkfun
    Lm <- loglik(n, th, mu, Y, w)
    Lm0 <- Lm + 2 * d1
    while ((iter <- iter + 1) <= control$maxit && (abs(Lm0 - 
        Lm)/d1 + abs(del)/d2) > control$epsilon) {
        eta <- g(mu)
        fit <- glm.fitter(x = X, y = Y, w = w, etastart = eta, 
            offset = offset, family = fam, control = list(maxit = control$maxit, 
                epsilon = control$epsilon, trace = control$trace > 
                  1), intercept = attr(Terms, "intercept") > 
                0)
        t0 <- th
        th <- theta.ml(Y, mu, sum(w), w, limit = control$maxit, 
            trace = control$trace > 2)
        fam <- do.call("negative.binomial", list(theta = th, 
            link = link))
        mu <- fit$fitted.values
        del <- t0 - th
        Lm0 <- Lm
        Lm <- loglik(n, th, mu, Y, w)
        if (control$trace) {
            Ls <- loglik(n, th, Y, Y, w)
            Dev <- 2 * (Ls - Lm)
            message(sprintf("Theta(%d) = %f, 2(Ls - Lm) = %f", 
                iter, signif(th), signif(Dev)), domain = NA)
        }
    }
    if (!is.null(attr(th, "warn"))) 
        fit$th.warn <- attr(th, "warn")
    if (iter > control$maxit) {
        warning("alternation limit reached")
        fit$th.warn <- gettext("alternation limit reached")
    }
    if (length(offset) && attr(Terms, "intercept")) {
        null.deviance <- if (length(Terms)) 
            glm.fitter(X[, "(Intercept)", drop = FALSE], Y, w, 
                offset = offset, family = fam, control = list(maxit = control$maxit, 
                  epsilon = control$epsilon, trace = control$trace > 
                    1), intercept = TRUE)$deviance
        else fit$deviance
        fit$null.deviance <- null.deviance
    }
    class(fit) <- c("negbin", "glm", "lm")
    fit$terms <- Terms
    fit$formula <- as.vector(attr(Terms, "formula"))
    Call$init.theta <- signif(as.vector(th), 10)
    Call$link <- link
    fit$call <- Call
    if (model) 
        fit$model <- mf
    fit$na.action <- attr(mf, "na.action")
    if (x) 
        fit$x <- X
    if (!y) 
        fit$y <- NULL
    fit$theta <- as.vector(th)
    fit$SE.theta <- attr(th, "SE")
    fit$twologlik <- as.vector(2 * Lm)
    fit$aic <- -fit$twologlik + 2 * fit$rank + 2
    fit$contrasts <- attr(X, "contrasts")
    fit$xlevels <- .getXlevels(Terms, mf)
    fit$method <- method
    fit$control <- control
    fit$offset <- offset
    fit
}


sammon <- function (d, y = cmdscale(d, k), k = 2, niter = 100, trace = TRUE, 
    magic = 0.2, tol = 1e-04) 
{
    call <- match.call()
    if (any(is.infinite(d))) 
        stop("Infs not allowed in 'd'")
    if (any(is.na(d)) && missing(y)) 
        stop("an initial configuration must be supplied if there are NAs in 'd'")
    if (!is.matrix(y)) 
        stop("'y' must be a matrix")
    if (is.null(n <- attr(d, "Size"))) {
        x <- as.matrix(d)
        if ((n <- nrow(x)) != ncol(x)) 
            stop("distances must be result of 'dist' or a square matrix")
        rn <- rownames(x)
    }
    else {
        x <- matrix(0, n, n)
        x[row(x) > col(x)] <- d
        x <- x + t(x)
        rn <- attr(d, "Labels")
    }
    n <- as.integer(n)
    if (is.na(n)) 
        stop("invalid size")
    ab <- x[row(x) < col(x)] <= 0
    if (any(ab, na.rm = TRUE)) {
        ab <- !is.na(ab) & ab
        aa <- cbind(as.vector(row(x)), as.vector(col(x)))[row(x) < 
            col(x), ]
        aa <- aa[ab, , drop = FALSE]
        stop(gettextf("zero or negative distance between objects %d and %d", 
            aa[1, 1], aa[1, 2]), domain = NA)
    }
    nas <- is.na(x)
    diag(nas) <- FALSE
    if (any(rowSums(!nas) < 2)) 
        stop("not enough non-missing data")
    if (any(dim(y) != c(n, k))) 
        stop("invalid initial configuration")
    if (any(!is.finite(y))) 
        stop("initial configuration must be complete")
    storage.mode(x) <- "double"
    storage.mode(y) <- "double"
    z <- .C(VR_sammon, x = x, n, as.integer(k), y = y, as.integer(niter), 
        e = double(1), as.integer(trace), as.double(magic), as.double(tol), 
        NAOK = TRUE)
    points <- z$y
    dimnames(points) <- list(rn, NULL)
    list(points = points, stress = z$e, call = call)
}


bcv <- function (x, nb = 1000, lower = 0.1 * hmax, upper = hmax) 
{
    fbcv <- function(h, x, n, d) .C(VR_bcv_bin, as.integer(n), 
        as.integer(length(x)), as.double(d), x, as.double(h), 
        u = double(1))$u
    n <- length(x)
    if (!n) 
        stop("'x' has length zero")
    hmax <- 1.144 * sqrt(var(x)) * n^(-1/5) * 4
    storage.mode(x) <- "double"
    Z <- .C(VR_den_bin, as.integer(n), as.integer(nb), d = double(1), 
        x, cnt = integer(nb))
    d <- Z$d
    cnt <- as.integer(Z$cnt)
    h <- optimize(fbcv, c(lower, upper), tol = 0.1 * lower, x = cnt, 
        n = n, d = d)$minimum
    if (h < 1.1 * lower | h > upper - 0.1 * lower) 
        warning("minimum occurred at one end of the range")
    h
}


studres <- function (object) 
lmwork(object)$studres




## Package Data

Aids2 <- MASS::Aids2		## Australian AIDS Survival Data

Animals <- MASS::Animals		## Brain and Body Weights for 28 Species

Boston <- MASS::Boston		## Housing Values in Suburbs of Boston

Cars93 <- MASS::Cars93		## Data from 93 Cars on Sale in the USA in 1993

Cushings <- MASS::Cushings		## Diagnostic Tests on Patients with Cushing's Syndrome

DDT <- MASS::DDT		## DDT in Kale

GAGurine <- MASS::GAGurine		## Level of GAG in Urine of Children

Insurance <- MASS::Insurance		## Numbers of Car Insurance claims

Melanoma <- MASS::Melanoma		## Survival from Malignant Melanoma

OME <- MASS::OME		## Tests of Auditory Perception in Children with OME

Pima.te <- MASS::Pima.te		## Diabetes in Pima Indian Women

Pima.tr <- MASS::Pima.tr		## Diabetes in Pima Indian Women

Pima.tr2 <- MASS::Pima.tr2		## Diabetes in Pima Indian Women

Rabbit <- MASS::Rabbit		## Blood Pressure in Rabbits

Rubber <- MASS::Rubber		## Accelerated Testing of Tyre Rubber

SP500 <- MASS::SP500		## Returns of the Standard and Poors 500

Sitka <- MASS::Sitka		## Growth Curves for Sitka Spruce Trees in 1988

Sitka89 <- MASS::Sitka89		## Growth Curves for Sitka Spruce Trees in 1989

Skye <- MASS::Skye		## AFM Compositions of Aphyric Skye Lavas

Traffic <- MASS::Traffic		## Effect of Swedish Speed Limits on Accidents

UScereal <- MASS::UScereal		## Nutritional and Marketing Information on US Cereals

UScrime <- MASS::UScrime		## The Effect of Punishment Regimes on Crime Rates

VA <- MASS::VA		## Veteran's Administration Lung Cancer Trial

abbey <- MASS::abbey		## Determinations of Nickel Content

accdeaths <- MASS::accdeaths		## Accidental Deaths in the US 1973-1978

anorexia <- MASS::anorexia		## Anorexia Data on Weight Change

bacteria <- MASS::bacteria		## Presence of Bacteria after Drug Treatments

beav1 <- MASS::beav1		## Body Temperature Series of Beaver 1

beav2 <- MASS::beav2		## Body Temperature Series of Beaver 2

biopsy <- MASS::biopsy		## Biopsy Data on Breast Cancer Patients

birthwt <- MASS::birthwt		## Risk Factors Associated with Low Infant Birth Weight

cabbages <- MASS::cabbages		## Data from a cabbage field trial

caith <- MASS::caith		## Colours of Eyes and Hair of People in Caithness

cats <- MASS::cats		## Anatomical Data from Domestic Cats

cement <- MASS::cement		## Heat Evolved by Setting Cements

chem <- MASS::chem		## Copper in Wholemeal Flour

coop <- MASS::coop		## Co-operative Trial in Analytical Chemistry

cpus <- MASS::cpus		## Performance of Computer CPUs

crabs <- MASS::crabs		## Morphological Measurements on Leptograpsus Crabs

deaths <- MASS::deaths		## Monthly Deaths from Lung Diseases in the UK

drivers <- MASS::drivers		## Deaths of Car Drivers in Great Britain 1969-84

eagles <- MASS::eagles		## Foraging Ecology of Bald Eagles

epil <- MASS::epil		## Seizure Counts for Epileptics

farms <- MASS::farms		## Ecological Factors in Farm Management

fgl <- MASS::fgl		## Measurements of Forensic Glass Fragments

forbes <- MASS::forbes		## Forbes' Data on Boiling Points in the Alps

galaxies <- MASS::galaxies		## Velocities for 82 Galaxies

gehan <- MASS::gehan		## Remission Times of Leukaemia Patients

genotype <- MASS::genotype		## Rat Genotype Data

geyser <- MASS::geyser		## Old Faithful Geyser Data

gilgais <- MASS::gilgais		## Line Transect of Soil in Gilgai Territory

hills <- MASS::hills		## Record Times in Scottish Hill Races

housing <- MASS::housing		## Frequency Table from a Copenhagen Housing Conditions Survey

immer <- MASS::immer		## Yields from a Barley Field Trial

leuk <- MASS::leuk		## Survival Times and White Blood Counts for Leukaemia Patients

mammals <- MASS::mammals		## Brain and Body Weights for 62 Species of Land Mammals

mcycle <- MASS::mcycle		## Data from a Simulated Motorcycle Accident

menarche <- MASS::menarche		## Age of Menarche in Warsaw

michelson <- MASS::michelson		## Michelson's Speed of Light Data

minn38 <- MASS::minn38		## Minnesota High School Graduates of 1938

motors <- MASS::motors		## Accelerated Life Testing of Motorettes

muscle <- MASS::muscle		## Effect of Calcium Chloride on Muscle Contraction in Rat Hearts

newcomb <- MASS::newcomb		## Newcomb's Measurements of the Passage Time of Light

nlschools <- MASS::nlschools		## Eighth-Grade Pupils in the Netherlands

npk <- MASS::npk		## Classical N, P, K Factorial Experiment

npr1 <- MASS::npr1		## US Naval Petroleum Reserve No. 1 data

oats <- MASS::oats		## Data from an Oats Field Trial

painters <- MASS::painters		## The Painter's Data of de Piles

petrol <- MASS::petrol		## N. L. Prater's Petrol Refinery Data

phones <- MASS::phones		## Belgium Phone Calls 1950-1973

quine <- MASS::quine		## Absenteeism from School in Rural New South Wales

road <- MASS::road		## Road Accident Deaths in US States

rotifer <- MASS::rotifer		## Numbers of Rotifers by Fluid Density

ships <- MASS::ships		## Ships Damage Data

shoes <- MASS::shoes		## Shoe wear data of Box, Hunter and Hunter

shrimp <- MASS::shrimp		## Percentage of Shrimp in Shrimp Cocktail

shuttle <- MASS::shuttle		## Space Shuttle Autolander Problem

snails <- MASS::snails		## Snail Mortality Data

steam <- MASS::steam		## The Saturated Steam Pressure Data

stormer <- MASS::stormer		## The Stormer Viscometer Data

survey <- MASS::survey		## Student Survey Data

synth.te <- MASS::synth.te		## Synthetic Classification Problem

synth.tr <- MASS::synth.tr		## Synthetic Classification Problem

topo <- MASS::topo		## Spatial Topographic Data

waders <- MASS::waders		## Counts of Waders at 15 Sites in South Africa

whiteside <- MASS::whiteside		## House Insulation: Whiteside's Data

wtloss <- MASS::wtloss		## Weight Loss Data from an Obese Patient



## Package Info

.skeleton_package_title = "Support Functions and Datasets for Venables and Ripley's MASS"

.skeleton_package_version = "7.3-45"

.skeleton_package_depends = "grDevices,graphics,stats,utils"

.skeleton_package_imports = "methods"


## Internal

.skeleton_version = 5


## EOF