##
## Exported symobls in package `nnet`
##

## Exported package methods

nnetHess <- function (net, x, y, weights) 
{
    x <- as.matrix(x)
    y <- as.matrix(y)
    if (dim(x)[1L] != dim(y)[1L]) 
        stop("dims of 'x' and 'y' must match")
    nw <- length(net$wts)
    decay <- net$decay
    if (length(decay) == 1) 
        decay <- rep(decay, nw)
    .C(VR_set_net, as.integer(net$n), as.integer(net$nconn), 
        as.integer(net$conn), as.double(decay), as.integer(net$nsunits), 
        as.integer(net$entropy), as.integer(net$softmax), as.integer(net$censored))
    ntr <- dim(x)[1L]
    if (missing(weights)) 
        weights <- rep(1, ntr)
    if (length(weights) != ntr || any(weights < 0)) 
        stop("invalid weights vector")
    Z <- as.double(cbind(x, y))
    storage.mode(weights) <- "double"
    z <- matrix(.C(VR_nnHessian, as.integer(ntr), Z, weights, 
        as.double(net$wts), H = double(nw * nw))$H, nw, nw)
    .C(VR_unset_net)
    z
}


nnet <- function (x, ...) 
UseMethod("nnet")


multinom <- function (formula, data, weights, subset, na.action, contrasts = NULL, 
    Hess = FALSE, summ = 0, censored = FALSE, model = FALSE, 
    ...) 
{
    class.ind <- function(cl) {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.integer(cl) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    summ2 <- function(X, Y) {
        X <- as.matrix(X)
        Y <- as.matrix(Y)
        n <- nrow(X)
        p <- ncol(X)
        q <- ncol(Y)
        Z <- t(cbind(X, Y))
        storage.mode(Z) <- "double"
        z <- .C(VR_summ2, as.integer(n), as.integer(p), as.integer(q), 
            Z = Z, na = integer(1L))
        Za <- t(z$Z[, 1L:z$na, drop = FALSE])
        list(X = Za[, 1L:p, drop = FALSE], Y = Za[, p + 1L:q])
    }
    call <- match.call()
    m <- match.call(expand.dots = FALSE)
    m$summ <- m$Hess <- m$contrasts <- m$censored <- m$model <- m$... <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    X <- model.matrix(Terms, m, contrasts)
    cons <- attr(X, "contrasts")
    Xr <- qr(X)$rank
    Y <- model.response(m)
    if (!is.matrix(Y)) 
        Y <- as.factor(Y)
    w <- model.weights(m)
    if (length(w) == 0L) 
        if (is.matrix(Y)) 
            w <- rep(1, dim(Y)[1L])
        else w <- rep(1, length(Y))
    lev <- levels(Y)
    if (is.factor(Y)) {
        counts <- table(Y)
        if (any(counts == 0L)) {
            empty <- lev[counts == 0L]
            warning(sprintf(ngettext(length(empty), "group %s is empty", 
                "groups %s are empty"), paste(sQuote(empty), 
                collapse = " ")), domain = NA)
            Y <- factor(Y, levels = lev[counts > 0L])
            lev <- lev[counts > 0L]
        }
        if (length(lev) < 2L) 
            stop("need two or more classes to fit a multinom model")
        if (length(lev) == 2L) 
            Y <- as.integer(Y) - 1
        else Y <- class.ind(Y)
    }
    if (summ == 1) {
        Z <- cbind(X, Y)
        z1 <- cumprod(apply(Z, 2L, max) + 1)
        Z1 <- apply(Z, 1L, function(x) sum(z1 * x))
        oZ <- order(Z1)
        Z2 <- !duplicated(Z1[oZ])
        oX <- (seq_along(Z1)[oZ])[Z2]
        X <- X[oX, , drop = FALSE]
        Y <- if (is.matrix(Y)) 
            Y[oX, , drop = FALSE]
        else Y[oX]
        w <- diff(c(0, cumsum(w))[c(Z2, TRUE)])
        print(dim(X))
    }
    if (summ == 2) {
        Z <- summ2(cbind(X, Y), w)
        X <- Z$X[, 1L:ncol(X)]
        Y <- Z$X[, ncol(X) + 1L:ncol(Y), drop = FALSE]
        w <- Z$Y
        print(dim(X))
    }
    if (summ == 3) {
        Z <- summ2(X, Y * w)
        X <- Z$X
        Y <- Z$Y[, 1L:ncol(Y), drop = FALSE]
        w <- rep(1, nrow(X))
        print(dim(X))
    }
    offset <- model.offset(m)
    r <- ncol(X)
    if (is.matrix(Y)) {
        p <- ncol(Y)
        sY <- Y %*% rep(1, p)
        if (any(sY == 0)) 
            stop("some case has no observations")
        if (!censored) {
            Y <- Y/matrix(sY, nrow(Y), p)
            w <- w * sY
        }
        if (length(offset) > 1L) {
            if (ncol(offset) != p) 
                stop("ncol(offset) is wrong")
            mask <- c(rep(FALSE, r + 1L + p), rep(c(FALSE, rep(TRUE, 
                r), rep(FALSE, p)), p - 1L))
            X <- cbind(X, offset)
            Wts <- as.vector(rbind(matrix(0, r + 1L, p), diag(p)))
            fit <- nnet.default(X, Y, w, Wts = Wts, mask = mask, 
                size = 0, skip = TRUE, softmax = TRUE, censored = censored, 
                rang = 0, ...)
        }
        else {
            mask <- c(rep(FALSE, r + 1L), rep(c(FALSE, rep(TRUE, 
                r)), p - 1L))
            fit <- nnet.default(X, Y, w, mask = mask, size = 0, 
                skip = TRUE, softmax = TRUE, censored = censored, 
                rang = 0, ...)
        }
    }
    else {
        if (length(offset) <= 1L) {
            mask <- c(FALSE, rep(TRUE, r))
            fit <- nnet.default(X, Y, w, mask = mask, size = 0, 
                skip = TRUE, entropy = TRUE, rang = 0, ...)
        }
        else {
            mask <- c(FALSE, rep(TRUE, r), FALSE)
            Wts <- c(rep(0, r + 1L), 1)
            X <- cbind(X, offset)
            fit <- nnet.default(X, Y, w, Wts = Wts, mask = mask, 
                size = 0, skip = TRUE, entropy = TRUE, rang = 0, 
                ...)
        }
    }
    fit$formula <- attr(Terms, "formula")
    fit$terms <- Terms
    fit$call <- call
    fit$weights <- w
    fit$lev <- lev
    fit$deviance <- 2 * fit$value
    fit$rank <- Xr
    edf <- ifelse(length(lev) == 2L, 1, length(lev) - 1) * Xr
    if (is.matrix(Y)) {
        edf <- (ncol(Y) - 1) * Xr
        if (length(dn <- colnames(Y)) > 0) 
            fit$lab <- dn
        else fit$lab <- 1L:ncol(Y)
    }
    fit$coefnames <- colnames(X)
    fit$vcoefnames <- fit$coefnames[1L:r]
    fit$na.action <- attr(m, "na.action")
    fit$contrasts <- cons
    fit$xlevels <- .getXlevels(Terms, m)
    fit$edf <- edf
    fit$AIC <- fit$deviance + 2 * edf
    if (model) 
        fit$model <- m
    class(fit) <- c("multinom", "nnet")
    if (Hess) 
        fit$Hessian <- multinomHess(fit, X)
    fit
}


which.is.max <- function (x) 
{
    y <- seq_along(x)[x == max(x)]
    if (length(y) > 1L) 
        sample(y, 1L)
    else y
}


class.ind <- function (cl) 
{
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1L:n) + n * (unclass(cl) - 1L)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
}


nnet.default <- function (x, y, weights, size, Wts, mask = rep(TRUE, length(wts)), 
    linout = FALSE, entropy = FALSE, softmax = FALSE, censored = FALSE, 
    skip = FALSE, rang = 0.7, decay = 0, maxit = 100, Hess = FALSE, 
    trace = TRUE, MaxNWts = 1000, abstol = 1e-04, reltol = 1e-08, 
    ...) 
{
    net <- NULL
    x <- as.matrix(x)
    y <- as.matrix(y)
    if (any(is.na(x))) 
        stop("missing values in 'x'")
    if (any(is.na(y))) 
        stop("missing values in 'y'")
    if (dim(x)[1L] != dim(y)[1L]) 
        stop("nrows of 'x' and 'y' must match")
    if (linout && entropy) 
        stop("entropy fit only for logistic units")
    if (softmax) {
        linout <- TRUE
        entropy <- FALSE
    }
    if (censored) {
        linout <- TRUE
        entropy <- FALSE
        softmax <- TRUE
    }
    net$n <- c(dim(x)[2L], size, dim(y)[2L])
    net$nunits <- as.integer(1L + sum(net$n))
    net$nconn <- rep(0, net$nunits + 1L)
    net$conn <- numeric(0L)
    net <- norm.net(net)
    if (skip) 
        net <- add.net(net, seq(1L, net$n[1L]), seq(1L + net$n[1L] + 
            net$n[2L], net$nunits - 1L))
    if ((nwts <- length(net$conn)) == 0) 
        stop("no weights to fit")
    if (nwts > MaxNWts) 
        stop(gettextf("too many (%d) weights", nwts), domain = NA)
    nsunits <- net$nunits
    if (linout) 
        nsunits <- net$nunits - net$n[3L]
    net$nsunits <- nsunits
    net$decay <- decay
    net$entropy <- entropy
    if (softmax && NCOL(y) < 2L) 
        stop("'softmax = TRUE' requires at least two response categories")
    net$softmax <- softmax
    net$censored <- censored
    if (missing(Wts)) 
        if (rang > 0) 
            wts <- runif(nwts, -rang, rang)
        else wts <- rep(0, nwts)
    else wts <- Wts
    if (length(wts) != nwts) 
        stop("weights vector of incorrect length")
    if (length(mask) != length(wts)) 
        stop("incorrect length of 'mask'")
    if (trace) {
        cat("# weights: ", length(wts))
        nw <- sum(mask != 0)
        if (nw < length(wts)) 
            cat(" (", nw, " variable)\n", sep = "")
        else cat("\n")
        flush.console()
    }
    if (length(decay) == 1L) 
        decay <- rep(decay, length(wts))
    .C(VR_set_net, as.integer(net$n), as.integer(net$nconn), 
        as.integer(net$conn), as.double(decay), as.integer(nsunits), 
        as.integer(entropy), as.integer(softmax), as.integer(censored))
    ntr <- dim(x)[1L]
    nout <- dim(y)[2L]
    if (missing(weights)) 
        weights <- rep(1, ntr)
    if (length(weights) != ntr || any(weights < 0)) 
        stop("invalid weights vector")
    Z <- as.double(cbind(x, y))
    storage.mode(weights) <- "double"
    tmp <- .C(VR_dovm, as.integer(ntr), Z, weights, as.integer(length(wts)), 
        wts = as.double(wts), val = double(1), as.integer(maxit), 
        as.logical(trace), as.integer(mask), as.double(abstol), 
        as.double(reltol), ifail = integer(1L))
    net$value <- tmp$val
    net$wts <- tmp$wts
    net$convergence <- tmp$ifail
    tmp <- matrix(.C(VR_nntest, as.integer(ntr), Z, tclass = double(ntr * 
        nout), as.double(net$wts))$tclass, ntr, nout)
    dimnames(tmp) <- list(rownames(x), colnames(y))
    net$fitted.values <- tmp
    tmp <- y - tmp
    dimnames(tmp) <- list(rownames(x), colnames(y))
    net$residuals <- tmp
    .C(VR_unset_net)
    if (entropy) 
        net$lev <- c("0", "1")
    if (softmax) 
        net$lev <- colnames(y)
    net$call <- match.call()
    if (Hess) 
        net$Hessian <- nnetHess(net, x, y, weights)
    class(net) <- "nnet"
    net
}


nnet.formula <- function (formula, data, weights, ..., subset, na.action, contrasts = NULL) 
{
    class.ind <- function(cl) {
        n <- length(cl)
        x <- matrix(0, n, length(levels(cl)))
        x[(1L:n) + n * (as.vector(unclass(cl)) - 1L)] <- 1
        dimnames(x) <- list(names(cl), levels(cl))
        x
    }
    m <- match.call(expand.dots = FALSE)
    if (is.matrix(eval.parent(m$data))) 
        m$data <- as.data.frame(data)
    m$... <- m$contrasts <- NULL
    m[[1L]] <- quote(stats::model.frame)
    m <- eval.parent(m)
    Terms <- attr(m, "terms")
    x <- model.matrix(Terms, m, contrasts)
    cons <- attr(x, "contrast")
    xint <- match("(Intercept)", colnames(x), nomatch = 0L)
    if (xint > 0L) 
        x <- x[, -xint, drop = FALSE]
    w <- model.weights(m)
    if (length(w) == 0L) 
        w <- rep(1, nrow(x))
    y <- model.response(m)
    if (is.factor(y)) {
        lev <- levels(y)
        counts <- table(y)
        if (any(counts == 0L)) {
            empty <- lev[counts == 0L]
            warning(sprintf(ngettext(length(empty), "group %s is empty", 
                "groups %s are empty"), paste(sQuote(empty), 
                collapse = " ")), domain = NA)
            y <- factor(y, levels = lev[counts > 0L])
        }
        if (length(lev) == 2L) {
            y <- as.vector(unclass(y)) - 1
            res <- nnet.default(x, y, w, entropy = TRUE, ...)
            res$lev <- lev
        }
        else {
            y <- class.ind(y)
            res <- nnet.default(x, y, w, softmax = TRUE, ...)
            res$lev <- lev
        }
    }
    else res <- nnet.default(x, y, w, ...)
    res$terms <- Terms
    res$coefnames <- colnames(x)
    res$call <- match.call()
    res$na.action <- attr(m, "na.action")
    res$contrasts <- cons
    res$xlevels <- .getXlevels(Terms, m)
    class(res) <- c("nnet.formula", "nnet")
    res
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Feed-Forward Neural Networks and Multinomial Log-Linear Models"

.skeleton_package_version = "7.3-12"

.skeleton_package_depends = "stats,utils"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF