##
## Exported symobls in package `boot`
##

## Exported package methods

lik.CI <- function (like, lim) 
{
    L <- like[, 2]
    theta <- like[, 1]
    n <- length(L)
    i <- min(c(1L:n)[L > lim])
    if (is.na(i)) 
        stop(gettextf("likelihood never exceeds %f", lim), domain = NA)
    j <- max(c(1L:n)[L > lim])
    if (i == j) 
        stop(gettextf("likelihood exceeds %f at only one point", 
            lim), domain = NA)
    if (i == 1) 
        bot <- -Inf
    else {
        i <- i + c(-1, 0, 1)
        x <- theta[i]
        y <- L[i] - lim
        co <- coefficients(lm(y ~ x + x^2))
        bot <- (-co[2L] + sqrt(co[2L]^2 - 4 * co[1L] * co[3L]))/(2 * 
            co[3L])
    }
    if (j == n) 
        top <- Inf
    else {
        j <- j + c(-1, 0, 1)
        x <- theta[j]
        y <- L[j] - lim
        co <- coefficients(lm(y ~ x + x^2))
        top <- (-co[2L] - sqrt(co[2L]^2 - 4 * co[1L] * co[3L]))/(2 * 
            co[3L])
    }
    out <- c(bot, top)
    names(out) <- NULL
    out
}


boot.ci <- function (boot.out, conf = 0.95, type = "all", index = 1L:min(2L, 
    length(boot.out$t0)), var.t0 = NULL, var.t = NULL, t0 = NULL, 
    t = NULL, L = NULL, h = function(t) t, hdot = function(t) rep(1, 
        length(t)), hinv = function(t) t, ...) 
{
    call <- match.call()
    if ((is.null(t) && !is.null(t0)) || (!is.null(t) && is.null(t0))) 
        stop("'t' and 't0' must be supplied together")
    t.o <- t
    t0.o <- t0
    vt0.o <- var.t0
    if (is.null(t)) {
        if (length(index) == 1L) {
            t0 <- boot.out$t0[index]
            t <- boot.out$t[, index]
        }
        else if (ncol(boot.out$t) < max(index)) {
            warning("index out of bounds; minimum index only used.")
            index <- min(index)
            t0 <- boot.out$t0[index]
            t <- boot.out$t[, index]
        }
        else {
            t0 <- boot.out$t0[index[1L]]
            t <- boot.out$t[, index[1L]]
            if (is.null(var.t0)) 
                var.t0 <- boot.out$t0[index[2L]]
            if (is.null(var.t)) 
                var.t <- boot.out$t[, index[2L]]
        }
    }
    if (const(t, min(1e-08, mean(t, na.rm = TRUE)/1e+06))) {
        print(paste("All values of t are equal to ", mean(t, 
            na.rm = TRUE), "\n Cannot calculate confidence intervals"))
        return(NULL)
    }
    if (length(t) != boot.out$R) 
        stop(gettextf("'t' must of length %d", boot.out$R), domain = NA)
    if (is.null(var.t)) 
        fins <- seq_along(t)[is.finite(t)]
    else {
        fins <- seq_along(t)[is.finite(t) & is.finite(var.t)]
        var.t <- var.t[fins]
    }
    t <- t[fins]
    R <- length(t)
    if (!is.null(var.t0)) 
        var.t0 <- var.t0 * hdot(t0)^2
    if (!is.null(var.t)) 
        var.t <- var.t * hdot(t)^2
    t0 <- h(t0)
    t <- h(t)
    if (missing(L)) 
        L <- boot.out$L
    output <- list(R = R, t0 = hinv(t0), call = call)
    if (any(type == "all" | type == "norm")) 
        output <- c(output, list(normal = norm.ci(boot.out, conf, 
            index[1L], var.t0 = vt0.o, t0 = t0.o, t = t.o, L = L, 
            h = h, hdot = hdot, hinv = hinv)))
    if (any(type == "all" | type == "basic")) 
        output <- c(output, list(basic = basic.ci(t0, t, conf, 
            hinv = hinv)))
    if (any(type == "all" | type == "stud")) {
        if (length(index) == 1L) 
            warning("bootstrap variances needed for studentized intervals")
        else output <- c(output, list(student = stud.ci(c(t0, 
            var.t0), cbind(t, var.t), conf, hinv = hinv)))
    }
    if (any(type == "all" | type == "perc")) 
        output <- c(output, list(percent = perc.ci(t, conf, hinv = hinv)))
    if (any(type == "all" | type == "bca")) {
        if (as.character(boot.out$call[1L]) == "tsboot") 
            warning("BCa intervals not defined for time series bootstraps")
        else output <- c(output, list(bca = bca.ci(boot.out, 
            conf, index[1L], L = L, t = t.o, t0 = t0.o, h = h, 
            hdot = hdot, hinv = hinv, ...)))
    }
    class(output) <- "bootci"
    output
}


var.linear <- function (L, strata = NULL) 
{
    vL <- 0
    n <- length(L)
    if (is.null(strata)) 
        strata <- rep(1, n)
    else strata <- tapply(seq_len(n), as.numeric(strata))
    S <- length(table(strata))
    for (s in 1L:S) {
        i.s <- seq_len(n)[strata == s]
        vL <- vL + sum(L[i.s]^2/length(i.s)^2)
    }
    vL
}


imp.prob <- function (boot.out = NULL, index = 1, t0 = boot.out$t0[index], 
    t = boot.out$t[, index], w = NULL, def = TRUE, q = NULL) 
{
    is.missing <- function(x) length(x) == 0L || is.na(x)
    if (missing(t) && is.null(boot.out$t)) 
        stop("bootstrap replicates must be supplied")
    if (is.null(w)) 
        if (!is.null(boot.out)) 
            w <- imp.weights(boot.out, def, q)
        else stop("either 'boot.out' or 'w' must be specified.")
    if ((length(index) > 1L) && (missing(t) || missing(t0))) {
        warning("only first element of 'index' used")
        index <- index[1L]
        if (is.missing(t)) 
            t <- boot.out$t[, index]
        if (is.missing(t0)) 
            t0 <- boot.out$t0[index]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    w <- w[fins]
    o <- order(t)
    t <- t[o]
    w <- w[o]
    raw <- rat <- reg <- rep(NA, length(t0))
    cum <- cumsum(w)/sum(w)
    cum.r <- imp.reg(w)
    for (i in seq_along(t0)) {
        raw[i] <- sum(w[t <= t0[i]])/length(w)
        rat[i] <- max(cum[t <= t0[i]])
        reg[i] <- max(cum.r[t <= t0[i]])
    }
    list(t0 = t0, raw = raw, rat = rat, reg = reg)
}


glm.diag.plots <- function (glmfit, glmdiag = glm.diag(glmfit), subset = NULL, 
    iden = FALSE, labels = NULL, ret = FALSE) 
{
    if (is.null(glmdiag)) 
        glmdiag <- glm.diag(glmfit)
    if (is.null(subset)) 
        subset <- seq_along(glmdiag$h)
    else if (is.logical(subset)) 
        subset <- seq_along(subset)[subset]
    else if (is.numeric(subset) && all(subset < 0)) 
        subset <- (1L:(length(subset) + length(glmdiag$h)))[subset]
    else if (is.character(subset)) {
        if (is.null(labels)) 
            labels <- subset
        subset <- seq_along(subset)
    }
    par(mfrow = c(2, 2))
    x1 <- predict(glmfit)
    plot(x1, glmdiag$res, xlab = "Linear predictor", ylab = "Residuals")
    pars <- vector(4L, mode = "list")
    pars[[1L]] <- par("usr")
    y2 <- glmdiag$rd
    x2 <- qnorm(ppoints(length(y2)))[rank(y2)]
    plot(x2, y2, ylab = "Quantiles of standard normal", xlab = "Ordered deviance residuals")
    abline(0, 1, lty = 2)
    pars[[2L]] <- par("usr")
    hh <- glmdiag$h/(1 - glmdiag$h)
    plot(hh, glmdiag$cook, xlab = "h/(1-h)", ylab = "Cook statistic")
    rx <- range(hh)
    ry <- range(glmdiag$cook)
    rank.fit <- glmfit$rank
    nobs <- rank.fit + glmfit$df.residual
    cooky <- 8/(nobs - 2 * rank.fit)
    hy <- (2 * rank.fit)/(nobs - 2 * rank.fit)
    if ((cooky >= ry[1L]) && (cooky <= ry[2L])) 
        abline(h = cooky, lty = 2)
    if ((hy >= rx[1L]) && (hy <= rx[2L])) 
        abline(v = hy, lty = 2)
    pars[[3L]] <- par("usr")
    plot(subset, glmdiag$cook, xlab = "Case", ylab = "Cook statistic")
    if ((cooky >= ry[1L]) && (cooky <= ry[2L])) 
        abline(h = cooky, lty = 2)
    xx <- list(x1, x2, hh, subset)
    yy <- list(glmdiag$res, y2, glmdiag$cook, glmdiag$cook)
    pars[[4L]] <- par("usr")
    if (is.null(labels)) 
        labels <- names(x1)
    while (iden) {
        cat("****************************************************\n")
        cat("Please Input a screen number (1,2,3 or 4)\n")
        cat("0 will terminate the function \n")
        num <- as.numeric(readline())
        if ((length(num) > 0L) && ((num == 1) || (num == 2) || 
            (num == 3) || (num == 4))) {
            cat(paste("Interactive Identification for screen", 
                num, "\n"))
            cat("left button = Identify, center button = Exit\n")
            nm <- num + 1
            par(mfg = c(trunc(nm/2), 1 + nm%%2, 2, 2))
            par(usr = pars[[num]])
            identify(xx[[num]], yy[[num]], labels)
        }
        else iden <- FALSE
    }
    par(mfrow = c(1, 1))
    if (ret) 
        glmdiag
    else invisible()
}


imp.weights <- function (boot.out, def = TRUE, q = NULL) 
{
    R <- boot.out$R
    if (length(R) == 1L) 
        def <- FALSE
    f <- boot.array(boot.out)
    n <- ncol(f)
    strata <- tapply(boot.out$strata, as.numeric(boot.out$strata))
    if (is.null(q)) 
        q <- rep(1, ncol(f))
    if (any(q == 0)) 
        stop("0 elements not allowed in 'q'")
    p <- boot.out$weights
    if ((length(R) == 1L) && all(abs(p - q)/p < 1e-10)) 
        return(rep(1, R))
    np <- length(R)
    q <- normalize(q, strata)
    lw.q <- as.vector(f %*% log(q))
    if (!isMatrix(p)) 
        p <- as.matrix(t(p))
    p <- t(apply(p, 1L, normalize, strata))
    lw.p <- matrix(NA, sum(R), np)
    for (i in 1L:np) {
        zz <- seq_len(n)[p[i, ] > 0]
        lw.p[, i] <- f[, zz] %*% log(p[i, zz])
    }
    if (def) 
        w <- 1/(exp(lw.p - lw.q) %*% R/sum(R))
    else {
        i <- cbind(seq_len(sum(R)), rep(seq_along(R), R))
        w <- exp(lw.q - lw.p[i])
    }
    as.vector(w)
}


jack.after.boot <- function (boot.out, index = 1, t = NULL, L = NULL, useJ = TRUE, 
    stinf = TRUE, alpha = NULL, main = "", ylab = NULL, ...) 
{
    t.o <- t
    if (is.null(t)) {
        if (length(index) > 1L) {
            index <- index[1L]
            warning("only first element of 'index' used")
        }
        t <- boot.out$t[, index]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    if (is.null(alpha)) {
        alpha <- c(0.05, 0.1, 0.16, 0.5, 0.84, 0.9, 0.95)
        if (is.null(ylab)) 
            ylab <- "5, 10, 16, 50, 84, 90, 95 %-iles of (T*-t)"
    }
    if (is.null(ylab)) 
        ylab <- "Percentiles of (T*-t)"
    data <- boot.out$data
    n <- NROW(data)
    f <- boot.array(boot.out)[fins, , drop = TRUE]
    percentiles <- matrix(data = NA, length(alpha), n)
    J <- numeric(n)
    for (j in seq_len(n)) {
        values <- t[f[, j] == 0]
        J[j] <- mean(values)
        percentiles[, j] <- quantile(values, alpha) - J[j]
    }
    if (!useJ) {
        if (is.null(L)) 
            J <- empinf(boot.out, index = index, t = t.o, ...)
        else J <- L
    }
    else J <- (n - 1) * (mean(J) - J)
    xtext <- "jackknife value"
    if (!useJ) {
        if (!is.null(L) || (is.null(t.o) && (boot.out$stype == 
            "w"))) 
            xtext <- paste("infinitesimal", xtext)
        else xtext <- paste("regression", xtext)
    }
    if (stinf) {
        J <- J/sqrt(var(J))
        xtext <- paste("standardized", xtext)
    }
    top <- max(percentiles)
    bot <- min(percentiles)
    ylts <- c(bot - 0.35 * (top - bot), top + 0.1 * (top - bot))
    percentiles <- percentiles[, order(J)]
    plot(sort(J), percentiles[1, ], ylim = ylts, type = "n", 
        xlab = xtext, ylab = ylab, main = main)
    for (j in seq_along(alpha)) lines(sort(J), percentiles[j, 
        ], type = "b", pch = "*")
    percentiles <- quantile(t, alpha) - mean(t)
    for (j in seq_along(alpha)) abline(h = percentiles[j], lty = 2)
    text(sort(J), rep(c(bot - 0.08 * (top - bot), NA, NA, NA, 
        NA), n, n), order(J), cex = 0.5)
    text(sort(J), rep(c(NA, bot - 0.14 * (top - bot), NA, NA, 
        NA), n, n), order(J), cex = 0.5)
    text(sort(J), rep(c(NA, NA, bot - 0.2 * (top - bot), NA, 
        NA), n, n), order(J), cex = 0.5)
    text(sort(J), rep(c(NA, NA, NA, bot - 0.26 * (top - bot), 
        NA), n, n), order(J), cex = 0.5)
    text(sort(J), rep(c(NA, NA, NA, NA, bot - 0.32 * (top - bot)), 
        n, n), order(J), cex = 0.5)
    invisible()
}


EL.profile <- function (y, tmin = min(y) + 0.1, tmax = max(y) - 0.1, n.t = 25, 
    u = function(y, t) y - t) 
{
    EL.loglik <- function(lambda) {
        temp <- 1 + lambda * EL.stuff$u
        if (any(temp <= 0)) 
            NA
        else -sum(log(1 + lambda * EL.stuff$u))
    }
    EL.paras <- matrix(NA, n.t, 3)
    lam <- 0.001
    for (it in 0:(n.t - 1)) {
        t <- tmin + ((tmax - tmin) * it)/(n.t - 1)
        EL.stuff <- list(u = u(y, t))
        EL.out <- nlm(EL.loglik, lam)
        i <- 1
        while (EL.out$code > 2 && (i < 20)) {
            i <- i + 1
            lam <- lam/5
            EL.out <- nlm(EL.loglik, lam)
        }
        EL.paras[1 + it, ] <- c(t, EL.loglik(EL.out$x), EL.out$x)
        lam <- EL.out$x
    }
    EL.paras[, 2] <- EL.paras[, 2] - max(EL.paras[, 2])
    EL.paras
}


saddle.distn <- function (A, u = NULL, alpha = NULL, wdist = "m", type = "simp", 
    npts = 20, t = NULL, t0 = NULL, init = rep(0.1, d), mu = rep(0.5, 
        n), LR = FALSE, strata = NULL, ...) 
{
    call <- match.call()
    if (is.null(alpha)) 
        alpha <- c(0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.2, 
            0.5, 0.8, 0.9, 0.95, 0.975, 0.99, 0.995, 0.999)
    if (is.null(t) && is.null(t0)) 
        stop("one of 't' or 't0' required")
    ep1 <- min(c(alpha, 0.01))/10
    ep2 <- (1 - max(c(alpha, 0.99)))/10
    d <- if (type == "simp") 
        1
    else if (is.function(u)) {
        if (is.null(t)) 
            length(u(t0[1L], ...))
        else length(u(t[1L], ...))
    }
    else 1L + length(u)
    i <- nsads <- 0
    if (!is.null(t)) 
        npts <- length(t)
    zeta <- matrix(NA, npts, 2L * d - 1L)
    spa <- matrix(NA, npts, 2L)
    pts <- NULL
    if (is.function(A)) {
        n <- nrow(as.matrix(A(t0[1L], ...)))
        if (is.null(u)) 
            stop("function 'u' missing")
        if (!is.function(u)) 
            stop("'u' must be a function")
        if (is.null(t)) {
            t1 <- t0[1L] - 2 * t0[2L]
            sad <- saddle(A = A(t1, ...), u = u(t1, ...), wdist = wdist, 
                type = type, d1 = 1, init = init, mu = mu, LR = LR, 
                strata = strata)
            bdu <- bdl <- NULL
            while (is.na(sad$spa[2L]) || (sad$spa[2L] > ep1) || 
                (sad$spa[2L] < ep1/100)) {
                nsads <- nsads + 1
                if (!is.na(sad$spa[2L]) && (sad$spa[2L] > ep1)) {
                  i <- i + 1
                  zeta[i, ] <- c(sad$zeta.hat, sad$zeta2.hat)
                  spa[i, ] <- sad$spa
                  pts <- c(pts, t1)
                  bdu <- t1
                }
                else bdl <- t1
                if (nsads == npts) 
                  stop("unable to find range")
                if (is.null(bdl)) {
                  t1 <- 2 * t1 - t0[1L]
                  sad <- saddle(A = A(t1, ...), u = u(t1, ...), 
                    wdist = wdist, type = type, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
                else if (is.null(bdu)) {
                  t1 <- (t0[1L] + bdl)/2
                  sad <- saddle(A = A(t1, ...), u = u(t1, ...), 
                    wdist = wdist, type = type, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
                else {
                  t1 <- (bdu + bdl)/2
                  sad <- saddle(A = A(t1, ...), u = u(t1, ...), 
                    wdist = wdist, type = type, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
            }
            i1 <- i <- i + 1
            nsads <- 0
            zeta[i, ] <- c(sad$zeta.hat, sad$zeta2.hat)
            spa[i, ] <- sad$spa
            pts <- c(pts, t1)
            t2 <- t0[1L] + 2 * t0[2L]
            sad <- saddle(A = A(t2, ...), u = u(t2, ...), wdist = wdist, 
                type = type, d1 = 1, init = init, mu = mu, LR = LR, 
                strata = strata)
            bdu <- bdl <- NULL
            while (is.na(sad$spa[2L]) || (1 - sad$spa[2L] > ep2) || 
                (1 - sad$spa[2L] < ep2/100)) {
                nsads <- nsads + 1
                if (!is.na(sad$spa[2L]) && (1 - sad$spa[2L] > 
                  ep2)) {
                  i <- i + 1
                  zeta[i, ] <- c(sad$zeta.hat, sad$zeta2.hat)
                  spa[i, ] <- sad$spa
                  pts <- c(pts, t2)
                  bdl <- t2
                }
                else bdu <- t2
                if (nsads == npts) 
                  stop("unable to find range")
                if (is.null(bdu)) {
                  t2 <- 2 * t2 - t0[1L]
                  sad <- saddle(A = A(t2, ...), u = u(t2, ...), 
                    wdist = wdist, type = type, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
                else if (is.null(bdl)) {
                  t2 <- (t0[1L] + bdu)/2
                  sad <- saddle(A = A(t2, ...), u = u(t2, ...), 
                    wdist = wdist, type = type, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
                else {
                  t2 <- (bdu + bdl)/2
                  sad <- saddle(A = A(t2, ...), u = u(t2, ...), 
                    wdist = wdist, type = type, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
            }
            i <- i + 1
            zeta[i, ] <- c(sad$zeta.hat, sad$zeta2.hat)
            spa[i, ] <- sad$spa
            pts <- c(pts, t2)
            if ((npts%%2) == 0) {
                tt1 <- seq.int(t1, t0[1L], length.out = npts/2 - 
                  i1 + 2)[-1L]
                tt2 <- seq.int(t0[1L], t2, length.out = npts/2 + 
                  i1 - i + 2)[-1L]
                t <- c(tt1[-length(tt1)], tt2[-length(tt2)])
            }
            else {
                ex <- 1 * (t1 + t2 > 2 * t0[1L])
                ll <- floor(npts/2) + 2
                tt1 <- seq.int(t1, t0[1L], length.out = ll - 
                  i1 + 1 - ex)[-1L]
                tt2 <- seq.int(t0[1L], t2, length.out = ll + 
                  i1 - i + ex)[-1L]
                t <- c(tt1[-length(tt1)], tt2[-length(tt2)])
            }
        }
        init1 <- init
        for (j in (i + 1):npts) {
            sad <- saddle(A = A(t[j - i], ...), u = u(t[j - i], 
                ...), wdist = wdist, type = type, d1 = 1, init = init1, 
                mu = mu, LR = LR, strata = strata)
            zeta[j, ] <- c(sad$zeta.hat, sad$zeta2.hat)
            init1 <- sad$zeta.hat
            spa[j, ] <- sad$spa
        }
    }
    else {
        A <- as.matrix(A)
        n <- nrow(A)
        if (is.null(t)) {
            t1 <- t0[1L] - 2 * t0[2L]
            sad <- saddle(A = A, u = c(t1, u), wdist = wdist, 
                type = type, d = d, d1 = 1, init = init, mu = mu, 
                LR = LR, strata = strata)
            bdu <- bdl <- NULL
            while (is.na(sad$spa[2L]) || (sad$spa[2L] > ep1) || 
                (sad$spa[2L] < ep1/100)) {
                if (!is.na(sad$spa[2L]) && (sad$spa[2L] > ep1)) {
                  i <- i + 1
                  zeta[i, ] <- c(sad$zeta.hat, sad$zeta2.hat)
                  spa[i, ] <- sad$spa
                  pts <- c(pts, t1)
                  bdu <- t1
                }
                else bdl <- t1
                if (i == floor(npts/2)) 
                  stop("unable to find range")
                if (is.null(bdl)) {
                  t1 <- 2 * t1 - t0[1L]
                  sad <- saddle(A = A, u = c(t1, u), wdist = wdist, 
                    type = type, d = d, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
                else if (is.null(bdu)) {
                  t1 <- (t0[1L] + bdl)/2
                  sad <- saddle(A = A, u = c(t1, u), wdist = wdist, 
                    type = type, d = d, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
                else {
                  t1 <- (bdu + bdl)/2
                  sad <- saddle(A = A, u = c(t1, u), wdist = wdist, 
                    type = type, d = d, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
            }
            i1 <- i <- i + 1
            zeta[i, ] <- c(sad$zeta.hat, sad$zeta2.hat)
            spa[i, ] <- sad$spa
            pts <- c(pts, t1)
            t2 <- t0[1L] + 2 * t0[2L]
            sad <- saddle(A = A, u = c(t2, u), wdist = wdist, 
                type = type, d = d, d1 = 1, init = init, mu = mu, 
                LR = LR, strata = strata)
            bdu <- bdl <- NULL
            while (is.na(sad$spa[2L]) || (1 - sad$spa[2L] > ep2) || 
                (1 - sad$spa[2L] < ep2/100)) {
                if (!is.na(sad$spa[2L]) && (1 - sad$spa[2L] > 
                  ep2)) {
                  i <- i + 1
                  zeta[i, ] <- c(sad$zeta.hat, sad$zeta2.hat)
                  spa[i, ] <- sad$spa
                  pts <- c(pts, t2)
                  bdl <- t2
                }
                else bdu <- t2
                if ((i - i1) == floor(npts/2)) 
                  stop("unable to find range")
                if (is.null(bdu)) {
                  t2 <- 2 * t2 - t0[1L]
                  sad <- saddle(A = A, u = c(t2, u), wdist = wdist, 
                    type = type, d = d, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
                else if (is.null(bdl)) {
                  t2 <- (t0[1L] + bdu)/2
                  sad <- saddle(A = A, u = c(t2, u), wdist = wdist, 
                    type = type, d = d, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
                else {
                  t2 <- (bdu + bdl)/2
                  sad <- saddle(A = A, u = c(t2, u), wdist = wdist, 
                    type = type, d = d, d1 = 1, init = init, 
                    mu = mu, LR = LR, strata = strata)
                }
            }
            i <- i + 1
            zeta[i, ] <- c(sad$zeta.hat, sad$zeta2.hat)
            spa[i, ] <- sad$spa
            pts <- c(pts, t2)
            if ((npts%%2) == 0) {
                tt1 <- seq.int(t1, t0[1L], length.out = npts/2 - 
                  i1 + 2)[-1L]
                tt2 <- seq.int(t0[1L], t2, length.out = npts/2 + 
                  i1 - i + 2)[-1L]
                t <- c(tt1[-length(tt1)], tt2[-length(tt2)])
            }
            else {
                ex <- 1 * (t1 + t2 > 2 * t0[1L])
                ll <- floor(npts/2) + 2
                tt1 <- seq.int(t1, t0[1L], length.out = ll - 
                  i1 + 1 - ex)[-1L]
                tt2 <- seq.int(t0[1L], t2, length.out = ll + 
                  i1 - i + ex)[-1L]
                t <- c(tt1[-length(tt1)], tt2[-length(tt2)])
            }
        }
        init1 <- init
        for (j in (i + 1):npts) {
            sad <- saddle(A = A, u = c(t[j - i], u), wdist = wdist, 
                type = type, d = d, d1 = 1, init = init, mu = mu, 
                LR = LR, strata = strata)
            zeta[j, ] <- c(sad$zeta.hat, sad$zeta2.hat)
            init1 <- sad$zeta.hat
            spa[j, ] <- sad$spa
        }
    }
    pts.in <- (1L:npts)[(abs(zeta[, 1L]) > 1e-06) & (abs(spa[, 
        2L] - 0.5) < 0.5 - 1e-10)]
    pts <- c(pts, t)[pts.in]
    zeta <- as.matrix(zeta[pts.in, ])
    spa <- spa[pts.in, ]
    distn <- smooth.spline(qnorm(spa[, 2]), pts)
    quantiles <- predict(distn, qnorm(alpha))$y
    quans <- cbind(alpha, quantiles)
    colnames(quans) <- c("alpha", "quantile")
    inds <- order(pts)
    psa <- cbind(pts[inds], spa[inds, ], zeta[inds, ])
    if (d == 1) 
        anames <- "zeta"
    else {
        anames <- rep("", 2 * d - 1)
        for (j in 1L:d) anames[j] <- paste("zeta1.", j, sep = "")
        for (j in (d + 1):(2 * d - 1)) anames[j] <- paste("zeta2.", 
            j - d, sep = "")
    }
    dimnames(psa) <- list(NULL, c("t", "gs", "Gs", anames))
    out <- list(quantiles = quans, points = psa, distn = distn, 
        call = call, LR = LR)
    class(out) <- "saddle.distn"
    out
}


glm.diag <- function (glmfit) 
{
    w <- if (is.null(glmfit$prior.weights)) 
        rep(1, length(glmfit$residuals))
    else glmfit$prior.weights
    sd <- switch(family(glmfit)$family[1L], gaussian = sqrt(glmfit$deviance/glmfit$df.residual), 
        Gamma = sqrt(sum(w * (glmfit$y/fitted(glmfit) - 1)^2)/glmfit$df.residual), 
        1)
    dev <- residuals(glmfit, type = "deviance")/sd
    pear <- residuals(glmfit, type = "pearson")/sd
    h <- rep(0, length(w))
    h[w != 0] <- lm.influence(glmfit)$hat
    p <- glmfit$rank
    rp <- pear/sqrt(1 - h)
    rd <- dev/sqrt(1 - h)
    cook <- (h * rp^2)/((1 - h) * p)
    res <- sign(dev) * sqrt(dev^2 + h * rp^2)
    list(res = res, rd = rd, rp = rp, cook = cook, h = h, sd = sd)
}


freq.array <- function (i.array) 
{
    result <- NULL
    n <- ncol(i.array)
    result <- t(apply(i.array, 1, tabulate, n))
    result
}


abc.ci <- function (data, statistic, index = 1, strata = rep(1, n), conf = 0.95, 
    eps = 0.001/n, ...) 
{
    y <- data
    n <- NROW(y)
    strata1 <- tapply(strata, as.numeric(strata))
    if (length(index) != 1L) {
        warning("only first element of 'index' used in 'abc.ci'")
        index <- index[1L]
    }
    S <- length(table(strata1))
    mat <- matrix(0, n, S)
    for (s in 1L:S) {
        gp <- seq_len(n)[strata1 == s]
        mat[gp, s] <- 1
    }
    w.orig <- rep(1/n, n)
    t0 <- statistic(y, w.orig/(w.orig %*% mat)[strata1], ...)[index]
    L <- L2 <- numeric(n)
    for (i in seq_len(n)) {
        w1 <- (1 - eps) * w.orig
        w1[i] <- w1[i] + eps
        w2 <- (1 + eps) * w.orig
        w2[i] <- w2[i] - eps
        t1 <- statistic(y, w1/(w1 %*% mat)[strata1], ...)[index]
        t2 <- statistic(y, w2/(w2 %*% mat)[strata1], ...)[index]
        L[i] <- (t1 - t2)/(2 * eps)
        L2[i] <- (t1 - 2 * t0 + t2)/eps^2
    }
    temp1 <- sum(L * L)
    sigmahat <- sqrt(temp1)/n
    ahat <- sum(L^3)/(6 * temp1^1.5)
    bhat <- sum(L2)/(2 * n * n)
    dhat <- L/(n * n * sigmahat)
    w3 <- w.orig + eps * dhat
    w4 <- w.orig - eps * dhat
    chat <- (statistic(y, w3/(w3 %*% mat)[strata1], ...)[index] - 
        2 * t0 + statistic(y, w4/(w4 %*% mat)[strata1], ...)[index])/(2 * 
        eps * eps * sigmahat)
    bprime <- ahat - (bhat/sigmahat - chat)
    alpha <- (1 + as.vector(rbind(-conf, conf)))/2
    zalpha <- qnorm(alpha)
    lalpha <- (bprime + zalpha)/(1 - ahat * (bprime + zalpha))^2
    out <- seq(alpha)
    for (i in seq_along(alpha)) {
        w.fin <- w.orig + lalpha[i] * dhat
        out[i] <- statistic(y, w.fin/(w.fin %*% mat)[strata1], 
            ...)[index]
    }
    out <- cbind(conf, matrix(out, ncol = 2L, byrow = TRUE))
    if (length(conf) == 1L) 
        out <- as.vector(out)
    out
}


cum3 <- function (a, b = a, c = a, unbiased = TRUE) 
{
    n <- length(a)
    if (unbiased) 
        mult <- n/((n - 1) * (n - 2))
    else mult <- 1/n
    mult * sum((a - mean(a)) * (b - mean(b)) * (c - mean(c)))
}


inv.logit <- function (x) 
plogis(x)


exp.tilt <- function (L, theta = NULL, t0 = 0, lambda = NULL, strata = rep(1, 
    length(L))) 
{
    tilt.dis <- function(lambda) {
        L <- para[[2L]]
        theta <- para[[1L]]
        strata <- para[[3L]]
        ns <- table(strata)
        tilt <- rep(NA, length(L))
        for (s in seq_along(ns)) {
            p <- exp(lambda * L[strata == s]/ns[s])
            tilt[strata == s] <- p/sum(p)
        }
        (sum(L * tilt) - theta)^2
    }
    tilted.prob <- function(lambda, L, strata) {
        ns <- table(strata)
        m <- length(lambda)
        tilt <- matrix(NA, m, length(L))
        for (i in 1L:m) for (s in seq_along(ns)) {
            p <- exp(lambda[i] * L[strata == s]/ns[s])
            tilt[i, strata == s] <- p/sum(p)
        }
        if (m == 1) 
            tilt <- as.vector(tilt)
        tilt
    }
    strata <- tapply(strata, as.numeric(strata))
    if (!is.null(theta)) {
        theta <- theta - t0
        m <- length(theta)
        lambda <- rep(NA, m)
        for (i in 1L:m) {
            para <- list(theta[i], L, strata)
            lambda[i] <- optim(0, tilt.dis, method = "BFGS")$par
            msd <- tilt.dis(lambda[i])
            if (is.na(msd) || (abs(msd) > 1e-06)) 
                stop(gettextf("unable to find multiplier for %f", 
                  theta[i]), domain = NA)
        }
    }
    else if (is.null(lambda)) 
        stop("'theta' or 'lambda' required")
    probs <- tilted.prob(lambda, L, strata)
    if (is.null(theta)) 
        theta <- t0 + sum(probs * L)
    else theta <- theta + t0
    list(p = probs, theta = theta, lambda = lambda)
}


tsboot <- function (tseries, statistic, R, l = NULL, sim = "model", endcorr = TRUE, 
    n.sim = NROW(tseries), orig.t = TRUE, ran.gen = function(tser, 
        n.sim, args) tser, ran.args = NULL, norm = TRUE, ..., 
    parallel = c("no", "multicore", "snow"), ncpus = getOption("boot.ncpus", 
        1L), cl = NULL) 
{
    if (missing(parallel)) 
        parallel <- getOption("boot.parallel", "no")
    parallel <- match.arg(parallel)
    have_mc <- have_snow <- FALSE
    if (parallel != "no" && ncpus > 1L) {
        if (parallel == "multicore") 
            have_mc <- .Platform$OS.type != "windows"
        else if (parallel == "snow") 
            have_snow <- TRUE
        if (!have_mc && !have_snow) 
            ncpus <- 1L
        loadNamespace("parallel")
    }
    statistic
    tscl <- class(tseries)
    R <- floor(R)
    if (R <= 0) 
        stop("'R' must be positive")
    call <- match.call()
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    t0 <- if (orig.t) 
        statistic(tseries, ...)
    else NULL
    ts.orig <- if (!isMatrix(tseries)) 
        as.matrix(tseries)
    else tseries
    n <- nrow(ts.orig)
    if (missing(n.sim)) 
        n.sim <- n
    class(ts.orig) <- tscl
    if ((sim == "model") || (sim == "scramble")) 
        l <- NULL
    else if ((is.null(l) || (l <= 0) || (l > n))) 
        stop("invalid value of 'l'")
    fn <- if (sim == "scramble") {
        rm(ts.orig)
        function(r) statistic(scramble(tseries, norm), ...)
    }
    else if (sim == "model") {
        rm(ts.orig)
        ran.gen
        ran.args
        function(r) statistic(ran.gen(tseries, n.sim, ran.args), 
            ...)
    }
    else if (sim %in% c("fixed", "geom")) {
        if (sim == "geom") 
            endcorr <- TRUE
        i.a <- ts.array(n, n.sim, R, l, sim, endcorr)
        ran.gen
        ran.args
        function(r) {
            ends <- if (sim == "geom") 
                cbind(i.a$starts[r, ], i.a$lengths[r, ])
            else cbind(i.a$starts[r, ], i.a$lengths)
            inds <- apply(ends, 1L, make.ends, n)
            inds <- if (is.list(inds)) 
                matrix(unlist(inds)[1L:n.sim], n.sim, 1L)
            else matrix(inds, n.sim, 1L)
            statistic(ran.gen(ts.orig[inds, ], n.sim, ran.args), 
                ...)
        }
    }
    else stop("unrecognized value of 'sim'")
    res <- if (ncpus > 1L && (have_mc || have_snow)) {
        if (have_mc) {
            parallel::mclapply(seq_len(R), fn, mc.cores = ncpus)
        }
        else if (have_snow) {
            list(...)
            if (is.null(cl)) {
                cl <- parallel::makePSOCKcluster(rep("localhost", 
                  ncpus))
                if (RNGkind()[1L] == "L'Ecuyer-CMRG") 
                  parallel::clusterSetRNGStream(cl)
                res <- parallel::parLapply(cl, seq_len(R), fn)
                parallel::stopCluster(cl)
                res
            }
            else parallel::parLapply(cl, seq_len(R), fn)
        }
    }
    else lapply(seq_len(R), fn)
    t <- matrix(, R, length(res[[1L]]))
    for (r in seq_len(R)) t[r, ] <- res[[r]]
    ts.return(t0 = t0, t = t, R = R, tseries = tseries, seed = seed, 
        stat = statistic, sim = sim, endcorr = endcorr, n.sim = n.sim, 
        l = l, ran.gen = ran.gen, ran.args = ran.args, call = call, 
        norm = norm)
}


cv.glm <- function (data, glmfit, cost = function(y, yhat) mean((y - yhat)^2), 
    K = n) 
{
    call <- match.call()
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    n <- nrow(data)
    if ((K > n) || (K <= 1)) 
        stop("'K' outside allowable range")
    K.o <- K
    K <- round(K)
    kvals <- unique(round(n/(1L:floor(n/2))))
    temp <- abs(kvals - K)
    if (!any(temp == 0)) 
        K <- kvals[temp == min(temp)][1L]
    if (K != K.o) 
        warning(gettextf("'K' has been set to %f", K), domain = NA)
    f <- ceiling(n/K)
    s <- sample0(rep(1L:K, f), n)
    n.s <- table(s)
    glm.y <- glmfit$y
    cost.0 <- cost(glm.y, fitted(glmfit))
    ms <- max(s)
    CV <- 0
    Call <- glmfit$call
    for (i in seq_len(ms)) {
        j.out <- seq_len(n)[(s == i)]
        j.in <- seq_len(n)[(s != i)]
        Call$data <- data[j.in, , drop = FALSE]
        d.glm <- eval.parent(Call)
        p.alpha <- n.s[i]/n
        cost.i <- cost(glm.y[j.out], predict(d.glm, data[j.out, 
            , drop = FALSE], type = "response"))
        CV <- CV + p.alpha * cost.i
        cost.0 <- cost.0 - p.alpha * cost(glm.y, predict(d.glm, 
            data, type = "response"))
    }
    list(call = call, K = K, delta = as.numeric(c(CV, CV + cost.0)), 
        seed = seed)
}


empinf <- function (boot.out = NULL, data = NULL, statistic = NULL, type = NULL, 
    stype = NULL, index = 1, t = NULL, strata = rep(1, n), eps = 0.001, 
    ...) 
{
    if (!is.null(boot.out)) {
        if (boot.out$sim == "parametric") 
            stop("influence values cannot be found from a parametric bootstrap")
        data <- boot.out$data
        if (is.null(statistic)) 
            statistic <- boot.out$statistic
        if (is.null(stype)) 
            stype <- boot.out$stype
        if (!is.null(boot.out$strata)) 
            strata <- boot.out$strata
    }
    else {
        if (is.null(data)) 
            stop("neither 'data' nor bootstrap object specified")
        if (is.null(statistic)) 
            stop("neither 'statistic' nor bootstrap object specified")
        if (is.null(stype)) 
            stype <- "w"
    }
    n <- NROW(data)
    if (is.null(type)) {
        if (!is.null(t)) 
            type <- "reg"
        else if (stype == "w") 
            type <- "inf"
        else if (!is.null(boot.out) && (boot.out$sim != "parametric") && 
            (boot.out$sim != "permutation")) 
            type <- "reg"
        else type <- "jack"
    }
    if (type == "inf") {
        if (stype != "w") 
            stop("'stype' must be \"w\" for type=\"inf\"")
        if (length(index) != 1L) {
            warning("only first element of 'index' used")
            index <- index[1L]
        }
        if (!is.null(t)) 
            warning("input 't' ignored; type=\"inf\"")
        L <- inf.jack(data, statistic, index, strata, eps, ...)
    }
    else if (type == "reg") {
        if (is.null(boot.out)) 
            stop("bootstrap object needed for type=\"reg\"")
        if (is.null(t)) {
            if (length(index) != 1L) {
                warning("only first element of 'index' used")
                index <- index[1L]
            }
            t <- boot.out$t[, index]
        }
        L <- empinf.reg(boot.out, t)
    }
    else if (type == "jack") {
        if (!is.null(t)) 
            warning("input 't' ignored; type=\"jack\"")
        if (length(index) != 1L) {
            warning("only first element of 'index' used")
            index <- index[1L]
        }
        L <- usual.jack(data, statistic, stype, index, strata, 
            ...)
    }
    else if (type == "pos") {
        if (!is.null(t)) 
            warning("input 't' ignored; type=\"pos\"")
        if (length(index) != 1L) {
            warning("only first element of 'index' used")
            index <- index[1L]
        }
        L <- positive.jack(data, statistic, stype, index, strata, 
            ...)
    }
    L
}


linear.approx <- function (boot.out, L = NULL, index = 1, type = NULL, t0 = NULL, 
    t = NULL, ...) 
{
    f <- boot.array(boot.out)
    n <- length(f[1, ])
    if ((length(index) > 1L) && (is.null(t0) || is.null(t))) {
        warning("only first element of 'index' used")
        index <- index[1L]
    }
    if (is.null(t0)) {
        t0 <- boot.out$t0[index]
        if (is.null(L)) 
            L <- empinf(boot.out, index = index, type = type, 
                ...)
    }
    else if (is.null(t) && is.null(L)) {
        warning("input 't0' ignored: neither 't' nor 'L' supplied")
        t0 <- t0[index]
        L <- empinf(boot.out, index = index, type = type, ...)
    }
    else if (is.null(L)) 
        L <- empinf(boot.out, type = type, t = t, ...)
    tL <- rep(t0, boot.out$R)
    strata <- boot.out$strata
    if (is.null(strata)) 
        strata <- rep(1, n)
    else strata <- tapply(strata, as.numeric(strata))
    S <- length(table(strata))
    for (s in 1L:S) {
        i.s <- seq_len(n)[strata == s]
        tL <- tL + f[, i.s] %*% L[i.s]/length(i.s)
    }
    as.vector(tL)
}


boot.array <- function (boot.out, indices = FALSE) 
{
    if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        temp <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    else temp <- NULL
    assign(".Random.seed", boot.out$seed, envir = .GlobalEnv)
    n <- NROW(boot.out$data)
    R <- boot.out$R
    sim <- boot.out$sim
    if (boot.out$call[[1L]] == "tsboot") {
        if (missing(indices)) 
            indices <- TRUE
        if (sim == "model") 
            stop("index array not defined for model-based resampling")
        n.sim <- boot.out$n.sim
        i.a <- ts.array(n, n.sim, R, boot.out$l, sim, boot.out$endcorr)
        out <- matrix(NA, R, n.sim)
        for (r in seq_len(R)) {
            if (sim == "geom") 
                ends <- cbind(i.a$starts[r, ], i.a$lengths[r, 
                  ])
            else ends <- cbind(i.a$starts[r, ], i.a$lengths)
            inds <- apply(ends, 1L, make.ends, n)
            if (is.list(inds)) 
                inds <- unlist(inds)[seq_len(n.sim)]
            out[r, ] <- inds
        }
    }
    else if (boot.out$call[[1L]] == "censboot") {
        if (sim == "ordinary") {
            strata <- tapply(seq_len(n), as.numeric(boot.out$strata))
            out <- cens.case(n, strata, R)
        }
        else stop("boot.array not implemented for this object")
    }
    else {
        if (sim == "parametric") 
            stop("array cannot be found for parametric bootstrap")
        strata <- tapply(seq_len(n), as.numeric(boot.out$strata))
        if (boot.out$call[[1L]] == "tilt.boot") 
            weights <- boot.out$weights
        else {
            weights <- boot.out$call$weights
            if (!is.null(weights)) 
                weights <- boot.out$weights
        }
        out <- index.array(n, R, sim, strata, 0, boot.out$L, 
            weights)
    }
    if (!indices) 
        out <- freq.array(out)
    if (!is.null(temp)) 
        assign(".Random.seed", temp, envir = .GlobalEnv)
    else rm(.Random.seed, pos = 1)
    out
}


norm.ci <- function (boot.out = NULL, conf = 0.95, index = 1, var.t0 = NULL, 
    t0 = NULL, t = NULL, L = NULL, h = function(t) t, hdot = function(t) 1, 
    hinv = function(t) t) 
{
    if (is.null(t0)) {
        if (!is.null(boot.out)) 
            t0 <- boot.out$t0[index]
        else stop("bootstrap output object or 't0' required")
    }
    if (!is.null(boot.out) && is.null(t)) 
        t <- boot.out$t[, index]
    if (!is.null(t)) {
        fins <- seq_along(t)[is.finite(t)]
        t <- h(t[fins])
    }
    if (is.null(var.t0)) {
        if (is.null(t)) {
            if (is.null(L)) 
                stop("unable to calculate 'var.t0'")
            else var.t0 <- sum((hdot(t0) * L/length(L))^2)
        }
        else var.t0 <- var(t)
    }
    else var.t0 <- var.t0 * hdot(t0)^2
    t0 <- h(t0)
    if (!is.null(t)) 
        bias <- mean(t) - t0
    else bias <- 0
    merr <- sqrt(var.t0) * qnorm((1 + conf)/2)
    out <- cbind(conf, hinv(t0 - bias - merr), hinv(t0 - bias + 
        merr))
    out
}


corr <- function (d, w = rep(1, nrow(d))/nrow(d)) 
{
    s <- sum(w)
    m1 <- sum(d[, 1L] * w)/s
    m2 <- sum(d[, 2L] * w)/s
    (sum(d[, 1L] * d[, 2L] * w)/s - m1 * m2)/sqrt((sum(d[, 1L]^2 * 
        w)/s - m1^2) * (sum(d[, 2L]^2 * w)/s - m2^2))
}


imp.moments <- function (boot.out = NULL, index = 1, t = boot.out$t[, index], 
    w = NULL, def = TRUE, q = NULL) 
{
    if (missing(t) && is.null(boot.out$t)) 
        stop("bootstrap replicates must be supplied")
    if (is.null(w)) 
        if (!is.null(boot.out)) 
            w <- imp.weights(boot.out, def, q)
        else stop("either 'boot.out' or 'w' must be specified.")
    if ((length(index) > 1L) && missing(t)) {
        warning("only first element of 'index' used")
        t <- boot.out$t[, index[1L]]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    w <- w[fins]
    if (!const(w)) {
        y <- t * w
        m.raw <- mean(y)
        m.rat <- sum(y)/sum(w)
        t.lm <- lm(y ~ w)
        m.reg <- mean(y) - coefficients(t.lm)[2L] * (mean(w) - 
            1)
        v.raw <- mean(w * (t - m.raw)^2)
        v.rat <- sum(w/sum(w) * (t - m.rat)^2)
        x <- w * (t - m.reg)^2
        t.lm2 <- lm(x ~ w)
        v.reg <- mean(x) - coefficients(t.lm2)[2L] * (mean(w) - 
            1)
    }
    else {
        m.raw <- m.rat <- m.reg <- mean(t)
        v.raw <- v.rat <- v.reg <- var(t)
    }
    list(raw = c(m.raw, v.raw), rat = c(m.rat, v.rat), reg = as.vector(c(m.reg, 
        v.reg)))
}


tilt.boot <- function (data, statistic, R, sim = "ordinary", stype = "i", 
    strata = rep(1, n), L = NULL, theta = NULL, alpha = c(0.025, 
        0.975), tilt = TRUE, width = 0.5, index = 1, ...) 
{
    if ((sim != "ordinary") && (sim != "balanced")) 
        stop("invalid value of 'sim' supplied")
    if (!is.null(theta) && (length(R) != length(theta) + 1)) 
        stop("'R' and 'theta' have incompatible lengths")
    if (!tilt && (R[1L] == 0)) 
        stop("R[1L] must be positive for frequency smoothing")
    call <- match.call()
    n <- NROW(data)
    if (R[1L] > 0) {
        if (is.null(theta) && (length(R) != length(alpha) + 1)) 
            stop("'R' and 'alpha' have incompatible lengths")
        boot0 <- boot(data, statistic, R = R[1L], sim = sim, 
            stype = stype, strata = strata, ...)
        if (is.null(theta)) {
            if (any(c(alpha, 1 - alpha) * (R[1L] + 1) <= 5)) 
                warning("extreme values used for quantiles")
            theta <- quantile(boot0$t[, index], alpha)
        }
    }
    else {
        tilt <- TRUE
        if (is.null(theta)) 
            stop("'theta' must be supplied if R[1L] = 0")
        if (!missing(alpha)) 
            warning("'alpha' ignored; R[1L] = 0")
        if (stype == "i") 
            orig <- seq_len(n)
        else if (stype == "f") 
            orig <- rep(1, n)
        else orig <- rep(1, n)/n
        boot0 <- boot.return(sim = sim, t0 = statistic(data, 
            orig, ...), t = NULL, strata = strata, R = 0, data = data, 
            stat = statistic, stype = stype, call = NULL, seed = get(".Random.seed", 
                envir = .GlobalEnv, inherits = FALSE), m = 0, 
            weights = NULL)
    }
    if (is.null(L) & tilt) 
        if (R[1L] > 0) 
            L <- empinf(boot0, index, ...)
        else L <- empinf(data = data, statistic = statistic, 
            stype = stype, index = index, ...)
    if (tilt) 
        probs <- exp.tilt(L, theta, strata = strata, t0 = boot0$t0[index])$p
    else probs <- smooth.f(theta, boot0, index, width = width)
    boot1 <- boot(data, statistic, R[-1L], sim = sim, stype = stype, 
        strata = strata, weights = probs, ...)
    boot0$t <- rbind(boot0$t, boot1$t)
    boot0$weights <- rbind(boot0$weights, boot1$weights)
    boot0$R <- c(boot0$R, boot1$R)
    boot0$call <- call
    boot0$theta <- theta
    boot0
}


EEF.profile <- function (y, tmin = min(y) + 0.1, tmax = max(y) - 0.1, n.t = 25, 
    u = function(y, t) y - t) 
{
    EEF.paras <- matrix(NA, n.t + 1, 4)
    for (it in 0:n.t) {
        t <- tmin + (tmax - tmin) * it/n.t
        psi <- as.vector(u(y, t))
        fit <- glm(zero ~ psi - 1, poisson(log))
        f <- fitted(fit)
        EEF.paras[1 + it, ] <- c(t, sum(log(f) - log(sum(f))), 
            sum(f - 1), coefficients(fit))
    }
    EEF.paras[, 2] <- EEF.paras[, 2] - max(EEF.paras[, 2])
    EEF.paras[, 3] <- EEF.paras[, 3] - max(EEF.paras[, 3])
    EEF.paras
}


imp.quantile <- function (boot.out = NULL, alpha = NULL, index = 1, t = boot.out$t[, 
    index], w = NULL, def = TRUE, q = NULL) 
{
    if (missing(t) && is.null(boot.out$t)) 
        stop("bootstrap replicates must be supplied")
    if (is.null(alpha)) 
        alpha <- c(0.01, 0.025, 0.05, 0.95, 0.975, 0.99)
    if (is.null(w)) 
        if (!is.null(boot.out)) 
            w <- imp.weights(boot.out, def, q)
        else stop("either 'boot.out' or 'w' must be specified.")
    if ((length(index) > 1L) && missing(t)) {
        warning("only first element of 'index' used")
        t <- boot.out$t[, index[1L]]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    w <- w[fins]
    o <- order(t)
    t <- t[o]
    w <- w[o]
    cum <- cumsum(w)
    o <- rev(o)
    w.m <- w[o]
    t.m <- -rev(t)
    cum.m <- cumsum(w.m)
    cum.rat <- cum/mean(w)
    cum.reg <- imp.reg(w)
    R <- length(w)
    raw <- rat <- reg <- rep(NA, length(alpha))
    for (i in seq_along(alpha)) {
        if (alpha[i] <= 0.5) 
            raw[i] <- max(t[cum <= (R + 1) * alpha[i]])
        else raw[i] <- -max(t.m[cum.m <= (R + 1) * (1 - alpha[i])])
        rat[i] <- max(t[cum.rat <= (R + 1) * alpha[i]])
        reg[i] <- max(t[cum.reg <= (R + 1) * alpha[i]])
    }
    list(alpha = alpha, raw = raw, rat = rat, reg = reg)
}


saddle <- function (A = NULL, u = NULL, wdist = "m", type = "simp", d = NULL, 
    d1 = 1, init = rep(0.1, d), mu = rep(0.5, n), LR = FALSE, 
    strata = NULL, K.adj = NULL, K2 = NULL) 
{
    det <- function(mat) {
        if (any(is.na(mat))) 
            NA
        else if (!all(is.finite(mat))) 
            Inf
        else abs(prod(eigen(mat, only.values = TRUE)$values))
    }
    sgn <- function(x, eps = 1e-10) if (abs(x) < eps) 
        0
    else 2 * (x > 0) - 1
    if (!is.null(A)) {
        A <- as.matrix(A)
        d <- ncol(A)
        if (length(u) != d) 
            stop(gettextf("number of columns of 'A' (%d) not equal to length of 'u' (%d)", 
                d, length(u)), domain = NA)
        n <- nrow(A)
    }
    else if (is.null(K.adj)) 
        stop("either 'A' and 'u' or 'K.adj' and 'K2' must be supplied")
    if (!is.null(K.adj)) {
        if (is.null(d)) 
            d <- 1
        type <- "simp"
        wdist <- "o"
        speq <- suppressWarnings(optim(init, K.adj))
        if (speq$convergence == 0) {
            ahat <- speq$par
            Khat <- K.adj(ahat)
            K2hat <- det(K2(ahat))
            gs <- 1/sqrt((2 * pi)^d * K2hat) * exp(Khat)
            if (d == 1) {
                r <- sgn(ahat) * sqrt(-2 * Khat)
                v <- ahat * sqrt(K2hat)
                if (LR) 
                  Gs <- pnorm(r) + dnorm(r) * (1/r + 1/v)
                else Gs <- pnorm(r + log(v/r)/r)
            }
            else Gs <- NA
        }
        else gs <- Gs <- ahat <- NA
    }
    else if (wdist == "m") {
        type <- "simp"
        if (is.null(strata)) {
            p <- mu/sum(mu)
            para <- list(p, A, u, n)
            K <- function(al) {
                w <- para[[1L]] * exp(al %*% t(para[[2L]]))
                para[[4L]] * log(sum(w)) - sum(al * para[[3L]])
            }
            speq <- suppressWarnings(optim(init, K))
            ahat <- speq$par
            w <- as.vector(p * exp(ahat %*% t(A)))
            Khat <- n * log(sum(w)) - sum(ahat * u)
            sw <- sum(w)
            if (d == 1) 
                K2hat <- n * (sum(w * A * A)/sw - (sum(w * A)/sw)^2)
            else {
                saw <- w %*% A
                sa2w <- t(matrix(w, n, d) * A) %*% A
                K2hat <- det(n/sw * (sa2w - (saw %*% t(saw))/sw))
            }
        }
        else {
            sm <- as.vector(tapply(mu, strata, sum)[strata])
            p <- mu/sm
            ns <- table(strata)
            para <- list(p, A, u, strata, ns)
            K <- function(al) {
                w <- para[[1L]] * exp(al %*% t(para[[2L]]))
                sum(para[[5]] * log(tapply(c(w), para[[4L]], 
                  sum))) - sum(al * para[[3L]])
            }
            speq <- suppressWarnings(optim(init, K))
            ahat <- speq$par
            w <- p * exp(ahat %*% t(A))
            Khat <- sum(ns * log(tapply(w, strata, sum))) - sum(ahat * 
                u)
            temp <- matrix(0, d, d)
            for (s in seq_along(ns)) {
                gp <- seq_len(n)[strata == s]
                sw <- sum(w[gp])
                saw <- w[gp] %*% A[gp, ]
                sa2w <- t(matrix(w[gp], ns[s], d) * A[gp, ]) %*% 
                  A[gp, ]
                temp <- temp + ns[s]/sw * (sa2w - (saw %*% t(saw))/sw)
            }
            K2hat <- det(temp)
        }
        if (speq$convergence == 0) {
            gs <- 1/sqrt(2 * pi * K2hat)^d * exp(Khat)
            if (d == 1) {
                r <- sgn(ahat) * sqrt(-2 * Khat)
                v <- ahat * sqrt(K2hat)
                if (LR) 
                  Gs <- pnorm(r) + dnorm(r) * (1/r - 1/v)
                else Gs <- pnorm(r + log(v/r)/r)
            }
            else Gs <- NA
        }
        else gs <- Gs <- ahat <- NA
    }
    else if (wdist == "p") {
        if (type == "cond") {
            smp <- simplex(rep(0, n), A3 = t(A), b3 = u)
            if (smp$solved == 1) {
                y <- smp$soln
                A1 <- A[, 1L:d1]
                A2 <- A[, -(1L:d1)]
                mod1 <- summary(glm(y ~ A1 + A2 + offset(log(mu)) - 
                  1, poisson, control = glm.control(maxit = 100)))
                mod2 <- summary(glm(y ~ A2 + offset(log(mu)) - 
                  1, poisson, control = glm.control(maxit = 100)))
                ahat <- mod1$coefficients[, 1L]
                ahat2 <- mod2$coefficients[, 1L]
                temp1 <- mod2$deviance - mod1$deviance
                temp2 <- det(mod2$cov.unscaled)/det(mod1$cov.unscaled)
                gs <- 1/sqrt((2 * pi)^d1 * temp2) * exp(-temp1/2)
                if (d1 == 1) {
                  r <- sgn(ahat[1L]) * sqrt(temp1)
                  v <- ahat[1L] * sqrt(temp2)
                  if (LR) 
                    Gs <- pnorm(r) + dnorm(r) * (1/r - 1/v)
                  else Gs <- pnorm(r + log(v/r)/r)
                }
                else Gs <- NA
            }
            else {
                ahat <- ahat2 <- NA
                gs <- Gs <- NA
            }
        }
        else stop("this type not implemented for Poisson")
    }
    else if (wdist == "b") {
        if (type == "cond") {
            smp <- simplex(rep(0, n), A1 = iden(n), b1 = rep(1 - 
                2e-06, n), A3 = t(A), b3 = u - 1e-06 * apply(A, 
                2L, sum))
            if (smp$solved == 1) {
                y <- smp$soln + 1e-06
                A1 <- A[, 1L:d1]
                A2 <- A[, -(1L:d1)]
                mod1 <- summary(glm(cbind(y, 1 - y) ~ A1 + A2 + 
                  offset(qlogis(mu)) - 1, binomial, control = glm.control(maxit = 100)))
                mod2 <- summary(glm(cbind(y, 1 - y) ~ A2 + offset(qlogis(mu)) - 
                  1, binomial, control = glm.control(maxit = 100)))
                ahat <- mod1$coefficients[, 1L]
                ahat2 <- mod2$coefficients[, 1L]
                temp1 <- mod2$deviance - mod1$deviance
                temp2 <- det(mod2$cov.unscaled)/det(mod1$cov.unscaled)
                gs <- 1/sqrt((2 * pi)^d1 * temp2) * exp(-temp1/2)
                if (d1 == 1) {
                  r <- sgn(ahat[1L]) * sqrt(temp1)
                  v <- ahat[1L] * sqrt(temp2)
                  if (LR) 
                    Gs <- pnorm(r) + dnorm(r) * (1/r - 1/v)
                  else Gs <- pnorm(r + log(v/r)/r)
                }
                else Gs <- NA
            }
            else {
                ahat <- ahat2 <- NA
                gs <- Gs <- NA
            }
        }
        else stop("this type not implemented for Binary")
    }
    if (type == "simp") 
        out <- list(spa = c(gs, Gs), zeta.hat = ahat)
    else out <- list(spa = c(gs, Gs), zeta.hat = ahat, zeta2.hat = ahat2)
    names(out$spa) <- c("pdf", "cdf")
    out
}


nested.corr <- function (data, w, t0, M) 
{
    data <- unname(as.matrix(data))
    corr.fun <- function(d, w = rep(1, nrow(d))/nrow(d)) {
        x <- d[, 1L]
        y <- d[, 2L]
        w <- w/sum(w)
        n <- nrow(d)
        m1 <- sum(x * w)
        m2 <- sum(y * w)
        v1 <- sum(x^2 * w) - m1^2
        v2 <- sum(y^2 * w) - m2^2
        rho <- (sum(x * y * w) - m1 * m2)/sqrt(v1 * v2)
        i <- rep(1L:n, round(n * w))
        us <- (x[i] - m1)/sqrt(v1)
        xs <- (y[i] - m2)/sqrt(v2)
        L <- us * xs - 0.5 * rho * (us^2 + xs^2)
        c(rho, sum(L^2)/nrow(d)^2)
    }
    n <- nrow(data)
    i <- rep(1L:n, round(n * w))
    t <- corr.fun(data, w)
    z <- (t[1L] - t0)/sqrt(t[2L])
    nested.boot <- boot(data[i, ], corr.fun, R = M, stype = "w")
    z.nested <- (nested.boot$t[, 1L] - t[1L])/sqrt(nested.boot$t[, 
        2L])
    c(z, sum(z.nested < z)/(M + 1))
}


k3.linear <- function (L, strata = NULL) 
{
    k3L <- 0
    n <- length(L)
    if (is.null(strata)) 
        strata <- rep(1, n)
    else strata <- tapply(seq_len(n), as.numeric(strata))
    S <- length(table(strata))
    for (s in 1L:S) {
        i.s <- seq_len(n)[strata == s]
        k3L <- k3L + sum(L[i.s]^3/length(i.s)^3)
    }
    k3L
}


logit <- function (p) 
qlogis(p)


censboot <- function (data, statistic, R, F.surv, G.surv, strata = matrix(1, 
    n, 2), sim = "ordinary", cox = NULL, index = c(1, 2), ..., 
    parallel = c("no", "multicore", "snow"), ncpus = getOption("boot.ncpus", 
        1L), cl = NULL) 
{
    mstrata <- missing(strata)
    if (any(is.na(data))) 
        stop("missing values not allowed in 'data'")
    if ((sim != "ordinary") && (sim != "model") && (sim != "cond") && 
        (sim != "weird")) 
        stop("unknown value of 'sim'")
    if ((sim == "model") && (is.null(cox))) 
        sim <- "ordinary"
    if (missing(parallel)) 
        parallel <- getOption("boot.parallel", "no")
    parallel <- match.arg(parallel)
    have_mc <- have_snow <- FALSE
    if (parallel != "no" && ncpus > 1L) {
        if (parallel == "multicore") 
            have_mc <- .Platform$OS.type != "windows"
        else if (parallel == "snow") 
            have_snow <- TRUE
        if (!have_mc && !have_snow) 
            ncpus <- 1L
        loadNamespace("parallel")
    }
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    call <- match.call()
    if (isMatrix(data)) 
        n <- nrow(data)
    else stop("'data' must be a matrix with at least 2 columns")
    if (ncol(data) < 2L) 
        stop("'data' must be a matrix with at least 2 columns")
    if (length(index) < 2L) 
        stop("'index' must contain 2 elements")
    if (length(index) > 2L) {
        warning("only first 2 elements of 'index' used")
        index <- index[1L:2L]
    }
    if (ncol(data) < max(index)) 
        stop("indices are incompatible with 'ncol(data)'")
    if (sim == "weird") {
        if (!is.null(cox)) 
            stop("sim = \"weird\" cannot be used with a \"coxph\" object")
        if (ncol(data) > 2L) 
            warning(gettextf("only columns %s and %s of 'data' used", 
                index[1L], index[2L]), domain = NA)
        data <- data[, index]
    }
    if (!is.null(cox) && is.null(cox$coefficients) && ((sim == 
        "cond") || (sim == "model"))) {
        warning("no coefficients in Cox model -- model ignored")
        cox <- NULL
    }
    if ((sim != "ordinary") && missing(F.surv)) 
        stop("'F.surv' is required but missing")
    if (missing(G.surv) && ((sim == "cond") || (sim == "model"))) 
        stop("'G.surv' is required but missing")
    if (NROW(strata) != n) 
        stop("'strata' of wrong length")
    if (!isMatrix(strata)) {
        if (!((sim == "weird") || (sim == "ordinary"))) 
            strata <- cbind(strata, 1)
    }
    else {
        if ((sim == "weird") || (sim == "ordinary")) 
            strata <- strata[, 1L]
        else strata <- strata[, 1L:2L]
    }
    temp.str <- strata
    strata <- if (isMatrix(strata)) 
        apply(strata, 2L, function(s, n) tapply(seq_len(n), as.numeric(s)), 
            n)
    else tapply(seq_len(n), as.numeric(strata))
    t0 <- if ((sim == "weird") && !mstrata) 
        statistic(data, temp.str, ...)
    else statistic(data, ...)
    fn <- if (sim == "ordinary") {
        bt <- cens.case(n, strata, R)
        function(r) statistic(data[sort(bt[r, ]), ], ...)
    }
    else if (sim == "weird") {
        data
        F.surv
        if (!mstrata) {
            function(r) {
                bootdata <- cens.weird(data, F.surv, strata)
                statistic(bootdata[, 1:2], bootdata[, 3L], ...)
            }
        }
        else {
            function(r) {
                bootdata <- cens.weird(data, F.surv, strata)
                statistic(bootdata[, 1:2], ...)
            }
        }
    }
    else {
        bt <- cens.resamp(data, R, F.surv, G.surv, strata, index, 
            cox, sim)
        function(r) {
            bootdata <- data
            bootdata[, index] <- bt[r, , ]
            oi <- order(bt[r, , 1L], 1 - bt[r, , 2L])
            statistic(bootdata[oi, ], ...)
        }
    }
    rm(mstrata)
    res <- if (ncpus > 1L && (have_mc || have_snow)) {
        if (have_mc) {
            parallel::mclapply(seq_len(R), fn, ..., mc.cores = ncpus)
        }
        else if (have_snow) {
            list(...)
            if (is.null(cl)) {
                cl <- parallel::makePSOCKcluster(rep("localhost", 
                  ncpus))
                if (RNGkind()[1L] == "L'Ecuyer-CMRG") 
                  parallel::clusterSetRNGStream(cl)
                parallel::clusterEvalQ(cl, library(survival))
                res <- parallel::parLapply(cl, seq_len(R), fn)
                parallel::stopCluster(cl)
                res
            }
            else {
                parallel::clusterEvalQ(cl, library(survival))
                parallel::parLapply(cl, seq_len(R), fn)
            }
        }
    }
    else lapply(seq_len(R), fn)
    t <- matrix(, R, length(t0))
    for (r in seq_len(R)) t[r, ] <- res[[r]]
    cens.return(sim, t0, t, temp.str, R, data, statistic, call, 
        seed)
}


smooth.f <- function (theta, boot.out, index = 1, t = boot.out$t[, index], 
    width = 0.5) 
{
    if ((length(index) > 1L) && missing(t)) {
        warning("only first element of 'index' used")
        t <- boot.out$t[, index[1L]]
    }
    if (isMatrix(t)) {
        warning("only first column of 't' used")
        t <- t[, 1L]
    }
    fins <- seq_along(t)[is.finite(t)]
    t <- t[fins]
    m <- length(theta)
    v <- imp.moments(boot.out, t = t)$reg[2L]
    eps <- width * sqrt(v)
    if (m == 1) 
        w <- dnorm((theta - t)/eps)/eps
    else {
        w <- matrix(0, length(t), m)
        for (i in 1L:m) w[, i] <- dnorm((theta[i] - t)/eps)/eps
    }
    f <- crossprod(boot.array(boot.out)[fins, ], w)
    strata <- boot.out$strata
    strata <- tapply(strata, as.numeric(strata))
    ns <- table(strata)
    out <- matrix(NA, ncol(f), nrow(f))
    for (s in seq_along(ns)) {
        ts <- matrix(f[strata == s, ], m, ns[s], byrow = TRUE)
        ss <- apply(ts, 1L, sum)
        out[, strata == s] <- ts/matrix(ss, m, ns[s])
    }
    if (m == 1) 
        out <- as.vector(out)
    out
}


simplex <- function (a, A1 = NULL, b1 = NULL, A2 = NULL, b2 = NULL, A3 = NULL, 
    b3 = NULL, maxi = FALSE, n.iter = n + 2 * m, eps = 1e-10) 
{
    call <- match.call()
    if (!is.null(A1)) 
        if (is.matrix(A1)) 
            m1 <- nrow(A1)
        else m1 <- 1
    else m1 <- 0
    if (!is.null(A2)) 
        if (is.matrix(A2)) 
            m2 <- nrow(A2)
        else m2 <- 1
    else m2 <- 0
    if (!is.null(A3)) 
        if (is.matrix(A3)) 
            m3 <- nrow(A3)
        else m3 <- 1
    else m3 <- 0
    m <- m1 + m2 + m3
    n <- length(a)
    a.o <- a
    if (maxi) 
        a <- -a
    if (m2 + m3 == 0) 
        out <- simplex1(c(a, rep(0, m1)), cbind(A1, iden(m1)), 
            b1, c(rep(0, m1), b1), n + (1L:m1), eps = eps)
    else {
        if (m2 > 0) 
            out1 <- simplex1(c(a, rep(0, m1 + 2 * m2 + m3)), 
                cbind(rbind(A1, A2, A3), rbind(iden(m1), zero(m2 + 
                  m3, m1)), rbind(zero(m1, m2), -iden(m2), zero(m3, 
                  m2)), rbind(zero(m1, m2 + m3), iden(m2 + m3))), 
                c(b1, b2, b3), c(rep(0, n), b1, rep(0, m2), b2, 
                  b3), c(n + (1L:m1), (n + m1 + m2) + (1L:(m2 + 
                  m3))), stage = 1, n1 = n + m1 + m2, n.iter = n.iter, 
                eps = eps)
        else out1 <- simplex1(c(a, rep(0, m1 + m3)), cbind(rbind(A1, 
            A3), iden(m1 + m3)), c(b1, b3), c(rep(0, n), b1, 
            b3), n + (1L:(m1 + m3)), stage = 1, n1 = n + m1, 
            n.iter = n.iter, eps = eps)
        if (out1$val.aux > eps) 
            out <- out1
        else out <- simplex1(out1$a[1L:(n + m1 + m2)], out1$A[, 
            1L:(n + m1 + m2)], out1$soln[out1$basic], out1$soln[1L:(n + 
            m1 + m2)], out1$basic, val = out1$value, n.iter = n.iter, 
            eps = eps)
    }
    if (maxi) 
        out$value <- -out$value
    out$maxi <- maxi
    if (m1 > 0L) 
        out$slack <- out$soln[n + (1L:m1)]
    if (m2 > 0L) 
        out$surplus <- out$soln[n + m1 + (1L:m2)]
    if (out$solved == -1) 
        out$artificial <- out$soln[-(1L:n + m1 + m2)]
    out$obj <- a.o
    names(out$obj) <- paste("x", seq_len(n), sep = "")
    out$soln <- out$soln[seq_len(n)]
    names(out$soln) <- paste("x", seq_len(n), sep = "")
    out$call <- call
    class(out) <- "simplex"
    out
}


boot <- function (data, statistic, R, sim = "ordinary", stype = c("i", 
    "f", "w"), strata = rep(1, n), L = NULL, m = 0, weights = NULL, 
    ran.gen = function(d, p) d, mle = NULL, simple = FALSE, ..., 
    parallel = c("no", "multicore", "snow"), ncpus = getOption("boot.ncpus", 
        1L), cl = NULL) 
{
    call <- match.call()
    stype <- match.arg(stype)
    if (missing(parallel)) 
        parallel <- getOption("boot.parallel", "no")
    parallel <- match.arg(parallel)
    have_mc <- have_snow <- FALSE
    if (parallel != "no" && ncpus > 1L) {
        if (parallel == "multicore") 
            have_mc <- .Platform$OS.type != "windows"
        else if (parallel == "snow") 
            have_snow <- TRUE
        if (!have_mc && !have_snow) 
            ncpus <- 1L
        loadNamespace("parallel")
    }
    if (simple && (sim != "ordinary" || stype != "i" || sum(m))) {
        warning("'simple=TRUE' is only valid for 'sim=\"ordinary\", stype=\"i\", n=0', so ignored")
        simple <- FALSE
    }
    if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
        runif(1)
    seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    n <- NROW(data)
    if ((n == 0) || is.null(n)) 
        stop("no data in call to 'boot'")
    temp.str <- strata
    strata <- tapply(seq_len(n), as.numeric(strata))
    t0 <- if (sim != "parametric") {
        if ((sim == "antithetic") && is.null(L)) 
            L <- empinf(data = data, statistic = statistic, stype = stype, 
                strata = strata, ...)
        if (sim != "ordinary") 
            m <- 0
        else if (any(m < 0)) 
            stop("negative value of 'm' supplied")
        if ((length(m) != 1L) && (length(m) != length(table(strata)))) 
            stop("length of 'm' incompatible with 'strata'")
        if ((sim == "ordinary") || (sim == "balanced")) {
            if (isMatrix(weights) && (nrow(weights) != length(R))) 
                stop("dimensions of 'R' and 'weights' do not match")
        }
        else weights <- NULL
        if (!is.null(weights)) 
            weights <- t(apply(matrix(weights, n, length(R), 
                byrow = TRUE), 2L, normalize, strata))
        if (!simple) 
            i <- index.array(n, R, sim, strata, m, L, weights)
        original <- if (stype == "f") 
            rep(1, n)
        else if (stype == "w") {
            ns <- tabulate(strata)[strata]
            1/ns
        }
        else seq_len(n)
        t0 <- if (sum(m) > 0L) 
            statistic(data, original, rep(1, sum(m)), ...)
        else statistic(data, original, ...)
        rm(original)
        t0
    }
    else statistic(data, ...)
    pred.i <- NULL
    fn <- if (sim == "parametric") {
        ran.gen
        data
        mle
        function(r) {
            dd <- ran.gen(data, mle)
            statistic(dd, ...)
        }
    }
    else {
        if (!simple && ncol(i) > n) {
            pred.i <- as.matrix(i[, (n + 1L):ncol(i)])
            i <- i[, seq_len(n)]
        }
        if (stype %in% c("f", "w")) {
            f <- freq.array(i)
            rm(i)
            if (stype == "w") 
                f <- f/ns
            if (sum(m) == 0L) 
                function(r) statistic(data, f[r, ], ...)
            else function(r) statistic(data, f[r, ], pred.i[r, 
                ], ...)
        }
        else if (sum(m) > 0L) 
            function(r) statistic(data, i[r, ], pred.i[r, ], 
                ...)
        else if (simple) 
            function(r) statistic(data, index.array(n, 1, sim, 
                strata, m, L, weights), ...)
        else function(r) statistic(data, i[r, ], ...)
    }
    RR <- sum(R)
    res <- if (ncpus > 1L && (have_mc || have_snow)) {
        if (have_mc) {
            parallel::mclapply(seq_len(RR), fn, mc.cores = ncpus)
        }
        else if (have_snow) {
            list(...)
            if (is.null(cl)) {
                cl <- parallel::makePSOCKcluster(rep("localhost", 
                  ncpus))
                if (RNGkind()[1L] == "L'Ecuyer-CMRG") 
                  parallel::clusterSetRNGStream(cl)
                res <- parallel::parLapply(cl, seq_len(RR), fn)
                parallel::stopCluster(cl)
                res
            }
            else parallel::parLapply(cl, seq_len(RR), fn)
        }
    }
    else lapply(seq_len(RR), fn)
    t.star <- matrix(, RR, length(t0))
    for (r in seq_len(RR)) t.star[r, ] <- res[[r]]
    if (is.null(weights)) 
        weights <- 1/tabulate(strata)[strata]
    boot.return(sim, t0, t.star, temp.str, R, data, statistic, 
        stype, call, seed, L, m, pred.i, weights, ran.gen, mle)
}


control <- function (boot.out, L = NULL, distn = NULL, index = 1, t0 = NULL, 
    t = NULL, bias.adj = FALSE, alpha = NULL, ...) 
{
    if (!is.null(boot.out$call$weights)) 
        stop("control methods undefined when 'boot.out' has weights")
    if (is.null(alpha)) 
        alpha <- c(1, 2.5, 5, 10, 20, 50, 80, 90, 95, 97.5, 99)/100
    tL <- dL <- bias <- bias.L <- var.L <- NULL
    k3.L <- q.out <- distn.L <- NULL
    stat <- boot.out$statistic
    data <- boot.out$data
    R <- boot.out$R
    f <- boot.array(boot.out)
    if (bias.adj) {
        if (length(index) > 1L) {
            warning("only first element of 'index' used")
            index <- index[1L]
        }
        f.big <- apply(f, 2L, sum)
        if (boot.out$stype == "i") {
            n <- ncol(f)
            i.big <- rep(seq_len(n), f.big)
            t.big <- stat(data, i.big, ...)[index]
        }
        else if (boot.out$stype == "f") 
            t.big <- stat(data, f.big, ...)[index]
        else if (boot.out$stype == "w") 
            t.big <- stat(data, f.big/R, ...)[index]
        bias <- mean(boot.out$t[, index]) - t.big
        out <- bias
    }
    else {
        if (is.null(t) || is.null(t0)) {
            if (length(index) > 1L) {
                warning("only first element of 'index' used")
                index <- index[1L]
            }
            if (is.null(L)) 
                L <- empinf(boot.out, index = index, ...)
            tL <- linear.approx(boot.out, L, index, ...)
            t <- boot.out$t[, index]
            t0 <- boot.out$t0[index]
        }
        else {
            if (is.null(L)) 
                L <- empinf(boot.out, t = t, ...)
            tL <- linear.approx(boot.out, L, t0 = t0, ...)
        }
        fins <- seq_along(t)[is.finite(t)]
        t <- t[fins]
        tL <- tL[fins]
        R <- length(t)
        dL <- t - tL
        bias.L <- mean(dL)
        strata <- tapply(boot.out$strata, as.numeric(boot.out$strata))
        var.L <- var.linear(L, strata) + 2 * var(tL, dL) + var(dL)
        k3.L <- k3.linear(L, strata) + 3 * cum3(tL, dL) + 3 * 
            cum3(dL, tL) + cum3(dL)
        if (is.null(distn)) {
            distn <- saddle.distn((t0 + L)/length(L), alpha = (1L:R)/(R + 
                1), t0 = c(t0, sqrt(var.L)), strata = strata)
            dist.q <- distn$quantiles[, 2]
            distn <- distn$distn
        }
        else dist.q <- predict(distn, x = qnorm((1L:R)/(R + 1)))$y
        distn.L <- sort(dL[order(tL)] + dist.q)
        q.out <- distn.L[(R + 1) * alpha]
        out <- list(L = L, tL = tL, bias = bias.L, var = var.L, 
            k3 = k3.L, quantiles = cbind(alpha, q.out), distn = distn)
    }
    out
}


envelope <- function (boot.out = NULL, mat = NULL, level = 0.95, index = 1L:ncol(mat)) 
{
    emperr <- function(rmat, p = 0.05, k = NULL) {
        R <- nrow(rmat)
        if (is.null(k)) 
            k <- p * (R + 1)/2
        else p <- 2 * k/(R + 1)
        kf <- function(x, k, R) 1 * ((min(x) <= k) | (max(x) >= 
            R + 1L - k))
        c(k, p, sum(apply(rmat, 1L, kf, k, R))/(R + 1))
    }
    kfun <- function(x, k1, k2) sort(x, partial = sort(c(k1, 
        k2)))[c(k1, k2)]
    if (!is.null(boot.out) && isMatrix(boot.out$t)) 
        mat <- boot.out$t
    if (!isMatrix(mat)) 
        stop("bootstrap output matrix missing")
    if (length(index) < 2L) 
        stop("use 'boot.ci' for scalar parameters")
    mat <- mat[, index]
    rmat <- apply(mat, 2L, rank)
    R <- nrow(mat)
    if (length(level) == 1L) 
        level <- rep(level, 2L)
    k.pt <- floor((R + 1) * (1 - level[1L])/2 + 1e-10)
    k.pt <- c(k.pt, R + 1 - k.pt)
    err.pt <- emperr(rmat, k = k.pt[1L])
    ov <- emperr(rmat, k = 1)
    ee <- err.pt
    al <- 1 - level[2L]
    if (ov[3L] > al) 
        warning("unable to achieve requested overall error rate")
    else {
        continue <- !(ee[3L] < al)
        while (continue) {
            kk <- ov[1L] + round((ee[1L] - ov[1L]) * (al - ov[3L])/(ee[3L] - 
                ov[3L]))
            if (kk == ov[1L]) 
                kk <- kk + 1
            else if (kk == ee[1L]) 
                kk <- kk - 1
            temp <- emperr(rmat, k = kk)
            if (temp[3L] > al) 
                ee <- temp
            else ov <- temp
            continue <- !(ee[1L] == ov[1L] + 1)
        }
    }
    k.ov <- c(ov[1L], R + 1 - ov[1L])
    err.ov <- ov[-1L]
    out <- apply(mat, 2L, kfun, k.pt, k.ov)
    list(point = out[2:1, ], overall = out[4:3, ], k.pt = k.pt, 
        err.pt = err.pt[-1L], k.ov = k.ov, err.ov = err.ov, err.nom = 1 - 
            level)
}




## Package Data

acme <- boot::acme		## Monthly Excess Returns

aids <- boot::aids		## Delay in AIDS Reporting in England and Wales

aircondit <- boot::aircondit		## Failures of Air-conditioning Equipment

aircondit7 <- boot::aircondit7		## Failures of Air-conditioning Equipment

amis <- boot::amis		## Car Speeding and Warning Signs

aml <- boot::aml		## Remission Times for Acute Myelogenous Leukaemia

beaver <- boot::beaver		## Beaver Body Temperature Data

bigcity <- boot::bigcity		## Population of U.S. Cities

brambles <- boot::brambles		## Spatial Location of Bramble Canes

breslow <- boot::breslow		## Smoking Deaths Among Doctors

calcium <- boot::calcium		## Calcium Uptake Data

cane <- boot::cane		## Sugar-cane Disease Data

capability <- boot::capability		## Simulated Manufacturing Process Data

catsM <- boot::catsM		## Weight Data for Domestic Cats

cav <- boot::cav		## Position of Muscle Caveolae

cd4 <- boot::cd4		## CD4 Counts for HIV-Positive Patients

cd4.nested <- boot::cd4.nested		## Nested Bootstrap of cd4 data

channing <- boot::channing		## Channing House Data

city <- boot::city		## Population of U.S. Cities

claridge <- boot::claridge		## Genetic Links to Left-handedness

cloth <- boot::cloth		## Number of Flaws in Cloth

co.transfer <- boot::co.transfer		## Carbon Monoxide Transfer

coal <- boot::coal		## Dates of Coal Mining Disasters

darwin <- boot::darwin		## Darwin's Plant Height Differences

dogs <- boot::dogs		## Cardiac Data for Domestic Dogs

downs.bc <- boot::downs.bc		## Incidence of Down's Syndrome in British Columbia

ducks <- boot::ducks		## Behavioral and Plumage Characteristics of Hybrid Ducks

fir <- boot::fir		## Counts of Balsam-fir Seedlings

frets <- boot::frets		## Head Dimensions in Brothers

grav <- boot::grav		## Acceleration Due to Gravity

gravity <- boot::gravity		## Acceleration Due to Gravity

hirose <- boot::hirose		## Failure Time of PET Film

islay <- boot::islay		## Jura Quartzite Azimuths on Islay

manaus <- boot::manaus		## Average Heights of the Rio Negro river at Manaus

melanoma <- boot::melanoma		## Survival from Malignant Melanoma

motor <- boot::motor		## Data from a Simulated Motorcycle Accident

neuro <- boot::neuro		## Neurophysiological Point Process Data

nitrofen <- boot::nitrofen		## Toxicity of Nitrofen in Aquatic Systems

nodal <- boot::nodal		## Nodal Involvement in Prostate Cancer

nuclear <- boot::nuclear		## Nuclear Power Station Construction Data

paulsen <- boot::paulsen		## Neurotransmission in Guinea Pig Brains

poisons <- boot::poisons		## Animal Survival Times

polar <- boot::polar		## Pole Positions of New Caledonian Laterites

remission <- boot::remission		## Cancer Remission and Cell Activity

salinity <- boot::salinity		## Water Salinity and River Discharge

survival <- boot::survival		## Survival of Rats after Radiation Doses

tau <- boot::tau		## Tau Particle Decay Modes

tuna <- boot::tuna		## Tuna Sighting Data

urine <- boot::urine		## Urine Analysis Data

wool <- boot::wool		## Australian Relative Wool Prices



## Package Info

.skeleton_package_title = "Bootstrap Functions (Originally by Angelo Canty for S)"

.skeleton_package_version = "1.3-17"

.skeleton_package_depends = "graphics,stats"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF