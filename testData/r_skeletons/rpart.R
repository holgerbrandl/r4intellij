##
## Exported symobls in package `rpart`
##

## Exported package methods

xpred.rpart <- function (fit, xval = 10L, cp, return.all = FALSE) 
{
    if (!inherits(fit, "rpart")) 
        stop("Invalid fit object")
    method <- fit$method
    method.int <- pmatch(method, c("anova", "poisson", "class", 
        "user", "exp"))
    if (method.int == 5L) 
        method.int <- 2L
    Terms <- fit$terms
    Y <- fit$y
    X <- fit$x
    wt <- fit$wt
    numresp <- fit$numresp
    if (is.null(Y) || is.null(X)) {
        m <- fit$model
        if (is.null(m)) {
            m <- fit$call[match(c("", "formula", "data", "weights", 
                "subset", "na.action"), names(fit$call), 0L)]
            if (is.null(m$na.action)) 
                m$na.action <- na.rpart
            m[[1]] <- quote(stats::model.frame)
            m <- eval.parent(m)
        }
        if (is.null(X)) 
            X <- rpart.matrix(m)
        if (is.null(wt)) 
            wt <- model.extract(m, "weights")
        if (is.null(Y)) {
            yflag <- TRUE
            Y <- model.extract(m, "response")
            offset <- attr(Terms, "offset")
            if (method != "user") {
                init <- get(paste("rpart", method, sep = "."))(Y, 
                  offset, NULL)
                Y <- init$y
                numy <- if (is.matrix(Y)) 
                  ncol(Y)
                else 1L
            }
        }
        else {
            yflag <- FALSE
            numy <- if (is.matrix(Y)) 
                ncol(Y)
            else 1L
        }
    }
    else {
        yflag <- FALSE
        numy <- if (is.matrix(Y)) 
            ncol(Y)
        else 1L
        offset <- 0L
    }
    nobs <- nrow(X)
    nvar <- ncol(X)
    if (length(wt) == 0) 
        wt <- rep(1, nobs)
    cats <- rep(0, nvar)
    xlevels <- attr(fit, "xlevels")
    if (!is.null(xlevels)) 
        cats[match(names(xlevels), colnames(X))] <- unlist(lapply(xlevels, 
            length))
    controls <- fit$control
    if (missing(cp)) {
        cp <- fit$cptable[, 1L]
        cp <- sqrt(cp * c(10, cp[-length(cp)]))
        cp[1L] <- (1 + fit$cptable[1L, 1L])/2
    }
    if (length(xval) == 1L) {
        xgroups <- sample(rep(1L:xval, length = nobs), nobs, 
            replace = FALSE)
    }
    else if (length(xval) == nrow(X)) {
        xgroups <- xval
        xval <- length(unique(xgroups))
    }
    else {
        if (!is.null(fit$na.action)) {
            temp <- as.integer(fit$na.action)
            xval <- xval[-temp]
            if (length(xval) == nobs) {
                xgroups <- xval
                xval <- length(unique(xgroups))
            }
            else stop("Wrong length for 'xval'")
        }
        else stop("Wrong length for 'xval'")
    }
    costs <- fit$call$costs
    if (is.null(costs)) 
        costs <- rep(1, nvar)
    parms <- fit$parms
    if (method == "user") {
        mlist <- fit$functions
        if (yflag) {
            init <- if (length(parms) == 0L) 
                mlist$init(Y, offset, , wt)
            else mlist$init(Y, offset, parms, wt)
            Y <- init$Y
            numy <- init$numy
            parms <- init$parms
        }
        else {
            numy <- if (is.matrix(Y)) 
                ncol(Y)
            else 1L
            init <- list(numresp = numresp, numy = numy, parms = parms)
        }
        keep <- rpartcallback(mlist, nobs, init)
        method.int <- 4L
    }
    if (is.matrix(Y)) 
        Y <- as.double(t(Y))
    else storage.mode(Y) <- "double"
    storage.mode(X) <- "double"
    storage.mode(wt) <- "double"
    temp <- as.double(unlist(parms))
    if (length(temp) == 0L) 
        temp <- 0
    pred <- .Call(C_xpred, ncat = as.integer(cats * (!fit$ordered)), 
        method = as.integer(method.int), as.double(unlist(controls)), 
        temp, as.integer(xval), as.integer(xgroups), Y, X, wt, 
        as.integer(numy), as.double(costs), as.integer(return.all), 
        as.double(cp), as.double(fit$frame[1L, "dev"]), as.integer(numresp))
    if (return.all && numresp > 1L) {
        temp <- array(pred, dim = c(numresp, length(cp), nrow(X)), 
            dimnames = list(NULL, format(cp), rownames(X)))
        aperm(temp)
    }
    else matrix(pred, nrow = nrow(X), byrow = TRUE, dimnames = list(rownames(X), 
        format(cp)))
}


prune <- function (tree, ...) 
UseMethod("prune")


meanvar <- function (tree, ...) 
UseMethod("meanvar")


plotcp <- function (x, minline = TRUE, lty = 3, col = 1, upper = c("size", 
    "splits", "none"), ...) 
{
    dots <- list(...)
    if (!inherits(x, "rpart")) 
        stop("Not a legitimate \"rpart\" object")
    upper <- match.arg(upper)
    p.rpart <- x$cptable
    if (ncol(p.rpart) < 5L) 
        stop("'cptable' does not contain cross-validation results")
    xstd <- p.rpart[, 5L]
    xerror <- p.rpart[, 4L]
    nsplit <- p.rpart[, 2L]
    ns <- seq_along(nsplit)
    cp0 <- p.rpart[, 1L]
    cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
    if (!"ylim" %in% names(dots)) 
        dots$ylim <- c(min(xerror - xstd) - 0.1, max(xerror + 
            xstd) + 0.1)
    do.call(plot, c(list(ns, xerror, axes = FALSE, xlab = "cp", 
        ylab = "X-val Relative Error", type = "o"), dots))
    box()
    axis(2, ...)
    segments(ns, xerror - xstd, ns, xerror + xstd)
    axis(1L, at = ns, labels = as.character(signif(cp, 2L)), 
        ...)
    switch(upper, size = {
        axis(3L, at = ns, labels = as.character(nsplit + 1), 
            ...)
        mtext("size of tree", side = 3, line = 3)
    }, splits = {
        axis(3L, at = ns, labels = as.character(nsplit), ...)
        mtext("number of splits", side = 3, line = 3)
    })
    minpos <- min(seq_along(xerror)[xerror == min(xerror)])
    if (minline) 
        abline(h = (xerror + xstd)[minpos], lty = lty, col = col)
    invisible()
}


rsq.rpart <- function (x) 
{
    if (!inherits(x, "rpart")) 
        stop("Not a legitimate \"rpart\" object")
    p.rpart <- printcp(x)
    xstd <- p.rpart[, 5L]
    xerror <- p.rpart[, 4L]
    rel.error <- p.rpart[, 3L]
    nsplit <- p.rpart[, 2L]
    method <- x$method
    if (!method == "anova") 
        warning("may not be applicable for this method")
    plot(nsplit, 1 - rel.error, xlab = "Number of Splits", ylab = "R-square", 
        ylim = c(0, 1), type = "o")
    par(new = TRUE)
    plot(nsplit, 1 - xerror, type = "o", ylim = c(0, 1), lty = 2, 
        xlab = " ", ylab = " ")
    legend(0, 1, c("Apparent", "X Relative"), lty = 1:2)
    ylim <- c(min(xerror - xstd) - 0.1, max(xerror + xstd) + 
        0.1)
    plot(nsplit, xerror, xlab = "Number of Splits", ylab = "X Relative Error", 
        ylim = ylim, type = "o")
    segments(nsplit, xerror - xstd, nsplit, xerror + xstd)
    invisible()
}


post <- function (tree, ...) 
UseMethod("post")


prune.rpart <- function (tree, cp, ...) 
{
    ff <- tree$frame
    id <- as.integer(row.names(ff))
    toss <- id[ff$complexity <= cp & ff$var != "<leaf>"]
    if (length(toss) == 0L) 
        return(tree)
    newx <- snip.rpart(tree, toss)
    temp <- pmax(tree$cptable[, 1L], cp)
    keep <- match(unique(temp), temp)
    newx$cptable <- tree$cptable[keep, , drop = FALSE]
    newx$cptable[max(keep), 1L] <- cp
    newx$variable.importance <- importance(newx)
    newx
}


path.rpart <- function (tree, nodes, pretty = 0, print.it = TRUE) 
{
    if (!inherits(tree, "rpart")) 
        stop("Not a legitimate \"rpart\" object")
    splits <- labels.rpart(tree, pretty = pretty)
    frame <- tree$frame
    n <- row.names(frame)
    node <- as.numeric(n)
    which <- descendants(node)
    path <- list()
    if (missing(nodes)) {
        xy <- rpartco(tree)
        while (length(i <- identify(xy, n = 1L, plot = FALSE)) > 
            0L) {
            path[[n[i]]] <- path.i <- splits[which[, i]]
            if (print.it) {
                cat("\n", "node number:", n[i], "\n")
                cat(paste("  ", path.i), sep = "\n")
            }
        }
    }
    else {
        if (length(nodes <- node.match(nodes, node)) == 0L) 
            return(invisible())
        for (i in nodes) {
            path[[n[i]]] <- path.i <- splits[which[, i]]
            if (print.it) {
                cat("\n", "node number:", n[i], "\n")
                cat(paste("  ", path.i), sep = "\n")
            }
        }
    }
    invisible(path)
}


rpart <- function (formula, data, weights, subset, na.action = na.rpart, 
    method, model = FALSE, x = FALSE, y = TRUE, parms, control, 
    cost, ...) 
{
    Call <- match.call()
    if (is.data.frame(model)) {
        m <- model
        model <- FALSE
    }
    else {
        indx <- match(c("formula", "data", "weights", "subset"), 
            names(Call), nomatch = 0L)
        if (indx[1] == 0L) 
            stop("a 'formula' argument is required")
        temp <- Call[c(1L, indx)]
        temp$na.action <- na.action
        temp[[1L]] <- quote(stats::model.frame)
        m <- eval.parent(temp)
    }
    Terms <- attr(m, "terms")
    if (any(attr(Terms, "order") > 1L)) 
        stop("Trees cannot handle interaction terms")
    Y <- model.response(m)
    wt <- model.weights(m)
    if (any(wt < 0)) 
        stop("negative weights not allowed")
    if (!length(wt)) 
        wt <- rep(1, nrow(m))
    offset <- model.offset(m)
    X <- rpart.matrix(m)
    nobs <- nrow(X)
    nvar <- ncol(X)
    if (missing(method)) {
        method <- if (is.factor(Y) || is.character(Y)) 
            "class"
        else if (inherits(Y, "Surv")) 
            "exp"
        else if (is.matrix(Y)) 
            "poisson"
        else "anova"
    }
    if (is.list(method)) {
        mlist <- method
        method <- "user"
        init <- if (missing(parms)) 
            mlist$init(Y, offset, wt = wt)
        else mlist$init(Y, offset, parms, wt)
        keep <- rpartcallback(mlist, nobs, init)
        method.int <- 4L
        parms <- init$parms
    }
    else {
        method.int <- pmatch(method, c("anova", "poisson", "class", 
            "exp"))
        if (is.na(method.int)) 
            stop("Invalid method")
        method <- c("anova", "poisson", "class", "exp")[method.int]
        if (method.int == 4L) 
            method.int <- 2L
        init <- if (missing(parms)) 
            get(paste("rpart", method, sep = "."), envir = environment())(Y, 
                offset, , wt)
        else get(paste("rpart", method, sep = "."), envir = environment())(Y, 
            offset, parms, wt)
        ns <- asNamespace("rpart")
        if (!is.null(init$print)) 
            environment(init$print) <- ns
        if (!is.null(init$summary)) 
            environment(init$summary) <- ns
        if (!is.null(init$text)) 
            environment(init$text) <- ns
    }
    Y <- init$y
    xlevels <- .getXlevels(Terms, m)
    cats <- rep(0L, ncol(X))
    if (!is.null(xlevels)) 
        cats[match(names(xlevels), colnames(X))] <- unlist(lapply(xlevels, 
            length))
    extraArgs <- list(...)
    if (length(extraArgs)) {
        controlargs <- names(formals(rpart.control))
        indx <- match(names(extraArgs), controlargs, nomatch = 0L)
        if (any(indx == 0L)) 
            stop(gettextf("Argument %s not matched", names(extraArgs)[indx == 
                0L]), domain = NA)
    }
    controls <- rpart.control(...)
    if (!missing(control)) 
        controls[names(control)] <- control
    xval <- controls$xval
    if (is.null(xval) || (length(xval) == 1L && xval == 0L) || 
        method == "user") {
        xgroups <- 0L
        xval <- 0L
    }
    else if (length(xval) == 1L) {
        xgroups <- sample(rep(1L:xval, length = nobs), nobs, 
            replace = FALSE)
    }
    else if (length(xval) == nobs) {
        xgroups <- xval
        xval <- length(unique(xgroups))
    }
    else {
        if (!is.null(attr(m, "na.action"))) {
            temp <- as.integer(attr(m, "na.action"))
            xval <- xval[-temp]
            if (length(xval) == nobs) {
                xgroups <- xval
                xval <- length(unique(xgroups))
            }
            else stop("Wrong length for 'xval'")
        }
        else stop("Wrong length for 'xval'")
    }
    if (missing(cost)) 
        cost <- rep(1, nvar)
    else {
        if (length(cost) != nvar) 
            stop("Cost vector is the wrong length")
        if (any(cost <= 0)) 
            stop("Cost vector must be positive")
    }
    tfun <- function(x) if (is.matrix(x)) 
        rep(is.ordered(x), ncol(x))
    else is.ordered(x)
    labs <- sub("^`(.*)`$", "\\1", attr(Terms, "term.labels"))
    isord <- unlist(lapply(m[labs], tfun))
    storage.mode(X) <- "double"
    storage.mode(wt) <- "double"
    temp <- as.double(unlist(init$parms))
    if (!length(temp)) 
        temp <- 0
    rpfit <- .Call(C_rpart, ncat = as.integer(cats * (!isord)), 
        method = as.integer(method.int), as.double(unlist(controls)), 
        temp, as.integer(xval), as.integer(xgroups), as.double(t(init$y)), 
        X, wt, as.integer(init$numy), as.double(cost))
    nsplit <- nrow(rpfit$isplit)
    ncat <- if (!is.null(rpfit$csplit)) 
        nrow(rpfit$csplit)
    else 0L
    if (nsplit == 0L) 
        xval <- 0L
    numcp <- ncol(rpfit$cptable)
    temp <- if (nrow(rpfit$cptable) == 3L) 
        c("CP", "nsplit", "rel error")
    else c("CP", "nsplit", "rel error", "xerror", "xstd")
    dimnames(rpfit$cptable) <- list(temp, 1L:numcp)
    tname <- c("<leaf>", colnames(X))
    splits <- matrix(c(rpfit$isplit[, 2:3], rpfit$dsplit), ncol = 5L, 
        dimnames = list(tname[rpfit$isplit[, 1L] + 1L], c("count", 
            "ncat", "improve", "index", "adj")))
    index <- rpfit$inode[, 2L]
    nadd <- sum(isord[rpfit$isplit[, 1L]])
    if (nadd > 0L) {
        newc <- matrix(0L, nadd, max(cats))
        cvar <- rpfit$isplit[, 1L]
        indx <- isord[cvar]
        cdir <- splits[indx, 2L]
        ccut <- floor(splits[indx, 4L])
        splits[indx, 2L] <- cats[cvar[indx]]
        splits[indx, 4L] <- ncat + 1L:nadd
        for (i in 1L:nadd) {
            newc[i, 1L:(cats[(cvar[indx])[i]])] <- -as.integer(cdir[i])
            newc[i, 1L:ccut[i]] <- as.integer(cdir[i])
        }
        catmat <- if (ncat == 0L) 
            newc
        else {
            cs <- rpfit$csplit
            ncs <- ncol(cs)
            ncc <- ncol(newc)
            if (ncs < ncc) 
                cs <- cbind(cs, matrix(0L, nrow(cs), ncc - ncs))
            rbind(cs, newc)
        }
        ncat <- ncat + nadd
    }
    else catmat <- rpfit$csplit
    if (nsplit == 0L) {
        frame <- data.frame(row.names = 1L, var = "<leaf>", n = rpfit$inode[, 
            5L], wt = rpfit$dnode[, 3L], dev = rpfit$dnode[, 
            1L], yval = rpfit$dnode[, 4L], complexity = rpfit$dnode[, 
            2L], ncompete = 0L, nsurrogate = 0L)
    }
    else {
        temp <- ifelse(index == 0L, 1L, index)
        svar <- ifelse(index == 0L, 0L, rpfit$isplit[temp, 1L])
        frame <- data.frame(row.names = rpfit$inode[, 1L], var = tname[svar + 
            1L], n = rpfit$inode[, 5L], wt = rpfit$dnode[, 3L], 
            dev = rpfit$dnode[, 1L], yval = rpfit$dnode[, 4L], 
            complexity = rpfit$dnode[, 2L], ncompete = pmax(0L, 
                rpfit$inode[, 3L] - 1L), nsurrogate = rpfit$inode[, 
                4L])
    }
    if (method.int == 3L) {
        numclass <- init$numresp - 2L
        nodeprob <- rpfit$dnode[, numclass + 5L]/sum(wt)
        temp <- pmax(1L, init$counts)
        temp <- rpfit$dnode[, 4L + (1L:numclass)] %*% diag(init$parms$prior/temp)
        yprob <- temp/rowSums(temp)
        yval2 <- matrix(rpfit$dnode[, 4L + (0L:numclass)], ncol = numclass + 
            1L)
        frame$yval2 <- cbind(yval2, yprob, nodeprob)
    }
    else if (init$numresp > 1L) 
        frame$yval2 <- rpfit$dnode[, -(1L:3L), drop = FALSE]
    if (is.null(init$summary)) 
        stop("Initialization routine is missing the 'summary' function")
    functions <- if (is.null(init$print)) 
        list(summary = init$summary)
    else list(summary = init$summary, print = init$print)
    if (!is.null(init$text)) 
        functions <- c(functions, list(text = init$text))
    if (method == "user") 
        functions <- c(functions, mlist)
    where <- rpfit$which
    names(where) <- row.names(m)
    ans <- list(frame = frame, where = where, call = Call, terms = Terms, 
        cptable = t(rpfit$cptable), method = method, parms = init$parms, 
        control = controls, functions = functions, numresp = init$numresp)
    if (nsplit) 
        ans$splits = splits
    if (ncat > 0L) 
        ans$csplit <- catmat + 2L
    if (nsplit) 
        ans$variable.importance <- importance(ans)
    if (model) {
        ans$model <- m
        if (missing(y)) 
            y <- FALSE
    }
    if (y) 
        ans$y <- Y
    if (x) {
        ans$x <- X
        ans$wt <- wt
    }
    ans$ordered <- isord
    if (!is.null(attr(m, "na.action"))) 
        ans$na.action <- attr(m, "na.action")
    if (!is.null(xlevels)) 
        attr(ans, "xlevels") <- xlevels
    if (method == "class") 
        attr(ans, "ylevels") <- init$ylevels
    class(ans) <- "rpart"
    ans
}


rpart.exp <- function (y, offset, parms, wt) 
{
    if (!inherits(y, "Surv")) 
        stop("Response must be a 'survival' object - use the 'Surv()' function")
    ny <- ncol(y)
    n <- nrow(y)
    status <- y[, ny]
    if (any(y[, 1L] <= 0)) 
        stop("Observation time must be > 0")
    if (all(status == 0)) 
        stop("No deaths in data set")
    time <- y[, ny - 1L]
    dtimes <- sort(unique(time[status == 1]))
    temp <- .Call(C_rpartexp2, as.double(dtimes), as.double(.Machine$double.eps))
    dtimes <- dtimes[temp == 1]
    if (length(dtimes) > 1000) 
        dtimes <- quantile(dtimes, 0:1000/1000)
    itable <- c(0, dtimes[-length(dtimes)], max(time))
    drate2 <- function(n, ny, y, wt, itable) {
        time <- y[, ny - 1L]
        status <- y[, ny]
        ilength <- diff(itable)
        ngrp <- length(ilength)
        index <- unclass(cut(time, itable, include.lowest = TRUE))
        itime <- time - itable[index]
        if (ny == 3L) {
            stime <- y[, 1L]
            index2 <- unclass(cut(stime, itable, include.lowest = TRUE))
            itime2 <- stime - itable[index2]
        }
        tab1 <- table(index)
        temp <- rev(cumsum(rev(tab1)))
        pyears <- ilength * c(temp[-1L], 0) + tapply(itime, index, 
            sum)
        if (ny == 3L) {
            tab2 <- table(index2, levels = 1:ngrp)
            temp <- rev(cumsum(rev(tab2)))
            py2 <- ilength * c(0, temp[-ngrp]) + tapply(itime2, 
                index2, sum)
            pyears <- pyears - py2
        }
        deaths <- tapply(status, index, sum)
        rate <- deaths/pyears
        rate
    }
    rate <- drate2(n, ny, y, wt, itable)
    cumhaz <- cumsum(c(0, rate * diff(itable)))
    newy <- approx(itable, cumhaz, time)$y
    if (ny == 3L) 
        newy <- newy - approx(itable, cumhaz, y[, 1L])$y
    if (length(offset) == n) 
        newy <- newy * exp(offset)
    if (missing(parms)) 
        parms <- c(shrink = 1L, method = 1L)
    else {
        parms <- as.list(parms)
        if (is.null(names(parms))) 
            stop("You must input a named list for parms")
        parmsNames <- c("method", "shrink")
        indx <- pmatch(names(parms), parmsNames, 0L)
        if (any(indx == 0L)) 
            stop(gettextf("'parms' component not matched: %s", 
                names(parms)[indx == 0L]), domain = NA)
        else names(parms) <- parmsNames[indx]
        if (is.null(parms$method)) 
            method <- 1L
        else method <- pmatch(parms$method, c("deviance", "sqrt"))
        if (is.na(method)) 
            stop("Invalid error method for Poisson")
        if (is.null(parms$shrink)) 
            shrink <- 2L - method
        else shrink <- parms$shrink
        if (!is.numeric(shrink) || shrink < 0L) 
            stop("Invalid shrinkage value")
        parms <- c(shrink = shrink, method = method)
    }
    list(y = cbind(newy, y[, 2L]), parms = parms, numresp = 2L, 
        numy = 2L, summary = function(yval, dev, wt, ylevel, 
            digits) {
            paste0("  events=", formatg(yval[, 2L]), ",  estimated rate=", 
                formatg(yval[, 1L], digits), " , mean deviance=", 
                formatg(dev/wt, digits))
        }, text = function(yval, dev, wt, ylevel, digits, n, 
            use.n) {
            if (use.n) paste0(formatg(yval[, 1L], digits), "\n", 
                formatg(yval[, 2L]), "/", n) else paste(formatg(yval[, 
                1L], digits))
        })
}


rpart.control <- function (minsplit = 20L, minbucket = round(minsplit/3), cp = 0.01, 
    maxcompete = 4L, maxsurrogate = 5L, usesurrogate = 2L, xval = 10L, 
    surrogatestyle = 0L, maxdepth = 30L, ...) 
{
    if (maxcompete < 0L) {
        warning("The value of 'maxcompete' supplied is < 0; the value 0 was used instead")
        maxcompete <- 0L
    }
    if (any(xval < 0L)) {
        warning("The value of 'xval' supplied is < 0; the value 0 was used instead")
        xval <- 0L
    }
    if (maxdepth > 30L) 
        stop("Maximum depth is 30")
    if (maxdepth < 1L) 
        stop("Maximum depth must be at least 1")
    if (missing(minsplit) && !missing(minbucket)) 
        minsplit <- minbucket * 3L
    if ((usesurrogate < 0L) || (usesurrogate > 2L)) {
        warning("The value of 'usesurrogate' supplied was out of range, the default value of 2 is used instead.")
        usesurrogate <- 2L
    }
    if ((surrogatestyle < 0L) || (surrogatestyle > 1L)) {
        warning("The value of 'surrogatestyle' supplied was out of range, the default value of 0 is used instead.")
        surrogatestyle <- 0L
    }
    list(minsplit = minsplit, minbucket = minbucket, cp = cp, 
        maxcompete = maxcompete, maxsurrogate = maxsurrogate, 
        usesurrogate = usesurrogate, surrogatestyle = surrogatestyle, 
        maxdepth = maxdepth, xval = xval)
}


printcp <- function (x, digits = getOption("digits") - 2L) 
{
    if (!inherits(x, "rpart")) 
        stop("'x' must be an \"rpart\" object")
    cat(switch(x$method, anova = "\nRegression tree:\n", class = "\nClassification tree:\n", 
        poisson = "\nRates regression tree:\n", exp = "\nSurvival regression tree:\n"))
    if (!is.null(cl <- x$call)) {
        dput(cl, control = NULL)
        cat("\n")
    }
    frame <- x$frame
    leaves <- frame$var == "<leaf>"
    used <- unique(frame$var[!leaves])
    if (!is.null(used)) {
        cat("Variables actually used in tree construction:\n")
        print(sort(as.character(used)), quote = FALSE)
        cat("\n")
    }
    cat("Root node error: ", format(frame$dev[1L], digits = digits), 
        "/", frame$n[1L], " = ", format(frame$dev[1L]/frame$n[1L], 
            digits = digits), "\n\n", sep = "")
    n <- x$frame$n
    omit <- x$na.action
    if (length(omit)) 
        cat("n=", n[1L], " (", naprint(omit), ")\n\n", sep = "")
    else cat("n=", n[1L], "\n\n")
    print(x$cptable, digits = digits)
    invisible(x$cptable)
}


na.rpart <- function (x) 
{
    Terms <- attr(x, "terms")
    if (!is.null(Terms)) 
        yvar <- attr(Terms, "response")
    else yvar <- 0L
    if (yvar == 0L) {
        xmiss <- is.na(x)
        keep <- (xmiss %*% rep(1, ncol(xmiss))) < ncol(xmiss)
    }
    else {
        xmiss <- is.na(x[-yvar])
        ymiss <- is.na(x[[yvar]])
        keep <- if (is.matrix(ymiss)) 
            ((xmiss %*% rep(1, ncol(xmiss))) < ncol(xmiss)) & 
                ((ymiss %*% rep(1, ncol(ymiss))) == 0)
        else ((xmiss %*% rep(1, ncol(xmiss))) < ncol(xmiss)) & 
            !ymiss
    }
    if (all(keep)) 
        x
    else {
        temp <- seq(keep)[!keep]
        names(temp) <- row.names(x)[!keep]
        class(temp) <- c("na.rpart", "omit")
        structure(x[keep, , drop = FALSE], na.action = temp)
    }
}


snip.rpart <- function (x, toss) 
{
    if (!inherits(x, "rpart")) 
        stop("Not an \"rpart\" object")
    if (missing(toss) || length(toss) == 0L) {
        toss <- snip.rpart.mouse(x)
        if (length(toss) == 0L) 
            return(x)
    }
    ff <- x$frame
    id <- as.integer(row.names(ff))
    ff.n <- length(id)
    toss <- unique(toss)
    toss.idx <- match(toss, id, 0L)
    if (any(toss.idx == 0L)) {
        warning(gettext("Nodes %s are not in this tree", toss[toss.idx == 
            0L]), domain = NA)
        toss <- toss[toss.idx > 0L]
        toss.idx <- toss.idx[toss.idx > 0L]
    }
    id2 <- id
    while (any(id2 > 1L)) {
        id2 <- id2%/%2L
        xx <- (match(id2, toss, 0L) > 0L)
        toss <- c(toss, id[xx])
        id2[xx] <- 0L
    }
    temp <- match(toss%/%2L, toss, 0L)
    newleaf <- match(toss[temp == 0L], id)
    keepit <- (1:ff.n)[is.na(match(id, toss))]
    n.split <- rep(1L:ff.n, ff$ncompete + ff$nsurrogate + (ff$var != 
        "<leaf>"))
    split <- x$splits[match(n.split, keepit, 0L) > 0L, , drop = FALSE]
    temp <- split[, 2L] > 1L
    if (any(temp)) {
        x$csplit <- x$csplit[split[temp, 4L], , drop = FALSE]
        split[temp, 4L] <- 1L
        if (is.matrix(x$csplit)) 
            split[temp, 4L] <- 1L:nrow(x$csplit)
    }
    else x$csplit <- NULL
    x$splits <- split
    ff$ncompete[newleaf] <- ff$nsurrogate[newleaf] <- 0L
    ff$var[newleaf] <- "<leaf>"
    x$frame <- ff[sort(c(keepit, newleaf)), ]
    id2 <- id[x$where]
    id3 <- id[sort(c(keepit, newleaf))]
    temp <- match(id2, id3, 0L)
    while (any(temp == 0L)) {
        id2[temp == 0L] <- id2[temp == 0L]%/%2L
        temp <- match(id2, id3, 0L)
    }
    x$where <- match(id2, id3)
    x
}




## Package Data

car.test.frame <- rpart::car.test.frame		## Automobile Data from 'Consumer Reports' 1990

car90 <- rpart::car90		## Automobile Data from 'Consumer Reports' 1990

cu.summary <- rpart::cu.summary		## Automobile Data from 'Consumer Reports' 1990

kyphosis <- rpart::kyphosis		## Data on Children who have had Corrective Spinal Surgery

solder <- rpart::solder		## Soldering of Components on Printed-Circuit Boards

stagec <- rpart::stagec		## Stage C Prostate Cancer



## Package Info

.skeleton_package_title = "Recursive Partitioning and Regression Trees"

.skeleton_package_version = "4.1-10"

.skeleton_package_depends = "graphics,stats,grDevices"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF