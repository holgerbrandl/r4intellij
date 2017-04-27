##
## Exported symobls in package `class`
##

## Exported package methods

multiedit <- function (x, class, k = 1, V = 3, I = 5, trace = TRUE) 
{
    n1 <- length(class)
    class <- unclass(class)
    index <- seq_len(n1)
    pass <- lpass <- 0L
    repeat {
        if (n1 < 5 * V) {
            warning("retained set is now too small to proceed")
            break
        }
        pass <- pass + 1L
        sub <- sample(V, length(class), replace = TRUE)
        keep <- logical(length(class))
        for (i in 1L:V) {
            train <- sub == i
            test <- sub == (1 + i%%V)
            keep[test] <- (knn(x[train, , drop = FALSE], x[test, 
                , drop = FALSE], class[train], k) == class[test])
        }
        x <- x[keep, , drop = FALSE]
        class <- class[keep]
        index <- index[keep]
        n2 <- length(class)
        if (n2 < n1) 
            lpass <- pass
        if (lpass <= pass - I) 
            break
        n1 <- n2
        if (trace) 
            message(gettextf("pass %s size %d", pass, n2), domain = NA)
    }
    index
}


knn.cv <- function (train, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE) 
{
    train <- as.matrix(train)
    if (any(is.na(train)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    p <- ncol(train)
    ntr <- nrow(train)
    if (length(cl) != ntr) 
        stop("'train' and 'class' have different lengths")
    if (ntr - 1 < k) {
        warning(gettextf("k = %d exceeds number %d of patterns", 
            k, ntr - 1), domain = NA)
        k <- ntr - 1
    }
    if (k < 1) 
        stop(gettextf("k = %d must be at least 1", k), domain = NA)
    clf <- as.factor(cl)
    nc <- max(unclass(clf))
    Z <- .C(VR_knn, as.integer(k), as.integer(l), as.integer(ntr), 
        as.integer(ntr), as.integer(p), as.double(train), as.integer(unclass(clf)), 
        as.double(train), res = integer(ntr), pr = double(ntr), 
        integer(nc + 1), as.integer(nc), as.integer(TRUE), as.integer(use.all))
    res <- factor(Z$res, levels = seq_along(levels(clf)), labels = levels(clf))
    if (prob) 
        attr(res, "prob") <- Z$pr
    res
}


knn <- function (train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE) 
{
    train <- as.matrix(train)
    if (is.null(dim(test))) 
        dim(test) <- c(1, length(test))
    test <- as.matrix(test)
    if (any(is.na(train)) || any(is.na(test)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    p <- ncol(train)
    ntr <- nrow(train)
    if (length(cl) != ntr) 
        stop("'train' and 'class' have different lengths")
    if (ntr < k) {
        warning(gettextf("k = %d exceeds number %d of patterns", 
            k, ntr), domain = NA)
        k <- ntr
    }
    if (k < 1) 
        stop(gettextf("k = %d must be at least 1", k), domain = NA)
    nte <- nrow(test)
    if (ncol(test) != p) 
        stop("dims of 'test' and 'train' differ")
    clf <- as.factor(cl)
    nc <- max(unclass(clf))
    Z <- .C(VR_knn, as.integer(k), as.integer(l), as.integer(ntr), 
        as.integer(nte), as.integer(p), as.double(train), as.integer(unclass(clf)), 
        as.double(test), res = integer(nte), pr = double(nte), 
        integer(nc + 1), as.integer(nc), as.integer(FALSE), as.integer(use.all))
    res <- factor(Z$res, levels = seq_along(levels(clf)), labels = levels(clf))
    if (prob) 
        attr(res, "prob") <- Z$pr
    res
}


condense <- function (train, class, store = sample(seq(n), 1), trace = TRUE) 
{
    n <- length(class)
    bag <- rep(TRUE, n)
    bag[store] <- FALSE
    repeat {
        if (trace) 
            print(seq(n)[!bag])
        if (sum(bag) == 0) 
            break
        res <- knn1(train[!bag, , drop = FALSE], train[bag, , 
            drop = FALSE], class[!bag])
        add <- res != class[bag]
        if (sum(add) == 0) 
            break
        cand <- (seq(n)[bag])[add]
        if (length(cand) > 1L) 
            cand <- sample(cand, 1L)
        bag[cand] <- FALSE
    }
    seq(n)[!bag]
}


knn1 <- function (train, test, cl) 
{
    train <- as.matrix(train)
    if (is.null(dim(test))) 
        dim(test) <- c(1, length(test))
    test <- as.matrix(test)
    if (any(is.na(train)) || any(is.na(test)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    p <- ncol(train)
    ntr <- nrow(train)
    if (length(cl) != ntr) 
        stop("'train' and 'class' have different lengths")
    nte <- nrow(test)
    if (ncol(test) != p) 
        stop("dims of 'test' and 'train' differ")
    clf <- as.factor(cl)
    nc <- max(unclass(clf))
    res <- .C(VR_knn1, as.integer(ntr), as.integer(nte), as.integer(p), 
        as.double(train), as.integer(unclass(clf)), as.double(test), 
        res = integer(nte), integer(nc + 1), as.integer(nc), 
        d = double(nte))$res
    factor(res, levels = seq_along(levels(clf)), labels = levels(clf))
}


lvqinit <- function (x, cl, size, prior, k = 5) 
{
    x <- as.matrix(x)
    n <- nrow(x)
    p <- ncol(x)
    if (length(cl) != n) 
        stop("'x' and 'cl' have different lengths")
    g <- as.factor(cl)
    if (any(is.na(x)) || any(is.na(g))) 
        stop("no missing values are allowed")
    counts <- tapply(rep(1, length(g)), g, sum)
    prop <- counts/n
    np <- length(prop)
    if (missing(prior)) 
        prior <- prop
    else if (any(prior < 0) || round(sum(prior), 5) != 1) 
        stop("invalid 'prior'")
    if (length(prior) != np) 
        stop("'prior' is of incorrect length")
    if (missing(size)) 
        size <- min(round(0.4 * np * (np - 1 + p/2), 0), n)
    inside <- knn.cv(x, cl, k) == cl
    selected <- numeric(0)
    for (i in 1L:np) {
        set <- seq_along(g)[unclass(g) == i & inside]
        if (length(set) > 1L) 
            set <- sample(set, min(length(set), round(size * 
                prior[i])))
        selected <- c(selected, set)
    }
    list(x = x[selected, , drop = FALSE], cl = cl[selected])
}


olvq1 <- function (x, cl, codebk, niter = 40 * nrow(codebk$x), alpha = 0.3) 
{
    x <- as.matrix(x)
    if (any(is.na(x)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    n <- nrow(x)
    p <- ncol(x)
    nc <- dim(codebk$x)[1L]
    if (length(cl) != n) 
        stop("'x' and 'cl' have different lengths")
    iters <- sample(n, niter, TRUE)
    z <- .C(VR_olvq, as.double(alpha), as.integer(n), as.integer(p), 
        as.double(x), as.integer(unclass(cl)), as.integer(nc), 
        xc = as.double(codebk$x), as.integer(codebk$cl), as.integer(niter), 
        as.integer(iters - 1L))
    xc <- matrix(z$xc, nc, p)
    dimnames(xc) <- dimnames(codebk$x)
    list(x = xc, cl = codebk$cl)
}


batchSOM <- function (data, grid = somgrid(), radii, init) 
{
    data <- as.matrix(data)
    nd <- nrow(data)
    ng <- nrow(grid$pts)
    if (missing(init)) 
        init <- data[sample(1L:nd, ng, replace = FALSE), , drop = FALSE]
    nhbrdist <- as.matrix(dist(grid$pts))
    for (r in radii) {
        cl <- as.numeric(knn1(init, data, 1L:ng))
        A <- (nhbrdist <= r)[, cl]
        ind <- rowSums(A) > 0
        init[ind, ] <- A[ind, ] %*% data/rowSums(A)[ind]
    }
    structure(list(grid = grid, codes = init), class = "SOM")
}


lvq1 <- function (x, cl, codebk, niter = 100 * nrow(codebk$x), alpha = 0.03) 
{
    x <- as.matrix(x)
    if (any(is.na(x)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    n <- nrow(x)
    p <- ncol(x)
    nc <- dim(codebk$x)[1L]
    if (length(cl) != n) 
        stop("'x' and 'cl' have different lengths")
    iters <- sample(n, niter, TRUE)
    z <- .C(VR_lvq1, as.double(alpha), as.integer(n), as.integer(p), 
        as.double(x), as.integer(unclass(cl)), as.integer(nc), 
        xc = as.double(codebk$x), as.integer(codebk$cl), as.integer(niter), 
        as.integer(iters - 1L))
    xc <- matrix(z$xc, nc, p)
    dimnames(xc) <- dimnames(codebk$x)
    list(x = xc, cl = codebk$cl)
}


somgrid <- function (xdim = 8, ydim = 6, topo = c("rectangular", "hexagonal")) 
{
    topo <- match.arg(topo)
    x <- 1L:xdim
    y <- 1L:ydim
    pts <- as.matrix(expand.grid(x = x, y = y))
    if (topo == "hexagonal") {
        pts[, 1L] <- pts[, 1L] + 0.5 * (pts[, 2L]%%2)
        pts[, 2L] <- sqrt(3)/2 * pts[, 2L]
    }
    res <- list(pts = pts, xdim = xdim, ydim = ydim, topo = topo)
    class(res) <- "somgrid"
    res
}


lvq2 <- function (x, cl, codebk, niter = 100 * nrow(codebk$x), alpha = 0.03, 
    win = 0.3) 
{
    x <- as.matrix(x)
    if (any(is.na(x)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    n <- nrow(x)
    p <- ncol(x)
    nc <- dim(codebk$x)[1L]
    if (length(cl) != n) 
        stop("'x' and 'cl' have different lengths")
    iters <- sample(n, niter, TRUE)
    z <- .C(VR_lvq2, as.double(alpha), as.double(win), as.integer(n), 
        as.integer(p), as.double(x), as.integer(unclass(cl)), 
        as.integer(nc), xc = as.double(codebk$x), as.integer(codebk$cl), 
        as.integer(niter), as.integer(iters - 1L))
    xc <- matrix(z$xc, nc, p)
    dimnames(xc) <- dimnames(codebk$x)
    list(x = xc, cl = codebk$cl)
}


reduce.nn <- function (train, ind, class) 
{
    n <- length(class)
    rest <- seq(n)[-ind]
    for (i in sample(ind)) {
        res <- knn1(train[-c(rest, i), , drop = FALSE], train[c(rest, 
            i), , drop = FALSE], class[-c(rest, i)])
        if (all(res == class[c(rest, i)])) 
            rest <- c(rest, i)
    }
    seq(n)[-rest]
}


lvq3 <- function (x, cl, codebk, niter = 100 * nrow(codebk$x), alpha = 0.03, 
    win = 0.3, epsilon = 0.1) 
{
    x <- as.matrix(x)
    if (any(is.na(x)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    n <- nrow(x)
    p <- ncol(x)
    nc <- dim(codebk$x)[1L]
    if (length(cl) != n) 
        stop("'x' and 'cl' have different lengths")
    iters <- sample(n, niter, TRUE)
    z <- .C(VR_lvq3, as.double(alpha), as.double(win), as.double(epsilon), 
        as.integer(n), as.integer(p), as.double(x), as.integer(unclass(cl)), 
        as.integer(nc), xc = as.double(codebk$x), as.integer(codebk$cl), 
        as.integer(niter), as.integer(iters - 1L))
    xc <- matrix(z$xc, nc, p)
    dimnames(xc) <- dimnames(codebk$x)
    list(x = xc, cl = codebk$cl)
}


SOM <- function (data, grid = somgrid(), rlen = 10000, alpha = seq(0.05, 
    0, len = rlen), radii = seq(4, 1, len = rlen), init) 
{
    data <- as.matrix(data)
    nd <- nrow(data)
    if (!nd) 
        stop("'SOM' called with no data")
    ng <- nrow(grid$pts)
    nphases <- 1L
    if (is.list(alpha)) {
        nphases <- length(alpha)
        if (!is.list(radii) || length(radii) != nphases) 
            stop("'radii' must be a list of the same length as 'alpha'")
    }
    if (missing(init)) 
        init <- data[sample(1L:nd, ng, replace = FALSE), , drop = FALSE]
    codes <- init
    nhbrdist <- as.matrix(dist(grid$pts))
    if (nphases == 1L) {
        rlen <- length(alpha)
        if (length(radii) != rlen) 
            stop("'alpha' and 'radii' do not have the same lengths")
        codes <- .C(VR_onlineSOM, data = as.double(data), codes = as.double(codes), 
            nhbrdist = as.double(nhbrdist), alpha = as.double(alpha), 
            radii = as.double(radii), n = as.integer(nrow(data)), 
            p = as.integer(ncol(data)), ncodes = as.integer(nrow(init)), 
            rlen = as.integer(rlen))$codes
    }
    else {
        for (k in 1L:nphases) {
            rlen <- length(alpha[[k]])
            if (length(radii[[k]]) != rlen) 
                stop("'alpha' and 'radii' do not match")
            codes <- .C(VR_onlineSOM, data = as.double(data), 
                codes = as.double(codes), nhbrdist = as.double(nhbrdist), 
                alpha = as.double(alpha[[k]]), radii = as.double(radii[[k]]), 
                n = as.integer(nrow(data)), p = as.integer(ncol(data)), 
                ncodes = as.integer(nrow(init)), rlen = as.integer(rlen))$codes
        }
    }
    dim(codes) <- dim(init)
    colnames(codes) <- colnames(init)
    structure(list(grid = grid, codes = codes), class = "SOM")
}


lvqtest <- function (codebk, test) 
knn1(codebk$x, test, codebk$cl)




## Package Data

# none


## Package Info

.skeleton_package_title = "Functions for Classification"

.skeleton_package_version = "7.3-14"

.skeleton_package_depends = "stats,utils"

.skeleton_package_imports = "MASS"


## Internal

.skeleton_version = 5


## EOF