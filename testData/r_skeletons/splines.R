##
## Exported symobls in package `splines`
##

## Exported package methods

bs <- function (x, df = NULL, knots = NULL, degree = 3, intercept = FALSE, 
    Boundary.knots = range(x)) 
{
    ord <- 1L + (degree <- as.integer(degree))
    if (ord <= 1) 
        stop("'degree' must be integer >= 1")
    nx <- names(x)
    x <- as.vector(x)
    nax <- is.na(x)
    if (nas <- any(nax)) 
        x <- x[!nax]
    outside <- if (!missing(Boundary.knots)) {
        Boundary.knots <- sort(Boundary.knots)
        (ol <- x < Boundary.knots[1L]) | (or <- x > Boundary.knots[2L])
    }
    else FALSE
    if (!is.null(df) && is.null(knots)) {
        nIknots <- df - ord + (1L - intercept)
        if (nIknots < 0L) {
            nIknots <- 0L
            warning(gettextf("'df' was too small; have used %d", 
                ord - (1L - intercept)), domain = NA)
        }
        knots <- if (nIknots > 0L) {
            knots <- seq.int(from = 0, to = 1, length.out = nIknots + 
                2L)[-c(1L, nIknots + 2L)]
            quantile(x[!outside], knots)
        }
    }
    Aknots <- sort(c(rep(Boundary.knots, ord), knots))
    if (any(outside)) {
        warning("some 'x' values beyond boundary knots may cause ill-conditioned bases")
        derivs <- 0:degree
        scalef <- gamma(1L:ord)
        basis <- array(0, c(length(x), length(Aknots) - degree - 
            1L))
        e <- 1/4
        if (any(ol)) {
            k.pivot <- (1 - e) * Boundary.knots[1L] + e * Aknots[ord + 
                1]
            xl <- cbind(1, outer(x[ol] - k.pivot, 1L:degree, 
                "^"))
            tt <- splineDesign(Aknots, rep(k.pivot, ord), ord, 
                derivs)
            basis[ol, ] <- xl %*% (tt/scalef)
        }
        if (any(or)) {
            k.pivot <- (1 - e) * Boundary.knots[2L] + e * Aknots[length(Aknots) - 
                ord]
            xr <- cbind(1, outer(x[or] - k.pivot, 1L:degree, 
                "^"))
            tt <- splineDesign(Aknots, rep(k.pivot, ord), ord, 
                derivs)
            basis[or, ] <- xr %*% (tt/scalef)
        }
        if (any(inside <- !outside)) 
            basis[inside, ] <- splineDesign(Aknots, x[inside], 
                ord)
    }
    else basis <- splineDesign(Aknots, x, ord)
    if (!intercept) 
        basis <- basis[, -1L, drop = FALSE]
    n.col <- ncol(basis)
    if (nas) {
        nmat <- matrix(NA, length(nax), n.col)
        nmat[!nax, ] <- basis
        basis <- nmat
    }
    dimnames(basis) <- list(nx, 1L:n.col)
    a <- list(degree = degree, knots = if (is.null(knots)) numeric(0L) else knots, 
        Boundary.knots = Boundary.knots, intercept = intercept)
    attributes(basis) <- c(attributes(basis), a)
    class(basis) <- c("bs", "basis", "matrix")
    basis
}


splineKnots <- function (object) 
UseMethod("splineKnots")


as.polySpline <- function (object, ...) 
polySpline(object, ...)


interpSpline <- function (obj1, obj2, bSpline = FALSE, period = NULL, na.action = na.fail, 
    sparse = FALSE) 
UseMethod("interpSpline")


splineDesign <- function (knots, x, ord = 4, derivs = 0L, outer.ok = FALSE, sparse = FALSE) 
{
    if ((nk <- length(knots <- as.numeric(knots))) <= 0) 
        stop("must have at least 'ord' knots")
    if (is.unsorted(knots)) 
        knots <- sort.int(knots)
    x <- as.numeric(x)
    nx <- length(x)
    if (length(derivs) > nx) 
        stop("length of 'derivs' is larger than length of 'x'")
    if (length(derivs) < 1L) 
        stop("empty 'derivs'")
    ord <- as.integer(ord)
    if (ord > nk || ord < 1) 
        stop("'ord' must be positive integer, at most the number of knots")
    if (!outer.ok && nk < 2 * ord - 1) 
        stop(gettextf("need at least %s (=%d) knots", "2*ord -1", 
            2 * ord - 1), domain = NA)
    o1 <- ord - 1L
    if (need.outer <- any(x < knots[ord] | knots[nk - o1] < x)) {
        if (outer.ok) {
            in.x <- knots[1L] <= x & x <= knots[nk]
            if ((x.out <- !all(in.x))) {
                x <- x[in.x]
                nnx <- length(x)
            }
            dkn <- diff(knots)[(nk - 1L):1]
            knots <- knots[c(rep.int(1L, o1), seq_len(nk), rep.int(nk, 
                max(0L, ord - match(TRUE, dkn > 0))))]
        }
        else stop(gettextf("the 'x' data must be in the range %g to %g unless you set '%s'", 
            knots[ord], knots[nk - o1], "outer.ok = TRUE"), domain = NA)
    }
    temp <- .Call(C_spline_basis, knots, ord, x, derivs)
    ncoef <- nk - ord
    ii <- if (need.outer && x.out) {
        rep.int((1L:nx)[in.x], rep.int(ord, nnx))
    }
    else rep.int(1L:nx, rep.int(ord, nx))
    jj <- c(outer(1L:ord, attr(temp, "Offsets"), "+"))
    if (sparse) {
        if (is.null(tryCatch(loadNamespace("Matrix"), error = function(e) NULL))) 
            stop(gettextf("%s needs package 'Matrix' correctly installed", 
                "splineDesign(*, sparse=TRUE)"), domain = NA)
        if (need.outer) {
            jj <- jj - o1 - 1L
            ok <- 0 <= jj & jj < ncoef
            methods::as(methods::new("dgTMatrix", i = ii[ok] - 
                1L, j = jj[ok], x = as.double(temp[ok]), Dim = c(nx, 
                ncoef)), "CsparseMatrix")
        }
        else methods::as(methods::new("dgTMatrix", i = ii - 1L, 
            j = jj - 1L, x = as.double(temp), Dim = c(nx, ncoef)), 
            "CsparseMatrix")
    }
    else {
        design <- matrix(double(nx * ncoef), nx, ncoef)
        if (need.outer) {
            jj <- jj - o1
            ok <- 1 <= jj & jj <= ncoef
            design[cbind(ii, jj)[ok, , drop = FALSE]] <- temp[ok]
        }
        else design[cbind(ii, jj)] <- temp
        design
    }
}


asVector <- function (object) 
UseMethod("asVector")


spline.des <- function (knots, x, ord = 4, derivs = integer(length(x)), outer.ok = FALSE, 
    sparse = FALSE) 
{
    if (is.unsorted(knots <- as.numeric(knots))) 
        knots <- sort.int(knots)
    list(knots = knots, order = ord, derivs = derivs, design = splineDesign(knots, 
        x, ord, derivs, outer.ok = outer.ok, sparse = sparse))
}


backSpline <- function (object) 
UseMethod("backSpline")


ns <- function (x, df = NULL, knots = NULL, intercept = FALSE, Boundary.knots = range(x)) 
{
    nx <- names(x)
    x <- as.vector(x)
    nax <- is.na(x)
    if (nas <- any(nax)) 
        x <- x[!nax]
    if (!missing(Boundary.knots)) {
        Boundary.knots <- sort(Boundary.knots)
        outside <- (ol <- x < Boundary.knots[1L]) | (or <- x > 
            Boundary.knots[2L])
    }
    else outside <- FALSE
    if (!is.null(df) && is.null(knots)) {
        nIknots <- df - 1L - intercept
        if (nIknots < 0L) {
            nIknots <- 0L
            warning(gettextf("'df' was too small; have used %d", 
                1L + intercept), domain = NA)
        }
        knots <- if (nIknots > 0L) {
            knots <- seq.int(0, 1, length.out = nIknots + 2L)[-c(1L, 
                nIknots + 2L)]
            quantile(x[!outside], knots)
        }
    }
    else nIknots <- length(knots)
    Aknots <- sort(c(rep(Boundary.knots, 4L), knots))
    if (any(outside)) {
        basis <- array(0, c(length(x), nIknots + 4L))
        if (any(ol)) {
            k.pivot <- Boundary.knots[1L]
            xl <- cbind(1, x[ol] - k.pivot)
            tt <- splineDesign(Aknots, rep(k.pivot, 2L), 4, c(0, 
                1))
            basis[ol, ] <- xl %*% tt
        }
        if (any(or)) {
            k.pivot <- Boundary.knots[2L]
            xr <- cbind(1, x[or] - k.pivot)
            tt <- splineDesign(Aknots, rep(k.pivot, 2L), 4, c(0, 
                1))
            basis[or, ] <- xr %*% tt
        }
        if (any(inside <- !outside)) 
            basis[inside, ] <- splineDesign(Aknots, x[inside], 
                4)
    }
    else basis <- splineDesign(Aknots, x, 4)
    const <- splineDesign(Aknots, Boundary.knots, 4, c(2, 2))
    if (!intercept) {
        const <- const[, -1, drop = FALSE]
        basis <- basis[, -1, drop = FALSE]
    }
    qr.const <- qr(t(const))
    basis <- as.matrix((t(qr.qty(qr.const, t(basis))))[, -(1L:2L), 
        drop = FALSE])
    n.col <- ncol(basis)
    if (nas) {
        nmat <- matrix(NA, length(nax), n.col)
        nmat[!nax, ] <- basis
        basis <- nmat
    }
    dimnames(basis) <- list(nx, 1L:n.col)
    a <- list(degree = 3L, knots = if (is.null(knots)) numeric() else knots, 
        Boundary.knots = Boundary.knots, intercept = intercept)
    attributes(basis) <- c(attributes(basis), a)
    class(basis) <- c("ns", "basis", "matrix")
    basis
}


xyVector <- function (x, y) 
{
    x <- as.vector(x)
    y <- as.vector(y)
    if (length(x) != length(y)) 
        stop("lengths of 'x' and 'y' must be the same")
    structure(list(x = x, y = y), class = "xyVector")
}


periodicSpline <- function (obj1, obj2, knots, period = 2 * pi, ord = 4) 
UseMethod("periodicSpline")


splineOrder <- function (object) 
UseMethod("splineOrder")


polySpline <- function (object, ...) 
UseMethod("polySpline")




## Package Data

# none


## Package Info

.skeleton_package_title = "Regression Spline Functions and Classes"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF