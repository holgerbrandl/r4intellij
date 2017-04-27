##
## Exported symobls in package `Matrix`
##

## Exported package methods

`.__T__qr.Q:base` <- "<environment>"

.__C__indMatrix <- new("classRepresentation"
    , slots = structure(list(perm = structure("integer", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("perm", 
"Dim", "Dimnames", "factors"))
    , contains = structure(list(sparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("sparseMatrix", 
"generalMatrix", "compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    n <- object@Dim[1]
    d <- object@Dim[2]
    perm <- object@perm
    if (length(perm) != n) 
        return(paste("length of 'perm' slot must be", n))
    if (n > 0 && (any(perm > d) || any(perm < 1))) 
        return("'perm' slot is not a valid index")
    TRUE
}
    , access = list()
    , className = structure("indMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(pMatrix = S4_object()), .Names = "pMatrix")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


onenormest <- function (A, t = min(n, 5), A.x, At.x, n, silent = FALSE, quiet = silent, 
    iter.max = 10, eps = 4 * .Machine$double.eps) 
{
    mi.A <- missing(A)
    mi.A.x <- missing(A.x)
    mi.At.x <- missing(At.x)
    no.A.x <- mi.A.x || !is.function(A.x)
    no.At.x <- mi.At.x || !is.function(At.x)
    if (mi.A && (no.A.x || no.At.x)) 
        stop("must either specify 'A' or the functions 'A.x' and 'At.x'")
    if (!mi.A && (!mi.A.x || !mi.At.x)) 
        warning("when 'A' is specified, 'A.x' and 'At.x' are disregarded")
    if (mi.A) {
        stopifnot(is.numeric(n), length(n) == 1, n == round(n), 
            n >= 0)
    }
    else {
        if (length(d <- dim(A)) != 2 || (n <- d[1]) != d[2]) 
            stop("'A' must be a square matrix")
        rm(d)
    }
    stopifnot(is.numeric(t), length(t) == 1, t >= 1, iter.max >= 
        1)
    X <- matrix(runif(n * t), n, t)
    X <- X/rep(colSums(X), each = n)
    been_there <- logical(n)
    I.t <- diag(nrow = t)
    est_old <- 0
    S <- matrix(0, n, t)
    for (iter in 1:(iter.max + 1)) {
        Y <- if (mi.A) 
            A.x(X)
        else A %*% X
        imax <- which.max(cY <- colSums(abs(Y)))
        est <- cY[imax]
        if (est > est_old || iter == 2) 
            w <- Y[, imax]
        if (iter >= 2 && est < est_old) {
            est <- est_old
            break
        }
        est_old <- est
        S_old <- S
        if (iter > iter.max) {
            if (!silent) 
                warning(gettextf("not converged in %d iterations", 
                  iter.max), domain = NA)
            break
        }
        S <- sign(Y)
        partest <- apply(abs(crossprod(S_old, S) - n) < eps * 
            n, 2, any)
        if (all(partest)) {
            if (!quiet) 
                message("hit a cycle (1) -- stop iterations")
            break
        }
        if (any(partest)) {
            numpar <- sum(partest)
            replacements <- matrix(sample(c(-1, 1), n * numpar, 
                replace = TRUE), n, numpar)
            S[, partest] <- replacements
        }
        partest <- apply(crossprod(S) - I.t == n, 2, any)
        if (any(partest)) {
            numpar <- sum(partest)
            replacements <- matrix(sample(c(-1, 1), n * numpar, 
                replace = TRUE), n, numpar)
            S[, partest] <- replacements
        }
        Z <- if (mi.A) 
            At.x(S)
        else crossprod(A, S)
        h <- pmax.int(2, as(abs(Z), "matrix"))
        dim(h) <- dim(Z)
        mhi <- apply(h, 2, which.max)
        if (iter >= 2 && all(mhi == imax)) {
            if (!quiet) 
                message("hit a cycle (2) -- stop iterations")
            break
        }
        r <- apply(h, 2, sort.int, decreasing = TRUE, index.return = TRUE)
        h <- sapply(r, `[[`, "x")
        ind <- sapply(r, `[[`, "ix")
        if (t > 1) {
            firstind <- ind[1:t]
            if (all(been_there[firstind])) {
                break
            }
            ind <- ind[!been_there[ind]]
            if (length(ind) < t) {
                if (!quiet) 
                  message("not enough new vecs -- stop iterations")
                break
            }
        }
        X <- matrix(0, n, t)
        X[cbind(ind[1:t], 1:t)] <- 1
        been_there[ind[1:t]] <- TRUE
    }
    v <- integer(n)
    v[imax] <- 1L
    list(est = est, v = v, w = w, iter = iter)
}


.nC2l <- function (from) 
.Call(nz_pattern_to_Csparse, from, 1L)


.__C__sparseQR <- new("classRepresentation"
    , slots = structure(list(V = structure("dgCMatrix", package = "Matrix"), 
    beta = structure("numeric", package = "methods"), p = structure("integer", package = "methods"), 
    R = structure("dgCMatrix", package = "Matrix"), q = structure("integer", package = "methods"), 
    Dim = structure("integer", package = "methods")), .Names = c("V", 
"beta", "p", "R", "q", "Dim"))
    , contains = structure(list(MatrixFactorization = S4_object()), .Names = "MatrixFactorization")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(sparseQR_validate, object)
    , access = list()
    , className = structure("sparseQR", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__diag:base` <- "<environment>"

isDiagonal <- function (object) 
standardGeneric("isDiagonal")


expm <- function (x) 
standardGeneric("expm")


cbind2 <- methods::cbind2 # re-exported from methods package

nnzero <- function (x, na.counted = NA) 
standardGeneric("nnzero")


`.__T__[:base` <- "<environment>"

.__C__ldenseMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(lMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("lMatrix", 
"denseMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ldenseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(lgeMatrix = S4_object(), 
    ltrMatrix = S4_object(), 
    ltpMatrix = S4_object(), 
    lsyMatrix = S4_object(), 
    lspMatrix = S4_object()), .Names = c("lgeMatrix", 
"ltrMatrix", "ltpMatrix", "lsyMatrix", "lspMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__atomicVector <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = logical(0)
    , validity = NULL
    , access = list()
    , className = structure("atomicVector", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(logical = S4_object(), 
    integer = S4_object(), 
    numeric = S4_object(), 
    complex = S4_object(), 
    raw = S4_object(), 
    character = S4_object(), 
    factor = S4_object(), 
    signature = S4_object(), 
    className = S4_object(), 
    ObjectsWithPackage = S4_object(), 
    factor = S4_object()), .Names = c("logical", 
"integer", "numeric", "complex", "raw", "character", "factor", 
"signature", "className", "ObjectsWithPackage", "factor"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


expand <- function (x, ...) 
standardGeneric("expand")


fac2Sparse <- function (from, to = c("d", "i", "l", "n", "z"), drop.unused.levels = TRUE, 
    giveCsparse = TRUE, factorPatt12, contrasts.arg = NULL) 
{
    stopifnot(is.logical(factorPatt12), length(factorPatt12) == 
        2)
    if (any(factorPatt12)) 
        m <- fac2sparse(from, to = to, drop.unused.levels = drop.unused.levels, 
            giveCsparse = giveCsparse)
    ans <- list(NULL, if (factorPatt12[2]) m)
    if (factorPatt12[1]) {
        if (is.null(contrasts.arg)) 
            contrasts.arg <- getOption("contrasts")[if (is.ordered(from)) 
                "ordered"
            else "unordered"]
        ans[[1]] <- crossprod(if (is.character(contrasts.arg)) {
            stopifnot(is.function(FUN <- get(contrasts.arg)))
            FUN(rownames(m), sparse = TRUE)
        }
        else as(contrasts.arg, "sparseMatrix"), m)
    }
    ans
}


`.__T__is.na:base` <- "<environment>"

.__C__zMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("complex", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"xMatrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("zMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


zapsmall <- function (x, digits = getOption("digits")) 
standardGeneric("zapsmall")


`.__T__-:base` <- "<environment>"

`.__T__diag<-:base` <- "<environment>"

cov2cor <- function (V) 
standardGeneric("cov2cor")


sparseMatrix <- function (i = ep, j = ep, p, x, dims, dimnames, symmetric = FALSE, 
    triangular = FALSE, index1 = TRUE, giveCsparse = TRUE, check = TRUE, 
    use.last.ij = FALSE) 
{
    if ((m.i <- missing(i)) + (m.j <- missing(j)) + (m.p <- missing(p)) != 
        1) 
        stop("exactly one of 'i', 'j', or 'p' must be missing from call")
    if (!m.p) {
        p <- as.integer(p)
        if ((lp <- length(p)) < 1 || p[1] != 0 || any((dp <- p[-1] - 
            p[-lp]) < 0)) 
            stop("'p' must be a non-decreasing vector (0, ...)")
        ep <- rep.int(seq_along(dp), dp)
    }
    i1 <- as.logical(index1)[1]
    i <- as.integer(i + (!(m.i || i1)))
    j <- as.integer(j + (!(m.j || i1)))
    dims.min <- suppressWarnings(c(max(i), max(j)))
    if (anyNA(dims.min)) 
        stop("NA's in (i,j) are not allowed")
    if (missing(dims)) {
        dims <- if (symmetric || triangular) 
            rep(max(dims.min), 2)
        else dims.min
    }
    else {
        stopifnot(all(dims >= dims.min))
        dims <- as.integer(dims)
    }
    if (symmetric && triangular) 
        stop("Both 'symmetric' and 'triangular', i.e. asking for diagonal matrix.  Use 'Diagonal()' instead")
    sx <- if (symmetric) {
        if (dims[1] != dims[2]) 
            stop("symmetric matrix must be square")
        "s"
    }
    else if (triangular) {
        if (dims[1] != dims[2]) 
            stop("triangular matrix must be square")
        "t"
    }
    else "g"
    isPat <- missing(x)
    kx <- if (isPat) 
        "n"
    else .M.kind(x)
    r <- new(paste0(kx, sx, "TMatrix"))
    r@Dim <- dims
    if (symmetric && all(i >= j)) 
        r@uplo <- "L"
    else if (triangular) {
        r@uplo <- if (all(i >= j)) 
            "L"
        else if (all(i <= j)) 
            "U"
        else stop("triangular matrix must have all i >= j or i <= j")
    }
    if (!isPat) {
        if (kx == "d" && !is.double(x)) 
            x <- as.double(x)
        if (length(x) != (n <- length(i))) {
            if (length(x) != 1 && n%%length(x) != 0) 
                warning("length(i) is not a multiple of length(x)")
            x <- rep_len(x, n)
        }
        if (use.last.ij && (id <- anyDuplicated(cbind(i, j), 
            fromLast = TRUE))) {
            i <- i[-id]
            j <- j[-id]
            x <- x[-id]
            if (any(idup <- duplicated(cbind(i, j), fromLast = TRUE))) {
                ndup <- -which(idup)
                i <- i[ndup]
                j <- j[ndup]
                x <- x[ndup]
            }
        }
        r@x <- x
    }
    r@i <- i - 1L
    r@j <- j - 1L
    if (!missing(dimnames)) 
        r@Dimnames <- .fixupDimnames(dimnames)
    if (check) 
        validObject(r)
    if (giveCsparse) 
        as(r, "CsparseMatrix")
    else r
}


.__C__dppMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(dspMatrix = S4_object(), 
    ddenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dspMatrix", 
"ddenseMatrix", "symmetricMatrix", "dMatrix", "denseMatrix", 
"compMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dppMatrix_validate, object)
    , access = list()
    , className = structure("dppMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__lgTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "x", "factors"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"lsparseMatrix", "generalMatrix", "lMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(xTMatrix_validate, object)
    , access = list()
    , className = structure("lgTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__dim:base` <- "<environment>"

.__C__CsparseMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames"))
    , contains = structure(list(sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(Csparse_validate, object)
    , access = list()
    , className = structure("CsparseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dgCMatrix = S4_object(), 
    dtCMatrix = S4_object(), 
    dsCMatrix = S4_object(), 
    lgCMatrix = S4_object(), 
    ltCMatrix = S4_object(), 
    lsCMatrix = S4_object(), 
    ngCMatrix = S4_object(), 
    ntCMatrix = S4_object(), 
    nsCMatrix = S4_object()), .Names = c("dgCMatrix", 
"dtCMatrix", "dsCMatrix", "lgCMatrix", "ltCMatrix", "lsCMatrix", 
"ngCMatrix", "ntCMatrix", "nsCMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__coerce:methods` <- "<environment>"

`.__T__unname:base` <- "<environment>"

which <- function (x, arr.ind = FALSE, useNames = TRUE) 
standardGeneric("which")


.__C__RsparseMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames"))
    , contains = structure(list(sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(Rsparse_validate, object)
    , access = list()
    , className = structure("RsparseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dgRMatrix = S4_object(), 
    dtRMatrix = S4_object(), 
    dsRMatrix = S4_object(), 
    lgRMatrix = S4_object(), 
    ltRMatrix = S4_object(), 
    lsRMatrix = S4_object(), 
    ngRMatrix = S4_object(), 
    ntRMatrix = S4_object(), 
    nsRMatrix = S4_object()), .Names = c("dgRMatrix", 
"dtRMatrix", "dsRMatrix", "lgRMatrix", "ltRMatrix", "lsRMatrix", 
"ngRMatrix", "ntRMatrix", "nsRMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


KhatriRao <- function (X, Y = X, FUN = "*", make.dimnames = FALSE) 
{
    stopifnot((p <- ncol(X)) == ncol(Y))
    X <- as(X, "CsparseMatrix")
    Y <- as(Y, "CsparseMatrix")
    xn <- diff(X@p)
    yn <- diff(yp <- Y@p)
    newp <- as.integer(diffinv(xn * yn))
    xn.yp <- xn[as.logical(yn)]
    yj <- .Call(Matrix_expand_pointers, yp)
    yj <- factor(yj)
    rep.yn <- rep.int(yn, xn)
    i1 <- rep.int(X@i, rep.yn)
    i2 <- unlist(rep(split.default(Y@i, yj), xn.yp))
    n1 <- nrow(X)
    n2 <- nrow(Y)
    newi <- i1 * n2 + i2
    dim <- as.integer(c(n1 * n2, p))
    dns <- if (make.dimnames) {
        list(as.vector(outer(rownames(Y), rownames(X), FUN = "paste", 
            sep = ":")), colnames(X))
    }
    else list(NULL, NULL)
    if ((nX <- is(X, "nMatrix")) & (nY <- is(Y, "nMatrix"))) 
        new("ngCMatrix", Dim = dim, Dimnames = dns, i = newi, 
            p = newp)
    else {
        if (nX) 
            X <- as(X, "lgCMatrix")
        if (nY) 
            Y <- as(Y, "lgCMatrix")
        x1 <- rep.int(X@x, rep.yn)
        x2 <- unlist(rep(split.default(Y@x, yj), xn.yp))
        new("dgCMatrix", Dim = dim, Dimnames = dns, i = newi, 
            p = newp, x = match.fun(FUN)(x1, x2))
    }
}


mean <- function (x, ...) 
standardGeneric("mean")


`.__T__dimnames<-:base` <- "<environment>"

colSums <- function (x, na.rm = FALSE, dims = 1, ...) 
standardGeneric("colSums")


`.__T__colSums:base` <- "<environment>"

band <- function (x, k1, k2, ...) 
standardGeneric("band")


`.__T__format:base` <- "<environment>"

`.__T__head:utils` <- "<environment>"

`.__T__as.logical:base` <- "<environment>"

`.__T__%%:base` <- "<environment>"

.__C__ngRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), factors = structure("list", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "factors"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"nsparseMatrix", "generalMatrix", "nMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ngRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Schur <- function (x, vectors, ...) 
standardGeneric("Schur")


`.__T__update:stats` <- "<environment>"

.__C__ltTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "x", "uplo", "diag"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"lsparseMatrix", "triangularMatrix", "lMatrix", "sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tTMatrix_validate, object)
    , access = list()
    , className = structure("ltTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


qr.qty <- function (qr, y) 
standardGeneric("qr.qty")


`.__T__summary:base` <- "<environment>"

..2dge <- function (from) 
.Call(dup_mMatrix_as_dgeMatrix, from)


`.__T__chol2inv:base` <- "<environment>"

.__C__rleDiff <- new("classRepresentation"
    , slots = structure(list(first = structure("numLike", package = "Matrix"), 
    rle = structure("rle", package = "methods")), .Names = c("first", 
"rle"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (length(object@first) != 1) 
        return("'first' must be of length one")
    rl <- object@rle
    if (!is.list(rl) || length(rl) != 2 || !identical(sort(names(rl)), 
        c("lengths", "values"))) 
        return("'rle' must be a list (lengths = *, values = *)")
    if (length(lens <- rl$lengths) != length(vals <- rl$values)) 
        return("'lengths' and 'values' differ in length")
    if (any(lens <= 0)) 
        return("'lengths' must be positive")
    TRUE
}
    , access = list()
    , className = structure("rleDiff", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


unname <- function (obj, force = FALSE) 
standardGeneric("unname")


.__C__ntpMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(ndenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    nMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ndenseMatrix", 
"triangularMatrix", "nMatrix", "denseMatrix", "Matrix", "mMatrix", 
"xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ntpMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__[<-:base` <- "<environment>"

`.__T__tril:Matrix` <- "<environment>"

.dsy2dsp <- function (from) 
.Call(dsyMatrix_as_dspMatrix, from)


rep2abI <- function (x, times) 
{
    r <- new("abIndex")
    if ((n <- length(x)) == 0) 
        return(r)
    if (n == 1) {
        r@kind <- "rleDiff"
        rD <- new("rleDiff")
        rD@first <- x[1L]
        rD@rle <- .rle(lengths = times - 1L, values = 0L)
        r@rleD <- rD
    }
    else {
        rr <- rleMaybe(.diff(x))
        if (is.null(rr)) {
            r@kind <- if (is.integer(x)) 
                "int32"
            else "double"
            r@x <- rep.int(x, times)
        }
        else {
            r@kind <- "rleDiff"
            rD <- new("rleDiff")
            rD@first <- x[1L]
            Dx <- x[1L] - x[length(x)]
            N <- (length(rr$lengths) + 1L) * times
            rD@rle <- .rle(lengths = rep.int(c(rr$lengths, 1L), 
                times)[-N], values = rep.int(c(rr$values, Dx), 
                times)[-N])
            r@rleD <- rD
        }
    }
    r
}


.__C__triangularMatrix <- new("classRepresentation"
    , slots = structure(list(uplo = structure("character", package = "methods"), 
    diag = structure("character", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("uplo", 
"diag", "Dim", "Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(triangularMatrix_validate, object)
    , access = list()
    , className = structure("triangularMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dtrMatrix = S4_object(), 
    dtpMatrix = S4_object(), 
    ltrMatrix = S4_object(), 
    ltpMatrix = S4_object(), 
    ntrMatrix = S4_object(), 
    ntpMatrix = S4_object(), 
    dtTMatrix = S4_object(), 
    dtCMatrix = S4_object(), 
    dtRMatrix = S4_object(), 
    ltTMatrix = S4_object(), 
    ltCMatrix = S4_object(), 
    ltRMatrix = S4_object(), 
    ntTMatrix = S4_object(), 
    ntCMatrix = S4_object(), 
    ntRMatrix = S4_object(), 
    Cholesky = S4_object(), 
    pCholesky = S4_object(), 
    BunchKaufman = S4_object(), 
    pBunchKaufman = S4_object()), .Names = c("dtrMatrix", 
"dtpMatrix", "ltrMatrix", "ltpMatrix", "ntrMatrix", "ntpMatrix", 
"dtTMatrix", "dtCMatrix", "dtRMatrix", "ltTMatrix", "ltCMatrix", 
"ltRMatrix", "ntTMatrix", "ntCMatrix", "ntRMatrix", "Cholesky", 
"pCholesky", "BunchKaufman", "pBunchKaufman"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


qr.coef <- function (qr, y) 
standardGeneric("qr.coef")


.__C__abIndex <- new("classRepresentation"
    , slots = structure(list(kind = structure("character", package = "methods"), 
    x = structure("numLike", package = "Matrix"), rleD = structure("rleDiff", package = "Matrix")), .Names = c("kind", 
"x", "rleD"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    switch(object@kind, int32 = if (!is.integer(object@x)) return("'x' slot must be integer when kind is 'int32'"), 
        double = if (!is.double(object@x)) return("'x' slot must be double when kind is 'double'"), 
        rleDiff = {
            if (length(object@x)) return("'x' slot must be empty when kind is 'rleDiff'")
        }, return("'kind' must be one of (\"int32\", \"double\", \"rleDiff\")"))
    TRUE
}
    , access = list()
    , className = structure("abIndex", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__pBunchKaufman <- new("classRepresentation"
    , slots = structure(list(perm = structure("integer", package = "methods"), 
    x = structure("numeric", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    diag = structure("character", package = "methods")), .Names = c("perm", 
"x", "Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(dtpMatrix = S4_object(), 
    MatrixFactorization = S4_object(), 
    ddenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dtpMatrix", 
"MatrixFactorization", "ddenseMatrix", "triangularMatrix", "dMatrix", 
"denseMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(pBunchKaufman_validate, object)
    , access = list()
    , className = structure("pBunchKaufman", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__CHMsuper <- new("classRepresentation"
    , slots = structure(list(super = structure("integer", package = "methods"), 
    pi = structure("integer", package = "methods"), px = structure("integer", package = "methods"), 
    s = structure("integer", package = "methods"), colcount = structure("integer", package = "methods"), 
    perm = structure("integer", package = "methods"), type = structure("integer", package = "methods"), 
    Dim = structure("integer", package = "methods")), .Names = c("super", 
"pi", "px", "s", "colcount", "perm", "type", "Dim"))
    , contains = structure(list(CHMfactor = S4_object(), 
    CholeskyFactorization = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("CHMfactor", 
"CholeskyFactorization", "MatrixFactorization"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(CHMsuper_validate, object)
    , access = list()
    , className = structure("CHMsuper", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dCHMsuper = S4_object(), 
    nCHMsuper = S4_object()), .Names = c("dCHMsuper", 
"nCHMsuper"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Diagonal <- function (n, x = NULL) 
{
    n <- if (missing(n)) 
        length(x)
    else {
        stopifnot(length(n) == 1, n == as.integer(n), n >= 0)
        as.integer(n)
    }
    if (missing(x)) 
        new("ddiMatrix", Dim = c(n, n), diag = "U")
    else {
        lx <- length(x)
        lx.1 <- lx == 1L
        stopifnot(lx.1 || lx == n)
        if (is.logical(x)) 
            cl <- "ldiMatrix"
        else if (is.numeric(x)) {
            cl <- "ddiMatrix"
            x <- as.numeric(x)
        }
        else if (is.complex(x)) {
            cl <- "zdiMatrix"
        }
        else stop("'x' has invalid data type")
        if (lx.1 && !is.na(x) && x == 1) 
            new(cl, Dim = c(n, n), diag = "U")
        else new(cl, Dim = c(n, n), diag = "N", x = if (lx.1) 
            rep.int(x, n)
        else x)
    }
}


.__C__nsyMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(ndenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    nMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ndenseMatrix", 
"symmetricMatrix", "nMatrix", "denseMatrix", "compMatrix", "Matrix", 
"mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dense_nonpacked_validate, object)
    , access = list()
    , className = structure("nsyMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__Math:base` <- "<environment>"

qrR <- function (qr, complete = FALSE, backPermute = TRUE, row.names = TRUE) 
{
    ir <- seq_len(qr@Dim[if (complete) 1L else 2L])
    r <- if (backPermute <- backPermute && (n <- length(qr@q)) && 
        !isSeq(qr@q, n - 1L)) 
        qr@R[ir, order(qr@q), drop = FALSE]
    else qr@R[ir, , drop = FALSE]
    if (row.names && !is.null(rn <- qr@V@Dimnames[[1]])) 
        r@Dimnames[[1]] <- rn[seq_len(r@Dim[1L])]
    if (complete || backPermute) 
        r
    else as(r, "triangularMatrix")
}


.__C__lsTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "x", "uplo", "factors"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"lsparseMatrix", "symmetricMatrix", "lMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tTMatrix_validate, object)
    , access = list()
    , className = structure("lsTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


rowSums <- function (x, na.rm = FALSE, dims = 1, ...) 
standardGeneric("rowSums")


skewpart <- function (x) 
standardGeneric("skewpart")


.diag.dsC <- function (x, Chx = Cholesky(x, LDL = TRUE), res.kind = "diag") 
{
    force(Chx)
    if (!missing(Chx)) 
        stopifnot(.isLDL(Chx), is.integer(Chx@p), is.double(Chx@x))
    .Call(diag_tC, Chx, res.kind)
}


.__C__ngeMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "factors"))
    , contains = structure(list(ndenseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    geMatrix = S4_object(), 
    nMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    xMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("ndenseMatrix", 
"generalMatrix", "geMatrix", "nMatrix", "denseMatrix", "xMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dense_nonpacked_validate, object)
    , access = list()
    , className = structure("ngeMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__ntRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    diag = structure("character", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"nsparseMatrix", "triangularMatrix", "nMatrix", "sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ntRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__kronecker:base` <- "<environment>"

.__C__dpoMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(dsyMatrix = S4_object(), 
    ddenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dsyMatrix", 
"ddenseMatrix", "symmetricMatrix", "dMatrix", "denseMatrix", 
"compMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dpoMatrix_validate, object)
    , access = list()
    , className = structure("dpoMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(corMatrix = S4_object()), .Names = "corMatrix")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


head <- function (x, ...) 
standardGeneric("head")


printSpMatrix <- function (x, digits = NULL, maxp = getOption("max.print"), cld = getClassDef(class(x)), 
    zero.print = ".", col.names, note.dropping.colnames = TRUE, 
    uniDiag = TRUE, col.trailer = "", align = c("fancy", "right")) 
{
    stopifnot(extends(cld, "sparseMatrix"))
    cx <- formatSpMatrix(x, digits = digits, maxp = maxp, cld = cld, 
        zero.print = zero.print, col.names = col.names, note.dropping.colnames = note.dropping.colnames, 
        uniDiag = uniDiag, align = align)
    if (col.trailer != "") 
        cx <- cbind(cx, col.trailer, deparse.level = 0)
    print(cx, quote = FALSE, right = TRUE, max = maxp)
    invisible(x)
}


`.__T__all:base` <- "<environment>"

`.__T__as.integer:base` <- "<environment>"

.__C__nsCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    nCsparseMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"nsparseMatrix", "symmetricMatrix", "nCsparseMatrix", "nMatrix", 
"sparseMatrix", "compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nsCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.solve.dgC.qr <- function (x, y, order = 1L) 
{
    cld <- getClass(class(x))
    .Call(dgCMatrix_qrsol, if (extends(cld, "dgCMatrix") || extends(cld, 
        "dtCMatrix")) x else as(x, "dgCMatrix"), y, order)
}


.__C__nspMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(ndenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    nMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ndenseMatrix", 
"symmetricMatrix", "nMatrix", "denseMatrix", "compMatrix", "Matrix", 
"mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dspMatrix_validate, object)
    , access = list()
    , className = structure("nspMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__qr.R:base` <- "<environment>"

.__C__zsparseVector <- new("classRepresentation"
    , slots = structure(list(x = structure("complex", package = "methods"), 
    length = structure("numeric", package = "methods"), i = structure("numeric", package = "methods")), .Names = c("x", 
"length", "i"))
    , contains = structure(list(sparseVector = S4_object(), 
    xsparseVector = S4_object()), .Names = c("sparseVector", 
"xsparseVector"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (length(object@i) != length(object@x)) 
        "'i' and 'x' differ in length"
    else TRUE
}
    , access = list()
    , className = structure("zsparseVector", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__ltCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "x", "uplo", "diag"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    lCsparseMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"lsparseMatrix", "triangularMatrix", "lCsparseMatrix", "lMatrix", 
"sparseMatrix", "Matrix", "xMatrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(xCMatrix_validate, object)
    , access = list()
    , className = structure("ltCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


drop <- function (x) 
standardGeneric("drop")


.__C__sparseVector <- new("classRepresentation"
    , slots = structure(list(length = structure("numeric", package = "methods"), 
    i = structure("numeric", package = "methods")), .Names = c("length", 
"i"))
    , contains = structure(list(), .Names = character(0))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    n <- object@length
    if (anyNA(i <- object@i)) 
        "'i' slot has NAs"
    else if (any(!is.finite(i))) 
        "'i' slot is not all finite"
    else if (any(i < 1)) 
        "'i' must be >= 1"
    else if (n == 0 && length(i)) 
        "'i' must be empty when the object length is zero"
    else if (any(i > n)) 
        sprintf("'i' must be in 1:%d", n)
    else if (is.unsorted(i, strictly = TRUE)) 
        "'i' must be sorted strictly increasingly"
    else TRUE
}
    , access = list()
    , className = structure("sparseVector", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dsparseVector = S4_object(), 
    isparseVector = S4_object(), 
    lsparseVector = S4_object(), 
    zsparseVector = S4_object(), 
    nsparseVector = S4_object()), .Names = c("dsparseVector", 
"isparseVector", "lsparseVector", "zsparseVector", "nsparseVector"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__qr:base` <- "<environment>"

rankMatrix <- function (x, tol = NULL, method = c("tolNorm2", "qr.R", "qrLINPACK", 
    "qr", "useGrad", "maybeGrad"), sval = svd(x, 0, 0)$d, warn.t = TRUE) 
{
    stopifnot(length(d <- dim(x)) == 2)
    p <- min(d)
    method <- match.arg(method)
    if (useGrad <- (method %in% c("useGrad", "maybeGrad"))) {
        stopifnot(length(sval) == p, diff(sval) <= 0)
        ln.av <- log(abs(sval))
        diff1 <- diff(ln.av)
        if (method == "maybeGrad") {
            grad <- (min(ln.av) - max(ln.av))/p
            useGrad <- (min(diff1) <= min(-3, 10 * grad))
        }
    }
    if (!useGrad) {
        x.dense <- is.numeric(x) || is(x, "denseMatrix")
        if ((Meth <- method) == "qr") 
            method <- if (x.dense) 
                "qrLINPACK"
            else "qr.R"
        else Meth <- substr(method, 1, 2)
        if (Meth == "qr") {
            if (is.null(tol)) 
                tol <- max(d) * .Machine$double.eps
        }
        else {
            if (is.null(tol)) {
                if (!x.dense && missing(sval) && prod(d) >= 100000L) 
                  warning(gettextf("rankMatrix(<large sparse Matrix>, method = '%s') coerces to dense matrix.\n Probably should rather use method = 'qr' !?", 
                    method), immediate. = TRUE, domain = NA)
                stopifnot(diff(sval) <= 0)
                tol <- max(d) * .Machine$double.eps
            }
            else stopifnot((tol <- as.numeric(tol)[[1]]) >= 0)
        }
    }
    structure(if (useGrad) 
        which.min(diff1)
    else if (Meth == "qr") {
        if ((do.t <- (d[1L] < d[2L])) && warn.t) 
            warning(gettextf("rankMatrix(x, method='qr'): computing t(x) as nrow(x) < ncol(x)"))
        q.r <- qr(if (do.t) 
            t(x)
        else x, tol = tol, LAPACK = method != "qrLINPACK")
        if (x.dense && (method == "qrLINPACK")) 
            q.r$rank
        else {
            diagR <- if (x.dense) 
                diag(q.r$qr)
            else diag(q.r@R)
            d.i <- abs(diagR)
            sum(d.i >= tol * max(d.i))
        }
    }
    else sum(sval >= tol * sval[1]), method = method, useGrad = useGrad, 
        tol = if (useGrad) 
            NA
        else tol)
}


rbind2 <- methods::rbind2 # re-exported from methods package

.__C__BunchKaufman <- new("classRepresentation"
    , slots = structure(list(perm = structure("integer", package = "methods"), 
    x = structure("numeric", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    diag = structure("character", package = "methods")), .Names = c("perm", 
"x", "Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(dtrMatrix = S4_object(), 
    MatrixFactorization = S4_object(), 
    ddenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dtrMatrix", 
"MatrixFactorization", "ddenseMatrix", "triangularMatrix", "dMatrix", 
"denseMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(BunchKaufman_validate, object)
    , access = list()
    , className = structure("BunchKaufman", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


rsparsematrix <- function (nrow, ncol, density, nnz = round(density * maxE), symmetric = FALSE, 
    rand.x = function(n) signif(rnorm(nnz), 2), ...) 
{
    maxE <- if (symmetric) 
        nrow * (nrow + 1)/2
    else nrow * ncol
    stopifnot((nnz <- as.integer(nnz)) >= 0, nrow >= 0, ncol >= 
        0, nnz <= maxE)
    ijI <- -1L + if (symmetric) 
        sample(indTri(nrow, diag = TRUE), nnz)
    else sample.int(maxE, nnz)
    if (is.null(rand.x)) 
        sparseMatrix(i = ijI%%nrow, j = ijI%/%nrow, index1 = FALSE, 
            symmetric = symmetric, dims = c(nrow, ncol), ...)
    else sparseMatrix(i = ijI%%nrow, j = ijI%/%nrow, index1 = FALSE, 
        symmetric = symmetric, x = rand.x(nnz), dims = c(nrow, 
            ncol), ...)
}


Cholesky <- function (A, perm = TRUE, LDL = !super, super = FALSE, Imult = 0, 
    ...) 
standardGeneric("Cholesky")


qr.resid <- function (qr, y) 
standardGeneric("qr.resid")


.diag2mat <- function (from) 
mkDiag(if (from@diag == "U") as1(from@x) else from@x, n = from@Dim[1])


.__C__nsRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"nsparseMatrix", "symmetricMatrix", "nMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nsRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__dgTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "x", "factors"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"dsparseMatrix", "generalMatrix", "dMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(xTMatrix_validate, object)
    , access = list()
    , className = structure("dgTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__dimnames:base` <- "<environment>"

.dsy2mat <- function (from, keep.dimnames = TRUE) 
.Call(dsyMatrix_as_matrix, from, keep.dimnames)


.__C__Cholesky <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(dtrMatrix = S4_object(), 
    CholeskyFactorization = S4_object(), 
    ddenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    MatrixFactorization = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dtrMatrix", 
"CholeskyFactorization", "ddenseMatrix", "triangularMatrix", 
"MatrixFactorization", "dMatrix", "denseMatrix", "Matrix", "mMatrix", 
"xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("Cholesky", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


tcrossprod <- function (x, y = NULL, ...) 
standardGeneric("tcrossprod")


`.__T__cov2cor:stats` <- "<environment>"

`.__T__qr.fitted:base` <- "<environment>"

`.__T__updown:Matrix` <- "<environment>"

`.__T__is.infinite:base` <- "<environment>"

.__C__denseMatrix <- new("classRepresentation"
    , slots = structure(list(Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("Dim", 
"Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("denseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(ddenseMatrix = S4_object(), 
    ldenseMatrix = S4_object(), 
    ndenseMatrix = S4_object(), 
    dgeMatrix = S4_object(), 
    dtrMatrix = S4_object(), 
    dtpMatrix = S4_object(), 
    dsyMatrix = S4_object(), 
    dspMatrix = S4_object(), 
    dpoMatrix = S4_object(), 
    dppMatrix = S4_object(), 
    lgeMatrix = S4_object(), 
    ltrMatrix = S4_object(), 
    ltpMatrix = S4_object(), 
    lsyMatrix = S4_object(), 
    lspMatrix = S4_object(), 
    ngeMatrix = S4_object(), 
    ntrMatrix = S4_object(), 
    ntpMatrix = S4_object(), 
    nsyMatrix = S4_object(), 
    nspMatrix = S4_object(), 
    corMatrix = S4_object(), 
    Cholesky = S4_object(), 
    pCholesky = S4_object(), 
    BunchKaufman = S4_object(), 
    pBunchKaufman = S4_object()), .Names = c("ddenseMatrix", 
"ldenseMatrix", "ndenseMatrix", "dgeMatrix", "dtrMatrix", "dtpMatrix", 
"dsyMatrix", "dspMatrix", "dpoMatrix", "dppMatrix", "lgeMatrix", 
"ltrMatrix", "ltpMatrix", "lsyMatrix", "lspMatrix", "ngeMatrix", 
"ntrMatrix", "ntpMatrix", "nsyMatrix", "nspMatrix", "corMatrix", 
"Cholesky", "pCholesky", "BunchKaufman", "pBunchKaufman"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


qr.Q <- function (qr, complete = FALSE, Dvec) 
standardGeneric("qr.Q")


qr.R <- function (qr, complete = FALSE, ...) 
standardGeneric("qr.R")


`.__T__prod:base` <- "<environment>"

forceSymmetric <- function (x, uplo) 
standardGeneric("forceSymmetric")


.__C__ntrMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(ndenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    nMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ndenseMatrix", 
"triangularMatrix", "nMatrix", "denseMatrix", "Matrix", "mMatrix", 
"xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dense_nonpacked_validate, object)
    , access = list()
    , className = structure("ntrMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__lsparseVector <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    length = structure("numeric", package = "methods"), i = structure("numeric", package = "methods")), .Names = c("x", 
"length", "i"))
    , contains = structure(list(sparseVector = S4_object(), 
    xsparseVector = S4_object()), .Names = c("sparseVector", 
"xsparseVector"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (length(object@i) != length(object@x)) 
        "'i' and 'x' differ in length"
    else TRUE
}
    , access = list()
    , className = structure("lsparseVector", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Logic <- methods::Logic # re-exported from methods package

diag <- function (x = 1, nrow, ncol) 
standardGeneric("diag")


.__C__dtTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "x", "uplo", "diag"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"dsparseMatrix", "triangularMatrix", "dMatrix", "sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tTMatrix_validate, object)
    , access = list()
    , className = structure("dtTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__lsparseMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("lMatrix", 
"sparseMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("lsparseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(lgTMatrix = S4_object(), 
    ltTMatrix = S4_object(), 
    lsTMatrix = S4_object(), 
    lgCMatrix = S4_object(), 
    ltCMatrix = S4_object(), 
    lsCMatrix = S4_object(), 
    lgRMatrix = S4_object(), 
    ltRMatrix = S4_object(), 
    lsRMatrix = S4_object()), .Names = c("lgTMatrix", 
"ltTMatrix", "lsTMatrix", "lgCMatrix", "ltCMatrix", "lsCMatrix", 
"lgRMatrix", "ltRMatrix", "lsRMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__nnzero:Matrix` <- "<environment>"

.__C__xsparseVector <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = new("dsparseVector"
    , x = numeric(0)
    , length = 0
    , i = numeric(0)
)
    , validity = NULL
    , access = list()
    , className = structure("xsparseVector", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dsparseVector = S4_object(), 
    isparseVector = S4_object(), 
    lsparseVector = S4_object(), 
    zsparseVector = S4_object()), .Names = c("dsparseVector", 
"isparseVector", "lsparseVector", "zsparseVector"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__expand:Matrix` <- "<environment>"

.bdiag <- function (lst) 
{
    stopifnot(is.list(lst), (nl <- length(lst)) >= 1)
    Tlst <- lapply(lapply(lst, as_Csp2), as, "TsparseMatrix")
    if (nl == 1) 
        return(Tlst[[1]])
    i_off <- c(0L, cumsum(vapply(Tlst, nrow, 1L)))
    j_off <- c(0L, cumsum(vapply(Tlst, ncol, 1L)))
    clss <- vapply(Tlst, class, "")
    typ <- substr(clss, 2, 2)
    knd <- substr(clss, 1, 1)
    sym <- typ == "s"
    tri <- typ == "t"
    use.n <- any(is.n <- knd == "n")
    if (use.n && !(use.n <- all(is.n))) {
        Tlst[is.n] <- lapply(Tlst[is.n], as, "lMatrix")
        knd[is.n] <- "l"
    }
    use.l <- !use.n && all(knd == "l")
    if (all(sym)) {
        uplos <- vapply(Tlst, slot, ".", "uplo")
        tLU <- table(uplos)
        if (length(tLU) == 1) {
            useU <- uplos[1] == "U"
        }
        else {
            useU <- diff(tLU) >= 0
            if (useU && (hasL <- tLU[1] > 0)) 
                Tlst[hasL] <- lapply(Tlst[hasL], t)
            else if (!useU && (hasU <- tLU[2] > 0)) 
                Tlst[hasU] <- lapply(Tlst[hasU], t)
        }
        if (use.n) {
            r <- new("nsTMatrix")
        }
        else {
            r <- new(paste0(if (use.l) 
                "l"
            else "d", "sTMatrix"))
            r@x <- unlist(lapply(Tlst, slot, "x"))
        }
        r@uplo <- if (useU) 
            "U"
        else "L"
    }
    else if (all(tri) && {
        ULs <- vapply(Tlst, slot, ".", "uplo")
        all(ULs[1L] == ULs[-1L])
    }) {
        if (use.n) {
            r <- new("ntTMatrix")
        }
        else {
            r <- new(paste0(if (use.l) 
                "l"
            else "d", "tTMatrix"))
            r@x <- unlist(lapply(Tlst, slot, "x"))
        }
        r@uplo <- ULs[1L]
    }
    else {
        if (any(sym)) 
            Tlst[sym] <- lapply(Tlst[sym], as, "generalMatrix")
        if (use.n) {
            r <- new("ngTMatrix")
        }
        else {
            r <- new(paste0(if (use.l) 
                "l"
            else "d", "gTMatrix"))
            r@x <- unlist(lapply(Tlst, slot, "x"))
        }
    }
    r@Dim <- c(i_off[nl + 1], j_off[nl + 1])
    r@i <- unlist(lapply(1:nl, function(k) Tlst[[k]]@i + i_off[k]))
    r@j <- unlist(lapply(1:nl, function(k) Tlst[[k]]@j + j_off[k]))
    r
}


sparseVector <- function (x, i, length) 
{
    newSpV(class = paste0(.V.kind(x), "sparseVector"), x = x, 
        i = i, length = length)
}


anyDuplicatedT <- function (x, di = dim(x)) 
anyDuplicated(.Call(m_encodeInd2, x@i, x@j, di, FALSE, FALSE))


colMeans <- function (x, na.rm = FALSE, dims = 1, ...) 
standardGeneric("colMeans")


T2graph <- function (from, need.uniq = is_not_uniqT(from), edgemode = NULL) 
{
    d <- dim(from)
    if (d[1] != d[2]) 
        stop("only square matrices can be used as incidence matrices for graphs")
    n <- d[1]
    if (n == 0) 
        return(new("graphNEL"))
    if (is.null(rn <- dimnames(from)[[1]])) 
        rn <- as.character(1:n)
    if (need.uniq) 
        from <- uniqTsparse(from)
    if (is.null(edgemode)) 
        edgemode <- if (isSymmetric(from)) {
            if (!is(from, "symmetricMatrix")) {
                from <- tril(from)
            }
            "undirected"
        }
        else {
            "directed"
        }
    ft1 <- cbind(rn[from@i + 1L], rn[from@j + 1L])
    graph::ftM2graphNEL(ft1, W = if (.hasSlot(from, "x")) 
        as.numeric(from@x), V = rn, edgemode = edgemode)
}


.__C__CHMfactor <- new("classRepresentation"
    , slots = structure(list(colcount = structure("integer", package = "methods"), 
    perm = structure("integer", package = "methods"), type = structure("integer", package = "methods"), 
    Dim = structure("integer", package = "methods")), .Names = c("colcount", 
"perm", "type", "Dim"))
    , contains = structure(list(CholeskyFactorization = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("CholeskyFactorization", 
"MatrixFactorization"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(CHMfactor_validate, object)
    , access = list()
    , className = structure("CHMfactor", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(CHMsuper = S4_object(), 
    CHMsimpl = S4_object(), 
    dCHMsuper = S4_object(), 
    nCHMsuper = S4_object(), 
    dCHMsimpl = S4_object(), 
    nCHMsimpl = S4_object()), .Names = c("CHMsuper", 
"CHMsimpl", "dCHMsuper", "nCHMsuper", "dCHMsimpl", "nCHMsimpl"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


chol2inv <- function (x, ...) 
standardGeneric("chol2inv")


.sparseDiagonal <- function (n, x = 1, uplo = "U", shape = if (missing(cols)) "t" else "g", 
    unitri, kind, cols = if (n) 0:(n - 1L) else integer(0)) 
{
    stopifnot(n == (n. <- as.integer(n)), (n <- n.) >= 0)
    if (!(mcols <- missing(cols))) 
        stopifnot(0 <= (cols <- as.integer(cols)), cols < n)
    m <- length(cols)
    if (missing(kind)) 
        kind <- if (is.double(x)) 
            "d"
        else if (is.logical(x)) 
            "l"
        else {
            storage.mode(x) <- "double"
            "d"
        }
    else stopifnot(any(kind == c("d", "l", "n")))
    stopifnot(is.character(shape), nchar(shape) == 1, any(shape == 
        c("t", "s", "g")))
    if ((missing(unitri) || unitri) && shape == "t" && (mcols || 
        cols == 0:(n - 1L)) && ((any(kind == c("l", "n")) && 
        allTrue(x)) || (kind == "d" && allTrue(x == 1)))) {
        new(paste0(kind, "tCMatrix"), Dim = c(n, n), uplo = uplo, 
            diag = "U", p = rep.int(0L, n + 1L))
    }
    else if (kind == "n") {
        if (shape == "g") 
            new("ngCMatrix", Dim = c(n, m), i = cols, p = 0:m)
        else new(paste0("n", shape, "CMatrix"), Dim = c(n, m), 
            uplo = uplo, i = cols, p = 0:m)
    }
    else {
        if ((lx <- length(x)) == 1) 
            x <- rep.int(x, m)
        else if (lx != m) 
            stop("length(x) must be either 1 or #{cols}")
        if (shape == "g") 
            new(paste0(kind, "gCMatrix"), Dim = c(n, m), x = x, 
                i = cols, p = 0:m)
        else new(paste0(kind, shape, "CMatrix"), Dim = c(n, m), 
            uplo = uplo, x = x, i = cols, p = 0:m)
    }
}


unpack <- function (x, ...) 
standardGeneric("unpack")


.__C__Matrix <- new("classRepresentation"
    , slots = structure(list(Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("Dim", 
"Dimnames"))
    , contains = structure(list(mMatrix = S4_object()), .Names = "mMatrix")
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (!isTRUE(r <- .Call(Dim_validate, object, "Matrix"))) 
        r
    else .Call(dimNames_validate, object)
}
    , access = list()
    , className = structure("Matrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(compMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    dMatrix = S4_object(), 
    iMatrix = S4_object(), 
    lMatrix = S4_object(), 
    nMatrix = S4_object(), 
    zMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    ddenseMatrix = S4_object(), 
    ldenseMatrix = S4_object(), 
    ndenseMatrix = S4_object(), 
    ddenseMatrix = S4_object(), 
    ldenseMatrix = S4_object(), 
    ndenseMatrix = S4_object(), 
    diagonalMatrix = S4_object(), 
    TsparseMatrix = S4_object(), 
    CsparseMatrix = S4_object(), 
    RsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    dgeMatrix = S4_object(), 
    dtrMatrix = S4_object(), 
    dtpMatrix = S4_object(), 
    dsyMatrix = S4_object(), 
    dspMatrix = S4_object(), 
    dpoMatrix = S4_object(), 
    dppMatrix = S4_object(), 
    lgeMatrix = S4_object(), 
    ltrMatrix = S4_object(), 
    ltpMatrix = S4_object(), 
    lsyMatrix = S4_object(), 
    lspMatrix = S4_object(), 
    ngeMatrix = S4_object(), 
    ntrMatrix = S4_object(), 
    ntpMatrix = S4_object(), 
    nsyMatrix = S4_object(), 
    nspMatrix = S4_object(), 
    ddiMatrix = S4_object(), 
    ldiMatrix = S4_object(), 
    corMatrix = S4_object(), 
    dgTMatrix = S4_object(), 
    dtTMatrix = S4_object(), 
    dsTMatrix = S4_object(), 
    dgCMatrix = S4_object(), 
    dtCMatrix = S4_object(), 
    dsCMatrix = S4_object(), 
    dgRMatrix = S4_object(), 
    dtRMatrix = S4_object(), 
    dsRMatrix = S4_object(), 
    lgTMatrix = S4_object(), 
    ltTMatrix = S4_object(), 
    lsTMatrix = S4_object(), 
    lgCMatrix = S4_object(), 
    ltCMatrix = S4_object(), 
    lsCMatrix = S4_object(), 
    lgRMatrix = S4_object(), 
    ltRMatrix = S4_object(), 
    lsRMatrix = S4_object(), 
    ngTMatrix = S4_object(), 
    ntTMatrix = S4_object(), 
    nsTMatrix = S4_object(), 
    ngCMatrix = S4_object(), 
    ntCMatrix = S4_object(), 
    nsCMatrix = S4_object(), 
    ngRMatrix = S4_object(), 
    ntRMatrix = S4_object(), 
    nsRMatrix = S4_object(), 
    indMatrix = S4_object(), 
    pMatrix = S4_object(), 
    Cholesky = S4_object(), 
    pCholesky = S4_object(), 
    BunchKaufman = S4_object(), 
    pBunchKaufman = S4_object()), .Names = c("compMatrix", 
"triangularMatrix", "dMatrix", "iMatrix", "lMatrix", "nMatrix", 
"zMatrix", "denseMatrix", "sparseMatrix", "generalMatrix", "symmetricMatrix", 
"ddenseMatrix", "ldenseMatrix", "ndenseMatrix", "ddenseMatrix", 
"ldenseMatrix", "ndenseMatrix", "diagonalMatrix", "TsparseMatrix", 
"CsparseMatrix", "RsparseMatrix", "dsparseMatrix", "lsparseMatrix", 
"nsparseMatrix", "dgeMatrix", "dtrMatrix", "dtpMatrix", "dsyMatrix", 
"dspMatrix", "dpoMatrix", "dppMatrix", "lgeMatrix", "ltrMatrix", 
"ltpMatrix", "lsyMatrix", "lspMatrix", "ngeMatrix", "ntrMatrix", 
"ntpMatrix", "nsyMatrix", "nspMatrix", "ddiMatrix", "ldiMatrix", 
"corMatrix", "dgTMatrix", "dtTMatrix", "dsTMatrix", "dgCMatrix", 
"dtCMatrix", "dsCMatrix", "dgRMatrix", "dtRMatrix", "dsRMatrix", 
"lgTMatrix", "ltTMatrix", "lsTMatrix", "lgCMatrix", "ltCMatrix", 
"lsCMatrix", "lgRMatrix", "ltRMatrix", "lsRMatrix", "ngTMatrix", 
"ntTMatrix", "nsTMatrix", "ngCMatrix", "ntCMatrix", "nsCMatrix", 
"ngRMatrix", "ntRMatrix", "nsRMatrix", "indMatrix", "pMatrix", 
"Cholesky", "pCholesky", "BunchKaufman", "pBunchKaufman"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__anyNA:base` <- "<environment>"

`diag<-` <- function (x, value) 
standardGeneric("diag<-")


.__C__corMatrix <- new("classRepresentation"
    , slots = structure(list(sd = structure("numeric", package = "methods"), 
    x = structure("numeric", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("sd", 
"x", "Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(dpoMatrix = S4_object(), 
    dsyMatrix = S4_object(), 
    ddenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dpoMatrix", 
"dsyMatrix", "ddenseMatrix", "symmetricMatrix", "dMatrix", "denseMatrix", 
"compMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    n <- object@Dim[2]
    if (length(sd <- object@sd) != n) 
        return("'sd' slot must be of length 'dim(.)[1]'")
    if (any(!is.finite(sd))) 
        return("'sd' slot has non-finite entries")
    if (any(sd < 0)) 
        return("'sd' slot has negative entries")
    TRUE
}
    , access = list()
    , className = structure("corMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__dsTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "x", "uplo", "factors"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"dsparseMatrix", "symmetricMatrix", "dMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tTMatrix_validate, object)
    , access = list()
    , className = structure("dsTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__lgCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "x", "factors"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    lCsparseMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"lsparseMatrix", "generalMatrix", "lCsparseMatrix", "lMatrix", 
"sparseMatrix", "compMatrix", "Matrix", "xMatrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(xCMatrix_validate, object)
    , access = list()
    , className = structure("lgCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__as.vector:base` <- "<environment>"

`.__T__rowMeans:base` <- "<environment>"

`.__T__as.matrix:base` <- "<environment>"

.__C__LU <- new("classRepresentation"
    , slots = structure(list(Dim = structure("integer", package = "methods")), .Names = "Dim")
    , contains = structure(list(MatrixFactorization = S4_object()), .Names = "MatrixFactorization")
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("LU", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(denseLU = S4_object(), 
    sparseLU = S4_object()), .Names = c("denseLU", 
"sparseLU"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__^:base` <- "<environment>"

`.__T__rowSums:base` <- "<environment>"

`.__T__as.numeric:base` <- "<environment>"

`.__T__length:base` <- "<environment>"

.__C__CholeskyFactorization <- new("classRepresentation"
    , slots = structure(list(Dim = structure("integer", package = "methods")), .Names = "Dim")
    , contains = structure(list(MatrixFactorization = S4_object()), .Names = "MatrixFactorization")
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("CholeskyFactorization", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(Cholesky = S4_object(), 
    pCholesky = S4_object(), 
    CHMfactor = S4_object(), 
    CHMsuper = S4_object(), 
    CHMsimpl = S4_object(), 
    dCHMsuper = S4_object(), 
    nCHMsuper = S4_object(), 
    dCHMsimpl = S4_object(), 
    nCHMsimpl = S4_object()), .Names = c("Cholesky", 
"pCholesky", "CHMfactor", "CHMsuper", "CHMsimpl", "dCHMsuper", 
"nCHMsuper", "dCHMsimpl", "nCHMsimpl"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


is.null.DN <- function (dn) 
{
    is.null(dn) || {
        if (!is.null(names(dn))) 
            names(dn) <- NULL
        ch0 <- character(0)
        identical(dn, list(NULL, NULL)) || identical(dn, list(ch0, 
            NULL)) || identical(dn, list(NULL, ch0)) || identical(dn, 
            list(ch0, ch0))
    }
}


norm <- function (x, type, ...) 
standardGeneric("norm")


Arith <- methods::Arith # re-exported from methods package

.__C__dtCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "x", "uplo", "diag"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    dCsparseMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"dsparseMatrix", "triangularMatrix", "dCsparseMatrix", "dMatrix", 
"sparseMatrix", "Matrix", "xMatrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tCMatrix_validate, object)
    , access = list()
    , className = structure("dtCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__qr.coef:base` <- "<environment>"

t <- function (x) 
standardGeneric("t")


`.__T__Cholesky:Matrix` <- "<environment>"

`.__T__solve:base` <- "<environment>"

`.__T__as.array:base` <- "<environment>"

.__C__dCHMsimpl <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    p = structure("integer", package = "methods"), i = structure("integer", package = "methods"), 
    nz = structure("integer", package = "methods"), nxt = structure("integer", package = "methods"), 
    prv = structure("integer", package = "methods"), colcount = structure("integer", package = "methods"), 
    perm = structure("integer", package = "methods"), type = structure("integer", package = "methods"), 
    Dim = structure("integer", package = "methods")), .Names = c("x", 
"p", "i", "nz", "nxt", "prv", "colcount", "perm", "type", "Dim"
))
    , contains = structure(list(CHMsimpl = S4_object(), 
    CHMfactor = S4_object(), 
    CholeskyFactorization = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("CHMsimpl", 
"CHMfactor", "CholeskyFactorization", "MatrixFactorization"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("dCHMsimpl", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


spMatrix <- function (nrow, ncol, i = integer(), j = integer(), x = numeric()) 
{
    dim <- c(as.integer(nrow), as.integer(ncol))
    kind <- .M.kind(x)
    new(paste0(kind, "gTMatrix"), Dim = dim, x = if (kind == 
        "d") 
        as.double(x)
    else x, i = as.integer(i - 1L), j = as.integer(j - 1L))
}


kronecker <- methods::kronecker # re-exported from methods package

`.__T__qr.resid:base` <- "<environment>"

invPerm <- function (p, zero.p = FALSE, zero.res = FALSE) 
.Call(inv_permutation, p, zero.p, zero.res)


.__C__ddenseMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dMatrix", 
"denseMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ddenseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dgeMatrix = S4_object(), 
    dtrMatrix = S4_object(), 
    dtpMatrix = S4_object(), 
    dsyMatrix = S4_object(), 
    dspMatrix = S4_object(), 
    dpoMatrix = S4_object(), 
    dppMatrix = S4_object(), 
    corMatrix = S4_object(), 
    Cholesky = S4_object(), 
    pCholesky = S4_object(), 
    BunchKaufman = S4_object(), 
    pBunchKaufman = S4_object()), .Names = c("dgeMatrix", 
"dtrMatrix", "dtpMatrix", "dsyMatrix", "dspMatrix", "dpoMatrix", 
"dppMatrix", "corMatrix", "Cholesky", "pCholesky", "BunchKaufman", 
"pBunchKaufman"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__which:base` <- "<environment>"

.__C__symmetricMatrix <- new("classRepresentation"
    , slots = structure(list(uplo = structure("character", package = "methods"), 
    factors = structure("list", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("uplo", 
"factors", "Dim", "Dimnames"))
    , contains = structure(list(compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("compMatrix", 
"Matrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(symmetricMatrix_validate, object)
    , access = list()
    , className = structure("symmetricMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dsyMatrix = S4_object(), 
    dspMatrix = S4_object(), 
    lsyMatrix = S4_object(), 
    lspMatrix = S4_object(), 
    nsyMatrix = S4_object(), 
    nspMatrix = S4_object(), 
    dsTMatrix = S4_object(), 
    dsCMatrix = S4_object(), 
    dsRMatrix = S4_object(), 
    lsTMatrix = S4_object(), 
    lsCMatrix = S4_object(), 
    lsRMatrix = S4_object(), 
    nsTMatrix = S4_object(), 
    nsCMatrix = S4_object(), 
    nsRMatrix = S4_object(), 
    dpoMatrix = S4_object(), 
    dppMatrix = S4_object(), 
    corMatrix = S4_object()), .Names = c("dsyMatrix", 
"dspMatrix", "lsyMatrix", "lspMatrix", "nsyMatrix", "nspMatrix", 
"dsTMatrix", "dsCMatrix", "dsRMatrix", "lsTMatrix", "lsCMatrix", 
"lsRMatrix", "nsTMatrix", "nsCMatrix", "nsRMatrix", "dpoMatrix", 
"dppMatrix", "corMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Compare <- methods::Compare # re-exported from methods package

graph2T <- function (from, use.weights = graph.has.weights(from) && graph.non.1.weights(from)) 
{
    nd <- graph::nodes(from)
    dnms <- list(nd, nd)
    dm <- rep.int(length(nd), 2)
    edge2i <- function(e) {
        rep.int(0:(dm[1] - 1L), lengths(e))
    }
    if (use.weights) {
        eWts <- graph::edgeWeights(from)
        names(eWts) <- NULL
        i <- edge2i(eWts)
        To <- unlist(lapply(eWts, names))
        j <- as.integer(match(To, nd)) - 1L
        new("dgTMatrix", i = i, j = j, x = unlist(eWts), Dim = dm, 
            Dimnames = dnms)
    }
    else {
        edges <- lapply(from@edgeL[nd], "[[", "edges")
        symm <- graph::edgemode(from) == "undirected"
        if (symm) 
            edges <- lapply(seq_along(edges), function(i) {
                e <- edges[[i]]
                e[e >= i]
            })
        i <- edge2i(edges)
        j <- as.integer(unlist(edges)) - 1L
        new(if (symm) 
            "nsTMatrix"
        else "ngTMatrix", i = i, j = j, Dim = dm, Dimnames = dnms)
    }
}


`.__T__crossprod:base` <- "<environment>"

rcond <- function (x, norm, ...) 
standardGeneric("rcond")


.__C__nMatrix <- new("classRepresentation"
    , slots = structure(list(Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("Dim", 
"Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(ndenseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    ngeMatrix = S4_object(), 
    ntrMatrix = S4_object(), 
    ntpMatrix = S4_object(), 
    nsyMatrix = S4_object(), 
    nspMatrix = S4_object()), .Names = c("ndenseMatrix", 
"nsparseMatrix", "ngeMatrix", "ntrMatrix", "ntpMatrix", "nsyMatrix", 
"nspMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__chol:base` <- "<environment>"

.__C__nsparseVector <- new("classRepresentation"
    , slots = structure(list(length = structure("numeric", package = "methods"), 
    i = structure("numeric", package = "methods")), .Names = c("length", 
"i"))
    , contains = structure(list(sparseVector = S4_object()), .Names = "sparseVector")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nsparseVector", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


abIseq <- function (from = 1, to = 1, by = ((to - from)/(length.out - 1)), 
    length.out = NULL, along.with = NULL) 
{
    if ((One <- nargs() == 1L) && !missing(from)) {
        lf <- length(from)
        return(if (mode(from) == "numeric" && lf == 1L) abIseq1(1L, 
            from) else if (lf) abIseq1(1L, lf) else new("abIndex"))
    }
    if (!missing(along.with)) {
        length.out <- length(along.with)
        if (One) 
            return(if (length.out) abIseq1(1L, length.out) else new("abIndex"))
    }
    else if (!missing(length.out)) 
        length.out <- ceiling(length.out)
    if (is.null(length.out)) 
        if (missing(by)) 
            abIseq1(from, to)
        else {
            del <- to - from
            if (del == 0 && to == 0) 
                return(as(to, "abIndex"))
            n <- del/by
            if (!(length(n) && is.finite(n))) {
                if (length(by) && by == 0 && length(del) && del == 
                  0) 
                  return(as(from, "abIndex"))
                stop("invalid (to - from)/by in seq(.)")
            }
            if (n < 0L) 
                stop("wrong sign in 'by' argument")
            if (n > .Machine$integer.max) 
                stop("'by' argument is much too small")
            dd <- abs(del)/max(abs(to), abs(from))
            if (dd < 100 * .Machine$double.eps) 
                return(from)
            n <- as.integer(n + 1e-07)
            x <- from + abIseq1(0L, n) * by
            if (by > 0) 
                pmin(x, to)
            else pmax(x, to)
        }
    else if (!is.finite(length.out) || length.out < 0L) 
        stop("length must be non-negative number")
    else if (length.out == 0L) 
        new("abIndex")
    else if (One) 
        abIseq1(1L, length.out)
    else if (missing(by)) {
        if (missing(to)) 
            to <- from + length.out - 1L
        if (missing(from)) 
            from <- to - length.out + 1L
        if (length.out > 2L) 
            if (from == to) 
                rep2abI(from, length.out)
            else c(as(from, "abIndex"), from + abIseq1(1L, length.out - 
                2L) * by, to)
        else as(c(from, to)[seq_len(length.out)], "abIndex")
    }
    else if (missing(to)) 
        from + abIseq1(0L, length.out - 1L) * by
    else if (missing(from)) 
        to - abIseq1(length.out - 1L, 0L) * by
    else stop("too many arguments")
}


as.matrix <- function (x, ...) 
standardGeneric("as.matrix")


lu <- function (x, ...) 
standardGeneric("lu")


`.__T__/:base` <- "<environment>"

`.__T__tail:utils` <- "<environment>"

`.__T__mean:base` <- "<environment>"

`.__T__isSymmetric:base` <- "<environment>"

symmpart <- function (x) 
standardGeneric("symmpart")


`%&%` <- function (x, y) 
standardGeneric("%&%")


`.__T__all.equal:base` <- "<environment>"

.diag2sT <- function (from, uplo = "U", kind = .M.kind(from)) 
{
    n <- from@Dim[1]
    i <- seq_len(n) - 1L
    new(paste0(kind, "sTMatrix"), Dim = from@Dim, Dimnames = from@Dimnames, 
        i = i, j = i, uplo = uplo, x = if (from@diag == "N") 
            from@x
        else rep.int(switch(kind, d = 1, l = , n = TRUE, stop(gettextf("%s kind not yet implemented", 
            sQuote(kind)), domain = NA)), n))
}


bdiag <- function (...) 
{
    if ((nA <- nargs()) == 0) 
        return(new("dgCMatrix"))
    if (nA == 1 && !is.list(...)) 
        return(as(..., "CsparseMatrix"))
    alis <- if (nA == 1 && is.list(..1)) 
        ..1
    else list(...)
    if (length(alis) == 1) 
        return(as(alis[[1]], "CsparseMatrix"))
    as(.bdiag(alis), "CsparseMatrix")
}


updown <- function (update, C, L) 
standardGeneric("updown")


Math2 <- methods::Math2 # re-exported from methods package

`.__T__%*%:base` <- "<environment>"

.__C__Schur <- new("classRepresentation"
    , slots = structure(list(T = structure("Matrix", package = "Matrix"), Q = structure("Matrix", package = "Matrix"), 
    EValues = structure("number", package = "Matrix"), Dim = structure("integer", package = "methods")), .Names = c("T", 
"Q", "EValues", "Dim"))
    , contains = structure(list(MatrixFactorization = S4_object()), .Names = "MatrixFactorization")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    dim <- object@Dim
    if ((n <- dim[1]) != dim[2]) 
        "'Dim' slot is not (n,n)"
    else if (any(dim(object@T) != n)) 
        "'dim(T)' is incorrect"
    else if (any(dim(object@Q) != n)) 
        "'dim(Q)' is incorrect"
    else if (length(object@EValues) != n) 
        "'EValues' is not of correct length"
    else TRUE
}
    , access = list()
    , className = structure("Schur", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__symmpart:Matrix` <- "<environment>"

.__C__lgRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "x", "factors"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"lsparseMatrix", "generalMatrix", "lMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(xRMatrix_validate, object)
    , access = list()
    , className = structure("lgRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


condest <- function (A, t = min(n, 5), normA = norm(A, "1"), silent = FALSE, 
    quiet = TRUE) 
{
    if (length(d <- dim(A)) != 2 || (n <- d[1]) != d[2]) 
        stop("'A' must be a square matrix")
    luA <- lu(A)
    i.n <- seq_len(n)
    isSparse <- is(A, "sparseMatrix")
    if (isSparse) {
        q. <- q.i <- luA@q + 1L
        q.i[q.i] <- i.n
        p. <- p.i <- luA@p + 1L
        p.i[p.i] <- i.n
        Ut <- t(luA@U)
        Lt <- t(luA@L)
        f.solve <- function(x) solve(luA@U, solve(luA@L, x[p., 
            ]))[q.i, ]
        f.solve_t <- function(x) solve(Lt, solve(Ut, x[q., ]))[p.i, 
            ]
    }
    else {
        e.A <- expand(luA)
        p. <- p.i <- luA@perm
        p.i[p.i] <- i.n
        Ut <- t(e.A$U)
        Lt <- t(e.A$L)
        f.solve <- function(x) solve(e.A$U, solve(e.A$L, x[p., 
            ]))
        f.solve_t <- function(x) solve(Lt, solve(Ut, x))[p.i, 
            ]
    }
    n1.res <- onenormest(A.x = f.solve, At.x = f.solve_t, t = t, 
        n = n, quiet = quiet, silent = silent)
    w <- n1.res[["w"]]
    list(est = normA * n1.res[["est"]], v = w/sum(abs(w)))
}


`.__T__rbind2:methods` <- "<environment>"

.dense2sy <- function (from, ...) 
{
    if (isSymmetric(from, ...)) 
        .Call(dense_to_symmetric, from, "U", FALSE)
    else stop("not a symmetric matrix; consider forceSymmetric() or symmpart()")
}


.T2Cmat <- function (from, isTri = is(from, "triangularMatrix")) 
.Call(Tsparse_to_Csparse, from, isTri)


cBind <- function (..., deparse.level = 1) 
{
    if (isTRUE(getOption("Matrix.warn")) || isTRUE(getOption("Matrix.verbose"))) 
        .Deprecated(msg = "'cBind' is deprecated.\n Since R version 3.2.0, base's cbind() should work fine with S4 objects")
    base::cbind(..., deparse.level = deparse.level)
}


.__C__pCholesky <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(dtpMatrix = S4_object(), 
    CholeskyFactorization = S4_object(), 
    ddenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    MatrixFactorization = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dtpMatrix", 
"CholeskyFactorization", "ddenseMatrix", "triangularMatrix", 
"MatrixFactorization", "dMatrix", "denseMatrix", "Matrix", "mMatrix", 
"xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("pCholesky", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.validateCsparse <- function (x, sort.if.needed = FALSE) 
.Call(Csparse_validate2, x, sort.if.needed)


`.__T__colMeans:base` <- "<environment>"

.__C__replValue <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = numeric(0)
    , validity = NULL
    , access = list()
    , className = structure("replValue", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(numeric = S4_object(), 
    logical = S4_object(), 
    complex = S4_object(), 
    raw = S4_object(), 
    integer = S4_object(), 
    factor = S4_object()), .Names = c("numeric", 
"logical", "complex", "raw", "integer", "factor"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


qr.fitted <- function (qr, y, k = qr$rank) 
standardGeneric("qr.fitted")


printSpMatrix2 <- function (x, digits = NULL, maxp = getOption("max.print"), zero.print = ".", 
    col.names, note.dropping.colnames = TRUE, uniDiag = TRUE, 
    suppRows = NULL, suppCols = NULL, col.trailer = if (suppCols) "......" else "", 
    align = c("fancy", "right"), width = getOption("width"), 
    fitWidth = TRUE) 
{
    d <- dim(x)
    cl <- class(x)
    cld <- getClassDef(cl)
    xtra <- if (extends(cld, "triangularMatrix") && x@diag == 
        "U") 
        " (unitriangular)"
    else ""
    cat(sprintf("%d x %d sparse Matrix of class \"%s\"%s\n", 
        d[1], d[2], cl, xtra))
    setW <- !missing(width) && width > getOption("width")
    if (setW) {
        op <- options(width = width)
        on.exit(options(op))
    }
    if ((identical(suppRows, FALSE) && identical(suppCols, FALSE)) || 
        (!isTRUE(suppRows) && !isTRUE(suppCols) && prod(d) <= 
            maxp)) {
        if (missing(col.trailer) && is.null(suppCols)) 
            suppCols <- FALSE
        printSpMatrix(x, cld = cld, digits = digits, maxp = maxp, 
            zero.print = zero.print, col.names = col.names, note.dropping.colnames = note.dropping.colnames, 
            uniDiag = uniDiag, col.trailer = col.trailer, align = align)
    }
    else {
        validObject(x)
        sTxt <- c(" ", gettext("in show(); maybe adjust 'options(max.print= *, width = *)'"), 
            "\n ..............................\n")
        useW <- width - (format.info(d[1], digits = digits)[1] + 
            3 + 1)
        if (is.null(suppCols)) 
            suppCols <- (d[2] * 2 > useW)
        nCc <- 1 + nchar(col.trailer, "width")
        if (suppCols) {
            nc <- (useW - nCc)%/%2
            x <- x[, 1:nc, drop = FALSE]
        }
        else nc <- d[2]
        nr <- maxp%/%nc
        if (is.null(suppRows)) 
            suppRows <- (nr < d[1])
        if (suppRows) {
            n2 <- ceiling(nr/2)
            if (fitWidth) {
                cM <- formatSpMatrix(x[seq_len(min(d[1], max(1, 
                  n2))), , drop = FALSE], digits = digits, maxp = maxp, 
                  zero.print = zero.print, col.names = col.names, 
                  align = align, note.dropping.colnames = note.dropping.colnames, 
                  uniDiag = FALSE)
                matW <- nchar(capture.output(print(cM, quote = FALSE, 
                  right = FALSE))[[1]])
                needW <- matW + (if (suppCols) 
                  nCc
                else 0)
                if (needW > useW) {
                  op <- options(width = width + (needW - useW))
                  if (!setW) 
                    on.exit(options(op))
                }
            }
            printSpMatrix(x[seq_len(min(d[1], max(1, n2))), , 
                drop = FALSE], digits = digits, maxp = maxp, 
                zero.print = zero.print, col.names = col.names, 
                note.dropping.colnames = note.dropping.colnames, 
                uniDiag = uniDiag, col.trailer = col.trailer, 
                align = align)
            suppTxt <- gettext(if (suppCols) 
                "suppressing columns and rows"
            else "suppressing rows")
            cat("\n ..............................", "\n ........", 
                suppTxt, sTxt, "\n", sep = "")
            printSpMatrix(tail(x, max(1, nr - n2)), digits = digits, 
                maxp = maxp, zero.print = zero.print, col.names = col.names, 
                note.dropping.colnames = note.dropping.colnames, 
                uniDiag = FALSE, col.trailer = col.trailer, align = align)
        }
        else if (suppCols) {
            printSpMatrix(x[, 1:nc, drop = FALSE], digits = digits, 
                maxp = maxp, zero.print = zero.print, col.names = col.names, 
                note.dropping.colnames = note.dropping.colnames, 
                uniDiag = uniDiag, col.trailer = col.trailer, 
                align = align)
            cat("\n .....", gettext("suppressing columns"), sTxt, 
                sep = "")
        }
        else stop("logic programming error in printSpMatrix2(), please report")
        invisible(x)
    }
}


format <- function (x, ...) 
standardGeneric("format")


diagN2U <- function (x, cl = getClassDef(class(x)), checkDense = FALSE) 
{
    if (!(extends(cl, "triangularMatrix") && x@diag == "N")) 
        return(x)
    if (checkDense && extends(cl, "denseMatrix")) {
        .dense.diagN2U(x)
    }
    else .Call(Csparse_diagN2U, as(x, "CsparseMatrix"))
}


`.__T__Schur:Matrix` <- "<environment>"

.__C__ltpMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(ldenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    lMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ldenseMatrix", 
"triangularMatrix", "lMatrix", "denseMatrix", "Matrix", "mMatrix", 
"xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ltpMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


tail <- function (x, ...) 
standardGeneric("tail")


.__C__sparseMatrix <- new("classRepresentation"
    , slots = structure(list(Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("Dim", 
"Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("sparseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(diagonalMatrix = S4_object(), 
    TsparseMatrix = S4_object(), 
    CsparseMatrix = S4_object(), 
    RsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    indMatrix = S4_object(), 
    ddiMatrix = S4_object(), 
    ldiMatrix = S4_object(), 
    dgTMatrix = S4_object(), 
    dtTMatrix = S4_object(), 
    dsTMatrix = S4_object(), 
    lgTMatrix = S4_object(), 
    ltTMatrix = S4_object(), 
    lsTMatrix = S4_object(), 
    ngTMatrix = S4_object(), 
    ntTMatrix = S4_object(), 
    nsTMatrix = S4_object(), 
    dgCMatrix = S4_object(), 
    dtCMatrix = S4_object(), 
    dsCMatrix = S4_object(), 
    lgCMatrix = S4_object(), 
    ltCMatrix = S4_object(), 
    lsCMatrix = S4_object(), 
    ngCMatrix = S4_object(), 
    ntCMatrix = S4_object(), 
    nsCMatrix = S4_object(), 
    dgRMatrix = S4_object(), 
    dtRMatrix = S4_object(), 
    dsRMatrix = S4_object(), 
    lgRMatrix = S4_object(), 
    ltRMatrix = S4_object(), 
    lsRMatrix = S4_object(), 
    ngRMatrix = S4_object(), 
    ntRMatrix = S4_object(), 
    nsRMatrix = S4_object(), 
    dgTMatrix = S4_object(), 
    dtTMatrix = S4_object(), 
    dsTMatrix = S4_object(), 
    dgCMatrix = S4_object(), 
    dtCMatrix = S4_object(), 
    dsCMatrix = S4_object(), 
    dgRMatrix = S4_object(), 
    dtRMatrix = S4_object(), 
    dsRMatrix = S4_object(), 
    lgTMatrix = S4_object(), 
    ltTMatrix = S4_object(), 
    lsTMatrix = S4_object(), 
    lgCMatrix = S4_object(), 
    ltCMatrix = S4_object(), 
    lsCMatrix = S4_object(), 
    lgRMatrix = S4_object(), 
    ltRMatrix = S4_object(), 
    lsRMatrix = S4_object(), 
    ngTMatrix = S4_object(), 
    ntTMatrix = S4_object(), 
    nsTMatrix = S4_object(), 
    ngCMatrix = S4_object(), 
    ntCMatrix = S4_object(), 
    nsCMatrix = S4_object(), 
    ngRMatrix = S4_object(), 
    ntRMatrix = S4_object(), 
    nsRMatrix = S4_object(), 
    pMatrix = S4_object()), .Names = c("diagonalMatrix", 
"TsparseMatrix", "CsparseMatrix", "RsparseMatrix", "dsparseMatrix", 
"lsparseMatrix", "nsparseMatrix", "indMatrix", "ddiMatrix", "ldiMatrix", 
"dgTMatrix", "dtTMatrix", "dsTMatrix", "lgTMatrix", "ltTMatrix", 
"lsTMatrix", "ngTMatrix", "ntTMatrix", "nsTMatrix", "dgCMatrix", 
"dtCMatrix", "dsCMatrix", "lgCMatrix", "ltCMatrix", "lsCMatrix", 
"ngCMatrix", "ntCMatrix", "nsCMatrix", "dgRMatrix", "dtRMatrix", 
"dsRMatrix", "lgRMatrix", "ltRMatrix", "lsRMatrix", "ngRMatrix", 
"ntRMatrix", "nsRMatrix", "dgTMatrix", "dtTMatrix", "dsTMatrix", 
"dgCMatrix", "dtCMatrix", "dsCMatrix", "dgRMatrix", "dtRMatrix", 
"dsRMatrix", "lgTMatrix", "ltTMatrix", "lsTMatrix", "lgCMatrix", 
"ltCMatrix", "lsCMatrix", "lgRMatrix", "ltRMatrix", "lsRMatrix", 
"ngTMatrix", "ntTMatrix", "nsTMatrix", "ngCMatrix", "ntCMatrix", 
"nsCMatrix", "ngRMatrix", "ntRMatrix", "nsRMatrix", "pMatrix"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__&:base` <- "<environment>"

`.__T__lu:Matrix` <- "<environment>"

`.__T__%/%:base` <- "<environment>"

`.__T__Ops:base` <- "<environment>"

.diag2tT <- function (from, uplo = "U", kind = .M.kind(from)) 
{
    i <- if (from@diag == "U") 
        integer(0)
    else seq_len(from@Dim[1]) - 1L
    new(paste0(kind, "tTMatrix"), diag = from@diag, Dim = from@Dim, 
        Dimnames = from@Dimnames, uplo = uplo, x = from@x, i = i, 
        j = i)
}


.__C__dgCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "x", "factors"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    dCsparseMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"dsparseMatrix", "generalMatrix", "dCsparseMatrix", "dMatrix", 
"sparseMatrix", "compMatrix", "Matrix", "xMatrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(xCMatrix_validate, object)
    , access = list()
    , className = structure("dgCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__determinant:base` <- "<environment>"

`.__T__facmul:Matrix` <- "<environment>"

.__C__lsyMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(ldenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    lMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ldenseMatrix", 
"symmetricMatrix", "lMatrix", "denseMatrix", "compMatrix", "Matrix", 
"mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dense_nonpacked_validate, object)
    , access = list()
    , className = structure("lsyMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Matrix <- function (data = NA, nrow = 1, ncol = 1, byrow = FALSE, dimnames = NULL, 
    sparse = NULL, doDiag = TRUE, forceCheck = FALSE) 
{
    i.M <- is(data, "Matrix")
    sM <- FALSE
    if (i.M) {
        if (is(data, "diagonalMatrix")) 
            return(data)
        sV <- FALSE
    }
    else if (inherits(data, "table")) 
        class(data) <- "matrix"
    else if (is(data, "sparseVector")) {
        data <- spV2M(data, nrow, ncol, byrow = byrow)
        i.M <- sparse <- forceCheck <- sM <- sV <- TRUE
    }
    if (is.null(sparse1 <- sparse) && (i.M || is(data, "matrix"))) 
        sparse <- sparseDefault(data)
    doDN <- TRUE
    i.m <- is.matrix(data)
    if (i.M) {
        if (!sV) {
            if (!missing(nrow) || !missing(ncol) || !missing(byrow)) 
                warning("'nrow', 'ncol', etc, are disregarded when 'data' is \"Matrix\" already")
            sM <- is(data, "sparseMatrix")
            if (!forceCheck && ((sparse && sM) || (!sparse && 
                !sM))) 
                return(data)
        }
    }
    else if (!i.m) {
        if (is.object(data) || !is.atomic(data)) 
            data <- as.vector(data)
        if (length(data) == 1 && is0(data) && !identical(sparse, 
            FALSE)) {
            if (is.null(sparse)) 
                sparse1 <- sparse <- TRUE
            i.M <- sM <- TRUE
            if (missing(nrow)) 
                nrow <- ceiling(1/ncol)
            else if (missing(ncol)) 
                ncol <- ceiling(1/nrow)
            isSym <- nrow == ncol
            data <- new(paste0(if (is.numeric(data)) 
                "d"
            else if (is.logical(data)) 
                "l"
            else stop("invalid 'data'"), if (isSym) 
                "s"
            else "g", "CMatrix"), p = rep.int(0L, ncol + 1L), 
                Dim = as.integer(c(nrow, ncol)), Dimnames = if (is.null.DN(dimnames)) 
                  list(NULL, NULL)
                else dimnames)
        }
        else {
            data <- .External(Mmatrix, data, nrow, ncol, byrow, 
                dimnames, missing(nrow), missing(ncol))
            if (is.null(sparse)) 
                sparse <- sparseDefault(data)
        }
        doDN <- FALSE
    }
    else if (!missing(nrow) || !missing(ncol) || !missing(byrow)) 
        warning("'nrow', 'ncol', etc, are disregarded for matrix 'data'")
    if (doDN && !is.null(dimnames)) 
        dimnames(data) <- dimnames
    isSym <- isSymmetric(data)
    if ((isTri <- !isSym)) 
        isTri <- isTriangular(data)
    isDiag <- isSym
    if (isDiag) 
        isDiag <- doDiag && !isTRUE(sparse1) && nrow(data) > 
            1 && isDiagonal(data)
    if (isDiag) {
        data <- as(data, "diagonalMatrix")
        isSym <- FALSE
    }
    else if (sparse && !sM) 
        data <- as(data, "sparseMatrix")
    else if (!sparse) {
        if (i.M) {
            if (!is(data, "denseMatrix")) 
                data <- as(data, "denseMatrix")
        }
        else {
            ctype <- typeof(data)
            if (ctype == "complex") 
                stop("complex matrices not yet implemented in Matrix package")
            if (ctype == "integer") 
                storage.mode(data) <- "double"
            data <- new(paste0(.M.kind(data), "geMatrix"), Dim = dim(data), 
                Dimnames = .M.DN(data), x = c(data))
        }
    }
    if (isTri && !is(data, "triangularMatrix")) {
        data <- if (attr(isTri, "kind") == "L") 
            tril(data)
        else triu(data)
    }
    else if (isSym && !is(data, "symmetricMatrix")) 
        data <- forceSymmetric(data)
    data
}


.__C__index <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = numeric(0)
    , validity = NULL
    , access = list()
    , className = structure("index", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(numeric = S4_object(), 
    logical = S4_object(), 
    character = S4_object(), 
    integer = S4_object(), 
    signature = S4_object(), 
    className = S4_object(), 
    ObjectsWithPackage = S4_object(), 
    factor = S4_object()), .Names = c("numeric", 
"logical", "character", "integer", "signature", "className", 
"ObjectsWithPackage", "factor"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__lgeMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "factors"))
    , contains = structure(list(ldenseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    geMatrix = S4_object(), 
    lMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("ldenseMatrix", 
"generalMatrix", "geMatrix", "lMatrix", "denseMatrix", "compMatrix", 
"Matrix", "xMatrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dense_nonpacked_validate, object)
    , access = list()
    , className = structure("lgeMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__ltRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "x", "uplo", "diag"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"lsparseMatrix", "triangularMatrix", "lMatrix", "sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tRMatrix_validate, object)
    , access = list()
    , className = structure("ltRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__any:base` <- "<environment>"

diff <- function (x, ...) 
standardGeneric("diff")


.__C__dCHMsuper <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    super = structure("integer", package = "methods"), pi = structure("integer", package = "methods"), 
    px = structure("integer", package = "methods"), s = structure("integer", package = "methods"), 
    colcount = structure("integer", package = "methods"), perm = structure("integer", package = "methods"), 
    type = structure("integer", package = "methods"), Dim = structure("integer", package = "methods")), .Names = c("x", 
"super", "pi", "px", "s", "colcount", "perm", "type", "Dim"))
    , contains = structure(list(CHMsuper = S4_object(), 
    CHMfactor = S4_object(), 
    CholeskyFactorization = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("CHMsuper", 
"CHMfactor", "CholeskyFactorization", "MatrixFactorization"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("dCHMsuper", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


formatSpMatrix <- function (x, digits = NULL, maxp = 1e+09, cld = getClassDef(class(x)), 
    zero.print = ".", col.names, note.dropping.colnames = TRUE, 
    uniDiag = TRUE, align = c("fancy", "right")) 
{
    stopifnot(extends(cld, "sparseMatrix"))
    validObject(x)
    d <- dim(x)
    unitD <- extends(cld, "triangularMatrix") && x@diag == "U"
    if (unitD) {
        if (extends(cld, "CsparseMatrix")) 
            x <- .Call(Csparse_diagU2N, x)
        else if (extends(cld, "TsparseMatrix")) 
            x <- .Call(Tsparse_diagU2N, x)
        else {
            kind <- .M.kind(x, cld)
            x <- .Call(Tsparse_diagU2N, as(as(x, paste0(kind, 
                "Matrix")), "TsparseMatrix"))
            cld <- getClassDef(class(x))
        }
    }
    if (prod(d) > maxp) {
        nr <- maxp%/%d[2]
        m <- as(x[1:max(1, nr), , drop = FALSE], "matrix")
    }
    else {
        m <- as(x, "matrix")
    }
    dn <- dimnames(m)
    binary <- extends(cld, "nsparseMatrix") || extends(cld, "indMatrix")
    logi <- binary || extends(cld, "lsparseMatrix")
    cx <- .formatSparseSimple(m, asLogical = logi, digits = digits, 
        col.names = col.names, note.dropping.colnames = note.dropping.colnames, 
        dn = dn)
    if (is.logical(zero.print)) 
        zero.print <- if (zero.print) 
            "0"
        else " "
    if (binary) {
        cx[!m] <- zero.print
        cx[m] <- "|"
    }
    else {
        d <- dim(cx)
        ne <- length(iN0 <- 1L + .Call(m_encodeInd, non0ind(x, 
            cld), di = d, FALSE, FALSE))
        if (0 < ne && (logi || ne < prod(d))) {
            cx <- formatSparseM(x, zero.print, align, m = m, 
                asLogical = logi, uniDiag = unitD & uniDiag, 
                digits = digits, cx = cx, iN0 = iN0, dn = dn)
        }
        else if (ne == 0) 
            cx[] <- zero.print
    }
    cx
}


.__C__lsCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "x", "uplo", "factors"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    lCsparseMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"lsparseMatrix", "symmetricMatrix", "lCsparseMatrix", "lMatrix", 
"sparseMatrix", "compMatrix", "Matrix", "xMatrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(xCMatrix_validate, object)
    , access = list()
    , className = structure("lsCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__lspMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(ldenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    lMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ldenseMatrix", 
"symmetricMatrix", "lMatrix", "denseMatrix", "compMatrix", "Matrix", 
"mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dspMatrix_validate, object)
    , access = list()
    , className = structure("lspMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__ndenseMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(nMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    xMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("nMatrix", 
"denseMatrix", "xMatrix", "Matrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ndenseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(ngeMatrix = S4_object(), 
    ntrMatrix = S4_object(), 
    ntpMatrix = S4_object(), 
    nsyMatrix = S4_object(), 
    nspMatrix = S4_object()), .Names = c("ngeMatrix", 
"ntrMatrix", "ntpMatrix", "nsyMatrix", "nspMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


bandSparse <- function (n, m = n, k, diagonals, symmetric = FALSE, giveCsparse = TRUE) 
{
    if (use.x <- !missing(diagonals)) 
        diag.isMat <- is.matrix(diagonals)
    len.k <- length(k)
    stopifnot(!use.x || is.list(diagonals) || diag.isMat, k == 
        as.integer(k), n == as.integer(n), m == as.integer(m))
    k <- as.integer(k)
    n <- as.integer(n)
    m <- as.integer(m)
    stopifnot(n >= 0, m >= 0, -n + 1 <= k, k <= m - 1)
    if (use.x) {
        if (diag.isMat) {
            if (ncol(diagonals) != len.k) 
                stop(gettextf("'diagonals' matrix must have %d columns (= length(k) )", 
                  len.k), domain = NA)
            getD <- function(j) diagonals[, j]
        }
        else {
            if (length(diagonals) != len.k) 
                stop(gettextf("'diagonals' must have the same length (%d) as 'k'", 
                  len.k), domain = NA)
            getD <- function(j) diagonals[[j]]
        }
    }
    if (symmetric && any(k < 0) && any(k > 0)) 
        stop("for symmetric band matrix, only specify upper or lower triangle\n hence, all k must have the same sign")
    dims <- c(n, m)
    k.lengths <- if (n >= m) {
        ifelse(k >= m - n, m - pmax(0, k), n + k)
    }
    else {
        ifelse(k >= -n + 1, n + pmin(0, k), m - k)
    }
    i <- j <- integer(sum(k.lengths))
    if (use.x) 
        x <- if (len.k > 0) 
            rep.int(getD(1)[1], length(i))
    off.i <- 0L
    for (s in seq_len(len.k)) {
        kk <- k[s]
        l.kk <- k.lengths[s]
        ii1 <- seq_len(l.kk)
        ind <- ii1 + off.i
        if (kk >= 0) {
            i[ind] <- ii1
            j[ind] <- ii1 + kk
        }
        else {
            i[ind] <- ii1 - kk
            j[ind] <- ii1
        }
        if (use.x) {
            xx <- getD(s)
            if (length(xx) < l.kk) 
                warning(gettextf("the %d-th (sub)-diagonal (k = %d) is too short; filling with NA's", 
                  s, kk), domain = NA)
            x[ind] <- xx[ii1]
        }
        off.i <- off.i + l.kk
    }
    if (symmetric) {
        UpLo <- if (min(k) >= 0) 
            "U"
        else "L"
        T <- if (use.x) {
            if (is.integer(x)) 
                x <- as.double(x)
            cc <- paste0(.M.kind(x), "sTMatrix")
            new(cc, i = i - 1L, j = j - 1L, x = x, Dim = dims, 
                uplo = UpLo)
        }
        else new("nsTMatrix", i = i - 1L, j = j - 1L, Dim = dims, 
            uplo = UpLo)
        if (giveCsparse) 
            as(T, "CsparseMatrix")
        else T
    }
    else {
        if (use.x) 
            sparseMatrix(i = i, j = j, x = x, dims = dims, giveCsparse = giveCsparse)
        else sparseMatrix(i = i, j = j, dims = dims, giveCsparse = giveCsparse)
    }
}


.__C__lMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"xMatrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("lMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(ldenseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    ldiMatrix = S4_object(), 
    lgeMatrix = S4_object(), 
    ltrMatrix = S4_object(), 
    ltpMatrix = S4_object(), 
    lsyMatrix = S4_object(), 
    lspMatrix = S4_object()), .Names = c("ldenseMatrix", 
"lsparseMatrix", "ldiMatrix", "lgeMatrix", "ltrMatrix", "ltpMatrix", 
"lsyMatrix", "lspMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


readHB <- function (file) 
{
    if (is.character(file)) 
        file <- if (file == "") 
            stdin()
        else file(file)
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file)
        on.exit(close(file))
    }
    hdr <- readLines(file, 4, ok = FALSE)
    ptrln <- as.integer(substr(hdr[2], 15, 28))
    indln <- as.integer(substr(hdr[2], 29, 42))
    valln <- as.integer(substr(hdr[2], 43, 56))
    rhsln <- as.integer(substr(hdr[2], 57, 70))
    if (!(t1 <- substr(hdr[3], 1, 1)) %in% c("C", "R", "P")) 
        stop(gettextf("Invalid storage type: %s", t1), domain = NA)
    if (t1 != "R") 
        stop("Only numeric sparse matrices allowed")
    if (!(t2 <- substr(hdr[3], 2, 2)) %in% c("H", "R", "S", "U", 
        "Z")) 
        stop(gettextf("Invalid storage format: %s", t2), domain = NA)
    if (!(t3 <- substr(hdr[3], 3, 3)) %in% c("A", "E")) 
        stop(gettextf("Invalid assembled indicator: %s", t3), 
            domain = NA)
    nr <- as.integer(substr(hdr[3], 15, 28))
    nc <- as.integer(substr(hdr[3], 29, 42))
    nz <- as.integer(substr(hdr[3], 43, 56))
    ptrfmt <- toupper(sub("[[:space:]]+$", "", substr(hdr[4], 
        1, 16)))
    indfmt <- toupper(sub("[[:space:]]+$", "", substr(hdr[4], 
        17, 32)))
    valfmt <- toupper(sub("[[:space:]]+$", "", substr(hdr[4], 
        33, 52)))
    if (!is.na(rhsln) && rhsln > 0) 
        readLines(file, 1, ok = FALSE)
    ptr <- readmany(file, ptrln, nc + 1, ptrfmt, as.integer)
    ind <- readmany(file, indln, nz, indfmt, as.integer)
    vals <- readmany(file, valln, nz, valfmt, as.numeric)
    if (t2 == "S") 
        new("dsCMatrix", uplo = "L", p = ptr - 1L, i = ind - 
            1L, x = vals, Dim = c(nr, nc))
    else new("dgCMatrix", p = ptr - 1L, i = ind - 1L, x = vals, 
        Dim = c(nr, nc))
}


coerce <- methods::coerce # re-exported from methods package

.diagU2N <- function (x, cl, checkDense = FALSE) 
{
    if (extends(cl, "CsparseMatrix")) 
        .Call(Csparse_diagU2N, x)
    else if (extends(cl, "TsparseMatrix")) 
        .Call(Tsparse_diagU2N, x)
    else {
        kind <- .M.kind(x, cl)
        if (checkDense && extends(cl, "denseMatrix")) {
            .dense.diagU2N(x, kind)
        }
        else {
            .Call(Tsparse_diagU2N, as(as(x, paste0(kind, "Matrix")), 
                "TsparseMatrix"))
        }
    }
}


Summary <- methods::Summary # re-exported from methods package

solve <- function (a, b, ...) 
standardGeneric("solve")


.__C__sparseLU <- new("classRepresentation"
    , slots = structure(list(L = structure("dtCMatrix", package = "Matrix"), 
    U = structure("dtCMatrix", package = "Matrix"), p = structure("integer", package = "methods"), 
    q = structure("integer", package = "methods"), Dim = structure("integer", package = "methods")), .Names = c("L", 
"U", "p", "q", "Dim"))
    , contains = structure(list(LU = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("LU", 
"MatrixFactorization"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("sparseLU", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


isSymmetric <- function (object, ...) 
standardGeneric("isSymmetric")


`.__T__show:methods` <- "<environment>"

rowMeans <- function (x, na.rm = FALSE, dims = 1, ...) 
standardGeneric("rowMeans")


.__C__lsRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("logical", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "x", "uplo", "factors"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    lsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"lsparseMatrix", "symmetricMatrix", "lMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tRMatrix_validate, object)
    , access = list()
    , className = structure("lsRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__print:base` <- "<environment>"

Math <- methods::Math # re-exported from methods package

`.__T__drop:base` <- "<environment>"

image <- function (x, ...) 
standardGeneric("image")


drop0 <- function (x, tol = 0, is.Csparse = NA) 
{
    .Call(Csparse_drop, if (isTRUE(is.Csparse) || is.na(is.Csparse) && 
        is(x, "CsparseMatrix")) x else as(x, "CsparseMatrix"), 
        tol)
}


Ops <- methods::Ops # re-exported from methods package

.__C__nCHMsimpl <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    i = structure("integer", package = "methods"), nz = structure("integer", package = "methods"), 
    nxt = structure("integer", package = "methods"), prv = structure("integer", package = "methods"), 
    colcount = structure("integer", package = "methods"), perm = structure("integer", package = "methods"), 
    type = structure("integer", package = "methods"), Dim = structure("integer", package = "methods")), .Names = c("p", 
"i", "nz", "nxt", "prv", "colcount", "perm", "type", "Dim"))
    , contains = structure(list(CHMsimpl = S4_object(), 
    CHMfactor = S4_object(), 
    CholeskyFactorization = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("CHMsimpl", 
"CHMfactor", "CholeskyFactorization", "MatrixFactorization"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nCHMsimpl", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


determinant <- function (x, logarithm = TRUE, ...) 
standardGeneric("determinant")


.symDiagonal <- function (n, x = rep.int(1, n), uplo = "U") 
.sparseDiagonal(n, x, uplo, shape = "s")


.__C__iMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("integer", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"xMatrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("iMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__ldiMatrix <- new("classRepresentation"
    , slots = structure(list(diag = structure("character", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    x = structure("logical", package = "methods")), .Names = c("diag", 
"Dim", "Dimnames", "x"))
    , contains = structure(list(diagonalMatrix = S4_object(), 
    lMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("diagonalMatrix", 
"lMatrix", "sparseMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ldiMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__ngTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), factors = structure("list", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "factors"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"nsparseMatrix", "generalMatrix", "nMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ngTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__qr.qy:base` <- "<environment>"

`.__T__t:base` <- "<environment>"

`.__T__*:base` <- "<environment>"

.__C__ltrMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("logical", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(ldenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    lMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ldenseMatrix", 
"triangularMatrix", "lMatrix", "denseMatrix", "Matrix", "mMatrix", 
"xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dense_nonpacked_validate, object)
    , access = list()
    , className = structure("ltrMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


sparse.model.matrix <- function (object, data = environment(object), contrasts.arg = NULL, 
    xlev = NULL, transpose = FALSE, drop.unused.levels = FALSE, 
    row.names = TRUE, verbose = FALSE, ...) 
{
    t <- if (missing(data)) 
        terms(object)
    else terms(object, data = data)
    if (is.null(attr(data, "terms"))) 
        data <- model.frame(object, data, xlev = xlev)
    else {
        reorder <- match(sapply(attr(t, "variables"), deparse, 
            width.cutoff = 500)[-1L], names(data))
        if (anyNA(reorder)) 
            stop("model frame and formula mismatch in model.matrix()")
        if (!isSeq(reorder, ncol(data), Ostart = FALSE)) 
            data <- data[, reorder, drop = FALSE]
    }
    int <- attr(t, "response")
    if (length(data)) {
        contr.funs <- as.character(getOption("contrasts"))
        namD <- names(data)
        for (i in namD) if (is.character(data[[i]])) {
            data[[i]] <- factor(data[[i]])
            warning(gettextf("variable '%s' converted to a factor", 
                i), domain = NA)
        }
        isF <- sapply(data, function(x) is.factor(x) || is.logical(x))
        isF[int] <- FALSE
        isOF <- sapply(data, is.ordered)
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
        data <- cbind(data, x = 0)
    }
    if (verbose) {
        cat("model.spmatrix(t, data, ..)  with t =\n")
        str(t, give.attr = FALSE)
    }
    ans <- model.spmatrix(t, data, transpose = transpose, drop.unused.levels = drop.unused.levels, 
        row.names = row.names, verbose = verbose)
    attr(ans, "contrasts") <- lapply(data[isF], function(x) attr(x, 
        "contrasts"))
    ans
}


.__C__dgRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "x", "factors"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"dsparseMatrix", "generalMatrix", "dMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(xRMatrix_validate, object)
    , access = list()
    , className = structure("dgRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__triu:Matrix` <- "<environment>"

abIseq1 <- function (from = 1, to = 1) 
{
    stopifnot(length(from) == 1L, length(to) == 1L)
    to <- to - from
    new("abIndex", kind = "rleDiff", rleD = new("rleDiff", first = as.integer(from), 
        rle = .rle(lengths = abs(to), values = as.integer(sign(to)))))
}


`.__T__Logic:base` <- "<environment>"

`.__T__zapsmall:base` <- "<environment>"

`.__T__unpack:Matrix` <- "<environment>"

pack <- function (x, ...) 
standardGeneric("pack")


.__C__diagonalMatrix <- new("classRepresentation"
    , slots = structure(list(diag = structure("character", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("diag", 
"Dim", "Dimnames"))
    , contains = structure(list(sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    d <- object@Dim
    if (d[1] != (n <- d[2])) 
        return("matrix is not square")
    lx <- length(object@x)
    if (object@diag == "U") {
        if (lx != 0) 
            return("diag = \"U\" (identity matrix) requires empty 'x' slot")
    }
    else if (object@diag == "N") {
        if (lx != n) 
            return("diagonal matrix has 'x' slot of length != 'n'")
    }
    else return("diagonal matrix 'diag' slot must be \"U\" or \"N\"")
    TRUE
}
    , access = list()
    , className = structure("diagonalMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(ddiMatrix = S4_object(), 
    ldiMatrix = S4_object()), .Names = c("ddiMatrix", 
"ldiMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__forceSymmetric:Matrix` <- "<environment>"

`.__T__skewpart:Matrix` <- "<environment>"

update <- function (object, ...) 
standardGeneric("update")


.__C__dtpMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(ddenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ddenseMatrix", 
"triangularMatrix", "dMatrix", "denseMatrix", "Matrix", "mMatrix", 
"xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dtpMatrix_validate, object)
    , access = list()
    , className = structure("dtpMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(pCholesky = S4_object(), 
    pBunchKaufman = S4_object()), .Names = c("pCholesky", 
"pBunchKaufman"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__isparseVector <- new("classRepresentation"
    , slots = structure(list(x = structure("integer", package = "methods"), 
    length = structure("numeric", package = "methods"), i = structure("numeric", package = "methods")), .Names = c("x", 
"length", "i"))
    , contains = structure(list(sparseVector = S4_object(), 
    xsparseVector = S4_object()), .Names = c("sparseVector", 
"xsparseVector"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (length(object@i) != length(object@x)) 
        "'i' and 'x' differ in length"
    else TRUE
}
    , access = list()
    , className = structure("isparseVector", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.formatSparseSimple <- function (m, asLogical = FALSE, digits = NULL, col.names, note.dropping.colnames = TRUE, 
    dn = dimnames(m)) 
{
    stopifnot(is.logical(asLogical))
    if (asLogical) 
        cx <- array("N", dim(m), dimnames = dn)
    else {
        cx <- apply(m, 2, format, digits = digits)
        if (is.null(dim(cx))) {
            dim(cx) <- dim(m)
            dimnames(cx) <- dn
        }
        else if (getRversion() < "3.2" && !is.null(names(dn))) {
            if (is.null(dimnames(cx))) 
                dimnames(cx) <- dn
            else names(dimnames(cx)) <- names(dn)
        }
    }
    if (missing(col.names)) 
        col.names <- {
            if (!is.null(cc <- getOption("sparse.colnames"))) 
                cc
            else if (is.null(dn[[2]])) 
                FALSE
            else {
                ncol(m) < 10
            }
        }
    if (identical(col.names, FALSE)) 
        cx <- emptyColnames(cx, msg.if.not.empty = note.dropping.colnames)
    else if (is.character(col.names)) {
        stopifnot(length(col.names) == 1)
        cn <- col.names
        switch(substr(cn, 1, 3), abb = {
            iarg <- as.integer(sub("^[^0-9]*", "", cn))
            colnames(cx) <- abbreviate(colnames(cx), minlength = iarg)
        }, sub = {
            iarg <- as.integer(sub("^[^0-9]*", "", cn))
            colnames(cx) <- substr(colnames(cx), 1, iarg)
        }, stop(gettextf("invalid 'col.names' string: %s", cn), 
            domain = NA))
    }
    cx
}


summary <- function (object, ...) 
standardGeneric("summary")


uniqTsparse <- function (x, class.x = c(class(x))) 
{
    if (extends(class.x, "TsparseMatrix")) {
        tri <- extends(class.x, "triangularMatrix")
        .Call(Csparse_to_Tsparse, .Call(Tsparse_to_Csparse, x, 
            tri), tri)
    }
    else stop(gettextf("not yet implemented for class %s", dQuote(class.x)), 
        domain = NA)
}


isTriangular <- function (object, upper = NA, ...) 
standardGeneric("isTriangular")


.__C__ntTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    diag = structure("character", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"nsparseMatrix", "triangularMatrix", "nMatrix", "sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ntTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


facmul <- function (x, factor, y, transpose, left, ...) 
standardGeneric("facmul")


qr <- function (x, ...) 
standardGeneric("qr")


.__C__CHMsimpl <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    i = structure("integer", package = "methods"), nz = structure("integer", package = "methods"), 
    nxt = structure("integer", package = "methods"), prv = structure("integer", package = "methods"), 
    colcount = structure("integer", package = "methods"), perm = structure("integer", package = "methods"), 
    type = structure("integer", package = "methods"), Dim = structure("integer", package = "methods")), .Names = c("p", 
"i", "nz", "nxt", "prv", "colcount", "perm", "type", "Dim"))
    , contains = structure(list(CHMfactor = S4_object(), 
    CholeskyFactorization = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("CHMfactor", 
"CholeskyFactorization", "MatrixFactorization"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(CHMsimpl_validate, object)
    , access = list()
    , className = structure("CHMsimpl", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dCHMsimpl = S4_object(), 
    nCHMsimpl = S4_object()), .Names = c("dCHMsimpl", 
"nCHMsimpl"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__MatrixFactorization <- new("classRepresentation"
    , slots = structure(list(Dim = structure("integer", package = "methods")), .Names = "Dim")
    , contains = structure(list(), .Names = character(0))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(MatrixFactorization_validate, object)
    , access = list()
    , className = structure("MatrixFactorization", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(CholeskyFactorization = S4_object(), 
    BunchKaufman = S4_object(), 
    pBunchKaufman = S4_object(), 
    LU = S4_object(), 
    sparseQR = S4_object(), 
    Schur = S4_object(), 
    Cholesky = S4_object(), 
    pCholesky = S4_object(), 
    CHMfactor = S4_object(), 
    denseLU = S4_object(), 
    sparseLU = S4_object(), 
    CHMsuper = S4_object(), 
    CHMsimpl = S4_object(), 
    dCHMsuper = S4_object(), 
    nCHMsuper = S4_object(), 
    dCHMsimpl = S4_object(), 
    nCHMsimpl = S4_object()), .Names = c("CholeskyFactorization", 
"BunchKaufman", "pBunchKaufman", "LU", "sparseQR", "Schur", "Cholesky", 
"pCholesky", "CHMfactor", "denseLU", "sparseLU", "CHMsuper", 
"CHMsimpl", "dCHMsuper", "nCHMsuper", "dCHMsimpl", "nCHMsimpl"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__BunchKaufman:Matrix` <- "<environment>"

.updateCHMfactor <- function (object, parent, mult) 
.Call(CHMfactor_update, object, parent, mult)


.__C__dsyMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(ddenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ddenseMatrix", 
"symmetricMatrix", "dMatrix", "denseMatrix", "compMatrix", "Matrix", 
"mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dense_nonpacked_validate, object)
    , access = list()
    , className = structure("dsyMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dpoMatrix = S4_object(), 
    corMatrix = S4_object()), .Names = c("dpoMatrix", 
"corMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Hilbert <- function (n) 
{
    n <- as.integer(n)
    i <- seq_len(n)
    new("dpoMatrix", x = c(1/outer(i - 1L, i, "+")), Dim = c(n, 
        n))
}


`.__T__!:base` <- "<environment>"

.__C__dgeMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "factors"))
    , contains = structure(list(ddenseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    geMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("ddenseMatrix", 
"generalMatrix", "geMatrix", "dMatrix", "denseMatrix", "compMatrix", 
"Matrix", "xMatrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dgeMatrix_validate, object)
    , access = list()
    , className = structure("dgeMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__dtRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "x", "uplo", "diag"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"dsparseMatrix", "triangularMatrix", "dMatrix", "sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tRMatrix_validate, object)
    , access = list()
    , className = structure("dtRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__cbind2:methods` <- "<environment>"

`.__T__Math2:methods` <- "<environment>"

.__C__nsparseMatrix <- new("classRepresentation"
    , slots = structure(list(Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("Dim", 
"Dimnames"))
    , contains = structure(list(nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("nMatrix", 
"sparseMatrix", "Matrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nsparseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(ngTMatrix = S4_object(), 
    ntTMatrix = S4_object(), 
    nsTMatrix = S4_object(), 
    ngCMatrix = S4_object(), 
    ntCMatrix = S4_object(), 
    nsCMatrix = S4_object(), 
    ngRMatrix = S4_object(), 
    ntRMatrix = S4_object(), 
    nsRMatrix = S4_object()), .Names = c("ngTMatrix", 
"ntTMatrix", "nsTMatrix", "ngCMatrix", "ntCMatrix", "nsCMatrix", 
"ngRMatrix", "ntRMatrix", "nsRMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.solve.dgC.chol <- function (x, y) 
.Call(dgCMatrix_cholsol, as(x, "CsparseMatrix"), y)


`.__T__dim<-:base` <- "<environment>"

`.__T__band:Matrix` <- "<environment>"

toeplitz <- function (x, ...) 
standardGeneric("toeplitz")


.__C__dsCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "x", "uplo", "factors"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dCsparseMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"dsparseMatrix", "symmetricMatrix", "dCsparseMatrix", "dMatrix", 
"sparseMatrix", "compMatrix", "Matrix", "xMatrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tCMatrix_validate, object)
    , access = list()
    , className = structure("dsCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__dspMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(ddenseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ddenseMatrix", 
"symmetricMatrix", "dMatrix", "denseMatrix", "compMatrix", "Matrix", 
"mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dspMatrix_validate, object)
    , access = list()
    , className = structure("dspMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dppMatrix = S4_object()), .Names = "dppMatrix")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


rBind <- function (..., deparse.level = 1) 
{
    if (isTRUE(getOption("Matrix.warn")) || isTRUE(getOption("Matrix.verbose"))) 
        .Deprecated(msg = "'rBind' is deprecated.\n Since R version 3.2.0, base's rbind() should work fine with S4 objects")
    base::rbind(..., deparse.level = deparse.level)
}


`.__T__diff:base` <- "<environment>"

nearPD <- function (x, corr = FALSE, keepDiag = FALSE, do2eigen = TRUE, 
    doSym = FALSE, doDykstra = TRUE, only.values = FALSE, ensureSymmetry = !isSymmetric(x), 
    eig.tol = 1e-06, conv.tol = 1e-07, posd.tol = 1e-08, maxit = 100, 
    conv.norm.type = "I", trace = FALSE) 
{
    if (ensureSymmetry) {
        x <- symmpart(x)
    }
    n <- ncol(x)
    if (keepDiag) 
        diagX0 <- diag(x)
    if (doDykstra) {
        D_S <- x
        D_S[] <- 0
    }
    X <- x
    iter <- 0
    converged <- FALSE
    conv <- Inf
    while (iter < maxit && !converged) {
        Y <- X
        if (doDykstra) 
            R <- Y - D_S
        e <- eigen(if (doDykstra) 
            R
        else Y, symmetric = TRUE)
        Q <- e$vectors
        d <- e$values
        p <- d > eig.tol * d[1]
        if (!any(p)) 
            stop("Matrix seems negative semi-definite")
        Q <- Q[, p, drop = FALSE]
        X <- tcrossprod(Q * rep(d[p], each = nrow(Q)), Q)
        if (doDykstra) 
            D_S <- X - R
        if (doSym) 
            X <- (X + t(X))/2
        if (corr) 
            diag(X) <- 1
        else if (keepDiag) 
            diag(X) <- diagX0
        conv <- norm(Y - X, conv.norm.type)/norm(Y, conv.norm.type)
        iter <- iter + 1
        if (trace) 
            cat(sprintf("iter %3d : #{p}=%d, ||Y-X|| / ||Y||= %11g\n", 
                iter, sum(p), conv))
        converged <- (conv <= conv.tol)
    }
    if (!converged) 
        warning(gettextf("'nearPD()' did not converge in %d iterations", 
            iter), domain = NA)
    if (do2eigen || only.values) {
        e <- eigen(X, symmetric = TRUE)
        d <- e$values
        Eps <- posd.tol * abs(d[1])
        if (d[n] < Eps) {
            d[d < Eps] <- Eps
            if (!only.values) {
                Q <- e$vectors
                o.diag <- diag(X)
                X <- Q %*% (d * t(Q))
                D <- sqrt(pmax(Eps, o.diag)/diag(X))
                X[] <- D * X * rep(D, each = n)
            }
        }
        if (only.values) 
            return(d)
        if (corr) 
            diag(X) <- 1
        else if (keepDiag) 
            diag(X) <- diagX0
    }
    structure(list(mat = new("dpoMatrix", x = as.vector(X), Dim = c(n, 
        n), Dimnames = .M.DN(x)), eigenvalues = d, corr = corr, 
        normF = norm(x - X, "F"), iterations = iter, rel.tol = conv, 
        converged = converged), class = "nearPD")
}


`.__T__image:graphics` <- "<environment>"

print <- function (x, ...) 
standardGeneric("print")


.__C__pMatrix <- new("classRepresentation"
    , slots = structure(list(perm = structure("integer", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("perm", 
"Dim", "Dimnames", "factors"))
    , contains = structure(list(indMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("indMatrix", 
"sparseMatrix", "generalMatrix", "compMatrix", "Matrix", "mMatrix"
))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    d <- object@Dim
    if (d[2] != (n <- d[1])) 
        return("pMatrix must be square")
    perm <- object@perm
    if (length(perm) != n) 
        return(paste("length of 'perm' slot must be", n))
    if (n > 0 && !(all(range(perm) == c(1, n)) && length(unique(perm)) == 
        n)) 
        return("'perm' slot is not a valid permutation")
    TRUE
}
    , access = list()
    , className = structure("pMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__nsTMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    factors = structure("list", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames", "uplo", "factors"))
    , contains = structure(list(TsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("TsparseMatrix", 
"nsparseMatrix", "symmetricMatrix", "nMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nsTMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__toeplitz:stats` <- "<environment>"

`.__T__tcrossprod:base` <- "<environment>"

.__C__compMatrix <- new("classRepresentation"
    , slots = structure(list(factors = structure("list", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("factors", 
"Dim", "Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("compMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(generalMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dpoMatrix = S4_object(), 
    dppMatrix = S4_object(), 
    corMatrix = S4_object(), 
    pMatrix = S4_object()), .Names = c("generalMatrix", 
"symmetricMatrix", "dpoMatrix", "dppMatrix", "corMatrix", "pMatrix"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


diagU2N <- function (x, cl = getClassDef(class(x)), checkDense = FALSE) 
{
    if (extends(cl, "triangularMatrix") && x@diag == "U") 
        .diagU2N(x, cl, checkDense = checkDense)
    else x
}


.__C__generalMatrix <- new("classRepresentation"
    , slots = structure(list(factors = structure("list", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("factors", 
"Dim", "Dimnames"))
    , contains = structure(list(compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("compMatrix", 
"Matrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("generalMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dgeMatrix = S4_object(), 
    lgeMatrix = S4_object(), 
    ngeMatrix = S4_object(), 
    dgTMatrix = S4_object(), 
    dgCMatrix = S4_object(), 
    dgRMatrix = S4_object(), 
    lgTMatrix = S4_object(), 
    lgCMatrix = S4_object(), 
    lgRMatrix = S4_object(), 
    ngTMatrix = S4_object(), 
    ngCMatrix = S4_object(), 
    ngRMatrix = S4_object(), 
    indMatrix = S4_object(), 
    pMatrix = S4_object()), .Names = c("dgeMatrix", 
"lgeMatrix", "ngeMatrix", "dgTMatrix", "dgCMatrix", "dgRMatrix", 
"lgTMatrix", "lgCMatrix", "lgRMatrix", "ngTMatrix", "ngCMatrix", 
"ngRMatrix", "indMatrix", "pMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


tril <- function (x, k = 0, ...) 
standardGeneric("tril")


.SuiteSparse_version <- function () 
{
    ssv <- .Call(get_SuiteSparse_version)
    package_version(list(major = ssv[1], minor = paste(ssv[2:3], 
        collapse = ".")))
}


.__C__dsRMatrix <- new("classRepresentation"
    , slots = structure(list(p = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), x = structure("numeric", package = "methods"), 
    uplo = structure("character", package = "methods"), factors = structure("list", package = "methods")), .Names = c("p", 
"j", "Dim", "Dimnames", "x", "uplo", "factors"))
    , contains = structure(list(RsparseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    symmetricMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("RsparseMatrix", 
"dsparseMatrix", "symmetricMatrix", "dMatrix", "sparseMatrix", 
"compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(tRMatrix_validate, object)
    , access = list()
    , className = structure("dsRMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.solve.dgC.lu <- function (a, b, tol = .Machine$double.eps) 
{
    lu.a <- LU.dgC(a)
    if (tol > 0) {
        rU <- range(abs(diag(lu.a@U)))
        if (rU[1]/rU[2] < tol) 
            stop(gettextf("LU computationally singular: ratio of extreme entries in |diag(U)| = %9.4g", 
                rU[1]/rU[2]), domain = NA)
    }
    n <- dim(a)[1L]
    b.isMat <- if (missing(b)) {
        b <- .sparseDiagonal(n)
        TRUE
    }
    else {
        isM <- !is.null(dim(b))
        if (isM && nrow(b) != n) 
            stop("RHS 'b' has wrong number of rows:", nrow(b))
        if (!isM && length(b) != n) 
            stop("RHS 'b' has wrong length", length(b))
        isM
    }
    bp <- if (b.isMat) 
        b[lu.a@p + 1L, ]
    else b[lu.a@p + 1L]
    R <- solve(lu.a@U, solve(lu.a@L, bp))
    R[invPerm(lu.a@q, zero.p = TRUE), ]
}


.__C__ntCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), uplo = structure("character", package = "methods"), 
    diag = structure("character", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    nCsparseMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"nsparseMatrix", "triangularMatrix", "nCsparseMatrix", "nMatrix", 
"sparseMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ntCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__Arith:base` <- "<environment>"

show <- methods::show # re-exported from methods package

`.__T__Compare:methods` <- "<environment>"

chol <- function (x, ...) 
standardGeneric("chol")


triu <- function (x, k = 0, ...) 
standardGeneric("triu")


`.__T__rep:base` <- "<environment>"

all.equal <- function (target, current, ...) 
standardGeneric("all.equal")


.__C__number <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = numeric(0)
    , validity = NULL
    , access = list()
    , className = structure("number", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(numeric = S4_object(), 
    complex = S4_object(), 
    integer = S4_object(), 
    factor = S4_object()), .Names = c("numeric", 
"complex", "integer", "factor"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__ddiMatrix <- new("classRepresentation"
    , slots = structure(list(diag = structure("character", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    x = structure("numeric", package = "methods")), .Names = c("diag", 
"Dim", "Dimnames", "x"))
    , contains = structure(list(diagonalMatrix = S4_object(), 
    dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("diagonalMatrix", 
"dMatrix", "sparseMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ddiMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__sum:base` <- "<environment>"

`.__T__norm:base` <- "<environment>"

`.__T__pack:Matrix` <- "<environment>"

.__C__dtrMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    uplo = structure("character", package = "methods"), diag = structure("character", package = "methods")), .Names = c("x", 
"Dim", "Dimnames", "uplo", "diag"))
    , contains = structure(list(ddenseMatrix = S4_object(), 
    triangularMatrix = S4_object(), 
    dMatrix = S4_object(), 
    denseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("ddenseMatrix", 
"triangularMatrix", "dMatrix", "denseMatrix", "Matrix", "mMatrix", 
"xMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dense_nonpacked_validate, object)
    , access = list()
    , className = structure("dtrMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(Cholesky = S4_object(), 
    BunchKaufman = S4_object()), .Names = c("Cholesky", 
"BunchKaufman"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


fac2sparse <- function (from, to = c("d", "i", "l", "n", "z"), drop.unused.levels = TRUE, 
    giveCsparse = TRUE) 
{
    fact <- if (drop.unused.levels) 
        factor(from)
    else as.factor(from)
    levs <- levels(fact)
    n <- length(fact)
    to <- match.arg(to)
    i <- as.integer(fact) - 1L
    df <- data.frame(i = i, j = if (n) 
        0:(n - 1L)
    else integer())[!is.na(i), ]
    if (to != "n") 
        df$x <- rep.int(switch(to, d = 1, i = 1L, l = TRUE, z = 1 + 
            (0+0i)), nrow(df))
    T <- do.call("new", c(list(Class = paste0(to, "gTMatrix"), 
        Dim = c(length(levs), n), Dimnames = list(levs, names(fact))), 
        df))
    if (giveCsparse) 
        .Call(Tsparse_to_Csparse, T, FALSE)
    else T
}


det <- function (x, ...) 
{
    z <- determinant(x, logarithm = TRUE, ...)
    c(z$sign * exp(z$modulus))
}


`.__T__expm:Matrix` <- "<environment>"

`.__T__+:base` <- "<environment>"

formatSparseM <- function (x, zero.print = ".", align = c("fancy", "right"), m = as(x, 
    "matrix"), asLogical = NULL, uniDiag = NULL, digits = NULL, 
    cx, iN0, dn = dimnames(m)) 
{
    cld <- getClassDef(class(x))
    if (is.null(asLogical)) {
        binary <- extends(cld, "nsparseMatrix") || extends(cld, 
            "indMatrix")
        asLogical <- {
            binary || extends(cld, "lsparseMatrix") || extends(cld, 
                "matrix") && is.logical(x)
        }
    }
    if (missing(cx)) 
        cx <- .formatSparseSimple(m, asLogical = asLogical, digits = digits, 
            dn = dn)
    if (is.null(d <- dim(cx))) {
        d <- dim(cx) <- dim(m)
        dimnames(cx) <- dn
    }
    if (missing(iN0)) 
        iN0 <- 1L + .Call(m_encodeInd, non0ind(x, cld), di = d, 
            FALSE, FALSE)
    if (asLogical) {
        cx[m] <- "|"
        if (!extends(cld, "sparseMatrix")) 
            x <- as(x, "sparseMatrix")
        if (anyFalse(x@x)) {
            if (extends(cld, "TsparseMatrix")) {
                x <- as(x, "CsparseMatrix")
                cld <- getClassDef(class(x))
            }
            F. <- is0(x@x)
            ij <- non0.i(x, cld, uniqT = FALSE)
            if (extends(cld, "symmetricMatrix")) {
                notdiag <- ij[, 1] != ij[, 2]
                ij <- rbind(ij, ij[notdiag, 2:1], deparse.level = 0)
                F. <- c(F., F.[notdiag])
            }
            iN0 <- 1L + .Call(m_encodeInd, ij, di = d, FALSE, 
                FALSE)
            cx[iN0[F.]] <- ":"
        }
    }
    else if (match.arg(align) == "fancy" && !is.integer(m)) {
        fi <- apply(m, 2, format.info)
        cols <- 1L + (0:(prod(d) - 1L))[-iN0]%/%d[1]
        pad <- ifelse(fi[3, ] == 0, fi[2, ] + as.logical(fi[2, 
            ] > 0), fi[2, ] + fi[3, ] + 4)
        if (any(doP <- pad > 0)) {
            z.p.pad <- rep.int(zero.print, length(pad))
            z.p.pad[doP] <- sprintf("%-*s", pad[doP] + 1, zero.print)
            zero.print <- z.p.pad[cols]
        }
        else zero.print <- rep.int(zero.print, length(cols))
    }
    if (!asLogical && isTRUE(uniDiag)) {
        if (any(diag(x) != 1)) 
            stop("uniDiag=TRUE, but not all diagonal entries are 1")
        D <- diag(cx)
        if (any((ir <- regexpr("1", D)) < 0)) {
            warning("uniDiag=TRUE, not all entries in diagonal coded as 1")
        }
        else {
            ir <- as.vector(ir)
            nD <- nchar(D, "bytes")
            substr(D, ir, nD) <- sprintf("I%*s", nD - ir, "")
            diag(cx) <- D
        }
    }
    cx[-iN0] <- zero.print
    cx
}


`.__T__writeMM:Matrix` <- "<environment>"

.__C__TsparseMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    j = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods")), .Names = c("i", 
"j", "Dim", "Dimnames"))
    , contains = structure(list(sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("sparseMatrix", 
"Matrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(Tsparse_validate, object)
    , access = list()
    , className = structure("TsparseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dgTMatrix = S4_object(), 
    dtTMatrix = S4_object(), 
    dsTMatrix = S4_object(), 
    lgTMatrix = S4_object(), 
    ltTMatrix = S4_object(), 
    lsTMatrix = S4_object(), 
    ngTMatrix = S4_object(), 
    ntTMatrix = S4_object(), 
    nsTMatrix = S4_object()), .Names = c("dgTMatrix", 
"dtTMatrix", "dsTMatrix", "lgTMatrix", "ltTMatrix", "lsTMatrix", 
"ngTMatrix", "ntTMatrix", "nsTMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__qr.qty:base` <- "<environment>"

BunchKaufman <- function (x, ...) 
standardGeneric("BunchKaufman")


readMM <- function (file) 
{
    if (is.character(file)) 
        file <- if (file == "") 
            stdin()
        else file(file)
    if (!inherits(file, "connection")) 
        stop("'file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file)
        on.exit(close(file))
    }
    scan1 <- function(what, ...) scan(file, nmax = 1, what = what, 
        quiet = TRUE, ...)
    if (scan1(character()) != "%%MatrixMarket") 
        stop("file is not a MatrixMarket file")
    if (!(typ <- tolower(scan1(character()))) %in% "matrix") 
        stop(gettextf("type '%s' not recognized", typ), domain = NA)
    if (!(repr <- tolower(scan1(character()))) %in% c("coordinate", 
        "array")) 
        stop(gettextf("representation '%s' not recognized", repr), 
            domain = NA)
    elt <- tolower(scan1(character()))
    if (!elt %in% c("real", "complex", "integer", "pattern")) 
        stop(gettextf("element type '%s' not recognized", elt), 
            domain = NA)
    sym <- tolower(scan1(character()))
    if (!sym %in% c("general", "symmetric", "skew-symmetric", 
        "hermitian")) 
        stop(gettextf("symmetry form '%s' not recognized", sym), 
            domain = NA)
    nr <- scan1(integer(), comment.char = "%")
    nc <- scan1(integer())
    nz <- scan1(integer())
    checkIJ <- function(els) {
        if (els$i < 1 || els$i > nr) 
            stop("readMM(): row\t values 'i' are not in 1:nr", 
                call. = FALSE)
        if (els$j < 1 || els$j > nc) 
            stop("readMM(): column values 'j' are not in 1:nc", 
                call. = FALSE)
    }
    if (repr == "coordinate") {
        switch(elt, real = , integer = {
            els <- scan(file, nmax = nz, quiet = TRUE, what = list(i = integer(), 
                j = integer(), x = numeric()))
            checkIJ(els)
            switch(sym, general = {
                new("dgTMatrix", Dim = c(nr, nc), i = els$i - 
                  1L, j = els$j - 1L, x = els$x)
            }, symmetric = {
                new("dsTMatrix", uplo = "L", Dim = c(nr, nc), 
                  i = els$i - 1L, j = els$j - 1L, x = els$x)
            }, `skew-symmetric` = {
                stop("symmetry form 'skew-symmetric' not yet implemented for reading")
                new("dgTMatrix", uplo = "L", Dim = c(nr, nc), 
                  i = els$i - 1L, j = els$j - 1L, x = els$x)
            }, hermitian = {
                stop("symmetry form 'hermitian' not yet implemented for reading")
            }, stop(gettextf("symmetry form '%s' is not yet implemented", 
                sym), domain = NA))
        }, pattern = {
            els <- scan(file, nmax = nz, quiet = TRUE, what = list(i = integer(), 
                j = integer()))
            checkIJ(els)
            switch(sym, general = {
                new("ngTMatrix", Dim = c(nr, nc), i = els$i - 
                  1L, j = els$j - 1L)
            }, symmetric = {
                new("nsTMatrix", uplo = "L", Dim = c(nr, nc), 
                  i = els$i - 1L, j = els$j - 1L)
            }, `skew-symmetric` = {
                stop("symmetry form 'skew-symmetric' not yet implemented for reading")
                new("ngTMatrix", uplo = "L", Dim = c(nr, nc), 
                  i = els$i - 1L, j = els$j - 1L)
            }, hermitian = {
                stop("symmetry form 'hermitian' not yet implemented for reading")
            }, stop(gettextf("symmetry form '%s' is not yet implemented", 
                sym), domain = NA))
        }, complex = {
            stop("element type 'complex' not yet implemented")
        }, stop(gettextf("'%s()' is not yet implemented for element type '%s'", 
            "readMM", elt), domain = NA))
    }
    else stop(gettextf("'%s()' is not yet implemented for  representation '%s'", 
        "readMM", repr), domain = NA)
}


qr.qy <- function (qr, y) 
standardGeneric("qr.qy")


`.__T__is.finite:base` <- "<environment>"

.__C__denseLU <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    perm = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods"), 
    Dim = structure("integer", package = "methods")), .Names = c("x", 
"perm", "Dimnames", "Dim"))
    , contains = structure(list(LU = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("LU", 
"MatrixFactorization"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(LU_validate, object)
    , access = list()
    , className = structure("denseLU", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__dMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(Matrix = S4_object(), 
    xMatrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("Matrix", 
"xMatrix", "mMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = function (object) 
.Call(dMatrix_validate, object)
    , access = list()
    , className = structure("dMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(ddenseMatrix = S4_object(), 
    dsparseMatrix = S4_object(), 
    ddiMatrix = S4_object(), 
    dgeMatrix = S4_object(), 
    dtrMatrix = S4_object(), 
    dtpMatrix = S4_object(), 
    dsyMatrix = S4_object(), 
    dspMatrix = S4_object(), 
    dpoMatrix = S4_object(), 
    dppMatrix = S4_object(), 
    corMatrix = S4_object(), 
    Cholesky = S4_object(), 
    pCholesky = S4_object(), 
    BunchKaufman = S4_object(), 
    pBunchKaufman = S4_object()), .Names = c("ddenseMatrix", 
"dsparseMatrix", "ddiMatrix", "dgeMatrix", "dtrMatrix", "dtpMatrix", 
"dsyMatrix", "dspMatrix", "dpoMatrix", "dppMatrix", "corMatrix", 
"Cholesky", "pCholesky", "BunchKaufman", "pBunchKaufman"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


crossprod <- function (x, y = NULL, ...) 
standardGeneric("crossprod")


`.__T__isTriangular:Matrix` <- "<environment>"

writeMM <- function (obj, file, ...) 
standardGeneric("writeMM")


`.__T__rcond:base` <- "<environment>"

`.__T__isDiagonal:Matrix` <- "<environment>"

isLDL <- function (x) 
{
    stopifnot(is(x, "CHMfactor"))
    as.logical(!x@type[2])
}


as.array <- function (x, ...) 
standardGeneric("as.array")


.__C__dsparseVector <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    length = structure("numeric", package = "methods"), i = structure("numeric", package = "methods")), .Names = c("x", 
"length", "i"))
    , contains = structure(list(sparseVector = S4_object(), 
    xsparseVector = S4_object()), .Names = c("sparseVector", 
"xsparseVector"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (length(object@i) != length(object@x)) 
        "'i' and 'x' differ in length"
    else TRUE
}
    , access = list()
    , className = structure("dsparseVector", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.dxC2mat <- function (from, chkUdiag = TRUE) 
.Call(Csparse_to_matrix, from, chkUdiag, NA)


MatrixClass <- function (cl, cld = getClassDef(cl), ...Matrix = TRUE, dropVirtual = TRUE, 
    ...) 
{
    if (is.null(pkg <- cld@package)) {
        if (is.null(pkg <- packageSlot(cl))) 
            return(character())
    }
    if (identical(pkg, "Matrix") && (!...Matrix || (cl != "indMatrix" && 
        identical(1L, grep("^[dlniz]..Matrix$", cl))))) 
        cl
    else {
        r <- .selectSuperClasses(cld@contains, dropVirtual = dropVirtual, 
            namesOnly = TRUE, ...)
        if (length(r)) 
            Recall(r[1], ...Matrix = ...Matrix, dropVirtual = dropVirtual)
        else r
    }
}


`.__T__%&%:Matrix` <- "<environment>"

.C2nC <- function (from, isTri = is(from, "triangularMatrix")) 
.Call(Csparse_to_nz_pattern, from, isTri)


.__C__dsparseMatrix <- new("classRepresentation"
    , slots = structure(list(x = structure("numeric", package = "methods"), 
    Dim = structure("integer", package = "methods"), Dimnames = structure("list", package = "methods")), .Names = c("x", 
"Dim", "Dimnames"))
    , contains = structure(list(dMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object(), 
    xMatrix = S4_object()), .Names = c("dMatrix", 
"sparseMatrix", "Matrix", "mMatrix", "xMatrix"))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("dsparseMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = structure(list(dgTMatrix = S4_object(), 
    dtTMatrix = S4_object(), 
    dsTMatrix = S4_object(), 
    dgCMatrix = S4_object(), 
    dtCMatrix = S4_object(), 
    dsCMatrix = S4_object(), 
    dgRMatrix = S4_object(), 
    dtRMatrix = S4_object(), 
    dsRMatrix = S4_object()), .Names = c("dgTMatrix", 
"dtTMatrix", "dsTMatrix", "dgCMatrix", "dtCMatrix", "dsCMatrix", 
"dgRMatrix", "dtRMatrix", "dsRMatrix"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__nCHMsuper <- new("classRepresentation"
    , slots = structure(list(super = structure("integer", package = "methods"), 
    pi = structure("integer", package = "methods"), px = structure("integer", package = "methods"), 
    s = structure("integer", package = "methods"), colcount = structure("integer", package = "methods"), 
    perm = structure("integer", package = "methods"), type = structure("integer", package = "methods"), 
    Dim = structure("integer", package = "methods")), .Names = c("super", 
"pi", "px", "s", "colcount", "perm", "type", "Dim"))
    , contains = structure(list(CHMsuper = S4_object(), 
    CHMfactor = S4_object(), 
    CholeskyFactorization = S4_object(), 
    MatrixFactorization = S4_object()), .Names = c("CHMsuper", 
"CHMfactor", "CholeskyFactorization", "MatrixFactorization"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nCHMsuper", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__ngCMatrix <- new("classRepresentation"
    , slots = structure(list(i = structure("integer", package = "methods"), 
    p = structure("integer", package = "methods"), Dim = structure("integer", package = "methods"), 
    Dimnames = structure("list", package = "methods"), factors = structure("list", package = "methods")), .Names = c("i", 
"p", "Dim", "Dimnames", "factors"))
    , contains = structure(list(CsparseMatrix = S4_object(), 
    nsparseMatrix = S4_object(), 
    generalMatrix = S4_object(), 
    nCsparseMatrix = S4_object(), 
    nMatrix = S4_object(), 
    sparseMatrix = S4_object(), 
    compMatrix = S4_object(), 
    Matrix = S4_object(), 
    mMatrix = S4_object()), .Names = c("CsparseMatrix", 
"nsparseMatrix", "generalMatrix", "nCsparseMatrix", "nMatrix", 
"sparseMatrix", "compMatrix", "Matrix", "mMatrix"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ngCMatrix", package = "Matrix")
    , package = "Matrix"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__Summary:base` <- "<environment>"

.nC2d <- function (from) 
.Call(nz_pattern_to_Csparse, from, 0L)




## Package Data

CAex <- Matrix::CAex		## Albers' example Matrix with "Difficult" Eigen Factorization

KNex <- Matrix::KNex		## Koenker-Ng Example Sparse Model Matrix and Response Vector

USCounties <- Matrix::USCounties		## USCounties Contiguity Matrix



## Package Info

.skeleton_package_title = "Sparse and Dense Matrix Classes and Methods"

.skeleton_package_version = "1.2-6"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods,graphics,grid,stats,utils,lattice"


## Internal

.skeleton_version = 5


## EOF