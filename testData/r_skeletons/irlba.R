##
## Exported symobls in package `irlba`
##

## Exported package methods

prcomp_irlba <- function (x, n = 3, retx = TRUE, center = TRUE, scale. = FALSE, 
    ...) 
{
    a <- names(as.list(match.call()))
    if ("tol" %in% a) 
        warning("The `tol` truncation argument from `prcomp` is not supported by\n`prcomp_irlba`. If specified, `tol` is passed to the `irlba` function to\ncontrol that algorithm's convergence tolerance. See `?prcomp_irlba` for help.")
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    args <- list(A = x, nv = n)
    if (is.logical(center)) {
        if (center) 
            args$center <- colMeans(x)
    }
    else args$center <- center
    if (is.logical(scale.)) {
        if (scale.) 
            args$scale <- apply(x, 2, sd)
    }
    else args$scale <- scale.
    if (!missing(...)) 
        args <- c(args, list(...))
    s <- do.call(irlba, args = args)
    ans <- list(sdev = s$d/sqrt(max(1, nrow(x) - 1)), rotation = s$v)
    colnames(ans$rotation) <- paste("PC", seq(1, ncol(ans$rotation)), 
        sep = "")
    ans$center <- args$center
    ans$scale <- args$scale
    if (retx) {
        ans <- c(ans, list(x = s$d * s$u))
        colnames(ans$x) <- paste("PC", seq(1, ncol(ans$rotation)), 
            sep = "")
    }
    class(ans) <- "prcomp"
    ans
}


partial_eigen <- function (x, n = 5, symmetric = TRUE, ...) 
{
    if (n > 0.5 * min(nrow(x), ncol(x))) {
        warning("You're computing a large percentage of total eigenvalues, the standard eigen function will likely work better!")
    }
    if (!symmetric) {
        L <- irlba(x, n, ...)
        return(list(vectors = L$v, values = L$d^2))
    }
    L <- irlba(x, n, ...)
    s <- sign(L$u[1, ] * L$v[1, ])
    if (all(s > 0)) {
        return(list(vectors = L$u, values = L$d))
    }
    i <- min(which(s < 0))
    shift <- L$d[i]
    L <- irlba(x, n, shift = shift, ...)
    return(list(vectors = L$u, values = L$d - shift))
}


irlba <- function (A, nv = 5, nu, maxit = 1000, work = nv + 7, reorth = TRUE, 
    tol = 1e-05, v = NULL, right_only = FALSE, verbose = FALSE, 
    scale, center, du, ds, dv, shift, mult, fastpath = TRUE) 
{
    ropts <- options(warn = 1)
    on.exit(options(ropts))
    eps <- .Machine$double.eps
    deflate <- missing(du) + missing(ds) + missing(dv)
    if (deflate == 3) {
        deflate <- FALSE
    }
    else if (deflate == 0) {
        deflate <- TRUE
        warning("The deflation options are deprecated and will be removed in a future version.")
        if (length(ds) > 1) 
            stop("deflation limited to one dimension")
        if (!is.null(dim(du))) 
            du <- du[, 1]
        if (!is.null(dim(dv))) 
            dv <- dv[, 1]
    }
    else stop("all three du ds dv parameters must be specified for deflation")
    if (!missing(center)) {
        if (deflate) 
            stop("the center parameter can't be specified together with deflation parameters")
        if (length(center) != ncol(A)) 
            stop("center must be a vector of length ncol(A)")
        if (fastpath && !right_only) 
            du <- NULL
        else du <- 1
        ds <- 1
        dv <- center
        deflate <- TRUE
    }
    iscomplex <- is.complex(A)
    m <- nrow(A)
    n <- ncol(A)
    if (missing(nu)) 
        nu <- nv
    if (!missing(mult) && deflate) 
        stop("the mult parameter can't be specified together with deflation parameters")
    missingmult <- FALSE
    if (missing(mult)) {
        missingmult <- TRUE
        mult <- `%*%`
    }
    k <- max(nu, nv)
    if (k <= 0) 
        stop("max(nu, nv) must be positive")
    if (k > min(m - 1, n - 1)) 
        stop("max(nu, nv) must be strictly less than min(nrow(A), ncol(A))")
    if (k >= 0.5 * min(m, n)) {
        warning("You're computing a large percentage of total singular values, standard svd might work better!")
    }
    if (work <= 1) 
        stop("work must be greater than 1")
    if (tol < 0) 
        stop("tol must be non-negative")
    if (maxit <= 0) 
        stop("maxit must be positive")
    if (work <= k) 
        work <- k + 1
    if (work >= min(n, m)) {
        work <- min(n, m)
        if (work <= k) {
            k <- work - 1
            warning("Requested subspace dimension too large! Reduced to ", 
                k)
        }
    }
    k_org <- k
    w_dim <- work
    if (right_only) {
        w_dim <- 1
        work <- min(min(m, n), work + 20)
        fastpath <- FALSE
    }
    if (verbose) {
        message("Working dimension size ", work)
    }
    if (min(m, n) < 6) {
        if (verbose) 
            message("Tiny problem detected, using standard `svd` function.")
        if (!missing(scale)) 
            A <- A/scale
        if (!missing(shift)) 
            A <- A + diag(shift)
        if (deflate) {
            if (is.null(du)) 
                du <- rep(1, nrow(A))
            A <- A - (ds * du) %*% t(dv)
        }
        s <- svd(A)
        return(list(d = s$d[1:k], u = s$u[, 1:nu, drop = FALSE], 
            v = s$v[, 1:nv, drop = FALSE], iter = 0, mprod = 0))
    }
    if (deflate) 
        fastpath <- fastpath && is.null(du)
    if ("Matrix" %in% attributes(class(A)) && !("dgCMatrix" %in% 
        class(A))) {
        fastpath <- FALSE
    }
    if ("matrix" %in% attributes(A)$.S3Class && !("matrix" %in% 
        class(A))) {
        fastpath <- FALSE
    }
    if (fastpath && missingmult && !iscomplex && !right_only) {
        RESTART <- 0
        RV <- RW <- RS <- NULL
        if (is.null(v)) {
            v <- rnorm(n)
            if (verbose) 
                message("Initializing starting vector v with samples from standard normal distribution.\nUse `set.seed` first for reproducibility.")
        }
        else if (is.list(v)) {
            if (is.null(v$v) || is.null(v$d) || is.null(v$u)) 
                stop("restart requires left and right singular vectors")
            if (max(nu, nv) <= min(ncol(v$u), ncol(v$v))) 
                return(v)
            RESTART <- as.integer(length(v$d))
            RND <- rnorm(n)
            RND <- orthog(RND, v$v)
            RV <- cbind(v$v, RND/norm2(RND))
            RW <- v$u
            RS <- v$d
            v <- NULL
        }
        SP <- ifelse(is.matrix(A), 0L, 1L)
        if (verbose) 
            message("irlba: using fast C implementation")
        SCALE <- NULL
        SHIFT <- NULL
        CENTER <- NULL
        if (!missing(scale)) {
            if (length(scale) != ncol(A)) 
                stop("scale length must mactch number of matrix columns")
            SCALE <- as.double(scale)
        }
        if (!missing(shift)) {
            if (length(shift) != 1) 
                stop("shift length must be 1")
            SHIFT <- as.double(shift)
        }
        if (deflate) {
            if (length(center) != ncol(A)) 
                stop("the centering vector length must match the number of matrix columns")
            CENTER <- as.double(center)
        }
        ans <- .Call("IRLB", A, as.integer(k), as.double(v), 
            as.integer(work), as.integer(maxit), as.double(tol), 
            .Machine$double.eps, as.integer(SP), RESTART, RV, 
            RW, RS, SCALE, SHIFT, CENTER, PACKAGE = "irlba")
        if (ans[[6]] == 0 || ans[[6]] == -2) {
            names(ans) <- c("d", "u", "v", "iter", "mprod", "err")
            ans$u <- matrix(head(ans$u, m * nu), nrow = m, ncol = nu)
            ans$v <- matrix(head(ans$v, n * nv), nrow = n, ncol = nv)
            if (tol * ans$d[1] < eps) 
                warning("convergence criterion below machine epsilon")
            if (ans[[6]] == -2) 
                warning("did not converge--results might be invlaid!; try increasing maxit or fastpath=FALSE")
            return(ans[-6])
        }
        errors <- c("invalid dimensions", "didn't converge", 
            "out of memory", "starting vector near the null space", 
            "linear dependency encountered")
        erridx <- abs(ans[[6]])
        if (erridx > 1) 
            warning("fast code path error ", errors[erridx], 
                "; re-trying with fastpath=FALSE.", immediate. = TRUE)
    }
    W <- matrix(0, m, w_dim)
    F <- matrix(0, n, 1)
    restart <- FALSE
    if (is.list(v)) {
        if (is.null(v$v) || is.null(v$d) || is.null(v$u)) 
            stop("restart requires left and right singular vectors")
        if (max(nu, nv) <= min(ncol(v$u), ncol(v$v))) 
            return(v)
        right_only <- FALSE
        W[, 1:ncol(v$u)] <- v$u
        d <- v$d
        V <- matrix(0, n, work)
        V[, 1:ncol(v$v)] <- v$v
        restart <- TRUE
    }
    else if (is.null(v)) {
        V <- matrix(0, n, work)
        V[, 1] <- rnorm(n)
    }
    else {
        V <- matrix(0, n, work)
        V[1:length(v)] <- v
    }
    B <- NULL
    Bsz <- NULL
    eps23 <- eps^(2/3)
    eps2 <- 2 * eps
    iter <- 1
    mprod <- 0
    R_F <- NULL
    sqrteps <- sqrt(eps)
    Smax <- 1
    Smin <- NULL
    SVTol <- tol
    if (restart) {
        B <- cbind(diag(d), 0)
        k <- length(d)
        F <- rnorm(n)
        F <- orthog(F, V[, 1:k])
        V[, k + 1] <- F/norm2(F)
    }
    while (iter <= maxit) {
        j <- 1
        if (iter == 1 && !restart) {
            V[, 1] <- V[, 1]/norm2(V[, 1])
        }
        else j <- k + 1
        j_w <- ifelse(w_dim > 1, j, 1)
        VJ <- V[, j]
        if (!missing(scale)) {
            VJ <- VJ/scale
        }
        avj <- mult(A, VJ)
        if ("Matrix" %in% attributes(class(avj)) && "x" %in% 
            slotNames(avj)) {
            if (length(avj@x) == nrow(W)) 
                avj <- slot(avj, "x")
            else avj <- as.vector(avj)
        }
        W[, j_w] <- avj
        mprod <- mprod + 1
        if (!missing(shift)) {
            W[, j_w] <- W[, j_w] + shift * VJ
        }
        if (deflate) {
            W[, j_w] <- W[, j_w] - ds * drop(cross(dv, VJ)) * 
                du
        }
        if (iter != 1 && w_dim > 1 && reorth) {
            W[, j] <- orthog(W[, j, drop = FALSE], W[, 1:(j - 
                1), drop = FALSE])
        }
        S <- norm2(W[, j_w, drop = FALSE])
        if (is.na(S) || S < eps2 && j == 1) 
            stop("starting vector near the null space")
        if (is.na(S) || S < eps2) {
            W[, j_w] <- rnorm(nrow(W))
            if (w_dim > 1) 
                W[, j] <- orthog(W[, j], W[, 1:(j - 1)])
            W[, j_w] <- W[, j_w]/norm2(W[, j_w])
            S <- 0
        }
        else W[, j_w] <- W[, j_w]/S
        while (j <= work) {
            j_w <- ifelse(w_dim > 1, j, 1)
            if (iscomplex) {
                F <- Conj(t(drop(mult(Conj(drop(W[, j_w])), A))))
            }
            else F <- t(drop(mult(drop(W[, j_w]), A)))
            if (!missing(shift)) 
                F <- F + shift * W[, j_w]
            if (!missing(scale)) 
                F <- F/scale
            mprod <- mprod + 1
            F <- drop(F - S * V[, j])
            F <- orthog(F, V[, 1:j, drop = FALSE])
            if (j + 1 <= work) {
                R <- norm2(F)
                if (R < eps2) {
                  F <- matrix(rnorm(dim(V)[1]), dim(V)[1], 1)
                  F <- orthog(F, V[, 1:j, drop = FALSE])
                  V[, j + 1] <- F/norm2(F)
                  R <- 0
                }
                else V[, j + 1] <- F/R
                if (is.null(B)) 
                  B <- cbind(S, R)
                else B <- rbind(cbind(B, 0), c(rep(0, ncol(B) - 
                  1), S, R))
                jp1_w <- ifelse(w_dim > 1, j + 1, 1)
                w_old <- W[, j_w]
                VJP1 <- V[, j + 1]
                if (!missing(scale)) {
                  VJP1 <- VJP1/scale
                }
                W[, jp1_w] <- drop(mult(A, drop(VJP1)))
                mprod <- mprod + 1
                if (!missing(shift)) {
                  W[, jp1_w] <- W[, jp1_w] + shift * VJP1
                }
                if (deflate) {
                  W[, jp1_w] <- W[, jp1_w] - ds * drop(cross(dv, 
                    VJP1)) * du
                }
                W[, jp1_w] <- W[, jp1_w] - R * w_old
                if (reorth && w_dim > 1) 
                  W[, j + 1] <- orthog(W[, j + 1], W[, 1:j])
                S <- norm2(W[, jp1_w])
                if (S < eps2) {
                  W[, jp1_w] <- rnorm(nrow(W))
                  if (w_dim > 1) 
                    W[, j + 1] <- orthog(W[, j + 1], W[, 1:j])
                  W[, jp1_w] <- W[, jp1_w]/norm2(W[, jp1_w])
                  S <- 0
                }
                else W[, jp1_w] <- W[, jp1_w]/S
            }
            else {
                B <- rbind(B, c(rep(0, j - 1), S))
            }
            j <- j + 1
        }
        if (verbose) {
            message("Lanczos iter = ", iter, ", dim = ", j - 
                1, ", mprod = ", mprod)
        }
        Bsz <- nrow(B)
        R_F <- norm2(F)
        F <- F/R_F
        Bsvd <- svd(B)
        if (iter == 1) {
            Smax <- Bsvd$d[1]
            Smin <- Bsvd$d[Bsz]
        }
        else {
            Smax <- max(Smax, Bsvd$d[1])
            Smin <- min(Smin, Bsvd$d[Bsz])
        }
        Smax <- max(eps23, Smax)
        if (Smin/Smax < sqrteps && !reorth) {
            warning("The matrix is ill-conditioned. Basis will be reorthogonalized.")
            reorth <- TRUE
        }
        R <- R_F * Bsvd$u[Bsz, , drop = FALSE]
        ct <- convtests(Bsz, tol, k_org, Bsvd$u, Bsvd$d, Bsvd$v, 
            abs(R), k, SVTol, Smax)
        k <- ct$k
        if (ct$converged) 
            break
        if (iter >= maxit) 
            break
        V[, 1:(k + dim(F)[2])] <- cbind(V[, 1:(dim(Bsvd$v)[1]), 
            drop = FALSE] %*% Bsvd$v[, 1:k], F)
        B <- cbind(diag(Bsvd$d[1:k], nrow = k), R[1:k])
        if (w_dim > 1) {
            W[, 1:k] <- W[, 1:(dim(Bsvd$u)[1]), drop = FALSE] %*% 
                Bsvd$u[, 1:k]
        }
        iter <- iter + 1
    }
    if (iter > maxit) 
        warning("did not converge--results might be invalid!; try increasing maxit")
    d <- Bsvd$d[1:k_org]
    if (!right_only) {
        u <- W[, 1:(dim(Bsvd$u)[1]), drop = FALSE] %*% Bsvd$u[, 
            1:k_org, drop = FALSE]
    }
    v <- V[, 1:(dim(Bsvd$v)[1]), drop = FALSE] %*% Bsvd$v[, 1:k_org, 
        drop = FALSE]
    if (tol * d[1] < eps) 
        warning("convergence criterion below machine epsilon")
    if (right_only) 
        return(list(d = d, v = v[, 1:nv, drop = FALSE], iter = iter, 
            mprod = mprod))
    return(list(d = d, u = u[, 1:nu, drop = FALSE], v = v[, 1:nv, 
        drop = FALSE], iter = iter, mprod = mprod))
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Fast Truncated SVD, PCA and Symmetric Eigendecomposition forLarge Dense and Sparse Matrices"

.skeleton_package_version = "2.1.2"

.skeleton_package_depends = "Matrix"

.skeleton_package_imports = "stats,methods"


## Internal

.skeleton_version = 5


## EOF