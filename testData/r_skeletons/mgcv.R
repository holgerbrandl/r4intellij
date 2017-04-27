##
## Exported symobls in package `mgcv`
##

## Exported package methods

null.space.dimension <- function (d, m) 
{
    if (sum(d < 0)) 
        stop("d can not be negative in call to null.space.dimension().")
    ind <- 2 * m < d + 1
    if (sum(ind)) {
        m[ind] <- 1
        ind <- 2 * m < d + 2
        while (sum(ind)) {
            m[ind] <- m[ind] + 1
            ind <- 2 * m < d + 2
        }
    }
    M <- m * 0 + 1
    ind <- M == 1
    i <- 0
    while (sum(ind)) {
        M[ind] <- M[ind] * (d[ind] + m[ind] - 1 - i)
        i <- i + 1
        ind <- i < d
    }
    ind <- d > 1
    i <- 2
    while (sum(ind)) {
        M[ind] <- M[ind]/i
        ind <- d > i
        i <- i + 1
    }
    M
}


smooth.construct.ds.smooth.spec <- function (object, data, knots) 
{
    xtra <- list()
    if (is.null(object$xt$max.knots)) 
        xtra$max.knots <- 2000
    else xtra$max.knots <- object$xt$max.knots
    if (is.null(object$xt$seed)) 
        xtra$seed <- 1
    else xtra$seed <- object$xt$seed
    x <- array(0, 0)
    for (i in 1:object$dim) {
        xx <- data[[object$term[i]]]
        if (i == 1) 
            n <- length(xx)
        else if (n != length(xx)) 
            stop("arguments of smooth not same dimension")
        x <- c(x, xx)
    }
    if (is.null(knots)) {
        knt <- 0
        nk <- 0
    }
    else {
        knt <- array(0, 0)
        for (i in 1:object$dim) {
            dum <- knots[[object$term[i]]]
            if (is.null(dum)) {
                knt <- 0
                nk <- 0
                break
            }
            knt <- c(knt, dum)
            nk0 <- length(dum)
            if (i > 1 && nk != nk0) 
                stop("components of knots relating to a single smooth must be of same length")
            nk <- nk0
        }
    }
    if (nk > n) {
        nk <- 0
        warning("more knots than data in a ds term: knots ignored.")
    }
    xu <- uniquecombs(matrix(x, n, object$dim))
    if (nrow(xu) < object$bs.dim) 
        stop("A term has fewer unique covariate combinations than specified maximum degrees of freedom")
    if (nk == 0) {
        nu <- nrow(xu)
        if (n > xtra$max.knots) {
            if (nu > xtra$max.knots) {
                seed <- try(get(".Random.seed", envir = .GlobalEnv), 
                  silent = TRUE)
                if (inherits(seed, "try-error")) {
                  runif(1)
                  seed <- get(".Random.seed", envir = .GlobalEnv)
                }
                kind <- RNGkind(NULL)
                RNGkind("default", "default")
                set.seed(xtra$seed)
                nk <- xtra$max.knots
                ind <- sample(1:nu, nk, replace = FALSE)
                knt <- as.numeric(xu[ind, ])
                RNGkind(kind[1], kind[2])
                assign(".Random.seed", seed, envir = .GlobalEnv)
            }
            else {
                knt <- xu
                nk <- nu
            }
        }
        else {
            knt <- xu
            nk <- nu
        }
    }
    if (is.na(object$p.order[1])) 
        object$p.order[1] <- 2
    if (is.na(object$p.order[2])) 
        object$p.order[2] <- 0
    object$p.order[1] <- round(object$p.order[1])
    object$p.order[2] <- round(object$p.order[2] * 2)/2
    if (object$p.order[1] < 1) 
        object$p.order[1] <- 1
    if (object$p.order[2] >= object$dim/2) {
        object$p.order[2] <- (object$dim - 1)/2
        warning("s value reduced")
    }
    if (object$p.order[2] <= -object$dim/2) {
        object$p.order[2] <- -(object$dim - 1)/2
        warning("s value increased")
    }
    if (sum(object$p.order) <= object$dim/2) {
        object$p.order[2] <- 1/2 + object$dim/2 - object$p.order[1]
        if (object$p.order[2] >= object$dim/2) 
            stop("No suitable s (i.e. m[2]) try increasing m[1]")
        warning("s value modified to give continuous function")
    }
    x <- matrix(x, n, object$dim)
    knt <- matrix(knt, nk, object$dim)
    object$shift <- colMeans(x)
    x <- sweep(x, 2, object$shift)
    knt <- sweep(knt, 2, object$shift)
    E <- DuchonE(knt, knt, m = object$p.order[1], s = object$p.order[2], 
        n = object$dim)
    T <- DuchonT(knt, m = object$p.order[1], n = object$dim)
    ind <- 1:ncol(T)
    def.k <- c(10, 30, 100)
    dd <- min(object$dim, length(def.k))
    if (object$bs.dim[1] < 0) 
        object$bs.dim <- ncol(T) + def.k[dd]
    if (object$bs.dim < ncol(T) + 1) {
        object$bs.dim <- ncol(T) + 1
        warning("basis dimension reset to minimum possible")
    }
    k <- object$bs.dim
    if (k < nk) {
        er <- slanczos(E, k, -1)
        D <- diag(er$values)
        U1 <- t(t(T) %*% er$vectors)
    }
    else {
        U1 <- T
        D <- E
        er <- list(vectors = diag(k))
    }
    rm(E)
    qru <- qr(U1)
    S <- qr.qty(qru, t(qr.qty(qru, D)[-ind, ]))[-ind, ]
    object$S <- list(S = rbind(cbind(S, matrix(0, k - length(ind), 
        length(ind))), matrix(0, length(ind), k)))
    object$UZ <- t(qr.qty(qru, t(er$vectors))[-ind, ])
    object$knt = knt
    object$df <- object$bs.dim
    object$null.space.dim <- length(ind)
    object$rank <- k - length(ind)
    class(object) <- "duchon.spline"
    object$X <- Predict.matrix.duchon.spline(object, data)
    object
}


fs.test <- function (x, y, r0 = 0.1, r = 0.5, l = 3, b = 1, exclude = TRUE) 
{
    q <- pi * r/2
    a <- d <- x * 0
    ind <- x >= 0 & y > 0
    a[ind] <- q + x[ind]
    d[ind] <- y[ind] - r
    ind <- x >= 0 & y <= 0
    a[ind] <- -q - x[ind]
    d[ind] <- -r - y[ind]
    ind <- x < 0
    a[ind] <- -atan(y[ind]/x[ind]) * r
    d[ind] <- sqrt(x[ind]^2 + y[ind]^2) - r
    ind <- abs(d) > r - r0 | (x > l & (x - l)^2 + d^2 > (r - 
        r0)^2)
    f <- a * b + d^2
    if (exclude) 
        f[ind] <- NA
    attr(f, "exclude") <- ind
    f
}


fix.family.rd <- function (fam) 
{
    if (!inherits(fam, "family")) 
        stop("fam not a family object")
    if (!is.null(fam$rd)) 
        return(fam)
    family <- fam$family
    if (family == "poisson") {
        fam$rd <- function(mu, wt, scale) {
            rpois(length(mu), mu)
        }
    }
    else if (family == "binomial") {
        fam$rd <- function(mu, wt, scale) {
            rbinom(mu, wt, mu)/wt
        }
    }
    else if (family == "Gamma") {
        fam$rd <- function(mu, wt, scale) {
            rgamma(mu, shape = 1/scale, scale = mu * scale)
        }
    }
    else if (family == "gaussian") {
        fam$rd <- function(mu, wt, scale) {
            rnorm(mu, mean = mu, sd = sqrt(scale/wt))
        }
    }
    else if (family == "inverse.gaussian") {
        fam$rd <- function(mu, wt, scale) {
            rig(mu, mu, scale)
        }
    }
    fam
}


Predict.matrix.mrf.smooth <- function (object, data) 
{
    x <- factor(data[[object$term]], levels = levels(object$knots))
    X <- model.matrix(~x - 1)
    if (!is.null(object$P)) 
        X <- X %*% object$P
    X
}


tensor.prod.penalties <- function (S) 
{
    m <- length(S)
    I <- list()
    for (i in 1:m) {
        n <- ncol(S[[i]])
        I[[i]] <- diag(n)
    }
    TS <- list()
    if (m == 1) 
        TS[[1]] <- S[[1]]
    else for (i in 1:m) {
        if (i == 1) 
            M0 <- S[[1]]
        else M0 <- I[[1]]
        for (j in 2:m) {
            if (i == j) 
                M1 <- S[[i]]
            else M1 <- I[[j]]
            M0 <- M0 %x% M1
        }
        TS[[i]] <- if (ncol(M0) == nrow(M0)) 
            (M0 + t(M0))/2
        else M0
    }
    TS
}


formXtViX <- function (V, X) 
{
    X <- X[V$ind, , drop = FALSE]
    if (is.list(V$V)) {
        Z <- X
        j0 <- 1
        for (i in 1:length(V$V)) {
            Cv <- chol(V$V[[i]])
            j1 <- j0 + nrow(V$V[[i]]) - 1
            Z[j0:j1, ] <- backsolve(Cv, X[j0:j1, , drop = FALSE], 
                transpose = TRUE)
            j0 <- j1 + 1
        }
    }
    else if (is.matrix(V$V)) {
        Cv <- chol(V$V)
        Z <- backsolve(Cv, X, transpose = TRUE)
    }
    else {
        Z <- X/sqrt(as.numeric(V$V))
    }
    qrz <- qr(Z, LAPACK = TRUE)
    R <- qr.R(qrz)
    R[, qrz$pivot] <- R
    colnames(R) <- colnames(X)
    R
}


in.out <- function (bnd, x) 
{
    if (!is.matrix(x)) 
        x <- matrix(x, 1, 2)
    lowLim <- min(bnd, na.rm = TRUE) - mean(abs(bnd), na.rm = TRUE)
    ind <- is.na(rowSums(bnd))
    bnd[ind, ] <- lowLim
    n <- nrow(bnd)
    um <- .C(C_in_out, bx = as.double(bnd[, 1]), by = as.double(bnd[, 
        2]), break.code = as.double(lowLim), x = as.double(x[, 
        1]), y = as.double(x[, 2]), inside = as.integer(x[, 2] * 
        0), nb = as.integer(n), n = as.integer(nrow(x)))
    as.logical(um$inside)
}


pen.edf <- function (x) 
{
    if (!inherits(x, "gam")) 
        stop("not a gam object")
    if (length(x$smooth) == 0) 
        return(NULL)
    k <- 0
    edf <- rep(0, 0)
    edf.name <- rep("", 0)
    for (i in 1:length(x$smooth)) {
        if (length(x$smooth[[i]]$S) > 0) {
            pind <- x$smooth[[i]]$first.para:x$smooth[[i]]$last.para
            Snames <- names(x$smooth[[i]]$S)
            if (is.null(Snames)) 
                Snames <- as.character(1:length(x$smooth[[i]]$S))
            if (length(Snames) == 1) 
                Snames <- ""
            for (j in 1:length(x$smooth[[i]]$S)) {
                ind <- rowSums(x$smooth[[i]]$S[[j]] != 0) != 
                  0
                k <- k + 1
                edf[k] <- sum(x$edf[pind[ind]])
                edf.name[k] <- paste(x$smooth[[i]]$label, Snames[j], 
                  sep = "")
            }
        }
    }
    names(edf) <- edf.name
    if (k == 0) 
        return(NULL)
    edf
}


te <- function (..., k = NA, bs = "cr", m = NA, d = NA, by = NA, fx = FALSE, 
    mp = TRUE, np = TRUE, xt = NULL, id = NULL, sp = NULL) 
{
    vars <- as.list(substitute(list(...)))[-1]
    dim <- length(vars)
    by.var <- deparse(substitute(by), backtick = TRUE)
    term <- deparse(vars[[1]], backtick = TRUE)
    if (dim > 1) 
        for (i in 2:dim) term[i] <- deparse(vars[[i]], backtick = TRUE)
    for (i in 1:dim) term[i] <- attr(terms(reformulate(term[i])), 
        "term.labels")
    if (sum(is.na(d)) || is.null(d)) {
        n.bases <- dim
        d <- rep(1, dim)
    }
    else {
        d <- round(d)
        ok <- TRUE
        if (sum(d <= 0)) 
            ok <- FALSE
        if (sum(d) != dim) 
            ok <- FALSE
        if (ok) 
            n.bases <- length(d)
        else {
            warning("something wrong with argument d.")
            n.bases <- dim
            d <- rep(1, dim)
        }
    }
    if (sum(is.na(k)) || is.null(k)) 
        k <- 5^d
    else {
        k <- round(k)
        ok <- TRUE
        if (sum(k < 3)) {
            ok <- FALSE
            warning("one or more supplied k too small - reset to default")
        }
        if (length(k) == 1 && ok) 
            k <- rep(k, n.bases)
        else if (length(k) != n.bases) 
            ok <- FALSE
        if (!ok) 
            k <- 5^d
    }
    if (sum(is.na(fx)) || is.null(fx)) 
        fx <- rep(FALSE, n.bases)
    else if (length(fx) == 1) 
        fx <- rep(fx, n.bases)
    else if (length(fx) != n.bases) {
        warning("dimension of fx is wrong")
        fx <- rep(FALSE, n.bases)
    }
    xtra <- list()
    if (is.null(xt) || length(xt) == 1) 
        for (i in 1:n.bases) xtra[[i]] <- xt
    else if (length(xt) == n.bases) 
        xtra <- xt
    else stop("xt argument is faulty.")
    if (length(bs) == 1) 
        bs <- rep(bs, n.bases)
    if (length(bs) != n.bases) {
        warning("bs wrong length and ignored.")
        bs <- rep("cr", n.bases)
    }
    bs[d > 1 & (bs == "cr" | bs == "cs" | bs == "ps" | bs == 
        "cp")] <- "tp"
    if (!is.list(m) && length(m) == 1) 
        m <- rep(m, n.bases)
    if (length(m) != n.bases) {
        warning("m wrong length and ignored.")
        m <- rep(0, n.bases)
    }
    if (!is.list(m)) 
        m[m < 0] <- 0
    if (length(unique(term)) != dim) 
        stop("Repeated variables as arguments of a smooth are not permitted")
    j <- 1
    margin <- list()
    for (i in 1:n.bases) {
        j1 <- j + d[i] - 1
        if (is.null(xt)) 
            xt1 <- NULL
        else xt1 <- xtra[[i]]
        stxt <- "s("
        for (l in j:j1) stxt <- paste(stxt, term[l], ",", sep = "")
        stxt <- paste(stxt, "k=", deparse(k[i], backtick = TRUE), 
            ",bs=", deparse(bs[i], backtick = TRUE), ",m=", deparse(m[[i]], 
                backtick = TRUE), ",xt=xt1", ")")
        margin[[i]] <- eval(parse(text = stxt))
        j <- j1 + 1
    }
    if (mp) 
        mp <- TRUE
    else mp <- FALSE
    if (np) 
        np <- TRUE
    else np <- FALSE
    full.call <- paste("te(", term[1], sep = "")
    if (dim > 1) 
        for (i in 2:dim) full.call <- paste(full.call, ",", term[i], 
            sep = "")
    label <- paste(full.call, ")", sep = "")
    if (!is.null(id)) {
        if (length(id) > 1) {
            id <- id[1]
            warning("only first element of `id' used")
        }
        id <- as.character(id)
    }
    ret <- list(margin = margin, term = term, by = by.var, fx = fx, 
        label = label, dim = dim, mp = mp, np = np, id = id, 
        sp = sp, inter = FALSE)
    class(ret) <- "tensor.smooth.spec"
    ret
}


smooth.construct.cp.smooth.spec <- function (object, data, knots) 
{
    if (length(object$p.order) == 1) 
        m <- rep(object$p.order, 2)
    else m <- object$p.order
    m[is.na(m)] <- 2
    object$p.order <- m
    if (object$bs.dim < 0) 
        object$bs.dim <- max(10, m[1])
    nk <- object$bs.dim + 1
    if (nk <= m[1]) 
        stop("basis dimension too small for b-spline order")
    if (length(object$term) != 1) 
        stop("Basis only handles 1D smooths")
    x <- data[[object$term]]
    k <- knots[[object$term]]
    if (is.null(k)) {
        x0 <- min(x)
        x1 <- max(x)
    }
    else if (length(k) == 2) {
        x0 <- min(k)
        x1 <- max(k)
        if (x0 > min(x) || x1 < max(x)) 
            stop("knot range does not include data")
    }
    if (is.null(k) || length(k) == 2) {
        k <- seq(x0, x1, length = nk)
    }
    else {
        if (length(k) != nk) 
            stop(paste("there should be ", nk, " supplied knots"))
    }
    if (length(k) != nk) 
        stop(paste("there should be", nk, "knots supplied"))
    object$X <- cSplineDes(x, k, ord = m[1] + 2)
    if (!is.null(k)) {
        if (sum(colSums(object$X) == 0) > 0) 
            warning("knot range is so wide that there is *no* information about some basis coefficients")
    }
    p.ord <- m[2]
    np <- ncol(object$X)
    if (p.ord > np - 1) 
        stop("penalty order too high for basis dimension")
    De <- diag(np + p.ord)
    if (p.ord > 0) {
        for (i in 1:p.ord) De <- diff(De)
        D <- De[, -(1:p.ord)]
        D[, (np - p.ord + 1):np] <- D[, (np - p.ord + 1):np] + 
            De[, 1:p.ord]
    }
    else D <- De
    object$S <- list(t(D) %*% D)
    object$rank <- np - 1
    object$null.space.dim <- 1
    object$knots <- k
    object$m <- m
    class(object) <- "cpspline.smooth"
    object
}


tensor.prod.model.matrix <- function (X) 
{
    m <- length(X)
    d <- unlist(lapply(X, ncol))
    n <- nrow(X[[1]])
    X <- as.numeric(unlist(X))
    T <- numeric(n * prod(d))
    .Call(C_mgcv_tmm, X, T, d, m, n)
    attr(T, "dim") <- c(n, prod(d))
    class(T) <- "matrix"
    T
}


betar <- function (theta = NULL, link = "logit", eps = .Machine$double.eps * 
    100) 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    if (linktemp %in% c("logit", "probit", "cloglog", "cauchit")) 
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
        else stop(linktemp, " link not available for beta regression; available links are  \"logit\", \"probit\", \"cloglog\" and \"cauchit\"")
    }
    n.theta <- 1
    if (!is.null(theta) && theta != 0) {
        if (theta > 0) {
            iniTheta <- log(theta)
            n.theta <- 0
        }
        else iniTheta <- log(-theta)
    }
    else iniTheta <- 0
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", iniTheta, envir = env)
    assign(".betarEps", eps, envir = env)
    getTheta <- function(trans = FALSE) if (trans) 
        exp(get(".Theta"))
    else get(".Theta")
    putTheta <- function(theta) assign(".Theta", theta, envir = environment(sys.function()))
    variance <- function(mu) {
        th <- get(".Theta")
        mu * (1 - mu)/(1 + exp(th))
    }
    validmu <- function(mu) all(mu > 0 & mu < 1)
    dev.resids <- function(y, mu, wt, theta = NULL) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        theta <- exp(theta)
        muth <- mu * theta
        2 * wt * (-lgamma(theta) + lgamma(muth) + lgamma(theta - 
            muth) - muth * (log(y) - log1p(-y)) - theta * log1p(-y) + 
            log(y) + log1p(-y))
    }
    Dd <- function(y, mu, theta, wt, level = 0) {
        theta <- exp(theta)
        onemu <- 1 - mu
        muth <- mu * theta
        onemuth <- onemu * theta
        psi0.th <- digamma(theta)
        psi1.th <- trigamma(theta)
        psi0.muth <- digamma(muth)
        psi0.onemuth <- digamma(onemuth)
        psi1.muth <- trigamma(muth)
        psi1.onemuth <- trigamma(onemuth)
        psi2.muth <- psigamma(muth, 2)
        psi2.onemuth <- psigamma(onemuth, 2)
        psi3.muth <- psigamma(muth, 3)
        psi3.onemuth <- psigamma(onemuth, 3)
        log.yoney <- log(y) - log1p(-y)
        r <- list()
        r$Dmu <- 2 * wt * theta * (psi0.muth - psi0.onemuth - 
            log.yoney)
        r$Dmu2 <- 2 * wt * theta^2 * (psi1.muth + psi1.onemuth)
        r$EDmu2 <- r$Dmu2
        if (level > 0) {
            r$Dth <- 2 * wt * theta * (-mu * log.yoney - log1p(-y) + 
                mu * psi0.muth + onemu * psi0.onemuth - psi0.th)
            r$Dmuth <- r$Dmu + 2 * wt * theta^2 * (mu * psi1.muth - 
                onemu * psi1.onemuth)
            r$Dmu3 <- 2 * wt * theta^3 * (psi2.muth - psi2.onemuth)
            r$Dmu2th <- 2 * r$Dmu2 + 2 * wt * theta^3 * (mu * 
                psi2.muth + onemu * psi2.onemuth)
        }
        if (level > 1) {
            r$Dmu4 <- 2 * wt * theta^4 * (psi3.muth + psi3.onemuth)
            r$Dth2 <- r$Dth + 2 * wt * theta^2 * (mu^2 * psi1.muth + 
                onemu^2 * psi1.onemuth - psi1.th)
            r$Dmuth2 <- r$Dmuth + 2 * wt * theta^2 * (mu^2 * 
                theta * psi2.muth + 2 * mu * psi1.muth - theta * 
                onemu^2 * psi2.onemuth - 2 * onemu * psi1.onemuth)
            r$Dmu2th2 <- 2 * r$Dmu2th + 2 * wt * theta^3 * (mu^2 * 
                theta * psi3.muth + 3 * mu * psi2.muth + onemu^2 * 
                theta * psi3.onemuth + 3 * onemu * psi2.onemuth)
            r$Dmu3th <- 3 * r$Dmu3 + 2 * wt * theta^4 * (mu * 
                psi3.muth - onemu * psi3.onemuth)
        }
        r
    }
    aic <- function(y, mu, theta = NULL, wt, dev) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        theta <- exp(theta)
        muth <- mu * theta
        term <- -lgamma(theta) + lgamma(muth) + lgamma(theta - 
            muth) - (muth - 1) * log(y) - (theta - muth - 1) * 
            log1p(-y)
        2 * sum(term * wt)
    }
    ls <- function(y, w, n, theta, scale) {
        list(ls = 0, lsth1 = 0, lsth2 = 0)
    }
    preinitialize <- NULL
    eval(parse(text = paste("preinitialize <- expression({\n eps <- ", 
        eps, "\n G$y[G$y >= 1-eps] <- 1 - eps\n  G$y[G$y<= eps] <- eps })")))
    saturated.ll <- function(y, wt, theta = NULL) {
        gbh <- function(y, eta, phi, deriv = FALSE, a = 1e-08, 
            b = 1 - a) {
            ind <- eta > 0
            expeta <- mu <- eta
            expeta[ind] <- exp(-eta[ind])
            expeta[!ind] <- exp(eta[!ind])
            mu[ind] <- (a * expeta[ind] + b)/(1 + expeta[ind])
            mu[!ind] <- (a + b * expeta[!ind])/(1 + expeta[!ind])
            l <- dbeta(y, phi * mu, phi * (1 - mu), log = TRUE)
            if (deriv) {
                g <- phi * log(y) - phi * log1p(-y) - phi * digamma(mu * 
                  phi) + phi * digamma((1 - mu) * phi)
                h <- -phi^2 * (trigamma(mu * phi) + trigamma((1 - 
                  mu) * phi))
                dmueta2 <- dmueta1 <- eta
                dmueta1 <- expeta * (b - a)/(1 + expeta)^2
                dmueta2 <- sign(eta) * ((a - b) * expeta + (b - 
                  a) * expeta^2)/(expeta + 1)^3
                h <- h * dmueta1^2 + g * dmueta2
                g <- g * dmueta1
            }
            else g = h = NULL
            list(l = l, g = g, h = h, mu = mu)
        }
        eps <- get(".betarEps")
        eta <- y
        a <- eps
        b <- 1 - eps
        y[y < eps] <- eps
        y[y > 1 - eps] <- 1 - eps
        eta[y <= eps * 1.2] <- eps * 1.2
        eta[y >= 1 - eps * 1.2] <- 1 - eps * 1.2
        eta <- log((eta - a)/(b - eta))
        mu <- LS <- ii <- 1:length(y)
        for (i in 1:200) {
            ls <- gbh(y, eta, theta, TRUE, a = eps/10)
            conv <- abs(ls$g) < mean(abs(ls$l) + 0.1) * 1e-08
            if (sum(conv) > 0) {
                LS[ii[conv]] <- ls$l[conv]
                mu[ii[conv]] <- ls$mu[conv]
                ii <- ii[!conv]
                if (length(ii) > 0) {
                  y <- y[!conv]
                  eta <- eta[!conv]
                  ls$l <- ls$l[!conv]
                  ls$g <- ls$g[!conv]
                  ls$h <- ls$h[!conv]
                }
                else break
            }
            h <- -ls$h
            hmin <- max(h) * 1e-04
            h[h < hmin] <- hmin
            delta <- ls$g/h
            ind <- abs(delta) > 2
            delta[ind] <- sign(delta[ind]) * 2
            ls1 <- gbh(y, eta + delta, theta, FALSE, a = eps/10)
            ind <- ls1$l < ls$l
            k <- 0
            while (sum(ind) > 0 && k < 20) {
                k <- k + 1
                delta[ind] <- delta[ind]/2
                ls1$l[ind] <- gbh(y[ind], eta[ind] + delta[ind], 
                  theta, FALSE, a = eps/10)$l
                ind <- ls1$l < ls$l
            }
            eta <- eta + delta
        }
        if (length(ii) > 0) {
            LS[ii] <- ls$l
            warning("saturated likelihood may be inaccurate")
        }
        list(f = sum(wt * LS), term = LS, mu = mu)
    }
    postproc <- expression({
        wts <- object$prior.weights
        theta <- object$family$getTheta(trans = TRUE)
        lf <- object$family$saturated.ll(G$y, wts, theta)
        object$family$data <- list(ls = lf$term, mu.ls = lf$mu)
        l2 <- object$family$dev.resids(G$y, object$fitted.values, 
            wts)
        object$deviance <- 2 * lf$f + sum(l2)
        wtdmu <- if (G$intercept) sum(wts * G$y)/sum(wts) else object$family$linkinv(G$offset)
        object$null.deviance <- 2 * lf$f + sum(object$family$dev.resids(G$y, 
            wtdmu, wts))
        object$family$family <- paste("Beta regression(", round(theta, 
            3), ")", sep = "")
    })
    initialize <- expression({
        n <- rep(1, nobs)
        mustart <- y
    })
    residuals <- function(object, type = c("deviance", "working", 
        "response", "pearson")) {
        if (type == "working") {
            res <- object$residuals
        }
        else if (type == "response") {
            res <- object$y - object$fitted.values
        }
        else if (type == "deviance") {
            y <- object$y
            mu <- object$fitted.values
            wts <- object$prior.weights
            lf <- object$family$saturated.ll(y, wts, object$family$getTheta(TRUE))
            object$family$data$ls <- lf$term
            res <- 2 * object$family$data$ls + object$family$dev.resids(y, 
                mu, wts)
            res[res < 0] <- 0
            s <- sign(y - mu)
            res <- sqrt(res) * s
        }
        else if (type == "pearson") {
            mu <- object$fitted.values
            res <- (object$y - mu)/object$family$variance(mu)^0.5
        }
        res
    }
    rd <- function(mu, wt, scale) {
        Theta <- exp(get(".Theta"))
        r <- rbeta(mu, shape1 = Theta * mu, shape2 = Theta * 
            (1 - mu))
        eps <- get(".betarEps")
        r[r >= 1 - eps] <- 1 - eps
        r[r < eps] <- eps
        r
    }
    qf <- function(p, mu, wt, scale) {
        Theta <- exp(get(".Theta"))
        q <- qbeta(p, shape1 = Theta * mu, shape2 = Theta * (1 - 
            mu))
        eps <- get(".betarEps")
        q[q >= 1 - eps] <- 1 - eps
        q[q < eps] <- eps
        q
    }
    environment(dev.resids) <- environment(aic) <- environment(getTheta) <- environment(rd) <- environment(qf) <- environment(variance) <- environment(putTheta) <- environment(saturated.ll) <- env
    structure(list(family = "Beta regression", link = linktemp, 
        linkfun = stats$linkfun, linkinv = stats$linkinv, dev.resids = dev.resids, 
        Dd = Dd, variance = variance, aic = aic, mu.eta = stats$mu.eta, 
        initialize = initialize, ls = ls, preinitialize = preinitialize, 
        postproc = postproc, residuals = residuals, saturated.ll = saturated.ll, 
        validmu = validmu, valideta = stats$valideta, n.theta = n.theta, 
        ini.theta = iniTheta, putTheta = putTheta, getTheta = getTheta, 
        rd = rd, qf = qf), class = c("extended.family", "family"))
}


extract.lme.cov2 <- function (b, data, start.level = 1) 
{
    if (!inherits(b, "lme")) 
        stop("object does not appear to be of class lme")
    grps <- nlme::getGroups(b)
    n <- length(grps)
    n.levels <- length(b$groups)
    if (is.null(b$modelStruct$corStruct)) 
        n.corlevels <- 0
    else n.corlevels <- length(all.vars(nlme::getGroupsFormula(b$modelStruct$corStruct)))
    if (n.levels < n.corlevels) {
        getGroupsFormula(b$modelStruct$corStruct)
        vnames <- all.vars(nlme::getGroupsFormula(b$modelStruct$corStruct))
        lab <- paste(eval(parse(text = vnames[1]), envir = b$data))
        if (length(vnames) > 1) 
            for (i in 2:length(vnames)) {
                lab <- paste(lab, "/", eval(parse(text = vnames[i]), 
                  envir = b$data), sep = "")
            }
        grps <- factor(lab)
    }
    if (n.levels >= start.level || n.corlevels >= start.level) {
        if (n.levels >= start.level) 
            Cgrps <- nlme::getGroups(b, level = start.level)
        else Cgrps <- grps
        Cind <- sort(as.numeric(Cgrps), index.return = TRUE)$ix
        rCind <- 1:n
        rCind[Cind] <- 1:n
        Clevel <- levels(Cgrps)
        n.cg <- length(Clevel)
        size.cg <- array(0, n.cg)
        for (i in 1:n.cg) size.cg[i] <- sum(Cgrps == Clevel[i])
    }
    else {
        n.cg <- 1
        Cind <- 1:n
    }
    if (is.null(b$modelStruct$varStruct)) 
        w <- rep(b$sigma, n)
    else {
        w <- 1/nlme::varWeights(b$modelStruct$varStruct)
        group.name <- names(b$groups)
        order.txt <- paste("ind<-order(data[[\"", group.name[1], 
            "\"]]", sep = "")
        if (length(b$groups) > 1) 
            for (i in 2:length(b$groups)) order.txt <- paste(order.txt, 
                ",data[[\"", group.name[i], "\"]]", sep = "")
        order.txt <- paste(order.txt, ")")
        eval(parse(text = order.txt))
        w[ind] <- w
        w <- w * b$sigma
    }
    w <- w[Cind]
    if (is.null(b$modelStruct$corStruct)) 
        V <- array(1, n)
    else {
        c.m <- nlme::corMatrix(b$modelStruct$corStruct)
        if (!is.list(c.m)) {
            V <- c.m
            V <- V[Cind, ]
            V <- V[, Cind]
        }
        else {
            V <- list()
            ind <- list()
            for (i in 1:n.cg) {
                V[[i]] <- matrix(0, size.cg[i], size.cg[i])
                ind[[i]] <- 1:size.cg[i]
            }
            Voff <- cumsum(c(1, size.cg))
            gr.name <- names(c.m)
            n.g <- length(c.m)
            j0 <- rep(1, n.cg)
            ii <- 1:n
            for (i in 1:n.g) {
                Clev <- unique(Cgrps[grps == gr.name[i]])
                if (length(Clev) > 1) 
                  stop("inner groupings not nested in outer!!")
                k <- (1:n.cg)[Clevel == Clev]
                j1 <- j0[k] + nrow(c.m[[i]]) - 1
                V[[k]][j0[k]:j1, j0[k]:j1] <- c.m[[i]]
                ind1 <- ii[grps == gr.name[i]]
                ind2 <- rCind[ind1]
                ind[[k]][j0[k]:j1] <- ind2 - Voff[k] + 1
                j0[k] <- j1 + 1
            }
            for (k in 1:n.cg) {
                V[[k]][ind[[k]], ] <- V[[k]]
                V[[k]][, ind[[k]]] <- V[[k]]
            }
        }
    }
    if (is.list(V)) {
        for (i in 1:n.cg) {
            wi <- w[Voff[i]:(Voff[i] + size.cg[i] - 1)]
            V[[i]] <- as.vector(wi) * t(as.vector(wi) * V[[i]])
        }
    }
    else if (is.matrix(V)) {
        V <- as.vector(w) * t(as.vector(w) * V)
    }
    else {
        V <- w^2 * V
    }
    X <- list()
    grp.dims <- b$dims$ncol
    Zt <- model.matrix(b$modelStruct$reStruct, data)
    cov <- as.matrix(b$modelStruct$reStruct)
    i.col <- 1
    Z <- matrix(0, n, 0)
    if (start.level <= n.levels) {
        for (i in 1:(n.levels - start.level + 1)) {
            if (length(levels(b$groups[[n.levels - i + 1]])) == 
                1) {
                X[[1]] <- matrix(rep(1, nrow(b$groups)))
            }
            else {
                X[[1]] <- model.matrix(~b$groups[[n.levels - 
                  i + 1]] - 1, contrasts.arg = c("contr.treatment", 
                  "contr.treatment"))
            }
            X[[2]] <- Zt[, i.col:(i.col + grp.dims[i] - 1), drop = FALSE]
            i.col <- i.col + grp.dims[i]
            Z <- cbind(Z, tensor.prod.model.matrix(X))
        }
        Vr <- matrix(0, ncol(Z), ncol(Z))
        start <- 1
        for (i in 1:(n.levels - start.level + 1)) {
            k <- n.levels - i + 1
            for (j in 1:b$dims$ngrps[i]) {
                stop <- start + ncol(cov[[k]]) - 1
                Vr[start:stop, start:stop] <- cov[[k]]
                start <- stop + 1
            }
        }
        Vr <- Vr * b$sigma^2
        Z <- Z[Cind, ]
        if (n.cg == 1) {
            if (is.matrix(V)) {
                V <- V + Z %*% Vr %*% t(Z)
            }
            else V <- diag(V) + Z %*% Vr %*% t(Z)
        }
        else {
            j0 <- 1
            Vz <- list()
            for (i in 1:n.cg) {
                j1 <- size.cg[i] + j0 - 1
                Zi <- Z[j0:j1, , drop = FALSE]
                Vz[[i]] <- Zi %*% Vr %*% t(Zi)
                j0 <- j1 + 1
            }
            if (is.list(V)) {
                for (i in 1:n.cg) V[[i]] <- V[[i]] + Vz[[i]]
            }
            else {
                j0 <- 1
                for (i in 1:n.cg) {
                  kk <- size.cg[i]
                  j1 <- kk + j0 - 1
                  Vz[[i]] <- Vz[[i]] + diag(x = V[j0:j1], nrow = kk, 
                    ncol = kk)
                  j0 <- j1 + 1
                }
                V <- Vz
            }
        }
    }
    list(V = V, ind = Cind)
}


jagam <- function (formula, family = gaussian, data = list(), file, weights = NULL, 
    na.action, offset = NULL, knots = NULL, sp = NULL, drop.unused.levels = TRUE, 
    control = gam.control(), centred = TRUE, sp.prior = "gamma", 
    diagonalize = FALSE) 
{
    if (is.null(file)) 
        stop("jagam requires a file for the JAGS model specification")
    cat("model {\n", file = file)
    if (!(sp.prior %in% c("gamma", "log.uniform"))) {
        warning("smoothing parameter prior choise not recognised, reset to gamma")
    }
    if (is.character(family)) 
        family <- eval(parse(text = family))
    if (is.function(family)) 
        family <- family()
    if (is.null(family$family)) 
        stop("family not recognized")
    gp <- interpret.gam(formula)
    cl <- match.call()
    mf <- match.call(expand.dots = FALSE)
    mf$formula <- gp$fake.formula
    mf$family <- mf$knots <- mf$sp <- mf$file <- mf$control <- mf$centred <- mf$sp.prior <- mf$diagonalize <- NULL
    mf$drop.unused.levels <- drop.unused.levels
    mf[[1]] <- as.name("model.frame")
    pmf <- mf
    pmf$formula <- gp$pf
    pmf <- eval(pmf, parent.frame())
    pterms <- attr(pmf, "terms")
    rm(pmf)
    mf <- eval(mf, parent.frame())
    if (nrow(mf) < 2) 
        stop("Not enough (non-NA) data to do anything meaningful")
    terms <- attr(mf, "terms")
    vars <- all.vars(gp$fake.formula[-2])
    inp <- parse(text = paste("list(", paste(vars, collapse = ","), 
        ")"))
    if (!is.list(data) && !is.data.frame(data)) 
        data <- as.data.frame(data)
    dl <- eval(inp, data, parent.frame())
    if (!control$keepData) {
        rm(data)
    }
    names(dl) <- vars
    var.summary <- variable.summary(gp$pf, dl, nrow(mf))
    rm(dl)
    G <- gam.setup(gp, pterms = pterms, data = mf, knots = knots, 
        sp = sp, H = NULL, absorb.cons = centred, sparse.cons = FALSE, 
        select = TRUE, idLinksBases = TRUE, scale.penalty = control$scalePenalty, 
        diagonal.penalty = diagonalize)
    G$model <- mf
    G$terms <- terms
    G$family <- family
    G$call <- cl
    G$var.summary <- var.summary
    use.weights <- if (is.null(weights)) 
        FALSE
    else TRUE
    use.weights <- write.jagslp("y", family, file, use.weights, 
        !is.null(G$offset))
    if (is.null(weights) && use.weights) 
        weights <- rep(1, nrow(G$X))
    jags.stuff <- list(y = G$y, n = length(G$y), X = G$X)
    if (!is.null(G$offset)) 
        jags.stuff$offset <- G$offset
    if (use.weights) 
        jags.stuff$w <- weights
    if (family$family == "binomial") 
        jags.stuff$y <- G$y * weights
    lambda <- initial.spg(G$X, G$y, G$w, family, G$S, G$off, 
        G$L)
    jags.ini <- list()
    lam <- if (is.null(G$L)) 
        lambda
    else G$L %*% lambda
    jin <- jini(G, lam)
    jags.ini$b <- jin$beta
    prior.tau <- signif(0.01/(abs(jin$beta) + jin$se)^2, 2)
    if (G$nsdf > 0) {
        ptau <- min(prior.tau[1:G$nsdf])
        cat("  ## Parametric effect priors CHECK tau=1/", signif(1/sqrt(ptau), 
            2), "^2 is appropriate!\n", file = file, append = TRUE, 
            sep = "")
        cat("  for (i in 1:", G$nsdf, ") { b[i] ~ dnorm(0,", 
            ptau, ") }\n", file = file, append = TRUE, sep = "")
    }
    n.sp <- 0
    for (i in 1:length(G$smooth)) {
        seperable <- FALSE
        M <- length(G$smooth[[i]]$S)
        p <- G$smooth[[i]]$last.para - G$smooth[[i]]$first.para + 
            1
        if (M <= 1) 
            seperable <- TRUE
        else {
            overlap <- rowSums(G$smooth[[i]]$S[[1]])
            for (j in 2:M) overlap <- overlap & rowSums(G$smooth[[i]]$S[[j]])
            if (!sum(overlap)) 
                seperable <- TRUE
        }
        if (seperable) {
            if (M > 0) 
                for (j in 1:M) {
                  if (max(abs(G$smooth[[i]]$S[[j]] - diag(diag(G$smooth[[i]]$S[[j]]), 
                    nrow = p))) > 0) 
                    seperable <- FALSE
                }
        }
        cat("  ## prior for ", G$smooth[[i]]$label, "... \n", 
            file = file, append = TRUE, sep = "")
        if (seperable) {
            b0 <- G$smooth[[i]]$first.para
            if (M == 0) {
                cat("  ## Note fixed vague prior, CHECK tau = 1/", 
                  signif(1/sqrt(ptau), 2), "^2...\n", file = file, 
                  append = TRUE, sep = "")
                b1 <- G$smooth[[i]]$last.para
                ptau <- min(prior.tau[b0:b1])
                cat("  for (i in ", b0, ":", b1, ") { b[i] ~ dnorm(0,", 
                  ptau, ") }\n", file = file, append = TRUE, 
                  sep = "")
            }
            else for (j in 1:M) {
                D <- diag(G$smooth[[i]]$S[[j]]) > 0
                b1 <- sum(as.numeric(D)) + b0 - 1
                n.sp <- n.sp + 1
                cat("  for (i in ", b0, ":", b1, ") { b[i] ~ dnorm(0, lambda[", 
                  n.sp, "]) }\n", file = file, append = TRUE, 
                  sep = "")
                b0 <- b1 + 1
            }
        }
        else {
            b0 <- G$smooth[[i]]$first.para
            b1 <- G$smooth[[i]]$last.para
            Kname <- paste("K", i, sep = "")
            Sname <- paste("S", i, sep = "")
            cat("  ", Kname, " <- ", Sname, "[1:", p, ",1:", 
                p, "] * lambda[", n.sp + 1, "] ", file = file, 
                append = TRUE, sep = "")
            if (M > 1) {
                for (j in 2:M) cat(" + ", Sname, "[1:", p, ",", 
                  (j - 1) * p + 1, ":", j * p, "] * lambda[", 
                  n.sp + j, "]", file = file, append = TRUE, 
                  sep = "")
            }
            cat("\n  b[", b0, ":", b1, "] ~ dmnorm(zero[", b0, 
                ":", b1, "],", Kname, ") \n", file = file, append = TRUE, 
                sep = "")
            n.sp <- n.sp + M
            Sc <- G$smooth[[i]]$S[[1]]
            if (M > 1) 
                for (j in 2:M) Sc <- cbind(Sc, G$smooth[[i]]$S[[j]])
            jags.stuff[[Sname]] <- Sc
            jags.stuff$zero <- rep(0, ncol(G$X))
        }
    }
    cat("  ## smoothing parameter priors CHECK...\n", file = file, 
        append = TRUE, sep = "")
    if (is.null(G$L)) {
        if (sp.prior == "log.uniform") {
            cat("  for (i in 1:", n.sp, ") {\n", file = file, 
                append = TRUE, sep = "")
            cat("    rho[i] ~ dunif(-12,12)\n", file = file, 
                append = TRUE, sep = "")
            cat("    lambda[i] <- exp(rho[i])\n", file = file, 
                append = TRUE, sep = "")
            cat("  }\n", file = file, append = TRUE, sep = "")
            jags.ini$rho <- log(lambda)
        }
        else {
            cat("  for (i in 1:", n.sp, ") {\n", file = file, 
                append = TRUE, sep = "")
            cat("    lambda[i] ~ dgamma(.05,.005)\n", file = file, 
                append = TRUE, sep = "")
            cat("    rho[i] <- log(lambda[i])\n", file = file, 
                append = TRUE, sep = "")
            cat("  }\n", file = file, append = TRUE, sep = "")
            jags.ini$lambda <- lambda
        }
    }
    else {
        jags.stuff$L <- G$L
        rho.lo <- FALSE
        if (any(G$lsp0 != 0)) {
            jags.stuff$rho.lo <- G$lsp0
            rho.lo <- TRUE
        }
        nr <- ncol(G$L)
        if (sp.prior == "log.uniform") {
            cat("  for (i in 1:", nr, ") { rho0[i] ~ dunif(-12,12) }\n", 
                file = file, append = TRUE, sep = "")
            if (rho.lo) 
                cat("  rho <- rho.lo + L %*% rho0\n", file = file, 
                  append = TRUE, sep = "")
            else cat("  rho <- L %*% rho0\n", file = file, append = TRUE, 
                sep = "")
            cat("  for (i in 1:", n.sp, ") { lambda[i] <- exp(rho[i]) }\n", 
                file = file, append = TRUE, sep = "")
            jags.ini$rho0 <- log(lambda)
        }
        else {
            cat("  for (i in 1:", nr, ") {\n", file = file, append = TRUE, 
                sep = "")
            cat("    lambda0[i] ~ dgamma(.05,.005)\n", file = file, 
                append = TRUE, sep = "")
            cat("    rho0[i] <- log(lambda0[i])\n", file = file, 
                append = TRUE, sep = "")
            cat("  }\n", file = file, append = TRUE, sep = "")
            if (rho.lo) 
                cat("  rho <- rho.lo + L %*% rho0\n", file = file, 
                  append = TRUE, sep = "")
            else cat("  rho <- L %*% rho0\n", file = file, append = TRUE, 
                sep = "")
            cat("  for (i in 1:", n.sp, ") { lambda[i] <- exp(rho[i]) }\n", 
                file = file, append = TRUE, sep = "")
            jags.ini$lambda0 <- lambda
        }
    }
    cat("}", file = file, append = TRUE)
    G$formula = formula
    G$rank = ncol(G$X)
    list(pregam = G, jags.data = jags.stuff, jags.ini = jags.ini)
}


ti <- function (..., k = NA, bs = "cr", m = NA, d = NA, by = NA, fx = FALSE, 
    np = TRUE, xt = NULL, id = NULL, sp = NULL, mc = NULL) 
{
    by.var <- deparse(substitute(by), backtick = TRUE)
    object <- te(..., k = k, bs = bs, m = m, d = d, fx = fx, 
        np = np, xt = xt, id = id, sp = sp)
    object$inter <- TRUE
    object$by <- by.var
    object$mc <- mc
    substr(object$label, 2, 2) <- "i"
    object
}


qq.gam <- function (object, rep = 0, level = 0.9, s.rep = 10, type = c("deviance", 
    "pearson", "response"), pch = ".", rl.col = 2, rep.col = "gray80", 
    ...) 
{
    type <- match.arg(type)
    ylab <- paste(type, "residuals")
    if (inherits(object, c("glm", "gam"))) {
        if (is.null(object$sig2)) 
            object$sig2 <- summary(object)$dispersion
    }
    else stop("object is not a glm or gam")
    object$na.action <- NULL
    D <- residuals(object, type = type)
    if (object$method %in% c("PQL", "lme.ML", "lme.REML", "lmer.REML", 
        "lmer.ML", "glmer.ML")) {
        qqnorm(D, ylab = ylab, pch = pch, ...)
        return()
    }
    lim <- Dq <- NULL
    if (rep == 0) {
        fam <- fix.family.qf(object$family)
        if (is.null(fam$qf)) 
            rep <- 50
        level <- 0
    }
    n <- length(D)
    if (rep > 0) {
        fam <- fix.family.rd(object$family)
        if (!is.null(fam$rd)) {
            dm <- matrix(0, n, rep)
            for (i in 1:rep) {
                yr <- fam$rd(object$fitted.values, object$prior.weights, 
                  object$sig2)
                object$y <- yr
                dm[, i] <- sort(residuals(object, type = type))
            }
            Dq <- quantile(as.numeric(dm), (1:n - 0.5)/n)
            alpha <- (1 - level)/2
            if (alpha > 0.5 || alpha < 0) 
                alpha <- 0.05
            if (level > 0 && level < 1) 
                lim <- apply(dm, 1, FUN = quantile, p = c(alpha, 
                  1 - alpha))
            else if (level >= 1) 
                lim <- level
        }
    }
    else {
        ix <- rank(D)
        U <- (ix - 0.5)/length(D)
        if (!is.null(fam$qf)) {
            dm <- matrix(0, n, s.rep)
            for (i in 1:s.rep) {
                U <- sample(U, n)
                q0 <- fam$qf(U, object$fitted.values, object$prior.weights, 
                  object$sig2)
                object$y <- q0
                dm[, i] <- sort(residuals(object, type = type))
            }
            Dq <- sort(rowMeans(dm))
        }
    }
    if (!is.null(Dq)) {
        qqplot(Dq, D, ylab = ylab, xlab = "theoretical quantiles", 
            ylim = range(c(lim, D)), pch = pch, ...)
        abline(0, 1, col = rl.col)
        if (!is.null(lim)) {
            if (level >= 1) 
                for (i in 1:rep) lines(Dq, dm[, i], col = rep.col)
            else {
                n <- length(Dq)
                polygon(c(Dq, Dq[n:1], Dq[1]), c(lim[1, ], lim[2, 
                  n:1], lim[1, 1]), col = rep.col, border = NA)
            }
            abline(0, 1, col = rl.col)
        }
        points(Dq, sort(D), pch = pch, ...)
        return(invisible(Dq))
    }
    else qqnorm(D, ylab = ylab, pch = pch, ...)
}


scat <- function (theta = NULL, link = "identity") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    if (linktemp %in% c("identity", "log", "inverse")) 
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
        else stop(linktemp, " link not available for scaled t distribution; available links are \"identity\", \"log\",  and \"inverse\"")
    }
    n.theta <- 2
    if (!is.null(theta) && sum(theta == 0) == 0) {
        if (abs(theta[1] < 2)) 
            stop("scaled t df must be >2")
        if (sum(theta < 0)) {
            iniTheta <- c(log(abs(theta[1]) - 2), log(abs(theta[2])))
        }
        else {
            iniTheta <- c(log(theta[1] - 2), log(theta[2]))
            n.theta <- 0
        }
    }
    else iniTheta <- c(-2, -1)
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", iniTheta, envir = env)
    getTheta <- function(trans = FALSE) {
        th <- get(".Theta")
        if (trans) {
            th <- exp(th)
            th[1] <- th[1] + 2
        }
        th
    }
    putTheta <- function(theta) assign(".Theta", theta, envir = environment(sys.function()))
    variance <- function(mu) {
        th <- get(".Theta")
        nu <- exp(th[1]) + 2
        sig <- exp(th[2])
        sig^2 * nu/(nu - 2)
    }
    validmu <- function(mu) all(is.finite(mu))
    dev.resids <- function(y, mu, wt, theta = NULL) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        nu <- exp(theta[1]) + 2
        sig <- exp(theta[2])
        wt * (nu + 1) * log1p((1/nu) * ((y - mu)/sig)^2)
    }
    Dd <- function(y, mu, theta, wt, level = 0) {
        nu <- exp(theta[1]) + 2
        sig <- exp(theta[2])
        nu1 <- nu + 1
        ym <- y - mu
        nu2 <- nu - 2
        a <- 1 + (ym/sig)^2/nu
        oo <- list()
        nu1ym <- nu1 * ym
        sig2a <- sig^2 * a
        nusig2a <- nu * sig2a
        f <- nu1ym/nusig2a
        f1 <- ym/nusig2a
        oo$Dmu <- -2 * wt * f
        oo$Dmu2 <- 2 * wt * nu1 * (1/nusig2a - 2 * f1^2)
        term <- 2 * nu1/sig^2/(nu + 3)
        n <- length(y)
        oo$EDmu2 <- rep(term, n)
        if (level > 0) {
            nu1nusig2a <- nu1/nusig2a
            nu2nu <- nu2/nu
            fym <- f * ym
            ff1 <- f * f1
            f1ym <- f1 * ym
            fymf1 <- fym * f1
            ymsig2a <- ym/sig2a
            oo$Dmu2th <- oo$Dmuth <- oo$Dth <- matrix(0, n, 2)
            oo$Dth[, 1] <- 1 * wt * nu2 * (log(a) - fym/nu)
            oo$Dth[, 2] <- -2 * wt * fym
            oo$Dmuth[, 1] <- 2 * wt * (f - ymsig2a - fymf1) * 
                nu2nu
            oo$Dmuth[, 2] <- 4 * wt * f * (1 - f1ym)
            oo$Dmu3 <- 4 * wt * f * (3/nusig2a - 4 * f1^2)
            oo$Dmu2th[, 1] <- 2 * wt * (-nu1nusig2a + 1/sig2a + 
                5 * ff1 - 2 * f1ym/sig2a - 4 * fymf1 * f1) * 
                nu2nu
            oo$Dmu2th[, 2] <- 4 * wt * (-nu1nusig2a + ff1 * 5 - 
                4 * ff1 * f1ym)
        }
        if (level > 1) {
            nu1nu <- nu1/nu
            fymf1ym <- fym * f1ym
            f1ymf1 <- f1ym * f1
            oo$Dmu4 <- 12 * wt * (-nu1nusig2a/nusig2a + 8 * ff1/nusig2a - 
                8 * ff1 * f1^2)
            n2d <- 3
            oo$Dmu3th <- matrix(0, n, 2)
            oo$Dmu2th2 <- oo$Dmuth2 <- oo$Dth2 <- matrix(0, n, 
                n2d)
            oo$Dmu3th[, 1] <- 4 * wt * (-6 * f/nusig2a + 3 * 
                f1/sig2a + 18 * ff1 * f1 - 4 * f1ymf1/sig2a - 
                12 * nu1ym * f1^4) * nu2nu
            oo$Dmu3th[, 2] <- 48 * wt * f * (-1/nusig2a + 3 * 
                f1^2 - 2 * f1ymf1 * f1)
            oo$Dth2[, 1] <- 1 * wt * (nu2 * log(a) + nu2nu * 
                ym^2 * (-2 * nu2 - nu1 + 2 * nu1 * nu2nu - nu1 * 
                nu2nu * f1ym)/nusig2a)
            oo$Dth2[, 2] <- 2 * wt * (fym - ym * ymsig2a - fymf1ym) * 
                nu2nu
            oo$Dth2[, 3] <- 4 * wt * fym * (1 - f1ym)
            term <- 2 * nu2nu - 2 * nu1nu * nu2nu - 1 + nu1nu
            oo$Dmuth2[, 1] <- 2 * wt * f1 * nu2 * (term - 2 * 
                nu2nu * f1ym + 4 * fym * nu2nu/nu - fym/nu - 
                2 * fymf1ym * nu2nu/nu)
            oo$Dmuth2[, 2] <- 4 * wt * (-f + ymsig2a + 3 * fymf1 - 
                ymsig2a * f1ym - 2 * fymf1 * f1ym) * nu2nu
            oo$Dmuth2[, 3] <- 8 * wt * f * (-1 + 3 * f1ym - 2 * 
                f1ym^2)
            oo$Dmu2th2[, 1] <- 2 * wt * nu2 * (-term + 10 * nu2nu * 
                f1ym - 16 * fym * nu2nu/nu - 2 * f1ym + 5 * nu1nu * 
                f1ym - 8 * nu2nu * f1ym^2 + 26 * fymf1ym * nu2nu/nu - 
                4 * nu1nu * f1ym^2 - 12 * nu1nu * nu2nu * f1ym^3)/nusig2a
            oo$Dmu2th2[, 2] <- 4 * wt * (nu1nusig2a - 1/sig2a - 
                11 * nu1 * f1^2 + 5 * f1ym/sig2a + 22 * nu1 * 
                f1ymf1 * f1 - 4 * f1ym^2/sig2a - 12 * nu1 * f1ymf1^2) * 
                nu2nu
            oo$Dmu2th2[, 3] <- 8 * wt * (nu1nusig2a - 11 * nu1 * 
                f1^2 + 22 * nu1 * f1ymf1 * f1 - 12 * nu1 * f1ymf1^2)
        }
        oo
    }
    aic <- function(y, mu, theta = NULL, wt, dev) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        nu <- exp(theta[1]) + 2
        sig <- exp(theta[2])
        term <- -lgamma((nu + 1)/2) + lgamma(nu/2) + log(sig * 
            (pi * nu)^0.5) + (nu + 1) * log1p(((y - mu)/sig)^2/nu)/2
        2 * sum(term * wt)
    }
    ls <- function(y, w, n, theta, scale) {
        nu <- exp(theta[1]) + 2
        sig <- exp(theta[2])
        nu2 <- nu - 2
        nu2nu <- nu2/nu
        nu12 <- (nu + 1)/2
        term <- lgamma(nu12) - lgamma(nu/2) - log(sig * (pi * 
            nu)^0.5)
        ls <- sum(term * w)
        lsth <- rep(0, 2)
        lsth2 <- matrix(0, 2, 2)
        term <- nu2 * digamma(nu12)/2 - nu2 * digamma(nu/2)/2 - 
            0.5 * nu2nu
        lsth[1] <- sum(w * term)
        lsth[2] <- sum(-1 * w)
        term <- nu2^2 * trigamma(nu12)/4 + nu2 * digamma(nu12)/2 - 
            nu2^2 * trigamma(nu/2)/4 - nu2 * digamma(nu/2)/2 + 
            0.5 * (nu2nu)^2 - 0.5 * nu2nu
        lsth2[1, 1] <- sum(term * w)
        lsth2[1, 2] <- lsth2[2, 1] <- lsth2[2, 2] <- 0
        list(ls = ls, lsth1 = lsth, lsth2 = lsth2)
    }
    preinitialize <- expression({
        if (G$family$n.theta > 0) {
            Theta <- c(-1, log(0.2 * var(G$y)^0.5))
            G$family$putTheta(Theta)
        }
    })
    initialize <- expression({
        if (any(is.na(y))) stop("NA values not allowed for the scaled t family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0) * 0.1
    })
    postproc <- expression({
        object$family$family <- paste("Scaled t(", paste(round(object$family$getTheta(TRUE), 
            3), collapse = ","), ")", sep = "")
    })
    rd <- function(mu, wt, scale) {
        theta <- get(".Theta")
        nu <- exp(theta[1]) + 2
        sig <- exp(theta[2])
        n <- length(mu)
        stats::rt(n = n, df = nu) * sig + mu
    }
    environment(dev.resids) <- environment(aic) <- environment(getTheta) <- environment(rd) <- environment(variance) <- environment(putTheta) <- env
    structure(list(family = "scaled t", link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, dev.resids = dev.resids, Dd = Dd, 
        variance = variance, postproc = postproc, aic = aic, 
        mu.eta = stats$mu.eta, initialize = initialize, ls = ls, 
        preinitialize = preinitialize, validmu = validmu, valideta = stats$valideta, 
        n.theta = n.theta, ini.theta = iniTheta, putTheta = putTheta, 
        getTheta = getTheta, rd = rd), class = c("extended.family", 
        "family"))
}


mvn <- function (d = 2) 
{
    if (d < 2) 
        stop("mvn requires 2 or more dimensional data")
    stats <- list()
    for (i in 1:d) {
        stats[[i]] <- make.link("identity")
    }
    validmu <- function(mu) all(is.finite(mu))
    preinitialize <- expression({
        ydim <- ncol(G$y)
        nbeta <- ncol(G$X)
        ntheta <- ydim * (ydim + 1)/2
        lpi <- attr(G$X, "lpi")
        XX <- crossprod(G$X)
        G$X <- cbind(G$X, matrix(0, nrow(G$X), ntheta))
        G$term.names <- c(G$term.names, paste("R", 1:ntheta, 
            sep = "."))
        attr(G$X, "lpi") <- lpi
        attr(G$X, "XX") <- XX
        attr(G$Sl, "E") <- cbind(attr(G$Sl, "E"), matrix(0, nbeta, 
            ntheta))
        G$family$data <- list(ydim = ydim, nbeta = nbeta)
        G$family$ibeta = rep(0, ncol(G$X))
        for (k in 1:ydim) {
            sin <- G$off %in% lpi[[k]]
            um <- magic(G$y[, k], G$X[, lpi[[k]]], rep(-1, sum(sin)), 
                G$S[sin], match(G$off[sin], lpi[[k]]), nt = control$nthreads)
            G$family$ibeta[lpi[[k]]] <- um$b
            G$family$ibeta[nbeta + 1] <- -0.5 * log(um$scale)
            nbeta <- nbeta + ydim - k + 1
        }
    })
    postproc <- expression({
        ydim <- G$family$data$ydim
        R <- matrix(0, ydim, ydim)
        ind <- G$family$data$nbeta + 1:(ydim * (ydim + 1)/2)
        theta <- object$coefficients[ind]
        k <- 1
        for (i in 1:ydim) for (j in i:ydim) {
            if (i == j) R[i, j] <- exp(theta[k]) else R[i, j] <- theta[k]
            k <- k + 1
        }
        object$family$data <- list(R = R)
        rsd <- R %*% t(object$y - object$fitted.values)
        object$deviance <- sum(rsd^2)
        rsd <- R %*% (t(object$y) - colMeans(object$y))
        object$null.deviance <- sum(rsd^2)
    })
    initialize <- expression({
        n <- rep(1, nobs)
        if (is.null(start)) start <- family$ibeta
        if (exists("rp", inherits = FALSE) && length(rp$rp) > 
            0) attr(x, "XX") <- Sl.repara(rp$rp, t(Sl.repara(rp$rp, 
            attr(x, "XX"))))
    })
    residuals <- function(object, type = c("response", "deviance")) {
        type <- match.arg(type)
        res <- object$y - object$fitted.values
        if (type == "deviance") 
            res <- res %*% t(object$family$data$R)
        res
    }
    ll <- function(y, X, coef, wt, family, deriv = 0, d1b = NULL, 
        d2b = NULL, Hp = NULL, rank = 0, fh = NULL, D = NULL) {
        lpi <- attr(X, "lpi")
        overlap <- attr(lpi, "overlap")
        drop <- attr(X, "drop")
        if (!is.null(drop)) {
            attr(X, "XX") <- attr(X, "XX")[-drop, -drop]
        }
        m <- length(lpi)
        if (overlap) {
            ip <- unlist(lpi)
            XX <- attr(X, "XX")[ip, ip]
            X <- lpi.expand(X)
            attr(X, "XX") <- XX
            rm(XX)
            lpi0 <- lpi
            lpi <- attr(X, "lpi")
            ind <- (max(ip) + 1):length(coef)
            if (length(ind) != m * (m + 1)/2) 
                stop("mvn dimension error")
            coef <- c(coef[ip], coef[ind])
            if (!is.null(d1b)) 
                d1b <- rbind(d1b[ip, ], d1b[ind, ])
        }
        else ind <- NULL
        lpstart <- rep(0, m)
        for (i in 1:(m - 1)) lpstart[i] <- lpi[[i + 1]][1]
        lpstart[m] <- lpi[[m]][length(lpi[[m]])] + 1
        nb <- length(coef)
        if (deriv < 2) {
            nsp = 0
            d1b <- dH <- 0
        }
        else {
            nsp = ncol(d1b)
            dH = rep(0, nsp * nb * nb)
        }
        oo <- .C("mvn_ll", y = as.double(t(y)), X = as.double(X), 
            XX = as.double(attr(X, "XX")), beta = as.double(coef), 
            n = as.integer(nrow(X)), lpi = as.integer(lpstart - 
                1), m = as.integer(m), ll = as.double(0), lb = as.double(coef * 
                0), lbb = as.double(rep(0, nb * nb)), dbeta = as.double(d1b), 
            dH = as.double(dH), deriv = as.integer(nsp > 0), 
            nsp = as.integer(nsp), nt = as.integer(1), PACKAGE = "mgcv")
        lb <- oo$lb
        lbb <- matrix(oo$lbb, nb, nb)
        if (overlap) {
            lb <- lpi.contract(lb, lpi0)
            lbb <- lpi.contract(lbb, lpi0)
        }
        if (nsp == 0) 
            d1H <- NULL
        else if (deriv == 2) {
            d1H <- matrix(0, nb, nsp)
            for (i in 1:nsp) {
                dH <- matrix(oo$dH[ind], nb, nb)
                if (overlap) 
                  dH <- lpi.contract(dH, lpi0)
                d1H[, i] <- diag(dH)
                ind <- ind + nb * nb
            }
        }
        else {
            d1H <- list()
            ind <- 1:(nb * nb)
            for (i in 1:nsp) {
                dH <- matrix(oo$dH[ind], nb, nb)
                if (overlap) 
                  dH <- lpi.contract(dH, lpi0)
                d1H[[i]] <- dH
                ind <- ind + nb * nb
            }
        }
        list(l = oo$ll, lb = lb, lbb = lbb, d1H = d1H)
    }
    structure(list(family = "Multivariate normal", ll = ll, nlp = d, 
        initialize = initialize, preinitialize = preinitialize, 
        postproc = postproc, residuals = residuals, validmu = validmu, 
        linfo = stats, d2link = 1, d3link = 1, d4link = 1, ls = 1, 
        available.derivs = 1), class = c("general.family", "extended.family", 
        "family"))
}


Predict.matrix.gp.smooth <- function (object, data) 
{
    nk <- nrow(object$knt)
    for (i in 1:object$dim) {
        xx <- data[[object$term[i]]]
        if (i == 1) {
            n <- length(xx)
            x <- matrix(xx, n, object$dim)
        }
        else {
            if (n != length(xx)) 
                stop("arguments of smooth not same dimension")
            x[, i] <- xx
        }
    }
    x <- sweep(x, 2, object$shift)
    if (n > nk) {
        n.chunk <- n%/%nk
        for (i in 1:n.chunk) {
            ind <- 1:nk + (i - 1) * nk
            Xc <- gpE(x = x[ind, , drop = FALSE], xk = object$knt, 
                object$gp.defn)
            Xc <- cbind(Xc %*% object$UZ, gpT(x = x[ind, , drop = FALSE]))
            if (i == 1) 
                X <- Xc
            else {
                X <- rbind(X, Xc)
                rm(Xc)
            }
        }
        if (n > ind[nk]) {
            ind <- (ind[nk] + 1):n
            Xc <- gpE(x = x[ind, , drop = FALSE], xk = object$knt, 
                object$gp.defn)
            Xc <- cbind(Xc %*% object$UZ, gpT(x = x[ind, , drop = FALSE]))
            X <- rbind(X, Xc)
            rm(Xc)
        }
    }
    else {
        X <- gpE(x = x, xk = object$knt, object$gp.defn)
        X <- cbind(X %*% object$UZ, gpT(x = x))
    }
    X
}


get.var <- function (txt, data, vecMat = TRUE) 
{
    x <- data[[txt]]
    if (is.null(x)) {
        x <- try(eval(parse(text = txt), data, enclos = NULL), 
            silent = TRUE)
        if (inherits(x, "try-error")) 
            x <- NULL
    }
    if (!is.numeric(x) && !is.factor(x)) 
        x <- NULL
    if (is.matrix(x)) 
        ismat <- TRUE
    else ismat <- FALSE
    if (vecMat && is.matrix(x)) 
        x <- as.numeric(x)
    if (ismat) 
        attr(x, "matrix") <- TRUE
    x
}


logLik.gam <- function (object, ...) 
{
    sc.p <- as.numeric(object$scale.estimated)
    p <- sum(object$edf) + sc.p
    val <- p - object$aic/2
    if (!is.null(object$edf2)) 
        p <- sum(object$edf2) + sc.p
    np <- length(object$coefficients) + sc.p
    if (p > np) 
        p <- np
    if (inherits(object$family, "extended.family") && !is.null(object$family$n.theta)) 
        p <- p + object$family$n.theta
    attr(val, "df") <- p
    class(val) <- "logLik"
    val
}


gamm <- function (formula, random = NULL, correlation = NULL, family = gaussian(), 
    data = list(), weights = NULL, subset = NULL, na.action, 
    knots = NULL, control = list(niterEM = 0, optimMethod = "L-BFGS-B"), 
    niterPQL = 20, verbosePQL = TRUE, method = "ML", drop.unused.levels = TRUE, 
    ...) 
{
    if (inherits(family, "extended.family")) 
        warning("family are not designed for use with gamm!")
    control <- do.call("lmeControl", control)
    if (!is.null(random)) {
        if (is.list(random)) {
            r.names <- names(random)
            if (is.null(r.names)) 
                stop("random argument must be a *named* list.")
            else if (sum(r.names == "")) 
                stop("all elements of random list must be named")
        }
        else stop("gamm() can only handle random effects defined as named lists")
        random.vars <- c(unlist(lapply(random, function(x) all.vars(formula(x)))), 
            r.names)
    }
    else random.vars <- NULL
    if (!is.null(correlation)) {
        cor.for <- attr(correlation, "formula")
        if (!is.null(cor.for)) 
            cor.vars <- all.vars(cor.for)
    }
    else cor.vars <- NULL
    wisvf <- try(inherits(weights, "varFunc"), silent = TRUE)
    if (inherits(wisvf, "try-error")) 
        wisvf <- FALSE
    if (wisvf) {
        if (inherits(weights, "varComb")) {
            vf.vars <- rep("", 0)
            for (i in 1:length(weights)) {
                vf.vars <- c(vf.vars, all.vars(attr(weights[[i]], 
                  "formula")))
            }
            vf.vars <- unique(vf.vars)
        }
        else {
            vf.for <- attr(weights, "formula")
            if (!is.null(vf.for)) 
                vf.vars <- all.vars(vf.for)
        }
    }
    else vf.vars <- NULL
    gp <- interpret.gam(formula)
    mf <- match.call(expand.dots = FALSE)
    mf$formula <- gp$fake.formula
    if (wisvf) {
        mf$correlation <- mf$random <- mf$family <- mf$control <- mf$scale <- mf$knots <- mf$sp <- mf$weights <- mf$min.sp <- mf$H <- mf$gamma <- mf$fit <- mf$niterPQL <- mf$verbosePQL <- mf$G <- mf$method <- mf$... <- NULL
    }
    else {
        mf$correlation <- mf$random <- mf$family <- mf$control <- mf$scale <- mf$knots <- mf$sp <- mf$min.sp <- mf$H <- mf$gamma <- mf$fit <- mf$niterPQL <- mf$verbosePQL <- mf$G <- mf$method <- mf$... <- NULL
    }
    mf$drop.unused.levels <- drop.unused.levels
    mf[[1]] <- as.name("model.frame")
    pmf <- mf
    gmf <- eval(mf, parent.frame())
    gam.terms <- attr(gmf, "terms")
    if (!wisvf) 
        weights <- gmf[["(weights)"]]
    allvars <- c(cor.vars, random.vars, vf.vars)
    if (length(allvars)) {
        mf$formula <- as.formula(paste(paste(deparse(gp$fake.formula, 
            backtick = TRUE), collapse = ""), "+", paste(allvars, 
            collapse = "+")))
        mf <- eval(mf, parent.frame())
    }
    else mf <- gmf
    rm(gmf)
    if (nrow(mf) < 2) 
        stop("Not enough (non-NA) data to do anything meaningful")
    vars <- all.vars(gp$fake.formula[-2])
    inp <- parse(text = paste("list(", paste(vars, collapse = ","), 
        ")"))
    dl <- eval(inp, data, parent.frame())
    names(dl) <- vars
    var.summary <- variable.summary(gp$pf, dl, nrow(mf))
    rm(dl)
    pmf$formula <- gp$pf
    pmf <- eval(pmf, parent.frame())
    pTerms <- attr(pmf, "terms")
    if (is.character(family)) 
        family <- eval(parse(text = family))
    if (is.function(family)) 
        family <- family()
    if (is.null(family$family)) 
        stop("family not recognized")
    G <- gamm.setup(gp, pterms = pTerms, data = mf, knots = knots, 
        parametric.only = FALSE, absorb.cons = TRUE)
    G$var.summary <- var.summary
    mf <- G$data
    n.sr <- length(G$random)
    if (is.null(random) && n.sr == 0) 
        stop("gamm models must have at least 1 smooth with unknown smoothing parameter or at least one other random effect")
    offset.name <- attr(mf, "names")[attr(attr(mf, "terms"), 
        "offset")]
    yname <- new.name("y", names(mf))
    eval(parse(text = paste("mf$", yname, "<-G$y", sep = "")))
    Xname <- new.name("X", names(mf))
    eval(parse(text = paste("mf$", Xname, "<-G$X", sep = "")))
    fixed.formula <- paste(yname, "~", Xname, "-1")
    if (length(offset.name)) {
        fixed.formula <- paste(fixed.formula, "+", offset.name)
    }
    fixed.formula <- as.formula(fixed.formula)
    rand <- G$random
    if (!is.null(random)) {
        r.m <- length(random)
        r.names <- c(names(rand), names(random))
        for (i in 1:r.m) rand[[n.sr + i]] <- random[[i]]
        names(rand) <- r.names
    }
    if (length(formula(correlation))) {
        corGroup <- paste(names(rand), collapse = "/")
        groupForm <- nlme::getGroupsFormula(correlation)
        if (!is.null(groupForm)) {
            groupFormNames <- all.vars(groupForm)
            exind <- groupFormNames %in% names(rand)
            groupFormNames <- groupFormNames[!exind]
            if (length(groupFormNames)) 
                corGroup <- paste(corGroup, paste(groupFormNames, 
                  collapse = "/"), sep = "/")
        }
        corForm <- as.formula(paste(deparse(nlme::getCovariateFormula(correlation)), 
            "|", corGroup))
        attr(correlation, "formula") <- corForm
    }
    ret <- list()
    if (family$family == "gaussian" && family$link == "identity" && 
        length(offset.name) == 0) 
        lme.used <- TRUE
    else lme.used <- FALSE
    if (lme.used && !is.null(weights) && !wisvf) 
        lme.used <- FALSE
    if (lme.used) {
        eval(parse(text = paste("ret$lme<-lme(", deparse(fixed.formula), 
            ",random=rand,data=strip.offset(mf),correlation=correlation,", 
            "control=control,weights=weights,method=method)", 
            sep = "")))
    }
    else {
        if (wisvf) 
            stop("weights must be like glm weights for generalized case")
        if (verbosePQL) 
            cat("\n Maximum number of PQL iterations: ", niterPQL, 
                "\n")
        eval(parse(text = paste("ret$lme<-gammPQL(", deparse(fixed.formula), 
            ",random=rand,data=strip.offset(mf),family=family,", 
            "correlation=correlation,control=control,", "weights=weights,niter=niterPQL,verbose=verbosePQL)", 
            sep = "")))
        G$y <- ret$lme$y
    }
    object <- list(model = mf, formula = formula, smooth = G$smooth, 
        nsdf = G$nsdf, family = family, df.null = nrow(G$X), 
        y = G$y, terms = gam.terms, pterms = G$pterms, xlevels = G$xlevels, 
        contrasts = G$contrasts, assign = G$assign, na.action = attr(mf, 
            "na.action"), cmX = G$cmX, var.summary = G$var.summary, 
        scale.estimated = TRUE)
    pvars <- all.vars(delete.response(object$terms))
    object$pred.formula <- if (length(pvars) > 0) 
        reformulate(pvars)
    else NULL
    bf <- as.numeric(ret$lme$coefficients$fixed)
    br <- as.numeric(unlist(lapply(ret$lme$coefficients$random, 
        t)))
    fs.present <- FALSE
    if (G$nsdf) 
        p <- bf[1:G$nsdf]
    else p <- array(0, 0)
    if (G$m > 0) 
        for (i in 1:G$m) {
            fx <- G$smooth[[i]]$fixed
            first <- G$smooth[[i]]$first.f.para
            last <- G$smooth[[i]]$last.f.para
            if (first <= last) 
                beta <- bf[first:last]
            else beta <- array(0, 0)
            if (fx) 
                p <- c(p, beta)
            else {
                ind <- G$smooth[[i]]$rind
                if (!is.null(G$smooth[[i]]$fac)) {
                  fs.present <- TRUE
                  if (first <= last) 
                    stop("Nested smooths must be fully random")
                  flev <- levels(G$smooth[[i]]$fac)
                  for (j in 1:length(flev)) {
                    b <- br[ind]
                    b <- G$smooth[[i]]$trans.D * b
                    if (!is.null(G$smooth[[i]]$trans.U)) 
                      b <- G$smooth[[i]]$trans.U %*% b
                    ind <- ind + G$smooth[[i]]$rinc
                    p <- c(p, b)
                  }
                }
                else {
                  b <- c(br[ind], beta)
                  b <- G$smooth[[i]]$trans.D * b
                  if (!is.null(G$smooth[[i]]$trans.U)) 
                    b <- G$smooth[[i]]$trans.U %*% b
                  p <- c(p, b)
                }
            }
        }
    var.param <- coef(ret$lme$modelStruct$reStruct)
    n.v <- length(var.param)
    spl <- list()
    if (G$m > 0) 
        for (i in 1:G$m) {
            ii <- G$pord[i]
            n.sp <- length(object$smooth[[ii]]$S)
            if (n.sp > 0) {
                if (inherits(object$smooth[[ii]], "tensor.smooth")) 
                  spl[[ii]] <- notExp2(var.param[(n.v - n.sp + 
                    1):n.v])
                else spl[[ii]] <- 1/notExp2(var.param[n.v:(n.v - 
                  n.sp + 1)])
            }
            n.v <- n.v - n.sp
        }
    object$sp <- rep(0, 0)
    if (length(spl)) 
        for (i in 1:length(spl)) if (!is.null(spl[[i]])) 
            object$sp <- c(object$sp, spl[[i]])
    if (length(object$sp) == 0) 
        object$sp <- NULL
    object$coefficients <- p
    V <- extract.lme.cov2(ret$lme, mf, n.sr + 1)
    first.para <- last.para <- rep(0, G$m)
    if (fs.present) {
        Xf <- G$Xf[, 1:G$nsdf, drop = FALSE]
        if (G$m > 0) 
            for (i in 1:G$m) {
                ind <- object$smooth[[i]]$first.para:object$smooth[[i]]$last.para
                if (is.null(object$smooth[[i]]$fac)) {
                  first.para[i] <- ncol(Xf) + 1
                  Xf <- cbind(Xf, G$Xf[, ind])
                  last.para[i] <- ncol(Xf)
                }
                else {
                  flev <- levels(object$smooth[[i]]$fac)
                  first.para[i] <- ncol(Xf) + 1
                  for (k in 1:length(flev)) {
                    Xf <- cbind(Xf, G$Xf[, ind] * as.numeric(object$smooth[[i]]$fac == 
                      flev[k]))
                  }
                  last.para[i] <- ncol(Xf)
                }
            }
        object$R <- formXtViX(V, Xf)
        XVX <- crossprod(object$R)
        nxf <- ncol(Xf)
    }
    else {
        if (G$m > 0) 
            for (i in 1:G$m) {
                first.para[i] <- object$smooth[[i]]$first.para
                last.para[i] <- object$smooth[[i]]$last.para
            }
        object$R <- formXtViX(V, G$Xf)
        XVX <- crossprod(object$R)
        nxf <- ncol(G$Xf)
    }
    object$R <- object$R * ret$lme$sigma
    S <- matrix(0, nxf, nxf)
    first <- G$nsdf + 1
    k <- 1
    if (G$m > 0) 
        for (i in 1:G$m) {
            if (is.null(object$smooth[[i]]$fac)) {
                ind <- first.para[i]:last.para[i]
                ns <- length(object$smooth[[i]]$S)
                if (ns) 
                  for (l in 1:ns) {
                    S[ind, ind] <- S[ind, ind] + object$smooth[[i]]$S[[l]] * 
                      object$sp[k]
                    k <- k + 1
                  }
            }
            else {
                flev <- levels(object$smooth[[i]]$fac)
                ind <- first.para[i]:(first.para[i] + object$smooth[[i]]$n.para - 
                  1)
                ns <- length(object$smooth[[i]]$S)
                for (j in 1:length(flev)) {
                  if (ns) 
                    for (l in 1:ns) {
                      S[ind, ind] <- S[ind, ind] + object$smooth[[i]]$S[[l]] * 
                        object$sp[k]
                      k <- k + 1
                    }
                  k <- k - ns
                  ind <- ind + object$smooth[[i]]$n.para
                }
                k <- k + ns
            }
        }
    S <- S/ret$lme$sigma^2
    if (G$m) 
        for (i in 1:G$m) {
            object$smooth[[i]]$first.para <- first.para[i]
            object$smooth[[i]]$last.para <- last.para[i]
        }
    ev <- eigen(XVX + S, symmetric = TRUE)
    ind <- ev$values != 0
    iv <- ev$values
    iv[ind] <- 1/ev$values[ind]
    Vb <- ev$vectors %*% (iv * t(ev$vectors))
    object$edf <- rowSums(Vb * t(XVX))
    object$df.residual <- length(object$y) - sum(object$edf)
    object$sig2 <- ret$lme$sigma^2
    if (lme.used) {
        object$method <- paste("lme.", method, sep = "")
    }
    else {
        object$method <- "PQL"
    }
    if (!lme.used || method == "ML") 
        Vb <- Vb * length(G$y)/(length(G$y) - G$nsdf)
    object$Vp <- Vb
    object$Ve <- Vb %*% XVX %*% Vb
    object$prior.weights <- weights
    class(object) <- "gam"
    if (!is.null(G$P)) {
        object$coefficients <- G$P %*% object$coefficients
        object$Vp <- G$P %*% object$Vp %*% t(G$P)
        object$Ve <- G$P %*% object$Ve %*% t(G$P)
    }
    object$linear.predictors <- predict.gam(object, type = "link")
    object$fitted.values <- object$family$linkinv(object$linear.predictors)
    object$residuals <- residuals(ret$lme)
    if (G$nsdf > 0) 
        term.names <- colnames(G$X)[1:G$nsdf]
    else term.names <- array("", 0)
    n.smooth <- length(G$smooth)
    if (n.smooth) {
        for (i in 1:n.smooth) {
            k <- 1
            for (j in object$smooth[[i]]$first.para:object$smooth[[i]]$last.para) {
                term.names[j] <- paste(object$smooth[[i]]$label, 
                  ".", as.character(k), sep = "")
                k <- k + 1
            }
        }
        if (!is.null(object$sp)) 
            names(object$sp) <- names(G$sp)
    }
    names(object$coefficients) <- term.names
    names(object$edf) <- term.names
    if (is.null(weights)) 
        object$prior.weights <- object$y * 0 + 1
    else if (wisvf) 
        object$prior.weights <- varWeights.dfo(ret$lme, mf)^2
    else object$prior.weights <- ret$lme$w
    object$weights <- object$prior.weights
    if (!is.null(G$Xcentre)) 
        object$Xcentre <- G$Xcentre
    environment(attr(object$model, "terms")) <- environment(object$terms) <- environment(object$pterms) <- environment(object$formula) <- environment(object$pred.formula) <- .GlobalEnv
    ret$gam <- object
    environment(attr(ret$lme$data, "terms")) <- environment(ret$lme$terms) <- .GlobalEnv
    if (!is.null(ret$lme$modelStruct$varStruct)) {
        environment(attr(ret$lme$modelStruct$varStruct, "formula")) <- .GlobalEnv
    }
    if (!is.null(ret$lme$modelStruct$corStruct)) {
        environment(attr(ret$lme$modelStruct$corStruct, "formula")) <- .GlobalEnv
    }
    class(ret) <- c("gamm", "list")
    ret
}


gam.check <- function (b, old.style = FALSE, type = c("deviance", "pearson", 
    "response"), k.sample = 5000, k.rep = 200, rep = 0, level = 0.9, 
    rl.col = 2, rep.col = "gray80", ...) 
{
    type <- match.arg(type)
    resid <- residuals(b, type = type)
    linpred <- if (is.matrix(b$linear.predictors) && !is.matrix(resid)) 
        napredict(b$na.action, b$linear.predictors[, 1])
    else napredict(b$na.action, b$linear.predictors)
    old.par <- par(mfrow = c(2, 2))
    if (old.style) 
        qqnorm(resid, ...)
    else qq.gam(b, rep = rep, level = level, type = type, rl.col = rl.col, 
        rep.col = rep.col, ...)
    plot(linpred, resid, main = "Resids vs. linear pred.", xlab = "linear predictor", 
        ylab = "residuals", ...)
    hist(resid, xlab = "Residuals", main = "Histogram of residuals", 
        ...)
    fv <- if (inherits(b$family, "extended.family")) 
        predict(b, type = "response")
    else fitted(b)
    if (is.matrix(fv) && !is.matrix(b$y)) 
        fv <- fv[, 1]
    plot(fv, napredict(b$na.action, b$y), xlab = "Fitted Values", 
        ylab = "Response", main = "Response vs. Fitted Values", 
        ...)
    if (!(b$method %in% c("GCV", "GACV", "UBRE", "REML", "ML", 
        "P-ML", "P-REML", "fREML"))) {
        par(old.par)
        return(invisible())
    }
    cat("\nMethod:", b$method, "  Optimizer:", b$optimizer)
    if (!is.null(b$outer.info)) {
        if (b$optimizer[2] %in% c("newton", "bfgs")) {
            boi <- b$outer.info
            cat("\n", boi$conv, " after ", boi$iter, " iteration", 
                sep = "")
            if (boi$iter == 1) 
                cat(".")
            else cat("s.")
            cat("\nGradient range [", min(boi$grad), ",", max(boi$grad), 
                "]", sep = "")
            cat("\n(score ", b$gcv.ubre, " & scale ", b$sig2, 
                ").", sep = "")
            ev <- eigen(boi$hess)$values
            if (min(ev) > 0) 
                cat("\nHessian positive definite, ")
            else cat("\n")
            cat("eigenvalue range [", min(ev), ",", max(ev), 
                "].\n", sep = "")
        }
        else {
            cat("\n")
            print(b$outer.info)
        }
    }
    else {
        if (length(b$sp) == 0) 
            cat("\nModel required no smoothing parameter selection")
        else {
            cat("\nSmoothing parameter selection converged after", 
                b$mgcv.conv$iter, "iteration")
            if (b$mgcv.conv$iter > 1) 
                cat("s")
            if (!b$mgcv.conv$fully.converged) 
                cat(" by steepest\ndescent step failure.\n")
            else cat(".\n")
            cat("The RMS", b$method, "score gradiant at convergence was", 
                b$mgcv.conv$rms.grad, ".\n")
            if (b$mgcv.conv$hess.pos.def) 
                cat("The Hessian was positive definite.\n")
            else cat("The Hessian was not positive definite.\n")
            cat("The estimated model rank was ", b$mgcv.conv$rank, 
                " (maximum possible: ", b$mgcv.conv$full.rank, 
                ")\n", sep = "")
        }
    }
    if (!is.null(b$rank)) {
        cat("Model rank = ", b$rank, "/", length(b$coefficients), 
            "\n")
    }
    cat("\n")
    kchck <- k.check(b, subsample = k.sample, n.rep = k.rep)
    if (!is.null(kchck)) {
        cat("Basis dimension (k) checking results. Low p-value (k-index<1) may\n")
        cat("indicate that k is too low, especially if edf is close to k'.\n\n")
        printCoefmat(kchck, digits = 3)
    }
    par(old.par)
}


smoothCon <- function (object, data, knots = NULL, absorb.cons = FALSE, scale.penalty = TRUE, 
    n = nrow(data), dataX = NULL, null.space.penalty = FALSE, 
    sparse.cons = 0, diagonal.penalty = FALSE, apply.by = TRUE) 
{
    sm <- smooth.construct3(object, data, knots)
    if (!is.null(attr(sm, "qrc"))) 
        warning("smooth objects should not have a qrc attribute.")
    if (is.null(sm$plot.me)) 
        sm$plot.me <- TRUE
    if (is.null(sm$side.constrain)) 
        sm$side.constrain <- TRUE
    if (!is.null(sm$g.index)) {
        sm$C <- matrix(colMeans(sm$X), 1, ncol(sm$X))
        if (length(sm$S)) {
            upen <- rowMeans(sm$S[[1]]) == 0
            if (length(sm$S) > 1) 
                for (i in 2:length(sm$S)) upen <- upen & rowMeans(sm$S[[i]]) == 
                  0
            if (sum(upen) == 0) 
                stop("something wrong in monotone setup - no unpenalized terms!")
            drop <- min(which(upen))
        }
        else drop <- 1
        sm$g.index <- sm$g.index[-drop]
    }
    else drop <- -1
    if (is.null(sm$C)) {
        if (sparse.cons <= 0) {
            sm$C <- matrix(colMeans(sm$X), 1, ncol(sm$X))
            if (sparse.cons == -1) {
                vcol <- apply(sm$X, 2, var)
                drop <- min((1:length(vcol))[vcol == min(vcol)])
            }
        }
        else if (sparse.cons > 0) {
            if (sum(sm$X == 0) > 0.1 * sum(sm$X != 0)) {
                if (sparse.cons == 1) {
                  xsd <- apply(sm$X, 2, FUN = sd)
                  if (sum(xsd == 0)) 
                    sm$C <- ((1:length(xsd))[xsd == 0])[1]
                  else {
                    xz <- apply(sm$X, 2, FUN = function(x) {
                      sum(x == 0)
                    })
                    sm$C <- ((1:length(xz))[xz == min(xz)])[1]
                  }
                }
                else if (sparse.cons == 2) {
                  sm$C = -1
                }
                else {
                  stop("unimplemented sparse constraint type requested")
                }
            }
            else {
                sm$C <- matrix(colSums(sm$X), 1, ncol(sm$X))
            }
        }
        else {
            sm$C <- matrix(colSums(sm$X), 1, ncol(sm$X))
        }
        alwaysCon <- FALSE
    }
    else {
        if (is.null(attr(sm$C, "always.apply"))) 
            alwaysCon <- FALSE
        else alwaysCon <- TRUE
    }
    if (is.null(sm$df)) 
        sm$df <- sm$bs.dim
    if (!is.null(object$fixed) && object$fixed) {
        sm$S <- NULL
    }
    sm$S.scale <- rep(1, length(sm$S))
    if (scale.penalty && length(sm$S) > 0 && is.null(sm$no.rescale)) {
        maXX <- norm(sm$X, type = "I")^2
        for (i in 1:length(sm$S)) {
            maS <- norm(sm$S[[i]])/maXX
            sm$S[[i]] <- sm$S[[i]]/maS
            sm$S.scale[i] <- maS
        }
    }
    if (!is.null(dataX)) {
        er <- Predict.matrix3(sm, dataX)
        sm$X <- er$X
        sm$ind <- er$ind
        rm(er)
    }
    if ((is.null(sm$ind) && nrow(sm$X) != n) || (!is.null(sm$ind) && 
        length(sm$ind) != n)) {
        matrixArg <- TRUE
        if (is.null(sm$ind)) 
            q <- nrow(sm$X)/n
        else q <- length(sm$ind)/n
        if (!is.null(sm$by.done)) 
            warning("handling `by' variables in smooth constructors may not work with the summation convention ")
    }
    else {
        matrixArg <- FALSE
        if (!is.null(sm$ind)) {
            offs <- attr(sm$X, "offset")
            sm$X <- sm$X[sm$ind, ]
            if (!is.null(offs)) 
                attr(sm$X, "offset") <- offs[sm$ind]
        }
    }
    offs <- NULL
    if (matrixArg || (object$by != "NA" && is.null(sm$by.done))) {
        if (is.null(dataX)) 
            by <- get.var(object$by, data)
        else by <- get.var(object$by, dataX)
        if (matrixArg && is.null(by)) {
            if (is.null(sm$ind)) 
                by <- rep(1, nrow(sm$X))
            else by <- rep(1, length(sm$ind))
        }
        if (is.null(by)) 
            stop("Can't find by variable")
        offs <- attr(sm$X, "offset")
        if (!is.factor(by)) {
            if (!alwaysCon) {
                if (matrixArg) {
                  L1 <- as.numeric(matrix(by, n, q) %*% rep(1, 
                    q))
                  if (sd(L1) > mean(L1) * .Machine$double.eps * 
                    1000) {
                    sm$C <- matrix(0, 0, 1)
                    if (!is.null(sm$Cp)) 
                      sm$Cp <- NULL
                  }
                  else sm$meanL1 <- mean(L1)
                }
                else {
                  if (sd(by) > mean(by) * .Machine$double.eps * 
                    1000) {
                    sm$C <- matrix(0, 0, 1)
                    if (!is.null(sm$Cp)) 
                      sm$Cp <- NULL
                  }
                }
            }
        }
    }
    if (absorb.cons && drop > 0) {
        if (!is.null(sm$by.done)) 
            warning("sweep and drop constraints unlikely to work well with self handling of by vars")
        qrc <- c(drop, as.numeric(sm$C)[-drop])
        class(qrc) <- "sweepDrop"
        sm$X <- sm$X[, -drop] - matrix(qrc[-1], nrow(sm$X), ncol(sm$X) - 
            1, byrow = TRUE)
        if (length(sm$S) > 0) 
            for (l in 1:length(sm$S)) {
                sm$S[[l]] <- sm$S[[l]][-drop, -drop]
            }
        attr(sm, "qrc") <- qrc
        attr(sm, "nCons") <- 1
        sm$Cp <- sm$C <- 0
        sm$rank <- pmin(sm$rank, ncol(sm$X))
        sm$df <- sm$df - 1
        sm$null.space.dim <- max(0, sm$null.space.dim - 1)
    }
    if (matrixArg || (object$by != "NA" && is.null(sm$by.done))) {
        if (is.factor(by)) {
            if (matrixArg) 
                stop("factor `by' variables can not be used with matrix arguments.")
            sml <- list()
            lev <- levels(by)
            if (is.ordered(by) && length(lev) > 1) 
                lev <- lev[-1]
            for (j in 1:length(lev)) {
                sml[[j]] <- sm
                by.dum <- as.numeric(lev[j] == by)
                sml[[j]]$X <- by.dum * sm$X
                sml[[j]]$by.level <- lev[j]
                sml[[j]]$label <- paste(sm$label, ":", object$by, 
                  lev[j], sep = "")
                if (!is.null(offs)) {
                  attr(sml[[j]]$X, "offset") <- offs * by.dum
                }
            }
        }
        else {
            sml <- list(sm)
            if ((is.null(sm$ind) && length(by) != nrow(sm$X)) || 
                (!is.null(sm$ind) && length(by) != length(sm$ind))) 
                stop("`by' variable must be same dimension as smooth arguments")
            if (matrixArg) {
                if (is.null(sm$ind)) {
                  sml[[1]]$X <- as.numeric(by) * sm$X
                  ind <- 1:n
                  X <- sml[[1]]$X[ind, ]
                  for (i in 2:q) {
                    ind <- ind + n
                    X <- X + sml[[1]]$X[ind, ]
                  }
                  sml[[1]]$X <- X
                  if (!is.null(offs)) {
                    offs <- attr(sm$X, "offset") * as.numeric(by)
                    ind <- 1:n
                    offX <- offs[ind, ]
                    for (i in 2:q) {
                      ind <- ind + n
                      offX <- offX + offs[ind, ]
                    }
                    attr(sml[[1]]$X, "offset") <- offX
                  }
                }
                else {
                  ind <- 0:(q - 1) * n
                  offs <- attr(sm$X, "offset")
                  if (!is.null(offs)) 
                    offX <- rep(0, n)
                  else offX <- NULL
                  sml[[1]]$X <- matrix(0, n, ncol(sm$X))
                  for (i in 1:n) {
                    ind <- ind + 1
                    sml[[1]]$X[i, ] <- colSums(by[ind] * sm$X[sm$ind[ind], 
                      ])
                    if (!is.null(offs)) {
                      offX[i] <- sum(offs[sm$ind[ind]] * by[ind])
                    }
                  }
                  attr(sml[[1]]$X, "offset") <- offX
                }
            }
            else {
                sml[[1]]$X <- as.numeric(by) * sm$X
                if (!is.null(offs)) 
                  attr(sml[[1]]$X, "offset") <- if (apply.by) 
                    offs * as.numeric(by)
                  else offs
            }
            if (object$by == "NA") 
                sml[[1]]$label <- sm$label
            else sml[[1]]$label <- paste(sm$label, ":", object$by, 
                sep = "")
        }
    }
    else {
        sml <- list(sm)
    }
    if (absorb.cons) {
        k <- ncol(sm$X)
        if (!is.null(sm$Cp) && is.matrix(sm$Cp)) {
            pj <- nrow(sm$Cp)
            qrcp <- qr(t(sm$Cp))
            for (i in 1:length(sml)) {
                sml[[i]]$Xp <- t(qr.qty(qrcp, t(sml[[i]]$X))[(pj + 
                  1):k, ])
                sml[[i]]$Cp <- NULL
                if (length(sml[[i]]$S)) {
                  sml[[i]]$Sp <- sml[[i]]$S
                  for (l in 1:length(sml[[i]]$S)) {
                    ZSZ <- qr.qty(qrcp, sml[[i]]$S[[l]])[(pj + 
                      1):k, ]
                    sml[[i]]$Sp[[l]] <- t(qr.qty(qrcp, t(ZSZ))[(pj + 
                      1):k, ])
                  }
                }
            }
        }
        else qrcp <- NULL
        if (is.matrix(sm$C)) {
            j <- nrow(sm$C)
            if (j > 0) {
                indi <- (1:ncol(sm$C))[colSums(sm$C) != 0]
                nx <- length(indi)
                if (nx < ncol(sm$C) && drop < 0) {
                  nc <- j
                  nz <- nx - nc
                  qrc <- qr(t(sm$C[, indi, drop = FALSE]))
                  for (i in 1:length(sml)) {
                    if (length(sm$S) > 0) 
                      for (l in 1:length(sm$S)) {
                        ZSZ <- sml[[i]]$S[[l]]
                        ZSZ[indi[1:nz], ] <- qr.qty(qrc, sml[[i]]$S[[l]][indi, 
                          , drop = FALSE])[(nc + 1):nx, ]
                        ZSZ <- ZSZ[-indi[(nz + 1):nx], ]
                        ZSZ[, indi[1:nz]] <- t(qr.qty(qrc, t(ZSZ[, 
                          indi, drop = FALSE]))[(nc + 1):nx, 
                          ])
                        sml[[i]]$S[[l]] <- ZSZ[, -indi[(nz + 
                          1):nx], drop = FALSE]
                      }
                    sml[[i]]$X[, indi[1:nz]] <- t(qr.qty(qrc, 
                      t(sml[[i]]$X[, indi, drop = FALSE]))[(nc + 
                      1):nx, ])
                    sml[[i]]$X <- sml[[i]]$X[, -indi[(nz + 1):nx]]
                    attr(sml[[i]], "qrc") <- qrc
                    attr(sml[[i]], "nCons") <- j
                    attr(sml[[i]], "indi") <- indi
                    sml[[i]]$C <- NULL
                    sml[[i]]$rank <- pmin(sm$rank, k - j)
                    sml[[i]]$df <- sml[[i]]$df - j
                    sml[[i]]$null.space.dim <- max(0, sml[[i]]$null.space.dim - 
                      j)
                  }
                }
                else {
                  {
                    qrc <- qr(t(sm$C))
                    for (i in 1:length(sml)) {
                      if (length(sm$S) > 0) 
                        for (l in 1:length(sm$S)) {
                          ZSZ <- qr.qty(qrc, sm$S[[l]])[(j + 
                            1):k, ]
                          sml[[i]]$S[[l]] <- t(qr.qty(qrc, t(ZSZ))[(j + 
                            1):k, ])
                        }
                      sml[[i]]$X <- t(qr.qty(qrc, t(sml[[i]]$X))[(j + 
                        1):k, ])
                    }
                  }
                  for (i in 1:length(sml)) {
                    attr(sml[[i]], "qrc") <- qrc
                    attr(sml[[i]], "nCons") <- j
                    sml[[i]]$C <- NULL
                    sml[[i]]$rank <- pmin(sm$rank, k - j)
                    sml[[i]]$df <- sml[[i]]$df - j
                    sml[[i]]$null.space.dim <- max(0, sml[[i]]$null.space.dim - 
                      j)
                  }
                }
            }
            else {
                for (i in 1:length(sml)) {
                  attr(sml[[i]], "qrc") <- "no constraints"
                  attr(sml[[i]], "nCons") <- 0
                }
            }
        }
        else if (sm$C > 0) {
            for (i in 1:length(sml)) {
                if (length(sm$S) > 0) 
                  for (l in 1:length(sm$S)) {
                    sml[[i]]$S[[l]] <- sml[[i]]$S[[l]][-sm$C, 
                      -sm$C]
                  }
                sml[[i]]$X <- sml[[i]]$X[, -sm$C]
                attr(sml[[i]], "qrc") <- sm$C
                attr(sml[[i]], "nCons") <- 1
                sml[[i]]$C <- NULL
                sml[[i]]$rank <- pmin(sm$rank, k - 1)
                sml[[i]]$df <- sml[[i]]$df - 1
                sml[[i]]$null.space.dim <- max(sml[[i]]$null.space.dim - 
                  1, 0)
            }
        }
        else if (sm$C < 0) {
            for (i in 1:length(sml)) {
                if (length(sm$S) > 0) 
                  for (l in 1:length(sm$S)) {
                    sml[[i]]$S[[l]] <- diff(t(diff(sml[[i]]$S[[l]])))
                  }
                sml[[i]]$X <- t(diff(t(sml[[i]]$X)))
                attr(sml[[i]], "qrc") <- sm$C
                attr(sml[[i]], "nCons") <- 1
                sml[[i]]$C <- NULL
                sml[[i]]$rank <- pmin(sm$rank, k - 1)
                sml[[i]]$df <- sml[[i]]$df - 1
                sml[[i]]$null.space.dim <- max(sml[[i]]$null.space.dim - 
                  1, 0)
            }
        }
        if (!is.null(qrcp)) {
            for (i in 1:length(sml)) {
                attr(sml[[i]], "qrc") <- qrcp
                if (pj != attr(sml[[i]], "nCons")) 
                  stop("Number of prediction and fit constraints must match")
                attr(sml[[i]], "indi") <- NULL
            }
        }
    }
    else for (i in 1:length(sml)) attr(sml[[i]], "qrc") <- NULL
    if (diagonal.penalty && length(sml[[1]]$S) == 1) {
        S11 <- sml[[1]]$S[[1]][1, 1]
        rank <- sml[[1]]$rank
        p <- ncol(sml[[1]]$X)
        if (is.null(rank) || max(abs(sml[[1]]$S[[1]] - diag(c(rep(S11, 
            rank), rep(0, p - rank))))) > abs(S11) * .Machine$double.eps^0.8) {
            np <- nat.param(sml[[1]]$X, sml[[1]]$S[[1]], rank = sml[[1]]$rank, 
                type = 2, unit.fnorm = FALSE)
            sml[[1]]$X <- np$X
            sml[[1]]$S[[1]] <- diag(p)
            diag(sml[[1]]$S[[1]]) <- c(np$D, rep(0, p - np$rank))
            sml[[1]]$diagRP <- np$P
            if (length(sml) > 1) 
                for (i in 2:length(sml)) {
                  sml[[i]]$X <- sml[[i]]$X %*% np$P
                  sml[[i]]$S <- sml[[1]]$S
                  sml[[i]]$diagRP <- np$P
                }
        }
    }
    if (null.space.penalty) {
        nsm <- length(sml[[1]]$S)
        if (nsm == 1) {
            S11 <- sml[[1]]$S[[1]][1, 1]
            rank <- sml[[1]]$rank
            p <- ncol(sml[[1]]$X)
            if (is.null(rank) || max(abs(sml[[1]]$S[[1]] - diag(c(rep(S11, 
                rank), rep(0, p - rank))))) > abs(S11) * .Machine$double.eps^0.8) 
                need.full <- TRUE
            else {
                need.full <- FALSE
                if (p > rank) 
                  for (i in 1:length(sml)) {
                    sml[[i]]$S[[2]] <- diag(c(rep(0, rank), rep(1, 
                      p - rank)))
                    sml[[i]]$rank[2] <- p - rank
                    sml[[i]]$S.scale[2] <- 1
                    sml[[i]]$null.space.dim <- 0
                  }
            }
        }
        else need.full <- if (nsm > 0) 
            TRUE
        else FALSE
        if (need.full) {
            St <- sml[[1]]$S[[1]]
            if (length(sml[[1]]$S) > 1) 
                for (i in 1:length(sml[[1]]$S)) St <- St + sml[[1]]$S[[i]]
            es <- eigen(St, symmetric = TRUE)
            ind <- es$values < max(es$values) * .Machine$double.eps^0.66
            if (sum(ind)) {
                U <- es$vectors[, ind, drop = FALSE]
                Sf <- U %*% t(U)
                M <- length(sm$S)
                for (i in 1:length(sml)) {
                  sml[[i]]$S[[M + 1]] <- Sf
                  sml[[i]]$rank[M + 1] <- sum(ind)
                  sml[[i]]$S.scale[M + 1] <- 1
                  sml[[i]]$null.space.dim <- 0
                }
            }
        }
    }
    if (!apply.by) 
        for (i in 1:length(sml)) {
            by.name <- sml[[i]]$by
            if (by.name != "NA") {
                sml[[i]]$by <- "NA"
                sml[[i]]$X0 <- PredictMat(sml[[i]], data)
                sml[[i]]$by <- by.name
            }
        }
    sml
}


gam.side <- function (sm, Xp, tol = .Machine$double.eps^0.5, with.pen = FALSE) 
{
    if (!with.pen) {
        with.pen <- nrow(Xp) < ncol(Xp) + sum(unlist(lapply(sm, 
            function(x) ncol(x$X))))
    }
    m <- length(sm)
    if (m == 0) 
        return(sm)
    v.names <- array("", 0)
    maxDim <- 1
    for (i in 1:m) {
        vn <- sm[[i]]$term
        if (sm[[i]]$by != "NA") 
            vn <- paste(vn, sm[[i]]$by, sep = "")
        if (!is.null(sm[[i]]$by.level)) 
            vn <- paste(vn, sm[[i]]$by.level, sep = "")
        sm[[i]]$vn <- vn
        v.names <- c(v.names, vn)
        if (sm[[i]]$dim > maxDim) 
            maxDim <- sm[[i]]$dim
    }
    lv <- length(v.names)
    v.names <- unique(v.names)
    if (lv == length(v.names)) 
        return(sm)
    intercept <- FALSE
    if (ncol(Xp)) {
        if (sum(apply(Xp, 2, sd) < .Machine$double.eps^0.75) > 
            0) 
            intercept <- TRUE
        else {
            f <- rep(1, nrow(Xp))
            ff <- qr.fitted(qr(Xp), f)
            if (max(abs(ff - f)) < .Machine$double.eps^0.75) 
                intercept <- TRUE
        }
    }
    sm.id <- as.list(v.names)
    names(sm.id) <- v.names
    for (i in 1:length(sm.id)) sm.id[[i]] <- array(0, 0)
    sm.dim <- sm.id
    for (d in 1:maxDim) {
        for (i in 1:m) {
            if (sm[[i]]$dim == d && sm[[i]]$side.constrain) 
                for (j in 1:d) {
                  term <- sm[[i]]$vn[j]
                  a <- sm.id[[term]]
                  la <- length(a) + 1
                  sm.id[[term]][la] <- i
                  sm.dim[[term]][la] <- d
                }
        }
    }
    if (maxDim == 1) 
        warning("model has repeated 1-d smooths of same variable.")
    if (with.pen) {
        k <- 1
        for (i in 1:m) {
            k1 <- k + ncol(sm[[i]]$X) - 1
            sm[[i]]$p.ind <- k:k1
            k <- k1 + 1
        }
        np <- k - 1
    }
    nobs <- nrow(sm[[1]]$X)
    for (d in 1:maxDim) {
        for (i in 1:m) {
            if (sm[[i]]$dim == d && sm[[i]]$side.constrain) {
                if (with.pen) 
                  X1 <- matrix(c(rep(1, nobs), rep(0, np)), nobs + 
                    np, as.integer(intercept))
                else X1 <- matrix(1, nobs, as.integer(intercept))
                for (j in 1:d) {
                  b <- sm.id[[sm[[i]]$vn[j]]]
                  k <- (1:length(b))[b == i]
                  if (k > 1) 
                    for (l in 1:(k - 1)) {
                      if (with.pen) {
                        if (is.null(sm[[b[l]]]$Xa)) 
                          sm[[b[l]]]$Xa <- augment.smX(sm[[b[l]]], 
                            nobs, np)
                        X1 <- cbind(X1, sm[[b[l]]]$Xa)
                      }
                      else X1 <- cbind(X1, sm[[b[l]]]$X)
                    }
                }
                if (ncol(X1) == as.integer(intercept)) 
                  ind <- NULL
                else {
                  if (with.pen) {
                    if (is.null(sm[[i]]$Xa)) 
                      sm[[i]]$Xa <- augment.smX(sm[[i]], nobs, 
                        np)
                    ind <- fixDependence(X1, sm[[i]]$Xa, tol = tol)
                  }
                  else ind <- fixDependence(X1, sm[[i]]$X, tol = tol)
                }
                if (!is.null(ind)) {
                  sm[[i]]$X <- sm[[i]]$X[, -ind]
                  nsmS <- length(sm[[i]]$S)
                  if (nsmS > 0) 
                    for (j in nsmS:1) {
                      sm[[i]]$S[[j]] <- sm[[i]]$S[[j]][-ind, 
                        -ind]
                      if (sum(sm[[i]]$S[[j]] != 0) == 0) 
                        rank <- 0
                      else rank <- qr(sm[[i]]$S[[j]], tol = tol, 
                        LAPACK = FALSE)$rank
                      sm[[i]]$rank[j] <- rank
                      if (rank == 0) {
                        sm[[i]]$rank <- sm[[i]]$rank[-j]
                        sm[[i]]$S[[j]] <- NULL
                        sm[[i]]$S.scale <- sm[[i]]$S.scale[-j]
                        if (!is.null(sm[[i]]$L)) 
                          sm[[i]]$L <- sm[[i]]$L[-j, , drop = FALSE]
                      }
                    }
                  mi <- length(sm[[i]]$S)
                  if (mi > 0) {
                    St <- sm[[i]]$S[[1]]/norm(sm[[i]]$S[[1]], 
                      type = "F")
                    if (mi > 1) 
                      for (j in 1:mi) St <- St + sm[[i]]$S[[j]]/norm(sm[[i]]$S[[j]], 
                        type = "F")
                    es <- eigen(St, symmetric = TRUE, only.values = TRUE)
                    sm[[i]]$null.space.dim <- sum(es$values < 
                      max(es$values) * .Machine$double.eps^0.75)
                  }
                  if (!is.null(sm[[i]]$L)) {
                    ind <- as.numeric(colSums(sm[[i]]$L != 0)) != 
                      0
                    sm[[i]]$L <- sm[[i]]$L[, ind, drop = FALSE]
                  }
                  sm[[i]]$df <- ncol(sm[[i]]$X)
                  attr(sm[[i]], "del.index") <- ind
                  if (!is.null(sm[[i]]$Xp)) {
                    if (with.pen) {
                      smi <- sm[[i]]
                      smi$X <- smi$Xp
                      smi$S <- smi$Sp
                      Xpa <- augment.smX(smi, nobs, np)
                      ind <- fixDependence(X1, Xpa, rank.def = length(ind))
                    }
                    else ind <- fixDependence(X1, sm[[i]]$Xp, 
                      rank.def = length(ind))
                    sm[[i]]$Xp <- sm[[i]]$Xp[, -ind, drop = FALSE]
                    attr(sm[[i]], "del.index") <- ind
                  }
                }
                sm[[i]]$vn <- NULL
            }
        }
    }
    if (with.pen) 
        for (i in 1:m) {
            sm[[i]]$p.ind <- NULL
            if (!is.null(sm[[i]]$Xa)) 
                sm[[i]]$Xa <- NULL
        }
    sm
}


print.summary.gam <- function (x, digits = max(3, getOption("digits") - 3), signif.stars = getOption("show.signif.stars"), 
    ...) 
{
    print(x$family)
    cat("Formula:\n")
    if (is.list(x$formula)) 
        for (i in 1:length(x$formula)) print(x$formula[[i]])
    else print(x$formula)
    if (length(x$p.coeff) > 0) {
        cat("\nParametric coefficients:\n")
        printCoefmat(x$p.table, digits = digits, signif.stars = signif.stars, 
            na.print = "NA", ...)
    }
    cat("\n")
    if (x$m > 0) {
        cat("Approximate significance of smooth terms:\n")
        printCoefmat(x$s.table, digits = digits, signif.stars = signif.stars, 
            has.Pvalue = TRUE, na.print = "NA", cs.ind = 1, ...)
    }
    cat("\n")
    if (!is.null(x$rank) && x$rank < x$np) 
        cat("Rank: ", x$rank, "/", x$np, "\n", sep = "")
    if (!is.null(x$r.sq)) 
        cat("R-sq.(adj) = ", formatC(x$r.sq, digits = 3, width = 5), 
            "  ")
    if (length(x$dev.expl) > 0) 
        cat("Deviance explained = ", formatC(x$dev.expl * 100, 
            digits = 3, width = 4), "%", sep = "")
    cat("\n")
    if (!is.null(x$method) && !(x$method %in% c("PQL", "lme.ML", 
        "lme.REML"))) 
        cat(x$method, " = ", formatC(x$sp.criterion, digits = 5), 
            sep = "")
    cat("  Scale est. = ", formatC(x$scale, digits = 5, width = 8, 
        flag = "-"), "  n = ", x$n, "\n", sep = "")
    invisible(x)
}


bandchol <- function (B) 
{
    n <- ncol(B)
    k <- 0
    if (n == nrow(B)) {
        A <- B * 0
        for (i in 1:n) {
            b <- sdiag(B, i - 1)
            if (sum(b != 0) != 0) {
                k <- i
                A[i, 1:length(b)] <- b
            }
        }
        B <- A[1:k, ]
    }
    oo <- .C(C_band_chol, B = as.double(B), n = as.integer(n), 
        k = as.integer(nrow(B)), info = as.integer(0))
    if (oo$info < 0) 
        stop("something wrong with inputs to LAPACK routine")
    if (oo$info > 0) 
        stop("not positive definite")
    B <- matrix(oo$B, nrow(B), n)
    if (k > 0) {
        A <- A * 0
        for (i in 1:k) sdiag(A, i - 1) <- B[i, 1:(n - i + 1)]
        B <- A
    }
    B
}


tw <- function (theta = NULL, link = "log", a = 1.01, b = 1.99) 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    if (linktemp %in% c("log", "identity", "sqrt", "inverse")) 
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
        else stop(gettextf("link \"%s\" not available for Tweedie family.", 
            linktemp, collapse = ""), domain = NA)
    }
    n.theta <- 1
    if (!is.null(theta) && theta != 0) {
        if (abs(theta) <= a || abs(theta) >= b) 
            stop("Tweedie p must be in interval (a,b)")
        if (theta > 0) {
            iniTheta <- log((theta - a)/(b - theta))
            n.theta <- 0
        }
        else iniTheta <- log((-theta - a)/(b + theta))
    }
    else iniTheta <- 0
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", iniTheta, envir = env)
    assign(".a", a, envir = env)
    assign(".b", b, envir = env)
    getTheta <- function(trans = FALSE) {
        th <- get(".Theta")
        a <- get(".a")
        b <- get(".b")
        if (trans) 
            th <- if (th > 0) 
                (b + a * exp(-th))/(1 + exp(-th))
            else (b * exp(th) + a)/(exp(th) + 1)
        th
    }
    putTheta <- function(theta) assign(".Theta", theta, envir = environment(sys.function()))
    validmu <- function(mu) all(mu > 0)
    variance <- function(mu) {
        th <- get(".Theta")
        a <- get(".a")
        b <- get(".b")
        p <- if (th > 0) 
            (b + a * exp(-th))/(1 + exp(-th))
        else (b * exp(th) + a)/(exp(th) + 1)
        mu^p
    }
    dev.resids <- function(y, mu, wt, theta = NULL) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        a <- get(".a")
        b <- get(".b")
        p <- if (theta > 0) 
            (b + a * exp(-theta))/(1 + exp(-theta))
        else (b * exp(theta) + a)/(exp(theta) + 1)
        y1 <- y + (y == 0)
        theta <- if (p == 1) 
            log(y1/mu)
        else (y1^(1 - p) - mu^(1 - p))/(1 - p)
        kappa <- if (p == 2) 
            log(y1/mu)
        else (y^(2 - p) - mu^(2 - p))/(2 - p)
        2 * (y * theta - kappa) * wt
    }
    Dd <- function(y, mu, theta, wt, level = 0) {
        a <- get(".a")
        b <- get(".b")
        th <- theta
        p <- if (th > 0) 
            (b + a * exp(-th))/(1 + exp(-th))
        else (b * exp(th) + a)/(exp(th) + 1)
        dpth1 <- if (th > 0) 
            exp(-th) * (b - a)/(1 + exp(-th))^2
        else exp(th) * (b - a)/(exp(th) + 1)^2
        dpth2 <- if (th > 0) 
            ((a - b) * exp(-th) + (b - a) * exp(-2 * th))/(exp(-th) + 
                1)^3
        else ((a - b) * exp(2 * th) + (b - a) * exp(th))/(exp(th) + 
            1)^3
        mu1p <- mu^(1 - p)
        mup <- mu^p
        r <- list()
        ymupi <- y/mup
        r$Dmu <- 2 * wt * (mu1p - ymupi)
        r$Dmu2 <- 2 * wt * (mu^(-1 - p) * p * y + (1 - p)/mup)
        r$EDmu2 <- (2 * wt)/mup
        if (level > 0) {
            i1p <- 1/(1 - p)
            y1 <- y + (y == 0)
            logmu <- log(mu)
            mu2p <- mu * mu1p
            r$Dth <- 2 * wt * ((y^(2 - p) * log(y1) - mu2p * 
                logmu)/(2 - p) + (y * mu1p * logmu - y^(2 - p) * 
                log(y1))/(1 - p) - (y^(2 - p) - mu2p)/(2 - p)^2 + 
                (y^(2 - p) - y * mu1p) * i1p^2) * dpth1
            r$Dmuth <- 2 * wt * logmu * (ymupi - mu1p) * dpth1
            mup1 <- mu^(-p - 1)
            r$Dmu3 <- -2 * wt * mup1 * p * (y/mu * (p + 1) + 
                1 - p)
            r$Dmu2th <- 2 * wt * (mup1 * y * (1 - p * logmu) - 
                (logmu * (1 - p) + 1)/mup) * dpth1
        }
        if (level > 1) {
            mup2 <- mup1/mu
            r$Dmu4 <- 2 * wt * mup2 * p * (p + 1) * (y * (p + 
                2)/mu + 1 - p)
            y2plogy <- y^(2 - p) * log(y1)
            y2plog2y <- y2plogy * log(y1)
            r$Dth2 <- 2 * wt * (((mu2p * logmu^2 - y2plog2y)/(2 - 
                p) + (y2plog2y - y * mu1p * logmu^2)/(1 - p) + 
                2 * (y2plogy - mu2p * logmu)/(2 - p)^2 + 2 * 
                (y * mu1p * logmu - y2plogy)/(1 - p)^2 + 2 * 
                (mu2p - y^(2 - p))/(2 - p)^3 + 2 * (y^(2 - p) - 
                y * mu^(1 - p))/(1 - p)^3) * dpth1^2) + r$Dth * 
                dpth2/dpth1
            r$Dmuth2 <- 2 * wt * ((mu1p * logmu^2 - logmu^2 * 
                ymupi) * dpth1^2) + r$Dmuth * dpth2/dpth1
            r$Dmu2th2 <- 2 * wt * ((mup1 * logmu * y * (logmu * 
                p - 2) + logmu/mup * (logmu * (1 - p) + 2)) * 
                dpth1^2) + r$Dmu2th * dpth2/dpth1
            r$Dmu3th <- 2 * wt * mup1 * (y/mu * (logmu * (1 + 
                p) * p - p - p - 1) + logmu * (1 - p) * p + p - 
                1 + p) * dpth1
        }
        r
    }
    aic <- function(y, mu, theta = NULL, wt, dev) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        a <- get(".a")
        b <- get(".b")
        p <- if (theta > 0) 
            (b + a * exp(-theta))/(1 + exp(-theta))
        else (b * exp(theta) + a)/(exp(theta) + 1)
        scale <- dev/sum(wt)
        -2 * sum(ldTweedie(y, mu, p = p, phi = scale)[, 1] * 
            wt) + 2
    }
    ls <- function(y, w, n, theta, scale) {
        a <- get(".a")
        b <- get(".b")
        LS <- colSums(w * ldTweedie(y, y, rho = log(scale), theta = theta, 
            a = a, b = b))
        lsth1 <- c(LS[4], LS[2])
        lsth2 <- matrix(c(LS[5], LS[6], LS[6], LS[3]), 2, 2)
        list(ls = LS[1], lsth1 = lsth1, lsth2 = lsth2)
    }
    initialize <- expression({
        n <- rep(1, nobs)
        mustart <- y + (y == 0) * 0.1
    })
    postproc <- expression({
        object$family$family <- paste("Tweedie(p=", round(object$family$getTheta(TRUE), 
            3), ")", sep = "")
    })
    rd <- function(mu, wt, scale) {
        th <- get(".Theta")
        a <- get(".a")
        b <- get(".b")
        p <- if (th > 0) 
            (b + a * exp(-th))/(1 + exp(-th))
        else (b * exp(th) + a)/(exp(th) + 1)
        if (p == 2) 
            rgamma(mu, shape = 1/scale, scale = mu * scale)
        else rTweedie(mu, p = p, phi = scale)
    }
    environment(Dd) <- environment(ls) <- environment(dev.resids) <- environment(aic) <- environment(getTheta) <- environment(rd) <- environment(variance) <- environment(putTheta) <- env
    structure(list(family = "Tweedie", link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, dev.resids = dev.resids, Dd = Dd, 
        variance = variance, rd = rd, aic = aic, mu.eta = stats$mu.eta, 
        initialize = initialize, postproc = postproc, ls = ls, 
        validmu = validmu, valideta = stats$valideta, canonical = "none", 
        n.theta = n.theta, ini.theta = iniTheta, putTheta = putTheta, 
        getTheta = getTheta, scale = -1), class = c("extended.family", 
        "family"))
}


vcov.gam <- function (object, freq = FALSE, dispersion = NULL, unconditional = FALSE, 
    ...) 
{
    if (freq) 
        vc <- object$Ve
    else {
        vc <- if (unconditional && !is.null(object$Vc)) 
            object$Vc
        else object$Vp
    }
    if (!is.null(dispersion)) 
        vc <- dispersion * vc/object$sig2
    name <- names(object$edf)
    dimnames(vc) <- list(name, name)
    vc
}


plot.gam <- function (x, residuals = FALSE, rug = TRUE, se = TRUE, pages = 0, 
    select = NULL, scale = -1, n = 100, n2 = 40, pers = FALSE, 
    theta = 30, phi = 30, jit = FALSE, xlab = NULL, ylab = NULL, 
    main = NULL, ylim = NULL, xlim = NULL, too.far = 0.1, all.terms = FALSE, 
    shade = FALSE, shade.col = "gray80", shift = 0, trans = I, 
    seWithMean = FALSE, unconditional = FALSE, by.resids = FALSE, 
    scheme = 0, ...) 
{
    sub.edf <- function(lab, edf) {
        pos <- regexpr(":", lab)[1]
        if (pos < 0) {
            pos <- nchar(lab) - 1
            lab <- paste(substr(lab, start = 1, stop = pos), 
                ",", round(edf, digits = 2), ")", sep = "")
        }
        else {
            lab1 <- substr(lab, start = 1, stop = pos - 2)
            lab2 <- substr(lab, start = pos - 1, stop = nchar(lab))
            lab <- paste(lab1, ",", round(edf, digits = 2), lab2, 
                sep = "")
        }
        lab
    }
    if (unconditional) {
        if (is.null(x$Vc)) 
            warning("Smoothness uncertainty corrected covariance not available")
        else x$Vp <- x$Vc
    }
    w.resid <- NULL
    if (length(residuals) > 1) {
        if (length(residuals) == length(x$residuals)) 
            w.resid <- residuals
        else warning("residuals argument to plot.gam is wrong length: ignored")
        partial.resids <- TRUE
    }
    else partial.resids <- residuals
    m <- length(x$smooth)
    if (length(scheme) == 1) 
        scheme <- rep(scheme, m)
    if (length(scheme) != m) {
        warn <- paste("scheme should be a single number, or a vector with", 
            m, "elements")
        warning(warn)
        scheme <- rep(scheme[1], m)
    }
    order <- if (is.list(x$pterms)) 
        unlist(lapply(x$pterms, attr, "order"))
    else attr(x$pterms, "order")
    if (all.terms) 
        n.para <- sum(order == 1)
    else n.para <- 0
    if (se) {
        if (is.numeric(se)) 
            se2.mult <- se1.mult <- se
        else {
            se1.mult <- 2
            se2.mult <- 1
        }
        if (se1.mult < 0) 
            se1.mult <- 0
        if (se2.mult < 0) 
            se2.mult <- 0
    }
    else se1.mult <- se2.mult <- 1
    if (se && x$Vp[1, 1] < 0) {
        se <- FALSE
        warning("No variance estimates available")
    }
    if (partial.resids) {
        if (is.null(w.resid)) {
            if (is.null(x$residuals) || is.null(x$weights)) 
                partial.resids <- FALSE
            else {
                wr <- sqrt(x$weights)
                w.resid <- x$residuals * wr/mean(wr)
            }
        }
        if (partial.resids) 
            fv.terms <- predict(x, type = "terms")
    }
    pd <- list()
    i <- 1
    if (m > 0) 
        for (i in 1:m) {
            first <- x$smooth[[i]]$first.para
            last <- x$smooth[[i]]$last.para
            edf <- sum(x$edf[first:last])
            term.lab <- sub.edf(x$smooth[[i]]$label, edf)
            attr(x$smooth[[i]], "coefficients") <- x$coefficients[first:last]
            P <- plot(x$smooth[[i]], P = NULL, data = x$model, 
                partial.resids = partial.resids, rug = rug, se = se, 
                scale = scale, n = n, n2 = n2, pers = pers, theta = theta, 
                phi = phi, jit = jit, xlab = xlab, ylab = ylab, 
                main = main, label = term.lab, ylim = ylim, xlim = xlim, 
                too.far = too.far, shade = shade, shade.col = shade.col, 
                se1.mult = se1.mult, se2.mult = se2.mult, shift = shift, 
                trans = trans, by.resids = by.resids, scheme = scheme[i], 
                ...)
            if (is.null(P)) 
                pd[[i]] <- list(plot.me = FALSE)
            else if (is.null(P$fit)) {
                p <- x$coefficients[first:last]
                offset <- attr(P$X, "offset")
                if (is.null(offset)) 
                  P$fit <- P$X %*% p
                else P$fit <- P$X %*% p + offset
                if (!is.null(P$exclude)) 
                  P$fit[P$exclude] <- NA
                if (se && P$se) {
                  if (seWithMean && attr(x$smooth[[i]], "nCons") > 
                    0) {
                    if (length(x$cmX) < ncol(x$Vp)) 
                      x$cmX <- c(x$cmX, rep(0, ncol(x$Vp) - length(x$cmX)))
                    X1 <- matrix(x$cmX, nrow(P$X), ncol(x$Vp), 
                      byrow = TRUE)
                    meanL1 <- x$smooth[[i]]$meanL1
                    if (!is.null(meanL1)) 
                      X1 <- X1/meanL1
                    X1[, first:last] <- P$X
                    se.fit <- sqrt(pmax(0, rowSums((X1 %*% x$Vp) * 
                      X1)))
                  }
                  else se.fit <- sqrt(pmax(0, rowSums((P$X %*% 
                    x$Vp[first:last, first:last, drop = FALSE]) * 
                    P$X)))
                  if (!is.null(P$exclude)) 
                    P$se.fit[P$exclude] <- NA
                }
                if (partial.resids) {
                  P$p.resid <- fv.terms[, length(order) + i] + 
                    w.resid
                }
                if (se && P$se) 
                  P$se <- se.fit * P$se.mult
                P$X <- NULL
                P$plot.me <- TRUE
                pd[[i]] <- P
                rm(P)
            }
            else {
                if (partial.resids) {
                  P$p.resid <- fv.terms[, length(order) + i] + 
                    w.resid
                }
                P$plot.me <- TRUE
                pd[[i]] <- P
                rm(P)
            }
        }
    n.plots <- n.para
    if (m > 0) 
        for (i in 1:m) n.plots <- n.plots + as.numeric(pd[[i]]$plot.me)
    if (n.plots == 0) 
        stop("No terms to plot - nothing for plot.gam() to do.")
    if (pages > n.plots) 
        pages <- n.plots
    if (pages < 0) 
        pages <- 0
    if (pages != 0) {
        ppp <- n.plots%/%pages
        if (n.plots%%pages != 0) {
            ppp <- ppp + 1
            while (ppp * (pages - 1) >= n.plots) pages <- pages - 
                1
        }
        c <- r <- trunc(sqrt(ppp))
        if (c < 1) 
            r <- c <- 1
        if (c * r < ppp) 
            c <- c + 1
        if (c * r < ppp) 
            r <- r + 1
        oldpar <- par(mfrow = c(r, c))
    }
    else {
        ppp <- 1
        oldpar <- par()
    }
    if ((pages == 0 && prod(par("mfcol")) < n.plots && dev.interactive()) || 
        pages > 1 && dev.interactive()) 
        ask <- TRUE
    else ask <- FALSE
    if (!is.null(select)) {
        ask <- FALSE
    }
    if (ask) {
        oask <- devAskNewPage(TRUE)
        on.exit(devAskNewPage(oask))
    }
    if (scale == -1 && is.null(ylim)) {
        k <- 0
        if (m > 0) 
            for (i in 1:m) if (pd[[i]]$plot.me && pd[[i]]$scale) {
                if (se && length(pd[[i]]$se) > 1) {
                  ul <- pd[[i]]$fit + pd[[i]]$se
                  ll <- pd[[i]]$fit - pd[[i]]$se
                  if (k == 0) {
                    ylim <- c(min(ll, na.rm = TRUE), max(ul, 
                      na.rm = TRUE))
                    k <- 1
                  }
                  else {
                    if (min(ll, na.rm = TRUE) < ylim[1]) 
                      ylim[1] <- min(ll, na.rm = TRUE)
                    if (max(ul, na.rm = TRUE) > ylim[2]) 
                      ylim[2] <- max(ul, na.rm = TRUE)
                  }
                }
                else {
                  if (k == 0) {
                    ylim <- range(pd[[i]]$fit, na.rm = TRUE)
                    k <- 1
                  }
                  else {
                    if (min(pd[[i]]$fit, na.rm = TRUE) < ylim[1]) 
                      ylim[1] <- min(pd[[i]]$fit, na.rm = TRUE)
                    if (max(pd[[i]]$fit, na.rm = TRUE) > ylim[2]) 
                      ylim[2] <- max(pd[[i]]$fit, na.rm = TRUE)
                  }
                }
                if (partial.resids) {
                  ul <- max(pd[[i]]$p.resid, na.rm = TRUE)
                  if (ul > ylim[2]) 
                    ylim[2] <- ul
                  ll <- min(pd[[i]]$p.resid, na.rm = TRUE)
                  if (ll < ylim[1]) 
                    ylim[1] <- ll
                }
            }
    }
    if (m > 0) 
        for (i in 1:m) if (pd[[i]]$plot.me && (is.null(select) || 
            i == select)) {
            plot(x$smooth[[i]], P = pd[[i]], partial.resids = partial.resids, 
                rug = rug, se = se, scale = scale, n = n, n2 = n2, 
                pers = pers, theta = theta, phi = phi, jit = jit, 
                xlab = xlab, ylab = ylab, main = main, ylim = ylim, 
                xlim = xlim, too.far = too.far, shade = shade, 
                shade.col = shade.col, shift = shift, trans = trans, 
                by.resids = by.resids, scheme = scheme[i], ...)
        }
    if (n.para > 0) {
        class(x) <- c("gam", "glm", "lm")
        if (is.null(select)) {
            attr(x, "para.only") <- TRUE
            termplot(x, se = se, rug = rug, col.se = 1, col.term = 1, 
                main = attr(x$pterms, "term.labels"), ...)
        }
        else {
            if (select > m) {
                select <- select - m
                term.labels <- attr(x$pterms, "term.labels")
                term.labels <- term.labels[order == 1]
                if (select <= length(term.labels)) {
                  termplot(x, terms = term.labels[select], se = se, 
                    rug = rug, col.se = 1, col.term = 1, ...)
                }
            }
        }
    }
    if (pages > 0) 
        par(oldpar)
    invisible(pd)
}


fix.family.var <- function (fam) 
{
    if (inherits(fam, "extended.family")) 
        return(fam)
    if (!inherits(fam, "family")) 
        stop("fam not a family object")
    if (!is.null(fam$dvar) && !is.null(fam$d2var) && !is.null(fam$d3var)) 
        return(fam)
    family <- fam$family
    if (family == "gaussian") {
        fam$d3var <- fam$d2var <- fam$dvar <- function(mu) rep.int(0, 
            length(mu))
        return(fam)
    }
    if (family == "poisson" || family == "quasipoisson") {
        fam$dvar <- function(mu) rep.int(1, length(mu))
        fam$d3var <- fam$d2var <- function(mu) rep.int(0, length(mu))
        return(fam)
    }
    if (family == "binomial" || family == "quasibinomial") {
        fam$dvar <- function(mu) 1 - 2 * mu
        fam$d2var <- function(mu) rep.int(-2, length(mu))
        fam$d3var <- function(mu) rep.int(0, length(mu))
        return(fam)
    }
    if (family == "Gamma") {
        fam$dvar <- function(mu) 2 * mu
        fam$d2var <- function(mu) rep.int(2, length(mu))
        fam$d3var <- function(mu) rep.int(0, length(mu))
        return(fam)
    }
    if (family == "quasi") {
        fam$dvar <- switch(fam$varfun, constant = function(mu) rep.int(0, 
            length(mu)), `mu(1-mu)` = function(mu) 1 - 2 * mu, 
            mu = function(mu) rep.int(1, length(mu)), `mu^2` = function(mu) 2 * 
                mu, `mu^3` = function(mu) 3 * mu^2)
        if (is.null(fam$dvar)) 
            stop("variance function not recognized for quasi")
        fam$d2var <- switch(fam$varfun, constant = function(mu) rep.int(0, 
            length(mu)), `mu(1-mu)` = function(mu) rep.int(-2, 
            length(mu)), mu = function(mu) rep.int(0, length(mu)), 
            `mu^2` = function(mu) rep.int(2, length(mu)), `mu^3` = function(mu) 6 * 
                mu)
        fam$d3var <- switch(fam$varfun, constant = function(mu) rep.int(0, 
            length(mu)), `mu(1-mu)` = function(mu) rep.int(0, 
            length(mu)), mu = function(mu) rep.int(0, length(mu)), 
            `mu^2` = function(mu) rep.int(0, length(mu)), `mu^3` = function(mu) rep.int(6, 
                length(mu)))
        return(fam)
    }
    if (family == "inverse.gaussian") {
        fam$dvar <- function(mu) 3 * mu^2
        fam$d2var <- function(mu) 6 * mu
        fam$d3var <- function(mu) rep.int(6, length(mu))
        return(fam)
    }
    stop("family not recognised")
}


ls.size <- function (x) 
{
    if (is.list(x) == FALSE) 
        return(object.size(x))
    xn <- names(x)
    n <- length(x)
    sz <- rep(-1, n)
    for (i in 1:n) sz[i] <- object.size(x[[i]])
    names(sz) <- xn
    sz
}


Predict.matrix.sf <- function (object, data) 
{
    x <- get.var(object$term[1], data)
    y <- get.var(object$term[2], data)
    b <- soap.basis(object$sd, x, y, film = TRUE, wiggly = FALSE, 
        penalty = FALSE)
    X <- t(object$irng * t(b$X))
    attr(X, "offset") <- b$offset
    X
}


inSide <- function (bnd, x, y) 
{
    xname <- deparse(substitute(x))
    yname <- deparse(substitute(y))
    bnd.name <- names(bnd)
    if (is.null(bnd.name)) 
        for (i in 1:length(bnd)) {
            bnd.name <- names(bnd[[i]])
            if (xname %in% bnd.name == FALSE || yname %in% bnd.name == 
                FALSE) 
                stop("variable names don't match boundary names")
            bnd.name[xname == bnd.name] <- "x"
            bnd.name[yname == bnd.name] <- "y"
            names(bnd[[i]]) <- bnd.name
        }
    else {
        if (xname %in% bnd.name == FALSE || yname %in% bnd.name == 
            FALSE) 
            stop("variable names don't match boundary names")
        bnd.name[xname == bnd.name] <- "x"
        bnd.name[yname == bnd.name] <- "y"
        names(bnd) <- bnd.name
    }
    bnd <- bnd2C(bnd)
    um <- .C(C_in_out, bx = as.double(bnd$x), by = as.double(bnd$y), 
        break.code = as.double(bnd$breakCode), x = as.double(x), 
        y = as.double(y), inside = as.integer(y * 0), nb = as.integer(bnd$n), 
        n = as.integer(length(x)))
    as.logical(um$inside)
}


ocat <- function (theta = NULL, link = "identity", R = NULL) 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    if (linktemp %in% c("identity")) 
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
        else stop(linktemp, " link not available for ordered categorical family; available links are \"identity\"")
    }
    if (is.null(theta) && is.null(R)) 
        stop("Must supply theta or R to ocat")
    if (!is.null(theta)) 
        R <- length(theta) + 2
    n.theta <- R - 2
    if (!is.null(theta) && sum(theta == 0) == 0) {
        if (sum(theta < 0)) 
            iniTheta <- log(abs(theta))
        else {
            iniTheta <- log(theta)
            n.theta <- 0
        }
    }
    else iniTheta <- rep(-1, length = R - 2)
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", iniTheta, envir = env)
    putTheta <- function(theta) assign(".Theta", theta, envir = environment(sys.function()))
    getTheta <- function(trans = FALSE) {
        theta <- get(".Theta")
        if (trans) {
            R = length(theta) + 2
            alpha <- rep(0, R - 1)
            alpha[1] <- -1
            if (R > 2) {
                ind <- 2:(R - 1)
                alpha[ind] <- alpha[1] + cumsum(exp(theta))
            }
            theta <- alpha
        }
        theta
    }
    postproc <- expression({
        object$family$family <- paste("Ordered Categorical(", 
            paste(round(object$family$getTheta(TRUE), 2), collapse = ","), 
            ")", sep = "")
    })
    validmu <- function(mu) all(is.finite(mu))
    dev.resids <- function(y, mu, wt, theta = NULL) {
        Fdiff <- function(a, b) {
            h <- rep(1, length(b))
            h[b > 0] <- -1
            eb <- exp(b * h)
            h <- h * 0 + 1
            h[a > 0] <- -1
            ea <- exp(a * h)
            ind <- b < 0
            bi <- eb[ind]
            ai <- ea[ind]
            h[ind] <- bi/(1 + bi) - ai/(1 + ai)
            ind1 <- a > 0
            bi <- eb[ind1]
            ai <- ea[ind1]
            h[ind1] <- (ai - bi)/((ai + 1) * (bi + 1))
            ind <- !ind & !ind1
            bi <- eb[ind]
            ai <- ea[ind]
            h[ind] <- (1 - ai * bi)/((bi + 1) * (ai + 1))
            h
        }
        if (is.null(theta)) 
            theta <- get(".Theta")
        R = length(theta) + 2
        alpha <- rep(0, R + 1)
        alpha[1] <- -Inf
        alpha[R + 1] <- Inf
        alpha[2] <- -1
        if (R > 2) {
            ind <- 3:R
            alpha[ind] <- alpha[2] + cumsum(exp(theta))
        }
        al1 <- alpha[y + 1]
        al0 = alpha[y]
        s <- sign((al1 + al0)/2 - mu)
        al1mu <- al1 - mu
        al0mu <- al0 - mu
        f <- Fdiff(al0mu, al1mu)
        rsd <- -2 * log(f)
        attr(rsd, "sign") <- s
        rsd
    }
    Dd <- function(y, mu, theta, wt = NULL, level = 0) {
        Fdiff <- function(a, b) {
            h <- rep(1, length(b))
            h[b > 0] <- -1
            eb <- exp(b * h)
            h <- h * 0 + 1
            h[a > 0] <- -1
            ea <- exp(a * h)
            ind <- b < 0
            bi <- eb[ind]
            ai <- ea[ind]
            h[ind] <- bi/(1 + bi) - ai/(1 + ai)
            ind1 <- a > 0
            bi <- eb[ind1]
            ai <- ea[ind1]
            h[ind1] <- (ai - bi)/((ai + 1) * (bi + 1))
            ind <- !ind & !ind1
            bi <- eb[ind]
            ai <- ea[ind]
            h[ind] <- (1 - ai * bi)/((bi + 1) * (ai + 1))
            h
        }
        abcd <- function(x, level = -1) {
            bj <- cj <- dj <- NULL
            h <- rep(1, length(x))
            h[x > 0] <- -1
            ex <- exp(x * h)
            ex1 <- ex + 1
            ex1k <- ex1^2
            aj <- -ex/ex1k
            if (level >= 0) {
                ex1k <- ex1k * ex1
                ex2 <- ex^2
                bj <- h * (ex - ex^2)/ex1k
                if (level > 0) {
                  ex1k <- ex1k * ex1
                  ex3 <- ex2 * ex
                  cj <- (-ex3 + 4 * ex2 - ex)/ex1k
                  if (level > 1) {
                    ex1k <- ex1k * ex1
                    ex4 <- ex3 * ex
                    dj <- h * (-ex4 + 11 * ex3 - 11 * ex2 + ex)/ex1k
                  }
                }
            }
            list(aj = aj, bj = bj, cj = cj, dj = dj)
        }
        R = length(theta) + 2
        alpha <- rep(0, R + 1)
        alpha[1] <- -Inf
        alpha[R + 1] <- Inf
        alpha[2] <- -1
        if (R > 2) {
            ind <- 3:R
            alpha[ind] <- alpha[2] + cumsum(exp(theta))
        }
        al1 <- alpha[y + 1]
        al0 = alpha[y]
        al1mu <- al1 - mu
        al0mu <- al0 - mu
        f <- pmax(Fdiff(al0mu, al1mu), .Machine$double.xmin)
        r1 <- abcd(al1mu, level)
        a1 <- r1$aj
        r0 <- abcd(al0mu, level)
        a0 <- r0$aj
        a <- a1 - a0
        if (level >= 0) {
            b1 <- r1$bj
            b0 <- r0$bj
            b <- b1 - b0
        }
        if (level > 0) {
            c1 <- r1$cj
            c0 <- r0$cj
            c <- c1 - c0
        }
        if (level > 1) {
            d1 <- r1$dj
            d0 <- r0$dj
            d <- d1 - d0
        }
        oo <- list(D = NULL, Dmu = NULL, Dmu2 = NULL, Dth = NULL, 
            Dmuth = NULL, Dmu2th = NULL)
        n <- length(y)
        oo$D <- -2 * log(f)
        if (level >= 0) {
            oo$Dmu <- -2 * a/f
            a2 <- a^2
            oo$EDmu2 <- oo$Dmu2 <- 2 * (a2/f - b)/f
        }
        if (R < 3) 
            level <- 0
        if (level > 0) {
            f2 <- f^2
            a3 <- a2 * a
            oo$Dmu3 <- 2 * (-c - 2 * a3/f2 + 3 * a * b/f)/f
            Dmua0 <- 2 * (a0 * a/f - b0)/f
            Dmua1 <- -2 * (a1 * a/f - b1)/f
            Dmu2a0 <- -2 * (c0 + (a0 * (2 * a2/f - b) - 2 * b0 * 
                a)/f)/f
            Dmu2a1 <- 2 * (c1 + (2 * (a1 * a2/f - b1 * a) - a1 * 
                b)/f)/f
            Da0 <- -2 * a0/f
            Da1 <- 2 * a1/f
            oo$Dmu2th <- oo$Dmuth <- oo$Dth <- matrix(0, n, R - 
                2)
            for (k in 1:(R - 2)) {
                etk <- exp(theta[k])
                ind <- y == k + 1
                oo$Dth[ind, k] <- Da1[ind] * etk
                oo$Dmuth[ind, k] <- Dmua1[ind] * etk
                oo$Dmu2th[ind, k] <- Dmu2a1[ind] * etk
                if (R > k + 2) {
                  ind <- y > k + 1 & y < R
                  oo$Dth[ind, k] <- (Da1[ind] + Da0[ind]) * etk
                  oo$Dmuth[ind, k] <- (Dmua1[ind] + Dmua0[ind]) * 
                    etk
                  oo$Dmu2th[ind, k] <- (Dmu2a1[ind] + Dmu2a0[ind]) * 
                    etk
                }
                ind <- y == R
                oo$Dth[ind, k] <- Da0[ind] * etk
                oo$Dmuth[ind, k] <- Dmua0[ind] * etk
                oo$Dmu2th[ind, k] <- Dmu2a0[ind] * etk
            }
        }
        if (level > 1) {
            oo$Dmu4 <- 2 * ((3 * b^2 + 4 * a * c)/f + a2 * (6 * 
                a2/f - 12 * b)/f2 - d)/f
            Dmu3a0 <- 2 * ((a0 * c + 3 * c0 * a + 3 * b0 * b)/f - 
                d0 + 6 * a * (a0 * a2/f - b0 * a - a0 * b)/f2)/f
            Dmu3a1 <- 2 * (d1 - (a1 * c + 3 * (c1 * a + b1 * 
                b))/f + 6 * a * (b1 * a - a1 * a2/f + a1 * b)/f2)/f
            Dmua0a0 <- 2 * (c0 + (2 * a0 * (b0 - a0 * a/f) - 
                b0 * a)/f)/f
            Dmua1a1 <- 2 * ((b1 * a + 2 * a1 * (b1 - a1 * a/f))/f - 
                c1)/f
            Dmua0a1 <- 2 * (a0 * (2 * a1 * a/f - b1) - b0 * a1)/f2
            Dmu2a0a0 <- 2 * (d0 + (b0 * (2 * b0 - b) + 2 * c0 * 
                (a0 - a))/f + 2 * (b0 * a2 + a0 * (3 * a0 * a2/f - 
                4 * b0 * a - a0 * b))/f2)/f
            Dmu2a1a1 <- 2 * ((2 * c1 * (a + a1) + b1 * (2 * b1 + 
                b))/f + 2 * (a1 * (3 * a1 * a2/f - a1 * b) - 
                b1 * a * (a + 4 * a1))/f2 - d1)/f
            Dmu2a0a1 <- 0
            Da0a0 <- 2 * (b0 + a0^2/f)/f
            Da1a1 <- -2 * (b1 - a1^2/f)/f
            Da0a1 <- -2 * a0 * a1/f2
            n2d <- (R - 2) * (R - 1)/2
            oo$Dmu3th <- matrix(0, n, R - 2)
            oo$Dmu2th2 <- oo$Dmuth2 <- oo$Dth2 <- matrix(0, n, 
                n2d)
            i <- 0
            for (j in 1:(R - 2)) for (k in j:(R - 2)) {
                i <- i + 1
                ind <- y >= j
                ar1.k <- ar.k <- rep(exp(theta[k]), n)
                ar.k[y == R | y <= k] <- 0
                ar1.k[y < k + 2] <- 0
                ar.j <- ar1.j <- rep(exp(theta[j]), n)
                ar.j[y == R | y <= j] <- 0
                ar1.j[y < j + 2] <- 0
                ar.kj <- ar1.kj <- rep(0, n)
                if (k == j) {
                  ar.kj[y > k & y < R] <- exp(theta[k])
                  ar1.kj[y > k + 1] <- exp(theta[k])
                  oo$Dmu3th[ind, k] <- Dmu3a1[ind] * ar.k[ind] + 
                    Dmu3a0[ind] * ar1.k[ind]
                }
                oo$Dth2[, i] <- Da1a1 * ar.k * ar.j + Da0a1 * 
                  ar.k * ar1.j + Da1 * ar.kj + Da0a0 * ar1.k * 
                  ar1.j + Da0a1 * ar1.k * ar.j + Da0 * ar1.kj
                oo$Dmuth2[, i] <- Dmua1a1 * ar.k * ar.j + Dmua0a1 * 
                  ar.k * ar1.j + Dmua1 * ar.kj + Dmua0a0 * ar1.k * 
                  ar1.j + Dmua0a1 * ar1.k * ar.j + Dmua0 * ar1.kj
                oo$Dmu2th2[, i] <- Dmu2a1a1 * ar.k * ar.j + Dmu2a0a1 * 
                  ar.k * ar1.j + Dmu2a1 * ar.kj + Dmu2a0a0 * 
                  ar1.k * ar1.j + Dmu2a0a1 * ar1.k * ar.j + Dmu2a0 * 
                  ar1.kj
            }
        }
        oo
    }
    aic <- function(y, mu, theta = NULL, wt, dev) {
        Fdiff <- function(a, b) {
            h <- rep(1, length(b))
            h[b > 0] <- -1
            eb <- exp(b * h)
            h <- h * 0 + 1
            h[a > 0] <- -1
            ea <- exp(a * h)
            ind <- b < 0
            bi <- eb[ind]
            ai <- ea[ind]
            h[ind] <- bi/(1 + bi) - ai/(1 + ai)
            ind1 <- a > 0
            bi <- eb[ind1]
            ai <- ea[ind1]
            h[ind1] <- (ai - bi)/((ai + 1) * (bi + 1))
            ind <- !ind & !ind1
            bi <- eb[ind]
            ai <- ea[ind]
            h[ind] <- (1 - ai * bi)/((bi + 1) * (ai + 1))
            h
        }
        if (is.null(theta)) 
            theta <- get(".Theta")
        R = length(theta) + 2
        alpha <- rep(0, R + 1)
        alpha[1] <- -Inf
        alpha[R + 1] <- Inf
        alpha[2] <- -1
        if (R > 2) {
            ind <- 3:R
            alpha[ind] <- alpha[2] + cumsum(exp(theta))
        }
        al1 <- alpha[y + 1]
        al0 = alpha[y]
        f <- Fdiff(al0 - mu, al1 - mu)
        -2 * sum(log(f))
    }
    ls <- function(y, w, n, theta, scale) {
        return(list(ls = 0, lsth1 = rep(0, R - 2), lsth2 = matrix(0, 
            R - 2, R - 2)))
        F <- function(x) {
            h <- ind <- x > 0
            h[ind] <- 1/(exp(-x[ind]) + 1)
            x <- exp(x[!ind])
            h[!ind] <- (x/(1 + x))
            h
        }
        Fdiff <- function(a, b) {
            h <- rep(1, length(b))
            h[b > 0] <- -1
            eb <- exp(b * h)
            h <- h * 0 + 1
            h[a > 0] <- -1
            ea <- exp(a * h)
            ind <- b < 0
            bi <- eb[ind]
            ai <- ea[ind]
            h[ind] <- bi/(1 + bi) - ai/(1 + ai)
            ind1 <- a > 0
            bi <- eb[ind1]
            ai <- ea[ind1]
            h[ind1] <- (ai - bi)/((ai + 1) * (bi + 1))
            ind <- !ind & !ind1
            bi <- eb[ind]
            ai <- ea[ind]
            h[ind] <- (1 - ai * bi)/((bi + 1) * (ai + 1))
            h
        }
        R = length(theta) + 2
        alpha <- rep(0, R + 1)
        alpha[1] <- -Inf
        alpha[R + 1] <- Inf
        alpha[2] <- -1
        if (R > 2) {
            ind <- 3:R
            alpha[ind] <- alpha[2] + cumsum(exp(theta))
        }
        al1 <- alpha[y + 1]
        al0 = alpha[y]
        g1 <- F((al1 - al0)/2)
        g0 <- F((al0 - al1)/2)
        A <- Fdiff((al0 - al1)/2, (al1 - al0)/2)
        ls <- sum(log(A))
        B <- g1^2 - g1 + g0^2 - g0
        C <- 2 * g1^3 - 3 * g1^2 + g1 - 2 * g0^3 + 3 * g0^2 - 
            g0
        Da0 <- 0.5 * B/A
        Da1 <- -0.5 * B/A
        Da0a0 <- 0.25 * C/A - 0.25 * B^2/A^2
        Da1a1 <- 0.25 * C/A - 0.25 * B^2/A^2
        Da0a1 <- -0.25 * C/A + 0.25 * B^2/A^2
        i <- 0
        n2d <- (R - 2) * (R - 1)/2
        n <- length(y)
        Dth <- matrix(0, n, R - 2)
        Dth2 <- matrix(0, n, n2d)
        for (j in 1:(R - 2)) for (k in j:(R - 2)) {
            i <- i + 1
            ind <- y >= j
            ar1.k <- ar.k <- rep(exp(theta[k]), n)
            ar.k[y == R | y <= k] <- 0
            ar1.k[y < k + 2] <- 0
            ar.j <- ar1.j <- rep(exp(theta[j]), n)
            ar.j[y == R | y <= j] <- 0
            ar1.j[y < j + 2] <- 0
            ar.kj <- ar1.kj <- rep(0, n)
            if (k == j) {
                ar.kj[y > k & y < R] <- exp(theta[k])
                ar1.kj[y > k + 1] <- exp(theta[k])
                Dth[ind, k] <- Da1[ind] * ar.k[ind] + Da0[ind] * 
                  ar1.k[ind]
            }
            Dth2[, i] <- Da1a1 * ar.k * ar.j + Da0a1 * ar.k * 
                ar1.j + Da1 * ar.kj + Da0a0 * ar1.k * ar1.j + 
                Da0a1 * ar1.k * ar.j + Da0 * ar1.kj
        }
        lsth2 = colSums(Dth2)
        if (R > 2) {
            ls2 <- matrix(0, R - 2, R - 2)
            ii <- 0
            for (i in 1:(R - 2)) for (j in i:(R - 2)) {
                ii <- ii + 1
                ls2[i, j] <- ls2[j, i] <- lsth2[ii]
            }
        }
        list(ls = ls, lsth1 = colSums(Dth), lsth2 = ls2)
    }
    preinitialize <- expression({
        ocat.ini <- function(R, y) {
            if (R < 3) return
            y <- c(1:R, y)
            p <- cumsum(tabulate(y[is.finite(y)])/length(y[is.finite(y)]))
            eta <- if (p[1] == 0) 5 else -1 - log(p[1]/(1 - p[1]))
            theta <- rep(-1, R - 1)
            for (i in 2:(R - 1)) theta[i] <- log(p[i]/(1 - p[i])) + 
                eta
            theta <- diff(theta)
            theta[theta <= 0.01] <- 0.01
            theta <- log(theta)
        }
        R3 <- length(G$family$getTheta()) + 2
        if (R3 > 2 && G$family$n.theta > 0) {
            Theta <- ocat.ini(R3, G$y)
            G$family$putTheta(Theta)
        }
    })
    initialize <- expression({
        R <- length(family$getTheta()) + 2
        if (any(y < 1) || any(y > R)) stop("values out of range")
        n <- rep(1, nobs)
        alpha <- rep(0, R + 1)
        alpha[1] <- -2
        alpha[2] <- -1
        if (R > 2) {
            ind <- 3:R
            alpha[ind] <- alpha[2] + cumsum(exp(family$getTheta()))
        }
        alpha[R + 1] <- alpha[R] + 1
        mustart <- (alpha[y + 1] + alpha[y])/2
    })
    residuals <- function(object, type = c("deviance", "working", 
        "response")) {
        if (type == "working") {
            res <- object$residuals
        }
        else if (type == "response") {
            theta <- object$family$getTheta()
            mu <- object$linear.predictors
            R = length(theta) + 2
            alpha <- rep(0, R + 1)
            alpha[1] <- -Inf
            alpha[R + 1] <- Inf
            alpha[2] <- -1
            if (R > 2) {
                ind <- 3:R
                alpha[ind] <- alpha[2] + cumsum(exp(theta))
            }
            fv <- mu * NA
            for (i in 1:(R + 1)) {
                ind <- mu > alpha[i] & mu <= alpha[i + 1]
                fv[ind] <- i
            }
            res <- object$y - fv
        }
        else if (type == "deviance") {
            y <- object$y
            mu <- object$fitted.values
            wts <- object$prior.weights
            res <- object$family$dev.resids(y, mu, wts)
            s <- attr(res, "sign")
            if (is.null(s)) 
                s <- sign(y - mu)
            res <- as.numeric(sqrt(pmax(res, 0)) * s)
        }
        res
    }
    predict <- function(family, se = FALSE, eta = NULL, y = NULL, 
        X = NULL, beta = NULL, off = NULL, Vb = NULL) {
        ocat.prob <- function(theta, lp, se = NULL) {
            R <- length(theta)
            dp <- prob <- matrix(0, length(lp), R + 2)
            prob[, R + 2] <- 1
            for (i in 1:R) {
                x <- theta[i] - lp
                ind <- x > 0
                prob[ind, i + 1] <- 1/(1 + exp(-x[ind]))
                ex <- exp(x[!ind])
                prob[!ind, i + 1] <- ex/(1 + ex)
                dp[, i + 1] <- prob[, i + 1] * (prob[, i + 1] - 
                  1)
            }
            prob <- t(diff(t(prob)))
            dp <- t(diff(t(dp)))
            if (!is.null(se)) 
                se <- as.numeric(se) * abs(dp)
            list(prob, se)
        }
        theta <- family$getTheta(TRUE)
        if (is.null(eta)) {
            mu <- X %*% beta + off
            se <- if (se) 
                sqrt(pmax(0, rowSums((X %*% Vb) * X)))
            else NULL
            p <- ocat.prob(theta, mu, se)
            if (is.null(se)) 
                return(p)
            else {
                names(p) <- c("fit", "se.fit")
                return(p)
            }
        }
        else {
            R = length(theta) + 2
            alpha <- rep(0, R)
            alpha[1] <- -Inf
            alpha[R] <- Inf
            fv <- eta * NA
            for (i in 1:(R + 1)) {
                ind <- eta > alpha[i] & eta <= alpha[i + 1]
                fv[ind] <- i
            }
            return(fv)
        }
    }
    rd <- function(mu, wt, scale) {
        theta <- get(".Theta")
        R = length(theta) + 2
        alpha <- rep(0, R + 1)
        alpha[1] <- -Inf
        alpha[R + 1] <- Inf
        alpha[2] <- -1
        if (R > 2) {
            ind <- 3:R
            alpha[ind] <- alpha[2] + cumsum(exp(theta))
        }
        y <- u <- runif(length(mu))
        u <- mu + log(u/(1 - u))
        for (i in 1:R) {
            y[u > alpha[i] & u <= alpha[i + 1]] <- i
        }
        y
    }
    environment(dev.resids) <- environment(aic) <- environment(putTheta) <- environment(getTheta) <- environment(rd) <- environment(predict) <- env
    structure(list(family = "Ordered Categorical", link = linktemp, 
        linkfun = stats$linkfun, linkinv = stats$linkinv, dev.resids = dev.resids, 
        Dd = Dd, aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
        postproc = postproc, preinitialize = preinitialize, ls = ls, 
        rd = rd, residuals = residuals, validmu = validmu, valideta = stats$valideta, 
        n.theta = n.theta, ini.theta = iniTheta, putTheta = putTheta, 
        predict = predict, step = 1, getTheta = getTheta, no.r.sq = TRUE), 
        class = c("extended.family", "family"))
}


fixDependence <- function (X1, X2, tol = .Machine$double.eps^0.5, rank.def = 0, 
    strict = FALSE) 
{
    qr1 <- qr(X1, LAPACK = TRUE)
    R11 <- abs(qr.R(qr1)[1, 1])
    r <- ncol(X1)
    n <- nrow(X1)
    if (strict) {
        QtX2 <- qr.qty(qr1, X2)
        QtX2[-(1:r), ] <- 0
        mdiff <- colMeans(abs(X2 - qr.qy(qr1, QtX2)))
        if (rank.def > 0) 
            ind <- (1:ncol(X2))[rank(mdiff) <= rank.def]
        else ind <- (1:ncol(X2))[mdiff < R11 * tol]
        if (length(ind) < 1) 
            ind <- NULL
    }
    else {
        QtX2 <- qr.qty(qr1, X2)[(r + 1):n, ]
        qr2 <- qr(QtX2, LAPACK = TRUE)
        R <- qr.R(qr2)
        r0 <- r <- nrow(R)
        if (rank.def > 0 && rank.def <= nrow(R)) 
            r0 <- r - rank.def
        else while (r0 > 0 && mean(abs(R[r0:r, r0:r])) < R11 * 
            tol) r0 <- r0 - 1
        r0 <- r0 + 1
        if (r0 > r) 
            return(NULL)
        else ind <- qr2$pivot[r0:r]
    }
    ind
}


Predict.matrix.cr.smooth <- function (object, data) 
{
    x <- data[[object$term]]
    if (length(x) < 1) 
        stop("no data to predict at")
    nx <- length(x)
    nk <- object$bs.dim
    X <- rep(0, nx * nk)
    S <- 1
    F.supplied <- 1
    if (is.null(object$F)) 
        stop("F is missing from cr smooth - refit model with current mgcv")
    oo <- .C(C_crspl, x = as.double(x), n = as.integer(nx), xk = as.double(object$xp), 
        nk = as.integer(nk), X = as.double(X), S = as.double(S), 
        F = as.double(object$F), Fsupplied = as.integer(F.supplied))
    X <- matrix(oo$X, nx, nk)
    X
}


sim2jam <- function (sam, pregam, edf.type = 2, burnin = 0) 
{
    if (is.null(sam$b)) 
        stop("coefficient simulation data is missing")
    if (burnin > 0) {
        nc <- dim(sam$b)[2]
        if (burnin >= nc * 0.9) {
            warning("burnin too large, reset")
            burnin <- min(nc - 1, floor(nc * 0.9))
        }
        ind <- (burnin + 1):nc
        sam$b <- sam$b[, ind, ]
        if (!is.null(sam$mu)) 
            sam$mu <- sam$mu[, ind, ]
        if (!is.null(sam$rho)) 
            sam$rho <- sam$rho[, ind, ]
        if (!is.null(sam$scale)) 
            sam$scale <- sam$scale[, ind, ]
    }
    pregam$Vp <- cov(t(sam$b[, , 1]))
    pregam$coefficients <- rowMeans(sam$b[, , 1])
    pregam$sig2 <- if (is.null(sam$scale)) 
        1
    else mean(sam$scale)
    n.chain <- dim(sam$b)[3]
    if (n.chain > 1) {
        for (i in 2:n.chain) {
            pregam$Vp <- pregam$Vp + cov(t(sam$b[, , i]))
            pregam$coefficients <- pregam$coefficients + rowMeans(sam$b[, 
                , i])
        }
        pregam$Vp <- pregam$Vp/n.chain
        pregam$coefficients <- pregam$coefficients/n.chain
    }
    if (edf.type < 2 && is.null(sam$rho)) {
        edf.type <- 2
        warning("rho missing from simulation data edf.type reset to 2")
    }
    if (edf.type > 0) {
        if (is.null(sam$mu)) {
            eta <- pregam$X %*% pregam$coefficients
            mu <- pregam$family$linkinv(eta)
        }
        else {
            mu <- rowMeans(sam$mu)
            eta <- pregam$family$linkfun(mu)
        }
        w <- as.numeric(pregam$w * pregam$family$mu.eta(eta)^2/pregam$family$variance(mu))
        XWX <- t(pregam$X) %*% (w * pregam$X)
    }
    else XWX <- t(pregam$X) %*% (pregam$X)
    if (edf.type < 2) {
        rho <- rowMeans(sam$rho)
        lambda <- exp(rho)
        XWXS <- XWX
        for (i in 1:length(lambda)) {
            ind <- pregam$off[i]:(pregam$off[i] + ncol(pregam$S[[i]]) - 
                1)
            XWXS[ind, ind] <- XWXS[ind, ind] + pregam$S[[i]] * 
                lambda[i]
        }
        pregam$edf <- diag(solve(XWXS, XWX))
    }
    else pregam$edf <- rowSums(pregam$Vp * t(XWX))/pregam$sig2
    class(pregam) <- "jam"
    pregam
}


fix.family.link <- function (fam) 
UseMethod("fix.family.link")


Predict.matrix.ts.smooth <- function (object, data) 
{
    Predict.matrix.tprs.smooth(object, data)
}


anova.gam <- function (object, ..., dispersion = NULL, test = NULL, freq = FALSE, 
    p.type = 0) 
{
    dotargs <- list(...)
    named <- if (is.null(names(dotargs))) 
        rep(FALSE, length(dotargs))
    else (names(dotargs) != "")
    if (any(named)) 
        warning("The following arguments to anova.glm(..) are invalid and dropped: ", 
            paste(deparse(dotargs[named]), collapse = ", "))
    dotargs <- dotargs[!named]
    is.glm <- unlist(lapply(dotargs, function(x) inherits(x, 
        "glm")))
    dotargs <- dotargs[is.glm]
    if (length(dotargs) > 0) 
        return(anova(structure(c(list(object), dotargs), class = "glmlist"), 
            dispersion = dispersion, test = test))
    if (!is.null(test)) 
        warning("test argument ignored")
    if (!inherits(object, "gam")) 
        stop("anova.gam called with non gam object")
    sg <- summary(object, dispersion = dispersion, freq = freq, 
        p.type = p.type)
    class(sg) <- "anova.gam"
    sg
}


ziP <- function (theta = NULL, link = "identity", b = 0) 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    if (linktemp %in% c("identity")) {
        stats <- make.link(linktemp)
    }
    else stop(linktemp, " link not available for zero inflated; available link for `lambda' is only  \"loga\"")
    n.theta <- 2
    if (!is.null(theta)) {
        iniTheta <- c(theta[1], theta[2])
        n.theta <- 0
    }
    else iniTheta <- c(0, 0)
    env <- new.env(parent = environment(ziP))
    if (b < 0) 
        b <- 0
    assign(".b", b, envir = env)
    assign(".Theta", iniTheta, envir = env)
    getTheta <- function(trans = FALSE) {
        th <- get(".Theta")
        if (trans) {
            th[2] <- get(".b") + exp(th[2])
        }
        th
    }
    putTheta <- function(theta) assign(".Theta", theta, envir = environment(sys.function()))
    validmu <- function(mu) all(is.finite(mu))
    dev.resids <- function(y, mu, wt, theta = NULL) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        b <- get(".b")
        p <- theta[1] + (b + exp(theta[2])) * mu
        -2 * zipll(y, mu, p, deriv = 0)$l
    }
    Dd <- function(y, mu, theta, wt = NULL, level = 0) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        deriv <- 1
        if (level == 1) 
            deriv <- 2
        else if (level > 1) 
            deriv <- 4
        b <- get(".b")
        g <- lind(mu, theta, level, b)
        z <- zipll(y, mu, g$p, deriv)
        oo <- list()
        n <- length(y)
        if (is.null(wt)) 
            wt <- rep(1, n)
        oo$Dmu <- -2 * wt * (z$l1[, 1] + z$l1[, 2] * g$p.l)
        oo$Dmu2 <- -2 * wt * (z$l2[, 1] + 2 * z$l2[, 2] * g$p.l + 
            z$l2[, 3] * g$p.l^2 + z$l1[, 2] * g$p.ll)
        oo$EDmu2 <- -2 * wt * (z$El2[, 1] + 2 * z$El2[, 2] * 
            g$p.l + z$El2[, 3] * g$p.l^2)
        if (level > 0) {
            oo$Dth <- -2 * wt * z$l1[, 2] * g$p.th
            oo$Dmuth <- -2 * wt * (z$l2[, 2] * g$p.th + z$l2[, 
                3] * g$p.l * g$p.th + z$l1[, 2] * g$p.lth)
            oo$Dmu2th <- -2 * wt * (z$l3[, 2] * g$p.th + 2 * 
                z$l3[, 3] * g$p.l * g$p.th + 2 * z$l2[, 2] * 
                g$p.lth + z$l3[, 4] * g$p.l^2 * g$p.th + z$l2[, 
                3] * (2 * g$p.l * g$p.lth + g$p.th * g$p.ll) + 
                z$l1[, 2] * g$p.llth)
            oo$Dmu3 <- -2 * wt * (z$l3[, 1] + 3 * z$l3[, 2] * 
                g$p.l + 3 * z$l3[, 3] * g$p.l^2 + 3 * z$l2[, 
                2] * g$p.ll + z$l3[, 4] * g$p.l^3 + 3 * z$l2[, 
                3] * g$p.l * g$p.ll + z$l1[, 2] * g$p.lll)
        }
        if (level > 1) {
            p.thth <- matrix(0, n, 3)
            p.thth[, 1] <- g$p.th[, 1]^2
            p.thth[, 2] <- g$p.th[, 1] * g$p.th[, 2]
            p.thth[, 3] <- g$p.th[, 2]^2
            oo$Dth2 <- -2 * wt * (z$l2[, 3] * p.thth + z$l1[, 
                2] * g$p.th2)
            p.lthth <- matrix(0, n, 3)
            p.lthth[, 1] <- g$p.th[, 1] * g$p.lth[, 1] * 2
            p.lthth[, 2] <- g$p.th[, 1] * g$p.lth[, 2] + g$p.th[, 
                2] * g$p.lth[, 1]
            p.lthth[, 3] <- g$p.th[, 2] * g$p.lth[, 2] * 2
            oo$Dmuth2 <- -2 * wt * (z$l3[, 3] * p.thth + z$l2[, 
                2] * g$p.th2 + z$l3[, 4] * g$p.l * p.thth + z$l2[, 
                3] * (g$p.th2 * g$p.l + p.lthth) + z$l1[, 2] * 
                g$p.lth2)
            p.lthlth <- matrix(0, n, 3)
            p.lthlth[, 1] <- g$p.lth[, 1] * g$p.lth[, 1] * 2
            p.lthlth[, 2] <- g$p.lth[, 1] * g$p.lth[, 2] + g$p.lth[, 
                2] * g$p.lth[, 1]
            p.lthlth[, 3] <- g$p.lth[, 2] * g$p.lth[, 2] * 2
            p.llthth <- matrix(0, n, 3)
            p.llthth[, 1] <- g$p.th[, 1] * g$p.llth[, 1] * 2
            p.llthth[, 2] <- g$p.th[, 1] * g$p.llth[, 2] + g$p.th[, 
                2] * g$p.llth[, 1]
            p.llthth[, 3] <- g$p.th[, 2] * g$p.llth[, 2] * 2
            oo$Dmu2th2 <- -2 * wt * (z$l4[, 3] * p.thth + z$l3[, 
                2] * g$p.th2 + 2 * z$l4[, 4] * p.thth * g$p.l + 
                2 * z$l3[, 3] * (g$p.th2 * g$p.l + p.lthth) + 
                2 * z$l2[, 2] * g$p.lth2 + z$l4[, 5] * p.thth * 
                g$p.l^2 + z$l3[, 4] * (g$p.th2 * g$p.l^2 + 2 * 
                p.lthth * g$p.l + p.thth * g$p.ll) + z$l2[, 3] * 
                (p.lthlth + 2 * g$p.l * g$p.lth2 + p.llthth + 
                  g$p.th2 * g$p.ll) + z$l1[, 2] * g$p.llth2)
            oo$Dmu3th <- -2 * wt * (z$l4[, 2] * g$p.th + 3 * 
                z$l4[, 3] * g$p.th * g$p.l + 3 * z$l3[, 2] * 
                g$p.lth + 2 * z$l4[, 4] * g$p.th * g$p.l^2 + 
                z$l3[, 3] * (6 * g$p.lth * g$p.l + 3 * g$p.th * 
                  g$p.ll) + 3 * z$l2[, 2] * g$p.llth + z$l4[, 
                4] * g$p.th * g$p.l^2 + z$l4[, 5] * g$p.th * 
                g$p.l^3 + 3 * z$l3[, 4] * (g$p.l^2 * g$p.lth + 
                g$p.th * g$p.l * g$p.ll) + z$l2[, 3] * (3 * g$p.lth * 
                g$p.ll + 3 * g$p.l * g$p.llth + g$p.th * g$p.lll) + 
                z$l1[, 2] * g$p.lllth)
            oo$Dmu4 <- -2 * wt * (z$l4[, 1] + 4 * z$l4[, 2] * 
                g$p.l + 6 * z$l4[, 3] * g$p.l^2 + 6 * z$l3[, 
                2] * g$p.ll + 4 * z$l4[, 4] * g$p.l^3 + 12 * 
                z$l3[, 3] * g$p.l * g$p.ll + 4 * z$l2[, 2] * 
                g$p.lll + z$l4[, 5] * g$p.l^4 + 6 * z$l3[, 4] * 
                g$p.l^2 * g$p.ll + z$l2[, 3] * (4 * g$p.l * g$p.lll + 
                3 * g$p.ll^2) + z$l1[, 2] * g$p.llll)
        }
        oo
    }
    aic <- function(y, mu, theta = NULL, wt, dev) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        b <- get(".b")
        p <- theta[1] + (b + exp(theta[2])) * mu
        sum(-2 * wt * zipll(y, mu, p, 0)$l)
    }
    ls <- function(y, w, n, theta, scale) {
        list(ls = 0, lsth1 = c(0, 0), lsth2 = matrix(0, 2, 2))
    }
    initialize <- expression({
        if (any(y < 0)) stop("negative values not allowed for the zero inflated Poisson family")
        if (all.equal(y, round(y)) != TRUE) {
            stop("Non-integer response variables are not allowed with ziP ")
        }
        if ((min(y) == 0 && max(y) == 1)) stop("Using ziP for binary data makes no sense")
        n <- rep(1, nobs)
        mustart <- log(y + (y == 0)/5)
    })
    postproc <- expression({
        object$family$family <- paste("Zero inflated Poisson(", 
            paste(round(object$family$getTheta(TRUE), 3), collapse = ","), 
            ")", sep = "")
        lf <- object$family$saturated.ll(G$y, family, object$prior.weights)
        object$family$data <- list(ls = lf)
        l2 <- object$family$dev.resids(G$y, object$linear.predictors, 
            object$prior.weights)
        object$deviance <- sum(l2 - lf)
        fnull <- function(gamma, object) {
            sum(object$family$dev.resids(object$y, rep(gamma, 
                length(object$y)), object$prior.weights))
        }
        meany <- mean(object$y)
        object$null.deviance <- optimize(fnull, interval = c(meany/5, 
            meany * 3), object = object)$objective - sum(lf)
    })
    rd <- function(mu, wt, scale) {
        rzip <- function(gamma, theta) {
            y <- gamma
            n <- length(y)
            lambda <- exp(gamma)
            mlam <- max(c(lambda[is.finite(lambda)], .Machine$double.eps^0.2))
            lambda[!is.finite(lambda)] <- mlam
            b <- get(".b")
            eta <- theta[1] + (b + exp(theta[2])) * gamma
            p <- 1 - exp(-exp(eta))
            ind <- p > runif(n)
            y[!ind] <- 0
            lami <- lambda[ind]
            yi <- p0 <- dpois(0, lami)
            nearly1 <- 1 - .Machine$double.eps * 10
            ii <- p0 > nearly1
            yi[ii] <- 1
            yi[!ii] <- qpois(runif(sum(!ii), p0[!ii], nearly1), 
                lami[!ii])
            y[ind] <- yi
            y
        }
        rzip(mu, get(".Theta"))
    }
    saturated.ll <- function(y, family, wt = rep(1, length(y))) {
        pind <- y > 0
        wt <- wt[pind]
        y <- y[pind]
        mu <- log(y)
        keep.on <- TRUE
        theta <- family$getTheta()
        r <- family$Dd(y, mu, theta, wt)
        l <- family$dev.resids(y, mu, wt, theta)
        lmax <- max(abs(l))
        ucov <- abs(r$Dmu) > lmax * 1e-07
        k <- 0
        while (keep.on) {
            step <- -r$Dmu/r$Dmu2
            step[!ucov] <- 0
            mu1 <- mu + step
            l1 <- family$dev.resids(y, mu1, wt, theta)
            ind <- l1 > l & ucov
            kk <- 0
            while (sum(ind) > 0 && kk < 50) {
                step[ind] <- step[ind]/2
                mu1 <- mu + step
                l1 <- family$dev.resids(y, mu1, wt, theta)
                ind <- l1 > l & ucov
                kk <- kk + 1
            }
            mu <- mu1
            l <- l1
            r <- family$Dd(y, mu, theta, wt)
            ucov <- abs(r$Dmu) > lmax * 1e-07
            k <- k + 1
            if (all(!ucov) || k == 100) 
                keep.on <- FALSE
        }
        l1 <- rep(0, length(pind))
        l1[pind] <- l
        l1
    }
    residuals <- function(object, type = c("deviance", "working", 
        "response")) {
        if (type == "working") {
            res <- object$residuals
        }
        else if (type == "response") {
            res <- object$y - predict.gam(object, type = "response")
        }
        else if (type == "deviance") {
            y <- object$y
            mu <- object$linear.predictors
            wts <- object$prior.weights
            res <- object$family$dev.resids(y, mu, wts)
            res <- res - object$family$saturated.ll(y, object$family, 
                wts)
            fv <- predict.gam(object, type = "response")
            s <- attr(res, "sign")
            if (is.null(s)) 
                s <- sign(y - fv)
            res <- as.numeric(sqrt(pmax(res, 0)) * s)
        }
        res
    }
    predict <- function(family, se = FALSE, eta = NULL, y = NULL, 
        X = NULL, beta = NULL, off = NULL, Vb = NULL) {
        theta <- family$getTheta()
        if (is.null(eta)) {
            gamma <- drop(X %*% beta + off)
            se <- if (se) 
                drop(sqrt(pmax(0, rowSums((X %*% Vb) * X))))
            else NULL
        }
        else {
            se <- NULL
            gamma <- eta
        }
        b <- get(".b")
        eta <- theta[1] + (b + exp(theta[2])) * gamma
        et <- exp(eta)
        mu <- p <- 1 - exp(-et)
        fv <- lambda <- exp(gamma)
        ind <- gamma < log(.Machine$double.eps)/2
        mu[!ind] <- lambda[!ind]/(1 - exp(-lambda[!ind]))
        mu[ind] <- 1
        fv <- list(p * mu)
        if (is.null(se)) 
            return(fv)
        else {
            dp.dg <- p
            ind <- eta < log(.Machine$double.xmax)/2
            dp.dg[!ind] <- 0
            dp.dg <- exp(-et) * et * exp(theta[2])
            dmu.dg <- (lambda + 1) * mu - mu^2
            fv[[2]] <- abs(dp.dg * mu + dmu.dg * p) * se
            names(fv) <- c("fit", "se.fit")
            return(fv)
        }
    }
    environment(saturated.ll) <- environment(dev.resids) <- environment(Dd) <- environment(aic) <- environment(getTheta) <- environment(rd) <- environment(predict) <- environment(putTheta) <- env
    structure(list(family = "zero inflated Poisson", link = linktemp, 
        linkfun = stats$linkfun, linkinv = stats$linkinv, dev.resids = dev.resids, 
        Dd = Dd, rd = rd, residuals = residuals, aic = aic, mu.eta = stats$mu.eta, 
        g2g = stats$g2g, g3g = stats$g3g, g4g = stats$g4g, initialize = initialize, 
        postproc = postproc, ls = ls, no.r.sq = TRUE, validmu = validmu, 
        valideta = stats$valideta, n.theta = n.theta, predict = predict, 
        ini.theta = iniTheta, putTheta = putTheta, getTheta = getTheta, 
        saturated.ll = saturated.ll), class = c("extended.family", 
        "family"))
}


nb <- function (theta = NULL, link = "log") 
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
        else stop(linktemp, " link not available for negative binomial family; available links are \"identity\", \"log\" and \"sqrt\"")
    }
    n.theta <- 1
    if (!is.null(theta) && theta != 0) {
        if (theta > 0) {
            iniTheta <- log(theta)
            n.theta <- 0
        }
        else iniTheta <- log(-theta)
    }
    else iniTheta <- 0
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", iniTheta, envir = env)
    getTheta <- function(trans = FALSE) if (trans) 
        exp(get(".Theta"))
    else get(".Theta")
    putTheta <- function(theta) assign(".Theta", theta, envir = environment(sys.function()))
    variance <- function(mu) mu + mu^2/exp(get(".Theta"))
    validmu <- function(mu) all(mu > 0)
    dev.resids <- function(y, mu, wt, theta = NULL) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        theta <- exp(theta)
        2 * wt * (y * log(pmax(1, y)/mu) - (y + theta) * log((y + 
            theta)/(mu + theta)))
    }
    Dd <- function(y, mu, theta, wt, level = 0) {
        theta <- exp(theta)
        yth <- y + theta
        muth <- mu + theta
        r <- list()
        r$Dmu <- 2 * wt * (yth/muth - y/mu)
        r$Dmu2 <- -2 * wt * (yth/muth^2 - y/mu^2)
        r$EDmu2 <- 2 * wt * (1/mu - 1/muth)
        if (level > 0) {
            r$Dth <- -2 * wt * theta * (log(yth/muth) + (1 - 
                yth/muth))
            r$Dmuth <- 2 * wt * theta * (1 - yth/muth)/muth
            r$Dmu3 <- 4 * wt * (yth/muth^3 - y/mu^3)
            r$Dmu2th <- 2 * wt * theta * (2 * yth/muth - 1)/muth^2
        }
        if (level > 1) {
            r$Dmu4 <- 2 * wt * (6 * y/mu^4 - 6 * yth/muth^4)
            r$Dth2 <- -2 * wt * theta * (log(yth/muth) + theta * 
                yth/muth^2 - yth/muth - 2 * theta/muth + 1 + 
                theta/yth)
            r$Dmuth2 <- 2 * wt * theta * (2 * theta * yth/muth^2 - 
                yth/muth - 2 * theta/muth + 1)/muth
            r$Dmu2th2 <- 2 * wt * theta * (-6 * yth * theta/muth^2 + 
                2 * yth/muth + 4 * theta/muth - 1)/muth^2
            r$Dmu3th <- 4 * wt * theta * (1 - 3 * yth/muth)/muth^3
        }
        r
    }
    aic <- function(y, mu, theta = NULL, wt, dev) {
        if (is.null(theta)) 
            theta <- get(".Theta")
        Theta <- exp(theta)
        term <- (y + Theta) * log(mu + Theta) - y * log(mu) + 
            lgamma(y + 1) - Theta * log(Theta) + lgamma(Theta) - 
            lgamma(Theta + y)
        2 * sum(term * wt)
    }
    ls <- function(y, w, n, theta, scale) {
        Theta <- exp(theta)
        ylogy <- y
        ind <- y > 0
        ylogy[ind] <- y[ind] * log(y[ind])
        term <- (y + Theta) * log(y + Theta) - ylogy + lgamma(y + 
            1) - Theta * log(Theta) + lgamma(Theta) - lgamma(Theta + 
            y)
        ls <- -sum(term * w)
        yth <- y + Theta
        lyth <- log(yth)
        psi0.yth <- digamma(yth)
        psi0.th <- digamma(Theta)
        term <- Theta * (lyth - psi0.yth + psi0.th - theta)
        lsth <- -sum(term * w)
        psi1.yth <- trigamma(yth)
        psi1.th <- trigamma(Theta)
        term <- Theta * (lyth - Theta * psi1.yth - psi0.yth + 
            Theta/yth + Theta * psi1.th + psi0.th - theta - 1)
        lsth2 <- -sum(term * w)
        list(ls = ls, lsth1 = lsth, lsth2 = lsth2)
    }
    initialize <- expression({
        if (any(y < 0)) stop("negative values not allowed for the negative binomial family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0)/6
    })
    postproc <- expression({
        object$family$family <- paste("Negative Binomial(", round(object$family$getTheta(TRUE), 
            3), ")", sep = "")
    })
    rd <- function(mu, wt, scale) {
        Theta <- exp(get(".Theta"))
        rnbinom(mu, size = Theta, mu = mu)
    }
    qf <- function(p, mu, wt, scale) {
        Theta <- exp(get(".Theta"))
        qnbinom(p, size = Theta, mu = mu)
    }
    environment(dev.resids) <- environment(aic) <- environment(getTheta) <- environment(rd) <- environment(qf) <- environment(variance) <- environment(putTheta) <- env
    structure(list(family = "negative binomial", link = linktemp, 
        linkfun = stats$linkfun, linkinv = stats$linkinv, dev.resids = dev.resids, 
        Dd = Dd, variance = variance, aic = aic, mu.eta = stats$mu.eta, 
        initialize = initialize, postproc = postproc, ls = ls, 
        validmu = validmu, valideta = stats$valideta, n.theta = n.theta, 
        ini.theta = iniTheta, putTheta = putTheta, getTheta = getTheta, 
        rd = rd, qf = qf), class = c("extended.family", "family"))
}


gam.fit <- function (G, start = NULL, etastart = NULL, mustart = NULL, family = gaussian(), 
    control = gam.control(), gamma = 1, fixedSteps = (control$maxit + 
        1), ...) 
{
    intercept <- G$intercept
    conv <- FALSE
    n <- nobs <- NROW(G$y)
    nvars <- NCOL(G$X)
    y <- G$y
    X <- G$X
    if (nvars == 0) 
        stop("Model seems to contain no terms")
    olm <- G$am
    find.theta <- FALSE
    if (substr(family$family[1], 1, 17) == "Negative Binomial") {
        Theta <- family$getTheta()
        if (length(Theta) == 1) {
            find.theta <- FALSE
            G$sig2 <- 1
        }
        else {
            if (length(Theta) > 2) 
                warning("Discrete Theta search not available with performance iteration")
            Theta <- range(Theta)
            T.max <- Theta[2]
            T.min <- Theta[1]
            Theta <- sqrt(T.max * T.min)
            find.theta <- TRUE
        }
        nb.link <- family$link
    }
    n.S <- length(G$S)
    if (n.S > 0) {
        S.size <- 0
        for (i in 1:n.S) S.size[i] <- mean(abs(G$S[[i]]))
    }
    weights <- G$w
    n.score <- sum(weights != 0)
    offset <- G$offset
    variance <- family$variance
    dev.resids <- family$dev.resids
    aic <- family$aic
    linkinv <- family$linkinv
    linkfun <- family$linkfun
    mu.eta <- family$mu.eta
    if (!is.function(variance) || !is.function(linkinv)) 
        stop("illegal `family' argument")
    valideta <- family$valideta
    if (is.null(valideta)) 
        valideta <- function(eta) TRUE
    validmu <- family$validmu
    if (is.null(validmu)) 
        validmu <- function(mu) TRUE
    if (is.null(mustart)) {
        eval(family$initialize)
    }
    else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
    }
    if (NCOL(y) > 1) 
        stop("y must be univariate unless binomial")
    coefold <- NULL
    eta <- if (!is.null(etastart)) 
        etastart
    else if (!is.null(start)) 
        if (length(start) != nvars) 
            stop(gettextf("Length of start should equal %d and correspond to initial coefs.", 
                nvars))
        else {
            coefold <- start
            offset + as.vector(if (NCOL(G$X) == 1) 
                G$X * start
            else G$X %*% start)
        }
    else family$linkfun(mustart)
    mu <- linkinv(eta)
    if (!(validmu(mu) && valideta(eta))) 
        stop("Can't find valid starting values: please specify some")
    devold <- sum(dev.resids(y, mu, weights))
    boundary <- FALSE
    scale <- G$sig2
    msp <- G$sp
    magic.control <- list(tol = G$conv.tol, step.half = G$max.half, 
        rank.tol = control$rank.tol)
    for (iter in 1:(control$maxit)) {
        good <- weights > 0
        varmu <- variance(mu)[good]
        if (any(is.na(varmu))) 
            stop("NAs in V(mu)")
        if (any(varmu == 0)) 
            stop("0s in V(mu)")
        mu.eta.val <- mu.eta(eta)
        if (any(is.na(mu.eta.val[good]))) 
            stop("NAs in d(mu)/d(eta)")
        good <- (weights > 0) & (mu.eta.val != 0)
        if (all(!good)) {
            conv <- FALSE
            warning(gettextf("No observations informative at iteration %d", 
                iter))
            break
        }
        mevg <- mu.eta.val[good]
        mug <- mu[good]
        yg <- y[good]
        weg <- weights[good]
        var.mug <- variance(mug)
        G$y <- z <- (eta - offset)[good] + (yg - mug)/mevg
        w <- sqrt((weg * mevg^2)/var.mug)
        G$w <- w
        G$X <- X[good, , drop = FALSE]
        G$sig2 <- scale
        if (sum(!is.finite(G$y)) + sum(!is.finite(G$w)) > 0) 
            stop("iterative weights or data non-finite in gam.fit - regularization may help. See ?gam.control.")
        mr <- magic(G$y, G$X, msp, G$S, G$off, L = G$L, lsp0 = G$lsp0, 
            G$rank, G$H, matrix(0, 0, ncol(G$X)), G$w, gamma = gamma, 
            G$sig2, G$sig2 < 0, ridge.parameter = control$irls.reg, 
            control = magic.control, n.score = n.score, nthreads = control$nthreads)
        G$p <- mr$b
        msp <- mr$sp
        G$sig2 <- mr$scale
        G$gcv.ubre <- mr$score
        if (find.theta) {
            mv <- magic.post.proc(G$X, mr, w = G$w^2)
            G$edf <- mv$edf
            Theta <- mgcv.find.theta(Theta, T.max, T.min, weights, 
                good, mu, mu.eta.val, G, .Machine$double.eps^0.5)
            family <- do.call("negbin", list(theta = Theta, link = nb.link))
            variance <- family$variance
            dev.resids <- family$dev.resids
            aic <- family$aic
            family$Theta <- Theta
        }
        if (any(!is.finite(G$p))) {
            conv <- FALSE
            warning(gettextf("Non-finite coefficients at iteration %d", 
                iter))
            break
        }
        start <- G$p
        eta <- drop(X %*% start)
        mu <- linkinv(eta <- eta + offset)
        eta <- linkfun(mu)
        dev <- sum(dev.resids(y, mu, weights))
        if (control$trace) 
            message(gettextf("Deviance = %s Iterations - %d", 
                dev, iter, domain = "R-mgcv"))
        boundary <- FALSE
        if (!is.finite(dev)) {
            if (is.null(coefold)) 
                stop("no valid set of coefficients has been found:please supply starting values", 
                  call. = FALSE)
            warning("Step size truncated due to divergence", 
                call. = FALSE)
            ii <- 1
            while (!is.finite(dev)) {
                if (ii > control$maxit) 
                  stop("inner loop 1; can't correct step size")
                ii <- ii + 1
                start <- (start + coefold)/2
                eta <- drop(X %*% start)
                mu <- linkinv(eta <- eta + offset)
                eta <- linkfun(mu)
                dev <- sum(dev.resids(y, mu, weights))
            }
            boundary <- TRUE
            if (control$trace) 
                cat("Step halved: new deviance =", dev, "\n")
        }
        if (!(valideta(eta) && validmu(mu))) {
            warning("Step size truncated: out of bounds.", call. = FALSE)
            ii <- 1
            while (!(valideta(eta) && validmu(mu))) {
                if (ii > control$maxit) 
                  stop("inner loop 2; can't correct step size")
                ii <- ii + 1
                start <- (start + coefold)/2
                eta <- drop(X %*% start)
                mu <- linkinv(eta <- eta + offset)
                eta <- linkfun(mu)
            }
            boundary <- TRUE
            dev <- sum(dev.resids(y, mu, weights))
            if (control$trace) 
                cat("Step halved: new deviance =", dev, "\n")
        }
        if (abs(dev - devold)/(0.1 + abs(dev)) < control$epsilon || 
            olm || iter >= fixedSteps) {
            conv <- TRUE
            coef <- start
            break
        }
        else {
            devold <- dev
            coefold <- coef <- start
        }
    }
    if (!conv) {
        warning("Algorithm did not converge")
    }
    if (boundary) 
        warning("Algorithm stopped at boundary value")
    eps <- 10 * .Machine$double.eps
    if (family$family[1] == "binomial") {
        if (any(mu > 1 - eps) || any(mu < eps)) 
            warning("fitted probabilities numerically 0 or 1 occurred")
    }
    if (family$family[1] == "poisson") {
        if (any(mu < eps)) 
            warning("fitted rates numerically 0 occurred")
    }
    residuals <- rep(NA, nobs)
    residuals[good] <- z - (eta - offset)[good]
    wt <- rep(0, nobs)
    wt[good] <- w^2
    wtdmu <- if (intercept) 
        sum(weights * y)/sum(weights)
    else linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    n.ok <- nobs - sum(weights == 0)
    nulldf <- n.ok - as.integer(intercept)
    mv <- magic.post.proc(G$X, mr, w = G$w^2)
    G$Vp <- mv$Vb
    G$hat <- mv$hat
    G$Ve <- mv$Ve
    G$edf <- mv$edf
    G$conv <- mr$gcv.info
    G$sp <- msp
    rank <- G$conv$rank
    aic.model <- aic(y, n, mu, weights, dev) + 2 * sum(G$edf)
    if (scale < 0) {
        gcv.ubre.dev <- n.score * dev/(n.score - gamma * sum(G$edf))^2
    }
    else {
        gcv.ubre.dev <- dev/n.score + 2 * gamma * sum(G$edf)/n.score - 
            G$sig2
    }
    list(coefficients = as.vector(coef), residuals = residuals, 
        fitted.values = mu, family = family, linear.predictors = eta, 
        deviance = dev, null.deviance = nulldev, iter = iter, 
        weights = wt, prior.weights = weights, df.null = nulldf, 
        y = y, converged = conv, sig2 = G$sig2, edf = G$edf, 
        edf1 = mv$edf1, hat = G$hat, R = mr$R, boundary = boundary, 
        sp = G$sp, nsdf = G$nsdf, Ve = G$Ve, Vp = G$Vp, rV = mr$rV, 
        mgcv.conv = G$conv, gcv.ubre = G$gcv.ubre, aic = aic.model, 
        rank = rank, gcv.ubre.dev = gcv.ubre.dev, scale.estimated = (scale < 
            0))
}


residuals.gam <- function (object, type = "deviance", ...) 
{
    if (!is.null(object$family$residuals)) {
        res <- object$family$residuals(object, type, ...)
        res <- naresid(object$na.action, res)
        return(res)
    }
    type <- match.arg(type, c("deviance", "pearson", "scaled.pearson", 
        "working", "response"))
    y <- object$y
    mu <- object$fitted.values
    wts <- object$prior.weights
    if (type == "working") {
        res <- object$residuals
    }
    else if (type == "response") {
        res <- y - mu
    }
    else if (type == "deviance") {
        res <- object$family$dev.resids(y, mu, wts)
        s <- attr(res, "sign")
        if (is.null(s)) 
            s <- sign(y - mu)
        res <- sqrt(pmax(res, 0)) * s
    }
    else {
        var <- object$family$variance
        if (is.null(var)) {
            warning("Pearson residuals not available for this family - returning deviance residuals")
            return(residuals.gam(object))
        }
        res <- (y - mu) * sqrt(wts)/sqrt(var(mu))
        if (type == "scaled.pearson") 
            res <- res/sqrt(object$sig2)
    }
    res <- naresid(object$na.action, res)
    res
}


spasm.smooth <- function (object, X, residual = FALSE, block = 0) 
UseMethod("spasm.smooth")


spasm.construct <- function (object, data) 
UseMethod("spasm.construct")


magic <- function (y, X, sp, S, off, L = NULL, lsp0 = NULL, rank = NULL, 
    H = NULL, C = NULL, w = NULL, gamma = 1, scale = 1, gcv = TRUE, 
    ridge.parameter = NULL, control = list(tol = 1e-06, step.half = 25, 
        rank.tol = .Machine$double.eps^0.5), extra.rss = 0, n.score = length(y), 
    nthreads = 1) 
{
    if (is.null(control)) 
        control <- list()
    if (is.null(control$tol)) 
        control$tol <- 1e-06
    if (is.null(control$step.half)) 
        control$step.half <- 25
    if (is.null(control$rank.tol)) 
        control$rank.tol <- .Machine$double.eps^0.5
    n.p <- length(S)
    n.b <- dim(X)[2]
    if (n.p) 
        def.sp <- initial.sp(X, S, off)
    else def.sp <- sp
    if (!is.null(L)) {
        if (!inherits(L, "matrix")) 
            stop("L must be a matrix.")
        if (nrow(L) < ncol(L)) 
            stop("L must have at least as many rows as columns.")
        if (nrow(L) != n.p || ncol(L) != length(sp)) 
            stop("L has inconsistent dimensions.")
        if (is.null(lsp0)) 
            lsp0 <- rep(0, nrow(L))
        if (ncol(L)) 
            def.sp <- exp(as.numeric(coef(lm(log(def.sp) ~ L - 
                1 + offset(lsp0)))))
    }
    if (n.p > 0) {
        for (i in 1:n.p) {
            if (is.null(rank)) 
                B <- mroot(S[[i]], method = "svd")
            else B <- mroot(S[[i]], rank = rank[i], method = "chol")
            m <- dim(B)[2]
            R <- matrix(0, n.b, m)
            R[off[i]:(off[i] + dim(B)[1] - 1), ] <- B
            S[[i]] <- R
        }
        rm(B)
        rm(R)
    }
    if (!is.null(C)) {
        n.con <- dim(C)[1]
        ns.qr <- qr(t(C))
        X <- t(qr.qty(ns.qr, t(X)))[, (n.con + 1):n.b, drop = FALSE]
        if (n.p > 0) 
            for (i in 1:n.p) {
                S[[i]] <- qr.qty(ns.qr, S[[i]])[(n.con + 1):n.b, 
                  , drop = FALSE]
                if (ncol(S[[i]]) > nrow(S[[i]])) {
                  S[[i]] <- t(qr.R(qr(t(S[[i]]))))
                }
            }
        if (!is.null(H)) {
            H <- qr.qty(ns.qr, H)[(n.con + 1):n.b, , drop = FALSE]
            H <- t(qr.qty(ns.qr, t(H))[(n.con + 1):n.b, , drop = FALSE])
        }
        full.rank = n.b - n.con
    }
    else full.rank = n.b
    if (!is.null(w)) {
        if (is.matrix(w)) {
            if (dim(w)[1] != dim(w)[2] || dim(w)[2] != dim(X)[1]) 
                stop("dimensions of supplied w wrong.")
            y <- w %*% y
            X <- w %*% X
        }
        else {
            if (length(y) != length(w)) 
                stop("w different length from y!")
            y <- y * w
            X <- as.vector(w) * X
        }
    }
    if (is.null(dim(X))) {
        n <- length(y)
        if (n != length(X)) 
            stop("X lost dimensions in magic!!")
        dim(X) <- c(n, 1)
    }
    Si <- array(0, 0)
    cS <- 0
    if (n.p > 0) 
        for (i in 1:n.p) {
            Si <- c(Si, S[[i]])
            cS[i] <- dim(S[[i]])[2]
        }
    rdef <- ncol(X) - nrow(X)
    if (rdef > 0) {
        n.score <- n.score
        X <- rbind(X, matrix(0, rdef, ncol(X)))
        y <- c(y, rep(0, rdef))
    }
    icontrol <- as.integer(gcv)
    icontrol[2] <- length(y)
    q <- icontrol[3] <- dim(X)[2]
    if (!is.null(ridge.parameter) && ridge.parameter > 0) {
        if (is.null(H)) 
            H <- diag(ridge.parameter, q)
        else H <- H + diag(ridge.parameter, q)
    }
    icontrol[4] <- as.integer(!is.null(H))
    icontrol[5] <- n.p
    icontrol[6] <- control$step.half
    if (is.null(L)) {
        icontrol[7] <- -1
        L <- diag(n.p)
    }
    else icontrol[7] <- ncol(L)
    if (is.null(lsp0)) 
        lsp0 <- rep(0, nrow(L))
    b <- array(0, icontrol[3])
    if (nthreads < 1) 
        nthreads <- 1
    if (nthreads > 1) 
        extra.x <- q^2 * nthreads
    else extra.x <- 0
    um <- .C(C_magic, as.double(y), X = as.double(c(X, rep(0, 
        extra.x))), sp = as.double(sp), as.double(def.sp), as.double(Si), 
        as.double(H), as.double(L), lsp0 = as.double(lsp0), score = as.double(gamma), 
        scale = as.double(scale), info = as.integer(icontrol), 
        as.integer(cS), as.double(control$rank.tol), rms.grad = as.double(control$tol), 
        b = as.double(b), rV = double(q * q), as.double(extra.rss), 
        as.integer(n.score), as.integer(nthreads))
    res <- list(b = um$b, scale = um$scale, score = um$score, 
        sp = um$sp, sp.full = as.numeric(exp(L %*% log(um$sp))))
    res$R <- matrix(um$X[1:q^2], q, q)
    res$rV <- matrix(um$rV[1:(um$info[1] * q)], q, um$info[1])
    gcv.info <- list(full.rank = full.rank, rank = um$info[1], 
        fully.converged = as.logical(um$info[2]), hess.pos.def = as.logical(um$info[3]), 
        iter = um$info[4], score.calls = um$info[5], rms.grad = um$rms.grad)
    res$gcv.info <- gcv.info
    if (!is.null(C)) {
        b <- c(rep(0, n.con), res$b)
        res$b <- qr.qy(ns.qr, b)
        b <- matrix(0, n.b, dim(res$rV)[2])
        b[(n.con + 1):n.b, ] <- res$rV
        res$rV <- qr.qy(ns.qr, b)
    }
    res
}


gam.fit3 <- function (x, y, sp, Eb, UrS = list(), weights = rep(1, nobs), 
    start = NULL, etastart = NULL, mustart = NULL, offset = rep(0, 
        nobs), U1 = diag(ncol(x)), Mp = -1, family = gaussian(), 
    control = gam.control(), intercept = TRUE, deriv = 2, gamma = 1, 
    scale = 1, printWarn = TRUE, scoreType = "REML", null.coef = rep(0, 
        ncol(x)), pearson.extra = 0, dev.extra = 0, n.true = -1, 
    Sl = NULL, ...) 
{
    if (control$trace) {
        t0 <- proc.time()
        tc <- 0
    }
    if (inherits(family, "extended.family")) {
        if (inherits(family, "general.family")) {
            return(gam.fit5(x, y, sp, Sl = Sl, weights = weights, 
                offset = offset, deriv = deriv, family = family, 
                control = control, Mp = Mp, start = start))
        }
        else return(gam.fit4(x, y, sp, Eb, UrS = UrS, weights = weights, 
            start = start, etastart = etastart, mustart = mustart, 
            offset = offset, U1 = U1, Mp = Mp, family = family, 
            control = control, deriv = deriv, scale = scale, 
            scoreType = scoreType, null.coef = null.coef, ...))
    }
    if (family$link == family$canonical) 
        fisher <- TRUE
    else fisher = FALSE
    if (scale > 0) 
        scale.known <- TRUE
    else scale.known <- FALSE
    if (!scale.known && scoreType %in% c("REML", "ML")) {
        nsp <- length(sp)
        scale <- exp(sp[nsp])
        sp <- sp[-nsp]
    }
    if (!deriv %in% c(0, 1, 2)) 
        stop("unsupported order of differentiation requested of gam.fit3")
    x <- as.matrix(x)
    nSp <- length(sp)
    if (nSp == 0) 
        deriv.sp <- 0
    else deriv.sp <- deriv
    rank.tol <- .Machine$double.eps * 100
    xnames <- dimnames(x)[[2]]
    ynames <- if (is.matrix(y)) 
        rownames(y)
    else names(y)
    q <- ncol(x)
    if (length(UrS)) {
        grderiv <- deriv * as.numeric(scoreType %in% c("REML", 
            "ML", "P-REML", "P-ML"))
        rp <- gam.reparam(UrS, sp, grderiv)
        T <- diag(q)
        T[1:ncol(rp$Qs), 1:ncol(rp$Qs)] <- rp$Qs
        T <- U1 %*% T
        null.coef <- t(T) %*% null.coef
        if (!is.null(start)) 
            start <- t(T) %*% start
        x <- .Call(C_mgcv_pmmult2, x, T, 0, 0, control$nthreads)
        rS <- list()
        for (i in 1:length(UrS)) {
            rS[[i]] <- rbind(rp$rS[[i]], matrix(0, Mp, ncol(rp$rS[[i]])))
        }
        Eb <- Eb %*% T
        rows.E <- q - Mp
        Sr <- cbind(rp$E, matrix(0, nrow(rp$E), Mp))
        St <- rbind(cbind(rp$S, matrix(0, nrow(rp$S), Mp)), matrix(0, 
            Mp, q))
    }
    else {
        T <- diag(q)
        St <- matrix(0, q, q)
        rSncol <- sp <- rows.E <- Eb <- Sr <- 0
        rS <- list(0)
        rp <- list(det = 0, det1 = rep(0, 0), det2 = rep(0, 0), 
            fixed.penalty = FALSE)
    }
    iter <- 0
    coef <- rep(0, ncol(x))
    conv <- FALSE
    n <- nobs <- NROW(y)
    if (n.true <= 0) 
        n.true <- nobs
    nvars <- ncol(x)
    EMPTY <- nvars == 0
    if (is.null(weights)) 
        weights <- rep.int(1, nobs)
    if (is.null(offset)) 
        offset <- rep.int(0, nobs)
    variance <- family$variance
    dev.resids <- family$dev.resids
    aic <- family$aic
    linkinv <- family$linkinv
    mu.eta <- family$mu.eta
    if (!is.function(variance) || !is.function(linkinv)) 
        stop("illegal `family' argument")
    valideta <- family$valideta
    if (is.null(valideta)) 
        valideta <- function(eta) TRUE
    validmu <- family$validmu
    if (is.null(validmu)) 
        validmu <- function(mu) TRUE
    if (is.null(mustart)) {
        eval(family$initialize)
    }
    else {
        mukeep <- mustart
        eval(family$initialize)
        mustart <- mukeep
    }
    if (family$family == "gaussian" && family$link == "identity") 
        strictly.additive <- TRUE
    else strictly.additive <- FALSE
    D1 <- D2 <- P <- P1 <- P2 <- trA <- trA1 <- trA2 <- GCV <- GCV1 <- GCV2 <- GACV <- GACV1 <- GACV2 <- UBRE <- UBRE1 <- UBRE2 <- REML <- REML1 <- REML2 <- NULL
    if (EMPTY) {
        eta <- rep.int(0, nobs) + offset
        if (!valideta(eta)) 
            stop("Invalid linear predictor values in empty model")
        mu <- linkinv(eta)
        if (!validmu(mu)) 
            stop("Invalid fitted means in empty model")
        dev <- sum(dev.resids(y, mu, weights))
        w <- (weights * mu.eta(eta)^2)/variance(mu)
        residuals <- (y - mu)/mu.eta(eta)
        good <- rep(TRUE, length(residuals))
        boundary <- conv <- TRUE
        coef <- numeric(0)
        iter <- 0
        V <- variance(mu)
        alpha <- dev
        trA2 <- trA1 <- trA <- 0
        if (deriv) 
            GCV2 <- GCV1 <- UBRE2 <- UBRE1 <- trA1 <- rep(0, 
                nSp)
        GCV <- nobs * alpha/(nobs - gamma * trA)^2
        UBRE <- alpha/nobs - scale + 2 * gamma/n * trA
        scale.est <- alpha/(nobs - trA)
    }
    else {
        eta <- if (!is.null(etastart)) 
            etastart
        else if (!is.null(start)) 
            if (length(start) != nvars) 
                stop(gettextf("Length of start should equal %d and correspond to initial coefs for %s", 
                  nvars, deparse(xnames)))
            else {
                coefold <- start
                offset + as.vector(if (NCOL(x) == 1) 
                  x * start
                else x %*% start)
            }
        else family$linkfun(mustart)
        mu <- linkinv(eta)
        boundary <- conv <- FALSE
        rV = matrix(0, ncol(x), ncol(x))
        coefold <- null.coef
        etaold <- null.eta <- as.numeric(x %*% null.coef + as.numeric(offset))
        old.pdev <- sum(dev.resids(y, linkinv(null.eta), weights)) + 
            t(null.coef) %*% St %*% null.coef
        ii <- 0
        while (!(validmu(mu) && valideta(eta))) {
            ii <- ii + 1
            if (ii > 20) 
                stop("Can't find valid starting values: please specify some")
            if (!is.null(start)) 
                start <- start * 0.9 + coefold * 0.1
            eta <- 0.9 * eta + 0.1 * etaold
            mu <- linkinv(eta)
        }
        for (iter in 1:control$maxit) {
            good <- weights > 0
            var.val <- variance(mu)
            varmu <- var.val[good]
            if (any(is.na(varmu))) 
                stop("NAs in V(mu)")
            if (any(varmu == 0)) 
                stop("0s in V(mu)")
            mu.eta.val <- mu.eta(eta)
            if (any(is.na(mu.eta.val[good]))) 
                stop("NAs in d(mu)/d(eta)")
            good <- (weights > 0) & (mu.eta.val != 0)
            if (all(!good)) {
                conv <- FALSE
                warning(gettextf("No observations informative at iteration %d", 
                  iter))
                break
            }
            mevg <- mu.eta.val[good]
            mug <- mu[good]
            yg <- y[good]
            weg <- weights[good]
            var.mug <- var.val[good]
            if (fisher) {
                z <- (eta - offset)[good] + (yg - mug)/mevg
                w <- (weg * mevg^2)/var.mug
            }
            else {
                c = yg - mug
                alpha <- 1 + c * (family$dvar(mug)/var.mug + 
                  family$d2link(mug) * mevg)
                alpha[alpha == 0] <- .Machine$double.eps
                z <- (eta - offset)[good] + (yg - mug)/(mevg * 
                  alpha)
                w <- weg * alpha * mevg^2/var.mug
            }
            if (sum(good) < ncol(x)) 
                stop("Not enough informative observations.")
            if (control$trace) 
                t1 <- proc.time()
            oo <- .C(C_pls_fit1, y = as.double(z), X = as.double(x[good, 
                ]), w = as.double(w), wy = as.double(w * z), 
                E = as.double(Sr), Es = as.double(Eb), n = as.integer(sum(good)), 
                q = as.integer(ncol(x)), rE = as.integer(rows.E), 
                eta = as.double(z), penalty = as.double(1), rank.tol = as.double(rank.tol), 
                nt = as.integer(control$nthreads), use.wy = as.integer(0))
            if (control$trace) 
                tc <- tc + sum((proc.time() - t1)[c(1, 4)])
            if (!fisher && oo$n < 0) {
                z <- (eta - offset)[good] + (yg - mug)/mevg
                w <- (weg * mevg^2)/var.mug
                if (control$trace) 
                  t1 <- proc.time()
                oo <- .C(C_pls_fit1, y = as.double(z), X = as.double(x[good, 
                  ]), w = as.double(w), wy = as.double(w * z), 
                  E = as.double(Sr), Es = as.double(Eb), n = as.integer(sum(good)), 
                  q = as.integer(ncol(x)), rE = as.integer(rows.E), 
                  eta = as.double(z), penalty = as.double(1), 
                  rank.tol = as.double(rank.tol), nt = as.integer(control$nthreads), 
                  use.wy = as.integer(0))
                if (control$trace) 
                  tc <- tc + sum((proc.time() - t1)[c(1, 4)])
            }
            start <- oo$y[1:ncol(x)]
            penalty <- oo$penalty
            eta <- drop(x %*% start)
            if (any(!is.finite(start))) {
                conv <- FALSE
                warning(gettextf("Non-finite coefficients at iteration %d", 
                  iter))
                break
            }
            mu <- linkinv(eta <- eta + offset)
            dev <- sum(dev.resids(y, mu, weights))
            if (control$trace) 
                message(gettextf("Deviance = %s Iterations - %d", 
                  dev, iter, domain = "R-mgcv"))
            boundary <- FALSE
            if (!is.finite(dev)) {
                if (is.null(coefold)) {
                  if (is.null(null.coef)) 
                    stop("no valid set of coefficients has been found:please supply starting values", 
                      call. = FALSE)
                  coefold <- null.coef
                  etaold <- null.eta
                }
                warning("Step size truncated due to divergence", 
                  call. = FALSE)
                ii <- 1
                while (!is.finite(dev)) {
                  if (ii > control$maxit) 
                    stop("inner loop 1; can't correct step size")
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- (eta + etaold)/2
                  mu <- linkinv(eta)
                  dev <- sum(dev.resids(y, mu, weights))
                }
                boundary <- TRUE
                penalty <- t(start) %*% St %*% start
                if (control$trace) 
                  cat("Step halved: new deviance =", dev, "\n")
            }
            if (!(valideta(eta) && validmu(mu))) {
                warning("Step size truncated: out of bounds", 
                  call. = FALSE)
                ii <- 1
                while (!(valideta(eta) && validmu(mu))) {
                  if (ii > control$maxit) 
                    stop("inner loop 2; can't correct step size")
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- (eta + etaold)/2
                  mu <- linkinv(eta)
                }
                boundary <- TRUE
                penalty <- t(start) %*% St %*% start
                dev <- sum(dev.resids(y, mu, weights))
                if (control$trace) 
                  cat("Step halved: new deviance =", dev, "\n")
            }
            pdev <- dev + penalty
            if (control$trace) 
                message(gettextf("penalized deviance = %s", pdev, 
                  domain = "R-mgcv"))
            div.thresh <- 10 * (0.1 + abs(old.pdev)) * .Machine$double.eps^0.5
            if (pdev - old.pdev > div.thresh) {
                ii <- 1
                if (iter == 1) {
                  etaold <- null.eta
                  coefold <- null.coef
                }
                while (pdev - old.pdev > div.thresh) {
                  if (ii > 100) 
                    stop("inner loop 3; can't correct step size")
                  ii <- ii + 1
                  start <- (start + coefold)/2
                  eta <- (eta + etaold)/2
                  mu <- linkinv(eta)
                  dev <- sum(dev.resids(y, mu, weights))
                  pdev <- dev + t(start) %*% St %*% start
                  if (control$trace) 
                    message(gettextf("Step halved: new penalized deviance = %g", 
                      pdev, "\n"))
                }
            }
            if (strictly.additive) {
                conv <- TRUE
                coef <- start
                break
            }
            if (abs(pdev - old.pdev)/(0.1 + abs(pdev)) < control$epsilon) {
                grad <- 2 * t(x[good, ]) %*% (w * ((x %*% start)[good] - 
                  z)) + 2 * St %*% start
                if (max(abs(grad)) > control$epsilon * max(abs(start + 
                  coefold))/2) {
                  old.pdev <- pdev
                  coef <- coefold <- start
                  etaold <- eta
                }
                else {
                  conv <- TRUE
                  coef <- start
                  etaold <- eta
                  break
                }
            }
            else {
                old.pdev <- pdev
                coef <- coefold <- start
                etaold <- eta
            }
        }
        wdr <- dev.resids(y, mu, weights)
        dev <- sum(wdr)
        wdr <- sign(y - mu) * sqrt(pmax(wdr, 0))
        good <- weights > 0
        var.val <- variance(mu)
        varmu <- var.val[good]
        if (any(is.na(varmu))) 
            stop("NAs in V(mu)")
        if (any(varmu == 0)) 
            stop("0s in V(mu)")
        mu.eta.val <- mu.eta(eta)
        if (any(is.na(mu.eta.val[good]))) 
            stop("NAs in d(mu)/d(eta)")
        good <- (weights > 0) & (mu.eta.val != 0)
        mevg <- mu.eta.val[good]
        mug <- mu[good]
        yg <- y[good]
        weg <- weights[good]
        etag <- eta[good]
        var.mug <- var.val[good]
        if (fisher) {
            z <- (eta - offset)[good] + (yg - mug)/mevg
            w <- (weg * mevg^2)/var.mug
            alpha <- wf <- 0
        }
        else {
            c <- yg - mug
            alpha <- 1 + c * (family$dvar(mug)/var.mug + family$d2link(mug) * 
                mevg)
            alpha[alpha == 0] <- .Machine$double.eps
            z <- (eta - offset)[good] + (yg - mug)/(mevg * alpha)
            wf <- weg * mevg^2/var.mug
            w <- wf * alpha
        }
        g1 <- 1/mevg
        g2 <- family$d2link(mug)
        g3 <- family$d3link(mug)
        V <- family$variance(mug)
        V1 <- family$dvar(mug)
        V2 <- family$d2var(mug)
        if (fisher) {
            g4 <- V3 <- 0
        }
        else {
            g4 <- family$d4link(mug)
            V3 <- family$d3var(mug)
        }
        if (TRUE) {
            g2 <- g2/g1
            g3 <- g3/g1
            g4 <- g4/g1
            V1 <- V1/V
            V2 <- V2/V
            V3 <- V3/V
        }
        P1 <- D1 <- array(0, nSp)
        P2 <- D2 <- matrix(0, nSp, nSp)
        trA1 <- array(0, nSp)
        trA2 <- matrix(0, nSp, nSp)
        rV = matrix(0, ncol(x), ncol(x))
        dum <- 1
        if (control$trace) 
            cat("calling gdi...")
        REML <- 0
        if (scoreType %in% c("REML", "P-REML")) {
            REML <- 1
            remlInd <- 1
        }
        else if (scoreType %in% c("ML", "P-ML")) {
            REML <- -1
            remlInd <- 0
        }
        if (REML == 0) 
            rSncol <- unlist(lapply(rS, ncol))
        else rSncol <- unlist(lapply(UrS, ncol))
        if (control$trace) 
            t1 <- proc.time()
        oo <- .C(C_gdi1, X = as.double(x[good, ]), E = as.double(Sr), 
            Eb = as.double(Eb), rS = as.double(unlist(rS)), U1 = as.double(U1), 
            sp = as.double(exp(sp)), z = as.double(z), w = as.double(w), 
            wf = as.double(wf), alpha = as.double(alpha), mu = as.double(mug), 
            eta = as.double(etag), y = as.double(yg), p.weights = as.double(weg), 
            g1 = as.double(g1), g2 = as.double(g2), g3 = as.double(g3), 
            g4 = as.double(g4), V0 = as.double(V), V1 = as.double(V1), 
            V2 = as.double(V2), V3 = as.double(V3), beta = as.double(coef), 
            b1 = as.double(rep(0, nSp * ncol(x))), w1 = as.double(rep(0, 
                nSp * length(z))), D1 = as.double(D1), D2 = as.double(D2), 
            P = as.double(dum), P1 = as.double(P1), P2 = as.double(P2), 
            trA = as.double(dum), trA1 = as.double(trA1), trA2 = as.double(trA2), 
            rV = as.double(rV), rank.tol = as.double(rank.tol), 
            conv.tol = as.double(control$epsilon), rank.est = as.integer(1), 
            n = as.integer(length(z)), p = as.integer(ncol(x)), 
            M = as.integer(nSp), Mp = as.integer(Mp), Enrow = as.integer(rows.E), 
            rSncol = as.integer(rSncol), deriv = as.integer(deriv.sp), 
            REML = as.integer(REML), fisher = as.integer(fisher), 
            fixed.penalty = as.integer(rp$fixed.penalty), nthreads = as.integer(control$nthreads), 
            dVkk = as.double(rep(0, nSp * nSp)))
        if (control$trace) {
            tg <- sum((proc.time() - t1)[c(1, 4)])
            cat("done!\n")
        }
        db.drho <- if (deriv) 
            T %*% matrix(oo$b1, ncol(x), nSp)
        else NULL
        dw.drho <- if (deriv) 
            matrix(oo$w1, length(z), nSp)
        else NULL
        rV <- matrix(oo$rV, ncol(x), ncol(x))
        Kmat <- matrix(0, nrow(x), ncol(x))
        Kmat[good, ] <- oo$X
        coef <- oo$beta
        eta <- drop(x %*% coef + offset)
        mu <- linkinv(eta)
        if (!(validmu(mu) && valideta(eta))) {
            coef <- start
            eta <- etaold
            mu <- linkinv(eta)
        }
        trA <- oo$trA
        if (control$scale.est %in% c("pearson", "fletcher", "Pearson", 
            "Fletcher")) {
            pearson <- sum(weights * (y - mu)^2/family$variance(mu))
            scale.est <- (pearson + dev.extra)/(n.true - trA)
            if (control$scale.est %in% c("fletcher", "Fletcher")) {
                s.bar = mean(family$dvar(mu) * (y - mu) * sqrt(weights)/family$variance(mu))
                if (is.finite(s.bar)) 
                  scale.est <- scale.est/(1 + s.bar)
            }
        }
        else {
            scale.est <- (dev + dev.extra)/(n.true - trA)
        }
        reml.scale <- NA
        if (scoreType %in% c("REML", "ML")) {
            ls <- family$ls(y, weights, n, scale) * n.true/nobs
            Dp <- dev + oo$conv.tol + dev.extra
            REML <- Dp/(2 * scale) - ls[1] + oo$rank.tol/2 - 
                rp$det/2 - remlInd * Mp/2 * log(2 * pi * scale)
            attr(REML, "Dp") <- Dp/(2 * scale)
            if (deriv) {
                REML1 <- oo$D1/(2 * scale) + oo$trA1/2 - rp$det1/2
                if (deriv == 2) 
                  REML2 <- (matrix(oo$D2, nSp, nSp)/scale + matrix(oo$trA2, 
                    nSp, nSp) - rp$det2)/2
                if (sum(!is.finite(REML2))) {
                  stop("Non finite derivatives. Try decreasing fit tolerance! See `epsilon' in `gam.contol'")
                }
            }
            if (!scale.known && deriv) {
                dlr.dlphi <- -Dp/(2 * scale) - ls[2] * scale - 
                  Mp/2 * remlInd
                d2lr.d2lphi <- Dp/(2 * scale) - ls[3] * scale^2 - 
                  ls[2] * scale
                d2lr.dspphi <- -oo$D1/(2 * scale)
                REML1 <- c(REML1, dlr.dlphi)
                if (deriv == 2) {
                  REML2 <- rbind(REML2, as.numeric(d2lr.dspphi))
                  REML2 <- cbind(REML2, c(as.numeric(d2lr.dspphi), 
                    d2lr.d2lphi))
                }
            }
            reml.scale <- scale
        }
        else if (scoreType %in% c("P-REML", "P-ML")) {
            reml.scale <- phi <- (oo$P * (nobs - Mp) + pearson.extra)/(n.true - 
                Mp)
            oo$P1 <- oo$P1 * (nobs - Mp)/(n.true - Mp)
            oo$P2 <- oo$P2 * (nobs - Mp)/(n.true - Mp)
            ls <- family$ls(y, weights, n, phi) * n.true/nobs
            Dp <- dev + oo$conv.tol + dev.extra
            K <- oo$rank.tol/2 - rp$det/2
            REML <- Dp/(2 * phi) - ls[1] + K - Mp/2 * log(2 * 
                pi * phi) * remlInd
            attr(REML, "Dp") <- Dp/(2 * phi)
            if (deriv) {
                phi1 <- oo$P1
                Dp1 <- oo$D1
                K1 <- oo$trA1/2 - rp$det1/2
                REML1 <- Dp1/(2 * phi) - phi1 * (Dp/(2 * phi^2) + 
                  Mp/(2 * phi) * remlInd + ls[2]) + K1
                if (deriv == 2) {
                  phi2 <- matrix(oo$P2, nSp, nSp)
                  Dp2 <- matrix(oo$D2, nSp, nSp)
                  K2 <- matrix(oo$trA2, nSp, nSp)/2 - rp$det2/2
                  REML2 <- Dp2/(2 * phi) - (outer(Dp1, phi1) + 
                    outer(phi1, Dp1))/(2 * phi^2) + (Dp/phi^3 - 
                    ls[3] + Mp/(2 * phi^2) * remlInd) * outer(phi1, 
                    phi1) - (Dp/(2 * phi^2) + ls[2] + Mp/(2 * 
                    phi) * remlInd) * phi2 + K2
                }
            }
        }
        else {
            P <- oo$P
            delta <- nobs - gamma * trA
            delta.2 <- delta * delta
            GCV <- nobs * dev/delta.2
            GACV <- dev/nobs + P * 2 * gamma * trA/(delta * nobs)
            UBRE <- dev/nobs - 2 * delta * scale/nobs + scale
            if (deriv) {
                trA1 <- oo$trA1
                D1 <- oo$D1
                P1 <- oo$P1
                if (sum(!is.finite(D1)) || sum(!is.finite(P1)) || 
                  sum(!is.finite(trA1))) {
                  stop("Non-finite derivatives. Try decreasing fit tolerance! See `epsilon' in `gam.contol'")
                }
                delta.3 <- delta * delta.2
                GCV1 <- nobs * D1/delta.2 + 2 * nobs * dev * 
                  trA1 * gamma/delta.3
                GACV1 <- D1/nobs + 2 * P/delta.2 * trA1 + 2 * 
                  gamma * trA * P1/(delta * nobs)
                UBRE1 <- D1/nobs + gamma * trA1 * 2 * scale/nobs
                if (deriv == 2) {
                  trA2 <- matrix(oo$trA2, nSp, nSp)
                  D2 <- matrix(oo$D2, nSp, nSp)
                  P2 <- matrix(oo$P2, nSp, nSp)
                  if (sum(!is.finite(D2)) || sum(!is.finite(P2)) || 
                    sum(!is.finite(trA2))) {
                    stop("Non-finite derivatives. Try decreasing fit tolerance! See `epsilon' in `gam.contol'")
                  }
                  GCV2 <- outer(trA1, D1)
                  GCV2 <- (GCV2 + t(GCV2)) * gamma * 2 * nobs/delta.3 + 
                    6 * nobs * dev * outer(trA1, trA1) * gamma * 
                      gamma/(delta.2 * delta.2) + nobs * D2/delta.2 + 
                    2 * nobs * dev * gamma * trA2/delta.3
                  GACV2 <- D2/nobs + outer(trA1, trA1) * 4 * 
                    P/(delta.3) + 2 * P * trA2/delta.2 + 2 * 
                    outer(trA1, P1)/delta.2 + 2 * outer(P1, trA1) * 
                    (1/(delta * nobs) + trA/(nobs * delta.2)) + 
                    2 * trA * P2/(delta * nobs)
                  GACV2 <- (GACV2 + t(GACV2)) * 0.5
                  UBRE2 <- D2/nobs + 2 * gamma * trA2 * scale/nobs
                }
            }
        }
        if (!conv && printWarn) 
            warning("Algorithm did not converge")
        if (printWarn && boundary) 
            warning("Algorithm stopped at boundary value")
        eps <- 10 * .Machine$double.eps
        if (printWarn && family$family[1] == "binomial") {
            if (any(mu > 1 - eps) || any(mu < eps)) 
                warning("fitted probabilities numerically 0 or 1 occurred")
        }
        if (printWarn && family$family[1] == "poisson") {
            if (any(mu < eps)) 
                warning("fitted rates numerically 0 occurred")
        }
        residuals <- rep.int(NA, nobs)
        residuals[good] <- z - (eta - offset)[good]
        coef <- as.numeric(T %*% coef)
        rV <- T %*% rV
        names(coef) <- xnames
    }
    names(residuals) <- ynames
    names(mu) <- ynames
    names(eta) <- ynames
    ww <- wt <- rep.int(0, nobs)
    if (fisher) {
        wt[good] <- w
        ww <- wt
    }
    else {
        wt[good] <- wf
        ww[good] <- w
    }
    names(wt) <- ynames
    names(weights) <- ynames
    names(y) <- ynames
    if (deriv && nrow(dw.drho) != nrow(x)) {
        w1 <- dw.drho
        dw.drho <- matrix(0, nrow(x), ncol(w1))
        dw.drho[good, ] <- w1
    }
    wtdmu <- if (intercept) 
        sum(weights * y)/sum(weights)
    else linkinv(offset)
    nulldev <- sum(dev.resids(y, wtdmu, weights))
    n.ok <- nobs - sum(weights == 0)
    nulldf <- n.ok - as.integer(intercept)
    aic.model <- aic(y, n, mu, weights, dev)
    if (control$trace) {
        t1 <- proc.time()
        at <- sum((t1 - t0)[c(1, 4)])
        cat("Proportion time in C: ", (tc + tg)/at, " ls:", tc/at, 
            " gdi:", tg/at, "\n")
    }
    list(coefficients = coef, residuals = residuals, fitted.values = mu, 
        family = family, linear.predictors = eta, deviance = dev, 
        null.deviance = nulldev, iter = iter, weights = wt, working.weights = ww, 
        prior.weights = weights, df.null = nulldf, y = y, converged = conv, 
        boundary = boundary, D1 = D1, D2 = D2, P = P, P1 = P1, 
        P2 = P2, trA = trA, trA1 = trA1, trA2 = trA2, GCV = GCV, 
        GCV1 = GCV1, GCV2 = GCV2, GACV = GACV, GACV1 = GACV1, 
        GACV2 = GACV2, UBRE = UBRE, UBRE1 = UBRE1, UBRE2 = UBRE2, 
        REML = REML, REML1 = REML1, REML2 = REML2, rV = rV, db.drho = db.drho, 
        dw.drho = dw.drho, dVkk = matrix(oo$dVkk, nSp, nSp), 
        scale.est = scale.est, reml.scale = reml.scale, aic = aic.model, 
        rank = oo$rank.est, K = Kmat)
}


Predict.matrix.Bspline.smooth <- function (object, data) 
{
    object$mono <- 0
    object$m <- object$m - 1
    Predict.matrix.pspline.smooth(object, data)
}


predict.gam <- function (object, newdata, type = "link", se.fit = FALSE, terms = NULL, 
    exclude = NULL, block.size = NULL, newdata.guaranteed = FALSE, 
    na.action = na.pass, unconditional = FALSE, ...) 
{
    if (unconditional) {
        if (is.null(object$Vc)) 
            warning("Smoothness uncertainty corrected covariance not available")
        else object$Vp <- object$Vc
    }
    if (type != "link" && type != "terms" && type != "iterms" && 
        type != "response" && type != "lpmatrix" && type != "newdata") {
        warning("Unknown type, reset to terms.")
        type <- "terms"
    }
    if (!inherits(object, "gam")) 
        stop("predict.gam can only be used to predict from gam objects")
    if (missing(newdata)) 
        na.act <- object$na.action
    else {
        if (is.null(na.action)) 
            na.act <- NULL
        else {
            na.txt <- "na.pass"
            if (is.character(na.action)) 
                na.txt <- substitute(na.action)
            else if (is.function(na.action)) 
                na.txt <- deparse(substitute(na.action))
            if (na.txt == "na.pass") 
                na.act <- "na.exclude"
            else if (na.txt == "na.exclude") 
                na.act <- "na.omit"
            else na.act <- na.action
        }
    }
    nd.is.mf <- FALSE
    yname <- all.vars(object$terms)[attr(object$terms, "response")]
    if (newdata.guaranteed == FALSE) {
        if (missing(newdata)) {
            newdata <- object$model
            new.data.ok <- FALSE
            nd.is.mf <- TRUE
            response <- newdata[[yname]]
        }
        else {
            new.data.ok <- TRUE
            if (is.data.frame(newdata) && !is.null(attr(newdata, 
                "terms"))) {
                if (sum(!(names(object$model) %in% names(newdata)))) 
                  stop("newdata is a model.frame: it should contain all required variables\n")
                nd.is.mf <- TRUE
            }
            else {
                yname <- all.vars(object$terms)[attr(object$terms, 
                  "response")]
                naresp <- FALSE
                if (!is.null(object$family$predict) && !is.null(newdata[[yname]])) {
                  if (!is.null(object$pred.formula)) 
                    object$pred.formula <- attr(object$pred.formula, 
                      "full")
                  response <- TRUE
                  Terms <- terms(object)
                  resp <- newdata[[yname]]
                  if (sum(is.na(resp)) > 0) {
                    naresp <- TRUE
                    rar <- range(resp, na.rm = TRUE)
                    thresh <- rar[1] * 1.01 - rar[2] * 0.01
                    resp[is.na(resp)] <- thresh
                    newdata[[yname]] <- thresh
                  }
                }
                else {
                  response <- FALSE
                  Terms <- delete.response(terms(object))
                }
                allNames <- if (is.null(object$pred.formula)) 
                  all.vars(Terms)
                else all.vars(object$pred.formula)
                if (length(allNames) > 0) {
                  ff <- if (is.null(object$pred.formula)) 
                    reformulate(allNames)
                  else object$pred.formula
                  if (sum(!(allNames %in% names(newdata)))) {
                    warning("not all required variables have been supplied in  newdata!\n")
                  }
                  newdata <- eval(model.frame(ff, data = newdata, 
                    na.action = na.act), parent.frame())
                  if (naresp) 
                    newdata[[yname]][newdata[[yname]] <= thresh] <- NA
                }
                na.act <- attr(newdata, "na.action")
                response <- if (response) 
                  newdata[[yname]]
                else NULL
            }
        }
    }
    else {
        na.act <- NULL
        new.data.ok = TRUE
        if (!is.null(attr(newdata, "terms"))) 
            nd.is.mf <- TRUE
        response <- newdata[[yname]]
    }
    if (new.data.ok) {
        nn <- names(newdata)
        mn <- colnames(object$model)
        for (i in 1:length(newdata)) if (nn[i] %in% mn && is.factor(object$model[, 
            nn[i]])) {
            levm <- levels(object$model[, nn[i]])
            levn <- levels(factor(newdata[[i]]))
            if (sum(!levn %in% levm) > 0) {
                msg <- paste(paste(levn[!levn %in% levm], collapse = ", "), 
                  "not in original fit", collapse = "")
                stop(msg)
            }
            newdata[[i]] <- factor(newdata[[i]], levels = levm)
        }
        if (type == "newdata") 
            return(newdata)
        if (length(newdata) == 1) 
            newdata[[2]] <- newdata[[1]]
        if (is.null(dim(newdata[[1]]))) 
            np <- length(newdata[[1]])
        else np <- dim(newdata[[1]])[1]
        nb <- length(object$coefficients)
        if (is.null(block.size)) 
            block.size <- 1000
        if (block.size < 1) 
            block.size <- np
    }
    else {
        np <- nrow(object$model)
        nb <- length(object$coefficients)
    }
    if (is.null(block.size)) {
        n.blocks <- 1
        b.size <- array(np, 1)
    }
    else {
        n.blocks <- np%/%block.size
        b.size <- rep(block.size, n.blocks)
        last.block <- np - sum(b.size)
        if (last.block > 0) {
            n.blocks <- n.blocks + 1
            b.size[n.blocks] <- last.block
        }
    }
    lpi <- if (is.list(object$formula)) 
        attr(object$formula, "lpi")
    else NULL
    n.smooth <- length(object$smooth)
    if (type == "lpmatrix") {
        H <- matrix(0, np, nb)
    }
    else if (type == "terms" || type == "iterms") {
        term.labels <- attr(object$pterms, "term.labels")
        if (is.null(attr(object, "para.only"))) 
            para.only <- FALSE
        else para.only <- TRUE
        n.pterms <- length(term.labels)
        fit <- array(0, c(np, n.pterms + as.numeric(!para.only) * 
            n.smooth))
        if (se.fit) 
            se <- fit
        ColNames <- term.labels
    }
    else {
        if (is.list(object$formula)) {
            nlp <- length(lpi)
        }
        else nlp <- 1
        fit <- if (nlp > 1) 
            matrix(0, np, nlp)
        else array(0, np)
        if (se.fit) 
            se <- fit
        fit1 <- NULL
    }
    stop <- 0
    if (is.list(object$pterms)) {
        if (type == "iterms") {
            warning("type iterms not available for multiple predictor cases")
            type <- "terms"
        }
        pstart <- attr(object$nsdf, "pstart")
        pind <- rep(0, 0)
        Terms <- list()
        pterms <- object$pterms
        for (i in 1:length(object$nsdf)) {
            Terms[[i]] <- delete.response(object$pterms[[i]])
            if (object$nsdf[i] > 0) 
                pind <- c(pind, pstart[i] - 1 + 1:object$nsdf[i])
        }
    }
    else {
        Terms <- list(delete.response(object$pterms))
        pterms <- list(object$pterms)
        pstart <- 1
        pind <- 1:object$nsdf
    }
    drop.intercept <- FALSE
    if (!is.null(object$family$drop.intercept) && object$family$drop.intercept) {
        drop.intercept <- TRUE
        for (i in 1:length(Terms)) attr(Terms[[i]], "intercept") <- 1
    }
    drop.ind <- attr(object$nsdf, "drop.ind")
    s.offset <- NULL
    any.soff <- FALSE
    if (n.blocks > 0) 
        for (b in 1:n.blocks) {
            start <- stop + 1
            stop <- start + b.size[b] - 1
            if (n.blocks == 1) 
                data <- newdata
            else data <- newdata[start:stop, ]
            X <- matrix(0, b.size[b], nb + length(drop.ind))
            Xoff <- matrix(0, b.size[b], n.smooth)
            for (i in 1:length(Terms)) {
                if (new.data.ok) {
                  if (nd.is.mf) 
                    mf <- model.frame(data, xlev = object$xlevels)
                  else {
                    mf <- model.frame(Terms[[i]], data, xlev = object$xlevels)
                    if (!is.null(cl <- attr(pterms[[i]], "dataClasses"))) 
                      .checkMFClasses(cl, mf)
                  }
                  Xp <- model.matrix(Terms[[i]], mf, contrasts = object$contrasts)
                }
                else {
                  Xp <- model.matrix(Terms[[i]], object$model)
                  mf <- newdata
                }
                if (drop.intercept) {
                  xat <- attributes(Xp)
                  ind <- xat$assign > 0
                  Xp <- Xp[, xat$assign > 0, drop = FALSE]
                  xat$assign <- xat$assign[ind]
                  xat$dimnames[[2]] <- xat$dimnames[[2]][ind]
                  xat$dim[2] <- xat$dim[2] - 1
                  attributes(Xp) <- xat
                }
                if (object$nsdf[i] > 0) 
                  X[, pstart[i] - 1 + 1:object$nsdf[i]] <- Xp
            }
            if (!is.null(drop.ind)) 
                X <- X[, -drop.ind]
            if (n.smooth) 
                for (k in 1:n.smooth) {
                  klab <- object$smooth[[k]]$label
                  if ((is.null(terms) || (klab %in% terms)) && 
                    (is.null(exclude) || !(klab %in% exclude))) {
                    Xfrag <- PredictMat(object$smooth[[k]], data)
                    X[, object$smooth[[k]]$first.para:object$smooth[[k]]$last.para] <- Xfrag
                    Xfrag.off <- attr(Xfrag, "offset")
                    if (!is.null(Xfrag.off)) {
                      Xoff[, k] <- Xfrag.off
                      any.soff <- TRUE
                    }
                  }
                  if (type == "terms" || type == "iterms") 
                    ColNames[n.pterms + k] <- klab
                }
            if (!is.null(object$Xcentre)) {
                X <- sweep(X, 2, object$Xcentre)
            }
            if (type == "lpmatrix") {
                H[start:stop, ] <- X
                if (any.soff) 
                  s.offset <- rbind(s.offset, Xoff)
            }
            else if (type == "terms" || type == "iterms") {
                lass <- if (is.list(object$assign)) 
                  object$assign
                else list(object$assign)
                k <- 0
                for (j in 1:length(lass)) if (length(lass[[j]])) {
                  ind <- 1:length(lass[[j]])
                  nptj <- max(lass[[j]])
                  if (nptj > 0) 
                    for (i in 1:nptj) {
                      k <- k + 1
                      ii <- ind[lass[[j]] == i] + pstart[j] - 
                        1
                      fit[start:stop, k] <- X[, ii, drop = FALSE] %*% 
                        object$coefficients[ii]
                      if (se.fit) 
                        se[start:stop, k] <- sqrt(pmax(0, rowSums((X[, 
                          ii, drop = FALSE] %*% object$Vp[ii, 
                          ii]) * X[, ii, drop = FALSE])))
                    }
                }
                if (n.smooth && !para.only) {
                  for (k in 1:n.smooth) {
                    first <- object$smooth[[k]]$first.para
                    last <- object$smooth[[k]]$last.para
                    fit[start:stop, n.pterms + k] <- X[, first:last, 
                      drop = FALSE] %*% object$coefficients[first:last] + 
                      Xoff[, k]
                    if (se.fit) {
                      if (type == "iterms" && attr(object$smooth[[k]], 
                        "nCons") > 0) {
                        if (length(object$cmX) < ncol(X)) 
                          object$cmX <- c(object$cmX, rep(0, 
                            ncol(X) - length(object$cmX)))
                        X1 <- matrix(object$cmX, nrow(X), ncol(X), 
                          byrow = TRUE)
                        meanL1 <- object$smooth[[k]]$meanL1
                        if (!is.null(meanL1)) 
                          X1 <- X1/meanL1
                        X1[, first:last] <- X[, first:last]
                        se[start:stop, n.pterms + k] <- sqrt(pmax(0, 
                          rowSums((X1 %*% object$Vp) * X1)))
                      }
                      else se[start:stop, n.pterms + k] <- sqrt(pmax(0, 
                        rowSums((X[, first:last, drop = FALSE] %*% 
                          object$Vp[first:last, first:last, drop = FALSE]) * 
                          X[, first:last, drop = FALSE])))
                    }
                  }
                  colnames(fit) <- ColNames
                  if (se.fit) 
                    colnames(se) <- ColNames
                }
                else {
                  if (para.only && is.list(object$pterms)) {
                    term.labels <- unlist(lapply(object$pterms, 
                      attr, "term.labels"))
                  }
                  colnames(fit) <- term.labels
                  if (se.fit) 
                    colnames(se) <- term.labels
                  order <- if (is.list(object$pterms)) 
                    unlist(lapply(object$pterms, attr, "order"))
                  else attr(object$pterms, "order")
                  term.labels <- term.labels[order == 1]
                  fit <- fit[, order == 1, drop = FALSE]
                  colnames(fit) <- term.labels
                  if (se.fit) {
                    se <- se[, order == 1, drop = FALSE]
                    colnames(se) <- term.labels
                  }
                }
            }
            else {
                fam <- object$family
                k <- attr(attr(object$model, "terms"), "offset")
                if (nlp > 1) {
                  if (is.null(fam$predict) || type == "link") {
                    off.ind <- (1:n.smooth)[as.logical(colSums(abs(Xoff)))]
                    for (j in 1:nlp) {
                      ind <- lpi[[j]]
                      fit[start:stop, j] <- X[, ind, drop = FALSE] %*% 
                        object$coefficients[ind]
                      if (length(off.ind)) 
                        for (i in off.ind) {
                          if (object$smooth[[i]]$first.para %in% 
                            ind) 
                            fit[start:stop, j] <- fit[start:stop, 
                              j] + Xoff[, i]
                        }
                      if (se.fit) 
                        se[start:stop, j] <- sqrt(pmax(0, rowSums((X[, 
                          ind, drop = FALSE] %*% object$Vp[ind, 
                          ind, drop = FALSE]) * X[, ind, drop = FALSE])))
                      if (j == 1 && !is.null(k)) 
                        fit[start:stop, j] <- fit[start:stop, 
                          j] + model.offset(mf)
                      if (type == "response") {
                        linfo <- object$family$linfo[[j]]
                        if (se.fit) 
                          se[start:stop, j] <- se[start:stop, 
                            j] * abs(linfo$mu.eta(fit[start:stop, 
                            j]))
                        fit[start:stop, j] <- linfo$linkinv(fit[start:stop, 
                          j])
                      }
                    }
                  }
                  else {
                    attr(X, "lpi") <- lpi
                    ffv <- fam$predict(fam, se.fit, y = response, 
                      X = X, beta = object$coefficients, off = offs, 
                      Vb = object$Vp)
                    if (is.matrix(fit) && !is.matrix(ffv[[1]])) {
                      fit <- fit[, 1]
                      if (se.fit) 
                        se <- se[, 1]
                    }
                    if (is.matrix(ffv[[1]]) && (!is.matrix(fit) || 
                      ncol(ffv[[1]]) != ncol(fit))) {
                      fit <- matrix(0, np, ncol(ffv[[1]]))
                      if (se.fit) 
                        se <- fit
                    }
                    if (is.matrix(fit)) {
                      fit[start:stop, ] <- ffv[[1]]
                      if (se.fit) 
                        se[start:stop, ] <- ffv[[2]]
                    }
                    else {
                      fit[start:stop] <- ffv[[1]]
                      if (se.fit) 
                        se[start:stop] <- ffv[[2]]
                    }
                  }
                }
                else {
                  offs <- if (is.null(k)) 
                    rowSums(Xoff)
                  else rowSums(Xoff) + model.offset(mf)
                  fit[start:stop] <- X %*% object$coefficients + 
                    offs
                  if (se.fit) 
                    se[start:stop] <- sqrt(pmax(0, rowSums((X %*% 
                      object$Vp) * X)))
                  if (type == "response") {
                    linkinv <- fam$linkinv
                    if (is.null(fam$predict)) {
                      dmu.deta <- fam$mu.eta
                      if (se.fit) 
                        se[start:stop] <- se[start:stop] * abs(dmu.deta(fit[start:stop]))
                      fit[start:stop] <- linkinv(fit[start:stop])
                    }
                    else {
                      ffv <- fam$predict(fam, se.fit, y = response, 
                        X = X, beta = object$coefficients, off = offs, 
                        Vb = object$Vp)
                      if (is.null(fit1) && is.matrix(ffv[[1]])) {
                        fit1 <- matrix(0, np, ncol(ffv[[1]]))
                        if (se.fit) 
                          se1 <- fit1
                      }
                      if (is.null(fit1)) {
                        fit[start:stop] <- ffv[[1]]
                        if (se.fit) 
                          se[start:stop] <- ffv[[2]]
                      }
                      else {
                        fit1[start:stop, ] <- ffv[[1]]
                        if (se.fit) 
                          se1[start:stop, ] <- ffv[[2]]
                      }
                    }
                  }
                }
            }
            rm(X)
        }
    if ((type == "terms" || type == "iterms") && (!is.null(terms) || 
        !is.null(exclude))) {
        cnames <- colnames(fit)
        if (!is.null(terms)) {
            if (sum(!(terms %in% cnames))) 
                warning("non-existent terms requested - ignoring")
            else {
                fit <- fit[, terms, drop = FALSE]
                if (se.fit) {
                  se <- se[, terms, drop = FALSE]
                }
            }
        }
        if (!is.null(exclude)) {
            if (sum(!(exclude %in% cnames))) 
                warning("non-existent exclude terms requested - ignoring")
            else {
                exclude <- which(cnames %in% exclude)
                fit <- fit[, -exclude, drop = FALSE]
                if (se.fit) {
                  se <- se[, -exclude, drop = FALSE]
                }
            }
        }
    }
    if (type == "response" && !is.null(fit1)) {
        fit <- fit1
        if (se.fit) 
            se <- se1
    }
    rn <- rownames(newdata)
    if (type == "lpmatrix") {
        colnames(H) <- names(object$coefficients)
        rownames(H) <- rn
        if (!is.null(s.offset)) {
            s.offset <- napredict(na.act, s.offset)
            attr(H, "offset") <- s.offset
        }
        if (!is.null(attr(attr(object$model, "terms"), "offset"))) {
            attr(H, "model.offset") <- napredict(na.act, model.offset(mf))
        }
        H <- napredict(na.act, H)
        if (length(object$nsdf) > 1) {
            attr(H, "lpi") <- lpi
        }
    }
    else {
        if (se.fit) {
            if (is.null(nrow(fit))) {
                names(fit) <- rn
                names(se) <- rn
                fit <- napredict(na.act, fit)
                se <- napredict(na.act, se)
            }
            else {
                rownames(fit) <- rn
                rownames(se) <- rn
                fit <- napredict(na.act, fit)
                se <- napredict(na.act, se)
            }
            H <- list(fit = fit, se.fit = se)
        }
        else {
            H <- fit
            if (is.null(nrow(H))) 
                names(H) <- rn
            else rownames(H) <- rn
            H <- napredict(na.act, H)
        }
    }
    if ((type == "terms" || type == "iterms") && attr(object$terms, 
        "intercept") == 1) 
        attr(H, "constant") <- object$coefficients[1]
    H
}


trichol <- function (ld, sd) 
{
    n <- length(ld)
    if (n < 2) 
        stop("don't be silly")
    if (n != length(sd) + 1) 
        stop("sd should have exactly one less entry than ld")
    oo <- .C(C_tri_chol, ld = as.double(ld), sd = as.double(sd), 
        n = as.integer(n), info = as.integer(0))
    if (oo$info < 0) 
        stop("something wrong with inputs to LAPACK routine")
    if (oo$info > 0) 
        stop("not positive definite")
    ld <- sqrt(oo$ld)
    sd <- oo$sd * ld[1:(n - 1)]
    list(ld = ld, sd = sd)
}


rmvn <- function (n, mu, V) 
{
    p <- ncol(V)
    R <- mroot(V, rank = ncol(V))
    if (is.matrix(mu)) {
        if (ncol(mu) != p || nrow(mu) != n) 
            stop("mu dimensions wrong")
        z <- matrix(rnorm(p * n), n, p) %*% t(R) + mu
    }
    else {
        if (length(mu) != p) 
            stop("mu dimensions wrong")
        z <- t(R %*% matrix(rnorm(p * n), p, n) + mu)
        if (n == 1) 
            z <- as.numeric(z)
    }
    z
}


bam <- function (formula, family = gaussian(), data = list(), weights = NULL, 
    subset = NULL, na.action = na.omit, offset = NULL, method = "fREML", 
    control = list(), select = FALSE, scale = 0, gamma = 1, knots = NULL, 
    sp = NULL, min.sp = NULL, paraPen = NULL, chunk.size = 10000, 
    rho = 0, AR.start = NULL, discrete = FALSE, sparse = FALSE, 
    cluster = NULL, nthreads = NA, gc.level = 1, use.chol = FALSE, 
    samfrac = 1, drop.unused.levels = TRUE, G = NULL, fit = TRUE, 
    ...) 
{
    control <- do.call("gam.control", control)
    if (control$trace) 
        t3 <- t2 <- t1 <- t0 <- proc.time()
    if (is.null(G)) {
        if (is.character(family)) 
            family <- eval(parse(text = family))
        if (is.function(family)) 
            family <- family()
        if (is.null(family$family)) 
            stop("family not recognized")
        if (family$family == "gaussian" && family$link == "identity") 
            am <- TRUE
        else am <- FALSE
        if (scale == 0) {
            if (family$family %in% c("poisson", "binomial")) 
                scale <- 1
            else scale <- -1
        }
        if (!method %in% c("fREML", "GCV.Cp", "REML", "ML", "P-REML", 
            "P-ML")) 
            stop("un-supported smoothness selection method")
        if (is.logical(discrete)) {
            discretize <- discrete
            discrete <- NULL
        }
        else {
            discretize <- if (is.numeric(discrete)) 
                TRUE
            else FALSE
        }
        if (discretize) {
            if (method != "fREML") {
                discretize <- FALSE
                warning("discretization only available with fREML")
            }
            else {
                if (!is.null(cluster)) 
                  warning("discrete method does not use parallel cluster - use nthreads instead")
            }
        }
        if (method %in% c("fREML") && !is.null(min.sp)) {
            min.sp <- NULL
            warning("min.sp not supported with fast REML computation, and ignored.")
        }
        if (sparse && method %in% c("fREML")) {
            method <- "REML"
            warning("sparse=TRUE not supported with fast REML, reset to REML.")
        }
        gp <- interpret.gam(formula)
        if (discretize) {
            if (length(gp$smooth.spec) > 0) 
                for (i in 1:length(gp$smooth.spec)) {
                  if (inherits(gp$smooth.spec[[i]], "tensor.smooth.spec")) 
                    gp$smooth.spec[[i]] <- tero(gp$smooth.spec[[i]])
                  if (inherits(gp$smooth.spec[[i]], c("re.smooth.spec", 
                    "fs.smooth.spec")) && gp$smooth.spec[[i]]$dim > 
                    1) {
                    class(gp$smooth.spec[[i]]) <- c(class(gp$smooth.spec[[i]]), 
                      "tensor.smooth.spec")
                    gp$smooth.spec[[i]]$margin <- list()
                    for (j in 1:gp$smooth.spec[[i]]$dim) gp$smooth.spec[[i]]$margin[[j]] <- list(term = gp$smooth.spec[[i]]$term[j])
                  }
                }
        }
        cl <- match.call()
        mf <- match.call(expand.dots = FALSE)
        mf$formula <- gp$fake.formula
        mf$method <- mf$family <- mf$control <- mf$scale <- mf$knots <- mf$sp <- mf$min.sp <- mf$gc.level <- mf$gamma <- mf$paraPen <- mf$chunk.size <- mf$rho <- mf$sparse <- mf$cluster <- mf$discrete <- mf$use.chol <- mf$samfrac <- mf$nthreads <- mf$G <- mf$fit <- mf$select <- mf$... <- NULL
        mf$drop.unused.levels <- drop.unused.levels
        mf[[1]] <- as.name("model.frame")
        pmf <- mf
        pmf$formula <- gp$pf
        pmf <- eval(pmf, parent.frame())
        pterms <- attr(pmf, "terms")
        if (gc.level > 0) 
            gc()
        mf <- eval(mf, parent.frame())
        if (nrow(mf) < 2) 
            stop("Not enough (non-NA) data to do anything meaningful")
        terms <- attr(mf, "terms")
        if (gc.level > 0) 
            gc()
        if (rho != 0 && !is.null(mf$"(AR.start)")) 
            if (!is.logical(mf$"(AR.start)")) 
                stop("AR.start must be logical")
        vars <- all.vars(gp$fake.formula[-2])
        inp <- parse(text = paste("list(", paste(vars, collapse = ","), 
            ")"))
        if (!is.list(data) && !is.data.frame(data)) 
            data <- as.data.frame(data)
        dl <- eval(inp, data, parent.frame())
        if (!control$keepData) {
            rm(data)
            gc()
        }
        names(dl) <- vars
        var.summary <- variable.summary(gp$pf, dl, nrow(mf))
        rm(dl)
        if (gc.level > 0) 
            gc()
        if (discretize) {
            dk <- discrete.mf(gp, mf, pmf, m = discrete)
            mf0 <- dk$mf
            sparse.cons <- 0
        }
        else {
            mf0 <- mini.mf(mf, chunk.size)
            if (sparse) 
                sparse.cons <- 2
            else sparse.cons <- -1
        }
        rm(pmf)
        if (control$trace) 
            t1 <- proc.time()
        reset <- TRUE
        while (reset) {
            G <- gam.setup(gp, pterms = pterms, data = mf0, knots = knots, 
                sp = sp, min.sp = min.sp, H = NULL, absorb.cons = TRUE, 
                sparse.cons = sparse.cons, select = select, idLinksBases = TRUE, 
                scale.penalty = control$scalePenalty, paraPen = paraPen, 
                apply.by = !discretize)
            if (!discretize && ncol(G$X) >= chunk.size) {
                chunk.size <- 4 * ncol(G$X)
                warning(gettextf("chunk.size < number of coefficients. Reset to %d", 
                  chunk.size))
                if (chunk.size >= nrow(mf)) {
                  mf0 <- mf
                }
                else reset <- FALSE
            }
            else reset <- FALSE
        }
        if (control$trace) 
            t2 <- proc.time()
        if (discretize) {
            v <- G$Xd <- list()
            G$Xd[[1]] <- model.matrix(G$pterms, mf)
            G$kd <- cbind(1:nrow(mf), dk$k)
            dk$k.start <- c(1, dk$k.start + 1)
            G$ks <- cbind(dk$k.start[-length(dk$k.start)], dk$k.start[-1])
            if (ncol(G$Xd[[1]])) {
                kb <- k <- 2
                qc <- dt <- ts <- rep(0, length(G$smooth) + 1)
                dt[1] <- ts[1] <- 1
                dk$nr <- c(NA, dk$nr)
            }
            else {
                kb <- k <- 1
                qc <- dt <- ts <- rep(0, length(G$smooth))
            }
            drop <- rep(0, 0)
            for (i in 1:length(G$smooth)) {
                ts[kb] <- k
                if (G$smooth[[i]]$by != "NA") {
                  dt[kb] <- 1
                  by.var <- dk$mf[[G$smooth[[i]]$by]][1:dk$nr[k]]
                  if (is.factor(by.var)) {
                    by.var <- as.numeric(by.var == G$smooth[[i]]$by.level)
                  }
                  G$Xd[[k]] <- matrix(by.var, dk$nr[k], 1)
                  k <- k + 1
                }
                else dt[kb] <- 0
                if (inherits(G$smooth[[i]], "tensor.smooth")) {
                  nmar <- length(G$smooth[[i]]$margin)
                  dt[kb] <- dt[kb] + nmar
                  if (inherits(G$smooth[[i]], "fs.interaction") && 
                    which(G$smooth[[i]]$fterm == G$smooth[[i]]$term) != 
                      1) {
                    G$smooth[[i]]$rind <- 2:1
                  }
                  if (!is.null(G$smooth[[i]]$rind)) {
                    rind <- k:(k + dt[kb] - 1)
                    dk$nr[rind] <- dk$nr[k + G$smooth[[i]]$rind - 
                      1]
                    G$ks[rind, ] <- G$ks[k + G$smooth[[i]]$rind - 
                      1, ]
                  }
                  for (j in 1:nmar) {
                    G$Xd[[k]] <- G$smooth[[i]]$margin[[j]]$X[1:dk$nr[k], 
                      , drop = FALSE]
                    k <- k + 1
                  }
                  di <- attr(G$smooth[[i]], "del.index")
                  if (!is.null(di) && length(di > 0)) {
                    di <- di + G$smooth[[i]]$first.para + length(drop) - 
                      1
                    drop <- c(drop, di)
                  }
                  qrc <- attr(G$smooth[[i]], "qrc")
                  if (inherits(qrc, "qr")) {
                    v[[kb]] <- qrc$qr/sqrt(qrc$qraux)
                    v[[kb]][1] <- sqrt(qrc$qraux)
                    qc[kb] <- 1
                  }
                  else {
                    v[[kb]] <- rep(0, 0)
                    if (!inherits(qrc, "character") || qrc != 
                      "no constraints") 
                      warning("unknown tensor constraint type")
                  }
                }
                else {
                  v[[kb]] <- rep(0, 0)
                  dt[kb] <- dt[kb] + 1
                  G$Xd[[k]] <- G$X[1:dk$nr[k], G$smooth[[i]]$first.para:G$smooth[[i]]$last.para]
                  k <- k + 1
                }
                kb <- kb + 1
            }
            if (length(drop > 0)) 
                G$drop <- drop
            G$v <- v
            G$ts <- ts
            G$dt <- dt
            G$qc <- qc
        }
        if (control$trace) 
            t3 <- proc.time()
        G$sparse <- sparse
        if (((!is.null(G$L) && ncol(G$L) < 1) || (length(G$sp) == 
            0)) && method == "fREML") 
            method <- "REML"
        G$var.summary <- var.summary
        G$family <- family
        G$terms <- terms
        G$pred.formula <- gp$pred.formula
        n <- nrow(mf)
        if (is.null(mf$"(weights)")) 
            G$w <- rep(1, n)
        else G$w <- mf$"(weights)"
        G$offset <- model.offset(mf)
        if (is.null(G$offset)) 
            G$offset <- rep(0, n)
        if (ncol(G$X) > nrow(mf)) 
            stop("Model has more coefficients than data")
        if (ncol(G$X) > chunk.size && !discretize) {
            chunk.size <- 4 * ncol(G$X)
            warning(gettextf("chunk.size < number of coefficients. Reset to %d", 
                chunk.size))
        }
        G$cl <- cl
        G$am <- am
        G$min.edf <- G$nsdf
        if (G$m) 
            for (i in 1:G$m) G$min.edf <- G$min.edf + G$smooth[[i]]$null.space.dim
        G$discretize <- discretize
        G$formula <- formula
        environment(G$pterms) <- environment(G$terms) <- environment(G$pred.formula) <- environment(G$formula) <- .BaseNamespaceEnv
    }
    else {
        scale <- G$scale
        mf <- G$mf
        G$mf <- NULL
        gp <- G$gp
        G$gp <- NULL
        na.action <- G$na.action
        G$na.action <- NULL
    }
    if (!fit) {
        G$scale <- scale
        G$mf <- mf
        G$na.action <- na.action
        G$gp <- gp
        return(G)
    }
    if (!is.finite(nthreads) || nthreads < 1) 
        nthreads <- max(1, length(cluster))
    G$conv.tol <- control$mgcv.tol
    G$max.half <- control$mgcv.half
    if (control$trace) 
        cat("Setup complete. Calling fit\n")
    colnamesX <- colnames(G$X)
    if (G$sparse) {
        warning("sparse=TRUE is deprecated")
        if (sum(G$X == 0)/prod(dim(G$X)) < 0.5) 
            warning("model matrix too dense for any possible benefit from sparse")
        if (nrow(mf) <= chunk.size) 
            G$X <- as(G$X, "dgCMatrix")
        else G$X <- sparse.model.matrix(G, mf, chunk.size)
        if (rho != 0) 
            warning("AR1 parameter rho unused with sparse fitting")
        object <- bgam.fit2(G, mf, chunk.size, gp, scale, gamma, 
            method = method, control = control, npt = nthreads, 
            ...)
    }
    else if (G$am && !G$discretize) {
        if (nrow(mf) > chunk.size) 
            G$X <- matrix(0, 0, ncol(G$X))
        if (gc.level > 1) 
            gc()
        object <- bam.fit(G, mf, chunk.size, gp, scale, gamma, 
            method, rho = rho, cl = cluster, gc.level = gc.level, 
            use.chol = use.chol, npt = nthreads)
    }
    else if (G$discretize) {
        object <- bgam.fitd(G, mf, gp, scale, nobs.extra = 0, 
            rho = rho, control = control, npt = nthreads, gc.level = gc.level, 
            ...)
    }
    else {
        G$X <- matrix(0, 0, ncol(G$X))
        if (gc.level > 1) 
            gc()
        if (rho != 0) 
            warning("AR1 parameter rho unused with generalized model")
        coef <- NULL
        if (samfrac < 1 && samfrac > 0) {
            ind <- sample(1:nrow(mf), ceiling(nrow(mf) * samfrac))
            if (length(ind) < 2 * ncol(G$X)) 
                warning("samfrac too small - ignored")
            else {
                Gw <- G$w
                Goffset <- G$offset
                G$w <- G$w[ind]
                G$offset <- G$offset[ind]
                control1 <- control
                control1$epsilon <- 0.01
                object <- bgam.fit(G, mf[ind, ], chunk.size, 
                  gp, scale, gamma, method = method, nobs.extra = 0, 
                  control = control1, cl = cluster, npt = nthreads, 
                  gc.level = gc.level, use.chol = use.chol, samfrac = 1, 
                  ...)
                G$w <- Gw
                G$offset <- Goffset
                coef <- object$coefficients
            }
        }
        object <- bgam.fit(G, mf, chunk.size, gp, scale, gamma, 
            method = method, coef = coef, control = control, 
            cl = cluster, npt = nthreads, gc.level = gc.level, 
            use.chol = use.chol, ...)
    }
    if (gc.level > 0) 
        gc()
    if (control$trace) 
        t4 <- proc.time()
    if (control$trace) 
        cat("Fit complete. Finishing gam object.\n")
    if (scale < 0) {
        object$scale.estimated <- TRUE
        object$scale <- object$scale.est
    }
    else {
        object$scale.estimated <- FALSE
        object$scale <- scale
    }
    object$assign <- G$assign
    object$boundary <- FALSE
    object$call <- G$cl
    object$cmX <- G$cmX
    object$contrasts <- G$contrasts
    object$control <- control
    object$converged <- TRUE
    object$data <- NA
    object$df.null <- nrow(mf)
    object$df.residual <- object$df.null - sum(object$edf)
    object$family <- family
    object$formula <- G$formula
    if (method == "GCV.Cp") {
        if (scale <= 0) 
            object$method <- "GCV"
        else object$method <- "UBRE"
    }
    else {
        object$method <- method
    }
    object$min.edf <- G$min.edf
    object$model <- mf
    rm(mf)
    if (gc.level > 0) 
        gc()
    object$na.action <- attr(object$model, "na.action")
    object$nsdf <- G$nsdf
    if (G$nsdf > 0) 
        names(object$coefficients)[1:G$nsdf] <- colnamesX[1:G$nsdf]
    object$offset <- G$offset
    object$pterms <- G$pterms
    object$pred.formula <- G$pred.formula
    object$smooth <- G$smooth
    object$terms <- G$terms
    object$var.summary <- G$var.summary
    if (is.null(object$wt)) 
        object$weights <- object$prior.weights
    else object$weights <- object$wt
    object$xlevels <- G$xlevels
    object$NA.action <- na.action
    names(object$sp) <- names(G$sp)
    if (!is.null(object$full.sp)) 
        names(object$full.sp) <- names(G$lsp0)
    names(object$coefficients) <- G$term.names
    names(object$edf) <- G$term.names
    class(object) <- c("bam", "gam", "glm", "lm")
    if (!G$discretize) {
        object$linear.predictors <- as.numeric(predict.bam(object, 
            newdata = object$model, block.size = chunk.size, 
            cluster = cluster))
    }
    else {
        object$dinfo <- list(gp = gp, v = G$v, ts = G$ts, dt = G$dt, 
            qc = G$qc, drop = G$drop)
    }
    rm(G)
    if (gc.level > 0) 
        gc()
    object$fitted.values <- family$linkinv(object$linear.predictors)
    object$residuals <- sqrt(family$dev.resids(object$y, object$fitted.values, 
        object$prior.weights)) * sign(object$y - object$fitted.values)
    if (rho != 0) 
        object$std.rsd <- AR.resid(object$residuals, rho, object$model$"(AR.start)")
    dev <- object$deviance <- sum(object$residuals^2)
    if (rho != 0 && family$family == "gaussian") 
        dev <- sum(object$std.rsd^2)
    object$aic <- family$aic(object$y, 1, object$fitted.values, 
        object$prior.weights, dev) - 2 * (length(object$y) - 
        sum(sum(object$model[["(AR.start)"]]))) * log(1/sqrt(1 - 
        rho^2)) + 2 * sum(object$edf)
    if (!is.null(object$edf2) && sum(object$edf2) > sum(object$edf1)) 
        object$edf2 <- object$edf1
    object$null.deviance <- sum(family$dev.resids(object$y, weighted.mean(object$y, 
        object$prior.weights), object$prior.weights))
    if (!is.null(object$full.sp)) {
        if (length(object$full.sp) == length(object$sp) && all.equal(object$sp, 
            object$full.sp) == TRUE) 
            object$full.sp <- NULL
    }
    environment(object$formula) <- environment(object$pred.formula) <- environment(object$terms) <- environment(object$pterms) <- environment(attr(object$model, 
        "terms")) <- .GlobalEnv
    if (control$trace) {
        t5 <- proc.time()
        t5 <- rbind(t1 - t0, t2 - t1, t3 - t2, t4 - t3, t5 - 
            t4)[, 1:3]
        row.names(t5) <- c("initial", "gam.setup", "pre-fit", 
            "fit", "finalise")
        print(t5)
    }
    names(object$gcv.ubre) <- method
    object
}


fs.boundary <- function (r0 = 0.1, r = 0.5, l = 3, n.theta = 20) 
{
    rr <- r + (r - r0)
    theta <- seq(pi, pi/2, length = n.theta)
    x <- rr * cos(theta)
    y <- rr * sin(theta)
    theta <- seq(pi/2, -pi/2, length = 2 * n.theta)
    x <- c(x, (r - r0) * cos(theta) + l)
    y <- c(y, (r - r0) * sin(theta) + r)
    theta <- seq(pi/2, pi, length = n.theta)
    x <- c(x, r0 * cos(theta))
    y <- c(y, r0 * sin(theta))
    n <- length(x)
    x <- c(x, x[n:1])
    y <- c(y, -y[n:1])
    return(list(x = x, y = y))
}


Predict.matrix.tprs.smooth <- function (object, data) 
{
    x <- array(0, 0)
    for (i in 1:object$dim) {
        xx <- data[[object$term[i]]]
        xx <- xx - object$shift[i]
        if (i == 1) 
            n <- length(xx)
        else if (length(xx) != n) 
            stop("arguments of smooth not same dimension")
        if (length(xx) < 1) 
            stop("no data to predict at")
        x <- c(x, xx)
    }
    by <- 0
    by.exists <- FALSE
    M <- null.space.dimension(object$dim, object$p.order[1])
    ind <- 1:object$bs.dim
    if (is.null(object$drop.null)) 
        object$drop.null <- 0
    if (object$drop.null > 0) 
        object$bs.dim <- object$bs.dim + M
    X <- matrix(0, n, object$bs.dim)
    oo <- .C(C_predict_tprs, as.double(x), as.integer(object$dim), 
        as.integer(n), as.integer(object$p.order[1]), as.integer(object$bs.dim), 
        as.integer(M), as.double(object$Xu), as.integer(nrow(object$Xu)), 
        as.double(object$UZ), as.double(by), as.integer(by.exists), 
        X = as.double(X))
    X <- matrix(oo$X, n, object$bs.dim)
    if (object$drop.null > 0) {
        if (FALSE) {
            X <- (X %*% object$P)[, ind]
        }
        else {
            X <- X[, ind]
            X <- sweep(X, 2, object$cmX)
        }
    }
    X
}


print.gam <- function (x, ...) 
{
    print(x$family)
    cat("Formula:\n")
    if (is.list(x$formula)) 
        for (i in 1:length(x$formula)) print(x$formula[[i]])
    else print(x$formula)
    n.smooth <- length(x$smooth)
    if (n.smooth == 0) 
        cat("Total model degrees of freedom", sum(x$edf), "\n")
    else {
        edf <- 0
        cat("\nEstimated degrees of freedom:\n")
        for (i in 1:n.smooth) edf[i] <- sum(x$edf[x$smooth[[i]]$first.para:x$smooth[[i]]$last.para])
        edf.str <- format(round(edf, digits = 4), digits = 3, 
            scientific = FALSE)
        for (i in 1:n.smooth) {
            cat(edf.str[i], " ", sep = "")
            if (i%%7 == 0) 
                cat("\n")
        }
        cat(" total =", round(sum(x$edf), digits = 2), "\n")
    }
    if (!is.null(x$method) && !(x$method %in% c("PQL", "lme.ML", 
        "lme.REML"))) 
        cat("\n", x$method, " score: ", x$gcv.ubre, "     ", 
            sep = "")
    if (!is.null(x$rank) && x$rank < length(x$coefficients)) 
        cat("rank: ", x$rank, "/", length(x$coefficients), sep = "")
    cat("\n")
    invisible(x)
}


fix.family.qf <- function (fam) 
{
    if (!inherits(fam, "family")) 
        stop("fam not a family object")
    if (!is.null(fam$qf)) 
        return(fam)
    family <- fam$family
    if (family == "poisson") {
        fam$qf <- function(p, mu, wt, scale) {
            qpois(p, mu)
        }
    }
    else if (family == "binomial") {
        fam$qf <- function(p, mu, wt, scale) {
            qbinom(p, wt, mu)/wt
        }
    }
    else if (family == "Gamma") {
        fam$qf <- function(p, mu, wt, scale) {
            qgamma(p, shape = 1/scale, scale = mu * scale)
        }
    }
    else if (family == "gaussian") {
        fam$qf <- function(p, mu, wt, scale) {
            qnorm(p, mean = mu, sd = sqrt(scale/wt))
        }
    }
    fam
}


sdiag <- function (A, k = 0) 
{
    p <- ncol(A)
    n <- nrow(A)
    if (k > p - 1 || -k > n - 1) 
        return()
    if (k >= 0) {
        i <- 1:n
        j <- (k + 1):p
    }
    else {
        i <- (-k + 1):n
        j <- 1:p
    }
    if (length(i) > length(j)) 
        i <- i[1:length(j)]
    else j <- j[1:length(i)]
    ii <- i + (j - 1) * n
    A[ii]
}


cox.ph <- function (link = "identity") 
{
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    if (linktemp %in% c("identity")) 
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
        else stop(linktemp, " link not available for coxph family; available link is \"identity\" ")
    }
    env <- new.env(parent = .GlobalEnv)
    validmu <- function(mu) all(is.finite(mu))
    preinitialize <- expression({
        G$family$data <- list()
        y.order <- order(G$y, decreasing = TRUE)
        G$family$data$y.order <- y.order
        G$y <- G$y[y.order]
        G$X <- G$X[y.order, , drop = FALSE]
        G$w <- G$w[y.order]
    })
    postproc <- expression({
        object$family$data <- G$family$hazard(G$y, G$X, object$coefficients, 
            G$w)
        rumblefish <- G$family$hazard(G$y, matrix(0, nrow(G$X), 
            0), object$coefficients, G$w)
        s0.base <- exp(-rumblefish$h[rumblefish$r])
        s0.base[s0.base >= 1] <- 1 - 2 * .Machine$double.eps
        object$fitted.values <- exp(-object$family$data$h[object$family$data$r] * 
            exp(object$linear.predictors))
        s.base <- exp(-object$family$data$h[object$family$data$r])
        s.base[s.base >= 1] <- 1 - 2 * .Machine$double.eps
        object$null.deviance <- 2 * sum(abs((object$prior.weights + 
            log(s0.base) + object$prior.weights * (log(-log(s0.base))))))
        object$linear.predictors[y.order] <- object$linear.predictors
        object$fitted.values[y.order] <- object$fitted.values
        object$y[y.order] <- object$y
        object$prior.weights[y.order] <- object$prior.weights
    })
    initialize <- expression({
        n <- rep(1, nobs)
        if (is.null(start)) start <- rep(0, ncol(x))
    })
    hazard <- function(y, X, beta, wt) {
        tr <- unique(y)
        r <- match(y, tr)
        nt <- length(tr)
        oo <- .C("coxpp", as.double(X %*% beta), A = as.double(X), 
            as.integer(r), d = as.integer(wt), h = as.double(rep(0, 
                nt)), q = as.double(rep(0, nt)), km = as.double(rep(0, 
                nt)), n = as.integer(nrow(X)), p = as.integer(ncol(X)), 
            nt = as.integer(nt), PACKAGE = "mgcv")
        p <- ncol(X)
        list(tr = tr, h = oo$h, q = oo$q, a = matrix(oo$A[p * 
            nt], p, nt), nt = nt, r = r, km = oo$km)
    }
    residuals <- function(object, type = c("deviance", "martingale")) {
        type <- match.arg(type)
        w <- object$prior.weights
        log.s <- log(object$fitted.values)
        res <- w + log.s
        if (type == "deviance") {
            log.s[log.s > -1e-50] <- -1e-50
            res <- sign(res) * sqrt(-2 * (res + w * log(-log.s)))
        }
        res
    }
    predict <- function(family, se = FALSE, eta = NULL, y = NULL, 
        X = NULL, beta = NULL, off = NULL, Vb = NULL) {
        if (sum(is.na(y)) > 0) 
            stop("NA times supplied for cox.ph prediction")
        ii <- order(y, decreasing = TRUE)
        n <- nrow(X)
        oo <- .C("coxpred", as.double(X[ii, ]), t = as.double(y[ii]), 
            as.double(beta), as.double(Vb), a = as.double(family$data$a), 
            h = as.double(family$data$h), q = as.double(family$data$q), 
            tr = as.double(family$data$tr), n = as.integer(n), 
            p = as.integer(ncol(X)), nt = as.integer(family$data$nt), 
            s = as.double(rep(0, n)), se = as.double(rep(0, n)), 
            PACKAGE = "mgcv")
        s <- sef <- oo$s
        s[ii] <- oo$s
        sef[ii] <- oo$se
        if (se) 
            return(list(fit = s, se.fit = sef))
        else return(list(fit = s))
    }
    rd <- qf <- NULL
    ll <- function(y, X, coef, wt, family, deriv = 0, d1b = 0, 
        d2b = 0, Hp = NULL, rank = 0, fh = NULL, D = NULL) {
        tr <- unique(y)
        r <- match(y, tr)
        p <- ncol(X)
        deriv <- deriv - 1
        mu <- X %*% coef
        g <- rep(0, p)
        H <- rep(0, p * p)
        if (deriv > 0) {
            M <- ncol(d1b)
            d1H <- if (deriv == 1) 
                rep(0, p * M)
            else rep(0, p * p * M)
        }
        else M <- d1H <- 0
        if (deriv > 2) {
            d2H <- rep(0, p * M * (M + 1)/2)
            if (is.list(fh)) {
                ev <- fh
            }
            else {
                ev <- eigen(Hp, symmetric = TRUE)
                if (rank < p) 
                  ev$values[(rank + 1):p] <- 0
            }
            X <- X %*% (ev$vectors * D)
            d1b <- t(ev$vectors) %*% (d1b/D)
            d2b <- t(ev$vectors) %*% (d2b/D)
        }
        else trHid2H <- d2H <- 0
        oo <- .C("coxlpl", as.double(mu), as.double(X), as.integer(r), 
            as.integer(wt), as.double(tr), n = as.integer(length(y)), 
            p = as.integer(p), nt = as.integer(length(tr)), lp = as.double(0), 
            g = as.double(g), H = as.double(H), d1b = as.double(d1b), 
            d1H = as.double(d1H), d2b = as.double(d2b), d2H = as.double(d2H), 
            n.sp = as.integer(M), deriv = as.integer(deriv), 
            PACKAGE = "mgcv")
        if (deriv == 1) 
            d1H <- matrix(oo$d1H, p, M)
        else if (deriv > 1) {
            ind <- 1:(p^2)
            d1H <- list()
            for (i in 1:M) {
                d1H[[i]] <- matrix(oo$d1H[ind], p, p)
                ind <- ind + p^2
            }
        }
        if (deriv > 2) {
            d2H <- matrix(oo$d2H, p, M * (M + 1)/2)
            d <- ev$values
            d[d > 0] <- 1/d[d > 0]
            d[d <= 0] <- 0
            trHid2H <- colSums(d2H * d)
        }
        assign(".log.partial.likelihood", oo$lp, envir = environment(sys.function()))
        list(l = oo$lp, lb = oo$g, lbb = matrix(oo$H, p, p), 
            d1H = d1H, d2H = d2H, trHid2H = trHid2H)
    }
    environment(ll) <- env
    structure(list(family = "Cox PH", link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, ll = ll, mu.eta = stats$mu.eta, 
        initialize = initialize, preinitialize = preinitialize, 
        postproc = postproc, hazard = hazard, predict = predict, 
        residuals = residuals, validmu = validmu, valideta = stats$valideta, 
        rd = rd, qf = qf, drop.intercept = TRUE, ls = 1, available.derivs = 2), 
        class = c("general.family", "extended.family", "family"))
}


gam.outer <- function (lsp, fscale, family, control, method, optimizer, criterion, 
    scale, gamma, G, ...) 
{
    if (is.null(optimizer[2])) 
        optimizer[2] <- "newton"
    if (!optimizer[2] %in% c("newton", "bfgs", "nlm", "optim", 
        "nlm.fd")) 
        stop("unknown outer optimization method.")
    if (length(lsp) == 0) {
        optimizer[2] <- "no.sps"
    }
    nbGetTheta <- substr(family$family[1], 1, 17) == "Negative Binomial" && 
        length(family$getTheta()) > 1
    if (nbGetTheta) 
        stop("Please provide a single value for theta or use nb to estimate it")
    if (optimizer[2] == "nlm.fd") {
        if (method %in% c("REML", "ML", "GACV.Cp", "P-ML", "P-REML")) 
            stop("nlm.fd only available for GCV/UBRE")
        um <- nlm(full.score, lsp, typsize = lsp, fscale = fscale, 
            stepmax = control$nlm$stepmax, ndigit = control$nlm$ndigit, 
            gradtol = control$nlm$gradtol, steptol = control$nlm$steptol, 
            iterlim = control$nlm$iterlim, G = G, family = family, 
            control = control, gamma = gamma, ...)
        lsp <- um$estimate
        object <- attr(full.score(lsp, G, family, control, gamma = gamma, 
            ...), "full.gam.object")
        object$gcv.ubre <- um$minimum
        object$outer.info <- um
        object$sp <- exp(lsp)
        return(object)
    }
    family <- fix.family.link(family)
    family <- fix.family.var(family)
    if (method %in% c("REML", "ML", "P-REML", "P-ML")) 
        family <- fix.family.ls(family)
    if (optimizer[2] == "newton" || optimizer[2] == "bfgs") {
        if (optimizer[2] == "bfgs") 
            b <- bfgs(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, 
                UrS = G$UrS, L = G$L, lsp0 = G$lsp0, offset = G$offset, 
                U1 = G$U1, Mp = G$Mp, family = family, weights = G$w, 
                control = control, gamma = gamma, scale = scale, 
                conv.tol = control$newton$conv.tol, maxNstep = control$newton$maxNstep, 
                maxSstep = control$newton$maxSstep, maxHalf = control$newton$maxHalf, 
                printWarn = FALSE, scoreType = criterion, null.coef = G$null.coef, 
                pearson.extra = G$pearson.extra, dev.extra = G$dev.extra, 
                n.true = G$n.true, Sl = G$Sl, ...)
        else b <- newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, 
            UrS = G$UrS, L = G$L, lsp0 = G$lsp0, offset = G$offset, 
            U1 = G$U1, Mp = G$Mp, family = family, weights = G$w, 
            control = control, gamma = gamma, scale = scale, 
            conv.tol = control$newton$conv.tol, maxNstep = control$newton$maxNstep, 
            maxSstep = control$newton$maxSstep, maxHalf = control$newton$maxHalf, 
            printWarn = FALSE, scoreType = criterion, null.coef = G$null.coef, 
            pearson.extra = G$pearson.extra, dev.extra = G$dev.extra, 
            n.true = G$n.true, Sl = G$Sl, ...)
        object <- b$object
        object$REML <- object$REML1 <- object$REML2 <- object$GACV <- object$D2 <- object$P2 <- object$UBRE2 <- object$trA2 <- object$GACV1 <- object$GACV2 <- object$GCV2 <- object$D1 <- object$P1 <- NULL
        object$sp <- as.numeric(exp(b$lsp))
        object$gcv.ubre <- b$score
        b <- list(conv = b$conv, iter = b$iter, grad = b$grad, 
            hess = b$hess, score.hist = b$score.hist)
        object$outer.info <- b
    }
    else {
        args <- list(X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, 
            offset = G$offset, U1 = G$U1, Mp = G$Mp, family = family, 
            weights = G$w, control = control, scoreType = criterion, 
            gamma = gamma, scale = scale, L = G$L, lsp0 = G$lsp0, 
            null.coef = G$null.coef, n.true = G$n.true)
        if (optimizer[2] == "nlm") {
            b <- nlm(gam4objective, lsp, typsize = lsp, fscale = fscale, 
                stepmax = control$nlm$stepmax, ndigit = control$nlm$ndigit, 
                gradtol = control$nlm$gradtol, steptol = control$nlm$steptol, 
                iterlim = control$nlm$iterlim, check.analyticals = control$nlm$check.analyticals, 
                args = args, ...)
            lsp <- b$estimate
        }
        else if (optimizer[2] == "optim") {
            b <- optim(par = lsp, fn = gam2objective, gr = gam2derivative, 
                method = "L-BFGS-B", control = list(fnscale = fscale, 
                  factr = control$optim$factr, lmm = min(5, length(lsp))), 
                args = args, ...)
            lsp <- b$par
        }
        else b <- NULL
        obj <- gam2objective(lsp, args, printWarn = TRUE, ...)
        object <- attr(obj, "full.fit")
        object$gcv.ubre <- as.numeric(obj)
        object$outer.info <- b
        object$sp <- exp(lsp)
    }
    if (scale > 0) {
        object$scale.estimated <- FALSE
        object$scale <- scale
    }
    else {
        object$scale <- object$scale.est
        object$scale.estimated <- TRUE
    }
    object$control <- control
    if (inherits(family, "general.family")) {
        mv <- gam.fit5.post.proc(object, G$Sl, G$L, G$S, G$off)
        object$coefficients <- Sl.initial.repara(G$Sl, object$coefficients, 
            inverse = TRUE)
    }
    else mv <- gam.fit3.post.proc(G$X, G$L, G$S, G$off, object)
    if (!is.null(mv$Vc)) 
        object$Vc <- mv$Vc
    if (!is.null(mv$edf2)) 
        object$edf2 <- mv$edf2
    object$Vp <- mv$Vb
    object$hat <- mv$hat
    object$Ve <- mv$Ve
    object$edf <- mv$edf
    object$edf1 <- mv$edf1
    object$R <- mv$R
    object$aic <- object$aic + 2 * sum(mv$edf)
    object$nsdf <- G$nsdf
    object$K <- object$D1 <- object$D2 <- object$P <- object$P1 <- object$P2 <- object$GACV <- object$GACV1 <- object$GACV2 <- object$REML <- object$REML1 <- object$REML2 <- object$GCV <- object$GCV1 <- object$GCV2 <- object$UBRE <- object$UBRE1 <- object$UBRE2 <- object$trA <- object$trA1 <- object$trA2 <- object$alpha <- object$alpha1 <- object$scale.est <- NULL
    object$sig2 <- object$scale
    object
}


Predict.matrix.pspline.smooth <- function (object, data) 
{
    m <- object$m[1] + 1
    ll <- object$knots[m + 1]
    ul <- object$knots[length(object$knots) - m]
    m <- m + 1
    x <- data[[object$term]]
    n <- length(x)
    ind <- x <= ul & x >= ll
    if (is.null(object$deriv)) 
        object$deriv <- 0
    if (sum(ind) == n) {
        X <- splines::spline.des(object$knots, x, m, rep(object$deriv, 
            n))$design
    }
    else {
        D <- splines::spline.des(object$knots, c(ll, ll, ul, 
            ul), m, c(0, 1, 0, 1))$design
        X <- matrix(0, n, ncol(D))
        nin <- sum(ind)
        if (nin > 0) 
            X[ind, ] <- splines::spline.des(object$knots, x[ind], 
                m, rep(object$deriv, nin))$design
        if (object$deriv < 2) {
            ind <- x < ll
            if (sum(ind) > 0) 
                X[ind, ] <- if (object$deriv == 0) 
                  cbind(1, x[ind] - ll) %*% D[1:2, ]
                else matrix(D[2, ], sum(ind), ncol(D), byrow = TRUE)
            ind <- x > ul
            if (sum(ind) > 0) 
                X[ind, ] <- if (object$deriv == 0) 
                  cbind(1, x[ind] - ul) %*% D[3:4, ]
                else matrix(D[4, ], sum(ind), ncol(D), byrow = TRUE)
        }
    }
    if (object$mono == 0) 
        X
    else X %*% object$B
}


smooth.construct.ts.smooth.spec <- function (object, data, knots) 
{
    attr(object, "shrink") <- 0.1
    object <- smooth.construct.tp.smooth.spec(object, data, knots)
    class(object) <- "ts.smooth"
    object
}


Predict.matrix.t2.smooth <- function (object, data) 
{
    m <- length(object$margin)
    X <- list()
    rank <- rep(0, m)
    for (i in 1:m) {
        term <- object$margin[[i]]$term
        dat <- list()
        for (j in 1:length(term)) dat[[term[j]]] <- data[[term[j]]]
        X[[i]] <- Predict.matrix(object$margin[[i]], dat) %*% 
            object$P[[i]]
        rank[i] <- object$margin[[i]]$rank
    }
    T <- t2.model.matrix(X, rank, full = object$full, ord = object$ord)
    T
}


smooth.construct2 <- function (object, data, knots) 
{
    dk <- ExtractData(object, data, knots)
    object <- smooth.construct(object, dk$data, dk$knots)
    ind <- attr(dk$data, "index")
    if (!is.null(ind)) {
        offs <- attr(object$X, "offset")
        object$X <- object$X[ind, ]
        if (!is.null(offs)) 
            attr(object$X, "offset") <- offs[ind]
    }
    class(object) <- c(class(object), "mgcv.smooth")
    object
}


smooth.construct.gp.smooth.spec <- function (object, data, knots) 
{
    xtra <- list()
    if (is.null(object$xt$max.knots)) 
        xtra$max.knots <- 2000
    else xtra$max.knots <- object$xt$max.knots
    if (is.null(object$xt$seed)) 
        xtra$seed <- 1
    else xtra$seed <- object$xt$seed
    x <- array(0, 0)
    for (i in 1:object$dim) {
        xx <- data[[object$term[i]]]
        if (i == 1) 
            n <- length(xx)
        else if (n != length(xx)) 
            stop("arguments of smooth not same dimension")
        x <- c(x, xx)
    }
    if (is.null(knots)) {
        knt <- 0
        nk <- 0
    }
    else {
        knt <- array(0, 0)
        for (i in 1:object$dim) {
            dum <- knots[[object$term[i]]]
            if (is.null(dum)) {
                knt <- 0
                nk <- 0
                break
            }
            knt <- c(knt, dum)
            nk0 <- length(dum)
            if (i > 1 && nk != nk0) 
                stop("components of knots relating to a single smooth must be of same length")
            nk <- nk0
        }
    }
    if (nk > n) {
        nk <- 0
        warning("more knots than data in an ms term: knots ignored.")
    }
    xu <- uniquecombs(matrix(x, n, object$dim))
    if (nrow(xu) < object$bs.dim) 
        stop("A term has fewer unique covariate combinations than specified maximum degrees of freedom")
    if (nk == 0) {
        nu <- nrow(xu)
        if (n > xtra$max.knots) {
            if (nu > xtra$max.knots) {
                seed <- try(get(".Random.seed", envir = .GlobalEnv), 
                  silent = TRUE)
                if (inherits(seed, "try-error")) {
                  runif(1)
                  seed <- get(".Random.seed", envir = .GlobalEnv)
                }
                kind <- RNGkind(NULL)
                RNGkind("default", "default")
                set.seed(xtra$seed)
                nk <- xtra$max.knots
                ind <- sample(1:nu, nk, replace = FALSE)
                knt <- as.numeric(xu[ind, ])
                RNGkind(kind[1], kind[2])
                assign(".Random.seed", seed, envir = .GlobalEnv)
            }
            else {
                knt <- xu
                nk <- nu
            }
        }
        else {
            knt <- xu
            nk <- nu
        }
    }
    x <- matrix(x, n, object$dim)
    knt <- matrix(knt, nk, object$dim)
    object$shift <- colMeans(x)
    x <- sweep(x, 2, object$shift)
    knt <- sweep(knt, 2, object$shift)
    E <- gpE(knt, knt, object$p.order)
    object$gp.defn <- attr(E, "defn")
    def.k <- c(10, 30, 100)
    dd <- ncol(knt)
    if (object$bs.dim[1] < 0) 
        object$bs.dim <- ncol(knt) + 1 + def.k[dd]
    if (object$bs.dim < ncol(knt) + 2) {
        object$bs.dim <- ncol(knt) + 2
        warning("basis dimension reset to minimum possible")
    }
    object$null.space.dim <- ncol(knt) + 1
    k <- object$bs.dim - object$null.space.dim
    if (k < nk) {
        er <- slanczos(E, k, -1)
        D <- diag(c(er$values, rep(0, object$null.space.dim)))
    }
    else {
        D <- matrix(0, object$bs.dim, object$bs.dim)
        D[1:k, 1:k] <- E
        er <- list(vectors = diag(k))
    }
    rm(E)
    object$S <- list(S = D)
    object$UZ <- er$vectors
    object$knt = knt
    object$df <- object$bs.dim
    object$rank <- k
    class(object) <- "gp.smooth"
    object$X <- Predict.matrix.gp.smooth(object, data)
    object
}


gam2objective <- function (lsp, args, ...) 
{
    reml <- args$scoreType %in% c("REML", "P-REML", "ML", "P-ML")
    if (!is.null(args$L)) {
        lsp <- args$L %*% lsp + args$lsp0
    }
    b <- gam.fit3(x = args$X, y = args$y, sp = lsp, Eb = args$Eb, 
        UrS = args$UrS, offset = args$offset, U1 = args$U1, Mp = args$Mp, 
        family = args$family, weights = args$w, deriv = 0, control = args$control, 
        gamma = args$gamma, scale = args$scale, scoreType = args$scoreType, 
        null.coef = args$null.coef, n.true = args$n.true, ...)
    if (reml) {
        ret <- b$REML
    }
    else if (args$scoreType == "GACV") {
        ret <- b$GACV
    }
    else if (args$scoreType == "UBRE") {
        ret <- b$UBRE
    }
    else {
        ret <- b$GCV
    }
    attr(ret, "full.fit") <- b
    ret
}


negbin <- function (theta = stop("'theta' must be specified"), link = "log") 
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
        else stop(gettextf("%s link not available for negative binomial family; available links are \"identity\", \"log\" and \"sqrt\"", 
            linktemp))
    }
    env <- new.env(parent = .GlobalEnv)
    assign(".Theta", theta, envir = env)
    variance <- function(mu) mu + mu^2/get(".Theta")[1]
    dvar <- function(mu) 1 + 2 * mu/get(".Theta")[1]
    d2var <- function(mu) rep(2/get(".Theta")[1], length(mu))
    d3var <- function(mu) rep(0, length(mu))
    getTheta <- function() get(".Theta")
    validmu <- function(mu) all(mu > 0)
    dev.resids <- function(y, mu, wt) {
        Theta <- get(".Theta")[1]
        2 * wt * (y * log(pmax(1, y)/mu) - (y + Theta) * log((y + 
            Theta)/(mu + Theta)))
    }
    aic <- function(y, n, mu, wt, dev) {
        Theta <- get(".Theta")[1]
        term <- (y + Theta) * log(mu + Theta) - y * log(mu) + 
            lgamma(y + 1) - Theta * log(Theta) + lgamma(Theta) - 
            lgamma(Theta + y)
        2 * sum(term * wt)
    }
    ls <- function(y, w, n, scale) {
        Theta <- get(".Theta")[1]
        ylogy <- y
        ind <- y > 0
        ylogy[ind] <- y[ind] * log(y[ind])
        term <- (y + Theta) * log(y + Theta) - ylogy + lgamma(y + 
            1) - Theta * log(Theta) + lgamma(Theta) - lgamma(Theta + 
            y)
        c(-sum(term * w), 0, 0)
    }
    initialize <- expression({
        if (any(y < 0)) stop("negative values not allowed for the negative binomial family")
        n <- rep(1, nobs)
        mustart <- y + (y == 0)/6
    })
    rd <- function(mu, wt, scale) {
        Theta <- get(".Theta")[1]
        rnbinom(mu, size = Theta, mu = mu)
    }
    qf <- function(p, mu, wt, scale) {
        Theta <- get(".Theta")[1]
        qnbinom(p, size = Theta, mu = mu)
    }
    environment(qf) <- environment(rd) <- environment(dvar) <- environment(d2var) <- environment(d3var) <- environment(variance) <- environment(validmu) <- environment(ls) <- environment(dev.resids) <- environment(aic) <- environment(getTheta) <- env
    famname <- paste("Negative Binomial(", format(round(theta, 
        3)), ")", sep = "")
    structure(list(family = famname, link = linktemp, linkfun = stats$linkfun, 
        linkinv = stats$linkinv, variance = variance, dvar = dvar, 
        d2var = d2var, d3var = d3var, dev.resids = dev.resids, 
        aic = aic, mu.eta = stats$mu.eta, initialize = initialize, 
        ls = ls, validmu = validmu, valideta = stats$valideta, 
        getTheta = getTheta, qf = qf, rd = rd, canonical = "log"), 
        class = "family")
}


Predict.matrix.tensor.smooth <- function (object, data) 
{
    m <- length(object$margin)
    X <- list()
    for (i in 1:m) {
        term <- object$margin[[i]]$term
        dat <- list()
        for (j in 1:length(term)) dat[[term[j]]] <- data[[term[j]]]
        X[[i]] <- if (object$mc[i]) 
            PredictMat(object$margin[[i]], dat, n = length(dat[[1]]))
        else Predict.matrix(object$margin[[i]], dat)
    }
    mxp <- length(object$XP)
    if (mxp > 0) 
        for (i in 1:mxp) if (!is.null(object$XP[[i]])) 
            X[[i]] <- X[[i]] %*% object$XP[[i]]
    T <- tensor.prod.model.matrix(X)
    if (is.null(object$udrop)) 
        T
    else T[, object$udrop]
}


formula.gam <- function (x, ...) 
{
    x$formula
}


spasm.sp <- function (object, sp, w = rep(1, object$nobs), get.trH = TRUE, 
    block = 0, centre = FALSE) 
UseMethod("spasm.sp")


notLog <- function (x) 
{
    f <- x
    ind <- x > exp(1)
    f[ind] <- sqrt(2 * x[ind]/exp(1) - 1)
    ind <- !ind & x > exp(-1)
    f[ind] <- log(x[ind])
    ind <- x <= exp(-1)
    x[ind] <- 1/x[ind]
    f[ind] <- sqrt(2 * x[ind]/exp(1) - 1)
    f[ind] <- -f[ind]
    f
}


smooth.construct.sf.smooth.spec <- function (object, data, knots) 
{
    if (is.null(knots)) 
        stop("knots must be specified for soap")
    if (object$dim != 2) 
        stop("soap films are bivariate only")
    x <- data[[object$term[1]]]
    y <- data[[object$term[2]]]
    knt <- list(x = knots[[object$term[1]]], y = knots[[object$term[2]]])
    bnd <- object$xt$bnd
    if (is.null(bnd)) 
        stop("can't soap smooth without a boundary")
    if (!inherits(bnd, "list")) 
        stop("bnd must be a list of boundary loops")
    for (i in 1:length(bnd)) {
        nm <- names(bnd[[i]])
        ind <- nm == object$term[1]
        if (sum(ind) != 1) 
            stop("faulty bnd")
        names(bnd[[i]])[ind] <- "x"
        ind <- nm == object$term[2]
        if (sum(ind) != 1) 
            stop("faulty bnd")
        names(bnd[[i]])[ind] <- "y"
    }
    if (length(object$bs.dim) == 1) 
        k <- rep(object$bs.dim, length(bnd))
    else {
        if (length(object$bs.dim) == length(bnd)) 
            k <- object$bs.dim
        else stop("k and bnd lengths are inconsistent")
    }
    if (is.null(object$xt$nmax)) 
        nmax <- 200
    else nmax <- object$xt$nmax
    sd <- setup.soap(bnd, knots = knt, nmax = nmax, k = k, bndSpec = object$xt$bndSpec)
    b <- soap.basis(sd, x, y, film = TRUE, wiggly = FALSE, penalty = TRUE)
    if (sum(is.na(b$X)) > 0) 
        stop("data outside soap boundary")
    if (ncol(b$X) == 0) 
        stop("no free coefs in sf smooth")
    ns.dim <- 0
    n <- length(sd$bc)
    k <- 0
    rr <- rep(0, length(b$S))
    if (n > 0) 
        for (i in 1:n) if (sd$bc[[i]]$free.bound) {
            nsd <- sd$bc[[i]]$bsm$null.space.dim
            ns.dim <- ns.dim + nsd
            k <- k + 1
            rr[k] <- ncol(b$S[[k]]) - nsd
        }
    object$null.space.dim <- ns.dim
    object$rank <- rr
    need.con <- TRUE
    for (i in 1:length(sd$bc)) if (!sd$bc[[i]]$free.bound) 
        need.con <- FALSE
    irng <- 1/as.numeric(apply(b$X, 2, max) - apply(b$X, 2, min))
    b$X <- t(t(b$X) * irng)
    if (length(b$S) > 0) 
        for (i in 1:length(b$S)) {
            a <- irng[b$off[i]:(b$off[i] + ncol(b$S[[i]]) - 1)]
            b$S[[i]] <- diag(a) %*% b$S[[i]] %*% diag(a)
        }
    object$irng <- irng
    object$X <- b$X
    attr(object$X, "offset") <- b$offset
    if (!object$fixed) {
        S <- list()
        n <- ncol(object$X)
        if (length(b$S) > 0) 
            for (i in 1:length(b$S)) {
                S[[i]] <- matrix(0, n, n)
                m <- ncol(b$S[[i]])
                ind <- b$off[i]:(b$off[i] + m - 1)
                S[[i]][ind, ind] <- b$S[[i]]
            }
        object$S <- S
    }
    if (!need.con) 
        object$C <- matrix(0, 0, ncol(object$X))
    object$df <- ncol(object$X)
    for (i in 1:length(sd$bc)) {
        sd$bc[[i]]$bsm <- sd$bc[[i]]$S <- NULL
    }
    object$sd <- sd
    class(object) <- c("sf", "soap.film")
    object
}


mono.con <- function (x, up = TRUE, lower = NA, upper = NA) 
{
    if (is.na(lower)) {
        lo <- 0
        lower <- 0
    }
    else lo <- 1
    if (is.na(upper)) {
        hi <- 0
        upper <- 0
    }
    else hi <- 1
    if (up) 
        inc <- 1
    else inc <- 0
    control <- 4 * inc + 2 * lo + hi
    n <- length(x)
    if (n < 4) 
        stop("At least three knots required in call to mono.con.")
    A <- matrix(0, 4 * (n - 1) + lo + hi, n)
    b <- array(0, 4 * (n - 1) + lo + hi)
    if (lo * hi == 1 && lower >= upper) 
        stop("lower bound >= upper bound in call to mono.con()")
    oo <- .C(C_RMonoCon, as.double(A), as.double(b), as.double(x), 
        as.integer(control), as.double(lower), as.double(upper), 
        as.integer(n))
    A <- matrix(oo[[1]], dim(A)[1], dim(A)[2])
    b <- array(oo[[2]], dim(A)[1])
    list(A = A, b = b)
}


polys.plot <- function (pc, z = NULL, scheme = "heat", lab = "", ...) 
{
    for (i in 1:length(pc)) {
        yr <- range(pc[[i]][, 2], na.rm = TRUE)
        xr <- range(pc[[i]][, 1], na.rm = TRUE)
        if (i == 1) {
            ylim <- yr
            xlim <- xr
        }
        else {
            if (yr[1] < ylim[1]) 
                ylim[1] <- yr[1]
            if (yr[2] > ylim[2]) 
                ylim[2] <- yr[2]
            if (xr[1] < xlim[1]) 
                xlim[1] <- xr[1]
            if (xr[2] > xlim[2]) 
                xlim[2] <- xr[2]
        }
    }
    mar <- par("mar")
    oldpar <- par(mar = c(2, mar[2], 2, 1))
    if (is.null(z)) {
        plot(0, 0, ylim = ylim, xlim = xlim, xaxt = "n", yaxt = "n", 
            type = "n", bty = "n", ylab = lab, xlab = "", ...)
        for (i in 1:length(pc)) {
            poly2(pc[[i]], col = NA)
        }
    }
    else {
        nz <- names(z)
        npc <- names(pc)
        if (!is.null(nz) && !is.null(npc)) {
            if (all.equal(sort(nz), sort(npc)) != TRUE) 
                stop("names of z and pc must match")
            z <- z[npc]
        }
        xmin <- xlim[1]
        xlim[1] <- xlim[1] - 0.1 * (xlim[2] - xlim[1])
        n.col <- 100
        if (scheme == "heat") 
            scheme <- heat.colors(n.col + 1)
        else scheme <- gray(0:n.col/n.col)
        zlim <- range(pretty(z))
        for (i in 1:length(pc)) pc[[i]][, 2] <- zlim[1] + (zlim[2] - 
            zlim[1]) * (pc[[i]][, 2] - ylim[1])/(ylim[2] - ylim[1])
        ylim <- zlim
        plot(0, 0, ylim = ylim, xlim = xlim, type = "n", xaxt = "n", 
            bty = "n", xlab = "", ylab = lab, ...)
        for (i in 1:length(pc)) {
            coli <- round((z[i] - zlim[1])/(zlim[2] - zlim[1]) * 
                n.col) + 1
            poly2(pc[[i]], col = scheme[coli])
        }
        xmin <- min(c(axTicks(1), xlim[1]))
        dx <- (xlim[2] - xlim[1]) * 0.05
        x0 <- xmin - 2 * dx
        x1 <- xmin + dx
        dy <- (ylim[2] - ylim[1])/n.col
        poly <- matrix(c(x0, x0, x1, x1, ylim[1], ylim[1] + dy, 
            ylim[1] + dy, ylim[1]), 4, 2)
        for (i in 1:n.col) {
            polygon(poly, col = scheme[i], border = NA)
            poly[, 2] <- poly[, 2] + dy
        }
        poly <- matrix(c(x0, x0, x1, x1, ylim[1], ylim[2], ylim[2], 
            ylim[1]), 4, 2)
        polygon(poly, border = "black")
    }
    par(oldpar)
}


Predict.matrix2 <- function (object, data) 
{
    dk <- ExtractData(object, data, NULL)
    X <- Predict.matrix(object, dk$data)
    ind <- attr(dk$data, "index")
    if (!is.null(ind)) {
        offs <- attr(X, "offset")
        X <- X[ind, ]
        if (!is.null(offs)) 
            attr(X, "offset") <- offs[ind]
    }
    X
}


gam <- function (formula, family = gaussian(), data = list(), weights = NULL, 
    subset = NULL, na.action, offset = NULL, method = "GCV.Cp", 
    optimizer = c("outer", "newton"), control = list(), scale = 0, 
    select = FALSE, knots = NULL, sp = NULL, min.sp = NULL, H = NULL, 
    gamma = 1, fit = TRUE, paraPen = NULL, G = NULL, in.out = NULL, 
    drop.unused.levels = TRUE, ...) 
{
    control <- do.call("gam.control", control)
    if (is.null(G)) {
        gp <- interpret.gam(formula)
        cl <- match.call()
        mf <- match.call(expand.dots = FALSE)
        mf$formula <- gp$fake.formula
        mf$family <- mf$control <- mf$scale <- mf$knots <- mf$sp <- mf$min.sp <- mf$H <- mf$select <- mf$gamma <- mf$method <- mf$fit <- mf$paraPen <- mf$G <- mf$optimizer <- mf$in.out <- mf$... <- NULL
        mf$drop.unused.levels <- drop.unused.levels
        mf[[1]] <- as.name("model.frame")
        pmf <- mf
        mf <- eval(mf, parent.frame())
        if (nrow(mf) < 2) 
            stop("Not enough (non-NA) data to do anything meaningful")
        terms <- attr(mf, "terms")
        vars <- all.vars(gp$fake.formula[-2])
        inp <- parse(text = paste("list(", paste(vars, collapse = ","), 
            ")"))
        if (!is.list(data) && !is.data.frame(data)) 
            data <- as.data.frame(data)
        dl <- eval(inp, data, parent.frame())
        names(dl) <- vars
        var.summary <- variable.summary(gp$pf, dl, nrow(mf))
        rm(dl)
        if (is.list(formula)) {
            environment(formula) <- environment(formula[[1]])
            pterms <- list()
            tlab <- rep("", 0)
            for (i in 1:length(formula)) {
                pmf$formula <- gp[[i]]$pf
                pterms[[i]] <- attr(eval(pmf, parent.frame()), 
                  "terms")
                tlabi <- attr(pterms[[i]], "term.labels")
                if (i > 1 && length(tlabi) > 0) 
                  tlabi <- paste(tlabi, i - 1, sep = ".")
                tlab <- c(tlab, tlabi)
            }
            attr(pterms, "term.labels") <- tlab
        }
        else {
            pmf$formula <- gp$pf
            pmf <- eval(pmf, parent.frame())
            pterms <- attr(pmf, "terms")
        }
        if (is.character(family)) 
            family <- eval(parse(text = family))
        if (is.function(family)) 
            family <- family()
        if (is.null(family$family)) 
            stop("family not recognized")
        if (family$family[1] == "gaussian" && family$link == 
            "identity") 
            am <- TRUE
        else am <- FALSE
        if (!control$keepData) 
            rm(data)
        drop.intercept <- if (is.null(family$drop.intercept) || 
            !family$drop.intercept) 
            FALSE
        else TRUE
        if (inherits(family, "general.family") && !is.null(family$presetup)) 
            eval(family$presetup)
        gsname <- if (is.list(formula)) 
            "gam.setup.list"
        else "gam.setup"
        G <- do.call(gsname, list(formula = gp, pterms = pterms, 
            data = mf, knots = knots, sp = sp, min.sp = min.sp, 
            H = H, absorb.cons = TRUE, sparse.cons = 0, select = select, 
            idLinksBases = control$idLinksBases, scale.penalty = control$scalePenalty, 
            paraPen = paraPen, drop.intercept = drop.intercept))
        G$var.summary <- var.summary
        G$family <- family
        if ((is.list(formula) && (is.null(family$nlp) || family$nlp != 
            gp$nlp)) || (!is.list(formula) && !is.null(family$npl) && 
            (family$npl > 1))) 
            stop("incorrect number of linear predictors for family")
        if (ncol(G$X) > nrow(G$X)) 
            stop("Model has more coefficients than data")
        G$terms <- terms
        G$mf <- mf
        G$cl <- cl
        G$am <- am
        if (is.null(G$offset)) 
            G$offset <- rep(0, G$n)
        G$min.edf <- G$nsdf
        if (G$m) 
            for (i in 1:G$m) G$min.edf <- G$min.edf + G$smooth[[i]]$null.space.dim
        G$formula <- formula
        G$pred.formula <- gp$pred.formula
        environment(G$formula) <- environment(formula)
    }
    if (!fit) 
        return(G)
    G$conv.tol <- control$mgcv.tol
    G$max.half <- control$mgcv.half
    object <- estimate.gam(G, method, optimizer, control, in.out, 
        scale, gamma, ...)
    if (!is.null(G$L)) {
        object$full.sp <- as.numeric(exp(G$L %*% log(object$sp) + 
            G$lsp0))
        names(object$full.sp) <- names(G$lsp0)
    }
    names(object$sp) <- names(G$sp)
    object$paraPen <- G$pP
    object$formula <- G$formula
    if (is.list(object$formula)) 
        attr(object$formula, "lpi") <- attr(G$X, "lpi")
    object$var.summary <- G$var.summary
    object$cmX <- G$cmX
    object$model <- G$mf
    object$na.action <- attr(G$mf, "na.action")
    object$control <- control
    object$terms <- G$terms
    object$pred.formula <- G$pred.formula
    attr(object$pred.formula, "full") <- reformulate(all.vars(object$terms))
    object$pterms <- G$pterms
    object$assign <- G$assign
    object$contrasts <- G$contrasts
    object$xlevels <- G$xlevels
    object$offset <- G$offset
    if (!is.null(G$Xcentre)) 
        object$Xcentre <- G$Xcentre
    if (control$keepData) 
        object$data <- data
    object$df.residual <- nrow(G$X) - sum(object$edf)
    object$min.edf <- G$min.edf
    if (G$am && !(method %in% c("REML", "ML", "P-ML", "P-REML"))) 
        object$optimizer <- "magic"
    else object$optimizer <- optimizer
    object$call <- G$cl
    class(object) <- c("gam", "glm", "lm")
    if (is.null(object$deviance)) 
        object$deviance <- sum(residuals(object, "deviance")^2)
    names(object$gcv.ubre) <- method
    environment(object$formula) <- environment(object$pred.formula) <- environment(object$terms) <- environment(object$pterms) <- .GlobalEnv
    if (!is.null(object$model)) 
        environment(attr(object$model, "terms")) <- .GlobalEnv
    if (!is.null(attr(object$pred.formula, "full"))) 
        environment(attr(object$pred.formula, "full")) <- .GlobalEnv
    object
}


bam.update <- function (b, data, chunk.size = 10000) 
{
    if (is.null(b$qrx)) {
        stop("Model can not be updated")
    }
    gp <- interpret.gam(b$formula)
    X <- predict(b, newdata = data, type = "lpmatrix", na.action = b$NA.action)
    rownames(X) <- NULL
    cnames <- names(b$coefficients)
    AR.start <- NULL
    getw <- "(weights)" %in% names(b$model)
    getARs <- "(AR.start)" %in% names(b$model)
    if (getw && getARs) {
        mf <- model.frame(gp$fake.formula, data, weights = weights, 
            AR.start = AR.start, xlev = b$xlev, na.action = b$NA.action)
        w <- mf[["(weights)"]]
    }
    else if (getw) {
        mf <- model.frame(gp$fake.formula, data, weights = weights, 
            xlev = b$xlev, na.action = b$NA.action)
        w <- mf[["(weights)"]]
    }
    else if (getARs) {
        mf <- model.frame(gp$fake.formula, data, AR.start = AR.start, 
            xlev = b$xlev, na.action = b$NA.action)
        w <- rep(1, nrow(mf))
    }
    else {
        mf <- model.frame(gp$fake.formula, data, xlev = b$xlev, 
            na.action = b$NA.action)
        w <- rep(1, nrow(mf))
    }
    b$model <- rbind(b$model, mf)
    off.col <- attr(attr(b$model, "terms"), "offset")
    if (is.null(off.col)) 
        offset <- rep(0, nrow(mf))
    else offset <- mf[, off.col]
    y <- mf[, attr(attr(b$model, "terms"), "response")] - offset
    b$G$y <- c(b$G$y, y)
    b$G$offset <- c(b$G$offset, offset)
    b$G$w <- c(b$G$w, w)
    b$G$n <- nrow(b$model)
    n <- b$G$n
    w <- sqrt(w)
    if (b$AR1.rho != 0) {
        rho <- b$AR1.rho
        ld <- 1/sqrt(1 - rho^2)
        sd <- -rho * ld
        wy <- c(b$yX.last[1], w * y)
        wX <- rbind(b$yX.last[-1], w * X)
        m <- nrow(wX)
        b$yX.last <- c(wy[m], wX[m, ])
        row <- c(1, rep(1:m, rep(2, m))[-c(1, 2 * m)])
        weight <- c(1, rep(c(sd, ld), m - 1))
        stop <- c(1, 1:(m - 1) * 2 + 1)
        if (!is.null(mf$"(AR.start)")) {
            ii <- which(mf$"(AR.start)" == TRUE)
            if (length(ii) > 0) {
                if (ii[1] == 1) 
                  ii <- ii[-1]
                weight[ii * 2 - 2] <- 0
                weight[ii * 2 - 1] <- 1
            }
        }
        wX <- rwMatrix(stop, row, weight, wX)[-1, ]
        wy <- rwMatrix(stop, row, weight, wy)[-1]
        b$qrx <- qr.update(wX, wy, b$qrx$R, b$qrx$f, b$qrx$y.norm2)
    }
    else {
        b$qrx <- qr.update(w * X, w * y, b$qrx$R, b$qrx$f, b$qrx$y.norm2)
    }
    rss.extra <- b$qrx$y.norm2 - sum(b$qrx$f^2)
    if (b$method == "GCV" || b$method == "UBRE") 
        method <- "GCV.Cp"
    else method <- b$method
    if (method == "GCV.Cp") {
        if (b$method == "GCV") 
            scale <- -1
        else scale = b$sig2
        fit <- magic(b$qrx$f, b$qrx$R, b$sp, b$G$S, b$G$off, 
            L = b$G$L, lsp0 = b$G$lsp0, rank = b$G$rank, H = b$G$H, 
            C = matrix(0, 0, ncol(b$qrx$R)), gamma = b$gamma, 
            scale = scale, gcv = (scale <= 0), extra.rss = rss.extra, 
            n.score = n)
        post <- magic.post.proc(b$qrx$R, fit, b$qrx$f * 0 + 1)
        b$y <- b$G$y
        b$offset <- b$G$offset
        b$prior.weights <- b$weights <- b$G$w
    }
    else if (method == "fREML") {
        um <- Sl.Xprep(b$Sl, b$qrx$R)
        lsp0 <- log(b$sp)
        log.phi <- log(b$sig2)
        fit <- fast.REML.fit(um$Sl, um$X, b$qrx$f, rho = lsp0, 
            L = b$G$L, rho.0 = b$G$lsp0, log.phi = log.phi, phi.fixed = !b$scale.estimated, 
            rss.extra = rss.extra, nobs = n, Mp = um$Mp, nt = 1)
        if (b$scale.estimated) 
            scale <- -1
        else scale = b$sig2
        res <- Sl.postproc(b$Sl, fit, um$undrop, b$qrx$R, cov = TRUE, 
            scale = scale, L = b$g$L)
        object <- list(coefficients = res$beta, edf = res$edf, 
            edf1 = res$edf1, edf2 = res$edf2, gcv.ubre = fit$reml, 
            hat = res$hat, outer.info = list(iter = fit$iter, 
                message = fit$conv), optimizer = "fast-REML", 
            rank = ncol(um$X), Ve = NULL, Vp = res$V, Vc = res$Vc, 
            db.drho = fit$d1b, scale.estimated = scale <= 0)
        if (scale <= 0) {
            nsp <- length(fit$rho)
            object$sig2 <- object$scale <- exp(fit$rho[nsp])
            object$sp <- exp(fit$rho[-nsp])
            nsp <- length(fit$rho.full)
            object$full.sp <- exp(fit$rho.full[-nsp])
        }
        else {
            object$sig2 <- object$scale <- scale
            object$sp <- exp(fit$rho)
            object$full.sp <- exp(fit$rho.full)
        }
        if (b$AR1.rho != 0) {
            df <- if (getARs) 
                sum(b$model$"(AR.start)")
            else 1
            object$gcv.ubre <- object$gcv.ubre - (n - df) * log(ld)
        }
        b$G$X <- b$qrx$R
        b$G$dev.extra <- rss.extra
        b$G$pearson.extra <- rss.extra
        b$G$n.true <- n
        b$y <- b$G$y
        b$offset <- b$G$offset
        b$prior.weights <- b$weights <- b$G$w
    }
    else {
        y <- b$G$y
        w <- b$G$w
        offset <- b$G$offset
        b$G$y <- b$qrx$f
        b$G$w <- b$G$y * 0 + 1
        b$G$X <- b$qrx$R
        b$G$n <- length(b$G$y)
        b$G$offset <- b$G$y * 0
        b$G$dev.extra <- rss.extra
        b$G$pearson.extra <- rss.extra
        b$G$n.true <- n
        if (b$scale.estimated) 
            scale <- -1
        else scale = b$sig2
        in.out <- list(sp = b$sp, scale = b$reml.scale)
        object <- gam(G = b$G, method = method, gamma = b$gamma, 
            scale = scale, in.out = in.out)
        if (b$AR1.rho != 0) {
            df <- if (getARs) 
                sum(b$model$"(AR.start)")
            else 1
            object$gcv.ubre <- object$gcv.ubre - (n - df) * log(ld)
        }
        b$offset <- b$G$offset <- offset
        b$prior.weights <- b$weights <- b$G$w <- w
        b$G$n <- n
        b$y <- b$G$y <- y
    }
    if (method == "GCV.Cp") {
        b$coefficients <- fit$b
        b$edf <- post$edf
        b$edf1 <- post$edf1
        b$full.sp <- fit$sp.full
        b$gcv.ubre <- fit$score
        b$hat <- post$hat
        b$mgcv.conv <- fit$gcv.info
        b$optimizer = "magic"
        b$rank <- fit$gcv.info$rank
        b$Ve <- post$Ve
        b$Vp <- post$Vb
        b$sig2 <- b$scale <- fit$scale
        b$sp <- fit$sp
    }
    else {
        b$coefficients <- object$coefficients
        b$edf <- object$edf
        b$edf1 <- object$edf1
        b$full.sp <- object$sp.full
        b$gcv.ubre <- object$gcv.ubre
        b$hat <- object$hat
        b$outer.info <- object$outer.info
        b$rank <- object$rank
        b$Ve <- object$Ve
        b$Vp <- object$Vp
        b$sig2 <- b$scale <- object$sig2
        b$sp <- object$sp
        if (b$AR1.rho != 0) {
            b$gcv.ubre <- b$gcv.ubre - (n - 1) * log(ld)
        }
    }
    b$R <- b$qrx$R
    b$G$X <- NULL
    b$linear.predictors <- as.numeric(predict.gam(b, newdata = b$model, 
        block.size = chunk.size))
    b$fitted.values <- b$linear.predictor
    b$residuals <- sqrt(b$family$dev.resids(b$y, b$fitted.values, 
        b$prior.weights)) * sign(b$y - b$fitted.values)
    b$deviance <- sum(b$residuals^2)
    b$aic <- b$family$aic(b$y, 1, b$fitted.values, b$prior.weights, 
        b$deviance) + 2 * sum(b$edf)
    if (b$AR1.rho != 0) {
        df <- if (getARs) 
            sum(b$model$"(AR.start)")
        else 1
        b$aic <- b$aic + 2 * (n - df) * log(ld)
    }
    b$null.deviance <- sum(b$family$dev.resids(b$y, mean(b$y), 
        b$prior.weights))
    names(b$coefficients) <- names(b$edf) <- cnames
    b
}


Tweedie <- function (p = 1, link = power(0)) 
{
    if (p <= 1 || p > 2) 
        stop("Only 1<p<=2 supported")
    linktemp <- substitute(link)
    if (!is.character(linktemp)) 
        linktemp <- deparse(linktemp)
    okLinks <- c("log", "identity", "sqrt", "inverse")
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
            stop(gettextf("link \"%s\" not available for Tweedie family.", 
                linktemp, collapse = ""), domain = NA)
        }
    }
    variance <- function(mu) mu^p
    dvar <- function(mu) p * mu^(p - 1)
    if (p == 1) 
        d2var <- function(mu) 0 * mu
    else d2var <- function(mu) p * (p - 1) * mu^(p - 2)
    if (p == 1 || p == 2) 
        d3var <- function(mu) 0 * mu
    else d3var <- function(mu) p * (p - 1) * (p - 2) * mu^(p - 
        3)
    validmu <- function(mu) all(mu >= 0)
    dev.resids <- function(y, mu, wt) {
        y1 <- y + (y == 0)
        if (p == 1) 
            theta <- log(y1/mu)
        else theta <- (y1^(1 - p) - mu^(1 - p))/(1 - p)
        if (p == 2) 
            kappa <- log(y1/mu)
        else kappa <- (y^(2 - p) - mu^(2 - p))/(2 - p)
        2 * wt * (y * theta - kappa)
    }
    initialize <- expression({
        n <- rep(1, nobs)
        mustart <- y + 0.1 * (y == 0)
    })
    ls <- function(y, w, n, scale) {
        power <- p
        colSums(w * ldTweedie(y, y, p = power, phi = scale))
    }
    aic <- function(y, n, mu, wt, dev) {
        power <- p
        scale <- dev/sum(wt)
        -2 * sum(ldTweedie(y, mu, p = power, phi = scale)[, 1] * 
            wt) + 2
    }
    if (p == 2) {
        rd <- function(mu, wt, scale) {
            rgamma(mu, shape = 1/scale, scale = mu * scale)
        }
    }
    else {
        rd <- function(mu, wt, scale) {
            rTweedie(mu, p = p, phi = scale)
        }
    }
    structure(list(family = paste("Tweedie(", p, ")", sep = ""), 
        variance = variance, dev.resids = dev.resids, aic = aic, 
        link = linktemp, linkfun = stats$linkfun, linkinv = stats$linkinv, 
        mu.eta = stats$mu.eta, initialize = initialize, validmu = validmu, 
        valideta = stats$valideta, dvar = dvar, d2var = d2var, 
        d3var = d3var, ls = ls, rd = rd, canonical = "none"), 
        class = "family")
}


full.score <- function (sp, G, family, control, gamma, ...) 
{
    if (is.null(G$L)) {
        G$sp <- exp(sp)
    }
    else {
        G$sp <- as.numeric(exp(G$L %*% sp + G$lsp0))
    }
    q <- NCOL(G$X)
    if (is.null(G$H)) 
        G$H <- matrix(0, q, q)
    for (i in 1:length(G$S)) {
        j <- ncol(G$S[[i]])
        off1 <- G$off[i]
        off2 <- off1 + j - 1
        G$H[off1:off2, off1:off2] <- G$H[off1:off2, off1:off2] + 
            G$sp[i] * G$S[[i]]
    }
    G$S <- list()
    G$L <- NULL
    xx <- gam.fit(G, family = family, control = control, gamma = gamma, 
        ...)
    res <- xx$gcv.ubre.dev
    attr(res, "full.gam.object") <- xx
    res
}


smooth.construct.tensor.smooth.spec <- function (object, data, knots) 
{
    inter <- object$inter
    m <- length(object$margin)
    if (inter) {
        object$mc <- if (is.null(object$mc)) 
            rep(TRUE, m)
        else as.logical(object$mc)
    }
    else {
        object$mc <- rep(FALSE, m)
    }
    Xm <- list()
    Sm <- list()
    nr <- r <- d <- array(0, m)
    C <- NULL
    object$plot.me <- TRUE
    mono <- rep(FALSE, m)
    for (i in 1:m) {
        if (!is.null(object$mono) && object$mono != 0) 
            mono[i] <- TRUE
        knt <- dat <- list()
        term <- object$margin[[i]]$term
        for (j in 1:length(term)) {
            dat[[term[j]]] <- data[[term[j]]]
            knt[[term[j]]] <- knots[[term[j]]]
        }
        object$margin[[i]] <- if (object$mc[i]) 
            smoothCon(object$margin[[i]], dat, knt, absorb.cons = TRUE, 
                n = length(dat[[1]]))[[1]]
        else smooth.construct(object$margin[[i]], dat, knt)
        Xm[[i]] <- object$margin[[i]]$X
        if (!is.null(object$margin[[i]]$te.ok)) {
            if (object$margin[[i]]$te.ok == 0) 
                stop("attempt to use unsuitable marginal smooth class")
            if (object$margin[[i]]$te.ok == 2) 
                object$plot.me <- FALSE
        }
        if (length(object$margin[[i]]$S) > 1) 
            stop("Sorry, tensor products of smooths with multiple penalties are not supported.")
        Sm[[i]] <- object$margin[[i]]$S[[1]]
        d[i] <- nrow(Sm[[i]])
        r[i] <- object$margin[[i]]$rank
        nr[i] <- object$margin[[i]]$null.space.dim
        if (!inter && !is.null(object$margin[[i]]$C) && nrow(object$margin[[i]]$C) == 
            0) 
            C <- matrix(0, 0, 0)
    }
    if (sum(mono)) {
        object$np <- FALSE
        km <- which(mono)
        g <- list()
        for (i in 1:length(km)) g[[i]] <- object$margin[[km[i]]]$g.index
        for (i in 1:length(object$margin)) {
            d <- object$margin[[i]]$bs.dim
            for (j in length(km)) if (i != km[j]) 
                g[[j]] <- if (i > km[j]) 
                  rep(g[[j]], each = d)
                else rep(g[[j]], d)
        }
        object$g.index <- as.logical(rowSums(matrix(unlist(g), 
            length(g[[1]]), length(g))))
    }
    XP <- list()
    if (object$np) 
        for (i in 1:m) {
            if (object$margin[[i]]$dim == 1) {
                if (!inherits(object$margin[[i]], c("cs.smooth", 
                  "cr.smooth", "cyclic.smooth", "random.effect"))) {
                  x <- get.var(object$margin[[i]]$term, data)
                  np <- ncol(object$margin[[i]]$X)
                  knt <- if (is.factor(x)) {
                    unique(x)
                  }
                  else {
                    seq(min(x), max(x), length = np)
                  }
                  pd <- data.frame(knt)
                  names(pd) <- object$margin[[i]]$term
                  sv <- if (object$mc[i]) 
                    svd(PredictMat(object$margin[[i]], pd))
                  else svd(Predict.matrix(object$margin[[i]], 
                    pd))
                  if (sv$d[np]/sv$d[1] < .Machine$double.eps^0.66) {
                    XP[[i]] <- NULL
                    warning("reparameterization unstable for margin: not done")
                  }
                  else {
                    XP[[i]] <- sv$v %*% (t(sv$u)/sv$d)
                    object$margin[[i]]$X <- Xm[[i]] <- Xm[[i]] %*% 
                      XP[[i]]
                    Sm[[i]] <- t(XP[[i]]) %*% Sm[[i]] %*% XP[[i]]
                  }
                }
                else XP[[i]] <- NULL
            }
            else XP[[i]] <- NULL
        }
    for (i in 1:m) Sm[[i]] <- Sm[[i]]/eigen(Sm[[i]], symmetric = TRUE, 
        only.values = TRUE)$values[1]
    max.rank <- prod(d)
    r <- max.rank * r/d
    X <- tensor.prod.model.matrix(Xm)
    if (object$mp) {
        S <- tensor.prod.penalties(Sm)
        for (i in m:1) if (object$fx[i]) {
            S[[i]] <- NULL
            r <- r[-i]
        }
    }
    else {
        warning("single penalty tensor product smooths are deprecated and likely to be removed soon")
        S <- Sm[[1]]
        r <- object$margin[[i]]$rank
        if (m > 1) 
            for (i in 2:m) {
                S <- S %x% Sm[[i]]
                r <- r * object$margin[[i]]$rank
            }
        if (sum(object$fx) == m) {
            S <- list()
            object$fixed = TRUE
        }
        else {
            S <- list(S)
            object$fixed = FALSE
        }
        nr <- max.rank - r
        object$bs.dim <- max.rank
    }
    if (!is.null(object$margin[[1]]$xt$dropu) && object$margin[[1]]$xt$dropu) {
        ind <- which(colSums(abs(X)) != 0)
        X <- X[, ind]
        if (!is.null(object$g.index)) 
            object$g.index <- object$g.index[ind]
        for (i in 1:m) {
            if (is.null(object$margin[[i]]$D)) 
                stop("basis not usable with reduced te")
            Sm[[i]] <- object$margin[[i]]$D
        }
        S <- tensor.prod.penalties(Sm)
        for (i in 1:m) {
            D <- S[[i]][rowSums(S[[i]][, -ind]) == 0, ind]
            r[i] <- nrow(D)
            S[[i]] <- crossprod(D)
        }
        object$udrop <- ind
    }
    object$X <- X
    object$S <- S
    if (inter) 
        object$C <- matrix(0, 0, 0)
    else object$C <- C
    object$df <- ncol(X)
    object$null.space.dim <- prod(nr)
    object$rank <- r
    object$XP <- XP
    class(object) <- "tensor.smooth"
    object
}


Predict.matrix.random.effect <- function (object, data) 
{
    X <- model.matrix(object$form, data)
    X
}


model.matrix.gam <- function (object, ...) 
{
    if (!inherits(object, "gam")) 
        stop("`object' is not of class \"gam\"")
    predict(object, type = "lpmatrix", ...)
}


sp.vcov <- function (x) 
{
    if (!inherits(x, "gam")) 
        stop("argument is not a gam object")
    if (x$method %in% c("ML", "P-ML", "REML", "P-REML") && !is.null(x$outer.info$hess)) {
        return(solve(x$outer.info$hess))
    }
    else return(NULL)
}


print.anova.gam <- function (x, digits = max(3, getOption("digits") - 3), ...) 
{
    print(x$family)
    cat("Formula:\n")
    if (is.list(x$formula)) 
        for (i in 1:length(x$formula)) print(x$formula[[i]])
    else print(x$formula)
    if (length(x$pTerms.pv) > 0) {
        cat("\nParametric Terms:\n")
        printCoefmat(x$pTerms.table, digits = digits, signif.stars = FALSE, 
            has.Pvalue = TRUE, na.print = "NA", ...)
    }
    cat("\n")
    if (x$m > 0) {
        cat("Approximate significance of smooth terms:\n")
        printCoefmat(x$s.table, digits = digits, signif.stars = FALSE, 
            has.Pvalue = TRUE, na.print = "NA", ...)
    }
    invisible(x)
}


slanczos <- function (A, k = 10, kl = -1, tol = .Machine$double.eps^0.5, 
    nt = 1) 
{
    if (tol <= 0 || tol > 0.01) 
        stop("silly tolerance supplied")
    k <- round(k)
    kl <- round(kl)
    if (k < 0) 
        stop("argument k must be positive.")
    m <- k + max(0, kl)
    n <- nrow(A)
    if (m < 1) 
        return(list(values = rep(0, 0), vectors = matrix(0, n, 
            0), iter = 0))
    if (n != ncol(A)) 
        stop("A not square")
    if (m > n) 
        stop("Can not have more eigenvalues than nrow(A)")
    oo <- .C(C_Rlanczos, A = as.double(A), U = as.double(rep(0, 
        n * m)), D = as.double(rep(0, m)), n = as.integer(n), 
        m = as.integer(k), ml = as.integer(kl), tol = as.double(tol), 
        nt = as.integer(nt))
    list(values = oo$D, vectors = matrix(oo$U, n, m), iter = oo$n)
}


Predict.matrix <- function (object, data) 
UseMethod("Predict.matrix")


t2 <- function (..., k = NA, bs = "cr", m = NA, d = NA, by = NA, xt = NULL, 
    id = NULL, sp = NULL, full = FALSE, ord = NULL) 
{
    vars <- as.list(substitute(list(...)))[-1]
    dim <- length(vars)
    by.var <- deparse(substitute(by), backtick = TRUE)
    term <- deparse(vars[[1]], backtick = TRUE)
    if (dim > 1) 
        for (i in 2:dim) {
            term[i] <- deparse(vars[[i]], backtick = TRUE)
        }
    for (i in 1:dim) term[i] <- attr(terms(reformulate(term[i])), 
        "term.labels")
    if (sum(is.na(d)) || is.null(d)) {
        n.bases <- dim
        d <- rep(1, dim)
    }
    else {
        d <- round(d)
        ok <- TRUE
        if (sum(d <= 0)) 
            ok <- FALSE
        if (sum(d) != dim) 
            ok <- FALSE
        if (ok) 
            n.bases <- length(d)
        else {
            warning("something wrong with argument d.")
            n.bases <- dim
            d <- rep(1, dim)
        }
    }
    if (sum(is.na(k)) || is.null(k)) 
        k <- 5^d
    else {
        k <- round(k)
        ok <- TRUE
        if (sum(k < 3)) {
            ok <- FALSE
            warning("one or more supplied k too small - reset to default")
        }
        if (length(k) == 1 && ok) 
            k <- rep(k, n.bases)
        else if (length(k) != n.bases) 
            ok <- FALSE
        if (!ok) 
            k <- 5^d
    }
    fx <- FALSE
    xtra <- list()
    if (is.null(xt) || length(xt) == 1) 
        for (i in 1:n.bases) xtra[[i]] <- xt
    else if (length(xt) == n.bases) 
        xtra <- xt
    else stop("xt argument is faulty.")
    if (length(bs) == 1) 
        bs <- rep(bs, n.bases)
    if (length(bs) != n.bases) {
        warning("bs wrong length and ignored.")
        bs <- rep("cr", n.bases)
    }
    bs[d > 1 & (bs == "cr" | bs == "cs" | bs == "ps" | bs == 
        "cp")] <- "tp"
    if (!is.list(m) && length(m) == 1) 
        m <- rep(m, n.bases)
    if (length(m) != n.bases) {
        warning("m wrong length and ignored.")
        m <- rep(0, n.bases)
    }
    if (!is.list(m)) 
        m[m < 0] <- 0
    if (length(unique(term)) != dim) 
        stop("Repeated variables as arguments of a smooth are not permitted")
    j <- 1
    margin <- list()
    for (i in 1:n.bases) {
        j1 <- j + d[i] - 1
        if (is.null(xt)) 
            xt1 <- NULL
        else xt1 <- xtra[[i]]
        stxt <- "s("
        for (l in j:j1) stxt <- paste(stxt, term[l], ",", sep = "")
        stxt <- paste(stxt, "k=", deparse(k[i], backtick = TRUE), 
            ",bs=", deparse(bs[i], backtick = TRUE), ",m=", deparse(m[[i]], 
                backtick = TRUE), ",xt=xt1", ")")
        margin[[i]] <- eval(parse(text = stxt))
        j <- j1 + 1
    }
    if (!is.null(ord)) {
        if (sum(ord %in% 0:n.bases) == 0) {
            ord <- NULL
            warning("ord is wrong. reset to NULL.")
        }
        if (sum(ord < 0) > 0 || sum(ord > n.bases) > 0) 
            warning("ord contains out of range orders (which will be ignored)")
    }
    full.call <- paste("t2(", term[1], sep = "")
    if (dim > 1) 
        for (i in 2:dim) full.call <- paste(full.call, ",", term[i], 
            sep = "")
    label <- paste(full.call, ")", sep = "")
    if (!is.null(id)) {
        if (length(id) > 1) {
            id <- id[1]
            warning("only first element of `id' used")
        }
        id <- as.character(id)
    }
    full <- as.logical(full)
    if (is.na(full)) 
        full <- FALSE
    ret <- list(margin = margin, term = term, by = by.var, fx = fx, 
        label = label, dim = dim, id = id, sp = sp, full = full, 
        ord = ord)
    class(ret) <- "t2.smooth.spec"
    ret
}


influence.gam <- function (model, ...) 
{
    model$hat
}


notLog2 <- function (x, d = .Options$mgcv.vc.logrange, b = 1/d) 
{
    x <- log(x)/d
    x <- pmin(1, x)
    x <- pmax(-1, x)
    asin(x)/b
}


smooth.construct <- function (object, data, knots) 
UseMethod("smooth.construct")


smooth.construct.tp.smooth.spec <- function (object, data, knots) 
{
    shrink <- attr(object, "shrink")
    xtra <- list()
    if (is.null(object$xt$max.knots)) 
        xtra$max.knots <- 2000
    else xtra$max.knots <- object$xt$max.knots
    if (is.null(object$xt$seed)) 
        xtra$seed <- 1
    else xtra$seed <- object$xt$seed
    x <- array(0, 0)
    shift <- array(0, object$dim)
    for (i in 1:object$dim) {
        xx <- data[[object$term[i]]]
        shift[i] <- mean(xx)
        xx <- xx - shift[i]
        if (i == 1) 
            n <- length(xx)
        else if (n != length(xx)) 
            stop("arguments of smooth not same dimension")
        x <- c(x, xx)
    }
    if (is.null(knots)) {
        knt <- 0
        nk <- 0
    }
    else {
        knt <- array(0, 0)
        for (i in 1:object$dim) {
            dum <- knots[[object$term[i]]] - shift[i]
            if (is.null(dum)) {
                knt <- 0
                nk <- 0
                break
            }
            knt <- c(knt, dum)
            nk0 <- length(dum)
            if (i > 1 && nk != nk0) 
                stop("components of knots relating to a single smooth must be of same length")
            nk <- nk0
        }
    }
    if (nk > n) {
        nk <- 0
        warning("more knots than data in a tp term: knots ignored.")
    }
    if (nk == 0 && n > xtra$max.knots) {
        xu <- uniquecombs(matrix(x, n, object$dim))
        nu <- nrow(xu)
        if (nu > xtra$max.knots) {
            seed <- try(get(".Random.seed", envir = .GlobalEnv), 
                silent = TRUE)
            if (inherits(seed, "try-error")) {
                runif(1)
                seed <- get(".Random.seed", envir = .GlobalEnv)
            }
            kind <- RNGkind(NULL)
            RNGkind("default", "default")
            set.seed(xtra$seed)
            nk <- xtra$max.knots
            ind <- sample(1:nu, nk, replace = FALSE)
            knt <- as.numeric(xu[ind, ])
            RNGkind(kind[1], kind[2])
            assign(".Random.seed", seed, envir = .GlobalEnv)
        }
    }
    object$p.order[is.na(object$p.order)] <- 0
    M <- null.space.dimension(object$dim, object$p.order[1])
    if (length(object$p.order) > 1 && object$p.order[2] == 0) 
        object$drop.null <- M
    else object$drop.null <- 0
    def.k <- c(8, 27, 100)
    dd <- min(object$dim, length(def.k))
    if (object$bs.dim[1] < 0) 
        object$bs.dim <- M + def.k[dd]
    k <- object$bs.dim
    if (k < M + 1) {
        k <- M + 1
        object$bs.dim <- k
        warning("basis dimension, k, increased to minimum possible\n")
    }
    X <- array(0, n * k)
    S <- array(0, k * k)
    UZ <- array(0, (n + M) * k)
    Xu <- x
    C <- array(0, k)
    nXu <- 0
    oo <- .C(C_construct_tprs, as.double(x), as.integer(object$dim), 
        as.integer(n), as.double(knt), as.integer(nk), as.integer(object$p.order[1]), 
        as.integer(object$bs.dim), X = as.double(X), S = as.double(S), 
        UZ = as.double(UZ), Xu = as.double(Xu), n.Xu = as.integer(nXu), 
        C = as.double(C))
    object$X <- matrix(oo$X, n, k)
    object$S <- list()
    if (!object$fixed) {
        object$S[[1]] <- matrix(oo$S, k, k)
        object$S[[1]] <- (object$S[[1]] + t(object$S[[1]]))/2
        if (!is.null(shrink)) {
            es <- eigen(object$S[[1]], symmetric = TRUE)
            es$values[(k - M + 1):k] <- es$values[k - M] * shrink
            object$S[[1]] <- es$vectors %*% (as.numeric(es$values) * 
                t(es$vectors))
        }
    }
    UZ.len <- (oo$n.Xu + M) * k
    object$UZ <- matrix(oo$UZ[1:UZ.len], oo$n.Xu + M, k)
    Xu.len <- oo$n.Xu * object$dim
    object$Xu <- matrix(oo$Xu[1:Xu.len], oo$n.Xu, object$dim)
    object$df <- object$bs.dim
    object$shift <- shift
    if (!is.null(shrink)) 
        M <- 0
    object$rank <- k - M
    object$null.space.dim <- M
    if (object$drop.null > 0) {
        ind <- 1:(k - M)
        if (FALSE) {
            np <- nat.param(object$X, object$S[[1]], rank = k - 
                M, type = 0)
            object$P <- np$P
            object$S[[1]] <- diag(np$D)
            object$X <- np$X[, ind]
        }
        else {
            object$S[[1]] <- object$S[[1]][ind, ind]
            object$X <- object$X[, ind]
            object$cmX <- colMeans(object$X)
            object$X <- sweep(object$X, 2, object$cmX)
        }
        object$null.space.dim <- 0
        object$df <- object$df - M
        object$bs.dim <- object$bs.dim - M
        object$C <- matrix(0, 0, ncol(object$X))
    }
    class(object) <- "tprs.smooth"
    object
}


extract.lme.cov <- function (b, data, start.level = 1) 
{
    if (!inherits(b, "lme")) 
        stop("object does not appear to be of class lme")
    grps <- nlme::getGroups(b)
    n <- length(grps)
    if (is.null(b$modelStruct$varStruct)) 
        w <- rep(b$sigma, n)
    else {
        w <- 1/nlme::varWeights(b$modelStruct$varStruct)
        group.name <- names(b$groups)
        order.txt <- paste("ind<-order(data[[\"", group.name[1], 
            "\"]]", sep = "")
        if (length(b$groups) > 1) 
            for (i in 2:length(b$groups)) order.txt <- paste(order.txt, 
                ",data[[\"", group.name[i], "\"]]", sep = "")
        order.txt <- paste(order.txt, ")")
        eval(parse(text = order.txt))
        w[ind] <- w
        w <- w * b$sigma
    }
    if (is.null(b$modelStruct$corStruct)) 
        V <- diag(n)
    else {
        c.m <- nlme::corMatrix(b$modelStruct$corStruct)
        if (!is.list(c.m)) 
            V <- c.m
        else {
            V <- matrix(0, n, n)
            gr.name <- names(c.m)
            n.g <- length(c.m)
            j0 <- 1
            ind <- ii <- 1:n
            for (i in 1:n.g) {
                j1 <- j0 + nrow(c.m[[i]]) - 1
                V[j0:j1, j0:j1] <- c.m[[i]]
                ind[j0:j1] <- ii[grps == gr.name[i]]
                j0 <- j1 + 1
            }
            V[ind, ] <- V
            V[, ind] <- V
        }
    }
    V <- as.vector(w) * t(as.vector(w) * V)
    X <- list()
    grp.dims <- b$dims$ncol
    Zt <- model.matrix(b$modelStruct$reStruct, data)
    cov <- as.matrix(b$modelStruct$reStruct)
    i.col <- 1
    n.levels <- length(b$groups)
    Z <- matrix(0, n, 0)
    if (start.level <= n.levels) {
        for (i in 1:(n.levels - start.level + 1)) {
            if (length(levels(b$groups[[n.levels - i + 1]])) == 
                1) {
                X[[1]] <- matrix(rep(1, nrow(b$groups)))
            }
            else {
                X[[1]] <- model.matrix(~b$groups[[n.levels - 
                  i + 1]] - 1, contrasts.arg = c("contr.treatment", 
                  "contr.treatment"))
            }
            X[[2]] <- Zt[, i.col:(i.col + grp.dims[i] - 1), drop = FALSE]
            i.col <- i.col + grp.dims[i]
            Z <- cbind(Z, tensor.prod.model.matrix(X))
        }
        Vr <- matrix(0, ncol(Z), ncol(Z))
        start <- 1
        for (i in 1:(n.levels - start.level + 1)) {
            k <- n.levels - i + 1
            for (j in 1:b$dims$ngrps[i]) {
                stop <- start + ncol(cov[[k]]) - 1
                Vr[start:stop, start:stop] <- cov[[k]]
                start <- stop + 1
            }
        }
        Vr <- Vr * b$sigma^2
        V <- V + Z %*% Vr %*% t(Z)
    }
    V
}


`sdiag<-` <- function (A, k = 0, value) 
{
    p <- ncol(A)
    n <- nrow(A)
    if (k > p - 1 || -k > n - 1) 
        return()
    if (k >= 0) {
        i <- 1:n
        j <- (k + 1):p
    }
    else {
        i <- (-k + 1):n
        j <- 1:p
    }
    if (length(i) > length(j)) 
        i <- i[1:length(j)]
    else j <- j[1:length(i)]
    ii <- i + (j - 1) * n
    A[ii] <- value
    A
}


pdTens <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent())) 
{
    object <- numeric(0)
    class(object) <- c("pdTens", "pdMat")
    nlme::pdConstruct(object, value, form, nam, data)
}


place.knots <- function (x, nk) 
{
    x <- sort(unique(x))
    n <- length(x)
    if (nk > n) 
        stop("more knots than unique data values is not allowed")
    if (nk < 2) 
        stop("too few knots")
    if (nk == 2) 
        return(range(x))
    delta <- (n - 1)/(nk - 1)
    lbi <- floor(delta * 1:(nk - 2)) + 1
    frac <- delta * 1:(nk - 2) + 1 - lbi
    x.shift <- x[-1]
    knot <- array(0, nk)
    knot[nk] <- x[n]
    knot[1] <- x[1]
    knot[2:(nk - 1)] <- x[lbi] * (1 - frac) + x.shift[lbi] * 
        frac
    knot
}


multinom <- function (K = 1) 
{
    if (K < 1) 
        stop("number of categories must be at least 2")
    stats <- list()
    for (i in 1:K) {
        stats[[i]] <- make.link("identity")
        fam <- structure(list(link = "identity", canonical = "none", 
            linkfun = stats[[i]]$linkfun, mu.eta = stats[[i]]$mu.eta), 
            class = "family")
        fam <- fix.family.link(fam)
        stats[[i]]$d2link <- fam$d2link
        stats[[i]]$d3link <- fam$d3link
        stats[[i]]$d4link <- fam$d4link
    }
    residuals <- function(object, type = c("deviance")) {
        type <- match.arg(type)
        p <- object$family$predict(object$family, eta = object$linear.predictors)[[1]]
        pc <- apply(p, 1, function(x) which(max(x) == x)[1]) - 
            1
        n <- length(pc)
        sgn <- rep(-1, n)
        sgn[pc == object$y] <- 1
        sgn * sqrt(-2 * log(pmax(.Machine$double.eps, p[1:n + 
            object$y * n])))
    }
    predict <- function(family, se = FALSE, eta = NULL, y = NULL, 
        X = NULL, beta = NULL, off = NULL, Vb = NULL) {
        if (is.null(eta)) {
            lpi <- attr(X, "lpi")
            if (is.null(lpi)) {
                lpi <- list(1:ncol(X))
            }
            K <- length(lpi)
            eta <- matrix(0, nrow(X), K)
            if (se) {
                ve <- matrix(0, nrow(X), K)
                ce <- matrix(0, nrow(X), K * (K - 1)/2)
            }
            for (i in 1:K) {
                Xi <- X[, lpi[[i]], drop = FALSE]
                eta[, i] <- Xi %*% beta[lpi[[i]]]
                if (se) {
                  ve[, i] <- drop(pmax(0, rowSums((Xi %*% Vb[lpi[[i]], 
                    lpi[[i]]]) * Xi)))
                  ii <- 0
                  if (i < K) 
                    for (j in (i + 1):K) {
                      ii <- ii + 1
                      ce[, ii] <- drop(pmax(0, rowSums((Xi %*% 
                        Vb[lpi[[i]], lpi[[j]]]) * X[, lpi[[j]]])))
                    }
                }
            }
        }
        else {
            se <- FALSE
        }
        gamma <- cbind(1, exp(eta))
        beta <- rowSums(gamma)
        gamma <- gamma/beta
        vp <- gamma * 0
        if (se) {
            for (j in 1:(K + 1)) {
                if (j == 1) 
                  dp <- -gamma[, -1, drop = FALSE]/beta
                else {
                  dp <- -gamma[, j] * gamma[, -1, drop = FALSE]
                  dp[, j - 1] <- gamma[, j] * (1 - gamma[, j])
                }
                vp[, j] <- rowSums(dp^2 * ve)
                ii <- 0
                for (i in 1:K) if (i < K) 
                  for (k in (i + 1):K) {
                    ii <- ii + 1
                    vp[, j] <- vp[, j] + 2 * dp[, i] * dp[, k] * 
                      ce[, ii]
                  }
                vp[, j] <- sqrt(pmax(0, vp[, j]))
            }
            return(list(fit = gamma, se.fit = vp))
        }
        list(fit = gamma)
    }
    postproc <- expression({
        multinom <- list()
        object$y <- round(object$y)
        multinom$nj <- tabulate(object$y + 1)
        multinom$n <- sum(multinom$nj)
        multinom$K <- length(multinom$nj) - 1
        multinom$gamma <- c(1, solve(diag(multinom$n/multinom$nj[-1], 
            multinom$K) - matrix(1, multinom$K, multinom$K), 
            rep(1, multinom$K)))
        multinom$gamma <- log(multinom$gamma/sum(multinom$gamma))
        object$null.deviance <- -2 * sum(multinom$gamma[object$y + 
            1])
    })
    ll <- function(y, X, coef, wt, family, deriv = 0, d1b = 0, 
        d2b = 0, Hp = NULL, rank = 0, fh = NULL, D = NULL, eta = NULL) {
        n <- length(y)
        if (is.null(eta)) {
            return.l <- FALSE
            jj <- attr(X, "lpi")
            K <- length(jj)
            eta <- matrix(1, n, K + 1)
            for (i in 1:K) eta[, i + 1] <- X[, jj[[i]], drop = FALSE] %*% 
                coef[jj[[i]]]
        }
        else {
            l2 <- 0
            K <- ncol(eta)
            eta <- cbind(1, eta)
            return.l <- TRUE
        }
        if (K != family$nlp) 
            stop("number of linear predictors doesn't match")
        y <- round(y)
        if (min(y) < 0 || max(y) > K) 
            stop("response not in 0 to number of predictors + 1")
        ee <- exp(eta[, -1, drop = FALSE])
        beta <- 1 + rowSums(ee)
        alpha <- log(beta)
        l0 <- eta[1:n + y * n] - alpha
        l <- sum(l0)
        l1 <- matrix(0, n, K)
        if (deriv > 0) {
            for (i in 1:K) l1[, i] <- ee[, i]/beta
            l2 <- matrix(0, n, K * (K + 1)/2)
            ii <- 0
            b2 <- beta^2
            for (i in 1:K) for (j in i:K) {
                ii <- ii + 1
                l2[, ii] <- if (i == j) 
                  -l1[, i] + ee[, i]^2/b2
                else (ee[, i] * ee[, j])/b2
            }
            for (i in 1:K) l1[, i] <- as.numeric(y == i) - l1[, 
                i]
        }
        l3 <- l4 <- 0
        tri <- family$tri
        if (deriv > 1) {
            l3 <- matrix(0, n, (K * (K + 3) + 2) * K/6)
            ii <- 0
            b3 <- b2 * beta
            for (i in 1:K) for (j in i:K) for (k in j:K) {
                ii <- ii + 1
                if (i == j && j == k) {
                  l3[, ii] <- l2[, tri$i2[i, i]] + 2 * ee[, i]^2/b2 - 
                    2 * ee[, i]^3/b3
                }
                else if (i != j && j != k & i != k) {
                  l3[, ii] <- -2 * (ee[, i] * ee[, j] * ee[, 
                    k])/b3
                }
                else {
                  kk <- if (i == j) 
                    k
                  else j
                  l3[, ii] <- l2[, tri$i2[i, kk]] - 2 * (ee[, 
                    i] * ee[, j] * ee[, k])/b3
                }
            }
        }
        if (deriv > 3) {
            l4 <- matrix(0, n, (6 + K * 11 + K^2 * 6 + K^3) * 
                K/24)
            ii <- 0
            b4 <- b3 * beta
            for (i in 1:K) for (j in i:K) for (k in j:K) for (l in k:K) {
                ii <- ii + 1
                uni <- unique(c(i, j, k, l))
                nun <- length(uni)
                if (nun == 1) {
                  l4[, ii] <- l3[, tri$i3[i, i, i]] + 4 * ee[, 
                    i]^2/b2 - 10 * ee[, i]^3/b3 + 6 * ee[, i]^4/b4
                }
                else if (nun == 4) {
                  l4[, ii] <- 6 * ee[, i] * ee[, j] * ee[, k] * 
                    ee[, l]/b4
                }
                else if (nun == 3) {
                  l4[, ii] <- l3[, tri$i3[uni[1], uni[2], uni[3]]] + 
                    6 * ee[, i] * ee[, j] * ee[, k] * ee[, l]/b4
                }
                else if (sum(uni[1] == c(i, j, k, l)) == 2) {
                  l4[, ii] <- l3[, tri$i3[uni[1], uni[2], uni[2]]] - 
                    2 * ee[, uni[1]]^2 * ee[, uni[2]]/b3 + 6 * 
                    ee[, i] * ee[, j] * ee[, k] * ee[, l]/b4
                }
                else {
                  if (sum(uni[1] == c(i, j, k, l)) == 1) 
                    uni <- uni[2:1]
                  l4[, ii] <- l3[, tri$i3[uni[1], uni[1], uni[2]]] - 
                    4 * ee[, uni[1]]^2 * ee[, uni[2]]/b3 + 6 * 
                    ee[, i] * ee[, j] * ee[, k] * ee[, l]/b4
                }
            }
        }
        if (return.l) 
            return(list(l = l0, l1 = l1, l2 = l2, l3 = l3, l4 = l4))
        if (deriv) {
            ret <- gamlss.gH(X, jj, l1, l2, tri$i2, l3 = l3, 
                i3 = tri$i3, l4 = l4, i4 = tri$i4, d1b = d1b, 
                d2b = d2b, deriv = deriv - 1, fh = fh, D = D)
        }
        else ret <- list()
        ret$l <- l
        ret
    }
    rd <- function(mu, wt, scale) {
        p <- exp(cbind(0, mu))
        p <- p/rowSums(p)
        cp <- t(apply(p, 1, cumsum))
        apply(cp, 1, function(x) min(which(x > runif(1)))) - 
            1
    }
    initialize <- expression({
        n <- rep(1, nobs)
        use.unscaled <- if (!is.null(attr(E, "use.unscaled"))) TRUE else FALSE
        if (is.null(start)) {
            jj <- attr(x, "lpi")
            start <- rep(0, ncol(x))
            for (k in 1:length(jj)) {
                yt1 <- 6 * as.numeric(y == k) - 3
                x1 <- x[, jj[[k]], drop = FALSE]
                e1 <- E[, jj[[k]], drop = FALSE]
                if (use.unscaled) {
                  qrx <- qr(rbind(x1, e1))
                  x1 <- rbind(x1, e1)
                  startji <- qr.coef(qr(x1), c(yt1, rep(0, nrow(E))))
                  startji[!is.finite(startji)] <- 0
                } else startji <- pen.reg(x1, e1, yt1)
                start[jj[[k]]] <- startji
            }
        }
    })
    structure(list(family = "multinom", ll = ll, link = NULL, 
        nlp = round(K), rd = rd, tri = trind.generator(K), initialize = initialize, 
        postproc = postproc, residuals = residuals, predict = predict, 
        linfo = stats, d2link = 1, d3link = 1, d4link = 1, ls = 1, 
        available.derivs = 2), class = c("general.family", "extended.family", 
        "family"))
}


smooth.construct.so.smooth.spec <- function (object, data, knots) 
{
    if (is.null(knots)) 
        stop("knots must be specified for soap")
    if (object$dim != 2) 
        stop("soap films are bivariate only")
    x <- data[[object$term[1]]]
    y <- data[[object$term[2]]]
    knt <- list(x = knots[[object$term[1]]], y = knots[[object$term[2]]])
    if (length(knt$x) < 1) 
        stop("need at least one interior knot")
    bnd <- object$xt$bnd
    if (is.null(bnd)) 
        stop("can't soap smooth without a boundary")
    if (!inherits(bnd, "list")) 
        stop("bnd must be a list of boundary loops")
    for (i in 1:length(bnd)) {
        nm <- names(bnd[[i]])
        ind <- nm == object$term[1]
        if (sum(ind) != 1) 
            stop("faulty bnd")
        names(bnd[[i]])[ind] <- "x"
        ind <- nm == object$term[2]
        if (sum(ind) != 1) 
            stop("faulty bnd")
        names(bnd[[i]])[ind] <- "y"
    }
    if (length(object$bs.dim) == 1) 
        k <- rep(object$bs.dim, length(bnd))
    else {
        if (length(object$bs.dim) == length(bnd)) 
            k <- object$bs.dim
        else stop("k and bnd lengths are inconsistent")
    }
    if (is.null(object$xt$nmax)) 
        nmax <- 200
    else nmax <- object$xt$nmax
    sd <- setup.soap(bnd, knots = knt, nmax = nmax, k = k, bndSpec = object$xt$bndSpec)
    b <- soap.basis(sd, x, y, film = TRUE, wiggly = TRUE, penalty = TRUE)
    if (sum(is.na(b$X)) > 0) 
        stop("data outside soap boundary")
    ns.dim <- 0
    n <- length(sd$bc)
    if (n > 0) 
        for (i in 1:n) if (sd$bc[[i]]$free.bound) 
            ns.dim <- ns.dim + sd$bc[[i]]$bsm$null.space.dim
    object$null.space.dim <- ns.dim
    need.con <- TRUE
    for (i in 1:length(sd$bc)) if (!sd$bc[[i]]$free.bound) 
        need.con <- FALSE
    irng <- 1/as.numeric(apply(b$X, 2, max) - apply(b$X, 2, min))
    b$X <- t(t(b$X) * irng)
    for (i in 1:length(b$S)) {
        a <- irng[b$off[i]:(b$off[i] + ncol(b$S[[i]]) - 1)]
        b$S[[i]] <- diag(a) %*% b$S[[i]] %*% diag(a)
    }
    object$irng <- irng
    object$X <- b$X
    attr(object$X, "offset") <- b$offset
    if (!object$fixed) {
        S <- list()
        n <- ncol(object$X)
        for (i in 1:length(b$S)) {
            S[[i]] <- matrix(0, n, n)
            m <- ncol(b$S[[i]])
            ind <- b$off[i]:(b$off[i] + m - 1)
            S[[i]][ind, ind] <- b$S[[i]]
        }
        object$S <- S
    }
    rr <- ncol(b$S[[1]]) - 1
    if (length(b$S) > 1) 
        for (i in 2:length(b$S)) rr <- c(rr, ncol(b$S[[i]]) - 
            1)
    rr[length(rr)] <- rr[length(rr)] + 1
    object$rank <- rr
    if (!need.con) 
        object$C <- matrix(0, 0, ncol(object$X))
    object$df <- ncol(object$X)
    for (i in 1:length(sd$bc)) {
        sd$bc[[i]]$bsm <- sd$bc[[i]]$S <- NULL
    }
    object$sd <- sd
    class(object) <- "soap.film"
    object
}


Predict.matrix.soap.film <- function (object, data) 
{
    x <- get.var(object$term[1], data)
    y <- get.var(object$term[2], data)
    b <- soap.basis(object$sd, x, y, film = TRUE, wiggly = TRUE, 
        penalty = FALSE)
    X <- t(object$irng * t(b$X))
    attr(X, "offset") <- b$offset
    X
}


smooth.construct.cc.smooth.spec <- function (object, data, knots) 
{
    getBD <- function(x) {
        n <- length(x)
        h <- x[2:n] - x[1:(n - 1)]
        n <- n - 1
        D <- B <- matrix(0, n, n)
        B[1, 1] <- (h[n] + h[1])/3
        B[1, 2] <- h[1]/6
        B[1, n] <- h[n]/6
        D[1, 1] <- -(1/h[1] + 1/h[n])
        D[1, 2] <- 1/h[1]
        D[1, n] <- 1/h[n]
        for (i in 2:(n - 1)) {
            B[i, i - 1] <- h[i - 1]/6
            B[i, i] <- (h[i - 1] + h[i])/3
            B[i, i + 1] <- h[i]/6
            D[i, i - 1] <- 1/h[i - 1]
            D[i, i] <- -(1/h[i - 1] + 1/h[i])
            D[i, i + 1] <- 1/h[i]
        }
        B[n, n - 1] <- h[n - 1]/6
        B[n, n] <- (h[n - 1] + h[n])/3
        B[n, 1] <- h[n]/6
        D[n, n - 1] <- 1/h[n - 1]
        D[n, n] <- -(1/h[n - 1] + 1/h[n])
        D[n, 1] <- 1/h[n]
        list(B = B, D = D)
    }
    if (length(object$term) != 1) 
        stop("Basis only handles 1D smooths")
    x <- data[[object$term]]
    if (object$bs.dim < 0) 
        object$bs.dim <- 10
    if (object$bs.dim < 4) {
        object$bs.dim <- 4
        warning("basis dimension, k, increased to minimum possible\n")
    }
    nk <- object$bs.dim
    k <- knots[[object$term]]
    if (is.null(k)) 
        k <- place.knots(x, nk)
    if (length(k) == 2) {
        k <- place.knots(c(k, x), nk)
    }
    if (length(k) != nk) 
        stop("number of supplied knots != k for a cc smooth")
    um <- getBD(k)
    BD <- solve(um$B, um$D)
    if (!object$fixed) {
        object$S <- list(t(um$D) %*% BD)
        object$S[[1]] <- (object$S[[1]] + t(object$S[[1]]))/2
    }
    object$BD <- BD
    object$xp <- k
    X <- Predict.matrix.cyclic.smooth(object, data)
    object$X <- X
    object$rank <- ncol(X) - 1
    object$df <- object$bs.dim - 1
    object$null.space.dim <- 1
    class(object) <- "cyclic.smooth"
    object
}


smooth.construct.bs.smooth.spec <- function (object, data, knots) 
{
    if (length(object$p.order) == 1) 
        m <- c(object$p.order, max(0, object$p.order - 1))
    else m <- object$p.order
    if (is.na(m[1])) 
        if (is.na(m[2])) 
            m <- c(3, 2)
        else m[1] <- m[2] + 1
    if (is.na(m[2])) 
        m[2] <- max(0, m[1] - 1)
    object$m <- object$p.order <- m
    if (object$bs.dim < 0) 
        object$bs.dim <- max(10, m[1])
    nk <- object$bs.dim - m[1] + 1
    if (nk <= 0) 
        stop("basis dimension too small for b-spline order")
    if (length(object$term) != 1) 
        stop("Basis only handles 1D smooths")
    x <- data[[object$term]]
    k <- knots[[object$term]]
    if (is.null(k)) {
        xl <- min(x)
        xu <- max(x)
    }
    else if (length(k) == 2) {
        xl <- min(k)
        xu <- max(k)
        if (xl > min(x) || xu < max(x)) 
            stop("knot range does not include data")
    }
    if (is.null(k) || length(k) == 2) {
        xr <- xu - xl
        xl <- xl - xr * 0.001
        xu <- xu + xr * 0.001
        dx <- (xu - xl)/(nk - 1)
        k <- seq(xl - dx * (m[1]), xu + dx * (m[1]), length = nk + 
            2 * m[1])
    }
    else {
        if (length(k) != nk + 2 * m[1]) 
            stop(paste("there should be ", nk + 2 * m[1], " supplied knots"))
    }
    if (is.null(object$deriv)) 
        object$deriv <- 0
    object$X <- splines::spline.des(k, x, m[1] + 1, x * 0 + object$deriv)$design
    if (!is.null(k)) {
        if (sum(colSums(object$X) == 0) > 0) 
            warning("there is *no* information about some basis coefficients")
    }
    if (length(unique(x)) < object$bs.dim) 
        warning("basis dimension is larger than number of unique covariates")
    object$knots <- k
    class(object) <- "Bspline.smooth"
    k0 <- k[m[1] + 1:nk]
    object$deriv <- m[2]
    pord <- m[1] - m[2]
    if (pord < 0) 
        stop("requested non-existent derivative in B-spline penalty")
    h <- diff(k0)
    if (pord == 0) 
        k1 <- (k0[2:nk] + k0[1:(nk - 1)])/2
    else {
        h1 <- rep(h/pord, each = pord)
        k1 <- cumsum(c(k0[1], h1))
    }
    dat <- data.frame(k1)
    names(dat) <- object$term
    D <- Predict.matrix.Bspline.smooth(object, dat)
    object$deriv <- NULL
    if (pord == 0) {
        object$D <- sqrt(h) * D
    }
    else {
        P <- solve(matrix(rep(seq(-1, 1, length = pord + 1), 
            pord + 1)^rep(0:pord, each = pord + 1), pord + 1, 
            pord + 1))
        i1 <- rep(1:(pord + 1), pord + 1) + rep(1:(pord + 1), 
            each = pord + 1)
        H <- matrix((1 + (-1)^(i1 - 2))/(i1 - 1), pord + 1, pord + 
            1)
        W1 <- t(P) %*% H %*% P
        h <- h/2
        ld0 <- rep(sdiag(W1), length(h)) * rep(h, each = pord + 
            1)
        i1 <- c(rep(1:pord, length(h)) + rep(0:(length(h) - 1) * 
            (pord + 1), each = pord), length(ld0))
        ld <- ld0[i1]
        i0 <- 1:(length(h) - 1) * pord + 1
        i2 <- 1:(length(h) - 1) * (pord + 1)
        ld[i0] <- ld[i0] + ld0[i2]
        B <- matrix(0, pord + 1, length(ld))
        B[1, ] <- ld
        for (k in 1:pord) {
            diwk <- sdiag(W1, k)
            ind <- 1:(length(ld) - k)
            B[k + 1, ind] <- (rep(h, each = pord) * rep(c(diwk, 
                rep(0, k - 1)), length(h)))[ind]
        }
        B <- bandchol(B)
        D1 <- B[1, ] * D
        for (k in 1:pord) {
            ind <- 1:(nrow(D) - k)
            D1[ind, ] <- D1[ind, ] + B[k + 1, ind] * D[ind + 
                k, ]
        }
        object$D <- D1
    }
    object$S <- list(crossprod(object$D))
    object$rank <- object$bs.dim - m[2]
    object$null.space.dim <- m[2]
    object
}


rig <- function (n, mean, scale) 
{
    if (length(n) > 1) 
        n <- length(n)
    y <- rnorm(n)^2
    mu2 <- 0 * y + mean^2
    x <- mean + 0.5 * scale * (mu2 * y - mean * sqrt(4 * mean * 
        y/scale + mu2 * y^2))
    ind <- runif(n) > mean/(mean + x)
    x[ind] <- mu2[ind]/x[ind]
    x
}


exclude.too.far <- function (g1, g2, d1, d2, dist) 
{
    mig <- min(g1)
    d1 <- d1 - mig
    g1 <- g1 - mig
    mag <- max(g1)
    d1 <- d1/mag
    g1 <- g1/mag
    mig <- min(g2)
    d2 <- d2 - mig
    g2 <- g2 - mig
    mag <- max(g2)
    d2 <- d2/mag
    g2 <- g2/mag
    n <- length(g1)
    m <- length(d1)
    if (length(g2) != n) 
        stop("grid vectors are different lengths")
    if (m != length(d2)) 
        stop("data vectors are of different lengths")
    if (dist < 0) 
        stop("supplied dist negative")
    distance <- array(0, n)
    o <- .C(C_MinimumSeparation, x = as.double(cbind(g1, g2)), 
        n = as.integer(n), d = as.integer(2), t = as.double(cbind(d1, 
            d2)), m = as.integer(m), distance = as.double(distance))
    res <- rep(FALSE, n)
    res[o$distance > dist] <- TRUE
    res
}


magic.post.proc <- function (X, object, w = NULL) 
{
    V <- tcrossprod(object$rV)
    if (!is.null(w)) {
        if (is.matrix(w)) 
            WX <- X <- w %*% X
        else WX <- as.vector(w) * X
    }
    else {
        WX <- X
    }
    M <- WX %*% V
    XWX <- crossprod(object$R)
    F <- Ve <- V %*% XWX
    edf1 <- rowSums(t(Ve) * Ve)
    Ve <- Ve %*% V * object$scale
    B <- X * M
    rm(M)
    hat <- rowSums(B)
    edf <- colSums(B)
    Vb <- V * object$scale
    rm(V)
    list(Ve = Ve, Vb = Vb, hat = hat, edf = edf, edf1 = 2 * edf - 
        edf1, F = F)
}


gam2derivative <- function (lsp, args, ...) 
{
    reml <- args$scoreType %in% c("REML", "P-REML", "ML", "P-ML")
    if (!is.null(args$L)) {
        lsp <- args$L %*% lsp + args$lsp0
    }
    b <- gam.fit3(x = args$X, y = args$y, sp = lsp, Eb = args$Eb, 
        UrS = args$UrS, offset = args$offset, U1 = args$U1, Mp = args$Mp, 
        family = args$family, weights = args$w, deriv = 1, control = args$control, 
        gamma = args$gamma, scale = args$scale, scoreType = args$scoreType, 
        null.coef = args$null.coef, n.true = args$n.true, ...)
    if (reml) {
        ret <- b$REML1
    }
    else if (args$scoreType == "GACV") {
        ret <- b$GACV1
    }
    else if (args$scoreType == "UBRE") {
        ret <- b$UBRE1
    }
    else {
        ret <- b$GCV1
    }
    if (!is.null(args$L)) 
        ret <- t(args$L) %*% ret
    ret
}


gam.vcomp <- function (x, rescale = TRUE, conf.lev = 0.95) 
{
    if (!inherits(x, "gam")) 
        stop("requires an object of class gam")
    if (!is.null(x$reml.scale) && is.finite(x$reml.scale)) 
        scale <- x$reml.scale
    else scale <- x$sig2
    if (length(x$sp) == 0) 
        return
    if (rescale) {
        m <- length(x$smooth)
        if (is.null(x$paraPen)) {
            k <- 1
            if (is.null(x$full.sp)) 
                kf <- -1
            else kf <- 1
        }
        else {
            k <- sum(x$paraPen$sp < 0) + 1
            if (is.null(x$full.sp)) 
                kf <- -1
            else kf <- length(x$paraPen$full.sp.names) + 1
        }
        idx <- rep("", 0)
        idxi <- rep(0, 0)
        if (m > 0) 
            for (i in 1:m) {
                if (!is.null(x$smooth[[i]]$id)) {
                  if (x$smooth[[i]]$id %in% idx) {
                    ok <- FALSE
                  }
                  else {
                    idx <- c(idx, x$smooth[[i]]$id)
                    idxi <- c(idxi, i)
                    ok <- TRUE
                  }
                }
                else {
                  ok <- TRUE
                }
                if (ok) {
                  if (length(x$smooth[[i]]$S.scale) != length(x$smooth[[i]]$S)) 
                    warning("S.scale vector doesn't match S list - please report to maintainer")
                  for (j in 1:length(x$smooth[[i]]$S.scale)) {
                    if (x$smooth[[i]]$sp[j] < 0) {
                      x$sp[k] <- x$sp[k]/x$smooth[[i]]$S.scale[j]
                      k <- k + 1
                      if (kf > 0) {
                        x$full.sp[kf] <- x$full.sp[kf]/x$smooth[[i]]$S.scale[j]
                        kf <- kf + 1
                      }
                    }
                    else {
                      x$full.sp[kf] <- x$full.sp[kf]/x$smooth[[i]]$S.scale[j]
                      kf <- kf + 1
                    }
                  }
                }
                else {
                  ii <- idxi[idx %in% x$smooth[[i]]$id]
                  for (j in 1:length(x$smooth[[ii]]$S.scale)) {
                    x$full.sp[kf] <- x$full.sp[kf]/x$smooth[[ii]]$S.scale[j]
                    kf <- kf + 1
                  }
                }
            }
    }
    vc <- c(scale/x$sp)
    names(vc) <- names(x$sp)
    if (is.null(x$full.sp)) 
        vc.full <- NULL
    else {
        vc.full <- c(scale/x$full.sp)
        names(vc.full) <- names(x$full.sp)
    }
    if (x$method %in% c("ML", "P-ML", "REML", "P-REML", "fREML") && 
        !is.null(x$outer.info$hess)) {
        if (is.null(x$family$n.theta) || x$family$n.theta <= 
            0) 
            H <- x$outer.info$hess
        else {
            ind <- 1:x$family$n.theta
            H <- x$outer.info$hess[-ind, -ind, drop = FALSE]
        }
        if (ncol(H) > length(x$sp)) 
            scale.est <- TRUE
        else scale.est <- FALSE
        J <- matrix(0, nrow(H), ncol(H))
        if (scale.est) {
            diag(J) <- -0.5
            J[, ncol(J)] <- 0.5
            vc <- c(vc, scale)
            names(vc) <- c(names(x$sp), "scale")
        }
        else {
            diag(J) <- -0.5
        }
        eh <- eigen(H, symmetric = TRUE)
        ind <- eh$values > max(eh$values) * .Machine$double.eps^75
        rank <- sum(ind)
        iv <- eh$values * 0
        iv[ind] <- 1/eh$values[ind]
        V <- eh$vectors %*% (iv * t(eh$vectors))
        V <- J %*% V %*% t(J)
        lsd <- log(sqrt(vc))
        sd.lsd <- sqrt(diag(V))
        if (conf.lev <= 0 || conf.lev >= 1) 
            conf.lev <- 0.95
        crit <- qnorm(1 - (1 - conf.lev)/2)
        ll <- lsd - crit * sd.lsd
        ul <- lsd + crit * sd.lsd
        res <- cbind(exp(lsd), exp(ll), exp(ul))
        rownames(res) <- names(vc)
        colnames(res) <- c("std.dev", "lower", "upper")
        cat("\n")
        cat(paste("Standard deviations and", conf.lev, "confidence intervals:\n\n"))
        print(res)
        cat("\nRank: ")
        cat(rank)
        cat("/")
        cat(ncol(H))
        cat("\n")
        if (!is.null(vc.full)) {
            cat("\nAll smooth components:\n")
            print(sqrt(vc.full))
            res <- list(all = sqrt(vc.full), vc = res)
        }
        invisible(res)
    }
    else {
        if (is.null(vc.full)) 
            return(sqrt(vc))
        else return(list(vc = sqrt(vc), all = sqrt(vc.full)))
    }
}


notExp <- function (x) 
{
    f <- x
    ind <- x > 1
    f[ind] <- exp(1) * (x[ind]^2 + 1)/2
    ind <- (x <= 1) & (x > -1)
    f[ind] <- exp(x[ind])
    ind <- (x <= -1)
    x[ind] <- -x[ind]
    f[ind] <- exp(1) * (x[ind]^2 + 1)/2
    f[ind] <- 1/f[ind]
    f
}


pdIdnot <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent())) 
{
    object <- numeric(0)
    class(object) <- c("pdIdnot", "pdMat")
    nlme::pdConstruct(object, value, form, nam, data)
}


gaulss <- function (link = list("identity", "logb"), b = 0.01) 
{
    if (length(link) != 2) 
        stop("gaulss requires 2 links specified as character strings")
    okLinks <- list(c("inverse", "log", "identity", "sqrt"), 
        "logb")
    stats <- list()
    if (link[[1]] %in% okLinks[[1]]) 
        stats[[1]] <- make.link(link[[1]])
    else stop(link[[1]], " link not available for mu parameter of gaulss")
    fam <- structure(list(link = link[[1]], canonical = "none", 
        linkfun = stats[[1]]$linkfun, mu.eta = stats[[1]]$mu.eta), 
        class = "family")
    fam <- fix.family.link(fam)
    stats[[1]]$d2link <- fam$d2link
    stats[[1]]$d3link <- fam$d3link
    stats[[1]]$d4link <- fam$d4link
    if (link[[2]] %in% okLinks[[2]]) {
        stats[[2]] <- list()
        stats[[2]]$valideta <- function(eta) TRUE
        stats[[2]]$link = link[[2]]
        stats[[2]]$linkfun <- eval(parse(text = paste("function(mu) log(1/mu -", 
            b, ")")))
        stats[[2]]$linkinv <- eval(parse(text = paste("function(eta) 1/(exp(eta) +", 
            b, ")")))
        stats[[2]]$mu.eta <- eval(parse(text = paste("function(eta) { ee <- exp(eta); -ee/(ee +", 
            b, ")^2 }")))
        stats[[2]]$d2link <- eval(parse(text = paste("function(mu) { mub <- 1 - mu *", 
            b, ";(2*mub-1)/(mub*mu)^2}")))
        stats[[2]]$d3link <- eval(parse(text = paste("function(mu) { mub <- 1 - mu *", 
            b, ";((1-mub)*mub*6-2)/(mub*mu)^3}")))
        stats[[2]]$d4link <- eval(parse(text = paste("function(mu) { mub <- 1 - mu *", 
            b, ";(((24*mub-36)*mub+24)*mub-6)/(mub*mu)^4}")))
    }
    else stop(link[[2]], " link not available for precision parameter of gaulss")
    residuals <- function(object, type = c("deviance", "pearson", 
        "response")) {
        type <- match.arg(type)
        rsd <- object$y - object$fitted[, 1]
        if (type == "response") 
            return(rsd)
        else return((rsd * object$fitted[, 2]))
    }
    postproc <- expression({
        object$null.deviance <- sum(((object$y - mean(object$y)) * 
            object$fitted[, 2])^2)
    })
    ll <- function(y, X, coef, wt, family, deriv = 0, d1b = 0, 
        d2b = 0, Hp = NULL, rank = 0, fh = NULL, D = NULL) {
        jj <- attr(X, "lpi")
        eta <- X[, jj[[1]], drop = FALSE] %*% coef[jj[[1]]]
        mu <- family$linfo[[1]]$linkinv(eta)
        eta1 <- X[, jj[[2]], drop = FALSE] %*% coef[jj[[2]]]
        tau <- family$linfo[[2]]$linkinv(eta1)
        n <- length(y)
        l1 <- matrix(0, n, 2)
        ymu <- y - mu
        ymu2 <- ymu^2
        tau2 <- tau^2
        l <- sum(-0.5 * ymu2 * tau2 - 0.5 * log(2 * pi) + log(tau))
        if (deriv > 0) {
            l1[, 1] <- tau2 * ymu
            l1[, 2] <- 1/tau - tau * ymu2
            l2 <- matrix(0, n, 3)
            l2[, 1] <- -tau2
            l2[, 2] <- 2 * l1[, 1]/tau
            l2[, 3] <- -ymu2 - 1/tau2
            ig1 <- cbind(family$linfo[[1]]$mu.eta(eta), family$linfo[[2]]$mu.eta(eta1))
            g2 <- cbind(family$linfo[[1]]$d2link(mu), family$linfo[[2]]$d2link(tau))
        }
        l3 <- l4 <- g3 <- g4 <- 0
        if (deriv > 1) {
            l3 <- matrix(0, n, 4)
            l3[, 2] <- -2 * tau
            l3[, 3] <- 2 * ymu
            l3[, 4] <- 2/tau^3
            g3 <- cbind(family$linfo[[1]]$d3link(mu), family$linfo[[2]]$d3link(tau))
        }
        if (deriv > 3) {
            l4 <- matrix(0, n, 5)
            l4[, 3] <- -2
            l4[, 5] <- -6/tau2^2
            g4 <- cbind(family$linfo[[1]]$d4link(mu), family$linfo[[2]]$d4link(tau))
        }
        if (deriv) {
            i2 <- family$tri$i2
            i3 <- family$tri$i3
            i4 <- family$tri$i4
            de <- gamlss.etamu(l1, l2, l3, l4, ig1, g2, g3, g4, 
                i2, i3, i4, deriv - 1)
            ret <- gamlss.gH(X, jj, de$l1, de$l2, i2, l3 = de$l3, 
                i3 = i3, l4 = de$l4, i4 = i4, d1b = d1b, d2b = d2b, 
                deriv = deriv - 1, fh = fh, D = D)
        }
        else ret <- list()
        ret$l <- l
        ret
    }
    initialize <- expression({
        n <- rep(1, nobs)
        use.unscaled <- if (!is.null(attr(E, "use.unscaled"))) TRUE else FALSE
        if (is.null(start)) {
            jj <- attr(x, "lpi")
            start <- rep(0, ncol(x))
            yt1 <- if (family$link[[1]] == "identity") y else family$linfo[[1]]$linkfun(abs(y) + 
                max(y) * 1e-07)
            x1 <- x[, jj[[1]], drop = FALSE]
            e1 <- E[, jj[[1]], drop = FALSE]
            if (use.unscaled) {
                qrx <- qr(rbind(x1, e1))
                x1 <- rbind(x1, e1)
                startji <- qr.coef(qr(x1), c(yt1, rep(0, nrow(E))))
                startji[!is.finite(startji)] <- 0
            } else startji <- pen.reg(x1, e1, yt1)
            start[jj[[1]]] <- startji
            lres1 <- log(abs(y - family$linfo[[1]]$linkinv(x[, 
                jj[[1]], drop = FALSE] %*% start[jj[[1]]])))
            x1 <- x[, jj[[2]], drop = FALSE]
            e1 <- E[, jj[[2]], drop = FALSE]
            if (use.unscaled) {
                x1 <- rbind(x1, e1)
                startji <- qr.coef(qr(x1), c(lres1, rep(0, nrow(E))))
                startji[!is.finite(startji)] <- 0
            } else startji <- pen.reg(x1, e1, lres1)
            start[jj[[2]]] <- startji
        }
    })
    structure(list(family = "gaulss", ll = ll, link = paste(link), 
        nlp = 2, tri = trind.generator(2), initialize = initialize, 
        postproc = postproc, residuals = residuals, linfo = stats, 
        d2link = 1, d3link = 1, d4link = 1, ls = 1, available.derivs = 2), 
        class = c("general.family", "extended.family", "family"))
}


initial.sp <- function (X, S, off, expensive = FALSE, XX = FALSE) 
{
    n.p <- length(S)
    if (XX) 
        expensive <- FALSE
    def.sp <- array(0, n.p)
    if (n.p) {
        ldxx <- if (XX) 
            diag(X)
        else colSums(X * X)
        ldss <- ldxx * 0
        if (expensive) 
            St <- matrix(0, ncol(X), ncol(X))
        pen <- rep(FALSE, length(ldxx))
        for (i in 1:n.p) {
            maS <- max(abs(S[[i]]))
            rsS <- rowMeans(abs(S[[i]]))
            csS <- colMeans(abs(S[[i]]))
            dS <- diag(abs(S[[i]]))
            thresh <- .Machine$double.eps^0.8 * maS
            ind <- rsS > thresh & csS > thresh & dS > thresh
            ss <- diag(S[[i]])[ind]
            start <- off[i]
            finish <- start + ncol(S[[i]]) - 1
            xx <- ldxx[start:finish]
            xx <- xx[ind]
            pen[start:finish] <- pen[start:finish] | ind
            sizeXX <- mean(xx)
            sizeS <- mean(ss)
            if (sizeS <= 0) 
                stop(gettextf("S[[%d]] matrix is not +ve definite.", 
                  i))
            def.sp[i] <- sizeXX/sizeS
            ldss[start:finish] <- ldss[start:finish] + def.sp[i] * 
                diag(S[[i]])
            if (expensive) 
                St[start:finish, start:finish] <- St[start:finish, 
                  start:finish] + def.sp[i] * S[[i]]
        }
        if (expensive) {
            msp <- single.sp(X, St)
            if (msp > 0) 
                def.sp <- def.sp * msp
        }
        else {
            ind <- ldss > 0 & pen
            ldxx <- ldxx[ind]
            ldss <- ldss[ind]
            while (mean(ldxx/(ldxx + ldss)) > 0.4) {
                def.sp <- def.sp * 10
                ldss <- ldss * 10
            }
            while (mean(ldxx/(ldxx + ldss)) < 0.4) {
                def.sp <- def.sp/10
                ldss <- ldss/10
            }
        }
    }
    as.numeric(def.sp)
}


notExp2 <- function (x, d = .Options$mgcv.vc.logrange, b = 1/d) 
{
    exp(d * sin(x * b))
}


gamSim <- function (eg = 1, n = 400, dist = "normal", scale = 2, verbose = TRUE) 
{
    if (eg == 1 || eg == 7) {
        if (eg == 1) {
            if (verbose) 
                cat("Gu & Wahba 4 term additive model\n")
        }
        else {
            if (verbose) 
                cat("Gu & Wahba 4 term additive model, correlated predictors\n")
        }
        x0 <- runif(n, 0, 1)
        if (eg == 7) 
            x1 <- x0 * 0.7 + runif(n, 0, 0.3)
        else x1 <- runif(n, 0, 1)
        x2 <- runif(n, 0, 1)
        if (eg == 7) 
            x3 <- x2 * 0.9 + runif(n, 0, 0.1)
        else x3 <- runif(n, 0, 1)
        f0 <- function(x) 2 * sin(pi * x)
        f1 <- function(x) exp(2 * x)
        f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * 
            (10 * x)^3 * (1 - x)^10
        f3 <- function(x) 0 * x
        f <- f0(x0) + f1(x1) + f2(x2)
        if (dist == "normal") {
            e <- rnorm(n, 0, scale)
            y <- f + e
        }
        else if (dist == "poisson") {
            g <- exp(f * scale)
            f <- log(g)
            y <- rpois(rep(1, n), g)
        }
        else if (dist == "binary") {
            f <- (f - 5) * scale
            g <- binomial()$linkinv(f)
            y <- rbinom(g, 1, g)
        }
        else stop("dist not recognised")
        data <- data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, 
            x3 = x3, f = f, f0 = f0(x0), f1 = f1(x1), f2 = f2(x2), 
            f3 = x3 * 0)
        return(data)
    }
    else if (eg == 2) {
        if (verbose) 
            cat("Bivariate smoothing example\n")
        test1 <- function(x, z, sx = 0.3, sz = 0.4) {
            (pi^sx * sz) * (1.2 * exp(-(x - 0.2)^2/sx^2 - (z - 
                0.3)^2/sz^2) + 0.8 * exp(-(x - 0.7)^2/sx^2 - 
                (z - 0.8)^2/sz^2))
        }
        x <- runif(n)
        z <- runif(n)
        xs <- seq(0, 1, length = 40)
        zs <- seq(0, 1, length = 40)
        pr <- data.frame(x = rep(xs, 40), z = rep(zs, rep(40, 
            40)))
        truth <- matrix(test1(pr$x, pr$z), 40, 40)
        f <- test1(x, z)
        y <- f + rnorm(n) * scale
        data <- data.frame(y = y, x = x, z = z, f = f)
        truth <- list(x = xs, z = zs, f = truth)
        return(list(data = data, truth = truth, pr = pr))
    }
    else if (eg == 3) {
        if (verbose) 
            cat("Continuous `by' variable example\n")
        x1 <- runif(n, 0, 1)
        x2 <- sort(runif(n, 0, 1))
        f <- 0.2 * x2^11 * (10 * (1 - x2))^6 + 10 * (10 * x2)^3 * 
            (1 - x2)^10
        e <- rnorm(n, 0, scale)
        y <- f * x1 + e
        return(data.frame(y = y, x1 = x1, x2 = x2, f = f))
    }
    else if (eg == 4) {
        if (verbose) 
            cat("Factor `by' variable example\n")
        x0 <- runif(n, 0, 1)
        x1 <- runif(n, 0, 1)
        x2 <- runif(n, 0, 1)
        f1 <- 2 * sin(pi * x2)
        f2 <- exp(2 * x2) - 3.75887
        f3 <- 0.2 * x2^11 * (10 * (1 - x2))^6 + 10 * (10 * x2)^3 * 
            (1 - x2)^10
        e <- rnorm(n, 0, scale)
        fac <- as.factor(sample(1:3, n, replace = TRUE))
        fac.1 <- as.numeric(fac == 1)
        fac.2 <- as.numeric(fac == 2)
        fac.3 <- as.numeric(fac == 3)
        y <- f1 * fac.1 + f2 * fac.2 + f3 * fac.3 + e
        return(data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, fac = fac, 
            f1 = f1, f2 = f2, f3 = f3))
    }
    else if (eg == 5) {
        if (verbose) 
            cat("Additive model + factor\n")
        x0 <- rep(1:4, 50)
        x1 <- runif(n, 0, 1)
        x2 <- runif(n, 0, 1)
        x3 <- runif(n, 0, 1)
        y <- 2 * x0
        y <- y + exp(2 * x1)
        y <- y + 0.2 * x2^11 * (10 * (1 - x2))^6 + 10 * (10 * 
            x2)^3 * (1 - x2)^10
        e <- rnorm(n, 0, scale)
        y <- y + e
        x0 <- as.factor(x0)
        return(data.frame(y = y, x0 = x0, x1 = x1, x2 = x2, x3 = x3))
    }
    else if (eg == 6) {
        if (verbose) 
            cat("4 term additive + random effect")
        dat <- gamSim(1, n = n, scale = 0)
        fac <- rep(1:4, n/4)
        dat$f <- dat$f + fac * 3
        dat$fac <- as.factor(fac)
        if (dist == "normal") {
            dat$y <- dat$f + rnorm(n) * scale
        }
        else if (dist == "poisson") {
            g <- exp(dat$f * scale)
            dat$y <- rpois(rep(1, n), g)
        }
        else if (dist == "binary") {
            g <- (dat$f - 5) * scale
            g <- binomial()$linkinv(g)
            dat$y <- rbinom(g, 1, g)
        }
        return(dat)
    }
}


s <- function (..., k = -1, fx = FALSE, bs = "tp", m = NA, by = NA, 
    xt = NULL, id = NULL, sp = NULL) 
{
    vars <- as.list(substitute(list(...)))[-1]
    d <- length(vars)
    by.var <- deparse(substitute(by), backtick = TRUE, width.cutoff = 500)
    if (by.var == ".") 
        stop("by=. not allowed")
    term <- deparse(vars[[1]], backtick = TRUE, width.cutoff = 500)
    if (term[1] == ".") 
        stop("s(.) not yet supported.")
    if (d > 1) 
        for (i in 2:d) {
            term[i] <- deparse(vars[[i]], backtick = TRUE, width.cutoff = 500)
            if (term[i] == ".") 
                stop("s(.) not yet supported.")
        }
    for (i in 1:d) term[i] <- attr(terms(reformulate(term[i])), 
        "term.labels")
    k.new <- round(k)
    if (all.equal(k.new, k) != TRUE) {
        warning("argument k of s() should be integer and has been rounded")
    }
    k <- k.new
    if (length(unique(term)) != d) 
        stop("Repeated variables as arguments of a smooth are not permitted")
    full.call <- paste("s(", term[1], sep = "")
    if (d > 1) 
        for (i in 2:d) full.call <- paste(full.call, ",", term[i], 
            sep = "")
    label <- paste(full.call, ")", sep = "")
    if (!is.null(id)) {
        if (length(id) > 1) {
            id <- id[1]
            warning("only first element of `id' used")
        }
        id <- as.character(id)
    }
    ret <- list(term = term, bs.dim = k, fixed = fx, dim = d, 
        p.order = m, by = by.var, label = label, xt = xt, id = id, 
        sp = sp)
    class(ret) <- paste(bs, ".smooth.spec", sep = "")
    ret
}


new.name <- function (proposed, old.names) 
{
    prop <- proposed
    k <- 0
    while (sum(old.names == prop)) {
        prop <- paste(proposed, ".", k, sep = "")
        k <- k + 1
    }
    prop
}


smooth.construct.sw.smooth.spec <- function (object, data, knots) 
{
    if (is.null(knots)) 
        stop("knots must be specified for soap")
    if (object$dim != 2) 
        stop("soap films are bivariate only")
    x <- data[[object$term[1]]]
    y <- data[[object$term[2]]]
    knt <- list(x = knots[[object$term[1]]], y = knots[[object$term[2]]])
    if (length(knt$x) < 1) 
        stop("need at least one interior knot")
    bnd <- object$xt$bnd
    if (is.null(bnd)) 
        stop("can't soap smooth without a boundary")
    if (!inherits(bnd, "list")) 
        stop("bnd must be a list of boundary loops")
    for (i in 1:length(bnd)) {
        nm <- names(bnd[[i]])
        ind <- nm == object$term[1]
        if (sum(ind) != 1) 
            stop("faulty bnd")
        names(bnd[[i]])[ind] <- "x"
        ind <- nm == object$term[2]
        if (sum(ind) != 1) 
            stop("faulty bnd")
        names(bnd[[i]])[ind] <- "y"
    }
    if (length(object$bs.dim) == 1) 
        k <- rep(object$bs.dim, length(bnd))
    else {
        if (length(object$bs.dim) == length(bnd)) 
            k <- object$bs.dim
        else stop("k and bnd lengths are inconsistent")
    }
    if (is.null(object$xt$nmax)) 
        nmax <- 200
    else nmax <- object$xt$nmax
    sd <- setup.soap(bnd, knots = knt, nmax = nmax, k = k, bndSpec = object$xt$bndSpec)
    b <- soap.basis(sd, x, y, film = FALSE, wiggly = TRUE, penalty = TRUE)
    if (sum(is.na(b$X)) > 0) 
        stop("data outside soap boundary")
    object$null.space.dim <- 0
    irng <- 1/as.numeric(apply(b$X, 2, max) - apply(b$X, 2, min))
    b$X <- t(t(b$X) * irng)
    for (i in 1:length(b$S)) {
        a <- irng[b$off[i]:(b$off[i] + ncol(b$S[[i]]) - 1)]
        b$S[[i]] <- diag(a) %*% b$S[[i]] %*% diag(a)
    }
    object$irng <- irng
    object$X <- b$X
    if (!object$fixed) {
        S <- list()
        n <- ncol(object$X)
        for (i in 1:length(b$S)) {
            S[[i]] <- matrix(0, n, n)
            m <- ncol(b$S[[i]])
            ind <- b$off[i]:(b$off[i] + m - 1)
            S[[i]][ind, ind] <- b$S[[i]]
        }
        object$S <- S
    }
    rr <- ncol(b$S[[1]]) - 1
    if (length(b$S) > 1) 
        for (i in 2:length(b$S)) rr <- c(rr, ncol(b$S[[i]]) - 
            1)
    rr[length(rr)] <- rr[length(rr)] + 1
    object$rank <- rr
    object$df <- ncol(object$X)
    for (i in 1:length(sd$bc)) {
        sd$bc[[i]]$bsm <- sd$bc[[i]]$S <- NULL
    }
    object$sd <- sd
    object$C <- matrix(0, 0, ncol(object$X))
    class(object) <- c("sw", "soap.film")
    object
}


smooth.construct.mrf.smooth.spec <- function (object, data, knots) 
{
    x <- as.factor(data[[object$term]])
    k <- knots[[object$term]]
    if (is.null(k)) {
        k <- as.factor(levels(x))
    }
    else k <- as.factor(k)
    if (object$bs.dim < 0) 
        object$bs.dim <- length(levels(k))
    if (object$bs.dim > length(levels(k))) 
        stop("MRF basis dimension set too high")
    if (sum(!levels(x) %in% levels(k))) 
        stop("data contain regions that are not contained in the knot specification")
    x <- factor(x, levels = levels(k))
    object$X <- model.matrix(~x - 1, data.frame(x = x))
    if (is.null(object$xt)) 
        stop("penalty matrix, boundary polygons and/or neighbours list must be supplied in xt")
    if (!is.null(object$xt$polys)) {
        a.name <- names(object$xt$polys)
        d.name <- unique(a.name[duplicated(a.name)])
        if (length(d.name)) {
            for (i in 1:length(d.name)) {
                ind <- (1:length(a.name))[a.name == d.name[i]]
                for (j in 2:length(ind)) object$xt$polys[[ind[1]]] <- rbind(object$xt$polys[[ind[1]]], 
                  c(NA, NA), object$xt$polys[[ind[j]]])
            }
            ind <- (1:length(a.name))[duplicated(a.name)]
            if (length(ind) > 0) 
                for (i in length(ind):1) object$xt$polys[[ind[i]]] <- NULL
        }
    }
    if (is.null(object$xt$penalty)) {
        if (is.null(object$xt$nb)) {
            if (is.null(object$xt$polys)) 
                stop("no spatial information provided!")
            object$xt$nb <- pol2nb(object$xt$polys)$nb
        }
        a.name <- names(object$xt$nb)
        if (all.equal(sort(a.name), sort(levels(k))) != TRUE) 
            stop("mismatch between nb/polys supplied area names and data area names")
        np <- ncol(object$X)
        S <- matrix(0, np, np)
        rownames(S) <- colnames(S) <- levels(k)
        for (i in 1:np) {
            ind <- object$xt$nb[[i]]
            lind <- length(ind)
            S[a.name[i], a.name[i]] <- lind
            if (lind > 0) 
                for (j in 1:lind) S[a.name[i], a.name[ind[j]]] <- -1
        }
        if (sum(S != t(S)) > 0) 
            stop("Something wrong with auto- penalty construction")
        object$S[[1]] <- S
    }
    else {
        object$S[[1]] <- object$xt$penalty
        if (ncol(object$S[[1]]) != nrow(object$S[[1]])) 
            stop("supplied penalty not square!")
        if (ncol(object$S[[1]]) != ncol(object$X)) 
            stop("supplied penalty wrong dimension!")
        if (!is.null(colnames(object$S[[1]]))) {
            a.name <- colnames(object$S[[1]])
            if (all.equal(levels(k), sort(a.name)) != TRUE) {
                stop("penalty column names don't match supplied area names!")
            }
            else {
                if (all.equal(sort(a.name), a.name) != TRUE) {
                  object$S[[1]] <- object$S[[1]][levels(k), ]
                  object$S[[1]] <- object$S[[1]][, levels(k)]
                }
            }
        }
    }
    if (object$bs.dim < length(levels(k))) {
        mi <- which(colSums(object$X) == 0)
        np <- ncol(object$X)
        if (length(mi) > 0) {
            object$X <- rbind(matrix(0, length(mi), np), object$X)
            for (i in 1:length(mi)) object$X[i, mi[i]] <- 1
        }
        rp <- nat.param(object$X, object$S[[1]], type = 0)
        ind <- (np - object$bs.dim + 1):np
        object$X <- if (length(mi)) 
            rp$X[-(1:length(mi)), ind]
        else rp$X[, ind]
        object$P <- rp$P[, ind]
        object$S[[1]] <- diag(c(rp$D[ind[ind <= rp$rank]], rep(0, 
            sum(ind > rp$rank))))
        object$rank <- rp$rank
    }
    else {
        ev <- eigen(object$S[[1]], symmetric = TRUE, only.values = TRUE)$values
        object$rank <- sum(ev > .Machine$double.eps^0.8 * max(ev))
    }
    object$null.space.dim <- ncol(object$X) - object$rank
    object$knots <- k
    object$df <- ncol(object$X)
    class(object) <- "mrf.smooth"
    object
}


gam.control <- function (nthreads = 1, irls.reg = 0, epsilon = 1e-07, maxit = 200, 
    mgcv.tol = 1e-07, mgcv.half = 15, trace = FALSE, rank.tol = .Machine$double.eps^0.5, 
    nlm = list(), optim = list(), newton = list(), outerPIsteps = 0, 
    idLinksBases = TRUE, scalePenalty = TRUE, keepData = FALSE, 
    scale.est = "fletcher") 
{
    scale.est <- match.arg(scale.est, c("fletcher", "pearson", 
        "deviance"))
    if (!is.numeric(nthreads) || nthreads < 1) 
        stop("nthreads must be a positive integer")
    if (!is.numeric(irls.reg) || irls.reg < 0) 
        stop("IRLS regularizing parameter must be a non-negative number.")
    if (!is.numeric(epsilon) || epsilon <= 0) 
        stop("value of epsilon must be > 0")
    if (!is.numeric(maxit) || maxit <= 0) 
        stop("maximum number of iterations must be > 0")
    if (rank.tol < 0 || rank.tol > 1) {
        rank.tol = .Machine$double.eps^0.5
        warning("silly value supplied for rank.tol: reset to square root of machine precision.")
    }
    if (is.null(nlm$ndigit) || nlm$ndigit < 2) 
        nlm$ndigit <- max(2, ceiling(-log10(epsilon)))
    nlm$ndigit <- round(nlm$ndigit)
    ndigit <- floor(-log10(.Machine$double.eps))
    if (nlm$ndigit > ndigit) 
        nlm$ndigit <- ndigit
    if (is.null(nlm$gradtol)) 
        nlm$gradtol <- epsilon * 10
    nlm$gradtol <- abs(nlm$gradtol)
    if (is.null(nlm$stepmax) || nlm$stepmax == 0) 
        nlm$stepmax <- 2
    nlm$stepmax <- abs(nlm$stepmax)
    if (is.null(nlm$steptol)) 
        nlm$steptol <- 1e-04
    nlm$steptol <- abs(nlm$steptol)
    if (is.null(nlm$iterlim)) 
        nlm$iterlim <- 200
    nlm$iterlim <- abs(nlm$iterlim)
    if (is.null(nlm$check.analyticals)) 
        nlm$check.analyticals <- FALSE
    nlm$check.analyticals <- as.logical(nlm$check.analyticals)
    if (is.null(newton$conv.tol)) 
        newton$conv.tol <- 1e-06
    if (is.null(newton$maxNstep)) 
        newton$maxNstep <- 5
    if (is.null(newton$maxSstep)) 
        newton$maxSstep <- 2
    if (is.null(newton$maxHalf)) 
        newton$maxHalf <- 30
    if (is.null(newton$use.svd)) 
        newton$use.svd <- FALSE
    if (is.null(optim$factr)) 
        optim$factr <- 1e+07
    optim$factr <- abs(optim$factr)
    list(nthreads = round(nthreads), irls.reg = irls.reg, epsilon = epsilon, 
        maxit = maxit, trace = trace, mgcv.tol = mgcv.tol, mgcv.half = mgcv.half, 
        rank.tol = rank.tol, nlm = nlm, optim = optim, newton = newton, 
        outerPIsteps = outerPIsteps, idLinksBases = idLinksBases, 
        scalePenalty = scalePenalty, keepData = as.logical(keepData[1]), 
        scale.est = scale.est)
}


ziplss <- function (link = list("identity", "identity")) 
{
    if (length(link) != 2) 
        stop("ziplss requires 2 links specified as character strings")
    okLinks <- list(c("identity"), c("identity"))
    stats <- list()
    param.names <- c("Poisson mean", "binary probability")
    for (i in 1:2) {
        if (link[[i]] %in% okLinks[[i]]) 
            stats[[i]] <- make.link(link[[i]])
        else stop(link[[i]], " link not available for ", param.names[i], 
            " parameter of ziplss")
        fam <- structure(list(link = link[[i]], canonical = "none", 
            linkfun = stats[[i]]$linkfun, mu.eta = stats[[i]]$mu.eta), 
            class = "family")
        fam <- fix.family.link(fam)
        stats[[i]]$d2link <- fam$d2link
        stats[[i]]$d3link <- fam$d3link
        stats[[i]]$d4link <- fam$d4link
    }
    residuals <- function(object, type = c("deviance", "response")) {
        ls <- function(y) {
            l <- y
            l[y < 2] <- 0
            ind <- y > 1 & y < 18
            glo <- c(1.593624, 2.821439, 3.92069, 4.965114, 5.984901, 
                6.993576, 7.997309, 8.998888, 9.999546, 10.999816, 
                11.999926, 12.999971, 13.999988, 14.999995, 15.999998, 
                16.999999)
            g <- y
            g[ind] <- glo[y[ind] - 1]
            ind <- y > 1
            l[ind] <- zipll(y[ind], log(g[ind]), g[ind] * 0 + 
                1e+10, deriv = 0)$l
            l
        }
        type <- match.arg(type)
        p <- exp(-exp(object$fitted[, 2]))
        lam <- exp(object$fitted[, 1])
        ind <- lam > .Machine$double.eps^0.5
        Ey <- p
        Ey[ind] <- p[ind] * lam[ind]/(1 - exp(-lam[ind]))
        rsd <- object$y - Ey
        if (type == "response") 
            return(rsd)
        else {
            sgn <- sign(rsd)
            ind <- object$y == 0
            rsd <- pmax(0, 2 * (ls(object$y) - zipll(object$y, 
                object$fitted[, 1], object$fitted[, 2], deriv = 0)$l))
            rsd <- sqrt(rsd) * sgn
        }
        rsd
    }
    predict <- function(family, se = FALSE, eta = NULL, y = NULL, 
        X = NULL, beta = NULL, off = NULL, Vb = NULL) {
        if (is.null(eta)) {
            lpi <- attr(X, "lpi")
            X1 <- X[, lpi[[1]], drop = FALSE]
            X2 <- X[, lpi[[2]], drop = FALSE]
            gamma <- drop(X1 %*% beta[lpi[[1]]])
            eta <- drop(X2 %*% beta[lpi[[2]]])
            if (se) {
                v.g <- drop(pmax(0, rowSums((X1 %*% Vb[lpi[[1]], 
                  lpi[[1]]]) * X1)))
                v.e <- drop(pmax(0, rowSums((X1 %*% Vb[lpi[[1]], 
                  lpi[[1]]]) * X1)))
                v.eg <- drop(pmax(0, rowSums((X1 %*% Vb[lpi[[1]], 
                  lpi[[2]]]) * X2)))
            }
        }
        else {
            se <- FALSE
            gamma <- eta[, 1]
            eta <- eta[, 2]
        }
        et <- exp(eta)
        mu <- p <- 1 - exp(-et)
        fv <- lambda <- exp(gamma)
        ind <- gamma < log(.Machine$double.eps)/2
        mu[!ind] <- lambda[!ind]/(1 - exp(-lambda[!ind]))
        mu[ind] <- 1
        fv <- list(p * mu)
        if (!se) 
            return(fv)
        else {
            df.de <- p
            ind <- eta < log(.Machine$double.xmax)/2
            df.de[!ind] <- 0
            df.de[ind] <- exp(-et[ind]) * et[ind]
            df.de <- df.de * mu
            df.dg <- ((lambda + 1) * mu - mu^2) * p
            fv[[2]] <- sqrt(df.dg^2 * v.g + df.de^2 * v.e + 2 * 
                df.de * df.dg * v.eg)
            names(fv) <- c("fit", "se.fit")
            return(fv)
        }
    }
    rd <- function(mu, wt, scale) {
        rzip <- function(gamma, eta) {
            y <- gamma
            n <- length(y)
            lambda <- exp(gamma)
            p <- 1 - exp(-exp(eta))
            ind <- p > runif(n)
            y[!ind] <- 0
            np <- sum(ind)
            y[ind] <- qpois(runif(np, dpois(0, lambda[ind]), 
                1), lambda[ind])
            y
        }
        rzip(mu[, 1], mu[, 2])
    }
    postproc <- expression({
        ls <- function(y) {
            l <- y
            l[y < 2] <- 0
            ind <- y > 1 & y < 18
            glo <- c(1.593624, 2.821439, 3.92069, 4.965114, 5.984901, 
                6.993576, 7.997309, 8.998888, 9.999546, 10.999816, 
                11.999926, 12.999971, 13.999988, 14.999995, 15.999998, 
                16.999999)
            g <- y
            g[ind] <- glo[y[ind] - 1]
            ind <- y > 1
            l[ind] <- zipll(y[ind], log(g[ind]), g[ind] * 0 + 
                1e+10, deriv = 0)$l
            l
        }
        fp <- function(p, y) {
            eps <- .Machine$double.eps^0.5
            l1p <- if (p > eps) log(1 - p) else -p - p^2/2
            l1p * sum(y == 0) + log(p) * sum(y > 0)
        }
        flam <- function(lam, y) {
            y <- y[y > 0]
            sum(y * log(lam) - log(exp(lam) - 1) - lgamma(y + 
                1))
        }
        lnull <- optimize(fp, interval = c(1e-60, 1 - 1e-10), 
            y = object$y, maximum = TRUE)$objective
        my <- mean(object$y[object$y > 0])
        lnull <- lnull + optimize(flam, interval = c(my/2, my * 
            2), y = object$y, maximum = TRUE)$objective
        object$null.deviance <- 2 * (sum(ls(object$y)) - lnull)
    })
    ll <- function(y, X, coef, wt, family, deriv = 0, d1b = 0, 
        d2b = 0, Hp = NULL, rank = 0, fh = NULL, D = NULL) {
        jj <- attr(X, "lpi")
        eta <- X[, jj[[1]], drop = FALSE] %*% coef[jj[[1]]]
        lambda <- family$linfo[[1]]$linkinv(eta)
        eta1 <- X[, jj[[2]], drop = FALSE] %*% coef[jj[[2]]]
        p <- family$linfo[[2]]$linkinv(eta1)
        zl <- zipll(y, lambda, p, deriv)
        if (deriv > 0) {
            ig1 <- cbind(family$linfo[[1]]$mu.eta(eta), family$linfo[[2]]$mu.eta(eta1))
            g2 <- cbind(family$linfo[[1]]$d2link(lambda), family$linfo[[2]]$d2link(p))
        }
        g3 <- g4 <- 0
        if (deriv > 1) {
            g3 <- cbind(family$linfo[[1]]$d3link(lambda), family$linfo[[2]]$d3link(p))
        }
        if (deriv > 3) {
            g4 <- cbind(family$linfo[[1]]$d4link(lambda), family$linfo[[2]]$d4link(p))
        }
        if (deriv) {
            i2 <- family$tri$i2
            i3 <- family$tri$i3
            i4 <- family$tri$i4
            de <- gamlss.etamu(zl$l1, zl$l2, zl$l3, zl$l4, ig1, 
                g2, g3, g4, i2, i3, i4, deriv - 1)
            ret <- gamlss.gH(X, jj, de$l1, de$l2, i2, l3 = de$l3, 
                i3 = i3, l4 = de$l4, i4 = i4, d1b = d1b, d2b = d2b, 
                deriv = deriv - 1, fh = fh, D = D)
        }
        else ret <- list()
        ret$l <- sum(zl$l)
        ret
    }
    initialize <- expression({
        n <- rep(1, nobs)
        if (all.equal(y, round(y)) != TRUE) {
            stop("Non-integer response variables are not allowed with ziplss ")
        }
        if ((min(y) == 0 && max(y) == 1)) stop("Using ziplss for binary data makes no sense")
        use.unscaled <- if (!is.null(attr(E, "use.unscaled"))) TRUE else FALSE
        if (is.null(start)) {
            jj <- attr(x, "lpi")
            start <- rep(0, ncol(x))
            x1 <- x[, jj[[2]], drop = FALSE]
            e1 <- E[, jj[[2]], drop = FALSE]
            yt1 <- as.numeric(as.logical(y))
            if (use.unscaled) {
                qrx <- qr(rbind(x1, e1))
                x1 <- rbind(x1, e1)
                startji <- qr.coef(qr(x1), c(yt1, rep(0, nrow(E))))
                startji[!is.finite(startji)] <- 0
            } else startji <- pen.reg(x1, e1, yt1)
            start[jj[[2]]] <- startji
            p <- drop(x1[1:nobs, , drop = FALSE] %*% startji)
            ind <- y == 0 & p < 0.5
            w <- rep(1, nobs)
            w[ind] <- 0.1
            yt1 <- family$linfo[[1]]$linkfun(log(abs(y) + (y == 
                0) * 0.2))
            yt1 <- yt1 * w
            x1 <- w * x[, jj[[1]], drop = FALSE]
            e1 <- E[, jj[[1]], drop = FALSE]
            if (use.unscaled) {
                x1 <- rbind(x1, e1)
                startji <- qr.coef(qr(x1), c(yt1, rep(0, nrow(E))))
                startji[!is.finite(startji)] <- 0
            } else startji <- pen.reg(x1, e1, yt1)
            start[jj[[1]]] <- startji
        }
    })
    structure(list(family = "ziplss", ll = ll, link = paste(link), 
        nlp = 2, tri = trind.generator(2), initialize = initialize, 
        postproc = postproc, residuals = residuals, rd = rd, 
        predict = predict, linfo = stats, d2link = 1, d3link = 1, 
        d4link = 1, ls = 1, available.derivs = 2), class = c("general.family", 
        "extended.family", "family"))
}


interpret.gam <- function (gf) 
{
    if (is.list(gf)) {
        d <- length(gf)
        resp <- gf[[1]][2]
        ret <- list()
        pav <- av <- rep("", 0)
        nlp <- 0
        for (i in 1:d) {
            textra <- if (i == 1) 
                NULL
            else paste(".", i - 1, sep = "")
            lpi <- getNumericResponse(gf[[i]])
            if (length(lpi) == 1) 
                warning("single linear predictor indices are ignored")
            if (length(lpi) > 0) 
                gf[[i]][[2]] <- NULL
            else {
                nlp <- nlp + 1
                lpi <- nlp
            }
            ret[[i]] <- interpret.gam0(gf[[i]], textra)
            ret[[i]]$lpi <- lpi
            respi <- rep("", 0)
            if (length(ret[[i]]$pf) == 2) {
                ret[[i]]$pf[3] <- ret[[i]]$pf[2]
                ret[[i]]$pf[2] <- resp
                respi <- rep("", 0)
            }
            else if (i > 1) 
                respi <- ret[[i]]$response
            av <- c(av, ret[[i]]$fake.names, respi)
            pav <- c(pav, ret[[i]]$pred.names)
        }
        av <- unique(av)
        pav <- unique(pav)
        ret$fake.formula <- if (length(av) > 0) 
            reformulate(av, response = ret[[1]]$response)
        else ret[[1]]$fake.formula
        ret$pred.formula <- if (length(pav) > 0) 
            reformulate(pav)
        else ~1
        ret$response <- ret[[1]]$response
        ret$nlp <- nlp
        for (i in 1:d) if (max(ret[[i]]$lpi) > nlp || min(ret[[i]]$lpi) < 
            1) 
            stop("linear predictor labels out of range")
        class(ret) <- "split.gam.formula"
        return(ret)
    }
    else interpret.gam0(gf)
}


concurvity <- function (b, full = TRUE) 
{
    if (!inherits(b, "gam")) 
        stop("requires an object of class gam")
    m <- length(b$smooth)
    if (m < 1) 
        stop("nothing to do for this model")
    X <- model.matrix(b)
    X <- X[rowSums(is.na(X)) == 0, ]
    X <- qr.R(qr(X, tol = 0, LAPACK = FALSE))
    stop <- start <- rep(1, m)
    lab <- rep("", m)
    for (i in 1:m) {
        start[i] <- b$smooth[[i]]$first.para
        stop[i] <- b$smooth[[i]]$last.para
        lab[i] <- b$smooth[[i]]$label
    }
    if (min(start) > 1) {
        start <- c(1, start)
        stop <- c(min(start) - 1, stop)
        lab <- c("para", lab)
        m <- m + 1
    }
    n.measures <- 3
    measure.names <- c("worst", "observed", "estimate")
    if (full) {
        conc <- matrix(0, n.measures, m)
        for (i in 1:m) {
            Xi <- X[, -(start[i]:stop[i]), drop = FALSE]
            Xj <- X[, start[i]:stop[i], drop = FALSE]
            r <- ncol(Xi)
            R <- qr.R(qr(cbind(Xi, Xj), LAPACK = FALSE, tol = 0))[, 
                -(1:r), drop = FALSE]
            Rt <- qr.R(qr(R))
            conc[1, i] <- svd(forwardsolve(t(Rt), t(R[1:r, , 
                drop = FALSE])))$d[1]^2
            beta <- b$coef[start[i]:stop[i]]
            conc[2, i] <- sum((R[1:r, , drop = FALSE] %*% beta)^2)/sum((Rt %*% 
                beta)^2)
            conc[3, i] <- sum(R[1:r, ]^2)/sum(R^2)
        }
        colnames(conc) <- lab
        rownames(conc) <- measure.names
    }
    else {
        conc <- list()
        for (i in 1:n.measures) conc[[i]] <- matrix(1, m, m)
        for (i in 1:m) {
            Xi <- X[, start[i]:stop[i], drop = FALSE]
            r <- ncol(Xi)
            for (j in 1:m) if (i != j) {
                Xj <- X[, start[j]:stop[j], drop = FALSE]
                R <- qr.R(qr(cbind(Xi, Xj), LAPACK = FALSE, tol = 0))[, 
                  -(1:r), drop = FALSE]
                Rt <- qr.R(qr(R))
                conc[[1]][i, j] <- svd(forwardsolve(t(Rt), t(R[1:r, 
                  , drop = FALSE])))$d[1]^2
                beta <- b$coef[start[j]:stop[j]]
                conc[[2]][i, j] <- sum((R[1:r, , drop = FALSE] %*% 
                  beta)^2)/sum((Rt %*% beta)^2)
                conc[[3]][i, j] <- sum(R[1:r, ]^2)/sum(R^2)
                rm(Xj, R, Rt)
            }
        }
        for (i in 1:n.measures) rownames(conc[[i]]) <- colnames(conc[[i]]) <- lab
        names(conc) <- measure.names
    }
    conc
}


smooth.construct.sos.smooth.spec <- function (object, data, knots) 
{
    xtra <- list()
    if (is.null(object$xt$max.knots)) 
        xtra$max.knots <- 2000
    else xtra$max.knots <- object$xt$max.knots
    if (is.null(object$xt$seed)) 
        xtra$seed <- 1
    else xtra$seed <- object$xt$seed
    if (object$dim != 2) 
        stop("Can only deal with a sphere")
    x <- array(0, 0)
    for (i in 1:2) {
        xx <- data[[object$term[i]]]
        if (i == 1) 
            n <- length(xx)
        else if (n != length(xx)) 
            stop("arguments of smooth not same dimension")
        x <- c(x, xx)
    }
    if (is.null(knots)) {
        knt <- 0
        nk <- 0
    }
    else {
        knt <- array(0, 0)
        for (i in 1:2) {
            dum <- knots[[object$term[i]]]
            if (is.null(dum)) {
                knt <- 0
                nk <- 0
                break
            }
            knt <- c(knt, dum)
            nk0 <- length(dum)
            if (i > 1 && nk != nk0) 
                stop("components of knots relating to a single smooth must be of same length")
            nk <- nk0
        }
    }
    if (nk > n) {
        nk <- 0
        warning("more knots than data in an sos term: knots ignored.")
    }
    if (nk == 0) {
        xu <- uniquecombs(matrix(x, n, 2))
        nu <- nrow(xu)
        if (n > xtra$max.knots) {
            if (nu > xtra$max.knots) {
                seed <- try(get(".Random.seed", envir = .GlobalEnv), 
                  silent = TRUE)
                if (inherits(seed, "try-error")) {
                  runif(1)
                  seed <- get(".Random.seed", envir = .GlobalEnv)
                }
                kind <- RNGkind(NULL)
                RNGkind("default", "default")
                set.seed(xtra$seed)
                nk <- xtra$max.knots
                ind <- sample(1:nu, nk, replace = FALSE)
                knt <- as.numeric(xu[ind, ])
                RNGkind(kind[1], kind[2])
                assign(".Random.seed", seed, envir = .GlobalEnv)
            }
            else {
                knt <- xu
                nk <- nu
            }
        }
        else {
            knt <- xu
            nk <- nu
        }
    }
    if (object$bs.dim[1] < 0) 
        object$bs.dim <- 50
    if (is.na(object$p.order)) 
        object$p.order <- 0
    object$p.order <- round(object$p.order)
    if (object$p.order < -1) 
        object$p.order <- -1
    if (object$p.order > 4) 
        object$p.order <- 4
    R <- makeR(la = knt[1:nk], lo = knt[-(1:nk)], lak = knt[1:nk], 
        lok = knt[-(1:nk)], m = object$p.order)
    T <- attr(R, "Tc")
    ind <- 1:ncol(T)
    k <- object$bs.dim
    if (k < nk) {
        er <- slanczos(R, k, -1)
        D <- diag(er$values)
        U1 <- t(t(T) %*% er$vectors)
    }
    else {
        U1 <- T
        D <- R
        er <- list(vectors = diag(k))
    }
    rm(R)
    qru <- qr(U1)
    S <- qr.qty(qru, t(qr.qty(qru, D)[-ind, ]))[-ind, ]
    object$S <- list(S = rbind(cbind(S, matrix(0, k - length(ind), 
        length(ind))), matrix(0, length(ind), k)))
    object$UZ <- t(qr.qty(qru, t(er$vectors))[-ind, ])
    object$knt = knt
    object$df <- object$bs.dim
    object$null.space.dim <- length(ind)
    object$rank <- k - length(ind)
    class(object) <- "sos.smooth"
    object$X <- Predict.matrix.sos.smooth(object, data)
    xs <- as.numeric(apply(object$X, 2, sd))
    xs[xs == min(xs)] <- 1
    xs <- 1/xs
    object$X <- t(t(object$X) * xs)
    object$S[[1]] <- t(t(xs * object$S[[1]]) * xs)
    object$xc.scale <- xs
    object
}


smooth.construct.cr.smooth.spec <- function (object, data, knots) 
{
    shrink <- attr(object, "shrink")
    if (length(object$term) != 1) 
        stop("Basis only handles 1D smooths")
    x <- data[[object$term]]
    nx <- length(x)
    if (is.null(knots)) 
        ok <- FALSE
    else {
        k <- knots[[object$term]]
        if (is.null(k)) 
            ok <- FALSE
        else ok <- TRUE
    }
    if (object$bs.dim < 0) 
        object$bs.dim <- 10
    if (object$bs.dim < 3) {
        object$bs.dim <- 3
        warning("basis dimension, k, increased to minimum possible\n")
    }
    xu <- unique(x)
    nk <- object$bs.dim
    if (length(xu) < nk) {
        msg <- paste(object$term, " has insufficient unique values to support ", 
            nk, " knots: reduce k.", sep = "")
        stop(msg)
    }
    if (!ok) {
        k <- quantile(xu, seq(0, 1, length = nk))
    }
    if (length(k) != nk) 
        stop("number of supplied knots != k for a cr smooth")
    X <- rep(0, nx * nk)
    F <- S <- rep(0, nk * nk)
    F.supplied <- 0
    oo <- .C(C_crspl, x = as.double(x), n = as.integer(nx), xk = as.double(k), 
        nk = as.integer(nk), X = as.double(X), S = as.double(S), 
        F = as.double(F), Fsupplied = as.integer(F.supplied))
    object$X <- matrix(oo$X, nx, nk)
    object$S <- list()
    if (!object$fixed) {
        object$S[[1]] <- matrix(oo$S, nk, nk)
        object$S[[1]] <- (object$S[[1]] + t(object$S[[1]]))/2
        if (!is.null(shrink)) {
            es <- eigen(object$S[[1]], symmetric = TRUE)
            es$values[nk - 1] <- es$values[nk - 2] * shrink
            es$values[nk] <- es$values[nk - 1] * shrink
            object$S[[1]] <- es$vectors %*% (as.numeric(es$values) * 
                t(es$vectors))
        }
    }
    if (is.null(shrink)) {
        object$rank <- nk - 2
        object$null.space.dim <- 2
    }
    else {
        object$rank <- nk
        object$null.space.dim <- 0
    }
    object$df <- object$bs.dim
    object$xp <- k
    object$F <- oo$F
    class(object) <- "cr.smooth"
    object
}


predict.bam <- function (object, newdata, type = "link", se.fit = FALSE, terms = NULL, 
    exclude = NULL, block.size = 50000, newdata.guaranteed = FALSE, 
    na.action = na.pass, cluster = NULL, discrete = TRUE, n.threads = 1, 
    ...) 
{
    if (discrete && !is.null(object$dinfo)) {
        return(predict.bamd(object, newdata, type, se.fit, terms, 
            exclude, block.size, newdata.guaranteed, na.action, 
            n.threads, ...))
    }
    object$Sl <- object$qrx <- object$R <- object$F <- object$Ve <- object$Vc <- object$G <- object$residuals <- object$fitted.values <- object$linear.predictors <- NULL
    gc()
    if (!is.null(cluster) && inherits(cluster, "cluster")) {
        n.threads <- length(cluster)
    }
    else n.threads <- 1
    if (missing(newdata)) 
        n <- nrow(object$model)
    else n <- nrow(newdata)
    if (n < 100 * n.threads) 
        n.threads <- 1
    if (n.threads == 1) {
        if (missing(newdata)) 
            return(predict.gam(object, newdata = object$model, 
                type = type, se.fit = se.fit, terms = terms, 
                exclude = exclude, block.size = block.size, newdata.guaranteed = newdata.guaranteed, 
                na.action = na.action, ...))
        else return(predict.gam(object, newdata = newdata, type = type, 
            se.fit = se.fit, terms = terms, exclude = exclude, 
            block.size = block.size, newdata.guaranteed = newdata.guaranteed, 
            na.action = na.action, ...))
    }
    else {
        nt <- rep(floor(n/n.threads), n.threads)
        nt[1] <- n - sum(nt[-1])
        arg <- list()
        n1 <- 0
        for (i in 1:n.threads) {
            n0 <- n1 + 1
            n1 <- n1 + nt[i]
            ind <- n0:n1
            arg[[i]] <- list(object = object, type = type, se.fit = se.fit, 
                terms = terms, exclude = exclude, block.size = block.size, 
                newdata.guaranteed = newdata.guaranteed, na.action = na.action)
            arg[[i]]$object$model <- object$model[1:2, ]
            if (missing(newdata)) {
                arg[[i]]$newdata <- object$model[ind, ]
            }
            else {
                arg[[i]]$newdata <- newdata[ind, ]
            }
        }
        if (!missing(newdata)) 
            rm(newdata)
        rm(object)
        gc()
        res <- parallel::parLapply(cluster, arg, pabapr)
        gc()
        if (type == "lpmatrix") {
            X <- res[[1]]
            for (i in 2:length(res)) X <- rbind(X, res[[i]])
            return(X)
        }
        else if (se.fit == TRUE) {
            rt <- list(fit = res[[1]]$fit, se.fit = res[[1]]$se.fit)
            if (type == "terms") {
                for (i in 2:length(res)) {
                  rt$fit <- rbind(rt$fit, res[[i]]$fit)
                  rt$se.fit <- rbind(rt$se.fit, res[[i]]$se.fit)
                }
            }
            else {
                for (i in 2:length(res)) {
                  rt$fit <- c(rt$fit, res[[i]]$fit)
                  rt$se.fit <- c(rt$se.fit, res[[i]]$se.fit)
                }
            }
            return(rt)
        }
        else {
            rt <- res[[1]]
            if (type == "terms") {
                for (i in 2:length(res)) rt <- rbind(rt, res[[i]])
            }
            else {
                for (i in 2:length(res)) rt <- c(rt, res[[i]])
            }
            return(rt)
        }
    }
}


mroot <- function (A, rank = NULL, method = "chol") 
{
    if (is.null(rank)) 
        rank <- 0
    if (!isTRUE(all.equal(A, t(A)))) 
        stop("Supplied matrix not symmetric")
    if (method == "svd") {
        um <- La.svd(A)
        if (sum(um$d != sort(um$d, decreasing = TRUE)) > 0) 
            stop("singular values not returned in order")
        if (rank < 1) {
            rank <- dim(A)[1]
            if (um$d[1] <= 0) 
                rank <- 1
            else while (rank > 0 && (um$d[rank]/um$d[1] < .Machine$double.eps || 
                all.equal(um$u[, rank], um$vt[rank, ]) != TRUE)) rank <- rank - 
                1
            if (rank == 0) 
                stop("Something wrong - matrix probably not +ve semi definite")
        }
        d <- um$d[1:rank]^0.5
        return(t(t(um$u[, 1:rank]) * as.vector(d)))
    }
    else if (method == "chol") {
        L <- suppressWarnings(chol(A, pivot = TRUE, tol = 0))
        piv <- order(attr(L, "pivot"))
        r <- attr(L, "rank")
        p <- ncol(L)
        if (r < p) 
            L[(r + 1):p, (r + 1):p] <- 0
        if (rank < 1) 
            rank <- r
        L <- L[, piv, drop = FALSE]
        L <- t(L[1:rank, , drop = FALSE])
        return(L)
    }
    else stop("method not recognised.")
}


smooth.construct.cs.smooth.spec <- function (object, data, knots) 
{
    attr(object, "shrink") <- 0.1
    object <- smooth.construct.cr.smooth.spec(object, data, knots)
    class(object) <- "cs.smooth"
    object
}


smooth.construct.t2.smooth.spec <- function (object, data, knots) 
{
    m <- length(object$margin)
    Xm <- list()
    Sm <- list()
    nr <- r <- d <- array(0, m)
    Pm <- list()
    C <- NULL
    object$plot.me <- TRUE
    for (i in 1:m) {
        knt <- dat <- list()
        term <- object$margin[[i]]$term
        for (j in 1:length(term)) {
            dat[[term[j]]] <- data[[term[j]]]
            knt[[term[j]]] <- knots[[term[j]]]
        }
        object$margin[[i]] <- smooth.construct(object$margin[[i]], 
            dat, knt)
        Xm[[i]] <- object$margin[[i]]$X
        if (!is.null(object$margin[[i]]$te.ok)) {
            if (object$margin[[i]]$te.ok == 0) 
                stop("attempt to use unsuitable marginal smooth class")
            if (object$margin[[i]]$te.ok == 2) 
                object$plot.me <- FALSE
        }
        if (length(object$margin[[i]]$S) > 1) 
            stop("Sorry, tensor products of smooths with multiple penalties are not supported.")
        Sm[[i]] <- object$margin[[i]]$S[[1]]
        d[i] <- nrow(Sm[[i]])
        r[i] <- object$margin[[i]]$rank
        nr[i] <- object$margin[[i]]$null.space.dim
        np <- nat.param(Xm[[i]], Sm[[i]], rank = r[i], type = 3, 
            unit.fnorm = TRUE)
        Xm[[i]] <- np$X
        dS <- rep(0, ncol(Xm[[i]]))
        dS[1:r[i]] <- 1
        Sm[[i]] <- diag(dS)
        Pm[[i]] <- np$P
        if (!is.null(object$margin[[i]]$C) && nrow(object$margin[[i]]$C) == 
            0) 
            C <- matrix(0, 0, 0)
    }
    X <- t2.model.matrix(Xm, r, full = object$full, ord = object$ord)
    sub.cols <- attr(X, "sub.cols")
    nsc <- length(sub.cols)
    S <- list()
    cxn <- c(0, cumsum(sub.cols))
    if (nsc > 0) 
        for (j in 1:nsc) {
            dd <- rep(0, ncol(X))
            dd[(cxn[j] + 1):cxn[j + 1]] <- 1
            S[[j]] <- diag(dd)
        }
    names(S) <- attr(X, "p.lab")
    if (length(object$fx) == 1) 
        object$fx <- rep(object$fx, nsc)
    else if (length(object$fx) != nsc) {
        warning("fx length wrong from t2 term: ignored")
        object$fx <- rep(FALSE, nsc)
    }
    if (!is.null(object$sp) && length(object$sp) != nsc) {
        object$sp <- NULL
        warning("length of sp incorrect in t2: ignored")
    }
    object$null.space.dim <- ncol(X) - sum(sub.cols)
    nup <- sum(sub.cols[1:nsc])
    if (is.null(C)) {
        if (object$null.space.dim == 0) {
            C <- matrix(0, 0, 0)
        }
        else {
            if (object$null.space.dim == 1) 
                C <- ncol(X)
            else C <- matrix(c(rep(0, nup), colSums(X[, (nup + 
                1):ncol(X), drop = FALSE])), 1, ncol(X))
        }
    }
    object$X <- X
    object$S <- S
    object$C <- C
    if (is.matrix(C) && nrow(C) == 0) 
        object$Cp <- NULL
    else object$Cp <- matrix(colSums(X), 1, ncol(X))
    object$df <- ncol(X)
    object$rank <- sub.cols[1:nsc]
    object$P <- Pm
    object$fixed <- as.logical(sum(object$fx))
    class(object) <- "t2.smooth"
    object
}


Predict.matrix.sw <- function (object, data) 
{
    x <- get.var(object$term[1], data)
    y <- get.var(object$term[2], data)
    X <- soap.basis(object$sd, x, y, film = FALSE, wiggly = TRUE, 
        penalty = FALSE)$X
    X <- t(object$irng * t(X))
    X
}


cSplineDes <- function (x, knots, ord = 4) 
{
    nk <- length(knots)
    if (ord < 2) 
        stop("order too low")
    if (nk < ord) 
        stop("too few knots")
    knots <- sort(knots)
    k1 <- knots[1]
    if (min(x) < k1 || max(x) > knots[nk]) 
        stop("x out of range")
    xc <- knots[nk - ord + 1]
    knots <- c(k1 - (knots[nk] - knots[(nk - ord + 1):(nk - 1)]), 
        knots)
    ind <- x > xc
    X1 <- splines::splineDesign(knots, x, ord, outer.ok = TRUE)
    x[ind] <- x[ind] - max(knots) + k1
    if (sum(ind)) {
        X2 <- splines::splineDesign(knots, x[ind], ord, outer.ok = TRUE)
        X1[ind, ] <- X1[ind, ] + X2
    }
    X1
}


Rrank <- function (R, tol = .Machine$double.eps^0.9) 
{
    rank <- m <- ncol(R)
    ok <- TRUE
    while (ok) {
        Rcond <- .C(C_R_cond, R = as.double(R), r = as.integer(m), 
            c = as.integer(rank), work = as.double(rep(0, 4 * 
                m)), Rcond = as.double(1))$Rcond
        if (Rcond * tol < 1) 
            ok <- FALSE
        else rank <- rank - 1
    }
    rank
}


Predict.matrix.cs.smooth <- function (object, data) 
{
    Predict.matrix.cr.smooth(object, data)
}


smooth.construct.ps.smooth.spec <- function (object, data, knots) 
{
    if (length(object$p.order) == 1) 
        m <- rep(object$p.order, 2)
    else m <- object$p.order
    m[is.na(m)] <- 2
    object$p.order <- m
    if (object$bs.dim < 0) 
        object$bs.dim <- max(10, m[1] + 1)
    nk <- object$bs.dim - m[1]
    if (nk <= 0) 
        stop("basis dimension too small for b-spline order")
    if (length(object$term) != 1) 
        stop("Basis only handles 1D smooths")
    x <- data[[object$term]]
    k <- knots[[object$term]]
    if (is.null(k)) {
        xl <- min(x)
        xu <- max(x)
    }
    else if (length(k) == 2) {
        xl <- min(k)
        xu <- max(k)
        if (xl > min(x) || xu < max(x)) 
            stop("knot range does not include data")
    }
    if (is.null(k) || length(k) == 2) {
        xr <- xu - xl
        xl <- xl - xr * 0.001
        xu <- xu + xr * 0.001
        dx <- (xu - xl)/(nk - 1)
        k <- seq(xl - dx * (m[1] + 1), xu + dx * (m[1] + 1), 
            length = nk + 2 * m[1] + 2)
    }
    else {
        if (length(k) != nk + 2 * m[1] + 2) 
            stop(paste("there should be ", nk + 2 * m[1] + 2, 
                " supplied knots"))
    }
    if (is.null(object$deriv)) 
        object$deriv <- 0
    object$X <- splines::spline.des(k, x, m[1] + 2, x * 0 + object$deriv)$design
    if (!is.null(k)) {
        if (sum(colSums(object$X) == 0) > 0) 
            warning("there is *no* information about some basis coefficients")
    }
    if (length(unique(x)) < object$bs.dim) 
        warning("basis dimension is larger than number of unique covariates")
    if (is.null(object$mono)) 
        object$mono <- 0
    if (object$mono != 0) {
        p <- ncol(object$X)
        B <- matrix(as.numeric(rep(1:p, p) >= rep(1:p, each = p)), 
            p, p)
        if (object$mono < 0) 
            B[, 2:p] <- -B[, 2:p]
        object$X <- object$X %*% B
        object$g.index <- c(FALSE, rep(TRUE, p - 1))
        object$D <- cbind(0, -diff(diag(p - 1)))
        object$S <- list(crossprod(object$D))
        object$B <- B
        object$rank <- p - 2
        object$null.space.dim <- 2
    }
    else {
        object$D <- S <- if (m[2] > 0) 
            diff(diag(object$bs.dim), differences = m[2])
        else diag(object$bs.dim)
        object$S <- list(crossprod(S))
        object$rank <- object$bs.dim - m[2]
        object$null.space.dim <- m[2]
    }
    object$knots <- k
    object$m <- m
    class(object) <- "pspline.smooth"
    object
}


uniquecombs <- function (x) 
{
    if (is.null(x)) 
        stop("x is null")
    if (is.null(nrow(x)) || is.null(ncol(x))) 
        x <- data.frame(x)
    if (inherits(x, "data.frame")) {
        xo <- x
        x <- data.matrix(xo)
    }
    else xo <- NULL
    ind <- rep(0, nrow(x))
    res <- .C(C_RuniqueCombs, x = as.double(x), ind = as.integer(ind), 
        r = as.integer(nrow(x)), c = as.integer(ncol(x)))
    n <- res$r * res$c
    x <- matrix(res$x[1:n], res$r, res$c)
    if (!is.null(xo)) {
        x <- as.data.frame(x)
        names(x) <- names(xo)
        for (i in 1:ncol(xo)) if (is.factor(xo[, i])) {
            xoi <- levels(xo[, i])
            x[, i] <- if (is.ordered(xo[, i])) 
                ordered(x[, i], levels = 1:length(xoi), labels = xoi)
            else factor(x[, i], levels = 1:length(xoi), labels = xoi)
            contrasts(x[, i]) <- contrasts(xo[, i])
        }
    }
    attr(x, "index") <- res$ind + 1
    x
}


Predict.matrix.duchon.spline <- function (object, data) 
{
    nk <- nrow(object$knt)
    for (i in 1:object$dim) {
        xx <- data[[object$term[i]]]
        if (i == 1) {
            n <- length(xx)
            x <- matrix(xx, n, object$dim)
        }
        else {
            if (n != length(xx)) 
                stop("arguments of smooth not same dimension")
            x[, i] <- xx
        }
    }
    x <- sweep(x, 2, object$shift)
    if (n > nk) {
        n.chunk <- n%/%nk
        for (i in 1:n.chunk) {
            ind <- 1:nk + (i - 1) * nk
            Xc <- DuchonE(x = x[ind, , drop = FALSE], xk = object$knt, 
                m = object$p.order[1], s = object$p.order[2], 
                n = object$dim)
            Xc <- cbind(Xc %*% object$UZ, DuchonT(x = x[ind, 
                , drop = FALSE], m = object$p.order[1], n = object$dim))
            if (i == 1) 
                X <- Xc
            else {
                X <- rbind(X, Xc)
                rm(Xc)
            }
        }
        if (n > ind[nk]) {
            ind <- (ind[nk] + 1):n
            Xc <- DuchonE(x = x[ind, , drop = FALSE], xk = object$knt, 
                m = object$p.order[1], s = object$p.order[2], 
                n = object$dim)
            Xc <- cbind(Xc %*% object$UZ, DuchonT(x = x[ind, 
                , drop = FALSE], m = object$p.order[1], n = object$dim))
            X <- rbind(X, Xc)
            rm(Xc)
        }
    }
    else {
        X <- DuchonE(x = x, xk = object$knt, m = object$p.order[1], 
            s = object$p.order[2], n = object$dim)
        X <- cbind(X %*% object$UZ, DuchonT(x = x, m = object$p.order[1], 
            n = object$dim))
    }
    X
}


vis.gam <- function (x, view = NULL, cond = list(), n.grid = 30, too.far = 0, 
    col = NA, color = "heat", contour.col = NULL, se = -1, type = "link", 
    plot.type = "persp", zlim = NULL, nCol = 50, ...) 
{
    fac.seq <- function(fac, n.grid) {
        fn <- length(levels(fac))
        gn <- n.grid
        if (fn > gn) 
            mf <- factor(levels(fac))[1:gn]
        else {
            ln <- floor(gn/fn)
            mf <- rep(levels(fac)[fn], gn)
            mf[1:(ln * fn)] <- rep(levels(fac), rep(ln, fn))
            mf <- factor(mf, levels = levels(fac))
        }
        mf
    }
    dnm <- names(list(...))
    v.names <- names(x$var.summary)
    if (is.null(view)) {
        k <- 0
        view <- rep("", 2)
        for (i in 1:length(v.names)) {
            ok <- TRUE
            if (is.matrix(x$var.summary[[i]])) 
                ok <- FALSE
            else if (is.factor(x$var.summary[[i]])) {
                if (length(levels(x$var.summary[[i]])) <= 1) 
                  ok <- FALSE
            }
            else {
                if (length(unique(x$var.summary[[i]])) == 1) 
                  ok <- FALSE
            }
            if (ok) {
                k <- k + 1
                view[k] <- v.names[i]
            }
            if (k == 2) 
                break
        }
        if (k < 2) 
            stop("Model does not seem to have enough terms to do anything useful")
    }
    else {
        if (sum(view %in% v.names) != 2) 
            stop(gettextf("view variables must be one of %s", 
                paste(v.names, collapse = ", ")))
        for (i in 1:2) if (!inherits(x$var.summary[[view[i]]], 
            c("numeric", "factor"))) 
            stop("Don't know what to do with parametric terms that are not simple numeric or factor variables")
    }
    ok <- TRUE
    for (i in 1:2) if (is.factor(x$var.summary[[view[i]]])) {
        if (length(levels(x$var.summary[[view[i]]])) <= 1) 
            ok <- FALSE
    }
    else {
        if (length(unique(x$var.summary[[view[i]]])) <= 1) 
            ok <- FALSE
    }
    if (!ok) 
        stop(gettextf("View variables must contain more than one value. view = c(%s,%s).", 
            view[1], view[2]))
    if (is.factor(x$var.summary[[view[1]]])) 
        m1 <- fac.seq(x$var.summary[[view[1]]], n.grid)
    else {
        r1 <- range(x$var.summary[[view[1]]])
        m1 <- seq(r1[1], r1[2], length = n.grid)
    }
    if (is.factor(x$var.summary[[view[2]]])) 
        m2 <- fac.seq(x$var.summary[[view[2]]], n.grid)
    else {
        r2 <- range(x$var.summary[[view[2]]])
        m2 <- seq(r2[1], r2[2], length = n.grid)
    }
    v1 <- rep(m1, n.grid)
    v2 <- rep(m2, rep(n.grid, n.grid))
    newd <- data.frame(matrix(0, n.grid * n.grid, 0))
    for (i in 1:length(x$var.summary)) {
        ma <- cond[[v.names[i]]]
        if (is.null(ma)) {
            ma <- x$var.summary[[i]]
            if (is.numeric(ma)) 
                ma <- ma[2]
        }
        if (is.matrix(x$var.summary[[i]])) 
            newd[[i]] <- matrix(ma, n.grid * n.grid, ncol(x$var.summary[[i]]), 
                byrow = TRUE)
        else newd[[i]] <- rep(ma, n.grid * n.grid)
    }
    names(newd) <- v.names
    newd[[view[1]]] <- v1
    newd[[view[2]]] <- v2
    if (type == "link") 
        zlab <- paste("linear predictor")
    else if (type == "response") 
        zlab <- type
    else stop("type must be \"link\" or \"response\"")
    fv <- predict.gam(x, newdata = newd, se.fit = TRUE, type = type)
    z <- fv$fit
    if (too.far > 0) {
        ex.tf <- exclude.too.far(v1, v2, x$model[, view[1]], 
            x$model[, view[2]], dist = too.far)
        fv$se.fit[ex.tf] <- fv$fit[ex.tf] <- NA
    }
    if (is.factor(m1)) {
        m1 <- as.numeric(m1)
        m1 <- seq(min(m1) - 0.5, max(m1) + 0.5, length = n.grid)
    }
    if (is.factor(m2)) {
        m2 <- as.numeric(m2)
        m2 <- seq(min(m1) - 0.5, max(m2) + 0.5, length = n.grid)
    }
    if (se <= 0) {
        old.warn <- options(warn = -1)
        av <- matrix(c(0.5, 0.5, rep(0, n.grid - 1)), n.grid, 
            n.grid - 1)
        options(old.warn)
        max.z <- max(z, na.rm = TRUE)
        z[is.na(z)] <- max.z * 10000
        z <- matrix(z, n.grid, n.grid)
        surf.col <- t(av) %*% z %*% av
        surf.col[surf.col > max.z * 2] <- NA
        if (!is.null(zlim)) {
            if (length(zlim) != 2 || zlim[1] >= zlim[2]) 
                stop("Something wrong with zlim")
            min.z <- zlim[1]
            max.z <- zlim[2]
        }
        else {
            min.z <- min(fv$fit, na.rm = TRUE)
            max.z <- max(fv$fit, na.rm = TRUE)
        }
        surf.col <- surf.col - min.z
        surf.col <- surf.col/(max.z - min.z)
        surf.col <- round(surf.col * nCol)
        con.col <- 1
        if (color == "heat") {
            pal <- heat.colors(nCol)
            con.col <- 3
        }
        else if (color == "topo") {
            pal <- topo.colors(nCol)
            con.col <- 2
        }
        else if (color == "cm") {
            pal <- cm.colors(nCol)
            con.col <- 1
        }
        else if (color == "terrain") {
            pal <- terrain.colors(nCol)
            con.col <- 2
        }
        else if (color == "gray" || color == "bw") {
            pal <- gray(seq(0.1, 0.9, length = nCol))
            con.col <- 1
        }
        else stop("color scheme not recognised")
        if (is.null(contour.col)) 
            contour.col <- con.col
        surf.col[surf.col < 1] <- 1
        surf.col[surf.col > nCol] <- nCol
        if (is.na(col)) 
            col <- pal[as.array(surf.col)]
        z <- matrix(fv$fit, n.grid, n.grid)
        if (plot.type == "contour") {
            stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
                ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), 
                ifelse("main" %in% dnm, "", ",main=zlab"), ",...)", 
                sep = "")
            if (color != "bw") {
                txt <- paste("image(m1,m2,z,col=pal,zlim=c(min.z,max.z)", 
                  stub, sep = "")
                eval(parse(text = txt))
                txt <- paste("contour(m1,m2,z,col=contour.col,zlim=c(min.z,max.z)", 
                  ifelse("add" %in% dnm, "", ",add=TRUE"), ",...)", 
                  sep = "")
                eval(parse(text = txt))
            }
            else {
                txt <- paste("contour(m1,m2,z,col=1,zlim=c(min.z,max.z)", 
                  stub, sep = "")
                eval(parse(text = txt))
            }
        }
        else {
            stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
                ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), 
                ifelse("zlab" %in% dnm, "", ",zlab=zlab"), ",...)", 
                sep = "")
            if (color == "bw") {
                op <- par(bg = "white")
                txt <- paste("persp(m1,m2,z,col=\"white\",zlim=c(min.z,max.z) ", 
                  stub, sep = "")
                eval(parse(text = txt))
                par(op)
            }
            else {
                txt <- paste("persp(m1,m2,z,col=col,zlim=c(min.z,max.z)", 
                  stub, sep = "")
                eval(parse(text = txt))
            }
        }
    }
    else {
        if (color == "bw" || color == "gray") {
            subs <- paste("grey are +/-", se, "s.e.")
            lo.col <- "gray"
            hi.col <- "gray"
        }
        else {
            subs <- paste("red/green are +/-", se, "s.e.")
            lo.col <- "green"
            hi.col <- "red"
        }
        if (!is.null(zlim)) {
            if (length(zlim) != 2 || zlim[1] >= zlim[2]) 
                stop("Something wrong with zlim")
            min.z <- zlim[1]
            max.z <- zlim[2]
        }
        else {
            z.max <- max(fv$fit + fv$se.fit * se, na.rm = TRUE)
            z.min <- min(fv$fit - fv$se.fit * se, na.rm = TRUE)
        }
        zlim <- c(z.min, z.max)
        z <- fv$fit - fv$se.fit * se
        z <- matrix(z, n.grid, n.grid)
        if (plot.type == "contour") 
            warning("sorry no option for contouring with errors: try plot.gam")
        stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
            ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), ifelse("zlab" %in% 
                dnm, "", ",zlab=zlab"), ifelse("sub" %in% dnm, 
                "", ",sub=subs"), ",...)", sep = "")
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=lo.col"), stub, sep = "")
        eval(parse(text = txt))
        par(new = TRUE)
        z <- fv$fit
        z <- matrix(z, n.grid, n.grid)
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=\"black\""), stub, sep = "")
        eval(parse(text = txt))
        par(new = TRUE)
        z <- fv$fit + se * fv$se.fit
        z <- matrix(z, n.grid, n.grid)
        txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
            dnm, "", ",border=hi.col"), stub, sep = "")
        eval(parse(text = txt))
    }
}


Predict.matrix.cyclic.smooth <- function (object, data) 
{
    pred.mat <- function(x, knots, BD) {
        j <- x
        n <- length(knots)
        h <- knots[2:n] - knots[1:(n - 1)]
        if (max(x) > max(knots) || min(x) < min(knots)) 
            x <- cwrap(min(knots), max(knots), x)
        for (i in n:2) j[x <= knots[i]] <- i
        j1 <- hj <- j - 1
        j[j == n] <- 1
        I <- diag(n - 1)
        X <- BD[j1, , drop = FALSE] * as.numeric(knots[j1 + 1] - 
            x)^3/as.numeric(6 * h[hj]) + BD[j, , drop = FALSE] * 
            as.numeric(x - knots[j1])^3/as.numeric(6 * h[hj]) - 
            BD[j1, , drop = FALSE] * as.numeric(h[hj] * (knots[j1 + 
                1] - x)/6) - BD[j, , drop = FALSE] * as.numeric(h[hj] * 
            (x - knots[j1])/6) + I[j1, , drop = FALSE] * as.numeric((knots[j1 + 
            1] - x)/h[hj]) + I[j, , drop = FALSE] * as.numeric((x - 
            knots[j1])/h[hj])
        X
    }
    x <- data[[object$term]]
    if (length(x) < 1) 
        stop("no data to predict at")
    X <- pred.mat(x, object$xp, object$BD)
    X
}


rTweedie <- function (mu, p = 1.5, phi = 1) 
{
    if (p <= 1 || p >= 2) 
        stop("p must be in (1,2)")
    if (sum(mu < 0)) 
        stop("mean, mu, must be non negative")
    if (phi <= 0) 
        stop("scale parameter must be positive")
    lambda <- mu^(2 - p)/((2 - p) * phi)
    shape <- (2 - p)/(p - 1)
    scale <- phi * (p - 1) * mu^(p - 1)
    n.sim <- length(mu)
    N <- rpois(length(lambda), lambda)
    gs <- rep(scale, N)
    y <- rgamma(gs * 0 + 1, shape = shape, scale = gs)
    lab <- rep(1:length(N), N)
    o <- .C(C_psum, y = as.double(rep(0, n.sim)), as.double(y), 
        as.integer(lab), as.integer(length(lab)))
    o$y
}


pcls <- function (M) 
{
    nar <- c(length(M$y), length(M$p), dim(M$Ain)[1], dim(M$C)[1], 
        0)
    H <- 0
    if (nrow(M$X) != nar[1]) 
        stop("nrow(M$X) != length(M$y)")
    if (ncol(M$X) != nar[2]) 
        stop("ncol(M$X) != length(M$p)")
    if (length(M$w) != nar[1]) 
        stop("length(M$w) != length(M$y)")
    if (nar[3] != length(M$bin)) 
        stop("nrow(M$Ain) != length(M$bin)")
    if (nrow(M$Ain) > 0) {
        if (ncol(M$Ain) != nar[2]) 
            stop("nrow(M$Ain) != length(M$p)")
        res <- as.numeric(M$Ain %*% M$p) - as.numeric(M$bin)
        if (sum(res < 0) > 0) 
            stop("initial parameters not feasible")
        res <- abs(res)
        if (sum(res < .Machine$double.eps^0.5) > 0) 
            warning("initial point very close to some inequality constraints")
        res <- mean(res)
        if (res < .Machine$double.eps^0.5) 
            warning("initial parameters very close to inequality constraints")
    }
    if (nrow(M$C) > 0) 
        if (ncol(M$C) != nar[2]) 
            stop("ncol(M$C) != length(M$p)")
    if (length(M$S) != length(M$off)) 
        stop("M$S and M$off have different lengths")
    if (length(M$S) != length(M$sp)) 
        stop("M$sp has different length to M$S and M$off")
    m <- length(M$S)
    Sa <- array(0, 0)
    df <- 0
    if (m > 0) 
        for (i in 1:m) {
            Sa <- c(Sa, M$S[[i]])
            df[i] <- nrow(M$S[[i]])
            if (M$off[i] + df[i] - 1 > nar[2]) 
                stop(gettextf("M$S[%d] is too large given M$off[%d]", 
                  i, i))
        }
    qra.exist <- FALSE
    if (ncol(M$X) > nrow(M$X)) {
        if (m > 0) 
            stop("Penalized model matrix must have no more columns than rows")
        else {
            qra <- qr(t(M$C))
            j <- nrow(M$C)
            k <- ncol(M$X)
            M$X <- t(qr.qty(qra, t(M$X))[(j + 1):k, ])
            M$Ain <- t(qr.qty(qra, t(M$Ain))[(j + 1):k, ])
            M$C <- matrix(0, 0, 0)
            M$p <- rep(0, ncol(M$X))
            nar[2] <- length(M$p)
            nar[4] <- 0
            qra.exist <- TRUE
            if (ncol(M$X) > nrow(M$X)) 
                stop("Model matrix not full column rank")
        }
    }
    o <- .C(C_RPCLS, as.double(M$X), as.double(M$p), as.double(M$y), 
        as.double(M$w), as.double(M$Ain), as.double(M$bin), as.double(M$C), 
        as.double(H), as.double(Sa), as.integer(M$off), as.integer(df), 
        as.double(M$sp), as.integer(length(M$off)), as.integer(nar))
    p <- array(o[[2]], length(M$p))
    if (qra.exist) 
        p <- qr.qy(qra, c(rep(0, j), p))
    p
}


summary.gam <- function (object, dispersion = NULL, freq = FALSE, p.type = 0, 
    ...) 
{
    pinv <- function(V, M, rank.tol = 1e-06) {
        D <- eigen(V, symmetric = TRUE)
        M1 <- length(D$values[D$values > rank.tol * D$values[1]])
        if (M > M1) 
            M <- M1
        if (M + 1 <= length(D$values)) 
            D$values[(M + 1):length(D$values)] <- 1
        D$values <- 1/D$values
        if (M + 1 <= length(D$values)) 
            D$values[(M + 1):length(D$values)] <- 0
        res <- D$vectors %*% (D$values * t(D$vectors))
        attr(res, "rank") <- M
        res
    }
    if (is.null(object$R)) {
        warning("p-values for any terms that can be penalized to zero will be unreliable: refit model to fix this.")
        useR <- FALSE
    }
    else useR <- TRUE
    if (p.type < -1) 
        useR <- FALSE
    if (p.type != 0) 
        warning("p.type!=0 is deprecated, and liable to be removed in future")
    p.table <- pTerms.table <- s.table <- NULL
    if (freq) 
        covmat <- object$Ve
    else covmat <- object$Vp
    name <- names(object$edf)
    dimnames(covmat) <- list(name, name)
    covmat.unscaled <- covmat/object$sig2
    est.disp <- object$scale.estimated
    if (!is.null(dispersion)) {
        covmat <- dispersion * covmat.unscaled
        object$Ve <- object$Ve * dispersion/object$sig2
        object$Vp <- object$Vp * dispersion/object$sig2
        est.disp <- FALSE
    }
    else dispersion <- object$sig2
    se <- diag(covmat)^0.5
    residual.df <- length(object$y) - sum(object$edf)
    if (sum(object$nsdf) > 0) {
        if (length(object$nsdf) > 1) {
            pstart <- attr(object$nsdf, "pstart")
            ind <- rep(0, 0)
            for (i in 1:length(object$nsdf)) if (object$nsdf[i] > 
                0) 
                ind <- c(ind, pstart[i]:(pstart[i] + object$nsdf[i] - 
                  1))
        }
        else {
            pstart <- 1
            ind <- 1:object$nsdf
        }
        p.coeff <- object$coefficients[ind]
        p.se <- se[ind]
        p.t <- p.coeff/p.se
        if (!est.disp) {
            p.pv <- 2 * pnorm(abs(p.t), lower.tail = FALSE)
            p.table <- cbind(p.coeff, p.se, p.t, p.pv)
            dimnames(p.table) <- list(names(p.coeff), c("Estimate", 
                "Std. Error", "z value", "Pr(>|z|)"))
        }
        else {
            p.pv <- 2 * pt(abs(p.t), df = residual.df, lower.tail = FALSE)
            p.table <- cbind(p.coeff, p.se, p.t, p.pv)
            dimnames(p.table) <- list(names(p.coeff), c("Estimate", 
                "Std. Error", "t value", "Pr(>|t|)"))
        }
    }
    else {
        p.coeff <- p.t <- p.pv <- array(0, 0)
    }
    pterms <- if (is.list(object$pterms)) 
        object$pterms
    else list(object$pterms)
    if (!is.list(object$assign)) 
        object$assign <- list(object$assign)
    npt <- length(unlist(lapply(pterms, attr, "term.labels")))
    if (npt > 0) 
        pTerms.df <- pTerms.chi.sq <- pTerms.pv <- array(0, npt)
    term.labels <- rep("", 0)
    k <- 0
    for (j in 1:length(pterms)) {
        tlj <- attr(pterms[[j]], "term.labels")
        nt <- length(tlj)
        if (j > 1 && nt > 0) 
            tlj <- paste(tlj, j - 1, sep = ".")
        term.labels <- c(term.labels, tlj)
        if (nt > 0) {
            np <- length(object$assign[[j]])
            ind <- pstart[j] - 1 + 1:np
            Vb <- covmat[ind, ind, drop = FALSE]
            bp <- array(object$coefficients[ind], np)
            for (i in 1:nt) {
                k <- k + 1
                ind <- object$assign[[j]] == i
                b <- bp[ind]
                V <- Vb[ind, ind]
                if (length(b) == 1) {
                  V <- 1/V
                  pTerms.df[k] <- nb <- 1
                  pTerms.chi.sq[k] <- V * b * b
                }
                else {
                  V <- pinv(V, length(b), rank.tol = .Machine$double.eps^0.5)
                  pTerms.df[k] <- nb <- attr(V, "rank")
                  pTerms.chi.sq[k] <- t(b) %*% V %*% b
                }
                if (!est.disp) 
                  pTerms.pv[k] <- pchisq(pTerms.chi.sq[k], df = nb, 
                    lower.tail = FALSE)
                else pTerms.pv[k] <- pf(pTerms.chi.sq[k]/nb, 
                  df1 = nb, df2 = residual.df, lower.tail = FALSE)
            }
        }
    }
    if (npt) {
        attr(pTerms.pv, "names") <- term.labels
        if (!est.disp) {
            pTerms.table <- cbind(pTerms.df, pTerms.chi.sq, pTerms.pv)
            dimnames(pTerms.table) <- list(term.labels, c("df", 
                "Chi.sq", "p-value"))
        }
        else {
            pTerms.table <- cbind(pTerms.df, pTerms.chi.sq/pTerms.df, 
                pTerms.pv)
            dimnames(pTerms.table) <- list(term.labels, c("df", 
                "F", "p-value"))
        }
    }
    else {
        pTerms.df <- pTerms.chi.sq <- pTerms.pv <- array(0, 0)
    }
    m <- length(object$smooth)
    if (p.type < 0) {
        kmax <- 0
        for (i in 1:m) {
            start <- object$smooth[[i]]$first.para
            stop <- object$smooth[[i]]$last.para
            k <- stop - start + 1
            if (k > kmax) 
                kmax <- k
        }
    }
    df <- edf1 <- edf <- s.pv <- chi.sq <- array(0, m)
    if (m > 0) {
        if (p.type < 5) {
            if (useR) 
                X <- object$R
            else {
                sub.samp <- max(1000, 2 * length(object$coefficients))
                if (nrow(object$model) > sub.samp) {
                  seed <- try(get(".Random.seed", envir = .GlobalEnv), 
                    silent = TRUE)
                  if (inherits(seed, "try-error")) {
                    runif(1)
                    seed <- get(".Random.seed", envir = .GlobalEnv)
                  }
                  kind <- RNGkind(NULL)
                  RNGkind("default", "default")
                  set.seed(11)
                  ind <- sample(1:nrow(object$model), sub.samp, 
                    replace = FALSE)
                  X <- predict(object, object$model[ind, ], type = "lpmatrix")
                  RNGkind(kind[1], kind[2])
                  assign(".Random.seed", seed, envir = .GlobalEnv)
                }
                else {
                  X <- model.matrix(object)
                }
                X <- X[!is.na(rowSums(X)), ]
            }
        }
        for (i in 1:m) {
            start <- object$smooth[[i]]$first.para
            stop <- object$smooth[[i]]$last.para
            if (p.type == 5) {
                V <- object$Ve[start:stop, start:stop, drop = FALSE]
            }
            else V <- object$Vp[start:stop, start:stop, drop = FALSE]
            p <- object$coefficients[start:stop]
            edf1[i] <- edf[i] <- sum(object$edf[start:stop])
            if (!is.null(object$edf1)) 
                edf1[i] <- sum(object$edf1[start:stop])
            if (p.type == 5) {
                M1 <- object$smooth[[i]]$df
                M <- min(M1, ceiling(2 * sum(object$edf[start:stop])))
                V <- pinv(V, M)
                chi.sq[i] <- t(p) %*% V %*% p
                df[i] <- attr(V, "rank")
            }
            else {
                Xt <- X[, start:stop, drop = FALSE]
                fx <- if (inherits(object$smooth[[i]], "tensor.smooth") && 
                  !is.null(object$smooth[[i]]$fx)) 
                  all(object$smooth[[i]]$fx)
                else object$smooth[[i]]$fixed
                if (!fx && object$smooth[[i]]$null.space.dim == 
                  0 && !is.null(object$R)) {
                  res <- reTest(object, i)
                }
                else {
                  df[i] <- min(ncol(Xt), edf1[i])
                  if (est.disp) 
                    rdf <- residual.df
                  else rdf <- -1
                  res <- testStat(p, Xt, V, df[i], type = p.type, 
                    res.df = rdf)
                }
                df[i] <- res$rank
                chi.sq[i] <- res$stat
                s.pv[i] <- res$pval
            }
            names(chi.sq)[i] <- object$smooth[[i]]$label
            if (p.type == 5) {
                if (!est.disp) 
                  s.pv[i] <- pchisq(chi.sq[i], df = df[i], lower.tail = FALSE)
                else s.pv[i] <- pf(chi.sq[i]/df[i], df1 = df[i], 
                  df2 = residual.df, lower.tail = FALSE)
                if (df[i] < 0.1) 
                  s.pv[i] <- NA
            }
        }
        if (!est.disp) {
            if (p.type == 5) {
                s.table <- cbind(edf, df, chi.sq, s.pv)
                dimnames(s.table) <- list(names(chi.sq), c("edf", 
                  "Est.rank", "Chi.sq", "p-value"))
            }
            else {
                s.table <- cbind(edf, df, chi.sq, s.pv)
                dimnames(s.table) <- list(names(chi.sq), c("edf", 
                  "Ref.df", "Chi.sq", "p-value"))
            }
        }
        else {
            if (p.type == 5) {
                s.table <- cbind(edf, df, chi.sq/df, s.pv)
                dimnames(s.table) <- list(names(chi.sq), c("edf", 
                  "Est.rank", "F", "p-value"))
            }
            else {
                s.table <- cbind(edf, df, chi.sq/df, s.pv)
                dimnames(s.table) <- list(names(chi.sq), c("edf", 
                  "Ref.df", "F", "p-value"))
            }
        }
    }
    w <- as.numeric(object$prior.weights)
    mean.y <- sum(w * object$y)/sum(w)
    w <- sqrt(w)
    nobs <- nrow(object$model)
    r.sq <- if (inherits(object$family, "general.family") || 
        !is.null(object$family$no.r.sq)) 
        NULL
    else 1 - var(w * (as.numeric(object$y) - object$fitted.values)) * 
        (nobs - 1)/(var(w * (as.numeric(object$y) - mean.y)) * 
        residual.df)
    dev.expl <- (object$null.deviance - object$deviance)/object$null.deviance
    if (object$method %in% c("REML", "ML")) 
        object$method <- paste("-", object$method, sep = "")
    ret <- list(p.coeff = p.coeff, se = se, p.t = p.t, p.pv = p.pv, 
        residual.df = residual.df, m = m, chi.sq = chi.sq, s.pv = s.pv, 
        scale = dispersion, r.sq = r.sq, family = object$family, 
        formula = object$formula, n = nobs, dev.expl = dev.expl, 
        edf = edf, dispersion = dispersion, pTerms.pv = pTerms.pv, 
        pTerms.chi.sq = pTerms.chi.sq, pTerms.df = pTerms.df, 
        cov.unscaled = covmat.unscaled, cov.scaled = covmat, 
        p.table = p.table, pTerms.table = pTerms.table, s.table = s.table, 
        method = object$method, sp.criterion = object$gcv.ubre, 
        rank = object$rank, np = length(object$coefficients))
    class(ret) <- "summary.gam"
    ret
}


smooth.construct.re.smooth.spec <- function (object, data, knots) 
{
    if (!is.null(object$id)) 
        stop("random effects don't work with ids.")
    form <- as.formula(paste("~", paste(object$term, collapse = ":"), 
        "-1"))
    object$X <- model.matrix(form, data)
    object$bs.dim <- ncol(object$X)
    if (inherits(object, "tensor.smooth.spec")) {
        object$margin <- list()
        maxd <- maxi <- 0
        for (i in 1:object$dim) {
            form1 <- as.formula(paste("~", object$term[i], "-1"))
            object$margin[[i]] <- list(X = model.matrix(form1, 
                data), term = object$term[i], form = form1, by = "NA")
            class(object$margin[[i]]) <- "random.effect"
            d <- ncol(object$margin[[i]]$X)
            if (d > maxd) {
                maxi <- i
                maxd <- d
            }
        }
        if (maxi < object$dim) {
            ns <- object$dim
            ind <- 1:ns
            ind[maxi] <- ns
            ind[ns] <- maxi
            object$margin <- object$margin[ind]
            object$term <- rep("", 0)
            for (i in 1:ns) object$term <- c(object$term, object$margin[[i]]$term)
            object$label <- paste0(substr(object$label, 1, 2), 
                paste0(object$term, collapse = ","), ")", collapse = "")
            object$rind <- ind
            if (!is.null(object$xt$S)) 
                stop("Please put term with most levels last in 're' to avoid spoiling supplied penalties")
        }
    }
    if (is.null(object$xt$S)) {
        object$S <- list(diag(object$bs.dim))
        object$rank <- object$bs.dim
    }
    else {
        object$S <- if (is.list(object$xt$S)) 
            object$xt$S
        else list(object$xt$S)
        for (i in 1:length(object$S)) {
            if (ncol(object$S[[i]]) != object$bs.dim || nrow(object$S[[i]]) != 
                object$bs.dim) 
                stop("supplied S matrices are wrong diminsion")
        }
        object$rank <- object$xt$rank
    }
    object$null.space.dim <- 0
    object$C <- matrix(0, 0, ncol(object$X))
    object$form <- form
    object$side.constrain <- FALSE
    object$plot.me <- TRUE
    object$te.ok <- if (inherits(object, "tensor.smooth.spec")) 
        0
    else 2
    object$random <- TRUE
    class(object) <- if (inherits(object, "tensor.smooth.spec")) 
        c("random.effect", "tensor.smooth")
    else "random.effect"
    object
}


PredictMat <- function (object, data, n = nrow(data)) 
{
    pm <- Predict.matrix3(object, data)
    qrc <- attr(object, "qrc")
    if (inherits(qrc, "sweepDrop")) {
        if (is.null(object$deriv) || object$deriv == 0) 
            pm$X <- pm$X[, -qrc[1], drop = FALSE] - matrix(qrc[-1], 
                nrow(pm$X), ncol(pm$X) - 1, byrow = TRUE)
        else pm$X <- pm$X[, -qrc[1], drop = FALSE]
    }
    if (!is.null(pm$ind) && length(pm$ind) != n) {
        if (is.null(attr(pm$X, "by.done")) && object$by != "NA") {
            by <- get.var(object$by, data)
            if (is.null(by)) 
                stop("Can't find by variable")
        }
        else by <- rep(1, length(pm$ind))
        q <- length(pm$ind)/n
        ind <- 0:(q - 1) * n
        offs <- attr(pm$X, "offset")
        if (!is.null(offs)) 
            offX <- rep(0, n)
        else offX <- NULL
        X <- matrix(0, n, ncol(pm$X))
        for (i in 1:n) {
            ind <- ind + 1
            X[i, ] <- colSums(by[ind] * pm$X[pm$ind[ind], , drop = FALSE])
            if (!is.null(offs)) {
                offX[i] <- sum(offs[pm$ind[ind]] * by[ind])
            }
        }
        offset <- offX
    }
    else {
        offset <- attr(pm$X, "offset")
        if (!is.null(pm$ind)) {
            X <- pm$X[pm$ind, , drop = FALSE]
            if (!is.null(offset)) 
                offset <- offset[pm$ind]
        }
        else X <- pm$X
        if (is.null(attr(pm$X, "by.done"))) {
            if (object$by != "NA") {
                by <- get.var(object$by, data)
                if (is.null(by)) 
                  stop("Can't find by variable")
                if (is.factor(by)) {
                  by.dum <- as.numeric(object$by.level == by)
                  X <- by.dum * X
                  if (!is.null(offset)) 
                    offset <- by.dum * offset
                }
                else {
                  if (length(by) != nrow(X)) 
                    stop("`by' variable must be same dimension as smooth arguments")
                  X <- as.numeric(by) * X
                  if (!is.null(offset)) 
                    offset <- as.numeric(by) * offset
                }
            }
        }
        rm(pm)
        attr(X, "by.done") <- NULL
        if (n != nrow(X)) {
            q <- nrow(X)/n
            ind <- 1:n
            Xs <- X[ind, ]
            if (!is.null(offset)) {
                get.off <- TRUE
                offs <- offset[ind]
            }
            else {
                get.off <- FALSE
                offs <- NULL
            }
            for (i in 2:q) {
                ind <- ind + n
                Xs <- Xs + X[ind, , drop = FALSE]
                if (get.off) 
                  offs <- offs + offset[ind]
            }
            offset <- offs
            X <- Xs
        }
    }
    if (!is.null(qrc)) {
        j <- attr(object, "nCons")
        if (j > 0) {
            k <- ncol(X)
            if (inherits(qrc, "qr")) {
                indi <- attr(object, "indi")
                if (is.null(indi)) {
                  if (sum(is.na(X))) {
                    ind <- !is.na(rowSums(X))
                    X1 <- t(qr.qty(qrc, t(X[ind, , drop = FALSE]))[(j + 
                      1):k, , drop = FALSE])
                    X <- matrix(NA, nrow(X), ncol(X1))
                    X[ind, ] <- X1
                  }
                  else {
                    X <- t(qr.qty(qrc, t(X))[(j + 1):k, , drop = FALSE])
                  }
                }
                else {
                  nx <- length(indi)
                  nc <- j
                  nz <- nx - nc
                  if (sum(is.na(X))) {
                    ind <- !is.na(rowSums(X))
                    X[ind, indi[1:nz]] <- t(qr.qty(qrc, t(X[ind, 
                      indi, drop = FALSE]))[(nc + 1):nx, ])
                    X <- X[, -indi[(nz + 1):nx]]
                    X[!ind, ] <- NA
                  }
                  else {
                    X[, indi[1:nz]] <- t(qr.qty(qrc, t(X[, indi, 
                      drop = FALSE]))[(nc + 1):nx, , drop = FALSE])
                    X <- X[, -indi[(nz + 1):nx]]
                  }
                }
            }
            else if (inherits(qrc, "sweepDrop")) {
            }
            else if (qrc > 0) {
                X <- X[, -qrc]
            }
            else if (qrc < 0) {
                X <- t(diff(t(X)))
            }
        }
    }
    if (!is.null(object$diagRP)) 
        X <- X %*% object$diagRP
    del.index <- attr(object, "del.index")
    if (!is.null(del.index)) 
        X <- X[, -del.index, drop = FALSE]
    attr(X, "offset") <- offset
    X
}


ldTweedie <- function (y, mu = y, p = 1.5, phi = 1, rho = NA, theta = NA, 
    a = 1.001, b = 1.999) 
{
    if (!is.na(rho) && !is.na(theta)) {
        if (length(rho) > 1 || length(theta) > 1) 
            stop("only scalar `rho' and `theta' allowed.")
        if (a >= b || a <= 1 || b >= 2) 
            stop("1<a<b<2 (strict) required")
        work.param <- TRUE
        th <- theta
        phi <- exp(rho)
        p <- if (th > 0) 
            (b + a * exp(-th))/(1 + exp(-th))
        else (b * exp(th) + a)/(exp(th) + 1)
        dpth1 <- if (th > 0) 
            exp(-th) * (b - a)/(1 + exp(-th))^2
        else exp(th) * (b - a)/(exp(th) + 1)^2
        dpth2 <- if (th > 0) 
            ((a - b) * exp(-th) + (b - a) * exp(-2 * th))/(exp(-th) + 
                1)^3
        else ((a - b) * exp(2 * th) + (b - a) * exp(th))/(exp(th) + 
            1)^3
    }
    else {
        work.param <- FALSE
        if (length(p) > 1 || length(phi) > 1) 
            stop("only scalar `p' and `phi' allowed.")
        rho <- log(phi)
        if (p > 1 && p < 2) {
            if (p <= a) 
                a <- (1 + p)/2
            if (p >= b) 
                b <- (2 + p)/2
            pabp <- (p - a)/(b - p)
            theta <- log((p - a)/(b - p))
            dthp1 <- (1 + pabp)/(p - a)
            dthp2 <- (pabp + 1)/((p - a) * (b - p)) - (pabp + 
                1)/(p - a)^2
        }
    }
    if (p < 1 || p > 2) 
        stop("p must be in [1,2]")
    ld <- cbind(y, y, y)
    ld <- cbind(ld, ld * NA)
    if (p == 2) {
        if (sum(y <= 0)) 
            stop("y must be strictly positive for a Gamma density")
        ld[, 1] <- dgamma(y, shape = 1/phi, rate = 1/(phi * mu), 
            log = TRUE)
        ld[, 2] <- (digamma(1/phi) + log(phi) - 1 + y/mu - log(y/mu))/(phi * 
            phi)
        ld[, 3] <- -2 * ld[, 2]/phi + (1 - trigamma(1/phi)/phi)/(phi^3)
        return(ld)
    }
    if (length(mu) == 1) 
        mu <- rep(mu, length(y))
    if (p == 1) {
        if (sum(!is.integer(y/phi))) 
            stop("y must be an integer multiple of phi for Tweedie(p=1)")
        ind <- (y != 0) | (mu != 0)
        bkt <- y * 0
        bkt[ind] <- (y[ind] * log(mu[ind]/phi) - mu[ind])
        dig <- digamma(y/phi + 1)
        trig <- trigamma(y/phi + 1)
        ld[, 1] <- bkt/phi - lgamma(y/phi + 1)
        ld[, 2] <- (-bkt - y + dig * y)/(phi * phi)
        ld[, 3] <- (2 * bkt + 3 * y - 2 * dig * y - trig * y * 
            y/phi)/(phi^3)
        return(ld)
    }
    ind <- y == 0
    ld[ind, ] <- 0
    ind <- ind & mu > 0
    ld[ind, 1] <- -mu[ind]^(2 - p)/(phi * (2 - p))
    ld[ind, 2] <- -ld[ind, 1]/phi
    ld[ind, 3] <- -2 * ld[ind, 2]/phi
    ld[ind, 4] <- -ld[ind, 1] * (log(mu[ind]) - 1/(2 - p))
    ld[ind, 5] <- 2 * ld[ind, 4]/(2 - p) + ld[ind, 1] * log(mu[ind])^2
    ld[ind, 6] <- -ld[ind, 4]/phi
    if (sum(!ind) == 0) 
        return(ld)
    ind <- y == 0
    y <- y[!ind]
    mu <- mu[!ind]
    w <- w1 <- w2 <- y * 0
    oo <- .C(C_tweedious, w = as.double(w), w1 = as.double(w1), 
        w2 = as.double(w2), w1p = as.double(y * 0), w2p = as.double(y * 
            0), w2pp = as.double(y * 0), y = as.double(y), eps = as.double(.Machine$double.eps^2), 
        n = as.integer(length(y)), th = as.double(theta), rho = as.double(rho), 
        a = as.double(a), b = as.double(b))
    if (!work.param) {
        oo$w2 <- oo$w2/phi^2 - oo$w1/phi^2
        oo$w1 <- oo$w1/phi
        oo$w2p <- oo$w2p * dthp1^2 + dthp2 * oo$w1p
        oo$w1p <- oo$w1p * dthp1
        oo$w2pp <- oo$w2pp * dthp1/phi
    }
    log.mu <- log(mu)
    mu1p <- theta <- mu^(1 - p)
    k.theta <- mu * theta/(2 - p)
    theta <- theta/(1 - p)
    l.base <- mu1p * (y/(1 - p) - mu/(2 - p))/phi
    ld[!ind, 1] <- l.base - log(y)
    ld[!ind, 2] <- -l.base/phi
    ld[!ind, 3] <- 2 * l.base/(phi * phi)
    x <- theta * y * (1/(1 - p) - log.mu)/phi + k.theta * (log.mu - 
        1/(2 - p))/phi
    ld[!ind, 4] <- x
    ld[!ind, 5] <- theta * y * (log.mu^2 - 2 * log.mu/(1 - p) + 
        2/(1 - p)^2)/phi - k.theta * (log.mu^2 - 2 * log.mu/(2 - 
        p) + 2/(2 - p)^2)/phi
    ld[!ind, 6] <- -x/phi
    if (work.param) {
        ld[, 3] <- ld[, 3] * phi^2 + ld[, 2] * phi
        ld[, 2] <- ld[, 2] * phi
        ld[, 5] <- ld[, 5] * dpth1^2 + ld[, 4] * dpth2
        ld[, 4] <- ld[, 4] * dpth1
        ld[, 6] <- ld[, 6] * dpth1 * phi
    }
    if (TRUE) {
        ld[!ind, 1] <- ld[!ind, 1] + oo$w
        ld[!ind, 2] <- ld[!ind, 2] + oo$w1
        ld[!ind, 3] <- ld[!ind, 3] + oo$w2
        ld[!ind, 4] <- ld[!ind, 4] + oo$w1p
        ld[!ind, 5] <- ld[!ind, 5] + oo$w2p
        ld[!ind, 6] <- ld[!ind, 6] + oo$w2pp
    }
    if (FALSE) {
        ld[!ind, 1] <- oo$w
        ld[!ind, 2] <- oo$w1
        ld[!ind, 3] <- oo$w2
        ld[!ind, 4] <- oo$w1p
        ld[!ind, 5] <- oo$w2p
        ld[!ind, 6] <- oo$w2pp
    }
    ld
}


fix.family.ls <- function (fam) 
{
    if (!inherits(fam, "family")) 
        stop("fam not a family object")
    if (!is.null(fam$ls)) 
        return(fam)
    family <- fam$family
    if (family == "gaussian") {
        fam$ls <- function(y, w, n, scale) {
            nobs <- sum(w > 0)
            c(-nobs * log(2 * pi * scale)/2 + sum(log(w[w > 0]))/2, 
                -nobs/(2 * scale), nobs/(2 * scale * scale))
        }
        return(fam)
    }
    if (family == "poisson") {
        fam$ls <- function(y, w, n, scale) {
            res <- rep(0, 3)
            res[1] <- sum(dpois(y, y, log = TRUE) * w)
            res
        }
        return(fam)
    }
    if (family == "binomial") {
        fam$ls <- function(y, w, n, scale) {
            c(-binomial()$aic(y, n, y, w, 0)/2, 0, 0)
        }
        return(fam)
    }
    if (family == "Gamma") {
        fam$ls <- function(y, w, n, scale) {
            res <- rep(0, 3)
            y <- y[w > 0]
            w <- w[w > 0]
            scale <- scale/w
            k <- -lgamma(1/scale) - log(scale)/scale - 1/scale
            res[1] <- sum(k - log(y))
            k <- (digamma(1/scale) + log(scale))/(scale * scale)
            res[2] <- sum(k/w)
            k <- (-trigamma(1/scale)/(scale) + (1 - 2 * log(scale) - 
                2 * digamma(1/scale)))/(scale^3)
            res[3] <- sum(k/w^2)
            res
        }
        return(fam)
    }
    if (family == "quasi" || family == "quasipoisson" || family == 
        "quasibinomial") {
        fam$ls <- function(y, w, n, scale) {
            nobs <- sum(w > 0)
            c(-nobs * log(scale)/2 + sum(log(w[w > 0]))/2, -nobs/(2 * 
                scale), nobs/(2 * scale * scale))
        }
        return(fam)
    }
    if (family == "inverse.gaussian") {
        fam$ls <- function(y, w, n, scale) {
            nobs <- sum(w > 0)
            c(-sum(log(2 * pi * scale * y^3))/2 + sum(log(w[w > 
                0]))/2, -nobs/(2 * scale), nobs/(2 * scale * 
                scale))
        }
        return(fam)
    }
    stop("family not recognised")
}


Predict.matrix.sos.smooth <- function (object, data) 
{
    nk <- length(object$knt)/2
    la <- data[[object$term[1]]]
    lo <- data[[object$term[2]]]
    lak <- object$knt[1:nk]
    lok <- object$knt[-(1:nk)]
    n <- length(la)
    if (n > nk) {
        n.chunk <- n%/%nk
        for (i in 1:n.chunk) {
            ind <- 1:nk + (i - 1) * nk
            Xc <- makeR(la = la[ind], lo = lo[ind], lak = lak, 
                lok = lok, m = object$p.order)
            Xc <- cbind(Xc %*% object$UZ, attr(Xc, "T"))
            if (i == 1) 
                X <- Xc
            else {
                X <- rbind(X, Xc)
                rm(Xc)
            }
        }
        if (n > ind[nk]) {
            ind <- (ind[nk] + 1):n
            Xc <- makeR(la = la[ind], lo = lo[ind], lak = lak, 
                lok = lok, m = object$p.order)
            Xc <- cbind(Xc %*% object$UZ, attr(Xc, "T"))
            X <- rbind(X, Xc)
            rm(Xc)
        }
    }
    else {
        X <- makeR(la = la, lo = lo, lak = lak, lok = lok, m = object$p.order)
        X <- cbind(X %*% object$UZ, attr(X, "T"))
    }
    if (!is.null(object$xc.scale)) 
        X <- t(t(X) * object$xc.scale)
    X
}


smooth.construct.ad.smooth.spec <- function (object, data, knots) 
{
    bs <- object$xt$bs
    if (length(bs) > 1) 
        bs <- bs[1]
    if (is.null(bs)) {
        bs <- "ps"
    }
    else {
        if (!bs %in% c("cc", "cr", "ps", "cp")) 
            bs[1] <- "ps"
    }
    if (bs == "cc" || bs == "cp") 
        bsp <- "cp"
    else bsp <- "ps"
    if (object$dim > 2) 
        stop("the adaptive smooth class is limited to 1 or 2 covariates.")
    else if (object$dim == 1) {
        if (object$bs.dim < 0) 
            object$bs.dim <- 40
        if (is.na(object$p.order[1])) 
            object$p.order[1] <- 5
        pobject <- object
        pobject$p.order <- c(2, 2)
        class(pobject) <- paste(bs[1], ".smooth.spec", sep = "")
        if (is.null(knots) && bs[1] %in% c("cr", "cc")) {
            x <- data[[object$term]]
            knots <- list(seq(min(x), max(x), length = object$bs.dim))
            names(knots) <- object$term
        }
        pspl <- smooth.construct(pobject, data, knots)
        nk <- ncol(pspl$X)
        k <- object$p.order[1]
        if (k >= nk - 2) 
            stop("penalty basis too large for smoothing basis")
        if (k <= 0) {
            pspl$fixed <- TRUE
            pspl$S <- NULL
        }
        else if (k >= 2) {
            x <- 1:(nk - 2)/nk
            m = 2
            if (k == 2) 
                V <- cbind(rep(1, nk - 2), x)
            else if (k == 3) {
                m <- 1
                ps2 <- smooth.construct(s(x, k = k, bs = bsp, 
                  m = m, fx = TRUE), data = data.frame(x = x), 
                  knots = NULL)
                V <- ps2$X
            }
            else {
                ps2 <- smooth.construct(s(x, k = k, bs = bsp, 
                  m = m, fx = TRUE), data = data.frame(x = x), 
                  knots = NULL)
                V <- ps2$X
            }
            Db <- diff(diff(diag(nk)))
            S <- list()
            for (i in 1:k) {
                S[[i]] <- t(Db) %*% (as.numeric(V[, i]) * Db)
                ind <- rowSums(abs(S[[i]])) > 0
                ev <- eigen(S[[i]][ind, ind], symmetric = TRUE, 
                  only.values = TRUE)$values
                pspl$rank[i] <- sum(ev > max(ev) * .Machine$double.eps^0.9)
            }
            pspl$S <- S
        }
    }
    else if (object$dim == 2) {
        object$bs.dim[object$bs.dim < 0] <- 15
        k <- object$bs.dim
        if (length(k) == 1) 
            k <- c(k[1], k[1])
        tec <- paste("te(", object$term[1], ",", object$term[2], 
            ",bs=bs,k=k,m=2)", sep = "")
        pobject <- eval(parse(text = tec))
        pobject$np <- FALSE
        if (is.null(knots) && bs[1] %in% c("cr", "cc")) {
            for (i in 1:2) {
                x <- data[[object$term[i]]]
                knots <- list(seq(min(x), max(x), length = k[i]))
                names(knots)[i] <- object$term[i]
            }
        }
        pspl <- smooth.construct(pobject, data, knots)
        kp <- object$p.order
        if (length(kp) != 2) 
            kp <- c(kp[1], kp[1])
        kp[is.na(kp)] <- 3
        kp.tot <- prod(kp)
        k.tot <- (k[1] - 2) * (k[2] - 2)
        if (kp.tot > k.tot) 
            stop("penalty basis too large for smoothing basis")
        if (kp.tot <= 0) {
            pspl$fixed <- TRUE
            pspl$S <- NULL
        }
        else {
            Db <- D2(ni = k[1], nj = k[2])
            pspl$S <- list()
            if (kp.tot == 1) {
                pspl$S[[1]] <- t(Db[[1]]) %*% Db[[1]] + t(Db[[2]]) %*% 
                  Db[[2]] + t(Db[[3]]) %*% Db[[3]]
                pspl$rank <- ncol(pspl$S[[1]]) - 3
            }
            else {
                if (kp.tot == 3) {
                  V <- cbind(rep(1, k.tot), Db[[4]], Db[[5]])
                }
                else {
                  ok <- TRUE
                  if (sum(kp < 2)) 
                    ok <- FALSE
                  if (!ok) 
                    stop("penalty basis too small")
                  m <- min(min(kp) - 2, 1)
                  m <- c(m, m)
                  j <- 1
                  ps2 <- smooth.construct(te(i, j, bs = bsp, 
                    k = kp, fx = TRUE, m = m, np = FALSE), data = data.frame(i = Db$rmt, 
                    j = Db$cmt), knots = NULL)
                  Vrr <- Predict.matrix(ps2, data.frame(i = Db$rr.ri, 
                    j = Db$rr.ci))
                  Vcc <- Predict.matrix(ps2, data.frame(i = Db$cc.ri, 
                    j = Db$cc.ci))
                  Vcr <- Predict.matrix(ps2, data.frame(i = Db$cr.ri, 
                    j = Db$cr.ci))
                }
                S <- list()
                for (i in 1:kp.tot) {
                  S[[i]] <- t(Db$Drr) %*% (as.numeric(Vrr[, i]) * 
                    Db$Drr) + t(Db$Dcc) %*% (as.numeric(Vcc[, 
                    i]) * Db$Dcc) + t(Db$Dcr) %*% (as.numeric(Vcr[, 
                    i]) * Db$Dcr)
                  ev <- eigen(S[[i]], symmetric = TRUE, only.values = TRUE)$values
                  pspl$rank[i] <- sum(ev > max(ev) * .Machine$double.eps * 
                    10)
                }
                pspl$S <- S
                pspl$pen.smooth <- ps2
            }
        }
    }
    pspl$te.ok <- 0
    pspl
}




## Package Data

columb <- mgcv::columb		## Reduced version of Columbus OH crime data

columb.polys <- mgcv::columb.polys		## Reduced version of Columbus OH crime data



## Package Info

.skeleton_package_title = "Mixed GAM Computation Vehicle with GCV/AIC/REML SmoothnessEstimation"

.skeleton_package_version = "1.8-12"

.skeleton_package_depends = "nlme"

.skeleton_package_imports = "methods,stats,graphics,Matrix"


## Internal

.skeleton_version = 5


## EOF