##
## Exported symobls in package `KernSmooth`
##

## Exported package methods

dpih <- function (x, scalest = "minim", level = 2L, gridsize = 401L, 
    range.x = range(x), truncate = TRUE) 
{
    if (level > 5L) 
        stop("Level should be between 0 and 5")
    n <- length(x)
    M <- gridsize
    a <- range.x[1L]
    b <- range.x[2L]
    gpoints <- seq(a, b, length = M)
    gcounts <- linbin(x, gpoints, truncate)
    scalest <- match.arg(scalest, c("minim", "stdev", "iqr"))
    scalest <- switch(scalest, stdev = sqrt(var(x)), iqr = (quantile(x, 
        3/4) - quantile(x, 1/4))/1.349, minim = min((quantile(x, 
        3/4) - quantile(x, 1/4))/1.349, sqrt(var(x))))
    if (scalest == 0) 
        stop("scale estimate is zero for input data")
    sx <- (x - mean(x))/scalest
    sa <- (a - mean(x))/scalest
    sb <- (b - mean(x))/scalest
    gpoints <- seq(sa, sb, length = M)
    gcounts <- linbin(sx, gpoints, truncate)
    hpi <- if (level == 0L) 
        (24 * sqrt(pi)/n)^(1/3)
    else if (level == 1L) {
        alpha <- (2/(3 * n))^(1/5) * sqrt(2)
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        (6/(-psi2hat * n))^(1/3)
    }
    else if (level == 2L) {
        alpha <- ((2/(5 * n))^(1/7)) * sqrt(2)
        psi4hat <- bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (sqrt(2/pi)/(psi4hat * n))^(1/5)
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        (6/(-psi2hat * n))^(1/3)
    }
    else if (level == 3L) {
        alpha <- ((2/(7 * n))^(1/9)) * sqrt(2)
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (-3 * sqrt(2/pi)/(psi6hat * n))^(1/7)
        psi4hat <- bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (sqrt(2/pi)/(psi4hat * n))^(1/5)
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        (6/(-psi2hat * n))^(1/3)
    }
    else if (level == 4L) {
        alpha <- ((2/(9 * n))^(1/11)) * sqrt(2)
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (15 * sqrt(2/pi)/(psi8hat * n))^(1/9)
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (-3 * sqrt(2/pi)/(psi6hat * n))^(1/7)
        psi4hat <- bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (sqrt(2/pi)/(psi4hat * n))^(1/5)
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        (6/(-psi2hat * n))^(1/3)
    }
    else if (level == 5L) {
        alpha <- ((2/(11 * n))^(1/13)) * sqrt(2)
        psi10hat <- bkfe(gcounts, 10L, alpha, range.x = c(sa, 
            sb), binned = TRUE)
        alpha <- (-105 * sqrt(2/pi)/(psi10hat * n))^(1/11)
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (15 * sqrt(2/pi)/(psi8hat * n))^(1/9)
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (-3 * sqrt(2/pi)/(psi6hat * n))^(1/7)
        psi4hat <- bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (sqrt(2/pi)/(psi4hat * n))^(1/5)
        psi2hat <- bkfe(gcounts, 2L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        (6/(-psi2hat * n))^(1/3)
    }
    scalest * hpi
}


dpill <- function (x, y, blockmax = 5, divisor = 20, trim = 0.01, proptrun = 0.05, 
    gridsize = 401L, range.x = range(x), truncate = TRUE) 
{
    xy <- cbind(x, y)
    xy <- xy[sort.list(xy[, 1L]), ]
    x <- xy[, 1L]
    y <- xy[, 2L]
    indlow <- floor(trim * length(x)) + 1
    indupp <- length(x) - floor(trim * length(x))
    x <- x[indlow:indupp]
    y <- y[indlow:indupp]
    n <- length(x)
    M <- gridsize
    a <- range.x[1L]
    b <- range.x[2L]
    gpoints <- seq(a, b, length = M)
    out <- rlbin(x, y, gpoints, truncate)
    xcounts <- out$xcounts
    ycounts <- out$ycounts
    Nmax <- max(min(floor(n/divisor), blockmax), 1)
    Nval <- cpblock(x, y, Nmax, 4)
    out <- blkest(x, y, Nval, 4)
    sigsqQ <- out$sigsqe
    th24Q <- out$th24e
    gamseh <- (sigsqQ * (b - a)/(abs(th24Q) * n))
    if (th24Q < 0) 
        gamseh <- (3 * gamseh/(8 * sqrt(pi)))^(1/7)
    if (th24Q > 0) 
        gamseh <- (15 * gamseh/(16 * sqrt(pi)))^(1/7)
    mddest <- locpoly(xcounts, ycounts, drv = 2L, bandwidth = gamseh, 
        range.x = range.x, binned = TRUE)$y
    llow <- floor(proptrun * M) + 1
    lupp <- M - floor(proptrun * M)
    th22kn <- sum((mddest[llow:lupp]^2) * xcounts[llow:lupp])/n
    C3K <- (1/2) + 2 * sqrt(2) - (4/3) * sqrt(3)
    C3K <- (4 * C3K/(sqrt(2 * pi)))^(1/9)
    lamseh <- C3K * (((sigsqQ^2) * (b - a)/((th22kn * n)^2))^(1/9))
    mest <- locpoly(xcounts, ycounts, bandwidth = lamseh, range.x = range.x, 
        binned = TRUE)$y
    Sdg <- sdiag(xcounts, bandwidth = lamseh, range.x = range.x, 
        binned = TRUE)$y
    SSTdg <- sstdiag(xcounts, bandwidth = lamseh, range.x = range.x, 
        binned = TRUE)$y
    sigsqn <- sum(y^2) - 2 * sum(mest * ycounts) + sum((mest^2) * 
        xcounts)
    sigsqd <- n - 2 * sum(Sdg * xcounts) + sum(SSTdg * xcounts)
    sigsqkn <- sigsqn/sigsqd
    (sigsqkn * (b - a)/(2 * sqrt(pi) * th22kn * n))^(1/5)
}


dpik <- function (x, scalest = "minim", level = 2L, kernel = "normal", 
    canonical = FALSE, gridsize = 401L, range.x = range(x), truncate = TRUE) 
{
    if (level > 5L) 
        stop("Level should be between 0 and 5")
    kernel <- match.arg(kernel, c("normal", "box", "epanech", 
        "biweight", "triweight"))
    del0 <- if (canonical) 
        1
    else switch(kernel, normal = 1/((4 * pi)^(1/10)), box = (9/2)^(1/5), 
        epanech = 15^(1/5), biweight = 35^(1/5), triweight = (9450/143)^(1/5))
    n <- length(x)
    M <- gridsize
    a <- range.x[1L]
    b <- range.x[2L]
    gpoints <- seq(a, b, length = M)
    gcounts <- linbin(x, gpoints, truncate)
    scalest <- match.arg(scalest, c("minim", "stdev", "iqr"))
    scalest <- switch(scalest, stdev = sqrt(var(x)), iqr = (quantile(x, 
        3/4) - quantile(x, 1/4))/1.349, minim = min((quantile(x, 
        3/4) - quantile(x, 1/4))/1.349, sqrt(var(x))))
    if (scalest == 0) 
        stop("scale estimate is zero for input data")
    sx <- (x - mean(x))/scalest
    sa <- (a - mean(x))/scalest
    sb <- (b - mean(x))/scalest
    gpoints <- seq(sa, sb, length = M)
    gcounts <- linbin(sx, gpoints, truncate)
    psi4hat <- if (level == 0L) 
        3/(8 * sqrt(pi))
    else if (level == 1L) {
        alpha <- (2 * (sqrt(2))^7/(5 * n))^(1/7)
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    }
    else if (level == 2L) {
        alpha <- (2 * (sqrt(2))^9/(7 * n))^(1/9)
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (-3 * sqrt(2/pi)/(psi6hat * n))^(1/7)
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    }
    else if (level == 3L) {
        alpha <- (2 * (sqrt(2))^11/(9 * n))^(1/11)
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (15 * sqrt(2/pi)/(psi8hat * n))^(1/9)
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (-3 * sqrt(2/pi)/(psi6hat * n))^(1/7)
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    }
    else if (level == 4L) {
        alpha <- (2 * (sqrt(2))^13/(11 * n))^(1/13)
        psi10hat <- bkfe(gcounts, 10L, alpha, range.x = c(sa, 
            sb), binned = TRUE)
        alpha <- (-105 * sqrt(2/pi)/(psi10hat * n))^(1/11)
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (15 * sqrt(2/pi)/(psi8hat * n))^(1/9)
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (-3 * sqrt(2/pi)/(psi6hat * n))^(1/7)
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    }
    else if (level == 5L) {
        alpha <- (2 * (sqrt(2))^15/(13 * n))^(1/15)
        psi12hat <- bkfe(gcounts, 12L, alpha, range.x = c(sa, 
            sb), binned = TRUE)
        alpha <- (945 * sqrt(2/pi)/(psi12hat * n))^(1/13)
        psi10hat <- bkfe(gcounts, 10L, alpha, range.x = c(sa, 
            sb), binned = TRUE)
        alpha <- (-105 * sqrt(2/pi)/(psi10hat * n))^(1/11)
        psi8hat <- bkfe(gcounts, 8L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (15 * sqrt(2/pi)/(psi8hat * n))^(1/9)
        psi6hat <- bkfe(gcounts, 6L, alpha, range.x = c(sa, sb), 
            binned = TRUE)
        alpha <- (-3 * sqrt(2/pi)/(psi6hat * n))^(1/7)
        bkfe(gcounts, 4L, alpha, range.x = c(sa, sb), binned = TRUE)
    }
    scalest * del0 * (1/(psi4hat * n))^(1/5)
}


bkde2D <- function (x, bandwidth, gridsize = c(51L, 51L), range.x, truncate = TRUE) 
{
    if (!missing(bandwidth) && min(bandwidth) <= 0) 
        stop("'bandwidth' must be strictly positive")
    n <- nrow(x)
    M <- gridsize
    h <- bandwidth
    tau <- 3.4
    if (length(h) == 1L) 
        h <- c(h, h)
    if (missing(range.x)) {
        range.x <- list(0, 0)
        for (id in (1L:2L)) range.x[[id]] <- c(min(x[, id]) - 
            1.5 * h[id], max(x[, id]) + 1.5 * h[id])
    }
    a <- c(range.x[[1L]][1L], range.x[[2L]][1L])
    b <- c(range.x[[1L]][2L], range.x[[2L]][2L])
    gpoints1 <- seq(a[1L], b[1L], length = M[1L])
    gpoints2 <- seq(a[2L], b[2L], length = M[2L])
    gcounts <- linbin2D(x, gpoints1, gpoints2)
    L <- numeric(2L)
    kapid <- list(0, 0)
    for (id in 1L:2L) {
        L[id] <- min(floor(tau * h[id] * (M[id] - 1)/(b[id] - 
            a[id])), M[id] - 1L)
        lvecid <- 0:L[id]
        facid <- (b[id] - a[id])/(h[id] * (M[id] - 1L))
        z <- matrix(dnorm(lvecid * facid)/h[id])
        tot <- sum(c(z, rev(z[-1L]))) * facid * h[id]
        kapid[[id]] <- z/tot
    }
    kapp <- kapid[[1L]] %*% (t(kapid[[2L]]))/n
    if (min(L) == 0) 
        warning("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")
    P <- 2^(ceiling(log(M + L)/log(2)))
    L1 <- L[1L]
    L2 <- L[2L]
    M1 <- M[1L]
    M2 <- M[2L]
    P1 <- P[1L]
    P2 <- P[2L]
    rp <- matrix(0, P1, P2)
    rp[1L:(L1 + 1), 1L:(L2 + 1)] <- kapp
    if (L1) 
        rp[(P1 - L1 + 1):P1, 1L:(L2 + 1)] <- kapp[(L1 + 1):2, 
            1L:(L2 + 1)]
    if (L2) 
        rp[, (P2 - L2 + 1):P2] <- rp[, (L2 + 1):2]
    sp <- matrix(0, P1, P2)
    sp[1L:M1, 1L:M2] <- gcounts
    rp <- fft(rp)
    sp <- fft(sp)
    rp <- Re(fft(rp * sp, inverse = TRUE)/(P1 * P2))[1L:M1, 1L:M2]
    rp <- rp * matrix(as.numeric(rp > 0), nrow(rp), ncol(rp))
    list(x1 = gpoints1, x2 = gpoints2, fhat = rp)
}


locpoly <- function (x, y, drv = 0L, degree, kernel = "normal", bandwidth, 
    gridsize = 401L, bwdisc = 25, range.x, binned = FALSE, truncate = TRUE) 
{
    if (!missing(bandwidth) && bandwidth <= 0) 
        stop("'bandwidth' must be strictly positive")
    drv <- as.integer(drv)
    if (missing(degree)) 
        degree <- drv + 1L
    else degree <- as.integer(degree)
    if (missing(range.x) && !binned) 
        if (missing(y)) {
            extra <- 0.05 * (max(x) - min(x))
            range.x <- c(min(x) - extra, max(x) + extra)
        }
        else range.x <- c(min(x), max(x))
    M <- gridsize
    Q <- as.integer(bwdisc)
    a <- range.x[1L]
    b <- range.x[2L]
    pp <- degree + 1L
    ppp <- 2L * degree + 1L
    tau <- 4
    if (missing(y)) {
        y <- NULL
        n <- length(x)
        gpoints <- seq(a, b, length = M)
        xcounts <- linbin(x, gpoints, truncate)
        ycounts <- (M - 1) * xcounts/(n * (b - a))
        xcounts <- rep(1, M)
    }
    else {
        if (!binned) {
            gpoints <- seq(a, b, length = M)
            out <- rlbin(x, y, gpoints, truncate)
            xcounts <- out$xcounts
            ycounts <- out$ycounts
        }
        else {
            xcounts <- x
            ycounts <- y
            M <- length(xcounts)
            gpoints <- seq(a, b, length = M)
        }
    }
    delta <- (b - a)/(M - 1L)
    if (length(bandwidth) == M) {
        hlow <- sort(bandwidth)[1L]
        hupp <- sort(bandwidth)[M]
        hdisc <- exp(seq(log(hlow), log(hupp), length = Q))
        Lvec <- floor(tau * hdisc/delta)
        indic <- if (Q > 1L) {
            lhdisc <- log(hdisc)
            gap <- (lhdisc[Q] - lhdisc[1L])/(Q - 1)
            if (gap == 0) 
                rep(1, M)
            else round(((log(bandwidth) - log(sort(bandwidth)[1L]))/gap) + 
                1)
        }
        else rep(1, M)
    }
    else if (length(bandwidth) == 1L) {
        indic <- rep(1, M)
        Q <- 1L
        Lvec <- rep(floor(tau * bandwidth/delta), Q)
        hdisc <- rep(bandwidth, Q)
    }
    else stop("'bandwidth' must be a scalar or an array of length 'gridsize'")
    if (min(Lvec) == 0) 
        stop("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")
    dimfkap <- 2L * sum(Lvec) + Q
    fkap <- rep(0, dimfkap)
    curvest <- rep(0, M)
    midpts <- rep(0, Q)
    ss <- matrix(0, M, ppp)
    tt <- matrix(0, M, pp)
    Smat <- matrix(0, pp, pp)
    Tvec <- rep(0, pp)
    ipvt <- rep(0, pp)
    out <- .Fortran(F_locpol, as.double(xcounts), as.double(ycounts), 
        as.integer(drv), as.double(delta), as.double(hdisc), 
        as.integer(Lvec), as.integer(indic), as.integer(midpts), 
        as.integer(M), as.integer(Q), as.double(fkap), as.integer(pp), 
        as.integer(ppp), as.double(ss), as.double(tt), as.double(Smat), 
        as.double(Tvec), as.integer(ipvt), as.double(curvest))
    curvest <- gamma(drv + 1) * out[[19L]]
    list(x = gpoints, y = curvest)
}


bkde <- function (x, kernel = "normal", canonical = FALSE, bandwidth, 
    gridsize = 401L, range.x, truncate = TRUE) 
{
    if (!missing(bandwidth) && bandwidth <= 0) 
        stop("'bandwidth' must be strictly positive")
    kernel <- match.arg(kernel, c("normal", "box", "epanech", 
        "biweight", "triweight"))
    n <- length(x)
    M <- gridsize
    del0 <- switch(kernel, normal = (1/(4 * pi))^(1/10), box = (9/2)^(1/5), 
        epanech = 15^(1/5), biweight = 35^(1/5), triweight = (9450/143)^(1/5))
    h <- if (missing(bandwidth)) 
        del0 * (243/(35 * n))^(1/5) * sqrt(var(x))
    else if (canonical) 
        del0 * bandwidth
    else bandwidth
    tau <- if (kernel == "normal") 
        4
    else 1
    if (missing(range.x)) 
        range.x <- c(min(x) - tau * h, max(x) + tau * h)
    a <- range.x[1L]
    b <- range.x[2L]
    gpoints <- seq(a, b, length = M)
    gcounts <- linbin(x, gpoints, truncate)
    delta <- (b - a)/(h * (M - 1L))
    L <- min(floor(tau/delta), M)
    if (L == 0) 
        warning("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")
    lvec <- 0L:L
    kappa <- if (kernel == "normal") 
        dnorm(lvec * delta)/(n * h)
    else if (kernel == "box") 
        0.5 * dbeta(0.5 * (lvec * delta + 1), 1, 1)/(n * h)
    else if (kernel == "epanech") 
        0.5 * dbeta(0.5 * (lvec * delta + 1), 2, 2)/(n * h)
    else if (kernel == "biweight") 
        0.5 * dbeta(0.5 * (lvec * delta + 1), 3, 3)/(n * h)
    else if (kernel == "triweight") 
        0.5 * dbeta(0.5 * (lvec * delta + 1), 4, 4)/(n * h)
    P <- 2^(ceiling(log(M + L + 1L)/log(2)))
    kappa <- c(kappa, rep(0, P - 2L * L - 1L), rev(kappa[-1L]))
    tot <- sum(kappa) * (b - a)/(M - 1L) * n
    gcounts <- c(gcounts, rep(0L, P - M))
    kappa <- fft(kappa/tot)
    gcounts <- fft(gcounts)
    list(x = gpoints, y = (Re(fft(kappa * gcounts, TRUE))/P)[1L:M])
}


bkfe <- function (x, drv, bandwidth, gridsize = 401L, range.x, binned = FALSE, 
    truncate = TRUE) 
{
    if (!missing(bandwidth) && bandwidth <= 0) 
        stop("'bandwidth' must be strictly positive")
    if (missing(range.x) && !binned) 
        range.x <- c(min(x), max(x))
    M <- gridsize
    a <- range.x[1L]
    b <- range.x[2L]
    h <- bandwidth
    if (!binned) {
        gpoints <- seq(a, b, length = gridsize)
        gcounts <- linbin(x, gpoints, truncate)
    }
    else {
        gcounts <- x
        M <- length(gcounts)
        gpoints <- seq(a, b, length = M)
    }
    n <- sum(gcounts)
    delta <- (b - a)/(M - 1)
    tau <- 4 + drv
    L <- min(floor(tau * h/delta), M)
    if (L == 0) 
        warning("Binning grid too coarse for current (small) bandwidth: consider increasing 'gridsize'")
    lvec <- 0L:L
    arg <- lvec * delta/h
    kappam <- dnorm(arg)/(h^(drv + 1))
    hmold0 <- 1
    hmold1 <- arg
    hmnew <- 1
    if (drv >= 2L) 
        for (i in (2L:drv)) {
            hmnew <- arg * hmold1 - (i - 1) * hmold0
            hmold0 <- hmold1
            hmold1 <- hmnew
        }
    kappam <- hmnew * kappam
    P <- 2^(ceiling(log(M + L + 1L)/log(2)))
    kappam <- c(kappam, rep(0, P - 2L * L - 1L), rev(kappam[-1L]))
    Gcounts <- c(gcounts, rep(0, P - M))
    kappam <- fft(kappam)
    Gcounts <- fft(Gcounts)
    sum(gcounts * (Re(fft(kappam * Gcounts, TRUE))/P)[1L:M])/(n^2)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Functions for Kernel Smoothing Supporting Wand & Jones (1995)"

.skeleton_package_version = "2.23-15"

.skeleton_package_depends = "stats"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF