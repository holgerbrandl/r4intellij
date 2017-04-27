##
## Exported symobls in package `labeling`
##

## Exported package methods

wilkinson <- function (dmin, dmax, m, Q = c(1, 5, 2, 2.5, 3, 4, 1.5, 7, 6, 
    8, 9), mincoverage = 0.8, mrange = max(floor(m/2), 2):ceiling(6 * 
    m)) 
{
    best <- NULL
    for (k in mrange) {
        result <- .wilkinson.nice.scale(dmin, dmax, k, Q, mincoverage, 
            mrange, m)
        if (!is.null(result) && (is.null(best) || result$score > 
            best$score)) {
            best <- result
        }
    }
    seq(best$lmin, best$lmax, by = best$lstep)
}


matplotlib <- function (dmin, dmax, m) 
{
    steps <- c(1, 2, 5, 10)
    nbins <- m
    trim <- TRUE
    vmin <- dmin
    vmax <- dmax
    params <- .matplotlib.scale.range(vmin, vmax, nbins)
    scale <- params[1]
    offset <- params[2]
    vmin <- vmin - offset
    vmax <- vmax - offset
    rawStep <- (vmax - vmin)/nbins
    scaledRawStep <- rawStep/scale
    bestMax <- vmax
    bestMin <- vmin
    scaledStep <- 1
    chosenFactor <- 1
    for (step in steps) {
        if (step >= scaledRawStep) {
            scaledStep <- step * scale
            chosenFactor <- step
            bestMin <- scaledStep * floor(vmin/scaledStep)
            bestMax <- bestMin + scaledStep * nbins
            if (bestMax >= vmax) 
                break
        }
    }
    if (trim) {
        extraBins <- floor((bestMax - vmax)/scaledStep)
        nbins <- nbins - extraBins
    }
    graphMin <- bestMin + offset
    graphMax <- graphMin + nbins * scaledStep
    seq(from = graphMin, to = graphMax, by = scaledStep)
}


gnuplot <- function (dmin, dmax, m) 
{
    ntick <- floor(m)
    power <- 10^floor(log10(dmax - dmin))
    norm_range <- (dmax - dmin)/power
    p <- (ntick - 1)/norm_range
    if (p > 40) 
        t <- 0.05
    else if (p > 20) 
        t <- 0.1
    else if (p > 10) 
        t <- 0.2
    else if (p > 4) 
        t <- 0.5
    else if (p > 2) 
        t <- 1
    else if (p > 0.5) 
        t <- 2
    else t <- ceiling(norm_range)
    d <- t * power
    graphmin <- floor(dmin/d) * d
    graphmax <- ceiling(dmax/d) * d
    seq(from = graphmin, to = graphmax, by = d)
}


sparks <- function (dmin, dmax, m) 
{
    fm <- m - 1
    ratio <- 0
    key <- 1
    kount <- 0
    r <- dmax - dmin
    b <- dmin
    while (ratio <= 0.8) {
        while (key <= 2) {
            while (r <= 1) {
                kount <- kount + 1
                r <- r * 10
            }
            while (r > 10) {
                kount <- kount - 1
                r <- r/10
            }
            b <- b * (10^kount)
            if (b < 0 && b != trunc(b)) 
                b <- b - 1
            b <- trunc(b)/(10^kount)
            r <- (dmax - b)/fm
            kount <- 0
            key <- key + 2
        }
        fstep <- trunc(r)
        if (fstep != r) 
            fstep <- fstep + 1
        if (r < 1.5) 
            fstep <- fstep - 0.5
        fstep <- fstep/(10^kount)
        ratio <- (dmax - dmin) * (fm * fstep)
        kount <- 1
        key <- 2
    }
    fmin <- b
    c <- fstep * trunc(b/fstep)
    if (c < 0 && c != b) 
        c <- c - fstep
    if ((c + fm * fstep) > dmax) 
        fmin <- c
    seq(from = fmin, to = fstep * (m - 1), by = fstep)
}


extended <- function (dmin, dmax, m, Q = c(1, 5, 2, 2.5, 4, 3), only.loose = FALSE, 
    w = c(0.25, 0.2, 0.5, 0.05)) 
{
    eps <- .Machine$double.eps * 100
    if (dmin > dmax) {
        temp <- dmin
        dmin <- dmax
        dmax <- temp
    }
    if (dmax - dmin < eps) {
        return(seq(from = dmin, to = dmax, length.out = m))
    }
    n <- length(Q)
    best <- list()
    best$score <- -2
    j <- 1
    while (j < Inf) {
        for (q in Q) {
            sm <- .simplicity.max(q, Q, j)
            if ((w[1] * sm + w[2] + w[3] + w[4]) < best$score) {
                j <- Inf
                break
            }
            k <- 2
            while (k < Inf) {
                dm <- .density.max(k, m)
                if ((w[1] * sm + w[2] + w[3] * dm + w[4]) < best$score) 
                  break
                delta <- (dmax - dmin)/(k + 1)/j/q
                z <- ceiling(log(delta, base = 10))
                while (z < Inf) {
                  step <- j * q * 10^z
                  cm <- .coverage.max(dmin, dmax, step * (k - 
                    1))
                  if ((w[1] * sm + w[2] * cm + w[3] * dm + w[4]) < 
                    best$score) 
                    break
                  min_start <- floor(dmax/(step)) * j - (k - 
                    1) * j
                  max_start <- ceiling(dmin/(step)) * j
                  if (min_start > max_start) {
                    z <- z + 1
                    next
                  }
                  for (start in min_start:max_start) {
                    lmin <- start * (step/j)
                    lmax <- lmin + step * (k - 1)
                    lstep <- step
                    s <- .simplicity(q, Q, j, lmin, lmax, lstep)
                    c <- .coverage(dmin, dmax, lmin, lmax)
                    g <- .density(k, m, dmin, dmax, lmin, lmax)
                    l <- .legibility(lmin, lmax, lstep)
                    score <- w[1] * s + w[2] * c + w[3] * g + 
                      w[4] * l
                    if (score > best$score && (!only.loose || 
                      (lmin <= dmin && lmax >= dmax))) {
                      best <- list(lmin = lmin, lmax = lmax, 
                        lstep = lstep, score = score)
                    }
                  }
                  z <- z + 1
                }
                k <- k + 1
            }
        }
        j <- j + 1
    }
    seq(from = best$lmin, to = best$lmax, by = best$lstep)
}


rpretty <- function (dmin, dmax, m = 6, n = floor(m) - 1, min.n = n%/%3, 
    shrink.sml = 0.75, high.u.bias = 1.5, u5.bias = 0.5 + 1.5 * 
        high.u.bias) 
{
    ndiv <- n
    h <- high.u.bias
    h5 <- u5.bias
    dx <- dmax - dmin
    if (dx == 0 && dmax == 0) {
        cell <- 1
        i_small <- TRUE
        U <- 1
    }
    else {
        cell <- max(abs(dmin), abs(dmax))
        U <- 1 + ifelse(h5 >= 1.5 * h + 0.5, 1/(1 + h), 1.5/(1 + 
            h5))
        i_small = dx < (cell * U * max(1, ndiv) * 1e-07 * 3)
    }
    if (i_small) {
        if (cell > 10) {
            cell <- 9 + cell/10
        }
        cell <- cell * shrink.sml
        if (min.n > 1) 
            cell <- cell/min.n
    }
    else {
        cell <- dx
        if (ndiv > 1) 
            cell <- cell/ndiv
    }
    if (cell < 20 * 1e-07) 
        cell <- 20 * 1e-07
    base <- 10^floor(log10(cell))
    unit <- base
    if ((2 * base) - cell < h * (cell - unit)) {
        unit <- 2 * base
        if ((5 * base) - cell < h5 * (cell - unit)) {
            unit <- 5 * base
            if ((10 * base) - cell < h * (cell - unit)) 
                unit <- 10 * base
        }
    }
    ns <- floor(dmin/unit + 1e-07)
    nu <- ceiling(dmax/unit - 1e-07)
    while (ns * unit > dmin + (1e-07 * unit)) ns <- ns - 1
    while (nu * unit < dmax - (1e-07 * unit)) nu <- nu + 1
    k <- floor(0.5 + nu - ns)
    if (k < min.n) {
        k <- min.n - k
        if (ns >= 0) {
            nu <- nu + k/2
            ns <- ns - k/2 + k%%2
        }
        else {
            ns <- ns - k/2
            nu <- nu + k/2 + k%%2
        }
        ndiv <- min.n
    }
    else {
        ndiv <- k
    }
    graphmin <- ns * unit
    graphmax <- nu * unit
    seq(from = graphmin, to = graphmax, by = unit)
}


heckbert <- function (dmin, dmax, m) 
{
    range <- .heckbert.nicenum((dmax - dmin), FALSE)
    lstep <- .heckbert.nicenum(range/(m - 1), TRUE)
    lmin <- floor(dmin/lstep) * lstep
    lmax <- ceiling(dmax/lstep) * lstep
    seq(lmin, lmax, by = lstep)
}


nelder <- function (dmin, dmax, m, Q = c(1, 1.2, 1.6, 2, 2.5, 3, 4, 5, 
    6, 8, 10)) 
{
    ntick <- floor(m)
    tol <- 5e-06
    bias <- 1e-04
    intervals <- m - 1
    x <- abs(dmax)
    if (x == 0) 
        x <- 1
    if (!((dmax - dmin)/x > tol)) {
    }
    step <- (dmax - dmin)/intervals
    s <- step
    while (s <= 1) s <- s * 10
    while (s > 10) s <- s/10
    x <- s - bias
    unit <- 1
    for (i in 1:length(Q)) {
        if (x < Q[i]) {
            unit <- i
            break
        }
    }
    step <- step * Q[unit]/s
    range <- step * intervals
    x <- 0.5 * (1 + (dmin + dmax - range)/step)
    j <- floor(x - bias)
    valmin <- step * j
    if (dmin > 0 && range >= dmax) 
        valmin <- 0
    valmax <- valmin + range
    if (!(dmax > 0 || range < -dmin)) {
        valmax <- 0
        valmin <- -range
    }
    seq(from = valmin, to = valmax, by = step)
}


thayer <- function (dmin, dmax, m) 
{
    r <- dmax - dmin
    b <- dmin
    kount <- 0
    kod <- 0
    while (kod < 2) {
        while (r <= 1) {
            kount <- kount + 1
            r <- r * 10
        }
        while (r > 10) {
            kount <- kount - 1
            r <- r/10
        }
        b <- b * (10^kount)
        if (b < 0) 
            b <- b - 1
        ib <- trunc(b)
        b <- ib
        b <- b/(10^kount)
        r <- dmax - b
        a <- r/(m - 1)
        kount <- 0
        while (a <= 1) {
            kount <- kount + 1
            a <- a * 10
        }
        while (a > 10) {
            kount <- kount - 1
            a <- a/10
        }
        ia <- trunc(a)
        if (ia == 6) 
            ia <- 7
        if (ia == 8) 
            ia <- 9
        aa <- 0
        if (a < 1.5) 
            aa <- -0.5
        a <- aa + 1 + ia
        a <- a/(10^kount)
        test <- (m - 1) * a
        test1 <- (dmax - dmin)/test
        if (test1 > 0.8) 
            kod <- 2
        if (kod < 2) {
            kount <- 1
            r <- dmax - dmin
            b <- dmin
            kod <- kod + 1
        }
    }
    iab <- trunc(b/a)
    if (iab < 0) 
        iab <- iab - 1
    c <- a * iab
    d <- c + (m - 1) * a
    if (d >= dmax) 
        b <- c
    valmin <- b
    valmax <- b + a * (m - 1)
    seq(from = valmin, to = valmax, by = a)
}


extended.figures <- function (samples = 100) 
{
    oldpar <- par()
    par(ask = TRUE)
    a <- runif(samples, -100, 400)
    b <- runif(samples, -100, 400)
    low <- pmin(a, b)
    high <- pmax(a, b)
    ticks <- runif(samples, 2, 10)
    generate.labelings <- function(labeler, dmin, dmax, ticks, 
        ...) {
        mapply(labeler, dmin, dmax, ticks, SIMPLIFY = FALSE, 
            MoreArgs = list(...))
    }
    h1 <- generate.labelings(heckbert, low, high, ticks)
    w1 <- generate.labelings(wilkinson, low, high, ticks, mincoverage = 0.8)
    f1 <- generate.labelings(extended, low, high, ticks, only.loose = TRUE)
    e1 <- generate.labelings(extended, low, high, ticks)
    figure2 <- function(r, names) {
        for (i in 1:length(r)) {
            d <- r[[i]]
            cover <- sapply(d, function(x) {
                max(x) - min(x)
            })/(high - low)
            hist(cover, breaks = seq(from = -0.01, to = 1000, 
                by = 0.02), xlab = "", ylab = names[i], main = ifelse(i == 
                1, "Density", ""), col = "darkgray", lab = c(3, 
                3, 3), xlim = c(0.5, 3.5), ylim = c(0, 0.12 * 
                samples), axes = FALSE, border = FALSE)
            axis(side = 1, at = c(0, 1, 2, 3, 4), xlab = "hello", 
                line = -0.1, lwd = 0.5)
            dens <- sapply(d, length)/ticks
            hist(dens, breaks = seq(from = -0.01, to = 10, by = 0.02), 
                xlab = "", ylab = names[i], main = ifelse(i == 
                  1, "Density", ""), col = "darkgray", lab = c(3, 
                  3, 3), xlim = c(0.5, 3.5), ylim = c(0, 0.06 * 
                  samples), axes = FALSE, border = FALSE)
            axis(side = 1, at = c(0, 1, 2, 3, 4), xlab = "hello", 
                line = -0.1, lwd = 0.5)
        }
    }
    par(mfrow = c(4, 2), mar = c(0.5, 1.85, 1, 0), oma = c(1, 
        0, 1, 0), mgp = c(0, 0.5, -0.3), font.main = 1, font.lab = 1, 
        cex.lab = 1, cex.main = 1, tcl = -0.2)
    figure2(list(h1, w1, f1, e1), names = c("Heckbert", "Wilkinson", 
        "Extended\n(loose)", "Extended\n(flexible)"))
    figure3 <- function(r, names) {
        for (i in 1:length(r)) {
            d <- r[[i]]
            steps <- sapply(d, function(x) round(median(diff(x)), 
                2))
            steps <- steps/(10^floor(log10(steps)))
            tab <- table(steps)
            barplot(rev(tab), xlim = c(0, 0.4 * samples), horiz = TRUE, 
                xlab = ifelse(i == 1, "Frequency", ""), xaxt = "n", 
                yaxt = "s", las = 1, main = names[i], border = NA, 
                col = "gray")
        }
    }
    par(mfrow = c(1, 4), mar = c(0.5, 0.75, 2, 0.5), oma = c(0, 
        2, 1, 1), mgp = c(0, 0.75, -0.3), cex.lab = 1, cex.main = 1)
    figure3(list(h1, w1, f1, e1), names = c("Heckbert", "Wilkinson", 
        "Extended\n(loose)", "Extended\n(flexible)"))
    par(oldpar)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Axis Labeling"

.skeleton_package_version = "0.3"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF