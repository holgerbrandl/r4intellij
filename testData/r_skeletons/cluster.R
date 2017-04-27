##
## Exported symobls in package `cluster`
##

## Exported package methods

coefHier <- function (object) 
{
    nh <- length(ht <- object$height)
    stopifnot(nh > 0, is.numeric(ht))
    .C(R_bncoef, n = as.integer(nh + 1L), ban = as.double(c(0, 
        ht)), cf = double(1))$cf
}


sizeDiss <- function (d) 
{
    discr <- 1 + 8 * length(d)
    sqrtdiscr <- round(sqrt(discr))
    if (sqrtdiscr^2 == discr) 
        (1 + sqrtdiscr)/2
    else NA
}


meanabsdev <- function (y) 
mean(abs(y - mean(y, na.rm = TRUE)), na.rm = TRUE)


daisy <- function (x, metric = c("euclidean", "manhattan", "gower"), stand = FALSE, 
    type = list(), weights = rep.int(1, p)) 
{
    if (length(dx <- dim(x)) != 2 || !(is.data.frame(x) || is.numeric(x))) 
        stop("x is not a dataframe or a numeric matrix.")
    n <- dx[1]
    p <- dx[2]
    varnms <- dimnames(x)[[2]]
    pColl <- function(n) paste(n, collapse = ", ")
    if (length(type)) {
        if (!is.list(type) || is.null(ntyp <- names(type)) || 
            any(ntyp == "")) 
            stop(gettextf("invalid %s; must be named list", sQuote("type")))
        for (nt in ntyp) {
            cvec <- type[[nt]]
            ct <- paste0("type$", nt)
            if (is.character(cvec)) {
                if (!is.null(varnms) && !all(cvec %in% varnms)) 
                  stop(gettextf("%s has invalid column names", 
                    ct))
            }
            else if (is.numeric(cvec)) {
                if (!all(1 <= cvec & cvec <= p)) 
                  stop(gettextf("%s must be in 1:ncol(x)", ct))
            }
            else stop(gettextf("%s must contain column names or numbers", 
                ct))
        }
        tA <- type$asymm
        tS <- type$symm
        if (!is.null(tA) || !is.null(tS)) {
            d.bin <- cbind(as.data.frame(x[, tA, drop = FALSE]), 
                x[, tS, drop = FALSE])
            lenB <- sapply(lapply(d.bin, function(y) levels(as.factor(y))), 
                length)
            if (any(lenB > 2)) 
                stop("at least one binary variable has more than 2 levels.")
            if (any(lenB < 2)) 
                warning("at least one binary variable has not 2 different levels.")
            if (any(is.f <- sapply(d.bin, is.factor))) 
                d.bin[is.f] <- lapply(d.bin[is.f], function(f) as.integer(as.character(f)))
            if (!all(sapply(d.bin, function(y) is.logical(y) || 
                all(sort(unique(as.numeric(y[!is.na(y)]))) %in% 
                  0:1)))) 
                stop("at least one binary variable has values not in {0,1,NA}")
        }
    }
    if (is.data.frame(x)) {
        type2 <- sapply(x, data.class)
        x <- data.matrix(x)
    }
    else {
        type2 <- rep("numeric", p)
        names(type2) <- colnames(x)
    }
    if (length(type)) {
        tT <- type$ordratio
        tL <- type$logratio
        x[, names(type2[tT])] <- unclass(as.ordered(x[, names(type2[tT])]))
        x[, names(type2[tL])] <- log10(x[, names(type2[tL])])
        type2[tA] <- "A"
        type2[tS] <- "S"
        type2[tT] <- "T"
    }
    type2[tI <- type2 %in% c("numeric", "integer")] <- "I"
    if (n > 9 && any(tI) && any(iBin <- apply(x[, tI, drop = FALSE], 
        2, function(v) length(table(v)) == 2))) 
        warning(gettextf("binary variable(s) %s treated as interval scaled", 
            pColl(which(tI)[iBin])))
    type2[type2 == "ordered"] <- "O"
    type2[type2 == "factor"] <- "N"
    if (any(ilog <- type2 == "logical")) {
        warning(sprintf(ngettext(sum(ilog), "setting 'logical' variable %s to type 'asymm'", 
            "setting 'logical' variables %s to type 'asymm'"), 
            pColl(which(ilog))), domain = NA)
        type2[ilog] <- "A"
    }
    all.I <- all(type2 == "I")
    if (all.I && {
        metric <- match.arg(metric)
        metric != "gower"
    }) {
        if (stand) {
            x <- scale(x, center = TRUE, scale = FALSE)
            sx <- colMeans(abs(x), na.rm = TRUE)
            if (0 %in% sx) {
                warning(gettextf("%s has constant columns %s; these are standardized to 0", 
                  sQuote("x"), pColl(which(sx == 0))))
                sx[sx == 0] <- 1
            }
            x <- scale(x, center = FALSE, scale = sx)
        }
        jdat <- 2L
        ndyst <- if (metric == "manhattan") 
            2L
        else 1L
    }
    else {
        if (!missing(metric) && metric != "gower" && !all.I) 
            warning("with mixed variables, metric \"gower\" is used automatically")
        colR <- apply(x, 2, range, na.rm = TRUE)
        colmin <- colR[1, ]
        sx <- colR[2, ] - colmin
        if (any(sx == 0)) 
            sx[sx == 0] <- 1
        x <- scale(x, center = colmin, scale = sx)
        jdat <- 1L
        ndyst <- 0L
        if (length(weights) == 1) 
            weights <- rep.int(weights, p)
        else if (length(weights) != p) 
            stop("'weights' must be of length p (or 1)")
    }
    typeCodes <- c("A", "S", "N", "O", "I", "T")
    type3 <- match(type2, typeCodes)
    if (any(ina <- is.na(type3))) 
        stop(gettextf("invalid type %s for column numbers %s", 
            type2[ina], pColl(which(ina))))
    if ((mdata <- any(inax <- is.na(x)))) {
        jtmd <- integer(p)
        jtmd[apply(inax, 2L, any)] <- -1L
        valmisdat <- 1.1 * max(abs(range(x, na.rm = TRUE)))
        x[inax] <- valmisdat
    }
    storage.mode(x) <- "double"
    disv <- .Fortran(cl_daisy, n, p, x, if (mdata) rep(valmisdat, 
        p) else double(1), as.double(weights), if (mdata) jtmd else integer(1), 
        jdat, type3, ndyst, as.integer(mdata), dis = double((n * 
            (n - 1))/2), NAOK = TRUE)$dis
    disv[disv == -1] <- NA
    full <- matrix(0, n, n)
    full[!lower.tri(full, diag = TRUE)] <- disv
    disv <- t(full)[lower.tri(full)]
    if (anyNA(disv)) 
        attr(disv, "NA.message") <- "NA-values in the dissimilarity matrix !"
    class(disv) <- dissiCl
    attr(disv, "Labels") <- dimnames(x)[[1]]
    attr(disv, "Size") <- n
    attr(disv, "Metric") <- if (!ndyst) 
        "mixed"
    else metric
    if (!ndyst) 
        attr(disv, "Types") <- typeCodes[type3]
    disv
}


mona <- function (x) 
{
    if (!is.matrix(x) && !is.data.frame(x)) 
        stop("x must be a matrix or data frame.")
    if (!all(vapply(lapply(as.data.frame(x), function(y) levels(as.factor(y))), 
        length, 1) == 2)) 
        stop("All variables must be binary (e.g., factor with 2 levels).")
    n <- nrow(x)
    jp <- ncol(x)
    x2 <- apply(as.matrix(x), 2, function(x) as.integer(factor(x))) - 
        1L
    x2[is.na(x2)] <- 2:2
    res <- .Fortran(cl_mona, as.integer(n), as.integer(jp), x2 = x2, 
        error = 0L, nban = integer(n), ner = integer(n), integer(n), 
        lava = integer(n), integer(jp))
    if (res$error != 0) {
        switch(res$error, stop("No clustering performed, an object was found with all values missing."), 
            stop("No clustering performed, found variable with more than half values missing."), 
            stop("No clustering performed, a variable was found with all non missing values identical."), 
            stop("No clustering performed, all variables have at least one missing value."))
    }
    storage.mode(res$x2) <- "integer"
    dimnames(res$x2) <- dnx <- dimnames(x)
    if (length(dnx[[2]]) != 0) {
        lava <- as.character(res$lava)
        lava[lava != "0"] <- dnx[[2]][res$lava]
        lava[lava == "0"] <- "NULL"
        res$lava <- lava
    }
    clustering <- list(data = res$x2, order = res$ner, variable = res$lava[-1], 
        step = res$nban[-1], call = match.call())
    if (length(dnx[[1]]) != 0) 
        clustering$order.lab <- dnx[[1]][res$ner]
    class(clustering) <- "mona"
    clustering
}


agnes <- function (x, diss = inherits(x, "dist"), metric = "euclidean", 
    stand = FALSE, method = "average", par.method, keep.diss = n < 
        100, keep.data = !diss, trace.lev = 0) 
{
    METHODS <- c("average", "single", "complete", "ward", "weighted", 
        "flexible", "gaverage")
    meth <- pmatch(method, METHODS)
    if (is.na(meth)) 
        stop("invalid clustering method")
    if (meth == -1) 
        stop("ambiguous clustering method")
    cl. <- match.call()
    method <- METHODS[meth]
    if (method == "flexible") {
        stopifnot((np <- length(a <- as.numeric(par.method))) >= 
            1)
        attr(method, "par") <- par.method <- if (np == 1) 
            c(a, a, 1 - 2 * a, 0)
        else if (np == 3) 
            c(a, 0)
        else if (np == 4) 
            a
        else stop("'par.method' must be of length 1, 3, or 4")
    }
    else if (method == "gaverage") {
        attr(method, "par") <- par.method <- if (missing(par.method)) {
            beta <- -0.1
            c(1 - beta, 1 - beta, beta, 0)
        }
        else {
            stopifnot((np <- length(b <- as.numeric(par.method))) >= 
                1)
            if (np == 1) 
                c(1 - b, 1 - b, b, 0)
            else if (np == 3) 
                c(b, 0)
            else if (np == 4) 
                b
            else stop("'par.method' must be of length 1, 3, or 4")
        }
    }
    else par.method <- double()
    if ((diss <- as.logical(diss))) {
        if (anyNA(x)) 
            stop("NA-values in the dissimilarity matrix not allowed.")
        if (data.class(x) != "dissimilarity") {
            if (!is.null(dim(x))) {
                x <- as.dist(x)
            }
            else {
                if (!is.numeric(x) || is.na(n <- sizeDiss(x))) 
                  stop("'x' is not and cannot be converted to class \"dissimilarity\"")
                attr(x, "Size") <- n
            }
            class(x) <- dissiCl
            if (is.null(attr(x, "Metric"))) 
                attr(x, "Metric") <- "unspecified"
        }
        n <- attr(x, "Size")
        dv <- x[lower.to.upper.tri.inds(n)]
        dv <- c(0, dv)
        jp <- 1L
        mdata <- FALSE
        ndyst <- 0
        x2 <- double(1)
    }
    else {
        x <- data.matrix(x)
        if (!is.numeric(x)) 
            stop("x is not a numeric dataframe or matrix.")
        x2 <- if (stand) 
            scale(x, scale = apply(x, 2, meanabsdev))
        else x
        storage.mode(x2) <- "double"
        ndyst <- if (metric == "manhattan") 
            2
        else 1
        n <- nrow(x2)
        jp <- ncol(x2)
        if ((mdata <- any(inax <- is.na(x2)))) {
            jtmd <- integer(jp)
            jtmd[apply(inax, 2L, any)] <- -1L
            valmisdat <- 1.1 * max(abs(range(x2, na.rm = TRUE)))
            x2[inax] <- valmisdat
        }
        dv <- double(1 + (n * (n - 1))/2)
    }
    if (n <= 1) 
        stop("need at least 2 objects to cluster")
    stopifnot(length(trace.lev <- as.integer(trace.lev)) == 1)
    C.keep.diss <- keep.diss && !diss
    res <- .C(twins, as.integer(n), as.integer(jp), x2, dv, dis = double(if (C.keep.diss) length(dv) else 1), 
        jdyss = if (C.keep.diss) diss + 10L else as.integer(diss), 
        if (mdata) rep(valmisdat, jp) else double(1), if (mdata) jtmd else integer(jp), 
        as.integer(ndyst), 1L, meth, integer(n), ner = integer(n), 
        ban = double(n), ac = double(1), par.method, merge = matrix(0L, 
            n - 1, 2), trace = trace.lev)
    if (!diss) {
        if (res$jdyss == -1) 
            stop("No clustering performed, NA-values in the dissimilarity matrix.\n")
        if (keep.diss) {
            disv <- res$dis[-1]
            disv[disv == -1] <- NA
            disv <- disv[upper.to.lower.tri.inds(n)]
            class(disv) <- dissiCl
            attr(disv, "Size") <- nrow(x)
            attr(disv, "Metric") <- metric
            attr(disv, "Labels") <- dimnames(x)[[1]]
        }
        if (length(dimnames(x)[[1]]) != 0) 
            order.lab <- dimnames(x)[[1]][res$ner]
    }
    else {
        if (keep.diss) 
            disv <- x
        if (length(attr(x, "Labels")) != 0) 
            order.lab <- attr(x, "Labels")[res$ner]
    }
    clustering <- list(order = res$ner, height = res$ban[-1], 
        ac = res$ac, merge = res$merge, diss = if (keep.diss) disv, 
        call = cl., method = METHODS[meth])
    if (exists("order.lab")) 
        clustering$order.lab <- order.lab
    if (keep.data && !diss) {
        if (mdata) 
            x2[x2 == valmisdat] <- NA
        clustering$data <- x2
    }
    class(clustering) <- c("agnes", "twins")
    clustering
}


fanny <- function (x, k, diss = inherits(x, "dist"), memb.exp = 2, metric = c("euclidean", 
    "manhattan", "SqEuclidean"), stand = FALSE, iniMem.p = NULL, 
    cluster.only = FALSE, keep.diss = !diss && !cluster.only && 
        n < 100, keep.data = !diss && !cluster.only, maxit = 500, 
    tol = 1e-15, trace.lev = 0) 
{
    if ((diss <- as.logical(diss))) {
        if (anyNA(x)) 
            stop("NA values in the dissimilarity matrix not allowed.")
        if (data.class(x) != "dissimilarity") {
            if (!is.null(dim(x))) {
                x <- as.dist(x)
            }
            else {
                if (!is.numeric(x) || is.na(n <- sizeDiss(x))) 
                  stop("'x' is not and cannot be converted to class \"dissimilarity\"")
                attr(x, "Size") <- n
            }
            class(x) <- dissiCl
            if (is.null(attr(x, "Metric"))) 
                attr(x, "Metric") <- "unspecified"
        }
        n <- attr(x, "Size")
        dv <- as.double(c(x, 0))
        jp <- 1
        mdata <- FALSE
        ndyst <- 0L
        x2 <- double(n)
        jdyss <- 1
    }
    else {
        x <- data.matrix(x)
        if (!is.numeric(x)) 
            stop("x is not a numeric dataframe or matrix.")
        x2 <- if (stand) 
            scale(x, scale = apply(x, 2, meanabsdev))
        else x
        metric <- match.arg(metric)
        ndyst <- which(metric == eval(formals()$metric))
        n <- nrow(x2)
        jp <- ncol(x2)
        if ((mdata <- any(inax <- is.na(x2)))) {
            jtmd <- as.integer(ifelse(apply(inax, 2, any), -1, 
                1))
            valmisdat <- 1.1 * max(abs(range(x2, na.rm = TRUE)))
            x2[inax] <- valmisdat
        }
        dv <- double(1 + (n * (n - 1))/2)
        jdyss <- 0
    }
    if ((k <- as.integer(k)) < 1 || k > n%/%2 - 1) 
        stop("'k' (number of clusters) must be in {1,2, .., n/2 -1}")
    if (length(memb.exp) != 1 || (memb.exp <- as.double(memb.exp)) < 
        1 || memb.exp == Inf) 
        stop("'memb.exp' must be a finite number > 1")
    if ((maxit <- as.integer(maxit)[1]) < 0) 
        stop("'maxit' must be non-negative integer")
    computeP <- is.null(iniMem.p)
    if (computeP) 
        iniMem.p <- matrix(0, n, k)
    else {
        dm <- dim(iniMem.p)
        if (length(dm) != 2 || !all(dm == c(n, k)) || !is.numeric(iniMem.p) || 
            any(iniMem.p < 0) || !isTRUE(all.equal(unname(rowSums(iniMem.p)), 
            rep(1, n)))) 
            stop("'iniMem.p' must be a nonnegative n * k matrix with rowSums == 1")
        if (!is.double(iniMem.p)) 
            storage.mode(iniMem.p) <- "double"
    }
    stopifnot(length(cluster.only) == 1)
    stopifnot(length(trace.lev) == 1)
    storage.mode(x2) <- "double"
    res <- .C(cl_fanny, as.integer(n), as.integer(jp), k, x2, 
        dis = dv, ok = as.integer(jdyss), if (mdata) rep(valmisdat, 
            jp) else double(1), if (mdata) jtmd else integer(jp), 
        ndyst, integer(n), integer(n), integer(n), double(n), 
        p = iniMem.p, dp = matrix(0, n, k), avsil = double(k), 
        integer(k), double(k), double(k), double(n), ttsil = as.double(0), 
        obj = as.double(c(cluster.only, trace.lev, computeP, 
            0)), clu = integer(n), silinf = if (cluster.only) 0 else matrix(0, 
            n, 4), memb.exp = memb.exp, tol = as.double(tol), 
        maxit = maxit)
    if (!(converged <- res$maxit > 0)) {
        warning(gettextf("FANNY algorithm has not converged in 'maxit' = %d iterations", 
            maxit))
    }
    if (!cluster.only) 
        sildim <- res$silinf[, 4]
    if (diss) {
        if (keep.diss) 
            disv <- x
        labs <- attr(x, "Labels")
    }
    else {
        if (res$ok == -1) 
            stop("No clustering performed, NA-values in the dissimilarity matrix.")
        labs <- dimnames(x)[[1]]
        if (keep.diss) {
            disv <- res$dis[-(1 + (n * (n - 1))/2)]
            disv[disv == -1] <- NA
            class(disv) <- dissiCl
            attr(disv, "Size") <- nrow(x)
            attr(disv, "Metric") <- metric
            attr(disv, "Labels") <- labs
        }
    }
    if (length(labs) != 0) {
        if (!cluster.only) 
            sildim <- labs[sildim]
        dimnames(res$p) <- list(labs, NULL)
        names(res$clu) <- labs
    }
    coeff <- if (memb.exp == 2) 
        res$obj[3:4]
    else {
        cf <- sum(res$p^2)/n
        c(cf, (k * cf - 1)/(k - 1))
    }
    names(coeff) <- c("dunn_coeff", "normalized")
    if (abs(coeff["normalized"]) < 1e-07) 
        warning("the memberships are all very close to 1/k. Maybe decrease 'memb.exp' ?")
    k.crisp <- res$obj[1]
    res$obj <- c(objective = res$obj[2])
    r <- list(membership = res$p, coeff = coeff, memb.exp = memb.exp, 
        clustering = res$clu, k.crisp = k.crisp, objective = c(res$obj, 
            tolerance = res$tol), convergence = c(iterations = res$maxit, 
            converged = converged, maxit = maxit), diss = if (keep.diss) disv, 
        call = match.call())
    if (k != 1 && !cluster.only) {
        dimnames(res$silinf) <- list(sildim, c("cluster", "neighbor", 
            "sil_width", ""))
        r$silinfo <- list(widths = res$silinf[, -4], clus.avg.widths = res$avsil[1:k], 
            avg.width = res$ttsil)
    }
    if (keep.data && !diss) {
        if (mdata) 
            x2[x2 == valmisdat] <- NA
        r$data <- x2
    }
    class(r) <- c("fanny", "partition")
    r
}


upper.to.lower.tri.inds <- function (n) 
{
    if ((n2 <- as.integer(n - 2L)) < 0) 
        stop("'n' must be >= 2")
    rep(1L + cumsum(0:n2), (n - 1):1) + unlist(lapply(0:n2, function(k) cumsum(k:n2)))
}


ellipsoidPoints <- function (A, d2, loc, n.half = 201) 
{
    if (length(d <- dim(A)) != 2 || (p <- d[1]) != d[2]) 
        stop("'A' must be p x p  cov-matrix defining an ellipsoid")
    if (p == 2) {
        detA <- A[1, 1] * A[2, 2] - A[1, 2]^2
        yl2 <- A[2, 2] * d2
        y <- seq(-sqrt(yl2), sqrt(yl2), length = n.half)
        sqrt.discr <- sqrt(detA * pmax(0, yl2 - y^2))/A[2, 2]
        sqrt.discr[c(1, n.half)] <- 0
        b <- loc[1] + A[1, 2]/A[2, 2] * y
        y <- loc[2] + y
        return(rbind(cbind(b - sqrt.discr, y), cbind(rev(b + 
            sqrt.discr), rev(y))))
    }
    else {
        detA <- det(A)
        stop("ellipsoidPoints() not yet implemented for p >= 3 dim.")
    }
}


predict.ellipsoid <- function (object, n.out = 201, ...) 
ellipsoidPoints(object$cov, d2 = object$d2, loc = object$loc, 
    n.half = n.out)


pltree <- function (x, ...) 
UseMethod("pltree")


coef.hclust <- function (object, ...) 
{
    ht <- object$height
    mrg <- object$merge
    nh <- length(ht)
    stopifnot(nh > 0, is.matrix(mrg), dim(mrg) == c(nh, 2), is.numeric(ht), 
        is.numeric(mrg), !is.unsorted(ht))
    1 - sum(rowSums(mrg < 0) * ht)/max(ht)/(nh + 1)
}


lower.to.upper.tri.inds <- function (n) 
{
    n1 <- as.integer(n - 1)
    if (n1 < 1) 
        stop("'n' must be >= 2")
    else if (n1 == 1) 
        1L
    else rep(seq_len(n1), seq_len(n1)) + c(0L, unlist(lapply(2:n1, 
        function(k) cumsum(c(0L, (n - 2L):(n - k))))))
}


silhouette <- function (x, ...) 
UseMethod("silhouette")


bannerplot <- function (x, w = rev(x$height), fromLeft = TRUE, main = NULL, 
    sub = NULL, xlab = "Height", adj = 0, col = c(2, 0), border = 0, 
    axes = TRUE, frame.plot = axes, rev.xax = !fromLeft, xax.pretty = TRUE, 
    labels = NULL, nmax.lab = 35, max.strlen = 5, yax.do = axes && 
        length(x$order) <= nmax.lab, yaxRight = fromLeft, y.mar = 2.4 + 
        max.strlen/2.5, ...) 
{
    m <- max(w)
    if (axes) {
        if (xax.pretty) {
            at.vals <- if (!is.logical(xax.pretty)) 
                pretty(c(0, w), n = xax.pretty)
            else pretty(c(0, w))
            n <- length(at.vals <- at.vals[at.vals <= m])
            if (at.vals[n] * 1.01 < m) {
                lab.vals <- c(at.vals, signif(m, 3))
                at.vals <- c(at.vals, m)
            }
            else lab.vals <- at.vals
        }
        else {
            ss <- seq(0, floor(m), length = 11)
            at.vals <- c(ss, m)
            lab.vals <- round(at.vals, 2)
        }
    }
    if (fromLeft) {
        w <- rbind(w, m - w)
        if (missing(col)) 
            col <- rev(col)
    }
    else {
        w <- rbind(m - w, w)
        if (axes && rev.xax) {
            at.vals <- m - rev(at.vals)
            lab.vals <- rev(lab.vals)
        }
    }
    if (yax.do) {
        ax <- if (yaxRight) 
            list(side = 4, pos = m)
        else list(side = 2, pos = 0)
        if ((pm <- par("mar"))[ax$side] < y.mar) {
            pm[ax$side] <- y.mar
            op <- par(mar = pm)
            on.exit(par(op))
        }
    }
    barplot(w, xlab = xlab, horiz = TRUE, space = 0, axes = FALSE, 
        col = col, border = border, mgp = c(2.5, 1, 0), ...)
    if (frame.plot && (border == 0 || border == par("bg"))) 
        rect(0, 0, m, ncol(w))
    title(main = main, sub = sub, adj = adj)
    if (axes) {
        axis(1, at = at.vals, labels = lab.vals, ...)
        if (yax.do) {
            if (is.null(labels)) 
                labels <- rev(if (length(x$order.lab) != 0) 
                  substring(x$order.lab, 1, max.strlen)
                else x$order)
            axis(ax$side, at = 0:(length(x$order) - 1), las = 1, 
                labels = labels, pos = ax$pos, mgp = c(3, 1.25, 
                  0), ...)
        }
    }
    invisible()
}


clusGap <- function (x, FUNcluster, K.max, B = 100, d.power = 1, verbose = interactive(), 
    ...) 
{
    stopifnot(is.function(FUNcluster), length(dim(x)) == 2, K.max >= 
        2, (n <- nrow(x)) >= 1, ncol(x) >= 1)
    if (B != (B. <- as.integer(B)) || (B <- B.) <= 0) 
        stop("'B' has to be a positive integer")
    if (is.data.frame(x)) 
        x <- as.matrix(x)
    ii <- seq_len(n)
    W.k <- function(X, kk) {
        clus <- if (kk > 1) 
            FUNcluster(X, kk, ...)$cluster
        else rep.int(1L, nrow(X))
        0.5 * sum(vapply(split(ii, clus), function(I) {
            xs <- X[I, , drop = FALSE]
            sum(dist(xs)^d.power/nrow(xs))
        }, 0))
    }
    logW <- E.logW <- SE.sim <- numeric(K.max)
    if (verbose) 
        cat("Clustering k = 1,2,..., K.max (= ", K.max, "): .. ", 
            sep = "")
    for (k in 1:K.max) logW[k] <- log(W.k(x, k))
    if (verbose) 
        cat("done\n")
    xs <- scale(x, center = TRUE, scale = FALSE)
    m.x <- rep(attr(xs, "scaled:center"), each = n)
    V.sx <- svd(xs, nu = 0)$v
    rng.x1 <- apply(xs %*% V.sx, 2, range)
    logWks <- matrix(0, B, K.max)
    if (verbose) 
        cat("Bootstrapping, b = 1,2,..., B (= ", B, ")  [one \".\" per sample]:\n", 
            sep = "")
    for (b in 1:B) {
        z1 <- apply(rng.x1, 2, function(M, nn) runif(nn, min = M[1], 
            max = M[2]), nn = n)
        z <- tcrossprod(z1, V.sx) + m.x
        for (k in 1:K.max) {
            logWks[b, k] <- log(W.k(z, k))
        }
        if (verbose) 
            cat(".", if (b%%50 == 0) 
                paste(b, "\n"))
    }
    if (verbose && (B%%50 != 0)) 
        cat("", B, "\n")
    E.logW <- colMeans(logWks)
    SE.sim <- sqrt((1 + 1/B) * apply(logWks, 2, var))
    structure(class = "clusGap", list(Tab = cbind(logW, E.logW, 
        gap = E.logW - logW, SE.sim), n = n, B = B, FUNcluster = FUNcluster))
}


volume <- function (object) 
UseMethod("volume")


clusplot <- function (x, ...) 
UseMethod("clusplot")


maxSE <- function (f, SE.f, method = c("firstSEmax", "Tibs2001SEmax", 
    "globalSEmax", "firstmax", "globalmax"), SE.factor = 1) 
{
    method <- match.arg(method)
    stopifnot((K <- length(f)) >= 1, K == length(SE.f), SE.f >= 
        0, SE.factor >= 0)
    fSE <- SE.factor * SE.f
    switch(method, firstmax = {
        decr <- diff(f) <= 0
        if (any(decr)) which.max(decr) else K
    }, globalmax = {
        which.max(f)
    }, Tibs2001SEmax = {
        g.s <- f - fSE
        if (any(mp <- f[-K] >= g.s[-1])) which.max(mp) else K
    }, firstSEmax = {
        decr <- diff(f) <= 0
        nc <- if (any(decr)) which.max(decr) else K
        if (any(mp <- f[seq_len(nc - 1)] >= f[nc] - fSE[nc])) which(mp)[1] else nc
    }, globalSEmax = {
        nc <- which.max(f)
        if (any(mp <- f[seq_len(nc - 1)] >= f[nc] - fSE[nc])) which(mp)[1] else nc
    })
}


diana <- function (x, diss = inherits(x, "dist"), metric = "euclidean", 
    stand = FALSE, keep.diss = n < 100, keep.data = !diss, trace.lev = 0) 
{
    if ((diss <- as.logical(diss))) {
        if (anyNA(x)) 
            stop("NA values in the dissimilarity matrix not allowed.")
        if (data.class(x) != "dissimilarity") {
            if (!is.null(dim(x))) {
                x <- as.dist(x)
            }
            else {
                if (!is.numeric(x) || is.na(n <- sizeDiss(x))) 
                  stop("'x' is not and cannot be converted to class \"dissimilarity\"")
                attr(x, "Size") <- n
            }
            class(x) <- dissiCl
            if (is.null(attr(x, "Metric"))) 
                attr(x, "Metric") <- "unspecified"
        }
        n <- as.integer(attr(x, "Size"))
        dv <- x[lower.to.upper.tri.inds(n)]
        dv <- c(0, dv)
        jp <- 1L
        mdata <- FALSE
        ndyst <- 0
        x2 <- double(1)
    }
    else {
        x <- data.matrix(x)
        if (!is.numeric(x)) 
            stop("x is not a numeric dataframe or matrix.")
        x2 <- if (stand) 
            scale(x, scale = apply(x, 2, meanabsdev))
        else x
        ndyst <- if (metric == "manhattan") 
            2
        else 1
        n <- nrow(x2)
        jp <- ncol(x2)
        if ((mdata <- any(inax <- is.na(x2)))) {
            jtmd <- integer(jp)
            jtmd[apply(inax, 2L, any)] <- -1L
            valmisdat <- 1.1 * max(abs(range(x2, na.rm = TRUE)))
            x2[inax] <- valmisdat
        }
        dv <- double(1 + (n * (n - 1))/2)
    }
    stopifnot(length(trace.lev <- as.integer(trace.lev)) == 1)
    C.keep.diss <- keep.diss && !diss
    res <- .C(twins, n, jp, as.double(x2), dv, dis = double(if (C.keep.diss) length(dv) else 1), 
        jdyss = if (C.keep.diss) diss + 10L else as.integer(diss), 
        if (mdata) rep(valmisdat, jp) else double(1), if (mdata) jtmd else integer(jp), 
        as.integer(ndyst), 2L, 0L, integer(n), ner = integer(n), 
        ban = double(n), dc = double(1), double(1), merge = matrix(0L, 
            n - 1, 2), trace = trace.lev)
    if (!diss) {
        if (res$jdyss == -1) 
            stop("No clustering performed, NA's in dissimilarity matrix.\n")
        if (keep.diss) {
            disv <- res$dis[-1]
            disv[disv == -1] <- NA
            disv <- disv[upper.to.lower.tri.inds(n)]
            class(disv) <- dissiCl
            attr(disv, "Size") <- nrow(x)
            attr(disv, "Metric") <- metric
            attr(disv, "Labels") <- dimnames(x)[[1]]
        }
        if (length(dimnames(x)[[1]]) != 0) 
            order.lab <- dimnames(x)[[1]][res$ner]
    }
    else {
        if (keep.diss) 
            disv <- x
        if (length(attr(x, "Labels")) != 0) 
            order.lab <- attr(x, "Labels")[res$ner]
    }
    clustering <- list(order = res$ner, height = res$ban[-1], 
        dc = res$dc, merge = res$merge, diss = if (keep.diss) disv, 
        call = match.call())
    if (exists("order.lab")) 
        clustering$order.lab <- order.lab
    if (keep.data && !diss) {
        if (mdata) 
            x2[x2 == valmisdat] <- NA
        clustering$data <- x2
    }
    class(clustering) <- c("diana", "twins")
    clustering
}


ellipsoidhull <- function (x, tol = 0.01, maxit = 5000, ret.wt = FALSE, ret.sqdist = FALSE, 
    ret.pr = FALSE) 
{
    if (!is.matrix(x) || !is.numeric(x)) 
        stop("'x' must be numeric  n x p matrix")
    if (anyNA(x)) {
        warning("omitting NAs")
        x <- na.omit(x)
    }
    n <- nrow(x)
    if (n == 0) 
        stop("no points without missing values")
    p <- ncol(x)
    res <- .C(spannel, n, ndep = p, dat = cbind(1, x), sqdist = double(n), 
        l1 = double((p + 1)^2), double(p), double(p), prob = double(n), 
        double(p + 1), eps = as.double(tol), maxit = as.integer(maxit), 
        ierr = integer(1))
    if (res$ierr != 0) 
        cat("Error in Fortran routine computing the spanning ellipsoid,", 
            "\n probably collinear data\n", sep = "")
    if (any(res$prob < 0) || all(res$prob == 0)) 
        stop("computed some negative or all 0 probabilities")
    conv <- res$maxit < maxit
    if (!conv) 
        warning(gettextf("algorithm possibly not converged in %d iterations", 
            maxit))
    conv <- conv && res$ierr == 0
    cov <- cov.wt(x, res$prob)
    res <- list(loc = cov$center, cov = cov$cov * (1 - sum(cov$wt^2)), 
        d2 = weighted.mean(res$sqdist, res$prob), wt = if (ret.wt) cov$wt, 
        sqdist = if (ret.sqdist) res$sqdist, prob = if (ret.pr) res$prob, 
        tol = tol, eps = max(res$sqdist) - p, it = res$maxit, 
        maxit = maxit, ierr = res$ierr, conv = conv)
    class(res) <- "ellipsoid"
    res
}


clara <- function (x, k, metric = "euclidean", stand = FALSE, samples = 5, 
    sampsize = min(n, 40 + 2 * k), trace = 0, medoids.x = TRUE, 
    keep.data = medoids.x, rngR = FALSE, pamLike = FALSE, correct.d = TRUE) 
{
    if (inherits(x, "dist")) 
        stop("'x' is a \"dist\" object, but should be a data matrix or frame")
    x <- data.matrix(x)
    if (!is.numeric(x)) 
        stop("x is not a numeric dataframe or matrix.")
    n <- nrow(x)
    if ((k <- as.integer(k)) < 1 || k > n - 1) 
        stop("The number of cluster should be at least 1 and at most n-1.")
    if ((sampsize <- as.integer(sampsize)) < max(2, k + 1)) 
        stop(gettextf("'sampsize' should be at least %d = max(2, 1+ number of clusters)", 
            max(2, k + 1)), domain = NA)
    if (n < sampsize) 
        stop(gettextf("'sampsize' = %d should not be larger than the number of objects, %d", 
            sampsize, n), domain = NA)
    if ((samples <- as.integer(samples)) < 1) 
        stop("'samples' should be at least 1")
    jp <- ncol(x)
    namx <- dimnames(x)[[1]]
    if (medoids.x) 
        ox <- x
    else if (keep.data) 
        stop("when 'medoids.x' is FALSE, 'keep.data' must be too")
    if (stand) 
        x <- scale(x, scale = apply(x, 2, meanabsdev))
    if (keep.data) 
        data <- x
    dFlag <- -1L
    if ((mdata <- any(inax <- is.na(x)))) {
        jtmd <- integer(jp)
        jtmd[apply(inax, 2L, any)] <- -1L
        valmisdat <- 1.1 * max(abs(range(x, na.rm = TRUE)))
        x[inax] <- valmisdat
        if (missing(correct.d)) 
            warning("Distance computations with NAs: using correct instead of pre-2016 wrong formula.\nUse  'correct.d=FALSE'  to get previous results or set 'correct.d=TRUE' explicitly\nto suppress this warning.")
        else if (!is.finite(dFlag <- as.integer(correct.d))) 
            stop("invalid 'correct.d'")
    }
    else rm(inax)
    res <- .C(cl_clara, n, jp, k, clu = as.double(x), samples, 
        sampsize, dis = double(1 + (sampsize * (sampsize - 1))/2), 
        as.integer(mdata), valmd = if (mdata) rep(valmisdat, 
            jp) else -1, jtmd = if (mdata) jtmd else integer(1), 
        as.integer(if (metric == "manhattan") 2 else 1), as.logical(rngR[1]), 
        as.logical(pamLike[1]), as.integer(dFlag), integer(sampsize), 
        integer(sampsize), sample = integer(sampsize), integer(k), 
        imed = integer(k), double(k), double(k), double(k), avdis = double(k), 
        maxdis = double(k), ratdis = double(k), size = integer(k), 
        obj = double(1), avsil = double(k), ttsil = double(1), 
        silinf = matrix(0, sampsize, 4), jstop = integer(1), 
        as.integer(trace), double(3 * sampsize), integer(6 * 
            sampsize))
    if (res$jstop) {
        if (mdata && any(aNA <- apply(inax, 1, all))) {
            i <- which(aNA)
            nNA <- length(i)
            pasteC <- function(...) paste(..., collapse = ",")
            if (nNA < 13) 
                stop(sprintf(ngettext(nNA, "Observation %s has *only* NAs --> omit it for clustering", 
                  "Observations %s have *only* NAs --> omit them for clustering!"), 
                  pasteC(i)), domain = NA)
            else stop(sprintf(ngettext(nNA, "%d observation (%s) has *only* NAs --> omit them for clustering!", 
                "%d observations (%s ...) have *only* NAs --> omit them for clustering!"), 
                nNA, pasteC(i[1:12])), domain = NA)
        }
        if (res$jstop == 1) 
            stop("Each of the random samples contains objects between which no distance can be computed.")
        if (res$jstop == 2) 
            stop(gettextf("For each of the %d samples, at least one object was found which could not be assigned to a cluster (because of missing values).", 
                samples))
        stop("invalid 'jstop' from .C(cl_clara,.): ", res$jstop)
    }
    res$clu <- as.integer(res$clu[1:n])
    sildim <- res$silinf[, 4]
    disv <- res$dis[-1]
    disv[disv == -1] <- NA
    disv <- disv[upper.to.lower.tri.inds(sampsize)]
    class(disv) <- dissiCl
    attr(disv, "Size") <- sampsize
    attr(disv, "Metric") <- metric
    attr(disv, "Labels") <- namx[res$sample]
    res$med <- if (medoids.x) 
        ox[res$imed, , drop = FALSE]
    if (!is.null(namx)) {
        sildim <- namx[sildim]
        res$sample <- namx[res$sample]
        names(res$clu) <- namx
    }
    r <- list(sample = res$sample, medoids = res$med, i.med = res$imed, 
        clustering = res$clu, objective = res$obj, clusinfo = cbind(size = res$size, 
            max_diss = res$maxdis, av_diss = res$avdis, isolation = res$ratdis), 
        diss = disv, call = match.call())
    if (k > 1) {
        dimnames(res$silinf) <- list(sildim, c("cluster", "neighbor", 
            "sil_width", ""))
        r$silinfo <- list(widths = res$silinf[, -4], clus.avg.widths = res$avsil, 
            avg.width = res$ttsil)
    }
    if (keep.data) 
        r$data <- data
    class(r) <- c("clara", "partition")
    r
}


sortSilhouette <- function (object, ...) 
{
    if (is.null(n <- nrow(object)) || n < 1) 
        stop("invalid silhouette structure")
    if (attr(object, "Ordered")) {
        if (is.null(attr(object, "iOrd"))) 
            attr(object, "iOrd") <- 1:n
        return(object)
    }
    if (is.null(rownames(object))) 
        rownames(object) <- as.character(1:n)
    cl <- object[, "cluster"]
    r <- object[iOrd <- order(cl, -object[, "sil_width"]), , 
        drop = FALSE]
    nms <- names(at <- attributes(object))
    for (n in nms[!(nms %in% c("dim", "dimnames", "iOrd", "Ordered"))]) attr(r, 
        n) <- at[[n]]
    attr(r, "iOrd") <- iOrd
    attr(r, "Ordered") <- TRUE
    r
}


pam <- function (x, k, diss = inherits(x, "dist"), metric = "euclidean", 
    medoids = NULL, stand = FALSE, cluster.only = FALSE, do.swap = TRUE, 
    keep.diss = !diss && !cluster.only && n < 100, keep.data = !diss && 
        !cluster.only, pamonce = FALSE, trace.lev = 0) 
{
    stopifnot(length(cluster.only) == 1, length(trace.lev) == 
        1)
    use.Call <- TRUE
    if ((diss <- as.logical(diss))) {
        if (anyNA(x)) 
            stop("NA values in the dissimilarity matrix not allowed.")
        if (data.class(x) != "dissimilarity") {
            if (!is.null(dim(x))) {
                x <- as.dist(x)
            }
            else {
                if (!is.numeric(x) || is.na(n <- sizeDiss(x))) 
                  stop("'x' is not and cannot be converted to class \"dissimilarity\"")
                attr(x, "Size") <- n
            }
            class(x) <- dissiCl
            if (is.null(attr(x, "Metric"))) 
                attr(x, "Metric") <- "unspecified"
        }
        if (keep.data) 
            stop("Cannot keep data when 'x' is a dissimilarity!")
        n <- attr(x, "Size")
        dv <- x[lower.to.upper.tri.inds(n)]
        dv <- c(0, dv)
        storage.mode(dv) <- "double"
        jp <- 1
        mdata <- FALSE
        ndyst <- 0
        if (!use.Call) 
            x2 <- double()
    }
    else {
        x <- data.matrix(x)
        if (!is.numeric(x)) 
            stop("x is not a numeric dataframe or matrix.")
        x2 <- x
        dimnames(x2) <- NULL
        if (stand) 
            x2 <- scale(x2, scale = apply(x2, 2, meanabsdev))
        ndyst <- if (metric == "manhattan") 
            2
        else 1
        n <- nrow(x2)
        jp <- ncol(x2)
        if ((mdata <- any(inax <- is.na(x2)))) {
            jtmd <- integer(jp)
            jtmd[apply(inax, 2L, any)] <- -1L
            valmisdat <- 1.1 * max(abs(range(x2, na.rm = TRUE)))
            x2[inax] <- valmisdat
        }
        storage.mode(x2) <- "double"
        if (!use.Call) 
            dv <- double(1 + (n * (n - 1))/2)
    }
    if ((k <- as.integer(k)) < 1 || k >= n) 
        stop("Number of clusters 'k' must be in {1,2, .., n-1}; hence n >= 2")
    if (is.null(medoids)) {
        if (!use.Call) 
            medoids <- integer(k)
    }
    else {
        if (!is.integer(medoids)) 
            medoids <- as.integer(medoids)
        if (length(medoids) != k || any(medoids < 1L) || any(medoids > 
            n) || any(duplicated(medoids))) 
            stop(gettextf("'medoids' must be NULL or vector of %d distinct indices in {1,2, .., n}, n=%d", 
                k, n))
    }
    nisol <- integer(if (cluster.only) 
        1
    else k)
    if (do.swap) 
        nisol[1] <- 1L
    if (use.Call) 
        res <- .Call(cl_Pam, k, n, !diss, if (diss) dv else x2, 
            !cluster.only, medoids, do.swap, trace.lev, keep.diss, 
            pamonce, if (mdata) rep(valmisdat, jp) else double(1), 
            if (mdata) jtmd else integer(jp), as.integer(ndyst))
    else res <- .C(cl_pam, as.integer(n), as.integer(jp), k, 
        x = x2, dys = dv, jdyss = as.integer(diss), if (mdata) rep(valmisdat, 
            jp) else double(1), if (mdata) jtmd else integer(jp), 
        as.integer(ndyst), integer(n), logical(n), integer(if (cluster.only) 1 else n), 
        double(n), double(n), avsil = double(n), double(n), ttsil = double(1), 
        obj = as.double(c(cluster.only, trace.lev)), med = medoids, 
        clu = integer(n), clusinf = if (cluster.only) 0 else matrix(0, 
            k, 5), silinf = if (cluster.only) 0 else matrix(0, 
            n, 4), isol = nisol, as.integer(pamonce))
    if (!diss && ((use.Call && is.integer(res)) || (!use.Call && 
        res$jdyss == -1))) 
        stop("No clustering performed, NAs in the computed dissimilarity matrix.")
    xLab <- if (diss) 
        attr(x, "Labels")
    else dimnames(x)[[1]]
    r.clu <- res$clu
    if (length(xLab) > 0) 
        names(r.clu) <- xLab
    if (cluster.only) 
        return(r.clu)
    medID <- res$med
    if (any(medID <= 0)) 
        stop("error from .C(cl_pam, *): invalid medID's")
    sildim <- res$silinf[, 4]
    if (diss) {
        r.med <- if (length(xLab) > 0) {
            sildim <- xLab[sildim]
            xLab[medID]
        }
        else medID
    }
    else {
        if (keep.diss) {
            disv <- res$dys[-1]
            disv[disv == -1] <- NA
            disv <- disv[upper.to.lower.tri.inds(n)]
            class(disv) <- dissiCl
            attr(disv, "Size") <- nrow(x)
            attr(disv, "Metric") <- metric
            attr(disv, "Labels") <- dimnames(x)[[1]]
        }
        r.med <- x[medID, , drop = FALSE]
        if (length(xLab) > 0) 
            sildim <- xLab[sildim]
    }
    r.obj <- structure(res$obj, .Names = c("build", "swap"))
    r.isol <- factor(res$isol, levels = 0:2, labels = c("no", 
        "L", "L*"))
    names(r.isol) <- 1:k
    r.clusinf <- res$clusinf
    dimnames(r.clusinf) <- list(NULL, c("size", "max_diss", "av_diss", 
        "diameter", "separation"))
    r <- list(medoids = r.med, id.med = medID, clustering = r.clu, 
        objective = r.obj, isolation = r.isol, clusinfo = r.clusinf, 
        silinfo = if (k != 1) {
            silinf <- res$silinf[, -4, drop = FALSE]
            dimnames(silinf) <- list(sildim, c("cluster", "neighbor", 
                "sil_width"))
            list(widths = silinf, clus.avg.widths = res$avsil[1:k], 
                avg.width = res$ttsil)
        }, diss = if (keep.diss) {
            if (diss) x else disv
        }, call = match.call())
    if (keep.data) {
        if (mdata) 
            x2[x2 == valmisdat] <- NA
        r$data <- structure(x2, dimnames = dimnames(x))
    }
    class(r) <- c("pam", "partition")
    r
}




## Package Data

agriculture <- cluster::agriculture		## European Union Agricultural Workforces

animals <- cluster::animals		## Attributes of Animals

chorSub <- cluster::chorSub		## Subset of C-horizon of Kola Data

flower <- cluster::flower		## Flower Characteristics

plantTraits <- cluster::plantTraits		## Plant Species Traits Data

pluton <- cluster::pluton		## Isotopic Composition Plutonium Batches

ruspini <- cluster::ruspini		## Ruspini Data

votes.repub <- cluster::votes.repub		## Votes for Republican Candidate in Presidential Elections

xclara <- cluster::xclara		## Bivariate Data Set with 3 Clusters



## Package Info

.skeleton_package_title = "'Finding Groups in Data': Cluster Analysis Extended Rousseeuw etal."

.skeleton_package_version = "2.0.4"

.skeleton_package_depends = ""

.skeleton_package_imports = "graphics,grDevices,stats,utils"


## Internal

.skeleton_version = 5


## EOF