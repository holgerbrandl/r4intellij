##
## Exported symobls in package `TTR`
##

## Exported package methods

VMA <- function (x, w, ratio = 1, ...) 
{
    x <- try.xts(x, error = as.matrix)
    w <- try.xts(w, error = as.matrix)
    if (NROW(w) != NROW(x)) 
        stop("Length of 'w' must equal the length of 'x'")
    if (NCOL(x) > 1 || NCOL(w) > 1) {
        stop("ncol(x) > 1 or ncol(w) > 1. VMA only supports univariate 'x' and 'w'")
    }
    x.na <- naCheck(x, 1)
    w.na <- naCheck(w, 1)
    ma <- .Call("vma", x, abs(w), ratio, PACKAGE = "TTR")
    if (!is.null(dim(ma))) {
        colnames(ma) <- "VMA"
    }
    reclass(ma, x)
}


CCI <- function (HLC, n = 20, maType, c = 0.015, ...) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    if (NCOL(HLC) == 3) {
        if (is.xts(HLC)) {
            xa <- xcoredata(HLC)
            HLC <- xts(apply(HLC, 1, mean), index(HLC))
            xcoredata(HLC) <- xa
        }
        else {
            HLC <- apply(HLC, 1, mean)
        }
    }
    else if (NCOL(HLC) != 1) {
        stop("Price series must be either High-Low-Close, or Close/univariate.")
    }
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "SMA"
    }
    mavg <- do.call(maType, c(list(HLC), maArgs))
    meanDev <- runMAD(HLC, n, center = mavg, stat = "mean")
    cci <- (HLC - mavg)/(c * meanDev)
    if (is.xts(cci)) {
        colnames(cci) <- "cci"
    }
    reclass(cci, HLC)
}


EMV <- function (HL, volume, n = 9, maType, vol.divisor = 10000, ...) 
{
    if (missing(HL) | missing(volume)) 
        stop("High-Low matrix (HL) and volume vector must be specified.")
    HL <- try.xts(HL, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)
    if (!(is.xts(HL) && is.xts(volume))) {
        HL <- as.matrix(HL)
        volume <- as.matrix(volume)
    }
    mid <- (HL[, 1] + HL[, 2])/2
    volume <- volume/vol.divisor
    emv <- momentum(mid, n = 1, na.pad = TRUE)/(volume/(HL[, 
        1] - HL[, 2]))
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "SMA"
    }
    maEMV <- do.call(maType, c(list(emv), maArgs))
    result <- cbind(emv, maEMV)
    colnames(result) <- c("emv", "maEMV")
    reclass(result, HL)
}


runMin <- function (x, n = 10, cumulative = FALSE) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. runMin only supports univariate 'x'")
    }
    NAs <- sum(is.na(x))
    if (NAs > 0) {
        if (any(is.na(x[-(1:NAs)]))) 
            stop("Series contains non-leading NAs")
        if (NAs + n > NROW(x)) 
            stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    len <- NROW(x) - NAs
    result <- double(NROW(x))
    if (cumulative) {
        result[beg:NROW(x)] <- cummin(x[beg:NROW(x)])
    }
    else {
        result[(n + beg - 1)] <- min(x[beg:(n + beg - 1)])
        result <- .Fortran("runmin", ia = as.double(x[beg:NROW(x)]), 
            lia = as.integer(len), n = as.integer(n), oa = as.double(result[beg:NROW(x)]), 
            loa = as.integer(len), PACKAGE = "TTR", DUP = TRUE)$oa
        result <- c(rep(NA, NAs), result)
    }
    is.na(result) <- c(1:(n - 1 + NAs))
    reclass(result, x)
}


runMAD <- function (x, n = 10, center = NULL, stat = "median", constant = 1.4826, 
    non.unique = "mean", cumulative = FALSE) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. runMAD only supports univariate 'x'")
    }
    NAs <- sum(is.na(x))
    if (NAs > 0) {
        if (any(is.na(x[-(1:NAs)]))) 
            stop("Series contains non-leading NAs")
        if (NAs + n > NROW(x)) 
            stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    len <- NROW(x) - NAs
    if (is.null(center)) {
        center <- runMedian(x, n, cumulative = cumulative)
    }
    center[1:(NAs + n - 1)] <- 0
    median <- match.arg(stat, c("mean", "median"))
    median <- switch(stat, median = TRUE, mean = FALSE)
    non.unique <- match.arg(non.unique, c("mean", "max", "min"))
    non.unique <- switch(non.unique, mean = 0, max = 1, min = -1)
    result <- .Fortran("runMAD", rs = as.double(x[beg:NROW(x)]), 
        cs = as.double(center[beg:NROW(x)]), la = as.integer(len), 
        n = as.integer(n), oa = double(len), stat = as.integer(median), 
        ver = as.integer(non.unique), cu = as.integer(cumulative), 
        PACKAGE = "TTR", DUP = TRUE)$oa
    if (median) 
        result <- result * constant
    is.na(result) <- c(1:(n - 1))
    result <- c(rep(NA, NAs), result)
    reclass(result, x)
}


runSD <- function (x, n = 10, sample = TRUE, cumulative = FALSE) 
{
    result <- sqrt(runCov(x, x, n, use = "all.obs", sample = sample, 
        cumulative))
    return(result)
}


DEMA <- function (x, n = 10, v = 1, wilder = FALSE, ratio = NULL) 
{
    if (v < 0 || v > 1) {
        stop("Please ensure 0 <= v <= 1")
    }
    dema <- (1 + v) * EMA(x, n, wilder, ratio) - EMA(EMA(x, n, 
        wilder, ratio), n, wilder, ratio) * v
    if (!is.null(dim(dema))) {
        colnames(dema) <- "DEMA"
    }
    return(dema)
}


MACD <- function (x, nFast = 12, nSlow = 26, nSig = 9, maType, percent = TRUE, 
    ...) 
{
    if (missing(maType)) {
        maType <- "EMA"
    }
    if (is.list(maType)) {
        maTypeInfo <- sapply(maType, is.list)
        if (!(all(maTypeInfo) && length(maTypeInfo) == 3)) {
            stop("If 'maType' is a list, you must specify\n ", 
                "*three* MAs (see Examples section of ?MACD)")
        }
        if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
            maType[[1]]$n <- nFast
        }
        if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
            maType[[2]]$n <- nSlow
        }
        if (!is.null(formals(maType[[3]][[1]])$n) && is.null(maType[[3]]$n)) {
            maType[[3]]$n <- nSig
        }
        mavg.fast <- do.call(maType[[1]][[1]], c(list(x), maType[[1]][-1]))
        mavg.slow <- do.call(maType[[2]][[1]], c(list(x), maType[[2]][-1]))
    }
    else {
        mavg.fast <- do.call(maType, c(list(x), list(n = nFast, 
            ...)))
        mavg.slow <- do.call(maType, c(list(x), list(n = nSlow, 
            ...)))
    }
    if (percent) {
        macd <- 100 * (mavg.fast/mavg.slow - 1)
    }
    else {
        macd <- mavg.fast - mavg.slow
    }
    if (is.list(maType)) {
        signal <- do.call(maType[[3]][[1]], c(list(macd), maType[[3]][-1]))
    }
    else signal <- do.call(maType, c(list(macd), list(n = nSig, 
        ...)))
    result <- cbind(macd, signal)
    colnames(result) <- c("macd", "signal")
    return(result)
}


HMA <- function (x, n = 20, ...) 
{
    reclass(WMA(2 * WMA(x, n = n/2, ...) - WMA(x, n = n, ...), 
        n = trunc(sqrt(n)), ...), x)
}


naCheck <- xts::naCheck # re-exported from xts package

DonchianChannel <- function (HL, n = 10, include.lag = FALSE) 
{
    HL <- try.xts(HL, error = as.matrix)
    if (!(NCOL(HL) %in% c(1, 2))) {
        stop("Price series must be either High-Low, or Close/univariate.")
    }
    if (NCOL(HL) == 2) {
        hi <- HL[, 1]
        lo <- HL[, 2]
    }
    else {
        hi <- HL
        lo <- HL
    }
    high <- runMax(hi, n)
    low <- runMin(lo, n)
    mid <- (high + low)/2
    result <- cbind(high, mid, low)
    colnames(result) <- c("high", "mid", "low")
    if (include.lag) {
        result <- lag.xts(result)
    }
    reclass(result, HL)
}


DPO <- function (x, n = 10, maType, shift = n/2 + 1, percent = FALSE, 
    ...) 
{
    x <- try.xts(x, error = as.matrix)
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "SMA"
    }
    mavg <- do.call(maType, c(list(x), maArgs))
    mavg <- lag.xts(mavg, -shift)
    if (percent) {
        DPO <- 100 * (x[, 1]/mavg - 1)
    }
    else {
        DPO <- x[, 1] - mavg
    }
    reclass(DPO, x)
}


wilderSum <- function (x, n = 10) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. wilderSum only supports univariate 'x'")
    }
    x.na <- naCheck(x, n)
    result <- .Call("wilderSum", x, n, PACKAGE = "TTR")
    reclass(result, x)
}


runVar <- function (x, y = NULL, n = 10, sample = TRUE, cumulative = FALSE) 
{
    if (is.null(y)) 
        y <- x
    result <- runCov(x, y, n, use = "all.obs", sample = sample, 
        cumulative)
    return(result)
}


ZigZag <- function (HL, change = 10, percent = TRUE, retrace = FALSE, lastExtreme = TRUE) 
{
    HL <- try.xts(HL, error = as.matrix)
    HL.na <- naCheck(HL, 0)
    if (NCOL(HL) == 2) {
        high <- HL[HL.na$nonNA, 1]
        low <- HL[HL.na$nonNA, 2]
    }
    else if (NCOL(HL.na) == 1) {
        high <- HL[HL.na$nonNA]
        low <- HL[HL.na$nonNA]
    }
    else stop("Price series must be either High-Low, or Univariate")
    nn <- NROW(HL.na$nonNA)
    zz <- rep(0, nn)
    if (percent) {
        change <- change/100
    }
    zz <- .Fortran("zigzag", iha = as.double(high), ila = as.double(low), 
        la = as.integer(nn), ch = as.double(change), pct = as.integer(percent), 
        rtr = as.integer(retrace), lex = as.integer(lastExtreme), 
        zz = as.double(zz), PACKAGE = "TTR", DUP = TRUE)$zz
    zz <- ifelse(zz == 0, NA, zz)
    zz <- approx(zz, xout = 1:nn)$y
    zz <- c(rep(NA, HL.na$NAs), zz)
    reclass(zz, HL)
}


VWAP <- function (price, volume, n = 10, ...) 
{
    res <- WMA(price, n = n, volume)
    if (!is.null(dim(res))) {
        colnames(res) <- "VWAP"
    }
    return(res)
}


runMedian <- function (x, n = 10, non.unique = "mean", cumulative = FALSE) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    NAs <- sum(is.na(x))
    if (NAs > 0) {
        if (any(is.na(x[-(1:NAs)]))) 
            stop("Series contains non-leading NAs")
        if (NAs + n > NROW(x)) 
            stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    len <- NROW(x) - NAs
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. runMedian only supports univariate 'x'")
    }
    non.unique <- match.arg(non.unique, c("mean", "max", "min"))
    non.unique <- switch(non.unique, mean = 0, max = 1, min = -1)
    result <- .Fortran("runmedian", ia = as.double(x[beg:NROW(x)]), 
        n = as.integer(n), oa = double(len), la = as.integer(len), 
        ver = as.integer(non.unique), cu = as.integer(cumulative), 
        PACKAGE = "TTR", DUP = TRUE)$oa
    is.na(result) <- c(1:(n - 1))
    result <- c(rep(NA, NAs), result)
    reclass(result, x)
}


runSum <- function (x, n = 10, cumulative = FALSE) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. runSum only supports univariate 'x'")
    }
    NAs <- sum(is.na(x))
    if (NAs > 0) {
        if (any(is.na(x[-(1:NAs)]))) 
            stop("Series contains non-leading NAs")
        if (NAs + n > NROW(x)) 
            stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    len <- NROW(x) - NAs
    result <- double(NROW(x))
    if (cumulative) {
        result[beg:NROW(x)] <- cumsum(x[beg:NROW(x)])
    }
    else {
        result[(n + beg - 1)] <- sum(x[beg:(n + beg - 1)])
        result <- .Fortran("runsum", ia = as.double(x[beg:NROW(x)]), 
            lia = as.integer(len), n = as.integer(n), oa = as.double(result[beg:NROW(x)]), 
            loa = as.integer(len), PACKAGE = "TTR", DUP = TRUE)$oa
        result <- c(rep(NA, NAs), result)
    }
    is.na(result) <- c(1:(n - 1 + NAs))
    reclass(result, x)
}


VWMA <- function (price, volume, n = 10, ...) 
{
    res <- WMA(price, n = n, volume)
    if (!is.null(dim(res))) {
        colnames(res) <- "VWAP"
    }
    return(res)
}


lags <- function (x, n = 1) 
{
    x <- as.matrix(x)
    if (is.null(colnames(x))) 
        colnames(x) <- paste("V", 1:NCOL(x), sep = "")
    out <- embed(x, n + 1)
    if (n == 1) 
        lag.names <- 1
    else if (NCOL(x) == 1) 
        lag.names <- 1:n
    else lag.names <- rep(1:n, NCOL(x))
    colnames(out) <- c(colnames(x), paste(colnames(x), sort(lag.names), 
        sep = "."))
    return(out)
}


runMean <- function (x, n = 10, cumulative = FALSE) 
{
    if (cumulative) {
        result <- runSum(x, n, cumulative)/1:NROW(x)
    }
    else {
        result <- runSum(x, n)/n
    }
    return(result)
}


SAR <- function (HL, accel = c(0.02, 0.2)) 
{
    HL <- try.xts(HL, error = as.matrix)
    HL.na <- naCheck(HL, 0)
    initGap <- sd(drop(coredata(HL[, 1] - HL[, 2])), na.rm = TRUE)
    sar <- .Call("sar", HL[, 1], HL[, 2], accel, initGap, PACKAGE = "TTR")
    reclass(sar, HL)
}


EVWMA <- function (price, volume, n = 10, ...) 
{
    price <- try.xts(price, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)
    if (!any(NROW(volume) == c(NROW(price), 1))) 
        stop("Length of 'volume' must equal 1 or the length of 'price'")
    if (n < 1 || n > NROW(price)) 
        stop("Invalid 'n'")
    if (NCOL(price) > 1 || NCOL(volume) > 1) {
        stop("ncol(price) > 1 or ncol(volume) > 1.", " EVWMA only supports univariate 'price' and 'volume'")
    }
    pv <- cbind(price, volume)
    if (any(nNonNA <- n > colSums(!is.na(pv)))) 
        stop("n > number of non-NA values in ", paste(c("price", 
            "volume")[which(nNonNA)], collapse = ", "))
    pv.na <- naCheck(pv, n)
    ma <- .Call("evwma", pv[, 1], pv[, 2], n, PACKAGE = "TTR")
    if (!is.null(dim(ma))) {
        colnames(ma) <- "EVWMA"
    }
    reclass(ma, price)
}


growth <- function (price, signals, ...) 
{
    if (missing(signals)) {
        signals <- rep(1, NROW(price))
    }
    else {
        signals <- as.vector(signals)
    }
    price <- as.vector(price)
    growth <- cumprod(1 + ROC(price, ...) * signals)
    return(growth)
}


WMA <- function (x, n = 10, wts = 1:n, ...) 
{
    x <- try.xts(x, error = as.matrix)
    wts <- try.xts(wts, error = as.matrix)
    if (!any(NROW(wts) == c(NROW(x), n))) 
        stop("Length of 'wts' must equal the length of 'x' or 'n'")
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    if (NCOL(x) > 1 || NCOL(wts) > 1) {
        stop("ncol(x) > 1 or ncol(wts) > 1. WMA only supports univariate 'x' and 'w'")
    }
    NAx <- sum(is.na(x))
    NAw <- sum(is.na(wts))
    NAs <- max(NAx, NAw)
    if (NAs > 0) {
        if (any(is.na(x[-(1:NAx)]))) 
            stop("'x' contains non-leading NAs")
        if (any(is.na(wts[-(1:NAw)]))) 
            stop("'wts' contains non-leading NAs")
    }
    if (NROW(wts) == n) {
        x <- na.omit(x)
        NAs <- NAx
        if (any(is.na(wts))) 
            stop("'wts' vector of length 'n' cannot have NA values")
        ma <- .Fortran("wma", ia = as.double(x), lia = as.integer(NROW(x)), 
            wts = as.double(wts), n = as.integer(n), oa = as.double(x), 
            loa = as.integer(NROW(x)), PACKAGE = "TTR", DUP = TRUE)$oa
    }
    else {
        xw <- na.omit(cbind(x, wts))
        ma <- runSum(xw[, 1] * xw[, 2], n)/runSum(xw[, 2], n)
    }
    ma[1:(n - 1)] <- NA
    ma <- c(rep(NA, NAs), ma)
    if (!is.null(dim(ma))) {
        colnames(ma) <- "WMA"
    }
    reclass(ma, x)
}


SMA <- function (x, n = 10, ...) 
{
    ma <- runMean(x, n)
    if (!is.null(dim(ma))) {
        colnames(ma) <- "SMA"
    }
    return(ma)
}


TRIX <- function (price, n = 20, nSig = 9, maType, percent = TRUE, ...) 
{
    if (missing(maType)) {
        maType <- "EMA"
    }
    if (is.list(maType)) {
        maTypeInfo <- sapply(maType, is.list)
        if (!(all(maTypeInfo) && length(maTypeInfo) == 4)) {
            stop("If 'maType' is a list, you must specify\n ", 
                "*four* MAs (see Examples section of ?TRIX)")
        }
        if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
            maType[[1]]$n <- n
        }
        if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
            maType[[2]]$n <- n
        }
        if (!is.null(formals(maType[[3]][[1]])$n) && is.null(maType[[3]]$n)) {
            maType[[3]]$n <- n
        }
        if (!is.null(formals(maType[[4]][[1]])$n) && is.null(maType[[4]]$n)) {
            maType[[4]]$n <- nSig
        }
        mavg1 <- do.call(maType[[1]][[1]], c(list(price), maType[[1]][-1]))
        mavg2 <- do.call(maType[[2]][[1]], c(list(mavg1), maType[[2]][-1]))
        mavg3 <- do.call(maType[[3]][[1]], c(list(mavg2), maType[[3]][-1]))
    }
    else {
        mavg1 <- do.call(maType, c(list(price), list(n = n, ...)))
        mavg2 <- do.call(maType, c(list(mavg1), list(n = n, ...)))
        mavg3 <- do.call(maType, c(list(mavg2), list(n = n, ...)))
    }
    if (percent) {
        TRIX <- 100 * ROC(mavg3, n = 1, na.pad = TRUE, type = "discrete")
    }
    else {
        TRIX <- momentum(mavg3, n = 1, na.pad = TRUE)
    }
    if (is.list(maType)) {
        signal <- do.call(maType[[4]][[1]], c(list(TRIX), maType[[4]][-1]))
    }
    else {
        signal <- do.call(maType, c(list(TRIX), list(n = n, ...)))
    }
    result <- cbind(TRIX, signal)
    colnames(result) <- c("TRIX", "signal")
    return(result)
}


ADX <- function (HLC, n = 14, maType, ...) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    dH <- momentum(HLC[, 1])
    dL <- -momentum(HLC[, 2])
    DMIp <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH > 
        dL, dH, 0))
    DMIn <- ifelse(dH == dL | (dH < 0 & dL < 0), 0, ifelse(dH < 
        dL, dL, 0))
    TR <- ATR(HLC)[, "tr"]
    TRsum <- wilderSum(TR, n = n)
    DIp <- 100 * wilderSum(DMIp, n = n)/TRsum
    DIn <- 100 * wilderSum(DMIn, n = n)/TRsum
    DX <- 100 * (abs(DIp - DIn)/(DIp + DIn))
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "EMA"
        maArgs$wilder <- TRUE
    }
    ADX <- do.call(maType, c(list(DX), maArgs))
    result <- cbind(DIp, DIn, DX, ADX)
    colnames(result) <- c("DIp", "DIn", "DX", "ADX")
    reclass(result, HLC)
}


adjRatios <- function (splits, dividends, close) 
{
    if (!missing(dividends) && missing(close)) 
        stop("\"close\" must be specified to adjust dividends")
    if (missing(close) || all(is.na(close)) || NROW(close) == 
        0) {
        close <- NA
    }
    else {
        if (NCOL(close) != 1) 
            stop("\"close\" must be univariate")
        close <- try.xts(close, error = stop("\"as.xts(close)\" failed"))
    }
    if (missing(splits) || all(is.na(splits)) || NROW(splits) == 
        0) {
        splits <- NA
    }
    else {
        if (NCOL(splits) != 1) 
            stop("\"splits\" must be univariate")
        splits <- try.xts(splits, error = stop("\"as.xts(splits)\" failed"))
    }
    if (missing(dividends) || all(is.na(dividends)) || NROW(dividends) == 
        0) {
        dividends <- NA
    }
    else {
        if (NCOL(dividends) != 1) 
            stop("\"dividends\" must be univariate")
        dividends <- try.xts(dividends, error = stop("\"as.xts(dividends)\" failed"))
    }
    obj <- merge.xts(close, splits, dividends)
    if (!isTRUE(is.na(close))) {
        obj <- obj[!is.na(obj[, 1]), ]
    }
    adj <- .Call("adjRatios", obj[, 2], obj[, 3], obj[, 1], PACKAGE = "TTR")
    adj <- xts(cbind(adj[[1]], adj[[2]]), index(obj))
    colnames(adj) <- c("Split", "Div")
    return(adj)
}


KST <- function (price, n = c(10, 10, 10, 15), nROC = c(10, 15, 20, 
    30), nSig = 9, maType, wts = 1:NROW(n), ...) 
{
    if (!all.equal(NROW(n), NROW(wts), NROW(nROC))) {
        stop("'n', 'nROC', and 'wts' must be the same length.")
    }
    else {
        N <- NROW(n)
    }
    ret <- NULL
    if (missing(maType)) {
        maType <- "SMA"
    }
    if (is.list(maType)) {
        maTypeInfo <- sapply(maType, is.list)
        if (!(all(maTypeInfo) && length(maTypeInfo) == N)) {
            stop("If 'maType' is a list, you must specify\n ", 
                "the same number of MAs as elements in 'n' and\n ", 
                "'nROC' (see Examples section of ?KST)")
        }
        for (i in 1:length(maType)) {
            if (!is.null(formals(maType[[i]][[1]])$n) && is.null(maType[[i]]$n)) {
                maType[[i]]$n <- n[i]
            }
            roc <- ROC(price, nROC[i], na.pad = TRUE)
            ma.roc <- do.call(maType[[i]][[1]], c(list(roc), 
                maType[[i]][-1])) * wts[i]
            ret <- cbind(ret, ma.roc)
        }
    }
    else {
        for (i in 1:NROW(n)) {
            roc <- ROC(price, nROC[i], na.pad = TRUE)
            ma.roc <- do.call(maType, c(list(roc), list(n = n[i], 
                ...))) * wts[i]
            ret <- cbind(ret, ma.roc)
        }
    }
    if (is.xts(ret)) {
        kst <- xts(100 * rowSums(ret), index(ret))
    }
    else {
        kst <- 100 * rowSums(ret)
    }
    if (is.list(maType)) {
        sigMA <- length(maType)
        signal <- do.call(maType[[sigMA]][[1]], c(list(kst), 
            maType[[sigMA]][-1]))
    }
    else {
        signal <- do.call(maType, c(list(kst), list(n = nSig, 
            ...)))
    }
    result <- cbind(kst, signal)
    colnames(result) <- c("kst", "signal")
    return(result)
}


GMMA <- function (x, short = c(3, 5, 8, 10, 12, 15), long = c(30, 35, 
    40, 45, 50, 60), maType) 
{
    x <- try.xts(x, error = as.matrix)
    if (missing(maType)) {
        maType <- "EMA"
    }
    fn <- function(g) {
        do.call(maType, list(x, n = g))
    }
    gmma <- do.call(cbind, lapply(c(short, long), fn))
    colnames(gmma) <- c(paste("short lag", short), paste("long lag", 
        long))
    reclass(gmma, x)
}


stoch <- function (HLC, nFastK = 14, nFastD = 3, nSlowD = 3, maType, bounded = TRUE, 
    smooth = 1, ...) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    if (NCOL(HLC) == 3) {
        high <- HLC[, 1]
        low <- HLC[, 2]
        close <- HLC[, 3]
    }
    else if (NCOL(HLC) == 1) {
        high <- HLC
        low <- HLC
        close <- HLC
    }
    else stop("Price series must be either High-Low-Close, or Close")
    if (bounded) {
        hmax <- runMax(high, nFastK)
        lmin <- runMin(low, nFastK)
    }
    else {
        hmax <- runMax(c(high[1], high[-NROW(HLC)]), nFastK)
        lmin <- runMax(c(low[1], low[-NROW(HLC)]), nFastK)
    }
    num <- close - lmin
    den <- hmax - lmin
    if (missing(maType)) {
        maType <- "SMA"
    }
    if (is.list(maType)) {
        maTypeInfo <- sapply(maType, is.list)
        if (!(all(maTypeInfo) && length(maTypeInfo) == 3)) {
            stop("If 'maType' is a list, you must specify\n ", 
                "*three* MAs (see Examples section of ?stochastics)")
        }
        if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
            maType[[1]]$n <- nFastD
        }
        if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
            maType[[2]]$n <- nSlowD
        }
        if (!is.null(formals(maType[[3]][[1]])$n) && is.null(maType[[3]]$n)) {
            maType[[3]]$n <- smooth
        }
        numMA <- do.call(maType[[3]][[1]], c(list(num), maType[[3]][-1]))
        denMA <- do.call(maType[[3]][[1]], c(list(den), maType[[3]][-1]))
        fastK <- numMA/denMA
        fastK[is.nan(fastK)] <- 0.5
        fastD <- do.call(maType[[1]][[1]], c(list(fastK), maType[[1]][-1]))
        slowD <- do.call(maType[[2]][[1]], c(list(fastD), maType[[2]][-1]))
    }
    else {
        numMA <- do.call(maType, c(list(num), list(n = smooth)))
        denMA <- do.call(maType, c(list(den), list(n = smooth)))
        fastK <- numMA/denMA
        fastK[is.nan(fastK)] <- 0.5
        fastD <- do.call(maType, c(list(fastK), list(n = nFastD, 
            ...)))
        slowD <- do.call(maType, c(list(fastD), list(n = nSlowD, 
            ...)))
    }
    result <- cbind(fastK, fastD, slowD)
    colnames(result) <- c("fastK", "fastD", "slowD")
    reclass(result, HLC)
}


momentum <- function (x, n = 1, na.pad = TRUE) 
{
    x <- try.xts(x, error = as.matrix)
    if (is.xts(x)) {
        mom <- diff(x, n, na.pad = na.pad)
    }
    else {
        NAs <- NULL
        if (na.pad) {
            NAs <- rep(NA, n)
        }
        mom <- c(NAs, diff(x, n))
    }
    reclass(mom, x)
}


WPR <- function (HLC, n = 14) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    if (NCOL(HLC) == 3) {
        high <- HLC[, 1]
        low <- HLC[, 2]
        close <- HLC[, 3]
    }
    else if (NCOL(HLC) == 1) {
        high <- HLC
        low <- HLC
        close <- HLC
    }
    else stop("Price series must be either High-Low-Close, or Close")
    hmax <- runMax(high, n)
    lmin <- runMin(low, n)
    pctR <- (hmax - close)/(hmax - lmin)
    pctR[is.nan(pctR)] <- 0.5
    reclass(pctR, HLC)
}


TDI <- function (price, n = 20, multiple = 2) 
{
    price <- try.xts(price, error = as.matrix)
    mom <- momentum(price, n, na.pad = TRUE)
    mom[is.na(mom)] <- 0
    di <- runSum(mom, n)
    abs.di <- abs(di)
    abs.mom.2n <- runSum(abs(mom), n * multiple)
    abs.mom.1n <- runSum(abs(mom), n)
    tdi <- abs.di - (abs.mom.2n - abs.mom.1n)
    result <- cbind(tdi, di)
    colnames(result) <- c("tdi", "di")
    reclass(result, price)
}


ROC <- function (x, n = 1, type = c("continuous", "discrete"), na.pad = TRUE) 
{
    x <- try.xts(x, error = as.matrix)
    type <- match.arg(type)
    if (is.xts(x)) {
        if (type == "discrete") {
            roc <- x/lag.xts(x, n, na.pad = na.pad) - 1
        }
        if (type == "continuous") {
            roc <- diff(log(x), n, na.pad = na.pad)
        }
        reclass(roc, x)
    }
    else {
        NAs <- NULL
        if (na.pad) {
            NAs <- rep(NA, n)
        }
        if (type == "discrete") {
            roc <- c(NAs, x[(n + 1):NROW(x)]/x[1:(NROW(x) - n)] - 
                1)
        }
        if (type == "continuous") {
            roc <- c(NAs, diff(log(x), n))
        }
        return(roc)
    }
}


EMA <- function (x, n = 10, wilder = FALSE, ratio = NULL, ...) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. EMA only supports univariate 'x'")
    }
    if (any(nNonNA <- n > colSums(!is.na(x)))) 
        stop("n > number of non-NA values in column(s) ", paste(which(nNonNA), 
            collapse = ", "))
    x.na <- naCheck(x, n)
    if (missing(n) && !missing(ratio)) 
        n <- trunc(2/ratio - 1)
    if (is.null(ratio)) {
        if (wilder) 
            ratio <- 1/n
        else ratio <- 2/(n + 1)
    }
    ma <- .Call("ema", x, n, ratio, PACKAGE = "TTR")
    ma <- reclass(ma, x)
    if (!is.null(dim(ma))) {
        colnames(ma) <- "EMA"
    }
    return(ma)
}


SMI <- function (HLC, n = 13, nFast = 2, nSlow = 25, nSig = 9, maType, 
    bounded = TRUE, ...) 
{
    if (ncol(HLC) == 3) {
        high <- HLC[, 1]
        low <- HLC[, 2]
        close <- HLC[, 3]
    }
    else if (ncol(HLC) == 1) {
        high <- HLC
        low <- HLC
        close <- HLC
    }
    else stop("Price series must be either High-Low-Close, or Close")
    if (bounded) {
        hmax <- runMax(high, n)
        lmin <- runMin(low, n)
    }
    else {
        hmax <- runMax(c(high[1], high[-NROW(HLC)]), n)
        lmin <- runMax(c(low[1], low[-NROW(HLC)]), n)
    }
    hmax <- ifelse(is.na(hmax), high, hmax)
    lmin <- ifelse(is.na(lmin), low, lmin)
    HLdiff <- hmax - lmin
    Cdiff <- close - (hmax + lmin)/2
    if (missing(maType)) {
        maType <- "EMA"
    }
    if (is.list(maType)) {
        maTypeInfo <- sapply(maType, is.list)
        if (!(all(maTypeInfo) && length(maTypeInfo) == 3)) {
            stop("If 'maType' is a list, you must specify\n ", 
                "*three* MAs (see Examples section of ?SMI)")
        }
        if (!is.null(formals(maType[[1]][[1]])$n) && is.null(maType[[1]]$n)) {
            maType[[1]]$n <- nFast
        }
        if (!is.null(formals(maType[[2]][[1]])$n) && is.null(maType[[2]]$n)) {
            maType[[2]]$n <- nSlow
        }
        if (!is.null(formals(maType[[3]][[1]])$n) && is.null(maType[[3]]$n)) {
            maType[[3]]$n <- nSig
        }
        num1 <- do.call(maType[[1]][[1]], c(list(Cdiff), maType[[1]][-1]))
        den1 <- do.call(maType[[1]][[1]], c(list(HLdiff), maType[[1]][-1]))
        num2 <- do.call(maType[[2]][[1]], c(list(num1), maType[[2]][-1]))
        den2 <- do.call(maType[[2]][[1]], c(list(den1), maType[[2]][-1]))
        SMI <- 100 * (num2/(den2/2))
        signal <- do.call(maType[[3]][[1]], c(list(SMI), maType[[3]][-1]))
    }
    else {
        num1 <- do.call(maType, c(list(Cdiff), list(n = nSlow, 
            ...)))
        den1 <- do.call(maType, c(list(HLdiff), list(n = nSlow, 
            ...)))
        num2 <- do.call(maType, c(list(num1), list(n = nFast, 
            ...)))
        den2 <- do.call(maType, c(list(den1), list(n = nFast, 
            ...)))
        SMI <- 100 * (num2/(den2/2))
        signal <- do.call(maType, c(list(SMI), list(n = nSig, 
            ...)))
    }
    result <- cbind(SMI, signal)
    colnames(result) <- c("SMI", "signal")
    reclass(result, HLC)
}


VHF <- function (price, n = 28) 
{
    price <- try.xts(price, error = as.matrix)
    if (NCOL(price) == 1) {
        high <- price
        low <- price
        close <- price
    }
    else if (NCOL(price) == 3) {
        high <- price[, 1]
        low <- price[, 2]
        close <- price[, 3]
    }
    else stop("Price series must be either Close, or High-Low-Close")
    hmax <- runMax(high, n)
    lmin <- runMin(low, n)
    denom <- abs(momentum(close, n = 1, na.pad = TRUE))
    VHF <- (hmax - lmin)/runSum(denom, n)
    reclass(VHF, price)
}


getYahooData <- function (symbol, start, end, freq = "daily", type = "price", 
    adjust = TRUE, quiet = FALSE) 
{
    if (missing(start)) {
        beg <- as.POSIXlt("1900-01-01")
    }
    else {
        beg <- as.POSIXlt(as.Date(as.character(start), "%Y%m%d"))
    }
    if (missing(end)) {
        end <- as.POSIXlt(Sys.Date())
    }
    else {
        end <- as.POSIXlt(as.Date(as.character(end), "%Y%m%d"))
    }
    if (beg > end) 
        stop("Start date must be before end date.")
    if (beg > as.POSIXlt(Sys.Date())) 
        stop("Start date is after today's date.")
    freq <- match.arg(freq, c("daily", "weekly", "monthly"))
    type <- match.arg(type, c("price", "split"))
    if (type == "price") {
        freq.url <- substr(freq, 1, 1)
    }
    else {
        freq.url <- "v"
        if (freq != "daily" & !quiet) 
            message("Only freq=\"daily\" data available for type=\"split\".\n", 
                "Setting freq=\"daily\"...")
    }
    flush.console()
    if (type == "price") {
        if (adjust) {
            if (freq == "daily") {
                ohlc <- getYahooData(symbol, start, freq = "daily", 
                  type = "price", adjust = FALSE, quiet = TRUE)
                divspl <- getYahooData(symbol, start, freq = "daily", 
                  type = "split", adjust = FALSE, quiet = TRUE)
                ohlc <- merge(ohlc, divspl, all = TRUE)
                if (NROW(divspl) != 0) {
                  adj <- adjRatios(ohlc[, "Split"], ohlc[, "Div"], 
                    ohlc[, "Close"])
                  s.ratio <- adj[, 1]
                  d.ratio <- adj[, 2]
                  cn <- colnames(ohlc)
                  ohlc <- cbind(ohlc, ohlc[, "Close"])
                  colnames(ohlc) <- c(cn, "Unadj.Close")
                  ohlc[, "Open"] <- ohlc[, "Open"] * d.ratio * 
                    s.ratio
                  ohlc[, "High"] <- ohlc[, "High"] * d.ratio * 
                    s.ratio
                  ohlc[, "Low"] <- ohlc[, "Low"] * d.ratio * 
                    s.ratio
                  ohlc[, "Close"] <- ohlc[, "Close"] * d.ratio * 
                    s.ratio
                  ohlc[, "Volume"] <- ohlc[, "Volume"] * (1/d.ratio)
                  ohlc <- ohlc[, c("Open", "High", "Low", "Close", 
                    "Volume", "Unadj.Close", "Div", "Split", 
                    "Adj.Div")]
                }
            }
            else stop("Only freq=\"daily\" adjusted data is currently supported.")
        }
        else {
            url <- paste("http://ichart.finance.yahoo.com/table.csv?s=", 
                symbol, "&a=", beg$mon, "&b=", beg$mday, "&c=", 
                beg$year + 1900, "&d=", end$mon, "&e=", end$mday, 
                "&f=", end$year + 1900, "&g=", freq.url, "&ignore=.csv", 
                sep = "")
            ohlc <- read.table(url, header = TRUE, sep = ",")
            ohlc[, "Adj.Close"] <- NULL
            ohlc <- ohlc[order(ohlc[, "Date"]), ]
            ohlc <- xts(ohlc[, -1], as.Date(as.character(ohlc[, 
                1])))
        }
    }
    else {
        if (!quiet) 
            message("Unadjusted and adjusted dividend data are always returned.")
        url <- paste("http://ichart.finance.yahoo.com/x?s=", 
            symbol, "&a=", beg$mon, "&b=", beg$mday, "&c=", beg$year + 
                1900, "&d=", end$mon, "&e=", end$mday, "&f=", 
            end$year + 1900, "&g=", freq.url, "&y=0&z=30000", 
            sep = "")
        ohlc <- read.table(url, skip = 1, sep = ",", fill = TRUE, 
            as.is = TRUE)
        div <- data.frame(Date = ohlc[ohlc[, "V1"] == "DIVIDEND", 
            "V2"], Adj.Div = as.numeric(ohlc[ohlc[, "V1"] == 
            "DIVIDEND", "V3"]), stringsAsFactors = FALSE)
        spl <- data.frame(Date = ohlc[ohlc[, "V1"] == "SPLIT", 
            "V2"], Split = as.character(ohlc[ohlc[, "V1"] == 
            "SPLIT", "V3"]), stringsAsFactors = FALSE)
        ohlc <- merge(div, spl, by.col = "Date", all = TRUE)
        if (NROW(ohlc) == 0) 
            return(ohlc)
        ohlc[, "Date"] <- as.Date(as.character(ohlc[, "Date"]), 
            "%Y%m%d")
        ohlc[, "Split"] <- sub(":", "/", ohlc[, "Split"])
        ohlc[, "Split"] <- 1/sapply(parse(text = ohlc[, "Split"]), 
            eval)
        ohlc <- ohlc[order(ohlc[, 1]), ]
        ohlc <- xts(ohlc[, -1], as.Date(as.character(ohlc[, 1])))
        if (all(is.na(ohlc[, "Split"]))) {
            s.ratio <- rep(1, NROW(ohlc))
        }
        else {
            s.ratio <- adjRatios(splits = ohlc[, "Split"])[, 
                1]
        }
        ohlc <- cbind(ohlc, ohlc[, "Adj.Div"] * (1/s.ratio))
        colnames(ohlc)[3] <- "Div"
        ohlc[, "Split"] <- as.numeric(ohlc[, "Split"])
        ohlc <- ohlc[, c("Div", "Split", "Adj.Div")]
    }
    ohlc <- ohlc[paste(beg, end, sep = "/"), ]
    return(ohlc)
}


MFI <- function (HLC, volume, n = 14) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)
    if (!(is.xts(HLC) && is.xts(volume))) {
        HLC <- as.matrix(HLC)
        volume <- as.matrix(volume)
    }
    if (NCOL(HLC) == 3) {
        if (is.xts(HLC)) {
            HLC <- xts(apply(HLC, 1, mean), index(HLC))
        }
        else {
            HLC <- apply(HLC, 1, mean)
        }
    }
    else if (NCOL(HLC) != 1) {
        stop("Price series must be either High-Low-Close, or Close/univariate.")
    }
    if (is.xts(HLC)) {
        priceLag <- lag.xts(HLC)
    }
    else {
        priceLag <- c(NA, HLC[-NROW(HLC)])
    }
    mf <- HLC * volume
    pmf <- ifelse(HLC > priceLag, mf, 0)
    nmf <- ifelse(HLC < priceLag, mf, 0)
    mr <- runSum(pmf, n)/runSum(nmf, n)
    mfi <- 100 - (100/(1 + mr))
    if (is.xts(mfi)) 
        colnames(mfi) <- "mfi"
    reclass(mfi, HLC)
}


CMF <- function (HLC, volume, n = 20) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)
    if (!(is.xts(HLC) && is.xts(volume))) {
        clv <- CLV(as.matrix(HLC))
        volume <- as.matrix(volume)
    }
    clv <- CLV(HLC)
    cmf <- runSum(clv * volume, n)/runSum(volume, n)
    reclass(cmf, HLC)
}


CLV <- function (HLC) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    clv <- ((HLC[, 3] - HLC[, 2]) - (HLC[, 1] - HLC[, 3]))/(HLC[, 
        1] - HLC[, 2])
    clv[is.nan(clv) | is.infinite(clv)] <- 0
    if (is.xts(clv)) 
        colnames(clv) <- "clv"
    reclass(clv, HLC)
}


aroon <- function (HL, n = 20) 
{
    HL <- try.xts(HL, error = as.matrix)
    if (NCOL(HL) == 1) {
        high <- HL
        low <- HL
    }
    else if (NCOL(HL) == 2) {
        high <- HL[, 1]
        low <- HL[, 2]
    }
    else stop("Price series must be either High-Low, or Close")
    aroonUp <- .Call("aroon_max", high, n, PACKAGE = "TTR")
    aroonDn <- .Call("aroon_max", -low, n, PACKAGE = "TTR")
    oscillator <- aroonUp - aroonDn
    result <- cbind(aroonUp, aroonDn, oscillator)
    colnames(result) <- c("aroonUp", "aroonDn", "oscillator")
    reclass(result, HL)
}


DVI <- function (price, n = 252, wts = c(0.8, 0.2), smooth = 3, magnitude = c(5, 
    100, 5), stretch = c(10, 100, 2), exact.multiplier = 1) 
{
    price <- try.xts(price, error = as.matrix)
    wts.sum <- sum(wts)
    wts[1] <- wts[1]/wts.sum
    wts[2] <- wts[2]/wts.sum
    r <- price/SMA(price, smooth) - 1
    mag <- SMA((SMA(r, magnitude[1]) + SMA(r, magnitude[2])/10)/2, 
        magnitude[3])
    b <- ifelse(price > lag.xts(price), 1, -1)
    str <- SMA((runSum(b, stretch[1]) + runSum(b, stretch[2])/10)/2, 
        stretch[3])
    dvi.mag <- runPercentRank(mag, n, FALSE, exact.multiplier)
    dvi.str <- runPercentRank(str, n, FALSE, exact.multiplier)
    dvi <- wts[1] * dvi.mag + wts[2] * dvi.str
    reclass(cbind(dvi.mag, dvi.str, dvi), price)
}


williamsAD <- function (HLC) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    dCl <- momentum(HLC[, 3], 1)
    atr <- ATR(HLC)
    ad <- HLC[, 3] - ifelse(dCl > 0, atr[, "trueLow"], atr[, 
        "trueHigh"])
    ad[dCl == 0] <- 0
    ad.na <- naCheck(ad)
    ad <- cumsum(ad[ad.na$nonNA])
    ad <- c(rep(NA, ad.na$NAs), ad)
    reclass(ad, HLC)
}


OBV <- function (price, volume) 
{
    price <- try.xts(price, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)
    if (!(is.xts(price) && is.xts(volume))) {
        price <- as.vector(price)
        volume <- as.vector(volume)
    }
    prChg <- ROC(price)
    obv <- c(volume[1], ifelse(prChg > 0, volume, -volume)[-1])
    obv[abs(prChg) < sqrt(.Machine$double.eps)] <- 0
    obv <- cumsum(obv)
    if (is.xts(obv)) {
        obv <- xts(obv, index(price))
        colnames(obv) <- "obv"
    }
    reclass(obv, price)
}


stockSymbols <- function (exchange = c("AMEX", "NASDAQ", "NYSE"), sort.by = c("Exchange", 
    "Symbol"), quiet = FALSE) 
{
    symbols <- NULL
    symbols.colnames <- c("Symbol", "Name", "LastSale", "MarketCap", 
        "IPOyear", "Sector", "Industry", "Exchange")
    exchange <- match.arg(exchange, several.ok = TRUE)
    sort.by <- match.arg(sort.by, symbols.colnames, several.ok = TRUE)
    for (i in exchange) {
        if (!quiet) 
            message("Fetching ", i, " symbols...")
        flush.console()
        url <- paste("http://www.nasdaq.com/screening/companies-by-name.aspx", 
            "?letter=0&exchange=", i, "&render=download", sep = "")
        exch <- read.csv(url, header = TRUE, as.is = TRUE, na = "n/a")
        col.loc <- sapply(symbols.colnames, grep, names(exch), 
            ignore.case = TRUE)
        exch <- exch[, c(col.loc, recursive = TRUE)]
        exch <- data.frame(exch, Exchange = i, stringsAsFactors = FALSE)
        colnames(exch) <- symbols.colnames
        exch$Symbol <- gsub("[[:space:]]", "", exch$Symbol)
        if (i == "AMEX") {
            exch$Symbol <- gsub("/WS$", "-WT", exch$Symbol)
            exch$Symbol <- gsub("/WS/", "-WT", exch$Symbol)
            exch$Symbol <- gsub("/U", "-U", exch$Symbol)
            exch$Symbol <- gsub("\\^", "-P", exch$Symbol)
            exch$Symbol <- gsub("/", "-", exch$Symbol)
            drop <- c(grep("\\.", exch$Symbol), grep("\\$", exch$Symbol), 
                grep(":", exch$Symbol))
            if (NROW(drop) != 0) {
                exch <- exch[-drop, ]
            }
        }
        else if (i == "NYSE") {
            exch$Symbol <- gsub("/WS$", "-WT", exch$Symbol)
            exch$Symbol <- gsub("/WS/", "-WT", exch$Symbol)
            exch$Symbol <- gsub("\\^", "-P", exch$Symbol)
            exch$Symbol <- gsub("/", "-", exch$Symbol)
            drop <- c(grep("\\$", exch$Symbol), grep(":", exch$Symbol), 
                grep("~", exch$Symbol))
            if (NROW(drop) != 0) {
                exch <- exch[-drop, ]
            }
        }
        symbols <- rbind(symbols, exch)
    }
    symbols <- symbols[do.call("order", symbols[, sort.by]), 
        ]
    rownames(symbols) <- 1:NROW(symbols)
    return(symbols)
}


RSI <- function (price, n = 14, maType, ...) 
{
    price <- try.xts(price, error = as.matrix)
    up <- momentum(price, n = 1, na.pad = TRUE)
    which.dn <- which(up < 0)
    dn <- up * 0
    dn[which.dn] <- -up[which.dn]
    up[which.dn] <- 0
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "EMA"
        maArgs$wilder <- TRUE
    }
    if (is.list(maType)) {
        maTypeInfo <- sapply(maType, is.list)
        if (!(all(maTypeInfo) && length(maTypeInfo) == 2)) {
            stop("If 'maType' is a list, you must specify\n ", 
                "*two* MAs (see Examples section of ?RSI)")
        }
        for (i in 1:length(maType)) {
            if (!is.null(formals(maType[[i]])$n) && is.null(maType[[i]]$n)) {
                maType[[i]]$n <- n
            }
            mavgUp <- do.call(maType[[1]][[1]], c(list(up), maType[[1]][-1]))
            mavgDn <- do.call(maType[[2]][[1]], c(list(dn), maType[[2]][-1]))
        }
    }
    else {
        mavgUp <- do.call(maType, c(list(up), maArgs))
        mavgDn <- do.call(maType, c(list(dn), maArgs))
    }
    rsi <- 100 * mavgUp/(mavgUp + mavgDn)
    reclass(rsi, price)
}


volatility <- function (OHLC, n = 10, calc = "close", N = 260, mean0 = FALSE, 
    ...) 
{
    OHLC <- try.xts(OHLC, error = as.matrix)
    calc <- match.arg(calc, c("close", "garman.klass", "parkinson", 
        "rogers.satchell", "gk.yz", "yang.zhang"))
    if (calc == "close") {
        if (NCOL(OHLC) == 1) {
            r <- ROC(OHLC[, 1], 1, ...)
        }
        else {
            r <- ROC(OHLC[, 4], 1, ...)
        }
        if (isTRUE(mean0)) {
            s <- sqrt(N) * sqrt(runSum(r^2, n - 1)/(n - 2))
        }
        else {
            s <- sqrt(N) * runSD(r, n - 1)
        }
    }
    if (calc == "garman.klass") {
        s <- sqrt(N/n * runSum(0.5 * log(OHLC[, 2]/OHLC[, 3])^2 - 
            (2 * log(2) - 1) * log(OHLC[, 4]/OHLC[, 1])^2, n))
    }
    if (calc == "parkinson") {
        s <- sqrt(N/(4 * n * log(2)) * runSum(log(OHLC[, 2]/OHLC[, 
            3])^2, n))
    }
    if (calc == "rogers.satchell") {
        s <- sqrt(N/n * runSum(log(OHLC[, 2]/OHLC[, 4]) * log(OHLC[, 
            2]/OHLC[, 1]) + log(OHLC[, 3]/OHLC[, 4]) * log(OHLC[, 
            3]/OHLC[, 1]), n))
    }
    if (calc == "gk.yz") {
        if (is.xts(OHLC)) {
            Cl1 <- lag.xts(OHLC[, 4])
        }
        else {
            Cl1 <- c(NA, OHLC[-NROW(OHLC), 4])
        }
        s <- sqrt(N/n * runSum(log(OHLC[, 1]/Cl1)^2 + 0.5 * log(OHLC[, 
            2]/OHLC[, 3])^2 - (2 * log(2) - 1) * log(OHLC[, 4]/OHLC[, 
            1])^2, n))
    }
    if (calc == "yang.zhang") {
        if (is.xts(OHLC)) {
            Cl1 <- lag.xts(OHLC[, 4])
        }
        else {
            Cl1 <- c(NA, OHLC[-NROW(OHLC), 4])
        }
        dots <- list(...)
        if (is.null(dots$alpha)) {
            alpha <- 1.34
        }
        if (is.null(dots$k)) {
            k <- (alpha - 1)/(alpha + (n + 1)/(n - 1))
        }
        s2o <- N * runVar(log(OHLC[, 1]/Cl1), n = n)
        s2c <- N * runVar(log(OHLC[, 4]/OHLC[, 1]), n = n)
        s2rs <- volatility(OHLC = OHLC, n = n, calc = "rogers.satchell", 
            N = N, ...)
        s <- sqrt(s2o + k * s2c + (1 - k) * (s2rs^2))
    }
    reclass(s, OHLC)
}


ZLEMA <- function (x, n = 10, ratio = NULL, ...) 
{
    x <- try.xts(x, error = as.matrix)
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. ZLEMA only supports univariate 'x'")
    }
    NAs <- sum(is.na(x))
    if (NAs > 0) {
        if (any(is.na(x[-(1:NAs)]))) 
            stop("Series contains non-leading NAs")
    }
    x <- na.omit(x)
    ma <- rep(1, NROW(x))
    ma[n] <- mean(x[1:n])
    if (is.null(ratio)) {
        ratio <- 2/(n + 1)
    }
    ma <- .Fortran("zlema", ia = as.double(x), lia = as.integer(NROW(x)), 
        n = as.integer(n), oa = as.double(ma), loa = as.integer(NROW(ma)), 
        ratio = as.double(ratio), PACKAGE = "TTR", DUP = TRUE)$oa
    ma[1:(n - 1)] <- NA
    ma <- c(rep(NA, NAs), ma)
    if (!is.null(dim(ma))) {
        colnames(ma) <- "ZLEMA"
    }
    reclass(ma, x)
}


chaikinAD <- function (HLC, volume) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    volume <- try.xts(volume, error = as.matrix)
    if (!(is.xts(HLC) && is.xts(volume))) {
        HLC <- as.matrix(HLC)
        volume <- as.matrix(volume)
    }
    ad <- CLV(HLC) * volume
    ad.na <- naCheck(ad)
    ad <- cumsum(ad[ad.na$nonNA])
    ad <- c(rep(NA, ad.na$NAs), ad)
    reclass(ad, HLC)
}


runPercentRank <- function (x, n = 260, cumulative = FALSE, exact.multiplier = 0.5) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    if (0 > exact.multiplier || exact.multiplier > 1) 
        stop("Invalid 'exact.multiplier'")
    NAs <- sum(is.na(x))
    if (NAs > 0) {
        if (any(is.na(x[-(1:NAs)]))) 
            stop("Series contains non-leading NAs")
    }
    beg <- 1 + NAs
    len <- NROW(x) - NAs
    result <- double(NROW(x))
    if (cumulative) {
        result <- .Fortran("cumprnk", ia = as.double(x[beg:NROW(x)]), 
            lia = as.integer(len), xmlt = as.double(exact.multiplier), 
            oa = as.double(result[beg:NROW(x)]), PACKAGE = "TTR", 
            DUP = TRUE)$oa
    }
    else if (identical(as.integer(n), 1L)) {
        result[] <- exact.multiplier
    }
    else {
        result <- .Fortran("runprnk", ia = as.double(x[beg:NROW(x)]), 
            lia = as.integer(len), n = as.integer(n), xmlt = as.double(exact.multiplier), 
            oa = as.double(result[beg:NROW(x)]), PACKAGE = "TTR", 
            DUP = TRUE)$oa
        is.na(result) <- c(1:(n - 1))
    }
    result <- c(rep(NA, NAs), result)
    reclass(result, x)
}


runCor <- function (x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE) 
{
    result <- runCov(x, y, n, use = use, sample = sample, cumulative)/(runSD(x, 
        n, sample = sample, cumulative) * runSD(y, n, sample = sample, 
        cumulative))
    return(result)
}


CMO <- function (x, n = 14) 
{
    x <- try.xts(x, error = as.matrix)
    up <- momentum(x, n = 1)
    dn <- ifelse(up < 0, abs(up), 0)
    up <- ifelse(up > 0, up, 0)
    up <- runSum(up, n)
    dn <- runSum(dn, n)
    cmo <- 100 * (up - dn)/(up + dn)
    reclass(cmo, x)
}


chaikinVolatility <- function (HL, n = 10, maType, ...) 
{
    HL <- try.xts(HL, error = as.matrix)
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "EMA"
    }
    mavg <- do.call(maType, c(list(HL[, 1] - HL[, 2]), maArgs))
    volatility <- ROC(mavg, n, type = "discrete")
    reclass(volatility, HL)
}


PBands <- function (prices, n = 20, maType = "SMA", sd = 2, ..., fastn = 2, 
    centered = FALSE, lavg = FALSE) 
{
    if (!is.vector(prices) && ncol(prices) > 1) 
        stop("prices should be a univariate series, maybe use", 
            "lapply(prices,PBands) instead?")
    prices <- try.xts(prices, error = as.matrix)
    if (missing(maType)) {
        maType <- "SMA"
    }
    maArgs <- list(n = n, ...)
    mavg <- do.call(maType, c(list(prices), maArgs))
    maFastArgs <- list(n = fastn, ...)
    fastmavg <- do.call(maType, c(list(prices), maFastArgs))
    sdev <- runSD((mavg - fastmavg), n = n, sample = FALSE)
    if (!isTRUE(centered)) {
        center <- mavg
    }
    else {
        centerrun <- (mavg - fastmavg)/sdev
        if (isTRUE(lavg)) {
            maArgs <- list(n = (n * 2), ...)
        }
        center <- mavg + (do.call(maType, c(list(centerrun), 
            maArgs)))
    }
    up <- center + sd * sdev
    dn <- center - sd * sdev
    res <- cbind(dn, center, up)
    colnames(res) <- c("dn", "center", "up")
    reclass(res, prices)
}


BBands <- function (HLC, n = 20, maType, sd = 2, ...) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    if (NCOL(HLC) == 3) {
        if (is.xts(HLC)) {
            xa <- xcoredata(HLC)
            HLC <- xts(apply(HLC, 1, mean), index(HLC))
            xcoredata(HLC) <- xa
        }
        else {
            HLC <- apply(HLC, 1, mean)
        }
    }
    else if (NCOL(HLC) != 1) {
        stop("Price series must be either High-Low-Close, or Close/univariate.")
    }
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "SMA"
    }
    mavg <- do.call(maType, c(list(HLC), maArgs))
    sdev <- runSD(HLC, n, sample = FALSE)
    up <- mavg + sd * sdev
    dn <- mavg - sd * sdev
    pctB <- (HLC - dn)/(up - dn)
    res <- cbind(dn, mavg, up, pctB)
    colnames(res) <- c("dn", "mavg", "up", "pctB")
    reclass(res, HLC)
}


ultimateOscillator <- function (HLC, n = c(7, 14, 28), wts = c(4, 2, 1)) 
{
    if (length(n) != 3 || length(wts) != 3) 
        stop("length(n) and length(wts) must both be 3")
    HLC <- try.xts(HLC, error = as.matrix)
    HLC.RECLASS <- attr(HLC, ".RECLASS")
    attr(HLC, ".RECLASS") <- FALSE
    atr <- ATR(HLC, n = 1)
    buyPressure <- HLC[, 3] - atr[, "trueLow"]
    avgs <- sapply(n, function(i) runSum(buyPressure, n = i)/runSum(atr[, 
        "tr"], n = i))
    attr(HLC, ".RECLASS") <- HLC.RECLASS
    osc <- 100 * (wts[1] * avgs[, 1] + wts[2] * avgs[, 2] + wts[3] * 
        avgs[, 3])/sum(wts)
    reclass(osc, HLC)
}


ATR <- function (HLC, n = 14, maType, ...) 
{
    HLC <- try.xts(HLC, error = as.matrix)
    if (is.xts(HLC)) {
        closeLag <- lag.xts(HLC[, 3])
    }
    else {
        closeLag <- c(NA, HLC[-NROW(HLC), 3])
    }
    trueHigh <- pmax(HLC[, 1], closeLag, na.rm = FALSE)
    trueLow <- pmin(HLC[, 2], closeLag, na.rm = FALSE)
    tr <- trueHigh - trueLow
    maArgs <- list(n = n, ...)
    if (missing(maType)) {
        maType <- "EMA"
        maArgs$wilder <- TRUE
    }
    atr <- do.call(maType, c(list(tr), maArgs))
    result <- cbind(tr, atr, trueHigh, trueLow)
    colnames(result) <- c("tr", "atr", "trueHigh", "trueLow")
    reclass(result, HLC)
}


runCov <- function (x, y, n = 10, use = "all.obs", sample = TRUE, cumulative = FALSE) 
{
    x <- try.xts(x, error = as.matrix)
    y <- try.xts(y, error = as.matrix)
    if (is.xts(x) && is.xts(y)) {
        xy <- cbind(x, y)
    }
    else {
        xy <- cbind(as.vector(x), as.vector(y))
    }
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    if (NCOL(x) > 1 || NCOL(y) > 1) {
        stop("ncol(x) > 1 or ncol(y) > 1.", " runCov only supports univariate 'x' and 'y'")
    }
    xNAs <- sum(is.na(x))
    yNAs <- sum(is.na(y))
    NAs <- max(xNAs, yNAs)
    if (NAs > 0) {
        if (any(is.na(xy[-(1:NAs), ]))) 
            stop("Series contain non-leading NAs")
        if (NAs + n > NROW(x)) 
            stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    len <- NROW(xy) - NAs
    xCenter <- runMean(x, n, cumulative)
    xCenter[1:(NAs + n - 1)] <- 0
    yCenter <- runMean(y, n, cumulative)
    yCenter[1:(NAs + n - 1)] <- 0
    result <- .Fortran("runCov", rs1 = as.double(x[beg:NROW(xy)]), 
        avg1 = as.double(xCenter[beg:NROW(xy)]), rs2 = as.double(y[beg:NROW(xy)]), 
        avg2 = as.double(yCenter[beg:NROW(xy)]), la = as.integer(len), 
        n = as.integer(n), samp = as.integer(sample), oa = double(len), 
        cu = as.integer(cumulative), PACKAGE = "TTR", DUP = TRUE)$oa
    is.na(result) <- c(1:(n - 1))
    result <- c(rep(NA, NAs), result)
    reclass(result, x)
}


rollSFM <- function (Ra, Rb, n = 60) 
{
    beta <- runCov(Ra, Rb, n)/runVar(Rb, n = n)
    alpha <- runMean(Ra, n) - beta * runMean(Rb, n)
    se.resid <- 1/(n * (n - 2)) * (n * runSum(Ra^2, n) - runSum(Ra, 
        n)^2 - beta^2 * (n * runSum(Rb^2, n) - runSum(Rb, n)^2))
    se.Ra <- runVar(Ra, n = n) * (n - 1)/(n - 2)
    r.squared <- 1 - se.resid/se.Ra
    result <- merge(alpha, beta, r.squared)
    return(result)
}


ALMA <- function (x, n = 9, offset = 0.85, sigma = 6, ...) 
{
    if (offset < 0 || offset > 1) {
        stop("Please ensure 0 <= offset <= 1")
    }
    if (sigma <= 0) 
        stop("sigma must be > 0")
    m <- floor(offset * (n - 1))
    s <- n/sigma
    wts <- exp(-((seq(0, n - 1) - m)^2)/(2 * s * s))
    sumWeights <- sum(wts)
    if (sumWeights != 0) 
        wts <- wts/sumWeights
    alma <- rollapply(x, width = n, FUN = function(xx) sum(xx * 
        wts), align = "right")
    reclass(alma, x)
}


runMax <- function (x, n = 10, cumulative = FALSE) 
{
    x <- try.xts(x, error = as.matrix)
    if (n < 1 || n > NROW(x)) 
        stop("Invalid 'n'")
    NAs <- sum(is.na(x))
    if (NAs > 0) {
        if (any(is.na(x[-(1:NAs)]))) 
            stop("Series contains non-leading NAs")
        if (NAs + n > NROW(x)) 
            stop("not enough non-NA values")
    }
    beg <- 1 + NAs
    len <- NROW(x) - NAs
    if (NCOL(x) > 1) {
        stop("ncol(x) > 1. runMax only supports univariate 'x'")
    }
    result <- double(NROW(x))
    if (cumulative) {
        result[beg:NROW(x)] <- cummax(x[beg:NROW(x)])
    }
    else {
        result[(n + beg - 1)] <- max(x[beg:(n + beg - 1)])
        result <- .Fortran("runmax", ia = as.double(x[beg:NROW(x)]), 
            lia = as.integer(len), n = as.integer(n), oa = as.double(result[beg:NROW(x)]), 
            loa = as.integer(len), PACKAGE = "TTR", DUP = TRUE)$oa
    }
    is.na(result) <- c(1:(n - 1))
    result <- c(rep(NA, NAs), result)
    reclass(result, x)
}




## Package Data

ttrc <- TTR::ttrc		## Technical Trading Rule Composite data



## Package Info

.skeleton_package_title = "Technical Trading Rules"

.skeleton_package_version = "0.23-1"

.skeleton_package_depends = ""

.skeleton_package_imports = "xts,zoo"


## Internal

.skeleton_version = 5


## EOF