##
## Exported symobls in package `nlme`
##

## Exported package methods

nlme <- function (model, data = sys.frame(sys.parent()), fixed, random = fixed, 
    groups, start, correlation = NULL, weights = NULL, subset, 
    method = c("ML", "REML"), na.action = na.fail, naPattern, 
    control = list(), verbose = FALSE) 
{
    UseMethod("nlme")
}


getData <- function (object) 
UseMethod("getData")


nlsList.formula <- function (model, data, start = NULL, control, level, subset, 
    na.action = na.fail, pool = TRUE, warn.nls = NA) 
{
    if (!missing(level) && length(level) > 1) 
        stop("multiple levels not allowed")
    if (is.na(warn.nls <- as.logical(warn.nls))) 
        warn.nls <- !identical(FALSE, getOption("show.error.messages"))
    Call <- match.call()
    if (!missing(subset)) {
        data <- data[eval(asOneSidedFormula(Call[["subset"]])[[2]], 
            data), , drop = FALSE]
    }
    if (!is.data.frame(data)) 
        data <- as.data.frame(data)
    data <- na.action(data)
    if (is.null(grpForm <- getGroupsFormula(model))) {
        if (inherits(data, "groupedData")) {
            if (missing(level)) 
                level <- length(getGroupsFormula(data, asList = TRUE))
            groups <- getGroups(data, level = level)[drop = TRUE]
            grpForm <- getGroupsFormula(data)
        }
        else {
            stop("'data' must be a \"groupedData\" object if 'formula' does not include groups")
        }
    }
    else {
        if (missing(level)) 
            level <- length(getGroupsFormula(model, asList = TRUE))
        model <- eval(substitute(Y ~ RHS, list(Y = model[[2]], 
            RHS = getCovariateFormula(model)[[2]])))
        groups <- getGroups(data, form = grpForm, level = level)[drop = TRUE]
    }
    if (is.null(start) && is.null(attr(data, "parameters"))) {
        FUN <- eval(model[[3]][[1]])
        if (is.function(FUN) && class(FUN) != "selfStart" && 
            !is.null(attr(FUN, "initial"))) {
            stop("old-style self-starting model functions\nare no longer supported.\nNew selfStart functions are available.\nUse\n  SSfpl instead of fpl,\n  SSfol instead of first.order.log,\n  SSbiexp instead of biexp,\n  SSlogis instead of logistic.\nIf writing your own selfStart model, see\n  \"help(selfStart)\"\nfor the new form of the \"initial\" attribute.")
        }
    }
    controlvals <- nls.control()
    if (!missing(control)) 
        controlvals[names(control)] <- control
    val <- lapply(split(data, groups), function(dat) tryCatch({
        data <- as.data.frame(dat)
        if (is.null(start)) {
            nls(model, data = data, control = controlvals)
        }
        else {
            nls(model, data = data, control = controlvals, start = start)
        }
    }, error = function(e) e))
    val <- warnErrList(val, warn = warn.nls)
    if (inherits(data, "groupedData")) {
        attr(val, "units") <- attr(data, "units")
        attr(val, "labels") <- attr(data, "labels")
        attr(val, "outer") <- attr(data, "outer")
    }
    structure(val, class = c("nlsList", "lmList"), call = Call, 
        dims = list(N = nrow(data), M = length(val)), groups = ordered(groups, 
            levels = names(val)), origOrder = match(unique(as.character(groups)), 
            names(val)), pool = pool, groupsForm = grpForm)
}


nlsList <- function (model, data, start, control, level, subset, na.action = na.fail, 
    pool = TRUE, warn.nls = NA) 
UseMethod("nlsList")


gsummary <- function (object, FUN = function(x) mean(x, na.rm = TRUE), omitGroupingFactor = FALSE, 
    form = formula(object), level, groups = getGroups(object, 
        form, level), invariantsOnly = FALSE, ...) 
{
    if (!inherits(object, "data.frame")) {
        stop("object must inherit from \"data.frame\"")
    }
    if (missing(groups)) {
        if (!inherits(form, "formula")) {
            stop("'form' must be a formula")
        }
        if (is.null(grpForm <- getGroupsFormula(form, asList = TRUE))) {
            grpForm <- splitFormula(asOneSidedFormula(form[[length(form)]]))
        }
        if (missing(level)) 
            level <- length(grpForm)
        else if (length(level) != 1) {
            stop("only one level allowed in 'gsummary'")
        }
    }
    gunique <- unique(groups)
    firstInGroup <- match(gunique, groups)
    asFirst <- firstInGroup[match(groups, gunique)]
    value <- as.data.frame(object[firstInGroup, , drop = FALSE])
    row.names(value) <- as.character(gunique)
    value <- value[as.character(sort(gunique)), , drop = FALSE]
    varying <- unlist(lapply(object, function(column, frst) {
        aux <- as.character(column)
        any(!identical(aux, aux[frst]))
    }, frst = asFirst))
    if (any(varying) && (!invariantsOnly)) {
        Mode <- function(x) {
            aux <- table(x)
            names(aux)[match(max(aux), aux)]
        }
        if (is.function(FUN)) {
            FUN <- list(numeric = FUN, ordered = Mode, factor = Mode)
        }
        else {
            if (!(is.list(FUN) && all(sapply(FUN, is.function)))) 
                stop("'FUN' can only be a function or a list of functions")
            auxFUN <- list(numeric = mean, ordered = Mode, factor = Mode)
            aux <- names(auxFUN)[is.na(match(names(auxFUN), names(FUN)))]
            if (length(aux) > 0) 
                FUN[aux] <- auxFUN[aux]
        }
        for (nm in names(object)[varying]) {
            dClass <- if (is.ordered(object[[nm]])) 
                "ordered"
            else if (is.factor(object[[nm]])) 
                "factor"
            else mode(object[[nm]])
            if (dClass == "numeric") {
                value[[nm]] <- as.vector(tapply(object[[nm]], 
                  groups, FUN[["numeric"]], ...))
            }
            else {
                value[[nm]] <- as.vector(tapply(as.character(object[[nm]]), 
                  groups, FUN[[dClass]]))
                if (inherits(object[, nm], "ordered")) {
                  value[[nm]] <- ordered(value[, nm], levels = levels(object[, 
                    nm]))[drop = TRUE]
                }
                else {
                  value[[nm]] <- factor(value[, nm], levels = levels(object[, 
                    nm]))[drop = TRUE]
                }
            }
        }
    }
    else {
        value <- value[, !varying, drop = FALSE]
    }
    if (omitGroupingFactor) {
        if (is.null(form)) {
            stop("cannot omit grouping factor without 'form'")
        }
        grpForm <- getGroupsFormula(form, asList = TRUE)
        if (missing(level)) 
            level <- length(grpForm)
        grpNames <- names(grpForm)[level]
        whichKeep <- is.na(match(names(value), grpNames))
        if (any(whichKeep)) {
            value <- value[, whichKeep, drop = FALSE]
        }
        else {
            return(NULL)
        }
    }
    value
}


augPred <- function (object, primary = NULL, minimum = min(primary), maximum = max(primary), 
    length.out = 51, ...) 
UseMethod("augPred")


corARMA <- function (value = double(p + q), form = ~1, p = 0, q = 0, fixed = FALSE) 
{
    if (!(p >= 0 && (p == round(p)))) {
        stop("autoregressive order must be a non-negative integer")
    }
    if (!(q >= 0 && (q == round(q)))) {
        stop("moving average order must be a non-negative integer")
    }
    if (0 == (p + q)) {
        return(corIdent())
    }
    if (length(value) != p + q) {
        stop("initial value for parameter of wrong length")
    }
    if (max(abs(value)) >= 1) {
        stop("parameters in ARMA structure must be < 1 in absolute value")
    }
    value <- .C(ARMA_unconstCoef, as.integer(p), as.integer(q), 
        pars = as.double(value))$pars
    attributes(value) <- list(formula = form, p = p, q = q, fixed = fixed)
    class(value) <- c("corARMA", "corStruct")
    value
}


pooledSD <- function (object) 
{
    if (!inherits(object, "lmList")) {
        stop("object must inherit from class \"lmList\"")
    }
    aux <- apply(sapply(object, function(el) {
        if (is.null(el)) {
            c(0, 0)
        }
        else {
            aux <- resid(el)
            c(sum(aux^2), length(aux) - length(coef(el)))
        }
    }), 1, sum)
    if (aux[2] == 0) {
        stop("no degrees of freedom for estimating std. dev.")
    }
    val <- sqrt(aux[1]/aux[2])
    attr(val, "df") <- aux[2]
    val
}


lme <- function (fixed, data = sys.frame(sys.parent()), random, correlation = NULL, 
    weights = NULL, subset, method = c("REML", "ML"), na.action = na.fail, 
    control = list(), contrasts = NULL, keep.data = TRUE) 
UseMethod("lme")


corNatural <- function (value = numeric(0), form = ~1, fixed = FALSE) 
{
    attr(value, "formula") <- form
    attr(value, "fixed") <- fixed
    class(value) <- c("corNatural", "corStruct")
    value
}


isBalanced <- function (object, countOnly = FALSE, level) 
UseMethod("isBalanced")


`coefficients<-` <- function (object, ..., value) 
UseMethod("coef<-")


pdSymm <- function (value = numeric(0), form = NULL, nam = NULL, data = parent.frame()) 
{
    object <- numeric(0)
    class(object) <- c("pdSymm", "pdMat")
    pdConstruct(object, value, form, nam, data)
}


corMatrix <- function (object, ...) 
UseMethod("corMatrix")


isInitialized <- function (object) 
UseMethod("isInitialized")


lmeControl <- function (maxIter = 50, msMaxIter = 50, tolerance = 1e-06, niterEM = 25, 
    msMaxEval = 200, msTol = 1e-07, msVerbose = FALSE, returnObject = FALSE, 
    gradHess = TRUE, apVar = TRUE, .relStep = .Machine$double.eps^(1/3), 
    minAbsParApVar = 0.05, opt = c("nlminb", "optim"), optimMethod = "BFGS", 
    natural = TRUE, sigma = NULL, ...) 
{
    if (is.null(sigma)) 
        sigma <- 0
    else if (!is.finite(sigma) || length(sigma) != 1 || sigma < 
        0) 
        stop("Within-group std. dev. must be a positive numeric value")
    list(maxIter = maxIter, msMaxIter = msMaxIter, tolerance = tolerance, 
        niterEM = niterEM, msMaxEval = msMaxEval, msTol = msTol, 
        msVerbose = msVerbose, returnObject = returnObject, gradHess = gradHess, 
        apVar = apVar, .relStep = .relStep, opt = match.arg(opt), 
        optimMethod = optimMethod, minAbsParApVar = minAbsParApVar, 
        natural = natural, sigma = sigma, ...)
}


corSymm <- function (value = numeric(0), form = ~1, fixed = FALSE) 
{
    attr(value, "formula") <- form
    attr(value, "fixed") <- fixed
    class(value) <- c("corSymm", "corStruct")
    value
}


`covariate<-` <- function (object, value) 
UseMethod("covariate<-")


random.effects <- function (object, ...) 
UseMethod("ranef")


groupedData <- function (formula, data = sys.parent(1), order.groups = TRUE, 
    FUN = function(x) max(x, na.rm = TRUE), outer = NULL, inner = NULL, 
    labels = NULL, units = NULL) 
{
    if (!(inherits(formula, "formula") && length(formula) == 
        3)) {
        stop("first argument to 'groupedData' must be a two-sided formula")
    }
    if (is.null(grpForm <- getGroupsFormula(formula, asList = TRUE))) {
        stop("right-hand side of first argument must be a conditional expression")
    }
    mCall <- match.call()
    mCall[[1]] <- as.name(ifelse(length(grpForm) == 1, "nfGroupedData", 
        "nmGroupedData"))
    eval(mCall, envir = parent.frame())
}


corSpher <- function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", 
    "maximum", "manhattan"), fixed = FALSE) 
{
    attr(value, "formula") <- form
    attr(value, "nugget") <- nugget
    attr(value, "metric") <- match.arg(metric)
    attr(value, "fixed") <- fixed
    class(value) <- c("corSpher", "corSpatial", "corStruct")
    value
}


nmGroupedData <- function (formula, data = sys.parent(1), order.groups = TRUE, 
    FUN = function(x) max(x, na.rm = TRUE), outer = NULL, inner = NULL, 
    labels = NULL, units = NULL) 
{
    checkForList <- function(object, nams, expand = FALSE) {
        if (is.null(object)) 
            return(object)
        if (is.list(object)) {
            if (is.null(names(object))) {
                names(object) <- nams[seq_along(object)]
            }
            return(object)
        }
        if (expand) {
            object <- rep(list(object), length(nams))
            names(object) <- nams
            return(object)
        }
        object <- list(object)
        names(object) <- nams[length(nams)]
        object
    }
    if (!(inherits(formula, "formula") && length(formula) == 
        3)) {
        stop("first argument to 'nmGroupedData' must be a two-sided formula")
    }
    grpForm <- getGroupsFormula(formula, asList = TRUE)
    if (is.null(grpForm)) {
        stop("right-hand side of first argument must be a conditional expression")
    }
    if (length(grpForm) == 1) {
        mCall <- match.call()[-1]
        do.call("nfGroupedData", mCall)
    }
    grpNames <- names(grpForm)
    names(grpNames) <- grpNames
    order.groups <- checkForList(order.groups, grpNames, TRUE)
    outer <- checkForList(outer, grpNames)
    inner <- checkForList(inner, grpNames)
    if (missing(data)) {
        vnames <- all.vars(asOneFormula(formula, outer, inner))
        alist <- lapply(as.list(vnames), as.name)
        names(alist) <- vnames
        data <- do.call("data.frame", alist)
    }
    else {
        if (!inherits(data, "data.frame")) {
            stop("second argument to 'groupedData' must inherit from data.frame")
        }
    }
    response <- getResponse(data, formula)
    primary <- getCovariate(data, formula)
    groups <- getGroups(data, formula)
    attr(data, "formula") <- formula
    attr(data, "formulaList") <- grpForm
    attr(data, "labels") <- labels
    attr(data, "units") <- units
    attr(data, "inner") <- inner
    attr(data, "outer") <- outer
    attr(data, "order.groups") <- order.groups
    attr(data, "FUN") <- FUN
    class(data) <- unique(c("nmGroupedData", "groupedData", class(data)))
    data
}


logDet <- function (object, ...) 
UseMethod("logDet")


pdIdent <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent())) 
{
    object <- numeric(0)
    class(object) <- c("pdIdent", "pdMat")
    pdConstruct(object, value, form, nam, data)
}


LDEsysMat <- function (pars, incidence) 
{
    tt <- incidence[, "To"]
    ff <- incidence[, "From"]
    pp <- pars[incidence[, "Par"]]
    n <- max(ff, tt)
    val <- array(double(n * n), c(n, n))
    diag(val) <- -tapply(pp, ff, sum)
    val[incidence[tt > 0, c("To", "From"), drop = FALSE]] <- pp[tt > 
        0]
    val
}


`matrix<-` <- function (object, value) 
UseMethod("matrix<-")


varExp <- function (value = numeric(0), form = ~fitted(.), fixed = NULL) 
{
    value <- unlist(value)
    fixed <- attr(value, "fixed") <- unlist(fixed)
    attr(value, "formula") <- form <- asOneSidedFormula(form)
    if (length(all.vars(getCovariateFormula(form))) == 0) {
        stop("'form' must have a covariate")
    }
    if (!is.null(getGroupsFormula(form))) {
        if (is.null(grpNames <- names(value)) && (length(value) > 
            1)) {
            stop("initial values must have group names in 'varExp'")
        }
        if (!is.null(fixed)) {
            if (is.null(names(fixed))) {
                stop("fixed parameters must have group names in 'varExp'")
            }
        }
        attr(value, "groupNames") <- c(grpNames, names(fixed))
    }
    else {
        attr(value, "whichFix") <- !is.null(fixed)
    }
    class(value) <- c("varExp", "varFunc")
    value
}


lmList.formula <- function (object, data, level, subset, na.action = na.fail, pool = TRUE, 
    warn.lm = TRUE) 
{
    Call <- match.call()
    if (!missing(subset)) {
        data <- data[eval(asOneSidedFormula(Call[["subset"]])[[2]], 
            data), , drop = FALSE]
    }
    if (!inherits(data, "data.frame")) 
        data <- as.data.frame(data)
    data <- na.action(data)
    if (is.null(grpForm <- getGroupsFormula(object))) {
        if (inherits(data, "groupedData")) {
            if (missing(level)) 
                level <- length(getGroupsFormula(data, asList = TRUE))
            else if (length(level) > 1) {
                stop("multiple levels not allowed")
            }
            groups <- getGroups(data, level = level)[drop = TRUE]
            grpForm <- getGroupsFormula(data)
            Call$object <- eval(parse(text = paste(deparse(Call$object), 
                deparse(grpForm[[2]]), sep = "|")))
        }
        else {
            stop("'data' must be a \"groupedData\" object if 'groups' argument is missing")
        }
    }
    else {
        if (missing(level)) 
            level <- length(getGroupsFormula(object, asList = TRUE))
        else if (length(level) > 1) {
            stop("multiple levels not allowed")
        }
        groups <- getGroups(data, form = grpForm, level = level)[drop = TRUE]
        object <- eval(substitute(Y ~ X, list(Y = getResponseFormula(object)[[2]], 
            X = getCovariateFormula(object)[[2]])))
    }
    val <- lapply(split(data, groups), function(dat) tryCatch(lm(object, 
        data = dat, na.action = na.action), error = function(e) e))
    val <- warnErrList(val, warn = warn.lm)
    if (inherits(data, "groupedData")) {
        attr(val, "units") <- attr(data, "units")
        attr(val, "labels") <- attr(data, "labels")
    }
    structure(val, class = "lmList", dims = list(N = nrow(data), 
        M = length(val)), call = Call, groupsForm = grpForm, 
        groups = ordered(groups, levels = names(val)), origOrder = match(unique(as.character(groups)), 
            names(val)), level = level, pool = pool)
}


corCompSymm <- function (value = 0, form = ~1, fixed = FALSE) 
{
    if (abs(value) >= 1) {
        stop("parameter in \"corCompSymm\" structure must be < 1 in absolute value")
    }
    attr(value, "formula") <- form
    attr(value, "fixed") <- fixed
    class(value) <- c("corCompSymm", "corStruct")
    value
}


pdLogChol <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.parent()) 
{
    object <- numeric(0)
    class(object) <- c("pdLogChol", "pdMat")
    pdConstruct(object, value, form, nam, data)
}


fdHess <- function (pars, fun, ..., .relStep = .Machine$double.eps^(1/3), 
    minAbsPar = 0) 
{
    pars <- as.numeric(pars)
    npar <- length(pars)
    incr <- pmax(abs(pars), minAbsPar) * .relStep
    baseInd <- diag(npar)
    frac <- c(1, incr, incr^2)
    cols <- list(0, baseInd, -baseInd)
    for (i in seq_along(pars)[-npar]) {
        cols <- c(cols, list(baseInd[, i] + baseInd[, -(1:i)]))
        frac <- c(frac, incr[i] * incr[-(1:i)])
    }
    indMat <- do.call("cbind", cols)
    shifted <- pars + incr * indMat
    indMat <- t(indMat)
    Xcols <- list(1, indMat, indMat^2)
    for (i in seq_along(pars)[-npar]) {
        Xcols <- c(Xcols, list(indMat[, i] * indMat[, -(1:i)]))
    }
    coefs <- solve(do.call("cbind", Xcols), apply(shifted, 2, 
        fun, ...))/frac
    Hess <- diag(coefs[1 + npar + seq_along(pars)], ncol = npar)
    Hess[row(Hess) > col(Hess)] <- coefs[-(1:(1 + 2 * npar))]
    list(mean = coefs[1], gradient = coefs[1 + seq_along(pars)], 
        Hessian = (Hess + t(Hess)))
}


pdNatural <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent())) 
{
    object <- numeric(0)
    class(object) <- c("pdNatural", "pdMat")
    pdConstruct(object, value, form, nam, data)
}


VarCorr <- function (x, sigma = 1, ...) 
UseMethod("VarCorr")


lmeStruct <- function (reStruct, corStruct = NULL, varStruct = NULL) 
{
    val <- list(reStruct = reStruct, corStruct = corStruct, varStruct = varStruct)
    structure(val[!vapply(val, is.null, NA)], settings = attr(val$reStruct, 
        "settings"), class = c("lmeStruct", "modelStruct"))
}


Initialize <- function (object, data, ...) 
UseMethod("Initialize")


lme.lmList <- function (fixed, data = sys.frame(sys.parent()), random, correlation = NULL, 
    weights = NULL, subset, method = c("REML", "ML"), na.action = na.fail, 
    control = list(), contrasts = NULL, keep.data = TRUE) 
{
    if (length(grpForm <- getGroupsFormula(fixed, asList = TRUE)) > 
        1) {
        stop("can only fit \"lmList\" objects with single grouping variable")
    }
    this.call <- as.list(match.call())[-1L]
    if (!is.na(match("data", names(this.call)))) {
        warning("'lme.lmList' will redefine 'data'")
    }
    last.call <- as.list(attr(fixed, "call"))[-1L]
    whichLast <- match(c("object", "data", "na.action"), names(last.call))
    whichLast <- whichLast[!is.na(whichLast)]
    last.call <- last.call[whichLast]
    names(last.call)[match(names(last.call), "object")] <- "fixed"
    this.call[names(last.call)] <- last.call
    this.call$fixed <- eval(substitute(L ~ R, list(L = getResponseFormula(fixed)[[2L]], 
        R = getCovariateFormula(fixed)[[2L]])))
    if (missing(random)) {
        random <- eval(as.call(this.call[["fixed"]][-2]))
    }
    random <- reStruct(random, data = NULL)
    mData <- this.call[["data"]]
    if (is.null(mData)) {
        allV <- all.vars(formula(random))
        if (length(allV) > 0) {
            alist <- lapply(as.list(allV), as.name)
            names(alist) <- allV
            alist <- c(as.list(quote(data.frame)), alist)
            mode(alist) <- "call"
            mData <- eval(alist, sys.parent(1))
        }
    }
    else {
        if (mode(mData) == "name" || mode(mData) == "call") {
            mData <- eval(mData)
        }
    }
    reSt <- reStruct(random, data = mData)
    names(reSt) <- names(grpForm)
    if (length(reSt) > 1) {
        stop("can only fit \"lmList\" objects with single grouping variable")
    }
    rNames <- Names(reSt[[1L]])
    if (all(match(rNames, names(cf <- na.omit(coef(fixed))), 
        0))) {
        if (isInitialized(reSt)) {
            warning("initial value for \"reStruct\" overwritten in 'lme.lmList'")
        }
        madRes <- mad(resid(fixed), na.rm = TRUE)
        madRan <- unlist(lapply(cf, mad, na.rm = TRUE)[rNames])
        names(madRan) <- rNames
        matrix(reSt) <- diag((madRan/madRes)^2, ncol = length(rNames))
    }
    this.call[["random"]] <- reSt
    val <- do.call(lme.formula, this.call)
    val$origCall <- match.call()
    val
}


varWeights <- function (object) 
UseMethod("varWeights")


varFunc <- function (object) 
{
    if (is.null(object)) 
        return(object)
    if (inherits(object, "varFunc")) {
        return(object)
    }
    if (inherits(object, "formula") || is.character(object)) {
        return(varFixed(asOneSidedFormula(object)))
    }
    stop("can only construct \"varFunc\" object from another \"varFunc\" object, a formula, or a character string")
}


pdFactor <- function (object) 
UseMethod("pdFactor")


getResponse <- function (object, form = formula(object)) 
UseMethod("getResponse")


ACF <- function (object, maxLag, ...) 
UseMethod("ACF")


corAR1 <- function (value = 0, form = ~1, fixed = FALSE) 
{
    if (abs(value) >= 1) {
        stop("parameter in AR(1) structure must be between -1 and 1")
    }
    value <- log((1 + value)/(1 - value))
    attr(value, "formula") <- form
    attr(value, "fixed") <- fixed
    class(value) <- c("corAR1", "corStruct")
    value
}


gnlsControl <- function (maxIter = 50, nlsMaxIter = 7, msMaxIter = 50, minScale = 0.001, 
    tolerance = 1e-06, nlsTol = 0.001, msTol = 1e-07, returnObject = FALSE, 
    msVerbose = FALSE, apVar = TRUE, .relStep = .Machine$double.eps^(1/3), 
    opt = c("nlminb", "optim"), optimMethod = "BFGS", minAbsParApVar = 0.05, 
    sigma = NULL) 
{
    if (is.null(sigma)) 
        sigma <- 0
    else if (!is.finite(sigma) || length(sigma) != 1 || sigma < 
        0) 
        stop("Within-group std. dev. must be a positive numeric value")
    list(maxIter = maxIter, nlsMaxIter = nlsMaxIter, msMaxIter = msMaxIter, 
        minScale = minScale, tolerance = tolerance, nlsTol = nlsTol, 
        msTol = msTol, returnObject = returnObject, msVerbose = msVerbose, 
        apVar = apVar, opt = match.arg(opt), optimMethod = optimMethod, 
        .relStep = .relStep, minAbsParApVar = minAbsParApVar, 
        sigma = sigma)
}


fixef <- function (object, ...) 
UseMethod("fixef")


collapse <- function (object, ...) 
UseMethod("collapse")


`coef<-` <- function (object, ..., value) 
UseMethod("coef<-")


pdDiag <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent())) 
{
    object <- numeric(0)
    class(object) <- c("pdDiag", "pdMat")
    pdConstruct(object, value, form, nam, data)
}


varFixed <- function (value = ~1) 
{
    if (!inherits(value, "formula")) {
        stop("'value' must be a one sided formula")
    }
    form <- asOneSidedFormula(value)
    if (length(all.vars(getCovariateFormula(form))) == 0) {
        stop("'form' must have a covariate")
    }
    if (!is.null(getGroupsFormula(form))) {
        form <- getCovariateFormula(form)
        warning("ignoring 'group' in \"varFixed\" formula")
    }
    value <- numeric(0)
    attr(value, "formula") <- form
    class(value) <- c("varFixed", "varFunc")
    value
}


glsControl <- function (maxIter = 50L, msMaxIter = 200L, tolerance = 1e-06, 
    msTol = 1e-07, msVerbose = FALSE, singular.ok = FALSE, returnObject = FALSE, 
    apVar = TRUE, .relStep = .Machine$double.eps^(1/3), opt = c("nlminb", 
        "optim"), optimMethod = "BFGS", minAbsParApVar = 0.05, 
    natural = TRUE, sigma = NULL) 
{
    if (is.null(sigma)) 
        sigma <- 0
    else {
        if (!is.finite(sigma) || length(sigma) != 1 || sigma < 
            0) 
            stop("Within-group std. dev. must be a positive numeric value")
    }
    list(maxIter = maxIter, msMaxIter = msMaxIter, tolerance = tolerance, 
        msTol = msTol, msVerbose = msVerbose, singular.ok = singular.ok, 
        returnObject = returnObject, apVar = apVar, minAbsParApVar = minAbsParApVar, 
        .relStep = .relStep, opt = match.arg(opt), optimMethod = optimMethod, 
        natural = natural, sigma = sigma)
}


plot.lme <- function (x, form = resid(., type = "pearson") ~ fitted(.), abline, 
    id = NULL, idLabels = NULL, idResType = c("pearson", "normalized"), 
    grid, ...) 
{
    object <- x
    if (!inherits(form, "formula")) {
        stop("'form' must be a formula")
    }
    allV <- all.vars(asOneFormula(form, id, idLabels))
    allV <- allV[is.na(match(allV, c("T", "F", "TRUE", "FALSE")))]
    if (length(allV) > 0) {
        data <- getData(object)
        if (is.null(data)) {
            alist <- lapply(as.list(allV), as.name)
            names(alist) <- allV
            alist <- c(list(quote(data.frame)), alist)
            mode(alist) <- "call"
            data <- eval(alist, sys.parent(1))
        }
        else {
            if (any(naV <- is.na(match(allV, names(data))))) {
                stop(sprintf(ngettext(sum(naV), "%s not found in data", 
                  "%s not found in data"), allV[naV]), domain = NA)
            }
        }
    }
    else data <- NULL
    if (inherits(data, "groupedData")) {
        ff <- formula(data)
        rF <- deparse(getResponseFormula(ff)[[2]])
        cF <- c_deparse(getCovariateFormula(ff)[[2]])
        lbs <- attr(data, "labels")
        unts <- attr(data, "units")
        if (!is.null(lbs$x)) 
            cL <- paste(lbs$x, unts$x)
        else cF <- NULL
        if (!is.null(lbs$y)) 
            rL <- paste(lbs$y, unts$y)
        else rF <- NULL
    }
    else {
        rF <- cF <- NULL
    }
    dots <- list(...)
    if (length(dots) > 0) 
        args <- dots
    else args <- list()
    data <- as.list(c(as.list(data), . = list(object)))
    covF <- getCovariateFormula(form)
    .x <- eval(covF[[2]], data)
    if (!is.numeric(.x)) {
        stop("covariate must be numeric")
    }
    argForm <- ~.x
    argData <- data.frame(.x = .x, check.names = FALSE)
    if (is.null(xlab <- attr(.x, "label"))) {
        xlab <- deparse(covF[[2]])
        if (!is.null(cF) && (xlab == cF)) 
            xlab <- cL
        else if (!is.null(rF) && (xlab == rF)) 
            xlab <- rL
    }
    if (is.null(args$xlab)) 
        args$xlab <- xlab
    respF <- getResponseFormula(form)
    if (!is.null(respF)) {
        .y <- eval(respF[[2]], data)
        if (is.null(ylab <- attr(.y, "label"))) {
            ylab <- deparse(respF[[2]])
            if (!is.null(cF) && (ylab == cF)) 
                ylab <- cL
            else if (!is.null(rF) && (ylab == rF)) 
                ylab <- rL
        }
        argForm <- .y ~ .x
        argData[, ".y"] <- .y
        if (is.null(args$ylab)) 
            args$ylab <- ylab
    }
    grpsF <- getGroupsFormula(form)
    if (!is.null(grpsF)) {
        gr <- splitFormula(grpsF, sep = "*")
        for (i in seq_along(gr)) {
            auxGr <- all.vars(gr[[i]])
            for (j in auxGr) {
                argData[[j]] <- eval(as.name(j), data)
            }
        }
        if (length(argForm) == 2) 
            argForm <- eval(parse(text = paste("~ .x |", deparse(grpsF[[2]]))))
        else argForm <- eval(parse(text = paste(".y ~ .x |", 
            deparse(grpsF[[2]]))))
    }
    args <- c(list(argForm, data = argData), args)
    if (is.null(args$cex)) 
        args$cex <- par("cex")
    if (is.null(args$adj)) 
        args$adj <- par("adj")
    if (!is.null(id)) {
        idResType <- match.arg(idResType)
        id <- switch(mode(id), numeric = {
            if ((id <= 0) || (id >= 1)) {
                stop("'id' must be between 0 and 1")
            }
            as.logical(abs(resid(object, type = idResType)) > 
                -qnorm(id/2))
        }, call = eval(asOneSidedFormula(id)[[2]], data), stop("'id' can only be a formula or numeric"))
        if (is.null(idLabels)) {
            idLabels <- getGroups(object)
            if (length(idLabels) == 0) 
                idLabels <- 1:object$dims$N
            idLabels <- as.character(idLabels)
        }
        else {
            if (mode(idLabels) == "call") {
                idLabels <- as.character(eval(asOneSidedFormula(idLabels)[[2]], 
                  data))
            }
            else if (is.vector(idLabels)) {
                if (length(idLabels <- unlist(idLabels)) != length(id)) {
                  stop("'idLabels' of incorrect length")
                }
                idLabels <- as.character(idLabels)
            }
            else {
                stop("'idLabels' can only be a formula or a vector")
            }
        }
    }
    if (missing(abline)) {
        if (missing(form)) {
            abline <- c(0, 0)
        }
        else {
            abline <- NULL
        }
    }
    assign("abl", abline)
    if (length(argForm) == 3) {
        if (is.numeric(.y)) {
            plotFun <- "xyplot"
            if (is.null(args$panel)) {
                args <- c(args, panel = list(function(x, y, subscripts, 
                  ...) {
                  x <- as.numeric(x)
                  y <- as.numeric(y)
                  dots <- list(...)
                  if (grid) panel.grid()
                  panel.xyplot(x, y, ...)
                  if (any(ids <- id[subscripts])) {
                    ltext(x[ids], y[ids], idLabels[subscripts][ids], 
                      cex = dots$cex, adj = dots$adj)
                  }
                  if (!is.null(abl)) {
                    if (length(abl) == 2) panel.abline(a = abl, 
                      ...) else panel.abline(h = abl, ...)
                  }
                }))
            }
        }
        else {
            plotFun <- "bwplot"
            if (is.null(args$panel)) {
                args <- c(args, panel = list(function(x, y, ...) {
                  if (grid) panel.grid()
                  panel.bwplot(x, y, ...)
                  if (!is.null(abl)) {
                    panel.abline(v = abl[1], ...)
                  }
                }))
            }
        }
    }
    else {
        plotFun <- "histogram"
        if (is.null(args$panel)) {
            args <- c(args, panel = list(function(x, ...) {
                if (grid) panel.grid()
                panel.histogram(x, ...)
                if (!is.null(abl)) {
                  panel.abline(v = abl[1], ...)
                }
            }))
        }
    }
    if (missing(grid)) {
        if (plotFun == "xyplot") 
            grid <- TRUE
        else grid <- FALSE
    }
    do.call(plotFun, as.list(args))
}


gapply <- function (object, which, FUN, form = formula(object), level, 
    groups = getGroups(object, form, level), ...) 
{
    if (!inherits(object, "data.frame")) {
        stop("object must inherit from \"data.frame\"")
    }
    if (missing(groups)) {
        if (!inherits(form, "formula")) {
            stop("'form' must be a formula")
        }
        if (is.null(grpForm <- getGroupsFormula(form, asList = TRUE))) {
            grpForm <- splitFormula(asOneSidedFormula(form[[length(form)]]))
        }
        if (missing(level)) 
            level <- length(grpForm)
        else if (length(level) != 1) {
            stop("only one level allowed in 'gapply'")
        }
        groups <- groups
    }
    if (!missing(which)) {
        switch(mode(which), character = {
            wchNot <- is.na(match(which, names(object)))
            if (any(wchNot)) {
                stop(sprintf(ngettext(sum(wchNot), "%s not matched", 
                  "%s not matched"), paste(which[wchNot], collapse = ",")), 
                  domain = NA)
            }
        }, numeric = {
            if (any(is.na(match(which, 1:ncol(object))))) {
                stop(gettextf("'which' must be between 1 and %d", 
                  ncol(object)), domain = NA)
            }
        }, stop("'which' can only be character or integer"))
        object <- object[, which, drop = FALSE]
    }
    val <- lapply(X = split(object, groups), FUN = FUN, ...)
    if (is.atomic(val[[1]]) && length(val[[1]]) == 1) {
        val <- unlist(val)
    }
    val
}


nlmeStruct <- function (reStruct, corStruct = NULL, varStruct = NULL) 
{
    val <- list(reStruct = reStruct, corStruct = corStruct, varStruct = varStruct)
    structure(val[!vapply(val, is.null, NA)], settings = attr(val$reStruct, 
        "settings"), class = c("nlmeStruct", "lmeStruct", "modelStruct"))
}


corFactor <- function (object, ...) 
UseMethod("corFactor")


corIdent <- function (form = NULL) 
{
    value <- numeric(0)
    attr(value, "formula") <- form
    attr(value, "fixed") <- TRUE
    class(value) <- c("corIdent", "corStruct")
    value
}


getGroupsFormula <- function (object, asList = FALSE, sep = "/") 
UseMethod("getGroupsFormula")


getResponseFormula <- function (object) 
{
    form <- formula(object)
    if (!(inherits(form, "formula") && (length(form) == 3))) {
        stop("'form' must be a two-sided formula")
    }
    eval(parse(text = paste("~", deparse(form[[2]]))))
}


reStruct <- function (object, pdClass = "pdLogChol", REML = FALSE, data = sys.frame(sys.parent())) 
{
    if (inherits(object, "reStruct")) {
        if (!missing(REML)) 
            attr(object, "settings")[1] <- as.integer(REML)
        object[] <- lapply(object, function(el, data) {
            pdMat(el, data = data)
        }, data = data)
        return(object)
    }
    plen <- NULL
    if (inherits(object, "formula")) {
        if (is.null(grpForm <- getGroupsFormula(object, asList = TRUE))) {
            object <- list(object)
        }
        else {
            if (length(object) == 3) {
                object <- eval(parse(text = paste(deparse(getResponseFormula(object)[[2]]), 
                  deparse(getCovariateFormula(object)[[2]], width.cutoff = 500), 
                  sep = "~")))
            }
            else {
                object <- getCovariateFormula(object)
            }
            object <- rep(list(object), length(grpForm))
            names(object) <- names(grpForm)
        }
    }
    else if (inherits(object, "pdMat")) {
        if (is.null(formula(object))) {
            stop("\"pdMat\" element must have a formula")
        }
        object <- list(object)
    }
    else {
        if (data.class(object) != "list") {
            stop("'object' must be a list or a formula")
        }
        if (is.null(names(object)) && all(unlist(lapply(object, 
            function(el) {
                inherits(el, "formula") && length(el) == 3
            })))) {
            object <- list(object)
        }
        else {
            object <- lapply(object, function(el) {
                if (inherits(el, "pdMat")) {
                  if (is.null(formula(el))) {
                    stop("\"pdMat\" elements must have a formula")
                  }
                  return(el)
                }
                if (inherits(el, "formula")) {
                  grpForm <- getGroupsFormula(el)
                  if (!is.null(grpForm)) {
                    el <- getCovariateFormula(el)
                    attr(el, "grpName") <- deparse(grpForm[[2]])
                  }
                  return(el)
                }
                else {
                  if (data.class(el) == "list" && all(unlist(lapply(el, 
                    function(el1) {
                      inherits(el1, "formula") && length(el1) == 
                        3
                    })))) {
                    return(el)
                  }
                  else {
                    stop("elements in 'object' must be formulas or \"pdMat\" objects")
                  }
                }
            })
        }
        if (is.null(namObj <- names(object))) {
            namObj <- rep("", length(object))
        }
        aux <- unlist(lapply(object, function(el) {
            if (inherits(el, "formula") && !is.null(attr(el, 
                "grpName"))) {
                attr(el, "grpName")
            }
            else ""
        }))
        auxNam <- namObj == ""
        if (any(auxNam)) {
            namObj[auxNam] <- aux[auxNam]
        }
        names(object) <- namObj
    }
    object <- lapply(object, function(el, pdClass, data) {
        pdMat(el, pdClass = pdClass, data = data)
    }, pdClass = pdClass, data = data)
    object <- rev(object)
    if (all(unlist(lapply(object, isInitialized)))) {
        plen <- unlist(lapply(object, function(el) length(coef(el))))
    }
    pC <- unlist(lapply(object, data.class))
    pC <- match(pC, c("pdSymm", "pdDiag", "pdIdent", "pdCompSymm", 
        "pdLogChol"), 0) - 1
    attr(object, "settings") <- c(as.integer(REML), 1, 0, pC)
    attr(object, "plen") <- plen
    class(object) <- "reStruct"
    object
}


glsEstimate <- function (object, conLin = attr(object, "conLin"), control = list(singular.ok = FALSE)) 
{
    dd <- conLin$dims
    p <- dd$p
    oXy <- conLin$Xy
    fixSig <- conLin$fixedSigma
    sigma <- conLin$sigma
    conLin <- recalc(object, conLin)
    val <- .C(gls_estimate, as.double(conLin$Xy), as.integer(unlist(dd)), 
        beta = double(p), sigma = as.double(sigma), logLik = double(1L), 
        varBeta = double(p * p), rank = integer(1), pivot = as.integer(1:(p + 
            1L)), NAOK = TRUE)[c("beta", "sigma", "logLik", "varBeta", 
        "rank", "pivot")]
    rnk <- val[["rank"]]
    rnkm1 <- rnk - 1
    if (!control$singular.ok && rnkm1 < p) {
        stop(gettextf("computed \"gls\" fit is singular, rank %s", 
            rnk), domain = NA)
    }
    N <- dd$N - dd$REML * p
    namCoef <- colnames(oXy)[val[["pivot"]][1:rnkm1] + 1L]
    varBeta <- t(array(val[["varBeta"]], c(rnkm1, rnkm1), list(namCoef, 
        namCoef)))
    beta <- val[["beta"]][1:rnkm1]
    names(beta) <- namCoef
    fitted <- c(oXy[, namCoef, drop = FALSE] %*% beta)
    resid <- oXy[, p + 1] - fitted
    ll <- conLin$logLik + val[["logLik"]]
    logLik <- if (!fixSig) {
        (N * (logb(N) - (1 + logb(2 * pi))))/2 + ll
    }
    else {
        (-N/2) * logb(2 * pi) + ll
    }
    list(logLik = logLik, beta = beta, sigma = val[["sigma"]], 
        varBeta = varBeta, fitted = fitted, resid = resid, auxSigma = sqrt(sum((resid)^2))/sqrt(N))
}


pdMat <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()), 
    pdClass = "pdSymm") 
{
    if (inherits(value, "pdMat")) {
        pdClass <- class(value)
    }
    object <- numeric(0)
    class(object) <- unique(c(pdClass, "pdMat"))
    pdConstruct(object, value, form, nam, data)
}


varPower <- function (value = numeric(0), form = ~fitted(.), fixed = NULL) 
{
    value <- unlist(value)
    fixed <- attr(value, "fixed") <- unlist(fixed)
    attr(value, "formula") <- form <- asOneSidedFormula(form)
    if (length(all.vars(getCovariateFormula(form))) == 0) {
        stop("'form' must have a covariate")
    }
    if (!is.null(getGroupsFormula(form))) {
        if (is.null(grpNames <- names(value)) && (length(value) > 
            1)) {
            stop("initial values must have group names in 'varPower'")
        }
        if (!is.null(fixed)) {
            if (is.null(names(fixed))) {
                stop("fixed parameters must have group names in 'varPower'")
            }
        }
        attr(value, "groupNames") <- c(grpNames, names(fixed))
    }
    else {
        attr(value, "whichFix") <- !is.null(fixed)
    }
    class(value) <- c("varPower", "varFunc")
    value
}


nlme.nlsList <- function (model, data = sys.frame(sys.parent()), fixed, random = fixed, 
    groups, start, correlation = NULL, weights = NULL, subset, 
    method = c("ML", "REML"), na.action = na.fail, naPattern, 
    control = list(), verbose = FALSE) 
{
    controlvals <- nlmeControl()
    controlvals[names(control)] <- control
    thisCall <- as.list(match.call())[-1]
    if (any(!is.na(match(names(thisCall), c("fixed", "data", 
        "start"))))) {
        warning("'nlme.nlsList' will redefine 'fixed', 'data', and 'start'")
    }
    method <- match.arg(method)
    REML <- method == "REML"
    last.call <- as.list(attr(model, "call"))[-1]
    last.call$control <- NULL
    last.call$pool <- NULL
    thisCall[names(last.call)] <- last.call
    thisModel <- last.call[["model"]]
    thisCall[["model"]] <- eval(parse(text = paste(deparse(getResponseFormula(thisModel)[[2]]), 
        c_deparse(getCovariateFormula(thisModel)[[2]]), sep = "~")))
    cf <- na.omit(coef(model))
    start <- list(fixed = unlist(lapply(cf, median, na.rm = TRUE)))
    pnames <- names(start$fixed) <- names(cf)
    thisCall[["fixed"]] <- lapply(as.list(pnames), function(el) eval(parse(text = paste(el, 
        1, sep = "~"))))
    if (missing(random)) {
        random <- thisCall[["fixed"]]
    }
    reSt <- reStruct(random, data = NULL)
    if (missing(groups)) {
        thisCall[["groups"]] <- groups <- getGroupsFormula(model)
    }
    if (length(reSt) > 1 || length(groups[[2]]) > 1) {
        stop("can only fit \"nlsList\" objects with single grouping variable")
    }
    ranForm <- formula(reSt)[[1]]
    if (!is.list(ranForm)) {
        ranForm <- list(ranForm)
    }
    mData <- thisCall[["data"]]
    if (is.null(mData)) {
        allV <- unique(unlist(lapply(ranForm, function(el) all.vars(el[[3]]))))
        if (length(allV) > 0) {
            alist <- lapply(as.list(allV), as.name)
            names(alist) <- allV
            alist <- c(as.list(quote(data.frame)), alist)
            mode(alist) <- "call"
            mData <- eval(alist, sys.parent(1))
        }
    }
    else if (mode(mData) == "name" || mode(mData) == "call") {
        mData <- eval(mData)
    }
    reSt <- reStruct(random, REML = REML, data = mData)
    names(reSt) <- deparse(groups[[2]])
    rnames <- sapply(lapply(ranForm, `[[`, 2L), deparse)
    if (all(match(rnames, pnames, 0))) {
        madRes <- mad(resid(model), na.rm = TRUE)
        madRan <- unlist(lapply(cf, mad, na.rm = TRUE))
        madRan <- madRan[rnames]
        if (isInitialized(reSt)) {
            warning("initial value for 'reStruct' overwritten in 'nlme.nlsList'")
        }
        matrix(reSt) <- diag((madRan/madRes)^2, ncol = length(rnames))
    }
    thisCall[["start"]] <- start
    thisCall[["random"]] <- reSt
    val <- do.call(nlme.formula, thisCall)
    val$origCall <- match.call()
    val
}


Names <- function (object, ...) 
UseMethod("Names")


glsStruct <- function (corStruct = NULL, varStruct = NULL) 
{
    val <- list(corStruct = corStruct, varStruct = varStruct)
    val <- val[!sapply(val, is.null)]
    class(val) <- c("glsStruct", "modelStruct")
    val
}


needUpdate <- function (object) 
UseMethod("needUpdate")


getVarCov <- function (obj, ...) 
UseMethod("getVarCov")


splitFormula <- function (form, sep = "/") 
{
    if (inherits(form, "formula") || mode(form) == "call" && 
        form[[1]] == as.name("~")) 
        return(splitFormula(form[[length(form)]], sep = sep))
    if (mode(form) == "call" && form[[1]] == as.name(sep)) 
        return(do.call("c", lapply(as.list(form[-1]), splitFormula, 
            sep = sep)))
    if (mode(form) == "(") 
        return(splitFormula(form[[2]], sep = sep))
    if (length(form) < 1) 
        return(NULL)
    list(asOneSidedFormula(form))
}


lme.formula <- function (fixed, data = sys.frame(sys.parent()), random = pdSymm(eval(as.call(fixed[-2]))), 
    correlation = NULL, weights = NULL, subset, method = c("REML", 
        "ML"), na.action = na.fail, control = list(), contrasts = NULL, 
    keep.data = TRUE) 
{
    Call <- match.call()
    miss.data <- missing(data) || !is.data.frame(data)
    controlvals <- lmeControl()
    if (!missing(control)) {
        controlvals[names(control)] <- control
    }
    fixedSigma <- controlvals$sigma > 0
    if (!inherits(fixed, "formula") || length(fixed) != 3) {
        stop("\nfixed-effects model must be a formula of the form \"resp ~ pred\"")
    }
    method <- match.arg(method)
    REML <- method == "REML"
    reSt <- reStruct(random, REML = REML, data = NULL)
    groups <- getGroupsFormula(reSt)
    if (is.null(groups)) {
        if (inherits(data, "groupedData")) {
            groups <- getGroupsFormula(data)
            namGrp <- rev(names(getGroupsFormula(data, asList = TRUE)))
            Q <- length(namGrp)
            if (length(reSt) != Q) {
                if (length(reSt) != 1) {
                  stop("incompatible lengths for 'random' and grouping factors")
                }
                randL <- vector("list", Q)
                names(randL) <- rev(namGrp)
                for (i in 1:Q) randL[[i]] <- random
                reSt <- reStruct(as.list(randL), REML = REML, 
                  data = NULL)
            }
            else {
                names(reSt) <- namGrp
            }
        }
        else {
            groups <- ~1
            names(reSt) <- "1"
        }
    }
    if (!is.null(correlation)) {
        add.form <- FALSE
        if (!is.null(corGrpsForm <- getGroupsFormula(correlation, 
            asList = TRUE))) {
            corGrpsForm <- unlist(lapply(corGrpsForm, function(el) deparse(el[[2L]])))
            lmeGrpsForm <- unlist(lapply(splitFormula(groups), 
                function(el) deparse(el[[2L]])))
            corQ <- length(corGrpsForm)
            lmeQ <- length(lmeGrpsForm)
            if (corQ <= lmeQ) {
                if (any(corGrpsForm != lmeGrpsForm[1:corQ])) {
                  stop("incompatible formulas for groups in 'random' and 'correlation'")
                }
                if (corQ < lmeQ) {
                  warning("cannot use smaller level of grouping for 'correlation' than for 'random'. Replacing the former with the latter.")
                  add.form <- TRUE
                }
            }
            else if (any(lmeGrpsForm != corGrpsForm[1:lmeQ])) {
                stop("incompatible formulas for groups in 'random' and 'correlation'")
            }
        }
        else {
            add.form <- TRUE
            corQ <- lmeQ <- 1
        }
        if (add.form) 
            attr(correlation, "formula") <- eval(substitute(~COV | 
                GRP, list(COV = getCovariateFormula(formula(correlation))[[2L]], 
                GRP = groups[[2L]])))
    }
    else {
        corQ <- lmeQ <- 1
    }
    lmeSt <- lmeStruct(reStruct = reSt, corStruct = correlation, 
        varStruct = varFunc(weights))
    mfArgs <- list(formula = asOneFormula(formula(lmeSt), fixed, 
        groups), data = data, na.action = na.action)
    if (!missing(subset)) {
        mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2L]]
    }
    mfArgs$drop.unused.levels <- TRUE
    dataMix <- do.call(model.frame, mfArgs)
    origOrder <- row.names(dataMix)
    for (i in names(contrasts)) contrasts(dataMix[[i]]) = contrasts[[i]]
    grps <- getGroups(dataMix, groups)
    if (inherits(grps, "factor")) {
        ord <- order(grps)
        grps <- data.frame(grps)
        row.names(grps) <- origOrder
        names(grps) <- as.character(deparse((groups[[2L]])))
    }
    else {
        ord <- do.call(order, grps)
        for (i in 2:ncol(grps)) {
            grps[, i] <- as.factor(paste(as.character(grps[, 
                i - 1]), as.character(grps[, i]), sep = "/"))
        }
    }
    if (corQ > lmeQ) {
        ord <- do.call(order, getGroups(dataMix, getGroupsFormula(correlation)))
    }
    grps <- grps[ord, , drop = FALSE]
    dataMix <- dataMix[ord, , drop = FALSE]
    revOrder <- match(origOrder, row.names(dataMix))
    N <- nrow(grps)
    Z <- model.matrix(reSt, dataMix)
    ncols <- attr(Z, "ncols")
    Names(lmeSt$reStruct) <- attr(Z, "nams")
    contr <- attr(Z, "contr")
    X <- model.frame(fixed, dataMix)
    Terms <- attr(X, "terms")
    auxContr <- lapply(X, function(el) if (inherits(el, "factor") && 
        length(levels(el)) > 1) 
        contrasts(el))
    contr <- c(contr, auxContr[is.na(match(names(auxContr), names(contr)))])
    contr <- contr[!unlist(lapply(contr, is.null))]
    X <- model.matrix(fixed, data = X)
    y <- eval(fixed[[2L]], dataMix)
    ncols <- c(ncols, dim(X)[2L], 1)
    Q <- ncol(grps)
    attr(lmeSt, "conLin") <- list(Xy = array(c(Z, X, y), c(N, 
        sum(ncols)), list(row.names(dataMix), c(colnames(Z), 
        colnames(X), deparse(fixed[[2L]])))), dims = MEdims(grps, 
        ncols), logLik = 0, sigma = controlvals$sigma, auxSigma = 0)
    tmpDims <- attr(lmeSt, "conLin")$dims
    if (max(tmpDims$ZXlen[[1L]]) < tmpDims$qvec[1L]) {
        warning(gettextf("fewer observations than random effects in all level %s groups", 
            Q), domain = NA)
    }
    fixDF <- getFixDF(X, grps, attr(lmeSt, "conLin")$dims$ngrps, 
        terms = Terms)
    lmeSt <- Initialize(lmeSt, dataMix, grps, control = controlvals)
    parMap <- attr(lmeSt, "pmap")
    if (length(lmeSt) == 1) {
        oldConLin <- attr(lmeSt, "conLin")
        decomp <- TRUE
        attr(lmeSt, "conLin") <- MEdecomp(attr(lmeSt, "conLin"))
    }
    else decomp <- FALSE
    numIter <- 0
    repeat {
        oldPars <- coef(lmeSt)
        optRes <- if (controlvals$opt == "nlminb") {
            control <- list(iter.max = controlvals$msMaxIter, 
                eval.max = controlvals$msMaxEval, trace = controlvals$msVerbose)
            keep <- c("abs.tol", "rel.tol", "x.tol", "xf.tol", 
                "step.min", "step.max", "sing.tol", "scale.init", 
                "diff.g")
            control <- c(control, controlvals[names(controlvals) %in% 
                keep])
            nlminb(c(coef(lmeSt)), function(lmePars) -logLik(lmeSt, 
                lmePars), control = control)
        }
        else {
            reltol <- controlvals$reltol
            if (is.null(reltol)) 
                reltol <- 100 * .Machine$double.eps
            control <- list(trace = controlvals$msVerbose, maxit = controlvals$msMaxIter, 
                reltol = if (numIter == 0) controlvals$msTol else reltol)
            keep <- c("fnscale", "parscale", "ndeps", "abstol", 
                "alpha", "beta", "gamma", "REPORT", "type", "lmm", 
                "factr", "pgtol", "temp", "tmax")
            control <- c(control, controlvals[names(controlvals) %in% 
                keep])
            optim(c(coef(lmeSt)), function(lmePars) -logLik(lmeSt, 
                lmePars), control = control, method = controlvals$optimMethod)
        }
        coef(lmeSt) <- optRes$par
        attr(lmeSt, "lmeFit") <- MEestimate(lmeSt, grps)
        if (!needUpdate(lmeSt)) {
            if (optRes$convergence) {
                msg <- gettextf("%s problem, convergence error code = %s\n  message = %s", 
                  controlvals$opt, optRes$convergence, paste(optRes$message, 
                    collapse = ""))
                if (!controlvals$returnObject) 
                  stop(msg, domain = NA)
                else warning(msg, domain = NA)
            }
            break
        }
        numIter <- numIter + 1L
        lmeSt <- update(lmeSt, dataMix)
        aConv <- coef(lmeSt)
        conv <- abs((oldPars - aConv)/ifelse(aConv == 0, 1, aConv))
        aConv <- NULL
        for (i in names(lmeSt)) {
            if (any(parMap[, i])) {
                aConv <- c(aConv, max(conv[parMap[, i]]))
                names(aConv)[length(aConv)] <- i
            }
        }
        if (max(aConv) <= controlvals$tolerance) {
            break
        }
        if (numIter > controlvals$maxIter) {
            msg <- gettext("maximum number of iterations (lmeControl(maxIter)) reached without convergence")
            if (controlvals$returnObject) {
                warning(msg, domain = NA)
                break
            }
            else stop(msg, domain = NA)
        }
    }
    lmeFit <- attr(lmeSt, "lmeFit")
    names(lmeFit$beta) <- namBeta <- colnames(X)
    attr(fixDF, "varFixFact") <- varFix <- lmeFit$sigma * lmeFit$varFix
    varFix <- crossprod(varFix)
    dimnames(varFix) <- list(namBeta, namBeta)
    Fitted <- fitted(lmeSt, level = 0:Q, conLin = if (decomp) 
        oldConLin
    else attr(lmeSt, "conLin"))[revOrder, , drop = FALSE]
    Resid <- y[revOrder] - Fitted
    rownames(Resid) <- rownames(Fitted) <- origOrder
    attr(Resid, "std") <- lmeFit$sigma/(varWeights(lmeSt)[revOrder])
    grps <- grps[revOrder, , drop = FALSE]
    lmeSt$reStruct <- solve(lmeSt$reStruct)
    dims <- attr(lmeSt, "conLin")$dims[c("N", "Q", "qvec", "ngrps", 
        "ncol")]
    attr(lmeSt, "fixedSigma") <- fixedSigma
    apVar <- if (controlvals$apVar) {
        lmeApVar(lmeSt, lmeFit$sigma, .relStep = controlvals[[".relStep"]], 
            minAbsPar = controlvals[["minAbsParApVar"]], natural = controlvals[["natural"]])
    }
    else {
        "Approximate variance-covariance matrix not available"
    }
    attr(lmeSt, "conLin") <- NULL
    attr(lmeSt, "lmeFit") <- NULL
    grpDta <- inherits(data, "groupedData")
    structure(class = "lme", list(modelStruct = lmeSt, dims = dims, 
        contrasts = contr, coefficients = list(fixed = lmeFit$beta, 
            random = lmeFit$b), varFix = varFix, sigma = lmeFit$sigma, 
        apVar = apVar, logLik = lmeFit$logLik, numIter = if (needUpdate(lmeSt)) numIter, 
        groups = grps, call = Call, terms = Terms, method = method, 
        fitted = Fitted, residuals = Resid, fixDF = fixDF, na.action = attr(dataMix, 
            "na.action"), data = if (keep.data && !miss.data) data), 
        units = if (grpDta) 
            attr(data, "units"), labels = if (grpDta) 
            attr(data, "labels"))
}


nfGroupedData <- function (formula, data = sys.parent(1), order.groups = TRUE, 
    FUN = function(x) max(x, na.rm = TRUE), outer = NULL, inner = NULL, 
    labels = NULL, units = NULL) 
{
    if (!(inherits(formula, "formula") && length(formula) == 
        3)) {
        stop("first argument to 'nfGroupedData' must be a two-sided formula")
    }
    grpForm <- getGroupsFormula(formula, asList = TRUE)
    if (is.null(grpForm)) {
        stop("right-hand side of first argument must be a conditional expression")
    }
    if (length(grpForm) > 1) {
        stop("only one level of grouping allowed")
    }
    if (missing(data)) {
        vnames <- all.vars(asOneFormula(formula, inner, outer))
        alist <- lapply(as.list(vnames), as.name)
        names(alist) <- vnames
        data <- do.call("data.frame", alist)
    }
    else {
        if (!inherits(data, "data.frame")) {
            stop("second argument to 'groupedData' must inherit from data.frame")
        }
    }
    response <- getResponse(data, formula)
    primary <- getCovariate(data, formula)
    groupName <- names(grpForm)
    groups <- getGroups(data, formula)
    data[[groupName]] <- groups
    if (order.groups) {
        if (!inherits(groups, "ordered")) {
            if (is.null(outer)) {
                data[[groupName]] <- ordered(groups, levels = names(sort(tapply(response, 
                  groups, FUN))))
            }
            else {
                outer <- asOneSidedFormula(outer)
                combined <- do.call("paste", c(data[, all.vars(outer), 
                  drop = FALSE], sep = "\a"))
                levs <- as.vector(unlist(lapply(split(data.frame(response = response, 
                  groups = groups), combined), function(obj, 
                  func) {
                  names(sort(tapply(obj$response, obj$groups, 
                    func)))
                }, func = FUN)))
                data[[groupName]] <- ordered(groups, levels = levs)
            }
        }
    }
    attr(data, "formula") <- formula
    attr(data, "labels") <- labels
    attr(data, "units") <- units
    attr(data, "outer") <- outer
    attr(data, "inner") <- inner
    attr(data, "FUN") <- FUN
    attr(data, "order.groups") <- order.groups
    dClass <- unique(c("nfGroupedData", "groupedData", class(data)))
    if ((length(all.vars(getCovariateFormula(formula))) == 0) || 
        (data.class(primary) != "numeric")) {
        class(data) <- unique(c("nffGroupedData", dClass))
    }
    else {
        class(data) <- unique(c("nfnGroupedData", dClass))
    }
    data
}


getCovariate <- function (object, form = formula(object), data) 
UseMethod("getCovariate")


pdBlocked <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent()), 
    pdClass = "pdSymm") 
{
    object <- numeric(0)
    class(object) <- c("pdBlocked", "pdMat")
    pdConstruct(object, value, form, nam, data, pdClass)
}


gnls <- function (model, data = sys.frame(sys.parent()), params, start, 
    correlation = NULL, weights = NULL, subset, na.action = na.fail, 
    naPattern, control = list(), verbose = FALSE) 
{
    finiteDiffGrad <- function(model, data, pars) {
        dframe <- data.frame(data, pars)
        base <- eval(model, dframe)
        nm <- colnames(pars)
        grad <- array(base, c(length(base), length(nm)), list(NULL, 
            nm))
        ssize <- sqrt(.Machine$double.eps)
        for (i in nm) {
            diff <- pp <- pars[, i]
            diff[pp == 0] <- ssize
            diff[pp != 0] <- pp[pp != 0] * ssize
            dframe[[i]] <- pp + diff
            grad[, i] <- (base - eval(model, dframe))/diff
            dframe[[i]] <- pp
        }
        grad
    }
    Call <- match.call()
    form <- model
    controlvals <- gnlsControl()
    if (!missing(control)) {
        controlvals[names(control)] <- control
    }
    if (!inherits(form, "formula")) 
        stop("'object' must be a formula")
    if (length(form) != 3) 
        stop("object formula must be of the form \"resp ~ pred\"")
    if (missing(start)) {
        if (!is.null(attr(eval(form[[3]][[1]]), "initial"))) {
            nlsCall <- Call[c("", "model", "data")]
            nlsCall[[1]] <- quote(stats::nls)
            names(nlsCall)[2] <- "formula"
            if (is.null(dim(data))) {
                stop("'data' must be given explicitly to use 'nls' to get initial estimates")
            }
            start <- coef(eval(nlsCall))
        }
        else {
            stop("no initial values for model parameters")
        }
    }
    else {
        start <- unlist(start)
    }
    gnlsModel <- call("-", form[[2]], form[[3]])
    if (missing(params)) {
        if (is.null(pNams <- names(start))) {
            stop("starting estimates must have names when 'params' is missing")
        }
        params <- eval(parse(text = paste(paste(pNams, collapse = "+"), 
            "1", sep = "~")))
    }
    if (!is.list(params)) {
        params <- list(params)
    }
    val <- NULL
    for (i in seq_along(params)) {
        if (is.name(params[[i]][[2]])) {
            val <- c(val, list(params[[i]]))
        }
        else {
            val <- c(val, eval(parse(text = paste("list(", paste(paste(all.vars(params[[i]][[2]]), 
                deparse(params[[i]][[3]]), sep = "~"), collapse = ","), 
                ")"))))
        }
    }
    params <- as.list(val)
    pnames <- character(length(params))
    for (i in seq_along(params)) {
        this <- eval(params[[i]])
        if (!inherits(this, "formula")) 
            stop("'params' must be a formula or list of formulae")
        if (length(this) != 3) 
            stop("formulae in 'params' must be of the form \"parameter ~ expr\"")
        if (!is.name(this[[2]])) 
            stop("formulae in 'params' must be of the form \"parameter ~ expr\"")
        pnames[i] <- as.character(this[[2]])
    }
    names(params) <- pnames
    groups <- if (!is.null(correlation)) 
        getGroupsFormula(correlation)
    gnlsSt <- gnlsStruct(corStruct = correlation, varStruct = varFunc(weights))
    mfArgs <- list(formula = asOneFormula(formula(gnlsSt), form, 
        params, groups, omit = c(pnames, "pi")), data = data, 
        na.action = na.action)
    if (!missing(subset)) {
        mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2]]
    }
    mfArgs$drop.unused.levels <- TRUE
    dataMod <- do.call("model.frame", mfArgs)
    origOrder <- row.names(dataMod)
    if (!is.null(groups)) {
        groups <- eval(parse(text = paste("~1", deparse(groups[[2]]), 
            sep = "|")))
        grps <- getGroups(dataMod, groups, level = length(getGroupsFormula(groups, 
            asList = TRUE)))
        ord <- order(grps)
        grps <- grps[ord]
        dataMod <- dataMod[ord, , drop = FALSE]
    }
    else grps <- NULL
    N <- dim(dataMod)[1]
    naPat <- if (missing(naPattern)) 
        rep(TRUE, N)
    else as.logical(eval(asOneSidedFormula(naPattern)[[2]], dataMod))
    origOrderShrunk <- origOrder[naPat]
    dataModShrunk <- dataMod[naPat, , drop = FALSE]
    yShrunk <- eval(form[[2]], dataModShrunk)
    if (!is.null(groups)) {
        grpShrunk <- grps[naPat]
        revOrderShrunk <- match(origOrderShrunk, row.names(dataModShrunk))
    }
    else {
        grpShrunk <- NULL
    }
    contr <- list()
    plist <- vector("list", length(pnames))
    names(plist) <- pnames
    for (nm in pnames) {
        plist[[nm]] <- TRUE
        if (deparse(params[[nm]][[3]]) != "1") {
            plist[[nm]] <- model.matrix(asOneSidedFormula(params[[nm]][[3]]), 
                model.frame(asOneSidedFormula(params[[nm]][[3]]), 
                  dataModShrunk))
            auxContr <- attr(plist[[nm]], "contrasts")
            contr <- c(contr, auxContr[is.na(match(names(auxContr), 
                names(contr)))])
        }
    }
    pn <- character(0)
    currPos <- 0
    parAssign <- list()
    for (nm in pnames) {
        if (is.logical(p <- plist[[nm]])) {
            currPos <- currPos + 1
            currVal <- list(currPos)
            pn <- c(pn, nm)
            names(currVal) <- nm
            parAssign <- c(parAssign, currVal)
        }
        else {
            currVal <- attr(p, "assign")
            fTerms <- terms(asOneSidedFormula(params[[nm]][[3]]), 
                data = data)
            namTerms <- attr(fTerms, "term.labels")
            if (attr(fTerms, "intercept") > 0) {
                namTerms <- c("(Intercept)", namTerms)
            }
            namTerms <- factor(currVal, labels = namTerms)
            currVal <- split(order(currVal), namTerms)
            names(currVal) <- paste(nm, names(currVal), sep = ".")
            parAssign <- c(parAssign, lapply(currVal, function(el, 
                currPos) {
                el + currPos
            }, currPos = currPos))
            currPos <- currPos + length(unlist(currVal))
            pn <- c(pn, paste(nm, colnames(p), sep = "."))
        }
    }
    pLen <- length(pn)
    if (length(start) != pLen) 
        stop("starting values for parameters are not of the correct length")
    spar <- start
    names(spar) <- pn
    NReal <- sum(naPat)
    pmap <- list()
    n1 <- 1
    for (nm in pnames) {
        if (is.logical(p <- plist[[nm]])) {
            pmap[[nm]] <- n1
            n1 <- n1 + 1
        }
        else {
            pmap[[nm]] <- n1:(n1 + ncol(p) - 1)
            n1 <- n1 + ncol(p)
        }
    }
    nlEnv <- list2env(list(model = gnlsModel, data = dataMod, 
        plist = plist, beta = as.vector(spar), X = array(0, c(NReal, 
            pLen), list(NULL, pn)), pmap = pmap, N = NReal, naPat = naPat, 
        .parameters = c("beta"), finiteDiffGrad = finiteDiffGrad))
    modelExpression <- ~{
        pars <- getParsGnls(plist, pmap, beta, N)
        res <- eval(model, data.frame(data, pars))
        if (!length(grad <- attr(res, "gradient"))) {
            grad <- finiteDiffGrad(model, data, pars)[naPat, 
                , drop = FALSE]
        }
        else {
            grad <- grad[naPat, , drop = FALSE]
        }
        res <- res[naPat]
        for (nm in names(plist)) {
            gradnm <- grad[, nm]
            X[, pmap[[nm]]] <- if (is.logical(p <- plist[[nm]])) 
                gradnm
            else gradnm * p
        }
        result <- c(X, res)
        result[is.na(result)] <- 0
        result
    }
    modelResid <- ~eval(model, data.frame(data, getParsGnls(plist, 
        pmap, beta, N)))[naPat]
    w <- eval(modelResid[[2]], envir = nlEnv)
    fixedSigma <- controlvals$sigma > 0
    Dims <- list(p = pLen, N = NReal, REML = FALSE)
    attr(gnlsSt, "conLin") <- list(Xy = array(w, c(NReal, 1), 
        list(row.names(dataModShrunk), deparse(form[[2]]))), 
        dims = Dims, logLik = 0, sigma = controlvals$sigma, auxSigma = 0, 
        fixedSigma = fixedSigma)
    attr(gnlsSt, "resp") <- yShrunk
    attr(gnlsSt, "model") <- modelResid
    attr(gnlsSt, "local") <- nlEnv
    attr(gnlsSt, "NReal") <- NReal
    gnlsSt <- Initialize(gnlsSt, dataModShrunk)
    parMap <- attr(gnlsSt, "pmap")
    numIter <- 0
    nlsSettings <- c(controlvals$nlsMaxIter, controlvals$minScale, 
        controlvals$nlsTol, 0, 0, 0)
    nlModel <- nonlinModel(modelExpression, nlEnv)
    repeat {
        numIter <- numIter + 1
        if (needUpdate(gnlsSt)) {
            gnlsSt <- update(gnlsSt, dataModShrunk)
        }
        if (length(oldPars <- coef(gnlsSt)) > 0) {
            if (controlvals$opt == "nlminb") {
                optRes <- nlminb(c(coef(gnlsSt)), function(gnlsPars) -logLik(gnlsSt, 
                  gnlsPars), control = list(trace = controlvals$msVerbose, 
                  iter.max = controlvals$msMaxIter))
                convIter <- optRes$iterations
            }
            else {
                optRes <- optim(c(coef(gnlsSt)), function(gnlsPars) -logLik(gnlsSt, 
                  gnlsPars), method = controlvals$optimMethod, 
                  control = list(trace = controlvals$msVerbose, 
                    maxit = controlvals$msMaxIter, reltol = if (numIter == 
                      0) controlvals$msTol else 100 * .Machine$double.eps))
                convIter <- optRes$count[2]
            }
            aConv <- coef(gnlsSt) <- optRes$par
            if (verbose) {
                cat("\n**Iteration", numIter)
                cat("\n")
                cat("GLS step: Objective:", format(optRes$value))
                print(gnlsSt)
            }
        }
        else {
            aConv <- oldPars <- NULL
        }
        if (is.null(correlation)) {
            cF <- 1
            cD <- 1
        }
        else {
            cF <- corFactor(gnlsSt$corStruct)
            cD <- Dim(gnlsSt$corStruct)
        }
        if (is.null(weights)) {
            vW <- 1
        }
        else {
            vW <- varWeights(gnlsSt$varStruct)
        }
        work <- .C(fit_gnls, thetaNLS = as.double(spar), as.integer(unlist(Dims)), 
            as.double(cF), as.double(vW), as.integer(unlist(cD)), 
            settings = as.double(nlsSettings), additional = double(NReal), 
            as.integer(!is.null(correlation)), as.integer(!is.null(weights)), 
            nlModel, NAOK = TRUE)
        if (work$settings[4] == 1) {
            if (controlvals$returnObject) {
                warning("step halving factor reduced below minimum in NLS step")
            }
            else {
                stop("step halving factor reduced below minimum in NLS step")
            }
            break
        }
        oldPars <- c(spar, oldPars)
        spar[] <- work$thetaNLS
        if (length(coef(gnlsSt)) == 0 && work$set[5] < controlvals$nlsMaxIter) {
            break
        }
        attr(gnlsSt, "conLin")$Xy[] <- work$additional
        attr(gnlsSt, "conLin")$logLik <- 0
        if (verbose) {
            cat("\nNLS step: RSS = ", format(work$set[6]), "\n model parameters:")
            for (i in 1:pLen) cat(format(signif(spar[i])), " ")
            cat("\n iterations:", work$set[5], "\n")
        }
        aConv <- c(spar, aConv)
        conv <- abs((oldPars - aConv)/ifelse(abs(aConv) < controlvals$tolerance, 
            1, aConv))
        aConv <- c(max(conv[1:pLen]))
        names(aConv) <- "params"
        if (length(conv) > pLen) {
            conv <- conv[-(1:pLen)]
            for (i in names(gnlsSt)) {
                if (any(parMap[, i])) {
                  aConv <- c(aConv, max(conv[parMap[, i]]))
                  names(aConv)[length(aConv)] <- i
                }
            }
        }
        if (verbose) {
            cat("\nConvergence:\n")
            print(aConv)
        }
        if ((max(aConv) <= controlvals$tolerance) || (aConv["params"] <= 
            controlvals$tolerance && convIter == 1)) {
            break
        }
        if (numIter >= controlvals$maxIter) {
            if (controlvals$returnObject) {
                warning("maximum number of iterations reached without convergence")
                break
            }
            else {
                stop("maximum number of iterations reached without convergence")
            }
        }
    }
    ww <- eval(modelExpression[[2]], envir = nlEnv)
    auxRes <- ww[NReal * pLen + (1:NReal)]
    attr(gnlsSt, "conLin")$Xy <- array(ww, c(NReal, pLen + 1))
    attr(gnlsSt, "conLin") <- c.L <- recalc(gnlsSt)
    if ((sigma <- controlvals$sigma) == 0) {
        sigma <- sqrt(sum((c.L$Xy[, pLen + 1])^2)/(NReal - pLen))
        lsig <- logb(sigma) + 0.5 * logb(1 - pLen/NReal)
        loglik <- (-NReal * (1 + logb(2 * pi) + 2 * lsig))/2 + 
            c.L$logLik
    }
    else {
        loglik <- -(NReal * (logb(2 * pi)/2 + logb(sigma)) + 
            sum((c.L$Xy[, pLen + 1])^2)/(2 * sigma^2)) + c.L$logLik
        lsig <- log(sigma)
    }
    varBeta <- qr(c.L$Xy[, 1:pLen, drop = FALSE])
    if (varBeta$rank < pLen) {
        print("approximate covariance matrix for parameter estimates not of full rank")
        return()
    }
    attr(parAssign, "varBetaFact") <- varBeta <- sigma * t(backsolve(qr.R(varBeta), 
        diag(pLen)))
    varBeta <- crossprod(varBeta)
    dimnames(varBeta) <- list(pn, pn)
    Resid <- resid(gnlsSt)
    Fitted <- yShrunk - Resid
    attr(Resid, "std") <- sigma/(varWeights(gnlsSt))
    if (!is.null(groups)) {
        attr(Resid, "std") <- attr(Resid, "std")[revOrderShrunk]
        Resid[] <- Resid[revOrderShrunk]
        Fitted[] <- Fitted[revOrderShrunk]
        grpShrunk[] <- grpShrunk[revOrderShrunk]
    }
    names(Resid) <- names(Fitted) <- origOrderShrunk
    attr(gnlsSt, "conLin")$Xy <- array(auxRes, c(NReal, 1))
    attr(gnlsSt, "fixedSigma") <- (controlvals$sigma > 0)
    apVar <- if (controlvals$apVar) 
        gnlsApVar(gnlsSt, lsig, .relStep = controlvals[[".relStep"]], 
            minAbsPar = controlvals[["minAbsParApVar"]])
    else "Approximate variance-covariance matrix not available"
    oClass <- class(gnlsSt)
    attributes(gnlsSt) <- attributes(gnlsSt)[!is.na(match(names(attributes(gnlsSt)), 
        c("names", "pmap", "fixedSigma")))]
    class(gnlsSt) <- oClass
    grpDta <- inherits(data, "groupedData")
    structure(class = c("gnls", "gls"), list(modelStruct = gnlsSt, 
        dims = Dims, contrasts = contr, coefficients = spar, 
        varBeta = varBeta, sigma = if (controlvals$sigma) controlvals$sigma else sigma, 
        apVar = apVar, logLik = loglik, numIter = numIter, groups = grpShrunk, 
        call = Call, method = "ML", fitted = Fitted, residuals = Resid, 
        plist = plist, pmap = pmap, parAssign = parAssign, na.action = attr(dataMod, 
            "na.action")), units = if (grpDta) 
        attr(data, "units"), labels = if (grpDta) 
        attr(data, "labels"))
}


nlmeControl <- function (maxIter = 50, pnlsMaxIter = 7, msMaxIter = 50, minScale = 0.001, 
    tolerance = 1e-05, niterEM = 25, pnlsTol = 0.001, msTol = 1e-06, 
    returnObject = FALSE, msVerbose = FALSE, gradHess = TRUE, 
    apVar = TRUE, .relStep = .Machine$double.eps^(1/3), minAbsParApVar = 0.05, 
    opt = c("nlminb", "nlm"), natural = TRUE, sigma = NULL, ...) 
{
    if (is.null(sigma)) 
        sigma <- 0
    else if (!is.finite(sigma) || length(sigma) != 1 || sigma < 
        0) 
        stop("Within-group std. dev. must be a positive numeric value")
    list(maxIter = maxIter, pnlsMaxIter = pnlsMaxIter, msMaxIter = msMaxIter, 
        minScale = minScale, tolerance = tolerance, niterEM = niterEM, 
        pnlsTol = pnlsTol, msTol = msTol, returnObject = returnObject, 
        msVerbose = msVerbose, gradHess = gradHess, apVar = apVar, 
        .relStep = .relStep, minAbsParApVar = minAbsParApVar, 
        opt = match.arg(opt), natural = natural, sigma = sigma, 
        ...)
}


ranef <- function (object, ...) 
UseMethod("ranef")


varConstPower <- function (const = numeric(0), power = numeric(0), form = ~fitted(.), 
    fixed = NULL) 
{
    CPconstr <- function(val, form, nam) {
        if ((lv <- length(val)) == 0) 
            return(val)
        if (lv > 2) {
            stop(gettextf("%s can have at most two components", 
                nam), domain = NA)
        }
        if (is.null(nv <- names(val))) {
            names(val) <- c("const", "power")[1:lv]
        }
        else {
            if (any(is.na(match(nv, c("const", "power"))))) {
                stop(gettextf("%s can only have names \"const\" and \"power\"", 
                  nam), domain = NA)
            }
        }
        nv <- names(val)
        if (data.class(val) == "list") {
            val <- lapply(val, unlist)
            grpNames <- unique(unlist(lapply(val, names)))
        }
        else {
            if (!is.numeric(val)) {
                stop(gettextf("%s can only be a list or numeric", 
                  nam), domain = NA)
            }
            val <- as.list(val)
            names(val) <- nv
            grpNames <- NULL
        }
        if (!is.null(getGroupsFormula(form))) {
            if (any(unlist(lapply(val, function(el) {
                (length(el) > 1) && is.null(names(el))
            })))) {
                stop(gettextf("%s must have group names in 'varConstPower'", 
                  nam), domain = NA)
            }
            attr(val, "groupNames") <- grpNames
        }
        if (length(val$const) > 0) {
            if (any(val$const <= 0)) {
                stop("constant in \"varConstPower\" structure must be > 0")
            }
            val$const <- log(val$const)
        }
        list(const = val$const, power = val$power)
    }
    value <- list(const = const, power = power)
    form <- asOneSidedFormula(form)
    if (length(all.vars(getCovariateFormula(form))) == 0) {
        stop("'form' must have a covariate")
    }
    value <- CPconstr(value, form, "Value")
    fixed <- CPconstr(fixed, form, "Fixed")
    attr(value, "formula") <- form
    attr(value, "groupNames") <- unique(c(attr(value, "groupNames"), 
        attr(attr(value[["const"]], "fixed"), "groupNames"), 
        attr(attr(value[["power"]], "fixed"), "groupNames")))
    for (i in names(fixed)) {
        attr(value[[i]], "fixed") <- c(fixed[[i]])
    }
    if (is.null(getGroupsFormula(form))) {
        whichFix <- array(FALSE, c(2, 1), list(c("const", "power"), 
            NULL))
        whichFix[, 1] <- unlist(lapply(value, function(el) !is.null(attr(el, 
            "fixed"))))
        attr(value, "whichFix") <- whichFix
    }
    class(value) <- c("varConstPower", "varFunc")
    value
}


Variogram <- function (object, distance, ...) 
UseMethod("Variogram")


quinModel <- function (Subject, time, conc, dose, interval, lV, lKa, lCl) 
{
    .C(nlme_one_comp_open, as.integer(length(time)), resp = as.double(dose), 
        as.double(cbind(Subject, time, conc, dose, interval, 
            exp(lV), exp(lKa), exp(lCl - lV))), NAOK = TRUE)$resp
}


getCovariateFormula <- function (object) 
{
    form <- formula(object)
    if (!(inherits(form, "formula"))) {
        stop("formula(object) must return a formula")
    }
    form <- form[[length(form)]]
    if (length(form) == 3 && form[[1]] == as.name("|")) {
        form <- form[[2]]
    }
    eval(substitute(~form))
}


lmList <- function (object, data, level, subset, na.action = na.fail, pool = TRUE, 
    warn.lm = TRUE) 
UseMethod("lmList")


nlme.formula <- function (model, data = sys.frame(sys.parent()), fixed, random, 
    groups, start, correlation = NULL, weights = NULL, subset, 
    method = c("ML", "REML"), na.action = na.fail, naPattern, 
    control = list(), verbose = FALSE) 
{
    finiteDiffGrad <- function(model, data, pars) {
        dframe <- data.frame(data, pars)
        base <- eval(model, dframe)
        nm <- colnames(pars)
        grad <- array(base, c(length(base), length(nm)), list(NULL, 
            nm))
        ssize <- sqrt(.Machine$double.eps)
        for (i in nm) {
            diff <- pp <- pars[, i]
            diff[pp == 0] <- ssize
            diff[pp != 0] <- pp[pp != 0] * ssize
            dframe[[i]] <- pp + diff
            grad[, i] <- (base - eval(model, dframe))/diff
            dframe[[i]] <- pp
        }
        grad
    }
    Call <- match.call()
    controlvals <- nlmeControl()
    if (!missing(control)) {
        controlvals[names(control)] <- control
    }
    if (!inherits(model, "formula")) 
        stop("'model' must be a formula")
    if (length(model) != 3) 
        stop("model formula must be of the form \"resp ~ pred\"")
    method <- match.arg(method)
    REML <- method == "REML"
    if (missing(random)) {
        random <- fixed
    }
    reSt <- reStruct(random, REML = REML, data = NULL)
    if (missing(groups)) {
        groups <- getGroupsFormula(reSt)
    }
    if (is.null(groups)) {
        if (inherits(data, "groupedData")) {
            groups <- getGroupsFormula(data)
            namGrp <- rev(names(getGroupsFormula(data, asList = TRUE)))
            Q <- length(namGrp)
            if (length(reSt) != Q) {
                if (length(reSt) != 1) 
                  stop("incompatible lengths for 'random' and grouping factors")
                randL <- vector("list", Q)
                names(randL) <- rev(namGrp)
                for (i in 1:Q) randL[[i]] <- random
                reSt <- reStruct(randL, REML = REML, data = NULL)
            }
        }
        else {
            groups <- ~1
            names(reSt) <- namGrp <- "1"
        }
    }
    else {
        g.exp <- eval(parse(text = paste("~1", deparse(groups[[2]]), 
            sep = "|")))
        namGrp <- rev(names(getGroupsFormula(g.exp, asList = TRUE)))
    }
    names(reSt) <- namGrp
    if (missing(start) && !is.null(attr(eval(model[[3]][[1]]), 
        "initial"))) {
        nlmeCall <- Call
        nlsLCall <- nlmeCall[c("", "model", "data", "groups")]
        nlsLCall[[1]] <- quote(nlme::nlsList)
        names(nlsLCall)[2] <- "model"
        nm <- names(nlmeCall)
        for (i in c("fixed", "data", "groups", "start")) if (i %in% 
            nm) 
            nlmeCall[[i]] <- NULL
        nlmeCall[[1]] <- quote(nlme::nlme.nlsList)
        if (is.null(dim(data))) {
            stop("'data' must be given explicitly to use 'nlsList'")
        }
        nlsLObj <- eval(nlsLCall)
        nlmeCall[["model"]] <- quote(nlsLObj)
        nlmeCall <- as.call(nlmeCall)
        val <- eval(nlmeCall)
        val$origCall <- NULL
        return(val)
    }
    if (is.numeric(start)) {
        start <- list(fixed = start)
    }
    nlmeModel <- call("-", model[[2]], model[[3]])
    if (!is.list(fixed)) 
        fixed <- list(fixed)
    fixed <- do.call(c, lapply(fixed, function(fix.i) {
        if (is.name(fix.i[[2]])) 
            list(fix.i)
        else eval(parse(text = paste0("list(", paste(paste(all.vars(fix.i[[2]]), 
            deparse(fix.i[[3]]), sep = "~"), collapse = ","), 
            ")")))
    }))
    fnames <- lapply(fixed, function(fix.i) {
        this <- eval(fix.i)
        if (!inherits(this, "formula")) 
            stop("'fixed' must be a formula or list of formulae")
        if (length(this) != 3) 
            stop("formulae in 'fixed' must be of the form \"parameter ~ expr\"")
        if (!is.name(this[[2]])) 
            stop("formulae in 'fixed' must be of the form \"parameter ~ expr\"")
        as.character(this[[2]])
    })
    names(fixed) <- fnames
    ranForm <- formula(reSt)
    Q <- length(ranForm)
    names(ranForm) <- namGrp
    rnames <- vector("list", Q)
    names(rnames) <- namGrp
    for (i in 1:Q) {
        rnames[[i]] <- character(length(ranForm[[i]]))
        for (j in seq_along(ranForm[[i]])) {
            this <- eval(ranForm[[i]][[j]])
            if (!inherits(this, "formula")) 
                stop("'random' must be a formula or list of formulae")
            if (length(this) != 3) 
                stop("formulae in 'random' must be of the form \"parameter ~ expr\"")
            if (!is.name(this[[2]])) 
                stop("formulae in 'random' must be of the form \"parameter ~ expr\"")
            rnames[[i]][j] <- deparse(this[[2]])
        }
        names(ranForm[[i]]) <- rnames[[i]]
    }
    pnames <- unique(c(fnames, unlist(rnames)))
    if (!is.null(correlation)) {
        if (!is.null(corGrpsForm <- getGroupsFormula(correlation, 
            asList = TRUE))) {
            corGrpsForm <- unlist(lapply(corGrpsForm, function(el) deparse(el[[2]])))
            corQ <- length(corGrpsForm)
            lmeGrpsForm <- unlist(lapply(splitFormula(groups), 
                function(el) deparse(el[[2]])))
            lmeQ <- length(lmeGrpsForm)
            if (corQ <= lmeQ) {
                if (any(corGrpsForm != lmeGrpsForm[1:corQ])) {
                  stop("incompatible formulas for groups in \"random\" and \"correlation\"")
                }
                if (corQ < lmeQ) {
                  warning("cannot use smaller level of grouping for \"correlation\" than for \"random\". Replacing the former with the latter.")
                  frm <- paste("~", c_deparse(getCovariateFormula(formula(correlation))[[2]]), 
                    "|", deparse(groups[[2]]))
                  attr(correlation, "formula") <- eval(parse(text = frm))
                }
            }
            else {
                if (any(lmeGrpsForm != corGrpsForm[1:lmeQ])) {
                  stop("incompatible formulas for groups in \"random\" and \"correlation\"")
                }
            }
        }
        else {
            frm <- paste("~", c_deparse(getCovariateFormula(formula(correlation))[[2]]), 
                "|", deparse(groups[[2]]))
            attr(correlation, "formula") <- eval(parse(text = frm))
            corQ <- lmeQ <- 1
        }
    }
    else {
        corQ <- lmeQ <- 1
    }
    nlmeSt <- nlmeStruct(reStruct = reSt, corStruct = correlation, 
        varStruct = varFunc(weights))
    mfArgs <- list(formula = asOneFormula(formula(nlmeSt), model, 
        fixed, groups, omit = c(pnames, "pi")), data = data, 
        na.action = na.action)
    if (!missing(subset)) {
        mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2]]
    }
    mfArgs$drop.unused.levels <- TRUE
    dataMix <- do.call(model.frame, mfArgs)
    origOrder <- row.names(dataMix)
    grps <- getGroups(dataMix, eval(parse(text = paste("~1", 
        deparse(groups[[2]]), sep = "|"))))
    N <- dim(dataMix)[1]
    if (missing(naPattern)) 
        naPat <- rep(TRUE, N)
    else naPat <- as.logical(eval(asOneSidedFormula(naPattern)[[2]], 
        dataMix))
    origOrderShrunk <- origOrder[naPat]
    if (inherits(grps, "factor")) {
        ord <- order(grps)
        grps <- data.frame(grps)
        row.names(grps) <- origOrder
        names(grps) <- as.character(deparse((groups[[2]])))
    }
    else {
        ord <- do.call(order, grps)
        for (i in 2:ncol(grps)) {
            grps[, i] <- as.factor(paste(as.character(grps[, 
                i - 1]), as.character(grps[, i]), sep = "/"))
        }
    }
    if (corQ > lmeQ) {
        ord <- do.call(order, getGroups(dataMix, getGroupsFormula(correlation)))
    }
    grps <- grps[ord, , drop = FALSE]
    dataMix <- dataMix[ord, , drop = FALSE]
    naPat <- naPat[ord]
    dataMixShrunk <- dataMix[naPat, , drop = FALSE]
    grpShrunk <- grps[naPat, , drop = FALSE]
    revOrderShrunk <- match(origOrderShrunk, row.names(dataMixShrunk))
    yShrunk <- eval(model[[2]], dataMixShrunk)
    contr <- list()
    plist <- vector("list", length(pnames))
    names(plist) <- pnames
    for (nm in pnames) {
        this <- list(fixed = !is.null(fixed[[nm]]), random = as.list(lapply(ranForm, 
            function(el, nm) !is.null(el[[nm]]), nm = nm)))
        if (this[["fixed"]]) {
            if (fixed[[nm]][[3]] != "1") {
                as1F <- asOneSidedFormula(fixed[[nm]][[3]])
                this[["fixed"]] <- model.matrix(as1F, model.frame(as1F, 
                  dataMix))
                auxContr <- attr(this[["fixed"]], "contrasts")
                contr <- c(contr, auxContr[is.na(match(names(auxContr), 
                  names(contr)))])
            }
        }
        if (any(unlist(this[["random"]]))) {
            for (i in 1:Q) {
                wch <- which(!is.na(match(rnames[[i]], nm)))
                if (length(wch) == 1) {
                  if ((rF.i <- ranForm[[i]][[nm]][[3]]) != "1") {
                    this[["random"]][[i]] <- model.matrix(asOneSidedFormula(rF.i), 
                      model.frame(asOneSidedFormula(rF.i), dataMix))
                    auxContr <- attr(this[["random"]][[i]], "contrasts")
                    contr <- c(contr, auxContr[is.na(match(names(auxContr), 
                      names(contr)))])
                  }
                }
                else if (length(wch) > 0) {
                  this[["random"]][[i]] <- th.ran.i <- as.list(lapply(ranForm[[i]][wch], 
                    function(el, data) {
                      if (el[[3]] == "1") 
                        TRUE
                      else model.matrix(asOneSidedFormula(el[[3]]), 
                        model.frame(asOneSidedFormula(el[[3]]), 
                          data))
                    }, data = dataMix))
                  for (j in seq_along(th.ran.i)) {
                    if (is.matrix(th.ran.i[[j]])) {
                      auxContr <- attr(th.ran.i[[j]], "contrasts")
                      contr <- c(contr, auxContr[is.na(match(names(auxContr), 
                        names(contr)))])
                    }
                  }
                }
            }
        }
        plist[[nm]] <- this
    }
    contrMat <- function(nm, contr, data) {
        x <- eval(parse(text = nm), data)
        levs <- levels(x)
        val <- do.call(contr[[nm]], list(n = length(levs)))
        rownames(val) <- levs
        val
    }
    nms <- names(contr)[sapply(contr, is.character)]
    contr[nms] <- lapply(nms, contrMat, contr = contr, data = dataMix)
    if (is.null(sfix <- start$fixed)) 
        stop("'start' must have a component called 'fixed'")
    fn <- character(0)
    currPos <- 0
    fixAssign <- list()
    for (nm in fnames) {
        if (is.logical(f <- plist[[nm]]$fixed)) {
            currPos <- currPos + 1
            currVal <- list(currPos)
            if (all(unlist(lapply(plist[[nm]]$random, is.logical)))) {
                fn <- c(fn, nm)
                names(currVal) <- nm
            }
            else {
                aux <- paste(nm, "(Intercept)", sep = ".")
                fn <- c(fn, aux)
                names(currVal) <- aux
            }
            fixAssign <- c(fixAssign, currVal)
        }
        else {
            currVal <- attr(f, "assign")
            fTerms <- terms(asOneSidedFormula(fixed[[nm]][[3]]), 
                data = data)
            namTerms <- attr(fTerms, "term.labels")
            if (attr(fTerms, "intercept") > 0) {
                namTerms <- c("(Intercept)", namTerms)
            }
            namTerms <- factor(currVal, labels = namTerms)
            currVal <- split(order(currVal), namTerms)
            names(currVal) <- paste(nm, names(currVal), sep = ".")
            fixAssign <- c(fixAssign, lapply(currVal, function(el) el + 
                currPos))
            currPos <- currPos + length(unlist(currVal))
            fn <- c(fn, paste(nm, colnames(f), sep = "."))
        }
    }
    fLen <- length(fn)
    if (length(sfix) != fLen) 
        stop("starting values for the 'fixed' component are not the correct length")
    names(sfix) <- fn
    rn <- wchRnames <- vector("list", Q)
    names(rn) <- names(wchRnames) <- namGrp
    for (i in 1:Q) {
        rn[[i]] <- character(0)
        uRnames <- unique(rnames[[i]])
        wchRnames[[i]] <- integer(length(uRnames))
        names(wchRnames[[i]]) <- uRnames
        for (j in seq_along(rnames[[i]])) {
            nm <- rnames[[i]][j]
            wchRnames[[i]][nm] <- wchRnames[[i]][nm] + 1
            r <- plist[[nm]]$random[[i]]
            if (data.class(r) == "list") 
                r <- r[[wchRnames[[i]][nm]]]
            if (is.logical(r)) {
                if (r) {
                  rn[[i]] <- c(rn[[i]], if (is.logical(plist[[nm]]$fixed)) nm else paste(nm, 
                    "(Intercept)", sep = "."))
                }
            }
            else {
                rn[[i]] <- c(rn[[i]], paste(nm, colnames(r), 
                  sep = "."))
            }
        }
    }
    Names(nlmeSt$reStruct) <- rn
    rNam <- unlist(rn)
    rlength <- lengths(rn)
    rLen <- sum(rlength)
    pLen <- rLen + fLen
    ncols <- c(rlength, fLen, 1)
    Dims <- MEdims(grpShrunk, ncols)
    if (max(Dims$ZXlen[[1]]) < Dims$qvec[1]) {
        warning(gettextf("fewer observations than random effects in all level %s groups", 
            Q), domain = NA)
    }
    sran <- vector("list", Q)
    names(sran) <- namGrp
    if (!is.null(sran0 <- start$random)) {
        if (inherits(sran0, "data.frame")) {
            sran0 <- list(as.matrix(sran0))
        }
        else {
            if (!is.list(sran0)) {
                if (!is.matrix(sran0)) {
                  stop("starting values for random effects should be a list, or a matrix")
                }
                sran0 <- list(as.matrix(sran0))
            }
        }
        if (is.null(namSran <- names(sran0))) {
            if (length(sran) != Q) {
                stop(gettextf("list with starting values for random effects must have names or be of length %d", 
                  Q), domain = NA)
            }
            names(sran0) <- rev(namGrp)
        }
        else {
            if (any(noMatch <- is.na(match(namSran, namGrp)))) {
                stop(sprintf(ngettext(sum(noMatch), "group name not matched in starting values for random effects: %s", 
                  "group names not matched in starting values for random effects: %s"), 
                  paste(namSran[noMatch], collapse = ", ")), 
                  domain = NA)
            }
        }
    }
    for (i in 1:Q) {
        if (is.null(sran[[i]] <- sran0[[namGrp[i]]])) {
            sran[[i]] <- array(0, c(rlength[i], Dims$ngrps[i]), 
                list(rn[[i]], unique(as.character(grps[, Q - 
                  i + 1]))))
        }
        else {
            if (!is.matrix(sran[[i]])) 
                stop("starting values for the random components should be a list of matrices")
            dimsran <- dim(sran[[i]])
            if (dimsran[1] != Dims$ngrps[i]) {
                stop(gettextf("number of rows in starting values for random component at level %s should be %d", 
                  namGrp[i], Dims$ngrps[i]), domain = NA)
            }
            if (dimsran[2] != rlength[i]) {
                stop(gettextf("number of columns in starting values for random component at level %s should be %d", 
                  namGrp[i], rlength[i]), domain = NA)
            }
            dnamesran <- dimnames(sran[[i]])
            if (is.null(dnamesran[[1]])) {
                stop("starting values for random effects must include group levels")
            }
            else {
                levGrps <- unique(as.character(grps[, Q - i + 
                  1]))
                if (!all(sort(dnamesran[[1]]) == sort(levGrps))) {
                  stop(gettextf("groups levels mismatch in 'random' and starting values for 'random' at level %s", 
                    namGrp[i]), domain = NA)
                }
                sran[[i]] <- sran[[i]][levGrps, , drop = FALSE]
            }
            if (!is.null(dnamesran[[2]])) {
                if (!all(sort(dnamesran[[2]]) == sort(rn[[i]]))) {
                  for (j in seq_len(rlength[i])) {
                    if (is.na(match(dnamesran[[2]][j], rn[[i]]))) {
                      if (!is.na(mDn <- match(paste(dnamesran[[2]][j], 
                        "(Intercept)", sep = "."), rn[[i]]))) {
                        dnamesran[[2]][j] <- rn[[i]][mDn]
                      }
                      else {
                        if (!is.na(mDn <- match(dnamesran[[2]][j], 
                          paste(rn[[i]], "(Intercept)", sep = ".")))) {
                          dnamesran[[2]][j] <- rn[[i]][mDn]
                        }
                        else {
                          stop(gettextf("names mismatch in 'random' and starting values  for 'random' at level %s", 
                            namGrp[i]), domain = NA)
                        }
                      }
                    }
                  }
                  dimnames(sran[[i]]) <- dnamesran
                }
                sran[[i]] <- sran[[i]][, rn[[i]], drop = FALSE]
            }
            else {
                dimnames(sran[[i]])[[2]] <- rn[[i]]
            }
            sran[[i]] <- t(sran[[i]])
        }
    }
    names(sran) <- namGrp
    nPars <- length(unlist(sran)) + fLen
    NReal <- sum(naPat)
    fmap <- list()
    n1 <- 1
    for (nm in fnames) {
        if (is.logical(f <- plist[[nm]]$fixed)) {
            fmap[[nm]] <- n1
            n1 <- n1 + 1
        }
        else {
            fmap[[nm]] <- n1:(n1 + ncol(f) - 1)
            n1 <- n1 + ncol(f)
        }
    }
    rmap <- rmapRel <- vector("list", Q)
    names(rmap) <- names(rmapRel) <- namGrp
    n1 <- 1
    startRan <- 0
    for (i in 1:Q) {
        wchRnames[[i]][] <- 0
        rmap[[i]] <- rmapRel[[i]] <- list()
        for (nm in rnames[[i]]) {
            wchRnames[[i]][nm] <- wchRnames[[i]][nm] + 1
            r <- plist[[nm]]$random[[i]]
            if (data.class(r) == "list") {
                r <- r[[wchRnames[[i]][nm]]]
            }
            if (is.logical(r)) {
                val <- n1
                n1 <- n1 + 1
            }
            else {
                val <- n1:(n1 + ncol(r) - 1)
                n1 <- n1 + ncol(r)
            }
            if (is.null(rmap[[i]][[nm]])) {
                rmap[[i]][[nm]] <- val
                rmapRel[[i]][[nm]] <- val - startRan
            }
            else {
                rmap[[i]][[nm]] <- c(rmap[[i]][[nm]], list(val))
                rmapRel[[i]][[nm]] <- c(rmapRel[[i]][[nm]], list(val - 
                  startRan))
            }
        }
        startRan <- startRan + ncols[i]
    }
    grpsRev <- rev(lapply(grps, as.character))
    bmap <- c(0, cumsum(unlist(lapply(sran, function(el) length(as.vector(el))))))
    nlEnv <- list2env(list(model = nlmeModel, data = dataMix, 
        groups = grpsRev, plist = plist, beta = as.vector(sfix), 
        bvec = unlist(sran), b = sran, X = array(0, c(N, fLen), 
            list(NULL, fn)), Z = array(0, c(N, rLen), list(NULL, 
            rNam)), fmap = fmap, rmap = rmap, rmapRel = rmapRel, 
        bmap = bmap, level = Q, N = N, Q = Q, naPat = naPat, 
        .parameters = c("bvec", "beta"), finiteDiffGrad = finiteDiffGrad))
    modelExpression <- ~{
        pars <- getParsNlme(plist, fmap, rmapRel, bmap, groups, 
            beta, bvec, b, level, N)
        res <- eval(model, data.frame(data, pars))
        if (!length(grad <- attr(res, "gradient"))) {
            grad <- finiteDiffGrad(model, data, pars)
        }
        for (nm in names(plist)) {
            gradnm <- grad[, nm]
            if (is.logical(f <- plist[[nm]]$fixed)) {
                if (f) 
                  X[, fmap[[nm]]] <- gradnm
            }
            else X[, fmap[[nm]]] <- gradnm * f
            for (i in 1:Q) {
                if (is.logical(r <- plist[[nm]]$random[[i]])) {
                  if (r) 
                    Z[, rmap[[i]][[nm]]] <- gradnm
                }
                else {
                  rm.i <- rmap[[i]][[nm]]
                  if (data.class(rm.i) != "list") {
                    Z[, rm.i] <- gradnm * r
                  }
                  else {
                    for (j in seq_along(rm.i)) {
                      Z[, rm.i[[j]]] <- if (is.logical(rr <- r[[j]])) 
                        gradnm
                      else gradnm * rr
                    }
                  }
                }
            }
        }
        result <- c(Z[naPat, ], X[naPat, ], res[naPat])
        result[is.na(result)] <- 0
        result
    }
    modelResid <- ~eval(model, data.frame(data, getParsNlme(plist, 
        fmap, rmapRel, bmap, groups, beta, bvec, b, level, N)))[naPat]
    ww <- eval(modelExpression[[2]], envir = nlEnv)
    w <- ww[NReal * pLen + (1:NReal)]
    ZX <- array(ww[1:(NReal * pLen)], c(NReal, pLen), list(row.names(dataMixShrunk), 
        c(rNam, fn)))
    w <- w + as.vector(ZX[, rLen + (1:fLen), drop = FALSE] %*% 
        sfix)
    if (!is.null(start$random)) {
        startRan <- 0
        for (i in 1:Q) {
            w <- w + as.vector((ZX[, startRan + 1:ncols[i], drop = FALSE] * 
                t(sran[[i]])[as.character(grpShrunk[, Q - i + 
                  1]), , drop = FALSE]) %*% rep(1, ncols[i]))
            startRan <- startRan + ncols[i]
        }
    }
    attr(nlmeSt, "conLin") <- list(Xy = array(c(ZX, w), dim = c(NReal, 
        sum(ncols)), dimnames = list(row.names(dataMixShrunk), 
        c(colnames(ZX), deparse(model[[2]])))), dims = Dims, 
        logLik = 0, sigma = controlvals$sigma, auxSigma = 0)
    attr(nlmeSt, "resp") <- yShrunk
    attr(nlmeSt, "model") <- modelResid
    attr(nlmeSt, "local") <- nlEnv
    attr(nlmeSt, "NReal") <- NReal
    nlmeSt <- Initialize(nlmeSt, dataMixShrunk, grpShrunk, control = controlvals)
    parMap <- attr(nlmeSt, "pmap")
    decomp <- length(coef(nlmeSt)) == length(coef(nlmeSt$reStruct)) && 
        !needUpdate(nlmeSt)
    if (decomp) {
        oldConLin <- attr(nlmeSt, "conLin")
    }
    numIter <- 0
    pnlsSettings <- c(controlvals$pnlsMaxIter, controlvals$minScale, 
        controlvals$pnlsTol, 0, 0, 0)
    nlModel <- nonlinModel(modelExpression, nlEnv)
    repeat {
        numIter <- numIter + 1
        if (needUpdate(nlmeSt)) {
            nlmeSt <- update(nlmeSt, dataMixShrunk)
        }
        if (decomp) {
            attr(nlmeSt, "conLin") <- MEdecomp(oldConLin)
        }
        oldPars <- coef(nlmeSt)
        if (controlvals$opt == "nlminb") {
            control <- list(trace = controlvals$msVerbose, iter.max = controlvals$msMaxIter)
            keep <- c("eval.max", "abs.tol", "rel.tol", "x.tol", 
                "xf.tol", "step.min", "step.max", "sing.tol", 
                "scale.init", "diff.g")
            control <- c(control, controlvals[names(controlvals) %in% 
                keep])
            optRes <- nlminb(c(coef(nlmeSt)), function(nlmePars) -logLik(nlmeSt, 
                nlmePars), control = control)
            aConv <- coef(nlmeSt) <- optRes$par
            convIter <- optRes$iterations
        }
        else {
            aNlm <- nlm(f = function(nlmePars) -logLik(nlmeSt, 
                nlmePars), p = c(coef(nlmeSt)), hessian = TRUE, 
                print.level = controlvals$msVerbose, gradtol = if (numIter == 
                  1) 
                  controlvals$msTol
                else 100 * .Machine$double.eps, iterlim = if (numIter < 
                  10) 
                  10
                else controlvals$msMaxIter, check.analyticals = FALSE)
            aConv <- coef(nlmeSt) <- aNlm$estimate
            convIter <- aNlm$iterations
        }
        nlmeFit <- attr(nlmeSt, "lmeFit") <- MEestimate(nlmeSt, 
            grpShrunk)
        if (verbose) {
            cat("\n**Iteration", numIter)
            cat(sprintf("\nLME step: Loglik: %s, %s iterations: %d\n", 
                format(nlmeFit$logLik), controlvals$opt, convIter))
            print(nlmeSt)
        }
        if (is.null(correlation)) {
            cF <- 1
            cD <- 1
        }
        else {
            cF <- corFactor(nlmeSt$corStruct)
            cD <- Dim(nlmeSt$corStruct)
        }
        vW <- if (is.null(weights)) 
            1
        else varWeights(nlmeSt$varStruct)
        work <- .C(fit_nlme, thetaPNLS = as.double(c(as.vector(unlist(sran)), 
            sfix)), pdFactor = as.double(pdFactor(nlmeSt$reStruct)), 
            as.integer(unlist(rev(grpShrunk))), as.integer(unlist(Dims)), 
            as.integer(attr(nlmeSt$reStruct, "settings"))[-(1:3)], 
            as.double(cF), as.double(vW), as.integer(unlist(cD)), 
            settings = as.double(pnlsSettings), additional = double(NReal * 
                (pLen + 1)), as.integer(!is.null(correlation)), 
            as.integer(!is.null(weights)), as.double(controlvals$sigma), 
            nlModel, NAOK = TRUE)
        if (work$settings[4] == 1) {
            msg <- "step halving factor reduced below minimum in PNLS step"
            if (controlvals$returnObject) 
                warning(msg)
            else stop(msg)
        }
        i.pdF <- 1
        for (i in seq_along(nlmeSt$reStruct)) {
            d.i <- dim(pdMatrix(nlmeSt$reStruct[[i]]))
            ni.pdF <- i.pdF + prod(d.i)
            pdF <- array(work$pdFactor[i.pdF:(ni.pdF - 1)], dim = d.i)
            matrix(nlmeSt$reStruct[[i]]) <- crossprod(pdF)
            i.pdF <- ni.pdF
        }
        oldPars <- c(sfix, oldPars)
        for (i in 1:Q) sran[[i]][] <- work$thetaPNLS[(bmap[i] + 
            1):bmap[i + 1]]
        sfix[] <- work$thetaPNLS[nPars + 1 - (fLen:1)]
        if (verbose) {
            cat("PNLS step: RSS = ", format(work$settings[6]), 
                "\n fixed effects: ")
            for (i in 1:fLen) cat(format(sfix[i]), " ")
            cat("\n iterations:", work$settings[5], "\n")
        }
        aConv <- c(sfix, coef(nlmeSt))
        w[] <- work$additional[(NReal * pLen) + 1:NReal]
        ZX[] <- work$additional[1:(NReal * pLen)]
        w <- w + as.vector(ZX[, rLen + (1:fLen), drop = FALSE] %*% 
            sfix)
        startRan <- 0
        for (i in 1:Q) {
            w <- w + as.vector((ZX[, startRan + 1:ncols[i], drop = FALSE] * 
                t(sran[[i]])[as.character(grpShrunk[, Q - i + 
                  1]), , drop = FALSE]) %*% rep(1, ncols[i]))
            startRan <- startRan + ncols[i]
        }
        if (decomp) {
            oldConLin$Xy[] <- c(ZX, w)
            oldConLin$logLik <- 0
        }
        else {
            attr(nlmeSt, "conLin")$Xy[] <- c(ZX, w)
            attr(nlmeSt, "conLin")$logLik <- 0
        }
        conv <- abs((oldPars - aConv)/ifelse(abs(aConv) < controlvals$tolerance, 
            1, aConv))
        aConv <- c(fixed = max(conv[1:fLen]))
        conv <- conv[-(1:fLen)]
        for (i in names(nlmeSt)) {
            if (any(parMap[, i])) {
                aConv <- c(aConv, max(conv[parMap[, i]]))
                names(aConv)[length(aConv)] <- i
            }
        }
        if (verbose) {
            cat(sprintf("Convergence crit. (must all become <= tolerance = %g):\n", 
                controlvals$tolerance))
            print(aConv)
        }
        if ((max(aConv) <= controlvals$tolerance) || (aConv["fixed"] <= 
            controlvals$tolerance && convIter == 1)) {
            break
        }
        if (numIter >= controlvals$maxIter) {
            msg <- gettextf("maximum number of iterations (maxIter = %d) reached without convergence", 
                controlvals$maxIter)
            if (controlvals$returnObject) {
                warning(msg, domain = NA)
                break
            }
            else {
                stop(msg, domain = NA)
            }
        }
    }
    nlmeFit <- if (decomp) 
        MEestimate(nlmeSt, grpShrunk, oldConLin)
    else MEestimate(nlmeSt, grpShrunk)
    fixDF <- getFixDF(ZX[, rLen + (1:fLen), drop = FALSE], grpShrunk, 
        attr(nlmeSt, "conLin")$dims$ngrps, fixAssign)
    attr(fixDF, "varFixFact") <- varFix <- nlmeFit$sigma * nlmeFit$varFix
    varFix <- crossprod(varFix)
    dimnames(varFix) <- list(fn, fn)
    Resid <- if (decomp) 
        resid(nlmeSt, level = 0:Q, oldConLin)[revOrderShrunk, 
            ]
    else resid(nlmeSt, level = 0:Q)[revOrderShrunk, ]
    Fitted <- yShrunk[revOrderShrunk] - Resid
    rownames(Resid) <- rownames(Fitted) <- origOrderShrunk
    grpShrunk <- grpShrunk[revOrderShrunk, , drop = FALSE]
    attr(Resid, "std") <- nlmeFit$sigma/(varWeights(nlmeSt)[revOrderShrunk])
    nlmeSt$reStruct <- solve(nlmeSt$reStruct)
    attr(nlmeSt, "fixedSigma") <- (controlvals$sigma > 0)
    dims <- attr(nlmeSt, "conLin")$dims[c("N", "Q", "qvec", "ngrps", 
        "ncol")]
    apVar <- if (controlvals$apVar) 
        lmeApVar(nlmeSt, nlmeFit$sigma, .relStep = controlvals[[".relStep"]], 
            minAbsPar = controlvals[["minAbsParApVar"]], natural = controlvals[["natural"]])
    else "Approximate variance-covariance matrix not available"
    sran <- lapply(sran, t)
    attributes(nlmeSt) <- attributes(nlmeSt)[c("names", "class", 
        "pmap", "fixedSigma")]
    isGrpd <- inherits(data, "groupedData")
    structure(class = c("nlme", "lme"), list(modelStruct = nlmeSt, 
        dims = dims, contrasts = contr, coefficients = list(fixed = sfix, 
            random = rev(sran)), varFix = varFix, sigma = nlmeFit$sigma, 
        apVar = apVar, logLik = nlmeFit$logLik, numIter = numIter, 
        groups = grpShrunk, call = Call, method = method, fitted = Fitted, 
        residuals = Resid, plist = plist, map = list(fmap = fmap, 
            rmap = rmap, rmapRel = rmapRel, bmap = bmap), fixDF = fixDF), 
        units = if (isGrpd) 
            attr(data, "units"), labels = if (isGrpd) 
            attr(data, "labels"))
}


asTable <- function (object) 
UseMethod("asTable")


fixed.effects <- function (object, ...) 
UseMethod("fixef")


gnlsStruct <- function (corStruct = NULL, varStruct = NULL) 
{
    val <- list(corStruct = corStruct, varStruct = varStruct)
    val <- val[!sapply(val, is.null)]
    class(val) <- c("gnlsStruct", "glsStruct", "modelStruct")
    val
}


allCoef <- function (..., extract = coef) 
{
    dots <- list(...)
    theta <- lapply(dots, extract)
    len <- lengths(theta)
    num <- seq_along(len)
    which <- if (sum(len) > 0) 
        outer(rep(num, len), num, "==")
    else array(FALSE, c(1, length(len)))
    cnames <- unlist(as.list(sys.call()[-1]))
    dimnames(which) <- list(NULL, cnames[cnames != substitute(extract)])
    theta <- unlist(theta)
    attr(theta, "map") <- which
    theta
}


corGaus <- function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", 
    "maximum", "manhattan"), fixed = FALSE) 
{
    attr(value, "formula") <- form
    attr(value, "nugget") <- nugget
    attr(value, "metric") <- match.arg(metric)
    attr(value, "fixed") <- fixed
    class(value) <- c("corGaus", "corSpatial", "corStruct")
    value
}


pdConstruct <- function (object, value, form, nam, data, ...) 
UseMethod("pdConstruct")


corLin <- function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", 
    "maximum", "manhattan"), fixed = FALSE) 
{
    attr(value, "formula") <- form
    attr(value, "nugget") <- nugget
    attr(value, "metric") <- match.arg(metric)
    attr(value, "fixed") <- fixed
    class(value) <- c("corLin", "corSpatial", "corStruct")
    value
}


varIdent <- function (value = numeric(0), form = ~1, fixed = NULL) 
{
    if (is.null(getGroupsFormula(form))) {
        value <- numeric(0)
        attr(value, "fixed") <- NULL
    }
    else {
        if ((lv <- length(value)) > 0) {
            if (is.null(grpNames <- names(value)) && (lv > 1)) {
                stop("initial values must have group names in 'varIdent'")
            }
            value <- unlist(value)
            if (any(value <= 0)) {
                stop("initial values for 'varIdent' must be > 0")
            }
            value <- log(value)
        }
        else grpNames <- NULL
        attr(value, "groupNames") <- grpNames
        if (!is.null(fixed)) {
            fix <- attr(value, "fixed") <- log(unlist(fixed))
            if (is.null(fixNames <- names(fix))) {
                stop("fixed parameters must have names in 'varIdent'")
            }
            if (!is.null(attr(value, "groupNames"))) {
                attr(value, "groupNames") <- c(attr(value, "groupNames"), 
                  fixNames)
            }
        }
    }
    attr(value, "formula") <- asOneSidedFormula(form)
    class(value) <- c("varIdent", "varFunc")
    value
}


corExp <- function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", 
    "maximum", "manhattan"), fixed = FALSE) 
{
    attr(value, "formula") <- form
    attr(value, "nugget") <- nugget
    attr(value, "metric") <- match.arg(metric)
    attr(value, "fixed") <- fixed
    class(value) <- c("corExp", "corSpatial", "corStruct")
    value
}


gls <- function (model, data = sys.frame(sys.parent()), correlation = NULL, 
    weights = NULL, subset, method = c("REML", "ML"), na.action = na.fail, 
    control = list(), verbose = FALSE) 
{
    Call <- match.call()
    controlvals <- glsControl()
    if (!missing(control)) 
        controlvals[names(control)] <- control
    if (!inherits(model, "formula") || length(model) != 3L) {
        stop("\nmodel must be a formula of the form \"resp ~ pred\"")
    }
    method <- match.arg(method)
    REML <- method == "REML"
    groups <- if (!is.null(correlation)) 
        getGroupsFormula(correlation)
    glsSt <- glsStruct(corStruct = correlation, varStruct = varFunc(weights))
    model <- terms(model, data = data)
    mfArgs <- list(formula = asOneFormula(formula(glsSt), model, 
        groups), data = data, na.action = na.action)
    if (!missing(subset)) {
        mfArgs[["subset"]] <- asOneSidedFormula(Call[["subset"]])[[2L]]
    }
    mfArgs$drop.unused.levels <- TRUE
    dataMod <- do.call(model.frame, mfArgs)
    origOrder <- row.names(dataMod)
    if (!is.null(groups)) {
        groups <- eval(substitute(~1 | GR, list(GR = groups[[2L]])))
        grps <- getGroups(dataMod, groups, level = length(getGroupsFormula(groups, 
            asList = TRUE)))
        ord <- order(grps)
        grps <- grps[ord]
        dataMod <- dataMod[ord, , drop = FALSE]
        revOrder <- match(origOrder, row.names(dataMod))
    }
    else grps <- NULL
    X <- model.frame(model, dataMod)
    contr <- lapply(X, function(el) if (inherits(el, "factor")) 
        contrasts(el))
    contr <- contr[!unlist(lapply(contr, is.null))]
    X <- model.matrix(model, X)
    if (ncol(X) == 0L) 
        stop("no coefficients to fit")
    y <- eval(model[[2L]], dataMod)
    N <- nrow(X)
    p <- ncol(X)
    parAssign <- attr(X, "assign")
    fTerms <- terms(as.formula(model), data = data)
    namTerms <- attr(fTerms, "term.labels")
    if (attr(fTerms, "intercept") > 0) {
        namTerms <- c("(Intercept)", namTerms)
    }
    namTerms <- factor(parAssign, labels = namTerms)
    parAssign <- split(order(parAssign), namTerms)
    fixedSigma <- (controlvals$sigma > 0)
    attr(glsSt, "conLin") <- list(Xy = array(c(X, y), c(N, ncol(X) + 
        1L), list(row.names(dataMod), c(colnames(X), deparse(model[[2]])))), 
        dims = list(N = N, p = p, REML = as.integer(REML)), logLik = 0, 
        sigma = controlvals$sigma, fixedSigma = fixedSigma)
    glsEstControl <- controlvals["singular.ok"]
    glsSt <- Initialize(glsSt, dataMod, glsEstControl)
    parMap <- attr(glsSt, "pmap")
    numIter <- numIter0 <- 0L
    repeat {
        oldPars <- c(attr(glsSt, "glsFit")[["beta"]], coef(glsSt))
        if (length(coef(glsSt))) {
            optRes <- if (controlvals$opt == "nlminb") {
                nlminb(c(coef(glsSt)), function(glsPars) -logLik(glsSt, 
                  glsPars), control = list(trace = controlvals$msVerbose, 
                  iter.max = controlvals$msMaxIter))
            }
            else {
                optim(c(coef(glsSt)), function(glsPars) -logLik(glsSt, 
                  glsPars), method = controlvals$optimMethod, 
                  control = list(trace = controlvals$msVerbose, 
                    maxit = controlvals$msMaxIter, reltol = if (numIter == 
                      0L) controlvals$msTol else 100 * .Machine$double.eps))
            }
            coef(glsSt) <- optRes$par
        }
        else {
            optRes <- list(convergence = 0)
        }
        attr(glsSt, "glsFit") <- glsEstimate(glsSt, control = glsEstControl)
        if (!needUpdate(glsSt)) {
            if (optRes$convergence) 
                stop(optRes$message)
            break
        }
        numIter <- numIter + 1L
        glsSt <- update(glsSt, dataMod)
        aConv <- c(attr(glsSt, "glsFit")[["beta"]], coef(glsSt))
        conv <- abs((oldPars - aConv)/ifelse(aConv == 0, 1, aConv))
        aConv <- c(beta = max(conv[1:p]))
        conv <- conv[-(1:p)]
        for (i in names(glsSt)) {
            if (any(parMap[, i])) {
                aConv <- c(aConv, max(conv[parMap[, i]]))
                names(aConv)[length(aConv)] <- i
            }
        }
        if (verbose) {
            cat("\nIteration:", numIter)
            cat("\nObjective:", format(optRes$value), "\n")
            print(glsSt)
            cat("\nConvergence:\n")
            print(aConv)
        }
        if (max(aConv) <= controlvals$tolerance) {
            break
        }
        if (numIter > controlvals$maxIter) {
            stop("maximum number of iterations reached without convergence")
        }
    }
    glsFit <- attr(glsSt, "glsFit")
    namBeta <- names(glsFit$beta)
    attr(glsSt, "fixedSigma") <- fixedSigma
    attr(parAssign, "varBetaFact") <- varBeta <- glsFit$sigma * 
        glsFit$varBeta * sqrt((N - REML * p)/(N - p))
    varBeta <- crossprod(varBeta)
    dimnames(varBeta) <- list(namBeta, namBeta)
    Fitted <- fitted(glsSt)
    if (!is.null(grps)) {
        grps <- grps[revOrder]
        Fitted <- Fitted[revOrder]
        Resid <- y[revOrder] - Fitted
        attr(Resid, "std") <- glsFit$sigma/varWeights(glsSt)[revOrder]
    }
    else {
        Resid <- y - Fitted
        attr(Resid, "std") <- glsFit$sigma/varWeights(glsSt)
    }
    names(Resid) <- names(Fitted) <- origOrder
    apVar <- if (controlvals$apVar) 
        glsApVar(glsSt, glsFit$sigma, .relStep = controlvals[[".relStep"]], 
            minAbsPar = controlvals[["minAbsParApVar"]], natural = controlvals[["natural"]])
    else "Approximate variance-covariance matrix not available"
    dims <- attr(glsSt, "conLin")[["dims"]]
    dims[["p"]] <- p
    attr(glsSt, "conLin") <- NULL
    attr(glsSt, "glsFit") <- NULL
    attr(glsSt, "fixedSigma") <- fixedSigma
    grpDta <- inherits(data, "groupedData")
    structure(class = "gls", list(modelStruct = glsSt, dims = dims, 
        contrasts = contr, coefficients = glsFit[["beta"]], varBeta = varBeta, 
        sigma = if (fixedSigma) controlvals$sigma else glsFit$sigma, 
        apVar = apVar, logLik = glsFit$logLik, numIter = if (needUpdate(glsSt)) numIter else numIter0, 
        groups = grps, call = Call, method = method, fitted = Fitted, 
        residuals = Resid, parAssign = parAssign, na.action = attr(dataMod, 
            "na.action")), namBetaFull = colnames(X), units = if (grpDta) 
        attr(data, "units"), labels = if (grpDta) 
        attr(data, "labels"))
}


`Names<-` <- function (object, ..., value) 
UseMethod("Names<-")


recalc <- function (object, conLin, ...) 
UseMethod("recalc")


phenoModel <- function (Subject, time, dose, lCl, lV) 
{
    .C(nlme_one_comp_first, as.integer(length(time)), resp = as.double(dose), 
        as.double(cbind(Subject, time, dose, exp(lV), exp(lCl))), 
        NAOK = TRUE)$resp
}


corRatio <- function (value = numeric(0), form = ~1, nugget = FALSE, metric = c("euclidean", 
    "maximum", "manhattan"), fixed = FALSE) 
{
    attr(value, "formula") <- form
    attr(value, "nugget") <- nugget
    attr(value, "metric") <- match.arg(metric)
    attr(value, "fixed") <- fixed
    class(value) <- c("corRatio", "corSpatial", "corStruct")
    value
}


comparePred <- function (object1, object2, primary = NULL, minimum = min(primary), 
    maximum = max(primary), length.out = 51, level = NULL, ...) 
UseMethod("comparePred")


asOneFormula <- function (..., omit = c(".", "pi")) 
{
    names <- unique(allVarsRec(list(...)))
    names <- names[is.na(match(names, omit))]
    if (length(names)) 
        eval(parse(text = paste("~", paste(names, collapse = "+")))[[1]])
    else ~1
}


pdCompSymm <- function (value = numeric(0), form = NULL, nam = NULL, data = sys.frame(sys.parent())) 
{
    object <- numeric(0)
    class(object) <- c("pdCompSymm", "pdMat")
    pdConstruct(object, value, form, nam, data)
}


compareFits <- function (object1, object2, which = 1:ncol(object1)) 
{
    dn1 <- dimnames(object1)
    dn2 <- dimnames(object2)
    aux <- rep(NA, length(dn1[[1]]))
    if (any(aux1 <- is.na(match(dn2[[2]], dn1[[2]])))) {
        object1[, dn2[[2]][aux1]] <- aux
    }
    if (any(aux1 <- is.na(match(dn1[[2]], dn2[[2]])))) {
        object2[, dn1[[2]][aux1]] <- aux
    }
    dn1 <- dimnames(object1)
    c1 <- deparse(substitute(object1))
    c2 <- deparse(substitute(object2))
    if (any(sort(dn1[[1]]) != sort(dn2[[1]]))) {
        stop("objects must have coefficients with same row names")
    }
    object2 <- object2[dn1[[1]], dn1[[2]], drop = FALSE]
    object1 <- object1[, which, drop = FALSE]
    object2 <- object2[, which, drop = FALSE]
    dn1 <- dimnames(object1)
    dm1 <- dim(object1)
    out <- array(0, c(dm1[1], 2, dm1[2]), list(dn1[[1]], c(c1, 
        c2), dn1[[2]]))
    for (i in dn1[[2]]) {
        out[, , i] <- cbind(object1[[i]], object2[[i]])
    }
    class(out) <- "compareFits"
    out
}


simulate.lme <- function (object, nsim = 1, seed = as.integer(runif(1, 0, .Machine$integer.max)), 
    m2, method = c("REML", "ML"), niterEM = c(40, 200), useGen, 
    ...) 
{
    if (inherits(nsim, "lm") || inherits(nsim, "lme")) 
        stop("order of arguments in 'simulate.lme' has changed to conform with generic in R-2.2.0", 
            domain = NA)
    getResults1 <- function(conLin, nIter, pdClass, REML, ssq, 
        p, pp1) {
        unlist(.C(mixed_combined, as.double(conLin$Xy), as.integer(unlist(conLin$dims)), 
            double(ssq), as.integer(nIter), as.integer(pdClass), 
            as.integer(REML), logLik = double(1), R0 = double(pp1), 
            lRSS = double(1), info = integer(1), sigma = as.double(conLin$sigma))[c("info", 
            "logLik")])
    }
    getResults2 <- function(conLin, reSt, REML, control) {
        lmeSt <- lmeStruct(reStruct = reStruct(reSt, REML = REML))
        attr(lmeSt, "conLin") <- conLin
        lmeSt <- Initialize(lmeSt, data = NULL, groups = NULL, 
            control = control)
        attr(lmeSt, "conLin") <- MEdecomp(attr(lmeSt, "conLin"))
        aMs <- nlminb(c(coef(lmeSt)), function(lmePars) -logLik(lmeSt, 
            lmePars), control = list(iter.max = control$msMaxIter, 
            eval.max = control$msMaxEval, trace = control$msVerbose))
        c(info = aMs$flags[1], logLik = -aMs$value)
    }
    if (!exists(".Random.seed", envir = .GlobalEnv)) 
        runif(1)
    RNGstate <- get(".Random.seed", envir = .GlobalEnv)
    on.exit(assign(".Random.seed", RNGstate, envir = .GlobalEnv))
    set.seed(seed)
    if (inherits(object, "lme")) {
        fit1 <- object
        object <- as.list(object$call[-1])
    }
    else {
        object <- as.list(match.call(lme, substitute(object))[-1])
        fit1 <- do.call(lme, object)
    }
    if (length(fit1$modelStruct) > 1) 
        stop("models with \"corStruct\" and/or \"varFunc\" objects not allowed")
    reSt1 <- fit1$modelStruct$reStruct
    condL1 <- do.call(createConLin, object)
    pdClass1 <- vapply(reSt1, data.class, "")
    pdClass1 <- match(pdClass1, c("pdSymm", "pdDiag", "pdIdent", 
        "pdCompSymm", "pdLogChol"), 0) - 1
    control1 <- lmeControl()
    if (!is.null(object$control)) {
        control1[names(object$control)] <- object$control
    }
    control1$niterEM <- niterEM[1]
    sig <- fit1$sigma
    DeltaInv <- pdMatrix(reSt1, factor = TRUE)
    for (i in names(DeltaInv)) {
        DeltaInv[[i]] <- sig * DeltaInv[[i]]
    }
    if (missing(useGen)) {
        useGen <- any(pdClass1 == -1)
    }
    nullD <- condL1$dims
    N <- nullD$N
    Q <- nullD$Q
    p1 <- nullD$ncol[Q + 1]
    pp11 <- p1 * (p1 + 1)
    ycol1 <- sum(nullD$ncol)
    qvec <- nullD$qvec[1:Q]
    ssq1 <- sum(qvec^2)
    csq1 <- cumsum(c(1, qvec[-Q]))
    csq2 <- cumsum(qvec)
    ngrp <- nullD$ngrps
    base <- condL1$Xy[, ycol1 - (nullD$ncol[Q + 1]:1), drop = FALSE] %*% 
        fixef(fit1)
    ind <- lapply(1:Q, function(i) rep(1:ngrp[i], nullD$ZXlen[[i]]))
    if (ML <- !is.na(match("ML", method))) 
        nML <- array(0, c(nsim, 2), list(1:nsim, c("info", "logLik")))
    if (REML <- !is.na(match("REML", method))) 
        nREML <- array(0, c(nsim, 2), list(1:nsim, c("info", 
            "logLik")))
    if ((ALT <- !missing(m2))) {
        if (inherits(m2, "lme")) {
            fit2 <- m2
            m2 <- as.list(m2$call[-1])
        }
        else {
            m2 <- as.list(match.call(lme, substitute(m2))[-1])
            if (is.null(m2$random)) {
                m2$random <- asOneSidedFormula(object$fixed[-2])
            }
            aux <- object
            aux[names(m2)] <- m2
            m2 <- aux
            fit2 <- do.call(lme, m2)
        }
        if (length(fit2$modelStruct) > 1) {
            stop("models with \"corStruct\" and/or \"varFunc\" objects not allowed")
        }
        condL2 <- do.call(createConLin, m2)
        reSt2 <- fit2$modelStruct$reStruct
        control2 <- lmeControl()
        if (!is.null(m2$control)) {
            control2[names(m2$control)] <- m2$control
        }
        control2$niterEM <- niterEM[2]
        pdClass2 <- vapply(fit2$modelStruct$reStruct, data.class, 
            "")
        pdClass2 <- match(pdClass2, c("pdSymm", "pdDiag", "pdIdent", 
            "pdCompSymm", "pdLogChol"), 0) - 1
        useGen <- useGen || any(pdClass2 == -1)
        altD <- condL2$dims
        ssq2 <- sum((altD$qvec[1:altD$Q])^2)
        p2 <- altD$ncol[altD$Q + 1]
        pp12 <- p2 * (p2 + 1)
        ycol2 <- sum(altD$ncol)
        if (ML) 
            aML <- nML
        if (REML) 
            aREML <- nREML
    }
    for (i in 1:nsim) {
        base2 <- base + rnorm(N, sd = sig)
        for (j in 1:Q) {
            base2 <- base2 + ((array(rnorm(ngrp[j] * qvec[j]), 
                c(ngrp[j], qvec[j]), list(1:ngrp[j], NULL)) %*% 
                DeltaInv[[j]])[ind[[j]], , drop = FALSE] * condL1$Xy[, 
                csq1[j]:csq2[j], drop = FALSE]) %*% rep(1, qvec[j])
        }
        condL1$Xy[, ycol1] <- base2
        if (REML) {
            nREML[i, ] <- if (useGen) 
                getResults2(condL1, reSt1, TRUE, control1)
            else getResults1(condL1, niterEM[1], pdClass1, TRUE, 
                ssq1, p1, pp11)
        }
        if (ML) {
            nML[i, ] <- if (useGen) 
                getResults2(condL1, reSt1, FALSE, control1)
            else getResults1(condL1, niterEM[1], pdClass1, FALSE, 
                ssq1, p1, pp11)
        }
        if (ALT) {
            condL2$Xy[, ycol2] <- base2
            if (REML) {
                aREML[i, ] <- if (useGen) 
                  getResults2(condL2, reSt2, TRUE, control2)
                else getResults1(condL2, niterEM[2], pdClass2, 
                  TRUE, ssq2, p2, pp12)
            }
            if (ML) {
                aML[i, ] <- if (useGen) 
                  getResults2(condL2, reSt2, FALSE, control2)
                else getResults1(condL2, niterEM[2], pdClass2, 
                  FALSE, ssq2, p2, pp12)
            }
        }
    }
    v.null <- v.alt <- list()
    if (ML) {
        nML[, "logLik"] <- nML[, "logLik"] + N * (log(N) - (1 + 
            log(2 * pi)))/2
        v.null$ML <- nML
        if (ALT) {
            aML[, "logLik"] <- aML[, "logLik"] + N * (log(N) - 
                (1 + log(2 * pi)))/2
            v.alt$ML <- aML
        }
    }
    if (REML) {
        nREML[, "logLik"] <- nREML[, "logLik"] + (N - p1) * (log(N - 
            p1) - (1 + log(2 * pi)))/2
        v.null$REML <- nREML
        if (ALT) {
            aREML[, "logLik"] <- aREML[, "logLik"] + (N - p2) * 
                (log(N - p2) - (1 + log(2 * pi)))/2
            v.alt$REML <- aREML
        }
    }
    df <- p1 + length(coef(reSt1)) + 1
    if (ALT) 
        df <- abs(df - (p2 + length(coef(reSt2)) + 1))
    structure(if (ALT && (ML || REML)) 
        list(null = v.null, alt = v.alt)
    else list(null = v.null), class = "simulate.lme", call = match.call(), 
        seed = seed, df = df, useGen = useGen)
}


Dim <- function (object, ...) 
UseMethod("Dim")


pdMatrix <- function (object, factor = FALSE) 
UseMethod("pdMatrix")


balancedGrouped <- function (form, data, labels = NULL, units = NULL) 
{
    form <- as.formula(form)
    data <- t(as.matrix(data))
    dn <- dimnames(data)
    if (all(!is.na(as.numeric(dn[[1]])))) {
        dn[[1]] <- as.numeric(dn[[1]])
    }
    names(dn) <- c(as.character(getCovariateFormula(form)[[2]]), 
        as.character(getGroupsFormula(form)[[2]]))
    frm <- do.call("expand.grid", dn)
    frm[[as.character(getResponseFormula(form)[[2]])]] <- as.vector(data)
    do.call("groupedData", list(form, data = frm, labels = labels, 
        units = units))
}


varComb <- function (...) 
{
    val <- list(...)
    if (!all(unlist(lapply(val, inherits, "varFunc")))) {
        stop("all arguments to 'varComb' must be of class \"varFunc\".")
    }
    if (is.null(names(val))) {
        names(val) <- LETTERS[seq_along(val)]
    }
    class(val) <- c("varComb", "varFunc")
    val
}


getGroups <- function (object, form = formula(object), level, data, sep = "/") 
UseMethod("getGroups")


intervals <- function (object, level = 0.95, ...) 
UseMethod("intervals")


anova.lme <- function (object, ..., test = TRUE, type = c("sequential", "marginal"), 
    adjustSigma = TRUE, Terms, L, verbose = FALSE) 
{
    fixSig <- attr(object$modelStruct, "fixedSigma")
    fixSig <- !is.null(fixSig) && fixSig
    Lmiss <- missing(L)
    dots <- list(...)
    if ((rt <- length(dots) + 1L) == 1L) {
        if (!inherits(object, "lme")) {
            stop("object must inherit from class \"lme\" ")
        }
        vFix <- attr(object$fixDF, "varFixFact")
        if (adjustSigma && object$method == "ML") 
            vFix <- sqrt(object$dims$N/(object$dims$N - ncol(vFix))) * 
                vFix
        c0 <- solve(t(vFix), fixef(object))
        assign <- attr(object$fixDF, "assign")
        nTerms <- length(assign)
        if (missing(Terms) && Lmiss) {
            type <- match.arg(type)
            Fval <- Pval <- double(nTerms)
            nDF <- integer(nTerms)
            dDF <- object$fixDF$terms
            for (i in 1:nTerms) {
                nDF[i] <- length(assign[[i]])
                if (type == "sequential") {
                  c0i <- c0[assign[[i]]]
                }
                else {
                  c0i <- c(qr.qty(qr(vFix[, assign[[i]], drop = FALSE]), 
                    c0))[1:nDF[i]]
                }
                Fval[i] <- sum(c0i^2)/nDF[i]
                Pval[i] <- 1 - pf(Fval[i], nDF[i], dDF[i])
            }
            aod <- data.frame(numDF = nDF, denDF = dDF, `F-value` = Fval, 
                `p-value` = Pval, check.names = FALSE)
            rownames(aod) <- names(assign)
            attr(aod, "rt") <- rt
        }
        else {
            nX <- length(unlist(assign))
            if (Lmiss) {
                if (is.numeric(Terms) && all(Terms == as.integer(Terms))) {
                  if (min(Terms) < 1 || max(Terms) > nTerms) {
                    stop(gettextf("'Terms' must be between 1 and %d", 
                      nTerms), domain = NA)
                  }
                }
                else {
                  if (is.character(Terms)) {
                    if (any(noMatch <- is.na(match(Terms, names(assign))))) {
                      stop(sprintf(ngettext(sum(noMatch), "term %s not matched", 
                        "terms %s not matched"), paste(Terms[noMatch], 
                        collapse = ", ")), domain = NA)
                    }
                  }
                  else {
                    stop("terms can only be integers or characters")
                  }
                }
                dDF <- unique(object$fixDF$terms[Terms])
                if (length(dDF) > 1) {
                  stop("terms must all have the same denominator DF")
                }
                lab <- paste("F-test for:", paste(names(assign[Terms]), 
                  collapse = ", "), "\n")
                L <- diag(nX)[unlist(assign[Terms]), , drop = FALSE]
            }
            else {
                L <- as.matrix(L)
                if (ncol(L) == 1) 
                  L <- t(L)
                nrowL <- nrow(L)
                ncolL <- ncol(L)
                if (ncol(L) > nX) {
                  stop(sprintf(ngettext(nX, "'L' must have at most %d column", 
                    "'L' must have at most %d columns"), nX), 
                    domain = NA)
                }
                dmsL1 <- rownames(L)
                L0 <- array(0, c(nrowL, nX), list(NULL, names(object$fixDF$X)))
                if (is.null(dmsL2 <- colnames(L))) {
                  L0[, 1:ncolL] <- L
                }
                else {
                  if (any(noMatch <- is.na(match(dmsL2, colnames(L0))))) {
                    stop(sprintf(ngettext(sum(noMatch), "effect %s not matched", 
                      "effects %s not matched"), paste(dmsL2[noMatch], 
                      collapse = ", ")), domain = NA)
                  }
                  L0[, dmsL2] <- L
                }
                L <- L0[noZeroRowL <- as.logical((L0 != 0) %*% 
                  rep(1, nX)), , drop = FALSE]
                nrowL <- nrow(L)
                rownames(L) <- if (is.null(dmsL1)) 
                  1:nrowL
                else dmsL1[noZeroRowL]
                dDF <- unique(object$fixDF$X[noZeroColL <- as.logical(c(rep(1, 
                  nrowL) %*% (L != 0)))])
                if (length(dDF) > 1) {
                  stop("L may only involve fixed effects with the same denominator DF")
                }
                lab <- "F-test for linear combination(s)\n"
            }
            nDF <- sum(svd.d(L) > 0)
            c0 <- c(qr.qty(qr(vFix %*% t(L)), c0))[1:nDF]
            Fval <- sum(c0^2)/nDF
            Pval <- pf(Fval, nDF, dDF, lower.tail = FALSE)
            aod <- data.frame(numDF = nDF, denDF = dDF, `F-value` = Fval, 
                `p-value` = Pval, check.names = FALSE)
            attr(aod, "rt") <- rt
            attr(aod, "label") <- lab
            if (!Lmiss) {
                attr(aod, "L") <- if (nrow(L) > 1) 
                  L[, noZeroColL, drop = FALSE]
                else L[, noZeroColL]
            }
        }
    }
    else {
        ancall <- sys.call()
        ancall$verbose <- ancall$test <- ancall$type <- NULL
        object <- list(object, ...)
        termsClass <- vapply(object, data.class, "")
        valid.cl <- c("gls", "gnls", "lm", "lmList", "lme", "nlme", 
            "nlsList", "nls")
        if (!all(match(termsClass, valid.cl, 0))) {
            valid.cl <- paste0("\"", valid.cl, "\"")
            stop(gettextf("objects must inherit from classes %s, or %s", 
                paste(head(valid.cl, -1), collapse = ", "), tail(valid.cl, 
                  1)), domain = NA)
        }
        resp <- vapply(object, function(el) deparse(getResponseFormula(el)[[2L]]), 
            "")
        subs <- as.logical(match(resp, resp[1L], FALSE))
        if (!all(subs)) 
            warning("some fitted objects deleted because response differs from the first model")
        if (sum(subs) == 1) 
            stop("first model has a different response from the rest")
        object <- object[subs]
        rt <- length(object)
        termsModel <- lapply(object, function(el) formula(el)[-2])
        estMeth <- vapply(object, function(el) if (is.null(val <- el[["method"]])) 
            NA_character_
        else val, "")
        if (length(uEst <- unique(estMeth[!is.na(estMeth)])) > 
            1) {
            stop("all fitted objects must have the same estimation method")
        }
        estMeth[is.na(estMeth)] <- uEst
        REML <- uEst == "REML"
        if (REML) {
            aux <- vapply(termsModel, function(el) {
                tt <- terms(el)
                val <- paste(sort(attr(tt, "term.labels")), collapse = "&")
                if (attr(tt, "intercept") == 1) 
                  paste(val, "(Intercept)", sep = "&")
                else val
            }, ".")
            if (length(unique(aux)) > 1) {
                warning("fitted objects with different fixed effects. REML comparisons are not meaningful.")
            }
        }
        termsCall <- lapply(object, function(el) {
            if (is.null(val <- el$call) && is.null(val <- attr(el, 
                "call"))) 
                stop("objects must have a \"call\" component or attribute")
            val
        })
        termsCall <- vapply(termsCall, function(el) paste(deparse(el), 
            collapse = ""), "")
        aux <- lapply(object, logLik, REML)
        if (length(unique(vapply(aux, attr, 1, "nall"))) > 1) {
            stop("all fitted objects must use the same number of observations")
        }
        dfModel <- vapply(aux, attr, 1, "df")
        logLik <- vapply(aux, c, 1.1)
        aod <- data.frame(call = termsCall, Model = 1:rt, df = dfModel, 
            AIC = vapply(aux, AIC, 1), BIC = vapply(aux, BIC, 
                1), logLik = logLik, check.names = FALSE)
        if (test) {
            ddf <- diff(dfModel)
            if (sum(abs(ddf)) > 0) {
                effects <- rep("", rt)
                for (i in 2:rt) {
                  if (ddf[i - 1] != 0) {
                    effects[i] <- paste(i - 1, i, sep = " vs ")
                  }
                }
                pval <- rep(NA, rt - 1)
                ldf <- as.logical(ddf)
                lratio <- 2 * abs(diff(logLik))
                lratio[!ldf] <- NA
                pval[ldf] <- pchisq(lratio[ldf], abs(ddf[ldf]), 
                  lower.tail = FALSE)
                aod <- data.frame(aod, Test = effects, L.Ratio = c(NA, 
                  lratio), `p-value` = c(NA, pval), check.names = FALSE)
            }
        }
        row.names(aod) <- vapply(as.list(ancall[-1L]), c_deparse, 
            "")
        attr(aod, "rt") <- rt
        attr(aod, "verbose") <- verbose
    }
    class(aod) <- c("anova.lme", "data.frame")
    aod
}


corSpatial <- function (value = numeric(0), form = ~1, nugget = FALSE, type = c("spherical", 
    "exponential", "gaussian", "linear", "rational"), metric = c("euclidean", 
    "maximum", "manhattan"), fixed = FALSE) 
{
    type <- match.arg(type)
    spClass <- switch(type, spherical = "corSpher", exponential = "corExp", 
        gaussian = "corGaus", linear = "corLin", rational = "corRatio")
    attr(value, "formula") <- form
    attr(value, "nugget") <- nugget
    attr(value, "metric") <- match.arg(metric)
    attr(value, "fixed") <- fixed
    class(value) <- c(spClass, "corSpatial", "corStruct")
    value
}


glsApVar <- function (glsSt, sigma, conLin = attr(glsSt, "conLin"), .relStep = .Machine$double.eps^(1/3), 
    minAbsPar = 0, natural = TRUE) 
{
    fixedSigma <- attr(glsSt, "fixedSigma")
    if (length(glsCoef <- coef(glsSt)) > 0L) {
        cSt <- glsSt[["corStruct"]]
        if (natural && !is.null(cSt) && inherits(cSt, "corSymm")) {
            cStNatPar <- coef(cSt, unconstrained = FALSE)
            class(cSt) <- c("corNatural", "corStruct")
            coef(cSt) <- log((1 + cStNatPar)/(1 - cStNatPar))
            glsSt[["corStruct"]] <- cSt
            glsCoef <- coef(glsSt)
        }
        dims <- conLin$dims
        N <- dims$N - dims$REML * dims$p
        conLin[["logLik"]] <- 0
        Pars <- if (fixedSigma) 
            glsCoef
        else c(glsCoef, lSigma = log(sigma))
        val <- fdHess(Pars, glsApVar.fullGlsLogLik, glsSt, conLin, 
            dims, N, .relStep = .relStep, minAbsPar = minAbsPar)[["Hessian"]]
        if (all(eigen(val, only.values = TRUE)$values < 0)) {
            val <- solve(-val)
            nP <- names(Pars)
            dimnames(val) <- list(nP, nP)
            attr(val, "Pars") <- Pars
            attr(val, "natural") <- natural
            val
        }
        else {
            "Non-positive definite approximate variance-covariance"
        }
    }
}


corCAR1 <- function (value = 0.2, form = ~1, fixed = FALSE) 
{
    if (value <= 0 | value >= 1) {
        stop("parameter in CAR(1) structure must be between 0 and 1")
    }
    value <- log(value/(1 - value))
    attr(value, "formula") <- form
    attr(value, "fixed") <- fixed
    class(value) <- c("corCAR1", "corStruct")
    value
}




## Package Data

Alfalfa <- nlme::Alfalfa		## Split-Plot Experiment on Varieties of Alfalfa

Assay <- nlme::Assay		## Bioassay on Cell Culture Plate

BodyWeight <- nlme::BodyWeight		## Rat weight over time for different diets

Cefamandole <- nlme::Cefamandole		## Pharmacokinetics of Cefamandole

Dialyzer <- nlme::Dialyzer		## High-Flux Hemodialyzer

Earthquake <- nlme::Earthquake		## Earthquake Intensity

Fatigue <- nlme::Fatigue		## Cracks caused by metal fatigue

Gasoline <- nlme::Gasoline		## Refinery yield of gasoline

Glucose <- nlme::Glucose		## Glucose levels over time

Glucose2 <- nlme::Glucose2		## Glucose Levels Following Alcohol Ingestion

Gun <- nlme::Gun		## Methods for firing naval guns

IGF <- nlme::IGF		## Radioimmunoassay of IGF-I Protein

Machines <- nlme::Machines		## Productivity Scores for Machines and Workers

MathAchSchool <- nlme::MathAchSchool		## School demographic data for MathAchieve

MathAchieve <- nlme::MathAchieve		## Mathematics achievement scores

Meat <- nlme::Meat		## Tenderness of meat

Milk <- nlme::Milk		## Protein content of cows' milk

Muscle <- nlme::Muscle		## Contraction of heart muscle sections

Nitrendipene <- nlme::Nitrendipene		## Assay of nitrendipene

Oats <- nlme::Oats		## Split-plot Experiment on Varieties of Oats

Orthodont <- nlme::Orthodont		## Growth curve data on an orthdontic measurement

Ovary <- nlme::Ovary		## Counts of Ovarian Follicles

Oxboys <- nlme::Oxboys		## Heights of Boys in Oxford

Oxide <- nlme::Oxide		## Variability in Semiconductor Manufacturing

PBG <- nlme::PBG		## Effect of Phenylbiguanide on Blood Pressure

Phenobarb <- nlme::Phenobarb		## Phenobarbitol Kinetics

Pixel <- nlme::Pixel		## X-ray pixel intensities over time

Quinidine <- nlme::Quinidine		## Quinidine Kinetics

Rail <- nlme::Rail		## Evaluation of Stress in Railway Rails

RatPupWeight <- nlme::RatPupWeight		## The weight of rat pups

Relaxin <- nlme::Relaxin		## Assay for Relaxin

Remifentanil <- nlme::Remifentanil		## Pharmacokinetics of remifentanil

Soybean <- nlme::Soybean		## Growth of soybean plants

Spruce <- nlme::Spruce		## Growth of Spruce Trees

Tetracycline1 <- nlme::Tetracycline1		## Pharmacokinetics of tetracycline

Tetracycline2 <- nlme::Tetracycline2		## Pharmacokinetics of tetracycline

Wafer <- nlme::Wafer		## Modeling of Analog MOS Circuits

Wheat <- nlme::Wheat		## Yields by growing conditions

Wheat2 <- nlme::Wheat2		## Wheat Yield Trials

bdf <- nlme::bdf		## Language scores

ergoStool <- nlme::ergoStool		## Ergometrics experiment with stool types



## Package Info

.skeleton_package_title = "Linear and Nonlinear Mixed Effects Models"

.skeleton_package_version = "3.1-128"

.skeleton_package_depends = ""

.skeleton_package_imports = "graphics,stats,utils,lattice"


## Internal

.skeleton_version = 5


## EOF