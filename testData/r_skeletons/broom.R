##
## Exported symobls in package `broom`
##

## Exported package methods

augment_columns <- function (x, data, newdata, type, type.predict = type, type.residuals = type, 
    se.fit = TRUE, ...) 
{
    notNAs <- function(o) if (is.null(o) || all(is.na(o))) {
        NULL
    }
    else {
        o
    }
    residuals0 <- failwith(NULL, stats::residuals, TRUE)
    influence0 <- failwith(NULL, stats::influence, TRUE)
    cooks.distance0 <- failwith(NULL, stats::cooks.distance, 
        TRUE)
    rstandard0 <- failwith(NULL, stats::rstandard, TRUE)
    predict0 <- failwith(NULL, stats::predict, TRUE)
    args <- list(x)
    if (!missing(newdata)) {
        args$newdata <- newdata
    }
    if (!missing(type.predict)) {
        args$type <- type.predict
    }
    args$se.fit <- se.fit
    args <- c(args, list(...))
    if ("panelmodel" %in% class(x)) {
        pred <- model.frame(x)[, 1] - residuals(x)
    }
    else {
        pred <- suppressWarnings(do.call(predict0, args))
    }
    if (is.null(pred)) {
        pred <- do.call(stats::fitted, args)
    }
    if (is.list(pred)) {
        ret <- data.frame(.fitted = pred$fit)
        ret$.se.fit <- pred$se.fit
    }
    else {
        ret <- data.frame(.fitted = as.numeric(pred))
    }
    na_action <- if (isS4(x)) {
        attr(stats::model.frame(x), "na.action")
    }
    else {
        stats::na.action(x)
    }
    if (missing(newdata) || is.null(newdata)) {
        if (!missing(type.residuals)) {
            ret$.resid <- residuals0(x, type = type.residuals)
        }
        else {
            ret$.resid <- residuals0(x)
        }
        infl <- influence0(x, do.coef = FALSE)
        if (!is.null(infl)) {
            if (is_mgcv(x)) {
                ret$.hat <- infl
                ret$.sigma <- NA
            }
            else {
                ret$.hat <- infl$hat
                ret$.sigma <- infl$sigma
            }
        }
        ret$.cooksd <- notNAs(cooks.distance0(x))
        ret$.std.resid <- notNAs(rstandard0(x))
        original <- data
        if (class(na_action) == "exclude") {
            if (length(stats::residuals(x)) > nrow(data)) {
                warning("When fitting with na.exclude, rows with NA in ", 
                  "original data will be dropped unless those rows are provided ", 
                  "in 'data' argument")
            }
        }
    }
    else {
        original <- newdata
    }
    if (is.null(na_action) || nrow(original) == nrow(ret)) {
        original <- fix_data_frame(original, newcol = ".rownames")
        return(unrowname(cbind(original, ret)))
    }
    else if (class(na_action) == "omit") {
        original <- fix_data_frame(original, newcol = ".rownames")
        original <- original[-na_action, ]
        return(unrowname(cbind(original, ret)))
    }
    ret$.rownames <- rownames(ret)
    original$.rownames <- rownames(original)
    ret <- merge(original, ret, by = ".rownames")
    ret <- ret[order(match(ret$.rownames, rownames(original))), 
        ]
    rownames(ret) <- NULL
    if (all(ret$.rownames == seq_along(ret$.rownames))) {
        ret$.rownames <- NULL
    }
    ret
}


bootstrap <- function (df, m, by_group = FALSE) 
{
    n <- nrow(df)
    attr(df, "indices") <- if (by_group && !is.null(groups(df))) {
        replicate(m, unlist(lapply(attr(df, "indices"), function(x) {
            sample(x, replace = TRUE)
        }), recursive = FALSE, use.names = FALSE), simplify = FALSE)
    }
    else {
        replicate(m, sample(n, replace = TRUE) - 1, simplify = FALSE)
    }
    attr(df, "drop") <- TRUE
    attr(df, "group_sizes") <- rep(n, m)
    attr(df, "biggest_group_size") <- n
    attr(df, "labels") <- data.frame(replicate = 1:m)
    attr(df, "vars") <- list(quote(replicate))
    class(df) <- c("grouped_df", "tbl_df", "tbl", "data.frame")
    df
}


tidy <- function (x, ...) 
UseMethod("tidy")


finish_glance <- function (ret, x) 
{
    ret$logLik <- tryCatch(as.numeric(stats::logLik(x)), error = function(e) NULL)
    ret$AIC <- tryCatch(stats::AIC(x), error = function(e) NULL)
    ret$BIC <- tryCatch(stats::BIC(x), error = function(e) NULL)
    if ("lmerMod" %in% class(x)) {
        ret$deviance <- tryCatch(stats::deviance(x, REML = FALSE), 
            error = function(e) NULL)
    }
    else {
        ret$deviance <- tryCatch(stats::deviance(x), error = function(e) NULL)
    }
    ret$df.residual <- tryCatch(df.residual(x), error = function(e) NULL)
    return(unrowname(ret))
}


glance <- function (x, ...) 
UseMethod("glance")


tidyMCMC <- function (x, pars, estimate.method = "mean", conf.int = FALSE, 
    conf.level = 0.95, conf.method = "quantile", droppars = "lp__", 
    rhat = FALSE, ess = FALSE, ...) 
{
    stan <- inherits(x, "stanfit")
    ss <- if (stan) 
        as.matrix(x, pars = pars)
    else as.matrix(x)
    ss <- ss[, !colnames(ss) %in% droppars, drop = FALSE]
    if (!missing(pars) && !stan) {
        if (length(badpars <- which(!pars %in% colnames(ss))) > 
            0) {
            stop("unrecognized parameters: ", pars[badpars])
        }
        ss <- ss[, pars]
    }
    estimate.method <- match.arg(estimate.method, c("mean", "median"))
    m <- switch(estimate.method, mean = colMeans(ss), median = apply(ss, 
        2, stats::median))
    ret <- data.frame(estimate = m, std.error = apply(ss, 2, 
        stats::sd))
    if (conf.int) {
        levs <- c((1 - conf.level)/2, (1 + conf.level)/2)
        conf.method <- match.arg(conf.method, c("quantile", "HPDinterval"))
        ci <- switch(conf.method, quantile = t(apply(ss, 2, stats::quantile, 
            levs)), coda::HPDinterval(coda::as.mcmc(ss), prob = conf.level))
        colnames(ci) <- c("conf.low", "conf.high")
        ret <- data.frame(ret, ci)
    }
    if (rhat || ess) {
        if (!stan) 
            warning("ignoring 'rhat' and 'ess' (only available for stanfit objects)")
        summ <- rstan::summary(x, pars = pars, probs = NULL)$summary[, 
            c("Rhat", "n_eff"), drop = FALSE]
        summ <- summ[!dimnames(summ)[[1L]] %in% droppars, , drop = FALSE]
        if (rhat) 
            ret$rhat <- summ[, "Rhat"]
        if (ess) 
            ret$ess <- as.integer(round(summ[, "n_eff"]))
    }
    return(fix_data_frame(ret))
}


fix_data_frame <- function (x, newnames = NULL, newcol = "term") 
{
    if (!is.null(newnames) && length(newnames) != ncol(x)) {
        stop("newnames must be NULL or have length equal to number of columns")
    }
    if (all(rownames(x) == seq_len(nrow(x)))) {
        ret <- data.frame(x, stringsAsFactors = FALSE)
        if (!is.null(newnames)) {
            colnames(ret) <- newnames
        }
    }
    else {
        ret <- data.frame(...new.col... = rownames(x), unrowname(x), 
            stringsAsFactors = FALSE)
        colnames(ret)[1] <- newcol
        if (!is.null(newnames)) {
            colnames(ret)[-1] <- newnames
        }
    }
    unrowname(ret)
}


confint_tidy <- function (x, conf.level = 0.95, func = stats::confint, ...) 
{
    CI <- suppressMessages(func(x, level = conf.level, ...))
    if (is.null(dim(CI))) {
        CI = matrix(CI, nrow = 1)
    }
    colnames(CI) = c("conf.low", "conf.high")
    unrowname(as.data.frame(CI))
}


augment <- function (x, ...) 
UseMethod("augment")


inflate <- function (.data, ..., stringsAsFactors = FALSE) 
{
    ret <- expand.grid(..., stringsAsFactors = stringsAsFactors)
    ret <- ret %>% group_by_(.dots = colnames(ret)) %>% do(.data)
    if (!is.null(groups(.data))) {
        ret <- ret %>% group_by_(.dots = groups(.data), add = TRUE)
    }
    ret
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Convert Statistical Analysis Objects into Tidy Data Frames"

.skeleton_package_version = "0.4.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "plyr,dplyr,tidyr,psych,stringr,reshape2,nlme,methods"


## Internal

.skeleton_version = 5


## EOF