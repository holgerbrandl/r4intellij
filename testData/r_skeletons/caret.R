##
## Exported symobls in package `caret`
##

## Exported package methods

calibration <- function (x, ...) 
UseMethod("calibration")


sbfControl <- function (functions = NULL, method = "boot", saveDetails = FALSE, 
    number = ifelse(method %in% c("cv", "repeatedcv"), 10, 25), 
    repeats = ifelse(method %in% c("cv", "repeatedcv"), 1, number), 
    verbose = FALSE, returnResamp = "final", p = 0.75, index = NULL, 
    indexOut = NULL, timingSamps = 0, seeds = NA, allowParallel = TRUE, 
    multivariate = FALSE) 
{
    list(functions = if (is.null(functions)) caretSBF else functions, 
        method = method, saveDetails = saveDetails, number = number, 
        repeats = repeats, returnResamp = returnResamp, verbose = verbose, 
        p = p, index = index, indexOut = indexOut, timingSamps = timingSamps, 
        seeds = seeds, allowParallel = allowParallel, multivariate = multivariate)
}


expandParameters <- function (fixed, seq) 
{
    if (is.null(seq)) 
        return(fixed)
    isSeq <- names(fixed) %in% names(seq)
    out <- fixed
    for (i in 1:nrow(seq)) {
        tmp <- fixed
        tmp[, isSeq] <- seq[i, ]
        out <- rbind(out, tmp)
    }
    out
}


gafs_raMutation <- function (population, parent, ...) 
{
    mutate <- parent <- as.vector(population[parent, ])
    n <- length(parent)
    j <- sample(1:n, size = 1)
    mutate[j] <- abs(mutate[j] - 1)
    mutate
}


safs_initial <- function (vars, prob = 0.2, ...) 
{
    sort(sample.int(vars, size = floor(vars * prob) + 1))
}


nnetBag <- structure(list(fit = function (x, y, ...) 
{
    loadNamespace("nnet")
    factorY <- is.factor(y)
    if (factorY) 
        y <- class2ind(y)
    out <- nnet::nnet(x, y, linout = !factorY, trace = FALSE, 
        ...)
    out$classification <- factorY
    out
}, pred = function (object, x) 
{
    out <- predict(object, x, type = "raw")
    if (object$classification) {
        colnames(out) <- colnames(object$fitted.values)
        rownames(out) <- NULL
    }
    else out <- predict(object, x, type = "raw")[, 1]
    out
}, aggregate = function (x, type = "class") 
{
    if (is.matrix(x[[1]]) | is.data.frame(x[[1]])) {
        pooled <- x[[1]] & NA
        classes <- colnames(pooled)
        for (i in 1:ncol(pooled)) {
            tmp <- lapply(x, function(y, col) y[, col], col = i)
            tmp <- do.call("rbind", tmp)
            pooled[, i] <- apply(tmp, 2, median)
        }
        if (type == "class") {
            out <- factor(classes[apply(pooled, 1, which.max)], 
                levels = classes)
        }
        else out <- as.data.frame(pooled)
    }
    else {
        x <- matrix(unlist(x), ncol = length(x))
        out <- apply(x, 1, median)
    }
    out
}), .Names = c("fit", "pred", "aggregate"))


filterVarImp <- function (x, y, nonpara = FALSE, ...) 
{
    notNumber <- sapply(x, function(x) !is.numeric(x))
    x = asNumeric(x)
    if (is.factor(y)) {
        classLevels <- levels(y)
        k <- length(classLevels)
        if (k > 2) {
            Combs <- combn(classLevels, 2)
            CombsN <- combn(1:k, 2)
            lStat <- lapply(1:ncol(Combs), FUN = function(cc) {
                yLevs <- as.character(y) %in% Combs[, cc]
                tmpX <- x[yLevs, ]
                tmpY <- as.numeric(y[yLevs] == Combs[, cc][2])
                apply(tmpX, 2, rocPerCol, cls = tmpY)
            })
            Stat = do.call("cbind", lStat)
            loutStat <- lapply(1:k, function(j) {
                apply(Stat[, CombsN[, j]], 1, max)
            })
            outStat = do.call("cbind", loutStat)
        }
        else {
            tmp <- apply(x, 2, rocPerCol, cls = y)
            outStat <- cbind(tmp, tmp)
        }
        outStat <- as.data.frame(outStat)
        colnames(outStat) <- classLevels
        rownames(outStat) <- dimnames(x)[[2]]
        outStat <- data.frame(outStat)
    }
    else {
        paraFoo <- function(data, y) abs(coef(summary(lm(y ~ 
            data, na.action = na.omit)))[2, "t value"])
        nonparaFoo <- function(x, y, ...) {
            meanMod <- sum((y - mean(y, rm.na = TRUE))^2)
            nzv <- nearZeroVar(x, saveMetrics = TRUE)
            if (nzv$zeroVar) 
                return(NA)
            if (nzv$percentUnique < 20) {
                regMod <- lm(y ~ x, na.action = na.omit, ...)
            }
            else {
                regMod <- try(loess(y ~ x, na.action = na.omit, 
                  ...), silent = TRUE)
                if (class(regMod) == "try-error" | any(is.nan(regMod$residuals))) 
                  try(regMod <- lm(y ~ x, ...))
                if (class(regMod) == "try-error") 
                  return(NA)
            }
            pR2 <- 1 - (sum(resid(regMod)^2)/meanMod)
            if (pR2 < 0) 
                pR2 <- 0
            pR2
        }
        testFunc <- if (nonpara) 
            nonparaFoo
        else paraFoo
        outStat <- apply(x, 2, testFunc, y = y)
        outStat <- data.frame(Overall = outStat)
    }
    outStat
}


postResample <- function (pred, obs) 
{
    isNA <- is.na(pred)
    pred <- pred[!isNA]
    obs <- obs[!isNA]
    if (!is.factor(obs) & is.numeric(obs)) {
        if (length(obs) + length(pred) == 0) {
            out <- rep(NA, 2)
        }
        else {
            if (length(unique(pred)) < 2 || length(unique(obs)) < 
                2) {
                resamplCor <- NA
            }
            else {
                resamplCor <- try(cor(pred, obs, use = "pairwise.complete.obs"), 
                  silent = TRUE)
                if (class(resamplCor) == "try-error") 
                  resamplCor <- NA
            }
            mse <- mean((pred - obs)^2)
            n <- length(obs)
            out <- c(sqrt(mse), resamplCor^2)
        }
        names(out) <- c("RMSE", "Rsquared")
    }
    else {
        if (length(obs) + length(pred) == 0) {
            out <- rep(NA, 2)
        }
        else {
            pred <- factor(pred, levels = levels(obs))
            requireNamespaceQuietStop("e1071")
            out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag", 
                "kappa")]
        }
        names(out) <- c("Accuracy", "Kappa")
    }
    if (any(is.nan(out))) 
        out[is.nan(out)] <- NA
    out
}


caretFuncs <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, first, last, ...) 
train(x, y, ...), pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (object$modelType == "Classification" & object$control$classProbs) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, rank = function (object, x, y) 
{
    vimp <- varImp(object, scale = FALSE)$importance
    if (!is.data.frame(vimp)) 
        vimp <- as.data.frame(vimp)
    if (object$modelType == "Regression") {
        vimp <- vimp[order(vimp[, 1], decreasing = TRUE), , drop = FALSE]
    }
    else {
        if (all(levels(y) %in% colnames(vimp)) & !("Overall" %in% 
            colnames(vimp))) {
            avImp <- apply(vimp[, levels(y), drop = TRUE], 1, 
                mean)
            vimp$Overall <- avImp
        }
    }
    vimp$var <- rownames(vimp)
    vimp
}, selectSize = function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}, selectVar = function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}), .Names = c("summary", "fit", "pred", "rank", "selectSize", 
"selectVar"))


getModelInfo <- function (model = NULL, regex = TRUE, ...) 
{
    load(system.file("models", "models.RData", package = "caret"))
    if (!is.null(model)) {
        keepers <- if (regex) 
            grepl(model, names(models), ...)
        else which(model == names(models))[1]
        models <- models[keepers]
    }
    if (length(models) == 0) 
        stop("That model is not in caret's built-in library")
    models
}


defaultSummary <- function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}


predictors <- function (x, ...) 
{
    UseMethod("predictors")
}


dotPlot <- function (x, top = min(20, dim(x$importance)[1]), ...) 
{
    varSubset <- sortImp(x, top)
    plotObj <- stack(varSubset)
    if (dim(varSubset)[2] == 1) {
        plotObj <- varSubset
        names(plotObj) <- "values"
        plotObj$ind <- "Overall"
    }
    else plotObj <- stack(varSubset)
    plotObj$Var <- rep(rownames(varSubset), dim(varSubset)[2])
    plotObj$Var <- factor(plotObj$Var, levels = rev(rownames(varSubset)))
    if (dim(varSubset)[2] < 3) {
        if (dim(varSubset)[2] > 1) 
            plotObj <- plotObj[plotObj$ind == levels(plotObj$ind)[1], 
                ]
        out <- dotplot(Var ~ values, data = plotObj, as.table = TRUE, 
            xlab = "Importance", ...)
    }
    else {
        out <- dotplot(Var ~ values, data = plotObj, groups = plotObj$ind, 
            auto.key = list(columns = min(3, length(levels(plotObj$ind)))), 
            as.table = TRUE, xlab = "Importance", ...)
    }
    out
}


bag <- function (x, ...) 
UseMethod("bag")


SLC14_1 <- function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", 
    corrValue = 0) 
{
    dat <- matrix(rnorm(n * 20, sd = 3), ncol = 20)
    foo <- function(x) x[1] + sin(x[2]) + log(abs(x[3])) + x[4]^2 + 
        x[5] * x[6] + ifelse(x[7] * x[8] * x[9] < 0, 1, 0) + 
        ifelse(x[10] > 0, 1, 0) + x[11] * ifelse(x[11] > 0, 1, 
        0) + sqrt(abs(x[12])) + cos(x[13]) + 2 * x[14] + abs(x[15]) + 
        ifelse(x[16] < -1, 1, 0) + x[17] * ifelse(x[17] < -1, 
        1, 0) - 2 * x[18] - x[19] * x[20]
    dat <- as.data.frame(dat)
    colnames(dat) <- well_numbered("Var", ncol(dat))
    if (noiseVars > 0 | corrVars > 0) 
        dat <- cbind(dat, make_noise(n = n, noiseVars = noiseVars, 
            corrVars = corrVars, corrType = corrType, corrValue = corrValue))
    dat$y <- apply(dat[, 1:20], 1, foo) + rnorm(n, sd = 3)
    dat
}


varImp <- function (object, ...) 
{
    UseMethod("varImp")
}


nzv <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, 
    names = FALSE) 
{
    if (is.null(dim(x))) 
        x <- matrix(x, ncol = 1)
    freqRatio <- apply(x, 2, function(data) {
        t <- table(data[!is.na(data)])
        if (length(t) <= 1) {
            return(0)
        }
        w <- which.max(t)
        return(max(t, na.rm = TRUE)/max(t[-w], na.rm = TRUE))
    })
    lunique <- apply(x, 2, function(data) length(unique(data[!is.na(data)])))
    percentUnique <- 100 * lunique/apply(x, 2, length)
    zeroVar <- (lunique == 1) | apply(x, 2, function(data) all(is.na(data)))
    if (saveMetrics) {
        out <- data.frame(freqRatio = freqRatio, percentUnique = percentUnique, 
            zeroVar = zeroVar, nzv = (freqRatio > freqCut & percentUnique <= 
                uniqueCut) | zeroVar)
    }
    else {
        out <- which((freqRatio > freqCut & percentUnique <= 
            uniqueCut) | zeroVar)
        names(out) <- NULL
        if (names) {
            out <- colnames(x)[out]
        }
    }
    out
}


gafs_tourSelection <- function (population, fitness, k = 3, ...) 
{
    popSize <- nrow(population)
    sel <- rep(NA, popSize)
    for (i in 1:popSize) {
        s <- sample(1:popSize, size = k)
        sel[i] <- s[which.max(fitness[s])]
    }
    out <- list(population = population[sel, , drop = FALSE], 
        fitness = fitness[sel])
    return(out)
}


SLC14_2 <- function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", 
    corrValue = 0) 
{
    dat <- matrix(rnorm(n * 200, sd = 4), ncol = 200)
    dat <- as.data.frame(dat)
    colnames(dat) <- well_numbered("Var", ncol(dat))
    if (noiseVars > 0 | corrVars > 0) 
        dat <- cbind(dat, make_noise(n = n, noiseVars = noiseVars, 
            corrVars = corrVars, corrType = corrType, corrValue = corrValue))
    dat$y <- apply(dat[, 1:200], 1, function(x) sum(log(abs(x)))) + 
        rnorm(n, sd = 5) - 1
    dat
}


sbfIter <- function (x, y, testX, testY, sbfControl = sbfControl(), ...) 
{
    if (is.null(colnames(x))) 
        stop("x must have column names")
    if (is.null(testX) | is.null(testY)) 
        stop("a test set must be specified")
    if (sbfControl$multivariate) {
        scores <- sbfControl$functions$score(x, y)
        if (length(scores) != ncol(x)) 
            stop(paste("when control$multivariate == TRUE, 'scores'", 
                "should return a vector with", ncol(x), "numeric values"))
    }
    else {
        scores <- apply(x, 2, sbfControl$functions$score, y = y)
    }
    retained <- sbfControl$functions$filter(scores, x, y)
    testX <- testX[, which(retained), drop = FALSE]
    fitObject <- sbfControl$functions$fit(x[, which(retained), 
        drop = FALSE], y, ...)
    modelPred <- sbfControl$functions$pred(fitObject, testX)
    if (is.data.frame(modelPred) | is.matrix(modelPred)) {
        if (is.matrix(modelPred)) 
            modelPred <- as.data.frame(modelPred)
        modelPred$obs <- testY
    }
    else modelPred <- data.frame(pred = modelPred, obs = testY)
    list(variables = names(retained)[which(retained)], pred = modelPred)
}


precision <- function (data, ...) 
UseMethod("precision")


ldaFuncs <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, first, last, ...) 
{
    loadNamespace("MASS")
    MASS::lda(x, y, ...)
}, pred = function (object, x) 
{
    tmp <- predict(object, x)
    out <- cbind(data.frame(pred = tmp$class), as.data.frame(tmp$posterior))
    out
}, rank = function (object, x, y) 
{
    vimp <- filterVarImp(x, y, TRUE)
    vimp$Overall <- apply(vimp, 1, mean)
    vimp <- vimp[order(vimp$Overall, decreasing = TRUE), ]
    vimp <- as.data.frame(vimp)[, "Overall", drop = FALSE]
    vimp$var <- rownames(vimp)
    vimp
}, selectSize = function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}, selectVar = function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}), .Names = c("summary", "fit", "pred", "rank", "selectSize", 
"selectVar"))


posPredValue <- function (data, ...) 
{
    UseMethod("posPredValue")
}


cforestStats <- function (x) 
getModelInfo("cforest", regex = FALSE)[[1]]$oob(x)


icr <- function (x, ...) 
UseMethod("icr")


treebagSBF <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, ...) 
{
    if (ncol(x) > 0) {
        loadNamespace("ipred")
        ipred::ipredbagg(y, x, ...)
    }
    else nullModel(y = y)
}, pred = function (object, x) 
{
    if (class(object) == "nullModel") {
        tmp <- predict(object, x)
        if (!is.null(object$levels)) {
            out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
                x, type = "prob")))
        }
        else out <- tmp
    }
    else {
        tmp <- predict(object, x)
        if (is.factor(object$y)) {
            out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
                x, type = "prob")))
        }
        else out <- tmp
    }
    out
}, score = function (x, y) 
{
    anovaScores(x, y)
}, filter = function (score, x, y) 
score <= 0.05), .Names = c("summary", "fit", "pred", "score", 
"filter"))


dummyVars <- function (formula, ...) 
{
    UseMethod("dummyVars")
}


gamFormula <- function (data, smoother = "s", cut = 8, y = "y") 
{
    nzv <- nearZeroVar(data)
    if (length(nzv) > 0) 
        data <- data[, -nzv, drop = FALSE]
    numValues <- apply(data, 2, function(x) length(unique(x)))
    prefix <- rep("", ncol(data))
    prefix[numValues > cut] <- paste(smoother, "(", sep = "")
    suffix <- rep("", ncol(data))
    suffix[numValues > cut] <- ")"
    rhs <- paste(prefix, names(numValues), suffix, sep = "")
    rhs <- paste(rhs, collapse = "+")
    form <- as.formula(paste(y, "~", rhs, sep = ""))
    form
}


recall <- function (data, ...) 
UseMethod("recall")


plsBag <- structure(list(fit = function (x, y, ...) 
{
    loadNamespace("pls")
    caret::plsda(x, y, ...)
}, pred = function (object, x) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    predict(object, x, type = "prob")[, , ]
}, aggregate = function (x, type = "class") 
{
    pooled <- x[[1]] * NA
    classes <- colnames(pooled)
    for (i in 1:ncol(pooled)) {
        tmp <- lapply(x, function(y, col) y[, col], col = i)
        tmp <- do.call("rbind", tmp)
        pooled[, i] <- apply(tmp, 2, median)
    }
    if (type == "class") {
        out <- factor(classes[apply(pooled, 1, which.max)], levels = classes)
    }
    else out <- as.data.frame(pooled)
    out
}), .Names = c("fit", "pred", "aggregate"))


checkConditionalX <- function (x, y) 
{
    x$.outcome <- y
    unique(unlist(dlply(x, .(.outcome), zeroVar)))
}


knnreg <- function (x, ...) 
UseMethod("knnreg")


plot.rfe <- function (x, metric = x$metric, ...) 
{
    x$results$Selected <- ""
    x$results$Selected[x$results$Variables == x$bestSubset] <- "*"
    results <- x$results[, colnames(x$results) %in% c("Variables", 
        "Selected", metric)]
    metric <- metric[which(metric %in% colnames(results))]
    plotForm <- as.formula(paste(metric, "~ Variables"))
    panel.profile <- function(x, y, groups, ...) {
        panel.xyplot(x, y, ...)
        panel.xyplot(x[groups == "*"], y[groups == "*"], pch = 16)
    }
    resampText <- resampName(x, FALSE)
    resampText <- paste(metric, resampText)
    out <- xyplot(plotForm, data = results, groups = Selected, 
        panel = panel.profile, ylab = resampText, ...)
    out
}


lmSBF <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, ...) 
{
    if (ncol(x) > 0) {
        tmp <- as.data.frame(x)
        tmp$y <- y
        lm(y ~ ., data = tmp)
    }
    else nullModel(y = y)
}, pred = function (object, x) 
{
    predict(object, x)
}, score = function (x, y) 
{
    anovaScores(y, x)
}, filter = function (score, x, y) 
score <= 0.05), .Names = c("summary", "fit", "pred", "score", 
"filter"))


R2 <- function (pred, obs, formula = "corr", na.rm = FALSE) 
{
    n <- sum(complete.cases(pred))
    switch(formula, corr = cor(obs, pred, use = ifelse(na.rm, 
        "complete.obs", "everything"))^2, traditional = 1 - (sum((obs - 
        pred)^2, na.rm = na.rm)/((n - 1) * var(obs, na.rm = na.rm))))
}


resampleSummary <- function (obs, resampled, index = NULL, keepData = TRUE) 
{
    numPred <- apply(resampled, 2, function(u) sum(!is.na(u)))
    if (all(numPred >= 2)) {
        performanceStats <- apply(resampled, 2, postResample, 
            obs = obs)
        out <- c(apply(performanceStats, 1, mean, na.rm = TRUE), 
            apply(performanceStats, 1, sd, na.rm = TRUE))
        if (keepData) {
            if (is.factor(obs)) {
                outResample <- data.frame(obs = rep(obs, dim(resampled)[2]), 
                  pred = factor(unlist(lapply(resampled, as.character)), 
                    levels = levels(obs)), group = paste("Resample", 
                    rep(1:dim(resampled)[2], each = dim(resampled)[1], 
                      sep = "")))
            }
            else {
                outResample <- data.frame(obs = rep(obs, dim(resampled)[2]), 
                  pred = unlist(lapply(resampled, I)), group = paste("Resample", 
                    rep(1:dim(resampled)[2], each = dim(resampled)[1], 
                      sep = "")))
            }
        }
        else outResample <- NULL
    }
    else {
        pred <- apply(resampled, 2, function(u) u[!is.na(u)])
        if (is.factor(obs)) 
            pred <- factor(as.character(pred), levels = levels(obs))
        tmp <- postResample(pred, obs)
        tmp2 <- tmp * 0
        out <- c(tmp, tmp * 0)
        outResample <- data.frame(obs = obs, pred = pred, group = "Resample1")
    }
    if (keepData) 
        outResample <- outResample[!is.na(outResample$pred), 
            ]
    list(metrics = out, data = outResample)
}


createTimeSlices <- function (y, initialWindow, horizon = 1, fixedWindow = TRUE, 
    skip = 0) 
{
    stops <- seq(initialWindow, (length(y) - horizon), by = skip + 
        1)
    if (fixedWindow) {
        starts <- stops - initialWindow + 1
    }
    else {
        starts <- rep(1, length(stops))
    }
    train <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
    test <- mapply(seq, stops + 1, stops + horizon, SIMPLIFY = FALSE)
    nums <- gsub(" ", "0", format(stops))
    names(train) <- paste("Training", nums, sep = "")
    names(test) <- paste("Testing", nums, sep = "")
    out <- list(train = train, test = test)
    out
}


bagEarthStats <- function (x) 
getModelInfo("bagEarth", regex = FALSE)[[1]]$oob(x)


compare_models <- function (a, b, metric = a$metric[1]) 
{
    mods <- list(a, b)
    rs <- resamples(mods)
    diffs <- diff(rs, metric = metric[1])
    diffs$statistics[[1]][[1]]
}


rfe <- function (x, ...) 
UseMethod("rfe")


rfSBF <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, ...) 
{
    if (ncol(x) > 0) {
        loadNamespace("randomForest")
        randomForest::randomForest(x, y, ...)
    }
    else nullModel(y = y)
}, pred = function (object, x) 
{
    if (class(object) == "nullModel") {
        tmp <- predict(object, x)
        if (!is.null(object$levels)) {
            out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
                x, type = "prob")))
        }
        else out <- tmp
    }
    else {
        tmp <- predict(object, x)
        if (is.factor(object$y)) {
            out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
                x, type = "prob")))
        }
        else out <- tmp
    }
    out
}, score = function (x, y) 
{
    if (is.factor(y)) 
        anovaScores(x, y)
    else gamScores(x, y)
}, filter = function (score, x, y) 
score <= 0.05), .Names = c("summary", "fit", "pred", "score", 
"filter"))


ipredStats <- function (x) 
getModelInfo("treebag", regex = FALSE)[[1]]$oob(x)


svmBag <- structure(list(fit = function (x, y, ...) 
{
    loadNamespace("kernlab")
    out <- kernlab::ksvm(as.matrix(x), y, prob.model = is.factor(y), 
        ...)
    out
}, pred = function (object, x) 
{
    if (is.character(lev(object))) {
        out <- predict(object, as.matrix(x), type = "probabilities")
        colnames(out) <- lev(object)
        rownames(out) <- NULL
    }
    else out <- predict(object, as.matrix(x))[, 1]
    out
}, aggregate = function (x, type = "class") 
{
    if (is.matrix(x[[1]]) | is.data.frame(x[[1]])) {
        pooled <- x[[1]] & NA
        classes <- colnames(pooled)
        for (i in 1:ncol(pooled)) {
            tmp <- lapply(x, function(y, col) y[, col], col = i)
            tmp <- do.call("rbind", tmp)
            pooled[, i] <- apply(tmp, 2, median)
        }
        if (type == "class") {
            out <- factor(classes[apply(pooled, 1, which.max)], 
                levels = classes)
        }
        else out <- as.data.frame(pooled)
    }
    else {
        x <- matrix(unlist(x), ncol = length(x))
        out <- apply(x, 1, median)
    }
    out
}), .Names = c("fit", "pred", "aggregate"))


lmFuncs <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, first, last, ...) 
{
    tmp <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    tmp$y <- y
    lm(y ~ ., data = tmp)
}, pred = function (object, x) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    predict(object, x)
}, rank = function (object, x, y) 
{
    coefs <- abs(coef(object))
    coefs <- coefs[names(coefs) != "(Intercept)"]
    coefs[is.na(coefs)] <- 0
    vimp <- data.frame(Overall = unname(coefs), var = names(coefs))
    rownames(vimp) <- names(coefs)
    vimp
}, selectSize = function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}, selectVar = function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}), .Names = c("summary", "fit", "pred", "rank", "selectSize", 
"selectVar"))


avNNet <- function (x, ...) 
UseMethod("avNNet")


findCorrelation <- function (x, cutoff = 0.9, verbose = FALSE, names = FALSE, exact = ncol(x) < 
    100) 
{
    if (names & is.null(colnames(x))) 
        stop("'x' must have column names when `names = TRUE`")
    out <- if (exact) 
        findCorrelation_exact(x = x, cutoff = cutoff, verbose = verbose)
    else findCorrelation_fast(x = x, cutoff = cutoff, verbose = verbose)
    out
    if (names) 
        out <- colnames(x)[out]
    out
}


probFunction <- function (method, modelFit, newdata = NULL, preProc = NULL, param = NULL) 
{
    if (!is.null(newdata) && !is.null(preProc)) 
        newdata <- predict(preProc, newdata)
    obsLevels <- levels(modelFit)
    classProb <- method$prob(modelFit = modelFit, newdata = newdata, 
        submodels = param)
    if (!is.data.frame(classProb) & is.null(param)) {
        classProb <- as.data.frame(classProb)
        if (!is.null(obsLevels)) 
            classprob <- classProb[, obsLevels]
    }
    classProb
}


classDist <- function (x, ...) 
UseMethod("classDist")


caretSA <- structure(list(fit = function (x, y, lev = NULL, last = FALSE, 
    ...) 
train(x, y, ...), pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (object$control$classProbs) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, fitness_intern = function (object, x, y, maximize, p) 
{
    perf_val <- getTrainPerf(object)
    perf_val <- perf_val[names(perf_val) != "method"]
    perf_val <- unlist(perf_val)
    names(perf_val) <- gsub("Train", "", names(perf_val))
    perf_val
}, fitness_extern = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, initial = function (vars, prob = 0.2, ...) 
{
    sort(sample.int(vars, size = floor(vars * prob) + 1))
}, perturb = function (x, vars, number = floor(vars * 0.01) + 
    1) 
{
    bin <- index2vec(x, vars)
    change <- sample(seq(along = bin), size = number)
    bin[change] <- ifelse(bin[change] == 1, 0, 1)
    sort(which(bin == 1))
}, prob = function (old, new, iteration = 1) 
{
    if (new < old) 
        return(1)
    ediff <- as.vector(old - new)
    ediff <- ediff/abs(old)
    exp(ediff * iteration)
}, selectIter = function (x, metric, maximize) 
{
    bestIter <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    bestIter
}), .Names = c("fit", "pred", "fitness_intern", "fitness_extern", 
"initial", "perturb", "prob", "selectIter"))


groupKFold <- function (group, k = length(unique(group))) 
{
    if (k > length(unique(group))) 
        stop(paste("`k` should be less than", length(unique(group))))
    dat <- data.frame(index = seq(along = group), group = group)
    groups <- data.frame(group = unique(dat$group))
    group_folds <- createFolds(groups$group, returnTrain = TRUE, 
        k = k)
    group_folds <- lapply(group_folds, function(x, y) y[x, , 
        drop = FALSE], y = groups)
    dat_folds <- lapply(group_folds, function(x, y) merge(x, 
        y), y = dat)
    lapply(dat_folds, function(x) sort(x$index))
}


trainControl <- function (method = "boot", number = ifelse(grepl("cv", method), 
    10, 25), repeats = ifelse(grepl("cv", method), 1, number), 
    p = 0.75, search = "grid", initialWindow = NULL, horizon = 1, 
    fixedWindow = TRUE, skip = 0, verboseIter = FALSE, returnData = TRUE, 
    returnResamp = "final", savePredictions = FALSE, classProbs = FALSE, 
    summaryFunction = defaultSummary, selectionFunction = "best", 
    preProcOptions = list(thresh = 0.95, ICAcomp = 3, k = 5, 
        freqCut = 95/5, uniqueCut = 10, cutoff = 0.9), sampling = NULL, 
    index = NULL, indexOut = NULL, indexFinal = NULL, timingSamps = 0, 
    predictionBounds = rep(FALSE, 2), seeds = NA, adaptive = list(min = 5, 
        alpha = 0.05, method = "gls", complete = TRUE), trim = FALSE, 
    allowParallel = TRUE) 
{
    if (is.null(selectionFunction)) 
        stop("null selectionFunction values not allowed")
    if (!(returnResamp %in% c("all", "final", "none"))) 
        stop("incorrect value of returnResamp")
    if (length(predictionBounds) > 0 && length(predictionBounds) != 
        2) 
        stop("'predictionBounds' should be a logical or numeric vector of length 2")
    if (any(names(preProcOptions) == "method")) 
        stop("'method' cannot be specified here")
    if (any(names(preProcOptions) == "x")) 
        stop("'x' cannot be specified here")
    if (!(adaptive$method %in% c("gls", "BT"))) 
        stop("incorrect value of adaptive$method")
    if (adaptive$alpha < 1e-07 | adaptive$alpha > 1) 
        stop("incorrect value of adaptive$alpha")
    if (grepl("adapt", method)) {
        num <- if (method == "adaptive_cv") 
            number * repeats
        else number
        if (adaptive$min >= num) 
            stop(paste("adaptive$min should be less than", num))
        if (adaptive$min <= 1) 
            stop("adaptive$min should be greater than 1")
    }
    if (!(search %in% c("grid", "random"))) 
        stop("`search` should be either 'grid' or 'random'")
    if (method == "oob" & any(names(match.call()) == "summaryFunction")) {
        warning("Custom summary measures cannot be computed for out-of-baf resampling. ", 
            "This value of `summaryFunction` will be ignored.", 
            call. = FALSE)
    }
    list(method = method, number = number, repeats = repeats, 
        search = search, p = p, initialWindow = initialWindow, 
        horizon = horizon, fixedWindow = fixedWindow, skip = skip, 
        verboseIter = verboseIter, returnData = returnData, returnResamp = returnResamp, 
        savePredictions = savePredictions, classProbs = classProbs, 
        summaryFunction = summaryFunction, selectionFunction = selectionFunction, 
        preProcOptions = preProcOptions, sampling = sampling, 
        index = index, indexOut = indexOut, indexFinal = indexFinal, 
        timingSamps = timingSamps, predictionBounds = predictionBounds, 
        seeds = seeds, adaptive = adaptive, trim = trim, allowParallel = allowParallel)
}


lift <- function (x, ...) 
UseMethod("lift")


summary.bagEarth <- function (object, ...) 
{
    requireNamespaceQuietStop("earth")
    oobStat <- apply(object$oob, 2, function(x) quantile(x, probs = c(0, 
        0.025, 0.25, 0.5, 0.75, 0.975, 1)))
    numTerms <- unlist(lapply(object$fit, function(x) length(x$selected.terms)))
    numVar <- unlist(lapply(object$fit, function(x) {
        imp <- rownames(earth::evimp(x, trim = FALSE))
        imp <- imp[!grepl("-unused", imp)]
        imp
    }))
    modelInfo <- cbind(numTerms, numVar)
    colnames(modelInfo) <- c("Num Terms", "Num Variables")
    out <- list(modelInfo = modelInfo, oobStat = oobStat, bagEarthCall = object$call)
    class(out) <- "summary.bagEarth"
    out
}


panel.lift2 <- function (x, y, pct = 0, values = NULL, ...) 
{
    polyx <- c(0, pct, 100, 0)
    polyy <- c(0, 100, 100, 0)
    regionStyle <- trellis.par.get("reference.line")
    panel.polygon(polyx, polyy, col = regionStyle$col, border = regionStyle$col)
    panel.xyplot(x, y, ...)
    if (!is.null(values)) {
        theDots <- list(...)
        if (any(names(theDots) == "groups")) {
            dat <- data.frame(x = x, y = y, groups = theDots$groups)
            ung <- unique(dat$groups)
            for (i in seq(along = ung)) {
                dat0 <- subset(dat, groups == ung[i])
                plotRef(dat0$x, dat0$y, values, iter = i)
            }
        }
        else plotRef(x, y, values)
    }
}


negPredValue <- function (data, ...) 
{
    UseMethod("negPredValue")
}


nbBag <- structure(list(fit = function (x, y, ...) 
{
    loadNamespace("klaR")
    klaR::NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)
}, pred = function (object, x) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    as.data.frame(predict(object, x)$posterior)
}, aggregate = function (x, type = "class") 
{
    pooled <- x[[1]] * NA
    classes <- colnames(pooled)
    for (i in 1:ncol(pooled)) {
        tmp <- lapply(x, function(y, col) y[, col], col = i)
        tmp <- do.call("rbind", tmp)
        pooled[, i] <- apply(tmp, 2, median)
    }
    if (type == "class") {
        out <- factor(classes[apply(pooled, 1, which.max)], levels = classes)
    }
    else out <- as.data.frame(pooled)
    out
}), .Names = c("fit", "pred", "aggregate"))


plot.train <- function (x, plotType = "scatter", metric = x$metric[1], digits = getOption("digits") - 
    3, xTrans = NULL, nameInStrip = FALSE, ...) 
{
    if (!(plotType %in% c("level", "scatter", "line"))) 
        stop("plotType must be either level, scatter or line")
    cutpoints <- c(0, 1.9, 2.9, 3.9, Inf)
    prettyVal <- function(u, dig, Name = NULL) {
        if (is.numeric(u)) {
            if (!is.null(Name)) 
                u <- paste(gsub(".", " ", Name, fixed = TRUE), 
                  ": ", format(u, digits = dig), sep = "")
            return(factor(u))
        }
        else return(if (!is.factor(u)) as.factor(u) else u)
    }
    params <- x$modelInfo$parameters$parameter
    if (grepl("adapt", x$control$method)) 
        warning("When using adaptive resampling, this plot may not accurately capture the relationship between the tuning parameters and model performance.")
    plotIt <- "yes"
    if (all(params == "parameter")) {
        plotIt <- "There are no tuning parameters for this model."
    }
    else {
        dat <- x$results
        if (x$method == "nb") 
            dat$usekernel <- factor(ifelse(dat$usekernel, "Nonparametric", 
                "Gaussian"))
        if (x$method == "gam") 
            dat$select <- factor(ifelse(dat$select, "Feature Selection", 
                "No Feature Selection"))
        if (x$method == "qrnn") 
            dat$bag <- factor(ifelse(dat$bag, "Bagging", "No Bagging"))
        if (x$method == "C5.0") 
            dat$winnow <- factor(ifelse(dat$winnow, "Winnowing", 
                "No Winnowing"))
        if (x$method == "M5") 
            dat$rules <- factor(ifelse(dat$rules == "Yes", "Rules", 
                "Trees"))
        paramValues <- apply(dat[, params, drop = FALSE], 2, 
            function(x) length(unique(x)))
        if (any(paramValues > 1)) {
            params <- names(paramValues)[paramValues > 1]
        }
        else plotIt <- "There are no tuning parameters with more than 1 value."
    }
    if (plotIt == "yes") {
        p <- length(params)
        dat <- dat[, c(metric, params)]
        if (p > 1) {
            numUnique <- unlist(lapply(dat[, -1], function(x) length(unique(x))))
            numUnique <- sort(numUnique, decreasing = TRUE)
            dat <- dat[, c(metric, names(numUnique))]
            params <- names(numUnique)
        }
        if (!is.null(xTrans) & plotType == "scatter") 
            dat[, 2] <- xTrans(dat[, 2])
        resampText <- resampName(x, FALSE)
        if (plotType %in% c("line", "scatter")) {
            if (plotType == "scatter") {
                if (p >= 2) 
                  for (i in 3:ncol(dat)) dat[, i] <- prettyVal(dat[, 
                    i], dig = digits, Name = if (i > 3) 
                    params[i - 1]
                  else NULL)
            }
            else {
                for (i in 2:ncol(dat)) dat[, i] <- prettyVal(dat[, 
                  i], dig = digits, Name = if (i > 3) 
                  params[i - 1]
                else NULL)
            }
            for (i in 2:ncol(dat)) if (is.logical(dat[, i])) 
                dat[, i] <- factor(dat[, i])
            if (p > 2 & nameInStrip) {
                strip_vars <- params[-(1:2)]
                strip_lab <- subset(x$modelInfo$parameters, parameter %in% 
                  strip_vars)$label
                for (i in seq_along(strip_vars)) dat[, strip_vars[i]] <- factor(paste(strip_lab[i], 
                  dat[, strip_vars[i]], sep = ": "))
            }
            form <- if (p <= 2) {
                as.formula(paste(metric, "~", params[1], sep = ""))
            }
            else as.formula(paste(metric, "~", params[1], "|", 
                paste(params[-(1:2)], collapse = "*"), sep = ""))
            defaultArgs <- list(x = form, data = dat, groups = if (p > 
                1) dat[, params[2]] else NULL)
            if (length(list(...)) > 0) 
                defaultArgs <- c(defaultArgs, list(...))
            lNames <- names(defaultArgs)
            if (!("ylab" %in% lNames)) 
                defaultArgs$ylab <- paste(metric, resampText)
            if (!("type" %in% lNames) & plotType == "scatter") 
                defaultArgs$type <- c("g", "o")
            if (!("type" %in% lNames) & plotType == "line") 
                defaultArgs$type <- c("g", "o")
            if (p > 1) {
                groupCols <- 4
                if (length(unique(dat[, 3])) < 4) 
                  groupCols <- length(unique(dat[, 3]))
                if (length(unique(dat[, 3])) %in% 5:6) 
                  groupCols <- 3
                groupCols <- as.numeric(cut(length(unique(dat[, 
                  3])), cutpoints, include.lowest = TRUE))
                if (!(any(c("key", "auto.key") %in% lNames))) 
                  defaultArgs$auto.key <- list(columns = groupCols, 
                    lines = TRUE, title = as.character(x$modelInfo$parameter$label)[x$modelInfo$parameter$parameter == 
                      params[2]], cex.title = 1)
            }
            if (!("xlab" %in% lNames)) 
                defaultArgs$xlab <- as.character(x$modelInfo$parameter$label)[x$modelInfo$parameter$parameter == 
                  params[1]]
            if (plotType == "scatter") {
                out <- do.call("xyplot", defaultArgs)
            }
            else {
                out <- do.call("stripplot", defaultArgs)
            }
        }
        if (plotType == "level") {
            if (p == 1) 
                stop("There must be at least 2 tuning parameters with multiple values")
            for (i in 2:ncol(dat)) dat[, i] <- prettyVal(dat[, 
                i], dig = digits, Name = if (i > 3) 
                params[i - 1]
            else NULL)
            if (p > 2 & nameInStrip) {
                strip_vars <- params[-(1:2)]
                strip_lab <- subset(x$modelInfo$parameters, parameter %in% 
                  strip_vars)$label
                for (i in seq_along(strip_vars)) dat[, strip_vars[i]] <- factor(paste(strip_lab[i], 
                  dat[, strip_vars[i]], sep = ": "))
            }
            form <- if (p <= 2) {
                as.formula(paste(metric, "~", params[1], "*", 
                  params[2], sep = ""))
            }
            else as.formula(paste(metric, "~", params[1], "*", 
                params[2], "|", paste(params[-(1:2)], collapse = "*"), 
                sep = ""))
            defaultArgs <- list(x = form, data = dat)
            if (length(list(...)) > 0) 
                defaultArgs <- c(defaultArgs, list(...))
            lNames <- names(defaultArgs)
            if (!("sub" %in% lNames)) 
                defaultArgs$sub <- paste(metric, resampText)
            if (!("xlab" %in% lNames)) 
                defaultArgs$xlab <- as.character(x$modelInfo$parameter$label)[x$modelInfo$parameter$parameter == 
                  params[1]]
            if (!("ylab" %in% lNames)) 
                defaultArgs$ylab <- as.character(x$modelInfo$parameter$label)[x$modelInfo$parameter$parameter == 
                  params[2]]
            out <- do.call("levelplot", defaultArgs)
        }
    }
    else stop(plotIt)
    out
}


resampleWrapper <- function (x, ind) 
{
    out <- rep(NA, dim(x$data)[1])
    trainData <- x$data
    x$data <- x$data[ind, ]
    tmpModelFit <- do.call(createModel, x)
    outBagData <- trainData[-ind, ]
    outBagData$.outcome <- NULL
    out[-ind] <- if (is.factor(x$data$.outcome)) {
        as.character(predictionFunction(x$method, tmpModelFit, 
            outBagData))
    }
    else {
        predictionFunction(x$method, tmpModelFit, outBagData)
    }
    out
}


bagFDA <- function (x, ...) 
UseMethod("bagFDA")


featurePlot <- function (x, y, plot = if (is.factor(y)) "strip" else "scatter", 
    labels = c("Feature", ""), ...) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    numFeat <- dim(x)[2]
    if (plot != "pairs") {
        stackX <- stack(x)
        stackX$.y <- rep(y, numFeat)
    }
    else {
        if (!is.factor(y)) {
            x <- data.frame(cbind(x, y))
        }
    }
    if (is.factor(y)) {
        featPlot <- switch(tolower(plot), strip = stripplot(values ~ 
            .y | ind, stackX, xlab = labels[1], ylab = labels[2], 
            ...), box = , boxplot = bwplot(values ~ .y | ind, 
            stackX, xlab = labels[1], ylab = labels[2], ...), 
            density = densityplot(~values | ind, stackX, groups = stackX$.y, 
                xlab = labels[1], ylab = labels[2], ...), pairs = splom(~x, 
                groups = y, ...), ellipse = splom(~x, groups = y, 
                panel = function(x, y, groups, subscripts, ...) {
                  requireNamespaceQuietStop("ellipse")
                  lineInfo <- trellis.par.get("superpose.line")
                  pointInfo <- trellis.par.get("superpose.symbol")
                  uniqueGroups <- sort(unique(groups))
                  for (i in seq(along = uniqueGroups)) {
                    id <- which(groups[subscripts] == uniqueGroups[i])
                    panel.xyplot(x[id], y[id], pch = pointInfo$pch[i], 
                      col = pointInfo$col[i], cex = pointInfo$cex[i], 
                      ...)
                    groupVar <- var(cbind(x[id], y[id]))
                    groupMean <- cbind(mean(x[id]), mean(y[id]))
                    groupEllipse <- ellipse::ellipse(groupVar, 
                      centre = groupMean, level = 0.95)
                    panel.xyplot(groupEllipse[, 1], groupEllipse[, 
                      2], type = "l", col = lineInfo$col[i], 
                      lty = lineInfo$lty[i], ...)
                  }
                }, ...))
    }
    else {
        featPlot <- switch(tolower(plot), scatter = , xyplot = xyplot(.y ~ 
            values | ind, stackX, scales = list(x = list(relation = "free")), 
            xlab = labels[1], ylab = labels[2], ...), pairs = splom(~x, 
            ...))
    }
    featPlot
}


nbSBF <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, ...) 
{
    if (ncol(x) > 0) {
        loadNamespace("klaR")
        klaR::NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)
    }
    else nullModel(y = y)
}, pred = function (object, x) 
{
    if (class(object) == "nullModel") {
        tmp <- predict(object, x)
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else {
        tmp <- predict(object, x)
        out <- cbind(data.frame(pred = tmp$class), as.data.frame(tmp$posterior))
    }
    out
}, pred = function (object, x) 
{
    predict(object, x)$class
}, score = function (x, y) 
{
    anovaScores(x, y)
}, filter = function (score, x, y) 
score <= 0.05), .Names = c("summary", "fit", "pred", "pred", 
"score", "filter"))


confusionMatrix.train <- function (data, norm = "overall", dnn = c("Prediction", "Reference"), 
    ...) 
{
    if (data$control$method %in% c("oob", "LOOCV", "none")) 
        stop("cannot compute confusion matrices for leave-one-out, out-of-bag resampling, or no resampling")
    if (inherits(data, "train")) {
        if (data$modelType == "Regression") 
            stop("confusion matrices are only valid for classification models")
        lev <- levels(data)
        resampledCM <- train_resampledCM(data)
    }
    else {
        lev <- data$obsLevels
        if (inherits(data, "rfe")) 
            resampledCM <- rfe_resampledCM(data)
        if (inherits(data, "sbf")) 
            resampledCM <- sbf_resampledCM(data)
    }
    if (!is.null(data$control$index)) {
        resampleN <- unlist(lapply(data$control$index, length))
        numResamp <- length(resampleN)
        resampText <- resampName(data)
    }
    else {
        resampText <- ""
        numResamp <- 0
    }
    counts <- as.matrix(resampledCM[, grep("^\\.?cell", colnames(resampledCM))])
    norm <- match.arg(norm, c("none", "overall", "average"))
    if (norm == "none") 
        counts <- matrix(apply(counts, 2, sum), nrow = length(lev))
    else counts <- matrix(apply(counts, 2, mean), nrow = length(lev))
    if (norm == "overall") 
        counts <- counts/sum(counts) * 100
    rownames(counts) <- colnames(counts) <- lev
    names(dimnames(counts)) <- dnn
    out <- list(table = as.table(counts), norm = norm, B = length(data$control$index), 
        text = paste(resampText, "Confusion Matrix"))
    class(out) <- paste0("confusionMatrix.", class(data))
    out
}


gafs_rwSelection <- function (population, fitness, ...) 
{
    popSize <- nrow(population)
    prob <- abs(fitness)/sum(abs(fitness))
    sel <- sample(1:popSize, size = popSize, prob = pmin(pmax(0, 
        prob), 1, na.rm = TRUE), replace = TRUE)
    out <- list(population = population[sel, , drop = FALSE], 
        fitness = fitness[sel])
    return(out)
}


predict.bagEarth <- function (object, newdata = NULL, type = "response", ...) 
{
    if (!any(type %in% c("response", "class", "prob"))) 
        stop("type must be either response, class or prob")
    requireNamespaceQuietStop("earth")
    getTrainPred <- function(x) {
        oobIndex <- seq(along = x$fitted.values)
        oobIndex <- oobIndex[!(oobIndex %in% unique(x$index))]
        data.frame(pred = x$fitted.values[oobIndex], sample = oobIndex)
    }
    if (is.null(newdata) & !is.null(object$x)) 
        newdata <- object$x
    if (is.null(newdata)) {
        pred <- lapply(object$fit, getTrainPred)
        pred <- rbind.fill(pred)
        out <- ddply(pred, .(sample), function(x) object$summary(x$pred))$V1
    }
    else {
        pred <- lapply(object$fit, function(x, y) {
            if (is.null(x$glm.list)) 
                predict(x, newdata = y)
            else predict(x, newdata = y, type = "response")
        }, y = newdata)
        out <- matrix(unlist(pred), ncol = object$B)
        out <- apply(out, 1, object$summary, na.rm = TRUE)
    }
    if (!all(is.na(object$levels))) {
        if (type == "prob") {
            out <- cbind(1 - out, out)
            colnames(out) <- object$levels
        }
        else {
            if (type == "class") {
                out <- factor(ifelse(out > 0.5, object$levels[2], 
                  object$levels[1]), levels = object$levels)
            }
        }
    }
    out
}


sbf <- function (x, ...) 
UseMethod("sbf")


pickVars <- function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}


specificity <- function (data, ...) 
{
    UseMethod("specificity")
}


bagEarth <- function (x, ...) 
UseMethod("bagEarth")


safs_perturb <- function (x, vars, number = floor(vars * 0.01) + 1) 
{
    bin <- index2vec(x, vars)
    change <- sample(seq(along = bin), size = number)
    bin[change] <- ifelse(bin[change] == 1, 0, 1)
    sort(which(bin == 1))
}


resampleHist <- function (object, type = "density", ...) 
{
    if (object$control$method == "oob") 
        stop("out-of-bag error rate was selected. This plot cannot be created")
    if (is.null(object$resample)) 
        stop("No resample values were found. This plot cannot be created")
    resample <- object$resample
    tuneNames <- as.character(object$modelInfo$parameter$parameter)
    if (any(names(resample) %in% tuneNames)) {
        bestTune <- object$bestTune
        colnames(bestTune) <- gsub("^\\.", "", colnames(bestTune))
        resample <- merge(bestTune, resample)
        resample <- resample[, !(names(resample) %in% tuneNames), 
            drop = FALSE]
    }
    results <- melt(resample, id.vars = "Resample")
    if (type == "density") {
        out <- densityplot(~value | variable, data = results, 
            scales = list(relation = "free"), xlab = "", as.table = TRUE, 
            ...)
    }
    else {
        out <- histogram(~value | variable, data = results, scales = list(relation = "free"), 
            as.table = TRUE, xlab = "", ...)
    }
    out
}


extractProb <- function (models, testX = NULL, testY = NULL, unkX = NULL, unkOnly = !is.null(unkX) & 
    is.null(testX), verbose = FALSE) 
{
    objectNames <- names(models)
    if (is.null(objectNames)) 
        objectNames <- paste("Object", 1:length(models), sep = "")
    if (any(unlist(lapply(models, function(x) is.null(x$modelInfo$prob))))) 
        stop("only classification models that produce probabilities are allowed")
    obsLevels <- levels(models[[1]])
    if (!unkOnly) {
        trainX <- models[[1]]$trainingData[, !(colnames(models[[1]]$trainingData) %in% 
            ".outcome")]
        trainY <- models[[1]]$trainingData$.outcome
    }
    if (verbose) {
        cat("Number of training samples:", length(trainY), "\n")
        cat("Number of test samples:    ", length(testY), "\n\n")
    }
    predProb <- predClass <- obs <- modelName <- dataType <- objName <- NULL
    if (!is.null(testX)) {
        if (!is.data.frame(testX)) 
            testX <- as.data.frame(testX)
        hasNa <- apply(testX, 1, function(data) any(is.na(data)))
        if (verbose) 
            cat("There were ", sum(hasNa), "rows with missing values\n\n")
        flush.console()
    }
    for (i in seq(along = models)) {
        if (verbose) 
            cat("starting ", models[[i]]$method, "\n")
        flush.console()
        if (!unkOnly) {
            tempTrainProb <- probFunction(models[[i]]$modelInfo, 
                models[[i]]$finalModel, trainX, models[[i]]$preProcess)
            tempTrainPred <- apply(tempTrainProb, 1, which.max)
            tempTrainPred <- colnames(tempTrainProb)[tempTrainPred]
            tempTrainPred <- factor(tempTrainPred, levels = obsLevels)
            if (verbose) 
                cat(models[[i]]$method, ":", length(tempTrainPred), 
                  "training predictions were added\n")
            flush.console()
            predProb <- if (is.null(predProb)) 
                tempTrainProb
            else rbind(predProb, tempTrainProb)
            predClass <- c(predClass, as.character(tempTrainPred))
            obs <- c(obs, as.character(trainY))
            modelName <- c(modelName, rep(models[[i]]$method, 
                length(tempTrainPred)))
            objName <- c(objName, rep(objectNames[[i]], length(tempTrainPred)))
            dataType <- c(dataType, rep("Training", length(tempTrainPred)))
            if (!is.null(testX) & !is.null(testY)) {
                if (!is.data.frame(testX)) 
                  testX <- as.data.frame(testX)
                tempX <- testX
                tempY <- testY
                tempX$.outcome <- NULL
                tempTestProb <- probFunction(models[[i]]$modelInfo, 
                  models[[i]]$finalModel, tempX, models[[i]]$preProcess)
                tempTestPred <- apply(tempTestProb, 1, which.max)
                tempTestPred <- colnames(tempTestProb)[tempTestPred]
                tempTestPred <- factor(tempTestPred, levels = obsLevels)
                if (verbose) 
                  cat(models[[i]]$method, ":", length(tempTestPred), 
                    "test predictions were added\n")
                predProb <- if (is.null(predProb)) 
                  tempTestProb
                else rbind(predProb, tempTestProb)
                predClass <- c(predClass, as.character(tempTestPred))
                obs <- c(obs, as.character(testY))
                modelName <- c(modelName, rep(models[[i]]$method, 
                  length(tempTestPred)))
                objName <- c(objName, rep(objectNames[[i]], length(tempTestPred)))
                dataType <- c(dataType, rep("Test", length(tempTestPred)))
            }
        }
        if (!is.null(unkX)) {
            if (!is.data.frame(unkX)) 
                unkX <- as.data.frame(unkX)
            tempX <- unkX
            tempX$.outcome <- NULL
            tempUnkProb <- probFunction(models[[i]]$modelInfo, 
                models[[i]]$finalModel, tempX, models[[i]]$preProcess)
            tempUnkPred <- apply(tempUnkProb, 1, which.max)
            tempUnkPred <- colnames(tempUnkProb)[tempUnkPred]
            tempUnkPred <- factor(tempUnkPred, levels = obsLevels)
            if (verbose) 
                cat(models[[i]]$method, ":", length(tempUnkPred), 
                  "unknown predictions were added\n")
            predProb <- if (is.null(predProb)) 
                tempUnkProb
            else rbind(predProb, tempUnkProb)
            predClass <- c(predClass, as.character(tempUnkPred))
            obs <- c(obs, rep(NA, length(tempUnkPred)))
            modelName <- c(modelName, rep(models[[i]]$method, 
                length(tempUnkPred)))
            objName <- c(objName, rep(objectNames[[i]], length(tempUnkPred)))
            dataType <- c(dataType, rep("Unknown", length(tempUnkPred)))
        }
        if (verbose) 
            cat("\n")
    }
    predClass <- factor(predClass, levels = obsLevels)
    obs <- factor(obs, levels = obsLevels)
    out <- data.frame(predProb)
    out$obs <- obs
    out$pred <- predClass
    out$model <- modelName
    out$dataType <- dataType
    out$object <- objName
    out
}


rfGA <- structure(list(fit = function (x, y, lev = NULL, last = FALSE, 
    ...) 
{
    loadNamespace("randomForest")
    randomForest::randomForest(x, y, ...)
}, pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (is.factor(object$y)) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, fitness_intern = function (object, x, y, maximize, p) 
rfStats(object), fitness_extern = function (data, lev = NULL, 
    model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, initial = function (vars, popSize, ...) 
{
    x <- matrix(NA, nrow = popSize, ncol = vars)
    probs <- seq(0.9, 0.1, length = popSize)
    for (i in 1:popSize) {
        x[i, ] <- sample(0:1, replace = TRUE, size = vars, prob = c(probs[i], 
            1 - probs[i]))
    }
    var_count <- apply(x, 1, sum)
    if (any(var_count == 0)) {
        for (i in which(var_count == 0)) {
            x[i, ] <- sample(0:1, replace = TRUE, size = vars)
        }
    }
    x
}, selection = function (population, fitness, r = NULL, q = NULL, 
    ...) 
{
    popSize = nrow(population)
    if (is.null(r)) 
        r <- 2/(popSize * (popSize - 1))
    if (is.null(q)) 
        q <- 2/popSize
    rank <- (popSize + 1) - rank(fitness, ties.method = "random")
    prob <- q - (rank - 1) * r
    sel <- sample(1:popSize, size = popSize, prob = pmin(pmax(0, 
        prob), 1, na.rm = TRUE), replace = TRUE)
    out <- list(population = population[sel, , drop = FALSE], 
        fitness = fitness[sel])
    out
}, crossover = function (population, fitness, parents, ...) 
{
    fitness <- fitness[parents]
    parents <- population[parents, , drop = FALSE]
    n <- ncol(parents)
    children <- matrix(as.double(NA), nrow = 2, ncol = n)
    fitnessChildren <- rep(NA, 2)
    crossOverPoint <- sample(0:n, size = 1)
    if (crossOverPoint == 0) {
        children[1:2, ] <- parents[2:1, ]
        fitnessChildren[1:2] <- fitness[2:1]
    }
    else if (crossOverPoint == n) {
        children <- parents
        fitnessChildren <- fitness
    }
    else {
        children[1, ] <- c(parents[1, 1:crossOverPoint], parents[2, 
            (crossOverPoint + 1):n])
        children[2, ] <- c(parents[2, 1:crossOverPoint], parents[1, 
            (crossOverPoint + 1):n])
    }
    out <- list(children = children, fitness = fitnessChildren)
    out
}, mutation = function (population, parent, ...) 
{
    mutate <- parent <- as.vector(population[parent, ])
    n <- length(parent)
    j <- sample(1:n, size = 1)
    mutate[j] <- abs(mutate[j] - 1)
    mutate
}, selectIter = function (x, metric, maximize) 
{
    bestIter <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    bestIter
}), .Names = c("fit", "pred", "fitness_intern", "fitness_extern", 
"initial", "selection", "crossover", "mutation", "selectIter"
))


tolerance <- function (x, metric, tol = 1.5, maximize) 
{
    index <- 1:nrow(x)
    if (!maximize) {
        best <- min(x[, metric])
        perf <- (x[, metric] - best)/best * 100
        candidates <- index[perf < tol]
        bestIter <- min(candidates)
    }
    else {
        best <- max(x[, metric])
        perf <- (x[, metric] - best)/best * -100
        candidates <- index[perf < tol]
        bestIter <- min(candidates)
    }
    bestIter
}


createMultiFolds <- function (y, k = 10, times = 5) 
{
    if (class(y)[1] == "Surv") 
        y <- y[, "time"]
    prettyNums <- paste("Rep", gsub(" ", "0", format(1:times)), 
        sep = "")
    for (i in 1:times) {
        tmp <- createFolds(y, k = k, list = TRUE, returnTrain = TRUE)
        names(tmp) <- paste("Fold", gsub(" ", "0", format(seq(along = tmp))), 
            ".", prettyNums[i], sep = "")
        out <- if (i == 1) 
            tmp
        else c(out, tmp)
    }
    out
}


gamFuncs <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, first, last, ...) 
{
    loaded <- search()
    gamLoaded <- any(loaded == "package:gam")
    if (gamLoaded) 
        detach(package:gam)
    loadNamespace("mgcv")
    gam <- get("gam", asNamespace("mgcv"))
    dat <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    dat$y <- y
    args <- list(formula = gamFormula(x, smoother = "s", y = "y"), 
        data = dat, family = if (!is.factor(y)) gaussian else binomial)
    do.call("gam", args)
}, pred = function (object, x) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    loaded <- search()
    gamLoaded <- any(loaded == "package:gam")
    if (gamLoaded) 
        detach(package:gam)
    loadNamespace("mgcv")
    rsp <- predict(object, newdata = x, type = "response")
    if (object$family$family == "binomial") {
        lvl <- levels(object$model$y)
        out <- data.frame(p1 = rsp, p2 = 1 - rsp, pred = factor(ifelse(rsp > 
            0.5, lvl[2], lvl[1]), levels = lvl))
        colnames(out)[1:2] <- make.names(lvl)
        out
    }
    else out <- data.frame(pred = rsp)
    out
}, rank = function (object, x, y) 
{
    loaded <- search()
    gamLoaded <- any(loaded == "package:gam")
    if (gamLoaded) 
        detach(package:gam)
    loadNamespace("mgcv")
    vimp <- varImp(object)
    vimp$var <- rownames(vimp)
    if (any(!(colnames(x) %in% rownames(vimp)))) {
        missing <- colnames(x)[!(colnames(x) %in% rownames(vimp))]
        tmpdf <- data.frame(var = missing, Overall = rep(0, length(missing)))
        vimp <- rbind(vimp, tmpdf)
    }
    vimp
}, selectSize = function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}, selectVar = function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}), .Names = c("summary", "fit", "pred", "rank", "selectSize", 
"selectVar"))


anovaScores <- function (x, y) 
{
    if (is.factor(x)) 
        stop("The predictors should be numeric")
    pv <- try(anova(lm(x ~ y), test = "F")[1, "Pr(>F)"], silent = TRUE)
    if (any(class(pv) == "try-error") || is.na(pv) || is.nan(pv)) 
        pv <- 1
    pv
}


print.train <- function (x, printCall = FALSE, details = FALSE, selectCol = FALSE, 
    showSD = FALSE, ...) 
{
    if (!is.null(x$modelInfo$label)) 
        cat(x$modelInfo$label, "\n\n")
    if (printCall) 
        printCall(x$call)
    if (!is.null(x$trainingData)) {
        chDim <- dim(x$trainingData)
        chDim[2] <- chDim[2] - 1
        if (x$modelType == "Classification") {
            lev <- levels(x)
            if (is.character(lev)) 
                chDim <- c(chDim, length(lev))
        }
        else lev <- NULL
        chDim <- format(chDim)
        cat(chDim[1], " samples", sep = "")
        if (!is.null(x$control$indexFinal)) 
            cat(",", length(x$control$indexFinal), "used for final model\n")
        else cat("\n")
        cat(chDim[2], " predictor", ifelse(chDim[2] > 1, "s\n", 
            "\n"), sep = "")
        if (is.character(lev)) {
            cat(chDim[3], "classes:", paste("'", lev, "'", sep = "", 
                collapse = ", "), "\n")
        }
        cat("\n")
    }
    if (!is.null(x$preProc)) {
        pp_list(x$preProc$method)
    }
    else cat("No pre-processing\n")
    if (!is.null(x$control$index)) {
        resampleN <- unlist(lapply(x$control$index, length))
        numResamp <- length(resampleN)
        resampText <- resampName(x)
        cat("Resampling:", resampText, "\n")
        if (x$control$method != "none") {
            outLabel <- x$metric
            resampleN <- as.character(resampleN)
            if (numResamp > 5) 
                resampleN <- c(resampleN[1:6], "...")
            cat("Summary of sample sizes:", paste(resampleN, 
                collapse = ", "), "\n")
        }
    }
    if (!is.null(x$control$sampling)) {
        cat("Addtional sampling using ")
        cat(switch(x$control$sampling$name, down = "down-sampling", 
            up = "up-sampling", smote = "SMOTE", rose = "ROSE", 
            custom = "a custom function"))
        if (!is.null(x$preProc)) {
            if (x$control$sampling$first) 
                cat(" prior to pre-processing")
            else cat(" after to pre-processing")
        }
        cat("\n\n")
    }
    if (x$control$method != "none") {
        tuneAcc <- x$results
        tuneAcc <- tuneAcc[, names(tuneAcc) != "parameter"]
        cat("Resampling results")
        if (dim(tuneAcc)[1] > 1) 
            cat(" across tuning parameters")
        if (showSD) 
            cat(" (values below are 'mean (sd)')")
        cat(":\n\n")
        if (dim(tuneAcc)[1] > 1) {
            numParam <- length(x$bestTune)
            finalTune <- x$bestTune
            optValues <- paste(names(finalTune), "=", format(finalTune, 
                ...))
            optString <- paste0("The final ", ifelse(numParam > 
                1, "values", "value"), " used for the model ", 
                ifelse(numParam > 1, "were ", "was "), stringFunc(optValues), 
                ".")
            finalTune$Selected <- "*"
            if (any(names(tuneAcc) %in% "method")) 
                names(tuneAcc)[names(tuneAcc) %in% "method"] <- ".method"
            if (any(names(finalTune) %in% "method")) 
                names(finalTune)[names(finalTune) %in% "method"] <- ".method"
            tuneAcc <- merge(tuneAcc, finalTune, all.x = TRUE)
            if (any(names(tuneAcc) %in% ".method")) 
                names(tuneAcc)[names(tuneAcc) %in% ".method"] <- "method"
            tuneAcc$Selected[is.na(tuneAcc$Selected)] <- ""
        }
        else optString <- ""
        sdCols <- grep("SD$", colnames(tuneAcc))
        if (showSD) {
            sdCheck <- unlist(lapply(tuneAcc[, sdCols, drop = FALSE], 
                function(u) all(is.na(u))))
            if (any(sdCheck)) {
                rmCols <- names(sdCheck)[sdCheck]
                tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% rmCols)]
            }
        }
        else {
            if (length(sdCols) > 0) 
                tuneAcc <- tuneAcc[, -sdCols, drop = FALSE]
        }
        params <- names(x$bestTune)
        if (!all(params == "parameter")) {
            numVals <- apply(tuneAcc[, params, drop = FALSE], 
                2, function(x) length(unique(x)))
            if (any(numVals < 2)) {
                constString <- NULL
                for (i in seq(along = numVals)) {
                  if (numVals[i] == 1) 
                    constString <- c(constString, paste0("Tuning parameter '", 
                      names(numVals)[i], "' was held constant at a value of ", 
                      stringFunc(tuneAcc[1, names(numVals)[i]])))
                }
                discard <- names(numVals)[which(numVals == 1)]
                tuneAcc <- tuneAcc[, !(names(tuneAcc) %in% discard), 
                  drop = FALSE]
            }
            else constString <- NULL
        }
        else constString <- NULL
        tuneAcc <- tuneAcc[, !grepl("Apparent$|Optimism$", names(tuneAcc)), 
            drop = FALSE]
        colnames(tuneAcc)[colnames(tuneAcc) == ".B"] <- "Resamples"
        nms <- names(tuneAcc)[names(tuneAcc) %in% params]
        sort_args <- vector(mode = "list", length = length(nms))
        for (i in seq(along = nms)) {
            sort_args[[i]] <- tuneAcc[, nms[i]]
        }
        tune_ord <- do.call("order", sort_args)
        if (!is.null(tune_ord)) 
            tuneAcc <- tuneAcc[tune_ord, , drop = FALSE]
        theDots <- list(...)
        theDots$x <- tuneAcc
        printMat <- do.call("format.data.frame", theDots)
        printMat <- as.matrix(printMat)
        rownames(printMat) <- rep("", dim(printMat)[1])
        if (showSD) {
            sdCols <- grep("SD$", colnames(printMat), value = TRUE)
            sd_dat <- printMat[, sdCols, drop = FALSE]
            printMat <- printMat[, !(colnames(printMat) %in% 
                sdCols), drop = FALSE]
            for (col_name in sdCols) {
                not_sd <- gsub("SD$", "", col_name)
                if (any(colnames(printMat) == not_sd)) {
                  printMat[, not_sd] <- paste0(printMat[, not_sd], 
                    " (", sd_dat[, col_name], ")")
                }
            }
        }
        if (!selectCol) 
            printMat <- printMat[, colnames(printMat) != "Selected", 
                drop = FALSE]
        print(printMat, quote = FALSE, print.gap = 2)
        cat("\n")
        if (!is.null(constString)) {
            cat(truncateText(paste(constString, collapse = "\n")))
            cat("\n")
        }
        if (dim(tuneAcc)[1] > 1) {
            if (is.null(x$update)) {
                met <- paste(x$metric, "was used to select the optimal model using")
                if (is.function(x$control$selectionFunction)) {
                  met <- paste(met, " a custom selection rule.\n")
                }
                else {
                  met <- paste(met, switch(x$control$selectionFunction, 
                    best = paste(" the", ifelse(x$maximize, "largest", 
                      "smallest"), "value.\n"), oneSE = " the one SE rule.\n", 
                    tolerance = " a tolerance rule.\n"))
                }
            }
            else {
                met <- paste("The tuning", ifelse(ncol(x$bestTune) > 
                  1, "parameters", "parameter"), "was set manually.\n")
            }
            cat(truncateText(met))
        }
        cat(truncateText(optString))
        if (nzchar(optString)) 
            cat("\n")
    }
    else printMat <- NULL
    if (details) {
        if (!(x$method %in% c("gbm", "treebag", "nb", "lvq", 
            "knn"))) {
            cat("\n----------------------------------------------------------\n")
            cat("\nThe final model:\n\n")
            switch(x$method, lm = , nnet = , multinom = , pls = , 
                earth = , lmStepAIC = , bagEarth = , bagFDA = print(summary(x$finalModel)), 
                rpart = , ctree = , ctree2 = , cforest = , glmboost = , 
                gamboost = , blackboost = , ada = , randomForest = , 
                pcaNNet = , svmradial = , svmpoly = , svmRadial = , 
                svmPoly = , rvmRadial = , rvmPoly = , lssvmRadial = , 
                lssvmPoly = , gaussprRadial = , gaussprPoly = , 
                enet = , lasso = , LMT = , JRip = , lda = , rda = , 
                pamr = , gpls = , J48 = , ppr = print(x$finalModel), 
                fda = {
                  print(x$finalModel)
                  cat("\n Summary of Terms\n\n")
                  print(x$finalModel$fit)
                })
        }
    }
    invisible(printMat)
}


MeanSD <- function (x, exclude = NULL) 
{
    if (!is.null(exclude)) 
        x <- x[, !(colnames(x) %in% exclude), drop = FALSE]
    out <- c(colMeans(x, na.rm = TRUE), sapply(x, sd, na.rm = TRUE))
    names(out)[-(1:ncol(x))] <- paste(names(out)[-(1:ncol(x))], 
        "SD", sep = "")
    out
}


nbFuncs <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, first, last, ...) 
{
    loadNamespace("klaR")
    klaR::NaiveBayes(x, y, usekernel = TRUE, fL = 2, ...)
}, pred = function (object, x) 
{
    tmp <- predict(object, x)
    out <- cbind(data.frame(pred = tmp$class), as.data.frame(tmp$posterior))
    out
}, rank = function (object, x, y) 
{
    vimp <- filterVarImp(x, y)
    if (is.factor(y)) {
        avImp <- apply(vimp, 1, mean)
        vimp$Overall <- avImp
    }
    vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
    vimp$var <- rownames(vimp)
    vimp
}, selectSize = function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}, selectVar = function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}), .Names = c("summary", "fit", "pred", "rank", "selectSize", 
"selectVar"))


progress <- function (x, names, iter, start = TRUE) 
{
    text <- paste(ifelse(start, "+ ", "- "), names[iter], ": ", 
        paste(colnames(x), x, sep = "=", collapse = ", "), sep = "")
    cat(text, "\n")
}


hasTerms <- function (x) 
{
    objNames <- c(names(x), slotNames(x))
    "terms" %in% tolower(objNames)
}


well_numbered <- function (prefix, items) 
{
    paste0(prefix, gsub(" ", "0", format(1:items)))
}


F_meas <- function (data, ...) 
UseMethod("F_meas")


RMSE <- function (pred, obs, na.rm = FALSE) 
sqrt(mean((pred - obs)^2, na.rm = na.rm))


predict.train <- function (object, newdata = NULL, type = "raw", na.action = na.omit, 
    ...) 
{
    if (all(names(object) != "modelInfo")) {
        object <- update(object, param = NULL)
    }
    if (!is.null(object$modelInfo$library)) 
        for (i in object$modelInfo$library) do.call("require", 
            list(package = i))
    if (!(type %in% c("raw", "prob"))) 
        stop("type must be either \"raw\" or \"prob\"")
    if (type == "prob") {
        if (is.null(object$modelInfo$prob)) 
            stop("only classification models that produce probabilities are allowed")
    }
    if (!is.null(newdata)) {
        if (inherits(object, "train.formula")) {
            newdata <- as.data.frame(newdata)
            rn <- row.names(newdata)
            Terms <- delete.response(object$terms)
            m <- model.frame(Terms, newdata, na.action = na.action, 
                xlev = object$xlevels)
            if (!is.null(cl <- attr(Terms, "dataClasses"))) 
                .checkMFClasses(cl, m)
            keep <- match(row.names(m), rn)
            newdata <- model.matrix(Terms, m, contrasts = object$contrasts)
            xint <- match("(Intercept)", colnames(newdata), nomatch = 0)
            if (xint > 0) 
                newdata <- newdata[, -xint, drop = FALSE]
        }
    }
    else if (object$control$method != "oob") {
        if (!is.null(object$trainingData)) {
            if (object$method == "pam") {
                newdata <- object$finalModel$xData
            }
            else {
                newdata <- object$trainingData
                newdata$.outcome <- NULL
                if ("train.formula" %in% class(object) && any(unlist(lapply(newdata, 
                  is.factor)))) {
                  newdata <- model.matrix(~., data = newdata)[, 
                    -1]
                  newdata <- as.data.frame(newdata)
                }
            }
        }
        else stop("please specify data via newdata")
    }
    if ("xNames" %in% names(object$finalModel) & is.null(object$preProcess$method$pca) & 
        is.null(object$preProcess$method$ica)) 
        newdata <- newdata[, colnames(newdata) %in% object$finalModel$xNames, 
            drop = FALSE]
    if (type == "prob") {
        out <- probFunction(method = object$modelInfo, modelFit = object$finalModel, 
            newdata = newdata, preProc = object$preProcess)
        obsLevels <- levels(object)
        out <- out[, obsLevels, drop = FALSE]
    }
    else {
        out <- predictionFunction(method = object$modelInfo, 
            modelFit = object$finalModel, newdata = newdata, 
            preProc = object$preProcess)
        if (object$modelType == "Regression") {
            out <- trimPredictions(pred = out, mod_type = object$modelType, 
                bounds = object$control$predictionBounds, limits = object$yLimit)
        }
        else {
            if (!("levels" %in% names(object))) 
                object$levels <- levels(object)
            out <- outcome_conversion(as.character(out), lv = object$levels)
        }
    }
    out
}


plsda <- function (x, ...) 
UseMethod("plsda")


index2vec <- function (x, vars, sign = FALSE) 
{
    bin <- rep(0, vars)
    bin[x] <- 1
    if (sign) 
        bin <- ifelse(bin == 0, -1, 1)
    bin
}


modelLookup <- function (model = NULL) 
{
    load(system.file("models", "models.RData", package = "caret"))
    if (!is.null(model)) {
        if (!(model %in% names(models))) 
            stop(paste("Model '", method, "' is not in the ", 
                "set of existing models", sep = ""))
        models <- models[model == names(models)]
    }
    out <- lapply(models, function(x) {
        out <- x$parameters[, c("parameter", "label")]
        out$forReg <- "Regression" %in% x$type
        out$forClass <- "Classification" %in% x$type
        out$probModel <- !is.null(x$prob)
        out
    })
    for (i in seq(along = out)) out[[i]]$model <- names(models)[i]
    out <- do.call("rbind", out)
    rownames(out) <- NULL
    out <- out[, c("model", "parameter", "label", "forReg", "forClass", 
        "probModel")]
    out[order(out$model), ]
}


bagControl <- function (fit = NULL, predict = NULL, aggregate = NULL, downSample = FALSE, 
    oob = TRUE, allowParallel = TRUE) 
{
    list(fit = fit, predict = predict, aggregate = aggregate, 
        downSample = downSample, oob = oob, allowParallel = allowParallel)
}


pickSizeBest <- function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}


extractPrediction <- function (models, testX = NULL, testY = NULL, unkX = NULL, unkOnly = !is.null(unkX) & 
    is.null(testX), verbose = FALSE) 
{
    objectNames <- names(models)
    if (is.null(objectNames)) 
        objectNames <- paste("Object", 1:length(models), sep = "")
    if (!unkOnly) {
        trainX <- models[[1]]$trainingData[, !(colnames(models[[1]]$trainingData) %in% 
            ".outcome"), drop = FALSE]
        trainY <- models[[1]]$trainingData$.outcome
    }
    obsLevels <- levels(models[[1]])
    if (verbose) {
        cat("Number of training samples:", length(trainY), "\n")
        cat("Number of test samples:    ", length(testY), "\n\n")
    }
    pred <- obs <- modelName <- dataType <- objName <- NULL
    if (!is.null(testX)) {
        hasNa <- apply(testX, 1, function(data) any(is.na(data)))
        if (verbose) 
            cat("There were ", sum(hasNa), "rows with missing values\n\n")
    }
    for (i in seq(along = models)) {
        if (!unkOnly) {
            tempTrainPred <- predictionFunction(models[[i]]$modelInfo, 
                models[[i]]$finalModel, trainX, models[[i]]$preProcess)
            if (verbose) 
                cat(models[[i]]$method, ":", length(tempTrainPred), 
                  "training predictions were added\n")
            if (models[[i]]$modelType == "Classification") {
                pred <- c(pred, as.character(tempTrainPred))
                obs <- c(obs, as.character(trainY))
            }
            else {
                tempTrainPred <- trimPredictions(mod_type = models[[i]]$modelType, 
                  bounds = models[[i]]$control$predictionBounds, 
                  limits = models[[i]]$yLimit, pred = tempTrainPred)
                pred <- c(pred, tempTrainPred)
                obs <- c(obs, trainY)
            }
            modelName <- c(modelName, rep(models[[i]]$method, 
                length(tempTrainPred)))
            objName <- c(objName, rep(objectNames[[i]], length(tempTrainPred)))
            dataType <- c(dataType, rep("Training", length(tempTrainPred)))
            if (!is.null(testX) & !is.null(testY)) {
                if (any(colnames(testX) == ".outcome")) 
                  testX <- testX[, colnames(testX) != ".outcome", 
                    drop = FALSE]
                tempTestPred <- predictionFunction(models[[i]]$modelInfo, 
                  models[[i]]$finalModel, testX, models[[i]]$preProcess)
                if (verbose) 
                  cat(models[[i]]$method, ":", length(tempTestPred), 
                    "test predictions were added\n")
                if (models[[i]]$modelType == "Classification") {
                  pred <- c(pred, as.character(tempTestPred))
                  obs <- c(obs, as.character(testY))
                }
                else {
                  tempTestPred <- trimPredictions(mod_type = models[[i]]$modelType, 
                    bounds = models[[i]]$control$predictionBounds, 
                    limits = models[[i]]$yLimit, pred = tempTestPred)
                  pred <- c(pred, tempTestPred)
                  obs <- c(obs, testY)
                }
                modelName <- c(modelName, rep(models[[i]]$method, 
                  length(tempTestPred)))
                objName <- c(objName, rep(objectNames[[i]], length(tempTestPred)))
                dataType <- c(dataType, rep("Test", length(tempTestPred)))
            }
            if (verbose) 
                cat("\n")
        }
        if (!is.null(unkX)) {
            if (any(colnames(unkX) == ".outcome")) 
                unkX <- unkX[, colnames(unkX) != ".outcome", 
                  drop = FALSE]
            tempUnkPred <- predictionFunction(models[[i]]$modelInfo, 
                models[[i]]$finalModel, unkX, models[[i]]$preProcess)
            if (verbose) 
                cat(models[[i]]$method, ":", length(tempUnkPred), 
                  "unknown predictions were added\n")
            if (models[[i]]$modelType == "Classification") {
                pred <- c(pred, as.character(tempUnkPred))
                obs <- c(obs, rep("", length(tempUnkPred)))
            }
            else {
                tempUnkPred <- trimPredictions(mod_type = models[[i]]$modelType, 
                  bounds = models[[i]]$control$predictionBounds, 
                  limits = models[[i]]$yLimit, pred = tempUnkPred)
                pred <- c(pred, tempUnkPred)
                obs <- c(obs, rep(NA, length(tempUnkPred)))
            }
            modelName <- c(modelName, rep(models[[i]]$method, 
                length(tempUnkPred)))
            objName <- c(objName, rep(objectNames[[i]], length(tempUnkPred)))
            dataType <- c(dataType, rep("Unknown", length(tempUnkPred)))
        }
        if (verbose) 
            cat("\n")
    }
    if (models[[1]]$modelType == "Classification") {
        pred <- factor(pred, levels = obsLevels)
        obs <- factor(obs, levels = obsLevels)
    }
    data.frame(obs = obs, pred = pred, model = modelName, dataType = dataType, 
        object = objName)
}


cluster <- function (x, ...) 
UseMethod("cluster")


sortImp <- function (object, top) 
{
    if (object$calledFrom == "varImp") {
        best <- switch(object$model, pam = "maxabs", "max")
    }
    else {
        best <- "max"
    }
    featureRank <- switch(best, max = rank(-apply(object$importance, 
        1, max, na.rm = TRUE)), min = rank(apply(object$importance, 
        1, min, na.rm = TRUE)), maxabs = rank(-apply(abs(object$importance), 
        1, max, na.rm = TRUE)))
    tiedRanks <- as.numeric(names(table(featureRank)[table(featureRank) > 
        1]))
    if (length(tiedRanks) > 0) {
        for (i in seq(along = tiedRanks)) {
            tmp <- featureRank[featureRank == tiedRanks[i]]
            featureRank[featureRank == tiedRanks[i]] <- tmp + 
                runif(length(tmp), min = 0.001, max = 0.999)
        }
    }
    featureOrder <- order(featureRank)
    out <- object$importance[featureOrder, , drop = FALSE]
    out <- out[1:top, , drop = FALSE]
    out
}


train <- function (x, ...) 
{
    UseMethod("train")
}


confusionMatrix <- function (data, ...) 
{
    UseMethod("confusionMatrix")
}


gafsControl <- function (functions = NULL, method = "repeatedcv", metric = NULL, 
    maximize = NULL, number = ifelse(grepl("cv", method), 10, 
        25), repeats = ifelse(grepl("cv", method), 1, 5), verbose = FALSE, 
    returnResamp = "final", p = 0.75, index = NULL, indexOut = NULL, 
    seeds = NULL, holdout = 0, genParallel = FALSE, allowParallel = TRUE) 
{
    if (!(method %in% c("cv", "boot", "repeatedcv", "LGOCV", 
        "LOOCV"))) 
        stop("method should be one of: \"cv\", \"boot\", \"repeatedcv\", \"LGOCV\" or \"LOOCV\"")
    if (holdout < 0 | holdout >= 1) 
        stop("'holdout' should be in [0, 1)")
    if (!is.null(metric)) {
        if (length(metric) != 2) 
            stop("'metric' should be a two-element named vector. See ?gafsControl")
        if (is.null(names(metric)) || any(sort(names(metric)) != 
            c("external", "internal"))) 
            stop("'metric' should have names 'internal' and 'external' See ?gafsControl")
    }
    if (!is.null(maximize)) {
        if (length(maximize) != 2) 
            stop("'maximize' should be a two-element named vector. See ?gafsControl")
        if (is.null(names(maximize)) || any(sort(names(maximize)) != 
            c("external", "internal"))) 
            stop("'maximize' should have names 'internal' and 'external' See ?gafsControl")
    }
    list(functions = if (is.null(functions)) caretFuncs else functions, 
        method = method, metric = metric, maximize = maximize, 
        number = number, repeats = repeats, returnResamp = returnResamp, 
        verbose = verbose, p = p, index = index, indexOut = indexOut, 
        seeds = seeds, holdout = holdout, genParallel = genParallel, 
        allowParallel = allowParallel)
}


gafs_uCrossover <- function (population, parents, ...) 
{
    parents <- population[parents, , drop = FALSE]
    n <- ncol(parents)
    u <- runif(n)
    children <- parents
    children[1:2, u > 0.5] <- children[2:1, u > 0.5]
    out <- list(children = children, fitness = rep(NA, 2))
    return(out)
}


predictionFunction <- function (method, modelFit, newdata, preProc = NULL, param = NULL) 
{
    if (!is.null(newdata) && !is.null(preProc)) 
        newdata <- predict(preProc, newdata)
    out <- method$predict(modelFit = modelFit, newdata = newdata, 
        submodels = param)
    out
}


expoTrans <- function (y, ...) 
UseMethod("expoTrans")


gafs.default <- function (x, y, iters = 10, popSize = 50, pcrossover = 0.8, pmutation = 0.1, 
    elite = 0, suggestions = NULL, differences = TRUE, gafsControl = gafsControl(), 
    ...) 
{
    startTime <- proc.time()
    funcCall <- match.call(expand.dots = TRUE)
    if (is.null(gafsControl$metric)) 
        gafsControl$metric <- rep(ifelse(is.factor(y), "Accuracy", 
            "RMSE"), 2)
    if (is.null(gafsControl$maximize)) 
        gafsControl$maximize <- rep(ifelse(gafsControl$metric == 
            "RMSE", FALSE, TRUE), 2)
    if (is.null(names(gafsControl$metric))) 
        names(gafsControl$metric) <- c("internal", "external")
    if (is.null(names(gafsControl$maximize))) 
        names(gafsControl$maximize) <- c("internal", "external")
    if (nrow(x) != length(y)) 
        stop("there should be the same number of samples in x and y")
    numFeat <- ncol(x)
    classLevels <- levels(y)
    if (is.null(gafsControl$index)) 
        gafsControl$index <- switch(tolower(gafsControl$method), 
            cv = createFolds(y, gafsControl$number, returnTrain = TRUE), 
            repeatedcv = createMultiFolds(y, gafsControl$number, 
                gafsControl$repeats), loocv = createFolds(y, 
                length(y), returnTrain = TRUE), boot = , boot632 = createResample(y, 
                gafsControl$number), test = createDataPartition(y, 
                1, gafsControl$p), lgocv = createDataPartition(y, 
                gafsControl$number, gafsControl$p))
    if (is.null(names(gafsControl$index))) 
        names(gafsControl$index) <- getFromNamespace("prettySeq", 
            "caret")(gafsControl$index)
    if (is.null(gafsControl$indexOut)) {
        gafsControl$indexOut <- lapply(gafsControl$index, function(training, 
            allSamples) allSamples[-unique(training)], allSamples = seq(along = y))
        names(gafsControl$indexOut) <- getFromNamespace("prettySeq", 
            "caret")(gafsControl$indexOut)
    }
    if (!is.null(gafsControl$seeds)) {
        if (length(gafsControl$seeds) < length(gafsControl$index) + 
            1) 
            stop(paste("There must be at least", length(gafsControl$index) + 
                1, "random number seeds passed to gafsControl"))
    }
    else {
        gafsControl$seeds <- sample.int(1e+05, length(gafsControl$index) + 
            1)
    }
    testOutput <- data.frame(pred = sample(y, min(10, length(y))), 
        obs = sample(y, min(10, length(y))))
    if (is.factor(y)) 
        for (i in seq(along = classLevels)) testOutput[, classLevels[i]] <- runif(nrow(testOutput))
    test <- gafsControl$functions$fitness_extern(testOutput, 
        lev = classLevels)
    perfNames <- names(test)
    if (is.null(perfNames)) {
        warning(paste("The external fitness results should be a *named* vector;", 
            "new name(s) are", paste(paste0("external", 1:length(test)), 
                sep = "", collapse = ", ")), immediate. = TRUE)
        perfNames <- paste0("external", 1:length(test))
    }
    if (!(gafsControl$metric["external"] %in% perfNames)) {
        warning(paste("The metric '", gafsControl$metric["external"], 
            "' is not created by the summary function; '", perfNames[1], 
            "' will be used instead", sep = ""))
        gafsControl$metric["external"] <- perfNames[1]
    }
    `%op%` <- getOper(gafsControl$allowParallel && getDoParWorkers() > 
        1)
    result <- foreach(i = seq(along = gafsControl$index), .combine = "c", 
        .verbose = FALSE, .errorhandling = "stop") %op% {
        ga_select(x[gafsControl$index[[i]], , drop = FALSE], 
            y[gafsControl$index[[i]]], funcs = gafsControl$functions, 
            ga_maximize = gafsControl$maximize, ga_metric = gafsControl$metric, 
            iters = iters, popSize = popSize, pcrossover = pcrossover, 
            pmutation = pmutation, elite = elite, suggestions = suggestions, 
            ga_verbose = gafsControl$verbose, testX = x[gafsControl$indexOut[[i]], 
                , drop = FALSE], testY = y[gafsControl$indexOut[[i]]], 
            ga_seed = gafsControl$seeds[i], Resample = names(gafsControl$index)[i], 
            holdout = gafsControl$holdout, lvl = classLevels, 
            genParallel = gafsControl$genParallel, ...)
    }
    external <- result[names(result) == "external"]
    external <- do.call("rbind", external)
    rownames(external) <- NULL
    internal <- result[names(result) == "internal"]
    internal <- do.call("rbind", internal)
    rownames(internal) <- NULL
    selected_vars <- result[names(result) == "final"]
    names(selected_vars) <- names(gafsControl$index)
    if (differences) {
        diffs <- try(process_diffs(result[names(result) == "diffs"], 
            colnames(x)), silent = TRUE)
        if (class(diffs)[1] == "try-error") {
            diffs <- NULL
        }
    }
    else diffs <- NULL
    rm(result)
    if (gafsControl$verbose) 
        cat("+ final GA\n")
    if (gafsControl$holdout > 0) {
        in_holdout <- createDataPartition(y, p = gafsControl$holdout, 
            list = FALSE)
        in_model <- seq(along = y)[-unique(in_holdout)]
    }
    else {
        in_model <- seq(along = y)
        in_holdout <- NULL
    }
    final_ga <- ga_select(x[in_model, , drop = FALSE], y[in_model], 
        funcs = gafsControl$functions, ga_maximize = gafsControl$maximize, 
        ga_metric = gafsControl$metric, iters = iters, popSize = popSize, 
        pcrossover = pcrossover, pmutation = pmutation, elite = elite, 
        suggestions = suggestions, ga_verbose = gafsControl$verbose, 
        testX = if (!is.null(in_holdout)) 
            x[in_holdout, , drop = FALSE]
        else NULL, testY = if (!is.null(in_holdout)) 
            y[in_holdout]
        else NULL, ga_seed = gafsControl$seeds[length(gafsControl$seeds)], 
        lvl = classLevels, genParallel = gafsControl$genParallel, 
        ...)
    averages <- ddply(external, .(Iter), function(x, nms) {
        apply(x[, perfNames, drop = FALSE], 2, mean)
    }, nms = perfNames)
    if (!is.null(gafsControl$functions$selectIter)) {
        best_index <- gafsControl$functions$selectIter(averages, 
            metric = gafsControl$metric["external"], maximize = gafsControl$maximize["external"])
        best_iter <- averages$Iter[best_index]
        best_vars <- colnames(x)[final_ga$subsets[[best_index]]]
    }
    else {
        best_index <- if (gafsControl$maximize["external"]) 
            which.max(averages[, gafsControl$metric["external"]])
        else which.min(averages[, gafsControl$metric["external"]])
        best_iter <- averages$Iter[best_index]
        best_vars <- colnames(x)[final_ga$subsets[[best_index]]]
    }
    if (gafsControl$verbose) 
        cat("+ final model\n")
    fit <- gafsControl$functions$fit(x[, best_vars, drop = FALSE], 
        y, lev = lvls, last = TRUE, ...)
    endTime <- proc.time()
    res <- list(fit = fit, ga = final_ga, ga_param = list(popSize = popSize, 
        pcrossover = pcrossover, pmutation = pmutation, elite = elite), 
        external = external, internal = internal, resampled_vars = selected_vars, 
        averages = averages, iters = iters, optVariables = best_vars, 
        optIter = best_iter, control = gafsControl, dims = dim(x), 
        differences = diffs, perfNames = perfNames, auto = TRUE, 
        the_dots = list(...), call = funcCall, times = list(everything = endTime - 
            startTime), levels = if (is.factor(y)) classLevels else NULL)
    class(res) <- "gafs"
    res
}


minDiss <- function (u) 
min(u, na.rm = TRUE)


rfeControl <- function (functions = NULL, rerank = FALSE, method = "boot", 
    saveDetails = FALSE, number = ifelse(method %in% c("cv", 
        "repeatedcv"), 10, 25), repeats = ifelse(method %in% 
        c("cv", "repeatedcv"), 1, number), verbose = FALSE, returnResamp = "final", 
    p = 0.75, index = NULL, indexOut = NULL, timingSamps = 0, 
    seeds = NA, allowParallel = TRUE) 
{
    list(functions = if (is.null(functions)) caretFuncs else functions, 
        rerank = rerank, method = method, saveDetails = saveDetails, 
        number = number, repeats = repeats, returnResamp = returnResamp, 
        verbose = verbose, p = p, index = index, indexOut = indexOut, 
        timingSamps = timingSamps, seeds = seeds, allowParallel = allowParallel)
}


gafs_spCrossover <- function (population, fitness, parents, ...) 
{
    fitness <- fitness[parents]
    parents <- population[parents, , drop = FALSE]
    n <- ncol(parents)
    children <- matrix(as.double(NA), nrow = 2, ncol = n)
    fitnessChildren <- rep(NA, 2)
    crossOverPoint <- sample(0:n, size = 1)
    if (crossOverPoint == 0) {
        children[1:2, ] <- parents[2:1, ]
        fitnessChildren[1:2] <- fitness[2:1]
    }
    else if (crossOverPoint == n) {
        children <- parents
        fitnessChildren <- fitness
    }
    else {
        children[1, ] <- c(parents[1, 1:crossOverPoint], parents[2, 
            (crossOverPoint + 1):n])
        children[2, ] <- c(parents[2, 1:crossOverPoint], parents[1, 
            (crossOverPoint + 1):n])
    }
    out <- list(children = children, fitness = fitnessChildren)
    out
}


panel.needle <- function (x, y, horizontal = TRUE, pch = if (is.null(groups)) dot.symbol$pch else sup.symbol$pch, 
    col = if (is.null(groups)) dot.symbol$col else sup.symbol$col, 
    lty = dot.line$lty, lwd = dot.line$lwd, col.line = dot.line$col, 
    levels.fos = NULL, groups = NULL, ...) 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")
    if (horizontal) {
        yscale <- extendrange(y, f = 0.2)
        if (is.null(levels.fos)) 
            levels.fos <- floor(yscale[2]) - ceiling(yscale[1]) + 
                1
        panel.abline(v = 0, col = 1, lty = 1, lwd = 1)
        pch <- rep(pch, length(x))
        pch <- ifelse(x == 0, NA, pch)
        for (i in seq(along = x)) lsegments(x[i], y[i], 0, y[i])
        if (is.null(groups)) 
            panel.xyplot(x = x, y = y, col = col, pch = pch, 
                ...)
        else panel.superpose(x = x, y = y, groups = groups, col = col, 
            pch = pch, ...)
    }
    else {
        xscale <- extendrange(x, f = 0.2)
        if (is.null(levels.fos)) 
            levels.fos <- floor(xscale[2]) - ceiling(xscale[1]) + 
                1
        panel.abline(h = 0, col = col.line, lty = lty, lwd = lwd)
        pch <- rep(pch, length(x))
        pch <- ifelse(x == 0, NA, pch)
        if (is.null(groups)) 
            panel.xyplot(x = x, y = y, col = col, pch = pch, 
                ...)
        else panel.superpose(x = x, y = y, groups = groups, col = col, 
            pch = pch, ...)
    }
}


contr.ltfr <- function (n, contrasts = TRUE, sparse = FALSE) 
{
    if (is.numeric(n) && length(n) == 1L) {
        if (n > 1L) 
            levels <- as.character(seq_len(n))
        else stop("not enough degrees of freedom to define contrasts")
    }
    else {
        levels <- as.character(n)
        n <- length(n)
    }
    contr <- .RDiag(levels, sparse = sparse)
    if (contrasts) {
        if (n < 2L) 
            stop(gettextf("contrasts not defined for %d degrees of freedom", 
                n - 1L), domain = NA)
    }
    contr
}


nearZeroVar <- function (x, freqCut = 95/5, uniqueCut = 10, saveMetrics = FALSE, 
    names = FALSE, foreach = FALSE, allowParallel = TRUE) 
{
    if (!foreach) 
        return(nzv(x, freqCut = freqCut, uniqueCut = uniqueCut, 
            saveMetrics = saveMetrics, names = names))
    `%op%` <- getOper(foreach && allowParallel && getDoParWorkers() > 
        1)
    if (saveMetrics) {
        res <- foreach(name = colnames(x), .combine = rbind) %op% 
            {
                r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, 
                  saveMetrics = TRUE)
                r[, "column"] <- name
                r
            }
        res <- res[, c(5, 1, 2, 3, 4)]
        rownames(res) <- as.character(res$column)
        res$column <- NULL
    }
    else {
        res <- foreach(name = colnames(x), .combine = c) %op% 
            {
                r <- nzv(x[[name]], freqCut = freqCut, uniqueCut = uniqueCut, 
                  saveMetrics = FALSE)
                if (length(r) > 0 && r == 1) 
                  TRUE
                else FALSE
            }
        res <- which(res)
        if (names) {
            res <- colnames(x)[res]
        }
    }
    res
}


twoClassSummary <- function (data, lev = NULL, model = NULL) 
{
    lvls <- levels(data$obs)
    if (length(lvls) > 2) 
        stop(paste("Your outcome has", length(lvls), "levels. The twoClassSummary() function isn't appropriate."))
    requireNamespaceQuietStop("ModelMetrics")
    if (!all(levels(data[, "pred"]) == lvls)) 
        stop("levels of observed and predicted data do not match")
    data$y = as.numeric(data$obs == lvls[2])
    rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 
        1), data[, lvls[1]])
    out <- c(rocAUC, sensitivity(data[, "pred"], data[, "obs"], 
        lev[1]), specificity(data[, "pred"], data[, "obs"], lev[2]))
    names(out) <- c("ROC", "Sens", "Spec")
    out
}


gafs <- function (x, ...) 
UseMethod("gafs")


checkResamples <- function (index, x, y) 
{
    if (!is.factor(y)) 
        stop("y must be a factor")
    if (length(levels(y)) < 2) 
        stop("y must have at least 2 levels")
    wrap <- function(index, x, y) checkConditionalX(x[index, 
        , drop = FALSE], y[index])
    unique(unlist(lapply(index, wrap, x = x, y = y)))
}


rfSA <- structure(list(fit = function (x, y, lev = NULL, last = FALSE, 
    ...) 
{
    loadNamespace("randomForest")
    randomForest::randomForest(x, y, ...)
}, pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (is.factor(object$y)) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, fitness_intern = function (object, x, y, maximize, p) 
rfStats(object), fitness_extern = function (data, lev = NULL, 
    model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, initial = function (vars, prob = 0.2, ...) 
{
    sort(sample.int(vars, size = floor(vars * prob) + 1))
}, perturb = function (x, vars, number = floor(vars * 0.01) + 
    1) 
{
    bin <- index2vec(x, vars)
    change <- sample(seq(along = bin), size = number)
    bin[change] <- ifelse(bin[change] == 1, 0, 1)
    sort(which(bin == 1))
}, prob = function (old, new, iteration = 1) 
{
    if (new < old) 
        return(1)
    ediff <- as.vector(old - new)
    ediff <- ediff/abs(old)
    exp(ediff * iteration)
}, selectIter = function (x, metric, maximize) 
{
    bestIter <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    bestIter
}), .Names = c("fit", "pred", "fitness_intern", "fitness_extern", 
"initial", "perturb", "prob", "selectIter"))


getTrainPerf <- function (x) 
{
    bestPerf <- x$bestTune
    colnames(bestPerf) <- gsub("^\\.", "", colnames(bestPerf))
    out <- merge(x$results, bestPerf)
    out <- out[, colnames(out) %in% x$perfNames, drop = FALSE]
    colnames(out) <- paste("Train", colnames(out), sep = "")
    out$method <- x$method
    out
}


splsda <- function (x, ...) 
UseMethod("splsda")


rfFuncs <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, first, last, ...) 
{
    loadNamespace("randomForest")
    randomForest::randomForest(x, y, importance = first, ...)
}, pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (is.factor(object$y)) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, rank = function (object, x, y) 
{
    vimp <- varImp(object)
    if (is.factor(y)) {
        if (all(levels(y) %in% colnames(vimp))) {
            avImp <- apply(vimp[, levels(y), drop = TRUE], 1, 
                mean)
            vimp$Overall <- avImp
        }
    }
    vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
    vimp$var <- rownames(vimp)
    vimp
}, selectSize = function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}, selectVar = function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}), .Names = c("summary", "fit", "pred", "rank", "selectSize", 
"selectVar"))


panel.lift <- function (x, y, ...) 
{
    panel.xyplot(x, y, ...)
    panel.abline(0, 1, col = "black")
}


spatialSign <- function (x) 
UseMethod("spatialSign")


downSample <- function (x, y, list = FALSE, yname = "Class") 
{
    xc <- class(x)
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    if (!is.factor(y)) {
        warning("Down-sampling requires a factor variable as the response. The original data was returned.")
        return(list(x = x, y = y))
    }
    minClass <- min(table(y))
    x$.outcome <- y
    x <- ddply(x, .(y), function(dat, n) dat[sample(seq(along = dat$.outcome), 
        n), , drop = FALSE], n = minClass)
    y <- x$.outcome
    x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
    if (list) {
        if (xc[1] == "matrix") 
            x <- as.matrix(x)
        out <- list(x = x, y = y)
    }
    else {
        out <- cbind(x, y)
        colnames(out)[ncol(out)] <- yname
    }
    out
}


rfeIter <- function (x, y, testX, testY, sizes, rfeControl = rfeControl(), 
    label = "", seeds = NA, ...) 
{
    if (is.null(colnames(x))) 
        stop("x must have column names")
    if (is.null(testX) | is.null(testY)) 
        stop("a test set must be specified")
    if (is.null(sizes)) 
        stop("please specify the number of features")
    predictionMatrix <- matrix(NA, nrow = length(testY), ncol = length(sizes))
    p <- ncol(x)
    retained <- colnames(x)
    sizeValues <- sort(unique(c(sizes, ncol(x))), decreasing = TRUE)
    sizeText <- format(sizeValues)
    finalVariables <- vector(length(sizeValues), mode = "list")
    for (k in seq(along = sizeValues)) {
        if (!any(is.na(seeds))) 
            set.seed(seeds[k])
        if (rfeControl$verbose) {
            cat("+(rfe) fit", ifelse(label != "", label, ""), 
                "size:", sizeText[k], "\n")
        }
        flush.console()
        fitObject <- rfeControl$functions$fit(x[, retained, drop = FALSE], 
            y, first = p == ncol(x[, retained, drop = FALSE]), 
            last = FALSE, ...)
        if (rfeControl$verbose) {
            cat("-(rfe) fit", ifelse(label != "", label, ""), 
                "size:", sizeText[k], "\n")
        }
        modelPred <- rfeControl$functions$pred(fitObject, testX[, 
            retained, drop = FALSE])
        if (is.data.frame(modelPred) | is.matrix(modelPred)) {
            if (is.matrix(modelPred)) {
                modelPred <- as.data.frame(modelPred)
                if (ncol(modelPred) == 1) 
                  names(modelPred) <- "pred"
            }
            modelPred$obs <- testY
            modelPred$Variables <- sizeValues[k]
        }
        else modelPred <- data.frame(pred = modelPred, obs = testY, 
            Variables = sizeValues[k])
        rfePred <- if (k == 1) 
            modelPred
        else rbind(rfePred, modelPred)
        if (!exists("modImp")) {
            if (rfeControl$verbose) {
                cat("+(rfe) imp", ifelse(label != "", label, 
                  ""), "\n")
            }
            modImp <- rfeControl$functions$rank(fitObject, x[, 
                retained, drop = FALSE], y)
            if (rfeControl$verbose) {
                cat("-(rfe) imp", ifelse(label != "", label, 
                  ""), "\n")
            }
        }
        else {
            if (rfeControl$rerank) {
                if (rfeControl$verbose) {
                  cat("+(rfe) imp", ifelse(label != "", label, 
                    ""), "size:", sizeText[k], "\n")
                }
                modImp <- rfeControl$functions$rank(fitObject, 
                  x[, retained, drop = FALSE], y)
                if (rfeControl$verbose) {
                  cat("-(rfe) imp", ifelse(label != "", label, 
                    ""), "size:", sizeText[k], "\n")
                }
            }
        }
        if (nrow(modImp) < sizeValues[k]) {
            msg1 <- paste0("rfe is expecting ", sizeValues[k], 
                " importance values but only has ", nrow(modImp), 
                ". ", "This may be caused by having zero-variance predictors, ", 
                "excessively-correlated predictors, factor predictors ", 
                "that were expanded into dummy variables or you may have ", 
                "failed to drop one of your dummy variables.")
            stop(msg1)
        }
        if (any(!complete.cases(modImp))) {
            stop(paste("There were missing importance values.", 
                "There may be linear dependencies in your predictor variables"))
        }
        finalVariables[[k]] <- subset(modImp, var %in% retained)
        finalVariables[[k]]$Variables <- sizeValues[[k]]
        if (k < length(sizeValues)) 
            retained <- as.character(modImp$var)[1:sizeValues[k + 
                1]]
    }
    list(finalVariables = finalVariables, pred = rfePred)
}


checkInstall <- function (pkg) 
{
    good <- rep(TRUE, length(pkg))
    for (i in seq(along = pkg)) {
        tested <- try(find.package(pkg[i]), silent = TRUE)
        if (class(tested)[1] == "try-error") 
            good[i] <- FALSE
    }
    if (any(!good)) {
        pkList <- paste(pkg[!good], collapse = ", ")
        msg <- paste(sum(!good), ifelse(sum(!good) > 1, " packages are", 
            " package is"), " needed for this model and", ifelse(sum(!good) > 
            1, " are", " is"), " not installed. (", pkList, "). Would you like to try to install", 
            ifelse(sum(!good) > 1, " them", " it"), " now?", 
            sep = "")
        cat(msg)
        if (interactive()) {
            bioc <- c("affy", "logicFS", "gpls", "vbmp")
            installChoice <- menu(c("yes", "no"))
            if (installChoice == 1) {
                hasBioc <- any(pkg[!good] %in% bioc)
                if (!hasBioc) {
                  install.packages(pkg[!good])
                }
                else {
                  inst <- pkg[!good]
                  instC <- inst[!(inst %in% bioc)]
                  instB <- inst[inst %in% bioc]
                  if (length(instC) > 0) 
                    install.packages(instC)
                  biocLite <- NULL
                  source("http://bioconductor.org/biocLite.R")
                  biocLite(instB)
                }
            }
            else stop()
        }
        else stop()
    }
}


predict.gafs <- function (object, newdata, ...) 
{
    newdata <- newdata[, object$optVariables, drop = FALSE]
    object$control$functions$pred(object$fit, newdata)
}


oneSE <- function (x, metric, num, maximize) 
{
    index <- 1:nrow(x)
    if (!maximize) {
        bestIndex <- which.min(x[, metric])
        perf <- x[bestIndex, metric] + (x[bestIndex, paste(metric, 
            "SD", sep = "")])/sqrt(num)
        candidates <- index[x[, metric] <= perf]
        bestIter <- min(candidates)
    }
    else {
        bestIndex <- which.max(x[, metric])
        perf <- x[bestIndex, metric] - (x[bestIndex, paste(metric, 
            "SD", sep = "")])/sqrt(num)
        candidates <- index[x[, metric] >= perf]
        bestIter <- min(candidates)
    }
    bestIter
}


best <- function (x, metric, maximize) 
{
    bestIter <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    bestIter
}


maxDissim <- function (a, b, n = 2, obj = minDiss, useNames = FALSE, randomFrac = 1, 
    verbose = FALSE, ...) 
{
    loadNamespace("proxy")
    if (nrow(b) < 2) 
        stop("there must be at least 2 samples in b")
    if (ncol(a) != ncol(b)) 
        stop("a and b must have the same number of columns")
    if (nrow(b) < n) 
        stop("n must be less than nrow(b)")
    if (randomFrac > 1 | randomFrac <= 0) 
        stop("randomFrac must be in (0, 1]")
    if (useNames) {
        if (is.null(rownames(b))) {
            warning("Cannot use rownames; swithcing to indices")
            free <- 1:nrow(b)
        }
        else free <- rownames(b)
    }
    else free <- 1:nrow(b)
    inSubset <- NULL
    newA <- a
    if (verbose) 
        cat("  adding:")
    for (i in 1:n) {
        pool <- if (randomFrac == 1) 
            free
        else sample(free, max(2, floor(randomFrac * length(free))))
        if (verbose) {
            cat("\nIter", i, "\n")
            cat("Number of candidates:", length(free), "\n")
            cat("Sampling from", length(pool), "samples\n")
        }
        diss <- proxy::dist(newA, b[pool, , drop = FALSE], ...)
        bNames <- colnames(b)[pool]
        tmp <- pool[which.max(apply(diss, 2, obj))]
        if (verbose) 
            cat("new sample:", tmp, "\n")
        inSubset <- c(inSubset, tmp)
        newA <- rbind(newA, b[tmp, , drop = FALSE])
        free <- free[!(free %in% inSubset)]
    }
    inSubset
}


rfStats <- function (x) 
getModelInfo("rf", regex = FALSE)[[1]]$oob(x)


preProcess <- function (x, ...) 
UseMethod("preProcess")


gafs_initial <- function (vars, popSize, ...) 
{
    x <- matrix(NA, nrow = popSize, ncol = vars)
    probs <- seq(0.9, 0.1, length = popSize)
    for (i in 1:popSize) {
        x[i, ] <- sample(0:1, replace = TRUE, size = vars, prob = c(probs[i], 
            1 - probs[i]))
    }
    var_count <- apply(x, 1, sum)
    if (any(var_count == 0)) {
        for (i in which(var_count == 0)) {
            x[i, ] <- sample(0:1, replace = TRUE, size = vars)
        }
    }
    x
}


safs <- function (x, ...) 
UseMethod("safs")


gamScores <- function (x, y) 
{
    if (is.factor(x)) 
        stop("The predictors should be numeric")
    requireNamespaceQuietStop("gam")
    pv <- try(anova(gam::gam(y ~ s(x)), test = "F")[2, "Pr(F)"], 
        silent = TRUE)
    if (any(class(pv) == "try-error")) 
        pv <- try(anova(lm(x ~ y), test = "F")[1, "Pr(>F)"], 
            silent = TRUE)
    if (any(class(pv) == "try-error") || is.na(pv) || is.nan(pv)) 
        pv <- 1
    pv
}


createFolds <- function (y, k = 10, list = TRUE, returnTrain = FALSE) 
{
    if (class(y)[1] == "Surv") 
        y <- y[, "time"]
    if (is.numeric(y)) {
        cuts <- floor(length(y)/k)
        if (cuts < 2) 
            cuts <- 2
        if (cuts > 5) 
            cuts <- 5
        breaks <- unique(quantile(y, probs = seq(0, 1, length = cuts)))
        y <- cut(y, breaks, include.lowest = TRUE)
    }
    if (k < length(y)) {
        y <- factor(as.character(y))
        numInClass <- table(y)
        foldVector <- vector(mode = "integer", length(y))
        for (i in 1:length(numInClass)) {
            min_reps <- numInClass[i]%/%k
            if (min_reps > 0) {
                spares <- numInClass[i]%%k
                seqVector <- rep(1:k, min_reps)
                if (spares > 0) 
                  seqVector <- c(seqVector, sample(1:k, spares))
                foldVector[which(y == names(numInClass)[i])] <- sample(seqVector)
            }
            else {
                foldVector[which(y == names(numInClass)[i])] <- sample(1:k, 
                  size = numInClass[i])
            }
        }
    }
    else foldVector <- seq(along = y)
    if (list) {
        out <- split(seq(along = y), foldVector)
        names(out) <- paste("Fold", gsub(" ", "0", format(seq(along = out))), 
            sep = "")
        if (returnTrain) 
            out <- lapply(out, function(data, y) y[-data], y = seq(along = y))
    }
    else out <- foldVector
    out
}


ctreeBag <- structure(list(fit = function (x, y, ...) 
{
    loadNamespace("party")
    data <- as.data.frame(x)
    data$y <- y
    party::ctree(y ~ ., data = data)
}, pred = function (object, x) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    obsLevels <- levels(object@data@get("response")[, 1])
    if (!is.null(obsLevels)) {
        rawProbs <- party::treeresponse(object, x)
        probMatrix <- matrix(unlist(rawProbs), ncol = length(obsLevels), 
            byrow = TRUE)
        out <- data.frame(probMatrix)
        colnames(out) <- obsLevels
        rownames(out) <- NULL
    }
    else out <- unlist(party::treeresponse(object, x))
    out
}, aggregate = function (x, type = "class") 
{
    if (is.matrix(x[[1]]) | is.data.frame(x[[1]])) {
        pooled <- x[[1]] & NA
        classes <- colnames(pooled)
        for (i in 1:ncol(pooled)) {
            tmp <- lapply(x, function(y, col) y[, col], col = i)
            tmp <- do.call("rbind", tmp)
            pooled[, i] <- apply(tmp, 2, median)
        }
        if (type == "class") {
            out <- factor(classes[apply(pooled, 1, which.max)], 
                levels = classes)
        }
        else out <- as.data.frame(pooled)
    }
    else {
        x <- matrix(unlist(x), ncol = length(x))
        out <- apply(x, 1, median)
    }
    out
}), .Names = c("fit", "pred", "aggregate"))


multiClassSummary <- function (data, lev = NULL, model = NULL) 
{
    if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
        stop("levels of observed and predicted data do not match")
    has_class_probs <- all(lev %in% colnames(data))
    if (has_class_probs) {
        lloss <- mnLogLoss(data = data, lev = lev, model = model)
        requireNamespaceQuietStop("ModelMetrics")
        prob_stats <- lapply(levels(data[, "pred"]), function(x) {
            obs <- ifelse(data[, "obs"] == x, 1, 0)
            prob <- data[, x]
            AUCs <- try(ModelMetrics::auc(obs, data[, x]), silent = TRUE)
            return(AUCs)
        })
        roc <- mean(unlist(prob_stats))
    }
    CM <- confusionMatrix(data[, "pred"], data[, "obs"])
    if (length(levels(data[, "pred"])) == 2) {
        class_stats <- CM$byClass
    }
    else {
        class_stats <- colMeans(CM$byClass)
        names(class_stats) <- paste("Mean", names(class_stats))
    }
    overall_stats <- if (has_class_probs) 
        c(CM$overall, logLoss = as.numeric(lloss), AUC = roc)
    else CM$overall
    stats <- c(overall_stats, class_stats)
    stats <- stats[!names(stats) %in% c("AccuracyNull", "AccuracyLower", 
        "AccuracyUpper", "AccuracyPValue", "McnemarPValue", "Mean Prevalence", 
        "Mean Detection Prevalence")]
    names(stats) <- gsub("[[:blank:]]+", "_", names(stats))
    stat_list <- c("Accuracy", "Kappa", "Mean_F1", "Mean_Sensitivity", 
        "Mean_Specificity", "Mean_Pos_Pred_Value", "Mean_Neg_Pred_Value", 
        "Mean_Detection_Rate", "Mean_Balanced_Accuracy")
    if (has_class_probs) 
        stat_list <- c("logLoss", "AUC", stat_list)
    if (length(levels(data[, "pred"])) == 2) 
        stat_list <- gsub("^Mean_", "", stat_list)
    stats <- stats[c(stat_list)]
    return(stats)
}


knn3 <- function (x, ...) 
UseMethod("knn3")


plotObsVsPred <- function (object, equalRanges = TRUE, ...) 
{
    object <- object[object$dataType != "Unknown", ]
    object$dataType <- factor(object$dataType)
    if (is.factor(object$obs)) {
        agreement <- object$obs == object$pred
        accuracyTable <- by(agreement, list(model = object$model, 
            data = object$dataType), mean)
        accuracyDF <- data.frame(unclass(accuracyTable))
        accuracyStacked <- stack(accuracyDF)
        accuracyStacked$model <- rep(dimnames(accuracyDF)[[1]], 
            dim(accuracyDF)[2])
        names(accuracyStacked) <- c("Accuracy", "Data", "Model")
        accuracyStacked$Data <- factor(ifelse(accuracyStacked$Data == 
            "Training", "Training (uncorrected)", as.character(accuracyStacked$Data)))
        out <- dotplot(Model ~ Accuracy, accuracyStacked, groups = accuracyStacked$Data, 
            ...)
    }
    else {
        if (equalRanges) {
            xLimits <- yLimits <- extendrange(c(object$obs, object$pred))
        }
        else {
            xLimits <- extendrange(object$obs)
            yLimits <- extendrange(object$pred)
        }
        out <- xyplot(obs ~ pred | model * dataType, object, 
            xlim = xLimits, ylim = yLimits, panel = function(x, 
                y, groups, subscripts, ...) {
                panel.xyplot(x, y, cex = 0.6)
                panel.abline(0, 1, col = trellis.par.get("superpose.line")$col[1], 
                  lty = 2)
                panel.loess(x, y, span = 0.75)
            }, xlab = "Predicted", ylab = "Observed", ...)
    }
    out
}


learing_curve_dat <- function (dat, outcome = NULL, proportion = (1:10)/10, test_prop = 0, 
    verbose = TRUE, ...) 
{
    if (is.null(outcome)) 
        stop("Please give a character stirng for the outcome column name")
    proportion <- sort(unique(proportion))
    n_size <- length(proportion)
    if (test_prop > 0) {
        for_model <- createDataPartition(dat[, outcome], p = 1 - 
            test_prop, list = FALSE)
    }
    else for_model <- 1:nrow(dat)
    n <- length(for_model)
    resampled <- vector(mode = "list", length = n_size)
    tested <- if (test_prop > 0) 
        resampled
    else NULL
    apparent <- resampled
    for (i in seq(along = proportion)) {
        if (verbose) 
            cat("Training for ", round(proportion[i] * 100, 1), 
                "% (n = ", floor(n * proportion[i]), ")\n", sep = "")
        in_mod <- if (proportion[i] < 1) 
            sample(for_model, size = floor(n * proportion[i]))
        else for_model
        mod <- train(x = dat[in_mod, colnames(dat) != outcome, 
            drop = FALSE], y = dat[in_mod, outcome], ...)
        if (i == 1) 
            perf_names <- mod$perfNames
        resampled[[i]] <- merge(mod$resample, mod$bestTune)
        resampled[[i]]$Training_Size <- length(in_mod)
        if (test_prop > 0) {
            if (!mod$control$classProbs) {
                test_preds <- extractPrediction(list(model = mod), 
                  testX = dat[-for_model, colnames(dat) != outcome, 
                    drop = FALSE], testY = dat[-for_model, outcome])
            }
            else {
                test_preds <- extractProb(list(model = mod), 
                  testX = dat[-for_model, colnames(dat) != outcome, 
                    drop = FALSE], testY = dat[-for_model, outcome])
            }
            test_perf <- mod$control$summaryFunction(test_preds, 
                lev = mod$finalModel$obsLevels)
            test_perf <- as.data.frame(t(test_perf))
            test_perf$Training_Size <- length(in_mod)
            tested[[i]] <- test_perf
            try(rm(test_preds, test_perf), silent = TRUE)
        }
        if (!mod$control$classProbs) {
            app_preds <- extractPrediction(list(model = mod), 
                testX = dat[in_mod, colnames(dat) != outcome, 
                  drop = FALSE], testY = dat[in_mod, outcome])
        }
        else {
            app_preds <- extractProb(list(model = mod), testX = dat[in_mod, 
                colnames(dat) != outcome, drop = FALSE], testY = dat[in_mod, 
                outcome])
        }
        app_perf <- mod$control$summaryFunction(app_preds, lev = mod$finalModel$obsLevels)
        app_perf <- as.data.frame(t(app_perf))
        app_perf$Training_Size <- length(in_mod)
        apparent[[i]] <- app_perf
        try(rm(mod, in_mod, app_preds, app_perf), silent = TRUE)
    }
    resampled <- do.call("rbind", resampled)
    resampled <- resampled[, c(perf_names, "Training_Size")]
    resampled$Data <- "Resampling"
    apparent <- do.call("rbind", apparent)
    apparent <- apparent[, c(perf_names, "Training_Size")]
    apparent$Data <- "Training"
    out <- rbind(resampled, apparent)
    if (test_prop > 0) {
        tested <- do.call("rbind", tested)
        tested <- tested[, c(perf_names, "Training_Size")]
        tested$Data <- "Testing"
        out <- rbind(out, tested)
    }
    out
}


prSummary <- function (data, lev = NULL, model = NULL) 
{
    requireNamespaceQuietStop("MLmetrics")
    if (length(levels(data$obs)) > 2) 
        stop(paste("Your outcome has", length(levels(data$obs)), 
            "levels. The prSummary() function isn't appropriate."))
    if (!all(levels(data[, "pred"]) == levels(data[, "obs"]))) 
        stop("levels of observed and predicted data do not match")
    c(AUC = MLmetrics::PRAUC(y_pred = data[, lev[1]], y_true = ifelse(data$obs == 
        lev[1], 1, 0)), Precision = precision.default(data = data$pred, 
        reference = data$obs, relevant = lev[1]), Recall = recall.default(data = data$pred, 
        reference = data$obs, relevant = lev[1]), F = F_meas.default(data = data$pred, 
        reference = data$obs, relevant = lev[1]))
}


plot.gafs <- function (x, metric = x$control$metric["external"], estimate = c("internal", 
    "external"), output = "ggplot", ...) 
{
    int_names <- names(x$internal)[!(names(x$internal) %in% ga_internal_names)]
    ext_names <- names(x$external)[!(names(x$external) %in% ga_external_names)]
    common <- intersect(int_names, ext_names)
    both_estimates <- length(estimate) == 2 && all(sort(estimate) == 
        c("external", "internal"))
    if (both_estimates) {
        if (!metric %in% common) 
            stop(paste("'", metric, "' not computed in both estimates"))
        tmp_e <- x$external[, c("Iter", "Resample", common)]
        tmp_e$Estimate <- "External"
        tmp_i <- x$internal[, c("Iter", "Resample", common)]
        tmp_i$Estimate <- "Internal"
        plot_dat <- rbind(tmp_e, tmp_i)
    }
    else {
        if ("internal" %in% estimate) {
            if (!metric %in% int_names) 
                stop(paste("'", metric, "' not computed internally"))
            plot_dat <- x$internal[, c("Iter", "Resample", int_names)]
        }
        if ("external" %in% estimate) {
            if (!metric %in% int_names) 
                stop(paste("'", metric, "' not computed externally"))
            plot_dat <- x$external[, c("Iter", "Resample", ext_names)]
        }
    }
    if (output == "data") 
        out <- plot_dat
    plot_dat <- if (both_estimates) 
        ddply(plot_dat, c("Iter", "Estimate"), function(x) c(Mean = mean(x[, 
            metric])))
    else ddply(plot_dat, c("Iter"), function(x) c(Mean = mean(x[, 
        metric])))
    if (output == "ggplot") {
        out <- if (both_estimates) 
            ggplot(plot_dat, aes(x = Iter, y = Mean, color = Estimate)) + 
                geom_point()
        else ggplot(plot_dat, aes(x = Iter, y = Mean)) + geom_point()
        out <- out + xlab("Generation")
    }
    if (output == "lattice") {
        out <- if (both_estimates) 
            xyplot(Mean ~ Iter, data = plot_dat, groups = Estimate, 
                ...)
        else xyplot(Mean ~ Iter, data = plot_dat, ...)
        out <- update(out, xlab = "Generation")
    }
    out
}


plotClassProbs <- function (object, plotType = "histogram", useObjects = FALSE, 
    ...) 
{
    obsLevels <- levels(object$obs)
    stackProbs <- melt(object, id.vars = c("obs", "model", "object", 
        "dataType"), measure.vars = if (length(obsLevels) == 
        2) 
        obsLevels[1]
    else obsLevels)
    names(stackProbs)[names(stackProbs) == "variable"] <- "Class"
    names(stackProbs)[names(stackProbs) == "value"] <- "Probability"
    names(stackProbs)[names(stackProbs) == "obs"] <- "Observed"
    stackProbs$Observed <- paste("Data:", as.character(stackProbs$Observed))
    stackProbs$Class <- paste("Prob:", as.character(stackProbs$Class))
    keepVars <- "Observed"
    if (length(unique(stackProbs$dataType)) > 1) 
        keepVars <- c(keepVars, "dataType")
    if (length(unique(stackProbs$model)) > 1) 
        keepVars <- c(keepVars, "model")
    if (any(names(object) == "object") & useObjects) {
        if (length(unique(stackProbs$object)) > 1) 
            keepVars <- c(keepVars, "object")
    }
    if (plotType == "histogram") {
        form <- if (length(obsLevels) == 2) {
            form <- if (length(keepVars) > 0) 
                paste("~ Probability|", paste(keepVars, collapse = "*"))
            else "~ Probability"
            form <- as.formula(form)
            out <- histogram(form, data = stackProbs, xlab = paste("Probability of", 
                obsLevels[1]), ...)
        }
        else {
            form <- if (length(keepVars) > 0) 
                paste("~ Probability|Class*", paste(keepVars, 
                  collapse = "*"))
            else "~ Probability|Class"
            form <- as.formula(form)
            out <- histogram(form, data = stackProbs, ...)
        }
    }
    else {
        keepVars <- keepVars[keepVars != "Observed"]
        form <- if (length(keepVars) > 0) 
            paste("~ Probability|", paste(keepVars, collapse = "*"))
        else "~ Probability"
        form <- as.formula(form)
        out <- densityplot(form, data = stackProbs, groups = Observed, 
            ...)
    }
    out
}


pickSizeTolerance <- function (x, metric, tol = 1.5, maximize) 
{
    if (!maximize) {
        best <- min(x[, metric])
        perf <- (x[, metric] - best)/best * 100
        flag <- perf <= tol
    }
    else {
        best <- max(x[, metric])
        perf <- (best - x[, metric])/best * 100
        flag <- perf <= tol
    }
    min(x[flag, "Variables"])
}


panel.calibration <- function (...) 
{
    panel.abline(0, 1, col = trellis.par.get("reference.line")$col, 
        lwd = trellis.par.get("reference.line")$lwd, lty = trellis.par.get("reference.line")$lty)
    panel.xyplot(...)
}


ldaBag <- structure(list(fit = function (x, y, ...) 
{
    loadNamespace("MASS")
    MASS::lda(x, y, ...)
}, pred = function (object, x) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    predict(object, x)$posterior
}, aggregate = function (x, type = "class") 
{
    pooled <- x[[1]] * NA
    n <- nrow(pooled)
    classes <- colnames(pooled)
    for (i in 1:ncol(pooled)) {
        tmp <- lapply(x, function(y, col) y[, col], col = i)
        tmp <- do.call("rbind", tmp)
        pooled[, i] <- apply(tmp, 2, median)
    }
    pooled <- apply(pooled, 1, function(x) x/sum(x))
    if (n != nrow(pooled)) 
        pooled <- t(pooled)
    if (type == "class") {
        out <- factor(classes[apply(pooled, 1, which.max)], levels = classes)
    }
    else out <- as.data.frame(pooled)
    out
}), .Names = c("fit", "pred", "aggregate"))


treebagSA <- structure(list(fit = function (x, y, lev = NULL, last = FALSE, 
    ...) 
{
    loadNamespace("ipred")
    ipred::ipredbagg(y, x, ...)
}, pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (is.factor(object$y)) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, fitness_intern = function (object, x, y, maximize, p) 
ipredStats(object)[1:2], fitness_extern = function (data, lev = NULL, 
    model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, initial = function (vars, prob = 0.2, ...) 
{
    sort(sample.int(vars, size = floor(vars * prob) + 1))
}, perturb = function (x, vars, number = floor(vars * 0.01) + 
    1) 
{
    bin <- index2vec(x, vars)
    change <- sample(seq(along = bin), size = number)
    bin[change] <- ifelse(bin[change] == 1, 0, 1)
    sort(which(bin == 1))
}, prob = function (old, new, iteration = 1) 
{
    if (new < old) 
        return(1)
    ediff <- as.vector(old - new)
    ediff <- ediff/abs(old)
    exp(ediff * iteration)
}, selectIter = function (x, metric, maximize) 
{
    bestIter <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    bestIter
}), .Names = c("fit", "pred", "fitness_intern", "fitness_extern", 
"initial", "perturb", "prob", "selectIter"))


findLinearCombos <- function (x) 
{
    if (!is.matrix(x)) 
        x <- as.matrix(x)
    lcList <- enumLC(x)
    initialList <- lcList
    badList <- NULL
    if (length(lcList) > 0) {
        continue <- TRUE
        while (continue) {
            tmp <- unlist(lapply(lcList, function(x) x[1]))
            tmp <- unique(tmp[!is.na(tmp)])
            badList <- unique(c(tmp, badList))
            lcList <- enumLC(x[, -badList])
            continue <- (length(lcList) > 0)
        }
    }
    else badList <- NULL
    list(linearCombos = initialList, remove = badList)
}


twoClassSim <- function (n = 100, intercept = -5, linearVars = 10, noiseVars = 0, 
    corrVars = 0, corrType = "AR1", corrValue = 0, mislabel = 0, 
    ordinal = FALSE) 
{
    requireNamespaceQuietStop("MASS")
    sigma <- matrix(c(2, 1.3, 1.3, 2), 2, 2)
    tmpData <- data.frame(MASS::mvrnorm(n = n, c(0, 0), sigma))
    names(tmpData) <- paste("TwoFactor", 1:2, sep = "")
    if (linearVars > 0) {
        tmpData <- cbind(tmpData, matrix(rnorm(n * linearVars), 
            ncol = linearVars))
        colnames(tmpData)[(1:linearVars) + 2] <- paste("Linear", 
            gsub(" ", "0", format(1:linearVars)), sep = "")
    }
    tmpData$Nonlinear1 <- runif(n, min = -1)
    tmpData <- cbind(tmpData, matrix(runif(n * 2), ncol = 2))
    colnames(tmpData)[(ncol(tmpData) - 1):ncol(tmpData)] <- paste("Nonlinear", 
        2:3, sep = "")
    tmpData <- as.data.frame(tmpData)
    p <- ncol(tmpData)
    if (noiseVars > 0) {
        tmpData <- cbind(tmpData, matrix(rnorm(n * noiseVars), 
            ncol = noiseVars))
        colnames(tmpData)[(p + 1):ncol(tmpData)] <- paste("Noise", 
            gsub(" ", "0", format(1:noiseVars)), sep = "")
    }
    if (corrVars > 0) {
        p <- ncol(tmpData)
        loadNamespace("MASS")
        if (corrType == "exch") {
            vc <- matrix(corrValue, ncol = corrVars, nrow = corrVars)
            diag(vc) <- 1
        }
        if (corrType == "AR1") {
            vcValues <- corrValue^(seq(0, corrVars - 1, by = 1))
            vc <- toeplitz(vcValues)
        }
        tmpData <- cbind(tmpData, MASS::mvrnorm(n, mu = rep(0, 
            corrVars), Sigma = vc))
        colnames(tmpData)[(p + 1):ncol(tmpData)] <- paste("Corr", 
            gsub(" ", "0", format(1:corrVars)), sep = "")
    }
    lp <- intercept - 4 * tmpData$TwoFactor1 + 4 * tmpData$TwoFactor2 + 
        2 * tmpData$TwoFactor1 * tmpData$TwoFactor2 + (tmpData$Nonlinear1^3) + 
        2 * exp(-6 * (tmpData$Nonlinear1 - 0.3)^2) + 2 * sin(pi * 
        tmpData$Nonlinear2 * tmpData$Nonlinear3)
    if (linearVars > 0) {
        lin <- seq(10, 1, length = linearVars)/4
        lin <- lin * rep(c(-1, 1), floor(linearVars) + 1)[1:linearVars]
        for (i in seq(along = lin)) lp <- lp + tmpData[, i + 
            3] * lin[i]
    }
    if (ordinal) {
        prob <- binomial()$linkinv(lp + rnorm(n, sd = 2))
        tmpData$Class <- cut(prob, breaks = c(0, 0.2, 0.75, 1), 
            include.lowest = TRUE, labels = c("low", "med", "high"), 
            ordered_result = TRUE)
    }
    else {
        prob <- binomial()$linkinv(lp)
        if (mislabel > 0 & mislabel < 1) {
            shuffle <- sample(1:nrow(tmpData), floor(nrow(tmpData) * 
                j))
            prob[shuffle] <- 1 - prob[shuffle]
        }
        tmpData$Class <- ifelse(prob <= runif(n), "Class1", "Class2")
        tmpData$Class <- factor(tmpData$Class, levels = c("Class1", 
            "Class2"))
    }
    tmpData
}


sensitivity <- function (data, ...) 
{
    UseMethod("sensitivity")
}


contr.dummy <- function (n, ...) 
{
    if (is.numeric(n) && length(n) == 1L) {
        if (n > 1L) 
            levels <- as.character(seq_len(n))
        else stop("not enough degrees of freedom to define contrasts")
    }
    else {
        levels <- as.character(n)
        n <- length(n)
    }
    out <- diag(n)
    rownames(out) <- levels
    colnames(out) <- levels
    out
}


getSamplingInfo <- function (method = NULL, regex = TRUE, ...) 
{
    load(system.file("models", "sampling.RData", package = "caret"))
    if (!is.null(method)) {
        keepers <- if (regex) 
            grepl(method, names(sampling_methods), ...)
        else which(method == names(sampling_methods))[1]
        sampling_methods <- sampling_methods[keepers]
    }
    if (length(sampling_methods) == 0) 
        stop("That sampling method is not in caret's built-in library")
    sampling_methods
}


ldaSBF <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, ...) 
{
    if (ncol(x) > 0) {
        loadNamespace("MASS")
        MASS::lda(x, y, ...)
    }
    else nullModel(y = y)
}, pred = function (object, x) 
{
    if (class(object) == "nullModel") {
        tmp <- predict(object, x)
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else {
        tmp <- predict(object, x)
        out <- cbind(data.frame(pred = tmp$class), as.data.frame(tmp$posterior))
    }
    out
}, score = function (x, y) 
{
    anovaScores(x, y)
}, filter = function (score, x, y) 
score <= 0.05), .Names = c("summary", "fit", "pred", "score", 
"filter"))


caretTheme <- function () 
list(plot.polygon = list(alpha = 1, col = "aliceblue", border = "black", 
    lty = 1, lwd = 1), background = list(col = "transparent"), 
    bar.fill = list(col = "#cce6ff"), box.rectangle = list(col = "black"), 
    box.umbrella = list(col = "black"), dot.line = list(col = "#e8e8e8"), 
    dot.symbol = list(col = "black"), plot.line = list(col = "black"), 
    plot.symbol = list(col = "black"), regions = list(col = c("#FEF8FA", 
        "#FDF6F9", "#FBF5F9", "#FAF3F8", "#F8F2F7", "#F7F0F7", 
        "#F5EEF6", "#F4EDF5", "#F2EBF5", "#F1EAF4", "#EFE8F3", 
        "#EDE7F2", "#ECE5F1", "#EAE4F1", "#E8E2F0", "#E6E1EF", 
        "#E4DFEE", "#E2DEED", "#E0DCEC", "#DEDAEB", "#DCD9EA", 
        "#D9D7E9", "#D7D6E8", "#D4D4E7", "#D1D2E6", "#CED1E5", 
        "#CCCFE4", "#C8CEE3", "#C5CCE2", "#C2CAE1", "#BFC9E0", 
        "#BBC7DF", "#B8C5DF", "#B4C4DE", "#B1C2DD", "#ADC0DC", 
        "#A9BFDB", "#A6BDDA", "#A2BBD9", "#9EB9D9", "#9BB8D8", 
        "#97B6D7", "#93B4D6", "#8FB2D5", "#8BB0D4", "#87AFD3", 
        "#83ADD2", "#7FABD1", "#7AA9D0", "#76A7CF", "#71A5CE", 
        "#6CA3CC", "#68A1CB", "#63A0CA", "#5D9EC9", "#589CC8", 
        "#539AC6", "#4E98C5", "#4996C4", "#4493C3", "#3F91C1", 
        "#3A8FC0", "#358DBF", "#308BBE", "#2C89BD", "#2887BC", 
        "#2385BB", "#1F83BA", "#1C80B9", "#187EB7", "#157CB6", 
        "#127AB5", "#0F78B3", "#0D76B2", "#0A73B0", "#0971AE", 
        "#076FAC", "#066DAA", "#056AA7", "#0568A5")), strip.shingle = list(col = c("#ff7f00", 
        "#00ff00", "#00ffff", "#ff00ff", "#ff0000", "#ffff00", 
        "#0080ff")), strip.background = list(col = c("#ffe5cc", 
        "#ccffcc", "#ccffff", "#ffccff", "#ffcccc", "#ffffcc", 
        "#cce6ff")), reference.line = list(col = "#e8e8e8"), 
    superpose.line = list(col = c("#9E0142", "#3288BD", "#F46D43", 
        "#5E4FA2", "#66C2A5", "black", "#9E0142", "#3288BD", 
        "#F46D43", "#5E4FA2", "#66C2A5", "black", "#9E0142", 
        "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
        "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", 
        "black", "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", 
        "#66C2A5", "black", "#9E0142", "#3288BD", "#F46D43", 
        "#5E4FA2", "#66C2A5", "black"), lty = rep(1:6, each = 6)), 
    superpose.symbol = list(pch = c(1, 4, 6, 0, 5, 17, 4, 6, 
        0, 5, 17, 1, 6, 0, 5, 17, 1, 4, 0, 5, 17, 1, 4, 6, 5, 
        17, 1, 4, 6, 0, 17, 1, 4, 6, 0, 5), cex = rep(0.7, 6 * 
        6), col = c("#9E0142", "#3288BD", "#F46D43", "#5E4FA2", 
        "#66C2A5", "black", "#9E0142", "#3288BD", "#F46D43", 
        "#5E4FA2", "#66C2A5", "black", "#9E0142", "#3288BD", 
        "#F46D43", "#5E4FA2", "#66C2A5", "black", "#9E0142", 
        "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", "black", 
        "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", "#66C2A5", 
        "black", "#9E0142", "#3288BD", "#F46D43", "#5E4FA2", 
        "#66C2A5", "black")))


sumDiss <- function (u) 
sum(u, na.rm = TRUE)


class2ind <- function (x, drop2nd = FALSE) 
{
    if (!is.factor(x)) 
        stop("'x' should be a factor")
    y <- model.matrix(~x - 1)
    colnames(y) <- gsub("^x", "", colnames(y))
    attributes(y)$assign <- NULL
    attributes(y)$contrasts <- NULL
    if (length(levels(x)) == 2 & drop2nd) {
        y <- y[, 1]
    }
    y
}


nullModel <- function (x, ...) 
UseMethod("nullModel")


treebagGA <- structure(list(fit = function (x, y, lev = NULL, last = FALSE, 
    ...) 
{
    loadNamespace("ipred")
    ipred::ipredbagg(y, x, ...)
}, pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (is.factor(object$y)) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, fitness_intern = function (object, x, y, maximize, p) 
ipredStats(object)[1:2], fitness_extern = function (data, lev = NULL, 
    model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, initial = function (vars, popSize, ...) 
{
    x <- matrix(NA, nrow = popSize, ncol = vars)
    probs <- seq(0.9, 0.1, length = popSize)
    for (i in 1:popSize) {
        x[i, ] <- sample(0:1, replace = TRUE, size = vars, prob = c(probs[i], 
            1 - probs[i]))
    }
    var_count <- apply(x, 1, sum)
    if (any(var_count == 0)) {
        for (i in which(var_count == 0)) {
            x[i, ] <- sample(0:1, replace = TRUE, size = vars)
        }
    }
    x
}, selection = function (population, fitness, r = NULL, q = NULL, 
    ...) 
{
    popSize = nrow(population)
    if (is.null(r)) 
        r <- 2/(popSize * (popSize - 1))
    if (is.null(q)) 
        q <- 2/popSize
    rank <- (popSize + 1) - rank(fitness, ties.method = "random")
    prob <- q - (rank - 1) * r
    sel <- sample(1:popSize, size = popSize, prob = pmin(pmax(0, 
        prob), 1, na.rm = TRUE), replace = TRUE)
    out <- list(population = population[sel, , drop = FALSE], 
        fitness = fitness[sel])
    out
}, crossover = function (population, fitness, parents, ...) 
{
    fitness <- fitness[parents]
    parents <- population[parents, , drop = FALSE]
    n <- ncol(parents)
    children <- matrix(as.double(NA), nrow = 2, ncol = n)
    fitnessChildren <- rep(NA, 2)
    crossOverPoint <- sample(0:n, size = 1)
    if (crossOverPoint == 0) {
        children[1:2, ] <- parents[2:1, ]
        fitnessChildren[1:2] <- fitness[2:1]
    }
    else if (crossOverPoint == n) {
        children <- parents
        fitnessChildren <- fitness
    }
    else {
        children[1, ] <- c(parents[1, 1:crossOverPoint], parents[2, 
            (crossOverPoint + 1):n])
        children[2, ] <- c(parents[2, 1:crossOverPoint], parents[1, 
            (crossOverPoint + 1):n])
    }
    out <- list(children = children, fitness = fitnessChildren)
    out
}, mutation = function (population, parent, ...) 
{
    mutate <- parent <- as.vector(population[parent, ])
    n <- length(parent)
    j <- sample(1:n, size = 1)
    mutate[j] <- abs(mutate[j] - 1)
    mutate
}, selectIter = function (x, metric, maximize) 
{
    bestIter <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    bestIter
}), .Names = c("fit", "pred", "fitness_intern", "fitness_extern", 
"initial", "selection", "crossover", "mutation", "selectIter"
))


resamples <- function (x, ...) 
UseMethod("resamples")


createModel <- function (x, y, wts, method, tuneValue, obsLevels, pp = NULL, 
    last = FALSE, sampling = NULL, classProbs, ...) 
{
    if (is.data.frame(x) | is.matrix(x)) 
        rownames(x) <- make.names(rownames(x), unique = TRUE)
    if (!is.null(sampling) && sampling$first) {
        tmp <- sampling$func(x, y)
        x <- tmp$x
        y <- tmp$y
        rm(tmp)
    }
    if (!is.null(pp$options)) {
        pp$method <- pp$options
        pp$options <- NULL
        if ("ica" %in% pp$method) 
            pp$n.comp <- pp$ICAcomp
        pp$ICAcomp <- NULL
        pp$x <- x
        ppObj <- do.call("preProcess", pp)
        ppObj$call <- "scrubed"
        x <- predict(ppObj, x)
        rm(pp)
    }
    else ppObj <- NULL
    if (!is.null(sampling) && !sampling$first) {
        tmp <- sampling$func(x, y)
        x <- tmp$x
        y <- tmp$y
        rm(tmp)
    }
    modelFit <- method$fit(x = x, y = y, wts = wts, param = tuneValue, 
        lev = obsLevels, last = last, classProbs = classProbs, 
        ...)
    if (is.null(method$label)) 
        method$label <- ""
    if (!isS4(modelFit) & !(method$label %in% c("Ensemble Partial Least Squares Regression", 
        "Ensemble Partial Least Squares Regression with Feature Selection"))) {
        modelFit$xNames <- colnames(x)
        modelFit$problemType <- if (is.factor(y)) 
            "Classification"
        else "Regression"
        modelFit$tuneValue <- tuneValue
        modelFit$obsLevels <- obsLevels
        modelFit$param <- list(...)
    }
    list(fit = modelFit, preProc = ppObj)
}


upSample <- function (x, y, list = FALSE, yname = "Class") 
{
    xc <- class(x)
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    if (!is.factor(y)) {
        warning("Up-sampling requires a factor variable as the response. The original data was returned.")
        return(list(x = x, y = y))
    }
    maxClass <- max(table(y))
    x$.outcome <- y
    x <- ddply(x, .(y), function(x, top = maxClass) {
        if (nrow(x) < top) {
            ind <- sample(1:nrow(x), size = top - nrow(x), replace = TRUE)
            ind <- c(1:nrow(x), ind)
            x <- x[ind, , drop = FALSE]
        }
        x
    })
    y <- x$.outcome
    x <- x[, !(colnames(x) %in% c("y", ".outcome")), drop = FALSE]
    if (list) {
        if (xc[1] == "matrix") 
            x <- as.matrix(x)
        out <- list(x = x, y = y)
    }
    else {
        out <- cbind(x, y)
        colnames(out)[ncol(out)] <- yname
    }
    out
}


createResample <- function (y, times = 10, list = TRUE) 
{
    if (class(y)[1] == "Surv") 
        y <- y[, "time"]
    trainIndex <- matrix(0, ncol = times, nrow = length(y))
    out <- apply(trainIndex, 2, function(data) {
        index <- seq(along = data)
        out <- sort(sample(index, size = length(index), replace = TRUE))
        out
    })
    if (list) {
        out <- as.data.frame(out)
        attributes(out) <- NULL
        names(out) <- prettySeq(out)
    }
    else {
        colnames(out) <- prettySeq(1:ncol(out))
    }
    out
}


mnLogLoss <- function (data, lev = NULL, model = NULL) 
{
    if (is.null(lev)) 
        stop("'lev' cannot be NULL")
    if (!all(lev %in% colnames(data))) 
        stop("'data' should have columns consistent with 'lev'")
    if (!all(sort(lev) %in% sort(levels(data$obs)))) 
        stop("'data$obs' should have levels consistent with 'lev'")
    dataComplete <- data[complete.cases(data), ]
    probs <- as.matrix(dataComplete[, lev, drop = FALSE])
    inds <- match(dataComplete$obs, colnames(probs))
    if (nlevels(dataComplete$obs) == 2) {
        logLoss <- ModelMetrics::logLoss(dataComplete$obs, probs)
    }
    else {
        logLoss <- ModelMetrics::mlogLoss(dataComplete$obs, probs)
    }
    c(logLoss = logLoss)
}


lrFuncs <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, first, last, ...) 
{
    tmp <- if (is.data.frame(x)) 
        x
    else as.data.frame(x)
    tmp$Class <- y
    glm(Class ~ ., data = tmp, family = "binomial")
}, pred = function (object, x) 
{
    if (!is.data.frame(x)) 
        x <- as.data.frame(x)
    lvl <- levels(object$data$Class)
    tmp <- predict(object, x, type = "response")
    out <- data.frame(1 - tmp, tmp)
    colnames(out) <- lvl
    out$pred <- factor(ifelse(tmp > 0.5, lvl[2], lvl[1]), levels = lvl)
    out
}, rank = function (object, x, y) 
{
    vimp <- varImp(object, scale = FALSE)
    vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
    vimp$var <- rownames(vimp)
    vimp
}, selectSize = function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}, selectVar = function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}), .Names = c("summary", "fit", "pred", "rank", "selectSize", 
"selectVar"))


BoxCoxTrans <- function (y, ...) 
UseMethod("BoxCoxTrans")


gafs_lrSelection <- function (population, fitness, r = NULL, q = NULL, ...) 
{
    popSize = nrow(population)
    if (is.null(r)) 
        r <- 2/(popSize * (popSize - 1))
    if (is.null(q)) 
        q <- 2/popSize
    rank <- (popSize + 1) - rank(fitness, ties.method = "random")
    prob <- q - (rank - 1) * r
    sel <- sample(1:popSize, size = popSize, prob = pmin(pmax(0, 
        prob), 1, na.rm = TRUE), replace = TRUE)
    out <- list(population = population[sel, , drop = FALSE], 
        fitness = fitness[sel])
    out
}


safs_prob <- function (old, new, iteration = 1) 
{
    if (new < old) 
        return(1)
    ediff <- as.vector(old - new)
    ediff <- ediff/abs(old)
    exp(ediff * iteration)
}


var_seq <- function (p, classification = FALSE, len = 3) 
{
    if (len == 1) {
        tuneSeq <- if (classification) 
            max(floor(p/3), 1)
        else floor(sqrt(p))
    }
    else {
        if (p <= len) {
            tuneSeq <- floor(seq(2, to = p, length = p))
        }
        else {
            if (p < 500) 
                tuneSeq <- floor(seq(2, to = p, length = len))
            else tuneSeq <- floor(2^seq(1, to = log(p, base = 2), 
                length = len))
        }
    }
    if (any(table(tuneSeq) > 1)) {
        tuneSeq <- unique(tuneSeq)
        cat("note: only", length(tuneSeq), "unique complexity parameters in default grid.", 
            "Truncating the grid to", length(tuneSeq), ".\n\n")
    }
    tuneSeq
}


caretGA <- structure(list(fit = function (x, y, lev = NULL, last = FALSE, 
    ...) 
train(x, y, ...), pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (object$control$classProbs) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, fitness_intern = function (object, x, y, maximize, p) 
{
    perf_val <- getTrainPerf(object)
    perf_val <- perf_val[names(perf_val) != "method"]
    perf_val <- unlist(perf_val)
    names(perf_val) <- gsub("Train", "", names(perf_val))
    perf_val
}, fitness_extern = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, initial = function (vars, popSize, ...) 
{
    x <- matrix(NA, nrow = popSize, ncol = vars)
    probs <- seq(0.9, 0.1, length = popSize)
    for (i in 1:popSize) {
        x[i, ] <- sample(0:1, replace = TRUE, size = vars, prob = c(probs[i], 
            1 - probs[i]))
    }
    var_count <- apply(x, 1, sum)
    if (any(var_count == 0)) {
        for (i in which(var_count == 0)) {
            x[i, ] <- sample(0:1, replace = TRUE, size = vars)
        }
    }
    x
}, selection = function (population, fitness, r = NULL, q = NULL, 
    ...) 
{
    popSize = nrow(population)
    if (is.null(r)) 
        r <- 2/(popSize * (popSize - 1))
    if (is.null(q)) 
        q <- 2/popSize
    rank <- (popSize + 1) - rank(fitness, ties.method = "random")
    prob <- q - (rank - 1) * r
    sel <- sample(1:popSize, size = popSize, prob = pmin(pmax(0, 
        prob), 1, na.rm = TRUE), replace = TRUE)
    out <- list(population = population[sel, , drop = FALSE], 
        fitness = fitness[sel])
    out
}, crossover = function (population, fitness, parents, ...) 
{
    fitness <- fitness[parents]
    parents <- population[parents, , drop = FALSE]
    n <- ncol(parents)
    children <- matrix(as.double(NA), nrow = 2, ncol = n)
    fitnessChildren <- rep(NA, 2)
    crossOverPoint <- sample(0:n, size = 1)
    if (crossOverPoint == 0) {
        children[1:2, ] <- parents[2:1, ]
        fitnessChildren[1:2] <- fitness[2:1]
    }
    else if (crossOverPoint == n) {
        children <- parents
        fitnessChildren <- fitness
    }
    else {
        children[1, ] <- c(parents[1, 1:crossOverPoint], parents[2, 
            (crossOverPoint + 1):n])
        children[2, ] <- c(parents[2, 1:crossOverPoint], parents[1, 
            (crossOverPoint + 1):n])
    }
    out <- list(children = children, fitness = fitnessChildren)
    out
}, mutation = function (population, parent, ...) 
{
    mutate <- parent <- as.vector(population[parent, ])
    n <- length(parent)
    j <- sample(1:n, size = 1)
    mutate[j] <- abs(mutate[j] - 1)
    mutate
}, selectIter = function (x, metric, maximize) 
{
    bestIter <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    bestIter
}), .Names = c("fit", "pred", "fitness_intern", "fitness_extern", 
"initial", "selection", "crossover", "mutation", "selectIter"
))


modelCor <- function (x, metric = x$metric[1], ...) 
{
    dat <- x$values[, grep(paste("~", metric[1], sep = ""), names(x$values))]
    colnames(dat) <- gsub(paste("~", metric[1], sep = ""), "", 
        colnames(dat))
    cor(dat, ...)
}


flatTable <- function (pred, obs) 
{
    cells <- as.vector(table(pred, obs))
    if (length(cells) == 0) 
        cells <- rep(NA, length(levels(obs))^2)
    names(cells) <- paste(".cell", seq(along = cells), sep = "")
    cells
}


pcaNNet <- function (x, ...) 
UseMethod("pcaNNet")


caretSBF <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, ...) 
{
    if (ncol(x) > 0) {
        train(x, y, ...)
    }
    else nullModel(y = y)
}, pred = function (object, x) 
{
    if (class(object) != "nullModel") {
        tmp <- predict(object, x)
        if (object$modelType == "Classification" & !is.null(object$modelInfo$prob)) {
            out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
                x, type = "prob")))
        }
        else out <- tmp
    }
    else {
        tmp <- predict(object, x)
        if (!is.null(object$levels)) {
            out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
                x, type = "prob")))
        }
        else out <- tmp
    }
    out
}, score = function (x, y) 
{
    if (is.factor(y)) 
        anovaScores(x, y)
    else gamScores(x, y)
}, filter = function (score, x, y) 
score <= 0.05), .Names = c("summary", "fit", "pred", "score", 
"filter"))


knnregTrain <- function (train, test, y, k = 5, use.all = TRUE) 
{
    train <- as.matrix(train)
    if (is.null(dim(test))) 
        dim(test) <- c(1, length(test))
    test <- as.matrix(test)
    if (any(is.na(train)) || any(is.na(test)) || any(is.na(y))) 
        stop("no missing values are allowed")
    p <- ncol(train)
    ntr <- nrow(train)
    if (length(y) != ntr) 
        stop("'train' and 'class' have different lengths")
    if (ntr < k) {
        warning(gettextf("k = %d exceeds number %d of patterns", 
            k, ntr), domain = NA)
        k <- ntr
    }
    if (k < 1) 
        stop(gettextf("k = %d must be at least 1", k), domain = NA)
    nte <- nrow(test)
    if (ncol(test) != p) 
        stop("dims of 'test' and 'train differ")
    Z <- .C("knn3reg", as.integer(k), as.integer(ntr), as.integer(nte), 
        as.integer(p), as.double(train), as.double(y), as.double(test), 
        double(nte), as.integer(FALSE), as.integer(use.all))
    Z[[8]]
}


LPH07_1 <- function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", 
    corrValue = 0, factors = FALSE, class = FALSE) 
{
    dat <- matrix(rbinom(n * 10, size = 1, prob = 0.4), ncol = 10)
    dat <- as.data.frame(dat)
    colnames(dat) <- well_numbered("Var", ncol(dat))
    foo <- function(w) 2 * w[1] * w[10] + 4 * w[2] * w[7] + 3 * 
        w[4] * w[5] - 5 * w[6] * w[10] + 3 * w[8] * w[9] + w[1] * 
        w[2] * w[4] - 2 * w[7] * (1 - w[6]) * w[2] * w[9] - 4 * 
        (1 - w[10]) * w[1] * (1 - w[4])
    if (noiseVars > 0 | corrVars > 0) 
        dat <- cbind(dat, make_noise(n = n, noiseVars = noiseVars, 
            corrVars = corrVars, corrType = corrType, corrValue = corrValue, 
            binary = TRUE))
    if (class) {
        dat$y <- apply(dat[, 1:10], 1, foo)
        dat$Class <- runif(nrow(dat)) <= binomial()$linkinv(dat$y)
        dat$Class <- factor(ifelse(dat$Class, "Class1", "Class2"))
        dat$y <- NULL
    }
    else dat$y <- apply(dat[, 1:10], 1, foo) + rnorm(n)
    if (factors) 
        for (i in grep("(^Var)|(^Noise)", names(dat), value = TRUE)) dat[, 
            i] <- factor(paste0("val", dat[, i]))
    dat
}


LPH07_2 <- function (n = 100, noiseVars = 0, corrVars = 0, corrType = "AR1", 
    corrValue = 0) 
{
    dat <- matrix(rnorm(n * 20, sd = 4), ncol = 20)
    dat <- as.data.frame(dat)
    colnames(dat) <- well_numbered("Var", ncol(dat))
    foo <- function(x) x[1] * x[2] + x[10]^2 - x[3] * x[17] - 
        x[15] * x[4] + x[9] * x[5] + x[19] - x[20]^2 + x[9] * 
        x[8]
    if (noiseVars > 0 | corrVars > 0) 
        dat <- cbind(dat, make_noise(n = n, noiseVars = noiseVars, 
            corrVars = corrVars, corrType = corrType, corrValue = corrValue))
    dat$y <- apply(dat[, 1:20], 1, foo) + rnorm(n, sd = 4)
    dat
}


knn3Train <- function (train, test, cl, k = 1, l = 0, prob = TRUE, use.all = TRUE) 
{
    train <- as.matrix(train)
    if (is.null(dim(test))) 
        dim(test) <- c(1, length(test))
    test <- as.matrix(test)
    if (any(is.na(train)) || any(is.na(test)) || any(is.na(cl))) 
        stop("no missing values are allowed")
    p <- ncol(train)
    ntr <- nrow(train)
    if (length(cl) != ntr) 
        stop("'train' and 'class' have different lengths")
    if (ntr < k) {
        warning(gettextf("k = %d exceeds number %d of patterns", 
            k, ntr), domain = NA)
        k <- ntr
    }
    if (k < 1) 
        stop(gettextf("k = %d must be at least 1", k), domain = NA)
    nte <- nrow(test)
    if (ncol(test) != p) 
        stop("dims of 'test' and 'train differ")
    clf <- as.factor(cl)
    nc <- max(unclass(clf))
    Z <- .C("knn3", as.integer(k), as.integer(l), as.integer(ntr), 
        as.integer(nte), as.integer(p), as.double(train), as.integer(unclass(clf)), 
        as.double(test), integer(nc + 1), as.integer(nc), as.integer(FALSE), 
        as.integer(use.all), all_vote = double(as.integer(nte * 
            nc)))
    classProbs <- matrix(Z$all_vote, nrow = nte, ncol = nc, byrow = TRUE)
    colnames(classProbs) <- sort(unique(clf))
    bestClass <- function(x) {
        out <- which(x == max(x))
        if (length(out) > 1) 
            out <- sample(out, 1)
        out
    }
    res <- colnames(classProbs)[apply(classProbs, 1, bestClass)]
    votes <- apply(classProbs * k, 1, max)
    inDoubt <- (votes < l)
    if (any(inDoubt)) 
        res[inDoubt] <- NA
    if (prob) 
        attr(res, "prob") <- classProbs
    res
}


safsControl <- function (functions = NULL, method = "repeatedcv", metric = NULL, 
    maximize = NULL, number = ifelse(grepl("cv", method), 10, 
        25), repeats = ifelse(grepl("cv", method), 1, 5), verbose = FALSE, 
    returnResamp = "final", p = 0.75, index = NULL, indexOut = NULL, 
    seeds = NULL, holdout = 0, improve = Inf, allowParallel = TRUE) 
{
    if (!(method %in% c("cv", "boot", "repeatedcv", "LGOCV", 
        "LOOCV"))) 
        stop("method should be one of: \"cv\", \"boot\", \"repeatedcv\", \"LGOCV\" or \"LOOCV\"")
    if (holdout < 0 | holdout >= 1) 
        stop("'holdout' should be in [0, 1)")
    if (improve < 2) 
        stop("'improve' should be >= 2")
    if (!is.null(metric)) {
        if (length(metric) != 2) 
            stop("'metric' should be a two-element named vector. See ?safsControl")
        if (is.null(names(metric)) || any(sort(names(metric)) != 
            c("external", "internal"))) 
            stop("'metric' should have names 'internal' and 'external' See ?safsControl")
    }
    if (!is.null(maximize)) {
        if (length(maximize) != 2) 
            stop("'maximize' should be a two-element named vector. See ?safsControl")
        if (is.null(names(maximize)) || any(sort(names(maximize)) != 
            c("external", "internal"))) 
            stop("'maximize' should have names 'internal' and 'external' See ?safsControl")
    }
    list(functions = if (is.null(functions)) caretFuncs else functions, 
        method = method, metric = metric, maximize = maximize, 
        number = number, repeats = repeats, returnResamp = returnResamp, 
        verbose = verbose, p = p, index = index, indexOut = indexOut, 
        seeds = seeds, holdout = holdout, improve = improve, 
        allowParallel = allowParallel)
}


createDataPartition <- function (y, times = 1, p = 0.5, list = TRUE, groups = min(5, 
    length(y))) 
{
    if (class(y)[1] == "Surv") 
        y <- y[, "time"]
    out <- vector(mode = "list", times)
    if (length(y) < 2) 
        stop("y must have at least 2 data points")
    if (groups < 2) 
        groups <- 2
    if (is.numeric(y)) {
        y <- cut(y, unique(quantile(y, probs = seq(0, 1, length = groups))), 
            include.lowest = TRUE)
    }
    else {
        xtab <- table(y)
        if (any(xtab == 0)) {
            warning(paste("Some classes have no records (", paste(names(xtab)[xtab == 
                0], sep = "", collapse = ", "), ") and these will be ignored"))
            y <- factor(as.character(y))
        }
        if (any(xtab == 1)) {
            warning(paste("Some classes have a single record (", 
                paste(names(xtab)[xtab == 1], sep = "", collapse = ", "), 
                ") and these will be selected for the sample"))
        }
    }
    subsample <- function(dat, p) {
        if (nrow(dat) == 1) {
            out <- dat$index
        }
        else {
            num <- ceiling(nrow(dat) * p)
            out <- sample(dat$index, size = num)
        }
        out
    }
    for (j in 1:times) {
        tmp <- dlply(data.frame(y = y, index = seq(along = y)), 
            .(y), subsample, p = p)
        tmp <- sort(as.vector(unlist(tmp)))
        out[[j]] <- tmp
    }
    if (!list) {
        out <- matrix(unlist(out), ncol = times)
        colnames(out) <- prettySeq(1:ncol(out))
    }
    else {
        names(out) <- prettySeq(out)
    }
    out
}


treebagFuncs <- structure(list(summary = function (data, lev = NULL, model = NULL) 
{
    if (is.character(data$obs)) 
        data$obs <- factor(data$obs, levels = lev)
    postResample(data[, "pred"], data[, "obs"])
}, fit = function (x, y, first, last, ...) 
{
    loadNamespace("ipred")
    ipred::ipredbagg(y, x, ...)
}, pred = function (object, x) 
{
    tmp <- predict(object, x)
    if (is.factor(object$y)) {
        out <- cbind(data.frame(pred = tmp), as.data.frame(predict(object, 
            x, type = "prob")))
    }
    else out <- tmp
    out
}, rank = function (object, x, y) 
{
    vimp <- varImp(object)
    vimp <- vimp[order(vimp$Overall, decreasing = TRUE), , drop = FALSE]
    vimp$var <- rownames(vimp)
    vimp
}, selectSize = function (x, metric, maximize) 
{
    best <- if (maximize) 
        which.max(x[, metric])
    else which.min(x[, metric])
    min(x[best, "Variables"])
}, selectVar = function (y, size) 
{
    finalImp <- ddply(y[, c("Overall", "var")], .(var), function(x) mean(x$Overall, 
        na.rm = TRUE))
    names(finalImp)[2] <- "Overall"
    finalImp <- finalImp[order(finalImp$Overall, decreasing = TRUE), 
        ]
    as.character(finalImp$var[1:size])
}), .Names = c("summary", "fit", "pred", "rank", "selectSize", 
"selectVar"))




## Package Data

GermanCredit <- caret::GermanCredit		## German Credit Data

Sacramento <- caret::Sacramento		## Sacramento CA Home Prices

cars <- caret::cars		## Kelly Blue Book resale data for 2005 model year GM cars

dhfr <- caret::dhfr		## Dihydrofolate Reductase Inhibitors Data

scat <- caret::scat		## Morphometric Data on Scat

segmentationData <- caret::segmentationData		## Cell Body Segmentation



## Package Info

.skeleton_package_title = "Classification and Regression Training"

.skeleton_package_version = "6.0-76"

.skeleton_package_depends = "lattice,ggplot2"

.skeleton_package_imports = "car,foreach,methods,plyr,ModelMetrics,nlme,reshape2,stats,stats4,utils,grDevices"


## Internal

.skeleton_version = 5


## EOF