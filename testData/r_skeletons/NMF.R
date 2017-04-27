##
## Exported symobls in package `NMF`
##

## Exported package methods

compare <- function (object, ...) 
standardGeneric("compare")


`.basis<-` <- function (object, value) 
standardGeneric(".basis<-")


purity <- function (x, y, ...) 
standardGeneric("purity")


ForeachBackend <- function (object, ...) 
standardGeneric("ForeachBackend")


fitted <- stats::fitted # re-exported from stats package

`.__T__.atrack:NMF` <- "<environment>"

seed <- function (x, model, method, ...) 
standardGeneric("seed")


nmfWrapper <- function (method, ..., .FIXED = FALSE) 
{
    .call <- match.call()
    if (nargs() > 1L && any(names(.call)[-(1:2)] == "")) 
        stop("Invalid call: all arguments must be named.")
    .fixedargs <- "method"
    .defaults <- names(.call)[-1L]
    .defaults <- .defaults[!.defaults %in% "method"]
    if (length(.defaults)) {
        if (isTRUE(.FIXED)) 
            .fixedargs <- c(.fixedargs, .defaults)
        else if (is.character(.FIXED)) {
            .FIXED <- .FIXED[.FIXED %in% .defaults]
            .fixedargs <- c(.fixedargs, .FIXED)
        }
    }
    .method <- method
    .checkArgs <- function(ca, args) {
        nm <- names(ca)[-1L]
        if (any(fnm <- !is.na(pmatch(nm, .fixedargs)))) {
            warning("Discarding fixed arguments from wrapped call to ", 
                .call[1L], " [", str_out(nm[fnm], Inf), "].", 
                immediate. = TRUE)
            ca <- ca[!c(FALSE, fnm)]
        }
        .call <- ca
        if (length(.defaults)) {
            defaults <- args[.defaults]
            .call <- expand_list(ca, defaults, .exact = FALSE)
        }
        .call[[1L]] <- as.name("nmf")
        .call[["method"]] <- force(.method)
        as.call(.call)
    }
    fwrap <- function(...) {
        ca <- match.call()
        args <- formals()
        .call <- .checkArgs(ca, args)
        e <- parent.frame()
        eval(.call, envir = e)
    }
    if (length(.defaults)) {
        formals(fwrap) <- expand_list(formals(fwrap), as.list(.call[.defaults]))
    }
    if (length(meth <- nmfFormals(.method))) {
        formals(fwrap) <- expand_list(formals(fwrap), meth)
    }
    return(fwrap)
}


offset <- stats::offset # re-exported from stats package

`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

staticVar <- function (name, value, init = FALSE) 
{
    if (missing(name)) 
        return(.Workspace)
    else if (is.null(name)) {
        .Workspace <<- NULL
        return()
    }
    else if (is.environment(name)) {
        nmf.debug("Strategy Workspace", "initialize static workspace: ", 
            capture.output(.Workspace), "=", capture.output(name))
        .Workspace <<- name
    }
    else if (isString(name) && is.environment(.Workspace)) {
        if (missing(value)) {
            get(name, envir = .Workspace, inherits = FALSE)
        }
        else {
            if (!init || !exists(name, envir = .Workspace, inherits = FALSE)) {
                if (init) 
                  nmf.debug("Strategy Workspace", "initialize variable '", 
                    name, "'")
                assign(name, value, envir = .Workspace)
            }
            get(name, envir = .Workspace, inherits = FALSE)
        }
    }
    else {
        stop("Invalid NMF workspace query: .Workspace=", class(.Workspace), 
            "| name=", name, if (!missing(value)) 
                paste0(" | value=", class(value)))
    }
}


rnmf <- function (x, target, ...) 
standardGeneric("rnmf")


`.__T__getRNG1:rngtools` <- rngtools::`.__T__getRNG1:rngtools` # re-exported from rngtools package

.getRNG <- rngtools::.getRNG # re-exported from rngtools package

`.__T__coefficients:stats` <- "<environment>"

basis <- function (object, ...) 
standardGeneric("basis")


`.__T__run:NMF` <- "<environment>"

.coef <- function (object, ...) 
standardGeneric(".coef")


is.empty.nmf <- function (x, ...) 
{
    nrow(x) == 0 && ncol(x) == 0
}


nmfEstimateRank <- function (x, range, method = nmf.getOption("default.algorithm"), 
    nrun = 30, model = NULL, ..., verbose = FALSE, stop = FALSE) 
{
    if (is.null(method)) 
        method <- nmf.getOption("default.algorithm")
    if (is(x, "formula")) {
        dummy <- nmfModel(x, 0L, data = model)
        V <- attr(dummy, "target")
    }
    else {
        V <- x
    }
    range <- sort(unique(range))
    c.matrices <- setNames(lapply(range, function(x) NA), as.character(range))
    fit <- setNames(lapply(range, function(x) NA), as.character(range))
    bootstrap.measures <- list()
    comb <- function(...) {
        measures <- list(...)
        err <- which(sapply(measures, is.character))
        if (length(err) == length(measures)) {
            msg <- paste(paste("#", seq_along(range), " ", measures, 
                sep = ""), collapse = "\n\t-")
            stop("All the runs produced an error:\n\t-", msg)
        }
        else if (length(err) > 0) {
            measures.ok <- sapply(measures[-err], function(x) x)
            n <- nrow(measures.ok)
            tmp.res <- matrix(as.numeric(NA), n, length(range))
            rownames(tmp.res) <- rownames(measures.ok)
            tmp.res[, -err] <- measures.ok
            tmp.res["rank", err] <- range[err]
            msg <- paste(paste("#", err, measures[err], " ", 
                sep = ""), collapse = "\n\t-")
            warning("NAs were produced due to errors in some of the runs:\n\t-", 
                msg)
            tmp.res
        }
        else sapply(measures, function(x) x)
    }
    k.rank <- 0
    measures <- sapply(range, function(r, ...) {
        k.rank <<- k.rank + 1L
        if (verbose) 
            cat("Compute NMF rank=", r, " ... ")
        orng <- RNGseed()
        if (k.rank < length(range)) 
            on.exit(RNGseed(orng), add = TRUE)
        res <- tryCatch({
            res <- nmf(x, r, method, nrun = nrun, model = model, 
                ...)
            if (!isNMFfit(res, recursive = FALSE)) 
                return(res)
            c.matrices[[as.character(r)]] <<- consensus(res)
            fit[[as.character(r)]] <<- res
            if (verbose) 
                cat("+ measures ... ")
            measures <- summary(res, target = V)
            if (verbose) 
                cat("OK\n")
            measures
        }, error = function(e) {
            mess <- if (is.null(e$call)) 
                e$message
            else paste(e$message, " [in call to '", e$call[1], 
                "']", sep = "")
            mess <- paste("[r=", r, "] -> ", mess, sep = "")
            if (stop) {
                if (verbose) 
                  cat("\n")
                stop(mess, call. = FALSE)
            }
            if (verbose) 
                message("ERROR")
            return(mess)
        })
        res
    }, ..., simplify = FALSE)
    measures <- do.call(comb, measures)
    measures <- as.data.frame(t(measures))
    res <- list(measures = measures, consensus = c.matrices, 
        fit = fit)
    class(res) <- "NMF.rank"
    return(res)
}


nmf.stop.stationary <- function (object, i, y, x, stationary.th = .Machine$double.eps, 
    check.interval = 5 * check.niter, check.niter = 10L, ...) 
{
    if (check.interval < check.niter) {
        stop("Invalid argument values: `check.interval` must always be greater than `check.niter`")
    }
    if (i == 0L || (i == 1L && is.null(.last.objective.value))) {
        .reset_value()
        if (is.partial.nmf(x)) 
            return(FALSE)
        current.value <- deviance(object, x, y, ...)
        if (is.nan(current.value)) 
            return(TRUE)
        .store_value(current.value)
        return(FALSE)
    }
    if (.niter == 0L && i%%check.interval != 0) 
        return(FALSE)
    current.value <- deviance(object, x, y, ...)
    if (is.nan(current.value)) 
        return(TRUE)
    .store_value(current.value)
    if (.niter == check.niter + 1) {
        crit <- abs(.last.objective.value[1L] - .last.objective.value[2L])/check.niter
        if (crit <= stationary.th) {
            if (nmf.getOption("verbose")) {
                message(crit)
            }
            return(TRUE)
        }
        .reset_value()
    }
    FALSE
}


nrun <- function (object, ...) 
standardGeneric("nrun")


randomize <- function (x, ...) 
{
    if (is(x, "ExpressionSet")) 
        x <- Biobase::exprs(x)
    apply(x, 2, function(c, ...) sample(c, size = length(c), 
        ...), ...)
}


`.__T__profcor:NMF` <- "<environment>"

nmf <- function (x, rank, method, ...) 
standardGeneric("nmf")


nbterms <- function (object) 
{
    length(ibterms(object))
}


summary <- base::summary # re-exported from base package

loadings <- stats::loadings # re-exported from stats package

nmf_update.euclidean.h_R <- function (v, w, h, wh = NULL, eps = 10^-9) 
{
    den <- if (is.null(wh)) 
        crossprod(w) %*% h
    else {
        t(w) %*% wh
    }
    pmax(h * crossprod(w, v), eps)/(den + eps)
}


`.__T__algorithm<-:NMF` <- "<environment>"

ibasis <- function (object, ...) 
{
    i <- 1:nbasis(object)
    if (length(idx <- ibterms(object, ...))) 
        i[-idx]
    else i
}


`.__T__residuals<-:NMF` <- "<environment>"

removeNMFSeed <- function (name, ...) 
{
    pkgreg_remove("seed", key = name, ...)
}


syntheticNMF <- function (n, r, p, offset = NULL, noise = TRUE, factors = FALSE, 
    seed = NULL) 
{
    if (!is.null(seed)) {
        os <- RNGseed()
        on.exit(RNGseed(os))
        set.seed(seed)
    }
    mu.W <- 1
    sd.W <- 1
    if (isTRUE(noise)) {
        noise <- list(mean = 0, sd = 1)
    }
    else if (isNumber(noise)) {
        noise <- list(mean = 0, sd = noise)
    }
    else if (is.list(noise)) {
        stopifnot(length(noise) == 2L)
        noise <- setNames(noise, c("mean", "sd"))
    }
    else noise <- FALSE
    if (length(r) == 1) {
        g <- rmultinom(1, p, rep(1, r))
    }
    else {
        g <- r
        p <- sum(r)
        r <- length(r)
    }
    H <- matrix(0, r, p)
    tmp <- 0
    for (i in 1:r) {
        H[i, (tmp + 1):(tmp + g[i])] <- 1
        tmp <- tmp + g[i]
    }
    if (length(n) == 1) {
        b <- rmultinom(1, n, rep(1, r))
    }
    else {
        b <- n
        n <- sum(n)
    }
    W <- matrix(0, n, r)
    tmp <- 0
    for (i in 1:r) {
        W[(tmp + 1):(tmp + b[i]), i] <- abs(rnorm(b[i], mu.W, 
            sd.W))
        tmp <- tmp + b[i]
    }
    res <- W %*% H
    if (!is.null(offset)) {
        if (length(offset) == 1L) 
            offset <- rnorm(n, mean = 0, sd = offset)
        stopifnot(length(offset) == n)
        res <- res + offset
    }
    if (!isFALSE(noise)) 
        res <- pmax(res + rmatrix(res, dist = rnorm, mean = noise$mean, 
            sd = noise$sd), 0)
    pData <- list(Group = factor(unlist(mapply(rep, 1:r, g, SIMPLIFY = FALSE))))
    fData <- list(Group = factor(unlist(mapply(rep, 1:r, b, SIMPLIFY = FALSE))))
    if (factors) 
        res <- list(res, W = W, H = H, offset = offset, pData = pData, 
            fData = fData)
    ExposeAttribute(res, coefficients = H, basis = W, offset = offset, 
        pData = pData, fData = fData, .VALUE = TRUE, .MODE = "r")
}


nmf_update.offset <- function (i, v, x, copy = FALSE, eps = 10^-9, ...) 
{
    w <- .basis(x)
    h <- .coef(x)
    off <- offset(x)
    if (i == 1 && length(off) == 0) 
        off <- rowMeans(v)
    h <- nmf_update.euclidean_offset.h(v, w, h, off, eps = eps, 
        copy = copy)
    w <- nmf_update.euclidean_offset.w(v, w, h, off, eps = eps, 
        copy = copy)
    x@offset <- off * pmax(rowSums(v), eps)/(rowSums(w %*% h + 
        off) + eps)
    if (copy) {
        .basis(x) <- w
        .coef(x) <- h
    }
    return(x)
}


runtime.all <- function (object, ...) 
standardGeneric("runtime.all")


`residuals<-` <- function (object, ..., value) 
standardGeneric("residuals<-")


atrack <- function (..., order = NULL, enforceNames = FALSE, .SPECIAL = NA, 
    .DATA = NULL, .CACHE = NULL) 
{
    l <- list(...)
    if (length(l) == 1L && is.atrack(l[[1]])) 
        object <- l[[1L]]
    else if (length(l) > 0) {
        object <- list()
        lapply(seq_along(l), function(i) {
            x <- l[[i]]
            if (is_NA(x) || is.null(x)) 
                return()
            xa <- .atrack(x, data = .DATA)
            if (is_NA(xa) || is.null(xa)) 
                return()
            n <- names(object)
            if (!is.list(xa)) 
                xa <- setNames(list(xa), names(l)[i])
            if (is.null(xa) || is_NA(xa)) 
                return()
            if (is.null(object)) 
                object <<- xa
            else object <<- c(object, xa)
        })
    }
    if (is.null(object)) 
        return()
    if (!length(object)) 
        return(annotationTrack())
    object <- annotationTrack(object)
    if (is.list(.SPECIAL)) {
        m <- match_atrack_code(object, names(.SPECIAL))
        i_spe <- which(m != 0L)
        if (length(i_spe)) {
            if (is.null(names(object))) 
                names(object) <- rep("", length(object))
            if (anyDuplicated(m[i_spe])) {
                g <- split(i_spe, m[i_spe])
                sapply(g, function(i) {
                  n <- names(object)[i]
                  if (length(n <- n[n != ""])) 
                    names(object)[i] <<- n[1L]
                })
                idup <- which(duplicated(m) & m != 0L)
                object <- object[-idup]
                m <- m[-idup]
                i_spe <- which(m != 0L)
            }
            if (anyValue(.CACHE)) {
                if (!is.atrack(.CACHE)) 
                  stop("Argument .CACHE should be an annotation track object. [", 
                    class(.CACHE), "]")
                i_spe_cache <- atrack_code(.CACHE)
                if (length(i_spe_cache)) {
                  .CACHE_SPE <- unlist(.CACHE[i_spe_cache])
                  if (!is.null(names(.CACHE_SPE))) {
                    sapply(i_spe, function(i) {
                      x <- object[[i]]
                      if (names(object)[i] == "" && !is_NA(j <- match(x, 
                        .CACHE_SPE)) && names(.CACHE_SPE)[j] != 
                        "") {
                        names(object)[i] <<- names(.CACHE_SPE)[j]
                      }
                    })
                  }
                }
            }
            a <- sapply(m[i_spe], function(i) .SPECIAL[[i]](), 
                simplify = FALSE)
            object[i_spe] <- a
            nm <- names(object)[i_spe]
            names(object)[i_spe] <- ifelse(nm != "", nm, names(a))
        }
        if (length(i <- atrack_code(object))) {
            warning("Discarding unresolved special annotation tracks: ", 
                str_out(unlist(object[i]), use.names = TRUE))
            object <- object[-i]
        }
    }
    if (enforceNames) {
        n <- names(object)
        xnames <- paste("X", 1:length(object), sep = "")
        if (is.null(n)) 
            names(object) <- xnames
        else names(object)[n == ""] <- xnames[n == ""]
    }
    if (!is.null(order)) {
        object <- sapply(object, function(x) x[order], simplify = FALSE)
    }
    annotationTrack(object)
}


`.__T__dimnames<-:base` <- "<environment>"

sparseness <- function (x, ...) 
standardGeneric("sparseness")


setDoBackend <- function (data, cleanup = FALSE) 
{
    ob <- getDoBackend()
    ofb <- ForeachBackend()
    if (cleanup) {
        doBackendCleanup(ofb)
    }
    if (!is.null(data)) {
        bdata <- data
        if (is.backend(data)) 
            data <- data[!names(data) %in% c("name", "cleanup")]
        do.call("setDoPar", data)
        setBackendCleanup(bdata)
    }
    else {
        do.call("setDoPar", list(NULL))
        fe <- ns_get(".foreachGlobals", "foreach")
        if (exists("fun", envir = fe, inherits = FALSE)) 
            remove("fun", envir = fe)
        setBackendCleanup(NULL)
    }
    invisible(ob)
}


evar <- function (object, ...) 
standardGeneric("evar")


basisnames <- function (x, ...) 
standardGeneric("basisnames")


profplot <- function (x, ...) 
{
    UseMethod("profplot")
}


`.__T__compare:NMF` <- "<environment>"

bterms <- function (object) 
{
    object@bterms
}


nmf.printOptions <- function () 
print(.OPTOBJ)


`.__T__featureScore:NMF` <- "<environment>"

`.__T__.coef<-:NMF` <- "<environment>"

featureScore <- function (object, ...) 
standardGeneric("featureScore")


nmf.options <- function (...) 
{
    .options(..., .DATA = .OPTOBJ)
}


.atrack <- function (object, ...) 
standardGeneric(".atrack")


pmax.inplace <- function (x, lim, skip = NULL) 
{
    .Call("ptr_pmax", x, lim, as.integer(skip), PACKAGE = "NMF")
}


`.__T__niter<-:NMF` <- "<environment>"

`.__T__summary:base` <- "<environment>"

isNMFfit <- function (object, recursive = TRUE) 
{
    res <- is(object, "NMFfit") || is(object, "NMFfitX")
    if (!res && recursive && is.list(object)) 
        sapply(object, isNMFfit)
    else res
}


`.__T__scoef:NMF` <- "<environment>"

`.__T__rmatrix:NMF` <- "<environment>"

nmf.getOption <- function (x, default = NULL) 
{
    options <- .OPTOBJ$options
    if (missing(default)) 
        return(options(x)[[1L]])
    if (x %in% names(options())) 
        options(x)[[1L]]
    else default
}


nmf_update.KL.h_R <- function (v, w, h, wh = NULL) 
{
    if (is.null(wh)) 
        wh <- w %*% h
    h * crossprod(w, v/wh)/colSums(w)
}


`.__T__offset:stats` <- "<environment>"

name <- function (object, ...) 
standardGeneric("name")


consensus <- function (object, ...) 
standardGeneric("consensus")


`.__T__coef:stats` <- "<environment>"

`.__T__icterms:NMF` <- "<environment>"

`.__T__entropy:NMF` <- "<environment>"

hasCoef <- function (x) 
nbasis(x) && ncol(coef(x)) != 0L


`.__T__seeding:NMF` <- "<environment>"

.__C__NMFfitXn <- new("classRepresentation"
    , slots = structure(list(.Data = structure("list", package = "methods"), 
    runtime.all = structure("proc_time", package = "NMF")), .Names = c(".Data", 
"runtime.all"))
    , contains = structure(list(NMFfitX = S4_object(), 
    list = S4_object(), 
    vector = S4_object()), .Names = c("NMFfitX", 
"list", "vector"))
    , virtual = FALSE
    , prototype = new("list"
)
    , validity = function (object) 
{
    ref.dim <- NULL
    ref.algo <- NULL
    for (i in seq_along(object)) {
        item <- object[[i]]
        if (!(is(item, "NMFfit") && !is(item, "NMFfitX"))) 
            return(paste("invalid class for element", i, "of input list [all elements must be a NMFfit object]"))
        if (is.null(ref.dim)) 
            ref.dim <- dim(item)
        if (!identical(ref.dim, dim(item))) 
            return(paste("invalid dimension for element", i, 
                "of input list [all elements must have the same dimensions]"))
        if (is.null(ref.algo)) 
            ref.algo <- algorithm(item)
        if (!identical(ref.algo, algorithm(item))) 
            return(paste("invalid algorithm for element", i, 
                "of input list [all elements must result from the same algorithm]"))
    }
}
    , access = list()
    , className = structure("NMFfitXn", package = "NMF")
    , package = "NMF"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__nneg:NMF` <- "<environment>"

hasBasis <- function (x) 
nbasis(x) && nrow(basis(x)) != 0L


`.__T__predict:stats` <- "<environment>"

aheatmap <- function (x, color = "-RdYlBu2:100", breaks = NA, border_color = NA, 
    cellwidth = NA, cellheight = NA, scale = "none", Rowv = TRUE, 
    Colv = TRUE, revC = identical(Colv, "Rowv") || is_NA(Rowv) || 
        (is.integer(Rowv) && length(Rowv) > 1) || is(Rowv, "silhouette"), 
    distfun = "euclidean", hclustfun = "complete", reorderfun = function(d, 
        w) reorder(d, w), treeheight = 50, legend = TRUE, annCol = NA, 
    annRow = NA, annColors = NA, annLegend = TRUE, labRow = NULL, 
    labCol = NULL, subsetRow = NULL, subsetCol = NULL, txt = NULL, 
    fontsize = 10, cexRow = min(0.2 + 1/log10(nr), 1.2), cexCol = min(0.2 + 
        1/log10(nc), 1.2), filename = NA, width = NA, height = NA, 
    main = NULL, sub = NULL, info = NULL, verbose = getOption("verbose"), 
    gp = gpar()) 
{
    ol <- lverbose(verbose)
    on.exit(lverbose(ol))
    if (is(x, "ExpressionSet")) {
        requireNamespace("Biobase")
        if (isTRUE(annCol)) 
            annCol <- atrack(x)
        x <- Biobase::exprs(x)
    }
    mat <- x
    if (!is.null(txt)) {
        if (!all(dim(mat), dim(x))) {
            stop("Incompatible data and text dimensions: arguments x and txt must have the same size.")
        }
    }
    res <- list()
    if (length(treeheight) == 1) 
        treeheight <- c(treeheight, treeheight)
    treeheight_row <- treeheight[1]
    treeheight_col <- treeheight[2]
    if (!is.null(subsetRow)) {
        if (verbose) 
            message("Compute row subset indexes")
        subsetRow <- subset_index(mat, 1L, subsetRow)
    }
    if (!is.null(subsetCol)) {
        if (verbose) 
            message("Compute column subset indexes")
        subsetCol <- subset_index(mat, 2L, subsetCol)
    }
    if (is.null(labRow) && is.null(rownames(mat))) 
        labRow <- 1L
    if (!is.null(labRow)) {
        if (verbose) 
            message("Process labRow")
        rownames(mat) <- generate_dimnames(labRow, nrow(mat), 
            rownames(mat))
    }
    if (is.null(labCol) && is.null(colnames(mat))) 
        labCol <- 1L
    if (!is.null(labCol)) {
        if (verbose) 
            message("Process labCol")
        colnames(mat) <- generate_dimnames(labCol, ncol(mat), 
            colnames(mat))
    }
    if (!is.null(subsetRow)) {
        mat <- mat[subsetRow, ]
    }
    if (!is.null(subsetCol)) {
        mat <- mat[, subsetCol]
    }
    tree_row <- if (!is_NA(Rowv)) {
        if (verbose) 
            message("Cluster rows")
        if (isReal(Rowv)) {
            treeheight_row <- Rowv
            Rowv <- TRUE
        }
        cluster_mat(mat, Rowv, distfun = distfun, hclustfun = hclustfun, 
            reorderfun = reorderfun, subset = subsetRow, verbose = verbose)
    }
    else NA
    if (identical(Rowv, FALSE) || !is_treedef(tree_row)) 
        treeheight_row <- 0
    tree_col <- if (!is_NA(Colv)) {
        if (identical(Colv, "Rowv")) {
            if (ncol(mat) != nrow(mat)) 
                stop("aheatmap - Colv='Rowv' but cannot treat columns and rows symmetrically: input matrix is not square.")
            treeheight_col <- treeheight_row
            tree_row
        }
        else {
            if (isReal(Colv)) {
                treeheight_col <- Colv
                Colv <- TRUE
            }
            if (verbose) 
                message("Cluster columns")
            cluster_mat(t(mat), Colv, distfun = distfun, hclustfun = hclustfun, 
                reorderfun = reorderfun, subset = subsetCol, 
                verbose = verbose)
        }
    }
    else NA
    if (identical(Colv, FALSE) || !is_treedef(tree_col)) 
        treeheight_col <- 0
    if (!is_NA(tree_row)) {
        if (revC) {
            if (verbose) 
                message("Reverse row clustering")
            tree_row <- rev(tree_row)
        }
        if (is_treedef(tree_row)) {
            res$Rowv <- tree_row$dendrogram
            res$rowInd <- order.dendrogram(tree_row$dendrogram)
            if (length(res$rowInd) != nrow(mat)) 
                stop("aheatmap - row dendrogram ordering gave index of wrong length (", 
                  length(res$rowInd), ")")
        }
        else {
            res$rowInd <- tree_row
            tree_row <- NA
        }
    }
    else if (revC) {
        res$rowInd <- nrow(mat):1L
    }
    res$rowInd <- subset2orginal_idx(res$rowInd, subsetRow)
    if (!is.null(res$rowInd)) {
        if (!is.integer(res$rowInd) || length(res$rowInd) != 
            nrow(mat)) 
            stop("aheatmap - Invalid row ordering: should be an integer vector of length nrow(mat)=", 
                nrow(mat))
        if (verbose) 
            message("Order rows")
        subInd <- attr(res$rowInd, "subset")
        ri <- if (is.null(subInd)) 
            res$rowInd
        else subInd
        mat <- mat[ri, , drop = FALSE]
        if (!is.null(txt)) 
            txt <- txt[ri, , drop = FALSE]
    }
    if (!is_NA(tree_col)) {
        if (is_treedef(tree_col)) {
            res$Colv <- tree_col$dendrogram
            res$colInd <- order.dendrogram(tree_col$dendrogram)
            if (length(res$colInd) != ncol(mat)) 
                stop("aheatmap - column dendrogram ordering gave index of wrong length (", 
                  length(res$colInd), ")")
        }
        else {
            res$colInd <- tree_col
            tree_col <- NA
        }
    }
    res$colInd <- subset2orginal_idx(res$colInd, subsetCol)
    if (!is.null(res$colInd)) {
        if (!is.integer(res$colInd) || length(res$colInd) != 
            ncol(mat)) 
            stop("aheatmap - Invalid column ordering: should be an integer vector of length ncol(mat)=", 
                ncol(mat))
        if (verbose) 
            message("Order columns")
        subInd <- attr(res$colInd, "subset")
        ci <- if (is.null(subInd)) 
            res$colInd
        else subInd
        mat <- mat[, ci, drop = FALSE]
        if (!is.null(txt)) 
            txt <- txt[, ci, drop = FALSE]
    }
    if (isTRUE(info) || is.character(info)) {
        if (verbose) 
            message("Compute info")
        if (!is.character(info)) 
            info <- NULL
        linfo <- NULL
        if (is_treedef(tree_row) && !is.null(tree_row$dist.method)) 
            linfo <- paste("rows:", tree_row$dist.method, "/", 
                tree_row$method)
        if (is_treedef(tree_col) && !is.null(tree_col$dist.method)) 
            linfo <- paste(linfo, paste(" - cols:", tree_col$dist.method, 
                "/", tree_col$method))
        info <- c(info, linfo)
    }
    if (is_treedef(tree_col)) 
        tree_col <- tree_col$dendrogram
    if (is_treedef(tree_row)) 
        tree_row <- tree_row$dendrogram
    if (verbose) 
        message("Scale matrix")
    mat = as.matrix(mat)
    mat = scale_mat(mat, scale)
    color <- ccRamp(color)
    if (is_NA(breaks) || isNumber(breaks)) {
        if (verbose) 
            message("Generate breaks")
        cbreaks <- if (isNumber(breaks)) 
            breaks
        else NA
        breaks = generate_breaks(as.vector(mat), length(color), 
            center = cbreaks)
    }
    if (isTRUE(legend)) {
        if (verbose) 
            message("Generate data legend breaks")
        legend = grid.pretty(range(as.vector(breaks)))
    }
    else {
        legend = NA
    }
    mat = scale_colours(mat, col = color, breaks = breaks)
    annotation_legend <- annLegend
    annotation_colors <- annColors
    annCol_processed <- atrack(annCol, order = res$colInd, .SPECIAL = specialAnnotation(2L), 
        .DATA = amargin(x, 2L), .CACHE = annRow)
    annRow_processed <- atrack(annRow, order = res$rowInd, .SPECIAL = specialAnnotation(1L), 
        .DATA = amargin(x, 1L), .CACHE = annCol)
    specialAnnotation(clear = TRUE)
    annTracks <- renderAnnotations(annCol_processed, annRow_processed, 
        annotation_colors = annotation_colors, verbose = verbose)
    nr <- nrow(mat)
    nc <- ncol(mat)
    res$vp <- heatmap_motor(mat, border_color = border_color, 
        cellwidth = cellwidth, cellheight = cellheight, treeheight_col = treeheight_col, 
        treeheight_row = treeheight_row, tree_col = tree_col, 
        tree_row = tree_row, filename = filename, width = width, 
        height = height, breaks = breaks, color = color, legend = legend, 
        annTracks = annTracks, annotation_legend = annotation_legend, 
        txt = txt, fontsize = fontsize, fontsize_row = cexRow * 
            fontsize, fontsize_col = cexCol * fontsize, main = main, 
        sub = sub, info = info, verbose = verbose, gp = gp)
    invisible(res)
}


.DollarNames <- Rcpp::.DollarNames # re-exported from Rcpp package

gVariable <- function (init, shared = FALSE) 
{
    if (shared) {
        if (!is.matrix(init)) 
            init <- as.matrix(init)
        requireNamespace("bigmemory")
        DATA <- bigmemory::as.big.matrix(init, type = "double", 
            shared = TRUE)
        DATA_DESC <- bigmemory::describe(DATA)
    }
    else {
        DATA_DESC <- basename(tempfile(".gVariable_"))
    }
    .VALUE <- NULL
    .loadpkg <- TRUE
    function(value) {
        if (shared && .loadpkg) {
            requireNamespace("bigmemory")
            .loadpkg <<- FALSE
        }
        if (shared) {
            DATA <- bigmemory::attach.big.matrix(DATA_DESC)
        }
        if (missing(value)) {
            if (!shared) {
                if (is.null(.VALUE)) 
                  .VALUE <<- init
                .VALUE
            }
            else DATA[]
        }
        else {
            if (!shared) 
                .VALUE <<- value
            else DATA[] <- value
        }
    }
}


nmf_update.lee_R <- function (i, v, x, rescale = TRUE, eps = 10^-9, ...) 
{
    w <- .basis(x)
    h <- .coef(x)
    h <- R_std.euclidean.update.h(v, w, h, eps = eps)
    w <- R_std.euclidean.update.w(v, w, h, eps = eps)
    if (rescale) 
        w <- sweep(w, 2L, colSums(w), "/", check.margin = FALSE)
    .basis(x) <- w
    .coef(x) <- h
    return(x)
}


fit <- function (object, ...) 
standardGeneric("fit")


extractFeatures <- function (object, ...) 
standardGeneric("extractFeatures")


rmatrix <- function (x, ...) 
standardGeneric("rmatrix")


algorithm <- function (object, ...) 
standardGeneric("algorithm")


nmfApply <- function (X, MARGIN, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
{
    if (MARGIN == 1L) 
        apply(basis(X), 1L, FUN, ...)
    else if (MARGIN == 4L) 
        apply(basis(X), 2L, FUN, ...)
    else if (MARGIN == 2L) 
        apply(coef(X), 2L, FUN, ...)
    else if (MARGIN == 5L) 
        apply(coef(X), 1L, FUN, ...)
    else if (MARGIN == 3L) {
        b <- basis(X)
        p <- coef(X)
        sapply(setNames(seq(nbasis(X), basisnames(X))), function(i, 
            ...) FUN(b[, i], p[i, ], ...), simplify = simplify, 
            USE.NAMES = USE.NAMES)
    }
    else stop("invalid argument 'MARGIN' (expected values are: 1-basis rows, 2-coef columns, 3-(basis columns, coef rows), or 4-basis columns or 5-coef rows)")
}


nmfCheck <- function (method = NULL, rank = max(ncol(x)/5, 3), x = NULL, 
    seed = 1234, ...) 
{
    if (isNumber(seed)) {
        os <- RNGseed()
        on.exit(RNGseed(os), add = TRUE)
        set.seed(seed)
        seed <- NULL
    }
    if (is.null(x)) {
        x <- rmatrix(20, 10)
    }
    res <- nmf(x, rank, method, seed = seed, ...)
}


nmfFormals <- function (x, ...) 
{
    UseMethod("nmfFormals")
}


`.__T__canFit:NMF` <- "<environment>"

nmf.stop.iteration <- function (n) 
{
    nmf.debug("Using stopping criterion - Fixed number of iterations: ", 
        n)
    if (!is.numeric(n)) 
        stop("Invalid argument `n`: must be an integer value")
    if (length(n) > 1) 
        warning("NMF::nmf - Argument `n` [", deparse(substitute(n)), 
            "] has length > 1: only using the first element.")
    .max <- n[1]
    function(object, i, y, x, ...) i >= .max
}


.__C__NMFfitX <- new("classRepresentation"
    , slots = structure(list(runtime.all = structure("proc_time", package = "NMF")), .Names = "runtime.all")
    , contains = structure(list(), .Names = character(0))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("NMFfitX", package = "NMF")
    , package = "NMF"
    , subclasses = structure(list(NMFfitX1 = S4_object(), 
    NMFfitXn = S4_object()), .Names = c("NMFfitX1", 
"NMFfitXn"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


NMFStrategy <- function (name, method, ...) 
standardGeneric("NMFStrategy")


nmf_update.ns_R <- function (i, v, x, ...) 
{
    S <- smoothing(x)
    w <- .basis(x)
    h <- .coef(x)
    h <- R_std.divergence.update.h(v, w %*% S, h)
    .coef(x) <- h
    w <- R_std.divergence.update.w(v, w, S %*% h)
    w <- sweep(w, 2L, colSums(w), "/", check.margin = FALSE)
    .basis(x) <- w
    return(x)
}


nmf_update.euclidean_offset.h <- function (v, w, h, offset, eps = 10^-9, copy = TRUE) 
{
    .Call("offset_euclidean_update_H", v, w, h, offset, eps, 
        copy, PACKAGE = "NMF")
}


`.__T__coefmap:NMF` <- "<environment>"

setNMFSeed <- function (..., overwrite = isLoadingNamespace(), verbose = TRUE) 
{
    method <- NMFSeed(...)
    res <- nmfRegister(method, overwrite = overwrite, verbose = verbose)
}


`.__T__fit<-:NMF` <- "<environment>"

runtime <- function (object, ...) 
standardGeneric("runtime")


basismap <- function (object, ...) 
standardGeneric("basismap")


`algorithm<-` <- function (object, ..., value) 
standardGeneric("algorithm<-")


nmfAlgorithm <- function (name = NULL, version = NULL, all = FALSE, ...) 
{
    if (is(name, "NMFStrategy")) 
        return(name)
    if (!is.null(version)) 
        all <- TRUE
    if (!is.null(name) && !all) 
        return(getNMFMethod(name, ...))
    algo <- getNMFMethod(all = TRUE)
    algo <- setNames(algo, sub("^\\.(.+#)?", "", algo))
    if (!all) 
        algo <- algo[!grepl("^\\.", algo)]
    if (!is.null(name)) 
        algo <- algo[grepl(str_c("^", name), names(algo))]
    if (!is.null(version)) {
        type <- match.arg(version, c("R"))
        algo <- Filter(function(x) grepl(str_c("^\\.", version, 
            "#"), x), algo)
    }
    if (is.null(version)) 
        algo <- setNames(algo, NULL)
    algo
}


posneg <- function (...) 
nneg(..., method = "posneg")


icterms <- function (object, ...) 
standardGeneric("icterms")


`.__T__dim:base` <- "<environment>"

is.nmf <- function (x) 
{
    clref <- getClass("NMF", .Force = TRUE, where = .PKG.NAMESPACE)
    is(x, clref)
}


icoef <- function (object, ...) 
{
    i <- 1:nbasis(object)
    if (length(idx <- icterms(object, ...))) 
        i[-idx]
    else i
}


nmfModels <- function (builtin.only = FALSE) 
{
    if (builtin.only) 
        return(.nmf.Models.Builtin)
    models <- names(methods::getClass("NMF")@subclasses)
    models.wraps <- c("NMFfit", names(methods::getClass("NMFfit")@subclasses))
    return(models[!is.element(models, models.wraps)])
}


nmf_update.euclidean_offset.w <- function (v, w, h, offset, eps = 10^-9, copy = TRUE) 
{
    .Call("offset_euclidean_update_W", v, w, h, offset, eps, 
        copy, PACKAGE = "NMF")
}


nmf_update.lee <- function (i, v, x, rescale = TRUE, copy = FALSE, eps = 10^-9, 
    weight = NULL, ...) 
{
    w <- .basis(x)
    h <- .coef(x)
    nb <- nbterms(x)
    nc <- ncterms(x)
    h <- std.euclidean.update.h(v, w, h, eps = eps, nbterms = nb, 
        ncterms = nc, copy = copy)
    if (copy) 
        .coef(x) <- h
    w <- std.euclidean.update.w(v, w, h, eps = eps, weight = weight, 
        nbterms = nb, ncterms = nc, copy = copy)
    if (rescale) {
        w <- sweep(w, 2L, colSums(w), "/", check.margin = FALSE)
    }
    .basis(x) <- w
    return(x)
}


nmf_update.euclidean.w_R <- function (v, w, h, wh = NULL, eps = 10^-9) 
{
    den <- if (is.null(wh)) 
        w %*% tcrossprod(h)
    else {
        wh %*% t(h)
    }
    pmax(w * tcrossprod(v, h), eps)/(den + eps)
}


canFit <- function (x, y, ...) 
standardGeneric("canFit")


.__C__NMFfitX1 <- new("classRepresentation"
    , slots = structure(list(consensus = structure("matrix", package = "methods"), 
    nrun = structure("integer", package = "methods"), rng1 = structure("ANY", package = "methods"), 
    runtime.all = structure("proc_time", package = "NMF"), fit = structure("NMF", package = "NMF"), 
    residuals = structure("numeric", package = "methods"), method = structure("character", package = "methods"), 
    seed = structure("character", package = "methods"), rng = structure("ANY", package = "methods"), 
    distance = structure(".functionSlotNULL", package = "NMF"), 
    parameters = structure("list", package = "methods"), runtime = structure("proc_time", package = "NMF"), 
    options = structure("list", package = "methods"), extra = structure("list", package = "methods"), 
    call = structure("call", package = "methods"), misc = structure("list", package = "methods")), .Names = c("consensus", 
"nrun", "rng1", "runtime.all", "fit", "residuals", "method", 
"seed", "rng", "distance", "parameters", "runtime", "options", 
"extra", "call", "misc"))
    , contains = structure(list(NMFfitX = S4_object(), 
    NMFfit = S4_object(), 
    NMF = S4_object()), .Names = c("NMFfitX", 
"NMFfit", "NMF"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("NMFfitX1", package = "NMF")
    , package = "NMF"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__NMFfit <- new("classRepresentation"
    , slots = structure(list(fit = structure("NMF", package = "NMF"), residuals = structure("numeric", package = "methods"), 
    method = structure("character", package = "methods"), seed = structure("character", package = "methods"), 
    rng = structure("ANY", package = "methods"), distance = structure(".functionSlotNULL", package = "NMF"), 
    parameters = structure("list", package = "methods"), runtime = structure("proc_time", package = "NMF"), 
    options = structure("list", package = "methods"), extra = structure("list", package = "methods"), 
    call = structure("call", package = "methods"), misc = structure("list", package = "methods")), .Names = c("fit", 
"residuals", "method", "seed", "rng", "distance", "parameters", 
"runtime", "options", "extra", "call", "misc"))
    , contains = structure(list(NMF = S4_object()), .Names = "NMF")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    obj <- objective(object)
    if (is.character(obj) && obj == "") 
        return(paste("Slot 'objective' must either be a non-empty character string or a function definition", 
            sep = ""))
    TRUE
}
    , access = list()
    , className = structure("NMFfit", package = "NMF")
    , package = "NMF"
    , subclasses = structure(list(NMFfitX1 = S4_object()), .Names = "NMFfitX1")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


nmfArgs <- function (x) 
{
    args(nmfWrapper(x))
}


entropy <- function (x, y, ...) 
standardGeneric("entropy")


nmfSeed <- function (name = NULL, ...) 
{
    nmfGet("seed", name, ...)
}


.__C__NMF <- new("classRepresentation"
    , slots = structure(list(misc = structure("list", package = "methods")), .Names = "misc")
    , contains = structure(list(), .Names = character(0))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("NMF", package = "NMF")
    , package = "NMF"
    , subclasses = structure(list(NMFstd = S4_object(), 
    NMFfit = S4_object(), 
    NMFOffset = S4_object(), 
    NMFns = S4_object()), .Names = c("NMFstd", 
"NMFfit", "NMFOffset", "NMFns"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__minfit:NMF` <- "<environment>"

`seeding<-` <- function (object, ..., value) 
standardGeneric("seeding<-")


`.__T__.basis<-:NMF` <- "<environment>"

`.__T__NMFSeed:NMF` <- "<environment>"

NMFStop <- function (s, check = TRUE) 
{
    key <- s
    fun <- if (is.integer(key)) 
        nmf.stop.iteration(key)
    else if (is.numeric(key)) 
        nmf.stop.threshold(key)
    else if (is.function(key)) 
        key
    else if (is.character(key)) {
        if (key == "nmf.stop.consensus") 
            key <- "connectivity"
        key2 <- paste("nmf.stop.", key, sep = "")
        e <- pkgmaker::packageEnv()
        sfun <- getFunction(key2, mustFind = FALSE, where = e)
        if (is.null(sfun)) 
            sfun <- getFunction(key, mustFind = FALSE, where = e)
        if (is.null(sfun)) 
            stop("Invalid key ['", key, "']: could not find functions '", 
                key2, "' or '", key, "'")
        sfun
    }
    else if (identical(key, FALSE)) 
        function(strategy, i, target, data, ...) {
            FALSE
        }
    else stop("Invalid key: should be a function, a character string or a single integer/numeric value. See ?NMFStop.")
    if (length(formals(fun)) == 0L) 
        fun <- fun()
    if (check) {
        n.stop <- names(formals(fun))
        if (length(n.stop) < 4) {
            stop("Invalid 'Stop' method - must have at least 4 arguments: ", 
                "NMF strategy object [strategy], ", "current iteration number [i], ", 
                "target matrix [y], ", "current NMF model iterate [x]")
        }
        n.stop <- n.stop[-seq(4)]
        if (!is.element("...", n.stop)) 
            stop("Invalid 'Stop' method: must have argument '...' (even if not used)")
    }
    fun
}


seqtime <- function (object, ...) 
standardGeneric("seqtime")


getDoParHosts <- function (object, ...) 
standardGeneric("getDoParHosts")


coef <- stats::coef # re-exported from stats package

rposneg <- function (object, ...) 
standardGeneric("rposneg")


`.__T__basismap:NMF` <- "<environment>"

`.__T__ibterms:NMF` <- "<environment>"

`.__T__basiscor:NMF` <- "<environment>"

`.__T__[:base` <- colorspace::`.__T__[:base` # re-exported from colorspace package

`.__T__seed:NMF` <- "<environment>"

`.__T__modelname:NMF` <- "<environment>"

metaHeatmap <- function (object, ...) 
standardGeneric("metaHeatmap")


nmf_update.brunet_R <- function (i, v, x, eps = .Machine$double.eps, ...) 
{
    w <- .basis(x)
    h <- .coef(x)
    h <- R_std.divergence.update.h(v, w, h)
    w <- R_std.divergence.update.w(v, w, h)
    if (i%%10 == 0) {
        h[h < eps] <- eps
        w[w < eps] <- eps
    }
    .basis(x) <- w
    .coef(x) <- h
    return(x)
}


connectivity <- function (object, ...) 
standardGeneric("connectivity")


`.__T__$:base` <- Rcpp::`.__T__$:base` # re-exported from Rcpp package

`.__T__niter:NMF` <- "<environment>"

.fcnnls <- function (x, y, verbose = FALSE, pseudo = FALSE, eps = 0) 
{
    if (any(dim(y) == 0L)) {
        stop("Empty target matrix 'y' [", paste(dim(y), collapse = " x "), 
            "]")
    }
    if (any(dim(x) == 0L)) {
        stop("Empty regression variable matrix 'x' [", paste(dim(x), 
            collapse = " x "), "]")
    }
    C <- x
    A <- y
    nObs = nrow(C)
    lVar = ncol(C)
    if (nrow(A) != nObs) 
        stop("C and A have imcompatible sizes")
    pRHS = ncol(A)
    W = matrix(0, lVar, pRHS)
    iter = 0
    maxiter = 3 * lVar
    CtC = crossprod(C)
    CtA = crossprod(C, A)
    K = .cssls(CtC, CtA, pseudo = pseudo)
    Pset = K > 0
    K[!Pset] = 0
    D = K
    Fset = which(colSums(Pset) != lVar)
    oitr = 0
    while (length(Fset) > 0) {
        oitr = oitr + 1
        if (verbose && oitr > 5) 
            cat(sprintf("%d ", oitr))
        K[, Fset] = .cssls(CtC, CtA[, Fset, drop = FALSE], Pset[, 
            Fset, drop = FALSE], pseudo = pseudo)
        Hset = Fset[colSums(K[, Fset, drop = FALSE] < eps) > 
            0]
        if (length(Hset) > 0) {
            nHset = length(Hset)
            alpha = matrix(0, lVar, nHset)
            while (nHset > 0 && (iter < maxiter)) {
                iter = iter + 1
                alpha[, 1:nHset] = Inf
                ij = which(Pset[, Hset, drop = FALSE] & (K[, 
                  Hset, drop = FALSE] < eps), arr.ind = TRUE)
                i = ij[, 1]
                j = ij[, 2]
                if (length(i) == 0) 
                  break
                hIdx = (j - 1) * lVar + i
                negIdx = (Hset[j] - 1) * lVar + i
                alpha[hIdx] = D[negIdx]/(D[negIdx] - K[negIdx])
                alpha.inf <- alpha[, 1:nHset, drop = FALSE]
                minIdx = max.col(-t(alpha.inf))
                alphaMin = alpha.inf[minIdx + (0:(nHset - 1) * 
                  lVar)]
                alpha[, 1:nHset] = matrix(alphaMin, lVar, nHset, 
                  byrow = TRUE)
                D[, Hset] = D[, Hset, drop = FALSE] - alpha[, 
                  1:nHset, drop = FALSE] * (D[, Hset, drop = FALSE] - 
                  K[, Hset, drop = FALSE])
                idx2zero = (Hset - 1) * lVar + minIdx
                D[idx2zero] = 0
                Pset[idx2zero] = FALSE
                K[, Hset] = .cssls(CtC, CtA[, Hset, drop = FALSE], 
                  Pset[, Hset, drop = FALSE], pseudo = pseudo)
                Hset = which(colSums(K < eps) > 0)
                nHset = length(Hset)
            }
        }
        W[, Fset] = CtA[, Fset, drop = FALSE] - CtC %*% K[, Fset, 
            drop = FALSE]
        Jset = which(colSums((ifelse(!(Pset[, Fset, drop = FALSE]), 
            1, 0) * W[, Fset, drop = FALSE]) > eps) == 0)
        Fset = setdiff(Fset, Fset[Jset])
        if (length(Fset) > 0) {
            mxidx = max.col(t(ifelse(!Pset[, Fset, drop = FALSE], 
                1, 0) * W[, Fset, drop = FALSE]))
            Pset[(Fset - 1) * lVar + mxidx] = TRUE
            D[, Fset] = K[, Fset, drop = FALSE]
        }
    }
    list(coef = K, Pset = Pset)
}


`.__T__NMFStrategy:NMF` <- "<environment>"

ts_tempfile <- function (pattern = "file", ..., host = TRUE, pid = TRUE) 
{
    if (host) 
        pattern <- c(pattern, Sys.info()["nodename"])
    if (pid) 
        pattern <- c(pattern, Sys.getpid())
    tempfile(paste(pattern, collapse = "_"), ...)
}


nmf_update.euclidean.h <- function (v, w, h, eps = 10^-9, nbterms = 0L, ncterms = 0L, copy = TRUE) 
{
    .Call("euclidean_update_H", v, w, h, eps, nbterms, ncterms, 
        copy, PACKAGE = "NMF")
}


`.__T__coef<-:NMF` <- "<environment>"

`.__T__basis<-:NMF` <- "<environment>"

nmfReport <- function (x, rank, method, colClass = NULL, ..., output = NULL, 
    template = NULL) 
{
    requireNamespace("knitr")
    if (is.null(template)) 
        template <- system.file("scripts/report.Rmd", package = "NMF")
    x <- force(x)
    rank <- force(rank)
    method <- force(method)
    if (isString(method)) 
        method <- list(method)
    args <- list(...)
    nmfRun <- function(x, rank, method, ...) {
        args <- expand_dots(args)
        str(args)
        do.call(nmf, c(list(x, rank, method), args))
    }
    accuracy <- NA
    res <- NA
    knitr::knit2html(template)
    res <- list(fits = res, accuracy = accuracy)
    saveRDS(res, file = "report_results.rds")
    invisible(res)
}


getNMFMethod <- function (...) 
nmfGet("algorithm", ...)


logs <- function (object, ...) 
standardGeneric("logs")


getDoBackend <- function () 
{
    fe_ns <- asNamespace("foreach")
    fe <- ns_get(".foreachGlobals", fe_ns)
    if (!exists("fun", where = fe, inherits = FALSE)) 
        return(NULL)
    getDoPar <- ns_get("getDoPar", fe_ns)
    c(getDoPar(), info = if (exists("info", where = fe, inherits = FALSE)) {
        get("info", fe, inherits = FALSE)
    } else {
        function(data, item) NULL
    }, cleanup = if (exists("cleanup", where = fe, inherits = FALSE)) {
        get("cleanup", fe, inherits = FALSE)
    })
}


minfit <- function (object, ...) 
standardGeneric("minfit")


`.__T__sparseness:NMF` <- "<environment>"

getNMFSeed <- function (name = NULL, ...) 
{
    nmfGet("seed", name, ...)
}


dispersion <- function (object, ...) 
standardGeneric("dispersion")


`.__T__rnmf:NMF` <- "<environment>"

`basisnames<-` <- function (x, ..., value) 
standardGeneric("basisnames<-")


nmf_update.euclidean.w <- function (v, w, h, eps = 10^-9, nbterms = 0L, ncterms = 0L, weight = NULL, 
    copy = TRUE) 
{
    .Call("euclidean_update_W", v, w, h, eps, weight, nbterms, 
        ncterms, copy, PACKAGE = "NMF")
}


removeNMFMethod <- function (name, ...) 
{
    pkgreg_remove("algorithm", key = name, ...)
}


nmf.equal <- function (x, y, ...) 
standardGeneric("nmf.equal")


`.__T__nrun:NMF` <- "<environment>"

hasTrack <- function (object, niter = NULL) 
{
    if (is.null(niter)) 
        length(slot(object, "residuals")) > 1
    else !is.na(slot(object, "residuals")[as.character(niter)])
}


nterms <- function (object) 
{
    length(ibterms(object)) + length(icterms(object))
}


is.partial.nmf <- function (x) 
!hasCoef(x) || !hasBasis(x)


`.__T__nbasis:NMF` <- "<environment>"

`.__T__consensusmap:NMF` <- "<environment>"

`.__T__logs:NMF` <- "<environment>"

`.coef<-` <- function (object, value) 
standardGeneric(".coef<-")


ibterms <- function (object, ...) 
standardGeneric("ibterms")


nmfModel <- function (rank, target = 0L, ...) 
standardGeneric("nmfModel")


`.__T__algorithm:NMF` <- "<environment>"

cophcor <- function (object, ...) 
standardGeneric("cophcor")


nmfRegisterAlgorithm <- function (name, method, ..., overwrite = isLoadingNamespace(), 
    verbose = TRUE) 
{
    call_const <- match.call(NMFStrategy)
    call_const[[1]] <- as.name("NMFStrategy")
    call_const$verbose <- NULL
    call_const$overwrite <- NULL
    if (missing(method) && !missing(name) && is.character(name) && 
        existsNMFMethod(name)) {
        call_const$method <- name
        call_const$name <- NULL
    }
    e <- parent.frame()
    method <- eval(call_const, envir = e)
    res <- nmfRegister(method, overwrite = overwrite, verbose = verbose)
    wrap <- nmfWrapper(method)
}


.__C__NMFstd <- new("classRepresentation"
    , slots = structure(list(W = structure("matrix", package = "methods"), 
    H = structure("matrix", package = "methods"), bterms = structure("data.frame", package = "methods"), 
    ibterms = structure("integer", package = "methods"), cterms = structure("data.frame", package = "methods"), 
    icterms = structure("integer", package = "methods"), misc = structure("list", package = "methods")), .Names = c("W", 
"H", "bterms", "ibterms", "cterms", "icterms", "misc"))
    , contains = structure(list(NMF = S4_object()), .Names = "NMF")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (ncol(object@W) != nrow(object@H)) {
        return(paste("Dimensions of W and H are not compatible [ncol(W)=", 
            ncol(object@W), "!= nrow(H)=", nrow(object@H), "]"))
    }
    if (!is.empty.nmf(object) && ncol(object@H) && ncol(object@W) > 
        ncol(object@H)) {
        warning(paste("Dimensions of W and H look strange [ncol(W)=", 
            ncol(object@W), "> ncol(H)=", ncol(object@H), "]"))
    }
    return(TRUE)
}
    , access = list()
    , className = structure("NMFstd", package = "NMF")
    , package = "NMF"
    , subclasses = structure(list(NMFOffset = S4_object(), 
    NMFns = S4_object()), .Names = c("NMFOffset", 
"NMFns"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


getRNG1 <- rngtools::getRNG1 # re-exported from rngtools package

`.__T__extractFeatures:NMF` <- "<environment>"

`.__T__.coef:NMF` <- "<environment>"

iterms <- function (object, ...) 
{
    c(ibterms(object), icterms(object))
}


hostfile <- function (pattern = "file", tmpdir = tempdir(), fileext = "", 
    host = TRUE, pid = TRUE) 
{
    if (host) 
        pattern <- c(pattern, Sys.info()["nodename"])
    if (pid) 
        pattern <- c(pattern, Sys.getpid())
    file.path(tmpdir, str_c(paste(pattern, collapse = "."), fileext))
}


`.__T__nmf.equal:NMF` <- "<environment>"

`coef<-` <- function (object, ..., value) 
standardGeneric("coef<-")


`.__T__name<-:NMF` <- "<environment>"

plot <- colorspace::plot # re-exported from colorspace package

nmf_update.offset_R <- function (i, v, x, eps = 10^-9, ...) 
{
    w <- .basis(x)
    h <- .coef(x)
    off <- offset(x)
    if (i == 1 && length(off) == 0) 
        off <- rowMeans(v)
    h <- R_std.euclidean.update.h(v, w, h, wh = w %*% h + off, 
        eps = eps)
    w <- R_std.euclidean.update.w(v, w, h, wh = w %*% h + off, 
        eps = eps)
    x@offset <- off * pmax(rowSums(v), eps)/(rowSums(w %*% h + 
        off) + eps)
    .basis(x) <- w
    .coef(x) <- h
    return(x)
}


seeding <- function (object, ...) 
standardGeneric("seeding")


nneg <- function (object, ...) 
standardGeneric("nneg")


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

fcnnls <- function (x, y, ...) 
standardGeneric("fcnnls")


getDoParNHosts <- function (object) 
{
    if (missing(object)) 
        foreach::getDoParWorkers()
    else {
        length(getDoParHosts(object))
    }
}


`.__T__seeding<-:NMF` <- "<environment>"

`.__T__getDoParHosts:NMF` <- "<environment>"

nmf.stop.threshold <- function (threshold) 
{
    nmf.debug("Using stopping criterion - Stationarity threshold: ", 
        threshold)
    if (!is.numeric(threshold)) 
        stop("Invalid argument `threshold`: must be a numeric value")
    if (length(threshold) > 1) 
        warning("NMF::nmf - Argument `threshold` [", deparse(substitute(threshold)), 
            "] has length > 1: only using the first element.")
    eval(parse(text = paste("function(strategy, i, target, data, stationary.th=", 
        threshold, ", ...){\n\t\tnmf.stop.stationary(strategy, i, target, data, stationary.th=stationary.th, ...)\n\t}")))
}


`.__T__c:base` <- "<environment>"

profplot.default <- function (x, y, scale = c("none", "max", "c1"), match.names = TRUE, 
    legend = TRUE, confint = TRUE, Colv, labels, annotation, 
    ..., add = FALSE) 
{
    res <- list()
    gpar <- list(...)
    if (!missing(y)) {
        xvar <- deparse(substitute(x))
        if (isNMFfit(x)) {
            gpar <- .set.list.defaults(gpar, xlab = paste("NMF model", 
                xvar, "- Method:", algorithm(x)))
            x <- fit(x)
        }
        if (is.nmf(x)) {
            gpar <- .set.list.defaults(gpar, main = "Mixture coefficient profile correlations", 
                xlab = paste("NMF model", xvar))
            x <- coef(x)
            if (is.null(rownames(x))) 
                rownames(x) <- paste("basis", 1:nrow(x), sep = "_")
        }
        else if (is(x, "ExpressionSet")) {
            x <- Biobase::exprs(x)
            gpar <- .set.list.defaults(gpar, main = "Expression profile correlations", 
                xlab = paste("ExpressionSet", xvar))
        }
        else {
            gpar <- .set.list.defaults(gpar, xlab = paste("Matrix ", 
                xvar))
        }
        if (!is.matrix(x)) 
            stop("NMF::profplot - Invalid argument `x`: could not extract mixture coefficient matrix")
        yvar <- deparse(substitute(y))
        if (isNMFfit(y)) {
            gpar <- .set.list.defaults(gpar, ylab = paste("NMF model", 
                yvar, "- Method:", algorithm(y)))
            y <- fit(y)
        }
        if (is.nmf(y)) {
            gpar <- .set.list.defaults(gpar, main = "Mixture coefficient profile correlations", 
                ylab = paste("NMF model", yvar))
            y <- coef(y)
        }
        else if (is(y, "ExpressionSet")) {
            y <- Biobase::exprs(y)
            gpar <- .set.list.defaults(gpar, main = "Expression profile correlations", 
                ylab = paste("ExpressionSet", yvar))
        }
        else {
            gpar <- .set.list.defaults(gpar, ylab = paste("Matrix ", 
                yvar))
        }
        if (!is.matrix(y)) 
            stop("NMF::profplot - Invalid argument `y`: could not extract profile matrix")
        if (match.names && !is.null(rownames(x)) && !is.null(rownames(y))) {
            y.idx <- match(rownames(x), rownames(y), nomatch = 0L)
            x.idx <- which(y.idx != 0L)
            if (length(x.idx) > 0L) {
                res$y.idx <- y.idx[x.idx]
                y <- y[y.idx, , drop = FALSE]
                res$x.idx <- x.idx
                x <- x[x.idx, , drop = FALSE]
            }
        }
        if (missing(scale)) 
            scale <- NULL
        else if (isTRUE(scale)) 
            scale <- "max"
        else if (isFALSE(scale)) 
            scale <- "none"
        scale <- match.arg(scale)
        scales <- "free"
        if (scale == "max") {
            gpar <- .set.list.defaults(gpar, xlim = c(0, 1), 
                ylim = c(0, 1))
            iscale <- (xm <- apply(abs(x), 1L, max)) > 0
            x[iscale, ] <- sweep(x[iscale, , drop = FALSE], 1L, 
                xm[iscale], "/")
            iscale <- (ym <- apply(abs(y), 1L, max)) > 0
            y[iscale, ] <- sweep(y[iscale, , drop = FALSE], 1L, 
                ym[iscale], "/")
            scales <- "fixed"
        }
        else if (scale == "c1") {
            gpar <- .set.list.defaults(gpar, xlim = c(0, 1), 
                ylim = c(0, 1))
            x <- sum2one(x)
            y <- sum2one(y)
        }
        else {
            Mx <- max(x, y)
            mx <- min(x, y)
            Mx <- Mx * 1.25
            mx <- mx * 0.75
            gpar <- .set.list.defaults(gpar, xlim = c(mx, Mx), 
                ylim = c(mx, Mx))
        }
        gpar <- .set.list.defaults(gpar, main = "Profile correlations")
        p <- do.call(corplot, c(list(x = t(x), y = t(y), scales = scales, 
            legend = legend, confint = confint, add = add), gpar))
        p <- expand_list(p, list(idx.map = res))
        return(p)
    }
    xvar <- deparse(substitute(x))
    if (isNMFfit(x)) {
        gpar <- .set.list.defaults(gpar, main = paste("Mixture coefficient profiles\nNMF method:", 
            algorithm(x), "- runs:", nrun(x)))
        x <- fit(x)
    }
    if (is.nmf(x)) {
        gpar <- .set.list.defaults(gpar, main = "Mixture coefficient profiles")
        x <- coef(x)
    }
    else if (is(x, "ExpressionSet")) {
        x <- Biobase::exprs(x)
        gpar <- .set.list.defaults(gpar, main = "Expression profiles")
    }
    if (!is.matrix(x)) 
        stop("NMF::profplot - Invalid argument `x`: could not extract profile matrix")
    if (missing(scale) || !isTRUE(scale)) 
        scale <- FALSE
    if (scale) {
        gpar <- .set.list.defaults(gpar, ylim = c(0, 1))
        x <- sum2one(x)
    }
    if (missing(labels)) {
        labels <- if (!is.null(colnames(x))) 
            colnames(x)
        else 1:ncol(x)
    }
    else if (length(labels) != ncol(x)) {
        labels <- rep(labels, length.out = ncol(x))
    }
    if (!missing(annotation) && length(annotation) != ncol(x)) 
        stop("NMF::profplot - Invalid argument `annotation`:: length should be equal to the number of columns in ", 
            xvar, " [=", ncol(x), "]")
    if (!missing(Colv) && !is_NA(Colv)) {
        ord <- if (length(Colv) == 1) {
            if (!is.numeric(Colv) || abs(Colv) > nrow(x)) 
                stop("NMF::profplot - Invalid singel argument `Colv`: should be an integer between -nrow(x) and nrow(", 
                  xvar, ") (i.e. [[-", nrow(x), ",", nrow(x), 
                  "]])")
            order(x[abs(Colv), ], decreasing = Colv < 0)
        }
        else {
            if (length(Colv) != ncol(x)) 
                stop("NMF::profplot - Invalid length for argument `Colv`: should be of length ncol(", 
                  xvar, ") [=", nrow(x), "]")
            if (is.integer(Colv) && length(setdiff(Colv, 1:ncol(x))) == 
                0) 
                Colv
            else order(Colv)
        }
        if (missing(annotation) && is.factor(Colv)) 
            annotation <- Colv
        x <- x[, ord]
        labels <- labels[ord]
        if (!missing(annotation) && !is_NA(annotation)) 
            annotation <- annotation[ord]
    }
    cols <- rainbow(nrow(x))
    gpar <- .set.list.defaults(gpar, xlab = "Samples", ylab = "Mixture coefficient value", 
        main = "Profile plot", type = "o", lty = 1, pch = 19, 
        cex = 0.8, col = cols)
    do.call(matplot, c(list(x = t(x)), gpar, xaxt = "n"))
    if (!isFALSE(legend)) {
        if (isTRUE(legend)) 
            legend <- "topleft"
        leg <- rownames(x)
        if (is.null(leg)) 
            leg <- paste("basis", 1:nrow(x), sep = "_")
        legend(legend, legend = leg, col = gpar$col, lwd = 1, 
            pch = gpar$pch)
    }
    px <- 1:ncol(x)
    axis(1, at = px, labels = FALSE)
    vps <- baseViewports()
    pushViewport(vps$inner, vps$figure, vps$plot)
    on.exit(popViewport(3), add = TRUE)
    voffset <- 1
    if (!missing(annotation) && !is_NA(annotation) && is.factor(annotation)) {
        grid.rect(x = unit(px, "native"), unit(-voffset, "lines"), 
            width = unit(1, "native"), height = unit(1, "lines"), 
            gp = gpar(fill = alphacol(rainbow(nlevels(annotation))[annotation], 
                50), col = "gray"))
        voffset <- voffset + 1
    }
    if (!is_NA(labels)) {
        adj <- if (is.character(labels) && max(nchar(labels)) >= 
            7) 
            list(just = "right", rot = 45)
        else list(just = "center", rot = 0)
        grid.text(labels, x = unit(px, "native"), y = unit(-voffset, 
            "lines"), just = adj$just, rot = adj$rot)
        voffset <- voffset + 1
    }
    invisible(nrow(x))
}


`.__T__fit:NMF` <- "<environment>"

`.__T__fitted:stats` <- "<environment>"

match_atrack <- function (x, data = NULL) 
{
    if (is.null(data) || length(x) == 0L) 
        return(x)
    refnames <- anames(data, default.margin = 1L)
    reflength <- alength(data, default.margin = 1L)
    if (is.null(reflength)) 
        return(x)
    if (is.character(x) && is.null(names(x)) && !is.null(refnames)) {
        if (any(x %in% refnames)) {
            vmessage("match_atrack - Annotation track [", str_out(x, 
                3, use.names = TRUE), "] has some values matching data names: converting into a logical using values as names.")
            x <- setNames(rep(TRUE, length(x)), x)
        }
    }
    .hasNames <- FALSE
    if (!is.null(names(x)) && !is.null(refnames)) {
        inref <- names(x) %in% refnames
        if (!all(inref)) {
            vmessage("match_atrack - Annotation track [", str_out(x, 
                3, use.names = TRUE), "] has partially matching names: subsetting track to match data")
            x <- x[inref]
            if (length(x) == 0L) 
                vmessage("match_atrack - Subset annotation track is empty")
        }
        else vmessage("match_atrack - Annotation track [", str_out(x, 
            3, use.names = TRUE), "] using names as identifiers")
        .hasNames <- TRUE
        if (anyDuplicated(names(x))) {
            dups <- duplicated(names(x))
            vmessage("match_atrack - Annotation track [", str_out(x, 
                3, use.names = TRUE), "]: removing duplicated names [", 
                str_out(x[dups], 3, use.names = TRUE), "]")
            x <- x[!dups]
        }
    }
    lx <- length(x)
    if (lx > reflength) {
        stop("match_atrack - Invalid annotation track [", str_out(x, 
            3, use.names = TRUE), "]: more elements [", lx, "] than rows in data [", 
            reflength, "].")
    }
    if (lx == reflength) {
        res <- if (!.hasNames) 
            x
        else x[match(refnames, names(x))]
        return(res)
    }
    res <- if (is.factor(x)) 
        setNames(factor(c(x, rep(NA, reflength - lx)), levels = c(levels(x), 
            NA)), refnames)
    else setNames(c(x, rep(NA, reflength - lx)), refnames)
    res[1:lx] <- NA
    if (!.hasNames) {
        if (is.integer(x)) 
            res[x] <- x
        else res[1:lx] <- x
    }
    else {
        res[match(names(x), refnames)] <- x
    }
    res
}


`niter<-` <- function (object, ..., value) 
standardGeneric("niter<-")


existsNMFMethod <- function (name, exact = TRUE) 
{
    !is.null(getNMFMethod(name, error = FALSE, exact = exact))
}


str_args <- function (x, exdent = 10L) 
{
    s <- capture.output(print(args(x)))
    paste(str_trim(s[-length(s)]), collapse = str_c("\n", paste(rep(" ", 
        exdent), collapse = "")))
}


.__C__NMFns <- new("classRepresentation"
    , slots = structure(list(theta = structure("numeric", package = "methods"), 
    W = structure("matrix", package = "methods"), H = structure("matrix", package = "methods"), 
    bterms = structure("data.frame", package = "methods"), ibterms = structure("integer", package = "methods"), 
    cterms = structure("data.frame", package = "methods"), icterms = structure("integer", package = "methods"), 
    misc = structure("list", package = "methods")), .Names = c("theta", 
"W", "H", "bterms", "ibterms", "cterms", "icterms", "misc"))
    , contains = structure(list(NMFstd = S4_object(), 
    NMF = S4_object()), .Names = c("NMFstd", 
"NMF"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (object@theta < 0 || object@theta > 1) 
        return(paste("Invalid value for theta (", object@theta, 
            "): must be between 0 and 1", sep = ""))
    TRUE
}
    , access = list()
    , className = structure("NMFns", package = "NMF")
    , package = "NMF"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


smoothing <- function (x, theta = x@theta, ...) 
{
    if (theta < 0 || theta > 1) 
        stop("Invalid smoothing parameter theta [", theta, "]: theta must be susch that 0 <= theta <=1")
    diag(1 - theta, nbasis(x)) + theta/nbasis(x)
}


nmf_update.ns <- function (i, v, x, copy = FALSE, ...) 
{
    S <- smoothing(x)
    w <- .basis(x)
    h <- .coef(x)
    h <- std.divergence.update.h(v, w %*% S, h, copy = copy)
    if (copy) 
        .coef(x) <- h
    w <- std.divergence.update.w(v, w, S %*% h, copy = copy)
    w <- sweep(w, 2L, colSums(w), "/", check.margin = FALSE)
    .basis(x) <- w
    return(x)
}


nmf_update.KL.w <- function (v, w, h, nbterms = 0L, ncterms = 0L, copy = TRUE) 
{
    .Call("divergence_update_W", v, w, h, nbterms, ncterms, copy, 
        PACKAGE = "NMF")
}


`.__T__objective<-:NMF` <- "<environment>"

profcor <- function (x, y, ...) 
standardGeneric("profcor")


`.__T__rposneg:NMF` <- "<environment>"

existsNMFSeed <- function (name, exact = TRUE) 
{
    res <- !is.null(getNMFSeed(name, error = FALSE, exact = exact))
    return(res)
}


`.__T__basisnames<-:NMF` <- "<environment>"

`.__T__consensushc:NMF` <- "<environment>"

nmf.resetOptions <- function (..., ALL = FALSE) 
{
    defaults <- .OPTOBJ$.defaults
    if (ALL) {
        .OPTOBJ$.options <- NULL
    }
    if (length(list(...)) > 0L) {
        onames <- c(...)
        if (!is.character(onames)) 
            stop("character strings expected for resetting option names")
        defaults <- defaults[names(defaults) %in% onames]
        if (length(not_default <- onames[!onames %in% names(defaults)])) {
            .OPTOBJ$.options[not_default] <- NULL
        }
    }
    if (length(defaults)) {
        .OPTOBJ$options(defaults)
    }
}


`.__T__$<-:base` <- "<environment>"

`.__T__dimnames:base` <- "<environment>"

`.__T__name:NMF` <- "<environment>"

which.best <- function (object, FUN = deviance, ...) 
{
    if (length(object) == 0) 
        return(integer())
    e <- sapply(object, FUN, ...)
    which.min(e)
}


`.__T__basisnames:NMF` <- "<environment>"

nmf_update.brunet <- function (i, v, x, copy = FALSE, eps = .Machine$double.eps, ...) 
{
    w <- .basis(x)
    h <- .coef(x)
    nb <- nbterms(x)
    nc <- ncterms(x)
    h <- std.divergence.update.h(v, w, h, nbterms = nb, ncterms = nc, 
        copy = copy)
    w <- std.divergence.update.w(v, w, h, nbterms = nb, ncterms = nc, 
        copy = copy)
    if (i%%10 == 0) {
        h <- pmax.inplace(h, eps, icterms(x))
        w <- pmax.inplace(w, eps, ibterms(x))
    }
    if (copy) {
        .basis(x) <- w
        .coef(x) <- h
    }
    return(x)
}


`name<-` <- function (object, ..., value) 
standardGeneric("name<-")


nmf_update.KL.h <- function (v, w, h, nbterms = 0L, ncterms = 0L, copy = TRUE) 
{
    .Call("divergence_update_H", v, w, h, nbterms, ncterms, copy, 
        PACKAGE = "NMF")
}


.__C__NMFOffset <- new("classRepresentation"
    , slots = structure(list(offset = structure("numeric", package = "methods"), 
    W = structure("matrix", package = "methods"), H = structure("matrix", package = "methods"), 
    bterms = structure("data.frame", package = "methods"), ibterms = structure("integer", package = "methods"), 
    cterms = structure("data.frame", package = "methods"), icterms = structure("integer", package = "methods"), 
    misc = structure("list", package = "methods")), .Names = c("offset", 
"W", "H", "bterms", "ibterms", "cterms", "icterms", "misc"))
    , contains = structure(list(NMFstd = S4_object(), 
    NMF = S4_object()), .Names = c("NMFstd", 
"NMF"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("NMFOffset", package = "NMF")
    , package = "NMF"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


trackError <- function (object, value, niter, force = FALSE) 
{
    track <- run.options(object, "error.track")
    track.interval <- run.options(object, "track.interval")
    if (force || (track && niter%%track.interval == 0)) {
        last.iter <- names(residuals(object))
        duplicate <- if (!is.null(last.iter)) 
            niter == last.iter
        else FALSE
        if (!duplicate) {
            iter <- if (niter >= 0) 
                niter
            residuals(object, niter = iter) <- value
        }
    }
    object
}


is.mixed <- function (object) 
{
    return(slot(object, "mixed"))
}


`.__T__basis:NMF` <- "<environment>"

`.__T__nmfModel:NMF` <- "<environment>"

nmf.stop.connectivity <- function (object, i, y, x, stopconv = 40, check.interval = 10, 
    ...) 
{
    if (i == 0L) {
        p <- ncol(x)
        .consold <<- matrix(0, p, p)
        .inc <<- 0
        return(FALSE)
    }
    if (i%%check.interval != 0) 
        return(FALSE)
    h <- coef(x, all = FALSE)
    index <- apply(h, 2, function(x) which.max(x))
    cons <- outer(index, index, function(x, y) ifelse(x == y, 
        1, 0))
    changes <- cons != .consold
    if (!any(changes)) 
        .inc <<- .inc + 1
    else {
        .consold <<- cons
        .inc <<- 0
    }
    if (.inc > stopconv) 
        return(TRUE)
    FALSE
}


`.__T__.DollarNames:utils` <- Rcpp::`.__T__.DollarNames:utils` # re-exported from Rcpp package

coefmap <- function (object, ...) 
standardGeneric("coefmap")


`.__T__connectivity:NMF` <- "<environment>"

`.__T__plot:graphics` <- colorspace::`.__T__plot:graphics` # re-exported from colorspace package

`objective<-` <- function (object, ..., value) 
standardGeneric("objective<-")


rss <- function (object, ...) 
standardGeneric("rss")


nmfDistance <- function (method = c("", "KL", "euclidean")) 
{
    if (is.null(method)) 
        return(NULL)
    if (is.character(method)) {
        errMeth <- try(method <- match.arg(method), silent = TRUE)
        if (inherits(errMeth, "try-error")) {
            if (is.character(method)) {
                errFun <- try(fun <- match.fun(method), silent = TRUE)
                if (inherits(errFun, "try-error")) 
                  stop("Could not find distance measure '", method, 
                    "':\n\t- not a predefined measures -> ", 
                    errMeth, "\t- not a function -> ", errFun)
            }
            else fun <- method
            if (!is.function(fun)) 
                stop("Invalid distance measure: should be a character string or a valid function definition")
        }
        else {
            fun <- switch(method, euclidean = function(x, y, 
                ...) {
                .rss(y, fitted(x))/2
            }, KL = function(x, y, ...) {
                .KL(y, fitted(x))
            })
        }
    }
    else if (is.function(method)) 
        fun <- method
    else stop("Invalid distance measure: should be a character string or a valid function definition")
    fun
}


`.__T__residuals:stats` <- "<environment>"

register <- function (x, ...) 
{
    UseMethod("register", x)
}


niter <- function (object, ...) 
standardGeneric("niter")


setNMFMethod <- function (name, method, ..., overwrite = isLoadingNamespace(), 
    verbose = TRUE) 
{
    call_const <- match.call(NMFStrategy)
    call_const[[1]] <- as.name("NMFStrategy")
    call_const$verbose <- NULL
    call_const$overwrite <- NULL
    if (missing(method) && !missing(name) && is.character(name) && 
        existsNMFMethod(name)) {
        call_const$method <- name
        call_const$name <- NULL
    }
    e <- parent.frame()
    method <- eval(call_const, envir = e)
    res <- nmfRegister(method, overwrite = overwrite, verbose = verbose)
    wrap <- nmfWrapper(method)
}


`.__T__dispersion:NMF` <- "<environment>"

nmfObject <- function (object, verbose = FALSE) 
{
    objectUpdater(object, verbose = verbose)
}


`.__T__objective:NMF` <- "<environment>"

`.__T__runtime:NMF` <- "<environment>"

ncterms <- function (object) 
{
    length(icterms(object))
}


modelname <- function (object, ...) 
standardGeneric("modelname")


ts_eval <- function (mutex = synchronicity::boost.mutex(), verbose = FALSE) 
{
    requireNamespace("bigmemory")
    requireNamespace("synchronicity")
    .MUTEX_DESC <- if (is(mutex, "boost.mutex")) 
        synchronicity::describe(mutex)
    else mutex
    loadpkg <- TRUE
    function(expr, envir = parent.frame()) {
        if (loadpkg) {
            requireNamespace("bigmemory")
            requireNamespace("synchronicity")
            loadpkg <<- FALSE
        }
        MUTEX <- synchronicity::attach.mutex(.MUTEX_DESC)
        synchronicity::lock(MUTEX)
        if (verbose) 
            message("#", Sys.getpid(), " - START mutex: ", .MUTEX_DESC@description$shared.name)
        ERROR <- "### <Error in mutex expression> ###\n"
        on.exit({
            if (verbose) {
                message(ERROR, "#", Sys.getpid(), " - END mutex: ", 
                  .MUTEX_DESC@description$shared.name)
            }
            synchronicity::unlock(MUTEX)
        })
        eval(expr, envir = envir)
        ERROR <- NULL
    }
}


`.__T__evar:NMF` <- "<environment>"

`.__T__consensus:NMF` <- "<environment>"

coefficients <- stats::coefficients # re-exported from stats package

predict <- stats::predict # re-exported from stats package

consensusmap <- function (object, ...) 
standardGeneric("consensusmap")


`.__T__runtime.all:NMF` <- "<environment>"

consensushc <- function (object, ...) 
standardGeneric("consensushc")


objective <- function (object, ...) 
standardGeneric("objective")


`.__T__loadings:stats` <- "<environment>"

`.__T__show:methods` <- Rcpp::`.__T__show:methods` # re-exported from Rcpp package

basiscor <- function (x, y, ...) 
standardGeneric("basiscor")


deviance <- stats::deviance # re-exported from stats package

`.__T__cophcor:NMF` <- "<environment>"

nmf_update.KL.w_R <- function (v, w, h, wh = NULL) 
{
    if (is.null(wh)) 
        wh <- w %*% h
    sweep(w * tcrossprod(v/wh, h), 2L, rowSums(h), "/", check.margin = FALSE)
}


`fit<-` <- function (object, value) 
standardGeneric("fit<-")


`.__T__purity:NMF` <- "<environment>"

.__C__NMFList <- new("classRepresentation"
    , slots = structure(list(.Data = structure("list", package = "methods"), 
    runtime = structure("proc_time", package = "NMF"), names = structure("character", package = "methods")), .Names = c(".Data", 
"runtime", "names"))
    , contains = structure(list(namedList = S4_object(), 
    list = S4_object(), 
    vector = S4_object()), .Names = c("namedList", 
"list", "vector"))
    , virtual = FALSE
    , prototype = new("list"
)
    , validity = function (object) 
{
    ok <- isNMFfit(object)
    if (!is.logical(ok)) 
        return("Could not validate elements in list: input is probably a complex structure of lists.")
    pb <- which(!ok)
    if (length(pb)) {
        return(paste("invalid class for element(s)", str_out(i), 
            "of input list [all elements must be fitted NMF models]"))
    }
}
    , access = list()
    , className = structure("NMFList", package = "NMF")
    , package = "NMF"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__deviance:stats` <- "<environment>"

cterms <- function (object) 
{
    object@cterms
}


`basis<-` <- function (object, ..., value) 
standardGeneric("basis<-")


`.__T__seqtime:NMF` <- "<environment>"

`.__T__rss:NMF` <- "<environment>"

run <- function (object, y, x, ...) 
standardGeneric("run")


NMFSeed <- function (key, method, ...) 
standardGeneric("NMFSeed")


`.__T__ForeachBackend:NMF` <- "<environment>"

`.__T__fcnnls:NMF` <- "<environment>"

`.__T__.getRNG:rngtools` <- rngtools::`.__T__.getRNG:rngtools` # re-exported from rngtools package

scoef <- function (object, ...) 
standardGeneric("scoef")


`.__T__metaHeatmap:NMF` <- "<environment>"

`.__T__nmf:NMF` <- "<environment>"

.basis <- function (object, ...) 
standardGeneric(".basis")


misc <- function (object, ...) 
{
    if (!isS4(object) && is.list(object)) 
        object[["misc"]]
    else attr(object, "misc")
}


show <- methods::show # re-exported from methods package

residuals <- stats::residuals # re-exported from stats package

`.__T__.basis:NMF` <- "<environment>"

neq.constraints.inplace <- function (x, constraints, ratio = NULL, value = NULL, copy = FALSE) 
{
    if (copy) 
        x <- clone(x)
    .Call("ptr_neq_constraints", x, constraints, ratio, value, 
        PACKAGE = "NMF")
}


nbasis <- function (x, ...) 
standardGeneric("nbasis")




## Package Data

esGolub <- NMF::esGolub		## Golub ExpressionSet



## Package Info

.skeleton_package_title = "Algorithms and Framework for Nonnegative Matrix Factorization(NMF)"

.skeleton_package_version = "0.20.6"

.skeleton_package_depends = "methods,utils,pkgmaker,registry,rngtools,cluster"

.skeleton_package_imports = "graphics,stats,stringr,digest,grid,grDevices,gridBase,colorspace,RColorBrewer,foreach,doParallel,ggplot2,reshape2"


## Internal

.skeleton_version = 5


## EOF