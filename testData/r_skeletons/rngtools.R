##
## Exported symobls in package `rngtools`
##

## Exported package methods

RNGdigest <- function (object = getRNG()) 
{
    x <- object
    object <- getRNG(x)
    if (is.null(object)) {
        warning("Found no embedded RNG data in object [", class(x), 
            "]: returned NULL digest [", digest(NULL), "].")
        return(digest(NULL))
    }
    digest(object)
}


RNGseq <- function (n, seed = NULL, ..., simplify = TRUE, version = 2) 
{
    library(parallel)
    if (n <= 0) 
        stop("NMF::createStream - invalid value for 'n' [positive value expected]")
    if (!is.null(seed)) 
        seed <- getRNG(seed, num.ok = TRUE) %||% seed
    if (is.matrix(seed)) 
        seed <- lapply(seq(ncol(seed)), function(i) seed[, i])
    if (is.list(seed)) {
        if (length(seed) > n) {
            warning("Reference seed sequence is longer than the required number of seed: only using the ", 
                n, " first seeds.")
            seed <- seed[1:n]
        }
        else if (length(seed) < n) 
            stop("Reference seed sequence is shorter [", length(seed), 
                "] than the required number of seed [", n, "].")
        res <- lapply(seed, as.integer)
    }
    else {
        orng <- RNGseed()
        .s <- RNGseq_seed(seed, ..., version = version)
        res <- lapply(1:n, function(i) {
            if (i == 1) 
                .s
            else .s <<- nextRNGStream(.s)
        })
        if (is.null(seed) && RNGkind()[1L] == "L'Ecuyer-CMRG" && 
            version >= 2) {
            RNGseed(c(orng[1L], nextRNGStream(.s)[2:7]))
        }
    }
    if (n == 1 && simplify) 
        res[[1]]
    else res
}


`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

RNGstr <- function (object, n = 7L, ...) 
{
    if (missing(object)) {
        rp <- RNGprovider()
        rs <- getRNG()
        if (rp == "base" || length(rs) > 1L) 
            object <- rs
        else return("Unknown")
    }
    seed <- getRNG(object, ...)
    if (is.null(seed)) 
        "NULL"
    else if (is.numeric(seed)) {
        if (length(seed) > n) {
            paste(str_out(seed, 3L), str_c("[", digest(seed), 
                "]"))
        }
        else {
            str_out(seed, Inf)
        }
    }
    else paste(class(seed), " [", digest(seed), "]", sep = "")
}


`.__T__getRNG1:rngtools` <- "<environment>"

.getRNG <- function (object, ...) 
standardGeneric(".getRNG")


nextRNG <- function (object, ..., ndraw = 0L) 
{
    orseed <- RNGseed()
    on.exit(RNGseed(orseed))
    if (missing(object)) {
        runif(1)
        return(getRNG())
    }
    rng <- .getRNGattribute(object)
    if (!is.null(rng)) {
        on.exit()
        return(nextRNG(rng, ...))
    }
    if (!is.numeric(object)) 
        stop("Invalid seed: expecting a numeric seed.")
    .setRNG(object, ...)
    if (ndraw > 0) {
        if (!isNumber(ndraw)) 
            stop("Invalid value in argument `ndraw`: single numeric value expected.")
        runif(ndraw)
    }
    res <- RNGseed()
    res
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

RNGtype <- function (object, ..., provider = FALSE) 
{
    res <- if (missing(object)) {
        RNGkind()
    }
    else {
        old <- RNGseed()
        rng <- getRNG(object, ...)
        if (is.null(rng)) {
            warning("Could not find embedded RNG data in ", deparse(substitute(object)), 
                ".", " Returned current type.")
        }
        on.exit(RNGseed(old))
        setRNG(rng)
        RNGkind()
    }
    if (provider) {
        prov <- RNGprovider(res)
        res <- c(res, prov)
    }
    res
}


`.__T__.setRNG:rngtools` <- "<environment>"

getRNG1 <- function (object, ...) 
standardGeneric("getRNG1")


setRNG <- function (object, ..., verbose = FALSE, check = TRUE) 
{
    if (is.null(object)) 
        return()
    rng <- getRNG(object, ...)
    if (!is.null(rng) && !identical(rng, object)) 
        return(setRNG(rng, ...))
    orseed <- getRNG()
    on.exit({
        message("Restoring RNG settings probably due to an error in setRNG")
        RNGseed(orseed)
    })
    tryCatch(.setRNG(object, ...), warning = function(err) {
        if (check && testRversion("> 3.0.1") && grepl("\\.Random\\.seed.* is not a valid", 
            err$message)) {
            stop("setRNG - Invalid RNG kind [", str_out(object), 
                "]: ", err$message, ".", call. = FALSE)
        }
        else {
            warning(err)
        }
    })
    on.exit()
    if (verbose) 
        showRNG()
    invisible(orseed)
}


rng.equal <- function (x, y) 
{
    if (missing(y)) 
        y <- getRNG()
    identical(RNGdigest(x), RNGdigest(y))
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

hasRNG <- function (object) 
{
    !is.null(.getRNGattribute(object))
}


RNGseq_seed <- function (seed = NULL, normal.kind = NULL, verbose = FALSE, version = 2) 
{
    orng <- RNGseed()
    on.exit({
        RNGseed(orng)
        if (verbose) message("# Restoring RNG: ", paste(RNGkind(), 
            collapse = " - "), " [", .collapse(orng), "]")
    })
    rkind_not_CMRG <- RNGkind()[1L] != "L'Ecuyer-CMRG"
    if (verbose) 
        message("# Original RNG: ", paste(RNGkind(), collapse = " - "), 
            " [", .collapse(orng), "]")
    if (is.numeric(seed)) {
        if (length(seed) == 1L) {
            if (verbose) 
                message("# Generate RNGstream random seed from ", 
                  seed, " ... ", appendLF = FALSE)
            if (version < 2 || rkind_not_CMRG) {
                set.seed(seed)
                RNGkind(kind = "L'Ecuyer-CMRG", normal.kind = normal.kind)
            }
            else {
                set.seed(seed, kind = "L'Ecuyer-CMRG", normal.kind = normal.kind)
            }
            if (verbose) 
                message("OK")
        }
        else if (length(seed) == 6L) {
            if (verbose) 
                message("# Directly use 6-long seed: ", paste(seed, 
                  collapse = ", "), " ... ", appendLF = FALSE)
            RNGkind("L'Ecuyer-CMRG", normal.kind = normal.kind)
            s <- RNGseed()
            s[2:7] <- as.integer(seed)
            RNGseed(s)
            if (verbose) 
                message("OK")
        }
        else if (length(seed) == 7L) {
            if (seed[1]%%100 != 7L) 
                stop("RNGseq_seed - Invalid 7-long numeric seed: RNG code should be '7', i.e. of type \"L'Ecuyer-CMRG\"")
            if (verbose) 
                message("# Directly use CMRG seed: ", paste(seed, 
                  collapse = ", "), " ... ", appendLF = FALSE)
            RNGseed(seed)
            if (verbose) 
                message("OK")
        }
        else stop("RNGseq_seed - Invalid numeric seed: should be a numeric of length 1, 6 or 7")
    }
    else if (is.null(seed)) {
        if (rkind_not_CMRG) {
            runif(1)
            orng1 <- RNGseed()
            RNGseed(orng)
            orng <- orng1
            if (verbose) 
                message("# Generate random RNGstream seed: ", 
                  appendLF = FALSE)
            RNGkind(kind = "L'Ecuyer", normal.kind = normal.kind)
            if (verbose) 
                message("OK")
        }
        else {
            if (version < 2) {
                on.exit()
                s <- nextRNGStream(orng)
                if (verbose) 
                  message("# Use next active RNG stream: ", .collapse(s[2:7]))
                RNGseed(s)
            }
            else {
                if (!is.null(normal.kind)) 
                  RNGkind(normal.kind = normal.kind)
                if (verbose) 
                  message("# Use current active RNG stream: ", 
                    .collapse(RNGseed()[2:7]))
            }
        }
    }
    else stop("RNGseq_seed - Invalid seed value: should be a numeric or NULL")
    s <- RNGseed()
    if (verbose) 
        message("# Seed RNGkind is: ", paste(RNGkind(), collapse = " - "), 
            " [", .collapse(s), "]")
    s
}


rng1.equal <- function (x, y) 
{
    if (missing(y)) 
        y <- getRNG()
    rng.equal(getRNG1(x), getRNG1(y))
}


`.__T__.getRNG:rngtools` <- "<environment>"

getRNG <- function (object, ..., num.ok = FALSE, extract = TRUE, recursive = TRUE) 
{
    if (missing(object) || is.null(object)) 
        return(.getRNG())
    if (extract && !is.null(rng <- .getRNGattribute(object))) {
        if (recursive && hasRNG(rng)) 
            getRNG(rng, ..., num.ok = num.ok)
        else rng
    }
    else if (isNumber(object)) {
        if (num.ok && isReal(object)) 
            object
        else if (isInteger(object)) 
            object
        else nextRNG(object, ...)
    }
    else .getRNG(object, ...)
}


checkRNG <- function (x, y = getRNG(), ...) 
{
    requireRUnit()
    checkTrue(rng.equal(x, y), ...)
}


RNGseed <- function (seed) 
{
    res <- if (missing(seed)) {
        if (exists(".Random.seed", where = .GlobalEnv)) 
            get(".Random.seed", envir = .GlobalEnv)
    }
    else if (is.null(seed)) {
        if (exists(".Random.seed", where = .GlobalEnv)) 
            rm(".Random.seed", envir = .GlobalEnv)
    }
    else {
        old <- RNGseed()
        assign(".Random.seed", seed, envir = .GlobalEnv)
        old
    }
    invisible(res)
}


.setRNG <- function (object, ...) 
standardGeneric(".setRNG")


RNGrecovery <- function () 
{
    s <- as.integer(c(401, 0, 0))
    assign(".Random.seed", s, envir = .GlobalEnv)
    RNGkind("default", "default")
}


RNGinfo <- function (object = getRNG(), ...) 
{
    kind <- RNGtype(object, ...)
    n <- c("kind", "normal", "provider")
    as.list(setNames(kind, n[1:length(kind)]))
}


showRNG <- function (object = getRNG(), indent = "#", ...) 
{
    tryCatch(suppressMessages(info <- RNGtype(object, ...)), 
        error = function(e) {
            stop("Could not show RNG due to error: ", conditionMessage(e))
        })
    cat(indent, "RNG kind: ", paste(info[1:2], collapse = " / "), 
        if (length(info) > 2L) 
            paste("[", info[3L], "]", sep = ""), "\n")
    cat(indent, "RNG state:", RNGstr(object), "\n")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Utility functions for working with Random Number Generators"

.skeleton_package_version = "1.2.4"

.skeleton_package_depends = "methods,pkgmaker"

.skeleton_package_imports = "stringr,digest"


## Internal

.skeleton_version = 5


## EOF