##
## Exported symobls in package `stats4`
##

## Exported package methods

nobs <- stats::nobs # re-exported from stats package

`.__T__BIC:stats` <- "<environment>"

`.__T__profile:stats` <- "<environment>"

profile <- stats::profile # re-exported from stats package

`.__T__logLik:stats` <- "<environment>"

`.__T__show:methods` <- methods::`.__T__show:methods` # re-exported from methods package

`.__T__summary:base` <- "<environment>"

BIC <- stats::BIC # re-exported from stats package

update <- stats::update # re-exported from stats package

.__C__mle <- new("classRepresentation"
    , slots = structure(list(call = structure("language", package = "methods"), 
    coef = structure("numeric", package = "methods"), fullcoef = structure("numeric", package = "methods"), 
    vcov = structure("matrix", package = "methods"), min = structure("numeric", package = "methods"), 
    details = structure("list", package = "methods"), minuslogl = structure("function", package = "methods"), 
    nobs = structure("integer", package = "methods"), method = structure("character", package = "methods")), .Names = c("call", 
"coef", "fullcoef", "vcov", "min", "details", "minuslogl", "nobs", 
"method"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("mle", package = "stats4")
    , package = "stats4"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__coef:stats` <- "<environment>"

mle <- function (minuslogl, start = formals(minuslogl), method = "BFGS", 
    fixed = list(), nobs, ...) 
{
    call <- match.call()
    n <- names(fixed)
    fullcoef <- formals(minuslogl)
    if (any(!n %in% names(fullcoef))) 
        stop("some named arguments in 'fixed' are not arguments to the supplied log-likelihood")
    fullcoef[n] <- fixed
    if (!missing(start) && (!is.list(start) || is.null(names(start)))) 
        stop("'start' must be a named list")
    start[n] <- NULL
    start <- sapply(start, eval.parent)
    nm <- names(start)
    oo <- match(nm, names(fullcoef))
    if (anyNA(oo)) 
        stop("some named arguments in 'start' are not arguments to the supplied log-likelihood")
    start <- start[order(oo)]
    nm <- names(start)
    f <- function(p) {
        l <- as.list(p)
        names(l) <- nm
        l[n] <- fixed
        do.call("minuslogl", l)
    }
    oout <- if (length(start)) 
        optim(start, f, method = method, hessian = TRUE, ...)
    else list(par = numeric(), value = f(start))
    coef <- oout$par
    vcov <- if (length(coef)) 
        solve(oout$hessian)
    else matrix(numeric(), 0L, 0L)
    min <- oout$value
    fullcoef[nm] <- coef
    new("mle", call = call, coef = coef, fullcoef = unlist(fullcoef), 
        vcov = vcov, min = min, details = oout, minuslogl = minuslogl, 
        nobs = if (missing(nobs)) 
            NA_integer_
        else nobs, method = method)
}


plot <- graphics::plot # re-exported from graphics package

summary <- function (object, ...) 
standardGeneric("summary")


.__C__profile.mle <- new("classRepresentation"
    , slots = structure(list(profile = structure("list", package = "methods"), 
    summary = structure("summary.mle", package = "stats4")), .Names = c("profile", 
"summary"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("profile.mle", package = "stats4")
    , package = "stats4"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__update:stats` <- "<environment>"

vcov <- stats::vcov # re-exported from stats package

`.__T__AIC:stats` <- "<environment>"

`.__T__confint:stats` <- "<environment>"

AIC <- stats::AIC # re-exported from stats package

`.__T__plot:graphics` <- "<environment>"

`.__T__vcov:stats` <- "<environment>"

logLik <- stats::logLik # re-exported from stats package

.__C__summary.mle <- new("classRepresentation"
    , slots = structure(list(call = structure("language", package = "methods"), 
    coef = structure("matrix", package = "methods"), m2logL = structure("numeric", package = "methods")), .Names = c("call", 
"coef", "m2logL"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("summary.mle", package = "stats4")
    , package = "stats4"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


confint <- stats::confint # re-exported from stats package

coef <- stats::coef # re-exported from stats package

show <- methods::show # re-exported from methods package

`.__T__nobs:stats` <- "<environment>"



## Package Data

# none


## Package Info

.skeleton_package_title = "Statistical Functions using S4 Classes"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF