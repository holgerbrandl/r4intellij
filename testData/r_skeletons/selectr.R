##
## Exported symobls in package `selectr`
##

## Exported package methods

querySelectorAll <- function (doc, selector, ns = NULL, ...) 
{
    tryLoadNamespaces()
    UseMethod("querySelectorAll", doc)
}


`.__T__$:base` <- Rcpp::`.__T__$:base` # re-exported from Rcpp package

`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

querySelectorAllNS <- function (doc, selector, ns, prefix = "descendant-or-self::", 
    ...) 
{
    tryLoadNamespaces()
    UseMethod("querySelectorAllNS", doc)
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

querySelector <- function (doc, selector, ns = NULL, ...) 
{
    tryLoadNamespaces()
    UseMethod("querySelector", doc)
}


querySelectorNS <- function (doc, selector, ns, prefix = "descendant-or-self::", 
    ...) 
{
    tryLoadNamespaces()
    UseMethod("querySelectorNS", doc)
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

css_to_xpath <- function (selector, prefix = "descendant-or-self::", translator = "generic") 
{
    if (missing(selector) || is.null(selector)) 
        stop("A valid selector (character vector) must be provided.")
    if (!is.character(selector)) 
        stop("The 'selector' argument must be a character vector")
    if (!is.character(prefix)) 
        stop("The 'prefix' argument must be a character vector")
    if (!is.character(translator)) 
        stop("The 'translator' argument must be a character vector")
    if (anyNA(selector)) {
        warning("NA values were found in the 'selector' argument, they have been removed")
        selector <- selector[!is.na(selector)]
    }
    if (anyNA(prefix)) {
        warning("NA values were found in the 'prefix' argument, they have been removed")
        prefix <- prefix[!is.na(prefix)]
    }
    if (anyNA(translator)) {
        warning("NA values were found in the 'translator' argument, they have been removed")
        translator <- translator[!is.na(translator)]
    }
    zeroLengthArgs <- character(0)
    if (!length(selector)) 
        zeroLengthArgs <- c(zeroLengthArgs, "selector")
    if (!length(prefix)) 
        zeroLengthArgs <- c(zeroLengthArgs, "prefix")
    if (!length(translator)) 
        zeroLengthArgs <- c(zeroLengthArgs, "translator")
    if (length(zeroLengthArgs)) {
        plural <- if (length(zeroLengthArgs) > 1) 
            "s"
        else ""
        stop(sprintf("Zero length character vector found for the following argument%s: %s", 
            plural, paste0(zeroLengthArgs, collapse = ",")))
    }
    translator <- sapply(translator, function(tran) {
        match.arg(tolower(tran), c("generic", "html", "xhtml"))
    })
    maxArgLength <- max(length(selector), length(prefix), length(translator))
    selector <- rep(selector, length.out = maxArgLength)
    prefix <- rep(prefix, length.out = maxArgLength)
    translator <- rep(translator, length.out = maxArgLength)
    results <- character(maxArgLength)
    for (i in seq_len(maxArgLength)) {
        sel <- selector[i]
        pref <- prefix[i]
        trans <- translator[i]
        tran <- if (trans == "html") {
            HTMLTranslator$new()
        }
        else if (trans == "xhtml") {
            HTMLTranslator$new(xhtml = TRUE)
        }
        else {
            GenericTranslator$new()
        }
        results[i] <- tran$css_to_xpath(sel, pref)
    }
    as.character(results)
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package



## Package Data

# none


## Package Info

.skeleton_package_title = "Translate CSS Selectors to XPath Expressions"

.skeleton_package_version = "0.3-1"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods,stringr"


## Internal

.skeleton_version = 5


## EOF