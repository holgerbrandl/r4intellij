##
## Exported symobls in package `iterators`
##

## Exported package methods

iter <- function (obj, ...) 
{
    UseMethod("iter")
}


ireadLines <- function (con, n = 1, ...) 
{
    if (!is.numeric(n) || length(n) != 1 || n < 1) 
        stop("n must be a numeric value >= 1")
    if (is.character(con)) {
        con <- file(con, open = "r")
        doClose <- TRUE
    }
    else {
        doClose <- FALSE
    }
    nextEl <- function() {
        if (is.null(con)) 
            stop("StopIteration", call. = FALSE)
        r <- readLines(con, n = n, ...)
        if (length(r) == 0) {
            if (doClose) 
                close(con)
            con <<- NULL
            stop("StopIteration", call. = FALSE)
        }
        r
    }
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


idiv <- function (n, ..., chunks, chunkSize) 
{
    if (!is.numeric(n) || length(n) != 1) 
        stop("n must be a numeric value")
    if (length(list(...)) > 0) 
        stop("chunks and chunkSize must be specified as named arguments")
    if ((missing(chunkSize) && missing(chunks)) || (!missing(chunkSize) && 
        !missing(chunks))) 
        stop("either chunks or chunkSize must be specified, but not both")
    if (missing(chunks)) {
        if (!is.numeric(chunkSize) || length(chunkSize) != 1 || 
            chunkSize < 1) 
            stop("chunkSize must be a numeric value >= 1")
        chunks <- ceiling(n/chunkSize)
    }
    nextEl <- function() {
        if (chunks <= 0 || n <= 0) 
            stop("StopIteration", call. = FALSE)
        m <- ceiling(n/chunks)
        n <<- n - m
        chunks <<- chunks - 1
        m
    }
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


irunif <- function (..., count) 
{
    if (!missing(count) && (!is.numeric(count) || length(count) != 
        1)) 
        stop("count must be a numeric value")
    m <- as.call(c(as.name(FUN), list(...)))
    fbody <- if (missing(count)) {
        m
    }
    else {
        substitute({
            if (count > 0) {
                count <<- count - 1L
                REPLACETHIS
            } else {
                stop("StopIteration", call. = FALSE)
            }
        }, list(REPLACETHIS = m))
    }
    nextEl <- function() NULL
    body(nextEl) <- fbody
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


irpois <- function (..., count) 
{
    if (!missing(count) && (!is.numeric(count) || length(count) != 
        1)) 
        stop("count must be a numeric value")
    m <- as.call(c(as.name(FUN), list(...)))
    fbody <- if (missing(count)) {
        m
    }
    else {
        substitute({
            if (count > 0) {
                count <<- count - 1L
                REPLACETHIS
            } else {
                stop("StopIteration", call. = FALSE)
            }
        }, list(REPLACETHIS = m))
    }
    nextEl <- function() NULL
    body(nextEl) <- fbody
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


iapply <- function (X, MARGIN) 
{
    xit <- icountn(dim(X)[MARGIN])
    nextEl <- function() {
        i <- nextElem(xit)
        j <- rep("", length(dim(X)))
        j[MARGIN] <- as.character(i)
        s <- paste("X[", paste(j, collapse = ","), "]", sep = "")
        x <- parse(text = s)
        eval(x)
    }
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


icountn <- function (vn) 
{
    n <- length(vn)
    if (n == 0) 
        stop("illegal zero length vector")
    icar <- icount(vn[n])
    if (n > 1) {
        icdr <- icountn(vn[-n])
        hasVal <- FALSE
        nextVal <- NULL
    }
    nextEl <- if (n == 1) {
        function() nextElem(icar)
    }
    else {
        function() {
            repeat {
                if (!hasVal) {
                  nextVal <<- nextElem(icar)
                  hasVal <<- TRUE
                }
                tryCatch({
                  return(c(nextElem(icdr), nextVal))
                }, error = function(e) {
                  if (identical(conditionMessage(e), "StopIteration")) {
                    icdr <<- icountn(vn[-n])
                    hasVal <<- FALSE
                  }
                  else {
                    stop(e)
                  }
                })
            }
        }
    }
    structure(list(nextElem = nextEl), class = c("abstractiter", 
        "iter"))
}


irnorm <- function (..., count) 
{
    if (!missing(count) && (!is.numeric(count) || length(count) != 
        1)) 
        stop("count must be a numeric value")
    m <- as.call(c(as.name(FUN), list(...)))
    fbody <- if (missing(count)) {
        m
    }
    else {
        substitute({
            if (count > 0) {
                count <<- count - 1L
                REPLACETHIS
            } else {
                stop("StopIteration", call. = FALSE)
            }
        }, list(REPLACETHIS = m))
    }
    nextEl <- function() NULL
    body(nextEl) <- fbody
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


irnbinom <- function (..., count) 
{
    if (!missing(count) && (!is.numeric(count) || length(count) != 
        1)) 
        stop("count must be a numeric value")
    m <- as.call(c(as.name(FUN), list(...)))
    fbody <- if (missing(count)) {
        m
    }
    else {
        substitute({
            if (count > 0) {
                count <<- count - 1L
                REPLACETHIS
            } else {
                stop("StopIteration", call. = FALSE)
            }
        }, list(REPLACETHIS = m))
    }
    nextEl <- function() NULL
    body(nextEl) <- fbody
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


isplit <- function (x, f, drop = FALSE, ...) 
{
    UseMethod("isplit")
}


irbinom <- function (..., count) 
{
    if (!missing(count) && (!is.numeric(count) || length(count) != 
        1)) 
        stop("count must be a numeric value")
    m <- as.call(c(as.name(FUN), list(...)))
    fbody <- if (missing(count)) {
        m
    }
    else {
        substitute({
            if (count > 0) {
                count <<- count - 1L
                REPLACETHIS
            } else {
                stop("StopIteration", call. = FALSE)
            }
        }, list(REPLACETHIS = m))
    }
    nextEl <- function() NULL
    body(nextEl) <- fbody
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


nextElem <- function (obj, ...) 
{
    UseMethod("nextElem")
}


iread.table <- function (file, ..., verbose = FALSE) 
{
    args <- list(...)
    argnames <- names(args)
    if (!all(c("header", "row.names") %in% argnames)) 
        stop("both header and row.names must be specified in this implementation")
    nrows <- if ("nrows" %in% argnames) 
        args$nrows
    else 1
    row.names <- args$row.names
    if (!is.numeric(nrows) || length(nrows) != 1 || nrows < 1) 
        stop("nrows must be a numeric value >= 1")
    if (is.character(file)) {
        file <- file(file, open = "r")
        doClose <- TRUE
    }
    else {
        doClose <- FALSE
    }
    m <- as.call(c(as.name("read.table"), file = "", list(...)))
    m$file <- file
    m$nrows <- nrows
    env <- sys.frame(sys.nframe())
    rnlen <- length(row.names)
    gotrownames <- is.character(row.names) && rnlen > 1
    first.time <- TRUE
    irow <- 1
    errmsg <- NULL
    nextEl <- function() {
        if (!is.null(errmsg)) 
            stop(paste("iterator failed previously:", errmsg), 
                call. = FALSE)
        if (is.null(file)) 
            stop("StopIteration", call. = FALSE)
        if (gotrownames) {
            rem <- rnlen - irow + 1
            nrows <<- min(nrows, rem)
            if (nrows > 1) 
                m$row.names <<- row.names[seq(irow, length = nrows)]
            else m["row.names"] <<- list(NULL)
            m$nrows <<- nrows
        }
        r <- tryCatch({
            if (nrows > 0) {
                if (verbose) 
                  print(m)
                eval(m, env)
            }
            else {
                NULL
            }
        }, error = function(e) {
            if (!identical(conditionMessage(e), "no lines available in input")) {
                if (doClose) 
                  close(file)
                file <<- NULL
                errmsg <<- conditionMessage(e)
                stop(e)
            }
            NULL
        })
        if (first.time) {
            first.time <<- FALSE
            m$header <<- FALSE
            m$skip <<- 0
            nms <- names(r)
            if (is.numeric(row.names)) {
                nms <- if (row.names == 1) 
                  c("", nms)
                else if (row.names >= length(nms)) 
                  c(nms, "")
                else c(nms[1:(row.names - 1)], "", nms[row.names:length(nms)])
            }
            m$col.names <<- nms
        }
        if (is.null(r) || nrow(r) == 0) {
            if (doClose) 
                close(file)
            file <<- NULL
            stop("StopIteration", call. = FALSE)
        }
        if (gotrownames) {
            if (nrows == 1) 
                rownames(r) <- row.names[irow]
            irow <<- irow + nrows
        }
        r
    }
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}


icount <- function (count) 
{
    if (missing(count)) 
        count <- NULL
    else if (!is.numeric(count) || length(count) != 1) 
        stop("count must be a numeric value")
    i <- 0L
    nextEl <- function() {
        if (is.null(count) || i < count) 
            (i <<- i + 1L)
        else stop("StopIteration", call. = FALSE)
    }
    it <- list(nextElem = nextEl)
    class(it) <- c("abstractiter", "iter")
    it
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Provides Iterator Construct for R"

.skeleton_package_version = "1.0.8"

.skeleton_package_depends = "utils"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF