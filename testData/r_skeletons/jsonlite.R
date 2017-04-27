##
## Exported symobls in package `jsonlite`
##

## Exported package methods

flatten <- function (x, recursive = TRUE) 
{
    stopifnot(is.data.frame(x))
    nr <- nrow(x)
    dfcolumns <- vapply(x, is.data.frame, logical(1))
    if (!any(dfcolumns)) {
        return(x)
    }
    x <- if (recursive) {
        c(x[!dfcolumns], do.call(c, lapply(x[dfcolumns], flatten)))
    }
    else {
        c(x[!dfcolumns], do.call(c, x[dfcolumns]))
    }
    class(x) <- "data.frame"
    row.names(x) <- if (!nr) 
        character(0)
    else 1:nr
    x
}


stream_out <- function (x, con = stdout(), pagesize = 500, verbose = TRUE, 
    ...) 
{
    if (!is(con, "connection")) {
        stop("Argument 'con' must be a connection.")
    }
    if (!isOpen(con, "w")) {
        if (verbose) 
            message("opening ", is(con), " output connection.")
        open(con, "wb")
        on.exit({
            if (verbose) message("closing ", is(con), " output connection.")
            close(con)
        })
    }
    apply_by_pages(x, stream_out_page, pagesize = pagesize, con = con, 
        verbose = verbose, ...)
}


rbind.pages <- function (pages) 
{
    loadpkg("plyr")
    stopifnot(is.list(pages))
    if (!length(pages)) {
        return(data.frame())
    }
    pages <- Filter(function(x) {
        !is.null(x)
    }, pages)
    stopifnot(all(vapply(pages, is.data.frame, logical(1))))
    dfdf <- lapply(pages, vapply, is.data.frame, logical(1))
    dfnames <- unique(names(which(unlist(dfdf))))
    if (!length(dfnames)) {
        return(plyr::rbind.fill(pages))
    }
    subpages <- lapply(dfnames, function(colname) {
        rbind.pages(lapply(pages, function(df) {
            if (!is.null(df[[colname]])) 
                df[[colname]]
            else as.data.frame(matrix(nrow = nrow(df), ncol = 0))
        }))
    })
    pages <- lapply(pages, function(df) {
        issubdf <- vapply(df, is.data.frame, logical(1))
        if (any(issubdf)) 
            df[issubdf] <- rep(NA, nrow(df))
        df
    })
    outdf <- plyr::rbind.fill(pages)
    for (i in seq_along(subpages)) {
        outdf[[dfnames[i]]] <- subpages[[i]]
    }
    outdf
}


base64_enc <- function (input) 
{
    if (is.character(input)) {
        input <- charToRaw(paste(input, collapse = "\n"))
    }
    stopifnot(is.raw(input))
    .Call(R_base64_encode, input)
}


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

read_json <- function (path, simplifyVector = FALSE, ...) 
{
    fromJSON(file(path), simplifyVector = simplifyVector, ...)
}


fromJSON <- function (txt, simplifyVector = TRUE, simplifyDataFrame = simplifyVector, 
    simplifyMatrix = simplifyVector, flatten = FALSE, ...) 
{
    if (!is.character(txt) && !inherits(txt, "connection")) {
        stop("Argument 'txt' must be a JSON string, URL or file.")
    }
    if (is.character(txt) && length(txt) == 1 && nchar(txt, type = "bytes") < 
        10000) {
        if (grepl("^https?://", txt, useBytes = TRUE)) {
            loadpkg("curl")
            h <- curl::new_handle(useragent = paste("jsonlite /", 
                R.version.string))
            curl::handle_setheaders(h, Accept = "application/json, text/*, */*")
            txt <- curl::curl(txt, handle = h)
        }
        else if (file.exists(txt)) {
            txt <- file(txt)
        }
    }
    fromJSON_string(txt = txt, simplifyVector = simplifyVector, 
        simplifyDataFrame = simplifyDataFrame, simplifyMatrix = simplifyMatrix, 
        flatten = flatten, ...)
}


validate <- function (txt) 
{
    stopifnot(is.character(txt))
    txt <- paste(txt, collapse = "\n")
    .Call(R_validate, as.character(txt))
}


prettify <- function (txt, indent = 4) 
{
    txt <- paste(txt, collapse = "\n")
    reformat(txt, TRUE, indent_string = paste(rep(" ", as.integer(indent)), 
        collapse = ""))
}


unserializeJSON <- function (txt) 
{
    unpack(parseJSON(txt))
}


base64_dec <- function (input) 
{
    if (is.character(input)) {
        input <- charToRaw(paste(input, collapse = "\n"))
    }
    stopifnot(is.raw(input))
    .Call(R_base64_decode, input)
}


toJSON <- function (x, dataframe = c("rows", "columns", "values"), matrix = c("rowmajor", 
    "columnmajor"), Date = c("ISO8601", "epoch"), POSIXt = c("string", 
    "ISO8601", "epoch", "mongo"), factor = c("string", "integer"), 
    complex = c("string", "list"), raw = c("base64", "hex", "mongo"), 
    null = c("list", "null"), na = c("null", "string"), auto_unbox = FALSE, 
    digits = 4, pretty = FALSE, force = FALSE, ...) 
{
    dataframe <- match.arg(dataframe)
    matrix <- match.arg(matrix)
    Date <- match.arg(Date)
    POSIXt <- match.arg(POSIXt)
    factor <- match.arg(factor)
    complex <- match.arg(complex)
    raw <- match.arg(raw)
    null <- match.arg(null)
    x <- force(x)
    if (!missing(na)) {
        na <- match.arg(na)
    }
    else {
        na <- NULL
    }
    indent <- if (isTRUE(pretty)) 
        0L
    else NA_integer_
    ans <- asJSON(x, dataframe = dataframe, Date = Date, POSIXt = POSIXt, 
        factor = factor, complex = complex, raw = raw, matrix = matrix, 
        auto_unbox = auto_unbox, digits = digits, na = na, null = null, 
        force = force, indent = indent, ...)
    if (is.numeric(pretty)) {
        prettify(ans, pretty)
    }
    else {
        class(ans) <- "json"
        return(ans)
    }
}


stream_in <- function (con, handler = NULL, pagesize = 500, verbose = TRUE, 
    ...) 
{
    if (!is(con, "connection")) {
        stop("Argument 'con' must be a connection.")
    }
    count <- 0
    cb <- if (is.null(handler)) {
        out <- new.env()
        function(x) {
            if (length(x)) {
                count <<- count + length(x)
                out[[as.character(count)]] <<- x
            }
        }
    }
    else {
        if (verbose) 
            message("using a custom handler function.")
        function(x) {
            handler(post_process(x, ...))
            count <<- count + length(x)
        }
    }
    if (!isOpen(con, "r")) {
        if (verbose) 
            message("opening ", is(con), " input connection.")
        open(con, "rb")
        on.exit({
            if (verbose) message("closing ", is(con), " input connection.")
            close(con)
        })
    }
    repeat {
        page <- readLines(con, n = pagesize, encoding = "UTF-8")
        if (length(page)) {
            cleanpage <- Filter(nchar, page)
            cb(lapply(cleanpage, parseJSON))
            if (verbose) 
                cat("\r Found", count, "records...")
        }
        if (length(page) < pagesize) 
            break
    }
    if (is.null(handler)) {
        if (verbose) 
            cat("\r Imported", count, "records. Simplifying...\n")
        out <- as.list(out, sorted = FALSE)
        post_process(unlist(out[order(as.numeric(names(out)))], 
            FALSE, FALSE), ...)
    }
    else {
        invisible()
    }
}


serializeJSON <- function (x, digits = 8, pretty = FALSE) 
{
    is(x)
    asJSON(pack(x), digits = digits, indent = if (isTRUE(pretty)) 
        0L
    else NA_integer_)
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

minify <- function (txt) 
{
    txt <- paste(txt, collapse = "\n")
    reformat(txt, FALSE)
}


write_json <- function (x, path, ...) 
{
    json <- jsonlite::toJSON(x, ...)
    writeLines(json, path)
}


unbox <- function (x) 
{
    if (is.null(x)) {
        return(x)
    }
    if (is.data.frame(x)) {
        if (nrow(x) == 1) {
            return(as.scalar(x))
        }
        else {
            stop("Tried to unbox dataframe with ", nrow(x), " rows.")
        }
    }
    if (!is.vector(unclass(x)) || !is.atomic(x) || length(dim(x)) > 
        1) {
        stop("Only atomic vectors of length 1 or data frames with 1 row can be unboxed.")
    }
    if (identical(length(x), 1L)) {
        return(as.scalar(x))
    }
    else {
        stop("Tried to unbox a vector of length ", length(x))
    }
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package



## Package Data

# none


## Package Info

.skeleton_package_title = "A Robust, High Performance JSON Parser and Generator for R"

.skeleton_package_version = "1.2"

.skeleton_package_depends = "methods"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF