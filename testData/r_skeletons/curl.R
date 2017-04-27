##
## Exported symobls in package `curl`
##

## Exported package methods

curl_download <- function (url, destfile, quiet = TRUE, mode = "wb", handle = new_handle()) 
{
    destfile <- normalizePath(destfile, mustWork = FALSE)
    nonblocking <- isTRUE(getOption("curl_interrupt", TRUE))
    .Call(R_download_curl, url, destfile, quiet, mode, handle, 
        nonblocking)
    invisible(destfile)
}


curl_version <- function () 
{
    .Call(R_curl_version)
}


curl_fetch_stream <- function (url, fun, handle = new_handle()) 
{
    con <- curl(url, handle = handle)
    open(con, "rb", blocking = FALSE)
    on.exit(close(con))
    while (isIncomplete(con)) {
        buf <- readBin(con, raw(), 8192L)
        if (length(buf)) 
            fun(buf)
    }
    handle_data(handle)
}


handle_setform <- function (handle, ..., .list = list()) 
{
    stopifnot(inherits(handle, "curl_handle"))
    form <- c(list(...), .list)
    for (i in seq_along(form)) {
        val <- form[[i]]
        if (!is.character(val) && !is.raw(val) && !inherits(val, 
            "form_file")) {
            stop("Insupported value type for form field '", names(form[i]), 
                "'.")
        }
    }
    .Call(R_handle_setform, handle, form)
    invisible(handle)
}


form_file <- function (path, type = NULL) 
{
    path <- normalizePath(path[1], mustWork = TRUE)
    if (!is.null(type)) {
        stopifnot(is.character(type))
    }
    out <- list(path = path, type = type)
    class(out) <- "form_file"
    out
}


handle_reset <- function (handle) 
{
    stopifnot(inherits(handle, "curl_handle"))
    .Call(R_handle_reset, handle)
    invisible(handle)
}


parse_date <- function (datestring) 
{
    out <- .Call(R_curl_getdate, datestring)
    class(out) <- c("POSIXct", "POSIXt")
    out
}


curl_fetch_memory <- function (url, handle = new_handle()) 
{
    nonblocking <- isTRUE(getOption("curl_interrupt", TRUE))
    output <- .Call(R_curl_fetch_memory, url, handle, nonblocking)
    res <- handle_data(handle)
    res$content <- output
    res
}


handle_setheaders <- function (handle, ..., .list = list()) 
{
    stopifnot(inherits(handle, "curl_handle"))
    opts <- c(list(...), .list)
    if (!all(vapply(opts, is.character, logical(1)))) {
        stop("All headers must be strings.")
    }
    names <- names(opts)
    values <- as.character(unlist(opts))
    vec <- paste0(names, ": ", values)
    .Call(R_handle_setheaders, handle, vec)
    invisible(handle)
}


multi_run <- function (timeout = Inf, pool = NULL) 
{
    if (is.null(pool)) 
        pool <- multi_default()
    stopifnot(is.numeric(timeout))
    stopifnot(inherits(pool, "curl_multi"))
    .Call(R_multi_run, pool, timeout)
}


curl_options <- function (filter = "") 
{
    m <- grep(filter, fixed = TRUE, names(option_table))
    option_table[m]
}


new_pool <- function (total_con = 100, host_con = 6, multiplex = TRUE) 
{
    pool <- .Call(R_multi_new)
    multi_set(pool = pool, total_con = total_con, host_con = host_con, 
        multiplex = multiplex)
}


has_internet <- function () 
{
    !is.null(nslookup("r-project.org", error = FALSE))
}


curl <- function (url = "http://httpbin.org/get", open = "", handle = new_handle()) 
{
    .Call(R_curl_connection, url, open, handle, TRUE)
}


parse_headers <- function (txt, multiple = FALSE) 
{
    if (is.raw(txt)) {
        txt <- rawToChar(txt)
    }
    stopifnot(is.character(txt))
    if (length(txt) > 1) {
        txt <- paste(txt, collapse = "\n")
    }
    sets <- strsplit(txt, "\\r\\n\\r\\n|\\n\\n|\\r\\r")[[1]]
    headers <- strsplit(sets, "\\r\\n|\\n|\\r")
    if (multiple) {
        headers
    }
    else {
        headers[[length(headers)]]
    }
}


curl_fetch_multi <- function (url, done = NULL, fail = NULL, pool = NULL, handle = new_handle()) 
{
    handle_setopt(handle, url = url)
    multi_add(handle = handle, done = done, fail = fail, pool = pool)
    invisible(handle)
}


curl_fetch_disk <- function (url, path, handle = new_handle()) 
{
    nonblocking <- isTRUE(getOption("curl_interrupt", TRUE))
    path <- normalizePath(path, mustWork = FALSE)
    output <- .Call(R_curl_fetch_disk, url, handle, path, "wb", 
        nonblocking)
    res <- handle_data(handle)
    res$content <- output
    res
}


nslookup <- function (host, error = TRUE) 
{
    stopifnot(is.character(host))
    if (grepl("://", host, fixed = TRUE)) 
        stop("This looks like a URL, not a hostname")
    out <- .Call(R_nslookup, host[1])
    if (isTRUE(error) && is.null(out)) 
        stop("Unable to resolve host: ", host)
    out
}


ie_proxy_info <- function () 
{
    .Call(R_proxy_info)
}


multi_cancel <- function (handle) 
{
    stopifnot(inherits(handle, "curl_handle"))
    .Call(R_multi_cancel, handle)
}


new_handle <- function (...) 
{
    h <- .Call(R_new_handle)
    handle_setopt(h, ...)
    h
}


multi_add <- function (handle, done = NULL, fail = NULL, pool = NULL) 
{
    if (is.null(pool)) 
        pool <- multi_default()
    stopifnot(inherits(handle, "curl_handle"))
    stopifnot(inherits(pool, "curl_multi"))
    stopifnot(is.null(done) || is.function(done))
    stopifnot(is.null(fail) || is.function(fail))
    .Call(R_multi_add, handle, done, fail, pool)
}


multi_set <- function (total_con = 50, host_con = 6, multiplex = TRUE, pool = NULL) 
{
    if (is.null(pool)) 
        pool <- multi_default()
    stopifnot(inherits(pool, "curl_multi"))
    stopifnot(is.numeric(total_con))
    stopifnot(is.numeric(host_con))
    stopifnot(is.logical(multiplex))
    .Call(R_multi_setopt, pool, total_con, host_con, multiplex)
}


handle_cookies <- function (handle) 
{
    stopifnot(inherits(handle, "curl_handle"))
    cookies <- .Call(R_get_handle_cookies, handle)
    df <- if (length(cookies)) {
        values <- lapply(strsplit(cookies, split = "\t"), `[`, 
            1:7)
        as.data.frame(do.call(rbind, values), stringsAsFactors = FALSE)
    }
    else {
        as.data.frame(matrix(ncol = 7, nrow = 0))
    }
    names(df) <- c("domain", "flag", "path", "secure", "expiration", 
        "name", "value")
    df$flag <- as.logical(df$flag)
    df$secure <- as.logical(df$secure)
    expires <- as.numeric(df$expiration)
    expires[expires == 0] <- Inf
    class(expires) = c("POSIXct", "POSIXt")
    df$expiration <- expires
    df
}


handle_data <- function (handle) 
{
    stopifnot(inherits(handle, "curl_handle"))
    out <- .Call(R_get_handle_response, handle)
    out$content = NULL
    out
}


handle_setopt <- function (handle, ..., .list = list()) 
{
    stopifnot(inherits(handle, "curl_handle"))
    values <- c(list(...), .list)
    opt_names <- tolower(names(values))
    keys <- as.integer(curl_options()[opt_names])
    na_keys <- is.na(keys)
    if (any(na_keys)) {
        bad_opts <- opt_names[na_keys]
        stop("Unknown option", ifelse(length(bad_opts) > 1, "s: ", 
            ": "), paste(bad_opts, collapse = ", "))
    }
    stopifnot(length(keys) == length(values))
    .Call(R_handle_setopt, handle, keys, values)
    invisible(handle)
}


curl_escape <- function (url) 
{
    .Call(R_curl_escape, enc2utf8(as.character(url)), FALSE)
}


ie_get_proxy_for_url <- function (target_url = "http://www.google.com") 
{
    stopifnot(is.character(target_url))
    info <- ie_proxy_info()
    if (length(info$Proxy)) {
        if (isTRUE(grepl("<local>", info$ProxyBypass, fixed = TRUE)) && 
            isTRUE(grepl("(://)[^./]+/", paste0(target_url, "/")))) {
            return(NULL)
        }
        else {
            return(info$Proxy)
        }
    }
    if (isTRUE(info$AutoDetect) || length(info$AutoConfigUrl)) {
        out <- .Call(R_get_proxy_for_url, target_url, info$AutoDetect, 
            info$AutoConfigUrl)
        if (isTRUE(out$HasProxy)) {
            return(out$Proxy)
        }
    }
    return(NULL)
}


curl_unescape <- function (url) 
{
    .Call(R_curl_escape, enc2utf8(as.character(url)), TRUE)
}


multi_list <- function (pool = NULL) 
{
    if (is.null(pool)) 
        pool <- multi_default()
    stopifnot(inherits(pool, "curl_multi"))
    as.list(.Call(R_multi_list, pool))
}




## Package Data

curl_symbols <- curl::curl_symbols		## List curl version and options.



## Package Info

.skeleton_package_title = "A Modern and Flexible Web Client for R"

.skeleton_package_version = "2.3"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF