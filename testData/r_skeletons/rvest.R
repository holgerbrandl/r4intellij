##
## Exported symobls in package `rvest`
##

## Exported package methods

xml <- function (x, ..., encoding = "") 
{
    .Deprecated("read_xml")
    xml2::read_xml(x, ..., encoding = encoding)
}


html_table <- function (x, header = NA, trim = TRUE, fill = FALSE, dec = ".") 
{
    UseMethod("html_table")
}


xml_node <- xml2::xml_node # re-exported from xml2 package

guess_encoding <- httr::guess_encoding # re-exported from httr package

xml_tag <- function (x) 
{
    .Deprecated("html_name")
    xml2::xml_name(x)
}


back <- function (x) 
{
    stopifnot(is.session(x))
    if (length(x$back) == 0) {
        stop("Can't go back any further", call. = FALSE)
    }
    url <- x$back[[1]]
    x$back <- x$back[-1]
    x$forward <- c(x$url, x$forward)
    request_GET(x, url)
}


is.session <- function (x) 
inherits(x, "session")


html_session <- function (url, ...) 
{
    session <- structure(list(handle = httr::handle(url), config = c(..., 
        httr::config(autoreferer = 1L)), url = NULL, back = character(), 
        forward = character(), response = NULL, html = new.env(parent = emptyenv(), 
            hash = FALSE)), class = "session")
    request_GET(session, url)
}


repair_encoding <- function (x, from = NULL) 
{
    if (!requireNamespace("stringi", quietly = TRUE)) {
        stop("stringi package required for encoding operations")
    }
    if (is.null(from)) {
        best_guess <- guess_encoding(x)[1, , drop = FALSE]
        from <- best_guess$encoding
        conf <- best_guess$confidence * 100
        if (conf < 50) {
            stop("No guess has more than 50% confidence", call. = FALSE)
        }
        message("Best guess: ", from, " (", conf, "% confident)")
    }
    stringi::stri_conv(x, from = from)
}


html_form <- function (x) 
UseMethod("html_form")


html_children <- function (x) 
{
    xml2::xml_children(x)
}


html_attr <- function (x, name, default = NA_character_) 
{
    xml2::xml_attr(x, name, default = default)
}


html_text <- function (x, trim = FALSE) 
{
    xml2::xml_text(x, trim = trim)
}


follow_link <- function (x, i, css, xpath, ...) 
{
    stopifnot(is.session(x))
    if (!missing(i)) {
        stopifnot(length(i) == 1)
        if (is.numeric(i)) {
            a <- html_nodes(x, "a")[[i]]
        }
        else if (is.character(i)) {
            links <- html_nodes(x, "a")
            text <- html_text(links)
            match <- grepl(i, text, fixed = TRUE)
            if (!any(match)) {
                stop("No links have text '", i, "'", call. = FALSE)
            }
            a <- links[[which(match)[1]]]
        }
    }
    else {
        links <- html_nodes(x, css = css, xpath = xpath)
        if (length(links) == 0) {
            stop("No links matched that expression", call. = FALSE)
        }
        a <- links[[1]]
    }
    url <- html_attr(a, "href")
    message("Navigating to ", url)
    jump_to(x, url, ...)
}


session_history <- function (x) 
{
    structure(list(back = rev(x$back), url = x$url, forward = x$forward), 
        class = "history")
}


html_nodes <- function (x, css, xpath) 
{
    UseMethod("html_nodes")
}


html_node <- function (x, css, xpath) 
{
    UseMethod("html_node")
}


set_values <- function (form, ...) 
{
    new_values <- list(...)
    no_match <- setdiff(names(new_values), names(form$fields))
    if (length(no_match) > 0) {
        stop("Unknown field names: ", paste(no_match, collapse = ", "), 
            call. = FALSE)
    }
    for (field in names(new_values)) {
        type <- form$fields[[field]]$type %||% "non-input"
        if (type == "hidden") {
            warning("Setting value of hidden field '", field, 
                "'.", call. = FALSE)
        }
        else if (type == "submit") {
            stop("Can't change value of submit input '", field, 
                "'.", call. = FALSE)
        }
        form$fields[[field]]$value <- new_values[[field]]
    }
    form
}


submit_form <- function (session, form, submit = NULL, ...) 
{
    request <- submit_request(form, submit)
    url <- xml2::url_absolute(form$url, session$url)
    if (request$method == "GET") {
        request_GET(session, url = url, query = request$values, 
            ...)
    }
    else if (request$method == "POST") {
        request_POST(session, url = url, body = request$values, 
            encode = request$encode, ...)
    }
    else {
        stop("Unknown method: ", request$method, call. = FALSE)
    }
}


pluck <- function (x, i, type) 
{
    if (missing(type)) {
        lapply(x, .subset2, i)
    }
    else {
        vapply(x, .subset2, i, FUN.VALUE = type)
    }
}


html <- function (x, ..., encoding = "") 
{
    .Deprecated("read_html")
    xml2::read_html(x, ..., encoding = encoding)
}


jump_to <- function (x, url, ...) 
{
    stopifnot(is.session(x))
    x$back <- c(x$url, x$back)
    x$forward <- character()
    url <- xml2::url_absolute(url, x$url)
    request_GET(x, url, ...)
}


`%>%` <- magrittr::`%>%` # re-exported from magrittr package

html_name <- function (x) 
{
    xml2::xml_name(x)
}


minimal_html <- function (title, html = "") 
{
    xml2::read_html(paste0("<!doctype html>\n", "<meta charset=utf-8>\n", 
        "<title>", title, "</title>\n", html))
}


html_attrs <- function (x) 
{
    xml2::xml_attrs(x)
}


html_tag <- function (x) 
{
    .Deprecated("html_name")
    xml2::xml_name(x)
}


xml_nodes <- function (x, css, xpath) 
{
    UseMethod("html_nodes")
}


google_form <- function (x) 
{
    xml2::read_html(httr::GET(paste0("https://docs.google.com/forms/d/", 
        x, "/viewform")))
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Easily Harvest (Scrape) Web Pages"

.skeleton_package_version = "0.3.2"

.skeleton_package_depends = "xml2"

.skeleton_package_imports = "httr,selectr,magrittr"


## Internal

.skeleton_version = 5


## EOF