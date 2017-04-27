##
## Exported symobls in package `httr`
##

## Exported package methods

Token2.0 <- "<environment>"

cookies <- function (x) 
UseMethod("cookies")


oauth_service_token <- function (endpoint, secrets, scope = NULL) 
{
    if (!is.oauth_endpoint(endpoint)) 
        stop("`endpoint` must be an OAuth endpoint", call. = FALSE)
    if (!is.list(secrets)) 
        stop("`secrets` must be a list.", call. = FALSE)
    if (!is.null(scope) && !(is.character(scope) && length(scope) == 
        1)) 
        stop("`scope` must be a length 1 character vector.", 
            call. = FALSE)
    TokenServiceAccount$new(endpoint = endpoint, secrets = secrets, 
        params = list(scope = scope))
}


RETRY <- function (verb, url = NULL, config = list(), ..., body = NULL, 
    encode = c("multipart", "form", "json", "raw"), times = 3, 
    pause_base = 1, pause_cap = 60, handle = NULL, quiet = FALSE) 
{
    stopifnot(is.numeric(times), length(times) == 1L)
    stopifnot(is.numeric(pause_base), length(pause_base) == 1L)
    stopifnot(is.numeric(pause_cap), length(pause_cap) == 1L)
    hu <- handle_url(handle, url, ...)
    req <- request_build(verb, hu$url, body_config(body, match.arg(encode)), 
        config, ...)
    resp <- request_perform(req, hu$handle$handle)
    i <- 1
    while (i < times && http_error(resp)) {
        backoff_full_jitter(i, status_code(resp), pause_base, 
            pause_cap, quiet = quiet)
        i <- i + 1
        resp <- request_perform(req, hu$handle$handle)
    }
    resp
}


write_stream <- function (f) 
{
    stopifnot(is.function(f), length(formals(f)) == 1)
    request(output = write_function("write_stream", f = f))
}


content_type_json <- function () 
content_type("application/json")


content_type <- function (type) 
{
    if (is.null(type)) 
        return()
    if (substr(type, 1, 1) == ".") {
        type <- mime::guess_type(type, empty = NULL)
    }
    add_headers(`Content-Type` = type)
}


GET <- function (url = NULL, config = list(), ..., handle = NULL) 
{
    hu <- handle_url(handle, url, ...)
    req <- request_build("GET", hu$url, as.request(config), ...)
    request_perform(req, hu$handle$handle)
}


oauth_exchanger <- function (request_url) 
{
    message("Please point your browser to the following url: ")
    message("")
    message("  ", request_url)
    message("")
    authorization_code <- str_trim(readline("Enter authorization code: "))
    info <- list(code = authorization_code)
    info
}


write_function <- function (subclass, ...) 
{
    structure(list(...), class = c(subclass, "write_function"))
}


add_headers <- function (..., .headers = character()) 
{
    request(headers = c(..., .headers))
}


oauth2.0_token <- function (endpoint, app, scope = NULL, user_params = NULL, type = NULL, 
    use_oob = getOption("httr_oob_default"), as_header = TRUE, 
    use_basic_auth = FALSE, cache = getOption("httr_oauth_cache")) 
{
    params <- list(scope = scope, user_params = user_params, 
        type = type, use_oob = use_oob, as_header = as_header, 
        use_basic_auth = use_basic_auth)
    Token2.0$new(app = app, endpoint = endpoint, params = params, 
        cache_path = cache)
}


headers <- function (x) 
UseMethod("headers")


progress <- function (type = c("down", "up"), con = stdout()) 
{
    type <- match.arg(type)
    request(options = list(noprogress = FALSE, progressfunction = progress_bar(type, 
        con)))
}


http_error <- function (x, ...) 
{
    UseMethod("http_error")
}


set_config <- function (config, override = FALSE) 
{
    stopifnot(is.request(config))
    old <- getOption("httr_config") %||% config()
    if (!override) 
        config <- c(old, config)
    options(httr_config = config)
    invisible(old)
}


http_type <- function (x) 
{
    stopifnot(is.response(x))
    type <- x$headers[["Content-Type"]] %||% mime::guess_type(x$url, 
        empty = "application/octet-stream")
    parse_media(type)$complete
}


upload_file <- function (path, type = NULL) 
{
    stopifnot(is.character(path), length(path) == 1, file.exists(path))
    if (is.null(type)) 
        type <- mime::guess_type(path)
    curl::form_file(path, type)
}


content <- function (x, as = NULL, type = NULL, encoding = NULL, ...) 
{
    stopifnot(is.response(x))
    type <- type %||% x$headers[["Content-Type"]] %||% mime::guess_type(x$url, 
        empty = "application/octet-stream")
    as <- as %||% parseability(type)
    as <- match.arg(as, c("raw", "text", "parsed"))
    if (is.path(x$content)) {
        raw <- readBin(x$content, "raw", file.info(x$content)$size)
    }
    else {
        raw <- x$content
    }
    switch(as, raw = raw, text = parse_text(raw, type, encoding), 
        parsed = parse_auto(raw, type, encoding, ...))
}


sign_oauth2.0 <- function (access_token, as_header = TRUE) 
{
    stop("Deprecated: supply token object to config directly", 
        call. = FALSE)
}


use_proxy <- function (url, port = NULL, username = NULL, password = NULL, 
    auth = "basic") 
{
    if (!is.null(username) || !is.null(password)) {
        proxyuserpwd <- paste0(username, ":", password)
    }
    else {
        proxyuserpwd <- NULL
    }
    if (!is.null(port)) 
        stopifnot(is.numeric(port))
    config(proxy = url, proxyuserpwd = proxyuserpwd, proxyport = port, 
        proxyauth = auth_flags(auth))
}


handle <- function (url, cookies = TRUE) 
{
    stopifnot(is.character(url), length(url) == 1)
    if (!missing(cookies)) 
        warning("Cookies argument is depcrated", call. = FALSE)
    h <- curl::new_handle()
    structure(list(handle = h, url = url), class = "handle")
}


http_date <- function (x) 
{
    if (is.null(x)) 
        return(NULL)
    stopifnot(inherits(x, "POSIXt"))
    c_time(strftime(x, "%a, %d %b %Y %H:%M:%S", tz = "GMT", usetz = TRUE))
}


write_memory <- function () 
{
    request(output = write_function("write_memory"))
}


authenticate <- function (user, password, type = "basic") 
{
    stopifnot(is.character(user), length(user) == 1)
    stopifnot(is.character(password), length(password) == 1)
    stopifnot(is.character(type), length(type) == 1)
    config(httpauth = auth_flags(type), userpwd = paste0(user, 
        ":", password))
}


warn_for_status <- function (x, task = NULL) 
{
    if (status_code(x) < 300) 
        return(invisible(x))
    call <- sys.call(-1)
    warning(http_condition(x, "warning", task = task, call = call))
}


parse_media <- function (x) 
{
    stopifnot(!is.null(x))
    parse <- function(x, sep) {
        scan(text = x, what = character(), sep = sep, quiet = TRUE, 
            quote = "\"")
    }
    pieces <- str_trim(parse(tolower(x), ";"))
    types <- str_split_fixed(pieces[1], "/", 2)[1, ]
    type <- tolower(types[1])
    subtype <- tolower(types[2])
    if (length(pieces) > 1) {
        param_pieces <- lapply(pieces[-1], str_split_fixed, "=", 
            2)
        names <- vapply(param_pieces, "[", 1, FUN.VALUE = character(1))
        values <- vapply(param_pieces, "[", 2, FUN.VALUE = character(1))
        params <- stats::setNames(as.list(values), names)
    }
    else {
        params <- list()
    }
    list(complete = paste(type, "/", subtype, sep = ""), type = type, 
        subtype = subtype, params = params)
}


curl_docs <- function (x) 
{
    stopifnot(is.character(x), length(x) == 1)
    opts <- httr_options()
    if (x %in% opts$httr) {
        x <- opts$libcurl[match(x, opts$httr)]
    }
    if (!(x %in% opts$libcurl)) {
        stop(x, " is not a known curl option", call. = FALSE)
    }
    url <- paste0("http://curl.haxx.se/libcurl/c/", x, ".html")
    BROWSE(url)
}


reset_config <- function () 
set_config(config(), TRUE)


config <- function (..., token = NULL) 
{
    request(options = list(...), auth_token = token)
}


handle_reset <- function (url) 
{
    name <- handle_name(url)
    if (exists(name, envir = handle_pool)) {
        rm(list = name, envir = handle_pool)
    }
}


sha1_hash <- function (key, string, method = "HMAC-SHA1") 
{
    if (is.character(string)) 
        string <- charToRaw(paste(string, collapse = "\n"))
    if (is.character(key)) 
        key <- charToRaw(paste(key, collapse = "\n"))
    if (!method %in% c("HMAC-SHA1", "RSA-SHA1")) {
        stop(paste0("Unsupported hashing method: ", method), 
            call. = FALSE)
    }
    if (method == "HMAC-SHA1") {
        hash <- openssl::sha1(string, key = key)
    }
    else {
        hash <- openssl::signature_create(string, openssl::sha1, 
            key = key)
    }
    openssl::base64_encode(hash)
}


build_url <- function (url) 
{
    stopifnot(is.url(url))
    scheme <- url$scheme
    hostname <- url$hostname
    if (!is.null(url$port)) {
        port <- paste0(":", url$port)
    }
    else {
        port <- NULL
    }
    path <- paste(gsub("^/", "", url$path), collapse = "/")
    if (!is.null(url$params)) {
        params <- paste0(";", url$params)
    }
    else {
        params <- NULL
    }
    if (is.list(url$query)) {
        query <- compose_query(url$query)
    }
    else {
        query <- url$query
    }
    if (!is.null(query)) {
        stopifnot(is.character(query), length(query) == 1)
        query <- paste0("?", query)
    }
    if (is.null(url$username) && !is.null(url$password)) {
        stop("Cannot set password without username")
    }
    paste0(scheme, "://", url$username, if (!is.null(url$password)) 
        ":", url$password, if (!is.null(url$username)) 
        "@", hostname, port, "/", path, params, query, if (!is.null(url$fragment)) 
        "#", url$fragment)
}


has_content <- function (x) 
{
    length(x$content) > 0
}


handle_find <- function (url) 
{
    name <- handle_name(url)
    if (exists(name, handle_pool)) {
        handle <- handle_pool[[name]]
    }
    else {
        handle <- handle(name)
        handle_pool[[name]] <- handle
    }
    handle
}


oauth_header <- function (info) 
{
    oauth <- paste0("OAuth ", paste0(oauth_encode(names(info)), 
        "=\"", oauth_encode(info), "\"", collapse = ", "))
    add_headers(Authorization = oauth)
}


BROWSE <- function (url = NULL, config = list(), ..., handle = NULL) 
{
    if (!interactive()) 
        return()
    hu <- handle_url(handle, url, ...)
    utils::browseURL(hu$url)
}


init_oauth1.0 <- function (endpoint, app, permission = NULL, is_interactive = interactive(), 
    private_key = NULL) 
{
    oauth_sig <- function(url, method, token = NULL, token_secret = NULL, 
        private_key = NULL, ...) {
        oauth_header(oauth_signature(url, method, app, token, 
            token_secret, private_key, other_params = c(list(...), 
                oauth_callback = oauth_callback())))
    }
    response <- POST(endpoint$request, oauth_sig(endpoint$request, 
        "POST", private_key = private_key))
    stop_for_status(response)
    params <- content(response, type = "application/x-www-form-urlencoded")
    token <- params$oauth_token
    secret <- params$oauth_token_secret
    authorize_url <- modify_url(endpoint$authorize, query = list(oauth_token = token, 
        permission = "read"))
    verifier <- oauth_listener(authorize_url, is_interactive)
    verifier <- verifier$oauth_verifier %||% verifier[[1]]
    response <- POST(endpoint$access, oauth_sig(endpoint$access, 
        "POST", token, secret, oauth_verifier = verifier, private_key = private_key), 
        body = "")
    stop_for_status(response)
    content(response, type = "application/x-www-form-urlencoded")
}


PATCH <- function (url = NULL, config = list(), ..., body = NULL, encode = c("multipart", 
    "form", "json", "raw"), handle = NULL) 
{
    encode <- match.arg(encode)
    hu <- handle_url(handle, url, ...)
    req <- request_build("PATCH", hu$url, body_config(body, match.arg(encode)), 
        config, ...)
    request_perform(req, hu$handle$handle)
}


url_ok <- function (x, ...) 
{
    warning("`url_ok(x)` is deprecated; ", "please use `identical(status_code(x), 200L)` instead.", 
        call. = FALSE)
    identical(status_code(HEAD(x, ...)), 200L)
}


http_condition <- function (x, type, task = NULL, call = sys.call(-1)) 
{
    type <- match.arg(type, c("error", "warning", "message"))
    if (is.null(task)) {
        task <- ""
    }
    else if (is.character(task)) {
        task <- paste0(" Failed to ", task, ".")
    }
    else {
        stop("`task` must be NULL or a character vector", call. = FALSE)
    }
    status <- status_code(x)
    reason <- http_status(status)$reason
    message <- sprintf("%s (HTTP %d).%s", reason, status, task)
    status_type <- (status%/%100) * 100
    http_class <- paste0("http_", unique(c(status, status_type, 
        "error")))
    structure(list(message = message, call = call), class = c(http_class, 
        type, "condition"))
}


http_status <- function (x) 
{
    status <- status_code(x)
    status_desc <- http_statuses[[as.character(status)]]
    if (is.na(status_desc)) {
        stop("Unknown http status code: ", status, call. = FALSE)
    }
    status_types <- c("Information", "Success", "Redirection", 
        "Client error", "Server error")
    status_type <- status_types[[status%/%100]]
    message <- paste(status_type, ": (", status, ") ", status_desc, 
        sep = "")
    list(category = status_type, reason = status_desc, message = message)
}


set_cookies <- function (..., .cookies = character(0)) 
{
    cookies <- c(..., .cookies)
    stopifnot(is.character(cookies))
    cookies_str <- vapply(cookies, curl::curl_escape, FUN.VALUE = character(1))
    cookie <- paste(names(cookies), cookies_str, sep = "=", collapse = ";")
    config(cookie = cookie)
}


message_for_status <- function (x, task = NULL) 
{
    call <- sys.call(-1)
    message(http_condition(x, "message", task = task, call = call))
}


accept_xml <- function () 
accept("application/xml")


stop_for_status <- function (x, task = NULL) 
{
    if (status_code(x) < 300) 
        return(invisible(x))
    call <- sys.call(-1)
    stop(http_condition(x, "error", task = task, call = call))
}


status_code <- function (x) 
UseMethod("status_code")


POST <- function (url = NULL, config = list(), ..., body = NULL, encode = c("multipart", 
    "form", "json", "raw"), handle = NULL) 
{
    encode <- match.arg(encode)
    hu <- handle_url(handle, url, ...)
    req <- request_build("POST", hu$url, body_config(body, match.arg(encode)), 
        as.request(config), ...)
    request_perform(req, hu$handle$handle)
}


oauth_callback <- function () 
{
    paste0("http://", Sys.getenv("HTTR_SERVER", "localhost"), 
        ":", Sys.getenv("HTTR_SERVER_PORT", "1410"), "/")
}


text_content <- function (x) 
{
    message("text_content() deprecated. Use content(x, as = 'text')")
    content(x, as = "text")
}


timeout <- function (seconds) 
{
    if (seconds < 0.001) {
        stop("Timeout cannot be less than 1 ms", call. = FALSE)
    }
    config(timeout_ms = seconds * 1000)
}


oauth_listener <- function (request_url, is_interactive = interactive()) 
{
    if (!is_installed("httpuv")) {
        stop("httpuv package required to capture OAuth credentials.")
    }
    if (!is_interactive) {
        stop("oauth_listener() needs an interactive environment.", 
            call. = FALSE)
    }
    info <- NULL
    listen <- function(env) {
        if (!identical(env$PATH_INFO, "/")) {
            return(list(status = 404L, headers = list(`Content-Type` = "text/plain"), 
                body = "Not found"))
        }
        query <- env$QUERY_STRING
        if (!is.character(query) || identical(query, "")) {
            info <<- NA
        }
        else {
            info <<- parse_query(gsub("^\\?", "", query))
        }
        list(status = 200L, headers = list(`Content-Type` = "text/plain"), 
            body = "Authentication complete. Please close this page and return to R.")
    }
    use <- listener_endpoint()
    server <- httpuv::startServer(use$host, use$port, list(call = listen))
    on.exit(httpuv::stopServer(server))
    message("Waiting for authentication in browser...")
    message("Press Esc/Ctrl + C to abort")
    BROWSE(request_url)
    while (is.null(info)) {
        httpuv::service()
        Sys.sleep(0.001)
    }
    httpuv::service()
    if (identical(info, NA)) {
        stop("Authentication failed.", call. = FALSE)
    }
    message("Authentication complete.")
    info
}


HEAD <- function (url = NULL, config = list(), ..., handle = NULL) 
{
    hu <- handle_url(handle, url, ...)
    req <- request_build("HEAD", hu$url, config, ..., config(nobody = TRUE))
    request_perform(req, hu$handle$handle)
}


PUT <- function (url = NULL, config = list(), ..., body = NULL, encode = c("multipart", 
    "form", "json", "raw"), handle = NULL) 
{
    hu <- handle_url(handle, url, ...)
    req <- request_build("PUT", hu$url, body_config(body, match.arg(encode)), 
        config, ...)
    request_perform(req, hu$handle$handle)
}


DELETE <- function (url = NULL, config = list(), ..., body = NULL, encode = c("multipart", 
    "form", "json", "raw"), handle = NULL) 
{
    hu <- handle_url(handle, url, ...)
    req <- request_build("DELETE", hu$url, body_config(body, 
        match.arg(encode)), as.request(config), ...)
    request_perform(req, hu$handle$handle)
}


verbose <- function (data_out = TRUE, data_in = FALSE, info = FALSE, ssl = FALSE) 
{
    debug <- function(type, msg) {
        switch(type + 1, text = if (info) prefix_message("*  ", 
            msg), headerIn = prefix_message("<- ", msg), headerOut = prefix_message("-> ", 
            msg), dataIn = if (data_in) prefix_message("<<  ", 
            msg, TRUE), dataOut = if (data_out) prefix_message(">> ", 
            msg, TRUE), sslDataIn = if (data_in && ssl) prefix_message("*< ", 
            msg, TRUE), sslDataOut = if (data_out && ssl) prefix_message("*> ", 
            msg, TRUE))
    }
    config(debugfunction = debug, verbose = TRUE)
}


accept <- function (type) 
{
    if (substr(type, 1, 1) == ".") {
        type <- mime::guess_type(type, empty = NULL)
    }
    add_headers(Accept = type)
}


sign_oauth1.0 <- function (app, token = NULL, token_secret = NULL, as_header = TRUE, 
    ...) 
{
    params <- list(as_header = as_header)
    credentials <- list(oauth_token = token, oauth_token_secret = token_secret)
    token <- Token1.0$new(endpoint = NULL, params = params, app = app, 
        credentials = credentials)
    request(auth_token = token)
}


oauth_app <- function (appname, key, secret = NULL) 
{
    if (missing(secret)) {
        env_name <- paste0(toupper(appname), "_CONSUMER_SECRET")
        secret <- Sys.getenv(env_name)
        if (secret == "") {
            warning("Couldn't find secret in environment variable ", 
                env_name, call. = FALSE)
            secret <- NULL
        }
        else {
            message("Using secret stored in environment variable ", 
                env_name)
        }
    }
    structure(list(appname = appname, secret = secret, key = key), 
        class = "oauth_app")
}


guess_media <- function (x) 
{
    .Deprecated("mime::guess_type")
    mime::guess_type(x, empty = NULL)
}


revoke_all <- function (cache_path = NA) 
{
    cache_path <- use_cache(cache_path)
    if (is.null(cache_path)) {
        stop("Can't find cache")
    }
    tokens <- load_cache(cache_path)
    cant_revoke <- vapply(tokens, function(x) is.null(x$endpoint$revoke), 
        logical(1))
    if (any(cant_revoke)) {
        manual <- tokens[cant_revoke]
        apps <- vapply(manual, function(x) {
            paste0(x$app$appname, " (", x$app$key, ")")
        }, character(1), USE.NAMES = FALSE)
        warning("Can't revoke the following tokens automatically: ", 
            paste0(apps, collapse = ", "), call. = FALSE)
    }
    lapply(tokens, function(x) try(revoke_oauth2.0(x)))
    invisible(TRUE)
}


oauth_endpoints <- function (name) 
{
    switch(name, linkedin = oauth_endpoint(base_url = "https://www.linkedin.com/uas/oauth2", 
        authorize = "authorization", access = "accessToken"), 
        twitter = oauth_endpoint(base_url = "https://api.twitter.com/oauth", 
            request = "request_token", authorize = "authenticate", 
            access = "access_token"), vimeo = oauth_endpoint(base_url = "https://vimeo.com/oauth", 
            request = "request_token", authorize = "authorize", 
            access = "access_token"), yahoo = oauth_endpoint(base_url = "https://api.login.yahoo.com/oauth/v2", 
            request = "get_request_token", authorize = "request_auth", 
            access = "get_token"), google = oauth_endpoint(base_url = "https://accounts.google.com/o/oauth2", 
            authorize = "auth", access = "token", validate = "https://www.googleapis.com/oauth2/v1/tokeninfo", 
            revoke = "revoke"), tumblr = oauth_endpoint(base_url = "http://www.tumblr.com/oauth/", 
            request = "request_token", authorize = "authorize", 
            access = "access_token"), facebook = oauth_endpoint(authorize = "https://www.facebook.com/dialog/oauth", 
            access = "https://graph.facebook.com/oauth/access_token"), 
        github = oauth_endpoint(base_url = "https://github.com/login/oauth", 
            request = NULL, authorize = "authorize", access = "access_token"), 
        azure = oauth_endpoint(base_url = "https://login.windows.net/common/oauth2", 
            authorize = "authorize", access = "token"), stop("Unknown endpoint", 
            call. = FALSE))
}


Token <- "<environment>"

user_agent <- function (agent) 
{
    stopifnot(is.character(agent), length(agent) == 1)
    config(useragent = agent)
}


oauth_endpoint <- function (request = NULL, authorize, access, ..., base_url = NULL) 
{
    urls <- list(request = request, authorize = authorize, access = access, 
        ...)
    if (is.null(base_url)) {
        return(do.call(endpoint, urls))
    }
    path <- parse_url(base_url)$path
    add_base_url <- function(x) {
        if (is.null(x)) 
            return(x)
        if (substr(x, 1, 4) == "http") 
            return(x)
        modify_url(base_url, path = file.path(path, x))
    }
    urls <- lapply(urls, add_base_url)
    do.call(endpoint, urls)
}


modify_url <- function (url, scheme = NULL, hostname = NULL, port = NULL, path = NULL, 
    query = NULL, params = NULL, fragment = NULL, username = NULL, 
    password = NULL) 
{
    old <- parse_url(url)
    new <- compact(list(scheme = scheme, hostname = hostname, 
        port = port, path = path, query = query, params = params, 
        fragment = fragment, username = username, password = password))
    build_url(utils::modifyList(old, new))
}


write_disk <- function (path, overwrite = FALSE) 
{
    if (!overwrite && file.exists(path)) {
        stop("Path exists and overwrite is FALSE", call. = FALSE)
    }
    request(output = write_function("write_disk", path = path, 
        file = NULL))
}


Token1.0 <- "<environment>"

cache_info <- function (r) 
{
    stopifnot(is.response(r))
    expires <- parse_http_date(r$headers$expires, Inf) %||% NULL
    control <- parse_cache_control(r$headers$`cache-control`)
    max_age <- as.integer(control$`max-age`) %||% NULL
    if (!is.null(max_age)) {
        expires <- r$date + max_age
    }
    else if (!is.null(r$headers$expires)) {
        expires <- parse_http_date(r$headers$expires, -Inf)
    }
    else {
        expires <- NULL
    }
    cacheable <- r$request$method %in% c("GET", "HEAD") && status_code(r) %in% 
        c(200L, 203L, 300L, 301L, 410L) && (!is.null(expires) || 
        !is.null(r$headers$etag) || !is.null(r$headers$`last-modified`)) && 
        !any(c("no-store", "no-cache") %in% control$flags)
    structure(list(method = r$method, url = r$url, cacheable = cacheable, 
        expires = expires, etag = r$headers$etag %||% NULL, modified = parse_http_date(r$headers$`last-modified`, 
            NULL)), class = "cache_info")
}


parse_url <- function (url) 
{
    if (is.url(url)) 
        return(url)
    url <- as.character(url)
    stopifnot(length(url) == 1)
    pull_off <- function(pattern) {
        if (!str_detect(url, pattern)) 
            return(NULL)
        piece <- str_match(url, pattern)[, 2]
        url <<- str_replace(url, pattern, "")
        piece
    }
    fragment <- pull_off("#(.*)$")
    scheme <- pull_off("^([[:alpha:]+.-]+):")
    netloc <- pull_off("^//([^/?]*)/?")
    if (identical(netloc, "")) {
        url <- paste0("/", url)
        port <- username <- password <- hostname <- NULL
    }
    else if (!is.null(netloc)) {
        pieces <- strsplit(netloc, "@")[[1]]
        if (length(pieces) == 1) {
            username <- NULL
            password <- NULL
            host <- pieces
        }
        else {
            user_pass <- strsplit(pieces[[1]], ":")[[1]]
            username <- user_pass[1]
            if (length(user_pass) == 1) {
                password <- NULL
            }
            else {
                password <- user_pass[2]
            }
            host <- pieces[2]
        }
        host_pieces <- str_split(host, ":")[[1]]
        hostname <- host_pieces[1]
        port <- if (length(host_pieces) > 1) 
            host_pieces[2]
    }
    else {
        port <- username <- password <- hostname <- NULL
    }
    query <- pull_off("\\?(.*)$")
    if (!is.null(query)) {
        query <- parse_query(query)
    }
    params <- pull_off(";(.*)$")
    structure(list(scheme = scheme, hostname = hostname, port = port, 
        path = url, query = query, params = params, fragment = fragment, 
        username = username, password = password), class = "url")
}


with_verbose <- function (expr, ...) 
{
    with_config(verbose(...), expr)
}


oauth_signature <- function (url, method = "GET", app, token = NULL, token_secret = NULL, 
    private_key = NULL, other_params = NULL) 
{
    if (!is.null(private_key)) {
        signature_method <- "RSA-SHA1"
    }
    else {
        signature_method <- "HMAC-SHA1"
    }
    method <- toupper(method)
    url <- parse_url(url)
    base_url <- build_url(url[c("scheme", "hostname", "port", 
        "url", "path")])
    oauth <- compact(list(oauth_consumer_key = app$key, oauth_nonce = nonce(), 
        oauth_signature_method = signature_method, oauth_timestamp = as.integer(Sys.time()), 
        oauth_version = "1.0", oauth_token = token))
    if (length(other_params) > 0) {
        oauth <- c(oauth, other_params)
    }
    params <- c(url$query, oauth)
    params_esc <- stats::setNames(oauth_encode(params), oauth_encode(names(params)))
    params_srt <- sort_names(params_esc)
    params_str <- paste0(names(params_srt), "=", params_srt, 
        collapse = "&")
    base_string <- paste0(method, "&", oauth_encode(base_url), 
        "&", oauth_encode(params_str))
    if (signature_method == "HMAC-SHA1") {
        private_key <- paste0(oauth_encode(app$secret), "&", 
            oauth_encode(token_secret))
    }
    oauth$oauth_signature <- sha1_hash(private_key, base_string, 
        signature_method)
    sort_names(oauth)
}


parsed_content <- function (x, ...) 
{
    message("text_content() deprecated. Use parsed_content(x, as = 'parsed')")
    content(x, as = "parsed", ...)
}


httr_options <- function (matches) 
{
    constants <- curl::curl_options()
    constants <- constants[order(names(constants))]
    rcurl <- tolower(names(constants))
    opts <- data.frame(httr = rcurl, libcurl = translate_curl(rcurl), 
        type = curl_option_types(constants), stringsAsFactors = FALSE)
    if (!missing(matches)) {
        sel <- grepl(matches, opts$httr, ignore.case = TRUE) | 
            grepl(matches, opts$libcurl, ignore.case = TRUE)
        opts <- opts[sel, , drop = FALSE]
    }
    opts
}


safe_callback <- function (f) 
{
    warning("`safe_callback()` is no longer needed and will be removed in a ", 
        "future version", call. = FALSE)
    f
}


accept_json <- function () 
accept("application/json")


hmac_sha1 <- function (key, string) 
{
    sha1_hash(key, string, "HMAC-SHA1")
}


url_success <- function (x, ...) 
{
    warning("`url_success(x)` is deprecated; please use `!http_error(x)` instead.", 
        call. = FALSE)
    !http_error(x, ...)
}


with_config <- function (config = config(), expr, override = FALSE) 
{
    stopifnot(is.request(config))
    old <- set_config(config, override)
    on.exit(set_config(old, override = TRUE))
    force(expr)
}


VERB <- function (verb, url = NULL, config = list(), ..., body = NULL, 
    encode = c("multipart", "form", "json", "raw"), handle = NULL) 
{
    hu <- handle_url(handle, url, ...)
    req <- request_build(verb, hu$url, body_config(body, match.arg(encode)), 
        config, ...)
    request_perform(req, hu$handle$handle)
}


oauth1.0_token <- function (endpoint, app, permission = NULL, as_header = TRUE, 
    private_key = NULL, cache = getOption("httr_oauth_cache")) 
{
    params <- list(permission = permission, as_header = as_header)
    Token1.0$new(app = app, endpoint = endpoint, params = params, 
        private_key = private_key, cache_path = cache)
}


init_oauth2.0 <- function (endpoint, app, scope = NULL, user_params = NULL, type = NULL, 
    use_oob = getOption("httr_oob_default"), is_interactive = interactive(), 
    use_basic_auth = FALSE) 
{
    if (!use_oob && !is_installed("httpuv")) {
        message("httpuv not installed, defaulting to out-of-band authentication")
        use_oob <- TRUE
    }
    if (isTRUE(use_oob)) {
        stopifnot(interactive())
        redirect_uri <- "urn:ietf:wg:oauth:2.0:oob"
        state <- NULL
    }
    else {
        redirect_uri <- oauth_callback()
        state <- nonce()
    }
    scope_arg <- paste(scope, collapse = " ")
    authorize_url <- modify_url(endpoint$authorize, query = compact(list(client_id = app$key, 
        scope = scope_arg, redirect_uri = redirect_uri, response_type = "code", 
        state = state)))
    if (isTRUE(use_oob)) {
        code <- oauth_exchanger(authorize_url)$code
    }
    else {
        code <- oauth_listener(authorize_url, is_interactive)$code
    }
    req_params <- list(client_id = app$key, redirect_uri = redirect_uri, 
        grant_type = "authorization_code", code = code)
    if (!is.null(user_params)) {
        req_params <- utils::modifyList(user_params, req_params)
    }
    if (isTRUE(use_basic_auth)) {
        req <- POST(endpoint$access, encode = "form", body = req_params, 
            authenticate(app$key, app$secret, type = "basic"))
    }
    else {
        req_params$client_secret <- app$secret
        req <- POST(endpoint$access, encode = "form", body = req_params)
    }
    stop_for_status(req, task = "get an access token")
    content(req, type = type)
}


TokenServiceAccount <- "<environment>"

httr_dr <- function () 
{
    check_for_nss()
}


insensitive <- function (x) 
{
    names(x) <- tolower(names(x))
    structure(x, class = c("insensitive", class(x)))
}


rerequest <- function (r) 
{
    x <- cache_info(r)
    if (!x$cacheable) {
        return(reperform(r))
    }
    if (!is.null(x$expires) && x$expires >= Sys.time()) {
        return(r)
    }
    req <- c(r$request, request(headers = c(`If-Modified-Since` = http_date(x$modified), 
        `If-None-Match` = x$etag)))
    validated <- request_perform(req, r$handle)
    if (status_code(validated) == 304L) {
        r
    }
    else {
        validated
    }
}


parse_http_date <- function (x, failure = NA) 
{
    if (length(x) == 0) 
        return(NULL)
    fmts <- c("%a, %d %b %Y %H:%M:%S", "%A, %d-%b-%y %H:%M:%S", 
        "%a %b %d %H:%M:%S %Y")
    for (fmt in fmts) {
        parsed <- c_time(as.POSIXct(strptime(x, fmt, tz = "GMT")))
        if (all(!is.na(parsed))) 
            return(parsed)
    }
    rep(failure, length(x))
}


content_type_xml <- function () 
content_type("application/xml")




## Package Data

# none


## Package Info

.skeleton_package_title = "Tools for Working with URLs and HTTP"

.skeleton_package_version = "1.2.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "jsonlite,mime,curl,openssl,R6"


## Internal

.skeleton_version = 5


## EOF