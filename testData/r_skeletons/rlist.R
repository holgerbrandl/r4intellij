##
## Exported symobls in package `rlist`
##

## Exported package methods

list.filter <- function (.data, ...) 
{
    conds <- dots(...)
    envir <- parent.frame()
    reduce(function(data, cond) {
        data[which(list.is.internal(data, cond, envir))]
    }, conds, .data)
}


list.cases <- function (.data, expr, simplify = TRUE, sorted = TRUE) 
{
    expr <- if (missing(expr)) 
        quote(.)
    else substitute(expr)
    values <- list.map.internal(.data, expr, parent.frame())
    if (simplify && all(vapply(values, is.atomic, logical(1L)))) {
        values <- c(values, recursive = TRUE)
    }
    cases <- unique(values)
    if (sorted && is.atomic(cases)) 
        cases <- sort(cases)
    cases
}


list.cbind <- function (.data) 
{
    list.do(.data, "cbind")
}


list.sort <- function (.data, ..., na.last = NA) 
{
    .data[list.order.internal(.data, dots(...), parent.frame(), 
        na.last = na.last)]
}


list.last <- function (.data, cond) 
{
    if (is.empty(.data)) 
        return(NULL)
    if (missing(cond)) 
        return(.data[[length(.data)]])
    res <- list.first.internal(rev(.data), substitute(cond), 
        parent.frame(), na.rm = TRUE)
    res$value
}


list.insert <- function (.data, index, ...) 
{
    values <- if (is.list(.data)) 
        list(...)
    else c(..., recursive = FALSE)
    n <- length(.data)
    if (index < -n) 
        stop("Invalid index")
    if (index < 0L) 
        index <- n + index + 1L
    c(.data[0L:max(0L, index - 1L)], values, if (index <= n) .data[index:length(.data)] else NULL)
}


List <- function (data = list()) 
{
    call <- createCallClosure(data)
    all <- createListClosure(list.all, data)
    any <- createListClosure(list.any, data)
    append <- createListClosure(list.append, data)
    apply <- createListClosure(list.apply, data)
    cases <- createListClosure(list.cases, data)
    cbind <- createListClosure(list.cbind, data)
    class <- createListClosure(list.class, data)
    clean <- createListClosure(list.clean, data)
    common <- createListClosure(list.common, data)
    count <- createListClosure(list.count, data)
    do <- createListClosure(list.do, data)
    exclude <- createListClosure(list.exclude, data)
    extract <- createListClosure(list.extract, data)
    filter <- createListClosure(list.filter, data)
    find <- createListClosure(list.find, data)
    findi <- createListClosure(list.findi, data)
    first <- createListClosure(list.first, data)
    flatten <- createListClosure(list.flatten, data)
    group <- createListClosure(list.group, data)
    is <- createListClosure(list.is, data)
    insert <- createListClosure(list.insert, data)
    iter <- createListClosure(list.iter, data)
    join <- createListClosure(list.join, data)
    last <- createListClosure(list.last, data)
    load <- createListClosure(list.load, data)
    map <- createListClosure(list.map, data)
    mapv <- createListClosure(list.mapv, data)
    match <- createListClosure(list.match, data)
    merge <- createListClosure(list.merge, data)
    names <- createListClosure(list.names, data)
    order <- createListClosure(list.order, data)
    parse <- createListClosure(list.parse, data)
    prepend <- createListClosure(list.prepend, data)
    rbind <- createListClosure(list.rbind, data)
    remove <- createListClosure(list.remove, data)
    reverse <- createListClosure(list.reverse, data)
    sample <- createListClosure(list.sample, data)
    save <- createListClosure(list.save, data)
    search <- createListClosure(list.search, data)
    select <- createListClosure(list.select, data)
    serialize <- createListClosure(list.serialize, data)
    skip <- createListClosure(list.skip, data)
    skipWhile <- createListClosure(list.skipWhile, data)
    sort <- createListClosure(list.sort, data)
    stack <- createListClosure(list.stack, data)
    table <- createListClosure(list.table, data)
    take <- createListClosure(list.take, data)
    takeWhile <- createListClosure(list.takeWhile, data)
    ungroup <- createListClosure(list.ungroup, data)
    unserialize <- createListClosure(list.unserialize, data)
    upzip <- createListClosure(list.unzip, data)
    update <- createListClosure(list.update, data)
    which <- createListClosure(list.which, data)
    zip <- createListClosure(list.zip, data)
    subset <- createListClosure(list.subset, data)
    envir <- environment()
    setclass(envir, c("List", "environment"))
}


list.unserialize <- function (file, type = tolower(tools::file_ext(file)), ...) 
{
    fun <- paste("list.unserialize", type, sep = ".")
    if (exists(fun, mode = "function")) {
        fun <- get(fun, mode = "function")
        fun(file, ...)
    }
    else {
        conn <- file(file, open = "r")
        res <- unserialize(conn)
        close(conn)
        res
    }
}


list.exclude <- function (.data, cond) 
{
    .data[!list.is.internal(.data, substitute(cond), parent.frame())]
}


list.parse <- function (x, ...) 
UseMethod("list.parse")


list.search <- function (.data, expr, classes = "ANY", n, unlist = FALSE) 
{
    vec <- rapply(.data, function(x) TRUE, classes = classes)
    if (missing(n)) 
        n <- sum(vec)
    l <- lambda(substitute(expr))
    args <- args_env(i = 0L, n = 0L, N = n, indices = integer(n), 
        result = vector("list", n))
    fun <- list.search.fun
    environment(fun) <- parent.frame()
    formals(fun) <- setnames(formals(fun), c(".data", ".expr", 
        ".args", ".n", l$symbols))
    tryWithCondition(rapply(.data, fun, classes = classes, .expr = l$expr, 
        .args = args), rlist.finished = NULL)
    result <- list.clean(args$result, recursive = FALSE)
    names(result) <- names(vec)[args$indices]
    if (unlist) 
        result <- c(result, recursive = TRUE)
    result
}


list.findi <- function (.data, cond, n = 1L) 
{
    list.findi.internal(.data, substitute(cond), parent.frame(), 
        n)
}


tryEval <- function (expr, def = NULL) 
{
    x <- try(expr, silent = TRUE)
    if (is.error(x)) 
        def
    else x
}


list.clean <- function (.data, fun = is.null, recursive = FALSE) 
{
    if (recursive) {
        .data <- lapply(.data, function(.item) {
            if (is.list(.item)) 
                list.clean(.item, fun, recursive = TRUE)
            else .item
        })
    }
    setmembers(.data, vapply(.data, fun, logical(1L)), NULL)
}


list.join <- function (x, y, xkey, ykey, ..., keep.order = TRUE) 
{
    if (missing(xkey) && missing(ykey)) 
        stop("At least one key should be specified")
    sxkey <- substitute(xkey)
    sykey <- substitute(ykey)
    dfsxkey <- substitute(data.frame(xkey))
    if (missing(sykey)) {
        sykey <- sxkey
        dfsykey <- substitute(data.frame(xkey))
    }
    else {
        dfsykey <- substitute(data.frame(ykey))
    }
    xkeys.list <- list.map.internal(x, dfsxkey, parent.frame())
    ykeys.list <- list.map.internal(y, dfsykey, parent.frame())
    xkeys.df <- list.rbind(xkeys.list)
    ykeys.df <- list.rbind(ykeys.list)
    if (is.name(sxkey)) 
        colnames(xkeys.df) <- as.character(sxkey)
    if (is.name(sykey)) 
        colnames(ykeys.df) <- as.character(sykey)
    if (!identical(colnames(xkeys.df), colnames(ykeys.df))) {
        stop("Inconsistent keys")
    }
    xkeys <- cbind(.xi = seq_along(xkeys.list), xkeys.df)
    ykeys <- cbind(.yi = seq_along(ykeys.list), ykeys.df)
    df <- merge.data.frame(xkeys, ykeys, by = colnames(xkeys)[-1L], 
        ...)
    if (keep.order) 
        df <- df[order(df$.xi), ]
    map(modifyList, list(x[df$.xi], y[df$.yi]))
}


list.common <- function (.data, expr) 
{
    if (!length(.data)) {
        return(NULL)
    }
    if (missing(expr)) {
        expr <- quote(.)
    }
    else {
        expr <- substitute(expr)
    }
    values <- list.map.internal(.data, expr, parent.frame())
    return(reduce(intersect, values, values[[1L]]))
}


list.zip <- function (..., use.argnames = TRUE, use.names = TRUE) 
{
    args <- list(...)
    if (use.argnames) 
        args <- set_argnames(dots(...), args)
    results <- map(args_list, args)
    if (!use.names) 
        names(results) <- NULL
    results
}


list.unzip <- function (.data, .fields = c("intersect", "union"), ..., .aggregate = "simplify2array", 
    .missing = NA) 
{
    data_names <- lapply(.data, names)
    aggregator <- lapply(.aggregate, match.fun)
    args <- list(...)
    if (length(args) >= 1L && (is.null(names(args)) || !all(nzchar(names(args))))) 
        stop("Custom aggregate function must have a name", call. = FALSE)
    args <- lapply(args, function(f) {
        if (is.null(f)) 
            NULL
        else if (is.vector(f)) 
            lapply(f, match.fun)
        else match.fun(f)
    })
    fields <- Reduce(match.fun(match.arg(.fields)), data_names)
    names(fields) <- fields
    fields[names(args)[vapply(args, is.null, logical(1L))]] <- NA
    fields <- na.omit(fields)
    lapply(fields, function(field) {
        items <- lapply(.data, "[[", field)
        if (!is.null(.missing)) {
            missings <- vapply(items, is.null, logical(1L))
            items[missings] <- .missing
        }
        agg_fun <- if (!is.null(args[[field]])) 
            args[[field]]
        else aggregator
        if (is.list(agg_fun)) {
            reduce(function(res, f) f(res), agg_fun, items)
        }
        else agg_fun(items)
    })
}


list.map <- function (.data, expr) 
{
    list.map.internal(.data, substitute(expr), parent.frame())
}


list.skip <- function (.data, n) 
{
    if (!is.numeric(n)) 
        stop("n must be numeric or integer", call. = FALSE)
    if (n > 0L) 
        .data[-(1L:n)]
    else if (n < 0L) 
        .data[1L:(-n)]
    else .data
}


list.order <- function (.data, ..., keep.names = FALSE, na.last = TRUE) 
{
    result <- list.order.internal(.data, dots(...), parent.frame(), 
        na.last = na.last)
    if (keep.names) 
        setnames(result, names(.data))
    else result
}


list.prepend <- function (.data, ...) 
{
    if (is.list(.data)) {
        c(list(...), .data)
    }
    else {
        c(..., .data, recursive = FALSE)
    }
}


list.append <- function (.data, ...) 
{
    if (is.list(.data)) {
        c(.data, list(...))
    }
    else {
        c(.data, ..., recursive = FALSE)
    }
}


list.find <- function (.data, cond, n = 1L) 
{
    .data[list.findi.internal(.data, substitute(cond), parent.frame(), 
        n)]
}


list.subset <- .Primitive("[")


list.count <- function (.data, cond) 
{
    if (missing(cond)) 
        return(length(.data))
    length(which(list.is.internal(.data, substitute(cond), parent.frame())))
}


list.update <- function (.data, ..., keep.null = FALSE) 
{
    items <- lapply(dots(...), list.map.internal, .data = .data, 
        envir = parent.frame())
    map(function(.data, ...) modifyList(.data, list(...), keep.null = keep.null), 
        c(list(.data), items))
}


list.table <- function (.data, ..., table.args = list(useNA = "ifany")) 
{
    if (missing(...)) 
        return(do.call(table, c(list(.data), table.args)))
    args <- set_argnames(dots(...))
    envir <- parent.frame()
    items <- lapply(args, function(arg) {
        values <- list.map.internal(.data, arg, envir)
        values[vapply(values, is.null, logical(1L))] <- NA
        c(values, recursive = TRUE)
    })
    do.call(table, c(items, table.args))
}


list.serialize <- function (x, file, type = tools::file_ext(file), ...) 
{
    fun <- paste("list.serialize", tolower(type), sep = ".")
    if (exists(fun, mode = "function")) {
        fun <- get(fun, mode = "function")
        fun(x, file, ...)
    }
    else {
        conn <- file(file, open = "w")
        serialize(x, conn)
        close(conn)
    }
    invisible(x)
}


list.merge <- function (...) 
{
    lists <- list(...)
    if (any(vapply(lists, function(x) is.null(names(x)), logical(1L)))) 
        stop("All arguments must be named list", call. = FALSE)
    reduce(modifyList, lists, list())
}


list.sample <- function (.data, size, replace = FALSE, weight = 1, prob = NULL) 
{
    if (is.null(prob)) {
        ws <- c(list.map.internal(.data, substitute(weight), 
            parent.frame()), recursive = TRUE)
        if (any(ws < 0)) 
            stop("Negative weight is not allowed")
        prob <- ws/sum(ws)
    }
    sample(.data, size, replace, prob)
}


list.match <- function (.data, pattern, ...) 
{
    .data[grep(pattern, names(.data), ...)]
}


list.apply <- function (.data, .fun, ...) 
lapply(X = .data, FUN = .fun, ...)


list.any <- function (.data, cond, na.rm = FALSE) 
{
    if (missing(.data)) 
        return(any(na.rm = na.rm))
    if (is.empty(.data) || missing(cond)) 
        return(any(.data, na.rm = na.rm))
    res <- list.first.internal(.data, substitute(cond), parent.frame(), 
        na.rm = na.rm)
    res$state
}


list.do <- function (.data, fun, ...) 
{
    do.call(what = fun, args = as.list(.data), ...)
}


list.ungroup <- function (.data, level = 1L, ..., group.names = FALSE, sort.names = FALSE) 
{
    result <- .data
    for (i in seq_len(level)) {
        if (!group.names) 
            names(result) <- NULL
        result <- unlist(result, recursive = FALSE)
    }
    result.names <- names(result)
    if (sort.names && !is.null(result.names)) {
        result[sort(result.names)]
    }
    else {
        result
    }
}


list.group <- function (.data, ..., sorted = TRUE) 
{
    list.group.internal(.data, dots(...), parent.frame(), compare = "identical", 
        sorted = sorted)
}


list.iter <- function (.data, expr) 
{
    list.map.internal(.data, substitute(expr), parent.frame())
    invisible(.data)
}


list.is <- function (.data, cond, use.names = TRUE) 
{
    items <- list.is.internal(.data, substitute(cond), parent.frame())
    if (use.names) 
        setnames(items, names(.data))
    else items
}


list.maps <- function (expr, ...) 
{
    expr <- substitute(expr)
    envir <- parent.frame()
    lists <- list(...)
    if (is.empty(lists)) 
        return(list())
    list1 <- lists[[1L]]
    xnames <- getnames(list1, character(1L))
    fun <- with(envir, function(..., .expr) eval(.expr, list(...)))
    args <- c(lists, list(.i = seq_along(list1), .name = xnames, 
        .expr = list(expr)))
    map(fun, args)
}


list.names <- function (.data, expr) 
{
    if (missing(expr)) 
        return(names(.data))
    expr <- substitute(expr)
    if (is.null(expr)) 
        return(setnames(.data, NULL))
    values <- list.map.internal(.data, expr, parent.frame())
    setnames(.data, values)
}


list.class <- function (.data, ..., sorted = TRUE) 
{
    list.group.internal(.data, dots(...), parent.frame(), proc = "unlist", 
        compare = "contains", sorted = sorted)
}


tryGet <- function (symbol, def = NULL, ..., envir = parent.frame()) 
{
    symbol <- substitute(symbol)
    if (is.symbol(symbol)) 
        symbol <- as.character(symbol)
    if (is.character(symbol)) {
        if (exists(symbol, inherits = FALSE, ..., envir = envir)) 
            get(symbol, inherits = FALSE, ..., envir = envir)
        else def
    }
    else stop("symbol must be a name or character", call. = FALSE)
}


list.select <- function (.data, ...) 
{
    args <- set_argnames(dots(...))
    quote <- as.call(c(quote(list), args))
    list.map.internal(.data, quote, parent.frame())
}


list.load <- function (file, type = tools::file_ext(file), ..., guess = c("json", 
    "yaml", "rds", "rdata", "xml"), action = c("none", "merge", 
    "ungroup"), progress = length(file) >= 5L) 
{
    if (length(file) == 0L) 
        return(list())
    nztype <- !is.na(type) & nzchar(type)
    fun <- paste("list.loadfile", tolower(type), sep = ".")
    fun[!nztype] <- NA_character_
    guess <- tolower(guess)
    pb <- if (progress) 
        txtProgressBar(min = 0L, max = length(file), style = 3L)
    else NULL
    res <- if (length(file) == 1L) 
        list.loadfile(file, fun, guess, ..., pb = pb, index = 1L)
    else {
        items <- map(list.loadfile, list(file, fun, index = seq_along(file)), 
            list(guess = guess, ..., pb = pb))
        switch(match.arg(action), merge = do.call("list.merge", 
            items), ungroup = list.ungroup(items), items)
    }
    if (!is.null(pb)) 
        close(pb)
    res
}


list.rbind <- function (.data) 
{
    list.do(.data, "rbind")
}


list.first <- function (.data, cond) 
{
    if (is.empty(.data)) 
        return(NULL)
    if (missing(cond)) 
        return(.data[[1L]])
    res <- list.first.internal(.data, substitute(cond), parent.frame(), 
        na.rm = TRUE)
    res$value
}


list.expand <- function (...) 
{
    args <- list(...)
    expand_args <- lapply(args, seq_along)
    expand_df <- do.call(expand.grid, expand_args)
    .mapply(function(...) {
        mapply(`[[`, args, list(...), USE.NAMES = TRUE, SIMPLIFY = FALSE)
    }, expand_df, NULL)
}


list.extract <- .Primitive("[[")


list.takeWhile <- function (.data, cond) 
{
    args <- args_env(i = 0L)
    tryWithCondition(list.map.internal(.data, substitute(cond), 
        parent.frame(), list.while.fun, args), rlist.finished = NULL)
    .data[0L:args$i]
}


list.take <- function (.data, n, force = FALSE) 
{
    if (!is.numeric(n)) 
        stop("n must be numeric or integer", call. = FALSE)
    .data[0L:if (force) 
        n
    else min(length(.data), n)]
}


list.reverse <- function (.data) 
{
    .data[rev.default(seq_along(.data))]
}


list.save <- function (x, file, type = tools::file_ext(file), ...) 
{
    fun <- paste("list.savefile", tolower(type), sep = ".")
    if (exists(fun, mode = "function")) {
        fun <- get(fun, mode = "function")
        fun(x, file, ...)
    }
    else {
        stop("Unrecognized type of file: ", file, call. = FALSE)
    }
    invisible(x)
}


list.remove <- function (.data, range = integer()) 
{
    if (is.logical(range)) {
        .data[!range]
    }
    else if (is.numeric(range)) {
        .data[-range]
    }
    else if (is.character(range)) {
        names <- names(.data)
        m <- vapply(range, "==", logical(length(.data)), names)
        selector <- apply(m, 1L, any)
        .data[!selector]
    }
}


list.skipWhile <- function (.data, cond) 
{
    args <- args_env(i = 0L)
    tryWithCondition(list.map.internal(.data, substitute(cond), 
        parent.frame(), list.while.fun, args), rlist.finished = NULL)
    .data[-(0L:args$i)]
}


list.if <- function (.data, cond, use.names = TRUE) 
{
    items <- list.is.internal(.data, substitute(cond), parent.frame())
    if (use.names) 
        setnames(items, names(.data))
    else items
}


list.mapv <- function (.data, expr, as, use.names = TRUE) 
{
    res <- list.map.internal(.data, substitute(expr), parent.frame())
    if (missing(as)) 
        unlist(res, use.names = use.names)
    else {
        res <- as.vector(res, as)
        if (use.names && !is.null(nm <- names(.data))) 
            names(res) <- nm
        res
    }
}


list.which <- function (.data, cond) 
{
    which(list.is.internal(.data, substitute(cond), parent.frame()))
}


list.stack <- function (.data, ..., data.table = FALSE) 
{
    dt <- data.table::rbindlist(.data, ...)
    if (!data.table) 
        data.table::setDF(dt)
    dt
}


list.all <- function (.data, cond, na.rm = FALSE) 
{
    if (missing(.data)) 
        return(all(na.rm = na.rm))
    if (is.empty(.data) || missing(cond)) 
        return(all(.data, na.rm = na.rm))
    l <- lambda(substitute(cond))
    l$expr <- as.call(list(quote(`!`), l$expr))
    res <- list.first.internal(.data, l, parent.frame(), na.rm = na.rm)
    !res$state
}


list.flatten <- function (x, use.names = TRUE, classes = "ANY") 
{
    len <- sum(rapply(x, function(x) 1L, classes = classes))
    y <- vector("list", len)
    i <- 0L
    items <- rapply(x, function(x) {
        i <<- i + 1L
        y[[i]] <<- x
        TRUE
    }, classes = classes)
    if (use.names && !is.null(nm <- names(items))) 
        names(y) <- nm
    y
}




## Package Data

nyweather <- rlist::nyweather		## New York hourly weather data



## Package Info

.skeleton_package_title = "A Toolbox for Non-Tabular Data Manipulation"

.skeleton_package_version = "0.4.6.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "yaml,jsonlite,XML,data.table"


## Internal

.skeleton_version = 5


## EOF