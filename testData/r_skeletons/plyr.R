##
## Exported symobls in package `plyr`
##

## Exported package methods

aaply <- function (.data, .margins, .fun = NULL, ..., .expand = TRUE, 
    .progress = "none", .inform = FALSE, .drop = TRUE, .parallel = FALSE, 
    .paropts = NULL) 
{
    pieces <- splitter_a(.data, .margins, .expand)
    laply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .drop = .drop, .parallel = .parallel, 
        .paropts = .paropts)
}


progress_win <- function (title = "plyr progress", ...) 
{
    n <- 0
    win <- NULL
    list(init = function(x) {
        win <<- utils::winProgressBar(max = x, title = title, 
            ...)
        utils::setWinProgressBar(win, 0)
    }, step = function() {
        n <<- n + 1
        utils::setWinProgressBar(win, n)
    }, term = function() close(win))
}


join <- function (x, y, by = NULL, type = "left", match = "all") 
{
    type <- match.arg(type, c("left", "right", "inner", "full"))
    match <- match.arg(match, c("first", "all"))
    if (is.null(by)) {
        by <- intersect(names(x), names(y))
        message("Joining by: ", paste(by, collapse = ", "))
    }
    switch(match, first = .join_first(x, y, by, type), all = .join_all(x, 
        y, by, type))
}


failwith <- function (default = NULL, f, quiet = FALSE) 
{
    f <- match.fun(f)
    function(...) try_default(f(...), default, quiet = quiet)
}


progress_tk <- function (title = "plyr progress", label = "Working...", ...) 
{
    stopifnot(requireNamespace("tcltk", quietly = TRUE))
    n <- 0
    tk <- NULL
    list(init = function(x) {
        tk <<- tcltk::tkProgressBar(max = x, title = title, label = label, 
            ...)
        tcltk::setTkProgressBar(tk, 0)
    }, step = function() {
        n <<- n + 1
        tcltk::setTkProgressBar(tk, n)
    }, term = function() close(tk))
}


maply <- function (.data, .fun = NULL, ..., .expand = TRUE, .progress = "none", 
    .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL) 
{
    if (is.matrix(.data) & !is.list(.data)) 
        .data <- .matrix_to_df(.data)
    f <- splat(.fun)
    aaply(.data = .data, .margins = 1, .fun = f, ..., .expand = .expand, 
        .progress = .progress, .inform = .inform, .parallel = .parallel, 
        .paropts = .paropts, .drop = .drop)
}


amv_dimnames <- function (x) 
{
    d <- if (is.vector(x)) 
        list(names(x))
    else dimnames(x)
    if (is.null(d)) 
        d <- rep(list(NULL), dims(x))
    null_names <- which(unlist(llply(d, is.null)))
    d[null_names] <- llply(null_names, function(i) seq_len(amv_dim(x)[i]))
    d
}


split_indices <- function (group, n = 0L) 
{
    .Call("plyr_split_indices", PACKAGE = "plyr", group, n)
}


each <- function (...) 
{
    fnames <- laply(match.call()[-1], deparse)
    fs <- list(...)
    if (length(fs[[1]]) > 1) {
        fs <- fs[[1]]
        snames <- as.list(match.call()[2])[[1]]
        fnames <- unlist(lapply(as.list(snames)[-1], deparse))
    }
    char <- laply(fs, is.character)
    fnames[char] <- fs[char]
    fs[char] <- llply(fs[char], match.fun)
    unames <- names(fs)
    if (is.null(unames)) 
        unames <- fnames
    unames[unames == ""] <- fnames[unames == ""]
    n <- length(fs)
    if (n == 1) {
        function(x, ...) {
            res <- fs[[1]](x, ...)
            if (length(res) == 1) 
                names(res) <- unames
            res
        }
    }
    else {
        proto <- NULL
        result <- NULL
        function(x, ...) {
            if (is.null(proto)) {
                result <<- vector("list", length = n)
                names(result) <- unames
                for (i in 1:n) result[[i]] <- fs[[i]](x, ...)
                proto <<- list_to_vector(result)
            }
            else {
                for (i in 1:n) proto[[i]] <- fs[[i]](x, ...)
            }
            proto
        }
    }
}


is.formula <- function (x) 
inherits(x, "formula")


progress_none <- function () 
{
    list(init = function(x) NULL, step = function() NULL, term = function() NULL)
}


r_ply <- function (.n, .expr, .progress = "none", .print = FALSE) 
{
    .rlply_worker(.n, .progress, eval.parent(substitute(function() .expr)), 
        .discard = TRUE, .print = .print)
    invisible(NULL)
}


match_df <- function (x, y, on = NULL) 
{
    if (is.null(on)) {
        on <- intersect(names(x), names(y))
        message("Matching on: ", paste(on, collapse = ", "))
    }
    keys <- join.keys(x, y, on)
    x[keys$x %in% keys$y, , drop = FALSE]
}


name_rows <- function (df) 
{
    stopifnot(is.data.frame(df))
    rn_col <- !is.null(df$.rownames)
    if (rn_col) {
        rownames(df) <- df$.rownames
        df$.rownames <- NULL
    }
    else {
        df$.rownames <- rownames(df)
        rownames(df) <- NULL
    }
    df
}


take <- function (x, along, indices, drop = FALSE) 
{
    nd <- length(dim(x))
    index <- as.list(rep(TRUE, nd))
    index[along] <- indices
    eval(as.call(c(as.name("["), as.name("x"), index, drop = drop)))
}


liply <- function (.iterator, .fun = NULL, ...) 
{
    .Deprecated("llply")
    stopifnot(inherits(.iterator, "iter"))
    if (is.null(.fun)) 
        return(as.list(.iterator))
    iterator <- itertools::ihasNext(.iterator)
    if (is.character(.fun)) 
        .fun <- each(.fun)
    if (!is.function(.fun)) 
        stop(".fun is not a function.")
    result <- vector("list", 50)
    i <- 0
    while (itertools::hasNext(iterator)) {
        piece <- iterators::nextElem(iterator)
        res <- .fun(piece, ...)
        i <- i + 1
        if (i > length(result)) {
            length(result) <- length(result) * 2
        }
        if (!is.null(res)) 
            result[[i]] <- res
    }
    length(result) <- i
    result
}


numcolwise <- function (.fun, ...) 
{
    colwise(.fun, is.numeric, ...)
}


tryNULL <- function (expr) 
try_default(expr, NULL, quiet = TRUE)


id <- function (.variables, drop = FALSE) 
{
    lengths <- vapply(.variables, length, integer(1))
    .variables <- .variables[lengths != 0]
    if (length(.variables) == 0) {
        n <- nrow(.variables) %||% 0L
        return(structure(seq_len(n), n = n))
    }
    if (length(.variables) == 1) {
        return(id_var(.variables[[1]], drop = drop))
    }
    ids <- rev(lapply(.variables, id_var, drop = drop))
    p <- length(ids)
    ndistinct <- vapply(ids, attr, "n", FUN.VALUE = numeric(1), 
        USE.NAMES = FALSE)
    n <- prod(ndistinct)
    if (n > 2^31) {
        char_id <- do.call("paste", c(ids, sep = "\r"))
        res <- match(char_id, unique(char_id))
    }
    else {
        combs <- c(1, cumprod(ndistinct[-p]))
        mat <- do.call("cbind", ids)
        res <- c((mat - 1L) %*% combs + 1L)
    }
    attr(res, "n") <- n
    if (drop) {
        id_var(res, drop = TRUE)
    }
    else {
        structure(as.integer(res), n = attr(res, "n"))
    }
}


colwise <- function (.fun, .cols = true, ...) 
{
    if (!is.function(.cols)) {
        .cols <- as.quoted(.cols)
        filter <- function(df) eval.quoted(.cols, df)
    }
    else {
        filter <- function(df) Filter(.cols, df)
    }
    dots <- list(...)
    function(df, ...) {
        stopifnot(is.data.frame(df))
        df <- strip_splits(df)
        filtered <- filter(df)
        if (length(filtered) == 0) 
            return(data.frame())
        out <- do.call("lapply", c(list(filtered, .fun, ...), 
            dots))
        names(out) <- names(filtered)
        quickdf(out)
    }
}


daply <- function (.data, .variables, .fun = NULL, ..., .progress = "none", 
    .inform = FALSE, .drop_i = TRUE, .drop_o = TRUE, .parallel = FALSE, 
    .paropts = NULL) 
{
    .variables <- as.quoted(.variables)
    pieces <- splitter_d(.data, .variables, drop = .drop_i)
    laply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .drop = .drop_o, .parallel = .parallel, 
        .paropts = .paropts)
}


mapvalues <- function (x, from, to, warn_missing = TRUE) 
{
    if (length(from) != length(to)) {
        stop("`from` and `to` vectors are not the same length.")
    }
    if (!is.atomic(x)) {
        stop("`x` must be an atomic vector.")
    }
    if (is.factor(x)) {
        levels(x) <- mapvalues(levels(x), from, to, warn_missing)
        return(x)
    }
    mapidx <- match(x, from)
    mapidxNA <- is.na(mapidx)
    from_found <- sort(unique(mapidx))
    if (warn_missing && length(from_found) != length(from)) {
        message("The following `from` values were not present in `x`: ", 
            paste(from[!(1:length(from) %in% from_found)], collapse = ", "))
    }
    x[!mapidxNA] <- to[mapidx[!mapidxNA]]
    x
}


a_ply <- function (.data, .margins, .fun = NULL, ..., .expand = TRUE, 
    .progress = "none", .inform = FALSE, .print = FALSE, .parallel = FALSE, 
    .paropts = NULL) 
{
    pieces <- splitter_a(.data, .margins, .expand)
    l_ply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .print = .print, .parallel = .parallel, 
        .paropts = .paropts)
}


rlply <- function (.n, .expr, .progress = "none") 
{
    res <- .rlply_worker(.n, .progress, eval.parent(substitute(function() .expr)))
    res
}


laply <- function (.data, .fun = NULL, ..., .progress = "none", .inform = FALSE, 
    .drop = TRUE, .parallel = FALSE, .paropts = NULL) 
{
    if (is.character(.fun)) 
        .fun <- do.call("each", as.list(.fun))
    if (!is.function(.fun)) 
        stop(".fun is not a function.")
    if (!inherits(.data, "split")) 
        .data <- as.list(.data)
    res <- llply(.data = .data, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts)
    list_to_array(res, attr(.data, "split_labels"), .drop)
}


unrowname <- function (x) 
{
    rownames(x) <- NULL
    x
}


idata.frame <- function (df) 
{
    self <- new.env()
    self$`_data` <- df
    self$`_rows` <- seq_len(nrow(df))
    self$`_cols` <- names(df)
    self$`_getters` <- lapply(names(df), function(name) {
        eval(substitute(function(v) {
            if (missing(v)) {
                `_data`[[name]][`_rows`]
            } else {
                stop("Immutable")
            }
        }, list(name = name)), envir = self)
    })
    names(self$`_getters`) <- names(df)
    for (name in names(df)) {
        f <- self$`_getters`[[name]]
        environment(f) <- self
        makeActiveBinding(name, f, self)
    }
    structure(self, class = c("idf", "environment"))
}


true <- function (...) 
TRUE


defaults <- function (x, y) 
{
    c(x, y[setdiff(names(y), names(x))])
}


m_ply <- function (.data, .fun = NULL, ..., .expand = TRUE, .progress = "none", 
    .inform = FALSE, .print = FALSE, .parallel = FALSE, .paropts = NULL) 
{
    if (is.matrix(.data) & !is.list(.data)) 
        .data <- .matrix_to_df(.data)
    f <- splat(.fun)
    a_ply(.data = .data, .margins = 1, .fun = f, ..., .expand = .expand, 
        .progress = .progress, .inform = .inform, .print = .print, 
        .parallel = .parallel, .paropts = .paropts)
}


splat <- function (flat) 
{
    function(args, ...) {
        do.call(flat, c(args, list(...)))
    }
}


summarize <- function (.data, ...) 
{
    stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
    cols <- as.list(substitute(list(...))[-1])
    if (is.null(names(cols))) {
        missing_names <- rep(TRUE, length(cols))
    }
    else {
        missing_names <- names(cols) == ""
    }
    if (any(missing_names)) {
        names <- unname(unlist(lapply(match.call(expand.dots = FALSE)$..., 
            deparse)))
        names(cols)[missing_names] <- names[missing_names]
    }
    .data <- as.list(.data)
    for (col in names(cols)) {
        .data[[col]] <- eval(cols[[col]], .data, parent.frame())
    }
    quickdf(.data[names(cols)])
}


round_any <- function (x, accuracy, f = round) 
{
    UseMethod("round_any")
}


try_default <- function (expr, default, quiet = FALSE) 
{
    result <- default
    if (quiet) {
        tryCatch(result <- expr, error = function(e) NULL)
    }
    else {
        try(result <- expr)
    }
    result
}


rdply <- function (.n, .expr, .progress = "none", .id = NA) 
{
    res <- .rlply_worker(.n, .progress, eval.parent(substitute(function() .expr)))
    names(res) <- seq_len(.n)
    if (is.null(.id)) {
        labels <- NULL
    }
    else {
        labels <- data.frame(.n = seq_len(.n))
        if (!is.na(.id)) {
            names(labels) <- .id
        }
    }
    list_to_dataframe(res, labels)
}


alply <- function (.data, .margins, .fun = NULL, ..., .expand = TRUE, 
    .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, 
    .dims = FALSE) 
{
    pieces <- splitter_a(.data, .margins, .expand)
    res <- llply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts)
    if (.dims) {
        labels <- attr(pieces, "split_labels")
        if (length(labels) == length(.margins)) {
            res_labels <- lapply(labels, function(x) as.character(unique(x)))
            res_dim <- sapply(res_labels, length)
            if (length(res_dim) > 0) {
                dim(res) <- res_dim
                dimnames(res) <- res_labels
            }
        }
    }
    res
}


rbind.fill.matrix <- function (...) 
{
    matrices <- list(...)
    if (length(matrices) == 0) 
        return()
    if (is.list(matrices[[1]]) && !is.matrix(matrices[[1]])) {
        matrices <- matrices[[1]]
    }
    tmp <- unlist(lapply(matrices, is.factor))
    if (any(tmp)) {
        stop("Input ", paste(which(tmp), collapse = ", "), " is a factor and ", 
            "needs to be converted first to either numeric or character.")
    }
    matrices[] <- lapply(matrices, as.matrix)
    lcols <- lapply(matrices, function(x) amv_dimnames(x)[[2]])
    cols <- unique(unlist(lcols))
    rows <- unlist(lapply(matrices, nrow))
    nrows <- sum(rows)
    output <- matrix(NA, nrow = nrows, ncol = length(cols))
    colnames(output) <- cols
    pos <- matrix(c(cumsum(rows) - rows + 1, rows), ncol = 2)
    for (i in seq_along(rows)) {
        rng <- seq(pos[i, 1], length.out = pos[i, 2])
        output[rng, lcols[[i]]] <- matrices[[i]]
    }
    output
}


count <- function (df, vars = NULL, wt_var = NULL) 
{
    if (is.atomic(df)) {
        df <- data.frame(x = df)
    }
    if (!is.null(vars)) {
        vars <- as.quoted(vars)
        df2 <- quickdf(eval.quoted(vars, df))
    }
    else {
        df2 <- df
    }
    id <- ninteraction(df2, drop = TRUE)
    u_id <- !duplicated(id)
    labels <- df2[u_id, , drop = FALSE]
    labels <- labels[order(id[u_id]), , drop = FALSE]
    if (is.null(wt_var) && "freq" %in% names(df)) {
        message("Using freq as weighting variable")
        wt_var <- "freq"
    }
    if (!is.null(wt_var)) {
        wt_var <- as.quoted(wt_var)
        if (length(wt_var) > 1) {
            stop("wt_var must be a single variable", call. = FALSE)
        }
        wt <- eval.quoted(wt_var, df)[[1]]
        freq <- vaggregate(wt, id, sum, .default = 0)
    }
    else {
        freq <- tabulate(id, attr(id, "n"))
    }
    unrowname(data.frame(labels, freq))
}


mlply <- function (.data, .fun = NULL, ..., .expand = TRUE, .progress = "none", 
    .inform = FALSE, .parallel = FALSE, .paropts = NULL) 
{
    if (is.matrix(.data) & !is.list(.data)) 
        .data <- .matrix_to_df(.data)
    f <- splat(.fun)
    alply(.data = .data, .margins = 1, .fun = f, ..., .expand = .expand, 
        .progress = .progress, .inform = .inform, .parallel = .parallel, 
        .paropts = .paropts)
}


d_ply <- function (.data, .variables, .fun = NULL, ..., .progress = "none", 
    .inform = FALSE, .drop = TRUE, .print = FALSE, .parallel = FALSE, 
    .paropts = NULL) 
{
    .variables <- as.quoted(.variables)
    pieces <- splitter_d(.data, .variables, drop = .drop)
    l_ply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .print = .print, .parallel = .parallel, 
        .paropts = .paropts)
}


catcolwise <- function (.fun, ...) 
{
    colwise(.fun, is.discrete, ...)
}


tryapply <- function (list, fun, ...) 
{
    compact(lapply(list, function(x) tryNULL(fun(x, ...))))
}


isplit2 <- function (x, f, drop = FALSE, ...) 
{
    .Deprecated(c("splitter_d", "splitter_a"))
    it <- iterators::isplit(seq_len(nrow(x)), f, drop = drop, 
        ...)
    nextEl <- function() {
        i <- iterators::nextElem(it)
        x[i$value, , drop = FALSE]
    }
    structure(list(nextElem = nextEl), class = c("abstractiter", 
        "iter"))
}


join_all <- function (dfs, by = NULL, type = "left", match = "all") 
{
    if (length(dfs) == 1) 
        return(dfs[[1]])
    joined <- dfs[[1]]
    for (i in 2:length(dfs)) {
        joined <- join(joined, dfs[[i]], by = by, type = type, 
            match = match)
    }
    joined
}


l_ply <- function (.data, .fun = NULL, ..., .progress = "none", .inform = FALSE, 
    .print = FALSE, .parallel = FALSE, .paropts = NULL) 
{
    if (is.character(.fun) || is.list(.fun)) 
        .fun <- each(.fun)
    if (!is.function(.fun)) 
        stop(".fun is not a function.")
    pieces <- as.list(.data)
    n <- length(pieces)
    if (n == 0) 
        return(invisible())
    if (.parallel && .progress != "none") {
        message("Progress disabled when using parallel plyr")
        .progress <- "none"
    }
    progress <- create_progress_bar(.progress)
    progress$init(n)
    on.exit(progress$term())
    if (.parallel && .print) {
        message("Printing disabled for parallel processing")
        .print <- FALSE
    }
    do.ply <- function(i) {
        piece <- pieces[[i]]
        if (.inform) {
            res <- try(.fun(piece, ...))
            if (inherits(res, "try-error")) {
                piece <- paste(utils::capture.output(print(piece)), 
                  collapse = "\n")
                stop("with piece ", i, ": \n", piece, call. = FALSE)
            }
        }
        else {
            res <- .fun(piece, ...)
        }
        if (.print) {
            print(res)
        }
        progress$step()
    }
    if (.parallel) {
        setup_parallel()
        .paropts$.combine <- function(...) NULL
        i <- seq_len(n)
        fe_call <- as.call(c(list(quote(foreach::foreach), i = i), 
            .paropts))
        fe <- eval(fe_call)
        foreach::`%dopar%`(fe, do.ply(i))
    }
    else {
        for (i in seq_len(n)) {
            do.ply(i)
        }
    }
    invisible()
}


empty <- function (df) 
{
    (is.null(df) || nrow(df) == 0 || ncol(df) == 0)
}


adply <- function (.data, .margins, .fun = NULL, ..., .expand = TRUE, 
    .progress = "none", .inform = FALSE, .parallel = FALSE, .paropts = NULL, 
    .id = NA) 
{
    pieces <- splitter_a(.data, .margins, .expand, .id)
    .id <- NA
    if (is.null(attr(pieces, "split_labels"))) {
        .id <- NULL
    }
    ldply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts, 
        .id = .id)
}


desc <- function (x) 
-xtfrm(x)


progress_text <- function (style = 3, ...) 
{
    n <- 0
    txt <- NULL
    list(init = function(x) {
        txt <<- utils::txtProgressBar(max = x, style = style, 
            ...)
        utils::setTxtProgressBar(txt, 0)
    }, step = function() {
        n <<- n + 1
        utils::setTxtProgressBar(txt, n)
    }, term = function() close(txt))
}


vaggregate <- function (.value, .group, .fun, ..., .default = NULL, .n = nlevels(.group)) 
{
    if (!is.integer(.group)) {
        if (is.list(.group)) {
            .group <- id(.group)
        }
        else {
            .group <- id(list(.group))
        }
    }
    if (is.null(.default)) {
        .default <- .fun(.value[0], ...)
    }
    fun <- function(i) {
        if (length(i) == 0) 
            return(.default)
        .fun(.value[i], ...)
    }
    indices <- split_indices(.group, .n)
    vapply(indices, fun, .default)
}


join.keys <- function (x, y, by) 
{
    joint <- rbind.fill(x[by], y[by])
    keys <- id(joint, drop = TRUE)
    n_x <- nrow(x)
    n_y <- nrow(y)
    list(x = keys[seq_len(n_x)], y = keys[n_x + seq_len(n_y)], 
        n = attr(keys, "n"))
}


mutate <- function (.data, ...) 
{
    stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
    cols <- as.list(substitute(list(...))[-1])
    cols <- cols[names(cols) != ""]
    for (col in names(cols)) {
        .data[[col]] <- eval(cols[[col]], .data, parent.frame())
    }
    .data
}


create_progress_bar <- function (name = "none", ...) 
{
    if (!is.character(name)) 
        return(name)
    name <- paste("progress", name, sep = "_")
    if (!exists(name, mode = "function")) {
        warning("Cannot find progress bar ", name, call. = FALSE)
        progress_none()
    }
    else {
        match.fun(name)(...)
    }
}


mdply <- function (.data, .fun = NULL, ..., .expand = TRUE, .progress = "none", 
    .inform = FALSE, .parallel = FALSE, .paropts = NULL) 
{
    if (is.matrix(.data) & !is.list(.data)) 
        .data <- .matrix_to_df(.data)
    f <- splat(.fun)
    adply(.data = .data, .margins = 1, .fun = f, ..., .expand = .expand, 
        .progress = .progress, .inform = .inform, .parallel = .parallel, 
        .paropts = .paropts)
}


arrange <- function (df, ...) 
{
    stopifnot(is.data.frame(df))
    ord <- eval(substitute(order(...)), df, parent.frame())
    if (length(ord) != nrow(df)) {
        stop("Length of ordering vectors don't match data frame size", 
            call. = FALSE)
    }
    unrowname(df[ord, , drop = FALSE])
}


revalue <- function (x, replace = NULL, warn_missing = TRUE) 
{
    if (!is.null(x) && !is.factor(x) && !is.character(x)) {
        stop("x is not a factor or a character vector.")
    }
    mapvalues(x, from = names(replace), to = replace, warn_missing = warn_missing)
}


progress_time <- function () 
{
    n <- 0
    txt <- NULL
    list(init = function(x) {
        txt <<- txtTimerBar(x)
        utils::setTxtProgressBar(txt, 0)
    }, step = function() {
        n <<- n + 1
        utils::setTxtProgressBar(txt, n)
    }, term = function() close(txt))
}


strip_splits <- function (df) 
{
    df[setdiff(names(df), attr(df, "vars"))]
}


compact <- function (l) 
Filter(Negate(is.null), l)


is.quoted <- function (x) 
inherits(x, "quoted")


dlply <- function (.data, .variables, .fun = NULL, ..., .progress = "none", 
    .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL) 
{
    .variables <- as.quoted(.variables)
    pieces <- splitter_d(.data, .variables, drop = .drop)
    llply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts)
}


llply <- function (.data, .fun = NULL, ..., .progress = "none", .inform = FALSE, 
    .parallel = FALSE, .paropts = NULL) 
{
    if (is.null(.fun)) 
        return(as.list(.data))
    if (is.character(.fun) || is.list(.fun)) 
        .fun <- each(.fun)
    if (!is.function(.fun)) 
        stop(".fun is not a function.")
    if (!inherits(.data, "split")) {
        pieces <- as.list(.data)
        fast_path <- .progress == "none" && !.inform && !.parallel
        if (fast_path) {
            return(structure(lapply(pieces, .fun, ...), dim = dim(pieces)))
        }
    }
    else {
        pieces <- .data
    }
    n <- length(pieces)
    if (n == 0) 
        return(list())
    if (.parallel && .progress != "none") {
        message("Progress disabled when using parallel plyr")
        .progress <- "none"
    }
    progress <- create_progress_bar(.progress)
    progress$init(n)
    on.exit(progress$term())
    result <- vector("list", n)
    do.ply <- function(i) {
        piece <- pieces[[i]]
        if (.inform) {
            res <- try(.fun(piece, ...))
            if (inherits(res, "try-error")) {
                piece <- paste(utils::capture.output(print(piece)), 
                  collapse = "\n")
                stop("with piece ", i, ": \n", piece, call. = FALSE)
            }
        }
        else {
            res <- .fun(piece, ...)
        }
        progress$step()
        res
    }
    if (.parallel) {
        setup_parallel()
        i <- seq_len(n)
        fe_call <- as.call(c(list(quote(foreach::foreach), i = i), 
            .paropts))
        fe <- eval(fe_call)
        result <- foreach::`%dopar%`(fe, do.ply(i))
    }
    else {
        result <- loop_apply(n, do.ply)
    }
    attributes(result)[c("split_type", "split_labels")] <- attributes(pieces)[c("split_type", 
        "split_labels")]
    names(result) <- names(pieces)
    if (!is.null(dim(pieces))) {
        dim(result) <- dim(pieces)
    }
    result
}


rename <- function (x, replace, warn_missing = TRUE, warn_duplicated = TRUE) 
{
    names(x) <- revalue(names(x), replace, warn_missing = warn_missing)
    duplicated_names <- names(x)[duplicated(names(x))]
    if (warn_duplicated && (length(duplicated_names) > 0L)) {
        duplicated_names_message <- paste0("`", duplicated_names, 
            "`", collapse = ", ")
        warning("The plyr::rename operation has created duplicates for the ", 
            "following name(s): (", duplicated_names_message, 
            ")", call. = FALSE)
    }
    x
}


here <- function (f) 
{
    call <- substitute(function(...) (f)(...), list(f = f))
    fun <- eval(call, parent.frame())
    attr(fun, "srcref") <- srcfilecopy("<text>", deparse(call))
    fun
}


. <- function (..., .env = parent.frame()) 
{
    structure(as.list(match.call()[-1]), env = .env, class = "quoted")
}


eval.quoted <- function (exprs, envir = NULL, enclos = NULL, try = FALSE) 
{
    if (is.numeric(exprs)) 
        return(envir[exprs])
    if (!is.null(envir) && !is.list(envir) && !is.environment(envir)) {
        stop("envir must be either NULL, a list, or an environment.")
    }
    qenv <- if (is.quoted(exprs)) 
        attr(exprs, "env")
    else parent.frame()
    if (is.null(envir)) 
        envir <- qenv
    if (is.data.frame(envir) && is.null(enclos)) 
        enclos <- qenv
    if (try) {
        results <- lapply(exprs, failwith(NULL, eval, quiet = TRUE), 
            envir = envir, enclos = enclos)
    }
    else {
        results <- lapply(exprs, eval, envir = envir, enclos = enclos)
    }
    names(results) <- names(exprs)
    results
}


raply <- function (.n, .expr, .progress = "none", .drop = TRUE) 
{
    res <- .rlply_worker(.n, .progress, eval.parent(substitute(function() .expr)))
    list_to_array(res, NULL, .drop)
}


is.discrete <- function (x) 
is.factor(x) || is.character(x) || is.logical(x)


ddply <- function (.data, .variables, .fun = NULL, ..., .progress = "none", 
    .inform = FALSE, .drop = TRUE, .parallel = FALSE, .paropts = NULL) 
{
    if (empty(.data)) 
        return(.data)
    .variables <- as.quoted(.variables)
    pieces <- splitter_d(.data, .variables, drop = .drop)
    ldply(.data = pieces, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts)
}


split_labels <- function (splits, drop, id = plyr::id(splits, drop = TRUE)) 
{
    if (length(splits) == 0) 
        return(data.frame())
    if (drop) {
        representative <- which(!duplicated(id))[order(unique(id))]
        quickdf(lapply(splits, function(x) x[representative]))
    }
    else {
        unique_values <- llply(splits, ulevels)
        names(unique_values) <- names(splits)
        rev(expand.grid(rev(unique_values), stringsAsFactors = FALSE))
    }
}


summarise <- function (.data, ...) 
{
    stopifnot(is.data.frame(.data) || is.list(.data) || is.environment(.data))
    cols <- as.list(substitute(list(...))[-1])
    if (is.null(names(cols))) {
        missing_names <- rep(TRUE, length(cols))
    }
    else {
        missing_names <- names(cols) == ""
    }
    if (any(missing_names)) {
        names <- unname(unlist(lapply(match.call(expand.dots = FALSE)$..., 
            deparse)))
        names(cols)[missing_names] <- names[missing_names]
    }
    .data <- as.list(.data)
    for (col in names(cols)) {
        .data[[col]] <- eval(cols[[col]], .data, parent.frame())
    }
    quickdf(.data[names(cols)])
}


rbind.fill <- function (...) 
{
    dfs <- list(...)
    if (length(dfs) == 0) 
        return()
    if (is.list(dfs[[1]]) && !is.data.frame(dfs[[1]])) {
        dfs <- dfs[[1]]
    }
    dfs <- compact(dfs)
    if (length(dfs) == 0) 
        return()
    if (length(dfs) == 1) 
        return(dfs[[1]])
    is_df <- vapply(dfs, is.data.frame, logical(1))
    if (any(!is_df)) {
        stop("All inputs to rbind.fill must be data.frames", 
            call. = FALSE)
    }
    rows <- unlist(lapply(dfs, .row_names_info, 2L))
    nrows <- sum(rows)
    ot <- output_template(dfs, nrows)
    setters <- ot$setters
    getters <- ot$getters
    if (length(setters) == 0) {
        return(as.data.frame(matrix(nrow = nrows, ncol = 0)))
    }
    pos <- matrix(c(cumsum(rows) - rows + 1, rows), ncol = 2)
    for (i in seq_along(rows)) {
        rng <- seq(pos[i, 1], length.out = pos[i, 2])
        df <- dfs[[i]]
        for (var in names(df)) {
            setters[[var]](rng, df[[var]])
        }
    }
    quickdf(lapply(getters, function(x) x()))
}


ldply <- function (.data, .fun = NULL, ..., .progress = "none", .inform = FALSE, 
    .parallel = FALSE, .paropts = NULL, .id = NA) 
{
    if (!inherits(.data, "split")) 
        .data <- as.list(.data)
    res <- llply(.data = .data, .fun = .fun, ..., .progress = .progress, 
        .inform = .inform, .parallel = .parallel, .paropts = .paropts)
    if (identical(.id, NA)) {
        .id <- ".id"
        id_as_factor <- FALSE
    }
    else {
        id_as_factor <- TRUE
    }
    list_to_dataframe(res, attr(.data, "split_labels"), .id, 
        id_as_factor)
}


quickdf <- function (list) 
{
    rows <- unique(unlist(lapply(list, NROW)))
    stopifnot(length(rows) == 1)
    names(list) <- make_names(list, "X")
    class(list) <- "data.frame"
    attr(list, "row.names") <- c(NA_integer_, -rows)
    list
}


as.quoted <- function (x, env = parent.frame()) 
UseMethod("as.quoted")




## Package Data

baseball <- plyr::baseball		## Yearly batting records for all major league baseball players

ozone <- plyr::ozone		## Monthly ozone measurements over Central America.



## Package Info

.skeleton_package_title = "Tools for Splitting, Applying and Combining Data"

.skeleton_package_version = "1.8.4"

.skeleton_package_depends = ""

.skeleton_package_imports = "Rcpp"


## Internal

.skeleton_version = 5


## EOF