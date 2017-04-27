##
## Exported symobls in package `data.table`
##

## Exported package methods

hour <- function (x) 
as.POSIXlt(x)$hour


shift <- function (x, n = 1L, fill = NA, type = c("lag", "lead"), give.names = FALSE) 
{
    type = match.arg(type)
    ans = .Call(Cshift, x, as.integer(n), fill, type)
    if (give.names) {
        if (is.null(names(x))) {
            xsub = substitute(x)
            if (is.atomic(x) && is.name(xsub)) 
                nx = deparse(xsub, 500L)
            else nx = paste("V", if (is.atomic(x)) 
                1L
            else seq_along(x), sep = "")
        }
        else nx = names(x)
        setattr(ans, "names", do.call("paste", c(CJ(nx, type, 
            n, sorted = FALSE), sep = "_")))
    }
    ans
}


is.data.table <- function (x) 
inherits(x, "data.table")


dcast.data.table <- function (data, formula, fun.aggregate = NULL, sep = "_", ..., 
    margins = NULL, subset = NULL, fill = NULL, drop = TRUE, 
    value.var = guess(data), verbose = getOption("datatable.verbose")) 
{
    if (!is.data.table(data)) 
        stop("'data' must be a data.table.")
    drop = as.logical(rep(drop, length.out = 2L))
    if (any(is.na(drop))) 
        stop("'drop' must be logical TRUE/FALSE")
    lvals = value_vars(value.var, names(data))
    valnames = unique(unlist(lvals))
    lvars = check_formula(formula, names(data), valnames)
    lvars = lapply(lvars, function(x) if (!length(x)) 
        quote(.)
    else x)
    allcols = c(unlist(lvars), lapply(valnames, as.name))
    dat = vector("list", length(allcols))
    for (i in seq_along(allcols)) {
        x = allcols[[i]]
        dat[[i]] = if (identical(x, quote(.))) 
            rep(".", nrow(data))
        else eval(x, data, parent.frame())
        if (is.function(dat[[i]])) 
            stop("Column [", deparse(x), "] not found or of unknown type.")
    }
    setattr(lvars, "names", c("lhs", "rhs"))
    varnames = make.unique(sapply(unlist(lvars), all.vars, max.names = 1L), 
        sep = sep)
    dupidx = which(valnames %in% varnames)
    if (length(dupidx)) {
        dups = valnames[dupidx]
        valnames = tail(make.unique(c(varnames, valnames)), -length(varnames))
        lvals = lapply(lvals, function(x) {
            x[x %in% dups] = valnames[dupidx]
            x
        })
    }
    lhsnames = head(varnames, length(lvars$lhs))
    rhsnames = tail(varnames, -length(lvars$lhs))
    setattr(dat, "names", c(varnames, valnames))
    setDT(dat)
    if (any(sapply(as.list(dat)[varnames], is.list))) {
        stop("Columns specified in formula can not be of type list")
    }
    m <- as.list(match.call()[-1L])
    subset <- m[["subset"]][[2L]]
    if (!is.null(subset)) {
        if (is.name(subset)) 
            subset = as.call(list(quote(`(`), subset))
        idx = which(eval(subset, data, parent.frame()))
        dat = .Call(CsubsetDT, dat, idx, seq_along(dat))
    }
    if (!nrow(dat) || !ncol(dat)) 
        stop("Can not cast an empty data.table")
    fun.call = m[["fun.aggregate"]]
    fill.default = NULL
    if (is.null(fun.call)) {
        oo = forderv(dat, by = varnames, retGrp = TRUE)
        if (attr(oo, "maxgrpn") > 1L) {
            message("Aggregate function missing, defaulting to 'length'")
            fun.call = quote(length)
        }
    }
    if (!is.null(fun.call)) {
        fun.call = aggregate_funs(fun.call, lvals, sep, ...)
        errmsg = "Aggregating function(s) should take vector inputs and return a single value (length=1). However, function(s) returns length!=1. This value will have to be used to fill any missing combinations, and therefore must be length=1. Either override by setting the 'fill' argument explicitly or modify your function to handle this case appropriately."
        if (is.null(fill)) {
            fill.default <- suppressWarnings(dat[0][, eval(fun.call)])
            if (nrow(fill.default) != 1L) 
                stop(errmsg, call. = FALSE)
        }
        if (!any(valnames %chin% varnames)) {
            dat = dat[, eval(fun.call), by = c(varnames)]
        }
        else {
            dat = dat[, {
                .SD
                eval(fun.call)
            }, by = c(varnames), .SDcols = valnames]
        }
    }
    order_ <- function(x) {
        o = forderv(x, retGrp = TRUE, sort = TRUE)
        idx = attr(o, "starts")
        if (!length(o)) 
            o = seq_along(x)
        o[idx]
    }
    cj_uniq <- function(DT) {
        do.call("CJ", lapply(DT, function(x) if (is.factor(x)) {
            xint = seq_along(levels(x))
            setattr(xint, "levels", levels(x))
            setattr(xint, "class", class(x))
        }
        else .Call(CsubsetVector, x, order_(x))))
    }
    valnames = setdiff(names(dat), varnames)
    if (!is.null(fun.call) || !is.null(subset)) 
        setkeyv(dat, varnames)
    if (length(rhsnames)) {
        lhs = shallow(dat, lhsnames)
        rhs = shallow(dat, rhsnames)
        val = shallow(dat, valnames)
        if (all(drop)) {
            map = setDT(lapply(list(lhsnames, rhsnames), function(cols) frankv(dat, 
                cols = cols, ties.method = "dense")))
            maporder = lapply(map, order_)
            mapunique = lapply(seq_along(map), function(i) .Call(CsubsetVector, 
                map[[i]], maporder[[i]]))
            lhs = .Call(CsubsetDT, lhs, maporder[[1L]], seq_along(lhs))
            rhs = .Call(CsubsetDT, rhs, maporder[[2L]], seq_along(rhs))
        }
        else {
            lhs_ = if (!drop[1L]) 
                cj_uniq(lhs)
            else setkey(unique(lhs, by = names(lhs)))
            rhs_ = if (!drop[2L]) 
                cj_uniq(rhs)
            else setkey(unique(rhs, by = names(rhs)))
            map = vector("list", 2L)
            .Call(Csetlistelt, map, 1L, lhs_[lhs, which = TRUE])
            .Call(Csetlistelt, map, 2L, rhs_[rhs, which = TRUE])
            setDT(map)
            mapunique = vector("list", 2L)
            .Call(Csetlistelt, mapunique, 1L, seq_len(nrow(lhs_)))
            .Call(Csetlistelt, mapunique, 2L, seq_len(nrow(rhs_)))
            lhs = lhs_
            rhs = rhs_
        }
        maplen = sapply(mapunique, length)
        idx = do.call("CJ", mapunique)[map, `:=`(I, .I)][["I"]]
        ans = .Call(Cfcast, lhs, val, maplen[[1L]], maplen[[2L]], 
            idx, fill, fill.default, is.null(fun.call))
        allcols = do.call("paste", c(rhs, sep = sep))
        if (length(valnames) > 1L) 
            allcols = do.call("paste", if (identical(".", allcols)) 
                list(valnames, sep = sep)
            else c(CJ(valnames, allcols, sorted = FALSE), sep = sep))
        setattr(ans, "names", c(lhsnames, allcols))
        setDT(ans)
        setattr(ans, "sorted", lhsnames)
    }
    else {
        if (drop) {
            if (is.null(subset) && is.null(fun.call)) {
                dat = copy(dat)
                setkeyv(dat, lhsnames)
            }
            ans = dat
        }
        else {
            lhs = shallow(dat, lhsnames)
            val = shallow(dat, valnames)
            lhs_ = cj_uniq(lhs)
            idx = lhs_[lhs, `:=`(I, .I)][["I"]]
            lhs_[, `:=`(I, NULL)]
            ans = .Call(Cfcast, lhs_, val, nrow(lhs_), 1L, idx, 
                fill, fill.default, is.null(fun.call))
            setDT(ans)
            setattr(ans, "sorted", lhsnames)
            setnames(ans, c(lhsnames, valnames))
        }
        if (length(valnames) == 1L) 
            setnames(ans, valnames, value.var)
    }
    return(ans)
}


rleid <- function (..., prefix = NULL) 
{
    rleidv(list(...), prefix = prefix)
}


chgroup <- function (x) 
{
    o = forderv(x, sort = FALSE, retGrp = TRUE)
    if (length(o)) 
        as.vector(o)
    else seq_along(x)
}


setindexv <- function (...) 
setkeyv(..., physical = FALSE)


copy <- function (x) 
{
    newx = .Call(Ccopy, x)
    if (!is.data.table(x)) {
        if (is.list(x)) {
            anydt = vapply(x, is.data.table, TRUE, USE.NAMES = FALSE)
            if (sum(anydt)) {
                newx[anydt] = lapply(newx[anydt], function(x) {
                  setattr(x, ".data.table.locked", NULL)
                  alloc.col(x)
                })
            }
        }
        return(newx)
    }
    setattr(newx, ".data.table.locked", NULL)
    alloc.col(newx)
}


as.chron.IDate <- function (x, time = NULL, ...) 
{
    if (!requireNamespace("chron", quietly = TRUE)) 
        stop("Install suggested `chron` package to use `as.chron.IDate` function.")
    else {
        if (!is.null(time)) {
            chron::chron(dates. = chron::as.chron(as.Date(x)), 
                times. = chron::as.chron(time))
        }
        else {
            chron::chron(dates. = chron::as.chron(as.Date(x)))
        }
    }
}


wday <- function (x) 
as.POSIXlt(x)$wday + 1L


shouldPrint <- function (x) 
{
    ret = (.global$print == "" || address(x) != .global$print)
    .global$print = ""
    ret
}


.GRP <- NULL


fsort <- function (x, decreasing = FALSE, na.last = FALSE, internal = FALSE, 
    verbose = FALSE, ...) 
{
    if (typeof(x) == "double" && !decreasing && !na.last) {
        if (internal) 
            stop("Internal code should not be being called on type double")
        return(.Call(Cfsort, x, verbose))
    }
    else {
        if (!internal) 
            warning("Input is not a vector of type double. New parallel sort has only been done for double vectors so far. Invoking relatively inefficient sort using order first.")
        o = forderv(x, order = !decreasing, na.last = na.last)
        return(if (length(o)) x[o] else x)
    }
}


rbindlist <- function (l, use.names = fill, fill = FALSE, idcol = NULL) 
{
    if (identical(idcol, FALSE)) 
        idcol = NULL
    else if (!is.null(idcol)) {
        if (isTRUE(idcol)) 
            idcol = ".id"
        if (!is.character(idcol)) 
            stop("idcol must be a logical or character vector of length 1. If logical TRUE the id column will named '.id'.")
        idcol = idcol[1L]
    }
    ans = .Call(Crbindlist, l, use.names, fill, idcol)
    if (!length(ans)) 
        return(null.data.table())
    setDT(ans)[]
}


.__C__data.table <- new("classRepresentation"
    , slots = structure(list(.Data = structure("list", package = "methods"), 
    names = structure("character", package = "methods"), row.names = structure("data.frameRowLabels", package = "methods"), 
    .S3Class = structure("character", package = "methods")), .Names = c(".Data", 
"names", "row.names", ".S3Class"))
    , contains = structure(list(data.frame = S4_object(), 
    list = S4_object(), 
    oldClass = S4_object(), 
    vector = S4_object()), .Names = c("data.frame", 
"list", "oldClass", "vector"))
    , virtual = TRUE
    , prototype = structure(list(), .Names = character(0), row.names = integer(0), .S3Class = c("data.table", 
"data.frame"))
    , validity = NULL
    , access = list()
    , className = structure("data.table", package = "data.table")
    , package = "data.table"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


getDTthreads <- function () 
{
    .Call(CgetDTthreads)
}


year <- function (x) 
as.POSIXlt(x)$year + 1900L


frankv <- function (x, cols = seq_along(x), order = 1L, na.last = TRUE, 
    ties.method = c("average", "first", "random", "max", "min", 
        "dense")) 
{
    ties.method = match.arg(ties.method)
    if (!length(na.last)) 
        stop("length(na.last) = 0")
    if (length(na.last) != 1L) {
        warning("length(na.last) > 1, only the first element will be used")
        na.last = na.last[1L]
    }
    keep = (na.last == "keep")
    na.last = as.logical(na.last)
    as_list <- function(x) {
        xx = vector("list", 1L)
        .Call(Csetlistelt, xx, 1L, x)
        xx
    }
    if (is.atomic(x)) {
        if (!missing(cols) && !is.null(cols)) 
            stop("x is a single vector, non-NULL 'cols' doesn't make sense")
        cols = 1L
        x = as_list(x)
    }
    else {
        if (!length(cols)) 
            stop("x is a list, 'cols' can not be 0-length")
        if (is.character(cols)) 
            cols = chmatch(cols, names(x))
        cols = as.integer(cols)
    }
    x = .shallow(x, cols)
    setDT(x)
    cols = seq_along(cols)
    if (is.na(na.last)) {
        set(x, j = "..na_prefix..", value = is_na(x, cols))
        order = if (length(order) == 1L) 
            c(1L, rep(order, length(cols)))
        else c(1L, order)
        cols = c(ncol(x), cols)
        nas = x[[ncol(x)]]
    }
    if (ties.method == "random") {
        set(x, i = if (is.na(na.last)) 
            which_(nas, FALSE)
        else NULL, j = "..stats_runif..", value = stats::runif(nrow(x)))
        order = if (length(order) == 1L) 
            c(rep(order, length(cols)), 1L)
        else c(order, 1L)
        cols = c(cols, ncol(x))
    }
    xorder = forderv(x, by = cols, order = order, sort = TRUE, 
        retGrp = TRUE, na.last = if (identical(na.last, FALSE)) 
            na.last
        else TRUE)
    xstart = attr(xorder, "starts")
    xsorted = FALSE
    if (!length(xorder)) {
        xsorted = TRUE
        xorder = seq_along(x[[1L]])
    }
    ans = switch(ties.method, average = , min = , max = , dense = {
        rank = .Call(Cfrank, xorder, xstart, uniqlengths(xstart, 
            length(xorder)), ties.method)
    }, first = , random = {
        if (xsorted) xorder else forderv(xorder)
    })
    V1 = NULL
    if (isTRUE(keep)) {
        ans = (setDT(as_list(ans))[which_(nas, TRUE), `:=`(V1, 
            NA)])[[1L]]
    }
    else if (is.na(na.last)) {
        ans = ans[which_(nas, FALSE)]
    }
    ans
}


fsetdiff <- function (x, y, all = FALSE) 
{
    if (!is.logical(all) || length(all) != 1L) 
        stop("argument 'all' should be logical of length one")
    if (!is.data.table(x) || !is.data.table(y)) 
        stop("x and y must be both data.tables")
    if (!identical(sort(names(x)), sort(names(y)))) 
        stop("x and y must have same column names")
    if (!identical(names(x), names(y))) 
        stop("x and y must have same column order")
    bad.type = setNames(c("raw", "complex", "list") %chin% c(vapply(x, 
        typeof, FUN.VALUE = ""), vapply(y, typeof, FUN.VALUE = "")), 
        c("raw", "complex", "list"))
    if (any(bad.type)) 
        stop(sprintf("x and y must not have unsupported column types: %s", 
            paste(names(bad.type)[bad.type], collapse = ", ")))
    if (!identical(lapply(x, class), lapply(y, class))) 
        stop("x and y must have same column classes")
    if (".seqn" %in% names(x)) 
        stop("None of the datasets to setdiff should contain a column named '.seqn'")
    if (!nrow(x)) 
        return(x)
    if (!nrow(y)) 
        return(if (!all) funique(x) else x)
    if (all) {
        x = shallow(x)[, `:=`(".seqn", rowidv(x))]
        y = shallow(y)[, `:=`(".seqn", rowidv(y))]
        jn.on = c(".seqn", setdiff(names(x), ".seqn"))
        x[!y, .SD, .SDcols = setdiff(names(x), ".seqn"), on = jn.on]
    }
    else {
        funique(x[!y, on = names(x)])
    }
}


`:=` <- function (...) 
stop("Check that is.data.table(DT) == TRUE. Otherwise, := and `:=`(...) are defined for use in j, once only and in particular ways. See help(\":=\").")


second <- function (x) 
as.integer(as.POSIXlt(x)$sec)


.BY <- NULL


month <- function (x) 
as.POSIXlt(x)$mon + 1L


setkey <- function (x, ..., verbose = getOption("datatable.verbose"), physical = TRUE) 
{
    if (is.character(x)) 
        stop("x may no longer be the character name of the data.table. The possibility was undocumented and has been removed.")
    cols = as.character(substitute(list(...))[-1])
    if (!length(cols)) 
        cols = colnames(x)
    else if (identical(cols, "NULL")) 
        cols = NULL
    setkeyv(x, cols, verbose = verbose, physical = physical)
}


frank <- function (x, ..., na.last = TRUE, ties.method = c("average", 
    "first", "random", "max", "min", "dense")) 
{
    cols = substitute(list(...))[-1]
    if (identical(as.character(cols), "NULL")) {
        cols = NULL
        order = 1L
    }
    else if (length(cols)) {
        cols = as.list(cols)
        order = rep(1L, length(cols))
        for (i in seq_along(cols)) {
            v = as.list(cols[[i]])
            if (length(v) > 1 && v[[1L]] == "+") 
                v = v[[-1L]]
            else if (length(v) > 1 && v[[1L]] == "-") {
                v = v[[-1L]]
                order[i] = -1L
            }
            cols[[i]] = as.character(v)
        }
        cols = unlist(cols, use.names = FALSE)
    }
    else {
        cols = colnames(x)
        order = if (is.null(cols)) 
            1L
        else rep(1L, length(cols))
    }
    frankv(x, cols = cols, order = order, na.last = na.last, 
        ties.method = ties.method)
}


tstrsplit <- function (x, ..., fill = NA, type.convert = FALSE, keep, names = FALSE) 
{
    ans = transpose(strsplit(as.character(x), ...), fill = fill, 
        ignore.empty = FALSE)
    if (!missing(keep)) {
        keep = suppressWarnings(as.integer(keep))
        chk = min(keep) >= min(1L, length(ans)) & max(keep) <= 
            length(ans)
        if (!isTRUE(chk)) 
            stop("'keep' should contain integer values between ", 
                min(1L, length(ans)), " and ", length(ans), ".")
        ans = ans[keep]
    }
    if (type.convert) 
        ans = lapply(ans, type.convert, as.is = TRUE)
    if (identical(names, FALSE)) 
        return(ans)
    else if (isTRUE(names)) 
        names = paste0("V", seq_along(ans))
    if (!is.character(names)) 
        stop("'names' must be TRUE/FALSE or a character vector.")
    if (length(names) != length(ans)) {
        str = if (missing(keep)) 
            "ans"
        else "keep"
        stop("length(names) (= ", length(names), ") is not equal to length(", 
            str, ") (= ", length(ans), ").")
    }
    setattr(ans, "names", names)
    ans
}


rowid <- function (..., prefix = NULL) 
{
    rowidv(list(...), prefix = prefix)
}


as.xts.data.table <- function (x, ...) 
{
    stopifnot(requireNamespace("xts"), !missing(x), is.data.table(x))
    if (!any((index_class <- class(x[[1L]])) %in% c("POSIXct", 
        "Date"))) 
        stop("data.table must have a POSIXct, Date or IDate column on first position, use `setcolorder` function.")
    colsNumeric = sapply(x, is.numeric)[-1L]
    if (any(!colsNumeric)) 
        warning(paste("Following columns are not numeric and will be omitted:", 
            paste(names(colsNumeric)[!colsNumeric], collapse = ", ")))
    r = setDF(x[, .SD, .SDcols = names(colsNumeric)[colsNumeric]])
    xts::as.xts(r, order.by = if ("IDate" %in% index_class) 
        as.Date(x[[1L]])
    else x[[1L]])
}


tables <- function (mb = TRUE, order.col = "NAME", width = 80, env = parent.frame(), 
    silent = FALSE) 
{
    tt = objects(envir = env, all.names = TRUE)
    ss = which(as.logical(sapply(tt, function(x) is.data.table(get(x, 
        envir = env)))))
    if (!length(ss)) {
        if (!silent) 
            cat("No objects of class data.table exist in .GlobalEnv\n")
        return(invisible(data.table(NULL)))
    }
    tab = tt[ss]
    info = data.table(NAME = tab)
    for (i in seq_along(tab)) {
        DT = get(tab[i], envir = env)
        set(info, i, "NROW", nrow(DT))
        set(info, i, "NCOL", ncol(DT))
        if (mb) 
            set(info, i, "MB", ceiling(as.numeric(object.size(DT))/1024^2))
        set(info, i, "COLS", paste(colnames(DT), collapse = ","))
        set(info, i, "KEY", paste(key(DT), collapse = ","))
    }
    info[, `:=`(NROW, format(sprintf("%4s", prettyNum(NROW, big.mark = ",")), 
        justify = "right"))]
    info[, `:=`(NCOL, format(sprintf("%4s", prettyNum(NCOL, big.mark = ",")), 
        justify = "right"))]
    if (mb) {
        total = sum(info$MB)
        info[, `:=`(MB, format(sprintf("%2s", prettyNum(MB, big.mark = ",")), 
            justify = "right"))]
    }
    if (!order.col %in% names(info)) 
        stop("order.col='", order.col, "' not a column name of info")
    info = info[base::order(info[[order.col]])]
    m = as.matrix(info)
    colnames(m)[2] = sprintf(paste("%", nchar(m[1, "NROW"]), 
        "s", sep = ""), "NROW")
    colnames(m)[3] = sprintf(paste("%", nchar(m[1, "NCOL"]), 
        "s", sep = ""), "NCOL")
    if (mb) 
        colnames(m)[4] = sprintf(paste("%", nchar(m[1, "MB"]), 
            "s", sep = ""), "MB")
    m[, "COLS"] = substring(m[, "COLS"], 1, width)
    m[, "KEY"] = substring(m[, "KEY"], 1, width)
    if (!silent) {
        print(m, quote = FALSE, right = FALSE)
        if (mb) 
            cat("Total: ", prettyNum(as.character(total), big.mark = ","), 
                "MB\n", sep = "")
    }
    invisible(info)
}


melt <- function (data, ..., na.rm = FALSE, value.name = "value") 
{
    if (is.data.table(data)) 
        UseMethod("melt", data)
    else reshape2::melt(data, ..., na.rm = na.rm, value.name = value.name)
}


fwrite <- function (x, file = "", append = FALSE, quote = "auto", sep = ",", 
    sep2 = c("", "|", ""), eol = if (.Platform$OS.type == "windows") "\r\n" else "\n", 
    na = "", dec = ".", row.names = FALSE, col.names = TRUE, 
    qmethod = c("double", "escape"), logicalAsInt = FALSE, dateTimeAs = c("ISO", 
        "squash", "epoch", "write.csv"), buffMB = 8, nThread = getDTthreads(), 
    showProgress = getOption("datatable.showProgress"), verbose = getOption("datatable.verbose")) 
{
    isLOGICAL = function(x) isTRUE(x) || identical(FALSE, x)
    na = as.character(na[1L])
    if (missing(qmethod)) 
        qmethod = qmethod[1L]
    if (missing(dateTimeAs)) 
        dateTimeAs = dateTimeAs[1L]
    else if (length(dateTimeAs) > 1) 
        stop("dateTimeAs must be a single string")
    dateTimeAs = chmatch(dateTimeAs, c("ISO", "squash", "epoch", 
        "write.csv")) - 1L
    if (is.na(dateTimeAs)) 
        stop("dateTimeAs must be 'ISO','squash','epoch' or 'write.csv'")
    buffMB = as.integer(buffMB)
    nThread = as.integer(nThread)
    stopifnot(is.list(x), ncol(x) > 0L, identical(quote, "auto") || 
        identical(quote, FALSE) || identical(quote, TRUE), is.character(sep) && 
        length(sep) == 1L && nchar(sep) == 1L, is.character(sep2) && 
        length(sep2) == 3L && nchar(sep2[2L]) == 1L, is.character(dec) && 
        length(dec) == 1L && nchar(dec) == 1L, dec != sep, is.character(eol) && 
        length(eol) == 1L, length(qmethod) == 1L && qmethod %in% 
        c("double", "escape"), isLOGICAL(col.names), isLOGICAL(append), 
        isLOGICAL(row.names), isLOGICAL(verbose), isLOGICAL(showProgress), 
        isLOGICAL(logicalAsInt), length(na) == 1L, is.character(file) && 
            length(file) == 1 && !is.na(file), length(buffMB) == 
            1 && !is.na(buffMB) && 1 <= buffMB && buffMB <= 1024, 
        length(nThread) == 1 && !is.na(nThread) && nThread >= 
            1)
    file <- path.expand(file)
    if (append && missing(col.names) && (file == "" || file.exists(file))) 
        col.names = FALSE
    if (identical(quote, "auto")) 
        quote = NA
    if (file == "") {
        nThread = 1L
        showProgress = FALSE
    }
    .Call(Cwritefile, x, file, sep, sep2, eol, na, dec, quote, 
        qmethod == "escape", append, row.names, col.names, logicalAsInt, 
        dateTimeAs, buffMB, nThread, showProgress, verbose)
    invisible()
}


setorder <- function (x, ..., na.last = FALSE) 
{
    if (!is.data.frame(x)) 
        stop("x must be a data.frame or data.table.")
    cols = substitute(list(...))[-1]
    if (identical(as.character(cols), "NULL")) 
        return(x)
    if (length(cols)) {
        cols = as.list(cols)
        order = rep(1L, length(cols))
        for (i in seq_along(cols)) {
            v = as.list(cols[[i]])
            if (length(v) > 1 && v[[1L]] == "+") 
                v = v[[-1L]]
            else if (length(v) > 1 && v[[1L]] == "-") {
                v = v[[-1L]]
                order[i] = -1L
            }
            cols[[i]] = as.character(v)
        }
        cols = unlist(cols, use.names = FALSE)
    }
    else {
        cols = colnames(x)
        order = rep(1L, length(cols))
    }
    setorderv(x, cols, order, na.last)
}


key <- function (x) 
attr(x, "sorted", exact = TRUE)


like <- function (vector, pattern) 
{
    if (is.factor(vector)) {
        as.integer(vector) %in% grep(pattern, levels(vector))
    }
    else {
        grepl(pattern, vector)
    }
}


key2 <- function (x) 
{
    warning("key2() will be deprecated in the next relase. Please use indices() instead.", 
        call. = FALSE)
    ans = names(attributes(attr(x, "index", exact = TRUE)))
    if (is.null(ans)) 
        return(ans)
    gsub("^__", "", ans)
}


truelength <- function (x) 
.Call(Ctruelength, x)


setnames <- function (x, old, new) 
{
    if (!is.data.frame(x)) 
        stop("x is not a data.table or data.frame")
    if (!length(attr(x, "names"))) 
        stop("x has no column names")
    if (length(names(x)) != length(x)) 
        stop("dt is length ", length(dt), " but its names are length ", 
            length(names(x)))
    if (missing(new)) {
        if (!is.character(old)) 
            stop("Passed a vector of type '", typeof(old), "'. Needs to be type 'character'.")
        if (length(old) != ncol(x)) 
            stop("Can't assign ", length(old), " names to a ", 
                ncol(x), " column data.table")
        w = which(names(x) != old)
        if (!length(w)) 
            return(invisible(x))
        new = old[w]
        i = w
    }
    else {
        if (missing(old)) 
            stop("When 'new' is provided, 'old' must be provided too")
        if (!is.character(new)) 
            stop("'new' is not a character vector")
        if (is.numeric(old)) {
            if (length(sgn <- unique(sign(old))) != 1L) 
                stop("Items of 'old' is numeric but has both +ve and -ve indices.")
            tt = abs(old) < 1L | abs(old) > length(x) | is.na(old)
            if (any(tt)) 
                stop("Items of 'old' either NA or outside range [1,", 
                  length(x), "]: ", paste(old[tt], collapse = ","))
            i = if (sgn == 1L) 
                as.integer(old)
            else seq_along(x)[as.integer(old)]
            if (any(duplicated(i))) 
                stop("Some duplicates exist in 'old': ", paste(i[duplicated(i)], 
                  collapse = ","))
        }
        else {
            if (!is.character(old)) 
                stop("'old' is type ", typeof(old), " but should be integer, double or character")
            if (any(duplicated(old))) 
                stop("Some duplicates exist in 'old': ", paste(old[duplicated(old)], 
                  collapse = ","))
            i = chmatch(old, names(x))
            if (any(is.na(i))) 
                stop("Items of 'old' not found in column names: ", 
                  paste(old[is.na(i)], collapse = ","))
            if (any(tt <- !is.na(chmatch(old, names(x)[-i])))) 
                stop("Some items of 'old' are duplicated (ambiguous) in column names: ", 
                  paste(old[tt], collapse = ","))
        }
        if (length(new) != length(i)) 
            stop("'old' is length ", length(i), " but 'new' is length ", 
                length(new))
    }
    m = chmatch(names(x)[i], key(x))
    w = which(!is.na(m))
    if (length(w)) 
        .Call(Csetcharvec, attr(x, "sorted"), m[w], new[w])
    idx = attr(x, "index")
    for (k in names(attributes(idx))) {
        tt = strsplit(k, split = "__")[[1]][-1]
        m = chmatch(names(x)[i], tt)
        w = which(!is.na(m))
        if (length(w)) {
            tt[m[w]] = new[w]
            newk = paste("__", paste(tt, collapse = "__"), sep = "")
            setattr(idx, newk, attr(idx, k))
            setattr(idx, k, NULL)
        }
    }
    .Call(Csetcharvec, attr(x, "names"), as.integer(i), new)
    invisible(x)
}


setDF <- function (x, rownames = NULL) 
{
    if (!is.list(x)) 
        stop("setDF only accepts data.table, data.frame or list of equal length as input")
    if (any(duplicated(rownames))) 
        stop("rownames contains duplicates")
    if (is.data.table(x)) {
        if (is.null(rownames)) {
            rn <- .set_row_names(nrow(x))
        }
        else {
            if (length(rownames) != nrow(x)) 
                stop("rownames incorrect length; expected ", 
                  nrow(x), " names, got ", length(rownames))
            rn <- rownames
        }
        setattr(x, "row.names", rn)
        setattr(x, "class", "data.frame")
        setattr(x, "sorted", NULL)
        setattr(x, ".internal.selfref", NULL)
    }
    else if (is.data.frame(x)) {
        if (!is.null(rownames)) {
            if (length(rownames) != nrow(x)) 
                stop("rownames incorrect length; expected ", 
                  nrow(x), " names, got ", length(rownames))
            setattr(x, "row.names", rownames)
        }
        x
    }
    else {
        n = vapply(x, length, 0L)
        mn = max(n)
        if (any(n < mn)) 
            stop("All elements in argument 'x' to 'setDF' must be of same length")
        xn = names(x)
        if (is.null(xn)) {
            setattr(x, "names", paste("V", seq_len(length(x)), 
                sep = ""))
        }
        else {
            idx = xn %chin% ""
            if (any(idx)) {
                xn[idx] = paste("V", seq_along(which(idx)), sep = "")
                setattr(x, "names", xn)
            }
        }
        if (is.null(rownames)) {
            rn <- .set_row_names(mn)
        }
        else {
            if (length(rownames) != mn) 
                stop("rownames incorrect length; expected ", 
                  mn, " names, got ", length(rownames))
            rn <- rownames
        }
        setattr(x, "row.names", rn)
        setattr(x, "class", "data.frame")
    }
    invisible(x)
}


week <- function (x) 
yday(x)%/%7L + 1L


setkeyv <- function (x, cols, verbose = getOption("datatable.verbose"), 
    physical = TRUE) 
{
    if (is.null(cols)) {
        if (physical) 
            setattr(x, "sorted", NULL)
        setattr(x, "index", NULL)
        return(invisible(x))
    }
    if (!is.data.table(x)) 
        stop("x is not a data.table")
    if (!is.character(cols)) 
        stop("cols is not a character vector. Please see further information in ?setkey.")
    if (physical && identical(attr(x, ".data.table.locked"), 
        TRUE)) 
        stop("Setting a physical key on .SD is reserved for possible future use; to modify the original data's order by group. Try set2key instead. Or, set*(copy(.SD)) as a (slow) last resort.")
    if (!length(cols)) {
        warning("cols is a character vector of zero length. Removed the key, but use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
        setattr(x, "sorted", NULL)
        return(invisible(x))
    }
    if (identical(cols, "")) 
        stop("cols is the empty string. Use NULL to remove the key.")
    if (any(nchar(cols) == 0)) 
        stop("cols contains some blanks.")
    if (!length(cols)) {
        cols = colnames(x)
    }
    else {
        cols <- gsub("`", "", cols)
        miss = !(cols %in% colnames(x))
        if (any(miss)) 
            stop("some columns are not in the data.table: " %+% 
                cols[miss])
    }
    alreadykeyedbythiskey = identical(key(x), cols)
    if (".xi" %chin% names(x)) 
        stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
    for (i in cols) {
        .xi = x[[i]]
        if (!typeof(.xi) %chin% c("integer", "logical", "character", 
            "double")) 
            stop("Column '", i, "' is type '", typeof(.xi), "' which is not supported as a key column type, currently.")
    }
    if (!is.character(cols) || length(cols) < 1) 
        stop("'cols' should be character at this point in setkey")
    if (verbose) {
        tt = system.time(o <- forderv(x, cols, sort = TRUE, retGrp = FALSE))
        cat("forder took", tt["user.self"] + tt["sys.self"], 
            "sec\n")
    }
    else {
        o <- forderv(x, cols, sort = TRUE, retGrp = FALSE)
    }
    if (!physical) {
        if (is.null(attr(x, "index", exact = TRUE))) 
            setattr(x, "index", integer())
        setattr(attr(x, "index", exact = TRUE), paste("__", paste(cols, 
            collapse = "__"), sep = ""), o)
        return(invisible(x))
    }
    setattr(x, "index", NULL)
    if (length(o)) {
        if (alreadykeyedbythiskey) 
            warning("Already keyed by this key but had invalid row order, key rebuilt. If you didn't go under the hood please let datatable-help know so the root cause can be fixed.")
        if (verbose) {
            tt = system.time(.Call(Creorder, x, o))
            cat("reorder took", tt["user.self"] + tt["sys.self"], 
                "sec\n")
        }
        else {
            .Call(Creorder, x, o)
        }
    }
    else {
        if (verbose) 
            cat("x is already ordered by these columns, no need to call reorder\n")
    }
    if (!alreadykeyedbythiskey) 
        setattr(x, "sorted", cols)
    invisible(x)
}


setorderv <- function (x, cols, order = 1L, na.last = FALSE) 
{
    if (is.null(cols)) 
        return(x)
    if (!is.data.frame(x)) 
        stop("x must be a data.frame or data.table")
    na.last = as.logical(na.last)
    if (is.na(na.last) || !length(na.last)) 
        stop("na.last must be logical TRUE/FALSE")
    if (!is.character(cols)) 
        stop("cols is not a character vector. Please see further information in ?setorder.")
    if (!length(cols)) {
        warning("cols is a character vector of zero length. Use NULL instead, or wrap with suppressWarnings() to avoid this warning.")
        return(x)
    }
    if (any(nchar(cols) == 0)) 
        stop("cols contains some blanks.")
    if (!length(cols)) {
        cols = colnames(x)
    }
    else {
        cols <- gsub("`", "", cols)
        miss = !(cols %in% colnames(x))
        if (any(miss)) 
            stop("some columns are not in the data.table: " %+% 
                cols[miss])
    }
    if (".xi" %in% colnames(x)) 
        stop("x contains a column called '.xi'. Conflicts with internal use by data.table.")
    for (i in cols) {
        .xi = x[[i]]
        if (!typeof(.xi) %chin% c("integer", "logical", "character", 
            "double")) 
            stop("Column '", i, "' is type '", typeof(.xi), "' which is not supported for ordering currently.")
    }
    if (!is.character(cols) || length(cols) < 1) 
        stop("'cols' should be character at this point in setkey.")
    o = forderv(x, cols, sort = TRUE, retGrp = FALSE, order = order, 
        na.last = na.last)
    if (length(o)) {
        .Call(Creorder, x, o)
        if (is.data.frame(x) & !is.data.table(x)) {
            .Call(Creorder, rn <- rownames(x), o)
            setattr(x, "row.names", rn)
        }
        setattr(x, "sorted", NULL)
        setattr(x, "index", NULL)
    }
    invisible(x)
}


.I <- NULL


set <- function (x, i = NULL, j, value) 
{
    if (is.atomic(value)) {
        l = vector("list", 1)
        .Call(Csetlistelt, l, 1L, value)
        value = l
    }
    .Call(Cassign, x, i, j, NULL, value, FALSE)
    invisible(x)
}


funion <- function (x, y, all = FALSE) 
{
    if (!is.logical(all) || length(all) != 1L) 
        stop("argument 'all' should be logical of length one")
    if (!is.data.table(x) || !is.data.table(y)) 
        stop("x and y must be both data.tables")
    if (!identical(sort(names(x)), sort(names(y)))) 
        stop("x and y must have same column names")
    if (!identical(names(x), names(y))) 
        stop("x and y must have same column order")
    bad.type = setNames(c("raw", "complex", if (!all) "list") %chin% 
        c(vapply(x, typeof, FUN.VALUE = ""), vapply(y, typeof, 
            FUN.VALUE = "")), c("raw", "complex", if (!all) "list"))
    if (any(bad.type)) 
        stop(sprintf("x and y must not have unsupported column types: %s", 
            paste(names(bad.type)[bad.type], collapse = ", ")))
    if (!identical(lapply(x, class), lapply(y, class))) 
        stop("x and y must have same column classes")
    ans = rbindlist(list(x, y))
    if (!all) 
        ans = funique(ans)
    ans
}


as.ITime <- function (x, ...) 
UseMethod("as.ITime")


.N <- NULL


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

inrange <- function (x, lower, upper, incbounds = TRUE) 
{
    query = setDT(list(x = x))
    subject = setDT(list(l = lower, u = upper))
    ops = if (incbounds) 
        c(4L, 2L)
    else c(5L, 3L)
    verbose = getOption("datatable.verbose")
    if (verbose) {
        last.started.at = proc.time()[3]
        cat("forderv(query) took ... ")
        flush.console()
    }
    xo = forderv(query)
    if (verbose) {
        cat(round(proc.time()[3] - last.started.at, 3), "secs\n")
        flush.console
    }
    ans = bmerge(shallow(subject), query, 1:2, c(1L, 1L), FALSE, 
        xo, 0, c(FALSE, TRUE), 0L, "all", ops, integer(0), 1L, 
        verbose)
    options(datatable.verbose = FALSE)
    setDT(ans[c("starts", "lens")], key = c("starts", "lens"))
    options(datatable.verbose = verbose)
    if (verbose) {
        last.started.at = proc.time()[3]
        cat("Generating final logical vector ... ")
        flush.console()
    }
    .Call(Cinrange, idx <- vector("logical", length(x)), xo, 
        ans[["starts"]], ans[["lens"]])
    if (verbose) {
        cat("done in", round(proc.time()[3] - last.started.at, 
            3), "secs\n")
        flush.console
    }
    idx
}


set2keyv <- function (...) 
{
    warning("set2key() will be deprecated in the next relase. Please use setindex() instead.", 
        call. = FALSE)
    setkeyv(..., physical = FALSE)
}


setDTthreads <- function (threads) 
{
    invisible(.Call(CsetDTthreads, as.integer(threads)))
}


.EACHI <- NULL


IDateTime <- function (x, ...) 
UseMethod("IDateTime")


`%like%` <- function (vector, pattern) 
{
    if (is.factor(vector)) {
        as.integer(vector) %in% grep(pattern, levels(vector))
    }
    else {
        grepl(pattern, vector)
    }
}


setDT <- function (x, keep.rownames = FALSE, key = NULL, check.names = FALSE) 
{
    name = substitute(x)
    if (is.name(name)) {
        home <- function(x, env) {
            if (identical(env, emptyenv())) 
                stop("Can not find symbol ", cname, call. = FALSE)
            else if (exists(x, env, inherits = FALSE)) 
                env
            else home(x, parent.env(env))
        }
        cname = as.character(name)
        envir = home(cname, parent.frame())
        if (bindingIsLocked(cname, envir)) {
            stop("Can not convert '", cname, "' to data.table by reference because binding is locked. It is very likely that '", 
                cname, "' resides within a package (or an environment) that is locked to prevent modifying its variable bindings. Try copying the object to your current environment, ex: var <- copy(var) and then using setDT again.")
        }
    }
    if (is.data.table(x)) {
        setattr(x, "class", .resetclass(x, "data.table"))
        if (!missing(key)) 
            setkeyv(x, key)
        if (check.names) 
            setattr(x, "names", make.names(names(x), unique = TRUE))
        if (selfrefok(x) > 0) 
            return(invisible(x))
        else alloc.col(x)
    }
    else if (is.data.frame(x)) {
        rn = if (!identical(keep.rownames, FALSE)) 
            rownames(x)
        else NULL
        setattr(x, "row.names", .set_row_names(nrow(x)))
        if (check.names) 
            setattr(x, "names", make.names(names(x), unique = TRUE))
        setattr(x, "class", .resetclass(x, "data.frame"))
        alloc.col(x)
        if (!is.null(rn)) {
            nm = c(if (is.character(keep.rownames)) keep.rownames[1L] else "rn", 
                names(x))
            x[, `:=`((nm[1L]), rn)]
            setcolorder(x, nm)
        }
    }
    else if (is.null(x) || (is.list(x) && !length(x))) {
        x = null.data.table()
    }
    else if (is.list(x)) {
        for (i in seq_along(x)) {
            if (inherits(x[[i]], "POSIXlt")) 
                stop("Column ", i, " is of POSIXlt type. Please convert it to POSIXct using as.POSIXct and run setDT again. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.")
        }
        n = vapply(x, length, 0L)
        mn = max(n)
        if (any(n < mn)) 
            stop("All elements in argument 'x' to 'setDT' must be of same length")
        xn = names(x)
        if (is.null(xn)) {
            setattr(x, "names", paste("V", seq_len(length(x)), 
                sep = ""))
        }
        else {
            idx = xn %chin% ""
            if (any(idx)) {
                xn[idx] = paste("V", seq_along(which(idx)), sep = "")
                setattr(x, "names", xn)
            }
            if (check.names) 
                setattr(x, "names", make.names(xn, unique = TRUE))
        }
        setattr(x, "row.names", .set_row_names(max(n)))
        setattr(x, "class", c("data.table", "data.frame"))
        alloc.col(x)
    }
    else {
        stop("Argument 'x' to 'setDT' should be a 'list', 'data.frame' or 'data.table'")
    }
    if (!is.null(key)) 
        setkeyv(x, key)
    if (is.name(name)) {
        name = as.character(name)
        assign(name, x, parent.frame(), inherits = TRUE)
    }
    else if (is.call(name) && (name[[1L]] == "$" || name[[1L]] == 
        "[[") && is.name(name[[2L]])) {
        k = eval(name[[2L]], parent.frame(), parent.frame())
        if (is.list(k)) {
            origj = j = if (name[[1L]] == "$") 
                as.character(name[[3L]])
            else eval(name[[3L]], parent.frame(), parent.frame())
            if (length(j) == 1L) {
                if (is.character(j)) {
                  j = match(j, names(k))
                  if (is.na(j)) 
                    stop("Item '", origj, "' not found in names of input list")
                }
            }
            .Call(Csetlistelt, k, as.integer(j), x)
        }
        else if (is.environment(k) && exists(as.character(name[[3L]]), 
            k)) {
            assign(as.character(name[[3L]]), x, k, inherits = FALSE)
        }
    }
    invisible(x)
}


SJ <- function (...) 
{
    JDT = as.data.table(list(...))
    setkey(JDT)
}


melt.data.table <- function (data, id.vars, measure.vars, variable.name = "variable", 
    value.name = "value", ..., na.rm = FALSE, variable.factor = TRUE, 
    value.factor = FALSE, verbose = getOption("datatable.verbose")) 
{
    if (!is.data.table(data)) 
        stop("'data' must be a data.table")
    if (missing(id.vars)) 
        id.vars = NULL
    if (missing(measure.vars)) 
        measure.vars = NULL
    measure.sub = substitute(measure.vars)
    if (is.call(measure.sub) && measure.sub[[1L]] == "patterns") {
        measure.sub = as.list(measure.sub)[-1L]
        idx = which(names(measure.sub) %in% "cols")
        if (length(idx)) {
            cols = eval(measure.sub[["cols"]], parent.frame())
            measure.sub = measure.sub[-idx]
        }
        else cols = names(data)
        pats = lapply(measure.sub, eval, parent.frame())
        measure.vars = patterns(pats, cols = cols)
    }
    if (is.list(measure.vars) && length(measure.vars) > 1L) {
        if (length(value.name) == 1L) 
            value.name = paste(value.name, seq_along(measure.vars), 
                sep = "")
    }
    ans <- .Call(Cfmelt, data, id.vars, measure.vars, as.logical(variable.factor), 
        as.logical(value.factor), variable.name, value.name, 
        as.logical(na.rm), as.logical(verbose))
    setDT(ans)
    if (any(duplicated(names(ans)))) {
        cat("Duplicate column names found in molten data.table. Setting unique names using 'make.names'\n")
        setnames(ans, make.unique(names(ans)))
    }
    setattr(ans, "sorted", NULL)
    ans
}


isoweek <- function (x) 
{
    xlt <- as.POSIXlt(x)
    nearest_thurs <- xlt + (3 - ((xlt$wday - 1)%%7)) * 86400
    year_start <- as.POSIXct(paste0(as.POSIXlt(nearest_thurs)$year + 
        1900L, "-01-01"))
    as.integer(1 + unclass(difftime(nearest_thurs, year_start, 
        units = "days"))%/%7)
}


uniqueN <- function (x, by = if (is.list(x)) seq_along(x) else NULL, na.rm = FALSE) 
{
    if (missing(by) && is.data.table(x) && isTRUE(getOption("datatable.old.unique.by.key"))) 
        by = key(x)
    if (is.null(x)) 
        return(0L)
    if (!is.atomic(x) && !is.data.frame(x)) 
        stop("x must be an atomic vector or data.frames/data.tables")
    if (is.atomic(x)) 
        x = as_list(x)
    if (is.null(by)) 
        by = seq_along(x)
    o = forderv(x, by = by, retGrp = TRUE, na.last = if (!na.rm) 
        FALSE
    else NA)
    starts = attr(o, "starts")
    if (!na.rm) {
        length(starts)
    }
    else {
        sum((if (length(o)) o[starts] else starts) != 0L)
    }
}


chorder <- function (x) 
{
    o = forderv(x, sort = TRUE, retGrp = FALSE)
    if (length(o)) 
        o
    else seq_along(x)
}


chmatch <- function (x, table, nomatch = NA_integer_) 
.Call(Cchmatchwrapper, x, table, as.integer(nomatch[1L]), FALSE)


fsetequal <- function (x, y) 
{
    if (!is.data.table(x) || !is.data.table(y)) 
        stop("x and y must be both data.tables")
    if (!identical(sort(names(x)), sort(names(y)))) 
        stop("x and y must have same column names")
    if (!identical(names(x), names(y))) 
        stop("x and y must have same column order")
    bad.type = setNames(c("raw", "complex", "list") %chin% c(vapply(x, 
        typeof, FUN.VALUE = ""), vapply(y, typeof, FUN.VALUE = "")), 
        c("raw", "complex", "list"))
    if (any(bad.type)) 
        stop(sprintf("x and y must not have unsupported column types: %s", 
            paste(names(bad.type)[bad.type], collapse = ", ")))
    if (!identical(lapply(x, class), lapply(y, class))) 
        stop("x and y must have same column classes")
    isTRUE(all.equal.data.table(x, y, check.attributes = FALSE, 
        ignore.row.order = TRUE))
}


transpose <- function (l, fill = NA, ignore.empty = FALSE) 
{
    ans = .Call(Ctranspose, l, fill, ignore.empty)
    if (is.data.table(l)) 
        setDT(ans)
    else if (is.data.frame(l)) {
        if (is.null(names(ans))) 
            setattr(ans, "names", paste("V", seq_along(ans), 
                sep = ""))
        setattr(ans, "row.names", .set_row_names(length(ans[[1L]])))
        setattr(ans, "class", "data.frame")
    }
    ans[]
}


dcast <- function (data, formula, fun.aggregate = NULL, ..., margins = NULL, 
    subset = NULL, fill = NULL, value.var = guess(data)) 
{
    if (is.data.table(data)) 
        UseMethod("dcast", data)
    else reshape2::dcast(data, formula, fun.aggregate = fun.aggregate, 
        ..., margins = margins, subset = subset, fill = fill, 
        value.var = value.var)
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

fintersect <- function (x, y, all = FALSE) 
{
    if (!is.logical(all) || length(all) != 1L) 
        stop("argument 'all' should be logical of length one")
    if (!is.data.table(x) || !is.data.table(y)) 
        stop("x and y must be both data.tables")
    if (!identical(sort(names(x)), sort(names(y)))) 
        stop("x and y must have same column names")
    if (!identical(names(x), names(y))) 
        stop("x and y must have same column order")
    bad.type = setNames(c("raw", "complex", "list") %chin% c(vapply(x, 
        typeof, FUN.VALUE = ""), vapply(y, typeof, FUN.VALUE = "")), 
        c("raw", "complex", "list"))
    if (any(bad.type)) 
        stop(sprintf("x and y must not have unsupported column types: %s", 
            paste(names(bad.type)[bad.type], collapse = ", ")))
    if (!identical(lapply(x, class), lapply(y, class))) 
        stop("x and y must have same column classes")
    if (".seqn" %in% names(x)) 
        stop("None of the datasets to intersect should contain a column named '.seqn'")
    if (!nrow(x) || !nrow(y)) 
        return(x[0L])
    if (all) {
        x = shallow(x)[, `:=`(".seqn", rowidv(x))]
        y = shallow(y)[, `:=`(".seqn", rowidv(y))]
        jn.on = c(".seqn", setdiff(names(x), ".seqn"))
        x[y, .SD, .SDcols = setdiff(names(x), ".seqn"), nomatch = 0L, 
            on = jn.on]
    }
    else {
        x[funique(y), nomatch = 0L, on = names(x), mult = "first"]
    }
}


getNumericRounding <- function () 
.Call(CgetNumericRounding)


indices <- function (x, vectors = FALSE) 
{
    ans = names(attributes(attr(x, "index", exact = TRUE)))
    if (is.null(ans)) 
        return(ans)
    ans <- gsub("^__", "", ans)
    if (isTRUE(vectors)) 
        ans <- strsplit(ans, "__", fixed = TRUE)
    ans
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

timetaken <- function (started.at) 
{
    if (inherits(started.at, "POSIXct")) {
        secs <- as.double(difftime(Sys.time(), started.at, units = "secs"))
    }
    else {
        secs = proc.time()[3] - started.at[3]
    }
    mins <- secs%/%60
    hrs <- mins%/%60
    days <- hrs%/%24
    mins = mins - hrs * 60
    hrs = hrs - 24 * days
    if (secs >= 60) {
        if (days >= 1) 
            res = sprintf("%d days ", as.integer(days))
        else res = ""
        paste(res, sprintf("%02.0f:%02.0f:%02.0f", hrs, mins, 
            secs%%60), sep = "")
    }
    else {
        sprintf(if (secs >= 10) 
            "%.1fsec"
        else "%.3fsec", secs)
    }
}


foverlaps <- function (x, y, by.x = if (!is.null(key(x))) key(x) else key(y), 
    by.y = key(y), maxgap = 0L, minoverlap = 1L, type = c("any", 
        "within", "start", "end", "equal"), mult = c("all", "first", 
        "last"), nomatch = getOption("datatable.nomatch"), which = FALSE, 
    verbose = getOption("datatable.verbose")) 
{
    if (!is.data.table(y) || !is.data.table(x)) 
        stop("y and x must both be data.tables. Use `setDT()` to convert list/data.frames to data.tables by reference or as.data.table() to convert to data.tables by copying.")
    maxgap = as.integer(maxgap)
    minoverlap = as.integer(minoverlap)
    which = as.logical(which)
    nomatch = as.integer(nomatch)
    if (!length(maxgap) || length(maxgap) != 1L || is.na(maxgap) || 
        maxgap < 0L) 
        stop("maxgap must be a non-negative integer value of length 1")
    if (!length(minoverlap) || length(minoverlap) != 1L || is.na(minoverlap) || 
        minoverlap < 1L) 
        stop("minoverlap must be a positive integer value of length 1")
    if (!length(which) || length(which) != 1L || is.na(which)) 
        stop("which must be a logical vector of length 1. Either TRUE/FALSE")
    if (!length(nomatch) || length(nomatch) != 1L || (!is.na(nomatch) && 
        nomatch != 0L)) 
        stop("nomatch must either be NA or 0, or (ideally) NA_integer_ or 0L")
    type = match.arg(type)
    mult = match.arg(mult)
    if (type == "equal") 
        stop("type = 'equal' is not implemented yet. But note that this is just the same as a normal data.table join y[x, ...], unless you are also interested in setting 'minoverlap / maxgap' arguments. But those arguments are not implemented yet as well.")
    if (maxgap > 0L || minoverlap > 1L) 
        stop("maxgap and minoverlap arguments are not yet implemented.")
    if (is.null(by.y)) 
        stop("'y' must be keyed (i.e., sorted, and, marked as sorted). Call setkey(y, ...) first, see ?setkey. Also check the examples in ?foverlaps.")
    if (length(by.x) < 2L || length(by.y) < 2L) 
        stop("'by.x' and 'by.y' should contain at least two column names (or numbers) each - corresponding to 'start' and 'end' points of intervals. Please see ?foverlaps and examples for more info.")
    if (is.numeric(by.x)) {
        if (any(by.x < 0L) || any(by.x > length(x))) 
            stop("Invalid numeric value for 'by.x'; it should be a vector with values 1 <= by.x <= length(x)")
        by.x = names(x)[by.x]
    }
    if (is.numeric(by.y)) {
        if (any(by.y < 0L) || any(by.y > length(y))) 
            stop("Invalid numeric value for 'by.x'; it should be a vector with values 1 <= by.y <= length(y)")
        by.y = names(y)[by.y]
    }
    if (!length(by.x) || !is.character(by.x)) 
        stop("A non-empty vector of column names is required for by.x")
    if (!length(by.y) || !is.character(by.y)) 
        stop("A non-empty vector of column names is required for by.y")
    if (!identical(by.y, key(y)[seq_along(by.y)])) 
        stop("The first ", length(by.y), " columns of y's key is not identical to the columns specified in by.y.")
    if (any(is.na(chmatch(by.x, names(x))))) 
        stop("Elements listed in 'by.x' must be valid names in data.table 'x'")
    if (any(is.na(chmatch(by.y, names(y))))) 
        stop("Elements listed in 'by.y' must be valid names in data.table 'y'")
    if (anyDuplicated(by.x) || anyDuplicated(by.y)) 
        stop("Duplicate columns are not allowed in overlap joins. This may change in the future.")
    if (length(by.x) != length(by.y)) 
        stop("length(by.x) != length(by.y). Columns specified in by.x should correspond to columns specified in by.y and should be of same lengths.")
    if (any(dup.x <- duplicated(names(x)))) 
        stop("x has some duplicated column name(s): ", paste(unique(names(x)[dup.x]), 
            collapse = ","), ". Please remove or rename the duplicate(s) and try again.")
    if (any(dup.y <- duplicated(names(y)))) 
        stop("y has some duplicated column name(s): ", paste(unique(names(y)[dup.y]), 
            collapse = ","), ". Please remove or rename the duplicate(s) and try again.")
    xnames = by.x
    xintervals = tail(xnames, 2L)
    ynames = by.y
    yintervals = tail(ynames, 2L)
    if (!storage.mode(x[[xintervals[1L]]]) %chin% c("double", 
        "integer") || !storage.mode(x[[xintervals[2L]]]) %chin% 
        c("double", "integer")) 
        stop("The last two columns in by.x should correspond to the 'start' and 'end' intervals in data.table 'x' and must be integer/numeric type.")
    if (any(x[[xintervals[2L]]] - x[[xintervals[1L]]] < 0L)) 
        stop("All entries in column ", xintervals[1L], " should be <= corresponding entries in column ", 
            xintervals[2L], " in data.table 'x'")
    if (!storage.mode(y[[yintervals[1L]]]) %chin% c("double", 
        "integer") || !storage.mode(y[[yintervals[2L]]]) %chin% 
        c("double", "integer")) 
        stop("The last two columns in by.y should correspond to the 'start' and 'end' intervals in data.table 'y' and must be integer/numeric type.")
    if (any(y[[yintervals[2L]]] - y[[yintervals[1L]]] < 0L)) 
        stop("All entries in column ", yintervals[1L], " should be <= corresponding entries in column ", 
            yintervals[2L], " in data.table 'y'")
    yclass = c(class(y[[yintervals[1L]]]), class(y[[yintervals[2L]]]))
    isdouble = FALSE
    isposix = FALSE
    if (any(c("numeric", "POSIXct") %chin% yclass)) {
        dt_eps <- function() {
            bits = floor(log2(.Machine$double.eps))
            2^(bits + (getNumericRounding() * 8L))
        }
        incr = 1 + dt_eps()
        isdouble = TRUE
        isposix = "POSIXct" %chin% yclass
    }
    else incr = 1L
    origx = x
    x = shallow(x, by.x)
    origy = y
    y = shallow(y, by.y)
    if (identical(by.x, key(origx)[seq_along(by.x)])) 
        setattr(x, "sorted", by.x)
    setattr(y, "sorted", by.y)
    roll = switch(type, start = , end = , equal = 0, any = , 
        within = +Inf)
    make_call <- function(names, fun = NULL) {
        if (is.character(names)) 
            names = lapply(names, as.name)
        call = c(substitute(fun, list(fun = fun)), names)
        if (!is.null(fun)) 
            as.call(call)
        else call
    }
    construct <- function(icols, mcols, type = type) {
        icall = make_call(icols)
        setattr(icall, "names", icols)
        mcall = make_call(mcols, quote(c))
        if (type %chin% c("within", "any")) {
            mcall[[3L]] = substitute(if (isposix) unclass(val) * 
                incr else if (isdouble) {
                (val + dt_eps()) * (1 + sign(val) * dt_eps())
            } else val + incr, list(val = mcall[[3L]], incr = incr))
        }
        make_call(c(icall, pos = mcall), quote(list))
    }
    uycols = switch(type, start = yintervals[1L], end = yintervals[2L], 
        any = , within = , equal = yintervals)
    call = construct(head(ynames, -2L), uycols, type)
    if (verbose) {
        last.started.at = proc.time()[3]
        cat("unique() + setkey() operations done in ...")
        flush.console()
    }
    uy = unique(y[, eval(call)])
    setkey(uy)[, `:=`(lookup = list(list(integer(0))), type_lookup = list(list(integer(0))), 
        count = 0L, type_count = 0L)]
    if (verbose) {
        cat(round(proc.time()[3] - last.started.at, 3), "secs\n")
        flush.console
    }
    matches <- function(ii, xx, del, ...) {
        cols = setdiff(names(xx), del)
        xx = shallow(xx, cols)
        ans = bmerge(xx, ii, seq_along(xx), seq_along(xx), haskey(xx), 
            integer(0), mult = mult, ops = rep(1L, length(xx)), 
            integer(0), 1L, verbose = verbose, ...)
        if (ans$allLen1) 
            ans$starts
        else vecseq(ans$starts, ans$lens, NULL)
    }
    indices <- function(x, y, intervals, ...) {
        if (type == "start") {
            sidx = eidx = matches(x, y, intervals[2L], rollends = c(FALSE, 
                FALSE), ...)
        }
        else if (type == "end") {
            eidx = sidx = matches(x, y, intervals[1L], rollends = c(FALSE, 
                FALSE), ...)
        }
        else {
            sidx = matches(x, y, intervals[2L], rollends = rep(type == 
                "any", 2L), ...)
            eidx = matches(x, y, intervals[1L], rollends = c(FALSE, 
                TRUE), ...)
        }
        list(sidx, eidx)
    }
    .Call(Clookup, uy, nrow(y), indices(uy, y, yintervals, nomatch = 0L, 
        roll = roll), maxgap, minoverlap, mult, type, verbose)
    if (maxgap == 0L && minoverlap == 1L) {
        iintervals = tail(names(x), 2L)
        if (verbose) {
            last.started.at = proc.time()[3]
            cat("binary search(es) done in ...")
            flush.console()
        }
        xmatches = indices(uy, x, xintervals, nomatch = 0L, roll = roll)
        if (verbose) {
            cat(round(proc.time()[3] - last.started.at, 3), "secs\n")
            flush.console
        }
        olaps = .Call(Coverlaps, uy, xmatches, mult, type, nomatch, 
            verbose)
    }
    else if (maxgap == 0L && minoverlap > 1L) {
        stop("Not yet implemented")
    }
    else if (maxgap > 0L && minoverlap == 1L) {
        stop("Not yet implemented")
    }
    else if (maxgap > 0L && minoverlap > 1L) {
        if (maxgap > minoverlap) 
            warning("maxgap > minoverlap. maxgap will have no effect here.")
        stop("Not yet implemented")
    }
    setDT(olaps)
    setnames(olaps, c("xid", "yid"))
    yid = NULL
    if (which) {
        if (mult %chin% c("first", "last")) 
            return(olaps$yid)
        else if (!is.na(nomatch)) 
            return(.Call(CsubsetDT, olaps, which(olaps$yid > 
                0L), seq_along(olaps)))
        else return(olaps)
    }
    else {
        if (!is.na(nomatch)) 
            olaps = .Call(CsubsetDT, olaps, which(olaps$yid > 
                0L), seq_along(olaps))
        ycols = setdiff(names(origy), head(by.y, -2L))
        idx = chmatch(ycols, names(origx), nomatch = 0L)
        ans = .Call(CsubsetDT, origx, olaps$xid, seq_along(origx))
        if (any(idx > 0L)) 
            setnames(ans, names(ans)[idx], paste("i.", names(ans)[idx], 
                sep = ""))
        xcols1 = head(by.x, -2L)
        xcols2 = setdiff(names(ans), xcols1)
        ans[, `:=`((ycols), .Call(CsubsetDT, origy, olaps$yid, 
            chmatch(ycols, names(origy))))]
        setcolorder(ans, c(xcols1, ycols, xcols2))
        return(ans[])
    }
}


.__C__IDate <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("IDate", package = "data.table")
    , package = "data.table"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


fread <- function (input = "", sep = "auto", sep2 = "auto", nrows = -1L, 
    header = "auto", na.strings = "NA", file, stringsAsFactors = FALSE, 
    verbose = getOption("datatable.verbose"), autostart = 1L, 
    skip = 0L, select = NULL, drop = NULL, colClasses = NULL, 
    integer64 = getOption("datatable.integer64"), dec = if (sep != 
        ".") "." else ",", col.names, check.names = FALSE, encoding = "unknown", 
    quote = "\"", strip.white = TRUE, fill = FALSE, blank.lines.skip = FALSE, 
    key = NULL, showProgress = getOption("datatable.showProgress"), 
    data.table = getOption("datatable.fread.datatable")) 
{
    if (!is.character(dec) || length(dec) != 1L || nchar(dec) != 
        1) 
        stop("dec must be a single character e.g. '.' or ','")
    if (length(encoding) != 1L || !encoding %in% c("unknown", 
        "UTF-8", "Latin-1")) {
        stop("Argument 'encoding' must be 'unknown', 'UTF-8' or 'Latin-1'.")
    }
    isLOGICAL = function(x) isTRUE(x) || identical(FALSE, x)
    stopifnot(isLOGICAL(strip.white), isLOGICAL(blank.lines.skip), 
        isLOGICAL(fill), isLOGICAL(showProgress), isLOGICAL(stringsAsFactors), 
        isLOGICAL(verbose), isLOGICAL(check.names))
    if (getOption("datatable.fread.dec.experiment") && Sys.localeconv()["decimal_point"] != 
        dec) {
        oldlocale = Sys.getlocale("LC_NUMERIC")
        if (verbose) 
            cat("dec='", dec, "' but current locale ('", oldlocale, 
                "') has dec='", Sys.localeconv()["decimal_point"], 
                "'. Attempting to change locale to one that has the desired decimal point.\n", 
                sep = "")
        on.exit(Sys.setlocale("LC_NUMERIC", oldlocale))
        if (dec == ".") {
            tt <- Sys.setlocale("LC_NUMERIC", "C")
            if (!identical(tt, "C")) 
                stop("It is supposed to be guaranteed that Sys.setlocale('LC_NUMERIC', 'C') will always work!")
        }
        else suppressWarnings(tt <- Sys.setlocale("LC_NUMERIC", 
            ""))
        if (Sys.localeconv()["decimal_point"] != dec) {
            if (verbose) 
                cat("Changing to system locale ('", tt, "') did not provide the desired dec. Now trying any provided in getOption('datatable.fread.dec.locale')\n", 
                  sep = "")
            for (i in getOption("datatable.fread.dec.locale")) {
                if (i == "") {
                  if (verbose) 
                    cat("Ignoring ''\n")
                  next
                }
                else {
                  if (verbose) 
                    cat("Trying '", i, "'\n", sep = "")
                }
                suppressWarnings(tt <- Sys.setlocale("LC_NUMERIC", 
                  i))
                cmd = paste("Sys.setlocale('LC_NUMERIC','", i, 
                  "')", sep = "")
                if (is.null(tt)) 
                  stop(cmd, " returned NULL (locale information is unavailable). See ?Sys.setlocale.")
                if (tt == "") {
                  if (verbose) 
                    cat(cmd, " returned \"\"; i.e., this locale name is not valid on your system. It was provided by you in getOption(\"datatable.fread.dec.locale\"). See ?Sys.setlocale and ?fread.")
                  next
                }
                if (toupper(tt) != toupper(i)) {
                  warning(cmd, " returned '", tt, "' != '", i, 
                    "' (not NULL not '' and allowing for case differences). This may not be a problem but please report.")
                }
                if (Sys.localeconv()["decimal_point"] == dec) 
                  break
                if (verbose) 
                  cat("Successfully changed locale but it provides dec='", 
                    Sys.localeconv()["decimal_point"], "' not the desired dec", 
                    sep = "")
            }
        }
        if (Sys.localeconv()["decimal_point"] != dec) {
            stop("Unable to change to a locale which provides the desired dec. You will need to add a valid locale name to getOption(\"datatable.fread.dec.locale\"). See the long paragraph in ?fread.", 
                if (verbose) 
                  ""
                else " Run again with verbose=TRUE to inspect.")
        }
        if (verbose) 
            cat("This R session's locale is now '", tt, "' which provides the desired decimal point for reading numerics in the file - success! The locale will be restored to what it was ('", 
                oldlocale, ") even if the function fails for other reasons.\n")
    }
    if (!missing(file)) {
        if (!identical(input, "")) 
            stop("You can provide 'input' or 'file', not both.")
        if (!file.exists(file)) 
            stop(sprintf("Provided file '%s' does not exists.", 
                file))
        input = file
    }
    is_url <- function(x) grepl("^(http|ftp)s?://", x)
    is_secureurl <- function(x) grepl("^(http|ftp)s://", x)
    is_file <- function(x) grepl("^file://", x)
    if (!is.character(input) || length(input) != 1L) {
        stop("'input' must be a single character string containing a file name, a command, full path to a file, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or the input data itself")
    }
    else if (is_url(input) || is_file(input)) {
        tt = tempfile()
        on.exit(unlink(tt), add = TRUE)
        if (!is_secureurl(input)) {
            method <- if (is_file(input)) 
                "auto"
            else getOption("download.file.method", default = "auto")
            download.file(input, tt, method = method, mode = "wb", 
                quiet = !showProgress)
        }
        else {
            if (!requireNamespace("curl", quietly = TRUE)) 
                stop("Input URL requires https:// connection for which fread() requires 'curl' package, but cannot be found. Please install the package using 'install.packages()'.")
            curl::curl_download(input, tt, mode = "wb", quiet = !showProgress)
        }
        input = tt
    }
    else if (input == "" || length(grep("\\n|\\r", input)) > 
        0) {
    }
    else if (isTRUE(file.info(input)$isdir)) {
        stop("'input' can not be a directory name, but must be a single character string containing a file name, a command, full path to a file, a URL starting 'http[s]://', 'ftp[s]://' or 'file://', or the input data itself.")
    }
    else if (!file.exists(input)) {
        if (length(grep(" ", input)) == 0) 
            stop("File '", input, "' does not exist. Include one or more spaces to consider the input a system command.")
        tt = tempfile()
        on.exit(unlink(tt), add = TRUE)
        if (.Platform$OS.type == "unix") {
            if (file.exists("/dev/shm") && file.info("/dev/shm")$isdir) {
                tt = tempfile(tmpdir = "/dev/shm")
            }
            system(paste("(", input, ") > ", tt, sep = ""))
        }
        else {
            shell(paste("(", input, ") > ", tt, sep = ""))
        }
        input = tt
    }
    if (identical(header, "auto")) 
        header = NA
    if (identical(sep, "auto")) 
        sep = NULL
    if (is.atomic(colClasses) && !is.null(names(colClasses))) 
        colClasses = tapply(names(colClasses), colClasses, c, 
            simplify = FALSE)
    ans = .Call(Creadfile, input, sep, as.integer(nrows), header, 
        na.strings, verbose, as.integer(autostart), skip, select, 
        drop, colClasses, integer64, dec, encoding, quote, strip.white, 
        blank.lines.skip, fill, showProgress)
    nr = length(ans[[1]])
    if ((!"bit64" %chin% loadedNamespaces()) && any(sapply(ans, 
        inherits, "integer64"))) 
        require_bit64()
    setattr(ans, "row.names", .set_row_names(nr))
    if (isTRUE(data.table)) {
        setattr(ans, "class", c("data.table", "data.frame"))
        alloc.col(ans)
    }
    else {
        setattr(ans, "class", "data.frame")
    }
    if (check.names) {
        setattr(ans, "names", make.names(names(ans), unique = TRUE))
    }
    cols = NULL
    if (stringsAsFactors) 
        cols = which(vapply(ans, is.character, TRUE))
    else if (length(colClasses)) {
        if (is.list(colClasses) && "factor" %in% names(colClasses)) 
            cols = colClasses[["factor"]]
        else if (is.character(colClasses) && "factor" %chin% 
            colClasses) 
            cols = which(colClasses == "factor")
    }
    setfactor(ans, cols, verbose)
    if (!missing(select)) {
        if (is.numeric(select)) {
            reorder = if (length(o <- forderv(select))) 
                o
            else seq_along(select)
        }
        else {
            reorder = select[select %chin% names(ans)]
        }
        setcolorder(ans, reorder)
    }
    if (!missing(col.names)) 
        setnames(ans, col.names)
    if (!is.null(key) && data.table) {
        if (!is.character(key)) 
            stop("key argument of data.table() must be character")
        if (length(key) == 1L) {
            key = strsplit(key, split = ",")[[1L]]
        }
        setkeyv(ans, key)
    }
    ans
}


yday <- function (x) 
as.POSIXlt(x)$yday + 1L


set2key <- function (...) 
{
    warning("set2key() will be deprecated in the next relase. Please use setindex() instead.", 
        call. = FALSE)
    setkey(..., physical = FALSE)
}


.__C__ITime <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("ITime", package = "data.table")
    , package = "data.table"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


setcolorder <- function (x, neworder) 
{
    if (length(neworder) != length(x)) 
        stop("neworder is length ", length(neworder), " but x has ", 
            length(x), " columns.")
    if (is.character(neworder)) {
        if (any(duplicated(neworder))) 
            stop("neworder contains duplicate column names")
        if (any(duplicated(names(x)))) 
            stop("x has some duplicated column name(s): ", paste(names(x)[duplicated(names(x))], 
                collapse = ","), ". Please remove or rename the duplicate(s) and try again.")
        o = as.integer(chmatch(neworder, names(x)))
        if (any(is.na(o))) 
            stop("Names in neworder not found in x: ", paste(neworder[is.na(o)], 
                collapse = ","))
    }
    else {
        if (!is.numeric(neworder)) 
            stop("neworder is not a character or numeric vector")
        o = as.integer(neworder)
        m = !(o %in% seq_len(length(x)))
        if (any(m)) 
            stop("Column numbers in neworder out of bounds: ", 
                paste(o[m], collapse = ","))
    }
    .Call(Csetcolorder, x, o)
    invisible(x)
}


last <- function (x, ...) 
{
    if (nargs() == 1L) {
        if (is.vector(x)) {
            if (!length(x)) 
                return(x)
            else return(x[[length(x)]])
        }
        else if (is.data.frame(x)) 
            return(x[NROW(x), ])
    }
    if (!requireNamespace("xts", quietly = TRUE)) {
        tail(x, n = 1L, ...)
    }
    else {
        if (!"package:xts" %in% search()) {
            tail(x, n = 1L, ...)
        }
        else xts::last(x, ...)
    }
}


rowidv <- function (x, cols = seq_along(x), prefix = NULL) 
{
    if (!is.null(prefix) && (!is.character(prefix) || length(prefix) != 
        1L)) 
        stop("prefix must be NULL or a character vector of length=1.")
    if (is.atomic(x)) {
        if (!missing(cols) && !is.null(cols)) 
            stop("x is a single vector, non-NULL 'cols' doesn't make sense.")
        cols = 1L
        x = as_list(x)
    }
    else {
        if (!length(cols)) 
            stop("x is a list, 'cols' can not be on 0-length.")
        if (is.character(cols)) 
            cols = chmatch(cols, names(x))
        cols = as.integer(cols)
    }
    xorder = forderv(x, by = cols, sort = FALSE, retGrp = TRUE)
    xstart = attr(xorder, "start")
    if (!length(xorder)) 
        xorder = seq_along(x[[1L]])
    ids = .Call(Cfrank, xorder, xstart, uniqlengths(xstart, length(xorder)), 
        "sequence")
    if (!is.null(prefix)) 
        ids = paste0(prefix, ids)
    ids
}


as.chron.ITime <- function (x, date = NULL, ...) 
{
    if (!requireNamespace("chron", quietly = TRUE)) 
        stop("Install suggested `chron` package to use `as.chron.ITime` function.")
    else {
        if (!is.null(date)) {
            chron::chron(dates. = chron::as.chron(as.Date(date)), 
                times. = chron::as.chron(x))
        }
        else {
            chron::chron(times. = as.character(x))
        }
    }
}


`%inrange%` <- function (x, y) 
inrange(x, y[[1L]], y[[2L]], incbounds = TRUE)


minute <- function (x) 
as.POSIXlt(x)$min


setindex <- function (...) 
setkey(..., physical = FALSE)


rleidv <- function (x, cols = seq_along(x), prefix = NULL) 
{
    if (!is.null(prefix) && (!is.character(prefix) || length(prefix) != 
        1L)) 
        stop("prefix must be NULL or a character vector of length=1.")
    if (is.atomic(x)) {
        if (!missing(cols) && !is.null(cols)) 
            stop("x is a single vector, non-NULL 'cols' doesn't make sense.")
        cols = 1L
        x = as_list(x)
    }
    else {
        if (!length(cols)) 
            stop("x is a list, 'cols' can not be 0-length.")
        if (is.character(cols)) 
            cols = chmatch(cols, names(x))
        cols = as.integer(cols)
    }
    ids = .Call(Crleid, x, cols)
    if (!is.null(prefix)) 
        ids = paste0(prefix, ids)
    ids
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

`%chin%` <- function (x, table) 
{
    .Call(Cchmatchwrapper, x, table, NA_integer_, TRUE)
}


data.table <- function (..., keep.rownames = FALSE, check.names = FALSE, key = NULL, 
    stringsAsFactors = FALSE) 
{
    x <- list(...)
    if (!.R.listCopiesNamed) 
        .Call(CcopyNamedInList, x)
    if (identical(x, list(NULL)) || identical(x, list(list())) || 
        identical(x, list(data.frame(NULL))) || identical(x, 
        list(data.table(NULL)))) 
        return(null.data.table())
    tt <- as.list(substitute(list(...)))[-1L]
    vnames = names(tt)
    if (is.null(vnames)) 
        vnames = rep.int("", length(x))
    vnames[is.na(vnames)] = ""
    novname = vnames == ""
    if (any(!novname)) {
        if (any(vnames[!novname] == ".SD")) 
            stop("A column may not be called .SD. That has special meaning.")
    }
    for (i in which(novname)) {
        if (is.null(ncol(x[[i]]))) {
            if ((tmp <- deparse(tt[[i]])[1]) == make.names(tmp)) 
                vnames[i] <- tmp
        }
    }
    tt = vnames == ""
    if (any(tt)) 
        vnames[tt] = paste("V", which(tt), sep = "")
    n <- length(x)
    if (n < 1L) 
        return(null.data.table())
    if (length(vnames) != n) 
        stop("logical error in vnames")
    vnames <- as.list.default(vnames)
    nrows = integer(n)
    numcols = integer(n)
    for (i in seq_len(n)) {
        xi = x[[i]]
        if (is.null(xi)) 
            stop("column or argument ", i, " is NULL")
        if ("POSIXlt" %chin% class(xi)) {
            warning("POSIXlt column type detected and converted to POSIXct. We do not recommend use of POSIXlt at all because it uses 40 bytes to store one date.")
            x[[i]] = as.POSIXct(xi)
        }
        else if (is.matrix(xi) || is.data.frame(xi)) {
            xi = as.data.table(xi, keep.rownames = keep.rownames)
            x[[i]] = xi
            numcols[i] = length(xi)
        }
        else if (is.table(xi)) {
            x[[i]] = xi = as.data.table.table(xi, keep.rownames = keep.rownames)
            numcols[i] = length(xi)
        }
        else if (is.function(xi)) {
            x[[i]] = xi = list(xi)
        }
        nrows[i] <- NROW(xi)
        if (numcols[i] > 0L) {
            namesi <- names(xi)
            if (length(namesi) == 0L) 
                namesi = rep.int("", ncol(xi))
            namesi[is.na(namesi)] = ""
            tt = namesi == ""
            if (any(tt)) 
                namesi[tt] = paste("V", which(tt), sep = "")
            if (novname[i]) 
                vnames[[i]] = namesi
            else vnames[[i]] = paste(vnames[[i]], namesi, sep = ".")
        }
    }
    nr <- max(nrows)
    ckey = NULL
    recycledkey = FALSE
    for (i in seq_len(n)) {
        xi = x[[i]]
        if (is.data.table(xi) && haskey(xi)) {
            if (nrows[i] < nr) 
                recycledkey = TRUE
            else ckey = c(ckey, key(xi))
        }
    }
    for (i in which(nrows < nr)) {
        xi <- x[[i]]
        if (identical(xi, list())) {
            x[[i]] = vector("list", nr)
            next
        }
        if (nrows[i] == 0L) 
            stop("Item ", i, " has no length. Provide at least one item (such as NA, NA_integer_ etc) to be repeated to match the ", 
                nr, " rows in the longest column. Or, all columns can be 0 length, for insert()ing rows into.")
        if (nr%%nrows[i] != 0L) 
            warning("Item ", i, " is of size ", nrows[i], " but maximum size is ", 
                nr, " (recycled leaving remainder of ", nr%%nrows[i], 
                " items)")
        if (is.data.frame(xi)) {
            ..i = rep(seq_len(nrow(xi)), length.out = nr)
            x[[i]] = xi[..i, , drop = FALSE]
            next
        }
        if (is.atomic(xi) || is.list(xi)) {
            x[[i]] = rep(xi, length.out = nr)
            next
        }
        stop("problem recycling column ", i, ", try a simpler type")
        stop("argument ", i, " (nrow ", nrows[i], ") cannot be recycled without remainder to match longest nrow (", 
            nr, ")")
    }
    if (any(numcols > 0L)) {
        value = vector("list", sum(pmax(numcols, 1L)))
        k = 1L
        for (i in seq_len(n)) {
            if (is.list(x[[i]]) && !is.ff(x[[i]])) {
                for (j in seq_len(length(x[[i]]))) {
                  value[[k]] = x[[i]][[j]]
                  k = k + 1L
                }
            }
            else {
                value[[k]] = x[[i]]
                k = k + 1L
            }
        }
    }
    else {
        value = x
    }
    vnames <- unlist(vnames)
    if (check.names) 
        vnames <- make.names(vnames, unique = TRUE)
    setattr(value, "names", vnames)
    setattr(value, "row.names", .set_row_names(nr))
    setattr(value, "class", c("data.table", "data.frame"))
    if (!is.null(key)) {
        if (!is.character(key)) 
            stop("key argument of data.table() must be character")
        if (length(key) == 1L) {
            key = strsplit(key, split = ",")[[1L]]
        }
        setkeyv(value, key)
    }
    else {
        if (length(ckey) && !recycledkey && !any(duplicated(ckey)) && 
            all(ckey %in% names(value)) && !any(duplicated(names(value)[names(value) %in% 
            ckey]))) 
            setattr(value, "sorted", ckey)
    }
    if (isTRUE(stringsAsFactors)) 
        setfactor(value, which(vapply(value, is.character, TRUE)), 
            FALSE)
    alloc.col(value)
}


`%between%` <- function (x, y) 
between(x, y[[1]], y[[2]], incbounds = TRUE)


setNumericRounding <- function (x) 
{
    .Call(CsetNumericRounding, as.integer(x))
    invisible()
}


setattr <- function (x, name, value) 
{
    if (name == "names" && is.data.table(x) && length(attr(x, 
        "names")) && !is.null(value)) 
        setnames(x, value)
    else {
        ans = .Call(Csetattrib, x, name, value)
        if (!is.null(ans)) {
            warning("Input is a length=1 logical that points to the same address as R's global TRUE value. Therefore the attribute has not been set by reference, rather on a copy. You will need to assign the result back to a variable. See https://github.com/Rdatatable/data.table/issues/1281 for more.")
            x = ans
        }
    }
    if (name == "levels" && is.factor(x) && anyDuplicated(value)) 
        .Call(Csetlevels, x, (value <- as.character(value)), 
            unique(value))
    invisible(x)
}


`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

.SD <- NULL


as.data.table <- function (x, keep.rownames = FALSE, ...) 
{
    if (is.null(x)) 
        return(null.data.table())
    UseMethod("as.data.table")
}


CJ <- function (..., sorted = TRUE, unique = FALSE) 
{
    l = list(...)
    if (unique) 
        l = lapply(l, unique)
    dups = FALSE
    j = lapply(l, class)
    if (length(l) == 1L && sorted && length(o <- forderv(l[[1L]]))) 
        l[[1L]] = l[[1L]][o]
    else if (length(l) > 1L) {
        n = vapply(l, length, 0L)
        nrow = prod(n)
        x = c(rev(take(cumprod(rev(n)))), 1L)
        for (i in seq_along(x)) {
            y = l[[i]]
            if (sorted) {
                if (length(o <- forderv(y, retGrp = TRUE))) 
                  y = y[o]
                if (!dups) 
                  dups = attr(o, "maxgrpn") > 1L
            }
            if (i == 1L) 
                l[[i]] = rep.int(y, times = rep.int(x[i], n[i]))
            else if (i == length(n)) 
                l[[i]] = rep.int(y, times = nrow/(x[i] * n[i]))
            else l[[i]] = rep.int(rep.int(y, times = rep.int(x[i], 
                n[i])), times = nrow/(x[i] * n[i]))
            if (any(class(l[[i]]) != j[[i]])) 
                setattr(l[[i]], "class", j[[i]])
        }
    }
    setattr(l, "row.names", .set_row_names(length(l[[1L]])))
    setattr(l, "class", c("data.table", "data.frame"))
    if (is.null(vnames <- names(l))) 
        vnames = vector("character", length(l))
    if (any(tt <- vnames == "")) {
        vnames[tt] = paste("V", which(tt), sep = "")
        setattr(l, "names", vnames)
    }
    l <- alloc.col(l)
    if (sorted) {
        if (!dups) 
            setattr(l, "sorted", names(l))
        else setkey(l)
    }
    l
}


haskey <- function (x) 
!is.null(key(x))


test.data.table <- function (verbose = FALSE, pkg = "pkg", silent = FALSE) 
{
    if (exists("test.data.table", .GlobalEnv, inherits = FALSE)) {
        if ("package:data.table" %in% search()) 
            stop("data.table package loaded")
        if (.Platform$OS.type == "unix" && Sys.info()["sysname"] != 
            "Darwin") 
            d = path.expand("~/data.table/inst/tests")
        else {
            if (!pkg %in% dir()) 
                stop(paste(pkg, " not in dir()", sep = ""))
            d = paste(getwd(), "/", pkg, "/inst/tests", sep = "")
        }
    }
    else {
        d = paste(getNamespaceInfo("data.table", "path"), "/tests", 
            sep = "")
    }
    oldenc = options(encoding = "UTF-8")[[1L]]
    olddir = setwd(d)
    on.exit(setwd(olddir))
    envirs <- list()
    for (fn in file.path(d, "tests.Rraw")) {
        cat("Running", fn, "\n")
        oldverbose = options(datatable.verbose = verbose)
        envirs[[fn]] = new.env(parent = .GlobalEnv)
        if (isTRUE(silent)) {
            try(sys.source(fn, envir = envirs[[fn]]), silent = silent)
        }
        else {
            sys.source(fn, envir = envirs[[fn]])
        }
        options(oldverbose)
    }
    options(encoding = oldenc)
    invisible(sum(sapply(envirs, `[[`, "nfail")) == 0)
}


as.IDate <- function (x, ...) 
UseMethod("as.IDate")


between <- function (x, lower, upper, incbounds = TRUE) 
{
    is_strictly_numeric <- function(x) is.numeric(x) && !"integer64" %in% 
        class(x)
    if (is_strictly_numeric(x) && is_strictly_numeric(lower) && 
        is_strictly_numeric(upper) && length(lower) == 1L && 
        length(upper) == 1L) {
        .Call(Cbetween, x, lower, upper, incbounds)
    }
    else {
        if (incbounds) 
            x >= lower & x <= upper
        else x > lower & x < upper
    }
}


mday <- function (x) 
as.POSIXlt(x)$mday


as.Date.IDate <- function (x, ...) 
{
    structure(as.numeric(x), class = "Date")
}


quarter <- function (x) 
as.POSIXlt(x)$mon%/%3L + 1L


alloc.col <- function (DT, n = getOption("datatable.alloccol"), verbose = getOption("datatable.verbose")) 
{
    name = substitute(DT)
    if (identical(name, quote(`*tmp*`))) 
        stop("alloc.col attempting to modify `*tmp*`")
    ans = .Call(Calloccolwrapper, DT, length(DT) + as.integer(eval(n)), 
        verbose)
    if (is.name(name)) {
        name = as.character(name)
        assign(name, ans, parent.frame(), inherits = TRUE)
    }
    .Call(Csetnamed, ans, 0L)
}


`key<-` <- function (x, value) 
{
    warning("The key(x)<-value form of setkey can copy the whole table. This is due to <- in R itself. Please change to setkeyv(x,value) or setkey(x,...) which do not copy and are faster. See help('setkey'). You can safely ignore this warning if it is inconvenient to change right now. Setting options(warn=2) turns this warning into an error, so you can then use traceback() to find and change your key<- calls.")
    setkeyv(x, value)
}


.rbind.data.table <- function (..., use.names = TRUE, fill = FALSE, idcol = NULL) 
{
    l = lapply(list(...), function(x) if (is.list(x)) 
        x
    else as.data.table(x))
    rbindlist(l, use.names, fill, idcol)
}


address <- function (x) 
.Call(Caddress, eval(substitute(x), parent.frame()))


first <- function (x, ...) 
{
    if (nargs() == 1L) {
        if (is.vector(x)) {
            if (!length(x)) 
                return(x)
            else return(x[[1L]])
        }
        else if (is.data.frame(x)) 
            return(x[1L, ])
    }
    if (!requireNamespace("xts", quietly = TRUE)) {
        head(x, n = 1L, ...)
    }
    else {
        if (!"package:xts" %in% search()) {
            head(x, n = 1L, ...)
        }
        else xts::first(x, ...)
    }
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Extension of `data.frame`"

.skeleton_package_version = "1.10.4"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods"


## Internal

.skeleton_version = 5


## EOF