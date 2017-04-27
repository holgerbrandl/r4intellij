##
## Exported symobls in package `lubridate`
##

## Exported package methods

dst <- function (x) 
UseMethod("dst")


am <- function (x) 
hour(x) < 12


month <- function (x, label = FALSE, abbr = TRUE) 
UseMethod("month")


make_datetime <- function (year = 1970L, month = 1L, day = 1L, hour = 0L, min = 0L, 
    sec = 0, tz = "UTC") 
{
    lengths <- vapply(list(year, month, day, hour, min, sec), 
        length, 1, USE.NAMES = FALSE)
    if (min(lengths) == 0L) {
        .POSIXct(numeric(), tz = tz)
    }
    else {
        N <- max(lengths)
        .POSIXct(.Call("make_dt", rep_len(as.integer(year), N), 
            rep_len(as.integer(month), N), rep_len(as.integer(day), 
                N), rep_len(as.integer(hour), N), rep_len(as.integer(min), 
                N), rep_len(sec, N)), tz = tz)
    }
}


is.duration <- function (x) 
is(x, "Duration")


`.__T__as.duration:lubridate` <- "<environment>"

`.__T__minute<-:lubridate` <- "<environment>"

int_aligns <- function (int1, int2) 
{
    int1 <- int_standardize(int1)
    int2 <- int_standardize(int2)
    int1@start == int2@start | (int1@start + int1@.Data) == (int2@start + 
        int2@.Data)
}


now <- function (tzone = "") 
with_tz(Sys.time(), tzone)


int_flip <- function (int) 
{
    new("Interval", -int@.Data, start = int@start + int@.Data, 
        tzone = int@tzone)
}


days_in_month <- function (x) 
{
    month_x <- month(x, label = TRUE)
    n_days <- N_DAYS_IN_MONTHS[month_x]
    n_days[month_x == "Feb" & leap_year(x)] <- 29L
    n_days
}


is.POSIXlt <- function (x) 
inherits(x, "POSIXlt")


`minute<-` <- function (x, value) 
standardGeneric("minute<-")


`.__T__union:base` <- "<environment>"

`.__T__Arith:base` <- "<environment>"

`.__T__%/%:base` <- "<environment>"

`yday<-` <- function (x, value) 
x <- x + days(value - yday(x))


`year<-` <- function (x, value) 
standardGeneric("year<-")


`.__T__/:base` <- "<environment>"

`.__T__show:methods` <- "<environment>"

emicroseconds <- function (x = 1) 
{
    .deprecated_fun("dmicroseconds", "1.5.0")
    new("Duration", x/1000/1000)
}


`%within%` <- function (a, b) 
standardGeneric("%within%")


int_standardize <- function (int) 
{
    negs <- !is.na(int@.Data) & int@.Data < 0
    int[negs] <- int_flip(int[negs])
    int
}


new_interval <- function (...) 
{
    .deprecated_fun("interval", "1.5.0")
    interval(...)
}


`.__T__time_length:lubridate` <- "<environment>"

`.__T__month<-:lubridate` <- "<environment>"

int_overlaps <- function (int1, int2) 
{
    stopifnot(c(is.interval(int1), is.interval(int2)))
    int1 <- int_standardize(int1)
    int2 <- int_standardize(int2)
    int1@start <= int2@start + int2@.Data & int2@start <= int1@start + 
        int1@.Data
}


mdy_hm <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = "mdyR", quiet = quiet, tz = tz, 
    locale = locale, truncated = truncated)


dseconds <- function (x = 1) 
new("Duration", x)


int_shift <- function (int, by) 
{
    if (!is.timespan(by)) 
        stop("by is not a recognized timespan object")
    if (is.interval(by)) 
        stop("an interval cannot be shifted by another interval.\n    Convert second interval to a period or duration.")
    interval(int@start + by, int_end(int) + by)
}


nanoseconds <- function (x = 1) 
seconds(x/1e+09)


`.__T__rep:base` <- "<environment>"

decimal_date <- function (date) 
UseMethod("decimal_date")


ms <- function (..., quiet = FALSE, roll = FALSE) 
{
    out <- .parse_hms(..., order = "MS", quiet = quiet)
    if (roll) {
        hms <- .roll_hms(min = out["M", ], sec = out["S", ])
        period(hour = hms$hour, minute = hms$min, second = hms$sec)
    }
    else {
        period(minute = out["M", ], second = out["S", ])
    }
}


as.difftime <- function (tim, format = "%X", units = "auto") 
standardGeneric("as.difftime")


new_duration <- function (...) 
{
    .deprecated_fun("duration", "1.5.0")
    duration(...)
}


.__C__Interval <- new("classRepresentation"
    , slots = structure(list(.Data = structure("numeric", package = "methods"), 
    start = structure("POSIXct", package = "methods"), tzone = structure("character", package = "methods")), .Names = c(".Data", 
"start", "tzone"))
    , contains = structure(list(Timespan = S4_object(), 
    numeric = S4_object(), 
    vector = S4_object()), .Names = c("Timespan", 
"numeric", "vector"))
    , virtual = FALSE
    , prototype = new("numeric"
)
    , validity = function (object) 
{
    errors <- character()
    if (!is.numeric(object@.Data)) {
        msg <- "Span length must be numeric."
        errors <- c(errors, msg)
    }
    if (!is(object@start, "POSIXct")) {
        msg <- "Start date must be in POSIXct format."
        errors <- c(errors, msg)
    }
    if (length(object@.Data) != length(object@start)) {
        msg <- paste("Inconsistent lengths: spans = ", length(object@.Data), 
            ", start dates = ", length(object@start), sep = "")
        errors <- c(errors, msg)
    }
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("Interval", package = "lubridate")
    , package = "lubridate"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


leap_year <- function (date) 
{
    recognized <- recognize(date)
    if (recognized) 
        year <- year(date)
    else if (all(is.numeric(date))) 
        year <- date
    else stop("unrecognized date format")
    (year%%4 == 0) & ((year%%100 != 0) | (year%%400 == 0))
}


`.__T__as.numeric:base` <- "<environment>"

`month<-` <- function (x, value) 
standardGeneric("month<-")


int_start <- function (int) 
int@start


make_difftime <- function (num = NULL, units = "auto", ...) 
{
    pieces <- list(...)
    if (!is.null(num) && length(pieces) > 0) {
        .difftime_from_num(c(num, .difftime_from_pieces(pieces)), 
            units)
    }
    else if (!is.null(num)) {
        .difftime_from_num(num, units)
    }
    else if (length(pieces)) {
        .difftime_from_num(.difftime_from_pieces(pieces), units)
    }
    else {
        stop("No valid values have been passed to 'make_difftime' constructor")
    }
}


`hour<-` <- function (x, value) 
standardGeneric("hour<-")


olson_time_zones <- function (order_by = c("name", "longitude")) 
{
    .deprecated_fun("OlsonNames", "1.5.8")
    order_by <- match.arg(order_by)
    tzdirs <- c(R.home("share"), "/usr/share", "/usr/share/lib", 
        "/usr/lib/zoneinfo", "/usr/local/etc", "/etc", "/usr/etc")
    tzfiles <- c(file.path(tzdirs, "zoneinfo", "zone.tab"), "/usr/share/lib/zoneinfo/tab/zone_sun.tab")
    tzfiles <- tzfiles[file.exists(tzfiles)]
    if (length(tzfiles) == 0) {
        warning("zone.tab file not found in any candidate locations: ", 
            str_join(tzdirs, collapse = " "))
    }
    tzfile <- tzfiles[[1]]
    tzones <- read.delim(tzfile, row.names = NULL, header = FALSE, 
        col.names = c("country", "coords", "name", "comments"), 
        as.is = TRUE, fill = TRUE, comment.char = "#")
    o <- order(switch(order_by, name = tzones$name, longitude = {
        longitude_string <- stringr::str_match(tzones$coords, 
            "[+-][[:digit:]]+([+-][[:digit:]]+)")[, 2]
        nch <- nchar(longitude_string)
        sign <- ifelse(substring(longitude_string, 1, 1) == "+", 
            1, -1)
        nss <- function(first, last) {
            as.numeric(substring(longitude_string, first, last))
        }
        sign * ifelse(nch == 5, 3600 * nss(2, 3) + 60 * nss(4, 
            5), ifelse(nch == 6, 3600 * nss(2, 4) + 60 * nss(6, 
            6), ifelse(nch == 7, 3600 * nss(2, 3) + 60 * nss(4, 
            5) + nss(6, 7), 3600 * nss(2, 4) + 60 * nss(6, 6) + 
            nss(7, 8))))
    }))
    tzones$name[o]
}


`.__T__[:base` <- "<environment>"

as_datetime <- function (x, ...) 
{
    standardGeneric("as_datetime")
}


is.difftime <- function (x) 
is(x, "difftime")


as_date <- function (x, ...) 
standardGeneric("as_date")


`.__T__second<-:lubridate` <- "<environment>"

day <- function (x) 
UseMethod("mday")


is.Date <- function (x) 
is(x, "Date")


round_date <- function (x, unit = "second") 
{
    if (!length(x)) 
        return(x)
    parsed_unit <- parse_period_unit(unit)
    n <- parsed_unit$n
    basic_unit <- standardise_period_names(parsed_unit$unit)
    new <- if (n == 1 && basic_unit %in% c("second", "minute", 
        "hour", "day")) {
        round.POSIXt(x, units = lub2base_units[[basic_unit]])
    }
    else {
        above <- unclass(as.POSIXct(ceiling_date(x, unit)))
        mid <- unclass(as.POSIXct(x))
        below <- unclass(as.POSIXct(floor_date(x, unit)))
        wabove <- (above - mid) <= (mid - below)
        wabove <- !is.na(wabove) & wabove
        new <- below
        new[wabove] <- above[wabove]
        .POSIXct(new, tz = tz(x))
    }
    reclass_date(new, x)
}


`qday<-` <- function (x, value) 
standardGeneric("qday<-")


hour <- function (x) 
UseMethod("hour")


int_diff <- function (times) 
{
    interval(times[-length(times)], times[-1])
}


`.__T__%m+%:lubridate` <- "<environment>"

milliseconds <- function (x = 1) 
seconds(x/1000)


ceiling_date <- function (x, unit = "seconds", change_on_boundary = NULL) 
{
    if (!length(x)) 
        return(x)
    parsed_unit <- parse_period_unit(unit)
    n <- parsed_unit$n
    unit <- standardise_period_names(parsed_unit$unit)
    if (is.null(change_on_boundary)) {
        change_on_boundary <- is.Date(x)
    }
    if (unit == "second") {
        sec <- second(x)
        csec <- ceil_multi_unit(sec, n)
        if (!change_on_boundary) {
            wsec <- which(csec - n == sec)
            if (length(wsec)) 
                csec[wsec] <- sec[wsec]
        }
        update(x, seconds = csec, simple = T)
    }
    else if (unit %in% c("minute", "hour", "day")) {
        new <- as_datetime(x, tz = tz(x))
        delta <- switch(unit, minute = 60, hour = 3600, day = 86400) * 
            n
        new <- if (change_on_boundary) {
            trunc_multi_unit(new, unit, n) + delta
        }
        else {
            new1 <- trunc_multi_unit(new, unit, n)
            not_same <- new1 != new
            new1[not_same] <- new1[not_same] + delta
            new1
        }
        reclass_date_maybe(new, x, unit)
    }
    else {
        if (n > 1 && unit == "week") {
            warning("Multi-unit not supported for weeks. Ignoring.")
        }
        new <- if (change_on_boundary) 
            x
        else update(x, seconds = second(x) - 1e-05, simple = T)
        if (unit %in% c("bimonth", "quarter", "halfyear")) {
            switch(unit, bimonth = n <- 2 * n, quarter = n <- 3 * 
                n, halfyear = n <- 6 * n)
            unit <- "month"
        }
        new <- switch(unit, minute = update(new, minute = ceil_multi_unit(minute(new), 
            n), second = 0, simple = T), hour = update(new, hour = ceil_multi_unit(hour(new), 
            n), minute = 0, second = 0, simple = T), week = update(new, 
            wday = 8, hour = 0, minute = 0, second = 0), month = update(new, 
            month = ceil_multi_unit1(month(new), n), mdays = 1, 
            hours = 0, minutes = 0, seconds = 0), year = update(new, 
            year = ceil_multi_unit(year(new), n), month = 1, 
            mday = 1, hour = 0, minute = 0, second = 0))
        reclass_date_maybe(new, x, unit)
    }
}


Arith <- methods::Arith # re-exported from methods package

yq <- function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME")) 
.parse_xxx(..., orders = "yq", quiet = quiet, tz = tz, locale = locale, 
    truncated = 0)


period_to_seconds <- function (x) 
{
    x@.Data + 60 * x@minute + 60 * 60 * x@hour + 60 * 60 * 24 * 
        x@day + 60 * 60 * 24 * 365.25/12 * x@month + 60 * 60 * 
        24 * 365.25 * x@year
}


origin <- structure(0, class = c("POSIXct", "POSIXt"), tzone = "UTC")


myd <- function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx(..., orders = "myd", quiet = quiet, tz = tz, locale = locale, 
    truncated = truncated)


`.__T__$:base` <- "<environment>"

`.__T__as_date:base` <- "<environment>"

intersect <- function (x, y) 
standardGeneric("intersect")


`.__T__%within%:lubridate` <- "<environment>"

`.__T__as.character:base` <- "<environment>"

interval <- function (start, end, tzone = attr(start, "tzone")) 
{
    if (is.null(tzone)) {
        tzone <- if (is.null(attr(end, "tzone"))) 
            "UTC"
        else attr(end, "tzone")
    }
    if (is.Date(start)) 
        start <- with_tz(as.POSIXct(start), "UTC")
    if (is.Date(end)) 
        end <- with_tz(as.POSIXct(end), "UTC")
    if (!is.POSIXct(start)) 
        start <- as.POSIXct(start, tz = tzone)
    if (!is.POSIXct(end)) 
        end <- as.POSIXct(end, tz = tzone)
    span <- as.numeric(end) - as.numeric(start)
    starts <- start + rep(0, length(span))
    if (tzone != tz(starts)) 
        starts <- with_tz(starts, tzone)
    new("Interval", span, start = starts, tzone = tzone)
}


show <- methods::show # re-exported from methods package

seconds <- function (x = 1) 
period(second = x)


mdy_hms <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = c("mdyTz", "mdyT"), quiet = quiet, 
    tz = tz, locale = locale, truncated = truncated)


microseconds <- function (x = 1) 
seconds(x/1e+06)


force_tz <- function (time, tzone = "") 
{
    if (is.data.frame(time)) {
        for (nm in names(time)) {
            if (is.POSIXt(time[[nm]])) {
                time[[nm]] <- force_tz(time[[nm]], tzone = tzone)
            }
        }
        time
    }
    else {
        check_tz(tzone)
        update(time, tz = tzone)
    }
}


is.timespan <- function (x) 
is(x, "Timespan")


is.POSIXct <- function (x) 
inherits(x, "POSIXct")


dmicroseconds <- function (x = 1) 
new("Duration", x/1000/1000)


quarter <- function (x, with_year = FALSE) 
{
    m <- month(x)
    quarters <- rep(1:4, each = 3)
    q <- quarters[m]
    if (with_year) 
        year(x) + q/10
    else q
}


`int_start<-` <- function (int, value) 
{
    value <- as.POSIXct(value)
    span <- as.numeric(int@start + int@.Data - value, "secs")
    equal.lengths <- data.frame(span, value)
    int <- new("Interval", span, start = equal.lengths$value, 
        tzone = int@tzone)
}


`mday<-` <- function (x, value) 
{
    day(x) <- value
    x
}


emilliseconds <- function (x = 1) 
{
    .deprecated_fun("dmilliseconds", "1.5.0")
    new("Duration", x/1000)
}


days <- function (x = 1) 
period(day = x)


period <- function (num = NULL, units = "second", ...) 
{
    nums <- list(...)
    if (is.character(num)) {
        parse_period(num)
    }
    else if (!is.null(num) && length(nums) > 0) {
        c(.period_from_num(num, units), .period_from_units(nums))
    }
    else if (!is.null(num)) {
        .period_from_num(num, units)
    }
    else if (length(nums)) {
        .period_from_units(nums)
    }
    else {
        stop("No valid values have been passed to 'period' constructor")
    }
}


parse_date_time2 <- function (x, orders, tz = "UTC", exact = FALSE, lt = FALSE) 
{
    if (length(orders) > 1) 
        warning("Multiple orders supplied. Only first order is used.")
    if (!exact) 
        orders <- gsub("[^[:alpha:]]+", "", as.character(orders[[1]]))
    if (lt) {
        .mklt(.Call("parse_dt", x, orders, FALSE, TRUE), tz)
    }
    else {
        if (tz == "UTC") {
            .POSIXct(.Call("parse_dt", x, orders, FALSE, FALSE), 
                tz = "UTC")
        }
        else {
            as.POSIXct(.mklt(.Call("parse_dt", x, orders, FALSE, 
                TRUE), tz))
        }
    }
}


`.__T__+:base` <- "<environment>"

`%m+%` <- function (e1, e2) 
standardGeneric("%m+%")


week <- function (x) 
(yday(x) - 1)%/%7 + 1


today <- function (tzone = "") 
{
    as.Date(force_tz(floor_date(now(tzone), "day"), tzone = "UTC"))
}


as.interval <- function (x, start, ...) 
standardGeneric("as.interval")


year <- function (x) 
UseMethod("year")


`.__T__qday<-:lubridate` <- "<environment>"

floor_date <- function (x, unit = "seconds") 
{
    if (!length(x)) 
        return(x)
    parsed_unit <- parse_period_unit(unit)
    n <- parsed_unit$n
    unit <- standardise_period_names(parsed_unit$unit)
    if (unit %in% c("second", "minute", "hour", "day")) {
        out <- trunc_multi_unit(x, unit, n)
        reclass_date_maybe(out, x, unit)
    }
    else {
        if (n > 1 && unit == "week") {
            warning("Multi-unit not supported for weeks. Ignoring.")
        }
        if (unit %in% c("bimonth", "quarter", "halfyear")) {
            switch(unit, bimonth = n <- 2 * n, quarter = n <- 3 * 
                n, halfyear = n <- 6 * n)
            unit <- "month"
        }
        switch(unit, week = update(x, wdays = 1, hours = 0, minutes = 0, 
            seconds = 0), month = {
            if (n > 1) update(x, months = floor_multi_unit1(month(x), 
                n), mdays = 1, hours = 0, minutes = 0, seconds = 0) else update(x, 
                mdays = 1, hours = 0, minutes = 0, seconds = 0)
        }, year = {
            if (n > 1) {
                y <- update(x, ydays = 1, hours = 0, minutes = 0, 
                  seconds = 0)
                update(y, years = floor_multi_unit(year(y), n))
            } else {
                update(x, ydays = 1, hours = 0, minutes = 0, 
                  seconds = 0)
            }
        })
    }
}


dmy_h <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = "dmyR", quiet = quiet, tz = tz, 
    locale = locale, truncated = truncated)


pm <- function (x) 
!am(x)


dmy <- function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx(..., orders = "dmy", quiet = quiet, tz = tz, locale = locale, 
    truncated = truncated)


make_date <- function (year = 1970L, month = 1L, day = 1L) 
{
    lengths <- vapply(list(year, month, day), length, 1, USE.NAMES = FALSE)
    if (min(lengths) == 0L) {
        as.Date(integer(), origin = origin)
    }
    else {
        N <- max(lengths)
        secs <- .Call("make_d", rep_len(as.integer(year), N), 
            rep_len(as.integer(month), N), rep_len(as.integer(day), 
                N))
        structure(secs/86400L, class = "Date")
    }
}


`tz<-` <- function (x, value) 
{
    new <- force_tz(x, value)
    reclass_date(new, x)
}


mdy <- function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx(..., orders = "mdy", quiet = quiet, tz = tz, locale = locale, 
    truncated = truncated)


guess_formats <- function (x, orders, locale = Sys.getlocale("LC_TIME"), preproc_wday = TRUE, 
    print_matches = FALSE) 
{
    orders <- gsub("[^[:alpha:]]+", "", orders)
    if (any(grepl("hms|hm|ms", orders))) {
        .deprecated("hms, hm and ms usage", ", please use HMS, HM or MS instead", 
            "1.5.6")
        orders <- gsub("hms", "HMS", orders, ignore.case = TRUE)
        orders <- gsub("hm", "HM", orders, ignore.case = TRUE)
        orders <- gsub("ms", "MS", orders, ignore.case = TRUE)
    }
    if (length(wp <- grepl("[^O]p", orders))) 
        orders <- c(sub("p", "Op", orders[wp], fixed = T), orders)
    if (length(wm <- grepl("[^O][mbB]", orders))) 
        orders <- c(sub("[mbB]", "Om", orders[wm]), orders)
    if (length(wT <- grepl("T", orders, fixed = T))) 
        orders <- c(sub("T", "HMSOp", orders[wT], fixed = T), 
            orders)
    if (length(wR <- grepl("R", orders, fixed = T))) 
        orders <- c(sub("R", "HMOp", orders[wR], fixed = T), 
            orders)
    if (length(wr <- grepl("r", orders, fixed = T))) 
        orders <- c(sub("r", "HOp", orders[wR], fixed = T), orders)
    osplits <- strsplit(orders, "", fixed = TRUE)
    osplits <- lapply(osplits, function(ospt) {
        if (length(which_O <- which(ospt == "O")) > 0) {
            ospt[which_O + 1] <- paste("O", ospt[which_O + 1], 
                sep = "")
            ospt[-which_O]
        }
        else ospt
    })
    reg <- .get_locale_regs(locale)
    flex_regs <- c(reg$alpha_flex, reg$num_flex, .c_parser_reg_flex)
    exact_regs <- c(reg$alpha_exact, reg$num_exact, .c_parser_reg_exact)
    REGS <- unlist(lapply(osplits, function(fnames) {
        which <- !fnames %in% c(names(reg$alpha_flex), names(reg$num_flex), 
            names(.c_parser_reg_exact))
        if (any(which)) 
            stop("Unknown formats supplied: ", paste(fnames[which], 
                sep = ", "))
        paste("^\\D*?\\b((", paste(unlist(flex_regs[fnames]), 
            collapse = "\\D*?"), ")|(", paste(unlist(exact_regs[fnames]), 
            collapse = "\\D*?"), "))\\D*$", sep = "")
    }))
    if (print_matches) {
        subs <- lapply(REGS, .substitute_formats, x, fmts_only = FALSE)
        names(subs) <- orders
        print(do.call(cbind, c(list(x), subs)))
    }
    .build_formats <- function(regs, orders, x) {
        out <- mapply(function(reg, name) {
            out <- .substitute_formats(reg, x)
            if (!is.null(out)) 
                names(out) <- rep.int(name, length(out))
            out
        }, REGS, orders, SIMPLIFY = F, USE.NAMES = F)
        names(out) <- NULL
        unlist(out)
    }
    if (preproc_wday && !any(grepl("[aA]", orders))) {
        x2 <- gsub(reg$alpha_exact[["A"]], "%A", x, ignore.case = T, 
            perl = T)
        x2 <- gsub(reg$alpha_exact[["a"]], "%a", x2, ignore.case = T, 
            perl = T)
        formats <- .build_formats(REGS, orders, x2)
        if (any(grepl("%[aA]", formats))) 
            c(formats, .build_formats(REGS, orders, x))
        else formats
    }
    else {
        .build_formats(REGS, orders, x)
    }
}


stamp <- function (x, orders = lubridate_formats, locale = Sys.getlocale("LC_TIME"), 
    quiet = FALSE) 
{
    fmts <- unique(guess_formats(x, orders, locale))
    if (is.null(fmts)) 
        stop("Couldn't guess formats of: ", x)
    if (length(fmts) == 1L) {
        FMT <- fmts[[1]]
    }
    else {
        trained <- .train_formats(x, fmts)
        formats <- .select_formats(trained)
        FMT <- formats[[1]]
        if (!quiet && length(trained) > 1) {
            message("Multiple formats matched: ", paste("\"", 
                names(trained), "\"(", trained, ")", sep = "", 
                collapse = ", "))
        }
    }
    if (!quiet) 
        message("Using: \"", FMT, "\"")
    if (grepl("%O[oOzu]|%z", FMT)) {
        oOz_end <- str_extract(FMT, "%O[oOz]$")
        if (is.na(oOz_end)) {
            FMT <- sub("%O[oOz]", "%z", sub("%Ou", "Z", FMT, 
                fixed = TRUE))
            eval(bquote(function(x) {
                if (tz(x[[1]]) != "UTC") 
                  x <- with_tz(x, tzone = "UTC")
                format(x, format = .(FMT))
            }))
        }
        else {
            FMT <- sub("%O[oOz]$", "", FMT)
            eval(bquote(function(x) paste0(format(x, format = .(FMT)), 
                .format_offset(x, fmt = .(oOz_end)))))
        }
    }
    else {
        eval(bquote(function(x) format(x, format = .(FMT))))
    }
}


`.__T__%%:base` <- "<environment>"

`.__T__year<-:lubridate` <- "<environment>"

stamp_date <- function (x, locale = Sys.getlocale("LC_TIME")) 
stamp(x, orders = c("ymd", "dmy", "mdy", "ydm", "dym", "myd", 
    "my", "ym", "md", "dm", "m", "d", "y"), locale = locale)


`date<-` <- function (x, value) 
standardGeneric("date<-")


dpicoseconds <- function (x = 1) 
new("Duration", x/1000/1000/1000/1000)


is.interval <- function (x) 
is(x, c("Interval"))


here <- function () 
{
    .deprecated_fun("now", "1.5.8")
    now()
}


mdy_h <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = "mdyr", quiet = quiet, tz = tz, 
    locale = locale, truncated = truncated)


dym <- function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx(..., orders = "dym", quiet = quiet, tz = tz, locale = locale, 
    truncated = truncated)


weeks <- function (x = 1) 
period(week = x)


dminutes <- function (x = 1) 
new("Duration", x * 60)


`.__T__intersect:base` <- "<environment>"

qday <- function (x) 
UseMethod("qday")


new_period <- function (...) 
{
    .deprecated_fun("period", "1.5.0")
    period(...)
}


union <- function (x, y) 
standardGeneric("union")


duration <- function (num = NULL, units = "seconds", ...) 
{
    nums <- list(...)
    if (is.character(num)) {
        as.duration(parse_period(num))
    }
    else if (!is.null(num) && length(nums) > 0) {
        c(.duration_from_num(num, units), .duration_from_units(nums))
    }
    else if (!is.null(num)) {
        .duration_from_num(num, units)
    }
    else if (length(nums)) {
        .duration_from_units(nums)
    }
    else {
        stop("No valid values have been passed to 'duration' constructor")
    }
}


isoyear <- function (x) 
{
    xday <- make_datetime(year(x), month(x), day(x), tz = tz(x))
    dn <- 1 + (wday(x) + 5)%%7
    nth <- xday + ddays(4 - dn)
    year(nth)
}


`.__T__$<-:base` <- "<environment>"

`week<-` <- function (x, value) 
x <- x + days((value - week(x)) * 7)


eweeks <- function (x = 1) 
{
    .deprecated_fun("dweeks", "1.5.0")
    new("Duration", x * 604800)
}


epicoseconds <- function (x = 1) 
{
    .deprecated_fun("dpicoseconds", "1.5.0")
    new("Duration", x/1000/1000/1000/1000)
}


ydm_h <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = "ydmR", quiet = quiet, tz = tz, 
    locale = locale, truncated = truncated)


`.__T__as.interval:lubridate` <- "<environment>"

fit_to_timeline <- function (lt, class = "POSIXct", simple = FALSE) 
{
    if (class != "POSIXlt" && class != "POSIXct") 
        stop("class argument must be POSIXlt or POSIXct")
    if (simple) {
        if (class == "POSIXct") 
            as.POSIXct(lt)
        else as.POSIXlt(as.POSIXct(lt))
    }
    else {
        ct <- as.POSIXct(lt)
        lt2 <- as.POSIXlt(ct)
        dstdiff <- !is.na(ct) & (lt$isdst != lt2$isdst)
        if (any(dstdiff)) {
            dlt <- lt[dstdiff]
            dlt2 <- lt2[dstdiff]
            dlt$isdst <- dlt2$isdst
            dlt$zone <- dlt2$zone
            dlt$gmtoff <- dlt2$gmtoff
            dct <- as.POSIXct(dlt)
            if (class == "POSIXct") 
                ct[dstdiff] <- dct
            else lt2[dstdiff] <- dlt
            chours <- format.POSIXlt(as.POSIXlt(dct), "%H", usetz = FALSE)
            lhours <- format.POSIXlt(dlt, "%H", usetz = FALSE)
            any <- any(hdiff <- chours != lhours)
            if (!is.na(any) && any) {
                if (class == "POSIXct") 
                  ct[dstdiff][hdiff] <- NA
                else lt2[dstdiff][hdiff] <- NA
            }
        }
        if (class == "POSIXct") 
            ct
        else lt2
    }
}


`.__T__setdiff:base` <- "<environment>"

`.__T__[<-:base` <- "<environment>"

`.__T__date<-:lubridate` <- "<environment>"

hms <- function (..., quiet = FALSE, roll = FALSE) 
{
    out <- .parse_hms(..., order = "HMS", quiet = quiet)
    if (roll) {
        hms <- .roll_hms(out["H", ], out["M", ], out["S", ])
        period(hour = hms$hour, minute = hms$min, second = hms$sec)
    }
    else {
        period(hour = out["H", ], minute = out["M", ], second = out["S", 
            ])
    }
}


wday <- function (x, label = FALSE, abbr = TRUE) 
UseMethod("wday")


seconds_to_period <- function (x) 
{
    span <- as.double(x)
    remainder <- abs(span)
    newper <- period(second = rep(0, length(x)))
    slot(newper, "day") <- remainder%/%(3600 * 24)
    remainder <- remainder%%(3600 * 24)
    slot(newper, "hour") <- remainder%/%(3600)
    remainder <- remainder%%(3600)
    slot(newper, "minute") <- remainder%/%(60)
    slot(newper, ".Data") <- remainder%%(60)
    newper * sign(span)
}


`day<-` <- function (x, value) 
standardGeneric("day<-")


`%m-%` <- function (e1, e2) 
standardGeneric("%m-%")


rollback <- function (dates, roll_to_first = FALSE, preserve_hms = TRUE) 
{
    if (length(dates) == 0) 
        return(dates)
    day(dates) <- 1
    if (!preserve_hms) {
        hour(dates) <- 0
        minute(dates) <- 0
        second(dates) <- 0
    }
    if (roll_to_first) {
        dates
    }
    else {
        dates - days(1)
    }
}


second <- function (x) 
UseMethod("second")


pretty_dates <- function (x, n, ...) 
{
    otz <- Sys.getenv("TZ")
    if (Sys.getenv("TZ") == "") 
        otz <- "unset"
    Sys.setenv(TZ = tz(x[1]))
    on.exit(if (otz == "unset") Sys.unsetenv("TZ") else Sys.setenv(TZ = otz))
    rng <- range(x)
    diff <- difftime(rng[2], rng[1], units = "secs")
    binunits <- pretty_unit(diff/n)
    f <- get(paste("pretty", binunits, sep = "_"), mode = "function")
    binlength <- f(diff, n)
    start <- pretty_point(min(rng), binunits, binlength)
    end <- pretty_point(max(rng), binunits, binlength, start = FALSE)
    breaks <- seq.POSIXt(start, end, paste(binlength, binunits))
    breaks
}


years <- function (x = 1) 
period(year = x)


`.__T__[[:base` <- "<environment>"

ydm_hms <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = c("ydmTz", "ydmT"), quiet = quiet, 
    tz = tz, locale = locale, truncated = truncated)


`wday<-` <- function (x, value) 
{
    if (!is.numeric(value)) {
        value <- pmatch(tolower(value), c("sunday", "monday", 
            "tuesday", "wednesday", "thursday", "friday", "saturday"))
    }
    x <- x + days(value - wday(x))
}


eyears <- function (x = 1) 
{
    .deprecated_fun("dyears", "1.5.0")
    new("Duration", x * 60 * 60 * 24 * 365)
}


`.__T__-:base` <- "<environment>"

`.__T__as.period:lubridate` <- "<environment>"

hm <- function (..., quiet = FALSE, roll = FALSE) 
{
    out <- .parse_hms(..., order = "HM", quiet = quiet)
    if (roll) {
        hms <- .roll_hms(hour = out["H", ], min = out["M", ])
        period(hour = hms$hour, minute = hms$min, second = hms$sec)
    }
    else {
        period(hour = out["H", ], minute = out["M", ])
    }
}


`second<-` <- function (x, value) 
standardGeneric("second<-")


.__C__Duration <- new("classRepresentation"
    , slots = structure(list(.Data = structure("numeric", package = "methods")), .Names = ".Data")
    , contains = structure(list(Timespan = S4_object(), 
    numeric = S4_object(), 
    vector = S4_object()), .Names = c("Timespan", 
"numeric", "vector"))
    , virtual = FALSE
    , prototype = new("numeric"
)
    , validity = function (object) 
{
    if (is.numeric(object@.Data)) 
        TRUE
    else "Duration value is not a number. Should be numeric."
}
    , access = list()
    , className = structure("Duration", package = "lubridate")
    , package = "lubridate"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


new_difftime <- function (...) 
{
    .deprecated_fun("make_difftime", "1.5.0")
    make_difftime(...)
}


setdiff <- function (x, y) 
standardGeneric("setdiff")


time_length <- function (x, unit = "second") 
standardGeneric("time_length")


`.__T__*:base` <- "<environment>"

`.__T__as_datetime:lubridate` <- "<environment>"

hours <- function (x = 1) 
period(hour = x)


`.__T__reclass_timespan:lubridate` <- "<environment>"

ymd <- function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx(..., orders = "ymd", quiet = quiet, tz = tz, locale = locale, 
    truncated = truncated)


dmilliseconds <- function (x = 1) 
new("Duration", x/1000)


ehours <- function (x = 1) 
{
    .deprecated_fun("dhours", "1.5.0")
    new("Duration", x * 3600)
}


ydm_hm <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = "ydmR", quiet = quiet, tz = tz, 
    locale = locale, truncated = truncated)


edays <- function (x = 1) 
{
    .deprecated_fun("ddays", "1.5.0")
    new("Duration", x * 86400)
}


minute <- function (x) 
UseMethod("minute")


stamp_time <- function (x, locale = Sys.getlocale("LC_TIME")) 
stamp(x, orders = c("hms", "hm", "ms", "h", "m", "s"), locale = locale)


Compare <- methods::Compare # re-exported from methods package

int_end <- function (int) 
int@start + int@.Data


ymd_h <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = "ymdr", quiet = quiet, tz = tz, 
    locale = locale, truncated = truncated)


`.__T__%m-%:lubridate` <- "<environment>"

dmy_hms <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = c("dmyTz", "dmyT"), quiet = quiet, 
    tz = tz, locale = locale, truncated = truncated)


`int_end<-` <- function (int, value) 
{
    value <- as.POSIXct(value)
    span <- as.numeric(value - int@start, "secs")
    int <- new("Interval", span, start = int@start, tzone = int@tzone)
}


picoseconds <- function (x = 1) 
seconds(x/1e+12)


`.__T__day<-:lubridate` <- "<environment>"

dmy_hm <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = "dmyR", quiet = quiet, tz = tz, 
    locale = locale, truncated = truncated)


semester <- function (x, with_year = FALSE) 
{
    m <- month(x)
    semesters <- rep(1:2, each = 6)
    s <- semesters[m]
    if (with_year) 
        year(x) + s/10
    else s
}


with_tz <- function (time, tzone = "") 
{
    if (is.data.frame(time)) {
        for (nm in names(time)) {
            if (is.POSIXt(time[[nm]])) {
                time[[nm]] <- with_tz(time[[nm]], tzone = tzone)
            }
        }
        time
    }
    else {
        check_tz(tzone)
        if (is.POSIXlt(time)) 
            new <- as.POSIXct(time)
        else new <- time
        attr(new, "tzone") <- tzone
        reclass_date(new, time)
    }
}


ymd_hms <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
{
    .parse_xxx_hms(..., orders = c("ymdTz", "ymdT"), quiet = quiet, 
        tz = tz, locale = locale, truncated = truncated)
}


ymd_hm <- function (..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx_hms(..., orders = "ymdR", quiet = quiet, tz = tz, 
    locale = locale, truncated = truncated)


as.period <- function (x, unit, ...) 
standardGeneric("as.period")


.__C__Period <- new("classRepresentation"
    , slots = structure(list(.Data = structure("numeric", package = "methods"), 
    year = structure("numeric", package = "methods"), month = structure("numeric", package = "methods"), 
    day = structure("numeric", package = "methods"), hour = structure("numeric", package = "methods"), 
    minute = structure("numeric", package = "methods")), .Names = c(".Data", 
"year", "month", "day", "hour", "minute"))
    , contains = structure(list(Timespan = S4_object(), 
    numeric = S4_object(), 
    vector = S4_object()), .Names = c("Timespan", 
"numeric", "vector"))
    , virtual = FALSE
    , prototype = new("numeric"
)
    , validity = function (object) 
{
    errors <- character()
    if (!is.numeric(object@.Data)) {
        msg <- "seconds (.Data) value must be numeric."
        errors <- c(errors, msg)
    }
    if (!is.numeric(object@year)) {
        msg <- "year value must be numeric."
        errors <- c(errors, msg)
    }
    if (!is.numeric(object@month)) {
        msg <- "year value must be numeric."
        errors <- c(errors, msg)
    }
    if (!is.numeric(object@day)) {
        msg <- "year value must be numeric."
        errors <- c(errors, msg)
    }
    if (!is.numeric(object@hour)) {
        msg <- "year value must be numeric."
        errors <- c(errors, msg)
    }
    if (!is.numeric(object@minute)) {
        msg <- "year value must be numeric."
        errors <- c(errors, msg)
    }
    n <- length(object@.Data)
    lengths <- c(length(object@year), length(object@month), length(object@day), 
        length(object@hour), length(object@minute))
    if (any(lengths != n)) {
        msg <- paste("Inconsistent lengths: year = ", lengths[1], 
            ", month = ", lengths[2], ", day = ", lengths[3], 
            ", hour = ", lengths[4], ", minute = ", lengths[5], 
            ", second = ", n, sep = "")
        errors <- c(errors, msg)
    }
    values <- c(object@year, object@month, object@day, object@hour, 
        object@minute)
    values <- na.omit(values)
    if (sum(values - trunc(values))) {
        msg <- "periods must have integer values"
        errors <- c(errors, msg)
    }
    if (length(errors) == 0) 
        TRUE
    else errors
}
    , access = list()
    , className = structure("Period", package = "lubridate")
    , package = "lubridate"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


is.instant <- function (x) 
inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))


parse_date_time <- function (x, orders, tz = "UTC", truncated = 0, quiet = FALSE, 
    locale = Sys.getlocale("LC_TIME"), select_formats = .select_formats, 
    exact = FALSE) 
{
    orig_locale <- Sys.getlocale("LC_TIME")
    Sys.setlocale("LC_TIME", locale)
    on.exit(Sys.setlocale("LC_TIME", orig_locale))
    x <- as.character(.num_to_date(x))
    if (truncated != 0) 
        orders <- .add_truncated(orders, truncated)
    .local_parse <- function(x, first = FALSE) {
        formats <- if (exact) {
            orders
        }
        else {
            train <- .get_train_set(x)
            .best_formats(train, orders, locale = locale, select_formats)
        }
        if (length(formats) > 0) {
            out <- .parse_date_time(x, formats, tz = tz, quiet = quiet)
            new_na <- is.na(out)
            if (any(new_na)) {
                x <- x[new_na]
                if (length(x) < length(out)) 
                  out[new_na] <- .local_parse(x)
            }
            out
        }
        else {
            if (first && !quiet) {
                warning("All formats failed to parse. No formats found.", 
                  call. = FALSE)
                warned <<- TRUE
            }
            failed <<- length(x)
            NA
        }
    }
    failed <- 0L
    warned <- FALSE
    to_parse <- !is.na(x) & nzchar(x)
    out <- .POSIXct(rep.int(NA, length(x)), tz = tz)
    out[to_parse] <- .local_parse(x[to_parse], TRUE)
    if (failed > 0 && !quiet && !warned) 
        warning(" ", failed, " failed to parse.", call. = FALSE)
    out
}


is.period <- function (x) 
is(x, "Period")


.__C__Timespan <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = new("Interval"
    , .Data = numeric(0)
    , start = S4_object()
    , tzone = character(0)
)
    , validity = NULL
    , access = list()
    , className = structure("Timespan", package = "lubridate")
    , package = "lubridate"
    , subclasses = structure(list(Interval = S4_object(), 
    Duration = S4_object(), 
    Period = S4_object()), .Names = c("Interval", 
"Duration", "Period"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


reclass_timespan <- function (new, orig) 
standardGeneric("reclass_timespan")


dweeks <- function (x = 1) 
new("Duration", x * 604800)


eseconds <- function (x = 1) 
{
    .deprecated_fun("dseconds", "1.5.0")
    new("Duration", x)
}


minutes <- function (x = 1) 
period(minute = x)


mday <- function (x) 
UseMethod("mday")


date <- function (x) 
UseMethod("date")


eminutes <- function (x = 1) 
{
    .deprecated_fun("dminutes", "1.5.0")
    new("Duration", x * 60)
}


reclass_date <- function (new, orig) 
UseMethod("reclass_date", orig)


`.__T__[[<-:base` <- "<environment>"

dnanoseconds <- function (x = 1) 
new("Duration", x/1000/1000/1000)


int_length <- function (int) 
int@.Data


ydm <- function (..., quiet = FALSE, tz = NULL, locale = Sys.getlocale("LC_TIME"), 
    truncated = 0) 
.parse_xxx(..., orders = "ydm", quiet = quiet, tz = tz, locale = locale, 
    truncated = truncated)


`.__T__c:base` <- "<environment>"

tz <- function (x) 
UseMethod("tz")


fast_strptime <- function (x, format, tz = "UTC", lt = TRUE) 
{
    if (length(format) > 1) 
        warning("Multiple formats supplied. Only first format is used.")
    format <- as.character(format[[1]])
    if (lt) {
        .mklt(.Call("parse_dt", x, format, TRUE, TRUE), tz)
    }
    else {
        if (tz == "UTC") {
            .POSIXct(.Call("parse_dt", x, format, TRUE, FALSE), 
                "UTC")
        }
        else {
            as.POSIXct(.mklt(.Call("parse_dt", x, format, TRUE, 
                TRUE), tz))
        }
    }
}


`%--%` <- function (start, end) 
interval(start, end)


dyears <- function (x = 1) 
new("Duration", x * 60 * 60 * 24 * 365)


`.__T__hour<-:lubridate` <- "<environment>"

yday <- function (x) 
UseMethod("yday")


ddays <- function (x = 1) 
new("Duration", x * 86400)


enanoseconds <- function (x = 1) 
{
    .deprecated_fun("dnanoseconds", "1.5.0")
    new("Duration", x/1000/1000/1000)
}


add_with_rollback <- function (e1, e2, roll_to_first = FALSE, preserve_hms = TRUE) 
{
    any_HMS <- any(e2@.Data != 0) || any(e2@minute != 0) || any(e2@hour != 
        0) || any(e2@day != 0)
    any_year <- any(e2@year != 0)
    if (!is.na(any_year) && any_year) {
        e2$month <- 12 * e2@year + e2@month
        e2$year <- 0L
    }
    new <- .quick_month_add(e1, e2@month)
    roll <- day(new) < day(e1)
    roll <- !is.na(roll) & roll
    new[roll] <- rollback(new[roll], roll_to_first = roll_to_first, 
        preserve_hms = preserve_hms)
    if (!is.na(any_HMS) && any_HMS) {
        e2$month <- 0L
        new + e2
    }
    else {
        new
    }
}


is.timepoint <- function (x) 
inherits(x, c("POSIXt", "POSIXct", "POSIXlt", "Date"))


as.duration <- function (x, ...) 
standardGeneric("as.duration")


isoweek <- function (x) 
{
    xday <- make_datetime(year(x), month(x), day(x))
    dn <- 1 + (wday(x) + 5)%%7
    nth <- xday + ddays(4 - dn)
    jan1 <- make_datetime(year(nth), 1, 1)
    1L + as.integer(difftime(nth, jan1, units = "days"))%/%7L
}


`.__T__as.difftime:base` <- "<environment>"

`.__T__Compare:methods` <- "<environment>"

date_decimal <- function (decimal, tz = "UTC") 
{
    Y <- trunc(decimal)
    start <- make_datetime(Y, 1L, 1L, tz = tz)
    end <- make_datetime(Y + 1L, 1L, 1L, tz = tz)
    seconds <- as.numeric(difftime(end, start, units = "secs"))
    frac <- decimal - Y
    end <- start + seconds * frac
    return(end)
}


is.POSIXt <- function (x) 
inherits(x, "POSIXt")


dhours <- function (x = 1) 
new("Duration", x * 3600)




## Package Data

lakers <- lubridate::lakers		## Lakers 2008-2009 basketball data set



## Package Info

.skeleton_package_title = "Make Dealing with Dates a Little Easier"

.skeleton_package_version = "1.6.0"

.skeleton_package_depends = "methods"

.skeleton_package_imports = "stringr"


## Internal

.skeleton_version = 5


## EOF