##
## Exported symobls in package `scales`
##

## Exported package methods

identity_pal <- function () 
{
    function(x) x
}


seq_gradient_pal <- function (low = mnsl("10B 4/6"), high = mnsl("10R 4/6"), space = "Lab") 
{
    gradient_n_pal(c(low, high), space = space)
}


fullseq <- function (range, size, ...) 
UseMethod("fullseq")


date_trans <- function () 
{
    trans_new("date", "from_date", "to_date", breaks = pretty_breaks())
}


reverse_trans <- function () 
{
    trans_new("reverse", function(x) -x, function(x) -x)
}


is.trans <- function (x) 
inherits(x, "trans")


ordinal_format <- function (x) 
{
    function(x) ordinal(x)
}


rescale_mid <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE), mid = 0) 
{
    if (zero_range(from) || zero_range(to)) 
        return(rep(mean(to), length(x)))
    extent <- 2 * max(abs(from - mid))
    (x - mid)/extent * diff(to) + mean(to)
}


math_format <- function (expr = 10^.x, format = force) 
{
    quoted <- substitute(expr)
    subs <- function(x) {
        do.call("substitute", list(quoted, list(.x = as.name(x))))
    }
    function(x) {
        x <- format(x)
        lapply(x, subs)
    }
}


squish_infinite <- function (x, range = c(0, 1)) 
{
    force(range)
    x[x == -Inf] <- range[1]
    x[x == Inf] <- range[2]
    x
}


ContinuousRange <- function (...) 
new("Continuous", ...)


ordinal <- function (x) 
{
    stopifnot(all(x > 0))
    suffixes <- list(st = "(?<!1)1$", nd = "(?<!1)2$", rd = "(?<!1)3$", 
        th = "(?<=1)[123]$", th = "[0456789]$")
    out <- utils::stack(lapply(suffixes, grep, x = x, perl = TRUE))
    paste0(comma(x), out$ind[order(out$values)])
}


gradient_n_pal <- function (colours, values = NULL, space = "Lab") 
{
    if (!identical(space, "Lab")) {
        warning("Non Lab interpolation is deprecated", call. = FALSE)
    }
    ramp <- colour_ramp(colours)
    function(x) {
        if (length(x) == 0) 
            return(character())
        if (!is.null(values)) {
            xs <- seq(0, 1, length.out = length(values))
            f <- stats::approxfun(values, xs)
            x <- f(x)
        }
        ramp(x)
    }
}


grey_pal <- function (start = 0.2, end = 0.8) 
{
    function(n) grDevices::grey.colors(n, start = start, end = end)
}


alpha <- function (colour, alpha = NA) 
{
    col <- grDevices::col2rgb(colour, TRUE)/255
    if (length(colour) != length(alpha)) {
        if (length(colour) > 1 && length(alpha) > 1) {
            stop("Only one of colour and alpha can be vectorised")
        }
        if (length(colour) > 1) {
            alpha <- rep(alpha, length.out = length(colour))
        }
        else if (length(alpha) > 1) {
            col <- col[, rep(1, length(alpha)), drop = FALSE]
        }
    }
    alpha[is.na(alpha)] <- col[4, ][is.na(alpha)]
    new_col <- grDevices::rgb(col[1, ], col[2, ], col[3, ], alpha)
    new_col[is.na(colour)] <- NA
    new_col
}


censor <- function (x, range = c(0, 1), only.finite = TRUE) 
{
    force(range)
    finite <- if (only.finite) 
        is.finite(x)
    else TRUE
    x[finite & x < range[1]] <- NA_real_
    x[finite & x > range[2]] <- NA_real_
    x
}


log_trans <- function (base = exp(1)) 
{
    trans <- function(x) log(x, base)
    inv <- function(x) base^x
    trans_new(paste0("log-", format(base)), trans, inv, log_breaks(base = base), 
        domain = c(1e-100, Inf))
}


log2_trans <- function () 
{
    log_trans(2)
}


hms_trans <- function () 
{
    trans_new("hms", transform = function(x) {
        structure(as.numeric(x), names = names(x))
    }, inverse = hms::as.hms, breaks = time_breaks())
}


col_numeric <- function (palette, domain, na.color = "#808080") 
{
    rng <- NULL
    if (length(domain) > 0) {
        rng <- range(domain, na.rm = TRUE)
        if (!all(is.finite(rng))) {
            stop("Wasn't able to determine range of domain")
        }
    }
    pf <- safePaletteFunc(palette, na.color)
    withColorAttr("numeric", list(na.color = na.color), function(x) {
        if (length(x) == 0 || all(is.na(x))) {
            return(pf(x))
        }
        if (is.null(rng)) 
            rng <- range(x, na.rm = TRUE)
        rescaled <- scales::rescale(x, from = rng)
        if (any(rescaled < 0 | rescaled > 1, na.rm = TRUE)) 
            warning("Some values were outside the color scale and will be treated as NA")
        pf(rescaled)
    })
}


log_breaks <- function (n = 5, base = 10) 
{
    function(x) {
        rng <- log(range(x, na.rm = TRUE), base = base)
        min <- floor(rng[1])
        max <- ceiling(rng[2])
        if (max == min) 
            return(base^min)
        by <- floor((max - min)/n) + 1
        base^seq(min, max, by = by)
    }
}


area_pal <- function (range = c(1, 6)) 
{
    function(x) rescale(sqrt(x), range, c(0, 1))
}


scientific_format <- function (digits = 3, ...) 
{
    function(x) scientific(x, digits, ...)
}


manual_pal <- function (values) 
{
    function(n) {
        n_values <- length(values)
        if (n > n_values) {
            warning("This manual palette can handle a maximum of ", 
                n_values, " values. You have supplied ", n, ".", 
                call. = FALSE)
        }
        values[seq_len(n)]
    }
}


col_bin <- function (palette, domain, bins = 7, pretty = TRUE, na.color = "#808080") 
{
    if (missing(domain) && length(bins) > 1) {
        domain <- NULL
    }
    autobin <- is.null(domain) && length(bins) == 1
    if (!is.null(domain)) 
        bins <- getBins(domain, NULL, bins, pretty)
    numColors <- if (length(bins) == 1) 
        bins
    else length(bins) - 1
    colorFunc <- col_factor(palette, domain = if (!autobin) 
        1:numColors, na.color = na.color)
    pf = safePaletteFunc(palette, na.color)
    withColorAttr("bin", list(bins = bins, na.color = na.color), 
        function(x) {
            if (length(x) == 0 || all(is.na(x))) {
                return(pf(x))
            }
            binsToUse <- getBins(domain, x, bins, pretty)
            ints <- cut(x, binsToUse, labels = FALSE, include.lowest = TRUE, 
                right = FALSE)
            if (any(is.na(x) != is.na(ints))) 
                warning("Some values were outside the color scale and will be treated as NA")
            colorFunc(ints)
        })
}


trans_format <- function (trans, format = scientific_format()) 
{
    if (is.character(trans)) 
        trans <- match.fun(trans)
    function(x) {
        x <- trans(x)
        format(x)
    }
}


zero_range <- function (x, tol = 1000 * .Machine$double.eps) 
{
    if (length(x) == 1) 
        return(TRUE)
    if (length(x) != 2) 
        stop("x must be length 1 or 2")
    if (any(is.na(x))) 
        return(NA)
    if (x[1] == x[2]) 
        return(TRUE)
    if (all(is.infinite(x))) 
        return(FALSE)
    m <- min(abs(x))
    if (m == 0) 
        return(FALSE)
    abs((x[1] - x[2])/m) < tol
}


comma <- function (x, ...) 
{
    format(x, ..., big.mark = ",", scientific = FALSE, trim = TRUE)
}


abs_area <- function (max) 
{
    function(x) rescale(sqrt(abs(x)), c(0, max), c(0, 1))
}


exp_trans <- function (base = exp(1)) 
{
    trans_new(paste0("power-", format(base)), function(x) base^x, 
        function(x) log(x, base = base))
}


time_trans <- function (tz = NULL) 
{
    to_time <- function(x) {
        force(x)
        structure(x, class = c("POSIXt", "POSIXct"), tzone = tz)
    }
    from_time <- function(x) {
        if (!inherits(x, "POSIXct")) {
            stop("Invalid input: time_trans works with objects of class ", 
                "POSIXct only", call. = FALSE)
        }
        if (is.null(tz)) {
            tz <<- attr(as.POSIXlt(x), "tzone")[[1]]
        }
        structure(as.numeric(x), names = names(x))
    }
    trans_new("time", "from_time", "to_time", breaks = pretty_breaks())
}


brewer_pal <- function (type = "seq", palette = 1, direction = 1) 
{
    pal <- pal_name(palette, type)
    function(n) {
        if (n < 3) {
            pal <- suppressWarnings(RColorBrewer::brewer.pal(n, 
                pal))
        }
        else {
            pal <- RColorBrewer::brewer.pal(n, pal)
        }
        pal <- pal[seq_len(n)]
        if (direction == -1) 
            pal <- rev(pal)
        pal
    }
}


trans_range <- function (trans, x) 
{
    trans <- as.trans(trans)
    range(trans$transform(range(squish(x, trans$domain), na.rm = TRUE)))
}


expand_range <- function (range, mul = 0, add = 0, zero_width = 1) 
{
    if (is.null(range)) 
        return()
    if (zero_range(range)) {
        c(range[1] - zero_width/2, range[1] + zero_width/2)
    }
    else {
        range + c(-1, 1) * (diff(range) * mul + add)
    }
}


rescale <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE, finite = TRUE)) 
{
    if (zero_range(from) || zero_range(to)) {
        return(ifelse(is.na(x), NA, mean(to)))
    }
    (x - from[1])/diff(from) * diff(to) + to[1]
}


rescale_none <- function (x, ...) 
{
    x
}


col_factor <- function (palette, domain, levels = NULL, ordered = FALSE, na.color = "#808080") 
{
    if (missing(domain) && !is.null(levels)) {
        domain <- NULL
    }
    if (!is.null(levels) && anyDuplicated(levels)) {
        warning("Duplicate levels detected")
        levels <- unique(levels)
    }
    lvls <- getLevels(domain, NULL, levels, ordered)
    hasFixedLevels <- is.null(lvls)
    pf <- safePaletteFunc(palette, na.color)
    withColorAttr("factor", list(na.color = na.color), function(x) {
        if (length(x) == 0 || all(is.na(x))) {
            return(pf(x))
        }
        lvls <- getLevels(domain, x, lvls, ordered)
        if (!is.factor(x) || hasFixedLevels) {
            origNa <- is.na(x)
            x <- factor(x, lvls)
            if (any(is.na(x) != origNa)) {
                warning("Some values were outside the color scale and will be treated as NA")
            }
        }
        scaled <- scales::rescale(as.integer(x), from = c(1, 
            length(lvls)))
        if (any(scaled < 0 | scaled > 1, na.rm = TRUE)) {
            warning("Some values were outside the color scale and will be treated as NA")
        }
        pf(scaled)
    })
}


train_continuous <- function (new, existing = NULL) 
{
    if (is.null(new)) 
        return(existing)
    if (is.factor(new) || !typeof(new) %in% c("integer", "double")) {
        stop("Discrete value supplied to continuous scale", call. = FALSE)
    }
    suppressWarnings(range(existing, new, na.rm = TRUE, finite = TRUE))
}


percent <- function (x) 
{
    if (length(x) == 0) 
        return(character())
    x <- round_any(x, precision(x)/100)
    paste0(comma(x * 100), "%")
}


trans_new <- function (name, transform, inverse, breaks = extended_breaks(), 
    format = format_format(), domain = c(-Inf, Inf)) 
{
    if (is.character(transform)) 
        transform <- match.fun(transform)
    if (is.character(inverse)) 
        inverse <- match.fun(inverse)
    structure(list(name = name, transform = transform, inverse = inverse, 
        breaks = breaks, format = format, domain = domain), class = "trans")
}


identity_trans <- function () 
{
    trans_new("identity", "force", "force")
}


unit_format <- function (unit = "m", scale = 1, sep = " ", ...) 
{
    function(x) {
        paste(comma(x * scale, ...), unit, sep = sep)
    }
}


colour_ramp <- function (colors, na.color = NA, alpha = FALSE) 
{
    if (length(colors) == 0) {
        stop("Must provide at least one color to create a color ramp")
    }
    colorMatrix <- grDevices::col2rgb(colors, alpha = alpha)
    structure(function(x) {
        doColorRamp(colorMatrix, x, alpha, ifelse(is.na(na.color), 
            "", na.color))
    }, safe_palette_func = TRUE)
}


date_breaks <- function (width = "1 month") 
{
    function(x) fullseq(x, width)
}


extended_breaks <- function (n = 5, ...) 
{
    function(x) {
        x <- x[is.finite(x)]
        if (length(x) == 0) {
            return(numeric())
        }
        rng <- range(x)
        labeling::extended(rng[1], rng[2], n, only.loose = FALSE, 
            ...)
    }
}


date_format <- function (format = "%Y-%m-%d", tz = "UTC") 
{
    function(x) format(x, format, tz = tz)
}


show_col <- function (colours, labels = TRUE, borders = NULL) 
{
    n <- length(colours)
    ncol <- ceiling(sqrt(n))
    nrow <- ceiling(n/ncol)
    colours <- c(colours, rep(NA, nrow * ncol - length(colours)))
    colours <- matrix(colours, ncol = ncol, byrow = TRUE)
    old <- par(pty = "s", mar = c(0, 0, 0, 0))
    on.exit(par(old))
    size <- max(dim(colours))
    plot(c(0, size), c(0, -size), type = "n", xlab = "", ylab = "", 
        axes = FALSE)
    rect(col(colours) - 1, -row(colours) + 1, col(colours), -row(colours), 
        col = colours, border = borders)
    if (labels) {
        text(col(colours) - 0.5, -row(colours) + 0.5, colours)
    }
}


col_quantile <- function (palette, domain, n = 4, probs = seq(0, 1, length.out = n + 
    1), na.color = "#808080") 
{
    if (!is.null(domain)) {
        bins <- stats::quantile(domain, probs, na.rm = TRUE, 
            names = FALSE)
        return(withColorAttr("quantile", list(probs = probs, 
            na.color = na.color), col_bin(palette, domain = NULL, 
            bins = bins, na.color = na.color)))
    }
    colorFunc <- col_factor(palette, domain = 1:(length(probs) - 
        1), na.color = na.color)
    withColorAttr("quantile", list(probs = probs, na.color = na.color), 
        function(x) {
            binsToUse <- stats::quantile(x, probs, na.rm = TRUE, 
                names = FALSE)
            ints <- cut(x, binsToUse, labels = FALSE, include.lowest = TRUE, 
                right = FALSE)
            if (any(is.na(x) != is.na(ints))) 
                warning("Some values were outside the color scale and will be treated as NA")
            colorFunc(ints)
        })
}


train_discrete <- function (new, existing = NULL, drop = FALSE, na.rm = FALSE) 
{
    if (is.null(new)) 
        return(existing)
    if (!is.discrete(new)) {
        stop("Continuous value supplied to discrete scale", call. = FALSE)
    }
    discrete_range(existing, new, drop = drop, na.rm = na.rm)
}


boxcox_trans <- function (p) 
{
    if (abs(p) < 1e-07) 
        return(log_trans())
    trans <- function(x) (x^p - 1)/p * sign(x - 1)
    inv <- function(x) (abs(x) * p + 1 * sign(x))^(1/p)
    trans_new(paste0("pow-", format(p)), trans, inv)
}


dollar_format <- function (prefix = "$", suffix = "", largest_with_cents = 1e+05, 
    ..., big.mark = ",", negative_parens = FALSE) 
{
    function(x) {
        if (length(x) == 0) 
            return(character())
        x <- round_any(x, 0.01)
        if (needs_cents(x, largest_with_cents)) {
            nsmall <- 2L
        }
        else {
            x <- round_any(x, 1)
            nsmall <- 0L
        }
        negative <- !is.na(x) & x < 0
        if (negative_parens) {
            x <- abs(x)
        }
        amount <- format(abs(x), nsmall = nsmall, trim = TRUE, 
            big.mark = big.mark, scientific = FALSE, digits = 1L)
        if (negative_parens) {
            paste0(ifelse(negative, "(", ""), prefix, amount, 
                suffix, ifelse(negative, ")", ""))
        }
        else {
            paste0(prefix, ifelse(negative, "-", ""), amount, 
                suffix)
        }
    }
}


scientific <- function (x, digits = 3, ...) 
{
    x <- signif(x, digits)
    format(x, trim = TRUE, scientific = TRUE, ...)
}


format_format <- function (...) 
{
    function(x) {
        if (!is.null(names(x))) 
            return(names(x))
        format(x, ..., trim = TRUE, justify = "left")
    }
}


comma_format <- function (...) 
{
    function(x) comma(x, ...)
}


col2hcl <- function (colour, h, c, l, alpha = 1) 
{
    rgb <- t(grDevices::col2rgb(colour))/255
    coords <- grDevices::convertColor(rgb, "sRGB", "Luv")
    if (missing(h)) 
        h <- atan2(coords[, "v"], coords[, "u"]) * 180/pi
    if (missing(c)) 
        c <- sqrt(coords[, "u"]^2 + coords[, "v"]^2)
    if (missing(l)) 
        l <- coords[, "L"]
    hcl_colours <- grDevices::hcl(h, c, l, alpha = alpha)
    names(hcl_colours) <- names(colour)
    hcl_colours
}


cscale <- function (x, palette, na.value = NA_real_, trans = identity_trans()) 
{
    stopifnot(is.trans(trans))
    x <- trans$transform(x)
    limits <- train_continuous(x)
    map_continuous(palette, x, limits, na.value)
}


DiscreteRange <- function (...) 
new("DiscreteRange", ...)


trans_breaks <- function (trans, inv, n = 5, ...) 
{
    trans <- match.fun(trans)
    inv <- match.fun(inv)
    function(x) {
        inv(pretty(trans(x), n, ...))
    }
}


reciprocal_trans <- function () 
{
    trans_new("reciprocal", function(x) 1/x, function(x) 1/x)
}


percent_format <- function () 
{
    function(x) {
        if (length(x) == 0) 
            return(character())
        x <- round_any(x, precision(x)/100)
        paste0(comma(x * 100), "%")
    }
}


as.trans <- function (x) 
{
    if (is.trans(x)) 
        return(x)
    f <- paste0(x, "_trans")
    match.fun(f)()
}


pretty_breaks <- function (n = 5, ...) 
{
    function(x) {
        breaks <- pretty(x, n, ...)
        names(breaks) <- attr(breaks, "labels")
        breaks
    }
}


parse_format <- function () 
{
    function(x) {
        lapply(as.character(x), function(x) parse(text = x, srcfile = NULL))
    }
}


wrap_format <- function (width) 
{
    function(x) {
        unlist(lapply(strwrap(x, width = width, simplify = FALSE), 
            paste0, collapse = "\n"))
    }
}


dichromat_pal <- function (name) 
{
    if (!any(name == names(dichromat::colorschemes))) {
        stop("Palette name must be one of ", paste0(names(dichromat::colorschemes), 
            collapse = ", "), call. = FALSE)
    }
    pal <- dichromat::colorschemes[[name]]
    function(n) pal[seq_len(n)]
}


sqrt_trans <- function () 
{
    trans_new("sqrt", "sqrt", function(x) x^2, domain = c(0, 
        Inf))
}


log1p_trans <- function () 
{
    trans_new("log1p", "log1p", "expm1")
}


cbreaks <- function (range, breaks = extended_breaks(), labels = scientific_format()) 
{
    if (zero_range(range)) {
        return(list(breaks = range[1], labels = format(range[1])))
    }
    if (is.function(breaks)) {
        breaks <- breaks(range)
        if (!is.function(labels)) {
            stop("Labels can only be manually specified in conjunction with breaks", 
                call. = FALSE)
        }
    }
    if (is.function(labels)) {
        labels <- labels(breaks)
    }
    else {
        if (length(labels) != length(breaks)) {
            stop("Labels and breaks must be same length")
        }
        if (is.expression(labels)) {
            labels <- as.list(labels)
        }
        else {
            labels <- as.character(labels)
        }
    }
    list(breaks = breaks, labels = labels)
}


muted <- function (colour, l = 30, c = 70) 
col2hcl(colour, l = l, c = c)


div_gradient_pal <- function (low = mnsl("10B 4/6"), mid = mnsl("N 8/0"), high = mnsl("10R 4/6"), 
    space = "Lab") 
{
    gradient_n_pal(c(low, mid, high), space = space)
}


linetype_pal <- function () 
{
    types <- c("solid", "22", "42", "44", "13", "1343", "73", 
        "2262", "12223242", "F282", "F4448444", "224282F2", "F1")
    function(n) {
        types[seq_len(n)]
    }
}


dscale <- function (x, palette, na.value = NA) 
{
    limits <- train_discrete(x)
    map_discrete(palette, x, limits, na.value)
}


discard <- function (x, range = c(0, 1)) 
{
    force(range)
    x[x >= range[1] & x <= range[2]]
}


rescale_pal <- function (range = c(0.1, 1)) 
{
    function(x) {
        rescale(x, range, c(0, 1))
    }
}


squish <- function (x, range = c(0, 1), only.finite = TRUE) 
{
    force(range)
    finite <- if (only.finite) 
        is.finite(x)
    else TRUE
    x[finite & x < range[1]] <- range[1]
    x[finite & x > range[2]] <- range[2]
    x
}


hue_pal <- function (h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, direction = 1) 
{
    function(n) {
        if ((diff(h)%%360) < 1) {
            h[2] <- h[2] - 360/n
        }
        rotate <- function(x) (x + h.start)%%360 * direction
        hues <- rotate(seq(h[1], h[2], length.out = n))
        grDevices::hcl(hues, c, l)
    }
}


dollar <- function (x) 
{
    if (length(x) == 0) 
        return(character())
    x <- round_any(x, 0.01)
    if (needs_cents(x, largest_with_cents)) {
        nsmall <- 2L
    }
    else {
        x <- round_any(x, 1)
        nsmall <- 0L
    }
    negative <- !is.na(x) & x < 0
    if (negative_parens) {
        x <- abs(x)
    }
    amount <- format(abs(x), nsmall = nsmall, trim = TRUE, big.mark = big.mark, 
        scientific = FALSE, digits = 1L)
    if (negative_parens) {
        paste0(ifelse(negative, "(", ""), prefix, amount, suffix, 
            ifelse(negative, ")", ""))
    }
    else {
        paste0(prefix, ifelse(negative, "-", ""), amount, suffix)
    }
}


logit_trans <- function () 
probability_trans("logis")


shape_pal <- function (solid = TRUE) 
{
    function(n) {
        if (n > 6) {
            msg <- paste("The shape palette can deal with a maximum of 6 discrete ", 
                "values because more than 6 becomes difficult to discriminate; ", 
                "you have ", n, ". Consider specifying shapes manually if you ", 
                "must have them.", sep = "")
            warning(paste(strwrap(msg), collapse = "\n"), call. = FALSE)
        }
        if (solid) {
            c(16, 17, 15, 3, 7, 8)[seq_len(n)]
        }
        else {
            c(1, 2, 0, 3, 7, 8)[seq_len(n)]
        }
    }
}


rescale_max <- function (x, to = c(0, 1), from = range(x, na.rm = TRUE)) 
{
    x/from[2] * to[2]
}


atanh_trans <- function () 
{
    trans_new("atanh", "atanh", "tanh")
}


probit_trans <- function () 
probability_trans("norm")


asn_trans <- function () 
{
    trans_new("asn", function(x) 2 * asin(sqrt(x)), function(x) sin(x/2)^2)
}


log10_trans <- function () 
{
    log_trans(10)
}


probability_trans <- function (distribution, ...) 
{
    qfun <- match.fun(paste0("q", distribution))
    pfun <- match.fun(paste0("p", distribution))
    trans_new(paste0("prob-", distribution), function(x) qfun(x, 
        ...), function(x) pfun(x, ...))
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Scale Functions for Visualization"

.skeleton_package_version = "0.4.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "RColorBrewer,dichromat,plyr,munsell,labeling,methods,Rcpp"


## Internal

.skeleton_version = 5


## EOF