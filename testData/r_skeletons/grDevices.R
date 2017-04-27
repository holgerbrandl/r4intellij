##
## Exported symobls in package `grDevices`
##

## Exported package methods

dev.size <- function (units = c("in", "cm", "px")) 
{
    units <- match.arg(units)
    size <- .External(C_devsize)
    if (units == "px") 
        size
    else size * graphics::par("cin")/graphics::par("cra") * if (units == 
        "cm") 
        2.54
    else 1
}


svg <- function (filename = if (onefile) "Rplots.svg" else "Rplot%03d.svg", 
    width = 7, height = 7, pointsize = 12, onefile = FALSE, family = "sans", 
    bg = "white", antialias = c("default", "none", "gray", "subpixel")) 
{
    if (!checkIntFormat(filename)) 
        stop("invalid 'filename'")
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(C_devCairo, filename, 4L, 72 * width, 
        72 * height, pointsize, bg, NA_integer_, antialias, onefile, 
        family, 300))
}


recordPlot <- function (load = NULL, attach = NULL) 
{
    if (dev.cur() == 1L) 
        stop("no current device to record from")
    res <- .External2(C_getSnapshot)
    attr(res, "pid") <- Sys.getpid()
    attr(res, "Rversion") <- getRversion()
    attr(res, "load") <- as.character(load)
    attr(res, "attach") <- as.character(attach)
    class(res) <- "recordedplot"
    res
}


cairo_ps <- function (filename = if (onefile) "Rplots.ps" else "Rplot%03d.ps", 
    width = 7, height = 7, pointsize = 12, onefile = FALSE, family = "sans", 
    bg = "white", antialias = c("default", "none", "gray", "subpixel"), 
    fallback_resolution = 300) 
{
    if (!checkIntFormat(filename)) 
        stop("invalid 'filename'")
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(C_devCairo, filename, 7L, 72 * width, 
        72 * height, pointsize, bg, NA_integer_, antialias, onefile, 
        family, fallback_resolution))
}


heat.colors <- function (n, alpha = 1) 
{
    if ((n <- as.integer(n[1L])) > 0) {
        j <- n%/%4
        i <- n - j
        c(rainbow(i, start = 0, end = 1/6, alpha = alpha), if (j > 
            0) hsv(h = 1/6, s = seq.int(from = 1 - 1/(2 * j), 
            to = 1/(2 * j), length.out = j), v = 1, alpha = alpha))
    }
    else character()
}


make.rgb <- function (red, green, blue, name = NULL, white = "D65", gamma = 2.2) 
{
    whitexyz <- c2to3(white.points[, white])
    rgb <- rbind(c2to3(red), c2to3(green), c2to3(blue))
    S <- drop(whitexyz %*% solve(rgb))
    M <- S * rgb
    if (is.numeric(gamma) && length(gamma) == 1) {
        dogamma <- function(x) x %^% gamma
        ungamma <- function(x) x %^% (1/gamma)
    }
    else if (gamma == "sRGB") {
        dogamma <- function(x) ifelse(x < 0.04045, x/12.92, ((x + 
            0.055)/1.055)^2.4)
        ungamma <- function(x) ifelse(x <= 0.0031308, 12.92 * 
            x, 1.055 * x %^% (1/2.4) - 0.055)
    }
    else stop("'gamma' must be a scalar or 'sRGB'")
    toXYZ <- function(rgb, ...) {
        dogamma(rgb) %*% M
    }
    toRGB <- function(xyz, ...) {
        ungamma(xyz %*% solve(M))
    }
    if (is.null(name)) 
        name <- deparse(sys.call())[1L]
    rval <- list(toXYZ = toXYZ, fromXYZ = toRGB, gamma = gamma, 
        reference.white = white, name = name)
    class(rval) <- c("RGBcolorConverter", "colorConverter")
    rval
}


terrain.colors <- function (n, alpha = 1) 
{
    if ((n <- as.integer(n[1L])) > 0) {
        k <- n%/%2
        h <- c(4/12, 2/12, 0/12)
        s <- c(1, 1, 0)
        v <- c(0.65, 0.9, 0.95)
        c(hsv(h = seq.int(h[1L], h[2L], length.out = k), s = seq.int(s[1L], 
            s[2L], length.out = k), v = seq.int(v[1L], v[2L], 
            length.out = k), alpha = alpha), hsv(h = seq.int(h[2L], 
            h[3L], length.out = n - k + 1)[-1L], s = seq.int(s[2L], 
            s[3L], length.out = n - k + 1)[-1L], v = seq.int(v[2L], 
            v[3L], length.out = n - k + 1)[-1L], alpha = alpha))
    }
    else character()
}


setEPS <- function (...) 
{
    dots <- list(...)
    args <- list(width = 7, height = 7)
    args[names(dots)] <- dots
    force <- list(onefile = FALSE, horizontal = FALSE, paper = "special")
    args[names(force)] <- force
    do.call("ps.options", args)
}


X11Font <- function (font) 
checkX11Font(font)


Type1Font <- function (family, metrics, encoding = "default") 
{
    font <- list(family = family, metrics = metrics, encoding = encoding)
    class(font) <- "Type1Font"
    checkFont(font)
}


quartz.save <- function (file, type = "png", device = dev.cur(), dpi = 100, 
    ...) 
{
    dev.set(device)
    current.device <- dev.cur()
    nm <- names(current.device)[1L]
    if (nm == "null device") 
        stop("no device to print from")
    if (!dev.displaylist()) 
        stop("can only print from a screen device")
    oc <- match.call()
    oc[[1L]] <- as.name("dev.copy")
    oc$file <- NULL
    oc$device <- quartz
    oc$type <- type
    if (missing(file)) 
        file <- paste("Rplot", type, sep = ".")
    oc$file <- file
    oc$dpi <- dpi
    din <- dev.size("in")
    w <- din[1L]
    h <- din[2L]
    if (is.null(oc$width)) 
        oc$width <- if (!is.null(oc$height)) 
            w/h * eval.parent(oc$height)
        else w
    if (is.null(oc$height)) 
        oc$height <- if (!is.null(oc$width)) 
            h/w * eval.parent(oc$width)
        else h
    on.exit(dev.set(current.device))
    dev.off(eval.parent(oc))
}


pdf <- function (file = if (onefile) "Rplots.pdf" else "Rplot%03d.pdf", 
    width, height, onefile, family, title, fonts, version, paper, 
    encoding, bg, fg, pointsize, pagecentre, colormodel, useDingbats, 
    useKerning, fillOddEven, compress) 
{
    initPSandPDFfonts()
    new <- list()
    if (!missing(width)) 
        new$width <- width
    if (!missing(height)) 
        new$height <- height
    if (!missing(onefile)) 
        new$onefile <- onefile
    if (!missing(title)) 
        new$title <- title
    if (!missing(fonts)) 
        new$fonts <- fonts
    if (!missing(version)) 
        new$version <- version
    if (!missing(paper)) 
        new$paper <- paper
    if (!missing(encoding)) 
        new$encoding <- encoding
    if (!missing(bg)) 
        new$bg <- bg
    if (!missing(fg)) 
        new$fg <- fg
    if (!missing(pointsize)) 
        new$pointsize <- pointsize
    if (!missing(pagecentre)) 
        new$pagecentre <- pagecentre
    if (!missing(colormodel)) 
        new$colormodel <- colormodel
    if (!missing(useDingbats)) 
        new$useDingbats <- useDingbats
    if (!missing(useKerning)) 
        new$useKerning <- useKerning
    if (!missing(fillOddEven)) 
        new$fillOddEven <- fillOddEven
    if (!missing(compress)) 
        new$compress <- compress
    old <- check.options(new, name.opt = ".PDF.Options", envir = .PSenv)
    if (!missing(family) && (inherits(family, "Type1Font") || 
        inherits(family, "CIDFont"))) {
        enc <- family$encoding
        if (inherits(family, "Type1Font") && !is.null(enc) && 
            enc != "default" && (is.null(old$encoding) || old$encoding == 
            "default")) 
            old$encoding <- enc
        family <- family$metrics
    }
    if (is.null(old$encoding) || old$encoding == "default") 
        old$encoding <- guessEncoding()
    if (!missing(family)) {
        if (length(family) == 4L) {
            family <- c(family, "Symbol.afm")
        }
        else if (length(family) == 5L) {
        }
        else if (length(family) == 1L) {
            pf <- pdfFonts(family)[[1L]]
            if (is.null(pf)) 
                stop(gettextf("unknown family '%s'", family), 
                  domain = NA)
            matchFont(pf, old$encoding)
        }
        else stop("invalid 'family' argument")
        old$family <- family
    }
    version <- old$version
    versions <- c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", 
        "2.0")
    if (version %in% versions) 
        version <- as.integer(strsplit(version, "[.]")[[1L]])
    else stop("invalid PDF version")
    onefile <- old$onefile
    if (!checkIntFormat(file)) 
        stop(gettextf("invalid 'file' argument '%s'", file), 
            domain = NA)
    .External(C_PDF, file, old$paper, old$family, old$encoding, 
        old$bg, old$fg, old$width, old$height, old$pointsize, 
        onefile, old$pagecentre, old$title, old$fonts, version[1L], 
        version[2L], old$colormodel, old$useDingbats, old$useKerning, 
        old$fillOddEven, old$compress)
    invisible()
}


grey <- function (level, alpha = NULL) 
.Call(C_gray, level, alpha)


dev.list <- function () 
{
    n <- if (exists(".Devices")) 
        get(".Devices")
    else list("null device")
    n <- unlist(n)
    i <- seq_along(n)[n != ""]
    names(i) <- n[i]
    i <- i[-1L]
    if (length(i) == 0L) 
        NULL
    else i
}


as.raster <- function (x, ...) 
UseMethod("as.raster")


dev.copy2pdf <- function (..., out.type = "pdf") 
{
    out.type <- match.arg(out.type, c("pdf", "quartz", "cairo"))
    current.device <- dev.cur()
    nm <- names(current.device)[1L]
    if (nm == "null device") 
        stop("no device to print from")
    if (!dev.displaylist()) 
        stop("can only print from a screen device")
    oc <- match.call()
    oc[[1L]] <- quote(grDevices::dev.copy)
    if (out.type == "quartz" && capabilities("aqua")) {
        oc$device <- quartz
        oc$type <- "pdf"
    }
    else if (out.type == "cairo" && capabilities("cairo")) {
        oc$device <- cairo_pdf
        oc$onefile <- FALSE
    }
    else {
        oc$device <- pdf
        oc$onefile <- FALSE
        if (is.null(oc$paper)) 
            oc$paper <- "special"
    }
    oc$out.type <- NULL
    din <- dev.size("in")
    w <- din[1L]
    h <- din[2L]
    if (is.null(oc$width)) 
        oc$width <- if (!is.null(oc$height)) 
            w/h * eval.parent(oc$height)
        else w
    if (is.null(oc$height)) 
        oc$height <- if (!is.null(oc$width)) 
            h/w * eval.parent(oc$width)
        else h
    if (is.null(oc$file)) 
        oc$file <- "Rplot.pdf"
    on.exit(dev.set(current.device))
    dev.off(eval.parent(oc))
}


quartzFont <- function (family) 
{
    checkQuartzFont(family)
}


dev.print <- function (device = postscript, ...) 
{
    current.device <- dev.cur()
    nm <- names(current.device)[1L]
    if (nm == "null device") 
        stop("no device to print from")
    if (!dev.displaylist()) 
        stop("can only print from a screen device")
    oc <- match.call()
    oc[[1L]] <- quote(grDevices::dev.copy)
    oc$device <- device
    din <- graphics::par("din")
    w <- din[1L]
    h <- din[2L]
    if (missing(device)) {
        if (is.null(oc$file)) 
            oc$file <- ""
        hz0 <- oc$horizontal
        hz <- if (is.null(hz0)) 
            ps.options()$horizontal
        else eval.parent(hz0)
        paper <- oc$paper
        if (is.null(paper)) 
            paper <- ps.options()$paper
        if (paper == "default") 
            paper <- getOption("papersize")
        paper <- tolower(paper)
        switch(paper, a4 = {
            wp <- 8.27
            hp <- 11.69
        }, legal = {
            wp <- 8.5
            hp <- 14
        }, executive = {
            wp <- 7.25
            hp <- 10.5
        }, {
            wp <- 8.5
            hp <- 11
        })
        wp <- wp - 0.5
        hp <- hp - 0.5
        if (!hz && is.null(hz0) && h < wp && wp < w && w < hp) {
            hz <- TRUE
        }
        else if (hz && is.null(hz0) && w < wp && wp < h && h < 
            hp) {
            hz <- FALSE
        }
        else {
            h0 <- if (hz) 
                wp
            else hp
            if (h > h0) {
                w <- w * h0/h
                h <- h0
            }
            w0 <- if (hz) 
                hp
            else wp
            if (w > w0) {
                h <- h * w0/w
                w <- w0
            }
        }
        if (is.null(oc$pointsize)) {
            pt <- ps.options()$pointsize
            oc$pointsize <- pt * w/din[1L]
        }
        if (is.null(hz0)) 
            oc$horizontal <- hz
        if (is.null(oc$width)) 
            oc$width <- w
        if (is.null(oc$height)) 
            oc$height <- h
    }
    else {
        devname <- deparse(substitute(device))
        if (devname %in% c("png", "jpeg", "bmp") && is.null(oc$width) && 
            is.null(oc$height)) 
            warning("need to specify one of 'width' and 'height'")
        if (is.null(oc$width)) 
            oc$width <- if (!is.null(oc$height)) 
                w/h * eval.parent(oc$height)
            else w
        if (is.null(oc$height)) 
            oc$height <- if (!is.null(oc$width)) 
                h/w * eval.parent(oc$width)
            else h
    }
    on.exit(dev.set(current.device))
    dev.off(eval.parent(oc))
}


dev.prev <- function (which = dev.cur()) 
{
    if (!exists(".Devices")) 
        .Devices <- list("null device")
    num.device <- .External(C_devprev, as.integer(which))
    names(num.device) <- .Devices[[num.device]]
    num.device
}


as.graphicsAnnot <- function (x) 
if (is.language(x) || !is.object(x)) x else as.character(x)


ps.options <- function (..., reset = FALSE, override.check = FALSE) 
{
    initPSandPDFfonts()
    old <- get(".PostScript.Options", envir = .PSenv)
    if (reset) {
        assign(".PostScript.Options", get(".PostScript.Options.default", 
            envir = .PSenv), envir = .PSenv)
    }
    l... <- length(new <- list(...))
    if (m <- match("append", names(new), 0L)) {
        warning("argument 'append' is for back-compatibility and will be ignored", 
            immediate. = TRUE)
        new <- new[-m]
    }
    check.options(new, name.opt = ".PostScript.Options", envir = .PSenv, 
        assign.opt = l... > 0, override.check = override.check)
    if (reset || l... > 0) 
        invisible(old)
    else old
}


pdf.options <- function (..., reset = FALSE) 
{
    initPSandPDFfonts()
    old <- get(".PDF.Options", envir = .PSenv)
    if (reset) {
        assign(".PDF.Options", get(".PDF.Options.default", envir = .PSenv), 
            envir = .PSenv)
    }
    l... <- length(new <- list(...))
    check.options(new, name.opt = ".PDF.Options", envir = .PSenv, 
        assign.opt = l... > 0)
    if (reset || l... > 0) 
        invisible(old)
    else old
}


colorRamp <- function (colors, bias = 1, space = c("rgb", "Lab"), interpolate = c("linear", 
    "spline"), alpha = FALSE) 
{
    if (bias <= 0) 
        stop("'bias' must be positive")
    if (!missing(space) && alpha) 
        stop("'alpha' must be false if 'space' is specified")
    colors <- t(col2rgb(colors, alpha = alpha)/255)
    space <- match.arg(space)
    interpolate <- match.arg(interpolate)
    if (space == "Lab") 
        colors <- convertColor(colors, from = "sRGB", to = "Lab")
    interpolate <- switch(interpolate, linear = stats::approxfun, 
        spline = stats::splinefun)
    if ((nc <- nrow(colors)) == 1L) {
        colors <- colors[c(1L, 1L), ]
        nc <- 2L
    }
    x <- seq.int(0, 1, length.out = nc)^bias
    palette <- c(interpolate(x, colors[, 1L]), interpolate(x, 
        colors[, 2L]), interpolate(x, colors[, 3L]), if (alpha) interpolate(x, 
        colors[, 4L]))
    roundcolor <- function(rgb) pmax(pmin(rgb, 1), 0)
    if (space == "Lab") 
        function(x) roundcolor(convertColor(cbind(palette[[1L]](x), 
            palette[[2L]](x), palette[[3L]](x), if (alpha) 
                palette[[4L]](x)), from = "Lab", to = "sRGB")) * 
            255
    else function(x) roundcolor(cbind(palette[[1L]](x), palette[[2L]](x), 
        palette[[3L]](x), if (alpha) 
            palette[[4L]](x))) * 255
}


dev.flush <- function (level = 1L) 
.External(C_devholdflush, -max(0L, level))


xyTable <- function (x, y = NULL, digits) 
{
    x <- xy.coords(x, y)
    y <- signif(x$y, digits = digits)
    x <- signif(x$x, digits = digits)
    n <- length(x)
    number <- if (n > 0) {
        orderxy <- order(x, y)
        x <- x[orderxy]
        y <- y[orderxy]
        first <- c(TRUE, (x[-1L] != x[-n]) | (y[-1L] != y[-n]))
        x <- x[first]
        y <- y[first]
        diff(c((1L:n)[first], n + 1L))
    }
    else integer()
    list(x = x, y = y, number = number)
}


graphics.off <- function () 
{
    while ((which <- dev.cur()) != 1) dev.off(which)
    invisible()
}


bitmap <- function (file, type = "png16m", height = 7, width = 7, res = 72, 
    units = "in", pointsize, taa = NA, gaa = NA, ...) 
{
    if (missing(file)) 
        stop("'file' is missing with no default")
    if (!is.character(file) || length(file) != 1L || !nzchar(file)) 
        stop("'file' must be a non-empty character string")
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    height <- switch(units, `in` = 1, cm = 1/2.54, mm = 1/25.4, 
        px = 1/res) * height
    width <- switch(units, `in` = 1, cm = 1/2.54, mm = 1/25.4, 
        px = 1/res) * width
    gsexe <- tools::find_gs_cmd()
    if (!nzchar(gsexe)) 
        stop("GhostScript was not found")
    check_gs_type(gsexe, type)
    if (missing(pointsize)) 
        pointsize <- 1.5 * min(width, height)
    extra <- ""
    if (!is.na(taa)) 
        extra <- paste0(" -dTextAlphaBits=", taa)
    if (!is.na(gaa)) 
        extra <- paste0(extra, " -dGraphicsAlphaBits=", gaa)
    cmd <- paste0("|", shQuote(gsexe), " -dNOPAUSE -dBATCH -q -sDEVICE=", 
        type, " -r", res, " -dAutoRotatePages=/None", " -g", 
        ceiling(res * width), "x", ceiling(res * height), extra, 
        " -sOutputFile=", shQuote(file), " -")
    postscript(file = cmd, width = width, height = height, pointsize = pointsize, 
        paper = "special", horizontal = FALSE, ...)
    invisible()
}


dev2bitmap <- function (file, type = "png16m", height = 7, width = 7, res = 72, 
    units = "in", pointsize, ..., method = c("postscript", "pdf"), 
    taa = NA, gaa = NA) 
{
    if (missing(file)) 
        stop("'file' is missing with no default")
    if (!is.character(file) || length(file) != 1L || !nzchar(file)) 
        stop("'file' must be a non-empty character string")
    method <- match.arg(method)
    units <- match.arg(units, c("in", "px", "cm", "mm"))
    height <- switch(units, `in` = 1, cm = 1/2.54, mm = 1/25.4, 
        px = 1/res) * height
    width <- switch(units, `in` = 1, cm = 1/2.54, mm = 1/25.4, 
        px = 1/res) * width
    gsexe <- tools::find_gs_cmd()
    if (!nzchar(gsexe)) 
        stop("GhostScript was not found")
    check_gs_type(gsexe, type)
    if (missing(pointsize)) 
        pointsize <- 1.5 * min(width, height)
    tmp <- tempfile("Rbit")
    on.exit(unlink(tmp))
    din <- graphics::par("din")
    w <- din[1L]
    h <- din[2L]
    if (missing(width) && !missing(height)) 
        width <- w/h * height
    if (missing(height) && !missing(width)) 
        height <- h/w * width
    current.device <- dev.cur()
    if (method == "pdf") 
        dev.off(dev.copy(device = pdf, file = tmp, width = width, 
            height = height, pointsize = pointsize, paper = "special", 
            ...))
    else dev.off(dev.copy(device = postscript, file = tmp, width = width, 
        height = height, pointsize = pointsize, paper = "special", 
        horizontal = FALSE, ...))
    dev.set(current.device)
    extra <- ""
    if (!is.na(taa)) 
        extra <- paste0(" -dTextAlphaBits=", taa)
    if (!is.na(gaa)) 
        extra <- paste0(extra, " -dGraphicsAlphaBits=", gaa)
    cmd <- paste0(shQuote(gsexe), " -dNOPAUSE -dBATCH -q -sDEVICE=", 
        type, " -r", res, " -dAutoRotatePages=/None", " -g", 
        ceiling(res * width), "x", ceiling(res * height), extra, 
        " -sOutputFile=", shQuote(file), " ", tmp)
    system(cmd)
    invisible()
}


xy.coords <- function (x, y = NULL, xlab = NULL, ylab = NULL, log = NULL, 
    recycle = FALSE) 
{
    if (is.null(y)) {
        ylab <- xlab
        if (is.language(x)) {
            if (inherits(x, "formula") && length(x) == 3) {
                ylab <- deparse(x[[2L]])
                xlab <- deparse(x[[3L]])
                y <- eval(x[[2L]], environment(x))
                x <- eval(x[[3L]], environment(x))
            }
            else stop("invalid first argument")
        }
        else if (inherits(x, "ts")) {
            y <- if (is.matrix(x)) 
                x[, 1]
            else x
            x <- stats::time(x)
            xlab <- "Time"
        }
        else if (is.complex(x)) {
            y <- Im(x)
            x <- Re(x)
            xlab <- paste0("Re(", ylab, ")")
            ylab <- paste0("Im(", ylab, ")")
        }
        else if (is.matrix(x) || is.data.frame(x)) {
            x <- data.matrix(x)
            if (ncol(x) == 1) {
                xlab <- "Index"
                y <- x[, 1]
                x <- seq_along(y)
            }
            else {
                colnames <- dimnames(x)[[2L]]
                if (is.null(colnames)) {
                  xlab <- paste0(ylab, "[,1]")
                  ylab <- paste0(ylab, "[,2]")
                }
                else {
                  xlab <- colnames[1L]
                  ylab <- colnames[2L]
                }
                y <- x[, 2]
                x <- x[, 1]
            }
        }
        else if (is.list(x)) {
            if (all(c("x", "y") %in% names(x))) {
                xlab <- paste0(ylab, "$x")
                ylab <- paste0(ylab, "$y")
                y <- x[["y"]]
                x <- x[["x"]]
            }
            else stop("'x' is a list, but does not have components 'x' and 'y'")
        }
        else {
            if (is.factor(x)) 
                x <- as.numeric(x)
            xlab <- "Index"
            y <- x
            x <- seq_along(x)
        }
    }
    if (inherits(x, "POSIXt")) 
        x <- as.POSIXct(x)
    if (length(x) != length(y)) {
        if (recycle) {
            if ((nx <- length(x)) < (ny <- length(y))) 
                x <- rep_len(x, ny)
            else y <- rep_len(y, nx)
        }
        else stop("'x' and 'y' lengths differ")
    }
    if (length(log) && log != "") {
        log <- strsplit(log, NULL)[[1L]]
        if ("x" %in% log && any(ii <- x <= 0 & !is.na(x))) {
            n <- as.integer(sum(ii))
            warning(sprintf(ngettext(n, "%d x value <= 0 omitted from logarithmic plot", 
                "%d x values <= 0 omitted from logarithmic plot"), 
                n), domain = NA)
            x[ii] <- NA
        }
        if ("y" %in% log && any(ii <- y <= 0 & !is.na(y))) {
            n <- as.integer(sum(ii))
            warning(sprintf(ngettext(n, "%d y value <= 0 omitted from logarithmic plot", 
                "%d y values <= 0 omitted from logarithmic plot"), 
                n), domain = NA)
            y[ii] <- NA
        }
    }
    return(list(x = as.double(x), y = as.double(y), xlab = xlab, 
        ylab = ylab))
}


grey.colors <- function (n, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL) 
gray(seq.int(from = start^gamma, to = end^gamma, length.out = n)^(1/gamma), 
    alpha)


postscript <- function (file = if (onefile) "Rplots.ps" else "Rplot%03d.ps", 
    onefile, family, title, fonts, encoding, bg, fg, width, height, 
    horizontal, pointsize, paper, pagecentre, print.it, command, 
    colormodel, useKerning, fillOddEven) 
{
    initPSandPDFfonts()
    new <- list()
    if (!missing(onefile)) 
        new$onefile <- onefile
    if (!missing(title)) 
        new$title <- title
    if (!missing(fonts)) 
        new$fonts <- fonts
    if (!missing(encoding)) 
        new$encoding <- encoding
    if (!missing(bg)) 
        new$bg <- bg
    if (!missing(fg)) 
        new$fg <- fg
    if (!missing(width)) 
        new$width <- width
    if (!missing(height)) 
        new$height <- height
    if (!missing(horizontal)) 
        new$horizontal <- horizontal
    if (!missing(pointsize)) 
        new$pointsize <- pointsize
    if (!missing(paper)) 
        new$paper <- paper
    if (!missing(pagecentre)) 
        new$pagecentre <- pagecentre
    if (!missing(print.it)) 
        new$print.it <- print.it
    if (!missing(command)) 
        new$command <- command
    if (!missing(colormodel)) 
        new$colormodel <- colormodel
    if (!missing(useKerning)) 
        new$useKerning <- useKerning
    if (!missing(fillOddEven)) 
        new$fillOddEven <- fillOddEven
    old <- check.options(new, name.opt = ".PostScript.Options", 
        envir = .PSenv)
    if (is.null(old$command) || old$command == "default") 
        old$command <- if (!is.null(cmd <- getOption("printcmd"))) 
            cmd
        else ""
    if (!missing(family) && (inherits(family, "Type1Font") || 
        inherits(family, "CIDFont"))) {
        enc <- family$encoding
        if (inherits(family, "Type1Font") && !is.null(enc) && 
            enc != "default" && (is.null(old$encoding) || old$encoding == 
            "default")) 
            old$encoding <- enc
        family <- family$metrics
    }
    if (is.null(old$encoding) || old$encoding == "default") 
        old$encoding <- guessEncoding(family)
    if (!missing(family)) {
        if (length(family) == 4L) {
            family <- c(family, "Symbol.afm")
        }
        else if (length(family) == 5L) {
        }
        else if (length(family) == 1L) {
            pf <- postscriptFonts(family)[[1L]]
            if (is.null(pf)) 
                stop(gettextf("unknown family '%s'", family), 
                  domain = NA)
            matchFont(pf, old$encoding)
        }
        else stop("invalid 'family' argument")
        old$family <- family
    }
    onefile <- old$onefile
    if (!checkIntFormat(file)) 
        stop(gettextf("invalid 'file' argument '%s'", file), 
            domain = NA)
    .External(C_PostScript, file, old$paper, old$family, old$encoding, 
        old$bg, old$fg, old$width, old$height, old$horizontal, 
        old$pointsize, onefile, old$pagecentre, old$print.it, 
        old$command, old$title, old$fonts, old$colormodel, old$useKerning, 
        old$fillOddEven)
    invisible()
}


dev.cur <- function () 
{
    if (!exists(".Devices")) 
        .Devices <- list("null device")
    num.device <- .External(C_devcur)
    names(num.device) <- .Devices[[num.device]]
    num.device
}


dev.hold <- function (level = 1L) 
.External(C_devholdflush, max(0L, level))


axisTicks <- function (usr, log, axp = NULL, nint = 5) 
{
    if (is.null(axp)) 
        axp <- unlist(.axisPars(usr, log = log, nintLog = nint), 
            use.names = FALSE)
    .Call(C_R_CreateAtVector, axp, if (log) 10^usr else usr, 
        nint, log)
}


x11 <- function (display = "", width, height, pointsize, gamma, bg, 
    canvas, fonts, family, xpos, ypos, title, type, antialias) 
{
    if (display != "XImage") {
        check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
        msg <- "screen devices should not be used in examples etc"
        if (identical(check, "stop")) 
            stop(msg, domain = NA)
        else if (identical(check, "warn")) 
            warning(msg, immediate. = TRUE, noBreaks. = TRUE, 
                domain = NA)
    }
    if (display == "" && .Platform$GUI == "AQUA" && is.na(Sys.getenv("DISPLAY", 
        NA))) 
        Sys.setenv(DISPLAY = ":0")
    new <- list()
    if (!missing(display)) 
        new$display <- display
    if (!missing(width)) 
        new$width <- width
    if (!missing(height)) 
        new$height <- height
    if (!missing(gamma)) 
        new$gamma <- gamma
    if (!missing(pointsize)) 
        new$pointsize <- pointsize
    if (!missing(bg)) 
        new$bg <- bg
    if (!missing(canvas)) 
        new$canvas <- canvas
    if (!missing(xpos)) 
        new$xpos <- xpos
    if (!missing(ypos)) 
        new$ypos <- ypos
    if (!missing(title)) 
        new$title <- title
    if (!checkIntFormat(new$title)) 
        stop("invalid 'title'")
    if (!missing(type)) {
        new$type <- match.arg(type, c("Xlib", "cairo", "nbcairo", 
            "dbcairo"))
        if (!capabilities("cairo") && type != "Xlib") 
            warning("cairo-based types are not supported on this build - using \"Xlib\"")
    }
    if (!missing(family)) 
        new$family <- family
    if (!missing(fonts)) 
        new$fonts <- fonts
    if (!missing(antialias) && type != "Xlib") 
        new$antialias <- match.arg(antialias, aa.cairo)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    if (d$type == "Xlib" && !missing(family)) {
        fns <- X11Fonts()
        if (!family %in% names(fns)) 
            stop("unknown family for X11(type = \"XLib\")")
        d$fonts[1] <- fns[[family]]
    }
    type <- if (capabilities("cairo")) 
        switch(d$type, cairo = 1L, nbcairo = 2L, dbcairo = 3L, 
            0L)
    else 0L
    if (display == "XImage") 
        type <- 0L
    antialias <- match(d$antialias, aa.cairo)
    if (grepl("darwin", R.version$os)) 
        check_for_XQuartz()
    .External2(C_X11, d$display, d$width, d$height, d$pointsize, 
        d$gamma, d$colortype, d$maxcubesize, d$bg, d$canvas, 
        d$fonts, NA_integer_, d$xpos, d$ypos, d$title, type, 
        antialias, d$family)
    invisible()
}


tiff <- function (filename = "Rplot%03d.tiff", width = 480, height = 480, 
    units = "px", pointsize = 12, compression = c("none", "rle", 
        "lzw", "jpeg", "zip", "lzw+p", "zip+p"), bg = "white", 
    res = NA, ..., type = c("cairo", "Xlib", "quartz"), antialias) 
{
    if (!checkIntFormat(filename)) 
        stop("invalid 'filename'")
    g <- .geometry(width, height, units, res)
    new <- list(...)
    type <- if (!missing(type)) 
        match.arg(type)
    else getOption("bitmapType")
    if (!missing(antialias)) 
        new$antialias <- match.arg(antialias, aa.cairo)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    antialias <- match(d$antialias, aa.cairo)
    comp <- switch(match.arg(compression), none = 1L, rle = 2L, 
        lzw = 5L, jpeg = 7L, zip = 8L, `lzw+p` = 15L, `zip+p` = 18L)
    if (type == "quartz" && capabilities("aqua")) {
        width <- g$width/ifelse(is.na(res), 72, res)
        height <- g$height/ifelse(is.na(res), 72, res)
        invisible(.External(C_Quartz, "tiff", path.expand(filename), 
            width, height, pointsize, d$family, d$antialias != 
                "none", "", bg, "white", if (is.na(res)) NULL else res))
    }
    else if (type == "cairo" && capabilities("cairo")) 
        invisible(.External(C_devCairo, filename, 8L, g$width, 
            g$height, pointsize, bg, res, antialias, comp, d$family, 
            300))
    else invisible(.External2(C_X11, paste("tiff::", comp, ":", 
        filename, sep = ""), g$width, g$height, pointsize, d$gamma, 
        d$colortype, d$maxcubesize, bg, bg, d$fonts, res, 0L, 
        0L, "", 0, 0, d$family))
}


check.options <- function (new, name.opt, reset = FALSE, assign.opt = FALSE, envir = .GlobalEnv, 
    check.attributes = c("mode", "length"), override.check = FALSE) 
{
    lnew <- length(new)
    if (lnew != length(newnames <- names(new))) 
        stop(gettextf("invalid arguments in '%s' (need named args)", 
            deparse(sys.call(sys.parent()))), domain = NA)
    if (!is.character(name.opt)) 
        stop("'name.opt' must be character, name of an existing list")
    if (reset) {
        if (exists(name.opt, envir = envir, inherits = FALSE)) {
            if (length(utils::find(name.opt)) > 1) 
                rm(list = name.opt, envir = envir)
        }
        else stop(gettextf("cannot reset non-existent '%s'", 
            name.opt), domain = NA)
    }
    old <- get(name.opt, envir = envir, inherits = FALSE)
    if (!is.list(old)) 
        stop(gettextf("invalid options in '%s'", name.opt), domain = NA)
    oldnames <- names(old)
    if (lnew > 0) {
        matches <- pmatch(newnames, oldnames)
        if (any(is.na(matches))) 
            stop(sprintf(ngettext(as.integer(sum(is.na(matches))), 
                "invalid argument name %s in '%s'", "invalid argument names %s in '%s'"), 
                paste(sQuote(newnames[is.na(matches)]), collapse = ", "), 
                deparse(sys.call(sys.parent()))), domain = NA)
        else {
            i.match <- oldnames[matches]
            prev <- old[i.match]
            doubt <- rep.int(FALSE, length(prev))
            for (fn in check.attributes) if (any(ii <- sapply(prev, 
                fn) != sapply(new, fn))) {
                ii <- ii & (names(prev) != "fonts")
                if (!any(ii)) 
                  next
                doubt <- doubt | ii
                do.keep <- ii & !override.check
                warning(paste(sQuote(paste0(fn, "(", names(prev[ii]), 
                  ")")), collapse = " and "), " ", ngettext(as.integer(sum(ii)), 
                  "differs between new and previous", "differ between new and previous"), 
                  if (any(do.keep)) {
                    paste("\n\t ==> ", gettextf("NOT changing %s", 
                      paste(sQuote(names(prev[do.keep])), collapse = " & ")), 
                      sep = "")
                  }
                  else "", domain = NA, call. = FALSE)
            }
            names(new) <- NULL
            if (any(doubt)) {
                ii <- !doubt | override.check
                old[i.match[ii]] <- new[ii]
            }
            else old[i.match] <- new
        }
        if (assign.opt) 
            assign(name.opt, old, envir = envir)
    }
    old
}


rainbow <- function (n, s = 1, v = 1, start = 0, end = max(1, n - 1)/n, 
    alpha = 1) 
{
    if ((n <- as.integer(n[1L])) > 0) {
        if (start == end || any(c(start, end) < 0) || any(c(start, 
            end) > 1)) 
            stop("'start' and 'end' must be distinct and in [0, 1].")
        hsv(h = seq.int(start, ifelse(start > end, 1, 0) + end, 
            length.out = n)%%1, s, v, alpha)
    }
    else character()
}


nclass.FD <- function (x) 
{
    h <- stats::IQR(x)
    if (h == 0) 
        h <- stats::mad(x, constant = 2)
    if (h > 0) 
        ceiling(diff(range(x))/(2 * h * length(x)^(-1/3)))
    else 1L
}


dev.copy2eps <- function (...) 
{
    current.device <- dev.cur()
    nm <- names(current.device)[1L]
    if (nm == "null device") 
        stop("no device to print from")
    if (!dev.displaylist()) 
        stop("can only print from a screen device")
    oc <- match.call()
    oc[[1L]] <- quote(grDevices::dev.copy)
    oc$device <- postscript
    oc$onefile <- FALSE
    oc$horizontal <- FALSE
    if (is.null(oc$paper)) 
        oc$paper <- "special"
    din <- dev.size("in")
    w <- din[1L]
    h <- din[2L]
    if (is.null(oc$width)) 
        oc$width <- if (!is.null(oc$height)) 
            w/h * eval.parent(oc$height)
        else w
    if (is.null(oc$height)) 
        oc$height <- if (!is.null(oc$width)) 
            h/w * eval.parent(oc$width)
        else h
    if (is.null(oc$file)) 
        oc$file <- "Rplot.eps"
    on.exit(dev.set(current.device))
    dev.off(eval.parent(oc))
}


nclass.Sturges <- function (x) 
ceiling(log2(length(x)) + 1)


replayPlot <- function (x, reloadPkgs = FALSE) 
{
    if (!inherits(x, "recordedplot")) 
        stop(gettextf("argument is not of class %s", dQuote("recordedplot")), 
            domain = NA)
    pid <- attr(x, "pid")
    if (is.null(pid) || pid != Sys.getpid()) {
        x <- restoreRecordedPlot(x, reloadPkgs)
    }
    invisible(.External2(C_playSnapshot, x))
}


dev.off <- function (which = dev.cur()) 
{
    if (which == 1) 
        stop("cannot shut down device 1 (the null device)")
    .External(C_devoff, as.integer(which))
    dev.cur()
}


jpeg <- function (filename = "Rplot%03d.jpeg", width = 480, height = 480, 
    units = "px", pointsize = 12, quality = 75, bg = "white", 
    res = NA, ..., type = c("cairo", "Xlib", "quartz"), antialias) 
{
    if (!checkIntFormat(filename)) 
        stop("invalid 'filename'")
    g <- .geometry(width, height, units, res)
    new <- list(...)
    type <- if (!missing(type)) 
        match.arg(type)
    else getOption("bitmapType")
    if (!missing(antialias)) 
        new$antialias <- match.arg(antialias, aa.cairo)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    antialias <- match(d$antialias, aa.cairo)
    if (type == "quartz" && capabilities("aqua")) {
        width <- g$width/ifelse(is.na(res), 72, res)
        height <- g$height/ifelse(is.na(res), 72, res)
        invisible(.External(C_Quartz, "jpeg", path.expand(filename), 
            width, height, pointsize, d$family, d$antialias != 
                "none", "", bg, "white", if (is.na(res)) NULL else res))
    }
    else if (type == "cairo" && capabilities("cairo")) 
        invisible(.External(C_devCairo, filename, 3L, g$width, 
            g$height, pointsize, bg, res, antialias, quality, 
            d$family, 300))
    else invisible(.External2(C_X11, paste("jpeg::", quality, 
        ":", filename, sep = ""), g$width, g$height, pointsize, 
        d$gamma, d$colortype, d$maxcubesize, bg, bg, d$fonts, 
        res, 0L, 0L, "", 0, 0, d$family))
}


colors <- function (distinct = FALSE) 
{
    c <- .Call(C_colors)
    if (distinct) 
        c[!duplicated(t(col2rgb(c)))]
    else c
}


setGraphicsEventHandlers <- function (which = dev.cur(), ...) 
setGraphicsEventEnv(which, as.environment(list(...)))


n2mfrow <- function (nr.plots) 
{
    if (nr.plots <= 3) 
        c(nr.plots, 1)
    else if (nr.plots <= 6) 
        c((nr.plots + 1)%/%2, 2)
    else if (nr.plots <= 12) 
        c((nr.plots + 2)%/%3, 3)
    else c(nrow <- ceiling(sqrt(nr.plots)), ceiling(nr.plots/nrow))
}


colorConverter <- function (toXYZ, fromXYZ, name, white = NULL) 
{
    rval <- list(toXYZ = toXYZ, fromXYZ = fromXYZ, name = name, 
        white = white)
    class(rval) <- "colorConverter"
    rval
}


setPS <- function (...) 
{
    dots <- list(...)
    args <- list(width = 0, height = 0)
    args[names(dots)] <- dots
    force <- list(onefile = TRUE, horizontal = TRUE, paper = "default")
    args[names(force)] <- force
    do.call("ps.options", args)
}


cm <- function (x) 
2.54 * x


dev.copy <- function (device, ..., which = dev.next()) 
{
    if (!missing(which) & !missing(device)) 
        stop("cannot supply 'which' and 'device' at the same time")
    old.device <- dev.cur()
    if (old.device == 1) 
        stop("cannot copy from the null device")
    if (missing(device)) {
        if (which == 1) 
            stop("cannot copy to the null device")
        else if (which == dev.cur()) 
            stop("cannot copy device to itself")
        dev.set(which)
    }
    else {
        if (!is.function(device)) 
            stop("'device' should be a function")
        else device(...)
    }
    on.exit(dev.set(old.device))
    .External(C_devcopy, old.device)
    on.exit()
    dev.cur()
}


pictex <- function (file = "Rplots.tex", width = 5, height = 4, debug = FALSE, 
    bg = "white", fg = "black") 
{
    .External(C_PicTeX, file, bg, fg, width, height, as.logical(debug))
    graphics::par(mar = c(5, 4, 2, 4) + 0.1)
}


recordGraphics <- function (expr, list, env) 
.Internal(recordGraphics(substitute(expr), list, env))


rgb2hsv <- function (r, g = NULL, b = NULL, maxColorValue = 255) 
{
    rgb <- if (is.null(g) && is.null(b)) 
        as.matrix(r)
    else rbind(r, g, b)
    if (!is.numeric(rgb)) 
        stop("rgb matrix must be numeric")
    d <- dim(rgb)
    if (d[1L] != 3L) 
        stop("rgb matrix must have 3 rows")
    n <- d[2L]
    if (n == 0L) 
        return(cbind(c(h = 1, s = 1, v = 1))[, 0L])
    rgb <- rgb/maxColorValue
    if (any(0 > rgb) || any(rgb > 1)) 
        stop("rgb values must be in [0, maxColorValue]")
    .Call(C_RGB2hsv, rgb)
}


gray <- function (level, alpha = NULL) 
.Call(C_gray, level, alpha)


extendrange <- function (x, r = range(x, na.rm = TRUE), f = 0.05) 
{
    if (!missing(r) && length(r) != 2) 
        stop("'r' must be a \"range\", hence of length 2")
    r + c(-f, f) * diff(r)
}


is.raster <- function (x) 
inherits(x, "raster")


colorspaces <- structure(list(XYZ = structure(list(toXYZ = function (x, w) 
x, fromXYZ = function (x, w) 
x, name = "XYZ", white = NULL), .Names = c("toXYZ", "fromXYZ", 
"name", "white"), class = "colorConverter"), `Apple RGB` = structure(list(
    toXYZ = function (rgb, ...) 
    {
        dogamma(rgb) %*% M
    }, fromXYZ = function (xyz, ...) 
    {
        ungamma(xyz %*% solve(M))
    }, gamma = 1.8, reference.white = "D65", name = "Apple RGB"), .Names = c("toXYZ", 
"fromXYZ", "gamma", "reference.white", "name"), class = c("RGBcolorConverter", 
"colorConverter")), sRGB = structure(list(toXYZ = function (rgb, 
    ...) 
{
    dogamma(rgb) %*% M
}, fromXYZ = function (xyz, ...) 
{
    ungamma(xyz %*% solve(M))
}, gamma = "sRGB", reference.white = "D65", name = "sRGB"), .Names = c("toXYZ", 
"fromXYZ", "gamma", "reference.white", "name"), class = c("RGBcolorConverter", 
"colorConverter")), `CIE RGB` = structure(list(toXYZ = function (rgb, 
    ...) 
{
    dogamma(rgb) %*% M
}, fromXYZ = function (xyz, ...) 
{
    ungamma(xyz %*% solve(M))
}, gamma = 2.2, reference.white = "E", name = "CIE RGB"), .Names = c("toXYZ", 
"fromXYZ", "gamma", "reference.white", "name"), class = c("RGBcolorConverter", 
"colorConverter")), Lab = structure(list(toXYZ = function (Lab, 
    white) 
{
    stopifnot(length(Lab) == 3L)
    epsilon <- 216/24389
    kappa <- 24389/27
    yr <- ifelse(Lab[1L] < kappa * epsilon, Lab[1L]/kappa, ((Lab[1L] + 
        16)/116)^3)
    fy <- ifelse(yr <= epsilon, (kappa * yr + 16)/116, (Lab[1L] + 
        16)/116)
    fx <- Lab[2L]/500 + fy
    fz <- fy - Lab[3L]/200
    zr <- ifelse(fz^3 <= epsilon, (116 * fz - 16)/kappa, fz^3)
    xr <- ifelse(fx^3 <= epsilon, (116 * fx - 16)/kappa, fx^3)
    c(X = xr, Y = yr, Z = zr) * white
}, fromXYZ = function (XYZ, white) 
{
    stopifnot(length(XYZ) == 3L)
    epsilon <- 216/24389
    kappa <- 24389/27
    xyzr <- XYZ/white
    fxyz <- ifelse(xyzr <= epsilon, (kappa * xyzr + 16)/116, 
        xyzr^(1/3))
    c(L = 116 * fxyz[2L] - 16, a = 500 * (fxyz[1L] - fxyz[2L]), 
        b = 200 * (fxyz[2L] - fxyz[3L]))
}, name = "Lab", white = NULL), .Names = c("toXYZ", "fromXYZ", 
"name", "white"), class = "colorConverter"), Luv = structure(list(
    toXYZ = function (Luv, white) 
    {
        epsilon <- 216/24389
        kappa <- 24389/27
        if (Luv[1L] == 0) 
            return(c(0, 0, 0))
        u0 <- 4 * white[1L]/(white[1L] + 15 * white[2L] + 3 * 
            white[3L])
        v0 <- 9 * white[2L]/(white[1L] + 15 * white[2L] + 3 * 
            white[3L])
        Y <- ifelse(Luv[1L] <= kappa * epsilon, Luv[1L]/kappa, 
            ((Luv[1L] + 16)/116)^3)
        a <- (52 * Luv[1L]/(Luv[2L] + 13 * Luv[1L] * u0) - 1)/3
        b <- -5 * Y
        c <- -1/3
        d <- Y * (39 * Luv[1L]/(Luv[3L] + 13 * Luv[1L] * v0) - 
            5)
        X <- (d - b)/(a - c)
        Z <- X * a + b
        c(X = X, Y = Y, Z = Z)
    }, fromXYZ = function (XYZ, white) 
    {
        epsilon <- 216/24389
        kappa <- 24389/27
        yr <- XYZ[2L]/white[2L]
        denom <- sum(XYZ * c(1, 15, 3))
        wdenom <- sum(white * c(1, 15, 3))
        u1 <- ifelse(denom == 0, 1, 4 * XYZ[1L]/denom)
        v1 <- ifelse(denom == 0, 1, 9 * XYZ[2L]/denom)
        ur <- 4 * white[1L]/wdenom
        vr <- 9 * white[2L]/wdenom
        L <- ifelse(yr <= epsilon, kappa * yr, 116 * (yr^(1/3)) - 
            16)
        c(L = L, u = 13 * L * (u1 - ur), v = 13 * L * (v1 - vr))
    }, name = "Luv", white = NULL), .Names = c("toXYZ", "fromXYZ", 
"name", "white"), class = "colorConverter")), .Names = c("XYZ", 
"Apple RGB", "sRGB", "CIE RGB", "Lab", "Luv"))


getGraphicsEvent <- function (prompt = "Waiting for input", onMouseDown = NULL, onMouseMove = NULL, 
    onMouseUp = NULL, onKeybd = NULL, consolePrompt = prompt) 
{
    if (!interactive()) 
        return(NULL)
    if (!missing(prompt) || !missing(onMouseDown) || !missing(onMouseMove) || 
        !missing(onMouseUp) || !missing(onKeybd)) {
        setGraphicsEventHandlers(prompt = prompt, onMouseDown = onMouseDown, 
            onMouseMove = onMouseMove, onMouseUp = onMouseUp, 
            onKeybd = onKeybd)
    }
    .External2(C_getGraphicsEvent, consolePrompt)
}


pdfFonts <- function (...) 
{
    initPSandPDFfonts()
    ndots <- length(fonts <- list(...))
    if (ndots == 0L) 
        get(".PDF.Fonts", envir = .PSenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0L) {
            if (!all(sapply(fonts, is.character))) 
                stop(gettextf("invalid arguments in '%s' (must be font names)", 
                  "pdfFonts"), domain = NA)
            else get(".PDF.Fonts", envir = .PSenv)[unlist(fonts)]
        }
        else {
            if (ndots != nnames) 
                stop(gettextf("invalid arguments in '%s' (need named args)", 
                  "pdfFonts"), domain = NA)
            setFonts(fonts, fontNames, ".PDF.Fonts")
        }
    }
}


convertColor <- function (color, from, to, from.ref.white = NULL, to.ref.white = NULL, 
    scale.in = 1, scale.out = 1, clip = TRUE) 
{
    if (is.character(from)) 
        from <- colorspaces[[match.arg(from, names(colorspaces))]]
    if (!inherits(from, "colorConverter")) 
        stop("'from' must be a \"colorConverter\" object or a string")
    if (is.character(to)) 
        to <- colorspaces[[match.arg(to, names(colorspaces))]]
    if (!inherits(to, "colorConverter")) 
        stop("'to' must be a \"colorConverter\" object or a string")
    if (is.null(from.ref.white)) 
        from.ref.white <- from$white
    else if (!is.null(from$white) && from.ref.white != from$white) 
        stop(gettextf("'from.ref.white' disagrees with definition of %s", 
            from$name), domain = NA)
    if (is.null(to.ref.white)) 
        to.ref.white <- to$white
    else if (!is.null(to$white) && to.ref.white != to$white) 
        stop(gettextf("'to.ref.white' disagrees with definition of %s", 
            to$name), domain = NA)
    if (is.null(to.ref.white) && is.null(from.ref.white)) 
        to.ref.white <- from.ref.white <- "D65"
    if (is.null(to.ref.white)) 
        to.ref.white <- from.ref.white
    if (is.null(from.ref.white)) 
        from.ref.white <- to.ref.white
    from.ref.white <- c2to3(white.points[, from.ref.white])
    to.ref.white <- c2to3(white.points[, to.ref.white])
    if (is.null(nrow(color))) 
        color <- matrix(color, nrow = 1L)
    if (!is.null(scale.in)) 
        color <- color/scale.in
    trim <- function(rgb) {
        rgb <- round(rgb, 5)
        if (is.na(clip)) 
            rgb[rgb < 0 | rgb > 1] <- NaN
        else if (clip) {
            rgb[rgb < 0] <- 0
            rgb[rgb > 1] <- 1
        }
        rgb
    }
    xyz <- apply(color, 1L, from$toXYZ, from.ref.white)
    if (is.null(nrow(xyz))) 
        xyz <- matrix(xyz, nrow = 1L)
    if (!isTRUE(all.equal(from.ref.white, to.ref.white))) {
        mc <- match.call()
        if (is.null(mc$from.ref.white) || is.null(mc$to.ref.white)) 
            warning("color spaces use different reference whites")
        xyz <- chromaticAdaptation(xyz, from.ref.white, to.ref.white)
    }
    rval <- apply(xyz, 2L, to$fromXYZ, to.ref.white)
    if (inherits(to, "RGBcolorConverter")) 
        rval <- trim(rval)
    if (is.matrix(rval)) 
        rval <- t(rval)
    if (is.null(scale.out)) 
        rval
    else rval * scale.out
}


dev.capabilities <- function (what = NULL) 
{
    zz <- .External(C_devcap)
    z <- vector("list", 6L)
    names(z) <- c("semiTransparency", "transparentBackground", 
        "rasterImage", "capture", "locator", "events")
    z[[1L]] <- c(NA, FALSE, TRUE)[zz[1L] + 1L]
    z[[2L]] <- c(NA, "no", "fully", "semi")[zz[2L] + 1L]
    z[[3L]] <- c(NA, "no", "yes", "non-missing")[zz[3L] + 1L]
    z[[4L]] <- c(NA, FALSE, TRUE)[zz[4L] + 1L]
    z[[5L]] <- c(NA, FALSE, TRUE)[zz[5L] + 1L]
    z[[6L]] <- c("", if (zz[6L]) "MouseDown", if (zz[7L]) "MouseMove", 
        if (zz[8L]) "MouseUp", if (zz[9L]) "Keybd")[-1L]
    if (!is.null(what)) 
        z[charmatch(what, names(z), 0L)]
    else z
}


quartz.options <- function (..., reset = FALSE) 
{
    old <- get(".quartz.Options", envir = .Quartzenv)
    if (reset) {
        assign(".quartz.Options", get(".quartz.Options.default", 
            envir = .Quartzenv), envir = .Quartzenv)
    }
    l... <- length(new <- list(...))
    check.options(new, name.opt = ".quartz.Options", envir = .Quartzenv, 
        assign.opt = l... > 0L)
    if (reset || l... > 0L) 
        invisible(old)
    else old
}


chull <- function (x, y = NULL) 
{
    X <- xy.coords(x, y, recycle = TRUE)
    x <- cbind(X$x, X$y)
    if (any(!is.finite(x))) 
        stop("finite coordinates are needed")
    if (nrow(x) == 0) 
        return(integer())
    if (nrow(x) == 1) 
        return(1L)
    res <- .Call(C_chull, x)
    if (length(res) < 2L) 
        return(res)
    xx <- sweep(x[res, ], 2L, colMeans(x[res, ]))
    angs <- atan2(xx[, 2L], -xx[, 1L])
    res[order(angs)]
}


blues9 <- c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", 
"#2171B5", "#08519C", "#08306B")


col2rgb <- function (col, alpha = FALSE) 
{
    if (any(as.character(col) %in% "0")) 
        stop("numerical color values must be positive", domain = NA)
    if (is.factor(col)) 
        col <- as.character(col)
    .Call(C_col2rgb, col, alpha)
}


palette <- function (value) 
{
    if (missing(value)) 
        .Call(C_palette, character())
    else invisible(.Call.graphics(C_palette, value))
}


quartzFonts <- function (...) 
{
    ndots <- length(fonts <- list(...))
    if (ndots == 0L) 
        get(".Quartz.Fonts", envir = .Quartzenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0L) {
            if (!all(sapply(fonts, is.character))) 
                stop("invalid arguments in 'quartzFonts' (must be font names)")
            else get(".Quartz.Fonts", envir = .Quartzenv)[unlist(fonts)]
        }
        else {
            if (ndots != nnames) 
                stop("invalid arguments in 'quartzFonts' (need named args)")
            setQuartzFonts(fonts, fontNames)
        }
    }
}


hsv <- function (h = 1, s = 1, v = 1, alpha = 1) 
.Call(C_hsv, h, s, v, if (missing(alpha)) NULL else alpha)


Hershey <- structure(list(typeface = c("serif", "sans serif", "script", 
"gothic english", "gothic german", "gothic italian", "serif symbol", 
"sans serif symbol"), fontindex = c("plain", "italic", "bold", 
"bold italic", "cyrillic", "oblique cyrillic", "EUC"), allowed = structure(c(1L, 
1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 5L, 6L, 
7L, 7L, 7L, 7L, 8L, 8L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 1L, 2L, 3L, 
4L, 1L, 2L, 3L, 1L, 1L, 1L, 1L, 2L, 3L, 4L, 1L, 2L), .Dim = c(23L, 
2L))), .Names = c("typeface", "fontindex", "allowed"))


cm.colors <- function (n, alpha = 1) 
{
    if ((n <- as.integer(n[1L])) > 0L) {
        even.n <- n%%2L == 0L
        k <- n%/%2L
        l1 <- k + 1L - even.n
        l2 <- n - k + even.n
        c(if (l1 > 0L) hsv(h = 6/12, s = seq.int(0.5, ifelse(even.n, 
            0.5/k, 0), length.out = l1), v = 1, alpha = alpha), 
            if (l2 > 1) hsv(h = 10/12, s = seq.int(0, 0.5, length.out = l2)[-1L], 
                v = 1, alpha = alpha))
    }
    else character()
}


cairo_pdf <- function (filename = if (onefile) "Rplots.pdf" else "Rplot%03d.pdf", 
    width = 7, height = 7, pointsize = 12, onefile = FALSE, family = "sans", 
    bg = "white", antialias = c("default", "none", "gray", "subpixel"), 
    fallback_resolution = 300) 
{
    if (!checkIntFormat(filename)) 
        stop("invalid 'filename'")
    antialiases <- eval(formals()$antialias)
    antialias <- match(match.arg(antialias, antialiases), antialiases)
    invisible(.External(C_devCairo, filename, 6L, 72 * width, 
        72 * height, pointsize, bg, NA_integer_, antialias, onefile, 
        family, fallback_resolution))
}


colours <- function (distinct = FALSE) 
{
    c <- .Call(C_colors)
    if (distinct) 
        c[!duplicated(t(col2rgb(c)))]
    else c
}


dev.interactive <- function (orNone = FALSE) 
{
    if (!interactive()) 
        return(FALSE)
    if (.Device %in% .known_interactive.devices) 
        return(TRUE)
    if (!(orNone && .Device == "null device")) 
        return(FALSE)
    newdev <- getOption("device")
    if (is.character(newdev)) 
        newdev %in% .known_interactive.devices
    else {
        if (.Platform$OS.type == "windows") 
            identical(newdev, windows)
        else identical(newdev, X11) || identical(newdev, quartz)
    }
}


adjustcolor <- function (col, alpha.f = 1, red.f = 1, green.f = 1, blue.f = 1, 
    offset = c(0, 0, 0, 0), transform = diag(c(red.f, green.f, 
        blue.f, alpha.f))) 
{
    stopifnot(length(offset)%%4L == 0L, !is.null(d <- dim(transform)), 
        d == c(4L, 4L))
    x <- col2rgb(col, alpha = TRUE)/255
    x[] <- pmax(0, pmin(1, transform %*% x + matrix(offset, nrow = 4L, 
        ncol = ncol(x))))
    rgb(x[1L, ], x[2L, ], x[3L, ], x[4L, ])
}


dev.new <- function (..., noRStudioGD = FALSE) 
{
    dev <- getOption("device")
    if (!is.character(dev) && !is.function(dev)) 
        stop("invalid setting for 'getOption(\"device\")'")
    if (noRStudioGD && is.character(dev) && dev == "RStudioGD") 
        dev <- .select_device()
    if (is.character(dev)) {
        dev <- if (exists(dev, .GlobalEnv)) 
            get(dev, .GlobalEnv)
        else if (exists(dev, asNamespace("grDevices"))) 
            get(dev, asNamespace("grDevices"))
        else stop(gettextf("device '%s' not found", dev), domain = NA)
    }
    a <- list(...)
    a2 <- names(formals(dev))
    a <- a[names(a) %in% a2]
    if (identical(dev, pdf)) {
        if (is.null(a[["file"]]) && file.exists("Rplots.pdf")) {
            fe <- file.exists(tmp <- paste0("Rplots", 1L:999, 
                ".pdf"))
            if (all(fe)) 
                stop("no suitable unused file name for pdf()")
            message(gettextf("dev.new(): using pdf(file=\"%s\")", 
                tmp[!fe][1L]), domain = NA)
            a$file <- tmp[!fe][1L]
        }
    }
    else if (identical(dev, postscript)) {
        if (is.null(a[["file"]]) && file.exists("Rplots.ps")) {
            fe <- file.exists(tmp <- paste0("Rplots", 1L:999, 
                ".ps"))
            if (all(fe)) 
                stop("no suitable unused file name for postscript()")
            message(gettextf("dev.new(): using postscript(file=\"%s\")", 
                tmp[!fe][1L]), domain = NA)
            a$file <- tmp[!fe][1L]
        }
    }
    else if (!is.null(a[["width"]]) && !is.null(a[["height"]]) && 
        (identical(dev, png) || identical(dev, jpeg) || identical(dev, 
            bmp) || identical(dev, tiff))) {
        if (is.null(a[["units"]]) && is.null(a[["res"]])) {
            a$units <- "in"
            a$res <- 72
        }
    }
    do.call(dev, a)
}


X11.options <- function (..., reset = FALSE) 
{
    old <- get(".X11.Options", envir = .X11env)
    if (reset) {
        assign(".X11.Options", get(".X11.Options.default", envir = .X11env), 
            envir = .X11env)
    }
    l... <- length(new <- list(...))
    check.options(new, name.opt = ".X11.Options", envir = .X11env, 
        assign.opt = l... > 0)
    if (reset || l... > 0) 
        invisible(old)
    else old
}


trans3d <- function (x, y, z, pmat) 
{
    tr <- cbind(x, y, z, 1) %*% pmat
    list(x = tr[, 1]/tr[, 4], y = tr[, 2]/tr[, 4])
}


dev.control <- function (displaylist = c("inhibit", "enable")) 
{
    if (dev.cur() <= 1) 
        stop("dev.control() called without an open graphics device")
    if (!missing(displaylist)) {
        displaylist <- match.arg(displaylist)
        .External(C_devcontrol, displaylist == "enable")
    }
    else stop("argument is missing with no default")
    invisible()
}


dev.next <- function (which = dev.cur()) 
{
    if (!exists(".Devices")) 
        .Devices <- list("null.device")
    num.device <- .External(C_devnext, as.integer(which))
    names(num.device) <- .Devices[[num.device]]
    num.device
}


CIDFont <- function (family, cmap, cmapEncoding, pdfresource = "") 
{
    font <- list(family = family, metrics = c("", "", "", ""), 
        cmap = cmap, cmapEncoding = cmapEncoding, pdfresource = pdfresource)
    class(font) <- "CIDFont"
    checkFont(font)
}


dev.capture <- function (native = FALSE) 
.External(C_devcapture, native)


bmp <- function (filename = "Rplot%03d.bmp", width = 480, height = 480, 
    units = "px", pointsize = 12, bg = "white", res = NA, ..., 
    type = c("cairo", "Xlib", "quartz"), antialias) 
{
    if (!checkIntFormat(filename)) 
        stop("invalid 'filename'")
    g <- .geometry(width, height, units, res)
    new <- list(...)
    type <- if (!missing(type)) 
        match.arg(type)
    else getOption("bitmapType")
    if (!missing(antialias)) 
        new$antialias <- match.arg(antialias, aa.cairo)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    antialias <- match(d$antialias, aa.cairo)
    if (type == "quartz" && capabilities("aqua")) {
        width <- g$width/ifelse(is.na(res), 72, res)
        height <- g$height/ifelse(is.na(res), 72, res)
        invisible(.External(C_Quartz, "bmp", path.expand(filename), 
            width, height, pointsize, d$family, d$antialias != 
                "none", "", bg, "white", if (is.na(res)) NULL else res))
    }
    else if (type == "cairo" && capabilities("cairo")) 
        invisible(.External(C_devCairo, filename, 9L, g$width, 
            g$height, pointsize, bg, res, antialias, 100L, d$family, 
            300))
    else invisible(.External2(C_X11, paste("bmp::", filename, 
        sep = ""), g$width, g$height, pointsize, d$gamma, d$colortype, 
        d$maxcubesize, bg, bg, d$fonts, res, 0L, 0L, "", 0, 0, 
        d$family))
}


png <- function (filename = "Rplot%03d.png", width = 480, height = 480, 
    units = "px", pointsize = 12, bg = "white", res = NA, ..., 
    type = c("cairo", "cairo-png", "Xlib", "quartz"), antialias) 
{
    if (!checkIntFormat(filename)) 
        stop("invalid 'filename'")
    g <- .geometry(width, height, units, res)
    new <- list(...)
    if (missing(type)) 
        type <- getOption("bitmapType")
    type <- match.arg(type)
    if (!missing(antialias)) 
        new$antialias <- match.arg(antialias, aa.cairo)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    antialias <- match(d$antialias, aa.cairo)
    if (type == "quartz" && capabilities("aqua")) {
        width <- g$width/ifelse(is.na(res), 72, res)
        height <- g$height/ifelse(is.na(res), 72, res)
        invisible(.External(C_Quartz, "png", path.expand(filename), 
            width, height, pointsize, d$family, d$antialias != 
                "none", "", bg, "white", if (is.na(res)) NULL else res))
    }
    else if (type == "cairo" && capabilities("cairo")) 
        invisible(.External(C_devCairo, filename, 2L, g$width, 
            g$height, pointsize, bg, res, antialias, 100L, d$family, 
            300))
    else if (type == "cairo-png" && capabilities("cairo")) 
        invisible(.External(C_devCairo, filename, 5L, g$width, 
            g$height, pointsize, bg, res, antialias, 100L, d$family, 
            300))
    else invisible(.External2(C_X11, paste("png::", filename, 
        sep = ""), g$width, g$height, pointsize, d$gamma, d$colortype, 
        d$maxcubesize, bg, bg, d$fonts, res, 0L, 0L, "", 0, 0, 
        d$family))
}


xfig <- function (file = if (onefile) "Rplots.fig" else "Rplot%03d.fig", 
    onefile = FALSE, encoding = "none", paper = "default", horizontal = TRUE, 
    width = 0, height = 0, family = "Helvetica", pointsize = 12, 
    bg = "transparent", fg = "black", pagecentre = TRUE, defaultfont = FALSE, 
    textspecial = FALSE) 
{
    initPSandPDFfonts()
    if (!checkIntFormat(file)) 
        stop(gettextf("invalid 'file' argument '%s'", file), 
            domain = NA)
    .External(C_XFig, file, paper, family, bg, fg, width, height, 
        horizontal, pointsize, onefile, pagecentre, defaultfont, 
        textspecial, encoding)
    invisible()
}


nclass.scott <- function (x) 
{
    h <- 3.5 * sqrt(stats::var(x)) * length(x)^(-1/3)
    if (h > 0) 
        ceiling(diff(range(x))/h)
    else 1L
}


gray.colors <- function (n, start = 0.3, end = 0.9, gamma = 2.2, alpha = NULL) 
gray(seq.int(from = start^gamma, to = end^gamma, length.out = n)^(1/gamma), 
    alpha)


deviceIsInteractive <- function (name = NULL) 
{
    if (length(name)) {
        if (!is.character(name)) 
            stop("'name' must be a character vector")
        unlockBinding(".known_interactive.devices", asNamespace("grDevices"))
        .known_interactive.devices <<- c(.known_interactive.devices, 
            name)
        lockBinding(".known_interactive.devices", asNamespace("grDevices"))
        invisible(.known_interactive.devices)
    }
    else .known_interactive.devices
}


setGraphicsEventEnv <- function (which = dev.cur(), env) 
{
    which <- as.integer(which)
    stopifnot(length(which) == 1)
    result <- getGraphicsEventEnv(which)
    env$which <- which
    .External2(C_setGraphicsEventEnv, which, env)
    invisible(result)
}


quartz <- function (title, width, height, pointsize, family, antialias, 
    type, file = NULL, bg, canvas, dpi) 
{
    if (missing(type) || type %in% c("", "native", "Cocoa")) {
        check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
        msg <- "screen devices should not be used in examples etc"
        if (identical(check, "stop")) 
            stop(msg, domain = NA)
        else if (identical(check, "warn")) 
            warning(msg, immediate. = TRUE, noBreaks. = TRUE, 
                domain = NA)
    }
    new <- list()
    if (!missing(title)) 
        new$title <- title
    if (!missing(width)) 
        new$width <- width
    if (!missing(height)) 
        new$height <- height
    if (!missing(pointsize)) 
        new$pointsize <- pointsize
    if (!missing(family)) 
        new$family <- family
    if (!missing(antialias)) 
        new$antialias <- antialias
    if (!missing(bg)) 
        new$bg <- bg
    if (!missing(canvas)) 
        new$canvas <- canvas
    if (!missing(type)) 
        new$type <- type
    if (!missing(dpi)) 
        new$dpi <- dpi
    if (!checkIntFormat(new$title)) 
        stop("invalid 'title'")
    if (!is.null(file) && !checkIntFormat(file)) 
        stop("invalid 'file'")
    d <- check.options(new, name.opt = ".quartz.Options", envir = .Quartzenv)
    .External(C_Quartz, d$type, file, d$width, d$height, d$pointsize, 
        d$family, d$antialias, d$title, d$bg, d$canvas, if (is.na(d$dpi)) NULL else d$dpi)
    invisible()
}


X11Fonts <- function (...) 
{
    ndots <- length(fonts <- list(...))
    if (ndots == 0) 
        get(".X11.Fonts", envir = .X11env)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0) {
            if (!all(sapply(fonts, is.character))) 
                stop("invalid arguments in 'X11Fonts' (must be font names)")
            else get(".X11.Fonts", envir = .X11env)[unlist(fonts)]
        }
        else {
            if (ndots != nnames) 
                stop("invalid arguments in 'X11Fonts' (need named args)")
            setX11Fonts(fonts, fontNames)
        }
    }
}


X11 <- function (display = "", width, height, pointsize, gamma, bg, 
    canvas, fonts, family, xpos, ypos, title, type, antialias) 
{
    if (display != "XImage") {
        check <- Sys.getenv("_R_CHECK_SCREEN_DEVICE_", "")
        msg <- "screen devices should not be used in examples etc"
        if (identical(check, "stop")) 
            stop(msg, domain = NA)
        else if (identical(check, "warn")) 
            warning(msg, immediate. = TRUE, noBreaks. = TRUE, 
                domain = NA)
    }
    if (display == "" && .Platform$GUI == "AQUA" && is.na(Sys.getenv("DISPLAY", 
        NA))) 
        Sys.setenv(DISPLAY = ":0")
    new <- list()
    if (!missing(display)) 
        new$display <- display
    if (!missing(width)) 
        new$width <- width
    if (!missing(height)) 
        new$height <- height
    if (!missing(gamma)) 
        new$gamma <- gamma
    if (!missing(pointsize)) 
        new$pointsize <- pointsize
    if (!missing(bg)) 
        new$bg <- bg
    if (!missing(canvas)) 
        new$canvas <- canvas
    if (!missing(xpos)) 
        new$xpos <- xpos
    if (!missing(ypos)) 
        new$ypos <- ypos
    if (!missing(title)) 
        new$title <- title
    if (!checkIntFormat(new$title)) 
        stop("invalid 'title'")
    if (!missing(type)) {
        new$type <- match.arg(type, c("Xlib", "cairo", "nbcairo", 
            "dbcairo"))
        if (!capabilities("cairo") && type != "Xlib") 
            warning("cairo-based types are not supported on this build - using \"Xlib\"")
    }
    if (!missing(family)) 
        new$family <- family
    if (!missing(fonts)) 
        new$fonts <- fonts
    if (!missing(antialias) && type != "Xlib") 
        new$antialias <- match.arg(antialias, aa.cairo)
    d <- check.options(new, name.opt = ".X11.Options", envir = .X11env)
    if (d$type == "Xlib" && !missing(family)) {
        fns <- X11Fonts()
        if (!family %in% names(fns)) 
            stop("unknown family for X11(type = \"XLib\")")
        d$fonts[1] <- fns[[family]]
    }
    type <- if (capabilities("cairo")) 
        switch(d$type, cairo = 1L, nbcairo = 2L, dbcairo = 3L, 
            0L)
    else 0L
    if (display == "XImage") 
        type <- 0L
    antialias <- match(d$antialias, aa.cairo)
    if (grepl("darwin", R.version$os)) 
        check_for_XQuartz()
    .External2(C_X11, d$display, d$width, d$height, d$pointsize, 
        d$gamma, d$colortype, d$maxcubesize, d$bg, d$canvas, 
        d$fonts, NA_integer_, d$xpos, d$ypos, d$title, type, 
        antialias, d$family)
    invisible()
}


colorRampPalette <- function (colors, ...) 
{
    ramp <- colorRamp(colors, ...)
    function(n) {
        x <- ramp(seq.int(0, 1, length.out = n))
        if (ncol(x) == 4L) 
            rgb(x[, 1L], x[, 2L], x[, 3L], x[, 4L], maxColorValue = 255)
        else rgb(x[, 1L], x[, 2L], x[, 3L], maxColorValue = 255)
    }
}


rgb <- function (red, green, blue, alpha, names = NULL, maxColorValue = 1) 
{
    if (missing(green) && missing(blue)) {
        if (is.matrix(red) || is.data.frame(red)) {
            red <- data.matrix(red)
            if (ncol(red) < 3L) 
                stop("at least 3 columns needed")
            green <- red[, 2L]
            blue <- red[, 3L]
            red <- red[, 1L]
        }
    }
    .Call(C_rgb, red, green, blue, if (missing(alpha)) NULL else alpha, 
        maxColorValue, names)
}


dev.set <- function (which = dev.next()) 
{
    which <- .External(C_devset, as.integer(which))
    names(which) <- .Devices[[which]]
    which
}


.axisPars <- function (usr, log = FALSE, nintLog = 5) 
{
    .Call(C_R_GAxisPars, usr, log, nintLog)
}


xyz.coords <- function (x, y = NULL, z = NULL, xlab = NULL, ylab = NULL, zlab = NULL, 
    log = NULL, recycle = FALSE) 
{
    if (is.null(y)) {
        if (is.language(x)) {
            if (inherits(x, "formula") && length(x) == 3 && length(rhs <- x[[3L]]) == 
                3) {
                zlab <- deparse(x[[2L]])
                ylab <- deparse(rhs[[3L]])
                xlab <- deparse(rhs[[2L]])
                pf <- parent.frame()
                z <- eval(x[[2L]], environment(x), pf)
                y <- eval(rhs[[3L]], environment(x), pf)
                x <- eval(rhs[[2L]], environment(x), pf)
            }
            else stop("invalid first argument [bad language object]")
        }
        else if (is.matrix(x) || is.data.frame(x)) {
            x <- data.matrix(x)
            if (ncol(x) < 2) 
                stop("at least 2 columns needed")
            if (ncol(x) == 2) {
                xlab <- "Index"
                y <- x[, 1]
                z <- x[, 2]
                x <- seq_along(y)
            }
            else {
                colnames <- dimnames(x)[[2L]]
                if (is.null(colnames)) {
                  zlab <- paste0(xlab, "[,3]")
                  ylab <- paste0(xlab, "[,2]")
                  xlab <- paste0(xlab, "[,1]")
                }
                else {
                  xlab <- colnames[1L]
                  ylab <- colnames[2L]
                  zlab <- colnames[3L]
                }
                y <- x[, 2]
                z <- x[, 3]
                x <- x[, 1]
            }
        }
        else if (is.list(x)) {
            if (all(c("x", "y", "z") %in% names(x))) {
                zlab <- paste0(xlab, "$z")
                ylab <- paste0(xlab, "$y")
                xlab <- paste0(xlab, "$x")
                y <- x[["y"]]
                z <- x[["z"]]
                x <- x[["x"]]
            }
            else stop("'x' is a list, but does not have components 'x', 'y'  and 'z'")
        }
    }
    if (!is.null(y) && is.null(z)) {
        if (is.complex(x)) {
            z <- y
            y <- Im(x)
            x <- Re(x)
            zlab <- ylab
            ylab <- paste0("Im(", xlab, ")")
            xlab <- paste0("Re(", xlab, ")")
        }
        else if (is.complex(y)) {
            z <- x
            x <- Re(y)
            y <- Im(y)
            zlab <- xlab
            xlab <- paste0("Re(", ylab, ")")
            ylab <- paste0("Im(", ylab, ")")
        }
        else {
            if (is.factor(x)) 
                x <- as.numeric(x)
            if (is.factor(y)) 
                y <- as.numeric(y)
            xlab <- "Index"
            z <- y
            y <- x
            x <- seq_along(x)
        }
    }
    if (((xl <- length(x)) != length(y)) || (xl != length(z))) {
        if (recycle) {
            ml <- max(xl, (yl <- length(y)), (zl <- length(z)))
            if (xl < ml && !is.null(x)) 
                x <- rep_len(x, ml)
            if (yl < ml && !is.null(y)) 
                y <- rep_len(y, ml)
            if (zl < ml && !is.null(z)) 
                z <- rep_len(z, ml)
        }
        else stop("'x', 'y' and 'z' lengths differ")
    }
    if (length(log) && log != "") {
        log <- strsplit(log, NULL)[[1L]]
        if ("x" %in% log && any(ii <- x <= 0 & !is.na(x))) {
            n <- sum(ii)
            warning(sprintf(ngettext(n, "%d x value <= 0 omitted from logarithmic plot", 
                "%d x values <= 0 omitted from logarithmic plot"), 
                n), domain = NA)
            x[ii] <- NA
        }
        if ("y" %in% log && any(ii <- y <= 0 & !is.na(y))) {
            n <- sum(ii)
            warning(sprintf(ngettext(n, "%d y value <= 0 omitted from logarithmic plot", 
                "%d y values <= 0 omitted from logarithmic plot"), 
                n), domain = NA)
            y[ii] <- NA
        }
        if ("z" %in% log && any(ii <- z <= 0 & !is.na(z))) {
            n <- sum(ii)
            warning(sprintf(ngettext(n, "%d z value <= 0 omitted from logarithmic plot", 
                "%d z values <= 0 omitted from logarithmic plot"), 
                n), domain = NA)
            z[ii] <- NA
        }
    }
    list(x = as.double(x), y = as.double(y), z = as.double(z), 
        xlab = xlab, ylab = ylab, zlab = zlab)
}


savePlot <- function (filename = paste("Rplot", type, sep = "."), type = c("png", 
    "jpeg", "tiff", "bmp"), device = dev.cur()) 
{
    type <- match.arg(type)
    devlist <- dev.list()
    devcur <- match(device, devlist, NA)
    if (is.na(devcur)) 
        stop("no such device")
    devname <- names(devlist)[devcur]
    if (devname != "X11cairo") 
        stop("can only copy from 'X11(type=\"*cairo\")' devices")
    invisible(.External2(C_savePlot, filename, type, device))
}


hcl <- function (h = 0, c = 35, l = 85, alpha = 1, fixup = TRUE) 
.Call(C_hcl, h, c, l, if (missing(alpha)) NULL else alpha, fixup)


postscriptFonts <- function (...) 
{
    initPSandPDFfonts()
    ndots <- length(fonts <- list(...))
    if (ndots == 0L) 
        get(".PostScript.Fonts", envir = .PSenv)
    else {
        fontNames <- names(fonts)
        nnames <- length(fontNames)
        if (nnames == 0L) {
            if (!all(sapply(fonts, is.character))) 
                stop(gettextf("invalid arguments in '%s' (must be font names)", 
                  "postscriptFonts"), domain = NA)
            else get(".PostScript.Fonts", envir = .PSenv)[unlist(fonts)]
        }
        else {
            if (ndots != nnames) 
                stop(gettextf("invalid arguments in '%s' (need named args)", 
                  "postscriptFonts"), domain = NA)
            setFonts(fonts, fontNames, ".PostScript.Fonts")
        }
    }
}


embedFonts <- function (file, format, outfile = file, fontpaths = character(), 
    options = character()) 
{
    if (!is.character(file) || length(file) != 1L || !nzchar(file)) 
        stop("'file' must be a non-empty character string")
    gsexe <- tools::find_gs_cmd()
    if (!nzchar(gsexe)) 
        stop("GhostScript was not found")
    if (.Platform$OS.type == "windows") 
        gsexe <- shortPathName(gsexe)
    suffix <- gsub(".+[.]", "", file)
    if (missing(format)) 
        format <- switch(suffix, ps = , eps = "ps2write", pdf = "pdfwrite")
    if (!is.character(format)) 
        stop("invalid output format")
    check_gs_type(gsexe, format)
    tmpfile <- tempfile("Rembed")
    if (length(fontpaths)) 
        fontpaths <- paste0("-sFONTPATH=", shQuote(paste(fontpaths, 
            collapse = .Platform$path.sep)))
    args <- c(paste0("-dNOPAUSE -dBATCH -q -dAutoRotatePages=/None -sDEVICE=", 
        format), paste0(" -sOutputFile=", tmpfile), fontpaths, 
        options, shQuote(file))
    ret <- system2(gsexe, args)
    if (ret != 0) 
        stop(gettextf("status %d in running command '%s'", ret, 
            cmd), domain = NA)
    if (outfile != file) 
        args[2] <- paste0(" -sOutputFile=", shQuote(outfile))
    cmd <- paste(c(shQuote(gsexe), args), collapse = " ")
    file.copy(tmpfile, outfile, overwrite = TRUE)
    invisible(cmd)
}


densCols <- function (x, y = NULL, nbin = 128, bandwidth, colramp = colorRampPalette(blues9[-(1:3)])) 
{
    xy <- xy.coords(x, y)
    select <- is.finite(xy$x) & is.finite(xy$y)
    x <- cbind(xy$x, xy$y)[select, ]
    map <- .smoothScatterCalcDensity(x, nbin, bandwidth)
    mkBreaks <- function(u) u - diff(range(u))/(length(u) - 1)/2
    xbin <- cut(x[, 1], mkBreaks(map$x1), labels = FALSE)
    ybin <- cut(x[, 2], mkBreaks(map$x2), labels = FALSE)
    dens <- map$fhat[cbind(xbin, ybin)]
    dens[is.na(dens)] <- 0
    colpal <- cut(dens, length(dens), labels = FALSE)
    cols <- rep(NA_character_, length(select))
    cols[select] <- colramp(length(dens))[colpal]
    cols
}


getGraphicsEventEnv <- function (which = dev.cur()) 
{
    which <- as.integer(which)
    stopifnot(length(which) == 1)
    .External2(C_getGraphicsEventEnv, which)
}


topo.colors <- function (n, alpha = 1) 
{
    if ((n <- as.integer(n[1L])) > 0) {
        j <- n%/%3
        k <- n%/%3
        i <- n - j - k
        c(if (i > 0) hsv(h = seq.int(from = 43/60, to = 31/60, 
            length.out = i), alpha = alpha), if (j > 0) hsv(h = seq.int(from = 23/60, 
            to = 11/60, length.out = j), alpha = alpha), if (k > 
            0) hsv(h = seq.int(from = 10/60, to = 6/60, length.out = k), 
            alpha = alpha, s = seq.int(from = 1, to = 0.3, length.out = k), 
            v = 1))
    }
    else character()
}


devAskNewPage <- function (ask = NULL) 
.External2(C_devAskNewPage, ask)


contourLines <- function (x = seq(0, 1, length.out = nrow(z)), y = seq(0, 1, 
    length.out = ncol(z)), z, nlevels = 10, levels = pretty(range(z, 
    na.rm = TRUE), nlevels)) 
{
    if (missing(z)) {
        if (!missing(x)) {
            if (is.list(x)) {
                z <- x$z
                y <- x$y
                x <- x$x
            }
            else {
                z <- x
                x <- seq.int(0, 1, length.out = nrow(z))
            }
        }
        else stop("no 'z' matrix specified")
    }
    else if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    if (any(diff(x) <= 0) || any(diff(y) <= 0)) 
        stop("increasing 'x' and 'y' values expected")
    if (!is.matrix(z) || nrow(z) <= 1 || ncol(z) <= 1) 
        stop("no proper 'z' matrix specified")
    if (1 * length(x) * length(y) != length(z)) 
        stop("dimensions of 'x', 'y' and 'z' do not match")
    invisible(.External2(C_contourLines, x, y, z, levels))
}


grSoftVersion <- function () 
{
    bm <- .Call(C_bmVersion)
    if (nzchar(bm[3L])) 
        bm[3L] <- strsplit(bm[3L], "\n")[[1L]][1L]
    c(cairo = cairoVersion(), bm)
}


boxplot.stats <- function (x, coef = 1.5, do.conf = TRUE, do.out = TRUE) 
{
    if (coef < 0) 
        stop("'coef' must not be negative")
    nna <- !is.na(x)
    n <- sum(nna)
    stats <- stats::fivenum(x, na.rm = TRUE)
    iqr <- diff(stats[c(2, 4)])
    if (coef == 0) 
        do.out <- FALSE
    else {
        out <- if (!is.na(iqr)) {
            x < (stats[2L] - coef * iqr) | x > (stats[4L] + coef * 
                iqr)
        }
        else !is.finite(x)
        if (any(out[nna], na.rm = TRUE)) 
            stats[c(1, 5)] <- range(x[!out], na.rm = TRUE)
    }
    conf <- if (do.conf) 
        stats[3L] + c(-1.58, 1.58) * iqr/sqrt(n)
    list(stats = stats, n = n, conf = conf, out = if (do.out) x[out & 
        nna] else numeric())
}




## Package Data

# none


## Package Info

.skeleton_package_title = "The R Graphics Devices and Support for Colours and Fonts"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF