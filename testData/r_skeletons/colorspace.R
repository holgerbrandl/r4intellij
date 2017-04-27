##
## Exported symobls in package `colorspace`
##

## Exported package methods

hex <- function (from, gamma = NULL, fixup = FALSE) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    coords <- as(from, "sRGB")@coords
    rval <- .Call("sRGB_to_RColor", coords, fixup, PACKAGE = "colorspace")
    if (!is.null(dnam <- attr(coords, "dimnames"))) 
        names(rval) <- dnam[[1L]]
    return(rval)
}


LAB <- function (L, A, B, names) 
{
    if (missing(L)) 
        return(new("LAB"))
    if (missing(names)) 
        names = dimnames(L)[[1]]
    coords = cbind(L, if (missing(A)) 
        NULL
    else A, if (missing(B)) 
        NULL
    else B)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("L", "A", "B"))
    new("LAB", coords = coords)
}


.__C__RGB <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("RGB", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


XYZ <- function (X, Y, Z, names) 
{
    if (missing(X)) 
        return(new("XYZ"))
    if (missing(names)) 
        names = dimnames(X)[[1]]
    coords = cbind(X, if (missing(Y)) 
        NULL
    else Y, if (missing(Z)) 
        NULL
    else Z)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("X", "Y", "Z"))
    new("XYZ", coords = coords)
}


.__C__HLS <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("HLS", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

coerce <- methods::coerce # re-exported from methods package

.__C__polarLUV <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("polarLUV", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


HSV <- function (H, S, V, names) 
{
    if (missing(H)) 
        return(new("HSV"))
    if (missing(names)) 
        names = dimnames(H)[[1]]
    coords = cbind(H, if (missing(S)) 
        NULL
    else S, if (missing(V)) 
        NULL
    else V)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("H", "S", "V"))
    new("HSV", coords = coords)
}


diverge_hsv <- function (n, h = c(240, 0), s = 1, v = 1, power = 1, gamma = NULL, 
    fixup = TRUE, alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    h <- rep(h, length.out = 2L)
    s <- s[1L]
    v <- v[1L]
    power <- power[1L]
    rval <- seq(-s, s, length = n)
    rval <- hex(as(HSV(H = ifelse(rval > 0, h[2L], h[1L]), S = abs(rval)^power, 
        V = v, ...), "RGB"), fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}


.__C__polarLAB <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("polarLAB", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


mixcolor <- function (alpha, color1, color2, where = class(color1)) 
{
    alpha = as.numeric(alpha)
    c1 = coords(as(color1, where))
    c2 = coords(as(color2, where))
    na = length(alpha)
    n1 = nrow(c1)
    n2 = nrow(c2)
    n = max(na, n1, n2)
    if (na < n) 
        alpha = rep(alpha, length = n)
    if (n1 < n) 
        c1 = c1[rep(1:n1, length = n), ]
    if (n2 < n) 
        c2 = c2[rep(1:n2, length = n), ]
    get(where)((1 - alpha) * c1 + alpha * c2)
}


hex2RGB <- function (x, gamma = FALSE) 
{
    x <- substr(x, 0, 7)
    rval <- if (gamma) 
        RGB(.Call("hex_to_RGB", x, gamma, PACKAGE = "colorspace"))
    else sRGB(.Call("hex_to_RGB", x, gamma, PACKAGE = "colorspace"))
    if (!is.null(nam <- names(x))) 
        attr(rval@coords, "dimnames")[[1L]] <- nam
    return(rval)
}


.__C__color <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("color", package = "colorspace")
    , package = "colorspace"
    , subclasses = structure(list(RGB = S4_object(), 
    sRGB = S4_object(), 
    XYZ = S4_object(), 
    LAB = S4_object(), 
    polarLAB = S4_object(), 
    HSV = S4_object(), 
    HLS = S4_object(), 
    LUV = S4_object(), 
    polarLUV = S4_object()), .Names = c("RGB", 
"sRGB", "XYZ", "LAB", "polarLAB", "HSV", "HLS", "LUV", "polarLUV"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


readRGB <- function (file, class = "RGB") 
{
    x = scan(file, what = list(R = 0, G = 0, B = 0, name = ""))
    as(RGB(R = x$R/255, G = x$G/255, B = x$B/255), Class = class)
}


`.__T__show:methods` <- "<environment>"

HLS <- function (H, L, S, names) 
{
    if (missing(H)) 
        return(new("HLS"))
    if (missing(names)) 
        names = dimnames(H)[[1]]
    coords = cbind(H, if (missing(L)) 
        NULL
    else L, if (missing(S)) 
        NULL
    else S)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("H", "L", "S"))
    new("HLS", coords = coords)
}


rainbow_hcl <- function (n, c = 50, l = 70, start = 0, end = 360 * (n - 1)/n, 
    gamma = NULL, fixup = TRUE, alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    rval <- hex(polarLUV(L = l, C = c, H = seq(start, end, length = n)), 
        fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}


plot <- function (x, y, ...) 
standardGeneric("plot")


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

`.__T__coords:colorspace` <- "<environment>"

desaturate <- function (col) 
{
    if (is.character(col) && (all(substr(col, 1L, 1L) == "#") & 
        all(nchar(col) %in% c(7L, 9L)))) {
        alpha <- substr(col, 8L, 9L)
        col <- substr(col, 1L, 7L)
        col <- hex2RGB(col)
    }
    else {
        col <- col2rgb(col, alpha = TRUE)
        alpha <- format(as.hexmode(col[4L, ]), width = 2L, upper.case = TRUE)
        alpha[alpha == "FF"] <- ""
        col <- RGB(t(col[1L:3L, ])/255)
    }
    col <- as(col, "polarLUV")
    col@coords[, 2L] <- 0
    col@coords[col@coords[, 1L] <= 0 | col@coords[, 1L] >= 100, 
        2L:3L] <- 0
    col <- hex(col)
    col <- paste(col, alpha, sep = "")
    return(col)
}


.__C__LUV <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("LUV", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__plot:graphics` <- "<environment>"

diverge_hcl <- function (n, h = c(260, 0), c = 80, l = c(30, 90), power = 1.5, 
    gamma = NULL, fixup = TRUE, alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- c[1L]
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, -1, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * abs(rval)^power[2L], 
        C = c * abs(rval)^power[1L], H = ifelse(rval > 0, h[1L], 
            h[2L])), fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}


terrain_hcl <- function (n, h = c(130, 0), c. = c(80, 0), l = c(60, 95), power = c(1/10, 
    1), gamma = NULL, fixup = TRUE, alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    rval <- heat_hcl(n, h = h, c. = c., l = l, power = power, 
        fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

choose_palette <- function (pal = diverge_hcl, n = 7L, parent = NULL, gui = "tcltk") 
{
    args <- list(pal = pal, n = n, parent = parent)
    gui <- match.arg(gui, c("tcltk", "shiny"))
    do.call(sprintf("choose_palette_%s", gui), args)
}


LUV <- function (L, U, V, names) 
{
    if (missing(L)) 
        return(new("LUV"))
    if (missing(names)) 
        names = dimnames(L)[[1]]
    coords = cbind(L, if (missing(U)) 
        NULL
    else U, if (missing(V)) 
        NULL
    else V)
    dimnames(coords) = list(names, c("L", "U", "V"))
    new("LUV", coords = coords)
}


readhex <- function (file = "", class = "RGB") 
as(hex2RGB(scan(file, what = "")), Class = class)


writehex <- function (x, file = "") 
{
    cat(hex(x), sep = "\n", file = file)
    file
}


sequential_hcl <- function (n, h = 260, c. = c(80, 0), l = c(30, 90), power = 1.5, 
    gamma = NULL, fixup = TRUE, alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    c <- rep(c., length.out = 2L)
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
        C = c[2L] - diff(c) * rval^power[1L], H = h[1L]), fixup = fixup, 
        ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}


`.__T__[:base` <- "<environment>"

.__C__HSV <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("HSV", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


sRGB <- function (R, G, B, names) 
{
    if (missing(R)) 
        return(new("sRGB"))
    if (missing(names)) 
        names = dimnames(R)[[1]]
    coords = cbind(R, if (missing(G)) 
        NULL
    else G, if (missing(B)) 
        NULL
    else B)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("R", "G", "B"))
    new("sRGB", coords = coords)
}


.__C__sRGB <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("sRGB", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


RGB <- function (R, G, B, names) 
{
    if (missing(R)) 
        return(new("RGB"))
    if (missing(names)) 
        names = dimnames(R)[[1]]
    coords = cbind(R, if (missing(G)) 
        NULL
    else G, if (missing(B)) 
        NULL
    else B)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("R", "G", "B"))
    new("RGB", coords = coords)
}


polarLAB <- function (L, C, H, names) 
{
    if (missing(L)) 
        return(new("polarLAB"))
    if (missing(names)) 
        names = dimnames(L)[[1]]
    coords = cbind(L, if (missing(C)) 
        NULL
    else C, if (missing(H)) 
        NULL
    else H)
    CheckMatrix(coords)
    dimnames(coords) = list(names, c("L", "C", "H"))
    new("polarLAB", coords = coords)
}


.__C__LAB <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("LAB", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__XYZ <- new("classRepresentation"
    , slots = structure(list(coords = structure("matrix", package = "methods")), .Names = "coords")
    , contains = structure(list(color = S4_object()), .Names = "color")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("XYZ", package = "colorspace")
    , package = "colorspace"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__coerce:methods` <- "<environment>"

coords <- function (color) 
standardGeneric("coords")


specplot <- function (x, rgb = TRUE, hcl = TRUE, fix = TRUE, cex = 1, type = "l", 
    lwd = 2 * cex, lty = 1, pch = NULL, legend = TRUE, palette = TRUE, 
    plot = TRUE) 
{
    x.na <- which(is.na(x))
    if (length(x.na) > 0) 
        x[x.na] <- "#ffffff"
    RGB <- hex2RGB(x)
    HCL <- as(RGB, "polarLUV")
    RGB <- coords(RGB)
    HCL <- coords(HCL)[, c("H", "C", "L")]
    HCL[which(is.na(HCL), arr.ind = TRUE)] <- 0
    if (length(x.na) > 0L) {
        for (i in 1:3) {
            HCL[x.na, i] <- NA
            RGB[x.na, i] <- NA
        }
    }
    if (fix & nrow(HCL) > 1L) {
        for (i in 2L:nrow(HCL)) {
            if (any(is.na(HCL[(i - 1L):i, ]))) 
                next
            d <- HCL[i, "H"] - HCL[i - 1L, "H"]
            if (abs(d) > 320) 
                HCL[i, "H"] <- HCL[i, "H"] - sign(d) * 360
            if (abs(HCL[i, "H"]) > 360) 
                HCL[1L:i, "H"] <- HCL[1L:i, "H"] - sign(HCL[i, 
                  "H"]) * 360
        }
        idx <- which(HCL[, "C"] < 8)
        if (length(idx) == nrow(HCL)) {
            HCL[, "H"] <- mean(HCL[, "H"])
        }
        else if (length(idx) > 0L) {
            n <- nrow(HCL)
            if (n >= 49L) {
                HCL[, "H"] <- 1/3 * (HCL[c(rep.int(1L, 2L), 1L:(n - 
                  2L)), "H"] + HCL[c(rep.int(1L, 1L), 1L:(n - 
                  1L)), "H"] + HCL[1L:n, "H"])
            }
            idxs <- split(idx, cumsum(c(1, diff(idx)) > 1))
            s <- 1L
            while (length(idxs) > 0L) {
                e <- if (s %in% idxs[[1L]]) {
                  if (length(idxs) > 1L) 
                    idxs[[2L]] - 1L
                  else n
                }
                else {
                  if (n %in% idxs[[1L]]) 
                    n
                  else round(mean(range(idxs[[1L]])))
                }
                io <- split(s:e, s:e %in% idx)
                if (length(io) == 2L & sum(!is.na(HCL[io[["FALSE"]], 
                  "H"])) > 0) {
                  HCL[io[["TRUE"]], "H"] <- stats::spline(io[["FALSE"]], 
                    HCL[io[["FALSE"]], "H"], xout = io[["TRUE"]], 
                    method = "natural")$y
                }
                idxs[[1L]] <- NULL
                s <- e + 1L
            }
        }
    }
    if (isTRUE(rgb)) 
        rgb <- hex(sRGB(c(0.8, 0, 0), c(0, 0.8, 0), c(0, 0, 0.8)))
    if (isTRUE(hcl)) 
        hcl <- rainbow_hcl(3L)
    show_rgb <- !identical(rgb, FALSE)
    show_hcl <- !identical(hcl, FALSE)
    if (plot & (show_rgb | show_hcl) & (length(x.na) == length(x))) {
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        par(xaxt = "n", yaxt = "n", bty = "n", mar = rep(0, 4))
        plot(0, type = "n", xlim = c(-1, 1), ylim = c(-1, 1))
        text(0, 0, "All colors NA\nCannot draw spectrum", col = 2)
    }
    else if (plot & (show_rgb | show_hcl)) {
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        nr <- show_rgb + palette + show_hcl
        layout(matrix(1L:nr, ncol = 1L, nrow = nr), heights = c(if (show_rgb) 10 else NULL, 
            if (palette) 2 else NULL, if (show_hcl) 10 else NULL))
        par(xaxt = "n", yaxs = "i", xaxs = "i", mar = c(0.2, 
            0, 0.2, 0), oma = c(2, 3, 2, 3), cex = cex)
        rgb <- rep(rgb, length.out = 3L)
        hcl <- rep(hcl, length.out = 3L)
        lwd <- rep(lwd, length.out = 3L)
        lty <- rep(lty, length.out = 3L)
        type <- rep(type, length.out = 3L)
        pch <- if (is.null(pch)) 
            ifelse(type == "l", NA, 1)
        else rep(pch, length.out = 3L)
        if (show_rgb) {
            plot(0, type = "n", ylim = c(0, 1), xlim = c(1, length(x)))
            lines(RGB[, "R"], lwd = lwd[1L], lty = lty[1L], col = rgb[1L], 
                type = type[1L], pch = pch[1L])
            lines(RGB[, "G"], lwd = lwd[2L], lty = lty[2L], col = rgb[2L], 
                type = type[1L], pch = pch[1L])
            lines(RGB[, "B"], lwd = lwd[3L], lty = lty[3L], col = rgb[3L], 
                type = type[1L], pch = pch[1L])
            if (legend) 
                legend("topleft", legend = c("Red", "Green", 
                  "Blue"), ncol = 3L, bty = "n", lwd = lwd, lty = lty, 
                  col = rgb, pch = pch)
            mtext(side = 3, "RGB Spectrum", cex = cex, line = 0.2)
            mtext(side = 2, "Red / Green / Blue", cex = cex, 
                line = 2)
        }
        if (palette) {
            par(xaxt = "n", yaxt = "n")
            image(matrix(seq_along(x), ncol = 1L), col = x)
            par(yaxt = "s")
        }
        if (show_hcl) {
            plot(0, type = "n", ylim = c(0, 100), xlim = c(1, 
                length(x)))
            if (min(HCL[, "H"], na.rm = TRUE) >= 0) {
                labels <- seq(0, 360, length.out = 5)
                axis(side = 4, at = labels/3.6, labels = labels)
                lines((HCL[, "H"])/3.6, lwd = lwd[1L], lty = lty[1L], 
                  col = hcl[1L], type = type[1L], pch = pch[1L])
            }
            else {
                labels <- seq(-360, 360, length.out = 5)
                axis(side = 4, at = labels/7.2 + 50, labels = labels)
                lines((HCL[, "H"] + 360)/7.2, lwd = lwd[1L], 
                  lty = lty[1L], col = hcl[1L], type = type[1L], 
                  pch = pch[1L])
            }
            lines(HCL[, "C"], lwd = lwd[2L], lty = lty[2L], col = hcl[2L], 
                type = type[1L], pch = pch[1L])
            lines(HCL[, "L"], lwd = lwd[3L], lty = lty[3L], col = hcl[3L], 
                type = type[1L], pch = pch[1L])
            legend("bottomleft", legend = c("Hue", "Chroma", 
                "Luminance"), ncol = 3L, bty = "n", lwd = lwd, 
                lty = lty, col = hcl, pch = pch)
            mtext(side = 1, "HCL Spectrum", cex = cex, line = 0.2)
            mtext(side = 2, "Chroma / Luminance", cex = cex, 
                line = 2)
            mtext(side = 4, "Hue", cex = cex, line = 2)
        }
    }
    if (length(x.na) > 0) 
        x[x.na] <- NA
    invisible(list(RGB = RGB, HCL = HCL, hex = x))
}


hclwizard <- function (n = 7L, gui = "shiny", shiny.trace = FALSE) 
{
    args <- list(n = n, shiny.trace = shiny.trace)
    gui <- match.arg(gui, c("tcltk", "shiny"))
    do.call(sprintf("choose_palette_%s", gui), args)
}


heat_hcl <- function (n, h = c(0, 90), c. = c(100, 30), l = c(50, 90), power = c(1/5, 
    1), gamma = NULL, fixup = TRUE, alpha = 1, ...) 
{
    if (!is.null(gamma)) 
        warning("'gamma' is deprecated and has no effect")
    if (n < 1L) 
        return(character(0L))
    h <- rep(h, length.out = 2L)
    c <- rep(c., length.out = 2L)
    l <- rep(l, length.out = 2L)
    power <- rep(power, length.out = 2L)
    rval <- seq(1, 0, length = n)
    rval <- hex(polarLUV(L = l[2L] - diff(l) * rval^power[2L], 
        C = c[2L] - diff(c) * rval^power[1L], H = h[2L] - diff(h) * 
            rval), fixup = fixup, ...)
    if (!missing(alpha)) {
        alpha <- pmax(pmin(alpha, 1), 0)
        alpha <- format(as.hexmode(round(alpha * 255 + 1e-04)), 
            width = 2L, upper.case = TRUE)
        rval <- paste(rval, alpha, sep = "")
    }
    return(rval)
}


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

polarLUV <- function (L, C, H, names) 
{
    if (missing(L)) 
        return(new("polarLUV"))
    if (missing(names)) 
        names = dimnames(L)[[1]]
    coords = cbind(L, if (missing(C)) 
        NULL
    else C, if (missing(H)) 
        NULL
    else H)
    dimnames(coords) = list(names, c("L", "C", "H"))
    new("polarLUV", coords = coords)
}


show <- methods::show # re-exported from methods package



## Package Data

USSouthPolygon <- colorspace::USSouthPolygon		## Polygon for County Map of US South States: Alabama, Georgia, and South Carolina



## Package Info

.skeleton_package_title = "Color Space Manipulation"

.skeleton_package_version = "1.3-2"

.skeleton_package_depends = "methods"

.skeleton_package_imports = "graphics,grDevices"


## Internal

.skeleton_version = 5


## EOF