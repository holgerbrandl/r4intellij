##
## Exported symobls in package `lattice`
##

## Exported package methods

trellis.currentLayout <- function (which = c("packet", "panel"), prefix = lattice.getStatus("current.prefix")) 
{
    which <- match.arg(which)
    switch(which, packet = lattice.getStatus("current.packet.positions", 
        prefix = prefix), panel = lattice.getStatus("current.panel.positions", 
        prefix = prefix))
}


prepanel.default.histogram <- function (x, breaks, equal.widths = TRUE, type = "density", nint = round(log2(length(x)) + 
    1), ...) 
{
    if (length(x) < 1) 
        prepanel.null()
    else {
        if (is.null(breaks)) {
            breaks <- if (is.factor(x)) 
                seq_len(1 + nlevels(x)) - 0.5
            else if (equal.widths) 
                do.breaks(range(x, finite = TRUE), nint)
            else quantile(x, 0:nint/nint, na.rm = TRUE)
        }
        h <- hist.constructor(x, breaks = breaks, ...)
        if (type != "density" && is.function(breaks)) {
            if (!isTRUE(all.equal(diff(range(diff(h$breaks))), 
                0))) 
                warning(gettextf("type='%s' can be misleading in this context", 
                  type))
        }
        y <- switch(type, count = h$counts, percent = 100 * h$counts/length(x), 
            density = h$density)
        list(xlim = if (is.factor(x)) levels(x) else scale.limits(c(x, 
            h$breaks)), ylim = range(0, y, finite = TRUE), dx = 1, 
            dy = 1)
    }
}


prepanel.spline <- function (x, y, npoints = 101, horizontal = FALSE, ..., keep.data = FALSE) 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) 
        return(prepanel.null())
    if (horizontal) {
        args <- list(x = y[ok], y = x[ok], ..., keep.data = keep.data)
        smooth <- checkArgsAndCall(smooth.spline, args)
        yy <- do.breaks(range(y[ok]), npoints)
        p <- predict(smooth, x = yy)
        list(xlim = range(x, p$y, finite = TRUE), ylim = range(y, 
            p$x, finite = TRUE), dx = diff(p$y), dy = diff(p$x))
    }
    else {
        args <- list(x = x[ok], y = y[ok], ..., keep.data = FALSE)
        smooth <- checkArgsAndCall(smooth.spline, args)
        xx <- do.breaks(range(x[ok]), npoints)
        p <- predict(smooth, x = xx)
        list(xlim = range(x, p$x, finite = TRUE), ylim = range(y, 
            p$y, finite = TRUE), dx = diff(p$x), dy = diff(p$y))
    }
}


trellis.panelArgs <- function (x, packet.number) 
{
    if (missing(x)) {
        x <- trellis.last.object()
        if (is.null(x)) 
            stop("Plot object was not saved, cannot retrieve panel data")
        if (lattice.getStatus("current.plot.multipage", prefix = lattice.getStatus("current.prefix"))) 
            warning("Plot spans multiple pages, only last page can be updated")
    }
    if (missing(packet.number)) {
        packet.number <- lattice::packet.number()
    }
    if (!length(packet.number)) 
        stop("You have to first select a panel using trellis.focus()")
    c(x$panel.args[[packet.number]], x$panel.args.common)
}


dotplot <- function (x, data, ...) 
UseMethod("dotplot")


xyplot <- function (x, data, ...) 
UseMethod("xyplot")


panel.contourplot <- function (...) 
panel.levelplot(...)


standard.theme <- function (name = .Device, color = name != "postscript") 
{
    if (color) {
        can.col <- if (name %in% c("windows", "X11")) 
            c("#000000", "#00ffff", "#ff00ff", "#00ff00", "#ff7f00", 
                "#007eff", "#ffff00", "#ff0000", "#c6ffff", "#ffc3ff", 
                "#c8ffc8", "#ffd18f", "#a9e2ff", "#ffffc3", "#ff8c8a", 
                "#aaaaaa", "#909090")
        else if (name %in% c("postscript", "pdf", "xfig")) 
            c("#000000", "#00ffff", "#ff00ff", "#00ff00", "#ff7f00", 
                "#0080ff", "#ffff00", "#ff0000", "#ccffff", "#ffccff", 
                "#ccffcc", "#ffe5cc", "#cce6ff", "#ffffcc", "#ffcccc", 
                "#e6e6e6", "transparent")
        else c("#000000", "#00FFFF", "#FF00FF", "#00FF00", "#FF7F00", 
            "#007EFF", "#FFFF00", "#FF0000", "#C6FFFF", "#FFC3FF", 
            "#C8FFC8", "#FFD18F", "#A9E2FF", "#FFFFC3", "#FF8C8A", 
            "#AAAAAA", "#909090")
    }
    else can.col <- c("#000000", "#999999", "#4C4C4C", "#E6E6E6", 
        "#F2F2F2", "#B2B2B2", "#000000", "#030303", "#050505", 
        "#080808", "#0A0A0A", "#0D0D0D", "#0F0F0F", "#121212", 
        "#151515", "#AAAAAA", "transparent")
    ans <- list(grid.pars = list(), fontsize = list(text = 12, 
        points = 8), background = list(alpha = 1, col = can.col[17]), 
        panel.background = list(col = "transparent"), clip = list(panel = "on", 
            strip = "on"), add.line = list(alpha = 1, col = can.col[1], 
            lty = 1, lwd = 1), add.text = list(alpha = 1, cex = 1, 
            col = can.col[1], font = 1, lineheight = 1.2), plot.polygon = list(alpha = 1, 
            col = can.col[2], border = "black", lty = 1, lwd = 1), 
        box.dot = list(alpha = 1, col = can.col[1], cex = 1, 
            font = 1, pch = 16), box.rectangle = list(alpha = 1, 
            col = can.col[2], fill = "transparent", lty = 1, 
            lwd = 1), box.umbrella = list(alpha = 1, col = can.col[2], 
            lty = 2, lwd = 1), dot.line = list(alpha = 1, col = can.col[16], 
            lty = 1, lwd = 1), dot.symbol = list(alpha = 1, cex = 0.8, 
            col = can.col[2], font = 1, pch = 16), plot.line = list(alpha = 1, 
            col = can.col[2], lty = 1, lwd = 1), plot.symbol = list(alpha = 1, 
            cex = 0.8, col = can.col[2], font = 1, pch = 1, fill = "transparent"), 
        reference.line = list(alpha = 1, col = can.col[16], lty = 1, 
            lwd = 1), strip.background = list(alpha = 1, col = can.col[c(12, 
            11, 9, 13, 10, 15, 14)]), strip.shingle = list(alpha = 1, 
            col = can.col[c(5, 4, 2, 6, 3, 8, 7)]), strip.border = list(alpha = 1, 
            col = rep(can.col[1], 7), lty = rep(1, 7), lwd = rep(1, 
                7)), superpose.line = list(alpha = 1, col = can.col[2:8], 
            lty = rep(1, 7), lwd = rep(1, 7)), superpose.symbol = list(alpha = rep(1, 
            7), cex = rep(0.8, 7), col = can.col[2:8], fill = lower.saturation(can.col[2:8]), 
            font = rep(1, 7), pch = rep(1, 7)), superpose.polygon = list(alpha = rep(1, 
            7), col = lower.saturation(can.col[2:8]), border = rep("black", 
            7), lty = rep(1, 7), lwd = rep(1, 7)), regions = list(alpha = 1, 
            col = rev(cm.colors(100))), shade.colors = list(alpha = 1, 
            palette = function(irr, ref, height, saturation = 0.9) {
                hsv(h = height, s = 1 - saturation * (1 - (1 - 
                  ref)^0.5), v = irr)
            }), axis.line = list(alpha = 1, col = can.col[1], 
            lty = 1, lwd = 1), axis.text = list(alpha = 1, cex = 0.8, 
            col = can.col[1], font = 1, lineheight = 1), axis.components = list(left = list(tck = 1, 
            pad1 = 1, pad2 = 1), top = list(tck = 1, pad1 = 1, 
            pad2 = 1), right = list(tck = 1, pad1 = 1, pad2 = 1), 
            bottom = list(tck = 1, pad1 = 1, pad2 = 1)), layout.heights = list(top.padding = 1, 
            main = 1, main.key.padding = 1, key.top = 1, xlab.top = 1, 
            key.axis.padding = 1, axis.top = 1, strip = 1, panel = 1, 
            axis.panel = 1, between = 1, axis.bottom = 1, axis.xlab.padding = 1, 
            xlab = 1, xlab.key.padding = 0, key.bottom = 1, key.sub.padding = 1, 
            sub = 1, bottom.padding = 1), layout.widths = list(left.padding = 1, 
            key.left = 1, key.ylab.padding = 0, ylab = 1, ylab.axis.padding = 1, 
            axis.left = 1, axis.panel = 1, strip.left = 1, panel = 1, 
            between = 1, axis.right = 1, axis.key.padding = 1, 
            ylab.right = 1, key.right = 1, right.padding = 1), 
        box.3d = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1), 
        par.xlab.text = list(alpha = 1, cex = 1, col = can.col[1], 
            font = 1, lineheight = 1), par.ylab.text = list(alpha = 1, 
            cex = 1, col = can.col[1], font = 1, lineheight = 1), 
        par.zlab.text = list(alpha = 1, cex = 1, col = can.col[1], 
            font = 1, lineheight = 1), par.main.text = list(alpha = 1, 
            cex = 1.2, col = can.col[1], font = 2, lineheight = 1), 
        par.sub.text = list(alpha = 1, cex = 1, col = can.col[1], 
            font = 2, lineheight = 1))
    if (color) {
        if (name == "postscript" || name == "pdf") {
            ans$plot.symbol$col <- can.col[6]
            ans$plot.line$col <- can.col[6]
            ans$dot.symbol$col <- can.col[6]
            ans$box.rectangle$col <- can.col[6]
            ans$box.umbrella$col <- can.col[6]
            ans$superpose.symbol$col <- c(can.col[c(6, 3, 4, 
                8)], "orange", "darkgreen", "brown")
            ans$superpose.symbol$col[c(3, 6)] <- ans$superpose.symbol$col[c(6, 
                3)]
            ans$superpose.line$col <- ans$superpose.symbol$col
        }
    }
    else {
        ans$plot.polygon$col <- can.col[5]
        ans$box.rectangle$col <- can.col[1]
        ans$box.umbrella$col <- can.col[1]
        ans$dot.line$col <- can.col[4]
        ans$dot.symbol$col <- can.col[1]
        ans$plot.line$col <- can.col[1]
        ans$plot.symbol$col <- can.col[1]
        ans$regions$col <- grey(seq(0.3^2.2, 0.9^2.2, length.out = 100)^(1/2.2))
        ans$shade.colors$palette <- function(irr, ref, height, 
            w = 0.5) grey(w * irr + (1 - w) * (1 - (1 - ref)^0.4))
        ans$strip.background$col <- can.col[rep(5, 7)]
        ans$strip.shingle$col <- can.col[rep(6, 7)]
        ans$superpose.line$col <- can.col[rep(1, 7)]
        ans$superpose.line$lty <- 1:7
        ans$superpose.symbol$col <- can.col[rep(1, 7)]
        ans$superpose.symbol$cex <- rep(0.7, 7)
        ans$superpose.symbol$pch <- c(1, 3, 6, 0, 5, 16, 17)
        ans$superpose.polygon$col <- grey((c(6, 12, 7, 11, 8, 
            10, 9)/15)^0.8)
    }
    ans
}


panel.mathdensity <- function (dmath = dnorm, args = list(mean = 0, sd = 1), n = 50, 
    col, col.line = reference.line$col, lwd = reference.line$lwd, 
    lty = reference.line$lty, type, ..., identifier = "mathdensity") 
{
    reference.line <- trellis.par.get("reference.line")
    if (!missing(col) && missing(col.line)) 
        col.line <- col
    x <- do.breaks(endpoints = current.panel.limits()$xlim, nint = n)
    y <- do.call("dmath", c(list(x = x), args))
    panel.lines(x = x, y = y, col = col.line, lwd = lwd, lty = lty, 
        ..., identifier = identifier)
}


panel.3dwire <- function (x, y, z, rot.mat = diag(4), distance, shade = FALSE, 
    shade.colors.palette = trellis.par.get("shade.colors")$palette, 
    light.source = c(0, 0, 1000), xlim, ylim, zlim, xlim.scaled, 
    ylim.scaled, zlim.scaled, col = if (shade) "transparent" else "black", 
    lty = 1, lwd = 1, alpha, col.groups = superpose.polygon$col, 
    polynum = 100, ..., .scale = FALSE, drape = FALSE, at, col.regions = regions$col, 
    alpha.regions = regions$alpha, identifier = "3dwire") 
{
    if (.scale) {
        x[] <- xlim.scaled[1] + diff(xlim.scaled) * (x - xlim[1])/diff(xlim)
        y[] <- ylim.scaled[1] + diff(ylim.scaled) * (y - ylim[1])/diff(ylim)
        z[] <- zlim.scaled[1] + diff(zlim.scaled) * (z - zlim[1])/diff(zlim)
    }
    if (shade) 
        drape <- FALSE
    regions <- if (drape) 
        trellis.par.get("regions")
    else {
        bg <- trellis.par.get("background")
        if (bg[["col"]] == "transparent") 
            bg[["col"]] <- "white"
        bg
    }
    numcol <- length(at) - 1
    numcol.r <- length(col.regions)
    col.regions <- if (numcol.r <= numcol) 
        rep(col.regions, length.out = numcol)
    else col.regions[floor(1 + (1:numcol - 1) * (numcol.r - 1)/(numcol - 
        1))]
    isParametrizedSurface <- is.matrix(x) && is.matrix(y) && 
        is.matrix(z)
    if (isParametrizedSurface) {
        is.na(x) <- (x < xlim.scaled[1] | x > xlim.scaled[2])
        is.na(y) <- (y < ylim.scaled[1] | y > ylim.scaled[2])
        is.na(z) <- (z < zlim.scaled[1] | z > zlim.scaled[2])
        htrange <- extend.limits(sqrt(range(x^2 + y^2 + z^2, 
            finite = TRUE)), prop = 0.01)
        ngroups <- 1
    }
    else {
        ngroups <- if (is.matrix(z)) 
            ncol(z)
        else 1
        superpose.polygon <- trellis.par.get("superpose.polygon")
        col.groups <- rep(col.groups, length.out = ngroups)
        if (length(col) > 1) 
            col <- rep(col, length.out = ngroups)
        id.x <- x >= xlim.scaled[1] & x <= xlim.scaled[2]
        id.y <- y >= ylim.scaled[1] & y <= ylim.scaled[2]
        id.z <- rep(id.y, length(id.x)) & rep(id.x, each = length(id.y))
        x <- x[id.x]
        y <- y[id.y]
        z <- z[id.z]
        htrange <- zlim.scaled
    }
    if (shade) {
        shade.colors.palette <- getFunctionOrName(shade.colors.palette)
        pol.x <- numeric(polynum * 3)
        pol.y <- numeric(polynum * 3)
        pol.fill <- character(polynum)
        pol.col <- col
        pol.alpha <- trellis.par.get("shade.colors")$alpha
        if (!missing(alpha)) 
            pol.alpha <- alpha
        count <- 0
        gridpolycount <- 0
        wirePolygon <- function(xx, yy, misc) {
            height <- (misc[3] - htrange[1])/diff(htrange)
            invalid <- (is.na(height) || any(is.na(xx)) || any(is.na(yy)) || 
                height > 1 || height < 0)
            if (!invalid) {
                pol.x[3 * count + 1:3] <<- xx
                pol.y[3 * count + 1:3] <<- yy
                count <<- count + 1
                pol.fill[count] <<- shade.colors.palette(misc[1], 
                  misc[2], height)
                if (count == polynum) {
                  gridpolycount <<- gridpolycount + 1
                  grid.polygon(x = pol.x, y = pol.y, id.lengths = rep(3, 
                    polynum), default.units = "native", name = trellis.grobname(paste(identifier, 
                    "polygons", gridpolycount, sep = "."), type = "panel"), 
                    gp = gpar(fill = pol.fill, col = pol.col, 
                      lty = lty, lwd = lwd, alpha = pol.alpha))
                  count <<- 0
                }
            }
        }
        .Call(wireframePanelCalculations, as.double(x), as.double(y), 
            as.double(z), as.double(rot.mat), as.double(distance), 
            if (isParametrizedSurface) as.integer(ncol(x)) else as.integer(length(x)), 
            if (isParametrizedSurface) as.integer(nrow(x)) else as.integer(length(y)), 
            as.integer(ngroups), as.double(light.source), environment(), 
            as.integer(shade), as.integer(isParametrizedSurface))
        if (count > 0) {
            grid.polygon(x = pol.x[1:(count * 3)], y = pol.y[1:(count * 
                3)], default.units = "native", id.lengths = rep(3, 
                count), name = trellis.grobname(paste(identifier, 
                "polygons", gridpolycount + 1, sep = "."), type = "panel"), 
                gp = gpar(fill = rep(pol.fill, length.out = count), 
                  col = rep(pol.col, length.out = count), lty = lty, 
                  lwd = lwd, alpha = pol.alpha))
        }
    }
    else {
        pol.x <- numeric(polynum * 4)
        pol.y <- numeric(polynum * 4)
        if (length(col.regions) > 1) {
            pol.fill <- vector(mode(col.regions), polynum)
            pol.col <- if (ngroups == 1 || length(col) == 1) 
                col[1]
            else vector(mode(col), polynum)
            pol.alpha <- alpha.regions
        }
        else if (ngroups == 1) {
            pol.fill <- col.regions[1]
            pol.col <- col[1]
            pol.alpha <- alpha.regions
        }
        else {
            pol.fill <- vector(mode(col.groups), polynum)
            pol.col <- if (length(col) == 1) 
                col[1]
            else vector(mode(col), polynum)
            pol.alpha <- superpose.polygon$alpha
        }
        count <- 0
        gridpolycount <- 0
        wirePolygon <- function(xx, yy, misc) {
            height <- (misc[3] - htrange[1])/diff(htrange)
            invalid <- (is.na(height) || any(is.na(xx)) || any(is.na(yy)) || 
                height > 1 || height < 0)
            if (!invalid) {
                pol.x[4 * count + 1:4] <<- xx
                pol.y[4 * count + 1:4] <<- yy
                count <<- count + 1
                if (length(col.regions) > 1) {
                  pol.fill[count] <<- col.regions[(seq_along(at)[at > 
                    misc[3]])[1] - 1]
                  if (ngroups > 1 && length(col) > 1) 
                    pol.col[count] <<- col[as.integer(misc[4])]
                }
                else if (ngroups > 1) {
                  pol.fill[count] <<- col.groups[as.integer(as.integer(misc[4]))]
                  if (length(col) > 1) 
                    pol.col[count] <<- col[as.integer(misc[4])]
                }
                if (count == polynum) {
                  gridpolycount <<- gridpolycount + 1
                  grid.polygon(x = pol.x, y = pol.y, id.lengths = rep(4, 
                    polynum), default.units = "native", name = trellis.grobname(paste(identifier, 
                    "polygons", gridpolycount, sep = "."), type = "panel"), 
                    gp = gpar(fill = pol.fill, col = pol.col, 
                      lty = lty, lwd = lwd, alpha = pol.alpha))
                  count <<- 0
                }
            }
        }
        .Call(wireframePanelCalculations, as.double(x), as.double(y), 
            as.double(z), as.double(rot.mat), as.double(distance), 
            if (isParametrizedSurface) as.integer(ncol(x)) else as.integer(length(x)), 
            if (isParametrizedSurface) as.integer(nrow(x)) else as.integer(length(y)), 
            as.integer(ngroups), as.double(light.source), environment(), 
            as.integer(shade), as.integer(isParametrizedSurface))
        if (count > 0) {
            grid.polygon(x = pol.x[1:(count * 4)], y = pol.y[1:(count * 
                4)], default.units = "native", id.lengths = rep(4, 
                count), name = trellis.grobname(paste(identifier, 
                "polygons", gridpolycount + 1, sep = "."), type = "panel"), 
                gp = gpar(fill = rep(pol.fill, length.out = count), 
                  col = rep(pol.col, length.out = count), lty = lty, 
                  lwd = lwd, alpha = pol.alpha))
        }
    }
}


wireframe <- function (x, data, ...) 
UseMethod("wireframe")


panel.superpose.2 <- function (..., distribute.type = TRUE) 
{
    panel.superpose(..., distribute.type = distribute.type)
}


panel.3dscatter <- function (x, y, z, rot.mat = diag(4), distance, groups = NULL, 
    type = "p", xlim, ylim, zlim, xlim.scaled, ylim.scaled, zlim.scaled, 
    zero.scaled, col, col.point = if (is.null(groups)) plot.symbol$col else superpose.symbol$col, 
    col.line = if (is.null(groups)) plot.line$col else superpose.line$col, 
    lty = if (is.null(groups)) plot.line$lty else superpose.line$lty, 
    lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd, 
    cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex, 
    pch = if (is.null(groups)) "+" else superpose.symbol$pch, 
    cross, ..., .scale = FALSE, subscripts = TRUE, identifier = "3dscatter") 
{
    if (.scale) {
        x <- xlim.scaled[1] + diff(xlim.scaled) * (x - xlim[1])/diff(xlim)
        y <- ylim.scaled[1] + diff(ylim.scaled) * (y - ylim[1])/diff(ylim)
        z <- zlim.scaled[1] + diff(zlim.scaled) * (z - zlim[1])/diff(zlim)
    }
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    if (!missing(col)) {
        col.point <- col
        col.line <- col
    }
    n <- length(x)
    if (n > 0) {
        if (is.null(groups)) {
            col.point <- rep(col.point, length.out = n)
            col.line <- rep(col.line, length.out = n)
            lty <- rep(lty, length.out = n)
            lwd <- rep(lwd, length.out = n)
            cex <- rep(cex, length.out = n)
            pch <- rep(pch, length.out = n)
        }
        else {
            nvals <- nlevels(as.factor(groups))
            groups <- as.numeric(as.factor(groups))[subscripts]
            col.point <- rep(col.point, length.out = nvals)[groups]
            col.line <- rep(col.line, length.out = nvals)[groups]
            lty <- rep(lty, length.out = nvals)[groups]
            lwd <- rep(lwd, length.out = nvals)[groups]
            cex <- rep(cex, length.out = nvals)[groups]
            pch <- rep(pch, length.out = nvals)[groups]
        }
        if (any(c("p", "b", "o") %in% type)) {
            id <- ((x >= xlim.scaled[1]) & (x <= xlim.scaled[2]) & 
                (y >= ylim.scaled[1]) & (y <= ylim.scaled[2]) & 
                (z >= zlim.scaled[1]) & (z <= zlim.scaled[2]) & 
                !is.na(x) & !is.na(y) & !is.na(z))
            m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
            ord <- sort.list(m[3, ])
            ord <- ord[id[ord]]
            if (missing(cross)) 
                cross <- all(pch == "+")
            if (cross) {
                tmpx0 <- rep(x[ord], each = 3) - rep(cex[ord], 
                  each = 3) * c(0.02, 0, 0)
                tmpx1 <- rep(x[ord], each = 3) + rep(cex[ord], 
                  each = 3) * c(0.02, 0, 0)
                tmpy0 <- rep(y[ord], each = 3) - rep(cex[ord], 
                  each = 3) * c(0, 0.02, 0)
                tmpy1 <- rep(y[ord], each = 3) + rep(cex[ord], 
                  each = 3) * c(0, 0.02, 0)
                tmpz0 <- rep(z[ord], each = 3) - rep(cex[ord], 
                  each = 3) * c(0, 0, 0.02)
                tmpz1 <- rep(z[ord], each = 3) + rep(cex[ord], 
                  each = 3) * c(0, 0, 0.02)
                m0 <- ltransform3dto3d(rbind(tmpx0, tmpy0, tmpz0), 
                  rot.mat, distance)
                m1 <- ltransform3dto3d(rbind(tmpx1, tmpy1, tmpz1), 
                  rot.mat, distance)
                lsegments(x0 = m0[1, ], y0 = m0[2, ], x1 = m1[1, 
                  ], y1 = m1[2, ], col = rep(col.line[ord], each = 3), 
                  ..., identifier = paste(identifier, "points", 
                    sep = "."))
            }
            else {
                lpoints(x = m[1, ord], y = m[2, ord], col = col.point[ord], 
                  pch = pch[ord], cex = cex[ord], ..., identifier = identifier)
            }
        }
        if (any(c("l", "b", "o") %in% type)) {
            ord <- if (is.null(groups)) 
                TRUE
            else sort.list(groups)
            tmplen <- length(x)
            tmpx0 <- x[ord][-1]
            tmpx1 <- x[ord][-tmplen]
            tmpy0 <- y[ord][-1]
            tmpy1 <- y[ord][-tmplen]
            tmpz0 <- z[ord][-1]
            tmpz1 <- z[ord][-tmplen]
            tmpcol0 <- col.line[ord][-1]
            tmpcol0[groups[ord][-1] != groups[ord][-tmplen]] <- "transparent"
            m0 <- ltransform3dto3d(rbind(tmpx0, tmpy0, tmpz0), 
                rot.mat, distance)
            m1 <- ltransform3dto3d(rbind(tmpx1, tmpy1, tmpz1), 
                rot.mat, distance)
            ord <- sort.list(pmax(m0[3, ], m1[3, ]))
            lsegments(x0 = m0[1, ord], y0 = m0[2, ord], x1 = m1[1, 
                ord], y1 = m1[2, ord], col = tmpcol0[ord], lwd = lwd[ord], 
                lty = lty[ord], ..., identifier = paste(identifier, 
                  "lines", sep = "."))
        }
        if ("h" %in% type) {
            id <- ((x >= xlim.scaled[1]) & (x <= xlim.scaled[2]) & 
                (y >= ylim.scaled[1]) & (y <= ylim.scaled[2]) & 
                !is.na(x) & !is.na(y) & !is.na(z))
            m <- ltransform3dto3d(rbind(x, y, z), rot.mat, distance)
            ord <- sort.list(m[3, ])
            ord <- ord[id[ord]]
            zero.scaled <- if (zero.scaled < zlim.scaled[1]) 
                zlim.scaled[1]
            else if (zero.scaled > zlim.scaled[2]) 
                zlim.scaled[2]
            else zero.scaled
            other.end <- ltransform3dto3d(rbind(x, y, zero.scaled), 
                rot.mat, distance)
            lsegments(m[1, ord], m[2, ord], other.end[1, ord], 
                other.end[2, ord], col = col.line[ord], lty = lty[ord], 
                lwd = lwd[ord], ..., identifier = paste(identifier, 
                  "hist", sep = "."))
        }
        if (any(!(type %in% c("p", "h", "l", "b", "o")))) {
            warning("'type' has unsupported values")
        }
    }
}


panel.loess <- function (x, y, span = 2/3, degree = 1, family = c("symmetric", 
    "gaussian"), evaluation = 50, lwd = plot.line$lwd, lty = plot.line$lty, 
    col, col.line = plot.line$col, type, horizontal = FALSE, 
    ..., identifier = "loess") 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) 
        return()
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal) {
        smooth <- loess.smooth(y[ok], x[ok], span = span, family = family, 
            degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$y, y = smooth$x, col = col.line, 
            lty = lty, lwd = lwd, ..., identifier = identifier)
    }
    else {
        smooth <- loess.smooth(x[ok], y[ok], span = span, family = family, 
            degree = degree, evaluation = evaluation)
        panel.lines(x = smooth$x, y = smooth$y, col = col.line, 
            lty = lty, lwd = lwd, ..., identifier = identifier)
    }
    smooth
}


prepanel.loess <- function (x, y, span = 2/3, degree = 1, family = c("symmetric", 
    "gaussian"), evaluation = 50, horizontal = FALSE, ...) 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) 
        return(prepanel.null())
    if (horizontal) {
        smooth <- loess.smooth(y[ok], x[ok], span = span, family = family, 
            degree = degree, evaluation = evaluation)
        list(xlim = range(x, smooth$y, finite = TRUE), ylim = range(y, 
            smooth$x, finite = TRUE), dx = diff(smooth$y), dy = diff(smooth$x))
    }
    else {
        smooth <- loess.smooth(x[ok], y[ok], span = span, family = family, 
            degree = degree, evaluation = evaluation)
        list(xlim = range(x, smooth$x, finite = TRUE), ylim = range(y, 
            smooth$y, finite = TRUE), dx = diff(smooth$x), dy = diff(smooth$y))
    }
}


panel.identify.qqmath <- function (x = panel.args$x, distribution = panel.args$distribution, 
    groups = panel.args$groups, subscripts = panel.args$subscripts, 
    labels = subscripts, panel.args = trellis.panelArgs(), ...) 
{
    x <- as.numeric(x)
    if (is.null(subscripts)) 
        subscripts <- seq_along(x)
    labels <- as.character(labels)
    if (length(labels) > length(subscripts)) 
        labels <- labels[subscripts]
    if (!is.null(panel.args$f.value)) 
        warning("'f.value' not supported; ignoring")
    distribution <- getFunctionOrName(distribution)
    getq <- function(x) {
        ans <- x
        id <- !is.na(x)
        ord <- order(x[id])
        if (any(id)) 
            ans[id] <- distribution(ppoints(sum(id)))[order(ord)]
        ans
    }
    if (is.null(groups)) {
        panel.identify(x = getq(x), y = x, labels = labels, ...)
    }
    else {
        allq <- rep(NA_real_, length(x))
        subg <- groups[subscripts]
        vals <- if (is.factor(groups)) 
            levels(groups)
        else sort(unique(groups))
        for (i in seq_along(vals)) {
            ok <- !is.na(subg) & (subg == vals[i])
            allq[ok] <- getq(x[ok])
        }
        panel.identify(x = allq, y = x, labels = labels, ...)
    }
}


barchart <- function (x, data, ...) 
UseMethod("barchart")


cloud <- function (x, data, ...) 
UseMethod("cloud")


panel.levelplot <- function (x, y, z, subscripts, at = pretty(z), shrink, labels = FALSE, 
    label.style = c("mixed", "flat", "align"), contour = FALSE, 
    region = TRUE, col = add.line$col, lty = add.line$lty, lwd = add.line$lwd, 
    border = "transparent", border.lty = 1, border.lwd = 0.1, 
    ..., col.regions = regions$col, alpha.regions = regions$alpha, 
    identifier = "levelplot") 
{
    if (length(subscripts) == 0) 
        return()
    regions <- trellis.par.get("regions")
    label.style <- match.arg(label.style)
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)
    zcol <- if (region) 
        level.colors(z, at, col.regions, colors = TRUE)
    else "transparent"
    x <- x[subscripts]
    y <- y[subscripts]
    minXwid <- if (length(unique(x)) > 1) 
        min(diff(sort(unique(x))))
    else 1
    minYwid <- if (length(unique(y)) > 1) 
        min(diff(sort(unique(y))))
    else 1
    fullZrange <- range(as.numeric(z), finite = TRUE)
    z <- z[subscripts]
    if (region) 
        zcol <- zcol[subscripts]
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    shrinkx <- c(1, 1)
    shrinky <- c(1, 1)
    if (!missing(shrink)) {
        if (is.numeric(shrink)) {
            shrinkx <- rep(shrink, length.out = 2)
            shrinky <- rep(shrink, length.out = 2)
        }
        else if (is.list(shrink)) {
            shrinkx <- rep(shrink[[1]], length.out = 2)
            shrinky <- rep(shrink[[1]], length.out = 2)
            if ("x" %in% names(shrink)) 
                shrinkx <- rep(shrink$x, length.out = 2)
            if ("y" %in% names(shrink)) 
                shrinky <- rep(shrink$y, length.out = 2)
        }
        else warning("Invalid 'shrink' parameter ignored")
    }
    scaleWidth <- function(z, min = 0.8, max = 0.8, zl = range(z, 
        finite = TRUE)) {
        if (diff(zl) == 0) 
            rep(0.5 * (min + max), length(z))
        else min + (max - min) * (z - zl[1])/diff(zl)
    }
    if (x.is.factor) {
        ux <- sort(unique(x[!is.na(x)]))
        lx <- rep(1, length(ux))
        cx <- ux
    }
    else {
        ux <- sort(unique(x[!is.na(x)]))
        bx <- if (length(ux) > 1) 
            c(3 * ux[1] - ux[2], ux[-length(ux)] + ux[-1], 3 * 
                ux[length(ux)] - ux[length(ux) - 1])/2
        else ux + c(-0.5, 0.5) * minXwid
        lx <- diff(bx)
        cx <- (bx[-1] + bx[-length(bx)])/2
    }
    if (y.is.factor) {
        uy <- sort(unique(y[!is.na(y)]))
        ly <- rep(1, length(uy))
        cy <- uy
    }
    else {
        uy <- sort(unique(y[!is.na(y)]))
        by <- if (length(uy) > 1) 
            c(3 * uy[1] - uy[2], uy[-length(uy)] + uy[-1], 3 * 
                uy[length(uy)] - uy[length(uy) - 1])/2
        else uy + c(-0.5, 0.5) * minYwid
        ly <- diff(by)
        cy <- (by[-1] + by[-length(by)])/2
    }
    idx <- match(x, ux)
    idy <- match(y, uy)
    if (region) 
        grid.rect(x = cx[idx], y = cy[idy], width = lx[idx] * 
            scaleWidth(z, shrinkx[1], shrinkx[2], fullZrange), 
            height = ly[idy] * scaleWidth(z, shrinky[1], shrinky[2], 
                fullZrange), default.units = "native", name = trellis.grobname(paste(identifier, 
                "rect", sep = "."), type = "panel", group = group), 
            gp = gpar(fill = zcol, col = border, lwd = border.lwd, 
                lty = border.lty, alpha = alpha.regions))
    if (contour) {
        cpl <- current.panel.limits(unit = "cm")
        asp <- diff(cpl$ylim)/diff(cpl$xlim)
        if (is.logical(labels) && !labels) 
            labels <- NULL
        else {
            if (is.characterOrExpression(labels)) 
                labels <- list(labels = labels)
            text <- trellis.par.get("add.text")
            tmp <- list(col = text$col, alpha = text$alpha, cex = text$cex, 
                fontfamily = text$fontfamily, fontface = text$fontface, 
                font = text$font)
            labels <- if (is.list(labels)) 
                updateList(tmp, labels)
            else tmp
            if (!is.characterOrExpression(labels$labels)) 
                labels$labels <- format(at, trim = TRUE)
        }
        add.line <- trellis.par.get("add.line")
        m <- matrix(NA_real_, nrow = length(ux), ncol = length(uy))
        m[(idy - 1) * length(ux) + idx] <- z
        clines <- contourLines(x = ux, y = uy, z = m, nlevels = length(at), 
            levels = at)
        ccount <- 0
        for (val in clines) {
            ccount <- ccount + 1
            llines(val, col = col, lty = lty, lwd = lwd, identifier = paste(identifier, 
                "line", ccount, sep = "."))
            if (length(val$x) > 5) {
                if (!is.null(labels)) {
                  slopes <- diff(val$y)/diff(val$x)
                  if (label.style == "flat") {
                    textloc <- which.min(abs(slopes))
                    rotangle <- 0
                  }
                  else if (label.style == "align") {
                    rx <- range(ux)
                    ry <- range(uy)
                    depth <- pmin(pmin(val$x - rx[1], rx[2] - 
                      val$x)/diff(rx), pmin(val$y - ry[1], ry[2] - 
                      val$y)/diff(ry))
                    textloc <- min(which.max(depth), length(slopes))
                    rotangle <- atan(asp * slopes[textloc] * 
                      diff(rx)/diff(ry)) * 180/base::pi
                  }
                  else if (label.style == "mixed") {
                    rx <- range(ux)
                    ry <- range(uy)
                    depth <- pmin(pmin(val$x - rx[1], rx[2] - 
                      val$x)/diff(rx), pmin(val$y - ry[1], ry[2] - 
                      val$y)/diff(ry))
                    textloc <- which.min(abs(slopes))
                    rotangle <- 0
                    if (depth[textloc] < 0.05) {
                      textloc <- min(which.max(depth), length(slopes))
                      rotangle <- atan(asp * slopes[textloc] * 
                        diff(rx)/diff(ry)) * 180/base::pi
                    }
                  }
                  else stop("Invalid label.style")
                  i <- match(val$level, at)
                  ltext(labels$labels[i], adj = c(0.5, 0), srt = rotangle, 
                    col = labels$col, alpha = labels$alpha, cex = labels$cex, 
                    font = labels$font, fontfamily = labels$fontfamily, 
                    fontface = labels$fontface, x = 0.5 * (val$x[textloc] + 
                      val$x[textloc + 1]), y = 0.5 * (val$y[textloc] + 
                      val$y[textloc + 1]), identifier = paste(identifier, 
                      "label", ccount, sep = "."))
                }
            }
        }
    }
}


panel.qqmathline <- function (x, y = x, distribution = qnorm, probs = c(0.25, 0.75), 
    qtype = 7, groups = NULL, ..., identifier = "qqmathline") 
{
    y <- as.numeric(y)
    stopifnot(length(probs) == 2)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(y))
    if (!is.null(groups)) 
        panel.superpose(x = y, y = NULL, distribution = distribution, 
            probs = probs, qtype = qtype, groups = groups, panel.groups = panel.qqmathline, 
            ...)
    else if (nobs > 0) {
        yy <- quantile(y, probs, names = FALSE, type = qtype, 
            na.rm = TRUE)
        xx <- distribution(probs)
        r <- diff(yy)/diff(xx)
        panel.abline(c(yy[1] - xx[1] * r, r), ..., identifier = identifier)
    }
}


panel.number <- function (prefix = lattice.getStatus("current.prefix")) 
{
    trellis.currentLayout("panel", prefix = prefix)[current.row(prefix = prefix), 
        current.column(prefix = prefix)]
}


equal.count <- function (x, ...) 
{
    attr(x, "levels") <- make.list.from.intervals(co.intervals(x, 
        ...))
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}


panel.wireframe <- function (...) 
panel.cloud(..., wireframe = TRUE)


shingle <- function (x, intervals = sort(unique(x))) 
{
    if (ncol(as.matrix(intervals)) == 1) 
        intervals <- cbind(intervals, intervals, deparse.level = 0)
    else if (ncol(as.matrix(intervals)) > 2) 
        stop("bad value of 'intervals'")
    attr(x, "levels") <- make.list.from.intervals(intervals)
    class(attr(x, "levels")) <- "shingleLevel"
    class(x) <- "shingle"
    x
}


panel.violin <- function (x, y, box.ratio = 1, box.width = box.ratio/(1 + box.ratio), 
    horizontal = TRUE, alpha = plot.polygon$alpha, border = plot.polygon$border, 
    lty = plot.polygon$lty, lwd = plot.polygon$lwd, col = plot.polygon$col, 
    varwidth = FALSE, bw = NULL, adjust = NULL, kernel = NULL, 
    window = NULL, width = NULL, n = 50, from = NULL, to = NULL, 
    cut = NULL, na.rm = TRUE, ..., identifier = "violin") 
{
    if (all(is.na(x) | is.na(y))) 
        return()
    x <- as.numeric(x)
    y <- as.numeric(y)
    plot.polygon <- trellis.par.get("plot.polygon")
    darg <- list()
    darg$bw <- bw
    darg$adjust <- adjust
    darg$kernel <- kernel
    darg$window <- window
    darg$width <- width
    darg$n <- n
    darg$from <- from
    darg$to <- to
    darg$cut <- cut
    darg$na.rm <- na.rm
    my.density <- function(x) {
        ans <- try(do.call("density", c(list(x = x), darg)), 
            silent = TRUE)
        if (inherits(ans, "try-error")) 
            list(x = rep(x[1], 3), y = c(0, 1, 0))
        else ans
    }
    numeric.list <- if (horizontal) 
        split(x, factor(y))
    else split(y, factor(x))
    levels.fos <- as.numeric(names(numeric.list))
    d.list <- lapply(numeric.list, my.density)
    dx.list <- lapply(d.list, "[[", "x")
    dy.list <- lapply(d.list, "[[", "y")
    max.d <- sapply(dy.list, max)
    if (varwidth) 
        max.d[] <- max(max.d)
    cur.limits <- current.panel.limits()
    xscale <- cur.limits$xlim
    yscale <- cur.limits$ylim
    height <- box.width
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    if (horizontal) {
        for (i in seq_along(levels.fos)) {
            if (is.finite(max.d[i])) {
                pushViewport(viewport(y = unit(levels.fos[i], 
                  "native"), height = unit(height, "native"), 
                  yscale = c(max.d[i] * c(-1, 1)), xscale = xscale))
                grid.polygon(x = c(dx.list[[i]], rev(dx.list[[i]])), 
                  y = c(dy.list[[i]], -rev(dy.list[[i]])), default.units = "native", 
                  name = trellis.grobname(identifier, type = "panel", 
                    group = group), gp = gpar(fill = col, col = border, 
                    lty = lty, lwd = lwd, alpha = alpha))
                popViewport()
            }
        }
    }
    else {
        for (i in seq_along(levels.fos)) {
            if (is.finite(max.d[i])) {
                pushViewport(viewport(x = unit(levels.fos[i], 
                  "native"), width = unit(height, "native"), 
                  xscale = c(max.d[i] * c(-1, 1)), yscale = yscale))
                grid.polygon(y = c(dx.list[[i]], rev(dx.list[[i]])), 
                  x = c(dy.list[[i]], -rev(dy.list[[i]])), default.units = "native", 
                  name = trellis.grobname(identifier, type = "panel", 
                    group = group), gp = gpar(fill = col, col = border, 
                    lty = lty, lwd = lwd, alpha = alpha))
                popViewport()
            }
        }
    }
    invisible()
}


panel.linejoin <- function (x, y, fun = mean, horizontal = TRUE, lwd = reference.line$lwd, 
    lty = reference.line$lty, col, col.line = reference.line$col, 
    type = "l", ..., identifier = "linejoin") 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    reference.line = trellis.par.get("reference.line")
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    if (horizontal) {
        vals <- unique(sort(y))
        yy <- seq_along(vals)
        xx <- numeric(length(yy))
        for (i in yy) xx[i] <- fun(x[y == vals[i]])
        panel.lines(xx, vals[yy], col = col.line, lty = lty, 
            lwd = lwd, ..., identifier = identifier)
    }
    else {
        vals <- unique(sort(x))
        xx <- seq_along(vals)
        yy <- numeric(length(xx))
        for (i in xx) yy[i] <- fun(y[x == vals[i]])
        panel.lines(vals[xx], yy, col = col.line, lty = lty, 
            lwd = lwd, ..., identifier = identifier)
    }
}


panel.polygon <- function (...) 
lpolygon(...)


panel.superpose.plain <- function (..., col = NA, col.line = plot.line$col, col.symbol = plot.symbol$col, 
    pch = plot.symbol$pch, cex = plot.symbol$cex, fill = plot.symbol$fill, 
    font = plot.symbol$font, fontface = plot.symbol$fontface, 
    fontfamily = plot.symbol$fontfamily, lty = plot.line$lty, 
    lwd = plot.line$lwd, alpha = plot.symbol$alpha) 
{
    plot.line <- trellis.par.get("plot.line")
    plot.symbol <- trellis.par.get("plot.symbol")
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
        if (missing(col.symbol)) 
            col.symbol <- col
    }
    panel.superpose(..., col.line = col.line, col.symbol = col.symbol, 
        pch = pch, cex = cex, fill = fill, font = font, fontfamily = fontfamily, 
        lty = lty, lwd = lwd, alpha = alpha)
}


current.row <- function (prefix = lattice.getStatus("current.prefix")) 
lattice.getStatus("current.focus.row", prefix = prefix)


lpoints <- function (x, ...) 
UseMethod("lpoints")


prepanel.default.bwplot <- function (x, y, horizontal = TRUE, nlevels, origin = NULL, stack = FALSE, 
    ...) 
{
    if (any(!is.na(x) & !is.na(y))) {
        if (horizontal) {
            if (!is.factor(y)) {
                if (missing(nlevels)) 
                  nlevels <- length(unique(y))
                y <- factor(y, levels = 1:nlevels)
            }
            list(xlim = if (stack) {
                foo1 <- if (any(x > 0)) range(tapply(x[x > 0], 
                  y[x > 0, drop = TRUE], sum, na.rm = TRUE), 
                  finite = TRUE) else 0
                foo2 <- if (any(x < 0)) range(tapply(x[x < 0], 
                  y[x < 0, drop = TRUE], sum, na.rm = TRUE), 
                  finite = TRUE) else 0
                range(foo1, foo2)
            } else scale.limits(c(x, origin)), ylim = levels(y), 
                yat = sort(unique(as.numeric(y))), dx = 1, dy = 1)
        }
        else {
            if (!is.factor(x)) {
                if (missing(nlevels)) 
                  nlevels <- length(unique(x))
                x <- factor(x, levels = 1:nlevels)
            }
            list(xlim = levels(x), xat = sort(unique(as.numeric(x))), 
                ylim = if (stack) {
                  foo1 <- if (any(y > 0)) range(tapply(y[y > 
                    0], x[y > 0], sum, na.rm = TRUE), finite = TRUE) else 0
                  foo2 <- if (any(y < 0)) range(tapply(y[y < 
                    0], x[y < 0], sum, na.rm = TRUE), finite = TRUE) else 0
                  range(foo1, foo2)
                } else scale.limits(c(y, origin)), dx = 1, dy = 1)
        }
    }
    else prepanel.null()
}


prepanel.default.parallel <- function (x, y, z, ..., horizontal.axis = TRUE) 
{
    if (horizontal.axis) 
        list(xlim = c(0, 1), ylim = extend.limits(c(1, ncol(as.data.frame(z))), 
            prop = 0.03), dx = 1, dy = 1)
    else list(xlim = extend.limits(c(1, ncol(as.data.frame(z))), 
        prop = 0.03), ylim = c(0, 1), dx = 1, dy = 1)
}


prepanel.qqmathline <- function (x, y = x, distribution = qnorm, probs = c(0.25, 0.75), 
    qtype = 7, groups = NULL, subscripts = TRUE, ...) 
{
    ans <- prepanel.default.qqmath(x, distribution = distribution, 
        qtype = qtype, groups = groups, subscripts = subscripts, 
        ...)
    y <- as.numeric(y)
    stopifnot(length(probs) == 2)
    distribution <- getFunctionOrName(distribution)
    getdy <- function(x) {
        diff(quantile(x, probs, names = FALSE, type = qtype, 
            na.rm = TRUE))
    }
    dy <- if (!is.null(groups)) 
        sapply(split(y, groups[subscripts]), getdy)
    else getdy(y)
    if (!all(is.na(dy))) {
        ans$dy <- dy[!is.na(dy)]
        ans$dx <- rep(diff(distribution(probs)), length(ans$dy))
    }
    ans
}


trellis.focus <- function (name, column = stop("column must be specified"), row = stop("row must be specified"), 
    side = NULL, clip.off = FALSE, highlight = interactive(), 
    ..., prefix, guess = TRUE, verbose = getOption("verbose")) 
{
    trellis.unfocus()
    if (!missing(prefix)) 
        lattice.setStatus(current.prefix = prefix)
    else prefix <- lattice.getStatus("current.prefix")
    if (missing(name) && missing(column) && missing(row)) 
        return(trellis.clickFocus(clip.off = clip.off, highlight = highlight, 
            ..., guess = guess, verbose = verbose))
    if (name %in% c("panel", "strip", "strip.left")) {
        ll <- lattice.getStatus("current.panel.positions", prefix = prefix)
        if (column > 0 && row > 0 && column <= ncol(ll) && row <= 
            nrow(ll) && ll[row, column] > 0) {
            lattice.setStatus(current.focus.column = column, 
                current.focus.row = row, prefix = prefix)
        }
        else stop("panel position unspecified or invalid")
    }
    else {
        if (!missing(row)) 
            lattice.setStatus(current.focus.row = row, prefix = prefix)
        if (!missing(column)) 
            lattice.setStatus(current.focus.column = column, 
                prefix = prefix)
    }
    lattice.setStatus(vp.depth = downViewport(trellis.vpname(name, 
        side = side, clip.off = clip.off, prefix = prefix)), 
        prefix = prefix)
    if (highlight) {
        lattice.setStatus(vp.highlighted = TRUE, prefix = prefix)
        gp <- do.call("gpar", updateList(lattice.getOption("highlight.gpar"), 
            list(...)))
        lvp <- rectGrob(name = "lvp.highlight", gp = gp)
        grid.draw(lvp)
    }
    else {
        lattice.setStatus(vp.highlighted = FALSE, prefix = prefix)
    }
    invisible()
}


do.breaks <- function (endpoints, nint) 
{
    if (length(endpoints) != 2) 
        stop("error")
    endpoints[1] + diff(endpoints) * 0:nint/nint
}


contourplot <- function (x, data, ...) 
UseMethod("contourplot")


lrect <- function (xleft, ybottom, xright, ytop, x = (xleft + xright)/2, 
    y = (ybottom + ytop)/2, width = xright - xleft, height = ytop - 
        ybottom, col = "transparent", border = "black", lty = 1, 
    lwd = 1, alpha = 1, just = "center", hjust = NULL, vjust = NULL, 
    ..., identifier = NULL, name.type = "panel") 
{
    border <- if (all(is.na(border))) 
        "transparent"
    else if (is.logical(border)) {
        if (border) 
            "black"
        else "transparent"
    }
    else border
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    grid.rect(x = x, y = y, width = width, height = height, default.units = "native", 
        just = just, hjust = hjust, vjust = vjust, name = primName("rect", 
            identifier, name.type, group), gp = gpar(fill = col, 
            col = border, lty = lty, lwd = lwd, alpha = alpha, 
            ...))
}


strip.default <- function (which.given, which.panel, var.name, factor.levels, 
    shingle.intervals = NULL, strip.names = c(FALSE, TRUE), strip.levels = c(TRUE, 
        FALSE), sep = " : ", style = 1, horizontal = TRUE, bg = trellis.par.get("strip.background")$col[which.given], 
    fg = trellis.par.get("strip.shingle")$col[which.given], par.strip.text = trellis.par.get("add.text")) 
{
    if (horizontal) 
        pushViewport(viewport(y = (which.given - 0.5)/length(which.panel), 
            height = 1/length(which.panel), clip = trellis.par.get("clip")$strip, 
            name = paste(lattice.getStatus("current.prefix"), 
                "strip.default", which.given, sep = ".")))
    else pushViewport(viewport(x = 1 - (which.given - 0.5)/length(which.panel), 
        width = 1/length(which.panel), clip = trellis.par.get("clip")$strip, 
        name = paste(lattice.getStatus("current.prefix"), "strip.left.default", 
            which.given, sep = ".")))
    gp.text <- gpar(col = par.strip.text$col, alpha = par.strip.text$alpha, 
        lineheight = par.strip.text$lineheight, fontfamily = par.strip.text$fontfamily, 
        fontface = chooseFace(par.strip.text$fontface, par.strip.text$font), 
        cex = par.strip.text$cex)
    name <- var.name[which.given]
    level <- which.panel[which.given]
    strip.names <- rep(strip.names, length.out = 2)
    strip.levels <- rep(strip.levels, length.out = 2)
    formatLabel <- function(s, abbreviate = par.strip.text$abbr, 
        minlength = par.strip.text$minl, dot = par.strip.text$dot) {
        if (is.null(abbreviate)) 
            abbreviate <- FALSE
        if (is.null(minlength)) 
            minlength <- 4
        if (is.null(dot)) 
            dot <- FALSE
        if (abbreviate) 
            abbreviate(s, minlength = minlength, dot = dot)
        else s
    }
    factor.levels <- formatLabel(factor.levels)
    if (!is.null(shingle.intervals)) {
        if (horizontal) 
            type <- "strip"
        else type <- "strip.left"
        grid.rect(name = trellis.grobname("bg", type = type), 
            gp = gpar(fill = bg, col = bg))
        t <- range(shingle.intervals)
        r <- (range(shingle.intervals[level, ]) - t[1])/diff(t)
        if (horizontal) 
            grid.rect(x = unit(r %*% c(0.5, 0.5), "npc"), width = max(unit(c(diff(r), 
                1), c("npc", "mm"))), name = trellis.grobname("fg", 
                type = "strip"), gp = gpar(col = fg, fill = fg))
        else grid.rect(y = unit(r %*% c(0.5, 0.5), "npc"), height = max(unit(c(diff(r), 
            1), c("npc", "mm"))), name = trellis.grobname("fg", 
            type = "strip.left"), gp = gpar(col = fg, fill = fg))
        paste.and.draw(name, factor.levels[level], sep = sep, 
            horizontal = horizontal, showl = strip.names[2], 
            showr = strip.levels[2], gp = gp.text)
    }
    else {
        num <- length(factor.levels)
        if (style != 2) {
            if (horizontal) 
                type <- "strip"
            else type <- "strip.left"
            grid.rect(name = trellis.grobname("bg", type = type), 
                gp = gpar(fill = bg, col = bg))
        }
        if (num > 0 && style %in% c(2, 3, 4)) {
            if (horizontal) {
                grid.rect(x = unit((2 * level - 1)/(2 * num), 
                  "npc"), width = unit(1/num, "npc"), name = trellis.grobname("fg", 
                  type = "strip"), gp = gpar(fill = fg, col = fg))
            }
            else {
                grid.rect(y = unit((2 * level - 1)/(2 * num), 
                  "npc"), height = unit(1/num, "npc"), name = trellis.grobname("fg", 
                  type = "strip.left"), gp = gpar(fill = fg, 
                  col = fg))
            }
        }
        if (style %in% c(1, 3)) {
            paste.and.draw(name, factor.levels[level], sep = sep, 
                horizontal = horizontal, showl = strip.names[1], 
                showr = strip.levels[1], gp = gp.text)
        }
        else if (num > 0) {
            lid <- if (style %in% c(2, 4)) 
                1:num
            else level
            if (horizontal) {
                grid.text(label = factor.levels[lid], x = (2 * 
                  lid - 1)/(2 * num), name = trellis.grobname("fg", 
                  type = "strip"), gp = gp.text)
            }
            else {
                grid.text(label = factor.levels[lid], y = (2 * 
                  lid - 1)/(2 * num), rot = 90, name = trellis.grobname("fg", 
                  type = "strip.left"), gp = gp.text)
            }
        }
    }
    upViewport()
    if (horizontal) 
        pushViewport(viewport(y = (which.given - 0.5)/length(which.panel), 
            height = 1/length(which.panel), clip = "off", name = paste(lattice.getStatus("current.prefix"), 
                "strip.default.off", which.given, sep = ".")))
    else pushViewport(viewport(x = 1 - (which.given - 0.5)/length(which.panel), 
        width = 1/length(which.panel), clip = "off", name = paste(lattice.getStatus("current.prefix"), 
            "strip.left.default.off", which.given, sep = ".")))
    strip.border <- trellis.par.get("strip.border")
    if (horizontal) 
        type <- "strip"
    else type <- "strip.left"
    grid.rect(name = trellis.grobname("border", type = type), 
        gp = gpar(col = rep(strip.border$col, length.out = which.given)[which.given], 
            lty = rep(strip.border$lty, length.out = which.given)[which.given], 
            lwd = rep(strip.border$lwd, length.out = which.given)[which.given], 
            alpha = rep(strip.border$alpha, length.out = which.given)[which.given], 
            fill = "transparent"))
    upViewport()
}


prepanel.default.qqmath <- function (x, f.value = NULL, distribution = qnorm, qtype = 7, 
    groups = NULL, subscripts, ..., tails.n = 0) 
{
    if (!is.numeric(x)) 
        x <- as.numeric(x)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    if (tails.n > 0) 
        f.value <- NULL
    getxx <- function(x, f.value = NULL, nobs = sum(!is.na(x))) {
        if (is.null(f.value)) 
            distribution(ppoints(nobs))
        else if (is.numeric(f.value)) 
            distribution(f.value)
        else distribution(f.value(nobs))
    }
    getyy <- function(x, f.value = NULL, nobs = sum(!is.na(x))) {
        if (is.null(f.value)) 
            sort(x)
        else if (is.numeric(f.value)) 
            quantile(x, f.value, names = FALSE, type = qtype, 
                na.rm = TRUE)
        else quantile(x, f.value(nobs), names = FALSE, type = qtype, 
            na.rm = TRUE)
    }
    if (!nobs) 
        prepanel.null()
    else if (!is.null(groups)) {
        sx <- split(x, groups[subscripts])
        xxlist <- lapply(sx, getxx, f.value = f.value)
        yylist <- lapply(sx, getyy, f.value = f.value)
        list(xlim = range(unlist(xxlist), finite = TRUE), ylim = range(unlist(yylist), 
            finite = TRUE), dx = unlist(lapply(xxlist, diff)), 
            dy = unlist(lapply(yylist, diff)))
    }
    else {
        xx <- getxx(x, f.value, nobs)
        yy <- getyy(x, f.value, nobs)
        list(xlim = scale.limits(xx), ylim = scale.limits(yy), 
            dx = diff(xx), dy = diff(yy))
    }
}


panel.fill <- function (col = trellis.par.get("background")$col, border = "transparent", 
    ..., identifier = "fill") 
{
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    grid.rect(name = trellis.grobname(identifier, type = "panel", 
        group = group), gp = gpar(fill = col, col = border, ...))
}


panel.link.splom <- function (threshold = 18, verbose = getOption("verbose"), ...) 
{
    ans <- numeric(0)
    repeat {
        new <- splom.linkPoint(threshold = threshold, verbose = verbose, 
            ...)
        if (is.null(new)) 
            break
        else ans[length(ans) + 1] <- new
    }
    ans
}


panel.splom <- function (..., identifier = "splom") 
panel.xyplot(..., identifier = identifier)


panel.levelplot.raster <- function (x, y, z, subscripts, at = pretty(z), ..., col.regions = regions$col, 
    alpha.regions = regions$alpha, interpolate = FALSE, identifier = "levelplot") 
{
    if (length(subscripts) == 0) 
        return()
    regions <- trellis.par.get("regions")
    x.is.factor <- is.factor(x)
    y.is.factor <- is.factor(y)
    x <- as.numeric(x)
    y <- as.numeric(y)
    z <- as.numeric(z)
    zcol <- level.colors(z, at, col.regions, colors = TRUE)
    x <- x[subscripts]
    y <- y[subscripts]
    z <- z[subscripts]
    zcol <- zcol[subscripts]
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    if (x.is.factor) {
        ux <- seq(from = min(x, na.rm = TRUE), to = max(x, na.rm = TRUE))
        xwid <- 1L
    }
    else {
        ux <- sort(unique(x[!is.na(x)]))
        if (!isTRUE(all.equal(diff(range(diff(ux))), 0))) 
            warning("'x' values are not equispaced; output may be wrong")
        xwid <- mean(diff(ux))
    }
    if (y.is.factor) {
        ux <- seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE))
        ywid <- 1L
    }
    else {
        uy <- sort(unique(y[!is.na(y)]))
        if (!isTRUE(all.equal(diff(range(diff(uy))), 0))) 
            warning("'y' values are not equispaced; output may be wrong")
        ywid <- mean(diff(uy))
    }
    ncolumns <- length(ux)
    nrows <- length(uy)
    xlow <- ux[1] - 0.5 * xwid
    xhigh <- ux[ncolumns] + 0.5 * xwid
    ylow <- uy[1] - 0.5 * ywid
    yhigh <- uy[nrows] + 0.5 * ywid
    zmat <- rep("transparent", ncolumns * nrows)
    idx <- match(x, ux)
    idy <- match(y, rev(uy))
    id <- idy + nrows * (idx - 1L)
    zmat[id] <- zcol
    dim(zmat) <- c(nrows, ncolumns)
    grid.raster(as.raster(zmat), interpolate = interpolate, x = xlow, 
        y = ylow, width = xhigh - xlow, height = yhigh - ylow, 
        just = c("left", "bottom"), default.units = "native", 
        name = trellis.grobname(paste(identifier, "raster", sep = "."), 
            type = "panel", group = group))
}


trellis.switchFocus <- function (name, side = NULL, clip.off = FALSE, highlight, ..., 
    prefix) 
{
    if (!missing(prefix) && prefix != lattice.getStatus("current.prefix")) 
        stop("'trellis.switchFocus' cannot be used to switch to a different 'prefix'.  Use 'trellis.focus' first")
    prefix <- lattice.getStatus("current.prefix")
    row <- lattice.getStatus("current.focus.row", prefix = prefix)
    column <- lattice.getStatus("current.focus.column", prefix = prefix)
    if (missing(highlight)) 
        highlight <- lattice.getStatus("vp.highlighted", prefix = prefix)
    trellis.focus(name = name, row = row, column = column, side = side, 
        clip.off = clip.off, highlight = highlight, ...)
}


panel.cloud <- function (x, y, subscripts, z, groups = NULL, perspective = TRUE, 
    distance = if (perspective) 0.2 else 0, xlim, ylim, zlim, 
    panel.3d.cloud = "panel.3dscatter", panel.3d.wireframe = "panel.3dwire", 
    screen = list(z = 40, x = -60), R.mat = diag(4), aspect = c(1, 
        1), par.box = NULL, xlab, ylab, zlab, xlab.default, ylab.default, 
    zlab.default, scales.3d, proportion = 0.6, wireframe = FALSE, 
    scpos, ..., at, identifier = "cloud") 
{
    if (is.factor(x)) 
        x <- as.numeric(x)
    if (is.factor(y)) 
        y <- as.numeric(y)
    if (is.factor(z)) 
        z <- as.numeric(z)
    rot.mat <- ltransform3dMatrix(screen = screen, R.mat = R.mat)
    isParametrizedSurface <- wireframe && is.matrix(x) && is.matrix(y) && 
        is.matrix(z)
    if (isParametrizedSurface) 
        zrng <- extend.limits(sqrt(range(x^2 + y^2 + z^2, finite = TRUE)))
    if (length(subscripts) > 0) {
        xlabelinfo <- calculateAxisComponents(xlim, at = scales.3d$x.scales$at, 
            num.limit = NULL, labels = scales.3d$x.scales$labels, 
            logsc = scales.3d$x.scales$log, abbreviate = scales.3d$x.scales$abbreviate, 
            minlength = scales.3d$x.scales$minlength, format.posixt = scales.3d$x.scales$format, 
            n = scales.3d$x.scales$tick.number)
        ylabelinfo <- calculateAxisComponents(ylim, at = scales.3d$y.scales$at, 
            num.limit = NULL, labels = scales.3d$y.scales$labels, 
            logsc = scales.3d$y.scales$log, abbreviate = scales.3d$y.scales$abbreviate, 
            minlength = scales.3d$y.scales$minlength, format.posixt = scales.3d$y.scales$format, 
            n = scales.3d$y.scales$tick.number)
        zlabelinfo <- calculateAxisComponents(zlim, at = scales.3d$z.scales$at, 
            num.limit = NULL, labels = scales.3d$z.scales$labels, 
            logsc = scales.3d$z.scales$log, abbreviate = scales.3d$z.scales$abbreviate, 
            minlength = scales.3d$z.scales$minlength, format.posixt = scales.3d$z.scales$format, 
            n = scales.3d$z.scales$tick.number)
        x.at <- xlabelinfo$at
        y.at <- ylabelinfo$at
        z.at <- zlabelinfo$at
        x.at.lab <- xlabelinfo$labels
        y.at.lab <- ylabelinfo$labels
        z.at.lab <- zlabelinfo$labels
        xlim <- xlabelinfo$num.limit
        ylim <- ylabelinfo$num.limit
        zlim <- zlabelinfo$num.limit
        par.box.final <- trellis.par.get("box.3d")
        if (!is.null(par.box)) 
            par.box.final[names(par.box)] <- par.box
        aspect <- rep(aspect, length.out = 2)
        if (!isParametrizedSurface) {
            x <- x[subscripts]
            y <- y[subscripts]
            z <- z[subscripts]
        }
        corners <- data.frame(x = c(-1, 1, 1, -1, -1, 1, 1, -1), 
            y = c(-1, -1, -1, -1, 1, 1, 1, 1) * aspect[1], z = c(-1, 
                -1, 1, 1, -1, -1, 1, 1) * aspect[2])
        corners <- corners/(2 * max(corners))
        xlim.scaled <- range(corners$x)
        ylim.scaled <- range(corners$y)
        zlim.scaled <- range(corners$z)
        box.center <- matrix(unlist(lapply(corners, mean)), 3, 
            1)
        pre <- c(1, 2, 4, 1, 2, 3, 4, 1, 5, 6, 8, 5)
        nxt <- c(2, 3, 3, 4, 6, 7, 8, 5, 6, 7, 7, 8)
        face.corners <- list(c(1, 2, 3, 4), c(2, 6, 7, 3), c(6, 
            5, 8, 7), c(5, 1, 4, 8), c(1, 2, 6, 5), c(4, 3, 7, 
            8))
        face.lines <- list(c(1, 2, 3, 4), c(5, 10, 6, 2), c(9, 
            12, 11, 10), c(8, 4, 7, 12), c(1, 5, 9, 8), c(3, 
            6, 11, 7))
        tmp <- ltransform3dto3d(t(as.matrix(corners)), rot.mat)
        farthest <- 1
        farval <- tmp[3, 1]
        for (i in 2:8) if (tmp[3, i] < farval) {
            farthest <- i
            farval <- tmp[3, i]
        }
        scale.position <- if (farthest == 1) 
            list(x = 3, y = 7, z = 2)
        else if (farthest == 2) 
            list(x = 9, y = 8, z = 10)
        else if (farthest == 3) 
            list(x = 11, y = 7, z = 10)
        else if (farthest == 4) 
            list(x = 11, y = 6, z = 2)
        else if (farthest == 5) 
            list(x = 1, y = 5, z = 4)
        else if (farthest == 6) 
            list(x = 1, y = 8, z = 12)
        else if (farthest == 7) 
            list(x = 3, y = 7, z = 2)
        else if (farthest == 8) 
            list(x = 3, y = 6, z = 10)
        if (!missing(scpos)) 
            scale.position[names(scpos)] <- scpos
        scpos <- scale.position
        labs <- rbind(x = c(0, corners$x[pre[scpos$y]], corners$x[pre[scpos$z]]), 
            y = c(corners$y[pre[scpos$x]], 0, corners$y[pre[scpos$z]]), 
            z = c(corners$z[pre[scpos$x]], corners$z[pre[scpos$y]], 
                0))
        labs[, 1] <- labs[, 1] * (1 + scales.3d$x.scales$distance/3)
        labs[, 2] <- labs[, 2] * (1 + scales.3d$y.scales$distance/3)
        labs[, 3] <- labs[, 3] * (1 + scales.3d$z.scales$distance/3)
        axes <- rbind(x = c(proportion * corners$x[c(pre[scpos$x], 
            nxt[scpos$x])], corners$x[c(pre[scpos$y], nxt[scpos$y])], 
            corners$x[c(pre[scpos$z], nxt[scpos$z])]), y = c(corners$y[c(pre[scpos$x], 
            nxt[scpos$x])], proportion * corners$y[c(pre[scpos$y], 
            nxt[scpos$y])], corners$y[c(pre[scpos$z], nxt[scpos$z])]), 
            z = c(corners$z[c(pre[scpos$x], nxt[scpos$x])], corners$z[c(pre[scpos$y], 
                nxt[scpos$y])], proportion * corners$z[c(pre[scpos$z], 
                nxt[scpos$z])]))
        axes[, 1:2] <- axes[, 1:2] * (1 + scales.3d$x.scales$distance/10)
        axes[, 3:4] <- axes[, 3:4] * (1 + scales.3d$y.scales$distance/10)
        axes[, 5:6] <- axes[, 5:6] * (1 + scales.3d$z.scales$distance/10)
        cmin <- lapply(corners, min)
        clen <- lapply(corners, function(x) diff(range(x, finite = TRUE)))
        x <- cmin$x + clen$x * (x - xlim[1])/diff(xlim)
        y <- cmin$y + clen$y * (y - ylim[1])/diff(ylim)
        z <- cmin$z + clen$z * (z - zlim[1])/diff(zlim)
        at <- if (isParametrizedSurface) {
            zrng.scaled <- extend.limits(sqrt(range(x^2 + y^2 + 
                z^2, finite = TRUE)))
            zrng.scaled[1] + diff(zrng.scaled) * (at - zrng[1])/diff(zrng)
        }
        else cmin$z + clen$z * (at - zlim[1])/diff(zlim)
        zero.scaled <- cmin$z - clen$z * zlim[1]/diff(zlim)
        x.at <- cmin$x + clen$x * (x.at - xlim[1])/diff(xlim)
        y.at <- cmin$y + clen$y * (y.at - ylim[1])/diff(ylim)
        z.at <- cmin$z + clen$z * (z.at - zlim[1])/diff(zlim)
        at.len <- length(x.at)
        x.at <- rbind(x = x.at, y = rep(corners$y[pre[scpos$x]], 
            at.len), z = rep(corners$z[pre[scpos$x]], at.len))
        at.len <- length(y.at)
        y.at <- rbind(x = rep(corners$x[pre[scpos$y]], at.len), 
            y = y.at, z = rep(corners$z[pre[scpos$y]], at.len))
        at.len <- length(z.at)
        z.at <- rbind(x = rep(corners$x[pre[scpos$z]], at.len), 
            y = rep(corners$y[pre[scpos$z]], at.len), z = z.at)
        x.at.end <- x.at + scales.3d$x.scales$tck * 0.05 * labs[, 
            1]
        y.at.end <- y.at + scales.3d$y.scales$tck * 0.05 * labs[, 
            2]
        z.at.end <- z.at + scales.3d$z.scales$tck * 0.05 * labs[, 
            3]
        x.labs <- x.at + 2 * scales.3d$x.scales$tck * 0.05 * 
            labs[, 1]
        y.labs <- y.at + 2 * scales.3d$y.scales$tck * 0.05 * 
            labs[, 2]
        z.labs <- z.at + 2 * scales.3d$z.scales$tck * 0.05 * 
            labs[, 3]
        corners <- ltransform3dto3d(t(as.matrix(corners)), rot.mat, 
            distance)
        taxes <- ltransform3dto3d(axes, rot.mat, distance)
        x.at <- ltransform3dto3d(x.at, rot.mat, distance)
        x.labs <- ltransform3dto3d(x.labs, rot.mat, distance)
        x.at.end <- ltransform3dto3d(x.at.end, rot.mat, distance)
        y.at <- ltransform3dto3d(y.at, rot.mat, distance)
        y.labs <- ltransform3dto3d(y.labs, rot.mat, distance)
        y.at.end <- ltransform3dto3d(y.at.end, rot.mat, distance)
        z.at <- ltransform3dto3d(z.at, rot.mat, distance)
        z.labs <- ltransform3dto3d(z.labs, rot.mat, distance)
        z.at.end <- ltransform3dto3d(z.at.end, rot.mat, distance)
        tlabs <- ltransform3dto3d(labs, rot.mat, distance)
        box.center <- ltransform3dto3d(box.center, rot.mat, distance)
        mark <- rep(FALSE, 12)
        box.center.z <- box.center[3]
        for (face in 1:6) if (mean(corners[3, face.corners[[face]]]) > 
            box.center.z) 
            mark[1:12 %in% face.lines[[face]]] <- TRUE
        lsegments(corners[1, pre[!mark]], corners[2, pre[!mark]], 
            corners[1, nxt[!mark]], corners[2, nxt[!mark]], col = par.box.final$col, 
            lwd = par.box.final$lwd, lty = 2, identifier = paste(identifier, 
                "back.box", sep = "."))
        if (wireframe) {
            if (isParametrizedSurface) {
                tmp <- z
            }
            else if (is.null(groups)) {
                nx <- length(unique(x))
                ny <- length(unique(y))
                len <- length(z)
                if (nx * ny == len) {
                  ord <- order(x, y)
                  tmp <- z[ord]
                  x <- sort(unique(x[!is.na(x)]))
                  y <- sort(unique(y[!is.na(y)]))
                }
                else {
                  tmp <- rep(NA_real_, nx * ny)
                  ux <- sort(unique(x[!is.na(x)]))
                  uy <- sort(unique(y[!is.na(y)]))
                  idx <- match(x, ux)
                  idy <- match(y, uy)
                  tmp[(idx - 1) * length(uy) + idy] <- z
                  x <- ux
                  y <- uy
                }
            }
            else {
                vals <- sort(unique(groups))
                tmp <- numeric(0)
                for (i in seq_along(vals)) {
                  id <- (groups[subscripts] == vals[i])
                  if (any(id)) {
                    ord <- order(x[id], y[id])
                    tmp <- cbind(tmp, z[id][ord])
                  }
                }
                x <- sort(unique(x))
                y <- sort(unique(y))
            }
            z <- list(NULL)
            panel.3d.wireframe <- getFunctionOrName(panel.3d.wireframe)
            pargs <- list(x = x, y = y, z = tmp, rot.mat = rot.mat, 
                distance = distance, at = at, xlim = xlim, ylim = ylim, 
                zlim = zlim, xlim.scaled = xlim.scaled, ylim.scaled = ylim.scaled, 
                zlim.scaled = zlim.scaled, zero.scaled = zero.scaled, 
                ...)
            if (!("..." %in% names(formals(panel.3d.wireframe)))) 
                pargs <- pargs[intersect(names(pargs), names(formals(panel.3d.wireframe)))]
            do.call("panel.3d.wireframe", pargs)
        }
        else {
            panel.3d.cloud <- getFunctionOrName(panel.3d.cloud)
            pargs <- list(x = x, y = y, z = z, rot.mat = rot.mat, 
                distance = distance, groups = groups, subscripts = subscripts, 
                xlim = xlim, ylim = ylim, zlim = zlim, xlim.scaled = xlim.scaled, 
                ylim.scaled = ylim.scaled, zlim.scaled = zlim.scaled, 
                zero.scaled = zero.scaled, ...)
            if (!("..." %in% names(formals(panel.3d.cloud)))) 
                pargs <- pargs[intersect(names(pargs), names(formals(panel.3d.cloud)))]
            do.call("panel.3d.cloud", pargs)
        }
        lsegments(corners[1, pre[mark]], corners[2, pre[mark]], 
            corners[1, nxt[mark]], corners[2, nxt[mark]], col = par.box.final$col, 
            lty = par.box.final$lty, lwd = par.box.final$lwd, 
            identifier = paste(identifier, "front.box", sep = "."))
        axis.text <- trellis.par.get("axis.text")
        axis.line <- trellis.par.get("axis.line")
        xaxis.col.line <- if (is.logical(scales.3d$x.scales$col.line)) 
            axis.line$col
        else scales.3d$x.scales$col.line
        xaxis.lty <- if (is.logical(scales.3d$x.scales$lty)) 
            axis.line$lty
        else scales.3d$x.scales$lty
        xaxis.lwd <- if (is.logical(scales.3d$x.scales$lwd)) 
            axis.line$lwd
        else scales.3d$x.scales$lwd
        xaxis.col.text <- if (is.logical(scales.3d$x.scales$col)) 
            axis.text$col
        else scales.3d$x.scales$col
        xaxis.font <- if (is.logical(scales.3d$x.scales$font)) 
            axis.text$font
        else scales.3d$x.scales$font
        xaxis.fontface <- if (is.logical(scales.3d$x.scales$fontface)) 
            axis.text$fontface
        else scales.3d$x.scales$fontface
        xaxis.fontfamily <- if (is.logical(scales.3d$x.scales$fontfamily)) 
            axis.text$fontfamily
        else scales.3d$x.scales$fontfamily
        xaxis.cex <- if (is.logical(scales.3d$x.scales$cex)) 
            rep(axis.text$cex, length.out = 1)
        else scales.3d$x.scales$cex
        xaxis.rot <- if (is.logical(scales.3d$x.scales$rot)) 
            0
        else scales.3d$x.scales$rot
        yaxis.col.line <- if (is.logical(scales.3d$y.scales$col.line)) 
            axis.line$col
        else scales.3d$y.scales$col.line
        yaxis.lty <- if (is.logical(scales.3d$y.scales$lty)) 
            axis.line$lty
        else scales.3d$y.scales$lty
        yaxis.lwd <- if (is.logical(scales.3d$y.scales$lwd)) 
            axis.line$lwd
        else scales.3d$y.scales$lwd
        yaxis.col.text <- if (is.logical(scales.3d$y.scales$col)) 
            axis.text$col
        else scales.3d$y.scales$col
        yaxis.font <- if (is.logical(scales.3d$y.scales$font)) 
            axis.text$font
        else scales.3d$y.scales$font
        yaxis.fontface <- if (is.logical(scales.3d$y.scales$fontface)) 
            axis.text$fontface
        else scales.3d$y.scales$fontface
        yaxis.fontfamily <- if (is.logical(scales.3d$y.scales$fontfamily)) 
            axis.text$fontfamily
        else scales.3d$y.scales$fontfamily
        yaxis.cex <- if (is.logical(scales.3d$y.scales$cex)) 
            rep(axis.text$cex, length.out = 1)
        else scales.3d$y.scales$cex
        yaxis.rot <- if (is.logical(scales.3d$y.scales$rot)) 
            0
        else scales.3d$y.scales$rot
        zaxis.col.line <- if (is.logical(scales.3d$z.scales$col.line)) 
            axis.line$col
        else scales.3d$z.scales$col.line
        zaxis.lty <- if (is.logical(scales.3d$z.scales$lty)) 
            axis.line$lty
        else scales.3d$z.scales$lty
        zaxis.lwd <- if (is.logical(scales.3d$z.scales$lwd)) 
            axis.line$lwd
        else scales.3d$z.scales$lwd
        zaxis.col.text <- if (is.logical(scales.3d$z.scales$col)) 
            axis.text$col
        else scales.3d$z.scales$col
        zaxis.font <- if (is.logical(scales.3d$z.scales$font)) 
            axis.text$font
        else scales.3d$z.scales$font
        zaxis.fontface <- if (is.logical(scales.3d$z.scales$fontface)) 
            axis.text$fontface
        else scales.3d$z.scales$fontface
        zaxis.fontfamily <- if (is.logical(scales.3d$z.scales$fontfamily)) 
            axis.text$fontfamily
        else scales.3d$z.scales$fontfamily
        zaxis.cex <- if (is.logical(scales.3d$z.scales$cex)) 
            rep(axis.text$cex, length.out = 1)
        else scales.3d$z.scales$cex
        zaxis.rot <- if (is.logical(scales.3d$z.scales$rot)) 
            0
        else scales.3d$z.scales$rot
        if (scales.3d$x.scales$draw) {
            if (scales.3d$x.scales$arrows) {
                larrows(x0 = taxes[1, 1], y0 = taxes[2, 1], x1 = taxes[1, 
                  2], y1 = taxes[2, 2], length = 0.02, unit = "npc", 
                  lty = xaxis.lty, lwd = xaxis.lwd, col = xaxis.col.line, 
                  identifier = paste(identifier, "x.axis.arrow", 
                    sep = "."))
            }
            else {
                lsegments(x0 = x.at[1, ], y0 = x.at[2, ], x1 = x.at.end[1, 
                  ], y1 = x.at.end[2, ], lty = xaxis.lty, col = xaxis.col.line, 
                  lwd = xaxis.lwd, identifier = paste(identifier, 
                    "x.axis.ticks", sep = "."))
                ltext(x.at.lab, x = x.labs[1, ], y = x.labs[2, 
                  ], cex = xaxis.cex, srt = xaxis.rot, font = xaxis.font, 
                  fontfamily = xaxis.fontfamily, fontface = xaxis.fontface, 
                  col = xaxis.col.text, identifier = paste(identifier, 
                    "x.axis.labels", sep = "."))
            }
        }
        if (scales.3d$y.scales$draw) {
            if (scales.3d$y.scales$arrows) {
                larrows(x0 = taxes[1, 3], y0 = taxes[2, 3], x1 = taxes[1, 
                  4], y1 = taxes[2, 4], length = 0.02, unit = "npc", 
                  lty = yaxis.lty, lwd = yaxis.lwd, col = yaxis.col.line, 
                  identifier = paste(identifier, "y.axis.arrow", 
                    sep = "."))
            }
            else {
                lsegments(x0 = y.at[1, ], y0 = y.at[2, ], x1 = y.at.end[1, 
                  ], y1 = y.at.end[2, ], lty = yaxis.lty, col = yaxis.col.line, 
                  lwd = yaxis.lwd, identifier = paste(identifier, 
                    "y.axis.ticks", sep = "."))
                ltext(y.at.lab, x = y.labs[1, ], y = y.labs[2, 
                  ], cex = yaxis.cex, srt = yaxis.rot, font = yaxis.font, 
                  fontfamily = yaxis.fontfamily, fontface = yaxis.fontface, 
                  col = yaxis.col.text, identifier = paste(identifier, 
                    "y.axis.labels", sep = "."))
            }
        }
        if (scales.3d$z.scales$draw) {
            if (scales.3d$z.scales$arrows) {
                larrows(x0 = taxes[1, 5], y0 = taxes[2, 5], x1 = taxes[1, 
                  6], y1 = taxes[2, 6], length = 0.02, unit = "npc", 
                  lty = zaxis.lty, lwd = zaxis.lwd, col = zaxis.col.line, 
                  identifier = paste(identifier, "z.axis.arrow", 
                    sep = "."))
            }
            else {
                lsegments(x0 = z.at[1, ], y0 = z.at[2, ], x1 = z.at.end[1, 
                  ], y1 = z.at.end[2, ], lty = zaxis.lty, col = zaxis.col.line, 
                  lwd = zaxis.lwd, identifier = paste(identifier, 
                    "z.axis.ticks", sep = "."))
                ltext(z.at.lab, x = z.labs[1, ], y = z.labs[2, 
                  ], cex = zaxis.cex, srt = zaxis.rot, font = zaxis.font, 
                  fontfamily = zaxis.fontfamily, fontface = zaxis.fontface, 
                  col = zaxis.col.text, identifier = paste(identifier, 
                    "z.axis.labels", sep = "."))
            }
        }
        xlab <- getLabelList(xlab, trellis.par.get("par.xlab.text"), 
            xlab.default)
        ylab <- getLabelList(ylab, trellis.par.get("par.ylab.text"), 
            ylab.default)
        zlab <- getLabelList(zlab, trellis.par.get("par.zlab.text"), 
            zlab.default)
        xlab <- grobFromLabelList(xlab, name = trellis.grobname("xlab", 
            type = ""))
        ylab <- grobFromLabelList(ylab, name = trellis.grobname("ylab", 
            type = ""))
        zlab <- grobFromLabelList(zlab, name = trellis.grobname("zlab", 
            type = ""))
        if (!is.null(xlab)) {
            pushViewport(viewport(x = tlabs[1, 1], y = tlabs[2, 
                1], default.units = "native"))
            grid.draw(xlab)
            upViewport()
        }
        if (!is.null(ylab)) {
            pushViewport(viewport(x = tlabs[1, 2], y = tlabs[2, 
                2], default.units = "native"))
            grid.draw(ylab)
            upViewport()
        }
        if (!is.null(zlab)) {
            pushViewport(viewport(x = tlabs[1, 3], y = tlabs[2, 
                3], default.units = "native"))
            grid.draw(zlab)
            upViewport()
        }
    }
}


bwplot <- function (x, data, ...) 
UseMethod("bwplot")


is.shingle <- function (x) 
inherits(x, "shingle")


col.whitebg <- function () 
list(background = list(col = "transparent"), plot.polygon = list(col = "#c8ffc8"), 
    box.rectangle = list(col = "darkgreen"), box.umbrella = list(col = "darkgreen"), 
    dot.line = list(col = "#e8e8e8"), dot.symbol = list(col = "darkgreen"), 
    plot.line = list(col = "darkgreen"), plot.symbol = list(col = "darkgreen"), 
    regions = list(col = heat.colors(100)), strip.shingle = list(col = c("#ff7f00", 
        "#00ff00", "#00ffff", "#0080ff", "#ff00ff", "#ff0000", 
        "#ffff00")), strip.background = list(col = c("#ffe5cc", 
        "#ccffcc", "#ccffff", "#cce6ff", "#ffccff", "#ffcccc", 
        "#ffffcc")), reference.line = list(col = "#e8e8e8"), 
    superpose.line = list(col = c("darkgreen", "red", "royalblue", 
        "brown", "orange", "turquoise", "orchid"), lty = 1:7), 
    superpose.symbol = list(pch = c(1, 3, 6, 0, 5, 16, 17), cex = rep(0.7, 
        7), col = c("darkgreen", "red", "royalblue", "brown", 
        "orange", "turquoise", "orchid")))


ltransform3dto3d <- function (x, R.mat, dist = 0) 
{
    if (length(x) == 0) 
        return(x)
    tdata <- R.mat %*% rbind(x, 1)
    tdata[1, ] <- tdata[1, ]/tdata[4, ]
    tdata[2, ] <- tdata[2, ]/tdata[4, ]
    tdata[3, ] <- tdata[3, ]/tdata[4, ]
    if (dist != 0) {
        tdata[1, ] <- tdata[1, ]/(1/dist - tdata[3, ])
        tdata[2, ] <- tdata[2, ]/(1/dist - tdata[3, ])
    }
    tdata[1:3, , drop = FALSE]
}


prepanel.default.cloud <- function (perspective = TRUE, distance = if (perspective) 0.2 else 0, 
    xlim, ylim, zlim, screen = list(z = 40, x = -60), R.mat = diag(4), 
    aspect = c(1, 1), panel.aspect = 1, ..., zoom = 0.8) 
{
    rot.mat <- ltransform3dMatrix(screen = screen, R.mat = R.mat)
    aspect <- rep(aspect, length.out = 2)
    corners <- rbind(x = c(-1, 1, 1, -1, -1, 1, 1, -1), y = c(-1, 
        -1, -1, -1, 1, 1, 1, 1) * aspect[1], z = c(-1, -1, 1, 
        1, -1, -1, 1, 1) * aspect[2])
    corners <- corners/(2 * max(corners))
    corners <- ltransform3dto3d(corners, rot.mat, dist = distance)
    xrng <- range(corners[1, ])
    yrng <- range(corners[2, ])
    slicelen <- max(diff(xrng), diff(yrng)/panel.aspect)
    list(xlim = extend.limits(xrng, length = slicelen)/zoom, 
        ylim = extend.limits(yrng, length = panel.aspect * slicelen)/zoom, 
        dx = 1, dy = 1)
}


panel.stripplot <- function (x, y, jitter.data = FALSE, factor = 0.5, amount = NULL, 
    horizontal = TRUE, groups = NULL, ..., identifier = "stripplot") 
{
    if (!any(is.finite(x) & is.finite(y))) 
        return()
    panel.xyplot(x = x, y = y, jitter.x = jitter.data && !horizontal, 
        jitter.y = jitter.data && horizontal, factor = factor, 
        amount = amount, groups = groups, horizontal = horizontal, 
        ..., identifier = identifier)
}


prepanel.default.levelplot <- function (x, y, subscripts, ...) 
{
    pad <- lattice.getOption("axis.padding")$numeric
    if (length(subscripts) > 0) {
        x <- x[subscripts]
        y <- y[subscripts]
        if (!is.factor(x)) {
            ux <- sort(unique(x[is.finite(x)]))
            if ((ulen <- length(ux)) < 2) 
                xlim <- ux + c(-1, 1)
            else {
                diffs <- diff(as.numeric(ux))[c(1, ulen - 1)]
                xlim <- c(ux[1] - diffs[1]/2, ux[ulen] + diffs[2]/2)
            }
        }
        if (!is.factor(y)) {
            uy <- sort(unique(y[is.finite(y)]))
            if ((ulen <- length(uy)) < 2) 
                ylim <- uy + c(-1, 1)
            else {
                diffs <- diff(as.numeric(uy))[c(1, ulen - 1)]
                ylim <- c(uy[1] - diffs[1]/2, uy[ulen] + diffs[2]/2)
            }
        }
        list(xlim = if (!is.factor(x)) {
            extend.limits(xlim, prop = -pad/(1 + 2 * pad))
        } else levels(x), ylim = if (!is.factor(y)) {
            extend.limits(ylim, prop = -pad/(1 + 2 * pad))
        } else levels(y), dx = if (is.numeric(x)) length(ux) else 1, 
            dy = if (is.numeric(y)) length(uy) else 1)
    }
    else prepanel.null()
}


trellis.grobname <- function (name, type = c("", "panel", "strip", "strip.left", 
    "key", "colorkey"), group = 0, which.given = lattice.getStatus("current.which.given", 
    prefix = prefix), which.panel = lattice.getStatus("current.which.panel", 
    prefix = prefix), column = lattice.getStatus("current.focus.column", 
    prefix = prefix), row = lattice.getStatus("current.focus.row", 
    prefix = prefix), prefix = lattice.getStatus("current.prefix")) 
{
    stripname <- function(striplab, name, column, row, which.given, 
        which.panel) {
        if (length(which.panel) > 1) {
            paste(name, "given", which.given, striplab, column, 
                row, sep = ".")
        }
        else {
            paste(name, striplab, column, row, sep = ".")
        }
    }
    if (group > 0) 
        name <- paste(name, "group", group, sep = ".")
    paste(prefix, switch(type, panel = paste(name, type, column, 
        row, sep = "."), strip = , strip.left = stripname(type, 
        name, column, row, which.given, which.panel), key = , 
        colorkey = paste(type, name, sep = "."), name), sep = ".")
}


canonical.theme <- function (name = .Device, color = name != "postscript") 
{
    if (color) {
        can.col <- if (name %in% c("windows", "X11")) 
            c("#000000", "#00ffff", "#ff00ff", "#00ff00", "#ff7f00", 
                "#007eff", "#ffff00", "#ff0000", "#c6ffff", "#ffc3ff", 
                "#c8ffc8", "#ffd18f", "#a9e2ff", "#ffffc3", "#ff8c8a", 
                "#aaaaaa", "#909090")
        else if (name %in% c("postscript", "pdf", "xfig")) 
            c("#000000", "#00ffff", "#ff00ff", "#00ff00", "#ff7f00", 
                "#0080ff", "#ffff00", "#ff0000", "#ccffff", "#ffccff", 
                "#ccffcc", "#ffe5cc", "#cce6ff", "#ffffcc", "#ffcccc", 
                "#e6e6e6", "transparent")
        else c("#000000", "#00FFFF", "#FF00FF", "#00FF00", "#FF7F00", 
            "#007EFF", "#FFFF00", "#FF0000", "#C6FFFF", "#FFC3FF", 
            "#C8FFC8", "#FFD18F", "#A9E2FF", "#FFFFC3", "#FF8C8A", 
            "#AAAAAA", "#909090")
    }
    else can.col <- c("#000000", "#999999", "#4C4C4C", "#E6E6E6", 
        "#F2F2F2", "#B2B2B2", "#000000", "#030303", "#050505", 
        "#080808", "#0A0A0A", "#0D0D0D", "#0F0F0F", "#121212", 
        "#151515", "#AAAAAA", "transparent")
    ans <- list(grid.pars = list(), fontsize = list(text = 12, 
        points = 8), background = list(alpha = 1, col = can.col[17]), 
        panel.background = list(col = "transparent"), clip = list(panel = "on", 
            strip = "on"), add.line = list(alpha = 1, col = can.col[1], 
            lty = 1, lwd = 1), add.text = list(alpha = 1, cex = 1, 
            col = can.col[1], font = 1, lineheight = 1.2), plot.polygon = list(alpha = 1, 
            col = can.col[2], border = "black", lty = 1, lwd = 1), 
        box.dot = list(alpha = 1, col = can.col[1], cex = 1, 
            font = 1, pch = 16), box.rectangle = list(alpha = 1, 
            col = can.col[2], fill = "transparent", lty = 1, 
            lwd = 1), box.umbrella = list(alpha = 1, col = can.col[2], 
            lty = 2, lwd = 1), dot.line = list(alpha = 1, col = can.col[16], 
            lty = 1, lwd = 1), dot.symbol = list(alpha = 1, cex = 0.8, 
            col = can.col[2], font = 1, pch = 16), plot.line = list(alpha = 1, 
            col = can.col[2], lty = 1, lwd = 1), plot.symbol = list(alpha = 1, 
            cex = 0.8, col = can.col[2], font = 1, pch = 1, fill = "transparent"), 
        reference.line = list(alpha = 1, col = can.col[16], lty = 1, 
            lwd = 1), strip.background = list(alpha = 1, col = can.col[c(12, 
            11, 9, 13, 10, 15, 14)]), strip.shingle = list(alpha = 1, 
            col = can.col[c(5, 4, 2, 6, 3, 8, 7)]), strip.border = list(alpha = 1, 
            col = rep(can.col[1], 7), lty = rep(1, 7), lwd = rep(1, 
                7)), superpose.line = list(alpha = 1, col = can.col[2:8], 
            lty = rep(1, 7), lwd = rep(1, 7)), superpose.symbol = list(alpha = rep(1, 
            7), cex = rep(0.8, 7), col = can.col[2:8], fill = lower.saturation(can.col[2:8]), 
            font = rep(1, 7), pch = rep(1, 7)), superpose.polygon = list(alpha = rep(1, 
            7), col = lower.saturation(can.col[2:8]), border = rep("black", 
            7), lty = rep(1, 7), lwd = rep(1, 7)), regions = list(alpha = 1, 
            col = rev(cm.colors(100))), shade.colors = list(alpha = 1, 
            palette = function(irr, ref, height, saturation = 0.9) {
                hsv(h = height, s = 1 - saturation * (1 - (1 - 
                  ref)^0.5), v = irr)
            }), axis.line = list(alpha = 1, col = can.col[1], 
            lty = 1, lwd = 1), axis.text = list(alpha = 1, cex = 0.8, 
            col = can.col[1], font = 1, lineheight = 1), axis.components = list(left = list(tck = 1, 
            pad1 = 1, pad2 = 1), top = list(tck = 1, pad1 = 1, 
            pad2 = 1), right = list(tck = 1, pad1 = 1, pad2 = 1), 
            bottom = list(tck = 1, pad1 = 1, pad2 = 1)), layout.heights = list(top.padding = 1, 
            main = 1, main.key.padding = 1, key.top = 1, xlab.top = 1, 
            key.axis.padding = 1, axis.top = 1, strip = 1, panel = 1, 
            axis.panel = 1, between = 1, axis.bottom = 1, axis.xlab.padding = 1, 
            xlab = 1, xlab.key.padding = 0, key.bottom = 1, key.sub.padding = 1, 
            sub = 1, bottom.padding = 1), layout.widths = list(left.padding = 1, 
            key.left = 1, key.ylab.padding = 0, ylab = 1, ylab.axis.padding = 1, 
            axis.left = 1, axis.panel = 1, strip.left = 1, panel = 1, 
            between = 1, axis.right = 1, axis.key.padding = 1, 
            ylab.right = 1, key.right = 1, right.padding = 1), 
        box.3d = list(alpha = 1, col = can.col[1], lty = 1, lwd = 1), 
        par.xlab.text = list(alpha = 1, cex = 1, col = can.col[1], 
            font = 1, lineheight = 1), par.ylab.text = list(alpha = 1, 
            cex = 1, col = can.col[1], font = 1, lineheight = 1), 
        par.zlab.text = list(alpha = 1, cex = 1, col = can.col[1], 
            font = 1, lineheight = 1), par.main.text = list(alpha = 1, 
            cex = 1.2, col = can.col[1], font = 2, lineheight = 1), 
        par.sub.text = list(alpha = 1, cex = 1, col = can.col[1], 
            font = 2, lineheight = 1))
    if (color) {
        if (name == "postscript" || name == "pdf") {
            ans$plot.symbol$col <- can.col[6]
            ans$plot.line$col <- can.col[6]
            ans$dot.symbol$col <- can.col[6]
            ans$box.rectangle$col <- can.col[6]
            ans$box.umbrella$col <- can.col[6]
            ans$superpose.symbol$col <- c(can.col[c(6, 3, 4, 
                8)], "orange", "darkgreen", "brown")
            ans$superpose.symbol$col[c(3, 6)] <- ans$superpose.symbol$col[c(6, 
                3)]
            ans$superpose.line$col <- ans$superpose.symbol$col
        }
    }
    else {
        ans$plot.polygon$col <- can.col[5]
        ans$box.rectangle$col <- can.col[1]
        ans$box.umbrella$col <- can.col[1]
        ans$dot.line$col <- can.col[4]
        ans$dot.symbol$col <- can.col[1]
        ans$plot.line$col <- can.col[1]
        ans$plot.symbol$col <- can.col[1]
        ans$regions$col <- grey(seq(0.3^2.2, 0.9^2.2, length.out = 100)^(1/2.2))
        ans$shade.colors$palette <- function(irr, ref, height, 
            w = 0.5) grey(w * irr + (1 - w) * (1 - (1 - ref)^0.4))
        ans$strip.background$col <- can.col[rep(5, 7)]
        ans$strip.shingle$col <- can.col[rep(6, 7)]
        ans$superpose.line$col <- can.col[rep(1, 7)]
        ans$superpose.line$lty <- 1:7
        ans$superpose.symbol$col <- can.col[rep(1, 7)]
        ans$superpose.symbol$cex <- rep(0.7, 7)
        ans$superpose.symbol$pch <- c(1, 3, 6, 0, 5, 16, 17)
        ans$superpose.polygon$col <- grey((c(6, 12, 7, 11, 8, 
            10, 9)/15)^0.8)
    }
    ans
}


as.factorOrShingle <- function (x, subset = TRUE, drop = FALSE) 
{
    x <- if (is.numeric(x)) 
        as.shingle(x)
    else as.factor(x)
    x[subset, drop = drop]
}


panel.segments <- function (...) 
lsegments(...)


panel.rug <- function (x = NULL, y = NULL, regular = TRUE, start = if (regular) 0 else 0.97, 
    end = if (regular) 0.03 else 1, x.units = rep("npc", 2), 
    y.units = rep("npc", 2), col = plot.line$col, col.line = col, 
    lty = plot.line$lty, lwd = plot.line$lwd, alpha = plot.line$alpha, 
    ..., identifier = "rug") 
{
    if (!any(is.finite(x))) 
        x <- NULL
    if (!any(is.finite(y))) 
        y <- NULL
    plot.line <- trellis.par.get("plot.line")
    x.units <- rep(x.units, length.out = 2)
    y.units <- rep(y.units, length.out = 2)
    id <- identifier
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    if (!is.null(x)) {
        grid.segments(x0 = unit(x, "native"), x1 = unit(x, "native"), 
            y0 = unit(start, x.units[1]), y1 = unit(end, x.units[2]), 
            name = trellis.grobname(paste(id, "x", sep = "."), 
                type = "panel", group = group), gp = gpar(col = col.line, 
                lty = lty, lwd = lwd, alpha = alpha))
    }
    if (!is.null(y)) {
        grid.segments(y0 = unit(y, "native"), y1 = unit(y, "native"), 
            x0 = unit(start, y.units[1]), x1 = unit(end, y.units[2]), 
            name = trellis.grobname(paste(id, "y", sep = "."), 
                type = "panel", group = group), gp = gpar(col = col.line, 
                lty = lty, lwd = lwd, alpha = alpha))
    }
}


panel.bwplot <- function (x, y, box.ratio = 1, box.width = box.ratio/(1 + box.ratio), 
    horizontal = TRUE, pch = box.dot$pch, col = box.dot$col, 
    alpha = box.dot$alpha, cex = box.dot$cex, font = box.dot$font, 
    fontfamily = box.dot$fontfamily, fontface = box.dot$fontface, 
    fill = box.rectangle$fill, varwidth = FALSE, notch = FALSE, 
    notch.frac = 0.5, ..., levels.fos = if (horizontal) sort(unique(y)) else sort(unique(x)), 
    stats = boxplot.stats, coef = 1.5, do.out = TRUE, identifier = "bwplot") 
{
    if (all(is.na(x) | is.na(y))) 
        return()
    x <- as.numeric(x)
    y <- as.numeric(y)
    box.dot <- trellis.par.get("box.dot")
    box.rectangle <- trellis.par.get("box.rectangle")
    box.umbrella <- trellis.par.get("box.umbrella")
    plot.symbol <- trellis.par.get("plot.symbol")
    fontsize.points <- trellis.par.get("fontsize")$points
    if (!notch) 
        notch.frac <- 0
    if (horizontal) {
        blist <- tapply(x, factor(y, levels = levels.fos), stats, 
            coef = coef, do.out = do.out)
        blist.stats <- t(sapply(blist, "[[", "stats"))
        blist.out <- lapply(blist, "[[", "out")
        blist.height <- box.width
        if (varwidth) {
            maxn <- max(table(y))
            blist.n <- sapply(blist, "[[", "n")
            blist.height <- sqrt(blist.n/maxn) * blist.height
        }
        blist.conf <- if (notch) 
            t(sapply(blist, "[[", "conf"))
        else blist.stats[, c(2, 4), drop = FALSE]
        xbnd <- cbind(blist.stats[, 3], blist.conf[, 2], blist.stats[, 
            4], blist.stats[, 4], blist.conf[, 2], blist.stats[, 
            3], blist.conf[, 1], blist.stats[, 2], blist.stats[, 
            2], blist.conf[, 1], blist.stats[, 3])
        ytop <- levels.fos + blist.height/2
        ybot <- levels.fos - blist.height/2
        ybnd <- cbind(ytop - notch.frac * blist.height/2, ytop, 
            ytop, ybot, ybot, ybot + notch.frac * blist.height/2, 
            ybot, ybot, ytop, ytop, ytop - notch.frac * blist.height/2)
        xs <- cbind(xbnd, NA_real_)
        ys <- cbind(ybnd, NA_real_)
        panel.polygon(t(xs), t(ys), lwd = box.rectangle$lwd, 
            lty = box.rectangle$lty, col = fill, alpha = box.rectangle$alpha, 
            border = box.rectangle$col, identifier = paste(identifier, 
                "box", sep = "."))
        panel.segments(c(blist.stats[, 2], blist.stats[, 4]), 
            rep(levels.fos, 2), c(blist.stats[, 1], blist.stats[, 
                5]), rep(levels.fos, 2), col = box.umbrella$col, 
            alpha = box.umbrella$alpha, lwd = box.umbrella$lwd, 
            lty = box.umbrella$lty, identifier = paste(identifier, 
                "whisker", sep = "."))
        panel.segments(c(blist.stats[, 1], blist.stats[, 5]), 
            levels.fos - blist.height/2, c(blist.stats[, 1], 
                blist.stats[, 5]), levels.fos + blist.height/2, 
            col = box.umbrella$col, alpha = box.umbrella$alpha, 
            lwd = box.umbrella$lwd, lty = box.umbrella$lty, identifier = paste(identifier, 
                "cap", sep = "."))
        if (all(pch == "|")) {
            mult <- if (notch) 
                1 - notch.frac
            else 1
            panel.segments(blist.stats[, 3], levels.fos - mult * 
                blist.height/2, blist.stats[, 3], levels.fos + 
                mult * blist.height/2, lwd = box.rectangle$lwd, 
                lty = box.rectangle$lty, col = box.rectangle$col, 
                alpha = alpha, identifier = paste(identifier, 
                  "dot", sep = "."))
        }
        else {
            panel.points(x = blist.stats[, 3], y = levels.fos, 
                pch = pch, col = col, alpha = alpha, cex = cex, 
                fontfamily = fontfamily, fontface = chooseFace(fontface, 
                  font), fontsize = fontsize.points, identifier = paste(identifier, 
                  "dot", sep = "."))
        }
        panel.points(x = unlist(blist.out), y = rep(levels.fos, 
            sapply(blist.out, length)), pch = plot.symbol$pch, 
            col = plot.symbol$col, alpha = plot.symbol$alpha, 
            cex = plot.symbol$cex, fontfamily = plot.symbol$fontfamily, 
            fontface = chooseFace(plot.symbol$fontface, plot.symbol$font), 
            fontsize = fontsize.points, identifier = paste(identifier, 
                "outlier", sep = "."))
    }
    else {
        blist <- tapply(y, factor(x, levels = levels.fos), stats, 
            coef = coef, do.out = do.out)
        blist.stats <- t(sapply(blist, "[[", "stats"))
        blist.out <- lapply(blist, "[[", "out")
        blist.height <- box.width
        if (varwidth) {
            maxn <- max(table(x))
            blist.n <- sapply(blist, "[[", "n")
            blist.height <- sqrt(blist.n/maxn) * blist.height
        }
        blist.conf <- if (notch) 
            sapply(blist, "[[", "conf")
        else t(blist.stats[, c(2, 4), drop = FALSE])
        ybnd <- cbind(blist.stats[, 3], blist.conf[2, ], blist.stats[, 
            4], blist.stats[, 4], blist.conf[2, ], blist.stats[, 
            3], blist.conf[1, ], blist.stats[, 2], blist.stats[, 
            2], blist.conf[1, ], blist.stats[, 3])
        xleft <- levels.fos - blist.height/2
        xright <- levels.fos + blist.height/2
        xbnd <- cbind(xleft + notch.frac * blist.height/2, xleft, 
            xleft, xright, xright, xright - notch.frac * blist.height/2, 
            xright, xright, xleft, xleft, xleft + notch.frac * 
                blist.height/2)
        xs <- cbind(xbnd, NA_real_)
        ys <- cbind(ybnd, NA_real_)
        panel.polygon(t(xs), t(ys), lwd = box.rectangle$lwd, 
            lty = box.rectangle$lty, col = fill, alpha = box.rectangle$alpha, 
            border = box.rectangle$col, identifier = paste(identifier, 
                "box", sep = "."))
        panel.segments(rep(levels.fos, 2), c(blist.stats[, 2], 
            blist.stats[, 4]), rep(levels.fos, 2), c(blist.stats[, 
            1], blist.stats[, 5]), col = box.umbrella$col, alpha = box.umbrella$alpha, 
            lwd = box.umbrella$lwd, lty = box.umbrella$lty, identifier = paste(identifier, 
                "whisker", sep = "."))
        panel.segments(levels.fos - blist.height/2, c(blist.stats[, 
            1], blist.stats[, 5]), levels.fos + blist.height/2, 
            c(blist.stats[, 1], blist.stats[, 5]), col = box.umbrella$col, 
            alpha = box.umbrella$alpha, lwd = box.umbrella$lwd, 
            lty = box.umbrella$lty, identifier = paste(identifier, 
                "cap", sep = "."))
        if (all(pch == "|")) {
            mult <- if (notch) 
                1 - notch.frac
            else 1
            panel.segments(levels.fos - mult * blist.height/2, 
                blist.stats[, 3], levels.fos + mult * blist.height/2, 
                blist.stats[, 3], lwd = box.rectangle$lwd, lty = box.rectangle$lty, 
                col = box.rectangle$col, alpha = alpha, identifier = paste(identifier, 
                  "dot", sep = "."))
        }
        else {
            panel.points(x = levels.fos, y = blist.stats[, 3], 
                pch = pch, col = col, alpha = alpha, cex = cex, 
                fontfamily = fontfamily, fontface = chooseFace(fontface, 
                  font), fontsize = fontsize.points, identifier = paste(identifier, 
                  "dot", sep = "."))
        }
        panel.points(x = rep(levels.fos, sapply(blist.out, length)), 
            y = unlist(blist.out), pch = plot.symbol$pch, col = plot.symbol$col, 
            alpha = plot.symbol$alpha, cex = plot.symbol$cex, 
            fontfamily = plot.symbol$fontfamily, fontface = chooseFace(plot.symbol$fontface, 
                plot.symbol$font), fontsize = fontsize.points, 
            identifier = paste(identifier, "outlier", sep = "."))
    }
}


panel.refline <- function (...) 
panel.abline(..., reference = TRUE)


show.settings <- function (x = NULL) 
{
    old.settings <- trellis.par.get()
    on.exit(trellis.par.set(old.settings))
    if (!is.null(x)) 
        trellis.par.set(x)
    theme <- trellis.par.get()
    d <- c("superpose.symbol", "superpose.line", "strip.background", 
        "strip.shingle", "dot.[symbol, line]", "box.[dot, rectangle, umbrella]", 
        "add.[line, text]", "reference.line", "plot.[symbol, line]", 
        "plot.shingle[plot.polygon]", "histogram[plot.polygon]", 
        "barchart[plot.polygon]", "superpose.polygon", "regions")
    d <- factor(d, levels = d)
    par.box <- trellis.par.get("axis.line")
    panel.box <- function() {
        panel.fill(col = "transparent", border = adjustcolor(par.box$col, 
            par.box$alpha), lty = par.box$lty, lwd = par.box$lwd)
    }
    xyplot(d ~ d | d, prepanel = function(x, y) {
        list(ylim = c(0, 1), xlim = as.character(x), xat = 1)
    }, panel = function(x, y) {
        cpl <- current.panel.limits()
        rsx <- function(u) {
            cpl$xlim[1] + u * diff(cpl$xlim)
        }
        switch(as.character(x), superpose.symbol = {
            superpose.symbol <- trellis.par.get("superpose.symbol")
            len <- max(2, sapply(superpose.symbol, length))
            panel.superpose(x = rep(rsx(ppoints(len)), len), 
                y = rep(ppoints(len), each = len), groups = gl(len, 
                  len), subscripts = 1:(len * len))
        }, superpose.line = {
            superpose.line <- trellis.par.get("superpose.line")
            len <- max(2, sapply(superpose.line, length))
            panel.superpose(x = rep(rsx(c(0, 1)), len), y = rep(ppoints(1:len), 
                each = 2), groups = gl(len, 2), subscripts = 1:(2 * 
                len), type = "l")
        }, strip.background = {
            strip.background <- trellis.par.get("strip.background")
            strip.border <- trellis.par.get("strip.border")
            len <- max(sapply(strip.background, length), sapply(strip.border, 
                length))
            panel.rect(y = ppoints(len), height = 0.5/len, xleft = cpl$xlim[1], 
                xright = cpl$xlim[2], col = adjustcolor(strip.background$col, 
                  strip.background$alpha), border = strip.border$col, 
                lty = strip.border$lty, lwd = strip.border$lwd)
        }, strip.shingle = {
            strip.shingle <- trellis.par.get("strip.shingle")
            len <- max(sapply(strip.shingle, length))
            panel.rect(y = ppoints(len), height = 0.5/len, xleft = cpl$xlim[1], 
                xright = cpl$xlim[2], col = adjustcolor(strip.shingle$col, 
                  strip.shingle$alpha), border = "transparent")
        }, `dot.[symbol, line]` = {
            panel.dotplot(x = rsx(ppoints(5, a = 0)), y = ppoints(5, 
                a = 0))
            panel.box()
        }, `box.[dot, rectangle, umbrella]` = {
            panel.bwplot(x = rsx(ppoints(5)), y = rep(0.5, 5), 
                box.width = 0.15)
            panel.box()
        }, `add.[line, text]` = {
            add.line <- trellis.par.get("add.line")
            xx <- seq(0.1, 0.9, length.out = 50)
            yy <- 0.5 + 0.45 * sin(0.1 + 11 * xx)
            panel.lines(x = rsx(xx), y = yy, col = add.line$col, 
                lty = add.line$lty, lwd = add.line$lwd)
            panel.text(labels = c("Hello", "World"), x = rsx(c(0.25, 
                0.75)), y = c(0.25, 0.75))
            panel.box()
        }, reference.line = {
            panel.grid()
            panel.box()
        }, `plot.[symbol, line]` = {
            xx <- seq(0.1, 0.9, length.out = 20)
            yy <- 0.5 + 0.4 * sin(0.1 + 11 * xx)
            panel.xyplot(x = rsx(xx + 0.05), y = yy + 0.01, type = "l")
            panel.xyplot(x = rsx(xx - 0.05), y = yy - 0.01)
            panel.box()
        }, `plot.shingle[plot.polygon]` = {
            xx <- seq(0.1, 0.4, length.out = 5)
            yy <- ppoints(5)
            panel.barchart(x = rsx(xx + 0.5), y = yy, origin = rsx(xx), 
                reference = FALSE, horizontal = TRUE, box.width = 1/10)
            panel.box()
        }, `histogram[plot.polygon]` = {
            xx <- ppoints(7, 0)
            panel.barchart(x = rsx(xx), y = (2:8)/9, horizontal = FALSE, 
                origin = 1/18, box.width = diff(rsx(xx))[1], 
                reference = FALSE)
            panel.box()
        }, `barchart[plot.polygon]` = {
            xx <- ppoints(6)
            panel.barchart(x = rev(rsx(xx)), y = xx, origin = cpl$xlim[1], 
                box.width = 1/12)
            panel.box()
        }, superpose.polygon = {
            superpose.polygon <- trellis.par.get("superpose.polygon")
            len <- max(2, sapply(superpose.polygon, length))
            xx <- ppoints(len)
            panel.barchart(x = rsx(rev(xx)), y = rep(0.5, len), 
                groups = gl(len, 1), subscripts = seq_len(len), 
                stack = FALSE, box.width = 0.9)
            panel.box()
        }, regions = {
            panel.levelplot(x = do.breaks(cpl$xlim, 98), y = rep(0.5, 
                99), z = 1:99 + 0.5, at = 1:100, region = TRUE, 
                subscripts = 1:99)
            panel.box()
        })
    }, par.settings = modifyList(theme, list(axis.line = list(col = "transparent"), 
        clip = list(panel = "off"))), as.table = TRUE, strip = FALSE, 
        xlab = "", ylab = "", between = list(x = 1, y = 0.5), 
        scales = list(relation = "free", y = list(draw = FALSE, 
            axs = "i"), x = list(tck = 0, axs = "r")))
}


panel.tmd.default <- function (x, y, groups = NULL, ..., identifier = "tmd") 
{
    panel.abline(h = 0)
    if (is.null(groups)) 
        panel.xyplot(x = (as.numeric(x) + as.numeric(y))/2, y = (as.numeric(y) - 
            as.numeric(x)), ..., identifier = identifier)
    else panel.superpose(x = (as.numeric(x) + as.numeric(y))/2, 
        y = (as.numeric(y) - as.numeric(x)), groups = groups, 
        ...)
}


oneway <- function (formula, data, location = mean, spread = function(x) sqrt(var(x))) 
{
    if (missing(data)) 
        data <- sys.frame(sys.parent())
    form <- latticeParseFormula(formula, data)
    y <- form$left
    x <- form$right
    if (!is.shingle(x)) 
        x <- as.factor(x)
    is.f.x <- is.factor(x)
    num.l.x <- nlevels(x)
    foo <- list()
    if (is.f.x) {
        foo$location <- if (is.function(location)) 
            as.vector(tapply(X = y, INDEX = list(x), FUN = location))
        else rep(location, num.l.x)
        foo$spread <- if (is.function(spread)) 
            as.vector(tapply(X = y, INDEX = list(x), FUN = spread))
        else rep(spread, num.l.x)
        foo$fitted.values <- numeric(length(y))
        sc <- numeric(length(y))
        for (i in seq_along(y)) {
            foo$fitted.values[i] <- foo$location[as.numeric(x)[i]]
            sc[i] <- foo$spread[as.numeric(x)[i]]
        }
        foo$residuals <- y - foo$fitted.values
        foo$scaled.residuals <- foo$residuals/sc
    }
    else stop("x must be (coercible to be) a factor")
    foo
}


trellis.last.object <- function (..., prefix = lattice.getStatus("current.prefix")) 
{
    if (!lattice.getStatus("current.plot.saved", prefix = prefix)) {
        warning("Requested 'trellis' object was not saved")
        return(invisible(NULL))
    }
    ans <- lattice.getStatus("last.object", prefix = prefix)
    update(ans, ...)
}


panel.brush.splom <- function (threshold = 18, verbose = getOption("verbose"), ...) 
{
    ans <- numeric(0)
    repeat {
        new <- splom.linkPoint(threshold = threshold, verbose = verbose, 
            ...)
        if (is.null(new)) 
            break
        else ans[length(ans) + 1] <- new
    }
    ans
}


panel.histogram <- function (x, breaks, equal.widths = TRUE, type = "density", nint = round(log2(length(x)) + 
    1), alpha = plot.polygon$alpha, col = plot.polygon$col, border = plot.polygon$border, 
    lty = plot.polygon$lty, lwd = plot.polygon$lwd, ..., identifier = "histogram") 
{
    plot.polygon <- trellis.par.get("plot.polygon")
    xscale <- current.panel.limits()$xlim
    panel.lines(x = xscale[1] + diff(xscale) * c(0.05, 0.95), 
        y = c(0, 0), col = border, lty = lty, lwd = lwd, alpha = alpha, 
        identifier = paste(identifier, "baseline", sep = "."))
    if (length(x) > 0) {
        if (is.null(breaks)) {
            breaks <- if (is.factor(x)) 
                seq_len(1 + nlevels(x)) - 0.5
            else if (equal.widths) 
                do.breaks(range(x, finite = TRUE), nint)
            else quantile(x, 0:nint/nint, na.rm = TRUE)
        }
        h <- hist.constructor(x, breaks = breaks, ...)
        y <- switch(type, count = h$counts, percent = 100 * h$counts/length(x), 
            density = h$density)
        breaks <- h$breaks
        nb <- length(breaks)
        if (length(y) != nb - 1) 
            warning("problem with 'hist' computations")
        if (nb > 1) {
            panel.rect(x = breaks[-nb], y = 0, height = y, width = diff(breaks), 
                col = col, alpha = alpha, border = border, lty = lty, 
                lwd = lwd, just = c("left", "bottom"), identifier = identifier)
        }
    }
}


splom <- function (x, data, ...) 
UseMethod("splom")


panel.abline <- function (a = NULL, b = 0, h = NULL, v = NULL, reg = NULL, coef = NULL, 
    col, col.line = add.line$col, lty = add.line$lty, lwd = add.line$lwd, 
    alpha = add.line$alpha, type, ..., reference = FALSE, identifier = "abline") 
{
    add.line <- if (reference) 
        trellis.par.get("reference.line")
    else trellis.par.get("add.line")
    if (!missing(col) && missing(col.line)) 
        col.line <- col
    if (!is.null(reg)) {
        if (!is.null(a)) 
            warning("'a' is overridden by 'reg'")
        a <- reg
    }
    if (is.object(a) || is.list(a)) {
        p <- length(coefa <- as.vector(coef(a)))
        if (p > 2) 
            warning(gettextf("only using the first two of %d regression coefficients", 
                p))
        islm <- inherits(a, "lm")
        noInt <- if (islm) 
            !as.logical(attr(stats::terms(a), "intercept"))
        else p == 1
        if (noInt) {
            a <- 0
            b <- coefa[1]
        }
        else {
            a <- coefa[1]
            b <- if (p >= 2) 
                coefa[2]
            else 0
        }
    }
    if (!is.null(coef)) {
        if (!is.null(a)) 
            warning("'a' and 'b' are overridden by 'coef'")
        a <- coef[1]
        b <- coef[2]
    }
    id <- identifier
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    if (!is.null(a)) {
        coeff <- c(a, b)
        cpl <- current.panel.limits()
        xx <- cpl$xlim
        yy <- cpl$ylim
        sign.dist.from.line <- function(x, y) {
            as.integer(sign(y - coeff[1] - coeff[2] * x))
        }
        sign.corners <- with(cpl, sign.dist.from.line(c(xlim[1], 
            xlim[2], xlim[2], xlim[1]), c(ylim[1], ylim[1], ylim[2], 
            ylim[2])))
        A <- prod(sign.corners[c(1, 2)]) <= 0
        B <- prod(sign.corners[c(2, 3)]) <= 0
        C <- prod(sign.corners[c(3, 4)]) <= 0
        D <- prod(sign.corners[c(4, 1)]) <= 0
        yfun <- function(x) coeff[1] + coeff[2] * x
        xfun <- function(y) (y - coeff[1])/coeff[2]
        drawfun <- function(x0, y0, x1, y1, ...) {
            panel.segments(x0, y0, x1, y1, col = col.line, lty = lty, 
                lwd = lwd, alpha = alpha, ..., identifier = id)
        }
        if (D && B) 
            drawfun(xx[1], yfun(xx[1]), xx[2], yfun(xx[2]), ...)
        else if (D && C) 
            drawfun(xx[1], yfun(xx[1]), xfun(yy[2]), yy[2], ...)
        else if (A && B) 
            drawfun(xfun(yy[1]), yy[1], xx[2], yfun(xx[2]), ...)
        else if (A && C) 
            drawfun(xfun(yy[1]), yy[1], xfun(yy[2]), yy[2], ...)
        else if (B && C) 
            drawfun(xfun(yy[1]), yy[1], xfun(yy[2]), yy[2], ...)
        else if (A && D) 
            drawfun(xx[1], yfun(xx[1]), xfun(yy[1]), yy[1], ...)
    }
    if (length(h <- as.numeric(h)) > 0) 
        grid.segments(y0 = h, y1 = h, default.units = "native", 
            name = trellis.grobname(paste(identifier, "h", sep = "."), 
                type = "panel", group = group), gp = gpar(col = col.line, 
                lty = lty, lwd = lwd, alpha = alpha))
    if (length(as.numeric(v)) > 0) 
        grid.segments(x0 = v, x1 = v, default.units = "native", 
            name = trellis.grobname(paste(identifier, "v", sep = "."), 
                type = "panel", group = group), gp = gpar(col = col.line, 
                lty = lty, lwd = lwd, alpha = alpha))
    invisible()
}


lattice.getOption <- function (name) 
{
    get("lattice.options", envir = .LatticeEnv)[[name]]
}


panel.error <- function (e) 
{
    grid.text(gettextf("Error using packet %g\n%s", panel.number(), 
        conditionMessage(e)))
}


current.panel.limits <- function (unit = "native") 
{
    list(xlim = convertX(unit(c(0, 1), "npc"), unit, valueOnly = TRUE), 
        ylim = convertY(unit(c(0, 1), "npc"), unit, valueOnly = TRUE))
}


trellis.par.set <- function (name, value, ..., theme, warn = TRUE, strict = FALSE) 
{
    if (is.null(dev.list())) {
        trellis.device()
        if (warn) 
            warning("Note: The default device has been opened to honour attempt to modify trellis settings")
    }
    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    if (is.null(lattice.theme[[.Device]])) {
        trellis.device(device = .Device, new = FALSE)
        lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    }
    if (missing(theme)) {
        if (!missing(value)) {
            theme <- list(value)
            names(theme) <- name
        }
        else if (!missing(name) && is.list(name)) {
            theme <- name
        }
        else theme <- list(...)
    }
    else {
        if (is.character(theme)) 
            theme <- get(theme)
        if (is.function(theme)) 
            theme <- theme()
        if (!is.list(theme)) {
            warning("Invalid 'theme' specified")
            theme <- NULL
        }
    }
    if (strict) {
        if (strict > 1L) 
            lattice.theme[[.Device]] <- theme
        else lattice.theme[[.Device]][names(theme)] <- theme
    }
    else lattice.theme[[.Device]] <- updateList(lattice.theme[[.Device]], 
        theme)
    assign("lattice.theme", lattice.theme, envir = .LatticeEnv)
    invisible()
}


parallelplot <- function (x, data, ...) 
UseMethod("parallelplot")


simpleTheme <- function (col, alpha, cex, pch, lty, lwd, font, fill, border, 
    col.points, col.line, alpha.points, alpha.line) 
{
    ans <- list(plot.symbol = list(), plot.line = list(), plot.polygon = list(), 
        superpose.symbol = list(), superpose.line = list(), superpose.polygon = list())
    setValue <- function(value, name, targets) {
        for (t in targets) ans[[t]][[name]] <<- value
    }
    if (!missing(col)) 
        setValue(col, "col", 1:6)
    if (!missing(alpha)) 
        setValue(alpha, "alpha", 1:6)
    if (!missing(cex)) 
        setValue(cex, "cex", c(1, 4))
    if (!missing(pch)) 
        setValue(pch, "pch", c(1, 4))
    if (!missing(lty)) 
        setValue(lty, "lty", 1:6)
    if (!missing(lwd)) 
        setValue(lwd, "lwd", 1:6)
    if (!missing(font)) 
        setValue(font, "font", c(1, 4))
    if (!missing(fill)) 
        setValue(fill, "fill", c(1, 3, 4, 6))
    if (!missing(border)) 
        setValue(border, "border", c(3, 6))
    if (!missing(col.points)) 
        setValue(col.points, "col", c(1, 4))
    if (!missing(col.line)) 
        setValue(col.line, "col", c(2, 5))
    if (!missing(alpha.points)) 
        setValue(alpha.points, "alpha", c(1, 4))
    if (!missing(alpha.line)) 
        setValue(alpha.line, "alpha", c(2, 5))
    for (nm in c("plot.symbol", "plot.line", "plot.polygon")) ans[[nm]] <- lapply(ans[[nm]], 
        head, 1)
    ans
}


trellis.device <- function (device = getOption("device"), color = !(dev.name == 
    "postscript"), theme = lattice.getOption("default.theme"), 
    new = TRUE, retain = FALSE, ...) 
{
    if (is.character(device)) {
        if (new || is.null(dev.list())) {
            device.call <- try(get(device), silent = TRUE)
            if (inherits(device.call, "try-error")) 
                device.call <- try(utils::getFromNamespace(device, 
                  "grDevices"), silent = TRUE)
            if (inherits(device.call, "try-error")) 
                stop(gettextf("Could not find device function '%s'", 
                  device))
        }
        dev.name <- device
    }
    else {
        device.call <- device
        dev.name <- deparse(substitute(device))
    }
    if ("bg" %in% names(list(...))) 
        warning("'trellis.device' has changed, 'bg' may not be doing what you think it is")
    if (new || is.null(dev.list())) {
        device.call(...)
        lattice.setStatus(print.more = FALSE)
    }
    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    if (!(.Device %in% names(lattice.theme))) {
        lattice.theme[[.Device]] <- canonical.theme(name = "pdf", 
            color = color)
        assign("lattice.theme", lattice.theme, envir = .LatticeEnv)
    }
    if (!retain) 
        trellis.par.set(canonical.theme(name = "pdf", color = color))
    if (!is.null(theme) && !is.list(theme)) {
        if (is.character(theme)) 
            theme <- get(theme)
        if (is.function(theme)) 
            theme <- theme()
        if (!is.list(theme)) {
            warning("Invalid 'theme' specified")
            theme <- NULL
        }
    }
    if (!is.null(theme)) 
        trellis.par.set(theme)
    return(invisible())
}


strip.custom <- function (...) 
{
    args <- list(...)
    function(...) {
        dots <- list(...)
        do.call("strip.default", updateList(dots, args))
    }
}


simpleKey <- function (text, points = TRUE, rectangles = FALSE, lines = FALSE, 
    col = add.text$col, cex = add.text$cex, alpha = add.text$alpha, 
    font = add.text$font, fontface = add.text$fontface, fontfamily = add.text$fontfamily, 
    lineheight = add.text$lineheight, ...) 
{
    add.text <- trellis.par.get("add.text")
    foo <- seq_along(text)
    ans <- list(text = list(lab = text), col = col, cex = cex, 
        alpha = alpha, font = font, fontface = fontface, fontfamily = fontfamily, 
        ...)
    if (points) 
        ans$points <- Rows(trellis.par.get("superpose.symbol"), 
            foo)
    if (rectangles) 
        ans$rectangles <- Rows(trellis.par.get("superpose.polygon"), 
            foo)
    if (lines) 
        ans$lines <- updateList(Rows(trellis.par.get("superpose.symbol"), 
            foo), Rows(trellis.par.get("superpose.line"), foo))
    ans
}


make.groups <- function (...) 
{
    tmp <- list(...)
    nms <- as.character(substitute(list(...)))[-1]
    if (is.null(names(tmp))) 
        names(tmp) <- nms
    else {
        unnamed <- names(tmp) == ""
        names(tmp)[unnamed] <- nms[unnamed]
    }
    if (all(sapply(tmp, is.data.frame))) {
        cbind(do.call(rbind, tmp), which = rep(gl(length(tmp), 
            1, labels = names(tmp)), sapply(tmp, nrow)))
    }
    else data.frame(data = unlist(tmp), which = rep(gl(length(tmp), 
        1, labels = names(tmp)), sapply(tmp, length)))
}


prepanel.tmd.qqmath <- function (x, f.value = NULL, distribution = qnorm, qtype = 7, 
    groups = NULL, subscripts, ...) 
{
    if (!is.numeric(x)) 
        x <- as.numeric(x)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    getxx <- function(x, f.value = NULL, nobs = sum(!is.na(x))) {
        if (is.null(f.value)) 
            distribution(ppoints(nobs))
        else distribution(f.value(nobs))
    }
    getyy <- function(x, f.value = NULL, nobs = sum(!is.na(x))) {
        if (is.null(f.value)) 
            sort(x)
        else quantile(x, f.value(nobs), names = FALSE, type = qtype, 
            na.rm = TRUE)
    }
    if (!nobs) 
        prepanel.null()
    else if (!is.null(groups)) {
        sx <- split(x, groups[subscripts])
        xxlist <- lapply(sx, getxx, f.value = f.value)
        yylist <- lapply(sx, getyy, f.value = f.value)
        meanlist <- difflist <- vector(mode = "list", length = length(sx))
        for (i in seq_along(sx)) {
            meanlist[[i]] <- (xxlist[[i]] + yylist[[1]])/2
            difflist[[i]] <- (yylist[[i]] - xxlist[[1]])
        }
        list(xlim = range(unlist(meanlist), na.rm = TRUE), ylim = range(unlist(difflist), 
            na.rm = TRUE), dx = unlist(lapply(meanlist, diff)), 
            dy = unlist(lapply(difflist, diff)))
    }
    else {
        xx <- getxx(x, f.value, nobs)
        yy <- getyy(x, f.value, nobs)
        tmd.mean <- (xx + yy)/2
        tmd.diff <- (yy - xx)
        list(xlim = range(tmd.mean), ylim = range(tmd.diff), 
            dx = diff(tmd.mean), dy = diff(tmd.diff))
    }
}


panel.curve <- function (expr, from, to, n = 101, curve.type = "l", col = add.line$col, 
    lty = add.line$lty, lwd = add.line$lwd, type, ..., identifier = "curve") 
{
    add.line <- trellis.par.get("add.line")
    sexpr <- substitute(expr)
    if (is.name(sexpr)) {
        fcall <- paste(sexpr, "(x)")
        expr <- parse(text = fcall)
    }
    else {
        if (!(is.call(sexpr) && match("x", all.vars(sexpr), nomatch = 0))) 
            stop("'expr' must be a function or an expression containing 'x'")
        expr <- sexpr
    }
    lims <- current.panel.limits()$xlim
    if (missing(from)) 
        from <- min(lims)
    if (missing(to)) 
        to <- max(lims)
    x <- seq(from, to, length.out = n)
    y <- eval(expr, envir = list(x = x), enclos = parent.frame())
    if (hasGroupNumber()) 
        id <- paste(identifier, "group", list(...)$group.number, 
            sep = ".")
    else id <- identifier
    panel.lines(x, y, type = curve.type, col = col, lty = lty, 
        lwd = lwd, ..., identifier = id)
}


lattice.options <- function (...) 
{
    new <- list(...)
    if (is.null(names(new)) && length(new) == 1 && is.list(new[[1]])) 
        new <- new[[1]]
    old <- .LatticeEnv$lattice.options
    if (length(new) == 0) 
        return(old)
    nm <- names(new)
    if (is.null(nm)) 
        return(old[unlist(new)])
    isNamed <- nm != ""
    if (any(!isNamed)) 
        nm[!isNamed] <- unlist(new[!isNamed])
    retVal <- old[nm]
    names(retVal) <- nm
    nm <- nm[isNamed]
    .LatticeEnv$lattice.options <- updateList(old, new[nm])
    invisible(retVal)
}


panel.rect <- function (...) 
lrect(...)


prepanel.tmd.default <- function (x, y, ...) 
{
    prepanel.default.xyplot(x = (as.numeric(x) + as.numeric(y))/2, 
        y = (as.numeric(y) - as.numeric(x)), ...)
}


histogram <- function (x, data, ...) 
UseMethod("histogram")


panel.average <- function (x, y, fun = mean, horizontal = TRUE, lwd = reference.line$lwd, 
    lty = reference.line$lty, col, col.line = reference.line$col, 
    type = "l", ..., identifier = "linejoin") 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    reference.line = trellis.par.get("reference.line")
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    if (horizontal) {
        vals <- unique(sort(y))
        yy <- seq_along(vals)
        xx <- numeric(length(yy))
        for (i in yy) xx[i] <- fun(x[y == vals[i]])
        panel.lines(xx, vals[yy], col = col.line, lty = lty, 
            lwd = lwd, ..., identifier = identifier)
    }
    else {
        vals <- unique(sort(x))
        xx <- seq_along(vals)
        yy <- numeric(length(xx))
        for (i in xx) yy[i] <- fun(y[x == vals[i]])
        panel.lines(vals[xx], yy, col = col.line, lty = lty, 
            lwd = lwd, ..., identifier = identifier)
    }
}


packet.panel.default <- function (layout, condlevels, page, row, column, skip, all.pages.skip = TRUE) 
{
    panels.per.page <- layout[1] * layout[2]
    panels.per.row <- layout[1]
    packet.order <- do.call(expand.grid, condlevels)
    if (all.pages.skip) {
        skip <- rep(skip, length.out = panels.per.page * page)
    }
    else {
        skip <- rep(skip, length.out = panels.per.page)
        skip <- rep(skip, page)
    }
    panel.number <- 1 + (page - 1) * panels.per.page + (row - 
        1) * panels.per.row + (column - 1)
    if (skip[panel.number]) 
        return(NULL)
    panel.number <- panel.number - sum(head(skip, panel.number))
    if (panel.number > nrow(packet.order)) 
        return(NULL)
    as.numeric(packet.order[panel.number, ])
}


draw.colorkey <- function (key, draw = FALSE, vp = NULL) 
{
    if (!is.list(key)) 
        stop("key must be a list")
    process.key <- function(col = regions$col, alpha = regions$alpha, 
        at, tick.number = 7, tck = 1, width = 2, height = 1, 
        space = "right", raster = FALSE, interpolate = FALSE, 
        axis.line = list(), axis.text = list(), ...) {
        regions <- trellis.par.get("regions")
        list(col = col, alpha = alpha, at = at, tick.number = tick.number, 
            tck = tck, width = width, height = height, space = space, 
            raster = raster, interpolate = interpolate, axis.line = axis.line, 
            axis.text = axis.text, ...)
    }
    key <- do.call(process.key, key)
    axis.line <- updateList(trellis.par.get("axis.line"), key$axis.line)
    axis.text <- updateList(trellis.par.get("axis.text"), key$axis.text)
    check.overlap <- TRUE
    key$at <- sort(key$at)
    numcol <- length(key$at) - 1
    key$col <- level.colors(x = seq_len(numcol) - 0.5, at = seq_len(numcol + 
        1) - 1, col.regions = key$col, colors = TRUE)
    atrange <- range(key$at, finite = TRUE)
    scat <- as.numeric(key$at)
    if (key$raster && !isTRUE(all.equal(diff(range(diff(scat))), 
        0))) 
        warning("'at' values are not equispaced; output may be wrong")
    reccentre <- (scat[-1] + scat[-length(scat)])/2
    recdim <- diff(scat)
    cex <- axis.text$cex
    col <- axis.text$col
    font <- axis.text$font
    fontfamily <- axis.text$fontfamily
    fontface <- axis.text$fontface
    lineheight <- axis.text$lineheight
    rot <- 0
    if (!is.null(key[["labels"]])) {
        key[["lab"]] <- key[["labels"]]
        key[["labels"]] <- NULL
        if (is.list(key[["lab"]]) && !is.null(key[["lab"]][["labels"]])) {
            key[["lab"]][["lab"]] <- key[["lab"]][["labels"]]
            key[["lab"]][["labels"]] <- NULL
        }
    }
    if (is.null(key$lab)) {
        at <- lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- format(at, trim = TRUE)
    }
    else if (is.characterOrExpression(key$lab) && length(key$lab) == 
        length(key$at)) {
        check.overlap <- FALSE
        at <- key$at
        labels <- key$lab
    }
    else if (is.list(key$lab)) {
        at <- if (!is.null(key$lab$at)) 
            key$lab$at
        else lpretty(atrange, key$tick.number)
        at <- at[at >= atrange[1] & at <= atrange[2]]
        labels <- if (!is.null(key$lab$lab)) {
            check.overlap <- FALSE
            key$lab$lab
        }
        else format(at, trim = TRUE)
        if (!is.null(key$lab$cex)) 
            cex <- key$lab$cex
        if (!is.null(key$lab$col)) 
            col <- key$lab$col
        if (!is.null(key$lab$font)) 
            font <- key$lab$font
        if (!is.null(key$lab$fontface)) 
            fontface <- key$lab$fontface
        if (!is.null(key$lab$fontfamily)) 
            fontfamily <- key$lab$fontfamily
        if (!is.null(key$lab$lineheight)) 
            lineheight <- key$lab$lineheight
        if (!is.null(key$lab$rot)) 
            rot <- key$lab$rot
    }
    else stop("malformed colorkey")
    labscat <- at
    do.labels <- (length(labscat) > 0)
    if (key$space == "right") {
        labelsGrob <- if (do.labels) 
            textGrob(label = labels, x = rep(0, length(labscat)), 
                y = labscat, vp = viewport(yscale = atrange), 
                default.units = "native", check.overlap = check.overlap, 
                just = if (rot == -90) 
                  c("center", "bottom")
                else c("left", "center"), rot = rot, name = trellis.grobname("labels", 
                  type = "colorkey"), gp = gpar(col = col, cex = cex, 
                  fontfamily = fontfamily, fontface = chooseFace(fontface, 
                    font), lineheight = lineheight))
        else nullGrob()
        heights.x <- c((1 - key$height)/2, key$height, (1 - key$height)/2)
        heights.units <- rep("null", 3)
        widths.x <- c(0.6 * key$width, do.labels * (0.3 + key$tck * 
            0.3), do.labels * 1)
        widths.units <- c("lines", "lines", "grobwidth")
        widths.data <- list(NULL, NULL, labelsGrob)
        key.layout <- grid.layout(nrow = 3, ncol = 3, heights = unit(heights.x, 
            heights.units), widths = unit(widths.x, widths.units, 
            data = widths.data), respect = TRUE)
        key.gf <- frameGrob(layout = key.layout, vp = vp, name = trellis.grobname("frame", 
            type = "colorkey"))
        if (key$raster) {
            key.gf <- placeGrob(key.gf, rasterGrob(matrix(rev(key$col), 
                ncol = 1), width = 1, height = 1, vp = viewport(clip = "on"), 
                name = trellis.grobname("raster", type = "colorkey"), 
                interpolate = key$interpolate), row = 2, col = 1)
        }
        else {
            key.gf <- placeGrob(key.gf, rectGrob(x = rep(0.5, 
                length(reccentre)), y = reccentre, default.units = "native", 
                vp = viewport(yscale = atrange), height = recdim, 
                name = trellis.grobname("image", type = "colorkey"), 
                gp = gpar(fill = key$col, col = "transparent", 
                  alpha = key$alpha)), row = 2, col = 1)
        }
        key.gf <- placeGrob(frame = key.gf, rectGrob(name = trellis.grobname("border", 
            type = "colorkey"), gp = gpar(col = axis.line$col, 
            lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, 
            fill = "transparent")), row = 2, col = 1)
        if (do.labels) {
            if (key$tck != 0) 
                key.gf <- placeGrob(frame = key.gf, segmentsGrob(x0 = rep(0, 
                  length(labscat)), y0 = labscat, x1 = rep(key$tck/(1 + 
                  key$tck), length(labscat)), y1 = labscat, vp = viewport(yscale = atrange), 
                  default.units = "native", name = trellis.grobname("ticks", 
                    type = "colorkey"), gp = gpar(col = axis.line$col, 
                    lty = axis.line$lty, lwd = axis.line$lwd)), 
                  row = 2, col = 2)
            key.gf <- placeGrob(key.gf, labelsGrob, row = 2, 
                col = 3)
        }
    }
    else if (key$space == "left") {
        labelsGrob <- if (do.labels) 
            textGrob(label = labels, x = rep(1, length(labscat)), 
                y = labscat, vp = viewport(yscale = atrange), 
                default.units = "native", check.overlap = check.overlap, 
                just = if (rot == 90) 
                  c("center", "bottom")
                else c("right", "center"), rot = rot, name = trellis.grobname("labels", 
                  type = "colorkey"), gp = gpar(col = col, cex = cex, 
                  fontfamily = fontfamily, fontface = chooseFace(fontface, 
                    font), lineheight = lineheight))
        else nullGrob()
        heights.x <- c((1 - key$height)/2, key$height, (1 - key$height)/2)
        heights.units <- rep("null", 3)
        widths.x <- c(do.labels * 1, do.labels * (0.3 + key$tck * 
            0.3), 0.6 * key$width)
        widths.units <- c("grobwidth", "lines", "lines")
        widths.data <- list(labelsGrob, NULL, NULL)
        key.layout <- grid.layout(nrow = 3, ncol = 3, heights = unit(heights.x, 
            heights.units), widths = unit(widths.x, widths.units, 
            data = widths.data), respect = TRUE)
        key.gf <- frameGrob(layout = key.layout, vp = vp, name = trellis.grobname("frame", 
            type = "colorkey"))
        if (key$raster) {
            key.gf <- placeGrob(key.gf, rasterGrob(matrix(rev(key$col), 
                ncol = 1), width = 1, height = 1, vp = viewport(clip = "on"), 
                name = trellis.grobname("raster", type = "colorkey"), 
                interpolate = key$interpolate), row = 2, col = 3)
        }
        else {
            key.gf <- placeGrob(key.gf, rectGrob(x = rep(0.5, 
                length(reccentre)), y = reccentre, default.units = "native", 
                vp = viewport(yscale = atrange), height = recdim, 
                name = trellis.grobname("image", type = "colorkey"), 
                gp = gpar(fill = key$col, col = "transparent", 
                  alpha = key$alpha)), row = 2, col = 3)
        }
        key.gf <- placeGrob(frame = key.gf, rectGrob(name = trellis.grobname("border", 
            type = "colorkey"), gp = gpar(col = axis.line$col, 
            lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, 
            fill = "transparent")), row = 2, col = 3)
        if (do.labels) {
            if (key$tck != 0) 
                key.gf <- placeGrob(frame = key.gf, segmentsGrob(x0 = rep(1, 
                  length(labscat)), y0 = labscat, x1 = rep(1 - 
                  key$tck/(1 + key$tck), length(labscat)), y1 = labscat, 
                  vp = viewport(yscale = atrange), default.units = "native", 
                  name = trellis.grobname("ticks", type = "colorkey"), 
                  gp = gpar(col = axis.line$col, lty = axis.line$lty, 
                    lwd = axis.line$lwd)), row = 2, col = 2)
            key.gf <- placeGrob(key.gf, labelsGrob, row = 2, 
                col = 1)
        }
    }
    else if (key$space == "top") {
        labelsGrob <- if (do.labels) 
            textGrob(label = labels, y = rep(0, length(labscat)), 
                x = labscat, vp = viewport(xscale = atrange), 
                default.units = "native", check.overlap = check.overlap, 
                just = if (rot == 0) 
                  c("center", "bottom")
                else c("left", "center"), rot = rot, name = trellis.grobname("labels", 
                  type = "colorkey"), gp = gpar(col = col, cex = cex, 
                  fontfamily = fontfamily, fontface = chooseFace(fontface, 
                    font), lineheight = lineheight))
        else nullGrob()
        widths.x <- c((1 - key$height)/2, key$height, (1 - key$height)/2)
        widths.units <- rep("null", 3)
        heights.x <- c(do.labels * 1, do.labels * (0.3 + key$tck * 
            0.3), 0.6 * key$width)
        heights.units <- c("grobheight", "lines", "lines")
        heights.data <- list(labelsGrob, NULL, NULL)
        key.layout <- grid.layout(nrow = 3, ncol = 3, heights = unit(heights.x, 
            heights.units, data = heights.data), widths = unit(widths.x, 
            widths.units), respect = TRUE)
        key.gf <- frameGrob(layout = key.layout, vp = vp, name = trellis.grobname("frame", 
            type = "colorkey"))
        if (key$raster) {
            key.gf <- placeGrob(key.gf, rasterGrob(matrix(key$col, 
                nrow = 1), width = 1, height = 1, vp = viewport(clip = "on"), 
                name = trellis.grobname("raster", type = "colorkey"), 
                interpolate = key$interpolate), row = 3, col = 2)
        }
        else {
            key.gf <- placeGrob(key.gf, rectGrob(y = rep(0.5, 
                length(reccentre)), x = reccentre, default.units = "native", 
                vp = viewport(xscale = atrange), width = recdim, 
                name = trellis.grobname("image", type = "colorkey"), 
                gp = gpar(fill = key$col, col = "transparent", 
                  alpha = key$alpha)), row = 3, col = 2)
        }
        key.gf <- placeGrob(frame = key.gf, rectGrob(name = trellis.grobname("border", 
            type = "colorkey"), gp = gpar(col = axis.line$col, 
            lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, 
            fill = "transparent")), row = 3, col = 2)
        if (do.labels) {
            if (key$tck != 0) 
                key.gf <- placeGrob(frame = key.gf, segmentsGrob(y0 = rep(0, 
                  length(labscat)), x0 = labscat, y1 = rep(key$tck/(1 + 
                  key$tck), length(labscat)), x1 = labscat, vp = viewport(xscale = atrange), 
                  default.units = "native", name = trellis.grobname("ticks", 
                    type = "colorkey"), gp = gpar(col = axis.line$col, 
                    lty = axis.line$lty, lwd = axis.line$lwd)), 
                  row = 2, col = 2)
            key.gf <- placeGrob(key.gf, labelsGrob, row = 1, 
                col = 2)
        }
    }
    else if (key$space == "bottom") {
        labelsGrob <- if (do.labels) 
            textGrob(label = labels, y = rep(1, length(labscat)), 
                x = labscat, vp = viewport(xscale = atrange), 
                default.units = "native", check.overlap = check.overlap, 
                just = if (rot == 0) 
                  c("center", "top")
                else c("right", "center"), rot = rot, name = trellis.grobname("labels", 
                  type = "colorkey"), gp = gpar(col = col, cex = cex, 
                  fontfamily = fontfamily, fontface = chooseFace(fontface, 
                    font), lineheight = lineheight))
        else nullGrob()
        widths.x <- c((1 - key$height)/2, key$height, (1 - key$height)/2)
        widths.units <- rep("null", 3)
        heights.x <- c(0.6 * key$width, do.labels * (0.3 + key$tck * 
            0.3), do.labels * 1)
        heights.units <- c("lines", "lines", "grobheight")
        heights.data <- list(NULL, NULL, labelsGrob)
        key.layout <- grid.layout(nrow = 3, ncol = 3, heights = unit(heights.x, 
            heights.units, data = heights.data), widths = unit(widths.x, 
            widths.units), respect = TRUE)
        key.gf <- frameGrob(layout = key.layout, vp = vp, name = trellis.grobname("frame", 
            type = "colorkey"))
        if (key$raster) {
            key.gf <- placeGrob(key.gf, rasterGrob(matrix(key$col, 
                nrow = 1), width = 1, height = 1, vp = viewport(clip = "on"), 
                name = trellis.grobname("raster", type = "colorkey"), 
                interpolate = key$interpolate), row = 1, col = 2)
        }
        else {
            key.gf <- placeGrob(key.gf, rectGrob(y = rep(0.5, 
                length(reccentre)), x = reccentre, default.units = "native", 
                vp = viewport(xscale = atrange), width = recdim, 
                name = trellis.grobname("image", type = "colorkey"), 
                gp = gpar(fill = key$col, col = "transparent", 
                  alpha = key$alpha)), row = 1, col = 2)
        }
        key.gf <- placeGrob(frame = key.gf, rectGrob(name = trellis.grobname("image", 
            type = "colorkey"), gp = gpar(col = axis.line$col, 
            lty = axis.line$lty, lwd = axis.line$lwd, alpha = axis.line$alpha, 
            fill = "transparent")), row = 1, col = 2)
        if (do.labels) {
            if (key$tck != 0) 
                key.gf <- placeGrob(frame = key.gf, segmentsGrob(y0 = rep(1, 
                  length(labscat)), x0 = labscat, y1 = rep(1 - 
                  key$tck/(1 + key$tck), length(labscat)), x1 = labscat, 
                  vp = viewport(xscale = atrange), default.units = "native", 
                  name = trellis.grobname("ticks", type = "colorkey"), 
                  gp = gpar(col = axis.line$col, lty = axis.line$lty, 
                    lwd = axis.line$lwd)), row = 2, col = 2)
            key.gf <- placeGrob(key.gf, labelsGrob, row = 3, 
                col = 2)
        }
    }
    if (draw) 
        grid.draw(key.gf)
    key.gf
}


panel.points <- function (...) 
lpoints(...)


qqmath <- function (x, data, ...) 
UseMethod("qqmath")


parallel <- function (x, data, ...) 
{
    .Deprecated("parallelplot")
    UseMethod("parallel")
}


panel.grid <- function (h = 3, v = 3, col, col.line = reference.line$col, lty = reference.line$lty, 
    lwd = reference.line$lwd, x = NULL, y = NULL, ..., identifier = "grid") 
{
    reference.line <- trellis.par.get("reference.line")
    if (!missing(col) && missing(col.line)) 
        col.line <- col
    h <- as.integer(h)
    v <- as.integer(v)
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    if (h > 0) 
        grid.segments(y0 = 1:h/(h + 1), y1 = 1:h/(h + 1), gp = gpar(col = col.line, 
            lty = lty, lwd = lwd), default.units = "npc", name = trellis.grobname(paste(identifier, 
            "h", sep = "."), type = "panel", group = group))
    if (v > 0) 
        grid.segments(x0 = 1:v/(v + 1), x1 = 1:v/(v + 1), gp = gpar(col = col.line, 
            lty = lty, lwd = lwd), default.units = "npc", name = trellis.grobname(paste(identifier, 
            "v", sep = "."), type = "panel", group = group))
    limits <- current.panel.limits()
    if (h < 0) {
        if (h == -1) 
            n <- 5
        else n <- -h
        scale <- limits$ylim
        if (!is.null(y)) {
            if (inherits(y, "factor")) 
                y <- as.character(y)
            mostattributes(scale) <- attributes(y)
        }
        at <- formattedTicksAndLabels(scale, n = n)$at
        at <- at[at > min(scale) & at < max(scale)]
        grid.segments(y0 = at, y1 = at, gp = gpar(col = col.line, 
            lty = lty, lwd = lwd), default.units = "native", 
            name = trellis.grobname(paste(identifier, "h", sep = "."), 
                type = "panel", group = group))
    }
    if (v < 0) {
        if (v == -1) 
            n <- 5
        else n <- -v
        scale <- limits$xlim
        if (!is.null(x)) {
            if (inherits(x, "factor")) 
                x <- as.character(y)
            mostattributes(scale) <- attributes(x)
        }
        at <- formattedTicksAndLabels(scale, n = n)$at
        at <- at[at > min(scale) & at < max(scale)]
        grid.segments(x0 = at, x1 = at, gp = gpar(col = col.line, 
            lty = lty, lwd = lwd), default.units = "native", 
            name = trellis.grobname(paste(identifier, "v", sep = "."), 
                type = "panel", group = group))
    }
}


trellis.vpname <- function (name = c("position", "split", "split.location", "toplevel", 
    "figure", "panel", "strip", "strip.left", "legend", "legend.region", 
    "main", "sub", "xlab", "ylab", "xlab.top", "ylab.right", 
    "page"), column = lattice.getStatus("current.focus.column", 
    prefix = prefix), row = lattice.getStatus("current.focus.row", 
    prefix = prefix), side = c("left", "top", "right", "bottom", 
    "inside"), clip.off = FALSE, prefix = lattice.getStatus("current.prefix")) 
{
    name <- match.arg(name)
    side <- match.arg(side)
    paste(prefix, switch(name, position = "position.vp", split = "split.vp", 
        split.location = "split.location.vp", toplevel = "toplevel.vp", 
        figure = "figure.vp", xlab = "xlab.vp", ylab = "ylab.vp", 
        main = "main.vp", sub = "sub.vp", xlab.top = "xlab.top.vp", 
        ylab.right = "ylab.right.vp", panel = if (clip.off) paste("panel", 
            column, row, "off", "vp", sep = ".") else paste("panel", 
            column, row, "vp", sep = "."), strip = if (clip.off) paste("strip", 
            column, row, "off", "vp", sep = ".") else paste("strip", 
            column, row, "vp", sep = "."), strip.left = if (clip.off) paste("strip.left", 
            column, row, "off", "vp", sep = ".") else paste("strip.left", 
            column, row, "vp", sep = "."), legend = paste("legend", 
            side, "vp", sep = "."), legend.region = "legend.region.vp"), 
        sep = ".")
}


qq <- function (x, data, ...) 
UseMethod("qq")


panel.pairs <- function (z, panel = lattice.getOption("panel.splom"), lower.panel = panel, 
    upper.panel = panel, diag.panel = "diag.panel.splom", as.matrix = FALSE, 
    groups = NULL, panel.subscripts, subscripts, pscales = 5, 
    prepanel.limits = scale.limits, varnames = colnames(z), varname.col = add.text$col, 
    varname.cex = add.text$cex, varname.font = add.text$font, 
    varname.fontfamily = add.text$fontfamily, varname.fontface = add.text$fontface, 
    axis.text.col = axis.text$col, axis.text.cex = axis.text$cex, 
    axis.text.font = axis.text$font, axis.text.fontfamily = axis.text$fontfamily, 
    axis.text.fontface = axis.text$fontface, axis.text.lineheight = axis.text$lineheight, 
    axis.line.col = axis.line$col, axis.line.lty = axis.line$lty, 
    axis.line.lwd = axis.line$lwd, axis.line.alpha = axis.line$alpha, 
    axis.line.tck = 1, ...) 
{
    lower.panel <- getFunctionOrName(lower.panel)
    upper.panel <- getFunctionOrName(upper.panel)
    diag.panel <- getFunctionOrName(diag.panel)
    add.text <- trellis.par.get("add.text")
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")
    n.var <- ncol(z)
    if (n.var == 0) 
        return()
    lim <- vector("list", length = n.var)
    for (i in seq_len(n.var)) lim[[i]] <- if (is.list(pscales) && 
        !is.null(pscales[[i]]$lim)) 
        pscales[[i]]$lim
    else prepanel.limits(z[, i])
    if (length(subscripts)) {
        draw <- is.list(pscales) || (is.numeric(pscales) && pscales != 
            0)
        splom.layout <- grid.layout(nrow = n.var, ncol = n.var)
        pushViewport(viewport(layout = splom.layout, name = "pairs"))
        for (i in 1:n.var) for (j in 1:n.var) {
            if (as.matrix) 
                pushViewport(viewport(layout.pos.row = i, layout.pos.col = j, 
                  name = paste("subpanel", j, i, sep = "."), 
                  clip = trellis.par.get("clip")$panel, xscale = as.numeric(extend.limits(lim[[j]])), 
                  yscale = as.numeric(extend.limits(lim[[i]]))))
            else pushViewport(viewport(layout.pos.row = n.var - 
                i + 1, layout.pos.col = j, name = paste("subpanel", 
                j, i, sep = "."), clip = trellis.par.get("clip")$panel, 
                xscale = as.numeric(extend.limits(lim[[j]])), 
                yscale = as.numeric(extend.limits(lim[[i]]))))
            if (i == j) {
                diag.panel(x = z[subscripts, j], varname = varnames[i], 
                  limits = lim[[i]], at = if (is.list(pscales)) 
                    pscales[[i]]$at
                  else NULL, labels = if (is.list(pscales)) 
                    pscales[[i]]$lab
                  else NULL, draw = draw, tick.number = if (is.numeric(pscales)) 
                    pscales
                  else 5, varname.col = varname.col, varname.cex = varname.cex, 
                  varname.font = varname.font, varname.fontfamily = varname.fontfamily, 
                  varname.fontface = varname.fontface, axis.text.col = axis.text.col, 
                  axis.text.cex = axis.text.cex, axis.text.font = axis.text.font, 
                  axis.text.fontfamily = axis.text.fontfamily, 
                  axis.text.fontface = axis.text.fontface, axis.text.lineheight = axis.text.lineheight, 
                  axis.line.col = axis.line.col, axis.line.lty = axis.line.lty, 
                  axis.line.lwd = axis.line.lwd, axis.line.alpha = axis.line.alpha, 
                  axis.line.tck = axis.line.tck, i = i, j = j, 
                  ...)
                grid.rect(name = trellis.grobname("pairs.border", 
                  type = "panel"), gp = gpar(col = axis.line.col, 
                  lty = axis.line.lty, lwd = axis.line.lwd, fill = "transparent"))
            }
            else {
                pargs <- if (!panel.subscripts) 
                  c(list(x = z[subscripts, j], y = z[subscripts, 
                    i]), list(...), list(i = i, j = j))
                else c(list(x = z[subscripts, j], y = z[subscripts, 
                  i], groups = groups, subscripts = subscripts), 
                  list(...), list(i = i, j = j))
                if (as.matrix) 
                  checkArgsAndCall(if (i > j) 
                    lower.panel
                  else upper.panel, pargs)
                else checkArgsAndCall(if (i < j) 
                  lower.panel
                else upper.panel, pargs)
                grid.rect(name = trellis.grobname("pairs.border", 
                  type = "panel"), gp = gpar(col = axis.line.col, 
                  lty = axis.line.lty, lwd = axis.line.lwd, fill = "transparent"))
            }
            upViewport()
        }
        upViewport()
    }
}


xyplot.ts <- function (x, data = NULL, screens = if (superpose) 1 else colnames(x), 
    ..., superpose = FALSE, cut = FALSE, type = "l", col = NULL, 
    lty = NULL, lwd = NULL, pch = NULL, cex = NULL, fill = NULL, 
    auto.key = superpose, panel = if (superpose) "panel.superpose" else "panel.superpose.plain", 
    par.settings = list(), layout = NULL, as.table = TRUE, xlab = "Time", 
    ylab = NULL, default.scales = list(y = list(relation = if (missing(cut)) "free" else "same"))) 
{
    if (NCOL(x) == 1) {
        if (missing(superpose)) 
            superpose <- TRUE
        if (missing(auto.key)) 
            auto.key <- FALSE
    }
    stopifnot(is.null(data))
    timex <- time(x)
    x <- as.matrix(x)
    if (is.null(colnames(x))) 
        colnames(x) <- paste("V", seq_len(NCOL(x)), sep = "")
    cn <- colnames(x)
    time <- NULL
    if (is.numeric(cut)) 
        cut <- list(number = cut)
    if (isTRUE(cut)) {
        timediff <- diff(timex)
        asp <- apply(x, 2, function(y) banking(timediff, diff(y)) * 
            diff(range(y))/diff(range(timex)))
        asp <- median(asp)
        nasp <- 1/(1:6)
        number <- which.min(abs(1 - (asp * 1:6)/nasp))
        cut <- list(number = number)
        if (number == 1) 
            cut <- FALSE
    }
    if (is.list(cut)) {
        ecargs <- list(x = timex)
        ecargs <- modifyList(ecargs, cut)
        time <- do.call(equal.count, ecargs)
        default.scales <- modifyList(list(x = list(relation = "sliced")), 
            default.scales)
    }
    screensgiven <- !missing(screens) && !is.numeric(screens)
    screens <- make.par.list(cn, screens, NROW(x), NCOL(x), 1)
    screens <- unlist(screens, use.names = FALSE)
    screens <- factor(screens, levels = unique(screens))
    screens <- rep(screens, length = NCOL(x))
    fac <- factor(rep(screens, each = NROW(x)))
    tt <- rep(timex, NCOL(x))
    fo <- if ((nlevels(fac) > 1) || screensgiven) {
        if (!is.null(time)) 
            x ~ tt | time * fac
        else x ~ tt | fac
    }
    else {
        if (!is.null(time)) 
            x ~ tt | time
        else x ~ tt
    }
    if (is.null(layout)) {
        npanels <- max(1, nlevels(fac)) * max(1, nlevels(time))
        nc <- ceiling(npanels/6)
        nr <- ceiling(npanels/nc)
        layout <- c(nc, nr)
    }
    if (is.logical(auto.key) && auto.key) 
        auto.key <- list()
    if (is.list(auto.key)) 
        auto.key <- modifyList(list(lines = TRUE, points = FALSE), 
            auto.key)
    needStyles <- (any(sapply(list(col, lty, lwd, pch, cex, fill), 
        length) > 1))
    needGroups <- ((length(unique(screens)) < NCOL(x)) || (needStyles) || 
        (is.list(type) && (length(type) > 1)))
    if (needGroups) 
        groups <- factor(col(x), labels = cn)
    else groups <- rep(factor(1), length(x))
    if (is.list(type)) 
        type <- make.par.list(cn, type, NROW(x), NCOL(x), "l")
    tmpcall <- quote(xyplot(fo, groups = groups, ..., panel = panel, 
        type = type, distribute.type = is.list(type), auto.key = auto.key, 
        par.settings = par.settings, layout = layout, as.table = as.table, 
        xlab = xlab, ylab = ylab, default.scales = default.scales))
    if (length(par.settings) > 0) {
        opar <- trellis.par.get()
        trellis.par.set(par.settings)
        on.exit(trellis.par.set(opar))
    }
    plot.line <- trellis.par.get("plot.line")
    plot.symbol <- trellis.par.get("plot.symbol")
    unlistIfSimple <- function(z) if (all(sapply(z, length) == 
        1)) 
        unlist(z)
    else z
    if (!is.null(col)) {
        col <- make.par.list(cn, col, NROW(x), NCOL(x), plot.line$col)
        tmpcall$col <- unlistIfSimple(col)
    }
    if (!is.null(lty)) {
        lty <- make.par.list(cn, lty, NROW(x), NCOL(x), plot.line$lty)
        tmpcall$lty <- unlistIfSimple(lty)
    }
    if (!is.null(lwd)) {
        lwd <- make.par.list(cn, lwd, NROW(x), NCOL(x), plot.line$lwd)
        tmpcall$lwd <- unlistIfSimple(lwd)
    }
    if (!is.null(pch)) {
        pch <- make.par.list(cn, pch, NROW(x), NCOL(x), plot.symbol$pch)
        tmpcall$pch <- unlistIfSimple(pch)
    }
    if (!is.null(cex)) {
        cex <- make.par.list(cn, cex, NROW(x), NCOL(x), plot.symbol$cex)
        tmpcall$cex <- unlistIfSimple(cex)
    }
    if (!is.null(fill)) {
        fill <- make.par.list(cn, fill, NROW(x), NCOL(x), plot.symbol$fill)
        tmpcall$fill <- unlistIfSimple(fill)
    }
    if (needStyles) {
        if (identical(panel, "panel.superpose.plain")) {
            if (is.null(col)) 
                col <- plot.line$col
            if (is.null(lty)) 
                lty <- plot.line$lty
            if (is.null(lwd)) 
                lwd <- plot.line$lwd
            if (is.null(pch)) 
                pch <- plot.symbol$pch
            if (is.null(cex)) 
                cex <- plot.symbol$cex
            if (is.null(fill)) 
                fill <- plot.symbol$fill
        }
        if (!is.null(col)) {
            par.settings <- modifyList(list(superpose.line = list(col = unlist(col)), 
                superpose.symbol = list(col = unlist(col))), 
                par.settings)
        }
        if (!is.null(lty)) {
            par.settings <- modifyList(list(superpose.line = list(lty = unlist(lty))), 
                par.settings)
        }
        if (!is.null(lwd)) {
            par.settings <- modifyList(list(superpose.line = list(lwd = unlist(lwd))), 
                par.settings)
        }
        if (!is.null(pch)) {
            par.settings <- modifyList(list(superpose.symbol = list(pch = unlist(pch))), 
                par.settings)
        }
        if (!is.null(cex)) {
            par.settings <- modifyList(list(superpose.symbol = list(cex = unlist(cex))), 
                par.settings)
        }
        if (!is.null(fill)) {
            par.settings <- modifyList(list(superpose.symbol = list(fill = unlist(fill))), 
                par.settings)
        }
    }
    obj <- eval(tmpcall)
    obj$call <- sys.call(sys.parent())
    obj$call[[1]] <- quote(xyplot)
    obj
}


levelplot <- function (x, data, ...) 
UseMethod("levelplot")


ltransform3dMatrix <- function (screen, R.mat = diag(4)) 
{
    rot.mat <- diag(3)
    screen.names <- names(screen)
    screen <- lapply(screen, "*", pi/180)
    for (i in seq_along(screen.names)) {
        th <- screen[[i]]
        cth <- cos(th)
        sth <- sin(th)
        tmp.mat <- (if (screen.names[i] == "x") 
            matrix(c(1, 0, 0, 0, cth, sth, 0, -sth, cth), 3, 
                3)
        else if (screen.names[i] == "y") 
            matrix(c(cth, 0, -sth, 0, 1, 0, sth, 0, cth), 3, 
                3)
        else if (screen.names[i] == "z") 
            matrix(c(cth, sth, 0, -sth, cth, 0, 0, 0, 1), 3, 
                3))
        rot.mat <- tmp.mat %*% rot.mat
    }
    rot.mat <- cbind(rot.mat, c(0, 0, 0))
    rot.mat <- rbind(rot.mat, c(0, 0, 0, 1))
    if (!missing(R.mat)) 
        rot.mat <- rot.mat %*% R.mat
    rot.mat
}


panel.tmd.qqmath <- function (x, f.value = NULL, distribution = qnorm, qtype = 7, 
    groups = NULL, subscripts, ..., identifier = "tmd") 
{
    panel.abline(h = 0)
    if (!is.numeric(x)) 
        x <- as.numeric(x)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    getxx <- function(x, f.value = NULL, nobs = sum(!is.na(x))) {
        if (is.null(f.value)) 
            distribution(ppoints(nobs))
        else distribution(f.value(nobs))
    }
    getyy <- function(x, f.value = NULL, nobs = sum(!is.na(x))) {
        if (is.null(f.value)) 
            sort(x)
        else quantile(x, f.value(nobs), names = FALSE, type = qtype, 
            na.rm = TRUE)
    }
    if (!nobs) 
        NULL
    else if (!is.null(groups)) {
        sx <- split(x, groups[subscripts])
        xxlist <- lapply(sx, getxx, f.value = f.value)
        yylist <- lapply(sx, getyy, f.value = f.value)
        xx <- unlist(xxlist)
        yy <- unlist(yylist)
        tmd.mean <- (xx + yy)/2
        tmd.diff <- (yy - xx)
        tmd.groups <- gl(length(xxlist), sapply(xxlist, length), 
            labels = names(xxlist))
        tmd.subscripts <- seq_along(xx)
        panel.superpose(x = tmd.mean, y = tmd.diff, groups = tmd.groups, 
            subscripts = tmd.subscripts, ...)
    }
    else {
        xx <- getxx(x, f.value, nobs)
        yy <- getyy(x, f.value, nobs)
        tmd.mean <- (xx + yy)/2
        tmd.diff <- (yy - xx)
        panel.xyplot(x = tmd.mean, y = tmd.diff, ..., identifier = identifier)
    }
}


stripplot <- function (x, data, ...) 
UseMethod("stripplot")


panel.qqmath <- function (x, f.value = NULL, distribution = qnorm, qtype = 7, 
    groups = NULL, ..., tails.n = 0, identifier = "qqmath") 
{
    x <- as.numeric(x)
    distribution <- getFunctionOrName(distribution)
    nobs <- sum(!is.na(x))
    if (!is.null(groups)) 
        panel.superpose(x, y = NULL, f.value = f.value, distribution = distribution, 
            qtype = qtype, groups = groups, panel.groups = panel.qqmath, 
            ..., tails.n = tails.n)
    else if (nobs) {
        if (is.null(f.value)) {
            panel.xyplot(x = distribution(ppoints(nobs)), y = sort(x), 
                ..., identifier = identifier)
        }
        else {
            pp <- if (is.numeric(f.value)) 
                f.value
            else f.value(nobs)
            if (tails.n > 0) {
                tails.n <- min(tails.n, nobs%/%2)
                ppd <- ppoints(nobs)
                pp <- pp[(pp > ppd[tails.n] & pp < ppd[nobs + 
                  1 - tails.n])]
                pp <- c(head(ppd, tails.n), pp, tail(ppd, tails.n))
                qtype <- 1
            }
            xx <- distribution(pp)
            yy <- quantile(x, pp, names = FALSE, type = qtype, 
                na.rm = TRUE)
            panel.xyplot(x = xx, y = yy, ..., identifier = identifier)
        }
    }
}


panel.arrows <- function (...) 
larrows(...)


rfs <- function (model, layout = c(2, 1), xlab = "f-value", ylab = NULL, 
    distribution = qunif, panel = function(...) {
        panel.grid(h = -1, v = -1)
        panel.qqmath(...)
    }, prepanel = NULL, strip = TRUE, ...) 
{
    if (!is.function(panel)) 
        panel <- eval(panel)
    if (!is.function(strip)) 
        strip <- eval(strip)
    fitval <- fitted.values(model) - mean(fitted.values(model))
    resids <- residuals(model)
    nf <- length(fitval)
    nr <- length(resids)
    data <- list(y = c(fitval, resids), f = c(rep(gettext("Fitted Values minus Mean"), 
        nf), rep(gettext("Residuals"), nr)))
    qqmath(~y | f, data = data, layout = layout, xlab = xlab, 
        ylab = ylab, distribution = distribution, panel = panel, 
        prepanel = prepanel, strip = strip, ...)
}


lsegments <- function (x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL, 
    col = add.line$col, alpha = add.line$alpha, lty = add.line$lty, 
    lwd = add.line$lwd, font, fontface, ..., identifier = NULL, 
    name.type = "panel") 
{
    if (missing(x0)) 
        x0 <- x2
    if (missing(y0)) 
        y0 <- y2
    add.line <- trellis.par.get("add.line")
    ml <- max(length(x0), length(x1), length(y0), length(y1))
    x0 <- rep(x0, length.out = ml)
    x1 <- rep(x1, length.out = ml)
    y0 <- rep(y0, length.out = ml)
    y1 <- rep(y1, length.out = ml)
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    grid.segments(x0 = x0, x1 = x1, y0 = y0, y1 = y1, name = primName("segments", 
        identifier, name.type, group), gp = gpar(lty = lty, col = col, 
        lwd = lwd, alpha = alpha, ...), default.units = "native")
}


panel.text <- function (...) 
ltext(...)


lplot.xy <- function (xy, type = c("p", "l", "o", "b", "c", "s", "S", "h", 
    "H"), pch = 1, lty = 1, col = 1, cex = 1, lwd = 1, font = 1, 
    fontfamily = NULL, fontface = NULL, col.line = col, col.symbol = col, 
    alpha = 1, fill = NULL, origin = 0, ..., identifier = NULL, 
    name.type = "panel") 
{
    x <- xy$x
    y <- xy$y
    fontsize.points <- trellis.par.get("fontsize")$points
    if (length(x) == 0) 
        return()
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    type <- match.arg(type)
    switch(type, p = {
        grid.points(x = x, y = y, name = primName("points", identifier, 
            name.type, group), gp = gpar(col = col.symbol, cex = cex, 
            lwd = lwd, alpha = alpha, fill = fill, fontsize = fontsize.points, 
            fontfamily = fontfamily, fontface = chooseFace(fontface, 
                font), ...), pch = pch, default.units = "native")
    }, c = , l = {
        grid.lines(x = x, y = y, name = primName("lines", identifier, 
            name.type, group), gp = gpar(lty = lty, col = col.line, 
            lwd = lwd, alpha = alpha, ...), default.units = "native")
    }, o = , b = {
        grid.points(x = x, y = y, name = primName("points", identifier, 
            name.type, group), gp = gpar(col = col.symbol, cex = cex, 
            lwd = lwd, alpha = alpha, fill = fill, fontsize = fontsize.points, 
            fontfamily = fontfamily, fontface = chooseFace(fontface, 
                font), ...), pch = pch, default.units = "native")
        grid.lines(x = x, y = y, name = primName("lines", identifier, 
            name.type, group), gp = gpar(lty = lty, col = col.line, 
            lwd = lwd, alpha = alpha, ...), default.units = "native")
    }, s = , S = {
        ord <- seq_along(x)
        if ((n <- length(x)) > 1) {
            xx <- numeric(2 * n - 1)
            yy <- numeric(2 * n - 1)
            xx[2 * 1:n - 1] <- x[ord]
            yy[2 * 1:n - 1] <- y[ord]
            xx[2 * 1:(n - 1)] <- x[ord][if (type == "s") -1 else -n]
            yy[2 * 1:(n - 1)] <- y[ord][if (type == "s") -n else -1]
            grid.lines(x = xx, y = yy, name = primName("lines", 
                identifier, name.type, group), gp = gpar(lty = lty, 
                col = col.line, lwd = lwd, alpha = alpha, ...), 
                default.units = "native")
        }
    }, h = {
        ylim <- current.viewport()$yscale
        zero <- if (min(ylim) > origin) min(ylim) else if (max(ylim) < 
            origin) max(ylim) else origin
        grid.segments(x0 = x, x1 = x, y0 = y, y1 = zero, name = primName("segments", 
            identifier, name.type, group), gp = gpar(lty = lty, 
            col = col.line, lwd = lwd, alpha = alpha, ...), default.units = "native")
    }, H = {
        xlim <- current.viewport()$xscale
        zero <- if (min(xlim) > origin) min(xlim) else if (max(xlim) < 
            origin) max(xlim) else origin
        grid.segments(x0 = x, x1 = zero, y0 = y, y1 = y, name = primName("segments", 
            identifier, name.type, group), gp = gpar(lty = lty, 
            col = col.line, lwd = lwd, alpha = alpha, ...), default.units = "native")
    })
    return()
}


panel.spline <- function (x, y, npoints = 101, lwd = plot.line$lwd, lty = plot.line$lty, 
    col, col.line = plot.line$col, type, horizontal = FALSE, 
    ..., keep.data = FALSE, identifier = "spline") 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 1) 
        return()
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
    }
    plot.line <- trellis.par.get("plot.line")
    if (horizontal) {
        args <- list(x = y[ok], y = x[ok], ..., keep.data = keep.data)
        smooth <- checkArgsAndCall(smooth.spline, args)
        yy <- do.breaks(range(y[ok]), npoints)
        p <- predict(smooth, x = yy)
        panel.lines(p$y, p$x, col = col.line, lty = lty, lwd = lwd, 
            ..., identifier = identifier)
    }
    else {
        args <- list(x = x[ok], y = y[ok], ..., keep.data = keep.data)
        smooth <- checkArgsAndCall(smooth.spline, args)
        xx <- do.breaks(range(x[ok]), npoints)
        p <- predict(smooth, x = xx)
        panel.lines(p$x, p$y, col = col.line, lty = lty, lwd = lwd, 
            ..., identifier = identifier)
    }
    smooth
}


draw.key <- function (key, draw = FALSE, vp = NULL, ...) 
{
    if (!is.list(key)) 
        stop("key must be a list")
    max.length <- 0
    process.key <- function(reverse.rows = FALSE, between = 2, 
        align = TRUE, title = NULL, rep = TRUE, background = trellis.par.get("background")$col, 
        alpha.background = 1, border = FALSE, transparent = FALSE, 
        col = "black", alpha = 1, lty = 1, lwd = 1, font = 1, 
        fontface = NULL, fontfamily = NULL, pch = 8, cex = 1, 
        fill = "transparent", adj = 0, type = "l", size = 5, 
        height = 1, angle = 0, density = -1, cex.title = 1.5 * 
            max(cex), padding.text = 1, lineheight = 1, columns = 1, 
        divide = 3, between.columns = 3, ..., lines.title = 2) {
        list(reverse.rows = reverse.rows, between = between, 
            align = align, title = title, rep = rep, background = background, 
            alpha.background = alpha.background, border = border, 
            transparent = transparent, col = col, alpha = alpha, 
            lty = lty, lwd = lwd, font = font, fontface = fontface, 
            fontfamily = fontfamily, pch = pch, cex = cex, fill = fill, 
            adj = adj, type = type, size = size, height = height, 
            angle = angle, density = density, cex.title = cex.title, 
            padding.text = padding.text, lineheight = lineheight, 
            columns = columns, divide = divide, between.columns = between.columns, 
            lines.title = lines.title, ...)
    }
    fontsize.points <- trellis.par.get("fontsize")$points
    key <- do.call(process.key, key, quote = TRUE)
    key.length <- length(key)
    key.names <- names(key)
    if (is.logical(key$border)) 
        key$border <- if (key$border) 
            "black"
        else "transparent"
    components <- list()
    for (i in 1:key.length) {
        curname <- pmatch(key.names[i], c("text", "rectangles", 
            "lines", "points"))
        if (is.na(curname)) {
        }
        else if (curname == 1) {
            if (!(is.characterOrExpression(key[[i]][[1]]))) 
                stop("first component of text must be vector of labels")
            pars <- list(labels = key[[i]][[1]], col = key$col, 
                alpha = key$alpha, adj = key$adj, cex = key$cex, 
                lineheight = key$lineheight, font = key$font, 
                fontface = key$fontface, fontfamily = key$fontfamily)
            pars <- pars[!sapply(pars, is.null)]
            key[[i]][[1]] <- NULL
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- length(pars$labels)
            for (j in 1:length(pars)) if (is.character(pars)) 
                pars[[j]] <- rep(pars[[j]], length.out = tmplen)
            max.length <- max(max.length, tmplen)
            components[[length(components) + 1]] <- list(type = "text", 
                pars = pars, length = tmplen)
        }
        else if (curname == 2) {
            pars <- list(col = key$col, border = "black", alpha = key$alpha, 
                size = key$size, height = key$height, angle = key$angle, 
                density = key$density)
            pars <- pars[!sapply(pars, is.null)]
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars, length)))
            max.length <- max(max.length, tmplen)
            components[[length(components) + 1]] <- list(type = "rectangles", 
                pars = pars, length = tmplen)
        }
        else if (curname == 3) {
            pars <- list(col = key$col, alpha = key$alpha, size = key$size, 
                lty = key$lty, cex = key$cex, pch = key$pch, 
                fill = key$fill, lwd = key$lwd, type = key$type)
            pars <- pars[!sapply(pars, is.null)]
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars, length)))
            max.length <- max(max.length, tmplen)
            components[[length(components) + 1]] <- list(type = "lines", 
                pars = pars, length = tmplen)
        }
        else if (curname == 4) {
            pars <- list(col = key$col, alpha = key$alpha, cex = key$cex, 
                pch = key$pch, lwd = key$lwd, fill = key$fill, 
                font = key$font, fontface = key$fontface, fontfamily = key$fontfamily)
            pars <- pars[!sapply(pars, is.null)]
            key[[i]] <- complete_names(key[[i]], pars, allow.invalid = TRUE)
            pars[names(key[[i]])] <- key[[i]]
            tmplen <- max(unlist(lapply(pars, length)))
            max.length <- max(max.length, tmplen)
            components[[length(components) + 1]] <- list(type = "points", 
                pars = pars, length = tmplen)
        }
    }
    number.of.components <- length(components)
    if (number.of.components == 0) 
        stop("Invalid key, need at least one component named lines, text, rect or points")
    for (i in seq_len(number.of.components)) {
        if (key$rep && (components[[i]]$type != "text")) 
            components[[i]]$length <- max.length
        components[[i]]$pars <- lapply(components[[i]]$pars, 
            rep, length.out = components[[i]]$length)
        if (key$reverse.rows) 
            components[[i]]$pars <- lapply(components[[i]]$pars, 
                rev)
    }
    column.blocks <- key$columns
    rows.per.block <- ceiling(max.length/column.blocks)
    if (column.blocks > max.length) 
        warning("not enough rows for columns")
    key$between <- rep(key$between, length.out = number.of.components)
    if (key$align) {
        n.row <- rows.per.block + 1
        n.col <- column.blocks * (1 + 3 * number.of.components) - 
            1
        textMatrix <- matrix(0, n.row, n.col)
        textList <- list()
        textCex <- numeric(0)
        heights.x <- rep(1, n.row)
        heights.units <- rep("lines", n.row)
        heights.data <- vector(mode = "list", length = n.row)
        if (length(key$title) > 0) {
            stopifnot(length(key$title) == 1, is.characterOrExpression(key$title))
            heights.x[1] <- key$lines.title * key$cex.title
            heights.units[1] <- "strheight"
            heights.data[[1]] <- key$title
        }
        else heights.x[1] <- 0
        widths.x <- rep(key$between.columns, n.col)
        widths.units <- rep("strwidth", n.col)
        widths.data <- as.list(rep("o", n.col))
        for (i in 1:column.blocks) {
            widths.x[(1:number.of.components - 1) * 3 + 1 + (i - 
                1) * 3 * number.of.components + i - 1] <- key$between/2
            widths.x[(1:number.of.components - 1) * 3 + 1 + (i - 
                1) * 3 * number.of.components + i + 1] <- key$between/2
        }
        index <- 1
        for (i in 1:number.of.components) {
            cur <- components[[i]]
            id <- (1:column.blocks - 1) * (number.of.components * 
                3 + 1) + i * 3 - 1
            if (cur$type == "text") {
                for (j in 1:cur$length) {
                  colblck <- ceiling(j/rows.per.block)
                  xx <- (colblck - 1) * (number.of.components * 
                    3 + 1) + i * 3 - 1
                  yy <- j%%rows.per.block + 1
                  if (yy == 1) 
                    yy <- rows.per.block + 1
                  textMatrix[yy, xx] <- index
                  textList <- c(textList, list(cur$pars$labels[j]))
                  textCex <- c(textCex, cur$pars$cex[j])
                  index <- index + 1
                }
            }
            else if (cur$type == "rectangles") {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "lines") {
                widths.x[id] <- max(cur$pars$size)
            }
            else if (cur$type == "points") {
                widths.x[id] <- max(cur$pars$cex)
            }
        }
        heights.insertlist.position <- 0
        heights.insertlist.unit <- unit(1, "null")
        for (i in seq_len(n.row)) {
            textLocations <- textMatrix[i, ]
            if (any(textLocations > 0)) {
                textLocations <- textLocations[textLocations > 
                  0]
                strbar <- textList[textLocations]
                heights.insertlist.position <- c(heights.insertlist.position, 
                  i)
                heights.insertlist.unit <- unit.c(heights.insertlist.unit, 
                  unit(0.2 * key$padding.text, "lines") + max(unit(textCex[textLocations], 
                    "strheight", strbar)))
            }
        }
        layout.heights <- unit(heights.x, heights.units, data = heights.data)
        if (length(heights.insertlist.position) > 1) 
            for (indx in 2:length(heights.insertlist.position)) layout.heights <- rearrangeUnit(layout.heights, 
                heights.insertlist.position[indx], heights.insertlist.unit[indx])
        widths.insertlist.position <- 0
        widths.insertlist.unit <- unit(1, "null")
        for (i in 1:n.col) {
            textLocations <- textMatrix[, i]
            if (any(textLocations > 0)) {
                textLocations <- textLocations[textLocations > 
                  0]
                strbar <- textList[textLocations]
                widths.insertlist.position <- c(widths.insertlist.position, 
                  i)
                widths.insertlist.unit <- unit.c(widths.insertlist.unit, 
                  max(unit(textCex[textLocations], "strwidth", 
                    strbar)))
            }
        }
        layout.widths <- unit(widths.x, widths.units, data = widths.data)
        if (length(widths.insertlist.position) > 1) 
            for (indx in 2:length(widths.insertlist.position)) layout.widths <- rearrangeUnit(layout.widths, 
                widths.insertlist.position[indx], widths.insertlist.unit[indx])
        key.layout <- grid.layout(nrow = n.row, ncol = n.col, 
            widths = layout.widths, heights = layout.heights, 
            respect = FALSE, just = if (is.null(key$just)) 
                "center"
            else key$just)
        key.gf <- frameGrob(layout = key.layout, vp = vp, name = trellis.grobname("frame", 
            type = "key"))
        if (!key$transparent) 
            key.gf <- placeGrob(key.gf, rectGrob(gp = gpar(fill = key$background, 
                alpha = key$alpha.background, col = key$border), 
                name = trellis.grobname("background", type = "key")), 
                row = NULL, col = NULL)
        else key.gf <- placeGrob(key.gf, rectGrob(gp = gpar(col = key$border), 
            name = trellis.grobname("background", type = "key")), 
            row = NULL, col = NULL)
        if (!is.null(key$title)) 
            key.gf <- placeGrob(key.gf, textGrob(label = key$title, 
                gp = gpar(cex = key$cex.title, lineheight = key$lineheight), 
                name = trellis.grobname("title", type = "key")), 
                row = 1, col = NULL)
        for (i in 1:number.of.components) {
            cur <- components[[i]]
            for (j in seq_len(cur$length)) {
                colblck <- ceiling(j/rows.per.block)
                xx <- (colblck - 1) * (number.of.components * 
                  3 + 1) + i * 3 - 1
                yy <- j%%rows.per.block + 1
                if (yy == 1) 
                  yy <- rows.per.block + 1
                componentx <- (colblck - 1) * (number.of.components) + 
                  i
                componenty <- (j - 1)%%rows.per.block + 1
                if (cur$type == "text") {
                  key.gf <- placeGrob(key.gf, textGrob(x = cur$pars$adj[j], 
                    hjust = cur$pars$adj[j], label = cur$pars$labels[j], 
                    gp = gpar(col = cur$pars$col[j], alpha = cur$pars$alpha[j], 
                      lineheight = cur$pars$lineheight[j], fontfamily = cur$pars$fontfamily[j], 
                      fontface = chooseFace(cur$pars$fontface[j], 
                        cur$pars$font[j]), cex = cur$pars$cex[j]), 
                    name = componentName("text", componentx, 
                      componenty)), row = yy, col = xx)
                }
                else if (cur$type == "rectangles") {
                  key.gf <- placeGrob(key.gf, rectGrob(height = cur$pars$height[j], 
                    width = cur$pars$size[j]/max(cur$pars$size), 
                    default.units = "npc", gp = gpar(alpha = cur$pars$alpha[j], 
                      fill = cur$pars$col[j], col = cur$pars$border[j]), 
                    name = componentName("rect", componentx, 
                      componenty)), row = yy, col = xx)
                }
                else if (cur$type == "lines") {
                  if (cur$pars$type[j] == "l") {
                    key.gf <- placeGrob(key.gf, linesGrob(x = c(0, 
                      1) * cur$pars$size[j]/max(cur$pars$size), 
                      y = c(0.5, 0.5), gp = gpar(col = cur$pars$col[j], 
                        alpha = cur$pars$alpha[j], lty = cur$pars$lty[j], 
                        lwd = cur$pars$lwd[j]), name = componentName("lines", 
                        componentx, componenty)), row = yy, col = xx)
                  }
                  else if (cur$pars$type[j] == "p") {
                    key.gf <- placeGrob(key.gf, pointsGrob(x = 0.5, 
                      y = 0.5, gp = gpar(col = cur$pars$col[j], 
                        alpha = cur$pars$alpha[j], cex = cur$pars$cex[j], 
                        fill = cur$pars$fill[j], fontfamily = cur$pars$fontfamily[j], 
                        fontface = chooseFace(cur$pars$fontface[j], 
                          cur$pars$font[j]), fontsize = fontsize.points), 
                      pch = cur$pars$pch[j], name = componentName("points", 
                        componentx, componenty)), row = yy, col = xx)
                  }
                  else {
                    key.gf <- placeGrob(key.gf, linesGrob(x = c(0, 
                      1) * cur$pars$size[j]/max(cur$pars$size), 
                      y = c(0.5, 0.5), gp = gpar(col = cur$pars$col[j], 
                        alpha = cur$pars$alpha[j], lty = cur$pars$lty[j], 
                        lwd = cur$pars$lwd[j]), name = componentName("lines", 
                        componentx, componenty)), row = yy, col = xx)
                    if (key$divide > 1) {
                      key.gf <- placeGrob(key.gf, pointsGrob(x = (1:key$divide - 
                        1)/(key$divide - 1), y = rep(0.5, key$divide), 
                        gp = gpar(col = cur$pars$col[j], alpha = cur$pars$alpha[j], 
                          cex = cur$pars$cex[j], fill = cur$pars$fill[j], 
                          fontfamily = cur$pars$fontfamily[j], 
                          fontface = chooseFace(cur$pars$fontface[j], 
                            cur$pars$font[j]), fontsize = fontsize.points), 
                        pch = cur$pars$pch[j], name = componentName("points", 
                          componentx, componenty)), row = yy, 
                        col = xx)
                    }
                    else if (key$divide == 1) {
                      key.gf <- placeGrob(key.gf, pointsGrob(x = 0.5, 
                        y = 0.5, gp = gpar(col = cur$pars$col[j], 
                          alpha = cur$pars$alpha[j], cex = cur$pars$cex[j], 
                          fill = cur$pars$fill[j], fontfamily = cur$pars$fontfamily[j], 
                          fontface = chooseFace(cur$pars$fontface[j], 
                            cur$pars$font[j]), fontsize = fontsize.points), 
                        pch = cur$pars$pch[j], name = componentName("points", 
                          componentx, componenty)), row = yy, 
                        col = xx)
                    }
                  }
                }
                else if (cur$type == "points") {
                  key.gf <- placeGrob(key.gf, pointsGrob(x = 0.5, 
                    y = 0.5, gp = gpar(col = cur$pars$col[j], 
                      alpha = cur$pars$alpha[j], cex = cur$pars$cex[j], 
                      lwd = cur$pars$lwd[j], fill = cur$pars$fill[j], 
                      fontfamily = cur$pars$fontfamily[j], fontface = chooseFace(cur$pars$fontface[j], 
                        cur$pars$font[j]), fontsize = fontsize.points), 
                    pch = cur$pars$pch[j], name = componentName("points", 
                      componentx, componenty)), row = yy, col = xx)
                }
            }
        }
    }
    else stop("Sorry, align=FALSE is not supported")
    if (draw) 
        grid.draw(key.gf)
    key.gf
}


lpolygon <- function (x, y = NULL, border = "black", col = "transparent", 
    fill = NULL, font, fontface, ..., identifier = NULL, name.type = "panel") 
{
    if (sum(!is.na(x)) < 1) 
        return()
    border <- if (all(is.na(border))) 
        "transparent"
    else if (is.logical(border)) {
        if (border) 
            "black"
        else "transparent"
    }
    else border
    xy <- xy.coords(x, y, recycle = TRUE)
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    n <- length(xy$x)
    w <- which(is.na(xy$x) | is.na(xy$y))
    id.lengths <- diff(c(0, w, n))
    grid.polygon(x = xy$x, y = xy$y, id.lengths = id.lengths, 
        default.units = "native", name = primName("polygon", 
            identifier, name.type, group), gp = gpar(fill = col, 
            col = border, ...))
}


prepanel.default.densityplot <- function (x, darg, groups = NULL, weights = NULL, subscripts = TRUE, 
    ...) 
{
    if (!is.numeric(x)) 
        x <- as.numeric(x)
    if (sum(!is.na(x)) < 1) 
        prepanel.null()
    else if (sum(!is.na(x)) == 1) {
        list(xlim = rep(x, 2), ylim = rep(0, 2), dx = 1, dy = 1)
    }
    else if (is.null(groups)) {
        h <- do.call(density, c(list(x = x, weights = weights[subscripts]), 
            darg))
        quants <- quantile(x, c(0.15, 0.85), names = FALSE, na.rm = TRUE)
        ok <- h$x > quants[1] & h$x < quants[2]
        list(xlim = range(h$x), ylim = range(h$y), dx = diff(h$x[ok]), 
            dy = diff(h$y[ok]))
    }
    else {
        vals <- sort(unique(groups))
        xl <- range(x, finite = TRUE)
        yl <- 0
        dxl <- numeric(0)
        dyl <- numeric(0)
        for (i in seq_along(vals)) {
            id <- (groups[subscripts] == vals[i])
            if (sum(id, na.rm = TRUE) > 1) {
                h <- do.call(density, c(list(x = x[id], weights = weights[subscripts][id]), 
                  darg))
                xl <- c(xl, h$x)
                yl <- c(yl, h$y)
                quants <- quantile(x[id], c(0.15, 0.85), names = FALSE, 
                  na.rm = TRUE)
                ok <- h$x > quants[1] & h$x < quants[2]
                dxl <- c(dxl, diff(h$x[ok]))
                dyl <- c(dyl, diff(h$y[ok]))
            }
        }
        list(xlim = range(xl, finite = TRUE), ylim = range(yl, 
            finite = TRUE), dx = dxl, dy = dyl)
    }
}


axis.default <- function (side = c("top", "bottom", "left", "right"), scales, 
    components, as.table, labels = c("default", "yes", "no"), 
    ticks = c("default", "yes", "no"), ..., prefix = lattice.getStatus("current.prefix")) 
{
    side <- match.arg(side)
    labels <- match.arg(labels)
    ticks <- match.arg(ticks)
    row <- lattice.getStatus("current.focus.row", prefix = prefix)
    column <- lattice.getStatus("current.focus.column", prefix = prefix)
    panel.layout <- trellis.currentLayout("panel", prefix = prefix)
    layout.dim <- dim(panel.layout)
    determineStatus <- function(x) {
        if (is.null(x) || (is.logical(x) && !x)) 
            FALSE
        else TRUE
    }
    lastPanel <- function() {
        ((pn <- panel.number(prefix = prefix)) > 0 && pn == max(panel.layout))
    }
    atBoundary <- function() {
        switch(side, top = if (as.table) row == 1 else row == 
            layout.dim[1], bottom = if (!as.table) row == 1 else row == 
            layout.dim[1], left = column == 1, right = column == 
            layout.dim[2] || lastPanel())
    }
    do.ticks <- switch(ticks, yes = TRUE, no = FALSE, default = scales$draw && 
        determineStatus(components[[side]]) && (if (scales$relation == 
        "same") atBoundary() else TRUE))
    do.labels <- switch(labels, yes = TRUE, no = FALSE, default = scales$draw && 
        (if (scales$relation == "same") {
            atBoundary() && switch(side, top = rep(scales$alternating, 
                length.out = column)[column] %in% c(2, 3), bottom = rep(scales$alternating, 
                length.out = column)[column] %in% c(1, 3), left = rep(scales$alternating, 
                length.out = row)[row] %in% c(1, 3), right = rep(scales$alternating, 
                length.out = row)[row] %in% c(2, 3))
        } else TRUE))
    if (do.ticks || do.labels) {
        comp.list <- switch(side, top = if (is.logical(components[["top"]]) && 
            components[["top"]]) components[["bottom"]] else components[["top"]], 
            bottom = components[["bottom"]], left = components[["left"]], 
            right = if (is.logical(components[["right"]]) && 
                components[["right"]]) components[["left"]] else components[["right"]])
        scales.tck <- switch(side, left = , bottom = scales$tck[1], 
            right = , top = scales$tck[2])
        if (!is.logical(comp.list)) {
            if (do.ticks) 
                panel.axis(side = side, at = comp.list$ticks$at, 
                  labels = FALSE, draw.labels = FALSE, check.overlap = FALSE, 
                  outside = TRUE, ticks = TRUE, tck = scales.tck * 
                    comp.list$ticks$tck, ...)
            if (do.labels) 
                panel.axis(side = side, at = comp.list$labels$at, 
                  labels = comp.list$labels$labels, draw.labels = TRUE, 
                  check.overlap = comp.list$labels$check.overlap, 
                  outside = TRUE, ticks = FALSE, tck = scales.tck * 
                    comp.list$ticks$tck, ...)
        }
    }
}


densityplot <- function (x, data, ...) 
UseMethod("densityplot")


panel.densityplot <- function (x, darg = list(n = 30), plot.points = "jitter", ref = FALSE, 
    groups = NULL, weights = NULL, jitter.amount = 0.01 * diff(current.panel.limits()$ylim), 
    type = "p", ..., identifier = "density") 
{
    if (ref) {
        reference.line <- trellis.par.get("reference.line")
        panel.abline(h = 0, col = reference.line$col, lty = reference.line$lty, 
            lwd = reference.line$lwd, identifier = paste(identifier, 
                "abline"))
    }
    if (!is.null(groups)) {
        panel.superpose(x, darg = darg, plot.points = plot.points, 
            ref = FALSE, groups = groups, weights = weights, 
            panel.groups = panel.densityplot, jitter.amount = jitter.amount, 
            type = type, ...)
    }
    else {
        switch(as.character(plot.points), `TRUE` = panel.xyplot(x = x, 
            y = rep(0, length(x)), type = type, ..., identifier = identifier), 
            rug = panel.rug(x = x, start = 0, end = 0, x.units = c("npc", 
                "native"), type = type, ..., identifier = paste(identifier, 
                "rug")), jitter = panel.xyplot(x = x, y = jitter(rep(0, 
                length(x)), amount = jitter.amount), type = type, 
                ..., identifier = identifier))
        density.fun <- function(x, weights, subscripts = TRUE, 
            darg, ...) {
            do.call("density", c(list(x = x, weights = weights[subscripts]), 
                darg))
        }
        if (sum(!is.na(x)) > 1) {
            h <- density.fun(x = x, weights = weights, ..., darg = darg)
            lim <- current.panel.limits()$xlim
            id <- h$x > min(lim) & h$x < max(lim)
            panel.lines(x = h$x[id], y = h$y[id], ..., identifier = identifier)
        }
    }
}


panel.identify <- function (x, y = NULL, subscripts = seq_along(x), labels = subscripts, 
    n = length(x), offset = 0.5, threshold = 18, panel.args = trellis.panelArgs(), 
    ...) 
{
    if (missing(x)) {
        x <- panel.args$x
        y <- panel.args$y
        if (missing(subscripts) && !is.null(panel.args$subscripts)) 
            subscripts <- panel.args$subscripts
    }
    xy <- xy.coords(x, y, recycle = TRUE)
    x <- xy$x
    y <- xy$y
    px <- convertX(unit(x, "native"), "points", TRUE)
    py <- convertY(unit(y, "native"), "points", TRUE)
    labels <- as.character(labels)
    if (length(labels) > length(subscripts)) 
        labels <- labels[subscripts]
    unmarked <- rep(TRUE, length(x))
    count <- 0
    while (count < n) {
        ll <- grid.locator(unit = "points")
        if (is.null(ll)) 
            break
        lx <- convertX(ll$x, "points", TRUE)
        ly <- convertY(ll$y, "points", TRUE)
        pdists <- sqrt((px - lx)^2 + (py - ly)^2)
        if (min(pdists, na.rm = TRUE) > threshold) 
            warning("no observations within ", threshold, " points")
        else {
            w <- which.min(pdists)
            if (unmarked[w]) {
                pos <- getTextPosition(x = lx - px[w], y = ly - 
                  py[w])
                ltext(x[w], y[w], labels[w], pos = pos, offset = offset, 
                  ..., identifier = "identify")
                unmarked[w] <- FALSE
                count <- count + 1
            }
            else warning("nearest observation already identified")
        }
    }
    subscripts[!unmarked]
}


prepanel.default.splom <- function (z, ...) 
{
    list(xlim = c(0.5, ncol(z) + 0.5), ylim = c(0.5, ncol(z) + 
        0.5), dx = 1, dy = 1)
}


as.shingle <- function (x) 
if (is.shingle(x)) x else shingle(x)


panel.smoothScatter <- function (x, y = NULL, nbin = 64, cuts = 255, bandwidth, colramp, 
    nrpoints = 100, transformation = function(x) x^0.25, pch = ".", 
    cex = 1, col = "black", range.x, ..., raster = FALSE, subscripts, 
    identifier = "smoothScatter") 
{
    if (missing(colramp)) 
        colramp <- colorRampPalette(c("white", "#F7FBFF", "#DEEBF7", 
            "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", 
            "#08519C", "#08306B"))
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (!is.numeric(nrpoints) | (nrpoints < 0) | (length(nrpoints) != 
        1)) 
        stop("'nrpoints' should be numeric scalar with value >= 0.")
    xy <- xy.coords(x, y)
    x <- cbind(xy$x, xy$y)[!(is.na(xy$x) | is.na(xy$y)), , drop = FALSE]
    if (nrow(x) < 1) 
        return()
    map <- .smoothScatterCalcDensity(x, nbin, bandwidth, range.x)
    xm <- map$x1
    ym <- map$x2
    dens <- map$fhat
    dens <- array(transformation(dens), dim = dim(dens))
    PFUN <- if (raster) 
        panel.levelplot.raster
    else panel.levelplot
    PFUN(x = rep(xm, length(ym)), y = rep(ym, each = length(xm)), 
        z = as.numeric(dens), subscripts = TRUE, at = seq(from = 0, 
            to = 1.01 * max(dens), length = cuts + 2), col.regions = colramp(cuts + 
            1), ..., identifier = identifier)
    if (nrpoints != 0) {
        stopifnot(length(xm) == nrow(dens), length(ym) == ncol(dens))
        ixm <- round((x[, 1] - xm[1])/(xm[length(xm)] - xm[1]) * 
            (length(xm) - 1))
        iym <- round((x[, 2] - ym[1])/(ym[length(ym)] - ym[1]) * 
            (length(ym) - 1))
        idens <- dens[1 + iym * length(xm) + ixm]
        nrpoints <- min(nrow(x), ceiling(nrpoints))
        sel <- order(idens, decreasing = FALSE)[1:nrpoints]
        panel.points(x[sel, 1:2], pch = pch, cex = cex, col = col, 
            identifier = identifier)
    }
}


panel.lines <- function (...) 
llines(...)


panel.identify.cloud <- function (x = panel.args$x, y = panel.args$y, z = panel.args$z, 
    subscripts = panel.args$subscripts, perspective = TRUE, distance = if (perspective) 0.2 else 0, 
    xlim = panel.args$xlim, ylim = panel.args$ylim, zlim = panel.args$zlim, 
    screen = list(z = 40, x = -60), R.mat = diag(4), aspect = c(1, 
        1), scales.3d = panel.args$scales.3d, ..., panel.3d.identify = panel.3didentify, 
    n = length(subscripts), offset = 0.5, threshold = 18, labels = subscripts, 
    panel.args = trellis.panelArgs()) 
{
    argOrDefault <- function(arg) {
        if (is.null(panel.args[[arg]])) 
            get(arg)
        else panel.args[[arg]]
    }
    if (missing(perspective)) 
        perspective <- argOrDefault("perspective")
    if (missing(distance)) 
        distance <- argOrDefault("distance")
    if (missing(screen)) 
        screen <- argOrDefault("screen")
    if (missing(R.mat)) 
        R.mat <- argOrDefault("R.mat")
    if (missing(aspect)) 
        aspect <- argOrDefault("aspect")
    if (is.factor(x)) 
        x <- as.numeric(x)
    if (is.factor(y)) 
        y <- as.numeric(y)
    if (is.factor(z)) 
        z <- as.numeric(z)
    rot.mat <- ltransform3dMatrix(screen = screen, R.mat = R.mat)
    if (length(subscripts) == 0) 
        return(integer(0))
    xlabelinfo <- calculateAxisComponents(xlim, at = scales.3d$x$at, 
        num.limit = NULL, labels = scales.3d$x$labels, logsc = scales.3d$x$log, 
        abbreviate = scales.3d$x$abbreviate, minlength = scales.3d$x$minlength, 
        format.posixt = scales.3d$x$format, n = scales.3d$x$tick.number)
    ylabelinfo <- calculateAxisComponents(ylim, at = scales.3d$y$at, 
        num.limit = NULL, labels = scales.3d$y$labels, logsc = scales.3d$y$log, 
        abbreviate = scales.3d$y$abbreviate, minlength = scales.3d$y$minlength, 
        format.posixt = scales.3d$y$format, n = scales.3d$y$tick.number)
    zlabelinfo <- calculateAxisComponents(zlim, at = scales.3d$z$at, 
        num.limit = NULL, labels = scales.3d$z$labels, logsc = scales.3d$z$log, 
        abbreviate = scales.3d$z$abbreviate, minlength = scales.3d$z$minlength, 
        format.posixt = scales.3d$z$format, n = scales.3d$z$tick.number)
    xlim <- xlabelinfo$num.limit
    ylim <- ylabelinfo$num.limit
    zlim <- zlabelinfo$num.limit
    aspect <- rep(aspect, length.out = 2)
    x <- x[subscripts]
    y <- y[subscripts]
    z <- z[subscripts]
    corners <- data.frame(x = c(-1, 1, 1, -1, -1, 1, 1, -1), 
        y = c(-1, -1, -1, -1, 1, 1, 1, 1) * aspect[1], z = c(-1, 
            -1, 1, 1, -1, -1, 1, 1) * aspect[2])
    corners <- corners/(2 * max(corners))
    xlim.scaled <- range(corners$x)
    ylim.scaled <- range(corners$y)
    zlim.scaled <- range(corners$z)
    cmin <- lapply(corners, min)
    clen <- lapply(corners, function(x) diff(range(x, finite = TRUE)))
    x <- cmin$x + clen$x * (x - xlim[1])/diff(xlim)
    y <- cmin$y + clen$y * (y - ylim[1])/diff(ylim)
    z <- cmin$z + clen$z * (z - zlim[1])/diff(zlim)
    panel.3d.identify(x, y, z, rot.mat = rot.mat, distance = distance, 
        xlim.scaled = xlim.scaled, ylim.scaled = ylim.scaled, 
        zlim.scaled = zlim.scaled, subscripts = subscripts, labels = labels, 
        n = length(x), offset = 0.5, threshold = 18, ...)
}


ltext <- function (x, ...) 
UseMethod("ltext")


yscale.components.default <- function (lim, packet.number = 0, packet.list = NULL, right = TRUE, 
    ...) 
{
    comps <- calculateAxisComponents(lim, packet.list = packet.list, 
        packet.number = packet.number, ...)
    list(num.limit = comps$num.limit, left = list(ticks = list(at = comps$at, 
        tck = 1), labels = list(at = comps$at, labels = comps$labels, 
        cex = 1, check.overlap = comps$check.overlap)), right = right)
}


panel.superpose <- function (x, y = NULL, subscripts, groups, panel.groups = "panel.xyplot", 
    ..., col = "black", col.line = superpose.line$col, col.symbol = superpose.symbol$col, 
    pch = superpose.symbol$pch, cex = superpose.symbol$cex, fill = superpose.symbol$fill, 
    font = superpose.symbol$font, fontface = superpose.symbol$fontface, 
    fontfamily = superpose.symbol$fontfamily, lty = superpose.line$lty, 
    lwd = superpose.line$lwd, alpha = superpose.symbol$alpha, 
    type = "p", grid = FALSE, distribute.type = FALSE) 
{
    if (distribute.type) {
        type <- as.list(type)
    }
    else {
        type <- unique(type)
        wg <- match("g", type, nomatch = NA_character_)
        if (!is.na(wg)) {
            if (missing(grid)) 
                grid <- TRUE
            type <- type[-wg]
        }
        type <- list(type)
    }
    if (grid) 
        panel.grid(h = -1, v = -1, x = x, y = y)
    x <- as.numeric(x)
    if (!is.null(y)) 
        y <- as.numeric(y)
    if (length(x) > 0) {
        if (!missing(col)) {
            if (missing(col.line)) 
                col.line <- col
            if (missing(col.symbol)) 
                col.symbol <- col
        }
        superpose.symbol <- trellis.par.get("superpose.symbol")
        superpose.line <- trellis.par.get("superpose.line")
        vals <- if (is.factor(groups)) 
            levels(groups)
        else sort(unique(groups))
        nvals <- length(vals)
        col <- rep(col, length.out = nvals)
        col.line <- rep(col.line, length.out = nvals)
        col.symbol <- rep(col.symbol, length.out = nvals)
        pch <- rep(pch, length.out = nvals)
        fill <- rep(fill, length.out = nvals)
        lty <- rep(lty, length.out = nvals)
        lwd <- rep(lwd, length.out = nvals)
        alpha <- rep(alpha, length.out = nvals)
        cex <- rep(cex, length.out = nvals)
        font <- rep(font, length.out = nvals)
        if (!is.null(fontface)) 
            fontface <- rep(fontface, length.out = nvals)
        if (!is.null(fontfamily)) 
            fontfamily <- rep(fontfamily, length.out = nvals)
        type <- rep(type, length.out = nvals)
        panel.groups <- getFunctionOrName(panel.groups)
        subg <- groups[subscripts]
        ok <- !is.na(subg)
        for (i in seq_along(vals)) {
            id <- ok & (subg == vals[i])
            if (any(id)) {
                args <- list(x = x[id], subscripts = subscripts[id], 
                  pch = pch[[i]], cex = cex[[i]], font = font[[i]], 
                  fontface = fontface[[i]], fontfamily = fontfamily[[i]], 
                  col = col[[i]], col.line = col.line[[i]], col.symbol = col.symbol[[i]], 
                  fill = fill[[i]], lty = lty[[i]], lwd = lwd[[i]], 
                  alpha = alpha[[i]], type = type[[i]], group.number = i, 
                  group.value = vals[i], ...)
                if (!is.null(y)) 
                  args$y <- y[id]
                do.call(panel.groups, args)
            }
        }
    }
}


diag.panel.splom <- function (x = NULL, varname = NULL, limits, at = NULL, labels = NULL, 
    draw = TRUE, tick.number = 5, varname.col = add.text$col, 
    varname.cex = add.text$cex, varname.lineheight = add.text$lineheight, 
    varname.font = add.text$font, varname.fontfamily = add.text$fontfamily, 
    varname.fontface = add.text$fontface, axis.text.col = axis.text$col, 
    axis.text.alpha = axis.text$alpha, axis.text.cex = axis.text$cex, 
    axis.text.font = axis.text$font, axis.text.fontfamily = axis.text$fontfamily, 
    axis.text.fontface = axis.text$fontface, axis.text.lineheight = axis.text$lineheight, 
    axis.line.col = axis.line$col, axis.line.alpha = axis.line$alpha, 
    axis.line.lty = axis.line$lty, axis.line.lwd = axis.line$lwd, 
    axis.line.tck = 1, ...) 
{
    add.text <- trellis.par.get("add.text")
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")
    if (!is.null(varname)) 
        grid.text(varname, name = trellis.grobname("diag.text", 
            type = "panel"), gp = gpar(col = varname.col, cex = varname.cex, 
            lineheight = varname.lineheight, fontface = chooseFace(varname.fontface, 
                varname.font), fontfamily = varname.fontfamily))
    if (draw) {
        rot <- if (is.numeric(limits)) 
            0
        else c(90, 0)
        axis.details <- formattedTicksAndLabels(limits, at = if (is.null(at)) 
            TRUE
        else at, labels = if (is.null(labels)) 
            TRUE
        else labels, logsc = FALSE, ..., n = tick.number)
        for (side in c("left", "top", "right", "bottom")) panel.axis(side = side, 
            at = axis.details$at, labels = axis.details$labels, 
            check.overlap = axis.details$check.overlap, ticks = TRUE, 
            half = TRUE, tck = axis.line.tck, rot = rot, text.col = axis.text.col, 
            text.alpha = axis.text.alpha, text.cex = axis.text.cex, 
            text.font = axis.text.font, text.fontfamily = axis.text.fontfamily, 
            text.fontface = axis.text.fontface, text.lineheight = axis.text.lineheight, 
            line.col = axis.line.col, line.alpha = axis.line.alpha, 
            line.lty = axis.line.lty, line.lwd = axis.line.lwd)
    }
}


packet.number <- function (prefix = lattice.getStatus("current.prefix")) 
{
    trellis.currentLayout("packet", prefix = prefix)[current.row(prefix = prefix), 
        current.column(prefix = prefix)]
}


level.colors <- function (x, at, col.regions, colors = TRUE, ...) 
{
    ind.col <- cut(x, at, include.lowest = TRUE, labels = FALSE)
    if (!colors) 
        ind.col
    else {
        if (missing(col.regions)) 
            col.regions <- trellis.par.get("regions")$col
        nregions <- length(at) - 1
        if (is.function(col.regions)) 
            col.regions <- col.regions(nregions)
        ncolor <- length(col.regions)
        col.regions <- if (ncolor <= nregions) 
            rep(col.regions, length.out = nregions)
        else col.regions[round(seq(1, ncolor, length.out = nregions))]
        col.regions[ind.col]
    }
}


banking <- function (dx, dy = 1) 
{
    if (is.list(dx)) {
        dy <- dx[[2]]
        dx <- dx[[1]]
    }
    if (length(dx) != length(dy)) 
        stop("Non matching lengths")
    id <- dx != 0 & dy != 0 & !is.na(dx) & !is.na(dy)
    if (any(id)) {
        r <- abs(dx[id]/dy[id])
        median(r)
    }
    else 1
}


prepanel.default.qq <- function (x, y, ...) 
{
    list(xlim = scale.limits(c(x, y)), ylim = scale.limits(c(x, 
        y)), dx = 1, dy = 1)
}


trellis.unfocus <- function () 
{
    prefix <- lattice.getStatus("current.prefix")
    if (lattice.getStatus("vp.highlighted", prefix = prefix)) {
        grid.remove("lvp.highlight", warn = FALSE)
        lattice.setStatus(vp.highlighted = FALSE, prefix = prefix)
    }
    lattice.setStatus(current.focus.column = 0, current.focus.row = 0, 
        prefix = prefix)
    if (lattice.getStatus("vp.depth", prefix = prefix) > 0) 
        upViewport(lattice.getStatus("vp.depth", prefix = prefix))
    lattice.setStatus(vp.depth = 0, prefix = prefix)
    invisible()
}


which.packet <- function (prefix = lattice.getStatus("current.prefix")) 
{
    lattice.getStatus("current.cond.levels", prefix = prefix)[[current.row(prefix = prefix), 
        current.column(prefix = prefix)]]
}


latticeParseFormula <- function (model, data, dimension = 2, subset = TRUE, groups = NULL, 
    multiple = FALSE, outer = FALSE, subscripts = FALSE, drop = NULL) 
{
    expr2char <- function(x) paste(deparse(x), collapse = "")
    if (inherits(groups, "formula")) {
        groupVar <- as.character(groups)[2]
        groups <- eval(parse(text = groupVar), data, environment(groups))
    }
    if (is.null(drop)) 
        drop <- TRUE
    if (is.list(drop)) {
        drop.unused.cond <- if (is.null(drop$cond)) 
            TRUE
        else drop$cond
        drop.unused.data <- if (is.null(drop$data)) 
            TRUE
        else drop$data
    }
    else {
        drop.unused.cond <- drop
        drop.unused.data <- drop
    }
    parseSide <- function(model) {
        model.vars <- list()
        while (length(model) == 3 && model[[1]] == as.name("+")) {
            model.vars <- c(model.vars, model[[3]])
            model <- model[[2]]
        }
        rev(c(model.vars, model))
    }
    parseCond <- function(model) {
        model <- substitute(~m, list(m = model))[[2]]
        model.vars <- list()
        while (length(model) == 3 && (model[[1]] == as.name("*") || 
            model[[1]] == as.name("+"))) {
            model.vars <- c(model.vars, model[[3]])
            model <- model[[2]]
        }
        rev(c(model.vars, model))
    }
    lrep <- function(x, n) {
        save.attr <- attributes(x)
        x <- rep(x, n)
        attributes(x) <- save.attr
        x
    }
    concat <- function(arglist) {
        if (length(arglist) == 1) 
            arglist[[1]]
        else if (any(sapply(arglist, is.factor))) {
            factor(unlist(lapply(arglist, as.character)))
        }
        else if (any(sapply(arglist, is.shingle))) {
            stop("shingles can not be concatenated")
        }
        else do.call("c", arglist)
    }
    if (!inherits(model, "formula")) 
        stop("model must be a formula object")
    if (multiple && !outer && !is.null(groups)) {
        multiple <- FALSE
        warning("'multiple=TRUE' ignored ('groups' non-null with 'outer=FALSE')")
    }
    ans <- if (dimension == 2) {
        list(left = NULL, right = NULL, condition = NULL, left.name = character(0), 
            right.name = character(0))
    }
    else if (dimension == 3) {
        list(left = NULL, right.x = NULL, right.y = NULL, condition = NULL, 
            left.name = character(0), right.x.name = character(0), 
            right.y.name = character(0))
    }
    else stop(gettextf("invalid dimension '%s'", as.character(dimension)))
    if (length(model) == 3) {
        if (multiple) {
            varsLHS <- parseSide(model[[2]])
            nLHS <- length(varsLHS)
        }
        else {
            varsLHS <- list(model[[2]])
            nLHS <- 1
        }
    }
    else {
        nLHS <- 1
    }
    modelRHS <- model[[length(model)]]
    if (length(modelRHS) == 3 && modelRHS[[1]] == as.name("|")) 
        modelRHS <- modelRHS[[2]]
    env <- environment(model)
    modelRHS <- model[[length(model)]]
    if (length(modelRHS) == 3 && modelRHS[[1]] == as.name("|")) {
        modelRHS.vars <- parseCond(modelRHS[[3]])
        modelRHS <- modelRHS[[2]]
        if (multiple && dimension == 2) {
            varsRHS <- parseSide(modelRHS)
            nRHS <- length(varsRHS)
        }
        else {
            varsRHS <- list(modelRHS)
            nRHS <- 1
        }
        ans$condition <- vector("list", length(modelRHS.vars))
        names(ans$condition) <- sapply(modelRHS.vars, expr2char)
        for (i in seq_along(modelRHS.vars)) {
            ans$condition[[i]] <- lrep(as.factorOrShingle(eval(modelRHS.vars[[i]], 
                data, env), subset, drop = drop.unused.cond), 
                nLHS * nRHS)
        }
    }
    else if (multiple && dimension == 2) {
        varsRHS <- parseSide(modelRHS)
        nRHS <- length(varsRHS)
    }
    else {
        varsRHS <- list(modelRHS)
        nRHS <- 1
    }
    if (length(model) == 3) {
        ans$left.name <- expr2char(model[[2]])
        ans$left <- lrep(concat(lapply(varsLHS, function(i) {
            tmp <- eval(i, data, env)
            if (!is.matrix(tmp)) 
                tmp <- if (is.factor(tmp) || is.shingle(tmp)) 
                  tmp[subset, drop = drop.unused.data]
                else tmp[subset]
            if (inherits(tmp, "POSIXt")) 
                tmp <- as.POSIXct(tmp)
            tmp
        })), nRHS)
    }
    if (dimension == 2) {
        tmp <- eval(varsRHS[[1]], data, env)
        if (is.matrix(tmp)) 
            tmp <- as.data.frame(tmp)
        nobs <- if (is.data.frame(tmp)) 
            nrow(tmp)
        else length(tmp)
        if (nLHS == 1 && nRHS == 1) {
            if (is.data.frame(tmp)) 
                ans$right <- tmp[subset, ]
            else ans$right <- if (is.factor(tmp) || is.shingle(tmp)) 
                tmp[subset, drop = drop.unused.data]
            else tmp[subset]
        }
        else {
            ans$right <- concat(lapply(varsRHS, function(i) {
                tmp <- eval(i, data, env)
                tmp <- if (is.factor(tmp) || is.shingle(tmp)) 
                  tmp[subset, drop = drop.unused.data]
                else tmp[subset]
                tmp <- lrep(tmp, nLHS)
                if (inherits(tmp, "POSIXt")) 
                  tmp <- as.POSIXct(tmp)
                tmp
            }))
        }
        ans$right.name <- expr2char(modelRHS)
        nRows <- length(ans$right)/(nLHS * nRHS)
    }
    else if (dimension == 3 && length(modelRHS) == 3 && (modelRHS[[1]] == 
        "*" || modelRHS[[1]] == "+")) {
        tmp <- eval(modelRHS[[2]], data, env)
        nobs <- length(tmp)
        if (!is.matrix(tmp)) 
            tmp <- if (is.factor(tmp) || is.shingle(tmp)) 
                tmp[subset, drop = drop.unused.data]
            else tmp[subset]
        ans$right.x <- lrep(tmp, nLHS)
        if (inherits(ans$right.x, "POSIXt")) 
            ans$right.x <- as.POSIXct(ans$right.x)
        tmp <- eval(modelRHS[[3]], data, env)
        if (!is.matrix(tmp)) 
            tmp <- if (is.factor(tmp) || is.shingle(tmp)) 
                tmp[subset, drop = drop.unused.data]
            else tmp[subset]
        ans$right.y <- lrep(tmp, nLHS)
        if (inherits(ans$right.y, "POSIXt")) 
            ans$right.y <- as.POSIXct(ans$right.y)
        ans$right.x.name <- expr2char(modelRHS[[2]])
        ans$right.y.name <- expr2char(modelRHS[[3]])
        nRows <- length(ans$right.x)/nLHS
    }
    else stop("invalid model")
    if (nLHS > 1) 
        LHSgroups <- rep(gl(nLHS, nRows, labels = sapply(varsLHS, 
            expr2char)), nRHS)
    if (nRHS > 1) 
        RHSgroups <- gl(nRHS, nRows * nLHS, labels = sapply(varsRHS, 
            expr2char))
    newFactor <- if (nLHS > 1 && nRHS > 1) {
        interaction2(LHSgroups, RHSgroups, sep = lattice.getOption("interaction.sep"))
    }
    else if (nLHS > 1) 
        LHSgroups
    else if (nRHS > 1) 
        RHSgroups
    else NULL
    if (nLHS == 1 && nRHS == 1) {
        if (!is.null(groups)) 
            ans$groups <- groups
        if (subscripts) 
            ans$subscr <- seq_len(nobs)[subset]
    }
    else if (outer) {
        if (!is.null(groups)) 
            ans$groups <- rep(groups, nLHS * nRHS)
        if (!is.null(newFactor)) {
            if (is.null(ans$condition)) 
                ans$condition <- list(newFactor)
            else ans$condition[[length(ans$condition) + 1]] <- newFactor
        }
        else stop("newFactor cannot be NULL; you have found a bug!")
        if (subscripts) 
            ans$subscr <- as.vector(matrix(seq_len(nobs * nLHS * 
                nRHS), nrow = nobs)[subset, ])
    }
    else {
        if (is.null(groups) && !is.null(newFactor)) 
            ans$groups <- newFactor
        else stop("newFactor != NULL && groups == NULL does not hold; you have found a bug!")
        if (subscripts) 
            ans$subscr <- seq_len(length(newFactor))
        if (length(newFactor) != nRows * nLHS * nRHS) 
            stop("Length check mismatch; you have found a bug!")
    }
    ans
}


panel.qq <- function (..., identifier = "qq") 
{
    reference.line <- trellis.par.get("reference.line")
    panel.abline(0, 1, col = reference.line$col, lty = reference.line$lty, 
        lwd = reference.line$lwd, identifier = paste(identifier, 
            "abline", sep = "."))
    panel.xyplot(..., identifier = identifier)
}


Rows <- function (x, which) 
{
    for (i in seq_along(x)) x[[i]] <- rep(x[[i]], length.out = max(which, 
        length(which)))[which]
    x
}


panel.xyplot <- function (x, y, type = "p", groups = NULL, pch = if (is.null(groups)) plot.symbol$pch else superpose.symbol$pch, 
    col, col.line = if (is.null(groups)) plot.line$col else superpose.line$col, 
    col.symbol = if (is.null(groups)) plot.symbol$col else superpose.symbol$col, 
    font = if (is.null(groups)) plot.symbol$font else superpose.symbol$font, 
    fontfamily = if (is.null(groups)) plot.symbol$fontfamily else superpose.symbol$fontfamily, 
    fontface = if (is.null(groups)) plot.symbol$fontface else superpose.symbol$fontface, 
    lty = if (is.null(groups)) plot.line$lty else superpose.line$lty, 
    cex = if (is.null(groups)) plot.symbol$cex else superpose.symbol$cex, 
    fill = if (is.null(groups)) plot.symbol$fill else superpose.symbol$fill, 
    lwd = if (is.null(groups)) plot.line$lwd else superpose.line$lwd, 
    horizontal = FALSE, ..., grid = FALSE, abline = NULL, jitter.x = FALSE, 
    jitter.y = FALSE, factor = 0.5, amount = NULL, identifier = "xyplot") 
{
    if (all(is.na(x) | is.na(y))) 
        return()
    plot.symbol <- trellis.par.get("plot.symbol")
    plot.line <- trellis.par.get("plot.line")
    superpose.symbol <- trellis.par.get("superpose.symbol")
    superpose.line <- trellis.par.get("superpose.line")
    if (!missing(col)) {
        if (missing(col.line)) 
            col.line <- col
        if (missing(col.symbol)) 
            col.symbol <- col
    }
    if (missing(grid) && ("g" %in% type)) 
        grid <- TRUE
    if (!identical(grid, FALSE)) {
        if (!is.list(grid)) 
            grid <- switch(as.character(grid), `TRUE` = list(h = -1, 
                v = -1, x = x, y = y), h = list(h = -1, v = 0, 
                y = y), v = list(h = 0, v = -1, x = x), list(h = 0, 
                v = 0))
        do.call(panel.grid, grid)
    }
    if (!is.null(abline)) {
        if (is.numeric(abline)) 
            abline <- as.list(abline)
        do.call(panel.abline, abline)
    }
    if (!is.null(groups)) 
        panel.superpose(x, y, type = type, groups = groups, pch = pch, 
            col.line = col.line, col.symbol = col.symbol, font = font, 
            fontfamily = fontfamily, fontface = fontface, lty = lty, 
            cex = cex, fill = fill, lwd = lwd, horizontal = horizontal, 
            panel.groups = panel.xyplot, jitter.x = jitter.x, 
            jitter.y = jitter.y, factor = factor, amount = amount, 
            grid = FALSE, ...)
    else {
        x <- as.numeric(x)
        y <- as.numeric(y)
        id <- identifier
        if ("o" %in% type || "b" %in% type) 
            type <- c(type, "p", "l")
        if ("p" %in% type) 
            panel.points(x = if (jitter.x) 
                jitter(x, factor = factor, amount = amount)
            else x, y = if (jitter.y) 
                jitter(y, factor = factor, amount = amount)
            else y, cex = cex, fill = fill, font = font, fontfamily = fontfamily, 
                fontface = fontface, col = col.symbol, pch = pch, 
                ..., identifier = id)
        if ("l" %in% type) 
            panel.lines(x = x, y = y, lty = lty, col = col.line, 
                lwd = lwd, ..., identifier = id)
        if ("h" %in% type) {
            if (horizontal) 
                panel.lines(x = x, y = y, type = "H", lty = lty, 
                  col = col.line, lwd = lwd, ..., identifier = id)
            else panel.lines(x = x, y = y, type = "h", lty = lty, 
                col = col.line, lwd = lwd, ..., identifier = id)
        }
        if ("s" %in% type) {
            ord <- if (horizontal) 
                sort.list(y)
            else sort.list(x)
            n <- length(x)
            xx <- numeric(2 * n - 1)
            yy <- numeric(2 * n - 1)
            xx[2 * 1:n - 1] <- x[ord]
            yy[2 * 1:n - 1] <- y[ord]
            xx[2 * 1:(n - 1)] <- x[ord][-1]
            yy[2 * 1:(n - 1)] <- y[ord][-n]
            panel.lines(x = xx, y = yy, lty = lty, col = col.line, 
                lwd = lwd, ..., identifier = id)
        }
        if ("S" %in% type) {
            ord <- if (horizontal) 
                sort.list(y)
            else sort.list(x)
            n <- length(x)
            xx <- numeric(2 * n - 1)
            yy <- numeric(2 * n - 1)
            xx[2 * 1:n - 1] <- x[ord]
            yy[2 * 1:n - 1] <- y[ord]
            xx[2 * 1:(n - 1)] <- x[ord][-n]
            yy[2 * 1:(n - 1)] <- y[ord][-1]
            panel.lines(x = xx, y = yy, lty = lty, col = col.line, 
                lwd = lwd, ..., identifier = id)
        }
        if ("r" %in% type) 
            panel.lmline(x, y, col = col.line, lty = lty, lwd = lwd, 
                ...)
        if ("smooth" %in% type) 
            panel.loess(x, y, horizontal = horizontal, col = col.line, 
                lty = lty, lwd = lwd, ...)
        if ("spline" %in% type) 
            panel.spline(x, y, horizontal = horizontal, col = col.line, 
                lty = lty, lwd = lwd, ...)
        if ("a" %in% type) 
            panel.linejoin(x, y, horizontal = horizontal, lwd = lwd, 
                lty = lty, col.line = col.line, ...)
    }
}


panel.axis <- function (side = c("bottom", "left", "top", "right"), at = pretty(scale.range), 
    labels = TRUE, draw.labels = TRUE, check.overlap = FALSE, 
    outside = FALSE, ticks = TRUE, half = !outside, which.half = switch(side, 
        bottom = "lower", left = "upper", top = "upper", right = "lower"), 
    tck = as.numeric(ticks), rot = if (is.logical(labels)) 0 else c(90, 
        0), text.col = axis.text$col, text.alpha = axis.text$alpha, 
    text.cex = axis.text$cex, text.font = axis.text$font, text.fontfamily = axis.text$fontfamily, 
    text.fontface = axis.text$fontface, text.lineheight = axis.text$lineheight, 
    line.col = axis.line$col, line.lty = axis.line$lty, line.lwd = axis.line$lwd, 
    line.alpha = axis.line$alpha) 
{
    side <- match.arg(side)
    orientation <- if (outside) 
        "outer"
    else "inner"
    cpl <- current.panel.limits()
    scale.range <- range(switch(side, left = cpl$ylim, top = cpl$xlim, 
        right = cpl$ylim, bottom = cpl$xlim))
    axis.line <- trellis.par.get("axis.line")
    axis.text <- trellis.par.get("axis.text")
    rot <- rep(rot, length.out = 2)
    if (is.null(at) || length(at) == 0) 
        return()
    if (is.logical(labels)) 
        labels <- if (labels) 
            format(at, trim = TRUE)
        else NULL
    if (check.overlap) {
        pad <- lattice.getOption("skip.boundary.labels")
        scale.range <- extend.limits(scale.range, prop = -pad)
    }
    keep.at <- at >= scale.range[1] & at <= scale.range[2]
    at <- at[keep.at]
    labels <- labels[keep.at]
    nal <- length(at)/2 + 0.5
    all.id <- seq_along(at)
    lower.id <- all.id <= nal
    upper.id <- all.id >= nal
    axid <- if (half) {
        if (which.half == "lower") 
            lower.id
        else upper.id
    }
    else rep(TRUE, length(all.id))
    if (!any(axid)) 
        return(invisible())
    gp.line <- gpar(col = line.col, alpha = line.alpha, lty = line.lty, 
        lwd = line.lwd)
    gp.text <- gpar(col = text.col, cex = text.cex, alpha = text.alpha, 
        fontface = chooseFace(text.fontface, text.font), fontfamily = text.fontfamily, 
        lineheight = text.lineheight)
    axis.units <- lattice.getOption("axis.units")[[orientation]][[side]]
    axis.settings <- trellis.par.get("axis.components")[[side]]
    tck.unit.x <- tck * axis.settings$tck * axis.units$tick$x
    tck.unit <- unit(x = tck.unit.x, units = axis.units$tick$units)
    lab.unit <- if (any(tck.unit.x > 0)) 
        tck.unit + unit(x = axis.settings$pad1 * axis.units$pad1$x, 
            units = axis.units$pad1$units)
    else unit(x = axis.settings$pad1 * axis.units$pad1$x, units = axis.units$pad1$units)
    orient.factor <- if (outside) 
        -1
    else 1
    if (ticks && any(tck.unit.x != 0)) 
        switch(side, bottom = grid.segments(x0 = unit(at[axid], 
            "native"), x1 = unit(at[axid], "native"), y0 = unit(0, 
            "npc"), y1 = orient.factor * tck.unit, name = trellis.grobname("ticks.bottom", 
            type = "panel"), gp = gp.line), top = grid.segments(x0 = unit(at[axid], 
            "native"), x1 = unit(at[axid], "native"), y0 = unit(1, 
            "npc"), y1 = unit(1, "npc") - orient.factor * tck.unit, 
            name = trellis.grobname("ticks.top", type = "panel"), 
            gp = gp.line), left = grid.segments(y0 = unit(at[axid], 
            "native"), y1 = unit(at[axid], "native"), x0 = unit(0, 
            "npc"), x1 = orient.factor * tck.unit, name = trellis.grobname("ticks.left", 
            type = "panel"), gp = gp.line), right = grid.segments(y0 = unit(at[axid], 
            "native"), y1 = unit(at[axid], "native"), x0 = unit(1, 
            "npc"), x1 = unit(1, "npc") - orient.factor * tck.unit, 
            name = trellis.grobname("ticks.right", type = "panel"), 
            gp = gp.line))
    if (draw.labels && !is.null(labels)) {
        {
            just <- if (outside) 
                switch(side, bottom = if (rot[1] == 0) c("centre", 
                  "top") else c("right", "centre"), top = if (rot[1] == 
                  0) c("centre", "bottom") else c("left", "centre"), 
                  left = if (rot[2] == 90) c("centre", "bottom") else c("right", 
                    "centre"), right = if (rot[2] == 90) c("centre", 
                    "top") else c("left", "centre"))
            else switch(side, bottom = if (rot[1] == 0) c("centre", 
                "bottom") else c("left", "centre"), top = if (rot[1] == 
                0) c("centre", "top") else c("right", "centre"), 
                left = if (rot[2] == 90) c("centre", "top") else c("left", 
                  "centre"), right = if (rot[2] == 90) c("centre", 
                  "bottom") else c("right", "centre"))
        }
        switch(side, bottom = grid.text(label = labels[axid], 
            x = unit(at[axid], "native"), y = orient.factor * 
                lab.unit, rot = rot[1], check.overlap = check.overlap, 
            just = just, name = trellis.grobname("ticklabels.bottom", 
                type = "panel"), gp = gp.text), top = grid.text(label = labels[axid], 
            x = unit(at[axid], "native"), y = unit(1, "npc") - 
                orient.factor * lab.unit, rot = rot[1], check.overlap = check.overlap, 
            just = just, name = trellis.grobname("ticklabels.top", 
                type = "panel"), gp = gp.text), left = grid.text(label = labels[axid], 
            y = unit(at[axid], "native"), x = orient.factor * 
                lab.unit, rot = rot[2], check.overlap = check.overlap, 
            just = just, name = trellis.grobname("ticklabels.left", 
                type = "panel"), gp = gp.text), right = grid.text(label = labels[axid], 
            y = unit(at[axid], "native"), x = unit(1, "npc") - 
                orient.factor * lab.unit, rot = rot[2], check.overlap = check.overlap, 
            just = just, name = trellis.grobname("ticklabels.right", 
                type = "panel"), gp = gp.text))
    }
    invisible()
}


trellis.par.get <- function (name = NULL) 
{
    if (is.null(dev.list())) 
        trellis.device()
    lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    if (is.null(lattice.theme[[.Device]])) {
        trellis.device(device = .Device, new = FALSE)
        lattice.theme <- get("lattice.theme", envir = .LatticeEnv)
    }
    if (is.null(name)) 
        lattice.theme[[.Device]]
    else if (name %in% names(lattice.theme[[.Device]])) 
        lattice.theme[[.Device]][[name]]
    else NULL
}


panel.barchart <- function (x, y, box.ratio = 1, box.width = box.ratio/(1 + box.ratio), 
    horizontal = TRUE, origin = NULL, reference = TRUE, stack = FALSE, 
    groups = NULL, col = if (is.null(groups)) plot.polygon$col else superpose.polygon$col, 
    border = if (is.null(groups)) plot.polygon$border else superpose.polygon$border, 
    lty = if (is.null(groups)) plot.polygon$lty else superpose.polygon$lty, 
    lwd = if (is.null(groups)) plot.polygon$lwd else superpose.polygon$lwd, 
    ..., identifier = "barchart") 
{
    plot.polygon <- trellis.par.get("plot.polygon")
    superpose.polygon <- trellis.par.get("superpose.polygon")
    reference.line <- trellis.par.get("reference.line")
    keep <- (function(x, y, groups, subscripts, ...) {
        !is.na(x) & !is.na(y) & if (is.null(groups)) 
            TRUE
        else !is.na(groups[subscripts])
    })(x = x, y = y, groups = groups, ...)
    if (!any(keep)) 
        return()
    x <- as.numeric(x[keep])
    y <- as.numeric(y[keep])
    if (!is.null(groups)) {
        groupSub <- function(groups, subscripts, ...) groups[subscripts[keep]]
        if (!is.factor(groups)) 
            groups <- factor(groups)
        nvals <- nlevels(groups)
        groups <- as.numeric(groupSub(groups, ...))
    }
    if (horizontal) {
        if (is.null(groups)) {
            if (is.null(origin)) {
                origin <- current.panel.limits()$xlim[1]
                reference <- FALSE
            }
            height <- box.width
            if (reference) 
                panel.abline(v = origin, col = reference.line$col, 
                  lty = reference.line$lty, lwd = reference.line$lwd, 
                  identifier = paste(identifier, "abline", sep = "."))
            panel.rect(x = rep(origin, length(y)), y = y, height = rep(height, 
                length(y)), width = x - origin, border = border, 
                col = col, lty = lty, lwd = lwd, just = c("left", 
                  "centre"), identifier = identifier)
        }
        else if (stack) {
            if (!is.null(origin) && origin != 0) 
                warning("'origin' forced to 0 for stacked bars")
            col <- rep(col, length.out = nvals)
            border <- rep(border, length.out = nvals)
            lty <- rep(lty, length.out = nvals)
            lwd <- rep(lwd, length.out = nvals)
            height <- box.width
            if (reference) 
                panel.abline(v = origin, col = reference.line$col, 
                  lty = reference.line$lty, lwd = reference.line$lwd, 
                  identifier = paste(identifier, "abline", sep = "."))
            for (i in unique(y)) {
                ok <- y == i
                ord <- sort.list(groups[ok])
                pos <- x[ok][ord] > 0
                nok <- sum(pos, na.rm = TRUE)
                if (nok > 0) 
                  panel.rect(x = cumsum(c(0, x[ok][ord][pos][-nok])), 
                    y = rep(i, nok), col = col[groups[ok][ord][pos]], 
                    border = border[groups[ok][ord][pos]], lty = lty[groups[ok][ord][pos]], 
                    lwd = lwd[groups[ok][ord][pos]], height = rep(height, 
                      nok), width = x[ok][ord][pos], just = c("left", 
                      "centre"), identifier = paste(identifier, 
                      "pos", i, sep = "."))
                neg <- x[ok][ord] < 0
                nok <- sum(neg, na.rm = TRUE)
                if (nok > 0) 
                  panel.rect(x = cumsum(c(0, x[ok][ord][neg][-nok])), 
                    y = rep(i, nok), col = col[groups[ok][ord][neg]], 
                    border = border[groups[ok][ord][neg]], lty = lty[groups[ok][ord][neg]], 
                    lwd = lwd[groups[ok][ord][neg]], height = rep(height, 
                      nok), width = x[ok][ord][neg], just = c("left", 
                      "centre"), identifier = paste(identifier, 
                      "neg", i, sep = "."))
            }
        }
        else {
            if (is.null(origin)) {
                origin <- current.panel.limits()$xlim[1]
                reference <- FALSE
            }
            col <- rep(col, length.out = nvals)
            border <- rep(border, length.out = nvals)
            lty <- rep(lty, length.out = nvals)
            lwd <- rep(lwd, length.out = nvals)
            height <- box.width/nvals
            if (reference) 
                panel.abline(v = origin, col = reference.line$col, 
                  lty = reference.line$lty, lwd = reference.line$lwd, 
                  identifier = paste(identifier, "abline", sep = "."))
            for (i in unique(y)) {
                ok <- y == i
                nok <- sum(ok, na.rm = TRUE)
                panel.rect(x = rep(origin, nok), y = (i + height * 
                  (groups[ok] - (nvals + 1)/2)), col = col[groups[ok]], 
                  border = border[groups[ok]], lty = lty[groups[ok]], 
                  lwd = lwd[groups[ok]], height = rep(height, 
                    nok), width = x[ok] - origin, just = c("left", 
                    "centre"), identifier = paste(identifier, 
                    "y", i, sep = "."))
            }
        }
    }
    else {
        if (is.null(groups)) {
            if (is.null(origin)) {
                origin <- current.panel.limits()$ylim[1]
                reference <- FALSE
            }
            width <- box.width
            if (reference) 
                panel.abline(h = origin, col = reference.line$col, 
                  lty = reference.line$lty, lwd = reference.line$lwd, 
                  identifier = paste(identifier, "abline", sep = "."))
            panel.rect(x = x, y = rep(origin, length(x)), col = col, 
                border = border, lty = lty, lwd = lwd, width = rep(width, 
                  length(x)), height = y - origin, just = c("centre", 
                  "bottom"), identifier = identifier)
        }
        else if (stack) {
            if (!is.null(origin) && origin != 0) 
                warning("'origin' forced to 0 for stacked bars")
            col <- rep(col, length.out = nvals)
            border <- rep(border, length.out = nvals)
            lty <- rep(lty, length.out = nvals)
            lwd <- rep(lwd, length.out = nvals)
            width <- box.width
            if (reference) 
                panel.abline(h = origin, col = reference.line$col, 
                  lty = reference.line$lty, lwd = reference.line$lwd, 
                  identifier = paste(identifier, "abline", sep = "."))
            for (i in unique(x)) {
                ok <- x == i
                ord <- sort.list(groups[ok])
                pos <- y[ok][ord] > 0
                nok <- sum(pos, na.rm = TRUE)
                if (nok > 0) 
                  panel.rect(x = rep(i, nok), y = cumsum(c(0, 
                    y[ok][ord][pos][-nok])), col = col[groups[ok][ord][pos]], 
                    border = border[groups[ok][ord][pos]], lty = lty[groups[ok][ord][pos]], 
                    lwd = lwd[groups[ok][ord][pos]], width = rep(width, 
                      nok), height = y[ok][ord][pos], just = c("centre", 
                      "bottom"), identifier = paste(identifier, 
                      "pos", i, sep = "."))
                neg <- y[ok][ord] < 0
                nok <- sum(neg, na.rm = TRUE)
                if (nok > 0) 
                  panel.rect(x = rep(i, nok), y = cumsum(c(0, 
                    y[ok][ord][neg][-nok])), col = col[groups[ok][ord][neg]], 
                    border = border[groups[ok][ord][neg]], lty = lty[groups[ok][ord][neg]], 
                    lwd = lwd[groups[ok][ord][neg]], width = rep(width, 
                      nok), height = y[ok][ord][neg], just = c("centre", 
                      "bottom"), identifier = paste(identifier, 
                      "neg", i, sep = "."))
            }
        }
        else {
            if (is.null(origin)) {
                origin <- current.panel.limits()$ylim[1]
                reference = FALSE
            }
            col <- rep(col, length.out = nvals)
            border <- rep(border, length.out = nvals)
            lty <- rep(lty, length.out = nvals)
            lwd <- rep(lwd, length.out = nvals)
            width <- box.width/nvals
            if (reference) 
                panel.abline(h = origin, col = reference.line$col, 
                  lty = reference.line$lty, lwd = reference.line$lwd, 
                  identifier = paste(identifier, "abline", sep = "."))
            for (i in unique(x)) {
                ok <- x == i
                nok <- sum(ok, na.rm = TRUE)
                panel.rect(x = (i + width * (groups[ok] - (nvals + 
                  1)/2)), y = rep(origin, nok), col = col[groups[ok]], 
                  border = border[groups[ok]], lty = lty[groups[ok]], 
                  lwd = lwd[groups[ok]], width = rep(width, nok), 
                  height = y[ok] - origin, just = c("centre", 
                    "bottom"), identifier = paste(identifier, 
                    "x", i, sep = "."))
            }
        }
    }
}


tmd <- function (object, ...) 
UseMethod("tmd")


panel.parallel <- function (x, y, z, subscripts, groups = NULL, col = superpose.line$col, 
    lwd = superpose.line$lwd, lty = superpose.line$lty, alpha = superpose.line$alpha, 
    common.scale = FALSE, lower = sapply(z, function(x) min(as.numeric(x), 
        na.rm = TRUE)), upper = sapply(z, function(x) max(as.numeric(x), 
        na.rm = TRUE)), ..., horizontal.axis = TRUE, identifier = "parallel") 
{
    superpose.line <- trellis.par.get("superpose.line")
    reference.line <- trellis.par.get("reference.line")
    n.r <- ncol(z)
    n.c <- length(subscripts)
    if (is.null(groups)) {
        col <- rep(col, length.out = n.c)
        lty <- rep(lty, length.out = n.c)
        lwd <- rep(lwd, length.out = n.c)
        alpha <- rep(alpha, length.out = n.c)
    }
    else {
        groups <- as.factor(groups)[subscripts]
        n.g <- nlevels(groups)
        gnum <- as.numeric(groups)
        col <- rep(col, length.out = n.g)[gnum]
        lty <- rep(lty, length.out = n.g)[gnum]
        lwd <- rep(lwd, length.out = n.g)[gnum]
        alpha <- rep(alpha, length.out = n.g)[gnum]
    }
    if (is.function(lower)) 
        lower <- sapply(z, lower)
    if (is.function(upper)) 
        upper <- sapply(z, upper)
    if (common.scale) {
        lower <- min(lower)
        upper <- max(upper)
    }
    lower <- rep(lower, length.out = n.r)
    upper <- rep(upper, length.out = n.r)
    dif <- upper - lower
    if (n.r > 1) 
        if (horizontal.axis) 
            panel.segments(x0 = 0, x1 = 1, y0 = seq_len(n.r), 
                y1 = seq_len(n.r), col = reference.line$col, 
                lwd = reference.line$lwd, lty = reference.line$lty, 
                identifier = paste(identifier, "reference", sep = "."))
        else panel.segments(x0 = seq_len(n.r), x1 = seq_len(n.r), 
            y0 = 0, y1 = 1, col = reference.line$col, lwd = reference.line$lwd, 
            lty = reference.line$lty, identifier = paste(identifier, 
                "reference", sep = "."))
    else return(invisible())
    for (i in seq_len(n.r - 1)) {
        z0 <- (as.numeric(z[subscripts, i]) - lower[i])/dif[i]
        z1 <- (as.numeric(z[subscripts, i + 1]) - lower[i + 1])/dif[i + 
            1]
        if (horizontal.axis) 
            panel.segments(x0 = z0, y0 = i, x1 = z1, y1 = i + 
                1, col = col, lty = lty, lwd = lwd, alpha = alpha, 
                ..., identifier = paste(identifier, i, sep = "."))
        else panel.segments(x0 = i, y0 = z0, x1 = i + 1, y1 = z1, 
            col = col, lty = lty, lwd = lwd, alpha = alpha, ..., 
            identifier = paste(identifier, i, sep = "."))
    }
    invisible()
}


current.column <- function (prefix = lattice.getStatus("current.prefix")) 
lattice.getStatus("current.focus.column", prefix = prefix)


larrows <- function (x0 = NULL, y0 = NULL, x1, y1, x2 = NULL, y2 = NULL, 
    angle = 30, code = 2, length = 0.25, unit = "inches", ends = switch(code, 
        "first", "last", "both"), type = "open", col = add.line$col, 
    alpha = add.line$alpha, lty = add.line$lty, lwd = add.line$lwd, 
    fill = NULL, ..., identifier = NULL, name.type = "panel") 
{
    if (missing(x0)) 
        x0 <- x2
    if (missing(y0)) 
        y0 <- y2
    add.line <- trellis.par.get("add.line")
    ml <- max(length(x0), length(x1), length(y0), length(y1))
    x0 <- rep(x0, length.out = ml)
    x1 <- rep(x1, length.out = ml)
    y0 <- rep(y0, length.out = ml)
    y1 <- rep(y1, length.out = ml)
    gp <- gpar(col = col, lty = lty, lwd = lwd, alpha = alpha, 
        fill = fill, ...)
    if (hasGroupNumber()) 
        group <- list(...)$group.number
    else group <- 0
    grid.segments(x0 = x0, x1 = x1, y0 = y0, y1 = y1, name = primName("arrows", 
        identifier, name.type, group), gp = gp, arrow = if (is.null(ends)) 
        NULL
    else arrow(angle = angle, length = unit(length, unit), ends = ends, 
        type = type), default.units = "native")
}


llines <- function (x, ...) 
UseMethod("llines")


prepanel.default.xyplot <- function (x, y, type, subscripts, groups = NULL, ...) 
{
    if (any(!is.na(x)) && any(!is.na(y))) {
        ord <- order(as.numeric(x))
        if (!is.null(groups)) {
            gg <- groups[subscripts]
            dx <- unlist(lapply(split(as.numeric(x)[ord], gg[ord]), 
                diff))
            dy <- unlist(lapply(split(as.numeric(y)[ord], gg[ord]), 
                diff))
        }
        else {
            dx <- diff(as.numeric(x[ord]))
            dy <- diff(as.numeric(y[ord]))
        }
        list(xlim = scale.limits(x), ylim = scale.limits(y), 
            dx = dx, dy = dy, xat = if (is.factor(x)) sort(unique(as.numeric(x))) else NULL, 
            yat = if (is.factor(y)) sort(unique(as.numeric(y))) else NULL)
    }
    else prepanel.null()
}


xscale.components.default <- function (lim, packet.number = 0, packet.list = NULL, top = TRUE, 
    ...) 
{
    comps <- calculateAxisComponents(lim, packet.list = packet.list, 
        packet.number = packet.number, ...)
    list(num.limit = comps$num.limit, bottom = list(ticks = list(at = comps$at, 
        tck = 1), labels = list(at = comps$at, labels = comps$labels, 
        check.overlap = comps$check.overlap)), top = top)
}


panel.dotplot <- function (x, y, horizontal = TRUE, pch = if (is.null(groups)) dot.symbol$pch else sup.symbol$pch, 
    col = if (is.null(groups)) dot.symbol$col else sup.symbol$col, 
    lty = dot.line$lty, lwd = dot.line$lwd, col.line = dot.line$col, 
    levels.fos = if (horizontal) unique(y) else unique(x), groups = NULL, 
    ..., identifier = "dotplot") 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    dot.line <- trellis.par.get("dot.line")
    dot.symbol <- trellis.par.get("dot.symbol")
    sup.symbol <- trellis.par.get("superpose.symbol")
    if (horizontal) {
        panel.abline(h = levels.fos, col = col.line, lty = lty, 
            lwd = lwd, identifier = paste(identifier, "abline", 
                sep = "."))
        panel.xyplot(x = x, y = y, col = col, pch = pch, groups = groups, 
            horizontal = horizontal, ..., identifier = identifier)
    }
    else {
        panel.abline(v = levels.fos, col = col.line, lty = lty, 
            lwd = lwd, identifier = paste(identifier, "abline", 
                sep = "."))
        panel.xyplot(x = x, y = y, col = col, pch = pch, groups = groups, 
            horizontal = horizontal, ..., identifier = identifier)
    }
}


panel.lmline <- function (x, y, ..., identifier = "lmline") 
{
    if (length(x) > 1) 
        panel.abline(lm(as.numeric(y) ~ as.numeric(x)), ..., 
            identifier = identifier)
}


prepanel.lmline <- function (x, y, ...) 
{
    x <- as.numeric(x)
    y <- as.numeric(y)
    if (length(x) > 1) {
        coeff <- coef(lm(y ~ x))
        tem <- coeff[1] + coeff[2] * range(x, finite = TRUE)
        list(xlim = range(x, finite = TRUE), ylim = range(y, 
            tem, finite = TRUE), dx = diff(range(x, finite = TRUE)), 
            dy = diff(tem, finite = TRUE))
    }
    else prepanel.null()
}




## Package Data

barley <- lattice::barley		## Yield data from a Minnesota barley trial

environmental <- lattice::environmental		## Atmospheric environmental conditions in New York City

ethanol <- lattice::ethanol		## Engine exhaust fumes from burning ethanol

melanoma <- lattice::melanoma		## Melanoma skin cancer incidence

singer <- lattice::singer		## Heights of New York Choral Society singers



## Package Info

.skeleton_package_title = "Trellis Graphics for R"

.skeleton_package_version = "0.20-33"

.skeleton_package_depends = ""

.skeleton_package_imports = "grid,grDevices,graphics,stats,utils"


## Internal

.skeleton_version = 5


## EOF