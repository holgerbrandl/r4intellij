##
## Exported symobls in package `grid`
##

## Exported package methods

grid.edit <- function (gPath, ..., strict = FALSE, grep = FALSE, global = FALSE, 
    allDevices = FALSE, redraw = TRUE) 
{
    if (allDevices) 
        stop("'allDevices' not yet implemented")
    if (is.character(gPath)) 
        gPath <- gPath(gPath)
    if (!inherits(gPath, "gPath")) 
        stop("invalid 'gPath'")
    if (!is.logical(grep)) 
        stop("invalid 'grep' value")
    grep <- rep(grep, length.out = depth(gPath))
    specs <- list(...)
    editDLfromGPath(gPath, specs, strict, grep, global, redraw)
}


pop.viewport <- function (n = 1, recording = TRUE) 
{
    .Defunct("popViewport")
}


grid.path <- function (...) 
{
    grid.draw(pathGrob(...))
}


getGrob <- function (gTree, gPath, strict = FALSE, grep = FALSE, global = FALSE) 
{
    if (!inherits(gTree, "gTree")) 
        stop("it is only valid to get a child from a \"gTree\"")
    if (is.character(gPath)) 
        gPath <- gPath(gPath)
    if (!inherits(gPath, "gPath")) 
        stop("invalid 'gPath'")
    if (depth(gPath) == 1 && strict) {
        gTree$children[[gPath$name]]
    }
    else {
        if (!is.logical(grep)) 
            stop("invalid 'grep' value")
        grep <- rep(grep, length.out = depth(gPath))
        getGTree(gTree, NULL, gPath, strict, grep, global)
    }
}


childNames <- function (gTree) 
{
    if (!inherits(gTree, "gTree")) 
        stop("it is only valid to get 'children' from a \"gTree\"")
    gTree$childrenOrder
}


convertNative <- function (unit, dimension = "x", type = "location") 
{
    .Defunct("convertUnit")
}


grid.polygon <- function (x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL, 
    id.lengths = NULL, default.units = "npc", name = NULL, gp = gpar(), 
    draw = TRUE, vp = NULL) 
{
    pg <- polygonGrob(x = x, y = y, id = id, id.lengths = id.lengths, 
        default.units = default.units, name = name, gp = gp, 
        vp = vp)
    if (draw) 
        grid.draw(pg)
    invisible(pg)
}


grid.yaxis <- function (at = NULL, label = TRUE, main = TRUE, edits = NULL, 
    name = NULL, gp = gpar(), draw = TRUE, vp = NULL) 
{
    if (is.null(at)) {
        major <- NULL
        ticks <- NULL
        labels <- NULL
    }
    else {
        major <- make.yaxis.major(at, main)
        ticks <- make.yaxis.ticks(at, main)
        if (is.logical(label) && length(label) == 0) 
            stop("logical 'label' supplied of length 0")
        if (is.logical(label) && !label) 
            labels <- NULL
        else labels <- make.yaxis.labels(at, label, main)
    }
    yg <- applyEdits(gTree(at = at, label = label, main = main, 
        children = gList(major, ticks, labels), edits = edits, 
        name = name, gp = gp, vp = vp, cl = c("yaxis", "axis")), 
        edits)
    if (draw) 
        grid.draw(yg)
    invisible(yg)
}


viewport.layout <- function (vp) 
{
    vp$layout
}


grid.multipanel <- function (x = stats::runif(90), y = stats::runif(90), z = stats::runif(90), 
    nplots = 9, nrow = 5, ncol = 2, newpage = TRUE, vp = NULL) 
{
    if (newpage) 
        grid.newpage()
    if (!is.null(vp)) 
        pushViewport(vp)
    stopifnot(nplots >= 1)
    if ((missing(nrow) || missing(ncol)) && !missing(nplots)) {
        rowcol <- grDevices::n2mfrow(nplots)
        nrow <- rowcol[1L]
        ncol <- rowcol[2L]
    }
    temp.vp <- viewport(layout = grid.layout(nrow, ncol))
    pushViewport(temp.vp)
    xscale <- extendrange(x)
    yscale <- extendrange(y)
    breaks <- seq.int(min(z), max(z), length.out = nplots + 1)
    for (i in 1L:nplots) {
        col <- (i - 1)%%ncol + 1
        row <- (i - 1)%/%ncol + 1
        panel.vp <- viewport(layout.pos.row = row, layout.pos.col = col)
        panelx <- x[z >= breaks[i] & z <= breaks[i + 1]]
        panely <- y[z >= breaks[i] & z <= breaks[i + 1]]
        grid.panel(panelx, panely, range(z), c(breaks[i], breaks[i + 
            1]), xscale, yscale, axis.left = (col == 1), axis.right = (col == 
            ncol || i == nplots), axis.bottom = (row == nrow), 
            axis.top = (row == 1), axis.left.label = is.even(row), 
            axis.right.label = is.odd(row), axis.bottom.label = is.even(col), 
            axis.top.label = is.odd(col), vp = panel.vp)
    }
    grid.text("Compression Ratio", unit(0.5, "npc"), unit(-4, 
        "lines"), gp = gpar(fontsize = 20), just = "center", 
        rot = 0)
    grid.text("NOx (micrograms/J)", unit(-4, "lines"), unit(0.5, 
        "npc"), gp = gpar(fontsize = 20), just = "centre", rot = 90)
    popViewport()
    if (!is.null(vp)) 
        popViewport()
}


grid.reorder <- function (gPath, order, back = TRUE, grep = FALSE, redraw = TRUE) 
{
    grob <- grid.get(gPath, grep = grep)
    grid.set(gPath, reorderGrob(grob, order, back = back), grep = grep, 
        redraw = redraw)
}


engine.display.list <- function (on = TRUE) 
{
    grid.Call(L_setEngineDLon, as.logical(on))
}


is.grob <- function (x) 
{
    inherits(x, "grob")
}


grid.move.to <- function (x = 0, y = 0, default.units = "npc", name = NULL, draw = TRUE, 
    vp = NULL) 
{
    mtg <- moveToGrob(x = x, y = y, default.units = default.units, 
        name = name, vp = vp)
    if (draw) 
        grid.draw(mtg)
    invisible(mtg)
}


placeGrob <- function (frame, grob, row = NULL, col = NULL) 
{
    if (!inherits(frame, "frame")) 
        stop("invalid 'frame'")
    if (!is.grob(grob)) 
        stop("invalid 'grob'")
    dim <- frameDim(frame)
    if (is.null(row)) 
        row <- c(1, dim[1L])
    if (is.null(col)) 
        col <- c(1, dim[2L])
    if (length(row) == 1) 
        row <- rep(row, 2)
    if (length(col) == 1) 
        col <- rep(col, 2)
    if (min(row) < 1 || max(row) > dim[1L] || min(col) < 1 || 
        max(col) > dim[2L]) 
        stop("invalid 'row' and/or 'col' (no such cell in frame layout)")
    cgrob <- cellGrob(col, row, NULL, grob, FALSE, cellViewport(col, 
        row, NULL))
    addGrob(frame, cgrob)
}


layout.heights <- function (lay) 
{
    lay$heights
}


viewport <- function (x = unit(0.5, "npc"), y = unit(0.5, "npc"), width = unit(1, 
    "npc"), height = unit(1, "npc"), default.units = "npc", just = "centre", 
    gp = gpar(), clip = "inherit", xscale = c(0, 1), yscale = c(0, 
        1), angle = 0, layout = NULL, layout.pos.row = NULL, 
    layout.pos.col = NULL, name = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    if (!is.unit(width)) 
        width <- unit(width, default.units)
    if (!is.unit(height)) 
        height <- unit(height, default.units)
    valid.viewport(x, y, width, height, just, gp, clip, xscale, 
        yscale, angle, layout, layout.pos.row, layout.pos.col, 
        name)
}


removeGrob <- function (gTree, gPath, strict = FALSE, grep = FALSE, global = FALSE, 
    warn = TRUE) 
{
    if (!inherits(gTree, "gTree")) 
        stop("it is only valid to remove a child from a \"gTree\"")
    if (is.character(gPath)) 
        gPath <- gPath(gPath)
    if (!inherits(gPath, "gPath")) 
        stop("invalid 'gPath'")
    if (!is.logical(grep)) 
        stop("invalid 'grep' value")
    grep <- rep(grep, length.out = depth(gPath))
    if (depth(gPath) == 1) {
        result <- removeName(gTree, gPath$name, strict, grep, 
            global, warn)
    }
    else {
        name <- gPath$name
        gPath <- gPath(gPath$path)
        greppath <- grep[-length(grep)]
        grepname <- grep[length(grep)]
        result <- removeGTree(gTree, name, NULL, gPath, strict, 
            greppath, grepname, global, warn)
    }
    if (is.null(result)) {
        if (warn) 
            warning(gettextf("'gPath' (%s) not found", as.character(gPath)), 
                domain = NA)
        gTree
    }
    else {
        result
    }
}


grid.gedit <- function (..., grep = TRUE, global = TRUE) 
{
    grid.edit(..., grep = grep, global = global)
}


layout.widths <- function (lay) 
{
    lay$widths
}


xsplinePoints <- function (x) 
{
    dlon <- grid.Call(L_setDLon, FALSE)
    on.exit(grid.Call(L_setDLon, dlon))
    tempgpar <- grid.Call(L_getGPar)
    on.exit(grid.Call(L_setGPar, tempgpar), add = TRUE)
    preDraw(x)
    devPoints <- grid.Call(L_xsplinePoints, x$x, x$y, x$shape, 
        x$open, x$arrow, x$repEnds, xsplineIndex(x), 0)
    postDraw(x)
    unitPoints <- lapply(devPoints, function(x) {
        names(x) <- c("x", "y")
        x$x <- unit(x$x, "inches")
        x$y <- unit(x$y, "inches")
        x
    })
    if (length(unitPoints) == 1) 
        unitPoints <- unitPoints[[1]]
    unitPoints
}


absolute.size <- function (unit) 
{
    absolute.units(unit)
}


grid.legend <- function (..., draw = TRUE) 
{
    g <- legendGrob(...)
    if (draw) 
        grid.draw(g)
    invisible(g)
}


nullGrob <- function (x = unit(0.5, "npc"), y = unit(0.5, "npc"), default.units = "npc", 
    name = NULL, vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, name = name, vp = vp, cl = "null")
}


makeContent <- function (x) 
{
    UseMethod("makeContent")
}


convertX <- function (x, unitTo, valueOnly = FALSE) 
{
    convertUnit(x, unitTo, "x", "location", "x", "location", 
        valueOnly = valueOnly)
}


explode <- function (x) 
{
    UseMethod("explode")
}


convertY <- function (x, unitTo, valueOnly = FALSE) 
{
    convertUnit(x, unitTo, "y", "location", "y", "location", 
        valueOnly = valueOnly)
}


plotViewport <- function (margins = c(5.1, 4.1, 4.1, 2.1), ...) 
{
    margins <- rep(as.numeric(margins), length.out = 4)
    viewport(x = unit(margins[2L], "lines"), width = unit(1, 
        "npc") - unit(sum(margins[c(2, 4)]), "lines"), y = unit(margins[1L], 
        "lines"), height = unit(1, "npc") - unit(sum(margins[c(1, 
        3)]), "lines"), just = c("left", "bottom"), ...)
}


functionGrob <- function (f, n = 101, range = "x", units = "native", name = NULL, 
    gp = gpar(), vp = NULL) 
{
    grob(f = f, range = range, units = units, n = n, gp = gp, 
        vp = vp, name = name, cl = "functiongrob")
}


forceGrob <- function (x) 
{
    UseMethod("forceGrob")
}


arcCurvature <- function (theta) 
{
    if (theta < 1 || theta > 359) 
        return(0)
    angle <- 0.5 * theta/180 * pi
    1/sin(angle) - 1/tan(angle)
}


grid.abline <- function (intercept = 0, slope = 1, ...) 
{
    grid.function(function(x) list(x = x, y = intercept + slope * 
        x), ...)
}


grid.gremove <- function (..., grep = TRUE, global = TRUE) 
{
    grid.remove(..., grep = grep, global = global)
}


grid.display.list <- function (on = TRUE) 
{
    grid.Call(L_setDLon, as.logical(on))
    if (on) {
        grid.Call(L_setDisplayList, vector("list", 100L))
        grid.Call(L_setDLindex, 0L)
    }
    else grid.Call(L_setDisplayList, NULL)
}


grid.convert <- function (x, unitTo, axisFrom = "x", typeFrom = "location", axisTo = axisFrom, 
    typeTo = typeFrom, valueOnly = FALSE) 
{
    .Defunct("convertUnit")
}


setChildren <- function (x, children) 
{
    if (!inherits(x, "gTree")) 
        stop("can only set 'children' for a \"gTree\"")
    if (!is.null(children) && !inherits(children, "gList")) 
        stop("'children' must be a \"gList\"")
    if (!is.null(children)) {
        cl <- class(children)
        children <- children[!sapply(children, is.null)]
        class(children) <- cl
    }
    if (length(children)) {
        x$children <- children
        childNames <- sapply(children, childName)
        names(x$children) <- childNames
        x$childrenOrder <- childNames
    }
    else {
        x$children <- gList()
        x$childrenOrder <- character()
    }
    x
}


rasterGrob <- function (image, x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
    width = NULL, height = NULL, just = "centre", hjust = NULL, 
    vjust = NULL, interpolate = TRUE, default.units = "npc", 
    name = NULL, gp = gpar(), vp = NULL) 
{
    if (inherits(image, "nativeRaster")) 
        raster <- image
    else raster <- as.raster(image)
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    if (!is.null(width) && !is.unit(width)) 
        width <- unit(width, default.units)
    if (!is.null(height) && !is.unit(height)) 
        height <- unit(height, default.units)
    grob(raster = raster, x = x, y = y, width = width, height = height, 
        just = just, hjust = hjust, vjust = vjust, interpolate = interpolate, 
        name = name, gp = gp, vp = vp, cl = "rastergrob")
}


addGrob <- function (gTree, child, gPath = NULL, strict = FALSE, grep = FALSE, 
    global = FALSE, warn = TRUE) 
{
    if (!inherits(child, "grob")) 
        stop("it is only valid to add a 'grob' to a \"gTree\"")
    if (is.null(gPath)) {
        addToGTree(gTree, child)
    }
    else {
        if (is.character(gPath)) 
            gPath <- gPath(gPath)
        if (!inherits(gTree, "gTree")) 
            stop("it is only valid to add a child to a \"gTree\"")
        if (!is.logical(grep)) 
            stop("invalid 'grep' value")
        grep <- rep(grep, length.out = depth(gPath))
        result <- addGTree(gTree, child, NULL, gPath, strict, 
            grep, global)
        if (is.null(result)) {
            if (warn) 
                warning(gettextf("'gPath' (%s) not found", as.character(gPath)), 
                  domain = NA)
            gTree
        }
        else {
            result
        }
    }
}


vpTree <- function (parent, children) 
{
    if (viewportorpath(parent) && inherits(children, "vpList")) {
        tree <- list(parent = parent, children = children)
        class(tree) <- c("vpTree", "viewport")
        tree
    }
    else {
        stop("'parent' must be a viewport and 'children' must be a 'vpList' in 'vpTree'")
    }
}


grid.grob <- function (list.struct, cl = NULL, draw = TRUE) 
.Defunct("grob")


xaxisGrob <- function (at = NULL, label = TRUE, main = TRUE, edits = NULL, 
    name = NULL, gp = gpar(), vp = NULL) 
{
    grid.xaxis(at = at, label = label, main = main, edits = edits, 
        name = name, gp = gp, draw = FALSE, vp = vp)
}


depth <- function (x, ...) 
{
    UseMethod("depth")
}


grid.locator <- function (unit = "native") 
{
    location <- c(grid.Call(L_locator), 1)
    if (is.na(location[1L])) 
        invisible(NULL)
    else {
        transform <- solve(current.transform())
        location <- (location %*% transform)
        location <- unit(location/location[3L], "inches")
        list(x = convertX(location[1L], unit), y = convertY(location[2L], 
            unit))
    }
}


grid.gget <- function (..., grep = TRUE, global = TRUE) 
{
    grid.get(..., grep = grep, global = global)
}


calcStringMetric <- function (text) 
{
    metric <- grid.Call(L_stringMetric, text)
    names(metric) <- c("ascent", "descent", "width")
    metric
}


rectGrob <- function (x = unit(0.5, "npc"), y = unit(0.5, "npc"), width = unit(1, 
    "npc"), height = unit(1, "npc"), just = "centre", hjust = NULL, 
    vjust = NULL, default.units = "npc", name = NULL, gp = gpar(), 
    vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    if (!is.unit(width)) 
        width <- unit(width, default.units)
    if (!is.unit(height)) 
        height <- unit(height, default.units)
    grob(x = x, y = y, width = width, height = height, just = just, 
        hjust = hjust, vjust = vjust, name = name, gp = gp, vp = vp, 
        cl = "rect")
}


grid.place <- function (gPath, grob, row = 1, col = 1, redraw = TRUE) 
{
    grid.set(gPath, placeGrob(grid.get(gPath), grob, row, col), 
        redraw)
}


grid.newpage <- function (recording = TRUE) 
{
    for (fun in getHook("before.grid.newpage")) {
        if (is.character(fun)) 
            fun <- get(fun)
        try(fun())
    }
    .Call(L_newpagerecording)
    .Call(L_newpage)
    .Call(L_initGPar)
    .Call(L_initViewportStack)
    if (recording) {
        .Call(L_initDisplayList)
        grDevices:::recordPalette()
        for (fun in getHook("grid.newpage")) {
            if (is.character(fun)) 
                fun <- get(fun)
            try(fun())
        }
    }
    invisible()
}


nestedListing <- function (x, gindent = "  ", vpindent = gindent) 
{
    makePrefix <- function(indent, depth) {
        indents <- rep(indent, length(depth))
        indents <- mapply(rep, indents, depth)
        sapply(indents, paste, collapse = "")
    }
    if (!inherits(x, "flatGridListing")) 
        stop("invalid listing")
    cat(paste0(makePrefix(gindent, x$gDepth), makePrefix(vpindent, 
        x$vpDepth), x$name), sep = "\n")
}


grid.revert <- function (x, ...) 
{
    UseMethod("grid.revert")
}


grid.grabExpr <- function (expr, warn = 2, wrap = FALSE, ...) 
{
    pdf(file = NULL)
    on.exit(dev.off())
    eval(expr)
    grabDL(warn, wrap, ...)
}


grid.bezier <- function (...) 
{
    grid.draw(bezierGrob(...))
}


grid.xaxis <- function (at = NULL, label = TRUE, main = TRUE, edits = NULL, 
    name = NULL, gp = gpar(), draw = TRUE, vp = NULL) 
{
    if (is.null(at)) {
        major <- NULL
        ticks <- NULL
        labels <- NULL
    }
    else {
        major <- make.xaxis.major(at, main)
        ticks <- make.xaxis.ticks(at, main)
        if (is.logical(label) && length(label) == 0) 
            stop("logical 'label' supplied of length 0")
        if (is.logical(label) && !label) 
            labels <- NULL
        else labels <- make.xaxis.labels(at, label, main)
    }
    xg <- applyEdits(gTree(at = at, label = label, main = main, 
        children = gList(major, ticks, labels), edits = edits, 
        name = name, gp = gp, vp = vp, cl = c("xaxis", "axis")), 
        edits)
    if (draw) 
        grid.draw(xg)
    invisible(xg)
}


gpar <- function (...) 
{
    gp <- validGP(list(...))
    class(gp) <- "gpar"
    gp
}


yDetails <- function (x, theta) 
{
    UseMethod("yDetails")
}


arrow <- function (angle = 30, length = unit(0.25, "inches"), ends = "last", 
    type = "open") 
{
    angle <- as.numeric(angle)
    if (!is.unit(length)) 
        stop("'length' must be a 'unit' object")
    ends <- as.integer(match(ends, c("first", "last", "both")))
    type <- as.integer(match(type, c("open", "closed")))
    if (anyNA(ends) || anyNA(type) || length(ends) == 0 || length(type) == 
        0) 
        stop("invalid 'ends' or 'type' argument")
    a <- list(angle = angle, length = length, ends = ends, type = type)
    class(a) <- "arrow"
    a
}


grid.circle <- function (x = 0.5, y = 0.5, r = 0.5, default.units = "npc", name = NULL, 
    gp = gpar(), draw = TRUE, vp = NULL) 
{
    cg <- circleGrob(x = x, y = y, r = r, default.units = default.units, 
        name = name, gp = gp, vp = vp)
    if (draw) 
        grid.draw(cg)
    invisible(cg)
}


gPath <- function (...) 
{
    names <- c(...)
    gPathFromVector(names)
}


grid.panel <- function (x = stats::runif(10), y = stats::runif(10), zrange = c(0, 
    1), zbin = stats::runif(2), xscale = extendrange(x), yscale = extendrange(y), 
    axis.left = TRUE, axis.left.label = TRUE, axis.right = FALSE, 
    axis.right.label = TRUE, axis.bottom = TRUE, axis.bottom.label = TRUE, 
    axis.top = FALSE, axis.top.label = TRUE, vp = NULL) 
{
    if (!is.null(vp)) 
        pushViewport(vp)
    temp.vp <- viewport(layout = grid.layout(2, 1, heights = unit(c(1, 
        1), c("lines", "null"))))
    pushViewport(temp.vp)
    strip.vp <- viewport(layout.pos.row = 1, layout.pos.col = 1, 
        xscale = xscale)
    pushViewport(strip.vp)
    grid.strip(range.full = zrange, range.thumb = zbin)
    grid.rect()
    if (axis.top) 
        grid.xaxis(main = FALSE, label = axis.top.label)
    popViewport()
    plot.vp <- viewport(layout.pos.row = 2, layout.pos.col = 1, 
        xscale = xscale, yscale = yscale)
    pushViewport(plot.vp)
    grid.grill()
    grid.points(x, y, gp = gpar(col = "blue"))
    grid.rect()
    if (axis.left) 
        grid.yaxis(label = axis.left.label)
    if (axis.right) 
        grid.yaxis(main = FALSE, label = axis.right.label)
    if (axis.bottom) 
        grid.xaxis(label = axis.bottom.label)
    popViewport(2)
    if (!is.null(vp)) 
        popViewport()
    invisible(list(strip.vp = strip.vp, plot.vp = plot.vp))
}


current.rotation <- function () 
{
    grid.Call(L_currentViewport)$rotation
}


grid.points <- function (x = stats::runif(10), y = stats::runif(10), pch = 1, 
    size = unit(1, "char"), default.units = "native", name = NULL, 
    gp = gpar(), draw = TRUE, vp = NULL) 
{
    pg <- pointsGrob(x = x, y = y, pch = pch, size = size, default.units = default.units, 
        name = name, gp = gp, vp = vp)
    if (draw) 
        grid.draw(pg)
    invisible(pg)
}


seekViewport <- function (name, recording = TRUE) 
{
    upViewport(0, recording = recording)
    downViewport(name, recording = recording)
}


grid.collection <- function (..., gp = gpar(), draw = TRUE, vp = NULL) 
{
    .Defunct("gTree")
}


current.parent <- function (n = 1) 
{
    if (n < 1) 
        stop("Invalid number of generations")
    vp <- grid.Call(L_currentViewport)
    generation <- 1
    while (generation <= n) {
        if (is.null(vp)) 
            stop("Invalid number of generations")
        vp <- vp$parent
        generation <- generation + 1
    }
    if (!is.null(vp)) 
        vpFromPushedvp(vp)
    else vp
}


layout.torture <- function () 
{
    top.vp <- viewport(y = 0, height = unit(1, "npc") - unit(1.5, 
        "lines"), just = c("centre", "bottom"))
    do.label <- function(label) {
        grid.rect(y = 1, height = unit(1.5, "lines"), just = c("center", 
            "top"))
        grid.text(label, y = unit(1, "npc") - unit(1, "lines"), 
            gp = gpar(font = 2))
    }
    grid.show.layout(grid.layout(3, 2), vp = top.vp)
    do.label("All dimensions relative -- no respect")
    grid.show.layout(grid.layout(3, 2, respect = TRUE), vp = top.vp)
    do.label("All dimensions relative -- full respect")
    grid.show.layout(grid.layout(3, 2, respect = matrix(c(1, 
        0, 0, 0, 0, 0), 3L, 2L, TRUE)), vp = top.vp)
    do.label("All dimensions relative -- only top-left cell respected")
    grid.show.layout(grid.layout(3, 2, respect = matrix(c(1, 
        0, 0, 0, 0, 1), 3L, 2L, TRUE)), vp = top.vp)
    do.label("All relative -- top-left, bottom-right respected")
    grid.show.layout(grid.layout(2, 3, widths = unit(c(2, 4, 
        1), c("null", "cm", "null")), heights = unit(c(6, 4), 
        c("cm", "null"))), vp = top.vp)
    do.label("Absolute and relative -- no respect")
    grid.show.layout(grid.layout(2, 3, widths = unit(c(2, 4, 
        1), c("null", "cm", "null")), heights = unit(c(6, 4), 
        c("cm", "null")), respect = TRUE), vp = top.vp)
    do.label("Absolute and relative -- full respect")
    grid.show.layout(grid.layout(2, 3, widths = unit(c(2, 4, 
        1), c("null", "cm", "null")), heights = unit(c(6, 4), 
        c("cm", "null")), respect = matrix(c(0, 0, 0, 0, 0, 1), 
        2L, 3L, TRUE)), vp = top.vp)
    do.label("Absolute and relative -- bottom-right respected")
}


pushViewport <- function (..., recording = TRUE) 
{
    if (missing(...)) 
        stop("must specify at least one viewport")
    else {
        vps <- list(...)
        lapply(vps, push.vp, recording)
    }
    invisible()
}


grobName <- function (grob = NULL, prefix = "GRID") 
{
    if (is.null(grob)) 
        grobAutoName(prefix)
    else {
        if (!is.grob(grob)) 
            stop("invalid 'grob' argument")
        else grobAutoName(prefix, class(grob)[1L])
    }
}


grid.cap <- function () 
{
    grid.Call(L_cap)
}


grid.clip <- function (...) 
{
    grid.draw(clipGrob(...))
}


grobWidth <- function (x) 
{
    UseMethod("grobWidth")
}


vpList <- function (...) 
{
    vps <- list(...)
    vpListFromList(vps)
}


xDetails <- function (x, theta) 
{
    UseMethod("xDetails")
}


grid.copy <- function (grob) 
{
    warning("this function is redundant and will disappear in future versions", 
        domain = NA)
    grob
}


unit.length <- function (unit) 
{
    warning("'unit.length' has been deprecated in favour of a unit method for the generic length function", 
        domain = NA)
    length(unit)
}


current.transform <- function () 
{
    grid.Call(L_currentViewport)$trans
}


push.viewport <- function (..., recording = TRUE) 
{
    .Defunct("pushViewport")
}


resolveRasterSize <- function (x) 
{
    if (is.null(x$width)) {
        if (is.null(x$height)) {
            rasterRatio <- dim(x$raster)[1]/dim(x$raster)[2]
            vpWidth <- convertWidth(unit(1, "npc"), "inches", 
                valueOnly = TRUE)
            vpHeight <- convertHeight(unit(1, "npc"), "inches", 
                valueOnly = TRUE)
            vpRatio <- vpHeight/vpWidth
            if (rasterRatio > vpRatio) {
                x$height <- unit(vpHeight, "inches")
                x$width <- unit(vpHeight * dim(x$raster)[2]/dim(x$raster)[1], 
                  "inches")
            }
            else {
                x$width <- unit(vpWidth, "inches")
                x$height <- unit(vpWidth * dim(x$raster)[1]/dim(x$raster)[2], 
                  "inches")
            }
        }
        else {
            h <- convertHeight(x$height, "inches", valueOnly = TRUE)
            x$width <- unit(h * dim(x$raster)[2]/dim(x$raster)[1], 
                "inches")
        }
    }
    else {
        if (is.null(x$height)) {
            w <- convertWidth(x$width, "inches", valueOnly = TRUE)
            x$height <- unit(w * dim(x$raster)[1]/dim(x$raster)[2], 
                "inches")
        }
    }
    x
}


grid.polyline <- function (...) 
{
    grid.draw(polylineGrob(...))
}


grid.arrows <- function (x = c(0.25, 0.75), y = 0.5, default.units = "npc", 
    grob = NULL, angle = 30, length = unit(0.25, "inches"), ends = "last", 
    type = "open", name = NULL, gp = gpar(), draw = TRUE, vp = NULL) 
{
    .Defunct(msg = "'grid.arrows' is defunct; use 'arrow' arguments to line drawing functions")
}


unit <- function (x, units, data = NULL) 
{
    x <- as.numeric(x)
    units <- as.character(units)
    if (length(x) == 0 || length(units) == 0) 
        stop("'x' and 'units' must have length > 0")
    valid.unit(x, units, recycle.data(data, FALSE, length(x), 
        units))
}


grid.pretty <- function (range) 
{
    if (!is.numeric(range)) 
        stop("'range' must be numeric")
    .Call(L_pretty, range)
}


popViewport <- function (n = 1, recording = TRUE) 
{
    if (n < 0) 
        stop("must pop at least one viewport")
    if (n == 0) 
        n <- vpDepth()
    if (n > 0) {
        grid.Call.graphics(L_unsetviewport, as.integer(n))
        if (recording) {
            class(n) <- "pop"
            record(n)
        }
    }
    invisible()
}


grid.convertHeight <- function (x, unitTo, valueOnly = FALSE) 
{
    .Defunct("convertHeight")
}


grid.refresh <- function () 
{
    draw.all()
}


grid.text <- function (label, x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
    just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, 
    default.units = "npc", name = NULL, gp = gpar(), draw = TRUE, 
    vp = NULL) 
{
    tg <- textGrob(label = label, x = x, y = y, just = just, 
        hjust = hjust, vjust = vjust, rot = rot, check.overlap = check.overlap, 
        default.units = default.units, name = name, gp = gp, 
        vp = vp)
    if (draw) 
        grid.draw(tg)
    invisible(tg)
}


grid.convertX <- function (x, unitTo, valueOnly = FALSE) 
{
    .Defunct("convertX")
}


grid.convertY <- function (x, unitTo, valueOnly = FALSE) 
{
    .Defunct("convertY")
}


applyEdits <- function (x, edits) 
{
    if (is.null(edits)) {
        x
    }
    else {
        if (is.gEdit(edits)) 
            applyEdit(x, edits)
        else {
            if (!inherits(edits, "gEditList")) 
                stop("invalid 'edit' information")
            for (i in edits) x <- applyEdits(x, i)
            x
        }
    }
}


grid.show.layout <- function (l, newpage = TRUE, vp.ex = 0.8, bg = "light grey", 
    cell.border = "blue", cell.fill = "light blue", cell.label = TRUE, 
    label.col = "blue", unit.col = "red", vp = NULL) 
{
    if (!is.layout(l)) 
        stop("'l' must be a layout")
    if (newpage) 
        grid.newpage()
    if (!is.null(vp)) 
        pushViewport(vp)
    grid.rect(gp = gpar(col = NULL, fill = bg))
    vp.mid <- viewport(0.5, 0.5, vp.ex, vp.ex, layout = l)
    pushViewport(vp.mid)
    grid.rect(gp = gpar(fill = "white"))
    gp.red <- gpar(col = unit.col)
    for (i in 1L:l$nrow) for (j in 1L:l$ncol) {
        vp.inner <- viewport(layout.pos.row = i, layout.pos.col = j)
        pushViewport(vp.inner)
        grid.rect(gp = gpar(col = cell.border, fill = cell.fill))
        if (cell.label) 
            grid.text(paste0("(", i, ", ", j, ")"), gp = gpar(col = label.col))
        if (j == 1) 
            grid.text(as.character(l$heights[i, top = FALSE]), 
                gp = gp.red, just = c("right", "centre"), x = unit(-0.05, 
                  "inches"), y = unit(0.5, "npc"), rot = 0)
        if (i == l$nrow) 
            grid.text(as.character(l$widths[j, top = FALSE]), 
                gp = gp.red, just = c("centre", "top"), x = unit(0.5, 
                  "npc"), y = unit(-0.05, "inches"), rot = 0)
        if (j == l$ncol) 
            grid.text(as.character(l$heights[i, top = FALSE]), 
                gp = gp.red, just = c("left", "centre"), x = unit(1, 
                  "npc") + unit(0.05, "inches"), y = unit(0.5, 
                  "npc"), rot = 0)
        if (i == 1) 
            grid.text(as.character(l$widths[j, top = FALSE]), 
                gp = gp.red, just = c("centre", "bottom"), x = unit(0.5, 
                  "npc"), y = unit(1, "npc") + unit(0.05, "inches"), 
                rot = 0)
        popViewport()
    }
    popViewport()
    if (!is.null(vp)) 
        popViewport()
    invisible(vp.mid)
}


stringAscent <- function (string) 
{
    n <- length(string)
    if (is.language(string)) {
        data <- vector("list", n)
        for (i in 1L:n) data[[i]] <- string[i]
    }
    else {
        data <- as.list(as.character(string))
    }
    unit(rep_len(1, n), "strascent", data = data)
}


valid.just <- function (just) 
{
    if (is.character(just)) 
        valid.charjust(just)
    else {
        valid.numjust(as.numeric(just))
    }
}


grid.strip <- function (label = "whatever", range.full = c(0, 1), range.thumb = c(0.3, 
    0.6), fill = "#FFBF00", thumb = "#FF8000", vp = NULL) 
{
    diff.full <- diff(range.full)
    diff.thumb <- diff(range.thumb)
    if (!is.null(vp)) 
        pushViewport(vp)
    grid.rect(gp = gpar(col = NULL, fill = fill))
    grid.rect((range.thumb[1L] - range.full[1L])/diff.full, 0, 
        diff.thumb/diff.full, 1, just = c("left", "bottom"), 
        gp = gpar(col = NULL, fill = thumb))
    grid.text(as.character(label))
    if (!is.null(vp)) 
        popViewport()
}


dataViewport <- function (xData = NULL, yData = NULL, xscale = NULL, yscale = NULL, 
    extension = 0.05, ...) 
{
    extension <- rep(extension, length.out = 2)
    if (is.null(xscale)) {
        if (is.null(xData)) 
            stop("must specify at least one of 'x' or 'xscale'")
        xscale <- extendrange(xData, f = extension[1L])
    }
    if (is.null(yscale)) {
        if (is.null(yData)) 
            stop("must specify at least one of 'y' or 'yscale'")
        yscale <- extendrange(yData, f = extension[2L])
    }
    viewport(xscale = xscale, yscale = yscale, ...)
}


pointsGrob <- function (x = stats::runif(10), y = stats::runif(10), pch = 1, 
    size = unit(1, "char"), default.units = "native", name = NULL, 
    gp = gpar(), vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, pch = pch, size = size, name = name, gp = gp, 
        vp = vp, cl = "points")
}


resolveHJust <- function (just, hjust) 
{
    if (is.null(hjust) || length(hjust) == 0) 
        valid.just(just)[1L]
    else hjust
}


grobAscent <- function (x) 
{
    UseMethod("grobAscent")
}


reorderGrob <- function (x, order, back = TRUE) 
{
    if (!inherits(x, "gTree")) 
        stop("can only reorder 'children' for a \"gTree\"")
    order <- unique(order)
    oldOrder <- x$childrenOrder
    N <- length(oldOrder)
    if (is.character(order)) {
        order <- match(order, x$childrenOrder)
    }
    if (is.numeric(order)) {
        if (any(!is.finite(order)) || !(all(order %in% 1:N))) {
            stop("Invalid 'order'")
        }
        if (back) {
            newOrder <- c(x$childrenOrder[order], x$childrenOrder[-order])
        }
        else {
            newOrder <- c(x$childrenOrder[-order], x$childrenOrder[order])
        }
    }
    x$childrenOrder <- newOrder
    x
}


gEdit <- function (...) 
{
    edit <- list(...)
    class(edit) <- "gEdit"
    edit
}


segmentsGrob <- function (x0 = unit(0, "npc"), y0 = unit(0, "npc"), x1 = unit(1, 
    "npc"), y1 = unit(1, "npc"), default.units = "npc", arrow = NULL, 
    name = NULL, gp = gpar(), vp = NULL) 
{
    if (!is.unit(x0)) 
        x0 <- unit(x0, default.units)
    if (!is.unit(x1)) 
        x1 <- unit(x1, default.units)
    if (!is.unit(y0)) 
        y0 <- unit(y0, default.units)
    if (!is.unit(y1)) 
        y1 <- unit(y1, default.units)
    grob(x0 = x0, y0 = y0, x1 = x1, y1 = y1, arrow = arrow, name = name, 
        gp = gp, vp = vp, cl = "segments")
}


polylineGrob <- function (x = unit(c(0, 1), "npc"), y = unit(c(0, 1), "npc"), 
    id = NULL, id.lengths = NULL, default.units = "npc", arrow = NULL, 
    name = NULL, gp = gpar(), vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, id = id, id.lengths = id.lengths, arrow = arrow, 
        name = name, gp = gp, vp = vp, cl = "polyline")
}


draw.details <- function (x, recording) 
{
    .Defunct("drawDetails")
}


grid.frame <- function (layout = NULL, name = NULL, gp = gpar(), vp = NULL, 
    draw = TRUE) 
{
    fg <- frameGrob(layout = layout, name = name, gp = gp, vp = vp)
    if (draw) 
        grid.draw(fg)
    invisible(fg)
}


editGrob <- function (grob, gPath = NULL, ..., strict = FALSE, grep = FALSE, 
    global = FALSE, warn = TRUE) 
{
    specs <- list(...)
    if (is.null(gPath)) {
        editThisGrob(grob, specs)
    }
    else {
        if (is.character(gPath)) 
            gPath <- gPath(gPath)
        if (!inherits(grob, "gTree")) 
            stop("it is only valid to edit a child of a \"gTree\"")
        if (!is.logical(grep)) 
            stop("invalid 'grep' value")
        grep <- rep(grep, length.out = depth(gPath))
        result <- editGTree(grob, specs, NULL, gPath, strict, 
            grep, global)
        if (is.null(result)) {
            if (warn) 
                warning(gettextf("'gPath' (%s) not found", as.character(gPath)), 
                  domain = NA)
            grob
        }
        else {
            result
        }
    }
}


editDetails <- function (x, specs) 
{
    UseMethod("editDetails")
}


showViewport <- function (vp = NULL, recurse = TRUE, depth = NULL, newpage = FALSE, 
    leaves = FALSE, col = rgb(0, 0, 1, 0.2), fill = rgb(0, 0, 
        1, 0.1), label = TRUE, nrow = 3, ncol = nrow) 
{
    cvpt <- current.vpTree()
    if (is.null(vp)) 
        vp <- cvpt
    if (newpage == FALSE && leaves == TRUE) 
        stop("must start new page if showing leaves separately")
    if (newpage) {
        grid.newpage()
    }
    if (!recurse) 
        depth <- 1
    if (leaves) {
        if (inherits(vp, "vpPath")) 
            showVP(vp, TRUE, cvpt, depth, col, fill, label)
        else showVPmatrix(vp, cvpt, depth, col, fill, label, 
            nrow, ncol)
    }
    else {
        showVP(vp, newpage, cvpt, depth, col, fill, label)
    }
    invisible()
}


lineToGrob <- function (x = 1, y = 1, default.units = "npc", arrow = NULL, 
    name = NULL, gp = gpar(), vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, arrow = arrow, name = name, gp = gp, vp = vp, 
        cl = "line.to")
}


moveToGrob <- function (x = 0, y = 0, default.units = "npc", name = NULL, vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, name = name, vp = vp, cl = "move.to")
}


applyEdit <- function (x, edit) 
{
    if (is.null(edit)) {
        x
    }
    else {
        if (!is.gEdit(edit)) 
            stop("invalid 'edit' information")
        newx <- do.call("editGrob", c(list(x), edit))
        if (is.null(newx)) 
            x
        else newx
    }
}


unit.pmin <- function (...) 
{
    select.i <- function(unit, i) {
        unit[i, top = FALSE]
    }
    x <- list(...)
    numargs <- length(x)
    if (numargs == 0L) 
        stop("Zero arguments where at least one expected")
    maxlength <- 0L
    for (i in seq_len(numargs)) if (length(x[[i]]) > maxlength) 
        maxlength <- length(x[[i]])
    result <- min(unit.list.from.list(lapply(x, select.i, 1L)))
    if (maxlength > 1L) 
        for (i in 2L:maxlength) result <- unit.c(result, min(unit.list.from.list(lapply(x, 
            select.i, i))))
    result
}


grid.ls <- function (x = NULL, grobs = TRUE, viewports = FALSE, fullNames = FALSE, 
    recursive = TRUE, print = TRUE, flatten = TRUE, ...) 
{
    if (is.null(x)) {
        listing <- gridListDL(grobs = grobs, viewports = viewports, 
            fullNames = fullNames, recursive = recursive)
    }
    else {
        listing <- gridList(x, grobs = grobs, viewports = viewports, 
            fullNames = fullNames, recursive = recursive)
    }
    if (flatten) {
        listing <- flattenListing(listing)
    }
    if (is.logical(print)) {
        if (print) {
            print(listing)
        }
    }
    else if (is.function(print)) {
        print(listing, ...)
    }
    else {
        stop("invalid 'print' argument")
    }
    invisible(listing)
}


grobPathListing <- function (x, ...) 
{
    subset <- grep("^g", x$type)
    if (length(subset)) {
        cl <- class(x)
        subListing <- lapply(x, "[", subset)
        class(subListing) <- cl
        pathListing(subListing, ...)
    }
}


grid.pack <- function (gPath, grob, redraw = TRUE, side = NULL, row = NULL, 
    row.before = NULL, row.after = NULL, col = NULL, col.before = NULL, 
    col.after = NULL, width = NULL, height = NULL, force.width = FALSE, 
    force.height = FALSE, border = NULL, dynamic = FALSE) 
{
    grid.set(gPath, packGrob(grid.get(gPath), grob, side, row, 
        row.before, row.after, col, col.before, col.after, width, 
        height, force.width, force.height, border), redraw)
}


convertWidth <- function (x, unitTo, valueOnly = FALSE) 
{
    convertUnit(x, unitTo, "x", "dimension", "x", "dimension", 
        valueOnly = valueOnly)
}


stringWidth <- function (string) 
{
    n <- length(string)
    if (is.language(string)) {
        data <- vector("list", n)
        for (i in 1L:n) data[[i]] <- string[i]
    }
    else {
        data <- as.list(as.character(string))
    }
    unit(rep_len(1, n), "strwidth", data = data)
}


linesGrob <- function (x = unit(c(0, 1), "npc"), y = unit(c(0, 1), "npc"), 
    default.units = "npc", arrow = NULL, name = NULL, gp = gpar(), 
    vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, arrow = arrow, name = name, gp = gp, vp = vp, 
        cl = "lines")
}


textGrob <- function (label, x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
    just = "centre", hjust = NULL, vjust = NULL, rot = 0, check.overlap = FALSE, 
    default.units = "npc", name = NULL, gp = gpar(), vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(label = label, x = x, y = y, just = just, hjust = hjust, 
        vjust = vjust, rot = rot, check.overlap = check.overlap, 
        name = name, gp = gp, vp = vp, cl = "text")
}


grob <- function (..., name = NULL, gp = NULL, vp = NULL, cl = NULL) 
{
    g <- list(..., name = name, gp = gp, vp = vp)
    if (!is.null(cl) && !is.character(cl)) 
        stop("invalid 'grob' class")
    class(g) <- c(cl, "grob", "gDesc")
    validGrob(g)
}


grid.add <- function (gPath, child, strict = FALSE, grep = FALSE, global = FALSE, 
    allDevices = FALSE, redraw = TRUE) 
{
    if (allDevices) 
        stop("'allDevices' not yet implemented")
    if (is.character(gPath)) 
        gPath <- gPath(gPath)
    if (!inherits(gPath, "gPath")) 
        stop("invalid 'gPath'")
    if (!is.logical(grep)) 
        stop("invalid 'grep' value")
    grep <- rep(grep, length.out = depth(gPath))
    addDLfromGPath(gPath, child, strict, grep, global, redraw)
}


grid.delay <- function (expr, list, name = NULL, gp = NULL, vp = NULL) 
{
    grid.draw(gTree(expr = substitute(expr), list = list, name = name, 
        gp = gp, vp = vp, cl = "delayedgrob"))
}


makeContext <- function (x) 
{
    UseMethod("makeContext")
}


grid.curve <- function (...) 
{
    grid.draw(curveGrob(...))
}


grid.force <- function (x, ...) 
{
    UseMethod("grid.force")
}


arrowsGrob <- function (x = c(0.25, 0.75), y = 0.5, default.units = "npc", 
    grob = NULL, angle = 30, length = unit(0.25, "inches"), ends = "last", 
    type = "open", name = NULL, gp = gpar(), vp = NULL) 
{
    .Defunct(msg = "'arrowsGrob' is defunct; use 'arrow' arguments to line drawing functions")
}


current.vpTree <- function (all = TRUE) 
{
    cpvp <- grid.Call(L_currentViewport)
    moving <- all && vpDepth() > 0
    if (moving) {
        savedname <- cpvp$name
        upViewport(0, recording = FALSE)
        cpvp <- grid.Call(L_currentViewport)
    }
    tree <- vpTreeFromNode(cpvp)
    if (moving) {
        downViewport(savedname, recording = FALSE)
    }
    tree
}


grid.record <- function (expr, list, name = NULL, gp = NULL, vp = NULL) 
{
    grid.draw(grob(expr = substitute(expr), list = list, name = name, 
        gp = gp, vp = vp, cl = "recordedGrob"))
}


convertHeight <- function (x, unitTo, valueOnly = FALSE) 
{
    convertUnit(x, unitTo, "y", "dimension", "y", "dimension", 
        valueOnly = valueOnly)
}


legendGrob <- function (labels, nrow, ncol, byrow = FALSE, do.lines = has.lty || 
    has.lwd, lines.first = TRUE, hgap = unit(1, "lines"), vgap = unit(1, 
    "lines"), default.units = "lines", pch, gp = gpar(), vp = NULL) 
{
    labels <- as.graphicsAnnot(labels)
    labels <- if (is.character(labels)) 
        as.list(labels)
    else as.expression(labels)
    nkeys <- if (is.call(labels)) 
        1
    else length(labels)
    if (nkeys == 0) 
        return(nullGrob(vp = vp))
    if (!is.unit(hgap)) 
        hgap <- unit(hgap, default.units)
    if (length(hgap) != 1) 
        stop("'hgap' must be single unit")
    if (!is.unit(vgap)) 
        vgap <- unit(vgap, default.units)
    if (length(vgap) != 1) 
        stop("'vgap' must be single unit")
    miss.nrow <- missing(nrow)
    miss.ncol <- missing(ncol)
    if (miss.nrow && miss.ncol) {
        ncol <- 1
        nrow <- nkeys
    }
    else if (miss.nrow && !miss.ncol) 
        nrow <- ceiling(nkeys/ncol)
    else if (!miss.nrow && miss.ncol) 
        ncol <- ceiling(nkeys/nrow)
    if (nrow < 1) 
        stop("'nrow' must be >= 1")
    if (ncol < 1) 
        stop("'ncol' must be >= 1")
    if (nrow * ncol < nkeys) 
        stop("nrow * ncol < #{legend labels}")
    if (has.pch <- !missing(pch) && length(pch) > 0) 
        pch <- rep_len(pch, nkeys)
    if (doGP <- length(nmgp <- names(gp)) > 0) {
        if (has.lty <- "lty" %in% nmgp) 
            gp$lty <- rep_len(gp$lty, nkeys)
        if (has.lwd <- "lwd" %in% nmgp) 
            gp$lwd <- rep_len(gp$lwd, nkeys)
        if (has.col <- "col" %in% nmgp) 
            gp$col <- rep_len(gp$col, nkeys)
        if (has.fill <- "fill" %in% nmgp) 
            gp$fill <- rep_len(gp$fill, nkeys)
    }
    else {
        gpi <- gp
        if (missing(do.lines)) 
            do.lines <- FALSE
    }
    u0 <- unit(0, "npc")
    u1 <- unit(1, "char")
    ord <- if (lines.first) 
        1:2
    else 2:1
    fg <- frameGrob(vp = vp)
    for (i in seq_len(nkeys)) {
        if (doGP) {
            gpi <- gp
            if (has.lty) 
                gpi$lty <- gp$lty[i]
            if (has.lwd) 
                gpi$lwd <- gp$lwd[i]
            if (has.col) 
                gpi$col <- gp$col[i]
            if (has.fill) 
                gpi$fill <- gp$fill[i]
        }
        if (byrow) {
            ci <- 1 + (i - 1)%%ncol
            ri <- 1 + (i - 1)%/%ncol
        }
        else {
            ci <- 1 + (i - 1)%/%nrow
            ri <- 1 + (i - 1)%%nrow
        }
        vg <- if (ri != nrow) 
            vgap
        else u0
        symbol.border <- unit.c(vg, u0, u0, 0.5 * hgap)
        text.border <- unit.c(vg, u0, u0, if (ci != ncol) 
            hgap
        else u0)
        plGrob <- if (has.pch && do.lines) 
            gTree(children = gList(linesGrob(0:1, 0.5, gp = gpi), 
                pointsGrob(0.5, 0.5, default.units = "npc", pch = pch[i], 
                  gp = gpi))[ord])
        else if (has.pch) 
            pointsGrob(0.5, 0.5, default.units = "npc", pch = pch[i], 
                gp = gpi)
        else if (do.lines) 
            linesGrob(0:1, 0.5, gp = gpi)
        else nullGrob()
        fg <- packGrob(fg, plGrob, col = 2 * ci - 1, row = ri, 
            border = symbol.border, width = u1, height = u1, 
            force.width = TRUE)
        gpi. <- gpi
        gpi.$col <- "black"
        fg <- packGrob(fg, textGrob(labels[[i]], x = 0, y = 0.5, 
            just = c("left", "centre"), gp = gpi.), col = 2 * 
            ci, row = ri, border = text.border)
    }
    fg
}


grid.set <- function (gPath, newGrob, strict = FALSE, grep = FALSE, redraw = TRUE) 
{
    if (is.character(gPath)) 
        gPath <- gPath(gPath)
    if (!inherits(gPath, "gPath")) 
        stop("invalid 'gPath'")
    if (!is.logical(grep)) 
        stop("invalid 'grep' value")
    grep <- rep(grep, length.out = depth(gPath))
    result <- setDLfromGPath(gPath, newGrob, strict, grep)
    if (result$index) {
        dl.index <- grid.Call(L_getDLindex)
        grid.Call(L_setDLindex, as.integer(result$index))
        grid.Call(L_setDLelt, result$grob)
        grid.Call(L_setDLindex, as.integer(dl.index))
        if (redraw) 
            draw.all()
    }
    else {
        stop("'gPath' does not specify a valid child")
    }
}


setGrob <- function (gTree, gPath, newGrob, strict = FALSE, grep = FALSE) 
{
    if (!inherits(gTree, "gTree")) 
        stop("it is only valid to set a child of a \"gTree\"")
    if (!inherits(newGrob, "grob")) 
        stop("it is only valid to set a 'grob' as child of a \"gTree\"")
    if (is.character(gPath)) 
        gPath <- gPath(gPath)
    if (!inherits(gPath, "gPath")) 
        stop("invalid 'gPath'")
    if (!is.logical(grep)) 
        stop("invalid 'grep' value")
    grep <- rep(grep, length.out = depth(gPath))
    if (depth(gPath) == 1 && strict) {
        if (old.pos <- nameMatch(gPath$name, gTree$childrenOrder, 
            grep)) {
            if (match(gTree$childrenOrder[old.pos], newGrob$name, 
                nomatch = 0L)) {
                gTree$children[[newGrob$name]] <- newGrob
            }
            else {
                stop(gettextf("New 'grob' name (%s) does not match 'gPath' (%s)", 
                  newGrob$name, gPath), domain = NA)
            }
        }
        else {
            stop("'gPath' does not specify a valid child")
        }
    }
    else {
        gTree <- setGTree(gTree, NULL, gPath, newGrob, strict, 
            grep)
        if (is.null(gTree)) 
            stop("'gPath' does not specify a valid child")
    }
    gTree
}


preDrawDetails <- function (x) 
{
    UseMethod("preDrawDetails")
}


bezierPoints <- function (x) 
{
    sg <- splinegrob(x)
    sg$vp <- x$vp
    xsplinePoints(sg)
}


grobHeight <- function (x) 
{
    UseMethod("grobHeight")
}


grid.xspline <- function (...) 
{
    grid.draw(xsplineGrob(...))
}


stringDescent <- function (string) 
{
    n <- length(string)
    if (is.language(string)) {
        data <- vector("list", n)
        for (i in 1L:n) data[[i]] <- string[i]
    }
    else {
        data <- as.list(as.character(string))
    }
    unit(rep_len(1, n), "strdescent", data = data)
}


getNames <- function () 
{
    dl <- grid.Call(L_getDisplayList)[1L:grid.Call(L_getDLindex)]
    names <- sapply(dl, getName)
    names[nzchar(names)]
}


current.viewport <- function () 
{
    vpFromPushedvp(grid.Call(L_currentViewport))
}


pathGrob <- function (x, y, id = NULL, id.lengths = NULL, rule = "winding", 
    default.units = "npc", name = NULL, gp = gpar(), vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, id = id, id.lengths = id.lengths, rule = rule, 
        name = name, gp = gp, vp = vp, cl = "pathgrob")
}


gTree <- function (..., name = NULL, gp = NULL, vp = NULL, children = NULL, 
    childrenvp = NULL, cl = NULL) 
{
    gt <- list(..., name = name, gp = gp, vp = vp)
    if (!is.null(cl) && !is.character(cl)) 
        stop("invalid \"gTree\" class")
    class(gt) <- c(cl, "gTree", "grob", "gDesc")
    gt <- validGrob(gt, childrenvp)
    gt <- setChildren(gt, children)
    return(gt)
}


yaxisGrob <- function (at = NULL, label = TRUE, main = TRUE, edits = NULL, 
    name = NULL, gp = gpar(), vp = NULL) 
{
    grid.yaxis(at = at, label = label, main = main, edits = edits, 
        name = name, gp = gp, draw = FALSE, vp = vp)
}


convertUnit <- function (x, unitTo, axisFrom = "x", typeFrom = "location", axisTo = axisFrom, 
    typeTo = typeFrom, valueOnly = FALSE) 
{
    whatfrom <- match(axisFrom, c("x", "y")) - 1L + 2L * (match(typeFrom, 
        c("location", "dimension")) - 1L)
    whatto <- match(axisTo, c("x", "y")) - 1L + 2L * (match(typeTo, 
        c("location", "dimension")) - 1L)
    if (!is.unit(x)) 
        stop("'x' argument must be a unit object")
    if (is.na(whatfrom) || is.na(whatto)) 
        stop("invalid 'axis' or 'type'")
    value <- grid.Call(L_convert, x, as.integer(whatfrom), as.integer(whatto), 
        valid.units(unitTo))
    if (!valueOnly) 
        unit(value, unitTo)
    else value
}


vpPath <- function (...) 
{
    names <- c(...)
    vpPathFromVector(names)
}


grid.plot.and.legend <- function () 
{
    grid.newpage()
    top.vp <- viewport(width = 0.8, height = 0.8)
    pushViewport(top.vp)
    x <- stats::runif(10)
    y1 <- stats::runif(10)
    y2 <- stats::runif(10)
    pch <- 1L:3
    labels <- c("Girls", "Boys", "Other")
    lf <- frameGrob()
    plot <- gTree(children = gList(rectGrob(), pointsGrob(x, 
        y1, pch = 1), pointsGrob(x, y2, pch = 2), xaxisGrob(), 
        yaxisGrob()))
    lf <- packGrob(lf, plot)
    lf <- packGrob(lf, grid.legend(labels, pch = pch, draw = FALSE), 
        height = unit(1, "null"), side = "right")
    grid.draw(lf)
}


current.vpPath <- function () 
{
    names <- NULL
    pvp <- grid.Call(L_currentViewport)
    while (!rootVP(pvp)) {
        names <- c(names, pvp$name)
        pvp <- pvp$parent
    }
    if (!is.null(names)) 
        vpPathFromVector(rev(names))
    else names
}


grid.function <- function (...) 
{
    grid.draw(functionGrob(...))
}


frameGrob <- function (layout = NULL, name = NULL, gp = gpar(), vp = NULL) 
{
    framevp <- if (!is.null(layout)) 
        viewport(layout = layout)
    gTree(framevp = framevp, name = name, gp = gp, vp = vp, cl = "frame")
}


grid.grep <- function (path, x = NULL, grobs = TRUE, viewports = FALSE, strict = FALSE, 
    grep = FALSE, global = FALSE, no.match = character()) 
{
    if (!inherits(path, "gPath")) 
        path <- gPath(path)
    depth <- depth(path)
    grep <- rep(grep, length.out = depth)
    pathPieces <- explode(path)
    if (is.null(x)) {
        dl <- grid.ls(grobs = grobs, viewports = viewports, print = FALSE)
    }
    else {
        dl <- grid.ls(x, grobs = grobs, viewports = viewports, 
            print = FALSE)
    }
    if (!length(dl$name)) 
        return(no.match)
    names <- names(dl)
    dl <- lapply(dl, function(x) {
        x[dl$type == "vpListing" | dl$type == "grobListing" | 
            dl$type == "gTreeListing"]
    })
    names(dl) <- names
    if (is.null(x)) {
        dl$depth <- ifelse(dl$type == "vpListing", dl$vpDepth - 
            1, dl$gDepth)
        dl$path <- ifelse(dl$type == "vpListing", clean(dl$vpPath), 
            dl$gPath)
    }
    else {
        dl$depth <- ifelse(dl$type == "vpListing", dl$vpDepth, 
            dl$gDepth)
        dl$path <- ifelse(dl$type == "vpListing", dl$vpPath, 
            dl$gPath)
    }
    matchingDepths <- if (!strict) 
        which((dl$depth + 1) >= depth)
    else which((dl$depth + 1) == depth)
    if (!length(matchingDepths)) 
        return(no.match)
    nMatches <- 0
    searchMatches <- vector("list", length(matchingDepths))
    for (i in matchingDepths) {
        dlPathPieces <- if (dl$depth[i] > 0) 
            c(explode(dl$path[i]), dl$name[i])
        else dl$name[i]
        matches <- logical(depth)
        if (!strict) {
            depthOffset <- 0
            while (depthOffset + depth <= dl$depth[i] + 1 && 
                !all(matches)) {
                for (j in 1:depth) {
                  matches[j] <- if (grep[j]) 
                    grepl(pathPieces[j], dlPathPieces[depthOffset + 
                      j])
                  else pathPieces[j] == dlPathPieces[depthOffset + 
                    j]
                }
                depthOffset <- depthOffset + 1
            }
        }
        else {
            for (j in 1:depth) {
                matches[j] <- if (grep[j]) 
                  grepl(pathPieces[j], dlPathPieces[j])
                else pathPieces[j] == dlPathPieces[j]
            }
        }
        if (all(matches)) {
            if (!global) {
                if (dl$type[i] == "vpListing") {
                  result <- do.call("vpPath", list(dlPathPieces))
                }
                else {
                  result <- do.call("gPath", list(dlPathPieces))
                  attr(result, "vpPath") <- clean(dl$vpPath[i])
                }
                return(result)
            }
            else {
                nMatches <- nMatches + 1
                if (dl$type[i] == "vpListing") {
                  result <- do.call("vpPath", list(dlPathPieces))
                }
                else {
                  result <- do.call("gPath", list(dlPathPieces))
                  attr(result, "vpPath") <- clean(dl$vpPath[i])
                }
                searchMatches[[nMatches]] <- result
            }
        }
    }
    if (!nMatches) 
        return(no.match)
    searchMatches <- searchMatches[1:nMatches]
    return(searchMatches)
}


viewport.transform <- function (vp) 
{
    .Defunct("current.transform")
}


ascentDetails <- function (x) 
{
    UseMethod("ascentDetails", x)
}


grid.remove <- function (gPath, warn = TRUE, strict = FALSE, grep = FALSE, global = FALSE, 
    allDevices = FALSE, redraw = TRUE) 
{
    if (allDevices) 
        stop("'allDevices' not yet implemented")
    if (is.character(gPath)) 
        gPath <- gPath(gPath)
    if (!inherits(gPath, "gPath")) 
        stop("invalid 'gPath'")
    if (!is.logical(grep)) 
        stop("invalid 'grep' value")
    grep <- rep(grep, length.out = depth(gPath))
    if (depth(gPath) == 1) {
        removeNameFromDL(gPath$name, strict, grep, global, warn, 
            redraw)
    }
    else {
        name <- gPath$name
        gPath <- gPath(gPath$path)
        greppath <- grep[-length(grep)]
        grepname <- grep[length(grep)]
        removeDLFromGPath(gPath, name, strict, greppath, grepname, 
            global, warn, redraw)
    }
}


grid.DLapply <- function (FUN, ...) 
{
    FUN <- match.fun(FUN)
    gridDLindex <- grid.Call(L_getDLindex)
    newDL <- vector("list", gridDLindex)
    for (i in 1:(gridDLindex - 1)) {
        elt <- grid.Call(L_getDLelt, i)
        newElt <- FUN(elt, ...)
        if (!(is.null(newElt) || inherits(newElt, class(elt)))) 
            stop("invalid modification of the display list")
        newDL[[i]] <- newElt
    }
    for (i in 1:(gridDLindex - 1)) {
        grid.Call(L_setDLindex, i)
        grid.Call(L_setDLelt, newDL[[i]])
    }
    grid.Call(L_setDLindex, gridDLindex)
}


grobTree <- function (..., name = NULL, gp = NULL, vp = NULL, childrenvp = NULL, 
    cl = NULL) 
{
    gTree(children = gList(...), name = name, gp = gp, vp = vp, 
        childrenvp = childrenvp, cl = cl)
}


grid.draw <- function (x, recording = TRUE) 
{
    if (!is.null(x)) 
        UseMethod("grid.draw")
}


downViewport <- function (name, strict = FALSE, recording = TRUE) 
{
    UseMethod("downViewport")
}


unit.c <- function (...) 
{
    x <- list(...)
    if (!all(sapply(x, is.unit))) 
        stop("it is invalid to combine 'unit' objects with other types")
    listUnit <- function(x) {
        inherits(x, "unit.list") || inherits(x, "unit.arithmetic")
    }
    ual <- any(sapply(x, listUnit))
    if (ual) 
        unit.list.from.list(x)
    else {
        values <- unlist(x)
        unitUnits <- function(x) {
            rep(attr(x, "unit"), length.out = length(x))
        }
        units <- unlist(lapply(x, unitUnits))
        unitData <- function(x) {
            data <- attr(x, "data")
            if (is.null(data)) 
                vector("list", length(x))
            else recycle.data(data, TRUE, length(x), unitUnits(x))
        }
        data <- do.call("c", lapply(x, unitData))
        unit(values, units, data = data)
    }
}


pathListing <- function (x, gvpSep = " | ", gAlign = TRUE) 
{
    appendToPrefix <- function(path, name) {
        emptyPath <- nchar(path) == 0
        ifelse(emptyPath, name, paste(path, name, sep = .grid.pathSep))
    }
    padPrefix <- function(path, maxLen) {
        paste0(path, strrep(" ", maxLen - nchar(path)))
    }
    if (!inherits(x, "flatGridListing")) 
        stop("invalid 'listing'")
    vpListings <- seq_along(x$name) %in% grep("^vp", x$type)
    paths <- x$vpPath
    if (sum(vpListings) > 0) {
        paths[vpListings] <- appendToPrefix(paths[vpListings], 
            x$name[vpListings])
        maxLen <- max(nchar(paths[vpListings]))
    }
    else maxLen <- max(nchar(paths))
    if (sum(!vpListings) > 0) {
        if (gAlign) {
            paths[!vpListings] <- padPrefix(paths[!vpListings], 
                maxLen)
        }
        paths[!vpListings] <- paste0(paths[!vpListings], gvpSep, 
            appendToPrefix(x$gPath[!vpListings], x$name[!vpListings]))
    }
    cat(paths, sep = "\n")
}


packGrob <- function (frame, grob, side = NULL, row = NULL, row.before = NULL, 
    row.after = NULL, col = NULL, col.before = NULL, col.after = NULL, 
    width = NULL, height = NULL, force.width = FALSE, force.height = FALSE, 
    border = NULL, dynamic = FALSE) 
{
    if (!inherits(frame, "frame")) 
        stop("invalid 'frame'")
    if (!is.grob(grob)) 
        stop("invalid 'grob'")
    if (!is.null(col) & length(col) > 1) {
        col <- range(col)
        col.range <- TRUE
    }
    else col.range <- FALSE
    if (!is.null(row) & length(row) > 1) {
        row <- range(row)
        row.range <- TRUE
    }
    else row.range <- FALSE
    frame.vp <- frame$framevp
    if (is.null(frame.vp)) 
        frame.vp <- viewport()
    lay <- viewport.layout(frame.vp)
    if (is.null(lay)) {
        ncol <- 0
        nrow <- 0
    }
    else {
        ncol <- layout.ncol(lay)
        nrow <- layout.nrow(lay)
    }
    ncs <- num.col.specs(side, col, col.before, col.after)
    if (ncs == 0) {
        if (ncol > 0) {
            col <- c(1, ncol)
            col.range <- TRUE
        }
        else col <- 1
        ncs <- 1
    }
    if (ncs != 1) 
        stop("cannot specify more than one of 'side=[\"left\", \"right\"]', 'col', 'col.before', or 'col.after'")
    nrs <- num.row.specs(side, row, row.before, row.after)
    if (nrs == 0) {
        if (nrow > 0) {
            row <- c(1, nrow)
            row.range <- TRUE
        }
        else row <- 1
        nrs <- 1
    }
    if (nrs != 1) 
        stop("must specify exactly one of 'side=[\"top\", \"bottom\"]', 'row', 'row.before', or 'row.after'")
    new.col <- new.col(side, col, col.before, col.after, ncol)
    col <- col.spec(side, col, col.before, col.after, ncol)
    new.row <- new.row(side, row, row.before, row.after, nrow)
    row <- row.spec(side, row, row.before, row.after, nrow)
    if (!is.null(grob)) 
        cgrob <- cellGrob(col, row, border, grob, dynamic, cellViewport(col, 
            row, border))
    if (is.null(width)) 
        if (is.null(grob)) 
            width <- unit(1, "null")
        else if (dynamic) 
            width <- unit(1, "grobwidth", gPath(cgrob$name))
        else width <- unit(1, "grobwidth", cgrob)
    if (is.null(height)) 
        if (is.null(grob)) 
            height <- unit(1, "null")
        else if (dynamic) 
            height <- unit(1, "grobheight", gPath(cgrob$name))
        else height <- unit(1, "grobheight", cgrob)
    if (!is.null(border)) {
        width <- sum(border[2L], width, border[4L])
        height <- sum(border[1L], height, border[3L])
    }
    if (new.col) 
        ncol <- ncol + 1
    if (new.row) 
        nrow <- nrow + 1
    if (is.null(lay)) {
        widths <- width
        heights <- height
    }
    else {
        if (col.range) 
            widths <- layout.widths(lay)
        else widths <- mod.dims(width, layout.widths(lay), col, 
            new.col, ncol, force.width)
        if (row.range) 
            heights <- layout.heights(lay)
        else heights <- mod.dims(height, layout.heights(lay), 
            row, new.row, nrow, force.height)
    }
    frame.vp$layout <- grid.layout(ncol = ncol, nrow = nrow, 
        widths = widths, heights = heights)
    if (new.col || new.row) {
        for (i in childNames(frame)) {
            child <- getGrob(frame, i)
            if (new.col) {
                newcol <- updateCol(child$col, col)
                child <- editGrob(child, col = newcol, cellvp = cellViewport(newcol, 
                  child$row, child$border))
            }
            if (new.row) {
                newrow <- updateRow(child$row, row)
                child <- editGrob(child, row = newrow, cellvp = cellViewport(child$col, 
                  newrow, child$border))
            }
            frame <- addGrob(frame, child)
        }
    }
    if (!is.null(grob)) {
        frame <- addGrob(frame, cgrob)
    }
    editGrob(frame, framevp = frame.vp)
}


grid.get <- function (gPath, strict = FALSE, grep = FALSE, global = FALSE, 
    allDevices = FALSE) 
{
    if (allDevices) 
        stop("'allDevices' not yet implemented")
    if (is.character(gPath)) 
        gPath <- gPath(gPath)
    if (!inherits(gPath, "gPath")) 
        stop("invalid 'gPath'")
    if (!is.logical(grep)) 
        stop("invalid 'grep' value")
    grep <- rep(grep, length.out = depth(gPath))
    getDLfromGPath(gPath, strict, grep, global)
}


unit.pmax <- function (...) 
{
    select.i <- function(unit, i) {
        unit[i, top = FALSE]
    }
    x <- list(...)
    numargs <- length(x)
    if (numargs == 0L) 
        stop("no arguments where at least one expected")
    maxlength <- 0L
    for (i in seq_len(numargs)) if (length(x[[i]]) > maxlength) 
        maxlength <- length(x[[i]])
    result <- max(unit.list.from.list(lapply(x, select.i, 1L)))
    if (maxlength > 1L) 
        for (i in 2L:maxlength) result <- unit.c(result, max(unit.list.from.list(lapply(x, 
            select.i, i))))
    result
}


is.unit <- function (unit) 
{
    inherits(unit, "unit")
}


curveGrob <- function (x1, y1, x2, y2, default.units = "npc", curvature = 1, 
    angle = 90, ncp = 1, shape = 0.5, square = TRUE, squareShape = 1, 
    inflect = FALSE, arrow = NULL, open = TRUE, debug = FALSE, 
    name = NULL, gp = gpar(), vp = NULL) 
{
    if (!is.unit(x1)) 
        x1 <- unit(x1, default.units)
    if (!is.unit(y1)) 
        y1 <- unit(y1, default.units)
    if (!is.unit(x2)) 
        x2 <- unit(x2, default.units)
    if (!is.unit(y2)) 
        y2 <- unit(y2, default.units)
    gTree(x1 = x1, y1 = y1, x2 = x2, y2 = y2, curvature = curvature, 
        angle = angle, ncp = ncp, shape = shape, square = square, 
        squareShape = squareShape, inflect = inflect, arrow = arrow, 
        open = open, debug = debug, name = name, gp = gp, vp = vp, 
        cl = "curve")
}


gEditList <- function (...) 
{
    edits <- list(...)
    if (!all(sapply(edits, is.gEdit))) 
        stop("'gEditList' can only contain 'gEdit' objects")
    class(edits) <- "gEditList"
    edits
}


grid.grab <- function (warn = 2, wrap = FALSE, ...) 
{
    grabDL(warn, wrap, ...)
}


grid.null <- function (...) 
{
    grid.draw(nullGrob(...))
}


grid.layout <- function (nrow = 1, ncol = 1, widths = unit(rep_len(1, ncol), 
    "null"), heights = unit(rep_len(1, nrow), "null"), default.units = "null", 
    respect = FALSE, just = "centre") 
{
    if (!is.unit(widths)) 
        widths <- unit(widths, default.units)
    if (!is.unit(heights)) 
        heights <- unit(heights, default.units)
    valid.layout(nrow, ncol, widths, heights, respect, just)
}


grid.raster <- function (image, x = unit(0.5, "npc"), y = unit(0.5, "npc"), 
    width = NULL, height = NULL, just = "centre", hjust = NULL, 
    vjust = NULL, interpolate = TRUE, default.units = "npc", 
    name = NULL, gp = gpar(), vp = NULL) 
{
    rg <- rasterGrob(image, x = x, y = y, width = width, height = height, 
        just = just, hjust = hjust, vjust = vjust, interpolate = interpolate, 
        default.units = default.units, name = name, gp = gp, 
        vp = vp)
    grid.draw(rg)
}


heightDetails <- function (x) 
{
    UseMethod("heightDetails", x)
}


showGrob <- function (x = NULL, gPath = NULL, strict = FALSE, grep = FALSE, 
    recurse = TRUE, depth = NULL, labelfun = grobLabel, ...) 
{
    if (is.null(x)) {
        if (is.null(gPath)) {
            dl <- grid.Call(L_getDisplayList)[1L:grid.Call(L_getDLindex)]
            grid.newpage(recording = FALSE)
            lapply(dl[-1], function(y) {
                if (is.grob(y)) 
                  y <- labelGrob(y, recurse, 1, depth, labelfun, 
                    ...)
                grid.draw(y, recording = FALSE)
            })
        }
        else {
            grobToLabel <- grid.get(gPath, strict = strict, grep = grep)
            scene <- grid.grab(wrap = TRUE)
            modScene <- setGrob(scene, gPath, labelGrob(grobToLabel, 
                recurse, 1, depth, labelfun, ...), strict = strict, 
                grep = grep)
            grid.newpage(recording = FALSE)
            grid.draw(modScene, recording = FALSE)
        }
    }
    else {
        grid.newpage()
        grid.draw(x)
        showGrob(NULL, gPath, strict, grep, recurse, depth, labelfun, 
            ...)
    }
    invisible()
}


stringHeight <- function (string) 
{
    n <- length(string)
    if (is.language(string)) {
        data <- vector("list", n)
        for (i in 1L:n) data[[i]] <- string[i]
    }
    else {
        data <- as.list(as.character(string))
    }
    unit(rep_len(1, n), "strheight", data = data)
}


widthDetails <- function (x) 
{
    UseMethod("widthDetails", x)
}


circleGrob <- function (x = 0.5, y = 0.5, r = 0.5, default.units = "npc", name = NULL, 
    gp = gpar(), vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    if (!is.unit(r)) 
        r <- unit(r, default.units)
    grob(x = x, y = y, r = r, name = name, gp = gp, vp = vp, 
        cl = "circle")
}


roundrectGrob <- function (x = 0.5, y = 0.5, width = 1, height = 1, default.units = "npc", 
    r = unit(0.1, "snpc"), just = "centre", name = NULL, gp = NULL, 
    vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    if (!is.unit(width)) 
        width <- unit(width, default.units)
    if (!is.unit(height)) 
        height <- unit(height, default.units)
    grob(x = x, y = y, width = width, height = height, r = r, 
        just = just, name = name, gp = gp, vp = vp, cl = "roundrect")
}


upViewport <- function (n = 1, recording = TRUE) 
{
    if (n < 0) 
        stop("must navigate up at least one viewport")
    if (n == 0) {
        n <- vpDepth()
        upPath <- current.vpPath()
    }
    if (n > 0) {
        path <- current.vpPath()
        upPath <- path[(depth(path) - n + 1):depth(path)]
        grid.Call.graphics(L_upviewport, as.integer(n))
        if (recording) {
            class(n) <- "up"
            record(n)
        }
    }
    invisible(upPath)
}


vpStack <- function (...) 
{
    vps <- list(...)
    if (all(sapply(vps, viewportorpath, simplify = TRUE))) {
        class(vps) <- c("vpStack", "viewport")
        vps
    }
    else {
        stop("only viewports allowed in 'vpStack'")
    }
}


grid.segments <- function (x0 = unit(0, "npc"), y0 = unit(0, "npc"), x1 = unit(1, 
    "npc"), y1 = unit(1, "npc"), default.units = "npc", arrow = NULL, 
    name = NULL, gp = gpar(), draw = TRUE, vp = NULL) 
{
    sg <- segmentsGrob(x0 = x0, y0 = y0, x1 = x1, y1 = y1, default.units = default.units, 
        arrow = arrow, name = name, gp = gp, vp = vp)
    if (draw) 
        grid.draw(sg)
    invisible(sg)
}


layoutRegion <- function (layout.pos.row = 1, layout.pos.col = 1) 
{
    region <- grid.Call(L_layoutRegion, if (is.null(layout.pos.row)) 
        layout.pos.row
    else as.integer(rep(layout.pos.row, length.out = 2)), if (is.null(layout.pos.col)) 
        layout.pos.col
    else as.integer(rep(layout.pos.col, length.out = 2)))
    list(left = unit(region[1L], "npc"), bottom = unit(region[2L], 
        "npc"), width = unit(region[3L], "npc"), height = unit(region[4L], 
        "npc"))
}


gList <- function (...) 
{
    gl <- list(...)
    if (length(gl) == 0L || all(sapply(gl, okGListelt, simplify = TRUE))) {
        if (!all(sapply(gl, is.grob))) 
            gl <- do.call("c", lapply(gl, as.gList))
        class(gl) <- c("gList")
        return(gl)
    }
    else {
        stop("only 'grobs' allowed in \"gList\"")
    }
}


grobDescent <- function (x) 
{
    UseMethod("grobDescent")
}


grid.rect <- function (x = unit(0.5, "npc"), y = unit(0.5, "npc"), width = unit(1, 
    "npc"), height = unit(1, "npc"), just = "centre", hjust = NULL, 
    vjust = NULL, default.units = "npc", name = NULL, gp = gpar(), 
    draw = TRUE, vp = NULL) 
{
    rg <- rectGrob(x = x, y = y, width = width, height = height, 
        just = just, hjust = hjust, vjust = vjust, default.units = default.units, 
        name = name, gp = gp, vp = vp)
    if (draw) 
        grid.draw(rg)
    invisible(rg)
}


unit.rep <- function (x, ...) 
{
    warning("'unit.rep' has been deprecated in favour of a unit method for the generic rep function", 
        domain = NA)
    rep(x, ...)
}


xsplineGrob <- function (x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL, 
    id.lengths = NULL, default.units = "npc", shape = 0, open = TRUE, 
    arrow = NULL, repEnds = TRUE, name = NULL, gp = gpar(), vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, shape = shape, open = open, id = id, id.lengths = id.lengths, 
        arrow = arrow, repEnds = repEnds, name = name, gp = gp, 
        vp = vp, cl = "xspline")
}


grid.roundrect <- function (...) 
{
    grid.draw(roundrectGrob(...))
}


clipGrob <- function (x = unit(0.5, "npc"), y = unit(0.5, "npc"), width = unit(1, 
    "npc"), height = unit(1, "npc"), just = "centre", hjust = NULL, 
    vjust = NULL, default.units = "npc", name = NULL, vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    if (!is.unit(width)) 
        width <- unit(width, default.units)
    if (!is.unit(height)) 
        height <- unit(height, default.units)
    grob(x = x, y = y, width = width, height = height, just = just, 
        hjust = hjust, vjust = vjust, name = name, vp = vp, cl = "clip")
}


grid.lines <- function (x = unit(c(0, 1), "npc"), y = unit(c(0, 1), "npc"), 
    default.units = "npc", arrow = NULL, name = NULL, gp = gpar(), 
    draw = TRUE, vp = NULL) 
{
    lg <- linesGrob(x = x, y = y, default.units = default.units, 
        arrow = arrow, name = name, gp = gp, vp = vp)
    if (draw) 
        grid.draw(lg)
    invisible(lg)
}


grid.grill <- function (h = unit(seq(0.25, 0.75, 0.25), "npc"), v = unit(seq(0.25, 
    0.75, 0.25), "npc"), default.units = "npc", gp = gpar(col = "grey"), 
    vp = NULL) 
{
    if (!is.unit(h)) 
        h <- unit(h, default.units)
    if (!is.unit(v)) 
        v <- unit(v, default.units)
    if (!is.null(vp)) 
        pushViewport(vp)
    grid.segments(v, unit(0, "npc"), v, unit(1, "npc"), gp = gp)
    grid.segments(unit(0, "npc"), h, unit(1, "npc"), h, gp = gp)
    if (!is.null(vp)) 
        popViewport()
}


grid.convertWidth <- function (x, unitTo, valueOnly = FALSE) 
{
    .Defunct("convertWidth")
}


drawDetails <- function (x, recording) 
{
    UseMethod("drawDetails")
}


grid.show.viewport <- function (v, parent.layout = NULL, newpage = TRUE, vp.ex = 0.8, 
    border.fill = "light grey", vp.col = "blue", vp.fill = "light blue", 
    scale.col = "red", vp = NULL) 
{
    if ((!is.null(v$layout.pos.row) || !is.null(v$layout.pos.col)) && 
        !is.null(parent.layout)) {
        if (!is.null(vp)) 
            pushViewport(vp)
        vp.mid <- grid.show.layout(parent.layout, vp.ex = vp.ex, 
            cell.border = "grey", cell.fill = "white", cell.label = FALSE, 
            newpage = newpage)
        pushViewport(vp.mid)
        pushViewport(v)
        gp.red <- gpar(col = scale.col)
        grid.rect(gp = gpar(col = "blue", fill = "light blue"))
        at <- grid.pretty(v$xscale)
        grid.xaxis(at = c(min(at), max(at)), gp = gp.red)
        at <- grid.pretty(v$yscale)
        grid.yaxis(at = c(min(at), max(at)), gp = gp.red)
        popViewport(2)
        if (!is.null(vp)) 
            popViewport()
    }
    else {
        if (newpage) 
            grid.newpage()
        if (!is.null(vp)) 
            pushViewport(vp)
        grid.rect(gp = gpar(col = NULL, fill = border.fill))
        vp.mid <- viewport(0.5, 0.5, vp.ex, vp.ex)
        pushViewport(vp.mid)
        grid.rect(gp = gpar(fill = "white"))
        x <- v$x
        y <- v$y
        w <- v$width
        h <- v$height
        pushViewport(v)
        grid.rect(gp = gpar(col = vp.col, fill = vp.fill))
        gp.red <- gpar(col = scale.col)
        at <- grid.pretty(v$xscale)
        grid.xaxis(at = c(min(at), max(at)), gp = gp.red)
        at <- grid.pretty(v$yscale)
        grid.yaxis(at = c(min(at), max(at)), gp = gp.red)
        grid.text(as.character(w), gp = gp.red, just = c("centre", 
            "bottom"), x = unit(0.5, "npc"), y = unit(1, "npc") + 
            unit(0.05, "inches"))
        grid.text(as.character(h), gp = gp.red, just = c("left", 
            "centre"), x = unit(1, "npc") + unit(0.05, "inches"), 
            y = unit(0.5, "npc"))
        popViewport()
        grid.lines(unit.c(x, x), unit.c(unit(0, "npc"), y), gp = gpar(col = scale.col, 
            lty = "dashed"))
        grid.lines(unit.c(unit(0, "npc"), x), unit.c(y, y), gp = gpar(col = scale.col, 
            lty = "dashed"))
        grid.text(as.character(x), gp = gp.red, just = c("centre", 
            "top"), x = x, y = unit(-0.05, "inches"))
        grid.text(as.character(y), gp = gp.red, just = c("bottom"), 
            x = unit(-0.05, "inches"), y = y, rot = 90)
        popViewport()
        if (!is.null(vp)) 
            popViewport()
    }
}


validDetails <- function (x) 
{
    UseMethod("validDetails")
}


grid.line.to <- function (x = 1, y = 1, default.units = "npc", arrow = NULL, 
    name = NULL, gp = gpar(), draw = TRUE, vp = NULL) 
{
    ltg <- lineToGrob(x = x, y = y, default.units = default.units, 
        arrow = arrow, name = name, gp = gp, vp = vp)
    if (draw) 
        grid.draw(ltg)
    invisible(ltg)
}


polygonGrob <- function (x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL, 
    id.lengths = NULL, default.units = "npc", name = NULL, gp = gpar(), 
    vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, id = id, id.lengths = id.lengths, name = name, 
        gp = gp, vp = vp, cl = "polygon")
}


bezierGrob <- function (x = c(0, 0.5, 1, 0.5), y = c(0.5, 1, 0.5, 0), id = NULL, 
    id.lengths = NULL, default.units = "npc", arrow = NULL, name = NULL, 
    gp = gpar(), vp = NULL) 
{
    if (!is.unit(x)) 
        x <- unit(x, default.units)
    if (!is.unit(y)) 
        y <- unit(y, default.units)
    grob(x = x, y = y, id = id, id.lengths = id.lengths, arrow = arrow, 
        name = name, gp = gp, vp = vp, cl = "beziergrob")
}


get.gpar <- function (names = NULL) 
{
    if (is.null(names)) {
        result <- grid.Call(L_getGPar)
        result$gamma <- NULL
    }
    else {
        if (!is.character(names) || !all(names %in% .grid.gpar.names)) 
            stop("must specify only valid 'gpar' names")
        if ("gamma" %in% names) {
            warning("'gamma' 'gpar' element is defunct")
            names <- names[-match("gamma", names)]
        }
        result <- unclass(grid.Call(L_getGPar))[names]
    }
    class(result) <- "gpar"
    result
}


resolveVJust <- function (just, vjust) 
{
    if (is.null(vjust) || length(vjust) == 0) 
        valid.just(just)[2L]
    else vjust
}


grobX <- function (x, theta) 
{
    UseMethod("grobX", x)
}


descentDetails <- function (x) 
{
    UseMethod("descentDetails", x)
}


grobY <- function (x, theta) 
{
    UseMethod("grobY", x)
}


postDrawDetails <- function (x) 
{
    UseMethod("postDrawDetails")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "The Grid Graphics Package"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF