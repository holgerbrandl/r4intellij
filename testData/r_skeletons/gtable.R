##
## Exported symobls in package `gtable`
##

## Exported package methods

gtable_trim <- function (x) 
{
    stopifnot(is.gtable(x))
    w <- range(x$layout$l, x$layout$r)
    h <- range(x$layout$t, x$layout$b)
    x$widths <- x$widths[seq.int(w[1], w[2])]
    x$heights <- x$heights[seq.int(h[1], h[2])]
    x$layout$l <- x$layout$l - w[1] + 1
    x$layout$r <- x$layout$r - w[1] + 1
    x$layout$t <- x$layout$t - h[1] + 1
    x$layout$b <- x$layout$b - h[1] + 1
    x
}


gtable_add_rows <- function (x, heights, pos = -1) 
{
    stopifnot(is.gtable(x))
    stopifnot(length(pos) == 1)
    n <- length(heights)
    pos <- neg_to_pos(pos, nrow(x))
    x$heights <- insert.unit(x$heights, heights, pos)
    x$layout$t <- ifelse(x$layout$t > pos, x$layout$t + n, x$layout$t)
    x$layout$b <- ifelse(x$layout$b > pos, x$layout$b + n, x$layout$b)
    x
}


gtable_row <- function (name, grobs, height = NULL, widths = NULL, z = NULL, 
    vp = NULL) 
{
    height <- height %||% unit(max(unlist(lapply(grobs, height_cm))), 
        "cm")
    widths <- widths %||% rep(unit(1, "null"), length(grobs))
    stopifnot(is.null(z) || length(z) == length(grobs))
    if (is.null(z)) 
        z <- Inf
    table <- gtable(name = name, vp = vp)
    table <- gtable_add_cols(table, widths)
    table <- gtable_add_rows(table, height)
    table <- gtable_add_grob(table, grobs, l = seq_along(grobs), 
        t = 1, z = z, clip = "off")
    table
}


gtable_height <- function (x) 
sum(x$heights)


gtable_add_cols <- function (x, widths, pos = -1) 
{
    stopifnot(is.gtable(x))
    stopifnot(length(pos) == 1)
    n <- length(widths)
    pos <- neg_to_pos(pos, ncol(x))
    x$widths <- insert.unit(x$widths, widths, pos)
    x$layout$l <- ifelse(x$layout$l > pos, x$layout$l + n, x$layout$l)
    x$layout$r <- ifelse(x$layout$r > pos, x$layout$r + n, x$layout$r)
    x
}


gtable_add_row_space <- function (x, height) 
{
    stopifnot(is.gtable(x))
    n <- nrow(x) - 1
    if (n == 0) 
        return(x)
    stopifnot(length(height) == 1 || length(height) == n)
    height <- rep(height, length.out = n)
    for (i in rev(seq_len(n))) {
        x <- gtable_add_rows(x, height[i], pos = i)
    }
    x
}


gtable <- function (widths = list(), heights = list(), respect = FALSE, 
    name = "layout", rownames = NULL, colnames = NULL, vp = NULL) 
{
    if (length(widths) > 0) {
        stopifnot(is.unit(widths))
        stopifnot(is.null(colnames) || length(colnames == length(widths)))
    }
    if (length(heights) > 0) {
        stopifnot(is.unit(heights))
        stopifnot(is.null(rownames) || length(rownames == length(heights)))
    }
    layout <- data.frame(t = numeric(), l = numeric(), b = numeric(), 
        r = numeric(), z = numeric(), clip = character(), name = character(), 
        stringsAsFactors = FALSE)
    if (!is.null(vp)) {
        vp <- viewport(name = name, x = vp$x, y = vp$y, width = vp$width, 
            height = vp$height, just = vp$just, gp = vp$gp, xscale = vp$xscale, 
            yscale = vp$yscale, angle = vp$angle, clip = vp$clip)
    }
    gTree(grobs = list(), layout = layout, widths = widths, heights = heights, 
        respect = respect, name = name, rownames = rownames, 
        colnames = colnames, vp = vp, cl = "gtable")
}


is.gtable <- function (x) 
{
    inherits(x, "gtable")
}


gtable_show_layout <- function (x) 
{
    stopifnot(is.gtable(x))
    grid.show.layout(gtable_layout(x))
}


gtable_add_grob <- function (x, grobs, t, l, b = t, r = l, z = Inf, clip = "on", 
    name = x$name) 
{
    stopifnot(is.gtable(x))
    if (is.grob(grobs)) 
        grobs <- list(grobs)
    stopifnot(is.list(grobs))
    if (!all(vapply(list(t, r, b, l, z, clip, name), len_same_or_1, 
        logical(1), grobs))) {
        stop("Not all inputs have either length 1 or same length same as 'grobs'")
    }
    if (length(z) == 1) {
        z <- rep(z, length(grobs))
    }
    zval <- c(x$layout$z, z[!is.infinite(z)])
    if (length(zval) == 0) {
        zmin <- 1
        zmax <- 0
    }
    else {
        zmin <- min(zval)
        zmax <- max(zval)
    }
    z[z == -Inf] <- zmin - rev(seq_len(sum(z == -Inf)))
    z[z == Inf] <- zmax + seq_len(sum(z == Inf))
    t <- neg_to_pos(t, nrow(x))
    b <- neg_to_pos(b, nrow(x))
    l <- neg_to_pos(l, ncol(x))
    r <- neg_to_pos(r, ncol(x))
    layout <- data.frame(t = t, l = l, b = b, r = r, z = z, clip = clip, 
        name = name, stringsAsFactors = FALSE)
    stopifnot(length(grobs) == nrow(layout))
    x$grobs <- c(x$grobs, grobs)
    x$layout <- rbind(x$layout, layout)
    x
}


gtable_col_spacer <- function (heights) 
{
    gtable_add_rows(gtable(), heights)
}


gtable_matrix <- function (name, grobs, widths = NULL, heights = NULL, z = NULL, 
    respect = FALSE, clip = "on", vp = NULL) 
{
    table <- gtable(name = name, respect = respect, vp = vp)
    stopifnot(length(widths) == ncol(grobs))
    stopifnot(length(heights) == nrow(grobs))
    stopifnot(is.null(z) || identical(dim(grobs), dim(z)))
    if (is.null(z)) 
        z <- Inf
    table <- gtable_add_cols(table, widths)
    table <- gtable_add_rows(table, heights)
    table <- gtable_add_grob(table, grobs, t = c(row(grobs)), 
        l = c(col(grobs)), z = as.vector(z), clip = clip)
    table
}


gtable_col <- function (name, grobs, width = NULL, heights = NULL, z = NULL, 
    vp = NULL) 
{
    width <- width %||% unit(max(unlist(lapply(grobs, width_cm))), 
        "cm")
    heights <- heights %||% rep(unit(1, "null"), length(grobs))
    stopifnot(is.null(z) || length(z) == length(grobs))
    if (is.null(z)) 
        z <- Inf
    table <- gtable(name = name, vp = vp)
    table <- gtable_add_rows(table, heights)
    table <- gtable_add_cols(table, width)
    table <- gtable_add_grob(table, grobs, t = seq_along(grobs), 
        l = 1, z = z, clip = "off")
    table
}


gtable_add_padding <- function (x, padding) 
{
    padding <- rep(padding, length.out = 4)
    x <- gtable_add_rows(x, pos = 0, heights = padding[1])
    x <- gtable_add_cols(x, pos = -1, widths = padding[2])
    x <- gtable_add_rows(x, pos = -1, heights = padding[3])
    x <- gtable_add_cols(x, pos = 0, widths = padding[4])
    x
}


gtable_row_spacer <- function (widths) 
{
    gtable_add_cols(gtable(), widths)
}


gtable_add_col_space <- function (x, width) 
{
    stopifnot(is.gtable(x))
    n <- ncol(x) - 1
    if (n == 0) 
        return(x)
    stopifnot(length(width) == 1 || length(width) == n)
    width <- rep(width, length.out = n)
    for (i in rev(seq_len(n))) {
        x <- gtable_add_cols(x, width[i], pos = i)
    }
    x
}


gtable_width <- function (x) 
sum(x$widths)


gtable_filter <- function (x, pattern, fixed = FALSE, trim = TRUE) 
{
    matches <- grepl(pattern, x$layout$name, fixed = fixed)
    x$layout <- x$layout[matches, , drop = FALSE]
    x$grobs <- x$grobs[matches]
    if (trim) 
        x <- gtable_trim(x)
    x
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Arrange 'Grobs' in Tables"

.skeleton_package_version = "0.2.0"

.skeleton_package_depends = ""

.skeleton_package_imports = "grid"


## Internal

.skeleton_version = 5


## EOF