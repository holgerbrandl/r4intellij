##
## Exported symobls in package `ggplot2`
##

## Exported package methods

scale_color_gradient <- function (..., low = "#132B43", high = "#56B1F7", space = "Lab", 
    na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("colour", "gradient", seq_gradient_pal(low, 
        high, space), na.value = na.value, guide = guide, ...)
}


scale_linetype_discrete <- function (..., na.value = "blank") 
{
    discrete_scale("linetype", "linetype_d", linetype_pal(), 
        na.value = na.value, ...)
}


labs <- function (...) 
{
    args <- list(...)
    if (is.list(args[[1]])) 
        args <- args[[1]]
    args <- rename_aes(args)
    structure(args, class = "labels")
}


geom_hline <- function (mapping = NULL, data = NULL, ..., yintercept, na.rm = FALSE, 
    show.legend = NA) 
{
    if (!missing(yintercept)) {
        data <- data.frame(yintercept = yintercept)
        mapping <- aes(yintercept = yintercept)
        show.legend <- FALSE
    }
    layer(data = data, mapping = mapping, stat = StatIdentity, 
        geom = GeomHline, position = PositionIdentity, show.legend = show.legend, 
        inherit.aes = FALSE, params = list(na.rm = na.rm, ...))
}


scale_colour_hue <- function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, 
    direction = 1, na.value = "grey50") 
{
    discrete_scale("colour", "hue", hue_pal(h, c, l, h.start, 
        direction), na.value = na.value, ...)
}


scale_size_datetime <- function () 
{
    scale_size_continuous(trans = "time")
}


draw_key_path <- function (data, params, size) 
{
    segmentsGrob(0.1, 0.5, 0.9, 0.5, gp = gpar(col = alpha(data$colour, 
        data$alpha), lwd = data$size * .pt, lty = data$linetype, 
        lineend = "butt"), arrow = params$arrow)
}


cut_number <- function (x, n = NULL, ...) 
{
    brk <- breaks(x, "n", n)
    if (anyDuplicated(brk)) 
        stop("Insufficient data values to produce ", n, " bins.", 
            call. = FALSE)
    cut(x, brk, include.lowest = TRUE, ...)
}


.pt <- 2.84527559055118


ScaleContinuousIdentity <- "<environment>"

scale_fill_hue <- function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, 
    direction = 1, na.value = "grey50") 
{
    discrete_scale("fill", "hue", hue_pal(h, c, l, h.start, direction), 
        na.value = na.value, ...)
}


GeomHline <- "<environment>"

AxisSecondary <- "<environment>"

waiver <- function () 
structure(list(), class = "waiver")


should_stop <- function (expr) 
{
    res <- try(print(force(expr)), TRUE)
    if (!inherits(res, "try-error")) 
        stop("No error!", call. = FALSE)
    invisible()
}


sec_axis <- function (trans = NULL, name = waiver(), breaks = waiver(), labels = waiver()) 
{
    if (!is.formula(trans)) 
        stop("transformation for secondary axes must be a formula", 
            call. = FALSE)
    ggproto(NULL, AxisSecondary, trans = trans, name = name, 
        breaks = breaks, labels = labels)
}


GeomArea <- "<environment>"

geom_histogram <- function (mapping = NULL, data = NULL, stat = "bin", position = "stack", 
    ..., binwidth = NULL, bins = NULL, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomBar, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(binwidth = binwidth, bins = bins, na.rm = na.rm, 
            pad = FALSE, ...))
}


scale_color_grey <- function (..., start = 0.2, end = 0.8, na.value = "red") 
{
    discrete_scale("colour", "grey", grey_pal(start, end), na.value = na.value, 
        ...)
}


StatEllipse <- "<environment>"

scale_y_datetime <- function (name = waiver(), breaks = waiver(), date_breaks = waiver(), 
    labels = waiver(), date_labels = waiver(), minor_breaks = waiver(), 
    date_minor_breaks = waiver(), timezone = NULL, limits = NULL, 
    expand = waiver(), position = "left") 
{
    scale_datetime(c("y", "ymin", "ymax", "yend"), "time", name = name, 
        breaks = breaks, date_breaks = date_breaks, labels = labels, 
        date_labels = date_labels, minor_breaks = minor_breaks, 
        date_minor_breaks = date_minor_breaks, timezone = timezone, 
        limits = limits, expand = expand, position = position)
}


CoordFlip <- "<environment>"

geom_map <- function (mapping = NULL, data = NULL, stat = "identity", ..., 
    map, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    stopifnot(is.data.frame(map))
    if (!is.null(map$lat)) 
        map$y <- map$lat
    if (!is.null(map$long)) 
        map$x <- map$long
    if (!is.null(map$region)) 
        map$id <- map$region
    stopifnot(all(c("x", "y", "id") %in% names(map)))
    layer(data = data, mapping = mapping, stat = stat, geom = GeomMap, 
        position = PositionIdentity, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(map = map, na.rm = na.rm, 
            ...))
}


GeomTile <- "<environment>"

scale_fill_date <- function () 
{
    scale_fill_continuous(trans = "date")
}


geom_area <- function (mapping = NULL, data = NULL, stat = "identity", position = "stack", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomArea, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


StatSummaryBin <- "<environment>"

theme_bw <- function (base_size = 11, base_family = "") 
{
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
        theme(panel.background = element_rect(fill = "white", 
            colour = NA), panel.border = element_rect(fill = NA, 
            colour = "grey20"), panel.grid.major = element_line(colour = "grey92"), 
            panel.grid.minor = element_line(colour = "grey92", 
                size = 0.25), strip.background = element_rect(fill = "grey85", 
                colour = "grey20"), legend.key = element_rect(fill = "white", 
                colour = NA), complete = TRUE)
}


Coord <- "<environment>"

GeomErrorbarh <- "<environment>"

stat_identity <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", 
    ..., show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatIdentity, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = FALSE, 
            ...))
}


draw_key_blank <- function (data, params, size) 
{
    zeroGrob()
}


PositionNudge <- "<environment>"

guide_legend <- function (title = waiver(), title.position = NULL, title.theme = NULL, 
    title.hjust = NULL, title.vjust = NULL, label = TRUE, label.position = NULL, 
    label.theme = NULL, label.hjust = NULL, label.vjust = NULL, 
    keywidth = NULL, keyheight = NULL, direction = NULL, default.unit = "line", 
    override.aes = list(), nrow = NULL, ncol = NULL, byrow = FALSE, 
    reverse = FALSE, order = 0, ...) 
{
    if (!is.null(keywidth) && !is.unit(keywidth)) 
        keywidth <- unit(keywidth, default.unit)
    if (!is.null(keyheight) && !is.unit(keyheight)) 
        keyheight <- unit(keyheight, default.unit)
    structure(list(title = title, title.position = title.position, 
        title.theme = title.theme, title.hjust = title.hjust, 
        title.vjust = title.vjust, label = label, label.position = label.position, 
        label.theme = label.theme, label.hjust = label.hjust, 
        label.vjust = label.vjust, keywidth = keywidth, keyheight = keyheight, 
        direction = direction, override.aes = rename_aes(override.aes), 
        nrow = nrow, ncol = ncol, byrow = byrow, reverse = reverse, 
        order = order, available_aes = c("any"), ..., name = "legend"), 
        class = c("guide", "legend"))
}


scale_linetype <- function (..., na.value = "blank") 
{
    discrete_scale("linetype", "linetype_d", linetype_pal(), 
        na.value = na.value, ...)
}


render_axes <- function (x = NULL, y = NULL, coord, theme, transpose = FALSE) 
{
    axes <- list()
    if (!is.null(x)) {
        axes$x <- lapply(x, coord$render_axis_h, theme)
    }
    if (!is.null(y)) {
        axes$y <- lapply(y, coord$render_axis_v, theme)
    }
    if (transpose) {
        axes <- list(x = list(top = lapply(axes$x, `[[`, "top"), 
            bottom = lapply(axes$x, `[[`, "bottom")), y = list(left = lapply(axes$y, 
            `[[`, "left"), right = lapply(axes$y, `[[`, "right")))
    }
    axes
}


draw_key_crossbar <- function (data, params, size) 
{
    grobTree(rectGrob(height = 0.5, width = 0.75), linesGrob(c(0.125, 
        0.875), 0.5), gp = gpar(col = data$colour, fill = alpha(data$fill, 
        data$alpha), lwd = data$size * .pt, lty = data$linetype))
}


is.ggproto <- function (x) 
inherits(x, "ggproto")


GeomMap <- "<environment>"

mean_cl_normal <- function (x, ...) 
{
    if (!requireNamespace("Hmisc", quietly = TRUE)) 
        stop("Hmisc package required for this function", call. = FALSE)
    fun <- getExportedValue("Hmisc", fun)
    result <- do.call(fun, list(x = quote(x), ...))
    plyr::rename(data.frame(t(result)), c(Median = "y", Mean = "y", 
        Lower = "ymin", Upper = "ymax"), warn_missing = FALSE)
}


scale_fill_discrete <- function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, 
    direction = 1, na.value = "grey50") 
{
    discrete_scale("fill", "hue", hue_pal(h, c, l, h.start, direction), 
        na.value = na.value, ...)
}


PositionIdentity <- "<environment>"

draw_key_boxplot <- function (data, params, size) 
{
    grobTree(linesGrob(0.5, c(0.1, 0.25)), linesGrob(0.5, c(0.75, 
        0.9)), rectGrob(height = 0.5, width = 0.75), linesGrob(c(0.125, 
        0.875), 0.5), gp = gpar(col = data$colour, fill = alpha(data$fill, 
        data$alpha), lwd = data$size * .pt, lty = data$linetype))
}


annotate <- function (geom, x = NULL, y = NULL, xmin = NULL, xmax = NULL, 
    ymin = NULL, ymax = NULL, xend = NULL, yend = NULL, ..., 
    na.rm = FALSE) 
{
    position <- compact(list(x = x, xmin = xmin, xmax = xmax, 
        xend = xend, y = y, ymin = ymin, ymax = ymax, yend = yend))
    aesthetics <- c(position, list(...))
    lengths <- vapply(aesthetics, length, integer(1))
    unequal <- length(unique(setdiff(lengths, 1L))) > 1L
    if (unequal) {
        bad <- lengths != 1L
        details <- paste(names(aesthetics)[bad], " (", lengths[bad], 
            ")", sep = "", collapse = ", ")
        stop("Unequal parameter lengths: ", details, call. = FALSE)
    }
    data <- data.frame(position)
    layer(geom = geom, params = list(na.rm = na.rm, ...), stat = StatIdentity, 
        position = PositionIdentity, data = data, mapping = aes_all(names(data)), 
        inherit.aes = FALSE, show.legend = FALSE)
}


scale_shape <- function (..., solid = TRUE) 
{
    discrete_scale("shape", "shape_d", shape_pal(solid), ...)
}


last_plot <- function () 
.store$get()


scale_colour_discrete <- function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, 
    direction = 1, na.value = "grey50") 
{
    discrete_scale("colour", "hue", hue_pal(h, c, l, h.start, 
        direction), na.value = na.value, ...)
}


map_data <- function (map, region = ".", exact = FALSE, ...) 
{
    try_require("maps", "map_data")
    fortify(map(map, region, exact = exact, plot = FALSE, fill = TRUE, 
        ...))
}


derive <- function () 
{
    structure(list(), class = "derived")
}


geom_linerange <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomLinerange, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


StatSummary <- "<environment>"

StatYdensity <- "<environment>"

ScaleContinuous <- "<environment>"

max_height <- function (grobs) 
{
    unit(max(unlist(lapply(grobs, height_cm))), "cm")
}


label_both <- function (labels, multi_line = TRUE, sep = ": ") 
{
    value <- label_value(labels, multi_line = multi_line)
    variable <- label_variable(labels, multi_line = multi_line)
    if (multi_line) {
        out <- vector("list", length(value))
        for (i in seq_along(out)) {
            out[[i]] <- paste(variable[[i]], value[[i]], sep = sep)
        }
    }
    else {
        value <- do.call("paste", c(value, sep = ", "))
        variable <- do.call("paste", c(variable, sep = ", "))
        out <- Map(paste, variable, value, sep = sep)
        out <- list(unname(unlist(out)))
    }
    out
}


stat_summary2d <- function (...) 
{
    message("Please use stat_summary_2d() instead")
    stat_summary_2d(...)
}


scale_y_continuous <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), 
    labels = waiver(), limits = NULL, expand = waiver(), oob = censor, 
    na.value = NA_real_, trans = "identity", position = "left", 
    sec.axis = waiver()) 
{
    sc <- continuous_scale(c("y", "ymin", "ymax", "yend", "yintercept", 
        "ymin_final", "ymax_final", "lower", "middle", "upper"), 
        "position_c", identity, name = name, breaks = breaks, 
        minor_breaks = minor_breaks, labels = labels, limits = limits, 
        expand = expand, oob = oob, na.value = na.value, trans = trans, 
        guide = "none", position = position, super = ScaleContinuousPosition)
    if (!is.waive(sec.axis)) {
        if (is.formula(sec.axis)) 
            sec.axis <- sec_axis(sec.axis)
        if (!is.sec_axis(sec.axis)) 
            stop("Secondary axes must be specified using 'sec_axis()'")
        sc$secondary.axis <- sec.axis
    }
    sc
}


scale_alpha_continuous <- function (..., range = c(0.1, 1)) 
{
    continuous_scale("alpha", "alpha_c", rescale_pal(range), 
        ...)
}


geom_hex <- function (mapping = NULL, data = NULL, stat = "binhex", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomHex, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


geom_density2d <- function (mapping = NULL, data = NULL, stat = "density2d", position = "identity", 
    ..., lineend = "butt", linejoin = "round", linemitre = 1, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomDensity2d, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(lineend = lineend, linejoin = linejoin, 
            linemitre = linemitre, na.rm = na.rm, ...))
}


median_hilow <- function (x, ...) 
{
    if (!requireNamespace("Hmisc", quietly = TRUE)) 
        stop("Hmisc package required for this function", call. = FALSE)
    fun <- getExportedValue("Hmisc", fun)
    result <- do.call(fun, list(x = quote(x), ...))
    plyr::rename(data.frame(t(result)), c(Median = "y", Mean = "y", 
        Lower = "ymin", Upper = "ymax"), warn_missing = FALSE)
}


theme <- function (line, rect, text, title, aspect.ratio, axis.title, 
    axis.title.x, axis.title.x.top, axis.title.y, axis.title.y.right, 
    axis.text, axis.text.x, axis.text.x.top, axis.text.y, axis.text.y.right, 
    axis.ticks, axis.ticks.x, axis.ticks.y, axis.ticks.length, 
    axis.line, axis.line.x, axis.line.y, legend.background, legend.margin, 
    legend.spacing, legend.spacing.x, legend.spacing.y, legend.key, 
    legend.key.size, legend.key.height, legend.key.width, legend.text, 
    legend.text.align, legend.title, legend.title.align, legend.position, 
    legend.direction, legend.justification, legend.box, legend.box.just, 
    legend.box.margin, legend.box.background, legend.box.spacing, 
    panel.background, panel.border, panel.spacing, panel.spacing.x, 
    panel.spacing.y, panel.grid, panel.grid.major, panel.grid.minor, 
    panel.grid.major.x, panel.grid.major.y, panel.grid.minor.x, 
    panel.grid.minor.y, panel.ontop, plot.background, plot.title, 
    plot.subtitle, plot.caption, plot.margin, strip.background, 
    strip.placement, strip.text, strip.text.x, strip.text.y, 
    strip.switch.pad.grid, strip.switch.pad.wrap, ..., complete = FALSE, 
    validate = TRUE) 
{
    elements <- find_args(..., complete = NULL, validate = NULL)
    if (!is.null(elements$axis.ticks.margin)) {
        warning("`axis.ticks.margin` is deprecated. Please set `margin` property ", 
            " of `axis.text` instead", call. = FALSE)
        elements$axis.ticks.margin <- NULL
    }
    if (!is.null(elements$panel.margin)) {
        warning("`panel.margin` is deprecated. Please use `panel.spacing` property ", 
            "instead", call. = FALSE)
        elements$panel.spacing <- elements$panel.margin
        elements$panel.margin <- NULL
    }
    if (!is.null(elements$panel.margin.x)) {
        warning("`panel.margin.x` is deprecated. Please use `panel.spacing.x` property ", 
            "instead", call. = FALSE)
        elements$panel.spacing.x <- elements$panel.margin.x
        elements$panel.margin.x <- NULL
    }
    if (!is.null(elements$panel.margin.y)) {
        warning("`panel.margin` is deprecated. Please use `panel.spacing` property ", 
            "instead", call. = FALSE)
        elements$panel.spacing.y <- elements$panel.margin.y
        elements$panel.margin.y <- NULL
    }
    if (is.unit(elements$legend.margin) && !is.margin(elements$legend.margin)) {
        warning("`legend.margin` must be specified using `margin()`. For the old ", 
            "behavior use legend.spacing", call. = FALSE)
        elements$legend.spacing <- elements$legend.margin
        elements$legend.margin <- margin()
    }
    if (validate) {
        mapply(validate_element, elements, names(elements))
    }
    if (complete) {
        elements <- lapply(elements, function(el) {
            if (inherits(el, "element") && !inherits(el, "element_blank")) {
                el$inherit.blank <- TRUE
            }
            el
        })
    }
    structure(elements, class = c("theme", "gg"), complete = complete, 
        validate = validate)
}


stat_count <- function (mapping = NULL, data = NULL, geom = "bar", position = "stack", 
    ..., width = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    params <- list(na.rm = na.rm, width = width, ...)
    if (!is.null(params$y)) {
        stop("stat_count() must not be used with a y aesthetic.", 
            call. = FALSE)
    }
    layer(data = data, mapping = mapping, stat = StatCount, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = params)
}


scale_fill_identity <- function (..., guide = "none") 
{
    sc <- discrete_scale("fill", "identity", identity_pal(), 
        ..., guide = guide, super = ScaleDiscreteIdentity)
    sc
}


GeomAbline <- "<environment>"

alpha <- scales::alpha # re-exported from scales package

GeomRibbon <- "<environment>"

scale_color_brewer <- function (..., type = "seq", palette = 1, direction = 1) 
{
    discrete_scale("colour", "brewer", brewer_pal(type, palette, 
        direction), ...)
}


update_labels <- function (p, labels) 
{
    p <- plot_clone(p)
    p$labels <- defaults(labels, p$labels)
    p
}


scale_color_continuous <- function (..., low = "#132B43", high = "#56B1F7", space = "Lab", 
    na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("colour", "gradient", seq_gradient_pal(low, 
        high, space), na.value = na.value, guide = guide, ...)
}


scale_x_continuous <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), 
    labels = waiver(), limits = NULL, expand = waiver(), oob = censor, 
    na.value = NA_real_, trans = "identity", position = "bottom", 
    sec.axis = waiver()) 
{
    sc <- continuous_scale(c("x", "xmin", "xmax", "xend", "xintercept", 
        "xmin_final", "xmax_final", "xlower", "xmiddle", "xupper"), 
        "position_c", identity, name = name, breaks = breaks, 
        minor_breaks = minor_breaks, labels = labels, limits = limits, 
        expand = expand, oob = oob, na.value = na.value, trans = trans, 
        guide = "none", position = position, super = ScaleContinuousPosition)
    if (!is.waive(sec.axis)) {
        if (is.formula(sec.axis)) 
            sec.axis <- sec_axis(sec.axis)
        if (!is.sec_axis(sec.axis)) 
            stop("Secondary axes must be specified using 'sec_axis()'")
        sc$secondary.axis <- sec.axis
    }
    sc
}


ggplot_build <- function (plot) 
{
    plot <- plot_clone(plot)
    if (length(plot$layers) == 0) {
        plot <- plot + geom_blank()
    }
    layers <- plot$layers
    layer_data <- lapply(layers, function(y) y$layer_data(plot$data))
    scales <- plot$scales
    by_layer <- function(f) {
        out <- vector("list", length(data))
        for (i in seq_along(data)) {
            out[[i]] <- f(l = layers[[i]], d = data[[i]])
        }
        out
    }
    layout <- create_layout(plot$facet)
    data <- layout$setup(layer_data, plot$data, plot$plot_env, 
        plot$coordinates)
    data <- layout$map(data)
    data <- by_layer(function(l, d) l$compute_aesthetics(d, plot))
    data <- lapply(data, scales_transform_df, scales = scales)
    scale_x <- function() scales$get_scales("x")
    scale_y <- function() scales$get_scales("y")
    layout$train_position(data, scale_x(), scale_y())
    data <- layout$map_position(data)
    data <- by_layer(function(l, d) l$compute_statistic(d, layout))
    data <- by_layer(function(l, d) l$map_statistic(d, plot))
    scales_add_missing(plot, c("x", "y"), plot$plot_env)
    data <- by_layer(function(l, d) l$compute_geom_1(d))
    data <- by_layer(function(l, d) l$compute_position(d, layout))
    layout$reset_scales()
    layout$train_position(data, scale_x(), scale_y())
    data <- layout$map_position(data)
    npscales <- scales$non_position_scales()
    if (npscales$n() > 0) {
        lapply(data, scales_train_df, scales = npscales)
        data <- lapply(data, scales_map_df, scales = npscales)
    }
    layout$train_ranges(plot$coordinates)
    data <- by_layer(function(l, d) l$compute_geom_2(d))
    data <- by_layer(function(l, d) l$finish_statistics(d))
    data <- layout$finish_data(data)
    list(data = data, layout = layout, plot = plot)
}


scale_x_sqrt <- function (...) 
{
    scale_x_continuous(..., trans = sqrt_trans())
}


aes_all <- function (vars) 
{
    names(vars) <- vars
    vars <- rename_aes(vars)
    structure(lapply(vars, as.name), class = "uneval")
}


scale_colour_continuous <- function (..., low = "#132B43", high = "#56B1F7", space = "Lab", 
    na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("colour", "gradient", seq_gradient_pal(low, 
        high, space), na.value = na.value, guide = guide, ...)
}


GeomPoint <- "<environment>"

GeomHex <- "<environment>"

stat_spoke <- function (...) 
{
    message("stat_spoke is deprecated, please use geom_spoke")
    geom_spoke(...)
}


aes_q <- function (x, y, ...) 
{
    mapping <- list(...)
    if (!missing(x)) 
        mapping["x"] <- list(x)
    if (!missing(y)) 
        mapping["y"] <- list(y)
    as_call <- function(x) {
        if (is.formula(x) && length(x) == 2) {
            x[[2]]
        }
        else if (is.call(x) || is.name(x) || is.atomic(x)) {
            x
        }
        else {
            stop("Aesthetic must be a one-sided formula, call, name, or constant.", 
                call. = FALSE)
        }
    }
    mapping <- lapply(mapping, as_call)
    structure(rename_aes(mapping), class = "uneval")
}


scale_shape_identity <- function (..., guide = "none") 
{
    sc <- continuous_scale("shape", "identity", identity_pal(), 
        ..., guide = guide, super = ScaleDiscreteIdentity)
    sc
}


CoordCartesian <- "<environment>"

geom_abline <- function (mapping = NULL, data = NULL, ..., slope, intercept, 
    na.rm = FALSE, show.legend = NA) 
{
    if (missing(mapping) && missing(slope) && missing(intercept)) {
        slope <- 1
        intercept <- 0
    }
    if (!missing(slope) || !missing(intercept)) {
        if (missing(slope)) 
            slope <- 1
        if (missing(intercept)) 
            intercept <- 0
        data <- data.frame(intercept = intercept, slope = slope)
        mapping <- aes(intercept = intercept, slope = slope)
        show.legend <- FALSE
    }
    layer(data = data, mapping = mapping, stat = StatIdentity, 
        geom = GeomAbline, position = PositionIdentity, show.legend = show.legend, 
        inherit.aes = FALSE, params = list(na.rm = na.rm, ...))
}


scale_linetype_manual <- function (..., values) 
{
    manual_scale("linetype", values, ...)
}


scale_y_sqrt <- function (...) 
{
    scale_y_continuous(..., trans = sqrt_trans())
}


GeomCurve <- "<environment>"

combine_vars <- function (data, env = emptyenv(), vars = NULL, drop = TRUE) 
{
    if (length(vars) == 0) 
        return(data.frame())
    values <- compact(plyr::llply(data, eval_facet_vars, vars = vars, 
        env = env))
    has_all <- unlist(plyr::llply(values, length)) == length(vars)
    if (!any(has_all)) {
        stop("At least one layer must contain all variables used for facetting")
    }
    base <- unique(plyr::ldply(values[has_all]))
    if (!drop) {
        base <- unique_combs(base)
    }
    for (value in values[!has_all]) {
        if (empty(value)) 
            next
        old <- base[setdiff(names(base), names(value))]
        new <- unique(value[intersect(names(base), names(value))])
        if (drop) {
            new <- unique_combs(new)
        }
        base <- rbind(base, df.grid(old, new))
    }
    if (empty(base)) {
        stop("Faceting variables must have at least one value", 
            call. = FALSE)
    }
    base
}


guide_colourbar <- function (title = waiver(), title.position = NULL, title.theme = NULL, 
    title.hjust = NULL, title.vjust = NULL, label = TRUE, label.position = NULL, 
    label.theme = NULL, label.hjust = NULL, label.vjust = NULL, 
    barwidth = NULL, barheight = NULL, nbin = 20, raster = TRUE, 
    ticks = TRUE, draw.ulim = TRUE, draw.llim = TRUE, direction = NULL, 
    default.unit = "line", reverse = FALSE, order = 0, ...) 
{
    if (!is.null(barwidth) && !is.unit(barwidth)) 
        barwidth <- unit(barwidth, default.unit)
    if (!is.null(barheight) && !is.unit(barheight)) 
        barheight <- unit(barheight, default.unit)
    structure(list(title = title, title.position = title.position, 
        title.theme = title.theme, title.hjust = title.hjust, 
        title.vjust = title.vjust, label = label, label.position = label.position, 
        label.theme = label.theme, label.hjust = label.hjust, 
        label.vjust = label.vjust, barwidth = barwidth, barheight = barheight, 
        nbin = nbin, raster = raster, ticks = ticks, draw.ulim = draw.ulim, 
        draw.llim = draw.llim, direction = direction, default.unit = default.unit, 
        reverse = reverse, order = order, available_aes = c("colour", 
            "color", "fill"), ..., name = "colorbar"), class = c("guide", 
        "colorbar"))
}


geom_point <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


update_geom_defaults <- function (geom, new) 
{
    if (is.character(geom)) {
        g <- find_subclass("Geom", geom, parent.frame())
    }
    else if (inherits(geom, "Geom")) {
        g <- geom
    }
    else {
        stop("`geom` must be a string (like \"point\") or a Geom object (like GeomPoint).", 
            call. = FALSE)
    }
    old <- g$default_aes
    g$default_aes <- defaults(new, old)
}


borders <- function (database = "world", regions = ".", fill = NA, colour = "grey50", 
    xlim = NULL, ylim = NULL, ...) 
{
    df <- map_data(database, regions, xlim = xlim, ylim = ylim)
    geom_polygon(aes_(~long, ~lat, group = ~group), data = df, 
        fill = fill, colour = colour, ..., inherit.aes = FALSE)
}


stat_bin_2d <- function (mapping = NULL, data = NULL, geom = "tile", position = "identity", 
    ..., bins = 30, binwidth = NULL, drop = TRUE, na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatBin2d, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(bins = bins, binwidth = binwidth, drop = drop, 
            na.rm = na.rm, ...))
}


coord_quickmap <- function (xlim = NULL, ylim = NULL, expand = TRUE) 
{
    ggproto(NULL, CoordQuickmap, limits = list(x = xlim, y = ylim), 
        expand = expand)
}


mean_cl_boot <- function (x, ...) 
{
    if (!requireNamespace("Hmisc", quietly = TRUE)) 
        stop("Hmisc package required for this function", call. = FALSE)
    fun <- getExportedValue("Hmisc", fun)
    result <- do.call(fun, list(x = quote(x), ...))
    plyr::rename(data.frame(t(result)), c(Median = "y", Mean = "y", 
        Lower = "ymin", Upper = "ymax"), warn_missing = FALSE)
}


stat_summary_hex <- function (mapping = NULL, data = NULL, geom = "hex", position = "identity", 
    ..., bins = 30, binwidth = NULL, drop = TRUE, fun = "mean", 
    fun.args = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatSummaryHex, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(bins = bins, 
            binwidth = binwidth, drop = drop, fun = fun, fun.args = fun.args, 
            na.rm = na.rm, ...))
}


StatQq <- "<environment>"

ylim <- function (...) 
{
    limits(c(...), "y")
}


PositionStack <- "<environment>"

scale_size_discrete <- function (..., range = c(2, 6)) 
{
    warning("Using size for a discrete variable is not advised.", 
        call. = FALSE)
    discrete_scale("size", "size_d", function(n) {
        area <- seq(range[1]^2, range[2]^2, length.out = n)
        sqrt(area)
    }, ...)
}


labeller <- function (..., .rows = NULL, .cols = NULL, keep.as.numeric = NULL, 
    .multi_line = TRUE, .default = label_value) 
{
    if (!is.null(keep.as.numeric)) {
        .Deprecated(old = "keep.as.numeric")
    }
    dots <- list(...)
    .default <- as_labeller(.default)
    function(labels) {
        if (!is.null(.rows) || !is.null(.cols)) {
            margin_labeller <- resolve_labeller(.rows, .cols, 
                labels)
        }
        else {
            margin_labeller <- NULL
        }
        if (is.null(margin_labeller)) {
            labellers <- lapply(dots, as_labeller)
        }
        else {
            margin_labeller <- as_labeller(margin_labeller, default = .default, 
                multi_line = .multi_line)
            if (any(names(dots) %in% names(labels))) {
                stop("Conflict between .", attr(labels, "type"), 
                  " and ", paste(names(dots), collapse = ", "), 
                  call. = FALSE)
            }
        }
        if (is.null(margin_labeller)) {
            out <- lapply(names(labels), function(label) {
                if (label %in% names(labellers)) {
                  labellers[[label]](labels[label])[[1]]
                }
                else {
                  .default(labels[label])[[1]]
                }
            })
            names(out) <- names(labels)
            if (.multi_line) {
                out
            }
            else {
                collapse_labels_lines(out)
            }
        }
        else {
            margin_labeller(labels)
        }
    }
}


GeomQuantile <- "<environment>"

label_wrap_gen <- function (width = 25, multi_line = TRUE) 
{
    fun <- function(labels) {
        labels <- label_value(labels, multi_line = multi_line)
        lapply(labels, function(x) {
            x <- strwrap(x, width = width, simplify = FALSE)
            vapply(x, paste, character(1), collapse = "\n")
        })
    }
    structure(fun, class = "labeller")
}


scale_x_date <- function (name = waiver(), breaks = waiver(), date_breaks = waiver(), 
    labels = waiver(), date_labels = waiver(), minor_breaks = waiver(), 
    date_minor_breaks = waiver(), limits = NULL, expand = waiver(), 
    position = "bottom") 
{
    scale_datetime(c("x", "xmin", "xmax", "xend"), "date", name = name, 
        breaks = breaks, date_breaks = date_breaks, labels = labels, 
        date_labels = date_labels, minor_breaks = minor_breaks, 
        date_minor_breaks = date_minor_breaks, limits = limits, 
        expand = expand, position = position)
}


annotation_raster <- function (raster, xmin, xmax, ymin, ymax, interpolate = FALSE) 
{
    raster <- grDevices::as.raster(raster)
    layer(data = dummy_data(), mapping = NULL, stat = StatIdentity, 
        position = PositionIdentity, geom = GeomRasterAnn, inherit.aes = FALSE, 
        params = list(raster = raster, xmin = xmin, xmax = xmax, 
            ymin = ymin, ymax = ymax, interpolate = interpolate))
}


scale_color_identity <- function (..., guide = "none") 
{
    sc <- discrete_scale("colour", "identity", identity_pal(), 
        ..., guide = guide, super = ScaleDiscreteIdentity)
    sc
}


annotation_logticks <- function (base = 10, sides = "bl", scaled = TRUE, short = unit(0.1, 
    "cm"), mid = unit(0.2, "cm"), long = unit(0.3, "cm"), colour = "black", 
    size = 0.5, linetype = 1, alpha = 1, color = NULL, ...) 
{
    if (!is.null(color)) 
        colour <- color
    layer(data = dummy_data(), mapping = NULL, stat = StatIdentity, 
        geom = GeomLogticks, position = PositionIdentity, show.legend = FALSE, 
        inherit.aes = FALSE, params = list(base = base, sides = sides, 
            scaled = scaled, short = short, mid = mid, long = long, 
            colour = colour, size = size, linetype = linetype, 
            alpha = alpha, ...))
}


xlim <- function (...) 
{
    limits(c(...), "x")
}


GeomBlank <- "<environment>"

geom_vline <- function (mapping = NULL, data = NULL, ..., xintercept, na.rm = FALSE, 
    show.legend = NA) 
{
    if (!missing(xintercept)) {
        data <- data.frame(xintercept = xintercept)
        mapping <- aes(xintercept = xintercept)
        show.legend <- FALSE
    }
    layer(data = data, mapping = mapping, stat = StatIdentity, 
        geom = GeomVline, position = PositionIdentity, show.legend = show.legend, 
        inherit.aes = FALSE, params = list(na.rm = na.rm, ...))
}


expand_limits <- function (...) 
{
    data <- data.frame(..., stringsAsFactors = FALSE)
    geom_blank(aes_all(names(data)), data, inherit.aes = FALSE)
}


stat_bin_hex <- function (mapping = NULL, data = NULL, geom = "hex", position = "identity", 
    ..., bins = 30, binwidth = NULL, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatBinhex, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(bins = bins, 
            binwidth = binwidth, na.rm = na.rm, ...))
}


scale_colour_manual <- function (..., values) 
{
    manual_scale("colour", values, ...)
}


render_strips <- function (x = NULL, y = NULL, labeller, theme) 
{
    list(x = build_strip(x, labeller, theme, TRUE), y = build_strip(y, 
        labeller, theme, FALSE))
}


scale_alpha <- function (..., range = c(0.1, 1)) 
{
    continuous_scale("alpha", "alpha_c", rescale_pal(range), 
        ...)
}


coord_trans <- function (x = "identity", y = "identity", limx = NULL, limy = NULL, 
    xtrans, ytrans) 
{
    if (!missing(xtrans)) {
        gg_dep("1.0.1", "`xtrans` arguments is deprecated; please use `x` instead.")
        x <- xtrans
    }
    if (!missing(ytrans)) {
        gg_dep("1.0.1", "`ytrans` arguments is deprecated; please use `y` instead.")
        y <- ytrans
    }
    if (is.character(x)) 
        x <- as.trans(x)
    if (is.character(y)) 
        y <- as.trans(y)
    ggproto(NULL, CoordTrans, trans = list(x = x, y = y), limits = list(x = limx, 
        y = limy))
}


scale_linetype_identity <- function (..., guide = "none") 
{
    sc <- discrete_scale("linetype", "identity", identity_pal(), 
        ..., guide = guide, super = ScaleDiscreteIdentity)
    sc
}


scale_y_reverse <- function (...) 
{
    scale_y_continuous(..., trans = reverse_trans())
}


coord_fixed <- function (ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) 
{
    ggproto(NULL, CoordFixed, limits = list(x = xlim, y = ylim), 
        ratio = ratio, expand = expand)
}


scale_y_date <- function (name = waiver(), breaks = waiver(), date_breaks = waiver(), 
    labels = waiver(), date_labels = waiver(), minor_breaks = waiver(), 
    date_minor_breaks = waiver(), limits = NULL, expand = waiver(), 
    position = "left") 
{
    scale_datetime(c("y", "ymin", "ymax", "yend"), "date", name = name, 
        breaks = breaks, date_breaks = date_breaks, labels = labels, 
        date_labels = date_labels, minor_breaks = minor_breaks, 
        date_minor_breaks = date_minor_breaks, limits = limits, 
        expand = expand, position = position)
}


stat_ellipse <- function (mapping = NULL, data = NULL, geom = "path", position = "identity", 
    ..., type = "t", level = 0.95, segments = 51, na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatEllipse, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(type = type, 
            level = level, segments = segments, na.rm = na.rm, 
            ...))
}


theme_classic <- function (base_size = 11, base_family = "") 
{
    theme_bw(base_size = base_size, base_family = base_family) %+replace% 
        theme(panel.border = element_blank(), panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black", 
                size = 0.5), legend.key = element_blank(), strip.background = element_rect(fill = "white", 
                colour = "black", size = 1), complete = TRUE)
}


StatBin <- "<environment>"

scale_fill_distiller <- function (..., type = "seq", palette = 1, direction = -1, values = NULL, 
    space = "Lab", na.value = "grey50", guide = "colourbar") 
{
    type <- match.arg(type, c("seq", "div", "qual"))
    if (type == "qual") {
        warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", 
            call. = FALSE)
    }
    continuous_scale("fill", "distiller", gradient_n_pal(brewer_pal(type, 
        palette, direction)(6), values, space), na.value = na.value, 
        guide = guide, ...)
}


element_rect <- function (fill = NULL, colour = NULL, size = NULL, linetype = NULL, 
    color = NULL, inherit.blank = FALSE) 
{
    if (!is.null(color)) 
        colour <- color
    structure(list(fill = fill, colour = colour, size = size, 
        linetype = linetype, inherit.blank = inherit.blank), 
        class = c("element_rect", "element"))
}


CoordMap <- "<environment>"

.stroke <- 3.77952755905512


StatFunction <- "<environment>"

position_nudge <- function (x = 0, y = 0) 
{
    ggproto(NULL, PositionNudge, x = x, y = y)
}


geom_curve <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., curvature = 0.5, angle = 90, ncp = 5, arrow = NULL, 
    lineend = "butt", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomCurve, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(arrow = arrow, curvature = curvature, angle = angle, 
            ncp = ncp, lineend = lineend, na.rm = na.rm, ...))
}


draw_key_vpath <- function (data, params, size) 
{
    segmentsGrob(0.5, 0.1, 0.5, 0.9, gp = gpar(col = alpha(data$colour, 
        data$alpha), lwd = data$size * .pt, lty = data$linetype, 
        lineend = "butt"), arrow = params$arrow)
}


stat_binhex <- function (mapping = NULL, data = NULL, geom = "hex", position = "identity", 
    ..., bins = 30, binwidth = NULL, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatBinhex, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(bins = bins, 
            binwidth = binwidth, na.rm = na.rm, ...))
}


continuous_scale <- function (aesthetics, scale_name, palette, name = waiver(), breaks = waiver(), 
    minor_breaks = waiver(), labels = waiver(), limits = NULL, 
    rescaler = rescale, oob = censor, expand = waiver(), na.value = NA_real_, 
    trans = "identity", guide = "legend", position = "left", 
    super = ScaleContinuous) 
{
    check_breaks_labels(breaks, labels)
    position <- match.arg(position, c("left", "right", "top", 
        "bottom"))
    if (is.null(breaks) && !is_position_aes(aesthetics) && guide != 
        "none") {
        guide <- "none"
    }
    trans <- as.trans(trans)
    if (!is.null(limits)) {
        limits <- trans$transform(limits)
    }
    ggproto(NULL, super, call = match.call(), aesthetics = aesthetics, 
        scale_name = scale_name, palette = palette, range = continuous_range(), 
        limits = limits, trans = trans, na.value = na.value, 
        expand = expand, rescaler = rescaler, oob = oob, name = name, 
        breaks = breaks, minor_breaks = minor_breaks, labels = labels, 
        guide = guide, position = position)
}


cut_interval <- function (x, n = NULL, length = NULL, ...) 
{
    cut(x, breaks(x, "width", n, length), include.lowest = TRUE, 
        ...)
}


gg_dep <- function (version, msg) 
{
    v <- as.package_version(version)
    cv <- utils::packageVersion("ggplot2")
    if (cv[[1, 1]] > v[[1, 1]] || cv[[1, 2]] > v[[1, 2]] + 1) {
        stop(msg, " (Defunct; last used in version ", version, 
            ")", call. = FALSE)
    }
    else if (cv[[1, 2]] > v[[1, 2]]) {
        warning(msg, " (Deprecated; last used in version ", version, 
            ")", call. = FALSE)
    }
    else if (cv[[1, 3]] > v[[1, 3]]) {
        message(msg, " (Deprecated; last used in version ", version, 
            ")")
    }
    invisible()
}


StatIdentity <- "<environment>"

coord_flip <- function (xlim = NULL, ylim = NULL, expand = TRUE) 
{
    ggproto(NULL, CoordFlip, limits = list(x = xlim, y = ylim), 
        expand = expand)
}


as_labeller <- function (x, default = label_value, multi_line = TRUE) 
{
    force(x)
    fun <- function(labels) {
        labels <- lapply(labels, as.character)
        default <- dispatch_args(default, multi_line = multi_line)
        if (is_labeller(x)) {
            x <- dispatch_args(x, multi_line = multi_line)
            x(labels)
        }
        else if (is.function(x)) {
            default(lapply(labels, x))
        }
        else if (is.character(x)) {
            default(lapply(labels, function(label) x[label]))
        }
        else {
            default(labels)
        }
    }
    structure(fun, class = "labeller")
}


StatBinhex <- "<environment>"

GeomContour <- "<environment>"

scale_y_log10 <- function (...) 
{
    scale_y_continuous(..., trans = log10_trans())
}


GeomSmooth <- "<environment>"

GeomErrorbar <- "<environment>"

theme_grey <- function (base_size = 11, base_family = "") 
{
    half_line <- base_size/2
    theme(line = element_line(colour = "black", size = 0.5, linetype = 1, 
        lineend = "butt"), rect = element_rect(fill = "white", 
        colour = "black", size = 0.5, linetype = 1), text = element_text(family = base_family, 
        face = "plain", colour = "black", size = base_size, lineheight = 0.9, 
        hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
        debug = FALSE), axis.line = element_blank(), axis.line.x = NULL, 
        axis.line.y = NULL, axis.text = element_text(size = rel(0.8), 
            colour = "grey30"), axis.text.x = element_text(margin = margin(t = 0.8 * 
            half_line/2), vjust = 1), axis.text.x.top = element_text(margin = margin(b = 0.8 * 
            half_line/2), vjust = 0), axis.text.y = element_text(margin = margin(r = 0.8 * 
            half_line/2), hjust = 1), axis.text.y.right = element_text(margin = margin(l = 0.8 * 
            half_line/2), hjust = 0), axis.ticks = element_line(colour = "grey20"), 
        axis.ticks.length = unit(half_line/2, "pt"), axis.title.x = element_text(margin = margin(t = half_line), 
            vjust = 1), axis.title.x.top = element_text(margin = margin(b = half_line), 
            vjust = 0), axis.title.y = element_text(angle = 90, 
            margin = margin(r = half_line), vjust = 1), axis.title.y.right = element_text(angle = -90, 
            margin = margin(l = half_line), vjust = 0), legend.background = element_rect(colour = NA), 
        legend.spacing = unit(0.4, "cm"), legend.spacing.x = NULL, 
        legend.spacing.y = NULL, legend.margin = margin(0.2, 
            0.2, 0.2, 0.2, "cm"), legend.key = element_rect(fill = "grey95", 
            colour = "white"), legend.key.size = unit(1.2, "lines"), 
        legend.key.height = NULL, legend.key.width = NULL, legend.text = element_text(size = rel(0.8)), 
        legend.text.align = NULL, legend.title = element_text(hjust = 0), 
        legend.title.align = NULL, legend.position = "right", 
        legend.direction = NULL, legend.justification = "center", 
        legend.box = NULL, legend.box.margin = margin(0, 0, 0, 
            0, "cm"), legend.box.background = element_blank(), 
        legend.box.spacing = unit(0.4, "cm"), panel.background = element_rect(fill = "grey92", 
            colour = NA), panel.border = element_blank(), panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white", size = 0.25), 
        panel.spacing = unit(half_line, "pt"), panel.spacing.x = NULL, 
        panel.spacing.y = NULL, panel.ontop = FALSE, strip.background = element_rect(fill = "grey85", 
            colour = NA), strip.text = element_text(colour = "grey10", 
            size = rel(0.8)), strip.text.x = element_text(margin = margin(t = half_line, 
            b = half_line)), strip.text.y = element_text(angle = -90, 
            margin = margin(l = half_line, r = half_line)), strip.placement = "inside", 
        strip.placement.x = NULL, strip.placement.y = NULL, strip.switch.pad.grid = unit(0.1, 
            "cm"), strip.switch.pad.wrap = unit(0.1, "cm"), plot.background = element_rect(colour = "white"), 
        plot.title = element_text(size = rel(1.2), hjust = 0, 
            vjust = 1, margin = margin(b = half_line * 1.2)), 
        plot.subtitle = element_text(size = rel(0.9), hjust = 0, 
            vjust = 1, margin = margin(b = half_line * 0.9)), 
        plot.caption = element_text(size = rel(0.9), hjust = 1, 
            vjust = 1, margin = margin(t = half_line * 0.9)), 
        plot.margin = margin(half_line, half_line, half_line, 
            half_line), complete = TRUE)
}


scale_alpha_identity <- function (..., guide = "none") 
{
    sc <- continuous_scale("alpha", "identity", identity_pal(), 
        ..., guide = guide, super = ScaleContinuousIdentity)
    sc
}


geom_rect <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomRect, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


label_context <- function (labels, multi_line = TRUE, sep = ": ") 
{
    if (length(labels) == 1) {
        label_value(labels, multi_line)
    }
    else {
        label_both(labels, multi_line)
    }
}


rel <- function (x) 
{
    structure(x, class = "rel")
}


Position <- "<environment>"

element_text <- function (family = NULL, face = NULL, colour = NULL, size = NULL, 
    hjust = NULL, vjust = NULL, angle = NULL, lineheight = NULL, 
    color = NULL, margin = NULL, debug = NULL, inherit.blank = FALSE) 
{
    if (!is.null(color)) 
        colour <- color
    structure(list(family = family, face = face, colour = colour, 
        size = size, hjust = hjust, vjust = vjust, angle = angle, 
        lineheight = lineheight, margin = margin, debug = debug, 
        inherit.blank = inherit.blank), class = c("element_text", 
        "element"))
}


element_grob <- function (element, ...) 
{
    UseMethod("element_grob")
}


GeomPolygon <- "<environment>"

autoplot <- function (object, ...) 
{
    UseMethod("autoplot")
}


scale_alpha_discrete <- function (..., range = c(0.1, 1)) 
{
    discrete_scale("alpha", "alpha_d", function(n) seq(range[1], 
        range[2], length.out = n), ...)
}


geom_errorbar <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomErrorbar, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


draw_key_rect <- function (data, params, size) 
{
    rectGrob(gp = gpar(col = NA, fill = alpha(data$fill, data$alpha), 
        lty = data$linetype))
}


ggsave <- function (filename, plot = last_plot(), device = NULL, path = NULL, 
    scale = 1, width = NA, height = NA, units = c("in", "cm", 
        "mm"), dpi = 300, limitsize = TRUE, ...) 
{
    dev <- plot_dev(device, filename, dpi = dpi)
    dim <- plot_dim(c(width, height), scale = scale, units = units, 
        limitsize = limitsize)
    if (!is.null(path)) {
        filename <- file.path(path, filename)
    }
    dev(file = filename, width = dim[1], height = dim[2], ...)
    on.exit(utils::capture.output(grDevices::dev.off()))
    grid.draw(plot)
    invisible()
}


theme_light <- function (base_size = 11, base_family = "") 
{
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
        theme(panel.background = element_rect(fill = "white", 
            colour = NA), panel.border = element_rect(fill = NA, 
            colour = "grey70", size = 0.5), panel.grid.major = element_line(colour = "grey87", 
            size = 0.25), panel.grid.minor = element_line(colour = "grey87", 
            size = 0.125), axis.ticks = element_line(colour = "grey70", 
            size = 0.25), legend.key = element_rect(fill = "white", 
            colour = NA), strip.background = element_rect(fill = "grey70", 
            colour = NA), strip.text = element_text(colour = "white", 
            size = rel(0.8)), complete = TRUE)
}


draw_key_polygon <- function (data, params, size) 
{
    lwd <- min(data$size, min(size)/4)
    rectGrob(width = unit(1, "npc") - unit(lwd, "mm"), height = unit(1, 
        "npc") - unit(lwd, "mm"), gp = gpar(col = data$colour, 
        fill = alpha(data$fill, data$alpha), lty = data$linetype, 
        lwd = lwd * .pt, linejoin = "mitre"))
}


is.Coord <- function (x) 
inherits(x, "Coord")


layer_scales <- function (plot, i = 1L, j = 1L) 
{
    b <- ggplot_build(plot)
    layout <- b$layout$panel_layout
    selected <- layout[layout$ROW == i & layout$COL == j, , drop = FALSE]
    list(x = b$layout$panel_scales$x[[selected$SCALE_X]], y = b$layout$panel_scales$y[[selected$SCALE_Y]])
}


geom_qq <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", 
    ..., distribution = stats::qnorm, dparams = list(), na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatQq, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(distribution = distribution, dparams = dparams, 
            na.rm = na.rm, ...))
}


ggplot <- function (data = NULL, mapping = aes(), ..., environment = parent.frame()) 
{
    UseMethod("ggplot")
}


PositionDodge <- "<environment>"

scale_x_discrete <- function (..., expand = waiver(), position = "bottom") 
{
    sc <- discrete_scale(c("x", "xmin", "xmax", "xend"), "position_d", 
        identity, ..., expand = expand, guide = "none", position = position, 
        super = ScaleDiscretePosition)
    sc$range_c <- continuous_range()
    sc
}


find_panel <- function (table) 
{
    layout <- table$layout
    panels <- layout[grepl("^panel", layout$name), , drop = FALSE]
    data.frame(t = min(panels$t), r = max(panels$r), b = max(panels$b), 
        l = min(panels$l))
}


ScaleDiscretePosition <- "<environment>"

theme_update <- function (...) 
{
    theme_set(theme_get() + theme(...))
}


stat_bin2d <- function (mapping = NULL, data = NULL, geom = "tile", position = "identity", 
    ..., bins = 30, binwidth = NULL, drop = TRUE, na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatBin2d, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(bins = bins, binwidth = binwidth, drop = drop, 
            na.rm = na.rm, ...))
}


GeomText <- "<environment>"

aes_auto <- function (data = NULL, ...) 
{
    warning("aes_auto() is deprecated", call. = FALSE)
    if (is.null(data)) {
        stop("aes_auto requires data.frame or names of data.frame.")
    }
    else if (is.data.frame(data)) {
        vars <- names(data)
    }
    else {
        vars <- data
    }
    vars <- intersect(.all_aesthetics, vars)
    names(vars) <- vars
    aes <- lapply(vars, function(x) parse(text = x)[[1]])
    if (length(match.call()) > 2) {
        args <- as.list(match.call()[-1])
        aes <- c(aes, args[names(args) != "data"])
    }
    structure(rename_aes(aes), class = "uneval")
}


element_line <- function (colour = NULL, size = NULL, linetype = NULL, lineend = NULL, 
    color = NULL, arrow = NULL, inherit.blank = FALSE) 
{
    if (!is.null(color)) 
        colour <- color
    if (is.null(arrow)) 
        arrow <- FALSE
    structure(list(colour = colour, size = size, linetype = linetype, 
        lineend = lineend, arrow = arrow, inherit.blank = inherit.blank), 
        class = c("element_line", "element"))
}


draw_key_pointrange <- function (data, params, size) 
{
    grobTree(draw_key_vpath(data, params, size), draw_key_point(transform(data, 
        size = data$size * 4), params))
}


GeomLabel <- "<environment>"

geom_boxplot <- function (mapping = NULL, data = NULL, stat = "boxplot", position = "dodge", 
    ..., outlier.colour = NULL, outlier.color = NULL, outlier.fill = NULL, 
    outlier.shape = 19, outlier.size = 1.5, outlier.stroke = 0.5, 
    outlier.alpha = NULL, notch = FALSE, notchwidth = 0.5, varwidth = FALSE, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomBoxplot, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(outlier.colour = outlier.color %||% outlier.colour, 
            outlier.fill = outlier.fill, outlier.shape = outlier.shape, 
            outlier.size = outlier.size, outlier.stroke = outlier.stroke, 
            outlier.alpha = outlier.alpha, notch = notch, notchwidth = notchwidth, 
            varwidth = varwidth, na.rm = na.rm, ...))
}


geom_crossbar <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., fatten = 2.5, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomCrossbar, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(fatten = fatten, na.rm = na.rm, ...))
}


scale_colour_brewer <- function (..., type = "seq", palette = 1, direction = 1) 
{
    discrete_scale("colour", "brewer", brewer_pal(type, palette, 
        direction), ...)
}


StatBoxplot <- "<environment>"

mean_sdl <- function (x, ...) 
{
    if (!requireNamespace("Hmisc", quietly = TRUE)) 
        stop("Hmisc package required for this function", call. = FALSE)
    fun <- getExportedValue("Hmisc", fun)
    result <- do.call(fun, list(x = quote(x), ...))
    plyr::rename(data.frame(t(result)), c(Median = "y", Mean = "y", 
        Lower = "ymin", Upper = "ymax"), warn_missing = FALSE)
}


Geom <- "<environment>"

geom_line <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomLine, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


scale_x_time <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), 
    labels = waiver(), limits = NULL, expand = waiver(), oob = censor, 
    na.value = NA_real_, position = "bottom") 
{
    scale_x_continuous(name = name, breaks = breaks, labels = labels, 
        minor_breaks = minor_breaks, limits = limits, expand = expand, 
        oob = oob, na.value = na.value, position = position, 
        trans = scales::hms_trans())
}


wrap_dims <- function (n, nrow = NULL, ncol = NULL) 
{
    if (is.null(ncol) && is.null(nrow)) {
        rc <- grDevices::n2mfrow(n)
        nrow <- rc[2]
        ncol <- rc[1]
    }
    else if (is.null(ncol)) {
        ncol <- ceiling(n/nrow)
    }
    else if (is.null(nrow)) {
        nrow <- ceiling(n/ncol)
    }
    stopifnot(nrow * ncol >= n)
    c(nrow, ncol)
}


scale_colour_grey <- function (..., start = 0.2, end = 0.8, na.value = "red") 
{
    discrete_scale("colour", "grey", grey_pal(start, end), na.value = na.value, 
        ...)
}


theme_get <- function () 
{
    theme_env$current
}


geom_col <- function (mapping = NULL, data = NULL, position = "stack", ..., 
    width = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = "identity", 
        geom = GeomCol, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(width = width, 
            na.rm = na.rm, ...))
}


qplot <- function (x, y = NULL, ..., data, facets = NULL, margins = FALSE, 
    geom = "auto", xlim = c(NA, NA), ylim = c(NA, NA), log = "", 
    main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), 
    asp = NA, stat = NULL, position = NULL) 
{
    if (!missing(stat)) 
        warning("`stat` is deprecated", call. = FALSE)
    if (!missing(position)) 
        warning("`position` is deprecated", call. = FALSE)
    if (!is.character(geom)) 
        stop("`geom` must be a character vector", call. = FALSE)
    argnames <- names(as.list(match.call(expand.dots = FALSE)[-1]))
    arguments <- as.list(match.call()[-1])
    env <- parent.frame()
    aesthetics <- compact(arguments[.all_aesthetics])
    aesthetics <- aesthetics[!is.constant(aesthetics)]
    aes_names <- names(aesthetics)
    aesthetics <- rename_aes(aesthetics)
    class(aesthetics) <- "uneval"
    if (missing(data)) {
        data <- data.frame()
        facetvars <- all.vars(facets)
        facetvars <- facetvars[facetvars != "."]
        names(facetvars) <- facetvars
        facetsdf <- as.data.frame(mget(facetvars, envir = env))
        if (nrow(facetsdf)) 
            data <- facetsdf
    }
    if ("auto" %in% geom) {
        if ("sample" %in% aes_names) {
            geom[geom == "auto"] <- "qq"
        }
        else if (missing(y)) {
            x <- eval(aesthetics$x, data, env)
            if (is.discrete(x)) {
                geom[geom == "auto"] <- "bar"
            }
            else {
                geom[geom == "auto"] <- "histogram"
            }
            if (missing(ylab)) 
                ylab <- "count"
        }
        else {
            if (missing(x)) {
                aesthetics$x <- bquote(seq_along(.(y)), aesthetics)
            }
            geom[geom == "auto"] <- "point"
        }
    }
    p <- ggplot(data, aesthetics, environment = env)
    if (is.null(facets)) {
        p <- p + facet_null()
    }
    else if (is.formula(facets) && length(facets) == 2) {
        p <- p + facet_wrap(facets)
    }
    else {
        p <- p + facet_grid(facets = deparse(facets), margins = margins)
    }
    if (!is.null(main)) 
        p <- p + ggtitle(main)
    for (g in geom) {
        params <- arguments[setdiff(names(arguments), c(aes_names, 
            argnames))]
        params <- lapply(params, eval, parent.frame())
        p <- p + do.call(paste0("geom_", g), params)
    }
    logv <- function(var) var %in% strsplit(log, "")[[1]]
    if (logv("x")) 
        p <- p + scale_x_log10()
    if (logv("y")) 
        p <- p + scale_y_log10()
    if (!is.na(asp)) 
        p <- p + theme(aspect.ratio = asp)
    if (!missing(xlab)) 
        p <- p + xlab(xlab)
    if (!missing(ylab)) 
        p <- p + ylab(ylab)
    if (!missing(xlim)) 
        p <- p + xlim(xlim)
    if (!missing(ylim)) 
        p <- p + ylim(ylim)
    p
}


remove_missing <- function (df, na.rm = FALSE, vars = names(df), name = "", finite = FALSE) 
{
    stopifnot(is.logical(na.rm))
    vars <- intersect(vars, names(df))
    if (name != "") 
        name <- paste(" (", name, ")", sep = "")
    if (finite) {
        missing <- !finite.cases(df[, vars, drop = FALSE])
        str <- "non-finite"
    }
    else {
        missing <- !stats::complete.cases(df[, vars, drop = FALSE])
        str <- "missing"
    }
    if (any(missing)) {
        df <- df[!missing, ]
        if (!na.rm) {
            warning_wrap("Removed ", sum(missing), " rows containing ", 
                str, " values", name, ".")
        }
    }
    df
}


scale_linetype_continuous <- function (...) 
{
    stop("A continuous variable can not be mapped to linetype", 
        call. = FALSE)
}


stat_summary_2d <- function (mapping = NULL, data = NULL, geom = "tile", position = "identity", 
    ..., bins = 30, binwidth = NULL, drop = TRUE, fun = "mean", 
    fun.args = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatSummary2d, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(bins = bins, 
            binwidth = binwidth, drop = drop, fun = fun, fun.args = fun.args, 
            na.rm = na.rm, ...))
}


geom_violin <- function (mapping = NULL, data = NULL, stat = "ydensity", position = "dodge", 
    ..., draw_quantiles = NULL, trim = TRUE, scale = "area", 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, 
            na.rm = na.rm, ...))
}


position_jitter <- function (width = NULL, height = NULL) 
{
    ggproto(NULL, PositionJitter, width = width, height = height)
}


GeomPointrange <- "<environment>"

calc_element <- function (element, theme, verbose = FALSE) 
{
    if (verbose) 
        message(element, " --> ", appendLF = FALSE)
    if (inherits(theme[[element]], "element_blank")) {
        if (verbose) 
            message("element_blank (no inheritance)")
        return(theme[[element]])
    }
    if (!is.null(theme[[element]]) && !inherits(theme[[element]], 
        .element_tree[[element]]$class)) {
        stop(element, " should have class ", .element_tree[[element]]$class)
    }
    pnames <- .element_tree[[element]]$inherit
    if (is.null(pnames)) {
        nullprops <- vapply(theme[[element]], is.null, logical(1))
        if (any(nullprops)) {
            stop("Theme element '", element, "' has NULL property: ", 
                paste(names(nullprops)[nullprops], collapse = ", "))
        }
        if (verbose) 
            message("nothing (top level)")
        return(theme[[element]])
    }
    if (verbose) 
        message(paste(pnames, collapse = ", "))
    parents <- lapply(pnames, calc_element, theme, verbose)
    Reduce(combine_elements, parents, theme[[element]])
}


stat_unique <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatUnique, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = na.rm, 
            ...))
}


scale_y_time <- function (name = waiver(), breaks = waiver(), minor_breaks = waiver(), 
    labels = waiver(), limits = NULL, expand = waiver(), oob = censor, 
    na.value = NA_real_, position = "left") 
{
    scale_y_continuous(name = name, breaks = breaks, labels = labels, 
        minor_breaks = minor_breaks, limits = limits, expand = expand, 
        oob = oob, na.value = na.value, position = position, 
        trans = scales::hms_trans())
}


theme_set <- function (new) 
{
    missing <- setdiff(names(theme_gray()), names(new))
    if (length(missing) > 0) {
        warning("New theme missing the following elements: ", 
            paste(missing, collapse = ", "), call. = FALSE)
    }
    old <- theme_env$current
    theme_env$current <- new
    invisible(old)
}


StatUnique <- "<environment>"

StatDensity <- "<environment>"

PositionJitter <- "<environment>"

scale_fill_brewer <- function (..., type = "seq", palette = 1, direction = 1) 
{
    discrete_scale("fill", "brewer", brewer_pal(type, palette, 
        direction), ...)
}


stat_density_2d <- function (mapping = NULL, data = NULL, geom = "density_2d", position = "identity", 
    ..., contour = TRUE, n = 100, h = NULL, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatDensity2d, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = na.rm, 
            contour = contour, n = n, h = h, ...))
}


position_jitterdodge <- function (jitter.width = NULL, jitter.height = 0, dodge.width = 0.75) 
{
    ggproto(NULL, PositionJitterdodge, jitter.width = jitter.width, 
        jitter.height = jitter.height, dodge.width = dodge.width)
}


position_stack <- function (vjust = 1, reverse = FALSE) 
{
    ggproto(NULL, PositionStack, vjust = vjust, reverse = reverse)
}


panel_rows <- function (table) 
{
    panels <- table$layout[grepl("^panel", table$layout$name), 
        , drop = FALSE]
    unique(panels[, c("t", "b")])
}


lims <- function (...) 
{
    args <- list(...)
    if (any(!has_name(args))) {
        stop("All arguments must be named", call. = FALSE)
    }
    Map(limits, args, names(args))
}


geom_density <- function (mapping = NULL, data = NULL, stat = "density", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomDensity, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


scale_colour_datetime <- function () 
{
    scale_colour_continuous(trans = "time")
}


geom_count <- function (mapping = NULL, data = NULL, stat = "sum", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


dup_axis <- function (trans = ~., name = derive(), breaks = derive(), labels = derive()) 
{
    sec_axis(trans, name, breaks, labels)
}


scale_x_reverse <- function (...) 
{
    scale_x_continuous(..., trans = reverse_trans())
}


draw_key_vline <- function (data, params, size) 
{
    segmentsGrob(0.5, 0, 0.5, 1, gp = gpar(col = alpha(data$colour, 
        data$alpha), lwd = data$size * .pt, lty = data$linetype, 
        lineend = "butt"))
}


layer_grob <- function (plot, i = 1L) 
{
    b <- ggplot_build(plot)
    b$plot$layers[[i]]$draw_geom(b$data[[i]], b$layout, b$plot$coordinates)
}


geom_text <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., parse = FALSE, nudge_x = 0, nudge_y = 0, check_overlap = FALSE, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`", 
                call. = FALSE)
        }
        position <- position_nudge(nudge_x, nudge_y)
    }
    layer(data = data, mapping = mapping, stat = stat, geom = GeomText, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(parse = parse, check_overlap = check_overlap, 
            na.rm = na.rm, ...))
}


scale_shape_continuous <- function (...) 
{
    stop("A continuous variable can not be mapped to shape", 
        call. = FALSE)
}


theme_void <- function (base_size = 11, base_family = "") 
{
    theme(line = element_blank(), rect = element_blank(), text = element_text(family = base_family, 
        face = "plain", colour = "black", size = base_size, lineheight = 0.9, 
        hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
        debug = FALSE), axis.text = element_blank(), axis.title = element_blank(), 
        legend.text = element_text(size = rel(0.8)), legend.title = element_text(hjust = 0), 
        strip.text = element_text(size = rel(0.8)), plot.margin = unit(c(0, 
            0, 0, 0), "lines"), complete = TRUE)
}


geom_label <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., parse = FALSE, nudge_x = 0, nudge_y = 0, label.padding = unit(0.25, 
        "lines"), label.r = unit(0.15, "lines"), label.size = 0.25, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            stop("Specify either `position` or `nudge_x`/`nudge_y`", 
                call. = FALSE)
        }
        position <- position_nudge(nudge_x, nudge_y)
    }
    layer(data = data, mapping = mapping, stat = stat, geom = GeomLabel, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(parse = parse, label.padding = label.padding, 
            label.r = label.r, label.size = label.size, na.rm = na.rm, 
            ...))
}


geom_tile <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomTile, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


theme_gray <- function (base_size = 11, base_family = "") 
{
    half_line <- base_size/2
    theme(line = element_line(colour = "black", size = 0.5, linetype = 1, 
        lineend = "butt"), rect = element_rect(fill = "white", 
        colour = "black", size = 0.5, linetype = 1), text = element_text(family = base_family, 
        face = "plain", colour = "black", size = base_size, lineheight = 0.9, 
        hjust = 0.5, vjust = 0.5, angle = 0, margin = margin(), 
        debug = FALSE), axis.line = element_blank(), axis.line.x = NULL, 
        axis.line.y = NULL, axis.text = element_text(size = rel(0.8), 
            colour = "grey30"), axis.text.x = element_text(margin = margin(t = 0.8 * 
            half_line/2), vjust = 1), axis.text.x.top = element_text(margin = margin(b = 0.8 * 
            half_line/2), vjust = 0), axis.text.y = element_text(margin = margin(r = 0.8 * 
            half_line/2), hjust = 1), axis.text.y.right = element_text(margin = margin(l = 0.8 * 
            half_line/2), hjust = 0), axis.ticks = element_line(colour = "grey20"), 
        axis.ticks.length = unit(half_line/2, "pt"), axis.title.x = element_text(margin = margin(t = half_line), 
            vjust = 1), axis.title.x.top = element_text(margin = margin(b = half_line), 
            vjust = 0), axis.title.y = element_text(angle = 90, 
            margin = margin(r = half_line), vjust = 1), axis.title.y.right = element_text(angle = -90, 
            margin = margin(l = half_line), vjust = 0), legend.background = element_rect(colour = NA), 
        legend.spacing = unit(0.4, "cm"), legend.spacing.x = NULL, 
        legend.spacing.y = NULL, legend.margin = margin(0.2, 
            0.2, 0.2, 0.2, "cm"), legend.key = element_rect(fill = "grey95", 
            colour = "white"), legend.key.size = unit(1.2, "lines"), 
        legend.key.height = NULL, legend.key.width = NULL, legend.text = element_text(size = rel(0.8)), 
        legend.text.align = NULL, legend.title = element_text(hjust = 0), 
        legend.title.align = NULL, legend.position = "right", 
        legend.direction = NULL, legend.justification = "center", 
        legend.box = NULL, legend.box.margin = margin(0, 0, 0, 
            0, "cm"), legend.box.background = element_blank(), 
        legend.box.spacing = unit(0.4, "cm"), panel.background = element_rect(fill = "grey92", 
            colour = NA), panel.border = element_blank(), panel.grid.major = element_line(colour = "white"), 
        panel.grid.minor = element_line(colour = "white", size = 0.25), 
        panel.spacing = unit(half_line, "pt"), panel.spacing.x = NULL, 
        panel.spacing.y = NULL, panel.ontop = FALSE, strip.background = element_rect(fill = "grey85", 
            colour = NA), strip.text = element_text(colour = "grey10", 
            size = rel(0.8)), strip.text.x = element_text(margin = margin(t = half_line, 
            b = half_line)), strip.text.y = element_text(angle = -90, 
            margin = margin(l = half_line, r = half_line)), strip.placement = "inside", 
        strip.placement.x = NULL, strip.placement.y = NULL, strip.switch.pad.grid = unit(0.1, 
            "cm"), strip.switch.pad.wrap = unit(0.1, "cm"), plot.background = element_rect(colour = "white"), 
        plot.title = element_text(size = rel(1.2), hjust = 0, 
            vjust = 1, margin = margin(b = half_line * 1.2)), 
        plot.subtitle = element_text(size = rel(0.9), hjust = 0, 
            vjust = 1, margin = margin(b = half_line * 0.9)), 
        plot.caption = element_text(size = rel(0.9), hjust = 1, 
            vjust = 1, margin = margin(t = half_line * 0.9)), 
        plot.margin = margin(half_line, half_line, half_line, 
            half_line), complete = TRUE)
}


StatBindot <- "<environment>"

geom_dotplot <- function (mapping = NULL, data = NULL, position = "identity", 
    ..., binwidth = NULL, binaxis = "x", method = "dotdensity", 
    binpositions = "bygroup", stackdir = "up", stackratio = 1, 
    dotsize = 1, stackgroups = FALSE, origin = NULL, right = TRUE, 
    width = 0.9, drop = FALSE, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    if (!is.null(position) && (identical(position, "stack") || 
        (inherits(position, "PositionStack")))) 
        message("position=\"stack\" doesn't work properly with geom_dotplot. Use stackgroups=TRUE instead.")
    if (stackgroups && method == "dotdensity" && binpositions == 
        "bygroup") 
        message("geom_dotplot called with stackgroups=TRUE and method=\"dotdensity\". You probably want to set binpositions=\"all\"")
    layer(data = data, mapping = mapping, stat = StatBindot, 
        geom = GeomDotplot, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(binaxis = binaxis, 
            binwidth = binwidth, binpositions = binpositions, 
            method = method, origin = origin, right = right, 
            width = width, drop = drop, stackdir = stackdir, 
            stackratio = stackratio, dotsize = dotsize, stackgroups = stackgroups, 
            na.rm = na.rm, ...))
}


is.facet <- function (x) 
inherits(x, "Facet")


StatSummaryHex <- "<environment>"

geom_density_2d <- function (mapping = NULL, data = NULL, stat = "density2d", position = "identity", 
    ..., lineend = "butt", linejoin = "round", linemitre = 1, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomDensity2d, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(lineend = lineend, linejoin = linejoin, 
            linemitre = linemitre, na.rm = na.rm, ...))
}


Scale <- "<environment>"

geom_rug <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., sides = "bl", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomRug, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(sides = sides, na.rm = na.rm, ...))
}


scale_color_gradientn <- function (..., colours, values = NULL, space = "Lab", na.value = "grey50", 
    guide = "colourbar", colors) 
{
    colours <- if (missing(colours)) 
        colors
    else colours
    continuous_scale("colour", "gradientn", gradient_n_pal(colours, 
        values, space), na.value = na.value, guide = guide, ...)
}


ScaleDiscreteIdentity <- "<environment>"

CoordTrans <- "<environment>"

facet_wrap <- function (facets, nrow = NULL, ncol = NULL, scales = "fixed", 
    shrink = TRUE, labeller = "label_value", as.table = TRUE, 
    switch = NULL, drop = TRUE, dir = "h", strip.position = "top") 
{
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", 
        "free"))
    dir <- match.arg(dir, c("h", "v"))
    free <- list(x = any(scales %in% c("free_x", "free")), y = any(scales %in% 
        c("free_y", "free")))
    if (!is.null(switch)) {
        .Deprecated("strip.position", old = "switch")
        strip.position <- if (switch == "x") 
            "bottom"
        else "left"
    }
    strip.position <- match.arg(strip.position, c("top", "bottom", 
        "left", "right"))
    if (identical(dir, "v")) {
        nrow_swap <- ncol
        ncol_swap <- nrow
        nrow <- sanitise_dim(nrow_swap)
        ncol <- sanitise_dim(ncol_swap)
    }
    else {
        nrow <- sanitise_dim(nrow)
        ncol <- sanitise_dim(ncol)
    }
    labeller <- check_labeller(labeller)
    ggproto(NULL, FacetWrap, shrink = shrink, params = list(facets = as.quoted(facets), 
        free = free, as.table = as.table, strip.position = strip.position, 
        drop = drop, ncol = ncol, nrow = nrow, labeller = labeller, 
        dir = dir))
}


update_stat_defaults <- function (stat, new) 
{
    if (is.character(stat)) {
        g <- find_subclass("Stat", stat, parent.frame())
    }
    else if (inherits(stat, "Stat")) {
        g <- stat
    }
    else {
        stop("`stat` must be a string (like \"point\") or a Stat object (like StatBin).", 
            call. = FALSE)
    }
    old <- g$default_aes
    g$default_aes <- defaults(new, old)
}


stat_quantile <- function (mapping = NULL, data = NULL, geom = "quantile", position = "identity", 
    ..., quantiles = c(0.25, 0.5, 0.75), formula = NULL, method = "rq", 
    method.args = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatQuantile, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(quantiles = quantiles, 
            formula = formula, method = method, method.args = method.args, 
            na.rm = na.rm, ...))
}


geom_errorbarh <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomErrorbarh, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


CoordFixed <- "<environment>"

stat_bin <- function (mapping = NULL, data = NULL, geom = "bar", position = "stack", 
    ..., binwidth = NULL, bins = NULL, center = NULL, boundary = NULL, 
    breaks = NULL, closed = c("right", "left"), pad = FALSE, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatBin, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(binwidth = binwidth, bins = bins, center = center, 
            boundary = boundary, breaks = breaks, closed = closed, 
            pad = pad, na.rm = na.rm, ...))
}


stat_summary_bin <- function (mapping = NULL, data = NULL, geom = "pointrange", position = "identity", 
    ..., fun.data = NULL, fun.y = NULL, fun.ymax = NULL, fun.ymin = NULL, 
    fun.args = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatSummaryBin, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(fun.data = fun.data, 
            fun.y = fun.y, fun.ymax = fun.ymax, fun.ymin = fun.ymin, 
            fun.args = fun.args, na.rm = na.rm, ...))
}


theme_minimal <- function (base_size = 11, base_family = "") 
{
    theme_bw(base_size = base_size, base_family = base_family) %+replace% 
        theme(axis.ticks = element_blank(), legend.background = element_blank(), 
            legend.key = element_blank(), panel.background = element_blank(), 
            panel.border = element_blank(), strip.background = element_blank(), 
            plot.background = element_blank(), complete = TRUE)
}


StatDensity2d <- "<environment>"

scale_color_distiller <- function (..., type = "seq", palette = 1, direction = -1, values = NULL, 
    space = "Lab", na.value = "grey50", guide = "colourbar") 
{
    type <- match.arg(type, c("seq", "div", "qual"))
    if (type == "qual") {
        warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", 
            call. = FALSE)
    }
    continuous_scale("colour", "distiller", gradient_n_pal(brewer_pal(type, 
        palette, direction)(6), values, space), na.value = na.value, 
        guide = guide, ...)
}


ggplot_gtable <- function (data) 
{
    plot <- data$plot
    layout <- data$layout
    data <- data$data
    theme <- plot_theme(plot)
    geom_grobs <- Map(function(l, d) l$draw_geom(d, layout, plot$coordinates), 
        plot$layers, data)
    plot_table <- layout$render(geom_grobs, data, plot$coordinates, 
        theme, plot$labels)
    position <- theme$legend.position
    if (length(position) == 2) {
        position <- "manual"
    }
    legend_box <- if (position != "none") {
        build_guides(plot$scales, plot$layers, plot$mapping, 
            position, theme, plot$guides, plot$labels)
    }
    else {
        zeroGrob()
    }
    if (is.zero(legend_box)) {
        position <- "none"
    }
    else {
        legend_width <- gtable_width(legend_box)
        legend_height <- gtable_height(legend_box)
        just <- valid.just(theme$legend.justification)
        xjust <- just[1]
        yjust <- just[2]
        if (position == "manual") {
            xpos <- theme$legend.position[1]
            ypos <- theme$legend.position[2]
            legend_box <- editGrob(legend_box, vp = viewport(x = xpos, 
                y = ypos, just = c(xjust, yjust), height = legend_height, 
                width = legend_width))
        }
        else {
            legend_box <- editGrob(legend_box, vp = viewport(x = xjust, 
                y = yjust, just = c(xjust, yjust)))
            legend_box <- gtable_add_rows(legend_box, unit(yjust, 
                "null"))
            legend_box <- gtable_add_rows(legend_box, unit(1 - 
                yjust, "null"), 0)
            legend_box <- gtable_add_cols(legend_box, unit(xjust, 
                "null"), 0)
            legend_box <- gtable_add_cols(legend_box, unit(1 - 
                xjust, "null"))
        }
    }
    panel_dim <- find_panel(plot_table)
    theme$legend.box.spacing <- theme$legend.box.spacing %||% 
        unit(0.2, "cm")
    if (position == "left") {
        plot_table <- gtable_add_cols(plot_table, theme$legend.box.spacing, 
            pos = 0)
        plot_table <- gtable_add_cols(plot_table, legend_width, 
            pos = 0)
        plot_table <- gtable_add_grob(plot_table, legend_box, 
            clip = "off", t = panel_dim$t, b = panel_dim$b, l = 1, 
            r = 1, name = "guide-box")
    }
    else if (position == "right") {
        plot_table <- gtable_add_cols(plot_table, theme$legend.box.spacing, 
            pos = -1)
        plot_table <- gtable_add_cols(plot_table, legend_width, 
            pos = -1)
        plot_table <- gtable_add_grob(plot_table, legend_box, 
            clip = "off", t = panel_dim$t, b = panel_dim$b, l = -1, 
            r = -1, name = "guide-box")
    }
    else if (position == "bottom") {
        plot_table <- gtable_add_rows(plot_table, theme$legend.box.spacing, 
            pos = -1)
        plot_table <- gtable_add_rows(plot_table, legend_height, 
            pos = -1)
        plot_table <- gtable_add_grob(plot_table, legend_box, 
            clip = "off", t = -1, b = -1, l = panel_dim$l, r = panel_dim$r, 
            name = "guide-box")
    }
    else if (position == "top") {
        plot_table <- gtable_add_rows(plot_table, theme$legend.box.spacing, 
            pos = 0)
        plot_table <- gtable_add_rows(plot_table, legend_height, 
            pos = 0)
        plot_table <- gtable_add_grob(plot_table, legend_box, 
            clip = "off", t = 1, b = 1, l = panel_dim$l, r = panel_dim$r, 
            name = "guide-box")
    }
    else if (position == "manual") {
        plot_table <- gtable_add_grob(plot_table, legend_box, 
            t = panel_dim$t, b = panel_dim$b, l = panel_dim$l, 
            r = panel_dim$r, clip = "off", name = "guide-box")
    }
    title <- element_render(theme, "plot.title", plot$labels$title, 
        expand_y = TRUE)
    title_height <- grobHeight(title)
    subtitle <- element_render(theme, "plot.subtitle", plot$labels$subtitle, 
        expand_y = TRUE)
    subtitle_height <- grobHeight(subtitle)
    caption <- element_render(theme, "plot.caption", plot$labels$caption, 
        expand_y = TRUE)
    caption_height <- grobHeight(caption)
    pans <- plot_table$layout[grepl("^panel", plot_table$layout$name), 
        , drop = FALSE]
    plot_table <- gtable_add_rows(plot_table, subtitle_height, 
        pos = 0)
    plot_table <- gtable_add_grob(plot_table, subtitle, name = "subtitle", 
        t = 1, b = 1, l = min(pans$l), r = max(pans$r), clip = "off")
    plot_table <- gtable_add_rows(plot_table, title_height, pos = 0)
    plot_table <- gtable_add_grob(plot_table, title, name = "title", 
        t = 1, b = 1, l = min(pans$l), r = max(pans$r), clip = "off")
    plot_table <- gtable_add_rows(plot_table, caption_height, 
        pos = -1)
    plot_table <- gtable_add_grob(plot_table, caption, name = "caption", 
        t = -1, b = -1, l = min(pans$l), r = max(pans$r), clip = "off")
    plot_table <- gtable_add_rows(plot_table, theme$plot.margin[1], 
        pos = 0)
    plot_table <- gtable_add_cols(plot_table, theme$plot.margin[2])
    plot_table <- gtable_add_rows(plot_table, theme$plot.margin[3])
    plot_table <- gtable_add_cols(plot_table, theme$plot.margin[4], 
        pos = 0)
    if (inherits(theme$plot.background, "element")) {
        plot_table <- gtable_add_grob(plot_table, element_render(theme, 
            "plot.background"), t = 1, l = 1, b = -1, r = -1, 
            name = "background", z = -Inf)
        plot_table$layout <- plot_table$layout[c(nrow(plot_table$layout), 
            1:(nrow(plot_table$layout) - 1)), ]
        plot_table$grobs <- plot_table$grobs[c(nrow(plot_table$layout), 
            1:(nrow(plot_table$layout) - 1))]
    }
    plot_table
}


ScaleContinuousDatetime <- "<environment>"

GeomAnnotationMap <- "<environment>"

theme_replace <- function (...) 
{
    theme_set(theme_get() %+replace% theme(...))
}


scale_color_discrete <- function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, 
    direction = 1, na.value = "grey50") 
{
    discrete_scale("colour", "hue", hue_pal(h, c, l, h.start, 
        direction), na.value = na.value, ...)
}


scale_colour_date <- function () 
{
    scale_colour_continuous(trans = "date")
}


margin <- function (t = 0, r = 0, b = 0, l = 0, unit = "pt") 
{
    structure(unit(c(t, r, b, l), unit), class = c("margin", 
        "unit"))
}


layer_data <- function (plot, i = 1L) 
{
    ggplot_build(plot)$data[[i]]
}


aes_string <- function (x, y, ...) 
{
    mapping <- list(...)
    if (!missing(x)) 
        mapping["x"] <- list(x)
    if (!missing(y)) 
        mapping["y"] <- list(y)
    mapping <- lapply(mapping, function(x) {
        if (is.character(x)) {
            parse(text = x)[[1]]
        }
        else {
            x
        }
    })
    structure(rename_aes(mapping), class = "uneval")
}


guide_colorbar <- function (title = waiver(), title.position = NULL, title.theme = NULL, 
    title.hjust = NULL, title.vjust = NULL, label = TRUE, label.position = NULL, 
    label.theme = NULL, label.hjust = NULL, label.vjust = NULL, 
    barwidth = NULL, barheight = NULL, nbin = 20, raster = TRUE, 
    ticks = TRUE, draw.ulim = TRUE, draw.llim = TRUE, direction = NULL, 
    default.unit = "line", reverse = FALSE, order = 0, ...) 
{
    if (!is.null(barwidth) && !is.unit(barwidth)) 
        barwidth <- unit(barwidth, default.unit)
    if (!is.null(barheight) && !is.unit(barheight)) 
        barheight <- unit(barheight, default.unit)
    structure(list(title = title, title.position = title.position, 
        title.theme = title.theme, title.hjust = title.hjust, 
        title.vjust = title.vjust, label = label, label.position = label.position, 
        label.theme = label.theme, label.hjust = label.hjust, 
        label.vjust = label.vjust, barwidth = barwidth, barheight = barheight, 
        nbin = nbin, raster = raster, ticks = ticks, draw.ulim = draw.ulim, 
        draw.llim = draw.llim, direction = direction, default.unit = default.unit, 
        reverse = reverse, order = order, available_aes = c("colour", 
            "color", "fill"), ..., name = "colorbar"), class = c("guide", 
        "colorbar"))
}


annotation_map <- function (map, ...) 
{
    stopifnot(is.data.frame(map))
    if (!is.null(map$lat)) 
        map$y <- map$lat
    if (!is.null(map$long)) 
        map$x <- map$long
    if (!is.null(map$region)) 
        map$id <- map$region
    stopifnot(all(c("x", "y", "id") %in% names(map)))
    layer(data = dummy_data(), stat = StatIdentity, geom = GeomAnnotationMap, 
        position = PositionIdentity, inherit.aes = FALSE, params = list(map = map, 
            ...))
}


geom_contour <- function (mapping = NULL, data = NULL, stat = "contour", position = "identity", 
    ..., lineend = "butt", linejoin = "round", linemitre = 1, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomContour, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(lineend = lineend, linejoin = linejoin, 
            linemitre = linemitre, na.rm = na.rm, ...))
}


theme_dark <- function (base_size = 11, base_family = "") 
{
    theme_grey(base_size = base_size, base_family = base_family) %+replace% 
        theme(panel.background = element_rect(fill = "grey50", 
            colour = NA), panel.grid.major = element_line(colour = "grey42", 
            size = 0.25), panel.grid.minor = element_line(colour = "grey42", 
            size = 0.125), axis.ticks = element_line(colour = "grey20", 
            size = 0.25), legend.key = element_rect(fill = "grey50", 
            colour = NA), strip.background = element_rect(fill = "grey15", 
            colour = NA), strip.text = element_text(colour = "grey90", 
            size = rel(0.8)), complete = TRUE)
}


element_blank <- function () 
{
    structure(list(), class = c("element_blank", "element"))
}


scale_colour_gradient2 <- function (..., low = muted("red"), mid = "white", high = muted("blue"), 
    midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("colour", "gradient2", div_gradient_pal(low, 
        mid, high, space), na.value = na.value, guide = guide, 
        ..., rescaler = mid_rescaler(mid = midpoint))
}


draw_key_label <- function (data, params, size) 
{
    grobTree(draw_key_rect(data, list()), draw_key_text(data, 
        list()))
}


GeomDotplot <- "<environment>"

ggplotGrob <- function (x) 
{
    ggplot_gtable(ggplot_build(x))
}


scale_size_continuous <- function (name = waiver(), breaks = waiver(), labels = waiver(), 
    limits = NULL, range = c(1, 6), trans = "identity", guide = "legend") 
{
    continuous_scale("size", "area", area_pal(range), name = name, 
        breaks = breaks, labels = labels, limits = limits, trans = trans, 
        guide = guide)
}


scale_fill_datetime <- function () 
{
    scale_fill_continuous(trans = "time")
}


stat_boxplot <- function (mapping = NULL, data = NULL, geom = "boxplot", position = "dodge", 
    ..., coef = 1.5, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatBoxplot, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = na.rm, 
            coef = coef, ...))
}


label_bquote <- function (rows = NULL, cols = NULL, default = label_value) 
{
    cols_quoted <- substitute(cols)
    rows_quoted <- substitute(rows)
    has_warned <- FALSE
    fun <- function(labels) {
        quoted <- resolve_labeller(rows_quoted, cols_quoted, 
            labels)
        if (is.null(quoted)) {
            return(label_value(labels))
        }
        evaluate <- function(...) {
            params <- list(...)
            if ("x" %in% find_names(quoted) && !"x" %in% names(params)) {
                if (!has_warned) {
                  warning("Referring to `x` is deprecated, use variable name instead", 
                    call. = FALSE)
                  has_warned <<- TRUE
                }
                params$x <- params[[1]]
            }
            eval(substitute(bquote(expr, params), list(expr = quoted)))
        }
        list(do.call("Map", c(list(f = evaluate), labels)))
    }
    structure(fun, class = "labeller")
}


fortify <- function (model, data, ...) 
UseMethod("fortify")


aes <- function (x, y, ...) 
{
    aes <- structure(as.list(match.call()[-1]), class = "uneval")
    rename_aes(aes)
}


geom_pointrange <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., fatten = 4, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPointrange, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(fatten = fatten, na.rm = na.rm, ...))
}


ScaleDiscrete <- "<environment>"

StatBin2d <- "<environment>"

stat_summary <- function (mapping = NULL, data = NULL, geom = "pointrange", position = "identity", 
    ..., fun.data = NULL, fun.y = NULL, fun.ymax = NULL, fun.ymin = NULL, 
    fun.args = list(), na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatSummary, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(fun.data = fun.data, 
            fun.y = fun.y, fun.ymax = fun.ymax, fun.ymin = fun.ymin, 
            fun.args = fun.args, na.rm = na.rm, ...))
}


stat_smooth <- function (mapping = NULL, data = NULL, geom = "smooth", position = "identity", 
    ..., method = "auto", formula = y ~ x, se = TRUE, n = 80, 
    span = 0.75, fullrange = FALSE, level = 0.95, method.args = list(), 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatSmooth, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(method = method, 
            formula = formula, se = se, n = n, fullrange = fullrange, 
            level = level, na.rm = na.rm, method.args = method.args, 
            span = span, ...))
}


scale_colour_distiller <- function (..., type = "seq", palette = 1, direction = -1, values = NULL, 
    space = "Lab", na.value = "grey50", guide = "colourbar") 
{
    type <- match.arg(type, c("seq", "div", "qual"))
    if (type == "qual") {
        warning("Using a discrete colour palette in a continuous scale.\n  Consider using type = \"seq\" or type = \"div\" instead", 
            call. = FALSE)
    }
    continuous_scale("colour", "distiller", gradient_n_pal(brewer_pal(type, 
        palette, direction)(6), values, space), na.value = na.value, 
        guide = guide, ...)
}


geom_spoke <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, geom = GeomSpoke, stat = stat, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


FacetNull <- "<environment>"

GeomSpoke <- "<environment>"

label_value <- function (labels, multi_line = TRUE) 
{
    labels <- lapply(labels, as.character)
    if (multi_line) {
        labels
    }
    else {
        collapse_labels_lines(labels)
    }
}


Facet <- "<environment>"

geom_freqpoly <- function (mapping = NULL, data = NULL, stat = "bin", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    params <- list(na.rm = na.rm, ...)
    if (identical(stat, "bin")) {
        params$pad <- TRUE
    }
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPath, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = params)
}


geom_bin2d <- function (mapping = NULL, data = NULL, stat = "bin2d", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomTile, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


ScaleContinuousDate <- "<environment>"

draw_key_abline <- function (data, params, size) 
{
    segmentsGrob(0, 0, 1, 1, gp = gpar(col = alpha(data$colour, 
        data$alpha), lwd = data$size * .pt, lty = data$linetype, 
        lineend = "butt"))
}


GeomDensity2d <- "<environment>"

facet_grid <- function (facets, margins = FALSE, scales = "fixed", space = "fixed", 
    shrink = TRUE, labeller = "label_value", as.table = TRUE, 
    switch = NULL, drop = TRUE) 
{
    scales <- match.arg(scales, c("fixed", "free_x", "free_y", 
        "free"))
    free <- list(x = any(scales %in% c("free_x", "free")), y = any(scales %in% 
        c("free_y", "free")))
    space <- match.arg(space, c("fixed", "free_x", "free_y", 
        "free"))
    space_free <- list(x = any(space %in% c("free_x", "free")), 
        y = any(space %in% c("free_y", "free")))
    if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
        stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
    }
    if (is.character(facets)) {
        facets <- stats::as.formula(facets)
    }
    if (is.formula(facets)) {
        lhs <- function(x) if (length(x) == 2) 
            NULL
        else x[-3]
        rhs <- function(x) if (length(x) == 2) 
            x
        else x[-2]
        rows <- as.quoted(lhs(facets))
        rows <- rows[!sapply(rows, identical, as.name("."))]
        cols <- as.quoted(rhs(facets))
        cols <- cols[!sapply(cols, identical, as.name("."))]
    }
    if (is.list(facets)) {
        rows <- as.quoted(facets[[1]])
        cols <- as.quoted(facets[[2]])
    }
    if (length(rows) + length(cols) == 0) {
        stop("Must specify at least one variable to facet by", 
            call. = FALSE)
    }
    labeller <- check_labeller(labeller)
    ggproto(NULL, FacetGrid, shrink = shrink, params = list(rows = rows, 
        cols = cols, margins = margins, free = free, space_free = space_free, 
        labeller = labeller, as.table = as.table, switch = switch, 
        drop = drop))
}


StatEcdf <- "<environment>"

scale_y_discrete <- function (..., expand = waiver(), position = "left") 
{
    sc <- discrete_scale(c("y", "ymin", "ymax", "yend"), "position_d", 
        identity, ..., expand = expand, guide = "none", position = position, 
        super = ScaleDiscretePosition)
    sc$range_c <- continuous_range()
    sc
}


discrete_scale <- function (aesthetics, scale_name, palette, name = waiver(), breaks = waiver(), 
    labels = waiver(), limits = NULL, expand = waiver(), na.translate = TRUE, 
    na.value = NA, drop = TRUE, guide = "legend", position = "left", 
    super = ScaleDiscrete) 
{
    check_breaks_labels(breaks, labels)
    position <- match.arg(position, c("left", "right", "top", 
        "bottom"))
    if (is.null(breaks) && !is_position_aes(aesthetics) && guide != 
        "none") {
        guide <- "none"
    }
    ggproto(NULL, super, call = match.call(), aesthetics = aesthetics, 
        scale_name = scale_name, palette = palette, range = discrete_range(), 
        limits = limits, na.value = na.value, na.translate = na.translate, 
        expand = expand, name = name, breaks = breaks, labels = labels, 
        drop = drop, guide = guide, position = position)
}


stat_ydensity <- function (mapping = NULL, data = NULL, geom = "violin", position = "dodge", 
    ..., bw = "nrd0", adjust = 1, kernel = "gaussian", trim = TRUE, 
    scale = "area", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    scale <- match.arg(scale, c("area", "count", "width"))
    layer(data = data, mapping = mapping, stat = StatYdensity, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(bw = bw, adjust = adjust, 
            kernel = kernel, trim = trim, scale = scale, na.rm = na.rm, 
            ...))
}


scale_radius <- function (name = waiver(), breaks = waiver(), labels = waiver(), 
    limits = NULL, range = c(1, 6), trans = "identity", guide = "legend") 
{
    continuous_scale("size", "radius", rescale_pal(range), name = name, 
        breaks = breaks, labels = labels, limits = limits, trans = trans, 
        guide = guide)
}


scale_x_datetime <- function (name = waiver(), breaks = waiver(), date_breaks = waiver(), 
    labels = waiver(), date_labels = waiver(), minor_breaks = waiver(), 
    date_minor_breaks = waiver(), timezone = NULL, limits = NULL, 
    expand = waiver(), position = "bottom") 
{
    scale_datetime(c("x", "xmin", "xmax", "xend"), "time", name = name, 
        breaks = breaks, date_breaks = date_breaks, labels = labels, 
        date_labels = date_labels, minor_breaks = minor_breaks, 
        date_minor_breaks = date_minor_breaks, timezone = timezone, 
        limits = limits, expand = expand, position = position)
}


scale_fill_gradient <- function (..., low = "#132B43", high = "#56B1F7", space = "Lab", 
    na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("fill", "gradient", seq_gradient_pal(low, 
        high, space), na.value = na.value, guide = guide, ...)
}


stat_ecdf <- function (mapping = NULL, data = NULL, geom = "step", position = "identity", 
    ..., n = NULL, pad = TRUE, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatEcdf, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(n = n, pad = pad, na.rm = na.rm, ...))
}


theme_linedraw <- function (base_size = 11, base_family = "") 
{
    theme_bw(base_size = base_size, base_family = base_family) %+replace% 
        theme(axis.text = element_text(colour = "black", size = rel(0.8)), 
            axis.ticks = element_line(colour = "black", size = 0.25), 
            panel.border = element_rect(fill = NA, colour = "black", 
                size = 0.5), panel.grid.major = element_line(colour = "black", 
                size = 0.05), panel.grid.minor = element_line(colour = "black", 
                size = 0.025), strip.background = element_rect(fill = "black"), 
            strip.text = element_text(colour = "white", size = rel(0.8)), 
            complete = TRUE)
}


resolution <- function (x, zero = TRUE) 
{
    if (is.integer(x) || zero_range(range(x, na.rm = TRUE))) 
        return(1)
    x <- unique(as.numeric(x))
    if (zero) {
        x <- unique(c(0, x))
    }
    min(diff(sort(x)))
}


GeomRug <- "<environment>"

max_width <- function (grobs) 
{
    unit(max(unlist(lapply(grobs, width_cm))), "cm")
}


CoordQuickmap <- "<environment>"

draw_key_smooth <- function (data, params, size) 
{
    data$fill <- alpha(data$fill, data$alpha)
    data$alpha <- 1
    grobTree(if (isTRUE(params$se)) 
        rectGrob(gp = gpar(col = NA, fill = data$fill)), draw_key_path(data, 
        params))
}


position_dodge <- function (width = NULL) 
{
    ggproto(NULL, PositionDodge, width = width)
}


annotation_custom <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) 
{
    layer(data = dummy_data(), stat = StatIdentity, position = PositionIdentity, 
        geom = GeomCustomAnn, inherit.aes = FALSE, params = list(grob = grob, 
            xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax))
}


draw_key_point <- function (data, params, size) 
{
    pointsGrob(0.5, 0.5, pch = data$shape, gp = gpar(col = alpha(data$colour, 
        data$alpha), fill = alpha(data$fill, data$alpha), fontsize = data$size * 
        .pt + data$stroke * .stroke/2, lwd = data$stroke * .stroke/2))
}


GeomViolin <- "<environment>"

ggproto_parent <- function (parent, self) 
{
    structure(list(parent = parent, self = self), class = "ggproto_parent")
}


GeomPath <- "<environment>"

CoordPolar <- "<environment>"

mean_se <- function (x, mult = 1) 
{
    x <- stats::na.omit(x)
    se <- mult * sqrt(stats::var(x)/length(x))
    mean <- mean(x)
    data.frame(y = mean, ymin = mean - se, ymax = mean + se)
}


scale_colour_identity <- function (..., guide = "none") 
{
    sc <- discrete_scale("colour", "identity", identity_pal(), 
        ..., guide = guide, super = ScaleDiscreteIdentity)
    sc
}


geom_blank <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomBlank, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(...), check.aes = FALSE)
}


Stat <- "<environment>"

stat_function <- function (mapping = NULL, data = NULL, geom = "path", position = "identity", 
    ..., fun, xlim = NULL, n = 101, args = list(), na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatFunction, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(fun = fun, n = n, 
            args = args, na.rm = na.rm, xlim = xlim, ...))
}


label_parsed <- function (labels, multi_line = TRUE) 
{
    labels <- label_value(labels, multi_line = multi_line)
    if (multi_line) {
        lapply(unname(labels), lapply, function(values) {
            c(parse(text = as.character(values)))
        })
    }
    else {
        lapply(labels, function(values) {
            values <- paste0("list(", values, ")")
            lapply(values, function(expr) c(parse(text = expr)))
        })
    }
}


stat_contour <- function (mapping = NULL, data = NULL, geom = "contour", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatContour, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = na.rm, 
            ...))
}


coord_map <- function (projection = "mercator", ..., parameters = NULL, orientation = NULL, 
    xlim = NULL, ylim = NULL) 
{
    if (is.null(parameters)) {
        params <- list(...)
    }
    else {
        params <- parameters
    }
    ggproto(NULL, CoordMap, projection = projection, orientation = orientation, 
        limits = list(x = xlim, y = ylim), params = params)
}


GeomVline <- "<environment>"

GeomRaster <- "<environment>"

position_fill <- function (vjust = 1, reverse = FALSE) 
{
    ggproto(NULL, PositionFill, vjust = vjust, reverse = reverse)
}


GeomBoxplot <- "<environment>"

transform_position <- function (df, trans_x = NULL, trans_y = NULL, ...) 
{
    scales <- aes_to_scale(names(df))
    if (!is.null(trans_x)) {
        df[scales == "x"] <- lapply(df[scales == "x"], trans_x, 
            ...)
    }
    if (!is.null(trans_y)) {
        df[scales == "y"] <- lapply(df[scales == "y"], trans_y, 
            ...)
    }
    df
}


ScaleContinuousPosition <- "<environment>"

scale_colour_gradient <- function (..., low = "#132B43", high = "#56B1F7", space = "Lab", 
    na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("colour", "gradient", seq_gradient_pal(low, 
        high, space), na.value = na.value, guide = guide, ...)
}


scale_x_log10 <- function (...) 
{
    scale_x_continuous(..., trans = log10_trans())
}


geom_bar <- function (mapping = NULL, data = NULL, stat = "count", position = "stack", 
    ..., width = NULL, binwidth = NULL, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    if (!is.null(binwidth)) {
        warning("`geom_bar()` no longer has a `binwidth` parameter. ", 
            "Please use `geom_histogram()` instead.", call. = "FALSE")
        return(geom_histogram(mapping = mapping, data = data, 
            position = position, width = width, binwidth = binwidth, 
            ..., na.rm = na.rm, show.legend = show.legend, inherit.aes = inherit.aes))
    }
    layer(data = data, mapping = mapping, stat = stat, geom = GeomBar, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(width = width, na.rm = na.rm, ...))
}


coord_cartesian <- function (xlim = NULL, ylim = NULL, expand = TRUE) 
{
    ggproto(NULL, CoordCartesian, limits = list(x = xlim, y = ylim), 
        expand = expand)
}


scale_size_manual <- function (..., values) 
{
    manual_scale("size", values, ...)
}


GeomCrossbar <- "<environment>"

coord_equal <- function (ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE) 
{
    ggproto(NULL, CoordFixed, limits = list(x = xlim, y = ylim), 
        ratio = ratio, expand = expand)
}


GeomLogticks <- "<environment>"

StatSummary2d <- "<environment>"

scale_color_manual <- function (..., values) 
{
    manual_scale("colour", values, ...)
}


`%+%` <- function (e1, e2) 
{
    e2name <- deparse(substitute(e2))
    if (is.theme(e1)) 
        add_theme(e1, e2, e2name)
    else if (is.ggplot(e1)) 
        add_ggplot(e1, e2, e2name)
}


scale_fill_manual <- function (..., values) 
{
    manual_scale("fill", values, ...)
}


ylab <- function (label) 
{
    labs(y = label)
}


scale_alpha_manual <- function (..., values) 
{
    manual_scale("alpha", values, ...)
}


PositionFill <- "<environment>"

StatQuantile <- "<environment>"

scale_color_hue <- function (..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0, 
    direction = 1, na.value = "grey50") 
{
    discrete_scale("colour", "hue", hue_pal(h, c, l, h.start, 
        direction), na.value = na.value, ...)
}


unit <- grid::unit # re-exported from grid package

GeomLinerange <- "<environment>"

scale_size_date <- function () 
{
    scale_size_continuous(trans = "date")
}


scale_fill_grey <- function (..., start = 0.2, end = 0.8, na.value = "red") 
{
    discrete_scale("fill", "grey", grey_pal(start, end), na.value = na.value, 
        ...)
}


xlab <- function (label) 
{
    labs(x = label)
}


draw_key_text <- function (data, params, size) 
{
    textGrob("a", 0.5, 0.5, rot = data$angle, gp = gpar(col = alpha(data$colour, 
        data$alpha), fontfamily = data$family, fontface = data$fontface, 
        fontsize = data$size * .pt))
}


GeomStep <- "<environment>"

scale_fill_gradientn <- function (..., colours, values = NULL, space = "Lab", na.value = "grey50", 
    guide = "colourbar", colors) 
{
    colours <- if (missing(colours)) 
        colors
    else colours
    continuous_scale("fill", "gradientn", gradient_n_pal(colours, 
        values, space), na.value = na.value, guide = guide, ...)
}


GeomBar <- "<environment>"

scale_size_identity <- function (..., guide = "none") 
{
    sc <- continuous_scale("size", "identity", identity_pal(), 
        ..., guide = guide, super = ScaleContinuousIdentity)
    sc
}


ggproto <- function (`_class` = NULL, `_inherit` = NULL, ...) 
{
    e <- new.env(parent = emptyenv())
    members <- list(...)
    if (length(members) != sum(nzchar(names(members)))) {
        stop("All members of a ggproto object must be named.")
    }
    if (length(members) > 0) {
        list2env(members, envir = e)
    }
    `_inherit` <- substitute(`_inherit`)
    env <- parent.frame()
    find_super <- function() {
        eval(`_inherit`, env, NULL)
    }
    super <- find_super()
    if (!is.null(super)) {
        if (!is.ggproto(super)) {
            stop("`_inherit` must be a ggproto object.")
        }
        e$super <- find_super
        class(e) <- c(`_class`, class(super))
    }
    else {
        class(e) <- c(`_class`, "ggproto")
    }
    e
}


geom_jitter <- function (mapping = NULL, data = NULL, stat = "identity", position = "jitter", 
    ..., width = NULL, height = NULL, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    if (!missing(width) || !missing(height)) {
        if (!missing(position)) {
            stop("Specify either `position` or `width`/`height`", 
                call. = FALSE)
        }
        position <- position_jitter(width = width, height = height)
    }
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPoint, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


geom_step <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    direction = "hv", na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, 
    ...) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomStep, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(direction = direction, na.rm = na.rm, ...))
}


GeomCol <- "<environment>"

is.ggplot <- function (x) 
inherits(x, "ggplot")


guides <- function (...) 
{
    args <- list(...)
    if (is.list(args[[1]]) && !inherits(args[[1]], "guide")) 
        args <- args[[1]]
    args <- rename_aes(args)
    structure(args, class = "guides")
}


geom_polygon <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPolygon, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


zeroGrob <- function () 
.zeroGrob


stat_sum <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatSum, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


geom_segment <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., arrow = NULL, lineend = "butt", na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSegment, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(arrow = arrow, lineend = lineend, na.rm = na.rm, 
            ...))
}


arrow <- grid::arrow # re-exported from grid package

PositionJitterdodge <- "<environment>"

StatSmooth <- "<environment>"

geom_smooth <- function (mapping = NULL, data = NULL, stat = "smooth", position = "identity", 
    ..., method = "auto", formula = y ~ x, se = TRUE, na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) 
{
    params <- list(na.rm = na.rm, ...)
    if (identical(stat, "smooth")) {
        params$method <- method
        params$formula <- formula
        params$se <- se
    }
    layer(data = data, mapping = mapping, stat = stat, geom = GeomSmooth, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = params)
}


GeomDensity <- "<environment>"

coord_polar <- function (theta = "x", start = 0, direction = 1) 
{
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") 
        "y"
    else "x"
    ggproto(NULL, CoordPolar, theta = theta, r = r, start = start, 
        direction = sign(direction))
}


ggtitle <- function (label, subtitle = NULL) 
{
    labs(title = label, subtitle = subtitle)
}


StatContour <- "<environment>"

coord_munch <- function (coord, data, range, segment_length = 0.01) 
{
    if (coord$is_linear()) 
        return(coord$transform(data, range))
    ranges <- coord$range(range)
    data$x[data$x == -Inf] <- ranges$x[1]
    data$x[data$x == Inf] <- ranges$x[2]
    data$y[data$y == -Inf] <- ranges$y[1]
    data$y[data$y == Inf] <- ranges$y[2]
    dist <- coord$distance(data$x, data$y, range)
    dist[data$group[-1] != data$group[-nrow(data)]] <- NA
    munched <- munch_data(data, dist, segment_length)
    coord$transform(munched, range)
}


cut_width <- function (x, width, center = NULL, boundary = NULL, closed = c("right", 
    "left")) 
{
    x <- as.numeric(x)
    width <- as.numeric(width)
    closed <- match.arg(closed)
    x_range <- range(x, na.rm = TRUE, finite = TRUE)
    if (length(x_range) == 0) {
        return(x)
    }
    if (!is.null(boundary) && !is.null(center)) {
        stop("Only one of 'boundary' and 'center' may be specified.")
    }
    if (is.null(boundary)) {
        if (is.null(center)) {
            boundary <- width/2
        }
        else {
            boundary <- center - width/2
        }
    }
    boundary <- as.numeric(boundary)
    min_x <- find_origin(x_range, width, boundary)
    max_x <- max(x, na.rm = TRUE) + (1 - 1e-08) * width
    breaks <- seq(min_x, max_x, width)
    cut(x, breaks, include.lowest = TRUE, right = (closed == 
        "right"))
}


FacetWrap <- "<environment>"

scale_colour_gradientn <- function (..., colours, values = NULL, space = "Lab", na.value = "grey50", 
    guide = "colourbar", colors) 
{
    colours <- if (missing(colours)) 
        colors
    else colours
    continuous_scale("colour", "gradientn", gradient_n_pal(colours, 
        values, space), na.value = na.value, guide = guide, ...)
}


`%+replace%` <- function (e1, e2) 
{
    if (!is.theme(e1) || !is.theme(e2)) {
        stop("%+replace% requires two theme objects", call. = FALSE)
    }
    e1[names(e2)] <- e2
    e1
}


position_identity <- function () 
{
    PositionIdentity
}


scale_fill_continuous <- function (..., low = "#132B43", high = "#56B1F7", space = "Lab", 
    na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("fill", "gradient", seq_gradient_pal(low, 
        high, space), na.value = na.value, guide = guide, ...)
}


benchplot <- function (x) 
{
    construct <- system.time(force(x))
    stopifnot(inherits(x, "ggplot"))
    build <- system.time(data <- ggplot_build(x))
    render <- system.time(grob <- ggplot_gtable(data))
    draw <- system.time(grid.draw(grob))
    times <- rbind(construct, build, render, draw)[, 1:3]
    plyr::unrowname(data.frame(step = c("construct", "build", 
        "render", "draw", "TOTAL"), rbind(times, colSums(times))))
}


facet_null <- function (shrink = TRUE) 
{
    ggproto(NULL, FacetNull, shrink = shrink)
}


GeomRasterAnn <- "<environment>"

GeomLine <- "<environment>"

scale_size_area <- function (..., max_size = 6) 
{
    continuous_scale("size", "area", palette = abs_area(max_size), 
        rescaler = rescale_max, ...)
}


scale_color_gradient2 <- function (..., low = muted("red"), mid = "white", high = muted("blue"), 
    midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("colour", "gradient2", div_gradient_pal(low, 
        mid, high, space), na.value = na.value, guide = guide, 
        ..., rescaler = mid_rescaler(mid = midpoint))
}


layer <- function (geom = NULL, stat = NULL, data = NULL, mapping = NULL, 
    position = NULL, params = list(), inherit.aes = TRUE, check.aes = TRUE, 
    check.param = TRUE, subset = NULL, show.legend = NA) 
{
    if (is.null(geom)) 
        stop("Attempted to create layer with no geom.", call. = FALSE)
    if (is.null(stat)) 
        stop("Attempted to create layer with no stat.", call. = FALSE)
    if (is.null(position)) 
        stop("Attempted to create layer with no position.", call. = FALSE)
    if (!is.null(params$show_guide)) {
        warning("`show_guide` has been deprecated. Please use `show.legend` instead.", 
            call. = FALSE)
        show.legend <- params$show_guide
        params$show_guide <- NULL
    }
    if (!is.logical(show.legend) || length(show.legend) != 1) {
        warning("`show.legend` must be a logical vector of length 1.", 
            call. = FALSE)
        show.legend <- FALSE
    }
    data <- fortify(data)
    if (!is.null(mapping) && !inherits(mapping, "uneval")) {
        stop("Mapping must be created by `aes()` or `aes_()`", 
            call. = FALSE)
    }
    if (is.character(geom)) 
        geom <- find_subclass("Geom", geom, parent.frame())
    if (is.character(stat)) 
        stat <- find_subclass("Stat", stat, parent.frame())
    if (is.character(position)) 
        position <- find_subclass("Position", position, parent.frame())
    if (is.null(params$na.rm)) {
        params$na.rm <- FALSE
    }
    params <- rename_aes(params)
    aes_params <- params[intersect(names(params), geom$aesthetics())]
    geom_params <- params[intersect(names(params), geom$parameters(TRUE))]
    stat_params <- params[intersect(names(params), stat$parameters(TRUE))]
    all <- c(geom$parameters(TRUE), stat$parameters(TRUE), geom$aesthetics())
    extra_param <- setdiff(names(params), all)
    if (check.param && length(extra_param) > 0) {
        warning("Ignoring unknown parameters: ", paste(extra_param, 
            collapse = ", "), call. = FALSE, immediate. = TRUE)
    }
    extra_aes <- setdiff(names(mapping), c(geom$aesthetics(), 
        stat$aesthetics()))
    if (check.aes && length(extra_aes) > 0) {
        warning("Ignoring unknown aesthetics: ", paste(extra_aes, 
            collapse = ", "), call. = FALSE, immediate. = TRUE)
    }
    ggproto("LayerInstance", Layer, geom = geom, geom_params = geom_params, 
        stat = stat, stat_params = stat_params, data = data, 
        mapping = mapping, aes_params = aes_params, subset = subset, 
        position = position, inherit.aes = inherit.aes, show.legend = show.legend)
}


GeomRect <- "<environment>"

GeomSegment <- "<environment>"

stat_qq <- function (mapping = NULL, data = NULL, geom = "point", position = "identity", 
    ..., distribution = stats::qnorm, dparams = list(), na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatQq, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(distribution = distribution, dparams = dparams, 
            na.rm = na.rm, ...))
}


geom_path <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., lineend = "butt", linejoin = "round", linemitre = 1, 
    arrow = NULL, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomPath, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(lineend = lineend, linejoin = linejoin, 
            linemitre = linemitre, arrow = arrow, na.rm = na.rm, 
            ...))
}


stat_density <- function (mapping = NULL, data = NULL, geom = "area", position = "stack", 
    ..., bw = "nrd0", adjust = 1, kernel = "gaussian", n = 512, 
    trim = FALSE, na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatDensity, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(bw = bw, adjust = adjust, 
            kernel = kernel, n = n, trim = trim, na.rm = na.rm, 
            ...))
}


panel_cols <- function (table) 
{
    panels <- table$layout[grepl("^panel", table$layout$name), 
        , drop = FALSE]
    unique(panels[, c("l", "r")])
}


StatSum <- "<environment>"

scale_fill_gradient2 <- function (..., low = muted("red"), mid = "white", high = muted("blue"), 
    midpoint = 0, space = "Lab", na.value = "grey50", guide = "colourbar") 
{
    continuous_scale("fill", "gradient2", div_gradient_pal(low, 
        mid, high, space), na.value = na.value, guide = guide, 
        ..., rescaler = mid_rescaler(mid = midpoint))
}


geom_raster <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., hjust = 0.5, vjust = 0.5, interpolate = FALSE, na.rm = FALSE, 
    show.legend = NA, inherit.aes = TRUE) 
{
    stopifnot(is.numeric(hjust), length(hjust) == 1)
    stopifnot(is.numeric(vjust), length(vjust) == 1)
    layer(data = data, mapping = mapping, stat = stat, geom = GeomRaster, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(hjust = hjust, vjust = vjust, interpolate = interpolate, 
            na.rm = na.rm, ...))
}


FacetGrid <- "<environment>"

draw_key_dotplot <- function (data, params, size) 
{
    pointsGrob(0.5, 0.5, size = unit(0.5, "npc"), pch = 21, gp = gpar(col = alpha(data$colour, 
        data$alpha), fill = alpha(data$fill, data$alpha)))
}


GeomCustomAnn <- "<environment>"

scale_shape_manual <- function (..., values) 
{
    manual_scale("shape", values, ...)
}


scale_size <- function (name = waiver(), breaks = waiver(), labels = waiver(), 
    limits = NULL, range = c(1, 6), trans = "identity", guide = "legend") 
{
    continuous_scale("size", "area", area_pal(range), name = name, 
        breaks = breaks, labels = labels, limits = limits, trans = trans, 
        guide = guide)
}


quickplot <- function (x, y = NULL, ..., data, facets = NULL, margins = FALSE, 
    geom = "auto", xlim = c(NA, NA), ylim = c(NA, NA), log = "", 
    main = NULL, xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), 
    asp = NA, stat = NULL, position = NULL) 
{
    if (!missing(stat)) 
        warning("`stat` is deprecated", call. = FALSE)
    if (!missing(position)) 
        warning("`position` is deprecated", call. = FALSE)
    if (!is.character(geom)) 
        stop("`geom` must be a character vector", call. = FALSE)
    argnames <- names(as.list(match.call(expand.dots = FALSE)[-1]))
    arguments <- as.list(match.call()[-1])
    env <- parent.frame()
    aesthetics <- compact(arguments[.all_aesthetics])
    aesthetics <- aesthetics[!is.constant(aesthetics)]
    aes_names <- names(aesthetics)
    aesthetics <- rename_aes(aesthetics)
    class(aesthetics) <- "uneval"
    if (missing(data)) {
        data <- data.frame()
        facetvars <- all.vars(facets)
        facetvars <- facetvars[facetvars != "."]
        names(facetvars) <- facetvars
        facetsdf <- as.data.frame(mget(facetvars, envir = env))
        if (nrow(facetsdf)) 
            data <- facetsdf
    }
    if ("auto" %in% geom) {
        if ("sample" %in% aes_names) {
            geom[geom == "auto"] <- "qq"
        }
        else if (missing(y)) {
            x <- eval(aesthetics$x, data, env)
            if (is.discrete(x)) {
                geom[geom == "auto"] <- "bar"
            }
            else {
                geom[geom == "auto"] <- "histogram"
            }
            if (missing(ylab)) 
                ylab <- "count"
        }
        else {
            if (missing(x)) {
                aesthetics$x <- bquote(seq_along(.(y)), aesthetics)
            }
            geom[geom == "auto"] <- "point"
        }
    }
    p <- ggplot(data, aesthetics, environment = env)
    if (is.null(facets)) {
        p <- p + facet_null()
    }
    else if (is.formula(facets) && length(facets) == 2) {
        p <- p + facet_wrap(facets)
    }
    else {
        p <- p + facet_grid(facets = deparse(facets), margins = margins)
    }
    if (!is.null(main)) 
        p <- p + ggtitle(main)
    for (g in geom) {
        params <- arguments[setdiff(names(arguments), c(aes_names, 
            argnames))]
        params <- lapply(params, eval, parent.frame())
        p <- p + do.call(paste0("geom_", g), params)
    }
    logv <- function(var) var %in% strsplit(log, "")[[1]]
    if (logv("x")) 
        p <- p + scale_x_log10()
    if (logv("y")) 
        p <- p + scale_y_log10()
    if (!is.na(asp)) 
        p <- p + theme(aspect.ratio = asp)
    if (!missing(xlab)) 
        p <- p + xlab(xlab)
    if (!missing(ylab)) 
        p <- p + ylab(ylab)
    if (!missing(xlim)) 
        p <- p + xlim(xlim)
    if (!missing(ylim)) 
        p <- p + ylim(ylim)
    p
}


stat_density2d <- function (mapping = NULL, data = NULL, geom = "density_2d", position = "identity", 
    ..., contour = TRUE, n = 100, h = NULL, na.rm = FALSE, show.legend = NA, 
    inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = StatDensity2d, 
        geom = geom, position = position, show.legend = show.legend, 
        inherit.aes = inherit.aes, params = list(na.rm = na.rm, 
            contour = contour, n = n, h = h, ...))
}


StatCount <- "<environment>"

geom_quantile <- function (mapping = NULL, data = NULL, stat = "quantile", position = "identity", 
    ..., lineend = "butt", linejoin = "round", linemitre = 1, 
    na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomQuantile, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(lineend = lineend, linejoin = linejoin, 
            linemitre = linemitre, na.rm = na.rm, ...))
}


aes_ <- function (x, y, ...) 
{
    mapping <- list(...)
    if (!missing(x)) 
        mapping["x"] <- list(x)
    if (!missing(y)) 
        mapping["y"] <- list(y)
    as_call <- function(x) {
        if (is.formula(x) && length(x) == 2) {
            x[[2]]
        }
        else if (is.call(x) || is.name(x) || is.atomic(x)) {
            x
        }
        else {
            stop("Aesthetic must be a one-sided formula, call, name, or constant.", 
                call. = FALSE)
        }
    }
    mapping <- lapply(mapping, as_call)
    structure(rename_aes(mapping), class = "uneval")
}


scale_shape_discrete <- function (..., solid = TRUE) 
{
    discrete_scale("shape", "shape_d", shape_pal(solid), ...)
}


geom_ribbon <- function (mapping = NULL, data = NULL, stat = "identity", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = GeomRibbon, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


is.theme <- function (x) 
inherits(x, "theme")




## Package Data

diamonds <- ggplot2::diamonds		## Prices of 50,000 round cut diamonds

economics <- ggplot2::economics		## US economic time series

economics_long <- ggplot2::economics_long		## US economic time series

faithfuld <- ggplot2::faithfuld		## 2d density estimate of Old Faithful data

luv_colours <- ggplot2::luv_colours		## 'colors()' in Luv space

midwest <- ggplot2::midwest		## Midwest demographics

mpg <- ggplot2::mpg		## Fuel economy data from 1999 and 2008 for 38 popular models of car

msleep <- ggplot2::msleep		## An updated and expanded version of the mammals sleep dataset

presidential <- ggplot2::presidential		## Terms of 11 presidents from Eisenhower to Obama

seals <- ggplot2::seals		## Vector field of seal movements

txhousing <- ggplot2::txhousing		## Housing sales in TX



## Package Info

.skeleton_package_title = "Create Elegant Data Visualisations Using the Grammar of Graphics"

.skeleton_package_version = "2.2.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "digest,grid,gtable,MASS,plyr,reshape2,scales,stats,tibble,lazyeval"


## Internal

.skeleton_version = 5


## EOF