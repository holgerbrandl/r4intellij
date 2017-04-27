##
## Exported symobls in package `highcharter`
##

## Exported package methods

hc_theme_chalk <- function (...) 
{
    cols <- colorRampPalette(c("#FFFFFF", "#8C8984"))(4)
    theme <- list(colors = cols, chart = list(divBackgroundImage = "https://www.amcharts.com/inspiration/chalk/bg.jpg", 
        backgroundColor = "transparent", style = list(fontFamily = "Shadows Into Light", 
            color = "#FFFFFF")), plotOptions = list(scatter = list(marker = list(radius = 10))), 
        title = list(style = list(fontSize = "30px", color = "#FFFFFF")), 
        subtitle = list(style = list(fontSize = "20px", color = "#FFFFFF")), 
        legend = list(enabled = TRUE, itemStyle = list(fontSize = "20px", 
            color = "#FFFFFF")), credits = list(enabled = FALSE), 
        xAxis = list(lineWidth = 1, tickWidth = 1, gridLineColor = "transparent", 
            labels = list(enabled = TRUE, style = list(color = "#FFFFFF", 
                fontSize = "20px")), title = list(enabled = TRUE, 
                style = list(color = "#FFFFFF", fontSize = "20px"))), 
        yAxis = list(lineWidth = 1, tickWidth = 1, gridLineColor = "transparent", 
            labels = list(enabled = TRUE, style = list(color = "#FFFFFF", 
                fontSize = "20px")), title = list(enabled = TRUE, 
                style = list(color = "#FFFFFF", fontSize = "20px"))), 
        tooltip = list(backgroundColor = "#333333", style = list(color = "#FFFFFF", 
            fontSize = "20px", padding = "10px")))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_add_annotations <- function (hc, x) 
{
    assertthat::assert_that(is.highchart(hc), (is.list(x) | is.data.frame(x)))
    if (is.data.frame(x)) 
        x <- list_parse(x)
    hc$x$hc_opts[["annotations"]] <- append(hc$x$hc_opts[["annotations"]], 
        x)
    hc
}


hc_scrollbar <- function (hc, ...) 
{
    .hc_opt(hc, "scrollbar", ...)
}


hc_pane <- function (hc, ...) 
{
    .hc_opt(hc, "pane", ...)
}


hc_colorAxis <- function (hc, ...) 
{
    .hc_opt(hc, "colorAxis", ...)
}


hc_add_series_boxplot <- function (hc, x, by = NULL, outliers = TRUE, ...) 
{
    .Deprecated("hcboxplot")
    if (is.null(by)) {
        by <- "value"
    }
    else {
        stopifnot(length(x) == length(by))
    }
    df <- data_frame(value = x, by = by) %>% group_by_("by") %>% 
        do(data = boxplot.stats(.$value))
    bxps <- map(df$data, "stats")
    hc <- hc %>% hc_xAxis(categories = df$by) %>% hc_add_series(data = bxps, 
        type = "boxplot", ...)
    if (outliers) {
        outs <- map2_df(seq(nrow(df)), df$data, function(x, y) {
            if (length(y$out) > 0) 
                d <- data_frame(x = x - 1, y = y$out)
            else d <- data_frame()
            d
        })
        if (nrow(outs) > 0) {
            hc <- hc %>% hc_add_series(data = list_parse(outs), 
                name = str_trim(paste(list(...)$name, "outliers")), 
                type = "scatter", marker = list(...), tooltip = list(headerFormat = "<span>{point.key}</span><br/>", 
                  pointFormat = "<span style='color:{point.color}'></span> \n            Outlier: <b>{point.y}</b><br/>"), 
                ...)
        }
    }
    hc
}


JS <- htmlwidgets::JS # re-exported from htmlwidgets package

hc_add_series_ts <- function (hc, ts, ...) 
{
    assertthat::assert_that(is.ts(ts), is.highchart(hc))
    .Deprecated("hc_add_series")
    dates <- time(ts) %>% zoo::as.Date()
    values <- as.vector(ts)
    hc %>% hc_add_series_times_values(dates, values, ...)
}


highchart <- function (hc_opts = list(), theme = getOption("highcharter.theme"), 
    type = "chart", width = NULL, height = NULL, elementId = NULL) 
{
    assertthat::assert_that(type %in% c("chart", "stock", "map"))
    opts <- .join_hc_opts()
    if (identical(hc_opts, list())) 
        hc_opts <- opts$chart
    unfonts <- unique(c(.hc_get_fonts(hc_opts), .hc_get_fonts(theme)))
    opts$chart <- NULL
    x <- list(hc_opts = hc_opts, theme = theme, conf_opts = opts, 
        type = type, fonts = unfonts, debug = getOption("highcharter.debug"))
    attr(x, "TOJSON_ARGS") <- list(pretty = getOption("highcharter.debug"))
    htmlwidgets::createWidget(name = "highchart", x, width = width, 
        height = height, package = "highcharter", elementId = elementId, 
        sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = "100%", 
            knitr.figure = FALSE, knitr.defaultWidth = "100%", 
            browser.fill = TRUE))
}


hc_motion <- function (hc, ...) 
{
    .hc_opt(hc, "motion", ...)
}


hc_rm_series <- function (hc, names = NULL) 
{
    stopifnot(!is.null(names))
    positions <- hc$x$hc_opts$series %>% map("name") %>% unlist()
    position <- which(positions %in% names)
    hc$x$hc_opts$series[position] <- NULL
    hc
}


fa_icon_mark <- function (iconname = "circle") 
{
    faicos <- readRDS(system.file("extdata/faicos.rds", package = "highcharter"))
    stopifnot(all(iconname %in% faicos$name))
    idx <- purrr::map_int(iconname, function(icn) which(faicos$name %in% 
        icn))
    cod <- faicos$code[idx]
    paste0("text:", cod)
}


hctreemap <- function (tm, ...) 
{
    assertthat::assert_that(is.list(tm))
    df <- tm$tm %>% tbl_df() %>% select_("-x0", "-y0", "-w", 
        "-h", "-stdErr", "-vColorValue") %>% rename_(value = "vSize", 
        valuecolor = "vColor") %>% purrr::map_if(is.factor, as.character) %>% 
        data.frame(stringsAsFactors = FALSE) %>% tbl_df()
    ndepth <- which(names(df) == "value") - 1
    ds <- map_df(seq(ndepth), function(lvl) {
        df2 <- df %>% filter_(sprintf("level == %s", lvl)) %>% 
            rename_(name = names(df)[lvl]) %>% mutate_(id = "highcharter::str_to_id(name)")
        if (lvl > 1) {
            df2 <- df2 %>% mutate_(parent = names(df)[lvl - 1], 
                parent = "highcharter::str_to_id(parent)")
        }
        else {
            df2 <- df2 %>% mutate_(parent = NA)
        }
        df2
    })
    ds <- list_parse(ds)
    ds <- map(ds, function(x) {
        if (is.na(x$parent)) 
            x$parent <- NULL
        x
    })
    hc_add_series(highchart(), data = ds, type = "treemap", ...)
}


colorize <- function (x, colors = c("#440154", "#21908C", "#FDE725")) 
{
    nuniques <- length(unique(x))
    palcols <- (grDevices::colorRampPalette(colors))(nuniques)
    if (!is.numeric(x) | nuniques < 10) {
        y <- as.numeric(as.factor(x))
        xcols <- palcols[y]
    }
    else {
        ecum <- ecdf(x)
        xcols <- palcols[ceiling(nuniques * ecum(x))]
    }
    xcols
}


hc_size <- function (hc, width = NULL, height = NULL) 
{
    assertthat::assert_that(is.highchart(hc))
    if (!is.null(width)) 
        hc$width <- width
    if (!is.null(height)) 
        hc$height <- height
    hc
}


download_map_data <- function (url = "custom/world.js", showinfo = FALSE) 
{
    url <- sprintf("https://code.highcharts.com/mapdata/%s", 
        fix_map_name(url))
    tmpfile <- tempfile(fileext = ".js")
    download.file(url, tmpfile)
    mapdata <- readLines(tmpfile, warn = FALSE)
    mapdata[1] <- gsub(".* = ", "", mapdata[1])
    mapdata <- paste(mapdata, collapse = "\n")
    mapdata <- jsonlite::fromJSON(mapdata, simplifyVector = FALSE)
    if (showinfo) {
        glimpse(get_data_from_map(mapdata))
    }
    mapdata
}


hc_rangeSelector <- function (hc, ...) 
{
    .hc_opt(hc, "rangeSelector", ...)
}


hc_annotations <- function (hc, ...) 
{
    .hc_opt(hc, "annotations", ...)
}


hcts <- function (x, ...) 
{
    hchart(as.ts(x), ...)
}


hc_theme_google <- function (...) 
{
    theme <- list(colors = c("#0266C8", "#F90101", "#F2B50F", 
        "#00933B"), chart = list(style = list(fontFamily = "Arial, sans-serif", 
        color = "#444444")), xAxis = list(gridLineWidth = 1, 
        gridLineColor = "#F3F3F3", lineColor = "#F3F3F3", minorGridLineColor = "#F3F3F3", 
        tickColor = "#F3F3F3", tickWidth = 1), yAxis = list(gridLineColor = "#F3F3F3", 
        lineColor = "#F3F3F3", minorGridLineColor = "#F3F3F3", 
        tickColor = "#F3F3F3", tickWidth = 1), legendBackgroundColor = "rgba(0, 0, 0, 0.5)", 
        background2 = "#505053", dataLabelsColor = "#B0B0B3", 
        textColor = "#C0C0C0", contrastTextColor = "#F0F0F3", 
        maskColor = "rgba(255,255,255,0.3)")
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_chart <- function (hc, ...) 
{
    .hc_opt(hc, "chart", ...)
}


hc_theme_merge <- function (...) 
{
    themes <- list(...)
    assert_that(unique(unlist(purrr::map(themes, class))) == 
        "hc_theme")
    theme <- structure(list.merge(...), class = "hc_theme")
    theme
}


get_hc_series_from_df <- function (data, type = NULL, ...) 
{
    assertthat::assert_that(is.data.frame(data))
    stopifnot(!is.null(type))
    pars <- eval(substitute(alist(...)))
    parsc <- map(pars, as.character)
    data <- mutate(data, ...)
    data <- ungroup(data)
    type <- ifelse(type == "point", "scatter", type)
    type <- ifelse("size" %in% names(data) & type == "scatter", 
        "bubble", type)
    if (type == "heatmap") {
        if (!is.numeric(data[["x"]])) {
            data[["xf"]] <- as.factor(data[["x"]])
            data[["x"]] <- as.numeric(as.factor(data[["x"]])) - 
                1
        }
        if (!is.numeric(data[["y"]])) {
            data[["yf"]] <- as.factor(data[["y"]])
            data[["y"]] <- as.numeric(as.factor(data[["y"]])) - 
                1
        }
    }
    if (has_name(parsc, "x")) {
        if (is.Date(data[["x"]])) {
            data[["x"]] <- datetime_to_timestamp(data[["x"]])
        }
        else if (is.character(data[["x"]]) | is.factor(data[["x"]])) {
            data[["name"]] <- data[["x"]]
            data[["x"]] <- NULL
        }
    }
    if (has_name(parsc, "color")) {
        if (type == "treemap") {
            data <- rename_(data, colorValue = "color")
        }
        else {
            data <- mutate_(data, colorv = "color", color = "highcharter::colorize(color)")
        }
    }
    if (has_name(parsc, "size") & type %in% c("bubble", "scatter")) 
        data <- mutate_(data, z = "size")
    if (!has_name(parsc, "group")) 
        data[["group"]] <- "Series"
    data[["charttpye"]] <- type
    dfs <- data %>% group_by_("group", "charttpye") %>% do(data = list_parse(select_(., 
        quote(-group), quote(-charttpye)))) %>% ungroup() %>% 
        rename_(name = "group", type = "charttpye")
    if (!has_name(parsc, "group")) 
        dfs[["name"]] <- NULL
    series <- list_parse(dfs)
    series
}


hc_add_series_scatter <- function (hc, x, y, z = NULL, color = NULL, label = NULL, showInLegend = FALSE, 
    ...) 
{
    .Deprecated("hc_add_series")
    assertthat::assert_that(is.highchart(hc), length(x) == length(y), 
        is.numeric(x), is.numeric(y))
    df <- data_frame(x, y)
    if (!is.null(z)) {
        assert_that(length(x) == length(z))
        df <- df %>% mutate(z = z)
    }
    if (!is.null(color)) {
        assert_that(length(x) == length(color))
        cols <- colorize(color)
        df <- df %>% mutate(valuecolor = color, color = cols)
    }
    if (!is.null(label)) {
        assert_that(length(x) == length(label))
        df <- df %>% mutate(label = label)
    }
    args <- list(...)
    for (i in seq_along(args)) {
        if (length(x) == length(args[[i]]) && names(args[i]) != 
            "name") {
            attr <- list(args[i])
            names(attr) <- names(args)[i]
            df <- cbind(df, attr)
            args[[i]] <- character(0)
        }
    }
    args <- Filter(length, args)
    ds <- list_parse(df)
    type <- ifelse(!is.null(z), "bubble", "scatter")
    if (!is.null(label)) {
        dlopts <- list(enabled = TRUE, format = "{point.label}")
    }
    else {
        dlopts <- list(enabled = FALSE)
    }
    do.call("hc_add_series", c(list(hc, data = ds, type = type, 
        showInLegend = showInLegend, dataLabels = dlopts), args))
}


hc_add_series_xts <- function (hc, x, ...) 
{
    .Deprecated("hc_add_series")
    assertthat::assert_that(is.highchart(hc), is.xts(x))
    hc$x$type <- "stock"
    timestamps <- datetime_to_timestamp(time(x))
    ds <- list_parse2(data.frame(timestamps, as.vector(x)))
    hc %>% hc_add_series(data = ds, ...)
}


list_parse2 <- function (df) 
{
    assertthat::assert_that(is.data.frame(df))
    list_parse(df) %>% map(setNames, NULL)
}


hc_theme_handdrawn <- function (...) 
{
    cols <- colorRampPalette(c("#171314", "#888782"))(4)
    theme <- list(colors = cols, chart = list(divBackgroundImage = "https://www.amcharts.com/inspiration/hand-drawn/bg.jpg", 
        backgroundColor = "transparent", style = list(fontFamily = "Berkshire Swash", 
            color = "#000000")), plotOptions = list(scatter = list(marker = list(radius = 10))), 
        title = list(style = list(fontSize = "30px", color = "#000000")), 
        subtitle = list(style = list(fontSize = "20px", color = "#000000")), 
        legend = list(enabled = TRUE, itemStyle = list(fontSize = "20px", 
            color = "#000000")), credits = list(enabled = FALSE), 
        xAxis = list(lineColor = "#000000", tickColor = "#000000", 
            lineWidth = 1, tickWidth = 1, gridLineColor = "transparent", 
            labels = list(enabled = TRUE, style = list(color = "#000000", 
                fontSize = "20px")), title = list(enabled = TRUE, 
                style = list(color = "#000000", fontSize = "20px"))), 
        yAxis = list(lineColor = "#000000", tickColor = "#000000", 
            lineWidth = 1, tickWidth = 1, gridLineColor = "transparent", 
            labels = list(enabled = TRUE, style = list(color = "#000000", 
                fontSize = "20px")), title = list(enabled = TRUE, 
                style = list(color = "#000000", fontSize = "20px"))), 
        tooltip = list(backgroundColor = "#C9C8C3", style = list(color = "#000000", 
            fontSize = "20px", padding = "10px")))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_plotOptions <- function (hc, ...) 
{
    .hc_opt(hc, "plotOptions", ...)
}


hc_add_series_map <- function (hc, map, df, value, joinBy, ...) 
{
    assertthat::assert_that(is.highchart(hc), is.list(map), is.data.frame(df), 
        value %in% names(df), tail(joinBy, 1) %in% names(df))
    joindf <- tail(joinBy, 1)
    ddta <- mutate_(df, value = value, code = joindf)
    ddta <- list_parse(ddta)
    hc$x$type <- "map"
    hc %>% hc_add_series(mapData = map, data = ddta, joinBy = c(joinBy[1], 
        "code"), ...) %>% hc_colorAxis(min = 0)
}


hc_add_series_treemap <- function (hc, tm, ...) 
{
    .Deprecated("hctreemap")
    assertthat::assert_that(is.highchart(hc), is.list(tm))
    df <- tm$tm %>% tbl_df() %>% select_("-x0", "-y0", "-w", 
        "-h", "-stdErr", "-vColorValue") %>% rename_(value = "vSize", 
        valuecolor = "vColor") %>% purrr::map_if(is.factor, as.character) %>% 
        data.frame(stringsAsFactors = FALSE) %>% tbl_df()
    ndepth <- which(names(df) == "value") - 1
    ds <- map_df(seq(ndepth), function(lvl) {
        df2 <- df %>% filter_(sprintf("level == %s", lvl)) %>% 
            rename_(name = names(df)[lvl]) %>% mutate_(id = "highcharter::str_to_id(name)")
        if (lvl > 1) {
            df2 <- df2 %>% mutate_(parent = names(df)[lvl - 1], 
                parent = "highcharter::str_to_id(parent)")
        }
        else {
            df2 <- df2 %>% mutate_(parent = NA)
        }
        df2
    })
    ds <- list_parse(ds)
    ds <- map(ds, function(x) {
        if (is.na(x$parent)) 
            x$parent <- NULL
        x
    })
    hc %>% hc_add_series(data = ds, type = "treemap", ...)
}


hex_to_rgba <- function (x, alpha = 1) 
{
    x %>% col2rgb() %>% t() %>% as.data.frame() %>% unite_(col = "rgba", 
        from = c("red", "green", "blue"), sep = ",") %>% .[[1]] %>% 
        sprintf("rgba(%s,%s)", ., alpha)
}


hc_theme_gridlight <- function (...) 
{
    theme <- list(colors = c("#7CB5EC", "#F7A35C", "#90EE7E", 
        "#7798BF", "#AAEEEE", "#FF0066", "#EEAAEE", "#55BF3B"), 
        chart = list(backgroundColor = NULL, style = list(fontFamily = "Dosis, sans-serif")), 
        title = list(style = list(fontSize = "16px", fontWeight = "bold", 
            textTransform = "uppercase")), tooltip = list(borderWidth = 0, 
            backgroundColor = "rgba(219,219,216,0.8)", shadow = FALSE), 
        legend = list(itemStyle = list(fontWeight = "bold", fontSize = "13px")), 
        xAxis = list(gridLineWidth = 1, labels = list(style = list(fontSize = "12px"))), 
        yAxis = list(minorTickInterval = "auto", title = list(style = list(textTransform = "uppercase")), 
            labels = list(style = list(fontSize = "12px"))), 
        plotOptions = list(candlestick = list(lineColor = "#404048")), 
        background2 = "#F0F0EA")
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


`%>%` <- igraph::`%>%` # re-exported from igraph package

color_stops <- function (n = 10, colors = c("#440154", "#21908C", "#FDE725")) 
{
    palcols <- (grDevices::colorRampPalette(colors))(n)
    list_parse2(data.frame(q = seq(0, n - 1)/(n - 1), c = palcols))
}


hc_theme_ffx <- function (...) 
{
    theme <- hc_theme(colors = c("#00AACC", "#FF4E00", "#B90000", 
        "#5F9B0A", "#CD6723"), chart = list(backgroundColor = list(linearGradient = list(0, 
        0, 0, 150), stops = list(list(0, "#CAE1F4"), list(1, 
        "#EEEEEE"))), style = list(fontFamily = "Open Sans")), 
        title = list(align = "left"), subtitle = list(align = "left"), 
        legend = list(align = "right", verticalAlign = "bottom"), 
        xAxis = list(gridLineWidth = 1, gridLineColor = "#F3F3F3", 
            lineColor = "#F3F3F3", minorGridLineColor = "#F3F3F3", 
            tickColor = "#F3F3F3", tickWidth = 1), yAxis = list(gridLineColor = "#F3F3F3", 
            lineColor = "#F3F3F3", minorGridLineColor = "#F3F3F3", 
            tickColor = "#F3F3F3", tickWidth = 1))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_tooltip <- function (hc, ..., sort = FALSE, table = FALSE) 
{
    if (sort) 
        hc <- .hc_tooltip_sort(hc)
    if (table) 
        hc <- .hc_tooltip_table(hc)
    if (length(list(...))) 
        hc <- .hc_opt(hc, "tooltip", ...)
    hc
}


random_id <- function (n = 1, length = 10) 
{
    source <- c(seq(0, 9), letters)
    replicate(n, paste0(sample(x = source, size = length, replace = TRUE), 
        collapse = ""))
}


hchart <- function (object, ...) 
{
    UseMethod("hchart")
}


hc_add_series_times_values <- function (hc, dates, values, ...) 
{
    assertthat::assert_that(is.highchart(hc), is.numeric(values), 
        is.timepoint(dates))
    timestamps <- datetime_to_timestamp(dates)
    ds <- list_parse2(data.frame(timestamps, values))
    hc %>% hc_xAxis(type = "datetime") %>% hc_add_series(marker = list(enabled = FALSE), 
        data = ds, ...)
}


hc_theme_flat <- function (...) 
{
    theme <- list(colors = c("#f1c40f", "#2ecc71", "#9b59b6", 
        "#e74c3c", "#34495e", "#3498db", "#1abc9c", "#f39c12", 
        "#d35400"), chart = list(backgroundColor = "#ECF0F1"), 
        xAxis = list(gridLineDashStyle = "Dash", gridLineWidth = 1, 
            gridLineColor = "#BDC3C7", lineColor = "#BDC3C7", 
            minorGridLineColor = "#BDC3C7", tickColor = "#BDC3C7", 
            tickWidth = 1), yAxis = list(gridLineDashStyle = "Dash", 
            gridLineColor = "#BDC3C7", lineColor = "#BDC3C7", 
            minorGridLineColor = "#BDC3C7", tickColor = "#BDC3C7", 
            tickWidth = 1), legendBackgroundColor = "rgba(0, 0, 0, 0.5)", 
        background2 = "#505053", dataLabelsColor = "#B0B0B3", 
        textColor = "#34495e", contrastTextColor = "#F0F0F3", 
        maskColor = "rgba(255,255,255,0.3)")
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_elementId <- function (hc, id = NULL) 
{
    assertthat::assert_that(is.highchart(hc))
    hc$elementId <- as.character(id)
    hc
}


hc_add_series_flags <- function (hc, dates, title = LETTERS[seq(length(dates))], text = title, 
    id = NULL, ...) 
{
    assertthat::assert_that(is.highchart(hc), is.date(dates))
    dfflags <- data_frame(x = datetime_to_timestamp(dates), title = title, 
        text = text)
    dsflags <- list_parse(dfflags)
    hc %>% hc_add_series(data = dsflags, onSeries = id, type = "flags", 
        ...)
}


highcharts_demo <- function () 
{
    dtemp <- structure(list(month = c("Jan", "Feb", "Mar", "Apr", 
        "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
        tokyo = c(7, 6.9, 9.5, 14.5, 18.2, 21.5, 25.2, 26.5, 
            23.3, 18.3, 13.9, 9.6), new_york = c(-0.2, 0.8, 5.7, 
            11.3, 17, 22, 24.8, 24.1, 20.1, 14.1, 8.6, 2.5), 
        berlin = c(-0.9, 0.6, 3.5, 8.4, 13.5, 17, 18.6, 17.9, 
            14.3, 9, 3.9, 1), london = c(3.9, 4.2, 5.7, 8.5, 
            11.9, 15.2, 17, 16.6, 14.2, 10.3, 6.6, 4.8)), .Names = c("month", 
        "tokyo", "new_york", "berlin", "london"), row.names = c(NA, 
        12L), class = c("tbl_df", "tbl", "data.frame"))
    highchart() %>% hc_title(text = "Monthly Average Temperature") %>% 
        hc_subtitle(text = "Source: WorldClimate.com") %>% hc_yAxis(title = list(text = "Temperature")) %>% 
        hc_xAxis(title = list(text = "Months")) %>% hc_xAxis(categories = dtemp$month) %>% 
        hc_add_series(name = "Tokyo", data = dtemp$tokyo) %>% 
        hc_add_series(name = "London", data = dtemp$london) %>% 
        hc_add_series(name = "Berlin", data = dtemp$berlin)
}


hc_mapNavigation <- function (hc, ...) 
{
    .hc_opt(hc, "mapNavigation", ...)
}


highchart2 <- function (hc_opts = list(), theme = NULL, width = NULL, height = NULL, 
    elementId = NULL, debug = FALSE) 
{
    unfonts <- unique(c(.hc_get_fonts(hc_opts), .hc_get_fonts(theme)))
    x <- list(hc_opts = hc_opts, theme = theme, fonts = unfonts, 
        debug = debug)
    htmlwidgets::createWidget(name = "highchart2", x, width = width, 
        height = height, package = "highcharter", elementId = elementId, 
        sizingPolicy = htmlwidgets::sizingPolicy(defaultWidth = "100%", 
            knitr.figure = FALSE, knitr.defaultWidth = "100%", 
            browser.fill = TRUE))
}


is.highchart <- function (x) 
{
    inherits(x, "highchart") || inherits(x, "highchart2")
}


hc_exporting <- function (hc, ...) 
{
    .hc_opt(hc, "exporting", ...)
}


renderHighchart <- function (expr, env = parent.frame(), quoted = FALSE) 
{
    if (!quoted) {
        expr <- substitute(expr)
    }
    shinyRenderWidget(expr, highchartOutput, env, quoted = TRUE)
}


hc_yAxis <- function (hc, ...) 
{
    .hc_opt(hc, "yAxis", ...)
}


hc_theme_tufte2 <- function (...) 
{
    theme <- hc_theme_tufte(xAxis = list(tickWidth = 0, lineWidth = 1, 
        lineColor = "#737373"), yAxis = list(tickWidth = 0, lineWidth = 1, 
        gridLineColor = "white", gridZIndex = 4))
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_add_series_list <- function (hc, x) 
{
    assertthat::assert_that(is.highchart(hc), (is.list(x) | is.data.frame(x)))
    if (is.data.frame(x)) 
        x <- list_parse(x)
    hc$x$hc_opts$series <- append(hc$x$hc_opts$series, x)
    hc
}


list.parse2 <- function (df) 
{
    .Deprecated("list_parse2")
    list_parse2(df)
}


hc_theme_monokai <- function (...) 
{
    theme <- hc_theme(colors = c("#F92672", "#66D9EF", "#A6E22E", 
        "#A6E22E"), chart = list(backgroundColor = "#272822", 
        style = list(fontFamily = "Inconsolata", color = "#A2A39C")), 
        title = list(style = list(color = "#A2A39C"), align = "left"), 
        subtitle = list(style = list(color = "#A2A39C"), align = "left"), 
        legend = list(align = "right", verticalAlign = "bottom", 
            itemStyle = list(fontWeight = "normal", color = "#A2A39C")), 
        xAxis = list(gridLineDashStyle = "Dot", gridLineWidth = 1, 
            gridLineColor = "#A2A39C", lineColor = "#A2A39C", 
            minorGridLineColor = "#A2A39C", tickColor = "#A2A39C", 
            tickWidth = 1), yAxis = list(gridLineDashStyle = "Dot", 
            gridLineColor = "#A2A39C", lineColor = "#A2A39C", 
            minorGridLineColor = "#A2A39C", tickColor = "#A2A39C", 
            tickWidth = 1))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


list.parse3 <- function (df) 
{
    .Deprecated("list_parse")
    list_parse(df)
}


str_to_id <- function (x) 
{
    assertthat::assert_that(is.character(x) | is.factor(x))
    x %>% as.character() %>% str_trim() %>% str_to_lower() %>% 
        str_replace_all("\\s+", "_") %>% str_replace_all("\\\\|/", 
        "_") %>% str_replace_all("\\[|\\]", "_") %>% str_replace_all("_+", 
        "_") %>% str_replace_all("_$|^_", "")
}


hc_theme_sandsignika <- function (...) 
{
    theme <- list(colors = c("#F45B5B", "#8085E9", "#8D4654", 
        "#7798BF", "#AAEEEE", "#FF0066", "#EEAAEE", "#55BF3B", 
        "#DF5353"), chart = list(backgroundColor = NULL, divBackgroundImage = "http://www.highcharts.com/samples/graphics/sand.png", 
        style = list(fontFamily = "Signika, serif")), title = list(style = list(color = "black", 
        fontSize = "16px", fontWeight = "bold")), subtitle = list(style = list(color = "black")), 
        tooltip = list(borderWidth = 0), legend = list(itemStyle = list(fontWeight = "bold", 
            fontSize = "13px")), xAxis = list(labels = list(style = list(color = "#6e6e70"))), 
        yAxis = list(labels = list(style = list(color = "#6e6e70"))), 
        plotOptions = list(series = list(shadow = FALSE), candlestick = list(lineColor = "#404048"), 
            map = list(shadow = FALSE)), navigator = list(xAxis = list(gridLineColor = "#D0D0D8")), 
        rangeSelector = list(buttonTheme = list(fill = "white", 
            stroke = "#C0C0C8", `stroke-width` = 1, states = list(select = list(fill = "#D0D0D8")))), 
        scrollbar = list(trackBorderColor = "#C0C0C8"), background2 = "#E0E0E8")
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


fix_1_length_data <- function (x) 
{
    if (getOption("highcharter.verbose")) 
        message("fix_1_length")
    if (class(x) != "list" & length(x) == 1) 
        x <- list(x)
    x
}


hc_add_series_labels_values <- function (hc, labels, values, colors = NULL, ...) 
{
    assertthat::assert_that(is.highchart(hc), is.numeric(values), 
        length(labels) == length(values))
    df <- data_frame(name = labels, y = values)
    if (!is.null(colors)) {
        assert_that(length(labels) == length(colors))
        df <- mutate(df, color = colors)
    }
    ds <- list_parse(df)
    hc <- hc %>% hc_add_series(data = ds, ...)
    hc
}


is.hexcolor <- function (x) 
{
    pattern <- "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3}|[A-Fa-f0-9]{8})$"
    str_detect(x, pattern)
}


hc_theme_flatdark <- function (...) 
{
    theme <- hc_theme_flat(chart = list(backgroundColor = "#34495e"), 
        xAxis = list(gridLineColor = "#46627f", tickColor = "#46627f", 
            lineColor = "#46627f", title = list(style = list(color = "#FFFFFF"))), 
        yAxis = list(gridLineColor = "#46627f", tickColor = "#46627f", 
            title = list(style = list(color = "#FFFFFF"))), title = list(style = list(color = "#FFFFFF")), 
        subtitle = list(style = list(color = "#666666")), legend = list(itemStyle = list(color = "#C0C0C0"), 
            itemHoverStyle = list(color = "#C0C0C0"), itemHiddenStyle = list(color = "#444444")))
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


mutate_mapping <- function (data, mapping) 
{
    stopifnot(is.data.frame(data), inherits(mapping, "hcaes"))
    tran <- as.character(mapping)
    newv <- names(mapping)
    setNames(tran, newv)
    data <- dplyr::mutate_(data, .dots = setNames(tran, newv))
    data
}


tags <- htmltools::tags # re-exported from htmltools package

hc_legend <- function (hc, ...) 
{
    .hc_opt(hc, "legend", ...)
}


dash_styles <- function () 
{
    .Deprecated()
    c("Solid", "ShortDash", "ShortDot", "ShortDashDot", "ShortDashDotDot", 
        "Dot", "Dash", "LongDash", "DashDot", "LongDashDot", 
        "LongDashDotDot")
}


hc_drilldown <- function (hc, ...) 
{
    .hc_opt(hc, "drilldown", ...)
}


hc_yAxis_multiples <- function (hc, ...) 
{
    if (length(list(...)) == 1 & class(list(...)[[1]]) == "hc_yaxis_list") {
        hc$x$hc_opts$yAxis <- list(...)[[1]]
    }
    else {
        hc$x$hc_opts$yAxis <- list(...)
    }
    hc
}


hc_title <- function (hc, ...) 
{
    .hc_opt(hc, "title", ...)
}


export_hc <- function (hc, filename = NULL) 
{
    . <- NULL
    stopifnot(!is.null(filename))
    if (!str_detect(filename, ".js$")) 
        filename <- str_c(filename, ".js")
    jslns <- hc$x$hc_opts %>% toJSON(pretty = TRUE, auto_unbox = TRUE, 
        force = TRUE, null = "null") %>% str_split("\n") %>% 
        head(1) %>% unlist() %>% str_replace("\"", "") %>% str_replace("\":", 
        ":")
    fflag <- str_detect(jslns, "function()")
    if (any(fflag)) {
        jslns <- ifelse(fflag, str_replace(jslns, "\"function", 
            "function"), jslns)
        jslns <- ifelse(fflag, str_replace(jslns, "\",$", ","), 
            jslns)
        jslns <- ifelse(fflag, str_replace(jslns, "\"$", ""), 
            jslns)
        jslns <- ifelse(fflag, str_replace_all(jslns, "\\\\n", 
            str_c("\\\\n", str_extract(jslns, "^\\s+"))), jslns)
    }
    jslns <- jslns %>% unlist() %>% tail(-1) %>% str_c("    ", 
        ., collapse = "\n") %>% str_replace_all("\n\\s{4,}\\]\\,\n\\s{4,}\\[\n\\s{4,}", 
        "],[") %>% sprintf("$(function () {\n  $('#container').highcharts({\n%s\n  );\n});", 
        .)
    writeLines(jslns, filename)
}


hc_add_series <- function (hc, data = NULL, ...) 
{
    assertthat::assert_that(is.highchart(hc))
    UseMethod("hc_add_series", data)
}


get_data_from_map <- function (mapdata) 
{
    mapdata$features %>% map("properties") %>% map_df(function(x) {
        x[!map_lgl(x, is.null)]
    })
}


hc_theme_elementary <- function (...) 
{
    theme <- list(colors = list("#41B5E9", "#FA8832", "#34393C", 
        "#E46151"), chart = list(style = list(color = "#333", 
        fontFamily = "Open Sans")), title = list(style = list(fontFamily = "Raleway", 
        fontWeight = "100")), subtitle = list(style = list(fontFamily = "Raleway", 
        fontWeight = "100")), legend = list(align = "right", 
        verticalAlign = "bottom"), xAxis = list(gridLineWidth = 1, 
        gridLineColor = "#F3F3F3", lineColor = "#F3F3F3", minorGridLineColor = "#F3F3F3", 
        tickColor = "#F3F3F3", tickWidth = 1), yAxis = list(gridLineColor = "#F3F3F3", 
        lineColor = "#F3F3F3", minorGridLineColor = "#F3F3F3", 
        tickColor = "#F3F3F3", tickWidth = 1))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hchist <- function (x, ...) 
{
    stopifnot(is.numeric(x))
    hchart(x, ...)
}


hc_add_theme <- function (hc, hc_thm) 
{
    assert_that(is.highchart(hc), .is_hc_theme(hc_thm))
    hc$x$fonts <- unique(c(hc$x$fonts, .hc_get_fonts(hc_thm)))
    hc$x$theme <- hc_thm
    hc
}


hc_navigator <- function (hc, ...) 
{
    .hc_opt(hc, "navigator", ...)
}


hc_theme_darkunica <- function (...) 
{
    theme <- list(colors = c("#2b908f", "#90ee7e", "#f45b5b", 
        "#7798BF", "#aaeeee", "#ff0066", "#eeaaee", "#55BF3B"), 
        chart = list(backgroundColor = list(linearGradient = list(x1 = 0, 
            y1 = 0, x2 = 1, y2 = 1), stops = list(list(0, "#2a2a2b"), 
            list(1, "#3e3e40"))), style = list(fontFamily = "Unica One, sans-serif"), 
            plotBorderColor = "#606063"), title = list(style = list(color = "#E0E0E3", 
            textTransform = "uppercase", fontSize = "20px")), 
        subtitle = list(style = list(color = "#E0E0E3", textTransform = "uppercase")), 
        xAxis = list(gridLineColor = "#707073", labels = list(style = list(color = "#E0E0E3")), 
            lineColor = "#707073", minorGridLineColor = "#505053", 
            tickColor = "#707073", title = list(style = list(color = "#A0A0A3"))), 
        yAxis = list(gridLineColor = "#707073", labels = list(style = list(color = "#E0E0E3")), 
            lineColor = "#707073", minorGridLineColor = "#505053", 
            tickColor = "#707073", tickWidth = 1, title = list(style = list(color = "#A0A0A3"))), 
        tooltip = list(backgroundColor = "rgba(0, 0, 0, 0.85)", 
            style = list(color = "#F0F0F0")), plotOptions = list(series = list(dataLabels = list(color = "#B0B0B3"), 
            marker = list(lineColor = "#333")), boxplot = list(fillColor = "#505053"), 
            candlestick = list(lineColor = "white"), errorbar = list(color = "white")), 
        legend = list(itemStyle = list(color = "#E0E0E3"), itemHoverStyle = list(color = "#FFF"), 
            itemHiddenStyle = list(color = "#606063")), credits = list(style = list(color = "#666")), 
        labels = list(style = list(color = "#707073")), drilldown = list(activeAxisLabelStyle = list(color = "#F0F0F3"), 
            activeDataLabelStyle = list(color = "#F0F0F3")), 
        navigation = list(buttonOptions = list(symbolStroke = "#DDDDDD", 
            theme = list(fill = "#505053"))), rangeSelector = list(buttonTheme = list(fill = "#505053", 
            stroke = "#000000", style = list(color = "#CCC"), 
            states = list(hover = list(fill = "#707073", stroke = "#000000", 
                style = list(color = "white")), select = list(fill = "#000003", 
                stroke = "#000000", style = list(color = "white")))), 
            inputBoxBorderColor = "#505053", inputStyle = list(backgroundColor = "#333", 
                color = "silver"), labelStyle = list(color = "silver")), 
        navigator = list(handles = list(backgroundColor = "#666", 
            borderColor = "#AAA"), outlineColor = "#CCC", maskFill = "rgba(255,255,255,0.1)", 
            series = list(color = "#7798BF", lineColor = "#A6C7ED"), 
            xAxis = list(gridLineColor = "#505053")), scrollbar = list(barBackgroundColor = "#808083", 
            barBorderColor = "#808083", buttonArrowColor = "#CCC", 
            buttonBackgroundColor = "#606063", buttonBorderColor = "#606063", 
            rifleColor = "#FFF", trackBackgroundColor = "#404043", 
            trackBorderColor = "#404043"), legendBackgroundColor = "rgba(0, 0, 0, 0.5)", 
        background2 = "#505053", dataLabelsColor = "#B0B0B3", 
        textColor = "#C0C0C0", contrastTextColor = "#F0F0F3", 
        maskColor = "rgba(255,255,255,0.3)")
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


highchartOutput <- function (outputId, width = "100%", height = "400px") 
{
    shinyWidgetOutput(outputId, "highchart", width, height, package = "highcharter")
}


renderHighchart2 <- function (expr, env = parent.frame(), quoted = FALSE) 
{
    if (!quoted) {
        expr <- substitute(expr)
    }
    shinyRenderWidget(expr, highchartOutput2, env, quoted = TRUE)
}


hw_grid <- function (..., ncol = NULL, rowheight = NULL) 
{
    input_list <- as.list(substitute(list(...)))[-1L]
    params <- list()
    for (i in 1:length(input_list)) {
        x <- eval.parent(input_list[[i]])
        if (any(class(x) == "list")) {
            for (j in 1:length(x)) {
                y <- eval(x[[j]])
                params[[length(params) + 1]] <- y
            }
        }
        else {
            params[[length(params) + 1]] <- x
        }
    }
    if (!all(sapply(params, function(x) inherits(x, "htmlwidget")))) 
        stop("All parameters must be htmlwidget objects")
    if (is.null(ncol)) 
        ncol <- n2mfrow(length(params))[1]
    if (ncol > 12) 
        ncol <- 12
    ncolm <- floor(ncol/2)
    divs <- map(params, function(x) {
        x$width <- "100%"
        if (!is.null(rowheight)) 
            x$height <- rowheight
        tags$div(class = sprintf("col-1-%s mobile-col-1-%s", 
            ncol, ncolm), x)
    })
    divgrid <- tags$div(class = "grid grid-pad", style = "with:100%", 
        divs)
    class(divgrid) <- c(class(divgrid), "htmlwdwtgrid")
    divgrid
}


hc_add_series_df <- function (hc, data, type = NULL, ...) 
{
    .Deprecated("hc_add_series")
    assertthat::assert_that(is.highchart(hc))
    series <- get_hc_series_from_df(data, type = type, ...)
    hc_add_series_list(hc, series)
}


hc_colors <- function (hc, colors) 
{
    assertthat::assert_that(is.vector(colors))
    if (length(colors) == 1) 
        colors <- list(colors)
    hc$x$hc_opts$colors <- colors
    hc
}


hc_annotationsOptions <- function (hc, ...) 
{
    .hc_opt(hc, "annotationsOptions", ...)
}


hcspark <- function (x = NULL, type = NULL, ...) 
{
    stopifnot(is.numeric(x))
    highchart() %>% hc_plotOptions(series = list(showInLegend = FALSE, 
        dataLabels = list(enabled = FALSE)), line = list(marker = list(enabled = FALSE))) %>% 
        hc_add_series(data = x, type = type, ...) %>% hc_add_theme(hc_theme_sparkline())
}


hcbar <- function (x, ...) 
{
    stopifnot(is.character(x) | is.factor(x))
    hchart(x, ...)
}


hc_subtitle <- function (hc, ...) 
{
    .hc_opt(hc, "subtitle", ...)
}


hcmap <- function (map = "custom/world", download_map_data = getOption("highcharter.download_map_data"), 
    data = NULL, value = NULL, joinBy = NULL, ...) 
{
    map <- fix_map_name(map)
    hc <- highchart(type = "map")
    if (download_map_data) {
        mapdata <- download_map_data(map)
    }
    else {
        dep <- htmlDependency(name = basename(map), version = "0.1.0", 
            src = c(href = "https://code.highcharts.com/mapdata"), 
            script = map)
        hc$dependencies <- c(hc$dependencies, list(dep))
        mapdata <- JS(sprintf("Highcharts.maps['%s']", str_replace(map, 
            "\\.js$", "")))
    }
    if (is.null(data)) {
        hc <- hc %>% hc_add_series.default(mapData = mapdata, 
            ...)
    }
    else {
        data <- mutate_(data, value = value)
        hc <- hc %>% hc_add_series.default(mapData = mapdata, 
            data = list_parse(data), joinBy = joinBy, ...) %>% 
            hc_colorAxis(auxpar = NULL)
    }
    hc %>% hc_credits(enabled = TRUE)
}


list_parse <- function (df) 
{
    assertthat::assert_that(is.data.frame(df))
    map_if(df, is.factor, as.character) %>% as_data_frame() %>% 
        list.parse() %>% setNames(NULL)
}


hc_defs <- function (hc, ...) 
{
    .hc_opt(hc, "defs", ...)
}


hcaes <- function (x, y, ...) 
{
    mapping <- structure(as.list(match.call()[-1]), class = "uneval")
    mapping <- mapping[names(mapping) != ""]
    class(mapping) <- c("hcaes", class(mapping))
    mapping
}


datetime_to_timestamp <- function (dt) 
{
    assertthat::assert_that(assertthat::is.date(dt) | assertthat::is.time(dt))
    tmstmp <- as.numeric(as.POSIXct(dt))
    tmstmp <- 1000 * tmstmp
    tmstmp
}


hc_series <- function (hc, ...) 
{
    .hc_opt(hc, "series", ...)
}


tooltip_table <- function (x, y, title = NULL, img = NULL, ...) 
{
    assertthat::assert_that(length(x) == length(y))
    tbl <- map2(x, y, function(x, y) {
        tags$tr(tags$th(x), tags$td(y))
    })
    tbl <- tags$table(tbl, ...)
    if (!is.null(title)) 
        tbl <- tagList(title, tbl)
    if (!is.null(img)) 
        tbl <- tagList(tbl, img)
    as.character(tbl)
}


hc_add_series_ohlc <- function (hc, x, type = "candlestick", ...) 
{
    .Deprecated("hc_add_series")
    assertthat::assert_that(is.highchart(hc), is.xts(x), is.OHLC(x))
    hc$x$type <- "stock"
    time <- datetime_to_timestamp(time(x))
    xdf <- cbind(time, as.data.frame(x))
    xds <- list_parse2(xdf)
    nm <- ifelse(!is.null(list(...)[["name"]]), list(...)[["name"]], 
        str_extract(names(x)[1], "^[A-Za-z]+"))
    hc <- hc %>% hc_add_series(data = xds, name = nm, type = type)
    hc
}


hciconarray <- function (labels, counts, rows = NULL, icons = NULL, size = 4, 
    ...) 
{
    assertthat::assert_that(length(counts) == length(labels))
    if (is.null(rows)) {
        sizegrid <- n2mfrow(sum(counts))
        w <- sizegrid[1]
        h <- sizegrid[2]
    }
    else {
        h <- rows
        w <- ceiling(sum(counts)/rows)
    }
    ds <- data_frame(x = rep(1:w, h), y = rep(1:h, each = w)) %>% 
        head(sum(counts)) %>% mutate_(y = "-y") %>% mutate(gr = rep(seq_along(labels), 
        times = counts)) %>% left_join(data_frame(gr = seq_along(labels), 
        name = as.character(labels)), by = "gr") %>% group_by_("name") %>% 
        do(data = list_parse2(data_frame(.$x, .$y))) %>% ungroup() %>% 
        left_join(data_frame(labels = as.character(labels), counts), 
            by = c(name = "labels")) %>% arrange_("-counts") %>% 
        mutate_(percent = "counts/sum(counts)*100")
    if (!is.null(icons)) {
        assertthat::assert_that(length(icons) %in% c(1, length(labels)))
        dsmrk <- ds %>% mutate(iconm = icons) %>% group_by_("name") %>% 
            do(marker = list(symbol = fa_icon_mark(.$iconm)))
        ds <- ds %>% left_join(dsmrk, by = "name") %>% mutate(icon = fa_icon(icons))
    }
    ds <- mutate(ds, ...)
    hc <- highchart() %>% hc_chart(type = "scatter") %>% hc_add_series_list(ds) %>% 
        hc_plotOptions(series = list(cursor = "default", marker = list(radius = size), 
            states = list(hover = list(enabled = FALSE)), events = list(legendItemClick = JS("function () { return false; }")))) %>% 
        hc_tooltip(pointFormat = "{point.series.options.counts} ({point.series.options.percent:.2f}%)") %>% 
        hc_add_theme(hc_theme_merge(getOption("highcharter.theme"), 
            hc_theme_null()))
    hc
}


hc_xAxis <- function (hc, ...) 
{
    .hc_opt(hc, "xAxis", ...)
}


hc_theme_db <- function (...) 
{
    theme <- list(colors = c("#A9CF54", "#C23C2A", "#FFFFFF", 
        "#979797", "#FBB829"), chart = list(backgroundColor = "#242F39"), 
        legend = list(enabled = TRUE, align = "right", verticalAlign = "bottom", 
            itemStyle = list(color = "#C0C0C0"), itemHoverStyle = list(color = "#C0C0C0"), 
            itemHiddenStyle = list(color = "#444444")), title = list(text = NULL, 
            style = list(color = "#FFFFFF")), tooltip = list(backgroundColor = "#1C242D", 
            borderColor = "#1C242D", borderWidth = 1, borderRadius = 0, 
            style = list(color = "#FFFFFF")), subtitle = list(style = list(color = "#666666")), 
        xAxis = list(gridLineColor = "#2E3740", gridLineWidth = 1, 
            labels = list(style = list(color = "#525252")), lineColor = "#2E3740", 
            tickColor = "#2E3740", title = list(style = list(color = "#FFFFFF"), 
                text = NULL)), yAxis = list(gridLineColor = "#2E3740", 
            gridLineWidth = 1, labels = list(style = list(color = "#525252"), 
                lineColor = "#2E3740", tickColor = "#2E3740", 
                title = list(style = list(color = "#FFFFFF"), 
                  text = NULL))))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hcboxplot <- function (x = NULL, var = NULL, var2 = NULL, outliers = TRUE, 
    ...) 
{
    stopifnot(is.numeric(x))
    if (is.null(var)) 
        var <- NA
    if (is.null(var2)) 
        var2 <- NA
    df <- data_frame(x, g1 = var, g2 = var2)
    get_box_values <- function(x) {
        boxplot.stats(x)$stats %>% t() %>% as.data.frame() %>% 
            setNames(c("low", "q1", "median", "q3", "high"))
    }
    get_outliers_values <- function(x) {
        boxplot.stats(x)$out
    }
    series_box <- df %>% group_by_("g1", "g2") %>% do(data = get_box_values(.$x)) %>% 
        unnest() %>% group_by_("g2") %>% do(data = list_parse(rename_(select_(., 
        "-g2"), name = "g1"))) %>% mutate(type = "boxplot") %>% 
        mutate_(id = "as.character(g2)")
    if (length(list(...)) > 0) 
        series_box <- add_arg_to_df(series_box, ...)
    series_out <- df %>% group_by_("g1", "g2") %>% do(data = get_outliers_values(.$x)) %>% 
        unnest() %>% group_by_("g2") %>% do(data = list_parse(select_(., 
        name = "g1", y = "data"))) %>% mutate(type = "scatter") %>% 
        mutate(linkedTo = "as.character(g2)")
    if (length(list(...)) > 0) 
        series_out <- add_arg_to_df(series_out, ...)
    if (!has_name(list(...), "color")) {
        colors <- colorize(seq(1, nrow(series_box)))
        colors <- hex_to_rgba(colors, alpha = 0.75)
    }
    if (!has_name(list(...), "name")) {
        series_box <- rename_(series_box, name = "g2")
        series_out <- rename_(series_out, name = "g2")
    }
    hc <- highchart() %>% hc_chart(type = "bar") %>% hc_xAxis(type = "category") %>% 
        hc_plotOptions(series = list(marker = list(symbol = "circle")))
    hc <- hc_add_series_list(hc, list_parse(series_box))
    if (is.na(var2) || is.na(var)) {
        hc <- hc %>% hc_xAxis(categories = "") %>% hc_plotOptions(series = list(showInLegend = FALSE))
    }
    if (outliers) 
        hc <- hc_add_series_list(hc, list_parse(series_out))
    hc
}


hc_theme_538 <- function (...) 
{
    theme <- list(colors = c("#FF2700", "#008FD5", "#77AB43", 
        "#636464", "#C4C4C4"), chart = list(backgroundColor = "#F0F0F0", 
        plotBorderColor = "#606063", style = list(fontFamily = "Roboto", 
            color = "#3C3C3C")), title = list(align = "left", 
        style = list(fontWeight = "bold")), subtitle = list(align = "left"), 
        xAxis = list(gridLineWidth = 1, gridLineColor = "#D7D7D8", 
            labels = list(style = list(fontFamily = "Unica One, sans-serif", 
                color = "#3C3C3C")), lineColor = "#D7D7D8", minorGridLineColor = "#505053", 
            tickColor = "#D7D7D8", tickWidth = 1, title = list(style = list(color = "#A0A0A3"))), 
        yAxis = list(gridLineColor = "#D7D7D8", labels = list(style = list(fontFamily = "Unica One, sans-serif", 
            color = "#3C3C3C")), lineColor = "#D7D7D8", minorGridLineColor = "#505053", 
            tickColor = "#D7D7D8", tickWidth = 1, title = list(style = list(color = "#A0A0A3"))), 
        tooltip = list(backgroundColor = "rgba(0, 0, 0, 0.85)", 
            style = list(color = "#F0F0F0")), legend = list(itemStyle = list(color = "#3C3C3C"), 
            itemHiddenStyle = list(color = "#606063")), credits = list(style = list(color = "#666")), 
        labels = list(style = list(color = "#D7D7D8")), legendBackgroundColor = "rgba(0, 0, 0, 0.5)", 
        background2 = "#505053", dataLabelsColor = "#B0B0B3", 
        textColor = "#C0C0C0", contrastTextColor = "#F0F0F3", 
        maskColor = "rgba(255,255,255,0.3)")
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_add_annotation <- function (hc, ...) 
{
    assertthat::assert_that(is.highchart(hc))
    hc$x$hc_opts[["annotations"]] <- append(hc$x$hc_opts[["annotations"]], 
        list(list(...)))
    hc
}


hcdensity <- function (x, ...) 
{
    stopifnot(inherits(x, "density") || inherits(x, "numeric"))
    if (class(x) == "numeric") 
        x <- density(x)
    hchart(x, ...)
}


hc_theme_sparkline <- function (...) 
{
    theme <- list(chart = list(backgroundColor = NULL, borderWidth = 0, 
        margin = list(2, 0, 2, 0), style = list(overflow = "visible"), 
        skipClone = TRUE), title = list(text = ""), xAxis = list(labels = list(enabled = FALSE), 
        title = list(text = NULL), startOnTick = FALSE, endOnTick = FALSE, 
        tickPositions = list()), yAxis = list(endOnTick = FALSE, 
        startOnTick = FALSE, labels = list(enabled = FALSE), 
        title = list(text = NULL), tickPositions = list()), tooltip = list(backgroundColor = NULL, 
        borderWidth = 0, shadow = FALSE, useHTML = TRUE, hideDelay = 0, 
        shared = TRUE, padding = 0, positioner = JS("function (w, h, point) { return { x: point.plotX - w / 2,\n        y: point.plotY - h };}")), 
        plotOptions = list(series = list(animation = FALSE, lineWidth = 1, 
            shadow = FALSE, states = list(hover = list(lineWidth = 1)), 
            marker = list(radius = 1, states = list(hover = list(radius = 2))), 
            fillOpacity = 0.25)))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_credits <- function (hc, ...) 
{
    .hc_opt(hc, "credits", ...)
}


hcpie <- function (x, ...) 
{
    stopifnot(is.character(x) | is.factor(x))
    hchart(x, type = "pie", ...)
}


create_yaxis <- function (naxis = 2, heights = 1, sep = 0.01, offset = 0, turnopposite = TRUE, 
    ...) 
{
    pcnt <- function(x) paste0(x * 100, "%")
    heights <- rep(heights, length = naxis)
    heights <- (heights/sum(heights)) %>% map(function(x) c(x, 
        sep)) %>% unlist() %>% head(-1) %>% {
        ./sum(.)
    } %>% round(5)
    tops <- cumsum(c(0, head(heights, -1)))
    tops <- pcnt(tops)
    heights <- pcnt(heights)
    dfaxis <- data_frame(height = heights, top = tops, offset = offset)
    dfaxis <- dfaxis %>% dplyr::filter(seq(1:nrow(dfaxis))%%2 != 
        0)
    if (turnopposite) {
        ops <- rep_len(c(FALSE, TRUE), length.out = nrow(dfaxis))
        dfaxis <- dfaxis %>% mutate(opposite = ops)
    }
    dfaxis <- bind_cols(dfaxis, data_frame(nid = seq(naxis), 
        ...))
    yaxis <- list_parse(dfaxis)
    class(yaxis) <- "hc_yaxis_list"
    yaxis
}


hc_theme_tufte <- function (...) 
{
    theme <- hc_theme(colors = list("#737373", "#D8D7D6", "#B2B0AD", 
        "#8C8984"), chart = list(style = list(fontFamily = "Cardo")), 
        xAxis = list(lineWidth = 0, minorGridLineWidth = 0, lineColor = "transparent", 
            tickColor = "#737373"), yAxis = list(lineWidth = 0, 
            minorGridLineWidth = 0, lineColor = "transparent", 
            tickColor = "#737373", tickWidth = 1, gridLineColor = "transparent"), 
        legend = list(enabled = FALSE))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_theme_smpl <- function (...) 
{
    theme <- hc_theme(colors = c("#d35400", "#2980b9", "#2ecc71", 
        "#f1c40f", "#2c3e50", "#7f8c8d"), chart = list(style = list(fontFamily = "Roboto")), 
        title = list(align = "left", style = list(fontFamily = "Roboto Condensed", 
            fontWeight = "bold")), subtitle = list(align = "left", 
            style = list(fontFamily = "Roboto Condensed")), legend = list(align = "right", 
            verticalAlign = "bottom"), xAxis = list(gridLineWidth = 1, 
            gridLineColor = "#F3F3F3", lineColor = "#F3F3F3", 
            minorGridLineColor = "#F3F3F3", tickColor = "#F3F3F3", 
            tickWidth = 1), yAxis = list(gridLineColor = "#F3F3F3", 
            lineColor = "#F3F3F3", minorGridLineColor = "#F3F3F3", 
            tickColor = "#F3F3F3", tickWidth = 1), plotOptions = list(line = list(marker = list(enabled = FALSE), 
            states = list(hover = list(lineWidthPlus = 1))), 
            spline = list(marker = list(enabled = FALSE), states = list(hover = list(lineWidthPlus = 1))), 
            area = list(marker = list(enabled = FALSE), states = list(hover = list(lineWidthPlus = 1))), 
            areaspline = list(marker = list(enabled = FALSE), 
                states = list(hover = list(lineWidthPlus = 1)))))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


fa_icon <- function (iconname = "circle") 
{
    faicos <- readRDS(system.file("extdata/faicos.rds", package = "highcharter"))
    stopifnot(iconname %in% faicos$name)
    sprintf("<i class=\"fa fa-%s\"></i>", iconname)
}


hc_theme <- function (...) 
{
    structure(list(...), class = "hc_theme")
}


highchartOutput2 <- function (outputId, width = "100%", height = "400px") 
{
    shinyWidgetOutput(outputId, "highchart2", width, height, 
        package = "highcharter")
}


hc_theme_ft <- function (...) 
{
    theme <- list(colors = c("#89736C", "#43423e", "#2e6e9e", 
        "#FF0000", "#BEDDDE"), chart = list(backgroundColor = "#FFF1E0", 
        style = list(fontFamily = "Droid Sans", color = "#777")), 
        title = list(align = "left", style = list(fontFamily = "Droid Serif", 
            color = "black", fontWeight = "bold")), subtitle = list(align = "left", 
            style = list(fontFamily = "Droid Serif", fontWeight = "bold")), 
        yAxis = list(gridLineDashStyle = "Dot", gridLineColor = "#CEC6B9", 
            lineColor = "#CEC6B9", minorGridLineColor = "#CEC6B9", 
            labels = list(align = "left", x = 0, y = -2), tickLength = 0, 
            tickColor = "#CEC6B9", tickWidth = 1, title = list(style = list(color = "#74736c"))), 
        tooltip = list(backgroundColor = "#FFFFFF", borderColor = "#76c0c1", 
            style = list(color = "#000000")), legend = list(itemStyle = list(color = "#3C3C3C"), 
            itemHiddenStyle = list(color = "#606063")), credits = list(style = list(color = "#666")), 
        labels = list(style = list(color = "#D7D7D8")), drilldown = list(activeAxisLabelStyle = list(color = "#F0F0F3"), 
            activeDataLabelStyle = list(color = "#F0F0F3")), 
        navigation = list(buttonOptions = list(symbolStroke = "#DDDDDD", 
            theme = list(fill = "#505053"))), legendBackgroundColor = "rgba(0, 0, 0, 0.5)", 
        background2 = "#505053", dataLabelsColor = "#B0B0B3", 
        textColor = "#C0C0C0", contrastTextColor = "#F0F0F3", 
        maskColor = "rgba(255,255,255,0.3)")
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


hc_theme_null <- function (...) 
{
    theme <- list(chart = list(backgroundColor = "transparent"), 
        plotOptions = list(line = list(marker = list(enabled = FALSE))), 
        legend = list(enabled = TRUE, align = "right", verticalAlign = "bottom"), 
        credits = list(enabled = FALSE), xAxis = list(visible = FALSE), 
        yAxis = list(visible = FALSE))
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}


color_classes <- function (breaks = NULL, colors = c("#440154", "#21908C", "#FDE725")) 
{
    lbrks <- length(breaks)
    list_parse(data.frame(from = breaks[-lbrks], to = breaks[-1], 
        color = (grDevices::colorRampPalette(colors))(lbrks - 
            1), stringsAsFactors = FALSE))
}


hc_theme_economist <- function (...) 
{
    theme <- list(colors = c("#6794a7", "#014d64", "#76c0c1", 
        "#01a2d9", "#7ad2f6", "#00887d", "#adadad", "#7bd3f6", 
        "#7c260b", "#ee8f71", "#76c0c1", "#a18376"), chart = list(backgroundColor = "#d5e4eb", 
        style = list(fontFamily = "Droid Sans", color = "#3C3C3C")), 
        title = list(align = "left", style = list(fontWeight = "bold")), 
        subtitle = list(align = "left"), yAxis = list(gridLineColor = "#FFFFFF", 
            lineColor = "#FFFFFF", minorGridLineColor = "#FFFFFF", 
            tickColor = "#D7D7D8", tickWidth = 1, title = list(style = list(color = "#A0A0A3"))), 
        tooltip = list(backgroundColor = "#FFFFFF", borderColor = "#76c0c1", 
            style = list(color = "#000000")), legend = list(itemStyle = list(color = "#3C3C3C"), 
            itemHiddenStyle = list(color = "#606063")), credits = list(style = list(color = "#666")), 
        labels = list(style = list(color = "#D7D7D8")), drilldown = list(activeAxisLabelStyle = list(color = "#F0F0F3"), 
            activeDataLabelStyle = list(color = "#F0F0F3")), 
        navigation = list(buttonOptions = list(symbolStroke = "#DDDDDD", 
            theme = list(fill = "#505053"))), legendBackgroundColor = "rgba(0, 0, 0, 0.5)", 
        background2 = "#505053", dataLabelsColor = "#B0B0B3", 
        textColor = "#C0C0C0", contrastTextColor = "#F0F0F3", 
        maskColor = "rgba(255,255,255,0.3)")
    theme <- structure(theme, class = "hc_theme")
    if (length(list(...)) > 0) {
        theme <- hc_theme_merge(theme, hc_theme(...))
    }
    theme
}




## Package Data

citytemp <- highcharter::citytemp		## City temperatures from a year

favorite_bars <- highcharter::favorite_bars		## Marshall's Favorite Bars

favorite_pies <- highcharter::favorite_pies		## Marshall's Favorite Pies

globaltemp <- highcharter::globaltemp		## globaltemp

pokemon <- highcharter::pokemon		## pokemon

stars <- highcharter::stars		## stars

unemployment <- highcharter::unemployment		## US Counties unemployment rate

uscountygeojson <- highcharter::uscountygeojson		## US Counties map in Geojson format (list)

usgeojson <- highcharter::usgeojson		## US States map in Geojson format (list)

vaccines <- highcharter::vaccines		## Vaccines

weather <- highcharter::weather		## Weather

worldgeojson <- highcharter::worldgeojson		## World map in Geojson format (list)



## Package Info

.skeleton_package_title = "A Wrapper for the 'Highcharts' Library"

.skeleton_package_version = "0.5.0"

.skeleton_package_depends = ""

.skeleton_package_imports = "htmlwidgets,magrittr,purrr,rlist,assertthat,zoo,dplyr,tibble,stringr,broom,xts,quantmod,tidyr,htmltools,jsonlite,igraph,lubridate"


## Internal

.skeleton_version = 5


## EOF