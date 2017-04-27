##
## Exported symobls in package `shiny`
##

## Exported package methods

passwordInput <- function (inputId, label, value = "", width = NULL, placeholder = NULL) 
{
    div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), label %AND% 
        tags$label(label, `for` = inputId), tags$input(id = inputId, 
        type = "password", class = "form-control", value = value, 
        placeholder = placeholder))
}


includeHTML <- htmltools::includeHTML # re-exported from htmltools package

knit_print.shiny.tag <- htmltools::knit_print.shiny.tag # re-exported from htmltools package

splitLayout <- function (..., cellWidths = NULL, cellArgs = list()) 
{
    children <- list(...)
    childIdx <- !nzchar(names(children) %OR% character(length(children)))
    attribs <- children[!childIdx]
    children <- children[childIdx]
    count <- length(children)
    if (length(cellWidths) == 0 || is.na(cellWidths)) {
        cellWidths <- sprintf("%.3f%%", 100/count)
    }
    cellWidths <- rep(cellWidths, length.out = count)
    cellWidths <- sapply(cellWidths, validateCssUnit)
    do.call(tags$div, c(list(class = "shiny-split-layout"), attribs, 
        mapply(children, cellWidths, FUN = function(x, w) {
            do.call(tags$div, c(list(style = sprintf("width: %s;", 
                w)), cellArgs, list(x)))
        }, SIMPLIFY = FALSE)))
}


fixedPage <- function (..., title = NULL, responsive = NULL, theme = NULL) 
{
    bootstrapPage(div(class = "container", ...), title = title, 
        responsive = responsive, theme = theme)
}


bookmarkButton <- function (label = "Bookmark...", icon = shiny::icon("link", lib = "glyphicon"), 
    title = "Bookmark this application's state and get a URL for sharing.", 
    ..., id = "._bookmark_") 
{
    actionButton(id, label, icon, title = title, ...)
}


setBookmarkExclude <- function (names = character(0), session = getDefaultReactiveDomain()) 
{
    session$setBookmarkExclude(names)
}


tabsetPanel <- function (..., id = NULL, selected = NULL, type = c("tabs", "pills"), 
    position = NULL) 
{
    if (!is.null(position)) {
        shinyDeprecated(msg = paste("tabsetPanel: argument 'position' is deprecated;", 
            "it has been discontinued in Bootstrap 3."), version = "0.10.2.2")
    }
    if (!is.null(id)) 
        selected <- restoreInput(id = id, default = selected)
    tabs <- list(...)
    type <- match.arg(type)
    tabset <- buildTabset(tabs, paste0("nav nav-", type), NULL, 
        id, selected)
    first <- tabset$navList
    second <- tabset$content
    tags$div(class = "tabbable", first, second)
}


animationOptions <- function (interval = 1000, loop = FALSE, playButton = NULL, pauseButton = NULL) 
{
    list(interval = interval, loop = loop, playButton = playButton, 
        pauseButton = pauseButton)
}


icon <- function (name, class = NULL, lib = "font-awesome") 
{
    prefixes <- list(`font-awesome` = "fa", glyphicon = "glyphicon")
    prefix <- prefixes[[lib]]
    if (is.null(prefix)) {
        stop("Unknown font library '", lib, "' specified. Must be one of ", 
            paste0("\"", names(prefixes), "\"", collapse = ", "))
    }
    iconClass <- ""
    if (!is.null(name)) 
        iconClass <- paste0(prefix, " ", prefix, "-", name)
    if (!is.null(class)) 
        iconClass <- paste(iconClass, class)
    iconTag <- tags$i(class = iconClass)
    if (lib == "font-awesome") {
        htmlDependencies(iconTag) <- htmlDependency("font-awesome", 
            "4.7.0", c(href = "shared/font-awesome"), stylesheet = "css/font-awesome.min.css")
    }
    iconTag
}


shinyUI <- function (ui) 
{
    .globals$ui <- list(ui)
    ui
}


code <- htmltools::code # re-exported from htmltools package

outputOptions <- function (x, name, ...) 
{
    if (!inherits(x, "shinyoutput")) {
        stop("x must be a shinyoutput object.")
    }
    if (!missing(name)) {
        name <- .subset2(x, "ns")(name)
    }
    else {
        name <- NULL
    }
    .subset2(x, "impl")$outputOptions(name, ...)
}


actionButton <- function (inputId, label, icon = NULL, width = NULL, ...) 
{
    value <- restoreInput(id = inputId, default = NULL)
    tags$button(id = inputId, style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), type = "button", 
        class = "btn btn-default action-button", `data-val` = value, 
        list(validateIcon(icon), label), ...)
}


freezeReactiveValue <- function (x, name) 
{
    domain <- getDefaultReactiveDomain()
    if (is.null(getDefaultReactiveDomain)) {
        stop("freezeReactiveValue() must be called when a default reactive domain is active.")
    }
    domain$freezeValue(x, name)
    invisible()
}


updateDateInput <- function (session, inputId, label = NULL, value = NULL, min = NULL, 
    max = NULL) 
{
    formatDate <- function(x) {
        if (is.null(x)) 
            return(NULL)
        format(as.Date(x), "%Y-%m-%d")
    }
    value <- formatDate(value)
    min <- formatDate(min)
    max <- formatDate(max)
    message <- dropNulls(list(label = label, value = value, min = min, 
        max = max))
    session$sendInputMessage(inputId, message)
}


verticalLayout <- function (..., fluid = TRUE) 
{
    lapply(list(...), function(row) {
        col <- column(12, row)
        if (fluid) 
            fluidRow(col)
        else fixedRow(col)
    })
}


registerInputHandler <- function (type, fun, force = FALSE) 
{
    if (inputHandlers$containsKey(type) && !force) {
        stop("There is already an input handler for type: ", 
            type)
    }
    inputHandlers$set(type, fun)
}


insertUI <- function (selector, where = c("beforeBegin", "afterBegin", "beforeEnd", 
    "afterEnd"), ui, multiple = FALSE, immediate = FALSE, session = getDefaultReactiveDomain()) 
{
    force(selector)
    force(ui)
    force(session)
    force(multiple)
    if (missing(where)) 
        where <- "beforeEnd"
    where <- match.arg(where)
    callback <- function() {
        session$sendInsertUI(selector = selector, multiple = multiple, 
            where = where, content = processDeps(ui, session))
    }
    if (!immediate) 
        session$onFlushed(callback, once = TRUE)
    else callback()
}


updateCheckboxInput <- function (session, inputId, label = NULL, value = NULL) 
{
    message <- dropNulls(list(label = label, value = value))
    session$sendInputMessage(inputId, message)
}


checkboxInput <- function (inputId, label, value = FALSE, width = NULL) 
{
    value <- restoreInput(id = inputId, default = value)
    inputTag <- tags$input(id = inputId, type = "checkbox")
    if (!is.null(value) && value) 
        inputTag$attribs$checked <- "checked"
    div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), div(class = "checkbox", 
        tags$label(inputTag, tags$span(label))))
}


plotPNG <- function (func, filename = tempfile(fileext = ".png"), width = 400, 
    height = 400, res = 72, ...) 
{
    if (capabilities("aqua")) {
        pngfun <- grDevices::png
    }
    else if ((getOption("shiny.usecairo") %OR% TRUE) && nchar(system.file(package = "Cairo"))) {
        pngfun <- Cairo::CairoPNG
    }
    else {
        pngfun <- grDevices::png
    }
    pngfun(filename = filename, width = width, height = height, 
        res = res, ...)
    op <- graphics::par(mar = rep(0, 4))
    tryCatch(graphics::plot.new(), finally = graphics::par(op))
    dv <- grDevices::dev.cur()
    on.exit(grDevices::dev.off(dv), add = TRUE)
    func()
    filename
}


bootstrapPage <- function (..., title = NULL, responsive = NULL, theme = NULL) 
{
    if (!is.null(responsive)) {
        shinyDeprecated("The 'responsive' argument is no longer used with Bootstrap 3.")
    }
    attachDependencies(tagList(if (!is.null(title)) 
        tags$head(tags$title(title)), if (!is.null(theme)) {
        tags$head(tags$link(rel = "stylesheet", type = "text/css", 
            href = theme))
    }, list(...)), bootstrapLib())
}


reactivePoll <- function (intervalMillis, session, checkFunc, valueFunc) 
{
    intervalMillis <- coerceToFunc(intervalMillis)
    rv <- reactiveValues(cookie = isolate(checkFunc()))
    observe({
        rv$cookie <- checkFunc()
        invalidateLater(intervalMillis(), session)
    })
    re <- reactive({
        rv$cookie
        valueFunc()
    }, label = NULL)
    return(re)
}


imageOutput <- function (outputId, width = "100%", height = "400px", click = NULL, 
    dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
    brush = NULL, clickId = NULL, hoverId = NULL, inline = FALSE) 
{
    if (!is.null(clickId)) {
        shinyDeprecated(msg = paste("The 'clickId' argument is deprecated. ", 
            "Please use 'click' instead. ", "See ?imageOutput or ?plotOutput for more information."), 
            version = "0.11.1")
        click <- clickId
    }
    if (!is.null(hoverId)) {
        shinyDeprecated(msg = paste("The 'hoverId' argument is deprecated. ", 
            "Please use 'hover' instead. ", "See ?imageOutput or ?plotOutput for more information."), 
            version = "0.11.1")
        hover <- hoverId
    }
    if (!is.null(hoverDelay) || !is.null(hoverDelayType)) {
        shinyDeprecated(msg = paste("The 'hoverDelay'and 'hoverDelayType' arguments are deprecated. ", 
            "Please use 'hoverOpts' instead. ", "See ?imageOutput or ?plotOutput for more information."), 
            version = "0.11.1")
        hover <- hoverOpts(id = hover, delay = hoverDelay, delayType = hoverDelayType)
    }
    style <- if (!inline) {
        paste("width:", validateCssUnit(width), ";", "height:", 
            validateCssUnit(height))
    }
    args <- list(id = outputId, class = "shiny-image-output", 
        style = style)
    formatOptNames <- function(opts, prefix) {
        newNames <- paste("data", prefix, names(opts), sep = "-")
        newNames <- gsub("([A-Z])", "-\\L\\1", newNames, perl = TRUE)
        names(opts) <- newNames
        opts
    }
    if (!is.null(click)) {
        if (is.character(click)) {
            click <- clickOpts(id = click)
        }
        args <- c(args, formatOptNames(click, "click"))
    }
    if (!is.null(dblclick)) {
        if (is.character(dblclick)) {
            dblclick <- clickOpts(id = dblclick)
        }
        args <- c(args, formatOptNames(dblclick, "dblclick"))
    }
    if (!is.null(hover)) {
        if (is.character(hover)) {
            hover <- hoverOpts(id = hover)
        }
        args <- c(args, formatOptNames(hover, "hover"))
    }
    if (!is.null(brush)) {
        if (is.character(brush)) {
            brush <- brushOpts(id = brush)
        }
        args <- c(args, formatOptNames(brush, "brush"))
    }
    container <- if (inline) 
        span
    else div
    do.call(container, args)
}


includeScript <- htmltools::includeScript # re-exported from htmltools package

fileInput <- function (inputId, label, multiple = FALSE, accept = NULL, width = NULL) 
{
    restoredValue <- restoreInput(id = inputId, default = NULL)
    if (!is.null(restoredValue) && !is.data.frame(restoredValue)) {
        warning("Restored value for ", inputId, " has incorrect format.")
        restoredValue <- NULL
    }
    if (!is.null(restoredValue)) {
        restoredValue <- toJSON(restoredValue, strict_atomic = FALSE)
    }
    inputTag <- tags$input(id = inputId, name = inputId, type = "file", 
        style = "display: none;", `data-restore` = restoredValue)
    if (multiple) 
        inputTag$attribs$multiple <- "multiple"
    if (length(accept) > 0) 
        inputTag$attribs$accept <- paste(accept, collapse = ",")
    div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), label %AND% 
        tags$label(label), div(class = "input-group", tags$label(class = "input-group-btn", 
        span(class = "btn btn-default btn-file", "Browse...", 
            inputTag)), tags$input(type = "text", class = "form-control", 
        placeholder = "No file selected", readonly = "readonly")), 
        tags$div(id = paste(inputId, "_progress", sep = ""), 
            class = "progress progress-striped active shiny-file-input-progress", 
            tags$div(class = "progress-bar")))
}


reactiveTable <- function (func, ...) 
{
    shinyDeprecated(new = "renderTable")
    renderTable({
        func()
    })
}


knit_print.reactive <- function (x, ..., inline = FALSE) 
{
    renderFunc <- if (inline) 
        renderText
    else renderPrint
    knitr::knit_print(renderFunc({
        x()
    }), inline = inline)
}


installExprFunction <- function (expr, name, eval.env = parent.frame(2), quoted = FALSE, 
    assign.env = parent.frame(1), label = deparse(sys.call(-1)[[1]]), 
    wrappedWithLabel = TRUE, ..stacktraceon = FALSE) 
{
    if (!quoted) {
        quoted <- TRUE
        expr <- eval(substitute(substitute(expr)), parent.frame())
    }
    func <- exprToFunction(expr, eval.env, quoted)
    if (length(label) > 1) {
        label <- paste0(label, collapse = "\n")
    }
    if (wrappedWithLabel) {
        func <- wrapFunctionLabel(func, label, ..stacktraceon = ..stacktraceon)
    }
    else {
        registerDebugHook(name, assign.env, label)
    }
    assign(name, func, envir = assign.env)
}


updateTextAreaInput <- function (session, inputId, label = NULL, value = NULL) 
{
    message <- dropNulls(list(label = label, value = value))
    session$sendInputMessage(inputId, message)
}


renderPlot <- function (expr, width = "auto", height = "auto", res = 72, ..., 
    env = parent.frame(), quoted = FALSE, execOnResize = FALSE, 
    outputArgs = list()) 
{
    installExprFunction(expr, "func", env, quoted, ..stacktraceon = TRUE)
    args <- list(...)
    if (is.function(width)) 
        widthWrapper <- reactive({
            width()
        })
    else widthWrapper <- function() {
        width
    }
    if (is.function(height)) 
        heightWrapper <- reactive({
            height()
        })
    else heightWrapper <- function() {
        height
    }
    print.ggplot <- function(x) {
        grid::grid.newpage()
        build <- ggplot2::ggplot_build(x)
        gtable <- ggplot2::ggplot_gtable(build)
        grid::grid.draw(gtable)
        structure(list(build = build, gtable = gtable), class = "ggplot_build_gtable")
    }
    getDims <- function() {
        width <- widthWrapper()
        height <- heightWrapper()
        if (width == "auto") 
            width <- session$clientData[[paste0("output_", outputName, 
                "_width")]]
        if (height == "auto") 
            height <- session$clientData[[paste0("output_", outputName, 
                "_height")]]
        list(width = width, height = height)
    }
    session <- NULL
    outputName <- NULL
    renderFunc <- function(shinysession, name, ...) {
        session <<- shinysession
        outputName <<- name
        dims <- getDims()
        if (is.null(dims$width) || is.null(dims$height) || dims$width <= 
            0 || dims$height <= 0) {
            return(NULL)
        }
        plotData <- plotObj()
        img <- plotData$img
        if (dims$width != img$width || dims$height != img$height) {
            pixelratio <- session$clientData$pixelratio %OR% 
                1
            coordmap <- NULL
            plotFunc <- function() {
                ..stacktraceon..(grDevices::replayPlot(plotData$recordedPlot))
                if (inherits(plotData$plotResult, "ggplot_build_gtable")) {
                  coordmap <<- getGgplotCoordmap(plotData$plotResult, 
                    pixelratio, res)
                }
                else {
                  coordmap <<- getPrevPlotCoordmap(dims$width, 
                    dims$height)
                }
            }
            outfile <- ..stacktraceoff..(plotPNG(plotFunc, width = dims$width * 
                pixelratio, height = dims$height * pixelratio, 
                res = res * pixelratio))
            on.exit(unlink(outfile))
            img <- dropNulls(list(src = session$fileUrl(name, 
                outfile, contentType = "image/png"), width = dims$width, 
                height = dims$height, coordmap = coordmap, error = attr(coordmap, 
                  "error", exact = TRUE)))
        }
        img
    }
    plotObj <- reactive(label = "plotObj", {
        if (execOnResize) {
            dims <- getDims()
        }
        else {
            isolate({
                dims <- getDims()
            })
        }
        if (is.null(dims$width) || is.null(dims$height) || dims$width <= 
            0 || dims$height <= 0) {
            return(NULL)
        }
        pixelratio <- session$clientData$pixelratio %OR% 1
        plotResult <- NULL
        recordedPlot <- NULL
        coordmap <- NULL
        plotFunc <- function() {
            success <- FALSE
            tryCatch({
                grDevices::dev.control(displaylist = "enable")
                result <- withVisible(func())
                success <- TRUE
            }, finally = {
                if (!success) {
                  getDims()
                }
            })
            if (result$visible) {
                utils::capture.output({
                  plotResult <<- ..stacktraceon..(print(result$value))
                })
            }
            recordedPlot <<- grDevices::recordPlot()
            if (inherits(plotResult, "ggplot_build_gtable")) {
                coordmap <<- getGgplotCoordmap(plotResult, pixelratio, 
                  res)
            }
            else {
                coordmap <<- getPrevPlotCoordmap(dims$width, 
                  dims$height)
            }
        }
        outfile <- ..stacktraceoff..(do.call(plotPNG, c(plotFunc, 
            width = dims$width * pixelratio, height = dims$height * 
                pixelratio, res = res * pixelratio, args)))
        on.exit(unlink(outfile))
        list(img = dropNulls(list(src = session$fileUrl(outputName, 
            outfile, contentType = "image/png"), width = dims$width, 
            height = dims$height, coordmap = coordmap, error = attr(coordmap, 
                "error", exact = TRUE))), plotResult = plotResult, 
            recordedPlot = recordedPlot)
    })
    outputFunc <- plotOutput
    if (!identical(height, "auto")) 
        formals(outputFunc)["height"] <- list(NULL)
    markRenderFunction(outputFunc, renderFunc, outputArgs = outputArgs)
}


knit_print.shiny.render.function <- function (x, ..., inline = FALSE) 
{
    x <- htmltools::as.tags(x, inline = inline)
    output <- knitr::knit_print(tagList(x))
    attr(output, "knit_cacheable") <- FALSE
    attr(output, "knit_meta") <- append(attr(output, "knit_meta"), 
        shiny_rmd_warning())
    output
}


textAreaInput <- function (inputId, label, value = "", width = NULL, height = NULL, 
    cols = NULL, rows = NULL, placeholder = NULL, resize = NULL) 
{
    value <- restoreInput(id = inputId, default = value)
    if (!is.null(resize)) {
        resize <- match.arg(resize, c("both", "none", "vertical", 
            "horizontal"))
    }
    style <- paste(if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), if (!is.null(height)) 
        paste0("height: ", validateCssUnit(height), ";"), if (!is.null(resize)) 
        paste0("resize: ", resize, ";"))
    if (length(style) == 0) 
        style <- NULL
    div(class = "form-group shiny-input-container", label %AND% 
        tags$label(label, `for` = inputId), tags$textarea(id = inputId, 
        class = "form-control", placeholder = placeholder, style = style, 
        rows = rows, cols = cols, value))
}


selectInput <- function (inputId, label, choices, selected = NULL, multiple = FALSE, 
    selectize = TRUE, width = NULL, size = NULL) 
{
    selected <- restoreInput(id = inputId, default = selected)
    choices <- choicesWithNames(choices)
    if (is.null(selected)) {
        if (!multiple) 
            selected <- firstChoice(choices)
    }
    else selected <- validateSelected(selected, choices, inputId)
    if (!is.null(size) && selectize) {
        stop("'size' argument is incompatible with 'selectize=TRUE'.")
    }
    selectTag <- tags$select(id = inputId, class = if (!selectize) 
        "form-control", size = size, selectOptions(choices, selected))
    if (multiple) 
        selectTag$attribs$multiple <- "multiple"
    res <- div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), controlLabel(inputId, 
        label), div(selectTag))
    if (!selectize) 
        return(res)
    selectizeIt(inputId, res, NULL, nonempty = !multiple && !("" %in% 
        choices))
}


shinyServer <- function (func) 
{
    .globals$server <- list(func)
    invisible(func)
}


setProgress <- function (value = NULL, message = NULL, detail = NULL, session = getDefaultReactiveDomain()) 
{
    if (is.null(session$progressStack)) 
        stop("'session' is not a ShinySession object.")
    if (session$progressStack$size() == 0) {
        warning("setProgress was called outside of withProgress; ignoring")
        return()
    }
    session$progressStack$peek()$set(value, message, detail)
    invisible()
}


removeUI <- function (selector, multiple = FALSE, immediate = FALSE, session = getDefaultReactiveDomain()) 
{
    force(selector)
    force(multiple)
    force(session)
    callback <- function() {
        session$sendRemoveUI(selector = selector, multiple = multiple)
    }
    if (!immediate) 
        session$onFlushed(callback, once = TRUE)
    else callback()
}


removeModal <- function (session = getDefaultReactiveDomain()) 
{
    session$sendModal("remove", NULL)
}


sidebarPanel <- function (..., width = 4) 
{
    div(class = paste0("col-sm-", width), tags$form(class = "well", 
        ...))
}


..stacktraceon.. <- function (expr) 
expr


exprToFunction <- function (expr, env = parent.frame(), quoted = FALSE) 
{
    if (!quoted) {
        expr <- eval(substitute(substitute(expr)), parent.frame())
    }
    makeFunction(body = expr, env = env)
}


fluidPage <- function (..., title = NULL, responsive = NULL, theme = NULL) 
{
    bootstrapPage(div(class = "container-fluid", ...), title = title, 
        responsive = responsive, theme = theme)
}


runGist <- function (gist, destdir = NULL, ...) 
{
    gistUrl <- if (is.numeric(gist) || grepl("^[0-9a-f]+$", gist)) {
        sprintf("https://gist.github.com/%s/download", gist)
    }
    else if (grepl("^https://gist.github.com/([^/]+/)?([0-9a-f]+)$", 
        gist)) {
        paste(gist, "/download", sep = "")
    }
    else {
        stop("Unrecognized gist identifier format")
    }
    runUrl(gistUrl, filetype = ".zip", destdir = destdir, ...)
}


updateSelectizeInput <- function (session, inputId, label = NULL, choices = NULL, selected = NULL, 
    options = list(), server = FALSE) 
{
    if (length(options)) {
        res <- checkAsIs(options)
        cfg <- tags$script(type = "application/json", `data-for` = session$ns(inputId), 
            `data-eval` = if (length(res$eval)) 
                HTML(toJSON(res$eval)), HTML(toJSON(res$options)))
        session$sendInputMessage(inputId, list(config = as.character(cfg)))
    }
    if (!server) {
        return(updateSelectInput(session, inputId, label, choices, 
            selected))
    }
    value <- unname(selected)
    attr(choices, "selected_value") <- value
    message <- dropNulls(list(label = label, value = value, url = session$registerDataObj(inputId, 
        choices, selectizeJSON)))
    session$sendInputMessage(inputId, message)
}


updateQueryString <- function (queryString, session = getDefaultReactiveDomain()) 
{
    session$updateQueryString(queryString)
}


runUrl <- function (url, filetype = NULL, subdir = NULL, destdir = NULL, 
    ...) 
{
    if (!is.null(subdir) && ".." %in% strsplit(subdir, "/")[[1]]) 
        stop("'..' not allowed in subdir")
    if (is.null(filetype)) 
        filetype <- basename(url)
    if (grepl("\\.tar\\.gz$", filetype)) 
        fileext <- ".tar.gz"
    else if (grepl("\\.tar$", filetype)) 
        fileext <- ".tar"
    else if (grepl("\\.zip$", filetype)) 
        fileext <- ".zip"
    else stop("Unknown file extension.")
    message("Downloading ", url)
    if (is.null(destdir)) {
        filePath <- tempfile("shinyapp", fileext = fileext)
        fileDir <- tempfile("shinyapp")
    }
    else {
        fileDir <- destdir
        filePath <- paste(destdir, fileext)
    }
    dir.create(fileDir, showWarnings = FALSE)
    if (download(url, filePath, mode = "wb", quiet = TRUE) != 
        0) 
        stop("Failed to download URL ", url)
    on.exit(unlink(filePath))
    if (fileext %in% c(".tar", ".tar.gz")) {
        first <- untar2(filePath, list = TRUE)[1]
        untar2(filePath, exdir = fileDir)
    }
    else if (fileext == ".zip") {
        first <- as.character(utils::unzip(filePath, list = TRUE)$Name)[1]
        utils::unzip(filePath, exdir = fileDir)
    }
    if (is.null(destdir)) {
        on.exit(unlink(fileDir, recursive = TRUE), add = TRUE)
    }
    appdir <- file.path(fileDir, first)
    if (!utils::file_test("-d", appdir)) 
        appdir <- dirname(appdir)
    if (!is.null(subdir)) 
        appdir <- file.path(appdir, subdir)
    runApp(appdir, ...)
}


submitButton <- function (text = "Apply Changes", icon = NULL, width = NULL) 
{
    div(tags$button(type = "submit", class = "btn btn-primary", 
        style = if (!is.null(width)) 
            paste0("width: ", validateCssUnit(width), ";"), list(icon, 
            text)))
}


fillCol <- function (..., flex = 1, width = "100%", height = "100%") 
{
    flexfill(..., direction = "column", flex = flex, width = width, 
        height = height)
}


as.shiny.appobj <- function (x) 
{
    UseMethod("as.shiny.appobj", x)
}


includeMarkdown <- htmltools::includeMarkdown # re-exported from htmltools package

safeError <- function (error) 
{
    if (inherits(error, "character")) {
        error <- simpleError(error)
    }
    if (!inherits(error, "error")) {
        stop("The class of the `error` parameter must be either 'error' or 'character'")
    }
    class(error) <- c("shiny.custom.error", class(error))
    error
}


reactiveText <- function (func) 
{
    shinyDeprecated(new = "renderText")
    renderText({
        func()
    })
}


withReactiveDomain <- function (domain, expr) 
{
    oldValue <- .globals$domain
    .globals$domain <- domain
    on.exit(.globals$domain <- oldValue)
    expr
}


runGadget <- function (app, server = NULL, port = getOption("shiny.port"), 
    viewer = paneViewer(), stopOnCancel = TRUE) 
{
    if (!is.shiny.appobj(app)) {
        app <- shinyApp(app, server)
    }
    if (isTRUE(stopOnCancel)) {
        app <- decorateServerFunc(app, function(input, output, 
            session) {
            observeEvent(input$cancel, {
                stopApp(stop("User cancel", call. = FALSE))
            })
        })
    }
    if (is.null(viewer)) {
        viewer <- utils::browseURL
    }
    shiny::runApp(app, port = port, launch.browser = viewer)
}


addResourcePath <- function (prefix, directoryPath) 
{
    prefix <- prefix[1]
    if (!grepl("^[a-z0-9\\-_][a-z0-9\\-_.]*$", prefix, ignore.case = TRUE, 
        perl = TRUE)) {
        stop("addResourcePath called with invalid prefix; please see documentation")
    }
    if (prefix %in% c("shared")) {
        stop("addResourcePath called with the reserved prefix '", 
            prefix, "'; ", "please use a different prefix")
    }
    directoryPath <- normalizePath(directoryPath, mustWork = TRUE)
    existing <- .globals$resources[[prefix]]
    .globals$resources[[prefix]] <- list(directoryPath = directoryPath, 
        func = staticHandler(directoryPath))
}


fillRow <- function (..., flex = 1, width = "100%", height = "100%") 
{
    flexfill(..., direction = "row", flex = flex, width = width, 
        height = height)
}


textInput <- function (inputId, label, value = "", width = NULL, placeholder = NULL) 
{
    value <- restoreInput(id = inputId, default = value)
    div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), label %AND% 
        tags$label(label, `for` = inputId), tags$input(id = inputId, 
        type = "text", class = "form-control", value = value, 
        placeholder = placeholder))
}


pageWithSidebar <- function (headerPanel, sidebarPanel, mainPanel) 
{
    bootstrapPage(div(class = "container-fluid", div(class = "row", 
        headerPanel), div(class = "row", sidebarPanel, mainPanel)))
}


fixedRow <- function (...) 
{
    div(class = "row", ...)
}


conditionStackTrace <- function (cond) 
{
    attr(cond, "stack.trace", exact = TRUE)
}


downloadLink <- function (outputId, label = "Download", class = NULL, ...) 
{
    tags$a(id = outputId, class = paste(c("shiny-download-link", 
        class), collapse = " "), href = "", target = "_blank", 
        download = NA, label, ...)
}


invalidateLater <- function (millis, session = getDefaultReactiveDomain()) 
{
    ctx <- .getReactiveEnvironment()$currentContext()
    timerCallbacks$schedule(millis, function() {
        if (!is.null(session) && session$isClosed()) {
            return(invisible())
        }
        ctx$invalidate()
    })
    invisible()
}


stopApp <- function (returnValue = invisible()) 
{
    .globals$reterror <- FALSE
    ..stacktraceoff..(tryCatch({
        captureStackTraces(.globals$retval <- withVisible(..stacktraceon..(force(returnValue))))
    }, error = function(e) {
        .globals$retval <- e
        .globals$reterror <- TRUE
    }))
    .globals$stopped <- TRUE
    httpuv::interrupt()
}


dateInput <- function (inputId, label, value = NULL, min = NULL, max = NULL, 
    format = "yyyy-mm-dd", startview = "month", weekstart = 0, 
    language = "en", width = NULL) 
{
    if (inherits(value, "Date")) 
        value <- format(value, "%Y-%m-%d")
    if (inherits(min, "Date")) 
        min <- format(min, "%Y-%m-%d")
    if (inherits(max, "Date")) 
        max <- format(max, "%Y-%m-%d")
    value <- restoreInput(id = inputId, default = value)
    tags$div(id = inputId, class = "shiny-date-input form-group shiny-input-container", 
        style = if (!is.null(width)) 
            paste0("width: ", validateCssUnit(width), ";"), controlLabel(inputId, 
            label), tags$input(type = "text", class = "form-control", 
            `data-date-language` = language, `data-date-week-start` = weekstart, 
            `data-date-format` = format, `data-date-start-view` = startview, 
            `data-min-date` = min, `data-max-date` = max, `data-initial-date` = value), 
        datePickerDependency)
}


tags <- htmltools::tags # re-exported from htmltools package

onBookmark <- function (fun, session = getDefaultReactiveDomain()) 
{
    session$onBookmark(fun)
}


..stacktraceoff.. <- function (expr) 
expr


brushedPoints <- function (df, brush, xvar = NULL, yvar = NULL, panelvar1 = NULL, 
    panelvar2 = NULL, allRows = FALSE) 
{
    if (is.null(brush)) {
        if (allRows) 
            df$selected_ <- FALSE
        else df <- df[0, , drop = FALSE]
        return(df)
    }
    if (is.null(brush$xmin)) {
        stop("brushedPoints requires a brush object with xmin, xmax, ymin, and ymax.")
    }
    use_x <- grepl("x", brush$direction)
    use_y <- grepl("y", brush$direction)
    xvar <- xvar %OR% brush$mapping$x
    yvar <- yvar %OR% brush$mapping$y
    panelvar1 <- panelvar1 %OR% brush$mapping$panelvar1
    panelvar2 <- panelvar2 %OR% brush$mapping$panelvar2
    keep_rows <- rep(TRUE, nrow(df))
    if (use_x) {
        if (is.null(xvar)) 
            stop("brushedPoints: not able to automatically infer `xvar` from brush")
        x <- asNumber(df[[xvar]])
        keep_rows <- keep_rows & (x >= brush$xmin & x <= brush$xmax)
    }
    if (use_y) {
        if (is.null(yvar)) 
            stop("brushedPoints: not able to automatically infer `yvar` from brush")
        y <- asNumber(df[[yvar]])
        keep_rows <- keep_rows & (y >= brush$ymin & y <= brush$ymax)
    }
    if (!is.null(panelvar1)) 
        keep_rows <- keep_rows & panelMatch(brush$panelvar1, 
            df[[panelvar1]])
    if (!is.null(panelvar2)) 
        keep_rows <- keep_rows & panelMatch(brush$panelvar2, 
            df[[panelvar2]])
    if (allRows) {
        df$selected_ <- keep_rows
        df
    }
    else {
        df[keep_rows, , drop = FALSE]
    }
}


basicPage <- function (...) 
{
    bootstrapPage(div(class = "container-fluid", list(...)))
}


markRenderFunction <- function (uiFunc, renderFunc, outputArgs = list()) 
{
    hasExecuted <- Mutable$new()
    hasExecuted$set(FALSE)
    origRenderFunc <- renderFunc
    renderFunc <- function(...) {
        if (length(outputArgs) != 0 && !hasExecuted$get()) {
            warning("Unused argument: outputArgs. The argument outputArgs is only ", 
                "meant to be used when embedding snippets of Shiny code in an ", 
                "R Markdown code chunk (using runtime: shiny). When running a ", 
                "full Shiny app, please set the output arguments directly in ", 
                "the corresponding output function of your UI code.")
            hasExecuted$set(TRUE)
        }
        if (is.null(formals(origRenderFunc))) 
            origRenderFunc()
        else origRenderFunc(...)
    }
    structure(renderFunc, class = c("shiny.render.function", 
        "function"), outputFunc = uiFunc, outputArgs = outputArgs, 
        hasExecuted = hasExecuted)
}


runApp <- function (appDir = getwd(), port = getOption("shiny.port"), launch.browser = getOption("shiny.launch.browser", 
    interactive()), host = getOption("shiny.host", "127.0.0.1"), 
    workerId = "", quiet = FALSE, display.mode = c("auto", "normal", 
        "showcase"), test.mode = getOption("shiny.testmode", 
        FALSE)) 
{
    on.exit({
        handlerManager$clear()
    }, add = TRUE)
    if (.globals$running) {
        stop("Can't call `runApp()` from within `runApp()`. If your ,", 
            "application code contains `runApp()`, please remove it.")
    }
    .globals$running <- TRUE
    on.exit({
        .globals$running <- FALSE
    }, add = TRUE)
    oldOptionSet <- .globals$options
    on.exit({
        .globals$options <- oldOptionSet
    }, add = TRUE)
    ops <- options(warn = 1, pool.scheduler = scheduleTask)
    on.exit(options(ops), add = TRUE)
    appParts <- as.shiny.appobj(appDir)
    appOps <- appParts$options
    findVal <- function(arg, default) {
        if (arg %in% names(appOps)) 
            appOps[[arg]]
        else default
    }
    if (missing(port)) 
        port <- findVal("port", port)
    if (missing(launch.browser)) 
        launch.browser <- findVal("launch.browser", launch.browser)
    if (missing(host)) 
        host <- findVal("host", host)
    if (missing(quiet)) 
        quiet <- findVal("quiet", quiet)
    if (missing(display.mode)) 
        display.mode <- findVal("display.mode", display.mode)
    if (missing(test.mode)) 
        test.mode <- findVal("test.mode", test.mode)
    if (is.null(host) || is.na(host)) 
        host <- "0.0.0.0"
    workerId(workerId)
    if (inShinyServer()) {
        ver <- Sys.getenv("SHINY_SERVER_VERSION")
        if (utils::compareVersion(ver, .shinyServerMinVersion) < 
            0) {
            warning("Shiny Server v", .shinyServerMinVersion, 
                " or later is required; please upgrade!")
        }
    }
    setShowcaseDefault(0)
    .globals$testMode <- test.mode
    if (test.mode) {
        message("Running application in test mode.")
    }
    if (is.character(appDir)) {
        desc <- file.path.ci(if (tolower(tools::file_ext(appDir)) == 
            "r") 
            dirname(appDir)
        else appDir, "DESCRIPTION")
        if (file.exists(desc)) {
            con <- file(desc, encoding = checkEncoding(desc))
            on.exit(close(con), add = TRUE)
            settings <- read.dcf(con)
            if ("DisplayMode" %in% colnames(settings)) {
                mode <- settings[1, "DisplayMode"]
                if (mode == "Showcase") {
                  setShowcaseDefault(1)
                  if ("IncludeWWW" %in% colnames(settings)) {
                    .globals$IncludeWWW <- as.logical(settings[1, 
                      "IncludeWWW"])
                    if (is.na(.globals$IncludeWWW)) {
                      stop("In your Description file, `IncludeWWW` ", 
                        "must be set to `True` (default) or `False`")
                    }
                  }
                  else {
                    .globals$IncludeWWW <- TRUE
                  }
                }
            }
        }
    }
    if (is.null(.globals$IncludeWWW) || is.na(.globals$IncludeWWW)) {
        .globals$IncludeWWW <- TRUE
    }
    display.mode <- match.arg(display.mode)
    if (display.mode == "normal") {
        setShowcaseDefault(0)
    }
    else if (display.mode == "showcase") {
        setShowcaseDefault(1)
    }
    require(shiny)
    if (is.null(port)) {
        for (i in 1:20) {
            if (!is.null(.globals$lastPort)) {
                port <- .globals$lastPort
                .globals$lastPort <- NULL
            }
            else {
                while (TRUE) {
                  port <- p_randomInt(3000, 8000)
                  if (!port %in% c(3659, 4045, 6000, 6665:6669)) {
                    break
                  }
                }
            }
            tmp <- try(startServer(host, port, list()), silent = TRUE)
            if (!inherits(tmp, "try-error")) {
                stopServer(tmp)
                .globals$lastPort <- port
                break
            }
        }
    }
    unconsumeAppOptions(appParts$appOptions)
    if (!is.null(appParts$onEnd)) 
        on.exit(appParts$onEnd(), add = TRUE)
    if (!is.null(appParts$onStart)) 
        appParts$onStart()
    server <- startApp(appParts, port, host, quiet)
    on.exit({
        stopServer(server)
    }, add = TRUE)
    if (!is.character(port)) {
        browseHost <- if (identical(host, "0.0.0.0")) 
            "127.0.0.1"
        else host
        appUrl <- paste("http://", browseHost, ":", port, sep = "")
        if (is.function(launch.browser)) 
            launch.browser(appUrl)
        else if (launch.browser) 
            utils::browseURL(appUrl)
    }
    else {
        appUrl <- NULL
    }
    callAppHook("onAppStart", appUrl)
    on.exit({
        callAppHook("onAppStop", appUrl)
    }, add = TRUE)
    .globals$reterror <- NULL
    .globals$retval <- NULL
    .globals$stopped <- FALSE
    ..stacktraceoff..(captureStackTraces({
        scheduleFlush()
        while (!.globals$stopped) {
            serviceApp()
            Sys.sleep(0.001)
        }
    }))
    if (isTRUE(.globals$reterror)) {
        stop(.globals$retval)
    }
    else if (.globals$retval$visible) 
        .globals$retval$value
    else invisible(.globals$retval$value)
}


serverInfo <- function () 
{
    .globals$serverInfo
}


renderPrint <- function (expr, env = parent.frame(), quoted = FALSE, width = getOption("width"), 
    outputArgs = list()) 
{
    installExprFunction(expr, "func", env, quoted)
    renderFunc <- function(shinysession, name, ...) {
        op <- options(width = width)
        on.exit(options(op), add = TRUE)
        paste(utils::capture.output(func()), collapse = "\n")
    }
    markRenderFunction(verbatimTextOutput, renderFunc, outputArgs = outputArgs)
}


navbarPage <- function (title, ..., id = NULL, selected = NULL, position = c("static-top", 
    "fixed-top", "fixed-bottom"), header = NULL, footer = NULL, 
    inverse = FALSE, collapsible = FALSE, collapsable, fluid = TRUE, 
    responsive = NULL, theme = NULL, windowTitle = title) 
{
    if (!missing(collapsable)) {
        shinyDeprecated("`collapsable` is deprecated; use `collapsible` instead.")
        collapsible <- collapsable
    }
    pageTitle <- title
    navbarClass <- "navbar navbar-default"
    position <- match.arg(position)
    if (!is.null(position)) 
        navbarClass <- paste(navbarClass, " navbar-", position, 
            sep = "")
    if (inverse) 
        navbarClass <- paste(navbarClass, "navbar-inverse")
    if (!is.null(id)) 
        selected <- restoreInput(id = id, default = selected)
    tabs <- list(...)
    tabset <- buildTabset(tabs, "nav navbar-nav", NULL, id, selected)
    className <- function(name) {
        if (fluid) 
            paste(name, "-fluid", sep = "")
        else name
    }
    if (collapsible) {
        navId <- paste("navbar-collapse-", p_randomInt(1000, 
            10000), sep = "")
        containerDiv <- div(class = className("container"), div(class = "navbar-header", 
            tags$button(type = "button", class = "navbar-toggle collapsed", 
                `data-toggle` = "collapse", `data-target` = paste0("#", 
                  navId), span(class = "sr-only", "Toggle navigation"), 
                span(class = "icon-bar"), span(class = "icon-bar"), 
                span(class = "icon-bar")), span(class = "navbar-brand", 
                pageTitle)), div(class = "navbar-collapse collapse", 
            id = navId, tabset$navList))
    }
    else {
        containerDiv <- div(class = className("container"), div(class = "navbar-header", 
            span(class = "navbar-brand", pageTitle)), tabset$navList)
    }
    contentDiv <- div(class = className("container"))
    if (!is.null(header)) 
        contentDiv <- tagAppendChild(contentDiv, div(class = "row", 
            header))
    contentDiv <- tagAppendChild(contentDiv, tabset$content)
    if (!is.null(footer)) 
        contentDiv <- tagAppendChild(contentDiv, div(class = "row", 
            footer))
    bootstrapPage(title = windowTitle, responsive = responsive, 
        theme = theme, tags$nav(class = navbarClass, role = "navigation", 
            containerDiv), contentDiv)
}


dblclickOpts <- function (id = NULL, clip = TRUE, delay = 400) 
{
    if (is.null(id)) 
        stop("id must not be NULL")
    list(id = id, clip = clip, delay = delay)
}


onReactiveDomainEnded <- function (domain, callback, failIfNull = FALSE) 
{
    if (is.null(domain)) {
        if (isTRUE(failIfNull)) 
            stop("onReactiveDomainEnded called with null domain and failIfNull=TRUE")
        else return()
    }
    domain$onEnded(callback)
}


enableBookmarking <- function (store = c("url", "server", "disable")) 
{
    store <- match.arg(store)
    shinyOptions(bookmarkStore = store)
}


printStackTrace <- function (cond, full = getOption("shiny.fullstacktrace", FALSE), 
    offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
    stackTrace <- attr(cond, "stack.trace", exact = TRUE)
    tryCatch(if (!is.null(stackTrace)) {
        message(paste0("Stack trace (innermost first):\n", paste0(collapse = "\n", 
            formatStackTrace(stackTrace, full = full, offset = offset, 
                indent = "    "))))
    }
    else {
        message("No stack trace available")
    }, error = function(cond) {
        warning("Failed to write stack trace: ", cond)
    })
    invisible()
}


is.singleton <- htmltools::is.singleton # re-exported from htmltools package

observeEvent <- function (eventExpr, handlerExpr, event.env = parent.frame(), 
    event.quoted = FALSE, handler.env = parent.frame(), handler.quoted = FALSE, 
    label = NULL, suspended = FALSE, priority = 0, domain = getDefaultReactiveDomain(), 
    autoDestroy = TRUE, ignoreNULL = TRUE, ignoreInit = FALSE, 
    once = FALSE) 
{
    eventFunc <- exprToFunction(eventExpr, event.env, event.quoted)
    if (is.null(label)) 
        label <- sprintf("observeEvent(%s)", paste(deparse(body(eventFunc)), 
            collapse = "\n"))
    eventFunc <- wrapFunctionLabel(eventFunc, "observeEventExpr", 
        ..stacktraceon = TRUE)
    handlerFunc <- exprToFunction(handlerExpr, handler.env, handler.quoted)
    handlerFunc <- wrapFunctionLabel(handlerFunc, "observeEventHandler", 
        ..stacktraceon = TRUE)
    initialized <- FALSE
    o <- observe({
        e <- eventFunc()
        if (ignoreInit && !initialized) {
            initialized <<- TRUE
            return()
        }
        if (ignoreNULL && isNullEvent(e)) {
            return()
        }
        if (once) {
            on.exit(o$destroy())
        }
        isolate(handlerFunc())
    }, label = label, suspended = suspended, priority = priority, 
        domain = domain, autoDestroy = TRUE, ..stacktraceon = FALSE)
    invisible(o)
}


showReactLog <- function (time = TRUE) 
{
    utils::browseURL(renderReactLog(time = as.logical(time)))
}


modalButton <- function (label, icon = NULL) 
{
    tags$button(type = "button", class = "btn btn-default", `data-dismiss` = "modal", 
        validateIcon(icon), label)
}


parseQueryString <- function (str, nested = FALSE) 
{
    if (is.null(str) || nchar(str) == 0) 
        return(list())
    if (substr(str, 1, 1) == "?") 
        str <- substr(str, 2, nchar(str))
    pairs <- strsplit(str, "&", fixed = TRUE)[[1]]
    pairs <- pairs[pairs != ""]
    pairs <- strsplit(pairs, "=", fixed = TRUE)
    keys <- vapply(pairs, function(x) x[1], FUN.VALUE = character(1))
    values <- vapply(pairs, function(x) x[2], FUN.VALUE = character(1))
    values[is.na(values)] <- ""
    keys <- gsub("+", " ", keys, fixed = TRUE)
    values <- gsub("+", " ", values, fixed = TRUE)
    keys <- URLdecode(keys)
    values <- URLdecode(values)
    res <- stats::setNames(as.list(values), keys)
    if (!nested) 
        return(res)
    for (i in grep("\\[.+\\]", keys)) {
        k <- strsplit(keys[i], "[][]")[[1L]]
        res <- assignNestedList(res, k[k != ""], values[i])
        res[[keys[i]]] <- NULL
    }
    res
}


isTruthy <- function (x) 
{
    if (inherits(x, "try-error")) 
        return(FALSE)
    if (!is.atomic(x)) 
        return(TRUE)
    if (is.null(x)) 
        return(FALSE)
    if (length(x) == 0) 
        return(FALSE)
    if (all(is.na(x))) 
        return(FALSE)
    if (is.character(x) && !any(nzchar(stats::na.omit(x)))) 
        return(FALSE)
    if (inherits(x, "shinyActionButtonValue") && x == 0) 
        return(FALSE)
    if (is.logical(x) && !any(stats::na.omit(x))) 
        return(FALSE)
    return(TRUE)
}


req <- function (..., cancelOutput = FALSE) 
{
    dotloop(function(item) {
        if (!isTruthy(item)) {
            if (isTRUE(cancelOutput)) {
                cancelOutput()
            }
            else {
                reactiveStop(class = "validation")
            }
        }
    }, ...)
    if (!missing(..1)) 
        ..1
    else invisible()
}


withTags <- htmltools::withTags # re-exported from htmltools package

createWebDependency <- function (dependency) 
{
    if (is.null(dependency)) 
        return(NULL)
    if (!inherits(dependency, "html_dependency")) 
        stop("Unexpected non-html_dependency type")
    if (is.null(dependency$src$href)) {
        prefix <- paste(dependency$name, "-", dependency$version, 
            sep = "")
        addResourcePath(prefix, dependency$src$file)
        dependency$src$href <- prefix
    }
    return(dependency)
}


throttle <- function (r, millis, priority = 100, domain = getDefaultReactiveDomain()) 
{
    force(r)
    force(millis)
    if (!is.function(millis)) {
        origMillis <- millis
        millis <- function() origMillis
    }
    v <- reactiveValues(trigger = 0, lastTriggeredAt = NULL, 
        pending = FALSE)
    blackoutMillisLeft <- function() {
        if (is.null(v$lastTriggeredAt)) {
            0
        }
        else {
            max(0, (v$lastTriggeredAt + millis()/1000) - Sys.time()) * 
                1000
        }
    }
    trigger <- function() {
        v$lastTriggeredAt <- Sys.time()
        v$trigger <- isolate(v$trigger)%%999999999 + 1
        v$pending <- FALSE
    }
    observeEvent(r(), {
        if (v$pending) {
        }
        else if (blackoutMillisLeft() > 0) {
            v$pending <- TRUE
        }
        else {
            trigger()
        }
    }, label = "throttle tracker", ignoreNULL = FALSE, priority = priority, 
        domain = domain)
    observe({
        if (!v$pending) {
            return()
        }
        timeout <- blackoutMillisLeft()
        if (timeout > 0) {
            invalidateLater(timeout)
        }
        else {
            trigger()
        }
    }, priority = priority, domain = domain)
    eventReactive(v$trigger, {
        r()
    }, label = "throttle result", ignoreNULL = FALSE, domain = domain)
}


img <- htmltools::img # re-exported from htmltools package

extractStackTrace <- function (calls, full = getOption("shiny.fullstacktrace", FALSE), 
    offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
    srcrefs <- getSrcRefs(calls)
    if (offset) {
        srcrefs <- c(utils::tail(srcrefs, -1), list(NULL))
    }
    calls <- setSrcRefs(calls, srcrefs)
    callnames <- getCallNames(calls)
    if (full) {
        toShow <- rep.int(TRUE, length(calls))
    }
    else {
        hideable <- callnames %in% c("stop", ".handleSimpleError", 
            "h")
        lastGoodCall <- max(which(!hideable))
        toRemove <- length(calls) - lastGoodCall
        if (toRemove > 0 && toRemove < 5) {
            calls <- utils::head(calls, -toRemove)
            callnames <- utils::head(callnames, -toRemove)
        }
        score <- rep.int(0, length(callnames))
        score[callnames == "..stacktraceoff.."] <- -1
        score[callnames == "..stacktraceon.."] <- 1
        toShow <- (1 + cumsum(score)) > 0 & !(callnames %in% 
            c("..stacktraceon..", "..stacktraceoff.."))
    }
    calls <- calls[toShow]
    calls <- rev(calls)
    index <- rev(which(toShow))
    width <- floor(log10(max(index))) + 1
    data.frame(num = index, call = getCallNames(calls), loc = getLocs(calls), 
        stringsAsFactors = FALSE)
}


selectizeInput <- function (inputId, ..., options = NULL, width = NULL) 
{
    selectizeIt(inputId, selectInput(inputId, ..., selectize = FALSE, 
        width = width), options)
}


removeNotification <- function (id = NULL, session = getDefaultReactiveDomain()) 
{
    if (is.null(id)) {
        stop("id is required.")
    }
    session$sendNotification("remove", id)
    id
}


getShinyOption <- function (name, default = NULL) 
{
    name <- as.character(name)
    if (name %in% names(.globals$options)) 
        .globals$options[[name]]
    else default
}


reactivePlot <- function (func, width = "auto", height = "auto", ...) 
{
    shinyDeprecated(new = "renderPlot")
    renderPlot({
        func()
    }, width = width, height = height, ...)
}


tagAppendAttributes <- htmltools::tagAppendAttributes # re-exported from htmltools package

repeatable <- function (rngfunc, seed = stats::runif(1, 0, .Machine$integer.max)) 
{
    force(seed)
    function(...) {
        if (exists(".Random.seed", where = globalenv())) {
            currentSeed <- get(".Random.seed", pos = globalenv())
            on.exit(assign(".Random.seed", currentSeed, pos = globalenv()))
        }
        else {
            on.exit(rm(".Random.seed", pos = globalenv()))
        }
        set.seed(seed)
        rngfunc(...)
    }
}


navbarMenu <- function (title, ..., icon = NULL) 
{
    structure(list(title = title, tabs = list(...), iconClass = iconClass(icon)), 
        class = "shiny.navbarmenu")
}


knit_print.shiny.tag.list <- htmltools::knit_print.shiny.tag.list # re-exported from htmltools package

getDefaultReactiveDomain <- function () 
{
    .globals$domain
}


formatStackTrace <- function (calls, indent = "    ", full = getOption("shiny.fullstacktrace", 
    FALSE), offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
    st <- extractStackTrace(calls, full = full, offset = offset)
    if (nrow(st) == 0) {
        return(character(0))
    }
    width <- floor(log10(max(st$num))) + 1
    paste0(indent, formatC(st$num, width = width), ": ", st$call, 
        st$loc)
}


shinyAppFile <- function (appFile, options = list()) 
{
    appFile <- normalizePath(appFile, mustWork = TRUE)
    appDir <- dirname(appFile)
    shinyAppDir_appR(basename(appFile), appDir, options = options)
}


updateDateRangeInput <- function (session, inputId, label = NULL, start = NULL, end = NULL, 
    min = NULL, max = NULL) 
{
    if (inherits(start, "Date")) 
        start <- format(start, "%Y-%m-%d")
    if (inherits(end, "Date")) 
        end <- format(end, "%Y-%m-%d")
    if (inherits(min, "Date")) 
        min <- format(min, "%Y-%m-%d")
    if (inherits(max, "Date")) 
        max <- format(max, "%Y-%m-%d")
    message <- dropNulls(list(label = label, value = dropNulls(list(start = start, 
        end = end)), min = min, max = max))
    session$sendInputMessage(inputId, message)
}


reactive <- function (x, env = parent.frame(), quoted = FALSE, label = NULL, 
    domain = getDefaultReactiveDomain(), ..stacktraceon = TRUE) 
{
    fun <- exprToFunction(x, env, quoted)
    srcref <- attr(substitute(x), "srcref", exact = TRUE)
    if (is.null(label)) {
        label <- srcrefToLabel(srcref[[1]], sprintf("reactive(%s)", 
            paste(deparse(body(fun)), collapse = "\n")))
    }
    if (length(srcref) >= 2) 
        attr(label, "srcref") <- srcref[[2]]
    attr(label, "srcfile") <- srcFileOfRef(srcref[[1]])
    o <- Observable$new(fun, label, domain, ..stacktraceon = ..stacktraceon)
    structure(o$getValue, observable = o, class = "reactive")
}


knit_print.html <- htmltools::knit_print.html # re-exported from htmltools package

paneViewer <- function (minHeight = NULL) 
{
    viewer <- getOption("viewer")
    if (is.null(viewer)) {
        utils::browseURL
    }
    else {
        function(url) {
            viewer(url, minHeight)
        }
    }
}


tagAppendChild <- htmltools::tagAppendChild # re-exported from htmltools package

is.reactive <- function (x) 
inherits(x, "reactive")


column <- function (width, ..., offset = 0) 
{
    if (!is.numeric(width) || (width < 1) || (width > 12)) 
        stop("column width must be between 1 and 12")
    colClass <- paste0("col-sm-", width)
    if (offset > 0) 
        colClass <- paste0(colClass, " col-sm-offset-", offset)
    div(class = colClass, ...)
}


withMathJax <- function (...) 
{
    path <- "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
    tagList(tags$head(singleton(tags$script(src = path, type = "text/javascript"))), 
        ..., tags$script(HTML("if (window.MathJax) MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);")))
}


reactiveValues <- function (...) 
{
    args <- list(...)
    if ((length(args) > 0) && (is.null(names(args)) || any(names(args) == 
        ""))) 
        stop("All arguments passed to reactiveValues() must be named.")
    values <- .createReactiveValues(ReactiveValues$new())
    .subset2(values, "impl")$mset(args)
    values
}


runGitHub <- function (repo, username = getOption("github.user"), ref = "master", 
    subdir = NULL, destdir = NULL, ...) 
{
    if (grepl("/", repo)) {
        res <- strsplit(repo, "/")[[1]]
        if (length(res) != 2) 
            stop("'repo' must be of the form 'username/repo'")
        username <- res[1]
        repo <- res[2]
    }
    url <- paste("https://github.com/", username, "/", repo, 
        "/archive/", ref, ".tar.gz", sep = "")
    runUrl(url, subdir = subdir, destdir = destdir, ...)
}


is.reactivevalues <- function (x) 
inherits(x, "reactivevalues")


includeCSS <- htmltools::includeCSS # re-exported from htmltools package

renderImage <- function (expr, env = parent.frame(), quoted = FALSE, deleteFile = TRUE, 
    outputArgs = list()) 
{
    installExprFunction(expr, "func", env, quoted)
    renderFunc <- function(shinysession, name, ...) {
        imageinfo <- func()
        if (deleteFile) {
            on.exit(unlink(imageinfo$src))
        }
        contentType <- imageinfo$contentType %OR% getContentType(imageinfo$src)
        extra_attr <- imageinfo[!names(imageinfo) %in% c("src", 
            "contentType")]
        c(src = shinysession$fileUrl(name, file = imageinfo$src, 
            contentType = contentType), extra_attr)
    }
    markRenderFunction(imageOutput, renderFunc, outputArgs = outputArgs)
}


flowLayout <- function (..., cellArgs = list()) 
{
    children <- list(...)
    childIdx <- !nzchar(names(children) %OR% character(length(children)))
    attribs <- children[!childIdx]
    children <- children[childIdx]
    do.call(tags$div, c(list(class = "shiny-flow-layout"), attribs, 
        lapply(children, function(x) {
            do.call(tags$div, c(cellArgs, list(x)))
        })))
}


br <- htmltools::br # re-exported from htmltools package

showModal <- function (ui, session = getDefaultReactiveDomain()) 
{
    res <- processDeps(ui, session)
    session$sendModal("show", list(html = res$html, deps = res$deps))
}


singleton <- htmltools::singleton # re-exported from htmltools package

span <- htmltools::span # re-exported from htmltools package

sliderInput <- function (inputId, label, min, max, value, step = NULL, round = FALSE, 
    format = NULL, locale = NULL, ticks = TRUE, animate = FALSE, 
    width = NULL, sep = ",", pre = NULL, post = NULL, timeFormat = NULL, 
    timezone = NULL, dragRange = TRUE) 
{
    if (!missing(format)) {
        shinyDeprecated(msg = "The `format` argument to sliderInput is deprecated. Use `sep`, `pre`, and `post` instead.", 
            version = "0.10.2.2")
    }
    if (!missing(locale)) {
        shinyDeprecated(msg = "The `locale` argument to sliderInput is deprecated. Use `sep`, `pre`, and `post` instead.", 
            version = "0.10.2.2")
    }
    value <- restoreInput(id = inputId, default = value)
    findStepSize <- function(min, max, step) {
        if (!is.null(step)) 
            return(step)
        range <- max - min
        if (range < 2 || hasDecimals(min) || hasDecimals(max)) {
            step <- pretty(c(min, max), n = 100)
            step[2] - step[1]
        }
        else {
            1
        }
    }
    if (inherits(min, "Date")) {
        if (!inherits(max, "Date") || !inherits(value, "Date")) 
            stop("`min`, `max`, and `value must all be Date or non-Date objects")
        dataType <- "date"
        if (is.null(timeFormat)) 
            timeFormat <- "%F"
    }
    else if (inherits(min, "POSIXt")) {
        if (!inherits(max, "POSIXt") || !inherits(value, "POSIXt")) 
            stop("`min`, `max`, and `value must all be POSIXt or non-POSIXt objects")
        dataType <- "datetime"
        if (is.null(timeFormat)) 
            timeFormat <- "%F %T"
    }
    else {
        dataType <- "number"
    }
    step <- findStepSize(min, max, step)
    if (dataType %in% c("date", "datetime")) {
        to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
        step <- to_ms(max) - to_ms(max - step)
        min <- to_ms(min)
        max <- to_ms(max)
        value <- to_ms(value)
    }
    range <- max - min
    if (ticks) {
        n_steps <- range/step
        scale_factor <- ceiling(n_steps/10)
        n_ticks <- n_steps/scale_factor
    }
    else {
        n_ticks <- NULL
    }
    sliderProps <- dropNulls(list(class = "js-range-slider", 
        id = inputId, `data-type` = if (length(value) > 1) "double", 
        `data-min` = formatNoSci(min), `data-max` = formatNoSci(max), 
        `data-from` = formatNoSci(value[1]), `data-to` = if (length(value) > 
            1) formatNoSci(value[2]), `data-step` = formatNoSci(step), 
        `data-grid` = ticks, `data-grid-num` = n_ticks, `data-grid-snap` = FALSE, 
        `data-prefix` = pre, `data-postfix` = post, `data-keyboard` = TRUE, 
        `data-keyboard-step` = step/(max - min) * 100, `data-drag-interval` = dragRange, 
        `data-data-type` = dataType, `data-time-format` = timeFormat, 
        `data-timezone` = timezone))
    if (sep == "") {
        sliderProps$`data-prettify-enabled` <- "0"
    }
    else {
        sliderProps$`data-prettify-separator` <- sep
    }
    sliderProps <- lapply(sliderProps, function(x) {
        if (identical(x, TRUE)) 
            "true"
        else if (identical(x, FALSE)) 
            "false"
        else x
    })
    sliderTag <- div(class = "form-group shiny-input-container", 
        style = if (!is.null(width)) 
            paste0("width: ", validateCssUnit(width), ";"), if (!is.null(label)) 
            controlLabel(inputId, label), do.call(tags$input, 
            sliderProps))
    if (identical(animate, TRUE)) 
        animate <- animationOptions()
    if (!is.null(animate) && !identical(animate, FALSE)) {
        if (is.null(animate$playButton)) 
            animate$playButton <- icon("play", lib = "glyphicon")
        if (is.null(animate$pauseButton)) 
            animate$pauseButton <- icon("pause", lib = "glyphicon")
        sliderTag <- tagAppendChild(sliderTag, tags$div(class = "slider-animate-container", 
            tags$a(href = "#", class = "slider-animate-button", 
                `data-target-id` = inputId, `data-interval` = animate$interval, 
                `data-loop` = animate$loop, span(class = "play", 
                  animate$playButton), span(class = "pause", 
                  animate$pauseButton))))
    }
    dep <- list(htmlDependency("ionrangeslider", "2.1.2", c(href = "shared/ionrangeslider"), 
        script = "js/ion.rangeSlider.min.js", stylesheet = c("css/ion.rangeSlider.css", 
            "css/ion.rangeSlider.skinShiny.css")), htmlDependency("strftime", 
        "0.9.2", c(href = "shared/strftime"), script = "strftime-min.js"))
    attachDependencies(sliderTag, dep)
}


plotOutput <- function (outputId, width = "100%", height = "400px", click = NULL, 
    dblclick = NULL, hover = NULL, hoverDelay = NULL, hoverDelayType = NULL, 
    brush = NULL, clickId = NULL, hoverId = NULL, inline = FALSE) 
{
    res <- imageOutput(outputId, width, height, click, dblclick, 
        hover, hoverDelay, hoverDelayType, brush, clickId, hoverId, 
        inline)
    res$attribs$class <- "shiny-plot-output"
    res
}


reactivePrint <- function (func) 
{
    shinyDeprecated(new = "renderPrint")
    renderPrint({
        func()
    })
}


renderDataTable <- function (expr, options = NULL, searchDelay = 500, callback = "function(oTable) {}", 
    escape = TRUE, env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
    installExprFunction(expr, "func", env, quoted)
    renderFunc <- function(shinysession, name, ...) {
        if (is.function(options)) 
            options <- options()
        options <- checkDT9(options)
        res <- checkAsIs(options)
        data <- func()
        if (length(dim(data)) != 2) 
            return()
        if (is.data.frame(data)) 
            data <- as.data.frame(data)
        action <- shinysession$registerDataObj(name, data, dataTablesJSON)
        colnames <- colnames(data)
        if (is.character(escape)) {
            escape <- stats::setNames(seq_len(ncol(data)), colnames)[escape]
            if (any(is.na(escape))) 
                stop("Some column names in the 'escape' argument not found in data")
        }
        colnames[escape] <- htmlEscape(colnames[escape])
        if (!is.logical(escape)) {
            if (!is.numeric(escape)) 
                stop("'escape' must be TRUE, FALSE, or a numeric vector, or column names")
            escape <- paste(escape, collapse = ",")
        }
        list(colnames = colnames, action = action, options = res$options, 
            evalOptions = if (length(res$eval)) I(res$eval), 
            searchDelay = searchDelay, callback = paste(callback, 
                collapse = "\n"), escape = escape)
    }
    markRenderFunction(dataTableOutput, renderFunc, outputArgs = outputArgs)
}


a <- htmltools::a # re-exported from htmltools package

downloadButton <- function (outputId, label = "Download", class = NULL, ...) 
{
    aTag <- tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", 
        class), href = "", target = "_blank", download = NA, 
        icon("download"), label, ...)
}


tagList <- htmltools::tagList # re-exported from htmltools package

updateTextInput <- function (session, inputId, label = NULL, value = NULL) 
{
    message <- dropNulls(list(label = label, value = value))
    session$sendInputMessage(inputId, message)
}


strong <- htmltools::strong # re-exported from htmltools package

textOutput <- function (outputId, container = if (inline) span else div, inline = FALSE) 
{
    container(id = outputId, class = "shiny-text-output")
}


tableOutput <- function (outputId) 
{
    div(id = outputId, class = "shiny-html-output")
}


helpText <- function (...) 
{
    span(class = "help-block", ...)
}


browserViewer <- function (browser = getOption("browser")) 
{
    function(url) {
        utils::browseURL(url, browser = browser)
    }
}


mainPanel <- function (..., width = 8) 
{
    div(class = paste0("col-sm-", width), ...)
}


tabPanel <- function (title, ..., value = title, icon = NULL) 
{
    divTag <- div(class = "tab-pane", title = title, `data-value` = value, 
        `data-icon-class` = iconClass(icon), ...)
}


reactiveFileReader <- function (intervalMillis, session, filePath, readFunc, ...) 
{
    filePath <- coerceToFunc(filePath)
    extraArgs <- list(...)
    reactivePoll(intervalMillis, session, function() {
        path <- filePath()
        info <- file.info(path)
        return(paste(path, info$mtime, info$size))
    }, function() {
        do.call(readFunc, c(filePath(), extraArgs))
    })
}


tag <- htmltools::tag # re-exported from htmltools package

observe <- function (x, env = parent.frame(), quoted = FALSE, label = NULL, 
    suspended = FALSE, priority = 0, domain = getDefaultReactiveDomain(), 
    autoDestroy = TRUE, ..stacktraceon = TRUE) 
{
    fun <- exprToFunction(x, env, quoted)
    if (is.null(label)) 
        label <- sprintf("observe(%s)", paste(deparse(body(fun)), 
            collapse = "\n"))
    o <- Observer$new(fun, label = label, suspended = suspended, 
        priority = priority, domain = domain, autoDestroy = autoDestroy, 
        ..stacktraceon = ..stacktraceon)
    invisible(o)
}


knit_print.shiny.appobj <- function (x, ...) 
{
    opts <- x$options %OR% list()
    width <- if (is.null(opts$width)) 
        "100%"
    else opts$width
    height <- if (is.null(opts$height)) 
        "400"
    else opts$height
    runtime <- knitr::opts_knit$get("rmarkdown.runtime")
    if (!is.null(runtime) && runtime != "shiny") {
        width <- validateCssUnit(width)
        height <- validateCssUnit(height)
        output <- tags$div(style = paste("width:", width, "; height:", 
            height, "; text-align: center;", "box-sizing: border-box;", 
            "-moz-box-sizing: border-box;", "-webkit-box-sizing: border-box;"), 
            class = "muted well", "Shiny applications not supported in static R Markdown documents")
    }
    else {
        path <- addSubApp(x)
        output <- deferredIFrame(path, width, height)
    }
    knitr::asis_output(htmlPreserve(format(output, indent = FALSE)), 
        meta = shiny_rmd_warning(), cacheable = FALSE)
}


updateTabsetPanel <- function (session, inputId, selected = NULL) 
{
    message <- dropNulls(list(value = selected))
    session$sendInputMessage(inputId, message)
}


p <- htmltools::p # re-exported from htmltools package

Progress <- "<environment>"

bootstrapLib <- function (theme = NULL) 
{
    htmlDependency("bootstrap", "3.3.7", c(href = "shared/bootstrap", 
        file = system.file("www/shared/bootstrap", package = "shiny")), 
        script = c("js/bootstrap.min.js", "shim/html5shiv.min.js", 
            "shim/respond.min.js"), stylesheet = if (is.null(theme)) 
            "css/bootstrap.min.css", meta = list(viewport = "width=device-width, initial-scale=1"))
}


validateCssUnit <- htmltools::validateCssUnit # re-exported from htmltools package

HTML <- htmltools::HTML # re-exported from htmltools package

titlePanel <- function (title, windowTitle = title) 
{
    tagList(tags$head(tags$title(windowTitle)), h2(title))
}


h1 <- htmltools::h1 # re-exported from htmltools package

withLogErrors <- function (expr, full = getOption("shiny.fullstacktrace", FALSE), 
    offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
    withCallingHandlers(captureStackTraces(expr), error = function(cond) {
        if (inherits(cond, "shiny.silent.error")) 
            return()
        if (isTRUE(getOption("show.error.messages"))) {
            printError(cond, full = full, offset = offset)
        }
    })
}


showNotification <- function (ui, action = NULL, duration = 5, closeButton = TRUE, 
    id = NULL, type = c("default", "message", "warning", "error"), 
    session = getDefaultReactiveDomain()) 
{
    if (is.null(id)) 
        id <- createUniqueId(8)
    res <- processDeps(ui, session)
    actionRes <- processDeps(action, session)
    session$sendNotification("show", list(html = res$html, action = actionRes$html, 
        deps = c(res$deps, actionRes$deps), duration = if (!is.null(duration)) duration * 
            1000, closeButton = closeButton, id = id, type = match.arg(type)))
    id
}


h2 <- htmltools::h2 # re-exported from htmltools package

dialogViewer <- function (dialogName, width = 600, height = 600) 
{
    viewer <- getOption("shinygadgets.showdialog")
    if (is.null(viewer)) {
        utils::browseURL
    }
    else {
        function(url) {
            viewer(dialogName, url, width = width, height = height)
        }
    }
}


reactiveValuesToList <- function (x, all.names = FALSE) 
{
    res <- .subset2(x, "impl")$toList(all.names)
    prefix <- .subset2(x, "ns")("")
    if (nzchar(prefix)) {
        fullNames <- names(res)
        fullNames <- fullNames[substring(fullNames, 1, nchar(prefix)) == 
            prefix]
        res <- res[fullNames]
        names(res) <- substring(fullNames, nchar(prefix) + 1)
    }
    res
}


h3 <- htmltools::h3 # re-exported from htmltools package

h4 <- htmltools::h4 # re-exported from htmltools package

reactiveTimer <- function (intervalMs = 1000, session = getDefaultReactiveDomain()) 
{
    dependents <- Map$new()
    timerCallbacks$schedule(intervalMs, function() {
        if (!is.null(session) && session$isClosed()) {
            return(invisible())
        }
        timerCallbacks$schedule(intervalMs, sys.function())
        lapply(dependents$values(), function(dep.ctx) {
            dep.ctx$invalidate()
            NULL
        })
    })
    return(function() {
        ctx <- .getReactiveEnvironment()$currentContext()
        if (!dependents$containsKey(ctx$id)) {
            dependents$set(ctx$id, ctx)
            ctx$onInvalidate(function() {
                dependents$remove(ctx$id)
            })
        }
        return(Sys.time())
    })
}


h5 <- htmltools::h5 # re-exported from htmltools package

debounce <- function (r, millis, priority = 100, domain = getDefaultReactiveDomain()) 
{
    force(r)
    force(millis)
    if (!is.function(millis)) {
        origMillis <- millis
        millis <- function() origMillis
    }
    v <- reactiveValues(trigger = NULL, when = NULL)
    firstRun <- TRUE
    observe({
        r()
        if (firstRun) {
            firstRun <<- FALSE
            return()
        }
        v$when <- Sys.time() + millis()/1000
    }, label = "debounce tracker", domain = domain, priority = priority)
    observe({
        if (is.null(v$when)) 
            return()
        now <- Sys.time()
        if (now >= v$when) {
            v$trigger <- isolate(v$trigger %OR% 0)%%999999999 + 
                1
            v$when <- NULL
        }
        else {
            invalidateLater((v$when - now) * 1000)
        }
    }, label = "debounce timer", domain = domain, priority = priority)
    er <- eventReactive(v$trigger, {
        r()
    }, label = "debounce result", ignoreNULL = FALSE, domain = domain)
    primer <- observe({
        primer$destroy()
        er()
    }, label = "debounce primer", domain = domain, priority = priority)
    er
}


h6 <- htmltools::h6 # re-exported from htmltools package

removeInputHandler <- function (type) 
{
    inputHandlers$remove(type)
}


conditionalPanel <- function (condition, ...) 
{
    div(`data-display-if` = condition, ...)
}


is.shiny.appobj <- function (x) 
{
    inherits(x, "shiny.appobj")
}


tagAppendChildren <- htmltools::tagAppendChildren # re-exported from htmltools package

exportTestValues <- function (..., quoted_ = FALSE, env_ = parent.frame(), session_ = getDefaultReactiveDomain()) 
{
    session_$exportTestValues(..., quoted_ = quoted_, env_ = env_)
}


updateSelectInput <- function (session, inputId, label = NULL, choices = NULL, selected = NULL) 
{
    choices <- if (!is.null(choices)) 
        choicesWithNames(choices)
    if (!is.null(selected)) 
        selected <- validateSelected(selected, choices, inputId)
    options <- if (!is.null(choices)) 
        selectOptions(choices, selected)
    message <- dropNulls(list(label = label, options = options, 
        value = selected))
    session$sendInputMessage(inputId, message)
}


runExample <- function (example = NA, port = NULL, launch.browser = getOption("shiny.launch.browser", 
    interactive()), host = getOption("shiny.host", "127.0.0.1"), 
    display.mode = c("auto", "normal", "showcase")) 
{
    examplesDir <- system.file("examples", package = "shiny")
    dir <- resolve(examplesDir, example)
    if (is.null(dir)) {
        if (is.na(example)) {
            errFun <- message
            errMsg <- ""
        }
        else {
            errFun <- stop
            errMsg <- paste("Example", example, "does not exist. ")
        }
        errFun(errMsg, "Valid examples are \"", paste(list.files(examplesDir), 
            collapse = "\", \""), "\"")
    }
    else {
        runApp(dir, port = port, host = host, launch.browser = launch.browser, 
            display.mode = display.mode)
    }
}


onRestored <- function (fun, session = getDefaultReactiveDomain()) 
{
    session$onRestored(fun)
}


updateCheckboxGroupInput <- function (session, inputId, label = NULL, choices = NULL, selected = NULL, 
    inline = FALSE) 
{
    updateInputOptions(session, inputId, label, choices, selected, 
        inline)
}


em <- htmltools::em # re-exported from htmltools package

htmlOutput <- function (outputId, inline = FALSE, container = if (inline) span else div, 
    ...) 
{
    if (anyUnnamed(list(...))) {
        warning("Unnamed elements in ... will be replaced with dynamic UI.")
    }
    container(id = outputId, class = "shiny-html-output", ...)
}


need <- function (expr, message = paste(label, "must be provided"), label) 
{
    force(message)
    if (!isTruthy(expr)) 
        return(message)
    else return(invisible(NULL))
}


urlModal <- function (url, title = "Bookmarked application link", subtitle = NULL) 
{
    subtitleTag <- tagList(br(), span(class = "text-muted", subtitle), 
        span(id = "shiny-bookmark-copy-text", class = "text-muted"))
    modalDialog(title = title, easyClose = TRUE, tags$textarea(class = "form-control", 
        rows = "1", style = "resize: none;", readonly = "readonly", 
        url), subtitleTag, tags$script("$('#shiny-modal').\n        one('show.bs.modal', function() {\n          setTimeout(function() {\n            var $textarea = $('#shiny-modal textarea');\n            $textarea.innerHeight($textarea[0].scrollHeight);\n          }, 200);\n        });\n      $('#shiny-modal')\n        .one('shown.bs.modal', function() {\n          $('#shiny-modal textarea').select().focus();\n        });\n      $('#shiny-bookmark-copy-text')\n        .text(function() {\n          if (/Mac/i.test(navigator.userAgent)) {\n            return 'Press ⌘-C to copy.';\n          } else {\n            return 'Press Ctrl-C to copy.';\n          }\n        });\n      "))
}


div <- htmltools::div # re-exported from htmltools package

withProgress <- function (expr, min = 0, max = 1, value = min + (max - min) * 
    0.1, message = NULL, detail = NULL, style = getShinyOption("progress.style", 
    default = "notification"), session = getDefaultReactiveDomain(), 
    env = parent.frame(), quoted = FALSE) 
{
    if (!quoted) 
        expr <- substitute(expr)
    if (is.null(session$progressStack)) 
        stop("'session' is not a ShinySession object.")
    style <- match.arg(style, c("notification", "old"))
    p <- Progress$new(session, min = min, max = max, style = style)
    session$progressStack$push(p)
    on.exit({
        session$progressStack$pop()
        p$close()
    })
    p$set(value, message, detail)
    eval(expr, env)
}


NS <- function (namespace, id = NULL) 
{
    if (missing(id)) {
        function(id) {
            paste(c(namespace, id), collapse = ns.sep)
        }
    }
    else {
        paste(c(namespace, id), collapse = ns.sep)
    }
}


numericInput <- function (inputId, label, value, min = NA, max = NA, step = NA, 
    width = NULL) 
{
    value <- restoreInput(id = inputId, default = value)
    inputTag <- tags$input(id = inputId, type = "number", class = "form-control", 
        value = formatNoSci(value))
    if (!is.na(min)) 
        inputTag$attribs$min = min
    if (!is.na(max)) 
        inputTag$attribs$max = max
    if (!is.na(step)) 
        inputTag$attribs$step = step
    div(class = "form-group shiny-input-container", style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), label %AND% 
        tags$label(label, `for` = inputId), inputTag)
}


fluidRow <- function (...) 
{
    div(class = "row", ...)
}


renderUI <- function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
    installExprFunction(expr, "func", env, quoted)
    renderFunc <- function(shinysession, name, ...) {
        result <- func()
        if (is.null(result) || length(result) == 0) 
            return(NULL)
        processDeps(result, shinysession)
    }
    markRenderFunction(uiOutput, renderFunc, outputArgs = outputArgs)
}


updateNavbarPage <- function (session, inputId, selected = NULL) 
{
    message <- dropNulls(list(value = selected))
    session$sendInputMessage(inputId, message)
}


maskReactiveContext <- function (expr) 
{
    .getReactiveEnvironment()$runWith(NULL, function() {
        expr
    })
}


printError <- function (cond, full = getOption("shiny.fullstacktrace", FALSE), 
    offset = getOption("shiny.stacktraceoffset", TRUE)) 
{
    warning(call. = FALSE, immediate. = TRUE, sprintf("Error in %s: %s", 
        getCallNames(list(conditionCall(cond))), conditionMessage(cond)))
    printStackTrace(cond, full = full, offset = offset)
    invisible()
}


reactiveUI <- function (func) 
{
    shinyDeprecated(new = "renderUI")
    renderUI({
        func()
    })
}


isolate <- function (expr) 
{
    ctx <- Context$new(getDefaultReactiveDomain(), "[isolate]", 
        type = "isolate")
    on.exit(ctx$invalidate())
    ..stacktraceoff..(ctx$run(function() {
        ..stacktraceon..(expr)
    }))
}


makeReactiveBinding <- function (symbol, env = parent.frame()) 
{
    if (exists(symbol, envir = env, inherits = FALSE)) {
        initialValue <- env[[symbol]]
        rm(list = symbol, envir = env, inherits = FALSE)
    }
    else initialValue <- NULL
    values <- reactiveValues(value = initialValue)
    makeActiveBinding(symbol, env = env, fun = function(v) {
        if (missing(v)) 
            values$value
        else values$value <- v
    })
    invisible()
}


pre <- htmltools::pre # re-exported from htmltools package

inputPanel <- function (...) 
{
    div(class = "shiny-input-panel", flowLayout(...))
}


onFlush <- function (fun, once = TRUE, session = getDefaultReactiveDomain()) 
{
    session$onFlush(fun, once = once)
}


updateSliderInput <- function (session, inputId, label = NULL, value = NULL, min = NULL, 
    max = NULL, step = NULL) 
{
    vals <- dropNulls(list(value, min, max))
    type <- unique(lapply(vals, function(x) {
        if (inherits(x, "Date")) 
            "date"
        else if (inherits(x, "POSIXt")) 
            "datetime"
        else "number"
    }))
    if (length(type) > 1) {
        stop("Type mismatch for value, min, and max")
    }
    if ((length(type) == 1) && (type == "date" || type == "datetime")) {
        to_ms <- function(x) 1000 * as.numeric(as.POSIXct(x))
        if (!is.null(min)) 
            min <- to_ms(min)
        if (!is.null(max)) 
            max <- to_ms(max)
        if (!is.null(value)) 
            value <- to_ms(value)
    }
    message <- dropNulls(list(label = label, value = formatNoSci(value), 
        min = formatNoSci(min), max = formatNoSci(max), step = formatNoSci(step)))
    session$sendInputMessage(inputId, message)
}


hoverOpts <- function (id = NULL, delay = 300, delayType = c("debounce", "throttle"), 
    clip = TRUE, nullOutside = TRUE) 
{
    if (is.null(id)) 
        stop("id must not be NULL")
    list(id = id, delay = delay, delayType = match.arg(delayType), 
        clip = clip, nullOutside = nullOutside)
}


`conditionStackTrace<-` <- function (cond, value) 
{
    attr(cond, "stack.trace") <- value
    invisible(cond)
}


sidebarLayout <- function (sidebarPanel, mainPanel, position = c("left", "right"), 
    fluid = TRUE) 
{
    position <- match.arg(position)
    if (position == "left") {
        firstPanel <- sidebarPanel
        secondPanel <- mainPanel
    }
    else if (position == "right") {
        firstPanel <- mainPanel
        secondPanel <- sidebarPanel
    }
    if (fluid) 
        fluidRow(firstPanel, secondPanel)
    else fixedRow(firstPanel, secondPanel)
}


incProgress <- function (amount = 0.1, message = NULL, detail = NULL, session = getDefaultReactiveDomain()) 
{
    if (is.null(session$progressStack)) 
        stop("'session' is not a ShinySession object.")
    if (session$progressStack$size() == 0) {
        warning("incProgress was called outside of withProgress; ignoring")
        return()
    }
    p <- session$progressStack$peek()
    p$inc(amount, message, detail)
    invisible()
}


captureStackTraces <- function (expr) 
{
    withCallingHandlers(expr, error = function(e) {
        if (is.null(attr(e, "stack.trace", exact = TRUE))) {
            calls <- sys.calls()
            attr(e, "stack.trace") <- calls
            stop(e)
        }
    })
}


restoreInput <- function (id, default) 
{
    force(default)
    if (!hasCurrentRestoreContext()) {
        return(default)
    }
    oldInputs <- getCurrentRestoreContext()$input
    if (oldInputs$available(id)) {
        oldInputs$get(id)
    }
    else {
        default
    }
}


htmlTemplate <- htmltools::htmlTemplate # re-exported from htmltools package

fillPage <- function (..., padding = 0, title = NULL, bootstrap = TRUE, theme = NULL) 
{
    fillCSS <- tags$head(tags$style(type = "text/css", "html, body { width: 100%; height: 100%; overflow: hidden; }", 
        sprintf("body { padding: %s; margin: 0; }", collapseSizes(padding))))
    if (isTRUE(bootstrap)) {
        bootstrapPage(title = title, theme = theme, fillCSS, 
            ...)
    }
    else {
        tagList(fillCSS, if (!is.null(title)) 
            tags$head(tags$title(title)), ...)
    }
}


updateNumericInput <- function (session, inputId, label = NULL, value = NULL, min = NULL, 
    max = NULL, step = NULL) 
{
    message <- dropNulls(list(label = label, value = formatNoSci(value), 
        min = formatNoSci(min), max = formatNoSci(max), step = formatNoSci(step)))
    session$sendInputMessage(inputId, message)
}


wellPanel <- function (...) 
{
    div(class = "well", ...)
}


verbatimTextOutput <- function (outputId, placeholder = FALSE) 
{
    pre(id = outputId, class = paste(c("shiny-text-output", if (!placeholder) "noplaceholder"), 
        collapse = " "))
}


nearPoints <- function (df, coordinfo, xvar = NULL, yvar = NULL, panelvar1 = NULL, 
    panelvar2 = NULL, threshold = 5, maxpoints = NULL, addDist = FALSE, 
    allRows = FALSE) 
{
    if (is.null(coordinfo)) {
        if (addDist) 
            df$dist_ <- NA_real_
        if (allRows) 
            df$selected_ <- FALSE
        else df <- df[0, , drop = FALSE]
        return(df)
    }
    if (is.null(coordinfo$x)) {
        stop("nearPoints requires a click/hover/double-click object with x and y values.")
    }
    xvar <- xvar %OR% coordinfo$mapping$x
    yvar <- yvar %OR% coordinfo$mapping$y
    panelvar1 <- panelvar1 %OR% coordinfo$mapping$panelvar1
    panelvar2 <- panelvar2 %OR% coordinfo$mapping$panelvar2
    if (is.null(xvar)) 
        stop("nearPoints: not able to automatically infer `xvar` from coordinfo")
    if (is.null(yvar)) 
        stop("nearPoints: not able to automatically infer `yvar` from coordinfo")
    x <- asNumber(df[[xvar]])
    y <- asNumber(df[[yvar]])
    coordPx <- scaleCoords(coordinfo$x, coordinfo$y, coordinfo)
    dataPx <- scaleCoords(x, y, coordinfo)
    dists <- sqrt((dataPx$x - coordPx$x)^2 + (dataPx$y - coordPx$y)^2)
    if (addDist) 
        df$dist_ <- dists
    keep_rows <- (dists <= threshold)
    if (!is.null(panelvar1)) 
        keep_rows <- keep_rows & panelMatch(coordinfo$panelvar1, 
            df[[panelvar1]])
    if (!is.null(panelvar2)) 
        keep_rows <- keep_rows & panelMatch(coordinfo$panelvar2, 
            df[[panelvar2]])
    keep_idx <- which(keep_rows)
    dists <- dists[keep_idx]
    keep_idx <- keep_idx[order(dists)]
    if (!is.null(maxpoints) && length(keep_idx) > maxpoints) {
        keep_idx <- keep_idx[seq_len(maxpoints)]
    }
    if (allRows) {
        df$selected_ <- FALSE
        df$selected_[keep_idx] <- TRUE
    }
    else {
        df <- df[keep_idx, , drop = FALSE]
    }
    df
}


showBookmarkUrlModal <- function (url) 
{
    store <- getShinyOption("bookmarkStore", default = "")
    if (store == "url") {
        subtitle <- "This link stores the current state of this application."
    }
    else if (store == "server") {
        subtitle <- "The current state of this application has been stored on the server."
    }
    else {
        subtitle <- NULL
    }
    showModal(urlModal(url, subtitle = subtitle))
}


onFlushed <- function (fun, once = TRUE, session = getDefaultReactiveDomain()) 
{
    session$onFlushed(fun, once = once)
}


headerPanel <- function (title, windowTitle = title) 
{
    tagList(tags$head(tags$title(windowTitle)), div(class = "col-sm-12", 
        h1(title)))
}


onSessionEnded <- function (fun, session = getDefaultReactiveDomain()) 
{
    session$onSessionEnded(fun)
}


suppressDependencies <- htmltools::suppressDependencies # re-exported from htmltools package

fixedPanel <- function (..., top = NULL, left = NULL, right = NULL, bottom = NULL, 
    width = NULL, height = NULL, draggable = FALSE, cursor = c("auto", 
        "move", "default", "inherit")) 
{
    absolutePanel(..., top = top, left = left, right = right, 
        bottom = bottom, width = width, height = height, draggable = draggable, 
        cursor = match.arg(cursor), fixed = TRUE)
}


radioButtons <- function (inputId, label, choices, selected = NULL, inline = FALSE, 
    width = NULL) 
{
    choices <- choicesWithNames(choices)
    selected <- restoreInput(id = inputId, default = selected)
    selected <- if (is.null(selected)) 
        choices[[1]]
    else {
        validateSelected(selected, choices, inputId)
    }
    if (length(selected) > 1) 
        stop("The 'selected' argument must be of length 1")
    options <- generateOptions(inputId, choices, selected, inline, 
        type = "radio")
    divClass <- "form-group shiny-input-radiogroup shiny-input-container"
    if (inline) 
        divClass <- paste(divClass, "shiny-input-container-inline")
    tags$div(id = inputId, style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), class = divClass, 
        controlLabel(inputId, label), options)
}


tagSetChildren <- htmltools::tagSetChildren # re-exported from htmltools package

eventReactive <- function (eventExpr, valueExpr, event.env = parent.frame(), event.quoted = FALSE, 
    value.env = parent.frame(), value.quoted = FALSE, label = NULL, 
    domain = getDefaultReactiveDomain(), ignoreNULL = TRUE, ignoreInit = FALSE) 
{
    eventFunc <- exprToFunction(eventExpr, event.env, event.quoted)
    if (is.null(label)) 
        label <- sprintf("eventReactive(%s)", paste(deparse(body(eventFunc)), 
            collapse = "\n"))
    eventFunc <- wrapFunctionLabel(eventFunc, "eventReactiveExpr", 
        ..stacktraceon = TRUE)
    handlerFunc <- exprToFunction(valueExpr, value.env, value.quoted)
    handlerFunc <- wrapFunctionLabel(handlerFunc, "eventReactiveHandler", 
        ..stacktraceon = TRUE)
    initialized <- FALSE
    invisible(reactive({
        e <- eventFunc()
        if (ignoreInit && !initialized) {
            initialized <<- TRUE
            req(FALSE)
        }
        req(!ignoreNULL || !isNullEvent(e))
        isolate(handlerFunc())
    }, label = label, domain = domain, ..stacktraceon = FALSE))
}


includeText <- htmltools::includeText # re-exported from htmltools package

ns.sep <- "-"


renderTable <- function (expr, striped = FALSE, hover = FALSE, bordered = FALSE, 
    spacing = c("s", "xs", "m", "l"), width = "auto", align = NULL, 
    rownames = FALSE, colnames = TRUE, digits = NULL, na = "NA", 
    ..., env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
    installExprFunction(expr, "func", env, quoted)
    if (!is.function(spacing)) 
        spacing <- match.arg(spacing)
    createWrapper <- function(arg) {
        if (is.function(arg)) 
            wrapper <- arg
        else wrapper <- function() arg
        return(wrapper)
    }
    stripedWrapper <- createWrapper(striped)
    hoverWrapper <- createWrapper(hover)
    borderedWrapper <- createWrapper(bordered)
    spacingWrapper <- createWrapper(spacing)
    widthWrapper <- createWrapper(width)
    alignWrapper <- createWrapper(align)
    rownamesWrapper <- createWrapper(rownames)
    colnamesWrapper <- createWrapper(colnames)
    digitsWrapper <- createWrapper(digits)
    naWrapper <- createWrapper(na)
    dots <- list(...)
    renderFunc <- function(shinysession, name, ...) {
        striped <- stripedWrapper()
        hover <- hoverWrapper()
        bordered <- borderedWrapper()
        format <- c(striped = striped, hover = hover, bordered = bordered)
        spacing <- spacingWrapper()
        width <- widthWrapper()
        align <- alignWrapper()
        rownames <- rownamesWrapper()
        colnames <- colnamesWrapper()
        digits <- digitsWrapper()
        na <- naWrapper()
        spacing_choices <- c("s", "xs", "m", "l")
        if (!(spacing %in% spacing_choices)) {
            stop(paste("`spacing` must be one of", paste0("'", 
                spacing_choices, "'", collapse = ", ")))
        }
        classNames <- paste0("table shiny-table", paste0(" table-", 
            names(format)[format], collapse = ""), paste0(" spacing-", 
            spacing))
        data <- func()
        data <- as.data.frame(data)
        if (is.null(data) || (is.data.frame(data) && nrow(data) == 
            0 && ncol(data) == 0)) 
            return(NULL)
        xtable_argnames <- setdiff(names(formals(xtable)), c("x", 
            "..."))
        xtable_args <- dots[intersect(names(dots), xtable_argnames)]
        non_xtable_args <- dots[setdiff(names(dots), xtable_argnames)]
        defaultAlignment <- function(col) {
            if (is.numeric(col)) 
                "r"
            else "l"
        }
        if (is.null(align) || align == "?") {
            names <- defaultAlignment(attr(data, "row.names"))
            cols <- paste(vapply(data, defaultAlignment, character(1)), 
                collapse = "")
            cols <- paste0(names, cols)
        }
        else {
            num_cols <- if (rownames) 
                nchar(align)
            else nchar(align) + 1
            valid <- !grepl("[^lcr\\?]", align)
            if (num_cols == ncol(data) + 1 && valid) {
                cols <- if (rownames) 
                  align
                else paste0("r", align)
                defaults <- grep("\\?", strsplit(cols, "")[[1]])
                if (length(defaults) != 0) {
                  vals <- vapply(data[, defaults - 1], defaultAlignment, 
                    character(1))
                  for (i in seq_len(length(defaults))) {
                    substr(cols, defaults[i], defaults[i]) <- vals[i]
                  }
                }
            }
            else if (nchar(align) == 1 && valid) {
                cols <- paste0(rep(align, ncol(data) + 1), collapse = "")
            }
            else {
                stop("`align` must contain only the characters `l`, `c`, `r` and/or `?` and", 
                  "have length either equal to 1 or to the total number of columns")
            }
        }
        xtable_args <- c(xtable_args, align = cols, digits = digits)
        xtable_res <- do.call(xtable, c(list(data), xtable_args))
        print_args <- list(x = xtable_res, type = "html", include.rownames = {
            if ("include.rownames" %in% names(dots)) dots$include.rownames else rownames
        }, include.colnames = {
            if ("include.colnames" %in% names(dots)) dots$include.colnames else colnames
        }, NA.string = {
            if ("NA.string" %in% names(dots)) dots$NA.string else na
        }, html.table.attributes = paste0({
            if ("html.table.attributes" %in% names(dots)) dots$html.table.attributes else ""
        }, " ", "class = '", htmlEscape(classNames, TRUE), "' ", 
            "style = 'width:", validateCssUnit(width), ";'"))
        print_args <- c(print_args, non_xtable_args)
        print_args <- print_args[unique(names(print_args))]
        tab <- paste(utils::capture.output(do.call(print, print_args)), 
            collapse = "\n")
        tab <- gsub(paste(">", na, "<"), paste(" class='NA'>", 
            na, "<"), tab)
        if (colnames) {
            tab <- sub("<tr>", "<thead> <tr>", tab)
            tab <- sub("</tr>", "</tr> </thead> <tbody>", tab)
            tab <- sub("</table>$", "</tbody> </table>", tab)
            cols <- if (rownames) 
                cols
            else substr(cols, 2, nchar(cols))
            cols <- strsplit(cols, "")[[1]]
            cols[cols == "l"] <- "left"
            cols[cols == "r"] <- "right"
            cols[cols == "c"] <- "center"
            for (i in seq_len(length(cols))) {
                tab <- sub("<th>", paste0("<th style='text-align: ", 
                  cols[i], ";'>"), tab)
            }
        }
        return(tab)
    }
    markRenderFunction(tableOutput, renderFunc, outputArgs = outputArgs)
}


actionLink <- function (inputId, label, icon = NULL, ...) 
{
    value <- restoreInput(id = inputId, default = NULL)
    tags$a(id = inputId, href = "#", class = "action-button", 
        `data-val` = value, list(validateIcon(icon), label), 
        ...)
}


brushOpts <- function (id = NULL, fill = "#9cf", stroke = "#036", opacity = 0.25, 
    delay = 300, delayType = c("debounce", "throttle"), clip = TRUE, 
    direction = c("xy", "x", "y"), resetOnNew = FALSE) 
{
    if (is.null(id)) 
        stop("id must not be NULL")
    list(id = id, fill = fill, stroke = stroke, opacity = opacity, 
        delay = delay, delayType = match.arg(delayType), clip = clip, 
        direction = match.arg(direction), resetOnNew = resetOnNew)
}


updateActionButton <- function (session, inputId, label = NULL, icon = NULL) 
{
    if (!is.null(icon)) 
        icon <- as.character(validateIcon(icon))
    message <- dropNulls(list(label = label, icon = icon))
    session$sendInputMessage(inputId, message)
}


shinyApp <- function (ui = NULL, server = NULL, onStart = NULL, options = list(), 
    uiPattern = "/", enableBookmarking = NULL) 
{
    if (is.null(server)) {
        stop("`server` missing from shinyApp")
    }
    uiPattern <- sprintf("^%s$", uiPattern)
    httpHandler <- uiHttpHandler(ui, uiPattern)
    serverFuncSource <- function() {
        server
    }
    if (!is.null(enableBookmarking)) {
        bookmarkStore <- match.arg(enableBookmarking, c("url", 
            "server", "disable"))
        enableBookmarking(bookmarkStore)
    }
    shinyOptions(appDir = getwd())
    appOptions <- consumeAppOptions()
    structure(list(httpHandler = httpHandler, serverFuncSource = serverFuncSource, 
        onStart = onStart, options = options, appOptions = appOptions), 
        class = "shiny.appobj")
}


dataTableOutput <- function (outputId) 
{
    attachDependencies(div(id = outputId, class = "shiny-datatable-output"), 
        dataTableDependency)
}


navlistPanel <- function (..., id = NULL, selected = NULL, well = TRUE, fluid = TRUE, 
    widths = c(4, 8)) 
{
    textFilter <- function(text) {
        tags$li(class = "navbar-brand", text)
    }
    if (!is.null(id)) 
        selected <- restoreInput(id = id, default = selected)
    tabs <- list(...)
    tabset <- buildTabset(tabs, "nav nav-pills nav-stacked", 
        textFilter, id, selected)
    columns <- list(column(widths[[1]], class = ifelse(well, 
        "well", ""), tabset$navList), column(widths[[2]], tabset$content))
    if (fluid) 
        fluidRow(columns)
    else fixedRow(columns)
}


callModule <- function (module, id, ..., session = getDefaultReactiveDomain()) 
{
    childScope <- session$makeScope(id)
    withReactiveDomain(childScope, {
        if (!is.function(module)) {
            stop("module argument must be a function")
        }
        module(childScope$input, childScope$output, childScope, 
            ...)
    })
}


dateRangeInput <- function (inputId, label, start = NULL, end = NULL, min = NULL, 
    max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0, 
    language = "en", separator = " to ", width = NULL) 
{
    if (inherits(start, "Date")) 
        start <- format(start, "%Y-%m-%d")
    if (inherits(end, "Date")) 
        end <- format(end, "%Y-%m-%d")
    if (inherits(min, "Date")) 
        min <- format(min, "%Y-%m-%d")
    if (inherits(max, "Date")) 
        max <- format(max, "%Y-%m-%d")
    restored <- restoreInput(id = inputId, default = list(start, 
        end))
    start <- restored[[1]]
    end <- restored[[2]]
    attachDependencies(div(id = inputId, class = "shiny-date-range-input form-group shiny-input-container", 
        style = if (!is.null(width)) 
            paste0("width: ", validateCssUnit(width), ";"), controlLabel(inputId, 
            label), div(class = "input-daterange input-group", 
            tags$input(class = "input-sm form-control", type = "text", 
                `data-date-language` = language, `data-date-weekstart` = weekstart, 
                `data-date-format` = format, `data-date-start-view` = startview, 
                `data-min-date` = min, `data-max-date` = max, 
                `data-initial-date` = start), span(class = "input-group-addon", 
                separator), tags$input(class = "input-sm form-control", 
                type = "text", `data-date-language` = language, 
                `data-date-weekstart` = weekstart, `data-date-format` = format, 
                `data-date-start-view` = startview, `data-min-date` = min, 
                `data-max-date` = max, `data-initial-date` = end))), 
        datePickerDependency)
}


uiOutput <- function (outputId, inline = FALSE, container = if (inline) span else div, 
    ...) 
{
    if (anyUnnamed(list(...))) {
        warning("Unnamed elements in ... will be replaced with dynamic UI.")
    }
    container(id = outputId, class = "shiny-html-output", ...)
}


validate <- function (..., errorClass = character(0)) 
{
    results <- sapply(list(...), function(x) {
        if (is.null(x)) 
            return(NA_character_)
        else if (identical(x, FALSE)) 
            return("")
        else if (is.character(x)) 
            return(paste(as.character(x), collapse = "\n"))
        else stop("Unexpected validation result: ", as.character(x))
    })
    results <- stats::na.omit(results)
    if (length(results) == 0) 
        return(invisible())
    results <- results[nzchar(results)]
    reactiveStop(paste(results, collapse = "\n"), c(errorClass, 
        "validation"))
}


shinyOptions <- function (...) 
{
    newOpts <- list(...)
    if (length(newOpts) > 0) {
        .globals$options <- dropNulls(mergeVectors(.globals$options, 
            newOpts))
        invisible(.globals$options)
    }
    else {
        .globals$options
    }
}


hr <- htmltools::hr # re-exported from htmltools package

modalDialog <- function (..., title = NULL, footer = modalButton("Dismiss"), 
    size = c("m", "s", "l"), easyClose = FALSE, fade = TRUE) 
{
    size <- match.arg(size)
    cls <- if (fade) 
        "modal fade"
    else "modal"
    div(id = "shiny-modal", class = cls, tabindex = "-1", `data-backdrop` = if (!easyClose) 
        "static", `data-keyboard` = if (!easyClose) 
        "false", div(class = "modal-dialog", class = switch(size, 
        s = "modal-sm", m = NULL, l = "modal-lg"), div(class = "modal-content", 
        if (!is.null(title)) 
            div(class = "modal-header", tags$h4(class = "modal-title", 
                title)), div(class = "modal-body", ...), if (!is.null(footer)) 
            div(class = "modal-footer", footer))), tags$script("$('#shiny-modal').modal().focus();"))
}


absolutePanel <- function (..., top = NULL, left = NULL, right = NULL, bottom = NULL, 
    width = NULL, height = NULL, draggable = FALSE, fixed = FALSE, 
    cursor = c("auto", "move", "default", "inherit")) 
{
    cssProps <- list(top = top, left = left, right = right, bottom = bottom, 
        width = width, height = height)
    cssProps <- cssProps[!sapply(cssProps, is.null)]
    cssProps <- sapply(cssProps, validateCssUnit)
    cssProps[["position"]] <- ifelse(fixed, "fixed", "absolute")
    cssProps[["cursor"]] <- match.arg(cursor)
    if (identical(cssProps[["cursor"]], "auto")) 
        cssProps[["cursor"]] <- ifelse(draggable, "move", "inherit")
    style <- paste(paste(names(cssProps), cssProps, sep = ":", 
        collapse = ";"), ";", sep = "")
    divTag <- tags$div(style = style, ...)
    if (isTRUE(draggable)) {
        divTag <- tagAppendAttributes(divTag, class = "draggable")
        return(tagList(singleton(tags$head(tags$script(src = "shared/jqueryui/jquery-ui.min.js"))), 
            divTag, tags$script("$(\".draggable\").draggable();")))
    }
    else {
        return(divTag)
    }
}


updateRadioButtons <- function (session, inputId, label = NULL, choices = NULL, selected = NULL, 
    inline = FALSE) 
{
    if (is.null(selected) && !is.null(choices)) 
        selected <- choices[[1]]
    updateInputOptions(session, inputId, label, choices, selected, 
        inline, type = "radio")
}


updateNavlistPanel <- function (session, inputId, selected = NULL) 
{
    message <- dropNulls(list(value = selected))
    session$sendInputMessage(inputId, message)
}


onBookmarked <- function (fun, session = getDefaultReactiveDomain()) 
{
    session$onBookmarked(fun)
}


renderText <- function (expr, env = parent.frame(), quoted = FALSE, outputArgs = list()) 
{
    installExprFunction(expr, "func", env, quoted)
    renderFunc <- function(shinysession, name, ...) {
        value <- func()
        return(paste(utils::capture.output(cat(value)), collapse = "\n"))
    }
    markRenderFunction(textOutput, renderFunc, outputArgs = outputArgs)
}


checkboxGroupInput <- function (inputId, label, choices, selected = NULL, inline = FALSE, 
    width = NULL) 
{
    selected <- restoreInput(id = inputId, default = selected)
    choices <- choicesWithNames(choices)
    if (!is.null(selected)) 
        selected <- validateSelected(selected, choices, inputId)
    options <- generateOptions(inputId, choices, selected, inline)
    divClass <- "form-group shiny-input-checkboxgroup shiny-input-container"
    if (inline) 
        divClass <- paste(divClass, "shiny-input-container-inline")
    tags$div(id = inputId, style = if (!is.null(width)) 
        paste0("width: ", validateCssUnit(width), ";"), class = divClass, 
        controlLabel(inputId, label), options)
}


downloadHandler <- function (filename, content, contentType = NA, outputArgs = list()) 
{
    renderFunc <- function(shinysession, name, ...) {
        shinysession$registerDownload(name, filename, contentType, 
            content)
    }
    markRenderFunction(downloadButton, renderFunc, outputArgs = outputArgs)
}


shinyAppDir <- function (appDir, options = list()) 
{
    if (!utils::file_test("-d", appDir)) {
        stop("No Shiny application exists at the path \"", appDir, 
            "\"")
    }
    appDir <- normalizePath(appDir, mustWork = TRUE)
    if (file.exists.ci(appDir, "server.R")) {
        shinyAppDir_serverR(appDir, options = options)
    }
    else if (file.exists.ci(appDir, "app.R")) {
        shinyAppDir_appR("app.R", appDir, options = options)
    }
    else {
        stop("App dir must contain either app.R or server.R.")
    }
}


onRestore <- function (fun, session = getDefaultReactiveDomain()) 
{
    session$onRestore(fun)
}


clickOpts <- function (id = NULL, clip = TRUE) 
{
    if (is.null(id)) 
        stop("id must not be NULL")
    list(id = id, clip = clip)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Web Application Framework for R"

.skeleton_package_version = "1.0.0"

.skeleton_package_depends = "methods"

.skeleton_package_imports = "utils,httpuv,mime,jsonlite,xtable,digest,htmltools,R6,sourcetools"


## Internal

.skeleton_version = 5


## EOF