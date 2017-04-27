##
## Exported symobls in package `htmlwidgets`
##

## Exported package methods

setWidgetIdSeed <- function (seed, kind = NULL, normal.kind = NULL) 
{
    sysSeed <- .GlobalEnv$.Random.seed
    on.exit({
        .globals$idSeed <- .GlobalEnv$.Random.seed
        if (!is.null(sysSeed)) .GlobalEnv$.Random.seed <- sysSeed else rm(".Random.seed", 
            envir = .GlobalEnv)
    })
    set.seed(seed, kind = kind, normal.kind = normal.kind)
}


shinyRenderWidget <- function (expr, outputFunction, env, quoted) 
{
    checkShinyVersion()
    func <- shiny::exprToFunction(expr, env, quoted)
    renderFunc <- function() {
        instance <- func()
        if (!is.null(instance$elementId)) {
            warning("Ignoring explicitly provided widget ID \"", 
                instance$elementId, "\"; Shiny doesn't use them")
        }
        if (!is.null(instance$prepend)) {
            warning("Ignoring prepended content; prependContent can't be used in a ", 
                "Shiny render call")
        }
        if (!is.null(instance$append)) {
            warning("Ignoring appended content; appendContent can't be used in a ", 
                "Shiny render call")
        }
        deps <- .subset2(instance, "dependencies")
        deps <- lapply(htmltools::resolveDependencies(deps), 
            shiny::createWebDependency)
        payload <- c(createPayload(instance), list(deps = deps))
        toJSON(payload)
    }
    shiny::markRenderFunction(outputFunction, renderFunc)
}


onRender <- function (x, jsCode, data = NULL) 
{
    addHook(x, "render", jsCode, data)
}


saveWidget <- function (widget, file, selfcontained = TRUE, libdir = NULL, 
    background = "white", knitrOptions = list()) 
{
    html <- toHTML(widget, standalone = TRUE, knitrOptions = knitrOptions)
    if (is.null(libdir)) {
        libdir <- paste(tools::file_path_sans_ext(basename(file)), 
            "_files", sep = "")
    }
    htmltools::save_html(html, file = file, libdir = libdir, 
        background = background)
    if (selfcontained) {
        if (!pandoc_available()) {
            stop("Saving a widget with selfcontained = TRUE requires pandoc. For details see:\n", 
                "https://github.com/rstudio/rmarkdown/blob/master/PANDOC.md")
        }
        pandoc_self_contained_html(file, file)
        unlink(libdir, recursive = TRUE)
    }
    invisible(NULL)
}


appendContent <- function (x, ...) 
{
    x$append <- c(x$append, list(...))
    x
}


sizingPolicy <- function (defaultWidth = NULL, defaultHeight = NULL, padding = NULL, 
    viewer.defaultWidth = NULL, viewer.defaultHeight = NULL, 
    viewer.padding = NULL, viewer.fill = TRUE, viewer.suppress = FALSE, 
    viewer.paneHeight = NULL, browser.defaultWidth = NULL, browser.defaultHeight = NULL, 
    browser.padding = NULL, browser.fill = FALSE, knitr.defaultWidth = NULL, 
    knitr.defaultHeight = NULL, knitr.figure = TRUE) 
{
    list(defaultWidth = defaultWidth, defaultHeight = defaultHeight, 
        padding = padding, viewer = list(defaultWidth = viewer.defaultWidth, 
            defaultHeight = viewer.defaultHeight, padding = viewer.padding, 
            fill = viewer.fill, suppress = viewer.suppress, paneHeight = viewer.paneHeight), 
        browser = list(defaultWidth = browser.defaultWidth, defaultHeight = browser.defaultHeight, 
            padding = browser.padding, fill = browser.fill), 
        knitr = list(defaultWidth = knitr.defaultWidth, defaultHeight = knitr.defaultHeight, 
            figure = knitr.figure))
}


prependContent <- function (x, ...) 
{
    x$prepend <- c(x$prepend, list(...))
    x
}


shinyWidgetOutput <- function (outputId, name, width, height, package = name, inline = FALSE) 
{
    checkShinyVersion()
    html <- htmltools::tagList(widget_html(name, package, id = outputId, 
        class = paste(name, "html-widget html-widget-output"), 
        style = sprintf("width:%s; height:%s; %s", htmltools::validateCssUnit(width), 
            htmltools::validateCssUnit(height), if (inline) 
                "display: inline-block;"
            else ""), width = width, height = height))
    dependencies = widget_dependencies(name, package)
    htmltools::attachDependencies(html, dependencies)
}


onStaticRenderComplete <- function (jsCode) 
{
    tags$script("HTMLWidgets.addPostRenderHandler(function() {", 
        HTML(paste0(jsCode, collapse = "\n")), "});")
}


createWidget <- function (name, x, width = NULL, height = NULL, sizingPolicy = htmlwidgets::sizingPolicy(), 
    package = name, dependencies = NULL, elementId = NULL, preRenderHook = NULL) 
{
    if (inherits(dependencies, "html_dependency")) 
        dependencies <- list(dependencies)
    structure(list(x = x, width = width, height = height, sizingPolicy = sizingPolicy, 
        dependencies = dependencies, elementId = elementId, preRenderHook = preRenderHook, 
        jsHooks = list()), class = c(name, if (sizingPolicy$viewer$suppress) "suppress_viewer", 
        "htmlwidget"), package = package)
}


JS <- function (...) 
{
    x <- c(...)
    if (is.null(x)) 
        return()
    if (!is.character(x)) 
        stop("The arguments for JS() must be a character vector")
    x <- paste(x, collapse = "\n")
    structure(x, class = unique(c("JS_EVAL", oldClass(x))))
}


scaffoldWidget <- function (name, bowerPkg = NULL, edit = interactive()) 
{
    if (!file.exists("DESCRIPTION")) {
        stop("You need to create a package to house your widget first!", 
            call. = F)
    }
    if (!file.exists("inst")) {
        dir.create("inst")
    }
    package = read.dcf("DESCRIPTION")[[1, "Package"]]
    addWidgetConstructor(name, package, edit)
    addWidgetYAML(name, bowerPkg, edit)
    addWidgetJS(name, edit)
}


getDependency <- function (name, package = name) 
{
    config = sprintf("htmlwidgets/%s.yaml", name)
    jsfile = sprintf("htmlwidgets/%s.js", name)
    config = yaml::yaml.load_file(system.file(config, package = package))
    widgetDep <- lapply(config$dependencies, function(l) {
        l$src = system.file(l$src, package = package)
        do.call(htmlDependency, l)
    })
    bindingDir <- system.file("htmlwidgets", package = package)
    argsDep <- NULL
    copyBindingDir <- getOption("htmlwidgets.copybindingdir", 
        TRUE)
    if (copyBindingDir) {
        if (packageVersion("htmltools") < "0.3.3") {
            bindingDir <- tempfile("widgetbinding")
            dir.create(bindingDir, mode = "0700")
            file.copy(system.file(jsfile, package = package), 
                bindingDir)
        }
        else argsDep <- list(all_files = FALSE)
    }
    bindingDep <- do.call(htmlDependency, c(list(paste0(name, 
        "-binding"), packageVersion(package), bindingDir, script = basename(jsfile)), 
        argsDep))
    c(list(htmlDependency("htmlwidgets", packageVersion("htmlwidgets"), 
        src = system.file("www", package = "htmlwidgets"), script = "htmlwidgets.js")), 
        widgetDep, list(bindingDep))
}




## Package Data

# none


## Package Info

.skeleton_package_title = "HTML Widgets for R"

.skeleton_package_version = "0.8"

.skeleton_package_depends = ""

.skeleton_package_imports = "htmltools,jsonlite,yaml"


## Internal

.skeleton_version = 5


## EOF