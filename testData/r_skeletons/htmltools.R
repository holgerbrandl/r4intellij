##
## Exported symobls in package `htmltools`
##

## Exported package methods

tag <- function (`_tag_name`, varArgs) 
{
    varArgsNames <- names(varArgs)
    if (is.null(varArgsNames)) 
        varArgsNames <- character(length = length(varArgs))
    named_idx <- nzchar(varArgsNames)
    attribs <- dropNulls(varArgs[named_idx])
    children <- unname(varArgs[!named_idx])
    structure(list(name = `_tag_name`, attribs = attribs, children = children), 
        class = "shiny.tag")
}


includeHTML <- function (path) 
{
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    return(HTML(paste8(lines, collapse = "\r\n")))
}


span <- function (...) 
tags$span(...)


h1 <- function (...) 
tags$h1(...)


renderTags <- function (x, singletons = character(0), indent = 0) 
{
    x <- tagify(x)
    singletonInfo <- takeSingletons(x, singletons)
    headInfo <- takeHeads(singletonInfo$ui)
    deps <- resolveDependencies(findDependencies(singletonInfo$ui))
    headIndent <- if (is.numeric(indent)) 
        indent + 1
    else indent
    headHtml <- doRenderTags(headInfo$head, indent = headIndent)
    bodyHtml <- doRenderTags(headInfo$ui, indent = indent)
    return(list(head = headHtml, singletons = singletonInfo$singletons, 
        dependencies = deps, html = bodyHtml))
}


h2 <- function (...) 
tags$h2(...)


tagList <- function (...) 
{
    lst <- list(...)
    class(lst) <- c("shiny.tag.list", "list")
    return(lst)
}


renderDependencies <- function (dependencies, srcType = c("href", "file"), encodeFunc = urlEncodePath, 
    hrefFilter = identity) 
{
    html <- c()
    for (dep in dependencies) {
        usableType <- srcType[which(srcType %in% names(dep$src))]
        if (length(usableType) == 0) 
            stop("Dependency ", dep$name, " ", dep$version, " does not have a usable source")
        dir <- dep$src[head(usableType, 1)]
        srcpath <- if (usableType == "file") {
            encodeFunc(dir)
        }
        else {
            href_path(dep)
        }
        srcpath <- sub("/$", "\\1", srcpath)
        if (length(dep$meta) > 0) {
            html <- c(html, paste("<meta name=\"", htmlEscape(names(dep$meta)), 
                "\" content=\"", htmlEscape(dep$meta), "\" />", 
                sep = ""))
        }
        if (length(dep$stylesheet) > 0) {
            html <- c(html, paste("<link href=\"", htmlEscape(hrefFilter(file.path(srcpath, 
                encodeFunc(dep$stylesheet)))), "\" rel=\"stylesheet\" />", 
                sep = ""))
        }
        if (length(dep$script) > 0) {
            html <- c(html, paste("<script src=\"", htmlEscape(hrefFilter(file.path(srcpath, 
                encodeFunc(dep$script)))), "\"></script>", sep = ""))
        }
        if (length(dep$attachment) > 0) {
            if (is.null(names(dep$attachment))) 
                names(dep$attachment) <- as.character(1:length(dep$attachment))
            html <- c(html, sprintf("<link id=\"%s-%s-attachment\" rel=\"attachment\" href=\"%s\"/>", 
                htmlEscape(dep$name), htmlEscape(names(dep$attachment)), 
                htmlEscape(hrefFilter(file.path(srcpath, encodeFunc(dep$attachment))))))
        }
        html <- c(html, dep$head)
    }
    HTML(paste(html, collapse = "\n"))
}


h3 <- function (...) 
tags$h3(...)


`htmlDependencies<-` <- function (x, value) 
{
    if (inherits(value, "html_dependency")) 
        value <- list(value)
    attr(x, "html_dependencies") <- value
    x
}


tagHasAttribute <- function (tag, attr) 
{
    result <- attr %in% names(tag$attribs)
    result
}


h4 <- function (...) 
tags$h4(...)


copyDependencyToDir <- function (dependency, outputDir, mustWork = TRUE) 
{
    dir <- dependency$src$file
    if (is.null(dir)) {
        if (mustWork) {
            stop("Dependency ", dependency$name, " ", dependency$version, 
                " is not disk-based")
        }
        else {
            return(dependency)
        }
    }
    if (length(outputDir) != 1 || outputDir %in% c("", "/")) 
        stop("outputDir must be of length 1 and cannot be \"\" or \"/\"")
    if (!dir_exists(outputDir)) 
        dir.create(outputDir)
    target_dir <- if (getOption("htmltools.dir.version", TRUE)) {
        paste(dependency$name, dependency$version, sep = "-")
    }
    else dependency$name
    target_dir <- file.path(outputDir, target_dir)
    if (dir_exists(target_dir)) 
        unlink(target_dir, recursive = TRUE)
    dir.create(target_dir)
    files <- if (dependency$all_files) 
        list.files(dir)
    else {
        unlist(dependency[c("script", "stylesheet", "attachment")])
    }
    srcfiles <- file.path(dir, files)
    destfiles <- file.path(target_dir, files)
    isdir <- file.info(srcfiles)$isdir
    destfiles <- ifelse(isdir, dirname(destfiles), destfiles)
    mapply(function(from, to, isdir) {
        if (!dir_exists(dirname(to))) 
            dir.create(dirname(to), recursive = TRUE)
        if (isdir && !dir_exists(to)) 
            dir.create(to)
        file.copy(from, to, overwrite = TRUE, recursive = isdir)
    }, srcfiles, destfiles, isdir)
    dependency$src$file <- normalizePath(target_dir, "/", TRUE)
    dependency
}


HTML <- function (text, ...) 
{
    htmlText <- c(text, as.character(list(...)))
    htmlText <- paste8(htmlText, collapse = " ")
    attr(htmlText, "html") <- TRUE
    class(htmlText) <- c("html", "character")
    htmlText
}


h5 <- function (...) 
tags$h5(...)


suppressDependencies <- function (...) 
{
    lapply(list(...), function(name) {
        attachDependencies(character(0), htmlDependency(name, 
            "9999", c(href = "")))
    })
}


h6 <- function (...) 
tags$h6(...)


withTags <- function (code) 
{
    eval(substitute(code), envir = as.list(tags), enclos = parent.frame())
}


is.browsable <- function (x) 
{
    return(isTRUE(attr(x, "browsable_html", exact = TRUE)))
}


save_html <- function (html, file, background = "white", libdir = "lib") 
{
    dir <- dirname(file)
    oldwd <- setwd(dir)
    on.exit(setwd(oldwd), add = TRUE)
    rendered <- renderTags(html)
    deps <- lapply(rendered$dependencies, function(dep) {
        dep <- copyDependencyToDir(dep, libdir, FALSE)
        dep <- makeDependencyRelative(dep, dir, FALSE)
        dep
    })
    html <- c("<!DOCTYPE html>", "<html>", "<head>", "<meta charset=\"utf-8\"/>", 
        renderDependencies(deps, c("href", "file")), rendered$head, 
        "</head>", sprintf("<body style=\"background-color:%s;\">", 
            htmlEscape(background)), rendered$html, "</body>", 
        "</html>")
    writeLines(html, file, useBytes = TRUE)
}


includeCSS <- function (path, ...) 
{
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    args <- list(...)
    if (is.null(args$type)) 
        args$type <- "text/css"
    return(do.call(tags$style, c(list(HTML(paste8(lines, collapse = "\r\n"))), 
        args)))
}


hr <- function (...) 
tags$hr(...)


doRenderTags <- function (x, indent = 0) 
{
    conn <- file(open = "w+b", encoding = "UTF-8")
    bytes <- 0
    connWriter <- function(text) {
        raw <- charToRaw(enc2utf8(text))
        bytes <<- bytes + length(raw)
        writeBin(raw, conn)
    }
    htmlResult <- tryCatch({
        tagWrite(x, connWriter, indent)
        flush(conn)
        bytes <- max(bytes - 1, 0)
        readChar(conn, bytes, useBytes = TRUE)
    }, finally = close(conn))
    Encoding(htmlResult) <- "UTF-8"
    return(HTML(htmlResult))
}


htmlDependencies <- function (x) 
{
    attr(x, "html_dependencies", TRUE)
}


includeText <- function (path) 
{
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    return(paste8(lines, collapse = "\r\n"))
}


html_print <- function (html, background = "white", viewer = getOption("viewer", 
    utils::browseURL)) 
{
    www_dir <- tempfile("viewhtml")
    dir.create(www_dir)
    index_html <- file.path(www_dir, "index.html")
    save_html(html, file = index_html, background = background, 
        libdir = "lib")
    if (!is.null(viewer)) 
        viewer(index_html)
    invisible(index_html)
}


is.singleton <- function (x) 
{
    isTRUE(attr(x, "htmltools.singleton"))
}


em <- function (...) 
tags$em(...)


htmlDependency <- function (name, version, src, meta = NULL, script = NULL, stylesheet = NULL, 
    head = NULL, attachment = NULL, all_files = TRUE) 
{
    if (isNamespace(parent.frame()) && any(substr(src, 1, 1) == 
        "/")) {
        warning("htmlDependency shouldn't be called from a namespace environment", 
            " with absolute paths (or paths from system.file()).", 
            " See ?htmlDependency for more information.")
    }
    version <- as.character(version)
    validateScalarName(name)
    validateScalarName(version)
    srcNames <- names(src)
    if (is.null(srcNames)) 
        srcNames <- rep.int("", length(src))
    srcNames[!nzchar(srcNames)] <- "file"
    names(src) <- srcNames
    src <- as.list(src)
    structure(class = "html_dependency", list(name = name, version = as.character(version), 
        src = src, meta = meta, script = script, stylesheet = stylesheet, 
        head = head, attachment = attachment, all_files = all_files))
}


tags <- structure(list(a = function (...) 
tag("a", list(...)), abbr = function (...) 
tag("abbr", list(...)), address = function (...) 
tag("address", list(...)), area = function (...) 
tag("area", list(...)), article = function (...) 
tag("article", list(...)), aside = function (...) 
tag("aside", list(...)), audio = function (...) 
tag("audio", list(...)), b = function (...) 
tag("b", list(...)), base = function (...) 
tag("base", list(...)), bdi = function (...) 
tag("bdi", list(...)), bdo = function (...) 
tag("bdo", list(...)), blockquote = function (...) 
tag("blockquote", list(...)), body = function (...) 
tag("body", list(...)), br = function (...) 
tag("br", list(...)), button = function (...) 
tag("button", list(...)), canvas = function (...) 
tag("canvas", list(...)), caption = function (...) 
tag("caption", list(...)), cite = function (...) 
tag("cite", list(...)), code = function (...) 
tag("code", list(...)), col = function (...) 
tag("col", list(...)), colgroup = function (...) 
tag("colgroup", list(...)), command = function (...) 
tag("command", list(...)), data = function (...) 
tag("data", list(...)), datalist = function (...) 
tag("datalist", list(...)), dd = function (...) 
tag("dd", list(...)), del = function (...) 
tag("del", list(...)), details = function (...) 
tag("details", list(...)), dfn = function (...) 
tag("dfn", list(...)), div = function (...) 
tag("div", list(...)), dl = function (...) 
tag("dl", list(...)), dt = function (...) 
tag("dt", list(...)), em = function (...) 
tag("em", list(...)), embed = function (...) 
tag("embed", list(...)), eventsource = function (...) 
tag("eventsource", list(...)), fieldset = function (...) 
tag("fieldset", list(...)), figcaption = function (...) 
tag("figcaption", list(...)), figure = function (...) 
tag("figure", list(...)), footer = function (...) 
tag("footer", list(...)), form = function (...) 
tag("form", list(...)), h1 = function (...) 
tag("h1", list(...)), h2 = function (...) 
tag("h2", list(...)), h3 = function (...) 
tag("h3", list(...)), h4 = function (...) 
tag("h4", list(...)), h5 = function (...) 
tag("h5", list(...)), h6 = function (...) 
tag("h6", list(...)), head = function (...) 
tag("head", list(...)), header = function (...) 
tag("header", list(...)), hgroup = function (...) 
tag("hgroup", list(...)), hr = function (...) 
tag("hr", list(...)), html = function (...) 
tag("html", list(...)), i = function (...) 
tag("i", list(...)), iframe = function (...) 
tag("iframe", list(...)), img = function (...) 
tag("img", list(...)), input = function (...) 
tag("input", list(...)), ins = function (...) 
tag("ins", list(...)), kbd = function (...) 
tag("kbd", list(...)), keygen = function (...) 
tag("keygen", list(...)), label = function (...) 
tag("label", list(...)), legend = function (...) 
tag("legend", list(...)), li = function (...) 
tag("li", list(...)), link = function (...) 
tag("link", list(...)), mark = function (...) 
tag("mark", list(...)), map = function (...) 
tag("map", list(...)), menu = function (...) 
tag("menu", list(...)), meta = function (...) 
tag("meta", list(...)), meter = function (...) 
tag("meter", list(...)), nav = function (...) 
tag("nav", list(...)), noscript = function (...) 
tag("noscript", list(...)), object = function (...) 
tag("object", list(...)), ol = function (...) 
tag("ol", list(...)), optgroup = function (...) 
tag("optgroup", list(...)), option = function (...) 
tag("option", list(...)), output = function (...) 
tag("output", list(...)), p = function (...) 
tag("p", list(...)), param = function (...) 
tag("param", list(...)), pre = function (...) 
tag("pre", list(...)), progress = function (...) 
tag("progress", list(...)), q = function (...) 
tag("q", list(...)), ruby = function (...) 
tag("ruby", list(...)), rp = function (...) 
tag("rp", list(...)), rt = function (...) 
tag("rt", list(...)), s = function (...) 
tag("s", list(...)), samp = function (...) 
tag("samp", list(...)), script = function (...) 
tag("script", list(...)), section = function (...) 
tag("section", list(...)), select = function (...) 
tag("select", list(...)), small = function (...) 
tag("small", list(...)), source = function (...) 
tag("source", list(...)), span = function (...) 
tag("span", list(...)), strong = function (...) 
tag("strong", list(...)), style = function (...) 
tag("style", list(...)), sub = function (...) 
tag("sub", list(...)), summary = function (...) 
tag("summary", list(...)), sup = function (...) 
tag("sup", list(...)), table = function (...) 
tag("table", list(...)), tbody = function (...) 
tag("tbody", list(...)), td = function (...) 
tag("td", list(...)), textarea = function (...) 
tag("textarea", list(...)), tfoot = function (...) 
tag("tfoot", list(...)), th = function (...) 
tag("th", list(...)), thead = function (...) 
tag("thead", list(...)), time = function (...) 
tag("time", list(...)), title = function (...) 
tag("title", list(...)), tr = function (...) 
tag("tr", list(...)), track = function (...) 
tag("track", list(...)), u = function (...) 
tag("u", list(...)), ul = function (...) 
tag("ul", list(...)), var = function (...) 
tag("var", list(...)), video = function (...) 
tag("video", list(...)), wbr = function (...) 
tag("wbr", list(...))), .Names = c("a", "abbr", "address", "area", 
"article", "aside", "audio", "b", "base", "bdi", "bdo", "blockquote", 
"body", "br", "button", "canvas", "caption", "cite", "code", 
"col", "colgroup", "command", "data", "datalist", "dd", "del", 
"details", "dfn", "div", "dl", "dt", "em", "embed", "eventsource", 
"fieldset", "figcaption", "figure", "footer", "form", "h1", "h2", 
"h3", "h4", "h5", "h6", "head", "header", "hgroup", "hr", "html", 
"i", "iframe", "img", "input", "ins", "kbd", "keygen", "label", 
"legend", "li", "link", "mark", "map", "menu", "meta", "meter", 
"nav", "noscript", "object", "ol", "optgroup", "option", "output", 
"p", "param", "pre", "progress", "q", "ruby", "rp", "rt", "s", 
"samp", "script", "section", "select", "small", "source", "span", 
"strong", "style", "sub", "summary", "sup", "table", "tbody", 
"td", "textarea", "tfoot", "th", "thead", "time", "title", "tr", 
"track", "u", "ul", "var", "video", "wbr"))


extractPreserveChunks <- function (strval) 
{
    startmarker <- "<!--html_preserve-->"
    endmarker <- "<!--/html_preserve-->"
    startmarker_len <- nchar(startmarker)
    endmarker_len <- nchar(endmarker)
    pattern <- "<!--/?html_preserve-->"
    if (length(strval) != 1) 
        strval <- paste(strval, collapse = "\n")
    matches <- gregexpr(pattern, strval)[[1]]
    lengths <- attr(matches, "match.length", TRUE)
    if (matches[[1]] == -1) 
        return(list(value = strval, chunks = character(0)))
    boundary_type <- lengths == startmarker_len
    preserve_level <- cumsum(ifelse(boundary_type, 1, -1))
    if (any(preserve_level < 0) || tail(preserve_level, 1) != 
        0) {
        stop("Invalid nesting of html_preserve directives")
    }
    is_top_level <- 1 == (preserve_level + c(0, preserve_level[-length(preserve_level)]))
    preserved <- character(0)
    top_level_matches <- matches[is_top_level]
    for (i in seq.int(length(top_level_matches) - 1, 1, by = -2)) {
        start_outer <- top_level_matches[[i]]
        start_inner <- start_outer + startmarker_len
        end_inner <- top_level_matches[[i + 1]]
        end_outer <- end_inner + endmarker_len
        id <- withPrivateSeed(paste("preserve", paste(format(as.hexmode(sample(256, 
            8, replace = TRUE) - 1), width = 2), collapse = ""), 
            sep = ""))
        preserved[id] <- gsub(pattern, "", substr(strval, start_inner, 
            end_inner - 1))
        strval <- paste(substr(strval, 1, start_outer - 1), id, 
            substr(strval, end_outer, nchar(strval)), sep = "")
        substr(strval, start_outer, end_outer - 1) <- id
    }
    list(value = strval, chunks = preserved)
}


restorePreserveChunks <- function (strval, chunks) 
{
    for (id in names(chunks)) strval <- gsub(id, chunks[[id]], 
        strval, fixed = TRUE, useBytes = TRUE)
    strval
}


htmlPreserve <- function (x) 
{
    x <- paste(x, collapse = "\r\n")
    if (nzchar(x)) 
        sprintf("<!--html_preserve-->%s<!--/html_preserve-->", 
            x)
    else x
}


tagGetAttribute <- function (tag, attr) 
{
    attribs <- tag$attribs
    attrIdx <- which(attr == names(attribs))
    if (length(attrIdx) == 0) {
        return(NULL)
    }
    result <- lapply(attribs[attrIdx], as.character)
    result <- paste(result, collapse = " ")
    result
}


resolveDependencies <- function (dependencies) 
{
    deps <- dependencies[!sapply(dependencies, is.null)]
    depnames <- sapply(deps, `[[`, "name")
    depvers <- numeric_version(sapply(deps, `[[`, "version"))
    return(lapply(unique(depnames), function(depname) {
        sorted <- order(ifelse(depnames == depname, TRUE, NA), 
            depvers, na.last = NA, decreasing = TRUE)
        deps[[sorted[[1]]]]
    }))
}


htmlEscape <- function (text, attribute = FALSE) 
{
    pattern <- if (attribute) 
        .htmlSpecialsPatternAttrib
    else .htmlSpecialsPattern
    if (!any(grepl(pattern, text, useBytes = TRUE))) 
        return(text)
    specials <- if (attribute) 
        .htmlSpecialsAttrib
    else .htmlSpecials
    for (chr in names(specials)) {
        text <- gsub(chr, specials[[chr]], text, fixed = TRUE, 
            useBytes = TRUE)
    }
    return(text)
}


takeSingletons <- function (ui, singletons = character(0), desingleton = TRUE) 
{
    result <- rewriteTags(ui, function(uiObj) {
        if (is.singleton(uiObj)) {
            sig <- digest(uiObj, "sha1")
            if (sig %in% singletons) 
                return(NULL)
            singletons <<- append(singletons, sig)
            if (desingleton) 
                uiObj <- singleton(uiObj, FALSE)
            return(uiObj)
        }
        else {
            return(uiObj)
        }
    }, TRUE)
    return(list(ui = result, singletons = singletons))
}


includeMarkdown <- function (path) 
{
    html <- markdown::markdownToHTML(path, fragment.only = TRUE)
    Encoding(html) <- "UTF-8"
    return(HTML(html))
}


code <- function (...) 
tags$code(...)


tagAppendChildren <- function (tag, ..., list = NULL) 
{
    tag$children <- c(tag$children, c(list(...), list))
    tag
}


knit_print.shiny.tag <- function (x, ...) 
{
    x <- tagify(x)
    output <- surroundSingletons(x)
    deps <- resolveDependencies(findDependencies(x))
    content <- takeHeads(output)
    head_content <- doRenderTags(tagList(content$head))
    meta <- if (length(head_content) > 1 || head_content != "") {
        list(structure(head_content, class = "shiny_head"))
    }
    meta <- c(meta, deps)
    knitr::asis_output(htmlPreserve(format(content$ui, indent = FALSE)), 
        meta = meta)
}


as.tags <- function (x, ...) 
{
    UseMethod("as.tags")
}


tagAppendChild <- function (tag, child) 
{
    tag$children[[length(tag$children) + 1]] <- child
    tag
}


br <- function (...) 
tags$br(...)


browsable <- function (x, value = TRUE) 
{
    attr(x, "browsable_html") <- if (isTRUE(value)) 
        TRUE
    else NULL
    return(x)
}


findDependencies <- function (tags) 
{
    dep <- htmlDependencies(tagify(tags))
    if (!is.null(dep) && inherits(dep, "html_dependency")) 
        dep <- list(dep)
    children <- if (is.list(tags)) {
        if (isTag(tags)) {
            tags$children
        }
        else {
            tags
        }
    }
    childDeps <- unlist(lapply(children, findDependencies), recursive = FALSE)
    c(childDeps, if (!is.null(dep)) dep)
}


singleton <- function (x, value = TRUE) 
{
    attr(x, "htmltools.singleton") <- if (isTRUE(value)) 
        TRUE
    else NULL
    return(x)
}


attachDependencies <- function (x, value, append = FALSE) 
{
    if (append) {
        if (inherits(value, "html_dependency")) 
            value <- list(value)
        old <- attr(x, "html_dependencies", TRUE)
        htmlDependencies(x) <- c(old, value)
    }
    else {
        htmlDependencies(x) <- value
    }
    return(x)
}


tagAppendAttributes <- function (tag, ...) 
{
    tag$attribs <- c(tag$attribs, list(...))
    tag
}


tagSetChildren <- function (tag, ..., list = NULL) 
{
    tag$children <- c(list(...), list)
    tag
}


renderDocument <- function (x, deps = NULL, processDep = identity) 
{
    if (!inherits(x, "html_document")) {
        stop("Object must be an object of class html_document")
    }
    if (inherits(deps, "html_dependency")) {
        deps <- list(deps)
    }
    result <- renderTags(x)
    deps <- c(deps, result$dependencies)
    deps <- resolveDependencies(deps)
    deps <- lapply(deps, processDep)
    depStr <- paste(sapply(deps, function(dep) {
        sprintf("%s[%s]", dep$name, dep$version)
    }), collapse = ";")
    depHtml <- renderDependencies(deps, "href")
    head_content <- paste0("  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\"/>", 
        sprintf("  <script type=\"application/shiny-singletons\">%s</script>", 
            paste(result$singletons, collapse = ",")), sprintf("  <script type=\"application/html-dependencies\">%s</script>", 
            depStr), depHtml, c(result$head, recursive = TRUE))
    sub("<!-- HEAD_CONTENT -->", head_content, result$html, fixed = TRUE)
}


div <- function (...) 
tags$div(...)


surroundSingletons <- function (ui) 
{
    rewriteTags(ui, surroundSingleton, TRUE)
}


a <- function (...) 
tags$a(...)


htmlTemplate <- function (filename = NULL, ..., text_ = NULL, document_ = "auto") 
{
    if (!xor(is.null(filename), is.null(text_))) {
        stop("htmlTemplate requires either `filename` or `text_`.")
    }
    if (!is.null(filename)) {
        html <- readChar(filename, file.info(filename)$size, 
            useBytes = TRUE)
        Encoding(html) <- "UTF-8"
    }
    else if (!is.null(text_)) {
        text_ <- paste8(text_, collapse = "\n")
        html <- enc2utf8(text_)
    }
    pieces <- template_dfa(html)
    Encoding(pieces) <- "UTF-8"
    vars <- list(...)
    if ("headContent" %in% names(vars)) {
        stop("Can't use reserved argument name 'headContent'.")
    }
    vars$headContent <- function() HTML("<!-- HEAD_CONTENT -->")
    env <- list2env(vars, parent = globalenv())
    pieces <- mapply(pieces, rep_len(c(FALSE, TRUE), length.out = length(pieces)), 
        FUN = function(piece, isCode) {
            if (isCode) {
                eval(parse(text = piece), env)
            }
            else if (piece == "") {
                NULL
            }
            else {
                HTML(piece)
            }
        }, SIMPLIFY = FALSE)
    result <- tagList(pieces)
    if (document_ == "auto") {
        document_ = grepl("<HTML(\\s[^<]*)?>", html, ignore.case = TRUE)
    }
    if (document_) {
        class(result) <- c("html_document", class(result))
    }
    result
}


includeScript <- function (path, ...) 
{
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    return(tags$script(HTML(paste8(lines, collapse = "\r\n")), 
        ...))
}


knit_print.shiny.tag.list <- function (x, ...) 
{
    x <- tagify(x)
    output <- surroundSingletons(x)
    deps <- resolveDependencies(findDependencies(x))
    content <- takeHeads(output)
    head_content <- doRenderTags(tagList(content$head))
    meta <- if (length(head_content) > 1 || head_content != "") {
        list(structure(head_content, class = "shiny_head"))
    }
    meta <- c(meta, deps)
    knitr::asis_output(htmlPreserve(format(content$ui, indent = FALSE)), 
        meta = meta)
}


strong <- function (...) 
tags$strong(...)


pre <- function (...) 
tags$pre(...)


subtractDependencies <- function (dependencies, remove, warnOnConflict = TRUE) 
{
    depnames <- sapply(dependencies, `[[`, "name")
    rmnames <- if (is.character(remove)) 
        remove
    else sapply(remove, `[[`, "name")
    matches <- depnames %in% rmnames
    if (warnOnConflict && !is.character(remove)) {
        for (loser in dependencies[matches]) {
            winner <- remove[[head(rmnames == loser$name, 1)]]
            if (compareVersion(loser$version, winner$version) > 
                0) {
                warning(sprintf(paste("The dependency %s %s conflicts with", 
                  "version %s"), loser$name, loser$version, winner$version))
            }
        }
    }
    return(dependencies[!matches])
}


makeDependencyRelative <- function (dependency, basepath, mustWork = TRUE) 
{
    basepath <- normalizePath(basepath, "/", TRUE)
    dir <- dependency$src$file
    if (is.null(dir)) {
        if (!mustWork) 
            return(dependency)
        else stop("Could not make dependency ", dependency$name, 
            " ", dependency$version, " relative; it is not file-based")
    }
    dependency$src <- c(file = relativeTo(basepath, dir))
    dependency
}


knit_print.html <- function (x, ...) 
{
    deps <- resolveDependencies(findDependencies(x))
    knitr::asis_output(htmlPreserve(as.character(x)), meta = if (length(deps)) 
        list(deps))
}


urlEncodePath <- function (x) 
{
    vURLEncode <- Vectorize(URLencode, USE.NAMES = FALSE)
    gsub("%2[Ff]", "/", vURLEncode(x, TRUE))
}


css <- function (..., collapse_ = "") 
{
    props <- list(...)
    if (length(props) == 0) {
        return("")
    }
    if (is.null(names(props)) || any(names(props) == "")) {
        stop("cssList expects all arguments to be named")
    }
    props[] <- lapply(props, paste, collapse = " ")
    props <- props[!sapply(props, empty)]
    if (length(props) == 0) {
        return("")
    }
    names(props) <- gsub("[._]", "-", tolower(gsub("([A-Z])", 
        "-\\1", names(props))))
    important <- ifelse(grepl("!$", names(props), perl = TRUE), 
        " !important", "")
    names(props) <- sub("!$", "", names(props), perl = TRUE)
    paste0(names(props), ":", props, important, ";", collapse = collapse_)
}


p <- function (...) 
tags$p(...)


validateCssUnit <- function (x) 
{
    if (is.null(x) || is.na(x)) 
        return(x)
    if (length(x) > 1 || (!is.character(x) && !is.numeric(x))) 
        stop("CSS units must be a single-element numeric or character vector")
    if (is.character(x) && nchar(x) > 0 && gsub("\\d*", "", x) == 
        "") 
        x <- as.numeric(x)
    pattern <- "^(auto|inherit|((\\.\\d+)|(\\d+(\\.\\d+)?))(%|in|cm|mm|em|ex|pt|pc|px))$"
    if (is.character(x) && !grepl(pattern, x)) {
        stop("\"", x, "\" is not a valid CSS unit (e.g., \"100%\", \"400px\", \"auto\")")
    }
    else if (is.numeric(x)) {
        x <- paste(x, "px", sep = "")
    }
    x
}


img <- function (...) 
tags$img(...)




## Package Data

# none


## Package Info

.skeleton_package_title = "Tools for HTML"

.skeleton_package_version = "0.3.5"

.skeleton_package_depends = ""

.skeleton_package_imports = "utils,digest,Rcpp"


## Internal

.skeleton_version = 5


## EOF