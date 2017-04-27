##
## Exported symobls in package `rstudioapi`
##

## Exported package methods

getActiveProject <- function () 
{
    callFun("getActiveProject")
}


getVersion <- function () 
{
    verifyAvailable()
    callFun("versionInfo")$version
}


previewRd <- function (rdFile) 
{
    callFun("previewRd", rdFile)
}


is.document_position <- function (x) 
{
    inherits(x, "document_position")
}


sendToConsole <- function (code, execute = TRUE) 
{
    callFun("sendToConsole", code, TRUE, execute, TRUE)
}


findFun <- function (name, version_needed = NULL, ...) 
{
    verifyAvailable(version_needed)
    if (usingTools()) 
        get(toolsName(name), toolsEnv(), ...)
    else get(name, envir = asNamespace("rstudio"), ...)
}


getConsoleEditorContext <- function () 
{
    context <- callFun("getConsoleEditorContext")
    context$selection <- as.document_selection(context$selection)
    structure(context, class = "document_context")
}


document_position <- function (row, column) 
{
    structure(c(row = as.numeric(row), column = as.numeric(column)), 
        class = "document_position")
}


isAvailable <- function (version_needed = NULL) 
{
    identical(.Platform$GUI, "RStudio") && version_ok(version_needed)
}


setCursorPosition <- function (position, id = NULL) 
{
    callFun("setSelectionRanges", position, id)
}


modifyRange <- function (location, text, id = NULL) 
{
    callFun("insertText", location, text, id)
}


insertText <- function (location, text, id = NULL) 
{
    callFun("insertText", location, text, id)
}


getSourceEditorContext <- function () 
{
    context <- callFun("getSourceEditorContext")
    context$selection <- as.document_selection(context$selection)
    structure(context, class = "document_context")
}


verifyAvailable <- function (version_needed = NULL) 
{
    if (!isAvailable()) 
        stop("RStudio not running", call. = FALSE)
    if (!version_ok(version_needed)) {
        stop("Need at least version ", version_needed, " of RStudio. ", 
            "Currently running ", getVersion(), call. = FALSE)
    }
    invisible(TRUE)
}


setSelectionRanges <- function (ranges, id = NULL) 
{
    callFun("setSelectionRanges", ranges, id)
}


navigateToFile <- function (file, line = 1L, column = 1L) 
{
    callFun("navigateToFile", file, as.integer(line), as.integer(column))
}


viewer <- function (url, height = NULL) 
{
    callFun("viewer", url, height = height)
}


callFun <- function (fname, ...) 
{
    verifyAvailable()
    if (usingTools()) 
        found <- exists(toolsName(fname), envir = toolsEnv(), 
            mode = "function")
    else found <- exists(fname, envir = asNamespace("rstudio"), 
        mode = "function")
    if (!found) 
        stop("Function ", fname, " not found in RStudio", call. = FALSE)
    f <- findFun(fname, mode = "function")
    f(...)
}


as.document_range <- function (x) 
{
    UseMethod("as.document_range")
}


setDocumentContents <- function (text, id = NULL) 
{
    location <- document_range(document_position(1, 1), document_position(Inf, 
        1))
    insertText(location, text, id)
}


getActiveDocumentContext <- function () 
{
    context <- callFun("getActiveDocumentContext")
    context$selection <- as.document_selection(context$selection)
    structure(context, class = "document_context")
}


primary_selection <- function (x, ...) 
{
    UseMethod("primary_selection")
}


hasFun <- function (name, version_needed = NULL, ...) 
{
    if (!isAvailable(version_needed)) 
        return(FALSE)
    if (usingTools()) 
        exists(toolsName(name), toolsEnv(), ...)
    else exists(name, envir = asNamespace("rstudio"), ...)
}


sourceMarkers <- function (name, markers, basePath = NULL, autoSelect = c("none", 
    "first", "error")) 
{
    callFun("sourceMarkers", name, markers, basePath, autoSelect)
}


is.document_range <- function (x) 
{
    inherits(x, "document_range")
}


as.document_position <- function (x) 
{
    UseMethod("as.document_position")
}


versionInfo <- function () 
{
    callFun("versionInfo")
}


document_range <- function (start, end = NULL) 
{
    if (is.null(end)) {
        if (length(start) != 4 || !is.numeric(start)) 
            stop("invalid range specification", call. = FALSE)
        end <- start[3:4]
        start <- start[1:2]
    }
    structure(list(start = as.document_position(start), end = as.document_position(end)), 
        class = "document_range")
}


askForPassword <- function (prompt) 
{
    callFun("askForPassword", prompt)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Safely Access the RStudio API"

.skeleton_package_version = "0.6"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF