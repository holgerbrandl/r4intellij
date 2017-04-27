##
## Exported symobls in package `repr`
##

## Exported package methods

repr_option_defaults <- structure(list(repr.plot.width = 7, repr.plot.height = 7, repr.plot.pointsize = 12, 
    repr.plot.bg = "white", repr.plot.antialias = "gray", repr.plot.res = 120, 
    repr.plot.quality = 90, repr.plot.family = "sans", repr.vector.quote = TRUE, 
    repr.matrix.max.rows = 60, repr.matrix.max.cols = 20, repr.matrix.latex.colspec = structure(list(
        row_head = "r|", col = "l", end = ""), .Names = c("row_head", 
    "col", "end")), repr.function.highlight = FALSE), .Names = c("repr.plot.width", 
"repr.plot.height", "repr.plot.pointsize", "repr.plot.bg", "repr.plot.antialias", 
"repr.plot.res", "repr.plot.quality", "repr.plot.family", "repr.vector.quote", 
"repr.matrix.max.rows", "repr.matrix.max.cols", "repr.matrix.latex.colspec", 
"repr.function.highlight"))


repr_jpg <- function (obj, ...) 
UseMethod("repr_jpg", obj)


repr_pdf <- function (obj, ...) 
UseMethod("repr_pdf", obj)


repr_markdown <- function (obj, ...) 
UseMethod("repr_markdown", obj)


repr_javascript <- function (obj, ...) 
UseMethod("repr_javascript", obj)


mime2repr <- structure(list(`text/plain` = function (obj, ...) 
UseMethod("repr_text", obj), `text/html` = function (obj, ...) 
UseMethod("repr_html", obj), `text/markdown` = function (obj, 
    ...) 
UseMethod("repr_markdown", obj), `text/latex` = function (obj, 
    ...) 
UseMethod("repr_latex", obj), `application/json` = function (obj, 
    ...) 
UseMethod("repr_json", obj), `application/javascript` = function (obj, 
    ...) 
UseMethod("repr_javascript", obj), `application/pdf` = function (obj, 
    ...) 
UseMethod("repr_pdf", obj), `image/png` = function (obj, ...) 
UseMethod("repr_png", obj), `image/jpeg` = function (obj, ...) 
UseMethod("repr_jpg", obj), `image/svg+xml` = function (obj, 
    ...) 
UseMethod("repr_svg", obj)), .Names = c("text/plain", "text/html", 
"text/markdown", "text/latex", "application/json", "application/javascript", 
"application/pdf", "image/png", "image/jpeg", "image/svg+xml"
))


repr_svg <- function (obj, ...) 
UseMethod("repr_svg", obj)


format2repr <- structure(list(text = function (obj, ...) 
UseMethod("repr_text", obj), html = function (obj, ...) 
UseMethod("repr_html", obj), markdown = function (obj, ...) 
UseMethod("repr_markdown", obj), latex = function (obj, ...) 
UseMethod("repr_latex", obj), json = function (obj, ...) 
UseMethod("repr_json", obj), javascript = function (obj, ...) 
UseMethod("repr_javascript", obj), pdf = function (obj, ...) 
UseMethod("repr_pdf", obj), png = function (obj, ...) 
UseMethod("repr_png", obj), jpg = function (obj, ...) 
UseMethod("repr_jpg", obj), svg = function (obj, ...) 
UseMethod("repr_svg", obj)), .Names = c("text", "html", "markdown", 
"latex", "json", "javascript", "pdf", "png", "jpg", "svg"))


repr_text <- function (obj, ...) 
UseMethod("repr_text", obj)


repr_html <- function (obj, ...) 
UseMethod("repr_html", obj)


repr_png <- function (obj, ...) 
UseMethod("repr_png", obj)


repr <- function (obj, format = "text", ...) 
{
    delegate <- format2repr[[format]]
    if (is.null(delegate)) 
        stop(sprintf("Repr format %s not known", format))
    delegate(obj, ...)
}


repr_latex <- function (obj, ...) 
UseMethod("repr_latex", obj)


repr_json <- function (obj, ...) 
UseMethod("repr_json", obj)




## Package Data

# none


## Package Info

.skeleton_package_title = "Serializable Representations"

.skeleton_package_version = "0.10"

.skeleton_package_depends = ""

.skeleton_package_imports = "utils,grDevices"


## Internal

.skeleton_version = 5


## EOF