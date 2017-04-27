##
## Exported symobls in package `yaml`
##

## Exported package methods

as.yaml <- function (x, line.sep = c("\n", "\r\n", "\r"), indent = 2, omap = FALSE, 
    column.major = TRUE, unicode = TRUE, precision = getOption("digits"), 
    indent.mapping.sequence = FALSE) 
{
    line.sep <- match.arg(line.sep)
    .Call("as.yaml", x, line.sep, indent, omap, column.major, 
        unicode, precision, indent.mapping.sequence, PACKAGE = "yaml")
}


yaml.load <- function (string, as.named.list = TRUE, handlers = NULL) 
{
    .Call("yaml.load", enc2utf8(string), as.named.list, handlers, 
        PACKAGE = "yaml")
}


yaml.load_file <- function (input, ...) 
{
    yaml.load(paste(readLines(input, encoding = "UTF-8"), collapse = "\n"), 
        ...)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Methods to Convert R Data to YAML and Back"

.skeleton_package_version = "2.1.14"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF