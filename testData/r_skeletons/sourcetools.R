##
## Exported symobls in package `sourcetools`
##

## Exported package methods

tokenize_string <- function (string) 
{
    .Call("sourcetools_tokenize_string", as.character(string), 
        PACKAGE = "sourcetools")
}


read <- function (path) 
{
    path <- normalizePath(path, mustWork = TRUE)
    .Call("sourcetools_read", path, PACKAGE = "sourcetools")
}


tokenize_file <- function (path) 
{
    path <- normalizePath(path, mustWork = TRUE)
    .Call("sourcetools_tokenize_file", path, PACKAGE = "sourcetools")
}


tokenize <- function (file = "", text = NULL) 
{
    if (is.null(text)) 
        text <- read(file)
    tokenize_string(text)
}


read_lines <- function (path) 
{
    path <- normalizePath(path, mustWork = TRUE)
    .Call("sourcetools_read_lines", path, PACKAGE = "sourcetools")
}


read_lines_bytes <- function (path) 
{
    path <- normalizePath(path, mustWork = TRUE)
    .Call("sourcetools_read_lines_bytes", path, PACKAGE = "sourcetools")
}


read_bytes <- function (path) 
{
    path <- normalizePath(path, mustWork = TRUE)
    .Call("sourcetools_read_bytes", path, PACKAGE = "sourcetools")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Tools for Reading, Tokenizing and Parsing R Code"

.skeleton_package_version = "0.1.5"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF