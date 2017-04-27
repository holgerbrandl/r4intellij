##
## Exported symobls in package `whisker`
##

## Exported package methods

rowSplit <- function (x, ...) 
{
    split(x, seq_len(nrow(x)), ...)
}


whisker.render <- function (template, data = parent.frame(), partials = list(), 
    debug = FALSE) 
{
    if (is.null(template) || template == "") {
        return("")
    }
    tmpl <- parseTemplate(template, partials = as.environment(partials), 
        debug = debug)
    return(tmpl(data))
}


iteratelist <- function (x, name = "name", value = "value") 
{
    x <- as.list(x)
    nms <- names(x)
    lapply(seq_along(x), function(i) {
        l <- list()
        l[name] <- nms[i]
        l[value] <- x[i]
        l
    })
}


whisker.escape <- function (x) 
{
    x <- gsub("&", "&amp;", x)
    x <- gsub("<", "&lt;", x)
    x <- gsub(">", "&gt;", x)
    x <- gsub("\"", "&quot;", x)
    x
}




## Package Data

# none


## Package Info

.skeleton_package_title = "{{mustache}} for R, logicless templating"

.skeleton_package_version = "0.3-2"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF