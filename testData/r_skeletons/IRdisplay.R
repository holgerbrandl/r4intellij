##
## Exported symobls in package `IRdisplay`
##

## Exported package methods

display_json <- function (data = NULL, file = NULL) 
display_raw("application/json", FALSE, data, file)


publish_mimebundle <- function (data, metadata = NULL) 
{
    getOption("jupyter.base_display_func")(data, metadata)
}


prepare_mimebundle <- function (obj, mimetypes = getOption("jupyter.display_mimetypes"), 
    metadata = NULL, error_handler = stop) 
{
    if (length(mimetypes) == 0L) 
        stop("option jupyter.display_mimetypes may not be NULL or of length 0")
    outer_handler <- if (identical(error_handler, stop)) 
        stop
    else function(e) {
    }
    data <- filter_map(mimetypes, function(mime) {
        tryCatch(withCallingHandlers({
            rpr <- mime2repr[[mime]](obj)
            if (is.null(rpr)) 
                return(NULL)
            prepare_content(is.raw(rpr), rpr)
        }, error = error_handler), error = outer_handler)
    })
    list(data = data, metadata = isolate_full_html(data, metadata))
}


irdisplay_option_defaults <- structure(list(jupyter.display_mimetypes = c("text/plain", "text/html", 
"text/markdown", "text/latex", "application/json", "application/javascript", 
"application/pdf", "image/png", "image/jpeg", "image/svg+xml"
), jupyter.base_display_func = function (data, metadata = NULL) 
{
    warning("IRdisplay can only be used from the IPython R kernel and R magic.")
}), .Names = c("jupyter.display_mimetypes", "jupyter.base_display_func"
))


display_jpeg <- function (data = NULL, file = NULL, width = NULL, height = NULL) 
display_raw("image/jpeg", TRUE, data, file, img_metadata(width, 
    height))


display_svg <- function (data = NULL, file = NULL, width = NULL, height = NULL) 
display_raw("image/svg+xml", FALSE, data, file, img_metadata(width, 
    height))


display_pdf <- function (data = NULL, file = NULL, width = NULL, height = NULL) 
display_raw("application/pdf", TRUE, data, file, img_metadata(width, 
    height))


display_latex <- function (data = NULL, file = NULL) 
display_raw("text/latex", FALSE, data, file)


display_html <- function (data = NULL, file = NULL) 
display_raw("text/html", FALSE, data, file, isolate_full_html(list(`text/html` = data)))


display <- function (obj) 
{
    bundle <- prepare_mimebundle(obj)
    publish_mimebundle(bundle$data, bundle$metadata)
}


display_javascript <- function (data = NULL, file = NULL) 
display_raw("application/javascript", FALSE, data, file)


display_png <- function (data = NULL, file = NULL, width = NULL, height = NULL) 
display_raw("image/png", TRUE, data, file, img_metadata(width, 
    height))


display_markdown <- function (data = NULL, file = NULL) 
display_raw("text/markdown", FALSE, data, file)




## Package Data

# none


## Package Info

.skeleton_package_title = "'Jupyter' Display Machinery"

.skeleton_package_version = "0.4.4"

.skeleton_package_depends = ""

.skeleton_package_imports = "repr"


## Internal

.skeleton_version = 5


## EOF