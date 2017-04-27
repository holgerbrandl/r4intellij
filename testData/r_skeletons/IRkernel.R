##
## Exported symobls in package `IRkernel`
##

## Exported package methods

installspec <- function (user = TRUE, name = "ir", displayname = "R") 
{
    jupyter <- find_jupyter()
    if (is.null(jupyter)) 
        stop(paste0("Jupyter or IPython 3.0 has to be installed but could neither run ", 
            dQuote("jupyter"), " nor ", dQuote("ipython"), ", ", 
            dQuote("ipython2"), " or ", dQuote("ipython3"), ".\n", 
            "(Note that ", dQuote("ipython2"), " is just IPython for Python 2, but still may be IPython 3.0)"))
    srcdir <- system.file("kernelspec", package = "IRkernel")
    tmp_name <- tempfile()
    dir.create(tmp_name)
    file.copy(srcdir, tmp_name, recursive = TRUE)
    spec_path <- file.path(tmp_name, "kernelspec", "kernel.json")
    spec <- fromJSON(spec_path)
    spec$argv[[1]] <- file.path(R.home("bin"), "R")
    spec$display_name <- displayname
    write(toJSON(spec, pretty = TRUE, auto_unbox = TRUE), file = spec_path)
    user_flag <- if (user) 
        "--user"
    else character(0)
    args <- c("kernelspec", "install", "--replace", "--name", 
        name, user_flag, file.path(tmp_name, "kernelspec"))
    exit_code <- system2(jupyter$binary, args)
    unlink(tmp_name, recursive = TRUE)
    invisible(exit_code)
}


comm_manager <- function () 
runtime_env$comm_manager


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

log_info <- function (...) 
{
    if (isTRUE(getOption("jupyter.log_level") >= 2L)) {
        log_msg("INFO", sprintf(...))
    }
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

.__C__CommManager <- new("refClassRepresentation"
    , fieldClasses = structure(list(send_response = "function", target_to_handler_map = "list", 
    commid_to_comm = "list", parent_request = "list"), .Names = c("send_response", 
"target_to_handler_map", "commid_to_comm", "parent_request"))
    , fieldPrototypes =  "<environment>"
    , refMethods =  "<environment>"
    , refSuperClasses = "envRefClass"
    , slots = structure(list(.xData = structure("environment", package = "methods")), .Names = ".xData")
    , contains = structure(list(envRefClass = S4_object(), 
    .environment = S4_object(), 
    refClass = S4_object(), 
    environment = S4_object(), 
    refObject = S4_object()), .Names = c("envRefClass", 
".environment", "refClass", "environment", "refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("CommManager", package = "IRkernel")
    , package = "IRkernel"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


main <- function (connection_file = "") 
{
    if (connection_file == "") {
        connection_file <- commandArgs(TRUE)[[1]]
    }
    log_debug("Starting the R kernel...")
    kernel <- Kernel$new(connection_file = connection_file)
    kernel$run()
}


log_error <- function (...) 
{
    if (isTRUE(getOption("jupyter.log_level") >= 1L)) {
        log_msg("ERROR", sprintf(...))
    }
}


log_debug <- function (...) 
{
    if (isTRUE(getOption("jupyter.log_level") >= 3L)) {
        log_msg("DEBUG", sprintf(...))
    }
}


jupyter_option_defaults <- structure(list(jupyter.rich_display = TRUE, jupyter.log_level = 1L, 
    jupyter.logfile = NA, jupyter.pager_classes = c("packageIQR", 
    "help_files_with_topic"), jupyter.plot_mimetypes = c("text/plain", 
    "image/png"), jupyter.in_kernel = FALSE), .Names = c("jupyter.rich_display", 
"jupyter.log_level", "jupyter.logfile", "jupyter.pager_classes", 
"jupyter.plot_mimetypes", "jupyter.in_kernel"))


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

.__C__Comm <- new("refClassRepresentation"
    , fieldClasses = structure(list(id = "character", target_name = "character", comm_manager = "CommManager", 
    msg_callback = "functionOrNULL", close_callback = "functionOrNULL"), .Names = c("id", 
"target_name", "comm_manager", "msg_callback", "close_callback"
))
    , fieldPrototypes =  "<environment>"
    , refMethods =  "<environment>"
    , refSuperClasses = "envRefClass"
    , slots = structure(list(.xData = structure("environment", package = "methods")), .Names = ".xData")
    , contains = structure(list(envRefClass = S4_object(), 
    .environment = S4_object(), 
    refClass = S4_object(), 
    environment = S4_object(), 
    refObject = S4_object()), .Names = c("envRefClass", 
".environment", "refClass", "environment", "refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("Comm", package = "IRkernel")
    , package = "IRkernel"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package



## Package Data

# none


## Package Info

.skeleton_package_title = "Native R Kernel for the 'Jupyter Notebook'"

.skeleton_package_version = "0.7.1"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF