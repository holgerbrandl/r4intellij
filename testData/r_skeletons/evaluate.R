##
## Exported symobls in package `evaluate`
##

## Exported package methods

create_traceback <- function (callstack) 
{
    if (length(callstack) == 0) 
        return()
    calls <- lapply(callstack, deparse, width = 500)
    calls <- sapply(calls, str_c, collapse = "\n")
    calls <- str_c(seq_along(calls), ": ", calls)
    calls <- str_replace(calls, "\n", "\n   ")
    calls
}


set_hooks <- function (hooks, action = "append") 
{
    stopifnot(is.list(hooks))
    stopifnot(!is.null(names(hooks)) && all(names(hooks) != ""))
    old <- list()
    for (hook_name in names(hooks)) {
        old[[hook_name]] <- getHook(hook_name)
        setHook(hook_name, hooks[[hook_name]], action = action)
    }
    invisible(old)
}


is.warning <- function (x) 
inherits(x, "warning")


is.source <- function (x) 
inherits(x, "source")


is.message <- function (x) 
inherits(x, "message")


is.error <- function (x) 
inherits(x, "error")


replay <- function (x) 
UseMethod("replay", x)


flush_console <- function () 
.env$flush_console()


new_output_handler <- function (source = identity, text = identity, graphics = identity, 
    message = identity, warning = identity, error = identity, 
    value = render) 
{
    source <- match.fun(source)
    stopifnot(length(formals(source)) >= 1)
    text <- match.fun(text)
    stopifnot(length(formals(text)) >= 1)
    graphics <- match.fun(graphics)
    stopifnot(length(formals(graphics)) >= 1)
    message <- match.fun(message)
    stopifnot(length(formals(message)) >= 1)
    warning <- match.fun(warning)
    stopifnot(length(formals(warning)) >= 1)
    error <- match.fun(error)
    stopifnot(length(formals(error)) >= 1)
    value <- match.fun(value)
    stopifnot(length(formals(value)) >= 1)
    structure(list(source = source, text = text, graphics = graphics, 
        message = message, warning = warning, error = error, 
        value = value), class = "output_handler")
}


inject_funs <- function (...) 
{
    funs <- list(...)
    funs <- funs[names(funs) != ""]
    .env$inject_funs <- Filter(is.function, funs)
}


is.recordedplot <- function (x) 
inherits(x, "recordedplot")


try_capture_stack <- function (quoted_code, env) 
{
    capture_calls <- function(e) {
        e$calls <- head(sys.calls()[-seq_len(frame + 7)], -2)
        signalCondition(e)
    }
    frame <- sys.nframe()
    tryCatch(withCallingHandlers(eval(quoted_code, env), error = capture_calls), 
        error = identity)
}


parse_all <- function (x, filename = NULL, allow_error = FALSE) 
UseMethod("parse_all")


is.value <- function (x) 
inherits(x, "value")


evaluate <- function (input, envir = parent.frame(), enclos = NULL, debug = FALSE, 
    stop_on_error = 0L, keep_warning = TRUE, keep_message = TRUE, 
    new_device = TRUE, output_handler = default_output_handler, 
    filename = NULL, include_timing = FALSE) 
{
    stop_on_error <- as.integer(stop_on_error)
    stopifnot(length(stop_on_error) == 1)
    parsed <- parse_all(input, filename, stop_on_error != 2L)
    if (inherits(err <- attr(parsed, "PARSE_ERROR"), "error")) {
        source <- new_source(parsed$src)
        output_handler$source(source)
        output_handler$error(err)
        err$call <- NULL
        return(list(source, err))
    }
    if (is.null(enclos)) {
        enclos <- if (is.list(envir) || is.pairlist(envir)) 
            parent.frame()
        else baseenv()
    }
    if (new_device) {
        if (identical(grDevices::pdf, getOption("device"))) {
            dev.new(file = NULL)
        }
        else dev.new()
        dev.control(displaylist = "enable")
        dev <- dev.cur()
        on.exit(dev.off(dev))
    }
    on.exit(assign("last_plot", NULL, envir = environment(plot_snapshot)), 
        add = TRUE)
    out <- vector("list", nrow(parsed))
    for (i in seq_along(out)) {
        expr <- parsed$expr[[i]]
        if (!is.null(expr)) 
            expr <- as.expression(expr)
        out[[i]] <- evaluate_call(expr, parsed$src[[i]], envir = envir, 
            enclos = enclos, debug = debug, last = i == length(out), 
            use_try = stop_on_error != 2L, keep_warning = keep_warning, 
            keep_message = keep_message, output_handler = output_handler, 
            include_timing = include_timing)
        if (stop_on_error > 0L) {
            errs <- vapply(out[[i]], is.error, logical(1))
            if (!any(errs)) 
                next
            if (stop_on_error == 1L) 
                break
        }
    }
    unlist(out, recursive = FALSE, use.names = FALSE)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Parsing and Evaluation Tools that Provide More Details than theDefault"

.skeleton_package_version = "0.10"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods,stringr"


## Internal

.skeleton_version = 5


## EOF