##
## Exported symobls in package `withr`
##

## Exported package methods

with_path <- function (new, code, action = "prefix") 
{
    old <- set_path(path = new, action = action)
    on.exit((function(old) set_path(old, "replace"))(old))
    force(code)
}


with_libpaths <- function (new, code, action = "replace") 
{
    old <- set_libpaths(paths = new, action = action)
    on.exit(.libPaths(old))
    force(code)
}


with_makevars <- function (new, code, path = file.path("~", ".R", "Makevars"), 
    assignment = c("=", ":=", "?=", "+=")) 
{
    assignment <- match.arg(assignment)
    makevars_file <- tempfile()
    on.exit(unlink(makevars_file), add = TRUE)
    with_envvar(c(R_MAKEVARS_USER = makevars_file), {
        set_makevars(new, path, makevars_file, assignment = assignment)
        force(code)
    })
}


with_output_sink <- function (new, code, append = FALSE, split = FALSE) 
{
    old <- set_output_sink(file = new, append = append, split = split)
    on.exit(reset_output_sink(old))
    force(code)
}


with_options <- function (new, code) 
{
    old <- set_options(new_options = new)
    on.exit(set_options(old))
    force(code)
}


with_dir <- function (new, code) 
{
    old <- setwd(dir = new)
    on.exit(setwd(old))
    force(code)
}


with_ <- function (set, reset = set, envir = parent.frame()) 
{
    fmls <- formals(set)
    if (length(fmls) > 0L) {
        called_fmls <- stats::setNames(lapply(names(fmls), as.symbol), 
            names(fmls))
        called_fmls[[1]] <- as.symbol("new")
        fun_args <- c(alist(new = , code = ), fmls[-1L])
    }
    else {
        called_fmls <- NULL
        fun_args <- alist(code = )
    }
    set_call <- as.call(c(substitute(set), called_fmls))
    fun <- eval(bquote(function(args) {
        old <- .(set_call)
        on.exit(.(reset)(old))
        force(code)
    }, as.environment(list(set_call = set_call, reset = if (missing(reset)) substitute(set) else substitute(reset)))))
    formals(fun) <- fun_args
    environment(fun) <- envir
    fun
}


with_par <- function (new, code, no.readonly = FALSE) 
{
    old <- graphics::par("..." = new, no.readonly = no.readonly)
    on.exit(graphics::par(old))
    force(code)
}


with_envvar <- function (new, code, action = "replace") 
{
    old <- set_envvar(envs = new, action = action)
    on.exit(set_envvar(old))
    force(code)
}


with_collate <- function (new, code) 
{
    old <- set_collate(locale = new)
    on.exit(set_collate(old))
    force(code)
}


with_temp_libpaths <- function (code) 
{
    old <- set_temp_libpath()
    on.exit(.libPaths(old))
    force(code)
}


with_locale <- function (new, code) 
{
    old <- set_locale(cats = new)
    on.exit(set_locale(old))
    force(code)
}


with_message_sink <- function (new, code, append = FALSE) 
{
    old <- set_message_sink(file = new, append = append)
    on.exit(reset_message_sink(old))
    force(code)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Run Code 'With' Temporarily Modified Global State"

.skeleton_package_version = "1.0.2"

.skeleton_package_depends = ""

.skeleton_package_imports = "stats,graphics"


## Internal

.skeleton_version = 5


## EOF