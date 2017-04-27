##
## Exported symobls in package `R6`
##

## Exported package methods

is.R6Class <- function (x) 
{
    inherits(x, "R6ClassGenerator")
}


is.R6 <- function (x) 
{
    inherits(x, "R6")
}


R6Class <- function (classname = NULL, public = list(), private = NULL, 
    active = NULL, inherit = NULL, lock_objects = TRUE, class = TRUE, 
    portable = TRUE, lock_class = FALSE, cloneable = TRUE, parent_env = parent.frame(), 
    lock) 
{
    if (!all_named(public) || !all_named(private) || !all_named(active)) 
        stop("All elements of public, private, and active must be named.")
    allnames <- c(names(public), names(private), names(active))
    if (any(duplicated(allnames))) 
        stop("All items in public, private, and active must have unique names.")
    if ("clone" %in% allnames) 
        stop("Cannot add a member with reserved name 'clone'.")
    if (any(c("self", "private", "super") %in% c(names(public), 
        names(private), names(active)))) 
        stop("Items cannot use reserved names 'self', 'private', and 'super'.")
    if ("initialize" %in% c(names(private), names(active))) 
        stop("'initialize' is not allowed in private or active.")
    if (length(get_nonfunctions(active)) != 0) 
        stop("All items in active must be functions.")
    if (!missing(lock)) {
        message(paste0("R6Class ", classname, ": 'lock' argument has been renamed to 'lock_objects' as of version 2.1.", 
            "This code will continue to work, but the 'lock' option will be removed in a later version of R6"))
        lock_objects <- lock
    }
    generator <- new.env(parent = capsule)
    generator$self <- generator
    generator_funs <- assign_func_envs(generator_funs, generator)
    list2env2(generator_funs, generator)
    generator$classname <- classname
    generator$active <- active
    generator$portable <- portable
    generator$parent_env <- parent_env
    generator$lock_objects <- lock_objects
    generator$class <- class
    generator$lock_class <- lock_class
    generator$public_fields <- get_nonfunctions(public)
    generator$private_fields <- get_nonfunctions(private)
    generator$public_methods <- get_functions(public)
    generator$private_methods <- get_functions(private)
    if (cloneable) 
        generator$public_methods$clone <- generator_funs$clone_method
    generator$inherit <- substitute(inherit)
    generator$debug_names <- character(0)
    attr(generator, "name") <- paste0(classname, "_generator")
    class(generator) <- "R6ClassGenerator"
    generator
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Classes with Reference Semantics"

.skeleton_package_version = "2.2.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF