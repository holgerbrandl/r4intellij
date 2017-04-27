##
## Exported symobls in package `hms`
##

## Exported package methods

hms <- function (seconds = NULL, minutes = NULL, hours = NULL, days = NULL) 
{
    args <- list(seconds = seconds, minutes = minutes, hours = hours, 
        days = days)
    check_args(args)
    arg_secs <- mapply(`*`, args, c(1, 60, 3600, 86400))
    secs <- Reduce(`+`, arg_secs[vapply(arg_secs, length, integer(1L)) > 
        0L])
    as.hms(as.difftime(secs, units = "secs"))
}


as.hms <- function (x, ...) 
UseMethod("as.hms", x)


is.hms <- function (x) 
inherits(x, "hms")




## Package Data

# none


## Package Info

.skeleton_package_title = "Pretty Time of Day"

.skeleton_package_version = "0.3"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods"


## Internal

.skeleton_version = 5


## EOF