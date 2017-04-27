##
## Exported symobls in package `doParallel`
##

## Exported package methods

stopImplicitCluster <- function () 
{
    if (exists(".revoDoParCluster", where = .options) && !is.null(.options[[".revoDoParCluster"]])) {
        stopCluster(.options[[".revoDoParCluster"]])
        remove(".revoDoParCluster", envir = .options)
    }
}


registerDoParallel <- function (cl, cores = NULL, ...) 
{
    opts <- list(...)
    optnames <- names(opts)
    if (is.null(optnames)) 
        optnames <- rep("", length(opts))
    unnamed <- !nzchar(optnames)
    if (any(unnamed)) {
        warning("ignoring doParallel package option(s) specified with unnamed argument")
        opts <- opts[!unnamed]
        optnames <- optnames[!unnamed]
    }
    recog <- optnames %in% c("nocompile")
    if (any(!recog)) {
        warning(sprintf("ignoring unrecognized doParallel package option(s): %s", 
            paste(optnames[!recog], collapse = ", ")), call. = FALSE)
        opts <- opts[recog]
        optnames <- optnames[recog]
    }
    old.optnames <- ls(.options, all.names = TRUE)
    rm(list = old.optnames, pos = .options)
    for (i in seq(along = opts)) {
        assign(optnames[i], opts[[i]], pos = .options)
    }
    if (missing(cl) || is.numeric(cl)) {
        if (.Platform$OS.type == "windows") {
            if (!missing(cl) && is.numeric(cl)) {
                cl <- makeCluster(cl)
            }
            else {
                if (!missing(cores) && is.numeric(cores)) {
                  cl <- makeCluster(cores)
                }
                else {
                  cl <- makeCluster(3)
                }
            }
            assign(".revoDoParCluster", cl, pos = .options)
            reg.finalizer(.options, function(e) {
                stopImplicitCluster()
            }, onexit = TRUE)
            setDoPar(doParallelSNOW, cl, snowinfo)
        }
        else {
            if (!missing(cl) && is.numeric(cl)) {
                cores <- cl
            }
            setDoPar(doParallelMC, cores, mcinfo)
        }
    }
    else {
        setDoPar(doParallelSNOW, cl, snowinfo)
    }
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Foreach Parallel Adaptor for the 'parallel' Package"

.skeleton_package_version = "1.0.10"

.skeleton_package_depends = "foreach,iterators,parallel,utils"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF