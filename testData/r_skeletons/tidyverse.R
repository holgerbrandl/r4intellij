##
## Exported symobls in package `tidyverse`
##

## Exported package methods

tidyverse_update <- function (recursive = FALSE) 
{
    deps <- tidyverse_deps(recursive)
    behind <- dplyr::filter(deps, behind)
    if (nrow(behind) == 0) {
        message("All tidyverse packages up-to-date")
        return(invisible())
    }
    message("The following packages are out of date:")
    bullets(format(behind$package), " (", behind$local, " -> ", 
        behind$cran, ")")
    message("Update now?")
    do_it <- utils::menu(c("Yes", "No")) == 1
    if (!do_it) {
        return(invisible())
    }
    utils::install.packages(behind$package, quiet = TRUE, dependencies = if (recursive) 
        FALSE
    else NA)
    invisible()
}


tidyverse_conflicts <- function () 
{
    tidy_names <- paste0("package:", tidyverse_packages())
    tidy_envs <- intersect(tidy_names, search())
    names(tidy_envs) <- tidy_envs
    misc_envs <- setdiff(search(), tidy_envs)
    names(misc_envs) <- misc_envs
    tidy_funs <- invert(lapply(tidy_envs, ls_env))
    misc_funs <- invert(lapply(misc_envs, ls_env))
    conflicts <- intersect(names(tidy_funs), names(misc_funs))
    conflict_funs <- purrr::map2(tidy_funs[conflicts], misc_funs[conflicts], 
        c)
    conflict_funs <- purrr::map2(purrr::set_names(names(conflict_funs)), 
        conflict_funs, confirm_conflict)
    conflict_funs <- purrr::compact(conflict_funs)
    rule("Conflicts with tidy packages", startup = TRUE)
    fun <- format(paste0(names(conflict_funs), "(): "))
    pkg <- conflict_funs %>% purrr::map(~gsub("^package:", "", 
        .)) %>% purrr::map_chr(paste0, collapse = ", ")
    packageStartupMessage(paste0(fun, pkg, collapse = "\n"))
}


tidyverse_packages <- function (include_self = TRUE) 
{
    raw <- utils::packageDescription("tidyverse")$Imports
    imports <- strsplit(raw, ",")[[1]]
    parsed <- gsub("^\\s+|\\s+$", "", imports)
    if (include_self) {
        parsed <- c(parsed, "tidyverse")
    }
    parsed
}


tidyverse_deps <- function (recursive = FALSE) 
{
    pkgs <- utils::available.packages()
    deps <- tools::package_dependencies("tidyverse", pkgs, recursive = recursive)
    pkg_deps <- unique(sort(unlist(deps)))
    base_pkgs <- c("base", "compiler", "datasets", "graphics", 
        "grDevices", "grid", "methods", "parallel", "splines", 
        "stats", "stats4", "tools", "tcltk", "utils")
    pkg_deps <- setdiff(pkg_deps, base_pkgs)
    cran_version <- lapply(pkgs[pkg_deps, "Version"], package_version)
    local_version <- lapply(pkg_deps, utils::packageVersion)
    behind <- purrr::map2_lgl(cran_version, local_version, `>`)
    tibble::tibble(package = pkg_deps, cran = cran_version %>% 
        purrr::map_chr(as.character), local = local_version %>% 
        purrr::map_chr(as.character), behind = behind)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Easily Install and Load 'Tidyverse' Packages"

.skeleton_package_version = "1.1.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "broom,dplyr,forcats,ggplot2,haven,httr,hms,jsonlite,lubridate,magrittr,modelr,purrr,readr,readxl,stringr,tibble,rvest,tidyr,xml2"


## Internal

.skeleton_version = 5


## EOF