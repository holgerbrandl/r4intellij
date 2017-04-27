##
## Exported symobls in package `devtools`
##

## Exported package methods

use_news_md <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_template("NEWS.md", data = pkg, open = TRUE, pkg = pkg)
}


on_path <- function (...) 
{
    commands <- c(...)
    stopifnot(is.character(commands))
    unname(Sys.which(commands) != "")
}


github_pull <- function (pull) 
structure(pull, class = "github_pull")


eval_clean <- function (expr, quiet = TRUE) 
{
    stopifnot(is.language(expr))
    tmp <- tempfile()
    on.exit(unlink(tmp))
    text <- deparse(expr)
    writeLines(text, tmp)
    suppressMessages(clean_source(tmp, quiet = quiet))
    invisible(TRUE)
}


check_cran <- function (pkgs, libpath = file.path(tempdir(), "R-lib"), srcpath = libpath, 
    bioconductor = FALSE, type = getOption("pkgType"), threads = getOption("Ncpus", 
        1), check_dir = tempfile("check_cran"), env_vars = NULL) 
{
    stopifnot(is.character(pkgs))
    if (length(pkgs) == 0) 
        return()
    rule("Checking ", length(pkgs), " CRAN packages", pad = "=")
    if (!file.exists(check_dir)) 
        dir.create(check_dir)
    message("Results saved in ", check_dir)
    old <- options(warn = 1)
    on.exit(options(old), add = TRUE)
    if (!file.exists(libpath)) 
        dir.create(libpath)
    libpath <- normalizePath(libpath)
    libpaths_orig <- withr::with_libpaths(libpath, {
        rule("Installing dependencies")
        repos <- c(CRAN = cran_mirror())
        if (bioconductor) {
            check_suggested("BiocInstaller")
            repos <- c(repos, BiocInstaller::biocinstallRepos())
        }
        available_src <- available_packages(repos, "source")
        message("Determining available packages")
        deps <- package_deps(pkgs, repos = repos, type = type, 
            dependencies = TRUE)
        update(deps, Ncpus = threads, quiet = TRUE)
        message("Downloading source packages for checking")
        urls <- lapply(pkgs, package_url, repos = repos, available = available_src)
        ok <- vapply(urls, function(x) !is.na(x$name), logical(1))
        if (any(!ok)) {
            message("Couldn't find source for: ", paste(pkgs[!ok], 
                collapse = ", "))
            urls <- urls[ok]
            pkgs <- pkgs[ok]
        }
        local_urls <- file.path(srcpath, vapply(urls, `[[`, "name", 
            FUN.VALUE = character(1)))
        remote_urls <- vapply(urls, `[[`, "url", FUN.VALUE = character(1))
        needs_download <- !vapply(local_urls, is_source_pkg, 
            logical(1))
        if (any(needs_download)) {
            message("Downloading ", sum(needs_download), " packages")
            Map(utils::download.file, remote_urls[needs_download], 
                local_urls[needs_download], quiet = TRUE)
        }
        rule("Checking packages")
        check_start <- Sys.time()
        pkg_names <- format(pkgs)
        check_pkg <- function(i) {
            start_time <- Sys.time()
            res <- check_built(local_urls[i], args = "--no-multiarch --no-manual --no-codoc", 
                env_vars = env_vars, check_dir = check_dir, quiet = TRUE)
            end_time <- Sys.time()
            message("Checked ", pkg_names[i], ": ", summarise_check_results(res, 
                colour = TRUE))
            status_update(i, length(pkgs), check_start)
            elapsed_time <- as.numeric(end_time - start_time, 
                units = "secs")
            writeLines(sprintf("%d  %s  %.1f", i, pkgs[i], elapsed_time), 
                file.path(check_dir, paste0(pkgs[i], ".Rcheck"), 
                  "check-time.txt"))
            NULL
        }
        if (length(pkgs) == 0) 
            return()
        if (identical(as.integer(threads), 1L)) {
            lapply(seq_along(pkgs), check_pkg)
        }
        else {
            parallel::mclapply(seq_along(pkgs), check_pkg, mc.preschedule = FALSE, 
                mc.cores = threads)
        }
        invisible(check_dir)
    })
}


use_github <- function (auth_token = github_pat(), private = FALSE, pkg = ".", 
    host = "https://api.github.com", protocol = c("ssh", "https"), 
    credentials = NULL) 
{
    if (is.null(auth_token)) {
        stop("GITHUB_PAT required to create new repo")
    }
    protocol <- match.arg(protocol)
    pkg <- as.package(pkg)
    use_git(pkg = pkg)
    if (uses_github(pkg$path)) {
        message("* GitHub is already initialized")
        return(invisible())
    }
    message("* Checking title and description")
    message("  Title: ", pkg$title)
    message("  Description: ", pkg$description)
    if (yesno("Are title and description ok?")) {
        return(invisible())
    }
    message("* Creating GitHub repository")
    create <- github_POST("user/repos", pat = auth_token, body = list(name = jsonlite::unbox(pkg$package), 
        description = jsonlite::unbox(gsub("\n", " ", pkg$title)), 
        private = jsonlite::unbox(private)), host = host)
    message("* Adding GitHub remote")
    r <- git2r::repository(pkg$path)
    origin_url <- switch(protocol, https = create$clone_url, 
        ssh = create$ssh_url)
    git2r::remote_add(r, "origin", origin_url)
    message("* Adding GitHub links to DESCRIPTION")
    use_github_links(pkg$path, auth_token = auth_token, host = host)
    if (git_uncommitted(pkg$path)) {
        git2r::add(r, "DESCRIPTION")
        git2r::commit(r, "Add GitHub links to DESCRIPTION")
    }
    message("* Pushing to GitHub and setting remote tracking branch")
    if (protocol == "ssh") {
        git2r::push(r, "origin", "refs/heads/master", credentials = credentials)
    }
    else {
        cred <- git2r::cred_user_pass("EMAIL", auth_token)
        git2r::push(r, "origin", "refs/heads/master", credentials = cred)
    }
    git2r::branch_set_upstream(git2r::head(r), "origin/master")
    message("* View repo at ", create$html_url)
    invisible(NULL)
}


check_man <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    document(pkg)
    old <- options(warn = -1)
    on.exit(options(old))
    message("Checking documentation...")
    ok <- man_message(("tools" %:::% ".check_package_parseRd")(dir = pkg$path)) && 
        man_message(("tools" %:::% ".check_Rd_metadata")(dir = pkg$path)) && 
        man_message(("tools" %:::% ".check_Rd_xrefs")(dir = pkg$path)) && 
        man_message(("tools" %:::% ".check_Rd_contents")(dir = pkg$path)) && 
        man_message(tools::checkDocFiles(dir = pkg$path))
    if (ok) {
        message("No issues detected")
    }
    invisible()
}


find_rtools <- function (cache = TRUE, debug = FALSE) 
{
    if (.Platform$OS.type != "windows") 
        return(TRUE)
    if (!cache) {
        set_rtools_path(NULL)
    }
    if (!is.null(get_rtools_path())) 
        return(TRUE)
    from_path <- scan_path_for_rtools(debug)
    if (is_compatible(from_path)) {
        set_rtools_path(from_path)
        return(TRUE)
    }
    if (!is.null(from_path)) {
        if (is.null(from_path$version)) {
            if (debug) 
                "gcc and ls on path, assuming set up is correct\n"
            return(TRUE)
        }
        else {
            message("WARNING: Rtools ", from_path$version, " found on the path", 
                " at ", from_path$path, " is not compatible with R ", 
                getRversion(), ".\n\n", "Please download and install ", 
                rtools_needed(), " from ", rtools_url, ", remove the incompatible version from your PATH.")
            return(invisible(FALSE))
        }
    }
    registry_candidates <- scan_registry_for_rtools(debug)
    if (length(registry_candidates) == 0) {
        message("WARNING: Rtools is required to build R packages, but is not ", 
            "currently installed.\n\n", "Please download and install ", 
            rtools_needed(), " from ", rtools_url, ".")
        return(invisible(FALSE))
    }
    from_registry <- Find(is_compatible, registry_candidates, 
        right = TRUE)
    if (is.null(from_registry)) {
        versions <- vapply(registry_candidates, function(x) x$version, 
            character(1))
        message("WARNING: Rtools is required to build R packages, but no version ", 
            "of Rtools compatible with R ", getRversion(), " was found. ", 
            "(Only the following incompatible version(s) of Rtools were found:", 
            paste(versions, collapse = ","), ")\n\n", "Please download and install ", 
            rtools_needed(), " from ", rtools_url, ".")
        return(invisible(FALSE))
    }
    installed_ver <- installed_version(from_registry$path, debug = debug)
    if (is.null(installed_ver)) {
        message("WARNING: Rtools is required to build R packages, but the ", 
            "version of Rtools previously installed in ", from_registry$path, 
            " has been deleted.\n\n", "Please download and install ", 
            rtools_needed(), " from ", rtools_url, ".")
        return(invisible(FALSE))
    }
    if (installed_ver != from_registry$version) {
        message("WARNING: Rtools is required to build R packages, but no version ", 
            "of Rtools compatible with R ", getRversion(), " was found. ", 
            "Rtools ", from_registry$version, " was previously installed in ", 
            from_registry$path, " but now that directory contains Rtools ", 
            installed_ver, ".\n\n", "Please download and install ", 
            rtools_needed(), " from ", rtools_url, ".")
        return(invisible(FALSE))
    }
    set_rtools_path(from_registry)
    TRUE
}


with_lib <- function (new, code) 
{
    .Deprecated(new = "withr::with_libpaths", package = "devtools")
    withr::with_libpaths(new = new, code = code, action = "prefix")
}


release_checks <- function (pkg = ".", built_path = NULL) 
{
    pkg <- as.package(pkg)
    message("Running additional devtools checks for ", pkg$package)
    check_version(pkg)
    check_dev_versions(pkg)
    check_vignette_titles(pkg)
    check_news_md(pkg)
    check_remotes(pkg)
}


check_failures <- function (path, error = TRUE, warning = TRUE, note = TRUE) 
{
    check_dir <- file.path(path, "00check.log")
    results <- parse_check_results(check_dir)
    c(if (error) results$errors, if (warning) results$warnings, 
        if (note) results$notes)
}


system_output <- function (cmd, args = character(), env_vars = character(), path = ".", 
    quiet = FALSE, ...) 
{
    full <- paste(shQuote(cmd), " ", paste(args, collapse = " "), 
        sep = "")
    if (!quiet) {
        message(wrap_command(full), "\n")
    }
    result <- withCallingHandlers(withr::with_dir(path, withr::with_envvar(env_vars, 
        system(full, intern = TRUE, ignore.stderr = quiet, ...))), 
        warning = function(w) stop(w))
    result
}


install_git <- function (url, subdir = NULL, branch = NULL, credentials = NULL, 
    args = character(0), ...) 
{
    if (!missing(args)) 
        warning("`args` is deprecated", call. = FALSE)
    remotes <- lapply(url, git_remote, subdir = subdir, branch = branch, 
        credentials = credentials)
    install_remotes(remotes, ...)
}


github_release <- function () 
structure(NA_integer_, class = "github_release")


imports_env <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    if (!is_loaded(pkg)) {
        stop("Namespace environment must be created before accessing imports environment.")
    }
    env <- parent.env(ns_env(pkg))
    if (attr(env, "name") != imports_env_name(pkg)) {
        stop("Imports environment does not have attribute 'name' with value ", 
            imports_env_name(pkg), ". This probably means that the namespace environment was not created correctly.")
    }
    env
}


use_code_of_conduct <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_template("CONDUCT.md", ignore = TRUE, pkg = pkg)
    message("* Don't forget to describe the code of conduct in your README.md:")
    message("Please note that this project is released with a ", 
        "[Contributor Code of Conduct](CONDUCT.md). ", "By participating in this ", 
        "project you agree to abide by its terms.")
}


dr_github <- function (path = ".") 
{
    if (!uses_git(path)) {
        return(doctor("github", "Path is not a git repository"))
    }
    if (!uses_github(path)) {
        return(doctor("github", "Path is not a GitHub repository"))
    }
    msg <- character()
    r <- git2r::repository(path, discover = TRUE)
    config <- git2r::config(r)
    config_names <- names(modifyList(config$global, config$local))
    if (!uses_github(path)) 
        msg[["github"]] <- " * cannot detect that this repo is connected to GitHub"
    if (!("user.name" %in% config_names)) 
        msg[["name"]] <- "* user.name config option not set"
    if (!("user.email" %in% config_names)) 
        msg[["user"]] <- "* user.email config option not set"
    if (!file.exists("~/.ssh/id_rsa")) 
        msg[["ssh"]] <- "* SSH private key not found"
    if (identical(Sys.getenv("GITHUB_PAT"), "")) 
        msg[["PAT"]] <- paste("* GITHUB_PAT environment variable not set", 
            "(this is not necessary unless you want to install private repos", 
            "or connect local repos to GitHub)")
    desc_path <- file.path(path, "DESCRIPTION")
    desc <- read_dcf(desc_path)
    field_empty <- function(d, f) is.null(d[[f]]) || identical(d[[f]], 
        "")
    field_no_re <- function(d, f, re) !grepl(re, d[[f]])
    re <- "https://github.com/(.*?)/(.*)"
    if (field_empty(desc, "URL")) {
        msg[["URL_empty"]] <- "* empty URL field in DESCRIPTION"
    }
    else if (field_no_re(desc, "URL", re)) {
        msg[["URL"]] <- "* no GitHub repo link in URL field in DESCRIPTION"
    }
    re <- paste0(re, "/issues")
    if (field_empty(desc, "BugReports")) {
        msg[["BugReports_empty"]] <- "* empty BugReports field in DESCRIPTION"
    }
    else if (field_no_re(desc, "BugReports", re)) {
        msg[["BugReports"]] <- "* no GitHub Issues link in URL field in DESCRIPTION"
    }
    doctor("github", msg)
}


get_path <- withr::get_path # re-exported from withr package

install_bitbucket <- function (repo, username, ref = "master", subdir = NULL, auth_user = NULL, 
    password = NULL, ...) 
{
    remotes <- lapply(repo, bitbucket_remote, username = username, 
        ref = ref, subdir = subdir, auth_user = auth_user, password = password)
    install_remotes(remotes, ...)
}


dev_mode <- function (on = NULL, path = getOption("devtools.path")) 
{
    lib_paths <- .libPaths()
    path <- normalizePath(path, winslash = "/", mustWork = FALSE)
    if (is.null(on)) {
        on <- !(path %in% lib_paths)
    }
    if (on) {
        if (!file.exists(path)) {
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
        }
        if (!file.exists(path)) {
            stop("Failed to create ", path, call. = FALSE)
        }
        if (!is_library(path)) {
            warning(path, " does not appear to be a library. ", 
                "Are sure you specified the correct directory?", 
                call. = FALSE)
        }
        message("Dev mode: ON")
        options(dev_path = path)
        if (is.null(.prompt)) 
            .prompt <<- getOption("prompt")
        options(prompt = paste("d> "))
        .libPaths(c(path, lib_paths))
    }
    else {
        message("Dev mode: OFF")
        options(dev_path = NULL)
        if (!is.null(.prompt)) 
            options(prompt = .prompt)
        .prompt <<- NULL
        .libPaths(setdiff(lib_paths, path))
    }
}


use_appveyor <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_template("appveyor.yml", ignore = TRUE, pkg = pkg)
    gh <- github_info(pkg$path)
    message("Next: \n", " * Turn on AppVeyor for this repo at https://ci.appveyor.com/projects\n", 
        " * Add an AppVeyor shield to your README.md:\n", "[![AppVeyor Build Status]", 
        "(https://ci.appveyor.com/api/projects/status/github/", 
        gh$username, "/", gh$repo, "?branch=master&svg=true)]", 
        "(https://ci.appveyor.com/project/", gh$username, "/", 
        gh$repo, ")")
    invisible(TRUE)
}


use_git_hook <- function (hook, script, pkg = ".") 
{
    pkg <- as.package(pkg)
    git_dir <- file.path(pkg$path, ".git")
    if (!file.exists(git_dir)) {
        stop("This project doesn't use git", call. = FALSE)
    }
    hook_dir <- file.path(git_dir, "hooks")
    if (!file.exists(hook_dir)) {
        dir.create(hook_dir)
    }
    hook_path <- file.path(hook_dir, hook)
    writeLines(script, hook_path)
    Sys.chmod(hook_path, "0744")
}


find_topic <- function (topic) 
{
    if (is.null(topic) || topic == "") 
        return(NULL)
    pieces <- strsplit(topic, "::")[[1]]
    if (length(pieces) == 1) {
        pkgs <- dev_packages()
    }
    else {
        pkgs <- pieces[1]
        topic <- pieces[2]
    }
    for (pkg in pkgs) {
        path <- getNamespaceInfo(pkg, "path")
        rd <- find_pkg_topic(path, topic)
        if (!is.null(rd)) 
            return(stats::setNames(file.path(path, "man", rd), 
                path))
    }
    NULL
}


use_git <- function (message = "Initial commit", pkg = ".") 
{
    pkg <- as.package(pkg)
    if (uses_git(pkg$path)) {
        message("* Git is already initialized")
        return(invisible())
    }
    message("* Initialising repo")
    r <- git2r::init(pkg$path)
    use_git_ignore(c(".Rproj.user", ".Rhistory", ".RData"), pkg = pkg)
    message("* Adding files and committing")
    paths <- unlist(git2r::status(r))
    git2r::add(r, paths)
    git2r::commit(r, message)
    invisible()
}


package_file <- function (..., path = ".") 
{
    if (!is.character(path) || length(path) != 1) {
        stop("`path` must be a string.", call. = FALSE)
    }
    path <- strip_slashes(normalizePath(path, mustWork = FALSE))
    if (!file.exists(path)) {
        stop("Can't find '", path, "'.", call. = FALSE)
    }
    if (!file.info(path)$isdir) {
        stop("'", path, "' is not a directory.", call. = FALSE)
    }
    while (!has_description(path)) {
        path <- dirname(path)
        if (is_root(path)) {
            stop("Could not find package root.", call. = FALSE)
        }
    }
    file.path(path, ...)
}


use_build_ignore <- function (files, escape = TRUE, pkg = ".") 
{
    pkg <- as.package(pkg)
    if (escape) {
        files <- paste0("^", gsub("\\.", "\\\\.", files), "$")
    }
    path <- file.path(pkg$path, ".Rbuildignore")
    union_write(path, files)
    invisible(TRUE)
}


install_version <- function (package, version = NULL, repos = getOption("repos"), 
    type = getOption("pkgType"), ...) 
{
    contriburl <- contrib.url(repos, type)
    available <- available.packages(contriburl)
    if (package %in% row.names(available)) {
        current.version <- available[package, "Version"]
        if (is.null(version) || version == current.version) {
            return(install.packages(package, repos = repos, contriburl = contriburl, 
                type = type, ...))
        }
    }
    info <- package_find_repo(package, repos)
    if (is.null(version)) {
        package.path <- info$path[NROW(info)]
    }
    else {
        package.path <- paste(package, "/", package, "_", version, 
            ".tar.gz", sep = "")
        if (!(package.path %in% info$path)) {
            stop(sprintf("version '%s' is invalid for package '%s'", 
                version, package))
        }
    }
    url <- paste(info$repo[1L], "/src/contrib/Archive/", package.path, 
        sep = "")
    install_url(url, ...)
}


package_deps <- function (pkg, dependencies = NA, repos = getOption("repos"), 
    type = getOption("pkgType")) 
{
    if (identical(type, "both")) {
        type <- "binary"
    }
    if (length(repos) == 0) 
        repos <- character()
    repos[repos == "@CRAN@"] <- cran_mirror()
    cran <- available_packages(repos, type)
    if (missing(pkg)) {
        pkg <- as.package(".")$package
    }
    deps <- sort(find_deps(pkg, cran, top_dep = dependencies))
    inst <- installed.packages()
    base <- unname(inst[inst[, "Priority"] %in% c("base", "recommended"), 
        "Package"])
    deps <- setdiff(deps, base)
    remote <- structure(lapply(deps, package2remote, repos = repos, 
        type = type), class = "remotes")
    inst_ver <- vapply(deps, local_sha, character(1))
    cran_ver <- vapply(remote, remote_sha, character(1))
    cran_remote <- vapply(remote, inherits, logical(1), "cran_remote")
    diff <- compare_versions(inst_ver, cran_ver, cran_remote)
    res <- structure(data.frame(package = deps, installed = inst_ver, 
        available = cran_ver, diff = diff, stringsAsFactors = FALSE), 
        class = c("package_deps", "data.frame"))
    res$remote <- remote
    res
}


inst <- function (name) 
{
    paths <- file.path(.libPaths(), name)
    paths <- paths[dir.exists(paths)]
    if (length(paths) > 0) {
        return(normalizePath(paths[1]))
    }
    else {
        return(NULL)
    }
}


system_check <- function (cmd, args = character(), env_vars = character(), path = ".", 
    quiet = FALSE, throw = TRUE, ...) 
{
    full <- paste(shQuote(cmd), " ", paste(args, collapse = " "), 
        sep = "")
    if (!quiet) {
        message(wrap_command(full))
        message()
    }
    result <- suppressWarnings(withr::with_dir(path, withr::with_envvar(env_vars, 
        system(full, intern = quiet, ignore.stderr = quiet, ...))))
    if (quiet) {
        status <- attr(result, "status") %||% 0L
    }
    else {
        status <- result
    }
    ok <- identical(as.character(status), "0")
    if (throw && !ok) {
        stop("Command failed (", status, ")", call. = FALSE)
    }
    invisible(status)
}


bash <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    withr::with_dir(pkg$path, system("bash"))
}


dev_meta <- function (name) 
{
    ns <- .getNamespace(name)
    if (is.null(ns)) {
        stop("Namespace not found for ", name, ". Is it loaded?")
    }
    if (is.null(ns$.__DEVTOOLS__)) {
        return(NULL)
    }
    ns$.__DEVTOOLS__
}


submit_cran <- function (pkg = ".", args = NULL) 
{
    built_path <- build_cran(pkg, args = args)
    upload_cran(pkg, built_path)
}


has_tests <- function () 
{
    system.file("tests", package = "devtools") != ""
}


install_deps <- function (pkg = ".", dependencies = NA, threads = getOption("Ncpus", 
    1), repos = getOption("repos"), type = getOption("pkgType"), 
    ..., upgrade = TRUE, quiet = FALSE, force_deps = FALSE) 
{
    pkg <- dev_package_deps(pkg, repos = repos, dependencies = dependencies, 
        type = type)
    update(pkg, ..., Ncpus = threads, quiet = quiet, upgrade = upgrade)
    invisible()
}


wd <- function (pkg = ".", path = "") 
{
    pkg <- as.package(pkg)
    path <- file.path(pkg$path, path)
    if (!file.exists(path)) {
        stop(path, " does not exist", call. = FALSE)
    }
    message("Changing working directory to ", path)
    setwd(path)
}


revdep_maintainers <- function (pkg = ".") 
{
    if (missing(pkg)) 
        pkg <- as.package(".")$package
    maintainers <- unique(packages()[revdep(pkg), "Maintainer"])
    class(maintainers) <- "maintainers"
    maintainers
}


with_makevars <- withr::with_makevars # re-exported from withr package

parse_ns_file <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    parseNamespaceFile(basename(pkg$path), dirname(pkg$path), 
        mustExist = FALSE)
}


is.package <- function (x) 
inherits(x, "package")


use_rstudio <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_template("template.Rproj", paste0(pkg$package, ".Rproj"), 
        pkg = pkg)
    use_git_ignore(c(".Rproj.user", ".Rhistory", ".RData"), pkg = pkg)
    use_build_ignore(c("^.*\\.Rproj$", "^\\.Rproj\\.user$"), 
        escape = FALSE, pkg = pkg)
    invisible(TRUE)
}


check <- function (pkg = ".", document = TRUE, build_args = NULL, ..., 
    manual = FALSE, cran = TRUE, check_version = FALSE, force_suggests = FALSE, 
    run_dont_test = FALSE, args = NULL, env_vars = NULL, quiet = FALSE, 
    check_dir = tempdir(), cleanup = TRUE) 
{
    pkg <- as.package(pkg)
    if (!missing(cleanup)) {
        warning("`cleanup` is deprecated", call. = FALSE)
    }
    if (document) {
        document(pkg)
    }
    if (!quiet) {
        show_env_vars(compiler_flags(FALSE))
        rule("Building ", pkg$package)
    }
    withr::with_envvar(compiler_flags(FALSE), action = "prefix", 
        {
            built_path <- build(pkg, tempdir(), quiet = quiet, 
                args = build_args, manual = manual, ...)
            on.exit(unlink(built_path), add = TRUE)
        })
    check_built(built_path, cran = cran, check_version = check_version, 
        force_suggests = force_suggests, run_dont_test = run_dont_test, 
        manual = manual, args = args, env_vars = env_vars, quiet = quiet, 
        check_dir = check_dir)
}


missing_s3 <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    check_suggested("roxygen2")
    loaded <- load_all(pkg)
    objs <- ls(envir = loaded$env)
    is_s3 <- function(x) roxygen2::is_s3_method(x, env = loaded$env)
    s3_objs <- Filter(is_s3, objs)
    ns <- parse_ns_file(pkg)
    exports <- paste(ns$S3methods[, 1], ns$S3methods[, 2], sep = ".")
    setdiff(s3_objs, exports)
}


use_readme_rmd <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_template("README.Rmd", ignore = TRUE, open = TRUE, pkg = pkg)
    use_build_ignore("^README-.*\\.png$", escape = FALSE, pkg = pkg)
    if (uses_git(pkg$path) && !file.exists(pkg$path, ".git", 
        "hooks", "pre-commit")) {
        message("* Adding pre-commit hook")
        use_git_hook("pre-commit", render_template("readme-rmd-pre-commit.sh"), 
            pkg = pkg)
    }
    invisible(TRUE)
}


load_dll <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    env <- ns_env(pkg)
    nsInfo <- parse_ns_file(pkg)
    dlls <- list()
    dynLibs <- nsInfo$dynlibs
    for (i in seq_along(dynLibs)) {
        lib <- dynLibs[i]
        dlls[[lib]] <- library.dynam2(pkg, lib)
        assignNativeRoutines(dlls[[lib]], lib, env, nsInfo$nativeRoutines[[lib]])
        if (!is.null(names(nsInfo$dynlibs)) && nzchar(names(nsInfo$dynlibs)[i])) 
            env[[names(nsInfo$dynlibs)[i]]] <- dlls[[lib]]
        setNamespaceInfo(env, "DLLs", dlls)
    }
    addNamespaceDynLibs(env, nsInfo$dynlibs)
    invisible(dlls)
}


reload <- function (pkg = ".", quiet = FALSE) 
{
    pkg <- as.package(pkg)
    if (is_attached(pkg)) {
        if (!quiet) 
            message("Reloading installed ", pkg$package)
        unload(pkg)
        require(pkg$package, character.only = TRUE, quietly = TRUE)
    }
}


use_data <- function (..., pkg = ".", internal = FALSE, overwrite = FALSE, 
    compress = "bzip2") 
{
    pkg <- as.package(pkg)
    objs <- get_objs_from_dots(dots(...))
    if (internal) {
        dir_name <- file.path(pkg$path, "R")
        paths <- file.path(dir_name, "sysdata.rda")
        objs <- list(objs)
    }
    else {
        dir_name <- file.path(pkg$path, "data")
        paths <- file.path(dir_name, paste0(objs, ".rda"))
    }
    check_data_paths(paths, overwrite)
    message("Saving ", paste(unlist(objs), collapse = ", "), 
        " as ", paste(basename(paths), collapse = ", "), " to ", 
        dir_name)
    envir <- parent.frame()
    mapply(save, list = objs, file = paths, MoreArgs = list(envir = envir, 
        compress = compress))
    invisible()
}


use_coverage <- function (pkg = ".", type = c("codecov", "coveralls")) 
{
    pkg <- as.package(pkg)
    check_suggested("covr")
    path <- file.path(pkg$path, ".travis.yml")
    if (!file.exists(path)) {
        use_travis()
    }
    message("* Adding covr to Suggests")
    add_desc_package(pkg, "Suggests", "covr")
    gh <- github_info(pkg$path)
    type <- match.arg(type)
    message("Next:")
    switch(type, codecov = {
        use_template("codecov.yml", "codecov.yml", ignore = TRUE, 
            pkg = pkg)
        message("* Add to `README.md`: \n", "[![Coverage Status]", 
            "(https://img.shields.io/codecov/c/github/", gh$fullname, 
            "/master.svg)]", "(https://codecov.io/github/", gh$fullname, 
            "?branch=master)")
        message("* Add to `.travis.yml`:\n", "after_success:\n", 
            "  - Rscript -e 'covr::codecov()'")
    }, coveralls = {
        message("* Turn on coveralls for this repo at https://coveralls.io/repos/new")
        message("* Add to `README.md`: \n", "[![Coverage Status]", 
            "(https://img.shields.io/coveralls/", gh$fullname, 
            ".svg)]", "(https://coveralls.io/r/", gh$fullname, 
            "?branch=master)")
        message("* Add to `.travis.yml`:\n", "after_success:\n", 
            "  - Rscript -e 'covr::coveralls()'")
    })
    invisible(TRUE)
}


use_package <- function (package, type = "Imports", pkg = ".") 
{
    stopifnot(is.character(package), length(package) == 1)
    stopifnot(is.character(type), length(type) == 1)
    if (!is_installed(package)) {
        stop(package, " must be installed before you can take a dependency on it", 
            call. = FALSE)
    }
    types <- c("Imports", "Depends", "Suggests", "Enhances", 
        "LinkingTo")
    names(types) <- tolower(types)
    type <- types[[match.arg(tolower(type), names(types))]]
    message("* Adding ", package, " to ", type)
    add_desc_package(pkg, type, package)
    msg <- switch(type, Imports = paste0("Refer to functions with ", 
        package, "::fun()"), Depends = paste0("Are you sure you want Depends? Imports is almost always", 
        " the better choice."), Suggests = paste0("Use requireNamespace(\"", 
        package, "\", quietly = TRUE)", " to test if package is installed,\n", 
        "then use ", package, "::fun() to refer to functions."), 
        Enhances = "", LinkingTo = show_includes(package))
    message("Next: ")
    message(msg)
    invisible()
}


clean_vignettes <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    vigns <- tools::pkgVignettes(dir = pkg$path)
    if (basename(vigns$dir) != "vignettes") 
        return()
    message("Cleaning built vignettes from ", pkg$package)
    doc_path <- file.path(pkg$path, "inst", "doc")
    vig_candidates <- dir(doc_path, full.names = TRUE)
    vig_rm <- vig_candidates[file_name(vig_candidates) %in% file_name(vigns$docs)]
    extra_candidates <- file.path(doc_path, basename(find_vignette_extras(pkg)))
    extra_rm <- extra_candidates[file.exists(extra_candidates)]
    to_remove <- c(vig_rm, extra_rm)
    if (length(to_remove) > 0) {
        message("Removing ", paste(basename(to_remove), collapse = ", "))
        file.remove(to_remove)
    }
    invisible(TRUE)
}


setup_rtools <- function (cache = TRUE, debug = FALSE) 
{
    if (.Platform$OS.type != "windows") 
        return(TRUE)
    if (!cache) {
        set_rtools_path(NULL)
    }
    if (!is.null(get_rtools_path())) 
        return(TRUE)
    from_path <- scan_path_for_rtools(debug)
    if (is_compatible(from_path)) {
        set_rtools_path(from_path)
        return(TRUE)
    }
    if (!is.null(from_path)) {
        if (is.null(from_path$version)) {
            if (debug) 
                "gcc and ls on path, assuming set up is correct\n"
            return(TRUE)
        }
        else {
            message("WARNING: Rtools ", from_path$version, " found on the path", 
                " at ", from_path$path, " is not compatible with R ", 
                getRversion(), ".\n\n", "Please download and install ", 
                rtools_needed(), " from ", rtools_url, ", remove the incompatible version from your PATH.")
            return(invisible(FALSE))
        }
    }
    registry_candidates <- scan_registry_for_rtools(debug)
    if (length(registry_candidates) == 0) {
        message("WARNING: Rtools is required to build R packages, but is not ", 
            "currently installed.\n\n", "Please download and install ", 
            rtools_needed(), " from ", rtools_url, ".")
        return(invisible(FALSE))
    }
    from_registry <- Find(is_compatible, registry_candidates, 
        right = TRUE)
    if (is.null(from_registry)) {
        versions <- vapply(registry_candidates, function(x) x$version, 
            character(1))
        message("WARNING: Rtools is required to build R packages, but no version ", 
            "of Rtools compatible with R ", getRversion(), " was found. ", 
            "(Only the following incompatible version(s) of Rtools were found:", 
            paste(versions, collapse = ","), ")\n\n", "Please download and install ", 
            rtools_needed(), " from ", rtools_url, ".")
        return(invisible(FALSE))
    }
    installed_ver <- installed_version(from_registry$path, debug = debug)
    if (is.null(installed_ver)) {
        message("WARNING: Rtools is required to build R packages, but the ", 
            "version of Rtools previously installed in ", from_registry$path, 
            " has been deleted.\n\n", "Please download and install ", 
            rtools_needed(), " from ", rtools_url, ".")
        return(invisible(FALSE))
    }
    if (installed_ver != from_registry$version) {
        message("WARNING: Rtools is required to build R packages, but no version ", 
            "of Rtools compatible with R ", getRversion(), " was found. ", 
            "Rtools ", from_registry$version, " was previously installed in ", 
            from_registry$path, " but now that directory contains Rtools ", 
            installed_ver, ".\n\n", "Please download and install ", 
            rtools_needed(), " from ", rtools_url, ".")
        return(invisible(FALSE))
    }
    set_rtools_path(from_registry)
    TRUE
}


unload <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    if (pkg$package == "compiler") {
        oldEnable <- compiler::enableJIT(0)
        if (oldEnable != 0) {
            warning("JIT automatically disabled when unloading the compiler.")
        }
    }
    if (pkg$package == "devtools") {
        eapply(ns_env(pkg), force, all.names = TRUE)
    }
    if (!is.null(dev_meta(pkg$package))) {
        remove_s4_classes(pkg)
    }
    if (pkg$package %in% loadedNamespaces()) {
        try(unloadNamespace(pkg$package), silent = TRUE)
    }
    else {
        stop("Package ", pkg$package, " not found in loaded packages or namespaces")
    }
    if (!is.null(.getNamespace(pkg$package))) {
        message("unloadNamespace(\"", pkg$package, "\") not successful, probably because another loaded package depends on it.", 
            "Forcing unload. If you encounter problems, please restart R.")
        unregister_namespace(pkg$package)
    }
    clear_cache()
    unload_dll(pkg)
}


source_url <- function (url, ..., sha1 = NULL) 
{
    stopifnot(is.character(url), length(url) == 1)
    temp_file <- tempfile()
    on.exit(unlink(temp_file))
    request <- httr::GET(url)
    httr::stop_for_status(request)
    writeBin(httr::content(request, type = "raw"), temp_file)
    file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
    if (is.null(sha1)) {
        message("SHA-1 hash of file is ", file_sha1)
    }
    else {
        if (nchar(sha1) < 6) {
            stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
        }
        file_sha1 <- substr(file_sha1, 1, nchar(sha1))
        if (!identical(file_sha1, sha1)) {
            stop("SHA-1 hash of downloaded file (", file_sha1, 
                ")\n  does not match expected value (", sha1, 
                ")", call. = FALSE)
        }
    }
    source(temp_file, ...)
}


check_built <- function (path = NULL, cran = TRUE, check_version = FALSE, force_suggests = FALSE, 
    run_dont_test = FALSE, manual = FALSE, args = NULL, env_vars = NULL, 
    check_dir = tempdir(), quiet = FALSE) 
{
    pkgname <- gsub("_.*?$", "", basename(path))
    args <- c("--timings", args)
    if (cran) {
        args <- c("--as-cran", args)
    }
    if (run_dont_test) {
        args <- c("--run-donttest", args)
    }
    if (manual && !has_latex(verbose = TRUE)) {
        manual <- FALSE
    }
    if (!manual) {
        args <- c(args, "--no-manual")
    }
    env_vars <- check_env_vars(cran, check_version, force_suggests, 
        env_vars)
    if (!quiet) {
        show_env_vars(env_vars)
        rule("Checking ", pkgname)
    }
    R(c(paste("CMD check", shQuote(path)), args), path = check_dir, 
        env_vars = env_vars, quiet = quiet, throw = FALSE)
    package_path <- file.path(normalizePath(check_dir), paste(pkgname, 
        ".Rcheck", sep = ""))
    if (!file.exists(package_path)) {
        stop("Check failed: '", package_path, "' doesn't exist", 
            call. = FALSE)
    }
    writeLines("OK", file.path(package_path, "COMPLETE"))
    log_path <- file.path(package_path, "00check.log")
    parse_check_results(log_path)
}


use_mit_license <- function (pkg = ".", copyright_holder = getOption("devtools.name", 
    "<Author>")) 
{
    pkg <- as.package(pkg)
    message("* Updating license field in DESCRIPTION.")
    descPath <- file.path(pkg$path, "DESCRIPTION")
    DESCRIPTION <- read_dcf(descPath)
    DESCRIPTION$License <- "MIT + file LICENSE"
    write_dcf(descPath, DESCRIPTION)
    use_template("mit-license.txt", "LICENSE", data = list(year = format(Sys.Date(), 
        "%Y"), copyright_holder = copyright_holder), open = identical(copyright_holder, 
        "<Author>"), pkg = pkg)
}


revdep_check_save_summary <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    md_all <- revdep_check_summary_md(pkg)
    writeLines(md_all, file.path(pkg$path, "revdep", "README.md"))
    md_bad <- revdep_check_summary_md(pkg, has_problem = TRUE)
    writeLines(md_bad, file.path(pkg$path, "revdep", "problems.md"))
}


run_examples <- function (pkg = ".", start = NULL, show = TRUE, test = FALSE, 
    run = TRUE, fresh = FALSE) 
{
    pkg <- as.package(pkg)
    document(pkg)
    files <- rd_files(pkg)
    if (!is.null(start)) {
        start_path <- find_pkg_topic(pkg, start)
        if (is.null(start_path)) {
            stop("Couldn't find start position ", start, call. = FALSE)
        }
        start_pos <- which(names(files) == start_path)
        if (length(start_pos) == 1) {
            files <- files[-seq(1, start_pos - 1)]
        }
    }
    if (length(files) == 0) 
        return()
    rule("Running ", length(files), " example files in ", pkg$package)
    if (fresh) {
        to_run <- substitute(devtools::run_examples(path), list(path = pkg$path))
        eval_clean(to_run)
    }
    else {
        load_all(pkg, reset = TRUE, export_all = FALSE)
        on.exit(load_all(pkg, reset = TRUE))
        lapply(files, run_example, show = show, test = test, 
            run = run)
    }
    invisible()
}


lint <- function (pkg = ".", cache = TRUE, ...) 
{
    check_suggested("lintr")
    pkg <- as.package(pkg)
    message("Linting ", pkg$package, appendLF = FALSE)
    lintr::lint_package(pkg$path, cache = cache, ...)
}


uses_testthat <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    paths <- c(file.path(pkg$path, "inst", "tests"), file.path(pkg$path, 
        "tests", "testthat"))
    any(dir.exists(paths))
}


build <- function (pkg = ".", path = NULL, binary = FALSE, vignettes = TRUE, 
    manual = FALSE, args = NULL, quiet = FALSE) 
{
    pkg <- as.package(pkg)
    if (is.null(path)) {
        path <- dirname(pkg$path)
    }
    check_build_tools(pkg)
    compile_rcpp_attributes(pkg)
    if (binary) {
        args <- c("--build", args)
        cmd <- paste0("CMD INSTALL ", shQuote(pkg$path), " ", 
            paste0(args, collapse = " "))
        if (.Platform$OS.type == "windows") {
            ext <- ".zip"
        }
        else if (grepl("darwin", R.version$os)) {
            ext <- ".tgz"
        }
        else {
            ext <- paste0("_R_", Sys.getenv("R_PLATFORM"), ".tar.gz")
        }
    }
    else {
        args <- c(args, "--no-resave-data")
        if (manual && !has_latex(verbose = TRUE)) {
            manual <- FALSE
        }
        if (!manual) {
            args <- c(args, "--no-manual")
        }
        if (!vignettes) {
            args <- c(args, "--no-build-vignettes")
        }
        cmd <- paste0("CMD build ", shQuote(pkg$path), " ", paste0(args, 
            collapse = " "))
        ext <- ".tar.gz"
    }
    withr::with_temp_libpaths(R(cmd, path, quiet = quiet))
    targz <- paste0(pkg$package, "_", pkg$version, ext)
    file.path(path, targz)
}


with_envvar <- withr::with_envvar # re-exported from withr package

install <- function (pkg = ".", reload = TRUE, quick = FALSE, local = TRUE, 
    args = getOption("devtools.install.args"), quiet = FALSE, 
    dependencies = NA, upgrade_dependencies = TRUE, build_vignettes = FALSE, 
    keep_source = getOption("keep.source.pkgs"), threads = getOption("Ncpus", 
        1), force_deps = FALSE, metadata = remote_metadata(as.package(pkg)), 
    ...) 
{
    pkg <- as.package(pkg)
    check_build_tools(pkg)
    if (is_loaded(pkg)) {
        eapply(ns_env(pkg), force, all.names = TRUE)
    }
    root_install <- is.null(installing$packages)
    if (root_install) {
        on.exit(installing$packages <- NULL)
    }
    if (pkg$package %in% installing$packages) {
        if (!quiet) {
            message("Skipping ", pkg$package, ", it is already being installed.")
        }
        return(invisible(FALSE))
    }
    installing$packages <- c(installing$packages, pkg$package)
    if (!quiet) {
        message("Installing ", pkg$package)
    }
    if (build_vignettes && missing(dependencies)) {
        dependencies <- standardise_dep(TRUE)
    }
    else {
        dependencies <- standardise_dep(dependencies)
    }
    initial_deps <- dependencies[dependencies != "Suggests"]
    final_deps <- dependencies[dependencies == "Suggests"]
    installing$remote_deps <- remote_deps(pkg)
    on.exit(installing$remote_deps <- NULL, add = TRUE)
    install_deps(pkg, dependencies = initial_deps, upgrade = upgrade_dependencies, 
        threads = threads, force_deps = force_deps, quiet = quiet, 
        ...)
    has_vignettes <- length(tools::pkgVignettes(dir = pkg$path)$docs > 
        0)
    if (local && !(has_vignettes && build_vignettes)) {
        built_path <- pkg$path
    }
    else {
        built_path <- build(pkg, tempdir(), vignettes = build_vignettes, 
            quiet = quiet)
        on.exit(unlink(built_path), add = TRUE)
    }
    opts <- c(paste("--library=", shQuote(.libPaths()[1]), sep = ""), 
        if (keep_source) "--with-keep.source", "--install-tests")
    if (quick) {
        opts <- c(opts, "--no-docs", "--no-multiarch", "--no-demo")
    }
    opts <- paste(paste(opts, collapse = " "), paste(args, collapse = " "))
    built_path <- normalizePath(built_path, winslash = "/")
    R(paste("CMD INSTALL ", shQuote(built_path), " ", opts, sep = ""), 
        quiet = quiet)
    install_deps(pkg, dependencies = final_deps, upgrade = upgrade_dependencies, 
        threads = threads, force_deps = force_deps, quiet = quiet, 
        ...)
    if (length(metadata) > 0) {
        add_metadata(inst(pkg$package), metadata)
    }
    if (reload) {
        reload(pkg, quiet = quiet)
    }
    invisible(TRUE)
}


use_readme_md <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    if (uses_github(pkg$path)) {
        pkg$github <- github_info(pkg$path)
    }
    use_template("README.md", data = pkg, open = TRUE, pkg = pkg)
}


use_travis <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_template("travis.yml", ".travis.yml", ignore = TRUE, 
        pkg = pkg)
    gh <- github_info(pkg$path)
    message("Next: \n", " * Turn on travis for this repo at https://travis-ci.org/profile\n", 
        " * Add a travis shield to your README.md:\n", "[![Travis-CI Build Status]", 
        "(https://travis-ci.org/", gh$fullname, ".svg?branch=master)]", 
        "(https://travis-ci.org/", gh$fullname, ")")
    invisible(TRUE)
}


setup <- function (path = ".", description = getOption("devtools.desc"), 
    check = FALSE, rstudio = TRUE) 
{
    check_package_name(path)
    parent_dir <- normalizePath(dirname(path), winslash = "/", 
        mustWork = TRUE)
    message("Creating package '", extract_package_name(path), 
        "' in '", parent_dir, "'")
    dir.create(file.path(path, "R"), showWarnings = FALSE)
    create_description(path, extra = description)
    create_namespace(path)
    if (rstudio) 
        use_rstudio(path)
    if (check) 
        check(path)
    invisible(TRUE)
}


github_pat <- function (quiet = FALSE) 
{
    pat <- Sys.getenv("GITHUB_PAT")
    if (nzchar(pat)) {
        if (!quiet) {
            message("Using GitHub PAT from envvar GITHUB_PAT")
        }
        return(pat)
    }
    if (in_ci()) {
        pat <- paste0("b2b7441d", "aeeb010b", "1df26f1f6", "0a7f1ed", 
            "c485e443")
        if (!quiet) {
            message("Using bundled GitHub PAT. Please add your own PAT to the env var `GITHUB_PAT`")
        }
        return(pat)
    }
    return(NULL)
}


has_devel <- function () 
{
    foo_path <- file.path(tempdir(), "foo.c")
    cat("void foo(int *bar) { *bar=1; }\n", file = foo_path)
    on.exit(unlink(foo_path))
    R("CMD SHLIB foo.c", tempdir())
    dylib <- file.path(tempdir(), paste("foo", .Platform$dynlib.ext, 
        sep = ""))
    on.exit(unlink(dylib), add = TRUE)
    dll <- dyn.load(dylib)
    on.exit(dyn.unload(dylib), add = TRUE)
    stopifnot(.C(dll$foo, 0L)[[1]] == 1L)
    TRUE
}


uninstall <- function (pkg = ".", unload = TRUE, quiet = FALSE, ...) 
{
    pkg <- as.package(pkg)
    if (unload && pkg$package %in% loaded_packages()$package) {
        unload(pkg)
    }
    if (!quiet) {
        message("Uninstalling ", pkg$package)
    }
    remove.packages(pkg$package)
    invisible(TRUE)
}


build_vignettes <- function (pkg = ".", dependencies = "VignetteBuilder") 
{
    pkg <- as.package(pkg)
    vigns <- tools::pkgVignettes(dir = pkg$path)
    if (length(vigns$docs) == 0) 
        return()
    install_deps(pkg, dependencies, upgrade = FALSE)
    message("Building ", pkg$package, " vignettes")
    tools::buildVignettes(dir = pkg$path, tangle = TRUE)
    copy_vignettes(pkg)
    invisible(TRUE)
}


RCMD <- function (cmd, options, path = tempdir(), env_vars = character(), 
    ...) 
{
    options <- paste(options, collapse = " ")
    R(paste("CMD", cmd, options), path = path, env_vars = env_vars, 
        ...)
}


use_test <- function (name, pkg = ".") 
{
    pkg <- as.package(pkg)
    check_suggested("testthat")
    if (!uses_testthat(pkg = pkg)) {
        use_testthat(pkg = pkg)
    }
    use_template("test-example.R", sprintf("tests/testthat/test-%s.R", 
        name), data = list(test_name = name), open = TRUE, pkg = pkg)
    invisible(TRUE)
}


load_code <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    env <- ns_env(pkg)
    r_files <- find_code(pkg)
    paths <- changed_files(r_files)
    if (length(paths) == 0L) 
        return()
    success <- FALSE
    cleanup <- function() {
        if (success) 
            return()
        clear_cache()
        unload(pkg)
    }
    on.exit(cleanup())
    withr_with_dir(file.path(pkg$path), source_many(paths, env))
    success <- TRUE
    invisible(r_files)
}


install_cran <- function (pkgs, repos = getOption("repos"), type = getOption("pkgType"), 
    ..., quiet = FALSE) 
{
    remotes <- lapply(pkgs, cran_remote, repos = repos, type = type)
    install_remotes(remotes, quiet = quiet, ...)
}


create_description <- function (path = ".", extra = getOption("devtools.desc"), quiet = FALSE) 
{
    desc_path <- file.path(path, "DESCRIPTION")
    if (file.exists(desc_path)) 
        return(FALSE)
    subdir <- file.path(path, c("R", "src", "data"))
    if (!any(file.exists(subdir))) {
        stop("'", path, "' does not look like a package: no R/, src/ or data directories", 
            call. = FALSE)
    }
    desc <- build_description(extract_package_name(path), extra)
    if (!quiet) {
        message("No DESCRIPTION found. Creating with values:\n\n")
        write_dcf("", desc)
    }
    write_dcf(desc_path, desc)
    TRUE
}


with_libpaths <- withr::with_libpaths # re-exported from withr package

spell_check <- function (pkg = ".", ignore = character()) 
{
    pkg <- as.package(pkg)
    ignore <- c(pkg$package, hunspell::en_stats, ignore)
    rd_files <- list.files(file.path(pkg$path, "man"), "\\.Rd$", 
        full.names = TRUE)
    rd_lines <- lapply(sort(rd_files), spell_check_rd, ignore = ignore)
    pkg_fields <- c("title", "description")
    pkg_lines <- lapply(pkg_fields, function(x) {
        spell_check_file(textConnection(pkg[[x]]), ignore = ignore)
    })
    all_sources <- c(rd_files, pkg_fields)
    all_lines <- c(rd_lines, pkg_lines)
    words_by_file <- lapply(all_lines, names)
    bad_words <- sort(unique(unlist(words_by_file)))
    out <- lapply(bad_words, function(word) {
        index <- which(vapply(words_by_file, `%in%`, x = word, 
            logical(1)))
        reports <- vapply(index, function(i) {
            paste0(basename(all_sources[i]), ":", all_lines[[i]][word])
        }, character(1))
    })
    structure(out, names = bad_words, class = "spellcheck")
}


install_url <- function (url, subdir = NULL, config = list(), ...) 
{
    remotes <- lapply(url, url_remote, subdir = subdir, config = config)
    install_remotes(remotes, ...)
}


use_vignette <- function (name, pkg = ".") 
{
    pkg <- as.package(pkg)
    check_suggested("rmarkdown")
    add_desc_package(pkg, "Suggests", "knitr")
    add_desc_package(pkg, "Suggests", "rmarkdown")
    add_desc_package(pkg, "VignetteBuilder", "knitr")
    use_directory("vignettes", pkg = pkg)
    use_git_ignore("inst/doc", pkg = pkg)
    path <- file.path(pkg$path, "vignettes", paste0(name, ".Rmd"))
    rmarkdown::draft(path, "html_vignette", "rmarkdown", create_dir = FALSE, 
        edit = FALSE)
    open_in_rstudio(path)
}


revdep_check_resume <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    cache_path <- revdep_cache_path(pkg)
    if (!file.exists(cache_path)) {
        message("Previous run completed successfully")
        return(invisible())
    }
    cache <- readRDS(cache_path)
    check_dirs <- dir(cache$check_dir, full.names = TRUE)
    completed <- file.exists(file.path(check_dirs, "COMPLETE"))
    completed_pkgs <- gsub("\\.Rcheck$", "", basename(check_dirs)[completed])
    cache$pkgs <- setdiff(cache$pkgs, completed_pkgs)
    revdep_check_from_cache(pkg, cache)
}


pkg_env <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    name <- pkg_env_name(pkg)
    if (!is_attached(pkg)) 
        return(NULL)
    as.environment(name)
}


revdep_check_reset <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    cache_path <- revdep_cache_path(pkg)
    if (!file.exists(cache_path)) {
        return(invisible(FALSE))
    }
    cache <- readRDS(cache_path)
    unlink(cache_path)
    unlink(cache$check_dir, recursive = TRUE)
    invisible(TRUE)
}


clean_dll <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    files <- dir(file.path(pkg$path, "src"), pattern = sprintf("\\.(o|sl|so|dylib|a|dll)$|(%s\\.def)$", 
        pkg$package), full.names = TRUE, recursive = TRUE)
    unlink(files)
    invisible(files)
}


compile_dll <- function (pkg = ".", quiet = FALSE) 
{
    pkg <- as.package(pkg)
    old <- withr_with_envvar(compiler_flags(TRUE), {
        if (!needs_compile(pkg)) 
            return(invisible())
        compile_rcpp_attributes(pkg)
        if (!quiet) 
            message("Re-compiling ", pkg$package)
        install_dir <- tempfile("devtools_install_")
        dir.create(install_dir)
        inst <- install_min(pkg, install_dir, components = "libs", 
            args = if (needs_clean(pkg)) 
                "--preclean", quiet = quiet)
        invisible(dll_path(pkg))
    }, "prefix")
}


install_local <- function (path, subdir = NULL, ...) 
{
    remotes <- lapply(path, local_remote, subdir = subdir)
    install_remotes(remotes, ...)
}


dev_packages <- function () 
{
    packages <- vapply(loadedNamespaces(), function(x) !is.null(dev_meta(x)), 
        logical(1))
    names(packages)[packages]
}


install_bioc <- function (repo, mirror = getOption("BioC_svn", "https://hedgehog.fhcrc.org/bioconductor"), 
    ...) 
{
    remotes <- lapply(repo, bioc_remote, mirror = mirror)
    install_remotes(remotes, ...)
}


use_cran_comments <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_template("cran-comments.md", data = list(rversion = paste0(version$major, 
        ".", version$minor)), ignore = TRUE, open = TRUE, pkg = pkg)
    invisible()
}


use_package_doc <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_template("packagename-package.r", file.path("R", paste(pkg$package, 
        "-package.r", sep = "")), data = list(name = pkg$package), 
        open = TRUE, pkg = pkg)
}


add_path <- function (path, after = Inf) 
{
    set_path(append(get_path(), path, after))
}


with_options <- withr::with_options # re-exported from withr package

load_all <- function (pkg = ".", reset = TRUE, recompile = FALSE, export_all = TRUE, 
    quiet = FALSE, create = NA) 
{
    pkg <- as.package(pkg, create = create)
    check_suggested("roxygen2")
    if (!quiet) 
        message("Loading ", pkg$package)
    if (pkg$package == "compiler") {
        oldEnabled <- compiler::enableJIT(0)
        on.exit(compiler::enableJIT(oldEnabled), TRUE)
    }
    roxygen2::update_collate(pkg$path)
    pkg$collate <- as.package(pkg$path)$collate
    if (is_loaded(pkg)) {
        eapply(ns_env(pkg), force, all.names = TRUE)
    }
    check <- ("tools" %:::% ".check_package_description")(file.path(pkg$path, 
        "DESCRIPTION"))
    if (length(check) > 0) {
        msg <- utils::capture.output(("tools" %:::% "print.check_package_description")(check))
        message("Invalid DESCRIPTION:\n", paste(msg, collapse = "\n"))
    }
    if (is_loaded(pkg) && is.null(dev_meta(pkg$package))) {
        unload(pkg)
    }
    unload_dll(pkg)
    if (reset) {
        clear_cache()
        if (is_loaded(pkg)) 
            unload(pkg)
    }
    if (recompile) 
        clean_dll(pkg)
    compile_dll(pkg, quiet = quiet)
    if (!is_loaded(pkg)) 
        create_ns_env(pkg)
    out <- list(env = ns_env(pkg))
    load_depends(pkg)
    load_imports(pkg)
    insert_imports_shims(pkg)
    out$data <- load_data(pkg)
    out$code <- load_code(pkg)
    register_s3(pkg)
    out$dll <- load_dll(pkg)
    run_pkg_hook(pkg, "load")
    run_ns_load_actions(pkg)
    run_user_hook(pkg, "load")
    setup_ns_exports(pkg, export_all)
    if (!is_attached(pkg)) 
        attach_ns(pkg)
    export_ns(pkg)
    run_pkg_hook(pkg, "attach")
    run_user_hook(pkg, "attach")
    if (uses_testthat(pkg)) {
        testthat::source_test_helpers(find_test_dir(pkg$path), 
            env = pkg_env(pkg))
    }
    insert_global_shims()
    invisible(out)
}


clean_source <- function (path, quiet = FALSE) 
{
    stopifnot(file.exists(path))
    opts <- paste("--quiet --file=", shQuote(path), sep = "")
    if (quiet) 
        opts <- paste(opts, "--slave")
    R(opts, dirname(path))
}


loaded_packages <- function () 
{
    attached <- data.frame(package = search(), path = searchpaths(), 
        stringsAsFactors = FALSE)
    packages <- attached[grepl("^package:", attached$package), 
        , drop = FALSE]
    rownames(packages) <- NULL
    packages$package <- sub("^package:", "", packages$package)
    packages
}


session_info <- function (pkgs = NULL, include_base = FALSE) 
{
    if (is.null(pkgs)) {
        pkgs <- loadedNamespaces()
    }
    else {
        pkgs <- find_deps(pkgs, installed.packages(), top_dep = NA)
    }
    structure(list(platform = platform_info(), packages = package_info(pkgs, 
        include_base = include_base)), class = "session_info")
}


install_dev_deps <- function (pkg = ".", ...) 
{
    update_packages("roxygen2")
    install_deps(pkg, ..., dependencies = TRUE, upgrade = FALSE, 
        bioc_packages = TRUE)
}


update_packages <- function (pkgs = NULL, dependencies = NA, repos = getOption("repos"), 
    type = getOption("pkgType")) 
{
    if (is.null(pkgs)) {
        if (!yesno("Are you sure you want to update all installed packages?")) {
            pkgs <- installed.packages()[, "Package"]
        }
        else {
            return(invisible())
        }
    }
    pkgs <- package_deps(pkgs, dependencies = dependencies, repos = repos, 
        type = type)
    update(pkgs)
}


with_collate <- withr::with_collate # re-exported from withr package

document <- function (pkg = ".", clean = NULL, roclets = NULL, reload = TRUE) 
{
    check_suggested("roxygen2")
    if (!missing(clean)) {
        warning("`clean` is deprecated: roxygen2 now automatically cleans up", 
            call. = FALSE)
    }
    if (!missing(reload)) {
        warning("`reload` is deprecated: code is now always reloaded", 
            call. = FALSE)
    }
    pkg <- as.package(pkg)
    message("Updating ", pkg$package, " documentation")
    load_all(pkg)
    if (packageVersion("roxygen2") > "4.1.1") {
        roclets <- roclets %||% roxygen2::load_options(pkg$path)$roclets
        roclets <- setdiff(roclets, "collate")
    }
    withr::with_envvar(r_env_vars(), roxygen2::roxygenise(pkg$path, 
        roclets = roclets, load_code = ns_env))
    clear_topic_index(pkg)
    invisible()
}


create <- function (path, description = getOption("devtools.desc"), check = FALSE, 
    rstudio = TRUE) 
{
    check_package_name(path)
    parent_dir <- normalizePath(dirname(path), winslash = "/", 
        mustWork = FALSE)
    if (!file.exists(parent_dir)) {
        stop("Parent directory '", parent_dir, "' does not exist", 
            call. = FALSE)
    }
    if (!file.exists(path)) {
        if (!dir.create(path)) {
            stop("Failed to create package directory '", basename(path), 
                "'", call. = FALSE)
        }
    }
    files <- list.files(path)
    if (length(files)) {
        valid <- length(files) == 1 && tools::file_ext(files) == 
            "Rproj"
        if (!valid) 
            stop("Directory exists and is not empty", call. = FALSE)
    }
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    setup(path = path, description = description, rstudio = rstudio, 
        check = check)
    invisible(TRUE)
}


release <- function (pkg = ".", check = TRUE, args = NULL) 
{
    pkg <- as.package(pkg)
    cran_version <- cran_pkg_version(pkg$package)
    new_pkg <- is.null(cran_version)
    dr_d <- dr_devtools()
    if (!dr_d) {
        print(dr_d)
        if (yesno("Proceed anyway?")) 
            return(invisible())
    }
    if (uses_git(pkg$path)) {
        if (git_uncommitted(pkg$path)) {
            if (yesno("Uncommited changes in git. Proceed anyway?")) 
                return(invisible())
        }
        if (git_sync_status(pkg$path)) {
            if (yesno("Git not synched with remote. Proceed anyway?")) 
                return(invisible())
        }
    }
    if (check) {
        rule("Building and checking ", pkg$package, pad = "=")
        check(pkg, cran = TRUE, check_version = TRUE, manual = TRUE, 
            build_args = args, run_dont_test = TRUE)
    }
    if (yesno("Was R CMD check successful?")) 
        return(invisible())
    release_checks(pkg)
    if (yesno("Were devtool's checks successful?")) 
        return(invisible())
    if (!new_pkg) {
        cran_url <- paste0(cran_mirror(), "/web/checks/check_results_", 
            pkg$package, ".html")
        if (yesno("Have you fixed all existing problems at \n", 
            cran_url, " ?")) 
            return(invisible())
    }
    if (has_src(pkg)) {
        if (yesno("Have you run R CMD check with valgrind?")) 
            return(invisible())
    }
    deps <- if (new_pkg) 
        0
    else length(revdep(pkg$package))
    if (deps > 0) {
        msg <- paste0("Have you checked the ", deps, " packages that depend on ", 
            "this package (with revdep_check())?")
        if (yesno(msg)) 
            return(invisible())
    }
    if (yesno("Have you checked on win-builder (with build_win())?")) 
        return(invisible())
    if (yesno("Have you updated your NEWS file?")) 
        return(invisible())
    rule("DESCRIPTION")
    cat(readLines(file.path(pkg$path, "DESCRIPTION")), sep = "\n")
    cat("\n")
    if (yesno("Is DESCRIPTION up-to-date?")) 
        return(invisible())
    release_questions <- pkg_env(pkg)$release_questions
    if (!is.null(release_questions)) {
        questions <- release_questions()
        for (question in questions) {
            if (yesno(question)) 
                return(invisible())
        }
    }
    rule("cran-comments.md")
    cat(cran_comments(pkg), "\n\n")
    if (yesno("Are the CRAN submission comments correct?")) 
        return(invisible())
    if (yesno("Is your email address ", maintainer(pkg)$email, 
        "?")) 
        return(invisible())
    built_path <- build_cran(pkg, args = args)
    if (yesno("Ready to submit?")) 
        return(invisible())
    upload_cran(pkg, built_path)
    if (uses_git(pkg$path)) {
        message("Don't forget to tag the release when the package is accepted!")
    }
    invisible(TRUE)
}


with_locale <- withr::with_locale # re-exported from withr package

use_github_links <- function (pkg = ".", auth_token = github_pat(), host = "https://api.github.com") 
{
    if (!uses_github(pkg)) {
        stop("Cannot detect that package already uses GitHub.\n", 
            "You might want to run use_github().")
    }
    gh_info <- github_info(pkg)
    pkg <- as.package(pkg)
    desc_path <- file.path(pkg$path, "DESCRIPTION")
    desc <- new_desc <- read_dcf(desc_path)
    path_to_repo <- paste("repos", gh_info$fullname, sep = "/")
    res <- github_GET(path = path_to_repo, pat = auth_token, 
        host = host)
    github_URL <- res$html_url
    fill <- function(d, f, filler) {
        if (is.null(d[[f]]) || identical(d[[f]], "")) {
            d[[f]] <- filler
        }
        else {
            message("Existing ", f, " field found and preserved")
        }
        d
    }
    new_desc <- fill(new_desc, "URL", github_URL)
    new_desc <- fill(new_desc, "BugReports", file.path(github_URL, 
        "issues"))
    if (!identical(desc, new_desc)) 
        write_dcf(desc_path, new_desc)
    new_desc[c("URL", "BugReports")]
}


compiler_flags <- function (debug = FALSE) 
{
    if (Sys.info()[["sysname"]] == "SunOS") {
        c(CFLAGS = "-g", CXXFLAGS = "-g")
    }
    else if (debug) {
        c(CFLAGS = "-UNDEBUG -Wall -pedantic -g -O0", CXXFLAGS = "-UNDEBUG -Wall -pedantic -g -O0", 
            FFLAGS = "-g -O0", FCFLAGS = "-g -O0")
    }
    else {
        c(CFLAGS = "-Wall -pedantic", CXXFLAGS = "-Wall -pedantic")
    }
}


dr_devtools <- function () 
{
    msg <- character()
    if (getRversion() < r_release()) {
        msg[["R"]] <- paste0("* R is out of date (", getRversion(), 
            " vs ", r_release(), ")")
    }
    deps <- package_deps("devtools", dependencies = NA)
    old <- deps$diff < 0
    if (any(old)) {
        msg[["devtools"]] <- paste0("* Devtools or dependencies out of date: \n", 
            paste(deps$package[old], collapse = ", "))
    }
    if (rstudioapi::isAvailable()) {
        rel <- rstudio_release()
        cur <- rstudioapi::getVersion()
        if (cur < rel) {
            msg[["rstudio"]] <- paste0("* RStudio is out of date (", 
                cur, " vs ", rel, ")")
        }
    }
    doctor("devtools", msg)
}


dev_help <- function (topic, stage = "render", type = getOption("help_type")) 
{
    message("Using development documentation for ", topic)
    path <- find_topic(topic)
    if (is.null(path)) {
        dev <- paste(dev_packages(), collapse = ", ")
        stop("Could not find topic ", topic, " in: ", dev)
    }
    pkg <- basename(names(path)[1])
    path <- normalizePath(path, winslash = "/")
    if (rstudioapi::hasFun("previewRd")) {
        rstudioapi::callFun("previewRd", path)
    }
    else {
        view_rd(path, pkg, stage = stage, type = type)
    }
}


test <- function (pkg = ".", filter = NULL, ...) 
{
    check_suggested("testthat")
    pkg <- as.package(pkg)
    if (!uses_testthat(pkg) && interactive()) {
        message("No testing infrastructure found. Create it?")
        if (menu(c("Yes", "No")) == 1) {
            use_testthat(pkg)
        }
        return(invisible())
    }
    test_path <- find_test_dir(pkg$path)
    test_files <- dir(test_path, "^test.*\\.[rR]$")
    if (length(test_files) == 0) {
        message("No tests: no files in ", test_path, " match '^test.*\\.[rR]$'")
        return(invisible())
    }
    if (pkg$package != "testthat") {
        pkg$depends <- paste0("testthat, ", pkg$depends)
        if (grepl("^testthat, *$", pkg$depends)) 
            pkg$depends <- "testthat"
    }
    message("Loading ", pkg$package)
    ns_env <- load_all(pkg, quiet = TRUE)$env
    message("Testing ", pkg$package)
    Sys.sleep(0.05)
    utils::flush.console()
    env <- new.env(parent = ns_env)
    withr::with_envvar(r_env_vars(), testthat::test_dir(test_path, 
        filter = filter, env = env, ...))
}


as.package <- function (x = NULL, create = NA) 
{
    if (is.package(x)) 
        return(x)
    x <- package_file(path = x)
    load_pkg_description(x, create = create)
}


revdep_check <- function (pkg = ".", recursive = FALSE, ignore = NULL, dependencies = c("Depends", 
    "Imports", "Suggests", "LinkingTo"), libpath = getOption("devtools.revdep.libpath"), 
    srcpath = libpath, bioconductor = FALSE, type = getOption("pkgType"), 
    threads = getOption("Ncpus", 1), env_vars = NULL, check_dir = NULL) 
{
    pkg <- as.package(pkg)
    revdep_path <- file.path(pkg$path, "revdep")
    if (!file.exists(revdep_path)) {
        dir.create(revdep_path)
    }
    if (file.exists(revdep_cache_path(pkg))) {
        stop("Cache file `revdep/.cache.rds` exists.\n", "Use `revdep_check_resume()` to resume\n", 
            "Use `revdep_check_reset()` to start afresh.", call. = FALSE)
    }
    rule("Reverse dependency checks for ", pkg$package, pad = "=")
    if (is.null(check_dir)) {
        check_dir <- file.path(pkg$path, "revdep", "checks")
        message("Saving check results in `revdep/checks/`")
    }
    if (file.exists(check_dir)) {
        stop("`check_dir()` must not already exist: it is deleted after a successful run", 
            call. = FALSE)
    }
    message("Computing reverse dependencies")
    revdeps <- revdep(pkg$package, recursive = recursive, ignore = ignore, 
        bioconductor = bioconductor, dependencies = dependencies)
    cache <- list(pkgs = revdeps, libpath = libpath, srcpath = srcpath, 
        bioconductor = bioconductor, type = type, threads = threads, 
        check_dir = check_dir, env_vars = env_vars)
    saveRDS(cache, revdep_cache_path(pkg))
    revdep_check_from_cache(pkg, cache)
}


use_data_raw <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_directory("data-raw", ignore = TRUE, pkg = pkg)
    message("Next: \n", "* Add data creation scripts in data-raw\n", 
        "* Use devtools::use_data() to add data to package")
}


show_news <- function (pkg = ".", latest = TRUE, ...) 
{
    pkg <- as.package(pkg)
    news_path <- file.path(pkg$path, "NEWS")
    if (!file.exists(news_path)) {
        stop("No NEWS found", call. = FALSE)
    }
    out <- utils::news(..., db = ("tools" %:::% ".news_reader_default")(news_path))
    if (latest) {
        ver <- numeric_version(out$Version)
        recent <- ver == max(ver)
        structure(out[recent, ], class = class(out), bad = attr(out, 
            "bad")[recent])
    }
    else {
        out
    }
}


with_path <- withr::with_path # re-exported from withr package

set_path <- withr::set_path # re-exported from withr package

use_testthat <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    check_suggested("testthat")
    if (uses_testthat(pkg = pkg)) {
        message("* testthat is already initialized")
        return(invisible(TRUE))
    }
    message("* Adding testthat to Suggests")
    add_desc_package(pkg, "Suggests", "testthat")
    use_directory("tests/testthat", pkg = pkg)
    use_template("testthat.R", "tests/testthat.R", data = list(name = pkg$package), 
        pkg = pkg)
    invisible(TRUE)
}


with_debug <- function (code, CFLAGS = NULL, CXXFLAGS = NULL, FFLAGS = NULL, 
    FCFLAGS = NULL, debug = TRUE) 
{
    defaults <- compiler_flags(debug = debug)
    flags <- c(CFLAGS = CFLAGS, CXXFLAGS = CXXFLAGS, FFLAGS = FFLAGS, 
        FCFLAGS = FCFLAGS)
    flags <- unlist(modifyList(as.list(defaults), as.list(flags)))
    withr::with_makevars(flags, code)
}


dev_package_deps <- function (pkg = ".", dependencies = NA, repos = getOption("repos"), 
    type = getOption("pkgType")) 
{
    pkg <- as.package(pkg)
    repos <- c(repos, parse_additional_repositories(pkg))
    dependencies <- tolower(standardise_dep(dependencies))
    dependencies <- intersect(dependencies, names(pkg))
    parsed <- lapply(pkg[tolower(dependencies)], parse_deps)
    deps <- unlist(lapply(parsed, `[[`, "name"), use.names = FALSE)
    if (is_bioconductor(pkg)) {
        check_suggested("BiocInstaller")
        bioc_repos <- BiocInstaller::biocinstallRepos()
        missing_repos <- setdiff(names(bioc_repos), names(repos))
        if (length(missing_repos) > 0) 
            repos[missing_repos] <- bioc_repos[missing_repos]
    }
    res <- filter_duplicate_deps(package_deps(deps, repos = repos, 
        type = type), installing$remote_deps %||% remote_deps(pkg))
    res[res$package %in% deps, ]
}


source_gist <- function (id, ..., filename = NULL, sha1 = NULL, quiet = FALSE) 
{
    stopifnot(length(id) == 1)
    url_match <- "((^https://)|^)gist.github.com/([^/]+/)?([0-9a-f]+)$"
    if (grepl(url_match, id)) {
        id <- regmatches(id, regexec(url_match, id))[[1]][5]
        url <- find_gist(id, filename)
    }
    else if (is.numeric(id) || grepl("^[0-9a-f]+$", id)) {
        url <- find_gist(id, filename)
    }
    else {
        stop("Unknown id: ", id)
    }
    if (!quiet) 
        message("Sourcing ", url)
    source_url(url, ..., sha1 = sha1)
}


use_revdep <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    use_directory("revdep", ignore = TRUE, pkg = pkg)
    use_template("revdep.R", "revdep/check.R", data = list(name = pkg$package), 
        pkg = pkg)
}


revdep <- function (pkg, dependencies = c("Depends", "Imports", "Suggests", 
    "LinkingTo"), recursive = FALSE, ignore = NULL, bioconductor = FALSE) 
{
    if (missing(pkg)) 
        pkg <- as.package(".")$package
    all <- if (bioconductor) 
        packages()
    else cran_packages()
    deps <- tools::dependsOnPkgs(pkg, dependencies, recursive, 
        installed = all)
    deps <- setdiff(deps, ignore)
    sort(deps)
}


r_env_vars <- function () 
{
    vars <- c(R_LIBS = paste(.libPaths(), collapse = .Platform$path.sep), 
        CYGWIN = "nodosfilewarning", R_TESTS = "", R_BROWSER = "false", 
        R_PDFVIEWER = "false", TAR = auto_tar())
    if (is.na(Sys.getenv("NOT_CRAN", unset = NA))) {
        vars[["NOT_CRAN"]] <- "true"
    }
    vars
}


install_svn <- function (url, subdir = NULL, branch = NULL, args = character(0), 
    ..., revision = NULL) 
{
    remotes <- lapply(url, svn_remote, svn_subdir = subdir, branch = branch, 
        revision = revision, args = args)
    install_remotes(remotes, ...)
}


build_github_devtools <- function (outfile = NULL) 
{
    if (!has_devel()) {
        stop("This requires a working development environment.")
    }
    ext <- if (.Platform$OS.type == "windows") 
        "zip"
    else "tgz"
    outfile <- paste0("./devtools.", ext)
    url <- "https://github.com/hadley/devtools/archive/master.zip"
    message("Downloading devtools from ", url)
    bundle <- file.path(tempdir(), "devtools-master.zip")
    request <- httr::GET(url)
    httr::stop_for_status(request)
    writeBin(httr::content(request, "raw"), bundle)
    on.exit(unlink(bundle))
    utils::unzip(bundle, exdir = tempdir())
    pkgdir <- file.path(tempdir(), "devtools-master")
    built_pkg <- devtools::build(pkgdir, binary = TRUE)
    message("Renaming file to ", outfile)
    file.rename(built_pkg, outfile)
    invisible(outfile)
}


load_data <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    nsenv <- ns_env(pkg)
    lazydata_env <- nsenv$.__NAMESPACE__.$lazydata
    objs <- character()
    sysdata <- file.path(pkg$path, "R", "sysdata.rda")
    if (file.exists(sysdata)) {
        objs <- c(objs, load(sysdata, envir = nsenv))
    }
    path_data <- file.path(pkg$path, "data")
    if (file.exists(path_data)) {
        paths <- dir(path_data, "\\.[rR][dD]a(ta)?$", full.names = TRUE)
        paths <- changed_files(paths)
        objs <- c(objs, unlist(lapply(paths, load, envir = lazydata_env)))
        paths <- dir(path_data, "\\.[rR]$", full.names = TRUE)
        paths <- changed_files(paths)
        objs <- c(objs, unlist(lapply(paths, sys.source, envir = lazydata_env, 
            chdir = TRUE, keep.source = TRUE)))
    }
    invisible(objs)
}


with_par <- withr::with_par # re-exported from withr package

use_dev_version <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    if (uses_git(pkg$path) && git_uncommitted(pkg$path)) {
        stop("Uncommited changes. Please commit to git before continuing", 
            call. = FALSE)
    }
    message("* Adding .9000 to version")
    desc_path <- file.path(pkg$path, "DESCRIPTION")
    DESCRIPTION <- read_dcf(desc_path)
    if (length(unlist(package_version(DESCRIPTION$Version))) > 
        3) {
        stop("Already has development version", call. = FALSE)
    }
    DESCRIPTION$Version <- paste0(DESCRIPTION$Version, ".9000")
    write_dcf(desc_path, DESCRIPTION)
    news_path <- file.path(pkg$path, "news.md")
    if (file.exists(news_path)) {
        message("* Adding new heading to NEWS.md")
        news <- readLines(news_path)
        news <- c(paste0("# ", pkg$package, " ", DESCRIPTION$Version), 
            "", news)
        writeLines(news, news_path)
    }
    if (uses_git(pkg$path)) {
        message("* Checking into git")
        r <- git2r::init(pkg$path)
        paths <- unlist(git2r::status(r))
        git2r::add(r, paths)
        git2r::commit(r, "Use development version")
    }
    invisible(TRUE)
}


in_dir <- function (new, code) 
{
    .Deprecated(new = "withr::with_dir", package = "devtools")
    withr::with_dir(new = new, code = code)
}


build_win <- function (pkg = ".", version = c("R-release", "R-devel"), args = NULL, 
    quiet = FALSE) 
{
    pkg <- as.package(pkg)
    if (missing(version)) {
        version <- "R-devel"
    }
    else {
        version <- match.arg(version, several.ok = TRUE)
    }
    if (!quiet) {
        message("Building windows version of ", pkg$package, 
            " for ", paste(version, collapse = ", "), " with win-builder.r-project.org.\n")
        if (interactive() && yesno("Email results to ", maintainer(pkg)$email, 
            "?")) {
            return(invisible())
        }
    }
    built_path <- build(pkg, tempdir(), args = args, quiet = quiet)
    on.exit(unlink(built_path))
    url <- paste0("ftp://win-builder.r-project.org/", version, 
        "/", basename(built_path))
    lapply(url, upload_ftp, file = built_path)
    if (!quiet) {
        message("Check ", maintainer(pkg)$email, " for a link to the built package", 
            if (length(version) > 1) 
                "s"
            else "", " in 30-60 mins.")
    }
    invisible()
}


ns_env <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    if (!is_loaded(pkg)) 
        return(NULL)
    asNamespace(pkg$package)
}


revdep_check_print_problems <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    summaries <- readRDS(revdep_check_path(pkg))$results
    problems <- vapply(summaries, function(x) first_problem(x$results), 
        character(1))
    problems <- problems[!is.na(problems)]
    dep_fail <- grepl("checking package dependencies", problems, 
        fixed = TRUE)
    inst_fail <- grepl("checking whether package .+ can be installed", 
        problems)
    pkgs <- names(problems)
    if (any(dep_fail)) {
        bad <- paste(pkgs[dep_fail], collapse = ", ")
        cat("* Failed to install dependencies for: ", bad, "\n", 
            sep = "")
    }
    if (any(inst_fail)) {
        bad <- paste(pkgs[inst_fail], collapse = ", ")
        cat("* Failed to install: ", bad, "\n", sep = "")
    }
    if (length(problems) > 0) {
        other <- problems[!inst_fail & !dep_fail]
        cat(paste0("* ", names(other), ": ", other, "\n"), sep = "")
    }
    else {
        cat("No ERRORs or WARNINGs found :)\n")
    }
}


parse_deps <- function (string) 
{
    if (is.null(string)) 
        return()
    stopifnot(is.character(string), length(string) == 1)
    if (grepl("^\\s*$", string)) 
        return()
    pieces <- strsplit(string, ",")[[1]]
    names <- gsub("\\s*\\(.*?\\)", "", pieces)
    names <- gsub("^\\s+|\\s+$", "", names)
    versions_str <- pieces
    have_version <- grepl("\\(.*\\)", versions_str)
    versions_str[!have_version] <- NA
    compare <- sub(".*\\((\\S+)\\s+.*\\)", "\\1", versions_str)
    versions <- sub(".*\\(\\S+\\s+(.*)\\)", "\\1", versions_str)
    compare_nna <- compare[!is.na(compare)]
    compare_valid <- compare_nna %in% c(">", ">=", "==", "<=", 
        "<")
    if (!all(compare_valid)) {
        stop("Invalid comparison operator in dependency: ", paste(compare_nna[!compare_valid], 
            collapse = ", "))
    }
    deps <- data.frame(name = names, compare = compare, version = versions, 
        stringsAsFactors = FALSE)
    deps[names != "R", ]
}


devtest <- function (package) 
{
    stopifnot(has_tests())
    path <- system.file(package = "devtools", "tests", "testthat", 
        package)
    if (path == "") 
        stop(package, " not found", call. = FALSE)
    path
}


revdep_email <- function (pkg = ".", date, author = getOption("devtools.name"), 
    draft = TRUE, unsent = NULL, template = "revdep/email.md", 
    only_problems = FALSE) 
{
    pkg <- as.package(pkg)
    force(date)
    if (is.null(author)) {
        stop("Please supply `author`", call. = FALSE)
    }
    if (is.null(unsent)) {
        results <- readRDS(revdep_check_path(pkg))$results
    }
    else {
        results <- unsent
    }
    if (only_problems) {
        results <- Filter(has_problems, results)
    }
    if (length(results) == 0) {
        message("No emails to send")
        return(invisible())
    }
    template_path <- file.path(pkg$path, template)
    if (!file.exists(template_path)) {
        stop("`", template, "` does not exist", call. = FALSE)
    }
    template <- readLines(template_path)
    maintainers <- vapply(results, function(x) x$maintainer, 
        character(1))
    orphaned <- grepl("ORPHAN", maintainers)
    if (any(orphaned)) {
        orphans <- paste(names(results)[orphaned], collapse = ", ")
        message("Dropping ", sum(orphaned), " orphaned packages: ", 
            orphans)
        results <- results[!orphaned]
        maintainers <- maintainers[!orphaned]
    }
    gh <- github_info(pkg$path)
    data <- lapply(results, maintainer_data, pkg = pkg, gh = gh, 
        date = date, author = author)
    bodies <- lapply(data, whisker::whisker.render, template = template)
    subjects <- lapply(data, function(x) {
        paste0(x$your_package, " and ", x$my_package, " release")
    })
    emails <- Map(maintainer_email, maintainers, bodies, subjects)
    message("Testing first email")
    send_email(emails[[1]], draft = TRUE)
    if (yesno("Did first draft email look ok?")) 
        return(invisible())
    sent <- vapply(emails, send_email, draft = draft, FUN.VALUE = logical(1))
    if (all(sent)) {
        message("All emails successfully sent")
    }
    else {
        message(sum(!sent), " failed. Call again with unsent = .Last.value")
    }
    results <- results[!sent]
    invisible(results)
}


dev_example <- function (topic) 
{
    path <- find_topic(topic)
    if (is.null(path)) {
        stop("Can't find development example for topic ", topic, 
            call. = FALSE)
    }
    pkg <- as.package(names(path)[[1]])
    load_all(pkg)
    run_example(path)
}


use_rcpp <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    check_suggested("Rcpp")
    message("Adding Rcpp to LinkingTo and Imports")
    add_desc_package(pkg, "LinkingTo", "Rcpp")
    add_desc_package(pkg, "Imports", "Rcpp")
    use_directory("src/", pkg = pkg)
    message("* Ignoring generated binary files.")
    ignore_path <- file.path(pkg$path, "src", ".gitignore")
    union_write(ignore_path, c("*.o", "*.so", "*.dll"))
    message("Next, include the following roxygen tags somewhere in your package:\n\n", 
        "#' @useDynLib ", pkg$package, "\n", "#' @importFrom Rcpp sourceCpp\n", 
        "NULL\n\n", "Then run document()")
}


use_cran_badge <- function (pkg = ".") 
{
    pkg <- as.package(pkg)
    message(" * Add a CRAN status shield by adding the following line to your README:\n", 
        "[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/", 
        pkg$package, ")](https://cran.r-project.org/package=", 
        pkg$package, ")")
    invisible(TRUE)
}


evalq_clean <- function (expr, quiet = TRUE) 
{
    eval_clean(substitute(expr), quiet = quiet)
}


install_github <- function (repo, username = NULL, ref = "master", subdir = NULL, 
    auth_token = github_pat(quiet), host = "https://api.github.com", 
    quiet = FALSE, ...) 
{
    remotes <- lapply(repo, github_remote, username = username, 
        ref = ref, subdir = subdir, auth_token = auth_token, 
        host = host)
    install_remotes(remotes, quiet = quiet, ...)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Tools to Make Developing R Packages Easier"

.skeleton_package_version = "1.12.0"

.skeleton_package_depends = ""

.skeleton_package_imports = "httr,utils,tools,methods,memoise,whisker,digest,rstudioapi,jsonlite,stats,git2r,withr"


## Internal

.skeleton_version = 5


## EOF