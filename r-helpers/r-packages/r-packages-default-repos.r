p <- file.path(Sys.getenv("HOME"), ".R", "repositories")
if (!file.exists(p))
    p <- file.path(R.home("etc"), "repositories")
a <- tools:::.read_repositories(p)
a[,"URL"]
