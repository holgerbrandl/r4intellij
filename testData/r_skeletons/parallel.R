##
## Exported symobls in package `parallel`
##

## Exported package methods

stopCluster <- function (cl = NULL) 
{
    cl <- defaultCluster(cl)
    if (identical(cl, get("default", envir = .reg))) 
        assign("default", NULL, envir = .reg)
    UseMethod("stopCluster")
}


mcMap <- function (f, ...) 
{
    f <- match.fun(f)
    mcmapply(f, ..., SIMPLIFY = FALSE, mc.silent = TRUE)
}


clusterExport <- function (cl = NULL, varlist, envir = .GlobalEnv) 
{
    for (name in varlist) {
        clusterCall(cl, gets, name, get(name, envir = envir))
    }
}


makePSOCKcluster <- function (names, ...) 
{
    if (is.numeric(names)) {
        names <- as.integer(names[1L])
        if (is.na(names) || names < 1L) 
            stop("numeric 'names' must be >= 1")
        names <- rep("localhost", names)
    }
    .check_ncores(length(names))
    options <- addClusterOptions(defaultClusterOptions, list(...))
    cl <- vector("list", length(names))
    for (i in seq_along(cl)) cl[[i]] <- newPSOCKnode(names[[i]], 
        options = options, rank = i)
    class(cl) <- c("SOCKcluster", "cluster")
    cl
}


parSapplyLB <- function (cl = NULL, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    answer <- parLapplyLB(cl, X = as.list(X), fun = FUN, ...)
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (!identical(simplify, FALSE) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}


clusterApplyLB <- function (cl = NULL, x, fun, ...) 
{
    argfun <- function(i) c(list(x[[i]]), list(...))
    dynamicClusterApply(cl, fun, length(x), argfun)
}


mclapply <- function (X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE, 
    mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L), 
    mc.cleanup = TRUE, mc.allow.recursive = TRUE) 
{
    cores <- as.integer(mc.cores)
    if (is.na(cores) || cores < 1L) 
        stop("'mc.cores' must be >= 1")
    .check_ncores(cores)
    if (isChild() && !isTRUE(mc.allow.recursive)) 
        return(lapply(X = X, FUN = FUN, ...))
    if (mc.set.seed) 
        mc.reset.stream()
    jobs <- list()
    cleanup <- function() {
        if (length(jobs) && mc.cleanup) {
            mccollect(children(jobs), FALSE)
            mckill(children(jobs), if (is.integer(mc.cleanup)) 
                mc.cleanup
            else tools::SIGTERM)
            mccollect(children(jobs))
        }
        if (length(jobs)) {
            mccollect(children(jobs), FALSE)
        }
    }
    on.exit(cleanup())
    if (!is.vector(X) || is.object(X)) 
        X <- as.list(X)
    if (!mc.preschedule) {
        FUN <- match.fun(FUN)
        if (length(X) <= cores) {
            jobs <- lapply(seq_along(X), function(i) mcparallel(FUN(X[[i]], 
                ...), name = names(X)[i], mc.set.seed = mc.set.seed, 
                silent = mc.silent))
            res <- mccollect(jobs)
            if (length(res) == length(X)) 
                names(res) <- names(X)
            has.errors <- sum(sapply(res, inherits, "try-error"))
        }
        else {
            sx <- seq_along(X)
            res <- vector("list", length(sx))
            names(res) <- names(X)
            ent <- rep(FALSE, length(X))
            fin <- rep(FALSE, length(X))
            jobid <- seq_len(cores)
            jobs <- lapply(jobid, function(i) mcparallel(FUN(X[[i]], 
                ...), mc.set.seed = mc.set.seed, silent = mc.silent))
            jobsp <- processID(jobs)
            ent[jobid] <- TRUE
            has.errors <- 0L
            while (!all(fin)) {
                s <- selectChildren(jobs, 0.5)
                if (is.null(s)) 
                  break
                if (is.integer(s)) 
                  for (ch in s) {
                    ji <- which(jobsp == ch)[1]
                    ci <- jobid[ji]
                    r <- readChild(ch)
                    if (is.raw(r)) {
                      child.res <- unserialize(r)
                      if (inherits(child.res, "try-error")) 
                        has.errors <- has.errors + 1L
                      if (!is.null(child.res)) 
                        res[[ci]] <- child.res
                    }
                    else {
                      fin[ci] <- TRUE
                      if (!all(ent)) {
                        nexti <- which(!ent)[1]
                        jobid[ji] <- nexti
                        jobs[[ji]] <- mcparallel(FUN(X[[nexti]], 
                          ...), mc.set.seed = mc.set.seed, silent = mc.silent)
                        jobsp[ji] <- processID(jobs[[ji]])
                        ent[nexti] <- TRUE
                      }
                    }
                  }
            }
        }
        if (has.errors) 
            warning(gettextf("%d function calls resulted in an error", 
                has.errors), domain = NA)
        return(res)
    }
    if (length(X) < cores) 
        cores <- length(X)
    if (cores < 2L) 
        return(lapply(X = X, FUN = FUN, ...))
    sindex <- lapply(seq_len(cores), function(i) seq(i, length(X), 
        by = cores))
    schedule <- lapply(seq_len(cores), function(i) X[seq(i, length(X), 
        by = cores)])
    ch <- list()
    res <- vector("list", length(X))
    names(res) <- names(X)
    cp <- rep(0L, cores)
    fin <- rep(FALSE, cores)
    dr <- rep(FALSE, cores)
    inner.do <- function(core) {
        S <- schedule[[core]]
        f <- mcfork()
        if (isTRUE(mc.set.seed)) 
            mc.advance.stream()
        if (inherits(f, "masterProcess")) {
            on.exit(mcexit(1L, structure("fatal error in wrapper code", 
                class = "try-error")))
            if (isTRUE(mc.set.seed)) 
                mc.set.stream()
            if (isTRUE(mc.silent)) 
                closeStdout(TRUE)
            sendMaster(try(lapply(X = S, FUN = FUN, ...), silent = TRUE))
            mcexit(0L)
        }
        jobs[[core]] <<- ch[[core]] <<- f
        cp[core] <<- f$pid
        NULL
    }
    job.res <- lapply(seq_len(cores), inner.do)
    ac <- cp[cp > 0]
    has.errors <- integer(0)
    while (!all(fin)) {
        s <- selectChildren(ac, 1)
        if (is.null(s)) 
            break
        if (is.integer(s)) 
            for (ch in s) {
                a <- readChild(ch)
                if (is.integer(a)) {
                  core <- which(cp == a)
                  fin[core] <- TRUE
                }
                else if (is.raw(a)) {
                  core <- which(cp == attr(a, "pid"))
                  job.res[[core]] <- ijr <- unserialize(a)
                  if (inherits(ijr, "try-error")) 
                    has.errors <- c(has.errors, core)
                  dr[core] <- TRUE
                }
            }
    }
    for (i in seq_len(cores)) {
        this <- job.res[[i]]
        if (inherits(this, "try-error")) {
            for (j in sindex[[i]]) res[[j]] <- this
        }
        else res[sindex[[i]]] <- this
    }
    if (length(has.errors)) {
        if (length(has.errors) == cores) 
            warning("all scheduled cores encountered errors in user code")
        else warning(sprintf(ngettext(has.errors, "scheduled core %s encountered error in user code, all values of the job will be affected", 
            "scheduled cores %s encountered errors in user code, all values of the jobs will be affected"), 
            paste(has.errors, collapse = ", ")), domain = NA)
    }
    res
}


parSapply <- function (cl = NULL, X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    answer <- parLapply(cl, X = as.list(X), fun = FUN, ...)
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (!identical(simplify, FALSE) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}


clusterSetRNGStream <- function (cl = NULL, iseed = NULL) 
{
    cl <- defaultCluster(cl)
    oldseed <- if (exists(".Random.seed", envir = .GlobalEnv, 
        inherits = FALSE)) 
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    else NULL
    RNGkind("L'Ecuyer-CMRG")
    if (!is.null(iseed)) 
        set.seed(iseed)
    nc <- length(cl)
    seeds <- vector("list", nc)
    seeds[[1L]] <- .Random.seed
    for (i in seq_len(nc - 1L)) seeds[[i + 1L]] <- nextRNGStream(seeds[[i]])
    if (!is.null(oldseed)) 
        assign(".Random.seed", oldseed, envir = .GlobalEnv)
    else rm(.Random.seed, envir = .GlobalEnv)
    for (i in seq_along(cl)) {
        expr <- substitute(assign(".Random.seed", seed, envir = .GlobalEnv), 
            list(seed = seeds[[i]]))
        sendCall(cl[[i]], eval, list(expr))
    }
    checkForRemoteErrors(lapply(cl, recvResult))
    invisible()
}


detectCores <- function (all.tests = FALSE, logical = TRUE) 
{
    systems <- list(linux = if (logical) "grep processor /proc/cpuinfo 2>/dev/null | wc -l" else "cat /proc/cpuinfo | grep 'cpu cores'| uniq | cut -f2 -d:", 
        darwin = if (logical) "/usr/sbin/sysctl -n hw.logicalcpu 2>/dev/null" else "/usr/sbin/sysctl -n hw.physicalcpu 2>/dev/null", 
        solaris = if (logical) "/usr/sbin/psrinfo -v | grep 'Status of.*processor' | wc -l" else "/bin/kstat -p -m cpu_info | grep :core_id | cut -f2 | uniq | wc -l", 
        freebsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null", openbsd = "/sbin/sysctl -n hw.ncpu 2>/dev/null", 
        irix = c("hinv | grep Processors | sed 's: .*::'", "hinv | grep '^Processor '| wc -l"))
    for (i in seq(systems)) if (all.tests || length(grep(paste0("^", 
        names(systems)[i]), R.version$os))) 
        for (cmd in systems[i]) {
            a <- try(suppressWarnings(system(cmd, TRUE)), silent = TRUE)
            if (inherits(a, "try-error")) 
                next
            a <- gsub("^ +", "", a[1])
            if (length(grep("^[1-9]", a))) 
                return(as.integer(a))
        }
    NA_integer_
}


mcparallel <- function (expr, name, mc.set.seed = TRUE, silent = FALSE, mc.affinity = NULL, 
    mc.interactive = FALSE, detached = FALSE) 
{
    f <- mcfork(detached)
    env <- parent.frame()
    if (isTRUE(mc.set.seed)) 
        mc.advance.stream()
    if (inherits(f, "masterProcess")) {
        on.exit(mcexit(1L, structure("fatal error in wrapper code", 
            class = "try-error")))
        if (isTRUE(mc.set.seed)) 
            mc.set.stream()
        mc.interactive <- as.logical(mc.interactive)
        if (isTRUE(mc.interactive)) 
            .Call(C_mc_interactive, TRUE)
        if (isTRUE(!mc.interactive)) 
            .Call(C_mc_interactive, FALSE)
        if (!is.null(mc.affinity)) 
            .Call(C_mc_affinity, mc.affinity)
        if (isTRUE(silent)) 
            closeStdout(TRUE)
        if (detached) {
            on.exit(mcexit(1L))
            eval(expr, env)
            mcexit(0L)
        }
        sendMaster(try(eval(expr, env), silent = TRUE))
        mcexit(0L)
    }
    if (!missing(name) && !is.null(name)) 
        f$name <- as.character(name)[1L]
    class(f) <- c("parallelJob", class(f))
    f
}


mcaffinity <- function (affinity = NULL) 
.Call(C_mc_affinity, affinity)


parApply <- function (cl = NULL, X, MARGIN, FUN, ...) 
{
    cl <- defaultCluster(cl)
    FUN <- match.fun(FUN)
    dl <- length(dim(X))
    if (!dl) 
        stop("dim(X) must have a positive length")
    if (is.object(X)) 
        X <- if (dl == 2L) 
            as.matrix(X)
        else as.array(X)
    d <- dim(X)
    dn <- dimnames(X)
    ds <- seq_len(dl)
    if (is.character(MARGIN)) {
        if (is.null(dnn <- names(dn))) 
            stop("'X' must have named dimnames")
        MARGIN <- match(MARGIN, dnn)
        if (anyNA(MARGIN)) 
            stop("not all elements of 'MARGIN' are names of dimensions")
    }
    s.call <- ds[-MARGIN]
    s.ans <- ds[MARGIN]
    d.call <- d[-MARGIN]
    d.ans <- d[MARGIN]
    dn.call <- dn[-MARGIN]
    dn.ans <- dn[MARGIN]
    d2 <- prod(d.ans)
    if (d2 == 0L) {
        newX <- array(vector(typeof(X), 1L), dim = c(prod(d.call), 
            1L))
        ans <- FUN(if (length(d.call) < 2L) 
            newX[, 1]
        else array(newX[, 1L], d.call, dn.call), ...)
        return(if (is.null(ans)) ans else if (length(d.ans) < 
            2L) ans[1L][-1L] else array(ans, d.ans, dn.ans))
    }
    newX <- aperm(X, c(s.call, s.ans))
    dim(newX) <- c(prod(d.call), d2)
    ans <- vector("list", d2)
    arglist <- if (length(d.call) < 2L) {
        if (length(dn.call)) 
            dimnames(newX) <- c(dn.call, list(NULL))
        lapply(seq_len(d2), function(i) newX[, i])
    }
    else lapply(seq_len(d2), function(i) array(newX[, i], d.call, 
        dn.call))
    ans <- parLapply(cl = cl, X = arglist, fun = FUN, ...)
    ans.list <- is.recursive(ans[[1L]])
    l.ans <- length(ans[[1L]])
    ans.names <- names(ans[[1L]])
    if (!ans.list) 
        ans.list <- any(lengths(ans) != l.ans)
    if (!ans.list && length(ans.names)) {
        all.same <- vapply(ans, function(x) identical(names(x), 
            ans.names), NA)
        if (!all(all.same)) 
            ans.names <- NULL
    }
    len.a <- if (ans.list) 
        d2
    else length(ans <- unlist(ans, recursive = FALSE))
    if (length(MARGIN) == 1L && len.a == d2) {
        names(ans) <- if (length(dn.ans[[1L]])) 
            dn.ans[[1L]]
        return(ans)
    }
    if (len.a == d2) 
        return(array(ans, d.ans, dn.ans))
    if (len.a && len.a%%d2 == 0L) {
        if (is.null(dn.ans)) 
            dn.ans <- vector(mode = "list", length(d.ans))
        dn.ans <- c(list(ans.names), dn.ans)
        return(array(ans, c(len.a%/%d2, d.ans), if (!all(vapply(dn.ans, 
            is.null, NA))) dn.ans))
    }
    return(ans)
}


clusterMap <- function (cl = NULL, fun, ..., MoreArgs = NULL, RECYCLE = TRUE, 
    SIMPLIFY = FALSE, USE.NAMES = TRUE, .scheduling = c("static", 
        "dynamic")) 
{
    cl <- defaultCluster(cl)
    args <- list(...)
    if (length(args) == 0) 
        stop("need at least one argument")
    .scheduling <- match.arg(.scheduling)
    n <- lengths(args)
    if (RECYCLE) {
        vlen <- max(n)
        if (vlen && min(n) == 0L) 
            stop("zero-length inputs cannot be mixed with those of non-zero length")
        if (!all(n == vlen)) 
            for (i in seq_along(args)) args[[i]] <- rep(args[[i]], 
                length.out = vlen)
    }
    else vlen <- min(n)
    argfun <- function(i) c(lapply(args, function(x) x[[i]]), 
        MoreArgs)
    answer <- if (.scheduling == "dynamic") 
        dynamicClusterApply(cl, fun, vlen, argfun)
    else staticClusterApply(cl, fun, vlen, argfun)
    if (USE.NAMES && length(args)) {
        if (is.null(names1 <- names(args[[1L]])) && is.character(args[[1L]])) 
            names(answer) <- args[[1L]]
        else if (!is.null(names1)) 
            names(answer) <- names1
    }
    if (!identical(SIMPLIFY, FALSE) && length(answer)) 
        simplify2array(answer, higher = (SIMPLIFY == "array"))
    else answer
}


makeCluster <- function (spec, type = getClusterOption("type"), ...) 
{
    switch(type, PSOCK = makePSOCKcluster(spec, ...), FORK = makeForkCluster(spec, 
        ...), SOCK = snow::makeSOCKcluster(spec, ...), MPI = snow::makeMPIcluster(spec, 
        ...), NWS = snow::makeNWScluster(spec, ...), stop("unknown cluster type"))
}


clusterApply <- function (cl = NULL, x, fun, ...) 
{
    argfun <- function(i) c(list(x[[i]]), list(...))
    staticClusterApply(cl, fun, length(x), argfun)
}


nextRNGStream <- function (seed) 
{
    if (!is.integer(seed) || seed[1L]%%100L != 7L) 
        stop("invalid value of 'seed'")
    .Call(C_nextStream, seed)
}


setDefaultCluster <- function (cl = NULL) 
{
    if (!is.null(cl)) 
        checkCluster(cl)
    assign("default", cl, envir = .reg)
}


clusterEvalQ <- function (cl = NULL, expr) 
clusterCall(cl, eval, substitute(expr), env = .GlobalEnv)


clusterCall <- function (cl = NULL, fun, ...) 
{
    cl <- defaultCluster(cl)
    for (i in seq_along(cl)) sendCall(cl[[i]], fun, list(...))
    checkForRemoteErrors(lapply(cl, recvResult))
}


mcmapply <- function (FUN, ..., MoreArgs = NULL, SIMPLIFY = TRUE, USE.NAMES = TRUE, 
    mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE, 
    mc.cores = getOption("mc.cores", 2L), mc.cleanup = TRUE) 
{
    FUN <- match.fun(FUN)
    dots <- list(...)
    if (!length(dots)) 
        return(list())
    lens <- sapply(dots, length)
    n <- max(lens)
    if (n && min(lens) == 0L) 
        stop("Zero-length inputs cannot be mixed with those of non-zero length")
    answer <- if (n < 2L) 
        .mapply(FUN, dots, MoreArgs)
    else {
        X <- if (!all(lens == n)) 
            lapply(dots, function(x) rep(x, length.out = n))
        else dots
        do_one <- function(indices, ...) {
            dots <- lapply(X, function(x) x[indices])
            .mapply(FUN, dots, MoreArgs)
        }
        answer <- mclapply(seq_len(n), do_one, mc.preschedule = mc.preschedule, 
            mc.set.seed = mc.set.seed, mc.silent = mc.silent, 
            mc.cores = mc.cores, mc.cleanup = mc.cleanup)
        do.call(c, answer)
    }
    if (USE.NAMES && length(dots)) {
        if (is.null(names1 <- names(dots[[1L]])) && is.character(dots[[1L]])) 
            names(answer) <- dots[[1L]]
        else if (!is.null(names1)) 
            names(answer) <- names1
    }
    if (!identical(SIMPLIFY, FALSE) && length(answer)) 
        simplify2array(answer, higher = (SIMPLIFY == "array"))
    else answer
}


splitIndices <- function (nx, ncl) 
{
    i <- seq_len(nx)
    if (ncl == 0L) 
        list()
    else if (ncl == 1L || nx == 1L) 
        list(i)
    else {
        fuzz <- min((nx - 1L)/1000, 0.4 * nx/ncl)
        breaks <- seq(1 - fuzz, nx + fuzz, length.out = ncl + 
            1L)
        structure(split(i, cut(i, breaks)), names = NULL)
    }
}


pvec <- function (v, FUN, ..., mc.set.seed = TRUE, mc.silent = FALSE, 
    mc.cores = getOption("mc.cores", 2L), mc.cleanup = TRUE) 
{
    if (!is.vector(v)) 
        stop("'v' must be a vector")
    cores <- as.integer(mc.cores)
    if (cores < 1L) 
        stop("'mc.cores' must be >= 1")
    if (cores == 1L) 
        return(FUN(v, ...))
    .check_ncores(cores)
    if (mc.set.seed) 
        mc.reset.stream()
    n <- length(v)
    l <- if (n <= cores) 
        as.list(v)
    else {
        il <- as.integer(n/cores)
        xc <- n - il * cores
        sl <- rep(il, cores)
        if (xc) 
            sl[1:xc] <- il + 1L
        si <- cumsum(c(1L, sl))
        se <- si + c(sl, 0L) - 1L
        lapply(seq_len(cores), function(ix) v[si[ix]:se[ix]])
    }
    jobs <- NULL
    cleanup <- function() {
        if (length(jobs) && mc.cleanup) {
            mccollect(children(jobs), FALSE)
            mckill(children(jobs), if (is.integer(mc.cleanup)) 
                mc.cleanup
            else 15L)
            mccollect(children(jobs))
        }
        if (length(jobs)) {
            mccollect(children(jobs), FALSE)
        }
    }
    on.exit(cleanup())
    FUN <- match.fun(FUN)
    jobs <- lapply(seq_len(min(n, cores)), function(i) mcparallel(FUN(l[[i]], 
        ...), name = i, mc.set.seed = mc.set.seed, silent = mc.silent))
    res <- mccollect(jobs)
    names(res) <- NULL
    res <- do.call(c, res)
    if (length(res) != n) 
        warning("some results may be missing, folded or caused an error")
    res
}


nextRNGSubStream <- function (seed) 
{
    if (!is.integer(seed) || seed[1L]%%100L != 7L) 
        stop("invalid value of 'seed'")
    .Call(C_nextSubStream, seed)
}


parCapply <- function (cl = NULL, x, FUN, ...) 
{
    cl <- defaultCluster(cl)
    do.call(c, clusterApply(cl = cl, x = splitCols(x, length(cl)), 
        fun = apply, MARGIN = 2L, FUN = FUN, ...), quote = TRUE)
}


parRapply <- function (cl = NULL, x, FUN, ...) 
{
    cl <- defaultCluster(cl)
    do.call(c, clusterApply(cl = cl, x = splitRows(x, length(cl)), 
        fun = apply, MARGIN = 1L, FUN = FUN, ...), quote = TRUE)
}


parLapplyLB <- function (cl = NULL, X, fun, ...) 
{
    cl <- defaultCluster(cl)
    do.call(c, clusterApplyLB(cl, x = splitList(X, length(cl)), 
        fun = lapply, fun, ...), quote = TRUE)
}


mc.reset.stream <- function () 
{
    if (RNGkind()[1L] == "L'Ecuyer-CMRG") {
        if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
            sample.int(1L)
        assign("LEcuyer.seed", get(".Random.seed", envir = .GlobalEnv, 
            inherits = FALSE), envir = RNGenv)
    }
}


mccollect <- function (jobs, wait = TRUE, timeout = 0, intermediate = FALSE) 
{
    if (missing(jobs)) 
        jobs <- children()
    if (!length(jobs)) 
        return(NULL)
    if (isTRUE(intermediate)) 
        intermediate <- utils::str
    if (!wait) {
        s <- selectChildren(jobs, timeout)
        if (is.logical(s) || !length(s)) 
            return(NULL)
        lapply(s, function(x) {
            r <- readChild(x)
            if (is.raw(r)) 
                unserialize(r)
            else NULL
        })
    }
    else {
        pids <- if (inherits(jobs, "process") || is.list(jobs)) 
            processID(jobs)
        else jobs
        if (!length(pids)) 
            return(NULL)
        if (!is.numeric(pids)) 
            stop("invalid 'jobs' argument")
        pids <- as.integer(pids)
        pnames <- as.character(pids)
        if (!inherits(jobs, "process") && is.list(jobs)) 
            for (i in seq(jobs)) if (!is.null(jobs[[i]]$name)) 
                pnames[i] <- as.character(jobs[[i]]$name)
        res <- lapply(pids, function(x) NULL)
        names(res) <- pnames
        fin <- rep(FALSE, length(jobs))
        while (!all(fin)) {
            s <- selectChildren(pids, 0.5)
            if (is.integer(s)) {
                for (pid in s) {
                  r <- readChild(pid)
                  if (is.integer(r) || is.null(r)) 
                    fin[pid == pids] <- TRUE
                  if (is.raw(r)) 
                    res[which(pid == pids)] <- list(unserialize(r))
                }
                if (is.function(intermediate)) 
                  intermediate(res)
            }
            else if (all(is.na(match(pids, processID(children()))))) 
                break
        }
        res
    }
}


parLapply <- function (cl = NULL, X, fun, ...) 
{
    cl <- defaultCluster(cl)
    do.call(c, clusterApply(cl, x = splitList(X, length(cl)), 
        fun = lapply, fun, ...), quote = TRUE)
}


clusterSplit <- function (cl = NULL, seq) 
{
    cl <- defaultCluster(cl)
    lapply(splitIndices(length(seq), length(cl)), function(i) seq[i])
}


makeForkCluster <- function (nnodes = getOption("mc.cores", 2L), ...) 
{
    nnodes <- as.integer(nnodes)
    if (is.na(nnodes) || nnodes < 1L) 
        stop("'nnodes' must be >= 1")
    .check_ncores(nnodes)
    cl <- vector("list", nnodes)
    for (i in seq_along(cl)) cl[[i]] <- newForkNode(..., rank = i)
    class(cl) <- c("SOCKcluster", "cluster")
    cl
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Support for Parallel computation in R"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF