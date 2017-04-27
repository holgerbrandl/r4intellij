##
## Exported symobls in package `Rcpp`
##

## Exported package methods

exposeClass <- function (class, constructors, fields, methods, file = paste0(CppClass, 
    "Module.cpp"), header = character(), module = paste0("class_", 
    class), CppClass = class, readOnly = character(), rename = character(), 
    Rfile = TRUE) 
{
    if (length(readOnly)) {
        readOnly <- as.character(readOnly)
        if (!all(nzchar(readOnly))) 
            stop("argument readOnly should be a vector of non-empty strings")
    }
    newnames <- allNames(rename)
    if (length(rename)) {
        if (!all(sapply(rename, function(x) is.character(x) && 
            length(x) == 1 && nzchar(x)))) 
            stop("argument rename should be a vector of single, non-empty strings")
        if (!all(nzchar(newnames))) 
            stop("all the elements of argument rename should be non-empty strings")
    }
    if (is.character(file)) {
        if (file.access("src", 3) == 0) 
            cfile <- file.path("src", file)
        else cfile <- file
        con <- file(cfile, "w")
        on.exit({
            message(sprintf("Wrote C++ file \"%s\"", cfile))
            close(con)
        })
    }
    else con <- file
    if (identical(Rfile, FALSE)) {
    }
    else {
        if (identical(Rfile, TRUE)) 
            Rfile <- sprintf("%sClass.R", class)
        if (is.character(Rfile)) {
            if (file.access("R", 3) == 0) 
                Rfile <- file.path("R", Rfile)
            Rcon <- file(Rfile, "w")
            msg <- sprintf("Wrote R file \"%s\"", Rfile)
            on.exit({
                message(msg)
                close(Rcon)
            }, add = TRUE)
        }
        else Rcon <- Rfile
        Rfile <- TRUE
    }
    mfile <- tempfile()
    mcon <- file(mfile, "w")
    writeLines(.stdHeader, con)
    if (length(header)) 
        writeLines(header, con)
    writeLines(c("", sprintf("RCPP_MODULE(%s) {\n", module), 
        ""), mcon)
    writeLines(sprintf("    class_<%s>(\"%s\")\n", CppClass, 
        class), mcon)
    for (cons in constructors) {
        if (length(cons) > 1 || (length(cons) == 1 && nzchar(cons) && 
            !identical(cons, "void"))) 
            cons <- paste0("<", paste(cons, collapse = ","), 
                ">")
        else cons = ""
        writeLines(paste0("    .constructor", cons, "()"), mcon)
    }
    writeLines("", mcon)
    flds <- .specifyItems(fields)
    nm <- names(flds)
    rdOnly <- nm %in% readOnly
    macros <- ifelse(rdOnly, ".field_readonly", ".field")
    test <- nm %in% rename
    if (any(test)) 
        nm[test] <- newnames[match(nm[test], newnames)]
    ns <- NULL
    for (i in seq_along(nm)) {
        typei <- flds[[i]]
        nmi <- fldi <- nm[[i]]
        macroi <- macros[[i]]
        if (!length(typei) || identical(typei, "")) 
            writeLines(sprintf("    %s(\"%s\", &%s::%s)", macroi, 
                nmi, CppClass, fldi), mcon)
        else {
            if (is.null(ns)) {
                ns <- paste("module", class, "NS", sep = "_")
                writeLines(sprintf("\nnamespace %s {\n", ns), 
                  con)
            }
            fldFuns <- .writeFieldFunction(fldi, typei, CppClass, 
                rdOnly[[i]], ns, con)
            if (rdOnly[[i]]) 
                writeLines(sprintf("    .property(\"%s\", &%s, \"read-only field\")", 
                  nmi, fldFuns[[1]]), mcon)
            else writeLines(sprintf("    .property(\"%s\", &%s, &%s)", 
                nmi, fldFuns[[1]], fldFuns[[2]]), mcon)
        }
    }
    writeLines("", mcon)
    sigs <- .specifyItems(methods)
    nm <- mds <- names(sigs)
    test <- nm %in% rename
    if (any(test)) 
        nm[test] <- newnames[match(nm[test], newnames)]
    for (i in seq_along(nm)) {
        sigi <- sigs[[i]]
        nmi <- nm[[i]]
        mdi <- mds[[i]]
        if (!length(sigi) || identical(sigi, "")) 
            writeLines(sprintf("    .method(\"%s\", &%s::%s)", 
                nmi, CppClass, mdi), mcon)
        else {
            if (is.null(ns)) {
                ns <- paste("module", class, "NS", sep = "_")
                writeLines(sprintf("\nnamespace %s {\n", ns), 
                  con)
            }
            mFun <- .writeMethodFunction(mdi, sigi, CppClass, 
                ns, con)
            writeLines(sprintf("    .method(\"%s\", &%s)", nmi, 
                mFun), mcon)
        }
    }
    writeLines("    ;\n}", mcon)
    close(mcon)
    if (!is.null(ns)) 
        writeLines(sprintf("} // %s", ns), con)
    writeLines(readLines(mfile), con)
    if (Rfile) {
        if (missing(CppClass)) 
            CppString <- ""
        else CppString <- paste(",", dQuote(CppClass))
        if (missing(module)) 
            ModString <- ""
        else ModString <- paste(", module =", dQuote(module))
        writeLines(sprintf("%s <- setRcppClass(\"%s\"%s%s)", 
            class, class, CppString, ModString), Rcon)
    }
}


setRcppClass <- function (Class, CppClass, module, fields = list(), contains = character(), 
    methods = list(), saveAs = Class, where = topenv(parent.frame()), 
    ...) 
{
    myCall <- match.call()
    myCall[[1]] <- quote(Rcpp::loadRcppClass)
    if (!missing(module) && moduleIsLoaded(module, where)) 
        eval.parent(myCall)
    else {
        f <- function(NS) NULL
        myCall$where = as.name("NS")
        body(f, where) <- myCall
        setLoadAction(f, where = where)
    }
}


prompt <- utils::prompt # re-exported from utils package

`.__C__C++Field` <- new("refClassRepresentation"
    , fieldClasses = structure(list(pointer = "externalptr", cpp_class = "character", 
    read_only = "logical", class_pointer = "externalptr", docstring = "character"), .Names = c("pointer", 
"cpp_class", "read_only", "class_pointer", "docstring"))
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
    , className = structure("C++Field", package = "Rcpp")
    , package = "Rcpp"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

populate <- function (module, env) 
{
    module <- Module(module, mustStart = TRUE)
    storage <- get("storage", as.environment(module))
    symbols <- ls(storage)
    is_ns <- isNamespace(env)
    for (x in symbols) {
        forceAssignInNamespace(x, storage[[x]], env)
    }
}


initialize <- methods::initialize # re-exported from methods package

cppFunction <- function (code, depends = character(), plugins = character(), 
    includes = character(), env = parent.frame(), rebuild = FALSE, 
    cacheDir = getOption("rcpp.cache.dir", tempdir()), showOutput = verbose, 
    verbose = getOption("verbose")) 
{
    if (!is.null(depends) && length(depends) > 0) {
        depends <- paste(depends, sep = ", ")
        scaffolding <- paste("// [[Rcpp::depends(", depends, 
            ")]]", sep = "")
        scaffolding <- c(scaffolding, "", .linkingToIncludes(depends, 
            FALSE), recursive = TRUE)
    }
    else {
        scaffolding <- "#include <Rcpp.h>"
    }
    if (!is.null(plugins) && length(plugins) > 0) {
        plugins <- paste(plugins, sep = ", ")
        pluginsAttrib <- paste("// [[Rcpp::plugins(", plugins, 
            ")]]", sep = "")
        scaffolding <- c(scaffolding, pluginsAttrib)
        for (pluginName in plugins) {
            plugin <- .findPlugin(pluginName)
            settings <- plugin()
            scaffolding <- c(scaffolding, settings$includes, 
                recursive = TRUE)
        }
    }
    scaffolding <- c(scaffolding, "", "using namespace Rcpp;", 
        "", includes, "// [[Rcpp::export]]", recursive = T)
    code <- paste(c(scaffolding, code, recursive = T), collapse = "\n")
    if (verbose) {
        cat("\nGenerated code for function definition:", "\n--------------------------------------------------------\n\n")
        cat(code)
        cat("\n")
    }
    if (is.null(env)) 
        env <- new.env()
    exported <- sourceCpp(code = code, env = env, rebuild = rebuild, 
        cacheDir = cacheDir, showOutput = showOutput, verbose = verbose)
    if (length(exported$functions) == 0) 
        stop("No function definition found")
    else if (length(exported$functions) > 1) 
        stop("More than one function definition")
    else {
        functionName <- exported$functions[[1]]
        invisible(get(functionName, env))
    }
}


sizeof <- function (type = "int", ...) 
{
    code <- sprintf("\n        SEXP manipulate_this_type(){\n            typedef %s type ;\n            return wrap( %s(type) ) ;\n        }", 
        type, what)
    dots <- list(code, ...)
    dots[["env"]] <- environment()
    manipulate_this_type <- do.call(cppFunction, dots)
    res <- manipulate_this_type()
    if (!is.null(class)) {
        class(res) <- class
    }
    res
}


`.__C__C++Function` <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    pointer = structure("externalptr", package = "methods"), 
    docstring = structure("character", package = "methods"), 
    signature = structure("character", package = "methods")), .Names = c(".Data", 
"pointer", "docstring", "signature"))
    , contains = structure(list(`function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object()), .Names = c("function", 
"OptionalFunction", "PossibleMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("C++Function", package = "Rcpp")
    , package = "Rcpp"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


evalCpp <- function (code, depends = character(), plugins = character(), 
    includes = character(), rebuild = FALSE, cacheDir = getOption("rcpp.cache.dir", 
        tempdir()), showOutput = verbose, verbose = getOption("verbose")) 
{
    code <- sprintf("SEXP get_value(){ return wrap( %s ) ; }", 
        code)
    env <- new.env()
    cppFunction(code, depends = depends, plugins = plugins, includes = includes, 
        env = env, rebuild = rebuild, cacheDir = cacheDir, showOutput = showOutput, 
        verbose = verbose)
    fun <- env[["get_value"]]
    fun()
}


`.__C__C++Constructor` <- new("refClassRepresentation"
    , fieldClasses = structure(list(pointer = "externalptr", class_pointer = "externalptr", 
    nargs = "integer", signature = "character", docstring = "character"), .Names = c("pointer", 
"class_pointer", "nargs", "signature", "docstring"))
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
    , className = structure("C++Constructor", package = "Rcpp")
    , package = "Rcpp"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__prompt:utils` <- "<environment>"

`.__C__C++OverloadedMethods` <- new("refClassRepresentation"
    , fieldClasses = structure(list(pointer = "externalptr", class_pointer = "externalptr", 
    size = "integer", void = "logical", const = "logical", docstrings = "character", 
    signatures = "character", nargs = "integer"), .Names = c("pointer", 
"class_pointer", "size", "void", "const", "docstrings", "signatures", 
"nargs"))
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
    , className = structure("C++OverloadedMethods", package = "Rcpp")
    , package = "Rcpp"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


LdFlags <- function () 
{
    cat(RcppLdFlags())
}


`.__T__show:methods` <- "<environment>"

loadRcppClass <- function (Class, CppClass = Class, module = paste0("class_", 
    Class), fields = character(), contains = character(), methods = list(), 
    saveAs = Class, where = topenv(parent.frame()), ...) 
{
    if (isBotchedSession()) {
        value <- setRefClass(Class, fields = fields, methods = methods, 
            contains = contains, where = where, ...)
        if (is.character(saveAs) && length(saveAs) == 1) 
            assign(saveAs, value, envir = where)
        return(value)
    }
    mod <- loadModule(module, NULL, env = where, loadNow = TRUE)
    storage <- get("storage", envir = as.environment(mod))
    if (exists(CppClass, envir = storage, inherits = FALSE)) {
        cppclassinfo <- get(CppClass, envir = storage)
        if (!is(cppclassinfo, "C++Class")) 
            stop(gettextf("Object \"%s\" in module \"%s\" is not a C++ class description", 
                CppClass, module))
    }
    else stop(gettextf("No object \"%s\" in module \"%s\"", CppClass, 
        module))
    allmethods <- .makeCppMethods(methods, cppclassinfo, where)
    allfields <- .makeCppFields(fields, cppclassinfo, where)
    value <- setRefClass(Class, fields = allfields, contains = c(contains, 
        "RcppClass"), methods = allmethods, where = where, ...)
    if (exists("globalVariables", envir = asNamespace("utils"))) 
        utils::globalVariables(c(names(allfields), names(allmethods)), 
            where)
    if (is.character(saveAs) && length(saveAs) == 1) 
        assign(saveAs, value, envir = where)
    value
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

loadModule <- function (module, what = character(), loadNow, env = topenv(parent.frame())) 
{
    if (is(module, "character")) {
        loadM <- NULL
        metaName <- .moduleMetaName(module)
        if (exists(metaName, envir = env, inherits = FALSE)) 
            loadM <- get(metaName, envir = env)
    }
    else if (is(module, "Module")) {
        loadM <- as.environment(module)
        module <- get(loadM, "moduleName")
    }
    else stop(gettextf("Argument \"module\" should be a module or the name of a module: got an object of class \"%s\"", 
        class(module)))
    if (missing(loadNow)) {
        if (is.null(loadM)) 
            loadM <- tryCatch(Module(module, mustStart = TRUE, 
                where = env), error = function(e) e)
        loadNow <- !is(loadM, "error")
    }
    if (loadNow) {
        .botched <- FALSE
        if (is.null(loadM)) 
            loadM <- tryCatch(Module(module, mustStart = TRUE, 
                where = env), error = function(e) e)
        if (is(loadM, "error")) {
            if (.botched) 
                return(.DummyModule(module, what))
            stop(gettextf("Unable to load module \"%s\": %s", 
                as(module, "character"), loadM$message))
        }
        if (!exists(metaName, envir = env, inherits = FALSE)) 
            assign(metaName, loadM, envir = env)
        if (!length(what)) 
            return(loadM)
        env <- as.environment(env)
        storage <- as.environment(get("storage", as.environment(loadM)))
        if (identical(what, TRUE)) 
            what <- objects(storage)
        missingObjs <- !sapply(what, function(symb) exists(symb, 
            envir = storage, inherits = FALSE))
        if (any(missingObjs)) {
            if (.botched) {
                for (el in what[missingObjs]) assign(el, NULL, 
                  envir = storage)
            }
            else {
                warning(gettextf("%s not found in module \"%s\"", 
                  paste0("\"", what[missingObjs], "\"", collapse = ", "), 
                  as.character(module)))
                what <- what[!missingObjs]
            }
        }
        assignAs <- .moduleNames(what)
        for (i in seq_along(what)) {
            if (.botched) 
                assign(assignAs[[i]], NULL, envir = storage)
            else assign(assignAs[[i]], get(what[[i]], envir = storage), 
                envir = env)
        }
        loadM
    }
    else {
        myCall <- match.call()
        f <- function(ns) NULL
        myCall$env <- as.name("ns")
        myCall$loadNow <- TRUE
        body(f, envir = env) <- myCall
        setLoadAction(f, where = env)
        invisible(myCall)
    }
}


cpp_object_initializer <- function (.self, .refClassDef, ..., .object_pointer) 
{
    selfEnv <- as.environment(.self)
    fields <- .refClassDef@fieldPrototypes
    pointer <- if (missing(.object_pointer)) 
        new_CppObject_xp(fields$.module, fields$.pointer, ...)
    else .object_pointer
    assign(".module", fields$.module, envir = selfEnv)
    assign(".pointer", pointer, envir = selfEnv)
    assign(".cppclass", fields$.pointer, envir = selfEnv)
    .self
}


`.__T__initialize:methods` <- "<environment>"

`.__T__.DollarNames:utils` <- "<environment>"

compileAttributes <- function (pkgdir = ".", verbose = getOption("verbose")) 
{
    pkgdir <- normalizePath(pkgdir, winslash = "/")
    descFile <- file.path(pkgdir, "DESCRIPTION")
    if (!file.exists(descFile)) 
        stop("pkgdir must refer to the directory containing an R package")
    pkgDesc <- read.dcf(descFile)[1, ]
    pkgname = .readPkgDescField(pkgDesc, "Package")
    depends <- c(.readPkgDescField(pkgDesc, "Depends", character()), 
        .readPkgDescField(pkgDesc, "Imports", character()), .readPkgDescField(pkgDesc, 
            "LinkingTo", character()))
    depends <- unique(.splitDepends(depends))
    depends <- depends[depends != "R"]
    srcDir <- file.path(pkgdir, "src")
    if (!file.exists(srcDir)) 
        return(FALSE)
    rDir <- file.path(pkgdir, "R")
    if (!file.exists(rDir)) 
        dir.create(rDir)
    cppFiles <- list.files(srcDir, pattern = "\\.((c(c|pp))|(h(pp)?))$")
    cppFileBasenames <- tools::file_path_sans_ext(cppFiles)
    cppFiles <- file.path(srcDir, cppFiles)
    cppFiles <- normalizePath(cppFiles, winslash = "/")
    linkingTo <- .readPkgDescField(pkgDesc, "LinkingTo")
    includes <- .linkingToIncludes(linkingTo, TRUE)
    typesHeader <- c(paste0(pkgname, "_types.h"), paste0(pkgname, 
        "_types.hpp"))
    pkgHeader <- c(paste0(pkgname, ".h"), typesHeader)
    pkgHeaderPath <- file.path(pkgdir, "inst", "include", pkgHeader)
    pkgHeader <- pkgHeader[file.exists(pkgHeaderPath)]
    if (length(pkgHeader) > 0) {
        pkgInclude <- paste("#include \"../inst/include/", pkgHeader, 
            "\"", sep = "")
        includes <- c(pkgInclude, includes)
    }
    pkgHeader <- typesHeader
    pkgHeaderPath <- file.path(pkgdir, "src", pkgHeader)
    pkgHeader <- pkgHeader[file.exists(pkgHeaderPath)]
    if (length(pkgHeader) > 0) 
        includes <- c(paste0("#include \"", pkgHeader, "\""), 
            includes)
    invisible(.Call("compileAttributes", PACKAGE = "Rcpp", pkgdir, 
        pkgname, depends, cppFiles, cppFileBasenames, includes, 
        verbose, .Platform))
}


`.__C__C++Object` <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = structure("C++Object", package = "Rcpp")
    , package = "Rcpp"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

sourceCpp <- function (file = "", code = NULL, env = globalenv(), embeddedR = TRUE, 
    rebuild = FALSE, cacheDir = getOption("rcpp.cache.dir", tempdir()), 
    cleanupCacheDir = FALSE, showOutput = verbose, verbose = getOption("verbose"), 
    dryRun = FALSE) 
{
    cacheDir <- path.expand(cacheDir)
    cacheDir <- .sourceCppPlatformCacheDir(cacheDir)
    cacheDir <- normalizePath(cacheDir)
    if (!missing(code)) {
        rWorkingDir <- getwd()
        file <- tempfile(fileext = ".cpp", tmpdir = cacheDir)
        con <- file(file, open = "w")
        writeLines(code, con)
        close(con)
    }
    else {
        rWorkingDir <- normalizePath(dirname(file))
    }
    file <- normalizePath(file, winslash = "/")
    if (!tools::file_ext(file) %in% c("cc", "cpp")) {
        stop("The filename '", basename(file), "' does not have an ", 
            "extension of .cc or .cpp so cannot be compiled.")
    }
    if (.Platform$OS.type == "windows") {
        if (grepl(" ", basename(file), fixed = TRUE)) {
            stop("The filename '", basename(file), "' contains spaces. This ", 
                "is not permitted.")
        }
    }
    context <- .Call("sourceCppContext", PACKAGE = "Rcpp", file, 
        code, rebuild, cacheDir, .Platform)
    if (context$buildRequired || rebuild) {
        if (verbose) 
            .printVerboseOutput(context)
        succeeded <- FALSE
        output <- NULL
        depends <- .getSourceCppDependencies(context$depends, 
            file)
        .validatePackages(depends, context$cppSourceFilename)
        envRestore <- .setupBuildEnvironment(depends, context$plugins, 
            file)
        cwd <- getwd()
        setwd(context$buildDirectory)
        fromCode <- !missing(code)
        if (!.callBuildHook(context$cppSourcePath, fromCode, 
            showOutput)) {
            .restoreEnvironment(envRestore)
            setwd(cwd)
            return(invisible(NULL))
        }
        on.exit({
            if (!succeeded) .showBuildFailureDiagnostics()
            .callBuildCompleteHook(succeeded, output)
            setwd(cwd)
            .restoreEnvironment(envRestore)
        })
        if (file.exists(context$previousDynlibPath)) {
            try(silent = TRUE, dyn.unload(context$previousDynlibPath))
            file.remove(context$previousDynlibPath)
        }
        cmd <- paste(R.home(component = "bin"), .Platform$file.sep, 
            "R ", "CMD SHLIB ", "-o ", shQuote(context$dynlibFilename), 
            " ", ifelse(rebuild, "--preclean ", ""), ifelse(dryRun, 
                "--dry-run ", ""), paste(shQuote(context$cppDependencySourcePaths), 
                collapse = " "), " ", shQuote(context$cppSourceFilename), 
            " ", sep = "")
        if (showOutput) 
            cat(cmd, "\n")
        result <- suppressWarnings(system(cmd, intern = !showOutput))
        if (!showOutput) {
            output <- result
            attributes(output) <- NULL
            status <- attr(result, "status")
            if (!is.null(status)) {
                cat(result, sep = "\n")
                succeeded <- FALSE
                stop("Error ", status, " occurred building shared library.")
            }
            else if (!file.exists(context$dynlibFilename)) {
                cat(result, sep = "\n")
                succeeded <- FALSE
                stop("Error occurred building shared library.")
            }
            else {
                succeeded <- TRUE
            }
        }
        else if (!identical(as.character(result), "0")) {
            succeeded <- FALSE
            stop("Error ", result, " occurred building shared library.")
        }
        else {
            succeeded <- TRUE
        }
    }
    else {
        cwd <- getwd()
        on.exit({
            setwd(cwd)
        })
        if (verbose) 
            cat("\nNo rebuild required (use rebuild = TRUE to ", 
                "force a rebuild)\n\n", sep = "")
    }
    if (dryRun) 
        return(invisible(NULL))
    if (length(context$exportedFunctions) > 0 || length(context$modules) > 
        0) {
        exports <- c(context$exportedFunctions, context$modules)
        removeObjs <- exports[exports %in% ls(envir = env, all.names = T)]
        remove(list = removeObjs, envir = env)
        scriptPath <- file.path(context$buildDirectory, context$rSourceFilename)
        source(scriptPath, local = env)
    }
    else if (getOption("rcpp.warnNoExports", default = TRUE)) {
        warning("No Rcpp::export attributes or RCPP_MODULE declarations ", 
            "found in source")
    }
    if (embeddedR && (length(context$embeddedR) > 0)) {
        srcConn <- textConnection(context$embeddedR)
        setwd(rWorkingDir)
        source(file = srcConn, echo = TRUE)
    }
    if (cleanupCacheDir) 
        cleanupSourceCppCache(cacheDir, context$cppSourcePath, 
            context$buildDirectory)
    invisible(list(functions = context$exportedFunctions, modules = context$modules, 
        cppSourcePath = context$cppSourcePath, buildDirectory = context$buildDirectory))
}


cpp_object_dummy <- function (.self, .refClassDef) 
{
    selfEnv <- as.environment(.self)
    fields <- .refClassDef@fieldPrototypes
    pointer <- new_dummyObject()
    assign(".module", fields$.module, envir = selfEnv)
    assign(".pointer", pointer, envir = selfEnv)
    assign(".cppclass", fields$.pointer, envir = selfEnv)
    .self
}


Rcpp.plugin.maker <- function (include.before = "", include.after = "", LinkingTo = unique(c(package, 
    "Rcpp")), Depends = unique(c(package, "Rcpp")), libs = "", 
    Makevars = NULL, Makevars.win = NULL, package = "Rcpp") 
{
    function(...) {
        includes <- sprintf("%s\n#include <Rcpp.h>\n%s\n\n#ifndef BEGIN_RCPP\n#define BEGIN_RCPP\n#endif\n\n#ifndef END_RCPP\n#define END_RCPP\n#endif\n\nusing namespace Rcpp;\n", 
            include.before, include.after)
        out <- list(env = list(PKG_LIBS = libs), includes = includes, 
            LinkingTo = LinkingTo, body = function(x) {
                sprintf("BEGIN_RCPP\n%s\nEND_RCPP", x)
            }, Depends = Depends)
        if (!is.null(Makevars)) 
            out$Makevars <- Makevars
        if (!is.null(Makevars.win)) 
            out$Makevars.win <- Makevars.win
        out
    }
}


RcppLdFlags <- function () 
{
    ""
}


.__C__RcppClass <- new("refClassRepresentation"
    , fieldClasses = structure(list(.CppObject = "C++Object"), .Names = ".CppObject")
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
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("RcppClass", package = "Rcpp")
    , package = "Rcpp"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

Module <- function (module, PACKAGE = methods::getPackageName(where), where = topenv(parent.frame()), 
    mustStart = FALSE) 
{
    if (inherits(module, "DLLInfo") && missing(mustStart)) 
        mustStart <- TRUE
    if (inherits(module, "Module")) {
        xp <- .getModulePointer(module, FALSE)
        if (!missing(PACKAGE)) 
            warning("ignoring PACKAGE argument in favor of internal package from Module object")
        env <- as.environment(module)
        PACKAGE <- get("packageName", envir = env)
        moduleName <- get("moduleName", envir = env)
    }
    else if (identical(typeof(module), "externalptr")) {
        xp <- module
        moduleName <- .Call(Module__name, xp)
        module <- methods::new("Module", pointer = xp, packageName = PACKAGE, 
            moduleName = moduleName)
    }
    else if (is.character(module)) {
        moduleName <- module
        xp <- .badModulePointer
        module <- methods::new("Module", pointer = xp, packageName = PACKAGE, 
            moduleName = moduleName)
    }
    if (identical(xp, .badModulePointer)) {
        if (mustStart) {
            name <- sprintf("_rcpp_module_boot_%s", moduleName)
            symbol <- tryCatch(getNativeSymbolInfo(name, PACKAGE), 
                error = function(e) e)
            if (inherits(symbol, "error")) 
                stop(gettextf("Failed to initialize module pointer: %s", 
                  symbol), domain = NA)
            xp <- .Call(symbol)
            .setModulePointer(module, xp)
        }
        else return(module)
    }
    classes <- .Call(Module__classes_info, xp)
    if (environmentIsLocked(where)) 
        where <- .GlobalEnv
    generators <- list()
    storage <- new.env()
    for (i in seq_along(classes)) {
        CLASS <- classes[[i]]
        clname <- CLASS@.Data
        fields <- cpp_fields(CLASS, where)
        methods <- cpp_refMethods(CLASS, where)
        generator <- methods::setRefClass(clname, fields = fields, 
            contains = "C++Object", methods = methods, where = where)
        .self <- .refClassDef <- NULL
        generator$methods(initialize = if (cpp_hasDefaultConstructor(CLASS)) 
            function(...) cpp_object_initializer(.self, .refClassDef, 
                ...)
        else function(...) {
            if (nargs()) 
                cpp_object_initializer(.self, .refClassDef, ...)
            else cpp_object_dummy(.self, .refClassDef)
        })
        rm(.self, .refClassDef)
        classDef <- methods::getClass(clname)
        fields <- classDef@fieldPrototypes
        assign(".pointer", CLASS@pointer, envir = fields)
        assign(".module", xp, envir = fields)
        assign(".CppClassName", clname, envir = fields)
        generators[[clname]] <- generator
        if (any(grepl("^[[]", names(CLASS@methods)))) {
            if ("[[" %in% names(CLASS@methods)) {
                methods::setMethod("[[", clname, function(x, 
                  i, j, ..., exact = TRUE) {
                  x$`[[`(i)
                }, where = where)
            }
            if ("[[<-" %in% names(CLASS@methods)) {
                methods::setReplaceMethod("[[", clname, function(x, 
                  i, j, ..., exact = TRUE, value) {
                  x$`[[<-`(i, value)
                  x
                }, where = where)
            }
        }
        if (any(grepl("show", names(CLASS@methods)))) {
            setMethod("show", clname, function(object) object$show(), 
                where = where)
        }
    }
    if (length(classes)) {
        module$refClassGenerators <- generators
    }
    for (i in seq_along(classes)) {
        CLASS <- classes[[i]]
        clname <- CLASS@.Data
        demangled_name <- sub("^Rcpp_", "", clname)
        .classes_map[[CLASS@typeid]] <- storage[[demangled_name]] <- .get_Module_Class(module, 
            demangled_name, xp)
        if (length(CLASS@enums)) {
            for (enum in CLASS@enums) {
                for (i in 1:length(enum)) {
                  storage[[paste(demangled_name, ".", names(enum)[i], 
                    sep = "")]] <- enum[i]
                }
            }
        }
    }
    functions <- .Call(Module__functions_names, xp)
    for (fun in functions) {
        storage[[fun]] <- .get_Module_function(module, fun, xp)
        converter_rx <- "^[.]___converter___(.*)___(.*)$"
        if (length(matches <- grep(converter_rx, functions))) {
            for (i in matches) {
                fun <- functions[i]
                from <- sub(converter_rx, "\\1", fun)
                to <- sub(converter_rx, "\\2", fun)
                converter <- function(from) {
                }
                body(converter) <- substitute({
                  CONVERT(from)
                }, list(CONVERT = storage[[fun]]))
                setAs(from, to, converter, where = where)
            }
        }
    }
    assign("storage", storage, envir = as.environment(module))
    module
}


registerPlugin <- function (name, plugin) 
{
    .plugins[[name]] <- plugin
}


.DollarNames <- utils::.DollarNames # re-exported from utils package

`.__C__C++Class` <- new("classRepresentation"
    , slots = structure(list(.Data = structure("character", package = "methods"), 
    pointer = structure("externalptr", package = "methods"), 
    module = structure("externalptr", package = "methods"), fields = structure("list", package = "methods"), 
    methods = structure("list", package = "methods"), constructors = structure("list", package = "methods"), 
    generator = structure("refGenerator", package = "Rcpp"), 
    docstring = structure("character", package = "methods"), 
    typeid = structure("character", package = "methods"), enums = structure("list", package = "methods"), 
    parents = structure("character", package = "methods")), .Names = c(".Data", 
"pointer", "module", "fields", "methods", "constructors", "generator", 
"docstring", "typeid", "enums", "parents"))
    , contains = structure(list(character = S4_object(), 
    vector = S4_object(), 
    data.frameRowLabels = S4_object(), 
    SuperClassMethod = S4_object()), .Names = c("character", 
"vector", "data.frameRowLabels", "SuperClassMethod"))
    , virtual = FALSE
    , prototype = new("character"
)
    , validity = NULL
    , access = list()
    , className = structure("C++Class", package = "Rcpp")
    , package = "Rcpp"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


loadRcppModules <- function (direct = TRUE) 
{
    .Deprecated("loadModule")
    calls <- sys.calls()
    w <- which(sapply(calls, function(call) {
        identical(call[[1L]], as.name("runHook"))
    }))
    if (!length(w)) 
        stop("loadRcppModules can only be used within a .onLoad function")
    w <- w[length(w)]
    call <- calls[[w]]
    if (!identical(call[[2L]], ".onLoad")) 
        stop("loadRcppModules can only be used within a .onLoad function")
    f <- sys.frame(w)
    ns <- get("env", f)
    if (!isNamespace(ns)) 
        stop("loadRcppModules not called from a namespace")
    pkg <- get("pkgname", f)
    lib <- get("libname", f)
    description <- packageDescription(pkg, lib.loc = lib)
    modules <- description[["RcppModules"]]
    if (!is.null(modules)) {
        modules <- strsplit(modules, "[[:space:]]*,[[:space:]]*")[[1L]]
        for (m in modules) {
            tryCatch({
                mod <- Module(m, pkg, mustStart = TRUE)
                if (isTRUE(direct)) {
                  populate(mod, ns)
                }
                else {
                  forceAssignInNamespace(m, mod, ns)
                }
                assign(.moduleMetaName(m), mod, envir = ns)
            }, error = function(e) {
                stop(sprintf("failed to load module %s from package %s\n%s", 
                  m, pkg, conditionMessage(e)))
            })
        }
    }
}


`.__T__$:base` <- "<environment>"

`.__T__formals<-:base` <- "<environment>"

`formals<-` <- function (fun, envir = environment(fun), value) 
standardGeneric("formals<-")


demangle <- function (type = "int", ...) 
{
    code <- sprintf("\n        SEXP manipulate_this_type(){\n            typedef %s type ;\n            return wrap( %s(type) ) ;\n        }", 
        type, what)
    dots <- list(code, ...)
    dots[["env"]] <- environment()
    manipulate_this_type <- do.call(cppFunction, dots)
    res <- manipulate_this_type()
    if (!is.null(class)) {
        class(res) <- class
    }
    res
}


.__C__Module <- new("classRepresentation"
    , slots = structure(list(.xData = structure("environment", package = "methods")), .Names = ".xData")
    , contains = structure(list(.environment = S4_object(), 
    environment = S4_object(), 
    refObject = S4_object()), .Names = c(".environment", 
"environment", "refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("Module", package = "Rcpp")
    , package = "Rcpp"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Rcpp.package.skeleton <- function (name = "anRpackage", list = character(), environment = .GlobalEnv, 
    path = ".", force = FALSE, code_files = character(), cpp_files = character(), 
    example_code = TRUE, attributes = TRUE, module = FALSE, author = "Your Name", 
    maintainer = if (missing(author)) "Your Name" else author, 
    email = "your@email.com", license = "GPL (>= 2)") 
{
    havePkgKitten <- requireNamespace("pkgKitten", quietly = TRUE)
    call <- match.call()
    call[[1]] <- as.name("package.skeleton")
    env <- parent.frame(1)
    if (!is.character(cpp_files)) 
        stop("'cpp_files' must be a character vector")
    if (!length(list)) {
        fake <- TRUE
        assign("Rcpp.fake.fun", function() {
        }, envir = env)
        if (example_code && !isTRUE(attributes)) {
            assign("rcpp_hello_world", function() {
            }, envir = env)
            remove_hello_world <- TRUE
        }
        else {
            remove_hello_world <- FALSE
        }
    }
    else {
        if (example_code && !isTRUE(attributes)) {
            if (!"rcpp_hello_world" %in% list) {
                assign("rcpp_hello_world", function() {
                }, envir = env)
                call[["list"]] <- as.call(c(as.name("c"), as.list(c("rcpp_hello_world", 
                  list))))
            }
            remove_hello_world <- TRUE
        }
        else {
            remove_hello_world <- FALSE
        }
        fake <- FALSE
    }
    call <- call[c(1L, which(names(call) %in% names(formals(package.skeleton))))]
    if (fake) {
        call[["list"]] <- c(if (isTRUE(example_code) && !isTRUE(attributes)) "rcpp_hello_world", 
            "Rcpp.fake.fun")
    }
    tryCatch(eval(call, envir = env), error = function(e) {
        stop(sprintf("error while calling `package.skeleton` : %s", 
            conditionMessage(e)))
    })
    message("\nAdding Rcpp settings")
    root <- file.path(path, name)
    DESCRIPTION <- file.path(root, "DESCRIPTION")
    if (file.exists(DESCRIPTION)) {
        imports <- c(if (isTRUE(module)) "methods", sprintf("Rcpp (>= %s)", 
            packageDescription("Rcpp")[["Version"]]))
        x <- cbind(read.dcf(DESCRIPTION), Imports = paste(imports, 
            collapse = ", "), LinkingTo = "Rcpp")
        x[, "Author"] <- author
        x[, "Maintainer"] <- sprintf("%s <%s>", maintainer, email)
        x[, "License"] <- license
        x[, "Title"] <- "What the Package Does in One 'Title Case' Line"
        x[, "Description"] <- "One paragraph description of what the package does as one or more full sentences."
        message(" >> added Imports: Rcpp")
        message(" >> added LinkingTo: Rcpp")
        write.dcf(x, file = DESCRIPTION)
    }
    NAMESPACE <- file.path(root, "NAMESPACE")
    lines <- readLines(NAMESPACE)
    ns <- file(NAMESPACE, open = "w")
    if (!grepl("useDynLib", lines)) {
        lines <- c(sprintf("useDynLib(%s)", name), lines)
        writeLines(lines, con = ns)
        message(" >> added useDynLib directive to NAMESPACE")
    }
    if (isTRUE(module)) {
        writeLines("import(methods, Rcpp)", ns)
        message(" >> added import(methods, Rcpp) directive to NAMESPACE")
    }
    else {
        writeLines("importFrom(Rcpp, evalCpp)", ns)
        message(" >> added importFrom(Rcpp, evalCpp) directive to NAMESPACE")
    }
    close(ns)
    if (havePkgKitten) {
        pkgKitten::playWithPerPackageHelpPage(name, path, maintainer, 
            email)
    }
    else {
        .playWithPerPackageHelpPage(name, path, maintainer, email)
    }
    src <- file.path(root, "src")
    if (!file.exists(src)) {
        dir.create(src)
    }
    skeleton <- system.file("skeleton", package = "Rcpp")
    if (length(cpp_files) > 0L) {
        for (file in cpp_files) {
            file.copy(file, src)
            message(" >> copied ", file, " to src directory")
        }
        compileAttributes(root)
    }
    if (example_code) {
        if (isTRUE(attributes)) {
            file.copy(file.path(skeleton, "rcpp_hello_world_attributes.cpp"), 
                file.path(src, "rcpp_hello_world.cpp"))
            message(" >> added example src file using Rcpp attributes")
            compileAttributes(root)
            message(" >> compiled Rcpp attributes")
            message(" >> do NOT modify by hand either RcppExports.cpp or ", 
                "RcppExports.R")
        }
        else {
            header <- readLines(file.path(skeleton, "rcpp_hello_world.h"))
            header <- gsub("@PKG@", name, header, fixed = TRUE)
            writeLines(header, file.path(src, "rcpp_hello_world.h"))
            message(" >> added example header file using Rcpp classes")
            file.copy(file.path(skeleton, "rcpp_hello_world.cpp"), 
                src)
            message(" >> added example src file using Rcpp classes")
            rcode <- readLines(file.path(skeleton, "rcpp_hello_world.R"))
            rcode <- gsub("@PKG@", name, rcode, fixed = TRUE)
            writeLines(rcode, file.path(root, "R", "rcpp_hello_world.R"))
            message(" >> added example R file calling the C++ example")
        }
        hello.Rd <- file.path(root, "man", "rcpp_hello_world.Rd")
        unlink(hello.Rd)
        file.copy(system.file("skeleton", "rcpp_hello_world.Rd", 
            package = "Rcpp"), hello.Rd)
        message(" >> added Rd file for rcpp_hello_world")
    }
    if (isTRUE(module)) {
        file.copy(system.file("skeleton", "rcpp_module.cpp", 
            package = "Rcpp"), file.path(root, "src"))
        file.copy(system.file("skeleton", "Num.cpp", package = "Rcpp"), 
            file.path(root, "src"))
        file.copy(system.file("skeleton", "stdVector.cpp", package = "Rcpp"), 
            file.path(root, "src"))
        file.copy(system.file("skeleton", "zzz.R", package = "Rcpp"), 
            file.path(root, "R"))
        file.copy(system.file("skeleton", "Rcpp_modules_examples.Rd", 
            package = "Rcpp"), file.path(root, "man"))
        message(" >> copied the example module file ")
    }
    lines <- readLines(package.doc <- file.path(root, "man", 
        sprintf("%s-package.Rd", name)))
    lines <- sub("~~ simple examples", "%% ~~ simple examples", 
        lines)
    lines <- lines[!grepl("~~ package title", lines)]
    lines <- lines[!grepl("~~ The author and", lines)]
    lines <- sub("Who wrote it", author, lines)
    lines <- sub("Who to complain to.*", sprintf("%s <%s>", maintainer, 
        email), lines)
    writeLines(lines, package.doc)
    if (fake) {
        rm("Rcpp.fake.fun", envir = env)
        unlink(file.path(root, "R", "Rcpp.fake.fun.R"))
        unlink(file.path(root, "man", "Rcpp.fake.fun.Rd"))
    }
    if (isTRUE(remove_hello_world)) {
        rm("rcpp_hello_world", envir = env)
    }
    invisible(NULL)
}


show <- methods::show # re-exported from methods package



## Package Data

# none


## Package Info

.skeleton_package_title = "Seamless R and C++ Integration"

.skeleton_package_version = "0.12.9"

.skeleton_package_depends = ""

.skeleton_package_imports = "methods,utils"


## Internal

.skeleton_version = 5


## EOF