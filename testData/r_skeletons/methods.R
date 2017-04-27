##
## Exported symobls in package `methods`
##

## Exported package methods

extends <- function (class1, class2, maybe = TRUE, fullInfo = FALSE) 
{
    if (is.character(class1)) {
        if (length(class1) > 1L) 
            class1 <- class1[[1L]]
        classDef1 <- getClassDef(class1)
    }
    else if (is(class1, "classRepresentation")) {
        classDef1 <- class1
        class1 <- classDef1@className
    }
    else stop("'class1' must be the name of a class or a class definition")
    if (missing(class2)) {
        if (is.null(classDef1)) 
            return(class1)
        ext <- classDef1@contains
        if (!identical(maybe, TRUE) && length(ext) > 0) {
            noTest <- vapply(ext, function(obj) identical(body(obj@test), 
                TRUE), NA)
            ext <- ext[noTest]
        }
        if (fullInfo) {
            ext[[class1]] <- TRUE
            return(ext)
        }
        else return(c(class1, names(ext)))
    }
    value <- NULL
    if (is.character(class2) && length(class2) == 1L) {
        if (.identC(class1[[1L]], class2) || .identC(class2, 
            "ANY")) 
            return(TRUE)
        if (!is.null(classDef1) && class2 %in% names(classDef1@contains)) 
            value <- classDef1@contains[[class2]]
        else classDef2 <- getClassDef(class2)
    }
    else if (is(class2, "classRepresentation")) {
        classDef2 <- class2
        class2 <- class2@className
    }
    else stop("'class2' must be the name of a class or a class definition")
    if (is.null(value)) 
        value <- possibleExtends(class1, class2, classDef1, classDef2)
    if (fullInfo) 
        value
    else if (is.logical(value)) 
        value
    else if (value@simple || identical(body(value@test), TRUE)) 
        TRUE
    else maybe
}


`S3Class<-` <- function (object, value) 
{
    if (isS4(object)) {
        current <- attr(object, ".S3Class")
        if (is.null(current)) {
            if (is.na(match(value, .BasicClasses))) 
                stop(gettextf("'S3Class' can only assign to S4 objects that extend \"oldClass\"; not true of class %s", 
                  dQuote(class(object))), domain = NA)
            mode(object) <- value
        }
        else slot(object, ".S3Class") <- value
    }
    else class(object) <- value
    object
}


makeMethodsList <- function (object, level = 1) 
{
    .MlistDeprecated("makeMethodsList()")
    mnames <- allNames(object)
    if (.noMlists()) {
        keep <- mnames %in% c("", "ANY")
        mnames <- mnames[keep]
        object <- object[keep]
    }
    value <- new("MethodsList")
    i <- match("", mnames)
    if (!is.na(i)) {
        mnames[[i]] <- "ANY"
        names(object) <- mnames
    }
    if (anyDuplicated(mnames)) 
        stop(gettextf("duplicate element names in 'MethodsList' at level %d: %s", 
            level, paste("\"", unique(mnames[duplicated(mnames)]), 
                "\"", collapse = ", ")), domain = NA)
    for (i in seq_along(object)) {
        eli <- object[[i]]
        if (is(eli, "function") || is(eli, "MethodsList")) {
        }
        else if (is(eli, "list") || is(eli, "named")) 
            object[[i]] <- Recall(eli, NULL, level + 1)
        else stop(gettextf("element %d at level %d (class %s) cannot be interpreted as a function or named list", 
            i, level, dQuote(class(eli))), domain = NA)
    }
    slot(value, "methods") <- object
    value
}


assignMethodsMetaData <- function (f, value, fdef, where) 
{
    where <- as.environment(where)
    if (is(value, "MethodsList")) {
        .MlistDeprecated()
        mname <- methodsPackageMetaName("M", fdef@generic, fdef@package)
        if (exists(mname, envir = where, inherits = FALSE) && 
            bindingIsLocked(mname, where)) {
        }
        else assign(mname, value, where)
    }
    if (dispatchIsInternal(fdef)) 
        setPrimitiveMethods(f, fdef@default, "reset", fdef, NULL)
    if (is(fdef, "groupGenericFunction")) 
        cacheGenericsMetaData(f, fdef, where = where, package = fdef@package)
}


Summary <- function (x, ..., na.rm = FALSE) 
standardGeneric("Summary")


setReplaceMethod <- function (f, ..., where = topenv(parent.frame())) 
setMethod(paste0(f, "<-"), ..., where = where)


.__C__EmptyMethodsList <- new("classRepresentation"
    , slots = structure(list(argument = structure("name", package = "methods"), 
    sublist = structure("list", package = "methods")), .Names = c("argument", 
"sublist"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("EmptyMethodsList", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


methodSignatureMatrix <- function (object, sigSlots = c("target", "defined")) 
{
    if (length(sigSlots)) {
        allSlots <- lapply(sigSlots, slot, object = object)
        mm <- unlist(allSlots)
        mm <- matrix(mm, nrow = length(allSlots), byrow = TRUE)
        dimnames(mm) <- list(sigSlots, names(allSlots[[1L]]))
        mm
    }
    else matrix(character(), 0L, 0L)
}


el <- function (object, where) 
object[where][[1L]]


.__C__lm <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("lm", package = "methods")
    , package = "methods"
    , subclasses = structure(list(mlm = S4_object(), 
    aov = S4_object(), 
    glm = S4_object(), 
    maov = S4_object(), 
    glm.null = S4_object()), .Names = c("mlm", 
"aov", "glm", "maov", "glm.null"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


completeClassDefinition <- function (Class, ClassDef = getClassDef(Class), where, doExtends = TRUE) 
{
    ClassDef <- .completeClassSlots(ClassDef, where)
    immediate <- ClassDef@contains
    properties <- ClassDef@slots
    prototype <- makePrototypeFromClassDef(properties, ClassDef, 
        immediate, where)
    virtual <- ClassDef@virtual
    extends <- if (doExtends) 
        completeExtends(ClassDef, where = where)
    else ClassDef@contains
    subclasses <- if (doExtends) 
        completeSubclasses(ClassDef, where = where)
    else ClassDef@subclasses
    if (is.na(virtual)) 
        virtual <- testVirtual(properties, immediate, prototype, 
            where)
    ClassDef <- .mergeClassDefSlots(ClassDef, slots = properties, 
        contains = extends, prototype = prototype, virtual = virtual, 
        subclasses = subclasses)
    if (any(!is.na(match(names(ClassDef@subclasses), names(ClassDef@contains)))) && 
        getOption("warn") > 0) {
        bad <- names(ClassDef@subclasses)[!is.na(match(names(ClassDef@subclasses), 
            names(ClassDef@contains)))]
        warning(gettextf("potential cycle in class inheritance: %s has duplicates in superclasses and subclasses (%s)", 
            dQuote(Class), paste(bad, collapse = ", ")), domain = NA)
    }
    ClassDef
}


newEmptyObject <- function () 
{
    value <- list()
    value
}


isGrammarSymbol <- function (symbol) 
{
    if (typeof(symbol) != "symbol") 
        FALSE
    else switch(as.character(symbol), `{` = , `if` = , `for` = , 
        `while` = , `repeat` = , return = , `next` = , `break` = , 
        `<-` = , `<<-` = TRUE, FALSE)
}


findFunction <- function (f, generic = TRUE, where = topenv(parent.frame())) 
{
    allWhere <- .findAll(f, where)
    ok <- logical(length(allWhere))
    for (i in seq_along(allWhere)) {
        wherei <- allWhere[[i]]
        if (!is.null(fdef <- wherei[[f]])) {
            ok[i] <- is.function(fdef) && (generic || is.primitive(fdef) || 
                !isGeneric(f, wherei, fdef))
        }
    }
    allWhere[ok]
}


hasMethods <- function (f, where, package = "") 
{
    fdef <- NULL
    nowhere <- missing(where)
    if (is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
        if (missing(package)) 
            package <- fdef@package
    }
    else if (!.isSingleString(f)) 
        stop(gettextf("argument 'f' must be a generic function or %s", 
            .notSingleString(f)), domain = NA)
    else if (missing(package)) {
        package <- packageSlot(f)
        if (is.null(package)) {
            if (missing(where)) 
                fdef <- getGeneric(f)
            else {
                fdef <- getGeneric(f, where = where)
                if (is.null(fdef)) 
                  fdef <- getGeneric(f)
            }
            if (is(fdef, "genericFunction")) 
                package <- fdef@package
            else stop(gettextf("'%s' is not a known generic function {and 'package' not specified}", 
                f), domain = NA)
        }
    }
    what <- .TableMetaName(f, package)
    testEv <- function(ev) exists(what, envir = ev, inherits = FALSE) && 
        length(names(get(what, envir = ev))) > 0L
    if (nowhere) {
        for (i in seq_along(search())) {
            if (testEv(as.environment(i))) 
                return(TRUE)
        }
        return(FALSE)
    }
    else testEv(as.environment(where))
}


.__C__ObjectsWithPackage <- new("classRepresentation"
    , slots = structure(list(.Data = structure("character", package = "methods"), 
    package = structure("character", package = "methods")), .Names = c(".Data", 
"package"))
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
    , className = structure("ObjectsWithPackage", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


sigToEnv <- function (signature, generic) 
{
    genericSig <- generic@signature
    package <- packageSlot(signature)
    if (is.null(package)) 
        parent <- environment(generic)
    else parent <- .requirePackage(package)
    value <- new.env(parent = parent)
    classes <- as.character(signature)
    args <- names(signature)
    for (i in seq_along(args)) assign(args[[i]], classes[[i]], 
        envir = value)
    if (length(args) < length(genericSig)) 
        for (other in genericSig[is.na(match(genericSig, args))]) assign(other, 
            "ANY", envir = value)
    value
}


.__C__namedList <- new("classRepresentation"
    , slots = structure(list(.Data = structure("list", package = "methods"), 
    names = structure("character", package = "methods")), .Names = c(".Data", 
"names"))
    , contains = structure(list(list = S4_object(), 
    vector = S4_object()), .Names = c("list", 
"vector"))
    , virtual = FALSE
    , prototype = new("list"
)
    , validity = NULL
    , access = list()
    , className = structure("namedList", package = "methods")
    , package = "methods"
    , subclasses = structure(list(listOfMethods = S4_object()), .Names = "listOfMethods")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


SignatureMethod <- function (names, signature, definition) 
{
    .MlistDeprecated("SignatureMethod()")
    n <- length(signature)
    if (n > length(names)) 
        stop("arguments 'names' and 'signature' must have the same length")
    if (n == 0) 
        return(definition)
    Class <- signature[[n]]
    name <- names[[n]]
    m <- MethodsList(name)
    slot(m, "methods")[[Class]] <- definition
    slot(m, "argument") <- as.name(name)
    SignatureMethod(names[-n], signature[-n], m)
}


allGenerics <- function (...) 
.Defunct("getGenerics")


method.skeleton <- function (generic, signature, file, external = FALSE, where = topenv(parent.frame())) 
{
    fdef <- getGeneric(generic, where = where)
    if (is.null(fdef)) {
        fdef <- implicitGeneric(generic, where = where)
        if (is.null(fdef)) 
            stop(gettextf("no function definition found for %s", 
                sQuote(generic)), domain = NA)
    }
    else {
        generic <- fdef@generic
    }
    signature <- matchSignature(signature, fdef)
    if (length(signature) == 0) 
        signature <- "ANY"
    sigNames <- fdef@signature
    length(sigNames) <- length(signature)
    method <- function() {
    }
    formals(method) <- formals(fdef)
    body(method) <- quote({
        stop("need a definition for the method here")
    })
    methodName <- paste(c(generic, signature), collapse = "_")
    if (missing(file)) 
        file <- paste0(methodName, ".R")
    output <- c(paste0("setMethod(\"", generic, "\","), paste0("    signature(", 
        paste0(sigNames, " = \"", signature, "\"", collapse = ", "), 
        "),"))
    method <- deparse(method)
    if (identical(external, FALSE)) 
        output <- c(output, paste0("    ", method), ")")
    else {
        if (is(external, "character")) 
            methodName <- toString(external)
        method[[1L]] <- paste0("`", methodName, "` <- ", method[[1L]])
        output <- c(method, "", output, paste0("  `", methodName, 
            "`)"))
    }
    writeLines(output, file)
    message(gettextf("Skeleton of method written to %s", if (is.character(file)) 
        file
    else "connection"), domain = NA)
    invisible(file)
}


.hasSlot <- function (object, name) 
.Call(C_R_hasSlot, object, name)


.__C__PossibleMethod <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = function () 
NULL
    , validity = NULL
    , access = list()
    , className = structure("PossibleMethod", package = "methods")
    , package = "methods"
    , subclasses = structure(list(`function` = S4_object(), 
    MethodDefinition = S4_object(), 
    classGeneratorFunction = S4_object(), 
    genericFunction = S4_object(), 
    derivedDefaultMethod = S4_object(), 
    MethodWithNext = S4_object(), 
    SealedMethodDefinition = S4_object(), 
    derivedDefaultMethod = S4_object(), 
    MethodWithNext = S4_object(), 
    SealedMethodDefinition = S4_object(), 
    standardGeneric = S4_object(), 
    nonstandardGenericFunction = S4_object(), 
    groupGenericFunction = S4_object(), 
    internalDispatchMethod = S4_object(), 
    internalDispatchMethod = S4_object(), 
    nonstandardGroupGenericFunction = S4_object(), 
    defaultBindingFunction = S4_object(), 
    refMethodDefWithTrace = S4_object(), 
    externalRefMethod = S4_object()), .Names = c("function", 
"MethodDefinition", "classGeneratorFunction", "genericFunction", 
"derivedDefaultMethod", "MethodWithNext", "SealedMethodDefinition", 
"derivedDefaultMethod", "MethodWithNext", "SealedMethodDefinition", 
"standardGeneric", "nonstandardGenericFunction", "groupGenericFunction", 
"internalDispatchMethod", "internalDispatchMethod", "nonstandardGroupGenericFunction", 
"defaultBindingFunction", "refMethodDefWithTrace", "externalRefMethod"
))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


selectMethod <- function (f, signature, optional = FALSE, useInherited = TRUE, 
    mlist = if (!is.null(fdef)) getMethodsForDispatch(fdef), 
    fdef = getGeneric(f, !optional), verbose = FALSE, doCache = FALSE) 
{
    if (is.environment(mlist)) {
        fenv <- environment(fdef)
        nsig <- .getGenericSigLength(fdef, fenv, FALSE)
        if (verbose) 
            cat("* mlist environment with", length(mlist), "potential methods\n")
        if (length(signature) < nsig) 
            signature[(length(signature) + 1):nsig] <- "ANY"
        if (identical(fdef@signature, "...")) {
            method <- .selectDotsMethod(signature, mlist, if (useInherited) 
                getMethodsForDispatch(fdef, inherited = TRUE))
            if (is.null(method) && !optional) 
                stop(gettextf("no method for %s matches class %s", 
                  sQuote("..."), dQuote(signature)), domain = NA)
            return(method)
        }
        method <- .findMethodInTable(signature, mlist, fdef)
        if (is.null(method)) {
            if (missing(useInherited)) 
                useInherited <- (is.na(match(signature, "ANY")) & 
                  if (identical(fdef, coerce)) 
                    c(TRUE, FALSE)
                  else TRUE)
            if (verbose) 
                cat("  no direct match found to signature (", 
                  paste(signature, collapse = ", "), ")\n", sep = "")
            methods <- if (any(useInherited)) {
                allmethods <- .getMethodsTable(fdef, fenv, check = FALSE, 
                  inherited = TRUE)
                .findInheritedMethods(signature, fdef, mtable = allmethods, 
                  table = mlist, useInherited = useInherited, 
                  verbose = verbose, doCache = doCache)
            }
            if (length(methods)) 
                return(methods[[1L]])
            else if (optional) 
                return(NULL)
            else stop(gettextf("no method found for signature %s", 
                paste(signature, collapse = ", ")))
        }
        else return(method)
    }
    else if (is.null(mlist)) {
        if (optional) 
            return(mlist)
        else stop(gettextf("%s has no methods defined", sQuote(f)), 
            domain = NA)
    }
    else stop("selectMethod(): mlist is not an environment or NULL :\n", 
        "** should no longer happen!", domain = NA)
}


metaNameUndo <- function (strings, prefix, searchForm = FALSE) 
{
    pattern <- methodsPackageMetaName(prefix, "")
    n <- nchar(pattern, "c")
    matched <- substr(strings, 1L, n) == pattern
    value <- substring(strings[matched], n + 1L)
    pkg <- sub("^[^:]*", "", value)
    if (searchForm) {
        global <- grep(".GlobalEnv", value)
        if (length(global)) {
            pkg[-global] <- paste0("package", pkg[-global])
            pkg[global] <- substring(pkg[global], 2L)
        }
    }
    else pkg <- substring(pkg, 2L)
    value <- sub(":.*", "", value)
    new("ObjectsWithPackage", value, package = pkg)
}


.__C__matrix <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(array = S4_object(), 
    structure = S4_object(), 
    vector = S4_object()), .Names = c("array", 
"structure", "vector"))
    , virtual = FALSE
    , prototype = structure(numeric(0), .Dim = c(0L, 0L))
    , validity = NULL
    , access = list()
    , className = "matrix"
    , package = "methods"
    , subclasses = structure(list(mts = S4_object()), .Names = "mts")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__refObject <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype =  "<environment>"
    , validity = NULL
    , access = list()
    , className = structure("refObject", package = "methods")
    , package = "methods"
    , subclasses = structure(list(environment = S4_object(), 
    externalptr = S4_object(), 
    name = S4_object(), 
    refClass = S4_object(), 
    .environment = S4_object(), 
    .externalptr = S4_object(), 
    .name = S4_object(), 
    sourceEnvironment = S4_object(), 
    refGeneratorSlot = S4_object(), 
    localRefClass = S4_object()), .Names = c("environment", 
"externalptr", "name", "refClass", ".environment", ".externalptr", 
".name", "sourceEnvironment", "refGeneratorSlot", "localRefClass"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__nonstandardGroupGenericFunction <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    groupMembers = structure("list", package = "methods"), generic = structure("character", package = "methods"), 
    package = structure("character", package = "methods"), group = structure("list", package = "methods"), 
    valueClass = structure("character", package = "methods"), 
    signature = structure("character", package = "methods"), 
    default = structure("optionalMethod", package = "methods"), 
    skeleton = structure("call", package = "methods")), .Names = c(".Data", 
"groupMembers", "generic", "package", "group", "valueClass", 
"signature", "default", "skeleton"))
    , contains = structure(list(groupGenericFunction = S4_object(), 
    nonstandardGeneric = S4_object(), 
    genericFunction = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("groupGenericFunction", 
"nonstandardGeneric", "genericFunction", "function", "OptionalFunction", 
"PossibleMethod", "optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("nonstandardGroupGenericFunction", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`slot<-` <- function (object, name, check = TRUE, value) 
{
    if (check) 
        value <- checkSlotAssignment(object, name, value)
    .Call(C_R_set_slot, object, name, value)
}


.__C__refMethodDefWithTrace <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    mayCall = structure("character", package = "methods"), name = structure("character", package = "methods"), 
    refClassName = structure("character", package = "methods"), 
    superClassMethod = structure("SuperClassMethod", package = "methods"), 
    original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c(".Data", 
"mayCall", "name", "refClassName", "superClassMethod", "original", 
"source"))
    , contains = structure(list(refMethodDef = S4_object(), 
    traceable = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    SuperClassMethod = S4_object()), .Names = c("refMethodDef", 
"traceable", "function", "OptionalFunction", "PossibleMethod", 
"SuperClassMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("refMethodDefWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__classGeneratorFunction <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    className = structure("character", package = "methods"), 
    package = structure("character", package = "methods")), .Names = c(".Data", 
"className", "package"))
    , contains = structure(list(`function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("function", 
"OptionalFunction", "PossibleMethod", "optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("classGeneratorFunction", package = "methods")
    , package = "methods"
    , subclasses = structure(list(refObjectGenerator = S4_object()), .Names = "refObjectGenerator")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


findMethod <- function (f, signature, where = topenv(parent.frame())) 
{
    if (is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
    }
    else fdef <- getGeneric(f, where = where)
    if (is.null(fdef)) {
        warning(gettextf("no generic function %s found", sQuote(f)), 
            domain = NA)
        return(character())
    }
    fM <- .TableMetaName(fdef@generic, fdef@package)
    where <- .findAll(fM, where)
    found <- logical(length(where))
    for (i in seq_along(where)) {
        wherei <- where[[i]]
        table <- get(fM, wherei, inherits = FALSE)
        mi <- .findMethodInTable(signature, table, fdef)
        found[i] <- !is.null(mi)
    }
    value <- where[found]
    what <- vapply(value, class, "", USE.NAMES = FALSE)
    if (identical(what, "numeric") || identical(what, "character")) 
        unlist(value)
    else value
}


showMethods <- function (f = character(), where = topenv(parent.frame()), classes = NULL, 
    includeDefs = FALSE, inherited = !includeDefs, showEmpty, 
    printTo = stdout(), fdef = getGeneric(f, where = where)) 
{
    if (missing(showEmpty)) 
        showEmpty <- !missing(f)
    if (identical(printTo, FALSE)) 
        con <- textConnection(NULL, "w")
    else con <- printTo
    if (is(f, "function")) {
        fdef <- f
        if (missing(where)) 
            where <- environment(f)
        f <- deparse(substitute(f))
        if (length(f) > 1L) 
            f <- paste(f, collapse = "; ")
    }
    if (!is(f, "character")) 
        stop(gettextf("first argument should be the names of one of more generic functions (got object of class %s)", 
            dQuote(class(f))), domain = NA)
    if (length(f) == 0L) {
        f <- if (missing(where)) 
            getGenerics()
        else getGenerics(where)
    }
    if (length(f) == 0L) 
        cat(file = con, "no applicable functions\n")
    else if (length(f) > 1L) {
        for (ff in f) {
            ffdef <- getGeneric(ff, where = where)
            if (missing(where)) {
                if (isGeneric(ff)) 
                  Recall(ff, classes = classes, includeDefs = includeDefs, 
                    inherited = inherited, showEmpty = showEmpty, 
                    printTo = con, fdef = ffdef)
            }
            else if (isGeneric(ff, where)) {
                Recall(ff, where = where, classes = classes, 
                  includeDefs = includeDefs, inherited = inherited, 
                  showEmpty = showEmpty, printTo = con, fdef = ffdef)
            }
        }
    }
    else {
        out <- paste0("\nFunction \"", f, "\":\n")
        if (!is(fdef, "genericFunction")) 
            cat(file = con, out, "<not an S4 generic function>\n")
        else .showMethodsTable(fdef, includeDefs, inherited, 
            classes = classes, showEmpty = showEmpty, printTo = con)
    }
    if (identical(printTo, FALSE)) {
        txtOut <- textConnectionValue(con)
        close(con)
        txtOut
    }
    else invisible(printTo)
}


possibleExtends <- function (class1, class2, ClassDef1 = getClassDef(class1), ClassDef2 = getClassDef(class2, 
    where = .classEnv(ClassDef1))) 
{
    if (.identC(class1[[1L]], class2) || .identC(class2, "ANY")) 
        return(TRUE)
    if (is.null(ClassDef1)) 
        return(FALSE)
    ext <- ClassDef1@contains
    if (!is.null(contained <- ext[[class2]])) 
        contained
    else if (is.null(ClassDef2)) 
        FALSE
    else {
        subs <- ClassDef2@subclasses
        if (!.identC(class(ClassDef2), "classRepresentation") && 
            isClassUnion(ClassDef2)) 
            any(c(class1, names(ext)) %in% names(subs))
        else {
            i <- match(class1, names(subs))
            i <- i[!is.na(i)]
            if (length(i)) 
                subs[[i[1L]]]
            else FALSE
        }
    }
}


`.__T__rbind2:methods` <- "<environment>"

findUnique <- function (what, message, where = topenv(parent.frame())) 
{
    where <- .findAll(what, where = where)
    if (length(where) > 1L) {
        if (missing(message)) 
            message <- sQuote(what)
        if (is.list(where)) 
            where <- unlist(where)
        if (is.numeric(where)) 
            where <- search()[where]
        warning(message, sprintf(" found on: %s; using the first one", 
            paste(sQuote(where), collapse = ", ")), domain = NA)
        where <- where[1L]
    }
    where
}


initialize <- function (.Object, ...) 
{
    value <- standardGeneric("initialize")
    if (!identical(class(value), class(.Object))) {
        cv <- class(value)
        co <- class(.Object)
        if (.identC(cv[[1L]], co)) {
            if (is.na(match(cv, .BasicClasses)) && length(cv) == 
                1L) {
                warning(gettextf("missing package slot (%s) in object of class %s (package info added)", 
                  packageSlot(co), dQuote(class(.Object))), domain = NA)
                class(value) <- class(.Object)
            }
            else return(value)
        }
        else stop(gettextf("'initialize' method returned an object of class %s instead of the required class %s", 
            paste(dQuote(class(value)), collapse = ", "), dQuote(class(.Object))), 
            domain = NA)
    }
    value
}


formalArgs <- function (def) 
names(formals(def))


.__C__NULL <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(OptionalFunction = S4_object(), 
    optionalMethod = S4_object()), .Names = c("OptionalFunction", 
"optionalMethod"))
    , virtual = FALSE
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = "NULL"
    , package = "methods"
    , subclasses = structure(list(.NULL = S4_object()), .Names = ".NULL")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__glm.null <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(glm = S4_object(), 
    lm = S4_object(), 
    oldClass = S4_object()), .Names = c("glm", 
"lm", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("glm.null", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


initRefFields <- function (.Object, classDef, selfEnv, args) 
{
    if (length(args)) {
        snames <- allNames(args)
        which <- nzchar(snames)
        elements <- args[which]
        supers <- args[!which]
        elNames <- names(elements)
        for (super in supers) {
            if (!is(super, "refClass")) {
                warning(gettextf("unnamed arguments to $new() must be objects from a reference class; got an object of class %s", 
                  dQuote(class(super))), domain = NA)
                next
            }
            fields <- names(super$.refClassDef@fieldClasses)
            fields <- fields[is.na(match(fields, elNames))]
            for (field in fields) elements[[field]] <- super$field(field)
            elNames <- names(elements)
        }
        for (field in elNames) envRefSetField(.Object, field, 
            classDef, selfEnv, elements[[field]])
    }
    .Object
}


`packageSlot<-` <- function (object, value) 
{
    attr(object, "package") <- value
    object
}


className <- function (class, package) 
{
    if (is(class, "character")) {
        className <- as.character(class)
        if (missing(package)) 
            package <- packageSlot(class)
        if (is.null(package)) {
            if (exists(className, envir = .classTable, inherits = FALSE)) 
                classDef <- get(className, envir = .classTable)
            else {
                classDef <- findClass(className, topenv(parent.frame()))
                if (length(classDef) == 1) 
                  classDef <- classDef[[1]]
            }
            if (is(classDef, "classRepresentation")) 
                package <- classDef@package
            else if (length(classDef) > 1L) {
                pkgs <- sapply(classDef, function(cl) cl@package)
                warning(gettextf("multiple class definitions for %s from packages: %s; picking the first", 
                  dQuote(className), paste(sQuote(pkgs), collapse = ", ")), 
                  domain = NA)
                package <- pkgs[[1L]]
            }
            else stop(gettextf("no package name supplied and no class definition found for %s", 
                dQuote(className)), domain = NA)
        }
    }
    else if (is(class, classDef)) {
        className <- class@className
        if (missing(package)) 
            package <- class@package
    }
    new("className", .Data = className, package = package)
}


S3Class <- function (object) 
{
    value <- attr(object, ".S3Class")
    if (is.null(value)) {
        if (isS4(object)) {
            if (is.na(match(".Data", names(getClass(class(object))@slots)))) 
                stop(gettextf("'S3Class' only defined for extensions of %s or classes with a data part:  not true of class %s", 
                  dQuote("oldClass"), dQuote(class(object))), 
                  domain = NA)
            class(getDataPart(object))
        }
        else class(object)
    }
    else value
}


setGenericImplicit <- function (name, where = topenv(parent.frame()), restore = TRUE) 
{
    if (!isGeneric(name, where)) {
        warning(gettextf("%s is not currently a generic:  define it first to create a non-default implicit form", 
            sQuote(name)), domain = NA)
        return(FALSE)
    }
    generic <- getGeneric(name, where = where)
    if (restore) 
        removeMethods(name, where, TRUE)
    else removeGeneric(name, where)
    .saveToImplicitGenerics(name, generic, where)
}


makeClassRepresentation <- function (name, slots = list(), superClasses = character(), prototype = NULL, 
    package, validity = NULL, access = list(), version = .newExternalptr(), 
    sealed = FALSE, virtual = NA, where) 
{
    if (any(superClasses %in% .AbnormalTypes)) 
        superClasses <- .addAbnormalDataType(superClasses)
    if (!is.null(prototype) || length(slots) || length(superClasses)) {
        pp <- reconcilePropertiesAndPrototype(name, slots, prototype, 
            superClasses, where)
        slots <- pp$properties
        prototype <- pp$prototype
    }
    contains <- list()
    if (nzchar(package)) 
        packageSlot(name) <- package
    for (what in superClasses) {
        whatClassDef <- if (is(what, "classRepresentation")) 
            what
        else if (is.null(packageSlot(what))) 
            getClass(what, where = where)
        else getClass(what)
        what <- whatClassDef@className
        contains[[what]] <- makeExtends(name, what, slots = slots, 
            classDef2 = whatClassDef, package = package)
    }
    validity <- .makeValidityMethod(name, validity)
    if (is.na(virtual)) {
        virtual <- testVirtual(slots, contains, prototype, where)
        if (virtual && !is.na(match("VIRTUAL", superClasses))) 
            contains[["VIRTUAL"]] <- NULL
    }
    if (!is.null(prototype) && is.na(match(name, .BasicClasses))) 
        prototype <- .asS4(prototype)
    if (".S3Class" %in% names(slots)) 
        prototype <- .addS3Class(name, prototype, contains, where)
    newClassRepresentation(className = name, slots = slots, contains = contains, 
        prototype = prototype, virtual = virtual, validity = validity, 
        access = access, package = package, versionKey = version, 
        sealed = sealed)
}


hasMethod <- function (f, signature = character(), where = .genEnv(f, topenv(parent.frame()))) 
{
    fdef <- getGeneric(f, where = where)
    if (is.null(fdef)) 
        FALSE
    else !is.null(selectMethod(f, signature, optional = TRUE, 
        fdef = fdef))
}


MethodAddCoerce <- function (method, argName, thisClass, methodClass) 
{
    if (.identC(thisClass, methodClass)) 
        return(method)
    ext <- possibleExtends(thisClass, methodClass)
    if (is.logical(ext) || ext@simple) 
        return(method)
    methodInsert <- function(method, addExpr) {
        if (is.function(method)) {
            newBody <- substitute({
                firstExpr
                secondExpr
            }, list(firstExpr = addExpr, secondExpr = body(method)))
            body(method, envir = environment(method)) <- newBody
        }
        else if (is(method, "MethodsList")) {
            .MlistDeprecated()
            methods <- method@allMethods
            for (i in seq_along(methods)) methods[[i]] <- Recall(methods[[i]], 
                addExpr)
            method@allMethods <- methods
        }
        method
    }
    addExpr <- substitute(XXX <- as(XXX, CLASS), list(XXX = argName, 
        CLASS = methodClass))
    methodInsert(method, addExpr)
}


getGeneric <- function (f, mustFind = FALSE, where, package = "") 
{
    if (is.function(f)) {
        if (is(f, "genericFunction")) 
            return(f)
        else if (is.primitive(f)) 
            return(genericForBasic(.primname(f), mustFind = mustFind))
        else stop("argument 'f' must be a string, generic function, or primitive: got an ordinary function")
    }
    value <- if (missing(where)) 
        .getGeneric(f, , package)
    else .getGeneric(f, where, package)
    if (is.null(value) && !is.null(baseDef <- baseenv()[[f]])) {
        if (is.function(baseDef)) {
            value <- genericForBasic(f, mustFind = FALSE)
            if (is(value, "genericFunction")) 
                value <- .cacheGeneric(f, value)
        }
    }
    if (is.function(value)) 
        value
    else {
        if (nzchar(package) && is.na(match(package, c("methods", 
            "base")))) {
            value <- tryCatch({
                ev <- getNamespace(package)
                .getGeneric(f, ev, package)
            }, error = function(e) NULL)
        }
        if (is.function(value)) 
            value
        else if (mustFind) 
            stop(gettextf("no generic function found for %s", 
                sQuote(f)), domain = NA)
        else NULL
    }
}


.__C__list <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(vector = S4_object()), .Names = "vector")
    , virtual = FALSE
    , prototype = list()
    , validity = NULL
    , access = list()
    , className = "list"
    , package = "methods"
    , subclasses = structure(list(data.frame = S4_object(), 
    namedList = S4_object(), 
    listOfMethods = S4_object()), .Names = c("data.frame", 
"namedList", "listOfMethods"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


evalqOnLoad <- function (expr, where = topenv(parent.frame()), aname = "") 
evalOnLoad(substitute(expr), where, aname)


resetGeneric <- function (f, fdef = getGeneric(f, where = where), mlist = getMethodsForDispatch(fdef), 
    where = topenv(parent.frame()), deflt = finalDefaultMethod(mlist)) 
{
    if (!is(fdef, "genericFunction")) {
        stop(gettextf("error in updating S4 generic function %s; the function definition is not an S4 generic function (class %s)", 
            sQuote(f), dQuote(class(fdef))), domain = NA)
    }
    .updateMethodsInTable(fdef, attach = "reset")
    f
}


makePrototypeFromClassDef <- function (slots, ClassDef, extends, where) 
{
    className <- ClassDef@className
    snames <- names(slots)
    supers <- names(extends)
    dataPartClass <- elNamed(slots, ".Data")
    prototype <- ClassDef@prototype
    dataPartDone <- is.null(dataPartClass) || is(prototype, dataPartClass)
    if (!.identC(class(prototype), className) && .isPrototype(prototype)) {
        pnames <- prototype@slots
        prototype <- prototype@object
    }
    else pnames <- names(attributes(prototype))
    if (length(slots) == 0L && !is.null(prototype)) 
        return(prototype)
    for (i in seq_along(extends)) {
        what <- el(supers, i)
        exti <- extends[[i]]
        if (identical(exti@simple, FALSE)) 
            next
        if (identical(what, "VIRTUAL")) {
        }
        else if (isClass(what, where = where)) {
            cli <- getClass(what, where = where)
            slotsi <- names(cli@slots)
            pri <- cli@prototype
            if (is.null(prototype)) {
                prototype <- pri
                pnames <- names(attributes(prototype))
            }
            else if (length(slots)) {
                for (slotName in slotsi) {
                  if (identical(slotName, ".Data")) {
                    if (!dataPartDone) {
                      prototype <- setDataPart(prototype, getDataPart(pri), 
                        FALSE)
                      dataPartDone <- TRUE
                    }
                  }
                  else if (is.na(match(slotName, pnames))) {
                    attr(prototype, slotName) <- attr(pri, slotName)
                    pnames <- c(pnames, slotName)
                  }
                }
            }
            else if (!dataPartDone && extends(cli, dataPartClass)) {
                prototype <- setDataPart(prototype, pri, FALSE)
                dataPartDone <- TRUE
            }
        }
    }
    if (length(slots) == 0L) 
        return(prototype)
    if (is.null(prototype)) 
        prototype <- defaultPrototype()
    pnames <- names(attributes(prototype))
    pslots <- if (.identC(class(prototype), className)) 
        names(attributes(unclass(prototype)))
    else if (isClass(class(prototype))) 
        names(getSlots(getClass(class(prototype))))
    if (!is.na(match(".Data", snames))) {
        dataPartClass <- elNamed(slots, ".Data")
        if (!(isVirtualClass(dataPartClass))) {
            if (isClass(class(prototype), where = where)) {
                prototypeClass <- getClass(class(prototype), 
                  where = where)
                OK <- extends(prototypeClass, dataPartClass)
            }
            else OK <- FALSE
            if (identical(OK, FALSE)) 
                stop(gettextf("in constructing the prototype for class %s: prototype has class %s, but the data part specifies class %s", 
                  dQuote(className), dQuote(.class1(prototype)), 
                  dQuote(dataPartClass)), domain = NA)
        }
        iData <- -match(".Data", snames)
        snames <- snames[iData]
        slots <- slots[iData]
    }
    for (j in seq_along(snames)) {
        name <- el(snames, j)
        i <- match(name, pnames)
        if (is.na(i)) {
            slot(prototype, name, check = FALSE) <- tryNew(el(slots, 
                j), where)
        }
    }
    extra <- pnames[is.na(match(pnames, snames)) & !is.na(match(pnames, 
        pslots))]
    if (length(extra) && is.na(match("oldClass", supers))) 
        warning(gettextf("in constructing the prototype for class %s, slots in prototype and not in class: %s", 
            dQuote(className), paste(extra, collapse = ", ")), 
            domain = NA)
    slotDefs <- getSlots(ClassDef)
    slotNames <- names(slotDefs)
    pnames <- names(attributes(prototype))
    pnames <- pnames[!is.na(match(pnames, slotNames))]
    check <- rep.int(FALSE, length(pnames))
    for (what in pnames) {
        pwhat <- slot(prototype, what)
        slotClass <- getClassDef(slotDefs[[what]], where)
        if (is.null(slotClass) || !extends(class(pwhat), slotClass)) {
            if (is.null(pwhat)) {
            }
            else if (is(slotClass, "classRepresentation") && 
                slotClass@virtual) {
            }
            else check[match(what, pnames)] <- TRUE
        }
    }
    if (any(check)) 
        stop(gettextf("in making the prototype for class %s elements of the prototype failed to match the corresponding slot class: %s", 
            dQuote(className), paste(pnames[check], "(class", 
                .dQ(slotDefs[match(pnames[check], slotNames)]), 
                ")", collapse = ", ")), domain = NA)
    prototype
}


.__C__functionWithTrace <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c(".Data", 
"original", "source"))
    , contains = structure(list(`function` = S4_object(), 
    traceable = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object()), .Names = c("function", 
"traceable", "OptionalFunction", "PossibleMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("functionWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__MethodDefinitionWithTrace <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    target = structure("signature", package = "methods"), defined = structure("signature", package = "methods"), 
    generic = structure("character", package = "methods"), original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c(".Data", 
"target", "defined", "generic", "original", "source"))
    , contains = structure(list(MethodDefinition = S4_object(), 
    traceable = S4_object(), 
    `function` = S4_object(), 
    PossibleMethod = S4_object(), 
    OptionalFunction = S4_object()), .Names = c("MethodDefinition", 
"traceable", "function", "PossibleMethod", "OptionalFunction"
))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("MethodDefinitionWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


slotNames <- function (x) 
if (is(x, "classRepresentation")) names(x@slots) else .slotNames(x)


setLoadActions <- function (..., .where = topenv(parent.frame())) 
{
    actionListName <- .actionMetaName("")
    currentAnames <- .assignActionListNames(.where)
    actions <- list(...)
    anames <- allNames(actions)
    previous <- anames %in% currentAnames
    if (any(previous)) {
        .assignActions(actions[previous], anames[previous], .where)
        if (all(previous)) 
            return(list())
        anames <- anames[!previous]
        actions <- actions[!previous]
    }
    anon <- !nzchar(anames)
    if (any(anon)) {
        n <- length(currentAnames)
        deflts <- paste0(".", seq(from = n + 1, length.out = length(actions)))
        anames[anon] <- deflts[anon]
    }
    .assignActions(actions, anames, .where)
    assign(actionListName, c(currentAnames, anames), envir = .where)
}


.__C__OptionalFunction <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = structure("OptionalFunction", package = "methods")
    , package = "methods"
    , subclasses = structure(list(`NULL` = S4_object(), 
    `function` = S4_object(), 
    classGeneratorFunction = S4_object(), 
    MethodDefinition = S4_object(), 
    genericFunction = S4_object(), 
    derivedDefaultMethod = S4_object(), 
    MethodWithNext = S4_object(), 
    SealedMethodDefinition = S4_object(), 
    standardGeneric = S4_object(), 
    nonstandardGenericFunction = S4_object(), 
    groupGenericFunction = S4_object(), 
    internalDispatchMethod = S4_object(), 
    nonstandardGroupGenericFunction = S4_object(), 
    .NULL = S4_object(), 
    defaultBindingFunction = S4_object(), 
    refMethodDefWithTrace = S4_object(), 
    externalRefMethod = S4_object()), .Names = c("NULL", 
"function", "classGeneratorFunction", "MethodDefinition", "genericFunction", 
"derivedDefaultMethod", "MethodWithNext", "SealedMethodDefinition", 
"standardGeneric", "nonstandardGenericFunction", "groupGenericFunction", 
"internalDispatchMethod", "nonstandardGroupGenericFunction", 
".NULL", "defaultBindingFunction", "refMethodDefWithTrace", "externalRefMethod"
))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__derivedDefaultMethod <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    target = structure("signature", package = "methods"), defined = structure("signature", package = "methods"), 
    generic = structure("character", package = "methods")), .Names = c(".Data", 
"target", "defined", "generic"))
    , contains = structure(list(MethodDefinition = S4_object(), 
    `function` = S4_object(), 
    PossibleMethod = S4_object(), 
    OptionalFunction = S4_object(), 
    optionalMethod = S4_object()), .Names = c("MethodDefinition", 
"function", "PossibleMethod", "OptionalFunction", "optionalMethod"
))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("derivedDefaultMethod", package = "methods")
    , package = "methods"
    , subclasses = structure(list(internalDispatchMethod = S4_object(), 
    derivedDefaultMethodWithTrace = S4_object()), .Names = c("internalDispatchMethod", 
"derivedDefaultMethodWithTrace"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


checkAtAssignment <- function (cl, name, valueClass) 
{
    ClassDef <- getClass(cl)
    slotClass <- ClassDef@slots[[name]]
    if (is.null(slotClass)) 
        stop(gettextf("%s is not a slot in class %s", sQuote(name), 
            dQuote(cl)), domain = NA)
    if (.identC(slotClass, valueClass)) 
        return(TRUE)
    ok <- possibleExtends(valueClass, slotClass, ClassDef2 = getClassDef(slotClass, 
        where = .classEnv(ClassDef)))
    if (identical(ok, FALSE)) 
        stop(gettextf("assignment of an object of class %s is not valid for @%s in an object of class %s; is(value, \"%s\") is not TRUE", 
            dQuote(valueClass), sQuote(name), dQuote(cl), slotClass), 
            domain = NA)
    TRUE
}


.__C__traceable <- new("classRepresentation"
    , slots = structure(list(original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c("original", 
"source"))
    , contains = structure(list(), .Names = character(0))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("traceable", package = "methods")
    , package = "methods"
    , subclasses = structure(list(functionWithTrace = S4_object(), 
    MethodDefinitionWithTrace = S4_object(), 
    MethodWithNextWithTrace = S4_object(), 
    genericFunctionWithTrace = S4_object(), 
    standardGenericWithTrace = S4_object(), 
    nonstandardGenericWithTrace = S4_object(), 
    groupGenericFunctionWithTrace = S4_object(), 
    derivedDefaultMethodWithTrace = S4_object(), 
    refMethodDefWithTrace = S4_object()), .Names = c("functionWithTrace", 
"MethodDefinitionWithTrace", "MethodWithNextWithTrace", "genericFunctionWithTrace", 
"standardGenericWithTrace", "nonstandardGenericWithTrace", "groupGenericFunctionWithTrace", 
"derivedDefaultMethodWithTrace", "refMethodDefWithTrace"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


setOldClass <- function (Classes, prototype = NULL, where = topenv(parent.frame()), 
    test = FALSE, S4Class) 
{
    simpleCase <- is.null(prototype)
    mainClass <- Classes[[1L]]
    prevDef <- getClassDef(mainClass, where, inherits = FALSE)
    if (!missing(S4Class)) {
        if (test) 
            stop("not allowed to have test==TRUE and an S4Class definition")
        if (!is(S4Class, "classRepresentation")) {
            if (is.character(S4Class)) {
                clName <- S4Class
                S4Class <- getClass(S4Class)
                if (.identC(clName, Classes[[1L]])) 
                  removeClass(clName, where = where)
            }
            else stop(gettextf("argument 'S4Class' must be a class definition: got an object of class %s", 
                dQuote(class(S4Class))), domain = NA)
        }
        if (!is.null(prototype)) {
            S4prototype <- S4Class@prototype
            S4Class@prototype <- .mergeAttrs(prototype, S4prototype)
        }
        Recall(Classes, where = where)
        return(.S4OldClass(Classes[[1L]], if (length(Classes) > 
            1) Classes[[2L]] else "oldClass", S4Class, where, 
            prevDef))
    }
    if (test) 
        return(.setOldIs(Classes, where))
    if (!is.null(prevDef)) {
        on.exit(.restoreClass(prevDef, where))
        removeClass(mainClass, where = where)
    }
    prevClass <- "oldClass"
    S3Class <- character()
    if (is.null(S3table <- where$.S3MethodsClasses)) {
        S3table <- new.env()
        assign(".S3MethodsClasses", S3table, envir = where)
    }
    dataPartClass <- NULL
    for (cl in rev(Classes)) {
        S3Class <- c(cl, S3Class)
        if (isClass(cl, where)) {
            def <- getClass(cl, where)
            if (!extends(def, prevClass)) {
                cl1 <- .validDataPartClass(cl, where, dataPartClass)
                if (is.null(cl1)) 
                  stop(gettextf("inconsistent old-style class information for %s; the class is defined but does not extend %s and is not valid as the data part", 
                    dQuote(cl), dQuote(prevClass)), domain = NA)
                else dataPartClass <- cl1
            }
            else {
                prevP <- def@prototype
                if (missing(prototype)) 
                  prototype <- prevP
                prevS3Class <- attr(prevP, ".S3Class")
                if (length(prevS3Class) > length(S3Class)) 
                  S3Class <- prevS3Class
            }
        }
        else {
            useP <- TRUE
            if (cl != mainClass || simpleCase) {
                setClass(cl, contains = c(prevClass, "VIRTUAL"), 
                  where = where)
            }
            else if (isClass(class(prototype))) 
                setClass(cl, contains = prevClass, prototype = prototype, 
                  where = where)
            else {
                if (.class1(prototype) != mainClass) 
                  stop(gettextf("the S3 class of the prototype, \"%s\", is undefined; only allowed when this is the S3 class being registered (\"%s\")", 
                    .class1(prototype), mainClass), domain = NA)
                setClass(cl, contains = prevClass, where = where)
                useP <- FALSE
            }
            def <- getClassDef(cl, where)
            if (useP) 
                clp <- def@prototype
            else clp <- prototype
            attr(clp, ".S3Class") <- S3Class
            def@prototype <- .notS4(clp)
            assignClassDef(cl, def, where = where)
            assign(cl, def, envir = S3table)
        }
        prevClass <- cl
    }
    if (!is.null(prevDef)) 
        on.exit()
}


`.__T__Complex:base` <- "<environment>"

substituteDirect <- function (object, frame = parent.frame(), cleanFunction = TRUE) 
{
    value <- .Call(C_do_substitute_direct, object, frame)
    if (cleanFunction && is.function(value)) {
        environment(value) <- .GlobalEnv
    }
    value
}


requireMethods <- function (functions, signature, message = "", where = topenv(parent.frame())) 
{
    for (f in functions) {
        method <- getMethod(f, optional = TRUE)
        if (!is.function(method)) 
            method <- getGeneric(f, where = where)
        body(method) <- substitute(stop(methods:::.missingMethod(FF, 
            MESSAGE, if (exists(".Method")) .Method), domain = NA), 
            list(FF = f, MESSAGE = message))
        environment(method) <- .GlobalEnv
        setMethod(f, signature, method, where = where)
    }
    NULL
}


Complex <- function (z) 
standardGeneric("Complex")


setIs <- function (class1, class2, test = NULL, coerce = NULL, replace = NULL, 
    by = character(), where = topenv(parent.frame()), classDef = getClass(class1, 
        TRUE, where = where), extensionObject = NULL, doComplete = TRUE) 
{
    where <- as.environment(where)
    classDef2 <- getClassDef(class2, where)
    if (is.null(classDef2)) 
        stop(gettextf("class %s has no visible definition from package or environment %s", 
            dQuote(class2), sQuote(getPackageName(where))), domain = NA)
    m1 <- classMetaName(class1)
    local1 <- exists(m1, where, inherits = FALSE) && !(classDef@sealed || 
        bindingIsLocked(m1, where))
    if (!local1) {
        m2 <- classMetaName(class2)
        local2 <- exists(m2, where, inherits = FALSE) && !(classDef2@sealed || 
            bindingIsLocked(m2, where))
        if (!local2) 
            stop(gettextf("cannot create a 'setIs' relation when neither of the classes (%s and %s) is local and modifiable in this package", 
                dQuote(class1), dQuote(class2)), domain = NA)
    }
    if (classDef@sealed && !isClassUnion(classDef2)) 
        stop(gettextf("class %s is sealed; new superclasses can not be defined, except by 'setClassUnion'", 
            dQuote(class1)), domain = NA)
    prevIs <- !identical(possibleExtends(class1, class2, classDef, 
        classDef2), FALSE)
    if (is.null(extensionObject)) 
        obj <- makeExtends(class1, class2, coerce, test, replace, 
            by, classDef1 = classDef, classDef2 = classDef2, 
            package = getPackageName(where))
    else obj <- extensionObject
    ok <- .validExtends(class1, class2, classDef, classDef2, 
        obj@simple)
    if (!identical(ok, TRUE)) 
        stop(ok)
    where2 <- .findOrCopyClass(class2, classDef2, where, "subclass")
    classDef2@subclasses[[class1]] <- obj
    if (doComplete) 
        classDef2@subclasses <- completeSubclasses(classDef2, 
            class1, obj, where)
    if (classDef2@virtual && is.na(match(class2, .specialVirtual))) {
        if (extends(classDef, "NULL")) 
            classDef2@prototype <- NULL
        else if (is.null(classDef2@prototype) && is.na(match("NULL", 
            names(classDef2@subclasses)))) {
            if (classDef@virtual) 
                classDef2@prototype <- classDef@prototype
            else classDef2@prototype <- .Call(C_new_object, classDef)
        }
    }
    assignClassDef(class2, classDef2, where2, TRUE)
    .removePreviousCoerce(class1, class2, where, prevIs)
    where1 <- .findOrCopyClass(class1, classDef, where, "superClass")
    .newDirectSuperclass(classDef@contains, class2, names(classDef2@contains)) <- obj
    if (doComplete) {
        classDef@contains <- completeExtends(classDef, class2, 
            obj, where = where)
        if (!is(classDef, "ClassUnionRepresentation")) 
            .checkSubclasses(class1, classDef, class2, classDef2, 
                where1, where2)
    }
    assignClassDef(class1, classDef, where1, TRUE)
    invisible(classDef)
}


methodsPackageMetaName <- function (prefix, name, package = "") 
.Call(C_R_methodsPackageMetaName, prefix, name, package)


linearizeMlist <- function (mlist, inherited = TRUE) 
{
    methods <- mlist@methods
    allMethods <- mlist@allMethods
    if (inherited && length(allMethods) >= length(methods)) {
        methods <- allMethods
    }
    preC <- function(y, x) c(x, y)
    cnames <- names(methods)
    value <- list()
    classes <- list()
    arguments <- list()
    argname <- as.character(mlist@argument)
    for (i in seq_along(cnames)) {
        mi <- methods[[i]]
        if (is.function(mi)) {
            value <- c(value, list(mi))
            classes <- c(classes, list(cnames[[i]]))
            arguments <- c(arguments, list(argname))
        }
        else if (is(mi, "MethodsList")) {
            .MlistDeprecated()
            mi <- Recall(mi, inherited)
            value <- c(value, mi@methods)
            classes <- c(classes, lapply(mi@classes, preC, cnames[[i]]))
            arguments <- c(arguments, lapply(mi@arguments, preC, 
                argname))
        }
        else warning(gettextf("skipping methods list element %s of unexpected class %s\n\n", 
            paste(cnames[i], collapse = ", "), dQuote(.class1(mi))), 
            domain = NA)
    }
    new("LinearMethodsList", methods = value, classes = classes, 
        arguments = arguments)
}


isSealedClass <- function (Class, where = topenv(parent.frame())) 
{
    if (is.character(Class)) 
        Class <- getClass(Class, TRUE, where)
    if (!is(Class, "classRepresentation")) 
        FALSE
    else Class@sealed
}


kronecker <- function (X, Y, FUN = "*", make.dimnames = FALSE, ...) 
standardGeneric("kronecker")


cbind2 <- function (x, y, ...) 
standardGeneric("cbind2")


.__C__builtin <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = FALSE
    , prototype = .Primitive("<-")
    , validity = NULL
    , access = list()
    , className = "builtin"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__ClassUnionRepresentation <- new("classRepresentation"
    , slots = structure(list(slots = structure("list", package = "methods"), 
    contains = structure("list", package = "methods"), virtual = structure("logical", package = "methods"), 
    prototype = structure("ANY", package = "methods"), validity = structure("OptionalFunction", package = "methods"), 
    access = structure("list", package = "methods"), className = structure("character", package = "methods"), 
    package = structure("character", package = "methods"), subclasses = structure("list", package = "methods"), 
    versionKey = structure("externalptr", package = "methods"), 
    sealed = structure("logical", package = "methods")), .Names = c("slots", 
"contains", "virtual", "prototype", "validity", "access", "className", 
"package", "subclasses", "versionKey", "sealed"))
    , contains = structure(list(classRepresentation = S4_object()), .Names = "classRepresentation")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = function (object) 
{
    if (identical(object@virtual, TRUE) && length(object@slots) == 
        0 && is.null(object@prototype)) 
        TRUE
    else "Class must be an empty virtual class with NULL prototype"
}
    , access = list()
    , className = structure("ClassUnionRepresentation", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


isGeneric <- function (f, where = topenv(parent.frame()), fdef = NULL, getName = FALSE) 
{
    if (is.null(fdef) && missing(where)) {
        fdef <- .getGenericFromCache(f, where)
        if (!is.null(fdef)) 
            return(if (getName) fdef@generic else TRUE)
    }
    if (is.null(fdef)) 
        fdef <- getFunction(f, where = where, mustFind = FALSE)
    if (is.null(fdef)) 
        return(FALSE)
    if (isBaseFun(fdef)) {
        if (is.character(f) && f %in% "as.double") 
            f <- "as.numeric"
        gen <- genericForBasic(f, mustFind = FALSE)
        return(is.function(gen) && length(names(.getMethodsTable(gen))) > 
            1L)
    }
    if (!is(fdef, "genericFunction")) 
        return(FALSE)
    gen <- fdef@generic
    if (missing(f) || .identC(gen, f)) {
        if (getName) 
            gen
        else TRUE
    }
    else {
        warning(gettextf("function %s appears to be a generic function, but with generic name %s", 
            sQuote(f), sQuote(gen)), domain = NA)
        FALSE
    }
}


.__C__refClassRepresentation <- new("classRepresentation"
    , slots = structure(list(fieldClasses = structure("list", package = "methods"), 
    fieldPrototypes = structure("environment", package = "methods"), 
    refMethods = structure("environment", package = "methods"), 
    refSuperClasses = structure("character", package = "methods"), 
    slots = structure("list", package = "methods"), contains = structure("list", package = "methods"), 
    virtual = structure("logical", package = "methods"), prototype = structure("ANY", package = "methods"), 
    validity = structure("OptionalFunction", package = "methods"), 
    access = structure("list", package = "methods"), className = structure("character", package = "methods"), 
    package = structure("character", package = "methods"), subclasses = structure("list", package = "methods"), 
    versionKey = structure("externalptr", package = "methods"), 
    sealed = structure("logical", package = "methods")), .Names = c("fieldClasses", 
"fieldPrototypes", "refMethods", "refSuperClasses", "slots", 
"contains", "virtual", "prototype", "validity", "access", "className", 
"package", "subclasses", "versionKey", "sealed"))
    , contains = structure(list(classRepresentation = S4_object()), .Names = "classRepresentation")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("refClassRepresentation", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__Arith:base` <- "<environment>"

`.__T__show:methods` <- "<environment>"

canCoerce <- function (object, Class) 
{
    is(object, Class) || !is.null(selectMethod("coerce", c(class(object), 
        Class), optional = TRUE, useInherited = c(from = TRUE, 
        to = FALSE)))
}


newBasic <- function (Class, ...) 
{
    msg <- NULL
    value <- switch(Class, `NULL` = return(NULL), logical = , 
        numeric = , character = , complex = , integer = , raw = , 
        list = as.vector(c(...), Class), expression = eval(substitute(expression(...))), 
        externalptr = {
            if (nargs() > 1) stop("'externalptr' objects cannot be initialized from new()")
            .newExternalptr()
        }, single = as.single(c(...)), array = if (!missing(...)) array(...) else structure(numeric(), 
            .Dim = 0L), matrix = if (!missing(...)) matrix(...) else matrix(0, 
            0L, 0L), ts = if (!missing(...)) stats::ts(...) else structure(NA, 
            .Tsp = c(1, 1, 1), class = "ts"), {
            args <- list(...)
            if (length(args) == 1L && is(args[[1L]], Class)) {
                value <- as(args[[1L]], Class)
            } else if (is.na(match(Class, .BasicClasses))) msg <- paste("Calling new() on an undefined and non-basic class (\"", 
                Class, "\")", sep = "") else msg <- gettextf("initializing objects from class %s with these arguments is not supported", 
                dQuote(Class))
        })
    if (is.null(msg)) 
        value
    else stop(msg, domain = NA)
}


.__C__.environment <- new("classRepresentation"
    , slots = structure(list(.xData = structure("environment", package = "methods")), .Names = ".xData")
    , contains = structure(list(environment = S4_object(), 
    refObject = S4_object()), .Names = c("environment", 
"refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure(".environment", package = "methods")
    , package = "methods"
    , subclasses = structure(list(sourceEnvironment = S4_object(), 
    envRefClass = S4_object(), 
    refGeneratorSlot = S4_object(), 
    localRefClass = S4_object()), .Names = c("sourceEnvironment", 
"envRefClass", "refGeneratorSlot", "localRefClass"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__refClass <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = structure(list(refObject = S4_object()), .Names = "refObject")
    , virtual = TRUE
    , prototype = new("envRefClass"
    , .xData =  "<environment>"
)
    , validity = NULL
    , access = list()
    , className = structure("refClass", package = "methods")
    , package = "methods"
    , subclasses = structure(list(envRefClass = S4_object(), 
    refObjectGenerator = S4_object()), .Names = c("envRefClass", 
"refObjectGenerator"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


completeSubclasses <- function (classDef, class2, extensionDef, where, classDef2 = getClassDef(class2, 
    where)) 
{
    ext <- classDef@subclasses
    for (i in seq_along(ext)) {
        if (.isIndirectExtension(ext[[i]])) {
            classDef <- .uncompleteClassDefinition(classDef, 
                "subclasses")
            break
        }
    }
    subclasses <- .walkClassGraph(classDef, "subclasses", where)
    if (!missing(class2) && length(classDef@contains)) {
        strictBy <- TRUE
        contains <- .transitiveExtends(class2, classDef@className, 
            extensionDef, classDef@contains, strictBy)
        for (i in seq_along(contains)) {
            obji <- contains[[i]]
            cli <- contains[[i]]@superClass
            cliDef <- getClassDef(cli, where)
            if (!extends(classDef2, cliDef)) 
                setIs(class2, cli, extensionObject = obji, doComplete = FALSE, 
                  where = where)
        }
    }
    subclasses
}


.__C__table <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = FALSE
    , prototype = structure(integer(0), .Dim = 0L, .Dimnames = structure(list(NULL), .Names = ""), class = "table", .S3Class = "table")
    , validity = NULL
    , access = list()
    , className = structure("table", package = "methods")
    , package = "methods"
    , subclasses = structure(list(summaryDefault = S4_object()), .Names = "summaryDefault")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


getLoadActions <- function (where = topenv(parent.frame())) 
{
    actionListName <- .actionMetaName("")
    if (!exists(actionListName, envir = where, inherits = FALSE)) 
        return(list())
    actions <- get(actionListName, envir = where)
    if (length(actions)) {
        allExists <- sapply(actions, function(what) exists(.actionMetaName(what), 
            envir = where, inherits = FALSE))
        if (!all(allExists)) {
            warning(gettextf("some actions are missing: %s", 
                paste(actions[!allExists], collapse = ", ")), 
                domain = NA)
            actions <- actions[allExists]
        }
        allFuns <- lapply(actions, function(what) get(.actionMetaName(what), 
            envir = where))
        names(allFuns) <- actions
        allFuns
    }
    else list()
}


getClassDef <- function (Class, where = topenv(parent.frame()), package = packageSlot(Class), 
    inherits = TRUE, resolve.msg = getOption("getClass.msg", 
        default = TRUE)) 
{
    value <- if (inherits) 
        .getClassFromCache(Class, where, package = package, resolve.msg = resolve.msg)
    if (is.null(value)) {
        cname <- classMetaName(if (length(Class) > 1L) 
            Class[[1L]]
        else Class)
        if (identical(nzchar(package), TRUE)) {
            whereP <- .requirePackage(package)
            value <- get0(cname, whereP, inherits = inherits)
        }
        if (is.null(value)) 
            value <- get0(cname, where, inherits = inherits)
    }
    value
}


findClass <- function (Class, where = topenv(parent.frame()), unique = "") 
{
    if (is(Class, "classRepresentation")) {
        pkg <- Class@package
        classDef <- Class
        Class <- Class@className
    }
    else {
        pkg <- packageSlot(Class)
        if (is.null(pkg)) 
            pkg <- ""
        classDef <- getClassDef(Class, where, pkg)
    }
    where <- if (missing(where) && nzchar(pkg)) 
        .requirePackage(pkg)
    else as.environment(where)
    what <- classMetaName(Class)
    where <- .findAll(what, where)
    if (length(where) > 1L && nzchar(pkg)) {
        pkgs <- sapply(where, function(db) get(what, db)@package)
        where <- where[match(pkg, pkgs, 0L)]
    }
    else pkgs <- pkg
    if (length(where) == 0L) {
        if (is.null(classDef)) 
            classDef <- getClassDef(Class)
        if (nzchar(unique)) {
            if (is(classDef, "classRepresentation")) 
                stop(gettextf("class %s is defined, with package %s, but no corresponding metadata object was found (not exported?)", 
                  dQuote(Class), sQuote(classDef@package)), domain = NA)
            else stop(gettextf("no definition of %s to use for %s", 
                dQuote(Class), unique), domain = NA)
        }
    }
    else if (length(where) > 1L) {
        pkgs <- sapply(where, getPackageName, create = FALSE)
        where <- where[!(nzchar(pkgs) & duplicated(pkgs))]
        if (length(where) > 1L) 
            if (nzchar(unique)) {
                pkgs <- base::unique(pkgs)
                where <- where[1L]
                warning(sprintf(ngettext(length(pkgs), "multiple definition of class %s visible (%s); using the definition\n   in package %s for %s", 
                  "multiple definitions of class %s visible (%s); using the definition\n   in package %s for %s"), 
                  dQuote(Class), paste(sQuote(pkgs), collapse = ", "), 
                  sQuote(pkgs[[1L]]), unique), domain = NA)
            }
    }
    where
}


.__C__language <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = `<UNDEFINED>`
    , validity = NULL
    , access = list()
    , className = structure("language", package = "methods")
    , package = "methods"
    , subclasses = structure(list(name = S4_object(), 
    call = S4_object(), 
    `{` = S4_object(), 
    `if` = S4_object(), 
    `<-` = S4_object(), 
    `for` = S4_object(), 
    `while` = S4_object(), 
    `repeat` = S4_object(), 
    `(` = S4_object(), 
    .name = S4_object()), .Names = c("name", 
"call", "{", "if", "<-", "for", "while", "repeat", "(", ".name"
))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`.__T__coerce:methods` <- "<environment>"

`.__T__$<-:base` <- "<environment>"

.__C__anova.glm.null <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(anova.glm = S4_object(), 
    oldClass = S4_object()), .Names = c("anova.glm", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("anova.glm.null", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


registerImplicitGenerics <- function (what = .ImplicitGenericsTable(where), where = topenv(parent.frame())) 
{
    if (!is.environment(what)) 
        stop(gettextf("must provide an environment table; got class %s", 
            dQuote(class(what))), domain = NA)
    objs <- as.list(what, all.names = TRUE)
    mapply(.cacheImplicitGeneric, names(objs), objs)
    NULL
}


`.__T__body<-:base` <- "<environment>"

isClassDef <- function (object) 
is(object, "classRepresentation")


getPrototype <- function (ClassDef) 
.Defunct()


setValidity <- function (Class, method, where = topenv(parent.frame())) 
{
    if (isClassDef(Class)) {
        ClassDef <- Class
        Class <- ClassDef@className
    }
    else {
        ClassDef <- getClassDef(Class, where)
    }
    method <- .makeValidityMethod(Class, method)
    if (is.null(method) || (is(method, "function") && length(formalArgs(method)) == 
        1L)) 
        ClassDef@validity <- method
    else stop("validity method must be NULL or a function of one argument")
    assignClassDef(Class, ClassDef, where = where)
    resetClass(Class, ClassDef, where = where)
}


setPrimitiveMethods <- function (f, fdef, code, generic, mlist = get(".Methods", envir = environment(generic))) 
.Call(C_R_M_setPrimitiveMethods, f, fdef, code, generic, mlist)


is <- function (object, class2) 
{
    cl <- class(object)
    S3Case <- length(cl) > 1L
    if (S3Case) 
        cl <- cl[[1L]]
    if (missing(class2)) 
        return(extends(cl))
    class1Def <- getClassDef(cl)
    if (is.null(class1Def)) 
        return(inherits(object, class2))
    if (is.character(class2)) 
        class2Def <- getClassDef(class2, .classDefEnv(class1Def))
    else {
        class2Def <- class2
        class2 <- class2Def@className
    }
    S3Case <- S3Case || (is.object(object) && !isS4(object))
    S3Case <- S3Case && (is.null(class2Def) || class2 %in% .BasicClasses || 
        extends(class2Def, "oldClass"))
    if (S3Case) 
        inherits(object, class2)
    else if (.identC(cl, class2) || .identC(class2, "ANY")) 
        TRUE
    else if (is.logical(ext <- possibleExtends(cl, class2, class1Def, 
        class2Def))) 
        ext
    else if (ext@simple) 
        TRUE
    else ext@test(object)
}


`.__T__Logic:base` <- "<environment>"

Logic <- function (e1, e2) 
standardGeneric("Logic")


.S4methods <- function (generic.function, class) 
{
    info <- if (!missing(generic.function)) 
        .S4methodsForGeneric(generic.function, class)
    else if (!missing(class)) 
        .S4methodsForClass(generic.function, class)
    else stop("must supply 'generic.function' or 'class'")
    structure(rownames(info), info = info, byclass = missing(generic.function), 
        class = "MethodsFunction")
}


.__C__socket <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("socket", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


cacheMethod <- function (f, sig, def, args = names(sig), fdef, inherited = FALSE) 
{
    ev <- environment(fdef)
    .cacheMethodInTable(fdef, sig, def, .getMethodsTable(fdef, 
        ev, inherited = inherited))
    if (!inherited) 
        .cacheMethodInTable(fdef, sig, def, .getMethodsTable(fdef, 
            ev, inherited = TRUE))
}


`S3Part<-` <- function (object, strictS3 = FALSE, needClass = .S3Class(object), 
    value) 
{
    S3Class <- .S3Class(value)
    def <- getClassDef(S3Class[[1L]])
    if (is.null(def) || !extends(def, needClass[[1L]])) 
        stop(gettextf("replacement value must extend class %s, got %s", 
            dQuote(needClass), dQuote(S3Class[[1L]])), domain = NA)
    slots <- slotNames(class(object))
    if (!strictS3) {
        fromValue <- names(attributes(value))
        slots <- slots[is.na(match(slots, fromValue))]
    }
    slots <- c("class", slots)
    for (slot in slots) attr(value, slot) <- attr(object, slot)
    if (extends(def, "oldClass")) 
        attr(value, ".S3Class") <- S3Class
    if (isS4(object)) 
        value <- .asS4(value)
    value
}


asMethodDefinition <- function (def, signature = list(.anyClassName), sealed = FALSE, 
    fdef = def) 
{
    switch(typeof(def), builtin = , special = , `NULL` = return(def), 
        closure = {
        }, stop(gettextf("invalid object for formal method definition: type %s", 
            dQuote(typeof(def))), domain = NA))
    if (is(def, "MethodDefinition")) {
        value <- def
        if (missing(signature)) 
            signature <- value@defined
    }
    else value <- new("MethodDefinition", def)
    if (sealed) 
        value <- new("SealedMethodDefinition", value)
    if (is(signature, "signature")) 
        classes <- signature
    else classes <- .MakeSignature(new("signature"), def, signature, 
        fdef)
    value@target <- classes
    value@defined <- classes
    value
}


.__C__special <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = FALSE
    , prototype = .Primitive("if")
    , validity = NULL
    , access = list()
    , className = "special"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


representation <- function (...) 
{
    value <- list(...)
    anames <- allNames(value)
    for (i in seq_along(value)) {
        ei <- value[[i]]
        if (!is.character(ei) || length(ei) != 1L) 
            stop(gettextf("element %d of the representation was not a single character string", 
                i), domain = NA)
    }
    includes <- as.character(value[!nzchar(anames)])
    if (anyDuplicated(includes)) 
        stop(gettextf("duplicate class names among superclasses: %s", 
            paste(.dQ(includes[duplicated(includes)]), collapse = ", ")), 
            domain = NA)
    slots <- anames[nzchar(anames)]
    if (anyDuplicated(slots)) {
        dslots <- slots[duplicated(slots)]
        stop(sprintf(ngettext(length(dslots), "duplicated slot name: %s", 
            "duplicated slot names: %s"), paste(sQuote(dslots), 
            collapse = "")), domain = NA)
    }
    value
}


.__C__SealedMethodDefinition <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    target = structure("signature", package = "methods"), defined = structure("signature", package = "methods"), 
    generic = structure("character", package = "methods")), .Names = c(".Data", 
"target", "defined", "generic"))
    , contains = structure(list(MethodDefinition = S4_object(), 
    `function` = S4_object(), 
    PossibleMethod = S4_object(), 
    OptionalFunction = S4_object(), 
    optionalMethod = S4_object()), .Names = c("MethodDefinition", 
"function", "PossibleMethod", "OptionalFunction", "optionalMethod"
))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("SealedMethodDefinition", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


isSealedMethod <- function (f, signature, fdef = getGeneric(f, FALSE, where = where), 
    where = topenv(parent.frame())) 
{
    fGen <- getFunction(f, TRUE, FALSE, where = where)
    if (!is.primitive(fGen)) {
        mdef <- getMethod(f, signature, optional = TRUE, where = where, 
            fdef = fGen)
        return(is(mdef, "SealedMethodDefinition"))
    }
    if (is(fdef, "genericFunction")) 
        signature <- matchSignature(signature, fdef)
    if (length(signature) == 0L) 
        TRUE
    else if (f %in% .subsetFuns) 
        !any(is.na(match(signature, .BasicClasses)))
    else {
        sealed <- !is.na(match(signature[[1L]], .BasicClasses))
        if (sealed && (!is.na(match("Ops", c(f, getGroup(f, TRUE)))) || 
            !is.na(match(f, c("%*%", "crossprod"))))) 
            sealed <- sealed && (length(signature) > 1L) && !is.na(match(signature[[2L]], 
                .BasicClasses))
        sealed
    }
}


.__C__raw <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(vector = S4_object()), .Names = "vector")
    , virtual = FALSE
    , prototype = raw(0)
    , validity = NULL
    , access = list()
    , className = "raw"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


S3Part <- function (object, strictS3 = FALSE, S3Class) 
{
    if (!isS4(object)) 
        return(object)
    classDef <- getClass(class(object))
    oldClassCase <- extends(classDef, "oldClass")
    defltS3Class <- missing(S3Class)
    if (oldClassCase) {
        if (defltS3Class) 
            S3Class <- .S3Class(object)
        keepSlots <- slotNames(S3Class[[1L]])
    }
    else {
        if (all(is.na(match(extends(classDef), .BasicClasses)))) 
            stop(gettextf("S3Part() is only defined for classes set up by setOldCLass(), basic classes or subclasses of these:  not true of class %s", 
                dQuote(class(object))), domain = NA)
        if (missing(S3Class)) {
            S3Class <- classDef@slots$.Data
            if (is.null(S3Class)) 
                S3Class <- typeof(object)
            keepSlots <- character()
        }
        else keepSlots <- slotNames(S3Class[[1L]])
    }
    if (!(defltS3Class || extends(classDef, S3Class))) 
        stop(gettextf("the 'S3Class' argument must be a superclass of %s:  not true of class %s", 
            dQuote(class(object)), dQuote(S3Class)), domain = NA)
    if (strictS3) 
        keepSlots <- keepSlots[is.na(match(keepSlots, ".S3Class"))]
    deleteSlots = slotNames(classDef)
    deleteSlots <- deleteSlots[is.na(match(deleteSlots, keepSlots))]
    for (slot in deleteSlots) attr(object, slot) <- NULL
    if (strictS3) {
        object <- .notS4(object)
        class(object) <- S3Class
    }
    else class(object) <- S3Class[[1L]]
    object
}


.__C__environment <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(refObject = S4_object()), .Names = "refObject")
    , virtual = FALSE
    , prototype =  "<environment>"
    , validity = NULL
    , access = list()
    , className = "environment"
    , package = "methods"
    , subclasses = structure(list(.environment = S4_object(), 
    sourceEnvironment = S4_object(), 
    envRefClass = S4_object(), 
    refGeneratorSlot = S4_object(), 
    localRefClass = S4_object()), .Names = c(".environment", 
"sourceEnvironment", "envRefClass", "refGeneratorSlot", "localRefClass"
))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__integer <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(numeric = S4_object(), 
    vector = S4_object(), 
    data.frameRowLabels = S4_object()), .Names = c("numeric", 
"vector", "data.frameRowLabels"))
    , virtual = FALSE
    , prototype = integer(0)
    , validity = NULL
    , access = list()
    , className = "integer"
    , package = "methods"
    , subclasses = structure(list(factor = S4_object()), .Names = "factor")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


listFromMlist <- function (mlist, prefix = list(), sigs. = TRUE, methods. = TRUE) 
{
    methodSlot <- slot(mlist, "methods")
    mnames <- names(methodSlot)
    argName <- as.character(slot(mlist, "argument"))
    sigs <- list()
    methods <- list()
    for (i in seq_along(methodSlot)) {
        thisMethod <- methodSlot[i]
        thisClass <- mnames[[i]]
        prefix[[argName]] <- thisClass
        if (is.function(thisMethod)) {
            if (sigs.) 
                sigs <- c(sigs, list(prefix))
            if (methods.) 
                methods <- c(methods, list(thisMethod))
        }
        else {
            more <- Recall(thisMethod, prefix)
            if (sigs.) 
                sigs <- c(sigs, more[[1]])
            if (methods.) 
                methods <- c(methods, more[[2]])
        }
    }
    list(sigs, methods)
}


.__C__mts <- new("classRepresentation"
    , slots = structure(list(.Data = structure("vector", package = "methods"), 
    tsp = structure("numeric", package = "methods"), .S3Class = structure("character", package = "methods")), .Names = c(".Data", 
"tsp", ".S3Class"))
    , contains = structure(list(matrix = S4_object(), 
    ts = S4_object(), 
    array = S4_object(), 
    structure = S4_object(), 
    oldClass = S4_object(), 
    vector = S4_object()), .Names = c("matrix", 
"ts", "array", "structure", "oldClass", "vector"))
    , virtual = FALSE
    , prototype = new("matrix"
)
    , validity = NULL
    , access = list()
    , className = structure("mts", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.doTracePrint <- function (msg = "") 
{
    call <- deparse(sys.call(sys.parent(1)))
    if (length(call) > 1) 
        call <- paste(call[[1L]], "....")
    cat("Tracing", call, msg, "\n")
}


.OldClassesList <- list(c("anova", "data.frame"), c("mlm", "lm"), c("aov", "lm"), 
    c("maov", "mlm", "lm"), c("POSIXct", "POSIXt"), c("POSIXlt", 
    "POSIXt"), "Date", "dump.frames", c("glm.null", "glm", "lm"
    ), c("anova.glm.null", "anova.glm"), "hsearch", "integrate", 
    "packageInfo", "libraryIQR", "packageIQR", "mtable", c("summaryDefault", 
    "table"), "recordedplot", "socket", "packageIQR", "density", 
    "logLik", "rle")


.__C__standardGeneric <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    generic = structure("character", package = "methods"), package = structure("character", package = "methods"), 
    group = structure("list", package = "methods"), valueClass = structure("character", package = "methods"), 
    signature = structure("character", package = "methods"), 
    default = structure("optionalMethod", package = "methods"), 
    skeleton = structure("call", package = "methods")), .Names = c(".Data", 
"generic", "package", "group", "valueClass", "signature", "default", 
"skeleton"))
    , contains = structure(list(genericFunction = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("genericFunction", 
"function", "OptionalFunction", "PossibleMethod", "optionalMethod"
))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("standardGeneric", package = "methods")
    , package = "methods"
    , subclasses = structure(list(standardGenericWithTrace = S4_object()), .Names = "standardGenericWithTrace")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__MethodsList <- new("classRepresentation"
    , slots = structure(list(methods = structure("list", package = "methods"), 
    argument = structure("name", package = "methods"), allMethods = structure("list", package = "methods")), .Names = c("methods", 
"argument", "allMethods"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("MethodsList", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`.__T__Math2:methods` <- "<environment>"

promptMethods <- function (f, filename = NULL, methods) 
{
    escape <- function(txt) gsub("%", "\\\\%", txt)
    packageString <- ""
    fdef <- getGeneric(f)
    if (!isGeneric(f, fdef = fdef)) 
        stop(gettextf("no generic function found corresponding to %s", 
            sQuote(f)), domain = NA)
    if (missing(methods)) {
        methods <- findMethods(fdef)
        where <- .genEnv(fdef, topenv(parent.frame()))
        if (!identical(where, .GlobalEnv)) 
            packageString <- sprintf("in Package \\pkg{%s}", 
                getPackageName(where))
    }
    fullName <- utils:::topicName("methods", f)
    n <- length(methods)
    labels <- character(n)
    aliases <- character(n)
    signatures <- findMethodSignatures(methods = methods, target = TRUE)
    args <- colnames(signatures)
    for (i in seq_len(n)) {
        sigi <- signatures[i, ]
        labels[[i]] <- sprintf("\\code{signature(%s)}", paste(sprintf("%s = \"%s\"", 
            args, escape(sigi)), collapse = ", "))
        aliases[[i]] <- paste0("\\alias{", utils:::topicName("method", 
            c(f, signatures[i, ])), "}")
    }
    text <- paste0("\n\\item{", labels, "}{\n%%  ~~describe this method here~~\n}")
    text <- c("\\section{Methods}{\n\\describe{", text, "}}")
    aliasText <- c(paste0("\\alias{", escape(fullName), "}"), 
        escape(aliases))
    if (identical(filename, FALSE)) 
        return(c(aliasText, text))
    if (is.null(filename) || identical(filename, TRUE)) 
        filename <- paste0(fullName, ".Rd")
    Rdtxt <- list(name = paste0("\\name{", fullName, "}"), type = "\\docType{methods}", 
        aliases = aliasText, title = sprintf("\\title{ ~~ Methods for Function \\code{%s} %s ~~}", 
            f, packageString), description = paste0("\\description{\n ~~ Methods for function", 
            " \\code{", f, "} ", sub("^in Package", "in package", 
                packageString), " ~~\n}"), `section{Methods}` = text, 
        keywords = c("\\keyword{methods}", "\\keyword{ ~~ other possible keyword(s) ~~ }"))
    if (is.na(filename)) 
        return(Rdtxt)
    cat(unlist(Rdtxt), file = filename, sep = "\n")
    .message("A shell of methods documentation has been written", 
        .fileDesc(filename), ".\n")
    invisible(filename)
}


setPackageName <- function (pkg, env) 
assign(".packageName", pkg, envir = env)


.__C__groupGenericFunction <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    groupMembers = structure("list", package = "methods"), generic = structure("character", package = "methods"), 
    package = structure("character", package = "methods"), group = structure("list", package = "methods"), 
    valueClass = structure("character", package = "methods"), 
    signature = structure("character", package = "methods"), 
    default = structure("optionalMethod", package = "methods"), 
    skeleton = structure("call", package = "methods")), .Names = c(".Data", 
"groupMembers", "generic", "package", "group", "valueClass", 
"signature", "default", "skeleton"))
    , contains = structure(list(genericFunction = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("genericFunction", 
"function", "OptionalFunction", "PossibleMethod", "optionalMethod"
))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("groupGenericFunction", package = "methods")
    , package = "methods"
    , subclasses = structure(list(nonstandardGroupGenericFunction = S4_object(), 
    groupGenericFunctionWithTrace = S4_object()), .Names = c("nonstandardGroupGenericFunction", 
"groupGenericFunctionWithTrace"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


tryNew <- function (Class, where) 
{
    ClassDef <- getClassDef(Class, where)
    if (is.null(ClassDef)) 
        return(NULL)
    else if (identical(ClassDef@virtual, TRUE)) 
        ClassDef@prototype
    else tryCatch(new(ClassDef), error = function(e) {
        value <- ClassDef@prototype
        class(value) <- ClassDef@className
        value
    })
}


getSubclasses <- function (ClassDef) 
.Defunct()


.classEnv <- function (Class, default = .requirePackage("methods"), mustFind = TRUE) 
{
    package <- {
        if (is.character(Class)) 
            packageSlot(Class)
        else Class@package
    }
    if (is.null(package)) {
        value <- default
        def <- getClassDef(Class, value, NULL)
        if (is.null(def)) {
            value <- .GlobalEnv
            def <- getClassDef(Class, value, NULL)
            if (is.null(def)) {
                value <- .requirePackage("methods")
                if (!identical(default, value)) 
                  def <- getClassDef(Class, value, NULL)
            }
        }
        if (is.null(def) && mustFind) 
            stop(gettextf("unable to find an environment containing class %s", 
                dQuote(Class)), domain = NA)
        value
    }
    else .requirePackage(package)
}


`.__C__(` <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object()), .Names = "language")
    , virtual = FALSE
    , prototype = (NULL)
    , validity = NULL
    , access = list()
    , className = "("
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


Arith <- function (e1, e2) 
standardGeneric("Arith")


.__C__LinearMethodsList <- new("classRepresentation"
    , slots = structure(list(methods = structure("list", package = "methods"), 
    arguments = structure("list", package = "methods"), classes = structure("list", package = "methods"), 
    generic = structure("genericFunction", package = "methods")), .Names = c("methods", 
"arguments", "classes", "generic"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("LinearMethodsList", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__formula <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = FALSE
    , prototype = structure(list(), class = "formula", .Environment =  "<environment>", .S3Class = "formula")
    , validity = NULL
    , access = list()
    , className = structure("formula", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__$:base` <- "<environment>"

assignClassDef <- function (Class, def, where = .GlobalEnv, force = FALSE) 
{
    if (!is(def, "classRepresentation")) 
        stop(gettextf("trying to assign an object of class %s as the definition of class %s: must supply a \"classRepresentation\" object", 
            dQuote(class(def)), dQuote(Class)), domain = NA)
    clName <- def@className
    attributes(clName) <- NULL
    if (!.identC(Class, clName)) 
        stop(gettextf("assigning as %s a class representation with internal name %s", 
            dQuote(Class), dQuote(def@className)), domain = NA)
    where <- as.environment(where)
    mname <- classMetaName(Class)
    if (exists(mname, envir = where, inherits = FALSE) && bindingIsLocked(mname, 
        where)) {
        if (force) 
            .assignOverBinding(mname, def, where, FALSE)
        else stop(gettextf("class %s has a locked definition in package %s", 
            dQuote(Class), sQuote(getPackageName(where))))
    }
    else assign(mname, def, where)
    if (cacheOnAssign(where)) 
        .cacheClass(clName, def, is(def, "ClassUnionRepresentation"), 
            where)
}


.__C__ordered <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(factor = S4_object(), 
    oldClass = S4_object()), .Names = c("factor", 
"oldClass"))
    , virtual = FALSE
    , prototype = structure(integer(0), .Label = character(0), class = c("ordered", 
"factor"), .S3Class = c("ordered", "factor"))
    , validity = NULL
    , access = list()
    , className = structure("ordered", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


seemsS4Object <- function (object) 
.Defunct("isS4")


setMethod <- function (f, signature = character(), definition, where = topenv(parent.frame()), 
    valueClass = NULL, sealed = FALSE) 
{
    if (is.function(f) && is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
        gwhere <- .genEnv(f)
    }
    else if (is.function(f)) {
        if (is.primitive(f)) {
            f <- .primname(f)
            fdef <- genericForBasic(f)
            gwhere <- .genEnv(f)
        }
        else stop("a function for argument 'f' must be a generic function")
    }
    else {
        where <- as.environment(where)
        gwhere <- .genEnv(f, where)
        f <- switch(f, as.double = "as.numeric", f)
        fdef <- getGeneric(f, where = if (identical(gwhere, baseenv())) 
            where
        else gwhere)
    }
    if (.lockedForMethods(fdef, where)) 
        stop(gettextf("the environment %s is locked; cannot assign methods for function %s", 
            sQuote(getPackageName(where)), sQuote(f)), domain = NA)
    hasMethods <- !is.null(fdef)
    deflt <- getFunction(f, generic = FALSE, mustFind = FALSE, 
        where = where)
    if (identical(gwhere, baseenv())) {
        allWhere <- findFunction(f, where = where)
        generics <- logical(length(allWhere))
        if (length(allWhere)) {
            for (i in seq_along(allWhere)) {
                fi <- get(f, allWhere[[i]])
                geni <- is(fi, "genericFunction")
                generics[[i]] <- geni
                if (!geni && is.null(deflt)) 
                  deflt <- fi
            }
        }
        if (any(generics)) {
            gwhere <- as.environment(allWhere[generics][[1L]])
            if (.lockedForMethods(fdef, gwhere)) {
                if (identical(as.environment(where), gwhere)) 
                  stop(gettextf("the 'where' environment (%s) is a locked namespace; cannot assign methods there", 
                    getPackageName(where)), domain = NA)
                msg <- gettextf("Copying the generic function %s to environment %s, because the previous version was in a sealed namespace (%s)", 
                  sQuote(f), sQuote(getPackageName(where)), sQuote(getPackageName(gwhere)))
                message(strwrap(msg), domain = NA)
                assign(f, fdef, where)
                gwhere <- where
            }
        }
    }
    if (!hasMethods) 
        fdef <- deflt
    if (is.null(fdef)) 
        stop(gettextf("no existing definition for function %s", 
            sQuote(f)), domain = NA)
    if (!hasMethods) {
        setGeneric(f, where = where)
        fdef <- getGeneric(f, where = where)
        thisPackage <- getPackageName(where)
        thisPName <- if (identical(thisPackage, ".GlobalEnv")) 
            "the global environment"
        else paste("package", sQuote(thisPackage))
        if (identical(as.character(fdef@package), thisPackage)) 
            message(gettextf("Creating a generic function from function %s in %s", 
                sQuote(f), thisPName), domain = NA)
        else message(gettextf("Creating a generic function for %s from package %s in %s", 
            sQuote(f), sQuote(fdef@package), thisPName), domain = NA)
    }
    else if (identical(gwhere, NA)) {
        if (is.null(.BasicFunsList[[f]])) 
            stop(sprintf("apparent internal error: a generic function was found for \"%s\", but no corresponding object was found searching from \"%s\"", 
                f, getPackageName(where)), domain = NA)
        if (!isGeneric(f)) 
            setGeneric(f)
    }
    if (isSealedMethod(f, signature, fdef, where = where)) 
        stop(gettextf("the method for function %s and signature %s is sealed and cannot be re-defined", 
            sQuote(f), .signatureString(fdef, signature)), domain = NA)
    signature <- matchSignature(signature, fdef, where)
    createMethod <- FALSE
    switch(typeof(definition), closure = {
        fnames <- formalArgs(fdef)
        mnames <- formalArgs(definition)
        if (!identical(mnames, fnames)) {
            if (length(fnames) == length(mnames) && length(mnames) == 
                1L) {
                warning(gettextf("For function %s, signature %s: argument in method definition changed from (%s) to (%s)", 
                  sQuote(f), sQuote(signature), mnames, fnames), 
                  domain = NA, call. = FALSE)
                formals(definition) <- formals(fdef)
                ll <- list(as.name(formalArgs(fdef)))
                names(ll) <- mnames
                body(definition) <- substituteDirect(body(definition), 
                  ll)
                mnames <- fnames
            } else {
                fullSig <- conformMethod(signature, mnames, fnames, 
                  f, fdef, definition)
                if (!identical(fullSig, signature)) {
                  formals(definition, envir = environment(definition)) <- formals(fdef)
                  signature <- fullSig
                }
                definition <- rematchDefinition(definition, fdef, 
                  mnames, fnames, signature)
            }
        }
        definition <- matchDefaults(definition, fdef)
        createMethod <- TRUE
    }, builtin = , special = {
        if (!identical(definition, deflt)) stop("primitive functions cannot be methods; they must be enclosed in a regular function")
    }, `NULL` = {
    }, stop(gettextf("invalid method definition: expected a function, got an object of class %s", 
        dQuote(class(definition))), domain = NA))
    fenv <- environment(fdef)
    nSig <- .getGenericSigLength(fdef, fenv, TRUE)
    signature <- .matchSigLength(signature, fdef, fenv, TRUE)
    margs <- (fdef@signature)[seq_along(signature)]
    if (createMethod) {
        definition <- asMethodDefinition(definition, signature, 
            sealed, fdef)
        definition@generic <- fdef@generic
    }
    is.not.base <- !identical(where, baseenv())
    whereMethods <- if (is.not.base && !.noMlists()) 
        insertMethod(.getOrMakeMethodsList(f, where, fdef), signature, 
            margs, definition)
    mtable <- getMethodsForDispatch(fdef)
    if (cacheOnAssign(where)) {
        .cacheMethodInTable(fdef, signature, definition, mtable)
        .cacheMethodInTable(fdef, signature, definition)
        if (is.not.base) 
            .addToMetaTable(fdef, signature, definition, where, 
                nSig)
        resetGeneric(f, fdef, mtable, gwhere, deflt)
    }
    assignMethodsMetaData(f, whereMethods, fdef, where)
    f
}


.__C__SClassExtension <- new("classRepresentation"
    , slots = structure(list(subClass = structure("character", package = "methods"), 
    superClass = structure("character", package = "methods"), 
    package = structure("character", package = "methods"), coerce = structure("function", package = "methods"), 
    test = structure("function", package = "methods"), replace = structure("function", package = "methods"), 
    simple = structure("logical", package = "methods"), by = structure("character", package = "methods"), 
    dataPart = structure("logical", package = "methods"), distance = structure("numeric", package = "methods")), .Names = c("subClass", 
"superClass", "package", "coerce", "test", "replace", "simple", 
"by", "dataPart", "distance"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("SClassExtension", package = "methods")
    , package = "methods"
    , subclasses = structure(list(conditionalExtension = S4_object()), .Names = "conditionalExtension")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__expression <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(vector = S4_object()), .Names = "vector")
    , virtual = FALSE
    , prototype = expression()
    , validity = NULL
    , access = list()
    , className = "expression"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__classRepresentation <- new("classRepresentation"
    , slots = structure(list(slots = "list", contains = "list", virtual = "logical", 
    prototype = "ANY", validity = "OptionalFunction", access = "list", 
    className = "character", package = "character", subclasses = "list", 
    versionKey = "externalptr", sealed = "logical"), .Names = c("slots", 
"contains", "virtual", "prototype", "validity", "access", "className", 
"package", "subclasses", "versionKey", "sealed"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = NA
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = character(0)
    , package = character(0)
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)
    , validity = NULL
    , access = list()
    , className = structure("classRepresentation", package = "methods")
    , package = "methods"
    , subclasses = structure(list(ClassUnionRepresentation = S4_object(), 
    refClassRepresentation = S4_object()), .Names = c("ClassUnionRepresentation", 
"refClassRepresentation"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


setRefClass <- function (Class, fields = character(), contains = character(), 
    methods = list(), where = topenv(parent.frame()), inheritPackage = FALSE, 
    ...) 
{
    fields <- inferProperties(fields, "field")
    info <- refClassInformation(Class, contains, fields, methods, 
        where)
    superClasses <- refSuperClasses <- fieldClasses <- fieldPrototypes <- refMethods <- NULL
    for (what in c("superClasses", "refSuperClasses", "fieldClasses", 
        "fieldPrototypes", "refMethods")) assign(what, info[[what]])
    classFun <- setClass(Class, contains = superClasses, where = where, 
        ...)
    classDef <- new("refClassRepresentation", getClassDef(Class, 
        where = where), fieldClasses = fieldClasses, refMethods = as.environment(refMethods), 
        fieldPrototypes = as.environment(fieldPrototypes), refSuperClasses = refSuperClasses)
    .setObjectParent(classDef@refMethods, if (inheritPackage) 
        refSuperClasses
    else NULL, where)
    assignClassDef(Class, classDef, where)
    generator <- new("refGeneratorSlot")
    env <- as.environment(generator)
    env$def <- classDef
    env$className <- Class
    .declareVariables(classDef, where)
    value <- new("refObjectGenerator", classFun, generator = generator)
    invisible(value)
}


.__C__numeric <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(vector = S4_object()), .Names = "vector")
    , virtual = FALSE
    , prototype = numeric(0)
    , validity = NULL
    , access = list()
    , className = "numeric"
    , package = "methods"
    , subclasses = structure(list(integer = S4_object(), 
    factor = S4_object()), .Names = c("integer", 
"factor"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


evalSource <- function (source, package = "", lock = TRUE, cache = FALSE) 
{
    if (!nzchar(package)) 
        envp <- .GlobalEnv
    else {
        pstring <- paste("package", package, sep = ":")
        packageIsVisible <- pstring %in% search()
        if (packageIsVisible) {
            envp <- as.environment(pstring)
        }
        else {
            envp <- tryCatch(asNamespace(package), error = function(cond) NULL)
        }
        if (is.null(envp)) 
            stop(gettextf("package %s is not attached and no namespace found for it", 
                sQuote(package)), domain = NA)
    }
    env <- new("sourceEnvironment", new.env(parent = envp), packageName = package, 
        sourceFile = (if (is.character(source)) 
            source
        else ""))
    env$.packageName <- package
    setCacheOnAssign(env, cache)
    if (is(source, "character")) 
        for (text in source) sys.source(text, envir = env)
    else if (is(source, "connection")) 
        sys.source(source, envir = env)
    else if (!is(source, "environment")) 
        stop(gettextf("invalid 'source' argument: expected file names or a connection but got an object of class %s", 
            dQuote(class(source)[[1L]])), domain = NA)
    if (lock) 
        lockEnvironment(env, bindings = TRUE)
    env
}


`.__T__[:base` <- "<environment>"

rbind2 <- function (x, y, ...) 
standardGeneric("rbind2")


balanceMethodsList <- function (mlist, args, check = TRUE) 
{
    .MlistDeprecated("balanceMethodsList()")
    moreArgs <- args[-1L]
    if (length(moreArgs) == 0L) 
        return(mlist)
    methods <- mlist@methods
    if (check && length(methods)) {
        depth <- 0
        el <- methods[[1L]]
        while (is(el, "MethodsList")) {
            mm <- el@methods
            if (length(mm) == 0L) 
                break
            depth <- depth + 1L
            el <- mm[[1L]]
        }
        if (depth >= length(args)) 
            return(mlist)
    }
    for (i in seq_along(methods)) {
        el <- methods[[i]]
        if (is(el, "MethodsList")) 
            el <- Recall(el, moreArgs, FALSE)
        else {
            if (is(el, "MethodDefinition")) {
                el@target[moreArgs] <- "ANY"
                el@defined[moreArgs] <- "ANY"
            }
            for (what in rev(moreArgs)) el <- new("MethodsList", 
                argument = as.name(what), methods = list(ANY = el))
        }
        methods[[i]] <- el
    }
    mlist@methods <- methods
    mlist
}


.__C__VIRTUAL <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = structure("VIRTUAL", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


conformMethod <- function (signature, mnames, fnames, f = "<unspecified>", fdef, 
    method) 
{
    sig0 <- signature
    fsig <- fdef@signature
    if (is.na(match("...", mnames)) && !is.na(match("...", fnames))) 
        fnames <- fnames[-match("...", fnames)]
    imf <- match(fnames, mnames)
    omitted <- is.na(imf)
    if (is.unsorted(imf[!omitted])) 
        stop(.renderSignature(f, signature), "formal arguments in method and generic do not appear in the same order", 
            call. = FALSE)
    if (!any(omitted)) 
        return(signature)
    sigNames <- names(signature)
    omittedSig <- sigNames %in% fnames[omitted]
    if (!any(omittedSig)) 
        return(signature)
    if (any(is.na(match(signature[omittedSig], c("ANY", "missing"))))) {
        bad <- omittedSig & is.na(match(signature[omittedSig], 
            c("ANY", "missing")))
        bad2 <- paste0(fnames[bad], " = \"", signature[bad], 
            "\"", collapse = ", ")
        stop(.renderSignature(f, sig0), gettextf("formal arguments (%s) omitted in the method definition cannot be in the signature", 
            bad2), call. = TRUE, domain = NA)
    }
    else if (!all(signature[omittedSig] == "missing")) {
        omittedSig <- omittedSig && (signature[omittedSig] != 
            "missing")
        .message("Note: ", .renderSignature(f, sig0), gettextf("expanding the signature to include omitted arguments in definition: %s", 
            paste(sigNames[omittedSig], "= \"missing\"", collapse = ", ")))
        omittedSig <- seq_along(omittedSig)[omittedSig]
        signature[omittedSig] <- "missing"
    }
    n <- length(signature)
    while (.identC(signature[[n]], "ANY")) n <- n - 1L
    length(signature) <- n
    length(fsig) <- n
    setNames(signature, fsig)
}


getClassPackage <- function (ClassDef) 
.Defunct()


getClass <- function (Class, .Force = FALSE, where = .classEnv(Class, topenv(parent.frame()), 
    FALSE), resolve.msg = getOption("getClass.msg", default = TRUE)) 
{
    value <- .getClassFromCache(Class, where, resolve.msg = resolve.msg)
    if (is.null(value)) {
        value <- getClassDef(Class, where, resolve.msg = resolve.msg)
        if (is.null(value)) {
            if (!.Force) 
                stop(gettextf("%s is not a defined class", dQuote(Class)), 
                  domain = NA)
            else value <- makeClassRepresentation(Class, package = "base", 
                virtual = TRUE, where = where)
        }
    }
    value
}


.__C__complex <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(vector = S4_object()), .Names = "vector")
    , virtual = FALSE
    , prototype = complex(0)
    , validity = NULL
    , access = list()
    , className = "complex"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


getMethod <- function (f, signature = character(), where = topenv(parent.frame()), 
    optional = FALSE, mlist, fdef) 
{
    if (!missing(where)) {
        env <- .NamespaceOrEnvironment(where)
        if (is.null(env)) 
            stop(gettextf("no environment or package corresponding to argument where=%s", 
                deparse(where)), domain = NA)
        where <- env
    }
    if (missing(fdef)) {
        if (missing(where)) 
            fdef <- getGeneric(f, FALSE)
        else {
            fdef <- getGeneric(f, FALSE, where = where)
            if (is.null(fdef)) 
                fdef <- getGeneric(f, FALSE)
        }
    }
    if (!is(fdef, "genericFunction")) {
        if (optional) 
            return(NULL)
        if (!is.character(f)) 
            f <- deparse(substitute(f))
        stop(gettextf("no generic function found for '%s'", f), 
            domain = NA)
    }
    if (missing(mlist)) 
        mlist <- if (missing(where)) 
            getMethodsForDispatch(fdef)
        else .getMethodsTableMetaData(fdef, where, optional)
    if (is.environment(mlist)) {
        signature <- matchSignature(signature, fdef)
        value <- .findMethodInTable(signature, mlist, fdef)
        if (is.null(value) && !optional) {
            if (!is.character(f)) 
                f <- deparse(substitute(f))
            stop(gettextf("no method found for function '%s' and signature %s", 
                f, paste(signature, collapse = ", ")))
        }
        return(value)
    }
    else if (is.null(mlist)) 
        return(mlist)
    stop("defunct methods list search", domain = NA)
}


getGenerics <- function (where, searchForm = FALSE) 
{
    if (missing(where)) {
        fdefs <- as.list(.genericTable, all.names = TRUE, sorted = TRUE)
        fnames <- mapply(function(nm, obj) {
            if (is.list(obj)) 
                names(obj)
            else nm
        }, names(fdefs), fdefs, SIMPLIFY = FALSE)
        packages <- lapply(fdefs, .packageForGeneric)
        new("ObjectsWithPackage", unlist(fnames), package = unlist(packages))
    }
    else {
        if (is.environment(where)) 
            where <- list(where)
        these <- unlist(lapply(where, objects, all.names = TRUE), 
            use.names = FALSE)
        metaNameUndo(unique(these), prefix = "T", searchForm = searchForm)
    }
}


finalDefaultMethod <- function (method) 
{
    repeat {
        if (is.function(method) || is.null(method)) 
            break
        if (is(method, "MethodsList")) {
            .MlistDeprecated()
            method <- slot(method, "methods")[["ANY"]]
        }
        else stop(gettextf("default method must be a method definition, a primitive or NULL: got an object of class %s", 
            dQuote(class(method))), domain = NA)
    }
    method
}


showClass <- function (Class, complete = TRUE, propertiesAreCalled = "Slots") 
{
    if (isClassDef(Class)) {
        ClassDef <- Class
        Class <- ClassDef@className
    }
    else if (complete) 
        ClassDef <- getClass(Class)
    else ClassDef <- getClassDef(Class)
    cat(if (identical(ClassDef@virtual, TRUE)) 
        "Virtual ", "Class ", .dQ(Class), if (nzchar(pkg <- ClassDef@package)) 
        c(" [", if (pkg != ".GlobalEnv") "package" else "in", 
            " \"", pkg, "\"]"), "\n", sep = "")
    x <- ClassDef@slots
    if (length(x)) {
        printPropertiesList(x, propertiesAreCalled)
    }
    else cat("\nNo ", propertiesAreCalled, ", prototype of class \"", 
        .class1(ClassDef@prototype), "\"\n", sep = "")
    ext <- ClassDef@contains
    if (length(ext)) {
        cat("\nExtends: ")
        showExtends(ext)
    }
    ext <- ClassDef@subclasses
    if (length(ext)) {
        cat("\nKnown Subclasses: ")
        showExtends(ext)
    }
}


.__C__name <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object(), 
    refObject = S4_object()), .Names = c("language", 
"refObject"))
    , virtual = FALSE
    , prototype = `<UNDEFINED>`
    , validity = NULL
    , access = list()
    , className = "name"
    , package = "methods"
    , subclasses = structure(list(.name = S4_object()), .Names = ".name")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`elNamed<-` <- function (x, name, value) 
{
    x[[name]] <- value
    x
}


.__C__nonstandardGeneric <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = new("nonstandardGenericFunction"
    , .Data = function () 
NULL
    , generic = character(0)
    , package = character(0)
    , group = list()
    , valueClass = character(0)
    , signature = character(0)
    , default = NULL
    , skeleton = `<undef>`()
)
    , validity = NULL
    , access = list()
    , className = structure("nonstandardGeneric", package = "methods")
    , package = "methods"
    , subclasses = structure(list(nonstandardGenericFunction = S4_object(), 
    nonstandardGroupGenericFunction = S4_object(), 
    nonstandardGenericWithTrace = S4_object()), .Names = c("nonstandardGenericFunction", 
"nonstandardGroupGenericFunction", "nonstandardGenericWithTrace"
))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__POSIXct <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(POSIXt = S4_object(), 
    oldClass = S4_object()), .Names = c("POSIXt", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("POSIXct", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__factor <- new("classRepresentation"
    , slots = structure(list(.Data = structure("integer", package = "methods"), 
    levels = structure("character", package = "methods"), .S3Class = structure("character", package = "methods")), .Names = c(".Data", 
"levels", ".S3Class"))
    , contains = structure(list(integer = S4_object(), 
    oldClass = S4_object(), 
    numeric = S4_object(), 
    vector = S4_object(), 
    data.frameRowLabels = S4_object()), .Names = c("integer", 
"oldClass", "numeric", "vector", "data.frameRowLabels"))
    , virtual = FALSE
    , prototype = new("integer"
)
    , validity = function (object) 
{
    levs <- levels(object)
    if (!is.character(levs)) 
        return("factor levels must be \"character\"")
    if (d <- anyDuplicated(levs)) 
        return(sprintf("duplicated level [%d] in factor", d))
    TRUE
}
    , access = list()
    , className = structure("factor", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__libraryIQR <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("libraryIQR", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


empty.dump <- function () 
list()


`.__T__[[<-:base` <- "<environment>"

validSlotNames <- function (names) 
{
    if (is.na(match("class", names))) 
        names
    else stop("\"class\" is a reserved slot name and cannot be redefined")
}


`.__T__coerce<-:methods` <- "<environment>"

new <- function (Class, ...) 
{
    ClassDef <- getClass(Class, where = topenv(parent.frame()))
    value <- .Call(C_new_object, ClassDef)
    initialize(value, ...)
}


.__C__listOfMethods <- new("classRepresentation"
    , slots = structure(list(.Data = structure("list", package = "methods"), 
    arguments = structure("character", package = "methods"), 
    signatures = structure("list", package = "methods"), generic = structure("genericFunction", package = "methods"), 
    names = structure("character", package = "methods")), .Names = c(".Data", 
"arguments", "signatures", "generic", "names"))
    , contains = structure(list(namedList = S4_object(), 
    list = S4_object(), 
    vector = S4_object()), .Names = c("namedList", 
"list", "vector"))
    , virtual = FALSE
    , prototype = new("list"
)
    , validity = NULL
    , access = list()
    , className = structure("listOfMethods", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


existsFunction <- function (f, generic = TRUE, where = topenv(parent.frame())) 
length(findFunction(f, generic, where)) > 0L


`el<-` <- .Primitive("[[<-")


.__C__.externalptr <- new("classRepresentation"
    , slots = structure(list(.xData = structure("externalptr", package = "methods")), .Names = ".xData")
    , contains = structure(list(externalptr = S4_object(), 
    refObject = S4_object()), .Names = c("externalptr", 
"refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure(".externalptr", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__classPrototypeDef <- new("classRepresentation"
    , slots = structure(list(object = structure("ANY", package = "methods"), 
    slots = structure("character", package = "methods"), dataPart = structure("logical", package = "methods")), .Names = c("object", 
"slots", "dataPart"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("classPrototypeDef", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


getClasses <- function (where = .externalCallerEnv(), inherits = missing(where)) 
{
    pat <- paste0("^", classMetaName(""))
    if (!is.environment(where)) 
        where <- as.environment(where)
    if (inherits) {
        evList <- .parentEnvList(where)
        clNames <- character()
        for (ev in evList) clNames <- c(clNames, grep(pat, names(ev), 
            value = TRUE))
        clNames <- unique(clNames)
    }
    else clNames <- grep(pat, names(where), value = TRUE)
    substring(clNames, nchar(pat, "c"))
}


isVirtualClass <- function (Class, where = topenv(parent.frame())) 
{
    if (isClassDef(Class)) 
        Class@virtual
    else if (isClass(Class, where = where)) 
        getClass(Class, where = where)@virtual
    else TRUE
}


elNamed <- function (x, name, mustFind = FALSE) 
{
    i <- match(name, names(x))
    if (is.na(i)) {
        if (mustFind) 
            stop(gettextf("%s is not one of the element names", 
                sQuote(name)), domain = NA)
        else NULL
    }
    else x[[i]]
}


getDataPart <- function (object) 
{
    if (identical(typeof(object), "S4")) {
        value <- attr(object, ".Data")
        if (is.null(value)) {
            value <- attr(object, ".xData")
            if (is.null(value)) 
                stop("Data part is undefined for general S4 object")
        }
        if (identical(value, .pseudoNULL)) 
            return(NULL)
        else return(value)
    }
    temp <- getClass(class(object))@slots
    if (length(temp) == 0L) 
        return(object)
    if (is.na(match(".Data", names(temp)))) 
        stop(gettextf("no '.Data' slot defined for class %s", 
            dQuote(class(object))), domain = NA)
    dataPart <- temp[[".Data"]]
    switch(dataPart, numeric = , vector = , integer = , character = , 
        logical = , complex = , list = attributes(object) <- NULL, 
        matrix = , array = {
            value <- object
            attributes(value) <- NULL
            attr(value, "dim") <- attr(object, "dim")
            attr(value, "dimnames") <- attr(object, "dimnames")
            object <- value
        }, ts = {
            value <- object
            attributes(value) <- NULL
            attr(value, "ts") <- attr(object, "ts")
            object <- value
        }, if (is.na(match(dataPart, .BasicClasses))) {
            attrVals <- attributes(object)
            attrs <- names(attrVals)
            attrs <- attrs[is.na(match(attrs, c("class", names(temp))))]
            attributes(object) <- attrVals[attrs]
        } else attributes(object) <- NULL)
    object
}


.__C__mtable <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("mtable", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__anova <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(data.frame = S4_object(), 
    oldClass = S4_object()), .Names = c("data.frame", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("anova", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


sealClass <- function (Class, where = topenv(parent.frame())) 
{
    if (missing(where)) 
        where <- findClass(Class, unique = "sealing the class", 
            where = where)
    classDef <- getClassDef(Class, where)
    if (!classDef@sealed) {
        classDef@sealed <- TRUE
        assignClassDef(Class, classDef, where)
    }
    invisible(classDef)
}


.selectSuperClasses <- function (ext, dropVirtual = FALSE, namesOnly = TRUE, directOnly = TRUE, 
    simpleOnly = directOnly) 
{
    addCond <- function(xpr, prev) if (length(prev)) 
        substitute(P && N, list(P = prev, N = xpr))
    else xpr
    C <- if (dropVirtual) {
        isVirtualExt <- function(x) getClass(x@superClass)@virtual
        quote(!isVirtualExt(exti))
    }
    else expression()
    if (directOnly) 
        C <- addCond(quote(length(exti@by) == 0), C)
    if (simpleOnly) 
        C <- addCond(quote(exti@simple), C)
    if (length(C)) {
        F <- function(exti) {
        }
        body(F) <- C
        ext <- ext[unlist(lapply(ext, F), use.names = FALSE)]
    }
    if (namesOnly) 
        names(ext)
    else ext
}


getMethodsMetaData <- function (f, where = topenv(parent.frame())) 
{
    fdef <- getGeneric(f, where = where)
    if (is.null(fdef)) 
        return(NULL)
    if (.noMlists()) {
        warning(sprintf("Methods list objects are not maintained in this version of R:  request for function %s may return incorrect information", 
            sQuote(fdef@generic)), domain = NA)
    }
    mname <- methodsPackageMetaName("M", fdef@generic, fdef@package)
    if (exists(mname, where = where, inherits = missing(where))) 
        get(mname, where)
    else if (missing(where)) 
        .makeMlistFromTable(fdef)
    else .makeMlistFromTable(fdef, where)
}


.__C__maov <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(mlm = S4_object(), 
    aov = S4_object(), 
    lm = S4_object(), 
    oldClass = S4_object()), .Names = c("mlm", 
"aov", "lm", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("maov", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


isClassUnion <- function (Class) 
{
    if (is.character(Class)) 
        Class <- getClass(Class, TRUE)
    extends(class(Class), "ClassUnionRepresentation")
}


getClassName <- function (ClassDef) 
.Defunct()


`languageEl<-` <- function (object, which, value) 
{
    data <- as.list(object)
    n <- length(data)
    type <- typeof(object)
    if (type == "closure") {
        ev <- environment(object)
        if (is.character(which)) {
            if (is.na(match(which, names(data)))) {
                body <- data[[n]]
                data <- data[-n]
                data[[which]] <- value
                data[[n + 1]] <- body
            }
            else data[[which]] <- value
        }
        else {
            if (which < 1 || which > n) 
                stop("invalid index for function argument")
            data[[which]] <- value
        }
        object <- as.function(data)
        environment(object) <- ev
        object
    }
    else if (type == "language") {
        if (is.character(which)) 
            data[[which]] <- value
        else if (isGrammarSymbol(data[[1L]])) 
            data[[which + 1]] <- value
        else {
            if (identical(which, 1) && is.character(value)) 
                value <- as.symbol(value)
            data[[which]] <- value
        }
        as.call(data)
    }
    else {
        object[[which]] <- value
        object
    }
}


matchSignature <- function (signature, fun, where = baseenv()) 
{
    if (!is(fun, "genericFunction")) 
        stop(gettextf("trying to match a method signature to an object (of class %s) that is not a generic function", 
            dQuote(class(fun))), domain = NA)
    anames <- fun@signature
    if (length(signature) == 0L) 
        return(character())
    if (is(signature, "character")) {
        pkgs <- packageSlot(signature)
        if (is.null(pkgs)) 
            pkgs <- character(length(signature))
        else if (length(pkgs) != length(signature)) 
            stop("invalid 'package' slot or attribute, wrong length")
        sigClasses <- as.character(signature)
    }
    else if (is(signature, "list")) {
        sigClasses <- pkgs <- character(length(signature))
        for (i in seq_along(signature)) {
            cli <- signature[[i]]
            if (is(cli, "classRepresentation")) {
                sigClasses[[i]] <- cli@className
                pkgs[[i]] <- cli@package
            }
            else if (is(cli, "character") && length(cli) == 1) {
                sigClasses[[i]] <- cli
                pkgi <- packageSlot(cli)
                if (is.character(pkgi)) 
                  pkgs[[i]] <- pkgi
            }
            else stop(gettextf("invalid element in a list for \"signature\" argument; element %d is neither a class definition nor a class name", 
                i), domain = NA)
        }
    }
    else stop(gettextf("trying to match a method signature of class %s; expects a list or a character vector", 
        dQuote(class(signature))), domain = NA)
    if (!identical(where, baseenv())) {
        unknown <- !nzchar(pkgs)
        for (i in seq_along(sigClasses)[unknown]) {
            cli <- getClassDef(sigClasses[[i]], where)
            if (!is.null(cli)) {
                pkgs[[i]] <- cli@package
                unknown[[i]] <- FALSE
            }
        }
        if (any(unknown)) {
            unknown <- unique(sigClasses[unknown])
            MSG <- if (identical(as.vector(coerce@generic), "coerce") && 
                length(unknown) == 1) 
                message
            else function(...) warning(..., call. = FALSE)
            MSG(.renderSignature(fun@generic, signature), sprintf(ngettext(length(unknown), 
                "no definition for class %s", "no definition for classes %s"), 
                paste(dQuote(unknown), collapse = ", ")), domain = NA)
        }
    }
    signature <- as.list(signature)
    if (length(sigClasses) != length(signature)) 
        stop(gettextf("object to use as a method signature for function %s does not look like a legitimate signature (a vector of single class names): there were %d class names, but %d elements in the signature object", 
            sQuote(fun@generic), length(sigClasses), length(signature)), 
            domain = NA)
    if (is.null(names(signature))) {
        which <- seq_along(signature)
        if (length(which) > length(anames)) 
            stop(gettextf("more elements in the method signature (%d) than in the generic signature (%d) for function %s", 
                length(which), length(anames), sQuote(fun@generic)), 
                domain = NA)
    }
    else {
        sigList <- signature
        for (i in seq_along(sigList)) sigList[[i]] <- c(sigClasses[[i]], 
            pkgs[[i]])
        fcall <- do.call("call", c("fun", sigList))
        if (identical(anames, formalArgs(fun))) 
            smatch <- match.call(fun, fcall)
        else {
            fmatch <- fun
            ff <- as.list(anames)
            names(ff) <- anames
            formals(fmatch, envir = environment(fun)) <- ff
            smatch <- match.call(fmatch, fcall)
        }
        snames <- names(smatch)[-1L]
        which <- match(snames, anames)
        if (anyNA(which)) 
            stop(sprintf(ngettext(sum(is.na(which)), "in the method signature for function %s invalid argument name in the signature: %s", 
                "in the method signature for function %s invalid argument names in the signature: %s"), 
                sQuote(fun@generic), paste(snames[is.na(which)], 
                  collapse = ", ")), domain = NA)
        smatch <- smatch[-1]
        for (i in seq_along(smatch)) {
            eli <- smatch[[i]]
            sigClasses[[i]] <- eli[[1]]
            pkgs[[i]] <- eli[[2]]
        }
    }
    n <- length(anames)
    value <- rep("ANY", n)
    valueP <- rep("methods", n)
    names(value) <- anames
    value[which] <- sigClasses
    valueP[which] <- pkgs
    unspec <- value == "ANY"
    while (n > 1 && unspec[[n]]) n <- n - 1
    length(value) <- length(valueP) <- n
    attr(value, "package") <- valueP
    value
}


.EmptyPrimitiveSkeletons <- list(f(x), fgets(x, value = value))


signature <- function (...) 
{
    value <- list(...)
    names <- names(value)
    for (i in seq_along(value)) {
        sigi <- value[[i]]
        if (!is.character(sigi) || length(sigi) != 1L) 
            stop(gettextf("bad class specified for element %d (should be a single character string)", 
                i), domain = NA)
    }
    setNames(as.character(value), names)
}


loadMethod <- function (method, fname, envir) 
standardGeneric("loadMethod")


.__C__packageInfo <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("packageInfo", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`.__C__{` <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object()), .Names = "language")
    , virtual = FALSE
    , prototype = {
}
    , validity = NULL
    , access = list()
    , className = "{"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


evalOnLoad <- function (expr, where = topenv(parent.frame()), aname = "") 
{
    f <- function(env) NULL
    body(f, where) <- substitute(eval(EXPR, ENV), list(EXPR = expr, 
        ENV = where))
    setLoadAction(f, aname, where)
}


.__C__MethodWithNextWithTrace <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    nextMethod = structure("PossibleMethod", package = "methods"), 
    excluded = structure("list", package = "methods"), target = structure("signature", package = "methods"), 
    defined = structure("signature", package = "methods"), generic = structure("character", package = "methods"), 
    original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c(".Data", 
"nextMethod", "excluded", "target", "defined", "generic", "original", 
"source"))
    , contains = structure(list(MethodWithNext = S4_object(), 
    traceable = S4_object(), 
    MethodDefinition = S4_object(), 
    `function` = S4_object(), 
    PossibleMethod = S4_object(), 
    OptionalFunction = S4_object(), 
    optionalMethod = S4_object()), .Names = c("MethodWithNext", 
"traceable", "MethodDefinition", "function", "PossibleMethod", 
"OptionalFunction", "optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("MethodWithNextWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__dump.frames <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("dump.frames", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


fixPre1.8 <- function (names, where = topenv(parent.frame())) 
{
    done <- character()
    for (what in names) {
        objWhere <- .findAll(what, where)
        if (length(objWhere) == 0) {
            warning(gettextf("object %s not found", sQuote(what)), 
                domain = NA)
            next
        }
        objWhere <- objWhere[[1L]]
        obj <- get(what, objWhere)
        if (is.null(attr(obj, "class"))) 
            next
        Class <- class(obj)
        if (is.null(attr(Class, "package"))) {
            if (isClass(Class, where = where)) {
                ClassDef <- getClass(Class, where = where)
                ok <- !(isVirtualClass(ClassDef) || !isTRUE(validObject(obj, 
                  test = TRUE)))
                if (ok) {
                  class(obj) <- ClassDef@className
                  assign(what, obj, objWhere)
                  done <- c(done, what)
                }
                else warning(gettextf("object %s not changed (it is not consistent with the current definition of class %s from %s)", 
                  sQuote(what), dQuote(Class), sQuote(ClassDef@package)), 
                  domain = NA)
            }
            else warning(gettextf("no definition for the class of %s (class %s) found", 
                sQuote(what), dQuote(class)), domain = NA)
        }
        else warning(gettextf("object %s not changed (it does not appear to be from a version of R earlier than 1.8.0)", 
            sQuote(what)), domain = NA)
    }
    done
}


getMethodsForDispatch <- function (fdef, inherited = FALSE) 
{
    .getMethodsTable(fdef, environment(fdef), inherited = inherited)
}


setLoadAction <- function (action, aname = "", where = topenv(parent.frame())) 
{
    currentAnames <- .assignActionListNames(where)
    if (!nzchar(aname)) 
        aname <- paste0(".", length(currentAnames) + 1)
    .assignActions(list(action), aname, where)
    if (is.na(match(aname, currentAnames))) {
        actionListName <- .actionMetaName("")
        assign(actionListName, c(currentAnames, aname), envir = where)
    }
}


.TraceWithMethods <- function (what, tracer = NULL, exit = NULL, at = numeric(), print = TRUE, 
    signature = NULL, where = .GlobalEnv, edit = FALSE, from = NULL, 
    untrace = FALSE, classMethod = FALSE) 
{
    fromPackage <- if (is.function(where)) {
        where <- if (is(where, "genericFunction")) 
            parent.env(environment(where))
        else environment(where)
        getPackageName(where)
    }
    else ""
    doEdit <- !identical(edit, FALSE)
    whereF <- NULL
    pname <- character()
    def <- NULL
    tracingWhere <- "in package"
    refCase <- isS4(where) && (is(where, "envRefClass") || is(where, 
        "refClassRepresentation"))
    if (refCase) {
        if (!is.null(signature)) 
            stop("argument 'signature' is not meaningful for tracing reference methods")
        .where <- where
        if (is(.where, "refGeneratorSlot") && !classMethod) 
            .where <- .where$def
        if (is(.where, "refClassRepresentation")) {
            pname <- .where@className
            .where <- .where@refMethods
            tracingWhere <- "for class"
        }
        else {
            tracingWhere <- "for object from class"
            pname <- class(.where)
        }
        def <- eval(substitute(.dollarForEnvRefClass(.where, 
            what)))
        if (!is(def, "refMethodDef")) {
            thisName <- substitute(what)
            stop(gettextf("%s is not a method for reference class %s", 
                sQuote(as.character(if (is.symbol(thisName)) thisName else what)), 
                dQuote(class(where))), domain = NA)
        }
        what <- def@name
        whereF <- .where
    }
    else if (is.function(what)) {
        def <- what
        if (is(def, "genericFunction")) {
            what <- def@generic
            whereF <- .genEnv(what, where)
            pname <- def@package
        }
        else {
            fname <- substitute(what)
            if (is.name(fname)) {
                what <- as.character(fname)
                temp <- .findFunEnvAndName(what, where)
                whereF <- temp$whereF
                pname <- temp$pname
            }
            else if (is.call(fname) && identical(fname[[1L]], 
                as.name("::"))) {
                whereF <- as.character(fname[[2L]])
                require(whereF, character.only = TRUE)
                whereF <- as.environment(paste("package", whereF, 
                  sep = ":"))
                pname <- fname[[2L]]
                what <- as.character(fname[[3L]])
            }
            else if (is.call(fname) && identical(fname[[1L]], 
                as.name(":::"))) {
                pname <- paste(fname[[2L]], "(not-exported)")
                whereF <- loadNamespace(as.character(fname[[2L]]))
                what <- as.character(fname[[3L]])
            }
            else stop("argument 'what' should be the name of a function")
        }
    }
    else {
        what <- as(what, "character")
        if (length(what) != 1) {
            for (f in what) {
                if (nargs() == 1) 
                  trace(f)
                else Recall(f, tracer, exit, at, print, signature, 
                  where, edit, from, untrace)
            }
            return(what)
        }
        temp <- .findFunEnvAndName(what, where, signature)
        whereF <- temp$whereF
        pname <- temp$pname
        fname <- what
    }
    if (what %in% .InvalidTracedFunctions) 
        stop(gettextf("tracing the internal function %s is not allowed", 
            sQuote(what)), domain = NA)
    if (.traceTraceState) {
        message(".TraceWithMethods: after computing what, whereF", 
            domain = NA)
        browser()
    }
    if (nargs() == 1) 
        return(if (untrace) .primUntrace(what) else .primTrace(what))
    if (is.null(whereF)) {
        allWhere <- findFunction(what, where = where)
        if (length(allWhere) == 0) 
            stop(gettextf("no function definition for %s found", 
                sQuote(what)), domain = NA)
        whereF <- as.environment(allWhere[[1L]])
    }
    if (is.null(tracer) && is.null(exit) && identical(edit, FALSE)) 
        tracer <- quote({
        })
    if (is.null(def)) 
        def <- getFunction(what, where = whereF)
    if (is(def, "traceable") && identical(edit, FALSE) && !untrace) 
        def <- .untracedFunction(def)
    if (!is.null(signature)) {
        fdef <- if (!is(def, "genericFunction")) 
            getGeneric(as.character(fname), TRUE, where)
        else def
        def <- selectMethod(what, signature, fdef = fdef, optional = TRUE)
        if (is.null(def)) {
            warning(gettextf("cannot untrace method for %s; no method defined for this signature: %s", 
                sQuote(what), paste(signature, collapse = ", ")), 
                domain = NA)
            return(def)
        }
        signature <- def@target
    }
    if (untrace) {
        if (.traceTraceState) {
            message(".TraceWithMethods: untrace case", domain = NA)
            browser()
        }
        if (is.null(signature)) {
            if (is(def, "traceable")) {
                newFun <- .untracedFunction(def)
            }
            else {
                .primUntrace(what)
                return(what)
            }
        }
        else {
            if (is(def, "traceable")) 
                newFun <- .untracedFunction(def)
            else {
                warning(gettextf("the method for %s for this signature was not being traced", 
                  sQuote(what)), domain = NA)
                return(what)
            }
        }
    }
    else {
        if (!is.null(exit)) {
            if (is.function(exit)) {
                tname <- substitute(exit)
                if (is.name(tname)) 
                  exit <- tname
                exit <- substitute(TRACE(), list(TRACE = exit))
            }
        }
        if (!is.null(tracer)) {
            if (is.function(tracer)) {
                tname <- substitute(tracer)
                if (is.name(tname)) 
                  tracer <- tname
                tracer <- substitute(TRACE(), list(TRACE = tracer))
            }
        }
        original <- .untracedFunction(def)
        traceClass <- .traceClassName(class(original))
        if (is.null(getClassDef(traceClass))) 
            traceClass <- .makeTraceClass(traceClass, class(original))
        if (doEdit && is.environment(edit)) {
            def <- .findNewDefForTrace(what, signature, edit, 
                fromPackage)
            environment(def) <- environment(original)
            if (is.null(c(tracer, exit))) {
                newFun <- new(traceClass, original)
                newFun@.Data <- def
            }
            else {
                newFun <- new(traceClass, def = def, tracer = tracer, 
                  exit = exit, at = at, print = print, doEdit = FALSE)
                newFun@original <- original
            }
            newFun@source <- edit
        }
        else newFun <- new(traceClass, def = if (doEdit) 
            def
        else original, tracer = tracer, exit = exit, at = at, 
            print = print, doEdit = edit)
    }
    global <- identical(whereF, .GlobalEnv)
    if (.traceTraceState) {
        message(".TraceWithMethods: about to assign or setMethod", 
            domain = NA)
        browser()
    }
    if (is.null(signature)) {
        if (bindingIsLocked(what, whereF)) 
            .assignOverBinding(what, newFun, whereF, global)
        else assign(what, newFun, whereF)
        if (length(pname) != 0) {
            spname <- sub("^namespace:", "", pname)
            ipkgs <- tryCatch(getNamespaceUsers(spname), error = function(e) {
                c()
            })
            for (importingPkg in ipkgs) {
                .updateInImportsEnv(what, newFun, importingPkg)
            }
        }
        if (length(grep("[^.]+[.][^.]+", what)) > 0) {
            S3MTableName <- ".__S3MethodsTable__."
            if (!is.null(tbl <- get0(S3MTableName, envir = whereF, 
                inherits = FALSE))) {
                if (exists(what, envir = tbl, inherits = FALSE)) {
                  tracedFun <- get(what, envir = whereF, inherits = TRUE)
                  assign(what, tracedFun, envir = tbl)
                }
            }
        }
    }
    else {
        if (untrace && is(newFun, "MethodDefinition") && !identical(newFun@target, 
            newFun@defined)) 
            newFun <- NULL
        setMethod(fdef, signature, newFun, where = baseenv())
    }
    if (!global) {
        action <- if (untrace) 
            "Untracing"
        else "Tracing"
        nameSpaceCase <- FALSE
        location <- if (.identC(fromPackage, "")) {
            if (length(pname) == 0 && !is.null(whereF)) 
                pname <- getPackageName(whereF)
            nameSpaceCase <- isNamespace(whereF) && !is.na(match(pname, 
                loadedNamespaces())) && identical(whereF, getNamespace(pname))
            if (length(pname) == 0) 
                ""
            else {
                if (nameSpaceCase) 
                  paste0(" in environment <namespace:", pname, 
                    ">")
                else paste0(" ", tracingWhere, " \"", pname, 
                  "\"")
            }
        }
        else paste0(" as seen from package \"", fromPackage, 
            "\"")
        object <- if (refCase) 
            "reference method"
        else if (is.null(signature)) 
            "function"
        else "specified method for function"
        object <- paste0(" ", object, " \"", what, "\" ")
        .message(action, object, location)
        if (nameSpaceCase && !untrace && exists(what, envir = .GlobalEnv)) {
            untcall <- paste0("untrace(\"", what, "\", where = getNamespace(\"", 
                pname, "\"))")
            .message("Warning: Tracing only in the namespace; to untrace you will need:\n    ", 
                untcall, "\n")
        }
    }
    what
}


missingArg <- function (symbol, envir = parent.frame(), eval = FALSE) 
.Call(C_R_missingArg, if (eval) symbol else substitute(symbol), 
    envir)


externalRefMethod <- function (...) 
new("externalRefMethod", ...)


.__C__function <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(OptionalFunction = S4_object(), 
    PossibleMethod = S4_object()), .Names = c("OptionalFunction", 
"PossibleMethod"))
    , virtual = FALSE
    , prototype = function () 
NULL
    , validity = NULL
    , access = list()
    , className = "function"
    , package = "methods"
    , subclasses = structure(list(classGeneratorFunction = S4_object(), 
    MethodDefinition = S4_object(), 
    genericFunction = S4_object(), 
    functionWithTrace = S4_object(), 
    activeBindingFunction = S4_object(), 
    refMethodDef = S4_object(), 
    derivedDefaultMethod = S4_object(), 
    MethodWithNext = S4_object(), 
    SealedMethodDefinition = S4_object(), 
    MethodDefinitionWithTrace = S4_object(), 
    standardGeneric = S4_object(), 
    nonstandardGenericFunction = S4_object(), 
    groupGenericFunction = S4_object(), 
    genericFunctionWithTrace = S4_object(), 
    defaultBindingFunction = S4_object(), 
    internalDispatchMethod = S4_object(), 
    MethodWithNextWithTrace = S4_object(), 
    derivedDefaultMethodWithTrace = S4_object(), 
    nonstandardGroupGenericFunction = S4_object(), 
    standardGenericWithTrace = S4_object(), 
    groupGenericFunctionWithTrace = S4_object(), 
    refMethodDefWithTrace = S4_object(), 
    externalRefMethod = S4_object(), 
    refObjectGenerator = S4_object()), .Names = c("classGeneratorFunction", 
"MethodDefinition", "genericFunction", "functionWithTrace", "activeBindingFunction", 
"refMethodDef", "derivedDefaultMethod", "MethodWithNext", "SealedMethodDefinition", 
"MethodDefinitionWithTrace", "standardGeneric", "nonstandardGenericFunction", 
"groupGenericFunction", "genericFunctionWithTrace", "defaultBindingFunction", 
"internalDispatchMethod", "MethodWithNextWithTrace", "derivedDefaultMethodWithTrace", 
"nonstandardGroupGenericFunction", "standardGenericWithTrace", 
"groupGenericFunctionWithTrace", "refMethodDefWithTrace", "externalRefMethod", 
"refObjectGenerator"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__envRefClass <- new("refClassRepresentation"
    , fieldClasses = list()
    , fieldPrototypes =  "<environment>"
    , refMethods =  "<environment>"
    , refSuperClasses = character(0)
    , slots = structure(list(.xData = structure("environment", package = "methods")), .Names = ".xData")
    , contains = structure(list(.environment = S4_object(), 
    refClass = S4_object(), 
    environment = S4_object(), 
    refObject = S4_object()), .Names = c(".environment", 
"refClass", "environment", "refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("envRefClass", package = "methods")
    , package = "methods"
    , subclasses = structure(list(refGeneratorSlot = S4_object(), 
    localRefClass = S4_object()), .Names = c("refGeneratorSlot", 
"localRefClass"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


defaultPrototype <- function () 
.defaultPrototype


.__C__glm <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(lm = S4_object(), 
    oldClass = S4_object()), .Names = c("lm", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("glm", package = "methods")
    , package = "methods"
    , subclasses = structure(list(glm.null = S4_object()), .Names = "glm.null")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


insertSource <- function (source, package = "", functions = allPlainObjects(), 
    methods = (if (missing(functions)) allMethodTables() else NULL), 
    force = missing(functions) & missing(methods)) 
{
    MPattern <- .TableMetaPattern()
    CPattern <- .ClassMetaPattern()
    allPlainObjects <- function() allObjects[!(grepl(MPattern, 
        allObjects) | grepl(CPattern, allObjects) | ".cacheOnAssign" == 
        allObjects)]
    allMethodTables <- function() allObjects[grepl(MPattern, 
        allObjects)]
    differs <- function(f1, f2) !(identical(body(f1), body(f2)) && 
        identical(args(f1), args(f2)))
    if (is.environment(source) && !nzchar(package)) {
        if (is(source, "sourceEnvironment")) 
            package <- source@packageName
        else if (exists(".packageName", envir = source, inherits = FALSE)) 
            package <- get(".packageName", envir = source)
    }
    if (is(source, "environment")) 
        env <- source
    else env <- evalSource(source, package, FALSE)
    envPackage <- getPackageName(env, FALSE)
    envp <- parent.env(env)
    if (identical(envp, .GlobalEnv) || !nzchar(envPackage)) {
        if (!nzchar(package)) 
            package <- .guessPackageName(env)
        if (identical(package, ".GlobalEnv")) 
            envns <- NULL
        else {
            pname <- paste0("package:", package)
            envp <- tryCatch(as.environment(pname), error = function(cond) NULL)
            if (is.null(envp)) {
                envp <- tryCatch(as.environment(pname), error = function(cond) NULL)
                if (is.null(envp)) 
                  stop(gettextf("cannot find an environment corresponding to package name '%s\"", 
                    package), domain = NA)
            }
            envns <- tryCatch(asNamespace(package), error = function(cond) NULL)
        }
        if (nzchar(package)) 
            assign(".packageName", package, envir = env)
    }
    else {
        if (isNamespace(envp)) 
            envns <- envp
        else envns <- tryCatch(asNamespace(package), error = function(cond) NULL)
    }
    if (nzchar(envPackage) && envPackage != package) 
        warning(gettextf("supplied package, %s, differs from package inferred from source, %s", 
            sQuote(package), sQuote(envPackage)), domain = NA)
    packageSlot(env) <- package
    allObjects <- names(env)
    if (!missing(functions)) {
        notThere <- is.na(match(functions, allObjects))
        if (any(notThere)) {
            warning(gettextf("cannot insert these (not found in source): %s", 
                paste("\"", functions[notThere], "\"", sep = "", 
                  collapse = ", ")), domain = NA)
        }
    }
    .mnames <- allMethodTables()
    if (length(methods) > 0) {
        notThere <- vapply(methods, function(fname) length(grep(fname, 
            .mnames, fixed = TRUE)) == 0, NA)
        if (any(notThere)) {
            warning(gettextf("cannot insert methods for these functions (methods table not found in source): %s", 
                paste("\"", methods[notThere], "\"", sep = "", 
                  collapse = ", ")), domain = NA)
            methods <- methods[!notThere]
        }
        methodNames <- vapply(methods, function(fname) .mnames[[grep(fname, 
            .mnames, fixed = TRUE)[[1]]]], "")
    }
    else {
        methodNames <- .mnames
        methods <- sub(.TableMetaPrefix(), "", methodNames)
        methods <- sub(":.*", "", methods)
    }
    notTraceable <- newObjects <- objectsDone <- character()
    for (i in seq_along(functions)) {
        this <- functions[[i]]
        thisWhere <- NULL
        if (is.null(envns) || exists(this, envir = envp, inherits = FALSE)) {
            envwhere <- envp
            thisWhere <- get(this, envir = envp)
        }
        else {
            envwhere <- envns
            if (is.environment(envns) && exists(this, envir = envns, 
                inherits = FALSE)) 
                thisWhere <- get(this, envir = envns)
        }
        thisObj <- get(this, envir = env)
        if (is.function(thisObj) && is.function(thisWhere) && 
            differs(thisObj, thisWhere)) {
            suppressMessages(.TraceWithMethods(this, where = envwhere, 
                edit = env))
            objectsDone <- c(objectsDone, this)
        }
        else if (force) 
            assign(this, thisObj, envir = envwhere)
        else if (!is.function(thisObj)) 
            notTraceable <- c(notTraceable, this)
        else if (is.null(thisWhere)) 
            newObjects <- c(newObjects, this)
    }
    if (length(notTraceable) > 0) 
        message(gettextf("Non-function objects are not currently inserted (not traceable): %s", 
            paste(notTraceable, collapse = ", ")), domain = NA)
    if (length(newObjects) > 0) 
        message(gettextf("New functions are not currently inserted (not untraceable): %s", 
            paste(newObjects, collapse = ", ")), domain = NA)
    if (length(objectsDone) > 0) 
        message(gettextf("Modified functions inserted through trace(): %s", 
            paste(objectsDone, collapse = ", ")), domain = NA)
    for (i in seq_along(methods)) {
        .copyMethods(methods[[i]], methodNames[[i]], env, envp)
    }
    if (!is.environment(source)) {
        lockEnvironment(env, bindings = TRUE)
        invisible(env)
    }
    else invisible(source)
}


dumpMethod <- function (f, signature = character(), file = defaultDumpName(f, 
    signature), where = topenv(parent.frame()), def = getMethod(f, 
    signature, where = where, optional = TRUE)) 
{
    if (!is.function(def)) 
        def <- getMethod(f, character(), where = where, optional = TRUE)
    closeit <- TRUE
    isSTDOUT <- FALSE
    if (is.character(file)) {
        if (!(isSTDOUT <- file == "")) 
            file <- file(file, "w")
    }
    else if (inherits(file, "connection")) {
        if (!isOpen(file)) 
            open(file, "w")
        else closeit <- FALSE
    }
    else stop("'file' must be a character string or a connection")
    if (!isSTDOUT) {
        sink(file)
        on.exit({
            sink()
            if (closeit) close(file)
        })
    }
    cat("setMethod(\"", f, "\", ", deparse(signature), ",\n", 
        sep = "")
    dput(def@.Data)
    cat(")\n", sep = "")
    if (!isSTDOUT) {
        on.exit()
        sink()
        if (closeit) 
            close(file)
    }
    invisible(file)
}


.__C__if <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object()), .Names = "language")
    , virtual = FALSE
    , prototype = if (NA) TRUE else FALSE
    , validity = NULL
    , access = list()
    , className = "if"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__oldClass <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(), .Names = character(0))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("oldClass", package = "methods")
    , package = "methods"
    , subclasses = structure(list(data.frame = S4_object(), 
    table = S4_object(), 
    summary.table = S4_object(), 
    formula = S4_object(), 
    lm = S4_object(), 
    POSIXt = S4_object(), 
    Date = S4_object(), 
    dump.frames = S4_object(), 
    anova.glm = S4_object(), 
    hsearch = S4_object(), 
    integrate = S4_object(), 
    packageInfo = S4_object(), 
    libraryIQR = S4_object(), 
    mtable = S4_object(), 
    recordedplot = S4_object(), 
    socket = S4_object(), 
    packageIQR = S4_object(), 
    density = S4_object(), 
    logLik = S4_object(), 
    rle = S4_object(), 
    summaryDefault = S4_object(), 
    mlm = S4_object(), 
    aov = S4_object(), 
    glm = S4_object(), 
    POSIXct = S4_object(), 
    POSIXlt = S4_object(), 
    anova.glm.null = S4_object(), 
    maov = S4_object(), 
    glm.null = S4_object()), .Names = c("data.frame", 
"table", "summary.table", "formula", "lm", "POSIXt", "Date", 
"dump.frames", "anova.glm", "hsearch", "integrate", "packageInfo", 
"libraryIQR", "mtable", "recordedplot", "socket", "packageIQR", 
"density", "logLik", "rle", "summaryDefault", "mlm", "aov", "glm", 
"POSIXct", "POSIXlt", "anova.glm.null", "maov", "glm.null"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


coerce <- function (from, to, strict = TRUE) 
{
    if (TRUE) {
        warning("direct use of coerce() is deprecated:  use as(from, class(to)) instead", 
            domain = NA)
        return(as(from, class(to), strict = strict))
    }
    standardGeneric("coerce")
}


hasArg <- function (name) 
{
    aname <- as.character(substitute(name))
    fnames <- names(formals(sys.function(sys.parent())))
    if (is.na(match(aname, fnames))) {
        if (is.na(match("...", fnames))) 
            FALSE
        else {
            dotsCall <- eval(quote(substitute(list(...))), sys.parent())
            !is.na(match(aname, names(dotsCall)))
        }
    }
    else eval(substitute(!missing(name)), sys.frame(sys.parent()))
}


.__C__refGeneratorSlot <- new("refClassRepresentation"
    , fieldClasses = structure(list(def = "ANY", className = "ANY"), .Names = c("def", 
"className"))
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
    , className = structure("refGeneratorSlot", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


testInheritedMethods <- function (f, signatures, test = TRUE, virtual = FALSE, groupMethods = TRUE, 
    where = .GlobalEnv) 
{
    .relevantClasses <- function(classes, excludeVirtual, where, 
        doinheritance) {
        classDefs <- lapply(classes, getClassDef, where)
        undefs <- vapply(classDefs, is.null, NA)
        if (any(undefs)) {
            .undefClasses <<- unique(c(.undefClasses, classes[undefs]))
            classes <- classes[!undefs]
            classDefs <- classDefs[!undefs]
        }
        if (doinheritance) {
            allSubs <- lapply(classDefs, function(what) names(what@subclasses))
            allSubs <- unique(unlist(allSubs))
            pattern <- sapply(allSubs, .matchSubsPattern, classes, 
                excludeVirtual)
            if (excludeVirtual) {
                excl <- nzchar(pattern)
                pattern <- pattern[excl]
                allSubs <- allSubs[excl]
            }
            if (length(allSubs) > 0) 
                allSubs <- sapply(split(allSubs, pattern), `[[`, 
                  1)
            else allSubs <- character()
        }
        else allSubs <- character()
        iAny <- match("ANY", classes, 0)
        if (iAny > 0) {
            classes[[iAny]] <- ".Other"
            classDefs[[iAny]] <- getClassDef(".Other")
        }
        if (excludeVirtual) 
            classes <- classes[vapply(classDefs, function(def) identical(def@virtual, 
                FALSE), NA)]
        unique(c(classes, allSubs))
    }
    if (!is(f, "genericFunction")) 
        f <- getGeneric(f)
    fname <- f@generic
    if (missing(signatures)) {
        mdefs <- findMethods(f)
        mnames <- names(mdefs)
        sigs <- findMethodSignatures(methods = mdefs)
        if (groupMethods) {
            groups <- getGroup(f, recursive = TRUE)
            for (group in groups) {
                fg <- getGeneric(group)
                mg <- findMethods(fg)
                sigsg <- findMethodSignatures(methods = mg)
                newSigs <- is.na(match(names(mg), mnames))
                mg <- mg[newSigs]
                mdefs <- c(mdefs, mg[newSigs])
                sigs <- rbind(sigs, sigsg[newSigs, ])
                mnames <- c(mnames, names(mg)[newSigs])
            }
        }
        if (length(sigs) == 0) 
            return(new("MethodSelectionReport", generic = fname))
        ok <- if (fname %in% c("coerce", "coerce<-")) 
            match(colnames(sigs), "from", 0) > 0
        else rep.int(TRUE, ncol(sigs))
        for (j in seq_len(ncol(sigs))) {
            classesj <- unique(sigs[, j])
            .undefClasses <- character()
            subclasses <- .relevantClasses(classesj, !virtual, 
                where, ok[[j]])
            nj <- length(subclasses)
            if (j > 1) {
                subclasses <- rep(subclasses, rep.int(ncomb, 
                  nj))
                ncomb <- ncomb * nj
                sigLabels <- paste(rep(sigLabels, times = nj), 
                  subclasses, sep = "#")
            }
            else {
                sigLabels <- subclasses
                ncomb <- nj
            }
            if (length(.undefClasses)) {
                warning(gettextf("undefined classes (%s) will be ignored for argument '%s'", 
                  paste0("\"", unique(.undefClasses), "\"", collapse = ", "), 
                  colnames(sigs)[[j]]), domain = NA)
                .undefClasses <- character()
            }
        }
        signatures <- strsplit(sigLabels, "#", fixed = TRUE)
    }
    else if (is(signatures, "matrix") && identical(typeof(signatures), 
        "character") && ncol(signatures) <= length(f@signature)) {
        siglist <- vector("list", nrow(signatures))
        for (i in seq_len(nrow(signatures))) siglist[[i]] <- signatures[i, 
            ]
        signatures <- siglist
    }
    else stop("argument 'signatures' must be a character matrix whose rows are method signatures")
    ambig_target <- character()
    ambig_candidates <- list()
    ambig_selected <- character()
    ambig_note <- character()
    if (test) {
        warninghandler <- function(cond) {
            ambig_target <<- c(ambig_target, attr(cond, "target"))
            ambig_candidates <<- c(ambig_candidates, list(attr(cond, 
                "candidates")))
            ambig_selected <<- c(ambig_selected, attr(cond, "selected"))
            ambig_note <<- c(ambig_note, attr(cond, "note"))
        }
        ambigOpt <- options(ambiguousMethodSelection = warninghandler)
        on.exit(options(ambigOpt))
        doSelect <- function(sig) {
            x <- selectMethod(f = f, sig, optional = TRUE)
            if (is(x, "MethodDefinition")) {
                nsig <- x@defined
                if (length(nsig) < length(sig)) 
                  c(nsig, rep("ANY", length(sig) - length(nsig)))
                else nsig
            }
            else if (is.null(x)) 
                rep_len("<NONE>", length(sig))
            else rep_len("ANY", length(sig))
        }
        signatures <- lapply(signatures, doSelect)
    }
    signatures <- sapply(signatures, paste0, collapse = "#")
    names(signatures) <- sigLabels
    new("MethodSelectionReport", generic = fname, allSelections = signatures, 
        target = ambig_target, selected = ambig_selected, candidates = ambig_candidates, 
        note = ambig_note)
}


insertClassMethods <- function (methods, Class, value, fieldNames, returnAll) 
{
    theseMethods <- names(value)
    prevMethods <- names(methods)
    allMethods <- unique(c(theseMethods, prevMethods))
    returnMethods <- if (returnAll) 
        methods
    else value
    check <- TRUE
    for (method in theseMethods) {
        prevMethod <- methods[[method]]
        if (is.null(prevMethod)) {
            superClassMethod <- if (identical(method, "initialize")) 
                "initFields"
            else ""
        }
        else if (identical(prevMethod@refClassName, Class)) 
            superClassMethod <- prevMethod@superClassMethod
        else {
            superClassMethod <- superClassMethodName(prevMethod)
            returnMethods[[superClassMethod]] <- prevMethod
        }
        def <- makeClassMethod(value[[method]], method, Class, 
            superClassMethod, allMethods)
        check <- check && .checkFieldsInMethod(def, fieldNames, 
            allMethods)
        returnMethods[[method]] <- def
    }
    if (is.na(check) && .methodsIsLoaded()) 
        message(gettextf("code for methods in class %s was not checked for suspicious field assignments (recommended package %s not available?)", 
            dQuote(Class), sQuote("codetools")), domain = NA)
    returnMethods
}


.__C__call <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object()), .Names = "language")
    , virtual = FALSE
    , prototype = `<undef>`()
    , validity = NULL
    , access = list()
    , className = "call"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__data.frame <- new("classRepresentation"
    , slots = structure(list(.Data = structure("list", package = "methods"), 
    names = structure("character", package = "methods"), row.names = structure("data.frameRowLabels", package = "methods"), 
    .S3Class = structure("character", package = "methods")), .Names = c(".Data", 
"names", "row.names", ".S3Class"))
    , contains = structure(list(list = S4_object(), 
    oldClass = S4_object(), 
    vector = S4_object()), .Names = c("list", 
"oldClass", "vector"))
    , virtual = FALSE
    , prototype = new("list"
)
    , validity = NULL
    , access = list()
    , className = structure("data.frame", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


Math <- function (x) 
standardGeneric("Math")


.__C__for <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object()), .Names = "language")
    , virtual = FALSE
    , prototype = for (NAME in logical()) NULL
    , validity = NULL
    , access = list()
    , className = "for"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


removeMethods <- function (f, where = topenv(parent.frame()), all = missing(where)) 
{
    fdef <- getGeneric(f, where = where)
    if (!is(fdef, "genericFunction")) {
        warning(gettextf("%s is not an S4 generic function in %s; methods not removed", 
            sQuote(f), sQuote(getPackageName(where))), domain = NA)
        return(FALSE)
    }
    methods <- getMethodsForDispatch(fdef)
    default <- getMethod(fdef, "ANY", optional = TRUE)
    fMetaName <- .TableMetaName(fdef@generic, fdef@package)
    oldMetaName <- methodsPackageMetaName("M", fdef@generic, 
        fdef@package)
    allWhere <- .findAll(fMetaName, where)
    if (!all) 
        allWhere <- allWhere[1L]
    value <- rep(TRUE, length(allWhere))
    cacheGenericsMetaData(f, fdef, FALSE, where)
    .uncacheGeneric(f, fdef)
    doGeneric <- TRUE
    for (i in seq_along(allWhere)) {
        db <- as.environment(allWhere[[i]])
        if (environmentIsLocked(db)) {
            warning(gettextf("cannot remove methods for %s in locked environment/package %s", 
                sQuote(f), sQuote(getPackageName(db))), domain = NA)
            value[[i]] <- FALSE
            next
        }
        if (exists(fMetaName, db, inherits = FALSE)) {
            theseMethods <- get(fMetaName, db)
            .mergeMethodsTable(fdef, methods, theseMethods, FALSE)
            rm(list = fMetaName, pos = db)
            if (exists(oldMetaName, db, inherits = FALSE)) 
                rm(list = oldMetaName, pos = db)
        }
    }
    all <- all && base::all(value)
    for (i in seq_along(allWhere)) {
        db <- as.environment(allWhere[[i]])
        if (doGeneric && isGeneric(f, db)) {
            if (all && is(default, "derivedDefaultMethod")) {
                default <- as(default, "function")
                rm(list = f, pos = db)
                if (!existsFunction(f, FALSE, db)) {
                  message(gettextf("Restoring default function definition of %s", 
                    sQuote(f)), domain = NA)
                  assign(f, default, db)
                }
            }
            else {
                resetGeneric(f, fdef, where = db, deflt = default)
            }
            doGeneric <- FALSE
        }
    }
    any(value)
}


.__C__array <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(structure = S4_object(), 
    vector = S4_object()), .Names = c("structure", 
"vector"))
    , virtual = FALSE
    , prototype = structure(numeric(0), .Dim = 0L)
    , validity = NULL
    , access = list()
    , className = "array"
    , package = "methods"
    , subclasses = structure(list(matrix = S4_object(), 
    mts = S4_object()), .Names = c("matrix", 
"mts"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


getPackageName <- function (where = topenv(parent.frame()), create = TRUE) 
{
    env <- as.environment(where)
    notSaved <- is.null(pkg <- get0(".packageName", env, inherits = FALSE))
    if (notSaved) {
        pkg <- if (identical(where, 1) || identical(env, topenv(parent.frame()))) 
            Sys.getenv("R_PACKAGE_NAME")
        else ""
    }
    envName <- environmentName(env)
    if (nzchar(envName) && regexpr("package:", envName, fixed = TRUE) == 
        1L) 
        pkg <- .rmpkg(envName)
    if (!nzchar(pkg)) {
        if (identical(env, .GlobalEnv)) 
            pkg <- ".GlobalEnv"
        else if (identical(env, .BaseNamespaceEnv)) 
            pkg <- "base"
        else {
            if (is.numeric(where)) 
                pkg <- search()[[where]]
            else if (is.environment(where)) {
                for (db in search()) if (identical(as.environment(db), 
                  where)) {
                  pkg <- db
                  break
                }
            }
            else if (nzchar(envName)) 
                pkg <- envName
            else pkg <- as.character(where)
            pkg <- .rmpkg(pkg)
        }
    }
    if (!nzchar(pkg)) {
        top <- topenv(env)
        if (!identical(top, env)) {
            pkg <- getPackageName(top, create = create)
        }
    }
    if (!nzchar(pkg) && create) {
        pkg <- as.character(Sys.time())
        warning(gettextf("Created a package name, %s, when none found", 
            sQuote(pkg)), domain = NA)
        assign(pkg, env, envir = .PackageEnvironments)
        if (notSaved && !environmentIsLocked(env)) 
            setPackageName(pkg, env)
    }
    pkg
}


.__C__MethodDefinition <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    target = structure("signature", package = "methods"), defined = structure("signature", package = "methods"), 
    generic = structure("character", package = "methods")), .Names = c(".Data", 
"target", "defined", "generic"))
    , contains = structure(list(`function` = S4_object(), 
    PossibleMethod = S4_object(), 
    OptionalFunction = S4_object()), .Names = c("function", 
"PossibleMethod", "OptionalFunction"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("MethodDefinition", package = "methods")
    , package = "methods"
    , subclasses = structure(list(derivedDefaultMethod = S4_object(), 
    MethodWithNext = S4_object(), 
    SealedMethodDefinition = S4_object(), 
    MethodDefinitionWithTrace = S4_object(), 
    internalDispatchMethod = S4_object(), 
    MethodWithNextWithTrace = S4_object(), 
    derivedDefaultMethodWithTrace = S4_object()), .Names = c("derivedDefaultMethod", 
"MethodWithNext", "SealedMethodDefinition", "MethodDefinitionWithTrace", 
"internalDispatchMethod", "MethodWithNextWithTrace", "derivedDefaultMethodWithTrace"
))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__nonstandardGenericWithTrace <- new("classRepresentation"
    , slots = structure(list(original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c("original", 
"source"))
    , contains = structure(list(nonstandardGeneric = S4_object(), 
    traceable = S4_object()), .Names = c("nonstandardGeneric", 
"traceable"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("nonstandardGenericWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


rematchDefinition <- function (definition, generic, mnames, fnames, signature) 
{
    added <- any(is.na(match(mnames, fnames)))
    keepsDots <- !is.na(match("...", mnames))
    if (!added && keepsDots) {
        formals(definition, envir = environment(definition)) <- formals(generic)
        return(definition)
    }
    dotsPos <- match("...", fnames)
    if (added && is.na(dotsPos)) 
        stop(gettextf("methods can add arguments to the generic %s only if '...' is an argument to the generic", 
            sQuote(generic@generic)), call. = TRUE)
    useNames <- !is.na(imf <- match(fnames, mnames)) | fnames == 
        "..."
    newCall <- lapply(c(".local", fnames[useNames]), as.name)
    if (is.unsorted(imf[!is.na(imf)])) 
        stop(.renderSignature(generic@generic, signature), "formal arguments in method and generic do not appear in the same order", 
            call. = FALSE)
    if (keepsDots && dotsPos < length(fnames)) {
        ntrail <- length(fnames) - dotsPos
        trailingArgs <- fnames[seq.int(to = length(fnames), length.out = ntrail)]
        if (!identical(mnames[seq.int(to = length(mnames), length.out = ntrail)], 
            trailingArgs)) 
            stop(gettextf("arguments (%s) after '...' in the generic must appear in the method, in the same place at the end of the argument list", 
                paste(trailingArgs, collapse = ", ")), call. = TRUE, 
                domain = NA)
        newCallNames <- character(length(newCall))
        newCallNames[seq.int(to = length(newCallNames), length.out = ntrail)] <- trailingArgs
        names(newCall) <- newCallNames
    }
    newCall <- as.call(newCall)
    newBody <- substitute({
        .local <- DEF
        NEWCALL
    }, list(DEF = definition, NEWCALL = newCall))
    generic <- .copyMethodDefaults(generic, definition)
    body(generic, envir = environment(definition)) <- newBody
    generic
}


insertMethod <- function (mlist, signature, args, def, cacheOnly = FALSE) 
{
    .MlistDeprecated("insertMethod()")
    if (.noMlists() && !identical(unique(signature), "ANY")) 
        return(mlist)
    if (identical(args[1L], "...") && !identical(names(signature), 
        "...")) {
        if (identical(signature[[1L]], "ANY")) 
            stop(gettextf("inserting method with invalid signature matching argument '...' to class %s", 
                dQuote(signature[[1L]])), domain = NA)
        args <- args[-1L]
        signature <- signature[-1L]
        if (length(signature) == 0L) 
            return(mlist)
    }
    if (length(signature) == 0L) 
        stop("inserting method corresponding to empty signature")
    if (!is(mlist, "MethodsList")) 
        stop(gettextf("inserting method into non-methods-list object (class %s)", 
            dQuote(.class1(mlist))), domain = NA)
    if (length(args) > 1 && !cacheOnly) 
        mlist <- balanceMethodsList(mlist, args)
    Class <- signature[[1]]
    methods <- if (cacheOnly) 
        mlist@allMethods
    else mlist@methods
    current <- methods[[Class]]
    if (is(current, "MethodsList")) {
        nextArg <- as.character(current@argument)
        sigArgs <- args
        n <- length(signature)
        length(sigArgs) <- n
        if (is.na(match(nextArg, sigArgs))) {
            n <- match(nextArg, args) - n
            if (is.na(n)) {
                n <- 1
                args <- c(args, nextArg)
            }
            signature <- c(signature, rep("ANY", n))
        }
    }
    if (length(signature) == 1) {
        if (is.null(current)) {
            if (!is.null(def)) 
                methods[[Class]] <- def
        }
        else {
            which <- match(Class, names(methods))
            if (is.null(def)) 
                methods <- methods[-which]
            else methods[[which]] <- def
        }
    }
    else {
        if (is.null(current)) 
            current <- new("MethodsList", argument = as.name(args[2L]))
        else if (is.function(current)) 
            current <- new("MethodsList", argument = as.name(args[2L]), 
                methods = list(ANY = current))
        methods[[Class]] <- Recall(current, signature[-1L], args[-1L], 
            def, cacheOnly)
    }
    mlist@allMethods <- methods
    if (!cacheOnly) 
        mlist@methods <- methods
    mlist
}


findMethods <- function (f, where, classes = character(), inherited = FALSE, 
    package = "") 
{
    if (is(f, "genericFunction")) {
        fdef <- f
        f <- fdef@generic
    }
    else if (.isSingleString(f)) {
        if (missing(where)) 
            fdef <- getGeneric(f, package = package)
        else {
            fdef <- getGeneric(f, where = where, package = package)
            if (is.null(fdef)) 
                fdef <- getGeneric(f, package = package)
        }
    }
    else if (!is(f, "function")) 
        stop(gettextf("argument %s must be a generic function or a single character string; got an object of class %s", 
            sQuote("f"), dQuote(class(f))), domain = NA)
    else {
        fdef <- f
        f <- deparse(substitute(f))
    }
    if (!is(fdef, "genericFunction")) {
        warning(gettextf("non-generic function '%s' given to findMethods()", 
            f), domain = NA)
        return(list())
    }
    object <- new("listOfMethods", arguments = fdef@signature, 
        generic = fdef)
    if (missing(where)) 
        table <- get(if (inherited) 
            ".AllMTable"
        else ".MTable", envir = environment(fdef))
    else {
        if (!identical(inherited, FALSE)) 
            stop(gettextf("only FALSE is meaningful for 'inherited', when 'where' is supplied (got %s)", 
                inherited), domain = NA)
        where <- as.environment(where)
        what <- .TableMetaName(f, fdef@package)
        if (is.null(table <- where[[what]])) 
            return(object)
    }
    objNames <- sort(names(table))
    if (length(classes)) {
        classesPattern <- paste0("#", classes, "#", collapse = "|")
        which <- grep(classesPattern, paste0("#", objNames, "#"))
        objNames <- objNames[which]
    }
    object@.Data <- mget(objNames, table)
    object@names <- objNames
    object@signatures <- strsplit(objNames, "#", fixed = TRUE)
    object
}


mergeMethods <- function (m1, m2, genericLabel = character()) 
{
    .MlistDeprecated("mergeMethods()")
    if (length(genericLabel) && is(m2, "MethodsList")) 
        m2 <- .GenericInPrimitiveMethods(m2, genericLabel)
    if (is.null(m1) || is(m1, "EmptyMethodsList")) 
        return(m2)
    tmp <- listFromMlist(m2)
    sigs <- tmp[[1]]
    methods <- tmp[[2]]
    for (i in seq_along(sigs)) {
        sigi <- sigs[[i]]
        if (.noMlists() && !identical(unique(sigi), "ANY")) 
            next
        args <- names(sigi)
        m1 <- insertMethod(m1, as.character(sigi), args, methods[[i]], 
            FALSE)
    }
    m1
}


.__C__POSIXt <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("POSIXt", package = "methods")
    , package = "methods"
    , subclasses = structure(list(POSIXct = S4_object(), 
    POSIXlt = S4_object()), .Names = c("POSIXct", 
"POSIXlt"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`coerce<-` <- function (from, to, value) 
{
    if (TRUE) {
        warning("direct use of coerce() is deprecated:  use as(from, class(to)) <- value instead", 
            domain = NA)
        return(`as<-`(from, class(to), value))
    }
    standardGeneric("coerce<-")
}


unRematchDefinition <- function (definition) 
{
    bdy <- body(definition)
    if (.identC(class(bdy), "{") && length(bdy) > 1L) {
        bdy <- bdy[[2L]]
        if (.identC(class(bdy), "<-") && identical(bdy[[2L]], 
            as.name(".local"))) 
            definition <- bdy[[3L]]
    }
    definition
}


.__C__refObjectGenerator <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    generator = structure("refGeneratorSlot", package = "methods"), 
    className = structure("character", package = "methods"), 
    package = structure("character", package = "methods")), .Names = c(".Data", 
"generator", "className", "package"))
    , contains = structure(list(classGeneratorFunction = S4_object(), 
    refClass = S4_object(), 
    `function` = S4_object(), 
    refObject = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("classGeneratorFunction", 
"refClass", "function", "refObject", "OptionalFunction", "PossibleMethod", 
"optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("refObjectGenerator", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__Date <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("Date", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__missing <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = structure("missing", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


callGeneric <- function (...) 
{
    call <- sys.call(sys.parent(1L))
    .local <- identical(call[[1L]], quote(.local))
    methodCtxInd <- 1L + if (.local) 
        1L
    else 0L
    callerCtxInd <- methodCtxInd + 1L
    methodCall <- sys.call(sys.parent(methodCtxInd))
    if (fromNextMethod(methodCall)) {
        methodCtxInd <- methodCtxInd + 1L
    }
    methodFrame <- parent.frame(methodCtxInd)
    genericName <- getGenericFromCall(methodCall, methodFrame)
    if (is.null(genericName)) {
        stop("callGeneric() must be called from within a method body")
    }
    if (nargs() == 0L) {
        callerFrame <- sys.frame(sys.parent(callerCtxInd))
        methodDef <- sys.function(sys.parent(1L))
        call <- match.call(methodDef, methodCall, expand.dots = FALSE, 
            envir = callerFrame)
        call[-1L] <- lapply(names(call[-1L]), as.name)
    }
    else {
        call <- sys.call()
    }
    call[[1L]] <- as.name(genericName)
    eval(call, parent.frame())
}


.__C__externalptr <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(refObject = S4_object()), .Names = "refObject")
    , virtual = FALSE
    , prototype = <pointer: (nil)>
    , validity = NULL
    , access = list()
    , className = "externalptr"
    , package = "methods"
    , subclasses = structure(list(.externalptr = S4_object()), .Names = ".externalptr")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`.__T__initialize:methods` <- "<environment>"

.__C__derivedDefaultMethodWithTrace <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    target = structure("signature", package = "methods"), defined = structure("signature", package = "methods"), 
    generic = structure("character", package = "methods"), original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c(".Data", 
"target", "defined", "generic", "original", "source"))
    , contains = structure(list(derivedDefaultMethod = S4_object(), 
    traceable = S4_object(), 
    MethodDefinition = S4_object(), 
    `function` = S4_object(), 
    PossibleMethod = S4_object(), 
    OptionalFunction = S4_object(), 
    optionalMethod = S4_object()), .Names = c("derivedDefaultMethod", 
"traceable", "MethodDefinition", "function", "PossibleMethod", 
"OptionalFunction", "optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("derivedDefaultMethodWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__ANY <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = structure("ANY", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


mlistMetaName <- function (name = "", package = "") 
.Defunct()


existsMethod <- function (f, signature = character(), where = topenv(parent.frame())) 
{
    if (missing(where)) 
        method <- getMethod(f, signature, optional = TRUE)
    else method <- getMethod(f, signature, where = where, optional = TRUE)
    !is.null(method)
}


showExtends <- function (ext, printTo = stdout()) 
{
    what <- names(ext)
    how <- character(length(ext))
    for (i in seq_along(ext)) {
        eli <- el(ext, i)
        if (is(eli, "SClassExtension")) {
            how[i] <- if (length(eli@by)) 
                paste("by class", paste0("\"", eli@by, "\", distance ", 
                  eli@distance, collapse = ", "))
            else if (identical(eli@dataPart, TRUE)) 
                "from data part"
            else "directly"
            if (!eli@simple) {
                if (is.function(eli@test) && !identical(body(eli@test), 
                  TRUE)) {
                  how[i] <- paste(how[i], if (is.function(eli@coerce)) 
                    ", with explicit test and coerce"
                  else ", with explicit test", sep = "")
                }
                else if (is.function(eli@coerce)) 
                  how[i] <- paste0(how[i], ", with explicit coerce")
            }
        }
    }
    if (identical(printTo, FALSE)) 
        list(what = what, how = how)
    else if (all(!nzchar(how)) || all(how == "directly")) {
        what <- paste0("\"", what, "\"")
        if (length(what) > 1L) 
            what <- c(paste0(what[-length(what)], ","), what[[length(what)]])
        cat(file = printTo, what, fill = TRUE)
    }
    else cat(file = printTo, "\n", paste0("Class \"", what, "\", ", 
        how, "\n"), sep = "")
}


showMlist <- function (mlist, includeDefs = TRUE, inherited = TRUE, classes = NULL, 
    useArgNames = TRUE, printTo = stdout()) 
{
    .MlistDeprecated("showMlist()")
    if (identical(printTo, FALSE)) {
        tmp <- tempfile()
        con <- file(tmp, "w")
    }
    else con <- printTo
    object <- linearizeMlist(mlist, inherited)
    methods <- object@methods
    signatures <- object@classes
    args <- object@arguments
    if (!is.null(classes) && length(signatures) > 0) {
        keep <- !vapply(signatures, function(x, y) all(is.na(match(x, 
            y))), NA, classes)
        methods <- methods[keep]
        signatures <- signatures[keep]
        args <- args[keep]
    }
    if (length(methods) == 0) 
        cat(file = con, "<Empty Methods List>\n")
    else {
        n <- length(methods)
        labels <- character(n)
        if (useArgNames) {
            for (i in 1L:n) {
                sigi <- signatures[[i]]
                labels[[i]] <- paste(args[[i]], " = \"", sigi, 
                  "\"", sep = "", collapse = ", ")
            }
        }
        else {
            for (i in 1L:n) labels[[i]] <- paste(signatures[[i]], 
                collapse = ", ")
        }
        for (i in seq_along(methods)) {
            cat(file = con, (if (includeDefs) 
                "## Signature:"
            else ""), labels[[i]])
            method <- methods[[i]]
            if (includeDefs) {
                cat(file = con, ":\n")
                if (is(method, "MethodDefinition")) 
                  cat(file = con, deparse(method@.Data), sep = "\n")
                else cat(file = con, deparse(method), sep = "\n")
            }
            if (is(method, "MethodDefinition") && !identical(method@target, 
                method@defined)) {
                defFrom <- method@defined
                cat(file = con, if (includeDefs) 
                  "##:"
                else "\n", "    (inherited from ", paste(names(defFrom), 
                  " = \"", as.character(defFrom), "\"", sep = "", 
                  collapse = ", "), ")", if (includeDefs) 
                  "\n", sep = "")
            }
            cat(file = con, "\n")
        }
    }
    if (identical(printTo, FALSE)) {
        close(con)
        value <- readLines(tmp)
        unlink(tmp)
        value
    }
}


MethodsList <- function (.ArgName, ...) 
{
    .MlistDeprecated("MethodsList()")
    value <- makeMethodsList(list(...))
    if (is.name(.ArgName)) {
    }
    else if (is.character(.ArgName) && length(.ArgName) == 1) 
        .ArgName <- as.name(.ArgName)
    else stop("invalid first argument: should be the name of the first argument in the dispatch")
    slot(value, "argument") <- .ArgName
    value
}


getVirtual <- function (ClassDef) 
.Defunct()


makeStandardGeneric <- function (f, fdef) 
{
    fgen <- fdef
    body(fgen) <- substitute(standardGeneric(FNAME), list(FNAME = f))
    if (typeof(fdef) != "closure") {
        fgen <- genericForBasic(f)
        message(gettextf("making a generic for special function %s", 
            sQuote(f)), domain = NA)
        setPrimitiveMethods(f, fdef, "reset", fgen, NULL)
    }
    fgen
}


.__C__className <- new("classRepresentation"
    , slots = structure(list(.Data = structure("character", package = "methods"), 
    package = structure("character", package = "methods")), .Names = c(".Data", 
"package"))
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
    , className = structure("className", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__character <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(vector = S4_object(), 
    data.frameRowLabels = S4_object(), 
    SuperClassMethod = S4_object()), .Names = c("vector", 
"data.frameRowLabels", "SuperClassMethod"))
    , virtual = FALSE
    , prototype = character(0)
    , validity = NULL
    , access = list()
    , className = "character"
    , package = "methods"
    , subclasses = structure(list(signature = S4_object(), 
    className = S4_object(), 
    ObjectsWithPackage = S4_object()), .Names = c("signature", 
"className", "ObjectsWithPackage"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


initFieldArgs <- function (.Object, classDef, selfEnv, ...) 
initRefFields(.Object, classDef, selfEnv, list(...))


.__C__genericFunction <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    generic = structure("character", package = "methods"), package = structure("character", package = "methods"), 
    group = structure("list", package = "methods"), valueClass = structure("character", package = "methods"), 
    signature = structure("character", package = "methods"), 
    default = structure("optionalMethod", package = "methods"), 
    skeleton = structure("call", package = "methods")), .Names = c(".Data", 
"generic", "package", "group", "valueClass", "signature", "default", 
"skeleton"))
    , contains = structure(list(`function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("function", 
"OptionalFunction", "PossibleMethod", "optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("genericFunction", package = "methods")
    , package = "methods"
    , subclasses = structure(list(standardGeneric = S4_object(), 
    nonstandardGenericFunction = S4_object(), 
    groupGenericFunction = S4_object(), 
    genericFunctionWithTrace = S4_object(), 
    nonstandardGroupGenericFunction = S4_object(), 
    standardGenericWithTrace = S4_object(), 
    groupGenericFunctionWithTrace = S4_object()), .Names = c("standardGeneric", 
"nonstandardGenericFunction", "groupGenericFunction", "genericFunctionWithTrace", 
"nonstandardGroupGenericFunction", "standardGenericWithTrace", 
"groupGenericFunctionWithTrace"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__signature <- new("classRepresentation"
    , slots = structure(list(.Data = structure("character", package = "methods"), 
    names = structure("character", package = "methods"), package = structure("character", package = "methods")), .Names = c(".Data", 
"names", "package"))
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
    , className = structure("signature", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__uninitializedField <- new("classRepresentation"
    , slots = structure(list(field = structure("character", package = "methods"), 
    className = structure("character", package = "methods")), .Names = c("field", 
"className"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("uninitializedField", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.untracedFunction <- function (f) 
{
    while (is(f, "traceable")) f <- f@original
    f
}


`.__T__Ops:base` <- "<environment>"

.__C__while <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object()), .Names = "language")
    , virtual = FALSE
    , prototype = while (FALSE) NULL
    , validity = NULL
    , access = list()
    , className = "while"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


validObject <- function (object, test = FALSE, complete = FALSE) 
{
    Class <- class(object)
    classDef <- getClassDef(Class)
    where <- .classEnv(classDef)
    anyStrings <- function(x) if (identical(x, TRUE)) 
        character()
    else x
    errors <- character()
    slotTypes <- classDef@slots
    slotNames <- names(slotTypes)
    attrNames <- c(".Data", ".S3Class", names(attributes(object)))
    if (any(is.na(match(slotNames, attrNames)))) {
        badSlots <- is.na(match(slotNames, attrNames))
        errors <- c(errors, paste("slots in class definition but not in object:", 
            paste0("\"", slotNames[badSlots], "\"", collapse = ", ")))
        slotTypes <- slotTypes[!badSlots]
        slotNames <- slotNames[!badSlots]
    }
    for (i in seq_along(slotTypes)) {
        classi <- slotTypes[[i]]
        classDefi <- getClassDef(classi, where = where)
        if (is.null(classDefi)) {
            errors <- c(errors, paste0("undefined class for slot \"", 
                slotNames[[i]], "\" (\"", classi, "\")"))
            next
        }
        namei <- slotNames[[i]]
        sloti <- try(switch(namei, .S3Class = S3Class(object), 
            slot(object, namei)), silent = TRUE)
        if (inherits(sloti, "try-error")) {
            errors <- c(errors, sloti)
            next
        }
        ok <- possibleExtends(class(sloti), classi, ClassDef2 = classDefi)
        if (identical(ok, FALSE)) {
            errors <- c(errors, paste0("invalid object for slot \"", 
                slotNames[[i]], "\" in class \"", Class, "\": got class \"", 
                class(sloti), "\", should be or extend class \"", 
                classi, "\""))
            next
        }
        if (!complete) 
            next
        errori <- anyStrings(Recall(sloti, TRUE, TRUE))
        if (length(errori)) {
            errori <- paste0("In slot \"", slotNames[[i]], "\" of class \"", 
                class(sloti), "\": ", errori)
            errors <- c(errors, errori)
        }
    }
    extends <- rev(classDef@contains)
    for (i in seq_along(extends)) {
        exti <- extends[[i]]
        superClass <- exti@superClass
        if (!exti@simple && !is(object, superClass)) 
            next
        superDef <- getClassDef(superClass, where = where)
        if (is.null(superDef)) {
            errors <- c(errors, paste0("superclass \"", superClass, 
                "\" not defined in the environment of the object's class"))
            break
        }
        validityMethod <- superDef@validity
        if (is(validityMethod, "function")) {
            errors <- c(errors, anyStrings(validityMethod(as(object, 
                superClass))))
            if (length(errors)) 
                break
        }
    }
    validityMethod <- classDef@validity
    if (length(errors) == 0L && is(validityMethod, "function")) {
        errors <- c(errors, anyStrings(validityMethod(object)))
    }
    if (length(errors)) {
        if (test) 
            errors
        else {
            msg <- gettextf("invalid class %s object", dQuote(Class))
            if (length(errors) > 1L) 
                stop(paste(paste0(msg, ":"), paste(seq_along(errors), 
                  errors, sep = ": "), collapse = "\n"), domain = NA)
            else stop(msg, ": ", errors, domain = NA)
        }
    }
    else TRUE
}


resetClass <- function (Class, classDef, where) 
{
    if (is(Class, "classRepresentation")) {
        classDef <- Class
        Class <- Class@className
        if (missing(where)) 
            where <- .classDefEnv(classDef)
    }
    else {
        if (missing(where)) {
            if (missing(classDef)) 
                where <- findClass(Class, unique = "resetting the definition")[[1L]]
            else where <- .classDefEnv(classDef)
        }
        if (missing(classDef)) {
            classDef <- getClassDef(Class, where)
            if (is.null(classDef)) {
                warning(gettextf("class %s not found on %s; 'resetClass' will have no effect", 
                  dQuote(Class), sQuote(getPackageName(where))), 
                  domain = NA)
                return(classDef)
            }
        }
        else if (!is(classDef, "classRepresentation")) 
            stop(gettextf("argument 'classDef' must be a string or a class representation; got an object of class %s", 
                dQuote(class(classDef))), domain = NA)
    }
    if (classDef@sealed) 
        warning(gettextf("class %s is sealed; 'resetClass' will have no effect", 
            dQuote(Class)), domain = NA)
    else {
        classDef <- .uncompleteClassDefinition(classDef)
        classDef <- completeClassDefinition(Class, classDef, 
            where)
        assignClassDef(Class, classDef, where)
    }
    classDef
}


.__C__hsearch <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("hsearch", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


isGroup <- function (f, where = topenv(parent.frame()), fdef = getGeneric(f, 
    where = where)) 
{
    is(fdef, "groupGenericFunction")
}


substituteFunctionArgs <- function (def, newArgs, args = formalArgs(def), silent = FALSE, 
    functionName = "a function") 
{
    if (!identical(args, newArgs)) {
        if (!missing(functionName)) 
            functionName <- paste("for", functionName)
        n <- length(args)
        if (n != length(newArgs)) 
            stop(sprintf("trying to change the argument list of %s with %d arguments to have arguments (%s)", 
                functionName, n, paste(newArgs, collapse = ", ")), 
                domain = NA)
        bdy <- body(def)
        checkFor <- newArgs[is.na(match(newArgs, args))]
        locals <- all.vars(bdy)
        if (length(checkFor) && any(!is.na(match(checkFor, locals)))) 
            stop(sprintf("get rid of variables in definition %s (%s); they conflict with the needed change to argument names (%s)", 
                functionName, paste(checkFor[!is.na(match(checkFor, 
                  locals))], collapse = ", "), paste(newArgs, 
                  collapse = ", ")), domain = NA)
        ll <- vector("list", 2L * n)
        for (i in seq_len(n)) {
            ll[[i]] <- as.name(args[[i]])
            ll[[n + i]] <- as.name(newArgs[[i]])
        }
        names(ll) <- c(args, newArgs)
        body(def, envir = environment(def)) <- substituteDirect(bdy, 
            ll)
        if (!silent) {
            msg <- sprintf("NOTE: arguments in definition %s changed from (%s) to (%s)", 
                functionName, paste(args, collapse = ", "), paste(newArgs, 
                  collapse = ", "))
            message(msg, domain = NA)
        }
    }
    def
}


`.__T__[<-:base` <- "<environment>"

multipleClasses <- function (details = FALSE) 
{
    classes <- as.list(.classTable, all.names = TRUE)
    dups <- Filter(is.list, classes)
    if (details) 
        dups
    else names(dups)
}


cacheGenericsMetaData <- function (f, fdef, attach = TRUE, where = topenv(parent.frame()), 
    package, methods) 
{
    if (!is(fdef, "genericFunction")) {
        warning(gettextf("no methods found for %s; cacheGenericsMetaData() will have no effect", 
            sQuote(f)), domain = NA)
        return(FALSE)
    }
    if (missing(package)) 
        package <- fdef@package
    deflt <- finalDefaultMethod(fdef@default)
    if (dispatchIsInternal(fdef)) {
        if (missing(methods)) 
            setPrimitiveMethods(f, deflt, "reset", fdef, NULL)
        else setPrimitiveMethods(f, deflt, "set", fdef, methods)
    }
    else if (isGroup(f, fdef = fdef)) {
        members <- fdef@groupMembers
        for (ff in members) {
            ffdef <- getGeneric(ff, where = where)
            if (is(ffdef, "genericFunction")) 
                Recall(ff, ffdef, attach, where, methods = .getMethodsTable(ffdef))
        }
    }
    TRUE
}


dumpMethods <- function (f, file = "", signature = NULL, methods = findMethods(f, 
    where = where), where = topenv(parent.frame())) 
{
    if (length(signature) > 0) 
        warning("argument 'signature' is not meaningful with the current implementation and is ignored \n(extract a subset of the methods list instead)")
    closeit <- TRUE
    isSTDOUT <- FALSE
    if (is.character(file)) {
        if (!(isSTDOUT <- file == "")) 
            file <- file(file, "w")
    }
    else if (inherits(file, "connection")) {
        if (!isOpen(file)) 
            open(file, "w")
        else closeit <- FALSE
    }
    else stop("'file' must be a character string or a connection")
    if (!isSTDOUT) {
        sink(file)
        on.exit({
            sink()
            if (closeit) close(file)
        })
    }
    sigs <- methods@signatures
    for (i in seq_along(methods)) dumpMethod(f, sigs[[i]], file = "", 
        def = methods[[i]])
}


.__C__conditionalExtension <- new("classRepresentation"
    , slots = structure(list(subClass = structure("character", package = "methods"), 
    superClass = structure("character", package = "methods"), 
    package = structure("character", package = "methods"), coerce = structure("function", package = "methods"), 
    test = structure("function", package = "methods"), replace = structure("function", package = "methods"), 
    simple = structure("logical", package = "methods"), by = structure("character", package = "methods"), 
    dataPart = structure("logical", package = "methods"), distance = structure("numeric", package = "methods")), .Names = c("subClass", 
"superClass", "package", "coerce", "test", "replace", "simple", 
"by", "dataPart", "distance"))
    , contains = structure(list(SClassExtension = S4_object()), .Names = "SClassExtension")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("conditionalExtension", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


prototype <- function (...) 
.prototype(...)


packageSlot <- function (object) 
attr(object, "package")


setGeneric <- function (name, def = NULL, group = list(), valueClass = character(), 
    where = topenv(parent.frame()), package = NULL, signature = NULL, 
    useAsDefault = NULL, genericFunction = NULL, simpleInheritanceOnly = NULL) 
{
    if (is.character(.isSingleName(name))) 
        stop(gettextf("invalid argument 'name': %s", .isSingleName(name)), 
            domain = NA)
    if (exists(name, "package:base") && inBasicFuns(name)) {
        name <- switch(name, as.double = "as.numeric", name)
        fdef <- getGeneric(name)
        compatibleSignature <- nargs() == 2L && !missing(signature) && 
            identical(signature, fdef@signature)
        if (nargs() <= 1 || compatibleSignature) {
            .cacheGeneric(name, fdef)
            return(name)
        }
        if (!is.function(useAsDefault) && !identical(useAsDefault, 
            FALSE)) {
            msg <- gettextf("%s dispatches internally;  methods can be defined, but the generic function is implicit, and cannot be changed.", 
                sQuote(name))
            stop(msg, domain = NA)
        }
    }
    simpleCall <- {
        nargs() < 2 || all(missing(def), missing(group), missing(valueClass), 
            missing(package), missing(signature), missing(useAsDefault), 
            missing(genericFunction), missing(simpleInheritanceOnly))
    }
    stdGenericBody <- substitute(standardGeneric(NAME), list(NAME = name))
    fdef <- if (is.null(package)) 
        getFunction(name, mustFind = FALSE, where = where)
    else {
        ev <- .NamespaceOrPackage(package)
        if (simpleCall) 
            implicitGeneric(name, ev)
        else getFunction(name, mustFind = FALSE, where = ev)
    }
    if (simpleCall) {
        if (is(fdef, "genericFunction")) 
            return(.GenericAssign(name, fdef, where))
    }
    if (is.null(fdef)) {
        if (isNamespace(where)) 
            fdef <- .getFromStandardPackages(name)
        else fdef <- getFunction(name, mustFind = FALSE)
    }
    if (is.null(fdef) && is.function(useAsDefault)) 
        fdef <- useAsDefault
    doUncache <- FALSE
    if (is.object(fdef) && is(fdef, "genericFunction")) {
        doUncache <- TRUE
        oldDef <- fdef
        prevDefault <- finalDefaultMethod(fdef@default)
        if (is.null(package)) 
            package <- fdef@package
    }
    else if (is.function(fdef)) {
        prevDefault <- fdef
        if (is.primitive(fdef)) 
            package <- "base"
        if (is.null(package)) 
            package <- getPackageName(environment(fdef))
    }
    else prevDefault <- NULL
    if (is.primitive(fdef)) 
        fdef <- getGeneric(name, where = where)
    else if (is.function(fdef)) 
        body(fdef, envir = as.environment(where)) <- stdGenericBody
    if (!is.null(def)) {
        if (is.primitive(def) || !is.function(def)) 
            stop(gettextf("if the 'def' argument is supplied, it must be a function that calls standardGeneric(\"%s\") or is the default", 
                name), domain = NA)
        nonstandardCase <- .NonstandardGenericTest(body(def), 
            name, stdGenericBody)
        if (is.na(nonstandardCase)) {
            if (is.null(useAsDefault)) {
                useAsDefault <- def
            }
            body(def, envir = as.environment(where)) <- stdGenericBody
            nonstandardCase <- FALSE
        }
        fdef <- def
        if (is.null(genericFunction) && nonstandardCase) 
            genericFunction <- new("nonstandardGenericFunction")
    }
    thisPackage <- getPackageName(where)
    if (is.null(package) || !nzchar(package)) 
        package <- thisPackage
    if (is.null(fdef)) 
        stop(gettextf("must supply a function skeleton for %s, explicitly or via an existing function", 
            sQuote(name)), domain = NA)
    ensureGeneric.fdef <- function(sig = signature) {
        if (!(is.object(fdef) && is(fdef, "genericFunction"))) {
            fdeflt <- if (is.function(useAsDefault)) 
                useAsDefault
            else if (identical(useAsDefault, FALSE)) 
                NULL
            else if (is.function(prevDefault) && !identical(formalArgs(prevDefault), 
                formalArgs(fdef)) && !is.primitive(prevDefault)) 
                NULL
            else prevDefault
            if (is.function(fdeflt)) 
                fdeflt <- .derivedDefaultMethod(fdeflt)
            fdef <<- makeGeneric(name, fdef, fdeflt, group = group, 
                valueClass = valueClass, package = package, signature = sig, 
                genericFunction = genericFunction, simpleInheritanceOnly = simpleInheritanceOnly)
        }
    }
    if (identical(package, thisPackage)) {
        ensureGeneric.fdef()
    }
    else {
        implicit <- implicitGeneric(name, .NamespaceOrPackage(package))
        if (is.null(implicit)) {
            ensureGeneric.fdef()
        }
        else {
            ensureGeneric.fdef(if (is.null(signature) && is.null(def)) 
                implicit@signature
            else signature)
            cmp <- .identicalGeneric(fdef, implicit, allow.extra.dots = !nzchar(Sys.getenv("R_SETGENERIC_PICKY_DOTS")))
            if (identical(cmp, TRUE)) {
                fdef <- implicit
            }
            else if (is.function(implicit)) {
                thisPName <- if (identical(thisPackage, ".GlobalEnv")) 
                  "the global environment"
                else paste("package", sQuote(thisPackage))
                if (is.null(def) && is.null(signature)) {
                  message(gettextf("Creating a generic function for %s from %s in %s\n    (from the saved implicit definition)", 
                    sQuote(name), sQuote(package), thisPName), 
                    domain = NA)
                  fdef <- implicit
                }
                else {
                  message(gettextf("Creating a new generic function for %s in %s", 
                    sQuote(name), thisPName), domain = NA)
                  fdef@package <- attr(fdef@generic, "package") <- thisPackage
                }
            }
            else {
                warning(gettextf("no generic version of %s on package %s is allowed;\n   a new generic will be assigned for %s", 
                  sQuote(name), sQuote(package), thisPName), 
                  domain = NA)
                fdef@package <- attr(fdef@generic, "package") <- thisPackage
            }
        }
    }
    if (identical(fdef@signature, "...")) 
        fdef <- .dotsGeneric(fdef)
    if (doUncache) 
        .uncacheGeneric(name, oldDef)
    groups <- fdef@group
    for (group in groups) {
        gdef <- getGeneric(group)
        if (is(gdef, "groupGenericFunction") && is.na(match(fdef@generic, 
            as.character(gdef@groupMembers)))) {
            gwhere <- .genEnv(group, where)
            gdef@groupMembers <- c(gdef@groupMembers, list(fdef@generic))
            assign(group, gdef, gwhere)
        }
    }
    .GenericAssign(name, fdef, where)
}


testVirtual <- function (properties, extends, prototype, where) 
{
    if (length(extends)) {
        en <- names(extends)
        if (!is.na(match("VIRTUAL", en))) 
            return(TRUE)
        for (what in en) {
            enDef <- getClassDef(what, where)
            if (!is.null(enDef) && identical(enDef@virtual, FALSE)) 
                return(FALSE)
        }
    }
    (length(properties) == 0L && is.null(prototype))
}


prohibitGeneric <- function (name, where = topenv(parent.frame())) 
{
    .saveToImplicitGenerics(name, FALSE, where)
}


.__C__groupGenericFunctionWithTrace <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    groupMembers = structure("list", package = "methods"), generic = structure("character", package = "methods"), 
    package = structure("character", package = "methods"), group = structure("list", package = "methods"), 
    valueClass = structure("character", package = "methods"), 
    signature = structure("character", package = "methods"), 
    default = structure("optionalMethod", package = "methods"), 
    skeleton = structure("call", package = "methods"), original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c(".Data", 
"groupMembers", "generic", "package", "group", "valueClass", 
"signature", "default", "skeleton", "original", "source"))
    , contains = structure(list(groupGenericFunction = S4_object(), 
    traceable = S4_object(), 
    genericFunction = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("groupGenericFunction", 
"traceable", "genericFunction", "function", "OptionalFunction", 
"PossibleMethod", "optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("groupGenericFunctionWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`.__T__addNextMethod:methods` <- "<environment>"

selectSuperClasses <- function (Class, dropVirtual = FALSE, namesOnly = TRUE, directOnly = TRUE, 
    simpleOnly = directOnly, where = topenv(parent.frame())) 
{
    ext <- if (isClassDef(Class)) 
        Class@contains
    else if (isClass(Class, where = where)) 
        getClass(Class, where = where)@contains
    else stop("'Class' must be a valid class definition or class")
    .selectSuperClasses(ext, dropVirtual = dropVirtual, namesOnly = namesOnly, 
        directOnly = directOnly, simpleOnly = simpleOnly)
}


slot <- function (object, name) 
.Call(C_R_get_slot, object, name)


doPrimitiveMethod <- function (name, def, call = sys.call(sys.parent()), ev = sys.frame(sys.parent(2))) 
{
    cat("called doPrimitiveMethod\n\n")
    if (!is.null(prev <- ev[[name]])) {
        on.exit(assign(name, prev, envir = ev))
    }
    else on.exit(rm(list = name, envir = ev))
    assign(name, def, envir = ev)
    eval(call, ev)
}


.__C__summaryDefault <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(table = S4_object(), 
    oldClass = S4_object()), .Names = c("table", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("summaryDefault", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__recordedplot <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("recordedplot", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.ShortPrimitiveSkeletons <- list(f(x, i), fgets(x, i, value = value))


show <- function (object) 
standardGeneric("show")


.__C__nonstandardGenericFunction <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    generic = structure("character", package = "methods"), package = structure("character", package = "methods"), 
    group = structure("list", package = "methods"), valueClass = structure("character", package = "methods"), 
    signature = structure("character", package = "methods"), 
    default = structure("optionalMethod", package = "methods"), 
    skeleton = structure("call", package = "methods")), .Names = c(".Data", 
"generic", "package", "group", "valueClass", "signature", "default", 
"skeleton"))
    , contains = structure(list(genericFunction = S4_object(), 
    nonstandardGeneric = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("genericFunction", 
"nonstandardGeneric", "function", "OptionalFunction", "PossibleMethod", 
"optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("nonstandardGenericFunction", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`.__T__Compare:methods` <- "<environment>"

cacheMetaData <- function (where, attach = TRUE, searchWhere = as.environment(where), 
    doCheck = TRUE) 
{
    pkg <- getPackageName(where)
    classes <- getClasses(where)
    for (cl in classes) {
        cldef <- (if (attach) 
            get(classMetaName(cl), where)
        else getClassDef(cl, searchWhere))
        if (is(cldef, "classRepresentation")) {
            if (attach) {
                .cacheClass(cl, cldef, is(cldef, "ClassUnionRepresentation"), 
                  where)
            }
            else if (identical(cldef@package, pkg)) {
                .uncacheClass(cl, cldef)
                .removeSuperclassBackRefs(cl, cldef, searchWhere)
            }
        }
    }
    generics <- .getGenerics(where)
    packages <- attr(generics, "package")
    if (length(packages) < length(generics)) 
        packages <- rep(packages, length.out = length(generics))
    if (attach && exists(".requireCachedGenerics", where, inherits = FALSE)) {
        others <- get(".requireCachedGenerics", where)
        generics <- c(generics, others)
        packages <- c(packages, attr(others, "package"))
    }
    dups <- duplicated(generics) & duplicated(packages)
    generics <- generics[!dups]
    for (i in seq_along(generics)) {
        f <- generics[[i]]
        fpkg <- packages[[i]]
        if (!identical(fpkg, pkg) && doCheck) {
            if (attach) {
                env <- as.environment(where)
                if (exists(f, envir = env, inherits = FALSE)) {
                  def <- get(f, envir = env)
                  fdef <- .genericOrImplicit(f, fpkg, env)
                  if (is.function(def)) {
                    if (identical(environment(def), environment(fdef))) 
                      next
                    else if (is(fdef, "genericFunction")) {
                      .assignOverBinding(f, fdef, env, FALSE)
                    }
                  }
                }
                else fdef <- getGeneric(f, FALSE, searchWhere, 
                  fpkg)
            }
            else fdef <- getGeneric(f, FALSE, searchWhere, fpkg)
        }
        else fdef <- getGeneric(f, FALSE, searchWhere, fpkg)
        if (!is(fdef, "genericFunction")) 
            next
        if (attach) 
            .cacheGeneric(f, fdef)
        else .uncacheGeneric(f, fdef)
        methods <- .updateMethodsInTable(fdef, where, attach)
        cacheGenericsMetaData(f, fdef, attach, where, fdef@package, 
            methods)
    }
    .doLoadActions(where, attach)
    invisible(NULL)
}


.__C__repeat <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object()), .Names = "language")
    , virtual = FALSE
    , prototype = repeat {
    break
}
    , validity = NULL
    , access = list()
    , className = "repeat"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


Ops <- function (e1, e2) 
standardGeneric("Ops")


superClassDepth <- function (ClassDef, soFar = ClassDef@className, simpleOnly = TRUE) 
{
    ext <- ClassDef@contains
    ok <- rep.int(TRUE, length(ext))
    for (i in seq_along(ext)) {
        exti <- ext[[i]]
        if (.isIndirectExtension(exti) || (simpleOnly && !exti@simple)) 
            ok[i] <- FALSE
    }
    ext <- ext[ok]
    immediate <- names(ext)
    notSoFar <- is.na(match(immediate, soFar))
    immediate <- immediate[notSoFar]
    super <- list(label = immediate, depth = rep.int(1, length(immediate)), 
        ext = ext)
    for (i in seq_along(immediate)) {
        what <- immediate[[i]]
        if (!is.na(match(what, soFar))) 
            next
        exti <- ext[[i]]
        soFar <- c(soFar, what)
        if (!is(exti, "SClassExtension")) 
            stop(gettextf("in definition of class %s, information for superclass %s is of class %s (expected \"SClassExtension\")", 
                dQuote(ClassDef@className), dQuote(what), dQuote(class(exti))), 
                domain = NA)
        superClass <- getClassDef(exti@superClass, package = exti@package)
        if (is.null(superClass)) {
            warning(gettextf("class %s extends an undefined class, %s", 
                dQuote(ClassDef@className), dQuote(what)), domain = NA)
            next
        }
        more <- Recall(superClass, soFar)
        whatMore <- more$label
        if (!all(is.na(match(whatMore, soFar)))) {
            ok <- is.na(match(whatMore, soFar))
            more$depth <- more$depth[ok]
            more$label <- more$label[ok]
            more$ext <- more$ext[ok]
            whatMore <- whatMore[ok]
        }
        if (length(whatMore)) {
            soFar <- c(soFar, whatMore)
            super$depth <- c(super$depth, 1 + more$depth)
            super$label <- c(super$label, more$label)
            super$ext <- c(super$ext, more$ext)
        }
    }
    super
}


.__C__vector <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = logical(0)
    , validity = NULL
    , access = list()
    , className = structure("vector", package = "methods")
    , package = "methods"
    , subclasses = structure(list(logical = S4_object(), 
    numeric = S4_object(), 
    character = S4_object(), 
    complex = S4_object(), 
    integer = S4_object(), 
    raw = S4_object(), 
    expression = S4_object(), 
    list = S4_object(), 
    structure = S4_object(), 
    array = S4_object(), 
    matrix = S4_object(), 
    signature = S4_object(), 
    className = S4_object(), 
    ObjectsWithPackage = S4_object(), 
    ts = S4_object(), 
    mts = S4_object(), 
    factor = S4_object(), 
    data.frame = S4_object(), 
    namedList = S4_object(), 
    listOfMethods = S4_object()), .Names = c("logical", 
"numeric", "character", "complex", "integer", "raw", "expression", 
"list", "structure", "array", "matrix", "signature", "className", 
"ObjectsWithPackage", "ts", "mts", "factor", "data.frame", "namedList", 
"listOfMethods"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__POSIXlt <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(POSIXt = S4_object(), 
    oldClass = S4_object()), .Names = c("POSIXt", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("POSIXlt", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


makeExtends <- function (Class, to, coerce = NULL, test = NULL, replace = NULL, 
    by = character(), package, slots = getSlots(classDef1), classDef1 = getClass(Class), 
    classDef2) 
{
    dataEquiv <- function(cl1, cl2) {
        .identC(cl1, cl2) || (extends(cl1, cl2) && !any(is.na(match(c(cl1, 
            cl2), .BasicClasses))))
    }
    packageEnv <- .requirePackage(package)
    class1Defined <- missing(slots)
    simple <- is.null(coerce) && is.null(test) && is.null(replace) && 
        (length(by) == 0)
    distance <- 1
    dataPartClass <- elNamed(slots, ".Data")
    dataPart <- FALSE
    if (simple && !is.null(dataPartClass)) {
        if (!(is.null(getClassDef(dataPartClass)) || is.null(getClassDef(to)))) {
            dataPart <- dataEquiv(dataPartClass, to)
        }
    }
    if (is.null(coerce)) {
        coerce <- .simpleExtCoerce
        if (isXS3Class(classDef2)) {
            body(coerce, envir = packageEnv) <- substitute({
                if (strict) S3Part(from, S3Class = S3CLASS) else from
            }, list(S3CLASS = to))
        }
        else if (!isVirtualClass(classDef2)) 
            body(coerce, envir = packageEnv) <- .simpleCoerceExpr(Class, 
                to, names(slots), classDef2)
    }
    else if (is(coerce, "function")) {
        if (length(formals(coerce)) == 1) {
            coerce <- .ChangeFormals(coerce, .simpleIsCoerce, 
                "'coerce' argument to setIs ")
            tmp <- .simpleExtCoerce
            body(tmp, envir = environment(coerce)) <- body(coerce)
            coerce <- tmp
        }
        else coerce <- .ChangeFormals(coerce, .simpleExtCoerce, 
            "'coerce' argument to setIs ")
    }
    else stop(gettextf("the 'coerce' argument to 'setIs' should be a function of one argument, got an object of class %s", 
        dQuote(class(coerce))), domain = NA)
    if (is.null(test)) {
        test <- .simpleExtTest
        extClass <- "SClassExtension"
    }
    else {
        test <- .ChangeFormals(test, .simpleExtTest, "'test' argument to setIs ")
        extClass <- "conditionalExtension"
    }
    if (is.null(replace)) {
        if (dataPart) {
            extn <- classDef2@contains[[dataPartClass]]
            if (is(extn, "SClassExtension")) 
                easy <- extn@simple
            else easy <- FALSE
            if (easy) 
                replace <- .dataPartReplace$f1
            else {
                replace <- .dataPartReplace$f2
                bdy <- body(replace)
                body(replace, envir = environment(replace)) <- substituteDirect(bdy, 
                  list(THISCLASS = dataPartClass))
            }
        }
        else if (simple) {
            replace <- .simpleExtReplace
            if (isXS3Class(classDef2)) {
                S3Class <- attr(classDef2@prototype, ".S3Class")
                if (is.null(S3Class)) 
                  S3Class <- to
                body(replace, envir = packageEnv) <- quote({
                  S3Part(from) <- value
                  from
                })
            }
            else if (isVirtualClass(classDef2)) {
                body(replace, envir = packageEnv) <- substitute({
                  if (!is(value, TO)) stop(gettextf("the computation: 'as(object,\"%s\") <- value' is valid when object has class %s only if 'is(value, \"%s\")' is TRUE ('class(value)' was %s)\n", 
                    TO, dQuote(FROM), TO, dQuote(class(value))), 
                    domain = NA)
                  value
                }, list(FROM = Class, TO = to))
            }
            else if (class1Defined && length(slots) == 0) {
                ext <- getAllSuperClasses(classDef1, TRUE)
                toSlots <- classDef2@slots
                sameSlots <- TRUE
                for (eclass in ext) {
                  if (.identC(eclass, to)) 
                    next
                  edef <- getClassDef(eclass, where = packageEnv)
                  if (!is.null(edef) && length(edef@slots) > 
                    0) {
                    sameSlots <- FALSE
                    break
                  }
                }
                if (sameSlots) 
                  body(replace, envir = packageEnv) <- substitute({
                    class(value) <- FROM
                    value
                  }, list(FROM = Class))
                else if (length(toSlots) == 0) 
                  replace <- .ErrorReplace
            }
            else body(replace, envir = packageEnv) <- .simpleReplaceExpr(classDef2)
        }
        else replace <- .ErrorReplace
        if (identical(replace, .ErrorReplace)) 
            warning(gettextf("there is no automatic definition for 'as(object, \"%s\") <- value' when object has class %s and no 'replace' argument was supplied; replacement will be an error", 
                to, dQuote(Class)), domain = NA)
    }
    else if (is(replace, "function")) {
        if (length(formals(replace)) == 2) {
            replace <- .ChangeFormals(replace, .dataPartReplace$f2args, 
                "'replace' argument to setIs ")
            tmp <- .ErrorReplace
            body(tmp, envir = environment(replace)) <- body(replace)
            replace <- tmp
        }
        else replace <- .ChangeFormals(replace, .ErrorReplace, 
            "'replace' argument to setIs ")
    }
    else stop(gettextf("the 'replace' argument to setIs() should be a function of 2 or 3 arguments, got an object of class %s", 
        dQuote(class(replace))), domain = NA)
    new(extClass, subClass = Class, superClass = to, package = package, 
        coerce = coerce, test = test, replace = replace, simple = simple, 
        by = by, dataPart = dataPart, distance = distance)
}


getValidity <- function (ClassDef) 
{
    ClassDef@validity
}


functionBody <- function (fun = sys.function(sys.parent())) 
{
    if (is.character(fun)) 
        fun <- get(fun, mode = "function", envir = parent.frame())
    .Internal(body(fun))
}


`.__C__<-` <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(language = S4_object()), .Names = "language")
    , virtual = FALSE
    , prototype = "<undef>" <- NULL
    , validity = NULL
    , access = list()
    , className = "<-"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


completeExtends <- function (ClassDef, class2, extensionDef, where) 
{
    ext <- ClassDef@contains
    for (i in seq_along(ext)) {
        if (.isIndirectExtension(ext[[i]])) {
            ClassDef <- .uncompleteClassDefinition(ClassDef, 
                "contains")
            break
        }
    }
    exts <- .walkClassGraph(ClassDef, "contains", where, attr(ext, 
        "conflicts"))
    if (length(exts)) {
        if ("oldClass" %in% names(exts) && length(ClassDef@slots) > 
            1L) 
            exts <- .S3Extends(ClassDef, exts, where)
    }
    if (!missing(class2) && length(ClassDef@subclasses)) {
        strictBy <- TRUE
        subclasses <- .transitiveSubclasses(ClassDef@className, 
            class2, extensionDef, ClassDef@subclasses, strictBy)
        for (i in seq_along(subclasses)) {
            obji <- subclasses[[i]]
            if (!extends(obji@subClass, class2)) 
                setIs(obji@subClass, class2, extensionObject = obji, 
                  doComplete = FALSE, where = where)
        }
    }
    exts
}


.__C__integrate <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("integrate", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__structure <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(vector = S4_object()), .Names = "vector")
    , virtual = TRUE
    , prototype = structure(numeric(0), .Dim = 0L)
    , validity = NULL
    , access = list()
    , className = structure("structure", package = "methods")
    , package = "methods"
    , subclasses = structure(list(array = S4_object(), 
    matrix = S4_object(), 
    ts = S4_object(), 
    mts = S4_object()), .Names = c("array", 
"matrix", "ts", "mts"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


slotsFromS3 <- function (object) 
standardGeneric("slotsFromS3")


`as<-` <- function (object, Class, value) 
{
    thisClass <- .class1(object)
    if (!.identC(.class1(value), Class)) 
        value <- as(value, Class, strict = FALSE)
    where <- .classEnv(class(object))
    coerceFun <- getGeneric("coerce<-", where = where)
    coerceMethods <- getMethodsForDispatch(coerceFun)
    asMethod <- .quickCoerceSelect(thisClass, Class, coerceFun, 
        coerceMethods, where)
    if (is.null(asMethod)) {
        sig <- c(from = thisClass, to = Class)
        canCache <- TRUE
        inherited <- FALSE
        asMethod <- selectMethod("coerce<-", sig, TRUE, FALSE, 
            fdef = coerceFun, mlist = coerceMethods)
        if (is.null(asMethod)) {
            if (is(object, Class)) {
                asMethod <- possibleExtends(thisClass, Class)
                if (identical(asMethod, TRUE)) {
                  class(value) <- class(object)
                  return(value)
                }
                else {
                  test <- asMethod@test
                  asMethod <- asMethod@replace
                  canCache <- (!is(test, "function")) || identical(body(test), 
                    TRUE)
                  if (canCache) {
                    ClassDef <- getClassDef(Class, where)
                    asMethod <- .asCoerceMethod(asMethod, thisClass, 
                      ClassDef, TRUE, where)
                  }
                }
            }
            else {
                asMethod <- selectMethod("coerce<-", sig, TRUE, 
                  c(from = TRUE, to = FALSE), doCache = TRUE)
                inherited <- TRUE
            }
        }
        if (canCache && !is.null(asMethod)) 
            cacheMethod("coerce<-", sig, asMethod, fdef = coerceFun, 
                inherited = inherited)
    }
    if (is.null(asMethod)) 
        stop(gettextf("no method or default for as() replacement of %s with Class=\"%s\"", 
            dQuote(thisClass), Class), domain = NA)
    asMethod(object, Class, value)
}


.__C__genericFunctionWithTrace <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    generic = structure("character", package = "methods"), package = structure("character", package = "methods"), 
    group = structure("list", package = "methods"), valueClass = structure("character", package = "methods"), 
    signature = structure("character", package = "methods"), 
    default = structure("optionalMethod", package = "methods"), 
    skeleton = structure("call", package = "methods"), original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c(".Data", 
"generic", "package", "group", "valueClass", "signature", "default", 
"skeleton", "original", "source"))
    , contains = structure(list(genericFunction = S4_object(), 
    traceable = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("genericFunction", 
"traceable", "function", "OptionalFunction", "PossibleMethod", 
"optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("genericFunctionWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__rle <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("rle", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__activeBindingFunction <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods")), .Names = ".Data")
    , contains = structure(list(`function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object()), .Names = c("function", 
"OptionalFunction", "PossibleMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("activeBindingFunction", package = "methods")
    , package = "methods"
    , subclasses = structure(list(defaultBindingFunction = S4_object()), .Names = "defaultBindingFunction")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


Math2 <- function (x, digits) 
standardGeneric("Math2")


.__C__SuperClassMethod <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = character(0)
    , validity = NULL
    , access = list()
    , className = structure("SuperClassMethod", package = "methods")
    , package = "methods"
    , subclasses = structure(list(character = S4_object(), 
    refMethodDef = S4_object(), 
    signature = S4_object(), 
    className = S4_object(), 
    ObjectsWithPackage = S4_object(), 
    refMethodDefWithTrace = S4_object()), .Names = c("character", 
"refMethodDef", "signature", "className", "ObjectsWithPackage", 
"refMethodDefWithTrace"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


promptClass <- function (clName, filename = NULL, type = "class", keywords = "classes", 
    where = topenv(parent.frame()), generatorName = clName) 
{
    classInSig <- function(g, where, cl) {
        cl %in% unique(unlist(findMethods(g, where)@signatures))
    }
    genWithClass <- function(cl, where) {
        allgen <- getGenerics(where = where)
        ok <- as.logical(unlist(lapply(allgen, classInSig, cl = cl, 
            where = where)))
        allgen[ok]
    }
    sigsList <- function(g, where) {
        methods <- findMethods(g, where)
        value <- methods@signatures
        args <- methods@arguments
        if (length(value)) {
            length(args) <- length(value[[1]])
            value <- lapply(value, function(x) {
                names(x) <- args
                x
            })
        }
        value
    }
    slotClassWithSource <- function(clname) {
        clDef <- getClassDef(clname)
        extds <- names(clDef@contains)
        allslots <- getSlots(clDef)
        for (j in rev(seq_along(extds))) {
            i <- extds[[j]]
            slotsi <- getSlots(getClass(i))
            if (length(slotsi)) 
                allslots[names(slotsi)] <- paste0("\"", as.character(slotsi), 
                  "\", from class \"", i, "\"")
        }
        slotsi <- getSlots(clDef)
        if (length(slotsi)) 
            allslots[names(slotsi)] <- paste0("\"", as.character(slotsi), 
                "\"")
        allslots
    }
    cleanPrompt <- function(object, name) {
        value <- utils::prompt(object, name = name, filename = NA)
        for (i in seq_along(value)) {
            item <- value[[i]]
            bad <- grepl("^ *%", item)
            if (any(bad)) 
                value[[i]] <- item[!bad]
        }
        value
    }
    pastePar <- function(x) {
        xn <- names(x)
        x <- as.character(x)
        xn <- if (length(xn) == length(x)) 
            paste(xn, "= ")
        else ""
        paste0("(", paste0(xn, "\"", x, "\"", collapse = ", "), 
            ")")
    }
    escape <- function(txt) gsub("%", "\\\\%", txt)
    if (is.null(filename)) 
        filename <- paste0(utils:::topicName(type, clName), ".Rd")
    if (!missing(where) && !is.na(match(clName, getClasses(where)))) 
        whereClass <- where
    else {
        whereClass <- utils::find(classMetaName(clName))
        if (length(whereClass) == 0L) 
            stop(gettextf("no definition of class %s found", 
                dQuote(clName)), domain = NA)
        else if (length(whereClass) > 1L) {
            if (identical(where, topenv(parent.frame()))) {
                whereClass <- whereClass[[1L]]
                warning(gettextf("multiple definitions of %s found; using the one on %s", 
                  dQuote(clName), whereClass), domain = NA)
            }
            else {
                if (exists(classMetaName(clName), where, inherits = FALSE)) 
                  whereClass <- where
                else stop(sprintf(ngettext(length(whereClass), 
                  "no definition of class %s in the specified position, %s, definition on : %s", 
                  "no definition of class %s in the specified position, %s, definitions on : %s"), 
                  dQuote(clName), where, paste(whereClass, collapse = ", ")), 
                  domain = NA)
            }
        }
    }
    fullName <- utils:::topicName("class", clName)
    clDef <- getClass(clName, where = whereClass)
    .name <- paste0("\\name{", fullName, "}")
    .type <- paste0("\\docType{", type, "}")
    .alias <- paste0("\\alias{", fullName, "}")
    .title <- sprintf("\\title{Class \\code{\"%s\"}}", clName)
    .desc <- paste0("\\description{", "\n%%  ~~ A concise (1-5 lines) description of what the class is. ~~", 
        "\n}")
    slotclasses <- getSlots(clDef)
    slotnames <- names(slotclasses)
    slotclasses <- as.character(slotclasses)
    nslots <- length(slotclasses)
    clNameQ <- paste0("\"", clName, "\"")
    .usage <- "\\section{Objects from the Class}"
    virtualClass <- isVirtualClass(clName)
    if (virtualClass) {
        .usage <- paste0(.usage, "{A virtual Class: No objects may be created from it.}")
        generator <- NULL
    }
    else {
        if (exists(generatorName, where, inherits = FALSE)) 
            generator <- get(generatorName, where, inherits = FALSE)
        else generator <- NULL
        if (is(generator, "classGeneratorFunction")) {
            promptGenerator <- cleanPrompt(generator, generatorName)
            callString <- .makeCallString(generator, generatorName)
            .alias <- c(.alias, promptGenerator$aliases)
        }
        else {
            initMethod <- unRematchDefinition(selectMethod("initialize", 
                clName))
            argNames <- formalArgs(initMethod)
            argNames[[1L]] <- clNameQ
            callString <- .makeCallString(initMethod, "new", 
                argNames)
        }
        .usage <- c(paste0(.usage, "{"), paste0("Objects can be created by calls of the form \\code{", 
            callString, "}."), "%%  ~~ describe objects here ~~ ", 
            "}")
    }
    .slots <- if (nslots > 0) {
        slotclasses <- slotClassWithSource(clName)
        slotnames <- names(slotclasses)
        .slots.head <- c("\\section{Slots}{", "  \\describe{")
        .slots.body <- paste0("    \\item{\\code{", slotnames, 
            "}:}", "{Object of class \\code{", slotclasses, "} ~~ }")
        .slots.tail <- c("  }", "}")
        c(.slots.head, .slots.body, .slots.tail)
    }
    else character()
    .extends <- clDef@contains
    if (length(.extends)) {
        .extends <- showExtends(.extends, printTo = FALSE)
        .extends <- c("\\section{Extends}{", paste0("Class \\code{\"\\linkS4class{", 
            .extends$what, "}\"}, ", gsub("^(by class) (\".*\")$", 
                "\\1 \\\\code{\\2}", .extends$how), "."), "}")
    }
    else .extends <- character()
    nmeths <- length(methnms <- genWithClass(clName, where = whereClass))
    .meths.head <- "\\section{Methods}{"
    .methAliases <- ""
    if (nmeths > 0) {
        .meths.body <- "  \\describe{"
        for (i in 1L:nmeths) {
            .sig <- sigsList(methnms[i], where = whereClass)
            for (j in seq_along(.sig)) {
                if (!all(is.na(match(.sig[[j]], clName)))) {
                  methn.i <- escape(methnms[i])
                  .meths.body <- c(.meths.body, paste0("    \\item{", 
                    methn.i, "}{\\code{signature", pastePar(.sig[[j]]), 
                    "}: ... }"))
                  cur <- paste(.sig[[j]], collapse = ",")
                  .methAliases <- paste0(.methAliases, "\\alias{", 
                    methn.i, ",", cur, "-method}\n")
                }
            }
        }
        .meths.body <- c(.meths.body, "\t }")
    }
    else {
        .meths.head <- "\\section{Methods}{"
        .meths.body <- paste("No methods defined with class", 
            clNameQ, "in the signature.")
    }
    .meths.tail <- "}"
    .keywords <- paste0("\\keyword{", keywords, "}")
    Rdtxt <- list(name = .name, version = "\\Rdversion{1.1}", 
        type = .type, aliases = .alias, methAliases = .methAliases, 
        title = .title, description = .desc, `section{Objects from the Class}` = .usage, 
        `section{Slots}` = .slots, `section{Extends}` = .extends, 
        `section{Methods}` = c(.meths.head, .meths.body, .meths.tail), 
        references = paste("\\references{\n%%  ~~put references to the", 
            "literature/web site here~~\n}"), author = "\\author{\n%%  ~~who you are~~\n}", 
        note = c("\\note{\n%%  ~~further notes~~\n}", "", paste("%% ~Make other sections like Warning with", 
            "\\section{Warning }{....} ~"), ""), seealso = c("\\seealso{", 
            paste("%%  ~~objects to See Also as", "\\code{\\link{~~fun~~}}, ~~~"), 
            paste("%%  ~~or \\code{\\linkS4class{CLASSNAME}}", 
                "for links to other classes ~~~"), "}"), examples = c("\\examples{", 
            paste0("showClass(", clNameQ, ")"), "}"), keywords = .keywords)
    if (is(clDef, "refClassRepresentation")) 
        Rdtxt <- refClassPrompt(clDef, Rdtxt, nmeths, nslots, 
            .meths.head)
    else if (is(generator, "classGeneratorFunction")) {
        what <- c("usage", "arguments")
        Rdtxt[what] <- promptGenerator[what]
    }
    if (is.na(filename)) 
        return(Rdtxt)
    cat(unlist(Rdtxt), file = filename, sep = "\n")
    .message("A shell of class documentation has been written", 
        .fileDesc(filename), ".\n")
    invisible(filename)
}


Quote <- function (expr)  .Primitive("quote")


.__C__internalDispatchMethod <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    internal = structure("character", package = "methods"), target = structure("signature", package = "methods"), 
    defined = structure("signature", package = "methods"), generic = structure("character", package = "methods")), .Names = c(".Data", 
"internal", "target", "defined", "generic"))
    , contains = structure(list(derivedDefaultMethod = S4_object(), 
    MethodDefinition = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("derivedDefaultMethod", 
"MethodDefinition", "function", "OptionalFunction", "PossibleMethod", 
"optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("internalDispatchMethod", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


getAllMethods <- function (f, fdef, where = topenv(parent.frame())) 
.Defunct()


setClassUnion <- function (name, members = character(), where = topenv(parent.frame())) 
{
    if (length(members) > 0) {
        membersDefined <- sapply(members, isClass, where = as.environment(where))
        if (!all(membersDefined)) 
            stop(gettextf("the member classes must be defined: not true of %s", 
                paste(.dQ(as(members[!membersDefined], "character")), 
                  collapse = ", ")), domain = NA)
    }
    def <- new("ClassUnionRepresentation", makeClassRepresentation(name, 
        package = getPackageName(where), where = where))
    prev <- getClassDef(name, where = where)
    value <- setClass(name, def, where = where)
    failed <- character()
    hasNull <- match("NULL", members, 0)
    if (hasNull) 
        members <- c("NULL", members[-hasNull])
    for (what in members) {
        if (is(try(setIs(what, name, where = where)), "try-error")) {
            if (!is.character(what)) 
                what <- getClass(what, TRUE, where)@className
            failed <- c(failed, what)
        }
    }
    if (length(failed) > 0) {
        if (is.null(prev)) 
            try(removeClass(name, where = where))
        else try(setClass(name, prev, where = where))
        stop(gettextf("unable to create union class:  could not set members %s", 
            paste(.dQ(failed), collapse = ", ")), domain = NA)
    }
    invisible(value)
}


`body<-` <- function (fun, envir = environment(fun), value) 
standardGeneric("body<-")


removeMethod <- function (f, signature = character(), where = topenv(parent.frame())) 
{
    if (is.function(f)) {
        if (is(f, "genericFunction")) {
            fdef <- f
            f <- f@generic
        }
        else if (is.primitive(f)) {
            f <- .primname(f)
            fdef <- genericForBasic(f, mustFind = FALSE)
        }
        else stop("function supplied as argument 'f' must be a generic")
    }
    else fdef <- getGeneric(f, where = where)
    if (is.null(fdef)) {
        warning(gettextf("no generic function %s found", sQuote(f)), 
            domain = NA)
        return(FALSE)
    }
    if (is.null(getMethod(fdef, signature, optional = TRUE))) {
        warning(gettextf("no method found for function %s and signature %s", 
            sQuote(fdef@generic), paste(.dQ(signature), collapse = ", ")), 
            domain = NA)
        return(FALSE)
    }
    setMethod(f, signature, NULL, where = where)
    TRUE
}


newClassRepresentation <- function (...) 
new("classRepresentation", ...)


.__C__localRefClass <- new("refClassRepresentation"
    , fieldClasses = list()
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
    , className = structure("localRefClass", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__refMethodDef <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    mayCall = structure("character", package = "methods"), name = structure("character", package = "methods"), 
    refClassName = structure("character", package = "methods"), 
    superClassMethod = structure("SuperClassMethod", package = "methods")), .Names = c(".Data", 
"mayCall", "name", "refClassName", "superClassMethod"))
    , contains = structure(list(`function` = S4_object(), 
    SuperClassMethod = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object()), .Names = c("function", 
"SuperClassMethod", "OptionalFunction", "PossibleMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("refMethodDef", package = "methods")
    , package = "methods"
    , subclasses = structure(list(refMethodDefWithTrace = S4_object(), 
    externalRefMethod = S4_object()), .Names = c("refMethodDefWithTrace", 
"externalRefMethod"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


generic.skeleton <- function (name, fdef, fdefault) 
{
    anames <- formalArgs(fdef)
    skeleton <- lapply(as.list(c(name, anames)), as.name)
    dots <- match("...", anames)
    if (!is.na(dots) && dots < length(anames)) {
        anames[1L:dots] <- ""
        names(skeleton) <- c("", anames)
    }
    if (is.null(fdefault)) {
        fdefault <- fdef
        msg <- gettextf("invalid call in method dispatch to '%s' (no default method)", 
            name)
        body(fdefault) <- substitute(stop(MESSAGE, domain = NA), 
            list(MESSAGE = msg))
        environment(fdefault) <- baseenv()
    }
    skeleton[[1L]] <- fdefault
    as.call(skeleton)
}


.__C__.name <- new("classRepresentation"
    , slots = structure(list(.xData = structure("name", package = "methods")), .Names = ".xData")
    , contains = structure(list(name = S4_object(), 
    language = S4_object(), 
    refObject = S4_object()), .Names = c("name", 
"language", "refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure(".name", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


hasLoadAction <- function (aname, where = topenv(parent.frame())) 
exists(.actionMetaName(aname), envir = where, inherits = FALSE)


setAs <- function (from, to, def, replace = NULL, where = topenv(parent.frame())) 
{
    fromDef <- getClassDef(from, where)
    extds <- possibleExtends(from, to, fromDef)
    if (is(extds, "SClassExtension")) {
        test <- extds@test
        if (is.null(replace)) 
            replace <- extds@replace
        test <- NULL
        setIs(from, to, test = test, coerce = def, replace = replace, 
            where = where)
    }
    else if (identical(extds, TRUE)) {
        if (.identC(from, to)) 
            stop(gettextf("trying to set an 'as' relation from %s to itself", 
                dQuote(.class1(from))), domain = NA)
        toDef <- getClassDef(to, where = where)
        if (is.null(toDef)) 
            stop(gettextf("class %s is not defined in this environment", 
                dQuote(to)), domain = NA)
        if (isClassUnion(toDef)) 
            stop(gettextf("class %s is a class union: 'coerce' relations to a class union are not meaningful", 
                dQuote(to)), domain = NA)
        setIs(from, to, coerce = def, replace = replace, where = where)
    }
    args <- formalArgs(def)
    if (!is.na(match("strict", args))) 
        args <- args[-match("strict", args)]
    if (length(args) == 1) 
        def <- substituteFunctionArgs(def, "from", functionName = "coerce")
    else if (length(args) != 2 || !identical(args, c("from", 
        "to"))) 
        stop(gettextf("'as' method should have one argument, or match the arguments of coerce(): got  (%s)", 
            paste(formalArgs(def), collapse = ", ")), domain = NA)
    method <- as.list(coerce@.Data)
    method$to <- to
    method <- as.function(method)
    body(method, envir = environment(def)) <- body(def)
    setMethod("coerce", c(from, to), method, where = where)
    if (!is.null(replace)) {
        args <- formalArgs(replace)
        if (identical(args, c("from", "to", "value"))) 
            method <- replace
        else {
            if (length(args) != 2) 
                stop(gettextf("a 'replace' method definition in 'setAs' must be a function of two arguments, got %d", 
                  length(args)), domain = NA)
            replace <- body(replace)
            if (!identical(args, c("from", "value"))) {
                ll <- list(quote(from), quote(value))
                names(ll) <- args
                replace <- substituteDirect(replace, ll)
                warning(gettextf("argument names in 'replace' changed to agree with 'coerce<-' generic:\n%s", 
                  paste(deparse(replace), sep = "\n    ")), domain = NA)
            }
            method <- eval(function(from, to, value) NULL)
            body(method, envir = .GlobalEnv) <- replace
        }
        setMethod("coerce<-", c(from, to), method, where = where)
    }
}


.__C__aov <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(lm = S4_object(), 
    oldClass = S4_object()), .Names = c("lm", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("aov", package = "methods")
    , package = "methods"
    , subclasses = structure(list(maov = S4_object()), .Names = "maov")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`.__T__cbind2:methods` <- "<environment>"

listFromMethods <- function (generic, where, table) 
{
    fdef <- getGeneric(generic)
    if (missing(table)) 
        table <- if (missing(where)) 
            .getMethodsTable(fdef)
        else get(.TableMetaName(fdef@generic, fdef@package), 
            envir = as.environment(where), inherits = FALSE)
    fev <- environment(fdef)
    nSigArgs <- .getGenericSigLength(fdef, fev)
    methods <- as.list(table, all.names = TRUE)
    names <- names(methods)
    if (nSigArgs > 1) {
        n <- length(names)
        sigs <- vector("list", n)
        namesCon <- textConnection(names)
        for (i in seq_len(n)) sigs[[i]] <- scan(namesCon, "", 
            sep = "#", nmax = nSigArgs, quiet = TRUE)
    }
    else sigs <- as.list(names)
    new("LinearMethodsList", classes = sigs, methods = methods, 
        arguments = .getGenericSigArgs(fdef, fev), generic = fdef)
}


.__C__data.frameRowLabels <- new("ClassUnionRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = character(0)
    , validity = NULL
    , access = list()
    , className = structure("data.frameRowLabels", package = "methods")
    , package = "methods"
    , subclasses = structure(list(character = S4_object(), 
    integer = S4_object(), 
    signature = S4_object(), 
    className = S4_object(), 
    ObjectsWithPackage = S4_object(), 
    factor = S4_object()), .Names = c("character", 
"integer", "signature", "className", "ObjectsWithPackage", "factor"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


classesToAM <- function (classes, includeSubclasses = FALSE, abbreviate = 2) 
{
    .mergeMatrices <- function(m1, m2) {
        if (nrow(m1) == 0) 
            return(m2)
        dn1 <- dimnames(m1)
        dn2 <- dimnames(m2)
        rows <- unique(c(dn1[[1]], dn2[[1]]))
        columns <- unique(c(dn1[[2]], dn2[[2]]))
        value <- matrix(0, length(rows), length(columns), dimnames = list(rows, 
            columns))
        value[dn1[[1]], dn1[[2]]] <- m1
        value[dn2[[1]], dn2[[2]]] <- m2
        value
    }
    if (length(includeSubclasses) == 1) 
        includeSubclasses <- rep.int(includeSubclasses, length(classes))
    if (!is(includeSubclasses, "logical") || length(includeSubclasses) != 
        length(classes)) 
        stop("argument 'includeSubclasses' must be a logical, either one value or a vector of the same length as argument 'classes'")
    value <- matrix(0, 0, 0)
    for (i in seq_along(classes)) {
        class <- classes[[i]]
        classDef <- getClass(class)
        value <- .mergeMatrices(value, .oneClassToAM(classDef, 
            includeSubclasses[[i]]))
    }
    abbr <- match(as.integer(abbreviate), 0:3) - 1
    if (length(abbr) != 1 || is.na(abbr)) 
        stop("argument 'abbreviate' must be 0, 1, 2, or 3")
    if (abbr%%2) 
        dimnames(value)[[1]] <- abbreviate(dimnames(value)[[1]])
    if (abbr%/%2) 
        dimnames(value)[[2]] <- abbreviate(dimnames(value)[[2]])
    value
}


getProperties <- function (ClassDef) 
.Defunct()


traceOff <- function (whatL) 
.Defunct("untrace")


classLabel <- function (Class) 
{
    if (is.character(Class) && length(Class)) {
        className <- Class[[1L]]
        packageName <- attr(Class, "package")
        if (is.null(packageName)) 
            packageName <- ""
    }
    else {
        if (is(Class, "classRepresentation")) {
            className <- Class@className
            packageName <- Class@package
        }
        else stop(gettextf("invalid call to 'classLabel': expected a name or a class definition, got an object of class %s", 
            classLabel(class(Class))), domain = NA)
    }
    if (.showPackage(className)) {
        packageName <- if (identical(packageName, ".GlobalEnv")) 
            " (from the global environment)"
        else paste0(" (from package \"", packageName, "\")")
        paste0("\"", className, "\"", packageName)
    }
    else paste0("\"", className, "\"")
}


removeMethodsObject <- function (f, where = topenv(parent.frame())) 
.Defunct()


allNames <- function (x) 
{
    value <- names(x)
    if (is.null(value)) 
        character(length(x))
    else value
}


getSlots <- function (x) 
{
    classDef <- if (isClassDef(x)) 
        x
    else getClass(x)
    props <- classDef@slots
    value <- as.character(props)
    names(value) <- names(props)
    value
}


.__C__S3 <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = "S3"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__S4 <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = "S4"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


inheritedSlotNames <- function (Class, where = topenv(parent.frame())) 
{
    ext <- if (isClassDef(Class)) 
        Class@contains
    else if (isClass(Class, where = where)) 
        getClass(Class, where = where)@contains
    supcl <- .selectSuperClasses(ext)
    unique(unlist(lapply(lapply(supcl, getClassDef), slotNames), 
        use.names = FALSE))
}


showDefault <- function (object, oldMethods = TRUE) 
{
    clDef <- getClass(cl <- class(object), .Force = TRUE)
    cl <- classLabel(cl)
    if (!is.null(clDef) && isS4(object) && is.na(match(clDef@className, 
        .BasicClasses))) {
        cat("An object of class ", cl, "\n", sep = "")
        slots <- slotNames(clDef)
        dataSlot <- .dataSlot(slots)
        if (length(dataSlot) > 0) {
            dataPart <- slot(object, dataSlot)
            show(dataPart)
            slots <- slots[is.na(match(slots, dataSlot))]
        }
        else if (length(slots) == 0L) 
            show(unclass(object))
        for (what in slots) {
            if (identical(what, ".Data")) 
                next
            cat("Slot ", deparse(what), ":\n", sep = "")
            print(slot(object, what))
            cat("\n")
        }
    }
    else print(object, useS4 = FALSE)
    invisible()
}


reconcilePropertiesAndPrototype <- function (name, properties, prototype, superClasses, where) 
{
    StandardPrototype <- defaultPrototype()
    slots <- validSlotNames(allNames(properties))
    dataPartClass <- elNamed(properties, ".Data")
    dataPartValue <- FALSE
    if (!is.null(dataPartClass) && is.null(.validDataPartClass(dataPartClass, 
        where))) 
        stop(gettextf("in defining class %s, the supplied data part class, %s is not valid (must be a basic class or a virtual class combining basic classes)", 
            dQuote(name), dQuote(dataPartClass)), domain = NA)
    prototypeClass <- getClass(class(prototype), where = where)
    if ((!is.null(dataPartClass) || length(superClasses)) && 
        is.na(match("VIRTUAL", superClasses))) {
        for (cl in superClasses) {
            clDef <- getClassDef(cl, where = where)
            if (is.null(clDef)) 
                stop(gettextf("no definition was found for superclass %s in the specification of class %s", 
                  dQuote(cl), dQuote(name)), domain = NA)
            thisDataPart <- .validDataPartClass(clDef, where, 
                dataPartClass)
            if (!is.null(thisDataPart)) {
                dataPartClass <- thisDataPart
                if (!is.null(clDef@prototype)) {
                  newObject <- clDef@prototype
                  dataPartValue <- TRUE
                }
            }
        }
        if (length(dataPartClass)) {
            if (is.na(match(".Data", slots))) {
                properties <- c(list(.Data = dataPartClass), 
                  properties)
                slots <- names(properties)
            }
            else if (!extends(elNamed(properties, ".Data"), dataPartClass)) 
                stop(gettextf("conflicting definition of data part: .Data = %s, superclass implies %s", 
                  dQuote(elNamed(properties, ".Data")), dQuote(dataPartClass)), 
                  domain = NA)
            if (is.null(prototype)) {
                if (dataPartValue) 
                  prototype <- newObject
                else if (isVirtualClass(dataPartClass, where = where)) 
                  prototype <- newBasic("logical")
                else prototype <- new(dataPartClass)
                prototypeClass <- getClass(class(prototype), 
                  where = where)
            }
            else {
                if (extends(prototypeClass, "classPrototypeDef")) {
                  hasDataPart <- identical(prototype@dataPart, 
                    TRUE)
                  if (!hasDataPart) {
                    if (!dataPartValue) 
                      newObject <- new(dataPartClass)
                    pobject <- prototype@object
                    anames <- names(attributes(pobject))
                    attributes(newObject)[anames] <- attributes(pobject)
                    prototype@object <- newObject
                  }
                  else if (!extends(getClass(class(prototype@object), 
                    where = where), dataPartClass)) 
                    stop(gettextf("a prototype object was supplied with object slot of class %s, but the class definition requires an object that is class %s", 
                      dQuote(class(prototype@object)), dQuote(dataPartClass)), 
                      domain = NA)
                }
                else if (!extends(prototypeClass, dataPartClass)) 
                  stop(gettextf("a prototype was supplied of class %s, but the class definition requires an object that is class %s", 
                    dQuote(class(prototype)), dQuote(dataPartClass)), 
                    domain = NA)
            }
        }
        if (is.null(prototype)) {
            prototype <- StandardPrototype
        }
    }
    allProps <- properties
    for (cl in superClasses) {
        clDef <- getClassDef(cl, where)
        if (is(clDef, "classRepresentation")) {
            theseProperties <- getSlots(clDef)
            theseSlots <- names(theseProperties)
            theseSlots <- theseSlots[theseSlots == ".Data"]
            dups <- !is.na(match(theseSlots, allProps))
            for (dup in theseSlots[dups]) if (!extends(elNamed(allProps, 
                dup), elNamed(theseProperties, dup))) 
                stop(gettextf("slot %s in class %s currently defined (or inherited) as \"%s\", conflicts with an inherited definition in class %s", 
                  sQuote(dup), dQuote(name), elNamed(allProps, 
                    dup), dQuote(cl)), domain = NA)
            theseSlots <- theseSlots[!dups]
            if (length(theseSlots)) 
                allProps[theseSlots] <- theseProperties[theseSlots]
        }
        else stop(gettextf("class %s extends an undefined class (%s)", 
            dQuote(name), dQuote(cl)), domain = NA)
    }
    if (is.null(dataPartClass)) {
        if (extends(prototypeClass, "classPrototypeDef")) {
        }
        else {
            if (is.list(prototype)) 
                prototype <- do.call("prototype", prototype)
            if (is.null(prototype)) 
                prototype <- StandardPrototype
        }
    }
    else {
        dataPartDef <- getClass(dataPartClass)
        checkDataPart <- !isXS3Class(dataPartDef)
        if (checkDataPart) 
            checkDataPart <- ((is.na(match(dataPartClass, .BasicClasses)) && 
                !isVirtualClass(dataPartDef)) || length(dataPartDef@slots))
        if (checkDataPart) 
            stop(gettextf("%s is not eligible to be the data part of another class (must be a basic class or a virtual class with no slots)", 
                dQuote(dataPartClass)), domain = NA)
        if (extends(prototypeClass, "classPrototypeDef")) {
        }
        else if (extends(prototypeClass, dataPartClass)) {
            if (extends(prototypeClass, "list") && length(names(prototype))) 
                warning("prototype is a list with named elements (could be ambiguous):  better to use function prototype() to avoid trouble.")
        }
        else if (is.list(prototype)) 
            prototype <- do.call("prototype", prototype)
    }
    if (extends(prototypeClass, "classPrototypeDef")) {
        pnames <- prototype@slots
        prototype <- prototype@object
        if (length(superClasses) == 0L && any(is.na(match(pnames, 
            slots)))) 
            stop(sprintf(ngettext(sum(is.na(match(pnames, slots))), 
                "named elements of prototype do not correspond to slot name: %s", 
                "named elements of prototype do not correspond to slot names: %s"), 
                paste(.dQ(pnames[is.na(match(pnames, slots))]), 
                  collapse = ", ")), domain = NA)
    }
    else pnames <- allNames(attributes(prototype))
    what <- seq_along(properties)
    props <- properties[what]
    what <- slots[what]
    nm <- names(attributes(prototype))
    for (i in seq_along(what)) {
        propName <- el(what, i)
        if (!identical(propName, ".Data") && !propName %in% nm) 
            slot(prototype, propName, FALSE) <- tryNew(el(props, 
                i), where)
    }
    list(properties = properties, prototype = prototype)
}


removeGeneric <- function (f, where = topenv(parent.frame())) 
{
    fdef <- NULL
    allEv <- findFunction(f, where = where)
    for (maybeEv in allEv) {
        fdef <- get(f, maybeEv)
        if (is(fdef, "genericFunction")) 
            break
    }
    found <- is(fdef, "genericFunction")
    if (found) {
        .removeMethodsMetaTable(fdef, where)
        oldMetaName <- methodsPackageMetaName("M", fdef@generic, 
            fdef@package)
        if (exists(oldMetaName, where, inherits = FALSE)) 
            rm(list = oldMetaName, pos = where)
        .uncacheGeneric(f, fdef)
        rm(list = fdef@generic, pos = where)
    }
    else {
        if (!is.character(f)) 
            f <- deparse(f)
        warning(gettextf("generic function %s not found for removal", 
            sQuote(f)), domain = NA)
    }
    return(found)
}


getRefClass <- function (Class, where = topenv(parent.frame())) 
{
    if (is(Class, "refClassRepresentation")) {
        classDef <- Class
        Class <- classDef@className
    }
    else if (is.character(Class)) {
        classDef <- getClass(Class, where = where)
        if (!is(classDef, "refClassRepresentation")) 
            stop(gettextf("class %s is defined but is not a reference class", 
                dQuote(Class)), domain = NA)
    }
    else stop(gettextf("class must be a reference class representation or a character string; got an object of class %s", 
        dQuote(class(Class))), domain = NA)
    generator <- new("refGeneratorSlot")
    env <- as.environment(generator)
    env$className <- Class
    env$def <- classDef
    classFun <- classGeneratorFunction(Class, where)
    classFun@package <- classDef@package
    new("refObjectGenerator", classFun, generator = generator)
}


.__C__anova.glm <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("anova.glm", package = "methods")
    , package = "methods"
    , subclasses = structure(list(anova.glm.null = S4_object()), .Names = "anova.glm.null")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


addNextMethod <- function (method, f = "<unknown>", mlist, optional = FALSE, envir) 
standardGeneric("addNextMethod")


.__C__packageIQR <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("packageIQR", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


Compare <- function (e1, e2) 
standardGeneric("Compare")


MethodsListSelect <- function (f, env, mlist = NULL, fEnv = if (is(fdef, "genericFunction")) environment(fdef) else baseenv(), 
    finalDefault = finalDefaultMethod(mlist), evalArgs = TRUE, 
    useInherited = TRUE, fdef = getGeneric(f, where = env), resetAllowed = TRUE) 
{
    .MlistDeprecated("MethodsListSelect()")
    if (!resetAllowed) 
        resetMlist <- .getMethodsForDispatch(fdef)
    if (is.null(f)) {
    }
    else {
        fMethods <- .getMethodsForDispatch(fdef)
        if (is.null(mlist) || (evalArgs && is.function(fMethods))) 
            mlist <- fMethods
    }
    resetNeeded <- .setIfBase(f, fdef, mlist)
    if (resetNeeded) {
        on.exit(.setMethodsForDispatch(f, fdef, mlist))
    }
    if (!is(mlist, "MethodsList")) {
        if (is.function(mlist)) {
            on.exit()
            return(mlist)
        }
        if (is.null(f)) 
            stop("invalid method sublist")
        else if (!is.null(mlist)) 
            stop(gettextf("%f is not a valid generic function: methods list was an object of class %s", 
                sQuote(f), dQuote(class(mlist))), domain = NA)
    }
    if (!is.logical(useInherited)) 
        stop(gettextf("%s must be TRUE, FALSE, or a named logical vector of those values; got an object of class %s", 
            sQuote("useInherited"), dQuote(class(useInherited))), 
            domain = NA)
    if (identical(mlist, .getMethodsForDispatch(fdef))) {
        resetNeeded <- TRUE
        .setMethodsForDispatch(f, fdef, finalDefault)
        if (is(mlist, "MethodsList")) {
            on.exit(.setMethodsForDispatch(f, fdef, mlist))
        }
    }
    argName <- slot(mlist, "argument")
    arg <- NULL
    if (evalArgs) {
        if (missingArg(argName, env, TRUE)) 
            thisClass <- "missing"
        else {
            arg <- eval(as.name(argName), env)
            if (missing(arg)) 
                return(finalDefault)
            thisClass <- .class1(arg)
        }
    }
    else thisClass <- get(as.character(argName), envir = env, 
        inherits = FALSE)
    if (identical(useInherited, TRUE) || identical(useInherited, 
        FALSE)) 
        thisInherit <- nextUseInherited <- useInherited
    else {
        which <- match(as.character(argName), names(useInherited))
        if (is.na(which)) {
            nextUseInherited <- useInherited
            thisInherit <- TRUE
        }
        else {
            thisInherit <- useInherited[[which]]
            nextUseInherited <- useInherited[-which]
        }
    }
    fromClass <- thisClass
    allMethods <- mlist@allMethods
    which <- match(thisClass, names(allMethods))
    inherited <- is.na(which)
    selection <- if (inherited) 
        NULL
    else allMethods[[which]]
    if (!inherited) {
        if (is(selection, "function")) {
            if (is.null(f)) {
                mlist <- .trimMlist(mlist, fromClass)
            }
            value <- mlist
        }
        else {
            method <- Recall(NULL, env, selection, finalDefault = finalDefault, 
                evalArgs = evalArgs, useInherited = nextUseInherited, 
                fdef = fdef, )
            if (is(method, "EmptyMethodsList")) 
                value <- method
            else {
                mlist@allMethods[[which]] <- method
                value <- mlist
            }
        }
    }
    if (inherited || is(value, "EmptyMethodsList")) {
        method <- NULL
        if (thisInherit) {
            allSelections <- inheritedSubMethodLists(arg, fromClass, 
                mlist, env)
            allClasses <- names(allSelections)
            for (i in seq_along(allSelections)) {
                selection <- allSelections[[i]]
                fromClass <- allClasses[[i]]
                if (is(selection, "function")) 
                  method <- selection
                else if (is(selection, "MethodsList")) {
                  method <- Recall(NULL, env, selection, finalDefault = finalDefault, 
                    evalArgs = evalArgs, useInherited = nextUseInherited, 
                    fdef = fdef)
                  if (is(method, "EmptyMethodsList")) 
                    selection <- method
                }
                if (!is(selection, "EmptyMethodsList")) 
                  break
            }
        }
        if ((is.null(selection) || is(selection, "EmptyMethodsList")) && 
            !is.null(f) && !is.null(finalDefault)) {
            method <- finalDefault
            fromClass <- "ANY"
        }
        if (is.null(method) || is(method, "EmptyMethodsList")) 
            value <- emptyMethodsList(mlist, thisClass)
        else {
            method <- MethodAddCoerce(method, argName, thisClass, 
                fromClass)
            value <- .insertCachedMethods(mlist, as.character(argName), 
                thisClass, fromClass, method)
        }
    }
    if (!is.null(f)) {
        if (is(value, "EmptyMethodsList")) 
            value <- NULL
        if (resetNeeded) {
            on.exit()
            if (resetAllowed) {
                if (is.null(value)) 
                  resetMlist <- mlist
                else resetMlist <- value
            }
            .setMethodsForDispatch(f, fdef, resetMlist)
            if (dispatchIsInternal(fdef)) 
                setPrimitiveMethods(f, finalDefault, "set", fdef, 
                  resetMlist)
        }
    }
    value
}


as <- function (object, Class, strict = TRUE, ext = possibleExtends(thisClass, 
    Class)) 
{
    if (.identC(Class, "double")) 
        Class <- "numeric"
    thisClass <- .class1(object)
    if (.identC(thisClass, Class) || .identC(Class, "ANY")) 
        return(object)
    where <- .classEnv(thisClass, mustFind = FALSE)
    coerceFun <- getGeneric("coerce", where = where)
    coerceMethods <- .getMethodsTable(coerceFun, environment(coerceFun), 
        inherited = TRUE)
    asMethod <- .quickCoerceSelect(thisClass, Class, coerceFun, 
        coerceMethods, where)
    if (is.null(asMethod)) {
        sig <- c(from = thisClass, to = Class)
        asMethod <- selectMethod("coerce", sig, optional = TRUE, 
            useInherited = FALSE, fdef = coerceFun, mlist = getMethodsForDispatch(coerceFun))
        if (is.null(asMethod)) {
            canCache <- TRUE
            inherited <- FALSE
            if (is(object, Class)) {
                ClassDef <- getClassDef(Class, where)
                if (identical(ext, FALSE)) 
                  stop(sprintf("internal problem in as(): %s is(object, \"%s\") is TRUE, but the metadata asserts that the 'is' relation is FALSE", 
                    dQuote(thisClass), Class), domain = NA)
                else if (identical(ext, TRUE)) 
                  asMethod <- .makeAsMethod(quote(from), TRUE, 
                    Class, ClassDef, where)
                else {
                  test <- ext@test
                  asMethod <- .makeAsMethod(ext@coerce, ext@simple, 
                    Class, ClassDef, where)
                  canCache <- (!is(test, "function")) || identical(body(test), 
                    TRUE)
                }
            }
            if (is.null(asMethod) && extends(Class, thisClass)) {
                ClassDef <- getClassDef(Class, where)
                asMethod <- .asFromReplace(thisClass, Class, 
                  ClassDef, where)
            }
            if (is.null(asMethod)) {
                asMethod <- selectMethod("coerce", sig, optional = TRUE, 
                  c(from = TRUE, to = FALSE), fdef = coerceFun, 
                  mlist = coerceMethods)
                inherited <- TRUE
            }
            else if (canCache) 
                asMethod <- .asCoerceMethod(asMethod, thisClass, 
                  ClassDef, FALSE, where)
            if (is.null(asMethod)) 
                stop(gettextf("no method or default for coercing %s to %s", 
                  dQuote(thisClass), dQuote(Class)), domain = NA)
            else if (canCache) {
                cacheMethod("coerce", sig, asMethod, fdef = coerceFun, 
                  inherited = inherited)
            }
        }
    }
    if (strict) 
        asMethod(object)
    else asMethod(object, strict = FALSE)
}


.__C__standardGenericWithTrace <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    generic = structure("character", package = "methods"), package = structure("character", package = "methods"), 
    group = structure("list", package = "methods"), valueClass = structure("character", package = "methods"), 
    signature = structure("character", package = "methods"), 
    default = structure("optionalMethod", package = "methods"), 
    skeleton = structure("call", package = "methods"), original = structure("PossibleMethod", package = "methods"), 
    source = structure("environment", package = "methods")), .Names = c(".Data", 
"generic", "package", "group", "valueClass", "signature", "default", 
"skeleton", "original", "source"))
    , contains = structure(list(standardGeneric = S4_object(), 
    traceable = S4_object(), 
    genericFunction = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object(), 
    optionalMethod = S4_object()), .Names = c("standardGeneric", 
"traceable", "genericFunction", "function", "OptionalFunction", 
"PossibleMethod", "optionalMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("standardGenericWithTrace", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


isXS3Class <- function (classDef) 
{
    ".S3Class" %in% names(classDef@slots)
}


languageEl <- function (object, which) 
{
    data <- as.list(object)
    if (is.character(which)) 
        data[[which]]
    else if (typeof(object) == "language") {
        if (isGrammarSymbol(data[[1L]])) 
            data[[which + 1]]
        else data[[which]]
    }
    else data[[which]]
}


removeClass <- function (Class, where = topenv(parent.frame()), resolve.msg = getOption("removeClass.msg", 
    default = TRUE)) 
{
    if (missing(where)) {
        classEnv <- .classEnv(Class, where, FALSE)
        classWhere <- findClass(Class, where = classEnv)
        if (length(classWhere) == 0L) {
            warning(gettextf("class definition for %s not found (no action taken)", 
                dQuote(Class)), domain = NA)
            return(FALSE)
        }
        if (length(classWhere) > 1L) 
            warning(gettextf("class %s has multiple definitions visible; only the first removed", 
                dQuote(Class)), domain = NA)
        classWhere <- classWhere[[1L]]
    }
    else classWhere <- where
    classDef <- getClassDef(Class, where = classWhere)
    if (length(classDef@subclasses)) {
        subclasses <- names(classDef@subclasses)
        found <- vapply(subclasses, isClass, NA, where = where, 
            USE.NAMES = TRUE)
        for (what in subclasses[found]) .removeSuperClass(what, 
            Class, resolve.msg = resolve.msg)
    }
    .removeSuperclassBackRefs(Class, classDef, classWhere)
    .uncacheClass(Class, classDef)
    .undefineMethod("initialize", Class, classWhere)
    what <- classMetaName(Class)
    rm(list = what, pos = classWhere)
    TRUE
}


.__C__nonStructure <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = structure("nonStructure", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


callNextMethod <- function (...) 
{
    method <- nextMethod <- NULL
    dotNextMethod <- as.name(".nextMethod")
    parent <- sys.parent(1)
    maybeMethod <- sys.function(parent)
    if (is(maybeMethod, "MethodDefinition")) {
        callEnv <- methodEnv <- parent.frame(1)
        mcall <- sys.call(parent)
        dotsenv <- parent.frame(2)
        i <- 1L
    }
    else {
        callEnv <- parent.frame(1)
        methodEnv <- parent.frame(2)
        mcall <- sys.call(sys.parent(2))
        dotsenv <- parent.frame(3)
        i <- 2L
    }
    if (!is.null(method <- methodEnv$.Method)) {
        nextMethod <- callEnv$.nextMethod
        f <- methodEnv$.Generic
    }
    else if (identical(mcall[[1L]], dotNextMethod)) {
        nextMethodEnv <- parent.frame(i + 1L)
        nextMethod <- nextMethodEnv$.nextMethod
        f <- nextMethodEnv$.Generic
    }
    else {
        if (is.primitive(mcall[[1L]])) {
            f <- .primname(mcall[[1L]])
        }
        else {
            f <- as.character(mcall[[1L]])
        }
        fdef <- genericForBasic(f)
        if (is.null(fdef)) 
            stop(gettextf("a call to callNextMethod() appears in a call to %s, but the call does not seem to come from either a generic function or another 'callNextMethod'", 
                sQuote(f)), domain = NA)
        f <- fdef@generic
        method <- maybeMethod
    }
    if (is(method, "MethodDefinition")) {
        if (is.null(nextMethod)) {
            if (!is(method, "MethodWithNext")) {
                method <- addNextMethod(method, f, envir = methodEnv)
                cacheMethod(f, method@target, method, fdef = getGeneric(f), 
                  inherited = TRUE)
            }
            nextMethod <- method@nextMethod
            assign(".nextMethod", nextMethod, envir = callEnv)
            assign(".Generic", f, envir = callEnv)
        }
    }
    else if (is.null(method)) {
        if (is.null(nextMethod)) 
            stop("call to 'callNextMethod' does not appear to be in a 'method' or 'callNextMethod' context")
        method <- nextMethod
        if (!is(method, "MethodWithNext")) {
            method <- addNextMethod(method, f, envir = methodEnv)
        }
        nextMethod <- method@nextMethod
        assign(".nextMethod", nextMethod, envir = callEnv)
        assign(".Generic", f, envir = callEnv)
        assign(".nextMethod", method, envir = nextMethodEnv)
        assign(".Generic", f, envir = nextMethodEnv)
    }
    else stop(gettextf("bad object found as method (class %s)", 
        dQuote(class(method))), domain = NA)
    if (is.null(nextMethod)) 
        stop("No next method available")
    subsetCase <- !is.na(match(f, .BasicSubsetFunctions))
    if (nargs() > 0) {
        call <- sys.call()
        call[[1L]] <- as.name(".nextMethod")
        eval(call, callEnv)
    }
    else {
        if (subsetCase) {
            call <- as.list(mcall)
            if ((f == "[") && length(names(call) > 0)) 
                call <- .doSubNextCall(call, method)
            else {
                fnames <- c("", formalArgs(method))
                i <- match("...", fnames)
                if (is.na(i) || i > length(call)) 
                  length(fnames) <- length(call)
                else {
                  i <- i - 1L
                  length(fnames) <- i
                  fnames <- c(fnames, rep("", length(call) - 
                    i))
                }
                if (substring(f, nchar(f) - 1L) == "<-") 
                  fnames[length(fnames)] <- "value"
                names(call) <- fnames
                call <- as.call(call)
            }
        }
        else call <- match.call(maybeMethod, mcall, expand.dots = FALSE, 
            envir = dotsenv)
        .Call(C_R_nextMethodCall, call, callEnv)
    }
}


defaultDumpName <- function (generic, signature) 
{
    if (missing(signature)) 
        paste(generic, "R", sep = ".", collapse = ".")
    else paste(generic, paste(signature, collapse = "."), "R", 
        sep = ".")
}


makeGeneric <- function (f, fdef, fdefault = fdef, group = list(), valueClass = character(), 
    package = getPackageName(environment(fdef)), signature = NULL, 
    genericFunction = NULL, simpleInheritanceOnly = NULL) 
{
    checkTrace <- function(fun, what, f) {
        if (is(fun, "traceable")) {
            warning(gettextf("the function being used as %s in making a generic function for %s is currently traced; the function used will have tracing removed", 
                what, sQuote(f)), domain = NA)
            .untracedFunction(fun)
        }
        else fun
    }
    if (missing(fdef)) {
        if (missing(fdefault)) 
            stop(gettextf("must supply either a generic function or a function as default for %s", 
                sQuote(f)), domain = NA)
        else if (isBaseFun(fdefault)) {
            fun <- genericForBasic(f)
            if (is.function(fun)) {
                return(fun)
            }
        }
        fdef <- fdefault
        body(fdef) <- substitute(standardGeneric(NAME), list(NAME = f))
        environment(fdef) <- asNamespace(package)
    }
    ev <- new.env()
    parent.env(ev) <- environment(fdef)
    environment(fdef) <- ev
    packageSlot(f) <- package
    assign(".Generic", f, envir = ev)
    fdef <- checkTrace(fdef)
    if (length(valueClass)) 
        fdef <- .ValidateValueClass(fdef, f, valueClass)
    group <- .asGroupArgument(group)
    if (is.null(genericFunction)) 
        value <- new("standardGeneric")
    else if (is(genericFunction, "genericFunction")) 
        value <- genericFunction
    else stop(gettextf("the %s argument must be NULL or a generic function object; got an object of class %s", 
        sQuote("genericFunction"), dQuote(class(genericFunction))), 
        domain = NA)
    value@.Data <- fdef
    value@generic <- f
    value@group <- group
    value@valueClass <- valueClass
    value@package <- package
    args <- formalArgs(fdef)
    if (is.null(signature)) 
        signature <- args
    else if (any(is.na(match(signature, args)))) 
        stop(sprintf(ngettext(sum(is.na(match(signature, args))), 
            "non-argument found in the signature: %s", "non-arguments found in the signature: %s"), 
            paste(signature[is.na(match(signature, args))], collapse = ", ")), 
            domain = NA)
    dots <- match("...", signature)
    if (!is.na(dots)) {
        if (length(signature) > 1L) 
            signature <- signature[-dots]
    }
    if (length(signature) == 0L) 
        stop("no suitable arguments to dispatch methods in this function")
    attr(signature, "simpleOnly") <- simpleInheritanceOnly
    value@signature <- signature
    if (is.null(fdefault)) {
    }
    else {
        fdefault <- checkTrace(fdefault)
        if (!identical(formalArgs(fdefault), formalArgs(fdef)) && 
            !is.primitive(fdefault)) 
            stop(sprintf(ngettext(length(fdef), "the formal argument of the generic function for %s (%s) differs from that of the non-generic to be used as the default (%s)", 
                "the formal arguments of the generic function for %s (%s) differ from those of the non-generic to be used as the default (%s)"), 
                f, paste(formalArgs(fdef), collapse = ", "), 
                paste(formalArgs(fdefault), collapse = ", ")), 
                domain = NA)
        fdefault <- asMethodDefinition(fdefault, fdef = value)
        if (is(fdefault, "MethodDefinition")) 
            fdefault@generic <- value@generic
    }
    value@default <- fdefault
    assign(".Methods", fdefault, envir = ev)
    .setupMethodsTables(value, TRUE)
    value@skeleton <- generic.skeleton(f, fdef, fdefault)
    value
}


isClass <- function (Class, formal = TRUE, where = topenv(parent.frame())) 
!is.null(getClassDef(Class, where))


`.__T__slotsFromS3:methods` <- "<environment>"

getAccess <- function (ClassDef) 
.Defunct()


.__C__.NULL <- new("classRepresentation"
    , slots = structure(list(.xData = structure("NULL", package = "methods")), .Names = ".xData")
    , contains = structure(list(`NULL` = S4_object(), 
    OptionalFunction = S4_object(), 
    optionalMethod = S4_object()), .Names = c("NULL", 
"OptionalFunction", "optionalMethod"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure(".NULL", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


getExtends <- function (ClassDef) 
.Defunct()


setGroupGeneric <- function (name, def = NULL, group = list(), valueClass = character(), 
    knownMembers = list(), package = getPackageName(where), where = topenv(parent.frame())) 
{
    if (is.null(def)) {
        def <- getFunction(name, where = where)
        if (isGroup(name, fdef = def)) {
            if (nargs() == 1) {
                message(gettextf("Function %s is already a group generic; no change", 
                  sQuote(name)), domain = NA)
                return(name)
            }
        }
    }
    body(def, envir = environment(def)) <- substitute(stop(MSG, 
        domain = NA), list(MSG = gettextf("Function %s is a group generic; do not call it directly", 
        sQuote(name))))
    if (is.character(knownMembers)) 
        knownMembers <- as.list(knownMembers)
    setGeneric(name, def, group = group, valueClass = valueClass, 
        package = package, useAsDefault = FALSE, genericFunction = new("groupGenericFunction", 
            def, groupMembers = knownMembers), where = where)
    .MakeImplicitGroupMembers(name, knownMembers, where)
    name
}


.__C__density <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("density", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


getFunction <- function (name, generic = TRUE, mustFind = TRUE, where = topenv(parent.frame())) 
{
    if (!nzchar(name)) 
        stop(gettextf("expected a non-empty character string for argument name"), 
            domain = NA)
    found <- FALSE
    where <- as.environment(where)
    f <- NULL
    lastEnv <- if (isNamespace(where)) 
        function(where) isBaseNamespace(where)
    else function(where) identical(where, baseenv())
    repeat {
        if (!is.null(f <- get0(name, envir = where, mode = "function", 
            inherits = FALSE))) 
            found <- generic || !is(f, "genericFunction")
        if (found || lastEnv(where)) 
            break
        where <- parent.env(where)
    }
    if (!found && mustFind) 
        stop(if (generic) 
            gettextf("no function %s found", sQuote(name))
        else gettextf("no non-generic function %s found", sQuote(name)), 
            domain = NA)
    f
}


.__C__logLik <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("logLik", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


classMetaName <- function (name) 
methodsPackageMetaName("C", name)


.__C__externalRefMethod <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    actual = structure("function", package = "methods"), mayCall = structure("character", package = "methods"), 
    name = structure("character", package = "methods"), refClassName = structure("character", package = "methods"), 
    superClassMethod = structure("SuperClassMethod", package = "methods")), .Names = c(".Data", 
"actual", "mayCall", "name", "refClassName", "superClassMethod"
))
    , contains = structure(list(refMethodDef = S4_object(), 
    `function` = S4_object(), 
    SuperClassMethod = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object()), .Names = c("refMethodDef", 
"function", "SuperClassMethod", "OptionalFunction", "PossibleMethod"
))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("externalRefMethod", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__MethodSelectionReport <- new("classRepresentation"
    , slots = structure(list(generic = structure("character", package = "methods"), 
    allSelections = structure("character", package = "methods"), 
    target = structure("character", package = "methods"), selected = structure("character", package = "methods"), 
    candidates = structure("list", package = "methods"), note = structure("character", package = "methods")), .Names = c("generic", 
"allSelections", "target", "selected", "candidates", "note"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("MethodSelectionReport", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


setClass <- function (Class, representation = list(), prototype = NULL, contains = character(), 
    validity = NULL, access = list(), where = topenv(parent.frame()), 
    version = .newExternalptr(), sealed = FALSE, package = getPackageName(where), 
    S3methods = FALSE, slots) 
{
    oldDef <- getClassDef(Class, where)
    if (is(oldDef, "classRepresentation") && oldDef@sealed) 
        stop(gettextf("%s has a sealed class definition and cannot be redefined", 
            dQuote(Class)), domain = NA)
    if (!missing(slots)) {
        if (!missing(representation)) 
            stop("Argument \"representation\" cannot be used if argument \"slots\" is supplied")
        properties <- inferProperties(slots, "slot")
        classDef <- makeClassRepresentation(Class, properties, 
            contains, prototype, package, validity, access, version, 
            sealed, where = where)
        superClasses <- names(classDef@contains)
    }
    else if (is(representation, "classRepresentation")) {
        classDef <- representation
        if (!(missing(prototype) && missing(contains) && missing(validity) && 
            missing(access) && missing(version) && missing(package))) 
            stop("only arguments 'Class' and 'where' can be supplied when argument 'representation' is a 'classRepresentation' object")
        if (length(classDef@package) == 0L) 
            classDef@package <- package
        superClasses <- allNames(classDef@contains)
    }
    else {
        if (is.character(representation) && length(representation) == 
            1L && is.null(names(representation))) 
            representation <- list(representation)
        slots <- nzchar(allNames(representation))
        superClasses <- c(as.character(representation[!slots]), 
            contains)
        properties <- representation[slots]
        classDef <- makeClassRepresentation(Class, properties, 
            superClasses, prototype, package, validity, access, 
            version, sealed, where = where)
        superClasses <- names(classDef@contains)
    }
    classDef <- completeClassDefinition(Class, classDef, where, 
        doExtends = FALSE)
    .uncacheClass(Class, classDef)
    if (length(superClasses) > 0L) {
        sealed <- classDef@sealed
        classDef@sealed <- FALSE
        assignClassDef(Class, classDef, where)
        badContains <- character()
        for (class2 in superClasses) {
            if (is(try(setIs(Class, class2, classDef = classDef, 
                where = where)), "try-error")) 
                badContains <- c(badContains, class2)
            else {
                classDef <- getClassDef(Class, where = where)
                if (is.null(classDef)) 
                  stop(sprintf("internal error: definition of class %s not properly assigned", 
                    dQuote(Class)), domain = NA)
            }
        }
        if (length(badContains)) {
            msg <- paste(.dQ(badContains), collapse = ", ")
            if (is(try(removeClass(Class, where)), "try-error")) 
                stop(gettextf("error in contained classes (%s) for class %s and unable to remove definition from %s", 
                  msg, dQuote(Class), sQuote(getPackageName(where))), 
                  domain = NA)
            if (is.null(oldDef)) 
                stop(gettextf("error in contained classes (%s) for class %s; class definition removed from %s", 
                  msg, dQuote(Class), sQuote(getPackageName(where))), 
                  domain = NA)
            else if (is(try(setClass(Class, oldDef, where = where)), 
                "try-error")) 
                stop(gettextf("error in contained classes (%s) for class %s and unable to restore previous definition from %s", 
                  msg, dQuote(Class), sQuote(getPackageName(where))), 
                  domain = NA)
            else stop(gettextf("error in contained classes (%s) for class %s; previous definition restored to %s", 
                msg, dQuote(Class), sQuote(getPackageName(where))), 
                domain = NA)
        }
        if (length(attr(classDef@contains, "conflicts")) > 0) 
            .reportSuperclassConflicts(Class, classDef@contains, 
                where)
        .checkRequiredGenerics(Class, classDef, where)
        if (sealed) {
            classDef@sealed <- TRUE
        }
    }
    if (S3methods) 
        classDef <- .setS3MethodsOn(classDef)
    assignClassDef(Class, classDef, where)
    invisible(classGeneratorFunction(classDef, where))
}


.valueClassTest <- function (object, classes, fname) 
{
    if (length(classes)) {
        for (Cl in classes) if (is(object, Cl)) 
            return(object)
        stop(gettextf("invalid value from generic function %s, class %s, expected %s", 
            sQuote(fname), dQuote(class(object)), paste(dQuote(classes), 
                collapse = " or ")), domain = NA)
    }
    object
}


.__C__optionalMethod <- new("classRepresentation"
    , slots = list()
    , contains = list()
    , virtual = TRUE
    , prototype = NULL
    , validity = NULL
    , access = list()
    , className = structure("optionalMethod", package = "methods")
    , package = "methods"
    , subclasses = structure(list(PossibleMethod = S4_object(), 
    `NULL` = S4_object(), 
    `function` = S4_object(), 
    classGeneratorFunction = S4_object(), 
    MethodDefinition = S4_object(), 
    derivedDefaultMethod = S4_object(), 
    internalDispatchMethod = S4_object(), 
    MethodWithNext = S4_object(), 
    SealedMethodDefinition = S4_object(), 
    genericFunction = S4_object(), 
    standardGeneric = S4_object(), 
    nonstandardGenericFunction = S4_object(), 
    groupGenericFunction = S4_object(), 
    nonstandardGroupGenericFunction = S4_object(), 
    .NULL = S4_object(), 
    MethodWithNextWithTrace = S4_object(), 
    genericFunctionWithTrace = S4_object(), 
    standardGenericWithTrace = S4_object(), 
    groupGenericFunctionWithTrace = S4_object(), 
    derivedDefaultMethodWithTrace = S4_object(), 
    refObjectGenerator = S4_object()), .Names = c("PossibleMethod", 
"NULL", "function", "classGeneratorFunction", "MethodDefinition", 
"derivedDefaultMethod", "internalDispatchMethod", "MethodWithNext", 
"SealedMethodDefinition", "genericFunction", "standardGeneric", 
"nonstandardGenericFunction", "groupGenericFunction", "nonstandardGroupGenericFunction", 
".NULL", "MethodWithNextWithTrace", "genericFunctionWithTrace", 
"standardGenericWithTrace", "groupGenericFunctionWithTrace", 
"derivedDefaultMethodWithTrace", "refObjectGenerator"))
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


getGroup <- function (fdef, recursive = FALSE, where = topenv(parent.frame())) 
{
    if (is.character(fdef)) 
        fdef <- getGeneric(fdef, where = where)
    if (is(fdef, "genericFunction")) 
        group <- fdef@group
    else group <- list()
    if (recursive && length(group)) {
        allGroups <- group
        for (gp in group) {
            fgp <- getGeneric(gp, where = where)
            if (is(fgp, "groupGenericFunction")) 
                allGroups <- c(allGroups, Recall(fgp, TRUE, where))
        }
        if (length(allGroups) > 1L) {
            ids <- sapply(allGroups, function(x) {
                pkg <- packageSlot(x)
                if (is.null(pkg)) 
                  x
                else paste(x, pkg, sep = ":")
            })
            allGroups <- allGroups[!duplicated(ids)]
        }
        allGroups
    }
    else group
}


checkSlotAssignment <- function (obj, name, value) 
{
    cl <- class(obj)
    ClassDef <- getClass(cl)
    slotClass <- ClassDef@slots[[name]]
    if (is.null(slotClass)) 
        stop(gettextf("%s is not a slot in class %s", sQuote(name), 
            dQuote(cl)), domain = NA)
    valueClass <- class(value)
    if (.identC(slotClass, valueClass)) 
        return(value)
    ok <- possibleExtends(valueClass, slotClass, ClassDef2 = getClassDef(slotClass, 
        where = .classEnv(ClassDef)))
    if (identical(ok, FALSE)) 
        stop(gettextf("assignment of an object of class %s is not valid for slot %s in an object of class %s; is(value, \"%s\") is not TRUE", 
            dQuote(valueClass), sQuote(name), dQuote(cl), slotClass), 
            domain = NA)
    else if (identical(ok, TRUE)) 
        value
    else as(value, slotClass, strict = FALSE, ext = ok)
}


implicitGeneric <- function (name, where = topenv(parent.frame()), generic = getGeneric(name, 
    where = where)) 
{
    if (!nzchar(name)) 
        stop(gettextf("expected a non-empty character string for argument name"), 
            domain = NA)
    if (!missing(generic) && is(generic, "genericFunction") && 
        !.identC(name, generic@generic)) 
        stop(gettextf("generic function supplied was not created for %s", 
            sQuote(name)), domain = NA)
    createGeneric <- (missing(generic) || !is(generic, "genericFunction")) && 
        !isGeneric(name, where)
    if (createGeneric) {
        fdefault <- getFunction(name, where = where, mustFind = FALSE)
        if (is.null(fdefault)) 
            return(NULL)
        env <- environment(fdefault)
        fdefault <- .derivedDefaultMethod(fdefault)
        if (isBaseFun(fdefault)) {
            value <- genericForBasic(name)
            if (is.function(value)) {
                if (!missing(generic) && !identical(value, generic)) 
                  stop(gettextf("%s is a primitive function; its generic form cannot be redefined", 
                    sQuote(name)), domain = NA)
                generic <- value
                fdefault <- generic@default
            }
            package <- "base"
        }
        else package <- getPackageName(env)
        group <- .getImplicitGroup(name, if (identical(package, 
            "base")) 
            .methodsNamespace
        else environment(fdefault))
        if (missing(generic)) {
            generic <- .getImplicitGeneric(name, env, package)
            if (is.null(generic)) {
                generic <- makeGeneric(name, fdefault = fdefault, 
                  package = package, group = group)
                .cacheImplicitGeneric(name, generic)
            }
        }
        else {
            generic <- makeGeneric(name, generic, fdefault, package = package, 
                group = group)
            .cacheImplicitGeneric(name, generic)
        }
    }
    generic
}


getAllSuperClasses <- function (ClassDef, simpleOnly = TRUE) 
{
    temp <- superClassDepth(ClassDef, simpleOnly = simpleOnly)
    unique(temp$label[sort.list(temp$depth)])
}


.__C__ts <- new("classRepresentation"
    , slots = structure(list(.Data = structure("vector", package = "methods"), 
    tsp = structure("numeric", package = "methods"), .S3Class = structure("character", package = "methods")), .Names = c(".Data", 
"tsp", ".S3Class"))
    , contains = structure(list(structure = S4_object(), 
    oldClass = S4_object(), 
    vector = S4_object()), .Names = c("structure", 
"oldClass", "vector"))
    , virtual = FALSE
    , prototype = structure(NA, .Tsp = c(1, 1, 1), .S3Class = "ts")
    , validity = NULL
    , access = list()
    , className = structure("ts", package = "methods")
    , package = "methods"
    , subclasses = structure(list(mts = S4_object()), .Names = "mts")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


getGroupMembers <- function (group, recursive = FALSE, character = TRUE) 
{
    .recMembers <- function(members, where) {
        all = vector("list", length(members))
        for (i in seq_along(members)) {
            what <- members[[i]]
            f <- getGeneric(what, FALSE, where)
            if (!is.null(f)) 
                all[[i]] <- what
            if (is(f, "groupGenericFunction")) {
                newMem <- f@groupMembers
                all <- c(all, Recall(newMem, where))
            }
        }
        all
    }
    f <- getGeneric(group)
    if (is.null(f)) {
        warning(gettextf("%s is not a generic function (or not visible here)", 
            sQuote(f)), domain = NA)
        return(character())
    }
    else if (!is(f, "groupGenericFunction")) 
        character()
    else {
        members <- f@groupMembers
        if (recursive) {
            where <- f@package
            if (identical(where, "base")) {
                where <- "methods"
                members <- .recMembers(members, .methodsNamespace)
            }
            else members <- .recMembers(members, .asEnvironmentPackage(where))
        }
        if (character) 
            sapply(members, function(x) {
                if (is(x, "character")) 
                  x
                else if (is(x, "genericFunction")) 
                  x@generic
                else stop(gettextf("invalid element in the \"groupMembers\" slot (class %s)", 
                  dQuote(class(x))), domain = NA)
            })
        else members
    }
}


traceOn <- function (what, tracer = browseAll, exit = NULL) 
{
    browseAll <- function() .Defunct()
    .Defunct("trace")
}


.__C__MethodWithNext <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    nextMethod = structure("PossibleMethod", package = "methods"), 
    excluded = structure("list", package = "methods"), target = structure("signature", package = "methods"), 
    defined = structure("signature", package = "methods"), generic = structure("character", package = "methods")), .Names = c(".Data", 
"nextMethod", "excluded", "target", "defined", "generic"))
    , contains = structure(list(MethodDefinition = S4_object(), 
    `function` = S4_object(), 
    PossibleMethod = S4_object(), 
    OptionalFunction = S4_object(), 
    optionalMethod = S4_object()), .Names = c("MethodDefinition", 
"function", "PossibleMethod", "OptionalFunction", "optionalMethod"
))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("MethodWithNext", package = "methods")
    , package = "methods"
    , subclasses = structure(list(MethodWithNextWithTrace = S4_object()), .Names = "MethodWithNextWithTrace")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__logical <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(vector = S4_object()), .Names = "vector")
    , virtual = FALSE
    , prototype = logical(0)
    , validity = NULL
    , access = list()
    , className = "logical"
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


setDataPart <- function (object, value, check = TRUE) 
{
    if (check || identical(typeof(object), "S4")) {
        classDef <- getClass(class(object))
        slots <- getSlots(classDef)
        dataSlot <- .dataSlot(names(slots))
        if (length(dataSlot) == 1) 
            dataClass <- elNamed(slots, dataSlot)
        else if (check) 
            stop(gettextf("class %s does not have a data part (a .Data slot) defined", 
                dQuote(class(object))), domain = NA)
        else return(.mergeAttrs(value, object))
        value <- as(value, dataClass)
        if (identical(typeof(object), "S4")) {
            if (is.null(value)) 
                value <- .pseudoNULL
            attr(object, dataSlot) <- value
            return(object)
        }
    }
    .mergeAttrs(value, object)
}


getMethods <- function (f, where = topenv(parent.frame()), table = FALSE) 
{
    if (!table) 
        .MlistDefunct("getMethods", "findMethods")
    nowhere <- missing(where)
    fdef <- getGeneric(f, where = where)
    f <- fdef@generic
    if (!is.null(fdef)) {
        if (table) 
            return(getMethodsForDispatch(fdef, TRUE))
    }
}


.__C__defaultBindingFunction <- new("classRepresentation"
    , slots = structure(list(.Data = structure("function", package = "methods"), 
    field = structure("character", package = "methods"), className = structure("character", package = "methods")), .Names = c(".Data", 
"field", "className"))
    , contains = structure(list(activeBindingFunction = S4_object(), 
    `function` = S4_object(), 
    OptionalFunction = S4_object(), 
    PossibleMethod = S4_object()), .Names = c("activeBindingFunction", 
"function", "OptionalFunction", "PossibleMethod"))
    , virtual = FALSE
    , prototype = new("function"
)
    , validity = NULL
    , access = list()
    , className = structure("defaultBindingFunction", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


findMethodSignatures <- function (..., target = TRUE, methods = findMethods(...)) 
{
    what <- methods@arguments
    if (target) 
        sigs <- methods@signatures
    else {
        anySig <- rep("ANY", length(what))
        for (m in methods) if (!is.primitive(m)) {
            length(anySig) <- length(m@defined)
            break
        }
        sigs <- lapply(methods, function(x) if (is.primitive(x)) 
            anySig
        else as.character(x@defined))
    }
    lens <- unique(vapply(sigs, length, 1, USE.NAMES = FALSE))
    if (length(lens) == 0) 
        return(matrix(character(), 0, length(methods@arguments)))
    if (length(lens) > 1L) {
        lens <- max(lens)
        anys <- rep("ANY", lens)
        sigs <- lapply(sigs, function(x) {
            if (length(x) < lens) {
                anys[seq_along(x)] <- x
                anys
            }
            else x
        })
    }
    length(what) <- lens
    t(matrix(unlist(sigs), nrow = lens, dimnames = list(what, 
        NULL)))
}


.__C__.Other <- new("classRepresentation"
    , slots = structure(list(label = structure("character", package = "methods")), .Names = "label")
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure(".Other", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


emptyMethodsList <- function (mlist, thisClass = "ANY", sublist = list()) 
{
    .MlistDeprecated("emptyMethodsList()")
    sublist[thisClass] <- list(NULL)
    new("EmptyMethodsList", argument = mlist@argument, sublist = sublist)
}


.__C__summary.table <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = FALSE
    , prototype = structure(list(n.vars = 1L, n.cases = 0L), .Names = c("n.vars", 
"n.cases"), class = "summary.table", .S3Class = "summary.table")
    , validity = NULL
    , access = list()
    , className = structure("summary.table", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.slotNames <- function (x) 
{
    classDef <- getClassDef(if (!isS4(x) && is.character(x) && 
        length(x) == 1L) 
        x
    else class(x))
    if (is.null(classDef)) 
        character()
    else names(classDef@slots)
}


.__C__mlm <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(lm = S4_object(), 
    oldClass = S4_object()), .Names = c("lm", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("mlm", package = "methods")
    , package = "methods"
    , subclasses = structure(list(maov = S4_object()), .Names = "maov")
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


.__C__sourceEnvironment <- new("classRepresentation"
    , slots = structure(list(packageName = structure("character", package = "methods"), 
    dateCreated = structure("POSIXt", package = "methods"), sourceFile = structure("character", package = "methods"), 
    .xData = structure("environment", package = "methods")), .Names = c("packageName", 
"dateCreated", "sourceFile", ".xData"))
    , contains = structure(list(.environment = S4_object(), 
    environment = S4_object(), 
    refObject = S4_object()), .Names = c(".environment", 
"environment", "refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("sourceEnvironment", package = "methods")
    , package = "methods"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = TRUE
)


`.__T__Summary:base` <- "<environment>"

`.__T__Math:base` <- "<environment>"

`functionBody<-` <- function (fun, envir = environment(fun), value) 
{
    if (is.expression(value)) {
        if (length(value) > 1L) 
            warning("using the first element of 'value' of type \"expression\"")
        value <- value[[1L]]
    }
    as.function(c(as.list(formals(fun)), list(value)), envir)
}


`.__T__loadMethod:methods` <- "<environment>"

`.__T__kronecker:base` <- "<environment>"



## Package Data

# none


## Package Info

.skeleton_package_title = "Formal Methods and Classes"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF