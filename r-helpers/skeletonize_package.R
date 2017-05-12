#!/usr/bin/env Rscript

args = commandArgs(TRUE)

if (length(args) != 2) {
    warning("Usage: skeletonize_package.R <package_name> <output_file>")
    quit(save = "no", status = 1, runLast = FALSE)
}

pName = args[1]
skeletonFile = args[2]

## test invcation:
# pName = "base"; skeletonFile=paste0("~/Desktop/skeleton/", pName); args= c(pName, skeletonFile)
# pName = "tools"; skeletonFile="tools.skeleton.R"
# pName = "ggplot2"; skeletonFile=paste0("~/Desktop/skeleton/", pName)
# pName = "xlsx"; skeletonFile=paste0("~/Desktop/skeleton/", pName)
# pName = "dplyr"; skeletonFile=paste0("~/Desktop/skeleton/", pName)
# pName = "graphics"; skeletonFile=paste0("~/Desktop/skeleton/", pName)
# pName = "fields"; skeletonFile=paste0("~/Desktop/skeleton/", pName)
# pName = "R.utils"; skeletonFile=paste0("~/Desktop/skeleton/", pName)


searchPath = search()

is_identifier = function(str) {
 return(grepl("^([[:alpha:]]|_|\\.)([[:alpha:]]|[[:digit:]]|_|\\.)*$", str) == TRUE)
}


shouldLoadLibrary = FALSE
pckgPrefixed = paste("package", pName, sep=":")

if (!pckgPrefixed %in% searchPath){
    shouldLoadLibrary = TRUE
}

if (shouldLoadLibrary) {
    library(package=pName, character.only=TRUE)
}

# functions = as.character(lsf.str(paste("package", pName, sep=":")))
# http://stackoverflow.com/questions/9658518/list-exported-objects-from-r-package-without-attaching-it
functions = getNamespaceExports(pName)

# "filter" %in% functions

# dir.create(skelBaseDirectory)

print(paste("writing skeleton of ",pName, "into", skeletonFile))

#' some symbols are defined as functions in base.R but our parser does not like it.
ignoreList = c("for", "function", "if", "repeat", "while")

sink(skeletonFile)
cat(paste0("##\n## Exported symobls in package `",pName, "`\n##\n\n"))
cat("## Exported package methods\n\n")
sink()


# http://stackoverflow.com/questions/26174703/get-namespace-of-function
detect_declaring_ns = function(symbol){
    ## we need to distinguish reexported from masked symbols here (better solution?)
    ## essentially there are 3 classes
    # symbol="glimpse" ## true rexports
    # symbol="filter"  ## masking stats-package methods
    # symbol="mutate"  ## own ns-exports
    # symbol="pairs.default"

    resolveResult = getAnywhere(symbol)

    ## remove masked symbols
    # origin = resolveResult$where
    # origin=resolveResult$where[resolveResult$dups]
    # just keep namespaces and take the first one
    origin = resolveResult$where
    origin = origin[grepl("namespace:", origin)][1]
    # last = origin[length(origin)]
    unlist(strsplit(origin, ":"))[2]
}

get_text_of_object = function(tmpFile, obj, use_dput){
    sink(tmpFile)
    if (use_dput) {
        dput(obj)
    }else {
        # there is no print for certain methods like R.utils::symbol that's we we need to wrap it with tryCatch
        # print(obj)
        tryCatch(print(obj), error = function(e) "")
    }
    sink()

    ## read the result back into a character vector
    fileObj = file(tmpFile)
    lines = readLines(fileObj)
    close(fileObj)

    lines
}



get_text_of = function(obj){
    # obj = get("as.character")
    # require(ggplot2); obj = get(".pt")
    # require(ggplot2); obj = get("GeomBar")
    # require(dplyr); obj = get("data_frame")
    # require(lubridate); obj = get(".__C__Interval")

    tmpFileName = tempfile(pattern = "tmp", tmpdir = tempdir(), fileext = "")

    lines = get_text_of_object(tmpFileName, obj, use_dput = FALSE)

    ## use it if it's a function declaration
    if (grepl('^(function|new)', lines[1])) {
        return(lines)
    }

    ## and fall back to print if not
    # http://stackoverflow.com/questions/31467732/does-r-have-function-startswith-or-endswith-like-python

    raw_text = get_text_of_object(tmpFileName, obj, use_dput = TRUE)
}

# http://stackoverflow.com/questions/2261079/how-to-trim-leading-and-trailing-whitespace-in-r
trim = function (x) gsub("^\\s+|\\s+$", "", x)


quote_non_identifier = function(symbol){
    if (is_identifier(symbol)) {
        symbol
    } else {
        paste0("`", symbol, "`")
    }
}

# "nasa" %in% functions --> FALSE
# "%>%" %in% functions --> TRUE


for (symbol in functions) {
    # symbol = functions[1]
    # symbol = "GeomBar"
    # symbol = "geom_histogram"
    # symbol = "data_frame"
    # symbol = "count"
    # symbol = "abs"
    # symbol = "filter"
    # symbol = "glimpse"
    # symbol = "%>%"
    # symbol = "rat.diet"
    # symbol = "pairs.default"
    # symbol = "GenericSummary"
    # print(paste("processing symbol ", symbol))
    # symbol="args"

    if (symbol %in% ignoreList)next


    # from http://adv-r.had.co.nz/Environments.html#env-basics
    # The parent of the global environment is the last package that you attached with library() or require().
    obj = base::get(symbol, envir = parent.env(globalenv()))
    # if (class(obj) != "function") {
    #     next
    # }


    ## start writing the entry to the skeleton
    sink(skeletonFile, append = T)

    cat(quote_non_identifier(symbol))
    cat(" <- ")

    ## handle rexported
    decl_ns = detect_declaring_ns(symbol)
    if (pName != decl_ns) {
        cat(paste0(decl_ns, "::", quote_non_identifier(symbol), " # re-exported from ", decl_ns, " package"))
        cat("\n\n")
        sink()
        next
    }

    # process non-function objects
    # TODO instead fo string we could/should write more typed placeholder structure here
    lines = get_text_of(obj)

    if (substring(lines[[1]], 0, 1) == "<") {
        cat("\"", trim(lines[[1]]), "\"", sep = "")
        cat("\n\n")
        sink()
        next
    }


    # errors = try(sink(skeletonFile, append=T))
    # if (!inherits(errors, "try-error")) {
    for (line in lines) {
        line = gsub("<pointer: ([A-z0-9]*)>", "pointer(\"\\1\")", line)
        # line = gsub("<S4 object ([A-z0-9]*)>", "(\"\\1\")", line)
        line = gsub("<S4 object of class .*>", "S4_object()", line)


        # sub = substring(line, 0, 10)
        # if (sub == "<bytecode:"  || sub == "<environme") break

        # fix ellipsis  (...) for which quotes are skipped when printing method body.
        # Potentially this should be rather fixed in the parser
        # Example purr::partial vs https://github.com/hadley/purrr/blob/master/R/partial.R
        # DEBUG line='    args = list(... = quote(expr = ))'
        line = gsub("... = ...", "...", line, fixed = T)
        line = gsub("(... =", "(\"...\" =", line, fixed = T)
        line = gsub(" ... =", " \"...\" =", line, fixed = T)
        line = gsub("<environment>", " \"<environment>\"", line, fixed = T)

        if (grepl("^<environment", line))break
        if (grepl("^<bytecode", line))break

        cat(line, append = TRUE)
        cat("\n", append = TRUE)
    }

    cat("\n")
    cat("\n")
    # }

    sink()
}


##
## Also export datasets from package into skelekton
##

# http://stackoverflow.com/questions/27709936/how-to-get-the-list-of-data-sets-in-a-particular-package

# dsets = as.data.frame(data(package = "ggplot2")$result)
# dsets = as.data.frame(data(package = "VennDiagram")$result)
if(!(pName %in% c("base", "stats", "backports"))){

    dsets = as.data.frame(data(package = pName)$result)

    ## this fails for packages like 'fields' that export data as symbol and data
    # stopifnot(length(intersect(dsets$Item, functions)) == 0)
    ## .. thus we rather just remove such duplicates here
    dsets = subset(dsets, ! (Item %in% functions))


    # remove columns with round brackets
dsets  = subset(dsets, !(0:nrow(dsets) %in% grep("(", as.character(dsets$Item), fixed=TRUE))[-1])


sink(skeletonFile, append=T)

cat("\n\n## Package Data\n\n")

if(nrow(dsets)>0){
    cat(with(dsets, paste0(as.character(Item), " <- ", as.character(pName), "::", as.character(Item), "\t\t## ", as.character(Title))), sep="\n\n")
}else{
    cat("# none")
}

sink()
}

## examples: /Users/brandl/Library/Caches/IntelliJIdea2016.1/plugins-sandbox/system/r_skeletons/1842261700/pryr.r:545
## /Users/brandl/Library/Caches/IntelliJIdea2016.1/plugins-sandbox/system/r_skeletons/1842261700/GSEABase.r:18

## write a completion tag into each file to also have a skeleton file for packages without symbols
sink(skeletonFile, append=T)


##
## Report package info (to finally replace RPackageService)
##

cat("\n\n\n## Package Info\n\n")

## report the title
packageTitle = gsub("[\r\n]" , "", unlist(packageDescription(pName)["Title"]))
## escape double quotes
packageTitle = gsub('"', "'", packageTitle)
cat(paste0(".skeleton_package_title = \"", packageTitle, "\"\n\n"))


## report the version
## we do NOT use packageVersion here beause it slightly reformats the version string.
## E.g BH 1.62.0-1 becomes 1.62.0.1
cat(paste0(".skeleton_package_version = \"", packageDescription(pName)$Version, "\"\n\n"))


## report depends and import

# todo fixme this requires network access which should be avoided
# todo some package don't correctly report it, potentialy we should do before
#      and after loading test (see com/r4intellij/packages/RPackageService.java:386)

library(tools);
chooseCRANmirror(ind = 1)

## note: it may be more elegant to use something along `devtools::session_info` or the underlying `find_deps`. However this seems to fall back as well to the network based `available.packages`
## See https://github.com/hadley/devtools/blob/1ce84b04568ff7846c3da754f28e7e22a23c8737/R/deps.R#L326

# db=NULL is required for compatibility with R 3.2.X
pckgDepends = unlist(lapply(package_dependencies(pName, db = available.packages(), which = "Depends"), function(x)paste(x, collapse = ",")))
pckgImports = unlist(lapply(package_dependencies(pName, db = available.packages(), which = "Imports"), function(x)paste(x, collapse = ",")))

cat(paste0(".skeleton_package_depends = \"", pckgDepends, "\"\n\n"))
cat(paste0(".skeleton_package_imports = \"", pckgImports, "\"\n\n"))


## report version of the skeletonization here to allow for reskeletonization of outdated skeletons
## in a future version of r4intellij


cat("\n## Internal\n\n")

## import: just change in sync with com.r4intellij.packages.RSkeletonGenerator.SKELETONIZE_VERSION
SKELETONIZE_VERSION = 5
cat(paste0(".skeleton_version = ", SKELETONIZE_VERSION, "\n\n"))


## indicate the end of the skeleton with an EOF flag so that we can check if skeletons were created correctly
cat("\n## EOF")

sink()
