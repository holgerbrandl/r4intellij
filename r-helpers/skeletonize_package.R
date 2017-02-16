args <- commandArgs(TRUE)

## test invcation:
# rm -rf  ~/Desktop/skeleton/clusterProfiler/ ~/Desktop/skeleton/clusterProfiler.r;
# R -f /Users/brandl/projects/rplugin/r4intellij/r-helpers/skeletonize_package.R --args ~/Desktop/skeleton clusterProfiler

# args <- c("~/Desktop/skeleton", "base"); dir.create(args[1])


skelBaseDirectory = args[1]
pName = args[2]
#pName = "Rsamtools"
#pName = "modeest"
#pName = "config"
#pName = "dplyr"

searchPath <- search()

is.identifier <- function(str) {
 return(grepl("^([[:alpha:]]|_|\\.)([[:alpha:]]|[[:digit:]]|_|\\.)*$", str) == TRUE)
}


## skip existing skeletons --> disabled because the SKELETONIZE_VERSION may have changed
# if (paste(pName, "r", sep=".") %in% list.files(path=skelBaseDirectory)) {
#      quit("no")
# }

# if(dir.exists(file.path(skelBaseDirectory, pName))){
#     print(paste("skipping package ", pName))
#     quit("no")
# }

shouldLoadLibrary = FALSE
pckgPrefixed = paste("package", pName, sep=":")

if (!pckgPrefixed %in% searchPath){
    shouldLoadLibrary = TRUE
}

if (shouldLoadLibrary) {
    library(package=pName, character.only=TRUE)
}

functions = as.character(lsf.str(paste("package", pName, sep=":")))

dir.create(skelBaseDirectory)

skeletonFile = file.path(skelBaseDirectory, paste0(pName, ".r"))
print(paste("writing skeleton of ",pName, "into", skeletonFile))

#' some symbols are defined as functions in base.R but our parser does not like it.
ignoreList = c("for", "function", "if", "repeat", "while")

sink(skeletonFile)
cat(paste0("##\n## Exported symobls in package `",pName, "`\n##\n\n"))
cat("## Exported package methods\n\n")
sink()


for (symbol in functions) {
    if(symbol %in% ignoreList) next

    # symbol = functions[1]
    # print(paste("processing symbol ", symbol))

    obj <- base::get(symbol)
    if (class(obj) != "function") {
        next
    }

    tmpFileName <- tempfile(pattern = "tmp", tmpdir = tempdir(), fileext = "")
    sink(tmpFileName)

    if (is.identifier(symbol)){
      cat(symbol)
    } else {
      cat("`")
      cat(symbol)
      cat("`")
    }

    cat(" <- ")
    print(obj)
    sink()

    fileObj <- file(tmpFileName)
    lines <- readLines(fileObj)
    close(fileObj)

    errors <- try(sink(skeletonFile, append=T))
    if (!inherits(errors, "try-error")) {
        for (line in lines) {
            sub <- substring(line, 0, 10)
            if (sub == "<bytecode:"  || sub == "<environme") break

            # fix ellipsis  (...) for which quotes are skipped when printing method body.
            # Potentially this should be rather fixed in the parser
            # Example purr::partial vs https://github.com/hadley/purrr/blob/master/R/partial.R
            # DEBUG line='    args <- list(... = quote(expr = ))'
            line = gsub("... = ...", "...", line, fixed=T)
            line = gsub("(... =", "(\"...\" =", line, fixed=T)
            line = gsub(" ... =", " \"...\" =", line, fixed=T)

            cat(line, append=TRUE)
            cat("\n", append=TRUE)
        }

        cat("\n")
        cat("\n")
    }

    sink()
}


##
## Also export datasets from package into skelekton
##

# http://stackoverflow.com/questions/27709936/how-to-get-the-list-of-data-sets-in-a-particular-package

# dsets <- as.data.frame(data(package = "ggplot2")$result)
# dsets <- as.data.frame(data(package = "VennDiagram")$result)
if(!(pName %in% c("base", "stats", "backports"))){
dsets <- as.data.frame(data(package = pName)$result)

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
packageName = gsub("[\r\n]" , "", unlist(packageDescription(pName)["Title"]))
cat(paste0(".skeleton_package_title = \"", packageName, "\"\n\n"))


## report the version
cat(paste0(".skeleton_package_version = \"", packageVersion(pName), "\"\n\n"))


## report depends and import

# todo fixme this requires network access which should be avoided
# todo some package don't correctly report it, potentialy we should do before
#      and after loading test (see com/r4intellij/packages/RPackageService.java:386)

library(tools);
chooseCRANmirror(ind = 1)

pckgDepends = unlist(lapply(package_dependencies(pName, which="Depends"), function(x)paste(x, collapse=",")))
pckgImports = unlist(lapply(package_dependencies(pName, which="Imports"), function(x)paste(x, collapse=",")))

cat(paste0(".skeleton_package_depends = \"", pckgDepends, "\"\n\n"))
cat(paste0(".skeleton_package_imports = \"", pckgImports, "\"\n\n"))


## report version of the skeletonization here to allow for reskeletonization of outdated skeletons
## in a future version of r4intellij


cat("\n## Internal\n\n")

## import: just change in sync with com.r4intellij.interpreter.RSkeletonGenerator.SKELETONIZE_VERSION
SKELETONIZE_VERSION = 1
cat(".skeleton_version = 1\n\n")


## indicate the end of the skeleton with an EOF flag so that we can check if skeletons were created correctly
cat("\n## EOF")

sink()
