args <- commandArgs(TRUE)

## test invcation:
# rm -rf  ~/Desktop/skeleton/clusterProfiler/ ~/Desktop/skeleton/clusterProfiler.r;
# R -f /Users/brandl/projects/rplugin/r4intellij_v2/r-helpers/skeletonize_package.R --args ~/Desktop/skeleton clusterProfiler

# args <- c("~/Desktop/skeleton", "base"); dir.create(args[1])


skelBaseDirectory = args[1]
pName = args[2]
#pName = "clusterProfiler"
#pName = "modeest"
#pName = "config"

searchPath <- search()

is.identifier <- function(str) {
 return(grepl("^([[:alpha:]]|_|\\.)([[:alpha:]]|[[:digit:]]|_|\\.)*$", str) == TRUE)
}


## skip existing skeletons
if (paste(pName, "r", sep=".") %in% list.files(path=skelBaseDirectory)) {
     quit("no")
}

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


ignoreList = c("for", "function", "if", "repeat", "while")

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


## examples: /Users/brandl/Library/Caches/IntelliJIdea2016.1/plugins-sandbox/system/r_skeletons/1842261700/pryr.r:545
## /Users/brandl/Library/Caches/IntelliJIdea2016.1/plugins-sandbox/system/r_skeletons/1842261700/GSEABase.r:18

## write a completion tag into each file to also have a skeleton file for packages without symbols
sink(skeletonFile, append=T)
cat("## EOF")
sink()
