args <- commandArgs(TRUE)

## test invcation:
# rm -rf  ~/Desktop/skeleton/clusterProfiler/;
# R -f /Users/brandl/projects/rplugin/r4intellij_v2/r-helpers/skeletonize_package.R --args ~/Desktop/skeleton viridisLite

# args <- c("~/Desktop/skeleton", "clusterProfiler"); dir.create(args)


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

if(dir.exists(file.path(skelBaseDirectory, pName))){
    print(paste("skipping package ", pName))
    quit("no")
}

shouldLoadLibrary = FALSE
pckgPrefixed = paste("package", pName, sep=":")

if (!pckgPrefixed %in% searchPath){
    shouldLoadLibrary = TRUE
}

if (shouldLoadLibrary) {
    library(package=pName, character.only=TRUE)
}

functions <- as.character(lsf.str(paste("package", pName, sep=":")))

dirName = paste(args[1], pName, sep="/")
dir.create(dirName)

for (symbol in functions) {
    # symbol = functions[1]
    print(paste("processing symbol ", symbol))
    obj <- base::get(symbol)
    if (class(obj) != "function") {
        next
    }
    name_without_extension <- ifelse(grepl("/", symbol), gsub("/", "slash", symbol), symbol)
    fileName <- paste(paste(dirName, name_without_extension, sep="/"), "r", sep=".")
    tmpFileName <- tempfile(pattern = "tmp", tmpdir = tempdir(), fileext = "")
    sink(tmpFileName)
    if (is.identifier(symbol)){
      cat(symbol)
    } else {
      cat("\"")
      cat(symbol)
      cat("\"")
    }

    cat(" <- ")
    print(obj)
    sink()

    fileObj <- file(tmpFileName)
    lines <- readLines(fileObj)
    close(fileObj)

    errors <- try(sink(fileName))
    if (!inherits(errors, "try-error")) {
        for (line in lines) {
            sub <- substring(line, 0, 10)
            if (sub == "<bytecode:"  || sub == "<environme") break
            cat(line, append=TRUE)
            cat("\n", append=TRUE)
        }
        sink()
    }
}