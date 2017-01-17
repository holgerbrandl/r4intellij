args <- commandArgs(TRUE)
packageNames <- .packages(all = TRUE)
searchPath <- search()

is.identifier <- function(str) {
 return(grepl("^([[:alpha:]]|_|\\.)([[:alpha:]]|[[:digit:]]|_|\\.)*$", str) == TRUE)
}


for (name in packageNames) {
#name = "clusterProfiler"
#name = "modeest"
    if (paste(name, "r", sep=".") %in% list.files(path=args[1])) {
        next
    }
    if(dir.exists(file.path(args[1], name))){
        print(paste("skipping package ", name))
        next
    }

    shouldLoadLibrary = FALSE
    pName = paste("package", name, sep=":")
    if (!pName %in% searchPath)
        shouldLoadLibrary = TRUE
    if (shouldLoadLibrary) {
        library(package=name, character.only=TRUE)
    }

    functions <- as.character(lsf.str(paste("package", name, sep=":")))

    dirName = paste(args[1], name, sep="/")
    dir.create(dirName)

    for (symbol in functions) {
        print(paste("processing symbol ", symbol))
    	obj <- get(symbol)
    	if (class(obj) != "function") {
    	    next
    	}
    	name_without_extension <- ifelse(grepl("/", symbol), gsub("/", "slash", symbol), symbol)
        fileName <- paste(paste(dirName, name_without_extension, sep="/"), "r", sep=".")
        tmpFileName <- tempfile(pattern = "tmp", tmpdir = tempdir(), fileext = "")
        sink(tmpFileName)
        if (is.identifier(symbol))
          cat(symbol)
        else {
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

    if (shouldLoadLibrary) {
        detach(pName, character.only=TRUE)
        diff <- setdiff(search(), searchPath)
        for (p in diff) {
            detach(p, character.only=TRUE)
        }
    }
}