args <- commandArgs(TRUE)
packageName <- args[1]
library(package=packageName, character.only=TRUE)
loadNamespace(package=packageName)
help(args[2])