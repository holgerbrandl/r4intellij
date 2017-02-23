args <- commandArgs(TRUE)
# args = c("dplyr", "glimpse")
packageName <- args[1]

library(package=packageName, character.only=TRUE)
loadNamespace(package=packageName)

options(help_type = "text")
help(args[2])