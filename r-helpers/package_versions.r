# library(tools)

versions = as.data.frame(installed.packages()[, c("Package", "Version")])

with(versions, cat(paste(Package, Version, sep = "\t"), sep = "\n"))

