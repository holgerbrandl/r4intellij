# install.packages("parser")
library(parser)


setwd("/Users/brandl/projects/rplugin/r4intellij")
require( parser )
p <- parser( "./misc/parser-example.R" )
p


pp <- parser("/Users/brandl/projects/rplugin/parser-package/inst/example/f.R")
pp