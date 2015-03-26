#http://stackoverflow.com/questions/8696158/find-all-functions-including-private-in-a-package
ls(getNamespace("ggplot2"), all.names=TRUE)

args(ggplot2::geom_bar)

## AnnotationDbi

require(dplyr)
require(AppliedPredictiveModeling)

ls(getNamespace("AppliedPredictiveModeling"), all.names=F)


args(AnnotationDbi::loadDb) %>% d
tt <- args(ggplot2::geom_bar)

args(base::c) %>% deparse %>% paste(collapse="")

args(ggplot2::geom_bar) %>% deparse %>% paste(collapse="")
arg <-args(ggplot2::geom_bar)
args(ggplot2::geom_bar) %>% deparse# %>% print


## function signature indexing
beautify_args <- function(name) {
  # browser(); 
  paste(deparse(substitute(name)), deparse(args(name)), collapse="")
}

beautify_args <- function(name) { paste(deparse(substitute(name)), deparse(args(name)), collapse='') }


beautify_args(ggplot2::geom_bar)

tt <- c(ggplot2::geom_bar, AnnotationDbi::loadDb)
sigfuns <- c(ggplot2::geom_bar, AnnotationDbi::loadDb)
sapply(sigfuns, beautify_args)

lapply(sigfuns, beautify_args)


### auto install dplyr
## http://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them

if(!require(dplyr)) install.packages('dplyr', repos='http://cran.us.r-project.org')
if(!require(stringr)) install.packages('stringr', repos='http://cran.us.r-project.org')


### Method signature by package com.r4intellij.misc.rinstallcache.LibraryIndexFactory.buildPackageCache

#ls(getNamespace("" + packageName + ""), all.names=F)
#ls(getNamespaceInfo('ggplot2', 'exports'))
getNamespaceExports("ggplot2")


name="AnnotationDbi::createAnnObjs"
beautify_args <- function(name) { paste(parse(text=name), deparse(args(eval(parse(text=name)))), collapse="") };
beautify_args <- function("AnnotationDbi::createAnnObjs") { paste(deparse(substitute(name)), deparse(args(name)), collapse="") };

beautify_args(sigfuns)

cat(paste('unlist2', paste(deparse(args(AnnotationDbi::unlist2)), collapse=""), sep='||'), fill=1000)

paste("toggleProbes", paste(deparse(args(AnnotationDbi::toggleProbes)), collapse=""), sep='||')