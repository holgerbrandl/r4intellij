library(tools);
library(tidyverse);
library(stringr);

chooseCRANmirror(ind = 1)


# from http://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
unload_packages <- function() {
    basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
    package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
    package.list <- setdiff(package.list,basic.packages)
    if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}

# from http://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
unload_namespaces <- function() {
    basic.packages <- c("stats","graphics","grDevices","utils","datasets","methods","base")
    package.list <- loadedNamespaces()
    package.list <- setdiff(package.list,basic.packages)

    # for (package in package.list) detach(package, character.only=TRUE, unload=T)
    for (package in package.list){
    tryCatch( {
      unloadNamespace(asNamespace(package))
        },  error=function(cond) {
           return("")
        })
    }
}


if(FALSE){
require(tidyverse)
require(ggplot2)
require(Rcpp)
unload_packages()
unload_namespaces()
sessionInfo()
require(tidyverse)

unload_packages()
package="scales"
package="ggplot2"
package="ggplot2"

detach(package, character.only=TRUE, unload=T)
unloadNamespace(asNamespace(package))


# #unload libraries
# library.dynam()[["Filename"]] %>% lapply(function(lib))
#
# library.dynam() %>%as.data.frame
#
# library.dynam.unload("lazyeval", libpath="/Library/Frameworks/R.framework/Versions/3.3/Resources/library/Rsamtools/libs/")
# #

}


## see http://stackoverflow.com/questions/9324869/how-do-i-determine-the-author-of-an-r-package
get_title = function(myPackage){
    unlist(packageDescription(myPackage)["Title"]) %>%   str_replace_all("[\r\n]" , "")
}


get_methods = function(myPackage){
    # print(paste("processing", asNamespace(myPackage)))
    # getNamespaceExports(asNamespace(myPackage)) %>% paste(collapse=",")
    tryCatch( {
    getNamespaceExports(asNamespace(myPackage, base.OK = T)) %>% paste(collapse=",")
    },  error=function(cond) {
       return("")
    }, warning=function(cond) {
           return("")
    }, finally={
        unload_packages()
        unload_namespaces()
        unload_namespaces()
        require(tidyverse)
          # message("Some other message at the end")
    })
    # getNamespaceExports(myPackage) %>% paste(collapse=",")
    # getNamespaceExports("acepack") %>% paste(collapse=",")
}

# data_frame(Package="dplyr") %>% mutate(methods=get_methods(as.character(Package)))
