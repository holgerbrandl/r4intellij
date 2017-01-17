library(tools);
library(tidyverse);
library(stringr);

chooseCRANmirror(ind = 1)


## see http://stackoverflow.com/questions/9324869/how-do-i-determine-the-author-of-an-r-package
get_title = function(packageName){
    unlist(packageDescription(packageName)["Title"]) %>%   str_replace_all("[\r\n]" , "")
}

get_title = function(packageName){
    unlist(packageDescription(packageName)["Title"]) %>%   str_replace_all("[\r\n]" , "")
}

## example DESCRIPTION: https://github.com/hadley/dplyr/blob/master/DESCRIPTION

pckgList = installed.packages()[,c("Package", "Version", "LibPath")] %>%
    as.data.frame %>%
    # add title and dependencies
    mutate(dependencies=package_dependencies(Package)) %>%
    rowwise() %>%
    mutate(dependencies=paste(dependencies, collapse=",")) %>%
    mutate(title=get_title(Package)) %>%
    mutate(methods=getNamespaceExports(c("dplyr")) %>% paste(collapse=","))

with(pckgList, paste(Package, Version, title, dependencies, methods, "\n", sep="\t")) %>% cat

## todo also add doc-string here
# pckgDocu <-library(help = "dplyr"); pckgDocu$info[[2]]