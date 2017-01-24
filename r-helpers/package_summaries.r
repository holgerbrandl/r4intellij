library(tools);
library(tidyverse);
library(stringr);
library(magrittr);

chooseCRANmirror(ind = 1)


## see http://stackoverflow.com/questions/9324869/how-do-i-determine-the-author-of-an-r-package
get_title = function(myPackage){
    unlist(packageDescription(myPackage)["Title"]) %>%   str_replace_all("[\r\n]" , "")
}

## example DESCRIPTION: https://github.com/hadley/dplyr/blob/master/DESCRIPTION

pckgList = installed.packages()[,c("Package", "Version", "LibPath")] %>% as.data.frame

pckgDepends = package_dependencies(pckgList$Package, which="Depends") %>%
    lapply(function(x)paste(x, collapse=",")) %$%
    data_frame(Package=names(.), depends=unlist(.))

pckgImports = package_dependencies(pckgList$Package, which="Imports") %>%
    lapply(function(x)paste(x, collapse=",")) %$%
    data_frame(Package=names(.), imports=unlist(.))

pckgList %<>% left_join(pckgDepends)
pckgList %<>% left_join(pckgImports)

# See  http://stackoverflow.com/questions/8637993/better-explanation-of-when-to-use-imports-depends

## also add short package description (aka title)
pckgList %<>% rowwise() %>% mutate(title=get_title(Package))

# if(F){
# options(width=200)
# filter(pckgList, Package=="tidyr")
# filter(pckgList, Package=="ggplot2") %>% knitr::kable()
# filter(pckgDepends, Package=="ggplot2") %>% knitr::kable()
# filter(pckgImports, Package=="ggplot2") %>% knitr::kable()
# }

## dump in a format suitable for parsing
with(pckgList, paste(Package, Version, title, depends, imports, "\n", sep="\t")) %>% cat

## todo also add doc-string here
# pckgDocu <-library(help = "dplyr"); pckgDocu$info[[2]]
