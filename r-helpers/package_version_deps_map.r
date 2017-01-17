library(tools);
library(tidyverse);

chooseCRANmirror(ind = 1)



get_deps = function(packageName){
   # paste(package_dependencies(packageName)$Depends, collapse='')
   paste(unlist(package_dependencies("dplyr")), collapse=",")
}

# get_deps("dplyr")
#

installed.packages() %>% as.data.frame %>% rownames()
installed.packages() %>% as.data.frame %>% head()

installed.packages()[,c("Package", "Version", "LibPath", "Description")] %>%
    as.data.frame %>%
    rownames_to_column %>%
    rowwise() %>%
    mutate(dependencies=get_deps(Package))

# https://github.com/hadley/dplyr/blob/master/DESCRIPTION


paste(pkgDepends('" + packageName + "')$Depends, collapse='')