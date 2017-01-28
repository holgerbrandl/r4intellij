Editor support
===============


Refactoring
------------

Renaming Support for
* function parameters: Renaming is applied within function defintion and call-sites (for named parameters)
* local variables
* functions


Intentions
----------


For general introduction see https://www.jetbrains.com/idea/docs/Programming_by_Intention.pdf

Code completion
------------


```r
require(tidyverse)
#require(dplyr)

mut # no completion in rstudio because imports are not resolved transitively
```
