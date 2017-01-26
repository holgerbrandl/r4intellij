
Documentation Provider
----------------------


* for library method
* for local function in the same file --> show doc-string
* for local function in the other file --> show doc-string
* for naming conflicted method --> show correct doc-string
```r
require(dplyr)
require(plyr)

rename(iris)
```

* for roxygen styled docs --> show correctly formatted docs
```r

#' Sum of vector elements.
#' 
#' \code{sum} returns the sum of all the values present in its arguments.
#' 
#' This is a generic function: methods can be defined for it directly or via the
#' \code{\link{Summary}} group generic. For this to work properly, the arguments
#' \code{...} should be unnamed, and dispatch is on the first argument.
sum <- function(..., na.rm = TRUE) {}

sum(2,3)

```

* for naming conflicted method --> show correct doc-string



Missing Unit Tests
------------------

resolveDependencies

* namespace preferences
* duplication


* renaming of function parameter: is the renaming also applied at the callsite? Yes


TODO
----

Write unit tests for all this if possilbe :-)


