From https://www.practicereproducibleresearch.org/core-chapters/7-glossary.html

There are several types of testing to be considered:

* Unit testing: This type of testing focuses on the operations of individual parts of the software ("units"). One rule of thumb is that unit testing should not require disk input/output, or access to the network. Unit testing works best when coupled with modular software design. In scientific software, unit testing takes the form of verification of known results from a specific function.

* Integration testing: This type of testing focuses on testing the combination of different parts of a system. For example, verifying that the outputs of one part of the system can be ingested as inputs by other parts of the system to produce reasonable results.

* Regression testing: This type of testing focuses on testing that previous results of a computation are maintained over time. This is useful to assess parts of the software for which it is hard to write unit tests. For example, parts of the software that contain random number generation can be tested to not deviate from a prior stored result by more than a certain factor.

* End-to-end testing: This type of testing verifies if the operations of an entire system, under realistic conditions, produce desired results. For example, an analysis pipeline that starts with raw experimental data (considered representative of the actual data that the system is designed to analyze) transforms and munges this data, and results in some statistical analysis. Testing an entire workflows is considered end-to-end testing (see also continuous integration, below).

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

autocompletion unit-tests?

Nice docs
http://www.jetbrains.org/intellij/sdk/docs/basics/testing_plugins.html

resolveDependencies

* namespace preferences
* duplication


### Refactorings

Inlining: See org.intellij.grammar.BnfInlineRuleTest


* renaming of function parameter: is the renaming also applied at the callsite? Yes


TODO
----

Write unit tests for all this if possilbe :-)


