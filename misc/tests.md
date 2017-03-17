
General
--------

http://stackoverflow.com/questions/1861875/upgrading-to-junit4-and-keeping-legacy-junit-3-tests-and-test-suites-by-running

http://stackoverflow.com/questions/264680/best-way-to-automagically-migrate-tests-from-junit-3-to-junit-4

http://stackoverflow.com/questions/2794407/how-to-configure-intellij-for-running-test-with-junit-4
> It seems that IDEA defaults to the use of JUnit3 as a test runner for any classes that have junit.framework.TestCase
> Force with @RunWith(JUnit4.class)



From https://www.practicereproducibleresearch.org/core-chapters/7-glossary.html

There are several types of testing to be considered:

* Unit testing: This type of testing focuses on the operations of individual parts of the software ("units"). One rule of thumb is that unit testing should not require disk input/output, or access to the network. Unit testing works best when coupled with modular software design. In scientific software, unit testing takes the form of verification of known results from a specific function.

* Integration testing: This type of testing focuses on testing the combination of different parts of a system. For example, verifying that the outputs of one part of the system can be ingested as inputs by other parts of the system to produce reasonable results.

* Regression testing: This type of testing focuses on testing that previous results of a computation are maintained over time. This is useful to assess parts of the software for which it is hard to write unit tests. For example, parts of the software that contain random number generation can be tested to not deviate from a prior stored result by more than a certain factor.

* End-to-end testing: This type of testing verifies if the operations of an entire system, under realistic conditions, produce desired results. For example, an analysis pipeline that starts with raw experimental data (considered representative of the actual data that the system is designed to analyze) transforms and munges this data, and results in some statistical analysis. Testing an entire workflows is considered end-to-end testing (see also continuous integration, below).

Index issues when running unit tests
------------------------------------

remove the index of the idea plugin-dev environment
```bash
cd ~/Library/Caches/IntelliJIdea2016.3/plugins-sandbox/test/system/
rm -rf index
```


Todo
----


Unify to use common base class for testing, currently we use 3
* UsefulTestCase
* CodeInsightFixtureTestCase
* LightPlatformCodeInsightFixtureTestCase

* choose between light and heavy, see http://www.jetbrains.org/intellij/sdk/docs/basics/testing_plugins/light_and_heavy_tests.html

See http://www.jetbrains.org/intellij/sdk/docs/basics/testing_plugins/tests_and_fixtures.html


Missing Unit Tests
------------------

autocompletion unit-tests? Just use CITF as usual with `com.intellij.testFramework.fixtures.CodeInsightTestFixture.testCompletion(java.lang.String, java.lang.String, java.lang.String...)` and friends

Nice docs
http://www.jetbrains.org/intellij/sdk/docs/basics/testing_plugins.html

resolveDependencies

* namespace preferences
* duplication

## help provider

* ns-prefix calls:
```r
dplyr::count(iris, Species)
```

### Refactorings

Inlining: See org.intellij.grammar.BnfInlineRuleTest


* renaming of function parameter: is the renaming also applied at the callsite? Yes

* User RResolver coverarge to define missing resolver tests

End2End Tests
-------------

* Documentation Provider [test cases](testData/end_to_end/doc_lookup.R)

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



TODO
----

Write unit tests for all this if possilbe :-)


Misc
====

Clear config between runs 
`rm -rf ~/Library/Caches/IntelliJIdea2016.3/plugins-sandbox/config/`

* hightlighting codes from com/intellij/openapi/options/colors/pages/GeneralColorsPage.java:60 
```
  "Code Inspections:\n" +
    "  <error>Error</error>\n" +
    "  <warning>Warning</warning>\n" +
    "  <weak_warning>Weak warning</weak_warning>\n" +
    "  <deprecated>Deprecated symbol</deprecated>\n" +
    "  <unused>Unused symbol</unused>\n"+
    "  <wrong_ref>Unknown symbol</wrong_ref>\n" +
    "  <server_error>Problem from server</server_error>\n" +
    "  <server_duplicate>Duplicate from server</server_duplicate>\n" +
```