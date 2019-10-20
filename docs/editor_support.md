Editor support
===============

General IDE usage
-----------------

* [How to use the navigation bar?](https://blog.jetbrains.com/pycharm/2017/01/life-without-the-project-explorer/)

Refactoring
------------

Renaming Support for
* function parameters: Renaming is applied within function defintion and call-sites (for named parameters)
* local variables
* functions


How does all this magic work?
-----------------------------

Most functions are enabled by running running a very efficient and powerful resolver, that can resolve variable, functions, and operators.

Intentions
----------


For general introduction see https://www.jetbrains.com/idea/docs/Programming_by_Intention.pdf

Code completion
------------


```r
require(tidyverse)
#require(dplyr)

mut # no completion in RStudio because imports are not resolved transitively
```


Useful references

* https://www.jetbrains.com/help/idea/2016.3/keyboard-shortcuts-you-cannot-miss.html
* https://www.jetbrains.com/idea/documentation/