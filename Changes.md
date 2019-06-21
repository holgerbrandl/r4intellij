Version History
===============

## Changes in v1.1

* Enabled spellchecking for comments


# Changes in 1.0.X


* Added refresh trigger to missing package inspection
* Improve function completion by adding round brackets: `base<caret>` -> `basename(<caret>)`
* More informative feedback in case of indexing issues
* Path completion
* Defer index update until user actually opens an R script


## Changes in v1.0

Restarted development by forking [TheR](https://github.com/ktisha/TheRPlugin). This adds the following additional features

* Interactive console
* Builtin help
* Package manager
* Debugger
* Run configurations
* Incomplete parameter list inspection
* Unused method parameter inspection
 

The following features were merged in from the previous version of r4intellij
* More recent R logo
* Live-templates
* Structure view to jump to sections and functions
* Code folding for function definitions, loops and if-else expressions 
* Enhanced code completion 
    * all identifiers in current document
    * package names in `require` and `library`
    * context aware method name completion for loaded packages 
* Language injection into string literals
* Basic code formatter
* Missing import inspection & quickfix
* Different color-schemes

Other improvements
* More robust and extended skeletonization
* Various parser enhancements
* Unused variable highlighting/inspection
* Improved reference resolver

New Quickfixes (aka intention) and Inspections
* Install missing library intention
* Replace T/F with TRUE/FALSE

External Code Snippet Evaluation was refactored out into separate [Send to Terminal](https://plugins.jetbrains.com/idea/plugin/9409-send-to-terminal) plugin. Code is available under https://github.com/holgerbrandl/send2terminal


## Changes in v0.12

*   New R icon
*   Fixed: parser fails to work for higher dimensional arrays #53
*   Fixed: functions declared with = don't show up in structure view #72
*   Allow for leading commas in functions invokations (e.g. `myFun(,5)`) which seem bad practice (use named arg

## Changes in v0.10

*   Improved Intellij v15 compatibility

## Changes in v0.9

*   Fixed PyCharm Support
*   (Experimental) Reformatter
*   Parser improvements

## Changes in v0.9

*   Added R color setting page
*   Named vector support
*   Support for more magrittr operators
*   Added live-templates for dplyr
*   Improved send to R/terminal

## Changes in v0.8.4

*   Added iTerm support for HongKee

## Changes in v0.8.3

*   Added support for pipe operator from magrittr package
*   Changed plugin definition to support also other Jetbrains products like PhpStorm, PyCharm, etc.
*   Improved compatibility to Intellij IDEA 13

## Changes in v0.8.2

*   Added support for more operators (like %+% or <<-

## Changes in v0.8

*   Added option to keep focus in editor after submitting code to R

## Changes in v0.7

*   Dramatically improved parser performance
*   Code evaluation bugfixes (macos)

## Changes in v0.6

*   Highlight usages of functions and variables in file
*   Advanced navigation (structure view, go to declaration)
*   Code section folding
*   Code evaluation connectors for Windows (Rgui) and MacOS (R, R64 and Terminal)
*   Customizable code evaluation snippets
*   Auto-import of functions
*   Simple function help

## Changes in v0.5

*   New shortcut action to call str() for the current word or selection (MacOS only)
*   Function body folding
*   More robust lexer
*   Finished (my first) parser --> allows for growing selection using "Select for word at caret" action
*   bug fixes

## Changes in v0.4

*   Basic code completion
*   Better shortcuts for code evaluation (MacOS only)

## Changes in version 0.3

*   Evaluate selection of current line in R (MacOS only)
*   Improved code highlighting
*   Added first live-templates

## Changes in version 0.2


## Initial release of the plugin. :-)

*   Syntax highlighting
*   Comment code blocks with usual shortcut ⌘/
*   Register .R as file type