## External jars

This creates a .jar or .zip archive file to be used to install and publish your plugin. If the plugin module does not depend on libraries, the .jar archive will be created. Otherwise, a .zip archive will be created that will include all the plugin libraries specified in the project settings. [src](https://confluence.jetbrains.com/display/IDEADEV/Getting+Started+with+Plugin+Development)


## Useful links
https://confluence.jetbrains.com/display/IDEADEV/Developing+Custom+Language+Plugins+for+IntelliJ+IDEA


API

## Release Action List

1. Increase version
2. Snapshot on github
3. Run tests
4. Deploy jar and upload to plugin reposiotry



## Competitors

## TinnR

* Allows for Rgui interaction to evaluate line or selection
    * including list variables or objects, clearing console, even stopping the current process.
* Code formatter
* Bracket matching & checking
* Commenting uncommenting



## Parser

parser package for R
* Uses almost identical version of R grammar
* According to parser docu: created using bison
* Source file creates c-parser

# Roadmap

* Basic refactorings to match StatET (rename variable, introduce local variable, inline local variable,  extract function, Generate Element Comment)


* File path completion (learn from bash plugin)
* Already possible by injecting bash into literal
* Better highlighting of syntax errors
* Intention to add roxygen docu + code basic tag completion for roxygen comments
* Intention to change function to S4 function
* Connectors for xterm and Rgui on windows


* Check that function is available and provide import library statement if necessary
* More context-aware auto-completion for variables, functions and file paths
* Push to R also for windows
* Example? Arc:ReplToolWindow
* ColorSettingsPage (see Bash implementation)
* Show parameter info
* BnfAnnotator: psi-aware highlightling of syntax elements


# Brainstorming

## R Console

* merge from R language support
* also see TinnR


## Options for code snippet evaluation

* R Session has almost complete implementation for console, objects, etc
* TextMate bundle
    * Start R in special mode that reads all input from file and writes all output to another one which then somehow imported into textmate
* I think FindWindow and SendMessage are the functions you want to use, in general.
* Use the clipboard
* Tinn-R: It also pops up additional menu and toolbar when it detects Rgui running on the same computer. These addons interact with the R console and allow to submit code in part or in whole and to control R directly. 
    * It seems to have some limitations
* Maybe DOM is a solution: rdom, RDCOMClient
* Or white 
* Or most promising, we could try to use the windows API via VBScript or C#
