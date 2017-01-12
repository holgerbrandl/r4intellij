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



## Intentions

incomplete arument list (via RTypeCheckerInspection)


# TheR

https://github.com/ktisha/TheRPlugin


cool
* dot support for name completion
* skeleton libraries allow to browse local packages

## Skeletons

skeltons are saved under  (see module deps --> libaries)
/Users/brandl/Library/Caches/IntelliJIdea2016.1/plugins-sandbox/system/r_skeletons/-1481726564

## potential improvements and differe ce

general
* auto-detect R and do not force user to specify installation location
* function help should be context aware
* no live-templates
* structure view
* run does not work for macos

intentions
* auto-import missing packages (use com.jetbrains.ther.packages.TheRPackagesUtil#getInstalledPackages)
* auto-install missing packages in require


## later features

* implement new code fence feature


Icons see http://www.jetbrains.org/intellij/sdk/docs/reference_guide/work_with_icons_and_images.html

custom zip deployment
https://intellij-support.jetbrains.com/hc/en-us/community/posts/206769505-Plugin-with-dependency-on-additional-module-how-to-get-two-jars-in-zip-

mine has file icons, theirs not


## todo

shortcut to evaluate current expression and proceed


impl fenceprovider
https://github.com/JetBrains/intellij-plugins/pull/464#event-918221586
