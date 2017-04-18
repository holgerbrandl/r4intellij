
# Introduction

This project provides an integration of [R](http://r-project.org), which is a language for statistical computing and graphics, into [Intellij IDEA](https://www.jetbrains.com/idea/).

![](readme_images/r4ij_example.png)

The plugin provides syntax highlighting, import-aware code completion, a powerful and customizable code formatter, refactorings to reshape existing code, smart indentation, automatic detection and quickfixes for common coding issues, and sophisticated means to navigate your code and installed packages. Furthermore, it integrates tightly with R help and documentation, includes an interactive debugger, and allows to execute R code in an integrated console.

[TOC]: # "Table of Contents"

# Table of Contents
- [Installation](#installation)
- [Support](#support)
- [Basic Usage](#basic-usage)
    - [File extensions](#file-extensions)
    - [Source code navigation](#source-code-navigation)
- [Interactive Console](#interactive-console)
- [Integration with R session](#integration-with-r-session)
- [Code completion](#code-completion)
- [Refactoring](#refactoring)
- [Intentions](#intentions)
- [Platform features](#platform-features)
- [Site navigation](#site-navigation)
- [Acknowledgements](#acknowledgements)
- [Links](#links)



Installation
------------

1.  Get Intellij IDEA, which is available as pro and as free open-source community edition from [JetBrains](http://jetbrains.com).
2.  To install the plugin, just open the plugin manager in Intellij Idea, search for "R Language Support" , and finally click install.
![](.README_images/plugins_browser.png)


Support
-------

Feel welcome to report problems or suggest new features by adding an issue to our [tracker](https://code.google.com/p/r4intellij/issues/list). Or be a hero and send us a pull request.

<!-- You can also ask questions in the plugins [discussion forum](http://devnet.jetbrains.net/community/idea/plugins) of Intellij. Don't forget to tag your posts with the tag r4intellij (otherwise we might not read it). -->

Before you asked questions, you should consider to read our [FAQ](faq.md).

Basic Usage
-----------

Just drag an R script into idea and start hacking.

Create a R script from the projects tools window:
![](.README_images/new_script.png)


You can also create a new project (the type does not matter) and organize your work into modules with differen content roots. Read the Intellij [documentation](http://www.jetbrains.com/idea/webhelp/intellij-idea.html) for more details about to do so.

Implemented features are accessible by the default shortcuts of the Intellij platform where possible. Custom shortcuts can be assigned under `Preferences -> Keymap`.

### File extensions


By default the plugin supports these file extensions: `.R`.  IntelliJ offers the possibility to link so far unknown file extensions to a plugin. Just choose R4Intellij to open files with this newly registered file extension as R files.

### Source code navigation

The following features are supported 

*   Go To --&gt; Declaration (of symbols and locally defined functions)*   View --&gt; Quick Definition Lookup (of functions): This will show the signature and a short title


*   Search --&gt; Highlight Usage: This will make Intellij to highlight all locations where a function or a symbol is being used*   Brace matching: If you position the caret before or after a bracket then IntelliJ will highlight the other element of the pair of brackets.*   File Structure View: In the structure view IntelliJ shows the functions and code sections of the R script.

* Integration with R-Help: Simply invoke `F1` on any symbol (data-sets, package names, functions).
![](.README_images/r_help_integration.png)
* Documentation lookup for user-defined functions
![](.README_images/user_def_fun_doc_lookup.png)


Interactive Console
-------------------

Start a new console under _Tools_

![](.README_images/new_repl.png)


To push code from the editor use `Meta
For a more general overview see https://www.jetbrains.com/help/idea/2016.3/working-with-embedded-local-terminal.html

Integration with R session
--------------------------

R4Intellij supports the evaluation of the current selection or line on Windows (RGui) and MacOS (R GUI, Terminal, [iTerm2](http://www.iterm2.com/)). Furthermore, the user can define up to 4 custom code action which allow to wrap the current selection into some function before sending it to R. For instance, a custom **head of a data.frame** action would be defined as _head(%snippet%);_. These custom code actions can be defined in the preferences and can get keyboard shortcuts assigned to them. 

![](readme_images/code_snippet_evaluation.png) 

![](readme_images/code_snippet_evaluation_result.png) 


On Mac, by default Ctrl+Option+Enter sends the current line or selection to the R Gui app. If RGui is not already started it will be started on the Mac dock and you may have to click it to make it visible. (If you did not know this you may think that nothing happened.) To send to the Terminal instead of RGui, go to IntelliJ->Preferences->Other settings->R4Intellij and change the "Snippet Evaluation Target" to Terminal. This is the Mac Terminal not the Terminal embedded in Intellij but that may well be what you wanted or hoped for anyway. The Evaluate menu option is accessible via the right-click floating context menu, not from the main IntelliJ application menu. To change the shortcut key for Evaluate, go to Preferences->Keymap->Plug-ins->R4Intellij->Evaluate then right click and "Add keyboard shortcut"; e.g. F5. You can have many Terminal sessions running and pressing F5 in Intellij will now send that line or selection to the last active Terminal (similar to NppToR on Windows). That snippet may well be R code or it could be bash commands or Python (or anything with a prompt, perhaps via ssh) that you have running in that Terminal. There is no limit to the number of Terminal sessions (unlike the limit of 4 in r-gedit). Very useful!

Code completion
---------------

R4Intellij builds an index of your local R installation which allows for much better completion, local error highlighting, integrated documentation lookup and more. 

To build the index of all packages installed in your R instance, R4Intellij needs . On most platforms it will guess it correctly, but you can adjust it if necessary in the preferences. 

The following completion modes are available: 

*   Basic word completion for variables and local functions*   Coming soon: Package name completion in library statements*   Coming soon: setwd() aware path completion*   Coming soon: completion of package names in library statements

Refactoring
-----------

All refactorings can be accessed via the "Refactor" menu

*   Renaming of symbols and functions

Intentions
----------

Intentions are on-the-fly checks of your R scripts, that highlight problems and (optionally) suggest automatic quick fixes 

* Auto-import of packages given a function name
![](readme_images/after_autoimport.png)

* Convert `T` to `TRUE` and `F` to `FALSE`


Platform features
-----------------

The main strength of our plugin is the underlying Intellij IDE. So if you have not worked with it before, read more about all its amazing [features](http://www.jetbrains.com/idea/index.html). It ships with everything from SCM integration (Git, Subversion, etc.), neat SQL integration, bash scripting support, and so many more interesting features. However, its main "feature" is its usability. Give it a try!


Site navigation
---------------

* [Development notes](https://github.com/holgerbrandl/r4intellij/blob/master/misc/devel_notes.md)
* [Release History](https://github.com/holgerbrandl/r4intellij/blob/master/Changes.md)

* [FAQ](faq.html)


Acknowledgements
---------------

This project was rebuilt by merging it's codebase with a fork of [The R plugin](https://github.com/ktisha/TheRPlugin). So half the credits go to go [ktisha](https://github.com/ktisha).
  
This project would not have been possible without great [bash plugin](https://plugins.jetbrains.com/plugin/4230?pr=phpStorm) which inspired us a lot when getting started with r4intellij, and from which we borrowed also some code.


Links
-----

Other R-IDEs
* [StatET](http://www.walware.de/goto/statet), [textmate](http://macromates.com/) (with the [rbundle](http://worldofrcraft.blogspot.com/2008/11/setting-up-textmate-to-use-r.html)), or [RStudio](http://rstudio.org/`).
