r4intellij
=====

This project provides an integration of R, which is a language for statistical computing and graphics, into Intellij&nbsp;IDEA. It aims to bring the best language for data mining and modeling in touch with the best IDE ever. 

By implementing a parser for R, the plugin allows for syntax highlighting, intelligent code folding and completion,&nbsp;refactorings, and more. 


Installation
----

1.  Get Intellij IDEA, which is available as pro and and as free open-source community edition from&nbsp;[JetBrains](http://jetbrains.com).
2.  To install the plugin, just open the plugin manager in Intellij Idea, search for "R4Intellij" , and finally click&nbsp;install.

Support
----


Feel welcome to ask questions in the plugins [discussion&nbsp;forum](http://devnet.jetbrains.net/community/idea/plugins) of Intellij. Don't forget to tag your posts with the tag r4intellij (otherwise we might not read it). 

Before you asked questions, you should consider to read our [FAQ](/docs/faq.md). 

Basic Usage
----

Just drag an R-file into idea and start hacking.

You can also create a new project (the type does not matter) and organize your work into modules with different&nbsp;content roots. Read the Intellij [documentation](http://www.jetbrains.com/idea/webhelp/intellij-idea.html) for more&nbsp;details about to do this. 

### File extensions


By default R4Intellij supports these file extensions: .R IntelliJ offers the possibility to link so far unknown file extensions to a plugin. Just choose R4Intellij to open files&nbsp;with this newly registered file extension as R files. 

### Source code navigation

The following features are supported 

*   Go To --&gt; Declaration (of symbols and locally defined functions)*   View --&gt; Quick Definition Lookup (of functions): This will show the signature and a short description

![](/docs/r_help_integration.png) 

*   Search --&gt; Highlight Usage: This will make Intellij to highlight all locations where a function or a symbol is&nbsp;being used*   Brace matching: If you position the caret before or after a bracket then IntelliJ will highlight the other element of&nbsp;the pair of brackets.*   File Structure View: In the structure view IntelliJ shows the functions and code sections of the R script.

Most of these functions use the default shortcuts of the Intellij platform. Custom shortcuts can be inferred from the&nbsp;menu entries. 

Integration with R session
----

R4Intellij supports the evaluation of the current selection or line on Windows (RGui) and MacOS (Terminal, R, [R64](/p/r4intellij/source/detail?r=64)). Furthermore, the user can define up to 4 custom code action which allow to wrap the current selection into some function before sending it to R. For instance, a custom **head of a data.frame** action would be defined as _head(%snippet%);_. These custom code actions can be defined in the preferences and can get keyboard shortcuts assigned to them. ![](/docs/code_snippet_evaluation.png) ![](/docs/code_snippet_evaluation_result.png) ![](/docs/code_snippet_action_config.png) 

Code completion
-----

R4Intellij builds an index of your local R installation which allows for much better completion, local error highlighting,&nbsp;integrated documentation lookup and more. 

To build the index of all packages installed in your R instance, R4Intellij needs . On most platforms it will guess it&nbsp;correctly, but you can adjust it if necessary in the preferences. 

The following completion modes are available: 

*   Basic word completion for variables and local functions*   Coming soon: Package name completion in library statements*   Coming soon: setwd() aware path completion*   Coming soon: completion of package names in library statements

Refactoring
----

All refactorings can be accessed via the "Refactor" menu

*   Renaming of symbols and functions

Intentions
----

Intentions are on-the-fly checks of your R scripts, that highlight problems and (optionally) suggest automatic&nbsp;quick fixes 

*   Auto-import of packages given a function name

![](/docs/before_autoimport.png?raw=true) ![](/docs/after_autoimport.png)

Planned &amp; Coming soon
----

*   Source code formatting*   Code transformation fixes (like: Convert qplot to ggplot call)*   More refactorings (Extract function; Extract variable)*   More context-aware autocompletion for variables, functions and file paths*   Inspections and quickfixes for common problems

Feel welcome to suggest new features by adding a ticket to our tracker! 

Platform features
----

The main strength of our plugin is the underlying Intellij IDE. So if you have not worked with it before, read more&nbsp;about all its amazing [features](http://www.jetbrains.com/idea/index.html). It ships with everything from SCM&nbsp;integration (Git, Subversion, etc.), neat SQL integration, bash scripting support, and so many more interesting&nbsp;features. However, it's main "feature" is its usability. Give it a try! 

You're still not convinced? Try something else!
----

Give a try to [StatET](http://www.walware.de/goto/statet), [textmate](http://macromates.com/) (with the&nbsp;[rbundle](http://worldofrcraft.blogspot.com/2008/11/setting-up-textmate-to-use-r.html)), or [http://rstudio.org/&nbsp;RStudio](http://rstudio.org/ RStudio). 

Feel welcome to report problems or suggest new features by adding an issue to our&nbsp;[tracker](https://code.google.com/p/r4intellij/issues/list). 

# Other projects of the author[](#Other_projects_of_the_author)

*   [Movito](http://www.movito.info/) - An awesome movie collection manager for files, BlueRays and DVDs. It supports movies and TV-series and provides a slick interface to explore and filter your movie collection
*   [OpenCards](http://opencards.info/) - A free flashcard learning software for ppt-files
 </td>