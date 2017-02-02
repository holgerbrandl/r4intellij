R4Intellij Development Notes
=============================

## 0-day bugs


usually after startup
```
method not implemented
java.lang.Throwable
	at com.intellij.openapi.diagnostic.Logger.error(Logger.java:132)
	at com.intellij.psi.impl.file.PsiPackageBase.getChildren(PsiPackageBase.java:157)
	at com.r4intellij.documentation.RDocumentationProvider.generateDoc(Unknown Source)
	at com.intellij.lang.documentation.CompositeDocumentationProvider.generateDoc(CompositeDocumentationProvider.java:144)
	at com.intellij.codeInsight.navigation.CtrlMouseHandler.a(CtrlMouseHandler.java:661)
	at com.intellij.openapi.application.impl.ApplicationImpl.runReadAction(ApplicationImpl.java:884)
	at com.intellij.codeInsight.navigation.CtrlMouseHandler.a(CtrlMouseHandler.java:658)
	at com.intellij.util.concurrency.QueueProcessor.runSafely(QueueProcessor.java:223)
	at com.intellij.util.Alarm$Request$1.run(Alarm.java:387)
	at com.intellij.util.Alarm$Request.run(Alarm.java:398)
	at java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)
	at java.util.concurrent.FutureTask.run(FutureTask.java:266)
	at com.intellij.util.concurrency.SchedulingWrapper$MyScheduledFutureTask.run(SchedulingWrapper.java:237)
	at com.intellij.util.concurrency.BoundedTaskExecutor$2.run(BoundedTaskExecutor.java:210)
	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
	at java.lang.Thread.run(Thread.java:745)
```

* fix all unit tests
    * UnusedParameterInspectionTest
Type Tests:
    * ArgumentsMatching
    * TripleDot

* remove deprecated api usage


## potential improvements and differences

* add "new r script" and "add new R-notebook" context menu entries (see /Users/brandl/projects/rplugin/BashSupport/src/com/ansorgit/plugins/bash/actions/NewBashFileAction.java)


### General 

* auto-detect R and do not force user to specify installation location
* function help should be context aware


intentions & inspections
------------------------

* missing arg inspection does not recognize dplyr piping --> Ignore first arg if right-hand-size of pipe 

* quick fix to simplify/ remove the dot in  `filtGraphData %>% graph.data.frame(., directed=TRUE)` if first arg

* check if urls and strings that are arguments of readr methods exist (allow to add working dir annotation to configure this locally)
    * http://stackoverflow.com/questions/18134718/java-quickest-way-to-check-if-url-exists
```r
# similar to type annotation
# @type recursive : logical

# Examples: 
# @working-dir ../../
# @working-dir ~/Users/
# @working-dir ${FOO}/bar

```
   
* intention to replace tidyverse imports with library(tidyverse)
* intention

* warn if data-frame arguments are not declared in script (with comment annotation option to flag presence). Since this is not possible on a general we could/should provide it for certain APIs like tidyverse --> what about existing missinfref-inspection


* highlight packages with naming conflicts (or indicate it visually in the IDE using virtual comment)

* "add braces for if/else statement" intention
* inspection in case of naming conflicts (same functions in imported packages) suggest to add prefix to method call (allow to override by annotation)
* offer quickfix to surround if expression with curly brackets

* Intention to add roxygen docu + code basic tag completion for roxygen comments
* Intention to change function to S4 function

Documentation provider
----------------------

* add title to package view like in RS:

![](.todo_images/packages_rstudio.png)

* also add links to package names to go to cran/bioconductor homepages
* Show parameter info
 
Formatter
---------

see `CodeStyleManager.adjustLineIndent()`

* break lines and indent properly in long ggplot commands like
```
    ggplot(aes(fct_revfreq(mpi_lab), fill=labtype)) + geom_bar() + coord_flip() + ggtitle("pd lab publications") + facet_wrap(~pub_time_category, ncol=3)
```

* ensure proper check indentation when using as pipe sink
```r

pdStories %>%
    filter(!is.na(pub_time_category)) %>%
    group_by(labtype, pub_time_category) %>% summarize(
        total_pubs=n(),
        num_postdocs=unlen(full_name),
        pubs_pdcount_norm=total_pubs/num_postdocs
    ) %>%
    ggplot(aes(labtype, pubs_pdcount_norm)) + geom_bar(stat="identity") + coord_flip() + ggtitle("pd lab publications normalized by total postdocs in categories") + facet_wrap(~pub_time_category, ncol=3)

```

* post fix template support https://www.jetbrains.com/help/idea/2016.3/using-postfix-templates.html

Completion Provider
-------------------


* method names after <package>:: 
* after function name completion, cursor should end up between brackets

* show library import suggestions also for infix operators (like %<>% --> magrittr)


* Make use of CompletionType enum to finetune/speed up auto-completion

* provide completion for named function parameters
* help for function parameter should open function help

* general method name completion with autoimpart
```
com<complete> # show all methods which start with com including their packacke prefix --> autoimport if not done is completion is accepted 
```
    * preference schemes for certain packages (dplyr, ggplot, etc)
* better File path completion for nested directories


Refactorings
------------

* **FIXME**: renaming for loop variables is broken
```r
for (name in packageNames) {
    if (paste(name, "r", sep=".") %in% list.files(path=args[1])) {
        next
    }
}
```

* make sure all rstudio refactorings work as well

![](.todo_images/refactorings.png)

* resolver should allow to avoid renaming of locally overloaded libray methods. So it should detect local function redefinitions
```r
require(dplyr)

require = function(a) a+1

require(dplyr) ## rename this to foo --> should not touch first import statment
```

* change signature refac
```
    <extensionPoint name="refactoring.changeSignatureUsageProcessor"
                    interface="com.intellij.refactoring.changeSignature.ChangeSignatureUsageProcessor"/>
```
* along with signature changes: remove unused parameter from method signature


Rnotebook support
-----------------

* implement new fenceprovider for enhanced RMd snippet injection https://github.com/JetBrains/intellij-plugins/pull/464#event-918221586

Direct md embedding like in Rstudio

![](.todo_images/embedded_images.png)

are not possible in the intellij editor, see 
https://intellij-support.jetbrains.com/hc/en-us/community/posts/206756045-Displaying-an-image-in-source-code-editor


http://stackoverflow.com/questions/29718926/saving-the-state-of-a-webview-and-reloading-the-position

http://rmarkdown.rstudio.com/r_notebook_format.html

http://rmarkdown.rstudio.com/r_notebooks.html#output_storage

![](.todo_images/chunk_storage.png)

The document’s chunk outputs are also stored in an internal RStudio folder beneath the project’s .Rproj.user folder. If you work with a notebook but don’t have a project open, the outputs are stored in the RStudio state folder in your home directory (the location of this folder varies between the desktop and the server).

* chunks. (should package imports be extrapolated to the complete file to work accross chungs?


https://slides.yihui.name/2017-rstudio-conf-ext-rmd-Yihui-Xie.html#5

https://slides.yihui.name/2017-rstudio-conf-rmarkdown-Yihui-Xie.html#1

http://ijlyttle.github.io/bsplus/

## Environment view 

![](.todo_images/environment_view.png)

* open table in idea support (differnt modes: internal, DT)
* integrate with table editor in intellij
* Allow to open table by clicking (R Console session required)

See  https://www.jetbrains.com/help/idea/2016.3/working-with-the-table-editor.html



Brainstorming
=============

* Connectors for xterm and Rgui on windows
* ColorSettingsPage (see Bash implementation)

* use gradle build similar to markdown plugin
*  also support rstudio like sectioning 
* What about packrat? http://rstudio.github.io/packrat/walkthrough.html


Send To Console Improvements
============================

* mandatory dependency on Send2Console / or add suggestion balloon

Send to Console improvement:
    * send to console: jump to next line after eval (option?) 
    * eval current top-level expression (option?)
    * also add options to send line to current run console instead 
    * later: potentially add separate impl for R4intellij for more smooth integration
    
* shortcut to evaluate current expression and proceed


* R Session has almost complete implementation for console, objects, etc

Windows Support
* I think FindWindow and SendMessage are the functions you want to use, in general.
* Tinn-R: It also pops up additional menu and toolbar when it detects Rgui running on the same computer. These addons interact with the R console and allow to submit code in part or in whole and to control R directly. 
    * It seems to have some limitations
* Maybe DOM is a solution: rdom, RDCOMClient
* Or most promising, we could try to use the windows API via VBScript or C#


Markdown impro wishlist
=======================

* Useful structure view **[done]**
* Click to to jump to code
* synced scrolling

* aligned cursor with preview
* highlight search results in preview
* search in preview
* intention to unify header styles

Also see [OpenApi notes](openapi_notes.md)
