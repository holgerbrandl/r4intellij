R4Intellij Development Notes
----------------------------

## 0-day bugs

* helper scripts are not runnable from jar and don't seem to be extracted

* libraries are attached as duplicates
```
     <orderEntry type="library" name="R Skeletons" level="project" />
     <orderEntry type="library" name="R Skeletons" level="project" />
     <orderEntry type="librar]y" name="R Skeletons" level="project" />
     <orderEntry type="library" name="R Skeletons" level="project" />
     <orderEntry type="library" name="R Skeletons" level="project" />
     <orderEntry type="library" name="R Skeletons" level="project" />
     <orderEntry type="library" name="R Skeletons" level="project" />

```

```text
2017-01-25 13:22:05,927 [ 134068]  ERROR - r4intellij.packages.LocalRUtil - Failed to run script: package_summaries.r
Exit code: 2
Error Output:  
java.lang.Throwable
	at com.intellij.openapi.diagnostic.Logger.error(Logger.java:132)
	at com.r4intellij.packages.RHelperUtil.getHelperOutput(RHelperUtil.java:64)
	at com.r4intellij.packages.LocalRUtil.getInstalledPackages(LocalRUtil.java:43)
	at com.r4intellij.packages.RPackageService.refreshIndex(RPackageService.java:152)
	at com.r4intellij.packages.RPackageService$1.run(RPackageService.java:90)
	at com.intellij.openapi.application.impl.ApplicationImpl$2.run(ApplicationImpl.java:309)
	at java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)
	at java.util.concurrent.FutureTask.run(FutureTask.java:266)
	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
	at java.lang.Thread.run(Thread.java:745)
2017-01-25 13:22:05,934 [ 134075]  ERROR - r4intellij.packages.LocalRUtil - IntelliJ IDEA 2016.3.3  Build #IU-163.11103.6 
2017-01-25 13:22:05,934 [ 134075]  ERROR - r4intellij.packages.LocalRUtil - JDK: 1.8.0_112-release 
2017-01-25 13:22:05,934 [ 134075]  ERROR - r4intellij.packages.LocalRUtil - VM: OpenJDK 64-Bit Server VM 
2017-01-25 13:22:05,934 [ 134075]  ERROR - r4intellij.packages.LocalRUtil - Vendor: JetBrains s.r.o 
2017-01-25 13:22:05,934 [ 134075]  ERROR - r4intellij.packages.LocalRUtil - OS: Mac OS X 
2017-01-25 13:22:05,934 [ 134075]  ERROR - r4intellij.packages.LocalRUtil - Last Action: RSkeletonsGenerator 
2017-01-25 13:22:05,947 [ 134088]  ERROR - plication.impl.ApplicationImpl - 1 
java.lang.ArrayIndexOutOfBoundsException: 1
	at com.r4intellij.packages.LocalRUtil$ParseDescriptorIntoPackage.apply(LocalRUtil.java:117)
	at com.r4intellij.packages.LocalRUtil$ParseDescriptorIntoPackage.apply(LocalRUtil.java:101)
	at com.google.common.collect.Iterators$8.transform(Iterators.java:799)
	at com.google.common.collect.TransformedIterator.next(TransformedIterator.java:48)
	at com.google.common.collect.Iterators.addAll(Iterators.java:362)
	at com.google.common.collect.Sets.newHashSet(Sets.java:238)
	at com.google.common.collect.Sets.newHashSet(Sets.java:218)
	at com.r4intellij.packages.LocalRUtil.getInstalledPackages(LocalRUtil.java:46)
	at com.r4intellij.packages.RPackageService.refreshIndex(RPackageService.java:152)
	at com.r4intellij.packages.RPackageService$1.run(RPackageService.java:90)
	at com.intellij.openapi.application.impl.ApplicationImpl$2.run(ApplicationImpl.java:309)
	at java.util.concurrent.Executors$RunnableAdapter.call(Executors.java:511)
	at java.util.concurrent.FutureTask.run(FutureTask.java:266)
	at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1142)
	at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:617)
	at java.lang.Thread.run(Thread.java:745)
```


* import action does not work for chunks. (should package imports be extrapolated to the complete file?)

* 

http://www.jetbrains.org/intellij/sdk/docs/tutorials/custom_language_support/completion_contributor.html

* improved dplyr life template


## potential improvements and differences

* inline variable refactoring
```text
How to implement variable inline refactoring for custom language?

Hello,

Is there a recommended pattern (like an abstract class) to provide a variable inlining refactoring for a custom language? For the r4intellij plugin I'm working on, usage search and the reference provider work well, so I'd think/hope that reference inlining should not be too hard.



I've found the thread https://intellij-support.jetbrains.com/hc/en-us/community/posts/206783125-Refactoring-Architecture-for-Plugins which is mentioning `com.intellij.refactoring.inline.InlineParameterDialog`, and I've also discovered "refactoring.inlineHandler" to register my inline refactoring but it's still a bit unclear to me how to proceed.

Thanks,
```

Examples 
```text
/Applications/IntelliJ IDEA.app/Contents/lib/idea.jar!/com/intellij/xml/refactoring/XmlInlineHandler.class:14
com.intellij.lang.refactoring.InlineActionHandler
<extensionPoint name="inlineActionHandler" interface="com.intellij.lang.refactoring.InlineActionHandler"/>

```


* mandatory dependency on Send2Console / or add suggestion balloon

Send to Console improvement:
    * send to console: jump to next line after eval (option?) 
    * eval current top-level expression (option?)
    * also add options to send line to current run console instead 
    * later: potentially add separate impl for R4intellij for more smooth integration




### General 

* auto-detect R and do not force user to specify installation location
* function help should be context aware
* no live-templates
* structure view
* run does not work for macos


### intentions & inspections

* auto-import missing packages (use com.jetbrains.the`r.packages.TheRPackagesUtil#getInstalledPackages)
* auto-install missing packages in require
* missing arg inspection does not recognize dplyr piping --> Ignore first arg if right-hand-size of pipe 
* quick fix to simplify/ remove the dot in  `filtGraphData %>% graph.data.frame(., directed=TRUE)` if first arg

* 





## later features

* after function name completion, cursor should end up between brackets

* implement new fenceprovider for enhanced RMd snippet injection https://github.com/JetBrains/intellij-plugins/pull/464#event-918221586

* shortcut to evaluate current expression and proceed

* intention to replace tidyverse imports with library(tidyverse)

* highlight packages with naming conflicts (or indicate it visually in the IDE using virtual comment)

* path completion within strings. Regular completion provider seems to apply.
    * support relative paths liks `test.txt`, `../foo.txt`
    * warn about missing data; and 
    
```r
# similar to type annotation
# @type recursive : logical

# Examples: 
# @working-dir ../../
# @working-dir ~/Users/
# @working-dir ${FOO}/bar

```
* show library import suggestions also for infix operators (like %<>% --> magrittr)
* also support rstudio like sectioning 

* make sure all rstudio refactorings work as well

![](.todo_images/refactorings.png)

---
* add title to package view like in RS:

![](.todo_images/packages_rstudio.png)

* also add links to package names to go to cran/bioconductor homepages

---
integrate with table editor in intellij

* https://www.jetbrains.com/help/idea/2016.3/working-with-the-table-editor.html
* Allow to open table by clicking (R Console session required)

* warn if data-frame arguments are not declared in script (with comment annotation option to flag presence). Since this is not possible on a general we could/should provide it for certain APIs like tidyverse

* intention in case of naming conflicts (same functions in imported packages) suggest to add prefix to method call (allow to override by annotation)

* bug: renaming for loop variables is broken

```r
for (name in packageNames) {
    if (paste(name, "r", sep=".") %in% list.files(path=args[1])) {
        next
    }
}
```

* offer quickfix to surround if expression with curly brackets


* necessity of 
```
application.invokeLater(new Runnable() {
    @Override
    public void run() {

        application.runWriteAction(new Runnable() {
        ....
```



* knit button
    * use `ExecutionManager.getInstance(project).startRunProfile();`
    
* post fix template support https://www.jetbrains.com/help/idea/2016.3/using-postfix-templates.html
* change signature refac
```
    <extensionPoint name="refactoring.changeSignatureUsageProcessor"
                    interface="com.intellij.refactoring.changeSignature.ChangeSignatureUsageProcessor"/>
```




## Send To Console Improvmentns

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


* add warning when using T/F https://www.r-bloggers.com/r-tip-avoid-using-t-and-f-as-synonyms-for-true-and-false/

###  Rnotebook support


Direct md embedding like in Rstudio

![](.todo_images/embedded_images.png)

are not possible in the intellij editor, see 
https://intellij-support.jetbrains.com/hc/en-us/community/posts/206756045-Displaying-an-image-in-source-code-editor


http://stackoverflow.com/questions/29718926/saving-the-state-of-a-webview-and-reloading-the-position

http://rmarkdown.rstudio.com/r_notebook_format.html

http://rmarkdown.rstudio.com/r_notebooks.html#output_storage

![](.todo_images/chunk_storage.png)

The document’s chunk outputs are also stored in an internal RStudio folder beneath the project’s .Rproj.user folder. If you work with a notebook but don’t have a project open, the outputs are stored in the RStudio state folder in your home directory (the location of this folder varies between the desktop and the server).


## Markdown impro wishlist

* aligned cursor with preview
* highlight search results in preview
* search in preview
* jump to code from preview


## Enhanced code completion

* Make use of CompletionType enum to finetune/speed up auto-completion

* provide completion for named function parameters
* help for function parameter should open function help


## Misc

What about packrat? http://rstudio.github.io/packrat/walkthrough.html


Also see [OpenApi notes](openapi_notes.md)




Brainstorming  & Roadmap
=======

* Basic refactorings to match StatET ()

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
