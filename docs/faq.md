
[TOC]: # "Contents"

# Contents
- [I've installed a new package. How can I force the index to be rebuild?](#ive-installed-a-new-package-how-can-i-force-the-index-to-be-rebuild)
- [R4Intellij fails to index package XY. What can/should I do?](#r4intellij-fails-to-index-package-xy-what-canshould-i-do)
- [Why do I get an error when the IDE is indexing packages relying on java (e.g. 'xlsx', 'rJava')?](#why-do-i-get-an-error-when-the-ide-is-indexing-packages-relying-on-java-eg-xlsx-rjava)
- [Why doesn't R4Intellij provide R-Session aware completion?](#why-doesnt-r4intellij-provide-r-session-aware-completion)



## I've installed a new package. How can I force the index to be rebuild?
The index will be updated when Intellij is restarted.

## R4Intellij fails to index package XY. What can/should I do?

If indexing of a package XY fails, you will not get proper code completion for functions from that package. So if you do not directly use XY in your scripts, you could simply ignore the problem.


To index a package, the IDE will internally attempt to load it into an R session. If this is not possible, the IDE can not index it, and will report an error once. The IDE will not reattempt to index it again unless you manually trigger an index refresh (_Tools_->_Refresh R Package Index_).

Most indexing issues are caused by faulty R installations. So before filing a ticket please make sure that you can load the affected package in R without any problem.

If your R installation is fine, the package can be loaded, please submit a ticket to our tracker. The indexing is known to fail occasionally, which is mainly because we have not yet discovered a fail-safe way to extract all package functions including their documentation.

To help us tracing down the problem, run the indexing step manually, and report back to us what goes wrong:
```bash
wget https://raw.githubusercontent.com/holgerbrandl/r4intellij/master/r-helpers/skeletonize_package.R

pckgName=tcltk ## your index-failing package here!!
R -f skeletonize_package.R --args  ${pckgName} ${pckgName}.skeleton.R
```

## Why do I get an error when the IDE is indexing packages relying on java (e.g. 'xlsx', 'rJava')?

Make sure that the Java [is correctly configured in R](http://stackoverflow.com/questions/34971966/how-does-one-configure-rjava-on-osx-to-select-the-right-jvm-jinit-failing)

## Why doesn't R4Intellij provide R-Session aware completion?

Because all it does is static code analysis at the moment. You can vote for [#96](https://github.com/holgerbrandl/r4intellij/issues/96).

