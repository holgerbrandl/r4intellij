---
title: simple site
tagline: Easy websites with GitHub Pages
---

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [I've installed a new package. How can I force the index to be rebuild?](#ive-installed-a-new-package-how-can-i-force-the-index-to-be-rebuild)
- [R4Intellij fails to index package XY. What can I do?](#r4intellij-fails-to-index-package-xy-what-can-i-do)
- [Why doesn't R4Intellij provide R-Session aware completion?](#why-doesnt-r4intellij-provide-r-session-aware-completion)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## I've installed a new package. How can I force the index to be rebuild?
The index will be updated when Intellij is restarted.

## R4Intellij fails to index package XY. What can I do?
Submit a ticket to our tracker. The indexing is known to fail occassionally, which is mainly because we have not yet discovered a fail-safe way to extract all package functions including their documentation.

## Why doesn't R4Intellij provide R-Session aware completion?
Because all it does is static code analysis at the moment.


## Why do I get an error when the plugin is indexing the 'xlsx' package?

Make sure that the package is [installed corrected](http://stackoverflow.com/questions/34971966/how-does-one-configure-rjava-on-osx-to-select-the-right-jvm-jinit-failing).