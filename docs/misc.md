<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**  *generated with [DocToc](https://github.com/thlorenz/doctoc)*

- [Competitors](#competitors)
  - [TheR](#ther)
  - [TinnR](#tinnr)
  - [StatET](#statet)
  - [Parser packages](#parser-packages)
- [hydrogen](#hydrogen)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

__


## Competitors

### TheR

https://github.com/ktisha/TheRPlugin


cool
* dot support for name completion
* skeleton libraries allow to browse local packages


### TinnR

* Allows for Rgui interaction to evaluate line or selection
    * including list variables or objects, clearing console, even stopping the current process.
* Code formatter
* Bracket matching & checking
* Commenting uncommenting


### StatET

* rename variable,
* introduce local variable, 
* inline local variable,  
* extract function,
* Generate Element Comment
 
### RStudio


#### Extract function

just limited support (bug?) in RS
```r
orthoGroups = data_frame(contig=V(orthoGraph)$name) %>% inner_join(vec_as_df(clusters(orthoGraphNoSmes)$membership, "contig", "ortho_group"))

## becomes
orthoGroups = data_frame(contig=V(orthoGraph)$name) %>% inner_join(
extract_group_model <- function(orthoGraphNoSmes) {
  vec_as_df(clusters(orthoGraphNoSmes)$membership, "contig", "ortho_group")
})

```



### Parser packages

parser package for R
* Uses almost identical version of R grammar
* According to parser docu: created using bison
* Source file creates c-parser


## hydrogen

* https://atom.io/packages/hydrogen


Website Admin
=============

Used theme https://github.com/pietromenna/jekyll-cayman-theme

Main docs on github
https://help.github.com/articles/using-jekyll-as-a-static-site-generator-with-github-pages/

Local deployment with jekyll (see from https://help.github.com/articles/setting-up-your-github-pages-site-locally-with-jekyll/)


setup
```bash

gem update --system  

echo "
source 'https://rubygems.org'
gem 'github-pages', group: :jekyll_plugins
" > Gemfile

bundle install
```

to run:

```bash
# from http://kbroman.org/simple_site/pages/local_test.html
# gem install github-pages
# gem update github-pages
# jekyll build

bundle exec jekyll serve
```

See https://support.rstudio.com/hc/en-us/articles/200710523-Navigating-Code

### Table of contents

Use https://github.com/thlorenz/doctoc

```bash
cd /Users/brandl/projects/rplugin/r4intellij/docs
doctoc .
doctoc --title "**Content**" README.md

```

### Website Todos

* https://github.com/ktisha/TheRPlugin


### Misc

* no automatic menu is possible without plugin: http://stackoverflow.com/questions/31194588/generate-a-menu-in-jekyll
* don't use .-prefixes https://help.github.com/articles/files-that-start-with-an-underscore-are-missing/
* add pages to jekyll site https://jekyllrb.com/docs/pages/

