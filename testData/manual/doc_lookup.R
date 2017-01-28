
# 1) help for library functions
basename(dirname(dirname(hResults[1])))


# 2) resolve ambiguous method names using imports
require(plyr)


arrange(iris)


# 3) example without docstring that override package (should provide help also for assignment)


#' This function mutates the argument into an alien with sharp teeth
transform = function(a) a

bar = transform(2)

# 4) local method redef without docstring (should show nothing)
slice = function(b) b

foo = slice(2)

