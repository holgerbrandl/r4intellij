# @type x : numeric
# @type y : numeric
test_function1 <- function(<caret>x1, y, z, d) {
    print("This is test function")
    barplot(x1)
    x1 + 1 + 1
    z
}

z <- "dsds"
test_function1(z, 2, 1, 1)