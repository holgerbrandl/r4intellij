# @type x : numeric
# @type y : numeric
test_function1 <- function(<caret>x, y, z, d) {
    print("This is test function")
    barplot(x)
    x + 1 + 1
    z
}

z <- "dsds"
test_function1(z, 2, 1, 1)