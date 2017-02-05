# @type x : numeric
# @type y : numeric
## see
test_function <- function(x, <warning descr="Unused parameter y">y</warning>, <warning descr="Unused parameter z">z</warning>, d) {
    print("This is test function")
    barplot(x)
    x + 1 + 1
    d
}

z <- "dsds"
test_function(z, 2, 1, 1)