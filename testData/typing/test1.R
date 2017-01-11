## @type x : numeric
## @type y : numeric
test_function <- function(x, y, z, d) {
    print("This is test function")
    barplot(x)
    x + 1 + 1
    z
}

z <- "dsds"
<warning descr="x expected to be of type numeric, found type character">test_function(z, 2, 1, 1)</warning>