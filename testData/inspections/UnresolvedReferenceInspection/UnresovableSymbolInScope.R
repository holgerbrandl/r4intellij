# @type x : numeric
# @type y : numeric
test_function <- function(x, y, z, d) {
    x + 1 + 1
    z
}

# z <- "dsds"
# although z shows up in the function body above, it should be detected as unresovled because the scope is different
test_function(<warning descr="Unresolved reference">z</warning>, 2, 1, 1)