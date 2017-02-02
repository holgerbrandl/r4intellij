## @type fumble : numeric
## @type fooey : numeric
f <- function(..., fumble, fooey) {
    fumble + fooey
}

f(f = 1, fooey = 2, fumble = 4)



# @type fumble : numeric
# @type fooey : numeric
fun <- function(fumble, fooey) {
    fumble + fooey
}

<warning descr="unused argument f = 1">fun(f = 1, fooey = 2, fumble = 4)</warning>