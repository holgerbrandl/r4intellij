## @type fumble : numeric
## @type fooey : numeric
fun <- function(fumble, fooey) {
    fumble + fooey
}


<warning descr="formal argument f matched by multiply actual arguments">fun(f = 1, foo = 2)</warning>

fun(f = 1, fooey = 2)