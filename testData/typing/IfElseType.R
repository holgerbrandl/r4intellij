f <- function(x) {
    if (x == 0) {
        "text"
    } else {
        1
    }
}

## @type x: logical
bar <- function(x) {
    x
}

<warning descr="x expected to be of type logical, found type numeric|character">bar(f(1))</warning>