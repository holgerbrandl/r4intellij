f <- function(x) {
    if (x == 0) {
        return(FALSE)
    }
    y <- "text"
}

## @type x : numeric
bar <- function(x) {
    x
}

<warning descr="x expected to be of type numeric, found type logical|character">bar(f(1))</warning>