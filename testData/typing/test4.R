## @type recursive : logical
## @rule (... : T, recursive = FALSE) -> max(T)
c <- function (..., recursive = FALSE)  .Primitive("c")


a <- c("1", "2", "3", recursive = FALSE)

## @type x : numeric
test_function <- function(x) {
    x
}

<warning descr="x expected to be of type numeric, found type character">test_function(a)</warning>

## @rule (x : T, y : M) -> max(T, M)
max <- function(x, y) {
    x
}

b <- max("a", 1)
<warning descr="x expected to be of type numeric, found type character">test_function(b)</warning>

## @rule (x : logical | character = TRUE) -> numeric
f <- function(x) {
    return(bar(x))
}

<warning descr="recursive = FALSE expected to be of type logical, found type numeric">c(recursive=f(TRUE))</warning>