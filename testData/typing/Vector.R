## @type recursive : logical
## @rule (... : T, recursive = FALSE) -> max(T)
c <- function (..., recursive = FALSE)  .Primitive("c")

x <- c(1, 2, 3)

x[4]  <- "bug"

##@type x : numeric
f <- function(x)x

<warning descr="x expected to be of type numeric, found type character">f(x)</warning>

y <- c(1, 2, 3, NA, 5)
f(y)

z <- c(1, 2, NA_character_)
<warning descr="x expected to be of type numeric, found type character">f(z)</warning>

