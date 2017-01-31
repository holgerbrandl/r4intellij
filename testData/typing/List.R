## @type x : complex
sin <- function (x)  .Primitive("sin")

x <- list(name="Liana", age = 22)

<warning descr="x expected to be of type complex, found type character">sin(x$name)</warning>

<warning descr="x expected to be of type complex, found type null">sin(x$gender)</warning>

x$gender <- "female"

<warning descr="x expected to be of type complex, found type character">sin(x$gender)</warning>

x$gender <- NULL

<warning descr="x expected to be of type complex, found type null">sin(x$gender)</warning>

<warning descr="x expected to be of type complex, found type list(name: character)">sin(x[1])</warning>

<warning descr="x expected to be of type complex, found type character">sin(x[[1]])</warning>