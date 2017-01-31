## @type e1 : logical
## @type e2 : logical
## @return logical
"|" <- function (e1, e2)  .Primitive("|")

## @type x : character
f <- function(x) x

<warning descr="x expected to be of type character, found type logical">f(T | F)</warning>

x <- <warning descr="e2 expected to be of type logical, found type character">TRUE | "22"</warning>