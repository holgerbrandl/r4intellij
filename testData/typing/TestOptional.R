## @optional x, y
f <- function(x, y, z, w) {
  z <- w
}

<warning descr="argument 'w' is missing, with no default">f()</warning>