## @type x : character
f <- function(x) x

for (i in 1:10) {
  <warning descr="x expected to be of type character, found type numeric">f(i)</warning>
}