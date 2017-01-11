## @type max : complex
## @type print.gap : integer
## @type digits : complex
## @type right : logical
## @type quote : logical|character
## @type useSource : logical
## @type na.print : character
print.default <- function (x, digits = NULL, quote = TRUE, na.print = NULL, print.gap = NULL,
    right = FALSE, max = NULL, useSource = TRUE, ...)
{
    noOpt <- missing(digits) && missing(quote) && missing(na.print) &&
        missing(print.gap) && missing(right) && missing(max) &&
        missing(useSource) && missing(...)
    .Internal(print.default(x, digits, quote, na.print, print.gap,
        right, max, useSource, noOpt))
}

print.some <- function(x) print(x[1])

print(x)

## @type x : character
f <- function(x) {

}

<warning descr="x expected to be of type character, found type numeric">f(1)</warning>
f(NULL)