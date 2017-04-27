##
## Exported symobls in package `bitops`
##

## Exported package methods

bitFlip <- function (a, bitWidth = 32) 
{
    .Call("bitFlip", a, bitWidth, PACKAGE = "bitops")
}


bitAnd <- function (a, b) 
{
    .Call("bitAnd", a, b, PACKAGE = "bitops")
}


bitXor <- function (a, b) 
{
    .Call("bitXor", a, b, PACKAGE = "bitops")
}


bitOr <- function (a, b) 
{
    .Call("bitOr", a, b, PACKAGE = "bitops")
}


cksum <- function (a) 
{
    x <- nchar(as.character(a)) * 0
    x <- x + .C("cksum", length(a), as.character(a), val = as.numeric(x), 
        NAOK = TRUE, DUP = TRUE, PACKAGE = "bitops")$val
    x[is.na(a)] <- NA
    x
}


bitShiftL <- function (a, b) 
{
    .Call("bitShiftL", a, b, PACKAGE = "bitops")
}


bitShiftR <- function (a, b) 
{
    .Call("bitShiftR", a, b, PACKAGE = "bitops")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Bitwise Operations"

.skeleton_package_version = "1.0-6"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF