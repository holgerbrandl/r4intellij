##
## Exported symobls in package `digest`
##

## Exported package methods

sha1 <- function (x, digits = 14L, zapsmall = 7L, ...) 
{
    UseMethod("sha1")
}


hmac <- function (key, object, algo = c("md5", "sha1", "crc32", "sha256", 
    "sha512"), serialize = FALSE, raw = FALSE, ...) 
{
    padded.key <- padWithZeros(key, algo)
    i.xored.key <- xor(padded.key, makeRaw(54))
    character.digest <- digest(c(i.xored.key, makeRaw(object)), 
        algo = algo, serialize = serialize, ...)
    raw.digest <- makeRaw.digest(character.digest)
    o.xored.key <- xor(padded.key, makeRaw(92))
    result <- digest(c(o.xored.key, raw.digest), algo = algo, 
        serialize = serialize, ...)
    if (raw) 
        result <- makeRaw.digest(result)
    return(result)
}


digest <- function (object, algo = c("md5", "sha1", "crc32", "sha256", 
    "sha512", "xxhash32", "xxhash64", "murmur32"), serialize = TRUE, 
    file = FALSE, length = Inf, skip = "auto", ascii = FALSE, 
    raw = FALSE, seed = 0, errormode = c("stop", "warn", "silent")) 
{
    algo <- match.arg(algo)
    errormode <- match.arg(errormode)
    .errorhandler <- function(txt, obj = "", mode = "stop") {
        if (mode == "stop") {
            stop(txt, obj, call. = FALSE)
        }
        else if (mode == "warn") {
            warning(txt, obj, call. = FALSE)
            return(invisible(NA))
        }
        else {
            return(invisible(NULL))
        }
    }
    if (is.infinite(length)) {
        length <- -1
    }
    if (is.character(file) && missing(object)) {
        object <- file
        file <- TRUE
    }
    if (serialize && !file) {
        object <- if ("nosharing" %in% names(formals(base::serialize))) 
            base::serialize(object, connection = NULL, ascii = ascii, 
                nosharing = TRUE)
        else base::serialize(object, connection = NULL, ascii = ascii)
        if (any(!is.na(pmatch(skip, "auto")))) {
            if (ascii) {
                skip <- which(object[1:30] == as.raw(10))[4]
            }
            else {
                skip <- 14
            }
        }
    }
    else if (!is.character(object) && !inherits(object, "raw")) {
        return(.errorhandler(paste("Argument object must be of type character", 
            "or raw vector if serialize is FALSE"), mode = errormode))
    }
    if (file && !is.character(object)) 
        return(.errorhandler("file=TRUE can only be used with a character object", 
            mode = errormode))
    algoint <- switch(algo, md5 = 1, sha1 = 2, crc32 = 3, sha256 = 4, 
        sha512 = 5, xxhash32 = 6, xxhash64 = 7, murmur32 = 8)
    if (file) {
        algoint <- algoint + 100
        object <- path.expand(object)
        if (!file.exists(object)) {
            return(.errorhandler("The file does not exist: ", 
                object, mode = errormode))
        }
        if (!isTRUE(!file.info(object)$isdir)) {
            return(.errorhandler("The specified pathname is not a file: ", 
                object, mode = errormode))
        }
        if (file.access(object, 4)) {
            return(.errorhandler("The specified file is not readable: ", 
                object, mode = errormode))
        }
    }
    if (is.character(skip)) 
        skip <- 0
    val <- .Call(digest_impl, object, as.integer(algoint), as.integer(length), 
        as.integer(skip), as.integer(raw), as.integer(seed))
    return(val)
}


AES <- function (key, mode = c("ECB", "CBC", "CTR"), IV = NULL) 
{
    mode <- match(match.arg(mode), modes)
    if (!(mode %in% c(1:2, 6))) 
        stop("Only ECB, CBC and CTR mode encryption are supported.")
    key <- as.raw(key)
    IV <- as.raw(IV)
    context <- .Call(AESinit, key)
    block_size <- 16
    key_size <- length(key)
    rm(key)
    encrypt <- function(text) {
        if (typeof(text) == "character") 
            text <- charToRaw(text)
        if (mode == 1) 
            .Call(AESencryptECB, context, text)
        else if (mode == 2) {
            len <- length(text)
            if (len%%16 != 0) 
                stop("Text length must be a multiple of 16 bytes")
            result <- raw(length(text))
            for (i in seq_len(len/16)) {
                ind <- (i - 1) * 16 + 1:16
                IV <<- .Call(AESencryptECB, context, xor(text[ind], 
                  IV))
                result[ind] <- IV
            }
            result
        }
        else if (mode == 6) {
            len <- length(text)
            blocks <- (len + 15)%/%16
            result <- raw(16 * blocks)
            zero <- as.raw(0)
            for (i in 1:blocks) {
                result[16 * (i - 1) + 1:16] <- IV
                byte <- 16
                repeat {
                  IV[byte] <<- as.raw((as.integer(IV[byte]) + 
                    1)%%256)
                  if (IV[byte] != zero || byte == 1) 
                    break
                  byte <- byte - 1
                }
            }
            result <- .Call(AESencryptECB, context, result)
            length(result) <- len
            xor(text, result)
        }
    }
    decrypt <- function(ciphertext, raw = FALSE) {
        if (mode == 1) 
            result <- .Call(AESdecryptECB, context, ciphertext)
        else if (mode == 2) {
            len <- length(ciphertext)
            if (len%%16 != 0) 
                stop("Ciphertext length must be a multiple of 16 bytes")
            result <- raw(length(ciphertext))
            for (i in seq_len(len/16)) {
                ind <- (i - 1) * 16 + 1:16
                res <- .Call(AESdecryptECB, context, ciphertext[ind])
                result[ind] <- xor(res, IV)
                IV <<- ciphertext[ind]
            }
        }
        else if (mode == 6) 
            result <- encrypt(ciphertext)
        if (!raw) 
            result <- rawToChar(result)
        result
    }
    structure(list(encrypt = encrypt, decrypt = decrypt, block_size = function() block_size, 
        IV = function() IV, key_size = function() key_size, mode = function() modes[mode]), 
        class = "AES")
}


makeRaw <- function (object) 
UseMethod("makeRaw")




## Package Data

# none


## Package Info

.skeleton_package_title = "Create Compact Hash Digests of R Objects"

.skeleton_package_version = "0.6.12"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF