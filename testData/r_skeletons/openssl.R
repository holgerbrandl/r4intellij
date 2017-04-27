##
## Exported symobls in package `openssl`
##

## Exported package methods

write_p7b <- function (ca, path = NULL) 
{
    ca <- if (inherits(ca, "cert")) {
        list(ca)
    }
    else {
        lapply(ca, read_cert)
    }
    bin <- .Call(R_write_pkcs7, ca)
    if (is.null(path)) 
        return(bin)
    writeBin(bin, path)
    invisible(path)
}


write_p12 <- function (key = NULL, cert = NULL, ca = NULL, name = NULL, password = NULL, 
    path = NULL) 
{
    if (!length(key) && !length(cert) && !length(ca)) 
        stop("Either 'key' or 'cert' or 'ca' must be given")
    if (is.function(password)) 
        password <- password("Enter a new password for your p12 file")
    if (length(key)) 
        key <- read_key(key)
    if (length(cert)) 
        cert <- read_cert(cert)
    if (length(name)) 
        stopifnot(is.character(name))
    if (length(password)) 
        stopifnot(is.character(password))
    bin <- .Call(R_write_pkcs12, key, cert, ca, name, password)
    if (is.null(path)) 
        return(bin)
    writeBin(bin, path)
    invisible(path)
}


blake2b <- function (x, key = NULL) 
{
    rawstringhash(x, "blake2b512", key)
}


rand_bytes <- function (n = 1) 
{
    if (!is.numeric(n)) {
        stop("Please provide a numeric value for n")
    }
    .Call(R_RAND_bytes, n)
}


read_cert <- function (file, der = is.raw(file)) 
{
    buf <- read_input(file)
    cert <- if (der) {
        parse_der_cert(buf)
    }
    else {
        parse_pem_cert(buf)
    }
    structure(cert, class = "cert")
}


aes_ctr_encrypt <- function (data, key, iv = rand_bytes(16)) 
{
    aes_encrypt(data, key, iv, "ctr")
}


aes_gcm_decrypt <- function (data, key, iv = attr(data, "iv")) 
{
    aes_decrypt(data, key, iv, "gcm")
}


sha384 <- function (x, key = NULL) 
{
    rawstringhash(x, "sha384", key)
}


signature_create <- function (data, hash = sha1, key = my_key(), password = askpass) 
{
    data <- path_or_raw(data)
    sk <- read_key(key, password = password)
    md <- if (is.null(hash)) 
        parse_hash(data)
    else hash(data)
    if (!is.raw(md) || !(length(md) %in% c(16, 20, 28, 32, 48, 
        64))) 
        stop("data must be md5, sha1, or sha2 digest")
    hash_sign(md, sk)
}


base64_encode <- function (bin, linebreaks = FALSE) 
{
    if (is.character(bin)) {
        bin <- charToRaw(paste(bin, collapse = ""))
    }
    stopifnot(is.raw(bin))
    .Call(R_base64_encode, bin, as.logical(linebreaks))
}


decrypt_envelope <- function (data, iv, session, key = my_key(), password) 
{
    sk <- read_key(key, password = password)
    stopifnot(inherits(sk, "rsa"))
    stopifnot(is.raw(iv))
    stopifnot(is.raw(session))
    data <- path_or_raw(data)
    .Call(R_envelope_decrypt, data, iv, session, sk)
}


encrypt_envelope <- function (data, pubkey = my_pubkey()) 
{
    pk <- read_pubkey(pubkey)
    stopifnot(inherits(pk, "rsa"))
    data <- path_or_raw(data)
    out <- .Call(R_envelope_encrypt, data, pk)
    structure(out, names = c("iv", "session", "data"))
}


my_pubkey <- function () 
{
    path <- Sys.getenv("USER_PUBKEY", Sys.getenv("USER_KEY", 
        "~/.ssh/id_rsa.pub"))
    if (file.exists(path)) 
        return(read_pubkey(path))
    key <- my_key()
    key$pubkey
}


my_key <- function () 
{
    path <- Sys.getenv("USER_KEY", "~/.ssh/id_rsa")
    if (!file.exists(path)) 
        stop("No suitable user key found.")
    read_key(path)
}


rand_num <- function (n = 1) 
{
    x <- matrix(as.numeric(rand_bytes(n * 8)), ncol = 8)
    as.numeric(x %*% 256^-(1:8))
}


ec_keygen <- function (curve = c("P-256", "P-384", "P-521")) 
{
    key <- .Call(R_keygen_ecdsa, match.arg(curve))
    structure(key, class = c("key", "ecdsa"))
}


ripemd160 <- function (x, key = NULL) 
{
    rawstringhash(x, "ripemd160", key)
}


sha1 <- function (x, key = NULL) 
{
    rawstringhash(x, "sha1", key)
}


sha2 <- function (x, size = 256, key = NULL) 
{
    rawstringhash(x, paste0("sha", size), key)
}


sha512 <- function (x, key = NULL) 
{
    rawstringhash(x, "sha512", key)
}


read_p7b <- function (file, der = is.raw(file)) 
{
    buf <- read_input(file)
    if (!isTRUE(der)) {
        buf <- parse_pem_pkcs7(buf)
    }
    data <- structure(parse_der_pkcs7(buf), names = c("certs", 
        "crl"))
    lapply(data$certs, read_cert)
}


askpass <- function (prompt = "Please enter your password: ") 
{
    FUN <- getOption("askpass", readline)
    FUN(prompt)
}


blake2s <- function (x, key = NULL) 
{
    rawstringhash(x, "blake2s256", key)
}


aes_ctr_decrypt <- function (data, key, iv = attr(data, "iv")) 
{
    aes_decrypt(data, key, iv, "ctr")
}


aes_cbc_encrypt <- function (data, key, iv = rand_bytes(16)) 
{
    aes_encrypt(data, key, iv, "cbc")
}


rsa_encrypt <- function (data, pubkey = my_pubkey()) 
{
    pk <- read_pubkey(pubkey)
    stopifnot(inherits(pk, "rsa"))
    stopifnot(is.raw(data))
    .Call(R_rsa_encrypt, data, pk)
}


ca_bundle <- function () 
{
    path <- system.file("cacert.pem", package = "openssl")
    read_cert_bundle(path)
}


read_cert_bundle <- function (file) 
{
    buf <- read_input(file)
    lapply(split_pem(buf), read_cert)
}


openssl_config <- function () 
{
    out <- .Call(R_openssl_config)
    structure(out, names = c("version", "ec"))
}


read_pubkey <- function (file, der = is.raw(file)) 
{
    if (inherits(file, "key") || inherits(file, "cert")) 
        return(as.list(file)$pubkey)
    if (is_pubkey_str(file)) 
        file <- textConnection(file)
    buf <- read_input(file)
    key <- if (isTRUE(der)) {
        parse_der_pubkey(buf)
    }
    else if (length(grepRaw("BEGIN SSH2 PUBLIC KEY", buf, fixed = TRUE))) {
        parse_ssh_pem(buf)
    }
    else if (is_pubkey_str(buf)) {
        parse_openssh(buf)
    }
    else {
        names <- pem_names(buf)
        if (!length(names) || !any(nchar(names) > 0)) {
            stop("Failed to parse pubkey PEM file")
        }
        else if (any(grepl("RSA PUBLIC KEY", names))) {
            parse_legacy_pubkey(buf)
        }
        else if (any(grepl("PUBLIC", names))) {
            parse_pem_pubkey(buf)
        }
        else if (any(grepl("PRIVATE|PARAMETERS", names))) {
            derive_pubkey(read_key(buf, der = FALSE))
        }
        else if (any(grepl("CERTIFICATE", names))) {
            cert_pubkey(parse_pem_cert(buf))
        }
        else {
            stop("Invalid PEM type: ", names)
        }
    }
    if (is.null(attr(key, "class"))) 
        class(key) <- c("pubkey", pubkey_type(key))
    key
}


aes_keygen <- function (length = 16) 
{
    structure(rand_bytes(length), class = c("bytes", "aes", "raw"))
}


rsa_keygen <- function (bits = 2048) 
{
    key <- .Call(R_keygen_rsa, as.integer(bits))
    structure(key, class = c("key", "rsa"))
}


dsa_keygen <- function (bits = 1024) 
{
    key <- .Call(R_keygen_dsa, as.integer(bits))
    structure(key, class = c("key", "dsa"))
}


bignum_mod_exp <- function (a, b, m) 
{
    .Call(R_bignum_mod_exp, a, b, m)
}


write_der <- function (x, path = NULL) 
{
    bin <- der_export(x)
    if (is.null(path)) 
        return(bin)
    writeBin(unclass(bin), path)
    invisible(path)
}


read_p12 <- function (file, password = askpass) 
{
    buf <- read_input(file)
    data <- parse_pkcs12(buf, password)
    out <- list(cert = NULL, key = NULL, ca = NULL)
    if (length(data[[1]])) 
        out$cert <- read_cert(data[[1]], der = TRUE)
    if (length(data[[2]])) 
        out$key <- read_key(data[[2]], der = TRUE)
    if (length(data[[3]])) 
        out$ca <- lapply(data[[3]], read_cert, der = TRUE)
    return(out)
}


md4 <- function (x, key = NULL) 
{
    rawstringhash(x, "md4", key)
}


sha224 <- function (x, key = NULL) 
{
    rawstringhash(x, "sha224", key)
}


download_ssl_cert <- function (host = "localhost", port = 443) 
{
    if (grepl("https?://", host)) 
        stop("Argument 'host' must be a hostname, not url. Take out the https:// prefix.")
    stopifnot(is.character(host))
    .Call(R_download_cert, host, as.character(port))
}


aes_cbc_decrypt <- function (data, key, iv = attr(data, "iv")) 
{
    aes_decrypt(data, key, iv, "cbc")
}


aes_gcm_encrypt <- function (data, key, iv = rand_bytes(16)) 
{
    aes_encrypt(data, key, iv, "gcm")
}


fingerprint <- function (key, hashfun = md5) 
{
    UseMethod("fingerprint")
}


rsa_decrypt <- function (data, key = my_key(), password = askpass) 
{
    sk <- read_key(key, password)
    stopifnot(inherits(sk, "rsa"))
    stopifnot(is.raw(data))
    .Call(R_rsa_decrypt, data, sk)
}


md5 <- function (x, key = NULL) 
{
    rawstringhash(x, "md5", key)
}


base64_decode <- function (text) 
{
    if (is.raw(text)) {
        text <- rawToChar(text)
    }
    stopifnot(is.character(text))
    text <- paste(text, collapse = "")
    text <- gsub("[\r\n]", "", text)[[1]]
    .Call(R_base64_decode, text)
}


sha256 <- function (x, key = NULL) 
{
    rawstringhash(x, "sha256", key)
}


bignum_mod_inv <- function (a, m) 
{
    .Call(R_bignum_mod_inv, a, m)
}


write_ssh <- function (pubkey, path = NULL) 
{
    if (inherits(pubkey, "key")) 
        pubkey <- derive_pubkey(pubkey)
    if (!inherits(pubkey, "pubkey")) 
        stop("Invalid pubkey file.")
    str <- as.list(pubkey)$ssh
    if (is.null(path)) 
        return(str)
    writeLines(str, path)
    invisible(path)
}


ec_dh <- function (key = my_key(), peerkey, password = askpass) 
{
    key <- read_key(key, password = password)
    peerkey <- read_pubkey(peerkey)
    stopifnot(inherits(key, "ecdsa"))
    stopifnot(inherits(peerkey, "ecdsa"))
    .Call(R_diffie_hellman, key, peerkey)
}


signature_verify <- function (data, sig, hash = sha1, pubkey = my_pubkey()) 
{
    data <- path_or_raw(data)
    sig <- path_or_raw(sig)
    pk <- read_pubkey(pubkey)
    md <- if (is.null(hash)) 
        parse_hash(data)
    else hash(data)
    if (!is.raw(md) || !(length(md) %in% c(16, 20, 28, 32, 48, 
        64))) 
        stop("data must be md5, sha1, or sha2 digest")
    hash_verify(md, sig, pk)
}


read_key <- function (file, password = askpass, der = is.raw(file)) 
{
    buf <- read_input(file)
    key <- if (isTRUE(der)) {
        parse_der_key(buf)
    }
    else if (length(grepRaw("BEGIN OPENSSH PRIVATE KEY", buf, 
        fixed = TRUE))) {
        stop("OpenSSL does not support them fancy OPENSSH bcrypt/ed25519 keys")
    }
    else if (is_pubkey_str(buf)) {
        stop("Input is a public key. Use read_pubkey() to read")
    }
    else {
        names <- pem_names(buf)
        if (!length(names) || !any(nchar(names) > 0)) 
            stop("Failed to parse private key PEM file")
        if (any(grepl("PUBLIC", names))) 
            stop("Input is a public key. Use read_pubkey() to read")
        if (any(grepl("CERTIFICATE", names))) 
            stop("Input is a certificate. Use read_cert() to read.")
        if (!any(grepl("PRIVATE", names))) 
            stop("Invalid input: ", names)
        parse_pem_key(buf, password)
    }
    structure(key, class = c("key", pubkey_type(derive_pubkey(key))))
}


cert_verify <- function (cert, root = ca_bundle()) 
{
    if (is.raw(cert)) 
        cert <- list(cert)
    if (!is.list(cert)) 
        cert <- read_cert_bundle(cert)
    stopifnot(inherits(cert[[1]], "cert"))
    if (!is.raw(root) && !is.list(root)) {
        buf <- read_input(root)
        names <- pem_names(buf)
        if (any(grepl("CERT", names))) {
            root <- read_cert_bundle(root)
        }
        else {
            root <- read_pubkey(root)
        }
    }
    if (inherits(root, "pubkey")) {
        pubkey_verify_cert(cert[[1]], root)
    }
    else {
        stopifnot(all(sapply(root, inherits, "cert")))
        cert_verify_cert(cert[[1]], cert[-1], root)
    }
}


write_pem <- function (x, path = NULL, password = NULL) 
{
    str <- pem_export(x, password)
    if (is.null(path)) 
        return(str)
    writeLines(str, path)
    invisible(path)
}


bignum <- function (x, hex = FALSE) 
{
    if (inherits(x, "bignum")) 
        return(x)
    stopifnot(is.raw(x) || is.character(x) || is.numeric(x))
    if (is.numeric(x)) {
        if (is_positive_integer(x)) {
            x <- formatC(x, format = "fg")
        }
        else {
            stop("Cannot convert to bignum: x must be positive integer, character or raw", 
                call. = FALSE)
        }
    }
    if (is.character(x)) {
        if (identical(x, "0")) {
        }
        else if (isTRUE(hex)) {
            if (!grepl("^([a-fA-F0-9]{2})+$", x)) 
                stop("Value '", x, "' is not valid hex string", 
                  call. = FALSE)
        }
        else {
            if (!grepl("^[0-9]+$", x)) 
                stop("Value '", x, "' is not valid integer", 
                  call. = FALSE)
        }
    }
    .Call(R_parse_bignum, x, hex)
}


read_pem <- function (file) 
{
    buf <- read_input(file)
    out <- parse_pem(buf)
    data <- lapply(out, `[[`, "data")
    names <- vapply(out, `[[`, character(1), "name")
    structure(data, names = names)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Toolkit for Encryption, Signatures and Certificates Based onOpenSSL"

.skeleton_package_version = "0.9.6"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF