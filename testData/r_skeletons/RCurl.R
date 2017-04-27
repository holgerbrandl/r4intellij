##
## Exported symobls in package `RCurl`
##

## Exported package methods

AUTH_DIGEST <- new("CURLAuth"
    , .Data = 2L
    , names = "CURLAUTH_DIGEST"
)


findHTTPHeaderEncoding <- function (str) 
{
    els = strsplit(str, "\\\r\\\n")
    v = lapply(els, getEncoding)
    if (any(!is.na(v))) {
        v[[!is.na(v)]][[1]]
    }
    else -1L
}


reset <- function (x, ...) 
standardGeneric("reset")


getFormParams <- function (query, isURL = grepl("^(http|\\?)", query)) 
{
    if (length(query) == 0) 
        return(NULL)
    if (isURL) 
        query = gsub(".*\\?", "", query)
    if (nchar(query) == 0) 
        return(character())
    els = strsplit(query, "[&=]")[[1]]
    i = seq(1, by = 2, length = length(els)/2)
    ans = structure(els[i + 1L], names = els[i])
    if (any(i <- is.na(ans))) 
        ans[i] = ""
    names(ans) = trim(names(ans))
    ans
}


base64Decode <- function (txt, mode = "character") 
base64(txt, FALSE, mode)


`.__T__[:base` <- "<environment>"

httpPUT <- function (url, content = NULL, ..., curl = getCurlHandle()) 
{
    if (!missing(content)) {
        val = if (is.character(content)) 
            charToRaw(paste(content, collapse = "\n"))
        else if (is.raw(content)) 
            content
        else stop("not certain how to convert content to the target type for a PUT request")
        getURLContent(url, infilesize = length(val), readfunction = val, 
            upload = TRUE, ..., curl = curl, customrequest = "PUT")
    }
    else getURLContent(url, ..., curl = curl, customrequest = "PUT")
}


debugGatherer <- function () 
{
    els = NULL
    info = NULL
    update = function(msg, type, curl) {
        els[[type + 1]] <<- c(els[[type + 1]], msg)
        info[[length(info) + 1]] <<- msg
        names(info)[length(info)] <<- names(type)
        0
    }
    reset = function() {
        els <<- list(text = character(), headerIn = character(), 
            headerOut = character(), dataIn = character(), dataOut = character(), 
            sslDataIn = character(), sslDataOut = character())
        info <<- list()
    }
    ans = list(update = update, value = function(collapse = "", 
        ordered = FALSE, ...) {
        if (ordered) return(info)
        if (is.null(collapse)) return(els)
        sapply(els, function(x) paste(x, collapse = collapse, 
            ...))
    }, reset = reset)
    class(ans) <- c("RCurlDebugHandler", "RCurlCallbackFunction")
    ans$reset()
    ans
}


getCurlMultiHandle <- function (..., .handles = list(...)) 
{
    ans = .Call("R_getCurlMultiHandle", PACKAGE = "RCurl")
    lapply(.handles, function(h) push(ans, h))
    ans
}


CFILE <- function (filename, mode = "r") 
{
    filename = path.expand(filename)
    .Call("R_openFile", filename, as.character(mode))
}


multiTextGatherer <- function (uris, binary = rep(NA, length(uris))) 
{
    if (is.numeric(uris)) 
        ans = lapply(1:uris, basicTextGatherer)
    else {
        ans = lapply(uris, basicTextGatherer)
        names(ans) = uris
    }
    class(ans) <- "MultiTextGatherer"
    ans
}


curlError <- function (type, msg, asError = TRUE) 
{
    if (!is.character(type)) {
        i = match(type, CURLcodeValues)
        typeName = if (is.na(i)) 
            character()
        else names(CURLcodeValues)[i]
    }
    typeName = gsub("^CURLE_", "", typeName)
    fun = (if (asError) 
        stop
    else warning)
    fun(structure(list(message = msg, call = sys.call()), class = c(typeName, 
        "GenericCurlError", "error", "condition")))
}


httpHEAD <- function (url, ..., curl = getCurlHandle()) 
{
    getURLContent(url, customrequest = "HEAD", nobody = TRUE, 
        ..., curl = curl)
}


CurlGlobalBits <- structure(0:3, .Names = c("none", "ssl", "win32", "all"), class = c("CurlGlobalBits", 
"BitIndicator"))


AUTH_BASIC <- new("CURLAuth"
    , .Data = 1L
    , names = "CURLAUTH_BASIC"
)


basicHeaderGatherer <- function (txt = character(), max = NA) 
basicTextGatherer(txt, max, parseHTTPHeader)


getForm <- function (uri, ..., .params = character(), .opts = list(), curl = getCurlHandle(), 
    .encoding = integer(), binary = NA, .checkParams = TRUE) 
{
    if (missing(.params)) {
        .params = list(...)
    }
    if (length(.params) == 0) 
        warning("No inputs passed to form")
    else if (missing(.opts) && .checkParams) 
        testCurlOptionsInFormParameters(.params)
    els = mapply(function(id, val) {
        paste(curlEscape(id), curlEscape(val), sep = "=", collapse = "&")
    }, names(.params), .params)
    args = paste(els, collapse = "&")
    uri = paste(uri, args, sep = if (grepl("\\?", uri)) 
        "&"
    else "?")
    getURLContent(uri, .opts = .opts, .encoding = .encoding, 
        binary = binary, curl = curl)
}


dupCurlHandle <- function (curl, ..., .opts = NULL, .encoding = integer()) 
{
    h = .Call("R_curl_easy_duphandle", curl, PACKAGE = "RCurl")
    curlSetOpt(..., .opts = .opts, curl = h, .encoding = .encoding)
    h
}


curlPercentEncode <- function (x, amp = TRUE, codes = PercentCodes, post.amp = FALSE) 
{
    if (!amp) {
        i = match("&", names(codes))
        if (!is.na(i)) 
            codes = codes[-i]
    }
    for (i in seq(along = codes)) {
        x = gsub(names(codes)[i], codes[i], x, fixed = TRUE)
    }
    if (post.amp) 
        x = gsub("%", "%25", x, fixed = TRUE)
    x
}


setBitIndicators <- function (vals, defs) 
{
    if (is.character(vals)) {
        i = match(vals, names(defs))
        if (any(is.na(i))) 
            stop("Unmatched bit field names ", paste(vals[is.na(i)], 
                collapse = ", "))
        vals = defs[i]
    }
    as.integer(sum(vals))
}


`.__T__|:base` <- "<environment>"

AUTH_NTLM <- new("CURLAuth"
    , .Data = 8L
    , names = "CURLAUTH_NTLM"
)


getURLContent <- function (url, ..., curl = getCurlHandle(.opts = .opts), .encoding = NA, 
    binary = NA, .opts = list(...), header = dynCurlReader(curl, 
        binary = binary, baseURL = url, isHTTP = isHTTP, encoding = .encoding), 
    isHTTP = length(grep("^[[:space:]]*http", url)) > 0) 
{
    url = as(url, "character")
    if (!missing(curl)) 
        curlSetOpt(.opts = .opts, curl = curl)
    if (is.logical(header)) {
        returnHeader = header
        header = dynCurlReader(curl, binary = binary, baseURL = url, 
            isHTTP = isHTTP, encoding = .encoding)
    }
    else returnHeader = FALSE
    if (!("headerfunction" %in% names(.opts))) {
        protect = missing(header)
        curlSetOpt(curl = curl, .isProtected = protect, headerfunction = header$update)
    }
    if (!isHTTP && !("writefunction" %in% names(.opts))) {
        protect = missing(header)
        curlSetOpt(curl = curl, .isProtected = protect, writefunction = header$update)
    }
    curlPerform(url = url, curl = curl, .opts = .opts)
    if (isHTTP && length(header$header())) {
        http.header = parseHTTPHeader(header$header())
        stop.if.HTTP.error(http.header)
    }
    if (returnHeader) 
        list(header = if (is(returnHeader, "AsIs")) header$header() else parseHTTPHeader(header$header()), 
            body = header$value())
    else header$value()
}


HTTP_VERSION_NONE <- 0L


parseHTTPHeader <- function (lines, multi = TRUE) 
{
    if (length(lines) < 1) 
        return(NULL)
    if (length(lines) == 1) 
        lines = strsplit(lines, "\r\n")[[1]]
    if (multi) {
        i = grep("^HTTP", lines)
        status = lines[max(i)]
        lines = lines[seq(max(i), length(lines))]
    }
    else status = lines[1]
    st = getStatus(status)
    if (st[["status"]] == 100) {
        if ((length(lines) - length(grep("^[[:space:]]*$", lines))) == 
            1) 
            return(st)
    }
    lines = lines[-1]
    lines = gsub("\r\n$", "", lines)
    lines = lines[lines != ""]
    if (FALSE) {
        header = lines[-1]
        header <- read.dcf(textConnection(header))
    }
    else {
        header = structure(sub("[^:]+: (.*)", "\\1", lines), 
            names = sub("([^:]+):.*", "\\1", lines))
    }
    header[["status"]] = st[["status"]]
    header[["statusMessage"]] = st[["message"]]
    header
}


`.__T__push:RCurl` <- "<environment>"

curlEscape <- function (urls) 
{
    .Call("R_curl_escape", as.character(urls), TRUE, PACKAGE = "RCurl")
}


AUTH_NONE <- new("CURLAuth"
    , .Data = 0L
    , names = "CURLAUTH_NONE"
)


httpDELETE <- function (url, ..., curl = getCurlHandle()) 
{
    getURLContent(url, customrequest = "DELETE", ..., curl = curl)
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

`.__T__pop:RCurl` <- "<environment>"

pop <- function (obj, val) 
standardGeneric("pop")


curlOptions <- function (..., .opts = list()) 
{
    .els = rev(merge(list(...), .opts))
    dups = duplicated(names(.els))
    if (any(dups)) {
        warning("Duplicated curl options: ", paste(names(.els)[dups], 
            collapse = ", "))
        .els = .els[!dups]
    }
    if (length(.els)) {
        if (any(names(.els) == "")) 
            stop("unnamed curl option(s): ", .els[names(.els) == 
                ""])
        names(.els) <- mapCurlOptNames(names(.els), asNames = TRUE)
        .els = .els[!is.na(names(.els))]
    }
    else .els = list()
    class(.els) = "CURLOptions"
    .els
}


AUTH_DIGEST_IE <- new("CURLAuth"
    , .Data = 16L
    , names = "CURLAUTH_DIGEST_IE"
)


AUTH_ONLY <- new("CURLAuth"
    , .Data = -2147483647L
    , names = "CURLAUTH_ONLY"
)


ftpUpload <- function (what, to, asText = inherits(what, "AsIs") || is.raw(what), 
    ..., curl = getCurlHandle()) 
{
    if (!asText && !inherits(what, "connection")) {
        file = file(what, "rb")
        on.exit(close(file))
    }
    else file = what
    curlPerform(url = to, upload = TRUE, readfunction = uploadFunctionHandler(file, 
        asText), ..., curl = curl)
}


SSLVERSION_SSLv2 <- 2L


CurlNetrc <- structure(0:2, .Names = c("ignored", "optional", "required"), class = c("CurlNetrcEnum", 
"Enum"))


.__C__CURLHandle <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("CURLHandle", package = "RCurl")
    , package = "RCurl"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

SSLVERSION_SSLv3 <- 3L


CurlFeatureBits <- structure(c(1L, 2L, 4L, 8L, 16L, 32L, 64L, 128L, 256L, 512L, 
1024L, 2048L, 4096L, 8192L, 16384L, 32768L), .Names = c("ipv6", 
"kerberos4", "ssl", "libz", "ntlm", "gssnegotiate", "debug", 
"asynchdns", "spnego", "largefile", "idn", "sspi", "conv", "curldebug", 
"tlsauth_srp", "ntlm_wb"), class = c("CurlFeatureBits", "BitIndicator"
))


push <- function (obj, val, id = character()) 
standardGeneric("push")


getURIAsynchronous <- function (url, ..., .opts = list(), write = NULL, curl = getCurlHandle(), 
    multiHandle = getCurlMultiHandle(), perform = Inf, .encoding = integer(), 
    binary = rep(NA, length(url))) 
{
    writeSupplied = !missing(write)
    if (!is.null(write) && !is.list(write)) 
        write = replicate(length(url), write)
    if (length(url) > 1) 
        curl = lapply(url, function(id) dupCurlHandle(curl))
    else curl = list(curl)
    if (length(.encoding)) 
        .encoding = rep(.encoding, length = length(url))
    if (is.null(write)) {
        writeSupplied = FALSE
        write = mapply(function(curl, url, binary) {
            force(url)
            force(curl)
            force(binary)
            dynCurlReader(curl, baseURL = url, binary = binary)
        }, curl, url, binary, SIMPLIFY = FALSE)
    }
    for (i in seq(along = url)) {
        w = write[[i]]
        if (inherits(w, "DynamicRCurlTextHandler")) {
            .opts[["headerfunction"]] = w$update
            .opts[["writefunction"]] = w$update
        }
        else if (inherits(w, "RCurlCallbackFunction")) 
            .opts[["writefunction"]] = w$update
        else .opts[["writefunction"]] = w
        opts = curlOptions(URL = url[i], ..., .opts = .opts)
        curlSetOpt(.opts = opts, curl = curl[[i]], .encoding = if (length(.encoding)) 
            .encoding[i]
        else integer())
        multiHandle = push(multiHandle, curl[[i]])
    }
    if (perform > 0) {
        ctr = 0
        while (TRUE) {
            status = curlMultiPerform(multiHandle)
            ctr <- ctr + 1
            if (status[2] == 0 || ctr > perform) 
                break
        }
        if (status[2] == 0 && (!writeSupplied || inherits(write, 
            "MultiTextGatherer"))) 
            return(sapply(write, function(w) w$value()))
    }
    list(multiHandle = multiHandle, write = write)
}


scp <- function (host, path, keypasswd = NA, user = getUserName(), rsa = TRUE, 
    key = sprintf(c("~/.ssh/id_%s.pub", "~/.ssh/id_%s"), if (rsa) "rsa" else "dsa"), 
    binary = NA, size = 5000, curl = getCurlHandle(), ...) 
{
    if (!is.na(user) && length(grep("@", host)) == 0) 
        host = paste(user, "@", host, sep = "")
    if (length(grep("^scp", host)) == 0) 
        host = paste("scp://", host, sep = "")
    if (!missing(path)) 
        host = paste(host, path, sep = "/")
    key = path.expand(key)
    opts = list(ssh.public.keyfile = key[1], ssh.private.keyfile = key[2], 
        ...)
    if (!is.na(keypasswd)) 
        opts$keypasswd = keypasswd
    opts$url = host
    if (is.na(binary) || binary) {
        binary = TRUE
        opts$writefunction = getNativeSymbolInfo("R_curl_write_binary_data")$address
        buf <- binaryBuffer(size)
        opts$file = buf@ref
    }
    else {
        h = basicTextGatherer()
        opts$writefunction = h$update
    }
    curlPerform(.opts = opts, curl = curl)
    if (binary) 
        as(buf, "raw")
    else h$value()
}


AUTH_NTLM_WB <- new("CURLAuth"
    , .Data = 32L
    , names = "CURLAUTH_NTLM_WB"
)


HTTP_VERSION_1_0 <- 1L


getCurlInfoConstants <- function () 
{
    x = .Call("R_getCURLInfoEnum", PACKAGE = "RCurl")
    names(x) = tolower(gsub("_", ".", names(x)))
    x
}


SSLVERSION_TLSv1 <- 1L


dynCurlReader <- function (curl = getCurlHandle(), txt = character(), max = NA, 
    value = NULL, verbose = FALSE, binary = NA, baseURL = NA, 
    isHTTP = NA, encoding = NA) 
{
    header = character()
    buf = NULL
    content.type = character()
    requestCount = 0
    curHeaderStatus = -1
    inBody = FALSE
    if (verbose) 
        cat("New call to dynCurlReader:", baseURL, "\n")
    update = function(str) {
        if (verbose) 
            cat("inBody? ", inBody, ", num bytes", nchar(str, 
                "bytes"), "\n", sep = "")
        if (!inBody && (length(str) == 0 || length(grep("^[[:space:]]+$", 
            str)))) {
            oldHeader = header
            header <<- c(txt, "")
            txt <<- character()
            if (is.na(isHTTP)) 
                isHTTP <<- length(grep("HTTP", header)) > 0
            if (isHTTP) {
                http.header = parseHTTPHeader(c(header, str))
                if (http.header[["status"]] == 100) {
                  curHeaderStatus <<- 100
                  val.ids = setdiff(names(http.header), c("status", 
                    "statusMessage", "message"))
                  if (length(val.ids)) 
                    header <<- http.header[val.ids]
                  else header <<- character()
                  return(nchar(str, "bytes"))
                }
                else curHeaderStatus = http.header[["status"]]
                if (length(oldHeader)) {
                  tmp = setdiff(names(oldHeader), names(header))
                  if (length(tmp)) 
                    header[tmp] <<- oldHeader[tmp]
                }
                content.type <<- getContentType(http.header, 
                  TRUE)
                if (is.na(binary)) 
                  binary = isBinaryContent(http.header, list(http.header["Content-Encoding"], 
                    content.type))
                if (is.na(http.header["status"])) {
                  header <<- character()
                  inBody <<- FALSE
                  return(nchar(str, "bytes"))
                }
            }
            else {
            }
            if (verbose) 
                cat("Setting option to read content-type", content.type[1], 
                  "character set", content.type["charset"], "\n")
            if (length(content.type) == 0 || (is.na(binary) || 
                binary)) {
                len = 5000
                buf <<- binaryBuffer(len)
                if (verbose) 
                  cat("Reading binary data:", content.type, "\n")
                curlSetOpt(writefunction = getNativeSymbolInfo("R_curl_write_binary_data")$address, 
                  file = buf@ref, curl = curl, .isProtected = c(TRUE, 
                    FALSE))
            }
            else {
                if (length(encoding) == 0 || is.na(encoding) || 
                  encoding == "") 
                  encoding <<- content.type["charset"]
                curlSetOpt(writefunction = update, .encoding = encoding, 
                  curl = curl, .isProtected = TRUE)
            }
            inBody <<- TRUE
            if (verbose) 
                print(header)
        }
        else {
            txt <<- c(txt, str)
            if (!is.na(max) && nchar(txt) >= max) 
                return(0)
        }
        nchar(str, "bytes")
    }
    reset = function() {
        txt <<- character()
        header <<- character()
        buf <<- NULL
        content.type <<- character()
        curHeaderStatus <<- -1
        inBody <<- FALSE
        isHTTP <- NA
        requestCount <<- requestCount + 1
    }
    val = if (missing(value)) 
        function(collapse = "", ...) {
            if (!is.null(buf)) {
                ans = as(buf, "raw")
                if (length(content.type)) 
                  attr(ans, "Content-Type") = content.type
                return(ans)
            }
            if (is.null(collapse)) 
                return(txt)
            ans = paste(txt, collapse = collapse, ...)
            ans = encode(ans)
            if (length(content.type)) 
                attr(ans, "Content-Type") = content.type
            ans
        }
    else function() {
        tmp = if (!is.null(buf)) 
            as(buf, "raw")
        else encode(txt)
        if (length(content.type)) 
            attr(tmp, "Content-Type") = content.type
        value(tmp)
    }
    encode = function(str) {
        mapUnicodeEscapes(str)
    }
    ans = list(update = update, value = val, reset = reset, header = function() header, 
        curl = function() curl)
    class(ans) <- c("DynamicRCurlTextHandler", "RCurlTextHandler", 
        "RCurlCallbackFunction")
    ans$reset()
    ans
}


HTTP_VERSION_1_1 <- 2L


`.__T__clone:RCurl` <- "<environment>"

getCurlHandle <- function (..., .opts = NULL, .encoding = integer(), .defaults = getOption("RCurlOptions")) 
{
    h = .Call("R_curl_easy_init", PACKAGE = "RCurl")
    if (length(.defaults)) {
        i = match(names(.defaults), names(.opts))
        .opts[names(.defaults)[is.na(i)]] = .defaults[is.na(i)]
    }
    curlSetOpt(..., .opts = .opts, curl = h, .encoding = .encoding)
    h
}


curlUnescape <- function (urls) 
{
    .Call("R_curl_escape", as.character(urls), FALSE, PACKAGE = "RCurl")
}


SSLVERSION_LAST <- 4L


httpGET <- function (url, ..., curl = getCurlHandle()) 
{
    getURLContent(url, ..., curl = curl)
}


getCurlErrorClassNames <- function () 
{
    gsub("^CURLE_", "", names(CURLcodeValues))
}


listCurlOptions <- function () 
{
    sort(names(getCurlOptionsConstants()))
}


curlVersion <- function (id = 0) 
{
    x = .Call("R_curl_version_info", as.integer(id), PACKAGE = "RCurl")
    x$features = getBitIndicators(x$features, CurlFeatureBits)
    x
}


binaryBuffer <- function (initialSize = 5000) 
{
    a = .Call(R_curl_BinaryData_new, as.integer(initialSize))
    new("RCurlBinaryBuffer", ref = a)
}


guessMIMEType <- function (name, default = NA) 
{
    data("mimeTypeExtensions", envir = environment())
    ext = getExtension(name)
    ans = mimeTypeExtensions[tolower(ext)]
    if (any(i <- is.na(ans))) 
        ans[i] = otherMIMETypes[tolower(ext[i])]
    if (any(i <- is.na(ans))) 
        ans[i] = default
    structure(ans, names = name)
}


merge.list <- function (x, y, ...) 
{
    if (length(x) == 0) 
        return(y)
    if (length(y) == 0) 
        return(x)
    i = match(names(y), names(x))
    i = is.na(i)
    if (any(i)) 
        x[names(y)[which(i)]] = y[which(i)]
    x
}


.__C__MultiCURLHandle <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods"), 
    subhandles = structure("list", package = "methods")), .Names = c("ref", 
"subhandles"))
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("MultiCURLHandle", package = "RCurl")
    , package = "RCurl"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


curlSetOpt <- function (..., .opts = list(), curl = getCurlHandle(), .encoding = integer(), 
    .forceHeaderNames = FALSE, .isProtected = FALSE) 
{
    .opts = curlOptions(..., .opts = .opts)
    if ("httpheader" %in% names(.opts)) {
        tmp = .opts[["httpheader"]]
        if (length(names(tmp)) && (.forceHeaderNames || (length(grep("^[^[:space:]]+:", 
            gsub("(ftp|http):", "", tmp))) != length(tmp)))) 
            .opts[["httpheader"]] = paste(names(tmp), tmp, sep = ": ")
    }
    .encoding = getEncodingValue(.encoding)
    if (length(.opts) || length(.encoding)) {
        optIds = mapCurlOptNames(names(.opts))
        idx = match(names(.opts), names(optionConverters))
        if (!all(is.na(idx))) {
            i = names(.opts)[!is.na(idx)]
            .opts[i] = lapply(i, function(x) optionConverters[[x]](.opts[[x]]))
        }
        if (!is.null(curl)) {
            .isProtected = rep(.isProtected, length(.opts))
            status = .Call("R_curl_easy_setopt", curl, .opts, 
                optIds, .isProtected, as.integer(.encoding), 
                PACKAGE = "RCurl")
            return(curl)
        }
    }
    else optIds = integer()
    tmp = list(ids = optIds, values = .opts)
    class(tmp) <- "ResolvedCURLOptions"
    tmp
}


httpPOST <- function (url, ..., curl = getCurlHandle()) 
{
    getURLContent(url, .opts = list(customrequest = "POST", ...), 
        curl = curl, post = 1L)
}


`.__T__&:base` <- "<environment>"

base64Encode <- function (txt, mode = "character") 
base64(txt, TRUE, mode)


getCurlOptionTypes <- function (opts = getCurlOptionsConstants()) 
{
    typeName = c("integer/logical", "string/pointer", "function", 
        "large number")
    type = floor(opts/10000)
    structure(typeName[type + 1], names = names(opts))
}


AUTH_ANYSAFE <- new("CURLAuth"
    , .Data = -18L
    , names = "CURLAUTH_ANYSAFE"
)


AUTH_ANY <- new("CURLAuth"
    , .Data = -17L
    , names = "CURLAUTH_ANY"
)


basicTextGatherer <- function (txt = character(), max = NA, value = NULL, .mapUnicode = TRUE) 
{
    update = function(str) {
        txt <<- c(txt, str)
        if (!is.na(max) && nchar(txt) >= max) 
            return(0)
        nchar(str, "bytes")
    }
    reset = function() {
        txt <<- character()
    }
    val = if (missing(value)) 
        function(collapse = "", ...) {
            if (!is.null(collapse)) 
                txt = paste(txt, collapse = collapse)
            if (.mapUnicode) 
                txt = mapUnicodeEscapes(txt)
            return(txt)
        }
    else function(collapse = "") {
        if (!is.null(collapse)) 
            txt = paste(txt, collapse = collapse)
        if (.mapUnicode) 
            txt = mapUnicodeEscapes(txt)
        value(txt)
    }
    ans = list(update = update, value = val, reset = reset)
    class(ans) <- c("RCurlTextHandler", "RCurlCallbackFunction")
    ans$reset()
    ans
}


.__C__CFILE <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("CFILE", package = "RCurl")
    , package = "RCurl"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


curlPerform <- function (..., .opts = list(), curl = getCurlHandle(), .encoding = integer()) 
{
    isProtected = missing(curl)
    .encoding = getEncodingValue(.encoding)
    .opts = curlSetOpt(..., .opts = .opts, curl = NULL, .encoding = .encoding)
    status = .Call("R_curl_easy_perform", curl, .opts, isProtected, 
        .encoding, PACKAGE = "RCurl")
    asCurlErrorCode(status)
}


`.__T__complete:RCurl` <- "<environment>"

chunkToLineReader <- function (f, verbose = FALSE) 
{
    leftOvers = ""
    read = function(txt) {
        if (nzchar(leftOvers)) {
            txt = paste(leftOvers, txt, sep = "")
            leftOvers <<- ""
        }
        leftOvers <<- gsub("^.*\n", "", txt)
        if (verbose && nchar(leftOvers)) 
            cat("Left over:", leftOvers, "\n")
        Lines = strsplit(txt, "\n")[[1]]
        if (nchar(leftOvers)) 
            Lines = Lines[-length(Lines)]
        if (length(Lines)) 
            f(Lines)
        nchar(txt)
    }
    list(read = read, pending = function() leftOvers)
}


close <- function (con, ...) 
standardGeneric("close")


SSLVERSION_DEFAULT <- 0L


clone <- function (x, ...) 
standardGeneric("clone")


complete <- function (obj, ...) 
standardGeneric("complete")


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

curlGlobalInit <- function (flags = c("ssl", "win32")) 
{
    status = .Call("R_curl_global_init", setBitIndicators(flags, 
        CurlGlobalBits), PACKAGE = "RCurl")
    asCurlErrorCode(status)
}


`.__T__reset:RCurl` <- "<environment>"

base64 <- function (txt, encode = !inherits(txt, "base64"), mode = "character") 
{
    asRaw = (as.character(mode) == "raw")
    encode
    if (typeof(txt) != "raw") 
        txt = as.character(txt)
    if (encode) {
        ans = .Call(R_base64_encode, txt, asRaw)
        class(ans) <- "base64"
        ans
    }
    else .Call(R_base64_decode, txt, asRaw)
}


`.__T__coerce:methods` <- "<environment>"

getCurlOptionsConstants <- function () 
{
    x = .Call("R_getCURLOptionEnum", PACKAGE = "RCurl")
    names(x) = gsub("_", ".", tolower(names(x)))
    x
}


getURI <- function (url, ..., .opts = list(), write = basicTextGatherer(.mapUnicode = .mapUnicode), 
    curl = getCurlHandle(), async = length(url) > 1, .encoding = integer(), 
    .mapUnicode = TRUE) 
{
    url = as.character(url)
    if (async) {
        if (missing(write)) 
            write = multiTextGatherer(url)
        return(getURIAsynchronous(url, ..., .opts = .opts, write = write, 
            curl = curl, .encoding = .encoding))
    }
    if (length(url) > 1) {
        dupWriter = FALSE
        if (missing(write)) 
            dupWriter = TRUE
        return(sapply(url, function(u) {
            if (dupWriter) write = basicTextGatherer()
            getURI(u, ..., .opts = .opts, write = write, curl = curl, 
                async = FALSE, .encoding = .encoding)
        }))
    }
    returnWriter = FALSE
    if (missing(write) || inherits(write, "RCurlCallbackFunction")) {
        writeFun = write$update
    }
    else {
        writeFun = write
        returnWriter = TRUE
    }
    opts = curlOptions(URL = url, writefunction = writeFun, ..., 
        .opts = .opts)
    status = curlPerform(curl = curl, .opts = opts, .encoding = .encoding)
    if (returnWriter) 
        return(write)
    write$value()
}


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

getURLAsynchronous <- function (url, ..., .opts = list(), write = NULL, curl = getCurlHandle(), 
    multiHandle = getCurlMultiHandle(), perform = Inf, .encoding = integer(), 
    binary = rep(NA, length(url))) 
{
    writeSupplied = !missing(write)
    if (!is.null(write) && !is.list(write)) 
        write = replicate(length(url), write)
    if (length(url) > 1) 
        curl = lapply(url, function(id) dupCurlHandle(curl))
    else curl = list(curl)
    if (length(.encoding)) 
        .encoding = rep(.encoding, length = length(url))
    if (is.null(write)) {
        writeSupplied = FALSE
        write = mapply(function(curl, url, binary) {
            force(url)
            force(curl)
            force(binary)
            dynCurlReader(curl, baseURL = url, binary = binary)
        }, curl, url, binary, SIMPLIFY = FALSE)
    }
    for (i in seq(along = url)) {
        w = write[[i]]
        if (inherits(w, "DynamicRCurlTextHandler")) {
            .opts[["headerfunction"]] = w$update
            .opts[["writefunction"]] = w$update
        }
        else if (inherits(w, "RCurlCallbackFunction")) 
            .opts[["writefunction"]] = w$update
        else .opts[["writefunction"]] = w
        opts = curlOptions(URL = url[i], ..., .opts = .opts)
        curlSetOpt(.opts = opts, curl = curl[[i]], .encoding = if (length(.encoding)) 
            .encoding[i]
        else integer())
        multiHandle = push(multiHandle, curl[[i]])
    }
    if (perform > 0) {
        ctr = 0
        while (TRUE) {
            status = curlMultiPerform(multiHandle)
            ctr <- ctr + 1
            if (status[2] == 0 || ctr > perform) 
                break
        }
        if (status[2] == 0 && (!writeSupplied || inherits(write, 
            "MultiTextGatherer"))) 
            return(sapply(write, function(w) w$value()))
    }
    list(multiHandle = multiHandle, write = write)
}


getURL <- function (url, ..., .opts = list(), write = basicTextGatherer(.mapUnicode = .mapUnicode), 
    curl = getCurlHandle(), async = length(url) > 1, .encoding = integer(), 
    .mapUnicode = TRUE) 
{
    url = as.character(url)
    if (async) {
        if (missing(write)) 
            write = multiTextGatherer(url)
        return(getURIAsynchronous(url, ..., .opts = .opts, write = write, 
            curl = curl, .encoding = .encoding))
    }
    if (length(url) > 1) {
        dupWriter = FALSE
        if (missing(write)) 
            dupWriter = TRUE
        return(sapply(url, function(u) {
            if (dupWriter) write = basicTextGatherer()
            getURI(u, ..., .opts = .opts, write = write, curl = curl, 
                async = FALSE, .encoding = .encoding)
        }))
    }
    returnWriter = FALSE
    if (missing(write) || inherits(write, "RCurlCallbackFunction")) {
        writeFun = write$update
    }
    else {
        writeFun = write
        returnWriter = TRUE
    }
    opts = curlOptions(URL = url, writefunction = writeFun, ..., 
        .opts = .opts)
    status = curlPerform(curl = curl, .opts = opts, .encoding = .encoding)
    if (returnWriter) 
        return(write)
    write$value()
}


getBinaryURL <- function (url, ..., .opts = list(), curl = getCurlHandle(), .buf = binaryBuffer(.len), 
    .len = 5000) 
{
    getURL(url, ..., write = getNativeSymbolInfo("R_curl_write_binary_data")$address, 
        file = .buf@ref, curl = curl, .opts = .opts)
    as(.buf, "raw")
}


getBitIndicators <- function (val, defs) 
{
    n = length(defs)
    ans = .C("R_check_bits", as.integer(val), as.integer(defs), 
        ans = logical(n), n, PACKAGE = "RCurl")$ans
    defs[ans]
}


fileUpload <- function (filename = character(), contents = character(), contentType = character()) 
{
    if (length(contents) == 0 && !file.exists(filename)) 
        stop("specified file does not exist: ", filename, ".  You must specify a valid file name or provide the contents to send.")
    filename = path.expand(filename)
    if (!typeof(contents) == "raw") 
        contents = as.character(contents)
    structure(list(filename = as.character(filename), contents = contents, 
        contentType = as.character(contentType)), class = "FileUploadInfo")
}


HTTP_VERSION_LAST <- 3L


AUTH_GSSNEGOTIATE <- new("CURLAuth"
    , .Data = 4L
    , names = "CURLAUTH_GSSNEGOTIATE"
)


`.__T__c:base` <- "<environment>"

`.__T__close:base` <- "<environment>"

httpOPTIONS <- function (url, ..., curl = getCurlHandle()) 
{
    ans = getURLContent(url, customrequest = "OPTIONS", ..., 
        curl = curl, header = TRUE)
    ans$header
}


url.exists <- function (url, ..., .opts = list(...), curl = getCurlHandle(.opts = .opts), 
    .header = FALSE) 
{
    g = basicTextGatherer()
    failed = FALSE
    ans = tryCatch(curlPerform(url = url, followlocation = TRUE, 
        headerfunction = g$update, nobody = TRUE, writefunction = g$update, 
        curl = curl), COULDNT_RESOLVE_HOST = function(x) failed <<- TRUE, 
        error = function(x) failed <<- TRUE)
    if (failed) 
        return(FALSE)
    if (grepl("^ftp", url)) {
        return(TRUE)
    }
    else header = parseHTTPHeader(g$value())
    if (.header) 
        header
    else as.integer(as.integer(header["status"])/100) == 2
}


getCurlInfo <- function (curl, which = getCurlInfoConstants()) 
{
    rnames = character()
    if (is.character(which)) {
        const = getCurlInfoConstants()
        i = pmatch(tolower(which), names(const))
        if (any(is.na(i))) 
            stop("Invalid curl info name", names(which)[is.na(i)])
        which = getCurlInfoConstants()[i]
    }
    x = .Call("R_curl_easy_getinfo", curl, as.integer(which), 
        PACKAGE = "RCurl")
    names(x) = names(which)
    x
}


postForm <- function (uri, ..., .params = list(), .opts = curlOptions(url = uri), 
    curl = getCurlHandle(), style = "HTTPPOST", .encoding = integer(), 
    binary = NA, .checkParams = TRUE, .contentEncodeFun = curlEscape) 
{
    isProtected = missing(curl)
    write = NULL
    noCurlOptions = missing(.opts)
    style = matchPostStyle(style)
    .params = merge(list(...), .params)
    hasOwnWrite = any(c("writefunction", "headerfunction") %in% 
        names(.opts))
    buf = NULL
    header = basicTextGatherer()
    if (!hasOwnWrite) {
        write = dynCurlReader(curl, verbose = FALSE, binary = binary, 
            baseURL = uri, encoding = .encoding)
        .opts[["headerfunction"]] = write$update
        on.exit(cleanupDynReader(NULL, curl))
        isProtected = rep(isProtected, length(.opts))
        isProtected[length(isProtected)] = TRUE
    }
    optionNames = names(.opts)
    .opts = curlSetOpt(url = uri, .opts = .opts, curl = NULL, 
        .encoding = .encoding)
    if (!is.na(style) && style == PostStyles["POST"]) {
        tmp = as.list(.params)
        .params = paste(unlist(mapply(function(id, val) paste(id, 
            if (is(val, "AsIs")) 
                val
            else .contentEncodeFun(val), sep = "="), names(tmp), 
            tmp)), collapse = "&")
    }
    else .params = lapply(.params, function(x) if (is.atomic(x)) 
        as.character(x)
    else x)
    if (length(.params) == 0) {
        postfields = getCurlOptionsConstants()["postfields"]
        if (!(postfields %in% .opts$ids)) 
            warning("No inputs passed to form")
    }
    else if (noCurlOptions && .checkParams) 
        testCurlOptionsInFormParameters(.params)
    status = .postForm(curl, .opts, .params, style)
    if (any(!isProtected)) {
        curlSetOpt(httpget = TRUE, curl = curl)
    }
    if (!is.null(write)) {
        http.header = parseHTTPHeader(write$header())
        stop.if.HTTP.error(http.header)
    }
    if (!is.null(buf)) {
        processContent(as(buf, "raw"), header, .encoding)
    }
    else if (!is.null(write)) 
        write$value()
}


coerce <- methods::coerce # re-exported from methods package

.postForm <- function (curl, .opts, .params, style = "HTTPPOST") 
{
    .Call("R_post_form", curl, .opts, .params, TRUE, matchPostStyle(style), 
        PACKAGE = "RCurl")
}


curlMultiPerform <- function (curl, multiple = TRUE) 
{
    status = .Call("R_curlMultiPerform", curl, as.logical(multiple), 
        PACKAGE = "RCurl")
    names(status) = c("status", "numHandlesRemaining")
    status
}




## Package Data

mimeTypeExtensions <- RCurl::mimeTypeExtensions		## Mapping from extension to MIME type



## Package Info

.skeleton_package_title = "General Network (HTTP/FTP/...) Client Interface for R"

.skeleton_package_version = "1.95-4.8"

.skeleton_package_depends = "methods,bitops"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF