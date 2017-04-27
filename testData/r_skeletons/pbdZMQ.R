##
## Exported symobls in package `pbdZMQ`
##

## Exported package methods

ZMQ.MC <- function (warning.at.error = TRUE, stop.at.error = FALSE) 
{
    list(warning.at.error = warning.at.error, stop.at.error = stop.at.error)
}


zmq.poll.get.revents <- function (index = 1L) 
{
    if (index < 1) {
        stop("index is a positive interger.")
    }
    ret <- .Call("R_zmq_poll_get_revents", as.integer(index - 
        1)[1], PACKAGE = "pbdZMQ")
    invisible(ret)
}


address <- function (host, port, transport = "tcp") 
{
    transports <- c("tcp", "inproc", "ipc", "pgm", "epgm")
    transport <- tolower(transport)
    match.arg(transport, transports)
    if (transport == "ipc") {
        if (!missing(port)) 
            warning("Ignoring specified port for ipc transport.")
        port <- ""
    }
    paste0(transport, "://", host, ":", port)
}


zmq.socket <- function (ctx, type = .pbd_env$ZMQ.ST$REP) 
{
    ret <- .Call("R_zmq_socket", ctx, type, PACKAGE = "pbdZMQ")
    ret
}


.zmqopt_set <- function (val, main, sub = NULL, envir = .GlobalEnv) 
{
    if (!is.null(sub)) {
        envir$.pbd_env[[main]][[sub]] <- val
    }
    else {
        envir$.pbd_env[[main]] <- val
    }
    invisible()
}


random_port <- function (min_port = 49152, max_port = 65536) 
{
    if (min_port < 49152) 
        warning("non-recommended 'min_port' value; see ?random_port for details")
    if (max_port > 65536) 
        warning("non-recommended 'max_port' value; see ?random_port for details")
    as.integer(runif(1, min_port, max_port + 1))
}


init.socket <- function (context, socket.type) 
{
    try.zmqt.close <- function(socket) {
        invisible(suppressWarnings(zmq.close(socket)))
    }
    socket.type <- sub(".*_", "", socket.type)
    id <- which(names(.pbd_env$ZMQ.ST) == socket.type)
    if (length(id) != 1) {
        stop("socket.type is not found.")
    }
    else {
        socket.type <- .pbd_env$ZMQ.ST[[id]]
    }
    socket <- zmq.socket(context, type = socket.type)
    reg.finalizer(socket, try.zmqt.close, onexit = TRUE)
    socket
}


.zmqopt_init <- function (envir = .GlobalEnv) 
{
    if (!exists(".pbd_env", envir = envir)) {
        envir$.pbd_env <- new.env()
    }
    envir$.pbd_env$ZMQ.MC <- ZMQ.MC()
    envir$.pbd_env$ZMQ.SR <- ZMQ.SR()
    envir$.pbd_env$ZMQ.SO <- ZMQ.SO()
    envir$.pbd_env$ZMQ.ST <- ZMQ.ST()
    envir$.pbd_env$ZMQ.PO <- ZMQ.PO()
    invisible()
}


random_open_port <- function (min_port = 49152, max_port = 65536, max_tries = 100) 
{
    ctxt <- init.context()
    socket <- init.socket(ctxt, "ZMQ_REP")
    ret <- -1
    for (i in 1:max_tries) {
        port <- random_port(min_port = min_port, max_port = max_port)
        addr <- paste0("tcp://*:", port)
        catch <- tryCatch(zmq.bind(socket, addr), error = identity, 
            warning = identity)
        if (!inherits(catch, "error") && !inherits(catch, "warning")) {
            ret <- port
            break
        }
    }
    rm(socket)
    rm(ctxt)
    invisible(gc())
    if (ret == -1) 
        stop("No valid port could be found.")
    else return(ret)
}


zmq.poll <- function (socket, type, timeout = -1L, MC = .pbd_env$ZMQ.MC) 
{
    if (length(socket) != length(type)) {
        stop("socket and type are of different length.")
    }
    type <- as.integer(type)
    if (!all(type %in% 1:7)) {
        stop("type should be integers in 1 to 7.")
    }
    zmq.poll.free()
    ret <- .Call("R_zmq_poll", socket, type, as.integer(timeout), 
        PACKAGE = "pbdZMQ")
    return(invisible(ret))
}


zmq.recv <- function (socket, len = 1024L, flags = .pbd_env$ZMQ.SR$BLOCK, 
    buf.type = c("char", "raw")) 
{
    if (buf.type[1] == "char") {
        ret <- zmq.recv.char(socket, len, flags = flags)
    }
    else if (buf.type[1] == "raw") {
        ret <- zmq.recv.raw(socket, len, flags = flags)
    }
    else {
        stop("buf type should be char or raw.")
    }
    invisible(ret)
}


zmq.poll.length <- function () 
{
    ret <- .Call("R_zmq_poll_length", PACKAGE = "pbdZMQ")
    invisible(ret)
}


zmq <- "<environment>"

zmq.bind <- function (socket, endpoint, MC = .pbd_env$ZMQ.MC) 
{
    ret <- .Call("R_zmq_bind", socket, endpoint, PACKAGE = "pbdZMQ")
    if (ret == -1) {
        if (MC$stop.at.error) {
            stop(paste("zmq.bind fails, ", endpoint, sep = ""))
            return(invisible(ret))
        }
        if (MC$warning.at.error) {
            warning(paste("zmq.bind fails, ", endpoint, sep = ""))
            return(invisible(ret))
        }
    }
    else {
        return(invisible(ret))
    }
}


init.context <- function () 
{
    try.zmq.ctx.destroy <- function(ctx) {
        invisible(suppressWarnings(zmq.ctx.destroy(ctx)))
    }
    ctx <- zmq.ctx.new()
    reg.finalizer(ctx, try.zmq.ctx.destroy, onexit = TRUE)
    ctx
}


.zmqopt_get <- function (main, sub = NULL, envir = .GlobalEnv) 
{
    if (!is.null(sub)) {
        envir$.pbd_env[[main]][[sub]]
    }
    else {
        envir$.pbd_env[[main]]
    }
}


Socket <- "<environment>"

zmq.sendfile <- function (port, filename, verbose = FALSE, flags = .pbd_env$ZMQ.SR$BLOCK) 
{
    ctx <- zmq.ctx.new()
    socket <- zmq.socket(ctx, .pbd_env$ZMQ.ST$PUSH)
    endpoint <- address("*", port)
    zmq.bind(socket, endpoint)
    filesize <- as.double(file.info(filename)$size)
    send.socket(socket, filesize)
    ret <- .Call("R_zmq_send_file", socket, filename, as.integer(verbose), 
        filesize, as.integer(flags), PACKAGE = "pbdZMQ")
    zmq.close(socket)
    zmq.ctx.destroy(ctx)
    invisible(ret)
}


zmq.poll2.get.revents <- function (index = 1L, poller) 
{
    if (index < 1) {
        stop("index is a positive interger.")
    }
    else if (index > poller$pollret[3]) {
        stop("index is too large.")
    }
    ret <- .Call("R_zmq_poll2_get_revents", as.integer(index - 
        1)[1], poller$pollitem, PACKAGE = "pbdZMQ")
    invisible(ret)
}


zmq.send <- function (socket, buf, flags = .pbd_env$ZMQ.SR$BLOCK) 
{
    if (is.character(buf)) {
        ret <- zmq.send.char(socket, buf[1], nchar(buf[1]), flags = flags)
    }
    else if (is.raw(buf)) {
        ret <- zmq.send.raw(socket, buf[1], length(buf[1]), flags = flags)
    }
    else {
        stop("buf type should be char or raw.")
    }
    invisible(ret)
}


pbd_opt <- function (..., bytext = "", envir = .GlobalEnv) 
{
    if (!exists(".pbd_env", envir = envir)) {
        envir$.pbd_env <- new.env()
    }
    arg <- list(...)
    if (length(arg) > 0) {
        names.arg <- names(arg)
        if (is.null(names.arg) || any(names.arg == "")) {
            stop("Options are all named.")
        }
        for (i.arg in 1:length(arg)) {
            envir$.pbd_env[[names.arg[i.arg]]] <- arg[[i.arg]]
        }
    }
    if (bytext != "") {
        eval(parse(text = bytext), envir = envir$.pbd_env)
    }
    invisible()
}


zmq.recvfile <- function (port, endpoint, filename, verbose = FALSE, flags = .pbd_env$ZMQ.SR$BLOCK) 
{
    ctx <- zmq.ctx.new()
    socket <- zmq.socket(ctx, .pbd_env$ZMQ.ST$PULL)
    endpoint <- address(endpoint, port)
    zmq.connect(socket, endpoint)
    filesize <- receive.socket(socket)
    ret <- .Call("R_zmq_recv_file", socket, filename, as.integer(verbose), 
        filesize, as.integer(flags), PACKAGE = "pbdZMQ")
    invisible(ret)
}


zmq.msg.recv <- function (socket, flags = .pbd_env$ZMQ.SR$BLOCK, unserialize = TRUE) 
{
    rmsg <- .Call("R_zmq_msg_recv", socket, as.integer(flags), 
        PACKAGE = "pbdZMQ")
    if (unserialize && is.raw(rmsg)) {
        rmsg <- unserialize(rmsg)
    }
    rmsg
}


ZMQ.PO <- function (POLLIN = 1L, POLLOUT = 2L, POLLERR = 4L) 
{
    list(POLLIN = POLLIN, POLLOUT = POLLOUT, POLLERR = POLLERR)
}


zmq.ctx.destroy <- function (ctx) 
{
    .Call("R_zmq_ctx_destroy", ctx, PACKAGE = "pbdZMQ")
    invisible()
}


zmq.strerror <- function (errno) 
{
    .Call("R_zmq_strerror", as.integer(errno[1]), PACKAGE = "pbdZMQ")
}


zmq.poll2 <- function (socket, type, timeout = -1L, MC = .pbd_env$ZMQ.MC) 
{
    if (length(socket) != length(type)) {
        stop("socket and type are of different length.")
    }
    type <- as.integer(type)
    if (!all(type %in% 1:7)) {
        stop("type should be integers in 1 to 7.")
    }
    ret <- .Call("R_zmq_poll2", socket, type, as.integer(timeout), 
        PACKAGE = "pbdZMQ")
    return(invisible(ret))
}


zmq.recv.multipart <- function (socket, unserialize = TRUE) 
{
    ret <- list()
    i.part <- 1
    ret[[i.part]] <- zmq.msg.recv(socket, flags = .pbd_env$ZMQ.SR$BLOCK, 
        unserialize = unserialize)
    opt.val <- zmq.getsockopt(socket, .pbd_env$ZMQ.SO$RCVMORE, 
        0L)
    while (opt.val == 1) {
        i.part <- i.part + 1
        ret[[i.part]] <- zmq.msg.recv(socket, flags = .pbd_env$ZMQ.SR$BLOCK, 
            unserialize = unserialize)
        opt.val <- zmq.getsockopt(socket, .pbd_env$ZMQ.SO$RCVMORE, 
            0L)
    }
    ret
}


zmq.poll2.interrupt <- function (socket, type, timeout = -1L, MC = .pbd_env$ZMQ.MC) 
{
    ret <- zmq.poll2(socket, type, timeout, MC)
    if (ret$pollret[1] == -1 && ret$pollret[2] == 4) {
        my.c <- structure(list(ret = ret), class = c("interrupt", 
            "condition"))
        signalCondition(my.c)
    }
    return(invisible(ret))
}


send.socket <- function (socket, data, send.more = FALSE, serialize = TRUE) 
{
    if (send.more) {
        flags <- .pbd_env$ZMQ.SR$SNDMORE
    }
    else {
        flags <- .pbd_env$ZMQ.SR$BLOCK
    }
    zmq.msg.send(data, socket, flags = flags, serialize = serialize)
}


zmq.connect <- function (socket, endpoint, MC = .pbd_env$ZMQ.MC) 
{
    ret <- .Call("R_zmq_connect", socket, endpoint, PACKAGE = "pbdZMQ")
    if (ret == -1) {
        if (MC$stop.at.error) {
            stop(paste("zmq.connect fails, ", endpoint, sep = ""))
            return(invisible(ret))
        }
        if (MC$warning.at.error) {
            warning(paste("zmq.connect fails, ", endpoint, sep = ""))
            return(invisible(ret))
        }
    }
    else {
        return(invisible(ret))
    }
}


zmq.msg.send <- function (rmsg, socket, flags = .pbd_env$ZMQ.SR$BLOCK, serialize = TRUE) 
{
    if (serialize) {
        rmsg <- serialize(rmsg, NULL)
    }
    ret <- .Call("R_zmq_msg_send", rmsg, socket, as.integer(flags), 
        PACKAGE = "pbdZMQ")
    invisible(ret)
}


bind.socket <- function (socket, address) 
{
    zmq.bind(socket, address)
}


connect.socket <- function (socket, address) 
{
    zmq.connect(socket, address)
}


Context <- "<environment>"

receive.socket <- function (socket, unserialize = TRUE, dont.wait = FALSE) 
{
    if (dont.wait) {
        flags <- .pbd_env$ZMQ.SR$DONTWAIT
    }
    else {
        flags <- .pbd_env$ZMQ.SR$BLOCK
    }
    zmq.msg.recv(socket, flags = flags, unserialize = unserialize)
}


zmq.close <- function (socket) 
{
    ret <- .Call("R_zmq_close", socket, PACKAGE = "pbdZMQ")
    invisible(ret)
}


ZMQ.SO <- function (AFFINITY = 4L, IDENTITY = 5L, SUBSCRIBE = 6L, UNSUBSCRIBE = 7L, 
    RATE = 8L, RECOVERY_IVL = 9L, SNDBUF = 11L, RCVBUF = 12L, 
    RCVMORE = 13L, FD = 14L, EVENTS = 15L, TYPE = 16L, LINGER = 17L, 
    RECONNECT_IVL = 18L, BACKLOG = 19L, RECONNECT_IVL_MAX = 21L, 
    MAXMSGSIZE = 22L, SNDHWM = 23L, RCVHWM = 24L, MULTICAST_HOPS = 25L, 
    RCVTIMEO = 27L, SNDTIMEO = 28L, LAST_ENDPOINT = 32L, ROUTER_MANDATORY = 33L, 
    TCP_KEEPALIVE = 34L, TCP_KEEPALIVE_CNT = 35L, TCP_KEEPALIVE_IDLE = 36L, 
    TCP_KEEPALIVE_INTVL = 37L, TCP_ACCEPT_FILTER = 38L, IMMEDIATE = 39L, 
    XPUB_VERBOSE = 40L, ROUTER_RAW = 41L, IPV6 = 42L, MECHANISM = 43L, 
    PLAIN_SERVER = 44L, PLAIN_USERNAME = 45L, PLAIN_PASSWORD = 46L, 
    CURVE_SERVER = 47L, CURVE_PUBLICKEY = 48L, CURVE_SECRETKEY = 49L, 
    CURVE_SERVERKEY = 50L, PROBE_ROUTER = 51L, REQ_CORRELATE = 52L, 
    REQ_RELAXED = 53L, CONFLATE = 54L, ZAP_DOMAIN = 55L, ROUTER_HANDOVER = 56L, 
    TOS = 57L, IPC_FILTER_PID = 58L, IPC_FILTER_UID = 59L, IPC_FILTER_GID = 60L, 
    CONNECT_RID = 61L, GSSAPI_SERVER = 62L, GSSAPI_PRINCIPAL = 63L, 
    GSSAPI_SERVICE_PRINCIPAL = 64L, GSSAPI_PLAINTEXT = 65L, HANDSHAKE_IVL = 66L, 
    IDENTITY_FD = 67L, SOCKS_PROXY = 68L, XPUB_NODROP = 69L) 
{
    list(AFFINITY = AFFINITY, IDENTITY = IDENTITY, SUBSCRIBE = SUBSCRIBE, 
        UNSUBSCRIBE = UNSUBSCRIBE, RATE = RATE, RECOVERY_IVL = RECOVERY_IVL, 
        SNDBUF = SNDBUF, RCVBUF = RCVBUF, RCVMORE = RCVMORE, 
        FD = FD, EVENTS = EVENTS, TYPE = TYPE, LINGER = LINGER, 
        RECONNECT_IVL = RECONNECT_IVL, BACKLOG = BACKLOG, RECONNECT_IVL_MAX = RECONNECT_IVL_MAX, 
        MAXMSGSIZE = MAXMSGSIZE, SNDHWM = SNDHWM, RCVHWM = RCVHWM, 
        MULTICAST_HOPS = MULTICAST_HOPS, RCVTIMEO = RCVTIMEO, 
        SNDTIMEO = SNDTIMEO, LAST_ENDPOINT = LAST_ENDPOINT, ROUTER_MANDATORY = ROUTER_MANDATORY, 
        TCP_KEEPALIVE = TCP_KEEPALIVE, TCP_KEEPALIVE_CNT = TCP_KEEPALIVE_CNT, 
        TCP_KEEPALIVE_IDLE = TCP_KEEPALIVE_IDLE, TCP_KEEPALIVE_INTVL = TCP_KEEPALIVE_INTVL, 
        TCP_ACCEPT_FILTER = TCP_ACCEPT_FILTER, IMMEDIATE = IMMEDIATE, 
        XPUB_VERBOSE = XPUB_VERBOSE, ROUTER_RAW = ROUTER_RAW, 
        IPV6 = IPV6, MECHANISM = MECHANISM, PLAIN_SERVER = PLAIN_SERVER, 
        PLAIN_USERNAME = PLAIN_USERNAME, PLAIN_PASSWORD = PLAIN_PASSWORD, 
        CURVE_SERVER = CURVE_SERVER, CURVE_PUBLICKEY = CURVE_PUBLICKEY, 
        CURVE_SECRETKEY = CURVE_SECRETKEY, CURVE_SERVERKEY = CURVE_SERVERKEY, 
        PROBE_ROUTER = PROBE_ROUTER, REQ_CORRELATE = REQ_CORRELATE, 
        REQ_RELAXED = REQ_RELAXED, CONFLATE = CONFLATE, ZAP_DOMAIN = ZAP_DOMAIN, 
        ROUTER_HANDOVER = ROUTER_HANDOVER, TOS = TOS, IPC_FILTER_PID = IPC_FILTER_PID, 
        IPC_FILTER_UID = IPC_FILTER_UID, IPC_FILTER_GID = IPC_FILTER_GID, 
        CONNECT_RID = CONNECT_RID, GSSAPI_SERVER = GSSAPI_SERVER, 
        GSSAPI_PRINCIPAL = GSSAPI_PRINCIPAL, GSSAPI_SERVICE_PRINCIPAL = GSSAPI_SERVICE_PRINCIPAL, 
        GSSAPI_PLAINTEXT = GSSAPI_PLAINTEXT, HANDSHAKE_IVL = HANDSHAKE_IVL, 
        IDENTITY_FD = IDENTITY_FD, SOCKS_PROXY = SOCKS_PROXY, 
        XPUB_NODROP = XPUB_NODROP)
}


zmq.ctx.new <- function () 
{
    ret <- .Call("R_zmq_ctx_new", PACKAGE = "pbdZMQ")
    ret
}


ZMQ.SR <- function (BLOCK = 0L, DONTWAIT = 1L, NOBLOCK = 1L, SNDMORE = 2L) 
{
    list(BLOCK = BLOCK, DONTWAIT = DONTWAIT, NOBLOCK = NOBLOCK, 
        SNDMORE = SNDMORE)
}


zmq.setsockopt <- function (socket, option.name, option.value, MC = .pbd_env$ZMQ.MC) 
{
    if (is.character(option.value)) {
        option.type <- 0L
    }
    else if (is.integer(option.value)) {
        option.type <- 1L
    }
    else {
        stop("Type of option.value is not implemented")
    }
    ret <- .Call("R_zmq_setsockopt", socket, option.name, option.value, 
        option.type, PACKAGE = "pbdZMQ")
    if (ret == -1) {
        if (MC$stop.at.error) {
            stop(paste("zmq.setsockopt fails, ", option.value, 
                sep = ""))
            return(invisible(ret))
        }
        if (MC$warning.at.error) {
            warning(paste("zmq.setsockopt fails, ", option.value, 
                sep = ""))
            return(invisible(ret))
        }
    }
    else {
        return(invisible(ret))
    }
}


zmq.send.multipart <- function (socket, parts, serialize = TRUE) 
{
    for (i.part in 1:(length(parts) - 1)) {
        zmq.msg.send(parts[[i.part]], socket, flags = .pbd_env$ZMQ.SR$SNDMORE, 
            serialize = serialize)
    }
    zmq.msg.send(parts[[length(parts)]], socket, flags = .pbd_env$ZMQ.SR$BLOCK, 
        serialize = serialize)
    invisible()
}


zmq.getsockopt <- function (socket, option.name, option.value, MC = .pbd_env$ZMQ.MC) 
{
    if (is.character(option.value)) {
        option.type <- 0L
    }
    else if (is.integer(option.value)) {
        option.type <- 1L
    }
    else {
        stop("Type of option.value is not implemented")
    }
    ret <- .Call("R_zmq_getsockopt", socket, option.name, option.value, 
        option.type, PACKAGE = "pbdZMQ")
    if (ret == -1) {
        if (MC$stop.at.error) {
            stop(paste("zmq.getsockopt fails, ", option.value, 
                sep = ""))
            return(invisible(ret))
        }
        if (MC$warning.at.error) {
            warning(paste("zmq.getsockopt fails, ", option.value, 
                sep = ""))
            return(invisible(ret))
        }
    }
    else {
        return(invisible(option.value))
    }
}


zmq.poll.interrupt <- function (socket, type, timeout = -1L, MC = .pbd_env$ZMQ.MC) 
{
    ret <- zmq.poll(socket, type, timeout, MC)
    if (ret[1] == -1 && ret[2] == 4) {
        my.c <- structure(list(ret = ret), class = c("interrupt", 
            "condition"))
        signalCondition(my.c)
    }
    return(invisible(ret))
}


zmq.disconnect <- function (socket, endpoint, MC = .pbd_env$ZMQ.MC) 
{
    ret <- .Call("R_zmq_disconnect", socket, endpoint, PACKAGE = "pbdZMQ")
    if (ret == -1) {
        if (MC$stop.at.error) {
            stop(paste("zmq.disconnect fails, ", endpoint, sep = ""))
            return(invisible(ret))
        }
        if (MC$warning.at.error) {
            warning(paste("zmq.disconnect fails, ", endpoint, 
                sep = ""))
            return(invisible(ret))
        }
    }
    else {
        return(invisible(ret))
    }
}


ZMQ.ST <- function (PAIR = 0L, PUB = 1L, SUB = 2L, REQ = 3L, REP = 4L, 
    DEALER = 5L, ROUTER = 6L, PULL = 7L, PUSH = 8L, XPUB = 9L, 
    XSUB = 10L, STREAM = 11L) 
{
    list(PAIR = PAIR, PUB = PUB, SUB = SUB, REQ = REQ, REP = REP, 
        DEALER = DEALER, ROUTER = ROUTER, PULL = PULL, PUSH = PUSH, 
        XPUB = XPUB, XSUB = XSUB, STREAM = STREAM)
}


zmq.poll.free <- function () 
{
    ret <- .Call("R_zmq_poll_free", PACKAGE = "pbdZMQ")
    invisible(ret)
}


zmq.version <- function () 
{
    ret <- .Call("R_zmq_version", PACKAGE = "pbdZMQ")
    package_version(ret)
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Programming with Big Data -- Interface to ZeroMQ"

.skeleton_package_version = "0.2-5"

.skeleton_package_depends = ""

.skeleton_package_imports = "R6"


## Internal

.skeleton_version = 5


## EOF