##
## Exported symobls in package `httpuv`
##

## Exported package methods

.__C__WebSocket <- new("refClassRepresentation"
    , fieldClasses = structure(list(.handle = "ANY", .messageCallbacks = "list", .closeCallbacks = "list", 
    request = "environment"), .Names = c(".handle", ".messageCallbacks", 
".closeCallbacks", "request"))
    , fieldPrototypes =  "<environment>"
    , refMethods =  "<environment>"
    , refSuperClasses = "envRefClass"
    , slots = structure(list(.xData = structure("environment", package = "methods")), .Names = ".xData")
    , contains = structure(list(envRefClass = S4_object(), 
    .environment = S4_object(), 
    refClass = S4_object(), 
    environment = S4_object(), 
    refObject = S4_object()), .Names = c("envRefClass", 
".environment", "refClass", "environment", "refObject"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("WebSocket", package = "httpuv")
    , package = "httpuv"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


runServer <- function (host, port, app, interruptIntervalMs = ifelse(interactive(), 
    100, 1000)) 
{
    server <- startServer(host, port, app)
    on.exit(stopServer(server))
    .globals$stopped <- FALSE
    while (!.globals$stopped) {
        service(interruptIntervalMs)
        Sys.sleep(0.001)
    }
}


stopServer <- function (handle) 
{
    destroyServer(handle)
}


`.__T__$:base` <- Rcpp::`.__T__$:base` # re-exported from Rcpp package

`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

startServer <- function (host, port, app) 
{
    appWrapper <- AppWrapper$new(app)
    server <- makeTcpServer(host, port, appWrapper$onHeaders, 
        appWrapper$onBodyData, appWrapper$call, appWrapper$onWSOpen, 
        appWrapper$onWSMessage, appWrapper$onWSClose)
    if (is.null(server)) {
        stop("Failed to create server")
    }
    return(server)
}


startPipeServer <- function (name, mask, app) 
{
    appWrapper <- AppWrapper$new(app)
    if (is.null(mask)) 
        mask <- -1
    server <- makePipeServer(name, mask, appWrapper$onHeaders, 
        appWrapper$onBodyData, appWrapper$call, appWrapper$onWSOpen, 
        appWrapper$onWSMessage, appWrapper$onWSClose)
    if (is.null(server)) {
        stop("Failed to create server")
    }
    return(server)
}


decodeURIComponent <- function (value) 
{
    .Call("httpuv_decodeURIComponent", PACKAGE = "httpuv", value)
}


service <- function (timeoutMs = ifelse(interactive(), 100, 1000)) 
{
    run(timeoutMs)
}


interrupt <- function () 
{
    stopLoop()
    .globals$stopped <- TRUE
}


stopDaemonizedServer <- function (server) 
{
    destroyDaemonizedServer(server)
}


rawToBase64 <- function (x) 
{
    base64encode(x)
}


encodeURI <- function (value) 
{
    .Call("httpuv_encodeURI", PACKAGE = "httpuv", value)
}


encodeURIComponent <- function (value) 
{
    .Call("httpuv_encodeURIComponent", PACKAGE = "httpuv", value)
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

startDaemonizedServer <- function (host, port, app) 
{
    server <- startServer(host, port, app)
    tryCatch({
        server <- daemonize(server)
    }, error = function(e) {
        stopServer(server)
        stop(e)
    })
    return(server)
}


decodeURI <- function (value) 
{
    .Call("httpuv_decodeURI", PACKAGE = "httpuv", value)
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package



## Package Data

# none


## Package Info

.skeleton_package_title = "HTTP and WebSocket Server Library"

.skeleton_package_version = "1.3.3"

.skeleton_package_depends = "methods"

.skeleton_package_imports = "Rcpp,utils"


## Internal

.skeleton_version = 5


## EOF