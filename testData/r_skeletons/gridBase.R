##
## Exported symobls in package `gridBase`
##

## Exported package methods

gridPLT <- function () 
{
    cvp <- currentViewportLoc()
    din <- par("din")
    omi <- par("omi")
    fig <- par("fig")
    if (badFIG(cvp, fig, omi, din)) 
        stop("Figure region too small and/or viewport too large")
    innerwidth <- din[1] - omi[2] - omi[4]
    innerheight <- din[2] - omi[1] - omi[3]
    width <- innerwidth * (fig[2] - fig[1])
    height <- innerheight * (fig[4] - fig[3])
    left <- omi[2] + innerwidth * fig[1]
    bottom <- omi[1] + innerheight * fig[3]
    plt <- round(c((cvp$left - left)/width, (cvp$right - left)/width, 
        (cvp$bottom - bottom)/height, (cvp$top - bottom)/height), 
        digits = 4)
    plt
}


gridFIG <- function () 
{
    cvp <- currentViewportLoc()
    din <- par("din")
    omi <- par("omi")
    if (badOMI(cvp, omi, din)) 
        stop("Outer margins too large and/or viewport too large")
    width <- din[1] - omi[2] - omi[4]
    height <- din[2] - omi[1] - omi[3]
    fig <- round(c((cvp$left - omi[1])/width, (cvp$right - omi[1])/width, 
        (cvp$bottom - omi[2])/height, (cvp$top - omi[2])/height), 
        digits = 4)
    fig
}


gridPAR <- function () 
{
    gpars <- get.gpar()
    gpars <- list(col = gpars$col, lwd = gpars$lwd, lty = gpars$lty)
}


baseViewports <- function () 
{
    omi <- par("omi")
    innervp <- viewport(x = unit(omi[2], "inches"), y = unit(omi[1], 
        "inches"), width = unit(1, "npc") - unit(omi[1], "inches") - 
        unit(omi[3], "inches"), height = unit(1, "npc") - unit(omi[2], 
        "inches") - unit(omi[4], "inches"), just = c("left", 
        "bottom"))
    fig <- par("fig")
    figurevp <- viewport(x = unit(fig[1], "npc"), y = unit(fig[3], 
        "npc"), width = unit(fig[2] - fig[1], "npc"), height = unit(fig[4] - 
        fig[3], "npc"), just = c("left", "bottom"))
    plt <- par("plt")
    usr <- par("usr")
    logscale <- FALSE
    if (par("xlog") || par("ylog")) {
        warning("viewport scales NOT set to user coordinates")
        logscales <- TRUE
    }
    plotvp <- viewport(x = unit(plt[1], "npc"), y = unit(plt[3], 
        "npc"), width = unit(plt[2] - plt[1], "npc"), height = unit(plt[4] - 
        plt[3], "npc"), just = c("left", "bottom"), xscale = c(usr[1], 
        usr[2]), yscale = c(usr[3], usr[4]))
    list(inner = innervp, figure = figurevp, plot = plotvp)
}


gridOMI <- function () 
{
    cvp <- currentViewportLoc()
    din <- par("din")
    omi <- round(c(cvp$bottom, cvp$left, din[2] - cvp$top, din[1] - 
        cvp$right), digits = 4)
    omi
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Integration of base and grid graphics"

.skeleton_package_version = "0.4-7"

.skeleton_package_depends = ""

.skeleton_package_imports = "graphics,grid"


## Internal

.skeleton_version = 5


## EOF