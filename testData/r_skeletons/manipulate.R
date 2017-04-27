##
## Exported symobls in package `manipulate`
##

## Exported package methods

slider <- function (min, max, initial = min, label = NULL, step = NULL, 
    ticks = TRUE) 
{
    if (!is.numeric(initial) || !is.numeric(min) || !is.numeric(max)) 
        stop("min, max, amd initial must all be numeric values")
    else if (initial < min) 
        stop(paste("slider initial value", initial, "is less than the specified minimum"))
    else if (initial > max) 
        stop(paste("slider initial value", initial, "is greater than the specified maximum"))
    else if (min > max) 
        stop(paste("slider maximum is greater than minimum"))
    else if (!is.null(step)) {
        if (!is.numeric(step)) 
            stop("step is not a numeric value")
        if (step > (max - min)) 
            stop("step is greater than range")
    }
    else if (!is.logical(ticks)) 
        stop("ticks is not a logical value")
    else if (!is.null(label) && !is.character(label)) 
        stop("label is not a character value")
    if (is.null(step)) 
        step <- -1
    slider <- list(type = 0, min = min, max = max, initialValue = initial, 
        label = label, step = step, ticks = ticks)
    class(slider) <- "manipulator.slider"
    return(slider)
}


checkbox <- function (initial = FALSE, label = NULL) 
{
    if (!is.logical(initial)) 
        stop("initial must be a logical")
    else if (!is.null(label) && !is.character(label)) 
        stop("label is not a character value")
    checkbox <- list(type = 2, initialValue = initial, label = label)
    class(checkbox) <- "manipulator.checkbox"
    return(checkbox)
}


manipulatorSetState <- function (name, value) 
{
    if (hasActiveManipulator()) {
        assign(name, value, envir = get(".state", envir = activeManipulator()))
        ensureManipulatorSaved()
        invisible(NULL)
    }
    else {
        stop("no plot manipulator currently active")
    }
}


manipulate <- function (`_expr`, ...) 
{
    manipulator <- new.env(parent = parent.frame())
    manipulator$.id <- createUUID()
    manipulator$.code <- substitute(`_expr`)
    manipulator$.codeAsText <- deparse(substitute(`_expr`), control = NULL)
    controls <- resolveVariableArguments(list(...))
    controlNames <- names(controls)
    duplicatedIndex <- anyDuplicated(controlNames)
    if (duplicatedIndex > 0) 
        stop(paste("duplicated control name:", controlNames[[duplicatedIndex]]))
    manipulator$.controls <- controls
    manipulator$.variables <- controlNames
    manipulator$.state <- new.env(parent = globalenv())
    manipulator$.userVisibleValues <- new.env(parent = globalenv())
    manipulator$.buttonNames <- character()
    for (name in names(controls)) {
        if (name == "") 
            stop("all controls passed to manipulate must be named")
        control <- controls[[name]]
        if (!(class(control) %in% c("manipulator.slider", "manipulator.picker", 
            "manipulator.checkbox", "manipulator.button"))) {
            stop(paste("argument", name, "is not a control"))
        }
        if (inherits(control, "manipulator.button")) 
            manipulator$.buttonNames <- append(manipulator$.buttonNames, 
                name)
        setManipulatorValue(manipulator, name, control$initialValue)
    }
    executeAndAttachManipulator(manipulator)
    invisible(NULL)
}


picker <- function (..., initial = NULL, label = NULL) 
{
    values <- resolveVariableArguments(list(...))
    valueNames <- names(values)
    if (is.null(valueNames)) 
        valueNames <- character(length(values))
    missingNames <- valueNames == ""
    valueNames[missingNames] <- paste(values)[missingNames]
    names(values) <- valueNames
    if (length(values) < 1) {
        stop("picker choices must contain at least one value")
    }
    else if (length(valueNames) != length(unique(valueNames))) {
        stop("picker choices must have unique names (duplicate detected)")
    }
    else if (!is.null(initial)) {
        if (length(initial) != 1) 
            stop("initial must be a single object")
        else if (!(as.character(initial) %in% valueNames)) 
            stop("initial doesn't match one of the supplied choices")
    }
    else if (!is.null(label) && !is.character(label)) {
        stop("label is not a character value")
    }
    if (is.null(initial)) 
        initial <- valueNames[1]
    picker <- list(type = 1, choices = valueNames, values = values, 
        initialValue = initial, label = label)
    class(picker) <- "manipulator.picker"
    return(picker)
}


button <- function (label) 
{
    if (!is.null(label) && !is.character(label)) 
        stop("label is not a character value")
    button <- list(type = 3, initialValue = FALSE, label = label)
    class(button) <- "manipulator.button"
    return(button)
}


manipulatorMouseClick <- function () 
{
    if (hasActiveManipulator()) {
        if (!exists(".mouseClick", envir = activeManipulator())) 
            assign(".mouseClick", NULL, envir = activeManipulator())
        get(".mouseClick", envir = activeManipulator())
    }
    else {
        stop("no plot manipulator currently active")
    }
}


manipulatorGetState <- function (name) 
{
    if (hasActiveManipulator()) {
        value <- NULL
        try(silent = TRUE, value <- get(name, envir = get(".state", 
            envir = activeManipulator())))
        return(value)
    }
    else {
        stop("no plot manipulator currently active")
    }
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Interactive Plots for RStudio"

.skeleton_package_version = "0.98.501"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF