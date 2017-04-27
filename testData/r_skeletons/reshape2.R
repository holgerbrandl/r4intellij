##
## Exported symobls in package `reshape2`
##

## Exported package methods

recast <- function (data, formula, ..., id.var, measure.var) 
{
    if (any(c("id.vars", "measure.vars") %in% names(match.call()))) {
        stop("Use var, not vars\n")
    }
    molten <- melt(data, id.var, measure.var)
    dcast(molten, formula, ...)
}


acast <- function (data, formula, fun.aggregate = NULL, ..., margins = NULL, 
    subset = NULL, fill = NULL, drop = TRUE, value.var = guess_value(data)) 
{
    formula <- parse_formula(formula, names(data), value.var)
    if (!is.null(margins)) {
        data <- add_margins(data, lapply(formula, names), margins)
    }
    res <- cast(data, formula, fun.aggregate, ..., subset = subset, 
        fill = fill, drop = drop, value.var = value.var)
    dimnames(res$data) <- lapply(res$labels, array_names)
    res$data
}


melt <- function (data, ..., na.rm = FALSE, value.name = "value") 
{
    UseMethod("melt", data)
}


add_margins <- function (df, vars, margins = TRUE) 
{
    margin_vars <- margins(vars, margins)
    if (length(margin_vars) == 0) 
        return(df)
    addAll <- function(x) {
        x <- addNA(x, TRUE)
        factor(x, levels = c(levels(x), "(all)"), exclude = NULL)
    }
    vars <- unique(unlist(margin_vars))
    df[vars] <- lapply(df[vars], addAll)
    rownames(df) <- NULL
    margin_dfs <- llply(margin_vars, function(vars) {
        df[vars] <- rep(list(factor("(all)")), length(vars))
        df
    })
    rbind.fill(margin_dfs)
}


colsplit <- function (string, pattern, names) 
{
    vars <- str_split_fixed(string, pattern, n = length(names))
    df <- data.frame(alply(vars, 2, type.convert, as.is = TRUE), 
        stringsAsFactors = FALSE)
    names(df) <- names
    df
}


dcast <- function (data, formula, fun.aggregate = NULL, ..., margins = NULL, 
    subset = NULL, fill = NULL, drop = TRUE, value.var = guess_value(data)) 
{
    formula <- parse_formula(formula, names(data), value.var)
    if (length(formula) > 2) {
        stop("Dataframes have at most two output dimensions")
    }
    if (!is.null(margins)) {
        data <- add_margins(data, lapply(formula, names), margins)
    }
    res <- cast(data, formula, fun.aggregate, ..., subset = subset, 
        fill = fill, drop = drop, value.var = value.var)
    data <- as.data.frame.matrix(res$data, stringsAsFactors = FALSE)
    names(data) <- array_names(res$labels[[2]])
    stopifnot(nrow(res$labels[[1]]) == nrow(data))
    cbind(res$labels[[1]], data)
}




## Package Data

french_fries <- reshape2::french_fries		## Sensory data from a french fries experiment.

smiths <- reshape2::smiths		## Demo data describing the Smiths.

tips <- reshape2::tips		## Tipping data



## Package Info

.skeleton_package_title = "Flexibly Reshape Data: A Reboot of the Reshape Package"

.skeleton_package_version = "1.4.2"

.skeleton_package_depends = ""

.skeleton_package_imports = "plyr,stringr,Rcpp"


## Internal

.skeleton_version = 5


## EOF