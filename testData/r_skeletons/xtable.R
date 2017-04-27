##
## Exported symobls in package `xtable`
##

## Exported package methods

sanitize.final <- function (str, type) 
{
    if (type == "latex") {
        return(str)
    }
    else {
        str$text <- gsub("  *", " ", str$text, fixed = TRUE)
        str$text <- gsub(" align=\"left\"", "", str$text, fixed = TRUE)
        return(str)
    }
}


xtableLSMeans <- function (x, caption = NULL, label = NULL, align = NULL, digits = NULL, 
    display = NULL, auto = FALSE, ...) 
{
    if (attr(x, "estName") == "lsmean") {
        xList <- split(x, f = x[, 2])
        for (i in 1:length(xList)) {
            xList[[i]] <- as.data.frame(xList[[i]][, -2])
        }
        attr(xList, "subheadings") <- paste0(dimnames(x)[[2]][2], 
            " = ", levels(x[[2]]))
        attr(xList, "message") <- c("", attr(x, "mesg"))
        xList <- xtableList(xList, caption = caption, label = label, 
            align = align, digits = digits, display = display, 
            auto = auto, ...)
    }
    else {
        xList <- x
        xList <- xtable.data.frame(xList, caption = caption, 
            label = label, align = align, digits = digits, display = display, 
            auto = auto, ...)
    }
    return(xList)
}


toLatex.xtable <- function (object, ...) 
{
    dotArgs <- list(...)
    dotArgs$x <- object
    dotArgs$type <- "latex"
    dotArgs$print.results <- FALSE
    z <- do.call("print.xtable", dotArgs)
    z <- strsplit(z, split = "\n")[[1]]
    class(z) <- "Latex"
    z
}


print.xtableFtable <- function (x, type = getOption("xtable.type", "latex"), file = getOption("xtable.file", 
    ""), append = getOption("xtable.append", FALSE), floating = getOption("xtable.floating", 
    TRUE), floating.environment = getOption("xtable.floating.environment", 
    "table"), table.placement = getOption("xtable.table.placement", 
    "ht"), caption.placement = getOption("xtable.caption.placement", 
    "bottom"), caption.width = getOption("xtable.caption.width", 
    NULL), latex.environments = getOption("xtable.latex.environments", 
    c("center")), tabular.environment = getOption("xtable.tabular.environment", 
    "tabular"), size = getOption("xtable.size", NULL), hline.after = getOption("xtable.hline.after", 
    NULL), NA.string = getOption("xtable.NA.string", ""), only.contents = getOption("xtable.only.contents", 
    FALSE), add.to.row = getOption("xtable.add.to.row", NULL), 
    sanitize.text.function = getOption("xtable.sanitize.text.function", 
        as.is), sanitize.rownames.function = getOption("xtable.sanitize.rownames.function", 
        sanitize.text.function), sanitize.colnames.function = getOption("xtable.sanitize.colnames.function", 
        sanitize.text.function), math.style.negative = getOption("xtable.math.style.negative", 
        FALSE), math.style.exponents = getOption("xtable.math.style.exponents", 
        FALSE), html.table.attributes = getOption("xtable.html.table.attributes", 
        "border=1"), print.results = getOption("xtable.print.results", 
        TRUE), format.args = getOption("xtable.format.args", 
        NULL), rotate.rownames = getOption("xtable.rotate.rownames", 
        FALSE), rotate.colnames = getOption("xtable.rotate.colnames", 
        FALSE), booktabs = getOption("xtable.booktabs", FALSE), 
    scalebox = getOption("xtable.scalebox", NULL), width = getOption("xtable.width", 
        NULL), comment = getOption("xtable.comment", TRUE), timestamp = getOption("xtable.timestamp", 
        date()), ...) 
{
    if (type == "latex") {
        caption <- attr(x, "ftableCaption")
        label <- attr(x, "ftableLabel")
        align <- attr(x, "ftableAlign")
        digits <- attr(x, "ftableDigits")
        quote <- attr(x, "quote")
        digits <- attr(x, "ftabelDigits")
        method <- attr(x, "method")
        lsep <- attr(x, "lsep")
        nCharRows <- attr(x, "nChars")[1]
        nCharCols <- attr(x, "nChars")[2]
        nRowVars <- length(attr(x, "row.vars"))
        nColVars <- length(attr(x, "col.vars"))
        class(x) <- "ftable"
        fmtFtbl <- format(x, quote = quote, digits = digits, 
            method = method, lsep = lsep)
        attr(fmtFtbl, "caption") <- caption
        attr(fmtFtbl, "label") <- label
        if (is.null(sanitize.rownames.function)) {
            fmtFtbl[nCharRows, 1:nRowVars] <- sanitize(fmtFtbl[nCharRows, 
                1:nRowVars], type = type)
        }
        else {
            fmtFtbl[nCharRows, 1:nRowVars] <- sanitize.rownames.function(fmtFtbl[nCharRows, 
                1:nRowVars])
        }
        if (is.null(sanitize.colnames.function)) {
            fmtFtbl[1:nColVars, nCharCols - 1] <- sanitize(fmtFtbl[1:nColVars, 
                nCharCols - 1], type = type)
        }
        else {
            fmtFtbl[1:nColVars, nCharCols - 1] <- sanitize.colnames.function(fmtFtbl[1:nColVars, 
                nCharCols - 1])
        }
        if (rotate.rownames) {
            fmtFtbl[1:dim(fmtFtbl)[1], 1:(nCharCols - 1)] <- paste0("\\begin{sideways} ", 
                fmtFtbl[1:dim(fmtFtbl)[1], 1:(nCharCols - 1)], 
                "\\end{sideways}")
        }
        if (rotate.colnames) {
            if (rotate.rownames) {
                fmtFtbl[1:(nCharRows), (nCharCols):dim(fmtFtbl)[2]] <- paste0("\\begin{sideways} ", 
                  fmtFtbl[1:(nCharRows), (nCharCols):dim(fmtFtbl)[2]], 
                  "\\end{sideways}")
            }
            else {
                fmtFtbl[1:(nCharRows), 1:dim(fmtFtbl)[2]] <- paste0("\\begin{sideways} ", 
                  fmtFtbl[1:(nCharRows), 1:dim(fmtFtbl)[2]], 
                  "\\end{sideways}")
            }
        }
        if (booktabs) 
            align <- gsub("|", "", align, fixed = TRUE)
        attr(fmtFtbl, "align") <- align
        attr(fmtFtbl, "digits") <- digits
        attr(fmtFtbl, "quote") <- quote
        attr(fmtFtbl, "display") <- display
        for (i in 1:nCharRows) {
            fmtFtbl[i, nCharCols:dim(fmtFtbl)[2]] <- paste0("\\multicolumn{1}{l}{ ", 
                fmtFtbl[i, nCharCols:dim(fmtFtbl)[2]], "}")
        }
        if (is.null(hline.after)) {
            hline.after <- c(-1, nCharRows, dim(fmtFtbl)[1])
        }
        print.xtable(fmtFtbl, hline.after = hline.after, include.rownames = FALSE, 
            include.colnames = FALSE, booktabs = booktabs, sanitize.text.function = as.is)
    }
    else {
        stop("print.xtableFtable not yet implemented for this type")
    }
}


print.xtableList <- function (x, type = getOption("xtable.type", "latex"), file = getOption("xtable.file", 
    ""), append = getOption("xtable.append", FALSE), floating = getOption("xtable.floating", 
    TRUE), floating.environment = getOption("xtable.floating.environment", 
    "table"), table.placement = getOption("xtable.table.placement", 
    "ht"), caption.placement = getOption("xtable.caption.placement", 
    "bottom"), caption.width = getOption("xtable.caption.width", 
    NULL), latex.environments = getOption("xtable.latex.environments", 
    c("center")), tabular.environment = getOption("xtable.tabular.environment", 
    "tabular"), size = getOption("xtable.size", NULL), hline.after = NULL, 
    NA.string = getOption("xtable.NA.string", ""), include.rownames = getOption("xtable.include.rownames", 
        TRUE), colnames.format = "single", only.contents = getOption("xtable.only.contents", 
        FALSE), add.to.row = NULL, sanitize.text.function = getOption("xtable.sanitize.text.function", 
        NULL), sanitize.rownames.function = getOption("xtable.sanitize.rownames.function", 
        sanitize.text.function), sanitize.colnames.function = getOption("xtable.sanitize.colnames.function", 
        sanitize.text.function), sanitize.subheadings.function = getOption("xtable.sanitize.subheadings.function", 
        sanitize.text.function), sanitize.message.function = getOption("xtable.sanitize.message.function", 
        sanitize.text.function), math.style.negative = getOption("xtable.math.style.negative", 
        FALSE), math.style.exponents = getOption("xtable.math.style.exponents", 
        FALSE), html.table.attributes = getOption("xtable.html.table.attributes", 
        "border=1"), print.results = getOption("xtable.print.results", 
        TRUE), format.args = getOption("xtable.format.args", 
        NULL), rotate.rownames = getOption("xtable.rotate.rownames", 
        FALSE), rotate.colnames = getOption("xtable.rotate.colnames", 
        FALSE), booktabs = getOption("xtable.booktabs", FALSE), 
    scalebox = getOption("xtable.scalebox", NULL), width = getOption("xtable.width", 
        NULL), comment = getOption("xtable.comment", TRUE), timestamp = getOption("xtable.timestamp", 
        date()), ...) 
{
    nCols <- dim(x[[1]])[2]
    rowNums <- sapply(x, dim)[1, ]
    combinedRowNums <- cumsum(rowNums)
    combined <- do.call(rbind, x)
    if (type == "latex") {
        if (booktabs) {
            tRule <- "\\toprule"
            mRule <- "\\midrule"
            bRule <- "\\bottomrule"
        }
        else {
            tRule <- "\\hline"
            mRule <- "\\hline"
            bRule <- "\\hline"
        }
        if (!is.null(sanitize.subheadings.function)) {
            for (i in 1:length(x)) {
                attr(x[[i]], "subheading") <- sanitize.subheadings.function(attr(x[[i]], 
                  "subheading"))
            }
        }
        if (!is.null(sanitize.message.function)) {
            xMessage <- attr(x, "message")
            xMessage <- sapply(xMessage, sanitize.message.function)
            attr(x, "message") <- xMessage
        }
        if (colnames.format == "single") {
            add.to.row <- list(pos = NULL, command = NULL)
            add.to.row$pos <- as.list(c(0, combinedRowNums[-length(x)], 
                dim(combined)[1]))
            command <- sapply(x, attr, "subheading")
            for (i in 1:length(x)) {
                if (!is.null(command[[i]])) {
                  add.to.row$command[i] <- paste0(mRule, "\n\\multicolumn{", 
                    nCols, "}{l}{", command[[i]], "}\\\\\n")
                }
                else {
                  add.to.row$command[i] <- paste0(mRule, "\n")
                }
            }
            if ((booktabs) & length(attr(x, "message") > 0)) {
                attr(x, "message")[1] <- paste0("\\rule{0em}{2.5ex}", 
                  attr(x, "message")[1])
            }
            add.to.row$command[length(x) + 1] <- paste0("\n\\multicolumn{", 
                nCols, "}{l}{", attr(x, "message"), "}\\\\\n", 
                collapse = "")
            add.to.row$command[length(x) + 1] <- paste0(bRule, 
                add.to.row$command[length(x) + 1])
            class(combined) <- c("xtableList", "data.frame")
            hline.after <- c(-1)
            include.colnames <- TRUE
        }
        if (colnames.format == "multiple") {
            if (is.null(sanitize.colnames.function)) {
                colHead <- names(x[[1]])
            }
            else {
                colHead <- sanitize.colnames.function(names(x[[1]]))
            }
            if (rotate.colnames) {
                colHead <- paste("\\begin{sideways}", colHead, 
                  "\\end{sideways}")
            }
            colHead <- paste0(colHead, collapse = " & ")
            if (include.rownames) {
                colHead <- paste0(" & ", colHead)
            }
            colHead <- paste0(tRule, "\n", colHead, " \\\\", 
                mRule, "\n")
            add.to.row <- list(pos = NULL, command = NULL)
            add.to.row$pos <- as.list(c(0, c(combinedRowNums[1:length(x)])))
            command <- sapply(x, attr, "subheading")
            add.to.row$command[1] <- if (!is.null(command[[1]])) {
                add.to.row$command[1] <- paste0("\n\\multicolumn{", 
                  nCols, "}{l}{", command[[1]], "}\\\\ \n", colHead, 
                  "\n")
            }
            else {
                add.to.row$command[1] <- colHead
            }
            for (i in 2:length(x)) {
                add.to.row$command[i] <- if (!is.null(command[[i]])) {
                  paste0(bRule, "\\\\ \n\\multicolumn{", nCols, 
                    "}{l}{", command[[i]], "}", "\\\\ \n", colHead)
                }
                else {
                  add.to.row$command[i] <- paste0("\n", colHead)
                }
            }
            if ((booktabs) & length(attr(x, "message") > 0)) {
                attr(x, "message")[1] <- paste0("\\rule{0em}{2.5ex}", 
                  attr(x, "message")[1])
            }
            add.to.row$command[length(x) + 1] <- paste0("\n\\multicolumn{", 
                nCols, "}{l}{", attr(x, "message"), "}\\\\\n", 
                collapse = "")
            add.to.row$command[length(x) + 1] <- paste0(bRule, 
                add.to.row$command[length(x) + 1])
            class(combined) <- c("xtableList", "data.frame")
            hline.after <- NULL
            include.colnames <- FALSE
        }
        print.xtable(combined, type = type, floating = floating, 
            floating.environment = floating.environment, table.placement = table.placement, 
            caption.placement = caption.placement, caption.width = caption.width, 
            latex.environments = latex.environments, tabular.environment = tabular.environment, 
            size = size, hline.after = hline.after, NA.string = NA.string, 
            include.rownames = include.rownames, include.colnames = include.colnames, 
            only.contents = only.contents, add.to.row = add.to.row, 
            sanitize.text.function = sanitize.text.function, 
            sanitize.rownames.function = sanitize.rownames.function, 
            sanitize.colnames.function = sanitize.colnames.function, 
            math.style.negative = math.style.negative, math.style.exponents = math.style.exponents, 
            html.table.attributes = html.table.attributes, print.results = print.results, 
            format.args = format.args, rotate.rownames = rotate.rownames, 
            rotate.colnames = rotate.colnames, booktabs = booktabs, 
            scalebox = scalebox, width = width, comment = comment, 
            timestamp = timestamp, ...)
    }
    else {
        stop("print.xtableList not yet implemented for this type")
    }
}


xdisplay <- function (x, pad = TRUE) 
{
    type <- function(v) {
        if (is.numeric(v)) {
            tp <- if (xdigits(v) == 0) 
                "d"
            else "f"
        }
        else {
            tp <- "s"
        }
        return(tp)
    }
    is.2d <- length(dim(x)) == 2
    disp <- if (is.2d) 
        sapply(as.data.frame(x), type)
    else type(x)
    output <- if (is.2d && pad) 
        c("s", disp)
    else disp
    return(output)
}


`caption<-` <- function (x, value) 
UseMethod("caption<-")


xtable <- function (x, caption = NULL, label = NULL, align = NULL, digits = NULL, 
    display = NULL, auto = FALSE, ...) 
{
    UseMethod("xtable")
}


as.math <- function (str, ...) 
{
    paste0("$", str, "$", ...)
}


caption <- function (x, ...) 
UseMethod("caption")


display <- function (x, ...) 
UseMethod("display")


`label<-` <- function (x, value) 
UseMethod("label<-")


sanitize <- function (str, type = "latex") 
{
    if (type == "latex") {
        result <- str
        result <- gsub("\\\\", "SANITIZE.BACKSLASH", result)
        result <- gsub("$", "\\$", result, fixed = TRUE)
        result <- gsub(">", "$>$", result, fixed = TRUE)
        result <- gsub("<", "$<$", result, fixed = TRUE)
        result <- gsub("|", "$|$", result, fixed = TRUE)
        result <- gsub("{", "\\{", result, fixed = TRUE)
        result <- gsub("}", "\\}", result, fixed = TRUE)
        result <- gsub("%", "\\%", result, fixed = TRUE)
        result <- gsub("&", "\\&", result, fixed = TRUE)
        result <- gsub("_", "\\_", result, fixed = TRUE)
        result <- gsub("#", "\\#", result, fixed = TRUE)
        result <- gsub("^", "\\verb|^|", result, fixed = TRUE)
        result <- gsub("~", "\\~{}", result, fixed = TRUE)
        result <- gsub("SANITIZE.BACKSLASH", "$\\backslash$", 
            result, fixed = TRUE)
        return(result)
    }
    else {
        result <- str
        result <- gsub("&", "&amp;", result, fixed = TRUE)
        result <- gsub(">", "&gt;", result, fixed = TRUE)
        result <- gsub("<", "&lt;", result, fixed = TRUE)
        return(result)
    }
}


print.xtableMatharray <- function (x, print.results = TRUE, format.args = getOption("xtable.format.args", 
    NULL), scalebox = getOption("xtable.scalebox", NULL), comment = FALSE, 
    timestamp = NULL, ...) 
{
    class(x) <- c("xtableMatharray", "data.frame")
    print.xtable(x, floating = FALSE, tabular.environment = "array", 
        include.rownames = FALSE, include.colnames = FALSE, hline.after = NULL, 
        print.results = print.results, format.args = format.args, 
        scalebox = scalebox, comment = comment, timestamp = timestamp, 
        ...)
}


xtableMatharray <- function (x, caption = NULL, label = NULL, align = NULL, digits = NULL, 
    display = NULL, auto = FALSE, ...) 
{
    class(x) <- c("xtableMatharray", "matrix")
    xtbl <- xtable.matrix(x, caption = caption, label = label, 
        align = align, digits = digits, display = display, auto = auto, 
        ...)
    class(xtbl) <- c("xtableMatharray", "xtable", "data.frame")
    return(xtbl)
}


xdigits <- function (x, pad = TRUE, zap = getOption("digits")) 
{
    dig <- function(v) {
        if (is.numeric(v)) {
            v <- na.omit(v)
            v <- zapsmall(abs(v - floor(v)), zap)
            dec <- if (any(v > 0)) 
                max(nchar(v) - 2L)
            else 0L
        }
        else {
            dec <- 0L
        }
        return(dec)
    }
    is.2d <- length(dim(x)) == 2
    decimals <- if (is.2d) 
        sapply(as.data.frame(x), dig)
    else dig(x)
    output <- if (is.2d && pad) 
        c(0L, decimals)
    else decimals
    return(output)
}


`align<-` <- function (x, value) 
UseMethod("align<-")


label <- function (x, ...) 
UseMethod("label")


align <- function (x, ...) 
UseMethod("align")


sanitize.numbers <- function (str, type, math.style.negative = FALSE, math.style.exponents = FALSE) 
{
    if (type == "latex") {
        result <- str
        if (math.style.negative) {
            for (i in 1:length(str)) {
                result[i] <- gsub("-", "$-$", result[i], fixed = TRUE)
            }
        }
        if (math.style.exponents) {
            if (is.logical(math.style.exponents) && !math.style.exponents) {
            }
            else if (is.logical(math.style.exponents) && math.style.exponents || 
                math.style.exponents == "$$") {
                for (i in 1:length(str)) {
                  result[i] <- gsub("^\\$?(-?)\\$?([0-9.]+)[eE]\\$?(-?)\\+?\\$?0*(\\d+)$", 
                    "$\\1\\2 \\\\times 10^{\\3\\4}$", result[i])
                }
            }
            else if (math.style.exponents == "ensuremath") {
                for (i in 1:length(str)) {
                  result[i] <- gsub("^\\$?(-?)\\$?([0-9.]+)[eE]\\$?(-?)\\+?\\$?0*(\\d+)$", 
                    "\\\\ensuremath{\\1\\2 \\\\times 10^{\\3\\4}}", 
                    result[i])
                }
            }
            else if (math.style.exponents == "UTF8" || math.style.exponents == 
                "UTF-8") {
                for (i in 1:length(str)) {
                  if (all(grepl("^\\$?(-?)\\$?([0-9.]+)[eE]\\$?(-?)\\+?\\$?0*(\\d+)$", 
                    result[i]))) {
                    temp <- strsplit(result[i], "eE", result[i])
                    result[i] <- paste0(temp[1], "×10", chartr("-1234567890", 
                      "⁻¹²³⁴⁵⁴6⁴7⁴8⁴9⁰", temp[2]))
                  }
                }
            }
        }
        return(result)
    }
    else {
        return(str)
    }
}


xtableFtable <- function (x, caption = NULL, label = NULL, align = NULL, digits = 0, 
    display = NULL, quote = FALSE, method = c("non.compact", 
        "row.compact", "col.compact", "compact"), lsep = " $\\vert$ ", 
    ...) 
{
    method <- match.arg(method)
    saveMethod <- method
    xDim <- dim(x)
    nRowVars <- length(attr(x, "row.vars"))
    nColVars <- length(attr(x, "col.vars"))
    if (nRowVars == 0) {
        if (method == "col.compact") {
            method <- "non.compact"
        }
        else if (method == "compact") {
            method <- "row.compact"
        }
    }
    if (nColVars == 0) {
        if (method == "row.compact") {
            method <- "non.compact"
        }
        else if (method == "compact") {
            method <- "col.compact"
        }
    }
    if (method == "non.compact") {
        nCharCols <- nRowVars + 2
        nCharRows <- nColVars + 1
    }
    if (method == "row.compact") {
        nCharCols <- nRowVars + 2
        nCharRows <- nColVars
    }
    if (method == "col.compact") {
        nCharCols <- nRowVars + 1
        nCharRows <- nColVars + 1
    }
    if (method == "compact") {
        nCharCols <- nRowVars + 1
        nCharRows <- nColVars
    }
    if (is.null(align)) {
        align <- c(rep("l", nCharCols - 1), "l |", rep("r", xDim[2]))
    }
    if (is.null(display)) {
        display <- c(rep("s", nCharCols), rep("d", xDim[2]))
    }
    attr(x, "ftableCaption") <- caption
    attr(x, "ftableLabel") <- label
    attr(x, "ftableAlign") <- align
    attr(x, "ftableDigits") <- digits
    attr(x, "quote") <- quote
    attr(x, "ftableDisplay") <- display
    attr(x, "method") <- method
    attr(x, "lsep") <- lsep
    attr(x, "nChars") <- c(nCharRows, nCharCols)
    class(x) <- c("xtableFtable", "ftable")
    return(x)
}


as.is <- function (str) 
{
    str
}


print.xtable <- function (x, type = getOption("xtable.type", "latex"), file = getOption("xtable.file", 
    ""), append = getOption("xtable.append", FALSE), floating = getOption("xtable.floating", 
    TRUE), floating.environment = getOption("xtable.floating.environment", 
    "table"), table.placement = getOption("xtable.table.placement", 
    "ht"), caption.placement = getOption("xtable.caption.placement", 
    "bottom"), caption.width = getOption("xtable.caption.width", 
    NULL), latex.environments = getOption("xtable.latex.environments", 
    c("center")), tabular.environment = getOption("xtable.tabular.environment", 
    "tabular"), size = getOption("xtable.size", NULL), hline.after = getOption("xtable.hline.after", 
    c(-1, 0, nrow(x))), NA.string = getOption("xtable.NA.string", 
    ""), include.rownames = getOption("xtable.include.rownames", 
    TRUE), include.colnames = getOption("xtable.include.colnames", 
    TRUE), only.contents = getOption("xtable.only.contents", 
    FALSE), add.to.row = getOption("xtable.add.to.row", NULL), 
    sanitize.text.function = getOption("xtable.sanitize.text.function", 
        NULL), sanitize.rownames.function = getOption("xtable.sanitize.rownames.function", 
        sanitize.text.function), sanitize.colnames.function = getOption("xtable.sanitize.colnames.function", 
        sanitize.text.function), math.style.negative = getOption("xtable.math.style.negative", 
        FALSE), math.style.exponents = getOption("xtable.math.style.exponents", 
        FALSE), html.table.attributes = getOption("xtable.html.table.attributes", 
        "border=1"), print.results = getOption("xtable.print.results", 
        TRUE), format.args = getOption("xtable.format.args", 
        NULL), rotate.rownames = getOption("xtable.rotate.rownames", 
        FALSE), rotate.colnames = getOption("xtable.rotate.colnames", 
        FALSE), booktabs = getOption("xtable.booktabs", FALSE), 
    scalebox = getOption("xtable.scalebox", NULL), width = getOption("xtable.width", 
        NULL), comment = getOption("xtable.comment", TRUE), timestamp = getOption("xtable.timestamp", 
        date()), ...) 
{
    caption <- attr(x, "caption", exact = TRUE)
    short.caption <- NULL
    if (!is.null(caption) && length(caption) > 1) {
        short.caption <- caption[2]
        caption <- caption[1]
    }
    pos <- 0
    if (include.rownames) 
        pos <- 1
    if (any(hline.after < -1) | any(hline.after > nrow(x))) {
        stop("'hline.after' must be inside [-1, nrow(x)]")
    }
    if (!is.null(add.to.row)) {
        if (is.list(add.to.row) && length(add.to.row) == 2) {
            if (is.null(names(add.to.row))) {
                names(add.to.row) <- c("pos", "command")
            }
            else if (any(sort(names(add.to.row)) != c("command", 
                "pos"))) {
                stop("the names of the elements of 'add.to.row' must be 'pos' and 'command'")
            }
            if (is.list(add.to.row$pos) && is.vector(add.to.row$command, 
                mode = "character")) {
                if ((npos <- length(add.to.row$pos)) != length(add.to.row$command)) {
                  stop("the length of 'add.to.row$pos' must be equal to the length of 'add.to.row$command'")
                }
                if (any(unlist(add.to.row$pos) < -1) | any(unlist(add.to.row$pos) > 
                  nrow(x))) {
                  stop("the values in add.to.row$pos must be inside the interval [-1, nrow(x)]")
                }
            }
            else {
                stop("the first argument ('pos') of 'add.to.row' must be a list, the second argument ('command') must be a vector of mode character")
            }
        }
        else {
            stop("'add.to.row' argument must be a list of length 2")
        }
    }
    else {
        add.to.row <- list(pos = list(), command = vector(length = 0, 
            mode = "character"))
        npos <- 0
    }
    if (type == "latex") {
        if (!booktabs) {
            PHEADER <- "\\hline\n"
        }
        else {
            if (is.null(hline.after)) {
                PHEADER <- ""
            }
            else {
                hline.after <- sort(hline.after)
                PHEADER <- rep("\\midrule\n", length(hline.after))
                if (hline.after[1] == -1) {
                  PHEADER[1] <- "\\toprule\n"
                }
                if (hline.after[length(hline.after)] == nrow(x)) {
                  PHEADER[length(hline.after)] <- "\\bottomrule\n"
                }
            }
        }
    }
    else {
        PHEADER <- ""
    }
    lastcol <- rep(" ", nrow(x) + 2)
    if (!is.null(hline.after)) {
        if (!booktabs) {
            add.to.row$pos[[npos + 1]] <- hline.after
        }
        else {
            for (i in 1:length(hline.after)) {
                add.to.row$pos[[npos + i]] <- hline.after[i]
            }
        }
        add.to.row$command <- c(add.to.row$command, PHEADER)
    }
    if (length(add.to.row$command) > 0) {
        for (i in 1:length(add.to.row$command)) {
            addpos <- add.to.row$pos[[i]]
            freq <- table(addpos)
            addpos <- unique(addpos)
            for (j in 1:length(addpos)) {
                lastcol[addpos[j] + 2] <- paste(lastcol[addpos[j] + 
                  2], paste(rep(add.to.row$command[i], freq[j]), 
                  sep = "", collapse = ""), sep = " ")
            }
        }
    }
    if (length(type) > 1) 
        stop("\"type\" must have length 1")
    type <- tolower(type)
    if (!all(!is.na(match(type, c("latex", "html"))))) {
        stop("\"type\" must be in {\"latex\", \"html\"}")
    }
    if (("margintable" %in% floating.environment) & (!is.null(table.placement))) {
        warning("margintable does not allow for table placement; setting table.placement to NULL")
        table.placement <- NULL
    }
    if (!is.null(table.placement) && !all(!is.na(match(unlist(strsplit(table.placement, 
        split = "")), c("H", "h", "t", "b", "p", "!"))))) {
        stop("\"table.placement\" must contain only elements of {\"h\",\"t\",\"b\",\"p\",\"!\"}")
    }
    if (!all(!is.na(match(caption.placement, c("bottom", "top"))))) {
        stop("\"caption.placement\" must be either {\"bottom\",\"top\"}")
    }
    if (type == "latex") {
        BCOMMENT <- "% "
        ECOMMENT <- "\n"
        if (tabular.environment == "longtable" & floating == 
            TRUE) {
            warning("Attempt to use \"longtable\" with floating = TRUE. Changing to FALSE.")
            floating <- FALSE
        }
        if (floating == TRUE) {
            BTABLE <- paste("\\begin{", floating.environment, 
                "}", ifelse(!is.null(table.placement), paste("[", 
                  table.placement, "]", sep = ""), ""), "\n", 
                sep = "")
            if (is.null(latex.environments) || (length(latex.environments) == 
                0)) {
                BENVIRONMENT <- ""
                EENVIRONMENT <- ""
            }
            else {
                BENVIRONMENT <- ""
                EENVIRONMENT <- ""
                if ("center" %in% latex.environments) {
                  BENVIRONMENT <- paste(BENVIRONMENT, "\\centering\n", 
                    sep = "")
                }
                for (i in 1:length(latex.environments)) {
                  if (latex.environments[i] == "") 
                    next
                  if (latex.environments[i] != "center") {
                    BENVIRONMENT <- paste(BENVIRONMENT, "\\begin{", 
                      latex.environments[i], "}\n", sep = "")
                    EENVIRONMENT <- paste("\\end{", latex.environments[i], 
                      "}\n", EENVIRONMENT, sep = "")
                  }
                }
            }
            ETABLE <- paste("\\end{", floating.environment, "}\n", 
                sep = "")
        }
        else {
            BTABLE <- ""
            ETABLE <- ""
            BENVIRONMENT <- ""
            EENVIRONMENT <- ""
        }
        tmp.index.start <- 1
        if (!include.rownames) {
            while (attr(x, "align", exact = TRUE)[tmp.index.start] == 
                "|") tmp.index.start <- tmp.index.start + 1
            tmp.index.start <- tmp.index.start + 1
        }
        if (is.null(width)) {
            WIDTH <- ""
        }
        else if (is.element(tabular.environment, c("tabular", 
            "longtable"))) {
            warning("Ignoring 'width' argument.  The 'tabular' and 'longtable' environments do not support a width specification.  Use another environment such as 'tabular*' or 'tabularx' to specify the width.")
            WIDTH <- ""
        }
        else {
            WIDTH <- paste("{", width, "}", sep = "")
        }
        BTABULAR <- paste("\\begin{", tabular.environment, "}", 
            WIDTH, "{", paste(c(attr(x, "align", exact = TRUE)[tmp.index.start:length(attr(x, 
                "align", exact = TRUE))], "}\n"), sep = "", collapse = ""), 
            sep = "")
        if (tabular.environment == "longtable" && caption.placement == 
            "top") {
            if (is.null(short.caption)) {
                BCAPTION <- "\\caption{"
            }
            else {
                BCAPTION <- paste("\\caption[", short.caption, 
                  "]{", sep = "")
            }
            ECAPTION <- "} \\\\ \n"
            if ((!is.null(caption)) && (type == "latex")) {
                BTABULAR <- paste(BTABULAR, BCAPTION, caption, 
                  ECAPTION, sep = "")
            }
        }
        BTABULAR <- paste(BTABULAR, lastcol[1], sep = "")
        ETABULAR <- paste("\\end{", tabular.environment, "}\n", 
            sep = "")
        if (!is.null(scalebox)) {
            BTABULAR <- paste("\\scalebox{", scalebox, "}{\n", 
                BTABULAR, sep = "")
            ETABULAR <- paste(ETABULAR, "}\n", sep = "")
        }
        if (is.null(size) || !is.character(size)) {
            BSIZE <- ""
            ESIZE <- ""
        }
        else {
            if (length(grep("^\\\\", size)) == 0) {
                size <- paste("\\", size, sep = "")
            }
            BSIZE <- paste("\\begingroup", size, "\n", sep = "")
            ESIZE <- "\\endgroup\n"
        }
        BLABEL <- "\\label{"
        ELABEL <- "}\n"
        if (!is.null(caption.width)) {
            BCAPTION <- paste("\\parbox{", caption.width, "}{", 
                sep = "")
            ECAPTION <- "}"
        }
        else {
            BCAPTION <- NULL
            ECAPTION <- NULL
        }
        if (is.null(short.caption)) {
            BCAPTION <- paste(BCAPTION, "\\caption{", sep = "")
        }
        else {
            BCAPTION <- paste(BCAPTION, "\\caption[", short.caption, 
                "]{", sep = "")
        }
        ECAPTION <- paste(ECAPTION, "} \n", sep = "")
        BROW <- ""
        EROW <- " \\\\ \n"
        BTH <- ""
        ETH <- ""
        STH <- " & "
        BTD1 <- " & "
        BTD2 <- ""
        BTD3 <- ""
        ETD <- ""
    }
    else {
        BCOMMENT <- "<!-- "
        ECOMMENT <- " -->\n"
        BTABLE <- paste("<table ", html.table.attributes, ">\n", 
            sep = "")
        ETABLE <- "</table>\n"
        BENVIRONMENT <- ""
        EENVIRONMENT <- ""
        BTABULAR <- ""
        ETABULAR <- ""
        BSIZE <- ""
        ESIZE <- ""
        BLABEL <- "<a name="
        ELABEL <- "></a>\n"
        BCAPTION <- paste("<caption align=\"", caption.placement, 
            "\"> ", sep = "")
        ECAPTION <- " </caption>\n"
        BROW <- "<tr>"
        EROW <- " </tr>\n"
        BTH <- " <th> "
        ETH <- " </th> "
        STH <- " </th> <th> "
        BTD1 <- " <td align=\""
        align.tmp <- attr(x, "align", exact = TRUE)
        align.tmp <- align.tmp[align.tmp != "|"]
        if (nrow(x) == 0) {
            BTD2 <- matrix(nrow = 0, ncol = ncol(x) + pos)
        }
        else {
            BTD2 <- matrix(align.tmp[(2 - pos):(ncol(x) + 1)], 
                nrow = nrow(x), ncol = ncol(x) + pos, byrow = TRUE)
        }
        BTD2[regexpr("^p", BTD2) > 0] <- "left"
        BTD2[BTD2 == "r"] <- "right"
        BTD2[BTD2 == "l"] <- "left"
        BTD2[BTD2 == "c"] <- "center"
        BTD3 <- "\"> "
        ETD <- " </td>"
    }
    result <- string("", file = file, append = append)
    info <- R.Version()
    if (comment) {
        result <- result + BCOMMENT + type + " table generated in " + 
            info$language + " " + info$major + "." + info$minor + 
            " by xtable " + packageDescription("xtable")$Version + 
            " package" + ECOMMENT
        if (!is.null(timestamp)) {
            result <- result + BCOMMENT + timestamp + ECOMMENT
        }
    }
    if (!only.contents) {
        result <- result + BTABLE
        result <- result + BENVIRONMENT
        if (floating == TRUE) {
            if ((!is.null(caption)) && (type == "html" || caption.placement == 
                "top")) {
                result <- result + BCAPTION + caption + ECAPTION
            }
            if (!is.null(attr(x, "label", exact = TRUE)) && (type == 
                "latex" && caption.placement == "top")) {
                result <- result + BLABEL + attr(x, "label", 
                  exact = TRUE) + ELABEL
            }
        }
        result <- result + BSIZE
        result <- result + BTABULAR
    }
    if (include.colnames) {
        result <- result + BROW + BTH
        if (include.rownames) {
            result <- result + STH
        }
        if (is.null(sanitize.colnames.function)) {
            CNAMES <- sanitize(names(x), type = type)
        }
        else {
            CNAMES <- sanitize.colnames.function(names(x))
        }
        if (rotate.colnames) {
            CNAMES <- paste("\\begin{sideways}", CNAMES, "\\end{sideways}")
        }
        result <- result + paste(CNAMES, collapse = STH)
        result <- result + ETH + EROW
    }
    cols <- matrix("", nrow = nrow(x), ncol = ncol(x) + pos)
    if (include.rownames) {
        if (is.null(sanitize.rownames.function)) {
            RNAMES <- sanitize(row.names(x), type = type)
        }
        else {
            RNAMES <- sanitize.rownames.function(row.names(x))
        }
        if (rotate.rownames) {
            RNAMES <- paste("\\begin{sideways}", RNAMES, "\\end{sideways}")
        }
        cols[, 1] <- RNAMES
    }
    varying.digits <- is.matrix(attr(x, "digits", exact = TRUE))
    for (i in 1:ncol(x)) {
        xcol <- x[, i]
        if (is.factor(xcol)) 
            xcol <- as.character(xcol)
        if (is.list(xcol)) 
            xcol <- sapply(xcol, unlist)
        ina <- is.na(xcol)
        is.numeric.column <- is.numeric(xcol)
        if (is.character(xcol)) {
            cols[, i + pos] <- xcol
        }
        else {
            if (is.null(format.args)) {
                format.args <- list()
            }
            if (is.null(format.args$decimal.mark)) {
                format.args$decimal.mark <- options()$OutDec
            }
            if (!varying.digits) {
                curFormatArgs <- c(list(x = xcol, format = ifelse(attr(x, 
                  "digits", exact = TRUE)[i + 1] < 0, "E", attr(x, 
                  "display", exact = TRUE)[i + 1]), digits = abs(attr(x, 
                  "digits", exact = TRUE)[i + 1])), format.args)
                cols[, i + pos] <- do.call("formatC", curFormatArgs)
            }
            else {
                for (j in 1:nrow(cols)) {
                  curFormatArgs <- c(list(x = xcol[j], format = ifelse(attr(x, 
                    "digits", exact = TRUE)[j, i + 1] < 0, "E", 
                    attr(x, "display", exact = TRUE)[i + 1]), 
                    digits = abs(attr(x, "digits", exact = TRUE)[j, 
                      i + 1])), format.args)
                  cols[j, i + pos] <- do.call("formatC", curFormatArgs)
                }
            }
        }
        if (any(ina)) 
            cols[ina, i + pos] <- NA.string
        if (is.numeric.column) {
            cols[, i + pos] <- sanitize.numbers(cols[, i + pos], 
                type = type, math.style.negative = math.style.negative, 
                math.style.exponents = math.style.exponents)
        }
        else {
            if (is.null(sanitize.text.function)) {
                cols[, i + pos] <- sanitize(cols[, i + pos], 
                  type = type)
            }
            else {
                cols[, i + pos] <- sanitize.text.function(cols[, 
                  i + pos])
            }
        }
    }
    multiplier <- 5
    full <- matrix("", nrow = nrow(x), ncol = multiplier * (ncol(x) + 
        pos) + 2)
    full[, 1] <- BROW
    full[, multiplier * (0:(ncol(x) + pos - 1)) + 2] <- BTD1
    full[, multiplier * (0:(ncol(x) + pos - 1)) + 3] <- BTD2
    full[, multiplier * (0:(ncol(x) + pos - 1)) + 4] <- BTD3
    full[, multiplier * (0:(ncol(x) + pos - 1)) + 5] <- cols
    full[, multiplier * (0:(ncol(x) + pos - 1)) + 6] <- ETD
    full[, multiplier * (ncol(x) + pos) + 2] <- paste(EROW, lastcol[-(1:2)], 
        sep = " ")
    if (type == "latex") 
        full[, 2] <- ""
    result <- result + lastcol[2] + paste(t(full), collapse = "")
    if (!only.contents) {
        if (tabular.environment == "longtable") {
            if (!booktabs) {
                result <- result + PHEADER
            }
            if (caption.placement == "bottom") {
                if ((!is.null(caption)) && (type == "latex")) {
                  result <- result + BCAPTION + caption + ECAPTION
                }
            }
            if (!is.null(attr(x, "label", exact = TRUE))) {
                result <- result + BLABEL + attr(x, "label", 
                  exact = TRUE) + ELABEL
            }
            ETABULAR <- "\\end{longtable}\n"
        }
        result <- result + ETABULAR
        result <- result + ESIZE
        if (floating == TRUE) {
            if ((!is.null(caption)) && (type == "latex" && caption.placement == 
                "bottom")) {
                result <- result + BCAPTION + caption + ECAPTION
            }
            if (!is.null(attr(x, "label", exact = TRUE)) && caption.placement == 
                "bottom") {
                result <- result + BLABEL + attr(x, "label", 
                  exact = TRUE) + ELABEL
            }
        }
        result <- result + EENVIRONMENT
        result <- result + ETABLE
    }
    result <- sanitize.final(result, type = type)
    if (print.results) {
        print(result)
    }
    return(invisible(result$text))
}


digits <- function (x, ...) 
UseMethod("digits")


autoformat <- function (xtab, zap = getOption("digits")) 
{
    align(xtab) <- xalign(xtab)
    digits(xtab) <- xdigits(xtab, zap = zap)
    display(xtab) <- xdisplay(xtab)
    return(xtab)
}


xtableList <- function (x, caption = NULL, label = NULL, align = NULL, digits = NULL, 
    display = NULL, ...) 
{
    if (is.null(digits)) {
        digitsList <- vector("list", length(x))
    }
    else {
        if (!is.list(digits)) {
            digitsList <- vector("list", length(x))
            for (i in 1:length(x)) digitsList[[i]] <- digits
        }
    }
    if (is.null(display)) {
        displayList <- vector("list", length(x))
    }
    else {
        if (!is.list(display)) {
            displayList <- vector("list", length(x))
            for (i in 1:length(x)) displayList[[i]] <- display
        }
    }
    xList <- vector("list", length(x))
    for (i in 1:length(x)) {
        xList[[i]] <- xtable(x[[i]], caption = caption, label = label, 
            align = align, digits = digitsList[[i]], display = displayList[[i]], 
            ...)
        attr(xList[[i]], "subheading") <- attr(x, "subheadings")[[i]]
    }
    attr(xList, "message") <- attr(x, "message")
    attr(xList, "caption") <- caption
    attr(xList, "label") <- label
    class(xList) <- c("xtableList")
    return(xList)
}


`display<-` <- function (x, value) 
UseMethod("display<-")


xalign <- function (x, pad = TRUE) 
{
    lr <- function(v) if (is.numeric(v)) 
        "r"
    else "l"
    is.2d <- length(dim(x)) == 2
    alignment <- if (is.2d) 
        sapply(as.data.frame(x), lr)
    else lr(x)
    output <- if (is.2d && pad) 
        c("l", alignment)
    else alignment
    return(output)
}


`digits<-` <- function (x, value) 
UseMethod("digits<-")




## Package Data

tli <- xtable::tli		## Math scores from Texas Assessment of Academic Skills (TAAS)



## Package Info

.skeleton_package_title = "Export Tables to LaTeX or HTML"

.skeleton_package_version = "1.8-2"

.skeleton_package_depends = ""

.skeleton_package_imports = "stats,utils"


## Internal

.skeleton_version = 5


## EOF