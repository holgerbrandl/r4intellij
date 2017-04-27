##
## Exported symobls in package `treemap`
##

## Exported package methods

itreemap <- function (dtf = NULL, index = NULL, vSize = NULL, vColor = NULL, 
    type = NULL, height = 700, command.line.output = TRUE) 
{
    obs <- ls(envir = .GlobalEnv)
    if (!length(obs)) 
        stop("No data.frames loaded")
    dfs <- obs[sapply(obs, function(x) inherits(get(x, envir = .GlobalEnv), 
        "data.frame"))]
    if (!length(dfs)) 
        stop("No data.frames loaded")
    dfvars <- lapply(dfs, function(x) names(get(x, envir = .GlobalEnv)))
    names(dfvars) <- dfs
    dfiscat <- lapply(dfs, function(x) sapply(get(x, envir = .GlobalEnv), 
        function(y) is.factor(y) || is.logical(y) || is.character(y)))
    names(dfiscat) <- dfs
    dfcat <- lapply(dfiscat, function(x) if (any(x)) 
        names(x[x])
    else "<NA>")
    dfnum <- lapply(dfiscat, function(x) if (any(!x)) 
        names(x[!x])
    else "<NA>")
    if (missing(dtf)) {
        dtfname <- dfs[1]
    }
    else {
        dtfname <- deparse(substitute(dtf))
        if (!dtfname %in% dfs) 
            stop(paste(dtfname, "is not a data.frame"))
    }
    if (missing(index)) {
        indexNames <- c(dfcat[[dtfname]][1], "<NA>", "<NA>", 
            "<NA>")
    }
    else {
        if (!(all(index %in% dfcat[[dtfname]]))) 
            stop("index variable(s) is(are) not categorical")
        indexNames <- if (length(index) < 4) 
            c(index, rep.int("<NA>", 4 - length(index)))
        else index[1:4]
    }
    if (missing(vSize)) {
        vSize <- dfnum[[dtfname]][1]
    }
    else {
        if (!(vSize %in% dfnum[[dtfname]])) 
            stop("vSize is not numeric")
    }
    if (missing(type)) {
        typeName <- "index"
    }
    else {
        if (!(type %in% c("value", "categorical", "comp", "dens", 
            "index", "depth"))) 
            stop("Invalid type")
        typeName <- type
    }
    if (missing(vColor)) {
        if (typeName %in% c("value", "comp", "dens")) 
            vColor <- dfnum[[dtfname]][1]
        if (typeName == "categorical") 
            vColor <- dfcat[[dtfname]][1]
    }
    else {
        if (typeName %in% c("value", "comp", "dens") && (!(vColor %in% 
            dfnum[[dtfname]]))) 
            stop("vColor is not numeric")
        if (typeName == "categorical" && (!(vColor %in% dfcat[[dtfname]]))) 
            stop("vColor is not categorical")
    }
    if (typeName %in% c("index", "depth")) 
        vColor <- "<not needed>"
    e <- environment()
    back <- 0
    x <- 0
    y <- 0
    count <- 0
    size <- ""
    color <- ""
    type <- ""
    index <- rep("", 4)
    runApp(list(ui = pageWithSidebar(headerPanel("", windowTitle = "Interactive Treemap"), 
        sidebarPanel(uiOutput("df"), uiOutput("ind1"), uiOutput("ind2"), 
            uiOutput("ind3"), uiOutput("ind4"), uiOutput("size"), 
            uiOutput("type"), uiOutput("color"), checkboxInput("fixscales", 
                "Fix scales", value = TRUE), checkboxInput("fixasp", 
                "Fix aspect ratio", value = TRUE), actionButton("back", 
                "Zoom out")), mainPanel(tabsetPanel(tabPanel("Treemap", 
            plotOutput("plot", hover = "hover", click = "click", 
                height = paste(height, "px", sep = "")), tableOutput("summary")), 
            tabPanel("Data", dataTableOutput("data")), tabPanel("Microdata", 
                dataTableOutput("microdata"))))), server = function(input, 
        output, session) {
        values <- reactiveValues()
        values$update <- FALSE
        dataset <- reactive({
            assign("filters", NULL, envir = e)
            assign("hcl", list(tmSetHCLoptions()), envir = e)
            assign("asp", NULL, envir = e)
            assign("range", NA, envir = e)
            assign("tm", NULL, envir = e)
            ifelse(is.null(input$df), dfs[1], input$df)
        })
        getHoverID <- reactive({
            p <- dataset()
            x <- input$hover$x
            y <- input$hover$y
            if (!is.null(tm)) {
                x <- (x - tm$vpCoorX[1])/(tm$vpCoorX[2] - tm$vpCoorX[1])
                y <- (y - tm$vpCoorY[1])/(tm$vpCoorY[2] - tm$vpCoorY[1])
                l <- tmLocate(list(x = x, y = y), tm)
                if (is.na(l[1, 1])) {
                  return(NULL)
                } else return(as.list(l[1, ]))
            } else {
                return(NULL)
            }
        })
        getClickID <- reactive({
            p <- dataset()
            x.new <- input$click$x
            y.new <- input$click$y
            if (is.null(x.new) || is.null(y.new)) return(NULL)
            if (x.new == x && y.new == y) return(NULL)
            assign("x", x.new, envir = e)
            assign("y", y.new, envir = e)
            if (!is.null(tm)) {
                x <- (x - tm$vpCoorX[1])/(tm$vpCoorX[2] - tm$vpCoorX[1])
                y <- (y - tm$vpCoorY[1])/(tm$vpCoorY[2] - tm$vpCoorY[1])
                l <- tmLocate(list(x = x, y = y), tm)
                if (is.na(l[1, 1])) {
                  return(NULL)
                } else return(as.list(l[1, ]))
            } else {
                return(NULL)
            }
        })
        getFilter <- reactive({
            p <- dataset()
            back.new <- input$back
            l <- getClickID()
            if (back.new == back) {
                if (!is.null(l)) if (!(l$x0 == 0 && l$y0 == 0 && 
                  l$w == 1 && l$y == 1)) {
                  filter <- as.character(l[[1]])
                  proceed <- is.null(filters)
                  if (!proceed) proceed <- (!length(filters)) || 
                    (filter != filters[length(filters)])
                  if (proceed) {
                    sel <- tm$tm[[1]] == filter
                    cols <- tm$tm$color[sel]
                    cols <- substr(cols, 1L, 7L)
                    cols <- hex2RGB(cols)
                    cols <- as(cols, "polarLUV")
                    hues <- cols@coords[, 3]
                    hcl.last <- hcl[[length(hcl)]]
                    hcl.last$hue_start <- min(hues)
                    hcl.last$hue_end <- max(hues)
                    notDeeper <- all(is.na(tm$tm[sel, 2]))
                    if (length(l) > 10 && !notDeeper) {
                      hcl.last$chroma <- hcl.last$chroma + hcl.last$chroma_slope
                      hcl.last$luminance <- hcl.last$luminance + 
                        hcl.last$luminance_slope
                    }
                    assign("hcl", c(hcl, list(hcl.last)), envir = e)
                    x0 <- tm$tm$x0[sel]
                    x1 <- x0 + tm$tm$w[sel]
                    y0 <- tm$tm$y0[sel]
                    y1 <- y0 + tm$tm$h[sel]
                    w <- max(x1) - min(x0)
                    h <- max(y1) - min(y0)
                    asp.new <- tm$aspRatio
                    assign("asp", if (is.null(asp)) c(asp.new, 
                      asp.new * (w/h)) else c(asp, asp.new * 
                      (w/h)), envir = e)
                    assign("range", tm$range, envir = e)
                    assign("filters", c(filters, filter), envir = e)
                  }
                }
            } else {
                if (!is.null(filters)) if (length(filters)) {
                  assign("filters", filters[-(length(filters))], 
                    envir = e)
                  assign("hcl", hcl[-(length(hcl))], envir = e)
                  assign("asp", asp[-(length(asp))], envir = e)
                  assign("range", tm$range, envir = e)
                }
                assign("back", back.new, envir = e)
            }
            filters
        })
        getSummary <- reactive({
            l <- getHoverID()
            if (!is.null(l)) {
                sizeID <- which(names(l) == "vSize")
                id <- switch(type, comp = sizeID + 2, dens = sizeID + 
                  2, value = sizeID + 1, index = sizeID, categorical = sizeID + 
                  1, depth = sizeID, color = sizeID)
                l <- l[1:id]
                names(l)[sizeID] <- size
                if (!(type %in% c("index", "depth", "color"))) names(l)[sizeID + 
                  1] <- color
                if (type == "comp") {
                  names(l)[sizeID + 2] <- paste("compared to", 
                    color, "(in %)")
                } else if (type == "dens") {
                  names(l)[sizeID + 2] <- paste(color, "per", 
                    size)
                }
                dt <- as.data.frame(l)
                row.names(dt) <- ""
                return(as.data.frame(dt))
            } else {
                dt <- data.frame("..." = "")
                row.names(dt) <- ""
                return(dt)
            }
        })
        output$df <- renderUI({
            selectInput("df", label = "Dataset:", choices = dfs, 
                selected = dtfname)
        })
        output$ind1 <- renderUI({
            p <- dataset()
            vars <- dfcat[[p]]
            selectInput("ind1", label = "Index variables", choices = vars, 
                selected = indexNames[1])
        })
        output$ind2 <- renderUI({
            p <- dataset()
            vars <- c("<NA>", dfcat[[p]])
            ind1 <- input$ind1
            if (!is.null(ind1)) {
                vars <- setdiff(vars, ind1)
                selectInput("ind2", label = "", choices = vars, 
                  selected = indexNames[2])
            }
        })
        output$ind3 <- renderUI({
            p <- dataset()
            vars <- c("<NA>", dfcat[[p]])
            ind1 <- input$ind1
            ind2 <- input$ind2
            if (!is.null(ind1) && !is.null(ind2)) {
                if (ind2 == "<NA>") {
                  vars <- "<NA>"
                } else {
                  vars <- setdiff(vars, c(ind1, ind2))
                }
                selectInput("ind3", label = "", choices = vars, 
                  selected = indexNames[3])
            }
        })
        output$ind4 <- renderUI({
            p <- dataset()
            vars <- c("<NA>", dfcat[[p]])
            ind1 <- input$ind1
            ind2 <- input$ind2
            ind3 <- input$ind3
            if (!is.null(ind1) && !is.null(ind2) && !is.null(ind3)) {
                if (ind3 == "<NA>") {
                  vars <- "<NA>"
                } else {
                  vars <- setdiff(vars, c(ind1, ind2, ind3))
                }
                selectInput("ind4", label = "", choices = vars, 
                  selected = indexNames[4])
            }
        })
        output$size <- renderUI({
            p <- dataset()
            vars <- dfnum[[p]]
            selectInput("size", label = "Size variable", choices = vars, 
                selected = vSize)
        })
        output$color <- renderUI({
            p <- dataset()
            type <- input$type
            if (!is.null(type)) {
                vars <- if (type == "index") {
                  "<not needed>"
                } else if (type == "value") {
                  dfnum[[p]]
                } else if (type == "comp") {
                  dfnum[[p]]
                } else if (type == "dens") {
                  dfnum[[p]]
                } else if (type == "depth") {
                  "<not needed>"
                } else if (type == "categorical") {
                  dfcat[[p]]
                }
                selectInput("color", label = "Color variable", 
                  choices = vars, selected = vColor)
            }
        })
        output$type <- renderUI({
            selectInput("type", label = "Type", choices = c("index", 
                "value", "comp", "dens", "depth", "categorical"), 
                selected = typeName)
        })
        output$plot <- renderPlot({
            filters <- getFilter()
            p <- dataset()
            size.new <- input$size
            color.new <- input$color
            type.new <- input$type
            ind1 <- input$ind1
            ind2 <- input$ind2
            ind3 <- input$ind3
            ind4 <- input$ind4
            asp.new <- input$fixasp
            scales <- input$fixscales
            if (is.null(size.new) || is.null(color.new) || is.null(type.new) || 
                is.null(ind1) || is.null(ind2) || is.null(ind3) || 
                is.null(ind4) || is.null(asp.new) || is.null(scales)) return(NULL)
            index.new <- c(ind1, ind2, ind3, ind4)
            if (all(index.new == index) && size.new == size && 
                color.new == color && type.new == type) {
            } else {
                assign("range", NA, envir = e)
            }
            assign("size", size.new, envir = e)
            assign("color", color.new, envir = e)
            assign("type", type.new, envir = e)
            assign("index", index.new, envir = e)
            index.new <- index.new[index.new != "<NA>"]
            zoomLevel <- if (is.null(filters)) 0 else length(filters)
            if (!(anyDuplicated(index.new)) && ((color.new == 
                "<not needed>" && (type.new %in% c("index", "depth"))) || 
                ((color.new %in% dfnum[[p]]) && (type.new %in% 
                  c("value", "comp", "dens"))) || ((color.new %in% 
                dfcat[[p]]) && (type.new == "categorical"))) && 
                all(index.new %in% dfcat[[p]])) {
                par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
                plot(c(0, 1), c(0, 1), axes = F, col = "white")
                vps <- baseViewports()
                dat <- get(p, envir = .GlobalEnv)
                if (zoomLevel > 0) {
                  filterString <- paste(paste(index.new[1:zoomLevel], 
                    paste("\"", filters, "\"", sep = ""), sep = " == "), 
                    collapse = " & ")
                  selection <- eval(parse(text = filterString), 
                    dat, parent.frame())
                  dat <- dat[selection, ]
                  allNA <- sapply(dat[, index.new], function(x) all(is.na(x)))
                  maxLevel <- ifelse(any(allNA), which(allNA)[1] - 
                    1, length(index.new))
                  minLevel <- min(maxLevel, zoomLevel + 1, length(index.new))
                  if (length(index.new) > 1) index.new <- index.new[(minLevel:maxLevel)]
                  aspRatio <- ifelse(asp.new, asp[length(asp)], 
                    NA)
                } else {
                  aspRatio <- NA
                }
                assign("count", count + 1, envir = e)
                assign("range", if (scales) range else NA, envir = e)
                hcl.new <- if (scales) as.list(hcl[[zoomLevel + 
                  1]]) else hcl[[1]]
                values$update <- TRUE
                tm <- treemap(dat, index = index.new, vSize = size.new, 
                  vColor = color.new, type = type.new, vp = vps$plot, 
                  palette.HCL.options = hcl.new, aspRatio = aspRatio, 
                  range = range, title = "")
                values$update <- FALSE
                assign("tm", tm, envir = e)
                tmString <- paste0("treemap(", ifelse(zoomLevel == 
                  0, p, paste0("subset(", p, ", subset=", filterString, 
                  ")")), ", index=", if (length(index.new) == 
                  1) paste0("\"", index.new, "\"") else paste0("c(", 
                  paste(paste0("\"", index.new, "\""), collapse = ", "), 
                  ")"), ", vSize=\"", size.new, "\"", if (color.new != 
                  "<not needed>") paste0(", vColor=\"", color.new, 
                  "\""), ", type=\"", type.new, "\")")
                if (command.line.output) cat(tmString, "\n")
            }
        })
        output$summary <- renderTable({
            getSummary()
        })
        output$microdata <- renderDataTable({
            filters <- getFilter()
            p <- dataset()
            size <- input$size
            color <- input$color
            type <- input$type
            ind1 <- input$ind1
            ind2 <- input$ind2
            ind3 <- input$ind3
            ind4 <- input$ind4
            asp.new <- input$fixasp
            scales <- input$fixscales
            update <- values$update
            dat <- get(p, envir = .GlobalEnv)
            index.new <- c(ind1, ind2, ind3, ind4)
            zoomLevel <- if (is.null(filters)) 0 else length(filters)
            if (zoomLevel > 0) {
                filterString <- paste(paste(index[1:zoomLevel], 
                  paste("\"", filters, "\"", sep = ""), sep = " == "), 
                  collapse = " & ")
                selection <- eval(parse(text = filterString), 
                  dat, parent.frame())
                dat <- dat[selection, ]
            }
            dat
        })
        output$data <- renderDataTable({
            p <- dataset()
            size.new <- input$size
            color.new <- input$color
            type.new <- input$type
            ind1 <- input$ind1
            ind2 <- input$ind2
            ind3 <- input$ind3
            ind4 <- input$ind4
            asp <- input$fixasp
            scales <- input$fixscales
            update <- values$update
            tm <- tm$tm
            lvls <- tm$level
            dat <- tm[lvls == max(lvls), 1:(ncol(tm) - 6)]
            sizeID <- which(names(dat) == "vSize")
            id <- switch(type, comp = sizeID + 2, dens = sizeID + 
                2, value = sizeID + 1, index = sizeID, categorical = sizeID + 
                1, depth = sizeID, color = sizeID)
            dat <- dat[, 1:id]
            names(dat)[sizeID] <- size
            if (!(type %in% c("index", "depth", "color"))) names(dat)[sizeID + 
                1] <- color
            if (type == "comp") {
                names(dat)[sizeID + 2] <- paste("compared to", 
                  color, "(in %)")
            } else if (type == "dens") {
                names(dat)[sizeID + 2] <- paste(color, "per", 
                  size)
            }
            dat
        })
    }))
}


treemap <- function (dtf, index, vSize, vColor = NULL, stdErr = NULL, type = "index", 
    fun.aggregate = "sum", title = NA, title.legend = NA, algorithm = "pivotSize", 
    sortID = "-size", mirror.x = FALSE, mirror.y = FALSE, palette = NA, 
    palette.HCL.options = NULL, range = NA, mapping = NA, n = 7, 
    fontsize.title = 14, fontsize.labels = 11, fontsize.legend = 12, 
    fontcolor.labels = NULL, fontface.labels = c("bold", rep("plain", 
        length(index) - 1)), fontfamily.title = "sans", fontfamily.labels = "sans", 
    fontfamily.legend = "sans", border.col = "black", border.lwds = c(length(index) + 
        1, (length(index) - 1):1), lowerbound.cex.labels = 0.4, 
    inflate.labels = FALSE, bg.labels = NULL, force.print.labels = FALSE, 
    overlap.labels = 0.5, align.labels = c("center", "center"), 
    xmod.labels = 0, ymod.labels = 0, eval.labels = FALSE, position.legend = NULL, 
    format.legend = NULL, drop.unused.levels = TRUE, aspRatio = NA, 
    vp = NULL, draw = TRUE, ...) 
{
    s <- NULL
    vColor.temp <- i <- w <- h <- NULL
    if (!exists("dtf")) 
        stop("Dataframe <dtf> not defined")
    if (!exists("index")) 
        stop("Attribute <index> not defined")
    if (!exists("vSize")) 
        stop("Attribute <vSize> not defined")
    if (!inherits(dtf, "data.frame")) 
        stop("Object <dtf> is not a data.frame")
    if (nrow(dtf) == 0) 
        stop("data.frame doesn't have any rows")
    if (any(!index %in% names(dtf))) 
        stop("<index> contains invalid column names")
    depth <- length(index)
    if (length(vSize) != 1) 
        stop("vSize should be one column name")
    if (!vSize %in% names(dtf)) 
        stop("vSize is invalid column name")
    if (!is.numeric(dtf[[vSize]])) 
        stop(paste("Column(s) in vSize not numeric", sep = ""))
    if (!is.null(stdErr)) {
        if (length(stdErr) != 1) 
            stop("stdErr should be one column name")
        if (!stdErr %in% names(dtf)) 
            stop("stdErr is invalid column name")
        if (!is.numeric(dtf[[stdErr]])) 
            stop(paste("Column(s) in stdErr not numeric", sep = ""))
    }
    else {
        stdErr <- vSize
    }
    vColorMplySplit <- function(vColor) {
        divided <- 0
        vColorMply <- unlist(strsplit(vColor, split = "*", fixed = TRUE))
        if (length(vColorMply) == 1) {
            vColorMply <- unlist(strsplit(vColor, split = "/", 
                fixed = TRUE))
            if (length(vColorMply) == 1) {
                vColorMply <- c(vColorMply, 1)
            }
            else {
                vColorMply[2] <- (1/as.numeric(vColorMply[2]))
                divided <- 1
            }
        }
        return(c(vColorMply, divided))
    }
    if (type %in% c("index", "depth")) 
        vColor <- NULL
    if (!is.null(vColor)) {
        if (length(vColor) != 1) 
            stop("length of vColor should be one")
        vColor2 <- vColorMplySplit(vColor)
        vColorX <- as.numeric(vColor2[2])
        if (is.na(vColorX)) 
            stop("vColor: invalid scale factor")
        vColor <- vColor2[1]
        if (!(vColor %in% names(dtf))) 
            stop("Invalid column name in vColor.")
        vColorDiv <- as.logical(as.numeric(vColor2[3]))
    }
    if (!type %in% c("value", "categorical", "comp", "dens", 
        "index", "depth", "color", "manual")) 
        stop("Invalid type")
    if (!is.function(match.fun(fun.aggregate))) 
        stop("fun.aggregate is not a function")
    if (type == "dens") 
        fun.aggregate <- "weighted.mean"
    if (!is.na(title[1]) && length(title) != 1) {
        warning("Length of title should be 1")
        title <- NA
    }
    if (is.na(title[1])) {
        title <- vSize
    }
    if (!is.na(title.legend[1]) && length(title.legend) != 1) {
        warning("Length of title.legend should be 1")
        title.legend <- NA
    }
    formatColorTitle <- function(var, varX = NA, var2 = NA, var2X = NA, 
        div) {
        if (!is.na(var2)) {
            if (var2X != 1) {
                if (div) 
                  var2 <- paste(1/var2X, var2, sep = "*")
                else var <- paste(var2X, var, sep = "*")
            }
            var <- paste(var, "per", var2, sep = " ")
        }
        else {
            if (varX != 1) {
                if (div) 
                  var <- paste(var, 1/varX, sep = "/")
                else var <- paste(varX, var, sep = "*")
            }
        }
        var
    }
    if (is.na(title.legend[1])) {
        suppressWarnings({
            if (!is.null(vColor)) {
                if (type == "dens") 
                  title.legend <- formatColorTitle(var = vColor, 
                    var2 = vSize, var2X = vColorX, div = vColorDiv)
                else title.legend <- formatColorTitle(var = vColor, 
                  varX = vColorX, div = vColorDiv)
            }
            else title.legend <- ""
        })
    }
    if (!algorithm %in% c("pivotSize", "squarified")) 
        stop("Invalid algorithm")
    if (length(sortID) != 1) 
        stop("sortID should be of length one")
    ascending <- substr(sortID, 1, 1) != "-"
    if (!ascending) 
        sortID <- substr(sortID, 2, nchar(sortID))
    if (sortID == "size") 
        sortID <- vSize
    if (sortID == "color") 
        sortID <- vColor
    if (sortID == "se") 
        sortID <- stdErr
    if (!(sortID %in% names(dtf))) 
        stop("Incorrect sortID")
    if (is.na(palette[1])) {
        if (type == "comp") {
            palette <- brewer.pal(11, "RdYlGn")
        }
        else if (type == "dens") {
            palette <- brewer.pal(9, "OrRd")
        }
        else if (type == "depth") {
            palette <- brewer.pal(8, "Set2")
        }
        else if (type == "index") {
            palette <- "HCL"
        }
        else if (type == "value") {
            palette <- brewer.pal(11, "RdYlGn")
        }
        else if (type == "categorical") {
            palette <- "HCL"
        }
        else if (type == "manual") {
            stop("For \"manual\" treemaps, a palette should be provided.")
        }
    }
    else {
        reverse <- (substr(palette[1], 1, 1) == "-")
        if (reverse) 
            palette[1] <- substr(palette[1], 2, nchar(palette[1]))
        if ((length(palette) == 1) && (palette[1] %in% row.names(brewer.pal.info))) {
            palette <- brewer.pal(brewer.pal.info[palette, "maxcolors"], 
                palette)
            if (reverse) 
                palette <- rev(palette)
        }
        else {
            if (palette[1] == "HCL" && !(type %in% c("depth", 
                "index", "categorical"))) {
                stop("HCL palette only applicable for treemap types \"depth\", \"index\" and \"categorical\".")
            }
            if (palette[1] != "HCL") 
                if (class(try(col2rgb(palette), silent = TRUE)) == 
                  "try-error") 
                  stop("color palette is not correct")
        }
    }
    palette.HCL.options <- tmSetHCLoptions(palette.HCL.options)
    if (!all(is.na(range))) {
        if (length(range) != 2) 
            stop("length range should be 2")
        if (!is.numeric(range)) 
            stop("range is not numeric")
    }
    else range <- c(NA, NA)
    if (!all(is.na(mapping))) {
        if (!length(mapping) %in% c(2, 3)) 
            stop("length range should be 2 or 3")
        if (!is.numeric(mapping)) 
            stop("range is not numeric")
        if (length(mapping) == 2) {
            mapping <- c(mapping[1], mean(mapping), mapping[2])
        }
    }
    else mapping <- c(NA, NA, NA)
    if (length(fontsize.title) != 1 || !is.numeric(fontsize.title)) 
        stop("Invalid fontsize.title")
    if (title == "") 
        fontsize.title <- 0
    if (!is.numeric(fontsize.labels)) 
        stop("Invalid fontsize.labels")
    fontsize.labels <- rep(fontsize.labels, length.out = depth)
    cex_indices <- fontsize.labels/max(min(fontsize.labels), 
        1)
    if (length(fontsize.legend) != 1 || !is.numeric(fontsize.legend)) 
        stop("Invalid fontsize.legend")
    if (!missing(fontcolor.labels)) 
        if (length(fontcolor.labels) != depth) 
            fontcolor.labels <- rep(fontcolor.labels, length.out = depth)
    if (length(fontface.labels) != depth) 
        fontface.labels <- rep(fontface.labels, length.out = depth)
    if (length(border.col) != depth) 
        border.col <- rep(border.col, length.out = depth)
    if (length(border.lwds) != depth) 
        border.lwds <- rep(border.lwds, length.out = depth)
    if (length(lowerbound.cex.labels) != 1 || !is.numeric(lowerbound.cex.labels)) 
        stop("Invalid lowerbound.cex.labels")
    if (lowerbound.cex.labels < 0 || lowerbound.cex.labels > 
        1) 
        stop("lowerbound.cex.labels not between 0 and 1")
    if (length(inflate.labels) != 1 || class(inflate.labels) != 
        "logical") 
        stop("Invalid inflate.labels")
    if (missing(bg.labels)) {
        bg.labels <- ifelse(type %in% c("value", "categorical"), 
            "#CCCCCCDC", 220)
    }
    else {
        if (length(bg.labels) != 1) 
            stop("Invalid bg.labels")
        if (!is.numeric(bg.labels)) {
            if (class(try(col2rgb(bg.labels), silent = TRUE)) == 
                "try-error") 
                stop("Invalid bg.labels")
        }
        else {
            if (bg.labels < 0 || bg.labels > 255) 
                stop("bg.labels should be between 0 and 255")
        }
    }
    if (length(force.print.labels) != 1 || class(force.print.labels) != 
        "logical") 
        stop("Invalid force.print.labels")
    if (length(overlap.labels) != 1 || !is.numeric(overlap.labels)) 
        stop("Invalid overlap.labels")
    if (overlap.labels < 0 || overlap.labels > 1) 
        stop("overlap.labels should be between 0 and 1")
    if (!is.list(align.labels)) 
        align.labels <- list(align.labels)
    if (length(align.labels) != depth) 
        align.labels <- rep(align.labels, length.out = depth)
    lapply(align.labels, function(al) if (!(al[1] %in% c("left", 
        "center", "centre", "right") && al[2] %in% c("top", "center", 
        "centre", "bottom"))) 
        stop("incorrect align.labels"))
    if (length(xmod.labels) != depth) 
        xmod.labels <- rep(xmod.labels, length.out = depth)
    if (length(ymod.labels) != depth) 
        ymod.labels <- rep(ymod.labels, length.out = depth)
    if (missing(position.legend)) {
        position.legend <- switch(type, categorical = "right", 
            depth = "right", index = "none", color = "none", 
            "bottom")
    }
    else {
        if (!position.legend %in% c("right", "bottom", "none")) 
            stop("Invalid position.legend")
    }
    if (length(drop.unused.levels) != 1 || class(drop.unused.levels) != 
        "logical") 
        stop("Invalid drop.unused.levels")
    if (length(aspRatio) != 1 || (!is.na(aspRatio[1]) && !is.numeric(aspRatio))) 
        stop("Invalid aspRatio")
    args <- list(...)
    args$na.rm <- TRUE
    if (inherits(dtf, c("tbl_df"))) {
        dtfDT <- as.data.table(data.frame(dtf[, c(index, vSize, 
            vColor, sortID, stdErr)]))
    }
    else if (is.data.table(dtf)) {
        dtfDT <- copy(dtf[, c(index, vSize, vColor, sortID, stdErr), 
            with = FALSE])
    }
    else {
        dtfDT <- as.data.table(dtf[, c(index, vSize, vColor, 
            sortID, stdErr)])
    }
    if (is.null(vColor)) {
        vColor <- "vColor.temp"
        vColorX <- 1
        dtfDT[, `:=`(vColor.temp, 1)]
        setcolorder(dtfDT, c(1:(ncol(dtfDT) - 3), ncol(dtfDT), 
            ncol(dtfDT) - 2, ncol(dtfDT) - 1))
    }
    indexList <- paste0("index", 1:depth)
    setnames(dtfDT, old = 1:ncol(dtfDT), new = c(indexList, "s", 
        "c", "i", "se"))
    if (vColorX != 1) 
        dtfDT[, `:=`(c, c/vColorX)]
    if (fun.aggregate == "weighted.mean") {
        if ("w" %in% names(args)) {
            if (is.character(args$w)) {
                dtfDT[, `:=`(w, eval(parse(text = args$w)))]
            }
            else {
                dtfDT[, `:=`(w, args$w)]
            }
        }
        else {
            dtfDT[, `:=`(w, s)]
        }
    }
    else {
        dtfDT[, `:=`(w, 1)]
    }
    for (d in 1:depth) {
        if (is.numeric(dtfDT[[d]])) {
            fact <- factor(dtfDT[[d]], levels = sort(unique(dtfDT[[d]])))
            dtfDT[, `:=`((d), fact)]
        }
        else if (!is.factor(dtfDT[[d]])) {
            fact <- factor(dtfDT[[d]])
            dtfDT[, `:=`((d), fact)]
        }
    }
    if (is.character(dtfDT[["c"]])) {
        fact <- factor(dtfDT[["c"]])
        dtfDT[, `:=`("c", fact), with = FALSE]
    }
    if (!is.numeric(dtfDT[["i"]])) {
        warning("sortID must be a numeric variable")
        dtfDT[, `:=`(i, integer(nrow(dtfDT)))]
    }
    if (!is.null(stdErr) && !is.numeric(dtfDT[["se"]])) {
        warning("stdErr must be a numeric variable")
        dtfDT[, `:=`("se", integer(dtfDT[["se"]]))]
    }
    setkeyv(dtfDT, indexList)
    datlist <- tmAggregate(dtfDT, indexList, type, ascending, 
        drop.unused.levels, fun.aggregate, args)
    catLabels <- switch(type, categorical = levels(datlist$c), 
        index = levels(datlist$index1), depth = index, standErr = datlist$se, 
        NA)
    if (!draw) 
        position.legend <- "none"
    vps <- tmGetViewports(vp, fontsize.title, fontsize.labels, 
        fontsize.legend, position.legend, type, aspRatio, title.legend, 
        catLabels)
    if (draw) 
        tmPrintTitles(vps, title, title.legend, position.legend, 
            fontfamily.title, fontfamily.legend)
    if (type == "color") {
        datlist$color <- as.character(datlist$c)
        datlist$colorvalue <- NA
    }
    else {
        attr(datlist, "range") <- 1:2
        datlist <- tmColorsLegend(datlist, vps, position.legend, 
            type, palette, range, mapping, indexNames = index, 
            palette.HCL.options = palette.HCL.options, border.col, 
            fontfamily.legend, n, format.legend)
    }
    datlist <- tmGenerateRect(datlist, vps, indexList, algorithm)
    if (mirror.x) 
        datlist <- within(datlist, x0 <- 1 - x0 - w)
    if (mirror.y) 
        datlist <- within(datlist, y0 <- 1 - y0 - h)
    if (draw) {
        tmDrawRect(datlist, vps, indexList, lowerbound.cex.labels, 
            inflate.labels, bg.labels, force.print.labels, cex_indices, 
            overlap.labels, border.col, border.lwds, fontcolor.labels, 
            fontface.labels, fontfamily.labels, align.labels, 
            xmod.labels, ymod.labels, eval.labels)
    }
    upViewport(0 + (!is.null(vp)))
    tm <- datlist[, c(indexList, "s", "c", "se", "colorvalue", 
        "l", "x0", "y0", "w", "h", "color"), with = FALSE]
    if (type == "dens") 
        tm[, `:=`(c, c * s)]
    setnames(tm, c(index, "vSize", "vColor", "stdErr", "vColorValue", 
        "level", "x0", "y0", "w", "h", "color"))
    tmSave <- list(tm = as.data.frame(tm), type = type, vSize = vSize, 
        vColor = ifelse(vColor == "vColor.temp", NA, vColor), 
        stdErr = stdErr, algorithm = algorithm, vpCoorX = vps$vpCoorX, 
        vpCoorY = vps$vpCoorY, aspRatio = vps$aspRatio, range = range, 
        mapping = mapping, draw = draw)
    invisible(tmSave)
}


treepalette <- function (dtf, index = names(dtf), method = "HCL", palette = NULL, 
    palette.HCL.options, return.parameters = TRUE, prepare.dat = TRUE) 
{
    .SD <- NULL
    palette.HCL.options <- tmSetHCLoptions(palette.HCL.options)
    k <- length(index)
    dat <- as.data.table(dtf)
    othercols <- setdiff(names(dat), index)
    if (length(othercols)) 
        dat[, `:=`(eval(othercols), NULL)]
    setcolorder(dat, index)
    dat[, `:=`(names(dat), lapply(.SD, as.factor))]
    if (prepare.dat) {
        if (k > 1) {
            dats <- list()
            for (i in 1:(k - 1)) {
                dats[[i]] <- dat[!duplicated(dat[, 1:i, with = FALSE]), 
                  ]
                for (j in (i + 1):k) dats[[i]][[j]] <- factor(NA, 
                  levels = levels(dats[[i]][[j]]))
            }
            dat <- rbindlist(c(list(dat), dats))
        }
        dat <- dat[!duplicated(dat), ]
        dep <- treedepth(dat)
        unikey <- do.call("paste", c(as.list(dat), list(dep, 
            sep = "__")))
        dat <- dat[order(unikey), ]
    }
    if (method == "HCL") {
        res <- treeapply(dat, list(lb = as.integer(palette.HCL.options$hue_start), 
            ub = as.integer(palette.HCL.options$hue_end), rev = FALSE), 
            fun = "addRange", frc = palette.HCL.options$hue_fraction, 
            hue_perm = palette.HCL.options$hue_perm, hue_rev = palette.HCL.options$hue_rev)
        point <- with(res, (lb + ub)/2)
        chr <- palette.HCL.options$chroma + palette.HCL.options$chroma_slope * 
            (res$l - 1)
        lum <- palette.HCL.options$luminance + palette.HCL.options$luminance_slope * 
            (res$l - 1)
        color <- hcl(point, c = chr, l = lum)
        if (return.parameters) {
            return(cbind(as.data.frame(dat), data.table(HCL.color = color, 
                HCL.H = point, HCL.C = chr, HCL.L = lum, HCL.hue_lb = res$lb, 
                HCL.hue_ub = res$ub)))
        }
        else {
            return(color)
        }
    }
    else if (method == "HSV") {
        nl <- nlevels(dat[[1]])
        palette <- substr(palette, 1, 7)
        palette <- rep(palette, length.out = nl)
        co <- coords(as(hex2RGB(palette), "HSV"))
        value <- as.list(as.data.frame(co))
        res <- treeapply(dat, value, fun = "hsvs")
        color <- with(res, hex(HSV(H, S, V)))
        if (return.parameters) {
            return(cbind(as.data.frame(dat), data.frame(HSV.color = color, 
                HSV.H = res$H, HSV.S = res$S, HSV.V = res$V)))
        }
        else {
            return(color)
        }
    }
}


treecolors <- function (height = 700) 
{
    runApp(list(ui = pageWithSidebar(headerPanel(p(HTML("Tree Colors <span style=\"color: #999999\">color schemes for tree structured data</span>")), 
        windowTitle = "Tree Colors"), sidebarPanel(p(strong("Random tree data")), 
        sliderInput(inputId = "n", label = "Number of leaf nodes", 
            min = 10, max = 100, step = 1, value = 30), sliderInput(inputId = "d", 
            label = "Tree depth", min = 2, max = 6, step = 1, 
            value = 3), br(), p(strong("Hue")), sliderInput(inputId = "Hstart", 
            label = "Hue start", min = -360, max = 360, step = 10, 
            value = 0), sliderInput(inputId = "Hend", label = "Hue end", 
            min = -360, max = 360, step = 10, value = 360), sliderInput(inputId = "Hf", 
            label = "Hue fraction", min = 0, max = 1, step = 0.05, 
            value = 0.75), checkboxInput("Hperm", "Hue permutations", 
            TRUE), checkboxInput("Hrev", "Hue reverse", TRUE), 
        br(), p(strong("Luminance")), sliderInput(inputId = "L", 
            label = "\nLuminance first level value", min = 0, 
            max = 100, step = 1, value = 70), sliderInput(inputId = "Lslope", 
            label = "Luminance slope value", min = -20, max = 20, 
            step = 1, value = -10), br(), p(strong("Chroma")), 
        sliderInput(inputId = "C", label = "Chroma first level value", 
            min = 0, max = 100, step = 1, value = 60), sliderInput(inputId = "Cslope", 
            label = "Chroma slope value", min = -20, max = 20, 
            step = 1, value = 5)), mainPanel(tabsetPanel(tabPanel("Graph (Reingold-Tilford)", 
        plotOutput("gplot1", height = paste(height, "px", sep = ""))), 
        tabPanel("Graph (Fruchterman-Reingold)", plotOutput("gplot2", 
            height = paste(height, "px", sep = ""))), tabPanel("Treemap", 
            plotOutput("tmplot", height = paste(height, "px", 
                sep = ""))), tabPanel("Bar chart", plotOutput("barchart", 
            height = paste(height, "px", sep = ""))), tabPanel("Data", 
            dataTableOutput("data"))))), server = function(input, 
        output, session) {
        data <- reactive({
            dat <- random.hierarchical.data(n = input$n, depth = input$d, 
                value.generator = rnorm, value.generator.args = list(mean = 15, 
                  sd = 3))
            dat$x[dat$x < 0] <- 0
            dat
        })
        HCL.options <- reactive({
            huestart <- input$Hstart
            hueend <- input$Hend
            list(hue_start = huestart, hue_end = hueend, hue_perm = input$Hperm, 
                hue_rev = input$Hrev, hue_fraction = input$Hf, 
                chroma = input$C, luminance = input$L, chroma_slope = input$Cslope, 
                luminance_slope = input$Lslope)
        })
        output$gplot1 <- renderPlot({
            dat <- data()
            treegraph(dat, index = names(dat)[1:(ncol(dat) - 
                1)], palette.HCL.options = HCL.options(), vertex.size = 6, 
                show.labels = TRUE)
        })
        output$gplot2 <- renderPlot({
            dat <- data()
            random.seed <- sample.int((2^31) - 1, 1)
            set.seed(1234)
            treegraph(dat, index = names(dat)[1:(ncol(dat) - 
                1)], palette.HCL.options = HCL.options(), vertex.size = 6, 
                vertex.layout = "fruchterman.reingold", show.labels = TRUE)
            set.seed(random.seed)
        })
        output$tmplot <- renderPlot({
            dat <- data()
            for (i in 1:(ncol(dat) - 2)) levels(dat[[i]]) <- paste("Category", 
                levels(dat[[i]]))
            treemap(dat, index = names(dat)[1:(ncol(dat) - 1)], 
                vSize = "x", palette.HCL.options = HCL.options(), 
                bg.labels = 255, overlap.labels = 0.1, title = "")
        })
        output$barchart <- renderPlot({
            dat <- data()
            d <- input$d
            for (i in 1:d) dat[[i]] <- factor(as.character(dat[[i]]), 
                levels = rev(unique(as.character(dat[[i]]))))
            datcolors <- treepalette(dat, index = paste("index", 
                1:d, sep = ""), palette.HCL.options = HCL.options())
            dat$color <- datcolors$HCL.color[match(dat[[d]], 
                datcolors[[d]])]
            dat$sp <- addSpace(dat[, 1:d])
            dat$sp <- max(dat$sp) - dat$sp
            print(ggplot(dat, aes_string(x = "sp", y = "x", fill = paste("index", 
                d, sep = ""))) + geom_bar(stat = "identity") + 
                scale_x_continuous("", breaks = dat$sp, labels = dat[[d]]) + 
                scale_y_continuous("") + scale_fill_manual(values = rev(dat$color)) + 
                coord_flip() + theme_bw() + theme(legend.position = "none"))
        })
        output$barchart <- renderPlot({
            dat <- data()
            d <- input$d
            for (i in 1:d) dat[[i]] <- factor(as.character(dat[[i]]), 
                levels = rev(unique(as.character(dat[[i]]))))
            datcolors <- treepalette(dat, index = paste("index", 
                1:d, sep = ""), palette.HCL.options = HCL.options())
            dat$color <- datcolors$HCL.color[match(dat[[d]], 
                datcolors[[d]])]
            dat$sp <- addSpace(dat[, 1:d])
            dat$sp <- max(dat$sp) - dat$sp
            print(ggplot(dat, aes_string(x = "sp", y = "x", fill = paste("index", 
                d, sep = ""))) + geom_bar(stat = "identity") + 
                scale_x_continuous("", breaks = dat$sp, labels = dat[[d]]) + 
                scale_y_continuous("") + scale_fill_manual(values = rev(dat$color)) + 
                coord_flip() + theme_bw() + theme(legend.position = "none"))
        })
        output$data <- renderDataTable({
            dat <- data()
            d <- input$d
            treepalette(dat, index = paste("index", 1:d, sep = ""), 
                palette.HCL.options = HCL.options())
        })
    }))
}


treegraph <- function (dtf, index = names(dtf), directed = FALSE, palette.HCL.options, 
    show.labels = FALSE, rootlabel = "", vertex.layout = "reingold.tilford", 
    vertex.layout.params, truncate.labels = NULL, vertex.size = 3, 
    vertex.label.dist = 0.3, vertex.label.cex = 0.8, vertex.label.family = "sans", 
    vertex.label.color = "black", mai = c(0, 0, 0, 0), ...) 
{
    palette.HCL.options <- tmSetHCLoptions(palette.HCL.options)
    k <- length(index)
    dat <- treepalette(dtf, index, palette.HCL.options = palette.HCL.options, 
        return.parameters = TRUE)[, 1:(k + 1)]
    dat <- unique(dat)
    if (!missing(truncate.labels)) {
        truncate.labels <- rep(truncate.labels, length.out = k)
        for (i in 1:k) {
            levels(dat[[i]]) <- substr(levels(dat[[i]]), 1, truncate.labels[i])
        }
    }
    dat <- cbind(dat, as.data.table(treeid(dat[, 1:k, drop = FALSE])))
    vdat <- dat[, c("current", "HCL.color")]
    setnames(vdat, "HCL.color", "color")
    vdat <- unique(vdat)
    rootname <- dat$parent[which(substr(dat$parent, 1, 2) == 
        "NA")][1]
    vdat <- rbind(list(current = rootname, color = hcl(h = 0, 
        c = 0, l = palette.HCL.options$luminance - palette.HCL.options$luminance_slope)), 
        vdat)
    g <- graph.data.frame(dat[, c("parent", "current")], vertices = vdat, 
        directed = directed)
    if (show.labels) {
        vdat_names <- gsub("__NA", "", vdat$current, fixed = TRUE)
        vdat_names[1] <- rootlabel
        ssres <- strsplit(vdat_names, split = "__", fixed = TRUE)
        ssres[[1]] <- rootlabel
        vdat_names <- sapply(ssres, function(x) x[length(x)])
        lengths <- nchar(vdat_names)
        heads <- floor((max(lengths) - lengths)/2)
        tails <- max(lengths) - lengths - heads
        hs <- sapply(heads, function(x) paste(rep(" ", x), collapse = ""))
        ts <- sapply(tails, function(x) paste(rep(" ", x), collapse = ""))
        vdat_names <- mapply(paste0, hs, vdat_names, ts)
    }
    else {
        vdat_names <- NA
    }
    par(mai = mai)
    vertex.layout.function <- get(paste("layout", vertex.layout, 
        sep = "."))
    if (missing(vertex.layout.params)) {
        vertex.layout.params <- if (vertex.layout == "reingold.tilford") {
            list(circular = T, root = 1)
        }
        else {
            list()
        }
    }
    vertex.layout.params = c(list(graph = g), vertex.layout.params)
    vertex.layout = do.call(vertex.layout.function, vertex.layout.params)
    plot(g, vertex.size = vertex.size, vertex.frame.color = NA, 
        vertex.label = vdat_names, layout = vertex.layout, vertex.label.dist = vertex.label.dist, 
        vertex.label.cex = vertex.label.cex, vertex.label.family = vertex.label.family, 
        vertex.label.color = vertex.label.color, vertex.label.degree = -pi/2.5, 
        ...)
    invisible(g)
}


random.hierarchical.data <- function (n = NULL, method = "random", number.children = 3, children.root = 4, 
    depth = 3, nodes.per.layer = NULL, labels = c("LETTERS", 
        "numbers", "letters"), labels.prefix = NULL, sep = ".", 
    colnames = c(paste("index", 1:depth, sep = ""), "x"), value.generator = rlnorm, 
    value.generator.args = NULL) 
{
    if (!missing(n)) {
        nchild <- n^(1/depth)
        nodes.per.layer <- round(nchild^(1:depth))
        method <- "random.arcs"
    }
    if (!(method %in% c("random", "full.tree", "random.arcs"))) 
        stop("Incorrect method.")
    if (method == "random") {
        if (is.null(number.children)) 
            stop("Required argument ave.number.children not specified.")
    }
    else if (method == "full.tree") {
        if (is.null(number.children)) 
            stop("Required argument number.children not specified.")
        if (length(number.children) == 1) 
            number.children <- rep(number.children, depth)
        else depth <- length(number.children)
    }
    else if (method == "random.arcs") {
        if (is.null(nodes.per.layer)) 
            stop("Required argument nodes.per.layer not specified.")
        if (length(nodes.per.layer) == 1) 
            nodes.per.layer <- rep(nodes.per.layer, depth)
        else depth <- length(nodes.per.layer)
    }
    l <- list(as.character(1:switch(method, random = children.root, 
        full.tree = number.children[1], random.arcs = nodes.per.layer[1])))
    isleaf <- list()
    for (d in 2:depth) {
        parents <- l[[d - 1]]
        nprev <- length(parents)
        x <- switch(method, random = rpois(nprev, lambda = number.children), 
            full.tree = rep(number.children[d], nprev), random.arcs = rep(1, 
                nprev) + table(factor(sample.int(nprev, nodes.per.layer[d] - 
                nprev, replace = TRUE), levels = 1:nprev)))
        l <- c(l, list(unlist(mapply(function(y, z) if (z == 
            0) NULL else paste(rep(y, z), 1:z, sep = sep), parents, 
            x, SIMPLIFY = FALSE))))
        isleaf <- c(isleaf, list(x == 0))
    }
    isleaf <- c(isleaf, list(rep(TRUE, length(l[[depth]]))))
    depth <- sum(sapply(l, length) != 0)
    l <- l[1:depth]
    isleaf <- isleaf[1:depth]
    labels <- rep(labels, length.out = depth)
    if (!missing(labels.prefix)) 
        labels.prefix <- rep(labels.prefix, length.out = depth)
    l <- mapply("[", l, isleaf)
    l <- lapply(l, function(ll) {
        if (!length(ll)) 
            return(NULL)
        spl <- lapply(ll, function(lll) strsplit(lll, split = sep, 
            fixed = TRUE)[[1]])
        rows <- matrix(unlist(lapply(spl, function(spll) sapply(1:length(spll), 
            function(x) paste(spll[1:x], collapse = sep)))), 
            nrow = length(ll), byrow = TRUE)
        if (ncol(rows) < depth) 
            rows <- cbind(rows, matrix(NA, nrow = length(ll), 
                ncol = depth - ncol(rows)))
        as.data.frame(rows, stringsAsFactors = FALSE)
    })
    l <- do.call("rbind", l)
    l <- l[do.call("order", l), ]
    l <- add.labels(l, labels, labels.prefix, sep)
    l$x <- do.call("value.generator", args = c(list(nrow(l)), 
        value.generator.args))
    names(l) <- colnames[c(1:depth, ncol(l))]
    l
}


tmPlot <- function (...) 
{
    cat("Note: tmPlot deprecated as of version 2.0. Please use treemap instead.\n")
    invisible(treemap(...))
}




## Package Data

GNI2014 <- treemap::GNI2014		## GNI 2014 Data

business <- treemap::business		## Fictitious Business Statistics Data



## Package Info

.skeleton_package_title = "Treemap Visualization"

.skeleton_package_version = "2.4-2"

.skeleton_package_depends = ""

.skeleton_package_imports = "colorspace,data.table,ggplot2,grid,gridBase,igraph,methods,RColorBrewer,shiny"


## Internal

.skeleton_version = 5


## EOF