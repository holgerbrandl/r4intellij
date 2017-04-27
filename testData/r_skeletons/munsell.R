##
## Exported symobls in package `munsell`
##

## Exported package methods

saturate <- function (col, steps = 1) 
{
    col <- na.exclude(col)
    col_hvc <- mnsl2hvc(as.vector(col))
    col_hvc[, "chroma"] <- col_hvc[, "chroma"] + 2 * steps
    greys <- col_hvc[, "chroma"] <= 0
    if (any(greys)) {
        col_hvc[greys, "hue"] <- "N"
        col_hvc[greys, "chroma"] <- 0
    }
    na_handle(col, hvc2mnsl(col_hvc))
}


chroma_slice <- function (chroma.name = seq(0, 26, by = 2), back.col = "white") 
{
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    if (!all(chroma.name %in% munsell.map$chroma)) 
        stop("invalid Chroma")
    ggplot2::ggplot(ggplot2::aes(x = hue, y = value), data = subset(munsell.map, 
        chroma %in% chroma.name & hue != "N")) + ggplot2::geom_tile(ggplot2::aes(fill = hex), 
        colour = back.col) + ggplot2::geom_text(ggplot2::aes(label = name, 
        colour = text_colour(name)), angle = 45, size = 2) + 
        ggplot2::scale_colour_identity() + ggplot2::scale_x_discrete("Hue") + 
        ggplot2::scale_y_continuous("Value") + ggplot2::coord_fixed(ratio = 1/4) + 
        ggplot2::facet_wrap(~chroma) + theme_munsell(back.col) + 
        ggplot2::scale_fill_identity()
}


seq_mnsl <- function (from, to, n, fix = FALSE) 
{
    cols <- in_gamut(c(from, to), fix = fix)
    if (any(is.na(cols))) 
        stop("Colors must be in gamut")
    in.LUV <- munsell.map[match(cols, munsell.map$name), c("L", 
        "U", "V")]
    LUV.seq <- matrix(c(seq(in.LUV$L[1], in.LUV$L[2], length = n), 
        seq(in.LUV$U[1], in.LUV$U[2], length = n), seq(in.LUV$V[1], 
            in.LUV$V[2], length = n)), ncol = 3)
    rgb2mnsl(as(LUV(LUV.seq), "sRGB")@coords)
}


check_mnsl <- function (col) 
{
    col_na <- na.exclude(col)
    col <- toupper(as.vector(col_na))
    right.format <- grep("^[N]|([0-9]?.?[0-9][A-Z]{1,2})[ ][0-9]?.?[0-9]/[0-9]?.?[0-9]{1,2}$", 
        col)
    if (length(right.format) != length((col))) {
        if (length(right.format) == 0) {
            bad.cols <- paste(col, collapse = ", ")
        }
        else {
            bad.cols <- paste(col[-right.format], collapse = ", ")
        }
        stop("some of your colours are not correctly formatted:", 
            bad.cols)
    }
    hues <- gsub("[0-9 /.]", "", col)
    act.hues <- c("N", "R", "YR", "Y", "GY", "G", "BG", "B", 
        "PB", "P", "RP")
    good.hue <- hues %in% act.hues
    if (!all(good.hue)) {
        bad.hue <- paste(hues[!good.hue], "in", col[!good.hue], 
            collapse = "; ")
        act.hue.str <- paste(act.hues, collapse = ", ")
        stop("you have specified invalid hue names: ", bad.hue, 
            "\n hues should be one of ", act.hue.str)
    }
    col.split <- lapply(strsplit(col, "/"), function(x) unlist(strsplit(x, 
        " ")))
    col.split <- lapply(col.split, gsub, pattern = "[A-Z]", replacement = "")
    step <- as.numeric(sapply(col.split, "[", 1))
    values <- as.numeric(sapply(col.split, "[", 2))
    chromas <- as.numeric(sapply(col.split, "[", 3))
    act.steps <- c(seq(2.5, 10, by = 2.5), NA)
    good.step <- step %in% act.steps
    if (!all(good.step)) {
        bad.step <- paste(step[!good.step], "in", col[!good.step], 
            collapse = "; ")
        act.step.str <- paste(act.steps, collapse = ", ")
        stop("you have specified invalid hue steps: ", bad.step, 
            "\n hues steps should be one of ", act.step.str)
    }
    good.value <- values == round(values) & values <= 10 & values >= 
        0
    if (!all(good.value)) {
        bad.value <- paste(values[!good.value], "in", col[!good.value], 
            collapse = "; ")
        stop("some colours have values that are not integers between 0 and 10: ", 
            bad.value)
    }
    good.chroma <- (chromas%%2) == 0
    if (!all(good.chroma)) {
        bad.chroma <- paste(chromas[!good.chroma], "in", col[!good.chroma], 
            collapse = "; ")
        stop("some colours have chromas that are not multiples of two: ", 
            bad.chroma)
    }
    na_handle(col_na, col)
}


rygbp <- function (col, steps = 1) 
{
    col <- na.exclude(col)
    col_hvc <- mnsl2hvc(as.vector(col))
    greys <- col_hvc[, "hue"] == "N"
    inds <- match(col_hvc$hue, mnsl_hues())
    col_hvc[, "hue"] <- mnsl_hues()[((inds + steps - 1)%%40) + 
        1]
    if (any(greys)) {
        warning("Greys returned untransformed")
        col_hvc[greys, "hue"] <- "N"
    }
    na_handle(col, hvc2mnsl(col_hvc))
}


plot_hex <- function (hex.colour, back.col = "white") 
{
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    if (length(hex.colour) == 1) {
        add.ops <- list(ggplot2::geom_text(ggplot2::aes(label = names)))
    }
    else add.ops <- list(ggplot2::facet_wrap(~names))
    df <- data.frame(colour = hex.colour, names = factor(hex.colour, 
        levels = hex.colour), x = 0, y = 0)
    ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) + 
        ggplot2::geom_tile(ggplot2::aes(fill = colour)) + ggplot2::scale_fill_identity() + 
        add.ops + ggplot2::scale_x_continuous(expand = c(0, 0)) + 
        ggplot2::scale_y_continuous(expand = c(0, 0)) + ggplot2::coord_fixed(ratio = 1) + 
        theme_munsell(back.col)
}


pbgyr <- function (col, steps = 1) 
{
    rygbp(col, steps = -steps)
}


complement <- function (col, ...) 
{
    col <- na.exclude(col)
    col_hvc <- mnsl2hvc(as.vector(col))
    greys <- col_hvc[, "hue"] == "N"
    inds <- match(col_hvc$hue, mnsl_hues())
    col_hvc[, "hue"] <- mnsl_hues()[((inds + 20 - 1)%%40) + 1]
    if (any(greys)) {
        warning("Complement not defined for greys")
        col_hvc[greys, "hue"] <- "N"
    }
    na_handle(col, hvc2mnsl(col_hvc))
}


desaturate <- colorspace::desaturate # re-exported from colorspace package

mnsl2hex <- function (col, ...) 
{
    col <- check_mnsl(col, ...)
    positions <- match(col, munsell.map$name)
    munsell.map[positions, "hex"]
}


fix_mnsl <- function (col) 
{
    col.split <- lapply(strsplit(col, "/"), function(x) unlist(strsplit(x, 
        " ")))
    max.chroma <- function(colour.args) {
        hue.value <- munsell.map[munsell.map$hue == colour.args[1] & 
            munsell.map$value == colour.args[2] & !is.na(munsell.map$hex), 
            ]
        hue.value[which.max(hue.value$chroma), "name"]
    }
    unlist(lapply(col.split, max.chroma))
}


in_gamut <- function (col, fix = FALSE) 
{
    col <- na.exclude(col)
    positions <- match(col, munsell.map$name)
    hex <- munsell.map[positions, "hex"]
    if (any(is.na(hex))) {
        bad.colours <- paste(col[is.na(hex)], collapse = ", ")
        if (!fix) {
            warning("some specified colours are undefined. You could try fix = TRUE")
            col[is.na(hex)] <- NA
        }
        else {
            col[is.na(hex)] <- fix_mnsl(col[is.na(hex)])
        }
    }
    na_handle(col, as.vector(col))
}


plot_mnsl <- function (cols, back.col = "white", ...) 
{
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    add.ops <- NULL
    if (length(cols) > 1) {
        add.ops <- list(ggplot2::facet_wrap(~num))
    }
    cols <- check_mnsl(cols)
    cols <- in_gamut(cols, ...)
    df <- data.frame(num = 1:length(cols), names = factor(cols, 
        levels = c(unique(cols))), hex = mnsl2hex(cols), x = 0, 
        y = 0, stringsAsFactors = FALSE)
    df$labels <- factor(df$names, levels = c(unique(cols), "NA"))
    df$labels[is.na(df$labels)] <- "NA"
    ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) + 
        ggplot2::geom_tile(ggplot2::aes(fill = hex)) + add.ops + 
        ggplot2::geom_text(ggplot2::aes(label = labels, colour = text_colour(as.character(names)))) + 
        ggplot2::scale_x_continuous(expand = c(0, 0)) + ggplot2::scale_y_continuous(expand = c(0, 
        0)) + ggplot2::coord_fixed() + theme_munsell(back.col) + 
        ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() + 
        ggplot2::theme(strip.background = ggplot2::element_blank(), 
            strip.text = ggplot2::element_blank())
}


hvc2mnsl <- function (hue, value = NULL, chroma = NULL, ...) 
{
    if (!(is.null(value) == is.null(chroma))) 
        stop("specify both value and chroma")
    hcv <- hue
    if (!is.null(value)) {
        hcv <- cbind(hcv, value, chroma)
    }
    hcv <- na.exclude(hcv)
    selected <- paste(hcv[, 1], " ", hcv[, 2], "/", hcv[, 3], 
        sep = "")
    selected <- check_mnsl(selected, ...)
    na_handle(hcv, selected)
}


darker <- function (col, steps = 1) 
{
    lighter(col, steps = -steps)
}


lighter <- function (col, steps = 1) 
{
    col <- na.exclude(col)
    col_hvc <- mnsl2hvc(as.vector(col))
    col_hvc[, "value"] <- col_hvc[, "value"] + steps
    whites <- col_hvc[, "value"] >= 10
    blacks <- col_hvc[, "value"] <= 0
    if (any(whites | blacks)) {
        col_hvc[whites, "hue"] <- "N"
        col_hvc[whites, "value"] <- 10
        col_hvc[whites, "chroma"] <- 0
        col_hvc[blacks, "hue"] <- "N"
        col_hvc[blacks, "value"] <- 0
        col_hvc[blacks, "chroma"] <- 0
    }
    na_handle(col, hvc2mnsl(col_hvc))
}


value_slice <- function (value.name = 1:10, back.col = "white") 
{
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    if (!all(value.name %in% munsell.map$value)) 
        stop("invalid Value")
    ggplot2::ggplot(ggplot2::aes(x = hue, y = factor(chroma)), 
        data = subset(munsell.map, value %in% value.name & hue != 
            "N" & !is.na(hex))) + ggplot2::geom_tile(ggplot2::aes(fill = hex), 
        colour = back.col) + ggplot2::coord_polar() + ggplot2::scale_x_discrete("Hue") + 
        ggplot2::scale_y_discrete("Chroma") + ggplot2::facet_wrap(~value) + 
        theme_munsell(back.col) + ggplot2::scale_fill_identity()
}


mnsl2hvc <- function (col, ...) 
{
    col <- check_mnsl(col, ...)
    col <- na.exclude(col)
    if (length(col) == 0) 
        stop("zero non-missing colours")
    col.split <- lapply(strsplit(col, "/"), function(x) unlist(strsplit(x, 
        " ")))
    col_mat <- data.frame(do.call(rbind, col.split), stringsAsFactors = FALSE)
    colnames(col_mat) <- c("hue", "value", "chroma")
    col_mat[, "value"] <- as.numeric(col_mat[, "value"])
    col_mat[, "chroma"] <- as.numeric(col_mat[, "chroma"])
    na_handle(col, col_mat)
}


complement_slice <- function (hue.name, back.col = "white") 
{
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    if (length(hue.name) > 1) 
        stop("complement_slice currently only takes one hue")
    if (!hue.name %in% munsell.map$hue) 
        stop("invalid hue name")
    comp.hue <- mnsl2hvc(complement(hvc2mnsl(hue.name, 2, 2)))$hue
    munsell.sub <- subset(munsell.map, hue == hue.name | hue == 
        comp.hue)
    munsell.sub <- within(munsell.sub, {
        chroma <- ifelse(hue == comp.hue, -1, 1) * chroma
        hue <- factor(hue, levels = c(comp.hue, "N", hue.name))
    })
    ggplot2::ggplot(ggplot2::aes(x = chroma, y = value), data = munsell.sub) + 
        ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col, 
            size = 1) + ggplot2::geom_text(ggplot2::aes(label = name, 
        colour = text_colour(name)), angle = 45, size = 2) + 
        ggplot2::scale_fill_identity() + ggplot2::scale_colour_identity() + 
        ggplot2::scale_x_continuous("Chroma") + ggplot2::scale_y_continuous("Value") + 
        ggplot2::facet_grid(. ~ hue, scales = "free_x", space = "free") + 
        ggplot2::coord_fixed() + theme_munsell(back.col)
}


rgb2mnsl <- function (R, G = NULL, B = NULL) 
{
    LUV.vals <- as(sRGB(R, G, B), "LUV")@coords
    if (any(LUV.vals[, "L"] == 0)) {
        LUV.vals[LUV.vals[, "L"] == 0, ] <- 0
    }
    ncolors <- nrow(LUV.vals)
    dist.calc <- function(x, y) rowSums((rep(x, each = ncolors) - 
        y)^2)
    dists <- apply(munsell.map[, c("L", "U", "V")], 1, dist.calc, 
        y = LUV.vals)
    if (is.null(dim(dists))) 
        closest <- which.min(dists)
    else closest <- apply(dists, 1, which.min)
    munsell.map[closest, "name"]
}


mnsl <- function (col, ...) 
{
    col <- check_mnsl(col, ...)
    positions <- match(col, munsell.map$name)
    munsell.map[positions, "hex"]
}


plot_closest <- function (R, G = NULL, B = NULL, back.col = "white") 
{
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    closest <- rgb2mnsl(R, G, B)
    ncolours <- length(closest)
    rgbnames <- apply(round(sRGB(R, G, B)@coords, 2), 1, paste, 
        collapse = ", ")
    little.df <- data.frame(type = rep(c("actual", "closest"), 
        each = ncolours), hex = c(hex(sRGB(R, G, B)), mnsl2hex(closest)), 
        name = c(rgbnames, closest), x = rep(c(0, 0), each = ncolours), 
        y = rep(1:ncolours, 2), text.colour = rep(text_colour(closest), 
            2))
    ggplot2::ggplot(data = little.df, ggplot2::aes(x = x, y = y)) + 
        ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col, 
            size = 2) + ggplot2::geom_text(ggplot2::aes(label = name, 
        colour = text.colour), size = 2) + ggplot2::scale_colour_identity() + 
        ggplot2::coord_fixed(ratio = 1) + theme_munsell(back.col) + 
        ggplot2::scale_fill_identity() + ggplot2::facet_wrap(~type)
}


text_colour <- function (cols) 
{
    values <- mnsl2hvc(cols)[, "value"]
    ifelse(values > 4, "black", "white")
}


hue_slice <- function (hue.name = "all", back.col = "white") 
{
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 needed for this function to work. Please install it.", 
            call. = FALSE)
    }
    if (any(hue.name == "all")) {
        return(ggplot2::ggplot(ggplot2::aes(x = factor(chroma), 
            y = factor(value)), data = munsell.map) + ggplot2::geom_tile(ggplot2::aes(fill = hex), 
            colour = back.col) + ggplot2::facet_wrap(~hue) + 
            ggplot2::scale_x_discrete("Chroma", expand = c(0, 
                0)) + ggplot2::coord_fixed(ratio = 1) + ggplot2::scale_y_discrete("Value", 
            expand = c(0, 0)) + theme_munsell(back.col) + ggplot2::scale_fill_identity())
    }
    else {
        if (!all(hue.name %in% munsell.map$hue)) 
            stop("invalid hue names")
        ggplot2::ggplot(ggplot2::aes(x = factor(chroma), y = factor(value)), 
            data = subset(munsell.map, hue %in% hue.name)) + 
            ggplot2::geom_tile(ggplot2::aes(fill = hex), colour = back.col, 
                size = 1) + ggplot2::geom_text(ggplot2::aes(label = name, 
            colour = text_colour(name)), angle = 45, size = 2) + 
            ggplot2::scale_colour_identity() + ggplot2::scale_x_discrete("Chroma") + 
            ggplot2::scale_y_discrete("Value", expand = c(0.125, 
                0)) + theme_munsell(back.col) + ggplot2::scale_fill_identity() + 
            ggplot2::facet_wrap(~hue)
    }
}


mnsl_hues <- function () 
{
    as.character(unique(munsell.map$hue)[-1])
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Utilities for Using Munsell Colours"

.skeleton_package_version = "0.4.3"

.skeleton_package_depends = ""

.skeleton_package_imports = "colorspace,methods"


## Internal

.skeleton_version = 5


## EOF