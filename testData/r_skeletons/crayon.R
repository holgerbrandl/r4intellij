##
## Exported symobls in package `crayon`
##

## Exported package methods

has_color <- function () 
{
    enabled <- getOption("crayon.enabled")
    if (!is.null(enabled)) {
        return(isTRUE(enabled))
    }
    if (!isatty(stdout())) {
        return(FALSE)
    }
    if (.Platform$OS.type == "windows") {
        if (Sys.getenv("ConEmuANSI") == "ON") {
            return(TRUE)
        }
        if (Sys.getenv("CMDER_ROOT") != "") {
            return(TRUE)
        }
        return(FALSE)
    }
    if (inside_emacs() && emacs_version()[1] >= 23) {
        return(TRUE)
    }
    if ("COLORTERM" %in% names(Sys.getenv())) {
        return(TRUE)
    }
    if (Sys.getenv("TERM") == "dumb") {
        return(FALSE)
    }
    grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux", Sys.getenv("TERM"), 
        ignore.case = TRUE, perl = TRUE)
}


col_nchar <- function (x, ...) 
{
    base::nchar(strip_style(x), ...)
}


chr <- function (x, ...) 
as.character(x, ...)


bgBlack <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bgBlack = structure(list(open = "\033[40m", 
    close = "\033[49m"), .Names = c("open", "close"))), .Names = "bgBlack"), class = "crayon")


combine_styles <- function (...) 
{
    styles <- lapply(list(...), use_or_make_style)
    all_ansi <- unlist(lapply(styles, attr, "_styles"), recursive = FALSE)
    make_crayon(all_ansi)
}


bgRed <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bgRed = structure(list(open = "\033[41m", 
    close = "\033[49m"), .Names = c("open", "close"))), .Names = "bgRed"), class = "crayon")


white <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(white = structure(list(open = "\033[37m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "white"), class = "crayon")


underline <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(underline = structure(list(open = "\033[4m", 
    close = "\033[24m"), .Names = c("open", "close"))), .Names = "underline"), class = "crayon")


has_style <- function (string) 
{
    grepl(ansi_regex, string, perl = TRUE)
}


bgWhite <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bgWhite = structure(list(open = "\033[47m", 
    close = "\033[49m"), .Names = c("open", "close"))), .Names = "bgWhite"), class = "crayon")


drop_style <- function (style) 
{
    my_styles[[style]] <<- NULL
    invisible()
}


bgCyan <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bgCyan = structure(list(open = "\033[46m", 
    close = "\033[49m"), .Names = c("open", "close"))), .Names = "bgCyan"), class = "crayon")


magenta <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(magenta = structure(list(open = "\033[35m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "magenta"), class = "crayon")


col_substr <- function (x, start, stop) 
{
    if (!is.character(x)) 
        x <- as.character(x)
    if (!length(x)) 
        return(x)
    start <- as.integer(start)
    stop <- as.integer(stop)
    if (!length(start) || !length(stop)) 
        stop("invalid substring arguments")
    if (anyNA(start) || anyNA(stop)) 
        stop("non-numeric substring arguments not supported")
    ansi <- re_table(ansi_regex, x)
    text <- non_matching(ansi, x, empty = TRUE)
    mapper <- map_to_ansi(x, text = text)
    nstart <- mapper(start)
    nstop <- mapper(stop)
    bef <- base::substr(x, 1, nstart - 1)
    aft <- base::substr(x, nstop + 1, base::nchar(x))
    ansi_bef <- vapply(regmatches(bef, gregexpr(ansi_regex, bef)), 
        paste, collapse = "", FUN.VALUE = "")
    ansi_aft <- vapply(regmatches(aft, gregexpr(ansi_regex, aft)), 
        paste, collapse = "", FUN.VALUE = "")
    paste(sep = "", ansi_bef, base::substr(x, nstart, nstop), 
        ansi_aft)
}


styles <- function () 
{
    my_styles
}


reset <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(reset = structure(list(open = "\033[0m", 
    close = "\033[0m"), .Names = c("open", "close"))), .Names = "reset"), class = "crayon")


bold <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bold = structure(list(open = "\033[1m", 
    close = "\033[22m"), .Names = c("open", "close"))), .Names = "bold"), class = "crayon")


col_strsplit <- function (x, split, ...) 
{
    split <- try(as.character(split), silent = TRUE)
    if (inherits(split, "try-error") || !is.character(split) || 
        length(split) > 1L) 
        stop("`split` must be character of length <= 1, or must coerce to that")
    if (!length(split)) 
        split <- ""
    plain <- strip_style(x)
    splits <- re_table(split, plain, ...)
    chunks <- non_matching(splits, plain, empty = TRUE)
    split.r <- rep(split, length.out = length(x))
    chunks <- lapply(seq_along(chunks), function(i) {
        y <- chunks[[i]]
        if (nrow(y) && !nzchar(split.r[[i]]) && !head(y, 1L)[, 
            "length"]) {
            y <- y[-1L, , drop = FALSE]
        }
        if (nrow(y) && !tail(y, 1L)[, "length"]) 
            y[-nrow(y), , drop = FALSE]
        else y
    })
    zero.chunks <- !vapply(chunks, nrow, integer(1L))
    res <- vector("list", length(chunks))
    res[zero.chunks] <- list(character(0L))
    res[!zero.chunks] <- mapply(chunks[!zero.chunks], x[!zero.chunks], 
        SIMPLIFY = FALSE, FUN = function(tab, xx) col_substring(xx, 
            tab[, "start"], tab[, "end"]))
    res
}


bgBlue <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bgBlue = structure(list(open = "\033[44m", 
    close = "\033[49m"), .Names = c("open", "close"))), .Names = "bgBlue"), class = "crayon")


bgGreen <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bgGreen = structure(list(open = "\033[42m", 
    close = "\033[49m"), .Names = c("open", "close"))), .Names = "bgGreen"), class = "crayon")


green <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(green = structure(list(open = "\033[32m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "green"), class = "crayon")


col_substring <- function (text, first, last = 1000000L) 
{
    if (!is.character(text)) 
        text <- as.character(text)
    n <- max(lt <- length(text), length(first), length(last))
    if (lt && lt < n) 
        text <- rep_len(text, length.out = n)
    col_substr(text, as.integer(first), as.integer(last))
}


finish <- function (x, ...) 
UseMethod("finish")


strikethrough <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(strikethrough = structure(list(
    open = "\033[9m", close = "\033[29m"), .Names = c("open", 
"close"))), .Names = "strikethrough"), class = "crayon")


bgMagenta <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bgMagenta = structure(list(open = "\033[45m", 
    close = "\033[49m"), .Names = c("open", "close"))), .Names = "bgMagenta"), class = "crayon")


black <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(black = structure(list(open = "\033[30m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "black"), class = "crayon")


num_colors <- function (forget = FALSE) 
{
    if (forget || is.null(cache)) 
        cache <<- i_num_colors()
    cache
}


show_ansi_colors <- function (colors = num_colors()) 
{
    if (colors < 8) {
        cat("Colors are not supported")
    }
    else if (colors < 256) {
        cat(ansi_colors_8, sep = "")
        invisible(ansi_colors_8)
    }
    else {
        cat(ansi_colors_256, sep = "")
        invisible(ansi_colors_256)
    }
}


red <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(red = structure(list(open = "\033[31m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "red"), class = "crayon")


strip_style <- function (string) 
{
    gsub(ansi_regex, "", string, perl = TRUE)
}


style <- function (string, as = NULL, bg = NULL) 
{
    as <- use_or_make_style(as)
    bg <- use_or_make_style(bg, bg = TRUE)
    if (!is(as, "crayon")) 
        stop("Cannot make style from 'as'")
    if (!is(bg, "crayon")) 
        stop("Cannot make style from 'bg'")
    as(bg(string))
}


cyan <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(cyan = structure(list(open = "\033[36m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "cyan"), class = "crayon")


yellow <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(yellow = structure(list(open = "\033[33m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "yellow"), class = "crayon")


bgYellow <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(bgYellow = structure(list(open = "\033[43m", 
    close = "\033[49m"), .Names = c("open", "close"))), .Names = "bgYellow"), class = "crayon")


silver <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(silver = structure(list(open = "\033[90m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "silver"), class = "crayon")


blurred <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(blurred = structure(list(open = "\033[2m", 
    close = "\033[22m"), .Names = c("open", "close"))), .Names = "blurred"), class = "crayon")


blue <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(blue = structure(list(open = "\033[34m", 
    close = "\033[39m"), .Names = c("open", "close"))), .Names = "blue"), class = "crayon")


make_style <- function (..., bg = FALSE, grey = FALSE, colors = num_colors()) 
{
    args <- list(...)
    stopifnot(length(args) == 1)
    style <- args[[1]]
    orig_style_name <- style_name <- names(args)[1]
    stopifnot(is.character(style) && length(style) == 1 || is_rgb_matrix(style) && 
        ncol(style) == 1, is.logical(bg) && length(bg) == 1, 
        is.numeric(colors) && length(colors) == 1)
    ansi_seqs <- if (is_builtin_style(style)) {
        if (bg && substr(style, 1, 2) != "bg") {
            style <- "bg" %+% capitalize(style)
        }
        if (is.null(style_name)) 
            style_name <- style
        builtin_styles[[style]]
    }
    else if (is_r_color(style)) {
        if (is.null(style_name)) 
            style_name <- style
        style_from_r_color(style, bg, colors, grey)
    }
    else if (is_rgb_matrix(style)) {
        style_from_rgb(style, bg, colors, grey)
    }
    else {
        stop("Unknown style specification: ", style)
    }
    if (!is.null(orig_style_name)) 
        define_style(orig_style_name, ansi_seqs)
    make_crayon(structure(list(ansi_seqs), names = style_name))
}


hidden <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(hidden = structure(list(open = "\033[8m", 
    close = "\033[28m"), .Names = c("open", "close"))), .Names = "hidden"), class = "crayon")


italic <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(italic = structure(list(open = "\033[3m", 
    close = "\033[23m"), .Names = c("open", "close"))), .Names = "italic"), class = "crayon")


`%+%` <- function (lhs, rhs) 
{
    stopifnot(is.character(lhs), is.character(rhs))
    stopifnot(length(lhs) == length(rhs) || length(lhs) == 1 || 
        length(rhs) == 1)
    if (length(lhs) == 0 && length(rhs) == 0) {
        paste0(lhs, rhs)
    }
    else if (length(lhs) == 0) {
        lhs
    }
    else if (length(rhs) == 0) {
        rhs
    }
    else {
        paste0(lhs, rhs)
    }
}


inverse <- structure(function (...) 
{
    my_styles <- attr(sys.function(), "_styles")
    text <- mypaste(...)
    if (has_color()) {
        for (st in rev(my_styles)) {
            text <- st$open %+% gsub(st$close, st$open, text, 
                fixed = TRUE) %+% st$close
        }
    }
    text
}, "`_styles`" = structure(list(inverse = structure(list(open = "\033[7m", 
    close = "\033[27m"), .Names = c("open", "close"))), .Names = "inverse"), class = "crayon")




## Package Data

# none


## Package Info

.skeleton_package_title = "Colored Terminal Output"

.skeleton_package_version = "1.3.2"

.skeleton_package_depends = ""

.skeleton_package_imports = "grDevices,methods,utils"


## Internal

.skeleton_version = 5


## EOF