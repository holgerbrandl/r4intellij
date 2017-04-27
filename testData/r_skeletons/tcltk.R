##
## Exported symobls in package `tcltk`
##

## Exported package methods

.Tcl.objv <- function (objv) 
structure(.External(.C_dotTclObjv, objv), class = "tclObj")


tkwm.transient <- function (...) 
tcl("wm", "transient", ...)


tksearch <- function (widget, ...) 
tcl(widget, "search", ...)


tkwm.aspect <- function (...) 
tcl("wm", "aspect", ...)


ttkseparator <- function (parent, ...) 
tkwidget(parent, "ttk::separator", ...)


tkyposition <- function (widget, ...) 
tcl(widget, "ypositions", ...)


tkflash <- function (widget, ...) 
tcl(widget, "flash", ...)


tkcheckbutton <- function (parent, ...) 
tkwidget(parent, "checkbutton", ...)


`tclvalue<-` <- function (x, value) 
UseMethod("tclvalue<-")


tkwm.deiconify <- function (...) 
tcl("wm", "deiconify", ...)


tclvar <- structure(list(), class = "tclvar")


tkitemconfigure <- function (widget, ...) 
tcl(widget, "itemconfigure", ...)


tksize <- function (widget, ...) 
tcl(widget, "size", ...)


tkwindow.create <- function (widget, ...) 
tcl(widget, "window", "create", ...)


ttkpanedwindow <- function (parent, ...) 
tkwidget(parent, "ttk::panedwindow", ...)


tkdtag <- function (widget, ...) 
tcl(widget, "dtag", ...)


tkmark.set <- function (widget, ...) 
tcl(widget, "mark", "set", ...)


tkgrid <- function (...) 
tcl("grid", ...)


tkselection.from <- function (widget, ...) 
tcl(widget, "selection", "from", ...)


tkXselection.own <- function (...) 
tcl("selection", "own", ...)


tkselection.includes <- function (widget, ...) 
tcl(widget, "selection", "includes", ...)


tktag.bind <- function (widget, ...) 
tcl(widget, "tag", "bind", ...)


tkgrid.location <- function (...) 
tcl("grid", "location", ...)


tkimage.delete <- function (...) 
tcl("image", "delete", ...)


tkclipboard.append <- function (...) 
tcl("clipboard", "append", ...)


tkXselection.clear <- function (...) 
tcl("selection", "clear", ...)


.Tcl <- function (...) 
structure(.External(.C_dotTcl, ...), class = "tclObj")


tclObj <- function (x) 
UseMethod("tclObj")


.Tcl.args <- function (...) 
{
    pframe <- parent.frame(3)
    name2opt <- function(x) if (x != "") 
        paste0("-", x)
    else ""
    isCallback <- function(x) is.function(x) || is.call(x) || 
        is.expression(x)
    makeAtomicCallback <- function(x, e) {
        if (is.name(x)) 
            x <- eval(x, e)
        if (is.call(x)) {
            if (identical(x[[1L]], as.name("break"))) 
                return("break")
            if (identical(x[[1L]], as.name("function"))) 
                x <- eval(x, e)
        }
        .Tcl.callback(x, e)
    }
    makeCallback <- function(x, e) {
        if (is.expression(x)) 
            paste(lapply(x, makeAtomicCallback, e), collapse = ";")
        else makeAtomicCallback(x, e)
    }
    val2string <- function(x) {
        if (is.null(x)) 
            return("")
        if (is.tkwin(x)) {
            current.win <<- x
            return(.Tk.ID(x))
        }
        if (inherits(x, "tclVar")) 
            return(names(unclass(x)$env))
        if (isCallback(x)) {
            ref <- local({
                value <- x
                envir <- pframe
                environment()
            })
            callback <- makeCallback(get("value", envir = ref), 
                get("envir", envir = ref))
            callback <- paste("{", callback, "}")
            assign(callback, ref, envir = current.win$env)
            return(callback)
        }
        x <- gsub("\\\\", "\\\\\\\\", as.character(x))
        x <- gsub("\"", "\\\\\"", as.character(x))
        x <- gsub("\\[", "\\\\[", as.character(x))
        x <- gsub("\\$", "\\\\$", as.character(x))
        paste0("\"", x, "\"", collapse = " ")
    }
    val <- list(...)
    nm <- names(val)
    if (!length(val)) 
        return("")
    nm <- if (is.null(nm)) 
        rep("", length(val))
    else sapply(nm, name2opt)
    current.win <- if (exists("win", envir = parent.frame())) 
        get("win", envir = parent.frame())
    else .TkRoot
    val <- sapply(val, val2string)
    paste(as.vector(rbind(nm, val)), collapse = " ")
}


tclArray <- function () 
{
    x <- tclVar()
    tcl("unset", x)
    tcl("array", "set", x, "")
    class(x) <- c(class(x), "tclArray")
    x
}


tkxview.moveto <- function (widget, ...) 
tcl(widget, "xview", "moveto", ...)


tkStartGUI <- function () 
{
    if (.Platform$OS.type == "windows") 
        stop("The tkGUI is not available under Windows")
    tclServiceMode(FALSE)
    tcl("source", file.path(.Library, "tcltk", "exec", "console.tcl"))
    .C(.C_RTcl_ActivateConsole)
    Menu <- .Tk.newwin(".menu")
    .Tk.newwin(".tk-R.term")
    Toolbar <- .Tk.newwin(".tk-R.toolbar")
    options(pager = tkpager)
    fileMenu <- tkmenu(Menu)
    demoMenu <- tkmenu(Menu)
    packageMenu <- tkmenu(Menu)
    helpMenu <- tkmenu(Menu)
    quitMenu <- tkmenu(fileMenu)
    tkadd(Menu, "cascade", label = gettext("File"), menu = fileMenu)
    tkadd(Menu, "cascade", label = gettext("Demos"), menu = demoMenu)
    tkadd(Menu, "cascade", label = gettext("Packages"), menu = packageMenu)
    tkadd(Menu, "cascade", label = gettext("Help"), menu = helpMenu)
    tkadd(fileMenu, "command", label = gettext("Source R code"), 
        command = function() {
            f <- as.character(tkgetOpenFile())
            if (length(f)) 
                source(f)
        })
    tkadd(fileMenu, "cascade", label = gettext("Quit"), menu = quitMenu)
    tkadd(quitMenu, "command", label = gettext("Save workspace"), 
        command = quote(q("yes")))
    tkadd(quitMenu, "command", label = gettext("Don't save workspace"), 
        command = quote(q("no")))
    tkadd(demoMenu, "command", label = gettext("t test"), command = quote(demo(tkttest)))
    tkadd(demoMenu, "command", label = gettext("Density"), command = quote(demo(tkdensity)))
    tkadd(demoMenu, "command", label = gettext("Interactive linear fitting"), 
        command = quote(demo(tkcanvas)))
    tkadd(demoMenu, "command", label = gettext("R FAQ"), command = quote(demo(tkfaq)))
    loadpackageWidget <- function() {
        pkglist <- .packages(all.available = TRUE)
        lvar <- tclVar()
        tclObj(lvar) <- pkglist
        box <- tklistbox(tt <- tktoplevel(), listvariable = lvar, 
            selectmode = "multiple")
        load <- function() {
            s <- as.integer(tkcurselection(box))
            if (!length(s)) 
                return
            lapply(pkglist[s + 1L], require, character.only = TRUE)
            tkdestroy(tt)
        }
        tkpack(box)
        tkpack(tkbutton(tt, text = gettext("Load"), command = load))
    }
    CRANpackageWidget <- function() {
        CRANurl <- utils::contrib.url(getOption("repos")["CRAN"])
        l <- utils::available.packages(CRANurl)[, 1L]
        lvar <- tclVar()
        tclObj(lvar) <- l
        box <- tklistbox(tt <- tktoplevel(), listvariable = lvar, 
            selectmode = "multiple")
        gogetem <- function() {
            s <- as.integer(tkcurselection(box))
            if (!length(s)) 
                return
            utils::install.packages(l[s + 1L])
            tkdestroy(tt)
        }
        tkpack(box)
        tkpack(tkbutton(tt, text = gettext("Go get them!"), command = gogetem))
    }
    tkadd(packageMenu, "command", label = gettext("Load packages"), 
        command = loadpackageWidget)
    tkadd(packageMenu, "command", label = gettext("Install packages from CRAN"), 
        command = CRANpackageWidget)
    local({
        label <- tklabel(Toolbar, text = gettext("Help topic:"))
        txtvar <- tclVar()
        entry <- tkentry(Toolbar, textvariable = txtvar)
        showhelp <- function() {
            s <- as.character(tclObj(txtvar))[1L]
            if (!length(s)) 
                return
            nm <- as.name(s)
            print(eval(substitute(help(nm))))
            tclvalue(txtvar) <- ""
        }
        tkpack(label, side = "left")
        tkpack(entry, side = "left")
        tkbind(entry, "<Return>", showhelp)
    })
    manuals <- matrix(c("R-FAQ", "Frequently asked questions", 
        "R-intro", "An Introduction to R", "R-admin", "R Administrators Manual", 
        "R-data", "R Data Import/Export", "R-exts", "Writing R extensions", 
        "R-lang", "R Language Reference", "refman", "R Reference Manual"), 
        ncol = 2L, byrow = TRUE)
    helpPDFMenu <- tkmenu(helpMenu)
    tkadd(helpMenu, "cascade", label = gettext("Manuals in PDF format"), 
        menu = helpPDFMenu)
    pdfBase <- file.path(R.home("doc"), "manual")
    apply(manuals, 1L, function(x) {
        f <- file.path(pdfBase, paste0(x[1L], ".pdf"))
        cmd <- function() system(paste(shQuote(getOption("pdfviewer")), 
            shQuote(f)), wait = FALSE)
        tkadd(helpPDFMenu, "command", label = x[2L], command = cmd, 
            state = if (file.exists(f)) 
                "normal"
            else "disabled")
    })
    assign(".GUIenv", environment(), envir = .GlobalEnv)
    invisible(tclServiceMode(TRUE))
}


ttkcombobox <- function (parent, ...) 
tkwidget(parent, "ttk::combobox", ...)


tkselection.present <- function (widget, ...) 
tcl(widget, "selection", "present", ...)


tclputs <- function (...) 
tcl("puts", ...)


tkfraction <- function (widget, ...) 
tcl(widget, "fraction", ...)


tkselection.adjust <- function (widget, ...) 
tcl(widget, "selection", "adjust", ...)


ttkcheckbutton <- function (parent, ...) 
tkwidget(parent, "ttk::checkbutton", ...)


tclclose <- function (...) 
tcl("close", ...)


tkwm.iconify <- function (...) 
tcl("wm", "iconify", ...)


is.tkwin <- function (x) 
inherits(x, "tkwin")


tkwm.iconbitmap <- function (...) 
tcl("wm", "iconbitmap", ...)


tclServiceMode <- function (on = NULL) 
.External(.C_RTcl_ServiceMode, as.logical(on))


tkwm.resizable <- function (...) 
tcl("wm", "resizable", ...)


tkwm.focusmodel <- function (...) 
tcl("wm", "focusmodel", ...)


tkyview.scroll <- function (widget, ...) 
tcl(widget, "yview", "scroll", ...)


getTkProgressBar <- function (pb) 
{
    if (!inherits(pb, "tkProgressBar")) 
        stop(gettextf("'pb' is not from class %s", dQuote("tkProgressBar")), 
            domain = NA)
    pb$getVal()
}


tkpopup <- function (...) 
tcl("tk_popup", ...)


tkselection.clear <- function (widget, ...) 
tcl(widget, "selection", "clear", ...)


tkwm.maxsize <- function (...) 
tcl("wm", "maxsize", ...)


tkmessageBox <- function (...) 
tcl("tk_messageBox", ...)


tklower <- function (...) 
tcl("lower", ...)


tkimage.width <- function (...) 
tcl("image", "width", ...)


tkcget <- function (widget, ...) 
tcl(widget, "cget", ...)


tkpostscript <- function (widget, ...) 
tcl(widget, "postscript", ...)


tkselection.set <- function (widget, ...) 
tcl(widget, "selection", "set", ...)


tkactivate <- function (widget, ...) 
tcl(widget, "activate", ...)


tkscale <- function (parent, ...) 
tkwidget(parent, "scale", ...)


tktag.add <- function (widget, ...) 
tcl(widget, "tag", "add", ...)


tkimage.height <- function (...) 
tcl("image", "height", ...)


`tclvalue<-.default` <- function (x, value) 
{
    name <- as.character(x)
    tcl("set", name, value)
    x
}


tkXselection.handle <- function (...) 
tcl("selection", "handle", ...)


tkpack.forget <- function (...) 
tcl("pack", "forget", ...)


tkselection.to <- function (widget, ...) 
tcl(widget, "selection", "to", ...)


tkimage.names <- function (...) 
tcl("image", "names", ...)


tkfont.create <- function (...) 
tcl("font", "create", ...)


tkcompare <- function (widget, ...) 
tcl(widget, "compare", ...)


tkitemcget <- function (widget, ...) 
tcl(widget, "itemcget", ...)


tkbutton <- function (parent, ...) 
tkwidget(parent, "button", ...)


tkbell <- function (...) 
tcl("bell", ...)


tkevent.info <- function (...) 
tcl("event", "info", ...)


`tclvalue<-.tclVar` <- function (x, value) 
{
    name <- names(unclass(x)$env)
    tcl("set", name, value)
    x
}


tkwindow.cget <- function (widget, ...) 
tcl(widget, "window", "cget", ...)


tkwinfo <- function (...) 
tcl("winfo", ...)


tktitle <- function (x) 
tcl("wm", "title", x)


tktag.names <- function (widget, ...) 
tcl(widget, "tag", "names", ...)


tkbbox <- function (widget, ...) 
tcl(widget, "bbox", ...)


ttkradiobutton <- function (parent, ...) 
tkwidget(parent, "ttk::radiobutton", ...)


tk_choose.dir <- function (default = "", caption = "Select directory") 
{
    res <- tclvalue(tcl("tk_chooseDirectory", initialdir = default, 
        title = caption))
    if (nzchar(res)) 
        res
    else NA_character_
}


tkpack <- function (...) 
tcl("pack", ...)


tkwm.iconwindow <- function (...) 
tcl("wm", "iconwindow ", ...)


tkmark.next <- function (widget, ...) 
tcl(widget, "mark", "next", ...)


tksee <- function (widget, ...) 
tcl(widget, "see", ...)


tkentrycget <- function (widget, ...) 
tcl(widget, "entrycget", ...)


tkplace.forget <- function (...) 
tcl("place", "forget", ...)


tkyview <- function (widget, ...) 
tcl(widget, "yview", ...)


tkxview.scroll <- function (widget, ...) 
tcl(widget, "xview", "scroll", ...)


tkgetOpenFile <- function (...) 
tcl("tk_getOpenFile", ...)


tkwm.withdraw <- function (...) 
tcl("wm", "withdraw", ...)


`tclObj<-.tclVar` <- function (x, value) 
{
    value <- as.tclObj(value)
    .External(.C_RTcl_AssignObjToVar, names(x$env), value)
    x
}


ttkframe <- function (parent, ...) 
tkwidget(parent, "ttk::frame", ...)


tkimage.inuse <- function (...) 
tcl("image", "inuse", ...)


tkwait.variable <- function (...) 
tcl("tkwait", "variable", ...)


tktag.configure <- function (widget, ...) 
tcl(widget, "tag", "configure", ...)


tkevent.generate <- function (...) 
tcl("event", "generate", ...)


tkwm.geometry <- function (...) 
tcl("wm", "geometry", ...)


ttkmenubutton <- function (parent, ...) 
tkwidget(parent, "ttk::menubutton", ...)


tkwm.command <- function (...) 
tcl("wm", "command", ...)


tkicursor <- function (widget, ...) 
tcl(widget, "icursor", ...)


tkdestroy <- function (win) 
{
    tcl("destroy", win)
    ID <- .Tk.ID(win)
    env <- get("parent", envir = win$env)$env
    if (exists(ID, envir = env, inherits = FALSE)) 
        rm(list = ID, envir = env)
}


tkset <- function (widget, ...) 
tcl(widget, "set", ...)


tkwm.protocol <- function (...) 
tcl("wm", "protocol", ...)


tclfile.tail <- function (...) 
tcl("file", "tail", ...)


tkmark.names <- function (widget, ...) 
tcl(widget, "mark", "names", ...)


tkmessage <- function (parent, ...) 
tkwidget(parent, "message", ...)


as.tclObj <- function (x, drop = FALSE) 
{
    if (is.tclObj(x)) 
        return(x)
    z <- switch(storage.mode(x), character = .External(.C_RTcl_ObjFromCharVector, 
        x, drop), double = .External(.C_RTcl_ObjFromDoubleVector, 
        x, drop), integer = .External(.C_RTcl_ObjFromIntVector, 
        x, drop), logical = .External(.C_RTcl_ObjFromIntVector, 
        as.integer(x), drop), raw = .External(.C_RTcl_ObjFromRawVector, 
        x), stop(gettextf("cannot handle object of mode '%s'", 
        storage.mode(x)), domain = NA))
    class(z) <- "tclObj"
    z
}


`tclObj<-` <- function (x, value) 
UseMethod("tclObj<-")


tkmark.previous <- function (widget, ...) 
tcl(widget, "mark", "previous", ...)


tkevent.delete <- function (...) 
tcl("event", "delete", ...)


tkwm.iconmask <- function (...) 
tcl("wm", "iconmask", ...)


tktag.cget <- function (widget, ...) 
tcl(widget, "tag", "cget", ...)


tkfont.configure <- function (...) 
tcl("font", "configure", ...)


tkgrid.rowconfigure <- function (...) 
tcl("grid", "rowconfigure", ...)


tkgrid.remove <- function (...) 
tcl("grid", "remove", ...)


tkwm.colormapwindows <- function (...) 
tcl("wm", "colormapwindows", ...)


tktag.delete <- function (widget, ...) 
tcl(widget, "tag", "delete", ...)


tkdeselect <- function (widget, ...) 
tcl(widget, "deselect", ...)


tkpack.info <- function (...) 
tcl("pack", "info", ...)


.Tk.newwin <- function (ID) 
{
    win <- list(ID = ID, env = new.env(parent = emptyenv()))
    win$env$num.subwin <- 0
    class(win) <- "tkwin"
    win
}


.Tk.ID <- function (win) 
win$ID


tkget <- function (widget, ...) 
tcl(widget, "get", ...)


tklistbox <- function (parent, ...) 
tkwidget(parent, "listbox", ...)


tkwm.overrideredirect <- function (...) 
tcl("wm", "overrideredirect", ...)


tkpost <- function (widget, ...) 
tcl(widget, "post", ...)


tkscan.mark <- function (widget, ...) 
tcl(widget, "scan", "mark", ...)


tkinvoke <- function (widget, ...) 
tcl(widget, "invoke", ...)


tclfile.dir <- function (...) 
tcl("file", "dir", ...)


tclread <- function (...) 
tcl("read", ...)


.Tcl.callback <- function (...) 
.External(.C_dotTclcallback, ...)


tkgetSaveFile <- function (...) 
tcl("tk_getSaveFile", ...)


tkpager <- function (file, header, title, delete.file) 
{
    title <- paste(title, header)
    for (i in seq_along(file)) {
        zfile <- file[[i]]
        tt <- tktoplevel()
        tkwm.title(tt, if (length(title)) 
            title[(i - 1L)%%length(title) + 1L]
        else "")
        txt <- tktext(tt, bg = "grey90")
        scr <- tkscrollbar(tt, repeatinterval = 5, command = function(...) tkyview(txt, 
            ...))
        tkconfigure(txt, yscrollcommand = function(...) tkset(scr, 
            ...))
        tkpack(txt, side = "left", fill = "both", expand = TRUE)
        tkpack(scr, side = "right", fill = "y")
        chn <- tcl("open", zfile)
        tkinsert(txt, "end", gsub("_\b", "", tclvalue(tcl("read", 
            chn))))
        tcl("close", chn)
        tkconfigure(txt, state = "disabled")
        tkmark.set(txt, "insert", "0.0")
        tkfocus(txt)
        if (delete.file) 
            tcl("file", "delete", zfile)
    }
}


tkindex <- function (widget, ...) 
tcl(widget, "index", ...)


tkgrid.columnconfigure <- function (...) 
tcl("grid", "columnconfigure", ...)


tkgrid.propagate <- function (...) 
tcl("grid", "propagate", ...)


tkimage.types <- function (...) 
tcl("image", "types", ...)


ttklabel <- function (parent, ...) 
tkwidget(parent, "ttk::label", ...)


tkgrid.forget <- function (...) 
tcl("grid", "forget", ...)


tkXselection.get <- function (...) 
tcl("selection", "get", ...)


tkselection.anchor <- function (widget, ...) 
tcl(widget, "selection", "anchor", ...)


tkfind <- function (widget, ...) 
tcl(widget, "find", ...)


tkfont.families <- function (...) 
tcl("font", "families", ...)


tkpostcascade <- function (widget, ...) 
tcl(widget, "postcascade", ...)


tkgrab <- function (...) 
tcl("grab", ...)


tkwm.grid <- function (...) 
tcl("wm", "grid", ...)


tkwait.window <- function (...) 
tcl("tkwait", "window", ...)


tktag.nextrange <- function (widget, ...) 
tcl(widget, "tag", "nextrange", ...)


tkgrab.current <- function (...) 
tcl("grab", "current", ...)


tclopen <- function (...) 
tcl("open", ...)


tkcanvas <- function (parent, ...) 
tkwidget(parent, "canvas", ...)


tkentry <- function (parent, ...) 
tkwidget(parent, "entry", ...)


ttksizegrip <- function (parent, ...) 
tkwidget(parent, "ttk::sizegrip", ...)


tkwm.client <- function (...) 
tcl("wm", "client", ...)


tkpack.propagate <- function (...) 
tcl("pack", "propagate", ...)


tkplace.info <- function (...) 
tcl("place", "info", ...)


.TkUp <- TRUE


ttkbutton <- function (parent, ...) 
tkwidget(parent, "ttk::button", ...)


tkplace <- function (...) 
tcl("place", ...)


tkscan.dragto <- function (widget, ...) 
tcl(widget, "scan", "dragto", ...)


tkgrab.status <- function (...) 
tcl("grab", "status", ...)


tkradiobutton <- function (parent, ...) 
tkwidget(parent, "radiobutton", ...)


tktag.remove <- function (widget, ...) 
tcl(widget, "tag", "remove", ...)


tkgettags <- function (widget, ...) 
tcl(widget, "gettags", ...)


tkimage.type <- function (...) 
tcl("image", "type", ...)


tkwm.minsize <- function (...) 
tcl("wm", "minsize", ...)


tkdelta <- function (widget, ...) 
tcl(widget, "delta", ...)


tkcurselection <- function (widget, ...) 
tcl(widget, "curselection", ...)


tkfocus <- function (...) 
tcl("focus", ...)


tkidentify <- function (widget, ...) 
tcl(widget, "identify", ...)


tkProgressBar <- function (title = "R progress bar", label = "", min = 0, max = 1, 
    initial = 0, width = 300) 
{
    useText <- FALSE
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if (!have_ttk && as.character(tclRequire("PBar")) == "FALSE") 
        useText <- TRUE
    .win <- tktoplevel()
    .val <- initial
    .killed <- FALSE
    tkwm.geometry(.win, sprintf("%dx80", width + 40))
    tkwm.title(.win, title)
    fn <- tkfont.create(family = "helvetica", size = 12)
    if (useText) {
        .lab <- tklabel(.win, text = label, font = fn, padx = 20)
        tkpack(.lab, side = "left")
        fn2 <- tkfont.create(family = "helvetica", size = 16)
        .vlab <- tklabel(.win, text = "0%", font = fn2, padx = 20)
        tkpack(.vlab, side = "right")
        up <- function(value) {
            if (!is.finite(value) || value < min || value > max) 
                return()
            .val <<- value
            tkconfigure(.vlab, text = sprintf("%d%%", round(100 * 
                (value - min)/(max - min))))
        }
    }
    else {
        .lab <- tklabel(.win, text = label, font = fn, pady = 10)
        .tkval <- tclVar(0)
        tkpack(.lab, side = "top")
        tkpack(tklabel(.win, text = "", font = fn), side = "bottom")
        pBar <- if (have_ttk) 
            ttkprogressbar(.win, length = width, variable = .tkval)
        else tkwidget(.win, "ProgressBar", width = width, variable = .tkval)
        tkpack(pBar, side = "bottom")
        up <- function(value) {
            if (!is.finite(value) || value < min || value > max) 
                return()
            .val <<- value
            tclvalue(.tkval) <<- 100 * (value - min)/(max - min)
        }
    }
    getVal <- function() .val
    kill <- function() if (!.killed) {
        tkdestroy(.win)
        .killed <<- TRUE
    }
    title <- function(title) tkwm.title(.win, title)
    lab <- function(label) tkconfigure(.lab, text = label)
    tkbind(.win, "<Destroy>", kill)
    up(initial)
    structure(list(getVal = getVal, up = up, title = title, label = lab, 
        kill = kill), class = "tkProgressBar")
}


tkitemscale <- function (widget, ...) 
tcl(widget, "scale", ...)


tkfont.measure <- function (...) 
tcl("font", "measure", ...)


tktoggle <- function (widget, ...) 
tcl(widget, "toggle", ...)


tkyview.moveto <- function (widget, ...) 
tcl(widget, "yview", "moveto", ...)


tkframe <- function (parent, ...) 
tkwidget(parent, "frame", ...)


tk_select.list <- function (choices, preselect = NULL, multiple = FALSE, title = NULL) 
{
    have_ttk <- as.character(tcl("info", "tclversion")) >= "8.5"
    if (!have_ttk) 
        ttkbutton <- tkbutton
    lvar <- tclVar()
    tclObj(lvar) <- choices
    oldmode <- tclServiceMode(FALSE)
    dlg <- tktoplevel()
    tkwm.title(dlg, title)
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    if (!is.null(title) && nzchar(title)) {
        lab <- if (have_ttk) 
            ttklabel(dlg, text = title, foreground = "blue")
        else tklabel(dlg, text = title, fg = "blue")
        tkpack(lab, side = "top")
    }
    onOK <- function() {
        res <- 1L + as.integer(tkcurselection(box))
        ans.select_list <<- choices[res]
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    onCancel <- function() {
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    buttons <- tkframe(dlg)
    tkpack(buttons, side = "bottom")
    OK <- ttkbutton(buttons, text = gettext("OK"), width = 6, 
        command = onOK)
    Cancel <- ttkbutton(buttons, text = gettext("Cancel"), command = onCancel)
    tkpack(OK, Cancel, side = "left", fill = "x", padx = "2m")
    scht <- as.numeric(tclvalue(tkwinfo("screenheight", dlg))) - 
        200L
    ht <- min(length(choices), scht%/%20)
    box <- tklistbox(dlg, height = ht, listvariable = lvar, bg = "white", 
        setgrid = 1, selectmode = ifelse(multiple, "multiple", 
            "single"))
    tmp <- tcl("font", "metrics", tkcget(box, font = NULL))
    tmp <- as.numeric(sub(".*linespace ([0-9]+) .*", "\\1", tclvalue(tmp))) + 
        3
    ht <- min(length(choices), scht%/%tmp)
    tkdestroy(box)
    if (ht < length(choices)) {
        scr <- if (have_ttk) 
            ttkscrollbar(dlg, command = function(...) tkyview(box, 
                ...))
        else tkscrollbar(dlg, repeatinterval = 5, command = function(...) tkyview(box, 
            ...))
        box <- tklistbox(dlg, height = ht, width = 0, listvariable = lvar, 
            bg = "white", setgrid = 1, selectmode = ifelse(multiple, 
                "multiple", "single"), yscrollcommand = function(...) tkset(scr, 
                ...))
        tkpack(box, side = "left", fill = "both", expand = TRUE)
        tkpack(scr, side = "right", fill = "y")
    }
    else {
        box <- tklistbox(dlg, height = ht, width = 0, listvariable = lvar, 
            bg = "white", selectmode = ifelse(multiple, "multiple", 
                "single"))
        tkpack(box, side = "left", fill = "both")
    }
    preselect <- match(preselect, choices)
    preselect <- preselect[preselect > 0L] - 1L
    if (length(preselect)) {
        for (i in preselect) tkselection.set(box, i)
        tkyview(box, preselect[1L])
    }
    ans.select_list <- character()
    tkbind(dlg, "<Destroy>", onCancel)
    tkbind(box, "<Double-ButtonPress-1>", onOK)
    tkfocus(box)
    tclServiceMode(oldmode)
    tkwait.window(dlg)
    Sys.sleep(0.1)
    if (!multiple && !length(ans.select_list)) 
        ans.select_list <- ""
    ans.select_list
}


tkbindtags <- function (...) 
tcl("bindtags", ...)


tkgrid.slaves <- function (...) 
tcl("grid", "slaves", ...)


tclVar <- function (init = "") 
{
    n <- .TkRoot$env$TclVarCount <- .TkRoot$env$TclVarCount + 
        1L
    name <- paste0("::RTcl", n)
    l <- list(env = new.env())
    assign(name, NULL, envir = l$env)
    reg.finalizer(l$env, function(env) tcl("unset", names(env)))
    class(l) <- "tclVar"
    tclvalue(l) <- init
    l
}


ttkscrollbar <- function (parent, ...) 
tkwidget(parent, "ttk::scrollbar", ...)


tkconfigure <- function (widget, ...) 
tcl(widget, "configure", ...)


tkfont.names <- function (...) 
tcl("font", "names", ...)


tktag.ranges <- function (widget, ...) 
tcl(widget, "tag", "ranges", ...)


tclRequire <- function (package, warn = TRUE) 
{
    a <- tryCatch(tcl("package", "require", package), error = identity)
    if (inherits(a, "error")) {
        if (warn) 
            warning(gettextf("Tcl package '%s' not found", package), 
                domain = NA)
        return(FALSE)
    }
    else return(a)
}


tkplace.slaves <- function (...) 
tcl("place", "slaves", ...)


tkraise <- function (...) 
tcl("raise", ...)


tkwm.iconname <- function (...) 
tcl("wm", "iconname ", ...)


setTkProgressBar <- function (pb, value, title = NULL, label = NULL) 
{
    if (!inherits(pb, "tkProgressBar")) 
        stop(gettextf("'pb' is not from class %s", dQuote("tkProgressBar")), 
            domain = NA)
    oldval <- pb$getVal()
    pb$up(value)
    if (!is.null(title)) 
        pb$title(title)
    if (!is.null(label)) 
        pb$label(label)
    tcl("update", "idletasks")
    invisible(oldval)
}


tkwm.iconposition <- function (...) 
tcl("wm", "iconposition", ...)


tkgrab.set <- function (...) 
tcl("grab", "set", ...)


tkclipboard.clear <- function (...) 
tcl("clipboard", "clear", ...)


tkwm.group <- function (...) 
tcl("wm", "group", ...)


`tktitle<-` <- function (x, value) 
{
    tcl("wm", "title", x, value)
    x
}


tkbind <- function (...) 
tcl("bind", ...)


tktag.raise <- function (widget, ...) 
tcl(widget, "tag", "raise", ...)


ttknotebook <- function (parent, ...) 
tkwidget(parent, "ttk::notebook", ...)


tkinsert <- function (widget, ...) 
tcl(widget, "insert", ...)


tkdlineinfo <- function (widget, ...) 
tcl(widget, "dlineinfo", ...)


tkitemfocus <- function (widget, ...) 
tcl(widget, "focus", ...)


tkscrollbar <- function (parent, ...) 
tkwidget(parent, "scrollbar", ...)


tkwm.frame <- function (...) 
tcl("wm", "frame", ...)


tkaddtag <- function (widget, ...) 
tcl(widget, "addtag", ...)


.Tcl.args.objv <- function (...) 
{
    pframe <- parent.frame(3)
    isCallback <- function(x) is.function(x) || is.call(x) || 
        is.expression(x)
    makeAtomicCallback <- function(x, e) {
        if (is.name(x)) 
            x <- eval(x, e)
        if (is.call(x)) {
            if (identical(x[[1L]], as.name("break"))) 
                return("break")
            if (identical(x[[1L]], as.name("function"))) 
                x <- eval(x, e)
        }
        .Tcl.callback(x, e)
    }
    makeCallback <- function(x, e) {
        if (is.expression(x)) 
            paste(lapply(x, makeAtomicCallback, e), collapse = ";")
        else makeAtomicCallback(x, e)
    }
    val2obj <- function(x) {
        if (is.null(x)) 
            return(NULL)
        if (is.tkwin(x)) {
            current.win <<- x
            return(as.tclObj(.Tk.ID(x)))
        }
        if (inherits(x, "tclVar")) 
            return(as.tclObj(names(unclass(x)$env)))
        if (isCallback(x)) {
            ref <- local({
                value <- x
                envir <- pframe
                environment()
            })
            callback <- makeCallback(get("value", envir = ref), 
                get("envir", envir = ref))
            assign(callback, ref, envir = current.win$env)
            return(as.tclObj(callback, drop = TRUE))
        }
        as.tclObj(x, drop = TRUE)
    }
    val <- list(...)
    current.win <- .TkRoot
    lapply(val, val2obj)
}


tkmark.unset <- function (widget, ...) 
tcl(widget, "mark", "unset", ...)


tkplace.configure <- function (...) 
tcl("place", "configure", ...)


tkwait.visibility <- function (...) 
tcl("tkwait", "visibility", ...)


ttkspinbox <- function (parent, ...) 
tkwidget(parent, "ttk::spinbox", ...)


tknearest <- function (widget, ...) 
tcl(widget, "nearest", ...)


tkcreate <- function (widget, ...) 
tcl(widget, "create", ...)


tkmenu <- function (parent, ...) 
tkwidget(parent, "menu", ...)


tkgrab.release <- function (...) 
tcl("grab", "release", ...)


tklabel <- function (parent, ...) 
tkwidget(parent, "label", ...)


tkfont.delete <- function (...) 
tcl("font", "delete", ...)


tkgrid.size <- function (...) 
tcl("grid", "size", ...)


tkdebug <- function (widget, ...) 
tcl(widget, "debug", ...)


tkwm.title <- function (...) 
tcl("wm", "title", ...)


tkgrid.info <- function (...) 
tcl("grid", "info", ...)


tkselect <- function (widget, ...) 
tcl(widget, "select", ...)


ttkprogressbar <- function (parent, ...) 
tkwidget(parent, "ttk::progressbar", ...)


tktype <- function (widget, ...) 
tcl(widget, "type", ...)


tclvalue <- function (x) 
UseMethod("tclvalue")


ttkentry <- function (parent, ...) 
tkwidget(parent, "ttk::entry", ...)


tkdchars <- function (widget, ...) 
tcl(widget, "dchars", ...)


.TkRoot <- structure(list(ID = "", env =  "<environment>"), .Names = c("ID", 
"env"), class = "tkwin")


tkitemlower <- function (widget, ...) 
tcl(widget, "lower", ...)


tkfont.metrics <- function (...) 
tcl("font", "metrics", ...)


tkunpost <- function (widget, ...) 
tcl(widget, "unpost", ...)


tkmark.gravity <- function (widget, ...) 
tcl(widget, "mark", "gravity", ...)


tkitembind <- function (widget, ...) 
tcl(widget, "bind", ...)


tkdialog <- function (...) 
tcl("tk_dialog", ...)


tkwm.positionfrom <- function (...) 
tcl("wm", "positionfrom", ...)


tkwm.state <- function (...) 
tcl("wm", "state", ...)


tkadd <- function (widget, ...) 
tcl(widget, "add", ...)


tkselection.range <- function (widget, ...) 
tcl(widget, "selection", "range", ...)


tktext <- function (parent, ...) 
tkwidget(parent, "text", ...)


tcl <- function (...) 
.Tcl.objv(.Tcl.args.objv(...))


tktoplevel <- function (parent = .TkRoot, ...) 
{
    w <- tkwidget(parent, "toplevel", ...)
    ID <- .Tk.ID(w)
    tkbind(w, "<Destroy>", function() {
        if (exists(ID, envir = parent$env, inherits = FALSE)) 
            rm(list = ID, envir = parent$env)
        tkbind(w, "<Destroy>", "")
    })
    utils::process.events()
    w
}


ttktreeview <- function (parent, ...) 
tkwidget(parent, "ttk::treeview", ...)


tkcoords <- function (widget, ...) 
tcl(widget, "coords", ...)


tkpack.slaves <- function (...) 
tcl("pack", "slaves", ...)


tkwindow.configure <- function (widget, ...) 
tcl(widget, "window", "configure", ...)


tktag.lower <- function (widget, ...) 
tcl(widget, "tag", "lower", ...)


tclVersion <- function () 
as.character(tcl("info", "patchlevel"))


tk_choose.files <- function (default = "", caption = "Select files", multi = TRUE, 
    filters = NULL, index = 1) 
{
    args <- list("tk_getOpenFile", title = caption, multiple = multi)
    if (nzchar(default)) 
        args <- c(args, initialdir = dirname(default), initialfile = basename(default))
    if (!is.null(filters)) {
        if (!is.character(filters) || length(dim(filters)) != 
            2 || ncol(filters) != 2) 
            stop("'filters' must be a 2-column character matrix")
        f <- filters
        f[] <- paste0("{", filters, "}")
        ff <- apply(f, 1, paste, collapse = " ")
        fff <- paste0("{", ff, "}")
        args <- c(args, filetypes = paste(fff, collapse = " "))
    }
    res <- tclvalue(do.call(tcl, args))
    if (nzchar(res)) 
        if (multi) {
            ans <- character()
            pat <- "([^{])*\\{([^}]*)\\}(.*)"
            while (grepl(pat, res)) {
                ans <- c(ans, sub(pat, "\\2", res))
                res <- sub(pat, "\\1\\3", res)
            }
            ans <- c(ans, strsplit(res, " ", fixed = TRUE)[[1]])
            ans[nzchar(ans)]
        }
        else res
    else character()
}


tkpack.configure <- function (...) 
tcl("pack", "configure", ...)


tkcanvasx <- function (widget, ...) 
tcl(widget, "canvasx", ...)


tkcanvasy <- function (widget, ...) 
tcl(widget, "canvasy", ...)


tktag.prevrange <- function (widget, ...) 
tcl(widget, "tag", "prevrange", ...)


tk_messageBox <- function (type = c("ok", "okcancel", "yesno", "yesnocancel", 
    "retrycancel", "aburtretrycancel"), message, caption = "", 
    default = "", ...) 
{
    type <- match.arg(type)
    args <- list("tk_messageBox", type = type, message = message, 
        title = caption, ...)
    if (nzchar(default)) 
        args <- c(args, default = default)
    tclvalue(do.call("tcl", args))
}


tkwidget <- function (parent, type, ...) 
{
    win <- .Tk.subwin(parent)
    tcl(type, win, ...)
    win
}


tkwm.sizefrom <- function (...) 
tcl("wm", "sizefrom", ...)


tkmenubutton <- function (parent, ...) 
tkwidget(parent, "menubutton", ...)


tkgrid.bbox <- function (...) 
tcl("grid", "bbox", ...)


ttkscale <- function (parent, ...) 
tkwidget(parent, "ttk::scale", ...)


tkxview <- function (widget, ...) 
tcl(widget, "xview", ...)


tkfont.actual <- function (...) 
tcl("font", "actual", ...)


tkitemraise <- function (widget, ...) 
tcl(widget, "raise", ...)


ttklabelframe <- function (parent, ...) 
tkwidget(parent, "ttk::labelframe", ...)


tkentryconfigure <- function (widget, ...) 
tcl(widget, "entryconfigure", ...)


tkgrid.configure <- function (...) 
tcl("grid", "configure", ...)


tkmove <- function (widget, ...) 
tcl(widget, "move", ...)


tkdump <- function (widget, ...) 
tcl(widget, "dump", ...)


tkevent.add <- function (...) 
tcl("event", "add", ...)


tkimage.create <- function (...) 
tcl("image", "create", ...)


addTclPath <- function (path = ".") 
{
    if (.Platform$OS.type == "windows") 
        path <- gsub("\\\\", "/", path)
    a <- tclvalue(tcl("set", "auto_path"))
    paths <- strsplit(a, " ", fixed = TRUE)[[1L]]
    if (!path %in% paths) 
        tcl("lappend", "auto_path", path)
    invisible(paths)
}


.Tk.subwin <- function (parent) 
{
    ID <- paste(parent$ID, parent$env$num.subwin <- parent$env$num.subwin + 
        1, sep = ".")
    win <- .Tk.newwin(ID)
    assign(ID, win, envir = parent$env)
    assign("parent", parent, envir = win$env)
    win
}


tkchooseDirectory <- function (...) 
tcl("tk_chooseDirectory", ...)


is.tclObj <- function (x) 
inherits(x, "tclObj")


tkwindow.names <- function (widget, ...) 
tcl(widget, "window", "names", ...)


tkdelete <- function (widget, ...) 
tcl(widget, "delete", ...)




## Package Data

# none


## Package Info

.skeleton_package_title = "Tcl/Tk Interface"

.skeleton_package_version = "3.3.0"

.skeleton_package_depends = ""

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF