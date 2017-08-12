##
## Exported symobls in package `knitr`
##

## Exported package methods

pat_rnw <- function () 
set_pattern("rnw")


fig_chunk <- function (label, ext = "", number, fig.path = opts_chunk$get("fig.path")) 
{
    fig_path(ext, list(fig.path = fig.path, label = label), number)
}


knit_params_yaml <- function (yaml, evaluate = TRUE) 
{
    parsed_yaml = yaml::yaml.load(yaml, handlers = knit_params_handlers(evaluate = evaluate))
    if (is.list(parsed_yaml) && !is.null(parsed_yaml$params)) {
        resolve_params(parsed_yaml$params, evaluate = evaluate)
    }
    else {
        list()
    }
}


stitch_rhtml <- function (..., envir = parent.frame()) 
stitch(..., envir = envir, template = system.file("misc", "knitr-template.Rhtml", 
    package = "knitr"))


hook_plot_asciidoc <- function (x, options) 
{
    base = opts_knit$get("base.url") %n% ""
    cap = .img.cap(options)
    width = sprintf("width=%s", options$out.width)
    height = sprintf("height=%s", options$out.height)
    align = sprintf("align=%s", options$fig.align)
    tags = paste(c(cap, width, height, align), collapse = ",")
    sprintf(".%s\nimage::%s%s[%s]\n", cap, base, .upload.url(x), 
        tags)
}


knit_print <- function (x, ...) 
{
    if (need_screenshot(x, ...)) {
        html_screenshot(x)
    }
    else {
        UseMethod("knit_print")
    }
}


render_html <- function () 
{
    set_html_dev()
    opts_knit$set(out.format = "html")
    html.hook = function(name) {
        force(name)
        function(x, options) {
            x = if (name == "source") {
                c(hilight_source(x, "html", options), "")
            }
            else escape_html(x)
            x = paste(x, collapse = "\n")
            sprintf("<div class=\"%s\"><pre class=\"knitr %s\">%s</pre></div>\n", 
                name, tolower(options$engine), x)
        }
    }
    h = opts_knit$get("header")
    if (!nzchar(h["highlight"])) 
        set_header(highlight = .header.hi.html)
    z = list()
    for (i in c("source", "warning", "message", "error")) z[[i]] = html.hook(i)
    knit_hooks$set(z)
    knit_hooks$set(inline = function(x) {
        sprintf(if (inherits(x, "AsIs")) 
            "%s"
        else "<code class=\"knitr inline\">%s</code>", .inline.hook(format_sci(x, 
            "html")))
    }, output = html.hook("output"), plot = hook_plot_html, chunk = .chunk.hook.html)
}


opts_template <- structure(list(get = function (name, default = FALSE, drop = TRUE) 
{
    if (default) 
        defaults = value
    if (missing(name)) 
        defaults
    else {
        if (drop && length(name) == 1) 
            defaults[[name]]
        else {
            setNames(defaults[name], name)
        }
    }
}, set = function (...) 
{
    dots = list(...)
    if (length(dots) == 0) 
        return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) 
        if (length(dots <- dots[[1]]) == 0) 
            return()
    defaults <<- merge(dots)
    invisible(NULL)
}, merge = function (values) 
merge_list(defaults, values), restore = function (target = value) 
defaults <<- target), .Names = c("get", "set", "merge", "restore"
))


combine_words <- function (words, sep = ", ", and = " and ", before = "", after = before) 
{
    n = length(words)
    if (n == 0) 
        return(words)
    words = paste0(before, words, after)
    if (n == 1) 
        return(words)
    if (n == 2) 
        return(paste(words, collapse = and))
    if (grepl("^ ", and) && grepl(" $", sep)) 
        and = gsub("^ ", "", and)
    words[n] = paste0(and, words[n])
    paste(words, collapse = sep)
}


imgur_upload <- function (file, key = "9f3460e67f308f6") 
{
    if (!is.character(key)) 
        stop("The Imgur API Key must be a character string!")
    res = RCurl::postForm("https://api.imgur.com/3/image.xml", 
        image = RCurl::fileUpload(file), .opts = RCurl::curlOptions(httpheader = c(Authorization = paste("Client-ID", 
            key)), cainfo = system.file("CurlSSL", "cacert.pem", 
            package = "RCurl")))
    res = XML::xmlToList(res)
    if (is.null(res$link)) 
        stop("failed to upload ", file)
    structure(res$link, XML = res)
}


write_bib <- function (x = .packages(), file = "", tweak = TRUE, width = NULL, 
    prefix = getOption("knitr.bib.prefix", "R-")) 
{
    idx = mapply(system.file, package = x) == ""
    if (any(idx)) {
        warning("package(s) ", paste(x[idx], collapse = ", "), 
            " not found")
        x = x[!idx]
    }
    x = setdiff(x, .base.pkgs)
    bib = sapply(x, function(pkg) {
        cite = citation(pkg, auto = if (pkg == "base") 
            NULL
        else TRUE)
        if (tweak) {
            cite$title = gsub(sprintf("^(%s: )(\\1)", pkg), "\\1", 
                cite$title)
            cite$title = gsub(" & ", " \\\\& ", cite$title)
        }
        entry = toBibtex(cite)
        entry[1] = sub("\\{,$", sprintf("{%s%s,", prefix, pkg), 
            entry[1])
        entry
    }, simplify = FALSE)
    if (tweak) {
        for (i in intersect(names(.tweak.bib), x)) {
            message("tweaking ", i)
            bib[[i]] = merge_list(bib[[i]], .tweak.bib[[i]])
        }
        bib = lapply(bib, function(b) {
            b["author"] = sub("Duncan Temple Lang", "Duncan {Temple Lang}", 
                b["author"])
            b["title"] = sub("'RStudio'", "RStudio", b["title"])
            if (!("year" %in% names(b))) 
                b["year"] = .this.year
            idx = which(names(b) == "")
            if (!is.null(width)) 
                b[-idx] = stringr::str_wrap(b[-idx], width, 2, 
                  4)
            structure(c(b[idx[1L]], b[-idx], b[idx[2L]]), class = "Bibtex")
        })
    }
    bib = bib[sort(x)]
    if (!is.null(file) && length(x)) 
        writeUTF8(unlist(bib), file)
    invisible(bib)
}


hook_movecode <- function (x) 
{
    x = split_lines(x)
    res = split(x, cumsum(grepl("^\\\\(begin|end)\\{figure\\}", 
        x)))
    x = split_lines(unlist(lapply(res, function(p) {
        if (length(p) <= 4 || !grepl("^\\\\begin\\{figure\\}", 
            p[1]) || length(grep("% knitr_do_not_move", p)) || 
            !any(grepl("\\\\begin\\{(alltt|kframe)\\}", p))) 
            return(p)
        idx = c(1, grep("\\\\includegraphics", p))
        if (length(idx) <= 1) 
            return(p)
        if (length(i <- grep("\\{\\\\centering.*\\\\includegraphics", 
            p))) {
            idx = c(idx, i - 1, j2 <- i + 1)
            for (j in j2) {
                while (p[j] != "}") idx = c(idx, j <- j + 1)
            }
        }
        if (length(i <- grep("\\\\hfill\\{\\}.*\\\\includegraphics", 
            p))) 
            idx = c(idx, i - 1, i + 1)
        if (length(i <- grep("\\\\includegraphics.*\\\\hfill\\{\\}", 
            p))) 
            idx = c(idx, i - 1, i + 1)
        idx = sort(c(idx, seq(grep("\\\\caption", p), grep("\\\\label", 
            p))))
        idx = unique(idx)
        p = paste(c(p[-idx], p[idx]), collapse = "\n")
        gsub("\\\\end\\{(kframe)\\}\\s*\\\\begin\\{\\1\\}", "", 
            p)
    }), use.names = FALSE))
    res = split(x, cumsum(grepl("^\\\\(begin|end)\\{table\\}", 
        x)))
    res = paste(unlist(lapply(res, function(p) {
        if (length(p) <= 4 || !grepl("^\\\\begin\\{table\\}", 
            p[1]) || length(grep("% knitr_do_not_move", p)) || 
            !any(grepl("\\\\begin\\{(alltt|kframe)\\}", p))) 
            return(p)
        if (!any(grepl("\\\\label\\{.*\\}", p))) 
            return(p)
        idx = c(1, seq(grep("\\\\caption", p), grep("\\\\label", 
            p)))
        i0 = grep("\\\\begin\\{tabular\\}", p)
        i1 = grep("\\\\end\\{tabular\\}", p)
        for (i in seq_along(i0)) idx = c(idx, i0[i]:i1[i])
        idx = sort(idx)
        p = paste(c(p[-idx], p[idx]), collapse = "\n")
        gsub("\\\\end\\{(kframe)\\}\\s*\\\\begin\\{\\1\\}", "", 
            p)
    }), use.names = FALSE), collapse = "\n")
    .rm.empty.envir(res)
}


pat_asciidoc <- function () 
set_pattern("asciidoc")


knit_patterns <- structure(list(get = function (name, default = FALSE, drop = TRUE) 
{
    if (default) 
        defaults = value
    if (missing(name)) 
        defaults
    else {
        if (drop && length(name) == 1) 
            defaults[[name]]
        else {
            setNames(defaults[name], name)
        }
    }
}, set = function (...) 
{
    dots = list(...)
    if (length(dots) == 0) 
        return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) 
        if (length(dots <- dots[[1]]) == 0) 
            return()
    defaults <<- merge(dots)
    invisible(NULL)
}, merge = function (values) 
merge_list(defaults, values), restore = function (target = value) 
defaults <<- target), .Names = c("get", "set", "merge", "restore"
))


stitch <- function (script, template = system.file("misc", "knitr-template.Rnw", 
    package = "knitr"), output = NULL, text = NULL, envir = parent.frame()) 
{
    lines = if (nosrc <- is.null(text)) 
        readLines(script, warn = FALSE)
    else split_lines(text)
    if (comment_to_var(lines[1L], ".knitr.title", "^#+ *title:", 
        envir)) 
        lines = lines[-1L]
    if (comment_to_var(lines[1L], ".knitr.author", "^#+ *author:", 
        envir)) 
        lines = lines[-1L]
    input = basename(template)
    input = sub_ext(basename(if (nosrc) 
        script
    else tempfile()), file_ext(input))
    txt = readLines(template, warn = FALSE)
    i = grep("%sCHUNK_LABEL_HERE", txt)
    if (length(i) != 1L) 
        stop("Wrong template for stitch: ", template)
    h = sub("CHUNK_LABEL_HERE", "", txt[i])
    j = grep(.sep.label, lines)
    if (length(j) == 0) {
        lines = c(sprintf(h, "auto-report"), lines)
    }
    else {
        lines[j] = sprintf(h, gsub(.sep.label, "\\2", lines[j]))
        if (j[1] != 1L) 
            lines = c(sprintf(h, ""), lines)
    }
    txt[i] = paste(lines, collapse = "\n")
    opts_chunk$set(fig.align = "center", par = TRUE, fig.width = 6, 
        fig.height = 6, fig.path = paste("figure", gsub("[^[:alnum:]]", 
            "-", input), sep = "/"))
    on.exit(opts_chunk$restore(), add = TRUE)
    knit_hooks$set(par = function(before, options, envir) {
        if (before) 
            par(mar = c(4, 4, 0.1, 0.1), cex.lab = 0.95, cex.axis = 0.9, 
                mgp = c(2, 0.7, 0), tcl = -0.3, las = 1)
    })
    on.exit(knit_hooks$restore(), add = TRUE)
    out = knit(input, output, envir = envir, text = txt)
    switch(file_ext(out), tex = {
        tools::texi2pdf(out, clean = TRUE)
        message("PDF output at: ", sub_ext(out, "pdf"))
    }, md = {
        out.html = sub_ext(out, "html")
        markdown::markdownToHTML(out, out.html)
        message("HTML output at: ", out.html)
    })
    out
}


set_header <- function (...) 
{
    opts_knit$set(header = merge_list(opts_knit$get("header"), 
        c(...)))
}


pat_md <- function () 
set_pattern("md")


pat_rst <- function () 
set_pattern("rst")


pat_html <- function () 
set_pattern("html")


engine_output <- function (options, code, out, extra = NULL) 
{
    if (!is.logical(options$echo)) 
        code = code[options$echo]
    if (length(code) != 1L) 
        code = paste(code, collapse = "\n")
    if (options$engine == "sas" && length(out) > 1L && !grepl("[[:alnum:]]", 
        out[2])) 
        out = tail(out, -3L)
    if (length(out) != 1L) 
        out = paste(out, collapse = "\n")
    out = sub("([^\n]+)$", "\\1\n", out)
    options$engine = switch(options$engine, mysql = "sql", node = "javascript", 
        psql = "sql", Rscript = "r", options$engine)
    if (options$engine == "stata") {
        out = gsub("\n\nrunning.*profile.do", "", out)
        out = sub("...\n\n\n", "", out)
        out = sub("\n. \nend of do-file\n", "", out)
    }
    paste(c(if (length(options$echo) > 1L || options$echo) (knit_hooks$get("source"))(code, 
        options), if (options$results != "hide" && !is_blank(out)) {
        if (options$engine == "highlight") out else wrap.character(out, 
            options)
    }, extra), collapse = "\n")
}


knit_hooks <- structure(list(get = function (name, default = FALSE, drop = TRUE) 
{
    if (default) 
        defaults = value
    if (missing(name)) 
        defaults
    else {
        if (drop && length(name) == 1) 
            defaults[[name]]
        else {
            setNames(defaults[name], name)
        }
    }
}, set = function (...) 
{
    dots = list(...)
    if (length(dots) == 0) 
        return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) 
        if (length(dots <- dots[[1]]) == 0) 
            return()
    defaults <<- merge(dots)
    invisible(NULL)
}, merge = function (values) 
merge_list(defaults, values), restore = function (target = value) 
defaults <<- target), .Names = c("get", "set", "merge", "restore"
))


read_rforge <- function (path, project, extra = "") 
{
    base = "http://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg"
    readLines(sprintf("%s/%s?root=%s%s", base, path, project, 
        extra))
}


knit2pdf <- function (input, output = NULL, compiler = NULL, envir = parent.frame(), 
    quiet = FALSE, encoding = getOption("encoding"), ...) 
{
    out = knit(input, output = output, envir = envir, quiet = quiet, 
        encoding = encoding)
    owd = setwd(dirname(out))
    on.exit(setwd(owd))
    if (is.null(compiler) && grepl("\\.rst$", out)) 
        compiler = "rst2pdf"
    if (!is.null(compiler)) {
        if (compiler == "rst2pdf") {
            if (tolower(file_ext(out)) != "rst") 
                stop("for rst2pdf compiler input must be a .rst file")
            rst2pdf(basename(out), ...)
            return(sub_ext(out, "pdf"))
        }
        else {
            oc = Sys.getenv("PDFLATEX", NA)
            on.exit(if (is.na(oc)) Sys.unsetenv("PDFLATEX") else Sys.setenv(PDFLATEX = oc), 
                add = TRUE)
            Sys.setenv(PDFLATEX = compiler)
        }
    }
    tools::texi2pdf(basename(out), ...)
    sub_ext(out, "pdf")
}


wrap_rmd <- function (file, width = 80, text = NULL, backup) 
{
    x = if (is.null(text)) 
        readLines(file, warn = FALSE)
    else split_lines(text)
    x = strip_white(x)
    if ((n <- length(x)) <= 1L) 
        return(x)
    idx = NULL
    i = grep("^---$", x)
    if (length(i) > 1 && i[1L] == 1L) 
        idx = c(idx, i[1L]:i[2L])
    i = grep("^(```|\\{% (end|)highlight [a-z ]*%\\}|</?script.*>)", 
        x)
    if (length(i)) {
        if (length(i)%%2L != 0L) 
            stop("markers for code blocks must be paired up")
        idx = c(idx, unlist(apply(matrix(i, ncol = 2L, byrow = TRUE), 
            1L, function(z) z[1L]:z[2L])))
    }
    idx = c(idx, grep("^(#|===|---|    |\t)", x))
    idx = c(idx, grep("^\\s*$", x))
    idx = c(idx, grep("^\\s*( |> |- |\\* |\\d+ )", x))
    idx = unique(idx)
    if (length(idx) == n) 
        return(x)
    i = logical(n)
    i[idx] = TRUE
    r = rle(i)
    n = length(r$lengths)
    txt = vector("list", n)
    j = c(0L, cumsum(r$lengths))
    for (i in seq_len(n)) {
        block = x[seq(j[i] + 1L, j[i + 1])]
        txt[[i]] = if (r$value[i]) {
            gsub("\\s+$", "", block)
        }
        else {
            strwrap(paste(block, collapse = "\n"), width)
        }
    }
    txt = unlist(txt)
    if (is.null(text)) {
        if (missing(backup)) 
            backup = file.path(dirname(file), paste0("__", basename(file)))
        if (!is.null(backup)) 
            file.copy(file, backup, overwrite = TRUE)
        writeLines(txt, file)
    }
    else txt
}


pat_textile <- function () 
set_pattern("textile")


set_parent <- function (parent) 
{
    if (child_mode()) 
        return(invisible(NULL))
    opts_knit$set(parent = TRUE)
    set_preamble(readLines(parent, warn = FALSE))
    invisible(NULL)
}


hook_pdfcrop <- function (before, options, envir) 
{
    ext = options$fig.ext
    if (options$dev == "tikz" && options$external) 
        ext = "pdf"
    if (before || (fig.num <- options$fig.num %n% 0L) == 0L) 
        return()
    paths = all_figs(options, ext, fig.num)
    in_base_dir(for (f in paths) plot_crop(f))
}


opts_knit <- structure(list(get = function (name, default = FALSE, drop = TRUE) 
{
    if (default) 
        defaults = value
    if (missing(name)) 
        defaults
    else {
        if (drop && length(name) == 1) 
            defaults[[name]]
        else {
            setNames(defaults[name], name)
        }
    }
}, set = function (...) 
{
    dots = list(...)
    if (length(dots) == 0) 
        return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) 
        if (length(dots <- dots[[1]]) == 0) 
            return()
    defaults <<- merge(dots)
    invisible(NULL)
}, merge = function (values) 
merge_list(defaults, values), restore = function (target = value) 
defaults <<- target), .Names = c("get", "set", "merge", "restore"
))


read_chunk <- function (path, lines = readLines(path, warn = FALSE), labels = NULL, 
    from = NULL, to = NULL, from.offset = 0L, to.offset = 0L) 
{
    if (!length(lines)) {
        warning("code is empty")
        return(invisible())
    }
    lab = .sep.label
    if (is.null(labels)) {
        if (!group_pattern(lab)) 
            return(invisible())
    }
    else {
        if (is.null(from)) 
            from = 1L
        if (!is.numeric(from)) 
            from = pattern_index(from, lines)
        if (is.null(to)) 
            to = c(from[-1L] - 1L, length(lines))
        if (!is.numeric(to)) 
            to = pattern_index(to, lines)
        stopifnot(length(labels) == length(from), length(from) == 
            length(to))
        from = from + from.offset
        to = to + to.offset
        code = list()
        for (i in seq_along(labels)) {
            code[[labels[i]]] = strip_white(lines[from[i]:to[i]])
        }
        knit_code$set(code)
        return(invisible())
    }
    idx = cumsum(grepl(lab, lines))
    if (idx[1] == 0) {
        idx = c(0, idx)
        lines = c("", lines)
    }
    groups = unname(split(lines, idx))
    labels = stringr::str_trim(gsub(lab, "\\2", sapply(groups, 
        `[`, 1)))
    labels = gsub(",.*", "", labels)
    code = lapply(groups, strip_chunk)
    for (i in which(!nzchar(labels))) labels[i] = unnamed_chunk()
    knit_code$set(setNames(code, labels))
}


hook_plot_html <- function (x, options) 
{
    fig.num = options$fig.num = options$fig.num %n% 1L
    fig.cur = options$fig.cur %n% 1L
    if (options$fig.show == "animate") {
        return(if (fig.cur < fig.num) "" else (opts_knit$get("animation.fun"))(x, 
            options))
    }
    ai = options$fig.show == "asis"
    plot1 = ai || fig.cur <= 1L
    plot2 = ai || fig.cur == fig.num
    d1 = if (plot1) 
        paste0(if (out_format("html")) 
            "</div>", sprintf("<div class=\"rimage %s\">", options$fig.align))
    d2 = if (plot2) 
        paste0("</div>", if (out_format("html")) 
            "<div class=\"rcode\">")
    paste0(d1, .img.tag(.upload.url(x), options$out.width, options$out.height, 
        .img.cap(options), paste(c(options$out.extra, "class=\"plot\""), 
            collapse = " ")), d2, "\n")
}


purl <- function (..., documentation = 1L) 
{
    doc = opts_knit$get("documentation")
    on.exit(opts_knit$set(documentation = doc))
    opts_knit$set(documentation = documentation)
    knit(..., tangle = TRUE)
}


rst2pdf <- function (input, command = "rst2pdf", options = "") 
{
    out = sub_ext(input, "pdf")
    system2(command, paste(shQuote(input), "-o", shQuote(out), 
        options))
    if (file.exists(out)) 
        out
    else stop("conversion by rst2pdf failed!")
}


hook_r2swf <- function (x, options) 
{
    x = c(sans_ext(x), file_ext(x))
    fig.num = options$fig.num
    fig.name = paste0(sub(paste0(fig.num, "$"), "", x[1]), 1:fig.num, 
        ".", x[2])
    swf.name = fig_path(".swf", options, NULL)
    w = options$out.width %n% (options$fig.width * options$dpi)
    h = options$out.height %n% (options$fig.height * options$dpi)
    swf2html = getFromNamespace("swf2html", "R2SWF")
    file2swf = getFromNamespace("file2swf", "R2SWF")
    swfhtml = swf2html(file2swf(files = fig.name, swf.name, interval = options$interval), 
        output = FALSE, fragment = TRUE, width = w, height = h)
    if (options$fig.align == "default") 
        return(swfhtml)
    sprintf(paste("<div align = \"%s\">\n", swfhtml, "\n</div>"), 
        options$fig.align)
}


knit_filter <- function (ifile, encoding = "unknown") 
{
    x = readLines(ifile, encoding = encoding, warn = FALSE)
    n = length(x)
    if (n == 0) 
        return(x)
    p = detect_pattern(x, tolower(file_ext(ifile)))
    if (is.null(p)) 
        return(x)
    p = all_patterns[[p]]
    p1 = p$chunk.begin
    p2 = p$chunk.end
    i1 = grepl(p1, x)
    i2 = filter_chunk_end(i1, grepl(p2, x))
    m = numeric(n)
    m[i1] = 1
    m[i2] = 2
    if (m[1] == 0) 
        m[1] = 2
    for (i in seq_len(n - 1)) if (m[i + 1] == 0) 
        m[i + 1] = m[i]
    x[m == 1 | i2] = ""
    x[m == 2] = gsub(p$inline.code, "", x[m == 2])
    structure(x, control = "-H -t")
}


all_labels <- function (...) 
{
    cond = as.list(match.call())[-1]
    code = knit_code$get()
    labels = names(code)
    if (length(cond) == 0) 
        return(labels)
    params = lapply(code, attr, "chunk_opts")
    idx = rep_len(TRUE, length(labels))
    for (i in seq_along(cond)) {
        for (j in seq_along(params)) {
            try_eval = function(expr) tryCatch(eval(expr, envir = params[[j]], 
                enclos = knit_global()), error = function(e) FALSE)
            if (idx[j]) {
                res = try_eval(cond[[i]])
                if (is.expression(res)) 
                  res = try_eval(res)
                idx[j] = res
            }
        }
    }
    labels[idx]
}


fig_path <- function (suffix = "", options = opts_current$get(), number) 
{
    if (suffix != "" && !grepl("[.]", suffix)) 
        suffix = paste0(".", suffix)
    if (missing(number)) 
        number = options$fig.cur %n% 1L
    if (!is.null(number)) 
        suffix = paste0("-", number, suffix)
    path = valid_path(options$fig.path, options$label)
    (if (out_format(c("latex", "sweave", "listings"))) 
        sanitize_fn
    else paste0)(path, suffix)
}


clean_cache <- function (clean = FALSE, path = opts_chunk$get("cache.path")) 
{
    odir = opts_knit$get("output.dir")
    if (is.null(odir)) {
        warning("This function must be called inside a source document")
        return()
    }
    owd = setwd(odir)
    on.exit(setwd(owd))
    if (file_test("-d", path)) {
        p0 = path
        p1 = ""
    }
    else {
        p0 = dirname(path)
        p1 = basename(path)
    }
    files = list.files(p0, cache_rx, full.names = TRUE)
    if (length(files) == 0) 
        return()
    base = basename(files)
    labs = .knitEnv$labels
    if (length(labs) == 0) 
        return()
    i = !(sub(cache_rx, "", base) %in% paste0(p1, labs))
    if (p1 != "") 
        i = i & (substr(base, 1, nchar(p1)) == p1)
    if (!any(i)) 
        return()
    if (clean) 
        unlink(files[i])
    else message("Clean these cache files?\n\n", paste(files[i], 
        collapse = "\n"), "\n")
}


raw_output <- function (x, markers = raw_markers, ...) 
{
    asis_output(paste(c(markers[1], x, markers[2]), collapse = ""), 
        ...)
}


include_graphics <- function (path, auto_pdf = TRUE, dpi = NULL) 
{
    if (auto_pdf && is_latex_output()) {
        path2 = sub_ext(path, "pdf")
        i = file.exists(path2)
        path[i] = path2[i]
    }
    structure(path, class = c("knit_image_paths", "knit_asis"), 
        dpi = dpi)
}


hook_optipng <- function (before, options, envir) 
{
    hook_png(before, options, envir, "optipng")
}


knit_watch <- function (input, compile = knit, interval = 1, ...) 
{
    mtime = function(...) file.info(...)[, "mtime"]
    last_time = mtime(input)
    updated = function() {
        this_time = mtime(input)
        on.exit(last_time <<- this_time, add = TRUE)
        this_time > last_time
    }
    for (f in input) compile(f, ...)
    while (TRUE) {
        for (f in input[updated()]) compile(f, ...)
        Sys.sleep(interval)
    }
}


knit2wp <- function (input, title = "A post from knitr", ..., envir = parent.frame(), 
    shortcode = FALSE, action = c("newPost", "editPost", "newPage"), 
    postid, encoding = getOption("encoding"), publish = TRUE) 
{
    out = knit(input, encoding = encoding, envir = envir)
    on.exit(unlink(out))
    con = file(out, encoding = encoding)
    on.exit(close(con), add = TRUE)
    content = native_encode(readLines(con, warn = FALSE))
    content = paste(content, collapse = "\n")
    content = markdown::markdownToHTML(text = content, fragment.only = TRUE)
    shortcode = rep(shortcode, length.out = 2L)
    if (shortcode[1]) 
        content = gsub("<pre><code class=\"([[:alpha:]]+)\">(.+?)</code></pre>", 
            "[sourcecode language=\"\\1\"]\\2[/sourcecode]", 
            content)
    content = gsub("<pre><code( class=\"no-highlight\"|)>(.+?)</code></pre>", 
        if (shortcode[2]) 
            "[sourcecode]\\2[/sourcecode]"
        else "<pre>\\2</pre>", content)
    content = native_encode(content, "UTF-8")
    title = native_encode(title, "UTF-8")
    action = match.arg(action)
    WPargs = list(content = list(description = content, title = title, 
        ...), publish = publish)
    if (action == "editPost") 
        WPargs = c(postid = postid, WPargs)
    do.call("library", list(package = "RWordPress", character.only = TRUE))
    do.call(action, args = WPargs)
}


normal_print <- function (x, ...) 
if (isS4(x)) methods::show(x) else print(x)


render_listings <- function () 
{
    render_sweave()
    opts_chunk$set(prompt = FALSE)
    opts_knit$set(out.format = "listings")
    test_latex_pkg("Sweavel", system.file("misc", "Sweavel.sty", 
        package = "knitr"))
    set_header(framed = "", highlight = "\\usepackage{Sweavel}")
    invisible(NULL)
}


spin <- function (hair, knit = TRUE, report = TRUE, text = NULL, envir = parent.frame(), 
    format = c("Rmd", "Rnw", "Rhtml", "Rtex", "Rrst"), doc = "^#+'[ ]?", 
    inline = "^[{][{](.+)[}][}][ ]*$", comment = c("^[# ]*/[*]", 
        "^.*[*]/ *$"), precious = !knit && is.null(text)) 
{
    format = match.arg(format)
    x = if (nosrc <- is.null(text)) 
        readLines(hair, warn = FALSE)
    else split_lines(text)
    stopifnot(length(comment) == 2L)
    c1 = grep(comment[1], x)
    c2 = grep(comment[2], x)
    if (length(c1) != length(c2)) 
        stop("comments must be put in pairs of start and end delimiters")
    if (length(c1)) 
        x = x[-unique(unlist(mapply(seq, c1, c2, SIMPLIFY = FALSE)))]
    p = .fmt.pat[[tolower(format)]]
    if (any(i <- grepl(inline, x))) 
        x[i] = gsub(inline, p[4], x[i])
    r = rle(grepl(doc, x) | i)
    n = length(r$lengths)
    txt = vector("list", n)
    idx = c(0L, cumsum(r$lengths))
    p1 = gsub("\\{", "\\\\{", paste0("^", p[1L], ".*", p[2L], 
        "$"))
    for (i in seq_len(n)) {
        block = x[seq(idx[i] + 1L, idx[i + 1])]
        txt[[i]] = if (r$values[i]) {
            sub(doc, "", block)
        }
        else {
            block = strip_white(block)
            if (!length(block)) 
                next
            if (length(opt <- grep("^#+(\\+|-| ----+| @knitr)", 
                block))) {
                block[opt] = paste0(p[1L], gsub("^#+(\\+|-| ----+| @knitr)\\s*|-*\\s*$", 
                  "", block[opt]), p[2L])
            }
            if (!grepl(p1, block[1L])) {
                block = c(paste0(p[1L], p[2L]), block)
            }
            c("", block, p[3L], "")
        }
    }
    txt = unlist(txt)
    if (report && format %in% c("Rnw", "Rtex") && !grepl("^\\s*\\\\documentclass", 
        txt)) {
        txt = c("\\documentclass{article}", "\\begin{document}", 
            txt, "\\end{document}")
    }
    if (nosrc) {
        outsrc = sub_ext(hair, format)
        cat(txt, file = outsrc, sep = "\n")
        txt = NULL
    }
    else outsrc = NULL
    if (!knit) 
        return(txt %n% outsrc)
    out = if (report) {
        if (format == "Rmd") {
            knit2html(outsrc, text = txt, envir = envir)
        }
        else if (!is.null(outsrc) && (format %in% c("Rnw", "Rtex"))) {
            knit2pdf(outsrc, envir = envir)
        }
    }
    else knit(outsrc, text = txt, envir = envir)
    if (!precious && !is.null(outsrc)) 
        file.remove(outsrc)
    invisible(out)
}


extract_raw_output <- function (text, markers = raw_markers) 
{
    r = sprintf("%s(.*?)%s", markers[1], markers[2])
    x = paste(text, collapse = "\n")
    m = gregexpr(r, x)
    s = regmatches(x, m)
    n = length(s[[1]])
    if (n == 0) 
        return(list(value = text, chunks = character()))
    chunks = tokens = character(n)
    for (i in seq_len(n)) {
        chunks[i] = sub(r, "\\1", s[[1]][i])
        tokens[i] = digest::digest(chunks[i])
        s[[1]][i] = gsub(r, paste0(markers[1], tokens[i], markers[2]), 
            s[[1]][i])
    }
    regmatches(x, m) = s
    list(value = x, chunks = setNames(chunks, tokens))
}


knit_child <- function (..., options = NULL, envir = knit_global()) 
{
    child = child_mode()
    opts_knit$set(child = TRUE)
    on.exit(opts_knit$set(child = child))
    if (is.list(options)) {
        options$label = options$child = NULL
        if (length(options)) {
            optc = opts_chunk$get(names(options), drop = FALSE)
            opts_chunk$set(options)
            on.exit({
                for (i in names(options)) if (identical(options[[i]], 
                  opts_chunk$get(i))) opts_chunk$set(optc[i])
            }, add = TRUE)
        }
    }
    res = knit(..., tangle = opts_knit$get("tangle"), envir = envir, 
        encoding = opts_knit$get("encoding") %n% getOption("encoding"))
    paste(c("", res), collapse = "\n")
}


load_cache <- function (label, object, notfound = "NOT AVAILABLE", path = opts_chunk$get("cache.path"), 
    lazy = TRUE) 
{
    owd = setwd(opts_knit$get("output.dir"))
    on.exit(setwd(owd))
    path = valid_path(path, label)
    p0 = dirname(path)
    p1 = basename(path)
    p2 = list.files(p0, cache_rx)
    if (length(p2) == 0) 
        return(notfound)
    p2 = p2[substr(p2, 1, nchar(p1)) == p1]
    if (length(p2) == 0) 
        return(notfound)
    if (length(p2) > 3) 
        stop("Wrong cache databases for the chunk ", label, ". You need to remove redundant cache files. Found ", 
            paste(p2, collapse = ", "))
    p2 = unique(gsub("[.](rdb|rdx|RData)$", "", p2))
    if (length(p2) != 1) 
        stop("Cannot identify the cache database for chunk ", 
            label)
    cache$load(file.path(p0, p2), lazy)
    if (missing(object)) 
        return(invisible(NULL))
    if (exists(object, envir = knit_global(), inherits = FALSE)) {
        get(object, envir = knit_global(), inherits = FALSE)
    }
    else notfound
}


hook_plot_md <- function (x, options) 
{
    if (is.null(to <- pandoc_to()) || is_html_output(to)) 
        return(hook_plot_md_base(x, options))
    if (!is.null(options$out.width) || !is.null(options$out.height) || 
        !is.null(options$out.extra) || options$fig.align != "default") {
        if (to %in% c("beamer", "latex")) {
            if (is.null(options$fig.scap)) 
                options$fig.scap = NA
            return(hook_plot_tex(x, options))
        }
        if (to == "docx") {
            warning("Chunk options fig.align, out.width, out.height, out.extra ", 
                "are not supported for Word output")
            options$out.width = options$out.height = options$out.extra = NULL
            options$fig.align = "default"
        }
    }
    if (options$fig.show == "hold" && to == "docx") {
        warning("The chunk option fig.show=\"hold\" is not supported for Word output")
        options$fig.show = "asis"
    }
    hook_plot_md_base(x, options)
}


hook_plot_textile <- function (x, options) 
{
    cap = .img.cap(options)
    if (is.na(cap)) 
        cap = ""
    tags = unlist(c(Map(sprintf, c("width: %s", "height: %s"), 
        options[c("out.width", "out.height")]), css_align(options$fig.align)))
    tags = if (length(tags)) 
        sprintf("{%s}", paste(tags, collapse = ";"))
    else ""
    paste0("!", tags, opts_knit$get("base.url"), .upload.url(x), 
        if (nzchar(cap)) 
            sprintf("(%s)", cap), "!\n\n", if (nzchar(cap)) 
            sprintf("p(knitr plot caption#%s). %s", options$label, 
                cap), "\n\n")
}


render_markdown <- function (strict = FALSE, fence_char = "`") 
{
    set_html_dev()
    opts_knit$set(out.format = "markdown")
    fence = paste(rep(fence_char, 3), collapse = "")
    hook.t = function(x, options, class = NULL) {
        if (strict) {
            paste("\n", indent_block(x), "", sep = "\n")
        }
        else {
            x = paste(c("", x), collapse = "\n")
            r = paste0("\n", fence_char, "{3,}")
            if (grepl(r, x)) {
                l = attr(gregexpr(r, x)[[1]], "match.length")
                l = max(l)
                if (l >= 4) 
                  fence = paste(rep(fence_char, l), collapse = "")
            }
            paste0("\n\n", fence, block_class(class), x, fence, 
                "\n\n")
        }
    }
    hook.o = function(x, options) {
        hook.t(x, options, options$class.output)
    }
    hook.r = function(x, options) {
        language = tolower(options$engine)
        if (language == "node") 
            language = "javascript"
        if (!options$highlight) 
            language = "text"
        if (!is.null(options$class.source)) {
            language = block_class(c(language, options$class.source))
        }
        paste0("\n\n", fence, language, "\n", x, fence, "\n\n")
    }
    knit_hooks$set(source = function(x, options) {
        x = hilight_source(x, "markdown", options)
        (if (strict) 
            hook.t
        else hook.r)(paste(c(x, ""), collapse = "\n"), options)
    }, output = hook.o, warning = hook.t, error = hook.t, message = hook.t, 
        inline = function(x) {
            fmt = pandoc_to()
            fmt = if (length(fmt) == 1L) 
                "latex"
            else "html"
            .inline.hook(format_sci(x, fmt))
        }, plot = hook_plot_md, chunk = function(x, options) {
            x = gsub(paste0("[\n]{2,}(", fence, "|    )"), "\n\n\\1", 
                x)
            x = gsub("[\n]+$", "", x)
            x = gsub("^[\n]+", "\n", x)
            if (isTRUE(options$collapse)) {
                x = gsub(paste0("\n([", fence_char, "]{3,})\n+\\1(", 
                  tolower(options$engine), ")?\n"), "\n", x)
            }
            if (is.null(s <- options$indent)) 
                return(x)
            line_prompt(x, prompt = s, continue = s)
        })
}


knit_params <- function (text, evaluate = TRUE) 
{
    text = split_lines(text)
    yaml = yaml_front_matter(text)
    if (is.null(yaml)) 
        return(list())
    knit_params_yaml(enc2utf8(yaml), evaluate = evaluate)
}


opts_hooks <- structure(list(get = function (name, default = FALSE, drop = TRUE) 
{
    if (default) 
        defaults = value
    if (missing(name)) 
        defaults
    else {
        if (drop && length(name) == 1) 
            defaults[[name]]
        else {
            setNames(defaults[name], name)
        }
    }
}, set = function (...) 
{
    dots = list(...)
    if (length(dots) == 0) 
        return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) 
        if (length(dots <- dots[[1]]) == 0) 
            return()
    defaults <<- merge(dots)
    invisible(NULL)
}, merge = function (values) 
merge_list(defaults, values), restore = function (target = value) 
defaults <<- target), .Names = c("get", "set", "merge", "restore"
))


set_alias <- function (...) 
{
    opts_knit$set(aliases = c(...))
}


hook_pngquant <- function (before, options, envir) 
{
    if (is.null(options[["pngquant"]])) 
        options$pngquant = "--skip-if-larger"
    options[["pngquant"]] = paste(options[["pngquant"]], "--ext -fs8.png")
    hook_png(before, options, envir, "pngquant", function(x) {
        x2 = sub("\\.png$", "-fs8.png", x)
        if (file.exists(x2)) 
            file.rename(x2, x)
    })
}


render_rst <- function (strict = FALSE) 
{
    set_html_dev()
    hook.s = function(x, options) {
        paste(c("\n\n::\n", indent_block(x), ""), collapse = "\n")
    }
    hook.t = function(x, options) {
        make_directive("sourcecode", tolower(options$engine), 
            "", content = x)
    }
    hook.i = function(x) .inline.hook(format_sci(x, "rst"))
    knit_hooks$set(source = function(x, options) {
        x = paste(c(hilight_source(x, "rst", options), ""), collapse = "\n")
        (if (strict) 
            hook.s
        else hook.t)(x, options)
    }, warning = hook.s, error = hook.s, message = hook.s, output = hook.s, 
        inline = hook.i, plot = hook_plot_rst)
}


render_asciidoc <- function () 
{
    set_html_dev()
    opts_knit$set(out.format = "asciidoc")
    hook.source = function(x, options) {
        x = paste(c(hilight_source(x, "asciidoc", options), ""), 
            collapse = "\n")
        sprintf("\n[source,%s]\n----\n%s----\n", tolower(options$engine), 
            x)
    }
    hook.message = function(x, options) {
        sprintf("\n[NOTE]\n====\n.Message\n%s\n====\n", substring(x, 
            comment_length(options$comment)))
    }
    hook.warning = function(x, options) {
        sprintf("\n[WARNING]\n====\n.Warning\n%s\n====\n", gsub("^.*Warning: ", 
            "", x))
    }
    hook.error = function(x, options) {
        sprintf("\n[CAUTION]\n====\n.Error\n%s\n====\n", gsub("^.*Error: ", 
            "", x))
    }
    hook.output = function(x, options) sprintf("\n----\n%s----\n", 
        x)
    knit_hooks$set(source = hook.source, output = hook.output, 
        message = hook.message, warning = hook.warning, error = hook.error, 
        plot = hook_plot_asciidoc)
}


render_textile <- function () 
{
    set_html_dev()
    opts_knit$set(out.format = "textile")
    textile.hook = function(name) {
        force(name)
        function(x, options) {
            if (name == "source") 
                x = c(hilight_source(x, "textile", options), 
                  "")
            x = paste(x, collapse = "\n")
            sprintf("bc(knitr %s %s#%s).. %s\np(knitr_end). \n\n", 
                tolower(options$engine), name, options$label, 
                x)
        }
    }
    hook.inline = function(x) .inline.hook(format_sci(x, "html"))
    z = list()
    for (i in c("source", "warning", "message", "error")) z[[i]] = textile.hook(i)
    knit_hooks$set(z)
    knit_hooks$set(inline = hook.inline, output = textile.hook("output"), 
        plot = hook_plot_textile)
}


opts_chunk <- structure(list(get = function (name, default = FALSE, drop = TRUE) 
{
    if (default) 
        defaults = value
    if (missing(name)) 
        defaults
    else {
        if (drop && length(name) == 1) 
            defaults[[name]]
        else {
            setNames(defaults[name], name)
        }
    }
}, set = function (...) 
{
    dots = list(...)
    if (length(dots) == 0) 
        return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) 
        if (length(dots <- dots[[1]]) == 0) 
            return()
    defaults <<- merge(dots)
    invisible(NULL)
}, merge = function (values) 
merge_list(defaults, values), restore = function (target = value) 
defaults <<- target), .Names = c("get", "set", "merge", "restore"
))


knit_rd_all <- function () 
{
    owd = getwd()
    on.exit(setwd(owd))
    links = tools::findHTMLlinks()
    for (p in .packages(TRUE)) {
        message("* Making static html help pages for ", p)
        setwd(system.file("html", package = p))
        knit_rd(p, links, frame = FALSE)
    }
}


knit_theme <- structure(list(set = function (theme) 
{
    header = if (is.list(theme)) 
        theme
    else theme_to_header(theme)
    opts_chunk$set(background = header$background)
    set_header(highlight = header$highlight)
}, get = function (theme = NULL) 
{
    if (is.null(theme)) {
        theme_dir = system.file("themes", package = "knitr")
        theme_files = list.files(theme_dir, pattern = "\\.css$")
        gsub("\\.css$", "", basename(theme_files))
    }
    else {
        theme_to_header(theme)
    }
}), .Names = c("set", "get"))


dep_auto <- function (path = opts_chunk$get("cache.path")) 
{
    owd = setwd(opts_knit$get("output.dir"))
    on.exit(setwd(owd))
    paths = valid_path(path, c("__objects", "__globals"))
    locals = parse_objects(paths[1L])
    globals = parse_objects(paths[2L])
    if (is.null(locals) || is.null(globals)) 
        return(invisible(NULL))
    if (!identical(names(locals), names(globals))) {
        warning("corrupt dependency files? \ntry remove ", paste(paths, 
            collapse = "; "))
        return(invisible(NULL))
    }
    nms = intersect(names(knit_code$get()), names(locals))
    if (length(nms) < 2) 
        return(invisible(NULL))
    for (i in 2:length(nms)) {
        if (length(g <- globals[[nms[i]]]) == 0) 
            next
        for (j in 1:(i - 1L)) {
            if (any(g %in% locals[[nms[j]]])) 
                dep_list$set(setNames(list(unique(c(dep_list$get(nms[j]), 
                  nms[i]))), nms[j]))
        }
    }
}


hook_plot_custom <- function (before, options, envir) 
{
    if (before) 
        return()
    if (options$fig.show == "hide") 
        return()
    ext = options$fig.ext %n% dev2ext(options$dev)
    hook = knit_hooks$get("plot")
    n = options$fig.num
    if (n == 0L) 
        n = options$fig.num = 1L
    res = unlist(lapply(seq_len(n), function(i) {
        options$fig.cur = i
        hook(fig_path(ext, options, i), reduce_plot_opts(options))
    }), use.names = FALSE)
    paste(res, collapse = "")
}


image_uri <- function (f) 
markdown:::.b64EncodeFile(f)


knit_meta_add <- function (meta, label = "") 
{
    if (length(meta)) {
        meta_id = attr(.knitEnv$meta, "knit_meta_id")
        .knitEnv$meta = c(.knitEnv$meta, meta)
        attr(.knitEnv$meta, "knit_meta_id") = c(meta_id, rep(label, 
            length(meta)))
    }
    .knitEnv$meta
}


knit <- function (input, output = NULL, tangle = FALSE, text = NULL, 
    quiet = FALSE, envir = parent.frame(), encoding = getOption("encoding")) 
{
    in.file = !missing(input) && (is.character(input) || prod(inherits(input, 
        c("file", "connection"), TRUE)))
    oconc = knit_concord$get()
    on.exit(knit_concord$set(oconc), add = TRUE)
    if (!missing(input)) 
        input2 = input
    if (in.file && !is.character(input)) 
        input = summary(input)$description
    if (child_mode()) {
        setwd(opts_knit$get("output.dir"))
        if (in.file && !is_abs_path(input)) {
            input = paste0(opts_knit$get("child.path"), input)
            input = input2 = file.path(input_dir(TRUE), input)
        }
        optk = opts_knit$get()
        on.exit(opts_knit$set(optk), add = TRUE)
        opts_knit$set(progress = opts_knit$get("progress") && 
            !quiet)
        quiet = !opts_knit$get("progress")
    }
    else {
        opts_knit$set(output.dir = getwd())
        knit_log$restore()
        on.exit(chunk_counter(reset = TRUE), add = TRUE)
        adjust_opts_knit()
        oopts = options(useFancyQuotes = FALSE, width = opts_knit$get("width"), 
            knitr.in.progress = TRUE, device = pdf_null)
        on.exit(options(oopts), add = TRUE)
        optc = opts_chunk$get()
        on.exit(opts_chunk$restore(optc), add = TRUE)
        ocode = knit_code$get()
        on.exit(knit_code$restore(ocode), add = TRUE)
        on.exit(opts_current$restore(), add = TRUE)
        optk = opts_knit$get()
        on.exit(opts_knit$set(optk), add = TRUE)
        opts_knit$set(tangle = tangle, encoding = encoding, progress = opts_knit$get("progress") && 
            !quiet)
    }
    oenvir = .knitEnv$knit_global
    .knitEnv$knit_global = envir
    on.exit({
        .knitEnv$knit_global = oenvir
    }, add = TRUE)
    ext = "unknown"
    if (in.file) {
        input.dir = .knitEnv$input.dir
        on.exit({
            .knitEnv$input.dir = input.dir
        }, add = TRUE)
        .knitEnv$input.dir = dirname(input)
        ext = tolower(file_ext(input))
        if ((is.null(output) || is.na(output)) && !child_mode()) 
            output = basename(auto_out_name(input, ext))
        if (is.character(output) && !child_mode()) {
            out.purl = sub_ext(input, "R")
            if (same_file(output, out.purl) && tangle && file_test("-nt", 
                out.purl, input)) 
                return(out.purl)
            otangle = .knitEnv$tangle.file
            .knitEnv$tangle.file = normalizePath(out.purl, mustWork = FALSE)
            .knitEnv$tangle.start = FALSE
            on.exit({
                .knitEnv$tangle.file = otangle
                .knitEnv$tangle.start = NULL
            }, add = TRUE)
        }
        if (is.null(getOption("tikzMetricsDictionary"))) {
            options(tikzMetricsDictionary = tikz_dict(input))
            on.exit(options(tikzMetricsDictionary = NULL), add = TRUE)
        }
        knit_concord$set(infile = input, outfile = output)
    }
    encoding = correct_encode(encoding)
    text = if (is.null(text)) {
        readLines(if (is.character(input2)) {
            con = file(input2, encoding = encoding)
            on.exit(close(con), add = TRUE)
            con
        }
        else input2, warn = FALSE)
    }
    else split_lines(text)
    if (!length(text)) {
        if (is.character(output)) 
            file.create(output)
        return(output)
    }
    apat = all_patterns
    opat = knit_patterns$get()
    on.exit(knit_patterns$restore(opat), add = TRUE)
    if (length(opat) == 0 || all(vapply(opat, is.null, logical(1)))) {
        if (is.null(pattern <- detect_pattern(text, ext))) {
            if (is.null(output)) 
                return(paste(text, collapse = "\n"))
            else {
                cat(text, sep = "\n", file = output)
                return(output)
            }
        }
        if (!(pattern %in% names(apat))) 
            stop("a pattern list cannot be automatically found for the file extension '", 
                ext, "' in built-in pattern lists; ", "see ?knit_patterns on how to set up customized patterns")
        set_pattern(pattern)
        if (pattern == "rnw" && length(sweave_lines <- which_sweave(text)) > 
            0) 
            remind_sweave(if (in.file) 
                input, sweave_lines)
        opts_knit$set(out.format = switch(pattern, rnw = "latex", 
            tex = "latex", html = "html", md = "markdown", rst = "rst", 
            brew = "brew", asciidoc = "asciidoc", textile = "textile"))
    }
    if (is.null(out_format())) 
        auto_format(ext)
    params = NULL
    if (out_format("markdown")) {
        if (child_mode()) {
            if (grepl("^---\\s*$", text[1])) {
                i = grep("^---\\s*$", text)
                if (length(i) >= 2) 
                  text = text[-(1:i[2])]
            }
        }
        else {
            params = knit_params(text)
            params = if (length(params)) 
                c("params <-", capture.output(dput(flatten_params(params), 
                  "")), "")
            .knitEnv$tangle.params = params
        }
    }
    if (identical(knit_hooks$get(names(.default.hooks)), .default.hooks) && 
        !child_mode()) {
        getFromNamespace(paste("render", out_format(), sep = "_"), 
            "knitr")()
        on.exit(knit_hooks$set(.default.hooks), add = TRUE)
    }
    progress = opts_knit$get("progress")
    if (in.file && !quiet) 
        message(ifelse(progress, "\n\n", ""), "processing file: ", 
            input)
    res = process_file(text, output)
    res = paste((knit_hooks$get("document"))(res), collapse = "\n")
    if (tangle) 
        res = c(params, res)
    if (!is.null(output)) 
        writeLines(if (encoding == "") 
            res
        else native_encode(res, to = encoding), con = output, 
            useBytes = encoding != "")
    if (!child_mode()) {
        dep_list$restore()
        .knitEnv$labels = NULL
    }
    if (in.file && is.character(output) && file.exists(output)) {
        concord_gen(input, output)
        if (!quiet) 
            message("output file: ", output, ifelse(progress, 
                "\n", ""))
    }
    output %n% res
}


render_latex <- function () 
{
    test_latex_pkg("framed", system.file("misc", "framed.sty", 
        package = "knitr"))
    opts_chunk$set(out.width = "\\maxwidth", dev = "pdf")
    opts_knit$set(out.format = "latex")
    h = opts_knit$get("header")
    if (!nzchar(h["framed"])) 
        set_header(framed = .header.framed)
    if (!nzchar(h["highlight"])) 
        set_header(highlight = .header.hi.tex)
    knit_hooks$set(source = function(x, options) {
        x = hilight_source(x, "latex", options)
        if (options$highlight) {
            if (options$engine == "R" || x[1] != "\\noindent") {
                paste(c("\\begin{alltt}", x, "\\end{alltt}", 
                  ""), collapse = "\n")
            }
            else {
                if ((n <- length(x)) > 4) 
                  x[n - 2] = sub("\\\\\\\\$", "", x[n - 2])
                paste(c(x, ""), collapse = "\n")
            }
        }
        else .verb.hook(x)
    }, output = function(x, options) {
        if (output_asis(x, options)) {
            paste0("\\end{kframe}", x, "\\begin{kframe}")
        }
        else .verb.hook(x)
    }, warning = .color.block("\\color{warningcolor}{", "}"), 
        message = .color.block("\\itshape\\color{messagecolor}{", 
            "}"), error = .color.block("\\bfseries\\color{errorcolor}{", 
            "}"), inline = .inline.hook.tex, chunk = .chunk.hook.tex, 
        plot = function(x, options) {
            paste0("\\end{kframe}", hook_plot_tex(x, options), 
                "\n\\begin{kframe}")
        })
}


hook_purl <- function (before, options, envir) 
{
    if (before || !options$purl || options$engine != "R") 
        return()
    output = .knitEnv$tangle.file
    if (isFALSE(.knitEnv$tangle.start)) {
        .knitEnv$tangle.start = TRUE
        unlink(output)
        params = .knitEnv$tangle.params
        if (length(params)) 
            writeLines(params, output)
        .knitEnv$tangle.params = NULL
    }
    code = options$code
    if (isFALSE(options$eval)) 
        code = comment_out(code, "# ", newline = FALSE)
    if (is.character(output)) {
        cat(label_code(code, options$params.src), file = output, 
            sep = "\n", append = TRUE)
    }
}


knit_exit <- function (append) 
{
    if (missing(append)) 
        append = if (out_format(c("latex", "sweave", "listings"))) 
            "\\end{document}"
        else if (out_format("html")) 
            "</body>\n</html>"
        else ""
    .knitEnv$terminate = append
    invisible()
}


render_jekyll <- function (highlight = c("pygments", "prettify", "none"), extra = "") 
{
    hi = match.arg(highlight)
    render_markdown(TRUE)
    if (hi == "none") 
        return()
    switch(hi, pygments = {
        hook.r = function(x, options) {
            paste0("\n\n{% highlight ", tolower(options$engine), 
                if (extra != "") " ", extra, " %}\n", x, "\n{% endhighlight %}\n\n")
        }
        hook.t = function(x, options) paste0("\n\n{% highlight text %}\n", 
            x, "{% endhighlight %}\n\n")
    }, prettify = {
        hook.r = function(x, options) {
            paste0("\n\n<pre><code class=\"prettyprint ", extra, 
                "\">", escape_html(x), "</code></pre>\n\n")
        }
        hook.t = function(x, options) paste0("\n\n<pre><code>", 
            escape_html(x), "</code></pre>\n\n")
    })
    knit_hooks$set(source = function(x, options) {
        x = paste(hilight_source(x, "markdown", options), collapse = "\n")
        hook.r(x, options)
    }, output = hook.t, warning = hook.t, error = hook.t, message = hook.t)
}


all_patterns <- structure(list(rnw = structure(list(chunk.begin = "^\\s*<<(.*)>>=.*$", 
    chunk.end = "^\\s*@\\s*(%+.*|)$", inline.code = "\\\\Sexpr\\{([^}]+)\\}", 
    inline.comment = "^\\s*%.*", ref.chunk = "^\\s*<<(.+)>>\\s*$", 
    header.begin = "(^|\n)\\s*\\\\documentclass[^}]+\\}", document.begin = "\\s*\\\\begin\\{document\\}"), .Names = c("chunk.begin", 
"chunk.end", "inline.code", "inline.comment", "ref.chunk", "header.begin", 
"document.begin")), brew = structure(list(inline.code = "<%[=]{0,1}\\s+([^%]+)\\s+[-]*%>"), .Names = "inline.code"), 
    tex = structure(list(chunk.begin = "^\\s*%+\\s*begin.rcode\\s*(.*)", 
        chunk.end = "^\\s*%+\\s*end.rcode", chunk.code = "^\\s*%+", 
        ref.chunk = "^%+\\s*<<(.+)>>\\s*$", inline.comment = "^\\s*%.*", 
        inline.code = "\\\\rinline\\{([^}]+)\\}", header.begin = "(^|\n)\\s*\\\\documentclass[^}]+\\}", 
        document.begin = "\\s*\\\\begin\\{document\\}"), .Names = c("chunk.begin", 
    "chunk.end", "chunk.code", "ref.chunk", "inline.comment", 
    "inline.code", "header.begin", "document.begin")), html = structure(list(
        chunk.begin = "^\\s*<!--\\s*begin.rcode\\s*(.*)", chunk.end = "^\\s*end.rcode\\s*-->", 
        ref.chunk = "^\\s*<<(.+)>>\\s*$", inline.code = "<!--\\s*rinline(.+?)-->", 
        header.begin = "\\s*<head>"), .Names = c("chunk.begin", 
    "chunk.end", "ref.chunk", "inline.code", "header.begin")), 
    md = structure(list(chunk.begin = "^[\t >]*```+\\s*\\{([a-zA-Z0-9]+.*)\\}\\s*$", 
        chunk.end = "^[\t >]*```+\\s*$", ref.chunk = "^\\s*<<(.+)>>\\s*$", 
        inline.code = "`r[ #]([^`]+)\\s*`"), .Names = c("chunk.begin", 
    "chunk.end", "ref.chunk", "inline.code")), rst = structure(list(
        chunk.begin = "^\\s*[.][.]\\s+\\{r(.*)\\}\\s*$", chunk.end = "^\\s*[.][.]\\s+[.][.]\\s*$", 
        chunk.code = "^\\s*[.][.]", ref.chunk = "^\\.*\\s*<<(.+)>>\\s*$", 
        inline.code = ":r:`([^`]+)`"), .Names = c("chunk.begin", 
    "chunk.end", "chunk.code", "ref.chunk", "inline.code")), 
    asciidoc = structure(list(chunk.begin = "^//\\s*begin[.]rcode(.*)$", 
        chunk.end = "^//\\s*end[.]rcode\\s*$", chunk.code = "^//+", 
        ref.chunk = "^\\s*<<(.+)>>\\s*$", inline.code = "`r +([^`]+)\\s*`|[+]r +([^+]+)\\s*[+]", 
        inline.comment = "^//.*"), .Names = c("chunk.begin", 
    "chunk.end", "chunk.code", "ref.chunk", "inline.code", "inline.comment"
    )), textile = structure(list(chunk.begin = "^###[.]\\s+begin[.]rcode(.*)$", 
        chunk.end = "^###[.]\\s+end[.]rcode\\s*$", ref.chunk = "^\\s*<<(.+)>>\\s*$", 
        inline.code = "@r +([^@]+)\\s*@", inline.comment = "^###[.].*"), .Names = c("chunk.begin", 
    "chunk.end", "ref.chunk", "inline.code", "inline.comment"
    ))), .Names = c("rnw", "brew", "tex", "html", "md", "rst", 
"asciidoc", "textile"))


kable <- function (x, format, digits = getOption("digits"), row.names = NA, 
    col.names = NA, align, caption = NULL, format.args = list(), 
    escape = TRUE, ...) 
{
    if (missing(format) || is.null(format)) 
        format = getOption("knitr.table.format")
    if (is.null(format)) 
        format = if (is.null(pandoc_to())) 
            switch(out_format() %n% "markdown", latex = "latex", 
                listings = "latex", sweave = "latex", html = "html", 
                markdown = "markdown", rst = "rst", stop("table format not implemented yet!"))
        else if (isTRUE(opts_knit$get("kable.force.latex")) && 
            is_latex_output()) {
            "latex"
        }
        else "pandoc"
    if (is.function(format)) 
        format = format()
    if (format != "latex" && !missing(align) && length(align) == 
        1L) 
        align = strsplit(align, "")[[1]]
    if (!is.null(caption) && !is.na(caption)) 
        caption = paste0(create_label("tab:", opts_current$get("label"), 
            latex = (format == "latex")), caption)
    if (inherits(x, "list")) {
        if (format == "pandoc" && is_latex_output()) 
            format = "latex"
        res = lapply(x, kable, format = format, digits = digits, 
            row.names = row.names, col.names = col.names, align = align, 
            caption = NA, format.args = format.args, escape = escape, 
            ...)
        res = unlist(lapply(res, paste, collapse = "\n"))
        res = if (format == "latex") {
            kable_latex_caption(res, caption)
        }
        else if (format == "html" || (format == "pandoc" && is_html_output())) 
            kable_html(matrix(paste0("\n\n", res, "\n\n"), 1), 
                caption = caption, escape = FALSE, table.attr = "class=\"kable_wrapper\"")
        else {
            res = paste(res, collapse = "\n\n")
            if (format == "pandoc") 
                kable_pandoc_caption(res, caption)
            else res
        }
        return(structure(res, format = format, class = "knitr_kable"))
    }
    if (identical(col.names, NA)) 
        col.names = colnames(x)
    if (!is.matrix(x)) 
        x = as.data.frame(x)
    m = ncol(x)
    isn = if (is.matrix(x)) 
        rep(is.numeric(x), m)
    else sapply(x, is.numeric)
    if (missing(align) || (format == "latex" && is.null(align))) 
        align = ifelse(isn, "r", "l")
    digits = rep(digits, length.out = m)
    for (j in seq_len(m)) {
        if (is_numeric(x[, j])) 
            x[, j] = round(x[, j], digits[j])
    }
    if (any(isn)) {
        if (is.matrix(x)) {
            if (is.table(x) && length(dim(x)) == 2) 
                class(x) = "matrix"
            x = format_matrix(x, format.args)
        }
        else x[, isn] = format_args(x[, isn], format.args)
    }
    if (is.na(row.names)) 
        row.names = has_rownames(x)
    if (!is.null(align)) 
        align = rep(align, length.out = m)
    if (row.names) {
        x = cbind(` ` = rownames(x), x)
        if (!is.null(col.names)) 
            col.names = c(" ", col.names)
        if (!is.null(align)) 
            align = c("l", align)
    }
    n = nrow(x)
    x = replace_na(base::format(as.matrix(x), trim = TRUE, justify = "none"), 
        is.na(x))
    if (!is.matrix(x)) 
        x = matrix(x, nrow = n)
    x = trimws(x)
    colnames(x) = col.names
    if (format != "latex" && length(align) && !all(align %in% 
        c("l", "r", "c"))) 
        stop("'align' must be a character vector of possible values 'l', 'r', and 'c'")
    attr(x, "align") = align
    res = do.call(paste("kable", format, sep = "_"), list(x = x, 
        caption = caption, escape = escape, ...))
    structure(res, format = format, class = "knitr_kable")
}


render_sweave <- function () 
{
    opts_chunk$set(highlight = FALSE, comment = NA, prompt = TRUE)
    opts_knit$set(out.format = "sweave")
    test_latex_pkg("Sweave", file.path(R.home("share"), "texmf", 
        "tex", "latex", "Sweave.sty"))
    set_header(framed = "", highlight = "\\usepackage{Sweave}")
    hook.i = function(x, options) paste(c("\\begin{Sinput}", 
        hilight_source(x, "sweave", options), "\\end{Sinput}", 
        ""), collapse = "\n")
    hook.s = function(x, options) paste0("\\begin{Soutput}\n", 
        x, "\\end{Soutput}\n")
    hook.c = function(x, options) {
        if (output_asis(x, options)) 
            return(x)
        paste0("\\begin{Schunk}\n", x, "\\end{Schunk}")
    }
    knit_hooks$set(source = hook.i, output = hook.s, warning = hook.s, 
        message = hook.s, error = hook.s, inline = .inline.hook.tex, 
        plot = hook_plot_tex, chunk = hook.c)
}


Sweave2knitr <- function (file, output = gsub("[.]([^.]+)$", "-knitr.\\1", file), 
    encoding = getOption("encoding"), text = NULL) 
{
    x = if (is.null(text)) 
        readLines(file(file, encoding = encoding), warn = FALSE)
    else text
    x = native_encode(x)
    x = gsub_msg("removing \\usepackage{Sweave}", "^\\s*\\\\usepackage(\\[.*\\])?\\{Sweave\\}", 
        "", x)
    i = grep("^<<(.*)>>=\\s*$", x)
    if (length(i)) {
        opts = gsub("^<<(.*)>>=\\s*$", "\\1", x[i])
        x[i] = paste0("<<", fix_sweave(opts), ">>=")
    }
    x = gsub_msg("replacing \\SweaveInput{...} with <<child='...'>>=", 
        "^\\s*\\\\SweaveInput\\{([^}]+)\\}", "\n<<'child-\\1', child='\\1'>>=\n@\n", 
        x)
    s = "^\\s*\\\\SweaveOpts\\{([^}]*)\\}.*$"
    i = grep(s, x)
    if (length(i)) {
        opts = fix_sweave(gsub(s, "\\1", x[i]))
        x[i] = gsub_msg("changing \\SweaveOpts{} to opts_chunk$set()", 
            s, "@_@_@", x[i])
        for (j in seq_along(i)) x[i[j]] = gsub("@_@_@", paste(c("\n<<include=FALSE>>=", 
            "library(knitr)", "opts_chunk$set(", opts[j], ")", 
            "@\n"), collapse = "\n"), x[i[j]])
    }
    i1 = grepl(all_patterns$rnw$chunk.begin, x)
    i2 = grepl(all_patterns$rnw$chunk.end, x)
    i = which(i2 & !filter_chunk_end(i1, i2))
    if (length(i)) {
        message("removing extra lines (#n shows line numbers):\n", 
            paste(formatUL(sprintf("(#%d) %s", i, x[i]), offset = 4), 
                collapse = "\n"))
        x = x[-i]
    }
    if (is.null(text)) {
        if (encoding != "native.enc") 
            x = native_encode(x, encoding)
        cat(x, sep = "\n", file = output)
    }
    else x
}


hook_plot_tex <- function (x, options) 
{
    rw = options$resize.width
    rh = options$resize.height
    resize1 = resize2 = ""
    if (!is.null(rw) || !is.null(rh)) {
        resize1 = sprintf("\\resizebox{%s}{%s}{", rw %n% "!", 
            rh %n% "!")
        resize2 = "} "
    }
    tikz = is_tikz_dev(options)
    a = options$fig.align
    fig.cur = options$fig.cur %n% 1L
    fig.num = options$fig.num %n% 1L
    animate = options$fig.show == "animate"
    if (!tikz && animate && fig.cur < fig.num) 
        return("")
    usesub = length(subcap <- options$fig.subcap) && fig.num > 
        1
    ai = options$fig.show != "hold"
    plot1 = ai || fig.cur <= 1L
    plot2 = ai || fig.cur == fig.num
    align1 = if (plot1) 
        switch(a, left = "\n\n", center = "\n\n{\\centering ", 
            right = "\n\n\\hfill{}", "\n")
    align2 = if (plot2) 
        switch(a, left = "\\hfill{}\n\n", center = "\n\n}\n\n", 
            right = "\n\n", "")
    cap = options$fig.cap
    scap = options$fig.scap
    fig1 = fig2 = ""
    mcap = fig.num > 1L && options$fig.show == "asis" && !length(subcap)
    sub1 = sub2 = ""
    if (length(cap) && !is.na(cap)) {
        lab = paste0(options$fig.lp, options$label)
        if (plot1) {
            pos = options$fig.pos
            if (pos != "" && !grepl("^[[{]", pos)) 
                pos = sprintf("[%s]", pos)
            fig1 = sprintf("\\begin{%s}%s", options$fig.env, 
                pos)
        }
        if (usesub) {
            sub1 = sprintf("\\subfloat[%s%s]{", subcap, create_label(lab, 
                fig.cur, latex = TRUE))
            sub2 = "}"
        }
        if (plot2) {
            if (is.null(scap) && !grepl("[{].*?[:.;].*?[}]", 
                cap)) {
                scap = strsplit(cap, "[:.;]( |\\\\|$)")[[1L]][1L]
            }
            scap = if (is.null(scap) || is.na(scap)) 
                ""
            else sprintf("[%s]", scap)
            cap = if (cap == "") 
                ""
            else sprintf("\\caption%s{%s}%s\n", scap, cap, create_label(lab, 
                if (mcap) 
                  fig.cur, latex = TRUE))
            fig2 = sprintf("%s\\end{%s}\n", cap, options$fig.env)
        }
    }
    else if (pandoc_to(c("latex", "beamer"))) {
        align.env = switch(a, left = "flushleft", center = "center", 
            right = "flushright")
        align1 = if (plot1) 
            if (a == "default") 
                "\n"
            else sprintf("\n\n\\begin{%s}", align.env)
        align2 = if (plot2) 
            if (a == "default") 
                ""
            else sprintf("\\end{%s}\n\n", align.env)
    }
    if (animate && identical(options$out.width, "\\maxwidth")) 
        options$out.width = NULL
    size = paste(c(sprintf("width=%s", options$out.width), sprintf("height=%s", 
        options$out.height), options$out.extra), collapse = ",")
    paste0(fig1, align1, sub1, resize1, if (tikz) {
        sprintf("\\input{%s}", x)
    }
    else if (animate) {
        aniopts = options$aniopts
        aniopts = if (is.na(aniopts)) 
            NULL
        else gsub(";", ",", aniopts)
        size = paste(c(size, sprintf("%s", aniopts)), collapse = ",")
        if (nzchar(size)) 
            size = sprintf("[%s]", size)
        sprintf("\\animategraphics%s{%s}{%s}{%s}{%s}", size, 
            1/options$interval, sub(sprintf("%d$", fig.num), 
                "", sans_ext(x)), 1L, fig.num)
    }
    else {
        if (nzchar(size)) 
            size = sprintf("[%s]", size)
        res = sprintf("\\includegraphics%s{%s} ", size, sans_ext(x))
        lnk = options$fig.link
        if (is.null(lnk) || is.na(lnk)) 
            res
        else sprintf("\\href{%s}{%s}", lnk, res)
    }, resize2, sub2, align2, fig2)
}


knit2html <- function (input, output = NULL, ..., envir = parent.frame(), 
    text = NULL, quiet = FALSE, encoding = getOption("encoding"), 
    force_v1 = FALSE) 
{
    if (!force_v1 && is.null(text)) {
        con = file(input, encoding = encoding)
        on.exit(close(con), add = TRUE)
        signal = if (is_R_CMD_check()) 
            warning2
        else stop2
        if (length(grep("^---\\s*$", head(readLines(con), 1)))) 
            signal("It seems you should call rmarkdown::render() instead of knitr::knit2html() ", 
                "because ", input, " appears to be an R Markdown v2 document.")
    }
    out = knit(input, text = text, envir = envir, encoding = encoding, 
        quiet = quiet)
    if (is.null(text)) {
        output = sub_ext(if (is.null(output) || is.na(output)) 
            out
        else output, "html")
        markdown::markdownToHTML(out, output, encoding = encoding, 
            ...)
        invisible(output)
    }
    else markdown::markdownToHTML(text = out, ...)
}


rocco <- function (input, ...) 
{
    out = knit2html(input, ..., stylesheet = system.file("misc", 
        "docco-classic.css", package = "knitr"), template = system.file("misc", 
        "docco-classic.html", package = "knitr"))
    txt = readLines(out)
    i1 = min(grep("<!--table start-->$", txt))
    i2 = max(grep("<!--table end-->$", txt))
    x = paste(txt[seq(i1 + 1, i2 - 1)], collapse = "\n")
    x = gsub("</pre>\\s*<pre>", "<!--ReDuNdAnTpRe-->", x)
    m = gregexpr("<pre><code( class=\"[[:alnum:]]+\")?>(.|\n)*?</code></pre>", 
        x)
    if (m[[1]][1] == -1) 
        stop("No code blocks in HTML output")
    code = regmatches(x, m)[[1]]
    code = gsub("<!--ReDuNdAnTpRe-->", "</pre>\n<pre>", code)
    code = paste0("<td class=\"code\">", c(code, ""), "</td></tr>")
    doc = regmatches(x, m, invert = TRUE)[[1]]
    doc = paste0("<tr><td class=\"docs\">", docAdjust(doc), "</td>")
    sec = 1
    for (i in seq_along(doc)) {
        while (grepl("<tr><td class=\"docs\">", doc[i])) {
            doc[i] = sub("<tr><td class=\"docs\">", paste0("<tr id=\"row", 
                sec, "\"><td class=\"docs\">", "<div class=\"pilwrap\">", 
                "<a class=\"pilcrow\" href=\"#row", sec, "\">&para;</a></div>"), 
                doc[i])
            sec = sec + 1
        }
    }
    html = c(txt[1:i1], paste0(doc, code, collapse = ""), txt[i2:length(txt)])
    writeLines(html, out)
    invisible(out)
}


include_url <- function (url, height = "400px") 
{
    include_url2(url, height)
}


current_input <- function (dir = FALSE) 
{
    input = knit_concord$get("infile")
    outwd = opts_knit$get("output.dir")
    if (is.null(input)) 
        return()
    if (dir) {
        if (is.null(outwd)) {
            warning("Cannot determine the directory of the input document")
            dir = FALSE
        }
    }
    if (!dir) 
        return(basename(input))
    if (is_abs_path(input)) 
        input
    else file.path(outwd, input)
}


hook_plot_rst <- function (x, options) 
{
    if (options$fig.show == "animate") 
        return(hook_plot_html(x, options))
    cap = .img.cap(options)
    make_directive("figure", paste0(opts_knit$get("base.url"), 
        .upload.url(x)), c(align = if (options$fig.align == "default") NULL else options$fig.align, 
        alt = cap, width = options$out.width, height = options$out.height), 
        cap)
}


all_rcpp_labels <- function (...) 
all_labels(expression(engine == "Rcpp"), ...)


stitch_rmd <- function (..., envir = parent.frame()) 
stitch(..., envir = envir, template = system.file("misc", "knitr-template.Rmd", 
    package = "knitr"))


inline_expr <- function (code, syntax) 
{
    if (!is.character(code) || length(code) != 1) 
        stop("The inline code must be a character string")
    if (!missing(syntax)) 
        pat = syntax
    else {
        inline = knit_patterns$get("inline.code")
        if (is.null(inline)) 
            stop("inline_expr() must be called in a knitting process")
        pat = NULL
        for (i in names(all_patterns)) {
            if (inline == all_patterns[[i]][["inline.code"]]) {
                pat = i
                break
            }
        }
    }
    if (is.null(pat)) 
        stop("Unknown document format")
    sprintf(switch(pat, rnw = "\\Sexpr{%s}", tex = "\\rinline{%s}", 
        html = "<!--rinline %s -->", md = "`r %s`", rst = ":r:`%s`", 
        asciidoc = "`r %s`", textile = "@r %s@", stop("Unknown syntax ", 
            pat)), code)
}


knit_engines <- structure(list(get = function (name, default = FALSE, drop = TRUE) 
{
    if (default) 
        defaults = value
    if (missing(name)) 
        defaults
    else {
        if (drop && length(name) == 1) 
            defaults[[name]]
        else {
            setNames(defaults[name], name)
        }
    }
}, set = function (...) 
{
    dots = list(...)
    if (length(dots) == 0) 
        return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) 
        if (length(dots <- dots[[1]]) == 0) 
            return()
    defaults <<- merge(dots)
    invisible(NULL)
}, merge = function (values) 
merge_list(defaults, values), restore = function (target = value) 
defaults <<- target), .Names = c("get", "set", "merge", "restore"
))


knit_expand <- function (file, ..., text = readLines(file, warn = FALSE), delim = c("{{", 
    "}}")) 
{
    if (length(delim) != 2L) 
        stop("\"delim\" must be of length 2")
    delim = gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", delim)
    delim = paste0(delim[1L], "((.|\n)+?)", delim[2L])
    txt = paste(text, collapse = "\n")
    if (packageVersion("stringr") <= "0.9.0") 
        delim = stringr::perl(delim)
    loc = stringr::str_locate_all(txt, delim)[[1L]]
    if (nrow(loc) == 0L) 
        return(txt)
    mat = stringr::str_extract_all(txt, delim)[[1L]]
    mat = sub(delim, "\\1", mat)
    env = list(...)
    env = if (length(env)) 
        list2env(env, parent = parent.frame())
    else parent.frame()
    inline_exec(list(code = mat, input = txt, location = loc), 
        envir = env, hook = identity)
}


spin_child <- function (input, format) 
{
    if (!isTRUE(getOption("knitr.in.progress"))) 
        return(sys.source(input, parent.frame()))
    fmt = if (missing(format)) {
        if (is.null(fmt <- out_format())) 
            stop("spin_child() must be called in a knitting process")
        .spin.fmt = c(latex = "Rnw", sweave = "Rnw", listings = "Rnw", 
            html = "Rhtml", markdown = "Rmd")
        if (is.na(fmt <- .spin.fmt[fmt])) 
            stop("the document format ", fmt, " is not supported yet")
        fmt
    }
    else format
    asis_output(knit_child(text = spin(text = readLines(input), 
        knit = FALSE, report = FALSE, format = fmt), quiet = TRUE))
}


knit2pandoc <- function (input, output = NULL, tangle = FALSE, text = NULL, 
    quiet = FALSE, envir = parent.frame(), encoding = getOption("encoding"), 
    to = "html", pandoc_wrapper = NULL, ...) 
{
    knit_output = knit(input, output, tangle, text, quiet, envir, 
        encoding)
    if (!is.null(pandoc_wrapper)) 
        return(pandoc_wrapper(knit_output, to, ...))
    if (!has_package("rmarkdown")) 
        return(pandoc(knit_output, to, ...))
    output = gsub(paste0(file_ext(knit_output), "$"), to, knit_output)
    rmarkdown::pandoc_convert(knit_output, to, output = output, 
        ...)
}


dep_prev <- function () 
{
    labs = names(knit_code$get())
    if ((n <- length(labs)) < 2L) 
        return()
    opts_knit$set(warn.uncached.dep = FALSE)
    for (i in 1L:(n - 1L)) {
        dep_list$set(setNames(list(labs[(i + 1L):n]), labs[i]))
    }
}


hook_ffmpeg_html <- function (x, options) 
{
    hook_ffmpeg(x, options, options$ffmpeg.format %n% "webm")
}


asis_output <- function (x, meta = NULL, cacheable = NA) 
{
    structure(x, class = "knit_asis", knit_meta = meta, knit_cacheable = cacheable)
}


knit_rd <- function (pkg, links = tools::findHTMLlinks(), frame = TRUE) 
{
    library(pkg, character.only = TRUE)
    optc = opts_chunk$get()
    on.exit(opts_chunk$set(optc))
    file.copy(system.file("misc", "R.css", package = "knitr"), 
        "./")
    pkgRdDB = getFromNamespace("fetchRdDB", "tools")(file.path(find.package(pkg), 
        "help", pkg))
    force(links)
    topics = names(pkgRdDB)
    for (p in topics) {
        message("** knitting documentation of ", p)
        tools::Rd2HTML(pkgRdDB[[p]], f <- tempfile(), package = pkg, 
            Links = links, no_links = is.null(links), stages = "render")
        txt = readLines(f, warn = FALSE)
        unlink(f)
        if (length(i <- grep("<h3>Examples</h3>", txt)) == 1L && 
            length(grep("</pre>", txt[i:length(txt)]))) {
            i0 = grep("<pre>", txt)
            i0 = i0[i0 > i][1L] - 1L
            i1 = grep("</pre>", txt)
            i1 = i1[i1 > i0][1L] + 1L
            tools::Rd2ex(pkgRdDB[[p]], ef <- tempfile())
            ex = readLines(ef, warn = FALSE)
            unlink(ef)
            ex = ex[-(1L:grep("### ** Examples", ex, fixed = TRUE))]
            ex = c("```{r}", ex, "```")
            opts_chunk$set(fig.path = paste0("figure/", p, "-"), 
                tidy = FALSE)
            res = try(knit2html(text = ex, envir = parent.frame(2), 
                fragment.only = TRUE, quiet = TRUE))
            if (inherits(res, "try-error")) {
                res = ex
                res[1] = "<pre><code class=\"r\">"
                res[length(res)] = "</code></pre>"
            }
            txt = c(txt[1:i0], res, txt[i1:length(txt)])
            txt = sub("</head>", "\n<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/styles/github.min.css\">\n<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/highlight.min.js\"></script>\n<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/8.3/languages/r.min.js\"></script>\n<script>hljs.initHighlightingOnLoad();</script>\n</head>", 
                txt)
        }
        else message("no examples found for ", p)
        writeLines(txt, paste0(p, ".html"))
    }
    unlink("figure/", recursive = TRUE)
    toc = sprintf("- <a href=\"%s\" target=\"content\">%s</a>", 
        paste0(topics, ".html"), topics)
    toc = c(paste0("# ", pkg), "", toc, "", paste("Generated with [knitr](https://yihui.name/knitr) ", 
        packageVersion("knitr")))
    markdown::markdownToHTML(text = paste(toc, collapse = "\n"), 
        output = "00frame_toc.html", title = paste("R Documentation of", 
            pkg), options = NULL, extensions = NULL, stylesheet = "R.css")
    txt = readLines(file.path(find.package(pkg), "html", "00Index.html"))
    unlink("00Index.html")
    writeLines(gsub("../../../doc/html/", "http://stat.ethz.ch/R-manual/R-devel/doc/html/", 
        txt, fixed = TRUE), "00Index.html")
    if (!frame) {
        unlink(c("00frame_toc.html", "index.html"))
        (if (is_windows()) 
            file.copy
        else file.symlink)("00Index.html", "index.html")
        return(invisible())
    }
    writeLines(sprintf("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\" \"http://www.w3.org/TR/html4/frameset.dtd\">\n<html>\n<head><title>Documentation of the %s package</title></head>\n<frameset cols=\"15%%,*\">\n  <frame src=\"00frame_toc.html\">\n  <frame src=\"00Index.html\" name=\"content\">\n</frameset>\n</html>\n", 
        pkg), "index.html")
}


pat_brew <- function () 
set_pattern("brew")


knit_meta <- function (class = NULL, clean = TRUE) 
{
    if (is.null(class)) {
        if (clean) 
            on.exit({
                .knitEnv$meta = list()
            }, add = TRUE)
        return(.knitEnv$meta)
    }
    matches = if (length(.knitEnv$meta)) {
        vapply(.knitEnv$meta, inherits, logical(1), what = class)
    }
    if (!any(matches)) 
        return(list())
    if (clean) 
        on.exit({
            .knitEnv$meta[matches] = NULL
            id = attr(.knitEnv$meta, "knit_meta_id")
            if (length(id)) attr(.knitEnv$meta, "knit_meta_id") = id[!matches]
        }, add = TRUE)
    .knitEnv$meta[matches]
}


opts_current <- structure(list(get = function (name, default = FALSE, drop = TRUE) 
{
    if (default) 
        defaults = value
    if (missing(name)) 
        defaults
    else {
        if (drop && length(name) == 1) 
            defaults[[name]]
        else {
            setNames(defaults[name], name)
        }
    }
}, set = function (...) 
{
    dots = list(...)
    if (length(dots) == 0) 
        return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) 
        if (length(dots <- dots[[1]]) == 0) 
            return()
    defaults <<- merge(dots)
    invisible(NULL)
}, merge = function (values) 
merge_list(defaults, values), restore = function (target = value) 
defaults <<- target), .Names = c("get", "set", "merge", "restore"
))


hook_scianimator <- function (x, options) 
{
    x = c(sans_ext(x), file_ext(x))
    fig.num = options$fig.num
    base = opts_knit$get("base.url") %n% ""
    id = gsub("[^[:alnum:]]", "_", options$label)
    sprintf("\n<div class=\"scianimator\">\n<div id=\"%s\" style=\"display: inline-block;\">\n</div>\n</div>\n<script type=\"text/javascript\">\n  (function($) {\n    $(document).ready(function() {\n      var imgs = Array(%s);\n      for (i = 0; ; i++) {\n        if (i == imgs.length) break;\n        imgs[i] = \"%s%s\" + (i + 1) + \".%s\";\n      }\n      $(\"#%s\").scianimator({\n          \"images\": imgs,\n          \"delay\": %s,\n          \"controls\": [\"first\", \"previous\", \"play\", \"next\", \"last\", \"loop\", \"speed\"],\n      });\n      $(\"#%s\").scianimator(\"play\");\n    });\n  })(jQuery);\n</script>\n", 
        id, fig.num, base, sub(paste0(fig.num, "$"), "", x[1]), 
        x[2], id, options$interval * 1000, id)
}


read_demo <- function (topic, package = NULL, ...) 
{
    paths = list.files(file.path(find.package(package), "demo"), 
        full.names = TRUE)
    read_chunk(paths[sans_ext(basename(paths)) == topic], ...)
}


knit_global <- function () 
{
    .knitEnv$knit_global %n% globalenv()
}


pandoc <- function (input, format, config = getOption("config.pandoc"), 
    ext = NA, encoding = getOption("encoding")) 
{
    if (Sys.which("pandoc") == "") 
        stop("Please install pandoc first: http://pandoc.org")
    cfg = if (is.null(config)) 
        sub_ext(input[1L], "pandoc")
    else config
    con = file(input[1L], encoding = encoding)
    tryCatch(txt <- pandoc_cfg(readLines(con, warn = FALSE)), 
        finally = close(con))
    if (file.exists(cfg)) 
        txt = c(txt, "", readLines(cfg, warn = FALSE))
    con = textConnection(txt)
    on.exit(close(con))
    cfg = read.dcf(con)
    nms = colnames(cfg)
    if (length(nms) && "format" %in% nms) {
        warning("the \"format\" field in the configuration must be renamed to \"t\"")
        colnames(cfg)[nms == "format"] = "t"
    }
    if (missing(format)) 
        format = pandoc_fmt(cfg)
    input_utf8 = input
    if (encoding != "UTF-8") {
        for (i in seq_along(input)) {
            input_utf8[i] = gsub("[.]([[:alnum:]]+)$", "_utf8.\\1", 
                input[i])
            encode_utf8(input[i], encoding, input_utf8[i])
        }
        on.exit(unlink(input_utf8), add = TRUE)
    }
    mapply(pandoc_one, input, input_utf8, format, ext, MoreArgs = list(cfg = cfg), 
        USE.NAMES = FALSE)
}


rand_seed <- {
    .GlobalEnv$.Random.seed
}


plot_crop <- function (x, quiet = TRUE) 
{
    ext = tolower(file_ext(x))
    if (ext == "pdf") {
        if (!has_utility("pdfcrop")) 
            return(x)
    }
    else if (!has_utility("convert", "ImageMagick")) 
        return(x)
    if (!quiet) 
        message("cropping ", x)
    x = shQuote(x)
    if (ext == "pdf") {
        cmd = "pdfcrop"
        args = c(x, x)
    }
    else {
        cmd = "convert"
        args = c(x, "-trim", x)
    }
    if (is_windows()) {
        shell(paste(c(cmd, args), collapse = " "))
    }
    else {
        system2(cmd, args = args, stdout = if (quiet) 
            FALSE
        else "")
    }
    x
}


pat_tex <- function () 
set_pattern("tex")


include_app <- function (url, height = "400px") 
{
    orig = url
    if (!grepl("?", url, fixed = TRUE)) 
        url = paste0(url, "?showcase=0")
    include_url2(url, height, orig)
}


restore_raw_output <- function (text, chunks, markers = raw_markers) 
{
    if ((n <- length(chunks)) == 0) 
        return(text)
    text = enc2utf8(text)
    chunks = enc2utf8(chunks)
    tokens = names(chunks)
    for (i in seq_len(n)) {
        r = paste0(markers[1], tokens[i], markers[2])
        text = gsub(r, chunks[i], text, fixed = TRUE, useBytes = TRUE)
    }
    Encoding(text) = "UTF-8"
    text
}




## Package Data

# none


## Package Info

.skeleton_package_title = "A General-Purpose Package for Dynamic Report Generation in R"

.skeleton_package_version = "1.16"

.skeleton_package_depends = ""

.skeleton_package_imports = "evaluate,digest,highr,markdown,stringr,yaml,methods,tools"


## Internal

.skeleton_version = 5


## EOF