##
## Exported symobls in package `igraph`
##

## Exported package methods

write_graph <- function (graph, file, format = c("edgelist", "pajek", "ncol", 
    "lgl", "graphml", "dimacs", "gml", "dot", "leda"), ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.character(file) || length(grep("://", file, fixed = TRUE)) > 
        0 || length(grep("~", file, fixed = TRUE)) > 0) {
        tmpfile <- TRUE
        origfile <- file
        file <- tempfile()
    }
    else {
        tmpfile <- FALSE
    }
    format <- igraph.match.arg(format)
    res <- switch(format, pajek = write.graph.pajek(graph, file, 
        ...), edgelist = write.graph.edgelist(graph, file, ...), 
        ncol = write.graph.ncol(graph, file, ...), lgl = write.graph.lgl(graph, 
            file, ...), graphml = write.graph.graphml(graph, 
            file, ...), dimacs = write.graph.dimacs(graph, file, 
            ...), gml = write.graph.gml(graph, file, ...), dot = write.graph.dot(graph, 
            file, ...), leda = write.graph.leda(graph, file, 
            ...), stop(paste("Unknown file format:", format)))
    if (tmpfile) {
        buffer <- read.graph.toraw(file)
        write.graph.fromraw(buffer, origfile)
    }
    invisible(res)
}


decompose <- function (graph, mode = c("weak", "strong"), max.comps = NA, 
    min.vertices = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, weak = 1, strong = 2)
    if (is.na(max.comps)) {
        max.comps = -1
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_decompose", graph, as.numeric(mode), as.numeric(max.comps), 
        as.numeric(min.vertices), PACKAGE = "igraph")
}


graph.adjacency <- function (adjmatrix, mode = c("directed", "undirected", "max", 
    "min", "upper", "lower", "plus"), weighted = NULL, diag = TRUE, 
    add.colnames = NULL, add.rownames = NA) 
{
    if (inherits(adjmatrix, "Matrix")) {
        res <- graph.adjacency.sparse(adjmatrix, mode = mode, 
            weighted = weighted, diag = diag)
    }
    else {
        res <- graph.adjacency.dense(adjmatrix, mode = mode, 
            weighted = weighted, diag = diag)
    }
    if (is.null(add.colnames)) {
        if (!is.null(colnames(adjmatrix))) {
            add.colnames <- "name"
        }
        else {
            add.colnames <- NA
        }
    }
    else if (!is.na(add.colnames)) {
        if (is.null(colnames(adjmatrix))) {
            warning("No column names to add")
            add.colnames <- NA
        }
    }
    if (is.null(add.rownames)) {
        if (!is.null(rownames(adjmatrix))) {
            add.rownames <- "name"
        }
        else {
            add.colnames <- NA
        }
    }
    else if (!is.na(add.rownames)) {
        if (is.null(rownames(adjmatrix))) {
            warning("No row names to add")
            add.rownames <- NA
        }
    }
    if (!is.na(add.rownames) && !is.na(add.colnames) && add.rownames == 
        add.colnames) {
        warning("Same attribute for columns and rows, row names are ignored")
        add.rownames <- NA
    }
    if (!is.na(add.colnames)) {
        res <- set_vertex_attr(res, add.colnames, value = colnames(adjmatrix))
    }
    if (!is.na(add.rownames)) {
        res <- set_vertex_attr(res, add.rownames, value = rownames(adjmatrix))
    }
    res
}


is_bipartite <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    "type" %in% vertex_attr_names(graph)
}


vertex_attr <- function (graph, name, index = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (missing(name)) {
        if (missing(index)) {
            vertex.attributes(graph)
        }
        else {
            vertex.attributes(graph, index = index)
        }
    }
    else {
        myattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 
            3L, PACKAGE = "igraph")[[as.character(name)]]
        if (!missing(index)) {
            index <- as.igraph.vs(graph, index)
            myattr <- myattr[index]
        }
        myattr
    }
}


ego <- function (graph, order, nodes = V(graph), mode = c("all", "out", 
    "in"), mindist = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    mindist <- as.integer(mindist)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_neighborhood", graph, as.igraph.vs(graph, 
        nodes) - 1, as.numeric(order), as.numeric(mode), mindist, 
        PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


all_shortest_paths <- function (graph, from, to = V(graph), mode = c("out", "all", 
    "in"), weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    if (is.null(weights)) {
        if ("weight" %in% edge_attr_names(graph)) {
            weights <- as.numeric(E(graph)$weight)
        }
    }
    else {
        if (length(weights) == 1 && is.na(weights)) {
            weights <- NULL
        }
        else {
            weights <- as.numeric(weights)
        }
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (is.null(weights)) {
        res <- .Call("R_igraph_get_all_shortest_paths", graph, 
            as.igraph.vs(graph, from) - 1, as.igraph.vs(graph, 
                to) - 1, as.numeric(mode), PACKAGE = "igraph")
    }
    else {
        res <- .Call("R_igraph_get_all_shortest_paths_dijkstra", 
            graph, as.igraph.vs(graph, from) - 1, as.igraph.vs(graph, 
                to) - 1, weights, as.numeric(mode), PACKAGE = "igraph")
    }
    if (igraph_opt("return.vs.es")) {
        res$res <- lapply(res$res, create_vs, graph = graph)
    }
    res
}


layout.circle <- function (..., params = list()) 
{
    do_call(layout_in_circle, .args = c(list(...), params))
}


plotHierarchy <- function (blocks, layout = layout_as_tree(hierarchy(blocks), 
    root = 1), ...) 
{
    plot(hierarchy(blocks), layout = layout, ...)
}


forest.fire.game <- function (nodes, fw.prob, bw.factor = 1, ambs = 1, directed = TRUE) 
{
    nodes <- as.integer(nodes)
    fw.prob <- as.numeric(fw.prob)
    bw.factor <- as.numeric(bw.factor)
    ambs <- as.integer(ambs)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_forest_fire_game", nodes, fw.prob, 
        bw.factor, ambs, directed, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Forest fire model")
    res <- set.graph.attribute(res, "fw.prob", fw.prob)
    res <- set.graph.attribute(res, "bw.factor", bw.factor)
    res <- set.graph.attribute(res, "ambs", ambs)
    res
}


layout_nicely <- function (graph, dim = 2, ...) 
{
    if ("layout" %in% graph_attr_names(graph)) {
        lay <- graph_attr(graph, "layout")
        if (is.function(lay)) {
            lay(graph, ...)
        }
        else {
            lay
        }
    }
    else if (all(c("x", "y") %in% vertex_attr_names(graph))) {
        if ("z" %in% vertex_attr_names(graph)) {
            cbind(V(graph)$x, V(graph)$y, V(graph)$z)
        }
        else {
            cbind(V(graph)$x, V(graph)$y)
        }
    }
    else if (vcount(graph) < 1000) {
        layout_with_fr(graph, dim = dim, ...)
    }
    else {
        layout_with_drl(graph, dim = dim, ...)
    }
}


layout_ <- function (graph, layout, ...) 
{
    modifiers <- list(...)
    stopifnot(all(sapply(modifiers, inherits, what = "igraph_layout_modifier")))
    ids <- sapply(modifiers, "[[", "id")
    stopifnot(all(ids %in% c("component_wise", "normalize")))
    if (anyDuplicated(ids)) 
        stop("Duplicate modifiers")
    names(modifiers) <- ids
    if ("component_wise" %in% ids) {
        graph$id <- seq(vcount(graph))
        comps <- decompose(graph)
        coords <- lapply(comps, function(comp) {
            do_call(layout$fun, list(graph = comp), layout$args)
        })
        all_coords <- merge_coords(comps, coords, method = modifiers[["component_wise"]]$args$merge_method)
        all_coords[unlist(sapply(comps, vertex_attr, "id")), 
            ] <- all_coords[]
        result <- all_coords
    }
    else {
        result <- do_call(layout$fun, list(graph = graph), layout$args)
    }
    if ("normalize" %in% ids) {
        result <- do_call(norm_coords, list(result), modifiers[["normalize"]]$args)
    }
    result
}


centr_eigen_tmax <- function (graph = NULL, nodes = 0, directed = FALSE, scale = TRUE) 
{
    if (!is.null(graph) && !is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.integer(nodes)
    directed <- as.logical(directed)
    scale <- as.logical(scale)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_eigenvector_centrality_tmax", 
        graph, nodes, directed, scale, PACKAGE = "igraph")
    res
}


dyad_census <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_dyad_census", graph, PACKAGE = "igraph")
    res
}


st_cuts <- function (graph, source, target) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    source <- as.igraph.vs(graph, source)
    target <- as.igraph.vs(graph, target)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_all_st_cuts", graph, source - 1, target - 
        1, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$cuts)) {
            res$cuts[[i_]] <- create_es(graph, res$cuts[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$partition1s)) {
            res$partition1s[[i_]] <- create_vs(graph, res$partition1s[[i_]])
        }
    }
    res
}


sample_traits <- function (nodes, types, k = 1, type.dist = rep(1, types), pref.matrix = matrix(1, 
    types, types), directed = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_establishment_game", as.double(nodes), 
        as.double(types), as.double(k), as.double(type.dist), 
        matrix(as.double(pref.matrix), types, types), as.logical(directed), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Trait-based growing graph"
        res$types <- types
        res$k <- k
        res$type.dist <- type.dist
        res$pref.matrix <- pref.matrix
    }
    res
}


from_incidence_matrix <- function (...) 
constructor_spec(graph_from_incidence_matrix, ...)


erdos.renyi.game <- function (n, p.or.m, type = c("gnp", "gnm"), directed = FALSE, 
    loops = FALSE, ...) 
{
    type <- igraph.match.arg(type)
    type1 <- switch(type, gnp = 0, gnm = 1)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_erdos_renyi_game", as.numeric(n), 
        as.numeric(type1), as.numeric(p.or.m), as.logical(directed), 
        as.logical(loops), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("Erdos renyi (%s) graph", type)
        res$type <- type
        res$loops <- loops
        if (type == "gnp") {
            res$p <- p.or.m
        }
        if (type == "gnm") {
            res$m <- p.or.m
        }
    }
    res
}


layout_components <- function (graph, layout = layout_with_kk, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    V(graph)$id <- seq(vcount(graph))
    gl <- decompose(graph)
    ll <- lapply(gl, layout, ...)
    l <- merge_coords(gl, ll)
    l[unlist(sapply(gl, vertex_attr, "id")), ] <- l[]
    l
}


count_subgraph_isomorphisms <- function (pattern, target, method = c("lad", "vf2"), ...) 
{
    method <- igraph.match.arg(method)
    if (method == "lad") {
        length(graph.subisomorphic.lad(pattern, target, all.maps = TRUE, 
            ...)$maps)
    }
    else if (method == "vf2") {
        graph.count.subisomorphisms.vf2(target, pattern, ...)
    }
}


dominator.tree <- function (graph, root, mode = c("out", "in")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    root <- as.igraph.vs(graph, root)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_dominator_tree", graph, root - 1, 
        mode, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res$dom <- create_vs(graph, res$dom)
    }
    if (igraph_opt("return.vs.es")) {
        res$leftout <- create_vs(graph, res$leftout)
    }
    res
}


igraph.drl.refine <- structure(list(edge.cut = 0.8, init.iterations = 0, init.temperature = 50, 
    init.attraction = 0.5, init.damping.mult = 1, liquid.iterations = 0, 
    liquid.temperature = 2000, liquid.attraction = 2, liquid.damping.mult = 1, 
    expansion.iterations = 50, expansion.temperature = 500, expansion.attraction = 0.1, 
    expansion.damping.mult = 0.25, cooldown.iterations = 50, 
    cooldown.temperature = 250, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 50, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 0, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult"))


nexus.search <- function (q, offset = 0, limit = 10, order = c("date", "name", 
    "popularity"), nexus.url = igraph_opt("nexus.url")) 
{
    order = igraph.match.arg(order)
    u <- paste(sep = "", nexus.url, "/api/search?q=", q, "&format=text", 
        "&offset=", offset, "&limit=", limit, "&order=", order)
    f <- url(URLencode(u))
    l <- readLines(f)
    close(f)
    if (length(l) == 0) {
        res <- list()
        class(res) <- "nexusDatasetInfoList"
        return(res)
    }
    nexus.format.result(l, name = paste("q:", q))
}


`%s%` <- function (x, y) 
{
    intersection(x, y)
}


closeness.estimate <- function (graph, vids = V(graph), mode = c("out", "in", "all", 
    "total"), cutoff, weights = NULL, normalized = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    cutoff <- as.numeric(cutoff)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_closeness_estimate", graph, vids - 
        1, mode, cutoff, weights, normalized, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


sample_gnm <- function (n, m, directed = FALSE, loops = FALSE) 
{
    type <- "gnm"
    type1 <- switch(type, gnp = 0, gnm = 1)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_erdos_renyi_game", as.numeric(n), 
        as.numeric(type1), as.numeric(m), as.logical(directed), 
        as.logical(loops), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("Erdos renyi (%s) graph", type)
        res$type <- type
        res$loops <- loops
        res$m <- m
    }
    res
}


from_adjacency <- function (...) 
constructor_spec(graph_from_adjacency_matrix, ...)


graph_from_lcf <- function (n, shifts, repeats = 1) 
{
    n <- as.integer(n)
    shifts <- as.numeric(shifts)
    repeats <- as.integer(repeats)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_lcf_vector", n, shifts, repeats, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "LCF graph")
    res
}


graph_from_graphdb <- function (url = NULL, prefix = "iso", type = "r001", nodes = NULL, 
    pair = "A", which = 0, base = "http://cneurocvs.rmki.kfki.hu/graphdb/gzip", 
    compressed = TRUE, directed = TRUE) 
{
    if (is.null(nodes) && is.null(url)) {
        stop("The `nodes' or the `url' argument must be non-null")
    }
    if (is.null(url)) {
        prefixes <- c("iso", "si6", "mcs10", "mcs30", "mcs50", 
            "mcs70", "mcs90")
        types <- c("r001", "r005", "r01", "r02", "m2D", "m2Dr2", 
            "m2Dr4", "m2Dr6", "m3D", "m3Dr2", "m3Dr4", "m3Dr6", 
            "m4D", "m4Dr2", "m4Dr4", "m4Dr6", "b03", "b03m", 
            "b06", "b06m", "b09", "b09m")
        sizecode <- if (nodes <= 100) 
            "s"
        else if (nodes < 2000) 
            "m"
        else "l"
        typegroups <- c("rand", "rand", "rand", "rand", "m2D", 
            "m2D", "m2D", "m2D", "m2D", "m3D", "m3D", "m3D", 
            "m4D", "m4D", "m4D", "m4D", "bvg", "bvg", "bvg", 
            "bvg", "bvg", "bvg")
        typegroup <- typegroups[which(types == type)]
        if (!prefix %in% prefixes) {
            stop("Invalid prefix!")
        }
        if (!type %in% types) {
            stop("Invalid graph type!")
        }
        suff <- if (compressed) 
            ".gz"
        else ""
        filename <- paste(sep = "", base, "/", prefix, "/", typegroup, 
            "/", type, "/", prefix, "_", type, "_", sizecode, 
            nodes, ".", pair, formatC(which, width = 2, flag = "0"), 
            suff)
    }
    else {
        filename <- url
    }
    f <- try(gzcon(file(filename, open = "rb")))
    if (inherits(f, "try-error")) {
        stop(paste("Cannot open URL:", filename))
    }
    buffer <- read.graph.toraw(f)
    f <- tempfile()
    write.graph.fromraw(buffer, f)
    .Call("R_igraph_read_graph_graphdb", f, as.logical(directed), 
        PACKAGE = "igraph")
}


set_graph_attr <- function (graph, name, value) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    base::.Call("R_igraph_mybracket3_set", graph, 9L, 2L, name, 
        value, PACKAGE = "igraph")
}


edge.betweenness <- function (graph, e = E(graph), directed = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    e <- as.igraph.es(graph, e)
    directed <- as.logical(directed)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_edge_betweenness", graph, directed, 
        weights, PACKAGE = "igraph")
    res[as.numeric(e)]
}


layout.drl <- function (graph, use.seed = FALSE, seed = matrix(runif(vcount(graph) * 
    2), ncol = 2), options = drl_defaults$default, weights = E(graph)$weight, 
    fixed = NULL, dim = 2) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (dim != 2 && dim != 3) {
        stop("`dim' must be 2 or 3")
    }
    use.seed <- as.logical(use.seed)
    seed <- as.matrix(seed)
    options.tmp <- drl_defaults$default
    options.tmp[names(options)] <- options
    options <- options.tmp
    if (!is.null(weights)) {
        weights <- as.numeric(weights)
    }
    if (!is.null(fixed)) {
        fixed <- as.logical(fixed)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (dim == 2) {
        res <- .Call("R_igraph_layout_drl", graph, seed, use.seed, 
            options, weights, fixed, PACKAGE = "igraph")
    }
    else {
        res <- .Call("R_igraph_layout_drl_3d", graph, seed, use.seed, 
            options, weights, fixed, PACKAGE = "igraph")
    }
    res
}


incident <- function (graph, v, mode = c("all", "out", "in", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is_directed(graph)) {
        mode <- igraph.match.arg(mode)
        mode <- switch(mode, out = 1, `in` = 2, all = 3, total = 3)
    }
    else {
        mode = 1
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_incident", graph, as.igraph.vs(graph, 
        v) - 1, as.numeric(mode), PACKAGE = "igraph") + 1L
    if (igraph_opt("return.vs.es")) 
        res <- create_es(graph, res)
    res
}


sample_gnp <- function (n, p, directed = FALSE, loops = FALSE) 
{
    type <- "gnp"
    type1 <- switch(type, gnp = 0, gnm = 1)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_erdos_renyi_game", as.numeric(n), 
        as.numeric(type1), as.numeric(p), as.logical(directed), 
        as.logical(loops), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("Erdos renyi (%s) graph", type)
        res$type <- type
        res$loops <- loops
        res$p <- p
    }
    res
}


graph.isocreate <- function (size, number, directed = TRUE) 
{
    size <- as.integer(size)
    number <- as.integer(number)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_isoclass_create", size, number, directed, 
        PACKAGE = "igraph")
    res
}


as.igraph <- function (x, ...) 
UseMethod("as.igraph")


layout_with_graphopt <- function (graph, start = NULL, niter = 500, charge = 0.001, mass = 30, 
    spring.length = 0, spring.constant = 1, max.sa.movement = 5) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(start)) {
        start <- structure(as.numeric(start), dim = dim(start))
    }
    niter <- as.double(niter)
    charge <- as.double(charge)
    mass <- as.double(mass)
    spring.length <- as.double(spring.length)
    spring.constant <- as.double(spring.constant)
    max.sa.movement <- as.double(max.sa.movement)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_layout_graphopt", graph, niter, charge, mass, 
        spring.length, spring.constant, max.sa.movement, start, 
        PACKAGE = "igraph")
}


from_edgelist <- function (...) 
constructor_spec(graph_from_edgelist, ...)


cited.type.game <- function (n, edges = 1, types = rep(0, n), pref = rep(1, length(types)), 
    directed = TRUE, attr = TRUE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_cited_type_game", as.numeric(n), as.numeric(edges), 
        as.numeric(types), as.numeric(pref), as.logical(directed), 
        PACKAGE = "igraph")
    if (attr) {
        V(res)$type <- types
    }
    if (igraph_opt("add.params")) {
        res$name <- "Random citation graph (cited type)"
        res$edges <- edges
    }
    res
}


authority.score <- function (graph, scale = TRUE, weights = NULL, options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    scale <- as.logical(scale)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_authority_score", graph, scale, weights, 
        options, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$vector) <- vertex_attr(graph, "name", )
    }
    res
}


independent.vertex.sets <- function (graph, min = NULL, max = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(min)) {
        min <- 0
    }
    if (is.null(max)) {
        max <- 0
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_independent_vertex_sets", graph, as.numeric(min), 
        as.numeric(max), PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


centralization.betweenness <- function (graph, directed = TRUE, nobigint = TRUE, normalized = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    nobigint <- as.logical(nobigint)
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_betweenness", graph, 
        directed, nobigint, normalized, PACKAGE = "igraph")
    res
}


isomorphic <- function (graph1, graph2, method = c("auto", "direct", "vf2", 
    "bliss"), ...) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    method <- igraph.match.arg(method)
    if (method == "auto") {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_isomorphic", graph1, graph2, PACKAGE = "igraph")
    }
    else if (method == "direct") {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_isomorphic_34", graph1, graph2, PACKAGE = "igraph")
    }
    else if (method == "vf2") {
        graph.isomorphic.vf2(graph1, graph2, ...)$iso
    }
    else if (method == "bliss") {
        graph.isomorphic.bliss(graph1, graph2, ...)$iso
    }
}


triad.census <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_triad_census", graph, PACKAGE = "igraph")
    res
}


nexus.list <- function (tags = NULL, offset = 0, limit = 10, operator = c("or", 
    "and"), order = c("date", "name", "popularity"), nexus.url = igraph_opt("nexus.url")) 
{
    operator = igraph.match.arg(operator)
    order = igraph.match.arg(order)
    if (is.null(tags)) {
        u <- paste(sep = "", nexus.url, "/api/dataset_info?format=text", 
            "&offset=", offset, "&limit=", limit, "&order=", 
            order)
        name <- "data set list"
    }
    else {
        tags <- paste(tags, collapse = "|")
        u <- paste(sep = "", nexus.url, "/api/dataset_info?tag=", 
            tags, "&operator=", operator, "&format=text", "&offset=", 
            offset, "&limit=", limit, "&order=", order)
        name <- paste("tags:", gsub("|", "; ", tags, fixed = TRUE))
    }
    f <- url(URLencode(u))
    l <- readLines(f)
    close(f)
    nexus.format.result(l, name)
}


layout_with_gem <- function (graph, coords = NULL, maxiter = 40 * vcount(graph)^2, 
    temp.max = vcount(graph), temp.min = 1/10, temp.init = sqrt(vcount(graph))) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(coords)) {
        coords <- as.matrix(structure(as.double(coords), dim = dim(coords)))
        use.seed <- TRUE
    }
    else {
        coords <- matrix(ncol = 2, nrow = 0)
        use.seed <- FALSE
    }
    maxiter <- as.integer(maxiter)
    temp.max <- as.numeric(temp.max)
    temp.min <- as.numeric(temp.min)
    temp.init <- as.numeric(temp.init)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_gem", graph, coords, use.seed, 
        maxiter, temp.max, temp.min, temp.init, PACKAGE = "igraph")
    res
}


independence.number <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_independence_number", graph, PACKAGE = "igraph")
}


tkplot.canvas <- function (tkp.id) 
{
    .tkplot.get(tkp.id)$canvas
}


maximum.bipartite.matching <- function (graph, types = NULL, weights = NULL, eps = .Machine$double.eps) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        types <- as.logical(types)
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    eps <- as.numeric(eps)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maximum_bipartite_matching", graph, 
        types, weights, eps, PACKAGE = "igraph")
    res$matching[res$matching == 0] <- NA
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$matching <- V(graph)$name[res$matching]
        names(res$matching) <- V(graph)$name
    }
    res
}


tk_coords <- function (tkp.id, norm = FALSE) 
{
    coords <- .tkplot.get(tkp.id, "coords")
    coords[, 2] <- max(coords[, 2]) - coords[, 2]
    if (norm) {
        coords[, 1] <- coords[, 1] - min(coords[, 1])
        coords[, 2] <- coords[, 2] - min(coords[, 2])
        coords[, 1] <- coords[, 1]/max(coords[, 1]) - 0.5
        coords[, 2] <- coords[, 2]/max(coords[, 2]) - 0.5
    }
    coords
}


which_loop <- function (graph, eids = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_is_loop", graph, as.igraph.es(graph, eids) - 
        1, PACKAGE = "igraph")
}


ecount <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_ecount", graph, PACKAGE = "igraph")
}


make_graph <- function (edges, ..., n = max(edges), isolates = NULL, directed = TRUE, 
    dir = directed, simplify = TRUE) 
{
    if (class(edges) == "formula") {
        if (!missing(n)) 
            stop("'n' should not be given for graph literals")
        if (!missing(isolates)) {
            stop("'isolates' should not be given for graph literals")
        }
        if (!missing(directed)) {
            stop("'directed' should not be given for graph literals")
        }
        mf <- as.list(match.call())[-1]
        mf[[1]] <- mf[[1]][[2]]
        graph_from_literal_i(mf)
    }
    else {
        if (!missing(simplify)) {
            stop("'simplify' should not be given for graph literals")
        }
        if (!missing(dir) && !missing(directed)) {
            stop("Only give one of 'dir' and 'directed'")
        }
        if (!missing(dir) && missing(directed)) 
            directed <- dir
        if (is.character(edges) && length(edges) == 1) {
            if (!missing(n)) 
                warning("'n' is ignored for the '", edges, "' graph")
            if (!missing(isolates)) {
                warning("'isolates' is ignored for the '", edges, 
                  "' graph")
            }
            if (!missing(directed)) {
                warning("'directed' is ignored for the '", edges, 
                  "' graph")
            }
            if (!missing(dir)) {
                warning("'dir' is ignored for the '", edges, 
                  "' graph")
            }
            if (length(list(...))) 
                stop("Extra arguments in make_graph")
            make_famous_graph(edges)
        }
        else if (is.numeric(edges) || is.null(edges) || (is.logical(edges) && 
            length(edges) == 0)) {
            if (is.null(edges) || is.logical(edges)) 
                edges <- as.numeric(edges)
            if (!is.null(isolates)) {
                warning("'isolates' ignored for numeric edge list")
            }
            old_graph <- function(edges, n = max(edges), directed = TRUE) {
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_create", as.numeric(edges) - 
                  1, as.numeric(n), as.logical(directed), PACKAGE = "igraph")
            }
            args <- list(edges, ...)
            if (!missing(n)) 
                args <- c(args, list(n = n))
            if (!missing(directed)) 
                args <- c(args, list(directed = directed))
            do.call(old_graph, args)
        }
        else if (is.character(edges)) {
            if (!missing(n)) {
                warning("'n' is ignored for edge list with vertex names")
            }
            if (length(list(...))) 
                stop("Extra arguments in make_graph")
            el <- matrix(edges, ncol = 2, byrow = TRUE)
            res <- graph_from_edgelist(el, directed = directed)
            if (!is.null(isolates)) {
                isolates <- as.character(isolates)
                res <- res + vertices(isolates)
            }
            res
        }
        else {
            stop("'edges' must be numeric or character")
        }
    }
}


vertex_connectivity <- function (graph, source = NULL, target = NULL, checks = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(source) && is.null(target)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_vertex_connectivity", graph, as.logical(checks), 
            PACKAGE = "igraph")
    }
    else if (!is.null(source) && !is.null(target)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_st_vertex_connectivity", graph, as.igraph.vs(graph, 
            source) - 1, as.igraph.vs(graph, target) - 1, PACKAGE = "igraph")
    }
    else {
        stop("either give both source and target or neither")
    }
}


set.edge.attribute <- function (graph, name, index = E(graph), value) 
{
    i_set_edge_attr(graph = graph, name = name, index = index, 
        value = value)
}


as_bipartite <- function (...) 
layout_spec(layout_as_bipartite, ...)


no.clusters <- function (graph, mode = c("weak", "strong")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, weak = 1, strong = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_no_clusters", graph, as.numeric(mode), PACKAGE = "igraph")
}


count_triangles <- function (graph, vids = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_adjacent_triangles", graph, vids - 
        1, PACKAGE = "igraph")
    res
}


triangles <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_list_triangles", graph, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res <- create_vs(graph, res)
    }
    res
}


nexus_search <- function (q, offset = 0, limit = 10, order = c("date", "name", 
    "popularity"), nexus.url = igraph_opt("nexus.url")) 
{
    order = igraph.match.arg(order)
    u <- paste(sep = "", nexus.url, "/api/search?q=", q, "&format=text", 
        "&offset=", offset, "&limit=", limit, "&order=", order)
    f <- url(URLencode(u))
    l <- readLines(f)
    close(f)
    if (length(l) == 0) {
        res <- list()
        class(res) <- "nexusDatasetInfoList"
        return(res)
    }
    nexus.format.result(l, name = paste("q:", q))
}


graph.cohesion <- function (x, ...) 
UseMethod("cohesion")


sample_cit_cit_types <- function (n, edges = 1, types = rep(0, n), pref = matrix(1, nrow = length(types), 
    ncol = length(types)), directed = TRUE, attr = TRUE) 
{
    pref <- structure(as.numeric(pref), dim = dim(pref))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_citing_cited_type_game", as.numeric(n), 
        as.numeric(types), pref, as.numeric(edges), as.logical(directed), 
        PACKAGE = "igraph")
    if (attr) {
        V(res)$type <- types
    }
    if (igraph_opt("add.params")) {
        res$name <- "Random citation graph (citing & cited type)"
        res$edges <- edges
    }
    res
}


tk_fit <- function (tkp.id, width = NULL, height = NULL) 
{
    tkp <- .tkplot.get(tkp.id)
    if (is.null(width)) {
        width <- as.numeric(tcltk::tkwinfo("width", tkp$canvas))
    }
    if (is.null(height)) {
        height <- as.numeric(tcltk::tkwinfo("height", tkp$canvas))
    }
    coords <- .tkplot.get(tkp.id, "coords")
    coords[, 1] <- coords[, 1] - min(coords[, 1])
    coords[, 2] <- coords[, 2] - min(coords[, 2])
    coords[, 1] <- coords[, 1]/max(coords[, 1]) * (width - (tkp$params$padding[2] + 
        tkp$params$padding[4]))
    coords[, 2] <- coords[, 2]/max(coords[, 2]) * (height - (tkp$params$padding[1] + 
        tkp$params$padding[3]))
    coords[, 1] <- coords[, 1] + tkp$params$padding[2]
    coords[, 2] <- coords[, 2] + tkp$params$padding[3]
    .tkplot.set(tkp.id, "coords", coords)
    .tkplot.update.vertices(tkp.id)
    invisible(NULL)
}


tk_postscript <- function (tkp.id) 
{
    tkp <- .tkplot.get(tkp.id)
    filename <- tcltk::tkgetSaveFile(initialfile = "Rplots.eps", 
        defaultextension = "eps", title = "Export graph to PostScript file")
    tcltk::tkpostscript(tkp$canvas, file = filename)
    invisible(NULL)
}


are_adjacent <- function (graph, v1, v2) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_are_connected", graph, as.igraph.vs(graph, 
        v1) - 1, as.igraph.vs(graph, v2) - 1, PACKAGE = "igraph")
}


sample_hierarchical_sbm <- function (n, m, rho, C, p) 
{
    mlen <- length(m)
    rholen <- if (is.list(rho)) 
        length(rho)
    else 1
    Clen <- if (is.list(C)) 
        length(C)
    else 1
    commonlen <- unique(c(mlen, rholen, Clen))
    if (length(commonlen) == 1 && commonlen == 1) {
        hsbm.1.game(n, m, rho, C, p)
    }
    else {
        commonlen <- setdiff(commonlen, 1)
        if (length(commonlen) != 1) {
            stop("Lengths of `m', `rho' and `C' must match")
        }
        m <- rep(m, length.out = commonlen)
        rho <- if (is.list(rho)) {
            rep(rho, length.out = commonlen)
        }
        else {
            rep(list(rho), length.out = commonlen)
        }
        C <- if (is.list(C)) {
            rep(C, length.out = commonlen)
        }
        else {
            rep(list(C), length.out = commonlen)
        }
        hsbm.list.game(n, m, rho, C, p)
    }
}


cohesive_blocks <- function (graph, labels = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_cohesive_blocks", graph, PACKAGE = "igraph")
    class(res) <- "cohesiveBlocks"
    if (labels && "name" %in% vertex_attr_names(graph)) {
        res$labels <- V(graph)$name
    }
    if (igraph_opt("return.vs.es")) {
        res$blocks <- lapply(res$blocks, create_vs, graph = graph)
    }
    res$vcount <- vcount(graph)
    res
}


`%u%` <- function (x, y) 
{
    union(x, y)
}


graph.adhesion <- function (graph, checks = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_adhesion", graph, as.logical(checks), PACKAGE = "igraph")
}


graphlets.candidate.basis <- function (graph, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    graph2 <- graph
    graph2[[9]] <- list(c(1, 0, 1), list(), list(), list())
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_graphlets_candidate_basis", graph2, 
        weights, PACKAGE = "igraph")
    res
}


estimate_closeness <- function (graph, vids = V(graph), mode = c("out", "in", "all", 
    "total"), cutoff, weights = NULL, normalized = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    cutoff <- as.numeric(cutoff)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_closeness_estimate", graph, vids - 
        1, mode, cutoff, weights, normalized, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


make_ego_graph <- function (graph, order, nodes = V(graph), mode = c("all", "out", 
    "in"), mindist = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    mindist <- as.integer(mindist)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_neighborhood_graphs", graph, as.igraph.vs(graph, 
        nodes) - 1, as.numeric(order), as.numeric(mode), mindist, 
        PACKAGE = "igraph")
    res
}


vertex.shapes <- function (shape = NULL) 
{
    if (is.null(shape)) {
        ls(.igraph.shapes)
    }
    else {
        .igraph.shapes[[shape]]
    }
}


centr_betw <- function (graph, directed = TRUE, nobigint = TRUE, normalized = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    nobigint <- as.logical(nobigint)
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_betweenness", graph, 
        directed, nobigint, normalized, PACKAGE = "igraph")
    res
}


with_graphopt <- function (...) 
layout_spec(layout_with_graphopt, ...)


igraphtest <- function () 
{
    do.call(require, list("testthat"))
    tdir <- system.file("tests", package = "igraph")
    do.call("test_dir", list(tdir))
}


make_empty_graph <- function (n = 0, directed = TRUE) 
{
    n <- as.integer(n)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_empty", n, directed, PACKAGE = "igraph")
    res
}


de_bruijn_graph <- function (...) 
constructor_spec(make_de_bruijn_graph, ...)


ego_size <- function (graph, order, nodes = V(graph), mode = c("all", "out", 
    "in"), mindist = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    mindist <- as.integer(mindist)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_neighborhood_size", graph, as.igraph.vs(graph, 
        nodes) - 1, as.numeric(order), as.numeric(mode), mindist, 
        PACKAGE = "igraph")
}


consensus_tree <- function (graph, hrg = NULL, start = FALSE, num.samples = 10000) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    start <- as.logical(start)
    num.samples <- as.integer(num.samples)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_consensus", graph, hrg, start, 
        num.samples, PACKAGE = "igraph")
    res
}


layout.spring <- function (graph, ...) 
{
    warning("Spring layout was removed, we use Fruchterman-Reingold instead.")
    layout_with_fr(graph)
}


is.matching <- function (graph, matching, types = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        types <- as.logical(types)
    }
    matching <- as.igraph.vs(graph, matching, na.ok = TRUE) - 
        1
    matching[is.na(matching)] <- -1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_matching", graph, types, matching, 
        PACKAGE = "igraph")
    res
}


diverging_pal <- function (n) 
{
    stopifnot(n > 0)
    x <- list("#F1A340", c("#F1A340", "#F7F7F7"), c("#F1A340", 
        "#F7F7F7", "#998EC3"), c("#E66101", "#FDB863", "#B2ABD2", 
        "#5E3C99"), c("#E66101", "#FDB863", "#F7F7F7", "#B2ABD2", 
        "#5E3C99"), c("#B35806", "#F1A340", "#FEE0B6", "#D8DAEB", 
        "#998EC3", "#542788"), c("#B35806", "#F1A340", "#FEE0B6", 
        "#F7F7F7", "#D8DAEB", "#998EC3", "#542788"), c("#B35806", 
        "#E08214", "#FDB863", "#FEE0B6", "#D8DAEB", "#B2ABD2", 
        "#8073AC", "#542788"), c("#B35806", "#E08214", "#FDB863", 
        "#FEE0B6", "#F7F7F7", "#D8DAEB", "#B2ABD2", "#8073AC", 
        "#542788"), c("#7F3B08", "#B35806", "#E08214", "#FDB863", 
        "#FEE0B6", "#D8DAEB", "#B2ABD2", "#8073AC", "#542788", 
        "#2D004B"), c("#7F3B08", "#B35806", "#E08214", "#FDB863", 
        "#FEE0B6", "#F7F7F7", "#D8DAEB", "#B2ABD2", "#8073AC", 
        "#542788", "#2D004B"))
    if (n > length(x)) 
        warning("Cannot make ", n, " divergent colors")
    n <- min(n, length(x))
    if (n == 0) 
        character()
    else x[[n]]
}


tk_rotate <- function (tkp.id, degree = NULL, rad = NULL) 
{
    coords <- .tkplot.get(tkp.id, "coords")
    if (is.null(degree) && is.null(rad)) {
        rad <- pi/2
    }
    else if (is.null(rad) && !is.null(degree)) {
        rad <- degree/180 * pi
    }
    center <- c(mean(range(coords[, 1])), mean(range(coords[, 
        2])))
    phi <- atan2(coords[, 2] - center[2], coords[, 1] - center[1])
    r <- sqrt((coords[, 1] - center[1])^2 + (coords[, 2] - center[2])^2)
    phi <- phi + rad
    coords[, 1] <- r * cos(phi)
    coords[, 2] <- r * sin(phi)
    .tkplot.set(tkp.id, "coords", coords)
    tk_center(tkp.id)
    invisible(NULL)
}


without_multiples <- function () 
{
    constructor_modifier(id = "without_multiples")
}


layout_with_mds <- function (graph, dist = NULL, dim = 2, options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(dist)) 
        dist <- structure(as.double(dist), dim = dim(dist))
    dim <- as.integer(dim)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_mds", graph, dist, dim, PACKAGE = "igraph")
    res
}


bibcoupling <- function (graph, v = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    v <- as.igraph.vs(graph, v)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_bibcoupling", graph, v - 1, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        rownames(res) <- vertex_attr(graph, "name", v)
        colnames(res) <- vertex_attr(graph, "name")
    }
    res
}


match_vertices <- function (A, B, m, start, iteration) 
{
    totv <- ncol(A)
    n <- totv - m
    if (m != 0) {
        A12 <- A[1:m, (m + 1):(m + n), drop = FALSE]
        A21 <- A[(m + 1):(m + n), 1:m, drop = FALSE]
        B12 <- B[1:m, (m + 1):(m + n), drop = FALSE]
        B21 <- B[(m + 1):(m + n), 1:m, drop = FALSE]
    }
    if (m == 0) {
        A12 <- Matrix::Matrix(0, n, n)
        A21 <- Matrix::Matrix(0, n, n)
        B12 <- Matrix::Matrix(0, n, n)
        B21 <- Matrix::Matrix(0, n, n)
    }
    A22 <- A[(m + 1):(m + n), (m + 1):(m + n)]
    B22 <- B[(m + 1):(m + n), (m + 1):(m + n)]
    patience <- iteration
    tol <- 1
    P <- start
    toggle <- 1
    iter <- 0
    while (toggle == 1 & iter < patience) {
        iter <- iter + 1
        x <- A21 %*% Matrix::t(B21)
        y <- Matrix::t(A12) %*% B12
        z <- A22 %*% P %*% Matrix::t(B22)
        w <- Matrix::t(A22) %*% P %*% B22
        Grad <- x + y + z + w
        ind <- unclass(solve_LSAP(as.matrix(Grad), maximum = TRUE))
        ind2 <- cbind(1:n, ind)
        T <- Matrix::Diagonal(n)
        T <- T[ind, ]
        wt <- Matrix::t(A22)[, order(ind)] %*% B22
        c <- sum(w * P)
        d <- sum(wt * P) + sum(w[ind2])
        e <- sum(wt[ind2])
        u <- sum(P * (x + y))
        v <- sum((x + y)[ind2])
        if (c - d + e == 0 && d - 2 * e + u - v == 0) {
            alpha <- 0
        }
        else {
            alpha <- -(d - 2 * e + u - v)/(2 * (c - d + e))
        }
        f0 <- 0
        f1 <- c - e + u - v
        falpha <- (c - d + e) * alpha^2 + (d - 2 * e + u - v) * 
            alpha
        if (alpha < tol && alpha > 0 && falpha > f0 && falpha > 
            f1) {
            P <- alpha * P + (1 - alpha) * T
        }
        else if (f0 > f1) {
            P <- T
        }
        else {
            toggle <- 0
        }
    }
    D <- P
    corr <- matrix(solve_LSAP(as.matrix(P), maximum = TRUE))
    P = Matrix::diag(n)
    P = rbind(cbind(Matrix::diag(m), matrix(0, m, n)), cbind(matrix(0, 
        n, m), P[corr, ]))
    corr <- cbind(matrix((m + 1):totv, n), matrix(m + corr, n))
    list(corr = corr, P = P, D = D)
}


edge_betweenness <- function (graph, e = E(graph), directed = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    e <- as.igraph.es(graph, e)
    directed <- as.logical(directed)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_edge_betweenness", graph, directed, 
        weights, PACKAGE = "igraph")
    res[as.numeric(e)]
}


sample_cit_types <- function (n, edges = 1, types = rep(0, n), pref = rep(1, length(types)), 
    directed = TRUE, attr = TRUE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_cited_type_game", as.numeric(n), as.numeric(edges), 
        as.numeric(types), as.numeric(pref), as.logical(directed), 
        PACKAGE = "igraph")
    if (attr) {
        V(res)$type <- types
    }
    if (igraph_opt("add.params")) {
        res$name <- "Random citation graph (cited type)"
        res$edges <- edges
    }
    res
}


make_directed_graph <- function (edges, n = max(edges)) 
{
    if (missing(n)) {
        make_graph(edges, directed = TRUE)
    }
    else {
        make_graph(edges, n = n, directed = TRUE)
    }
}


getIgraphOpt <- function (x, default = NULL) 
{
    if (missing(default)) 
        return(igraph_options(x)[[1L]])
    if (x %in% names(igraph_options())) 
        igraph_options(x)[[1L]]
    else default
}


in_circle <- function (...) 
layout_spec(layout_in_circle, ...)


E <- function (graph, P = NULL, path = NULL, directed = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    update_es_ref(graph)
    if (!is.null(P) && !is.null(path)) {
        stop("Cannot give both `P' and `path' at the same time")
    }
    if (is.null(P) && is.null(path)) {
        ec <- ecount(graph)
        res <- seq_len(ec)
    }
    else if (!is.null(P)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        res <- .Call("R_igraph_es_pairs", graph, as.igraph.vs(graph, 
            P) - 1, as.logical(directed), PACKAGE = "igraph") + 
            1
    }
    else {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        res <- .Call("R_igraph_es_path", graph, as.igraph.vs(graph, 
            path) - 1, as.logical(directed), PACKAGE = "igraph") + 
            1
    }
    if ("name" %in% edge_attr_names(graph)) {
        names(res) <- edge_attr(graph)$name[res]
    }
    if (is.named(graph)) {
        el <- ends(graph, es = res)
        attr(res, "vnames") <- paste(el[, 1], el[, 2], sep = "|")
    }
    class(res) <- "igraph.es"
    add_vses_graph_ref(res, graph)
}


set.vertex.attribute <- function (graph, name, index = V(graph), value) 
{
    i_set_vertex_attr(graph = graph, name = name, index = index, 
        value = value)
}


delete_edge_attr <- function (graph, name) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    name <- as.character(name)
    if (!name %in% edge_attr_names(graph)) {
        stop("No such edge attribute: ", name)
    }
    eattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 4L, 
        PACKAGE = "igraph")
    eattr[[name]] <- NULL
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 4L, eattr, 
        PACKAGE = "igraph")
}


.igraph.progress <- function (percent, message, clean = FALSE) 
{
    if (clean) {
        if (!is.null(.igraph.pb)) {
            close(.igraph.pb)
        }
        return(invisible())
    }
    type <- igraph_opt("verbose")
    if (is.logical(type) && type) {
        .igraph.progress.txt(percent, message)
    }
    else {
        switch(type, tk = .igraph.progress.tk(percent, message), 
            tkconsole = .igraph.progress.tkconsole(percent, message), 
            stop("Cannot interpret 'verbose' option, this should not happen"))
    }
}


graphs_from_cohesive_blocks <- function (blocks, graph) 
{
    lapply(blocks(blocks), induced_subgraph, graph = graph)
}


igraph.options <- function (...) 
{
    if (nargs() == 0) 
        return(.igraph.pars)
    current <- .igraph.pars
    temp <- list(...)
    if (length(temp) == 1 && is.null(names(temp))) {
        arg <- temp[[1]]
        switch(mode(arg), list = temp <- arg, character = return(.igraph.pars[arg]), 
            stop("invalid argument: ", sQuote(arg)))
    }
    if (length(temp) == 0) 
        return(current)
    n <- names(temp)
    if (is.null(n)) 
        stop("options must be given by name")
    env <- asNamespace("igraph")
    cb <- intersect(names(igraph.pars.callbacks), n)
    for (cn in cb) {
        temp[[cn]] <- igraph.pars.callbacks[[cn]](temp[[cn]])
    }
    current <- .igraph.pars
    current[n] <- temp
    assign(".igraph.pars", current, envir = env)
    invisible(current)
}


graph.subisomorphic.vf2 <- function (graph1, graph2, vertex.color1, vertex.color2, edge.color1, 
    edge.color2) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    if (missing(vertex.color1)) {
        if ("color" %in% vertex_attr_names(graph1)) {
            vertex.color1 <- V(graph1)$color
        }
        else {
            vertex.color1 <- NULL
        }
    }
    if (!is.null(vertex.color1)) {
        vertex.color1 <- as.integer(vertex.color1) - 1L
    }
    if (missing(vertex.color2)) {
        if ("color" %in% vertex_attr_names(graph2)) {
            vertex.color2 <- V(graph2)$color
        }
        else {
            vertex.color2 <- NULL
        }
    }
    if (!is.null(vertex.color2)) {
        vertex.color2 <- as.integer(vertex.color2) - 1L
    }
    if (missing(edge.color1)) {
        if ("color" %in% edge_attr_names(graph1)) {
            edge.color1 <- E(graph1)$color
        }
        else {
            edge.color1 <- NULL
        }
    }
    if (!is.null(edge.color1)) {
        edge.color1 <- as.integer(edge.color1) - 1L
    }
    if (missing(edge.color2)) {
        if ("color" %in% edge_attr_names(graph2)) {
            edge.color2 <- E(graph2)$color
        }
        else {
            edge.color2 <- NULL
        }
    }
    if (!is.null(edge.color2)) {
        edge.color2 <- as.integer(edge.color2) - 1L
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_subisomorphic_vf2", graph1, graph2, 
        vertex.color1, vertex.color2, edge.color1, edge.color2, 
        PACKAGE = "igraph")
    res
}


subcomponent <- function (graph, v, mode = c("all", "out", "in")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_subcomponent", graph, as.igraph.vs(graph, 
        v) - 1, as.numeric(mode), PACKAGE = "igraph") + 1L
    if (igraph_opt("return.vs.es")) 
        res <- create_vs(graph, res)
    res
}


shortest_paths <- function (graph, from, to = V(graph), mode = c("out", "all", 
    "in"), weights = NULL, output = c("vpath", "epath", "both"), 
    predecessors = FALSE, inbound.edges = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    output <- igraph.match.arg(output)
    output <- switch(output, vpath = 0, epath = 1, both = 2)
    if (is.null(weights)) {
        if ("weight" %in% edge_attr_names(graph)) {
            weights <- as.numeric(E(graph)$weight)
        }
    }
    else {
        if (length(weights) == 1 && is.na(weights)) {
            weights <- NULL
        }
        else {
            weights <- as.numeric(weights)
        }
    }
    to <- as.igraph.vs(graph, to) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_shortest_paths", graph, as.igraph.vs(graph, 
        from) - 1, to, as.numeric(mode), as.numeric(length(to)), 
        weights, as.numeric(output), as.logical(predecessors), 
        as.logical(inbound.edges), PACKAGE = "igraph")
    if (!is.null(res$vpath)) {
        res$vpath <- lapply(res$vpath, function(x) x + 1)
    }
    if (!is.null(res$epath)) {
        res$epath <- lapply(res$epath, function(x) x + 1)
    }
    if (!is.null(res$predecessors)) {
        res$predecessors <- res$predecessors + 1
    }
    if (!is.null(res$inbound_edges)) {
        res$inbound_edges <- res$inbound_edges + 1
    }
    if (igraph_opt("return.vs.es")) {
        if (!is.null(res$vpath)) {
            res$vpath <- lapply(res$vpath, create_vs, graph = graph)
        }
        if (!is.null(res$epath)) {
            res$epath <- lapply(res$epath, create_es, graph = graph)
        }
        if (!is.null(res$predecessors)) {
            res$predecessors <- create_vs(res$predecessors, graph = graph, 
                na_ok = TRUE)
        }
        if (!is.null(res$inbound_edges)) {
            res$inbound_edges <- create_es(res$inbound_edges, 
                graph = graph, na_ok = TRUE)
        }
    }
    res
}


cutat <- function (communities, no, steps) 
{
    if (!inherits(communities, "communities")) {
        stop("Not a community structure")
    }
    if (!is_hierarchical(communities)) {
        stop("Not a hierarchical communitity structure")
    }
    if ((!missing(no) && !missing(steps)) || (missing(no) && 
        missing(steps))) {
        stop("Please give either `no' or `steps' (but not both)")
    }
    if (!missing(steps)) {
        mm <- merges(communities)
        if (steps > nrow(mm)) {
            warning("Cannot make that many steps")
            steps <- nrow(mm)
        }
        community.to.membership2(mm, communities$vcount, steps)
    }
    else {
        mm <- merges(communities)
        noc <- communities$vcount - nrow(mm)
        if (no < noc) {
            warning("Cannot have that few communities")
            no = noc
        }
        steps <- communities$vcount - no
        community.to.membership2(mm, communities$vcount, steps)
    }
}


bipartite.random.game <- function (n1, n2, type = c("gnp", "gnm"), p, m, directed = FALSE, 
    mode = c("out", "in", "all")) 
{
    n1 <- as.integer(n1)
    n2 <- as.integer(n2)
    type <- igraph.match.arg(type)
    if (!missing(p)) {
        p <- as.numeric(p)
    }
    if (!missing(m)) {
        m <- as.integer(m)
    }
    directed <- as.logical(directed)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3)
    if (type == "gnp" && missing(p)) {
        stop("Connection probability `p' is not given for Gnp graph")
    }
    if (type == "gnp" && !missing(m)) {
        warning("Number of edges `m' is ignored for Gnp graph")
    }
    if (type == "gnm" && missing(m)) {
        stop("Number of edges `m' is not given for Gnm graph")
    }
    if (type == "gnm" && !missing(p)) {
        warning("Connection probability `p' is ignored for Gnp graph")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (type == "gnp") {
        res <- .Call("R_igraph_bipartite_game_gnp", n1, n2, p, 
            directed, mode, PACKAGE = "igraph")
        res <- set_vertex_attr(res$graph, "type", value = res$types)
        res$name <- "Bipartite Gnp random graph"
        res$p <- p
    }
    else if (type == "gnm") {
        res <- .Call("R_igraph_bipartite_game_gnm", n1, n2, m, 
            directed, mode, PACKAGE = "igraph")
        res <- set_vertex_attr(res$graph, "type", value = res$types)
        res$name <- "Bipartite Gnm random graph"
        res$m <- m
    }
    res
}


get.edge <- function (graph, id) 
{
    .Deprecated("ends", msg = paste("'get.edge' is deperecated, please use", 
        "'ends' instead."))
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    id <- as.numeric(id)
    ec <- ecount(graph)
    if (id < 1 || id > ec) {
        stop("No such edge")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_edge", graph, as.numeric(id) - 
        1, PACKAGE = "igraph")
    res + 1
}


sir <- function (graph, beta, gamma, no.sim = 100) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    beta <- as.numeric(beta)
    gamma <- as.numeric(gamma)
    no.sim <- as.integer(no.sim)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_sir", graph, beta, gamma, no.sim, 
        PACKAGE = "igraph")
    class(res) <- "sir"
    res
}


union <- function (...) 
UseMethod("union")


with_vertex_ <- function (...) 
{
    args <- grab_args()
    constructor_modifier(id = "with_vertex_", args = args)
}


count_components <- function (graph, mode = c("weak", "strong")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, weak = 1, strong = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_no_clusters", graph, as.numeric(mode), PACKAGE = "igraph")
}


drl_defaults <- structure(list(coarsen = structure(list(edge.cut = 0.8, init.iterations = 0, 
    init.temperature = 2000, init.attraction = 10, init.damping.mult = 1, 
    liquid.iterations = 200, liquid.temperature = 2000, liquid.attraction = 2, 
    liquid.damping.mult = 1, expansion.iterations = 200, expansion.temperature = 2000, 
    expansion.attraction = 10, expansion.damping.mult = 1, cooldown.iterations = 200, 
    cooldown.temperature = 2000, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 50, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 100, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult")), coarsest = structure(list(
    edge.cut = 0.8, init.iterations = 0, init.temperature = 2000, 
    init.attraction = 10, init.damping.mult = 1, liquid.iterations = 200, 
    liquid.temperature = 2000, liquid.attraction = 2, liquid.damping.mult = 1, 
    expansion.iterations = 200, expansion.temperature = 2000, 
    expansion.attraction = 10, expansion.damping.mult = 1, cooldown.iterations = 200, 
    cooldown.temperature = 2000, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 200, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 100, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult")), default = structure(list(
    edge.cut = 0.8, init.iterations = 0, init.temperature = 2000, 
    init.attraction = 10, init.damping.mult = 1, liquid.iterations = 200, 
    liquid.temperature = 2000, liquid.attraction = 10, liquid.damping.mult = 1, 
    expansion.iterations = 200, expansion.temperature = 2000, 
    expansion.attraction = 2, expansion.damping.mult = 1, cooldown.iterations = 200, 
    cooldown.temperature = 2000, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 50, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 100, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult")), final = structure(list(
    edge.cut = 0.8, init.iterations = 0, init.temperature = 50, 
    init.attraction = 0.5, init.damping.mult = 0, liquid.iterations = 0, 
    liquid.temperature = 2000, liquid.attraction = 2, liquid.damping.mult = 1, 
    expansion.iterations = 50, expansion.temperature = 2000, 
    expansion.attraction = 2, expansion.damping.mult = 1, cooldown.iterations = 50, 
    cooldown.temperature = 200, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 50, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 25, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult")), refine = structure(list(
    edge.cut = 0.8, init.iterations = 0, init.temperature = 50, 
    init.attraction = 0.5, init.damping.mult = 1, liquid.iterations = 0, 
    liquid.temperature = 2000, liquid.attraction = 2, liquid.damping.mult = 1, 
    expansion.iterations = 50, expansion.temperature = 500, expansion.attraction = 0.1, 
    expansion.damping.mult = 0.25, cooldown.iterations = 50, 
    cooldown.temperature = 250, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 50, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 0, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult"))), .Names = c("coarsen", 
"coarsest", "default", "final", "refine"))


sample_dot_product <- function (vecs, directed = FALSE) 
{
    vecs <- as.matrix(structure(as.double(vecs), dim = dim(vecs)))
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_dot_product_game", vecs, directed, 
        PACKAGE = "igraph")
    res
}


layout_with_sugiyama <- function (graph, layers = NULL, hgap = 1, vgap = 1, maxiter = 100, 
    weights = NULL, attributes = c("default", "all", "none")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(layers)) 
        layers <- as.numeric(layers) - 1
    hgap <- as.numeric(hgap)
    vgap <- as.numeric(vgap)
    maxiter <- as.integer(maxiter)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    attributes <- igraph.match.arg(attributes)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_sugiyama", graph, layers, hgap, 
        vgap, maxiter, weights, PACKAGE = "igraph")
    res$res[, 2] <- max(res$res[, 2]) - res$res[, 2] + 1
    vc <- vcount(graph)
    res$layout <- res$res[seq_len(vc), ]
    if (nrow(res$res) == vc) {
        res$layout.dummy <- matrix(nrow = 0, ncol = 2)
    }
    else {
        res$layout.dummy <- res$res[(vc + 1):nrow(res$res), ]
    }
    E(res$extd_graph)$orig <- res$extd_to_orig_eids
    res$extd_to_orig_eids <- NULL
    res$extd_graph <- set_vertex_attr(res$extd_graph, "dummy", 
        value = c(rep(FALSE, vc), rep(TRUE, nrow(res$res) - vc)))
    res$extd_graph$layout <- rbind(res$layout, res$layout.dummy)
    if (attributes == "default" || attributes == "all") {
        if ("size" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$size <- 0
            V(res$extd_graph)$size[!V(res$extd_graph)$dummy] <- V(graph)$size
        }
        if ("size2" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$size2 <- 0
            V(res$extd_graph)$size2[!V(res$extd_graph)$dummy] <- V(graph)$size2
        }
        if ("shape" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$shape <- "none"
            V(res$extd_graph)$shape[!V(res$extd_graph)$dummy] <- V(graph)$shape
        }
        if ("label" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$label <- ""
            V(res$extd_graph)$label[!V(res$extd_graph)$dummy] <- V(graph)$label
        }
        if ("color" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$color <- head(V(graph)$color, 1)
            V(res$extd_graph)$color[!V(res$extd_graph)$dummy] <- V(graph)$color
        }
        eetar <- as_edgelist(res$extd_graph, names = FALSE)[, 
            2]
        E(res$extd_graph)$arrow.mode <- 0
        if ("arrow.mode" %in% edge_attr_names(graph)) {
            E(res$extd_graph)$arrow.mode[eetar <= vc] <- E(graph)$arrow.mode
        }
        else {
            E(res$extd_graph)$arrow.mode[eetar <= vc] <- is_directed(graph) * 
                2
        }
        if ("arrow.size" %in% edge_attr_names(graph)) {
            E(res$extd_graph)$arrow.size <- 0
            E(res$extd_graph)$arrow.size[eetar <= vc] <- E(graph)$arrow.size
        }
    }
    if (attributes == "all") {
        gatt <- setdiff(graph_attr_names(graph), "layout")
        vatt <- setdiff(vertex_attr_names(graph), c("size", "size2", 
            "shape", "label", "color"))
        eatt <- setdiff(edge_attr_names(graph), c("arrow.mode", 
            "arrow.size"))
        for (ga in gatt) {
            res$extd_graph <- set_graph_attr(res$extd_graph, 
                ga, graph_attr(graph, ga))
        }
        for (va in vatt) {
            notdummy <- which(!V(res$extd_graph)$dummy)
            res$extd_graph <- set_vertex_attr(res$extd_graph, 
                va, notdummy, vertex_attr(graph, va))
        }
        for (ea in eatt) {
            eanew <- edge_attr(graph, ea)[E(res$extd_graph)$orig]
            res$extd_graph <- set_edge_attr(res$extd_graph, ea, 
                value = eanew)
        }
    }
    res$res <- NULL
    res
}


V <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    update_vs_ref(graph)
    res <- seq_len(vcount(graph))
    if (is_named(graph)) 
        names(res) <- vertex_attr(graph)$name
    class(res) <- "igraph.vs"
    add_vses_graph_ref(res, graph)
}


ring <- function (...) 
constructor_spec(make_ring, ...)


bipartite_projection_size <- function (graph, types = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        if (!is.logical(types)) {
            warning("vertex types converted to logical")
        }
        types <- as.logical(types)
        if (any(is.na(types))) {
            stop("`NA' is not allowed in vertex types")
        }
    }
    else {
        stop("Not a bipartite graph, supply `types' argument")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_bipartite_projection_size", graph, 
        types, PACKAGE = "igraph")
    res
}


layout.norm <- function (layout, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, 
    zmax = 1) 
{
    if (!is.matrix(layout)) {
        stop("`layout' not a matrix")
    }
    if (ncol(layout) != 2 && ncol(layout) != 3) {
        stop("`layout' should have 2 or three columns")
    }
    if (!is.null(xmin) && !is.null(xmax)) {
        layout[, 1] <- .layout.norm.col(layout[, 1], xmin, xmax)
    }
    if (!is.null(ymin) && !is.null(ymax)) {
        layout[, 2] <- .layout.norm.col(layout[, 2], ymin, ymax)
    }
    if (ncol(layout) == 3 && !is.null(zmin) && !is.null(zmax)) {
        layout[, 3] <- .layout.norm.col(layout[, 3], zmin, zmax)
    }
    layout
}


establishment.game <- function (nodes, types, k = 1, type.dist = rep(1, types), pref.matrix = matrix(1, 
    types, types), directed = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_establishment_game", as.double(nodes), 
        as.double(types), as.double(k), as.double(type.dist), 
        matrix(as.double(pref.matrix), types, types), as.logical(directed), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Trait-based growing graph"
        res$types <- types
        res$k <- k
        res$type.dist <- type.dist
        res$pref.matrix <- pref.matrix
    }
    res
}


shortest.paths <- function (graph, v = V(graph), to = V(graph), mode = c("all", 
    "out", "in"), weights = NULL, algorithm = c("automatic", 
    "unweighted", "dijkstra", "bellman-ford", "johnson")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    v <- as.igraph.vs(graph, v)
    to <- as.igraph.vs(graph, to)
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    algorithm <- igraph.match.arg(algorithm)
    algorithm <- switch(algorithm, automatic = 0, unweighted = 1, 
        dijkstra = 2, `bellman-ford` = 3, johnson = 4)
    if (is.null(weights)) {
        if ("weight" %in% edge_attr_names(graph)) {
            weights <- as.numeric(E(graph)$weight)
        }
    }
    else {
        if (length(weights) == 1 && is.na(weights)) {
            weights <- NULL
        }
        else {
            weights <- as.numeric(weights)
        }
    }
    if (!is.null(weights) && algorithm == 1) {
        weights <- NULL
        warning("Unweighted algorithm chosen, weights ignored")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_shortest_paths", graph, v - 1, to - 
        1, as.numeric(mode), weights, as.numeric(algorithm), 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        rownames(res) <- V(graph)$name[v]
        colnames(res) <- V(graph)$name[to]
    }
    res
}


tkplot.fit.to.screen <- function (tkp.id, width = NULL, height = NULL) 
{
    tkp <- .tkplot.get(tkp.id)
    if (is.null(width)) {
        width <- as.numeric(tcltk::tkwinfo("width", tkp$canvas))
    }
    if (is.null(height)) {
        height <- as.numeric(tcltk::tkwinfo("height", tkp$canvas))
    }
    coords <- .tkplot.get(tkp.id, "coords")
    coords[, 1] <- coords[, 1] - min(coords[, 1])
    coords[, 2] <- coords[, 2] - min(coords[, 2])
    coords[, 1] <- coords[, 1]/max(coords[, 1]) * (width - (tkp$params$padding[2] + 
        tkp$params$padding[4]))
    coords[, 2] <- coords[, 2]/max(coords[, 2]) * (height - (tkp$params$padding[1] + 
        tkp$params$padding[3]))
    coords[, 1] <- coords[, 1] + tkp$params$padding[2]
    coords[, 2] <- coords[, 2] + tkp$params$padding[3]
    .tkplot.set(tkp.id, "coords", coords)
    .tkplot.update.vertices(tkp.id)
    invisible(NULL)
}


bipartite.mapping <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_bipartite", graph, PACKAGE = "igraph")
    res
}


count_max_cliques <- function (graph, min = NULL, max = NULL, subset = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(min)) {
        min <- 0
    }
    if (is.null(max)) {
        max <- 0
    }
    min <- as.integer(min)
    max <- as.integer(max)
    if (!is.null(subset)) {
        subset <- as.integer(as.igraph.vs(graph, subset) - 1)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maximal_cliques_count", graph, subset, 
        min, max, PACKAGE = "igraph")
    res
}


graph_from_literal <- function (..., simplify = TRUE) 
{
    mf <- as.list(match.call())[-1]
    graph_from_literal_i(mf)
}


graph.compose <- function (g1, g2, byname = "auto") 
{
    if (!is_igraph(g1) || !is_igraph(g2)) {
        stop("Not a graph object")
    }
    if (byname != "auto" && !is.logical(byname)) {
        stop("`byname' must be \"auto\", or logical")
    }
    nonamed <- is_named(g1) + is_named(g2)
    if (byname == "auto") {
        byname <- nonamed == 2
        if (nonamed == 1) {
            warning("One, but not both graphs are named, not using vertex names")
        }
    }
    else if (byname && nonamed != 2) {
        stop("Some graphs are not named")
    }
    if (byname) {
        uninames <- unique(c(V(g1)$name, V(g2)$name))
        if (vcount(g1) < length(uninames)) {
            g1 <- g1 + setdiff(uninames, V(g1)$name)
        }
        if (vcount(g2) < length(uninames)) {
            g2 <- g2 + setdiff(uninames, V(g2)$name)
        }
        if (any(uninames != V(g1)$name)) {
            g1 <- permute(g1, match(V(g1)$name, uninames))
        }
        if (any(uninames != V(g2)$name)) {
            g2 <- permute(g2, match(V(g2)$name, uninames))
        }
    }
    edgemaps <- (length(edge_attr_names(g1)) != 0 || length(edge_attr_names(g2)) != 
        0)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_compose", g1, g2, edgemaps, PACKAGE = "igraph")
    maps <- list(res$edge_map1, res$edge_map2)
    res <- res$graph
    graphs <- list(g1, g2)
    graph.attributes(res) <- rename.attr.if.needed("g", graphs)
    if (byname) {
        vertex.attributes(res) <- rename.attr.if.needed("v", 
            graphs, vcount(res), ignore = "name")
        V(res)$name <- uninames
    }
    else {
        vertex.attributes(res) <- rename.attr.if.needed("v", 
            graphs, vcount(res))
    }
    if (edgemaps) {
        edge.attributes(res) <- rename.attr.if.needed("e", graphs, 
            ecount(res), maps2 = maps)
    }
    res
}


permute <- function (graph, permutation) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    permutation <- as.numeric(permutation) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_permute_vertices", graph, permutation, 
        PACKAGE = "igraph")
    res
}


write.graph <- function (graph, file, format = c("edgelist", "pajek", "ncol", 
    "lgl", "graphml", "dimacs", "gml", "dot", "leda"), ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.character(file) || length(grep("://", file, fixed = TRUE)) > 
        0 || length(grep("~", file, fixed = TRUE)) > 0) {
        tmpfile <- TRUE
        origfile <- file
        file <- tempfile()
    }
    else {
        tmpfile <- FALSE
    }
    format <- igraph.match.arg(format)
    res <- switch(format, pajek = write.graph.pajek(graph, file, 
        ...), edgelist = write.graph.edgelist(graph, file, ...), 
        ncol = write.graph.ncol(graph, file, ...), lgl = write.graph.lgl(graph, 
            file, ...), graphml = write.graph.graphml(graph, 
            file, ...), dimacs = write.graph.dimacs(graph, file, 
            ...), gml = write.graph.gml(graph, file, ...), dot = write.graph.dot(graph, 
            file, ...), leda = write.graph.leda(graph, file, 
            ...), stop(paste("Unknown file format:", format)))
    if (tmpfile) {
        buffer <- read.graph.toraw(file)
        write.graph.fromraw(buffer, origfile)
    }
    invisible(res)
}


bonpow <- function (graph, nodes = V(graph), loops = FALSE, exponent = 1, 
    rescale = FALSE, tol = 1e-07, sparse = TRUE) 
{
    nodes <- as.igraph.vs(graph, nodes)
    if (sparse) {
        res <- bonpow.sparse(graph, nodes, loops, exponent, rescale, 
            tol)
    }
    else {
        res <- bonpow.dense(graph, nodes, loops, exponent, rescale, 
            tol)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", nodes)
    }
    res
}


as_adjacency_matrix <- function (graph, type = c("both", "upper", "lower"), attr = NULL, 
    edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!sparse) {
        get.adjacency.dense(graph, type = type, attr = attr, 
            edges = edges, names = names)
    }
    else {
        get.adjacency.sparse(graph, type = type, attr = attr, 
            edges = edges, names = names)
    }
}


tkigraph <- function () 
{
    requireNamespace("tcltk", quietly = TRUE) || stop("tcl/tk library not available")
    options(scipen = 10000)
    if (!exists("window", envir = .tkigraph.env, inherits = FALSE)) {
        assign("window", TRUE, envir = .tkigraph.env)
        assign("graphs", list(), envir = .tkigraph.env)
        assign("selected", list(), envir = .tkigraph.env)
        assign("tklines", list(), envir = .tkigraph.env)
    }
    else {
        stop("tkigraph window is already open!")
    }
    top <- tcltk::tktoplevel(background = "lightgrey", width = 700, 
        height = 400)
    tcltk::tktitle(top) <- "iGraph GUI (Social Network Basics)"
    topframe <- tcltk::tkframe(top, relief = "sunken", borderwidth = 1)
    scr <- tcltk::tkscrollbar(top, repeatinterval = 5, command = function(...) tcltk::tkyview(topframe))
    tcltk::tkplace(topframe, x = 0, y = 0, relwidth = 1)
    if (!exists("top", envir = .tkigraph.env, inherits = FALSE)) {
        assign("top", top, envir = .tkigraph.env)
        assign("topframe", topframe, envir = .tkigraph.env)
    }
    tcltk::tkbind(top, "<Destroy>", function() .tkigraph.close())
    main.menu <- tcltk::tkmenu(top)
    graph.menu <- tcltk::tkmenu(main.menu)
    create.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(create.menu, "command", label = "By hand", command = function() {
        .tkigraph.by.hand()
    })
    tcltk::tkadd(create.menu, "separator")
    tcltk::tkadd(create.menu, "command", label = "Ring", command = function() {
        .tkigraph.ring()
    })
    tcltk::tkadd(create.menu, "command", label = "Tree", command = function() {
        .tkigraph.tree()
    })
    tcltk::tkadd(create.menu, "command", label = "Lattice", command = function() {
        .tkigraph.lattice()
    })
    tcltk::tkadd(create.menu, "command", label = "Star", command = function() {
        .tkigraph.star()
    })
    tcltk::tkadd(create.menu, "command", label = "Full", command = function() {
        .tkigraph.full()
    })
    tcltk::tkadd(create.menu, "separator")
    tcltk::tkadd(create.menu, "command", label = "Graph atlas...", 
        command = function() {
            .tkigraph.atlas()
        })
    tcltk::tkadd(create.menu, "separator")
    tcltk::tkadd(create.menu, "command", label = "Moody-White network", 
        command = function() {
            g <- graph_from_adjacency_matrix(.tkigraph.net.moody.white, 
                mode = "undirected")
            g <- set_graph_attr(g, "name", "Moody-White network")
            .tkigraph.add.graph(g)
        })
    tcltk::tkadd(create.menu, "separator")
    tcltk::tkadd(create.menu, "command", label = "Random (Erdos-Renyi G(n,p))", 
        command = function() {
            .tkigraph.erdos.renyi.game()
        })
    tcltk::tkadd(create.menu, "command", label = "Random (Erdos-Renyi G(n,m))", 
        command = function() {
            .tkigraph.erdos.renyi.gnm.game()
        })
    tcltk::tkadd(create.menu, "command", label = "Random (Barabasi-Albert)", 
        command = function() {
            .tkigraph.barabasi.game()
        })
    tcltk::tkadd(create.menu, "command", label = "Random (Configuration model)", 
        command = function() {
            .tkigraph.degree.sequence.game()
        })
    tcltk::tkadd(create.menu, "command", label = "Watts-Strogatz random graph", 
        command = function() {
            .tkigraph.watts.strogatz()
        })
    tcltk::tkadd(create.menu, "separator")
    tcltk::tkadd(create.menu, "command", label = "Simplify", 
        command = function() {
            .tkigraph.simplify()
        })
    tcltk::tkadd(graph.menu, "cascade", label = "Create", menu = create.menu)
    tcltk::tkadd(graph.menu, "command", label = "Delete", command = function() {
        .tkigraph.delete()
    })
    tcltk::tkadd(graph.menu, "separator")
    tcltk::tkadd(graph.menu, "command", label = "Show graph", 
        command = function() {
            .tkigraph.show()
        })
    tcltk::tkadd(graph.menu, "command", label = "Basic statistics", 
        command = function() {
            .tkigraph.stat()
        })
    tcltk::tkadd(graph.menu, "separator")
    tcltk::tkadd(graph.menu, "command", label = "Import session", 
        command = function() {
            .tkigraph.load()
        })
    tcltk::tkadd(graph.menu, "command", label = "Export session", 
        command = function() {
            .tkigraph.save()
        })
    tcltk::tkadd(graph.menu, "separator")
    tcltk::tkadd(graph.menu, "command", label = "Import adjacency matrix", 
        command = function() .tkigraph.import.adjacency())
    tcltk::tkadd(graph.menu, "command", label = "Import edge list", 
        command = function() .tkigraph.import.edgelist())
    tcltk::tkadd(graph.menu, "command", label = "Import Pajek file", 
        command = function() .tkigraph.import.pajek())
    tcltk::tkadd(graph.menu, "command", label = "Export adjacency matrix", 
        command = function() .tkigraph.export.adjacency())
    tcltk::tkadd(graph.menu, "command", label = "Export edge list", 
        command = function() .tkigraph.export.edgelist())
    tcltk::tkadd(graph.menu, "command", label = "Export Pajek file", 
        command = function() .tkigraph.export.pajek())
    tcltk::tkadd(main.menu, "cascade", label = "Graph", menu = graph.menu)
    plot.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(plot.menu, "command", label = "Simple", command = function() {
        .tkigraph.plot(simple = TRUE)
    })
    tcltk::tkadd(plot.menu, "command", label = "Advanced", command = function() {
        .tkigraph.plot(simple = FALSE)
    })
    tcltk::tkadd(main.menu, "cascade", label = "Draw", menu = plot.menu)
    centrality.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(centrality.menu, "command", label = "Degree (out)", 
        command = function() {
            .tkigraph.degree("out")
        })
    tcltk::tkadd(centrality.menu, "command", label = "Degree (in)", 
        command = function() {
            .tkigraph.degree("in")
        })
    tcltk::tkadd(centrality.menu, "command", label = "Degree (total)", 
        command = function() {
            .tkigraph.degree("total")
        })
    tcltk::tkadd(centrality.menu, "command", label = "Plot log-log degree distribution", 
        command = function() {
            .tkigraph.degree.dist(power = FALSE)
        })
    tcltk::tkadd(centrality.menu, "command", label = "Fit a power-law to degree distribution", 
        command = function() {
            .tkigraph.degree.dist(power = TRUE)
        })
    tcltk::tkadd(centrality.menu, "separator")
    tcltk::tkadd(centrality.menu, "command", label = "Closeness", 
        command = function() {
            .tkigraph.closeness()
        })
    tcltk::tkadd(centrality.menu, "command", label = "Betweenness", 
        command = function() {
            .tkigraph.betweenness()
        })
    tcltk::tkadd(centrality.menu, "command", label = "Burt's constraint", 
        command = function() {
            .tkigraph.constraints()
        })
    tcltk::tkadd(centrality.menu, "command", label = "Page rank", 
        command = function() {
            .tkigraph.page.rank()
        })
    tcltk::tkadd(centrality.menu, "separator")
    tcltk::tkadd(centrality.menu, "command", label = "Edge betweenness", 
        command = function() {
            .tkigraph.edge.betweenness()
        })
    tcltk::tkadd(main.menu, "cascade", label = "Centrality", 
        menu = centrality.menu)
    distances.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(distances.menu, "command", label = "Distance matrix", 
        command = function() {
            .tkigraph.dist.matrix()
        })
    tcltk::tkadd(distances.menu, "command", label = "Distances from/to vertex", 
        command = function() {
            .tkigraph.distance.tofrom()
        })
    tcltk::tkadd(distances.menu, "command", label = "Diameter (undirected)", 
        command = function() {
            .tkigraph.diameter()
        })
    tcltk::tkadd(distances.menu, "command", label = "Draw diameter", 
        command = function() {
            .tkigraph.plot.diameter(simple = FALSE)
        })
    tcltk::tkadd(distances.menu, "command", label = "Average path length (undirected)", 
        command = function() {
            .tkigraph.diameter(mode = "path")
        })
    tcltk::tkadd(main.menu, "cascade", label = "Distances", menu = distances.menu)
    component.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(component.menu, "command", label = "Show components", 
        command = function() {
            .tkigraph.clusters()
        })
    tcltk::tkadd(component.menu, "command", label = "Show membership", 
        command = function() {
            .tkigraph.clusters.membership()
        })
    tcltk::tkadd(component.menu, "command", label = "Calculate component sizes", 
        command = function() {
            .tkigraph.calculate.clusters()
        })
    tcltk::tkadd(component.menu, "command", label = "Draw components", 
        command = function() {
            .tkigraph.plot.comp(simple = FALSE)
        })
    tcltk::tkadd(component.menu, "command", label = "Create graph from giant component", 
        command = function() {
            .tkigraph.create.giantcomp()
        })
    tcltk::tkadd(component.menu, "command", label = "Create graph from component of a vertex", 
        command = function() {
            .tkigraph.create.mycomp()
        })
    tcltk::tkadd(component.menu, "command", label = "Create graph from a component", 
        command = function() {
            .tkigraph.create.comp()
        })
    community.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(community.menu, "command", label = "Spinglass algorithm", 
        command = function() {
            .tkigraph.spinglass()
        })
    tcltk::tkadd(community.menu, "command", label = "Spinglass algorithm, single vertex", 
        command = function() {
            .tkigraph.my.spinglass()
        })
    cohesion.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(cohesion.menu, "command", label = "Cohesion of all components", 
        command = function() {
            .tkigraph.cohesion()
        })
    subgraph.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(subgraph.menu, "cascade", label = "Components", 
        menu = component.menu)
    tcltk::tkadd(subgraph.menu, "cascade", label = "Communities", 
        menu = community.menu)
    tcltk::tkadd(subgraph.menu, "cascade", label = "Cohesion", 
        menu = cohesion.menu)
    tcltk::tkadd(main.menu, "cascade", label = "Subgraphs", menu = subgraph.menu)
    motif.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(motif.menu, "command", label = "Draw motifs", 
        command = function() {
            .tkigraph.motifs.draw()
        })
    tcltk::tkadd(motif.menu, "command", label = "Find motifs", 
        command = function() {
            .tkigraph.motifs.find()
        })
    tcltk::tkadd(main.menu, "cascade", label = "Motifs", menu = motif.menu)
    help.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(help.menu, "command", label = "Contents", command = function() {
        .tkigraph.help()
    })
    tcltk::tkadd(help.menu, "command", label = "In external browser", 
        command = function() {
            .tkigraph.help.external()
        })
    tcltk::tkadd(help.menu, "separator")
    tcltk::tkadd(help.menu, "command", label = "About", command = function() {
        .tkigraph.about()
    })
    tcltk::tkadd(main.menu, "cascade", label = "Help", menu = help.menu)
    tcltk::tkadd(main.menu, "command", label = "Quit", command = .tkigraph.close)
    tcltk::tkconfigure(top, "-menu", main.menu)
    tcltk::tkgrid(tcltk::tklabel(top, text = ""), tcltk::tklabel(top, 
        text = "#", justify = "center", relief = "raised"), tcltk::tklabel(top, 
        text = "Name", width = 50, relief = "raised", justify = "left"), 
        tcltk::tklabel(top, text = "|V|", width = 6, relief = "raised", 
            justify = "left"), tcltk::tklabel(top, text = "|E|", 
            width = 6, relief = "raised", justify = "left"), 
        tcltk::tklabel(top, text = "Dir.", width = 6, relief = "raised", 
            justify = "left"), sticky = "nsew", `in` = topframe)
    tcltk::tkgrid.columnconfigure(topframe, 2, weight = 1)
    invisible(NULL)
}


sample_forestfire <- function (nodes, fw.prob, bw.factor = 1, ambs = 1, directed = TRUE) 
{
    nodes <- as.integer(nodes)
    fw.prob <- as.numeric(fw.prob)
    bw.factor <- as.numeric(bw.factor)
    ambs <- as.integer(ambs)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_forest_fire_game", nodes, fw.prob, 
        bw.factor, ambs, directed, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Forest fire model")
    res <- set.graph.attribute(res, "fw.prob", fw.prob)
    res <- set.graph.attribute(res, "bw.factor", bw.factor)
    res <- set.graph.attribute(res, "ambs", ambs)
    res
}


sequential_pal <- function (n) 
{
    stopifnot(n >= 0)
    x <- list("#FEE8C8", c("#FEE8C8", "#FDBB84"), c("#FEE8C8", 
        "#FDBB84", "#E34A33"), c("#FEF0D9", "#FDCC8A", "#FC8D59", 
        "#D7301F"), c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", 
        "#B30000"), c("#FEF0D9", "#FDD49E", "#FDBB84", "#FC8D59", 
        "#E34A33", "#B30000"), c("#FEF0D9", "#FDD49E", "#FDBB84", 
        "#FC8D59", "#EF6548", "#D7301F", "#990000"), c("#FFF7EC", 
        "#FEE8C8", "#FDD49E", "#FDBB84", "#FC8D59", "#EF6548", 
        "#D7301F", "#990000"), c("#FFF7EC", "#FEE8C8", "#FDD49E", 
        "#FDBB84", "#FC8D59", "#EF6548", "#D7301F", "#B30000", 
        "#7F0000"))
    if (n > length(x)) 
        warning("Cannot make ", n, " sequential colors")
    n <- min(n, length(x))
    if (n == 0) 
        character()
    else x[[n]]
}


`vertex_attr<-` <- function (graph, name, index = V(graph), value) 
{
    if (missing(name)) {
        `vertex.attributes<-`(graph, index = index, value = value)
    }
    else {
        set_vertex_attr(graph, name = name, index = index, value = value)
    }
}


preference.game <- function (nodes, types, type.dist = rep(1, types), fixed.sizes = FALSE, 
    pref.matrix = matrix(1, types, types), directed = FALSE, 
    loops = FALSE) 
{
    if (nrow(pref.matrix) != types || ncol(pref.matrix) != types) {
        stop("Invalid size for preference matrix")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_preference_game", as.double(nodes), 
        as.double(types), as.double(type.dist), as.logical(fixed.sizes), 
        matrix(as.double(pref.matrix), types, types), as.logical(directed), 
        as.logical(loops), PACKAGE = "igraph")
    V(res[[1]])$type <- res[[2]] + 1
    if (igraph_opt("add.params")) {
        res[[1]]$name <- "Preference random graph"
        res[[1]]$types <- types
        res[[1]]$type.dist <- type.dist
        res[[1]]$fixed.sizes <- fixed.sizes
        res[[1]]$pref.matrix <- pref.matrix
        res[[1]]$loops <- loops
    }
    res[[1]]
}


leading.eigenvector.community <- function (graph, steps = -1, weights = NULL, start = NULL, options = arpack_defaults, 
    callback = NULL, extra = NULL, env = parent.frame()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    steps <- as.integer(steps)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (!is.null(start)) {
        start <- as.numeric(start) - 1
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_leading_eigenvector", graph, 
        steps, weights, options, start, callback, extra, env, 
        environment(igraph.i.levc.arp), PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$algorithm <- "leading eigenvector"
    res$vcount <- vcount(graph)
    res$membership <- res$membership + 1
    res$merges <- res$merges + 1
    res$history <- res$history + 1
    class(res) <- "communities"
    res
}


merge_coords <- function (graphs, layouts, method = "dla") 
{
    if (!all(sapply(graphs, is_igraph))) {
        stop("Not a graph object")
    }
    if (method == "dla") {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        res <- .Call("R_igraph_layout_merge_dla", graphs, layouts, 
            PACKAGE = "igraph")
    }
    else {
        stop("Invalid `method'.")
    }
    res
}


is.hierarchical <- function (communities) 
{
    !is.null(communities$merges)
}


optimal.community <- function (graph, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_optimal_modularity", graph, 
        weights, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "optimal"
    res$membership <- res$membership + 1
    class(res) <- "communities"
    res
}


edge_connectivity <- function (graph, source = NULL, target = NULL, checks = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(source) && is.null(target)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_edge_connectivity", graph, as.logical(checks), 
            PACKAGE = "igraph")
    }
    else if (!is.null(source) && !is.null(target)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_st_edge_connectivity", graph, as.igraph.vs(graph, 
            source) - 1, as.igraph.vs(graph, target) - 1, PACKAGE = "igraph")
    }
    else {
        stop("either give both source and target or neither")
    }
}


largest_cliques <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_largest_cliques", graph, PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


as_adj_list <- function (graph, mode = c("all", "out", "in", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- as.numeric(switch(mode, out = 1, `in` = 2, all = 3, 
        total = 3))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_adjlist", graph, mode, PACKAGE = "igraph")
    res <- lapply(res, function(x) V(graph)[x + 1])
    if (is_named(graph)) 
        names(res) <- V(graph)$name
    res
}


full_bipartite_graph <- function (...) 
constructor_spec(make_full_bipartite_graph, ...)


sample_sphere_surface <- function (dim, n = 1, radius = 1, positive = TRUE) 
{
    dim <- as.integer(dim)
    n <- as.integer(n)
    radius <- as.numeric(radius)
    positive <- as.logical(positive)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_sample_sphere_surface", dim, n, radius, 
        positive, PACKAGE = "igraph")
    res
}


sample_bipartite <- function (n1, n2, type = c("gnp", "gnm"), p, m, directed = FALSE, 
    mode = c("out", "in", "all")) 
{
    n1 <- as.integer(n1)
    n2 <- as.integer(n2)
    type <- igraph.match.arg(type)
    if (!missing(p)) {
        p <- as.numeric(p)
    }
    if (!missing(m)) {
        m <- as.integer(m)
    }
    directed <- as.logical(directed)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3)
    if (type == "gnp" && missing(p)) {
        stop("Connection probability `p' is not given for Gnp graph")
    }
    if (type == "gnp" && !missing(m)) {
        warning("Number of edges `m' is ignored for Gnp graph")
    }
    if (type == "gnm" && missing(m)) {
        stop("Number of edges `m' is not given for Gnm graph")
    }
    if (type == "gnm" && !missing(p)) {
        warning("Connection probability `p' is ignored for Gnp graph")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (type == "gnp") {
        res <- .Call("R_igraph_bipartite_game_gnp", n1, n2, p, 
            directed, mode, PACKAGE = "igraph")
        res <- set_vertex_attr(res$graph, "type", value = res$types)
        res$name <- "Bipartite Gnp random graph"
        res$p <- p
    }
    else if (type == "gnm") {
        res <- .Call("R_igraph_bipartite_game_gnm", n1, n2, m, 
            directed, mode, PACKAGE = "igraph")
        res <- set_vertex_attr(res$graph, "type", value = res$types)
        res$name <- "Bipartite Gnm random graph"
        res$m <- m
    }
    res
}


graph_from_adjacency_matrix <- function (adjmatrix, mode = c("directed", "undirected", "max", 
    "min", "upper", "lower", "plus"), weighted = NULL, diag = TRUE, 
    add.colnames = NULL, add.rownames = NA) 
{
    if (inherits(adjmatrix, "Matrix")) {
        res <- graph.adjacency.sparse(adjmatrix, mode = mode, 
            weighted = weighted, diag = diag)
    }
    else {
        res <- graph.adjacency.dense(adjmatrix, mode = mode, 
            weighted = weighted, diag = diag)
    }
    if (is.null(add.colnames)) {
        if (!is.null(colnames(adjmatrix))) {
            add.colnames <- "name"
        }
        else {
            add.colnames <- NA
        }
    }
    else if (!is.na(add.colnames)) {
        if (is.null(colnames(adjmatrix))) {
            warning("No column names to add")
            add.colnames <- NA
        }
    }
    if (is.null(add.rownames)) {
        if (!is.null(rownames(adjmatrix))) {
            add.rownames <- "name"
        }
        else {
            add.colnames <- NA
        }
    }
    else if (!is.na(add.rownames)) {
        if (is.null(rownames(adjmatrix))) {
            warning("No row names to add")
            add.rownames <- NA
        }
    }
    if (!is.na(add.rownames) && !is.na(add.colnames) && add.rownames == 
        add.colnames) {
        warning("Same attribute for columns and rows, row names are ignored")
        add.rownames <- NA
    }
    if (!is.na(add.colnames)) {
        res <- set_vertex_attr(res, add.colnames, value = colnames(adjmatrix))
    }
    if (!is.na(add.rownames)) {
        res <- set_vertex_attr(res, add.rownames, value = rownames(adjmatrix))
    }
    res
}


full_citation_graph <- function (...) 
constructor_spec(make_full_citation_graph, ...)


alpha_centrality <- function (graph, nodes = V(graph), alpha = 1, loops = FALSE, 
    exo = 1, weights = NULL, tol = 1e-07, sparse = TRUE) 
{
    nodes <- as.igraph.vs(graph, nodes)
    if (sparse) {
        res <- alpha.centrality.sparse(graph, nodes, alpha, loops, 
            exo, weights, tol)
    }
    else {
        res <- alpha.centrality.dense(graph, nodes, alpha, loops, 
            exo, weights, tol)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", nodes)
    }
    res
}


power_centrality <- function (graph, nodes = V(graph), loops = FALSE, exponent = 1, 
    rescale = FALSE, tol = 1e-07, sparse = TRUE) 
{
    nodes <- as.igraph.vs(graph, nodes)
    if (sparse) {
        res <- bonpow.sparse(graph, nodes, loops, exponent, rescale, 
            tol)
    }
    else {
        res <- bonpow.dense(graph, nodes, loops, exponent, rescale, 
            tol)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", nodes)
    }
    res
}


is_min_separator <- function (graph, candidate) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    candidate <- as.igraph.vs(graph, candidate)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_minimal_separator", graph, candidate - 
        1, PACKAGE = "igraph")
    res
}


centr_clo <- function (graph, mode = c("out", "in", "all", "total"), normalized = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_closeness", graph, 
        mode, normalized, PACKAGE = "igraph")
    res
}


mst <- function (graph, weights = NULL, algorithm = NULL, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(algorithm)) {
        if (!is.null(weights) || "weight" %in% edge_attr_names(graph)) {
            algorithm <- "prim"
        }
        else {
            algorithm <- "unweighted"
        }
    }
    if (algorithm == "unweighted") {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_minimum_spanning_tree_unweighted", graph, 
            PACKAGE = "igraph")
    }
    else if (algorithm == "prim") {
        if (is.null(weights) && !"weight" %in% edge_attr_names(graph)) {
            stop("edges weights must be supplied for Prim's algorithm")
        }
        else if (is.null(weights)) {
            weights <- E(graph)$weight
        }
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_minimum_spanning_tree_prim", graph, as.numeric(weights), 
            PACKAGE = "igraph")
    }
    else {
        stop("Invalid algorithm")
    }
}


is_named <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    "name" %in% vertex_attr_names(graph)
}


subgraph <- function (graph, v) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_subgraph", graph, as.igraph.vs(graph, v) - 
        1, PACKAGE = "igraph")
}


lastcit.game <- function (n, edges = 1, agebins = n/7100, pref = (1:(agebins + 
    1))^-3, directed = TRUE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_lastcit_game", as.numeric(n), as.numeric(edges), 
        as.numeric(agebins), as.numeric(pref), as.logical(directed), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Random citation graph based on last citation"
        res$edges <- edges
        res$agebins <- agebins
    }
    res
}


modularity <- function (x, ...) 
UseMethod("modularity")


add.vertex.shape <- function (shape, clip = shape_noclip, plot = shape_noplot, parameters = list()) 
{
    assign(shape, value = list(clip = clip, plot = plot), envir = .igraph.shapes)
    do.call(igraph.options, parameters)
    invisible(TRUE)
}


tkplot.rotate <- function (tkp.id, degree = NULL, rad = NULL) 
{
    coords <- .tkplot.get(tkp.id, "coords")
    if (is.null(degree) && is.null(rad)) {
        rad <- pi/2
    }
    else if (is.null(rad) && !is.null(degree)) {
        rad <- degree/180 * pi
    }
    center <- c(mean(range(coords[, 1])), mean(range(coords[, 
        2])))
    phi <- atan2(coords[, 2] - center[2], coords[, 1] - center[1])
    r <- sqrt((coords[, 1] - center[1])^2 + (coords[, 2] - center[2])^2)
    phi <- phi + rad
    coords[, 1] <- r * cos(phi)
    coords[, 2] <- r * sin(phi)
    .tkplot.set(tkp.id, "coords", coords)
    tk_center(tkp.id)
    invisible(NULL)
}


bipartite_mapping <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_bipartite", graph, PACKAGE = "igraph")
    res
}


sample_fitness_pl <- function (no.of.nodes, no.of.edges, exponent.out, exponent.in = -1, 
    loops = FALSE, multiple = FALSE, finite.size.correction = TRUE) 
{
    no.of.nodes <- as.integer(no.of.nodes)
    no.of.edges <- as.integer(no.of.edges)
    exponent.out <- as.numeric(exponent.out)
    exponent.in <- as.numeric(exponent.in)
    loops <- as.logical(loops)
    multiple <- as.logical(multiple)
    finite.size.correction <- as.logical(finite.size.correction)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_static_power_law_game", no.of.nodes, 
        no.of.edges, exponent.out, exponent.in, loops, multiple, 
        finite.size.correction, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Static power law model")
    res <- set.graph.attribute(res, "exponent.out", exponent.out)
    res <- set.graph.attribute(res, "exponent.in", exponent.in)
    res <- set.graph.attribute(res, "loops", loops)
    res <- set.graph.attribute(res, "multiple", multiple)
    res <- set.graph.attribute(res, "finite.size.correction", 
        finite.size.correction)
    res
}


hrg.dendrogram <- function (hrg) 
{
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_dendrogram", hrg, PACKAGE = "igraph")
    res
}


is_separator <- function (graph, candidate) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    candidate <- as.igraph.vs(graph, candidate)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_separator", graph, candidate - 
        1, PACKAGE = "igraph")
    res
}


igraphdemo <- function (which) 
{
    if (missing(which)) {
        demodir <- system.file("demo", package = "igraph")
        if (demodir == "") {
            stop("Could not find igraph demos, broken igraph installation?")
        }
        return(sub("\\.R$", "", list.files(demodir)))
    }
    if (!grepl("\\.R$", which)) {
        which <- paste(which, sep = ".", "R")
    }
    if (!file.exists(which) && !grepl("^/", which)) {
        which <- system.file(paste("demo", sep = "/", which), 
            package = "igraph")
    }
    if (which == "" || !file.exists(which)) {
        stop("Could not find demo file")
    }
    .igraphdemo.next <- function(top, txt) {
        act <- as.character(tcltk::tktag.nextrange(txt, "active", 
            "0.0"))
        if (length(act) == 0) {
            return()
        }
        options(keep.source = TRUE)
        text <- tcltk::tclvalue(tcltk::tkget(txt, act[1], act[2]))
        cat("=======================================================\n")
        expr <- parse(text = text)
        for (i in seq_along(expr)) {
            co <- as.character(attributes(expr)$srcref[[i]])
            co[1] <- paste("> ", sep = "", co[1])
            if (length(co) > 1) {
                co[-1] <- paste(" +", sep = "", co[-1])
            }
            cat(co, sep = "\n")
            res <- withVisible(eval(expr[[i]], envir = .GlobalEnv))
            if (res$visible) {
                print(res$value)
            }
        }
        cat("> -------------------------------------------------------\n")
        cat(options()$prompt)
        tcltk::tktag.remove(txt, "activechunk", act[1], act[2])
        tcltk::tktag.remove(txt, "active", act[1], act[2])
        nex <- as.character(tcltk::tktag.nextrange(txt, "activechunk", 
            act[1]))
        if (length(nex) != 0) {
            tcltk::tktag.add(txt, "active", nex[1], nex[2])
            tcltk::tksee(txt, paste(sep = "", as.numeric(nex[2]), 
                ".0"))
            tcltk::tksee(txt, paste(sep = "", as.numeric(nex[1]), 
                ".0"))
        }
    }
    .igraphdemo.close <- function(top) {
        tcltk::tkdestroy(top)
    }
    .igraphdemo.reset <- function(top, txt, which) {
        demolines <- readLines(which)
        demolines <- demolines[!grepl("^pause\\(\\)$", demolines)]
        demolines <- paste(" ", sep = "", demolines)
        ch <- grep("^[ ]*###", demolines)
        ch <- c(ch, length(demolines) + 1)
        if (length(ch) == 1) {
            warning("Demo source file does not contain chunks")
        }
        else {
            demolines <- demolines[ch[1]:length(demolines)]
            ch <- grep("^[ ]*###", demolines)
            ch <- c(ch, length(demolines) + 1)
        }
        tcltk::tkconfigure(txt, state = "normal")
        tcltk::tkdelete(txt, "0.0", "end")
        tcltk::tkinsert(txt, "insert", paste(demolines, collapse = "\n"))
        tcltk::tkconfigure(txt, state = "disabled")
        for (i in seq_along(ch[-1])) {
            from <- paste(sep = "", ch[i], ".0")
            to <- paste(sep = "", ch[i + 1] - 1, ".0")
            tcltk::tktag.add(txt, "chunk", from, to)
            tcltk::tktag.add(txt, "activechunk", from, to)
        }
        tcltk::tktag.configure(txt, "chunk", "-borderwidth", 
            "1")
        tcltk::tktag.configure(txt, "chunk", "-relief", "sunken")
        if (length(ch) >= 2) {
            tcltk::tktag.add(txt, "active", paste(sep = "", ch[1], 
                ".0"), paste(sep = "", ch[2] - 1, ".0"))
            tcltk::tktag.configure(txt, "active", "-foreground", 
                "red")
            tcltk::tktag.configure(txt, "active", "-background", 
                "lightgrey")
        }
        comm <- grep("^#", demolines)
        for (i in comm) {
            tcltk::tktag.add(txt, "comment", paste(sep = "", 
                i, ".0"), paste(sep = "", i, ".end"))
        }
        tcltk::tktag.configure(txt, "comment", "-font", "bold")
        tcltk::tktag.configure(txt, "comment", "-foreground", 
            "darkolivegreen")
    }
    top <- tcltk::tktoplevel(background = "lightgrey")
    tcltk::tktitle(top) <- paste("igraph demo:", which)
    main.menu <- tcltk::tkmenu(top)
    tcltk::tkadd(main.menu, "command", label = "Close", command = function() .igraphdemo.close(top))
    tcltk::tkadd(main.menu, "command", label = "Reset", command = function() .igraphdemo.reset(top, 
        txt, which))
    tcltk::tkconfigure(top, "-menu", main.menu)
    scr <- tcltk::tkscrollbar(top, repeatinterval = 5, command = function(...) tcltk::tkyview(txt, 
        ...))
    txt <- tcltk::tktext(top, yscrollcommand = function(...) tcltk::tkset(scr, 
        ...), width = 80, height = 40)
    but <- tcltk::tkbutton(top, text = "Next", command = function() .igraphdemo.next(top, 
        txt))
    tcltk::tkpack(but, side = "bottom", fill = "x", expand = 0)
    tcltk::tkpack(scr, side = "right", fill = "y", expand = 0)
    tcltk::tkpack(txt, side = "left", fill = "both", expand = 1)
    .igraphdemo.reset(top, txt, which)
    invisible()
}


layout.lgl <- function (..., params = list()) 
{
    do_call(layout_with_lgl, .args = c(list(...), params))
}


print.igraph <- function (x, full = igraph_opt("print.full"), graph.attributes = igraph_opt("print.graph.attributes"), 
    vertex.attributes = igraph_opt("print.vertex.attributes"), 
    edge.attributes = igraph_opt("print.edge.attributes"), names = TRUE, 
    max.lines = igraph_opt("auto.print.lines"), ...) 
{
    if (!is_igraph(x)) {
        stop("Not a graph object")
    }
    head_lines <- .print.header(x)
    if (is.logical(full) && full) {
        if (graph.attributes) {
            head_lines <- head_lines + .print.graph.attributes(x, 
                full, max.lines)
        }
        if (vertex.attributes) {
            head_lines <- head_lines + .print.vertex.attributes(x, 
                full, max.lines)
        }
        if (ecount(x) == 0) {
        }
        else if (edge.attributes && length(edge_attr_names(x)) != 
            0) {
            .print.edges.edgelist(x, names = names)
        }
        else if (median(degree(x, mode = "out")) < 3) {
            .print.edges.compressed(x, names = names, max.lines = NULL)
        }
        else if (is_named(x)) {
            .print.edges.adjlist.named(x)
        }
        else {
            .print.edges.adjlist(x)
        }
    }
    else if (full == "auto") {
        .print.edges.compressed(x, names = names, max.lines = max.lines - 
            head_lines)
    }
    invisible(x)
}


eccentricity <- function (graph, vids = V(graph), mode = c("all", "out", "in", 
    "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_eccentricity", graph, vids - 1, mode, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


as_data_frame <- function (x, what = c("edges", "vertices", "both")) 
{
    if (!is_igraph(x)) {
        stop("Not a graph object")
    }
    what <- igraph.match.arg(what)
    if (what %in% c("vertices", "both")) {
        ver <- .Call("R_igraph_mybracket2", x, 9L, 3L, PACKAGE = "igraph")
        class(ver) <- "data.frame"
        rn <- if (is_named(x)) {
            V(x)$name
        }
        else {
            seq_len(vcount(x))
        }
        rownames(ver) <- rn
    }
    if (what %in% c("edges", "both")) {
        el <- as_edgelist(x)
        edg <- c(list(from = el[, 1]), list(to = el[, 2]), .Call("R_igraph_mybracket2", 
            x, 9L, 4L, PACKAGE = "igraph"))
        class(edg) <- "data.frame"
        rownames(edg) <- seq_len(ecount(x))
    }
    if (what == "both") {
        list(vertices = ver, edges = edg)
    }
    else if (what == "vertices") {
        ver
    }
    else {
        edg
    }
}


getAICc <- function (gfit) 
{
    Xorigin = gfit$Xorigin
    Xmean = gfit$W %*% gfit$H %*% diag(colSums(Xorigin))
    Xmean[Xmean < 1e-12] = 0
    Phat = gfit$W %*% gfit$H
    Phat[Phat < 1e-12] = 0
    if (ncol(gfit$W) > 1) {
        nparams = colSums((1 * (gfit$W > 0)))
        nparams = nparams %*% diag(1/colSums(Xorigin %*% t(gfit$H)))
        nparams = sum(nparams)
    }
    else {
        nparams = colSums((1 * (gfit$W > 0)))
        nparams = nparams/colSums(Xorigin %*% t(gfit$H))
        nparams = sum(nparams)
    }
    zeroprob = which(Phat < 1e-12)
    if (any(Xorigin[zeroprob] > 1e-12)) {
        retval = Inf
    }
    else {
        if (length(zeroprob) > 0) {
            Phat = Phat[-zeroprob]
        }
        retval = 2 * (nparams) - 2 * sum(Phat * log(Phat))
    }
    data.frame(nclust = ncol(gfit$W), negloglikpart = -2 * sum(Phat * 
        log(Phat)), parampart = 2 * nparams, AIC = retval)
}


layout_with_drl <- function (graph, use.seed = FALSE, seed = matrix(runif(vcount(graph) * 
    2), ncol = 2), options = drl_defaults$default, weights = E(graph)$weight, 
    fixed = NULL, dim = 2) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (dim != 2 && dim != 3) {
        stop("`dim' must be 2 or 3")
    }
    use.seed <- as.logical(use.seed)
    seed <- as.matrix(seed)
    options.tmp <- drl_defaults$default
    options.tmp[names(options)] <- options
    options <- options.tmp
    if (!is.null(weights)) {
        weights <- as.numeric(weights)
    }
    if (!is.null(fixed)) {
        fixed <- as.logical(fixed)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (dim == 2) {
        res <- .Call("R_igraph_layout_drl", graph, seed, use.seed, 
            options, weights, fixed, PACKAGE = "igraph")
    }
    else {
        res <- .Call("R_igraph_layout_drl_3d", graph, seed, use.seed, 
            options, weights, fixed, PACKAGE = "igraph")
    }
    res
}


with_sugiyama <- function (...) 
layout_spec(layout_with_sugiyama, ...)


cluster_fast_greedy <- function (graph, merges = TRUE, modularity = TRUE, membership = TRUE, 
    weights = E(graph)$weight) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(weights)) {
        weights <- as.numeric(weights)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_fastgreedy", graph, as.logical(merges), 
        as.logical(modularity), as.logical(membership), weights, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$algorithm <- "fast greedy"
    res$vcount <- vcount(graph)
    res$membership <- res$membership + 1
    res$merges <- res$merges + 1
    class(res) <- "communities"
    res
}


graph.bipartite <- function (types, edges, directed = FALSE) 
{
    types <- as.logical(types)
    edges <- as.numeric(edges) - 1
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_create_bipartite", types, edges, directed, 
        PACKAGE = "igraph")
    set_vertex_attr(res, "type", value = types)
}


remove.graph.attribute <- function (graph, name) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    name <- as.character(name)
    if (!name %in% graph_attr_names(graph)) {
        stop("No such graph attribute: ", name)
    }
    gattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 2L, 
        PACKAGE = "igraph")
    gattr[[name]] <- NULL
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 2L, gattr, 
        PACKAGE = "igraph")
}


sample_pa_age <- function (n, pa.exp, aging.exp, m = NULL, aging.bin = 300, out.dist = NULL, 
    out.seq = NULL, out.pref = FALSE, directed = TRUE, zero.deg.appeal = 1, 
    zero.age.appeal = 0, deg.coef = 1, age.coef = 1, time.window = NULL) 
{
    if (!is.null(out.seq) && (!is.null(m) || !is.null(out.dist))) {
        warning("if `out.seq' is given `m' and `out.dist' should be NULL")
        m <- out.dist <- NULL
    }
    if (is.null(out.seq) && !is.null(out.dist) && !is.null(m)) {
        warning("if `out.dist' is given `m' will be ignored")
        m <- NULL
    }
    if (!is.null(out.seq) && length(out.seq) != n) {
        stop("`out.seq' should be of length `n'")
    }
    if (!is.null(out.seq) && min(out.seq) < 0) {
        stop("negative elements in `out.seq'")
    }
    if (!is.null(m) && m < 0) {
        stop("`m' is negative")
    }
    if (!is.null(time.window) && time.window <= 0) {
        stop("time window size should be positive")
    }
    if (!is.null(m) && m == 0) {
        warning("`m' is zero, graph will be empty")
    }
    if (pa.exp < 0) {
        warning("preferential attachment is negative")
    }
    if (aging.exp > 0) {
        warning("aging exponent is positive")
    }
    if (zero.deg.appeal <= 0) {
        warning("initial attractiveness is not positive")
    }
    if (is.null(m) && is.null(out.dist) && is.null(out.seq)) {
        m <- 1
    }
    n <- as.numeric(n)
    if (!is.null(m)) {
        m <- as.numeric(m)
    }
    if (!is.null(out.dist)) {
        out.dist <- as.numeric(out.dist)
    }
    if (!is.null(out.seq)) {
        out.seq <- as.numeric(out.seq)
    }
    out.pref <- as.logical(out.pref)
    if (!is.null(out.dist)) {
        out.seq <- as.numeric(sample(0:(length(out.dist) - 1), 
            n, replace = TRUE, prob = out.dist))
    }
    if (is.null(out.seq)) {
        out.seq <- numeric()
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- if (is.null(time.window)) {
        .Call("R_igraph_barabasi_aging_game", as.numeric(n), 
            as.numeric(pa.exp), as.numeric(aging.exp), as.numeric(aging.bin), 
            m, out.seq, out.pref, as.numeric(zero.deg.appeal), 
            as.numeric(zero.age.appeal), as.numeric(deg.coef), 
            as.numeric(age.coef), directed, PACKAGE = "igraph")
    }
    else {
        .Call("R_igraph_recent_degree_aging_game", as.numeric(n), 
            as.numeric(pa.exp), as.numeric(aging.exp), as.numeric(aging.bin), 
            m, out.seq, out.pref, as.numeric(zero.deg.appeal), 
            directed, time.window, PACKAGE = "igraph")
    }
    if (igraph_opt("add.params")) {
        res$name <- "Aging Barabasi graph"
        res$pa.exp <- pa.exp
        res$aging.exp <- aging.exp
        res$m <- m
        res$aging.bin <- aging.bin
        res$out.pref <- out.pref
        res$zero.deg.appeal <- zero.deg.appeal
        res$zero.age.appeal <- zero.age.appeal
        res$deg.coef <- deg.coef
        res$age.coef <- age.coef
        res$time.window <- if (is.null(time.window)) 
            Inf
        else time.window
    }
    res
}


srand <- function (seed) 
{
    warning("This function does nothing, as calling srand from R packages\n", 
        "is now not allowed. If you want to reproduce your past\n", 
        "results, use an older version of igraph, e.g. 0.7.1")
}


distance_table <- function (graph, directed = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_path_length_hist", graph, directed, 
        PACKAGE = "igraph")
    res
}


decompose.graph <- function (graph, mode = c("weak", "strong"), max.comps = NA, 
    min.vertices = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, weak = 1, strong = 2)
    if (is.na(max.comps)) {
        max.comps = -1
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_decompose", graph, as.numeric(mode), as.numeric(max.comps), 
        as.numeric(min.vertices), PACKAGE = "igraph")
}


graph.difference <- function (...) 
UseMethod("difference")


graph.count.subisomorphisms.vf2 <- function (graph1, graph2, vertex.color1, vertex.color2, edge.color1, 
    edge.color2) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    if (missing(vertex.color1)) {
        if ("color" %in% vertex_attr_names(graph1)) {
            vertex.color1 <- V(graph1)$color
        }
        else {
            vertex.color1 <- NULL
        }
    }
    if (!is.null(vertex.color1)) {
        vertex.color1 <- as.integer(vertex.color1) - 1L
    }
    if (missing(vertex.color2)) {
        if ("color" %in% vertex_attr_names(graph2)) {
            vertex.color2 <- V(graph2)$color
        }
        else {
            vertex.color2 <- NULL
        }
    }
    if (!is.null(vertex.color2)) {
        vertex.color2 <- as.integer(vertex.color2) - 1L
    }
    if (missing(edge.color1)) {
        if ("color" %in% edge_attr_names(graph1)) {
            edge.color1 <- E(graph1)$color
        }
        else {
            edge.color1 <- NULL
        }
    }
    if (!is.null(edge.color1)) {
        edge.color1 <- as.integer(edge.color1) - 1L
    }
    if (missing(edge.color2)) {
        if ("color" %in% edge_attr_names(graph2)) {
            edge.color2 <- E(graph2)$color
        }
        else {
            edge.color2 <- NULL
        }
    }
    if (!is.null(edge.color2)) {
        edge.color2 <- as.integer(edge.color2) - 1L
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_count_subisomorphisms_vf2", graph1, 
        graph2, vertex.color1, vertex.color2, edge.color1, edge.color2, 
        PACKAGE = "igraph")
    res
}


list.vertex.attributes <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    res <- base::.Call("R_igraph_mybracket2_names", graph, 9L, 
        3L, PACKAGE = "igraph")
    if (is.null(res)) {
        res <- character()
    }
    res
}


igraph.eigen.default <- structure(list(pos = "LM", howmany = 1L, il = -1L, iu = -1L, 
    vl = -Inf, vu = Inf, vestimate = 0L, balance = "none"), .Names = c("pos", 
"howmany", "il", "iu", "vl", "vu", "vestimate", "balance"))


growing.random.game <- function (n, m = 1, directed = TRUE, citation = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_growing_random_game", as.numeric(n), 
        as.numeric(m), as.logical(directed), as.logical(citation), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Growing random graph"
        res$m <- m
        res$citation <- citation
    }
    res
}


is_degseq <- function (out.deg, in.deg = NULL) 
{
    out.deg <- as.numeric(out.deg)
    if (!is.null(in.deg)) 
        in.deg <- as.numeric(in.deg)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_degree_sequence", out.deg, in.deg, 
        PACKAGE = "igraph")
    res
}


graph.data.frame <- function (d, directed = TRUE, vertices = NULL) 
{
    d <- as.data.frame(d)
    if (!is.null(vertices)) {
        vertices <- as.data.frame(vertices)
    }
    if (ncol(d) < 2) {
        stop("the data frame should contain at least two columns")
    }
    if (any(is.na(d[, 1:2]))) {
        warning("In `d' `NA' elements were replaced with string \"NA\"")
        d[, 1:2][is.na(d[, 1:2])] <- "NA"
    }
    if (!is.null(vertices) && any(is.na(vertices[, 1]))) {
        warning("In `vertices[,1]' `NA' elements were replaced with string \"NA\"")
        vertices[, 1][is.na(vertices[, 1])] <- "NA"
    }
    names <- unique(c(as.character(d[, 1]), as.character(d[, 
        2])))
    if (!is.null(vertices)) {
        names2 <- names
        vertices <- as.data.frame(vertices)
        if (ncol(vertices) < 1) {
            stop("Vertex data frame contains no rows")
        }
        names <- as.character(vertices[, 1])
        if (any(duplicated(names))) {
            stop("Duplicate vertex names")
        }
        if (any(!names2 %in% names)) {
            stop("Some vertex names in edge list are not listed in vertex data frame")
        }
    }
    g <- make_empty_graph(n = 0, directed = directed)
    attrs <- list(name = names)
    if (!is.null(vertices)) {
        if (ncol(vertices) > 1) {
            for (i in 2:ncol(vertices)) {
                newval <- vertices[, i]
                if (class(newval) == "factor") {
                  newval <- as.character(newval)
                }
                attrs[[names(vertices)[i]]] <- newval
            }
        }
    }
    g <- add_vertices(g, length(names), attr = attrs)
    from <- as.character(d[, 1])
    to <- as.character(d[, 2])
    edges <- rbind(match(from, names), match(to, names))
    attrs <- list()
    if (ncol(d) > 2) {
        for (i in 3:ncol(d)) {
            newval <- d[, i]
            if (class(newval) == "factor") {
                newval <- as.character(newval)
            }
            attrs[[names(d)[i]]] <- newval
        }
    }
    g <- add_edges(g, edges, attr = attrs)
    g
}


igraph.version <- function () 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_version", PACKAGE = "igraph")
}


sample_correlated_gnp_pair <- function (n, corr, p, directed = FALSE, permutation = NULL) 
{
    n <- as.integer(n)
    corr <- as.numeric(corr)
    p <- as.numeric(p)
    directed <- as.logical(directed)
    if (!is.null(permutation)) 
        permutation <- as.numeric(permutation) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_correlated_pair_game", n, corr, p, 
        directed, permutation, PACKAGE = "igraph")
    res
}


randomly <- function (...) 
layout_spec(layout_randomly, ...)


make_ring <- function (n, directed = FALSE, mutual = FALSE, circular = TRUE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_ring", as.numeric(n), as.logical(directed), 
        as.logical(mutual), as.logical(circular), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Ring graph"
        res$mutual <- mutual
        res$circular <- circular
    }
    res
}


biconnected_components <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_biconnected_components", graph, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$tree_edges)) {
            res$tree_edges[[i_]] <- create_es(graph, res$tree_edges[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$component_edges)) {
            res$component_edges[[i_]] <- create_es(graph, res$component_edges[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$components)) {
            res$components[[i_]] <- create_vs(graph, res$components[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        res$articulation_points <- create_vs(graph, res$articulation_points)
    }
    res
}


stMincuts <- function (graph, source, target, capacity = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    source <- as.igraph.vs(graph, source)
    target <- as.igraph.vs(graph, target)
    if (is.null(capacity) && "weight" %in% edge_attr_names(graph)) {
        capacity <- E(graph)$weight
    }
    if (!is.null(capacity) && any(!is.na(capacity))) {
        capacity <- as.numeric(capacity)
    }
    else {
        capacity <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_all_st_mincuts", graph, source - 1, 
        target - 1, capacity, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$cuts)) {
            res$cuts[[i_]] <- create_es(graph, res$cuts[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$partition1s)) {
            res$partition1s[[i_]] <- create_vs(graph, res$partition1s[[i_]])
        }
    }
    res
}


tkplot.setcoords <- function (tkp.id, coords) 
{
    stopifnot(is.matrix(coords), ncol(coords) == 2)
    .tkplot.set(tkp.id, "coords", coords)
    .tkplot.update.vertices(tkp.id)
    invisible(NULL)
}


is_dag <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_dag", graph, PACKAGE = "igraph")
    res
}


smallworld <- function (...) 
constructor_spec(sample_smallworld, ...)


tk_off <- function () 
{
    eapply(.tkplot.env, function(tkp) {
        tcltk::tkdestroy(tkp$top)
    })
    rm(list = ls(.tkplot.env), envir = .tkplot.env)
    invisible(NULL)
}


get.edges <- function (graph, es) 
{
    ends(graph, es, names = FALSE)
}


make_full_citation_graph <- function (n, directed = TRUE) 
{
    n <- as.integer(n)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_full_citation", n, directed, PACKAGE = "igraph")
    res <- set_graph_attr(res, "name", "Full citation graph")
    res
}


upgrade_graph <- function (graph) 
{
    stopifnot(is_igraph(graph))
    g_ver <- graph_version(graph)
    p_ver <- graph_version()
    if (g_ver < p_ver) {
        if ((g_ver == "0.4.0" && p_ver == "0.8.0")) {
            .Call("R_igraph_add_env", graph, PACKAGE = "igraph")
        }
        else if (g_ver == "0.7.999" && p_ver == "0.8.0") {
            .Call("R_igraph_add_version_to_env", graph, PACKAGE = "igraph")
        }
        else {
            stop("Don't know how to upgrade graph from ", g_ver, 
                " to ", p_ver)
        }
    }
    else if (g_ver > p_ver) {
        stop("Don't know how to downgrade graph from ", g_ver, 
            " to ", p_ver)
    }
    else {
        graph
    }
}


callaway.traits.game <- function (nodes, types, edge.per.step = 1, type.dist = rep(1, 
    types), pref.matrix = matrix(1, types, types), directed = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_callaway_traits_game", as.double(nodes), 
        as.double(types), as.double(edge.per.step), as.double(type.dist), 
        matrix(as.double(pref.matrix), types, types), as.logical(directed), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Trait-based Callaway graph"
        res$types <- types
        res$edge.per.step <- edge.per.step
        res$type.dist <- type.dist
        res$pref.matrix <- pref.matrix
    }
    res
}


local_scan <- function (graph.us, graph.them = NULL, k = 1, FUN = NULL, weighted = FALSE, 
    mode = c("out", "in", "all"), neighborhoods = NULL, ...) 
{
    stopifnot(is.igraph(graph.us))
    stopifnot(is.null(graph.them) || is.igraph(graph.them))
    stopifnot(is.null(graph.them) || vcount(graph.them) == vcount(graph.us))
    stopifnot(length(k) == 1, k >= 0, as.integer(k) == k)
    stopifnot(is.null(FUN) || is.function(FUN) || (is.character(FUN) && 
        length(FUN) == 1))
    stopifnot(is.logical(weighted), length(weighted) == 1)
    stopifnot(!weighted || (is.weighted(graph.us) && (is.null(graph.them) || 
        is.weighted(graph.them))))
    if (!is.null(neighborhoods)) {
        stopifnot(is.list(neighborhoods))
        stopifnot(length(neighborhoods) == vcount(graph.us))
    }
    if (!is.null(neighborhoods) && k == 0) {
        warning("`neighborhoods' ignored for k=0")
        neighborhoods <- NULL
    }
    mode <- igraph.match.arg(mode)
    cmode <- switch(mode, out = 1, `in` = 2, all = 3, total = 3)
    sumweights <- function(g) sum(E(g)$weight)
    if (is.null(FUN)) {
        FUN <- if (weighted) 
            "sumweights"
        else "ecount"
    }
    res <- if (is.null(graph.them)) {
        if (!is.null(neighborhoods)) {
            if (is.character(FUN) && FUN %in% c("ecount", "sumweights")) {
                neighborhoods <- lapply(neighborhoods, function(x) {
                  as.integer(x) - 1L
                })
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_local_scan_neighborhood_ecount", 
                  graph.us, if (weighted) as.numeric(E(graph.us)$weight) else NULL, 
                  neighborhoods, PACKAGE = "igraph")
            }
            else {
                sapply(lapply(neighborhoods, induced.subgraph, 
                  graph = graph.us), FUN, ...)
            }
        }
        else {
            if (k == 0) {
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_local_scan_0", graph.us, if (weighted) as.numeric(E(graph.us)$weight) else NULL, 
                  cmode, PACKAGE = "igraph")
            }
            else if (k == 1 && is.character(FUN) && FUN %in% 
                c("ecount", "sumweights")) {
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_local_scan_1_ecount", graph.us, 
                  if (weighted) as.numeric(E(graph.us)$weight) else NULL, 
                  cmode, PACKAGE = "igraph")
            }
            else if (is.character(FUN) && FUN %in% c("ecount", 
                "sumweights")) {
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_local_scan_k_ecount", graph.us, 
                  as.integer(k), if (weighted) as.numeric(E(graph.us)$weight) else NULL, 
                  cmode, PACKAGE = "igraph")
            }
            else {
                sapply(graph.neighborhood(graph.us, order = k, 
                  V(graph.us), mode = mode), FUN, ...)
            }
        }
    }
    else {
        if (!is.null(neighborhoods)) {
            neighborhoods <- lapply(neighborhoods, as.vector)
            if (is.character(FUN) && FUN %in% c("ecount", "wumweights")) {
                neighborhoods <- lapply(neighborhoods, function(x) {
                  as.integer(x) - 1L
                })
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_local_scan_neighborhood_ecount", 
                  graph.them, if (weighted) as.numeric(E(graph.them)$weight) else NULL, 
                  neighborhoods, PACKAGE = "igraph")
            }
            else {
                sapply(lapply(neighborhoods, induced.subgraph, 
                  graph = graph.them), FUN, ...)
            }
        }
        else {
            if (k == 0) {
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_local_scan_0_them", graph.us, 
                  graph.them, if (weighted) as.numeric(E(graph.them)$weight) else NULL, 
                  cmode, PACKAGE = "igraph")
            }
            else if (k == 1 && is.character(FUN) && FUN %in% 
                c("ecount", "sumweights")) {
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_local_scan_1_ecount_them", graph.us, 
                  graph.them, if (weighted) as.numeric(E(graph.them)$weight) else NULL, 
                  cmode, PACKAGE = "igraph")
            }
            else if (is.character(FUN) && FUN %in% c("ecount", 
                "sumweights")) {
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_local_scan_k_ecount_them", graph.us, 
                  graph.them, as.integer(k), if (weighted) as.numeric(E(graph.them)$weight) else NULL, 
                  cmode, PACKAGE = "igraph")
            }
            else {
                sapply(V(graph.us), function(x) {
                  vei <- neighborhood(graph.us, order = k, nodes = x, 
                    mode = mode)[[1]]
                  if (!is.function(FUN)) {
                    FUN <- getFunction(FUN, where = environment())
                  }
                  FUN(induced.subgraph(graph.them, vei), ...)
                })
            }
        }
    }
    res <- as.numeric(res)
    if (igraph_opt("add.vertex.names") && is_named(graph.us)) {
        names(res) <- V(graph.us)$name
    }
    res
}


blockGraphs <- function (blocks, graph) 
{
    lapply(blocks(blocks), induced_subgraph, graph = graph)
}


add_vertices <- function (graph, nv, ..., attr = list()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    attrs <- list(...)
    attrs <- append(attrs, attr)
    nam <- names(attrs)
    if (length(attrs) != 0 && (is.null(nam) || any(nam == ""))) {
        stop("please supply names for attributes")
    }
    vertices.orig <- vcount(graph)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    graph <- .Call("R_igraph_add_vertices", graph, as.numeric(nv), 
        PACKAGE = "igraph")
    vertices.new <- vcount(graph)
    if (vertices.new - vertices.orig != 0) {
        idx <- seq(vertices.orig + 1, vertices.new)
    }
    else {
        idx <- numeric()
    }
    vattrs <- .Call("R_igraph_mybracket2", graph, 9L, 3L, PACKAGE = "igraph")
    for (i in seq(attrs)) {
        vattrs[[nam[i]]][idx] <- attrs[[nam[i]]]
    }
    .Call("R_igraph_mybracket2_set", graph, 9L, 3L, vattrs, PACKAGE = "igraph")
}


is.named <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    "name" %in% vertex_attr_names(graph)
}


similarity.jaccard <- function (graph, vids = V(graph), mode = c("all", "out", "in", 
    "total"), loops = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    loops <- as.logical(loops)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_similarity_jaccard", graph, vids - 
        1, mode, loops, PACKAGE = "igraph")
    res
}


kautz_graph <- function (...) 
constructor_spec(make_kautz_graph, ...)


infomap.community <- function (graph, e.weights = NULL, v.weights = NULL, nb.trials = 10, 
    modularity = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(e.weights) && "weight" %in% edge_attr_names(graph)) {
        e.weights <- E(graph)$weight
    }
    if (!is.null(e.weights) && any(!is.na(e.weights))) {
        e.weights <- as.numeric(e.weights)
    }
    else {
        e.weights <- NULL
    }
    if (is.null(v.weights) && "weight" %in% vertex_attr_names(graph)) {
        v.weights <- V(graph)$weight
    }
    if (!is.null(v.weights) && any(!is.na(v.weights))) {
        v.weights <- as.numeric(v.weights)
    }
    else {
        v.weights <- NULL
    }
    nb.trials <- as.integer(nb.trials)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_infomap", graph, e.weights, 
        v.weights, nb.trials, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "infomap"
    res$membership <- res$membership + 1
    if (modularity) {
        res$modularity <- modularity(graph, res$membership, weights = e.weights)
    }
    class(res) <- "communities"
    res
}


sample_degseq <- function (out.deg, in.deg = NULL, method = c("simple", "vl", 
    "simple.no.multiple")) 
{
    method <- igraph.match.arg(method)
    method1 <- switch(method, simple = 0, vl = 1, simple.no.multiple = 2)
    if (!is.null(in.deg)) {
        in.deg <- as.numeric(in.deg)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_degree_sequence_game", as.numeric(out.deg), 
        in.deg, as.numeric(method1), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Degree sequence random graph"
        res$method <- method
    }
    res
}


disjoint_union <- function (...) 
{
    graphs <- unlist(recursive = FALSE, lapply(list(...), function(l) {
        if (is_igraph(l)) 
            list(l)
        else l
    }))
    if (!all(sapply(graphs, is_igraph))) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_disjoint_union", graphs, PACKAGE = "igraph")
    graph.attributes(res) <- rename.attr.if.needed("g", graphs)
    attr <- list()
    vc <- sapply(graphs, vcount)
    cumvc <- c(0, cumsum(vc))
    for (i in seq_along(graphs)) {
        va <- vertex.attributes(graphs[[i]])
        exattr <- intersect(names(va), names(attr))
        noattr <- setdiff(names(attr), names(va))
        newattr <- setdiff(names(va), names(attr))
        for (a in seq_along(exattr)) {
            attr[[exattr[a]]] <- c(attr[[exattr[a]]], va[[exattr[a]]])
        }
        for (a in seq_along(noattr)) {
            attr[[noattr[a]]] <- c(attr[[noattr[a]]], rep(NA, 
                vc[i]))
        }
        for (a in seq_along(newattr)) {
            attr[[newattr[a]]] <- c(rep(NA, cumvc[i]), va[[newattr[a]]])
        }
    }
    vertex.attributes(res) <- attr
    if ("name" %in% names(attr) && any(duplicated(attr$name))) {
        warning("Duplicate vertex names in disjoint union")
    }
    attr <- list()
    ec <- sapply(graphs, ecount)
    cumec <- c(0, cumsum(ec))
    for (i in seq_along(graphs)) {
        ea <- edge.attributes(graphs[[i]])
        exattr <- intersect(names(ea), names(attr))
        noattr <- setdiff(names(attr), names(ea))
        newattr <- setdiff(names(ea), names(attr))
        for (a in seq_along(exattr)) {
            attr[[exattr[a]]] <- c(attr[[exattr[a]]], ea[[exattr[a]]])
        }
        for (a in seq_along(noattr)) {
            attr[[noattr[a]]] <- c(attr[[noattr[a]]], rep(NA, 
                ec[i]))
        }
        for (a in seq_along(newattr)) {
            attr[[newattr[a]]] <- c(rep(NA, cumec[i]), ea[[newattr[a]]])
        }
    }
    edge.attributes(res) <- attr
    res
}


graph.lcf <- function (n, shifts, repeats = 1) 
{
    n <- as.integer(n)
    shifts <- as.numeric(shifts)
    repeats <- as.integer(repeats)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_lcf_vector", n, shifts, repeats, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "LCF graph")
    res
}


from_literal <- function (...) 
constructor_spec(graph_from_literal, ..., .lazy = TRUE)


interconnected.islands.game <- function (islands.n, islands.size, islands.pin, n.inter) 
{
    islands.n <- as.integer(islands.n)
    islands.size <- as.integer(islands.size)
    islands.pin <- as.numeric(islands.pin)
    n.inter <- as.integer(n.inter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_simple_interconnected_islands_game", 
        islands.n, islands.size, islands.pin, n.inter, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Interconnected islands model")
    res <- set.graph.attribute(res, "islands.n", islands.n)
    res <- set.graph.attribute(res, "islands.size", islands.size)
    res <- set.graph.attribute(res, "islands.pin", islands.pin)
    res <- set.graph.attribute(res, "n.inter", n.inter)
    res
}


graph.full.bipartite <- function (n1, n2, directed = FALSE, mode = c("all", "out", "in")) 
{
    n1 <- as.integer(n1)
    n2 <- as.integer(n2)
    directed <- as.logical(directed)
    mode1 <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_full_bipartite", n1, n2, as.logical(directed), 
        mode1, PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$graph$name <- "Full bipartite graph"
        res$n1 <- n1
        res$n2 <- n2
        res$mode <- mode
    }
    set_vertex_attr(res$graph, "type", value = res$types)
}


arpack_defaults <- structure(list(bmat = "I", n = 0, which = "XX", nev = 1, tol = 0, 
    ncv = 3, ldv = 0, ishift = 1, maxiter = 1000, nb = 1, mode = 1, 
    start = 0, sigma = 0, sigmai = 0), .Names = c("bmat", "n", 
"which", "nev", "tol", "ncv", "ldv", "ishift", "maxiter", "nb", 
"mode", "start", "sigma", "sigmai"))


subgraph_isomorphisms <- function (pattern, target, method = c("lad", "vf2"), ...) 
{
    method <- igraph.match.arg(method)
    if (method == "lad") {
        graph.subisomorphic.lad(pattern, target, all.maps = TRUE, 
            ...)$maps
    }
    else if (method == "vf2") {
        graph.get.subisomorphisms.vf2(target, pattern, ...)
    }
}


hierarchical_sbm <- function (...) 
constructor_spec(sample_hierarchical_sbm, ...)


tk_close <- function (tkp.id, window.close = TRUE) 
{
    if (window.close) {
        cmd <- paste(sep = "", "tkp.", tkp.id, "$top")
        top <- eval(parse(text = cmd), .tkplot.env)
        tcltk::tkbind(top, "<Destroy>", "")
        tcltk::tkdestroy(top)
    }
    cmd <- paste(sep = "", "tkp.", tkp.id)
    rm(list = cmd, envir = .tkplot.env)
    invisible(NULL)
}


remove.vertex.attribute <- function (graph, name) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    name <- as.character(name)
    if (!name %in% vertex_attr_names(graph)) {
        stop("No such vertex attribute: ", name)
    }
    vattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 3L, 
        PACKAGE = "igraph")
    vattr[[name]] <- NULL
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 3L, vattr, 
        PACKAGE = "igraph")
}


assortativity.degree <- function (graph, directed = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_assortativity_degree", graph, directed, 
        PACKAGE = "igraph")
    res
}


neighbors <- function (graph, v, mode = c("out", "in", "all", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.character(mode)) {
        mode <- igraph.match.arg(mode)
        mode <- switch(mode, out = 1, `in` = 2, all = 3, total = 3)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_neighbors", graph, as.igraph.vs(graph, 
        v) - 1, as.numeric(mode), PACKAGE = "igraph")
    V(graph)[res + 1]
}


is.weighted <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    "weight" %in% edge_attr_names(graph)
}


is.maximal.matching <- function (graph, matching, types = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        types <- as.logical(types)
    }
    matching <- as.igraph.vs(graph, matching, na.ok = TRUE) - 
        1
    matching[is.na(matching)] <- -1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_maximal_matching", graph, types, 
        matching, PACKAGE = "igraph")
    res
}


average.path.length <- function (graph, directed = TRUE, unconnected = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_average_path_length", graph, as.logical(directed), 
        as.logical(unconnected), PACKAGE = "igraph")
}


layout.svd <- function (graph, ...) 
{
    warning("SVD layout was removed, we use Fruchterman-Reingold instead.")
    layout_with_fr(graph)
}


similarity <- function (graph, vids = V(graph), mode = c("all", "out", "in", 
    "total"), loops = FALSE, method = c("jaccard", "dice", "invlogweighted")) 
{
    method <- igraph.match.arg(method)
    if (method == "jaccard") {
        similarity.jaccard(graph, vids, mode, loops)
    }
    else if (method == "dice") {
        similarity.dice(graph, vids, mode, loops)
    }
    else if (method == "invlogweighted") {
        similarity.invlogweighted(graph, vids, mode)
    }
}


bipartite_graph <- function (...) 
constructor_spec(make_bipartite_graph, ...)


centr_degree_tmax <- function (graph = NULL, nodes = 0, mode = c("all", "out", "in", 
    "total"), loops = FALSE) 
{
    if (!is.null(graph) && !is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.integer(nodes)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    loops <- as.logical(loops)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_degree_tmax", graph, 
        nodes, mode, loops, PACKAGE = "igraph")
    res
}


tkplot.export.postscript <- function (tkp.id) 
{
    tkp <- .tkplot.get(tkp.id)
    filename <- tcltk::tkgetSaveFile(initialfile = "Rplots.eps", 
        defaultextension = "eps", title = "Export graph to PostScript file")
    tcltk::tkpostscript(tkp$canvas, file = filename)
    invisible(NULL)
}


is.multiple <- function (graph, eids = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_is_multiple", graph, as.igraph.es(graph, 
        eids) - 1, PACKAGE = "igraph")
}


layout.random <- function (..., params = list()) 
{
    do_call(layout_randomly, .args = c(list(...), params))
}


show_trace <- function (communities) 
{
    if (!inherits(communities, "communities")) {
        stop("Not a community structure")
    }
    if (is.null(communities$history)) {
        stop("History was not recorded")
    }
    res <- character()
    i <- 1
    while (i <= length(communities$history)) {
        if (communities$history[i] == 2) {
            resnew <- paste("Splitting community", communities$history[i + 
                1], "into two.")
            i <- i + 2
        }
        else if (communities$history[i] == 3) {
            resnew <- paste("Failed splitting community", communities$history[i + 
                1], "into two.")
            i <- i + 2
        }
        else if (communities$history[i] == 4) {
            resnew <- "Starting with the whole graph as a community."
            i <- i + 1
        }
        else if (communities$history[i] == 5) {
            resnew <- paste("Starting from the", communities$history[i + 
                1], "given communities.")
            i <- i + 2
        }
        res <- c(res, resnew)
    }
    res
}


betweenness.estimate <- function (graph, vids = V(graph), directed = TRUE, cutoff, weights = NULL, 
    nobigint = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    directed <- as.logical(directed)
    cutoff <- as.numeric(cutoff)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    nobigint <- as.logical(nobigint)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_betweenness_estimate", graph, vids - 
        1, directed, cutoff, weights, nobigint, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


is_subgraph_isomorphic_to <- function (pattern, target, method = c("auto", "lad", "vf2"), 
    ...) 
{
    method <- igraph.match.arg(method)
    if (method == "auto") 
        method <- "lad"
    if (method == "lad") {
        graph.subisomorphic.lad(pattern, target, map = FALSE, 
            all.maps = FALSE, ...)$iso
    }
    else if (method == "vf2") {
        graph.subisomorphic.vf2(target, pattern, ...)$iso
    }
}


edge.attributes <- function (graph, index = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!missing(index)) {
        index <- as.igraph.es(graph, index)
    }
    res <- base::.Call("R_igraph_mybracket2_copy", graph, 9L, 
        4L, PACKAGE = "igraph")
    if (!missing(index) && (length(index) != ecount(graph) || 
        any(index != E(graph)))) {
        for (i in seq_along(value)) {
            value[[i]] <- value[[i]][index]
        }
    }
    res
}


cluster_optimal <- function (graph, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_optimal_modularity", graph, 
        weights, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "optimal"
    res$membership <- res$membership + 1
    class(res) <- "communities"
    res
}


max_cliques <- function (graph, min = NULL, max = NULL, subset = NULL, file = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(min)) {
        min <- 0
    }
    if (is.null(max)) {
        max <- 0
    }
    if (!is.null(subset)) {
        subset <- as.integer(as.igraph.vs(graph, subset) - 1)
    }
    if (!is.null(file)) {
        if (!is.character(file) || length(grep("://", file, fixed = TRUE)) > 
            0 || length(grep("~", file, fixed = TRUE)) > 0) {
            tmpfile <- TRUE
            origfile <- file
            file <- tempfile()
        }
        else {
            tmpfile <- FALSE
        }
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        res <- .Call("R_igraph_maximal_cliques_file", graph, 
            subset, file, as.numeric(min), as.numeric(max), PACKAGE = "igraph")
        if (tmpfile) {
            buffer <- read.graph.toraw(file)
            write.graph.fromraw(buffer, origfile)
        }
        invisible(NULL)
    }
    else {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        res <- .Call("R_igraph_maximal_cliques", graph, subset, 
            as.numeric(min), as.numeric(max), PACKAGE = "igraph")
        res <- lapply(res, function(x) x + 1)
        if (igraph_opt("return.vs.es")) {
            res <- lapply(res, create_vs, graph = graph)
        }
        res
    }
}


largest.independent.vertex.sets <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_largest_independent_vertex_sets", 
        graph, PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


centralization.betweenness.tmax <- function (graph = NULL, nodes = 0, directed = TRUE) 
{
    if (!is.null(graph) && !is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.integer(nodes)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_betweenness_tmax", 
        graph, nodes, directed, PACKAGE = "igraph")
    res
}


path.length.hist <- function (graph, directed = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_path_length_hist", graph, directed, 
        PACKAGE = "igraph")
    res
}


hub_score <- function (graph, scale = TRUE, weights = NULL, options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    scale <- as.logical(scale)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hub_score", graph, scale, weights, 
        options, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$vector) <- vertex_attr(graph, "name", )
    }
    res
}


centralization.closeness <- function (graph, mode = c("out", "in", "all", "total"), normalized = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_closeness", graph, 
        mode, normalized, PACKAGE = "igraph")
    res
}


edges <- function (...) 
{
    structure(list(...), class = "igraph.edge")
}


undirected_graph <- function (...) 
constructor_spec(make_undirected_graph, ...)


is_chordal <- function (graph, alpha = NULL, alpham1 = NULL, fillin = FALSE, 
    newgraph = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(alpha)) 
        alpha <- as.numeric(alpha) - 1
    if (!is.null(alpham1)) 
        alpham1 <- as.numeric(alpham1) - 1
    fillin <- as.logical(fillin)
    newgraph <- as.logical(newgraph)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_chordal", graph, alpha, alpham1, 
        fillin, newgraph, PACKAGE = "igraph")
    if (fillin) {
        res$fillin <- res$fillin + 1
    }
    res
}


as_edgelist <- function (graph, names = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- matrix(.Call("R_igraph_get_edgelist", graph, TRUE, 
        PACKAGE = "igraph"), ncol = 2)
    res <- res + 1
    if (names && "name" %in% vertex_attr_names(graph)) {
        res <- matrix(V(graph)$name[res], ncol = 2)
    }
    res
}


automorphisms <- function (graph, sh = "fm") 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    sh <- switch(igraph.match.arg(sh), f = 0, fl = 1, fs = 2, 
        fm = 3, flm = 4, fsm = 5)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_automorphisms", graph, sh, PACKAGE = "igraph")
    res
}


`graph_attr<-` <- function (graph, name, value) 
{
    if (missing(name)) {
        `graph.attributes<-`(graph, value)
    }
    else {
        set_graph_attr(graph, name, value)
    }
}


isomorphism_class <- function (graph, v) 
{
    if (missing(v)) {
        graph.isoclass(graph)
    }
    else {
        graph.isoclass.subgraph(graph, v)
    }
}


sample_grg <- function (nodes, radius, torus = FALSE, coords = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_grg_game", as.double(nodes), as.double(radius), 
        as.logical(torus), as.logical(coords), PACKAGE = "igraph")
    if (coords) {
        V(res[[1]])$x <- res[[2]]
        V(res[[1]])$y <- res[[3]]
    }
    if (igraph_opt("add.params")) {
        res[[1]]$name <- "Geometric random graph"
        res[[1]]$radius <- radius
        res[[1]]$torus <- torus
    }
    res[[1]]
}


graph.edgelist <- function (el, directed = TRUE) 
{
    if (!is.matrix(el) || ncol(el) != 2) {
        stop("graph_from_edgelist expects a matrix with two columns")
    }
    if (nrow(el) == 0) {
        res <- make_empty_graph(directed = directed)
    }
    else {
        if (is.character(el)) {
            names <- unique(as.character(t(el)))
            ids <- seq(names)
            names(ids) <- names
            res <- graph(unname(ids[t(el)]), directed = directed)
            rm(ids)
            V(res)$name <- names
        }
        else {
            res <- graph(t(el), directed = directed)
        }
    }
    res
}


topological.sort <- function (graph, mode = c("out", "all", "in")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_topological_sorting", graph, as.numeric(mode), 
        PACKAGE = "igraph") + 1L
    if (igraph_opt("return.vs.es")) 
        res <- create_vs(graph, res)
    res
}


shape_noclip <- function (coords, el, params, end = c("both", "from", "to")) 
{
    end <- igraph.match.arg(end)
    if (end == "both") {
        coords
    }
    else if (end == "from") {
        coords[, 1:2, drop = FALSE]
    }
    else {
        coords[, 3:4, drop = FALSE]
    }
}


igraph.from.graphNEL <- function (graphNEL, name = TRUE, weight = TRUE, unlist.attrs = TRUE) 
{
    if (!inherits(graphNEL, "graphNEL")) {
        stop("Not a graphNEL graph")
    }
    al <- lapply(graph::edgeL(graphNEL), "[[", "edges")
    if (graph::edgemode(graphNEL) == "undirected") {
        al <- mapply(SIMPLIFY = FALSE, seq_along(al), al, FUN = function(n, 
            l) {
            c(l, rep(n, sum(l == n)))
        })
    }
    mode <- if (graph::edgemode(graphNEL) == "directed") 
        "out"
    else "all"
    g <- graph_from_adj_list(al, mode = mode, duplicate = TRUE)
    if (name) {
        V(g)$name <- graph::nodes(graphNEL)
    }
    g.n <- names(graphNEL@graphData)
    g.n <- g.n[g.n != "edgemode"]
    for (n in g.n) {
        g <- set_graph_attr(g, n, graphNEL@graphData[[n]])
    }
    v.n <- names(graph::nodeDataDefaults(graphNEL))
    for (n in v.n) {
        val <- unname(graph::nodeData(graphNEL, attr = n))
        if (unlist.attrs && all(sapply(val, length) == 1)) {
            val <- unlist(val)
        }
        g <- set_vertex_attr(g, n, value = val)
    }
    e.n <- names(graph::edgeDataDefaults(graphNEL))
    if (!weight) {
        e.n <- e.n[e.n != "weight"]
    }
    if (length(e.n) > 0) {
        el <- as_edgelist(g)
        el <- paste(sep = "|", el[, 1], el[, 2])
        for (n in e.n) {
            val <- unname(graph::edgeData(graphNEL, attr = n)[el])
            if (unlist.attrs && all(sapply(val, length) == 1)) {
                val <- unlist(val)
            }
            g <- set_edge_attr(g, n, value = val)
        }
    }
    g
}


layout_as_tree <- function (graph, root = numeric(), circular = FALSE, rootlevel = numeric(), 
    mode = "out", flip.y = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    root <- as.igraph.vs(graph, root) - 1
    circular <- as.logical(circular)
    rootlevel <- as.double(rootlevel)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    flip.y <- as.logical(flip.y)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_reingold_tilford", graph, root, 
        mode, rootlevel, circular, PACKAGE = "igraph")
    if (flip.y) {
        res[, 2] <- max(res[, 2]) - res[, 2]
    }
    res
}


without_loops <- function () 
{
    constructor_modifier(id = "without_loops")
}


graphlet_basis <- function (graph, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    graph2 <- graph
    graph2[[9]] <- list(c(1, 0, 1), list(), list(), list())
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_graphlets_candidate_basis", graph2, 
        weights, PACKAGE = "igraph")
    res
}


triad_census <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_triad_census", graph, PACKAGE = "igraph")
    res
}


is.directed <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_is_directed", graph, PACKAGE = "igraph")
}


get.edgelist <- function (graph, names = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- matrix(.Call("R_igraph_get_edgelist", graph, TRUE, 
        PACKAGE = "igraph"), ncol = 2)
    res <- res + 1
    if (names && "name" %in% vertex_attr_names(graph)) {
        res <- matrix(V(graph)$name[res], ncol = 2)
    }
    res
}


as_graphnel <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not an igraph graph")
    }
    if ("name" %in% vertex_attr_names(graph) && is.character(V(graph)$name)) {
        name <- V(graph)$name
    }
    else {
        name <- as.character(seq(vcount(graph)))
    }
    edgemode <- if (is_directed(graph)) 
        "directed"
    else "undirected"
    if ("weight" %in% edge_attr_names(graph) && is.numeric(E(graph)$weight)) {
        al <- lapply(as_adj_edge_list(graph, "out"), as.vector)
        for (i in seq(along = al)) {
            edges <- ends(graph, al[[i]], names = FALSE)
            edges <- ifelse(edges[, 2] == i, edges[, 1], edges[, 
                2])
            weights <- E(graph)$weight[al[[i]]]
            al[[i]] <- list(edges = edges, weights = weights)
        }
    }
    else {
        al <- as_adj_list(graph, "out")
        al <- lapply(al, function(x) list(edges = as.vector(x)))
    }
    names(al) <- name
    res <- new("graphNEL", nodes = name, edgeL = al, edgemode = edgemode)
    g.n <- graph_attr_names(graph)
    if ("directed" %in% g.n) {
        warning("Cannot add graph attribute `directed'")
        g.n <- g.n[g.n != "directed"]
    }
    for (n in g.n) {
        res@graphData[[n]] <- graph_attr(graph, n)
    }
    v.n <- vertex_attr_names(graph)
    v.n <- v.n[v.n != "name"]
    for (n in v.n) {
        graph::nodeDataDefaults(res, attr = n) <- NA
        graph::nodeData(res, attr = n) <- vertex_attr(graph, 
            n)
    }
    e.n <- edge_attr_names(graph)
    e.n <- e.n[e.n != "weight"]
    if (length(e.n) > 0) {
        el <- as_edgelist(graph)
        el <- paste(sep = "|", el[, 1], el[, 2])
        for (n in e.n) {
            graph::edgeDataDefaults(res, attr = n) <- NA
            res@edgeData@data[el] <- mapply(function(x, y) {
                xx <- c(x, y)
                names(xx)[length(xx)] <- n
                xx
            }, res@edgeData@data[el], edge_attr(graph, n), SIMPLIFY = FALSE)
        }
    }
    res
}


asym_pref <- function (...) 
constructor_spec(sample_asym_pref, ...)


atlas <- function (...) 
constructor_spec(graph_from_atlas, ...)


graph.kautz <- function (m, n) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_kautz", as.numeric(m), as.numeric(n), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("Kautz graph %i-%i", m, n)
        res$m <- m
        res$n <- n
    }
    res
}


cluster_spinglass <- function (graph, weights = NULL, vertex = NULL, spins = 25, parupdate = FALSE, 
    start.temp = 1, stop.temp = 0.01, cool.fact = 0.99, update.rule = c("config", 
        "random", "simple"), gamma = 1, implementation = c("orig", 
        "neg"), gamma.minus = 1) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    update.rule <- igraph.match.arg(update.rule)
    update.rule <- switch(update.rule, simple = 0, random = 0, 
        config = 1)
    implementation <- switch(igraph.match.arg(implementation), 
        orig = 0, neg = 1)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (is.null(vertex)) {
        res <- .Call("R_igraph_spinglass_community", graph, weights, 
            as.numeric(spins), as.logical(parupdate), as.numeric(start.temp), 
            as.numeric(stop.temp), as.numeric(cool.fact), as.numeric(update.rule), 
            as.numeric(gamma), as.numeric(implementation), as.numeric(gamma.minus), 
            PACKAGE = "igraph")
        res$algorithm <- "spinglass"
        res$vcount <- vcount(graph)
        res$membership <- res$membership + 1
        if (igraph_opt("add.vertex.names") && is_named(graph)) {
            res$names <- vertex_attr(graph, "name")
        }
        class(res) <- "communities"
    }
    else {
        res <- .Call("R_igraph_spinglass_my_community", graph, 
            weights, as.igraph.vs(graph, vertex) - 1, as.numeric(spins), 
            as.numeric(update.rule), as.numeric(gamma), PACKAGE = "igraph")
        res$community <- res$community + 1
    }
    res
}


graph.complementer <- function (graph, loops = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_complementer", graph, as.logical(loops), 
        PACKAGE = "igraph")
}


dim_select <- function (sv) 
{
    sv <- as.numeric(sv)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_dim_select", sv, PACKAGE = "igraph")
    res
}


unfold.tree <- function (graph, mode = c("all", "out", "in", "total"), roots) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    roots <- as.igraph.vs(graph, roots) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_unfold_tree", graph, mode, roots, 
        PACKAGE = "igraph")
    res
}


is.minimal.separator <- function (graph, candidate) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    candidate <- as.igraph.vs(graph, candidate)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_minimal_separator", graph, candidate - 
        1, PACKAGE = "igraph")
    res
}


graph.motifs <- function (graph, size = 3, cut.prob = rep(0, size)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cut.prob <- as.numeric(cut.prob)
    if (length(cut.prob) != size) {
        cut.prob <- c(cut.prob[-length(cut.prob)], rep(cut.prob[-length(cut.prob)], 
            length(cut.prob) - 1))
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_motifs_randesu", graph, as.integer(size), 
        as.numeric(cut.prob), PACKAGE = "igraph")
    res[is.nan(res)] <- NA
    res
}


hrg <- function (graph, prob) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    prob <- as.numeric(prob)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_create", graph, prob, PACKAGE = "igraph")
    class(res) <- "igraphHRG"
    res
}


is.connected <- function (graph, mode = c("weak", "strong")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, weak = 1, strong = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_is_connected", graph, as.numeric(mode), PACKAGE = "igraph")
}


knn <- function (graph, vids = V(graph), weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_avg_nearest_neighbor_degree", graph, 
        vids - 1, weights, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$knn) <- vertex_attr(graph, "name", vids)
    }
    res
}


vertex_attr_names <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    res <- base::.Call("R_igraph_mybracket2_names", graph, 9L, 
        3L, PACKAGE = "igraph")
    if (is.null(res)) {
        res <- character()
    }
    res
}


is_isomorphic_to <- function (graph1, graph2, method = c("auto", "direct", "vf2", 
    "bliss"), ...) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    method <- igraph.match.arg(method)
    if (method == "auto") {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_isomorphic", graph1, graph2, PACKAGE = "igraph")
    }
    else if (method == "direct") {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_isomorphic_34", graph1, graph2, PACKAGE = "igraph")
    }
    else if (method == "vf2") {
        graph.isomorphic.vf2(graph1, graph2, ...)$iso
    }
    else if (method == "bliss") {
        graph.isomorphic.bliss(graph1, graph2, ...)$iso
    }
}


is_simple <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_simple", graph, PACKAGE = "igraph")
    res
}


layout_with_fr <- function (graph, coords = NULL, dim = 2, niter = 500, start.temp = sqrt(vcount(graph)), 
    grid = c("auto", "grid", "nogrid"), weights = NULL, minx = NULL, 
    maxx = NULL, miny = NULL, maxy = NULL, minz = NULL, maxz = NULL, 
    coolexp, maxdelta, area, repulserad, maxiter) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(coords)) {
        coords <- as.matrix(structure(as.double(coords), dim = dim(coords)))
    }
    dim <- as.integer(dim)
    if (dim != 2L && dim != 3L) {
        stop("Dimension must be two or three")
    }
    if (!missing(niter) && !missing(maxiter)) {
        stop("Both `niter' and `maxiter' are given, give only one of them")
    }
    if (!missing(maxiter)) 
        niter <- maxiter
    niter <- as.integer(niter)
    start.temp <- as.numeric(start.temp)
    grid <- igraph.match.arg(grid)
    grid <- switch(grid, grid = 0L, nogrid = 1L, auto = 2L)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (!is.null(minx)) 
        minx <- as.numeric(minx)
    if (!is.null(maxx)) 
        maxx <- as.numeric(maxx)
    if (!is.null(miny)) 
        miny <- as.numeric(miny)
    if (!is.null(maxy)) 
        maxy <- as.numeric(maxy)
    if (!is.null(minz)) 
        minz <- as.numeric(minz)
    if (!is.null(maxz)) 
        maxz <- as.numeric(maxz)
    if (!missing(coolexp)) {
        warning("Argument `coolexp' is deprecated and has no effect")
    }
    if (!missing(maxdelta)) {
        warning("Argument `maxdelta' is deprecated and has no effect")
    }
    if (!missing(area)) {
        warning("Argument `area' is deprecated and has no effect")
    }
    if (!missing(repulserad)) {
        warning("Argument `repulserad' is deprecated and has no effect")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (dim == 2) {
        res <- .Call("R_igraph_layout_fruchterman_reingold", 
            graph, coords, niter, start.temp, weights, minx, 
            maxx, miny, maxy, grid, PACKAGE = "igraph")
    }
    else {
        res <- .Call("R_igraph_layout_fruchterman_reingold_3d", 
            graph, coords, niter, start.temp, weights, minx, 
            maxx, miny, maxy, minz, maxz, PACKAGE = "igraph")
    }
    res
}


maximal.cliques <- function (graph, min = NULL, max = NULL, subset = NULL, file = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(min)) {
        min <- 0
    }
    if (is.null(max)) {
        max <- 0
    }
    if (!is.null(subset)) {
        subset <- as.integer(as.igraph.vs(graph, subset) - 1)
    }
    if (!is.null(file)) {
        if (!is.character(file) || length(grep("://", file, fixed = TRUE)) > 
            0 || length(grep("~", file, fixed = TRUE)) > 0) {
            tmpfile <- TRUE
            origfile <- file
            file <- tempfile()
        }
        else {
            tmpfile <- FALSE
        }
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        res <- .Call("R_igraph_maximal_cliques_file", graph, 
            subset, file, as.numeric(min), as.numeric(max), PACKAGE = "igraph")
        if (tmpfile) {
            buffer <- read.graph.toraw(file)
            write.graph.fromraw(buffer, origfile)
        }
        invisible(NULL)
    }
    else {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        res <- .Call("R_igraph_maximal_cliques", graph, subset, 
            as.numeric(min), as.numeric(max), PACKAGE = "igraph")
        res <- lapply(res, function(x) x + 1)
        if (igraph_opt("return.vs.es")) {
            res <- lapply(res, create_vs, graph = graph)
        }
        res
    }
}


hub.score <- function (graph, scale = TRUE, weights = NULL, options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    scale <- as.logical(scale)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hub_score", graph, scale, weights, 
        options, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$vector) <- vertex_attr(graph, "name", )
    }
    res
}


edge_density <- function (graph, loops = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_density", graph, as.logical(loops), PACKAGE = "igraph")
}


layout_on_grid <- function (graph, width = 0, height = 0, dim = 2) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    width <- as.integer(width)
    dim <- as.integer(dim)
    stopifnot(dim == 2 || dim == 3)
    if (dim == 3) {
        height <- as.integer(height)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (dim == 2) {
        res <- .Call("R_igraph_layout_grid", graph, width, PACKAGE = "igraph")
    }
    else {
        res <- .Call("R_igraph_layout_grid_3d", graph, width, 
            height, PACKAGE = "igraph")
    }
    res
}


layout.davidson.harel <- function (graph, coords = NULL, maxiter = 10, fineiter = max(10, 
    log2(vcount(graph))), cool.fact = 0.75, weight.node.dist = 1, 
    weight.border = 0, weight.edge.lengths = edge_density(graph)/10, 
    weight.edge.crossings = 1 - sqrt(edge_density(graph)), weight.node.edge.dist = 0.2 * 
        (1 - edge_density(graph))) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(coords)) {
        coords <- as.matrix(structure(as.double(coords), dim = dim(coords)))
        use.seed <- TRUE
    }
    else {
        coords <- matrix(ncol = 2, nrow = 0)
        use.seed <- FALSE
    }
    maxiter <- as.integer(maxiter)
    fineiter <- as.integer(fineiter)
    cool.fact <- as.numeric(cool.fact)
    weight.node.dist <- as.numeric(weight.node.dist)
    weight.border <- as.numeric(weight.border)
    weight.edge.lengths <- as.numeric(weight.edge.lengths)
    weight.edge.crossings <- as.numeric(weight.edge.crossings)
    weight.node.edge.dist <- as.numeric(weight.node.edge.dist)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_davidson_harel", graph, coords, 
        use.seed, maxiter, fineiter, cool.fact, weight.node.dist, 
        weight.border, weight.edge.lengths, weight.edge.crossings, 
        weight.node.edge.dist, PACKAGE = "igraph")
    res
}


fastgreedy.community <- function (graph, merges = TRUE, modularity = TRUE, membership = TRUE, 
    weights = E(graph)$weight) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(weights)) {
        weights <- as.numeric(weights)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_fastgreedy", graph, as.logical(merges), 
        as.logical(modularity), as.logical(membership), weights, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$algorithm <- "fast greedy"
    res$vcount <- vcount(graph)
    res$membership <- res$membership + 1
    res$merges <- res$merges + 1
    class(res) <- "communities"
    res
}


head_of <- function (graph, es) 
{
    create_vs(graph, ends(graph, es, names = FALSE)[, 1])
}


cluster.distribution <- function (graph, cumulative = FALSE, mul.size = FALSE, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cs <- components(graph, ...)$csize
    hi <- hist(cs, -1:max(cs), plot = FALSE)$density
    if (mul.size) {
        hi <- hi * 1:max(cs)
        hi <- hi/sum(hi)
    }
    if (!cumulative) {
        res <- hi
    }
    else {
        res <- rev(cumsum(rev(hi)))
    }
    res
}


tk_canvas <- function (tkp.id) 
{
    .tkplot.get(tkp.id)$canvas
}


maximal_ivs <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maximal_independent_vertex_sets", 
        graph, PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


is.degree.sequence <- function (out.deg, in.deg = NULL) 
{
    out.deg <- as.numeric(out.deg)
    if (!is.null(in.deg)) 
        in.deg <- as.numeric(in.deg)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_degree_sequence", out.deg, in.deg, 
        PACKAGE = "igraph")
    res
}


edge_attr_names <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    res <- base::.Call("R_igraph_mybracket2_names", graph, 9L, 
        4L, PACKAGE = "igraph")
    if (is.null(res)) {
        res <- character()
    }
    res
}


`graph.attributes<-` <- function (graph, value) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.list(value) || (length(value) > 0 && is.null(names(value))) || 
        any(names(value) == "") || any(duplicated(names(value)))) {
        stop("Value must be a named list with unique names")
    }
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 2L, value, 
        PACKAGE = "igraph")
}


normalize <- function (xmin = -1, xmax = 1, ymin = xmin, ymax = xmax, zmin = xmin, 
    zmax = xmax) 
{
    args <- grab_args()
    layout_modifier(id = "normalize", args = args)
}


convex_hull <- function (data) 
{
    data <- as.matrix(structure(as.double(data), dim = dim(data)))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_convex_hull", data, PACKAGE = "igraph")
    res
}


`E<-` <- function (x, path = NULL, P = NULL, directed = NULL, value) 
{
    if (!is_igraph(x)) {
        stop("Not a graph object")
    }
    if (!"name" %in% names(attributes(value)) || !"value" %in% 
        names(attributes(value))) {
        stop("invalid indexing")
    }
    i_set_edge_attr(x, attr(value, "name"), index = value, value = attr(value, 
        "value"), check = FALSE)
}


gsize <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_ecount", graph, PACKAGE = "igraph")
}


has.multiple <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_has_multiple", graph, PACKAGE = "igraph")
    res
}


graph.diversity <- function (graph, weights = NULL, vids = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    vids <- as.igraph.vs(graph, vids)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_diversity", graph, weights, vids - 
        1, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


graph.isomorphic.vf2 <- function (graph1, graph2, vertex.color1, vertex.color2, edge.color1, 
    edge.color2) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    if (missing(vertex.color1)) {
        if ("color" %in% vertex_attr_names(graph1)) {
            vertex.color1 <- V(graph1)$color
        }
        else {
            vertex.color1 <- NULL
        }
    }
    if (!is.null(vertex.color1)) {
        vertex.color1 <- as.integer(vertex.color1) - 1L
    }
    if (missing(vertex.color2)) {
        if ("color" %in% vertex_attr_names(graph2)) {
            vertex.color2 <- V(graph2)$color
        }
        else {
            vertex.color2 <- NULL
        }
    }
    if (!is.null(vertex.color2)) {
        vertex.color2 <- as.integer(vertex.color2) - 1L
    }
    if (missing(edge.color1)) {
        if ("color" %in% edge_attr_names(graph1)) {
            edge.color1 <- E(graph1)$color
        }
        else {
            edge.color1 <- NULL
        }
    }
    if (!is.null(edge.color1)) {
        edge.color1 <- as.integer(edge.color1) - 1L
    }
    if (missing(edge.color2)) {
        if ("color" %in% edge_attr_names(graph2)) {
            edge.color2 <- E(graph2)$color
        }
        else {
            edge.color2 <- NULL
        }
    }
    if (!is.null(edge.color2)) {
        edge.color2 <- as.integer(edge.color2) - 1L
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_isomorphic_vf2", graph1, graph2, vertex.color1, 
        vertex.color2, edge.color1, edge.color2, PACKAGE = "igraph")
    res
}


graph.bfs <- function (graph, root, neimode = c("out", "in", "all", "total"), 
    unreachable = TRUE, restricted = NULL, order = TRUE, rank = FALSE, 
    father = FALSE, pred = FALSE, succ = FALSE, dist = FALSE, 
    callback = NULL, extra = NULL, rho = parent.frame()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (length(root) == 1) {
        root <- as.igraph.vs(graph, root) - 1
        roots <- NULL
    }
    else {
        roots <- as.igraph.vs(graph, root) - 1
        root <- 0
    }
    neimode <- switch(igraph.match.arg(neimode), out = 1, `in` = 2, 
        all = 3, total = 3)
    unreachable <- as.logical(unreachable)
    if (!is.null(restricted)) {
        restricted <- as.igraph.vs(graph, restricted)
    }
    if (!is.null(callback)) {
        callback <- as.function(callback)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_bfs", graph, root, roots, neimode, 
        unreachable, restricted, as.logical(order), as.logical(rank), 
        as.logical(father), as.logical(pred), as.logical(succ), 
        as.logical(dist), callback, extra, rho, PACKAGE = "igraph")
    if (order) 
        res$order <- res$order + 1
    if (rank) 
        res$rank <- res$rank + 1
    if (father) 
        res$father <- res$father + 1
    if (pred) 
        res$pred <- res$pred + 1
    if (succ) 
        res$succ <- res$succ + 1
    if (igraph_opt("return.vs.es")) {
        if (order) 
            res$order <- create_vs(graph, res$order, na_ok = TRUE)
        if (father) 
            res$father <- create_vs(graph, res$father, na_ok = TRUE)
        if (pred) 
            res$pred <- create_vs(graph, res$pred, na_ok = TRUE)
        if (succ) 
            res$succ <- create_vs(graph, res$succ, na_ok = TRUE)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        if (rank) 
            names(res$rank) <- V(graph)$name
        if (father) 
            names(res$father) <- V(graph)$name
        if (pred) 
            names(res$pred) <- V(graph)$name
        if (succ) 
            names(res$succ) <- V(graph)$name
        if (dist) 
            names(res$dist) <- V(graph)$name
    }
    res
}


induced.subgraph <- function (graph, vids, impl = c("auto", "copy_and_delete", "create_from_scratch")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    impl <- switch(igraph.match.arg(impl), auto = 0, copy_and_delete = 1, 
        create_from_scratch = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_induced_subgraph", graph, vids - 1, 
        impl, PACKAGE = "igraph")
    res
}


biconnected.components <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_biconnected_components", graph, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$tree_edges)) {
            res$tree_edges[[i_]] <- create_es(graph, res$tree_edges[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$component_edges)) {
            res$component_edges[[i_]] <- create_es(graph, res$component_edges[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$components)) {
            res$components[[i_]] <- create_vs(graph, res$components[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        res$articulation_points <- create_vs(graph, res$articulation_points)
    }
    res
}


pa_age <- function (...) 
constructor_spec(sample_pa_age, ...)


graph.de.bruijn <- function (m, n) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_de_bruijn", as.numeric(m), as.numeric(n), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("De-Bruijn graph %i-%i", m, n)
        res$m <- m
        res$n <- n
    }
    res
}


scgGrouping <- function (V, nt, mtype = c("symmetric", "laplacian", "stochastic"), 
    algo = c("optimum", "interv_km", "interv", "exact_scg"), 
    p = NULL, maxiter = 100) 
{
    V <- as.matrix(structure(as.double(V), dim = dim(V)))
    groups <- as.numeric(nt)
    mtype <- switch(igraph.match.arg(mtype), symmetric = 1, laplacian = 2, 
        stochastic = 3)
    algo <- switch(igraph.match.arg(algo), optimum = 1, interv_km = 2, 
        interv = 3, exact_scg = 4)
    if (!is.null(p)) 
        p <- as.numeric(p)
    maxiter <- as.integer(maxiter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_scg_grouping", V, as.integer(nt[1]), 
        if (length(nt) == 1) NULL else nt, mtype, algo, p, maxiter, 
        PACKAGE = "igraph")
    res
}


as_incidence_matrix <- function (graph, types = NULL, attr = NULL, names = TRUE, sparse = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        types <- as.logical(types)
    }
    else {
        stop("Not a bipartite graph, supply `types' argument")
    }
    names <- as.logical(names)
    sparse <- as.logical(sparse)
    if (sparse) {
        get.incidence.sparse(graph, types = types, names = names, 
            attr = attr)
    }
    else {
        get.incidence.dense(graph, types = types, names = names, 
            attr = attr)
    }
}


farthest.nodes <- function (graph, directed = TRUE, unconnected = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_farthest_points", graph, as.logical(directed), 
        as.logical(unconnected), weights, PACKAGE = "igraph")
    res <- list(vertices = res[1:2] + 1L, distance = res[3])
    if (igraph_opt("return.vs.es")) {
        res$vertices <- create_vs(graph, res$vertices)
    }
    res
}


get.adjacency <- function (graph, type = c("both", "upper", "lower"), attr = NULL, 
    edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!sparse) {
        get.adjacency.dense(graph, type = type, attr = attr, 
            edges = edges, names = names)
    }
    else {
        get.adjacency.sparse(graph, type = type, attr = attr, 
            edges = edges, names = names)
    }
}


graph.lattice <- function (dimvector = NULL, length = NULL, dim = NULL, nei = 1, 
    directed = FALSE, mutual = FALSE, circular = FALSE) 
{
    if (is.null(dimvector)) {
        dimvector <- rep(length, dim)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_lattice", as.numeric(dimvector), as.numeric(nei), 
        as.logical(directed), as.logical(mutual), as.logical(circular), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Lattice graph"
        res$dimvector <- dimvector
        res$nei <- nei
        res$mutual <- mutual
        res$circular <- circular
    }
    res
}


cit_cit_types <- function (...) 
constructor_spec(sample_cit_cit_types, ...)


`%--%` <- function (f, t) 
{
    from <- get(".igraph.from", parent.frame())
    to <- get(".igraph.to", parent.frame())
    graph <- get(".igraph.graph", parent.frame())
    f <- as.igraph.vs(graph, f) - 1
    t <- as.igraph.vs(graph, t) - 1
    (from %in% f & to %in% t) | (to %in% f & from %in% t)
}


lattice <- function (...) 
constructor_spec(make_lattice, ...)


unfold_tree <- function (graph, mode = c("all", "out", "in", "total"), roots) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    roots <- as.igraph.vs(graph, roots) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_unfold_tree", graph, mode, roots, 
        PACKAGE = "igraph")
    res
}


graph.extended.chordal.ring <- function (n, w) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_extended_chordal_ring", as.numeric(n), 
        as.matrix(w), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Extended chordal ring"
        res$w <- w
    }
    res
}


graph.isoclass.subgraph <- function (graph, vids) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_isoclass_subgraph", graph, vids, PACKAGE = "igraph")
    res
}


eigen_centrality <- function (graph, directed = FALSE, scale = TRUE, weights = NULL, 
    options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    scale <- as.logical(scale)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_eigenvector_centrality", graph, directed, 
        scale, weights, options, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$vector) <- vertex_attr(graph, "name", )
    }
    res
}


diameter <- function (graph, directed = TRUE, unconnected = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_diameter", graph, as.logical(directed), as.logical(unconnected), 
        weights, PACKAGE = "igraph")
}


graph.full <- function (n, directed = FALSE, loops = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_full", as.numeric(n), as.logical(directed), 
        as.logical(loops), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Full graph"
        res$loops <- loops
    }
    res
}


adhesion <- function (graph, checks = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_adhesion", graph, as.logical(checks), PACKAGE = "igraph")
}


connect.neighborhood <- function (graph, order, mode = c("all", "out", "in", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_connect_neighborhood", graph, as.numeric(order), 
        as.numeric(mode), PACKAGE = "igraph")
}


walktrap.community <- function (graph, weights = E(graph)$weight, steps = 4, merges = TRUE, 
    modularity = TRUE, membership = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object!")
    }
    if (membership && !modularity) {
        modularity <- TRUE
    }
    if (!is.null(weights)) {
        weights <- as.numeric(weights)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_walktrap_community", graph, weights, 
        as.numeric(steps), as.logical(merges), as.logical(modularity), 
        as.logical(membership), PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "walktrap"
    res$membership <- res$membership + 1
    res$merges <- res$merges + 1
    class(res) <- "communities"
    res
}


curve_multiple <- function (graph, start = 0.5) 
{
    cm <- count_multiple(graph)
    el <- apply(as_edgelist(graph, names = FALSE), 1, paste, 
        collapse = ":")
    ord <- order(el)
    res <- numeric(length(ord))
    p <- 1
    while (p <= length(res)) {
        m <- cm[ord[p]]
        idx <- p:(p + m - 1)
        if (m == 1) {
            r <- 0
        }
        else {
            r <- seq(-start, start, length = m)
        }
        res[ord[idx]] <- r
        p <- p + m
    }
    res
}


without_attr <- function () 
{
    constructor_modifier(id = "without_attr")
}


igraph.drl.coarsen <- structure(list(edge.cut = 0.8, init.iterations = 0, init.temperature = 2000, 
    init.attraction = 10, init.damping.mult = 1, liquid.iterations = 200, 
    liquid.temperature = 2000, liquid.attraction = 2, liquid.damping.mult = 1, 
    expansion.iterations = 200, expansion.temperature = 2000, 
    expansion.attraction = 10, expansion.damping.mult = 1, cooldown.iterations = 200, 
    cooldown.temperature = 2000, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 50, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 100, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult"))


centralize.scores <- function (scores, theoretical.max = 0, normalized = TRUE) 
{
    scores <- as.numeric(scores)
    theoretical.max <- as.numeric(theoretical.max)
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization", scores, theoretical.max, 
        normalized, PACKAGE = "igraph")
    res
}


count_motifs <- function (graph, size = 3, cut.prob = rep(0, size)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cut.prob <- as.numeric(cut.prob)
    if (length(cut.prob) != size) {
        cut.prob <- c(cut.prob[-length(cut.prob)], rep(cut.prob[-length(cut.prob)], 
            length(cut.prob) - 1))
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_motifs_randesu_no", graph, as.integer(size), 
        as.numeric(cut.prob), PACKAGE = "igraph")
}


make_chordal_ring <- function (n, w) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_extended_chordal_ring", as.numeric(n), 
        as.matrix(w), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Extended chordal ring"
        res$w <- w
    }
    res
}


dendPlot <- function (x, mode = igraph_opt("dend.plot.type"), ...) 
UseMethod("plot_dendrogram")


component_wise <- function (merge_method = "dla") 
{
    args <- grab_args()
    layout_modifier(id = "component_wise", args = args)
}


all_simple_paths <- function (graph, from, to = V(graph), mode = c("out", "in", "all", 
    "total")) 
{
    if (!is_igraph(graph)) 
        stop("Not a graph object")
    from <- as.igraph.vs(graph, from)
    to <- as.igraph.vs(graph, to)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_all_simple_paths", graph, from - 
        1, to - 1, mode, PACKAGE = "igraph")
    res <- get.all.simple.paths.pp(res)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


shapes <- function (shape = NULL) 
{
    if (is.null(shape)) {
        ls(.igraph.shapes)
    }
    else {
        .igraph.shapes[[shape]]
    }
}


is_graphical <- function (out.deg, in.deg = NULL) 
{
    out.deg <- as.numeric(out.deg)
    if (!is.null(in.deg)) 
        in.deg <- as.numeric(in.deg)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_graphical_degree_sequence", out.deg, 
        in.deg, PACKAGE = "igraph")
    res
}


split_join_distance <- function (comm1, comm2) 
{
    comm1 <- if (inherits(comm1, "communities")) {
        as.numeric(membership(comm1))
    }
    else {
        as.numeric(comm1)
    }
    comm2 <- if (inherits(comm2, "communities")) {
        as.numeric(membership(comm2))
    }
    else {
        as.numeric(comm2)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_split_join_distance", comm1, comm2, 
        PACKAGE = "igraph")
    unlist(res)
}


subgraph_centrality <- function (graph, diag = FALSE) 
{
    A <- as_adj(graph)
    if (!diag) {
        diag(A) <- 0
    }
    eig <- eigen(A)
    res <- as.vector(eig$vectors^2 %*% exp(eig$values))
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name")
    }
    res
}


gclust.app <- function (glist, r = 1, nmfout = FALSE, maxit = 10000, nmfmethod = "lee") 
{
    if (is.list(glist)) {
        Time <- length(glist)
        if (class(glist[[1]]) == "igraph") {
            glist <- lapply(glist, get.adjacency)
        }
        Xorigin <- sapply(glist, as.vector)
    }
    else {
        Xorigin = glist
    }
    Xraw = Xorigin
    XX = Xorigin %*% solve(diag(colSums(Xorigin)))
    stash = which(rowSums(XX) == 0)
    if (length(stash) > 0) {
        XX = XX[-stash, ]
    }
    mynmf = NMF::nmf(XX, rank = r, method = nmfmethod)
    WW = matrix(0, nrow = nrow(Xraw), ncol = r)
    if (length(stash) > 0) {
        WW[-stash, ] = cbind(NMF::basis(mynmf))
    }
    else {
        WW = NMF::basis(mynmf)
    }
    HH = coef(mynmf)
    if (r > 1) {
        HH = diag(colSums(WW)) %*% HH
        WW = round(WW %*% solve(diag(colSums(WW))), 12)
        HH = round(HH %*% solve(diag(colSums(HH))), 12)
    }
    else {
        HH = colSums(WW) %*% HH
        WW = round(WW/colSums(WW), 12)
        HH = round(HH %*% solve(diag(colSums(HH))), 12)
    }
    if (nmfout) 
        return(list(nmf = mynmf, W = WW, H = HH, Xorigin = Xraw))
    else return(list(nmf = NULL, W = WW, H = HH, Xorigin = Xraw))
}


scg_group <- function (V, nt, mtype = c("symmetric", "laplacian", "stochastic"), 
    algo = c("optimum", "interv_km", "interv", "exact_scg"), 
    p = NULL, maxiter = 100) 
{
    V <- as.matrix(structure(as.double(V), dim = dim(V)))
    groups <- as.numeric(nt)
    mtype <- switch(igraph.match.arg(mtype), symmetric = 1, laplacian = 2, 
        stochastic = 3)
    algo <- switch(igraph.match.arg(algo), optimum = 1, interv_km = 2, 
        interv = 3, exact_scg = 4)
    if (!is.null(p)) 
        p <- as.numeric(p)
    maxiter <- as.integer(maxiter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_scg_grouping", V, as.integer(nt[1]), 
        if (length(nt) == 1) NULL else nt, mtype, algo, p, maxiter, 
        PACKAGE = "igraph")
    res
}


count.multiple <- function (graph, eids = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_count_multiple", graph, as.igraph.es(graph, 
        eids) - 1, PACKAGE = "igraph")
}


remove.edge.attribute <- function (graph, name) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    name <- as.character(name)
    if (!name %in% edge_attr_names(graph)) {
        stop("No such edge attribute: ", name)
    }
    eattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 4L, 
        PACKAGE = "igraph")
    eattr[[name]] <- NULL
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 4L, eattr, 
        PACKAGE = "igraph")
}


tail_of <- function (graph, es) 
{
    create_vs(graph, ends(graph, es, names = FALSE)[, 2])
}


blocks <- function (blocks) 
{
    blocks$blocks
}


hrg.fit <- function (graph, hrg = NULL, start = FALSE, steps = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    start <- as.logical(start)
    steps <- as.integer(steps)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_fit", graph, hrg, start, steps, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    class(res) <- "igraphHRG"
    res
}


as.undirected <- function (graph, mode = c("collapse", "each", "mutual"), edge.attr.comb = igraph_opt("edge.attr.comb")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), collapse = 1, each = 0, 
        mutual = 2)
    edge.attr.comb <- igraph.i.attribute.combination(edge.attr.comb)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_to_undirected", graph, mode, edge.attr.comb, 
        PACKAGE = "igraph")
    res
}


get.diameter <- function (graph, directed = TRUE, unconnected = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_diameter", graph, as.logical(directed), 
        as.logical(unconnected), weights, PACKAGE = "igraph") + 
        1L
    if (igraph_opt("return.vs.es")) {
        res <- create_vs(graph, res)
    }
    res
}


vertices <- function (...) 
{
    structure(list(...), class = "igraph.vertex")
}


autocurve.edges <- function (graph, start = 0.5) 
{
    cm <- count_multiple(graph)
    el <- apply(as_edgelist(graph, names = FALSE), 1, paste, 
        collapse = ":")
    ord <- order(el)
    res <- numeric(length(ord))
    p <- 1
    while (p <= length(res)) {
        m <- cm[ord[p]]
        idx <- p:(p + m - 1)
        if (m == 1) {
            r <- 0
        }
        else {
            r <- seq(-start, start, length = m)
        }
        res[ord[idx]] <- r
        p <- p + m
    }
    res
}


asPhylo <- function (x, ...) 
UseMethod("as_phylo")


tkplot.getcoords <- function (tkp.id, norm = FALSE) 
{
    coords <- .tkplot.get(tkp.id, "coords")
    coords[, 2] <- max(coords[, 2]) - coords[, 2]
    if (norm) {
        coords[, 1] <- coords[, 1] - min(coords[, 1])
        coords[, 2] <- coords[, 2] - min(coords[, 2])
        coords[, 1] <- coords[, 1]/max(coords[, 1]) - 0.5
        coords[, 2] <- coords[, 2]/max(coords[, 2]) - 0.5
    }
    coords
}


membership <- function (communities) 
{
    if (!is.null(communities$membership)) {
        res <- communities$membership
    }
    else if (!is.null(communities$merges) && !is.null(communities$modularity)) {
        res <- community.to.membership2(communities$merges, communities$vcount, 
            which.max(communities$modularity))
    }
    else {
        stop("Cannot calculate community membership")
    }
    if (igraph_opt("add.vertex.names") && !is.null(communities$names)) {
        names(res) <- communities$names
    }
    class(res) <- "membership"
    res
}


layout.sphere <- function (..., params = list()) 
{
    do_call(layout_on_sphere, .args = c(list(...), params))
}


embed_adjacency_matrix <- function (graph, no, weights = NULL, which = c("lm", "la", "sa"), 
    scaled = TRUE, cvec = graph.strength(graph, weights = weights)/(vcount(graph) - 
        1), options = igraph.arpack.default) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    no <- as.integer(no)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    which <- switch(igraph.match.arg(which), lm = 0L, la = 2L, 
        sa = 3L)
    scaled <- as.logical(scaled)
    cvec <- as.numeric(cvec)
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_adjacency_spectral_embedding", graph, 
        no, weights, which, scaled, cvec, options, PACKAGE = "igraph")
    res
}


layout_with_dh <- function (graph, coords = NULL, maxiter = 10, fineiter = max(10, 
    log2(vcount(graph))), cool.fact = 0.75, weight.node.dist = 1, 
    weight.border = 0, weight.edge.lengths = edge_density(graph)/10, 
    weight.edge.crossings = 1 - sqrt(edge_density(graph)), weight.node.edge.dist = 0.2 * 
        (1 - edge_density(graph))) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(coords)) {
        coords <- as.matrix(structure(as.double(coords), dim = dim(coords)))
        use.seed <- TRUE
    }
    else {
        coords <- matrix(ncol = 2, nrow = 0)
        use.seed <- FALSE
    }
    maxiter <- as.integer(maxiter)
    fineiter <- as.integer(fineiter)
    cool.fact <- as.numeric(cool.fact)
    weight.node.dist <- as.numeric(weight.node.dist)
    weight.border <- as.numeric(weight.border)
    weight.edge.lengths <- as.numeric(weight.edge.lengths)
    weight.edge.crossings <- as.numeric(weight.edge.crossings)
    weight.node.edge.dist <- as.numeric(weight.node.edge.dist)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_davidson_harel", graph, coords, 
        use.seed, maxiter, fineiter, cool.fact, weight.node.dist, 
        weight.border, weight.edge.lengths, weight.edge.crossings, 
        weight.node.edge.dist, PACKAGE = "igraph")
    res
}


strength <- function (graph, vids = V(graph), mode = c("all", "out", "in", 
    "total"), loops = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    loops <- as.logical(loops)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_strength", graph, vids - 1, mode, 
        loops, weights, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


aging.ba.game <- function (n, pa.exp, aging.exp, m = NULL, aging.bin = 300, out.dist = NULL, 
    out.seq = NULL, out.pref = FALSE, directed = TRUE, zero.deg.appeal = 1, 
    zero.age.appeal = 0, deg.coef = 1, age.coef = 1, time.window = NULL) 
{
    if (!is.null(out.seq) && (!is.null(m) || !is.null(out.dist))) {
        warning("if `out.seq' is given `m' and `out.dist' should be NULL")
        m <- out.dist <- NULL
    }
    if (is.null(out.seq) && !is.null(out.dist) && !is.null(m)) {
        warning("if `out.dist' is given `m' will be ignored")
        m <- NULL
    }
    if (!is.null(out.seq) && length(out.seq) != n) {
        stop("`out.seq' should be of length `n'")
    }
    if (!is.null(out.seq) && min(out.seq) < 0) {
        stop("negative elements in `out.seq'")
    }
    if (!is.null(m) && m < 0) {
        stop("`m' is negative")
    }
    if (!is.null(time.window) && time.window <= 0) {
        stop("time window size should be positive")
    }
    if (!is.null(m) && m == 0) {
        warning("`m' is zero, graph will be empty")
    }
    if (pa.exp < 0) {
        warning("preferential attachment is negative")
    }
    if (aging.exp > 0) {
        warning("aging exponent is positive")
    }
    if (zero.deg.appeal <= 0) {
        warning("initial attractiveness is not positive")
    }
    if (is.null(m) && is.null(out.dist) && is.null(out.seq)) {
        m <- 1
    }
    n <- as.numeric(n)
    if (!is.null(m)) {
        m <- as.numeric(m)
    }
    if (!is.null(out.dist)) {
        out.dist <- as.numeric(out.dist)
    }
    if (!is.null(out.seq)) {
        out.seq <- as.numeric(out.seq)
    }
    out.pref <- as.logical(out.pref)
    if (!is.null(out.dist)) {
        out.seq <- as.numeric(sample(0:(length(out.dist) - 1), 
            n, replace = TRUE, prob = out.dist))
    }
    if (is.null(out.seq)) {
        out.seq <- numeric()
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- if (is.null(time.window)) {
        .Call("R_igraph_barabasi_aging_game", as.numeric(n), 
            as.numeric(pa.exp), as.numeric(aging.exp), as.numeric(aging.bin), 
            m, out.seq, out.pref, as.numeric(zero.deg.appeal), 
            as.numeric(zero.age.appeal), as.numeric(deg.coef), 
            as.numeric(age.coef), directed, PACKAGE = "igraph")
    }
    else {
        .Call("R_igraph_recent_degree_aging_game", as.numeric(n), 
            as.numeric(pa.exp), as.numeric(aging.exp), as.numeric(aging.bin), 
            m, out.seq, out.pref, as.numeric(zero.deg.appeal), 
            directed, time.window, PACKAGE = "igraph")
    }
    if (igraph_opt("add.params")) {
        res$name <- "Aging Barabasi graph"
        res$pa.exp <- pa.exp
        res$aging.exp <- aging.exp
        res$m <- m
        res$aging.bin <- aging.bin
        res$out.pref <- out.pref
        res$zero.deg.appeal <- zero.deg.appeal
        res$zero.age.appeal <- zero.age.appeal
        res$deg.coef <- deg.coef
        res$age.coef <- age.coef
        res$time.window <- if (is.null(time.window)) 
            Inf
        else time.window
    }
    res
}


hrg.consensus <- function (graph, hrg = NULL, start = FALSE, num.samples = 10000) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    start <- as.logical(start)
    num.samples <- as.integer(num.samples)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_consensus", graph, hrg, start, 
        num.samples, PACKAGE = "igraph")
    res
}


which_mutual <- function (graph, es = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    es <- as.igraph.es(graph, es)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_mutual", graph, es - 1, PACKAGE = "igraph")
    res
}


graphlets.project <- function (graph, weights = NULL, cliques, niter = 1000, Mu = rep(1, 
    length(cliques))) 
{
    if (!is.igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    Mu <- as.numeric(Mu)
    niter <- as.integer(niter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_graphlets_project", graph, weights, 
        cliques, Mu, niter, PACKAGE = "igraph")
    res
}


graph <- function (edges, ..., n = max(edges), isolates = NULL, directed = TRUE, 
    dir = directed, simplify = TRUE) 
{
    if (class(edges) == "formula") {
        if (!missing(n)) 
            stop("'n' should not be given for graph literals")
        if (!missing(isolates)) {
            stop("'isolates' should not be given for graph literals")
        }
        if (!missing(directed)) {
            stop("'directed' should not be given for graph literals")
        }
        mf <- as.list(match.call())[-1]
        mf[[1]] <- mf[[1]][[2]]
        graph_from_literal_i(mf)
    }
    else {
        if (!missing(simplify)) {
            stop("'simplify' should not be given for graph literals")
        }
        if (!missing(dir) && !missing(directed)) {
            stop("Only give one of 'dir' and 'directed'")
        }
        if (!missing(dir) && missing(directed)) 
            directed <- dir
        if (is.character(edges) && length(edges) == 1) {
            if (!missing(n)) 
                warning("'n' is ignored for the '", edges, "' graph")
            if (!missing(isolates)) {
                warning("'isolates' is ignored for the '", edges, 
                  "' graph")
            }
            if (!missing(directed)) {
                warning("'directed' is ignored for the '", edges, 
                  "' graph")
            }
            if (!missing(dir)) {
                warning("'dir' is ignored for the '", edges, 
                  "' graph")
            }
            if (length(list(...))) 
                stop("Extra arguments in make_graph")
            make_famous_graph(edges)
        }
        else if (is.numeric(edges) || is.null(edges) || (is.logical(edges) && 
            length(edges) == 0)) {
            if (is.null(edges) || is.logical(edges)) 
                edges <- as.numeric(edges)
            if (!is.null(isolates)) {
                warning("'isolates' ignored for numeric edge list")
            }
            old_graph <- function(edges, n = max(edges), directed = TRUE) {
                on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
                .Call("R_igraph_create", as.numeric(edges) - 
                  1, as.numeric(n), as.logical(directed), PACKAGE = "igraph")
            }
            args <- list(edges, ...)
            if (!missing(n)) 
                args <- c(args, list(n = n))
            if (!missing(directed)) 
                args <- c(args, list(directed = directed))
            do.call(old_graph, args)
        }
        else if (is.character(edges)) {
            if (!missing(n)) {
                warning("'n' is ignored for edge list with vertex names")
            }
            if (length(list(...))) 
                stop("Extra arguments in make_graph")
            el <- matrix(edges, ncol = 2, byrow = TRUE)
            res <- graph_from_edgelist(el, directed = directed)
            if (!is.null(isolates)) {
                isolates <- as.character(isolates)
                res <- res + vertices(isolates)
            }
            res
        }
        else {
            stop("'edges' must be numeric or character")
        }
    }
}


`edge.attributes<-` <- function (graph, index = E(graph), value) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.list(value) || (length(value) > 0 && is.null(names(value))) || 
        any(names(value) == "") || any(duplicated(names(value)))) {
        stop("Value must be a named list with unique names")
    }
    if (any(sapply(value, length) != length(index))) {
        stop("Invalid attribute value length, must match number of edges")
    }
    if (!missing(index)) {
        index <- as.igraph.es(graph, index)
        if (any(duplicated(index)) || any(is.na(index))) {
            stop("Invalid edges in index")
        }
    }
    if (!missing(index) && (length(index) != ecount(graph) || 
        any(index != E(graph)))) {
        es <- E(graph)
        for (i in seq_along(value)) {
            tmp <- value[[i]]
            length(tmp) <- 0
            length(tmp) <- length(es)
            tmp[index] <- value[[i]]
            value[[i]] <- tmp
        }
    }
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 4L, value, 
        PACKAGE = "igraph")
}


export_pajek <- function (blocks, graph, file, project.file = TRUE) 
{
    if (!project.file && !is.character(file)) {
        stop(paste("`file' must be a filename (without extension) when writing", 
            "to separate files"))
    }
    if (project.file) {
        return(exportPajek.cohesiveblocks.pf(blocks, graph, file))
    }
    else {
        return(exportPajek.cohesiveblocks.nopf(blocks, graph, 
            file))
    }
}


get.incidence <- function (graph, types = NULL, attr = NULL, names = TRUE, sparse = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        types <- as.logical(types)
    }
    else {
        stop("Not a bipartite graph, supply `types' argument")
    }
    names <- as.logical(names)
    sparse <- as.logical(sparse)
    if (sparse) {
        get.incidence.sparse(graph, types = types, names = names, 
            attr = attr)
    }
    else {
        get.incidence.dense(graph, types = types, names = names, 
            attr = attr)
    }
}


asymmetric.preference.game <- function (nodes, types, type.dist.matrix = matrix(1, types, types), 
    pref.matrix = matrix(1, types, types), loops = FALSE) 
{
    if (nrow(pref.matrix) != types || ncol(pref.matrix) != types) {
        stop("Invalid size for preference matrix")
    }
    if (nrow(type.dist.matrix) != types || ncol(type.dist.matrix) != 
        types) {
        stop("Invalid size for type distribution matrix")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_asymmetric_preference_game", as.double(nodes), 
        as.double(types), matrix(as.double(type.dist.matrix), 
            types, types), matrix(as.double(pref.matrix), types, 
            types), as.logical(loops), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Asymmetric preference random graph"
        res$types <- types
        res$type.dist.matrix <- type.dist.matrix
        res$pref.matrix <- pref.matrix
        res$loops <- loops
    }
}


layout.auto <- function (graph, dim = 2, ...) 
{
    if ("layout" %in% graph_attr_names(graph)) {
        lay <- graph_attr(graph, "layout")
        if (is.function(lay)) {
            lay(graph, ...)
        }
        else {
            lay
        }
    }
    else if (all(c("x", "y") %in% vertex_attr_names(graph))) {
        if ("z" %in% vertex_attr_names(graph)) {
            cbind(V(graph)$x, V(graph)$y, V(graph)$z)
        }
        else {
            cbind(V(graph)$x, V(graph)$y)
        }
    }
    else if (vcount(graph) < 1000) {
        layout_with_fr(graph, dim = dim, ...)
    }
    else {
        layout_with_drl(graph, dim = dim, ...)
    }
}


full_graph <- function (...) 
constructor_spec(make_full_graph, ...)


layout.graphopt <- function (graph, start = NULL, niter = 500, charge = 0.001, mass = 30, 
    spring.length = 0, spring.constant = 1, max.sa.movement = 5) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(start)) {
        start <- structure(as.numeric(start), dim = dim(start))
    }
    niter <- as.double(niter)
    charge <- as.double(charge)
    mass <- as.double(mass)
    spring.length <- as.double(spring.length)
    spring.constant <- as.double(spring.constant)
    max.sa.movement <- as.double(max.sa.movement)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_layout_graphopt", graph, niter, charge, mass, 
        spring.length, spring.constant, max.sa.movement, start, 
        PACKAGE = "igraph")
}


graph.eigen <- function (graph, algorithm = c("arpack", "auto", "lapack", "comp_auto", 
    "comp_lapack", "comp_arpack"), which = list(), options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    algorithm <- switch(igraph.match.arg(algorithm), auto = 0, 
        lapack = 1, arpack = 2, comp_auto = 3, comp_lapack = 4, 
        comp_arpack = 5)
    which.tmp <- eigen_defaults
    which.tmp[names(which)] <- which
    which <- which.tmp
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_eigen_adjacency", graph, algorithm, 
        which, options, PACKAGE = "igraph")
    res
}


nexus_info <- function (id, nexus.url = igraph_opt("nexus.url")) 
{
    if (inherits(id, "nexusDatasetInfo")) {
        id <- id$id
    }
    else if (inherits(id, "nexusDatasetInfoList")) {
        rid <- sapply(id, "[[", "id")
        res <- lapply(rid, nexus_info, nexus.url = nexus.url)
        class(res) <- class(id)
        attributes(res) <- attributes(id)
        return(res)
    }
    u <- paste(sep = "", nexus.url, "/api/dataset_info?format=text&id=", 
        id)
    f <- url(URLencode(u))
    l <- readLines(f)
    close(f)
    l2 <- character()
    for (i in seq_along(l)) {
        if (!grepl("^  ", l[i])) {
            l2 <- c(l2, l[i])
        }
        else {
            l2[length(l2)] <- paste(sep = "\n", l2[length(l2)], 
                sub("  ", "", l[i], fixed = TRUE))
        }
    }
    l2 <- lapply(l2, function(x) c(sub("[ ]*:.*$", "", x), sub("^[^:]*:[ ]*", 
        "", x)))
    res <- makeNexusDatasetInfo(l2)
    if (!"attributes" %in% names(res)) {
        res$attributes <- list()
    }
    return(res)
}


centr_clo_tmax <- function (graph = NULL, nodes = 0, mode = c("out", "in", "all", 
    "total")) 
{
    if (!is.null(graph) && !is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.integer(nodes)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_closeness_tmax", graph, 
        nodes, mode, PACKAGE = "igraph")
    res
}


get.adjlist <- function (graph, mode = c("all", "out", "in", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- as.numeric(switch(mode, out = 1, `in` = 2, all = 3, 
        total = 3))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_adjlist", graph, mode, PACKAGE = "igraph")
    res <- lapply(res, function(x) V(graph)[x + 1])
    if (is_named(graph)) 
        names(res) <- V(graph)$name
    res
}


vertex.connectivity <- function (graph, source = NULL, target = NULL, checks = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(source) && is.null(target)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_vertex_connectivity", graph, as.logical(checks), 
            PACKAGE = "igraph")
    }
    else if (!is.null(source) && !is.null(target)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_st_vertex_connectivity", graph, as.igraph.vs(graph, 
            source) - 1, as.igraph.vs(graph, target) - 1, PACKAGE = "igraph")
    }
    else {
        stop("either give both source and target or neither")
    }
}


get.adjedgelist <- function (graph, mode = c("all", "out", "in", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- as.numeric(switch(mode, out = 1, `in` = 2, all = 3, 
        total = 3))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_adjedgelist", graph, mode, PACKAGE = "igraph")
    res <- lapply(res, function(x) E(graph)[x + 1])
    if (is_named(graph)) 
        names(res) <- V(graph)$name
    res
}


watts.strogatz.game <- function (dim, size, nei, p, loops = FALSE, multiple = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_watts_strogatz_game", as.numeric(dim), 
        as.numeric(size), as.numeric(nei), as.numeric(p), as.logical(loops), 
        as.logical(multiple), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Watts-Strogatz random graph"
        res$dim <- dim
        res$size <- size
        res$nei <- nei
        res$p <- p
        res$loops <- loops
        res$multiple <- multiple
    }
    res
}


layout.kamada.kawai <- function (..., params = list()) 
{
    do_call(layout_with_kk, .args = c(list(...), params))
}


neighborhood.size <- function (graph, order, nodes = V(graph), mode = c("all", "out", 
    "in"), mindist = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    mindist <- as.integer(mindist)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_neighborhood_size", graph, as.igraph.vs(graph, 
        nodes) - 1, as.numeric(order), as.numeric(mode), mindist, 
        PACKAGE = "igraph")
}


dyad.census <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_dyad_census", graph, PACKAGE = "igraph")
    res
}


tkplot.reshape <- function (tkp.id, newlayout, ..., params) 
{
    tkp <- .tkplot.get(tkp.id)
    new_coords <- do_call(newlayout, .args = c(list(tkp$graph), 
        list(...), params))
    .tkplot.set(tkp.id, "coords", new_coords)
    tk_fit(tkp.id)
    .tkplot.update.vertices(tkp.id)
    invisible(NULL)
}


graph_from_isomorphism_class <- function (size, number, directed = TRUE) 
{
    size <- as.integer(size)
    number <- as.integer(number)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_isoclass_create", size, number, directed, 
        PACKAGE = "igraph")
    res
}


constraint <- function (graph, nodes = V(graph), weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.igraph.vs(graph, nodes)
    if (is.null(weights)) {
        if ("weight" %in% edge_attr_names(graph)) {
            weights <- E(graph)$weight
        }
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_constraint", graph, nodes - 1, as.numeric(weights), 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- V(graph)$name[nodes]
    }
    res
}


tkplot.close <- function (tkp.id, window.close = TRUE) 
{
    if (window.close) {
        cmd <- paste(sep = "", "tkp.", tkp.id, "$top")
        top <- eval(parse(text = cmd), .tkplot.env)
        tcltk::tkbind(top, "<Destroy>", "")
        tcltk::tkdestroy(top)
    }
    cmd <- paste(sep = "", "tkp.", tkp.id)
    rm(list = cmd, envir = .tkplot.env)
    invisible(NULL)
}


aging.barabasi.game <- function (n, pa.exp, aging.exp, m = NULL, aging.bin = 300, out.dist = NULL, 
    out.seq = NULL, out.pref = FALSE, directed = TRUE, zero.deg.appeal = 1, 
    zero.age.appeal = 0, deg.coef = 1, age.coef = 1, time.window = NULL) 
{
    if (!is.null(out.seq) && (!is.null(m) || !is.null(out.dist))) {
        warning("if `out.seq' is given `m' and `out.dist' should be NULL")
        m <- out.dist <- NULL
    }
    if (is.null(out.seq) && !is.null(out.dist) && !is.null(m)) {
        warning("if `out.dist' is given `m' will be ignored")
        m <- NULL
    }
    if (!is.null(out.seq) && length(out.seq) != n) {
        stop("`out.seq' should be of length `n'")
    }
    if (!is.null(out.seq) && min(out.seq) < 0) {
        stop("negative elements in `out.seq'")
    }
    if (!is.null(m) && m < 0) {
        stop("`m' is negative")
    }
    if (!is.null(time.window) && time.window <= 0) {
        stop("time window size should be positive")
    }
    if (!is.null(m) && m == 0) {
        warning("`m' is zero, graph will be empty")
    }
    if (pa.exp < 0) {
        warning("preferential attachment is negative")
    }
    if (aging.exp > 0) {
        warning("aging exponent is positive")
    }
    if (zero.deg.appeal <= 0) {
        warning("initial attractiveness is not positive")
    }
    if (is.null(m) && is.null(out.dist) && is.null(out.seq)) {
        m <- 1
    }
    n <- as.numeric(n)
    if (!is.null(m)) {
        m <- as.numeric(m)
    }
    if (!is.null(out.dist)) {
        out.dist <- as.numeric(out.dist)
    }
    if (!is.null(out.seq)) {
        out.seq <- as.numeric(out.seq)
    }
    out.pref <- as.logical(out.pref)
    if (!is.null(out.dist)) {
        out.seq <- as.numeric(sample(0:(length(out.dist) - 1), 
            n, replace = TRUE, prob = out.dist))
    }
    if (is.null(out.seq)) {
        out.seq <- numeric()
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- if (is.null(time.window)) {
        .Call("R_igraph_barabasi_aging_game", as.numeric(n), 
            as.numeric(pa.exp), as.numeric(aging.exp), as.numeric(aging.bin), 
            m, out.seq, out.pref, as.numeric(zero.deg.appeal), 
            as.numeric(zero.age.appeal), as.numeric(deg.coef), 
            as.numeric(age.coef), directed, PACKAGE = "igraph")
    }
    else {
        .Call("R_igraph_recent_degree_aging_game", as.numeric(n), 
            as.numeric(pa.exp), as.numeric(aging.exp), as.numeric(aging.bin), 
            m, out.seq, out.pref, as.numeric(zero.deg.appeal), 
            directed, time.window, PACKAGE = "igraph")
    }
    if (igraph_opt("add.params")) {
        res$name <- "Aging Barabasi graph"
        res$pa.exp <- pa.exp
        res$aging.exp <- aging.exp
        res$m <- m
        res$aging.bin <- aging.bin
        res$out.pref <- out.pref
        res$zero.deg.appeal <- zero.deg.appeal
        res$zero.age.appeal <- zero.age.appeal
        res$deg.coef <- deg.coef
        res$age.coef <- age.coef
        res$time.window <- if (is.null(time.window)) 
            Inf
        else time.window
    }
    res
}


edge_attr <- function (graph, name, index = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (missing(name)) {
        edge.attributes(graph, name)
    }
    else {
        name <- as.character(name)
        index <- as.igraph.es(graph, index)
        myattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 
            4L, PACKAGE = "igraph")[[name]]
        myattr[index]
    }
}


intersection <- function (...) 
UseMethod("intersection")


igraph_version <- function () 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_version", PACKAGE = "igraph")
}


add.vertices <- function (graph, nv, ..., attr = list()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    attrs <- list(...)
    attrs <- append(attrs, attr)
    nam <- names(attrs)
    if (length(attrs) != 0 && (is.null(nam) || any(nam == ""))) {
        stop("please supply names for attributes")
    }
    vertices.orig <- vcount(graph)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    graph <- .Call("R_igraph_add_vertices", graph, as.numeric(nv), 
        PACKAGE = "igraph")
    vertices.new <- vcount(graph)
    if (vertices.new - vertices.orig != 0) {
        idx <- seq(vertices.orig + 1, vertices.new)
    }
    else {
        idx <- numeric()
    }
    vattrs <- .Call("R_igraph_mybracket2", graph, 9L, 3L, PACKAGE = "igraph")
    for (i in seq(attrs)) {
        vattrs[[nam[i]]][idx] <- attrs[[nam[i]]]
    }
    .Call("R_igraph_mybracket2_set", graph, 9L, 3L, vattrs, PACKAGE = "igraph")
}


simplified <- function () 
{
    constructor_modifier(id = "simplified")
}


last_cit <- function (...) 
constructor_spec(sample_last_cit, ...)


console <- function () 
{
    oldverb <- igraph_opt("verbose")
    igraph_options(verbose = "tkconsole")
    pb <- .igraph.progress.tkconsole.create(oldverb)
    assign(".igraph.pb", pb, envir = asNamespace("igraph"))
    .igraph.progress.tkconsole.message("Console started.\n")
    invisible()
}


barabasi.game <- function (n, power = 1, m = NULL, out.dist = NULL, out.seq = NULL, 
    out.pref = FALSE, zero.appeal = 1, directed = TRUE, algorithm = c("psumtree", 
        "psumtree-multiple", "bag"), start.graph = NULL) 
{
    if (!is.null(start.graph) && !is_igraph(start.graph)) {
        stop("`start.graph' not an `igraph' object")
    }
    if (!is.null(out.seq) && (!is.null(m) || !is.null(out.dist))) {
        warning("if `out.seq' is given `m' and `out.dist' should be NULL")
        m <- out.dist <- NULL
    }
    if (is.null(out.seq) && !is.null(out.dist) && !is.null(m)) {
        warning("if `out.dist' is given `m' will be ignored")
        m <- NULL
    }
    if (!is.null(m) && m == 0) {
        warning("`m' is zero, graph will be empty")
    }
    if (power < 0) {
        warning("`power' is negative")
    }
    if (is.null(m) && is.null(out.dist) && is.null(out.seq)) {
        m <- 1
    }
    n <- as.numeric(n)
    power <- as.numeric(power)
    if (!is.null(m)) {
        m <- as.numeric(m)
    }
    if (!is.null(out.dist)) {
        out.dist <- as.numeric(out.dist)
    }
    if (!is.null(out.seq)) {
        out.seq <- as.numeric(out.seq)
    }
    out.pref <- as.logical(out.pref)
    if (!is.null(out.dist)) {
        nn <- if (is.null(start.graph)) 
            n
        else n - vcount(start.graph)
        out.seq <- as.numeric(sample(0:(length(out.dist) - 1), 
            nn, replace = TRUE, prob = out.dist))
    }
    if (is.null(out.seq)) {
        out.seq <- numeric()
    }
    algorithm <- igraph.match.arg(algorithm)
    algorithm1 <- switch(algorithm, psumtree = 1, `psumtree-multiple` = 2, 
        bag = 0)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_barabasi_game", n, power, m, out.seq, 
        out.pref, zero.appeal, directed, algorithm1, start.graph, 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Barabasi graph"
        res$power <- power
        res$m <- m
        res$zero.appeal <- zero.appeal
        res$algorithm <- algorithm
    }
    res
}


ends <- function (graph, es, names = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    es2 <- as.igraph.es(graph, na.omit(es)) - 1
    res <- matrix(NA_integer_, ncol = length(es), nrow = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (length(es) == 1) {
        res[, !is.na(es)] <- .Call("R_igraph_get_edge", graph, 
            es2, PACKAGE = "igraph") + 1
    }
    else {
        res[, !is.na(es)] <- .Call("R_igraph_edges", graph, es2, 
            PACKAGE = "igraph") + 1
    }
    if (names && is_named(graph)) {
        res <- vertex_attr(graph, "name")[res]
    }
    matrix(res, ncol = 2, byrow = TRUE)
}


get.shortest.paths <- function (graph, from, to = V(graph), mode = c("out", "all", 
    "in"), weights = NULL, output = c("vpath", "epath", "both"), 
    predecessors = FALSE, inbound.edges = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    output <- igraph.match.arg(output)
    output <- switch(output, vpath = 0, epath = 1, both = 2)
    if (is.null(weights)) {
        if ("weight" %in% edge_attr_names(graph)) {
            weights <- as.numeric(E(graph)$weight)
        }
    }
    else {
        if (length(weights) == 1 && is.na(weights)) {
            weights <- NULL
        }
        else {
            weights <- as.numeric(weights)
        }
    }
    to <- as.igraph.vs(graph, to) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_shortest_paths", graph, as.igraph.vs(graph, 
        from) - 1, to, as.numeric(mode), as.numeric(length(to)), 
        weights, as.numeric(output), as.logical(predecessors), 
        as.logical(inbound.edges), PACKAGE = "igraph")
    if (!is.null(res$vpath)) {
        res$vpath <- lapply(res$vpath, function(x) x + 1)
    }
    if (!is.null(res$epath)) {
        res$epath <- lapply(res$epath, function(x) x + 1)
    }
    if (!is.null(res$predecessors)) {
        res$predecessors <- res$predecessors + 1
    }
    if (!is.null(res$inbound_edges)) {
        res$inbound_edges <- res$inbound_edges + 1
    }
    if (igraph_opt("return.vs.es")) {
        if (!is.null(res$vpath)) {
            res$vpath <- lapply(res$vpath, create_vs, graph = graph)
        }
        if (!is.null(res$epath)) {
            res$epath <- lapply(res$epath, create_es, graph = graph)
        }
        if (!is.null(res$predecessors)) {
            res$predecessors <- create_vs(res$predecessors, graph = graph, 
                na_ok = TRUE)
        }
        if (!is.null(res$inbound_edges)) {
            res$inbound_edges <- create_es(res$inbound_edges, 
                graph = graph, na_ok = TRUE)
        }
    }
    res
}


`V<-` <- function (x, value) 
{
    if (!is_igraph(x)) {
        stop("Not a graph object")
    }
    if (!"name" %in% names(attributes(value)) || !"value" %in% 
        names(attributes(value))) {
        stop("invalid indexing")
    }
    i_set_vertex_attr(x, attr(value, "name"), index = value, 
        value = attr(value, "value"), check = FALSE)
}


cluster_louvain <- function (graph, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_multilevel", graph, weights, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "multi level"
    res$membership <- res$membership + 1
    res$memberships <- res$memberships + 1
    class(res) <- "communities"
    res
}


graph.full.citation <- function (n, directed = TRUE) 
{
    n <- as.integer(n)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_full_citation", n, directed, PACKAGE = "igraph")
    res <- set_graph_attr(res, "name", "Full citation graph")
    res
}


fit_power_law <- function (x, xmin = NULL, start = 2, force.continuous = FALSE, 
    implementation = c("plfit", "R.mle"), ...) 
{
    implementation <- igraph.match.arg(implementation)
    if (implementation == "r.mle") {
        power.law.fit.old(x, xmin, start, ...)
    }
    else if (implementation == "plfit") {
        if (is.null(xmin)) 
            xmin <- -1
        power.law.fit.new(x, xmin = xmin, force.continuous = force.continuous)
    }
}


induced_subgraph <- function (graph, vids, impl = c("auto", "copy_and_delete", "create_from_scratch")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    impl <- switch(igraph.match.arg(impl), auto = 0, copy_and_delete = 1, 
        create_from_scratch = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_induced_subgraph", graph, vids - 1, 
        impl, PACKAGE = "igraph")
    res
}


igraph.drl.default <- structure(list(edge.cut = 0.8, init.iterations = 0, init.temperature = 2000, 
    init.attraction = 10, init.damping.mult = 1, liquid.iterations = 200, 
    liquid.temperature = 2000, liquid.attraction = 10, liquid.damping.mult = 1, 
    expansion.iterations = 200, expansion.temperature = 2000, 
    expansion.attraction = 2, expansion.damping.mult = 1, cooldown.iterations = 200, 
    cooldown.temperature = 2000, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 50, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 100, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult"))


gclust.rsvt <- function (glist, r = 1, maxsvt = 10, nmfout = FALSE, maxit = 10000, 
    nmfmethod = "lee") 
{
    if (is.list(glist)) {
        Time <- length(glist)
        if (class(glist[[1]]) == "igraph") {
            glist <- lapply(glist, get.adjacency)
        }
        Xorigin <- sapply(glist, as.vector)
    }
    else {
        Xorigin = glist
    }
    Xraw = Xorigin
    Xorigin = Xorigin %*% solve(diag(colSums(Xorigin)))
    mysvd = tryCatch(irlba::irlba(Xorigin, r, r, maxit = maxit), 
        error = function(e) svd(Xorigin, r, r))
    U = mysvd$u[, 1:r, drop = FALSE]
    V = mysvd$v[, 1:r, drop = FALSE]
    if (r == 1) {
        S = matrix(mysvd$d[1:r], 1, 1)
    }
    else {
        S = diag(mysvd$d[1:r])
    }
    maxsvt = ifelse(maxsvt > 0, maxsvt, 1)
    for (itr in 1:maxsvt) {
        X = U %*% S %*% t(V)
        X[X < 0] = 0
        mysvd = tryCatch(irlba::irlba(X, r, r, maxit = maxit), 
            error = function(e) svd(X, r, r))
        UU = mysvd$u[, 1:r, drop = FALSE]
        VV = mysvd$v[, 1:r, drop = FALSE]
        if (r == 1) {
            SS = matrix(mysvd$d[1:r], 1, 1)
        }
        else {
            SS = diag(mysvd$d[1:r])
        }
        if (norm(UU - U) + norm(VV - V) + norm(SS - S) < 1e-12) {
            break
        }
        U = UU
        V = VV
        S = SS
    }
    XX = X %*% solve(diag(colSums(X)))
    stash = which(rowSums(XX) == 0)
    if (length(stash) > 0) {
        XX = XX[-stash, ]
    }
    mynmf = NMF::nmf(XX, rank = r, method = nmfmethod)
    WW = matrix(0, nrow = nrow(X), ncol = r)
    if (length(stash) > 0) {
        WW[-stash, ] = NMF::basis(mynmf)
    }
    else {
        WW = NMF::basis(mynmf)
    }
    HH = coef(mynmf)
    if (r > 1) {
        HH = diag(colSums(WW)) %*% HH
        WW = round(WW %*% solve(diag(colSums(WW))), 12)
        HH = round(HH %*% solve(diag(colSums(HH))), 12)
    }
    else {
        HH = colSums(WW) %*% HH
        WW = round(WW/colSums(WW), 12)
        HH = round(HH %*% solve(diag(colSums(HH))), 12)
    }
    if (nmfout) 
        return(list(nmf = mynmf, W = WW, H = HH, Xorigin = Xraw))
    else return(list(nmf = NULL, W = WW, H = HH, Xorigin = Xraw))
}


`edge_attr<-` <- function (graph, name, index = E(graph), value) 
{
    if (missing(name)) {
        `edge.attributes<-`(graph, index = index, value = value)
    }
    else {
        set_edge_attr(graph, name = name, index = index, value = value)
    }
}


multilevel.community <- function (graph, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_multilevel", graph, weights, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "multi level"
    res$membership <- res$membership + 1
    res$memberships <- res$memberships + 1
    class(res) <- "communities"
    res
}


get.edge.attribute <- function (graph, name, index = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (missing(name)) {
        edge.attributes(graph, name)
    }
    else {
        name <- as.character(name)
        index <- as.igraph.es(graph, index)
        myattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 
            4L, PACKAGE = "igraph")[[name]]
        myattr[index]
    }
}


plot_dendrogram <- function (x, mode = igraph_opt("dend.plot.type"), ...) 
UseMethod("plot_dendrogram")


tk_center <- function (tkp.id) 
{
    tkp <- .tkplot.get(tkp.id)
    width <- as.numeric(tcltk::tkwinfo("width", tkp$canvas))
    height <- as.numeric(tcltk::tkwinfo("height", tkp$canvas))
    coords <- .tkplot.get(tkp.id, "coords")
    canvas.center.x <- width/2
    canvas.center.y <- height/2
    coords <- .tkplot.get(tkp.id, "coords")
    r1 <- range(coords[, 1])
    r2 <- range(coords[, 2])
    coords.center.x <- (r1[1] + r1[2])/2
    coords.center.y <- (r2[1] + r2[2])/2
    coords[, 1] <- coords[, 1] + canvas.center.x - coords.center.x
    coords[, 2] <- coords[, 2] + canvas.center.y - coords.center.y
    .tkplot.set(tkp.id, "coords", coords)
    .tkplot.update.vertices(tkp.id)
    invisible(NULL)
}


articulation.points <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_articulation_points", graph, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res <- create_vs(graph, res)
    }
    res
}


igraph.shape.noclip <- function (coords, el, params, end = c("both", "from", "to")) 
{
    end <- igraph.match.arg(end)
    if (end == "both") {
        coords
    }
    else if (end == "from") {
        coords[, 1:2, drop = FALSE]
    }
    else {
        coords[, 3:4, drop = FALSE]
    }
}


igraph.shape.noplot <- function (coords, v = NULL, params) 
{
    invisible(NULL)
}


keeping_degseq <- function (loops = FALSE, niter = 100) 
{
    method <- list(fun = rewire_keeping_degseq, args = list(loops = loops, 
        niter = niter))
    add_class(method, "igraph_rewiring_method")
}


k.regular.game <- function (no.of.nodes, k, directed = FALSE, multiple = FALSE) 
{
    no.of.nodes <- as.integer(no.of.nodes)
    k <- as.integer(k)
    directed <- as.logical(directed)
    multiple <- as.logical(multiple)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_k_regular_game", no.of.nodes, k, directed, 
        multiple, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "k-regular graph")
    res <- set.graph.attribute(res, "k", k)
    res
}


get.graph.attribute <- function (graph, name) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (missing(name)) {
        graph.attributes(graph)
    }
    else {
        base::.Call("R_igraph_mybracket2", graph, 9L, 2L, PACKAGE = "igraph")[[as.character(name)]]
    }
}


random_walk <- function (graph, start, steps, mode = c("out", "in", "all"), 
    stuck = c("return", "error")) 
{
    if (!is_igraph(graph)) 
        stop("Not a graph object")
    start <- as.igraph.vs(graph, start)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    steps <- as.integer(steps)
    stuck <- switch(igraph.match.arg(stuck), error = 0L, return = 1L)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_random_walk", graph, start - 1, mode, 
        steps, stuck, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res <- create_vs(graph, res)
    }
    res
}


igraph.drl.final <- structure(list(edge.cut = 0.8, init.iterations = 0, init.temperature = 50, 
    init.attraction = 0.5, init.damping.mult = 0, liquid.iterations = 0, 
    liquid.temperature = 2000, liquid.attraction = 2, liquid.damping.mult = 1, 
    expansion.iterations = 50, expansion.temperature = 2000, 
    expansion.attraction = 2, expansion.damping.mult = 1, cooldown.iterations = 50, 
    cooldown.temperature = 200, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 50, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 25, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult"))


power.law.fit <- function (x, xmin = NULL, start = 2, force.continuous = FALSE, 
    implementation = c("plfit", "R.mle"), ...) 
{
    implementation <- igraph.match.arg(implementation)
    if (implementation == "r.mle") {
        power.law.fit.old(x, xmin, start, ...)
    }
    else if (implementation == "plfit") {
        if (is.null(xmin)) 
            xmin <- -1
        power.law.fit.new(x, xmin = xmin, force.continuous = force.continuous)
    }
}


igraph.to.graphNEL <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not an igraph graph")
    }
    if ("name" %in% vertex_attr_names(graph) && is.character(V(graph)$name)) {
        name <- V(graph)$name
    }
    else {
        name <- as.character(seq(vcount(graph)))
    }
    edgemode <- if (is_directed(graph)) 
        "directed"
    else "undirected"
    if ("weight" %in% edge_attr_names(graph) && is.numeric(E(graph)$weight)) {
        al <- lapply(as_adj_edge_list(graph, "out"), as.vector)
        for (i in seq(along = al)) {
            edges <- ends(graph, al[[i]], names = FALSE)
            edges <- ifelse(edges[, 2] == i, edges[, 1], edges[, 
                2])
            weights <- E(graph)$weight[al[[i]]]
            al[[i]] <- list(edges = edges, weights = weights)
        }
    }
    else {
        al <- as_adj_list(graph, "out")
        al <- lapply(al, function(x) list(edges = as.vector(x)))
    }
    names(al) <- name
    res <- new("graphNEL", nodes = name, edgeL = al, edgemode = edgemode)
    g.n <- graph_attr_names(graph)
    if ("directed" %in% g.n) {
        warning("Cannot add graph attribute `directed'")
        g.n <- g.n[g.n != "directed"]
    }
    for (n in g.n) {
        res@graphData[[n]] <- graph_attr(graph, n)
    }
    v.n <- vertex_attr_names(graph)
    v.n <- v.n[v.n != "name"]
    for (n in v.n) {
        graph::nodeDataDefaults(res, attr = n) <- NA
        graph::nodeData(res, attr = n) <- vertex_attr(graph, 
            n)
    }
    e.n <- edge_attr_names(graph)
    e.n <- e.n[e.n != "weight"]
    if (length(e.n) > 0) {
        el <- as_edgelist(graph)
        el <- paste(sep = "|", el[, 1], el[, 2])
        for (n in e.n) {
            graph::edgeDataDefaults(res, attr = n) <- NA
            res@edgeData@data[el] <- mapply(function(x, y) {
                xx <- c(x, y)
                names(xx)[length(xx)] <- n
                xx
            }, res@edgeData@data[el], edge_attr(graph, n), SIMPLIFY = FALSE)
        }
    }
    res
}


centralization.evcent.tmax <- function (graph = NULL, nodes = 0, directed = FALSE, scale = TRUE) 
{
    if (!is.null(graph) && !is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.integer(nodes)
    directed <- as.logical(directed)
    scale <- as.logical(scale)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_eigenvector_centrality_tmax", 
        graph, nodes, directed, scale, PACKAGE = "igraph")
    res
}


layout.bipartite <- function (graph, types = NULL, hgap = 1, vgap = 1, maxiter = 100) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        if (!is.logical(types)) {
            warning("vertex types converted to logical")
        }
        types <- as.logical(types)
        if (any(is.na(types))) {
            stop("`NA' is not allowed in vertex types")
        }
    }
    else {
        stop("Not a bipartite graph, supply `types' argument")
    }
    hgap <- as.numeric(hgap)
    vgap <- as.numeric(vgap)
    maxiter <- as.integer(maxiter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_bipartite", graph, types, hgap, 
        vgap, maxiter, PACKAGE = "igraph")
    res
}


with_fr <- function (...) 
layout_spec(layout_with_fr, ...)


cohesion <- function (x, ...) 
UseMethod("cohesion")


min_separators <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_minimum_size_separators", graph, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res)) {
            res[[i_]] <- create_vs(graph, res[[i_]])
        }
    }
    res
}


is.mutual <- function (graph, es = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    es <- as.igraph.es(graph, es)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_mutual", graph, es - 1, PACKAGE = "igraph")
    res
}


make_undirected_graph <- function (edges, n = max(edges)) 
{
    if (missing(n)) {
        make_graph(edges, directed = FALSE)
    }
    else {
        make_graph(edges, n = n, directed = FALSE)
    }
}


on_sphere <- function (...) 
layout_spec(layout_on_sphere, ...)


is.simple <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_simple", graph, PACKAGE = "igraph")
    res
}


graph.laplacian <- function (graph, normalized = FALSE, weights = NULL, sparse = igraph_opt("sparsematrices")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    normalized <- as.logical(normalized)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    sparse <- as.logical(sparse)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_laplacian", graph, normalized, weights, 
        sparse, PACKAGE = "igraph")
    if (sparse) {
        res <- igraph.i.spMatrix(res)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        rownames(res) <- colnames(res) <- V(graph)$name
    }
    res
}


nicely <- function (...) 
layout_spec(layout_nicely, ...)


showtrace <- function (communities) 
{
    if (!inherits(communities, "communities")) {
        stop("Not a community structure")
    }
    if (is.null(communities$history)) {
        stop("History was not recorded")
    }
    res <- character()
    i <- 1
    while (i <= length(communities$history)) {
        if (communities$history[i] == 2) {
            resnew <- paste("Splitting community", communities$history[i + 
                1], "into two.")
            i <- i + 2
        }
        else if (communities$history[i] == 3) {
            resnew <- paste("Failed splitting community", communities$history[i + 
                1], "into two.")
            i <- i + 2
        }
        else if (communities$history[i] == 4) {
            resnew <- "Starting with the whole graph as a community."
            i <- i + 1
        }
        else if (communities$history[i] == 5) {
            resnew <- paste("Starting from the", communities$history[i + 
                1], "given communities.")
            i <- i + 2
        }
        res <- c(res, resnew)
    }
    res
}


degree <- function (graph, v = V(graph), mode = c("all", "out", "in", "total"), 
    loops = TRUE, normalized = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    v <- as.igraph.vs(graph, v)
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_degree", graph, v - 1, as.numeric(mode), 
        as.logical(loops), PACKAGE = "igraph")
    if (normalized) {
        res <- res/(vcount(graph) - 1)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- V(graph)$name[v]
    }
    res
}


assortativity_degree <- function (graph, directed = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_assortativity_degree", graph, directed, 
        PACKAGE = "igraph")
    res
}


clique.number <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_clique_number", graph, PACKAGE = "igraph")
}


sample_k_regular <- function (no.of.nodes, k, directed = FALSE, multiple = FALSE) 
{
    no.of.nodes <- as.integer(no.of.nodes)
    k <- as.integer(k)
    directed <- as.logical(directed)
    multiple <- as.logical(multiple)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_k_regular_game", no.of.nodes, k, directed, 
        multiple, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "k-regular graph")
    res <- set.graph.attribute(res, "k", k)
    res
}


fit_hrg <- function (graph, hrg = NULL, start = FALSE, steps = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    start <- as.logical(start)
    steps <- as.integer(steps)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_fit", graph, hrg, start, steps, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    class(res) <- "igraphHRG"
    res
}


make_clusters <- function (graph, membership = NULL, algorithm = NULL, merges = NULL, 
    modularity = TRUE) 
{
    stopifnot(is.null(membership) || is.numeric(membership))
    stopifnot(is.null(algorithm) || (is.character(algorithm) && 
        length(algorithm) == 1))
    stopifnot(is.null(merges) || (is.matrix(merges) && is.numeric(merges) && 
        ncol(merges) == 2))
    stopifnot(is.null(modularity) || (is.logical(modularity) && 
        length(modularity) == 1) || (is.numeric(modularity) && 
        length(modularity) %in% c(1, length(membership))))
    if (is.logical(modularity)) {
        if (modularity && !is.null(membership)) {
            modularity <- modularity(graph, membership)
        }
        else {
            modularity <- NULL
        }
    }
    res <- list(membership = membership, algorithm = if (is.null(algorithm)) "unknown" else algorithm, 
        modularity = modularity)
    if (!is.null(merges)) {
        res$merges <- merges
    }
    if (!is.null(membership)) {
        res$vcount <- length(membership)
    }
    else if (!is.null(merges)) {
        res$vcount <- nrow(merges) + 1
    }
    class(res) <- "communities"
    res
}


motifs <- function (graph, size = 3, cut.prob = rep(0, size)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cut.prob <- as.numeric(cut.prob)
    if (length(cut.prob) != size) {
        cut.prob <- c(cut.prob[-length(cut.prob)], rep(cut.prob[-length(cut.prob)], 
            length(cut.prob) - 1))
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_motifs_randesu", graph, as.integer(size), 
        as.numeric(cut.prob), PACKAGE = "igraph")
    res[is.nan(res)] <- NA
    res
}


label.propagation.community <- function (graph, weights = NULL, initial = NULL, fixed = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (!is.null(initial)) 
        initial <- as.numeric(initial)
    if (!is.null(fixed)) 
        fixed <- as.logical(fixed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_label_propagation", graph, 
        weights, initial, fixed, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "label propagation"
    res$membership <- res$membership + 1
    class(res) <- "communities"
    res
}


plot_hierarchy <- function (blocks, layout = layout_as_tree(hierarchy(blocks), 
    root = 1), ...) 
{
    plot(hierarchy(blocks), layout = layout, ...)
}


delete_edges <- function (graph, edges) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_delete_edges", graph, as.igraph.es(graph, 
        edges) - 1, PACKAGE = "igraph")
}


get.stochastic <- function (graph, column.wise = FALSE, sparse = igraph_opt("sparsematrices")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    column.wise <- as.logical(column.wise)
    if (length(column.wise) != 1) {
        stop("`column.wise' must be a logical scalar")
    }
    sparse <- as.logical(sparse)
    if (length(sparse) != 1) {
        stop("`sparse' must be a logical scalar")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (sparse) {
        res <- .Call("R_igraph_get_stochastic_sparsemat", graph, 
            column.wise, PACKAGE = "igraph")
        res <- igraph.i.spMatrix(res)
    }
    else {
        res <- .Call("R_igraph_get_stochastic", graph, column.wise, 
            PACKAGE = "igraph")
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        rownames(res) <- colnames(res) <- V(graph)$name
    }
    res
}


delete.edges <- function (graph, edges) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_delete_edges", graph, as.igraph.es(graph, 
        edges) - 1, PACKAGE = "igraph")
}


is.chordal <- function (graph, alpha = NULL, alpham1 = NULL, fillin = FALSE, 
    newgraph = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(alpha)) 
        alpha <- as.numeric(alpha) - 1
    if (!is.null(alpham1)) 
        alpham1 <- as.numeric(alpham1) - 1
    fillin <- as.logical(fillin)
    newgraph <- as.logical(newgraph)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_chordal", graph, alpha, alpham1, 
        fillin, newgraph, PACKAGE = "igraph")
    if (fillin) {
        res$fillin <- res$fillin + 1
    }
    res
}


random.graph.game <- function (n, p.or.m, type = c("gnp", "gnm"), directed = FALSE, 
    loops = FALSE, ...) 
{
    type <- igraph.match.arg(type)
    type1 <- switch(type, gnp = 0, gnm = 1)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_erdos_renyi_game", as.numeric(n), 
        as.numeric(type1), as.numeric(p.or.m), as.logical(directed), 
        as.logical(loops), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("Erdos renyi (%s) graph", type)
        res$type <- type
        res$loops <- loops
        if (type == "gnp") {
            res$p <- p.or.m
        }
        if (type == "gnm") {
            res$m <- p.or.m
        }
    }
    res
}


delete_vertex_attr <- function (graph, name) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    name <- as.character(name)
    if (!name %in% vertex_attr_names(graph)) {
        stop("No such vertex attribute: ", name)
    }
    vattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 3L, 
        PACKAGE = "igraph")
    vattr[[name]] <- NULL
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 3L, vattr, 
        PACKAGE = "igraph")
}


scg_semi_proj <- function (groups, mtype = c("symmetric", "laplacian", "stochastic"), 
    p = NULL, norm = c("row", "col"), sparse = igraph_opt("sparsematrices")) 
{
    groups <- as.numeric(groups) - 1
    mtype <- switch(igraph.match.arg(mtype), symmetric = 1, laplacian = 2, 
        stochastic = 3)
    if (!is.null(p)) 
        p <- as.numeric(p)
    norm <- switch(igraph.match.arg(norm), row = 1, col = 2)
    sparse <- as.logical(sparse)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_scg_semiprojectors", groups, mtype, 
        p, norm, sparse, PACKAGE = "igraph")
    if (sparse) {
        res$L <- igraph.i.spMatrix(res$L)
        res$R <- igraph.i.spMatrix(res$R)
    }
    res
}


clusters <- function (graph, mode = c("weak", "strong")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), weak = 1, strong = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_clusters", graph, mode, PACKAGE = "igraph")
    res$membership <- res$membership + 1
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$membership) <- V(graph)$name
    }
    res
}


cluster_walktrap <- function (graph, weights = E(graph)$weight, steps = 4, merges = TRUE, 
    modularity = TRUE, membership = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object!")
    }
    if (membership && !modularity) {
        modularity <- TRUE
    }
    if (!is.null(weights)) {
        weights <- as.numeric(weights)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_walktrap_community", graph, weights, 
        as.numeric(steps), as.logical(merges), as.logical(modularity), 
        as.logical(membership), PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "walktrap"
    res$membership <- res$membership + 1
    res$merges <- res$merges + 1
    class(res) <- "communities"
    res
}


sample_last_cit <- function (n, edges = 1, agebins = n/7100, pref = (1:(agebins + 
    1))^-3, directed = TRUE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_lastcit_game", as.numeric(n), as.numeric(edges), 
        as.numeric(agebins), as.numeric(pref), as.logical(directed), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Random citation graph based on last citation"
        res$edges <- edges
        res$agebins <- agebins
    }
    res
}


graph.neighborhood <- function (graph, order, nodes = V(graph), mode = c("all", "out", 
    "in"), mindist = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    mindist <- as.integer(mindist)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_neighborhood_graphs", graph, as.igraph.vs(graph, 
        nodes) - 1, as.numeric(order), as.numeric(mode), mindist, 
        PACKAGE = "igraph")
    res
}


sample_growing <- function (n, m = 1, directed = TRUE, citation = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_growing_random_game", as.numeric(n), 
        as.numeric(m), as.logical(directed), as.logical(citation), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Growing random graph"
        res$m <- m
        res$citation <- citation
    }
    res
}


igraph_options <- function (...) 
{
    if (nargs() == 0) 
        return(.igraph.pars)
    current <- .igraph.pars
    temp <- list(...)
    if (length(temp) == 1 && is.null(names(temp))) {
        arg <- temp[[1]]
        switch(mode(arg), list = temp <- arg, character = return(.igraph.pars[arg]), 
            stop("invalid argument: ", sQuote(arg)))
    }
    if (length(temp) == 0) 
        return(current)
    n <- names(temp)
    if (is.null(n)) 
        stop("options must be given by name")
    env <- asNamespace("igraph")
    cb <- intersect(names(igraph.pars.callbacks), n)
    for (cn in cb) {
        temp[[cn]] <- igraph.pars.callbacks[[cn]](temp[[cn]])
    }
    current <- .igraph.pars
    current[n] <- temp
    assign(".igraph.pars", current, envir = env)
    invisible(current)
}


evcent <- function (graph, directed = FALSE, scale = TRUE, weights = NULL, 
    options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    scale <- as.logical(scale)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_eigenvector_centrality", graph, directed, 
        scale, weights, options, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$vector) <- vertex_attr(graph, "name", )
    }
    res
}


centr_eigen <- function (graph, directed = FALSE, scale = TRUE, options = arpack_defaults, 
    normalized = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    scale <- as.logical(scale)
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_eigenvector_centrality", 
        graph, directed, scale, options, normalized, PACKAGE = "igraph")
    res
}


with_mds <- function (...) 
layout_spec(layout_with_mds, ...)


stochastic_matrix <- function (graph, column.wise = FALSE, sparse = igraph_opt("sparsematrices")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    column.wise <- as.logical(column.wise)
    if (length(column.wise) != 1) {
        stop("`column.wise' must be a logical scalar")
    }
    sparse <- as.logical(sparse)
    if (length(sparse) != 1) {
        stop("`sparse' must be a logical scalar")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (sparse) {
        res <- .Call("R_igraph_get_stochastic_sparsemat", graph, 
            column.wise, PACKAGE = "igraph")
        res <- igraph.i.spMatrix(res)
    }
    else {
        res <- .Call("R_igraph_get_stochastic", graph, column.wise, 
            PACKAGE = "igraph")
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        rownames(res) <- colnames(res) <- V(graph)$name
    }
    res
}


graph.empty <- function (n = 0, directed = TRUE) 
{
    n <- as.integer(n)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_empty", n, directed, PACKAGE = "igraph")
    res
}


merges <- function (communities) 
{
    if (!is.null(communities$merges)) {
        communities$merges
    }
    else {
        stop("Not a hierarchical community structure")
    }
}


min_cut <- function (graph, source = NULL, target = NULL, capacity = NULL, 
    value.only = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(capacity)) {
        if ("capacity" %in% edge_attr_names(graph)) {
            capacity <- E(graph)$capacity
        }
    }
    if (is.null(source) && !is.null(target) || is.null(target) && 
        !is.null(source)) {
        stop("Please give both source and target or neither")
    }
    if (!is.null(capacity)) {
        capacity <- as.numeric(capacity)
    }
    value.only <- as.logical(value.only)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (is.null(target) && is.null(source)) {
        if (value.only) {
            res <- .Call("R_igraph_mincut_value", graph, capacity, 
                PACKAGE = "igraph")
        }
        else {
            res <- .Call("R_igraph_mincut", graph, capacity, 
                PACKAGE = "igraph")
            res$cut <- res$cut + 1
            res$partition1 <- res$partition1 + 1
            res$partition2 <- res$partition2 + 1
            if (igraph_opt("return.vs.es")) {
                res$cut <- create_es(graph, res$cut)
                res$partition1 <- create_vs(graph, res$partition1)
                res$partition2 <- create_vs(graph, res$partition2)
            }
            res
        }
    }
    else {
        if (value.only) {
            res <- .Call("R_igraph_st_mincut_value", graph, as.igraph.vs(graph, 
                source) - 1, as.igraph.vs(graph, target) - 1, 
                capacity, PACKAGE = "igraph")
        }
        else {
            stop("Calculating minimum s-t cuts is not implemented yet")
        }
    }
    res
}


make_bipartite_graph <- function (types, edges, directed = FALSE) 
{
    types <- as.logical(types)
    edges <- as.numeric(edges) - 1
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_create_bipartite", types, edges, directed, 
        PACKAGE = "igraph")
    set_vertex_attr(res, "type", value = types)
}


spinglass.community <- function (graph, weights = NULL, vertex = NULL, spins = 25, parupdate = FALSE, 
    start.temp = 1, stop.temp = 0.01, cool.fact = 0.99, update.rule = c("config", 
        "random", "simple"), gamma = 1, implementation = c("orig", 
        "neg"), gamma.minus = 1) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    update.rule <- igraph.match.arg(update.rule)
    update.rule <- switch(update.rule, simple = 0, random = 0, 
        config = 1)
    implementation <- switch(igraph.match.arg(implementation), 
        orig = 0, neg = 1)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (is.null(vertex)) {
        res <- .Call("R_igraph_spinglass_community", graph, weights, 
            as.numeric(spins), as.logical(parupdate), as.numeric(start.temp), 
            as.numeric(stop.temp), as.numeric(cool.fact), as.numeric(update.rule), 
            as.numeric(gamma), as.numeric(implementation), as.numeric(gamma.minus), 
            PACKAGE = "igraph")
        res$algorithm <- "spinglass"
        res$vcount <- vcount(graph)
        res$membership <- res$membership + 1
        if (igraph_opt("add.vertex.names") && is_named(graph)) {
            res$names <- vertex_attr(graph, "name")
        }
        class(res) <- "communities"
    }
    else {
        res <- .Call("R_igraph_spinglass_my_community", graph, 
            weights, as.igraph.vs(graph, vertex) - 1, as.numeric(spins), 
            as.numeric(update.rule), as.numeric(gamma), PACKAGE = "igraph")
        res$community <- res$community + 1
    }
    res
}


crossing <- function (communities, graph) 
{
    m <- membership(communities)
    el <- as_edgelist(graph, names = FALSE)
    m1 <- m[el[, 1]]
    m2 <- m[el[, 2]]
    res <- m1 != m2
    if (!is.null(names(m1))) {
        names(res) <- paste(names(m1), names(m2), sep = "|")
    }
    res
}


is_directed <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_is_directed", graph, PACKAGE = "igraph")
}


bipartite.projection <- function (graph, types = NULL, multiplicity = TRUE, probe1 = NULL, 
    which = c("both", "true", "false"), remove.type = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        if (!is.logical(types)) {
            warning("vertex types converted to logical")
        }
        types <- as.logical(types)
        if (any(is.na(types))) {
            stop("`NA' is not allowed in vertex types")
        }
    }
    else {
        stop("Not a bipartite graph, supply `types' argument")
    }
    if (!is.null(probe1)) {
        probe1 <- as.igraph.vs(graph, probe1) - 1
    }
    else {
        probe1 <- -1
    }
    which <- switch(igraph.match.arg(which), both = 0L, false = 1L, 
        true = 2L)
    if (which != "both" && probe1 != -1) {
        warning("`probe1' ignored if only one projection is requested")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_bipartite_projection", graph, types, 
        as.integer(probe1), which, PACKAGE = "igraph")
    if (remove.type) {
        if (is_igraph(res[[1]])) {
            res[[1]] <- delete_vertex_attr(res[[1]], "type")
        }
        if (is_igraph(res[[2]])) {
            res[[2]] <- delete_vertex_attr(res[[2]], "type")
        }
    }
    if (which == 0L) {
        if (multiplicity) {
            E(res[[1]])$weight <- res[[3]]
            E(res[[2]])$weight <- res[[4]]
        }
        res[1:2]
    }
    else if (which == 1L) {
        if (multiplicity) {
            E(res[[1]])$weight <- res[[3]]
        }
        res[[1]]
    }
    else {
        if (multiplicity) {
            E(res[[2]])$weight <- res[[4]]
        }
        res[[2]]
    }
}


as_star <- function (...) 
layout_spec(layout_as_star, ...)


nexus_list <- function (tags = NULL, offset = 0, limit = 10, operator = c("or", 
    "and"), order = c("date", "name", "popularity"), nexus.url = igraph_opt("nexus.url")) 
{
    operator = igraph.match.arg(operator)
    order = igraph.match.arg(order)
    if (is.null(tags)) {
        u <- paste(sep = "", nexus.url, "/api/dataset_info?format=text", 
            "&offset=", offset, "&limit=", limit, "&order=", 
            order)
        name <- "data set list"
    }
    else {
        tags <- paste(tags, collapse = "|")
        u <- paste(sep = "", nexus.url, "/api/dataset_info?tag=", 
            tags, "&operator=", operator, "&format=text", "&offset=", 
            offset, "&limit=", limit, "&order=", order)
        name <- paste("tags:", gsub("|", "; ", tags, fixed = TRUE))
    }
    f <- url(URLencode(u))
    l <- readLines(f)
    close(f)
    nexus.format.result(l, name)
}


similarity.invlogweighted <- function (graph, vids = V(graph), mode = c("all", "out", "in", 
    "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_similarity_inverse_log_weighted", 
        graph, vids - 1, mode, PACKAGE = "igraph")
    res
}


pref <- function (...) 
constructor_spec(sample_pref, ...)


min_st_separators <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_all_minimal_st_separators", graph, 
        PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res)) {
            res[[i_]] <- create_vs(graph, res[[i_]])
        }
    }
    res
}


star <- function (...) 
constructor_spec(make_star, ...)


vertex.disjoint.paths <- function (graph, source = NULL, target = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_vertex_disjoint_paths", graph, as.igraph.vs(graph, 
        source) - 1, as.igraph.vs(graph, target) - 1, PACKAGE = "igraph")
}


vertex.attributes <- function (graph, index = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!missing(index)) {
        index <- as.igraph.vs(graph, index)
    }
    res <- base::.Call("R_igraph_mybracket2_copy", graph, 9L, 
        3L, PACKAGE = "igraph")
    if (!missing(index) && (length(index) != vcount(graph) || 
        any(index != V(graph)))) {
        for (i in seq_along(value)) {
            value[[i]] <- value[[i]][index]
        }
    }
    res
}


graph_attr_names <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    res <- base::.Call("R_igraph_mybracket2_names", graph, 9L, 
        2L, PACKAGE = "igraph")
    if (is.null(res)) {
        res <- character()
    }
    res
}


graph.isomorphic <- function (graph1, graph2) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_isomorphic", graph1, graph2, PACKAGE = "igraph")
    res
}


centralization.evcent <- function (graph, directed = FALSE, scale = TRUE, options = arpack_defaults, 
    normalized = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    directed <- as.logical(directed)
    scale <- as.logical(scale)
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_eigenvector_centrality", 
        graph, directed, scale, options, normalized, PACKAGE = "igraph")
    res
}


layout_with_lgl <- function (graph, maxiter = 150, maxdelta = vcount(graph), area = vcount(graph)^2, 
    coolexp = 1.5, repulserad = area * vcount(graph), cellsize = sqrt(sqrt(area)), 
    root = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(root)) {
        root <- -1
    }
    else {
        root <- as.igraph.vs(graph, root) - 1
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_layout_lgl", graph, as.double(maxiter), as.double(maxdelta), 
        as.double(area), as.double(coolexp), as.double(repulserad), 
        as.double(cellsize), root, PACKAGE = "igraph")
}


is.loop <- function (graph, eids = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_is_loop", graph, as.igraph.es(graph, eids) - 
        1, PACKAGE = "igraph")
}


radius <- function (graph, mode = c("all", "out", "in", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_radius", graph, mode, PACKAGE = "igraph")
    res
}


with_dh <- function (...) 
layout_spec(layout_with_dh, ...)


graph.strength <- function (graph, vids = V(graph), mode = c("all", "out", "in", 
    "total"), loops = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    loops <- as.logical(loops)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_strength", graph, vids - 1, mode, 
        loops, weights, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


any_multiple <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_has_multiple", graph, PACKAGE = "igraph")
    res
}


alpha.centrality <- function (graph, nodes = V(graph), alpha = 1, loops = FALSE, 
    exo = 1, weights = NULL, tol = 1e-07, sparse = TRUE) 
{
    nodes <- as.igraph.vs(graph, nodes)
    if (sparse) {
        res <- alpha.centrality.sparse(graph, nodes, alpha, loops, 
            exo, weights, tol)
    }
    else {
        res <- alpha.centrality.dense(graph, nodes, alpha, loops, 
            exo, weights, tol)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", nodes)
    }
    res
}


tkplot.off <- function () 
{
    eapply(.tkplot.env, function(tkp) {
        tcltk::tkdestroy(tkp$top)
    })
    rm(list = ls(.tkplot.env), envir = .tkplot.env)
    invisible(NULL)
}


with_graph_ <- function (...) 
{
    args <- grab_args()
    constructor_modifier(id = "with_graph_", args = args)
}


layout_with_kk <- function (graph, coords = NULL, dim = 2, maxiter = 50 * vcount(graph), 
    epsilon = 0, kkconst = vcount(graph), weights = NULL, minx = NULL, 
    maxx = NULL, miny = NULL, maxy = NULL, minz = NULL, maxz = NULL, 
    niter, sigma, initemp, coolexp, start) 
{
    if (!missing(coords) && !missing(start)) {
        stop("Both `coords' and `start' are given, give only one of them.")
    }
    if (!missing(start)) 
        coords <- start
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(coords)) {
        coords <- as.matrix(structure(as.double(coords), dim = dim(coords)))
    }
    dim <- as.integer(dim)
    if (dim != 2L && dim != 3L) {
        stop("Dimension must be two or three")
    }
    maxiter <- as.integer(maxiter)
    epsilon <- as.numeric(epsilon)
    kkconst <- as.numeric(kkconst)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (!is.null(minx)) 
        minx <- as.numeric(minx)
    if (!is.null(maxx)) 
        maxx <- as.numeric(maxx)
    if (!is.null(miny)) 
        miny <- as.numeric(miny)
    if (!is.null(maxy)) 
        maxy <- as.numeric(maxy)
    if (!is.null(minz)) 
        minz <- as.numeric(minz)
    if (!is.null(maxz)) 
        maxz <- as.numeric(maxz)
    if (!missing(niter)) {
        warning("Argument `niter' is deprecated and has no effect")
    }
    if (!missing(sigma)) {
        warning("Argument `sigma' is deprecated and has no effect")
    }
    if (!missing(initemp)) {
        warning("Argument `initemp' is deprecated and has no effect")
    }
    if (!missing(coolexp)) {
        warning("Argument `coolexp' is deprecated and has no effect")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (dim == 2) {
        res <- .Call("R_igraph_layout_kamada_kawai", graph, coords, 
            maxiter, epsilon, kkconst, weights, minx, maxx, miny, 
            maxy, PACKAGE = "igraph")
    }
    else {
        res <- .Call("R_igraph_layout_kamada_kawai_3d", graph, 
            coords, maxiter, epsilon, kkconst, weights, minx, 
            maxx, miny, maxy, minz, maxz, PACKAGE = "igraph")
    }
    res
}


cluster_infomap <- function (graph, e.weights = NULL, v.weights = NULL, nb.trials = 10, 
    modularity = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(e.weights) && "weight" %in% edge_attr_names(graph)) {
        e.weights <- E(graph)$weight
    }
    if (!is.null(e.weights) && any(!is.na(e.weights))) {
        e.weights <- as.numeric(e.weights)
    }
    else {
        e.weights <- NULL
    }
    if (is.null(v.weights) && "weight" %in% vertex_attr_names(graph)) {
        v.weights <- V(graph)$weight
    }
    if (!is.null(v.weights) && any(!is.na(v.weights))) {
        v.weights <- as.numeric(v.weights)
    }
    else {
        v.weights <- NULL
    }
    nb.trials <- as.integer(nb.trials)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_infomap", graph, e.weights, 
        v.weights, nb.trials, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "infomap"
    res$membership <- res$membership + 1
    if (modularity) {
        res$modularity <- modularity(graph, res$membership, weights = e.weights)
    }
    class(res) <- "communities"
    res
}


maximum.cardinality.search <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maximum_cardinality_search", graph, 
        PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res$alpha <- create_vs(graph, res$alpha)
    }
    res
}


layout.fruchterman.reingold <- function (..., params = list()) 
{
    do_call(layout_with_fr, .args = c(list(...), params))
}


spectrum <- function (graph, algorithm = c("arpack", "auto", "lapack", "comp_auto", 
    "comp_lapack", "comp_arpack"), which = list(), options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    algorithm <- switch(igraph.match.arg(algorithm), auto = 0, 
        lapack = 1, arpack = 2, comp_auto = 3, comp_lapack = 4, 
        comp_arpack = 5)
    which.tmp <- eigen_defaults
    which.tmp[names(which)] <- which
    which <- which.tmp
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_eigen_adjacency", graph, algorithm, 
        which, options, PACKAGE = "igraph")
    res
}


from_data_frame <- function (...) 
constructor_spec(graph_from_data_frame, ...)


degree.distribution <- function (graph, cumulative = FALSE, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cs <- degree(graph, ...)
    hi <- hist(cs, -1:max(cs), plot = FALSE)$density
    if (!cumulative) {
        res <- hi
    }
    else {
        res <- rev(cumsum(rev(hi)))
    }
    res
}


graph_version <- function (graph) 
{
    if (missing(graph)) {
        "0.8.0"
    }
    else {
        stopifnot(is_igraph(graph))
        .Call("R_igraph_graph_version", graph, PACKAGE = "igraph")
    }
}


dfs <- function (graph, root, neimode = c("out", "in", "all", "total"), 
    unreachable = TRUE, order = TRUE, order.out = FALSE, father = FALSE, 
    dist = FALSE, in.callback = NULL, out.callback = NULL, extra = NULL, 
    rho = parent.frame()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    root <- as.igraph.vs(graph, root) - 1
    neimode <- switch(igraph.match.arg(neimode), out = 1, `in` = 2, 
        all = 3, total = 3)
    unreachable <- as.logical(unreachable)
    if (!is.null(in.callback)) {
        in.callback <- as.function(in.callback)
    }
    if (!is.null(out.callback)) {
        out.callback <- as.function(out.callback)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_dfs", graph, root, neimode, unreachable, 
        as.logical(order), as.logical(order.out), as.logical(father), 
        as.logical(dist), in.callback, out.callback, extra, rho, 
        PACKAGE = "igraph")
    if (order) 
        res$order <- res$order + 1
    if (order.out) 
        res$order.out <- res$order.out + 1
    if (father) 
        res$father <- res$father + 1
    if (igraph_opt("return.vs.es")) {
        if (order) 
            res$order <- V(graph)[res$order, na_ok = TRUE]
        if (order.out) 
            res$order.out <- V(graph)[res$order.out, na_ok = TRUE]
        if (father) 
            res$father <- create_vs(graph, res$father, na_ok = TRUE)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        if (father) 
            names(res$father) <- V(graph)$name
        if (dist) 
            names(res$dist) <- V(graph)$name
    }
    res
}


set_edge_attr <- function (graph, name, index = E(graph), value) 
{
    i_set_edge_attr(graph = graph, name = name, index = index, 
        value = value)
}


permute.vertices <- function (graph, permutation) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    permutation <- as.numeric(permutation) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_permute_vertices", graph, permutation, 
        PACKAGE = "igraph")
    res
}


st_min_cuts <- function (graph, source, target, capacity = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    source <- as.igraph.vs(graph, source)
    target <- as.igraph.vs(graph, target)
    if (is.null(capacity) && "weight" %in% edge_attr_names(graph)) {
        capacity <- E(graph)$weight
    }
    if (!is.null(capacity) && any(!is.na(capacity))) {
        capacity <- as.numeric(capacity)
    }
    else {
        capacity <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_all_st_mincuts", graph, source - 1, 
        target - 1, capacity, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$cuts)) {
            res$cuts[[i_]] <- create_es(graph, res$cuts[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$partition1s)) {
            res$partition1s[[i_]] <- create_vs(graph, res$partition1s[[i_]])
        }
    }
    res
}


centralization.closeness.tmax <- function (graph = NULL, nodes = 0, mode = c("out", "in", "all", 
    "total")) 
{
    if (!is.null(graph) && !is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.integer(nodes)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_closeness_tmax", graph, 
        nodes, mode, PACKAGE = "igraph")
    res
}


traits_callaway <- function (...) 
constructor_spec(sample_traits_callaway, ...)


tk_reshape <- function (tkp.id, newlayout, ..., params) 
{
    tkp <- .tkplot.get(tkp.id)
    new_coords <- do_call(newlayout, .args = c(list(tkp$graph), 
        list(...), params))
    .tkplot.set(tkp.id, "coords", new_coords)
    tk_fit(tkp.id)
    .tkplot.update.vertices(tkp.id)
    invisible(NULL)
}


add_shape <- function (shape, clip = shape_noclip, plot = shape_noplot, parameters = list()) 
{
    assign(shape, value = list(clip = clip, plot = plot), envir = .igraph.shapes)
    do.call(igraph.options, parameters)
    invisible(TRUE)
}


vertex_disjoint_paths <- function (graph, source = NULL, target = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_vertex_disjoint_paths", graph, as.igraph.vs(graph, 
        source) - 1, as.igraph.vs(graph, target) - 1, PACKAGE = "igraph")
}


estimate_edge_betweenness <- function (graph, e = E(graph), directed = TRUE, cutoff, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    e <- as.igraph.es(graph, e)
    directed <- as.logical(directed)
    cutoff <- as.numeric(cutoff)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_edge_betweenness_estimate", graph, 
        directed, cutoff, weights, PACKAGE = "igraph")
    res[as.numeric(e)]
}


page_rank <- function (graph, algo = c("prpack", "arpack", "power"), vids = V(graph), 
    directed = TRUE, damping = 0.85, personalized = NULL, weights = NULL, 
    options = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    algo <- switch(igraph.match.arg(algo), power = 0L, arpack = 1L, 
        prpack = 2L)
    vids <- as.igraph.vs(graph, vids)
    directed <- as.logical(directed)
    damping <- as.numeric(damping)
    if (!is.null(personalized)) 
        personalized <- as.numeric(personalized)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (is.null(options)) {
        if (algo == 0L) {
            options <- list(niter = 1000, eps = 0.001)
        }
        else if (algo == 1L) {
            options <- arpack_defaults
        }
        else {
            options <- NULL
        }
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_personalized_pagerank", graph, algo, 
        vids - 1, directed, damping, personalized, weights, options, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$vector) <- vertex_attr(graph, "name", vids)
    }
    res
}


identical_graphs <- function (g1, g2) 
{
    stopifnot(is_igraph(g1), is_igraph(g2))
    base::.Call("R_igraph_identical_graphs", g1, g2, PACKAGE = "igraph")
}


graph_ <- function (...) 
{
    me <- attr(sys.function(), "name") %||% "construct"
    args <- list(...)
    cidx <- vapply(args, inherits, TRUE, what = "igraph_constructor_spec")
    if (sum(cidx) == 0) {
        stop("Don't know how to ", me, ", nothing given")
    }
    if (sum(cidx) > 1) {
        stop("Don't know how to ", me, ", multiple constructors given")
    }
    cons <- args[cidx][[1]]
    args <- args[!cidx]
    wmods <- vapply(args, class, "") == "igraph_constructor_modifier"
    mods <- args[wmods]
    args <- args[!wmods]
    args2 <- if (cons$lazy) 
        lapply(cons$args, "[[", "expr")
    else lazy_eval(cons$args)
    res <- do_call(cons$fun, args2, args)
    for (m in mods) {
        if (m$id == "without_attr") {
            ga <- graph_attr_names(res)
            va <- vertex_attr_names(res)
            ea <- edge_attr_names(res)
            for (g in ga) res <- delete_graph_attr(res, g)
            for (v in va) res <- delete_vertex_attr(res, v)
            for (e in ea) res <- delete_edge_attr(res, e)
        }
        else if (m$id == "without_loops") {
            res <- simplify(res, remove.loops = TRUE, remove.multiple = FALSE)
        }
        else if (m$id == "without_multiples") {
            res <- simplify(res, remove.loops = FALSE, remove.multiple = TRUE)
        }
        else if (m$id == "simplified") {
            res <- simplify(res)
        }
        else if (m$id == "with_vertex_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_vertex_attr(res, n, value = v)
            }
        }
        else if (m$id == "with_edge_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_edge_attr(res, n, value = v)
            }
        }
        else if (m$id == "with_graph_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_graph_attr(res, n, value = v)
            }
        }
    }
    res
}


scgSemiProjectors <- function (groups, mtype = c("symmetric", "laplacian", "stochastic"), 
    p = NULL, norm = c("row", "col"), sparse = igraph_opt("sparsematrices")) 
{
    groups <- as.numeric(groups) - 1
    mtype <- switch(igraph.match.arg(mtype), symmetric = 1, laplacian = 2, 
        stochastic = 3)
    if (!is.null(p)) 
        p <- as.numeric(p)
    norm <- switch(igraph.match.arg(norm), row = 1, col = 2)
    sparse <- as.logical(sparse)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_scg_semiprojectors", groups, mtype, 
        p, norm, sparse, PACKAGE = "igraph")
    if (sparse) {
        res$L <- igraph.i.spMatrix(res$L)
        res$R <- igraph.i.spMatrix(res$R)
    }
    res
}


make_full_graph <- function (n, directed = FALSE, loops = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_full", as.numeric(n), as.logical(directed), 
        as.logical(loops), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Full graph"
        res$loops <- loops
    }
    res
}


girth <- function (graph, circle = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_girth", graph, as.logical(circle), 
        PACKAGE = "igraph")
    if (igraph_opt("return.vs.es") && circle) {
        res$circle <- create_vs(graph, res$circle)
    }
    res
}


as_tree <- function (...) 
layout_spec(layout_as_tree, ...)


reciprocity <- function (graph, ignore.loops = TRUE, mode = c("default", "ratio")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), default = 0, ratio = 1)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_reciprocity", graph, as.logical(ignore.loops), 
        as.numeric(mode), PACKAGE = "igraph")
}


cocitation <- function (graph, v = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    v <- as.igraph.vs(graph, v)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_cocitation", graph, v - 1, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        rownames(res) <- vertex_attr(graph, "name", v)
        colnames(res) <- vertex_attr(graph, "name")
    }
    res
}


tkplot.center <- function (tkp.id) 
{
    tkp <- .tkplot.get(tkp.id)
    width <- as.numeric(tcltk::tkwinfo("width", tkp$canvas))
    height <- as.numeric(tcltk::tkwinfo("height", tkp$canvas))
    coords <- .tkplot.get(tkp.id, "coords")
    canvas.center.x <- width/2
    canvas.center.y <- height/2
    coords <- .tkplot.get(tkp.id, "coords")
    r1 <- range(coords[, 1])
    r2 <- range(coords[, 2])
    coords.center.x <- (r1[1] + r1[2])/2
    coords.center.y <- (r2[1] + r2[2])/2
    coords[, 1] <- coords[, 1] + canvas.center.x - coords.center.x
    coords[, 2] <- coords[, 2] + canvas.center.y - coords.center.y
    .tkplot.set(tkp.id, "coords", coords)
    .tkplot.update.vertices(tkp.id)
    invisible(NULL)
}


get.data.frame <- function (x, what = c("edges", "vertices", "both")) 
{
    if (!is_igraph(x)) {
        stop("Not a graph object")
    }
    what <- igraph.match.arg(what)
    if (what %in% c("vertices", "both")) {
        ver <- .Call("R_igraph_mybracket2", x, 9L, 3L, PACKAGE = "igraph")
        class(ver) <- "data.frame"
        rn <- if (is_named(x)) {
            V(x)$name
        }
        else {
            seq_len(vcount(x))
        }
        rownames(ver) <- rn
    }
    if (what %in% c("edges", "both")) {
        el <- as_edgelist(x)
        edg <- c(list(from = el[, 1]), list(to = el[, 2]), .Call("R_igraph_mybracket2", 
            x, 9L, 4L, PACKAGE = "igraph"))
        class(edg) <- "data.frame"
        rownames(edg) <- seq_len(ecount(x))
    }
    if (what == "both") {
        list(vertices = ver, edges = edg)
    }
    else if (what == "vertices") {
        ver
    }
    else {
        edg
    }
}


difference <- function (...) 
UseMethod("difference")


groups <- function (x) 
UseMethod("groups")


graph.dfs <- function (graph, root, neimode = c("out", "in", "all", "total"), 
    unreachable = TRUE, order = TRUE, order.out = FALSE, father = FALSE, 
    dist = FALSE, in.callback = NULL, out.callback = NULL, extra = NULL, 
    rho = parent.frame()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    root <- as.igraph.vs(graph, root) - 1
    neimode <- switch(igraph.match.arg(neimode), out = 1, `in` = 2, 
        all = 3, total = 3)
    unreachable <- as.logical(unreachable)
    if (!is.null(in.callback)) {
        in.callback <- as.function(in.callback)
    }
    if (!is.null(out.callback)) {
        out.callback <- as.function(out.callback)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_dfs", graph, root, neimode, unreachable, 
        as.logical(order), as.logical(order.out), as.logical(father), 
        as.logical(dist), in.callback, out.callback, extra, rho, 
        PACKAGE = "igraph")
    if (order) 
        res$order <- res$order + 1
    if (order.out) 
        res$order.out <- res$order.out + 1
    if (father) 
        res$father <- res$father + 1
    if (igraph_opt("return.vs.es")) {
        if (order) 
            res$order <- V(graph)[res$order, na_ok = TRUE]
        if (order.out) 
            res$order.out <- V(graph)[res$order.out, na_ok = TRUE]
        if (father) 
            res$father <- create_vs(graph, res$father, na_ok = TRUE)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        if (father) 
            names(res$father) <- V(graph)$name
        if (dist) 
            names(res$dist) <- V(graph)$name
    }
    res
}


is_max_matching <- function (graph, matching, types = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        types <- as.logical(types)
    }
    matching <- as.igraph.vs(graph, matching, na.ok = TRUE) - 
        1
    matching[is.na(matching)] <- -1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_maximal_matching", graph, types, 
        matching, PACKAGE = "igraph")
    res
}


vertex <- function (...) 
{
    structure(list(...), class = "igraph.vertex")
}


graph_from_adj_list <- function (adjlist, mode = c("out", "in", "all", "total"), duplicate = TRUE) 
{
    adjlist <- lapply(adjlist, function(x) as.integer(x) - 1L)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    duplicate <- as.logical(duplicate)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_adjlist", adjlist, mode, duplicate, 
        PACKAGE = "igraph")
    res
}


graph.motifs.est <- function (graph, size = 3, cut.prob = rep(0, size), sample.size = vcount(graph)/10, 
    sample = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cut.prob <- as.numeric(cut.prob)
    if (length(cut.prob) != size) {
        cut.prob <- c(cut.prob[-length(cut.prob)], rep(cut.prob[-length(cut.prob)], 
            length(cut.prob) - 1))
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_motifs_randesu_estimate", graph, as.integer(size), 
        as.numeric(cut.prob), as.integer(sample.size), as.numeric(sample), 
        PACKAGE = "igraph")
}


centralize <- function (scores, theoretical.max = 0, normalized = TRUE) 
{
    scores <- as.numeric(scores)
    theoretical.max <- as.numeric(theoretical.max)
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization", scores, theoretical.max, 
        normalized, PACKAGE = "igraph")
    res
}


canonical.permutation <- function (graph, sh = "fm") 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    sh <- switch(igraph.match.arg(sh), f = 0, fl = 1, fs = 2, 
        fm = 3, flm = 4, fsm = 5)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_canonical_permutation", graph, sh, 
        PACKAGE = "igraph")
    res
}


degree_distribution <- function (graph, cumulative = FALSE, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cs <- degree(graph, ...)
    hi <- hist(cs, -1:max(cs), plot = FALSE)$density
    if (!cumulative) {
        res <- hi
    }
    else {
        res <- rev(cumsum(rev(hi)))
    }
    res
}


sample_hrg <- function (hrg) 
{
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_game", hrg, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Hierarchical random graph model")
    res
}


coreness <- function (graph, mode = c("all", "out", "in")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_coreness", graph, as.numeric(mode), 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name")
    }
    res
}


growing <- function (...) 
constructor_spec(sample_growing, ...)


articulation_points <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_articulation_points", graph, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res <- create_vs(graph, res)
    }
    res
}


igraph.arpack.default <- structure(list(bmat = "I", n = 0, which = "XX", nev = 1, tol = 0, 
    ncv = 3, ldv = 0, ishift = 1, maxiter = 1000, nb = 1, mode = 1, 
    start = 0, sigma = 0, sigmai = 0), .Names = c("bmat", "n", 
"which", "nev", "tol", "ncv", "ldv", "ishift", "maxiter", "nb", 
"mode", "start", "sigma", "sigmai"))


graph.coreness <- function (graph, mode = c("all", "out", "in")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_coreness", graph, as.numeric(mode), 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name")
    }
    res
}


arpack <- function (func, extra = NULL, sym = FALSE, options = arpack_defaults, 
    env = parent.frame(), complex = !sym) 
{
    if (!is.list(options) || (is.null(names(options)) && length(options) != 
        0)) {
        stop("options must be a named list")
    }
    if (any(names(options) == "")) {
        stop("all options must be named")
    }
    if (any(!names(options) %in% names(arpack_defaults))) {
        stop("unkown ARPACK option(s): ", paste(setdiff(names(options), 
            names(arpack_defaults)), collapse = ", "))
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    if (sym && complex) {
        complex <- FALSE
        warning("Symmetric matrix, setting `complex' to FALSE")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_arpack", func, extra, options, env, 
        sym, PACKAGE = "igraph")
    if (complex) {
        rew <- arpack.unpack.complex(res$vectors, res$values, 
            min(res$options$nev, res$options$nconv))
        res$vectors <- rew$vectors
        res$values <- rew$values
        res$values <- apply(res$values, 1, function(x) x[1] + 
            x[2] * (0+1i))
        dim(res$vectors) <- c(nrow(res$vectors) * 2, ncol(res$vectors)/2)
        res$vectors <- apply(res$vectors, 2, function(x) {
            l <- length(x)/2
            x[1:l] + x[(l + 1):length(x)] * (0+1i)
        })
    }
    else {
        if (is.matrix(res$values)) {
            if (!all(res$values[, 2] == 0)) {
                warning("Dropping imaginary parts of eigenvalues")
            }
            res$values <- res$values[, 1]
        }
        res$vectors <- res$vectors[, 1:length(res$values)]
    }
    res
}


layout.grid <- function (graph, width = 0, height = 0, dim = 2) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    width <- as.integer(width)
    dim <- as.integer(dim)
    stopifnot(dim == 2 || dim == 3)
    if (dim == 3) {
        height <- as.integer(height)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (dim == 2) {
        res <- .Call("R_igraph_layout_grid", graph, width, PACKAGE = "igraph")
    }
    else {
        res <- .Call("R_igraph_layout_grid_3d", graph, width, 
            height, PACKAGE = "igraph")
    }
    res
}


sample_correlated_gnp <- function (old.graph, corr, p = old.graph$p, permutation = NULL) 
{
    if (!is_igraph(old.graph)) {
        stop("Not a graph object")
    }
    corr <- as.numeric(corr)
    p <- as.numeric(p)
    if (!is.null(permutation)) 
        permutation <- as.numeric(permutation) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_correlated_game", old.graph, corr, 
        p, permutation, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Correlated random graph")
    res <- set.graph.attribute(res, "corr", corr)
    res <- set.graph.attribute(res, "p", p)
    res
}


minimal.st.separators <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_all_minimal_st_separators", graph, 
        PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res)) {
            res[[i_]] <- create_vs(graph, res[[i_]])
        }
    }
    res
}


scg_eps <- function (V, groups, mtype = c("symmetric", "laplacian", "stochastic"), 
    p = NULL, norm = c("row", "col")) 
{
    V <- as.matrix(structure(as.double(V), dim = dim(V)))
    groups <- as.numeric(groups) - 1
    mtype <- switch(igraph.match.arg(mtype), symmetric = 1, laplacian = 2, 
        stochastic = 3)
    if (!is.null(p)) 
        p <- as.numeric(p)
    norm <- switch(igraph.match.arg(norm), row = 1, col = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_scg_norm_eps", V, groups, mtype, p, 
        norm, PACKAGE = "igraph")
    res
}


minimum.size.separators <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_minimum_size_separators", graph, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res)) {
            res[[i_]] <- create_vs(graph, res[[i_]])
        }
    }
    res
}


plot.igraph <- function (x, axes = FALSE, add = FALSE, xlim = c(-1, 1), ylim = c(-1, 
    1), mark.groups = list(), mark.shape = 1/2, mark.col = rainbow(length(mark.groups), 
    alpha = 0.3), mark.border = rainbow(length(mark.groups), 
    alpha = 1), mark.expand = 15, ...) 
{
    graph <- x
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    params <- i.parse.plot.params(graph, list(...))
    vertex.size <- 1/200 * params("vertex", "size")
    label.family <- params("vertex", "label.family")
    label.font <- params("vertex", "label.font")
    label.cex <- params("vertex", "label.cex")
    label.degree <- params("vertex", "label.degree")
    label.color <- params("vertex", "label.color")
    label.dist <- params("vertex", "label.dist")
    labels <- params("vertex", "label")
    shape <- igraph.check.shapes(params("vertex", "shape"))
    edge.color <- params("edge", "color")
    edge.width <- params("edge", "width")
    edge.lty <- params("edge", "lty")
    arrow.mode <- params("edge", "arrow.mode")
    edge.labels <- params("edge", "label")
    loop.angle <- params("edge", "loop.angle")
    edge.label.font <- params("edge", "label.font")
    edge.label.family <- params("edge", "label.family")
    edge.label.cex <- params("edge", "label.cex")
    edge.label.color <- params("edge", "label.color")
    elab.x <- params("edge", "label.x")
    elab.y <- params("edge", "label.y")
    arrow.size <- params("edge", "arrow.size")[1]
    arrow.width <- params("edge", "arrow.width")[1]
    curved <- params("edge", "curved")
    if (is.function(curved)) {
        curved <- curved(graph)
    }
    layout <- params("plot", "layout")
    margin <- params("plot", "margin")
    margin <- rep(margin, length = 4)
    rescale <- params("plot", "rescale")
    asp <- params("plot", "asp")
    frame <- params("plot", "frame")
    main <- params("plot", "main")
    sub <- params("plot", "sub")
    xlab <- params("plot", "xlab")
    ylab <- params("plot", "ylab")
    palette <- params("plot", "palette")
    if (!is.null(palette)) {
        old_palette <- palette(palette)
        on.exit(palette(old_palette), add = TRUE)
    }
    arrow.mode <- i.get.arrow.mode(graph, arrow.mode)
    maxv <- max(vertex.size)
    if (rescale) {
        layout <- norm_coords(layout, -1, 1, -1, 1)
        xlim <- c(xlim[1] - margin[2] - maxv, xlim[2] + margin[4] + 
            maxv)
        ylim <- c(ylim[1] - margin[1] - maxv, ylim[2] + margin[3] + 
            maxv)
    }
    if (!add) {
        plot(0, 0, type = "n", xlab = xlab, ylab = ylab, xlim = xlim, 
            ylim = ylim, axes = axes, frame = frame, asp = asp, 
            main = main, sub = sub)
    }
    if (!is.list(mark.groups) && is.numeric(mark.groups)) {
        mark.groups <- list(mark.groups)
    }
    mark.shape <- rep(mark.shape, length = length(mark.groups))
    mark.border <- rep(mark.border, length = length(mark.groups))
    mark.col <- rep(mark.col, length = length(mark.groups))
    mark.expand <- rep(mark.expand, length = length(mark.groups))
    for (g in seq_along(mark.groups)) {
        v <- V(graph)[mark.groups[[g]]]
        if (length(vertex.size) == 1) {
            vs <- vertex.size
        }
        else {
            vs <- rep(vertex.size, length = vcount(graph))[v]
        }
        igraph.polygon(layout[v, , drop = FALSE], vertex.size = vs, 
            expand.by = mark.expand[g]/200, shape = mark.shape[g], 
            col = mark.col[g], border = mark.border[g])
    }
    el <- as_edgelist(graph, names = FALSE)
    loops.e <- which(el[, 1] == el[, 2])
    nonloops.e <- which(el[, 1] != el[, 2])
    loops.v <- el[, 1][loops.e]
    loop.labels <- edge.labels[loops.e]
    loop.labx <- if (is.null(elab.x)) {
        rep(NA, length(loops.e))
    }
    else {
        elab.x[loops.e]
    }
    loop.laby <- if (is.null(elab.y)) {
        rep(NA, length(loops.e))
    }
    else {
        elab.y[loops.e]
    }
    edge.labels <- edge.labels[nonloops.e]
    elab.x <- if (is.null(elab.x)) 
        NULL
    else elab.x[nonloops.e]
    elab.y <- if (is.null(elab.y)) 
        NULL
    else elab.y[nonloops.e]
    el <- el[nonloops.e, , drop = FALSE]
    edge.coords <- matrix(0, nrow = nrow(el), ncol = 4)
    edge.coords[, 1] <- layout[, 1][el[, 1]]
    edge.coords[, 2] <- layout[, 2][el[, 1]]
    edge.coords[, 3] <- layout[, 1][el[, 2]]
    edge.coords[, 4] <- layout[, 2][el[, 2]]
    if (length(unique(shape)) == 1) {
        ec <- .igraph.shapes[[shape[1]]]$clip(edge.coords, el, 
            params = params, end = "both")
    }
    else {
        shape <- rep(shape, length = vcount(graph))
        ec <- edge.coords
        ec[, 1:2] <- t(sapply(seq(length = nrow(el)), function(x) {
            .igraph.shapes[[shape[el[x, 1]]]]$clip(edge.coords[x, 
                , drop = FALSE], el[x, , drop = FALSE], params = params, 
                end = "from")
        }))
        ec[, 3:4] <- t(sapply(seq(length = nrow(el)), function(x) {
            .igraph.shapes[[shape[el[x, 2]]]]$clip(edge.coords[x, 
                , drop = FALSE], el[x, , drop = FALSE], params = params, 
                end = "to")
        }))
    }
    x0 <- ec[, 1]
    y0 <- ec[, 2]
    x1 <- ec[, 3]
    y1 <- ec[, 4]
    if (length(loops.e) > 0) {
        ec <- edge.color
        if (length(ec) > 1) {
            ec <- ec[loops.e]
        }
        point.on.cubic.bezier <- function(cp, t) {
            c <- 3 * (cp[2, ] - cp[1, ])
            b <- 3 * (cp[3, ] - cp[2, ]) - c
            a <- cp[4, ] - cp[1, ] - c - b
            t2 <- t * t
            t3 <- t * t * t
            a * t3 + b * t2 + c * t + cp[1, ]
        }
        compute.bezier <- function(cp, points) {
            dt <- seq(0, 1, by = 1/(points - 1))
            sapply(dt, function(t) point.on.cubic.bezier(cp, 
                t))
        }
        plot.bezier <- function(cp, points, color, width, arr, 
            lty, arrow.size, arr.w) {
            p <- compute.bezier(cp, points)
            polygon(p[1, ], p[2, ], border = color, lwd = width, 
                lty = lty)
            if (arr == 1 || arr == 3) {
                igraph.Arrows(p[1, ncol(p) - 1], p[2, ncol(p) - 
                  1], p[1, ncol(p)], p[2, ncol(p)], sh.col = color, 
                  h.col = color, size = arrow.size, sh.lwd = width, 
                  h.lwd = width, open = FALSE, code = 2, width = arr.w)
            }
            if (arr == 2 || arr == 3) {
                igraph.Arrows(p[1, 2], p[2, 2], p[1, 1], p[2, 
                  1], sh.col = color, h.col = color, size = arrow.size, 
                  sh.lwd = width, h.lwd = width, open = FALSE, 
                  code = 2, width = arr.w)
            }
        }
        loop <- function(x0, y0, cx = x0, cy = y0, color, angle = 0, 
            label = NA, width = 1, arr = 2, lty = 1, arrow.size = arrow.size, 
            arr.w = arr.w, lab.x, lab.y) {
            rad <- angle
            center <- c(cx, cy)
            cp <- matrix(c(x0, y0, x0 + 0.4, y0 + 0.2, x0 + 0.4, 
                y0 - 0.2, x0, y0), ncol = 2, byrow = TRUE)
            phi <- atan2(cp[, 2] - center[2], cp[, 1] - center[1])
            r <- sqrt((cp[, 1] - center[1])^2 + (cp[, 2] - center[2])^2)
            phi <- phi + rad
            cp[, 1] <- cx + r * cos(phi)
            cp[, 2] <- cy + r * sin(phi)
            plot.bezier(cp, 50, color, width, arr = arr, lty = lty, 
                arrow.size = arrow.size, arr.w = arr.w)
            if (is.language(label) || !is.na(label)) {
                lx <- x0 + 0.3
                ly <- y0
                phi <- atan2(ly - center[2], lx - center[1])
                r <- sqrt((lx - center[1])^2 + (ly - center[2])^2)
                phi <- phi + rad
                lx <- cx + r * cos(phi)
                ly <- cy + r * sin(phi)
                if (!is.na(lab.x)) {
                  lx <- lab.x
                }
                if (!is.na(lab.y)) {
                  ly <- lab.y
                }
                text(lx, ly, label, col = edge.label.color, font = edge.label.font, 
                  family = edge.label.family, cex = edge.label.cex)
            }
        }
        ec <- edge.color
        if (length(ec) > 1) {
            ec <- ec[loops.e]
        }
        vs <- vertex.size
        if (length(vertex.size) > 1) {
            vs <- vs[loops.v]
        }
        ew <- edge.width
        if (length(edge.width) > 1) {
            ew <- ew[loops.e]
        }
        la <- loop.angle
        if (length(loop.angle) > 1) {
            la <- la[loops.e]
        }
        lty <- edge.lty
        if (length(edge.lty) > 1) {
            lty <- lty[loops.e]
        }
        arr <- arrow.mode
        if (length(arrow.mode) > 1) {
            arr <- arrow.mode[loops.e]
        }
        asize <- arrow.size
        if (length(arrow.size) > 1) {
            asize <- arrow.size[loops.e]
        }
        xx0 <- layout[loops.v, 1] + cos(la) * vs
        yy0 <- layout[loops.v, 2] - sin(la) * vs
        mapply(loop, xx0, yy0, color = ec, angle = -la, label = loop.labels, 
            lty = lty, width = ew, arr = arr, arrow.size = asize, 
            arr.w = arrow.width, lab.x = loop.labx, lab.y = loop.laby)
    }
    if (length(x0) != 0) {
        if (length(edge.color) > 1) {
            edge.color <- edge.color[nonloops.e]
        }
        if (length(edge.width) > 1) {
            edge.width <- edge.width[nonloops.e]
        }
        if (length(edge.lty) > 1) {
            edge.lty <- edge.lty[nonloops.e]
        }
        if (length(arrow.mode) > 1) {
            arrow.mode <- arrow.mode[nonloops.e]
        }
        if (length(arrow.size) > 1) {
            arrow.size <- arrow.size[nonloops.e]
        }
        if (length(curved) > 1) {
            curved <- curved[nonloops.e]
        }
        if (length(unique(arrow.mode)) == 1) {
            lc <- igraph.Arrows(x0, y0, x1, y1, h.col = edge.color, 
                sh.col = edge.color, sh.lwd = edge.width, h.lwd = 1, 
                open = FALSE, code = arrow.mode[1], sh.lty = edge.lty, 
                h.lty = 1, size = arrow.size, width = arrow.width, 
                curved = curved)
            lc.x <- lc$lab.x
            lc.y <- lc$lab.y
        }
        else {
            curved <- rep(curved, length = ecount(graph))[nonloops.e]
            lc.x <- lc.y <- numeric(length(curved))
            for (code in 0:3) {
                valid <- arrow.mode == code
                if (!any(valid)) {
                  next
                }
                ec <- edge.color
                if (length(ec) > 1) {
                  ec <- ec[valid]
                }
                ew <- edge.width
                if (length(ew) > 1) {
                  ew <- ew[valid]
                }
                el <- edge.lty
                if (length(el) > 1) {
                  el <- el[valid]
                }
                lc <- igraph.Arrows(x0[valid], y0[valid], x1[valid], 
                  y1[valid], code = code, sh.col = ec, h.col = ec, 
                  sh.lwd = ew, h.lwd = 1, h.lty = 1, sh.lty = el, 
                  open = FALSE, size = arrow.size, width = arrow.width, 
                  curved = curved[valid])
                lc.x[valid] <- lc$lab.x
                lc.y[valid] <- lc$lab.y
            }
        }
        if (!is.null(elab.x)) {
            lc.x <- ifelse(is.na(elab.x), lc.x, elab.x)
        }
        if (!is.null(elab.y)) {
            lc.y <- ifelse(is.na(elab.y), lc.y, elab.y)
        }
        text(lc.x, lc.y, labels = edge.labels, col = edge.label.color, 
            family = edge.label.family, font = edge.label.font, 
            cex = edge.label.cex)
    }
    rm(x0, y0, x1, y1)
    if (length(unique(shape)) == 1) {
        .igraph.shapes[[shape[1]]]$plot(layout, params = params)
    }
    else {
        sapply(seq(length = vcount(graph)), function(x) {
            .igraph.shapes[[shape[x]]]$plot(layout[x, , drop = FALSE], 
                v = x, params = params)
        })
    }
    par(xpd = TRUE)
    x <- layout[, 1] + label.dist * cos(-label.degree) * (vertex.size + 
        6 * 8 * log10(nchar(labels) + 1))/200
    y <- layout[, 2] + label.dist * sin(-label.degree) * (vertex.size + 
        6 * 8 * log10(nchar(labels) + 1))/200
    if (length(label.family) == 1) {
        text(x, y, labels = labels, col = label.color, family = label.family, 
            font = label.font, cex = label.cex)
    }
    else {
        if1 <- function(vect, idx) if (length(vect) == 1) 
            vect
        else vect[idx]
        sapply(seq_len(vcount(graph)), function(v) {
            text(x[v], y[v], labels = if1(labels, v), col = if1(label.color, 
                v), family = if1(label.family, v), font = if1(label.font, 
                v), cex = if1(label.cex, v))
        })
    }
    rm(x, y)
    invisible(NULL)
}


`%c%` <- function (x, y) 
{
    compose(x, y)
}


pa <- function (...) 
constructor_spec(sample_pa, ...)


largest.cliques <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_largest_cliques", graph, PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


cit_types <- function (...) 
constructor_spec(sample_cit_types, ...)


sample_pa <- function (n, power = 1, m = NULL, out.dist = NULL, out.seq = NULL, 
    out.pref = FALSE, zero.appeal = 1, directed = TRUE, algorithm = c("psumtree", 
        "psumtree-multiple", "bag"), start.graph = NULL) 
{
    if (!is.null(start.graph) && !is_igraph(start.graph)) {
        stop("`start.graph' not an `igraph' object")
    }
    if (!is.null(out.seq) && (!is.null(m) || !is.null(out.dist))) {
        warning("if `out.seq' is given `m' and `out.dist' should be NULL")
        m <- out.dist <- NULL
    }
    if (is.null(out.seq) && !is.null(out.dist) && !is.null(m)) {
        warning("if `out.dist' is given `m' will be ignored")
        m <- NULL
    }
    if (!is.null(m) && m == 0) {
        warning("`m' is zero, graph will be empty")
    }
    if (power < 0) {
        warning("`power' is negative")
    }
    if (is.null(m) && is.null(out.dist) && is.null(out.seq)) {
        m <- 1
    }
    n <- as.numeric(n)
    power <- as.numeric(power)
    if (!is.null(m)) {
        m <- as.numeric(m)
    }
    if (!is.null(out.dist)) {
        out.dist <- as.numeric(out.dist)
    }
    if (!is.null(out.seq)) {
        out.seq <- as.numeric(out.seq)
    }
    out.pref <- as.logical(out.pref)
    if (!is.null(out.dist)) {
        nn <- if (is.null(start.graph)) 
            n
        else n - vcount(start.graph)
        out.seq <- as.numeric(sample(0:(length(out.dist) - 1), 
            nn, replace = TRUE, prob = out.dist))
    }
    if (is.null(out.seq)) {
        out.seq <- numeric()
    }
    algorithm <- igraph.match.arg(algorithm)
    algorithm1 <- switch(algorithm, psumtree = 1, `psumtree-multiple` = 2, 
        bag = 0)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_barabasi_game", n, power, m, out.seq, 
        out.pref, zero.appeal, directed, algorithm1, start.graph, 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Barabasi graph"
        res$power <- power
        res$m <- m
        res$zero.appeal <- zero.appeal
        res$algorithm <- algorithm
    }
    res
}


layout_on_sphere <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_layout_sphere", graph, PACKAGE = "igraph")
}


edge.disjoint.paths <- function (graph, source, target) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_edge_disjoint_paths", graph, as.igraph.vs(graph, 
        source) - 1, as.igraph.vs(graph, target) - 1, PACKAGE = "igraph")
}


count_isomorphisms <- function (graph1, graph2, method = "vf2", ...) 
{
    method <- igraph.match.arg(method)
    if (method == "vf2") {
        graph.count.isomorphisms.vf2(graph1, graph2, ...)
    }
}


graph.attributes <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    base::.Call("R_igraph_mybracket2_copy", graph, 9L, 2L, PACKAGE = "igraph")
}


centralization.degree.tmax <- function (graph = NULL, nodes = 0, mode = c("all", "out", "in", 
    "total"), loops = FALSE) 
{
    if (!is.null(graph) && !is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.integer(nodes)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    loops <- as.logical(loops)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_degree_tmax", graph, 
        nodes, mode, loops, PACKAGE = "igraph")
    res
}


layout_randomly <- function (graph, dim = 2) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (dim == 2) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_layout_random", graph, PACKAGE = "igraph")
    }
    else if (dim == 3) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_layout_random_3d", graph, PACKAGE = "igraph")
    }
    else {
        stop("Invalid `dim' value")
    }
}


subgraph_isomorphic <- function (pattern, target, method = c("auto", "lad", "vf2"), 
    ...) 
{
    method <- igraph.match.arg(method)
    if (method == "auto") 
        method <- "lad"
    if (method == "lad") {
        graph.subisomorphic.lad(pattern, target, map = FALSE, 
            all.maps = FALSE, ...)$iso
    }
    else if (method == "vf2") {
        graph.subisomorphic.vf2(target, pattern, ...)$iso
    }
}


is_igraph <- function (graph) 
{
    "igraph" %in% class(graph)
}


modularity_matrix <- function (graph, membership, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    membership <- as.numeric(membership) - 1
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_modularity_matrix", graph, membership, 
        weights, PACKAGE = "igraph")
    res
}


is.bipartite <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    "type" %in% vertex_attr_names(graph)
}


sample_pref <- function (nodes, types, type.dist = rep(1, types), fixed.sizes = FALSE, 
    pref.matrix = matrix(1, types, types), directed = FALSE, 
    loops = FALSE) 
{
    if (nrow(pref.matrix) != types || ncol(pref.matrix) != types) {
        stop("Invalid size for preference matrix")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_preference_game", as.double(nodes), 
        as.double(types), as.double(type.dist), as.logical(fixed.sizes), 
        matrix(as.double(pref.matrix), types, types), as.logical(directed), 
        as.logical(loops), PACKAGE = "igraph")
    V(res[[1]])$type <- res[[2]] + 1
    if (igraph_opt("add.params")) {
        res[[1]]$name <- "Preference random graph"
        res[[1]]$types <- types
        res[[1]]$type.dist <- type.dist
        res[[1]]$fixed.sizes <- fixed.sizes
        res[[1]]$pref.matrix <- pref.matrix
        res[[1]]$loops <- loops
    }
    res[[1]]
}


graph.incidence <- function (incidence, directed = FALSE, mode = c("all", "out", 
    "in", "total"), multiple = FALSE, weighted = NULL, add.names = NULL) 
{
    directed <- as.logical(directed)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    multiple <- as.logical(multiple)
    if (inherits(incidence, "Matrix")) {
        res <- graph.incidence.sparse(incidence, directed = directed, 
            mode = mode, multiple = multiple, weighted = weighted)
    }
    else {
        incidence <- as.matrix(incidence)
        res <- graph.incidence.dense(incidence, directed = directed, 
            mode = mode, multiple = multiple, weighted = weighted)
    }
    if (is.null(add.names)) {
        if (!is.null(rownames(incidence)) && !is.null(colnames(incidence))) {
            add.names <- "name"
        }
        else {
            add.names <- NA
        }
    }
    else if (!is.na(add.names)) {
        if (is.null(rownames(incidence)) || is.null(colnames(incidence))) {
            warning("Cannot add row- and column names, at least one of them is missing")
            add.names <- NA
        }
    }
    if (!is.na(add.names)) {
        res <- set_vertex_attr(res, add.names, value = c(rownames(incidence), 
            colnames(incidence)))
    }
    res
}


with_edge_ <- function (...) 
{
    args <- grab_args()
    constructor_modifier(id = "with_edge_", args = args)
}


ba.game <- function (n, power = 1, m = NULL, out.dist = NULL, out.seq = NULL, 
    out.pref = FALSE, zero.appeal = 1, directed = TRUE, algorithm = c("psumtree", 
        "psumtree-multiple", "bag"), start.graph = NULL) 
{
    if (!is.null(start.graph) && !is_igraph(start.graph)) {
        stop("`start.graph' not an `igraph' object")
    }
    if (!is.null(out.seq) && (!is.null(m) || !is.null(out.dist))) {
        warning("if `out.seq' is given `m' and `out.dist' should be NULL")
        m <- out.dist <- NULL
    }
    if (is.null(out.seq) && !is.null(out.dist) && !is.null(m)) {
        warning("if `out.dist' is given `m' will be ignored")
        m <- NULL
    }
    if (!is.null(m) && m == 0) {
        warning("`m' is zero, graph will be empty")
    }
    if (power < 0) {
        warning("`power' is negative")
    }
    if (is.null(m) && is.null(out.dist) && is.null(out.seq)) {
        m <- 1
    }
    n <- as.numeric(n)
    power <- as.numeric(power)
    if (!is.null(m)) {
        m <- as.numeric(m)
    }
    if (!is.null(out.dist)) {
        out.dist <- as.numeric(out.dist)
    }
    if (!is.null(out.seq)) {
        out.seq <- as.numeric(out.seq)
    }
    out.pref <- as.logical(out.pref)
    if (!is.null(out.dist)) {
        nn <- if (is.null(start.graph)) 
            n
        else n - vcount(start.graph)
        out.seq <- as.numeric(sample(0:(length(out.dist) - 1), 
            nn, replace = TRUE, prob = out.dist))
    }
    if (is.null(out.seq)) {
        out.seq <- numeric()
    }
    algorithm <- igraph.match.arg(algorithm)
    algorithm1 <- switch(algorithm, psumtree = 1, `psumtree-multiple` = 2, 
        bag = 0)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_barabasi_game", n, power, m, out.seq, 
        out.pref, zero.appeal, directed, algorithm1, start.graph, 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Barabasi graph"
        res$power <- power
        res$m <- m
        res$zero.appeal <- zero.appeal
        res$algorithm <- algorithm
    }
    res
}


layout.merge <- function (graphs, layouts, method = "dla") 
{
    if (!all(sapply(graphs, is_igraph))) {
        stop("Not a graph object")
    }
    if (method == "dla") {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        res <- .Call("R_igraph_layout_merge_dla", graphs, layouts, 
            PACKAGE = "igraph")
    }
    else {
        stop("Invalid `method'.")
    }
    res
}


sample_traits_callaway <- function (nodes, types, edge.per.step = 1, type.dist = rep(1, 
    types), pref.matrix = matrix(1, types, types), directed = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_callaway_traits_game", as.double(nodes), 
        as.double(types), as.double(edge.per.step), as.double(type.dist), 
        matrix(as.double(pref.matrix), types, types), as.logical(directed), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Trait-based Callaway graph"
        res$types <- types
        res$edge.per.step <- edge.per.step
        res$type.dist <- type.dist
        res$pref.matrix <- pref.matrix
    }
    res
}


dominator_tree <- function (graph, root, mode = c("out", "in")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    root <- as.igraph.vs(graph, root)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_dominator_tree", graph, root - 1, 
        mode, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res$dom <- create_vs(graph, res$dom)
    }
    if (igraph_opt("return.vs.es")) {
        res$leftout <- create_vs(graph, res$leftout)
    }
    res
}


count_multiple <- function (graph, eids = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_count_multiple", graph, as.igraph.es(graph, 
        eids) - 1, PACKAGE = "igraph")
}


rewire <- function (graph, with) 
{
    if (!is(with, "igraph_rewiring_method")) {
        stop("'with' is not an igraph rewiring method")
    }
    do_call(with$fun, list(graph), .args = with$args)
}


delete.vertices <- function (graph, v) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_delete_vertices", graph, as.igraph.vs(graph, 
        v) - 1, PACKAGE = "igraph")
}


graphlets <- function (graph, weights = NULL, niter = 1000) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    niter <- as.integer(niter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_graphlets", graph, weights, niter, 
        PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$cliques)) {
            res$cliques[[i_]] <- create_vs(graph, res$cliques[[i_]])
        }
    }
    res
}


delete_vertices <- function (graph, v) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_delete_vertices", graph, as.igraph.vs(graph, 
        v) - 1, PACKAGE = "igraph")
}


get.edge.ids <- function (graph, vp, directed = TRUE, error = FALSE, multi = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_get_eids", graph, as.igraph.vs(graph, vp) - 
        1, as.logical(directed), as.logical(error), as.logical(multi), 
        PACKAGE = "igraph") + 1
}


categorical_pal <- function (n) 
{
    stopifnot(n > 0)
    x <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
        "#D55E00", "#CC79A7", "#999999")
    if (n > length(x)) 
        warning("Cannot make ", n, " categorical colors")
    n <- min(n, length(x))
    x[seq_len(n)]
}


layout.reingold.tilford <- function (..., params = list()) 
{
    do_call(layout_as_tree, .args = c(list(...), params))
}


tkplot <- function (graph, canvas.width = 450, canvas.height = 450, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    requireNamespace("tcltk", quietly = TRUE) || stop("tcl/tk library not available")
    params <- i.parse.plot.params(graph, list(...))
    labels <- params("vertex", "label")
    label.color <- .tkplot.convert.color(params("vertex", "label.color"))
    label.font <- .tkplot.convert.font(params("vertex", "label.font"), 
        params("vertex", "label.family"), params("vertex", "label.cex"))
    label.degree <- params("vertex", "label.degree")
    label.dist <- params("vertex", "label.dist")
    vertex.color <- .tkplot.convert.color(params("vertex", "color"))
    vertex.size <- params("vertex", "size")
    vertex.frame.color <- .tkplot.convert.color(params("vertex", 
        "frame.color"))
    edge.color <- .tkplot.convert.color(params("edge", "color"))
    edge.width <- params("edge", "width")
    edge.labels <- params("edge", "label")
    edge.lty <- params("edge", "lty")
    loop.angle <- params("edge", "loop.angle")
    arrow.mode <- params("edge", "arrow.mode")
    edge.label.font <- .tkplot.convert.font(params("edge", "label.font"), 
        params("edge", "label.family"), params("edge", "label.cex"))
    edge.label.color <- params("edge", "label.color")
    arrow.size <- params("edge", "arrow.size")[1]
    curved <- params("edge", "curved")
    curved <- rep(curved, length = ecount(graph))
    layout <- unname(params("plot", "layout"))
    layout[, 2] <- -layout[, 2]
    margin <- params("plot", "margin")
    margin <- rep(margin, length = 4)
    arrow.mode <- i.get.arrow.mode(graph, arrow.mode)
    edge.lty <- i.tkplot.get.edge.lty(edge.lty)
    top <- tcltk::tktoplevel(background = "lightgrey")
    canvas <- tcltk::tkcanvas(top, relief = "raised", width = canvas.width, 
        height = canvas.height, borderwidth = 2)
    tcltk::tkpack(canvas, fill = "both", expand = 1)
    vertex.params <- sdf(vertex.color = vertex.color, vertex.size = vertex.size, 
        label.font = label.font, NROW = vcount(graph))
    params <- list(vertex.params = vertex.params, edge.color = edge.color, 
        label.color = label.color, labels.state = 1, edge.width = edge.width, 
        padding = margin * 300 + max(vertex.size) + 5, grid = 0, 
        label.degree = label.degree, label.dist = label.dist, 
        edge.labels = edge.labels, vertex.frame.color = vertex.frame.color, 
        loop.angle = loop.angle, edge.lty = edge.lty, arrow.mode = arrow.mode, 
        edge.label.font = edge.label.font, edge.label.color = edge.label.color, 
        arrow.size = arrow.size, curved = curved)
    popup.menu <- tcltk::tkmenu(canvas)
    tcltk::tkadd(popup.menu, "command", label = "Fit to screen", 
        command = function() {
            tk_fit(tkp.id)
        })
    vertex.popup.menu <- tcltk::tkmenu(canvas)
    tcltk::tkadd(vertex.popup.menu, "command", label = "Vertex color", 
        command = function() {
            tkp <- .tkplot.get(tkp.id)
            vids <- .tkplot.get.selected.vertices(tkp.id)
            if (length(vids) == 0) 
                return(FALSE)
            initialcolor <- tkp$params$vertex.params[vids[1], 
                "vertex.color"]
            color <- .tkplot.select.color(initialcolor)
            if (color == "") 
                return(FALSE)
            .tkplot.update.vertex.color(tkp.id, vids, color)
        })
    tcltk::tkadd(vertex.popup.menu, "command", label = "Vertex size", 
        command = function() {
            tkp <- .tkplot.get(tkp.id)
            vids <- .tkplot.get.selected.vertices(tkp.id)
            if (length(vids) == 0) 
                return(FALSE)
            initialsize <- tkp$params$vertex.params[1, "vertex.size"]
            size <- .tkplot.select.number("Vertex size", initialsize, 
                1, 20)
            if (is.na(size)) 
                return(FALSE)
            .tkplot.update.vertex.size(tkp.id, vids, size)
        })
    edge.popup.menu <- tcltk::tkmenu(canvas)
    tcltk::tkadd(edge.popup.menu, "command", label = "Edge color", 
        command = function() {
            tkp <- .tkplot.get(tkp.id)
            eids <- .tkplot.get.selected.edges(tkp.id)
            if (length(eids) == 0) 
                return(FALSE)
            initialcolor <- ifelse(length(tkp$params$edge.color) > 
                1, tkp$params$edge.color[eids[1]], tkp$params$edge.color)
            color <- .tkplot.select.color(initialcolor)
            if (color == "") 
                return(FALSE)
            .tkplot.update.edge.color(tkp.id, eids, color)
        })
    tcltk::tkadd(edge.popup.menu, "command", label = "Edge width", 
        command = function() {
            tkp <- .tkplot.get(tkp.id)
            eids <- .tkplot.get.selected.edges(tkp.id)
            if (length(eids) == 0) 
                return(FALSE)
            initialwidth <- ifelse(length(tkp$params$edge.width) > 
                1, tkp$params$edge.width[eids[1]], tkp$params$edge.width)
            width <- .tkplot.select.number("Edge width", initialwidth, 
                1, 10)
            if (is.na(width)) 
                return(FALSE)
            .tkplot.update.edge.width(tkp.id, eids, width)
        })
    tkp <- list(top = top, canvas = canvas, graph = graph, coords = layout, 
        labels = labels, params = params, popup.menu = popup.menu, 
        vertex.popup.menu = vertex.popup.menu, edge.popup.menu = edge.popup.menu)
    tkp.id <- .tkplot.new(tkp)
    tcltk::tktitle(top) <- paste("Graph plot", as.character(tkp.id))
    main.menu <- tcltk::tkmenu(top)
    tcltk::tkadd(main.menu, "command", label = "Close", command = function() {
        tk_close(tkp.id, TRUE)
    })
    select.menu <- .tkplot.select.menu(tkp.id, main.menu)
    tcltk::tkadd(main.menu, "cascade", label = "Select", menu = select.menu)
    layout.menu <- .tkplot.layout.menu(tkp.id, main.menu)
    tcltk::tkadd(main.menu, "cascade", label = "Layout", menu = layout.menu)
    view.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(main.menu, "cascade", label = "View", menu = view.menu)
    tcltk::tkadd(view.menu, "command", label = "Fit to screen", 
        command = function() {
            tk_fit(tkp.id)
        })
    tcltk::tkadd(view.menu, "command", label = "Center on screen", 
        command = function() {
            tk_center(tkp.id)
        })
    tcltk::tkadd(view.menu, "separator")
    view.menu.labels <- tcltk::tclVar(1)
    view.menu.grid <- tcltk::tclVar(0)
    tcltk::tkadd(view.menu, "checkbutton", label = "Labels", 
        variable = view.menu.labels, command = function() {
            .tkplot.toggle.labels(tkp.id)
        })
    tcltk::tkadd(view.menu, "separator")
    rotate.menu <- tcltk::tkmenu(view.menu)
    tcltk::tkadd(view.menu, "cascade", label = "Rotate", menu = rotate.menu)
    sapply(c(-90, -45, -15, -5, -1, 1, 5, 15, 45, 90), function(deg) {
        tcltk::tkadd(rotate.menu, "command", label = paste(deg, 
            "degree"), command = function() {
            tk_rotate(tkp.id, degree = deg)
        })
    })
    export.menu <- tcltk::tkmenu(main.menu)
    tcltk::tkadd(main.menu, "cascade", label = "Export", menu = export.menu)
    tcltk::tkadd(export.menu, "command", label = "Postscript", 
        command = function() {
            tk_postscript(tkp.id)
        })
    tcltk::tkconfigure(top, "-menu", main.menu)
    .tkplot.create.edges(tkp.id)
    .tkplot.create.vertices(tkp.id)
    tk_fit(tkp.id, canvas.width, canvas.height)
    tcltk::tkbind(top, "<Destroy>", function() tk_close(tkp.id, 
        FALSE))
    tcltk::tkitembind(canvas, "vertex||label||edge", "<1>", function(x, 
        y) {
        tkp <- .tkplot.get(tkp.id)
        canvas <- .tkplot.get(tkp.id, "canvas")
        .tkplot.deselect.all(tkp.id)
        .tkplot.select.current(tkp.id)
    })
    tcltk::tkitembind(canvas, "vertex||label||edge", "<Control-1>", 
        function(x, y) {
            canvas <- .tkplot.get(tkp.id, "canvas")
            curtags <- as.character(tcltk::tkgettags(canvas, 
                "current"))
            seltags <- as.character(tcltk::tkgettags(canvas, 
                "selected"))
            if ("vertex" %in% curtags && "vertex" %in% seltags) {
                if ("selected" %in% curtags) {
                  .tkplot.deselect.current(tkp.id)
                }
                else {
                  .tkplot.select.current(tkp.id)
                }
            }
            else if ("edge" %in% curtags && "edge" %in% seltags) {
                if ("selected" %in% curtags) {
                  .tkplot.deselect.current(tkp.id)
                }
                else {
                  .tkplot.select.current(tkp.id)
                }
            }
            else if ("label" %in% curtags && "vertex" %in% seltags) {
                vtag <- curtags[pmatch("v-", curtags)]
                tkid <- as.numeric(tcltk::tkfind(canvas, "withtag", 
                  paste(sep = "", "vertex&&", vtag)))
                vtags <- as.character(tcltk::tkgettags(canvas, 
                  tkid))
                if ("selected" %in% vtags) {
                  .tkplot.deselect.vertex(tkp.id, tkid)
                }
                else {
                  .tkplot.select.vertex(tkp.id, tkid)
                }
            }
            else {
                .tkplot.deselect.all(tkp.id)
                .tkplot.select.current(tkp.id)
            }
        })
    tcltk::tkitembind(canvas, "vertex||edge||label", "<Shift-Alt-1>", 
        function(x, y) {
            canvas <- .tkplot.get(tkp.id, "canvas")
            tcltk::tkitemlower(canvas, "current")
        })
    tcltk::tkitembind(canvas, "vertex||edge||label", "<Alt-1>", 
        function(x, y) {
            canvas <- .tkplot.get(tkp.id, "canvas")
            tcltk::tkitemraise(canvas, "current")
        })
    tcltk::tkbind(canvas, "<3>", function(x, y) {
        canvas <- .tkplot.get(tkp.id, "canvas")
        tags <- as.character(tcltk::tkgettags(canvas, "current"))
        if ("label" %in% tags) {
            vtag <- tags[pmatch("v-", tags)]
            vid <- as.character(tcltk::tkfind(canvas, "withtag", 
                paste(sep = "", "vertex&&", vtag)))
            tags <- as.character(tcltk::tkgettags(canvas, vid))
        }
        if ("selected" %in% tags) {
        }
        else {
            .tkplot.deselect.all(tkp.id)
            .tkplot.select.current(tkp.id)
        }
        tags <- as.character(tcltk::tkgettags(canvas, "selected"))
        if ("vertex" %in% tags || "label" %in% tags) {
            menu <- .tkplot.get(tkp.id, "vertex.popup.menu")
        }
        else if ("edge" %in% tags) {
            menu <- .tkplot.get(tkp.id, "edge.popup.menu")
        }
        else {
            menu <- .tkplot.get(tkp.id, "popup.menu")
        }
        x <- as.integer(x) + as.integer(tcltk::tkwinfo("rootx", 
            canvas))
        y <- as.integer(y) + as.integer(tcltk::tkwinfo("rooty", 
            canvas))
        tcltk::.Tcl(paste("tk_popup", tcltk::.Tcl.args(menu, 
            x, y)))
    })
    if (tkp$params$label.dist == 0) 
        tobind <- "vertex||label"
    else tobind <- "vertex"
    tcltk::tkitembind(canvas, tobind, "<B1-Motion>", function(x, 
        y) {
        tkp <- .tkplot.get(tkp.id)
        x <- as.numeric(x)
        y <- as.numeric(y)
        width <- as.numeric(tcltk::tkwinfo("width", tkp$canvas))
        height <- as.numeric(tcltk::tkwinfo("height", tkp$canvas))
        if (x < 10) {
            x <- 10
        }
        if (x > width - 10) {
            x <- width - 10
        }
        if (y < 10) {
            y <- 10
        }
        if (y > height - 10) {
            y <- height - 10
        }
        tags <- as.character(tcltk::tkgettags(tkp$canvas, "selected"))
        id <- as.numeric(strsplit(tags[pmatch("v-", tags)], "-", 
            fixed = TRUE)[[1]][2])
        if (is.na(id)) {
            return()
        }
        .tkplot.set.vertex.coords(tkp.id, id, x, y)
        .tkplot.update.vertex(tkp.id, id, x, y)
    })
    if (tkp$params$label.dist != 0) {
        tcltk::tkitembind(canvas, "label", "<B1-Motion>", function(x, 
            y) {
            tkp <- .tkplot.get(tkp.id)
            x <- as.numeric(x)
            y <- as.numeric(y)
            tags <- as.character(tcltk::tkgettags(tkp$canvas, 
                "selected"))
            id <- as.numeric(strsplit(tags[pmatch("v-", tags)], 
                "-", fixed = TRUE)[[1]][2])
            if (is.na(id)) {
                return()
            }
            phi <- pi + atan2(tkp$coords[id, 2] - y, tkp$coords[id, 
                1] - x)
            .tkplot.set.label.degree(tkp.id, id, phi)
            .tkplot.update.label(tkp.id, id, tkp$coords[id, 1], 
                tkp$coords[id, 2])
        })
    }
    rm(tkp, params, layout, vertex.color, edge.color, top, canvas, 
        main.menu, layout.menu, view.menu, export.menu, label.font, 
        label.degree, vertex.frame.color, vertex.params)
    tkp.id
}


is.igraph <- function (graph) 
{
    "igraph" %in% class(graph)
}


graph.atlas <- function (n) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_atlas", as.numeric(n), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("Graph from the Atlas #%i", n)
        res$n <- n
    }
    res
}


with_drl <- function (...) 
layout_spec(layout_with_drl, ...)


vcount <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_vcount", graph, PACKAGE = "igraph")
    res
}


is_hierarchical <- function (communities) 
{
    !is.null(communities$merges)
}


as_membership <- function (x) 
add_class(x, "membership")


make_lattice <- function (dimvector = NULL, length = NULL, dim = NULL, nei = 1, 
    directed = FALSE, mutual = FALSE, circular = FALSE) 
{
    if (is.null(dimvector)) {
        dimvector <- rep(length, dim)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_lattice", as.numeric(dimvector), as.numeric(nei), 
        as.logical(directed), as.logical(mutual), as.logical(circular), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Lattice graph"
        res$dimvector <- dimvector
        res$nei <- nei
        res$mutual <- mutual
        res$circular <- circular
    }
    res
}


ivs_size <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_independence_number", graph, PACKAGE = "igraph")
}


read_graph <- function (file, format = c("edgelist", "pajek", "ncol", "lgl", 
    "graphml", "dimacs", "graphdb", "gml", "dl"), ...) 
{
    if (!is.character(file) || length(grep("://", file, fixed = TRUE)) > 
        0 || length(grep("~", file, fixed = TRUE)) > 0) {
        buffer <- read.graph.toraw(file)
        file <- tempfile()
        write.graph.fromraw(buffer, file)
    }
    format <- igraph.match.arg(format)
    res <- switch(format, pajek = read.graph.pajek(file, ...), 
        ncol = read.graph.ncol(file, ...), edgelist = read.graph.edgelist(file, 
            ...), lgl = read.graph.lgl(file, ...), graphml = read.graph.graphml(file, 
            ...), dimacs = read.graph.dimacs(file, ...), graphdb = read.graph.graphdb(file, 
            ...), gml = read.graph.gml(file, ...), dl = read.graph.dl(file, 
            ...), stop(paste("Unknown file format:", format)))
    res
}


bipartite.projection.size <- function (graph, types = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        if (!is.logical(types)) {
            warning("vertex types converted to logical")
        }
        types <- as.logical(types)
        if (any(is.na(types))) {
            stop("`NA' is not allowed in vertex types")
        }
    }
    else {
        stop("Not a bipartite graph, supply `types' argument")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_bipartite_projection_size", graph, 
        types, PACKAGE = "igraph")
    res
}


canonical_permutation <- function (graph, sh = "fm") 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    sh <- switch(igraph.match.arg(sh), f = 0, fl = 1, fs = 2, 
        fm = 3, flm = 4, fsm = 5)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_canonical_permutation", graph, sh, 
        PACKAGE = "igraph")
    res
}


graph.motifs.no <- function (graph, size = 3, cut.prob = rep(0, size)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cut.prob <- as.numeric(cut.prob)
    if (length(cut.prob) != size) {
        cut.prob <- c(cut.prob[-length(cut.prob)], rep(cut.prob[-length(cut.prob)], 
            length(cut.prob) - 1))
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_motifs_randesu_no", graph, as.integer(size), 
        as.numeric(cut.prob), PACKAGE = "igraph")
}


edge_disjoint_paths <- function (graph, source, target) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_edge_disjoint_paths", graph, as.igraph.vs(graph, 
        source) - 1, as.igraph.vs(graph, target) - 1, PACKAGE = "igraph")
}


assortativity.nominal <- function (graph, types, directed = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    types <- as.numeric(types) - 1
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_assortativity_nominal", graph, types, 
        directed, PACKAGE = "igraph")
    res
}


are.connected <- function (graph, v1, v2) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_are_connected", graph, as.igraph.vs(graph, 
        v1) - 1, as.igraph.vs(graph, v2) - 1, PACKAGE = "igraph")
}


mean_distance <- function (graph, directed = TRUE, unconnected = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_average_path_length", graph, as.logical(directed), 
        as.logical(unconnected), PACKAGE = "igraph")
}


minimum.spanning.tree <- function (graph, weights = NULL, algorithm = NULL, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(algorithm)) {
        if (!is.null(weights) || "weight" %in% edge_attr_names(graph)) {
            algorithm <- "prim"
        }
        else {
            algorithm <- "unweighted"
        }
    }
    if (algorithm == "unweighted") {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_minimum_spanning_tree_unweighted", graph, 
            PACKAGE = "igraph")
    }
    else if (algorithm == "prim") {
        if (is.null(weights) && !"weight" %in% edge_attr_names(graph)) {
            stop("edges weights must be supplied for Prim's algorithm")
        }
        else if (is.null(weights)) {
            weights <- E(graph)$weight
        }
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_minimum_spanning_tree_prim", graph, as.numeric(weights), 
            PACKAGE = "igraph")
    }
    else {
        stop("Invalid algorithm")
    }
}


running_mean <- function (v, binwidth) 
{
    v <- as.numeric(v)
    binwidth <- as.numeric(binwidth)
    if (length(v) < binwidth) {
        stop("Vector too short for this binwidth.")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_running_mean", v, binwidth, PACKAGE = "igraph")
}


code.length <- function (communities) 
{
    communities$codelength
}


as.directed <- function (graph, mode = c("mutual", "arbitrary")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, arbitrary = 0, mutual = 1)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_to_directed", graph, as.numeric(mode), PACKAGE = "igraph")
}


tree <- function (...) 
constructor_spec(make_tree, ...)


sample_seq <- function (low, high, length) 
{
    if (length > high - low + 1) {
        stop("length too big for this interval")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_random_sample", as.numeric(low), as.numeric(high), 
        as.numeric(length), PACKAGE = "igraph")
}


is.separator <- function (graph, candidate) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    candidate <- as.igraph.vs(graph, candidate)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_separator", graph, candidate - 
        1, PACKAGE = "igraph")
    res
}


is.dag <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_dag", graph, PACKAGE = "igraph")
    res
}


isomorphisms <- function (graph1, graph2, method = "vf2", ...) 
{
    method <- igraph.match.arg(method)
    if (method == "vf2") {
        graph.get.isomorphisms.vf2(graph1, graph2, ...)
    }
}


layout.fruchterman.reingold.grid <- function (graph, ...) 
{
    warning("Grid Fruchterman-Reingold layout was removed,\n", 
        "we use Fruchterman-Reingold instead.")
    layout_with_fr(graph)
}


simplify <- function (graph, remove.multiple = TRUE, remove.loops = TRUE, 
    edge.attr.comb = igraph_opt("edge.attr.comb")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    remove.multiple <- as.logical(remove.multiple)
    remove.loops <- as.logical(remove.loops)
    edge.attr.comb <- igraph.i.attribute.combination(edge.attr.comb)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_simplify", graph, remove.multiple, 
        remove.loops, edge.attr.comb, PACKAGE = "igraph")
    res
}


.igraph.status <- function (message) 
{
    type <- igraph_opt("verbose")
    if (is.logical(type) && type) {
        message(message, appendLF = FALSE)
    }
    else {
        switch(type, tk = message(message, appendLF = FALSE), 
            tkconsole = .igraph.progress.tkconsole.message(message, 
                start = TRUE), stop("Cannot interpret 'verbose' option, this should not happen"))
    }
    0L
}


compare <- function (comm1, comm2, method = c("vi", "nmi", "split.join", 
    "rand", "adjusted.rand")) 
UseMethod("compare")


citing.cited.type.game <- function (n, edges = 1, types = rep(0, n), pref = matrix(1, nrow = length(types), 
    ncol = length(types)), directed = TRUE, attr = TRUE) 
{
    pref <- structure(as.numeric(pref), dim = dim(pref))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_citing_cited_type_game", as.numeric(n), 
        as.numeric(types), pref, as.numeric(edges), as.logical(directed), 
        PACKAGE = "igraph")
    if (attr) {
        V(res)$type <- types
    }
    if (igraph_opt("add.params")) {
        res$name <- "Random citation graph (citing & cited type)"
        res$edges <- edges
    }
    res
}


assortativity <- function (graph, types1, types2 = NULL, directed = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    types1 <- as.numeric(types1)
    if (!is.null(types2)) 
        types2 <- as.numeric(types2)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_assortativity", graph, types1, types2, 
        directed, PACKAGE = "igraph")
    res
}


predict_edges <- function (graph, hrg = NULL, start = FALSE, num.samples = 10000, 
    num.bins = 25) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    start <- as.logical(start)
    num.samples <- as.integer(num.samples)
    num.bins <- as.integer(num.bins)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_predict", graph, hrg, start, num.samples, 
        num.bins, PACKAGE = "igraph")
    res$edges <- matrix(res$edges, ncol = 2, byrow = TRUE)
    class(res$hrg) <- "igraphHRG"
    res
}


graph_from_data_frame <- function (d, directed = TRUE, vertices = NULL) 
{
    d <- as.data.frame(d)
    if (!is.null(vertices)) {
        vertices <- as.data.frame(vertices)
    }
    if (ncol(d) < 2) {
        stop("the data frame should contain at least two columns")
    }
    if (any(is.na(d[, 1:2]))) {
        warning("In `d' `NA' elements were replaced with string \"NA\"")
        d[, 1:2][is.na(d[, 1:2])] <- "NA"
    }
    if (!is.null(vertices) && any(is.na(vertices[, 1]))) {
        warning("In `vertices[,1]' `NA' elements were replaced with string \"NA\"")
        vertices[, 1][is.na(vertices[, 1])] <- "NA"
    }
    names <- unique(c(as.character(d[, 1]), as.character(d[, 
        2])))
    if (!is.null(vertices)) {
        names2 <- names
        vertices <- as.data.frame(vertices)
        if (ncol(vertices) < 1) {
            stop("Vertex data frame contains no rows")
        }
        names <- as.character(vertices[, 1])
        if (any(duplicated(names))) {
            stop("Duplicate vertex names")
        }
        if (any(!names2 %in% names)) {
            stop("Some vertex names in edge list are not listed in vertex data frame")
        }
    }
    g <- make_empty_graph(n = 0, directed = directed)
    attrs <- list(name = names)
    if (!is.null(vertices)) {
        if (ncol(vertices) > 1) {
            for (i in 2:ncol(vertices)) {
                newval <- vertices[, i]
                if (class(newval) == "factor") {
                  newval <- as.character(newval)
                }
                attrs[[names(vertices)[i]]] <- newval
            }
        }
    }
    g <- add_vertices(g, length(names), attr = attrs)
    from <- as.character(d[, 1])
    to <- as.character(d[, 2])
    edges <- rbind(match(from, names), match(to, names))
    attrs <- list()
    if (ncol(d) > 2) {
        for (i in 3:ncol(d)) {
            newval <- d[, i]
            if (class(newval) == "factor") {
                newval <- as.character(newval)
            }
            attrs[[names(d)[i]]] <- newval
        }
    }
    g <- add_edges(g, edges, attr = attrs)
    g
}


sbm.game <- function (n, pref.matrix, block.sizes, directed = FALSE, loops = FALSE) 
{
    n <- as.integer(n)
    pref.matrix <- as.matrix(structure(as.double(pref.matrix), 
        dim = dim(pref.matrix)))
    block.sizes <- as.integer(block.sizes)
    directed <- as.logical(directed)
    loops <- as.logical(loops)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_sbm_game", n, pref.matrix, block.sizes, 
        directed, loops, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Stochastic block-model")
    res <- set.graph.attribute(res, "loops", loops)
    res
}


`vertex.attributes<-` <- function (graph, index = V(graph), value) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.list(value) || (length(value) > 0 && is.null(names(value))) || 
        any(names(value) == "") || any(duplicated(names(value)))) {
        stop("Value must be a named list with unique names")
    }
    if (any(sapply(value, length) != length(index))) {
        stop("Invalid attribute value length, must match number of vertices")
    }
    if (!missing(index)) {
        index <- as.igraph.vs(graph, index)
        if (any(duplicated(index)) || any(is.na(index))) {
            stop("Invalid vertices in index")
        }
    }
    if (!missing(index) && (length(index) != vcount(graph) || 
        any(index != V(graph)))) {
        vs <- V(graph)
        for (i in seq_along(value)) {
            tmp <- value[[i]]
            length(tmp) <- 0
            length(tmp) <- length(vs)
            tmp[index] <- value[[i]]
            value[[i]] <- tmp
        }
    }
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 3L, value, 
        PACKAGE = "igraph")
}


hrg_tree <- function (hrg) 
{
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_dendrogram", hrg, PACKAGE = "igraph")
    res
}


layout.mds <- function (graph, dist = NULL, dim = 2, options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(dist)) 
        dist <- structure(as.double(dist), dim = dim(dist))
    dim <- as.integer(dim)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_mds", graph, dist, dim, PACKAGE = "igraph")
    res
}


mod.matrix <- function (graph, membership, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    membership <- as.numeric(membership) - 1
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_modularity_matrix", graph, membership, 
        weights, PACKAGE = "igraph")
    res
}


layout_as_bipartite <- function (graph, types = NULL, hgap = 1, vgap = 1, maxiter = 100) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        if (!is.logical(types)) {
            warning("vertex types converted to logical")
        }
        types <- as.logical(types)
        if (any(is.na(types))) {
            stop("`NA' is not allowed in vertex types")
        }
    }
    else {
        stop("Not a bipartite graph, supply `types' argument")
    }
    hgap <- as.numeric(hgap)
    vgap <- as.numeric(vgap)
    maxiter <- as.integer(maxiter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_bipartite", graph, types, hgap, 
        vgap, maxiter, PACKAGE = "igraph")
    res
}


centr_degree <- function (graph, mode = c("all", "out", "in", "total"), loops = TRUE, 
    normalized = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    loops <- as.logical(loops)
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_degree", graph, mode, 
        loops, normalized, PACKAGE = "igraph")
    res
}


farthest_vertices <- function (graph, directed = TRUE, unconnected = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_farthest_points", graph, as.logical(directed), 
        as.logical(unconnected), weights, PACKAGE = "igraph")
    res <- list(vertices = res[1:2] + 1L, distance = res[3])
    if (igraph_opt("return.vs.es")) {
        res$vertices <- create_vs(graph, res$vertices)
    }
    res
}


igraph.sample <- function (low, high, length) 
{
    if (length > high - low + 1) {
        stop("length too big for this interval")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_random_sample", as.numeric(low), as.numeric(high), 
        as.numeric(length), PACKAGE = "igraph")
}


igraph_demo <- function (which) 
{
    if (missing(which)) {
        demodir <- system.file("demo", package = "igraph")
        if (demodir == "") {
            stop("Could not find igraph demos, broken igraph installation?")
        }
        return(sub("\\.R$", "", list.files(demodir)))
    }
    if (!grepl("\\.R$", which)) {
        which <- paste(which, sep = ".", "R")
    }
    if (!file.exists(which) && !grepl("^/", which)) {
        which <- system.file(paste("demo", sep = "/", which), 
            package = "igraph")
    }
    if (which == "" || !file.exists(which)) {
        stop("Could not find demo file")
    }
    .igraphdemo.next <- function(top, txt) {
        act <- as.character(tcltk::tktag.nextrange(txt, "active", 
            "0.0"))
        if (length(act) == 0) {
            return()
        }
        options(keep.source = TRUE)
        text <- tcltk::tclvalue(tcltk::tkget(txt, act[1], act[2]))
        cat("=======================================================\n")
        expr <- parse(text = text)
        for (i in seq_along(expr)) {
            co <- as.character(attributes(expr)$srcref[[i]])
            co[1] <- paste("> ", sep = "", co[1])
            if (length(co) > 1) {
                co[-1] <- paste(" +", sep = "", co[-1])
            }
            cat(co, sep = "\n")
            res <- withVisible(eval(expr[[i]], envir = .GlobalEnv))
            if (res$visible) {
                print(res$value)
            }
        }
        cat("> -------------------------------------------------------\n")
        cat(options()$prompt)
        tcltk::tktag.remove(txt, "activechunk", act[1], act[2])
        tcltk::tktag.remove(txt, "active", act[1], act[2])
        nex <- as.character(tcltk::tktag.nextrange(txt, "activechunk", 
            act[1]))
        if (length(nex) != 0) {
            tcltk::tktag.add(txt, "active", nex[1], nex[2])
            tcltk::tksee(txt, paste(sep = "", as.numeric(nex[2]), 
                ".0"))
            tcltk::tksee(txt, paste(sep = "", as.numeric(nex[1]), 
                ".0"))
        }
    }
    .igraphdemo.close <- function(top) {
        tcltk::tkdestroy(top)
    }
    .igraphdemo.reset <- function(top, txt, which) {
        demolines <- readLines(which)
        demolines <- demolines[!grepl("^pause\\(\\)$", demolines)]
        demolines <- paste(" ", sep = "", demolines)
        ch <- grep("^[ ]*###", demolines)
        ch <- c(ch, length(demolines) + 1)
        if (length(ch) == 1) {
            warning("Demo source file does not contain chunks")
        }
        else {
            demolines <- demolines[ch[1]:length(demolines)]
            ch <- grep("^[ ]*###", demolines)
            ch <- c(ch, length(demolines) + 1)
        }
        tcltk::tkconfigure(txt, state = "normal")
        tcltk::tkdelete(txt, "0.0", "end")
        tcltk::tkinsert(txt, "insert", paste(demolines, collapse = "\n"))
        tcltk::tkconfigure(txt, state = "disabled")
        for (i in seq_along(ch[-1])) {
            from <- paste(sep = "", ch[i], ".0")
            to <- paste(sep = "", ch[i + 1] - 1, ".0")
            tcltk::tktag.add(txt, "chunk", from, to)
            tcltk::tktag.add(txt, "activechunk", from, to)
        }
        tcltk::tktag.configure(txt, "chunk", "-borderwidth", 
            "1")
        tcltk::tktag.configure(txt, "chunk", "-relief", "sunken")
        if (length(ch) >= 2) {
            tcltk::tktag.add(txt, "active", paste(sep = "", ch[1], 
                ".0"), paste(sep = "", ch[2] - 1, ".0"))
            tcltk::tktag.configure(txt, "active", "-foreground", 
                "red")
            tcltk::tktag.configure(txt, "active", "-background", 
                "lightgrey")
        }
        comm <- grep("^#", demolines)
        for (i in comm) {
            tcltk::tktag.add(txt, "comment", paste(sep = "", 
                i, ".0"), paste(sep = "", i, ".end"))
        }
        tcltk::tktag.configure(txt, "comment", "-font", "bold")
        tcltk::tktag.configure(txt, "comment", "-foreground", 
            "darkolivegreen")
    }
    top <- tcltk::tktoplevel(background = "lightgrey")
    tcltk::tktitle(top) <- paste("igraph demo:", which)
    main.menu <- tcltk::tkmenu(top)
    tcltk::tkadd(main.menu, "command", label = "Close", command = function() .igraphdemo.close(top))
    tcltk::tkadd(main.menu, "command", label = "Reset", command = function() .igraphdemo.reset(top, 
        txt, which))
    tcltk::tkconfigure(top, "-menu", main.menu)
    scr <- tcltk::tkscrollbar(top, repeatinterval = 5, command = function(...) tcltk::tkyview(txt, 
        ...))
    txt <- tcltk::tktext(top, yscrollcommand = function(...) tcltk::tkset(scr, 
        ...), width = 80, height = 40)
    but <- tcltk::tkbutton(top, text = "Next", command = function() .igraphdemo.next(top, 
        txt))
    tcltk::tkpack(but, side = "bottom", fill = "x", expand = 0)
    tcltk::tkpack(scr, side = "right", fill = "y", expand = 0)
    tcltk::tkpack(txt, side = "left", fill = "both", expand = 1)
    .igraphdemo.reset(top, txt, which)
    invisible()
}


cohesive.blocks <- function (graph, labels = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_cohesive_blocks", graph, PACKAGE = "igraph")
    class(res) <- "cohesiveBlocks"
    if (labels && "name" %in% vertex_attr_names(graph)) {
        res$labels <- V(graph)$name
    }
    if (igraph_opt("return.vs.es")) {
        res$blocks <- lapply(res$blocks, create_vs, graph = graph)
    }
    res$vcount <- vcount(graph)
    res
}


largest_ivs <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_largest_independent_vertex_sets", 
        graph, PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


dot_product <- function (...) 
constructor_spec(sample_dot_product, ...)


make_kautz_graph <- function (m, n) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_kautz", as.numeric(m), as.numeric(n), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("Kautz graph %i-%i", m, n)
        res$m <- m
        res$n <- n
    }
    res
}


make_de_bruijn_graph <- function (m, n) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_de_bruijn", as.numeric(m), as.numeric(n), 
        PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("De-Bruijn graph %i-%i", m, n)
        res$m <- m
        res$n <- n
    }
    res
}


`%->%` <- function (f, t) 
{
    from <- get(".igraph.from", parent.frame())
    to <- get(".igraph.to", parent.frame())
    graph <- get(".igraph.graph", parent.frame())
    f <- as.igraph.vs(graph, f) - 1
    t <- as.igraph.vs(graph, t) - 1
    if (is_directed(graph)) {
        from %in% f & to %in% t
    }
    else {
        (from %in% f & to %in% t) | (to %in% f & from %in% t)
    }
}


bipartite_projection <- function (graph, types = NULL, multiplicity = TRUE, probe1 = NULL, 
    which = c("both", "true", "false"), remove.type = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        if (!is.logical(types)) {
            warning("vertex types converted to logical")
        }
        types <- as.logical(types)
        if (any(is.na(types))) {
            stop("`NA' is not allowed in vertex types")
        }
    }
    else {
        stop("Not a bipartite graph, supply `types' argument")
    }
    if (!is.null(probe1)) {
        probe1 <- as.igraph.vs(graph, probe1) - 1
    }
    else {
        probe1 <- -1
    }
    which <- switch(igraph.match.arg(which), both = 0L, false = 1L, 
        true = 2L)
    if (which != "both" && probe1 != -1) {
        warning("`probe1' ignored if only one projection is requested")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_bipartite_projection", graph, types, 
        as.integer(probe1), which, PACKAGE = "igraph")
    if (remove.type) {
        if (is_igraph(res[[1]])) {
            res[[1]] <- delete_vertex_attr(res[[1]], "type")
        }
        if (is_igraph(res[[2]])) {
            res[[2]] <- delete_vertex_attr(res[[2]], "type")
        }
    }
    if (which == 0L) {
        if (multiplicity) {
            E(res[[1]])$weight <- res[[3]]
            E(res[[2]])$weight <- res[[4]]
        }
        res[1:2]
    }
    else if (which == 1L) {
        if (multiplicity) {
            E(res[[1]])$weight <- res[[3]]
        }
        res[[1]]
    }
    else {
        if (multiplicity) {
            E(res[[2]])$weight <- res[[4]]
        }
        res[[2]]
    }
}


scgNormEps <- function (V, groups, mtype = c("symmetric", "laplacian", "stochastic"), 
    p = NULL, norm = c("row", "col")) 
{
    V <- as.matrix(structure(as.double(V), dim = dim(V)))
    groups <- as.numeric(groups) - 1
    mtype <- switch(igraph.match.arg(mtype), symmetric = 1, laplacian = 2, 
        stochastic = 3)
    if (!is.null(p)) 
        p <- as.numeric(p)
    norm <- switch(igraph.match.arg(norm), row = 1, col = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_scg_norm_eps", V, groups, mtype, p, 
        norm, PACKAGE = "igraph")
    res
}


connect <- function (graph, order, mode = c("all", "out", "in", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_connect_neighborhood", graph, as.numeric(order), 
        as.numeric(mode), PACKAGE = "igraph")
}


edge.betweenness.estimate <- function (graph, e = E(graph), directed = TRUE, cutoff, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    e <- as.igraph.es(graph, e)
    directed <- as.logical(directed)
    cutoff <- as.numeric(cutoff)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_edge_betweenness_estimate", graph, 
        directed, cutoff, weights, PACKAGE = "igraph")
    res[as.numeric(e)]
}


layout.sugiyama <- function (graph, layers = NULL, hgap = 1, vgap = 1, maxiter = 100, 
    weights = NULL, attributes = c("default", "all", "none")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(layers)) 
        layers <- as.numeric(layers) - 1
    hgap <- as.numeric(hgap)
    vgap <- as.numeric(vgap)
    maxiter <- as.integer(maxiter)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    attributes <- igraph.match.arg(attributes)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_sugiyama", graph, layers, hgap, 
        vgap, maxiter, weights, PACKAGE = "igraph")
    res$res[, 2] <- max(res$res[, 2]) - res$res[, 2] + 1
    vc <- vcount(graph)
    res$layout <- res$res[seq_len(vc), ]
    if (nrow(res$res) == vc) {
        res$layout.dummy <- matrix(nrow = 0, ncol = 2)
    }
    else {
        res$layout.dummy <- res$res[(vc + 1):nrow(res$res), ]
    }
    E(res$extd_graph)$orig <- res$extd_to_orig_eids
    res$extd_to_orig_eids <- NULL
    res$extd_graph <- set_vertex_attr(res$extd_graph, "dummy", 
        value = c(rep(FALSE, vc), rep(TRUE, nrow(res$res) - vc)))
    res$extd_graph$layout <- rbind(res$layout, res$layout.dummy)
    if (attributes == "default" || attributes == "all") {
        if ("size" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$size <- 0
            V(res$extd_graph)$size[!V(res$extd_graph)$dummy] <- V(graph)$size
        }
        if ("size2" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$size2 <- 0
            V(res$extd_graph)$size2[!V(res$extd_graph)$dummy] <- V(graph)$size2
        }
        if ("shape" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$shape <- "none"
            V(res$extd_graph)$shape[!V(res$extd_graph)$dummy] <- V(graph)$shape
        }
        if ("label" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$label <- ""
            V(res$extd_graph)$label[!V(res$extd_graph)$dummy] <- V(graph)$label
        }
        if ("color" %in% vertex_attr_names(graph)) {
            V(res$extd_graph)$color <- head(V(graph)$color, 1)
            V(res$extd_graph)$color[!V(res$extd_graph)$dummy] <- V(graph)$color
        }
        eetar <- as_edgelist(res$extd_graph, names = FALSE)[, 
            2]
        E(res$extd_graph)$arrow.mode <- 0
        if ("arrow.mode" %in% edge_attr_names(graph)) {
            E(res$extd_graph)$arrow.mode[eetar <= vc] <- E(graph)$arrow.mode
        }
        else {
            E(res$extd_graph)$arrow.mode[eetar <= vc] <- is_directed(graph) * 
                2
        }
        if ("arrow.size" %in% edge_attr_names(graph)) {
            E(res$extd_graph)$arrow.size <- 0
            E(res$extd_graph)$arrow.size[eetar <= vc] <- E(graph)$arrow.size
        }
    }
    if (attributes == "all") {
        gatt <- setdiff(graph_attr_names(graph), "layout")
        vatt <- setdiff(vertex_attr_names(graph), c("size", "size2", 
            "shape", "label", "color"))
        eatt <- setdiff(edge_attr_names(graph), c("arrow.mode", 
            "arrow.size"))
        for (ga in gatt) {
            res$extd_graph <- set_graph_attr(res$extd_graph, 
                ga, graph_attr(graph, ga))
        }
        for (va in vatt) {
            notdummy <- which(!V(res$extd_graph)$dummy)
            res$extd_graph <- set_vertex_attr(res$extd_graph, 
                va, notdummy, vertex_attr(graph, va))
        }
        for (ea in eatt) {
            eanew <- edge_attr(graph, ea)[E(res$extd_graph)$orig]
            res$extd_graph <- set_edge_attr(res$extd_graph, ea, 
                value = eanew)
        }
    }
    res$res <- NULL
    res
}


stCuts <- function (graph, source, target) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    source <- as.igraph.vs(graph, source)
    target <- as.igraph.vs(graph, target)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_all_st_cuts", graph, source - 1, target - 
        1, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$cuts)) {
            res$cuts[[i_]] <- create_es(graph, res$cuts[[i_]])
        }
    }
    if (igraph_opt("return.vs.es")) {
        for (i_ in seq_along(res$partition1s)) {
            res$partition1s[[i_]] <- create_vs(graph, res$partition1s[[i_]])
        }
    }
    res
}


read.graph <- function (file, format = c("edgelist", "pajek", "ncol", "lgl", 
    "graphml", "dimacs", "graphdb", "gml", "dl"), ...) 
{
    if (!is.character(file) || length(grep("://", file, fixed = TRUE)) > 
        0 || length(grep("~", file, fixed = TRUE)) > 0) {
        buffer <- read.graph.toraw(file)
        file <- tempfile()
        write.graph.fromraw(buffer, file)
    }
    format <- igraph.match.arg(format)
    res <- switch(format, pajek = read.graph.pajek(file, ...), 
        ncol = read.graph.ncol(file, ...), edgelist = read.graph.edgelist(file, 
            ...), lgl = read.graph.lgl(file, ...), graphml = read.graph.graphml(file, 
            ...), dimacs = read.graph.dimacs(file, ...), graphdb = read.graph.graphdb(file, 
            ...), gml = read.graph.gml(file, ...), dl = read.graph.dl(file, 
            ...), stop(paste("Unknown file format:", format)))
    res
}


cluster_label_prop <- function (graph, weights = NULL, initial = NULL, fixed = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (!is.null(initial)) 
        initial <- as.numeric(initial)
    if (!is.null(fixed)) 
        fixed <- as.logical(fixed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_label_propagation", graph, 
        weights, initial, fixed, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "label propagation"
    res$membership <- res$membership + 1
    class(res) <- "communities"
    res
}


graph.density <- function (graph, loops = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_density", graph, as.logical(loops), PACKAGE = "igraph")
}


graph.star <- function (n, mode = c("in", "out", "mutual", "undirected"), center = 1) 
{
    mode <- igraph.match.arg(mode)
    mode1 <- switch(mode, out = 0, `in` = 1, undirected = 2, 
        mutual = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_star", as.numeric(n), as.numeric(mode1), 
        as.numeric(center) - 1, PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- switch(mode, `in` = "In-star", out = "Out-star", 
            "Star")
        res$mode <- mode
        res$center <- center
    }
    res
}


ivs <- function (graph, min = NULL, max = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(min)) {
        min <- 0
    }
    if (is.null(max)) {
        max <- 0
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_independent_vertex_sets", graph, as.numeric(min), 
        as.numeric(max), PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


components <- function (graph, mode = c("weak", "strong")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), weak = 1, strong = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_clusters", graph, mode, PACKAGE = "igraph")
    res$membership <- res$membership + 1
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$membership) <- V(graph)$name
    }
    res
}


tk_set_coords <- function (tkp.id, coords) 
{
    stopifnot(is.matrix(coords), ncol(coords) == 2)
    .tkplot.set(tkp.id, "coords", coords)
    .tkplot.update.vertices(tkp.id)
    invisible(NULL)
}


path <- function (...) 
{
    structure(list(...), class = "igraph.path")
}


transitivity <- function (graph, type = c("undirected", "global", "globalundirected", 
    "localundirected", "local", "average", "localaverage", "localaverageundirected", 
    "barrat", "weighted"), vids = NULL, weights = NULL, isolates = c("NaN", 
    "zero")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    type <- igraph.match.arg(type)
    type <- switch(type, undirected = 0, global = 0, globalundirected = 0, 
        localundirected = 1, local = 1, average = 2, localaverage = 2, 
        localaverageundirected = 2, barrat = 3, weighted = 3)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    isolates <- igraph.match.arg(isolates)
    isolates <- as.double(switch(isolates, nan = 0, zero = 1))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (type == 0) {
        .Call("R_igraph_transitivity_undirected", graph, isolates, 
            PACKAGE = "igraph")
    }
    else if (type == 1) {
        if (is.null(vids)) {
            .Call("R_igraph_transitivity_local_undirected_all", 
                graph, isolates, PACKAGE = "igraph")
        }
        else {
            vids <- as.igraph.vs(graph, vids) - 1
            .Call("R_igraph_transitivity_local_undirected", graph, 
                vids, isolates, PACKAGE = "igraph")
        }
    }
    else if (type == 2) {
        .Call("R_igraph_transitivity_avglocal_undirected", graph, 
            isolates, PACKAGE = "igraph")
    }
    else if (type == 3) {
        if (is.null(vids)) {
            vids <- V(graph)
        }
        vids <- as.igraph.vs(graph, vids) - 1
        if (is.null(weights)) {
            .Call("R_igraph_transitivity_local_undirected", graph, 
                vids, isolates, PACKAGE = "igraph")
        }
        else {
            .Call("R_igraph_transitivity_barrat", graph, vids, 
                weights, isolates, PACKAGE = "igraph")
        }
    }
}


subgraph.centrality <- function (graph, diag = FALSE) 
{
    A <- as_adj(graph)
    if (!diag) {
        diag(A) <- 0
    }
    eig <- eigen(A)
    res <- as.vector(eig$vectors^2 %*% exp(eig$values))
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name")
    }
    res
}


incident_edges <- function (graph, v, mode = c("out", "in", "all", "total")) 
{
    if (!is_igraph(graph)) 
        stop("Not a graph object")
    vv <- as.igraph.vs(graph, v) - 1
    mode <- switch(match.arg(mode), out = 1, `in` = 2, all = 3, 
        total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_incident_edges", graph, vv, mode, 
        PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, function(x) create_es(graph, x + 1))
    }
    if (is_named(graph)) 
        names(res) <- V(graph)$name[vv + 1]
    res
}


as_ids <- function (seq) 
UseMethod("as_ids")


graph.subisomorphic.lad <- function (pattern, target, domains = NULL, induced = FALSE, map = TRUE, 
    all.maps = FALSE, time.limit = Inf) 
{
    if (!is_igraph(pattern)) {
        stop("Not a graph object")
    }
    if (!is_igraph(target)) {
        stop("Not a graph object")
    }
    induced <- as.logical(induced)
    if (time.limit == Inf) {
        time.limit <- 0L
    }
    else {
        time.limit <- as.integer(time.limit)
    }
    map <- as.logical(map)
    all.maps <- as.logical(all.maps)
    if (!is.null(domains)) {
        if (!is.list(domains)) {
            stop("`domains' must be a list of vertex vectors from `target'")
        }
        if (length(domains) != vcount(pattern)) {
            stop("`domains' length and `pattern' number of vertices must match")
        }
        domains <- lapply(domains, function(x) as.igraph.vs(target, 
            x) - 1)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_subisomorphic_lad", pattern, target, 
        domains, induced, time.limit, map, all.maps, PACKAGE = "igraph")
    if (map) {
        res$map <- res$map + 1
        if (igraph_opt("add.vertex.names") && is_named(target)) {
            names(res$map) <- V(target)$name[res$map]
        }
    }
    if (all.maps) 
        res$maps <- lapply(res$maps, function(x) V(target)[x + 
            1])
    res
}


sample_ <- function (...) 
{
    me <- attr(sys.function(), "name") %||% "construct"
    args <- list(...)
    cidx <- vapply(args, inherits, TRUE, what = "igraph_constructor_spec")
    if (sum(cidx) == 0) {
        stop("Don't know how to ", me, ", nothing given")
    }
    if (sum(cidx) > 1) {
        stop("Don't know how to ", me, ", multiple constructors given")
    }
    cons <- args[cidx][[1]]
    args <- args[!cidx]
    wmods <- vapply(args, class, "") == "igraph_constructor_modifier"
    mods <- args[wmods]
    args <- args[!wmods]
    args2 <- if (cons$lazy) 
        lapply(cons$args, "[[", "expr")
    else lazy_eval(cons$args)
    res <- do_call(cons$fun, args2, args)
    for (m in mods) {
        if (m$id == "without_attr") {
            ga <- graph_attr_names(res)
            va <- vertex_attr_names(res)
            ea <- edge_attr_names(res)
            for (g in ga) res <- delete_graph_attr(res, g)
            for (v in va) res <- delete_vertex_attr(res, v)
            for (e in ea) res <- delete_edge_attr(res, e)
        }
        else if (m$id == "without_loops") {
            res <- simplify(res, remove.loops = TRUE, remove.multiple = FALSE)
        }
        else if (m$id == "without_multiples") {
            res <- simplify(res, remove.loops = FALSE, remove.multiple = TRUE)
        }
        else if (m$id == "simplified") {
            res <- simplify(res)
        }
        else if (m$id == "with_vertex_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_vertex_attr(res, n, value = v)
            }
        }
        else if (m$id == "with_edge_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_edge_attr(res, n, value = v)
            }
        }
        else if (m$id == "with_graph_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_graph_attr(res, n, value = v)
            }
        }
    }
    res
}


sample_fitness <- function (no.of.edges, fitness.out, fitness.in = NULL, loops = FALSE, 
    multiple = FALSE) 
{
    no.of.edges <- as.integer(no.of.edges)
    fitness.out <- as.numeric(fitness.out)
    if (!is.null(fitness.in)) 
        fitness.in <- as.numeric(fitness.in)
    loops <- as.logical(loops)
    multiple <- as.logical(multiple)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_static_fitness_game", no.of.edges, 
        fitness.out, fitness.in, loops, multiple, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Static fitness model")
    res <- set.graph.attribute(res, "loops", loops)
    res <- set.graph.attribute(res, "multiple", multiple)
    res
}


distances <- function (graph, v = V(graph), to = V(graph), mode = c("all", 
    "out", "in"), weights = NULL, algorithm = c("automatic", 
    "unweighted", "dijkstra", "bellman-ford", "johnson")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    v <- as.igraph.vs(graph, v)
    to <- as.igraph.vs(graph, to)
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    algorithm <- igraph.match.arg(algorithm)
    algorithm <- switch(algorithm, automatic = 0, unweighted = 1, 
        dijkstra = 2, `bellman-ford` = 3, johnson = 4)
    if (is.null(weights)) {
        if ("weight" %in% edge_attr_names(graph)) {
            weights <- as.numeric(E(graph)$weight)
        }
    }
    else {
        if (length(weights) == 1 && is.na(weights)) {
            weights <- NULL
        }
        else {
            weights <- as.numeric(weights)
        }
    }
    if (!is.null(weights) && algorithm == 1) {
        weights <- NULL
        warning("Unweighted algorithm chosen, weights ignored")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_shortest_paths", graph, v - 1, to - 
        1, as.numeric(mode), weights, as.numeric(algorithm), 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        rownames(res) <- V(graph)$name[v]
        colnames(res) <- V(graph)$name[to]
    }
    res
}


sample_motifs <- function (graph, size = 3, cut.prob = rep(0, size), sample.size = vcount(graph)/10, 
    sample = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cut.prob <- as.numeric(cut.prob)
    if (length(cut.prob) != size) {
        cut.prob <- c(cut.prob[-length(cut.prob)], rep(cut.prob[-length(cut.prob)], 
            length(cut.prob) - 1))
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_motifs_randesu_estimate", graph, as.integer(size), 
        as.numeric(cut.prob), as.integer(sample.size), as.numeric(sample), 
        PACKAGE = "igraph")
}


graph.ring <- function (n, directed = FALSE, mutual = FALSE, circular = TRUE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_ring", as.numeric(n), as.logical(directed), 
        as.logical(mutual), as.logical(circular), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Ring graph"
        res$mutual <- mutual
        res$circular <- circular
    }
    res
}


graph.isomorphic.bliss <- function (graph1, graph2, sh1 = "fm", sh2 = "fm") 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    sh1 <- switch(igraph.match.arg(sh1), f = 0, fl = 1, fs = 2, 
        fm = 3, flm = 4, fsm = 5)
    sh2 <- switch(igraph.match.arg(sh2), f = 0, fl = 1, fs = 2, 
        fm = 3, flm = 4, fsm = 5)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_isomorphic_bliss", graph1, graph2, 
        sh1, sh2, PACKAGE = "igraph")
    res
}


is_weighted <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    "weight" %in% edge_attr_names(graph)
}


as_adj_edge_list <- function (graph, mode = c("all", "out", "in", "total")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- as.numeric(switch(mode, out = 1, `in` = 2, all = 3, 
        total = 3))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_adjedgelist", graph, mode, PACKAGE = "igraph")
    res <- lapply(res, function(x) E(graph)[x + 1])
    if (is_named(graph)) 
        names(res) <- V(graph)$name
    res
}


graph.count.isomorphisms.vf2 <- function (graph1, graph2, vertex.color1, vertex.color2, edge.color1, 
    edge.color2) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    if (missing(vertex.color1)) {
        if ("color" %in% vertex_attr_names(graph1)) {
            vertex.color1 <- V(graph1)$color
        }
        else {
            vertex.color1 <- NULL
        }
    }
    if (!is.null(vertex.color1)) {
        vertex.color1 <- as.integer(vertex.color1) - 1L
    }
    if (missing(vertex.color2)) {
        if ("color" %in% vertex_attr_names(graph2)) {
            vertex.color2 <- V(graph2)$color
        }
        else {
            vertex.color2 <- NULL
        }
    }
    if (!is.null(vertex.color2)) {
        vertex.color2 <- as.integer(vertex.color2) - 1L
    }
    if (missing(edge.color1)) {
        if ("color" %in% edge_attr_names(graph1)) {
            edge.color1 <- E(graph1)$color
        }
        else {
            edge.color1 <- NULL
        }
    }
    if (!is.null(edge.color1)) {
        edge.color1 <- as.integer(edge.color1) - 1L
    }
    if (missing(edge.color2)) {
        if ("color" %in% edge_attr_names(graph2)) {
            edge.color2 <- E(graph2)$color
        }
        else {
            edge.color2 <- NULL
        }
    }
    if (!is.null(edge.color2)) {
        edge.color2 <- as.integer(edge.color2) - 1L
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_count_isomorphisms_vf2", graph1, graph2, 
        vertex.color1, vertex.color2, edge.color1, edge.color2, 
        PACKAGE = "igraph")
    res
}


cut_at <- function (communities, no, steps) 
{
    if (!inherits(communities, "communities")) {
        stop("Not a community structure")
    }
    if (!is_hierarchical(communities)) {
        stop("Not a hierarchical communitity structure")
    }
    if ((!missing(no) && !missing(steps)) || (missing(no) && 
        missing(steps))) {
        stop("Please give either `no' or `steps' (but not both)")
    }
    if (!missing(steps)) {
        mm <- merges(communities)
        if (steps > nrow(mm)) {
            warning("Cannot make that many steps")
            steps <- nrow(mm)
        }
        community.to.membership2(mm, communities$vcount, steps)
    }
    else {
        mm <- merges(communities)
        noc <- communities$vcount - nrow(mm)
        if (no < noc) {
            warning("Cannot have that few communities")
            no = noc
        }
        steps <- communities$vcount - no
        community.to.membership2(mm, communities$vcount, steps)
    }
}


page.rank.old <- function (graph, vids = V(graph), directed = TRUE, niter = 1000, 
    eps = 0.001, damping = 0.85, old = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    directed <- as.logical(directed)
    niter <- as.integer(niter)
    eps <- as.numeric(eps)
    damping <- as.numeric(damping)
    old <- as.logical(old)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_pagerank_old", graph, vids - 1, directed, 
        niter, eps, damping, old, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


`%<-%` <- function (t, value) 
{
    from <- get(".igraph.from", parent.frame())
    to <- get(".igraph.to", parent.frame())
    graph <- get(".igraph.graph", parent.frame())
    value <- as.igraph.vs(graph, value) - 1
    t <- as.igraph.vs(graph, t) - 1
    if (is_directed(graph)) {
        from %in% value & to %in% t
    }
    else {
        (from %in% value & to %in% t) | (to %in% value & from %in% 
            t)
    }
}


graph.disjoint.union <- function (...) 
{
    graphs <- unlist(recursive = FALSE, lapply(list(...), function(l) {
        if (is_igraph(l)) 
            list(l)
        else l
    }))
    if (!all(sapply(graphs, is_igraph))) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_disjoint_union", graphs, PACKAGE = "igraph")
    graph.attributes(res) <- rename.attr.if.needed("g", graphs)
    attr <- list()
    vc <- sapply(graphs, vcount)
    cumvc <- c(0, cumsum(vc))
    for (i in seq_along(graphs)) {
        va <- vertex.attributes(graphs[[i]])
        exattr <- intersect(names(va), names(attr))
        noattr <- setdiff(names(attr), names(va))
        newattr <- setdiff(names(va), names(attr))
        for (a in seq_along(exattr)) {
            attr[[exattr[a]]] <- c(attr[[exattr[a]]], va[[exattr[a]]])
        }
        for (a in seq_along(noattr)) {
            attr[[noattr[a]]] <- c(attr[[noattr[a]]], rep(NA, 
                vc[i]))
        }
        for (a in seq_along(newattr)) {
            attr[[newattr[a]]] <- c(rep(NA, cumvc[i]), va[[newattr[a]]])
        }
    }
    vertex.attributes(res) <- attr
    if ("name" %in% names(attr) && any(duplicated(attr$name))) {
        warning("Duplicate vertex names in disjoint union")
    }
    attr <- list()
    ec <- sapply(graphs, ecount)
    cumec <- c(0, cumsum(ec))
    for (i in seq_along(graphs)) {
        ea <- edge.attributes(graphs[[i]])
        exattr <- intersect(names(ea), names(attr))
        noattr <- setdiff(names(attr), names(ea))
        newattr <- setdiff(names(ea), names(attr))
        for (a in seq_along(exattr)) {
            attr[[exattr[a]]] <- c(attr[[exattr[a]]], ea[[exattr[a]]])
        }
        for (a in seq_along(noattr)) {
            attr[[noattr[a]]] <- c(attr[[noattr[a]]], rep(NA, 
                ec[i]))
        }
        for (a in seq_along(newattr)) {
            attr[[newattr[a]]] <- c(rep(NA, cumec[i]), ea[[newattr[a]]])
        }
    }
    edge.attributes(res) <- attr
    res
}


with_kk <- function (...) 
layout_spec(layout_with_kk, ...)


list.graph.attributes <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    res <- base::.Call("R_igraph_mybracket2_names", graph, 9L, 
        2L, PACKAGE = "igraph")
    if (is.null(res)) {
        res <- character()
    }
    res
}


get.vertex.attribute <- function (graph, name, index = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (missing(name)) {
        if (missing(index)) {
            vertex.attributes(graph)
        }
        else {
            vertex.attributes(graph, index = index)
        }
    }
    else {
        myattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 
            3L, PACKAGE = "igraph")[[as.character(name)]]
        if (!missing(index)) {
            index <- as.igraph.vs(graph, index)
            myattr <- myattr[index]
        }
        myattr
    }
}


layout.gem <- function (graph, coords = NULL, maxiter = 40 * vcount(graph)^2, 
    temp.max = vcount(graph), temp.min = 1/10, temp.init = sqrt(vcount(graph))) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!is.null(coords)) {
        coords <- as.matrix(structure(as.double(coords), dim = dim(coords)))
        use.seed <- TRUE
    }
    else {
        coords <- matrix(ncol = 2, nrow = 0)
        use.seed <- FALSE
    }
    maxiter <- as.integer(maxiter)
    temp.max <- as.numeric(temp.max)
    temp.min <- as.numeric(temp.min)
    temp.init <- as.numeric(temp.init)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_gem", graph, coords, use.seed, 
        maxiter, temp.max, temp.min, temp.init, PACKAGE = "igraph")
    res
}


estimate_betweenness <- function (graph, vids = V(graph), directed = TRUE, cutoff, weights = NULL, 
    nobigint = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    directed <- as.logical(directed)
    cutoff <- as.numeric(cutoff)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    nobigint <- as.logical(nobigint)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_betweenness_estimate", graph, vids - 
        1, directed, cutoff, weights, nobigint, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


directed_graph <- function (...) 
constructor_spec(make_directed_graph, ...)


neighborhood <- function (graph, order, nodes = V(graph), mode = c("all", "out", 
    "in"), mindist = 0) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    mindist <- as.integer(mindist)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_neighborhood", graph, as.igraph.vs(graph, 
        nodes) - 1, as.numeric(order), as.numeric(mode), mindist, 
        PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


line_graph <- function (...) 
constructor_spec(make_line_graph, ...)


similarity.dice <- function (graph, vids = V(graph), mode = c("all", "out", "in", 
    "total"), loops = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    loops <- as.logical(loops)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_similarity_dice", graph, vids - 1, 
        mode, loops, PACKAGE = "igraph")
    res
}


`%>%` <- magrittr::`%>%` # re-exported from magrittr package

static.fitness.game <- function (no.of.edges, fitness.out, fitness.in = NULL, loops = FALSE, 
    multiple = FALSE) 
{
    no.of.edges <- as.integer(no.of.edges)
    fitness.out <- as.numeric(fitness.out)
    if (!is.null(fitness.in)) 
        fitness.in <- as.numeric(fitness.in)
    loops <- as.logical(loops)
    multiple <- as.logical(multiple)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_static_fitness_game", no.of.edges, 
        fitness.out, fitness.in, loops, multiple, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Static fitness model")
    res <- set.graph.attribute(res, "loops", loops)
    res <- set.graph.attribute(res, "multiple", multiple)
    res
}


betweenness <- function (graph, v = V(graph), directed = TRUE, weights = NULL, 
    nobigint = TRUE, normalized = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    v <- as.igraph.vs(graph, v)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_betweenness", graph, v - 1, as.logical(directed), 
        weights, as.logical(nobigint), PACKAGE = "igraph")
    if (normalized) {
        vc <- vcount(graph)
        if (is_directed(graph) && directed) {
            res <- res/(vc * vc - 3 * vc + 2)
        }
        else {
            res <- 2 * res/(vc * vc - 3 * vc + 2)
        }
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- V(graph)$name[v]
    }
    res
}


graph_from_graphnel <- function (graphNEL, name = TRUE, weight = TRUE, unlist.attrs = TRUE) 
{
    if (!inherits(graphNEL, "graphNEL")) {
        stop("Not a graphNEL graph")
    }
    al <- lapply(graph::edgeL(graphNEL), "[[", "edges")
    if (graph::edgemode(graphNEL) == "undirected") {
        al <- mapply(SIMPLIFY = FALSE, seq_along(al), al, FUN = function(n, 
            l) {
            c(l, rep(n, sum(l == n)))
        })
    }
    mode <- if (graph::edgemode(graphNEL) == "directed") 
        "out"
    else "all"
    g <- graph_from_adj_list(al, mode = mode, duplicate = TRUE)
    if (name) {
        V(g)$name <- graph::nodes(graphNEL)
    }
    g.n <- names(graphNEL@graphData)
    g.n <- g.n[g.n != "edgemode"]
    for (n in g.n) {
        g <- set_graph_attr(g, n, graphNEL@graphData[[n]])
    }
    v.n <- names(graph::nodeDataDefaults(graphNEL))
    for (n in v.n) {
        val <- unname(graph::nodeData(graphNEL, attr = n))
        if (unlist.attrs && all(sapply(val, length) == 1)) {
            val <- unlist(val)
        }
        g <- set_vertex_attr(g, n, value = val)
    }
    e.n <- names(graph::edgeDataDefaults(graphNEL))
    if (!weight) {
        e.n <- e.n[e.n != "weight"]
    }
    if (length(e.n) > 0) {
        el <- as_edgelist(g)
        el <- paste(sep = "|", el[, 1], el[, 2])
        for (n in e.n) {
            val <- unname(graph::edgeData(graphNEL, attr = n)[el])
            if (unlist.attrs && all(sapply(val, length) == 1)) {
                val <- unlist(val)
            }
            g <- set_edge_attr(g, n, value = val)
        }
    }
    g
}


compose <- function (g1, g2, byname = "auto") 
{
    if (!is_igraph(g1) || !is_igraph(g2)) {
        stop("Not a graph object")
    }
    if (byname != "auto" && !is.logical(byname)) {
        stop("`byname' must be \"auto\", or logical")
    }
    nonamed <- is_named(g1) + is_named(g2)
    if (byname == "auto") {
        byname <- nonamed == 2
        if (nonamed == 1) {
            warning("One, but not both graphs are named, not using vertex names")
        }
    }
    else if (byname && nonamed != 2) {
        stop("Some graphs are not named")
    }
    if (byname) {
        uninames <- unique(c(V(g1)$name, V(g2)$name))
        if (vcount(g1) < length(uninames)) {
            g1 <- g1 + setdiff(uninames, V(g1)$name)
        }
        if (vcount(g2) < length(uninames)) {
            g2 <- g2 + setdiff(uninames, V(g2)$name)
        }
        if (any(uninames != V(g1)$name)) {
            g1 <- permute(g1, match(V(g1)$name, uninames))
        }
        if (any(uninames != V(g2)$name)) {
            g2 <- permute(g2, match(V(g2)$name, uninames))
        }
    }
    edgemaps <- (length(edge_attr_names(g1)) != 0 || length(edge_attr_names(g2)) != 
        0)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_compose", g1, g2, edgemaps, PACKAGE = "igraph")
    maps <- list(res$edge_map1, res$edge_map2)
    res <- res$graph
    graphs <- list(g1, g2)
    graph.attributes(res) <- rename.attr.if.needed("g", graphs)
    if (byname) {
        vertex.attributes(res) <- rename.attr.if.needed("v", 
            graphs, vcount(res), ignore = "name")
        V(res)$name <- uninames
    }
    else {
        vertex.attributes(res) <- rename.attr.if.needed("v", 
            graphs, vcount(res))
    }
    if (edgemaps) {
        edge.attributes(res) <- rename.attr.if.needed("e", graphs, 
            ecount(res), maps2 = maps)
    }
    res
}


delete_graph_attr <- function (graph, name) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    name <- as.character(name)
    if (!name %in% graph_attr_names(graph)) {
        stop("No such graph attribute: ", name)
    }
    gattr <- base::.Call("R_igraph_mybracket2", graph, 9L, 2L, 
        PACKAGE = "igraph")
    gattr[[name]] <- NULL
    base::.Call("R_igraph_mybracket2_set", graph, 9L, 2L, gattr, 
        PACKAGE = "igraph")
}


topo_sort <- function (graph, mode = c("out", "all", "in")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_topological_sorting", graph, as.numeric(mode), 
        PACKAGE = "igraph") + 1L
    if (igraph_opt("return.vs.es")) 
        res <- create_vs(graph, res)
    res
}


assortativity_nominal <- function (graph, types, directed = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    types <- as.numeric(types) - 1
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_assortativity_nominal", graph, types, 
        directed, PACKAGE = "igraph")
    res
}


convex.hull <- function (data) 
{
    data <- as.matrix(structure(as.double(data), dim = dim(data)))
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_convex_hull", data, PACKAGE = "igraph")
    res
}


sample_smallworld <- function (dim, size, nei, p, loops = FALSE, multiple = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_watts_strogatz_game", as.numeric(dim), 
        as.numeric(size), as.numeric(nei), as.numeric(p), as.logical(loops), 
        as.logical(multiple), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Watts-Strogatz random graph"
        res$dim <- dim
        res$size <- size
        res$nei <- nei
        res$p <- p
        res$loops <- loops
        res$multiple <- multiple
    }
    res
}


clique_num <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_clique_number", graph, PACKAGE = "igraph")
}


graph.get.isomorphisms.vf2 <- function (graph1, graph2, vertex.color1, vertex.color2, edge.color1, 
    edge.color2) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    if (missing(vertex.color1)) {
        if ("color" %in% vertex_attr_names(graph1)) {
            vertex.color1 <- V(graph1)$color
        }
        else {
            vertex.color1 <- NULL
        }
    }
    if (!is.null(vertex.color1)) {
        vertex.color1 <- as.integer(vertex.color1) - 1L
    }
    if (missing(vertex.color2)) {
        if ("color" %in% vertex_attr_names(graph2)) {
            vertex.color2 <- V(graph2)$color
        }
        else {
            vertex.color2 <- NULL
        }
    }
    if (!is.null(vertex.color2)) {
        vertex.color2 <- as.integer(vertex.color2) - 1L
    }
    if (missing(edge.color1)) {
        if ("color" %in% edge_attr_names(graph1)) {
            edge.color1 <- E(graph1)$color
        }
        else {
            edge.color1 <- NULL
        }
    }
    if (!is.null(edge.color1)) {
        edge.color1 <- as.integer(edge.color1) - 1L
    }
    if (missing(edge.color2)) {
        if ("color" %in% edge_attr_names(graph2)) {
            edge.color2 <- E(graph2)$color
        }
        else {
            edge.color2 <- NULL
        }
    }
    if (!is.null(edge.color2)) {
        edge.color2 <- as.integer(edge.color2) - 1L
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_isomorphisms_vf2", graph1, graph2, 
        vertex.color1, vertex.color2, edge.color1, edge.color2, 
        PACKAGE = "igraph")
    lapply(res, function(x) V(graph2)[x + 1])
}


component_distribution <- function (graph, cumulative = FALSE, mul.size = FALSE, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    cs <- components(graph, ...)$csize
    hi <- hist(cs, -1:max(cs), plot = FALSE)$density
    if (mul.size) {
        hi <- hi * 1:max(cs)
        hi <- hi/sum(hi)
    }
    if (!cumulative) {
        res <- hi
    }
    else {
        res <- rev(cumsum(rev(hi)))
    }
    res
}


exportPajek <- function (blocks, graph, file, project.file = TRUE) 
{
    if (!project.file && !is.character(file)) {
        stop(paste("`file' must be a filename (without extension) when writing", 
            "to separate files"))
    }
    if (project.file) {
        return(exportPajek.cohesiveblocks.pf(blocks, graph, file))
    }
    else {
        return(exportPajek.cohesiveblocks.nopf(blocks, graph, 
            file))
    }
}


edge.betweenness.community <- function (graph, weights = E(graph)$weight, directed = TRUE, 
    edge.betweenness = TRUE, merges = TRUE, bridges = TRUE, modularity = TRUE, 
    membership = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object!")
    }
    if (!is.null(weights)) {
        weights <- as.numeric(weights)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_edge_betweenness", graph, 
        weights, as.logical(directed), as.logical(edge.betweenness), 
        as.logical(merges), as.logical(bridges), as.logical(modularity), 
        as.logical(membership), PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "edge betweenness"
    res$membership <- res$membership + 1
    res$merges <- res$merges + 1
    res$removed.edges <- res$removed.edges + 1
    res$bridges <- res$bridges + 1
    class(res) <- "communities"
    res
}


sample_sbm <- function (n, pref.matrix, block.sizes, directed = FALSE, loops = FALSE) 
{
    n <- as.integer(n)
    pref.matrix <- as.matrix(structure(as.double(pref.matrix), 
        dim = dim(pref.matrix)))
    block.sizes <- as.integer(block.sizes)
    directed <- as.logical(directed)
    loops <- as.logical(loops)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_sbm_game", n, pref.matrix, block.sizes, 
        directed, loops, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Stochastic block-model")
    res <- set.graph.attribute(res, "loops", loops)
    res
}


layout_as_star <- function (graph, center = V(graph)[1], order = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    center <- as.igraph.vs(graph, center)
    if (!is.null(order)) 
        order <- as.numeric(order) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_star", graph, center - 1, order, 
        PACKAGE = "igraph")
    res
}


cluster_leading_eigen <- function (graph, steps = -1, weights = NULL, start = NULL, options = arpack_defaults, 
    callback = NULL, extra = NULL, env = parent.frame()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    steps <- as.integer(steps)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (!is.null(start)) {
        start <- as.numeric(start) - 1
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_leading_eigenvector", graph, 
        steps, weights, options, start, callback, extra, env, 
        environment(igraph.i.levc.arp), PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$algorithm <- "leading eigenvector"
    res$vcount <- vcount(graph)
    res$membership <- res$membership + 1
    res$merges <- res$merges + 1
    res$history <- res$history + 1
    class(res) <- "communities"
    res
}


r_pal <- function (n) 
{
    x <- palette()
    if (n > length(x)) 
        warning("Cannot make ", n, " divergent colors")
    n <- min(n, length(x))
    if (n == 0) 
        character()
    else x[[n]]
}


on_grid <- function (...) 
layout_spec(layout_on_grid, ...)


centr_betw_tmax <- function (graph = NULL, nodes = 0, directed = TRUE) 
{
    if (!is.null(graph) && !is_igraph(graph)) {
        stop("Not a graph object")
    }
    nodes <- as.integer(nodes)
    directed <- as.logical(directed)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_betweenness_tmax", 
        graph, nodes, directed, PACKAGE = "igraph")
    res
}


add_layout_ <- function (graph, ..., overwrite = TRUE) 
{
    if (overwrite && "layout" %in% graph_attr_names(graph)) {
        graph <- delete_graph_attr(graph, "layout")
    }
    graph$layout <- layout_(graph, ...)
    graph
}


contract <- function (graph, mapping, vertex.attr.comb = igraph_opt("vertex.attr.comb")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mapping <- as.numeric(mapping) - 1
    vertex.attr.comb <- igraph.i.attribute.combination(vertex.attr.comb)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_contract_vertices", graph, mapping, 
        vertex.attr.comb, PACKAGE = "igraph")
    res
}


shape_noplot <- function (coords, v = NULL, params) 
{
    invisible(NULL)
}


graph.mincut <- function (graph, source = NULL, target = NULL, capacity = NULL, 
    value.only = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(capacity)) {
        if ("capacity" %in% edge_attr_names(graph)) {
            capacity <- E(graph)$capacity
        }
    }
    if (is.null(source) && !is.null(target) || is.null(target) && 
        !is.null(source)) {
        stop("Please give both source and target or neither")
    }
    if (!is.null(capacity)) {
        capacity <- as.numeric(capacity)
    }
    value.only <- as.logical(value.only)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (is.null(target) && is.null(source)) {
        if (value.only) {
            res <- .Call("R_igraph_mincut_value", graph, capacity, 
                PACKAGE = "igraph")
        }
        else {
            res <- .Call("R_igraph_mincut", graph, capacity, 
                PACKAGE = "igraph")
            res$cut <- res$cut + 1
            res$partition1 <- res$partition1 + 1
            res$partition2 <- res$partition2 + 1
            if (igraph_opt("return.vs.es")) {
                res$cut <- create_es(graph, res$cut)
                res$partition1 <- create_vs(graph, res$partition1)
                res$partition2 <- create_vs(graph, res$partition2)
            }
            res
        }
    }
    else {
        if (value.only) {
            res <- .Call("R_igraph_st_mincut_value", graph, as.igraph.vs(graph, 
                source) - 1, as.igraph.vs(graph, target) - 1, 
                capacity, PACKAGE = "igraph")
        }
        else {
            stop("Calculating minimum s-t cuts is not implemented yet")
        }
    }
    res
}


page_rank_old <- function (graph, vids = V(graph), directed = TRUE, niter = 1000, 
    eps = 0.001, damping = 0.85, old = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    directed <- as.logical(directed)
    niter <- as.integer(niter)
    eps <- as.numeric(eps)
    damping <- as.numeric(damping)
    old <- as.logical(old)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_pagerank_old", graph, vids - 1, directed, 
        niter, eps, damping, old, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


graph.isomorphic.34 <- function (graph1, graph2) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_isomorphic_34", graph1, graph2, PACKAGE = "igraph")
    res
}


igraph.drl.coarsest <- structure(list(edge.cut = 0.8, init.iterations = 0, init.temperature = 2000, 
    init.attraction = 10, init.damping.mult = 1, liquid.iterations = 200, 
    liquid.temperature = 2000, liquid.attraction = 2, liquid.damping.mult = 1, 
    expansion.iterations = 200, expansion.temperature = 2000, 
    expansion.attraction = 10, expansion.damping.mult = 1, cooldown.iterations = 200, 
    cooldown.temperature = 2000, cooldown.attraction = 1, cooldown.damping.mult = 0.1, 
    crunch.iterations = 200, crunch.temperature = 250, crunch.attraction = 1, 
    crunch.damping.mult = 0.25, simmer.iterations = 100, simmer.temperature = 250, 
    simmer.attraction = 0.5, simmer.damping.mult = 0), .Names = c("edge.cut", 
"init.iterations", "init.temperature", "init.attraction", "init.damping.mult", 
"liquid.iterations", "liquid.temperature", "liquid.attraction", 
"liquid.damping.mult", "expansion.iterations", "expansion.temperature", 
"expansion.attraction", "expansion.damping.mult", "cooldown.iterations", 
"cooldown.temperature", "cooldown.attraction", "cooldown.damping.mult", 
"crunch.iterations", "crunch.temperature", "crunch.attraction", 
"crunch.damping.mult", "simmer.iterations", "simmer.temperature", 
"simmer.attraction", "simmer.damping.mult"))


sample_sphere_volume <- function (dim, n = 1, radius = 1, positive = TRUE) 
{
    dim <- as.integer(dim)
    n <- as.integer(n)
    radius <- as.numeric(radius)
    positive <- as.logical(positive)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_sample_sphere_volume", dim, n, radius, 
        positive, PACKAGE = "igraph")
    res
}


maxcohesion <- function (blocks) 
{
    res <- numeric(blocks$vcount)
    myb <- blocks(blocks)
    coh <- cohesion(blocks)
    oo <- order(coh)
    myb <- myb[oo]
    coh <- coh[oo]
    for (b in seq_along(myb)) {
        res[myb[[b]]] <- coh[b]
    }
    res
}


time_bins <- function (x, middle = TRUE) 
UseMethod("time_bins")


make_ <- function (...) 
{
    me <- attr(sys.function(), "name") %||% "construct"
    args <- list(...)
    cidx <- vapply(args, inherits, TRUE, what = "igraph_constructor_spec")
    if (sum(cidx) == 0) {
        stop("Don't know how to ", me, ", nothing given")
    }
    if (sum(cidx) > 1) {
        stop("Don't know how to ", me, ", multiple constructors given")
    }
    cons <- args[cidx][[1]]
    args <- args[!cidx]
    wmods <- vapply(args, class, "") == "igraph_constructor_modifier"
    mods <- args[wmods]
    args <- args[!wmods]
    args2 <- if (cons$lazy) 
        lapply(cons$args, "[[", "expr")
    else lazy_eval(cons$args)
    res <- do_call(cons$fun, args2, args)
    for (m in mods) {
        if (m$id == "without_attr") {
            ga <- graph_attr_names(res)
            va <- vertex_attr_names(res)
            ea <- edge_attr_names(res)
            for (g in ga) res <- delete_graph_attr(res, g)
            for (v in va) res <- delete_vertex_attr(res, v)
            for (e in ea) res <- delete_edge_attr(res, e)
        }
        else if (m$id == "without_loops") {
            res <- simplify(res, remove.loops = TRUE, remove.multiple = FALSE)
        }
        else if (m$id == "without_multiples") {
            res <- simplify(res, remove.loops = FALSE, remove.multiple = TRUE)
        }
        else if (m$id == "simplified") {
            res <- simplify(res)
        }
        else if (m$id == "with_vertex_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_vertex_attr(res, n, value = v)
            }
        }
        else if (m$id == "with_edge_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_edge_attr(res, n, value = v)
            }
        }
        else if (m$id == "with_graph_") {
            m$args <- lapply(m$args, eval)
            for (a in seq_along(m$args)) {
                n <- names(m$args)[a]
                v <- m$args[[a]]
                stopifnot(!is.null(n))
                res <- set_graph_attr(res, n, value = v)
            }
        }
    }
    res
}


subgraph.edges <- function (graph, eids, delete.vertices = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    eids <- as.igraph.es(graph, eids)
    delete.vertices <- as.logical(delete.vertices)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_subgraph_edges", graph, eids - 1, 
        delete.vertices, PACKAGE = "igraph")
    res
}


sample_dirichlet <- function (n, alpha) 
{
    n <- as.integer(n)
    alpha <- as.numeric(alpha)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_sample_dirichlet", n, alpha, PACKAGE = "igraph")
    res
}


norm_coords <- function (layout, xmin = -1, xmax = 1, ymin = -1, ymax = 1, zmin = -1, 
    zmax = 1) 
{
    if (!is.matrix(layout)) {
        stop("`layout' not a matrix")
    }
    if (ncol(layout) != 2 && ncol(layout) != 3) {
        stop("`layout' should have 2 or three columns")
    }
    if (!is.null(xmin) && !is.null(xmax)) {
        layout[, 1] <- .layout.norm.col(layout[, 1], xmin, xmax)
    }
    if (!is.null(ymin) && !is.null(ymax)) {
        layout[, 2] <- .layout.norm.col(layout[, 2], ymin, ymax)
    }
    if (ncol(layout) == 3 && !is.null(zmin) && !is.null(zmax)) {
        layout[, 3] <- .layout.norm.col(layout[, 3], zmin, zmax)
    }
    layout
}


graph_from_edgelist <- function (el, directed = TRUE) 
{
    if (!is.matrix(el) || ncol(el) != 2) {
        stop("graph_from_edgelist expects a matrix with two columns")
    }
    if (nrow(el) == 0) {
        res <- make_empty_graph(directed = directed)
    }
    else {
        if (is.character(el)) {
            names <- unique(as.character(t(el)))
            ids <- seq(names)
            names(ids) <- names
            res <- graph(unname(ids[t(el)]), directed = directed)
            rm(ids)
            V(res)$name <- names
        }
        else {
            res <- graph(t(el), directed = directed)
        }
    }
    res
}


graph.union <- function (..., byname = "auto") 
{
    .igraph.graph.union.or.intersection("R_igraph_union", ..., 
        byname = byname, keep.all.vertices = TRUE)
}


max_cohesion <- function (blocks) 
{
    res <- numeric(blocks$vcount)
    myb <- blocks(blocks)
    coh <- cohesion(blocks)
    oo <- order(coh)
    myb <- myb[oo]
    coh <- coh[oo]
    for (b in seq_along(myb)) {
        res[myb[[b]]] <- coh[b]
    }
    res
}


list.edge.attributes <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    res <- base::.Call("R_igraph_mybracket2_names", graph, 9L, 
        4L, PACKAGE = "igraph")
    if (is.null(res)) {
        res <- character()
    }
    res
}


each_edge <- function (prob, loops = FALSE, multiple = FALSE) 
{
    method <- list(fun = rewire_each_edge, args = list(prob = prob, 
        loops = loops, multiple = multiple))
    add_class(method, "igraph_rewiring_method")
}


get_diameter <- function (graph, directed = TRUE, unconnected = TRUE, weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_diameter", graph, as.logical(directed), 
        as.logical(unconnected), weights, PACKAGE = "igraph") + 
        1L
    if (igraph_opt("return.vs.es")) {
        res <- create_vs(graph, res)
    }
    res
}


closeness <- function (graph, vids = V(graph), mode = c("out", "in", "all", 
    "total"), weights = NULL, normalized = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_closeness", graph, vids - 1, mode, 
        weights, normalized, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- V(graph)$name[vids]
    }
    res
}


nexus_get <- function (id, offset = 0, order = c("date", "name", "popularity"), 
    nexus.url = igraph_opt("nexus.url")) 
{
    order = igraph.match.arg(order)
    if (inherits(id, "nexusDatasetInfo")) {
        id <- id$id
    }
    else if (inherits(id, "nexusDatasetInfoList")) {
        id <- sapply(id, "[[", "id")
        return(lapply(id, nexus_get, nexus.url = nexus.url))
    }
    u <- paste(sep = "", nexus.url, "/api/dataset?id=", id, "&format=R-igraph")
    env <- new.env()
    rdata <- url(URLencode(u))
    load(rdata, envir = env)
    close(rdata)
    res <- get(ls(env)[1], env)
    upgrade_if_igraph <- function(x) if (is_igraph(x)) 
        upgrade_graph(x)
    else x
    if (is_igraph(res)) {
        upgrade_if_igraph(res)
    }
    else if (is.list(res)) {
        res2 <- lapply(res, upgrade_if_igraph)
        attributes(res2) <- attributes(res)
        res2
    }
}


gnm <- function (...) 
constructor_spec(sample_gnm, ...)


gnp <- function (...) 
constructor_spec(sample_gnp, ...)


static.power.law.game <- function (no.of.nodes, no.of.edges, exponent.out, exponent.in = -1, 
    loops = FALSE, multiple = FALSE, finite.size.correction = TRUE) 
{
    no.of.nodes <- as.integer(no.of.nodes)
    no.of.edges <- as.integer(no.of.edges)
    exponent.out <- as.numeric(exponent.out)
    exponent.in <- as.numeric(exponent.in)
    loops <- as.logical(loops)
    multiple <- as.logical(multiple)
    finite.size.correction <- as.logical(finite.size.correction)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_static_power_law_game", no.of.nodes, 
        no.of.edges, exponent.out, exponent.in, loops, multiple, 
        finite.size.correction, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Static power law model")
    res <- set.graph.attribute(res, "exponent.out", exponent.out)
    res <- set.graph.attribute(res, "exponent.in", exponent.in)
    res <- set.graph.attribute(res, "loops", loops)
    res <- set.graph.attribute(res, "multiple", multiple)
    res <- set.graph.attribute(res, "finite.size.correction", 
        finite.size.correction)
    res
}


graph_from_atlas <- function (n) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_atlas", as.numeric(n), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- sprintf("Graph from the Atlas #%i", n)
        res$n <- n
    }
    res
}


empty_graph <- function (...) 
constructor_spec(make_empty_graph, ...)


line.graph <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_line_graph", graph, PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Line graph"
    }
    res
}


bipartite <- function (...) 
constructor_spec(sample_bipartite, ...)


`%m%` <- function (x, y) 
{
    difference(x, y)
}


as_adj <- function (graph, type = c("both", "upper", "lower"), attr = NULL, 
    edges = FALSE, names = TRUE, sparse = igraph_opt("sparsematrices")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (!sparse) {
        get.adjacency.dense(graph, type = type, attr = attr, 
            edges = edges, names = names)
    }
    else {
        get.adjacency.sparse(graph, type = type, attr = attr, 
            edges = edges, names = names)
    }
}


cluster_edge_betweenness <- function (graph, weights = E(graph)$weight, directed = TRUE, 
    edge.betweenness = TRUE, merges = TRUE, bridges = TRUE, modularity = TRUE, 
    membership = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object!")
    }
    if (!is.null(weights)) {
        weights <- as.numeric(weights)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_community_edge_betweenness", graph, 
        weights, as.logical(directed), as.logical(edge.betweenness), 
        as.logical(merges), as.logical(bridges), as.logical(modularity), 
        as.logical(membership), PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$names <- V(graph)$name
    }
    res$vcount <- vcount(graph)
    res$algorithm <- "edge betweenness"
    res$membership <- res$membership + 1
    res$merges <- res$merges + 1
    res$removed.edges <- res$removed.edges + 1
    res$bridges <- res$bridges + 1
    class(res) <- "communities"
    res
}


get.all.shortest.paths <- function (graph, from, to = V(graph), mode = c("out", "all", 
    "in"), weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, out = 1, `in` = 2, all = 3)
    if (is.null(weights)) {
        if ("weight" %in% edge_attr_names(graph)) {
            weights <- as.numeric(E(graph)$weight)
        }
    }
    else {
        if (length(weights) == 1 && is.na(weights)) {
            weights <- NULL
        }
        else {
            weights <- as.numeric(weights)
        }
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    if (is.null(weights)) {
        res <- .Call("R_igraph_get_all_shortest_paths", graph, 
            as.igraph.vs(graph, from) - 1, as.igraph.vs(graph, 
                to) - 1, as.numeric(mode), PACKAGE = "igraph")
    }
    else {
        res <- .Call("R_igraph_get_all_shortest_paths_dijkstra", 
            graph, as.igraph.vs(graph, from) - 1, as.igraph.vs(graph, 
                to) - 1, weights, as.numeric(mode), PACKAGE = "igraph")
    }
    if (igraph_opt("return.vs.es")) {
        res$res <- lapply(res$res, create_vs, graph = graph)
    }
    res
}


layout.grid.3d <- function (graph, width = 0, height = 0) 
{
    .Deprecated("layout_on_grid", msg = paste0("layout.grid.3d is deprecated from\n", 
        "igraph 0.8.0, please use layout_on_grid instead"))
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    width <- as.integer(width)
    height <- as.integer(height)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_grid_3d", graph, width, height, 
        PACKAGE = "igraph")
    res
}


is_connected <- function (graph, mode = c("weak", "strong")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- igraph.match.arg(mode)
    mode <- switch(mode, weak = 1, strong = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_is_connected", graph, as.numeric(mode), PACKAGE = "igraph")
}


authority_score <- function (graph, scale = TRUE, weights = NULL, options = arpack_defaults) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    scale <- as.logical(scale)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_authority_score", graph, scale, weights, 
        options, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$vector) <- vertex_attr(graph, "name", )
    }
    res
}


graph.intersection <- function (...) 
UseMethod("intersection")


bfs <- function (graph, root, neimode = c("out", "in", "all", "total"), 
    unreachable = TRUE, restricted = NULL, order = TRUE, rank = FALSE, 
    father = FALSE, pred = FALSE, succ = FALSE, dist = FALSE, 
    callback = NULL, extra = NULL, rho = parent.frame()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (length(root) == 1) {
        root <- as.igraph.vs(graph, root) - 1
        roots <- NULL
    }
    else {
        roots <- as.igraph.vs(graph, root) - 1
        root <- 0
    }
    neimode <- switch(igraph.match.arg(neimode), out = 1, `in` = 2, 
        all = 3, total = 3)
    unreachable <- as.logical(unreachable)
    if (!is.null(restricted)) {
        restricted <- as.igraph.vs(graph, restricted)
    }
    if (!is.null(callback)) {
        callback <- as.function(callback)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_bfs", graph, root, roots, neimode, 
        unreachable, restricted, as.logical(order), as.logical(rank), 
        as.logical(father), as.logical(pred), as.logical(succ), 
        as.logical(dist), callback, extra, rho, PACKAGE = "igraph")
    if (order) 
        res$order <- res$order + 1
    if (rank) 
        res$rank <- res$rank + 1
    if (father) 
        res$father <- res$father + 1
    if (pred) 
        res$pred <- res$pred + 1
    if (succ) 
        res$succ <- res$succ + 1
    if (igraph_opt("return.vs.es")) {
        if (order) 
            res$order <- create_vs(graph, res$order, na_ok = TRUE)
        if (father) 
            res$father <- create_vs(graph, res$father, na_ok = TRUE)
        if (pred) 
            res$pred <- create_vs(graph, res$pred, na_ok = TRUE)
        if (succ) 
            res$succ <- create_vs(graph, res$succ, na_ok = TRUE)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        if (rank) 
            names(res$rank) <- V(graph)$name
        if (father) 
            names(res$father) <- V(graph)$name
        if (pred) 
            names(res$pred) <- V(graph)$name
        if (succ) 
            names(res$succ) <- V(graph)$name
        if (dist) 
            names(res$dist) <- V(graph)$name
    }
    res
}


graph_from_incidence_matrix <- function (incidence, directed = FALSE, mode = c("all", "out", 
    "in", "total"), multiple = FALSE, weighted = NULL, add.names = NULL) 
{
    directed <- as.logical(directed)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    multiple <- as.logical(multiple)
    if (inherits(incidence, "Matrix")) {
        res <- graph.incidence.sparse(incidence, directed = directed, 
            mode = mode, multiple = multiple, weighted = weighted)
    }
    else {
        incidence <- as.matrix(incidence)
        res <- graph.incidence.dense(incidence, directed = directed, 
            mode = mode, multiple = multiple, weighted = weighted)
    }
    if (is.null(add.names)) {
        if (!is.null(rownames(incidence)) && !is.null(colnames(incidence))) {
            add.names <- "name"
        }
        else {
            add.names <- NA
        }
    }
    else if (!is.na(add.names)) {
        if (is.null(rownames(incidence)) || is.null(colnames(incidence))) {
            warning("Cannot add row- and column names, at least one of them is missing")
            add.names <- NA
        }
    }
    if (!is.na(add.names)) {
        res <- set_vertex_attr(res, add.names, value = c(rownames(incidence), 
            colnames(incidence)))
    }
    res
}


algorithm <- function (communities) 
{
    communities$algorithm
}


sample_asym_pref <- function (nodes, types, type.dist.matrix = matrix(1, types, types), 
    pref.matrix = matrix(1, types, types), loops = FALSE) 
{
    if (nrow(pref.matrix) != types || ncol(pref.matrix) != types) {
        stop("Invalid size for preference matrix")
    }
    if (nrow(type.dist.matrix) != types || ncol(type.dist.matrix) != 
        types) {
        stop("Invalid size for type distribution matrix")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_asymmetric_preference_game", as.double(nodes), 
        as.double(types), matrix(as.double(type.dist.matrix), 
            types, types), matrix(as.double(pref.matrix), types, 
            types), as.logical(loops), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Asymmetric preference random graph"
        res$types <- types
        res$type.dist.matrix <- type.dist.matrix
        res$pref.matrix <- pref.matrix
        res$loops <- loops
    }
}


sample_islands <- function (islands.n, islands.size, islands.pin, n.inter) 
{
    islands.n <- as.integer(islands.n)
    islands.size <- as.integer(islands.size)
    islands.pin <- as.numeric(islands.pin)
    n.inter <- as.integer(n.inter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_simple_interconnected_islands_game", 
        islands.n, islands.size, islands.pin, n.inter, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Interconnected islands model")
    res <- set.graph.attribute(res, "islands.n", islands.n)
    res <- set.graph.attribute(res, "islands.size", islands.size)
    res <- set.graph.attribute(res, "islands.pin", islands.pin)
    res <- set.graph.attribute(res, "n.inter", n.inter)
    res
}


with_lgl <- function (...) 
layout_spec(layout_with_lgl, ...)


`%du%` <- function (x, y) 
{
    disjoint_union(x, y)
}


gorder <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_vcount", graph, PACKAGE = "igraph")
    res
}


graph_attr <- function (graph, name) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (missing(name)) {
        graph.attributes(graph)
    }
    else {
        base::.Call("R_igraph_mybracket2", graph, 9L, 2L, PACKAGE = "igraph")[[as.character(name)]]
    }
}


laplacian_matrix <- function (graph, normalized = FALSE, weights = NULL, sparse = igraph_opt("sparsematrices")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    normalized <- as.logical(normalized)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    sparse <- as.logical(sparse)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_laplacian", graph, normalized, weights, 
        sparse, PACKAGE = "igraph")
    if (sparse) {
        res <- igraph.i.spMatrix(res)
    }
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        rownames(res) <- colnames(res) <- V(graph)$name
    }
    res
}


centralization.degree <- function (graph, mode = c("all", "out", "in", "total"), loops = TRUE, 
    normalized = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    loops <- as.logical(loops)
    normalized <- as.logical(normalized)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_centralization_degree", graph, mode, 
        loops, normalized, PACKAGE = "igraph")
    res
}


degree.sequence.game <- function (out.deg, in.deg = NULL, method = c("simple", "vl", 
    "simple.no.multiple")) 
{
    method <- igraph.match.arg(method)
    method1 <- switch(method, simple = 0, vl = 1, simple.no.multiple = 2)
    if (!is.null(in.deg)) {
        in.deg <- as.numeric(in.deg)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_degree_sequence_game", as.numeric(out.deg), 
        in.deg, as.numeric(method1), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Degree sequence random graph"
        res$method <- method
    }
    res
}


as_long_data_frame <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    ver <- .Call("R_igraph_mybracket2", graph, 9L, 3L, PACKAGE = "igraph")
    class(ver) <- "data.frame"
    rn <- if (is_named(graph)) {
        V(graph)$name
    }
    else {
        seq_len(vcount(graph))
    }
    rownames(ver) <- rn
    el <- as_edgelist(graph, names = FALSE)
    edg <- c(list(from = el[, 1]), list(to = el[, 2]), .Call("R_igraph_mybracket2", 
        graph, 9L, 4L, PACKAGE = "igraph"))
    class(edg) <- "data.frame"
    rownames(edg) <- seq_len(ecount(graph))
    ver2 <- ver
    names(ver) <- paste0("from_", names(ver))
    names(ver2) <- paste0("to_", names(ver2))
    edg <- cbind(edg, ver[el[, 1], ], ver2[el[, 2], ])
    edg
}


layout.star <- function (graph, center = V(graph)[1], order = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    center <- as.igraph.vs(graph, center)
    if (!is.null(order)) 
        order <- as.numeric(order) - 1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_layout_star", graph, center - 1, order, 
        PACKAGE = "igraph")
    res
}


chordal_ring <- function (...) 
constructor_spec(make_chordal_ring, ...)


traits <- function (...) 
constructor_spec(sample_traits, ...)


add_edges <- function (graph, edges, ..., attr = list()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    attrs <- list(...)
    attrs <- append(attrs, attr)
    nam <- names(attrs)
    if (length(attrs) != 0 && (is.null(nam) || any(nam == ""))) {
        stop("please supply names for attributes")
    }
    edges.orig <- ecount(graph)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    graph <- .Call("R_igraph_add_edges", graph, as.igraph.vs(graph, 
        edges) - 1, PACKAGE = "igraph")
    edges.new <- ecount(graph)
    if (edges.new - edges.orig != 0) {
        idx <- seq(edges.orig + 1, edges.new)
    }
    else {
        idx <- numeric()
    }
    eattrs <- .Call("R_igraph_mybracket2", graph, 9L, 4L, PACKAGE = "igraph")
    for (i in seq(attrs)) {
        eattrs[[nam[i]]][idx] <- attrs[[nam[i]]]
    }
    .Call("R_igraph_mybracket2_set", graph, 9L, 4L, eattrs, PACKAGE = "igraph")
}


rglplot <- function (x, ...) 
UseMethod("rglplot", x)


make_line_graph <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_line_graph", graph, PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Line graph"
    }
    res
}


make_star <- function (n, mode = c("in", "out", "mutual", "undirected"), center = 1) 
{
    mode <- igraph.match.arg(mode)
    mode1 <- switch(mode, out = 0, `in` = 1, undirected = 2, 
        mutual = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_star", as.numeric(n), as.numeric(mode1), 
        as.numeric(center) - 1, PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- switch(mode, `in` = "In-star", out = "Out-star", 
            "Star")
        res$mode <- mode
        res$center <- center
    }
    res
}


parent <- function (blocks) 
{
    blocks$parent
}


maximal.cliques.count <- function (graph, min = NULL, max = NULL, subset = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(min)) {
        min <- 0
    }
    if (is.null(max)) {
        max <- 0
    }
    min <- as.integer(min)
    max <- as.integer(max)
    if (!is.null(subset)) {
        subset <- as.integer(as.igraph.vs(graph, subset) - 1)
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maximal_cliques_count", graph, subset, 
        min, max, PACKAGE = "igraph")
    res
}


hrg.game <- function (hrg) 
{
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_game", hrg, PACKAGE = "igraph")
    res <- set.graph.attribute(res, "name", "Hierarchical random graph model")
    res
}


graph.tree <- function (n, children = 2, mode = c("out", "in", "undirected")) 
{
    mode <- igraph.match.arg(mode)
    mode1 <- switch(mode, out = 0, `in` = 1, undirected = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_tree", as.numeric(n), as.numeric(children), 
        as.numeric(mode1), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Tree"
        res$children <- children
        res$mode <- mode
    }
    res
}


degseq <- function (...) 
constructor_spec(sample_degseq, ...)


complementer <- function (graph, loops = FALSE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_complementer", graph, as.logical(loops), 
        PACKAGE = "igraph")
}


nexus.info <- function (id, nexus.url = igraph_opt("nexus.url")) 
{
    if (inherits(id, "nexusDatasetInfo")) {
        id <- id$id
    }
    else if (inherits(id, "nexusDatasetInfoList")) {
        rid <- sapply(id, "[[", "id")
        res <- lapply(rid, nexus_info, nexus.url = nexus.url)
        class(res) <- class(id)
        attributes(res) <- attributes(id)
        return(res)
    }
    u <- paste(sep = "", nexus.url, "/api/dataset_info?format=text&id=", 
        id)
    f <- url(URLencode(u))
    l <- readLines(f)
    close(f)
    l2 <- character()
    for (i in seq_along(l)) {
        if (!grepl("^  ", l[i])) {
            l2 <- c(l2, l[i])
        }
        else {
            l2[length(l2)] <- paste(sep = "\n", l2[length(l2)], 
                sub("  ", "", l[i], fixed = TRUE))
        }
    }
    l2 <- lapply(l2, function(x) c(sub("[ ]*:.*$", "", x), sub("^[^:]*:[ ]*", 
        "", x)))
    res <- makeNexusDatasetInfo(l2)
    if (!"attributes" %in% names(res)) {
        res$attributes <- list()
    }
    return(res)
}


embed_laplacian_matrix <- function (graph, no, weights = NULL, which = c("lm", "la", "sa"), 
    degmode = c("out", "in", "all", "total"), type = c("default", 
        "D-A", "DAD", "I-DAD", "OAP"), scaled = TRUE, options = igraph.arpack.default) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    no <- as.integer(no)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    which <- switch(igraph.match.arg(which), lm = 0L, la = 2L, 
        sa = 3L)
    degmode <- switch(igraph.match.arg(degmode), out = 1, `in` = 2, 
        all = 3, total = 3)
    type <- switch(igraph.match.arg(type), default = if (is.directed(graph)) 3L else 0L, 
        da = 0L, `d-a` = 0L, idad = 1L, `i-dad` = 1L, dad = 2L, 
        oap = 3L)
    scaled <- as.logical(scaled)
    options.tmp <- arpack_defaults
    options.tmp[names(options)] <- options
    options <- options.tmp
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_laplacian_spectral_embedding", graph, 
        no, weights, which, degmode, type, scaled, options, PACKAGE = "igraph")
    res
}


scan_stat <- function (graphs, tau = 1, ell = 0, locality = c("us", "them"), 
    ...) 
{
    stopifnot(is.list(graphs), length(graphs) > 0, all(sapply(graphs, 
        is_igraph)), length(unique(sapply(graphs, is_directed))) == 
        1, length(unique(sapply(graphs, gorder))) == 1)
    stopifnot(length(tau) == 1, tau >= 0, as.integer(tau) == 
        tau)
    stopifnot(length(ell) == 1, ell >= 0, as.integer(ell) == 
        ell)
    locality <- igraph.match.arg(locality)
    maxTime = length(graphs)
    nVertex = vcount(graphs[[1]])
    if (locality == "us") {
        lstatPsi <- matrix(0, nrow = nVertex, ncol = maxTime)
        for (i in 1:maxTime) {
            lstatPsi[, i] <- local_scan(graphs[[i]], ...)
        }
        lstat <- lstatPsi
    }
    else if (locality == "them") {
        lstatPhi <- array(0, dim = c(nVertex, (tau + 1), maxTime))
        for (i in 1:maxTime) {
            if (i > tau) {
                g <- graphs[[i]]
                for (j in 0:tau) {
                  lstatPhi[, (j + 1), i] <- local_scan(graph.us = graphs[[i]], 
                    graph.them = graphs[[i - tau + j]], ...)
                }
            }
        }
        lstat <- lstatPhi
    }
    scan_temp_norm(scan_vertex_norm(lstat, tau), tau, ell)
}


make_tree <- function (n, children = 2, mode = c("out", "in", "undirected")) 
{
    mode <- igraph.match.arg(mode)
    mode1 <- switch(mode, out = 0, `in` = 1, undirected = 2)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_tree", as.numeric(n), as.numeric(children), 
        as.numeric(mode1), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- "Tree"
        res$children <- children
        res$mode <- mode
    }
    res
}


cliques <- function (graph, min = NULL, max = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(min)) {
        min <- 0
    }
    if (is.null(max)) {
        max <- 0
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_cliques", graph, as.numeric(min), 
        as.numeric(max), PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


is_matching <- function (graph, matching, types = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        types <- as.logical(types)
    }
    matching <- as.igraph.vs(graph, matching, na.ok = TRUE) - 
        1
    matching[is.na(matching)] <- -1
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_matching", graph, types, matching, 
        PACKAGE = "igraph")
    res
}


max_flow <- function (graph, source, target, capacity = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    source <- as.igraph.vs(graph, source)
    target <- as.igraph.vs(graph, target)
    if (is.null(capacity) && "capacity" %in% edge_attr_names(graph)) {
        capacity <- E(graph)$capacity
    }
    if (!is.null(capacity) && any(!is.na(capacity))) {
        capacity <- as.numeric(capacity)
    }
    else {
        capacity <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maxflow", graph, source - 1, target - 
        1, capacity, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res$partition1 <- create_vs(graph, res$partition1)
    }
    if (igraph_opt("return.vs.es")) {
        res$partition2 <- create_vs(graph, res$partition2)
    }
    res
}


edge <- function (...) 
{
    structure(list(...), class = "igraph.edge")
}


graph.get.subisomorphisms.vf2 <- function (graph1, graph2, vertex.color1, vertex.color2, edge.color1, 
    edge.color2) 
{
    if (!is_igraph(graph1)) {
        stop("Not a graph object")
    }
    if (!is_igraph(graph2)) {
        stop("Not a graph object")
    }
    if (missing(vertex.color1)) {
        if ("color" %in% vertex_attr_names(graph1)) {
            vertex.color1 <- V(graph1)$color
        }
        else {
            vertex.color1 <- NULL
        }
    }
    if (!is.null(vertex.color1)) {
        vertex.color1 <- as.integer(vertex.color1) - 1L
    }
    if (missing(vertex.color2)) {
        if ("color" %in% vertex_attr_names(graph2)) {
            vertex.color2 <- V(graph2)$color
        }
        else {
            vertex.color2 <- NULL
        }
    }
    if (!is.null(vertex.color2)) {
        vertex.color2 <- as.integer(vertex.color2) - 1L
    }
    if (missing(edge.color1)) {
        if ("color" %in% edge_attr_names(graph1)) {
            edge.color1 <- E(graph1)$color
        }
        else {
            edge.color1 <- NULL
        }
    }
    if (!is.null(edge.color1)) {
        edge.color1 <- as.integer(edge.color1) - 1L
    }
    if (missing(edge.color2)) {
        if ("color" %in% edge_attr_names(graph2)) {
            edge.color2 <- E(graph2)$color
        }
        else {
            edge.color2 <- NULL
        }
    }
    if (!is.null(edge.color2)) {
        edge.color2 <- as.integer(edge.color2) - 1L
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_get_subisomorphisms_vf2", graph1, 
        graph2, vertex.color1, vertex.color2, edge.color1, edge.color2, 
        PACKAGE = "igraph")
    lapply(res, function(x) V(graph1)[x + 1])
}


graph.knn <- function (graph, vids = V(graph), weights = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_avg_nearest_neighbor_degree", graph, 
        vids - 1, weights, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$knn) <- vertex_attr(graph, "name", vids)
    }
    res
}


communities <- function (x) 
{
    m <- membership(x)
    groups.default(list(membership = m))
}


adjacent.triangles <- function (graph, vids = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    vids <- as.igraph.vs(graph, vids)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_adjacent_triangles", graph, vids - 
        1, PACKAGE = "igraph")
    res
}


make_full_bipartite_graph <- function (n1, n2, directed = FALSE, mode = c("all", "out", "in")) 
{
    n1 <- as.integer(n1)
    n2 <- as.integer(n2)
    directed <- as.logical(directed)
    mode1 <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_full_bipartite", n1, n2, as.logical(directed), 
        mode1, PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$graph$name <- "Full bipartite graph"
        res$n1 <- n1
        res$n2 <- n2
        res$mode <- mode
    }
    set_vertex_attr(res$graph, "type", value = res$types)
}


which_multiple <- function (graph, eids = E(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_is_multiple", graph, as.igraph.es(graph, 
        eids) - 1, PACKAGE = "igraph")
}


grg.game <- function (nodes, radius, torus = FALSE, coords = FALSE) 
{
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_grg_game", as.double(nodes), as.double(radius), 
        as.logical(torus), as.logical(coords), PACKAGE = "igraph")
    if (coords) {
        V(res[[1]])$x <- res[[2]]
        V(res[[1]])$y <- res[[3]]
    }
    if (igraph_opt("add.params")) {
        res[[1]]$name <- "Geometric random graph"
        res[[1]]$radius <- radius
        res[[1]]$torus <- torus
    }
    res[[1]]
}


graph.formula <- function (..., simplify = TRUE) 
{
    mf <- as.list(match.call())[-1]
    graph_from_literal_i(mf)
}


sbm <- function (...) 
constructor_spec(sample_sbm, ...)


page.rank <- function (graph, algo = c("prpack", "arpack", "power"), vids = V(graph), 
    directed = TRUE, damping = 0.85, personalized = NULL, weights = NULL, 
    options = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    algo <- switch(igraph.match.arg(algo), power = 0L, arpack = 1L, 
        prpack = 2L)
    vids <- as.igraph.vs(graph, vids)
    directed <- as.logical(directed)
    damping <- as.numeric(damping)
    if (!is.null(personalized)) 
        personalized <- as.numeric(personalized)
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    if (is.null(options)) {
        if (algo == 0L) {
            options <- list(niter = 1000, eps = 0.001)
        }
        else if (algo == 1L) {
            options <- arpack_defaults
        }
        else {
            options <- NULL
        }
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_personalized_pagerank", graph, algo, 
        vids - 1, directed, damping, personalized, weights, options, 
        PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res$vector) <- vertex_attr(graph, "name", vids)
    }
    res
}


create.communities <- function (graph, membership = NULL, algorithm = NULL, merges = NULL, 
    modularity = TRUE) 
{
    stopifnot(is.null(membership) || is.numeric(membership))
    stopifnot(is.null(algorithm) || (is.character(algorithm) && 
        length(algorithm) == 1))
    stopifnot(is.null(merges) || (is.matrix(merges) && is.numeric(merges) && 
        ncol(merges) == 2))
    stopifnot(is.null(modularity) || (is.logical(modularity) && 
        length(modularity) == 1) || (is.numeric(modularity) && 
        length(modularity) %in% c(1, length(membership))))
    if (is.logical(modularity)) {
        if (modularity && !is.null(membership)) {
            modularity <- modularity(graph, membership)
        }
        else {
            modularity <- NULL
        }
    }
    res <- list(membership = membership, algorithm = if (is.null(algorithm)) "unknown" else algorithm, 
        modularity = modularity)
    if (!is.null(merges)) {
        res$merges <- merges
    }
    if (!is.null(membership)) {
        res$vcount <- length(membership)
    }
    else if (!is.null(merges)) {
        res$vcount <- nrow(merges) + 1
    }
    class(res) <- "communities"
    res
}


igraph_opt <- function (x, default = NULL) 
{
    if (missing(default)) 
        return(igraph_options(x)[[1L]])
    if (x %in% names(igraph_options())) 
        igraph_options(x)[[1L]]
    else default
}


diversity <- function (graph, weights = NULL, vids = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    vids <- as.igraph.vs(graph, vids)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_diversity", graph, weights, vids - 
        1, PACKAGE = "igraph")
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        names(res) <- vertex_attr(graph, "name", vids)
    }
    res
}


layout_in_circle <- function (graph, order = V(graph)) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    order <- as.igraph.vs(graph, order) - 1L
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_layout_circle", graph, order, PACKAGE = "igraph")
}


graph.isoclass <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_isoclass", graph, PACKAGE = "igraph")
    res
}


grg <- function (...) 
constructor_spec(sample_grg, ...)


scg <- function (X, ev, nt, groups = NULL, mtype = c("symmetric", "laplacian", 
    "stochastic"), algo = c("optimum", "interv_km", "interv", 
    "exact_scg"), norm = c("row", "col"), direction = c("default", 
    "left", "right"), evec = NULL, p = NULL, use.arpack = FALSE, 
    maxiter = 300, sparse = igraph_opt("sparsematrices"), output = c("default", 
        "matrix", "graph"), semproj = FALSE, epairs = FALSE, 
    stat.prob = FALSE) 
UseMethod("scg")


as_phylo <- function (x, ...) 
UseMethod("as_phylo")


hrg.predict <- function (graph, hrg = NULL, start = FALSE, num.samples = 10000, 
    num.bins = 25) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(hrg)) {
        hrg <- list(left = c(), right = c(), prob = c(), edges = c(), 
            vertices = c())
    }
    hrg <- lapply(hrg[c("left", "right", "prob", "edges", "vertices")], 
        as.numeric)
    start <- as.logical(start)
    num.samples <- as.integer(num.samples)
    num.bins <- as.integer(num.bins)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_predict", graph, hrg, start, num.samples, 
        num.bins, PACKAGE = "igraph")
    res$edges <- matrix(res$edges, ncol = 2, byrow = TRUE)
    class(res$hrg) <- "igraphHRG"
    res
}


max_cardinality <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maximum_cardinality_search", graph, 
        PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res$alpha <- create_vs(graph, res$alpha)
    }
    res
}


hierarchy <- function (blocks) 
{
    blocks$blockTree
}


graphlet_proj <- function (graph, weights = NULL, cliques, niter = 1000, Mu = rep(1, 
    length(cliques))) 
{
    if (!is.igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    Mu <- as.numeric(Mu)
    niter <- as.integer(niter)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_graphlets_project", graph, weights, 
        cliques, Mu, niter, PACKAGE = "igraph")
    res
}


adjacent_vertices <- function (graph, v, mode = c("out", "in", "all", "total")) 
{
    if (!is_igraph(graph)) 
        stop("Not a graph object")
    vv <- as.igraph.vs(graph, v) - 1
    mode <- switch(match.arg(mode), out = 1, `in` = 2, all = 3, 
        total = 3)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_adjacent_vertices", graph, vv, mode, 
        PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, function(x) create_vs(graph, x + 1))
    }
    if (is_named(graph)) 
        names(res) <- V(graph)$name[vv + 1]
    res
}


graph.famous <- function (name) 
{
    name <- gsub("\\s", "_", name)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_famous", as.character(name), PACKAGE = "igraph")
    if (igraph_opt("add.params")) {
        res$name <- capitalize(name)
    }
    res
}


nexus.get <- function (id, offset = 0, order = c("date", "name", "popularity"), 
    nexus.url = igraph_opt("nexus.url")) 
{
    order = igraph.match.arg(order)
    if (inherits(id, "nexusDatasetInfo")) {
        id <- id$id
    }
    else if (inherits(id, "nexusDatasetInfoList")) {
        id <- sapply(id, "[[", "id")
        return(lapply(id, nexus_get, nexus.url = nexus.url))
    }
    u <- paste(sep = "", nexus.url, "/api/dataset?id=", id, "&format=R-igraph")
    env <- new.env()
    rdata <- url(URLencode(u))
    load(rdata, envir = env)
    close(rdata)
    res <- get(ls(env)[1], env)
    upgrade_if_igraph <- function(x) if (is_igraph(x)) 
        upgrade_graph(x)
    else x
    if (is_igraph(res)) {
        upgrade_if_igraph(res)
    }
    else if (is.list(res)) {
        res2 <- lapply(res, upgrade_if_igraph)
        attributes(res2) <- attributes(res)
        res2
    }
}


is.graphical.degree.sequence <- function (out.deg, in.deg = NULL) 
{
    out.deg <- as.numeric(out.deg)
    if (!is.null(in.deg)) 
        in.deg <- as.numeric(in.deg)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_is_graphical_degree_sequence", out.deg, 
        in.deg, PACKAGE = "igraph")
    res
}


running.mean <- function (v, binwidth) 
{
    v <- as.numeric(v)
    binwidth <- as.numeric(binwidth)
    if (length(v) < binwidth) {
        stop("Vector too short for this binwidth.")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    .Call("R_igraph_running_mean", v, binwidth, PACKAGE = "igraph")
}


code_len <- function (communities) 
{
    communities$codelength
}


with_gem <- function (...) 
layout_spec(layout_with_gem, ...)


set.graph.attribute <- function (graph, name, value) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    base::.Call("R_igraph_mybracket3_set", graph, 9L, 2L, name, 
        value, PACKAGE = "igraph")
}


add.edges <- function (graph, edges, ..., attr = list()) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    attrs <- list(...)
    attrs <- append(attrs, attr)
    nam <- names(attrs)
    if (length(attrs) != 0 && (is.null(nam) || any(nam == ""))) {
        stop("please supply names for attributes")
    }
    edges.orig <- ecount(graph)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    graph <- .Call("R_igraph_add_edges", graph, as.igraph.vs(graph, 
        edges) - 1, PACKAGE = "igraph")
    edges.new <- ecount(graph)
    if (edges.new - edges.orig != 0) {
        idx <- seq(edges.orig + 1, edges.new)
    }
    else {
        idx <- numeric()
    }
    eattrs <- .Call("R_igraph_mybracket2", graph, 9L, 4L, PACKAGE = "igraph")
    for (i in seq(attrs)) {
        eattrs[[nam[i]]][idx] <- attrs[[nam[i]]]
    }
    .Call("R_igraph_mybracket2_set", graph, 9L, 4L, eattrs, PACKAGE = "igraph")
}


hrg.create <- function (graph, prob) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    prob <- as.numeric(prob)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_hrg_create", graph, prob, PACKAGE = "igraph")
    class(res) <- "igraphHRG"
    res
}


set_vertex_attr <- function (graph, name, index = V(graph), value) 
{
    i_set_vertex_attr(graph = graph, name = name, index = index, 
        value = value)
}


maximal.independent.vertex.sets <- function (graph) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maximal_independent_vertex_sets", 
        graph, PACKAGE = "igraph")
    res <- lapply(res, function(x) x + 1)
    if (igraph_opt("return.vs.es")) {
        res <- lapply(res, create_vs, graph = graph)
    }
    res
}


sizes <- function (communities) 
{
    m <- membership(communities)
    table(`Community sizes` = m)
}


graph.automorphisms <- function (graph, sh = "fm") 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    sh <- switch(igraph.match.arg(sh), f = 0, fl = 1, fs = 2, 
        fm = 3, flm = 4, fsm = 5)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_automorphisms", graph, sh, PACKAGE = "igraph")
    res
}


graph.adjlist <- function (adjlist, mode = c("out", "in", "all", "total"), duplicate = TRUE) 
{
    adjlist <- lapply(adjlist, function(x) as.integer(x) - 1L)
    mode <- switch(igraph.match.arg(mode), out = 1, `in` = 2, 
        all = 3, total = 3)
    duplicate <- as.logical(duplicate)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_adjlist", adjlist, mode, duplicate, 
        PACKAGE = "igraph")
    res
}


piecewise.layout <- function (graph, layout = layout_with_kk, ...) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    V(graph)$id <- seq(vcount(graph))
    gl <- decompose(graph)
    ll <- lapply(gl, layout, ...)
    l <- merge_coords(gl, ll)
    l[unlist(sapply(gl, vertex_attr, "id")), ] <- l[]
    l
}


max_bipartite_match <- function (graph, types = NULL, weights = NULL, eps = .Machine$double.eps) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(types) && "type" %in% vertex_attr_names(graph)) {
        types <- V(graph)$type
    }
    if (!is.null(types)) {
        types <- as.logical(types)
    }
    if (is.null(weights) && "weight" %in% edge_attr_names(graph)) {
        weights <- E(graph)$weight
    }
    if (!is.null(weights) && any(!is.na(weights))) {
        weights <- as.numeric(weights)
    }
    else {
        weights <- NULL
    }
    eps <- as.numeric(eps)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maximum_bipartite_matching", graph, 
        types, weights, eps, PACKAGE = "igraph")
    res$matching[res$matching == 0] <- NA
    if (igraph_opt("add.vertex.names") && is_named(graph)) {
        res$matching <- V(graph)$name[res$matching]
        names(res$matching) <- V(graph)$name
    }
    res
}


igraph.console <- function () 
{
    oldverb <- igraph_opt("verbose")
    igraph_options(verbose = "tkconsole")
    pb <- .igraph.progress.tkconsole.create(oldverb)
    assign(".igraph.pb", pb, envir = asNamespace("igraph"))
    .igraph.progress.tkconsole.message("Console started.\n")
    invisible()
}


edge.connectivity <- function (graph, source = NULL, target = NULL, checks = TRUE) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    if (is.null(source) && is.null(target)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_edge_connectivity", graph, as.logical(checks), 
            PACKAGE = "igraph")
    }
    else if (!is.null(source) && !is.null(target)) {
        on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
        .Call("R_igraph_st_edge_connectivity", graph, as.igraph.vs(graph, 
            source) - 1, as.igraph.vs(graph, target) - 1, PACKAGE = "igraph")
    }
    else {
        stop("either give both source and target or neither")
    }
}


graph.graphdb <- function (url = NULL, prefix = "iso", type = "r001", nodes = NULL, 
    pair = "A", which = 0, base = "http://cneurocvs.rmki.kfki.hu/graphdb/gzip", 
    compressed = TRUE, directed = TRUE) 
{
    if (is.null(nodes) && is.null(url)) {
        stop("The `nodes' or the `url' argument must be non-null")
    }
    if (is.null(url)) {
        prefixes <- c("iso", "si6", "mcs10", "mcs30", "mcs50", 
            "mcs70", "mcs90")
        types <- c("r001", "r005", "r01", "r02", "m2D", "m2Dr2", 
            "m2Dr4", "m2Dr6", "m3D", "m3Dr2", "m3Dr4", "m3Dr6", 
            "m4D", "m4Dr2", "m4Dr4", "m4Dr6", "b03", "b03m", 
            "b06", "b06m", "b09", "b09m")
        sizecode <- if (nodes <= 100) 
            "s"
        else if (nodes < 2000) 
            "m"
        else "l"
        typegroups <- c("rand", "rand", "rand", "rand", "m2D", 
            "m2D", "m2D", "m2D", "m2D", "m3D", "m3D", "m3D", 
            "m4D", "m4D", "m4D", "m4D", "bvg", "bvg", "bvg", 
            "bvg", "bvg", "bvg")
        typegroup <- typegroups[which(types == type)]
        if (!prefix %in% prefixes) {
            stop("Invalid prefix!")
        }
        if (!type %in% types) {
            stop("Invalid graph type!")
        }
        suff <- if (compressed) 
            ".gz"
        else ""
        filename <- paste(sep = "", base, "/", prefix, "/", typegroup, 
            "/", type, "/", prefix, "_", type, "_", sizecode, 
            nodes, ".", pair, formatC(which, width = 2, flag = "0"), 
            suff)
    }
    else {
        filename <- url
    }
    f <- try(gzcon(file(filename, open = "rb")))
    if (inherits(f, "try-error")) {
        stop(paste("Cannot open URL:", filename))
    }
    buffer <- read.graph.toraw(f)
    f <- tempfile()
    write.graph.fromraw(buffer, f)
    .Call("R_igraph_read_graph_graphdb", f, as.logical(directed), 
        PACKAGE = "igraph")
}


graph.maxflow <- function (graph, source, target, capacity = NULL) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    source <- as.igraph.vs(graph, source)
    target <- as.igraph.vs(graph, target)
    if (is.null(capacity) && "capacity" %in% edge_attr_names(graph)) {
        capacity <- E(graph)$capacity
    }
    if (!is.null(capacity) && any(!is.na(capacity))) {
        capacity <- as.numeric(capacity)
    }
    else {
        capacity <- NULL
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_maxflow", graph, source - 1, target - 
        1, capacity, PACKAGE = "igraph")
    if (igraph_opt("return.vs.es")) {
        res$partition1 <- create_vs(graph, res$partition1)
    }
    if (igraph_opt("return.vs.es")) {
        res$partition2 <- create_vs(graph, res$partition2)
    }
    res
}


igraph_test <- function () 
{
    do.call(require, list("testthat"))
    tdir <- system.file("tests", package = "igraph")
    do.call("test_dir", list(tdir))
}


aging.prefatt.game <- function (n, pa.exp, aging.exp, m = NULL, aging.bin = 300, out.dist = NULL, 
    out.seq = NULL, out.pref = FALSE, directed = TRUE, zero.deg.appeal = 1, 
    zero.age.appeal = 0, deg.coef = 1, age.coef = 1, time.window = NULL) 
{
    if (!is.null(out.seq) && (!is.null(m) || !is.null(out.dist))) {
        warning("if `out.seq' is given `m' and `out.dist' should be NULL")
        m <- out.dist <- NULL
    }
    if (is.null(out.seq) && !is.null(out.dist) && !is.null(m)) {
        warning("if `out.dist' is given `m' will be ignored")
        m <- NULL
    }
    if (!is.null(out.seq) && length(out.seq) != n) {
        stop("`out.seq' should be of length `n'")
    }
    if (!is.null(out.seq) && min(out.seq) < 0) {
        stop("negative elements in `out.seq'")
    }
    if (!is.null(m) && m < 0) {
        stop("`m' is negative")
    }
    if (!is.null(time.window) && time.window <= 0) {
        stop("time window size should be positive")
    }
    if (!is.null(m) && m == 0) {
        warning("`m' is zero, graph will be empty")
    }
    if (pa.exp < 0) {
        warning("preferential attachment is negative")
    }
    if (aging.exp > 0) {
        warning("aging exponent is positive")
    }
    if (zero.deg.appeal <= 0) {
        warning("initial attractiveness is not positive")
    }
    if (is.null(m) && is.null(out.dist) && is.null(out.seq)) {
        m <- 1
    }
    n <- as.numeric(n)
    if (!is.null(m)) {
        m <- as.numeric(m)
    }
    if (!is.null(out.dist)) {
        out.dist <- as.numeric(out.dist)
    }
    if (!is.null(out.seq)) {
        out.seq <- as.numeric(out.seq)
    }
    out.pref <- as.logical(out.pref)
    if (!is.null(out.dist)) {
        out.seq <- as.numeric(sample(0:(length(out.dist) - 1), 
            n, replace = TRUE, prob = out.dist))
    }
    if (is.null(out.seq)) {
        out.seq <- numeric()
    }
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- if (is.null(time.window)) {
        .Call("R_igraph_barabasi_aging_game", as.numeric(n), 
            as.numeric(pa.exp), as.numeric(aging.exp), as.numeric(aging.bin), 
            m, out.seq, out.pref, as.numeric(zero.deg.appeal), 
            as.numeric(zero.age.appeal), as.numeric(deg.coef), 
            as.numeric(age.coef), directed, PACKAGE = "igraph")
    }
    else {
        .Call("R_igraph_recent_degree_aging_game", as.numeric(n), 
            as.numeric(pa.exp), as.numeric(aging.exp), as.numeric(aging.bin), 
            m, out.seq, out.pref, as.numeric(zero.deg.appeal), 
            directed, time.window, PACKAGE = "igraph")
    }
    if (igraph_opt("add.params")) {
        res$name <- "Aging Barabasi graph"
        res$pa.exp <- pa.exp
        res$aging.exp <- aging.exp
        res$m <- m
        res$aging.bin <- aging.bin
        res$out.pref <- out.pref
        res$zero.deg.appeal <- zero.deg.appeal
        res$zero.age.appeal <- zero.age.appeal
        res$deg.coef <- deg.coef
        res$age.coef <- age.coef
        res$time.window <- if (is.null(time.window)) 
            Inf
        else time.window
    }
    res
}


contract.vertices <- function (graph, mapping, vertex.attr.comb = igraph_opt("vertex.attr.comb")) 
{
    if (!is_igraph(graph)) {
        stop("Not a graph object")
    }
    mapping <- as.numeric(mapping) - 1
    vertex.attr.comb <- igraph.i.attribute.combination(vertex.attr.comb)
    on.exit(.Call("R_igraph_finalizer", PACKAGE = "igraph"))
    res <- .Call("R_igraph_contract_vertices", graph, mapping, 
        vertex.attr.comb, PACKAGE = "igraph")
    res
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Network Analysis and Visualization"

.skeleton_package_version = "1.0.1"

.skeleton_package_depends = "methods"

.skeleton_package_imports = "Matrix,magrittr,NMF,irlba"


## Internal

.skeleton_version = 5


## EOF