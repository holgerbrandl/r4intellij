##
## Exported symobls in package `xml2`
##

## Exported package methods

xml_siblings <- function (x) 
{
    nodeset_apply(x, node_siblings)
}


xml_ns <- function (x) 
{
    UseMethod("xml_ns")
}


xml_text <- function (x, trim = FALSE) 
{
    UseMethod("xml_text")
}


xml_parent <- function (x) 
{
    UseMethod("xml_parent")
}


xml_add_child <- function (.x, .value, ..., .where = length(xml_children(.x)), 
    .copy = TRUE) 
{
    UseMethod("xml_add_child")
}


xml_new_document <- function (version = "1.0", encoding = "UTF-8") 
{
    doc <- doc_new(version)
    structure(list(doc = doc), class = "xml_document")
}


xml_root <- function (x) 
{
    stopifnot(inherits(x, c("xml_node", "xml_document", "xml_nodeset")))
    if (inherits(x, "xml_nodeset")) {
        if (length(x) == 0) {
            return(NULL)
        }
        else {
            return(xml_root(x[[1]]))
        }
    }
    if (!doc_has_root(x$doc)) {
        xml_missing()
    }
    else {
        xml_document(x$doc)
    }
}


xml_find_one <- function (x, xpath, ns = xml_ns(x)) 
{
    .Deprecated("xml_find_first")
    UseMethod("xml_find_first")
}


xml_missing <- function () 
{
    structure(list(), class = "xml_missing")
}


xml_url <- function (x) 
{
    UseMethod("xml_url")
}


write_xml <- function (x, file, ...) 
{
    UseMethod("write_xml")
}


`xml_attr<-` <- function (x, attr, ns = character(), value) 
{
    UseMethod("xml_attr<-")
}


`xml_text<-` <- function (x, value) 
{
    UseMethod("xml_text<-")
}


xml_parents <- function (x) 
{
    nodeset_apply(x, node_parents)
}


xml_new_root <- function (.value, ..., .copy = inherits(.value, "xml_node"), 
    .version = "1.0", .encoding = "UTF-8") 
{
    xml_add_child(xml_new_document(version = .version, encoding = .encoding), 
        .value = .value, ..., .copy = .copy)
}


xml_comment <- function (content) 
{
    structure(content, class = "xml_comment")
}


xml_add_parent <- function (.x, .value, ...) 
{
    UseMethod("xml_add_parent")
}


as_xml_document <- function (x, ...) 
{
    UseMethod("as_xml_document")
}


xml_type <- function (x) 
{
    UseMethod("xml_type")
}


xml_structure <- function (x, indent = 2) 
{
    tree_structure(x, indent = indent, html = FALSE)
}


xml_set_namespace <- function (.x, prefix = "", uri = "") 
{
    stopifnot(inherits(.x, "xml_node"))
    if (nzchar(uri)) {
        node_set_namespace_uri(.x$doc, .x$node, uri)
    }
    else {
        node_set_namespace_prefix(.x$doc, .x$node, prefix)
    }
    invisible(.x)
}


xml_double <- function (x) 
{
    UseMethod("xml_double")
}


url_parse <- function (x) 
{
    .Call("xml2_url_parse", PACKAGE = "xml2", x)
}


xml_replace <- function (.x, .value, ..., .copy = TRUE) 
{
    UseMethod("xml_replace")
}


xml_add_sibling <- function (.x, .value, ..., .where = c("after", "before"), .copy = TRUE) 
{
    UseMethod("xml_add_sibling")
}


write_html <- function (x, file, ...) 
{
    UseMethod("write_html")
}


xml_find_all <- function (x, xpath, ns = xml_ns(x)) 
{
    UseMethod("xml_find_all")
}


read_xml <- function (x, encoding = "", ..., as_html = FALSE, options = "NOBLANKS") 
{
    UseMethod("read_xml")
}


xml_find_num <- function (x, xpath, ns = xml_ns(x)) 
{
    UseMethod("xml_find_num")
}


`xml_attrs<-` <- function (x, ns = character(), value) 
{
    UseMethod("xml_attrs<-")
}


xml_children <- function (x) 
{
    nodeset_apply(x, node_children)
}


xml_ns_strip <- function (x) 
{
    namespace_element_nodes <- xml_find_all(x, "//namespace::*[name()='']/parent::*")
    xml_attr(namespace_element_nodes, "xmlns") <- NULL
    invisible(x)
}


xml_cdata <- function (content) 
{
    structure(content, class = "xml_cdata")
}


xml_integer <- function (x) 
{
    UseMethod("xml_integer")
}


xml_set_attr <- function (x, attr, value, ns = character()) 
{
    UseMethod("xml_set_attr")
}


xml_find_chr <- function (x, xpath, ns = xml_ns(x)) 
{
    UseMethod("xml_find_chr")
}


xml_has_attr <- function (x, attr, ns = character()) 
{
    !is.na(xml_attr(x, attr, ns = ns))
}


xml_attr <- function (x, attr, ns = character(), default = NA_character_) 
{
    UseMethod("xml_attr")
}


xml_set_attrs <- function (x, value, ns = character()) 
{
    UseMethod("xml_set_attrs")
}


xml_attrs <- function (x, ns = character()) 
{
    UseMethod("xml_attrs")
}


xml_contents <- function (x) 
{
    nodeset_apply(x, node_children, onlyNode = FALSE)
}


xml_set_text <- function (x, value) 
{
    UseMethod("xml_text<-")
}


xml_child <- function (x, search = 1, ns = xml_ns(x)) 
{
    if (length(search) != 1) {
        stop("`search` must be of length 1", call. = FALSE)
    }
    if (is.numeric(search)) {
        xml_children(x)[[search]]
    }
    else if (is.character(search)) {
        xml_find_first(x, xpath = paste0("./", search), ns = ns)
    }
    else {
        stop("`search` must be `numeric` or `character`", call. = FALSE)
    }
}


xml_find_first <- function (x, xpath, ns = xml_ns(x)) 
{
    UseMethod("xml_find_first")
}


as_list <- function (x, ns = character(), ...) 
{
    UseMethod("as_list")
}


xml_dtd <- function (name = "", external_id = "", system_id = "") 
{
    structure(list(name = name, external_id = external_id, system_id = system_id), 
        class = "xml_dtd")
}


xml_validate <- function (x, schema) 
{
    UseMethod("xml_validate")
}


xml_unserialize <- function (connection, ...) 
{
    object <- unserialize(connection)
    if (inherits(object, "xml_serialized_nodeset")) {
        x <- read_xml(unclass(object), ...)
        xml_find_all(x, "/*/node()")
    }
    else if (inherits(object, "xml_serialized_node")) {
        x <- read_xml(unclass(object), ...)
        xml_find_first(x, "/node()")
    }
    else if (inherits(object, "xml_serialized_document")) {
        x <- read_xml(unclass(object), ...)
    }
    else {
        stop("Not a serialized xml2 object", call. = FALSE)
    }
}


xml_set_name <- function (x, value, ns = character()) 
{
    UseMethod("xml_set_name")
}


url_relative <- function (x, base) 
{
    .Call("xml2_url_relative", PACKAGE = "xml2", x, base)
}


xml_name <- function (x, ns = character()) 
{
    UseMethod("xml_name")
}


url_absolute <- function (x, base) 
{
    .Call("xml2_url_absolute", PACKAGE = "xml2", x, base)
}


url_escape <- function (x, reserved = "") 
{
    .Call("xml2_url_escape", PACKAGE = "xml2", x, reserved)
}


xml_remove <- function (.x, free = FALSE) 
{
    UseMethod("xml_remove")
}


url_unescape <- function (x) 
{
    .Call("xml2_url_unescape", PACKAGE = "xml2", x)
}


xml_ns_rename <- function (old, ...) 
{
    new <- c(...)
    m <- match(names(new), names(old))
    if (any(is.na(m))) {
        missing <- paste(names(new)[is.na(m)], collapse = ", ")
        stop("Some prefixes [", missing, "] don't already exist.", 
            call. = FALSE)
    }
    names(old)[m] <- new
    old
}


html_structure <- function (x, indent = 2) 
{
    tree_structure(x, indent = indent, html = TRUE)
}


xml_serialize <- function (object, connection, ...) 
UseMethod("xml_serialize")


xml_path <- function (x) 
{
    UseMethod("xml_path")
}


xml_length <- function (x, only_elements = TRUE) 
{
    UseMethod("xml_length")
}


read_html <- function (x, encoding = "", ..., options = c("RECOVER", "NOERROR", 
    "NOBLANKS")) 
{
    UseMethod("read_html")
}


xml_find_lgl <- function (x, xpath, ns = xml_ns(x)) 
{
    UseMethod("xml_find_lgl")
}


`xml_name<-` <- function (x, ns = character(), value) 
{
    UseMethod("xml_name<-")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Parse XML"

.skeleton_package_version = "1.1.1"

.skeleton_package_depends = ""

.skeleton_package_imports = "Rcpp"


## Internal

.skeleton_version = 5


## EOF