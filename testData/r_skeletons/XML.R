##
## Exported symobls in package `XML`
##

## Exported package methods

xmlSerializeHook <- function (x) 
{
    if (inherits(x, c("XMLInternalDocument", "XMLInternalElementNode"))) 
        c(as(x, "character"), class(x)[1])
    else if (is(x, "XMLNodeSet")) 
        c(sapply(x, as, "character"), class(x)[1])
    else NULL
}


`.__T__getEncoding:XML` <- "<environment>"

`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

.__C__ExternalReference <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("ExternalReference", package = "XML")
    , package = "XML"
    , subclasses = structure(list(xmlSchemaRef = S4_object(), 
    xmlSchemaElementRef = S4_object(), 
    xmlSchemaTypeRef = S4_object(), 
    xmlSchemaAttributeRef = S4_object(), 
    xmlSchemaAttributeGroupRef = S4_object(), 
    xmlSchemaNotationRef = S4_object()), .Names = c("xmlSchemaRef", 
"xmlSchemaElementRef", "xmlSchemaTypeRef", "xmlSchemaAttributeRef", 
"xmlSchemaAttributeGroupRef", "xmlSchemaNotationRef"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__XMLInternalPINode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLInternalPINode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


newXMLDTDNode <- function (nodeName, externalID = character(), systemID = character(), 
    doc = NULL, addFinalizer = NA) 
{
    if (length(nodeName) > 1 && missing(externalID)) 
        externalID = nodeName[2]
    if (length(nodeName) > 2 && missing(systemID)) 
        systemID = nodeName[3]
    .Call("R_newXMLDtd", doc, as.character(nodeName), as.character(externalID), 
        as.character(systemID), addFinalizer, PACKAGE = "XML")
}


xmlStructuredStop <- function (msg, code, domain, line, col, level, filename, class = "XMLError") 
{
    err = makeXMLError(msg, code, domain, line, col, level, filename, 
        class)
    stop(err)
}


catalogAdd <- function (orig, replace, type = "rewriteURI") 
{
    if (missing(replace)) {
        replace = orig
        orig = names(replace)
    }
    else length(replace) = length(orig)
    idx = pmatch(type, XMLCatalogTypes)
    if (any(is.na(idx))) {
        stop("unrecognized XML catalog type(s) ", type[is.na(idx)], 
            ". Must be one of ", paste("'", XMLCatalogTypes, 
                "'", sep = "", collapse = ", "))
    }
    type = XMLCatalogTypes[idx]
    type = rep(as.character(type), length = length(orig))
    xmlInitializeCatalog()
    .Call("RS_XML_catalogAdd", as.character(orig), as.character(replace), 
        as.character(type), PACKAGE = "XML")
}


toString.XMLNode <- function (x, ...) 
{
    .tempXMLOutput = ""
    con <- textConnection(".tempXMLOutput", "w", local = TRUE)
    sink(con)
    print(x)
    sink()
    close(con)
    paste(.tempXMLOutput, collapse = "\n")
}


getSibling <- function (node, after = TRUE, ...) 
UseMethod("getSibling")


.__C__XMLInternalDocument <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLAbstractDocument = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLAbstractDocument", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLInternalDocument", package = "XML")
    , package = "XML"
    , subclasses = structure(list(HTMLInternalDocument = S4_object(), 
    XMLCodeDoc = S4_object()), .Names = c("HTMLInternalDocument", 
"XMLCodeDoc"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


htmlTreeParse <- function (file, ignoreBlanks = TRUE, handlers = NULL, replaceEntities = FALSE, 
    asText = FALSE, trim = TRUE, validate = FALSE, getDTD = TRUE, 
    isURL = FALSE, asTree = FALSE, addAttributeNamespaces = FALSE, 
    useInternalNodes = FALSE, isSchema = FALSE, fullNamespaceInfo = FALSE, 
    encoding = character(), useDotNames = length(grep("^\\.", 
        names(handlers))) > 0, xinclude = TRUE, addFinalizer = TRUE, 
    error = htmlErrorHandler, isHTML = TRUE, options = integer(), 
    parentFirst = FALSE) 
{
    isMissingAsText = missing(asText)
    if (length(file) > 1) {
        file = paste(file, collapse = "\n")
        if (!missing(asText) && !asText) 
            stop(structure(list(message = "multiple URLs passed to xmlTreeParse. If this is the content of the file, specify asText = TRUE"), 
                class = c("MultipleURLError", "XMLParserError", 
                  "simpleError", "error", "condition")))
        asText = TRUE
    }
    if (missing(isURL) && !asText) 
        isURL <- length(grep("^(http|ftp|file)://", file, useBytes = TRUE, 
            perl = TRUE))
    if (isHTML) {
        validate = FALSE
        getDTD = FALSE
        isSchema = FALSE
        docClass = "HTMLInternalDocument"
    }
    else docClass = character()
    checkHandlerNames(handlers, "DOM")
    if (missing(fullNamespaceInfo) && inherits(handlers, "RequiresNamespaceInfo")) 
        fullNamespaceInfo = TRUE
    oldValidate = xmlValidity()
    xmlValidity(validate)
    on.exit(xmlValidity(oldValidate))
    if (!asText && isURL == FALSE) {
        if (file.exists(file) == FALSE) 
            if (!missing(asText) && asText == FALSE) {
                e = simpleError(paste("File", file, "does not exist"))
                class(e) = c("FileNotFound", class(e))
                stop(e)
            }
            else asText <- TRUE
    }
    if (asText && length(file) > 1) 
        file = paste(file, collapse = "\n")
    old = setEntitySubstitution(replaceEntities)
    on.exit(setEntitySubstitution(old), add = TRUE)
    if (asText && length(grep(sprintf("^%s?\\s*<", BOMRegExp), 
        file, perl = TRUE, useBytes = TRUE)) == 0) {
        if (!isHTML || (isMissingAsText && !inherits(file, "AsIs"))) {
            e = simpleError(paste("XML content does not seem to be XML:", 
                sQuote(file)))
            class(e) = c("XMLInputError", class(e))
            (if (isHTML) 
                warning
            else stop)(e)
        }
    }
    if (!is.logical(xinclude)) {
        xinclude = as.logical(xinclude)
    }
    if (!asText && !isURL) 
        file = path.expand(as.character(file))
    if (useInternalNodes && trim) {
        prevBlanks = .Call("RS_XML_setKeepBlanksDefault", 0L, 
            PACKAGE = "XML")
        on.exit(.Call("RS_XML_setKeepBlanksDefault", prevBlanks, 
            PACKAGE = "XML"), add = TRUE)
    }
    .oldErrorHandler = setXMLErrorHandler(error)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    if (length(options)) 
        options = sum(options)
    ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
        as.logical(ignoreBlanks), as.logical(replaceEntities), 
        as.logical(asText), as.logical(trim), as.logical(validate), 
        as.logical(getDTD), as.logical(isURL), as.logical(addAttributeNamespaces), 
        as.logical(useInternalNodes), as.logical(isHTML), as.logical(isSchema), 
        as.logical(fullNamespaceInfo), as.character(encoding), 
        as.logical(useDotNames), xinclude, error, addFinalizer, 
        as.integer(options), as.logical(parentFirst), PACKAGE = "XML")
    if (!missing(handlers) && length(handlers) && !as.logical(asTree)) 
        return(handlers)
    if (!isSchema && length(class(ans))) 
        class(ans) = c(docClass, oldClass(class(ans)))
    if (inherits(ans, "XMLInternalDocument")) 
        addDocFinalizer(ans, addFinalizer)
    else if (!getDTD && !isSchema) {
        class(ans) = oldClass("XMLDocumentContent")
    }
    ans
}


newXMLNamespace <- function (node, namespace, prefix = names(namespace), set = FALSE) 
{
    if (is.null(namespace)) 
        return(NULL)
    ns <- .Call("R_xmlNewNs", node, namespace, as.character(prefix), 
        PACKAGE = "XML")
    if (set) 
        setXMLNamespace(node, ns)
    ns
}


NOBLANKS <- 256


.__C__XMLInternalNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLAbstractNode", 
"oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLInternalNode", package = "XML")
    , package = "XML"
    , subclasses = structure(list(XMLInternalCDataNode = S4_object(), 
    XMLInternalPINode = S4_object(), 
    XMLInternalCommentNode = S4_object(), 
    XMLInternalElementNode = S4_object(), 
    XMLInternalTextNode = S4_object(), 
    XMLXIncludeStartNode = S4_object(), 
    XMLXIncludeEndNode = S4_object(), 
    XMLEntityDeclNode = S4_object(), 
    XMLAttributeDeclNode = S4_object(), 
    XMLDocumentNode = S4_object(), 
    XMLDocumentTypeNode = S4_object(), 
    XMLDocumentFragNode = S4_object(), 
    XMLNamespaceDeclNode = S4_object(), 
    XMLAttributeNode = S4_object(), 
    XMLDTDNode = S4_object()), .Names = c("XMLInternalCDataNode", 
"XMLInternalPINode", "XMLInternalCommentNode", "XMLInternalElementNode", 
"XMLInternalTextNode", "XMLXIncludeStartNode", "XMLXIncludeEndNode", 
"XMLEntityDeclNode", "XMLAttributeDeclNode", "XMLDocumentNode", 
"XMLDocumentTypeNode", "XMLDocumentFragNode", "XMLNamespaceDeclNode", 
"XMLAttributeNode", "XMLDTDNode"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


dtdValidElement <- function (name, within, dtd, pos = NULL) 
{
    el <- dtdElement(within, dtd)
    if (is.null(el)) 
        stop(paste("No such element \"", within, "\" in DTD", 
            sep = "", collapse = ""))
    return(dtdElementValidEntry(el, name, pos = pos))
}


xpathSApply <- function (doc, path, fun = NULL, ..., namespaces = xmlNamespaceDefinitions(doc, 
    simplify = TRUE), resolveNamespaces = TRUE, simplify = TRUE, 
    addFinalizer = NA) 
{
    answer = xpathApply(doc, path, fun, ..., namespaces = namespaces, 
        resolveNamespaces = resolveNamespaces, addFinalizer = addFinalizer)
    if (simplify && length(answer) && length(common.len <- unique(unlist(lapply(answer, 
        length)))) == 1) {
        if (common.len == 1) 
            unlist(answer, recursive = FALSE)
        else if (common.len > 1) 
            array(unlist(answer, recursive = FALSE), dim = c(common.len, 
                length(answer)), dimnames = if (!(is.null(n1 <- names(answer[[1]])) & 
                is.null(n2 <- names(answer)))) 
                list(n1, n2))
        else answer
    }
    else answer
}


.__C__XMLAbstractNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLAbstractNode", package = "XML")
    , package = "XML"
    , subclasses = structure(list(RXMLAbstractNode = S4_object(), 
    XMLInternalNode = S4_object(), 
    XMLHashTreeNode = S4_object(), 
    XMLNode = S4_object(), 
    XMLTextNode = S4_object(), 
    XMLPINode = S4_object(), 
    XMLCommentNode = S4_object(), 
    XMLProcessingInstruction = S4_object(), 
    XMLCDataNode = S4_object(), 
    XMLTreeNode = S4_object(), 
    XMLInternalCDataNode = S4_object(), 
    XMLInternalPINode = S4_object(), 
    XMLInternalCommentNode = S4_object(), 
    XMLInternalElementNode = S4_object(), 
    XMLInternalTextNode = S4_object(), 
    XMLXIncludeStartNode = S4_object(), 
    XMLXIncludeEndNode = S4_object(), 
    XMLEntityDeclNode = S4_object(), 
    XMLAttributeDeclNode = S4_object(), 
    XMLDocumentNode = S4_object(), 
    XMLDocumentTypeNode = S4_object(), 
    XMLDocumentFragNode = S4_object(), 
    XMLNamespaceDeclNode = S4_object(), 
    XMLAttributeNode = S4_object(), 
    XMLDTDNode = S4_object()), .Names = c("RXMLAbstractNode", 
"XMLInternalNode", "XMLHashTreeNode", "XMLNode", "XMLTextNode", 
"XMLPINode", "XMLCommentNode", "XMLProcessingInstruction", "XMLCDataNode", 
"XMLTreeNode", "XMLInternalCDataNode", "XMLInternalPINode", "XMLInternalCommentNode", 
"XMLInternalElementNode", "XMLInternalTextNode", "XMLXIncludeStartNode", 
"XMLXIncludeEndNode", "XMLEntityDeclNode", "XMLAttributeDeclNode", 
"XMLDocumentNode", "XMLDocumentTypeNode", "XMLDocumentFragNode", 
"XMLNamespaceDeclNode", "XMLAttributeNode", "XMLDTDNode"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__xmlSchemaAttributeRef <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = structure(list(ExternalReference = S4_object()), .Names = "ExternalReference")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("xmlSchemaAttributeRef", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


addChildren <- function (node, ..., kids = list(...), at = NA, cdata = FALSE, 
    append = TRUE) 
UseMethod("addChildren")


addSibling <- function (node, ..., kids = list(...), after = NA) 
{
    UseMethod("addSibling")
}


entityDeclaration.SAX <- function (name, base, sysId, publicId, notationName, .state = NULL) 
{
    standardGeneric("entityDeclaration.SAX")
}


xmlDOMApply <- function (dom, func) 
{
    .Call("RS_XML_RecursiveApply", dom, func, NULL, PACKAGE = "XML")
}


getXIncludes <- function (filename, recursive = TRUE, skip = character(), omitPattern = "\\.(js|html?|txt|R|c)$", 
    namespace = c(xi = "http://www.w3.org/2003/XInclude"), duplicated = TRUE) 
{
    doc = xmlParse(filename, xinclude = FALSE)
    if (missing(namespace)) {
        ns = xmlNamespaceDefinitions(doc, simplify = TRUE)
        if ("http://www.w3.org/2001/XInclude" %in% ns) 
            namespace = c(xi = "http://www.w3.org/2001/XInclude")
    }
    nodes = getNodeSet(doc, "//xi:include", namespaces = namespace)
    files = sapply(nodes, xmlGetAttr, "href")
    nonRecursive = as.logical(sapply(nodes, xmlGetAttr, "text", 
        FALSE))
    if (length(omitPattern)) 
        nonRecursive = grepl(omitPattern, files) | nonRecursive
    if (recursive) {
        processed = c(filename, skip)
        for (f in unique(files[!nonRecursive])) {
            f = getRelativeURL(f, filename)
            if (file.exists(f)) 
                files = c(files, getXIncludes(f, TRUE, skip = processed))
            else warning(f, " doesn't exist")
            processed = c(processed, f)
        }
    }
    files = unlist(files)
    if (!duplicated) 
        unique(files)
    else files
}


removeNodes <- function (node, free = rep(FALSE, length(node))) 
UseMethod("removeNodes")


xmlCommentNode <- function (text) 
{
    node <- xmlTextNode(text)
    class(node) <- oldClass("XMLCommentNode")
    node
}


.__C__URI <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("URI", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__source:base` <- "<environment>"

.__C__XMLXIncludeEndNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLXIncludeEndNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`docName<-` <- function (x, value) 
standardGeneric("docName<-")


readKeyValueDB <- function (doc, ...) 
standardGeneric("readKeyValueDB")


xmlParse <- function (file, ignoreBlanks = TRUE, handlers = NULL, replaceEntities = FALSE, 
    asText = FALSE, trim = TRUE, validate = FALSE, getDTD = TRUE, 
    isURL = FALSE, asTree = FALSE, addAttributeNamespaces = FALSE, 
    useInternalNodes = TRUE, isSchema = FALSE, fullNamespaceInfo = FALSE, 
    encoding = character(), useDotNames = length(grep("^\\.", 
        names(handlers))) > 0, xinclude = TRUE, addFinalizer = TRUE, 
    error = xmlErrorCumulator(), isHTML = FALSE, options = integer(), 
    parentFirst = FALSE) 
{
    isMissingAsText = missing(asText)
    if (length(file) > 1) {
        file = paste(file, collapse = "\n")
        if (!missing(asText) && !asText) 
            stop(structure(list(message = "multiple URLs passed to xmlTreeParse. If this is the content of the file, specify asText = TRUE"), 
                class = c("MultipleURLError", "XMLParserError", 
                  "simpleError", "error", "condition")))
        asText = TRUE
    }
    if (missing(isURL) && !asText) 
        isURL <- length(grep("^(http|ftp|file)://", file, useBytes = TRUE, 
            perl = TRUE))
    if (isHTML) {
        validate = FALSE
        getDTD = FALSE
        isSchema = FALSE
        docClass = "HTMLInternalDocument"
    }
    else docClass = character()
    checkHandlerNames(handlers, "DOM")
    if (missing(fullNamespaceInfo) && inherits(handlers, "RequiresNamespaceInfo")) 
        fullNamespaceInfo = TRUE
    oldValidate = xmlValidity()
    xmlValidity(validate)
    on.exit(xmlValidity(oldValidate))
    if (!asText && isURL == FALSE) {
        if (file.exists(file) == FALSE) 
            if (!missing(asText) && asText == FALSE) {
                e = simpleError(paste("File", file, "does not exist"))
                class(e) = c("FileNotFound", class(e))
                stop(e)
            }
            else asText <- TRUE
    }
    if (asText && length(file) > 1) 
        file = paste(file, collapse = "\n")
    old = setEntitySubstitution(replaceEntities)
    on.exit(setEntitySubstitution(old), add = TRUE)
    if (asText && length(grep(sprintf("^%s?\\s*<", BOMRegExp), 
        file, perl = TRUE, useBytes = TRUE)) == 0) {
        if (!isHTML || (isMissingAsText && !inherits(file, "AsIs"))) {
            e = simpleError(paste("XML content does not seem to be XML:", 
                sQuote(file)))
            class(e) = c("XMLInputError", class(e))
            (if (isHTML) 
                warning
            else stop)(e)
        }
    }
    if (!is.logical(xinclude)) {
        xinclude = as.logical(xinclude)
    }
    if (!asText && !isURL) 
        file = path.expand(as.character(file))
    if (useInternalNodes && trim) {
        prevBlanks = .Call("RS_XML_setKeepBlanksDefault", 0L, 
            PACKAGE = "XML")
        on.exit(.Call("RS_XML_setKeepBlanksDefault", prevBlanks, 
            PACKAGE = "XML"), add = TRUE)
    }
    .oldErrorHandler = setXMLErrorHandler(error)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    if (length(options)) 
        options = sum(options)
    ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
        as.logical(ignoreBlanks), as.logical(replaceEntities), 
        as.logical(asText), as.logical(trim), as.logical(validate), 
        as.logical(getDTD), as.logical(isURL), as.logical(addAttributeNamespaces), 
        as.logical(useInternalNodes), as.logical(isHTML), as.logical(isSchema), 
        as.logical(fullNamespaceInfo), as.character(encoding), 
        as.logical(useDotNames), xinclude, error, addFinalizer, 
        as.integer(options), as.logical(parentFirst), PACKAGE = "XML")
    if (!missing(handlers) && length(handlers) && !as.logical(asTree)) 
        return(handlers)
    if (!isSchema && length(class(ans))) 
        class(ans) = c(docClass, oldClass(class(ans)))
    if (inherits(ans, "XMLInternalDocument")) 
        addDocFinalizer(ans, addFinalizer)
    else if (!getDTD && !isSchema) {
        class(ans) = oldClass("XMLDocumentContent")
    }
    ans
}


xmlSApply <- function (X, FUN, ...) 
{
    UseMethod("xmlSApply")
}


docName <- function (doc, ...) 
standardGeneric("docName")


processXInclude <- function (node, flags = 0L) 
UseMethod("processXInclude")


OLD10 <- 131072


`.__T__names:base` <- "<environment>"

newXMLDoc <- function (dtd = "", namespaces = NULL, addFinalizer = TRUE, name = character(), 
    node = NULL, isHTML = FALSE) 
{
    if (is(dtd, "XMLInternalNode")) {
        dtdNode = dtd
        dtd = character()
    }
    else dtdNode = NULL
    ans = .Call("R_newXMLDoc", dtd, namespaces, as.logical(isHTML), 
        PACKAGE = "XML")
    class(ans) = oldClass(class(ans))
    addDocFinalizer(ans, addFinalizer)
    if (length(name)) 
        docName(ans) = as.character(name)
    if (length(dtdNode)) 
        addChildren(ans, dtdNode)
    if (length(node)) {
        if (is.character(node)) 
            newXMLTextNode(node, addFinalizer = FALSE, parent = doc)
        else addChildren(ans, node)
    }
    ans
}


newXMLCommentNode <- function (text, parent = NULL, doc = NULL, at = NA, addFinalizer = NA) 
{
    a = .Call("R_xmlNewComment", as.character(text), doc, addFinalizer, 
        PACKAGE = "XML")
    if (!is.null(parent)) 
        addChildren(parent, a, at = at)
    a
}


NODICT <- 4096


catalogDump <- function (fileName = tempfile(), asText = TRUE) 
{
    xmlInitializeCatalog()
    ans = .Call("RS_XML_catalogDump", as.character(fileName), 
        PACKAGE = "XML")
    if (missing(fileName)) {
        ans = xmlParse(fileName)
        if (asText) 
            ans = saveXML(ans)
        unlink(fileName)
    }
    ans
}


.__C__XMLNamespaceDefinitions <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLNamespaceDefinitions", package = "XML")
    , package = "XML"
    , subclasses = structure(list(SimplifiedXMLNamespaceDefinitions = S4_object()), .Names = "SimplifiedXMLNamespaceDefinitions")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__processingInstruction.SAX:XML` <- "<environment>"

supportsExpat <- function () 
{
    FALSE
}


PEDANTIC <- 128


.__C__XMLString <- new("classRepresentation"
    , slots = structure(list(.Data = structure("character", package = "methods")), .Names = ".Data")
    , contains = structure(list(character = S4_object(), 
    vector = S4_object(), 
    data.frameRowLabels = S4_object(), 
    SuperClassMethod = S4_object()), .Names = c("character", 
"vector", "data.frameRowLabels", "SuperClassMethod"))
    , virtual = FALSE
    , prototype = new("character"
)
    , validity = NULL
    , access = list()
    , className = structure("XMLString", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


getLineNumber <- function (node, ...) 
{
    if (!is(node, "XMLInternalNode")) 
        stop("This must be an C-level/native/internal XML node, i.e. of class 'XMLInternalNode'. Got ", 
            paste(class(node), collapse = ", "))
    .Call("R_getLineNumber", node, PACKAGE = "XML")
}


`.__T__coerce:methods` <- methods::`.__T__coerce:methods` # re-exported from methods package

xmlNativeTreeParse <- function (file, ignoreBlanks = TRUE, handlers = NULL, replaceEntities = FALSE, 
    asText = FALSE, trim = TRUE, validate = FALSE, getDTD = TRUE, 
    isURL = FALSE, asTree = FALSE, addAttributeNamespaces = FALSE, 
    useInternalNodes = TRUE, isSchema = FALSE, fullNamespaceInfo = FALSE, 
    encoding = character(), useDotNames = length(grep("^\\.", 
        names(handlers))) > 0, xinclude = TRUE, addFinalizer = TRUE, 
    error = xmlErrorCumulator(), isHTML = FALSE, options = integer(), 
    parentFirst = FALSE) 
{
    isMissingAsText = missing(asText)
    if (length(file) > 1) {
        file = paste(file, collapse = "\n")
        if (!missing(asText) && !asText) 
            stop(structure(list(message = "multiple URLs passed to xmlTreeParse. If this is the content of the file, specify asText = TRUE"), 
                class = c("MultipleURLError", "XMLParserError", 
                  "simpleError", "error", "condition")))
        asText = TRUE
    }
    if (missing(isURL) && !asText) 
        isURL <- length(grep("^(http|ftp|file)://", file, useBytes = TRUE, 
            perl = TRUE))
    if (isHTML) {
        validate = FALSE
        getDTD = FALSE
        isSchema = FALSE
        docClass = "HTMLInternalDocument"
    }
    else docClass = character()
    checkHandlerNames(handlers, "DOM")
    if (missing(fullNamespaceInfo) && inherits(handlers, "RequiresNamespaceInfo")) 
        fullNamespaceInfo = TRUE
    oldValidate = xmlValidity()
    xmlValidity(validate)
    on.exit(xmlValidity(oldValidate))
    if (!asText && isURL == FALSE) {
        if (file.exists(file) == FALSE) 
            if (!missing(asText) && asText == FALSE) {
                e = simpleError(paste("File", file, "does not exist"))
                class(e) = c("FileNotFound", class(e))
                stop(e)
            }
            else asText <- TRUE
    }
    if (asText && length(file) > 1) 
        file = paste(file, collapse = "\n")
    old = setEntitySubstitution(replaceEntities)
    on.exit(setEntitySubstitution(old), add = TRUE)
    if (asText && length(grep(sprintf("^%s?\\s*<", BOMRegExp), 
        file, perl = TRUE, useBytes = TRUE)) == 0) {
        if (!isHTML || (isMissingAsText && !inherits(file, "AsIs"))) {
            e = simpleError(paste("XML content does not seem to be XML:", 
                sQuote(file)))
            class(e) = c("XMLInputError", class(e))
            (if (isHTML) 
                warning
            else stop)(e)
        }
    }
    if (!is.logical(xinclude)) {
        xinclude = as.logical(xinclude)
    }
    if (!asText && !isURL) 
        file = path.expand(as.character(file))
    if (useInternalNodes && trim) {
        prevBlanks = .Call("RS_XML_setKeepBlanksDefault", 0L, 
            PACKAGE = "XML")
        on.exit(.Call("RS_XML_setKeepBlanksDefault", prevBlanks, 
            PACKAGE = "XML"), add = TRUE)
    }
    .oldErrorHandler = setXMLErrorHandler(error)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    if (length(options)) 
        options = sum(options)
    ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
        as.logical(ignoreBlanks), as.logical(replaceEntities), 
        as.logical(asText), as.logical(trim), as.logical(validate), 
        as.logical(getDTD), as.logical(isURL), as.logical(addAttributeNamespaces), 
        as.logical(useInternalNodes), as.logical(isHTML), as.logical(isSchema), 
        as.logical(fullNamespaceInfo), as.character(encoding), 
        as.logical(useDotNames), xinclude, error, addFinalizer, 
        as.integer(options), as.logical(parentFirst), PACKAGE = "XML")
    if (!missing(handlers) && length(handlers) && !as.logical(asTree)) 
        return(handlers)
    if (!isSchema && length(class(ans))) 
        class(ans) = c(docClass, oldClass(class(ans)))
    if (inherits(ans, "XMLInternalDocument")) 
        addDocFinalizer(ans, addFinalizer)
    else if (!getDTD && !isSchema) {
        class(ans) = oldClass("XMLDocumentContent")
    }
    ans
}


processingInstruction.SAX <- function (target, content, .state = NULL) 
{
    standardGeneric("processingInstruction.SAX")
}


xmlNamespace <- function (x) 
{
    UseMethod("xmlNamespace")
}


.__C__XMLXIncludeStartNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLXIncludeStartNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


libxmlFeatures <- function () 
{
    .Call("R_getXMLFeatures", PACKAGE = "XML")
}


`.__T__xmlSourceSection:XML` <- "<environment>"

.__C__XMLNamespace <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLNamespace", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlSource <- function (url, ..., envir = globalenv(), xpath = character(), 
    ids = character(), omit = character(), ask = FALSE, example = NA, 
    fatal = TRUE, verbose = TRUE, echo = verbose, print = echo, 
    xnodes = DefaultXMLSourceXPath, namespaces = DefaultXPathNamespaces, 
    section = character(), eval = TRUE, init = TRUE, setNodeNames = FALSE, 
    parse = TRUE, force = FALSE) 
{
    standardGeneric("xmlSource")
}


getHTMLLinks <- function (doc, externalOnly = TRUE, xpQuery = "//a/@href", baseURL = docName(doc), 
    relative = FALSE) 
{
    if (is.character(doc)) 
        doc = htmlParse(doc)
    if (is(doc, "XMLInternalNode") && grepl("^/", xpQuery)) 
        xpQuery = sprintf(".%s", xpQuery)
    links = as.character(getNodeSet(doc, xpQuery))
    links = if (externalOnly) 
        grep("^#", links, value = TRUE, invert = TRUE)
    else links
    if (relative) 
        sapply(links, getRelativeURL, baseURL)
    else links
}


parseDTD <- function (extId, asText = FALSE, name = "", isURL = FALSE, error = xmlErrorCumulator()) 
{
    extId <- as.character(extId)
    if (!asText && missing(isURL)) {
        isURL <- length(grep("(http|ftp)://", extId, useBytes = TRUE)) > 
            0
    }
    if (missing(name)) 
        name <- extId
    .oldErrorHandler = setXMLErrorHandler(error)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    if (asText) {
        f <- gsub("\\", "/", tempfile(), fixed = TRUE)
        cat(extId, "\n", file = f)
        extId = f
        asText = FALSE
    }
    .Call("RS_XML_getDTD", as.character(name), as.character(extId), 
        as.logical(asText), as.logical(isURL), error, PACKAGE = "XML")
}


`.__T__xmlSourceFunctions:XML` <- "<environment>"

xmlNamespaces <- function (x, addNames = TRUE, recursive = FALSE, simplify = FALSE, 
    ...) 
{
    UseMethod("xmlNamespaceDefinitions")
}


.__C__FormattedInteger <- new("classRepresentation"
    , slots = structure(list(.Data = structure("integer", package = "methods")), .Names = ".Data")
    , contains = structure(list(integer = S4_object(), 
    numeric = S4_object(), 
    vector = S4_object(), 
    data.frameRowLabels = S4_object()), .Names = c("integer", 
"numeric", "vector", "data.frameRowLabels"))
    , virtual = FALSE
    , prototype = new("integer"
)
    , validity = NULL
    , access = list()
    , className = structure("FormattedInteger", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


asXMLNode <- function (x) 
{
    if (!inherits(x, "XMLNode")) {
        xmlTextNode(x)
    }
    else {
        x
    }
}


xmlPINode <- function (sys, value, namespace = "") 
{
    x <- xmlNode(name = sys, namespace = namespace)
    x$value <- value
    class(x) <- oldClass("XMLProcessingInstruction")
    x
}


xmlClone <- function (node, recursive = TRUE, addFinalizer = FALSE, ...) 
standardGeneric("xmlClone")


.__C__XMLCodeFile <- new("classRepresentation"
    , slots = structure(list(.Data = structure("character", package = "methods")), .Names = ".Data")
    , contains = structure(list(character = S4_object(), 
    vector = S4_object(), 
    data.frameRowLabels = S4_object(), 
    SuperClassMethod = S4_object()), .Names = c("character", 
"vector", "data.frameRowLabels", "SuperClassMethod"))
    , virtual = FALSE
    , prototype = new("character"
)
    , validity = NULL
    , access = list()
    , className = structure("XMLCodeFile", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__xmlSource:XML` <- "<environment>"

supportsLibxml <- function () 
{
    TRUE
}


xmlNamespaceDefinitions <- function (x, addNames = TRUE, recursive = FALSE, simplify = FALSE, 
    ...) 
{
    UseMethod("xmlNamespaceDefinitions")
}


free <- function (obj) 
standardGeneric("free")


source <- base::source # re-exported from base package

.__C__XMLInternalElementNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLInternalElementNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__xmlChildren<-:XML` <- "<environment>"

xmlOutputDOM <- function (tag = "doc", attrs = NULL, dtd = NULL, nameSpace = NULL, 
    nsURI = character(0), xmlDeclaration = NULL) 
{
    buf <- NULL
    current <- NULL
    startingNode = 1
    if (is.logical(xmlDeclaration) && xmlDeclaration) 
        xmlDeclaration = xmlPINode("xml", "version = \"1.0\"")
    else if (is.character(xmlDeclaration)) {
        if (length(grep("version *=", xmlDeclaration)) == 0) 
            xmlDeclaration = paste(xmlDeclaration, "version='1.0'")
        xmlDeclaration = xmlPINode("xml", xmlDeclaration)
    }
    if (length(dtd)) 
        dtd = paste("<!DOCTYPE", tag, "SYSTEM", ddQuote(dtd[1]), 
            if (length(dtd) > 1) 
                paste("PUBLIC", ddQuote(dtd[2])), ">")
    reset <- function() {
        buf <<- xmlNode(tag, attrs = attrs, namespace = nameSpace)
        if (length(nsURI) > 0) {
            names(nsURI) <- paste("xmlns", names(nsURI), sep = ":")
            buf$attributes <<- nsURI
        }
        current <<- integer(0)
        invisible(buf)
    }
    reset()
    addTag <- function(tag, ..., attrs = NULL, close = TRUE, 
        namespace = NULL, .children = list(...)) {
        if (missing(namespace)) 
            namespace <- nameSpace
        addNode(n <- xmlNode(tag, attrs = attrs, namespace = namespace, 
            .children = .children))
        if (close == FALSE) {
            current <<- c(current, xmlSize(getCurrent()))
        }
        invisible(n)
    }
    getCurrentExpr <- function() {
        if (length(current) > 0) {
            p <- seq(2, length = length(current) - 1)
            kall <- call("[[", as.name("buf"), current[1])
            for (i in p) {
                kall <- call("[[", kall, current[i])
            }
        }
        else kall <- as.name("buf")
        kall
    }
    getCurrent <- function() {
        eval(getCurrentExpr())
    }
    addNode <- function(node) {
        kall <- getCurrentExpr()
        if (length(current) > 0) {
            lhs <- kall
            kall <- call("append.xmlNode", kall, node)
            kall <- call("<<-", lhs, kall)
        }
        else {
            kall <- call("append.xmlNode", kall, node)
        }
        val <- eval(kall)
        if (length(current) == 0) 
            buf <<- val
        invisible(node)
    }
    addComment <- function(...) {
        addNode(xmlCommentNode(paste(sapply(list(...), as.character), 
            sep = "")))
    }
    addCData <- function(text) {
        addNode(xmlCDataNode(text))
    }
    addPI <- function(name, text) {
        addNode(xmlPINode(name, text))
    }
    addText <- function(text, namespace = "") {
        addNode(xmlTextNode(text, namespace))
    }
    closeTag <- function(name = "", namespace = NULL) {
        current <<- current[-length(current)]
    }
    getValue = function() {
        if (!is.null(xmlDeclaration)) 
            structure(list(xmlDeclaration = xmlDeclaration, root = buf, 
                doctype = dtd), class = "XMLRDocument")
        else buf
    }
    con <- list(value = getValue, addTag = addTag, addEndTag = function(name) {
        closeTag(name)
    }, closeTag = closeTag, reset = reset, addNode = addNode, 
        add = function(...) {
        }, addComment = addComment, addPI = addPI, addCData = addCData, 
        current = function() {
            current
        })
    ans = new("XMLOutputDOM", con)
    names(ans) = names(con)
    ans
}


`.__T__xmlClone:XML` <- "<environment>"

`.__T__xmlNamespaces<-:XML` <- "<environment>"

xmlCodeFile <- function (f, parse = FALSE) 
{
    if (parse) 
        new("XMLCodeDoc", xmlParse(f))
    else new("XMLCodeFile", f)
}


isXMLString <- function (str) 
{
    is(str, "XMLString") || length(grep("<([a-zA-Z]+:)?[a-zA-Z]+(/?>| [a-zA-Z]+=[\"'])", 
        str)) > 0
}


.__C__SchemaAttributeTable <- new("classRepresentation"
    , slots = structure(list(ref = structure("ExternalReference", package = "XML")), .Names = "ref")
    , contains = structure(list(libxmlTypeTable = S4_object()), .Names = "libxmlTypeTable")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("SchemaAttributeTable", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__Percent <- new("classRepresentation"
    , slots = structure(list(.Data = structure("numeric", package = "methods")), .Names = ".Data")
    , contains = structure(list(numeric = S4_object(), 
    vector = S4_object()), .Names = c("numeric", 
"vector"))
    , virtual = FALSE
    , prototype = new("numeric"
)
    , validity = NULL
    , access = list()
    , className = structure("Percent", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


readHTMLList <- function (doc, trim = TRUE, elFun = xmlValue, which = integer(), 
    ...) 
standardGeneric("readHTMLList")


removeXMLNamespaces <- function (node, ..., all = FALSE, .els = unlist(list(...))) 
standardGeneric("removeXMLNamespaces")


newXMLNode <- function (name, ..., attrs = NULL, namespace = character(), namespaceDefinitions = character(), 
    doc = NULL, .children = list(...), parent = NULL, at = NA, 
    cdata = FALSE, suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", 
        FALSE), sibling = NULL, addFinalizer = NA, noNamespace = length(namespace) == 
        0 && !missing(namespace), fixNamespaces = c(dummy = TRUE, 
        default = TRUE)) 
{
    if (length(attrs)) {
        ids = names(attrs)
        attrs = structure(as(attrs, "character"), names = ids)
        i = grep("^xmlns", names(attrs))
        if (length(i)) {
            warning("Don't specify namespace definitions via 'attrs'; use namespaceDefinitions")
            namespace = c(namespace, structure(attrs[i], names = gsub("^xmlns:", 
                "", names(attrs)[i])))
            attrs = attrs[-i]
        }
    }
    else attrs = character()
    ns = character()
    name = strsplit(name, ":")[[1]]
    if (length(name) == 2) {
        ns = name[1]
        name = name[2]
        noNamespace = FALSE
    }
    if (is.list(parent)) {
        if (length(parent) < 1 || !(is(parent[[1]], "XMLInternalElementNode") || 
            is(parent[[1]], "XMLInternalDocument"))) 
            stop("incorrect value for parent")
        parent = parent[[1]]
    }
    if (missing(doc) && !missing(parent) && inherits(parent, 
        "XMLInternalDocument")) {
        doc = parent
        parent = NULL
    }
    if (is.null(doc) && !is.null(parent)) {
        doc = if (inherits(parent, "XMLInternalDocument")) 
            parent
        else .Call("R_getXMLNodeDocument", parent, PACKAGE = "XML")
    }
    node <- .Call("R_newXMLNode", as.character(name), character(), 
        character(), doc, namespaceDefinitions, addFinalizer, 
        PACKAGE = "XML")
    if (!is.null(sibling)) 
        addSibling(sibling, node, after = as.logical(at))
    else if (!is.null(parent)) 
        addChildren(parent, node, at = at)
    if (TRUE) {
        nsDefs = lapply(seq(along = namespaceDefinitions), function(i) newNamespace(node, 
            namespaceDefinitions[[i]], names(namespaceDefinitions)[i], 
            set = FALSE))
        if (length(namespaceDefinitions)) 
            names(nsDefs) = if (length(names(namespaceDefinitions))) 
                names(namespaceDefinitions)
            else ""
    }
    else nsDefs = xmlNamespaceDefinitions(node)
    addAttributes(node, .attrs = attrs, suppressNamespaceWarning = suppressNamespaceWarning)
    if (is(namespace, "XMLNamespaceRef")) {
        setInternalNamespace(node, namespace)
    }
    else if (is.na(noNamespace) || !noNamespace) {
        ns = getNodeNamespace(ns, nsDefs, node, namespace, noNamespace, 
            namespaceDefinitions, parent, suppressNamespaceWarning)
        if (is.null(ns)) 
            !.Call("R_setNamespaceFromAncestors", node, PACKAGE = "XML")
    }
    if (length(ns) && (inherits(ns, c("XMLNamespaceRef", "XMLNamespaceDeclaration")) || 
        (is.character(ns) && ns != ""))) 
        setXMLNamespace(node, ns)
    if (length(.children)) {
        if (!is.list(.children)) 
            .children = list(.children)
        addChildren(node, kids = .children, cdata = cdata, addFinalizer = addFinalizer)
    }
    if (any(fixNamespaces)) {
        xmlFixNamespaces(node, fixNamespaces)
    }
    node
}


`.__T__[[:base` <- "<environment>"

`.__T__xmlNamespace<-:XML` <- "<environment>"

.__C__SchemaTypeTable <- new("classRepresentation"
    , slots = structure(list(ref = structure("ExternalReference", package = "XML")), .Names = "ref")
    , contains = structure(list(libxmlTypeTable = S4_object()), .Names = "libxmlTypeTable")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("SchemaTypeTable", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`xmlNamespaces<-` <- function (x, append = TRUE, set = FALSE, value) 
standardGeneric("xmlNamespaces<-")


.__C__XMLDocumentFragNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLDocumentFragNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__endElement.SAX:XML` <- "<environment>"

.__C__XMLAbstractDocument <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(oldClass = S4_object()), .Names = "oldClass")
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLAbstractDocument", package = "XML")
    , package = "XML"
    , subclasses = structure(list(XMLInternalDocument = S4_object(), 
    XMLHashTree = S4_object(), 
    XMLDocument = S4_object(), 
    HTMLInternalDocument = S4_object(), 
    XMLCodeDoc = S4_object()), .Names = c("XMLInternalDocument", 
"XMLHashTree", "XMLDocument", "HTMLInternalDocument", "XMLCodeDoc"
))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


NOBASEFIX <- 262144


getHTMLExternalFiles <- function (doc, xpQuery = c("//img/@src", "//link/@href", "//script/@href", 
    "//embed/@src"), baseURL = docName(doc), relative = FALSE, 
    asNodes = FALSE, recursive = FALSE) 
{
    if (is.character(doc)) 
        doc = htmlParse(doc)
    if (asNodes) 
        xpQuery = gsub("/@[a-zA-Z-]$+", "", xpQuery)
    nodes = getNodeSet(doc, xpQuery)
    if (asNodes) 
        return(nodes)
    nodes = as.character(nodes)
    ans = if (relative) 
        getRelativeURL(nodes, baseURL)
    else nodes
    ans
}


xmlOutputBuffer <- function (dtd = NULL, nameSpace = NULL, buf = NULL, nsURI = NULL, 
    header = "<?xml version=\"1.0\"?>") 
{
    if (is.null(buf)) 
        buf <- header
    else if (inherits(buf, "connection")) {
        if (!isOpen(buf)) {
            open(buf, rw = "w")
            on.exit(close(buf))
        }
        cat(header, "\n", sep = "", file = buf)
    }
    else if (!is.null(header)) 
        cat(header, "\n", sep = "", file = buf)
    emittedDocType <- FALSE
    if (missing(nameSpace) && !is.null(nsURI) && !is.null(names(nsURI))) {
        nameSpace <- names(nsURI)[1]
    }
    openTags <- NULL
    lastTag <- 0
    addOpenTag <- function(tag, ns, xmlns) {
        lastTag <<- lastTag + 1
        if (lastTag == 1) {
            rval <- matrix(c(tag, if (is.null(ns)) "" else ns, 
                if (is.null(xmlns)) "" else xmlns), nrow = 1, 
                dimnames = list(NULL, c("tagname", "nsprefix", 
                  "nsURI")))
        }
        else rval <- rbind(openTags, c(tag, ifelse(is.null(ns), 
            openTags[lastTag - 1, 2], ns), ifelse(is.null(xmlns), 
            "", xmlns)))
        openTags <<- rval
    }
    checkNamespace <- function(ns) {
        return(TRUE)
        if ((lastTag == 0)) 
            stop(paste("Namespace `", ns, "' is not defined\n", 
                sep = ""))
        m <- match(ns, openTags$nsprefix, NULL)
        if (any(!is.null(openTags[m, "nsURI"]))) 
            return(FALSE)
        stop(paste("Namespace:", ns, "is not defined\n", sep = " "))
    }
    openTag <- function(tag, ..., attrs = NULL, sep = "\n", namespace = NULL, 
        xmlns = NULL) {
        addTag(tag, ..., attrs = attrs, sep = sep, namespace = namespace, 
            xmlns, close = FALSE)
    }
    addTag <- function(tag, ..., attrs = NULL, sep = "\n", close = TRUE, 
        namespace = NULL, xmlns = NULL) {
        tmp <- ""
        startingTag <- is.null(getOpenTag())
        if (is.null(namespace)) {
            if (!is.null(xmlns)) {
                if (is.null(names(xmlns))) 
                  stop("you must specify the namespace as well as xmlns")
                namespace <- names(xmlns)[1]
            }
            else {
                cur <- getOpenTag()
                if (is.null(cur)) {
                  namespace <- nameSpace
                }
                else {
                  startingTag <- FALSE
                  namespace <- cur[["nsprefix"]]
                }
            }
        }
        if (!startingTag && !is.null(namespace) && namespace == 
            nameSpace && is.null(xmlns)) {
            tmp1 <- getOpenTag()
            if (is.null(tmp1) && !is.null(nsURI)) {
                xmlns <- nsURI[1]
            }
        }
        if (!is.null(namespace) && is.null(xmlns)) 
            checkNamespace(namespace)
        if (!is.null(namespace) && !is.null(xmlns)) {
            if (!is.null(names(xmlns))) {
                tmpp <- xmlns
                names(tmpp) <- paste("xmlns", names(tmpp), sep = ":")
                attrs <- c(attrs, tmpp)
            }
            else attrs[[paste("xmlns", namespace, sep = ":")]] <- xmlns
        }
        if (startingTag && !is.null(nsURI)) {
            tmpp <- nsURI
            names(tmpp) <- paste("xmlns", names(nsURI), sep = ":")
            attrs <- c(attrs, tmpp)
        }
        tagName <- if (!is.null(namespace) && namespace != "") 
            paste(namespace, tag, sep = ":")
        else tag
        if (!is.null(attrs)) {
            tmp <- paste(" ", paste(names(attrs), paste("\"", 
                attrs, "\"", sep = ""), sep = "=", collapse = " "), 
                sep = "")
        }
        if (length(dtd) && !emittedDocType) {
            add(paste("<!DOCTYPE", tag, "SYSTEM", ddQuote(dtd[1]), 
                if (length(dtd) > 1) 
                  paste("PUBLIC", ddQuote(dtd[2])), ">"))
            emittedDocType <<- TRUE
        }
        add(paste("<", tagName, tmp, ">", sep = ""))
        if (length(list(...)) > 0) {
            add(..., sep = sep)
        }
        if (close) 
            add(paste(if (sep == "\n") 
                ""
            else "\n", "</", tagName, ">", "\n", sep = ""), sep = "")
        else addOpenTag(tag, namespace, xmlns)
        NULL
    }
    closeTag <- function(name = NULL, namespace = nameSpace) {
        if (is.null(name)) {
            tmp <- getOpenTag()
            name <- tmp[1]
            if (length(tmp) > 1) 
                namespace <- tmp[2]
            openTags <<- openTags[-lastTag, , drop = FALSE]
            lastTag <<- lastTag - 1
        }
        else if (is.numeric(name)) {
            for (i in 1:name) closeTag()
            return()
        }
        add("</", ifelse(!is.null(namespace) && namespace != 
            "", paste(namespace, name, sep = ":"), name), ">\n", 
            sep = "")
    }
    getOpenTag <- function() {
        if (lastTag > 0) 
            openTags[lastTag, ]
        else NULL
    }
    paste0 <- function(..., sep = "", collapse = "") paste(..., 
        sep = sep, collapse = collapse)
    reset <- function() {
        buf <<- header
        openTags <<- list()
        lastTag <<- 0
    }
    addComment <- function(..., sep = "\n") {
        add("<!--", ..., "-->", sep = sep)
    }
    add <- function(..., sep = "\n") {
        if (is.character(buf)) 
            buf <<- paste(buf, paste0(..., collapse = sep), sep = sep)
        else cat(paste0(..., collapse = sep), sep, sep = "", 
            file = buf)
    }
    addCData <- function(text) {
        add("<![CDATA[", text, "]]>", sep = "\n")
    }
    addPI <- function(name, text) {
        add("<?", name, " ", text, "?>\n", sep = "")
    }
    tagString <- function(tag, ..., attrs, close = FALSE) {
        tmp <- ""
        if (!missing(attrs)) {
            tmp <- paste(" ", paste(names(attrs), paste("\"", 
                attrs, "\"", sep = ""), sep = "=", collapse = " "), 
                sep = "")
        }
        return(paste0("<", tag, tmp, ">", ..., "</", tag, ">"))
    }
    con <- list(value = function() {
        buf
    }, addTag = addTag, openTag = openTag, closeTag = closeTag, 
        addEndTag = closeTag, reset = reset, tagString = tagString, 
        add = add, addComment = addComment, addPI = addPI, addCData = addCData, 
        getOpenTag = getOpenTag, addOpenTag = addOpenTag)
    ans = new("XMLOutputBuffer", con)
    names(ans) = names(con)
    ans
}


schemaValidationErrorHandler <- function () 
{
    errors = character()
    warnings = character()
    h = function(msg) {
        if (inherits(msg, "XMLSchemaWarning")) 
            warnings <<- c(warnings, msg)
        else errors <<- c(errors, msg)
    }
    structure(list(handler = h, results = function() list(errors = errors, 
        warnings = warnings)), class = "XMLSchemaValidateHandler")
}


xmlNode <- function (name, ..., attrs = NULL, namespace = "", namespaceDefinitions = NULL, 
    .children = list(...)) 
{
    kids <- lapply(.children, asXMLNode)
    kids = addNames(kids)
    node <- list(name = name, attributes = attrs, children = kids, 
        namespace = namespace, namespaceDefinitions = as(namespaceDefinitions, 
            "XMLNamespaceDefinitions"))
    class(node) <- oldClass("XMLNode")
    node
}


xmlApply <- function (X, FUN, ...) 
{
    UseMethod("xmlApply")
}


xpathApply <- function (doc, path, fun = NULL, ..., namespaces = xmlNamespaceDefinitions(doc, 
    simplify = TRUE), resolveNamespaces = TRUE, addFinalizer = NA) 
{
    UseMethod("xpathApply")
}


`xmlAttrs<-` <- function (node, append = TRUE, suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", 
    FALSE), value) 
standardGeneric("xmlAttrs<-")


.__C__xmlSchemaTypeRef <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = structure(list(ExternalReference = S4_object()), .Names = "ExternalReference")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("xmlSchemaTypeRef", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlElementSummary <- function (url, handlers = xmlElementSummaryHandlers(url)) 
{
    handlers
    if (file.exists(url) && file.info(url)[1, "isdir"]) 
        url = list.files(url, pattern = "\\.xml$", full.names = TRUE)
    if (length(url) > 1) 
        lapply(url, xmlElementSummary, handlers)
    else xmlEventParse(url, handlers, replaceEntities = FALSE)
    handlers$result()
}


endElement.SAX <- function (name, .state = NULL) 
{
    standardGeneric("endElement.SAX")
}


readHTMLTable <- function (doc, header = NA, colClasses = NULL, skip.rows = integer(), 
    trim = TRUE, elFun = xmlValue, as.data.frame = TRUE, which = integer(), 
    ...) 
standardGeneric("readHTMLTable")


dtdElement <- function (name, dtd) 
{
    dtd$elements[[name]]
}


`.__T__removeXMLNamespaces:XML` <- "<environment>"

coerce <- methods::coerce # re-exported from methods package

NOCDATA <- 16384


.__C__xmlSchemaNotationRef <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = structure(list(ExternalReference = S4_object()), .Names = "ExternalReference")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("xmlSchemaNotationRef", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlValue <- function (x, ignoreComments = FALSE, recursive = TRUE, encoding = getEncoding(x), 
    trim = FALSE) 
{
    UseMethod("xmlValue")
}


xmlSearchNs <- function (node, ns, asPrefix = TRUE, doc = as(node, "XMLInternalDocument")) 
{
    .Call("R_xmlSearchNs", doc, node, as.character(ns), as.logical(asPrefix), 
        PACKAGE = "XML")
}


catalogLoad <- function (fileNames) 
{
    .Call("RS_XML_loadCatalog", path.expand(fileNames), PACKAGE = "XML")
}


xmlCDataNode <- function (...) 
{
    txt <- paste(..., collapse = "")
    node <- xmlNode("text")
    node$value <- txt
    class(node) <- oldClass("XMLCDataNode")
    node
}


asXMLTreeNode <- function (node, env, id = get(".nodeIdGenerator", env)(xmlName(node)), 
    className = "XMLTreeNode") 
{
    node$id = id
    node$env = env
    class(node) = c(className, class(node))
    node
}


`.__T__readKeyValueDB:XML` <- "<environment>"

.__C__SchemaElementTable <- new("classRepresentation"
    , slots = structure(list(ref = structure("ExternalReference", package = "XML")), .Names = "ref")
    , contains = structure(list(libxmlTypeTable = S4_object()), .Names = "libxmlTypeTable")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("SchemaElementTable", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__xmlParent:XML` <- "<environment>"

append.xmlNode <- function (to, ...) 
{
    UseMethod("append")
}


`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

.__C__xmlSchemaElementRef <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = structure(list(ExternalReference = S4_object()), .Names = "ExternalReference")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("xmlSchemaElementRef", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__docName<-:XML` <- "<environment>"

newHTMLDoc <- function (dtd = "loose", addFinalizer = TRUE, name = character(), 
    node = newXMLNode("html", newXMLNode("head", addFinalizer = FALSE), 
        newXMLNode("body", addFinalizer = FALSE), addFinalizer = FALSE)) 
{
    if (is.na(dtd) || dtd == "") 
        dtd = ""
    else if (tolower(dtd) %in% c("html5", "5")) 
        dtd = "5"
    else {
        i = grep(dtd, HTML_DTDs)
        if (length(i)) {
            if (length(i) > 1) 
                warning("matched multiple DTDs. Using the first")
            dtd = HTML_DTDs[i[1]]
        }
        else dtd = ""
    }
    doc = newXMLDoc(dtd = dtd, isHTML = TRUE, addFinalizer = addFinalizer, 
        node = node)
    doc
}


parseURI <- function (uri) 
{
    if (is.na(uri)) 
        return(structure(as.character(uri), class = "URI"))
    u = .Call("R_parseURI", as.character(uri), PACKAGE = "XML")
    if (u$port == 0) 
        u$port = as.integer(NA)
    class(u) = "URI"
    u
}


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

.__C__SAXState <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(), .Names = character(0))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("SAXState", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`xmlChildren<-` <- function (x, ..., value) 
{
    standardGeneric("xmlChildren<-")
}


.__C__SchemaNotationTable <- new("classRepresentation"
    , slots = structure(list(ref = structure("ExternalReference", package = "XML")), .Names = "ref")
    , contains = structure(list(libxmlTypeTable = S4_object()), .Names = "libxmlTypeTable")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("SchemaNotationTable", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlSchemaParse <- function (file, asText = FALSE, xinclude = TRUE, error = xmlErrorCumulator()) 
{
    xmlParse(file, asText = asText, isSchema = TRUE, xinclude = xinclude, 
        error = error)
}


`.__T__addAttributes:XML` <- "<environment>"

getNodePosition <- function (x) 
{
    if (is.list(x)) 
        return(sapply(x, getNodePosition))
    tmp = getNodeLocation(x)
    sprintf("%s:%d", tmp$file[1], tmp$line)
}


SAX1 <- 512


xmlRoot <- function (x, skip = TRUE, ...) 
{
    UseMethod("xmlRoot")
}


.__C__XMLNamespaceDeclNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLNamespaceDeclNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


removeChildren <- function (node, ..., kids = list(...), free = FALSE) 
{
    UseMethod("removeChildren")
}


dtdIsAttribute <- function (name, element, dtd) 
{
    if (!inherits(element, "XMLElementDef")) {
        element <- dtdElement(as.character(element), dtd)
    }
    return(!is.na(match(name, names(element$attributes))))
}


`.__T__text.SAX:XML` <- "<environment>"

newXMLTextNode <- function (text, parent = NULL, doc = NULL, cdata = FALSE, escapeEntities = is(text, 
    "AsIs"), addFinalizer = NA) 
{
    if (cdata) 
        return(newXMLCDataNode(text, parent, doc, addFinalizer = addFinalizer))
    a = .Call("R_newXMLTextNode", as.character(text), doc, addFinalizer, 
        PACKAGE = "XML")
    if (escapeEntities) 
        setNoEnc(a)
    if (!is.null(parent)) 
        addChildren(parent, a)
    a
}


newXMLCDataNode <- function (text, parent = NULL, doc = NULL, at = NA, sep = "\n", 
    addFinalizer = NA) 
{
    text = paste(as.character(text), collapse = "\n")
    a = .Call("R_newXMLCDataNode", doc, text, addFinalizer, PACKAGE = "XML")
    if (!is.null(parent)) 
        addChildren(parent, a, at = at)
    a
}


OLDSAX <- 1048576


DTDATTR <- 8


catalogResolve <- function (id, type = "uri", asIs = FALSE, debug = FALSE) 
{
    xmlInitializeCatalog()
    type = rep(type, length = length(id))
    types = c("uri", "public", "system")
    i = pmatch(tolower(type), types, duplicates.ok = TRUE)
    if (any(is.na(i))) 
        stop("don't recognize type. Must be one of ", paste(types, 
            collapse = ", "))
    ans = .Call("R_xmlCatalogResolve", as.character(id), i, as.logical(debug), 
        PACKAGE = "XML")
    if (asIs) 
        ans[is.na(ans)] = id[is.na(ans)]
    ans
}


.__C__XMLDTDNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLDTDNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


dtdElementValidEntry <- function (element, name, pos = NULL) 
{
    UseMethod("dtdElementValidEntry", element)
}


`.__T__xmlAttrs<-:XML` <- "<environment>"

htmlParse <- function (file, ignoreBlanks = TRUE, handlers = NULL, replaceEntities = FALSE, 
    asText = FALSE, trim = TRUE, validate = FALSE, getDTD = TRUE, 
    isURL = FALSE, asTree = FALSE, addAttributeNamespaces = FALSE, 
    useInternalNodes = TRUE, isSchema = FALSE, fullNamespaceInfo = FALSE, 
    encoding = character(), useDotNames = length(grep("^\\.", 
        names(handlers))) > 0, xinclude = TRUE, addFinalizer = TRUE, 
    error = htmlErrorHandler, isHTML = TRUE, options = integer(), 
    parentFirst = FALSE) 
{
    isMissingAsText = missing(asText)
    if (length(file) > 1) {
        file = paste(file, collapse = "\n")
        if (!missing(asText) && !asText) 
            stop(structure(list(message = "multiple URLs passed to xmlTreeParse. If this is the content of the file, specify asText = TRUE"), 
                class = c("MultipleURLError", "XMLParserError", 
                  "simpleError", "error", "condition")))
        asText = TRUE
    }
    if (missing(isURL) && !asText) 
        isURL <- length(grep("^(http|ftp|file)://", file, useBytes = TRUE, 
            perl = TRUE))
    if (isHTML) {
        validate = FALSE
        getDTD = FALSE
        isSchema = FALSE
        docClass = "HTMLInternalDocument"
    }
    else docClass = character()
    checkHandlerNames(handlers, "DOM")
    if (missing(fullNamespaceInfo) && inherits(handlers, "RequiresNamespaceInfo")) 
        fullNamespaceInfo = TRUE
    oldValidate = xmlValidity()
    xmlValidity(validate)
    on.exit(xmlValidity(oldValidate))
    if (!asText && isURL == FALSE) {
        if (file.exists(file) == FALSE) 
            if (!missing(asText) && asText == FALSE) {
                e = simpleError(paste("File", file, "does not exist"))
                class(e) = c("FileNotFound", class(e))
                stop(e)
            }
            else asText <- TRUE
    }
    if (asText && length(file) > 1) 
        file = paste(file, collapse = "\n")
    old = setEntitySubstitution(replaceEntities)
    on.exit(setEntitySubstitution(old), add = TRUE)
    if (asText && length(grep(sprintf("^%s?\\s*<", BOMRegExp), 
        file, perl = TRUE, useBytes = TRUE)) == 0) {
        if (!isHTML || (isMissingAsText && !inherits(file, "AsIs"))) {
            e = simpleError(paste("XML content does not seem to be XML:", 
                sQuote(file)))
            class(e) = c("XMLInputError", class(e))
            (if (isHTML) 
                warning
            else stop)(e)
        }
    }
    if (!is.logical(xinclude)) {
        xinclude = as.logical(xinclude)
    }
    if (!asText && !isURL) 
        file = path.expand(as.character(file))
    if (useInternalNodes && trim) {
        prevBlanks = .Call("RS_XML_setKeepBlanksDefault", 0L, 
            PACKAGE = "XML")
        on.exit(.Call("RS_XML_setKeepBlanksDefault", prevBlanks, 
            PACKAGE = "XML"), add = TRUE)
    }
    .oldErrorHandler = setXMLErrorHandler(error)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    if (length(options)) 
        options = sum(options)
    ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
        as.logical(ignoreBlanks), as.logical(replaceEntities), 
        as.logical(asText), as.logical(trim), as.logical(validate), 
        as.logical(getDTD), as.logical(isURL), as.logical(addAttributeNamespaces), 
        as.logical(useInternalNodes), as.logical(isHTML), as.logical(isSchema), 
        as.logical(fullNamespaceInfo), as.character(encoding), 
        as.logical(useDotNames), xinclude, error, addFinalizer, 
        as.integer(options), as.logical(parentFirst), PACKAGE = "XML")
    if (!missing(handlers) && length(handlers) && !as.logical(asTree)) 
        return(handlers)
    if (!isSchema && length(class(ans))) 
        class(ans) = c(docClass, oldClass(class(ans)))
    if (inherits(ans, "XMLInternalDocument")) 
        addDocFinalizer(ans, addFinalizer)
    else if (!getDTD && !isSchema) {
        class(ans) = oldClass("XMLDocumentContent")
    }
    ans
}


xmlStopParser <- function (parser) 
{
    if (!inherits(parser, "XMLParserContext")) 
        stop("Need an XMLParserContext object for xmlStopParser")
    .Call("RS_XML_xmlStopParser", parser, PACKAGE = "XML")
}


`.__T__free:XML` <- "<environment>"

DTDVALID <- 16


.__C__XMLInternalCDataNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLInternalCDataNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlErrorCumulator <- function (class = "XMLParserErrorList", immediate = TRUE) 
{
    messages = character()
    function(msg, ...) {
        if (length(grep("\\\n$", msg)) == 0) 
            paste(msg, "\n", sep = "")
        if (immediate) 
            cat(msg)
        if (length(msg) == 0) {
            e = simpleError(paste(1:length(messages), messages, 
                sep = ": ", collapse = ""))
            class(e) = c(class, class(e))
            stop(e)
        }
        messages <<- c(messages, msg)
    }
}


xmlAttrs <- function (node, ...) 
{
    UseMethod("xmlAttrs", node)
}


NOERROR <- 32


compareXMLDocs <- function (a, b, ...) 
{
    sa = summary(a, ...)
    sb = summary(b, ...)
    inAOnly = setdiff(names(sa$nameCounts), names(sb$nameCounts))
    inBOnly = setdiff(names(sb$nameCounts), names(sa$nameCounts))
    common.ids = intersect(names(sa$nameCounts), names(sb$nameCounts))
    diffs = sa$nameCounts[common.ids] - sb$nameCounts[common.ids]
    diffs = diffs[diffs != 0]
    list(inA = sa$nameCounts[inAOnly], inB = sb$nameCounts[inBOnly], 
        countDiffs = diffs)
}


makeClassTemplate <- function (xnode, types = character(), default = "ANY", className = xmlName(xnode), 
    where = globalenv()) 
{
    user.types = types
    slots = names(xnode)
    types = xmlSApply(xnode, function(x) {
        if (xmlSize(x) == 0) 
            default
        else if (xmlSize(x) == 1 || is(x, "XMLInternalTextNode")) 
            "character"
        else xmlName(x)
    })
    names(types) = slots
    types[names(xmlAttrs(xnode))] = "character"
    if (length(user.types)) 
        types[names(user.types)] = user.types
    coerce = sprintf("setAs('XMLAbstractNode', '%s', function(from) xmlToS4(from))", 
        className)
    def = if (length(types)) 
        sprintf("setClass('%s',\n    representation(%s))", className, 
            paste(sQuote(names(types)), sQuote(types), sep = " = ", 
                collapse = ",\n\t"))
    else sprintf("setClass('%s')", className)
    if (!is.null(where) && !(is.logical(where) && !where)) {
        eval(parse(text = def), envir = where)
        eval(parse(text = coerce), envir = where)
    }
    list(name = className, slots = types, def = def, coerce = coerce)
}


.__C__XMLInternalTextNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLInternalTextNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlAttributeType <- function (def, defaultType = FALSE) 
{
    if (defaultType == FALSE & names(def$type)[1] == "Enumeration") {
        return(paste("(", paste(def$defaultValue, collapse = " | "), 
            ")", sep = " ", collapse = ""))
    }
    switch(ifelse(defaultType, names(def$defaultType)[1], names(def$type)[1]), 
        Fixed = "#FIXED", CDATA = "CDATA", Implied = "#IMPLIED", 
        Required = "#REQUIRED", Id = "#ID", IDRef = "#IDREF", 
        IDRefs = "#IDREFS", Entity = "#ENTITY", Entities = "ENTITIES", 
        NMToken = "#NMTOKEN", NMTokens = "#NMTOKENS", Enumeration = "", 
        Notation = "", "<BROKEN>")
}


`xmlValue<-` <- function (x, ..., value) 
standardGeneric("xmlValue<-")


.__C__FormattedNumber <- new("classRepresentation"
    , slots = structure(list(.Data = structure("numeric", package = "methods")), .Names = ".Data")
    , contains = structure(list(numeric = S4_object(), 
    vector = S4_object()), .Names = c("numeric", 
"vector"))
    , virtual = FALSE
    , prototype = new("numeric"
)
    , validity = NULL
    , access = list()
    , className = structure("FormattedNumber", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


DTDLOAD <- 4


RECOVER <- 1


.__C__XMLInternalCommentNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLInternalCommentNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


matchNamespaces <- function (doc, namespaces, nsDefs = xmlNamespaceDefinitions(doc, 
    recursive = TRUE, simplify = FALSE), defaultNs = getDefaultNamespace(doc, 
    simplify = TRUE)) 
{
    if (is.character(namespaces) && length(namespaces) == 1 && 
        is.null(names(namespaces)) && length(defaultNs) > 0) {
        tmp = defaultNs
        names(tmp)[names(tmp) == ""] = namespaces
        tmp = as(tmp[[1]], "character")
        if (length(names(tmp)) == 0 || names(tmp) == "") 
            names(tmp) = namespaces
        return(tmp)
    }
    if (is.null(names(namespaces))) 
        names(namespaces) = rep("", length(namespaces))
    i = (names(namespaces) == "")
    if (any(i)) {
        if (i[1] && length(defaultNs) && is.na(match(namespaces[1], 
            names(nsDefs)))) {
            names(namespaces)[1] = namespaces[1]
            namespaces[1] = defaultNs
            msg = paste("using", names(namespaces)[1], "as prefix for default namespace", 
                defaultNs)
            e = simpleWarning(msg)
            class(e) = c("XPathDefaultNamespace", class(e))
            warning(e)
            i[1] = FALSE
        }
        if (sum(i) > 0) {
            dups = names(nsDefs)[duplicated(names(nsDefs))]
            tmp = match(namespaces[i], dups)
            if (length(dups) > 0 && any(is.na(tmp))) 
                stop("duplicate namespaces, so cannot match namespace prefix(es) ", 
                  paste(namespaces[i][is.na(tmp)], collapse = ", "), 
                  " in ", paste(unique(names(nsDefs)), collapse = ", "))
            idx = match(namespaces[i], names(nsDefs))
            if (any(is.na(idx))) 
                stop("cannot find defined namespace(s) with prefix(es) ", 
                  paste(namespaces[i][is.na(idx)], collapse = ", "))
            names(namespaces)[i] = namespaces[i]
            namespaces[i] = sapply(nsDefs[idx], function(x) x$uri)
        }
        else if (length(defaultNs) == 0) 
            stop("There is no default namespace on the target XML document")
    }
    if (!is.character(namespaces) || (length(namespaces) > 1 && 
        length(names(namespaces)) == 0)) 
        stop("Namespaces must be a named character vector")
    if (length(namespaces) && (length(names(namespaces)) == 0 || 
        any(names(namespaces) == ""))) 
        warning("namespaces without a name/prefix are not handled as you might expect in XPath. Use a prefix")
    namespaces
}


getRelativeURL <- function (u, baseURL, sep = "/", addBase = TRUE, simplify = TRUE) 
{
    if (length(u) > 1) 
        return(sapply(u, getRelativeURL, baseURL, sep))
    pu = parseURI(u)
    if (pu$scheme == "" && addBase) {
        b = parseURI(baseURL)
        b$query = ""
        if (grepl("^/", pu$path)) {
            b$path = u
            return(as(b, "character"))
        }
        b$path = sprintf("%s%s%s", dirname(b$path), sep, u)
        if (simplify && grepl("..", b$path, fixed = TRUE)) 
            b$path = simplifyPath(b$path)
        return(as(b, "character"))
    }
    else u
}


`.__T__xmlValue<-:XML` <- "<environment>"

`.__T__readHTMLList:XML` <- "<environment>"

xmlContainsElement <- function (name, dtd) 
{
    return(!is.na(match(name, dtd$element)))
}


NOENT <- 2


append.XMLNode <- function (to, ...) 
{
    args <- list(...)
    if (!inherits(args[[1]], "XMLNode") && is.list(args[[1]])) 
        args <- args[[1]]
    idx <- seq(length(to$children) + 1, length = length(args))
    args = addNames(args)
    if (is.null(to$children)) 
        to$children <- args
    else {
        to$children[idx] <- args
        names(to$children)[idx] <- names(args)
    }
    to
}


`.__T__readSolrDoc:XML` <- "<environment>"

startElement.SAX <- function (name, atts, .state = NULL) 
standardGeneric("startElement.SAX")


.__C__libxmlTypeTable <- new("classRepresentation"
    , slots = structure(list(ref = structure("ExternalReference", package = "XML")), .Names = "ref")
    , contains = list()
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("libxmlTypeTable", package = "XML")
    , package = "XML"
    , subclasses = structure(list(SchemaElementTable = S4_object(), 
    SchemaTypeTable = S4_object(), 
    SchemaAttributeTable = S4_object(), 
    SchemaAttributeGroupTable = S4_object(), 
    SchemaNotationTable = S4_object()), .Names = c("SchemaElementTable", 
"SchemaTypeTable", "SchemaAttributeTable", "SchemaAttributeGroupTable", 
"SchemaNotationTable"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


NSCLEAN <- 8192


xmlParserContextFunction <- function (f, class = "XMLParserContextFunction") 
{
    class(f) = c(class, class(f))
    f
}


getXMLErrors <- function (filename, parse = xmlParse, ...) 
{
    f = xmlErrorFun()
    opts = options()
    options(error = NULL)
    on.exit(options(opts))
    tryCatch(parse(filename, ..., error = f$handler), error = function(e) {
    })
    f$errors()
}


HUGE <- 524288


`xmlName<-` <- function (x, value) 
{
    UseMethod("xmlName<-")
}


xmlInternalTreeParse <- function (file, ignoreBlanks = TRUE, handlers = NULL, replaceEntities = FALSE, 
    asText = FALSE, trim = TRUE, validate = FALSE, getDTD = TRUE, 
    isURL = FALSE, asTree = FALSE, addAttributeNamespaces = FALSE, 
    useInternalNodes = TRUE, isSchema = FALSE, fullNamespaceInfo = FALSE, 
    encoding = character(), useDotNames = length(grep("^\\.", 
        names(handlers))) > 0, xinclude = TRUE, addFinalizer = TRUE, 
    error = xmlErrorCumulator(), isHTML = FALSE, options = integer(), 
    parentFirst = FALSE) 
{
    isMissingAsText = missing(asText)
    if (length(file) > 1) {
        file = paste(file, collapse = "\n")
        if (!missing(asText) && !asText) 
            stop(structure(list(message = "multiple URLs passed to xmlTreeParse. If this is the content of the file, specify asText = TRUE"), 
                class = c("MultipleURLError", "XMLParserError", 
                  "simpleError", "error", "condition")))
        asText = TRUE
    }
    if (missing(isURL) && !asText) 
        isURL <- length(grep("^(http|ftp|file)://", file, useBytes = TRUE, 
            perl = TRUE))
    if (isHTML) {
        validate = FALSE
        getDTD = FALSE
        isSchema = FALSE
        docClass = "HTMLInternalDocument"
    }
    else docClass = character()
    checkHandlerNames(handlers, "DOM")
    if (missing(fullNamespaceInfo) && inherits(handlers, "RequiresNamespaceInfo")) 
        fullNamespaceInfo = TRUE
    oldValidate = xmlValidity()
    xmlValidity(validate)
    on.exit(xmlValidity(oldValidate))
    if (!asText && isURL == FALSE) {
        if (file.exists(file) == FALSE) 
            if (!missing(asText) && asText == FALSE) {
                e = simpleError(paste("File", file, "does not exist"))
                class(e) = c("FileNotFound", class(e))
                stop(e)
            }
            else asText <- TRUE
    }
    if (asText && length(file) > 1) 
        file = paste(file, collapse = "\n")
    old = setEntitySubstitution(replaceEntities)
    on.exit(setEntitySubstitution(old), add = TRUE)
    if (asText && length(grep(sprintf("^%s?\\s*<", BOMRegExp), 
        file, perl = TRUE, useBytes = TRUE)) == 0) {
        if (!isHTML || (isMissingAsText && !inherits(file, "AsIs"))) {
            e = simpleError(paste("XML content does not seem to be XML:", 
                sQuote(file)))
            class(e) = c("XMLInputError", class(e))
            (if (isHTML) 
                warning
            else stop)(e)
        }
    }
    if (!is.logical(xinclude)) {
        xinclude = as.logical(xinclude)
    }
    if (!asText && !isURL) 
        file = path.expand(as.character(file))
    if (useInternalNodes && trim) {
        prevBlanks = .Call("RS_XML_setKeepBlanksDefault", 0L, 
            PACKAGE = "XML")
        on.exit(.Call("RS_XML_setKeepBlanksDefault", prevBlanks, 
            PACKAGE = "XML"), add = TRUE)
    }
    .oldErrorHandler = setXMLErrorHandler(error)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    if (length(options)) 
        options = sum(options)
    ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
        as.logical(ignoreBlanks), as.logical(replaceEntities), 
        as.logical(asText), as.logical(trim), as.logical(validate), 
        as.logical(getDTD), as.logical(isURL), as.logical(addAttributeNamespaces), 
        as.logical(useInternalNodes), as.logical(isHTML), as.logical(isSchema), 
        as.logical(fullNamespaceInfo), as.character(encoding), 
        as.logical(useDotNames), xinclude, error, addFinalizer, 
        as.integer(options), as.logical(parentFirst), PACKAGE = "XML")
    if (!missing(handlers) && length(handlers) && !as.logical(asTree)) 
        return(handlers)
    if (!isSchema && length(class(ans))) 
        class(ans) = c(docClass, oldClass(class(ans)))
    if (inherits(ans, "XMLInternalDocument")) 
        addDocFinalizer(ans, addFinalizer)
    else if (!getDTD && !isSchema) {
        class(ans) = oldClass("XMLDocumentContent")
    }
    ans
}


xmlEventHandler <- function () 
{
    con <- xmlOutputDOM()
    startElement <- function(name, atts, ...) {
        con$addTag(name, attrs = atts, close = FALSE)
    }
    endElement <- function(name) {
        con$closeTag(name)
    }
    text <- function(x, ...) {
        con$addNode(xmlTextNode(x))
    }
    comment <- function(x, ...) {
        xmlCommentNode(x)
    }
    externalEntity <- function(ctxt, baseURI, sysId, publicId, 
        ...) {
        cat("externalEntity", ctxt, baseURI, sysId, publicId, 
            "\n")
    }
    entityDeclaration <- function(name, baseURI, sysId, publicId, 
        notation, ...) {
        cat("externalEntity", name, baseURI, sysId, publicId, 
            notation, "\n")
    }
    processingInstruction <- function(sys, value) {
        con$addNode(xmlPINode(sys, value))
    }
    list(startElement = startElement, endElement = endElement, 
        processingInstruction = processingInstruction, text = text, 
        comment = comment, externalEntity = externalEntity, entityDeclaration = entityDeclaration, 
        dom = function() {
            con
        })
}


xmlSize.default <- function (obj) 
{
    length(obj)
}


`.__T__xmlToDataFrame:XML` <- "<environment>"

removeAttributes <- function (node, ..., .attrs = NULL, .namespace = FALSE, .all = (length(list(...)) + 
    length(.attrs)) == 0) 
standardGeneric("removeAttributes")


replaceNodes <- function (oldNode, newNode, ...) 
{
    UseMethod("replaceNodes")
}


getChildrenStrings <- function (node, encoding = getEncoding(node), asVector = TRUE, 
    len = xmlSize(node), addNames = TRUE) 
{
    encoding = getEncodingREnum(encoding)
    .Call("R_childStringValues", node, as.integer(len), as.logical(asVector), 
        as.integer(encoding), as.logical(addNames), PACKAGE = "XML")
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

getDefaultNamespace <- function (doc, ns = xmlNamespaceDefinitions(doc, simplify = simplify), 
    simplify = FALSE) 
{
    if (length(ns) == 0) 
        return(character())
    i = which(names(ns) == "")
    if (length(i)) 
        ns[i]
    else character()
}


`xmlNamespace<-` <- function (x, ..., value) 
standardGeneric("xmlNamespace<-")


saveXML <- function (doc, file = NULL, compression = 0, indent = TRUE, prefix = "<?xml version=\"1.0\"?>\n", 
    doctype = NULL, encoding = getEncoding(doc), ...) 
standardGeneric("saveXML")


xmlParseString <- function (content, doc = NULL, namespaces = RXMLNamespaces, clean = TRUE, 
    addFinalizer = NA) 
{
    f = function(cdata = FALSE) newXMLNode("para", newXMLTextNode(content, 
        cdata = cdata, doc = doc), doc = doc)
    if (inherits(content, "AsIs")) 
        return(f(TRUE))
    if (!isXMLString(content)) 
        return(f(TRUE))
    content = as(content, "XMLString")
    ns = paste(paste("xmlns", names(RXMLNamespaces), sep = ":"), 
        sprintf("\"%s\"", RXMLNamespaces), sep = "=", collapse = " ")
    txt = paste("<para ", ns, ">", content, "</para>", sep = "")
    local.doc = tryCatch(xmlParse(txt, addFinalizer = addFinalizer), 
        error = function(e) e)
    if (inherits(local.doc, "condition")) 
        return(f(TRUE))
    tmp = xmlRoot(local.doc)
    if (xmlSize(tmp) == 1) 
        tmp = tmp[[1]]
    if (clean) 
        removeXMLNamespaces(tmp, .els = names(namespaces))
    if (inherits(doc, "XMLInternalDocument")) {
        manageMemory = manageMemory_p(addFinalizer)
        .Call("RS_XML_copyNodesToDoc", tmp, doc, addFinalizer, 
            PACKAGE = "XML")
    }
    else tmp
}


xmlSchemaValidate <- function (schema, doc, errorHandler = xmlErrorFun(), options = 0L) 
{
    if (is.character(doc)) 
        doc = xmlParse(doc)
    if (is.character(schema)) 
        schema = xmlSchemaParse(schema)
    .oldErrorHandler = setXMLErrorHandler(if (is.list(errorHandler)) 
        errorHandler[[1]]
    else errorHandler)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    status = .Call("RS_XML_xmlSchemaValidateDoc", schema@ref, 
        doc, as.integer(options), NULL, PACKAGE = "XML")
    if (inherits(errorHandler, "XMLStructuredErrorCumulator")) 
        structure(list(status = status, errors = errorHandler[[2]]()), 
            class = "XMLSchemaValidationResults")
    else if (inherits(errorHandler, "XMLSchemaValidateHandler")) 
        c(status = status, errorHandler$results())
    else status
}


`.__T__toHTML:XML` <- "<environment>"

NONET <- 2048


`.__T__xmlToS4:XML` <- "<environment>"

xmlParent <- function (x, ...) 
standardGeneric("xmlParent")


xmlGetAttr <- function (node, name, default = NULL, converter = NULL, namespaceDefinition = character(), 
    addNamespace = length(grep(":", name)) > 0) 
{
    a <- xmlAttrs(node, addNamespace)
    if (is.null(a) || is.na(match(name, names(a)))) 
        return(default)
    if (length(namespaceDefinition)) 
        verifyNamespace(name, namespaceDefinition, node)
    if (!is.null(converter)) 
        converter(a[[name]])
    else a[[name]]
}


ensureNamespace <- function (doc, what) 
{
    if (is(doc, "XMLInternalDocument")) 
        node = xmlRoot(doc)
    else node = doc
    defs = xmlNamespaceDefinitions(xmlRoot(doc), simplify = TRUE)
    i = match(what, defs)
    w = is.na(i)
    if (any(w)) {
        sapply(names(what)[w], function(id) newXMLNamespace(node, 
            what[id], id))
        names(what)[w]
    }
    else names(defs)[i]
}


.__C__xmlSchemaRef <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = structure(list(ExternalReference = S4_object()), .Names = "ExternalReference")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("xmlSchemaRef", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlAncestors <- function (x, fun = NULL, ..., addFinalizer = NA, count = -1L) 
{
    ans = list()
    tmp = x
    while (!is.null(tmp)) {
        if (!is.null(fun)) 
            ans = c(fun(tmp, ...), ans)
        else ans = c(tmp, ans)
        if (count > 0 && length(ans) == count) 
            break
        tmp = xmlParent(tmp, addFinalizer = addFinalizer)
    }
    ans
}


xmlSize <- function (obj) 
{
    UseMethod("xmlSize", obj)
}


xmlTextNode <- function (value, namespace = "", entities = XMLEntities, cdata = FALSE) 
{
    node <- xmlNode("text", namespace = namespace)
    if (length(entities) && !inherits(value, "AsIs")) 
        value = insertEntities(value, XMLEntities)
    if (cdata) 
        value = xmlCDataNode(value)
    node$value <- value
    if (!cdata) 
        class(node) <- oldClass("XMLTextNode")
    if (length(entities)) 
        class(node) <- c(class(node), "EntitiesEscaped")
    node
}


.__C__XMLNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(RXMLAbstractNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("RXMLAbstractNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLNode", package = "XML")
    , package = "XML"
    , subclasses = structure(list(XMLTextNode = S4_object(), 
    XMLPINode = S4_object(), 
    XMLCommentNode = S4_object(), 
    XMLProcessingInstruction = S4_object(), 
    XMLCDataNode = S4_object(), 
    XMLTreeNode = S4_object()), .Names = c("XMLTextNode", 
"XMLPINode", "XMLCommentNode", "XMLProcessingInstruction", "XMLCDataNode", 
"XMLTreeNode"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


findXInclude <- function (x, asNode = FALSE, recursive = FALSE) 
{
    while (!is.null(x)) {
        tmp = getSiblingXIncludeStart(x, TRUE)
        if (!is.null(tmp)) 
            return(fixFindXInclude(tmp, asNode, recursive))
        sib = x
        if (is(sib, "XMLXIncludeStartNode")) 
            return(fixFindXInclude(sib, asNode, recursive))
        x = xmlParent(x)
    }
    fixFindXInclude(x, asNode, recursive)
}


comment.SAX <- function (content, .state = NULL) 
{
    standardGeneric("comment.SAX")
}


xmlChildren <- function (x, addNames = TRUE, ...) 
{
    UseMethod("xmlChildren")
}


toHTML <- tools::toHTML # re-exported from tools package

Doctype <- function (system = character(), public = character(), name = "") 
{
    if (length(public) == 1 && length(system) > 0) {
        public = c(public, system)
        system = character()
    }
    new("Doctype", name = name, system = system, public = public)
}


.__C__xmlSchemaAttributeGroupRef <- new("classRepresentation"
    , slots = structure(list(ref = structure("externalptr", package = "methods")), .Names = "ref")
    , contains = structure(list(ExternalReference = S4_object()), .Names = "ExternalReference")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("xmlSchemaAttributeGroupRef", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlToS4 <- function (node, obj = new(xmlName(node)), ...) 
standardGeneric("xmlToS4")


xmlCleanNamespaces <- function (doc, options = integer(), out = docName(doc), ...) 
{
    if (is(doc, "XMLInternalDocument")) 
        doc = saveXML(doc)
    options = unique(c(options, NSCLEAN))
    newDoc = xmlParse(doc, ..., options = options)
    if (is.logical(out)) 
        out = if (out) 
            docName(doc)
        else character()
    if (is.character(out) && length(out)) 
        saveXML(newDoc, out)
    else newDoc
}


`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

`.__T__startElement.SAX:XML` <- "<environment>"

xmlDeserializeHook <- function (x) 
{
    if (length(x) == 2) {
        if (x[2] == "XMLInternalElementNode") 
            xmlRoot(xmlParse(I(x[1])))
        else if (x[2] == "XMLNodeSet") {
            structure(lapply(x, function(x) xmlRoot(xmlParse(I(x)))), 
                "XMLNodeSet")
        }
        else if (x[2] == "XMLInternalDocument") 
            xmlParse(I(x[1]))
        else stop("Not sure how to handle ", x[2])
    }
    else xmlParse(I(x))
}


setXMLNamespace <- function (node, namespace, append = FALSE) 
{
    if (is.character(namespace) && is.null(names(namespace))) 
        namespace = findNamespaceDefinition(node, namespace)
    else if (is.character(namespace)) 
        namespace = newNamespace(node, namespace)
    else if (!is.null(namespace) && !inherits(namespace, c("XMLNamespaceRef", 
        "XMLNamespaceDeclaration"))) 
        stop("Must provide a namespace definition, a prefix of existing namespace or a reference to a namespace definition")
    .Call("R_xmlSetNs", node, namespace, FALSE, PACKAGE = "XML")
}


text.SAX <- function (content, .state = NULL) 
{
    standardGeneric("text.SAX")
}


xmlContainsEntity <- function (name, dtd) 
{
    return(!is.na(match(name, dtd$entities)))
}


.__C__XMLEntityDeclNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLEntityDeclNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


catalogClearTable <- function () 
{
    .Call("RS_XML_clearCatalog", PACKAGE = "XML")
}


xmlName <- function (node, full = FALSE) 
{
    UseMethod("xmlName", node)
}


xmlToDataFrame <- function (doc, colClasses = NULL, homogeneous = NA, collectNames = TRUE, 
    nodes = list(), stringsAsFactors = default.stringsAsFactors()) 
standardGeneric("xmlToDataFrame")


xmlTree <- function (tag = NULL, attrs = NULL, dtd = NULL, namespaces = list(), 
    doc = newXMLDoc(dtd, namespaces)) 
{
    currentNodes <- list(doc)
    isXML2 <- libxmlVersion()$major != "1"
    if (!is.null(dtd)) {
        if (isXML2) {
            node = NULL
            if (inherits(dtd, "XMLDTDNode")) 
                node = dtd
            else if (is.character(dtd) && dtd[1] != "") 
                node = newXMLDTDNode(dtd, doc = doc)
            if (!is.null(node)) {
                addChildren(doc, node)
                currentNodes[[2]] <- node
            }
        }
        else warning("DTDs not supported in R for libxml 1.*. Use libxml2 instead.")
    }
    definedNamespaces = list()
    defaultNamespace = NULL
    addNamespaceDefinitions = is.null(tag)
    setActiveNamespace = function(ns) {
        defaultNamespace <<- ns
    }
    asXMLNode <- function(x) {
        if (inherits(x, "XMLInternalNode")) 
            return(x)
        v = if (is.list(x)) 
            lapply(x, asXMLNode)
        else newXMLTextNode(as.character(x), doc = doc, escapeEntities = is(x, 
            "AsIs"))
        v
    }
    setNamespace <- function(node, namespace = defaultNamespace) {
        if (length(namespace) == 0 || !(length(namespace) == 
            1 && is.null(names(namespace)))) 
            return(NULL)
        if (is.list(namespace)) 
            return(NULL)
        if (!is.na(match(namespace, names(namespaces))) && is.na(match(namespace, 
            names(definedNamespaces)))) {
            ns <- .Call("R_xmlNewNs", node, namespaces[[namespace]], 
                namespace, PACKAGE = "XML")
            definedNamespaces[[namespace]] <<- ns
        }
        setXMLNamespace(node, definedNamespaces[[namespace]])
    }
    addTag <- function(name, ..., attrs = NULL, close = TRUE, 
        namespace = defaultNamespace, .children = list(...)) {
        if (inherits(name, "XMLInternalNode")) {
            addChildren(currentNodes[[1]], name)
            currentNodes <<- c(node, currentNodes)
            addChildren(node, kids = .children)
            if (close) 
                currentNodes <<- currentNodes[-1]
            return(name)
        }
        if (FALSE) {
            if (length(namespace) == 1 && length(names(namespace)) == 
                0) {
                tmp = namespace
                if (length(currentNodes)) {
                  defs = namespaceDeclarations(currentNodes[[1]], 
                    TRUE)
                  i = match(namespace, names(defs))
                  if (!is.na(i)) 
                    namespace = defs[[i]]
                }
            }
        }
        if (!is.null(attrs)) 
            storage.mode(attrs) <- "character"
        if (inherits(name, "XMLInternalNode")) 
            node = name
        else {
            parent = if (length(currentNodes) > 1) 
                currentNodes[[1]]
            else xmlRoot(currentNodes[[1]])
            node <- newXMLNode(name, attrs = attrs, namespace = namespace, 
                doc = doc, parent = parent, namespaceDefinitions = if (addNamespaceDefinitions) 
                  namespaces
                else NULL)
            if (addNamespaceDefinitions) {
                addNamespaceDefinitions <<- FALSE
            }
        }
        currentNodes <<- c(node, currentNodes)
        for (i in .children) addChildren(node, asXMLNode(i))
        if (close == TRUE) 
            closeTag()
        invisible(node)
    }
    closeTag <- function(name = "") {
        if (nargs() == 0) {
            tmp <- currentNodes[[1]]
            currentNodes <<- currentNodes[-1]
        }
        else if (is.character(name)) {
            w = sapply(currentNodes, inherits, "XMLInternalElementNode")
            useNamespace = length(grep(":", name)) > 0
            ids = sapply(currentNodes[w], xmlName, useNamespace)
            tmp = list()
            for (id in name) {
                i = which(id == ids)
                if (length(i) == 0) 
                  stop("Cannot close tag for node with name ", 
                    id, " - no such node open")
                tmp = c(tmp, currentNodes[1:i])
                currentNodes <<- currentNodes[-c(1:i)]
                ids = ids[-(1:i)]
            }
        }
        else if (inherits(name, "numeric")) {
            num = name
            if (is.na(num) || num == -1) 
                w = seq(along = currentNodes[-length(currentNodes)])
            else if (length(num) == 1) 
                w = 1:num
            else w = num
            tmp = currentNodes[w]
            currentNodes <<- currentNodes[-w]
        }
        invisible(tmp)
    }
    add = function(node, parent = currentNodes[[1]], close = TRUE) {
        if (!is.null(parent)) {
            addChildren(parent, node)
            if (!close) 
                currentNodes <<- c(node, currentNodes)
        }
        invisible(node)
    }
    addComment <- function(...) {
        add(newXMLCommentNode(paste(as.character(list(...)), 
            sep = ""), doc = doc))
    }
    addCData <- function(text) {
        add(newXMLCDataNode(text, doc = doc))
    }
    addPI <- function(name, text) {
        add(newXMLPINode(name, text, doc = doc), NULL)
    }
    if (!is.null(tag)) {
        if (is.character(tag)) {
            node = addTag(tag, attrs = attrs, namespace = namespaces, 
                close = FALSE)
        }
        else if (inherits(tag, "XMLInternalNode")) {
            if (is.null(xmlParent(node))) 
                addChildren(doc, node)
        }
    }
    v <- list(addTag = addTag, addNode = addTag, addCData = addCData, 
        addPI = addPI, closeTag = closeTag, closeNode = closeTag, 
        addComment = addComment, setNamespace = setActiveNamespace, 
        value = function() doc, doc = function() doc, add = function(...) {
        })
    ans = new("XMLInternalDOM", v)
    names(ans) = names(v)
    ans
}


.__C__XMLTreeNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLNode = S4_object(), 
    RXMLAbstractNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLNode", 
"RXMLAbstractNode", "XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLTreeNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


newXMLPINode <- function (name, text, parent = NULL, doc = NULL, at = NA, addFinalizer = NA) 
{
    a = .Call("R_newXMLPINode", doc, as.character(name), as.character(text), 
        addFinalizer, PACKAGE = "XML")
    if (!is.null(parent)) 
        addChildren(parent, a, at = at)
    a
}


NOWARNING <- 64


`.__T__removeAttributes:XML` <- "<environment>"

`.__T__saveXML:XML` <- "<environment>"

xmlHashTree <- function (nodes = list(), parents = character(), children = list(), 
    env = new.env(TRUE, parent = emptyenv())) 
{
    .count = 0
    env$.children = .children = new.env(TRUE)
    env$.parents = .parents = new.env(TRUE)
    f = function(suggestion = "") {
        if (suggestion == "" || exists(suggestion, env, inherits = FALSE)) 
            as.character(.count + 1)
        else suggestion
    }
    assign(".nodeIdGenerator", f, env)
    addNode = function(node, parent = character(), ..., attrs = NULL, 
        namespace = NULL, namespaceDefinitions = character(), 
        .children = list(...), cdata = FALSE, suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", 
            FALSE)) {
        if (is.character(node)) 
            node = xmlNode(node, attrs = attrs, namespace = namespace, 
                namespaceDefinitions = namespaceDefinitions)
        .kids = .children
        .children = .this$.children
        node = asXMLTreeNode(node, .this, className = "XMLHashTreeNode")
        id = node$id
        assign(id, node, env)
        .count <<- .count + 1
        if (!inherits(parent, "XMLNode") && (!is.environment(parent) && 
            length(parent) == 0) || parent == "") 
            return(node)
        if (inherits(parent, "XMLHashTreeNode")) 
            parent = parent$id
        if (length(parent)) {
            assign(id, parent, envir = .parents)
            if (exists(parent, .children, inherits = FALSE)) 
                tmp = c(get(parent, .children), id)
            else tmp = id
            assign(parent, tmp, .children)
        }
        return(node)
    }
    env$.addNode <- addNode
    .tidy = function() {
        idx <- idx - 1
        length(nodeSet) <- idx
        length(nodeNames) <- idx
        names(nodeSet) <- nodeNames
        .nodes <<- nodeSet
        idx
    }
    .this = structure(env, class = oldClass("XMLHashTree"))
    .this
}


xmlTreeParse <- function (file, ignoreBlanks = TRUE, handlers = NULL, replaceEntities = FALSE, 
    asText = FALSE, trim = TRUE, validate = FALSE, getDTD = TRUE, 
    isURL = FALSE, asTree = FALSE, addAttributeNamespaces = FALSE, 
    useInternalNodes = FALSE, isSchema = FALSE, fullNamespaceInfo = FALSE, 
    encoding = character(), useDotNames = length(grep("^\\.", 
        names(handlers))) > 0, xinclude = TRUE, addFinalizer = TRUE, 
    error = xmlErrorCumulator(), isHTML = FALSE, options = integer(), 
    parentFirst = FALSE) 
{
    isMissingAsText = missing(asText)
    if (length(file) > 1) {
        file = paste(file, collapse = "\n")
        if (!missing(asText) && !asText) 
            stop(structure(list(message = "multiple URLs passed to xmlTreeParse. If this is the content of the file, specify asText = TRUE"), 
                class = c("MultipleURLError", "XMLParserError", 
                  "simpleError", "error", "condition")))
        asText = TRUE
    }
    if (missing(isURL) && !asText) 
        isURL <- length(grep("^(http|ftp|file)://", file, useBytes = TRUE, 
            perl = TRUE))
    if (isHTML) {
        validate = FALSE
        getDTD = FALSE
        isSchema = FALSE
        docClass = "HTMLInternalDocument"
    }
    else docClass = character()
    checkHandlerNames(handlers, "DOM")
    if (missing(fullNamespaceInfo) && inherits(handlers, "RequiresNamespaceInfo")) 
        fullNamespaceInfo = TRUE
    oldValidate = xmlValidity()
    xmlValidity(validate)
    on.exit(xmlValidity(oldValidate))
    if (!asText && isURL == FALSE) {
        if (file.exists(file) == FALSE) 
            if (!missing(asText) && asText == FALSE) {
                e = simpleError(paste("File", file, "does not exist"))
                class(e) = c("FileNotFound", class(e))
                stop(e)
            }
            else asText <- TRUE
    }
    if (asText && length(file) > 1) 
        file = paste(file, collapse = "\n")
    old = setEntitySubstitution(replaceEntities)
    on.exit(setEntitySubstitution(old), add = TRUE)
    if (asText && length(grep(sprintf("^%s?\\s*<", BOMRegExp), 
        file, perl = TRUE, useBytes = TRUE)) == 0) {
        if (!isHTML || (isMissingAsText && !inherits(file, "AsIs"))) {
            e = simpleError(paste("XML content does not seem to be XML:", 
                sQuote(file)))
            class(e) = c("XMLInputError", class(e))
            (if (isHTML) 
                warning
            else stop)(e)
        }
    }
    if (!is.logical(xinclude)) {
        xinclude = as.logical(xinclude)
    }
    if (!asText && !isURL) 
        file = path.expand(as.character(file))
    if (useInternalNodes && trim) {
        prevBlanks = .Call("RS_XML_setKeepBlanksDefault", 0L, 
            PACKAGE = "XML")
        on.exit(.Call("RS_XML_setKeepBlanksDefault", prevBlanks, 
            PACKAGE = "XML"), add = TRUE)
    }
    .oldErrorHandler = setXMLErrorHandler(error)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    if (length(options)) 
        options = sum(options)
    ans <- .Call("RS_XML_ParseTree", as.character(file), handlers, 
        as.logical(ignoreBlanks), as.logical(replaceEntities), 
        as.logical(asText), as.logical(trim), as.logical(validate), 
        as.logical(getDTD), as.logical(isURL), as.logical(addAttributeNamespaces), 
        as.logical(useInternalNodes), as.logical(isHTML), as.logical(isSchema), 
        as.logical(fullNamespaceInfo), as.character(encoding), 
        as.logical(useDotNames), xinclude, error, addFinalizer, 
        as.integer(options), as.logical(parentFirst), PACKAGE = "XML")
    if (!missing(handlers) && length(handlers) && !as.logical(asTree)) 
        return(handlers)
    if (!isSchema && length(class(ans))) 
        class(ans) = c(docClass, oldClass(class(ans)))
    if (inherits(ans, "XMLInternalDocument")) 
        addDocFinalizer(ans, addFinalizer)
    else if (!getDTD && !isSchema) {
        class(ans) = oldClass("XMLDocumentContent")
    }
    ans
}


`xmlParent<-` <- function (x, ..., value) 
{
    addChildren(value, ..., kids = list(x))
}


`.__T__readHTMLTable:XML` <- "<environment>"

.__C__XMLCodeDoc <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalDocument = S4_object(), 
    XMLAbstractDocument = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalDocument", 
"XMLAbstractDocument", "oldClass"))
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("XMLCodeDoc", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


addAttributes <- function (node, ..., .attrs = NULL, suppressNamespaceWarning = getOption("suppressXMLNamespaceWarning", 
    FALSE), append = TRUE) 
standardGeneric("addAttributes")


libxmlVersion <- function (runTime = FALSE) 
{
    v <- .Call(if (runTime) "RS_XML_libxmlVersionRuntime" else "RS_XML_libxmlVersion", 
        PACKAGE = "XML")
    v <- as.character(v)
    els <- substring(v, 1:nchar(v), 1:nchar(v))
    list(major = els[1], minor = paste(els[2:3], sep = "", collapse = ""), 
        patch = paste(els[4:5], sep = "", collapse = ""))
}


xmlElementsByTagName <- function (el, name, recursive = FALSE) 
{
    kids = xmlChildren(el)
    idx = (names(kids) == name)
    els = kids[idx]
    if (!recursive || xmlSize(el) == 0) 
        return(els)
    subs = xmlApply(el, xmlElementsByTagName, name, TRUE)
    subs = unlist(subs, recursive = FALSE)
    append(els, subs[!sapply(subs, is.null)])
}


`.__T__docName:XML` <- "<environment>"

getEncoding <- function (obj, ...) 
{
    standardGeneric("getEncoding")
}


.__C__HTMLInternalDocument <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalDocument = S4_object(), 
    XMLAbstractDocument = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalDocument", 
"XMLAbstractDocument", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("HTMLInternalDocument", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


addNode <- function (node, parent, to, ...) 
{
    UseMethod("addNode", to)
}


xml <- function (x) 
{
    new("XMLString", x)
}


parseXMLAndAdd <- function (txt, parent = NULL, top = "tmp", nsDefs = character()) 
{
    txt = paste(txt, collapse = "")
    if (!inherits(txt, "AsIs") && length(top) > 0) {
        open = sprintf("%s%s", top, paste(sprintf(" xmlns%s%s=\"%s\"", 
            ifelse(names(nsDefs) != "", ":", ""), names(nsDefs), 
            nsDefs), collapse = ""))
        tmp = sprintf("<%s>%s</%s>", open, txt, top)
    }
    else tmp = txt
    doc = xmlParse(tmp, asText = TRUE)
    if (!is.null(parent)) 
        invisible(.Call("R_insertXMLNode", xmlChildren(xmlRoot(doc)), 
            parent, -1L, FALSE, PACKAGE = "XML"))
    else xmlRoot(doc)
}


.__C__SchemaAttributeGroupTable <- new("classRepresentation"
    , slots = structure(list(ref = structure("ExternalReference", package = "XML")), .Names = "ref")
    , contains = structure(list(libxmlTypeTable = S4_object()), .Names = "libxmlTypeTable")
    , virtual = FALSE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("SchemaAttributeGroupTable", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


xmlDoc <- function (node, addFinalizer = TRUE) 
{
    if (!is(node, "XMLInternalElementNode")) 
        stop("xmlDoc must be passed an internal XML node")
    doc = .Call("RS_XML_createDocFromNode", node, PACKAGE = "XML")
    addDocFinalizer(doc, addFinalizer)
    doc
}


names.XMLNode <- function (x) 
{
    xmlSApply(x, xmlName)
}


xmlEventParse <- function (file, handlers = xmlEventHandler(), ignoreBlanks = FALSE, 
    addContext = TRUE, useTagName = TRUE, asText = FALSE, trim = TRUE, 
    useExpat = FALSE, isURL = FALSE, state = NULL, replaceEntities = TRUE, 
    validate = FALSE, saxVersion = 1, branches = NULL, useDotNames = length(grep("^\\.", 
        names(handlers))) > 0, error = xmlErrorCumulator(), addFinalizer = NA) 
{
    if (libxmlVersion()$major < 2 && !is.character(file)) 
        stop("Without libxml2, the source of the XML can only be specified as a URI.")
    i = grep("^/", names(handlers))
    if (length(i)) {
        endElementHandlers = handlers[i]
        names(endElementHandlers) = gsub("^/", "", names(endElementHandlers))
        handlers = handlers[-i]
    }
    else endElementHandlers = list()
    checkHandlerNames(handlers, "SAX")
    if (validate) 
        warning("Currently, libxml2 does support validation using SAX/event-driven parsing. It requires a DOM.")
    else {
        oldValidate = xmlValidity()
        xmlValidity(validate)
        on.exit(xmlValidity(oldValidate))
    }
    if (!any(saxVersion == c(1, 2))) {
        stop("saxVersion must be 1 or 2")
    }
    if (inherits(file, "connection")) {
        con = file
        if (!isOpen(file)) {
            open(file, "r")
            on.exit(close(con))
        }
        file = function(len) {
            txt = readLines(con, 1)
            if (length(txt) == 0) 
                return(txt)
            paste(txt, "\n", sep = "")
        }
    }
    else if (is.function(file)) {
        on.exit(file(-1))
    }
    else {
        if (!asText && missing(isURL)) {
            isURL <- length(grep("http://", file)) | length(grep("ftp://", 
                file)) | length(grep("file://", file))
        }
        if (isURL == FALSE && asText == FALSE) {
            file = path.expand(file)
            if (file.exists(file) == FALSE) 
                stop(paste("File", file, "does not exist "))
        }
        file = as.character(file)
    }
    branches = as.list(branches)
    if (length(branches) > 0 && (length(names(branches)) == 0 || 
        any(names(branches) == ""))) 
        stop("All branch elements must have a name!")
    old = setEntitySubstitution(replaceEntities)
    on.exit(setEntitySubstitution(old))
    if (!is.function(error)) 
        stop("error must be a function")
    .oldErrorHandler = setXMLErrorHandler(error)
    on.exit(.Call("RS_XML_setStructuredErrorHandler", .oldErrorHandler, 
        PACKAGE = "XML"), add = TRUE)
    state <- .Call("RS_XML_Parse", file, handlers, endElementHandlers, 
        as.logical(addContext), as.logical(ignoreBlanks), as.logical(useTagName), 
        as.logical(asText), as.logical(trim), as.logical(useExpat), 
        state, as.logical(replaceEntities), as.logical(validate), 
        as.integer(saxVersion), branches, as.logical(useDotNames), 
        error, addFinalizer, PACKAGE = "XML")
    if (!is.null(state)) 
        return(state)
    else return(invisible(handlers))
}


`.__T__entityDeclaration.SAX:XML` <- "<environment>"

dtdEntity <- function (name, dtd) 
{
    dtd$entities[[name]]
}


`.__T__show:methods` <- methods::`.__T__show:methods` # re-exported from methods package

xmlToList <- function (node, addAttributes = TRUE, simplify = FALSE) 
{
    if (is.character(node)) 
        node = xmlParse(node)
    if (inherits(node, "XMLAbstractDocument")) 
        node = xmlRoot(node)
    if (any(inherits(node, c("XMLTextNode", "XMLInternalTextNode")))) 
        xmlValue(node)
    else if (xmlSize(node) == 0) 
        xmlAttrs(node)
    else {
        if (is.list(node)) {
            tmp = vals = xmlSApply(node, xmlToList, addAttributes)
            tt = xmlSApply(node, inherits, c("XMLTextNode", "XMLInternalTextNode"))
        }
        else {
            tmp = vals = (if (simplify) 
                xmlSApply
            else xmlApply)(node, xmlToList, addAttributes)
            tt = xmlSApply(node, inherits, c("XMLTextNode", "XMLInternalTextNode"))
        }
        vals[tt] = (if (simplify) 
            sapply
        else lapply)(vals[tt], function(x) x[[1]])
        if (length(attrs <- xmlAttrs(node)) > 0) {
            if (addAttributes) 
                vals[[".attrs"]] = attrs
            else attributes(vals) = as.list(attrs)
        }
        if (any(tt) && length(vals) == 1) 
            vals[[1]]
        else vals
    }
}


xmlSourceFunctions <- function (doc, ids = character(), parse = TRUE, ...) 
{
    standardGeneric("xmlSourceFunctions")
}


XINCLUDE <- 1024


xmlHandler <- function () 
{
    data <- list()
    startElement <- function(name, atts, ...) {
        if (is.null(atts)) 
            atts <- list()
        data[[name]] <<- atts
    }
    text <- function(x, ...) {
        cat("MyText:", x, "\n")
    }
    comment <- function(x, ...) {
        cat("comment", x, "\n")
    }
    externalEntity <- function(ctxt, baseURI, sysId, publicId, 
        ...) {
        cat("externalEntity", ctxt, baseURI, sysId, publicId, 
            "\n")
    }
    entityDeclaration <- function(name, baseURI, sysId, publicId, 
        notation, ...) {
        cat("externalEntity", name, baseURI, sysId, publicId, 
            notation, "\n")
    }
    foo <- function(x, attrs, ...) {
        cat("In foo\n")
    }
    return(list(startElement = startElement, getData = function() {
        data
    }, comment = comment, externalEntity = externalEntity, entityDeclaration = entityDeclaration, 
        text = text, foo = foo))
}


COMPACT <- 65536


.__C__XMLDocumentTypeNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLDocumentTypeNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


NOXINCNODE <- 32768


getNodeLocation <- function (node, recursive = TRUE, fileOnly = FALSE) 
{
    if (is.list(node)) 
        return(lapply(node, getNodeLocation, recursive, fileOnly))
    fil = findXInclude(node, recursive = recursive)
    if (is.null(fil)) 
        fil = docName(node)
    if (fileOnly) 
        fil[1]
    else list(file = fil, line = getLineNumber(node))
}


.__C__XMLDocumentNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLDocumentNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__comment.SAX:XML` <- "<environment>"

xmlXIncludes <- function (filename, recursive = TRUE, omitPattern = "\\.(js|html?|txt|R|c)$", 
    namespace = c(xi = "http://www.w3.org/2003/XInclude"), addNames = TRUE, 
    clean = NULL, ignoreTextParse = FALSE) 
{
    doc = xmlParse(filename, xinclude = FALSE)
    if (missing(namespace)) {
        ns = xmlNamespaceDefinitions(doc, simplify = TRUE)
        if ("http://www.w3.org/2001/XInclude" %in% ns) 
            namespace = c(xi = "http://www.w3.org/2001/XInclude")
    }
    nodes = getNodeSet(doc, "//xi:include[not(ancestor::ignore)]", 
        namespaces = namespace)
    files = lapply(nodes, xmlGetAttr, "href")
    nonRecursive = as.logical(sapply(nodes, xmlGetAttr, "parse", 
        "") == "text")
    d = duplicated(files)
    files = files[!d]
    nodes = nodes[!d]
    nonRecursive = nonRecursive[!d]
    if (ignoreTextParse) {
        files = files[!nonRecursive]
        nonRecursive = rep(FALSE, length(files))
    }
    files = doClean(files, clean)
    if (length(omitPattern)) 
        nonRecursive = grepl(omitPattern, unlist(files)) | nonRecursive
    if (recursive) {
        ans = files
        ans[!nonRecursive] = lapply(files[!nonRecursive], function(x) {
            u = getRelativeURL(x, filename)
            u = gsub("#.*$", "", u)
            xmlXIncludes(u, recursive = TRUE, addNames = addNames, 
                clean = clean, ignoreTextParse = ignoreTextParse)
        })
        if (addNames) 
            names(ans) = files
        if (length(ans) == 0 || ans == "") 
            ans = list()
        files = ans
        files = lapply(files, function(x) if (is.character(x)) 
            list(name = x)
        else x)
    }
    else files = unlist(files)
    list(name = doClean(filename, clean), children = files)
}


getNodeSet <- function (doc, path, namespaces = xmlNamespaceDefinitions(doc, 
    simplify = TRUE), fun = NULL, sessionEncoding = CE_NATIVE, 
    addFinalizer = NA, ...) 
{
    xpathApply(doc, path, fun, ..., namespaces = namespaces, 
        sessionEncoding = sessionEncoding, addFinalizer = addFinalizer)
}


readSolrDoc <- function (doc, ...) 
standardGeneric("readSolrDoc")


.__C__XMLAttributes <- new("classRepresentation"
    , slots = structure(list(.Data = structure("character", package = "methods")), .Names = ".Data")
    , contains = structure(list(character = S4_object(), 
    vector = S4_object(), 
    data.frameRowLabels = S4_object(), 
    SuperClassMethod = S4_object()), .Names = c("character", 
"vector", "data.frameRowLabels", "SuperClassMethod"))
    , virtual = FALSE
    , prototype = new("character"
)
    , validity = NULL
    , access = list()
    , className = structure("XMLAttributes", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


.__C__XMLAttributeDeclNode <- new("classRepresentation"
    , slots = structure(list(.S3Class = structure("character", package = "methods")), .Names = ".S3Class")
    , contains = structure(list(XMLInternalNode = S4_object(), 
    XMLAbstractNode = S4_object(), 
    oldClass = S4_object()), .Names = c("XMLInternalNode", 
"XMLAbstractNode", "oldClass"))
    , virtual = TRUE
    , prototype = S4_object()
    , validity = NULL
    , access = list()
    , className = structure("XMLAttributeDeclNode", package = "XML")
    , package = "XML"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


show <- methods::show # re-exported from methods package

xmlSourceSection <- function (doc, ids = character(), xnodes = c(".//r:function", 
    ".//r:init[not(@eval='false')]", ".//r:code[not(@eval='false')]", 
    ".//r:plot[not(@eval='false')]"), namespaces = DefaultXPathNamespaces, 
    ...) 
standardGeneric("xmlSourceSection")


genericSAXHandlers <- function (include, exclude, useDotNames = FALSE) 
{
    if (!exists("startElement.SAX")) 
        stop("You must call .InitSAXMethods before calling genericSAXHandlers()n")
    ans <- list(startElement = startElement.SAX, endElement = endElement.SAX, 
        comment = comment.SAX, text = text.SAX, processingInstruction = processingInstruction.SAX, 
        entityDeclaration = entityDeclaration.SAX)
    if (!missing(include)) 
        ans <- ans[include]
    else if (!missing(exclude)) {
        which <- match(exclude, names(ans))
        ans <- ans[-which]
    }
    if (useDotNames) 
        names(ans) = paste(".", names(ans), sep = "")
    ans
}


xmlParseDoc <- function (file, options = 1L, encoding = character(), asText = !file.exists(file), 
    baseURL = file) 
{
    if (is.character(options)) {
        i = pmatch(options, names(parserOptions))
        if (any(is.na(i))) 
            stop("unrecognized XML parser options: ", paste(options[is.na(i)], 
                collapse = ", "))
        options = parserOptions[i]
    }
    else {
        if (!all(options %in% parserOptions)) 
            stop("unrecognized XML parser options: ", paste(options[!(options %in% 
                parserOptions)], collapse = ", "))
    }
    options = as.integer(sum(options))
    if (asText) 
        .Call("R_xmlReadMemory", file, nchar(file), as.character(encoding), 
            options, as.character(baseURL), PACKAGE = "XML")
    else .Call("R_xmlReadFile", path.expand(file), as.character(encoding), 
        options, PACKAGE = "XML")
}




## Package Data

# none


## Package Info

.skeleton_package_title = "Tools for Parsing and Generating XML Within R and S-Plus"

.skeleton_package_version = "3.98-1.5"

.skeleton_package_depends = "methods,utils"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF