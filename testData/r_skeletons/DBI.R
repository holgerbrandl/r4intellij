##
## Exported symobls in package `DBI`
##

## Exported package methods

`.__T__dbWriteTable:DBI` <- "<environment>"

.__C__DBIResult <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(DBIObject = S4_object()), .Names = "DBIObject")
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("DBIResult", package = "DBI")
    , package = "DBI"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__dbGetQuery:DBI` <- "<environment>"

sqlParseVariables <- function (con, sql, ...) 
{
    standardGeneric("sqlParseVariables")
}


sqlRownamesToColumn <- function (df, row.names = NA) 
{
    name <- guessRowName(df, row.names)
    if (is.null(name)) {
        rownames(df) <- NULL
        return(df)
    }
    rn <- stats::setNames(list(row.names(df)), name)
    df <- c(rn, df)
    class(df) <- "data.frame"
    attr(df, "row.names") <- .set_row_names(length(rn[[1]]))
    df
}


dbReadTable <- function (conn, name, ...) 
{
    ans <- {
        standardGeneric("dbReadTable")
    }
    .valueClassTest(ans, "data.frame", "dbReadTable")
}


dbGetRowsAffected <- function (res, ...) 
{
    ans <- standardGeneric("dbGetRowsAffected")
    .valueClassTest(ans, "numeric", "dbGetRowsAffected")
}


make.db.names.default <- function (snames, keywords = .SQL92Keywords, unique = TRUE, allow.keywords = TRUE) 
{
    makeUnique <- function(x, sep = "_") {
        if (length(x) == 0) 
            return(x)
        out <- x
        lc <- make.names(tolower(x), unique = FALSE)
        i <- duplicated(lc)
        lc <- make.names(lc, unique = TRUE)
        out[i] <- paste(out[i], substring(lc[i], first = nchar(out[i]) + 
            1), sep = sep)
        out
    }
    fc <- substring(snames, 1, 1)
    lc <- substring(snames, nchar(snames))
    i <- match(fc, c("'", "\""), 0) > 0 & match(lc, c("'", "\""), 
        0) > 0
    snames[!i] <- make.names(snames[!i], unique = FALSE)
    if (unique) 
        snames[!i] <- makeUnique(snames[!i])
    if (!allow.keywords) {
        kwi <- match(keywords, toupper(snames), nomatch = 0L)
        snames[kwi] <- paste("\"", snames[kwi], "\"", sep = "")
    }
    gsub("\\.", "_", snames)
}


ANSI <- function () 
{
    new("AnsiConnection")
}


sqlParseVariablesImpl <- function (sql, quotes, comments) 
{
    sql_arr <- c(strsplit(as.character(sql), "", fixed = TRUE)[[1]], 
        " ")
    var_chars <- c(LETTERS, letters, 0:9, "_")
    var_pos_start <- integer()
    var_pos_end <- integer()
    quote_spec_offset <- 0L
    comment_spec_offset <- 0L
    sql_variable_start <- 0L
    sql_variable_end <- 0L
    for (c in seq_along(comments)) {
        comments[[c]][[1]] <- strsplit(comments[[c]][[1]], "", 
            fixed = TRUE)[[1]]
        comments[[c]][[2]] <- strsplit(comments[[c]][[2]], "", 
            fixed = TRUE)[[1]]
    }
    for (q in seq_along(quotes)) {
        quotes[[q]][[5]] <- nchar(quotes[[q]][[3]]) > 0L
    }
    state <- "default"
    i <- 0
    while (i < length(sql_arr)) {
        i <- i + 1
        switch(state, default = {
            if (sql_arr[[i]] == "?") {
                sql_variable_start <- i
                state <- "variable"
                next
            }
            for (q in seq_along(quotes)) {
                if (identical(sql_arr[[i]], quotes[[q]][[1]])) {
                  quote_spec_offset <- q
                  state <- "quote"
                  break
                }
            }
            if (state != "default") next
            for (c in seq_along(comments)) {
                comment_start_arr <- comments[[c]][[1]]
                comment_start_length <- length(comment_start_arr)
                if (identical(sql_arr[i:(i + comment_start_length - 
                  1)], comment_start_arr)) {
                  comment_spec_offset <- c
                  i <- i + comment_start_length
                  state <- "comment"
                  break
                }
            }
        }, variable = {
            if (!(sql_arr[[i]] %in% var_chars)) {
                if (i - sql_variable_start < 2) {
                  stop("Length 0 variable")
                }
                var_pos_start <- c(var_pos_start, sql_variable_start)
                var_pos_end <- c(var_pos_end, i - 1)
                i <- i - 1
                state <- "default"
            }
        }, quote = {
            if (quotes[[quote_spec_offset]][[5]] && identical(sql_arr[[i]], 
                quotes[[quote_spec_offset]][[3]])) {
                i <- i + 1
                next
            }
            if (identical(sql_arr[[i]], quotes[[quote_spec_offset]][[2]])) {
                quote_spec_offset <- 0L
                state <- "default"
            }
        }, comment = {
            comment_end_arr <- comments[[comment_spec_offset]][[2]]
            comment_end_length <- length(comment_end_arr)
            if (identical(sql_arr[i:(i + comment_end_length - 
                1)], comment_end_arr)) {
                i <- i + comment_end_length
                comment_spec_offset <- 0L
                state <- "default"
            }
        })
    }
    if (quote_spec_offset > 0L) {
        stop("Unterminated literal")
    }
    if (comment_spec_offset > 0L && comments[[comment_spec_offset]][[3]]) {
        stop("Unterminated comment")
    }
    list(start = as.integer(var_pos_start), end = as.integer(var_pos_end))
}


dbBreak <- function () 
{
    signalCondition(structure(list(message = "Aborting DBI processing", 
        call = NULL), class = c("dbi_abort", "condition")))
    stop("Invalid usage of dbBreak().", call. = FALSE)
}


dbCallProc <- function (conn, ...) 
{
    ans <- {
        .Deprecated()
        standardGeneric("dbCallProc")
    }
    .valueClassTest(ans, "logical", "dbCallProc")
}


dbGetDBIVersion <- function () 
{
    .Deprecated("packageVersion('DBI')")
    utils::packageVersion("DBI")
}


`.__T__sqlInterpolate:DBI` <- "<environment>"

.__C__SQL <- new("classRepresentation"
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
    , className = structure("SQL", package = "DBI")
    , package = "DBI"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


sqlInterpolate <- function (`_con`, `_sql`, ...) 
{
    standardGeneric("sqlInterpolate")
}


print.list.pairs <- function (x, ...) 
{
    .Deprecated()
    print_list_pairs(x, ...)
}


`.__T__dbWithTransaction:DBI` <- "<environment>"

dbBind <- function (res, params, ...) 
{
    standardGeneric("dbBind")
}


`.__T__dbSetDataMappings:DBI` <- "<environment>"

`.__T__sqlParseVariables:DBI` <- "<environment>"

show <- methods::show # re-exported from methods package

`.__T__SQLKeywords:DBI` <- "<environment>"

sqlAppendTable <- function (con, table, values, row.names = NA, ...) 
{
    standardGeneric("sqlAppendTable")
}


dbListConnections <- function (drv, ...) 
standardGeneric("dbListConnections")


dbClearResult <- function (res, ...) 
{
    ans <- standardGeneric("dbClearResult")
    .valueClassTest(ans, "logical", "dbClearResult")
}


sqlCommentSpec <- function (start, end, endRequired) 
{
    list(start, end, endRequired)
}


`.__T__sqlCreateTable:DBI` <- "<environment>"

.__C__DBIConnection <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(DBIObject = S4_object()), .Names = "DBIObject")
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("DBIConnection", package = "DBI")
    , package = "DBI"
    , subclasses = structure(list(AnsiConnection = S4_object()), .Names = "AnsiConnection")
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


make.db.names <- function (dbObj, snames, keywords = .SQL92Keywords, unique = TRUE, 
    allow.keywords = TRUE, ...) 
{
    ans <- {
        standardGeneric("make.db.names")
    }
    .valueClassTest(ans, "character", "make.db.names")
}


`.__T__dbGetRowCount:DBI` <- "<environment>"

`.__T__dbExistsTable:DBI` <- "<environment>"

dbSendStatement <- function (conn, statement, ...) 
{
    ans <- standardGeneric("dbSendStatement")
    .valueClassTest(ans, "DBIResult", "dbSendStatement")
}


dbRemoveTable <- function (conn, name, ...) 
{
    ans <- standardGeneric("dbRemoveTable")
    .valueClassTest(ans, "logical", "dbRemoveTable")
}


`.__T__dbColumnInfo:DBI` <- "<environment>"

`.__T__dbDataType:DBI` <- "<environment>"

`.__T__dbQuoteString:DBI` <- "<environment>"

SQL <- function (x) 
new("SQL", x)


dbRollback <- function (conn, ...) 
{
    ans <- standardGeneric("dbRollback")
    .valueClassTest(ans, "logical", "dbRollback")
}


`.__T__dbGetRowsAffected:DBI` <- "<environment>"

`.__T__fetch:DBI` <- "<environment>"

`.__T__dbQuoteIdentifier:DBI` <- "<environment>"

sqlCreateTable <- function (con, table, fields, row.names = NA, temporary = FALSE, 
    ...) 
{
    standardGeneric("sqlCreateTable")
}


`.__T__dbSendStatement:DBI` <- "<environment>"

dbExecute <- function (conn, statement, ...) 
standardGeneric("dbExecute")


dbColumnInfo <- function (res, ...) 
{
    ans <- standardGeneric("dbColumnInfo")
    .valueClassTest(ans, "data.frame", "dbColumnInfo")
}


sqlAppendTableTemplate <- function (con, table, values, row.names = NA, prefix = "?", ...) 
{
    table <- dbQuoteIdentifier(con, table)
    values <- sqlRownamesToColumn(values[0, , drop = FALSE], 
        row.names)
    fields <- dbQuoteIdentifier(con, names(values))
    SQL(paste0("INSERT INTO ", table, "\n", "  (", paste(fields, 
        collapse = ", "), ")\n", "VALUES\n", paste0("  (", paste0(prefix, 
        seq_along(fields), collapse = ", "), ")", collapse = ",\n")))
}


.__C__DBIDriver <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(DBIObject = S4_object()), .Names = "DBIObject")
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("DBIDriver", package = "DBI")
    , package = "DBI"
    , subclasses = list()
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


isSQLKeyword.default <- function (name, keywords = .SQL92Keywords, case = c("lower", 
    "upper", "any")[3]) 
{
    n <- pmatch(case, c("lower", "upper", "any"), nomatch = 0)
    if (n == 0) 
        stop("case must be one of \"lower\", \"upper\", or \"any\"")
    kw <- switch(c("lower", "upper", "any")[n], lower = tolower(keywords), 
        upper = toupper(keywords), any = toupper(keywords))
    if (n == 3) 
        name <- toupper(name)
    match(name, keywords, nomatch = 0) > 0
}


`.__T__dbRollback:DBI` <- "<environment>"

dbDisconnect <- function (conn, ...) 
{
    ans <- standardGeneric("dbDisconnect")
    .valueClassTest(ans, "logical", "dbDisconnect")
}


dbQuoteString <- function (conn, x, ...) 
{
    standardGeneric("dbQuoteString")
}


SQLKeywords <- function (dbObj, ...) 
{
    ans <- {
        standardGeneric("SQLKeywords")
    }
    .valueClassTest(ans, "character", "SQLKeywords")
}


`.__T__dbExecute:DBI` <- "<environment>"

.__C__DBIObject <- new("classRepresentation"
    , slots = list()
    , contains = structure(list(), .Names = character(0))
    , virtual = TRUE
    , prototype = new("S4"
)
    , validity = NULL
    , access = list()
    , className = structure("DBIObject", package = "DBI")
    , package = "DBI"
    , subclasses = structure(list(DBIDriver = S4_object(), 
    DBIConnection = S4_object(), 
    DBIResult = S4_object(), 
    AnsiConnection = S4_object()), .Names = c("DBIDriver", 
"DBIConnection", "DBIResult", "AnsiConnection"))
    , versionKey = <pointer: (nil)>
    , sealed = FALSE
)


`.__T__$:base` <- methods::`.__T__$:base` # re-exported from methods package

`.__T__dbBind:DBI` <- "<environment>"

dbGetException <- function (conn, ...) 
standardGeneric("dbGetException")


`.__T__dbListResults:DBI` <- "<environment>"

`.__T__show:methods` <- "<environment>"

`.__T__isSQLKeyword:DBI` <- "<environment>"

`.__T__make.db.names:DBI` <- "<environment>"

dbListResults <- function (conn, ...) 
standardGeneric("dbListResults")


dbFetch <- function (res, n = -1, ...) 
{
    ans <- standardGeneric("dbFetch")
    .valueClassTest(ans, "data.frame", "dbFetch")
}


`.__T__dbUnloadDriver:DBI` <- "<environment>"

`.__T__$<-:base` <- methods::`.__T__$<-:base` # re-exported from methods package

`.__T__dbDisconnect:DBI` <- "<environment>"

dbConnect <- function (drv, ...) 
{
    ans <- standardGeneric("dbConnect")
    .valueClassTest(ans, "DBIConnection", "dbConnect")
}


dbUnloadDriver <- function (drv, ...) 
{
    ans <- standardGeneric("dbUnloadDriver")
    .valueClassTest(ans, "logical", "dbUnloadDriver")
}


`.__T__[<-:base` <- methods::`.__T__[<-:base` # re-exported from methods package

`.__T__dbIsValid:DBI` <- "<environment>"

dbExistsTable <- function (conn, name, ...) 
{
    ans <- standardGeneric("dbExistsTable")
    .valueClassTest(ans, "logical", "dbExistsTable")
}


dbGetStatement <- function (res, ...) 
{
    ans <- standardGeneric("dbGetStatement")
    .valueClassTest(ans, "character", "dbGetStatement")
}


`.__T__dbSendQuery:DBI` <- "<environment>"

fetch <- function (res, n = -1, ...) 
{
    ans <- standardGeneric("fetch")
    .valueClassTest(ans, "data.frame", "fetch")
}


dbGetQuery <- function (conn, statement, ...) 
standardGeneric("dbGetQuery")


dbHasCompleted <- function (res, ...) 
{
    ans <- standardGeneric("dbHasCompleted")
    .valueClassTest(ans, "logical", "dbHasCompleted")
}


`.__T__dbDriver:DBI` <- "<environment>"

dbGetInfo <- function (dbObj, ...) 
standardGeneric("dbGetInfo")


isSQLKeyword <- function (dbObj, name, keywords = .SQL92Keywords, case = c("lower", 
    "upper", "any")[3], ...) 
{
    ans <- {
        standardGeneric("isSQLKeyword")
    }
    .valueClassTest(ans, "logical", "isSQLKeyword")
}


dbDriver <- function (drvName, ...) 
{
    ans <- standardGeneric("dbDriver")
    .valueClassTest(ans, "DBIDriver", "dbDriver")
}


`.__T__dbHasCompleted:DBI` <- "<environment>"

dbGetRowCount <- function (res, ...) 
{
    ans <- standardGeneric("dbGetRowCount")
    .valueClassTest(ans, "numeric", "dbGetRowCount")
}


`.__T__dbBegin:DBI` <- "<environment>"

`.__T__dbGetException:DBI` <- "<environment>"

dbBegin <- function (conn, ...) 
{
    ans <- standardGeneric("dbBegin")
    .valueClassTest(ans, "logical", "dbBegin")
}


dbWithTransaction <- function (conn, code) 
standardGeneric("dbWithTransaction")


`.__T__dbListFields:DBI` <- "<environment>"

dbWriteTable <- function (conn, name, value, ...) 
{
    ans <- {
        standardGeneric("dbWriteTable")
    }
    .valueClassTest(ans, "logical", "dbWriteTable")
}


`.__T__[[<-:base` <- methods::`.__T__[[<-:base` # re-exported from methods package

dbCommit <- function (conn, ...) 
{
    ans <- standardGeneric("dbCommit")
    .valueClassTest(ans, "logical", "dbCommit")
}


dbListFields <- function (conn, name, ...) 
{
    ans <- standardGeneric("dbListFields")
    .valueClassTest(ans, "character", "dbListFields")
}


`.__T__[:base` <- methods::`.__T__[:base` # re-exported from methods package

dbQuoteIdentifier <- function (conn, x, ...) 
{
    standardGeneric("dbQuoteIdentifier")
}


`.__T__sqlData:DBI` <- "<environment>"

`.__T__dbListTables:DBI` <- "<environment>"

`.__T__dbRemoveTable:DBI` <- "<environment>"

dbSetDataMappings <- function (res, flds, ...) 
{
    ans <- {
        .Deprecated()
        standardGeneric("dbSetDataMappings")
    }
    .valueClassTest(ans, "logical", "dbSetDataMappings")
}


`.__T__sqlAppendTable:DBI` <- "<environment>"

`.__T__dbClearResult:DBI` <- "<environment>"

dbDataType <- function (dbObj, obj, ...) 
{
    ans <- standardGeneric("dbDataType")
    .valueClassTest(ans, "character", "dbDataType")
}


`.__T__dbConnect:DBI` <- "<environment>"

dbListTables <- function (conn, ...) 
{
    ans <- standardGeneric("dbListTables")
    .valueClassTest(ans, "character", "dbListTables")
}


.SQL92Keywords <- c("ABSOLUTE", "ADD", "ALL", "ALLOCATE", "ALTER", "AND", "ANY", 
"ARE", "AS", "ASC", "ASSERTION", "AT", "AUTHORIZATION", "AVG", 
"BEGIN", "BETWEEN", "BIT", "BIT_LENGTH", "BY", "CASCADE", "CASCADED", 
"CASE", "CAST", "CATALOG", "CHAR", "CHARACTER", "CHARACTER_LENGTH", 
"CHAR_LENGTH", "CHECK", "CLOSE", "COALESCE", "COLLATE", "COLLATION", 
"COLUMN", "COMMIT", "CONNECT", "CONNECTION", "CONSTRAINT", "CONSTRAINTS", 
"CONTINUE", "CONVERT", "CORRESPONDING", "COUNT", "CREATE", "CURRENT", 
"CURRENT_DATE", "CURRENT_TIMESTAMP", "CURRENT_TYPE", "CURSOR", 
"DATE", "DAY", "DEALLOCATE", "DEC", "DECIMAL", "DECLARE", "DEFAULT", 
"DEFERRABLE", "DEFERRED", "DELETE", "DESC", "DESCRIBE", "DESCRIPTOR", 
"DIAGNOSTICS", "DICONNECT", "DICTIONATRY", "DISPLACEMENT", "DISTINCT", 
"DOMAIN", "DOUBLE", "DROP", "ELSE", "END", "END-EXEC", "ESCAPE", 
"EXCEPT", "EXCEPTION", "EXEC", "EXECUTE", "EXISTS", "EXTERNAL", 
"EXTRACT", "FALSE", "FETCH", "FIRST", "FLOAT", "FOR", "FOREIGN", 
"FOUND", "FROM", "FULL", "GET", "GLOBAL", "GO", "GOTO", "GRANT", 
"GROUP", "HAVING", "HOUR", "IDENTITY", "IGNORE", "IMMEDIATE", 
"IN", "INCLUDE", "INDEX", "INDICATOR", "INITIALLY", "INNER", 
"INPUT", "INSENSITIVE", "INSERT", "INT", "INTEGER", "INTERSECT", 
"INTERVAL", "INTO", "IS", "ISOLATION", "JOIN", "KEY", "LANGUAGE", 
"LAST", "LEFT", "LEVEL", "LIKE", "LOCAL", "LOWER", "MATCH", "MAX", 
"MIN", "MINUTE", "MODULE", "MONTH", "NAMES", "NATIONAL", "NCHAR", 
"NEXT", "NOT", "NULL", "NULLIF", "NUMERIC", "OCTECT_LENGTH", 
"OF", "OFF", "ONLY", "OPEN", "OPTION", "OR", "ORDER", "OUTER", 
"OUTPUT", "OVERLAPS", "PARTIAL", "POSITION", "PRECISION", "PREPARE", 
"PRESERVE", "PRIMARY", "PRIOR", "PRIVILEGES", "PROCEDURE", "PUBLIC", 
"READ", "REAL", "REFERENCES", "RESTRICT", "REVOKE", "RIGHT", 
"ROLLBACK", "ROWS", "SCHEMA", "SCROLL", "SECOND", "SECTION", 
"SELECT", "SET", "SIZE", "SMALLINT", "SOME", "SQL", "SQLCA", 
"SQLCODE", "SQLERROR", "SQLSTATE", "SQLWARNING", "SUBSTRING", 
"SUM", "SYSTEM", "TABLE", "TEMPORARY", "THEN", "TIME", "TIMESTAMP", 
"TIMEZONE_HOUR", "TIMEZONE_MINUTE", "TO", "TRANSACTION", "TRANSLATE", 
"TRANSLATION", "TRUE", "UNION", "UNIQUE", "UNKNOWN", "UPDATE", 
"UPPER", "USAGE", "USER", "USING", "VALUE", "VALUES", "VARCHAR", 
"VARYING", "VIEW", "WHEN", "WHENEVER", "WHERE", "WITH", "WORK", 
"WRITE", "YEAR", "ZONE")


sqlQuoteSpec <- function (start, end, escape = "", doubleEscape = TRUE) 
{
    list(start, end, escape, doubleEscape)
}


`.__T__dbGetStatement:DBI` <- "<environment>"

`.__T__dbGetInfo:DBI` <- "<environment>"

`.__T__dbReadTable:DBI` <- "<environment>"

sqlColumnToRownames <- function (df, row.names = NA) 
{
    name <- guessColName(df, row.names)
    if (is.null(name)) 
        return(df)
    if (!(name %in% names(df))) {
        stop("Column ", name, " not present in output", call. = FALSE)
    }
    row.names(df) <- df[[name]]
    df[[name]] <- NULL
    df
}


`.__T__dbCommit:DBI` <- "<environment>"

`.__T__dbCallProc:DBI` <- "<environment>"

sqlData <- function (con, value, row.names = NA, ...) 
{
    standardGeneric("sqlData")
}


`.__T__dbFetch:DBI` <- "<environment>"

dbIsValid <- function (dbObj, ...) 
{
    ans <- standardGeneric("dbIsValid")
    .valueClassTest(ans, "logical", "dbIsValid")
}


dbSendQuery <- function (conn, statement, ...) 
{
    ans <- standardGeneric("dbSendQuery")
    .valueClassTest(ans, "DBIResult", "dbSendQuery")
}


`.__T__dbListConnections:DBI` <- "<environment>"



## Package Data

# none


## Package Info

.skeleton_package_title = "R Database Interface"

.skeleton_package_version = "0.5-1"

.skeleton_package_depends = "methods"

.skeleton_package_imports = ""


## Internal

.skeleton_version = 5


## EOF