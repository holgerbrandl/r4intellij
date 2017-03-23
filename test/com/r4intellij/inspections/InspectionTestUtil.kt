/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.inspections

/**
 * @author Holger Brandl
 */


fun noImportWarning(symbol: String, foundIn: String): String = noImportWarning(symbol, listOf(foundIn))


fun noImportWarning(symbol: String, foundIn: List<String> = emptyList()): String {
    return """<warning descr="${UnresolvedReferenceInspection.missingImportMsg(symbol, foundIn)}">$symbol</warning>"""
}

fun unresolvedError(symbol: String) = """<error descr="${UnresolvedReferenceInspection.UNRESOLVED_MSG}">${symbol}</error>"""


fun errorForwardRef(varName: String): String {
    return "<error descr=\"Forward reference\">$varName</error>"
}

fun errorTooManyArgs(funCall: String): String {
    return """<error descr="Too many arguments">${funCall}</error>""""
}

fun errorMissingArg(argName: String, funCall: String): String {
    return """<warning descr="argument '${argName}' is missing, with no default">${funCall}</warning>"""
}
