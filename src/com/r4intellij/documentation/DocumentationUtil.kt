/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.documentation

/**
 * @author Holger Brandl
 */


fun transformLinks(htmlResponse: String): String {
    // http://127.0.0.1:5678/library/dplyr/help/ungroup
    val linkMatcher = Regex("http://[0-9:.]*/library/([A-z0-9._]*)/help/([A-z0-9._]*)")
    var htmlPsiLinks = htmlResponse.replace(linkMatcher) { "psi_element://${it.groupValues[1]}::${it.groupValues[2]}" }


    // http://127.0.0.1:5678/library/tibble/html/00Index.html
    val indexMatcher = Regex("http://[0-9:.]*/library/([A-z0-9._]*)/html/00Index.html")
    htmlPsiLinks = htmlPsiLinks.replace(indexMatcher) { "psi_element://${it.groupValues[1]}" }

    return htmlPsiLinks
}