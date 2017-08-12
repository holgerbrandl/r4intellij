/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages

import com.google.common.base.CharMatcher
import com.google.common.collect.Lists
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.module.impl.scopes.LibraryScope
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.progress.Task
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Computable
import com.r4intellij.packages.RHelperUtil.getHelperOutput
import com.r4intellij.packages.RSkeletonGenerator.*
import com.r4intellij.psi.api.RAssignmentStatement
import com.r4intellij.psi.references.RResolver.getSkeletonLibrary
import com.r4intellij.psi.references.RResolver.getTrimmedFileName
import com.r4intellij.psi.stubs.RAssignmentNameIndex
import java.io.File

val SKELETON_PROPERTIES: List<String> = listOf(SKELETON_TITLE, SKELETON_PCKG_VERSION, SKELETON_DEPENDS, SKELETON_IMPORTS, SKELETON_SKEL_VERSION)

val RHELPER_PACKAGE_VERSIONS = RHelperUtil.PluginResourceFile("package_versions.r")


/**
 * @author Holger Brandl
 */

// see /Users/brandl/projects/jb/intellij-community/platform/projectModel-api/src/com/intellij/openapi/application/actions.kt
inline fun <T> runReadAction(crossinline runnable: () -> T): T {
    //    return ReadAction.compute { it -> runnable} ??
    return ApplicationManager.getApplication().runReadAction(Computable { runnable() })
}

inline fun <T> runWriteAction(crossinline runnable: () -> T): T {
    return ApplicationManager.getApplication().runWriteAction(Computable { runnable() })
}


fun rebuildIndex(project: Project) {

    val library = getSkeletonLibrary(project)

    if (library == null) {
        RPackageService.LOG.error("Could not find skeleton library")
        return
    }


    // identify packages missing or outdated in cache
    val indexCache = RIndexCache.getInstance()

    // too wide scope in scratches?
    // see https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000093684-Reference-search-scope-different-between-project-files-and-scratches-
    //    System.err.println("num keys is ${RAssignmentNameIndex.allKeys(project).size}")


    if (project.isDisposed) return

    val titleStatements = runReadAction { RAssignmentNameIndex.find(SKELETON_TITLE, project, LibraryScope(project, library)) }

    val updateTitles = titleStatements.filter({
        val pckgName = getTrimmedFileName(it)
        val cachePckg = indexCache.getByName(pckgName)
        cachePckg == null || !isSamePckgVersion(File(it.containingFile.virtualFile.canonicalPath), cachePckg.version)
    })


    if (updateTitles.isEmpty()) {
        // refresh cache as well here to also clear deleted packages
        indexCache.replaceAndCleanup(Lists.newArrayList(), project)
        return
    }

    var indexCounter = 0

    ProgressManager.getInstance().run(object : Task.Backgroundable(project, "Refreshing R Index Cache...") {
        override fun run(progressIndicator: ProgressIndicator) {

            val reindexed = updateTitles.map { titleStatement ->
                progressIndicator.fraction = indexCounter++.toDouble() / updateTitles.size
                progressIndicator.text = "Caching index of '${getTrimmedFileName(titleStatement)}'"

                runReadAction { buildPackage(titleStatement) }
            }

            // add the recached ones
            indexCache.replaceAndCleanup(reindexed, project)
        }
    })
}


private val dquoteMatcher = CharMatcher.anyOf("\"")

private fun buildPackage(titleStatement: RAssignmentStatement): RPackage {
    val packageName = getTrimmedFileName(titleStatement)

    // ore filterIsInstance<RAssignmentStatement>()
    val skelAssignments = titleStatement.containingFile.children.mapNotNull { it as? RAssignmentStatement }

    val splitBySkelProp = skelAssignments.groupBy({ it -> SKELETON_PROPERTIES.contains(it.assignee.text) }, { it })


    // narrow down to properties
    val skelProps = runReadAction {
        splitBySkelProp.get(true)!!.map { propAssign ->
            propAssign.assignee.text to dquoteMatcher.trimFrom(propAssign.assignedValue.text)
        }
    }.toMap()

    // not we can assume the presence of all properties here, otherwise the skeleton would have not been copy into the library
    val title = skelProps.get(SKELETON_TITLE)!!
    val version = skelProps.get(SKELETON_PCKG_VERSION)!!
    val imports = skelProps.get(SKELETON_IMPORTS)!!.split(",")
    val depends = skelProps.get(SKELETON_DEPENDS)!!.split(",")

    val rPackage = RPackage(packageName, version, title, depends.toSet(), imports.toSet())

    // continue here treating data and functions differently
    val symbols = splitBySkelProp.getOrElse(false, { emptyList() })
    val dataFunSplit = symbols.partition { it -> it.assignedValue.text.startsWith(packageName!! + "::") }
    val packageFunctions = dataFunSplit.second.map { PckgFunction(it.assignee.text) }
    val packageData = dataFunSplit.first.map { PckgDataSet(it.assignee.text) }

    rPackage.setFunctions(packageFunctions)
    rPackage.setDatSets(packageData)

    //    System.err.print(".")
    return rPackage
}


/**
 * Fetch R package info including description and version.
 */
fun getInstalledPackageVersions(): Map<String, String> {
    val helperOutput = getHelperOutput(RHELPER_PACKAGE_VERSIONS) ?: return emptyMap()


    // remove try-catch once #111 has been resolved
    return helperOutput.split("\n").filter { it.isNotBlank() }.map {
        val splitLine = it.split("\t")

        try {
            val packageName = splitLine[0].trim()
            val version = splitLine[1].trim()

            packageName to version
        } catch(e: Throwable) {
            throw RuntimeException("failed to split package-version in line '$it'.\n Helper output was:\n" + helperOutput, e)
        }

    }.toMap()

}
