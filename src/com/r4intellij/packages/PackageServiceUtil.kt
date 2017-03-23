/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages

import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.module.impl.scopes.LibraryScope
import com.intellij.openapi.progress.ProgressIndicator
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.progress.Task
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.libraries.LibraryTablesRegistrar
import com.intellij.openapi.util.Computable
import com.r4intellij.packages.RHelperUtil.getHelperOutput
import com.r4intellij.psi.api.RAssignmentStatement
import com.r4intellij.psi.api.RReferenceExpression
import com.r4intellij.psi.references.RResolver.getTrimmedFileName
import com.r4intellij.psi.stubs.RAssignmentNameIndex
import com.r4intellij.settings.LibraryUtil

private val SKELETON_TITLE = ".skeleton_package_title"
private val SKELETON_PCKG_VERSION = ".skeleton_package_version"
private val SKELETON_DEPENDS = ".skeleton_package_depends"
private val SKELETON_IMPORTS = ".skeleton_package_imports"
private val SKELETON_SKEL_VERSION = ".skeleton_version"

/**
 * @author Holger Brandl
 */

inline fun <T> runReadAction(crossinline runnable: () -> T): T {
    //    return ReadAction.compute { it -> runnable} ??
    return ApplicationManager.getApplication().runReadAction(Computable { runnable() })
}

inline fun <T> runWriteAction(crossinline runnable: () -> T): T {
    return ApplicationManager.getApplication().runWriteAction(Computable { runnable() })
}


fun rebuildIndex(project: Project, vararg packageNames: String = emptyArray()) {

    val libraryTable = LibraryTablesRegistrar.getInstance().getLibraryTable(project)
    val library = libraryTable.getLibraryByName(LibraryUtil.R_SKELETONS)


    if (library == null) {
        RPackageService.LOG.error("Could not find skeleton library")
        return
    }

    // too wide scope in scratches?
    // see https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000093684-Reference-search-scope-different-between-project-files-and-scratches-
    //    System.err.println("num keys is ${RAssignmentNameIndex.allKeys(project).size}")


    val titleStatements = runReadAction { RAssignmentNameIndex.find(SKELETON_TITLE, project, LibraryScope(project, library)) }


    val updateTitles = titleStatements.filter { packageNames.isEmpty() || packageNames.contains(getTrimmedFileName(it)) }

    var indexCounter = 0

    ProgressManager.getInstance().run(object : Task.Backgroundable(project, "Fancy title") {
        override fun run(progressIndicator: ProgressIndicator) {

            // start your process

            // Set the progress bar percentage and text
            progressIndicator.fraction = 0.10
            progressIndicator.text = "90% to finish"

            val reindexed = updateTitles.map { titleStatement ->
                progressIndicator.fraction = indexCounter++.toDouble() / updateTitles.size
                progressIndicator.text = "Indexing '${getTrimmedFileName(titleStatement)}'"

                buildPackage(titleStatement)
            }

            RPackageService.getInstance().packages.addAll(reindexed)
        }
    })
}

private val skelProps = listOf(SKELETON_TITLE, SKELETON_PCKG_VERSION, SKELETON_DEPENDS, SKELETON_IMPORTS, SKELETON_SKEL_VERSION)

private fun buildPackage(titleStatement: RAssignmentStatement): RPackage {
    val packageName = getTrimmedFileName(titleStatement)

    // ore filterIsInstance<RAssignmentStatement>()
    val skelAssignments = titleStatement.containingFile.children.mapNotNull { it as? RAssignmentStatement }

    skelAssignments.mapNotNull { (it.assignee as? RReferenceExpression) }
    val title = getSkeletonProperty(skelAssignments, SKELETON_TITLE)
    val version = getSkeletonProperty(skelAssignments, SKELETON_PCKG_VERSION)
    val imports = getSkeletonProperty(skelAssignments, SKELETON_PCKG_VERSION).split(",")
    val depends = getSkeletonProperty(skelAssignments, SKELETON_PCKG_VERSION).split(",")

    val rPackage = RPackage(packageName, version, title, depends.toSet(), imports.toSet())

    // continue here treating data and functions differently
    val nsSymbols = skelAssignments.map { it.assignee.text }.filter { !skelProps.contains(it) }
    rPackage.setFunctions(nsSymbols.map { PckgFunction(it) })

    System.err.print(".")
    return rPackage
}

private fun getSkeletonProperty(skeletonAssignments: List<RAssignmentStatement>, propertyName: String): String {
    val firstOrNull = skeletonAssignments.firstOrNull { (it.assignee as? RReferenceExpression)?.name.equals(propertyName) }

    if (firstOrNull == null) {
        throw RuntimeException("could not extract '${propertyName}' in ${skeletonAssignments.first().containingFile.name}")
    }

    return firstOrNull.assignedValue.text
}


val RHELPER_PACKAGE_VERSIONS = RHelperUtil.PluginResourceFile("package_versions.r")


/**
 * Fetch R package info including description and version.
 */
fun getInstalledPackageVersions(): Map<String, String> {
    val helperOutput = getHelperOutput(RHELPER_PACKAGE_VERSIONS)

    if (helperOutput == null) {
        return emptyMap()
    }

    return helperOutput.split("\n").map {
        val splitLine = it.split("\t")

        val packageName = splitLine[0].trim()
        val version = splitLine[1].trim()

        packageName to version
    }.toMap()
}
