package com.r4intellij.inspections

import com.intellij.testFramework.fixtures.CodeInsightTestFixture
import com.r4intellij.RTestCase
import com.r4intellij.inspections.UnresolvedReferenceInspection.missingImportMsg
import com.r4intellij.inspections.UnresolvedReferenceInspectionTest.forwardRef
import org.jetbrains.annotations.NotNull

/**
 * Test if package dependencies are resolved correctly.
 *
 * @author Holger Brandl
 */
class DependencyTests : RTestCase() {


    override fun configureFixture(@NotNull myFixture: CodeInsightTestFixture) {
        myFixture.enableInspections(UnresolvedReferenceInspection::class.java)
    }

    /**
     * since it is associated with base the package should be resolvable. This should come from  stub-index
     */
    fun testIris() {
        // myFixture.addFileToProject("base.R", readFileAsString(getSkeletonPath("utils").toPath()));

        // rather use actual library here to see if stub-index is working correctly
        createSkeletonLibrary("datasets")
        doExprTest("iris")
    }


    fun testPackageDataWithoutImport() {
        doExprTest("<warning descr=\"Unresolved reference\">nasa</warning>") // todo why unresolved --> should be be captured by import inspection
    }


    fun testPackageData() {
        createSkeletonLibrary("dplyr")
        doExprTest("require(dplyr); nasa")
    }


    fun testTransitiveDependencies() {
        //        addPckgsToSkeletonLibrary("caret", "ggplot2");
        doExprTest("require(caret); ggplot(iris)")
    }


    fun testForwardImportAfterUsage() {
        doExprTest(forwardRef("glimpse") + "(iris) ; require(dplyr)")
    }


    // see https://github.com/tidyverse/tidyverse/issues/40
    fun testTransitiveTidyverseDependencies() {
        createSkeletonLibrary("dplyr", "datasets")
        doExprTest("require(tidyverse); group_by(iris)")
    }


    fun testTidyrImportMissing() {
        doExprTest("${noImportWarning("gather", "tidyr")}(iris)")
    }

    fun testMissingImportInPipe() {
        doExprTest("iris %>% ${noImportWarning("glimpse", "tibble")}")
    }


    fun testResolveNamespaceCall() {
        addPckgsToSkeletonLibrary("dplyr")
        doExprTest("dplyr::group_by(iris)")
    }

    fun testMissingOperatorImport() {
        addPckgsToSkeletonLibrary("dplyr")

        // note: rexporting packages are not listed intentionally, because orign should be preferred over rexport
        doExprTest("iris ${noImportWarning("%>%", listOf("magrittr"))} head")
    }


    companion object {

        fun noImportWarning(symbol: String, foundIn: String): String = noImportWarning(symbol, listOf(foundIn))


        fun noImportWarning(symbol: String, foundIn: List<String> = emptyList()): String {
            return """<warning descr="${missingImportMsg(symbol, foundIn)}">$symbol</warning>"""
        }
    }
}
