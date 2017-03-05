package com.r4intellij.parser

import com.intellij.psi.PsiReference
import com.intellij.testFramework.fixtures.CodeInsightTestFixture
import com.r4intellij.RTestCase
import com.r4intellij.inspections.UnresolvedReferenceInspection
import com.r4intellij.inspections.UnusedVariableInspection
import org.intellij.lang.annotations.Language

/**
 * @author Holger Brandl
 */
open class AbstractResolverTest : RTestCase() {
    protected fun assertResolvant(expected: String, reference: PsiReference) {
        val operatorResolvant = reference.resolve()

        assertEquals(expected, operatorResolvant!!.text)
    }

    protected fun createPsi(@Language("R") expressionList: String, testWarnings: Boolean = true): CodeInsightTestFixture {
        myFixture.configureByText("a.R", expressionList)

        myFixture.enableInspections(
                UnusedVariableInspection::class.java,
                UnresolvedReferenceInspection::class.java
        );

        myFixture.testHighlighting(testWarnings, false, false)

        return myFixture
    }
}