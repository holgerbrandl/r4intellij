package com.r4intellij.parser

import com.intellij.psi.util.PsiTreeUtil
import com.intellij.testFramework.fixtures.CodeInsightTestFixture
import com.r4intellij.RTestCase
import com.r4intellij.inspections.UnresolvedReferenceInspection
import com.r4intellij.inspections.UnusedVariableInspection
import com.r4intellij.psi.api.ROperator
import com.r4intellij.psi.api.ROperatorExpression
import org.intellij.lang.annotations.Language

/**
 * @author Holger Brandl
 */
class ResolverTest : RTestCase() {


    fun testSimpleAssignment() {
        createPsi("foo = 23; 16 + foo")
    }

    fun testBlockResolve() {
        createPsi("""
        foo = 3

        {
            foo + 1
        }
        """)
    }

    fun testBlockAssignment() {
        createPsi("""
        foo = { 1 + 1 }
        foo
        """)
    }


    fun testVarUsedInIf() {
        createPsi("""
            x=3
            if(T){
                x + 3
            }
            """)
    }

    fun testLocalFunctionCall() {
        createPsi("foo = function(x) x; foo(3)")
    }

    fun testOperatorQuoteModes() {
        // we should resolve both ops and also find their usage
        createPsi("""
        `%foo%` <- function(a,b) 3

        1 %foo% 3

        '%bar%' <- function(a,b) 3;

        1 %bar% 3
        """)

        // do additional testing here
        val symbol = PsiTreeUtil.findChildrenOfType(myFixture.file, ROperatorExpression::class.java).last().operator
        assertResolvant(symbol, "'%bar%' <- function(a,b) 3")
    }


    private fun assertResolvant(symbol: ROperator, expected: String) {
        val operatorResolvant = symbol.reference!!.resolve()

        assertEquals(expected, operatorResolvant!!.text)
    }


    private fun createPsi(@Language("R") expressionList: String): CodeInsightTestFixture {
        myFixture.configureByText("a.R", expressionList)

        myFixture.enableInspections(UnusedVariableInspection::class.java, UnresolvedReferenceInspection::class.java);
        myFixture.testHighlighting(true, false, false)

        return myFixture
    }
}