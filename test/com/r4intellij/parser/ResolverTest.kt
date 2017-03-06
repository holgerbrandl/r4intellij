package com.r4intellij.parser

import com.intellij.psi.util.PsiTreeUtil
import com.r4intellij.psi.api.ROperatorExpression
import com.r4intellij.psi.api.RReferenceExpression

/**
 * @author Holger Brandl
 */
class ResolverTest : AbstractResolverTest() {


    fun testSimpleAssignment() {
        checkExpression("foo = 23; 16 + foo")
    }

    fun testBlockResolve() {
        checkExpression("""
        foo = 3

        {
            foo + 1
        }
        """)
    }

    fun testBlockAssignment() {
        checkExpression("""
        foo = { 1 + 1 }
        foo
        """)
    }


    fun testVarUsedInIf() {
        checkExpression("""
            x=3
            if(T){
                x + 3
            }
            """)
    }

    fun testLocalFunctionCall() {
        checkExpression("foo = function(x) x; foo(3)")
    }

    fun testOperatorQuoteModes() {
        // we should resolve both ops and also find their usage
        checkExpression("""
        `%foo%` <- function(a,b) 3

        1 %foo% 3

        '%bar%' <- function(a,b) 3;

        1 %bar% 3
        """)

        // do additional testing here
        val symbol = PsiTreeUtil.findChildrenOfType(myFixture.file, ROperatorExpression::class.java).last().operator
        assertResolvant("'%bar%' <- function(a,b) 3", symbol.reference!!)
    }


    fun testResolveToDiamonOpReassignment() {
        // we should resolve both ops and also find their usage
        myFixture.configureByText("a.R", """
        my_data = iris
        my_data %<>% transform(foo='bar')
        my_data
        """)

        // do additional testing here
        val symbol = PsiTreeUtil.findChildrenOfType(myFixture.file, RReferenceExpression::class.java).last()
        assertResolvant("my_data %<>% transform(foo='bar')", symbol.reference!!)
    }
}