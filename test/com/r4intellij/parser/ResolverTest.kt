package com.r4intellij.parser

import com.intellij.patterns.PlatformPatterns.psiElement
import com.intellij.patterns.PsiElementPattern
import com.intellij.psi.SyntaxTraverser
import com.intellij.psi.util.PsiTreeUtil
import com.r4intellij.psi.api.RArgumentList
import com.r4intellij.psi.api.RExpression
import com.r4intellij.psi.api.ROperatorExpression
import com.r4intellij.psi.api.RReferenceExpression

/**
 * @author Holger Brandl
 */
class ResolverTest : AbstractResolverTest() {

    fun testResolveFunctionWithoutCall() {
        createBaseLibraryWith("dplyr")

        checkExpression("require(dplyr); glimpse")
    }

    fun testSpecialConstantsFromBase() {
        checkExpression("""
        NaN
        NA
        Inf
        """)

        // todo are there others?
    }


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
            if(TRUE){
                x + 3
            }
            """)
    }

    fun testLocalFunctionCall() {
        checkExpression("foo = function(x) x; foo(3)")
    }

    fun testNsOperatorRef() {
        createSkeletonLibrary("dplyr")
        checkExpression("""dplyr::`%>%`""")
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
        val symbol = PsiTreeUtil.findChildrenOfType(myFixture.file!!, ROperatorExpression::class.java).last().operator
        assertResolvant("'%bar%' <- function(a,b) 3", symbol!!.reference!!)
    }


    val dotMatcher: PsiElementPattern.Capture<RExpression> = psiElement(RExpression::class.java)
            .withText(".")
            .withParent(psiElement(RArgumentList::class.java))


    fun testDontFlagLhsCallInPipe() {
        // we had a regression that the LHS was incorrectly considered as pipe-target.
        // This test should avoid it from happening again

        createBaseLibraryWith("magrittr")
        checkExpression("""
        library(magrittr)
        names(iris) %>% make.unique(sep= "_")
        """)
    }

    // todo v1.2 reenable
    fun _testResolveDotToBlockExpr() {
        createSkeletonLibrary("magrittr")
        checkExpression("""
        require(magrittr)
        { 1+1 } %>% length(.)
        """)


        //        https://intellij-support.jetbrains.com/hc/en-us/community/posts/115000118964-Utility-methods-for-tree-processing-using-PsiElementPattern-
        val matches = SyntaxTraverser.psiTraverser(myFixture.file).filter(dotMatcher::accepts)
    }


    fun _testSimpleDotUsageInPipe() {
        // todo v1.2
        createSkeletonLibrary("datasets", "magrittr")

        checkExpression("""
        require(magrittr)
        iris %>% paste0(., ".gephi.txt")
        """)
    }

    fun _testComplexDotUsageInPipe() {
        // todo v1.2

        createSkeletonLibrary("datasets", "magrittr", "dplyr")
        checkExpression("""
        require(dplyr)
        irisModel = lm(Sepal.Length ~  Species * Sepal.Width, data=iris)
        iris %>% transform(., Y = exp(predict(irisModel, newdata=.)))
        """)

        // other example


        // ensure that both dots are resolved correctly
    }

    fun _testSubPipe() {
        // todo v1.1 affects pipe-support and unquoted context detection
        createSkeletonLibrary("datasets", "magrittr", "dplyr")

        checkExpression("""
        require(dplyr)
        require(stringr)

        iris %>% mutate(contig = str_split_fixed(contig, "[x]", 2) %>% paste("_suffix")) %>%
        """)

    }

    // todo v1.2 reenable
    fun _testResolveToDiamondOpReassignment() {
        myFixture.configureByText("a.R", """
        my_data = iris
        my_data %<>% transform(foo='bar')
        my_data
        """)

        // make sure that the last line resolves to the second and NOT to the initial declaration
        val symbol = PsiTreeUtil.findChildrenOfType(myFixture.file, RReferenceExpression::class.java).last()
        assertResolvant("my_data %<>% transform(foo='bar')", symbol.reference!!)

        // todo in a very strict sense this is wrong, since %<>% might be overloaded. and the resolver should
        // resolve to either first or second line depending on how %<>% is defined
    }
}