package com.r4intellij.parser

import com.r4intellij.inspections.RInspectionTest

/**
 * @author Holger Brandl
 */

class UnquotedVariablesTest : AbstractResolverTest() {


    fun testUnquotedMultidots() {
        RInspectionTest.createLibraryFromPckgNames(myFixture, "dplyr")

        // nothing should be detected as unresolved
        createPsi("""
            require(dplyr)
            count(iris, Species, Sepal.Length)
        """
        )
    }

    fun testNamedArgsInStrangeOrder() {
        RInspectionTest.createLibraryFromPckgNames(myFixture, "dplyr")

        // nothing should be detected as unresolved
        createPsi("""
            dplyr::inner_join(by="Species", y=iris, x=iris)
        """
        )
    }


    fun testWithScope() {
        RInspectionTest.createLibraryFromPckgNames(myFixture, "base", "datasets")

        createPsi("""
           with(iris, Sepal.Length + Sepal.Width)
        """
        )
    }


    fun testSimpleMutate() {
        RInspectionTest.createLibraryFromPckgNames(myFixture, "base", "datasets", "dplyr")

        createPsi("""
           dplyr::mutate(iris, foo=Species)
        """
        )
    }

    fun testCascadedMutate() {
        RInspectionTest.createLibraryFromPckgNames(myFixture, "base", "datasets", "dplyr")

        createPsi("""
            requir(dplyr)
           mutate(iris, foo=paste("prefix", Species))
        """
        )
    }


    // todo test whitelisting (warning -> whitelist -> no warning) --> reset for future run

}
