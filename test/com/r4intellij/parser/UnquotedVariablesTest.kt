package com.r4intellij.parser

/**
 * @author Holger Brandl
 */

class UnquotedVariablesTest : AbstractResolverTest() {


    fun testUnquotedMultidots() {
        createSkeletonLibrary("dplyr")

        // nothing should be detected as unresolved
        checkExpression("""
            require(dplyr)
            count(iris, Species, Sepal.Length)
        """
        )
    }

    fun testNamedArgsInStrangeOrder() {
        createSkeletonLibrary("dplyr")

        // nothing should be detected as unresolved
        checkExpression("""
            dplyr::inner_join(by="Species", y=iris, x=iris)
        """
        )
    }


    fun testWithScope() {
        createSkeletonLibrary("base", "datasets")

        checkExpression("""
           with(iris, Sepal.Length + Sepal.Width)
        """
        )
    }


    fun testSimpleMutate() {
        createSkeletonLibrary("base", "datasets", "dplyr")

        checkExpression("""
           dplyr::mutate(iris, foo=Species)
        """
        )
    }

    fun testCascadedMutate() {
        createSkeletonLibrary("base", "datasets", "dplyr")

        checkExpression("""
            require(dplyr)
            mutate(iris, foo=paste("prefix", Species))
        """
        )
    }

    fun testUnaryTildeFormula() {
        createSkeletonLibrary("tibble")

        checkExpression("""
            require(tibble)
            frame_data(~foo, "bar")
        """
        )
    }

    fun testBinaryTildeFormula() {
        createSkeletonLibrary("stats", "datasets")

        checkExpression("""
            require(tibble)
            lm(Species ~ Sepal.Length + Sepal.Width, data=iris2)
        """
        )
    }


    // todo test whitelisting (warning -> whitelist -> no warning) --> reset for future run

}
