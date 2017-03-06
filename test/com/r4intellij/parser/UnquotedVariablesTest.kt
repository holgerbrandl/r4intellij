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

    fun testCascadedCallAsTripleDot() {
        createSkeletonLibrary("base", "datasets", "dplyr")

        checkExpression("""
            require(dplyr)
            mutate(iris, foo=paste("prefix", Species)) ## Species should be ignored
        """
        )
    }

    fun testCascadedCallAsNamedArg() {
        // todo come up with an example. Something like gather(iris, key=my_key_transform(Species)) but more real

        // not if if there is any use-case to call a function with an unquoted variable name and use the result
        // as a named parameter
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
