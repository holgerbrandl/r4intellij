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

    /** Make sure that most common whitelisting ruels from base package are correctly working. */
    fun testBaseSubsetAndTransform() {
        createSkeletonLibrary("base", "datasets")

        checkExpression("""
            someIris = subset(iris, -Species)
            transform(someIris, total_length=Sepal.Length + Petal.Length)
        """
        )
    }

    fun testNamedArgsInStrangeOrder() {
        createSkeletonLibrary("datasets", "dplyr")

        // nothing should be detected as unresolved
        checkExpression("""
            dplyr::inner_join(by="Species", y=iris, x=iris)
        """
        )
    }


    fun testSimpleWithScopr() {
        createSkeletonLibrary("base", "datasets")

        checkExpression("""
           with(iris, Species)
        """
        )
    }

    fun testWithExpressionScope() {
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
            mutate(iris, foo=paste("prefix", Species)) ## Species should not pop here but should be ignored
        """
        )
    }

    fun testCascadedCallAsNamedArg() {
        createSkeletonLibrary("base", "datasets", "dplyr")

        checkExpression("""
            require(dplyr)

            ## without pipe
            top_n(iris, 10, wt=abs(contribution))

            ## with pipe
            #iris %>% top_n(10, wt=abs(contribution))

            # # or same with base::with
            # with(iris, expr=paste0(as.character(Item), " <- foo"))
        """
        )
    }


    fun testIgnoreAllArgs() {
        createSkeletonLibrary("datasets", "ggplot2")

        // aes is whitelisting all its args with a  *, make sure that this works
        checkExpression("""
            require(ggplot2)
            ggplot(iris, aes(Species, y=Sepal.Width, fill=Sepal.Width)) # no arg of aes should be flagged
        """)
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
        createSkeletonLibrary("stats", "datasets", "tibble")

        checkExpression("""
            require(tibble)
            lm(Species ~ Sepal.Length + Sepal.Width, data=iris)
        """
        )
    }


    // todo test whitelisting (warning -> whitelist -> no warning) --> reset for future run

}
