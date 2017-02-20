package com.r4intellij.codestyle

/**
 * @author Holger Brandl
 */
class FormatterTest : RFormatterTestCase() {


    fun testIfSpaceBrace() {
        commomSettings.SPACE_BEFORE_IF_LBRACE = true
        checkFormatting("""
if(TRUE) print(1)
""", """
if (TRUE) print(1)
""")
    }


    fun testLongPipeWrap() {
        checkFormatting("""
foo = bar %>% mutate() %>% group_by() %>% filter() %>% head
""", """
foo = bar %>%
    mutate() %>%
    group_by() %>%
    filter() %>%
    head
""")
    }

    fun testGgChainWrap() {
        checkFormatting(
                """
require(ggplot2)

ggplot(iris) + geom_point() +ggtitle("iris plot")+ facet_grid(~Species) + scale_x_log10()
""",
                """
require(ggplot2)

iris %>% ggplot() +
    geom_point() +
    ggtitle("iris plot") +
    facet_grid(~ Species) +
    scale_x_log10()
""")

    }
}
