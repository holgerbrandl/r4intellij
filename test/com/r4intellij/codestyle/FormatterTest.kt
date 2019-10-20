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


    fun testIndentAfterRBracketEnter() {
        checkFormatting("""
mutate(
iris,
bla = 1
)
""", """
mutate(
    iris,
    bla = 1
)
""")
    }

    fun testGgChainWrap() {
        checkFormatting(
                """
require(ggplot2)

iris %>% ggplot()  + geom_point() +ggtitle("iris plot")+ facet_grid(~Species) + scale_x_log10()
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

    fun testIndentWithoutBraces() = checkFormatting(
            """
            demo <- function(x) {
                if (x) if (x) print(x)
                if (x) if (x)
                    print(x)
                if (x)
                    if (x)
                        print(x)
                while (x)
                    sum <- sum + x
                repeat
                    print(x)
                repeat
                    break
                for (i in x)
                    next
                repeat
                    1
            }
            """,

            """
            demo <- function(x) {
                if (x) if (x) print(x)
                if (x) if (x)
                    print(x)
                if (x)
                    if (x)
                        print(x)
                while (x)
                    sum <- sum + x
                repeat
                    print(x)
                repeat
                    break
                for (i in x)
                    next
                repeat
                    1
            }
            """)

    fun testIndentFunctionWithoutBraces() = checkFormatting(
            """
            echo<-function(x)message(x)
            echo <- function ( x ) message ( x )
            echo  <-  function  (  x  )  message  (  x  )

            echo <- function(x)
            message(x)

            echo <- function(x)
                    message(x)
            """,

            """
            echo <- function(x)message(x)  # TODO: add spaces
            echo <- function (x) message (x)  # TODO: remove spaces
            echo <- function  (x)  message  (x)  # TODO: remove spaces

            echo <- function(x)
                message(x)

            echo <- function(x)
                message(x)
            """)

    fun testIndentWithBraces() = checkFormatting(
            """
            demo <- function(x) {
                if (x) if (x) { action() }
                if (x)
                    if (x) { action() }
                if (x) {
                    action()
                }
                if (x)
                {
                    action()
                }
            }
            """,

            """
            demo <- function(x) {
                if (x) if (x) { action() }
                if (x)
                    if (x) { action() }
                if (x) {
                    action()
                }
                if (x)
                {
                    action()
                }
            }
            """)

    fun testSpaceAfterIf() = checkFormatting(
            """
            if (1) 1 else 2
            if (1) '' else ''
            if (1) print(1)
            if (1) f(1)
            if(1)if(1)print(1)

            demo <- function(x) {
                if (x) 1 else 2
                if (x) 1
            }
            """,

            """
            if (1) 1 else 2
            if (1) '' else ''
            if (1) print(1)
            if (1) f(1)
            if (1) if (1) print(1)

            demo <- function(x) {
                if (x) 1 else 2
                if (x) 1
            }
            """)

    fun testSpaceParentheses() = checkFormatting(
            """
            x <- a (0) + a(0) + a( 1 ) + a(  2  )
            x <- a ((0)) + a((0)) + a( ( 1 ) ) + a(  (  2  )  )
            """,

            """
            x <- a (0) + a(0) + a(1) + a(2)  # TODO: remove first space
            x <- a ((0)) + a((0)) + a((1)) + a((2))  # TODO: remove first space
            """)

    fun testSpaceBrackets() = checkFormatting(
            """
            x <- a [0] + a[0] + a[ 1 ] + a[  2  ]
            x <- a [[0]] + a[[0]] + a[[ 1 ]] + a[[  2  ]]
            """,

            """
            x <- a[0] + a[0] + a[1] + a[2]
            x <- a[[0]] + a[[0]] + a[[1]] + a[[2]]
            """)

    fun testSpaceBraces() = checkFormatting(
            """
            x <- {0} + { 1 } + {  2  }
            """,

            """
            x <- {0} + { 1 } + {  2  }
            """)

    fun testSpaceParenthesesBraces() = checkFormatting(
            """
            if(x){action({code})}
            if ( x ) { action ( { code } ) }
            if  (  x  )  {  action  (  {  code  }  )  }
            """,

            """
            if (x) {action({code})}
            if (x) { action ({ code }) }
            if (x) {  action  ({  code  })  }
            """)

    fun testFunctionCall() = checkFormatting(
            """
            message <- c(
            'first',
            'second',
            'third')
            message <- c(
                'first',
                'second',
                'third')
            message <- c(
                    'first',
                    'second',
                    'third')
            message <- c(
                         'first',
                         'second',
                         'third')
            """,

            """
            message <- c(
                'first',
                'second',
                'third')
            message <- c(
                'first',
                'second',
                'third')
            message <- c(
                'first',
                'second',
                'third')
            message <- c(
                'first',
                'second',
                'third')
            """)
}
