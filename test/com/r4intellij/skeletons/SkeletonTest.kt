/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.skeletons

import com.r4intellij.RTestCase
import com.r4intellij.interpreter.RSkeletonGenerator.SKELETONIZE_PACKAGE
import com.r4intellij.packages.RHelperUtil
import java.io.File

/**
 * @author Holger Brandl
 */

class SkeletonTest : RTestCase() {

    val TEST_DIRECTORY = "unit_test_skeletons"

    //    companion object {
    //
    //        @BeforeClass @JvmStatic
    //        fun buildSkeletons() {
    //
    //        }
    //
    //    }

    fun testParsability() {
        val testPackages = listOf("base", "ggplot2", "dplyr", "stats")

        File(TEST_DIRECTORY).mkdir()

        val buildSkeleton = { pckg: String ->
            RHelperUtil.runHelperWithArgs(SKELETONIZE_PACKAGE, TEST_DIRECTORY, pckg)
        }

        assertFalse(testPackages.map(buildSkeleton).any { runResult -> runResult!!.exitCode != 0 })


        testPackages.forEach { pckg ->
            myFixture.configureByFile(File(TEST_DIRECTORY, pckg + ".R").absolutePath)
            myFixture.checkHighlighting() // should be all green
        }

        // test a few assumptions about what the skeletons should incldue

        // numeric package constants
        assertTrue(File(TEST_DIRECTORY, "ggplot2.R").readLines().any {
            line ->
            line == ".pt <- 2.845276"
        })

        // complex objects
        assertTrue(File(TEST_DIRECTORY, "ggplot2.R").readLines().any {
            line ->
            line == """GeomBar <- "<environment>""""
        })

        // re-exported symbols
        assertTrue(File(TEST_DIRECTORY, "dplyr.R").readLines().any { line ->
            line.contains("data_frame <- tibble::data_frame")
        })

        // bundled data sets
        assertTrue(File(TEST_DIRECTORY, "dplyr.R").readLines().any { line ->
            line.contains("""nasa <- dplyr::nasa""")
        })
    }
}