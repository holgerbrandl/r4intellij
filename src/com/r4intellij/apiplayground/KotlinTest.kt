package com.r4intellij.apiplayground

import org.intellij.lang.annotations.Language

/**
 * @author Holger Brandl
 */
fun foo(): Int = 1


@Language("Groovy")
val foo = """
foo = "a*b"
"""
@Language("R")
val bar = """
if(TRUE) print('hello')
"""
