package com.r4intellij.psi.references

import com.intellij.psi.PsiElementResolveResult
import com.intellij.psi.ResolveResult

/**
 * @author Holger Brandl
 */

class MissingImportResolveResult(val containedIn: List<ResolveResult>) : PsiElementResolveResult(containedIn.get(index = 0).element!!) {}