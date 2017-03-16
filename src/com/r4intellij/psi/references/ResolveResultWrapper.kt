package com.r4intellij.psi.references

import com.intellij.psi.PsiElement
import com.intellij.psi.ResolveResult
import com.r4intellij.RPsiUtils

/**
 * @author Holger Brandl
 */

class ResolveResultWrapper(val element: PsiElement, val includeFwdRefs: Boolean, resolveResults: Array<ResolveResult>) {

    val results = resolveResults.toList();


    fun getBestResolve(): ResolveResult? {
        val resultsInScope = results.filterNot { it is MissingImportResolveResult }

        val fwdRefPredicate = RPsiUtils.createForwardRefPredicate(element)

        // get most local backward reference
        val mostLocalRef: ResolveResult? = resultsInScope
                .filterNot { fwdRefPredicate.test(it) }
                .lastOrNull()

        val bestRef: ResolveResult?

        if (includeFwdRefs) {
            // or first forward reference
            bestRef = mostLocalRef ?: resultsInScope.firstOrNull()
        } else {
            bestRef = mostLocalRef
        }

        return bestRef
    }

    fun getBest(): PsiElement? = getBestResolve()?.element

    fun getImportOptions(): List<String> {
        val importOptions = results.firstOrNull { it is MissingImportResolveResult } as? MissingImportResolveResult

        if (importOptions == null) return emptyList()

        return importOptions.containedIn.map { RResolver.getSkeletonPckgName(it) }
    }
}