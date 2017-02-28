package com.r4intellij.psi.references

import com.google.common.base.CharMatcher
import com.intellij.psi.PsiElement
import com.intellij.psi.PsiElementResolveResult
import com.intellij.psi.PsiWhiteSpace
import com.intellij.psi.ResolveResult
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.util.PsiTreeUtil
import com.r4intellij.psi.api.*

/**
 * @author Holger Brandl
 */

class FileContextResolver() {

    var forwardRefs = false
    var findAll = false


    fun resolveFromInner(element: PsiElement, context: PsiElement, elementName: String): List<ResolveResult> {
        val resolveScope = PsiTreeUtil.getParentOfType(context,
                RFunctionExpression::class.java, RBlockExpression::class.java, RForStatement::class.java, RFile::class.java)

        if (resolveScope == null) return emptyList()


        // list all elements before (or after when running forward mode) and process them

        val results = emptyResults()

        // normal lookup: start as local as possible
        val scopeChildren = resolveScope.children.filterNot { it is LeafPsiElement || it is PsiWhiteSpace }

        // note: when searching forward refs we include the barrier expressoin containing the symbol to be resolved
        val candidates = if (forwardRefs) {
            scopeChildren.dropWhile { !PsiTreeUtil.isAncestor(it, element, true) && element != it }.reversed()
        } else {
            scopeChildren.takeWhile { !PsiTreeUtil.isAncestor(it, element, true) && element != it }
        }


        val iterator = candidates.reversed().iterator()
        while (iterator.hasNext() && (findAll || results.isEmpty())) {
            val candidate = iterator.next()

            // resolve until we hit the already processed element
            when (candidate) {
                is RAssignmentStatement -> {
                    resolveFromAssignment(candidate, element, elementName, results)
                }
                else -> {
                    resolveInContextByBlockRecursion(candidate, element, elementName)
                }
            }

            results.addAll(resolveInContextByBlockRecursion(candidate, element, elementName));
        }

        // since the children were processed, now process the context parameters (is any)
        // visitor pattern might work as well here
        when (resolveScope) {
            is RFunctionExpression -> {
                val list = resolveScope.getParameterList()
                for (parameter in list.getParameterList()) {
                    if (elementName == parameter.getName()) {
                        results.add(PsiElementResolveResult(parameter))
                    }
                }
            }

            is RForStatement -> {
                val target = resolveScope.getTarget()
                if (elementName == target.getName()) {
                    results.add(PsiElementResolveResult(target))
                }
            }
        }

        // now redo for next upper context level (just if all results are requested and none were found so far)
        if (findAll || results.isEmpty()) {
            results.addAll(resolveFromInner(element, resolveScope, elementName))
        }

        return results;
    }

    private fun resolveInContextByBlockRecursion(context: PsiElement, element: PsiElement, elementName: String): List<ResolveResult> {
        val result = emptyResults()

        //    RResolver.resolveFromAssignmentInContext(element, elementName, result, context)

        // also recurse into blocks, if and elses of current context
        context.acceptChildren(object : RVisitor() {
            override fun visitBlockExpression(blockExpression: RBlockExpression) {
                result.addAll(resolveInContextByBlockRecursion(blockExpression, element, elementName))
                blockExpression.acceptChildren(this)
            }

            override fun visitAssignmentStatement(statement: RAssignmentStatement) {
                resolveFromAssignment(statement, element, elementName, result)
                statement.acceptChildren(this)
            }

            override fun visitIfStatement(ifStatement: RIfStatement) {
                ifStatement.acceptChildren(this)
                super.visitIfStatement(ifStatement)
            }
        })

        // local first thus reverse
        return result.reversed();
    }
}

private fun emptyResults() = emptyList<ResolveResult>().toMutableList()


internal fun resolveFromAssignment(statement: RAssignmentStatement, element: PsiElement, elementName: String, result: MutableList<ResolveResult>) {
    val assignee = statement.assignee

    // 2nd check necessary to disallow self-references
    // --> disabled to allow for correct usage search
    // --> see design considerations in devel_notes.md
    if (assignee == null || assignee === element) return


    if (assignee.text == elementName) {
        val resolveResult = PsiElementResolveResult(statement)
        result.add(resolveResult)
    }

    // also resolve member expressions
    if (assignee is RMemberExpression) {
        val expr = assignee.expression
        if (expr.text == elementName && expr !== element) {
            // disallow self-references
            result.add(PsiElementResolveResult(statement))
        }
    }


    // if assignee is a reference expression it could be a backticked operator definition
    // same for assignees which are literal expression. R support those as well for defining operators
    if (assignee is RReferenceExpression || assignee is RStringLiteralExpression) {
        val charMatcher = CharMatcher.anyOf("`\"'")

        if (charMatcher.matches(assignee.text[0])) {
            val operatorCandidate = charMatcher.trimFrom(assignee.text)

            if (operatorCandidate == elementName) {
                result.add(PsiElementResolveResult(statement))
            }
        }
    }
}
