/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.formatting;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.intellij.formatting.Indent;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.impl.source.tree.LeafPsiElement;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.psi.api.RExpression;
import com.r4intellij.psi.api.RFile;
import com.r4intellij.psi.api.RPsiElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * This class is based on code taken from the Groovy plugin.
 *
 * @author ilyas, jansorg, holgerbrandl
 */
public abstract class RIndentProcessor implements RElementTypes {

    private static final TokenSet BLOCKS = TokenSet.create(R_BLOCK_EXPRESSION);


    /**
     * Calculates indent, based on code style, between parent block and child node
     *
     * @param parent        parent block
     * @param child         child node
     * @return indent
     */
    @NotNull
    public static Indent getChildIndent(@NotNull RFormatterBlock parent, @NotNull final ASTNode child) {
        ASTNode astParent = parent.getNode();
        final PsiElement psiParent = astParent.getPsi();

        ASTNode prevChildNode = child.getTreePrev();
        // For R file
        if (psiParent instanceof RPsiElement || psiParent instanceof RFile) {
            return Indent.getNoneIndent();
        }

        if (prevChildNode != null && R_LBRACE.equals(prevChildNode.getElementType()) && !R_RBRACE.equals(child.getElementType())) {
            return Indent.getNormalIndent();
        }

        if (prevChildNode != null && R_OPERATOR.equals(prevChildNode.getElementType())) {

//            parent.getNode().getPsi().getParent().getChildren();

            // traverse up the tree to see if it a pipe chain
            int pipePosition = getPipePosition(psiParent, 0);
            if (pipePosition == 1) {
                return Indent.getNormalIndent();
//                return Indent.getContinuationWithoutFirstIndent();
//                return Indent.getContinuationIndent();
            } else {
                return Indent.getNoneIndent();
//                return Indent.getNormalIndent();
            }
        }

//        if (astParent.getPsi() instanceof RFundef) {
//
//            if (child.getElementType().equals(R_EXPR_OR_ASSIGN)) {
//                return Indent.getContinuationIndent();
////                return indentForBlock(psiParent, child);
//            }
//        }
//
//        if (child.getElementType().equals(R_EXPR_OR_ASSIGN)) {
//            return Indent.getContinuationIndent();
////                return indentForBlock(psiParent, child);
//        }

        if (BLOCKS.contains(child.getElementType())) {
            return indentForBlock(psiParent, child);
        }


        return Indent.getNoneIndent();
    }


    private static int getPipePosition(PsiElement node, int curLevel) {
        if (node != null) {

            if (node instanceof RExpression && continsPipeOp(node)) {

                return getPipePosition(node.getParent(), curLevel + 1);

            } else {
                return getPipePosition(node.getParent(), curLevel);
            }
        } else {
            return curLevel;
        }
    }


    private static boolean continsPipeOp(PsiElement node) {
        boolean hasPipeOp = Iterables.tryFind(PsiTreeUtil.getChildrenOfTypeAsList(node, LeafPsiElement.class), new Predicate<LeafPsiElement>() {
            @Override
            public boolean apply(@Nullable LeafPsiElement leafPsiElement) {
                return leafPsiElement.getElementType().equals(R_OPERATOR);
            }
        }).isPresent();

        return hasPipeOp;
    }


    /**
     * Indent for common block
     *
     * @param psiBlock
     * @param child
     * @return
     */
    private static Indent indentForBlock(PsiElement psiBlock, ASTNode child) {
        if (R_LBRACE.equals(child.getElementType()) || R_RBRACE.equals(child.getElementType())) {
            return Indent.getNoneIndent();
        }

//        if(psiBlock instanceof RFundef){
//            return Indent.getNormalIndent();
//        }
//
//        if (child.getElementType().equals(R_EXPR_OR_ASSIGN)) {
//            return Indent.getNormalIndent();
//        }
//

        return Indent.getNormalIndent();
//        return Indent.getContinuationIndent();
//        return Indent.getContinuationIndent(true);
    }
}

