/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.formatting;

import com.intellij.formatting.*;
import com.intellij.lang.ASTNode;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.tree.ILazyParseableElementType;
import com.r4intellij.psi.RFile;
import com.r4intellij.psi.RTypes;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Block implementation for the R formatter.
 * <p/>
 * This class is based on the block code for the Groovy formatter.
 *
 * @author ilyas, jansorg
 */
public class RBlock implements Block {

    final protected ASTNode myNode;
    final protected Alignment myAlignment;
    final protected Indent myIndent;
    final protected Wrap myWrap;
    final protected CodeStyleSettings mySettings;

    protected List<Block> mySubBlocks = null;


    public RBlock(@NotNull final ASTNode node, @Nullable final Alignment alignment, @NotNull final Indent indent, @Nullable final Wrap wrap, final CodeStyleSettings settings) {
        myNode = node;
        myAlignment = alignment;
        myIndent = indent;
        myWrap = wrap;
        mySettings = settings;
    }


    @NotNull
    public ASTNode getNode() {
        return myNode;
    }


    @NotNull
    public CodeStyleSettings getSettings() {
        return mySettings;
    }


    @NotNull
    public TextRange getTextRange() {
        return myNode.getTextRange();
    }


    @NotNull
    public List<Block> getSubBlocks() {
        if (mySubBlocks == null) {
            mySubBlocks = RBlockGenerator.generateSubBlocks(myNode, myAlignment, myWrap, mySettings, this);
        }
        return mySubBlocks;
    }


    @Nullable
    public Wrap getWrap() {
        return myWrap;
    }


    @Nullable
    public Indent getIndent() {
        return myIndent;
    }


    @Nullable
    public Alignment getAlignment() {
        return myAlignment;
    }


    /**
     * Returns spacing between neighrbour elements
     *
     * @param child1 left element
     * @param child2 right element
     * @return
     */
    @Nullable
    public Spacing getSpacing(Block child1, Block child2) {
        if ((child1 instanceof RBlock) && (child2 instanceof RBlock)) {
            // todo reenable
//            Spacing spacing = RSpacingProcessor.getSpacing(((RBlock) child1), ((RBlock) child2), mySettings);
//            return spacing != null ? spacing : RSpacingProcessorBasic.getSpacing(((RBlock) child1), ((RBlock) child2), mySettings);
        }
        return null;
    }


    @NotNull
    public ChildAttributes getChildAttributes(final int newChildIndex) {
        return getAttributesByParent();
    }


    private ChildAttributes getAttributesByParent() {
        ASTNode astNode = myNode;
        final PsiElement psiParent = astNode.getPsi();
        if (psiParent instanceof RFile) {
            return new ChildAttributes(Indent.getNoneIndent(), null);
        }

        if (RTypes.R_EXPR_OR_ASSIGN == astNode.getElementType()) {
            return new ChildAttributes(Indent.getNormalIndent(), null);
        }

        return new ChildAttributes(Indent.getNoneIndent(), null);
    }


    public boolean isIncomplete() {
        return isIncomplete(myNode);
    }


    /**
     * @param node Tree node
     * @return true if node is incomplete
     */
    public boolean isIncomplete(@NotNull final ASTNode node) {
        if (node.getElementType() instanceof ILazyParseableElementType) {
            return false;
        }
        ASTNode lastChild = node.getLastChildNode();
        while (lastChild != null &&
                !(lastChild.getElementType() instanceof ILazyParseableElementType) &&
                (lastChild.getPsi() instanceof PsiWhiteSpace || lastChild.getPsi() instanceof PsiComment)) {
            lastChild = lastChild.getTreePrev();
        }
        return lastChild != null && (lastChild.getPsi() instanceof PsiErrorElement || isIncomplete(lastChild));
    }


    public boolean isLeaf() {
        return myNode.getFirstChildNode() == null;
    }
}
