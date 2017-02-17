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
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.tree.ILazyParseableElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.containers.ContainerUtil;
import com.r4intellij.RLanguage;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.psi.api.RFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static com.intellij.json.psi.JsonPsiUtil.hasElementType;
import static com.intellij.psi.formatter.FormatterUtil.isWhitespaceOrEmpty;
import static com.r4intellij.parsing.RElementTypes.R_LBRACE;
import static com.r4intellij.parsing.RElementTypes.R_RBRACE;

/**
 * Block implementation for the R formatter.
 * <p/>
 * This class is based on the block code for the Groovy and Json formatter.
 *
 * @author ilyas, jansorg, holgerbrandl
 */
public class RFormatterBlock implements ASTBlock {

    final protected ASTNode myNode;
    final protected Alignment myAlignment;
    final protected Indent myIndent;
    final protected Wrap myWrap;
    final protected CodeStyleSettings mySettings;

    final SpacingBuilder mySpacingBuilder;


    protected List<Block> mySubBlocks = null;


    public RFormatterBlock(@NotNull final ASTNode node, @Nullable final Alignment alignment, @NotNull final Indent indent, @Nullable final Wrap wrap, final CodeStyleSettings settings, SpacingBuilder spacingBuilder) {
        myNode = node;
        myAlignment = alignment;
        myIndent = indent;
        myWrap = wrap;
        mySettings = settings;

        // TODO fine tune rules depending on psi elements
        // alternatively this could be done in com.r4intellij.editor.formatting.RBlockGenerator.generateSubBlocks(); see for example com.intellij.json.formatter.JsonBlock#makeSubBlock

//        myPsiElement = node.getPsi();
//
//        if (myPsiElement instanceof JsonObject) {
//            myChildWrap = Wrap.createWrap(getCustomSettings().OBJECT_WRAPPING, true);
//        }

        mySpacingBuilder = spacingBuilder;
    }


    @NotNull
    public List<Block> getSubBlocks() {
        if (mySubBlocks == null) {
            mySubBlocks = ContainerUtil.mapNotNull(myNode.getChildren(null), node -> {
                if (isWhitespaceOrEmpty(node)) {
                    return null;
                }
                return makeSubBlock(node);
            });
        }
        return mySubBlocks;
    }


    @NotNull
    public RFormatterBlock makeSubBlock(ASTNode childNode) {
        Indent indent = Indent.getNoneIndent();
        Alignment alignment = null;
        Wrap wrap = null;


        if (!hasElementType(childNode, TokenSet.create(R_LBRACE, R_RBRACE))) {
            wrap = myWrap;
//            indent = Indent.getNormalIndent(true);
            indent = Indent.getNormalIndent();
        }
//        if (hasElementType(childNode, TokenSet.create(R_ASSIGNMENT_STATEMENT, R_CALL_EXPRESSION))) {
//            wrap = myWrap;
////            indent = Indent.getNormalIndent(true);
//            indent = Indent.getAbsoluteLabelIndent();
//        }
//        Indent indent = RIndentProcessor.getChildIndent(this, childNode);

        return new RFormatterBlock(childNode, alignment, indent, wrap, mySettings, mySpacingBuilder);
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


    @Nullable
    public Spacing getSpacing(Block child1, @NotNull Block child2) {
//        if ((child1 instanceof RFormatterBlock) && (child2 instanceof RFormatterBlock)) {
        // todo reenable
//            Spacing spacing = RSpacingProcessor.getSpacing(((RFormatterBlock) child1), ((RFormatterBlock) child2), mySettings);
//            return spacing != null ? spacing : RSpacingProcessorBasic.getSpacing(((RFormatterBlock) child1), ((RFormatterBlock) child2), mySettings);
//        }
        return mySpacingBuilder.getSpacing(this, child1, child2);
//        return new SpacingBuilder(mySettings, RLanguage.getInstance()).around(OPERATORS).spaces(1, false).getSpacing(this, child1, child2);
    }


    // see http://www.jetbrains.org/intellij/sdk/docs/reference_guide/custom_language_support/code_formatting.html
    @NotNull
    public ChildAttributes getChildAttributes(final int newChildIndex) {
        ASTNode astNode = myNode;
        final PsiElement psiParent = astNode.getPsi();
        if (psiParent instanceof RFile) {
            return new ChildAttributes(Indent.getNoneIndent(), null);
        }

        if (RElementTypes.R_EXPRESSION == astNode.getElementType()) {
            return new ChildAttributes(Indent.getContinuationIndent(), null);
        }

        return new ChildAttributes(Indent.getContinuationIndent(), null);
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


    @NotNull
    public ASTNode getNode() {
        return myNode;
    }


    @NotNull
    public TextRange getTextRange() {
        return myNode.getTextRange();
    }


    @NotNull
    public CodeStyleSettings getSettings() {
        return mySettings;
    }


    @NotNull
    private RCodeStyleSettings getCustomSettings() {
        return mySettings.getCustomSettings(RCodeStyleSettings.class);
    }


    @NotNull
    private CommonCodeStyleSettings getCommonSettings() {
        return mySettings.getCommonSettings(RLanguage.getInstance());
    }

}
