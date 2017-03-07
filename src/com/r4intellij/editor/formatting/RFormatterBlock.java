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
import com.intellij.patterns.PatternCondition;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiErrorElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CommonCodeStyleSettings;
import com.intellij.psi.tree.ILazyParseableElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import com.r4intellij.RLanguage;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.psi.api.RFile;
import com.r4intellij.psi.api.ROperator;
import com.r4intellij.psi.api.ROperatorExpression;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

import static com.intellij.json.JsonElementTypes.COMMA;
import static com.intellij.json.psi.JsonPsiUtil.hasElementType;
import static com.intellij.patterns.PlatformPatterns.psiElement;
import static com.intellij.psi.formatter.FormatterUtil.isWhitespaceOrEmpty;
import static com.r4intellij.parsing.RElementTypes.*;

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
        mySpacingBuilder = spacingBuilder;
    }


    @NotNull
    public List<Block> getSubBlocks() {
        if (mySubBlocks == null) {
            mySubBlocks = ContainerUtil.mapNotNull(myNode.getChildren(null), node -> {
                if (isWhitespaceOrEmpty(node) || hasElementType(node, R_NL)) {
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

        if (hasElementType(childNode, COMMA)) {
            wrap = Wrap.createWrap(WrapType.NONE, true);
        }

        if (hasElementType(myNode, TokenSet.create(R_BLOCK_EXPRESSION)) &&
                !hasElementType(childNode, TokenSet.create(R_LBRACE, R_RBRACE))) {

            indent = Indent.getNormalIndent();

        }
//        else if (!hasElementType(childNode, TokenSet.create(R_BLOCK_EXPRESSION))) {
//            wrap = myWrap;
////            indent = Indent.getNormalIndent(true);
////            indent = Indent.getNormalIndent();
//            indent = Indent.getNoneIndent();
//        }
//        if (hasElementType(childNode, TokenSet.create(R_ASSIGNMENT_STATEMENT, R_CALL_EXPRESSION))) {
//            wrap = myWrap;
////            indent = Indent.getNormalIndent(true);
//            indent = Indent.getAbsoluteLabelIndent();
//        }
//        Indent indent = RIndentProcessor.getChildIndent(this, childNode);

        // note The indent specifies how the block is indented relative to its parent block.

        // wrapping and indentation of pipes
        PsiElement childPsi = childNode.getPsi();
        if (isRightExprInOpChain(childPsi)) {
            wrap = Wrap.createWrap(WrapType.ALWAYS, true);
//                Wrap.createWrap(mySettings.METHOD_CALL_CHAIN_WRAP, false) :

            // is first than indent(ROperatorExpression)childNode.getPsi())
            ROperatorExpression parentOpExpr = (ROperatorExpression) childPsi.getParent();

//            PsiElementPattern.Capture<ROperatorExpression> chainOpCapture = buildOpExpCapture(parentOpExpr.getOperator());
//
//            if (getSameOpChainRoot((ROperatorExpression) childPsi.getParent()) == childPsi.getParent()) {
            // is last opexpr in chain with same operator
//            if (chainOpCapture.accepts(parentOpExpr) && !psiElement().withChild(chainOpCapture).accepts(parentOpExpr)) {
//            if (parentOpExpr.getOperator().getText().equals()) {
            indent = Indent.getNormalIndent();
//            }else{
//                indent = Indent.getNormalIndent();
//            }
        } else {
            wrap = Wrap.createWrap(WrapType.NONE, false);

        }

        return new RFormatterBlock(childNode, alignment, indent, wrap, mySettings, mySpacingBuilder);
    }


    private final static PatternCondition<ROperatorExpression> IS_BINARY_OP = new PatternCondition<ROperatorExpression>("isBinary") {
        @Override
        public boolean accepts(@NotNull ROperatorExpression operatorExpression, ProcessingContext processingContext) {
            return operatorExpression.isBinary();
        }
    };

//    private final static PsiElementPattern.Capture<ROperatorExpression> BINOP_CAPTURE = psiElement(ROperatorExpression.class).with(IS_BINARY_OP);


    public static PsiElementPattern.Capture<ROperatorExpression> buildOpExpCapture(ROperator rOperator) {
        return psiElement(ROperatorExpression.class).
                withChild(psiElement(ROperator.class).withText(rOperator.getText()));
//                with(IS_BINARY_OP).
    }


    private static boolean isRightExprInOpChain(PsiElement blockPsi) {
        if (blockPsi.getParent() == null || !(blockPsi.getParent() instanceof ROperatorExpression)) return false;

        ROperatorExpression opExpr = (ROperatorExpression) blockPsi.getParent();

        // is right expr
        if (!opExpr.isBinary() || opExpr.getRightExpr() != blockPsi) return false;


        ROperatorExpression chainRootExpr = getSameOpChainRoot(opExpr);


        // is actual chain to avoid wrapping of simple binary operator expressions
        PsiElementPattern.Capture<ROperatorExpression> opCapture = buildOpExpCapture(chainRootExpr.getOperator());
        // at least chain of 3
        if (!opCapture.withChild(opCapture).accepts(chainRootExpr)) return false;

        // is long enough to justify wrapping
        if (chainRootExpr.getText().length() < 50) return false;


        // later: ensure that current chaoinRoot is not RHS in named argument list  (like in mutate(foo, foo = str_detect(Species) %>% get_col %>% trim)

        return true;
    }


    @NotNull
    private static ROperatorExpression getSameOpChainRoot(@NotNull ROperatorExpression opExpr) {
        while (true) {
            PsiElement opParent = opExpr.getParent();

            if (opParent == null || !(opParent instanceof ROperatorExpression)) return opExpr;

            ROperatorExpression opExprParent = (ROperatorExpression) opParent;


            if (opExprParent.getOperator().getText().equals(opExpr.getOperator().getText())) {
                opExpr = opExprParent;
                continue;
            }

            return opExpr;
        }
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

        return new ChildAttributes(Indent.getNormalIndent(), null);
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
