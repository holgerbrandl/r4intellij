/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor;


import com.google.common.collect.Lists;
import com.intellij.lang.ASTNode;
import com.intellij.lang.folding.FoldingBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.editor.Document;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.r4intellij.parsing.RParserDefinition;
import com.r4intellij.psi.api.RForStatement;
import com.r4intellij.psi.api.RFunctionExpression;
import com.r4intellij.psi.api.RIfStatement;
import com.r4intellij.psi.api.RParameterList;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static com.r4intellij.parsing.RElementTypes.*;


/**
 * Defines how code folding should behave for Arc files
 */
public class RFoldingBuilder implements FoldingBuilder {

    public String getPlaceholderText(@NotNull ASTNode node) {
        if (node.getElementType() == R_FUNCTION_EXPRESSION) {
            RFunctionExpression def = (RFunctionExpression) node.getPsi();
            RParameterList funargs = def.getParameterList();

//            if (funargs != null) {
            return "function" + funargs.getText() + " ...";
        }

        if (node.getElementType() == R_IF_STATEMENT) {
            RIfStatement def = (RIfStatement) node.getPsi();
            return "if (" + def.getExpressionList().get(0).getText() + ")} ...";
        }
        if (node.getElementType() == R_FOR_STATEMENT) {
            RForStatement def = (RForStatement) node.getPsi();
            return "for (" + def.getExpressionList().get(0).getText() + " in " + def.getExpressionList().get(1).getText() + ")} ...";
        }

        return null;
    }


    public boolean isCollapsedByDefault(ASTNode node) {
        return false;
    }


    public FoldingDescriptor[] buildFoldRegions(ASTNode node, Document document) {
        touchTree(node);
        List<FoldingDescriptor> descriptors = new ArrayList<FoldingDescriptor>();
        // todo fixme
        appendDescriptors(node, descriptors);
        return descriptors.toArray(new FoldingDescriptor[descriptors.size()]);
    }


    /**
     * We have to touch the PSI tree to get the folding to show up when we first open a file
     */
    private void touchTree(ASTNode node) {
        if (node.getElementType() == RParserDefinition.FILE) {
            final PsiElement firstChild = node.getPsi().getFirstChild();
        }
    }


    private void appendDescriptors(final ASTNode node, final List<FoldingDescriptor> descriptors) {
        ArrayList<IElementType> foldable = Lists.newArrayList(R_FUNCTION_EXPRESSION, R_WHILE_STATEMENT, R_ELSE, R_IF_STATEMENT, R_FOR_STATEMENT);
        if (foldable.contains(node.getElementType())) {
            descriptors.add(new FoldingDescriptor(node, node.getTextRange()));
        }
//        if (node.getElementType() == R_IF) {
//            descriptors.add(new FoldingDescriptor(node, node.getTextRange()));
//        }
//        } else if (node.getElementType() == R_SECTION) {
////            descriptors.add(new FoldingDescriptor(node, node.getTextRange()));
//
////            PsiElement sectionElement = node.getPsi().getParent().getNextSibling();
//            PsiElement sectionElement = null;
//            List<RSection> rSections = ((RFile) node.getPsi().getContainingFile()).calcSections();
//            for (RSection rSection : rSections) {
//                if (rSection.getTextOffset() > node.getPsi().getTextOffset()) {
//                    sectionElement = rSection;
//                    break;
//                }
//            }
//
//            int end = sectionElement != null ? sectionElement.getTextOffset() - 1 : node.getPsi().getContainingFile().getTextRange().getEndOffset();
//            descriptors.add(new FoldingDescriptor(node, new TextRange(node.getStartOffset(), end)));
//        }

        ASTNode child = node.getFirstChildNode();
        while (child != null) {
            appendDescriptors(child, descriptors);
            child = child.getTreeNext();
        }
    }


//    private boolean isFoldableNode(ASTNode node) {
//        return (node.getElementType() == R_FUNCTION_EXPRESSION
////                || node.getElementType() == R_SECTION
////                || (node.getElementType() == MACRO_DEFINITION)
////                || (node.getElementType() == SINGLE_ARG_ANONYMOUS_FUNCTION_DEFINITION)
////                || (node.getElementType() == ANONYMOUS_FUNCTION_DEFINITION)
////                || (node.getElementType() == MULTILINE_COMMENT)
////                || (node.getElementType() == BLOCK_COMMENT
//        );
//    }
}
