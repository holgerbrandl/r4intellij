/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor;


import com.intellij.lang.ASTNode;
import com.intellij.lang.folding.FoldingBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.util.TextRange;
import com.r4intellij.psi.api.*;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static com.r4intellij.parsing.RElementTypes.*;



/**
 * Defines how code folding should behave for R files
 *
 * For details see http://www.jetbrains.org/intellij/sdk/docs/tutorials/custom_language_support/folding_builder.html
 */
public class RFoldingBuilder implements FoldingBuilder {

    public String getPlaceholderText(@NotNull ASTNode node) {
        if (node.getElementType() == R_BLOCK_EXPRESSION) {

            return "{...}";
        }
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

        if (node.getElementType() == R_ELSE) {
//            RIfStatement def = (RIfStatement) node.getPsi();
            return "else { ... } ";
        }
        if (node.getElementType() == R_FOR_STATEMENT) {
            RForStatement def = (RForStatement) node.getPsi();
            return "for (" + def.getExpressionList().get(0).getText() + " in " + def.getExpressionList().get(1).getText() + ") ...";
        }

        return null;
    }


    public boolean isCollapsedByDefault(@NotNull ASTNode node) {
        return false;
    }


    @NotNull
    public FoldingDescriptor[] buildFoldRegions(@NotNull ASTNode node, @NotNull Document document) {
        List<FoldingDescriptor> descriptors = new ArrayList<FoldingDescriptor>();
        appendDescriptors(node, descriptors);
        return descriptors.toArray(new FoldingDescriptor[descriptors.size()]);
    }


    private void appendDescriptors(final ASTNode node, final List<FoldingDescriptor> descriptors) {

        if (node.getElementType() == R_BLOCK_EXPRESSION) {
            RBlockExpression blockExpr = (RBlockExpression) node.getPsi();


            int lbraceStart = blockExpr.getLbrace().getTextRange().getStartOffset();
            int rbraceStart = blockExpr.getRbrace().getTextRange().getEndOffset();

            descriptors.add(new FoldingDescriptor(node, new TextRange(lbraceStart, rbraceStart)));
        }


        ASTNode child = node.getFirstChildNode();
        while (child != null)


        {
            appendDescriptors(child, descriptors);
            child = child.getTreeNext();
        }
    }
}
