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
import com.intellij.psi.PsiElement;
import com.r4intellij.lang.parser.RParserDefinition;
import com.r4intellij.psi.RFormlist;
import com.r4intellij.psi.RFundef;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

import static com.r4intellij.psi.RTypes.R_FUNDEF;


/**
 * Defines how code folding should behave for Arc files
 */
public class RFoldingBuilder implements FoldingBuilder {

    public String getPlaceholderText(@NotNull ASTNode node) {
        if (node.getElementType() == R_FUNDEF) {
            RFundef def = (RFundef) node.getPsi();
            RFormlist funargs = def.getFormlist();
            if (funargs != null) {
                return "function(" + funargs.getText() + ")...";
            } else {
                return "function()...";
            }
        }
//        } else if (node.getElementType() == MACRO_DEFINITION) {
//            Mac def = (Mac) node.getPsi();
//            return "(mac " + def.getName() + " ...)";
//        } else if (node.getElementType() == SINGLE_ARG_ANONYMOUS_FUNCTION_DEFINITION) {
//            return "[...]";
//        } else if (node.getElementType() == ANONYMOUS_FUNCTION_DEFINITION) {
//            return "(fn ...)";
//        } else if (node.getElementType() == BLOCK_COMMENT || node.getElementType() == MULTILINE_COMMENT) {
//            String text = node.getText();
//            return text.length() > 20 ? text.substring(0, 20) + "..." : text;
//        }
        return null;
    }

    public boolean isCollapsedByDefault(ASTNode node) {
        return false;
    }

    public FoldingDescriptor[] buildFoldRegions(ASTNode node, Document document) {
        touchTree(node);
        List<FoldingDescriptor> descriptors = new ArrayList<FoldingDescriptor>();
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
        if (isFoldableNode(node)) {
            descriptors.add(new FoldingDescriptor(node, node.getTextRange()));
        }

        ASTNode child = node.getFirstChildNode();
        while (child != null) {
            appendDescriptors(child, descriptors);
            child = child.getTreeNext();
        }
    }

    private boolean isFoldableNode(ASTNode node) {
        return (node.getElementType() == R_FUNDEF
//                || (node.getElementType() == MACRO_DEFINITION)
//                || (node.getElementType() == SINGLE_ARG_ANONYMOUS_FUNCTION_DEFINITION)
//                || (node.getElementType() == ANONYMOUS_FUNCTION_DEFINITION)
//                || (node.getElementType() == MULTILINE_COMMENT)
//                || (node.getElementType() == BLOCK_COMMENT
        );
    }
}
