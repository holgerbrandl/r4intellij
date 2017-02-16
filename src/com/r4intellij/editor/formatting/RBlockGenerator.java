/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.formatting;

import com.intellij.formatting.Alignment;
import com.intellij.formatting.Block;
import com.intellij.formatting.Indent;
import com.intellij.formatting.Wrap;
import com.intellij.lang.ASTNode;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.r4intellij.parsing.RElementTypes;

import java.util.ArrayList;
import java.util.List;

/**
 * Utility class to generate myBlock hierarchy
 * <p/>
 * This code was taken from the Groovy plugin.
 *
 * @author ilyas, jansorg
 */
public class RBlockGenerator implements RElementTypes {

    // blocking is the key to formatting
    // https://confluence.jetbrains.com/display/IDEADEV/Developing+Custom+Language+Plugins+for+IntelliJ+IDEA#DevelopingCustomLanguagePluginsforIntelliJIDEA-CodeFormatter


    public static List<Block> generateSubBlocks(ASTNode node,
                                                Alignment _myAlignment,
                                                Wrap _myWrap,
                                                CodeStyleSettings _mySettings,
                                                RFormatterBlock block) {

        // For other cases
        final ArrayList<Block> subBlocks = new ArrayList<Block>();
        ASTNode children[] = getRChildren(node);
        ASTNode prevChildNode = null;
        for (ASTNode childNode : children) {
            if (canBeCorrectBlock(childNode)) {
                Indent indent = RIndentProcessor.getChildIndent(block, prevChildNode, childNode);
                subBlocks.add(new RFormatterBlock(childNode, _myAlignment, indent, _myWrap, _mySettings));

                prevChildNode = childNode;
            }
        }
        return subBlocks;
    }


    /**
     * @param node Tree node
     * @return true, if the current node can be myBlock node, else otherwise
     */
    private static boolean canBeCorrectBlock(final ASTNode node) {
        return (node.getText().trim().length() > 0);
    }


    private static ASTNode[] getRChildren(final ASTNode node) {
        //PsiElement psi = node.getPsi();
        /*if (psi instanceof OuterLanguageElement) {
            TextRange range = node.getTextRange();
            ArrayList<ASTNode> childList = new ArrayList<ASTNode>();
            PsiFile groovyFile = psi.getContainingFile().getViewProvider().getPsi(RFileType.BASH_LANGUAGE);
            if (groovyFile instanceof RFile) {
                addChildNodes(groovyFile, childList, range);
            }
            return childList.toArray(new ASTNode[childList.size()]);
        } */

        return node.getChildren(null);
    }

    /*private static void addChildNodes(PsiElement elem, ArrayList<ASTNode> childNodes, TextRange range) {
        ASTNode node = elem.getNode();
        if (range.contains(elem.getTextRange()) && node != null) {
            childNodes.add(node);
        } else {
            for (PsiElement child : elem.getChildren()) {
                addChildNodes(child, childNodes, range);
            }
        }

    } */
}
