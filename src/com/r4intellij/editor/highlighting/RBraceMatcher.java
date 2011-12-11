/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.highlighting;

import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import static com.r4intellij.lang.lexer.RTokenTypes.*;


/**
 * Braces matcher for R files. Referenced by the plugin.xml file.
 *
 * @author Holger Brandl
 */
public class RBraceMatcher implements PairedBraceMatcher {

    private static final BracePair[] PAIRS = new BracePair[]{
            new BracePair(LEFT_PAREN, RIGHT_PAREN, false),
            new BracePair(LEFT_SQUARE, RIGHT_SQUARE, false),
            new BracePair(EXPR_ARITH, _EXPR_ARITH, true),
            new BracePair(EXPR_CONDITIONAL, _EXPR_CONDITIONAL, false),
//            new BracePair(STRING_BEGIN, STRING_END, false),
            new BracePair(LEFT_CURLY, RIGHT_CURLY, true), //structural
    };

    public BracePair[] getPairs() {
        return PAIRS;
    }

    public boolean isPairedBracesAllowedBeforeType(@NotNull final IElementType lbraceType, @Nullable final IElementType tokenType) {
        return TokenType.WHITE_SPACE == tokenType
//                || comments.contains(tokenType)
//                || tokenType == SEMI
                || tokenType == COMMA
                || tokenType == RIGHT_PAREN
                || tokenType == RIGHT_SQUARE
                || tokenType == RIGHT_CURLY
                || null == tokenType;
    }

    public int getCodeConstructStart(final PsiFile file, int openingBraceOffset) {
        return openingBraceOffset;
    }
}
