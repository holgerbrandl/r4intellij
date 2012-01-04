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

import static com.r4intellij.psi.RTypes.*;


/**
 * Braces matcher for R files. Referenced by the plugin.xml file.
 *
 * @author Holger Brandl
 */
public class RBraceMatcher implements PairedBraceMatcher {

    private static final BracePair[] PAIRS = new BracePair[]{
            new BracePair(R_LEFT_PAREN, R_RIGHT_PAREN, false),
            new BracePair(R_LEFT_BRACE, R_RIGHT_BRACE, false),
//            new BracePair(STRING_BEGIN, STRING_END, false),
            new BracePair(R_LEFT_BRACKET, R_RIGHT_BRACKET, true), //structural
    };

    public BracePair[] getPairs() {
        return PAIRS;
    }

    public boolean isPairedBracesAllowedBeforeType(@NotNull final IElementType lbraceType, @Nullable final IElementType tokenType) {
        return TokenType.WHITE_SPACE == tokenType
//                || comments.contains(tokenType)
//                || tokenType == SEMI
                || tokenType == R_COMMA
                || tokenType == R_RIGHT_PAREN
                || tokenType == R_RIGHT_BRACE
                || tokenType == R_RIGHT_BRACKET
                || null == tokenType;
    }

    public int getCodeConstructStart(final PsiFile file, int openingBraceOffset) {
        return openingBraceOffset;
    }
}
