/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.highlighting;

import com.intellij.lang.CodeDocumentationAwareCommenter;
import com.intellij.psi.PsiComment;
import com.intellij.psi.tree.IElementType;
import com.r4intellij.psi.RTypes;
import org.jetbrains.annotations.Nullable;


/**
 * Comment handler for the R language.
 *
 * @author Holger Brandl
 */
public class RCommenter implements CodeDocumentationAwareCommenter {

    public String getLineCommentPrefix() {
        return "#";
    }


    public String getBlockCommentPrefix() {
        return null;
    }


    public String getBlockCommentSuffix() {
        return null;
    }


    public String getCommentedBlockCommentPrefix() {
        return null;
    }


    public String getCommentedBlockCommentSuffix() {
        return null;
    }


    @Nullable
    public IElementType getLineCommentTokenType() {
        return RTypes.R_COMMENT;
    }


    @Nullable
    public IElementType getBlockCommentTokenType() {
        return null;
    }


    @Nullable
    public IElementType getDocumentationCommentTokenType() {
        return null;
    }


    @Nullable
    public String getDocumentationCommentPrefix() {
        return null;
    }


    @Nullable
    public String getDocumentationCommentLinePrefix() {
        return null;
    }


    @Nullable
    public String getDocumentationCommentSuffix() {
        return null;
    }


    public boolean isDocumentationComment(PsiComment element) {
        return false;
    }
}
