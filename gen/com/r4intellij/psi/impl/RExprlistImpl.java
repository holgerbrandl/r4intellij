/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RExprOrAssign;
import com.r4intellij.psi.RExprlist;
import org.jetbrains.annotations.Nullable;

import static com.r4intellij.psi.RTypes.R_WHITESPACE;


public class RExprlistImpl extends RCompositeElementImpl implements RExprlist {

    public RExprlistImpl(ASTNode node) {
        super(node);
    }

    @Override
    @Nullable
    public RExprOrAssign getExprOrAssign() {
        return PsiTreeUtil.getChildOfType(this, RExprOrAssign.class);
    }

    @Override
    @Nullable
    public PsiElement getWhitespace() {
        ASTNode child = getNode().findChildByType(R_WHITESPACE);
        return child == null ? null : child.getPsi();
    }

}
