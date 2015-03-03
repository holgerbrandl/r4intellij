/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.r4intellij.psi.RExpr;
import com.r4intellij.psi.RSub;
import com.r4intellij.psi.RVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class RSubImpl extends RCompositeElementImpl implements RSub {

    public RSubImpl(ASTNode node) {
        super(node);
    }


    public void accept(@NotNull PsiElementVisitor visitor) {
        if (visitor instanceof RVisitor) ((RVisitor) visitor).visitSub(this);
        else super.accept(visitor);
    }


    @Override
    @Nullable
    public RExpr getExpr() {
        return findChildByClass(RExpr.class);
    }

}
