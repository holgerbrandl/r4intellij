/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.r4intellij.psi.RAssignOp;
import com.r4intellij.psi.RExpr;
import com.r4intellij.psi.RExprOrAssign;
import com.r4intellij.psi.RVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class RExprOrAssignImpl extends RCompositeElementImpl implements RExprOrAssign {

    public RExprOrAssignImpl(ASTNode node) {
        super(node);
    }


    public void accept(@NotNull RVisitor visitor) {
        visitor.visitExprOrAssign(this);
    }


    public void accept(@NotNull PsiElementVisitor visitor) {
        if (visitor instanceof RVisitor) accept((RVisitor) visitor);
        else super.accept(visitor);
    }


    @Override
    @Nullable
    public RAssignOp getAssignOp() {
        return findChildByClass(RAssignOp.class);
    }


    @Override
    @NotNull
    public RExpr getExpr() {
        return findNotNullChildByClass(RExpr.class);
    }


    @Override
    @Nullable
    public RExprOrAssign getExprOrAssign() {
        return findChildByClass(RExprOrAssign.class);
    }

}
