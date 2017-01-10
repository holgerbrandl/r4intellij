/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RExprOrAssign;
import com.r4intellij.psi.RExprlist;
import com.r4intellij.psi.RVisitor;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class RExprlistImpl extends RCompositeElementImpl implements RExprlist {

    public RExprlistImpl(ASTNode node) {
        super(node);
    }


    public void accept(@NotNull RVisitor visitor) {
        visitor.visitExprlist(this);
    }


    public void accept(@NotNull PsiElementVisitor visitor) {
        if (visitor instanceof RVisitor) accept((RVisitor) visitor);
        else super.accept(visitor);
    }


    @Override
    @NotNull
    public List<RExprOrAssign> getExprOrAssignList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RExprOrAssign.class);
    }

}
