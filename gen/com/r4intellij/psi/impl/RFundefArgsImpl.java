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
import com.r4intellij.psi.RFdArgument;
import com.r4intellij.psi.RFundefArgs;
import com.r4intellij.psi.RVisitor;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class RFundefArgsImpl extends RCompositeElementImpl implements RFundefArgs {

    public RFundefArgsImpl(ASTNode node) {
        super(node);
    }


    public void accept(@NotNull RVisitor visitor) {
        visitor.visitFundefArgs(this);
    }


    public void accept(@NotNull PsiElementVisitor visitor) {
        if (visitor instanceof RVisitor) accept((RVisitor) visitor);
        else super.accept(visitor);
    }


    @Override
    @NotNull
    public List<RFdArgument> getFdArgumentList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RFdArgument.class);
    }

}
