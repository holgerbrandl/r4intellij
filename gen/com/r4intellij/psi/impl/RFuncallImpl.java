/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.r4intellij.psi.RFuncall;
import com.r4intellij.psi.RSublist;
import com.r4intellij.psi.RVariable;
import com.r4intellij.psi.RVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;


public class RFuncallImpl extends AbstractRFunCall implements RFuncall {

    public RFuncallImpl(ASTNode node) {
        super(node);
    }


    public void accept(@NotNull PsiElementVisitor visitor) {
        if (visitor instanceof RVisitor) ((RVisitor) visitor).visitFuncall(this);
        else super.accept(visitor);
    }


    @Override
    @Nullable
    public RSublist getSublist() {
        return findChildByClass(RSublist.class);
    }


    @Override
    @NotNull
    public RVariable getVariable() {
        return findNotNullChildByClass(RVariable.class);
    }

}
