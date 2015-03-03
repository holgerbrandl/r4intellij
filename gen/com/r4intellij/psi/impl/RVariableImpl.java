/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.r4intellij.psi.RVariable;
import com.r4intellij.psi.RVisitor;
import org.jetbrains.annotations.NotNull;

public class RVariableImpl extends RVarImpl implements RVariable {

    public RVariableImpl(ASTNode node) {
        super(node);
    }


    public void accept(@NotNull PsiElementVisitor visitor) {
        if (visitor instanceof RVisitor) ((RVisitor) visitor).visitVariable(this);
        else super.accept(visitor);
    }

}
