/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RFuncall;
import com.r4intellij.psi.RSublist;
import com.r4intellij.psi.RVariable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;


public class RFuncallImpl extends AbstractRFunCall implements RFuncall {

    public RFuncallImpl(ASTNode node) {
        super(node);
    }

    @Override
    @Nullable
    public RSublist getSublist() {
        return PsiTreeUtil.getChildOfType(this, RSublist.class);
    }

    @Override
    @NotNull
    public RVariable getVariable() {
        return PsiTreeUtil.getChildOfType(this, RVariable.class);
    }

}
