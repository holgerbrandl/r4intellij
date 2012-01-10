/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RExprOrAssign;
import com.r4intellij.psi.RFormlist;
import com.r4intellij.psi.RFundef;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;


public class RFundefImpl extends RCompositeElementImpl implements RFundef {

    public RFundefImpl(ASTNode node) {
        super(node);
    }

    @Override
    @NotNull
    public RExprOrAssign getExprOrAssign() {
        return PsiTreeUtil.getChildOfType(this, RExprOrAssign.class);
    }

    @Override
    @Nullable
    public RFormlist getFormlist() {
        return PsiTreeUtil.getChildOfType(this, RFormlist.class);
    }

}
