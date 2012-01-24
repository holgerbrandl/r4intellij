/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RExpr;
import com.r4intellij.psi.RFdArgument;
import org.jetbrains.annotations.Nullable;


public class RFdArgumentImpl extends RCompositeElementImpl implements RFdArgument {

    public RFdArgumentImpl(ASTNode node) {
        super(node);
    }

    @Override
    @Nullable
    public RExpr getExpr() {
        return PsiTreeUtil.getChildOfType(this, RExpr.class);
    }

}
