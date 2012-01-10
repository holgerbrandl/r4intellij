/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.REqualAssign;
import com.r4intellij.psi.RExpr;
import com.r4intellij.psi.RExprOrAssign;
import org.jetbrains.annotations.Nullable;


public class RExprOrAssignImpl extends RCompositeElementImpl implements RExprOrAssign {

    public RExprOrAssignImpl(ASTNode node) {
        super(node);
    }

    @Override
    @Nullable
    public REqualAssign getEqualAssign() {
        return PsiTreeUtil.getChildOfType(this, REqualAssign.class);
    }

    @Override
    @Nullable
    public RExpr getExpr() {
        return PsiTreeUtil.getChildOfType(this, RExpr.class);
    }

}
