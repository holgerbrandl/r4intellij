/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RExpr;
import com.r4intellij.psi.RFormlist;
import org.jetbrains.annotations.NotNull;

import java.util.List;


public class RFormlistImpl extends RCompositeElementImpl implements RFormlist {

    public RFormlistImpl(ASTNode node) {
        super(node);
    }

    @Override
    @NotNull
    public List<RExpr> getExprList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RExpr.class);
    }

}
