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
import com.r4intellij.psi.RIfcond;
import org.jetbrains.annotations.NotNull;


public class RIfcondImpl extends RCompositeElementImpl implements RIfcond {

    public RIfcondImpl(ASTNode node) {
        super(node);
    }

    @Override
    @NotNull
    public RExpr getExpr() {
        return PsiTreeUtil.getChildOfType(this, RExpr.class);
    }

}
