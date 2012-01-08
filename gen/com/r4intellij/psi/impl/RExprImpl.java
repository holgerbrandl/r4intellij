/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;


public class RExprImpl extends RCompositeElementImpl implements RExpr {

    public RExprImpl(ASTNode node) {
        super(node);
    }

    @Override
    @Nullable
    public RCond getCond() {
        return PsiTreeUtil.getChildOfType(this, RCond.class);
    }

    @Override
    @NotNull
    public List<RExpr> getExprList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RExpr.class);
    }

    @Override
    @Nullable
    public RExprOrAssign getExprOrAssign() {
        return PsiTreeUtil.getChildOfType(this, RExprOrAssign.class);
    }

    @Override
    @Nullable
    public RExprlist getExprlist() {
        return PsiTreeUtil.getChildOfType(this, RExprlist.class);
    }

    @Override
    @Nullable
    public RForcond getForcond() {
        return PsiTreeUtil.getChildOfType(this, RForcond.class);
    }

    @Override
    @Nullable
    public RFundef getFundef() {
        return PsiTreeUtil.getChildOfType(this, RFundef.class);
    }

    @Override
    @Nullable
    public RIfcond getIfcond() {
        return PsiTreeUtil.getChildOfType(this, RIfcond.class);
    }

    @Override
    @NotNull
    public List<RSublist> getSublistList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RSublist.class);
    }

    @Override
    @NotNull
    public List<RVariable> getVariableList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RVariable.class);
    }

}
