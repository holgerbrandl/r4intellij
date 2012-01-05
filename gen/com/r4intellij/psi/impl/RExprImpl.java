/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import java.util.List;

import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;

import static com.r4intellij.psi.RTypes.*;

import com.r4intellij.psi.*;


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
    @Nullable
    public RCr getCr() {
        return PsiTreeUtil.getChildOfType(this, RCr.class);
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
    public RFormlist getFormlist() {
        return PsiTreeUtil.getChildOfType(this, RFormlist.class);
    }

    @Override
    @Nullable
    public RIfcond getIfcond() {
        return PsiTreeUtil.getChildOfType(this, RIfcond.class);
    }

    @Override
    @Nullable
    public RSublist getSublist() {
        return PsiTreeUtil.getChildOfType(this, RSublist.class);
    }

}
