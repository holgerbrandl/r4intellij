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


public class RSublistImpl extends RCompositeElementImpl implements RSublist {

    public RSublistImpl(ASTNode node) {
        super(node);
    }

    @Override
    @Nullable
    public RCr getCr() {
        return PsiTreeUtil.getChildOfType(this, RCr.class);
    }

    @Override
    @NotNull
    public RSub getSub() {
        return PsiTreeUtil.getChildOfType(this, RSub.class);
    }

}
