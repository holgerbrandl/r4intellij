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


public class RProgImpl extends RCompositeElementImpl implements RProg {

    public RProgImpl(ASTNode node) {
        super(node);
    }

    @Override
    @Nullable
    public RExprOrAssign getExprOrAssign() {
        return PsiTreeUtil.getChildOfType(this, RExprOrAssign.class);
    }

}
