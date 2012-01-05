/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RExprOrAssign;
import com.r4intellij.psi.RProg;
import org.jetbrains.annotations.Nullable;


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
