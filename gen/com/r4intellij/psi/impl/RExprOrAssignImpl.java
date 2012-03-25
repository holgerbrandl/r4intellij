/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RExpr;
import com.r4intellij.psi.RExprOrAssign;
import org.jetbrains.annotations.Nullable;


public class RExprOrAssignImpl extends RCompositeElementImpl implements RExprOrAssign {

    public RExprOrAssignImpl(ASTNode node) {
        super(node);
    }


    @Override
    @Nullable
    public RExpr getExpr() {
        return findChildByClass(RExpr.class);
    }


    @Override
    @Nullable
    public RExprOrAssign getExprOrAssign() {
        return findChildByClass(RExprOrAssign.class);
    }

}
