/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RCond;
import com.r4intellij.psi.RExpr;
import org.jetbrains.annotations.NotNull;


public class RCondImpl extends RCompositeElementImpl implements RCond {

    public RCondImpl(ASTNode node) {
        super(node);
    }


    @Override
    @NotNull
    public RExpr getExpr() {
        return findNotNullChildByClass(RExpr.class);
    }

}
