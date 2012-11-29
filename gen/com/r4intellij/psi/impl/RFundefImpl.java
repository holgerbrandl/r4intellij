/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RExprOrAssign;
import com.r4intellij.psi.RFundef;
import com.r4intellij.psi.RFundefArgs;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;


public class RFundefImpl extends RCompositeElementImpl implements RFundef {

    public RFundefImpl(ASTNode node) {
        super(node);
    }


    @Override
    @NotNull
    public RExprOrAssign getExprOrAssign() {
        return findNotNullChildByClass(RExprOrAssign.class);
    }


    @Override
    @Nullable
    public RFundefArgs getFundefArgs() {
        return findChildByClass(RFundefArgs.class);
    }

}
