/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.r4intellij.psi.RCommand;
import com.r4intellij.psi.RExprOrAssign;
import com.r4intellij.psi.RSection;
import org.jetbrains.annotations.Nullable;


public class RCommandImpl extends RCompositeElementImpl implements RCommand {

    public RCommandImpl(ASTNode node) {
        super(node);
    }


    @Override
    @Nullable
    public RExprOrAssign getExprOrAssign() {
        return findChildByClass(RExprOrAssign.class);
    }


    @Override
    @Nullable
    public RSection getSection() {
        return findChildByClass(RSection.class);
    }

}
