/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RCr;
import com.r4intellij.psi.RSub;
import com.r4intellij.psi.RSublist;
import org.jetbrains.annotations.NotNull;

import java.util.List;


public class RSublistImpl extends RCompositeElementImpl implements RSublist {

    public RSublistImpl(ASTNode node) {
        super(node);
    }

    @Override
    @NotNull
    public List<RCr> getCrList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RCr.class);
    }

    @Override
    @NotNull
    public List<RSub> getSubList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RSub.class);
    }

}
