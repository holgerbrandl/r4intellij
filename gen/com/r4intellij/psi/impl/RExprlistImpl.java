/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RExprOrAssign;
import com.r4intellij.psi.RExprlist;
import org.jetbrains.annotations.NotNull;

import java.util.List;


public class RExprlistImpl extends RCompositeElementImpl implements RExprlist {

    public RExprlistImpl(ASTNode node) {
        super(node);
    }

    @Override
    @NotNull
    public List<RExprOrAssign> getExprOrAssignList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RExprOrAssign.class);
    }

}
