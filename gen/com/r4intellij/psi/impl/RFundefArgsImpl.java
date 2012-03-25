/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.RFdArgument;
import com.r4intellij.psi.RFundefArgs;
import org.jetbrains.annotations.NotNull;

import java.util.List;


public class RFundefArgsImpl extends RCompositeElementImpl implements RFundefArgs {

    public RFundefArgsImpl(ASTNode node) {
        super(node);
    }


    @Override
    @NotNull
    public List<RFdArgument> getFdArgumentList() {
        return PsiTreeUtil.getChildrenOfTypeAsList(this, RFdArgument.class);
    }

}
