/*
 * Copyright 2012 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElementVisitor;
import com.r4intellij.psi.RSection;
import com.r4intellij.psi.RVisitor;
import org.jetbrains.annotations.NotNull;

public class RSectionImpl extends AbstractRSection implements RSection {

    public RSectionImpl(ASTNode node) {
        super(node);
    }


    public void accept(@NotNull PsiElementVisitor visitor) {
        if (visitor instanceof RVisitor) ((RVisitor) visitor).visitSection(this);
        else super.accept(visitor);
    }

}
