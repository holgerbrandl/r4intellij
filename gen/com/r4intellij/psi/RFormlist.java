/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi;

import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nullable;


public interface RFormlist extends RCompositeElement {

    @Nullable
    public RExpr getExpr();

    @Nullable
    public PsiElement getWhitespace();

}
