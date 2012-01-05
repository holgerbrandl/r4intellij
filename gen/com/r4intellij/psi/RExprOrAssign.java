/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi;

import java.util.List;

import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;


public interface RExprOrAssign extends RCompositeElement {

    @Nullable
    public REqualAssign getEqualAssign();

    @Nullable
    public RExpr getExpr();

}
