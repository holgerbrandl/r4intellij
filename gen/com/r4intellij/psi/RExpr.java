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


public interface RExpr extends RCompositeElement {

    @Nullable
    public RCond getCond();

    @Nullable
    public RCr getCr();

    @Nullable
    public RExprOrAssign getExprOrAssign();

    @Nullable
    public RExprlist getExprlist();

    @Nullable
    public RForcond getForcond();

    @Nullable
    public RFormlist getFormlist();

    @Nullable
    public RIfcond getIfcond();

    @Nullable
    public RSublist getSublist();

}
