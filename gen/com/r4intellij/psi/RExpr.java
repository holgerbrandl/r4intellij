/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */
package com.r4intellij.psi;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;


public interface RExpr extends RCompositeElement {

    @Nullable
    public RCond getCond();

    @NotNull
    public List<RExpr> getExprList();

    @Nullable
    public RExprOrAssign getExprOrAssign();

    @Nullable
    public RExprlist getExprlist();

    @Nullable
    public RForcond getForcond();

    @Nullable
    public RFundef getFundef();

    @Nullable
    public RIfcond getIfcond();

    @NotNull
    public List<RSublist> getSublistList();

    @NotNull
    public List<RVariable> getVariableList();

}
