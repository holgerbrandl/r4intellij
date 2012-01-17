/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.rinstallcache;

import org.jetbrains.annotations.NotNull;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class Function {

    private final String funName;
    private final String funDef;

    public Function(@NotNull String funName, @NotNull String funDef) {
        this.funName = funName;
        this.funDef = funDef;
    }

    public String getFunName() {
        return funName;
    }

    public String getFunDef() {
        return funDef;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Function function = (Function) o;

        return funName.equals(function.funName);
    }

    @Override
    public int hashCode() {
        return funName.hashCode();
    }
}
