/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.misc.rinstallcache;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class Function implements Serializable {

    private static final long serialVersionUID = -6194025361973531069L;

    private final String funName;
    private final String funDesc;
    private String funSignature;


    public Function(@NotNull String funName, @NotNull String funDesc) {
        this.funName = funName;
        this.funDesc = funDesc;
    }

    public String getFunName() {
        return funName;
    }

    public String getFunDesc() {
        return funDesc;
    }

    public void setFunSignature(String funSignature) {
        this.funSignature = funSignature;
    }

    public String getFunSignature() {
        return funSignature;
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

    @Override
    public String toString() {
        return funName;
    }

    public String getBasicFunSignature() {
        if (getFunSignature().contains(") \n{"))
            return getFunSignature().split("\\) \n\\{")[0].replace("function", getFunName()) + ")";

        Matcher matcher = Pattern.compile("(.*)\\{", Pattern.DOTALL).matcher(getFunSignature());
        if (matcher.find())
            return matcher.group(1);


        return getFunSignature();
    }
}
