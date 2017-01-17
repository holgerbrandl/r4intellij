/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.packages;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class Function implements Serializable {

    private static final long serialVersionUID = -61945361973531069L;

    private final String funName;

    private String description;
    private String funSignature;
    private String shortDesc;


    public Function(@NotNull String funName, @NotNull String funSignature) {
        this.funName = funName;
        this.funSignature = funSignature;
    }


    public String getFunName() {
        return funName;
    }


    public String getFunSignature() {
        return funSignature != null ? funSignature.replace("function", getFunName()) : "";
    }


    public void setShortDesc(String fundDesc) {
        this.shortDesc = fundDesc;
    }


    public String getShortDesc() {
        return shortDesc;
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

//    public String getBasicFunSignature() {
//        if (getFunSignature().contains(") \n{"))
//            return getFunSignature().split("\\) \n\\{")[0].replace("function", getFunName()) + ")";
//
//        Matcher matcher = Pattern.compile("(.*)\\{", Pattern.DOTALL).matcher(getFunSignature());
//        if (matcher.find())
//            return matcher.group(1);
//
//
//        return getFunSignature();
//    }

}
