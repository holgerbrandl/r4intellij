

package com.r4intellij.packages;

import org.jetbrains.annotations.NotNull;

import java.io.Serializable;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class PckgFunction implements Serializable {

    private static final long serialVersionUID = -61945361973531069L;

    private final String funName;

    private String description;
    private String funSignature;
    private String shortDesc;


    public PckgFunction(@NotNull String funName) {
        this.funName = funName;
    }


    @Deprecated
    public PckgFunction(@NotNull String funName, @NotNull String funSignature) {
        this.funName = funName;
        this.funSignature = funSignature;
    }


    public String getName() {
        return funName;
    }


    public String getFunSignature() {
        return funSignature != null ? funSignature.replace("function", getName()) : "";
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

        PckgFunction function = (PckgFunction) o;

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
//            return getFunSignature().split("\\) \n\\{")[0].replace("function", getName()) + ")";
//
//        Matcher matcher = Pattern.compile("(.*)\\{", Pattern.DOTALL).matcher(getFunSignature());
//        if (matcher.find())
//            return matcher.group(1);
//
//
//        return getFunSignature();
//    }

}
