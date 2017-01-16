package com.r4intellij.typing.types;

import com.r4intellij.psi.api.RParameter;

public class RTypedParameter {
    String myName;
    RType myType;
    RParameter myParameter;
    boolean myOptional;


    public RTypedParameter(String name, RType type, RParameter parameter) {
        myName = name;
        myType = type;
        myParameter = parameter;
    }


    public String getName() {
        return myName;
    }


    public RType getType() {
        return myType;
    }


    public RParameter getParameter() {
        return myParameter;
    }


    public void setType(RType type) {
        myType = type;
    }


    public void setOptional(boolean optional) {
        myOptional = optional;
    }


    public boolean isOptional() {
        return myOptional;
    }
}
