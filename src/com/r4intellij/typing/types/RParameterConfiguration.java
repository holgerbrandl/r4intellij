package com.r4intellij.typing.types;

import com.r4intellij.psi.api.RExpression;

public class RParameterConfiguration {

    private RType myType;
    private RExpression myValue;


    public RParameterConfiguration(RType type, RExpression value) {
        myType = type;
        myValue = value;
    }


    public RType getType() {
        return myType;
    }


    public RExpression getValue() {
        return myValue;
    }
}
