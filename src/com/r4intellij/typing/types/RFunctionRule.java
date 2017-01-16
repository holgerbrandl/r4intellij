package com.r4intellij.typing.types;

import com.r4intellij.psi.api.RExpression;

import java.util.HashMap;
import java.util.Map;

public class RFunctionRule {

    private RType myReturnType;
    private Map<String, RParameterConfiguration> myParameters = new HashMap<String, RParameterConfiguration>();


    public RFunctionRule(RType returnType) {
        myReturnType = returnType;
    }


    public Map<String, RParameterConfiguration> getParameters() {
        return myParameters;
    }


    public void addParameter(String name, RType type, RExpression value) {
        myParameters.put(name, new RParameterConfiguration(type, value));
    }


    public RType getReturnType() {
        return myReturnType;
    }
}
