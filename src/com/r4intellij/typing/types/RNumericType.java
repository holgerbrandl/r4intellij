package com.r4intellij.typing.types;

public class RNumericType extends RComplexType {
    public static RNumericType INSTANCE = new RNumericType();


    @Override
    public String getCanonicalName() {
        return "numeric";
    }
}
