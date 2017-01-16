package com.r4intellij.typing.types;

public class RUnknownType extends RType {
    public static final RUnknownType INSTANCE = new RUnknownType();


    @Override
    public String getCanonicalName() {
        return "Unknown";
    }


    @Override
    public RType getSlotType(String tag) {
        return RUnknownType.INSTANCE;
    }
}
