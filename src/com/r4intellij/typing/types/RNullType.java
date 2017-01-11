package com.r4intellij.typing.types;

public class RNullType extends RAtomicType {
  public static RType INSTANCE = new RNullType();

  @Override
  public String getCanonicalName() {
    return "null";
  }

  @Override
  public RType getMemberType(String tag) {
    return INSTANCE;
  }
}
