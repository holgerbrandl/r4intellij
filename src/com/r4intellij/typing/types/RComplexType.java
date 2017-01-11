package com.r4intellij.typing.types;

public class RComplexType extends RAtomicType {
  public static RComplexType INSTANCE = new RComplexType();

  @Override
  public String getCanonicalName() {
    return "complex";
  }
}
