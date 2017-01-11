package com.r4intellij.typing.types;

public class RIntegerType extends RNumericType {
  public static RIntegerType INSTANCE = new RIntegerType();

  @Override
  public String getCanonicalName() {
    return "integer";
  }
}
