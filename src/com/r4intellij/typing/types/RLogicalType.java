package com.r4intellij.typing.types;

public class RLogicalType extends RIntegerType {
  public static RType INSTANCE = new RLogicalType();

  @Override
  public String getCanonicalName() {
    return "logical";
  }
}
