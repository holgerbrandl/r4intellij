package com.r4intellij.typing.types;

public class RRawType extends RAtomicType {
  public static RRawType INSTANCE = new RRawType();

  @Override
  public String getCanonicalName() {
    return "raw";
  }
}
