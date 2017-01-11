package com.r4intellij.typing.types;

public class RCharacterType extends RAtomicType {
  public static RCharacterType INSTANCE = new RCharacterType();

  @Override
  public String getCanonicalName() {
    return "character";
  }
}
