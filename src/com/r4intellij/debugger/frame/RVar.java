package com.r4intellij.debugger.frame;

import org.jetbrains.annotations.NotNull;

public class RVar {

  @NotNull
  private final String myName;

  @NotNull
  private final String myType;

  @NotNull
  private final String myValue;

  @NotNull
  private final RValueModifier myModifier;

  public RVar(@NotNull final String name,
              @NotNull final String type,
              @NotNull final String value,
              @NotNull final RValueModifier modifier) {
    myName = name;
    myType = type;
    myValue = value;
    myModifier = modifier;
  }

  @NotNull
  public String getName() {
    return myName;
  }

  @NotNull
  public String getType() {
    return myType;
  }

  @NotNull
  public String getValue() {
    return myValue;
  }

  @NotNull
  public RValueModifier getModifier() {
    return myModifier;
  }
}
