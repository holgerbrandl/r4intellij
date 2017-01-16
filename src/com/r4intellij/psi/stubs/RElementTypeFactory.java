package com.r4intellij.psi.stubs;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

public class RElementTypeFactory {
  private RElementTypeFactory() {
  }

  public static IElementType getElementTypeByName(@NotNull String name) {
      if (name.equals("R_ASSIGNMENT_STATEMENT")) {
      return new RAssignmentElementType(name);
    }
    throw new IllegalArgumentException("Unknown element type: " + name);
  }
}
