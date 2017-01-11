package com.r4intellij.run.debug.resolve;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

class RFunctionDescriptor {

  @NotNull
  private final String myName;

  @Nullable
  private final RFunctionDescriptor myParent;

  @NotNull
  private final Map<String, List<RFunctionDescriptor>> myChildren;

  private final int myStartLine;
  private final int myEndLine;

  public RFunctionDescriptor(@NotNull final String name,
                             @Nullable final RFunctionDescriptor parent,
                             final int startLine,
                             final int endLine) {
    myName = name;
    myParent = parent;
    myChildren = new HashMap<String, List<RFunctionDescriptor>>();
    myStartLine = startLine;
    myEndLine = endLine;
  }

  @NotNull
  public String getName() {
    return myName;
  }

  @Nullable
  public RFunctionDescriptor getParent() {
    return myParent;
  }

  @NotNull
  public Map<String, List<RFunctionDescriptor>> getChildren() {
    return myChildren;
  }

  public int getStartLine() {
    return myStartLine;
  }

  public int getEndLine() {
    return myEndLine;
  }
}
