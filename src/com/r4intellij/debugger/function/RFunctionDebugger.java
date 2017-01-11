package com.r4intellij.debugger.function;

import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.exception.RDebuggerException;
import org.jetbrains.annotations.NotNull;

public interface RFunctionDebugger {

  @NotNull
  RLocation getLocation();

  boolean hasNext();

  void advance() throws RDebuggerException;

  @NotNull
  String getResult();
}
