package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.function.RFunctionDebugger;
import com.r4intellij.debugger.function.RFunctionDebuggerHandler;
import org.jetbrains.annotations.NotNull;

public class IllegalRFunctionDebuggerHandler implements RFunctionDebuggerHandler {

  @Override
  public void appendDebugger(@NotNull final RFunctionDebugger debugger) {
    throw new IllegalStateException("AppendDebugger shouldn't be called");
  }

  @Override
  public void setReturnLineNumber(final int lineNumber) {
    throw new IllegalStateException("SetReturnLineNumber shouldn't be called");
  }

  @Override
  public void setDropFrames(final int number) {
    throw new IllegalStateException("SetDropFrames shouldn't be called");
  }
}
