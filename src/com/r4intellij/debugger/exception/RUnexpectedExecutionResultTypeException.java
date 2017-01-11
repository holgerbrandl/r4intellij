package com.r4intellij.debugger.exception;

import org.jetbrains.annotations.NotNull;

public class RUnexpectedExecutionResultTypeException extends RDebuggerException {

  public RUnexpectedExecutionResultTypeException(@NotNull final String message) {
    super(message);
  }
}
