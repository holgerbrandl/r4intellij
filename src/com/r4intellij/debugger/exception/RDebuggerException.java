package com.r4intellij.debugger.exception;

import org.jetbrains.annotations.NotNull;

public class RDebuggerException extends Exception {

  public RDebuggerException(@NotNull final Exception cause) {
    super(cause);
  }

  public RDebuggerException(@NotNull final String message) {
    super(message);
  }
}
