package com.r4intellij.debugger.evaluator;

import org.jetbrains.annotations.NotNull;

public interface RExpressionHandler {

  @NotNull
  String handle(final int frameNumber, @NotNull final String expression);

  void setLastFrameNumber(final int lastFrameNumber);
}
