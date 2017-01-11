package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.evaluator.RExpressionHandler;
import org.jetbrains.annotations.NotNull;

public class IllegalRExpressionHandler implements RExpressionHandler {

  @NotNull
  @Override
  public String handle(final int frameNumber, @NotNull final String expression) {
    throw new IllegalStateException("Handle shouldn't be called");
  }

  @Override
  public void setLastFrameNumber(final int lastFrameNumber) {
    throw new IllegalStateException("SetMaxFrameNumber shouldn't be called");
  }
}
