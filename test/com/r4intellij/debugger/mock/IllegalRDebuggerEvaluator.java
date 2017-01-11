package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.evaluator.RDebuggerEvaluator;
import org.jetbrains.annotations.NotNull;

public class IllegalRDebuggerEvaluator implements RDebuggerEvaluator {

  @Override
  public void evaluate(@NotNull final String expression, @NotNull final Receiver receiver) {
    throw new IllegalStateException("EvalExpression shouldn't be called");
  }
}
