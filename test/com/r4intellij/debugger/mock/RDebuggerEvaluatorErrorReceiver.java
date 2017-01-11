package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.evaluator.RDebuggerEvaluator;
import org.jetbrains.annotations.NotNull;

public class RDebuggerEvaluatorErrorReceiver implements RDebuggerEvaluator.Receiver {

  private int myCounter = 0;

  @Override
  public void receiveResult(@NotNull final String result) {
    throw new IllegalStateException("ReceiveResult shouldn't be called");
  }

  @Override
  public void receiveError(@NotNull final Exception e) {
    myCounter++;
  }

  @Override
  public void receiveError(@NotNull final String error) {
    myCounter++;
  }

  public int getCounter() {
    return myCounter;
  }
}
