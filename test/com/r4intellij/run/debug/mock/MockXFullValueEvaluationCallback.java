package com.r4intellij.run.debug.mock;

import com.intellij.xdebugger.frame.XFullValueEvaluator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.awt.*;

import static org.junit.Assert.assertEquals;

public class MockXFullValueEvaluationCallback implements XFullValueEvaluator.XFullValueEvaluationCallback {

  @NotNull
  private final String myExpected;

  private int myCounter = 0;

  public MockXFullValueEvaluationCallback(@NotNull final String expected) {
    myExpected = expected;
  }

  @Override
  public void evaluated(@NotNull final String fullValue) {
    myCounter++;

    assertEquals(myExpected, fullValue);
  }

  @Override
  public void evaluated(@NotNull final String fullValue, @Nullable final Font font) {
    throw new IllegalStateException("Evaluated shouldn't be called");
  }

  @Override
  public boolean isObsolete() {
    throw new IllegalStateException("IsObsolete shouldn't be called");
  }

  @Override
  public void errorOccurred(@NotNull final String errorMessage) {
    throw new IllegalStateException("ErrorOccurred shouldn't be called");
  }

  public int getCounter() {
    return myCounter;
  }
}
