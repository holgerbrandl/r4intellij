package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.frame.RValueModifierHandler;

public class IllegalRValueModifierHandler implements RValueModifierHandler {

  @Override
  public boolean isModificationAvailable(final int frameNumber) {
    throw new IllegalStateException("IsModificationAvailable shouldn't be called");
  }

  @Override
  public void setLastFrameNumber(final int lastFrameNumber) {
    throw new IllegalStateException("SetLastFrameNumber shouldn't be called");
  }
}
