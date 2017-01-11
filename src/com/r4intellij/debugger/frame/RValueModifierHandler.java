package com.r4intellij.debugger.frame;

public interface RValueModifierHandler {

  boolean isModificationAvailable(final int frameNumber);

  void setLastFrameNumber(final int lastFrameNumber);
}
