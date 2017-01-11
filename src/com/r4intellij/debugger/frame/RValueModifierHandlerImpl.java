package com.r4intellij.debugger.frame;

public class RValueModifierHandlerImpl implements RValueModifierHandler {

  private int myLastFrameNumber = 0;

  @Override
  public boolean isModificationAvailable(final int frameNumber) {
    return myLastFrameNumber == frameNumber;
  }

  @Override
  public void setLastFrameNumber(final int lastFrameNumber) {
    myLastFrameNumber = lastFrameNumber;
  }
}
