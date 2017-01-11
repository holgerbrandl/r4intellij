package com.r4intellij.psi;

import com.intellij.psi.tree.IStubFileElementType;
import com.r4intellij.RLanguage;

public class RFileElementType extends IStubFileElementType {
  public RFileElementType() {
    super(RLanguage.getInstance());
  }

  @Override
  public int getStubVersion() {
    return 2;
  }
}
