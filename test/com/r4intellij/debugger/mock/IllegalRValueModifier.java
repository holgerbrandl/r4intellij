package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.frame.RValueModifier;
import org.jetbrains.annotations.NotNull;

public class IllegalRValueModifier implements RValueModifier {

  @Override
  public boolean isEnabled() {
    throw new IllegalStateException("IsEnabled shouldn't be called");
  }

  @Override
  public void setValue(@NotNull final String name, @NotNull final String value, @NotNull final Listener listener) {
    throw new IllegalStateException("SetValue shouldn't be called");
  }
}
