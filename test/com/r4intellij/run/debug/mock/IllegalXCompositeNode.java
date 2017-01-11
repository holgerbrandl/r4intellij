package com.r4intellij.run.debug.mock;

import com.intellij.ui.SimpleTextAttributes;
import com.intellij.xdebugger.frame.XCompositeNode;
import com.intellij.xdebugger.frame.XDebuggerTreeNodeHyperlink;
import com.intellij.xdebugger.frame.XValueChildrenList;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class IllegalXCompositeNode implements XCompositeNode {

  @Override
  public void addChildren(@NotNull final XValueChildrenList children, final boolean last) {
    throw new IllegalStateException("AddChildren shouldn't be called");
  }

  @Override
  public void tooManyChildren(final int remaining) {
    throw new IllegalStateException("TooManyChildren shouldn't be called");
  }

  @Override
  public void setAlreadySorted(final boolean alreadySorted) {
    throw new IllegalStateException("SetAlreadySorted shouldn't be called");
  }

  @Override
  public void setErrorMessage(@NotNull final String errorMessage) {
    throw new IllegalStateException("SetErrorMessage shouldn't be called");
  }

  @Override
  public void setErrorMessage(@NotNull final String errorMessage, final XDebuggerTreeNodeHyperlink link) {
    throw new IllegalStateException("SetErrorMessage shouldn't be called");
  }

  @Override
  public void setMessage(@NotNull final String message,
                         @Nullable final Icon icon,
                         @NotNull final SimpleTextAttributes attributes,
                         @Nullable final XDebuggerTreeNodeHyperlink link) {
    throw new IllegalStateException("SetMessage shouldn't be called");
  }

  @Override
  public boolean isObsolete() {
    throw new IllegalStateException("IsObsolete shouldn't be called");
  }
}
