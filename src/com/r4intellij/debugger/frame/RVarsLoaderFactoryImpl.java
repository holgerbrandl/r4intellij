package com.r4intellij.debugger.frame;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;

public class RVarsLoaderFactoryImpl implements RVarsLoaderFactory {

  @NotNull
  private final RExecutor myExecutor;

  @NotNull
  private final ROutputReceiver myReceiver;

  public RVarsLoaderFactoryImpl(@NotNull final RExecutor executor, @NotNull final ROutputReceiver receiver) {
    myExecutor = executor;
    myReceiver = receiver;
  }

  @NotNull
  @Override
  public RVarsLoader getLoader(@NotNull final RValueModifier modifier,
                               final int frameNumber) {
    return new RVarsLoaderImpl(myExecutor, myReceiver, modifier, frameNumber);
  }
}
