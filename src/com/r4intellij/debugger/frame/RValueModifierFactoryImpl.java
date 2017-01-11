package com.r4intellij.debugger.frame;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import org.jetbrains.annotations.NotNull;

public class RValueModifierFactoryImpl implements RValueModifierFactory {

  @NotNull
  @Override
  public RValueModifier getModifier(@NotNull final RExecutor executor,
                                    @NotNull final RFunctionDebuggerFactory factory,
                                    @NotNull final ROutputReceiver receiver,
                                    @NotNull final RValueModifierHandler handler,
                                    final int frameNumber) {
    return new RValueModifierImpl(executor, factory, receiver, handler, frameNumber);
  }
}
