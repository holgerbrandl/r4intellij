package com.r4intellij.debugger.evaluator;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import org.jetbrains.annotations.NotNull;

public class RDebuggerEvaluatorFactoryImpl implements RDebuggerEvaluatorFactory {

  @NotNull
  @Override
  public RDebuggerEvaluator getEvaluator(@NotNull final RExecutor executor,
                                         @NotNull final RFunctionDebuggerFactory factory,
                                         @NotNull final ROutputReceiver receiver,
                                         @NotNull final RExpressionHandler handler,
                                         final int frameNumber) {
    return new RDebuggerEvaluatorImpl(executor, factory, receiver, handler, frameNumber);
  }
}
