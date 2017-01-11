package com.r4intellij.debugger.frame;

import com.r4intellij.debugger.RDebuggerUtils;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RUnexpectedExecutionResultTypeException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.data.RCommands.EXECUTE_AND_STEP_COMMAND;
import static com.r4intellij.debugger.executor.RExecutionResultType.*;
import static com.r4intellij.debugger.executor.RExecutorUtils.execute;

class RValueModifierImpl implements RValueModifier {

  @NotNull
  private final RExecutor myExecutor;

  @NotNull
  private final RFunctionDebuggerFactory myFactory;

  @NotNull
  private final ROutputReceiver myReceiver;

  @NotNull
  private final RValueModifierHandler myHandler;

  private final int myFrameNumber;

  public RValueModifierImpl(@NotNull final RExecutor executor,
                            @NotNull final RFunctionDebuggerFactory factory,
                            @NotNull final ROutputReceiver receiver,
                            @NotNull final RValueModifierHandler handler,
                            final int frameNumber) {
    myExecutor = executor;
    myFactory = factory;
    myReceiver = receiver;
    myHandler = handler;
    myFrameNumber = frameNumber;
  }

  @Override
  public boolean isEnabled() {
    return myHandler.isModificationAvailable(myFrameNumber);
  }

  @Override
  public void setValue(@NotNull final String name, @NotNull final String value, @NotNull final Listener listener) {
    if (!isEnabled()) {
      throw new IllegalStateException("SetValue could be called only if isEnabled returns true");
    }

    try {
      doSetValue(name, value, listener);
    }
    catch (final RDebuggerException e) {
      listener.onError(e);
    }
  }

  private void doSetValue(@NotNull final String name, @NotNull final String value, @NotNull final Listener listener)
    throws RDebuggerException {
    final RExecutionResult result = execute(myExecutor, name + " <- " + value, myReceiver);

    switch (result.getType()) {
      case EMPTY:
        if (result.getError().isEmpty()) {
          listener.onSuccess();
        }
        else {
          listener.onError(result.getError());
        }

        return;
      case DEBUGGING_IN:
        RDebuggerUtils.forciblyEvaluateFunction(myExecutor, myFactory, myReceiver);

        listener.onSuccess();

        return;
      case DEBUG_AT:
        execute(myExecutor, EXECUTE_AND_STEP_COMMAND, RESPONSE, myReceiver);

        listener.onSuccess();

        return;
      default:
        throw new RUnexpectedExecutionResultTypeException(
          "Actual type is not the same as expected: " +
          "[" +
          "actual: " + result.getType() + ", " +
          "expected: " +
          "[" + DEBUGGING_IN + ", " + EMPTY + ", " + DEBUG_AT + "]" +
          "]"
        );
    }
  }
}
