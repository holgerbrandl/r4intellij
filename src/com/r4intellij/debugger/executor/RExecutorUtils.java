package com.r4intellij.debugger.executor;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RUnexpectedExecutionResultTypeException;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.RDebuggerStringUtils.appendError;

public final class RExecutorUtils {

  @NotNull
  public static RExecutionResult execute(@NotNull final RExecutor executor,
                                         @NotNull final String command,
                                         @NotNull final RExecutionResultType expectedType) throws RDebuggerException {
    final RExecutionResult result = executor.execute(command);

    if (result.getType() != expectedType) {
      throw new RUnexpectedExecutionResultTypeException(
        "Actual type is not the same as expected: [actual: " + result.getType() + ", expected: " + expectedType + "]"
      );
    }

    return result;
  }

  @NotNull
  public static String execute(@NotNull final RExecutor executor,
                               @NotNull final String command,
                               @NotNull final RExecutionResultType expectedType,
                               @NotNull final ROutputReceiver receiver) throws RDebuggerException {
    final RExecutionResult result = execute(executor, command, expectedType);

    appendError(result, receiver);

    return result.getOutput();
  }

  @NotNull
  public static RExecutionResult execute(@NotNull final RExecutor executor,
                                         @NotNull final String command,
                                         @NotNull final ROutputReceiver receiver) throws RDebuggerException {
    final RExecutionResult result = executor.execute(command);

    appendError(result, receiver);

    return result;
  }
}
