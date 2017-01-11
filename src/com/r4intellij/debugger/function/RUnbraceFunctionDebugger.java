package com.r4intellij.debugger.function;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;

class RUnbraceFunctionDebugger extends RFunctionDebuggerBase {

  public RUnbraceFunctionDebugger(@NotNull final RExecutor executor,
                                  @NotNull final RFunctionDebuggerFactory debuggerFactory,
                                  @NotNull final RFunctionDebuggerHandler debuggerHandler,
                                  @NotNull final ROutputReceiver outputReceiver,
                                  @NotNull final String functionName) throws RDebuggerException {
    super(executor, debuggerFactory, debuggerHandler, outputReceiver, functionName);
  }

  @Override
  protected void handleDebugAt(@NotNull final RExecutionResult result) throws RDebuggerException {
    handleDebugAt(result, true, false);
  }

  @Override
  protected int initCurrentLine() throws RDebuggerException {
    return 0;
  }

  @NotNull
  @Override
  protected RExecutionResultType getStartTraceType() {
    return RExecutionResultType.START_TRACE_UNBRACE;
  }
}
