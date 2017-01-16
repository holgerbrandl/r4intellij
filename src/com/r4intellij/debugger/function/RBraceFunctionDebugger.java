package com.r4intellij.debugger.function;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.executor.RExecutionResultType.START_TRACE_BRACE;

class RBraceFunctionDebugger extends RFunctionDebuggerBase {

    public RBraceFunctionDebugger(@NotNull final RExecutor executor,
                                  @NotNull final RFunctionDebuggerFactory debuggerFactory,
                                  @NotNull final RFunctionDebuggerHandler debuggerHandler,
                                  @NotNull final ROutputReceiver outputReceiver,
                                  @NotNull final String functionName) throws RDebuggerException {
        super(executor, debuggerFactory, debuggerHandler, outputReceiver, functionName);
    }


    @Override
    protected void handleDebugAt(@NotNull final RExecutionResult result) throws RDebuggerException {
        handleDebugAt(result, true, true);
    }


    @NotNull
    @Override
    protected RExecutionResultType getStartTraceType() {
        return START_TRACE_BRACE;
    }
}
