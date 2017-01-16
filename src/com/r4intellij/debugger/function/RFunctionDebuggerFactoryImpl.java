package com.r4intellij.debugger.function;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RUnexpectedExecutionResultTypeException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.RDebuggerStringUtils.findCurrentLineEnd;
import static com.r4intellij.debugger.RDebuggerStringUtils.findNextLineBegin;
import static com.r4intellij.debugger.data.RCommands.EXECUTE_AND_STEP_COMMAND;
import static com.r4intellij.debugger.executor.RExecutionResultType.START_TRACE_BRACE;
import static com.r4intellij.debugger.executor.RExecutionResultType.START_TRACE_UNBRACE;
import static com.r4intellij.debugger.executor.RExecutorUtils.execute;

public class RFunctionDebuggerFactoryImpl implements RFunctionDebuggerFactory {

    @NotNull
    @Override
    public RFunctionDebugger getFunctionDebugger(@NotNull final RExecutor executor,
                                                 @NotNull final RFunctionDebuggerHandler debuggerHandler,
                                                 @NotNull final ROutputReceiver outputReceiver)
            throws RDebuggerException {
        execute(executor, EXECUTE_AND_STEP_COMMAND, RExecutionResultType.DEBUG_AT, outputReceiver);

        final RExecutionResult startTraceResult = execute(executor, EXECUTE_AND_STEP_COMMAND, outputReceiver);

        switch (startTraceResult.getType()) {
            case START_TRACE_BRACE:
                return new RBraceFunctionDebugger(
                        executor,
                        this,
                        debuggerHandler,
                        outputReceiver,
                        extractFunctionName(startTraceResult.getOutput())
                );

            case START_TRACE_UNBRACE:
                return new RUnbraceFunctionDebugger(
                        executor,
                        this,
                        debuggerHandler,
                        outputReceiver,
                        extractFunctionName(startTraceResult.getOutput())
                );
            default:
                throw new RUnexpectedExecutionResultTypeException(
                        "Actual type is not the same as expected: " +
                                "[" +
                                "actual: " + startTraceResult.getType() + ", " +
                                "expected: " +
                                "[" + START_TRACE_BRACE + ", " + START_TRACE_UNBRACE + "]" +
                                "]"
                );
        }
    }


    @NotNull
    private static String extractFunctionName(@NotNull final String startTraceText) {
        final int secondLineBegin = findNextLineBegin(startTraceText, 0);
        final int secondLineEnd = findCurrentLineEnd(startTraceText, secondLineBegin);

        return startTraceText.substring(
                secondLineBegin + "[1] \"".length(),
                secondLineEnd - "\"".length()
        );
    }
}
