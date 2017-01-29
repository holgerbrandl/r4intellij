package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.function.RTraceAndDebugUtilsTest.LS_FUNCTIONS_COMMAND;
import static com.r4intellij.debugger.function.RTraceAndDebugUtilsTest.NO_FUNCTIONS_RESULT;

public abstract class MockRExecutor implements RExecutor {

    @NotNull
    public static final String LS_FUNCTIONS_ERROR = "error_ls";

    private int myCounter = 0;


    @NotNull
    @Override
    public RExecutionResult execute(@NotNull final String command) throws RDebuggerException {
        myCounter++;

        if (useNoFunctionsResult() && command.equals(LS_FUNCTIONS_COMMAND)) {
            return NO_FUNCTIONS_RESULT;
        }

        return doExecute(command);
    }


    public int getCounter() {
        return myCounter;
    }


    protected boolean useNoFunctionsResult() {
        return true;
    }


    @NotNull
    protected abstract RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException;
}
