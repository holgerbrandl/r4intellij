package com.r4intellij.debugger.mock;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import org.jetbrains.annotations.NotNull;

public class AlwaysSameResultRExecutor extends MockRExecutor {

    @NotNull
    private final String myText;

    @NotNull
    private final RExecutionResultType myType;

    @NotNull
    private final TextRange myOutputRange;

    @NotNull
    private final String myError;


    public AlwaysSameResultRExecutor(@NotNull final String text,
                                     @NotNull final RExecutionResultType type,
                                     @NotNull final TextRange outputRange,
                                     @NotNull final String error) {
        myText = text;
        myType = type;
        myOutputRange = outputRange;
        myError = error;
    }


    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
        return new RExecutionResult(myText, myType, myOutputRange, myError);
    }
}
