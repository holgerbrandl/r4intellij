package com.r4intellij.debugger.executor;

import com.intellij.openapi.util.TextRange;
import org.jetbrains.annotations.NotNull;

public class RExecutionResult {

    @NotNull
    private final String myOutput;

    @NotNull
    private final RExecutionResultType myType;

    @NotNull
    private final TextRange myResultRange;

    @NotNull
    private final String myError;


    public RExecutionResult(@NotNull final String output,
                            @NotNull final RExecutionResultType type,
                            @NotNull final TextRange resultRange,
                            @NotNull final String error) {
        myOutput = output;
        myType = type;
        myResultRange = resultRange;
        myError = error;
    }


    @NotNull
    public String getOutput() {
        return myOutput;
    }


    @NotNull
    public RExecutionResultType getType() {
        return myType;
    }


    @NotNull
    public TextRange getResultRange() {
        return myResultRange;
    }


    @NotNull
    public String getError() {
        return myError;
    }
}
