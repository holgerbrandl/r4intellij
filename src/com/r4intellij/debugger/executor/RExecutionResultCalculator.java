package com.r4intellij.debugger.executor;

import org.jetbrains.annotations.NotNull;

public interface RExecutionResultCalculator {

    boolean isComplete(@NotNull final CharSequence output);


    @NotNull
    RExecutionResult calculate(@NotNull final CharSequence output, @NotNull final String error);
}
