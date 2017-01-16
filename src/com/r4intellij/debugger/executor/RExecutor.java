package com.r4intellij.debugger.executor;

import com.r4intellij.debugger.exception.RDebuggerException;
import org.jetbrains.annotations.NotNull;

public interface RExecutor {

    @NotNull
    RExecutionResult execute(@NotNull final String command) throws RDebuggerException;
}
