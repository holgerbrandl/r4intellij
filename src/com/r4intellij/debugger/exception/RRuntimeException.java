package com.r4intellij.debugger.exception;

import org.jetbrains.annotations.NotNull;

public class RRuntimeException extends RDebuggerException {

    public RRuntimeException(@NotNull final String message) {
        super(message);
    }
}
