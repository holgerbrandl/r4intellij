package com.r4intellij.debugger.function;

import com.r4intellij.debugger.exception.RDebuggerException;
import org.jetbrains.annotations.NotNull;

public interface RFunctionDebuggerHandler {

    void appendDebugger(@NotNull final RFunctionDebugger debugger) throws RDebuggerException;


    void setReturnLineNumber(final int lineNumber);


    void setDropFrames(final int number);
}
