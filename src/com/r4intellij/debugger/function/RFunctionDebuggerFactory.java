package com.r4intellij.debugger.function;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;

public interface RFunctionDebuggerFactory {

    @NotNull
    RFunctionDebugger getFunctionDebugger(@NotNull final RExecutor executor,
                                          @NotNull final RFunctionDebuggerHandler debuggerHandler,
                                          @NotNull final ROutputReceiver outputReceiver) throws RDebuggerException;
}
