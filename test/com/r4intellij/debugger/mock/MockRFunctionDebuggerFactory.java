package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.function.RFunctionDebugger;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import com.r4intellij.debugger.function.RFunctionDebuggerHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class MockRFunctionDebuggerFactory implements RFunctionDebuggerFactory {

    @Nullable
    private final MockRFunctionDebugger myDebugger;

    private int myCounter;


    public MockRFunctionDebuggerFactory(@Nullable final MockRFunctionDebugger debugger) {
        myDebugger = debugger;
        myCounter = 0;
    }


    @NotNull
    @Override
    public RFunctionDebugger getFunctionDebugger(@NotNull final RExecutor executor,
                                                 @NotNull final RFunctionDebuggerHandler debuggerHandler,
                                                 @NotNull final ROutputReceiver outputReceiver) throws RDebuggerException {
        if (myDebugger == null) {
            throw new IllegalStateException("GetFunctionDebugger shouldn't be called");
        }

        myCounter++;

        myDebugger.setHandler(debuggerHandler);

        return myDebugger;
    }


    public int getCounter() {
        return myCounter;
    }
}
