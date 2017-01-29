package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.exception.RDebuggerException;
import org.jetbrains.annotations.NotNull;

public class IllegalRFunctionDebugger extends MockRFunctionDebugger {

    public IllegalRFunctionDebugger() {
        super("", 0, null);
    }


    @NotNull
    @Override
    public RLocation getLocation() {
        throw new IllegalStateException("GetLocation shouldn't be called");
    }


    @Override
    public boolean hasNext() {
        throw new IllegalStateException("HasNext shouldn't be called");
    }


    @Override
    public void advance() throws RDebuggerException {
        throw new IllegalStateException("Advance shouldn't be called");
    }
}
