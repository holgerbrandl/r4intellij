package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.function.RFunctionDebugger;
import com.r4intellij.debugger.function.RFunctionDebuggerHandler;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class MockRFunctionDebugger implements RFunctionDebugger {

    @NotNull
    private final String myFunctionName;

    private final int myLimit;

    @Nullable
    private final String myResult;

    private int myCounter = 0;

    @Nullable
    private RFunctionDebuggerHandler myHandler;


    public MockRFunctionDebugger(@NotNull final String functionName, final int limit, @Nullable final String result) {
        myFunctionName = functionName;
        myLimit = limit;
        myResult = result;
    }


    @NotNull
    @Override
    public RLocation getLocation() {
        return new RLocation(myFunctionName, myCounter);
    }


    @Override
    public boolean hasNext() {
        return myCounter < myLimit;
    }


    @Override
    public void advance() throws RDebuggerException {
        myCounter++;
    }


    @NotNull
    @Override
    public String getResult() {
        if (myResult == null) {
            throw new IllegalStateException("GetResult shouldn't be called");
        }

        return myResult;
    }


    @Nullable
    public RFunctionDebuggerHandler getHandler() {
        return myHandler;
    }


    public void setHandler(@Nullable final RFunctionDebuggerHandler handler) {
        myHandler = handler;
    }


    public int getCounter() {
        return myCounter;
    }
}
