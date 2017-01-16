package com.r4intellij.debugger.data;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class RLocation {

    @NotNull
    private final String myFunctionName;

    private final int myLine;


    public RLocation(@NotNull final String functionName, final int line) {
        myFunctionName = functionName;
        myLine = line;
    }


    @NotNull
    public String getFunctionName() {
        return myFunctionName;
    }


    public int getLine() {
        return myLine;
    }


    @Override
    public boolean equals(@Nullable final Object o) {
        if (o == this) return true;
        if (o == null || getClass() != o.getClass()) return false;

        final RLocation location = (RLocation) o;

        return myLine == location.myLine && myFunctionName.equals(location.myFunctionName);
    }


    @Override
    public int hashCode() {
        return 31 * myFunctionName.hashCode() + myLine;
    }
}
