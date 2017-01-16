package com.r4intellij.run.debug.resolve;

import com.intellij.xdebugger.XSourcePosition;
import com.r4intellij.debugger.data.RLocation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public interface RResolvingSession {

    @Nullable
    XSourcePosition resolveNext(@NotNull final RLocation nextLocation);


    @Nullable
    XSourcePosition resolveCurrent(final int line);


    void dropLast(final int number);
}
