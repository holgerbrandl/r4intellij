package com.r4intellij.debugger.frame;

import com.r4intellij.debugger.exception.RDebuggerException;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public interface RVarsLoader {

    @NotNull
    List<RVar> load() throws RDebuggerException;
}
