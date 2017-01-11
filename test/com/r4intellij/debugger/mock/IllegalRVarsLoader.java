package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.frame.RVar;
import com.r4intellij.debugger.frame.RVarsLoader;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class IllegalRVarsLoader implements RVarsLoader {

  @NotNull
  @Override
  public List<RVar> load() throws RDebuggerException {
    throw new IllegalStateException("Load shouldn't be called");
  }
}
