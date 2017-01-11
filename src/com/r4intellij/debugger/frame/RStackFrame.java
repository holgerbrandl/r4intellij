package com.r4intellij.debugger.frame;

import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.evaluator.RDebuggerEvaluator;
import org.jetbrains.annotations.NotNull;

public class RStackFrame {

  @NotNull
  private final RLocation myLocation;

  @NotNull
  private final RVarsLoader myLoader;

  @NotNull
  private final RDebuggerEvaluator myEvaluator;

  public RStackFrame(@NotNull final RLocation location,
                     @NotNull final RVarsLoader loader,
                     @NotNull final RDebuggerEvaluator evaluator) {
    myLocation = location;
    myLoader = loader;
    myEvaluator = evaluator;
  }

  @NotNull
  public RLocation getLocation() {
    return myLocation;
  }

  @NotNull
  public RVarsLoader getLoader() {
    return myLoader;
  }

  @NotNull
  public RDebuggerEvaluator getEvaluator() {
    return myEvaluator;
  }
}
