package com.r4intellij.run.debug.stack;

import com.intellij.xdebugger.frame.XExecutionStack;
import com.intellij.xdebugger.frame.XStackFrame;
import com.intellij.xdebugger.frame.XSuspendContext;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

class RXSuspendContext extends XSuspendContext {

  @NotNull
  private final RXExecutionStack myExecutionStack;

  public RXSuspendContext(@NotNull final List<RXStackFrame> stack) {
    myExecutionStack = new RXExecutionStack(stack);
  }

  @NotNull
  @Override
  public XExecutionStack getActiveExecutionStack() {
    return myExecutionStack;
  }

  private static class RXExecutionStack extends XExecutionStack {

    @NotNull
    private final List<RXStackFrame> myStack;

    private RXExecutionStack(@NotNull final List<RXStackFrame> stack) {
      super(""); // argument used as a description for current thread

      myStack = stack;
    }

    @Nullable
    @Override
    public XStackFrame getTopFrame() {
      return myStack.isEmpty() ? null : myStack.get(0);
    }

    @Override
    public void computeStackFrames(final int firstFrameIndex, final XStackFrameContainer container) {
      if (firstFrameIndex <= myStack.size()) {
        final List<RXStackFrame> stackFrames = myStack.subList(firstFrameIndex, myStack.size());

        container.addStackFrames(stackFrames, true);
      }
    }
  }
}
