package com.r4intellij.run.debug.stack;

import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.frame.XExecutionStack;
import com.intellij.xdebugger.frame.XStackFrame;
import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.frame.RStackFrame;
import com.r4intellij.debugger.mock.IllegalRDebuggerEvaluator;
import com.r4intellij.debugger.mock.IllegalRVarsLoader;
import com.r4intellij.run.debug.mock.ExecutorServices;
import com.r4intellij.run.debug.mock.MockXSourcePosition;
import com.r4intellij.run.debug.mock.MockXStackFrameContainer;
import com.r4intellij.run.debug.resolve.RResolvingSession;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

public class RXStackTest {

  @Test
  public void sameDepth() {
    final List<RStackFrame> originalStack = new ArrayList<RStackFrame>();
    final MockRResolvingSession resolvingSession = new MockRResolvingSession();

    final RXStack stack = new RXStack(originalStack, resolvingSession, ExecutorServices.ILLEGAL_EXECUTOR);

    originalStack.add(
      new RStackFrame(
        new RLocation("abc", 2),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    originalStack.add(
      new RStackFrame(
        new RLocation("def", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    stack.update();

    assertEquals(0, resolvingSession.myCurrent);
    assertEquals(2, resolvingSession.myNext);
    assertEquals(0, resolvingSession.myDropped);
    check(stack, 2, 1);

    originalStack.set(
      1,
      new RStackFrame(
        new RLocation("def", 2),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    stack.update();

    assertEquals(1, resolvingSession.myCurrent);
    assertEquals(2, resolvingSession.myNext);
    assertEquals(0, resolvingSession.myDropped);
    check(stack, 3, 1);
  }

  @Test
  public void plusOneDepth() {
    final List<RStackFrame> originalStack = new ArrayList<RStackFrame>();
    final MockRResolvingSession resolvingSession = new MockRResolvingSession();

    final RXStack stack = new RXStack(originalStack, resolvingSession, ExecutorServices.ILLEGAL_EXECUTOR);

    originalStack.add(
      new RStackFrame(
        new RLocation("abc", 2),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    originalStack.add(
      new RStackFrame(
        new RLocation("def", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    stack.update();

    assertEquals(2, resolvingSession.myNext);
    assertEquals(0, resolvingSession.myCurrent);
    assertEquals(0, resolvingSession.myDropped);
    check(stack, 2, 1);

    originalStack.add(
      new RStackFrame(
        new RLocation("ghi", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    stack.update();

    assertEquals(3, resolvingSession.myNext);
    assertEquals(0, resolvingSession.myCurrent);
    assertEquals(0, resolvingSession.myDropped);
    check(stack, 3, 2, 1);
  }

  @Test
  public void moreDepth() {
    final List<RStackFrame> originalStack = new ArrayList<RStackFrame>();
    final MockRResolvingSession resolvingSession = new MockRResolvingSession();

    final RXStack stack = new RXStack(originalStack, resolvingSession, ExecutorServices.ILLEGAL_EXECUTOR);

    originalStack.add(
      new RStackFrame(
        new RLocation("abc", 2),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    originalStack.add(
      new RStackFrame(
        new RLocation("def", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    stack.update();

    assertEquals(2, resolvingSession.myNext);
    assertEquals(0, resolvingSession.myCurrent);
    assertEquals(0, resolvingSession.myDropped);
    check(stack, 2, 1);

    originalStack.add(
      new RStackFrame(
        new RLocation("ghi", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    originalStack.add(
      new RStackFrame(
        new RLocation("jkl", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    stack.update();

    assertEquals(4, resolvingSession.myNext);
    assertEquals(0, resolvingSession.myCurrent);
    assertEquals(0, resolvingSession.myDropped);
    check(stack, 4, 3, 2, 1);
  }

  @Test
  public void minusOneDepth() {
    final List<RStackFrame> originalStack = new ArrayList<RStackFrame>();
    final MockRResolvingSession resolvingSession = new MockRResolvingSession();

    final RXStack stack = new RXStack(originalStack, resolvingSession, ExecutorServices.ILLEGAL_EXECUTOR);

    originalStack.add(
      new RStackFrame(
        new RLocation("abc", 2),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    originalStack.add(
      new RStackFrame(
        new RLocation("def", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    stack.update();

    assertEquals(2, resolvingSession.myNext);
    assertEquals(0, resolvingSession.myCurrent);
    assertEquals(0, resolvingSession.myDropped);
    check(stack, 2, 1);

    originalStack.remove(originalStack.size() - 1);

    stack.update();

    assertEquals(2, resolvingSession.myNext);
    assertEquals(1, resolvingSession.myCurrent);
    assertEquals(1, resolvingSession.myDropped);
    check(stack, 3, 1);
  }

  @Test
  public void lessDepth() {
    final List<RStackFrame> originalStack = new ArrayList<RStackFrame>();
    final MockRResolvingSession resolvingSession = new MockRResolvingSession();

    final RXStack stack = new RXStack(originalStack, resolvingSession, ExecutorServices.ILLEGAL_EXECUTOR);

    originalStack.add(
      new RStackFrame(
        new RLocation("abc", 2),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    originalStack.add(
      new RStackFrame(
        new RLocation("def", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    originalStack.add(
      new RStackFrame(
        new RLocation("ghi", 1),
        new IllegalRVarsLoader(),
        new IllegalRDebuggerEvaluator()
      )
    );

    stack.update();

    assertEquals(3, resolvingSession.myNext);
    assertEquals(0, resolvingSession.myCurrent);
    assertEquals(0, resolvingSession.myDropped);
    check(stack, 3, 2, 1);

    originalStack.remove(originalStack.size() - 1);
    originalStack.remove(originalStack.size() - 1);

    stack.update();

    assertEquals(3, resolvingSession.myNext);
    assertEquals(1, resolvingSession.myCurrent);
    assertEquals(2, resolvingSession.myDropped);
    check(stack, 4);
  }

  private void check(@NotNull final RXStack stack, @NotNull final int... lines) {
    final MockXStackFrameContainer container = new MockXStackFrameContainer();

    final XExecutionStack executionStack = stack.getSuspendContext().getActiveExecutionStack();
    assert executionStack != null;

    executionStack.computeStackFrames(0, container);

    int index = 0;
    for (final XStackFrame frame : container.getResult()) {
      final XSourcePosition position = frame.getSourcePosition();
      assert position != null;

      assertEquals(lines[index], position.getLine());

      index++;
    }
  }

  private static class MockRResolvingSession implements RResolvingSession {

    private int myNext = 0;
    private int myCurrent = 0;
    private int myDropped = 0;

    @Nullable
    @Override
    public XSourcePosition resolveNext(@NotNull final RLocation nextLocation) {
      myNext++;

      return new MockXSourcePosition(null, myNext + myCurrent);
    }

    @Nullable
    @Override
    public XSourcePosition resolveCurrent(final int line) {
      myCurrent++;

      return new MockXSourcePosition(null, myNext + myCurrent);
    }

    @Override
    public void dropLast(final int number) {
      myDropped += number;
    }
  }
}