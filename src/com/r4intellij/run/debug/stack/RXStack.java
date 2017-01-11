package com.r4intellij.run.debug.stack;

import com.intellij.xdebugger.frame.XSuspendContext;
import com.r4intellij.debugger.frame.RStackFrame;
import com.r4intellij.run.debug.resolve.RResolvingSession;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.ListIterator;
import java.util.concurrent.ExecutorService;

public class RXStack {

  @NotNull
  private final List<RStackFrame> myOriginalStack;

  @NotNull
  private final RResolvingSession mySession;

  @NotNull
  private final ExecutorService myExecutor;

  @Nullable
  private List<RXStackFrame> myStack;

  @Nullable
  private XSuspendContext mySuspendContext;

  public RXStack(@NotNull final List<RStackFrame> stack,
                 @NotNull final RResolvingSession session,
                 @NotNull final ExecutorService executor) {
    myOriginalStack = stack;
    mySession = session;
    myExecutor = executor;
    myStack = null;
    mySuspendContext = null;
  }

  public void update() {
    // `myOriginalStack` ends with newest frame and `myStack` ends with eldest frame

    myStack = calculateStack(myStack, myOriginalStack);
    mySuspendContext = new RXSuspendContext(myStack);
  }

  @NotNull
  public XSuspendContext getSuspendContext() {
    if (mySuspendContext == null || myStack == null) {
      throw new IllegalStateException("GetSuspendContext could be called only after update");
    }

    return mySuspendContext;
  }

  @NotNull
  private List<RXStackFrame> calculateStack(@Nullable final List<RXStackFrame> previousStack,
                                            @NotNull final List<RStackFrame> currentStack) {
    if (previousStack == null) {
      return calculateStackFully(currentStack);
    }

    final int prevSize = previousStack.size();
    final int currentSize = currentStack.size();

    if (prevSize == currentSize) {
      return calculateStackOnTheSameDepth(previousStack, currentStack);
    }
    else if (currentSize > prevSize) {
      return calculateStackOnTheMoreDepth(previousStack, currentStack);
    }
    else {
      return calculateStackOnTheLessDepth(previousStack, currentStack);
    }
  }

  @NotNull
  private List<RXStackFrame> calculateStackFully(@NotNull final List<RStackFrame> currentStack) {
    final RXStackFrame[] result = new RXStackFrame[currentStack.size()];

    int index = 0;
    for (final RStackFrame frame : currentStack) {
      result[result.length - 1 - index] =
        new RXStackFrame(
          frame,
          mySession.resolveNext(frame.getLocation()),
          myExecutor
        );

      index++;
    }

    return Arrays.asList(result);
  }

  @NotNull
  private List<RXStackFrame> calculateStackOnTheSameDepth(@NotNull final List<RXStackFrame> previousStack,
                                                          @NotNull final List<RStackFrame> currentStack) {
    final List<RXStackFrame> result = new ArrayList<RXStackFrame>(previousStack);
    final RStackFrame lastFrame = currentStack.get(currentStack.size() - 1);

    result.set(
      0,
      new RXStackFrame(
        lastFrame,
        mySession.resolveCurrent(
          lastFrame.getLocation().getLine()
        ),
        myExecutor
      )
    );

    return result;
  }

  @NotNull
  private List<RXStackFrame> calculateStackOnTheMoreDepth(@NotNull final List<RXStackFrame> previousStack,
                                                          @NotNull final List<RStackFrame> currentStack) {
    final RXStackFrame[] result = new RXStackFrame[currentStack.size()];
    final int offset = currentStack.size() - previousStack.size();

    int index = 0;
    for (final RXStackFrame previousFrame : previousStack) {
      result[offset + index] = previousFrame;

      index++;
    }

    final ListIterator<RStackFrame> currentStackIterator = currentStack.listIterator(previousStack.size());

    for (int i = 0; i < offset; i++) {
      final RStackFrame frame = currentStackIterator.next();

      result[offset - 1 - i] =
        new RXStackFrame(
          frame,
          mySession.resolveNext(frame.getLocation()),
          myExecutor
        );
    }

    return Arrays.asList(result);
  }

  @NotNull
  private List<RXStackFrame> calculateStackOnTheLessDepth(@NotNull final List<RXStackFrame> previousStack,
                                                          @NotNull final List<RStackFrame> currentStack) {
    final RXStackFrame[] result = new RXStackFrame[currentStack.size()];
    final int offset = previousStack.size() - currentStack.size();

    final ListIterator<RXStackFrame> previousStackIterator = previousStack.listIterator(offset);

    for (int i = 0; i < currentStack.size(); i++) {
      result[i] = previousStackIterator.next();
    }

    final RStackFrame lastFrame = currentStack.get(currentStack.size() - 1);

    mySession.dropLast(offset);

    result[0] =
      new RXStackFrame(
        lastFrame,
        mySession.resolveCurrent(
          lastFrame.getLocation().getLine()
        ),
        myExecutor
      );

    return Arrays.asList(result);
  }
}
