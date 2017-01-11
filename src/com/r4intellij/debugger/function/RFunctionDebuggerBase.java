package com.r4intellij.debugger.function;

import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.data.RFunctionConstants;
import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RRuntimeException;
import com.r4intellij.debugger.exception.RUnexpectedExecutionResultTypeException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.RDebuggerStringUtils.*;
import static com.r4intellij.debugger.data.RCommands.EXECUTE_AND_STEP_COMMAND;
import static com.r4intellij.debugger.data.RLanguageConstants.FOR_LOOP_PREFIX;
import static com.r4intellij.debugger.data.RLanguageConstants.WHILE_LOOP_PREFIX;
import static com.r4intellij.debugger.data.RResponseConstants.DEBUG_AT_LINE_PREFIX;
import static com.r4intellij.debugger.data.RResponseConstants.EXITING_FROM_PREFIX;
import static com.r4intellij.debugger.executor.RExecutionResultType.*;
import static com.r4intellij.debugger.executor.RExecutorUtils.execute;
import static com.r4intellij.debugger.function.RTraceAndDebugUtils.traceAndDebugFunctions;

// TODO [dbg][upd_test]
abstract class RFunctionDebuggerBase implements RFunctionDebugger {

  @NotNull
  private final RExecutor myExecutor;

  @NotNull
  private final RFunctionDebuggerFactory myDebuggerFactory;

  @NotNull
  private final RFunctionDebuggerHandler myDebuggerHandler;

  @NotNull
  private final ROutputReceiver myOutputReceiver;

  @NotNull
  private final String myFunctionName;

  private int myCurrentLineNumber;

  @NotNull
  private String myResult;

  public RFunctionDebuggerBase(@NotNull final RExecutor executor,
                               @NotNull final RFunctionDebuggerFactory debuggerFactory,
                               @NotNull final RFunctionDebuggerHandler debuggerHandler,
                               @NotNull final ROutputReceiver outputReceiver,
                               @NotNull final String functionName) throws RDebuggerException {
    myExecutor = executor;
    myDebuggerFactory = debuggerFactory;
    myDebuggerHandler = debuggerHandler;
    myOutputReceiver = outputReceiver;
    myFunctionName = functionName;

    myCurrentLineNumber = initCurrentLine();
    traceAndDebugFunctions(myExecutor, myOutputReceiver);

    myResult = "";
  }

  @NotNull
  @Override
  public RLocation getLocation() {
    return new RLocation(myFunctionName, myCurrentLineNumber);
  }

  @Override
  public boolean hasNext() {
    return myCurrentLineNumber != -1;
  }

  @Override
  public void advance() throws RDebuggerException {
    if (!hasNext()) {
      throw new IllegalStateException("Advance could be called only if hasNext returns true");
    }

    final RExecutionResult result = myExecutor.execute(EXECUTE_AND_STEP_COMMAND);

    switch (result.getType()) {
      case CONTINUE_TRACE:
        handleContinueTrace(result);
        break;
      case DEBUG_AT:
        handleDebugAt(result);
        break;
      case DEBUGGING_IN:
        handleDebuggingIn(result);
        break;
      case EMPTY:
        handleEmpty(result);
        break;
      case EXITING_FROM:
        handleEndTrace(result);
        break;
      case RECURSIVE_EXITING_FROM:
        handleRecursiveEndTrace(result);
        break;
      default:
        throw new RUnexpectedExecutionResultTypeException(
          "Actual type is not the same as expected: " +
          "[" +
          "actual: " + result.getType() + ", " +
          "expected: " +
          "[" +
          CONTINUE_TRACE + ", " +
          DEBUG_AT + ", " +
          DEBUGGING_IN + ", " +
          EMPTY + ", " +
          RExecutionResultType.EXITING_FROM + ", " +
          RECURSIVE_EXITING_FROM +
          "]" +
          "]"
        );
    }
  }

  @NotNull
  @Override
  public String getResult() {
    if (hasNext()) {
      throw new IllegalStateException("GetResult could be called only if hasNext returns false");
    }

    return myResult;
  }

  protected abstract void handleDebugAt(@NotNull final RExecutionResult result) throws RDebuggerException;

  @NotNull
  protected abstract RExecutionResultType getStartTraceType();

  protected int initCurrentLine() throws RDebuggerException {
    handleDebugAt(
      execute(
        myExecutor,
        EXECUTE_AND_STEP_COMMAND,
        DEBUG_AT
      ),
      false,
      true
    );

    return myCurrentLineNumber;
  }

  protected void handleDebugAt(@NotNull final RExecutionResult result,
                               final boolean enableTraceAndDebug,
                               final boolean extractLineNumber) throws RDebuggerException {
    appendResult(result, myOutputReceiver);
    appendError(result, myOutputReceiver);

    final String output = result.getOutput();
    final int debugAtIndex = findNextLineAfterResult(result);

    if (isBraceLoopEntrance(output, debugAtIndex)) {
      handleDebugAt(execute(myExecutor, EXECUTE_AND_STEP_COMMAND, DEBUG_AT), enableTraceAndDebug, true);
    }
    else {
      if (extractLineNumber) {
        myCurrentLineNumber = extractLineNumber(output, debugAtIndex);
      }

      if (enableTraceAndDebug) {
        traceAndDebugFunctions(myExecutor, myOutputReceiver);
      }
    }
  }

  protected void handleContinueTrace(@NotNull final RExecutionResult result) throws RDebuggerException {
    handleEndTraceResult(result, 0);
    appendError(result, myOutputReceiver);

    execute(myExecutor, EXECUTE_AND_STEP_COMMAND, DEBUG_AT, myOutputReceiver);
    execute(myExecutor, EXECUTE_AND_STEP_COMMAND, getStartTraceType(), myOutputReceiver);

    myCurrentLineNumber = initCurrentLine();
    traceAndDebugFunctions(myExecutor, myOutputReceiver);
  }

  protected void handleEndTrace(@NotNull final RExecutionResult result) throws RDebuggerException {
    final int lastExitingFromEntry = result.getOutput().lastIndexOf(EXITING_FROM_PREFIX);

    handleEndTraceResult(result, lastExitingFromEntry);
    appendError(result, myOutputReceiver);
    handleEndTraceReturn(result, lastExitingFromEntry);

    myCurrentLineNumber = -1;
  }

  protected void handleDebuggingIn(@NotNull final RExecutionResult result) throws RDebuggerException {
    appendError(result, myOutputReceiver);

    myDebuggerHandler.appendDebugger(
      myDebuggerFactory.getFunctionDebugger(
        myExecutor,
        myDebuggerHandler,
        myOutputReceiver
      )
    );
  }

  protected void handleRecursiveEndTrace(@NotNull final RExecutionResult result) throws RDebuggerException {
    final RecursiveEndTraceData data = calculateRecursiveEndTraceData(result);

    handleEndTraceResult(result, data.myLastExitingFrom);
    appendError(result, myOutputReceiver);
    handleEndTraceReturn(result, data.myLastExitingFrom);

    myCurrentLineNumber = -1;

    myDebuggerHandler.setDropFrames(data.myExitingFromCount);
  }

  protected void handleEmpty(@NotNull final RExecutionResult result) throws RDebuggerException {
    appendError(result, myOutputReceiver);

    throw new RRuntimeException(result.getError());
  }

  private int extractLineNumber(@NotNull final String output, final int debugAtIndex) {
    final int lineNumberBegin = debugAtIndex + DEBUG_AT_LINE_PREFIX.length();
    final int lineNumberEnd = output.indexOf(':', lineNumberBegin + 1);

    return Integer.parseInt(output.substring(lineNumberBegin, lineNumberEnd)) - 1; // -1 because of `MAIN_FUNCTION` declaration
  }

  private int findNextLineAfterResult(@NotNull final RExecutionResult result) {
    int index = result.getResultRange().getEndOffset();

    final String output = result.getOutput();

    while (index < output.length() && StringUtil.isLineBreak(output.charAt(index))) {
      index++;
    }

    return index;
  }

  private boolean isBraceLoopEntrance(@NotNull final String output, final int debugAtIndex) {
    final int lineNumberBegin = debugAtIndex + DEBUG_AT_LINE_PREFIX.length();
    final int loopEntranceBegin = output.indexOf(':', lineNumberBegin + 1) + 2;
    final int lines = StringUtil.countNewLines(output.substring(loopEntranceBegin));

    return lines > 1 && (
      output.startsWith(FOR_LOOP_PREFIX, loopEntranceBegin) ||
      output.startsWith(WHILE_LOOP_PREFIX, loopEntranceBegin)
    );
  }

  private void handleEndTraceResult(@NotNull final RExecutionResult result, final int lastExitingFrom) {
    final TextRange resultRange = result.getResultRange();

    if (resultRange.getStartOffset() == 0 ||
        result.getType() == EXITING_FROM && !myFunctionName.equals(RFunctionConstants.MAIN_FUNCTION_NAME) ||
        isRecursiveEndTraceWithOutputInside(result, lastExitingFrom)) {
      appendResult(result, myOutputReceiver);
    }

    myResult = resultRange.substring(result.getOutput());
  }

  @NotNull
  private RecursiveEndTraceData calculateRecursiveEndTraceData(@NotNull final RExecutionResult result) {
    final String output = result.getOutput();

    int lastEntry = -1;
    int currentIndex = 0;
    int count = 0;

    while ((currentIndex = output.indexOf(EXITING_FROM_PREFIX, currentIndex)) != -1) {
      lastEntry = currentIndex;

      count++;
      currentIndex += EXITING_FROM_PREFIX.length();
    }

    return new RecursiveEndTraceData(lastEntry, count);
  }

  private void handleEndTraceReturn(@NotNull final RExecutionResult result, final int lastExitingFrom) throws RDebuggerException {
    final String output = result.getOutput();
    final int debugAtIndex = findDebugAtIndexInEndTraceReturn(result, lastExitingFrom);

    if (output.startsWith(DEBUG_AT_LINE_PREFIX, debugAtIndex)) {
      if (isBraceLoopEntrance(output, debugAtIndex)) {
        handleDebugAt(
          execute(myExecutor, EXECUTE_AND_STEP_COMMAND, DEBUG_AT),
          false,
          true
        );
      }

      myDebuggerHandler.setReturnLineNumber(extractLineNumber(output, debugAtIndex));
    }
  }

  private int findDebugAtIndexInEndTraceReturn(@NotNull final RExecutionResult result, final int lastExitingFrom) {
    if (result.getResultRange().getStartOffset() == 0 || isRecursiveEndTraceWithOutputInside(result, lastExitingFrom)) {
      return findNextLineBegin(
        result.getOutput(),
        lastExitingFrom + EXITING_FROM_PREFIX.length()
      );
    }
    else {
      return findNextLineAfterResult(result);
    }
  }

  private boolean isRecursiveEndTraceWithOutputInside(@NotNull final RExecutionResult result, final int lastExitingFrom) {
    return result.getType() == RECURSIVE_EXITING_FROM && result.getResultRange().getEndOffset() < lastExitingFrom;
  }

  private static class RecursiveEndTraceData {

    private final int myLastExitingFrom;
    private final int myExitingFromCount;

    private RecursiveEndTraceData(final int lastExitingFrom, final int exitingFromCount) {
      myLastExitingFrom = lastExitingFrom;
      myExitingFromCount = exitingFromCount;
    }
  }
}
