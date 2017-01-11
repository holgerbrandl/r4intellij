package com.r4intellij.debugger.function;

import com.intellij.openapi.util.TextRange;
import com.intellij.util.containers.ContainerUtil;
import com.r4intellij.debugger.MockitoUtils;
import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RRuntimeException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.mock.*;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.r4intellij.debugger.data.RCommands.EXECUTE_AND_STEP_COMMAND;
import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_ENTER_FUNCTION_SUFFIX;
import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_FUNCTION_PREFIX;
import static com.r4intellij.debugger.data.RResponseConstants.*;
import static com.r4intellij.debugger.mock.MockRExecutor.LS_FUNCTIONS_ERROR;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class RUnbraceFunctionDebuggerTest {

  @Test
  public void ordinary() throws RDebuggerException {
    /*
    `x + 1`
    */

    final OrdinaryRExecutor executor = new OrdinaryRExecutor();
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalRFunctionDebuggerHandler(),
      receiver,
      "abc"
    );

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(1, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertFalse(debugger.hasNext());
    assertEquals(new RLocation("abc", -1), debugger.getLocation());
    assertEquals("", debugger.getResult());
    assertEquals(2, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
  }

  @Test
  public void function() throws RDebuggerException {
    /*
    f()
    */

    final FunctionRExecutor executor = new FunctionRExecutor();
    final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(new IllegalRFunctionDebugger());
    final FunctionRFunctionDebuggerHandler handler = new FunctionRFunctionDebuggerHandler();
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      factory,
      handler,
      receiver,
      "abc"
    );

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(1, executor.getCounter());
    assertEquals(0, factory.getCounter());
    assertEquals(0, handler.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    // one of next debuggers will handle `RECURSIVE_EXITING_FROM`

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(2, executor.getCounter());
    assertEquals(1, factory.getCounter());
    assertEquals(1, handler.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_debugging"), receiver.getErrors());
  }

  @Test
  public void recursiveReturnAndOutputBefore() throws RDebuggerException {
    /*
    `x + 1` with recursive return
    */

    final RExecutionResult secondResult = new RExecutionResult(
      "[1] 1 2 3\n" +
      EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
      EXITING_FROM_PREFIX + "def()\n" +
      EXITING_FROM_PREFIX + "abc(1:10)\n" +
      BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
      RExecutionResultType.RECURSIVE_EXITING_FROM,
      new TextRange(0, 9),
      "error_exit"
    );

    recursiveReturn(secondResult, true, -1);
  }

  @Test
  public void recursiveReturnAndOutputInside() throws RDebuggerException {
    /*
    `x + 1` with recursive return
    */

    final RExecutionResult secondResult = new RExecutionResult(
      EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
      "[1] 1 2 3\n" +
      EXITING_FROM_PREFIX + "def()\n" +
      EXITING_FROM_PREFIX + "abc(1:10)\n" +
      BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
      RExecutionResultType.RECURSIVE_EXITING_FROM,
      new TextRange(42, 51),
      "error_exit"
    );

    recursiveReturn(secondResult, true, -1);
  }

  @Test
  public void recursiveReturnAndOutputAfter() throws RDebuggerException {
    /*
    `x + 1` with recursive return
    */

    final RExecutionResult secondResult = new RExecutionResult(
      EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
      EXITING_FROM_PREFIX + "def()\n" +
      EXITING_FROM_PREFIX + "abc(1:10)\n" +
      "[1] 1 2 3\n" +
      BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
      RExecutionResultType.RECURSIVE_EXITING_FROM,
      new TextRange(86, 95),
      "error_exit"
    );

    recursiveReturn(secondResult, false, -1);
  }

  @Test
  public void recursiveReturnAndOutputBeforeAndDebugAt() throws RDebuggerException {

    /*
    `x + 1` with recursive return and `debug at` at the end
    */

    final RExecutionResult secondResult = new RExecutionResult(
      "[1] 1 2 3\n" +
      EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
      EXITING_FROM_PREFIX + "def()\n" +
      EXITING_FROM_PREFIX + "abc(1:10)\n" +
      DEBUG_AT_LINE_PREFIX + "6: x <- c(1)" +
      BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
      RExecutionResultType.RECURSIVE_EXITING_FROM,
      new TextRange(0, 9),
      "error_exit"
    );

    recursiveReturn(secondResult, true, 5);
  }

  @Test
  public void recursiveReturnAndOutputInsideAndDebugAt() throws RDebuggerException {
    /*
    `x + 1` with recursive return and `debug at` at the end
    */

    final RExecutionResult secondResult = new RExecutionResult(
      EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
      "[1] 1 2 3\n" +
      EXITING_FROM_PREFIX + "def()\n" +
      EXITING_FROM_PREFIX + "abc(1:10)\n" +
      DEBUG_AT_LINE_PREFIX + "6: x <- c(1)" +
      BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
      RExecutionResultType.RECURSIVE_EXITING_FROM,
      new TextRange(42, 51),
      "error_exit"
    );

    recursiveReturn(secondResult, true, 5);
  }

  @Test
  public void recursiveReturnAndOutputAfterAndDebugAt() throws RDebuggerException {
    /*
    `x + 1` with recursive return and `debug at` at the end
    */

    final RExecutionResult secondResult = new RExecutionResult(
      EXITING_FROM_PREFIX + "FUN(c(-1, 0, 1)[[3L]], ...)\n" +
      EXITING_FROM_PREFIX + "def()\n" +
      EXITING_FROM_PREFIX + "abc(1:10)\n" +
      "[1] 1 2 3\n" +
      DEBUG_AT_LINE_PREFIX + "6: x <- c(1)" +
      BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
      RExecutionResultType.RECURSIVE_EXITING_FROM,
      new TextRange(86, 95),
      "error_exit"
    );

    recursiveReturn(secondResult, false, 5);
  }

  @Test
  public void exitingFromWithOutputBefore() throws RDebuggerException {
    /*
    `print(x + 1)`
    */

    final RExecutionResult secondResult = new RExecutionResult(
      "[1] 1 2 3\n" +
      EXITING_FROM_PREFIX + "abc(c(1:10))\n" +
      BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
      RExecutionResultType.EXITING_FROM,
      new TextRange(0, 9),
      "error_exit"
    );

    exitingFromWithOutput(secondResult, -1);
  }

  @Test
  public void exitingFromWithOutputAfter() throws RDebuggerException {
    /*
    `x + 1` with `debug at` at the end
    */

    final RExecutionResult secondResult = new RExecutionResult(
      EXITING_FROM_PREFIX + "abc(c(1:10))\n" +
      "[1] 1 2 3\n" +
      DEBUG_AT_LINE_PREFIX + "6: x <- c(1)\n" +
      BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
      RExecutionResultType.EXITING_FROM,
      new TextRange(27, 36),
      "error_exit"
    );

    exitingFromWithOutput(secondResult, 5);
  }

  @Test
  public void continueTrace() throws RDebuggerException {
    /*
    `x + 1` with `continue trace`
    */

    final ContinueTraceRExecutor executor = new ContinueTraceRExecutor();
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalRFunctionDebuggerHandler(),
      receiver,
      "abc"
    );

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(1, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(5, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Arrays.asList("error_continue", "error_entry", "error_entry", LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertFalse(debugger.hasNext());
    assertEquals(new RLocation("abc", -1), debugger.getLocation());
    assertEquals("[1] 4 5 6", debugger.getResult());
    assertEquals(6, executor.getCounter());
    assertEquals(Collections.singletonList("[1] 4 5 6"), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
  }

  @Test(expected = RRuntimeException.class)
  public void error() throws RDebuggerException {
    /*
    if (10 > log(-1)) {
      print("ok")
    }
    */

    final ErrorRExecutor executor = new ErrorRExecutor();
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalRFunctionDebuggerHandler(),
      receiver,
      "abc"
    );

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(1, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    debugger.advance();
  }


  @Test
  public void loop() throws RDebuggerException {
    /*
    for (i in 1:2) ...
    */

    final LoopRExecutor executor = new LoopRExecutor();
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalRFunctionDebuggerHandler(),
      receiver,
      "abc"
    );

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(1, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(3, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Arrays.asList("error_dbg_at_1", LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(5, executor.getCounter());
    assertEquals(Collections.singletonList("[1] 1 2 3"), receiver.getOutputs());
    assertEquals(Arrays.asList("error_dbg_at_2", LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertFalse(debugger.hasNext());
    assertEquals(new RLocation("abc", -1), debugger.getLocation());
    assertEquals("[1] 1\n[1] 2", debugger.getResult());
    assertEquals(6, executor.getCounter());
    assertEquals(Collections.singletonList("[1] 1\n[1] 2"), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
  }

  @Test
  public void loopWithFunction() throws RDebuggerException {
    /*
    for (i in 1:2) d(i)
    */

    final LoopWithFunctionRExecutor executor = new LoopWithFunctionRExecutor();
    final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(new IllegalRFunctionDebugger());
    final LoopWithFunctionRFunctionDebuggerHandler handler = new LoopWithFunctionRFunctionDebuggerHandler();
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      factory,
      handler,
      receiver,
      "abc"
    );

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(1, executor.getCounter());
    assertEquals(0, factory.getCounter());
    assertEquals(0, handler.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(3, executor.getCounter());
    assertEquals(0, factory.getCounter());
    assertEquals(0, handler.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Arrays.asList("error_dbg_at", LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(4, executor.getCounter());
    assertEquals(1, factory.getCounter());
    assertEquals(1, handler.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_debugging"), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(5, executor.getCounter());
    assertEquals(2, factory.getCounter());
    assertEquals(2, handler.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_debugging"), receiver.getErrors());
  }

  @Test
  public void ifStatement() throws RDebuggerException {
    /*
    if (i > 5) print(i) else print(i + 1)
    */

    final IfStatementRExecutor executor = new IfStatementRExecutor();
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalRFunctionDebuggerHandler(),
      receiver,
      "abc"
    );

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(1, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    assertEquals(3, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Arrays.asList("error_body", LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertFalse(debugger.hasNext());
    assertEquals(new RLocation("abc", -1), debugger.getLocation());
    assertEquals(4, executor.getCounter());
    assertEquals(Collections.singletonList("[1] 2"), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
  }

  private void recursiveReturn(@NotNull final RExecutionResult secondResult, final boolean output, final int returnLineNumber)
    throws RDebuggerException {
    final RExecutor executor = MockitoUtils.setupExecutor(
      new ContainerUtil.ImmutableMapBuilder<String, List<RExecutionResult>>()
        .put(RTraceAndDebugUtilsTest.LS_FUNCTIONS_COMMAND, Collections.singletonList(RTraceAndDebugUtilsTest.NO_FUNCTIONS_RESULT))
        .put(EXECUTE_AND_STEP_COMMAND, Collections.singletonList(secondResult))
        .build()
    );

    final RFunctionDebuggerHandler handler = mock(RFunctionDebuggerHandler.class);
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      new MockRFunctionDebuggerFactory(null),
      handler,
      receiver,
      "abc"
    );

    final List<String> currentCommands = new ArrayList<String>(Collections.singletonList(RTraceAndDebugUtilsTest.LS_FUNCTIONS_COMMAND));

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    MockitoUtils.verifyExecutor(executor, currentCommands);
    verifyZeroInteractions(handler);
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    currentCommands.add(EXECUTE_AND_STEP_COMMAND);
    debugger.advance();

    assertFalse(debugger.hasNext());
    assertEquals(new RLocation("abc", -1), debugger.getLocation());
    assertEquals("[1] 1 2 3", debugger.getResult());
    MockitoUtils.verifyExecutor(executor, currentCommands);
    verify(handler, times(1)).setDropFrames(3);
    if (returnLineNumber != -1) verify(handler, times(1)).setReturnLineNumber(returnLineNumber);
    assertEquals(output ? Collections.singletonList("[1] 1 2 3") : Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());

    verifyNoMoreInteractions(executor, handler);
  }

  private void exitingFromWithOutput(@NotNull final RExecutionResult secondResult, final int returnLineNumber)
    throws RDebuggerException {
    final RExecutor executor = MockitoUtils.setupExecutor(
      new ContainerUtil.ImmutableMapBuilder<String, List<RExecutionResult>>()
        .put(RTraceAndDebugUtilsTest.LS_FUNCTIONS_COMMAND, Collections.singletonList(RTraceAndDebugUtilsTest.NO_FUNCTIONS_RESULT))
        .put(EXECUTE_AND_STEP_COMMAND, Collections.singletonList(secondResult))
        .build()
    );

    final RFunctionDebuggerHandler handler = mock(RFunctionDebuggerHandler.class);
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final RUnbraceFunctionDebugger debugger = new RUnbraceFunctionDebugger(
      executor,
      new MockRFunctionDebuggerFactory(null),
      handler,
      receiver,
      "abc"
    );

    assertTrue(debugger.hasNext());
    assertEquals(new RLocation("abc", 0), debugger.getLocation());
    MockitoUtils.verifyExecutor(executor, Collections.singletonList(RTraceAndDebugUtilsTest.LS_FUNCTIONS_COMMAND));
    verifyZeroInteractions(handler);
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList(LS_FUNCTIONS_ERROR), receiver.getErrors());

    receiver.reset();
    debugger.advance();

    assertFalse(debugger.hasNext());
    assertEquals(new RLocation("abc", -1), debugger.getLocation());
    assertEquals("[1] 1 2 3", debugger.getResult());
    MockitoUtils.verifyExecutor(executor, Arrays.asList(RTraceAndDebugUtilsTest.LS_FUNCTIONS_COMMAND, EXECUTE_AND_STEP_COMMAND));
    if (returnLineNumber != -1) verify(handler, times(1)).setReturnLineNumber(returnLineNumber);
    assertEquals(Collections.singletonList("[1] 1 2 3"), receiver.getOutputs());
    assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());

    verifyNoMoreInteractions(executor, handler);
  }

  private static class OrdinaryRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (command.equals(EXECUTE_AND_STEP_COMMAND)) {
        return new RExecutionResult(
          EXITING_FROM_PREFIX + "abc(c(1:10))\n" +
          BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
          RExecutionResultType.EXITING_FROM,
          TextRange.EMPTY_RANGE,
          "error_exit"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }

  private static class FunctionRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (command.equals(EXECUTE_AND_STEP_COMMAND)) {
        return new RExecutionResult(
          DEBUGGING_IN_PREFIX + "abc(c(1:10))\n" +
          DEBUG_AT_PREFIX + "{\n" +
          "    .doTrace(" + SERVICE_FUNCTION_PREFIX + "abc" + SERVICE_ENTER_FUNCTION_SUFFIX + "(), \"on entry\")\n" +
          "    {\n" +
          "        x + 1\n" +
          "    }\n" +
          "}\n" +
          BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
          RExecutionResultType.DEBUGGING_IN,
          TextRange.EMPTY_RANGE,
          "error_debugging"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }

  private static class FunctionRFunctionDebuggerHandler extends IllegalRFunctionDebuggerHandler {

    private int myCounter = 0;

    @Override
    public void appendDebugger(@NotNull final RFunctionDebugger debugger) {
      myCounter++;
    }

    public int getCounter() {
      return myCounter;
    }
  }

  private static class ContinueTraceRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 2) {
        return new RExecutionResult(
          EXITING_FROM_PREFIX + "abc()\n" +
          "[1] 1 2 3\n" +
          DEBUGGING_IN_PREFIX + "abc()\n" +
          DEBUG_AT_PREFIX + "{\n" +
          "    .doTrace(" + SERVICE_FUNCTION_PREFIX + "abc" + SERVICE_ENTER_FUNCTION_SUFFIX + "(), \"on entry\")\n" +
          "    {\n" +
          "        c(1:3)\n" +
          "    }\n" +
          "}\n" +
          BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
          RExecutionResultType.CONTINUE_TRACE,
          new TextRange(20, 29),
          "error_continue"
        );
      }

      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 3) {
        return new RExecutionResult(
          "output",
          RExecutionResultType.DEBUG_AT,
          TextRange.EMPTY_RANGE,
          "error_entry"
        );
      }

      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 4) {
        return new RExecutionResult(
          TRACING_PREFIX + "abc() on entry \n" +
          "[1] \"abc\"\n" +
          DEBUG_AT_PREFIX + "c(4:6)\n" +
          BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
          RExecutionResultType.START_TRACE_UNBRACE,
          TextRange.EMPTY_RANGE,
          "error_entry"
        );
      }

      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 6) {
        return new RExecutionResult(
          EXITING_FROM_PREFIX + "abc()\n" +
          "[1] 4 5 6\n" +
          BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
          RExecutionResultType.EXITING_FROM,
          new TextRange(20, 29),
          "error_exit"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }

  private static class ErrorRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 2) {
        return new RExecutionResult(
          "",
          RExecutionResultType.EMPTY,
          TextRange.EMPTY_RANGE,
          "Error in if (10 > log(-1)) { : missing value where TRUE/FALSE needed\n" +
          "In addition: Warning message:\n" +
          "In log(-1) : NaNs produced"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }

  private static class LoopRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 2) {
        return new RExecutionResult(
          DEBUG_AT_PREFIX + "print(i)",
          RExecutionResultType.DEBUG_AT,
          TextRange.EMPTY_RANGE,
          "error_dbg_at_1"
        );
      }

      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 4) {
        return new RExecutionResult(
          "[1] 1 2 3\n" +
          DEBUG_AT_PREFIX + "print(i)",
          RExecutionResultType.DEBUG_AT,
          new TextRange(0, 9),
          "error_dbg_at_2"
        );
      }

      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 6) {
        return new RExecutionResult(
          "[1] 1\n[1] 2\n" +
          EXITING_FROM_PREFIX + "abc()\n" +
          BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
          RExecutionResultType.EXITING_FROM,
          new TextRange(0, 11),
          "error_exit"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }

  private static class LoopWithFunctionRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 2) {
        return new RExecutionResult(
          DEBUG_AT_PREFIX + "h()",
          RExecutionResultType.DEBUG_AT,
          TextRange.EMPTY_RANGE,
          "error_dbg_at"
        );
      }

      if (command.equals(EXECUTE_AND_STEP_COMMAND) && (getCounter() == 4 || getCounter() == 5)) {
        return new RExecutionResult(
          DEBUGGING_IN_PREFIX + "d(i)\n" +
          DEBUG_AT_PREFIX + "{\n" +
          "    .doTrace(" + SERVICE_FUNCTION_PREFIX + "abc" + SERVICE_ENTER_FUNCTION_SUFFIX + "(), \"on entry\")\n" +
          "    print(i)\n" +
          "}\n" +
          BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
          RExecutionResultType.DEBUGGING_IN,
          TextRange.EMPTY_RANGE,
          "error_debugging"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }

  private static class LoopWithFunctionRFunctionDebuggerHandler extends IllegalRFunctionDebuggerHandler {

    private int myCounter = 0;

    @Override
    public void appendDebugger(@NotNull final RFunctionDebugger debugger) {
      myCounter++;
    }

    public int getCounter() {
      return myCounter;
    }
  }

  private static class IfStatementRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 2) {
        return new RExecutionResult(
          DEBUG_AT_LINE_PREFIX + "2: print(i + 1)\n" +
          BROWSE_PREFIX + "2" + BROWSE_SUFFIX,
          RExecutionResultType.DEBUG_AT,
          TextRange.EMPTY_RANGE,
          "error_body"
        );
      }

      if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 4) {
        return new RExecutionResult(
          "[1] 2\n" +
          EXITING_FROM_PREFIX + "f(1)\n" +
          BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
          RExecutionResultType.EXITING_FROM,
          new TextRange(0, 5),
          "error_exit"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }
}