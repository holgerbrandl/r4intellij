package com.r4intellij.debugger.function;

import com.intellij.openapi.util.TextRange;
import com.intellij.util.containers.ContainerUtil;
import com.r4intellij.debugger.MockitoUtils;
import com.r4intellij.debugger.data.RFunctionConstants;
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
import static com.r4intellij.debugger.data.RFunctionConstants.*;
import static com.r4intellij.debugger.data.RResponseConstants.*;
import static com.r4intellij.debugger.mock.MockRExecutor.LS_FUNCTIONS_ERROR;
import static org.junit.Assert.*;
import static org.mockito.Mockito.*;

public class RBraceFunctionDebuggerTest {

    @Test
    public void ordinary() throws RDebuggerException {
    /*
    abc() {
      instruction1
      instruction2
    }
    */

        final OrdinaryRExecutor executor = new OrdinaryRExecutor();
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                new MockRFunctionDebuggerFactory(null),
                new IllegalRFunctionDebuggerHandler(),
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(2, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_dbg_at_1", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 1), debugger.getLocation());
        assertEquals(4, executor.getCounter());
        assertEquals(Collections.singletonList("[1] 1 2 3"), receiver.getOutputs());
        assertEquals(Arrays.asList("error_dbg_at_2", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation("abc", -1), debugger.getLocation());
        assertEquals("", debugger.getResult());
        assertEquals(5, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
    }


    @Test
    public void function() throws RDebuggerException {
    /*
    abc() {
      def()
      instruction2
    }
    */

        final FunctionRExecutor executor = new FunctionRExecutor();
        final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(new IllegalRFunctionDebugger());
        final FunctionRFunctionDebuggerHandler handler = new FunctionRFunctionDebuggerHandler();
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                factory,
                handler,
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(2, executor.getCounter());
        assertEquals(0, factory.getCounter());
        assertEquals(0, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_dbg_at_1", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(3, executor.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(1, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_debugging"), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation("abc", -1), debugger.getLocation());
        assertEquals("[1] 1 2 3", debugger.getResult());
        assertEquals(4, executor.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(1, handler.getCounter());
        assertEquals(Collections.singletonList("[1] 1 2 3"), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
    }


    @Test
    public void recursiveReturnAndOutputBefore() throws RDebuggerException {
    /*
    abc() {
      `x + 1` with recursive return
    }
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: c(1)\n" +
                        BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "ghi()\n" +
                        EXITING_FROM_PREFIX + "def()\n" +
                        EXITING_FROM_PREFIX + "abc()\n" +
                        BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.RECURSIVE_EXITING_FROM,
                new TextRange(0, 9),
                "error_exit"
        );

        exiting(firstResult, thirdResult, "abc", true, true, -1);
    }


    @Test
    public void recursiveReturnAndOutputInside() throws RDebuggerException {
     /*
    abc() {
      `x + 1` with recursive return
    }
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: c(1)\n" +
                        BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                EXITING_FROM_PREFIX + "ghi()\n" +
                        "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "def()\n" +
                        EXITING_FROM_PREFIX + "abc()\n" +
                        BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.RECURSIVE_EXITING_FROM,
                new TextRange(20, 29),
                "error_exit"
        );

        exiting(firstResult, thirdResult, "abc", true, true, -1);
    }


    @Test
    public void recursiveReturnAndOutputAfter() throws RDebuggerException {
    /*
    abc() {
      `x + 1` with recursive return
    }
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: c(1)\n" +
                        BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                EXITING_FROM_PREFIX + "ghi()\n" +
                        EXITING_FROM_PREFIX + "def()\n" +
                        EXITING_FROM_PREFIX + "abc()\n" +
                        "[1] 1 2 3\n" +
                        BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.RECURSIVE_EXITING_FROM,
                new TextRange(60, 69),
                "error_exit"
        );

        exiting(firstResult, thirdResult, "abc", false, true, -1);
    }


    @Test
    public void recursiveReturnAndOutputBeforeAndDebugAt() throws RDebuggerException {
    /*
    abc() {
      `x + 1` with recursive return and `debug at`
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: c(1)\n" +
                        BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "ghi()\n" +
                        EXITING_FROM_PREFIX + "def()\n" +
                        EXITING_FROM_PREFIX + "abc()\n" +
                        DEBUG_AT_LINE_PREFIX + "4: x <- c(1)\n" +
                        BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.RECURSIVE_EXITING_FROM,
                new TextRange(0, 9),
                "error_exit"
        );

        exiting(firstResult, thirdResult, "abc", true, true, 3);
    }


    @Test
    public void recursiveReturnAndOutputInsideAndDebugAt() throws RDebuggerException {
    /*
    abc() {
      `x + 1` with recursive return and `debug at`
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: c(1)\n" +
                        BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                EXITING_FROM_PREFIX + "ghi()\n" +
                        "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "def()\n" +
                        EXITING_FROM_PREFIX + "abc()\n" +
                        DEBUG_AT_LINE_PREFIX + "4: x <- c(1)\n" +
                        BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.RECURSIVE_EXITING_FROM,
                new TextRange(20, 29),
                "error_exit"
        );

        exiting(firstResult, thirdResult, "abc", true, true, 3);
    }


    @Test
    public void recursiveReturnAndOutputAfterAndDebugAt() throws RDebuggerException {
    /*
    abc() {
      `x + 1` with recursive return and `debug at`
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: c(1)\n" +
                        BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                EXITING_FROM_PREFIX + "ghi()\n" +
                        EXITING_FROM_PREFIX + "def()\n" +
                        EXITING_FROM_PREFIX + "abc()\n" +
                        "[1] 1 2 3\n" +
                        DEBUG_AT_LINE_PREFIX + "4: x <- c(1)\n" +
                        BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.RECURSIVE_EXITING_FROM,
                new TextRange(60, 69),
                "error_exit"
        );

        exiting(firstResult, thirdResult, "abc", false, true, 3);
    }


    @Test
    public void exitingFromWithOutputBefore() throws RDebuggerException {
    /*
    abc() {
      print(c(1:3))
    }
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: c(1)\n" +
                        BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + "abc()\n" +
                        BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.EXITING_FROM,
                new TextRange(0, 9),
                "error_exit"
        );

        exiting(firstResult, thirdResult, "abc", true, false, -1);
    }


    @Test
    public void exitingFromWithOutputAfter() throws RDebuggerException {
    /*
    abc() {
      `x + 1` with `debug at`
    }
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: c(1)\n" +
                        BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                EXITING_FROM_PREFIX + "abc()\n" +
                        "[1] 1 2 3\n" +
                        DEBUG_AT_LINE_PREFIX + "4: x <- c(1)\n" +
                        BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                RExecutionResultType.EXITING_FROM,
                new TextRange(20, 29),
                "error_exit"
        );

        exiting(firstResult, thirdResult, "abc", true, false, 3);
    }


    @Test
    public void exitingFromMainWithOutputBefore() throws RDebuggerException {
    /*
    main() {
      print("ok")
    }
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: ls()",
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                "[1] 1 2 3\n" +
                        EXITING_FROM_PREFIX + RFunctionConstants.MAIN_FUNCTION_NAME + "()",
                RExecutionResultType.EXITING_FROM,
                new TextRange(0, 9),
                "error_exit"
        );

        exiting(firstResult, thirdResult, MAIN_FUNCTION_NAME, true, false, -1);
    }


    @Test
    public void exitingFromMainWithOutputAfter() throws RDebuggerException {
    /*
    main() {
      ls()
    }
    */

        final RExecutionResult firstResult = new RExecutionResult(
                DEBUG_AT_LINE_PREFIX + "1: ls()",
                RExecutionResultType.DEBUG_AT,
                TextRange.EMPTY_RANGE,
                "error_dbg_at"
        );
        final RExecutionResult thirdResult = new RExecutionResult(
                EXITING_FROM_PREFIX + RFunctionConstants.MAIN_FUNCTION_NAME + "()\n" +
                        "[1] 1 2 3",
                RExecutionResultType.EXITING_FROM,
                new TextRange(36, 45),
                "error_exit"
        );

        exiting(firstResult, thirdResult, MAIN_FUNCTION_NAME, false, false, -1);
    }


    @Test
    public void continueTrace() throws RDebuggerException {
    /*
    abc() {
      `x + 1` with `continue trace`
    }
    */

        final ContinueTraceRExecutor executor = new ContinueTraceRExecutor();
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                new MockRFunctionDebuggerFactory(null),
                new IllegalRFunctionDebuggerHandler(),
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(2, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_dbg_at", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(7, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(
                Arrays.asList("error_continue", "error_entry", "error_entry", "error_dbg_at", LS_FUNCTIONS_ERROR),
                receiver.getErrors()
        );

        receiver.reset();
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation("abc", -1), debugger.getLocation());
        assertEquals("[1] 4 5 6", debugger.getResult());
        assertEquals(8, executor.getCounter());
        assertEquals(Collections.singletonList("[1] 4 5 6"), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
    }


    @Test
    public void braceLoop() throws RDebuggerException {
    /*
    for (i in 1:2) { ... }
    */

        braceLoop("");
    }


    @Test
    public void braceLoopWithOutputBefore() throws RDebuggerException {
    /*
    print(1)
    for (i in 1:2) { ... }
    */

        braceLoop("[1] 1 2 3\n[4] 4 5 6\n");
    }


    @Test
    public void braceLoopWithFunction() throws RDebuggerException {
    /*
    for (i in 1:2) { d(i) }
    */

        final BraceLoopWithFunctionRExecutor executor = new BraceLoopWithFunctionRExecutor();
        final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(new IllegalRFunctionDebugger());
        final BraceLoopWithFunctionRFunctionDebuggerHandler handler = new BraceLoopWithFunctionRFunctionDebuggerHandler();
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                factory,
                handler,
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(3, executor.getCounter());
        assertEquals(0, factory.getCounter());
        assertEquals(0, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_ent1", "error_ent2", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 1), debugger.getLocation());
        assertEquals(5, executor.getCounter());
        assertEquals(0, factory.getCounter());
        assertEquals(0, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_body", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 1), debugger.getLocation());
        assertEquals(6, executor.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(1, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_dbg_in"), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 1), debugger.getLocation());
        assertEquals(8, executor.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(1, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_body", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 1), debugger.getLocation());
        assertEquals(9, executor.getCounter());
        assertEquals(2, factory.getCounter());
        assertEquals(2, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_dbg_in"), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation("abc", -1), debugger.getLocation());
        assertEquals(10, executor.getCounter());
        assertEquals(2, factory.getCounter());
        assertEquals(2, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
    }


    @Test
    public void unbraceLoop() throws RDebuggerException {
    /*
    for (i in 1:2) ...
    */

        unbraceLoop("");
    }


    @Test
    public void unbraceLoopWithOutputBefore() throws RDebuggerException {
    /*
    print(1)
    for (i in 1:2) ...
    */

        unbraceLoop("[1] 1 2 3\n[4] 4 5 6\n");
    }


    @Test
    public void unbraceLoopWithFunction() throws RDebuggerException {
    /*
    for (i in 1:2) d(i)
    */

        final UnbraceLoopWithFunctionRExecutor executor = new UnbraceLoopWithFunctionRExecutor();
        final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(new IllegalRFunctionDebugger());
        final UnbraceLoopWithFunctionRFunctionDebuggerHandler handler = new UnbraceLoopWithFunctionRFunctionDebuggerHandler();
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                factory,
                handler,
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(2, executor.getCounter());
        assertEquals(0, factory.getCounter());
        assertEquals(0, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_ent", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        // debugger handles `DEBUGGING_IN`,
        // `d` iterations run with `CONTINUE_TRACE` between them

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(3, executor.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(1, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_dbg_in"), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation("abc", -1), debugger.getLocation());
        assertEquals(4, executor.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(1, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
    }


    @Test
    public void exitingFromWithBraceLoopAfter() throws RDebuggerException {
    /*
    abc() {
      print(1)
    }
    for (i in 1:2) { ... }
    */

        exitingFromWithLoopAfter(true);
    }


    @Test
    public void exitingFromWithUnbraceLoopAfter() throws RDebuggerException {
    /*
    abc() {
      print(1)
    }
    for (i in 1:2) ...
    */

        exitingFromWithLoopAfter(false);
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

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                new MockRFunctionDebuggerFactory(null),
                new IllegalRFunctionDebuggerHandler(),
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 1), debugger.getLocation());
        assertEquals(2, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error1", LS_FUNCTIONS_ERROR), receiver.getErrors());

        debugger.advance();
    }


    private void braceLoop(@NotNull final String outputBefore) throws RDebuggerException {
        final BraceLoopRExecutor executor = new BraceLoopRExecutor(outputBefore);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                new MockRFunctionDebuggerFactory(null),
                new IllegalRFunctionDebuggerHandler(),
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(3, executor.getCounter());
        assertEquals(outputBefore.isEmpty() ? Collections.emptyList() : Collections.singletonList(outputBefore), receiver.getOutputs());
        assertEquals(Arrays.asList("error_ent1", "error_ent2", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 1), debugger.getLocation());
        assertEquals(5, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_body", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(7, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_ent2", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 1), debugger.getLocation());
        assertEquals(9, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_body", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation("abc", -1), debugger.getLocation());
        assertEquals(10, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
    }


    private void unbraceLoop(@NotNull final String outputBefore) throws RDebuggerException {
        final UnbraceLoopRExecutor executor = new UnbraceLoopRExecutor(outputBefore);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                new MockRFunctionDebuggerFactory(null),
                new IllegalRFunctionDebuggerHandler(),
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(2, executor.getCounter());
        assertEquals(outputBefore.isEmpty() ? Collections.emptyList() : Collections.singletonList(outputBefore), receiver.getOutputs());
        assertEquals(Arrays.asList("error_ent1", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation("abc", -1), debugger.getLocation());
        assertEquals(3, executor.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());
    }


    private void exitingFromWithLoopAfter(final boolean isBrace) throws RDebuggerException {
        final ExitingFromWithLoopAfterRExecutor executor = new ExitingFromWithLoopAfterRExecutor(isBrace);
        final ExitingFromWithLoopAfterRFunctionDebuggerHandler handler = new ExitingFromWithLoopAfterRFunctionDebuggerHandler();
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                new MockRFunctionDebuggerFactory(null),
                handler,
                receiver,
                "abc"
        );

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation("abc", 0), debugger.getLocation());
        assertEquals(2, executor.getCounter());
        assertEquals(0, handler.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_body", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation("abc", -1), debugger.getLocation());
        assertEquals(isBrace ? 4 : 3, executor.getCounter());
        assertEquals(2, handler.getCounter());
        assertEquals(Collections.singletonList("[1] 1"), receiver.getOutputs());
        assertEquals(isBrace ? Arrays.asList("error_exit", "error_loop") : Collections.singletonList("error_exit"), receiver.getErrors());
    }


    private void exiting(@NotNull final RExecutionResult firstResult,
                         @NotNull final RExecutionResult thirdResult,
                         @NotNull final String functionName,
                         final boolean output,
                         final boolean recursive,
                         final int returnLineNumber)
            throws RDebuggerException {
        final RExecutor executor = MockitoUtils.setupExecutor(
                new ContainerUtil.ImmutableMapBuilder<String, List<RExecutionResult>>()
                        .put(EXECUTE_AND_STEP_COMMAND, Arrays.asList(firstResult, thirdResult))
                        .put(RTraceAndDebugUtilsTest.LS_FUNCTIONS_COMMAND, Collections.singletonList(RTraceAndDebugUtilsTest.NO_FUNCTIONS_RESULT))
                        .build()
        );

        final RFunctionDebuggerHandler handler = mock(RFunctionDebuggerHandler.class);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RBraceFunctionDebugger debugger = new RBraceFunctionDebugger(
                executor,
                new MockRFunctionDebuggerFactory(null),
                handler,
                receiver,
                functionName
        );

        final List<String> currentCommands =
                new ArrayList<String>(Arrays.asList(EXECUTE_AND_STEP_COMMAND, RTraceAndDebugUtilsTest.LS_FUNCTIONS_COMMAND));

        assertTrue(debugger.hasNext());
        assertEquals(new RLocation(functionName, 0), debugger.getLocation());
        MockitoUtils.verifyExecutor(executor, currentCommands);
        verifyZeroInteractions(handler);
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Arrays.asList("error_dbg_at", LS_FUNCTIONS_ERROR), receiver.getErrors());

        receiver.reset();
        currentCommands.add(EXECUTE_AND_STEP_COMMAND);
        debugger.advance();

        assertFalse(debugger.hasNext());
        assertEquals(new RLocation(functionName, -1), debugger.getLocation());
        assertEquals("[1] 1 2 3", debugger.getResult());
        MockitoUtils.verifyExecutor(executor, currentCommands);
        if (recursive) verify(handler, times(1)).setDropFrames(3);
        if (returnLineNumber != -1) verify(handler, times(1)).setReturnLineNumber(returnLineNumber);
        assertEquals(output ? Collections.singletonList("[1] 1 2 3") : Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList("error_exit"), receiver.getErrors());

        verifyNoMoreInteractions(executor, handler);
    }


    private static class OrdinaryRExecutor extends MockRExecutor {

        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 1) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "1: print(c(1))\n" +
                                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_dbg_at_1"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 3) {
                return new RExecutionResult(
                        "[1] 1 2 3\n" +
                                DEBUG_AT_LINE_PREFIX + "2: c(1) + 1\n" +
                                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        new TextRange(0, 9),
                        "error_dbg_at_2"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 5) {
                return new RExecutionResult(
                        EXITING_FROM_PREFIX + "abc()\n" +
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
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 1) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "1: def()\n" +
                                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_dbg_at_1"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 3) {
                return new RExecutionResult(
                        DEBUGGING_IN_PREFIX + "def()\n" +
                                "debug: {\n" +
                                "    .doTrace(" + SERVICE_FUNCTION_PREFIX + "def" + SERVICE_ENTER_FUNCTION_SUFFIX + "(), \"on entry\")\n" +
                                "    {\n" +
                                "        print(\"x\")\n" +
                                "    }\n" +
                                "}\n" +
                                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUGGING_IN,
                        TextRange.EMPTY_RANGE,
                        "error_debugging"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 4) {
                return new RExecutionResult(
                        EXITING_FROM_PREFIX + "abc()\n" +
                                "[1] 1 2 3\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.EXITING_FROM,
                        new TextRange(20, 29),
                        "error_exit"
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
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && (getCounter() == 1 || getCounter() == 6)) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "1: c(1:3)\n" +
                                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_dbg_at"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 3) {
                return new RExecutionResult(
                        EXITING_FROM_PREFIX + "abc()\n" +
                                "[1] 1 2 3\n" +
                                DEBUGGING_IN_PREFIX + "abc()\n" +
                                "debug: {\n" +
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

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 4) {
                return new RExecutionResult(
                        "output",
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_entry"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 5) {
                return new RExecutionResult(
                        TRACING_PREFIX + "abc() on entry \n" +
                                "[1] \"abc\"\n" +
                                "debug: {\n" +
                                "    c(4:6)\n" +
                                "}\n" +
                                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                        RExecutionResultType.START_TRACE_BRACE,
                        TextRange.EMPTY_RANGE,
                        "error_entry"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 8) {
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


    private static class BraceLoopRExecutor extends MockRExecutor {

        @NotNull
        private final String myOutputBefore;


        public BraceLoopRExecutor(@NotNull final String outputBefore) {
            myOutputBefore = outputBefore;
        }


        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 1) {
                return new RExecutionResult(
                        myOutputBefore +
                                DEBUG_AT_LINE_PREFIX + "1: for (i in 1:2) {\n" +
                                "ls()\n" +
                                "}\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        myOutputBefore.isEmpty() ? TextRange.EMPTY_RANGE : new TextRange(0, myOutputBefore.length()),
                        "error_ent1"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && (getCounter() == 2 || getCounter() == 6)) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "1: i\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_ent2"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && (getCounter() == 4 || getCounter() == 8)) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "2: ls()\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_body"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 10) {
                return new RExecutionResult(
                        EXITING_FROM_PREFIX + "abc()\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.EXITING_FROM,
                        TextRange.EMPTY_RANGE,
                        "error_exit"
                );
            }

            throw new IllegalStateException("Unexpected command");
        }
    }


    private static class BraceLoopWithFunctionRExecutor extends MockRExecutor {

        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 1) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "1: for (i in 1:2) {\n" +
                                "    d(i)\n" +
                                "}\n" +
                                BROWSE_PREFIX + "2" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_ent1"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 2) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "1: i\n" +
                                BROWSE_PREFIX + "2" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_ent2"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && (getCounter() == 4 || getCounter() == 7)) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "2: d(i)\n" +
                                BROWSE_PREFIX + "2" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_body"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && (getCounter() == 6 || getCounter() == 9)) {
                return new RExecutionResult(
                        DEBUGGING_IN_PREFIX + "d(i)\n" +
                                DEBUG_AT_PREFIX + "print(i)\n" +
                                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUGGING_IN,
                        TextRange.EMPTY_RANGE,
                        "error_dbg_in"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 10) {
                return new RExecutionResult(
                        EXITING_FROM_PREFIX + "f()\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.EXITING_FROM,
                        TextRange.EMPTY_RANGE,
                        "error_exit"
                );
            }

            throw new IllegalStateException("Unexpected command");
        }
    }


    private static class BraceLoopWithFunctionRFunctionDebuggerHandler extends IllegalRFunctionDebuggerHandler {

        private int myCounter = 0;


        @Override
        public void appendDebugger(@NotNull final RFunctionDebugger debugger) {
            myCounter++;
        }


        public int getCounter() {
            return myCounter;
        }
    }


    private static class UnbraceLoopRExecutor extends MockRExecutor {

        @NotNull
        private final String myOutputBefore;


        public UnbraceLoopRExecutor(@NotNull final String outputBefore) {
            myOutputBefore = outputBefore;
        }


        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 1) {
                return new RExecutionResult(
                        myOutputBefore +
                                DEBUG_AT_LINE_PREFIX + "1: for (i in 1:2) print(i)\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        myOutputBefore.isEmpty() ? TextRange.EMPTY_RANGE : new TextRange(0, myOutputBefore.length()),
                        "error_ent1"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 3) {
                return new RExecutionResult(
                        EXITING_FROM_PREFIX + "abc()\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.EXITING_FROM,
                        TextRange.EMPTY_RANGE,
                        "error_exit"
                );
            }

            throw new IllegalStateException("Unexpected command");
        }
    }


    private static class UnbraceLoopWithFunctionRExecutor extends MockRExecutor {

        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 1) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "1: for (i in 1:2) d(i)",
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_ent"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 3) {
                return new RExecutionResult(
                        DEBUGGING_IN_PREFIX + "d(i)\n" +
                                DEBUG_AT_PREFIX + "print(i)\n" +
                                BROWSE_PREFIX + "4" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUGGING_IN,
                        TextRange.EMPTY_RANGE,
                        "error_dbg_in"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 4) {
                return new RExecutionResult(
                        EXITING_FROM_PREFIX + "f()\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.EXITING_FROM,
                        TextRange.EMPTY_RANGE,
                        "error_exit"
                );
            }

            throw new IllegalStateException("Unexpected command");
        }
    }


    private static class UnbraceLoopWithFunctionRFunctionDebuggerHandler extends IllegalRFunctionDebuggerHandler {

        private int myCounter = 0;


        @Override
        public void appendDebugger(@NotNull final RFunctionDebugger debugger) {
            myCounter++;
        }


        public int getCounter() {
            return myCounter;
        }
    }


    private static class ExitingFromWithLoopAfterRExecutor extends MockRExecutor {

        private final boolean myIsBrace;


        public ExitingFromWithLoopAfterRExecutor(final boolean isBrace) {
            myIsBrace = isBrace;
        }


        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 1) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "1: print(1)\n" +
                                BROWSE_PREFIX + "3" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_body"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 3) {
                final String debugAt = myIsBrace
                        ?
                        DEBUG_AT_LINE_PREFIX + "3: for (i in 1:2) {\n" +
                                "    print(i)\n" +
                                "}"
                        :
                        DEBUG_AT_LINE_PREFIX + "3: for (i in 1:2) print(i)";

                return new RExecutionResult(
                        "[1] 1\n" +
                                EXITING_FROM_PREFIX + "d()\n" +
                                debugAt + "\n" +
                                BROWSE_PREFIX + "2" + BROWSE_SUFFIX,
                        RExecutionResultType.EXITING_FROM,
                        new TextRange(0, 5),
                        "error_exit"
                );
            }

            if (myIsBrace && command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 4) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "3: i\n" +
                                BROWSE_PREFIX + "2" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_loop"
                );
            }

            throw new IllegalStateException("Unexpected command");
        }
    }


    private static class ExitingFromWithLoopAfterRFunctionDebuggerHandler extends IllegalRFunctionDebuggerHandler {

        private int myCounter = 0;


        @Override
        public void setReturnLineNumber(final int lineNumber) {
            myCounter += lineNumber;
        }


        public int getCounter() {
            return myCounter;
        }
    }


    private static class ErrorRExecutor extends MockRExecutor {

        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 1) {
                return new RExecutionResult(
                        DEBUG_AT_LINE_PREFIX + "2: if (10 > log(-1)) {\n" +
                                "    print(\"ok\")\n" +
                                "}\n" +
                                BROWSE_PREFIX + "1" + BROWSE_SUFFIX,
                        RExecutionResultType.DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error1"
                );
            }

            if (command.equals(EXECUTE_AND_STEP_COMMAND) && getCounter() == 3) {
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
}