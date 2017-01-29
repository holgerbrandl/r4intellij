package com.r4intellij.debugger.function;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.data.RResponseConstants;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RUnexpectedExecutionResultTypeException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.mock.IllegalRFunctionDebuggerHandler;
import com.r4intellij.debugger.mock.MockRExecutor;
import com.r4intellij.debugger.mock.MockROutputReceiver;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static com.r4intellij.debugger.executor.RExecutionResultType.*;
import static com.r4intellij.debugger.mock.MockRExecutor.LS_FUNCTIONS_ERROR;
import static org.junit.Assert.assertEquals;

public class RFunctionDebuggerFactoryImplTest {

    @Test
    public void braceFunction() throws RDebuggerException {
        final MockROutputReceiver outputReceiver = new MockROutputReceiver();

        final RFunctionDebugger debugger = new RFunctionDebuggerFactoryImpl().getFunctionDebugger(
                new BraceRExecutor(),
                new IllegalRFunctionDebuggerHandler(),
                outputReceiver
        );

        final RLocation expected = new RLocation("abc", 1);

        assertEquals(expected, debugger.getLocation());
        assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
        assertEquals(Arrays.asList("error_entry", "error_st", "error_dbg_at", LS_FUNCTIONS_ERROR), outputReceiver.getErrors());
    }


    @Test
    public void unbraceFunction() throws RDebuggerException {
        final MockROutputReceiver outputReceiver = new MockROutputReceiver();

        final RFunctionDebugger debugger = new RFunctionDebuggerFactoryImpl().getFunctionDebugger(
                new UnbraceRExecutor(),
                new IllegalRFunctionDebuggerHandler(),
                outputReceiver
        );

        final RLocation expected = new RLocation("abc", 0);

        assertEquals(expected, debugger.getLocation());
        assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
        assertEquals(Arrays.asList("error_entry", "error_st", LS_FUNCTIONS_ERROR), outputReceiver.getErrors());
    }


    @Test(expected = RUnexpectedExecutionResultTypeException.class)
    public void unexpectedResult() throws RDebuggerException {
        final MockROutputReceiver outputReceiver = new MockROutputReceiver();

        new RFunctionDebuggerFactoryImpl().getFunctionDebugger(
                new UnexpectedResultRExecutor(),
                new IllegalRFunctionDebuggerHandler(),
                outputReceiver
        );
    }


    private static class BraceRExecutor extends MockRExecutor {

        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (getCounter() == 1) {
                return new RExecutionResult(
                        "",
                        DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_entry"
                );
            }

            if (getCounter() == 2) {
                return new RExecutionResult(
                        RResponseConstants.TRACING_PREFIX + "abc(1) on entry\n" +
                                "[1] \"abc\"\n" +
                                "debug: {\n" +
                                "    x + 1\n" +
                                "}",
                        START_TRACE_BRACE,
                        TextRange.EMPTY_RANGE,
                        "error_st"
                );
            }

            if (getCounter() == 3) {
                return new RExecutionResult(
                        RResponseConstants.DEBUG_AT_LINE_PREFIX + "2: x + 1",
                        DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_dbg_at"
                );
            }

            throw new IllegalStateException("Unexpected command");
        }
    }


    private static class UnbraceRExecutor extends MockRExecutor {

        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (getCounter() == 1) {
                return new RExecutionResult(
                        "",
                        DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_entry"
                );
            }

            if (getCounter() == 2) {
                return new RExecutionResult(
                        RResponseConstants.TRACING_PREFIX + "abc(1) on entry\n" +
                                "[1] \"abc\"\n" +
                                "debug: x + 1",
                        START_TRACE_UNBRACE,
                        TextRange.EMPTY_RANGE,
                        "error_st"
                );
            }

            throw new IllegalStateException("Unexpected command");
        }
    }


    private static class UnexpectedResultRExecutor extends MockRExecutor {

        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            if (getCounter() < 3) {
                return new RExecutionResult(
                        "",
                        DEBUG_AT,
                        TextRange.EMPTY_RANGE,
                        "error_entry"
                );
            }

            throw new IllegalStateException("Unexpected command");
        }
    }
}