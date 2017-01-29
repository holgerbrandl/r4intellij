package com.r4intellij.debugger.executor;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.mock.AlwaysSameResultRExecutor;
import com.r4intellij.debugger.mock.MockROutputReceiver;
import org.junit.Test;

import java.util.Collections;

import static com.r4intellij.debugger.executor.RExecutionResultType.PLUS;
import static com.r4intellij.debugger.executor.RExecutionResultType.RESPONSE;
import static com.r4intellij.debugger.executor.RExecutorUtils.execute;
import static org.junit.Assert.assertEquals;

public class RExecutorUtilsTest {

    @Test(expected = RDebuggerException.class)
    public void invalidCommandExecuting() throws RDebuggerException {
        final String output = "abc";
        final RExecutionResultType type = RESPONSE;
        final TextRange resultRange = TextRange.allOf(output);
        final String error = "";

        final RExecutor executor = new AlwaysSameResultRExecutor(output, type, resultRange, error);

        execute(executor, "def", PLUS);
    }


    @Test
    public void correctCommandExecuting() throws RDebuggerException {
        final String output = "abc";
        final RExecutionResultType type = RESPONSE;
        final TextRange resultRange = TextRange.allOf(output);
        final String error = "";

        final RExecutor executor = new AlwaysSameResultRExecutor(output, type, resultRange, error);

        final RExecutionResult result = execute(executor, "def", RESPONSE);

        assertEquals(output, result.getOutput());
        assertEquals(RESPONSE, result.getType());
        assertEquals(resultRange, result.getResultRange());
        assertEquals(error, result.getError());
    }


    @Test
    public void errorCommandExecuting1() throws RDebuggerException {
        final String output = "abc";
        final RExecutionResultType type = RESPONSE;
        final TextRange resultRange = TextRange.allOf(output);
        final String error = "error";

        final RExecutor executor = new AlwaysSameResultRExecutor(output, type, resultRange, error);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final String result = execute(executor, "def", RESPONSE, receiver);

        assertEquals(output, result);
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList(error), receiver.getErrors());
    }


    @Test
    public void errorCommandExecuting2() throws RDebuggerException {
        final String output = "abc";
        final RExecutionResultType type = RESPONSE;
        final TextRange resultRange = TextRange.allOf(output);
        final String error = "error";

        final RExecutor executor = new AlwaysSameResultRExecutor(output, type, resultRange, error);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RExecutionResult result = execute(executor, "def", receiver);

        assertEquals(output, result.getOutput());
        assertEquals(RESPONSE, result.getType());
        assertEquals(resultRange, result.getResultRange());
        assertEquals(error, result.getError());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.singletonList(error), receiver.getErrors());
    }
}