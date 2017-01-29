package com.r4intellij.run.debug.stack;

import com.intellij.xdebugger.frame.XExecutionStack;
import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.frame.RStackFrame;
import com.r4intellij.debugger.mock.IllegalRDebuggerEvaluator;
import com.r4intellij.debugger.mock.IllegalRVarsLoader;
import com.r4intellij.run.debug.mock.ExecutorServices;
import com.r4intellij.run.debug.mock.MockXStackFrameContainer;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static org.junit.Assert.assertEquals;

public class RXSuspendContextTest {

    @Test
    public void ordinary() {
        final RXStackFrame first = new RXStackFrame(
                new RStackFrame(
                        new RLocation("abc", 2),
                        new IllegalRVarsLoader(),
                        new IllegalRDebuggerEvaluator()
                ),
                null,
                ExecutorServices.ILLEGAL_EXECUTOR
        );

        final RXStackFrame second = new RXStackFrame(
                new RStackFrame(
                        new RLocation("def", 1),
                        new IllegalRVarsLoader(),
                        new IllegalRDebuggerEvaluator()
                ),
                null,
                ExecutorServices.ILLEGAL_EXECUTOR
        );

        final RXSuspendContext context = new RXSuspendContext(Arrays.asList(first, second));
        final XExecutionStack stack = context.getActiveExecutionStack();
        final MockXStackFrameContainer container = new MockXStackFrameContainer();

        stack.computeStackFrames(1, container);
        assertEquals(Collections.singletonList(second), container.getResult());
        assertEquals(first, stack.getTopFrame());
    }
}