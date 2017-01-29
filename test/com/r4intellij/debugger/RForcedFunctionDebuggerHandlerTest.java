package com.r4intellij.debugger;

import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.mock.MockRExecutor;
import com.r4intellij.debugger.mock.MockRFunctionDebugger;
import com.r4intellij.debugger.mock.MockRFunctionDebuggerFactory;
import com.r4intellij.debugger.mock.MockROutputReceiver;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.util.Collections;

import static org.junit.Assert.assertEquals;

public class RForcedFunctionDebuggerHandlerTest {

    @Test
    public void stack1() throws RDebuggerException {
    /*
    def() {
      instruction1
    }
    */

        final String result = "[1] 1 2 3";

        final Stack1RFunctionDebugger debugger = new Stack1RFunctionDebugger();
        final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(debugger);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RForcedFunctionDebuggerHandler handler = new RForcedFunctionDebuggerHandler(
                new IllegalRExecutor(),
                factory,
                receiver
        );

        //noinspection StatementWithEmptyBody
        while (handler.advance()) {
        }

        assertEquals(1, debugger.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.emptyList(), receiver.getErrors());
        assertEquals(result, handler.getResult());
    }


    @Test
    public void stack21() throws RDebuggerException {
    /*
    def() {
      instruction1
      abc() {
        instruction1
        instruction2
      }
      instruction2
    }
    */

        final String result = "[1] 1 2 3";

        final MockRFunctionDebugger secondFunctionDebugger = new MockRFunctionDebugger("abc", 2, null);
        final MockRFunctionDebugger firstFunctionDebugger = new Stack211RFunctionDebugger(secondFunctionDebugger);
        final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(firstFunctionDebugger);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RForcedFunctionDebuggerHandler handler = new RForcedFunctionDebuggerHandler(
                new IllegalRExecutor(),
                factory,
                receiver
        );

        //noinspection StatementWithEmptyBody
        while (handler.advance()) {
        }

        assertEquals(2, secondFunctionDebugger.getCounter());
        assertEquals(3, firstFunctionDebugger.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.emptyList(), receiver.getErrors());
        assertEquals(result, handler.getResult());
    }


    @Test
    public void stack22() throws RDebuggerException {
    /*
    def() {
      instruction1
      abc() {
        instruction1
        instruction2
      }
    }
    */

        final String result = "[1] 1 2 3";

        final MockRFunctionDebugger secondFunctionDebugger = new Stack222RFunctionDebugger();
        final MockRFunctionDebugger firstFunctionDebugger = new Stack221RFunctionDebugger(secondFunctionDebugger);
        final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(firstFunctionDebugger);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RForcedFunctionDebuggerHandler handler = new RForcedFunctionDebuggerHandler(
                new IllegalRExecutor(),
                factory,
                receiver
        );

        //noinspection StatementWithEmptyBody
        while (handler.advance()) {
        }

        assertEquals(2, secondFunctionDebugger.getCounter());
        assertEquals(2, firstFunctionDebugger.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.emptyList(), receiver.getErrors());
        assertEquals(result, handler.getResult());
    }


    @Test
    public void stack3() throws RDebuggerException {
    /*
    def() {
      instruction1
      abc() {
        instruction1
        ghi() {
          instruction1
          instruction2
        }
      }
      instruction2
    }
    */

        final String result = "[1] 1 2 3";

        final MockRFunctionDebugger thirdFunctionDebugger = new Stack33RFunctionDebugger();
        final MockRFunctionDebugger secondFunctionDebugger = new Stack32RFunctionDebugger(thirdFunctionDebugger);
        final MockRFunctionDebugger firstFunctionDebugger = new Stack31RFunctionDebugger(secondFunctionDebugger);
        final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(firstFunctionDebugger);
        final MockROutputReceiver receiver = new MockROutputReceiver();

        final RForcedFunctionDebuggerHandler handler = new RForcedFunctionDebuggerHandler(
                new IllegalRExecutor(),
                factory,
                receiver
        );

        //noinspection StatementWithEmptyBody
        while (handler.advance()) {
        }

        assertEquals(2, thirdFunctionDebugger.getCounter());
        assertEquals(2, secondFunctionDebugger.getCounter());
        assertEquals(3, firstFunctionDebugger.getCounter());
        assertEquals(1, factory.getCounter());
        assertEquals(Collections.emptyList(), receiver.getOutputs());
        assertEquals(Collections.emptyList(), receiver.getErrors());
        assertEquals(result, handler.getResult());
    }


    private static class IllegalRExecutor extends MockRExecutor {

        @NotNull
        @Override
        protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
            throw new IllegalStateException("DoExecute shouldn't be called");
        }
    }


    private static class Stack1RFunctionDebugger extends MockRFunctionDebugger {

        public Stack1RFunctionDebugger() {
            super("", 1, null);
        }


        @NotNull
        @Override
        public RLocation getLocation() {
            throw new IllegalStateException("GetLocation shouldn't be called");
        }


        @NotNull
        @Override
        public String getResult() {
            return "[1] 1 2 3";
        }
    }


    private static class Stack211RFunctionDebugger extends MockRFunctionDebugger {

        @NotNull
        private final MockRFunctionDebugger myNextFunctionDebugger;


        public Stack211RFunctionDebugger(@NotNull final MockRFunctionDebugger debugger) {
            super("def", 3, null);

            myNextFunctionDebugger = debugger;
        }


        @NotNull
        @Override
        public RLocation getLocation() {
            throw new IllegalStateException("GetLocation shouldn't be called");
        }


        @Override
        public void advance() throws RDebuggerException {
            super.advance();

            if (getCounter() == 2) {
                assert getHandler() != null;

                getHandler().appendDebugger(myNextFunctionDebugger);
            }
        }


        @NotNull
        @Override
        public String getResult() {
            return "[1] 1 2 3";
        }
    }


    private static class Stack221RFunctionDebugger extends MockRFunctionDebugger {

        @NotNull
        private final MockRFunctionDebugger myNextFunctionDebugger;


        public Stack221RFunctionDebugger(@NotNull final MockRFunctionDebugger debugger) {
            super("def", 2, null);

            myNextFunctionDebugger = debugger;
        }


        @NotNull
        @Override
        public RLocation getLocation() {
            throw new IllegalStateException("GetLocation shouldn't be called");
        }


        @Override
        public void advance() throws RDebuggerException {
            super.advance();

            if (getCounter() == 2) {
                assert getHandler() != null;

                myNextFunctionDebugger.setHandler(getHandler());
                getHandler().appendDebugger(myNextFunctionDebugger);
            }
        }
    }


    private static class Stack222RFunctionDebugger extends MockRFunctionDebugger {

        public Stack222RFunctionDebugger() {
            super("abc", 2, null);
        }


        @NotNull
        @Override
        public RLocation getLocation() {
            throw new IllegalStateException("GetLocation shouldn't be called");
        }


        @Override
        public void advance() throws RDebuggerException {
            super.advance();

            if (getCounter() == 2) {
                assert getHandler() != null;

                getHandler().setDropFrames(2);
            }
        }


        @NotNull
        @Override
        public String getResult() {
            return "[1] 1 2 3";
        }
    }


    private static class Stack31RFunctionDebugger extends MockRFunctionDebugger {

        @NotNull
        private final MockRFunctionDebugger myNextFunctionDebugger;


        public Stack31RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
            super("def", 3, null);

            myNextFunctionDebugger = nextFunctionDebugger;
        }


        @Override
        public void advance() throws RDebuggerException {
            super.advance();

            if (getCounter() == 2) {
                assert getHandler() != null;

                myNextFunctionDebugger.setHandler(getHandler());
                getHandler().appendDebugger(myNextFunctionDebugger);
            }
        }


        @NotNull
        @Override
        public String getResult() {
            return "[1] 1 2 3";
        }
    }


    private static class Stack32RFunctionDebugger extends MockRFunctionDebugger {

        @NotNull
        private final MockRFunctionDebugger myNextFunctionDebugger;


        public Stack32RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
            super("abc", 2, null);

            myNextFunctionDebugger = nextFunctionDebugger;
        }


        @Override
        public void advance() throws RDebuggerException {
            super.advance();

            if (getCounter() == 2) {
                assert getHandler() != null;

                myNextFunctionDebugger.setHandler(getHandler());
                getHandler().appendDebugger(myNextFunctionDebugger);
            }
        }
    }


    private static class Stack33RFunctionDebugger extends MockRFunctionDebugger {

        public Stack33RFunctionDebugger() {
            super("ghi", 2, null);
        }


        @Override
        public void advance() throws RDebuggerException {
            super.advance();

            if (getCounter() == 2) {
                assert getHandler() != null;

                getHandler().setDropFrames(2);
            }
        }
    }
}