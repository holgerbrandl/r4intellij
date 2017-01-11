package com.r4intellij.debugger.evaluator;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.data.RCommands;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.mock.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;

import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_ENTER_FUNCTION_SUFFIX;
import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_FUNCTION_PREFIX;
import static com.r4intellij.debugger.data.RResponseConstants.*;
import static com.r4intellij.debugger.executor.RExecutionResultType.*;
import static org.junit.Assert.assertEquals;

public class RDebuggerEvaluatorImplTest {

  @Test
  public void unexpectedResultType() {
    final String expression = "def <- function() {";

    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      "",
      PLUS,
      TextRange.EMPTY_RANGE,
      "error"
    );
    final MockRExpressionHandler handler = new MockRExpressionHandler();

    final RDebuggerEvaluatorImpl evaluator = new RDebuggerEvaluatorImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalROutputReceiver(),
      handler,
      1
    );

    final RDebuggerEvaluatorErrorReceiver receiver = new RDebuggerEvaluatorErrorReceiver();

    evaluator.evaluate(expression, receiver);

    assertEquals(1, executor.getCounter());
    assertEquals(1, handler.myCounter);
    assertEquals(expression, handler.myLastExpression);
    assertEquals(1, receiver.getCounter());
  }

  @Test
  public void errorDuringExecution() {
    final String expression = "abc";

    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      "",
      EMPTY,
      TextRange.EMPTY_RANGE,
      "error"
    );
    final MockRExpressionHandler handler = new MockRExpressionHandler();

    final RDebuggerEvaluatorImpl evaluator = new RDebuggerEvaluatorImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalROutputReceiver(),
      handler,
      1
    );

    final RDebuggerEvaluatorErrorReceiver receiver = new RDebuggerEvaluatorErrorReceiver();

    evaluator.evaluate(expression, receiver);

    assertEquals(1, executor.getCounter());
    assertEquals(1, handler.myCounter);
    assertEquals(expression, handler.myLastExpression);
    assertEquals(1, receiver.getCounter());
  }

  @Test
  public void exceptionDuringExecution() {
    final String expression = "def";

    final ExceptionDuringExecutionRExecutor executor = new ExceptionDuringExecutionRExecutor();
    final MockRExpressionHandler handler = new MockRExpressionHandler();

    final RDebuggerEvaluatorImpl evaluator = new RDebuggerEvaluatorImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalROutputReceiver(),
      handler,
      1
    );

    final RDebuggerEvaluatorErrorReceiver receiver = new RDebuggerEvaluatorErrorReceiver();

    evaluator.evaluate(expression, receiver);

    assertEquals(1, executor.getCounter());
    assertEquals(1, handler.myCounter);
    assertEquals(expression, handler.myLastExpression);
    assertEquals(1, receiver.getCounter());
  }

  @Test
  public void expression() {
    final String expression = "def(c(1:5))";
    final String output = "[1] 1 2 3";
    final String error = "error";

    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      output,
      RESPONSE,
      TextRange.allOf(output),
      error
    );

    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler handler = new MockRExpressionHandler();

    final RDebuggerEvaluatorImpl evaluator = new RDebuggerEvaluatorImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      outputReceiver,
      handler,
      1
    );

    final RDebuggerEvaluatorReceiver receiver = new RDebuggerEvaluatorReceiver(output);

    evaluator.evaluate(expression, receiver);

    assertEquals(1, executor.getCounter());
    assertEquals(1, receiver.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList(error), outputReceiver.getErrors());
    assertEquals(1, handler.myCounter);
    assertEquals(expression, handler.myLastExpression);
  }

  @Test
  public void inDebugExpression() {
    final String expression = "def(c(1:5))";
    final String output = "[1] 1 2 3";

    final InDebugRExecutor executor = new InDebugRExecutor();
    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler handler = new MockRExpressionHandler();

    final RDebuggerEvaluatorImpl evaluator = new RDebuggerEvaluatorImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      outputReceiver,
      handler,
      1
    );

    final RDebuggerEvaluatorReceiver receiver = new RDebuggerEvaluatorReceiver(output);

    evaluator.evaluate(expression, receiver);

    assertEquals(2, executor.getCounter());
    assertEquals(1, receiver.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Arrays.asList("abc", "def"), outputReceiver.getErrors());
    assertEquals(1, handler.myCounter);
    assertEquals(expression, handler.myLastExpression);
  }

  @Test
  public void innerFunctionValue() {
    final String expression = "def";
    final String error = "error";

    final String output = "function(x) {\n" +
                          "    x ^ 2\n" +
                          "}\n" +
                          ENVIRONMENT_PREFIX + "0xfffffff>";

    final String result = "function(x) {\n" +
                          "    x ^ 2\n" +
                          "}";

    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      output,
      RESPONSE,
      TextRange.allOf(output),
      error
    );

    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler handler = new MockRExpressionHandler();

    final RDebuggerEvaluatorImpl evaluator = new RDebuggerEvaluatorImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      outputReceiver,
      handler,
      1
    );

    final RDebuggerEvaluatorReceiver receiver = new RDebuggerEvaluatorReceiver(result);

    evaluator.evaluate(expression, receiver);

    assertEquals(1, executor.getCounter());
    assertEquals(1, receiver.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList(error), outputReceiver.getErrors());
    assertEquals(1, handler.myCounter);
    assertEquals(expression, handler.myLastExpression);
  }

  @Test
  public void function() {
    final String expression = "def(c(1:5))";
    final String error = "error";
    final String result = "[1] 1 2 3";

    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      DEBUGGING_IN_PREFIX + expression + "\n" +
      DEBUG_AT_PREFIX + "{\n" +
      "    .doTrace(" + SERVICE_FUNCTION_PREFIX + "def" + SERVICE_ENTER_FUNCTION_SUFFIX + "(), \"on entry\")\n" +
      "    {\n" +
      "        print(\"x\")\n" +
      "    }\n" +
      "}",
      DEBUGGING_IN,
      TextRange.EMPTY_RANGE,
      error
    );

    final MyFunctionDebugger debugger = new MyFunctionDebugger();
    final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(debugger);
    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler handler = new MockRExpressionHandler();

    final RDebuggerEvaluatorImpl evaluator = new RDebuggerEvaluatorImpl(
      executor,
      factory,
      outputReceiver,
      handler,
      1
    );

    final RDebuggerEvaluatorReceiver receiver = new RDebuggerEvaluatorReceiver(result);

    evaluator.evaluate(expression, receiver);

    assertEquals(1, executor.getCounter());
    assertEquals(2, debugger.getCounter());
    assertEquals(1, factory.getCounter());
    assertEquals(1, receiver.getCounter());
    assertEquals(Collections.singletonList(error), outputReceiver.getErrors());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(1, handler.myCounter);
    assertEquals(expression, handler.myLastExpression);
  }

  private static class MockRExpressionHandler extends IllegalRExpressionHandler {

    @Nullable
    private String myLastExpression = null;

    private int myCounter = 0;

    @NotNull
    @Override
    public String handle(final int frameNumber, @NotNull final String expression) {
      myCounter += frameNumber;
      myLastExpression = expression;

      return expression;
    }
  }

  private static class ExceptionDuringExecutionRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      throw new RDebuggerException("");
    }
  }

  private static class InDebugRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (getCounter() == 1) {
        return new RExecutionResult(
          DEBUG_AT_LINE_PREFIX + "2: " + RCommands.expressionOnFrameCommand(0, "abc"),
          DEBUG_AT,
          TextRange.EMPTY_RANGE,
          "abc"
        );
      }

      if (getCounter() == 2) {
        return new RExecutionResult(
          "[1] 1 2 3",
          RESPONSE,
          TextRange.allOf("[1] 1 2 3"),
          "def"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }

  private static class MyFunctionDebugger extends MockRFunctionDebugger {

    public MyFunctionDebugger() {
      super("def", 2, null);
    }

    @NotNull
    @Override
    public String getResult() {
      return "[1] 1 2 3";
    }
  }
}