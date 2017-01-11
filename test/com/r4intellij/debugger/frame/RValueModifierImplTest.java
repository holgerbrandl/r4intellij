package com.r4intellij.debugger.frame;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.mock.*;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.util.Collections;

import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_ENTER_FUNCTION_SUFFIX;
import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_FUNCTION_PREFIX;
import static com.r4intellij.debugger.data.RResponseConstants.*;
import static com.r4intellij.debugger.executor.RExecutionResultType.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class RValueModifierImplTest {

  @Test
  public void illegal() {
    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      "text",
      RExecutionResultType.RESPONSE,
      TextRange.allOf("text"),
      ""
    );

    final AlwaysSameResponseHandler handler = new AlwaysSameResponseHandler(false);

    final RValueModifierImpl modifier = new RValueModifierImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalROutputReceiver(),
      handler,
      0
    );

    try {
      modifier.setValue("name", "value", new IllegalListener());

      fail();
    }
    catch (final IllegalStateException ignored) {
    }

    assertEquals(0, executor.getCounter());
    assertEquals(1, handler.myCounter);
  }

  @Test
  public void unexpectedResultType() {
    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      "text",
      RExecutionResultType.RESPONSE,
      TextRange.allOf("text"),
      "error"
    );

    final MockROutputReceiver receiver = new MockROutputReceiver();
    final AlwaysSameResponseHandler handler = new AlwaysSameResponseHandler(true);

    final RValueModifierImpl modifier = new RValueModifierImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      receiver,
      handler,
      0
    );

    final ExceptionListener listener = new ExceptionListener();

    modifier.setValue("name", "value", listener);

    assertEquals(1, executor.getCounter());
    assertEquals(Collections.singletonList("error"), receiver.getErrors());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(1, handler.myCounter);
    assertEquals(1, listener.myCounter);
  }

  @Test
  public void errorDuringExecution() {
    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      "",
      RExecutionResultType.EMPTY,
      TextRange.EMPTY_RANGE,
      "error"
    );

    final MockROutputReceiver receiver = new MockROutputReceiver();
    final AlwaysSameResponseHandler handler = new AlwaysSameResponseHandler(true);
    final ErrorListener listener = new ErrorListener();

    final RValueModifierImpl modifier = new RValueModifierImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      receiver,
      handler,
      0
    );

    modifier.setValue("name", "value", listener);

    assertEquals(1, executor.getCounter());
    assertEquals(Collections.singletonList("error"), receiver.getErrors());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(1, handler.myCounter);
    assertEquals(1, listener.myCounter);
  }

  @Test
  public void exceptionDuringExecution() {
    final ExceptionDuringExecutionRExecutor executor = new ExceptionDuringExecutionRExecutor();
    final AlwaysSameResponseHandler handler = new AlwaysSameResponseHandler(true);
    final ExceptionListener listener = new ExceptionListener();

    final RValueModifierImpl modifier = new RValueModifierImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalROutputReceiver(),
      handler,
      0
    );

    modifier.setValue("name", "value", listener);

    assertEquals(1, executor.getCounter());
    assertEquals(1, handler.myCounter);
    assertEquals(1, listener.myCounter);
  }

  @Test
  public void expression() {
    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      "",
      RExecutionResultType.EMPTY,
      TextRange.EMPTY_RANGE,
      ""
    );

    final AlwaysSameResponseHandler handler = new AlwaysSameResponseHandler(true);
    final SuccessListener listener = new SuccessListener();

    final RValueModifierImpl modifier = new RValueModifierImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      new IllegalROutputReceiver(),
      handler,
      0
    );

    modifier.setValue("name", "value", listener);

    assertEquals(1, executor.getCounter());
    assertEquals(1, handler.myCounter);
    assertEquals(1, listener.myCounter);
  }

  @Test
  public void inDebugExpression() {
    final InDebugRExecutor executor = new InDebugRExecutor();

    final AlwaysSameResponseHandler handler = new AlwaysSameResponseHandler(true);
    final MockROutputReceiver receiver = new MockROutputReceiver();
    final SuccessListener listener = new SuccessListener();

    final RValueModifierImpl modifier = new RValueModifierImpl(
      executor,
      new MockRFunctionDebuggerFactory(null),
      receiver,
      handler,
      0
    );

    modifier.setValue("name", "value", listener);

    assertEquals(2, executor.getCounter());
    assertEquals(Collections.singletonList("abc"), receiver.getErrors());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(1, handler.myCounter);
    assertEquals(1, listener.myCounter);
  }

  @Test
  public void function() {
    final String error = "error";

    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(
      DEBUGGING_IN_PREFIX + "def(c(1:5))\n" +
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

    final MockRFunctionDebugger debugger = new MockRFunctionDebugger("def", 2, "result");
    final MockRFunctionDebuggerFactory factory = new MockRFunctionDebuggerFactory(debugger);
    final MockROutputReceiver receiver = new MockROutputReceiver();
    final AlwaysSameResponseHandler handler = new AlwaysSameResponseHandler(true);
    final SuccessListener listener = new SuccessListener();

    final RValueModifierImpl modifier = new RValueModifierImpl(
      executor,
      factory,
      receiver,
      handler,
      0
    );

    modifier.setValue("name", "value", listener);

    assertEquals(1, executor.getCounter());
    assertEquals(2, debugger.getCounter());
    assertEquals(1, factory.getCounter());
    assertEquals(Collections.singletonList(error), receiver.getErrors());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(1, handler.myCounter);
    assertEquals(1, listener.myCounter);
  }

  private static class AlwaysSameResponseHandler extends IllegalRValueModifierHandler {

    private final boolean myResponse;

    private int myCounter = 0;

    public AlwaysSameResponseHandler(final boolean response) {
      myResponse = response;
    }

    @Override
    public boolean isModificationAvailable(final int frameNumber) {
      myCounter++;

      return myResponse;
    }
  }

  private static class IllegalListener implements RValueModifier.Listener {

    @Override
    public void onSuccess() {
      throw new IllegalStateException("OnSuccess shouldn't be called");
    }

    @Override
    public void onError(@NotNull final String error) {
      throw new IllegalStateException("OnError shouldn't be called");
    }

    @Override
    public void onError(@NotNull final Exception e) {
      throw new IllegalStateException("OnError shouldn't be called");
    }
  }

  private static class ErrorListener extends IllegalListener {

    private int myCounter = 0;

    @Override
    public void onError(@NotNull final String error) {
      myCounter++;
    }
  }

  private static class ExceptionDuringExecutionRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      throw new RDebuggerException("");
    }
  }

  private static class ExceptionListener extends IllegalListener {

    private int myCounter = 0;

    @Override
    public void onError(@NotNull final Exception e) {
      myCounter++;
    }
  }

  private static class SuccessListener extends IllegalListener {

    private int myCounter = 0;

    @Override
    public void onSuccess() {
      myCounter++;
    }
  }

  private static class InDebugRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (getCounter() == 1) {
        return new RExecutionResult(
          DEBUG_AT_LINE_PREFIX + "2: x <- c(1:10)",
          DEBUG_AT,
          TextRange.EMPTY_RANGE,
          "abc"
        );
      }

      if (getCounter() == 2) {
        return new RExecutionResult(
          "",
          RESPONSE,
          TextRange.EMPTY_RANGE,
          ""
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }
}