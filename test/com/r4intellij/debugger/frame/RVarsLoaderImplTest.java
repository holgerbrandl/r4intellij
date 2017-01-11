package com.r4intellij.debugger.frame;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.mock.AlwaysSameResultRExecutor;
import com.r4intellij.debugger.mock.IllegalRValueModifier;
import com.r4intellij.debugger.mock.MockRExecutor;
import com.r4intellij.debugger.mock.MockROutputReceiver;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static com.r4intellij.debugger.data.RCommands.expressionOnFrameCommand;
import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_FUNCTION_PREFIX;
import static com.r4intellij.debugger.data.RLanguageConstants.FUNCTION_TYPE;
import static com.r4intellij.debugger.data.RResponseConstants.DEBUG_AT_LINE_PREFIX;
import static com.r4intellij.debugger.data.RResponseConstants.ENVIRONMENT_PREFIX;
import static com.r4intellij.debugger.executor.RExecutionResultType.DEBUG_AT;
import static com.r4intellij.debugger.executor.RExecutionResultType.RESPONSE;
import static com.r4intellij.debugger.mock.MockRExecutor.LS_FUNCTIONS_ERROR;
import static org.junit.Assert.assertEquals;

public class RVarsLoaderImplTest {

  @Test
  public void empty() throws RDebuggerException {
    final String output = "character(0)";
    final AlwaysSameResultRExecutor executor = new AlwaysSameResultRExecutor(output, RESPONSE, TextRange.allOf(output), "error");
    final MockROutputReceiver receiver = new MockROutputReceiver();

    assertEquals(
      0,
      new RVarsLoaderImpl(
        executor,
        receiver,
        new IllegalRValueModifier(),
        0
      ).load().size()
    );

    assertEquals(1, executor.getCounter());
    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Collections.singletonList("error"), receiver.getErrors());
  }

  @Test
  public void ordinary() throws RDebuggerException {
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final List<RVar> actual = new RVarsLoaderImpl(
      new OrdinaryRExecutor(),
      receiver,
      new IllegalRValueModifier(),
      0
    ).load();

    assertEquals(3, actual.size());

    assertEquals("a", actual.get(0).getName());
    assertEquals("[1] \"integer\"", actual.get(0).getType());
    assertEquals("[1] 1 2 3", actual.get(0).getValue());

    assertEquals("b", actual.get(1).getName());
    assertEquals(FUNCTION_TYPE, actual.get(1).getType());
    assertEquals(
      "function(x) {\n" +
      "    x ^ 2\n" +
      "}",
      actual.get(1).getValue()
    );

    assertEquals("c", actual.get(2).getName());
    assertEquals(FUNCTION_TYPE, actual.get(2).getType());
    assertEquals(
      "function(x) {\n" +
      "    x ^ 2\n" +
      "}",
      actual.get(2).getValue()
    );

    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(
      Arrays.asList(LS_FUNCTIONS_ERROR, "error_ta", "error_va", "error_t4", "error_vb", "error_t6", "error_vc", "error_t8"),
      receiver.getErrors()
    );
  }

  @Test
  public void inDebug() throws RDebuggerException {
    final MockROutputReceiver receiver = new MockROutputReceiver();

    final List<RVar> actual = new RVarsLoaderImpl(
      new InDebugRExecutor(),
      receiver,
      new IllegalRValueModifier(),
      0
    ).load();

    assertEquals(1, actual.size());
    assertEquals("a", actual.get(0).getName());
    assertEquals("[1] \"integer\"", actual.get(0).getType());
    assertEquals("[1] 1 2 3", actual.get(0).getValue());

    assertEquals(Collections.emptyList(), receiver.getOutputs());
    assertEquals(Arrays.asList(LS_FUNCTIONS_ERROR, "error_ta", "error_dbg_at", "error_va"), receiver.getErrors());
  }

  private static class OrdinaryRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (getCounter() == 1) {
        final String output = "[1] \"a\" \"b\" \"c\"\n" +
                              "[4] \"" + SERVICE_FUNCTION_PREFIX + "d" + "\"";
        // list, function, inner function and service function

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          LS_FUNCTIONS_ERROR
        );
      }

      if (getCounter() == 2) { // type of a
        final String output = "[1] \"integer\"";

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          "error_ta"
        );
      }

      if (getCounter() == 3) { // value of a
        final String output = "[1] 1 2 3";

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          "error_va"
        );
      }

      if (getCounter() == 4 || getCounter() == 6 || getCounter() == 8 || getCounter() == 9) { // type of b, c, d, e
        final String output = FUNCTION_TYPE;

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          "error_t" + getCounter()
        );
      }

      if (getCounter() == 5) { // value of b
        final String output = "function(x) {\n" +
                              "    x ^ 2\n" +
                              "}";

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          "error_vb"
        );
      }

      if (getCounter() == 7) { // value of c
        final String output = "function(x) {\n" +
                              "    x ^ 2\n" +
                              "}\n" +
                              ENVIRONMENT_PREFIX + "0xfffffff>";

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          "error_vc"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }

  private static class InDebugRExecutor extends MockRExecutor {

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (getCounter() == 1) {
        final String output = "[1] \"a\"";

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          LS_FUNCTIONS_ERROR
        );
      }

      if (getCounter() == 2) {
        final String output = "[1] \"integer\"";

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          "error_ta"
        );
      }

      if (getCounter() == 3) {
        final String output = DEBUG_AT_LINE_PREFIX + "2: print(" + expressionOnFrameCommand(0, "a") + ")";

        return new RExecutionResult(
          output,
          DEBUG_AT,
          TextRange.EMPTY_RANGE,
          "error_dbg_at"
        );
      }

      if (getCounter() == 4) {
        final String output = "[1] 1 2 3";

        return new RExecutionResult(
          output,
          RESPONSE,
          TextRange.allOf(output),
          "error_va"
        );
      }

      throw new IllegalStateException("Unexpected command");
    }
  }
}