package com.r4intellij.run.run;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import static com.r4intellij.debugger.data.RResponseConstants.PROMPT;
import static org.junit.Assert.*;

public class RRunExecutionResultCalculatorTest {

  @NotNull
  private static final RRunExecutionResultCalculator CALCULATOR = new RRunExecutionResultCalculator();

  @Test
  public void complete() {
    assertTrue(CALCULATOR.isComplete(PROMPT + "command\n" + PROMPT));
  }

  @Test
  public void incomplete() {
    assertFalse(CALCULATOR.isComplete(PROMPT));
  }

  @Test
  public void empty() {
    final RExecutionResult result = CALCULATOR.calculate(
      PROMPT + "command\n" + PROMPT,
      "error"
    );

    assertEquals("", result.getOutput());
    assertEquals(RExecutionResultType.RESPONSE, result.getType());
    assertEquals(TextRange.EMPTY_RANGE, result.getResultRange());
    assertEquals("error", result.getError());
  }

  @Test
  public void notEmpty() {
    final String resultOutput = "[1] \"OK\"";

    final RExecutionResult result = CALCULATOR.calculate(
      PROMPT + "command\n" + resultOutput + "\n" + PROMPT,
      "error"
    );

    assertEquals(resultOutput, result.getOutput());
    assertEquals(RExecutionResultType.RESPONSE, result.getType());
    assertEquals(TextRange.allOf(resultOutput), result.getResultRange());
    assertEquals("error", result.getError());
  }
}