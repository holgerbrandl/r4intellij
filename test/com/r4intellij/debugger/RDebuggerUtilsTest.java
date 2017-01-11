package com.r4intellij.debugger;

import org.junit.Test;

import static com.r4intellij.debugger.data.RResponseConstants.ENVIRONMENT_PREFIX;
import static org.junit.Assert.assertEquals;

public class RDebuggerUtilsTest {

  @Test
  public void outerFunctionValueHandling() {
    assertEquals(
      "function(x) {\n" +
      "    x ^ 2\n" +
      "}",
      RDebuggerUtils.calculateRepresentation(
        "function(x) {\n" +
        "    x ^ 2\n" +
        "}"
      )
    );
  }

  @Test
  public void innerFunctionValueHandling() {
    assertEquals(
      "function(x) {\n" +
      "    x ^ 2\n" +
      "}",
      RDebuggerUtils.calculateRepresentation(
        "function(x) {\n" +
        "    x ^ 2\n" +
        "}\n" +
        ENVIRONMENT_PREFIX + "0xfffffff>"
      )
    );
  }
}