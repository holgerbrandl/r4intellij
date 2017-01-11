package com.r4intellij.debugger.evaluator;

import com.r4intellij.debugger.RDebuggerUtils;
import org.junit.Test;

import static com.r4intellij.debugger.data.RCommands.expressionOnFrameCommand;
import static org.junit.Assert.assertEquals;

public class RExpressionHandlerImplTest {

  @Test
  public void identifierOnTheLast() {
    final RExpressionHandlerImpl handler = new RExpressionHandlerImpl();
    handler.setLastFrameNumber(1);

    assertEquals(
      RDebuggerUtils.calculateValueCommand(1, "abc"),
      handler.handle(1, "abc")
    );
  }

  @Test
  public void identifierOnThePrevious() {
    final RExpressionHandlerImpl handler = new RExpressionHandlerImpl();
    handler.setLastFrameNumber(2);

    assertEquals(
      RDebuggerUtils.calculateValueCommand(1, "abc"),
      handler.handle(1, "abc")
    );
  }

  @Test
  public void callOnTheLast() {
    final RExpressionHandlerImpl handler = new RExpressionHandlerImpl();
    handler.setLastFrameNumber(1);

    assertEquals(
      "abc()",
      handler.handle(1, "abc()")
    );
  }

  @Test
  public void callOnThePrevious() {
    final RExpressionHandlerImpl handler = new RExpressionHandlerImpl();
    handler.setLastFrameNumber(2);

    assertEquals(
      expressionOnFrameCommand(1, "abc()"),
      handler.handle(1, "abc()")
    );
  }
}