package com.r4intellij.debugger;

import com.intellij.openapi.util.TextRange;
import com.r4intellij.debugger.data.RLanguageConstants;
import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.data.RResponseConstants;
import com.r4intellij.debugger.evaluator.RDebuggerEvaluator;
import com.r4intellij.debugger.evaluator.RDebuggerEvaluatorFactory;
import com.r4intellij.debugger.evaluator.RExpressionHandler;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.frame.*;
import com.r4intellij.debugger.function.RFunctionDebugger;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import com.r4intellij.debugger.mock.*;
import org.jetbrains.annotations.NotNull;
import org.junit.Test;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Collections;

import static com.r4intellij.debugger.data.RFunctionConstants.MAIN_FUNCTION_NAME;
import static com.r4intellij.debugger.mock.MockRExecutor.LS_FUNCTIONS_ERROR;
import static org.junit.Assert.*;

public class RDebuggerTest {

  @Test
  public void empty() throws RDebuggerException {
    final EmptyRExecutor executor = new EmptyRExecutor();
    final MockRVarsLoaderFactory loaderFactory = new MockRVarsLoaderFactory();
    final MockRDebuggerEvaluatorFactory evaluatorFactory = new MockRDebuggerEvaluatorFactory();
    final MockRScriptReader scriptReader = new MockRScriptReader(0);
    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRValueModifierFactory modifierFactory = new MockRValueModifierFactory();

    final RDebugger debugger = new RDebugger(
      executor,
      new MockRFunctionDebuggerFactory(null),
      loaderFactory,
      evaluatorFactory,
      scriptReader,
      outputReceiver,
      new IllegalRExpressionHandler(),
      modifierFactory,
      new IllegalRValueModifierHandler()
    );

    assertEquals(0, executor.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(0, evaluatorFactory.myCounter);
    assertFalse(scriptReader.isClosed());
    assertEquals(0, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, modifierFactory.myCounter);
    assertEquals(0, debugger.getStack().size());

    assertFalse(debugger.advance());

    assertEquals(4, executor.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(0, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Arrays.asList("error1", "error_complete", LS_FUNCTIONS_ERROR, "error_body"), outputReceiver.getErrors());
    assertEquals(0, modifierFactory.myCounter);
    assertEquals(0, debugger.getStack().size());
  }

  @Test
  public void stack1() throws RDebuggerException {
    // just `main`

    /*
    instruction1
    instruction2
    */

    final int scriptLength = 2;

    final MockRExecutor executor = new MockRExecutor(scriptLength);
    final MockRFunctionDebugger functionDebugger = new MockRFunctionDebugger(MAIN_FUNCTION_NAME, scriptLength, null);
    final MockRFunctionDebuggerFactory debuggerFactory = new MockRFunctionDebuggerFactory(functionDebugger);
    final MockRVarsLoaderFactory loaderFactory = new MockRVarsLoaderFactory();
    final MockRDebuggerEvaluatorFactory evaluatorFactory = new MockRDebuggerEvaluatorFactory();
    final MockRScriptReader scriptReader = new MockRScriptReader(scriptLength);
    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler expressionHandler = new MockRExpressionHandler();
    final MockRValueModifierFactory modifierFactory = new MockRValueModifierFactory();
    final MockRValueModifierHandler modifierHandler = new MockRValueModifierHandler();

    final RDebugger debugger = new RDebugger(
      executor,
      debuggerFactory,
      loaderFactory,
      evaluatorFactory,
      scriptReader,
      outputReceiver,
      expressionHandler,
      modifierFactory,
      modifierHandler
    );

    assertEquals(0, executor.getCounter());
    assertEquals(0, functionDebugger.getCounter());
    assertEquals(0, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(0, evaluatorFactory.myCounter);
    assertFalse(scriptReader.isClosed());
    assertEquals(0, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(0, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());

    assertTrue(debugger.advance());

    assertEquals(8, executor.getCounter());
    assertEquals(0, functionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(
      Arrays.asList("error1", "error2", "error3", "error_complete", LS_FUNCTIONS_ERROR, "error_body", "error_call", "error0"),
      outputReceiver.getErrors()
    );
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 0), debugger.getStack().get(0).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(8, executor.getCounter());
    assertEquals(1, functionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());

    assertFalse(debugger.advance());

    assertEquals(8, executor.getCounter());
    assertEquals(2, functionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(-1, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(-1, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());
  }

  @Test
  public void stack2() throws RDebuggerException {
    // `main` and function

    /*
    instruction1
    abc() {
      instruction1
      instruction2
    }
    instruction2
    */

    final int scriptLength = 6;

    final MockRExecutor executor = new MockRExecutor(scriptLength);
    final MockRFunctionDebugger secondFunctionDebugger = new MockRFunctionDebugger("abc", 2, null);
    final Stack21RFunctionDebugger firstFunctionDebugger = new Stack21RFunctionDebugger(secondFunctionDebugger);
    final MockRFunctionDebuggerFactory debuggerFactory = new MockRFunctionDebuggerFactory(firstFunctionDebugger);
    final MockRVarsLoaderFactory loaderFactory = new MockRVarsLoaderFactory();
    final MockRDebuggerEvaluatorFactory evaluatorFactory = new MockRDebuggerEvaluatorFactory();
    final MockRScriptReader scriptReader = new MockRScriptReader(scriptLength);
    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler expressionHandler = new MockRExpressionHandler();
    final MockRValueModifierFactory modifierFactory = new MockRValueModifierFactory();
    final MockRValueModifierHandler modifierHandler = new MockRValueModifierHandler();

    final RDebugger debugger = new RDebugger(
      executor,
      debuggerFactory,
      loaderFactory,
      evaluatorFactory,
      scriptReader,
      outputReceiver,
      expressionHandler,
      modifierFactory,
      modifierHandler
    );

    assertEquals(0, executor.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(0, firstFunctionDebugger.getCounter());
    assertEquals(0, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(0, evaluatorFactory.myCounter);
    assertFalse(scriptReader.isClosed());
    assertEquals(0, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(0, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());

    assertTrue(debugger.advance());

    assertEquals(12, executor.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(0, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(
      Arrays.asList(
        "error1", "error2", "error3", "error4", "error5", "error6", "error7", "error_complete", LS_FUNCTIONS_ERROR, "error_body",
        "error_call", "error0"
      ),
      outputReceiver.getErrors()
    );
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 0), debugger.getStack().get(0).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(12, executor.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(1, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());

    assertTrue(debugger.advance());

    assertEquals(13, executor.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList("error1"), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 0), debugger.getStack().get(1).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(13, executor.getCounter());
    assertEquals(1, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());

    assertTrue(debugger.advance());

    assertEquals(13, executor.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 2), debugger.getStack().get(0).getLocation());

    assertFalse(debugger.advance());

    assertEquals(13, executor.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(3, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());
  }

  @Test
  public void stack31() throws RDebuggerException {
    // `main`, function `a`, function `b` with `debug at` at the end

    /*
    instruction1
    abc() {
      instruction1
      def() {
        instruction1
        instruction2
      }
      instruction2
     }
     instruction2
     */

    final int scriptLength = 10;

    final MockRExecutor executor = new MockRExecutor(scriptLength);
    final MockRFunctionDebugger thirdFunctionDebugger = new Stack313RFunctionDebugger();
    final MockRFunctionDebugger secondFunctionDebugger = new Stack312RFunctionDebugger(thirdFunctionDebugger);
    final MockRFunctionDebugger firstFunctionDebugger = new Stack311RFunctionDebugger(secondFunctionDebugger);
    final MockRFunctionDebuggerFactory debuggerFactory = new MockRFunctionDebuggerFactory(firstFunctionDebugger);
    final MockRVarsLoaderFactory loaderFactory = new MockRVarsLoaderFactory();
    final MockRDebuggerEvaluatorFactory evaluatorFactory = new MockRDebuggerEvaluatorFactory();
    final MockRScriptReader scriptReader = new MockRScriptReader(scriptLength);
    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler expressionHandler = new MockRExpressionHandler();
    final MockRValueModifierFactory modifierFactory = new MockRValueModifierFactory();
    final MockRValueModifierHandler modifierHandler = new MockRValueModifierHandler();

    final RDebugger debugger = new RDebugger(
      executor,
      debuggerFactory,
      loaderFactory,
      evaluatorFactory,
      scriptReader,
      outputReceiver,
      expressionHandler,
      modifierFactory,
      modifierHandler
    );

    assertEquals(0, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(0, firstFunctionDebugger.getCounter());
    assertEquals(0, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(0, evaluatorFactory.myCounter);
    assertFalse(scriptReader.isClosed());
    assertEquals(0, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(0, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());

    assertTrue(debugger.advance());

    assertEquals(16, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(0, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(
      Arrays.asList(
        "error1", "error2", "error3", "error4", "error5", "error6", "error7", "error8", "error9", "error10", "error11", "error_complete",
        LS_FUNCTIONS_ERROR, "error_body", "error_call", "error0"
      ),
      outputReceiver.getErrors()
    );
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 0), debugger.getStack().get(0).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(16, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(1, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());

    assertTrue(debugger.advance());

    assertEquals(17, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList("error1"), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 0), debugger.getStack().get(1).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(17, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(1, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());

    assertTrue(debugger.advance());

    assertEquals(18, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList("error2"), outputReceiver.getErrors());
    assertEquals(3, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(3, modifierHandler.myCounter);
    assertEquals(3, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());
    assertEquals(new RLocation("def", 0), debugger.getStack().get(2).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(18, executor.getCounter());
    assertEquals(1, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(3, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(3, modifierHandler.myCounter);
    assertEquals(3, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());
    assertEquals(new RLocation("def", 1), debugger.getStack().get(2).getLocation());

    assertTrue(debugger.advance());

    assertEquals(18, executor.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(4, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(4, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 5), debugger.getStack().get(1).getLocation());

    assertTrue(debugger.advance());

    assertEquals(18, executor.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(3, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(4, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(4, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 2), debugger.getStack().get(0).getLocation());

    assertFalse(debugger.advance());

    assertEquals(18, executor.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(3, secondFunctionDebugger.getCounter());
    assertEquals(3, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(3, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(3, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());
  }

  @Test
  public void stack32() throws RDebuggerException {
    // `main`, function `a` and function `b` with recursive return

    /*
    instruction1
    abc() {
      instruction1
      def() {
        instruction1
        instruction2
      }
    }
    instruction2
    */

    final int scriptLength = 9;

    final MockRExecutor executor = new MockRExecutor(scriptLength);
    final MockRFunctionDebugger thirdFunctionDebugger = new Stack323RFunctionDebugger();
    final MockRFunctionDebugger secondFunctionDebugger = new Stack322RFunctionDebugger(thirdFunctionDebugger);
    final MockRFunctionDebugger firstFunctionDebugger = new Stack321RFunctionDebugger(secondFunctionDebugger);
    final MockRFunctionDebuggerFactory debuggerFactory = new MockRFunctionDebuggerFactory(firstFunctionDebugger);
    final MockRVarsLoaderFactory loaderFactory = new MockRVarsLoaderFactory();
    final MockRDebuggerEvaluatorFactory evaluatorFactory = new MockRDebuggerEvaluatorFactory();
    final MockRScriptReader scriptReader = new MockRScriptReader(scriptLength);
    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler expressionHandler = new MockRExpressionHandler();
    final MockRValueModifierFactory modifierFactory = new MockRValueModifierFactory();
    final MockRValueModifierHandler modifierHandler = new MockRValueModifierHandler();

    final RDebugger debugger = new RDebugger(
      executor,
      debuggerFactory,
      loaderFactory,
      evaluatorFactory,
      scriptReader,
      outputReceiver,
      expressionHandler,
      modifierFactory,
      modifierHandler
    );

    assertEquals(0, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(0, firstFunctionDebugger.getCounter());
    assertEquals(0, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(0, evaluatorFactory.myCounter);
    assertFalse(scriptReader.isClosed());
    assertEquals(0, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(0, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());

    assertTrue(debugger.advance());

    assertEquals(15, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(0, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(
      Arrays.asList(
        "error1", "error2", "error3", "error4", "error5", "error6", "error7", "error8", "error9", "error10", "error_complete",
        LS_FUNCTIONS_ERROR, "error_body", "error_call", "error0"
      ),
      outputReceiver.getErrors()
    );
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 0), debugger.getStack().get(0).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(15, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(1, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());

    assertTrue(debugger.advance());

    assertEquals(16, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList("error1"), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 0), debugger.getStack().get(1).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(16, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(1, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());

    assertTrue(debugger.advance());

    assertEquals(17, executor.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList("error2"), outputReceiver.getErrors());
    assertEquals(3, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(3, modifierHandler.myCounter);
    assertEquals(3, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());
    assertEquals(new RLocation("def", 0), debugger.getStack().get(2).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(17, executor.getCounter());
    assertEquals(1, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(3, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(3, modifierHandler.myCounter);
    assertEquals(3, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());
    assertEquals(new RLocation("def", 1), debugger.getStack().get(2).getLocation());

    assertTrue(debugger.advance());

    assertEquals(17, executor.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(4, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(4, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 2), debugger.getStack().get(0).getLocation());

    assertFalse(debugger.advance());

    assertEquals(17, executor.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(3, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(3, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(3, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());
  }

  @Test
  public void stack4() throws RDebuggerException {
    // `main`, function `a`, function `b`, function `c` - recursive return from `c` and `b` with `debug at` at the end

    /*
    instruction1
    abc() {
      instruction1
      def() {
        instruction1
        ghi() {
          instruction1
          instruction2
        }
      }
      instruction2
    }
    instruction2
    */

    final int scriptLength = 13;

    final MockRExecutor executor = new MockRExecutor(scriptLength);
    final MockRFunctionDebugger fourthFunctionDebugger = new Stack44RFunctionDebugger();
    final MockRFunctionDebugger thirdFunctionDebugger = new Stack43RFunctionDebugger(fourthFunctionDebugger);
    final MockRFunctionDebugger secondFunctionDebugger = new Stack42RFunctionDebugger(thirdFunctionDebugger);
    final MockRFunctionDebugger firstFunctionDebugger = new Stack41RFunctionDebugger(secondFunctionDebugger);
    final MockRFunctionDebuggerFactory debuggerFactory = new MockRFunctionDebuggerFactory(firstFunctionDebugger);
    final MockRVarsLoaderFactory loaderFactory = new MockRVarsLoaderFactory();
    final MockRDebuggerEvaluatorFactory evaluatorFactory = new MockRDebuggerEvaluatorFactory();
    final MockRScriptReader scriptReader = new MockRScriptReader(scriptLength);
    final MockROutputReceiver outputReceiver = new MockROutputReceiver();
    final MockRExpressionHandler expressionHandler = new MockRExpressionHandler();
    final MockRValueModifierFactory modifierFactory = new MockRValueModifierFactory();
    final MockRValueModifierHandler modifierHandler = new MockRValueModifierHandler();

    final RDebugger debugger = new RDebugger(
      executor,
      debuggerFactory,
      loaderFactory,
      evaluatorFactory,
      scriptReader,
      outputReceiver,
      expressionHandler,
      modifierFactory,
      modifierHandler
    );

    assertEquals(0, executor.getCounter());
    assertEquals(0, fourthFunctionDebugger.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(0, firstFunctionDebugger.getCounter());
    assertEquals(0, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(0, evaluatorFactory.myCounter);
    assertFalse(scriptReader.isClosed());
    assertEquals(0, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(0, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());

    assertTrue(debugger.advance());

    assertEquals(19, executor.getCounter());
    assertEquals(0, fourthFunctionDebugger.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(0, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(
      Arrays.asList(
        "error1", "error2", "error3", "error4", "error5", "error6", "error7", "error8", "error9", "error10", "error11", "error12",
        "error13", "error14", "error_complete", LS_FUNCTIONS_ERROR, "error_body", "error_call", "error0"
      ),
      outputReceiver.getErrors()
    );
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 0), debugger.getStack().get(0).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(19, executor.getCounter());
    assertEquals(0, fourthFunctionDebugger.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(1, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(0, loaderFactory.myCounter);
    assertEquals(1, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(0, expressionHandler.myCounter);
    assertEquals(1, modifierFactory.myCounter);
    assertEquals(0, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());

    assertTrue(debugger.advance());

    assertEquals(20, executor.getCounter());
    assertEquals(0, fourthFunctionDebugger.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(0, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList("error1"), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 0), debugger.getStack().get(1).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(20, executor.getCounter());
    assertEquals(0, fourthFunctionDebugger.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(1, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(1, loaderFactory.myCounter);
    assertEquals(2, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(1, expressionHandler.myCounter);
    assertEquals(2, modifierFactory.myCounter);
    assertEquals(1, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());

    assertTrue(debugger.advance());

    assertEquals(21, executor.getCounter());
    assertEquals(0, fourthFunctionDebugger.getCounter());
    assertEquals(0, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList("error2"), outputReceiver.getErrors());
    assertEquals(3, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(3, modifierHandler.myCounter);
    assertEquals(3, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());
    assertEquals(new RLocation("def", 0), debugger.getStack().get(2).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(21, executor.getCounter());
    assertEquals(0, fourthFunctionDebugger.getCounter());
    assertEquals(1, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(3, loaderFactory.myCounter);
    assertEquals(3, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(3, expressionHandler.myCounter);
    assertEquals(3, modifierFactory.myCounter);
    assertEquals(3, modifierHandler.myCounter);
    assertEquals(3, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());
    assertEquals(new RLocation("def", 1), debugger.getStack().get(2).getLocation());

    assertTrue(debugger.advance());

    assertEquals(22, executor.getCounter());
    assertEquals(0, fourthFunctionDebugger.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(6, loaderFactory.myCounter);
    assertEquals(4, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.singletonList("error3"), outputReceiver.getErrors());
    assertEquals(6, expressionHandler.myCounter);
    assertEquals(4, modifierFactory.myCounter);
    assertEquals(6, modifierHandler.myCounter);
    assertEquals(4, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());
    assertEquals(new RLocation("def", 1), debugger.getStack().get(2).getLocation());
    assertEquals(new RLocation("ghi", 0), debugger.getStack().get(3).getLocation());

    outputReceiver.reset();
    assertTrue(debugger.advance());

    assertEquals(22, executor.getCounter());
    assertEquals(1, fourthFunctionDebugger.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(6, loaderFactory.myCounter);
    assertEquals(4, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(6, expressionHandler.myCounter);
    assertEquals(4, modifierFactory.myCounter);
    assertEquals(6, modifierHandler.myCounter);
    assertEquals(4, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 1), debugger.getStack().get(1).getLocation());
    assertEquals(new RLocation("def", 1), debugger.getStack().get(2).getLocation());
    assertEquals(new RLocation("ghi", 1), debugger.getStack().get(3).getLocation());

    assertTrue(debugger.advance());

    assertEquals(22, executor.getCounter());
    assertEquals(2, fourthFunctionDebugger.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(2, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(6, loaderFactory.myCounter);
    assertEquals(4, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(9, expressionHandler.myCounter);
    assertEquals(4, modifierFactory.myCounter);
    assertEquals(9, modifierHandler.myCounter);
    assertEquals(2, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 1), debugger.getStack().get(0).getLocation());
    assertEquals(new RLocation("abc", 5), debugger.getStack().get(1).getLocation());

    assertTrue(debugger.advance());

    assertEquals(22, executor.getCounter());
    assertEquals(2, fourthFunctionDebugger.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(3, secondFunctionDebugger.getCounter());
    assertEquals(2, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(6, loaderFactory.myCounter);
    assertEquals(4, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(9, expressionHandler.myCounter);
    assertEquals(4, modifierFactory.myCounter);
    assertEquals(9, modifierHandler.myCounter);
    assertEquals(1, debugger.getStack().size());
    assertEquals(new RLocation(MAIN_FUNCTION_NAME, 2), debugger.getStack().get(0).getLocation());

    assertFalse(debugger.advance());

    assertEquals(22, executor.getCounter());
    assertEquals(2, fourthFunctionDebugger.getCounter());
    assertEquals(2, thirdFunctionDebugger.getCounter());
    assertEquals(3, secondFunctionDebugger.getCounter());
    assertEquals(3, firstFunctionDebugger.getCounter());
    assertEquals(1, debuggerFactory.getCounter());
    assertEquals(6, loaderFactory.myCounter);
    assertEquals(4, evaluatorFactory.myCounter);
    assertTrue(scriptReader.isClosed());
    assertEquals(scriptLength + 1, scriptReader.getCounter());
    assertEquals(Collections.emptyList(), outputReceiver.getOutputs());
    assertEquals(Collections.emptyList(), outputReceiver.getErrors());
    assertEquals(8, expressionHandler.myCounter);
    assertEquals(4, modifierFactory.myCounter);
    assertEquals(8, modifierHandler.myCounter);
    assertEquals(0, debugger.getStack().size());
  }

  private static class MockRExecutor extends com.r4intellij.debugger.mock.MockRExecutor {

    private final int myScriptLength;

    public MockRExecutor(final int scriptLength) {
      myScriptLength = scriptLength;
    }

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (getCounter() < myScriptLength + 2) {
        return new RExecutionResult(
          RResponseConstants.PLUS_AND_SPACE,
          RExecutionResultType.PLUS,
          TextRange.EMPTY_RANGE,
          "error" + getCounter()
        );
      }

      if (getCounter() == myScriptLength + 2) {
        return new RExecutionResult(
          "",
          RExecutionResultType.EMPTY,
          TextRange.EMPTY_RANGE,
          "error_complete"
        );
      }

      if (getCounter() == myScriptLength + 4) {
        return new RExecutionResult(
          " \n \n \n \n \n \n ",
          RExecutionResultType.RESPONSE,
          TextRange.allOf(" \n \n \n \n \n \n "),
          "error_body"
        );
      }

      if (getCounter() == myScriptLength + 5) {
        return new RExecutionResult(
          "",
          RExecutionResultType.DEBUGGING_IN,
          TextRange.EMPTY_RANGE,
          "error_call"
        );
      }

      final int frameNumber = getCounter() - 1 - myScriptLength - 5;

      return new RExecutionResult(
        "[1] " + frameNumber,
        RExecutionResultType.RESPONSE,
        TextRange.allOf("[1] " + frameNumber),
        "error" + frameNumber
      );
    }
  }

  private static class EmptyRExecutor extends MockRExecutor {

    public EmptyRExecutor() {
      super(0);
    }

    @NotNull
    @Override
    protected RExecutionResult doExecute(@NotNull final String command) throws RDebuggerException {
      if (getCounter() < 4) {
        return super.doExecute(command);
      }
      else if (getCounter() == 4) {
        return new RExecutionResult(
          " \n \n \n ",
          RExecutionResultType.RESPONSE,
          TextRange.allOf(" \n \n \n "),
          "error_body"
        );
      }
      else {
        throw new IllegalStateException("Unexpected command");
      }
    }
  }

  private static class MockRVarsLoaderFactory implements RVarsLoaderFactory {

    private int myCounter = 0;

    @NotNull
    @Override
    public RVarsLoader getLoader(@NotNull final RValueModifier modifier,
                                 final int frameNumber) {
      myCounter += frameNumber;

      return new IllegalRVarsLoader();
    }
  }

  private static class MockRDebuggerEvaluatorFactory implements RDebuggerEvaluatorFactory {

    private int myCounter = 0;

    @NotNull
    @Override
    public RDebuggerEvaluator getEvaluator(@NotNull final RExecutor executor,
                                           @NotNull final RFunctionDebuggerFactory factory,
                                           @NotNull final ROutputReceiver receiver,
                                           @NotNull final RExpressionHandler handler,
                                           final int frameNumber) {
      myCounter++;

      return new IllegalRDebuggerEvaluator();
    }
  }

  private static class MockRScriptReader extends BufferedReader {

    private int myCounter;
    private boolean myIsClosed;

    public MockRScriptReader(final int length) {
      super(new StringReader(calculateString(length)));

      myCounter = 0;
      myIsClosed = false;
    }

    @Override
    public String readLine() throws IOException {
      final String result = super.readLine();

      myCounter++;

      return result;
    }

    @Override
    public void close() throws IOException {
      super.close();

      myIsClosed = true;
    }

    public int getCounter() {
      return myCounter;
    }

    public boolean isClosed() {
      return myIsClosed;
    }

    @NotNull
    private static String calculateString(final int length) {
      final StringBuilder sb = new StringBuilder();

      for (int i = 0; i < length; i++) {
        sb.append(RLanguageConstants.LINE_SEPARATOR);
      }

      return sb.toString();
    }
  }

  private static class MockRExpressionHandler extends IllegalRExpressionHandler {

    private int myCounter = 0;

    @Override
    public void setLastFrameNumber(final int lastFrameNumber) {
      myCounter += lastFrameNumber;
    }
  }

  private static class MockRValueModifierFactory implements RValueModifierFactory {

    private int myCounter = 0;

    @NotNull
    @Override
    public RValueModifier getModifier(@NotNull final RExecutor executor,
                                      @NotNull final RFunctionDebuggerFactory factory,
                                      @NotNull final ROutputReceiver receiver,
                                      @NotNull final RValueModifierHandler handler,
                                      final int frameNumber) {
      myCounter++;

      return new IllegalRValueModifier();
    }
  }

  private static class MockRValueModifierHandler extends IllegalRValueModifierHandler {

    private int myCounter = 0;

    @Override
    public void setLastFrameNumber(final int lastFrameNumber) {
      myCounter += lastFrameNumber;
    }
  }

  private static class Stack21RFunctionDebugger extends MockRFunctionDebugger {

    @NotNull
    private final RFunctionDebugger myNextFunctionDebugger;

    public Stack21RFunctionDebugger(@NotNull final RFunctionDebugger nextFunctionDebugger) {
      super(MAIN_FUNCTION_NAME, 3, null);

      myNextFunctionDebugger = nextFunctionDebugger;
    }

    @Override
    public void advance() throws RDebuggerException {
      super.advance();

      if (getCounter() == 2) {
        assert getHandler() != null;

        getHandler().appendDebugger(myNextFunctionDebugger);
      }
    }
  }

  private static class Stack311RFunctionDebugger extends MockRFunctionDebugger {

    @NotNull
    private final MockRFunctionDebugger myNextFunctionDebugger;

    public Stack311RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
      super(MAIN_FUNCTION_NAME, 3, null);

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

  private static class Stack312RFunctionDebugger extends MockRFunctionDebugger {

    @NotNull
    private final MockRFunctionDebugger myNextFunctionDebugger;

    public Stack312RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
      super("abc", 3, null);

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

  private static class Stack313RFunctionDebugger extends MockRFunctionDebugger {

    public Stack313RFunctionDebugger() {
      super("def", 2, null);
    }

    @Override
    public void advance() throws RDebuggerException {
      super.advance();

      if (getCounter() == 2) {
        assert getHandler() != null;

        getHandler().setReturnLineNumber(5);
      }
    }
  }

  private static class Stack321RFunctionDebugger extends MockRFunctionDebugger {

    @NotNull
    private final MockRFunctionDebugger myNextFunctionDebugger;

    public Stack321RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
      super(MAIN_FUNCTION_NAME, 3, null);

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

  private static class Stack322RFunctionDebugger extends MockRFunctionDebugger {

    @NotNull
    private final MockRFunctionDebugger myNextFunctionDebugger;

    public Stack322RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
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

  private static class Stack323RFunctionDebugger extends MockRFunctionDebugger {

    public Stack323RFunctionDebugger() {
      super("def", 2, null);
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

  private static class Stack41RFunctionDebugger extends MockRFunctionDebugger {

    @NotNull
    private final MockRFunctionDebugger myNextFunctionDebugger;

    public Stack41RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
      super(MAIN_FUNCTION_NAME, 3, null);

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

  private static class Stack42RFunctionDebugger extends MockRFunctionDebugger {

    @NotNull
    private final MockRFunctionDebugger myNextFunctionDebugger;

    public Stack42RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
      super("abc", 3, null);

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

  private static class Stack43RFunctionDebugger extends MockRFunctionDebugger {

    @NotNull
    private final MockRFunctionDebugger myNextFunctionDebugger;

    public Stack43RFunctionDebugger(@NotNull final MockRFunctionDebugger nextFunctionDebugger) {
      super("def", 2, null);

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

  private static class Stack44RFunctionDebugger extends MockRFunctionDebugger {

    public Stack44RFunctionDebugger() {
      super("ghi", 2, null);
    }

    @Override
    public void advance() throws RDebuggerException {
      super.advance();

      if (getCounter() == 2) {
        assert getHandler() != null;

        getHandler().setDropFrames(2);
        getHandler().setReturnLineNumber(5);
      }
    }
  }
}