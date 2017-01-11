package com.r4intellij.debugger;

import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.debugger.data.RLocation;
import com.r4intellij.debugger.evaluator.RDebuggerEvaluatorFactory;
import com.r4intellij.debugger.evaluator.RExpressionHandler;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutionResultType;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.frame.RStackFrame;
import com.r4intellij.debugger.frame.RValueModifierFactory;
import com.r4intellij.debugger.frame.RValueModifierHandler;
import com.r4intellij.debugger.frame.RVarsLoaderFactory;
import com.r4intellij.debugger.function.RFunctionDebugger;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import com.r4intellij.debugger.function.RFunctionDebuggerHandler;
import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static com.r4intellij.debugger.data.RCommands.SYS_NFRAME_COMMAND;
import static com.r4intellij.debugger.data.RCommands.bodyCommand;
import static com.r4intellij.debugger.data.RFunctionConstants.MAIN_FUNCTION_NAME;
import static com.r4intellij.debugger.executor.RExecutionResultType.*;
import static com.r4intellij.debugger.executor.RExecutorUtils.execute;
import static com.r4intellij.debugger.function.RTraceAndDebugUtils.traceAndDebugFunctions;

public class RDebugger implements RFunctionDebuggerHandler {

  @NotNull
  private static final Logger LOGGER = Logger.getInstance(RDebugger.class);

  @NotNull
  private final RExecutor myExecutor;

  @NotNull
  private final RFunctionDebuggerFactory myDebuggerFactory;

  @NotNull
  private final RVarsLoaderFactory myLoaderFactory;

  @NotNull
  private final RDebuggerEvaluatorFactory myEvaluatorFactory;

  @NotNull
  private final BufferedReader myScriptReader;

  @NotNull
  private final ROutputReceiver myOutputReceiver;

  @NotNull
  private final RExpressionHandler myExpressionHandler;

  @NotNull
  private final RValueModifierFactory myModifierFactory;

  @NotNull
  private final RValueModifierHandler myModifierHandler;

  @NotNull
  private final List<RFunctionDebugger> myDebuggers;

  @NotNull
  private final List<RStackFrame> myStack;

  @NotNull
  private final List<RStackFrame> myUnmodifiableStack;

  private int myReturnLineNumber;

  private int myDropFrames;

  private boolean myIsStarted;

  public RDebugger(@NotNull final RExecutor executor,
                   @NotNull final RFunctionDebuggerFactory debuggerFactory,
                   @NotNull final RVarsLoaderFactory loaderFactory,
                   @NotNull final RDebuggerEvaluatorFactory evaluatorFactory,
                   @NotNull final BufferedReader scriptReader,
                   @NotNull final ROutputReceiver outputReceiver,
                   @NotNull final RExpressionHandler expressionHandler,
                   @NotNull final RValueModifierFactory modifierFactory,
                   @NotNull final RValueModifierHandler modifierHandler) {
    myExecutor = executor;
    myDebuggerFactory = debuggerFactory;
    myLoaderFactory = loaderFactory;

    myEvaluatorFactory = evaluatorFactory;
    myScriptReader = scriptReader;
    myOutputReceiver = outputReceiver;
    myExpressionHandler = expressionHandler;
    myModifierFactory = modifierFactory;
    myModifierHandler = modifierHandler;

    myDebuggers = new ArrayList<RFunctionDebugger>();
    myStack = new ArrayList<RStackFrame>();
    myUnmodifiableStack = Collections.unmodifiableList(myStack);

    myReturnLineNumber = -1;
    myDropFrames = 1;
    myIsStarted = false;
  }

  public boolean advance() throws RDebuggerException {
    if (!myIsStarted) {
      return prepareDebug();
    }
    else {
      return continueDebug();
    }
  }

  @NotNull
  public List<RStackFrame> getStack() {
    return myUnmodifiableStack;
  }

  @Override
  public void appendDebugger(@NotNull final RFunctionDebugger debugger) throws RDebuggerException {
    myDebuggers.add(debugger);

    myStack.add(
      new RStackFrame(
        debugger.getLocation(),
        myLoaderFactory.getLoader(
          myModifierFactory.getModifier(
            myExecutor,
            myDebuggerFactory,
            myOutputReceiver,
            myModifierHandler,
            myStack.size()
          ),
          loadFrameNumber()
        ),
        myEvaluatorFactory.getEvaluator(
          myExecutor,
          myDebuggerFactory,
          myOutputReceiver,
          myExpressionHandler,
          myStack.size()
        )
      )
    );

    myExpressionHandler.setLastFrameNumber(myStack.size() - 1);
    myModifierHandler.setLastFrameNumber(myStack.size() - 1);
  }

  @Override
  public void setReturnLineNumber(final int lineNumber) {
    myReturnLineNumber = lineNumber;
  }

  @Override
  public void setDropFrames(final int number) {
    myDropFrames = number;
  }

  private boolean prepareDebug() throws RDebuggerException {
    myIsStarted = true;

    submitMainFunction();
    closeReader();

    traceAndDebugFunctions(myExecutor, myOutputReceiver);

    if (isMainFunctionEmpty()) {
      return false;
    }

    execute(myExecutor, MAIN_FUNCTION_NAME + "()", RExecutionResultType.DEBUGGING_IN, myOutputReceiver);

    appendDebugger(
      myDebuggerFactory.getFunctionDebugger(
        myExecutor,
        this,
        myOutputReceiver
      )
    );

    return topDebugger().hasNext();
  }

  private boolean continueDebug() throws RDebuggerException {
    topDebugger().advance(); // Don't forget that advance could append new debugger

    while (!topDebugger().hasNext()) {
      for (int i = 0; i < myDropFrames; i++) {
        popDebugger();
      }

      myDropFrames = 1;

      if (myDebuggers.isEmpty()) {
        return false;
      }
    }

    final RLocation topLocation = getTopLocation();
    final RStackFrame lastFrame = myStack.get(myStack.size() - 1);

    myStack.set(
      myStack.size() - 1,
      new RStackFrame(
        topLocation,
        lastFrame.getLoader(),
        lastFrame.getEvaluator()
      )
    );

    return true;
  }

  private int loadFrameNumber() throws RDebuggerException {
    final String frameNumber = execute(myExecutor, SYS_NFRAME_COMMAND, RExecutionResultType.RESPONSE, myOutputReceiver);

    return Integer.parseInt(frameNumber.substring("[1] ".length()));
  }

  private void submitMainFunction() throws RDebuggerException {
    execute(myExecutor, MAIN_FUNCTION_NAME + " <- function() {", PLUS, myOutputReceiver);

    try {
      String command;

      while ((command = myScriptReader.readLine()) != null) {
        execute(myExecutor, command, PLUS, myOutputReceiver);
      }
    }
    catch (final IOException e) {
      throw new RDebuggerException(e);
    }

    execute(myExecutor, "}", EMPTY, myOutputReceiver);
  }

  private void closeReader() {
    try {
      myScriptReader.close();
    }
    catch (final IOException e) {
      LOGGER.warn(e);
    }
  }

  @NotNull
  private RFunctionDebugger topDebugger() {
    return myDebuggers.get(myDebuggers.size() - 1);
  }

  private void popDebugger() {
    myDebuggers.remove(myDebuggers.size() - 1);
    myStack.remove(myStack.size() - 1);

    myExpressionHandler.setLastFrameNumber(myStack.size() - 1);
    myModifierHandler.setLastFrameNumber(myStack.size() - 1);
  }

  @NotNull
  private RLocation getTopLocation() {
    final RFunctionDebugger topDebugger = topDebugger();

    if (myReturnLineNumber != -1) {
      final RLocation result = new RLocation(
        topDebugger.getLocation().getFunctionName(),
        myReturnLineNumber
      );

      myReturnLineNumber = -1;

      return result;
    }

    return topDebugger.getLocation();
  }

  private boolean isMainFunctionEmpty() throws RDebuggerException {
    final String collapsedMainFunction = execute(myExecutor, bodyCommand(MAIN_FUNCTION_NAME), RESPONSE, myOutputReceiver);

    return StringUtil.countNewLines(collapsedMainFunction) < 5;
  }
}
