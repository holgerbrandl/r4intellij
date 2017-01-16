package com.r4intellij.debugger.evaluator;

import com.r4intellij.debugger.RDebuggerUtils;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RUnexpectedExecutionResultTypeException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.RDebuggerStringUtils.appendError;
import static com.r4intellij.debugger.RDebuggerUtils.calculateRepresentation;
import static com.r4intellij.debugger.data.RCommands.EXECUTE_AND_STEP_COMMAND;
import static com.r4intellij.debugger.executor.RExecutionResultType.*;
import static com.r4intellij.debugger.executor.RExecutorUtils.execute;

class RDebuggerEvaluatorImpl implements RDebuggerEvaluator {

    @NotNull
    private final RExecutor myExecutor;

    @NotNull
    private final RFunctionDebuggerFactory myFactory;

    @NotNull
    private final ROutputReceiver myReceiver;

    @NotNull
    private final RExpressionHandler myHandler;

    private final int myFrameNumber;


    public RDebuggerEvaluatorImpl(@NotNull final RExecutor executor,
                                  @NotNull final RFunctionDebuggerFactory factory,
                                  @NotNull final ROutputReceiver receiver,
                                  @NotNull final RExpressionHandler handler,
                                  final int frameNumber) {
        myExecutor = executor;
        myFactory = factory;
        myReceiver = receiver;
        myHandler = handler;
        myFrameNumber = frameNumber;
    }


    @Override
    public void evaluate(@NotNull final String expression, @NotNull final Receiver receiver) {
        try {
            doEvaluate(
                    myHandler.handle(myFrameNumber, expression),
                    receiver
            );
        } catch (final RDebuggerException e) {
            receiver.receiveError(e);
        }
    }


    private void doEvaluate(@NotNull final String expression,
                            @NotNull final Receiver receiver) throws RDebuggerException {
        final RExecutionResult result = myExecutor.execute(expression);

        switch (result.getType()) {
            case DEBUGGING_IN:
                appendError(result, myReceiver);

                receiver.receiveResult(
                        calculateRepresentation(
                                RDebuggerUtils.forciblyEvaluateFunction(
                                        myExecutor, myFactory, myReceiver
                                )
                        )
                );

                break;
            case EMPTY:
                final String error = result.getError();

                if (!error.isEmpty()) {
                    receiver.receiveError(error);
                }

                break;
            case RESPONSE:
                appendError(result, myReceiver);

                receiver.receiveResult(
                        calculateRepresentation(
                                result.getOutput()
                        )
                );

                break;
            case DEBUG_AT:
                appendError(result, myReceiver);

                receiver.receiveResult(
                        calculateRepresentation(
                                execute(myExecutor, EXECUTE_AND_STEP_COMMAND, RESPONSE, myReceiver)
                        )
                );

                break;
            default:
                throw new RUnexpectedExecutionResultTypeException(
                        "Actual type is not the same as expected: " +
                                "[" +
                                "actual: " + result.getType() + ", " +
                                "expected: " +
                                "[" + DEBUGGING_IN + ", " + EMPTY + ", " + RESPONSE + ", " + DEBUG_AT + "]" +
                                "]"
                );
        }
    }
}
