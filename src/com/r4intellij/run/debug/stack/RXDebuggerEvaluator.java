package com.r4intellij.run.debug.stack;

import com.intellij.xdebugger.XSourcePosition;
import com.intellij.xdebugger.evaluation.XDebuggerEvaluator;
import com.r4intellij.debugger.evaluator.RDebuggerEvaluator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.concurrent.ExecutorService;

// TODO [xdbg][test]
class RXDebuggerEvaluator extends XDebuggerEvaluator {

    @NotNull
    private final RDebuggerEvaluator myEvaluator;

    @NotNull
    private final ExecutorService myExecutor;


    public RXDebuggerEvaluator(@NotNull final RDebuggerEvaluator evaluator, @NotNull final ExecutorService executor) {
        myEvaluator = evaluator;
        myExecutor = executor;
    }


    @Override
    public void evaluate(@NotNull final String expression,
                         @NotNull final XEvaluationCallback callback,
                         @Nullable final XSourcePosition expressionPosition) {
        myExecutor.execute(
                new Runnable() {
                    @Override
                    public void run() {
                        myEvaluator.evaluate(
                                expression,
                                new ExpressionReceiver(callback)
                        );
                    }
                }
        );
    }


    private static class ExpressionReceiver implements RDebuggerEvaluator.Receiver {

        @NotNull
        private final XEvaluationCallback myCallback;


        public ExpressionReceiver(@NotNull final XEvaluationCallback callback) {
            myCallback = callback;
        }


        @Override
        public void receiveResult(@NotNull final String result) {
            myCallback.evaluated(new RXValue(result));
        }


        @Override
        public void receiveError(@NotNull final Exception e) {
            receiveError(e.getMessage());
        }


        @Override
        public void receiveError(@NotNull final String error) {
            myCallback.errorOccurred(error);
        }
    }
}
