package com.r4intellij.debugger.evaluator;

import org.jetbrains.annotations.NotNull;

public interface RDebuggerEvaluator {

    void evaluate(@NotNull final String expression, @NotNull final Receiver receiver);


    interface Receiver {

        void receiveResult(@NotNull final String result);


        void receiveError(@NotNull final Exception e);


        void receiveError(@NotNull final String error);
    }
}
