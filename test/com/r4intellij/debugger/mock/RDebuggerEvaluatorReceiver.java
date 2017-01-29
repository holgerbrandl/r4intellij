package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.evaluator.RDebuggerEvaluator;
import org.jetbrains.annotations.NotNull;

import static org.junit.Assert.assertEquals;

public class RDebuggerEvaluatorReceiver implements RDebuggerEvaluator.Receiver {

    @NotNull
    private final String myExpectedResult;

    private int myCounter;


    public RDebuggerEvaluatorReceiver(@NotNull final String expectedResult) {
        myExpectedResult = expectedResult;
        myCounter = 0;
    }


    @Override
    public void receiveResult(@NotNull final String result) {
        myCounter++;

        assertEquals(myExpectedResult, result);
    }


    @Override
    public void receiveError(@NotNull final Exception e) {
        throw new IllegalStateException("ReceiveError shouldn't be called");
    }


    @Override
    public void receiveError(@NotNull final String error) {
        throw new IllegalStateException("ReceiveError shouldn't be called");
    }


    public int getCounter() {
        return myCounter;
    }
}
