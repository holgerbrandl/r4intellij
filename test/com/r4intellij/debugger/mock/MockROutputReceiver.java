package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.ROutputReceiver;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.List;

public class MockROutputReceiver implements ROutputReceiver {

    @NotNull
    private final List<String> myOutputs = new ArrayList<String>();

    @NotNull
    private final List<String> myErrors = new ArrayList<String>();


    @Override
    public void receiveOutput(@NotNull final String output) {
        myOutputs.add(output);
    }


    @Override
    public void receiveError(@NotNull final String error) {
        myErrors.add(error);
    }


    @NotNull
    public List<String> getOutputs() {
        return myOutputs;
    }


    @NotNull
    public List<String> getErrors() {
        return myErrors;
    }


    public void reset() {
        myOutputs.clear();
        myErrors.clear();
    }
}
