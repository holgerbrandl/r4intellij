package com.r4intellij.debugger.mock;

import com.r4intellij.debugger.ROutputReceiver;
import org.jetbrains.annotations.NotNull;

public class IllegalROutputReceiver implements ROutputReceiver {

    @Override
    public void receiveOutput(@NotNull final String output) {
        throw new IllegalStateException("ReceiverOutput shouldn't be called");
    }


    @Override
    public void receiveError(@NotNull final String error) {
        throw new IllegalStateException("ReceiverError shouldn't be called");
    }
}
