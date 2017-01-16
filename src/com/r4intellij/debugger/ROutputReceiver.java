package com.r4intellij.debugger;

import org.jetbrains.annotations.NotNull;

public interface ROutputReceiver {

    void receiveOutput(@NotNull final String output);


    void receiveError(@NotNull final String error);
}
