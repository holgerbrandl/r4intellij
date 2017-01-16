package com.r4intellij.run;

import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.process.ProcessOutputTypes;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.debugger.ROutputReceiver;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.data.RLanguageConstants.LINE_SEPARATOR;

public class ROutputReceiverImpl implements ROutputReceiver {

    @NotNull
    private final ProcessHandler myProcessHandler;


    public ROutputReceiverImpl(@NotNull final ProcessHandler processHandler) {
        myProcessHandler = processHandler;
    }


    @Override
    public void receiveOutput(@NotNull final String output) {
        receiveOutput(output, ProcessOutputTypes.STDOUT);
    }


    @Override
    public void receiveError(@NotNull final String error) {
        receiveOutput(error, ProcessOutputTypes.STDERR);
    }


    private void receiveOutput(@NotNull final String output, @NotNull final Key type) {
        myProcessHandler.notifyTextAvailable(output, type);

        if (!StringUtil.endsWithLineBreak(output)) {
            myProcessHandler.notifyTextAvailable(LINE_SEPARATOR, type);
        }
    }
}
