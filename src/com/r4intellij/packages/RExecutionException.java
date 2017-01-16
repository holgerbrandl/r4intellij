package com.r4intellij.packages;

import com.intellij.execution.ExecutionException;
import org.jetbrains.annotations.NotNull;


public class RExecutionException extends ExecutionException {

    @NotNull
    private final String myCommand;
    @NotNull
    private final String myStdout;
    @NotNull
    private final String myStderr;
    private final int myExitCode;


    RExecutionException(@NotNull final String message, @NotNull final String command,
                        @NotNull final String stdout, @NotNull final String stderr, int exitCode) {
        super(message);
        myCommand = command;
        myStdout = stdout;
        myStderr = stderr;
        myExitCode = exitCode;
    }


    @NotNull
    public String getCommand() {
        return myCommand;
    }


    public int getExitCode() {
        return myExitCode;
    }


    @NotNull
    public String getStdout() {
        return myStdout;
    }


    @NotNull
    public String getStderr() {
        return myStderr;
    }
}
