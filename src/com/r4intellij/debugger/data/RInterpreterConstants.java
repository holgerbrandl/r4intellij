package com.r4intellij.debugger.data;

import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.List;

import static com.r4intellij.debugger.data.RCommands.BROWSER_COMMAND;
import static com.r4intellij.debugger.data.RCommands.optionsCommand;

public final class RInterpreterConstants {

    @NotNull
    public static final String NO_SAVE_PARAMETER = "--no-save";

    @NotNull
    public static final String QUIET_PARAMETER = "--quiet";

    @NotNull
    public static final String ARGS_PARAMETER = "--args";

    @NotNull
    public static final List<String> DEFAULT_PARAMETERS = Arrays.asList(NO_SAVE_PARAMETER, QUIET_PARAMETER);

    @NotNull
    public static final List<String> INIT_DEBUG_COMMANDS = Arrays.asList(BROWSER_COMMAND, optionsCommand("keep.source", "TRUE"));
}
