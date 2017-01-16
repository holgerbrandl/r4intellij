package com.r4intellij.debugger.data;

import org.jetbrains.annotations.NotNull;

public final class RResponseConstants {

    @NotNull
    public static final String PLUS_AND_SPACE = "+ ";

    @NotNull
    public static final String PROMPT = "> ";

    @NotNull
    public static final String BROWSE_PREFIX = "Browse[";

    @NotNull
    public static final String BROWSE_SUFFIX = "]" + PROMPT;

    @NotNull
    public static final String DEBUGGING_IN_PREFIX = "debugging in: ";

    @NotNull
    public static final String DEBUG_AT_LINE_PREFIX = "debug at #";

    @NotNull
    public static final String DEBUG_AT_PREFIX = "debug: ";

    @NotNull
    public static final String TRACING_PREFIX = "Tracing ";

    @NotNull
    public static final String EXITING_FROM_PREFIX = "exiting from: ";

    @NotNull
    public static final String ENVIRONMENT_PREFIX = "<environment: ";
}
