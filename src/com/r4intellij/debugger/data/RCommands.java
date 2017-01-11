package com.r4intellij.debugger.data;

import org.jetbrains.annotations.NotNull;

public final class RCommands {

  @NotNull
  public static final String TYPEOF_FUNCTION = "typeof";

  @NotNull
  public static final String BROWSER_COMMAND = "browser()";

  @NotNull
  public static final String EXECUTE_AND_STEP_COMMAND = "n";

  @NotNull
  public static final String ENVIRONMENT_COMMAND = "environment()";

  @NotNull
  public static final String SYS_NFRAME_COMMAND = "sys.nframe()";

  @NotNull
  public static final String QUIT_COMMAND = "q()";

  @NotNull
  public static String optionsCommand(@NotNull final String key, @NotNull final String value) {
    return String.format("options(%s=%s)", key, value);
  }

  @NotNull
  public static String loadLibCommand(@NotNull final String path) {
    return String.format("dyn.load(\"%s\")", path);
  }

  @NotNull
  public static String lsCommand(final int frameNumber) {
    return String.format("ls(%s)", sysFrameCommand(frameNumber));
  }

  @NotNull
  public static String typeOfCommand(@NotNull final String identifier) {
    return String.format("%s(%s)", TYPEOF_FUNCTION, identifier);
  }

  @NotNull
  public static String traceCommand(@NotNull final String functionName, @NotNull final String enterFunctionName) {
    return String.format("trace(%s, %s, where = environment())", functionName, enterFunctionName);
  }

  @NotNull
  public static String debugCommand(@NotNull final String function) {
    return String.format("debug(%s)", function);
  }

  @NotNull
  public static String attrCommand(@NotNull final String identifier, @NotNull final String attribute) {
    return String.format("attr(%s, \"%s\")", identifier, attribute);
  }

  @NotNull
  public static String expressionOnFrameCommand(final int frameNumber, @NotNull final String expression) {
    return String.format("%s$%s", sysFrameCommand(frameNumber), expression);
  }

  @NotNull
  public static String isDebuggedCommand(@NotNull final String function) {
    return String.format("isdebugged(%s)", function);
  }

  @NotNull
  public static String eapplyCommand(@NotNull final String environment, @NotNull final String function) {
    return String.format("eapply(%s, %s)", environment, function);
  }

  @NotNull
  public static String filterCommand(@NotNull final String function, @NotNull final String identifier) {
    return String.format("Filter(%s, %s)", function, identifier);
  }

  @NotNull
  public static String bodyCommand(@NotNull final String function) {
    return String.format("body(%s)", function);
  }

  @NotNull
  public static String sourceCommand(@NotNull final String path) {
    return String.format("source(\"%s\")", path);
  }

  @NotNull
  public static String rVersionCommand(@NotNull final String key) {
    return String.format("R.Version()[\"%s\"]", key);
  }

  @NotNull
  private static String sysFrameCommand(final int frameNumber) {
    return String.format("sys.frame(%d)", frameNumber);
  }
}
