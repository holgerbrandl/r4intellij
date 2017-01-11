package com.r4intellij.debugger.data;

import com.intellij.util.LineSeparator;
import org.jetbrains.annotations.NotNull;

public final class RLanguageConstants {

  @NotNull
  public static final String LINE_SEPARATOR = LineSeparator.getSystemLineSeparator().getSeparatorString();

  @NotNull
  public static final String CLOSURE = "closure";

  @NotNull
  public static final String FUNCTION_TYPE = "[1] \"" + CLOSURE + "\"";

  @NotNull
  public static final String FOR_LOOP_PREFIX = "for";

  @NotNull
  public static final String WHILE_LOOP_PREFIX = "while";
}
