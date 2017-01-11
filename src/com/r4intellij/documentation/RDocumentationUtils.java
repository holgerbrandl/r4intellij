package com.r4intellij.documentation;

import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.RHelp;
import org.jetbrains.annotations.NotNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public final class RDocumentationUtils {
  private static final Pattern ourPattern = Pattern.compile("^.+:");

  @NotNull
  public static String getFormattedString(@NotNull final RHelp help) {
    final StringBuilder builder = new StringBuilder();
    if (help.myDescription != null) {
      builder.append("<b><u>Description:</u></b>");
      builder.append("<br/>");
      builder.append(help.myDescription);
    }
    if (help.myArguments != null) {
      builder.append("<br/><br/>");
      builder.append("<b><u>Arguments:</u></b>");
      String[] args = help.myArguments.split("\n");
      for (String arg : args) {
        formatAndAppend(builder, arg);
      }
    }
    if (help.myValue != null) {
      builder.append("<br/><br/>");
      builder.append("<b><u>Returns:</u></b>");
      builder.append("<br/>");
      builder.append(help.myValue);
    }
    return builder.toString();
  }

  @NotNull
  public static String getFormattedString(@NotNull String docString) {
    final StringBuilder builder = new StringBuilder();
    final String[] strings = StringUtil.splitByLines(docString);
    for (String string : strings) {
      final String trimmedString = string.trim();
      if (trimmedString.startsWith("Arguments:") || trimmedString.startsWith("Returns:")) {
        builder.append("<br/>");
        builder.append("<b>").append(string).append("</b>");
      }
      else {
        formatAndAppend(builder, trimmedString);
      }
    }
    return builder.toString();
  }

  private static void formatAndAppend(@NotNull final StringBuilder builder, @NotNull final String docString) {
    builder.append("<br/>");
    final Matcher matcher = ourPattern.matcher(docString);
    if (matcher.find()) {
      builder.append(matcher.replaceFirst("<b>$0</b>"));
    }
    else {
      builder.append(docString);
    }
    builder.append(" ");
  }
}
