package com.r4intellij.documentation;

import org.jetbrains.annotations.NotNull;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RHelp {
    private static final Pattern ourPattern = Pattern.compile("^.+:");
    public String myDescription;
    public String myArguments;
    public String myValue;
    public String myUsage;
    public String myExamples;


    public RHelp(@NotNull final String documentationText) {
        final String[] lines = documentationText.split("\n");
        String section = null;
        StringBuilder sectionText = null;
        for (String line : lines) {
            String trimmed = line.trim();
            if (trimmed.isEmpty()) {
                if (section != null && sectionText.length() > 0) {
                    sectionText.append('\n');
                }
            } else {
                if (line.startsWith("\t") || line.startsWith(" ")) {
                    if (section != null) {
                        sectionText.append(trimmed);
                        sectionText.append('\n');
                    }
                } else {
                    if (trimmed.endsWith(":")) {
                        saveSection(section, sectionText);
                        section = trimmed.substring(0, trimmed.length() - 1);
                        sectionText = new StringBuilder();
                    } else {
                        if (section != null) {
                            sectionText.append(trimmed);
                            sectionText.append('\n');
                        }
                    }
                }
            }
        }
        saveSection(section, sectionText);
    }


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


    public static void formatAndAppend(@NotNull final StringBuilder builder, @NotNull final String docString) {
        builder.append("<br/>");
        final Matcher matcher = ourPattern.matcher(docString);
        if (matcher.find()) {
            builder.append(matcher.replaceFirst("<b>$0</b>"));
        } else {
            builder.append(docString);
        }
        builder.append(" ");
    }


    private void saveSection(String section, StringBuilder sectionText) {
        if (section != null) {
            section = section.replaceAll("_\b", "");
            if ("Arguments".equals(section)) {
                myArguments = sectionText.toString();
            } else if ("Value".equals(section)) {
                myValue = sectionText.toString();
            } else if ("Usage".equals(section)) {
                myUsage = sectionText.toString();
            } else if ("Examples".equals(section)) {
                myExamples = sectionText.toString();
            } else if ("Description".equals(section)) {
                myDescription = sectionText.toString();
            }
        }
    }
}
