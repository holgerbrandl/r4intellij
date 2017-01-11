package com.r4intellij;

import org.jetbrains.annotations.NotNull;

public class RHelp {
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
      }
      else {
        if (line.startsWith("\t") || line.startsWith(" ")) {
          if (section != null) {
            sectionText.append(trimmed);
            sectionText.append('\n');
          }
        }
        else {
          if (trimmed.endsWith(":")) {
            saveSection(section, sectionText);
            section = trimmed.substring(0, trimmed.length() - 1);
            sectionText = new StringBuilder();
          }
          else {
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

  private void saveSection(String section, StringBuilder sectionText) {
    if (section != null) {
      section = section.replaceAll("_\b", "");
      if ("Arguments".equals(section)) {
        myArguments = sectionText.toString();
      }
      else if ("Value".equals(section)) {
        myValue = sectionText.toString();
      }
      else if ("Usage".equals(section)) {
        myUsage = sectionText.toString();
      }
      else if ("Examples".equals(section)) {
        myExamples = sectionText.toString();
      }
      else if ("Description".equals(section)) {
        myDescription = sectionText.toString();
      }
    }
  }
}
