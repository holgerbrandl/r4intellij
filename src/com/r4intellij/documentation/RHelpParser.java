package com.r4intellij.documentation;

import com.google.common.base.Joiner;
import com.google.common.collect.Maps;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class RHelpParser {

    private static final Pattern ARG_PATTERN = Pattern.compile("^.+:");


    private final Map<String, List<String>> sections;
    private final String title;
    private final String docHeader;


    public RHelpParser(@NotNull final String documentationText) {

        // parse help text
        final String[] lines = documentationText.split("\n");
        Predicate<String> isSectionHeader = s -> s.startsWith("_");
        List<List<String>> sectionsRaw = splitIntoSections(Arrays.asList(lines), isSectionHeader);


        List<String> headerSection = sectionsRaw.remove(0);
        String[] splitDocHeader = headerSection.get(1).split("[ ]+");
        docHeader = splitDocHeader[1].replace("package:", "") + "::" + splitDocHeader[0];

        title = sectionsRaw.remove(0).get(0).replaceAll("_\b", "").replaceAll(":$", "");

        // remodel into header content map
        sections = Maps.newHashMap();
        for (List<String> section : sectionsRaw) {
            String sectionHeader = section.get(0);
            sectionHeader = sectionHeader.replaceAll("_\b", "");
            sectionHeader = sectionHeader.replaceAll(":$", "");

            sections.put(sectionHeader, section.subList(1, section.size()));
        }
    }


    // see http://stackoverflow.com/questions/29095967/splitting-list-into-sublists-along-elements/29111023#29111023
    private static List<List<String>> splitIntoSections(List<String> input, Predicate<String> isSectionHeader) {
        List<List<String>> result = new ArrayList<>();
        int lastSectionStart = 0;

        for (int cur = 0; cur < input.size(); cur++) {
            if (isSectionHeader.test(input.get(cur))) {
                result.add(input.subList(lastSectionStart, cur));

                lastSectionStart = cur;
            }
        }
        result.add(input.subList(lastSectionStart, input.size()));

        return result;
    }


    @NotNull
    public String getFormattedString() {
        final StringBuilder builder = new StringBuilder();

        builder.append(docHeader);

        if (getTitle() != null) {
            builder.append("<br>\n<h2>" + getTitle() + "</h2>");
        }


        for (String sectionHeader : sections.keySet()) {
            builder.append("<br>\n<b><u>" + sectionHeader + ":</u></b>");

            builder.append("<br>\n");

            if (sectionHeader.equals("Arguments")) {
                List<String> args = sections.get(sectionHeader);
                for (String arg : args) {
                    formatAndAppend(builder, arg);
                }

            } else if (Arrays.asList("Examples", "Usage").contains(sectionHeader)) {
                builder.append("<code>" + Joiner.on("<br>\n").join(sections.get(sectionHeader)) + "</code>");

            } else {
                builder.append(Joiner.on("<br>\n").join(sections.get(sectionHeader)));
            }

//            builder.append("</p>");

        }

        return builder.toString();
    }


    private static void formatAndAppend(@NotNull final StringBuilder builder, @NotNull final String docString) {
        builder.append("<br/>");
        final Matcher matcher = ARG_PATTERN.matcher(docString);
        if (matcher.find()) {
            builder.append(matcher.replaceFirst("<b>$0</b>"));
        } else {
            builder.append(docString);
        }
        builder.append(" ");
    }


    public String getTitle() {
        return title;
    }


    public String getDocArguments() {
        return Joiner.on("\n").join(sections.get("Arguments"));
    }


    public String getDocValue() {
        return Joiner.on("\n").join(sections.get("Value"));
    }


    public String getDocUsage() {
        return Joiner.on("\n").join(sections.get("Usage"));
    }


    public String getDocExamples() {
        return Joiner.on("\n").join(sections.get("Examples"));
    }
}
