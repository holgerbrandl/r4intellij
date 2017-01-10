/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.highlighting;

import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.options.colors.AttributesDescriptor;
import com.intellij.openapi.options.colors.ColorDescriptor;
import com.intellij.openapi.options.colors.ColorSettingsPage;
import com.intellij.openapi.util.IconLoader;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Map;

/**
 * RColorSettingsPage implementation
 * Created on 7/23/14.
 *
 * @author HongKee Moon
 */
public class RColorSettingsPage implements ColorSettingsPage {
    /**
     * The path to the sample .R file
     */
    protected static final String SAMPLE_R_SCRIPT = "\n" +
            "for (i in names(list)) {\n" +
            "   if(true)\n" +
            "   {\n" +
            "      #line comment\n" +
            "      names[,i] = 0\n" +
            "      names[,i+1] = test()\n" +
            "      names[,'added'] = \"string\"\n" +
            "   }\n" +
            "}\n";


//    for (i in names(list)) {
//        if(true)
//        {
//            #line comment
//            names[,i] = 0
//            names[,'added'] = "string"
//        }
//    }

    /**
     * The sample .R document shown in the colors settings dialog
     */
    private static final AttributesDescriptor[] DESCRIPTORS = new AttributesDescriptor[]{
            new AttributesDescriptor("Comment", RHighlighterColors.COMMENT_ATTR_KEY),
            new AttributesDescriptor("Keyword", RHighlighterColors.KEYWORD_ATTR_KEY),
            new AttributesDescriptor("Parenthesis", RHighlighterColors.PAREN_ATTR_KEY),
            new AttributesDescriptor("Braces", RHighlighterColors.BRACES_ATTR_KEY),
            new AttributesDescriptor("Brackets", RHighlighterColors.BRACKETS_ATTR_KEY),
            new AttributesDescriptor("Number", RHighlighterColors.NUMBER_ATTR_KEY),
            new AttributesDescriptor("String ...", RHighlighterColors.STRING_ATTR_KEY)
//            new AttributesDescriptor("Function Call", RHighlighterColors.FUNCALL_ATTR_KEY),
    };


    @Nullable
    @Override
    public Icon getIcon() {
        return IconLoader.findIcon("/icons/r_logo_16.png");
    }


    @NotNull
    @Override
    public SyntaxHighlighter getHighlighter() {
        return new RSyntaxHighlighter(null, null);
    }


    @NotNull
    @Override
    public String getDemoText() {
        return SAMPLE_R_SCRIPT;
    }


    @Nullable
    @Override
    public Map<String, TextAttributesKey> getAdditionalHighlightingTagToDescriptorMap() {
        return null;
    }


    @NotNull
    @Override
    public AttributesDescriptor[] getAttributeDescriptors() {
        return DESCRIPTORS;
    }


    @NotNull
    @Override
    public ColorDescriptor[] getColorDescriptors() {
        return ColorDescriptor.EMPTY_ARRAY;
    }


    @NotNull
    @Override
    public String getDisplayName() {
        return "R";
    }
}
