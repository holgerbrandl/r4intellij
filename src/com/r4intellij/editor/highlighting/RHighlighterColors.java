/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.highlighting;

import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;

/**
 * RHighterColors description
 * <p/>
 * Created on 7/23/14.
 *
 * @author HongKee Moon
 */
public class RHighlighterColors {
    /**
     * Default style for regular comment started with #
     */
    public static final TextAttributesKey COMMENT_ATTR_KEY = TextAttributesKey.createTextAttributesKey("R.COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);

    /**
     * Default style for keyword
     */
    public static final TextAttributesKey KEYWORD_ATTR_KEY = TextAttributesKey.createTextAttributesKey("R.KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);

    /**
     * Default style for parentheses
     */
    public static final TextAttributesKey PAREN_ATTR_KEY = TextAttributesKey.createTextAttributesKey("R.PAREN", DefaultLanguageHighlighterColors.PARENTHESES);

    /**
     * Default style for braces
     */
    public static final TextAttributesKey BRACES_ATTR_KEY = TextAttributesKey.createTextAttributesKey("R.BRACES", DefaultLanguageHighlighterColors.BRACES);

    /**
     * Default style for brackets
     */
    public static final TextAttributesKey BRACKETS_ATTR_KEY = TextAttributesKey.createTextAttributesKey("R.BRACKETS", DefaultLanguageHighlighterColors.BRACKETS);

    /**
     * Default style for number
     */
    public static final TextAttributesKey NUMBER_ATTR_KEY = TextAttributesKey.createTextAttributesKey("R.NUMBER", DefaultLanguageHighlighterColors.NUMBER);

    /**
     * Default style for string
     */
    public static final TextAttributesKey STRING_ATTR_KEY = TextAttributesKey.createTextAttributesKey("R.STRING", DefaultLanguageHighlighterColors.STRING);


//    public static final TextAttributesKey FUNCALL_ATTR_KEY = TextAttributesKey.createTextAttributesKey("R.FUNCALL", DefaultLanguageHighlighterColors.FUNCTION_CALL);

}
