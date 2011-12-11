/*
 * Copyright 2009 Holger Brandl
 * File: BashSyntaxHighlighter.java, Class: BashSyntaxHighlighter
 * Last modified: 2010-03-18
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.r4intellij.editor.highlighting;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.SyntaxHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.ui.SimpleTextAttributes;
import com.r4intellij.lang.lexer.RLexer;
import com.r4intellij.lang.lexer.RTokenTypes;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.util.HashMap;
import java.util.Map;


/**
 * Defines bash token highlighting and formatting.
 */
public class RSyntaxHighlighter extends SyntaxHighlighterBase implements RTokenTypes {

    private static final TokenSet lineCommentSet = TokenSet.create(COMMENT);
    private static final TokenSet parenthesisSet = TokenSet.create(LEFT_PAREN, RIGHT_PAREN);
    private static final TokenSet curlySet = TokenSet.create(LEFT_CURLY, RIGHT_CURLY);
    private static final TokenSet bracketSet = TokenSet.create(LEFT_SQUARE, RIGHT_SQUARE);
    private static final TokenSet string2Set = TokenSet.create(RTokenTypes.STRING_LITERAL);
    private static final TokenSet numberSet = TokenSet.create(RTokenTypes.NUMBER, INTEGER_LITERAL);
    private static final TokenSet internalCommandSet = TokenSet.create(RTokenTypes.INTERNAL_COMMAND);
    private static final TokenSet varUseSet = TokenSet.create(RTokenTypes.VARIABLE);
    private static final TokenSet badCharacterSet = TokenSet.create(RTokenTypes.BAD_CHARACTER);

    @NonNls
    public static final String LINE_COMMENT_ID = "Line comment";
    @NonNls
    public static final String KEYWORD_ID = "Keyword";
    @NonNls
    public static final String PAREN_ID = "Parenthesis";
    @NonNls
    public static final String BRACES_ID = "Braces";
    @NonNls
    public static final String BRACKETS_ID = "Brackets";
    @NonNls
    public static final String NUMBER_ID = "Number";
    @NonNls
    public static final String STRING_ID = "String \"...\"";
    @NonNls
    public static final String BAD_CHARACTER_ID = "Bad character";
    @NonNls
    public static final String INTERNAL_COMMAND_ID = "Build-in Bash command";

    @NonNls
    public static final String FUNCTION_CALL_ID = "Function call";
    @NonNls
    public static final String VAR_DEF_ID = "Variable declaration, e.g. a=1";
    @NonNls
    public static final String VAR_USE_ID = "Variable use $a";
    @NonNls
    public static final String VAR_USE_BUILTIN_ID = "Variable use of built-in ($PATH, ...)";

    //custom highlightings
    private static final TextAttributes SHEBANG_ATTRIB = SyntaxHighlighterColors.LINE_COMMENT.getDefaultAttributes().clone();
    public static final TextAttributes HERE_DOC_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    private static final TextAttributes HERE_DOC_START_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    private static final TextAttributes HERE_DOC_END_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    private static final TextAttributes BACKQUOTE_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    private static final TextAttributes INTERNAL_COMMAND_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    private static final TextAttributes EXTERNAL_COMMAND_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    private static final TextAttributes SUBSHELL_COMMAND_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    private static final TextAttributes FUNCTION_CALL_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    private static final TextAttributes VAR_DEF_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    public static final TextAttributes VAR_USE_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    public static final TextAttributes VAR_USE_INTERNAL_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();
    public static final TextAttributes VAR_USE_COMPOSED_ATTRIB = HighlighterColors.TEXT.getDefaultAttributes().clone();

    private static final TextAttributes STRING_ATTRIB = SyntaxHighlighterColors.STRING.getDefaultAttributes().clone();
    private static final TextAttributes STRING2_ATTRIB = SyntaxHighlighterColors.STRING.getDefaultAttributes().clone();

    static {
        //register
        TextAttributesKey.createTextAttributesKey(LINE_COMMENT_ID, SyntaxHighlighterColors.LINE_COMMENT.getDefaultAttributes());
        TextAttributesKey.createTextAttributesKey(KEYWORD_ID, SyntaxHighlighterColors.KEYWORD.getDefaultAttributes());
        TextAttributesKey.createTextAttributesKey(PAREN_ID, SyntaxHighlighterColors.PARENTHS.getDefaultAttributes());
        TextAttributesKey.createTextAttributesKey(BRACES_ID, SyntaxHighlighterColors.BRACES.getDefaultAttributes());
        TextAttributesKey.createTextAttributesKey(BRACKETS_ID, SyntaxHighlighterColors.BRACKETS.getDefaultAttributes());
        TextAttributesKey.createTextAttributesKey(BRACKETS_ID, SyntaxHighlighterColors.BRACKETS.getDefaultAttributes());
        TextAttributesKey.createTextAttributesKey(NUMBER_ID, SyntaxHighlighterColors.NUMBER.getDefaultAttributes());
        TextAttributesKey.createTextAttributesKey(STRING_ID, STRING2_ATTRIB);
        TextAttributesKey.createTextAttributesKey(INTERNAL_COMMAND_ID, INTERNAL_COMMAND_ATTRIB);
        TextAttributesKey.createTextAttributesKey(VAR_USE_ID, VAR_USE_ATTRIB);
        TextAttributesKey.createTextAttributesKey(BAD_CHARACTER_ID, HighlighterColors.BAD_CHARACTER.getDefaultAttributes());

        //psi highlighting
        TextAttributesKey.createTextAttributesKey(STRING_ID, STRING_ATTRIB);
        TextAttributesKey.createTextAttributesKey(FUNCTION_CALL_ID, FUNCTION_CALL_ATTRIB);
        TextAttributesKey.createTextAttributesKey(VAR_DEF_ID, VAR_DEF_ATTRIB);
        TextAttributesKey.createTextAttributesKey(VAR_USE_BUILTIN_ID, VAR_USE_INTERNAL_ATTRIB);
    }

    public static final TextAttributesKey LINE_COMMENT = TextAttributesKey.createTextAttributesKey(LINE_COMMENT_ID);
    public static final TextAttributesKey KEYWORD = TextAttributesKey.createTextAttributesKey(KEYWORD_ID);
    public static final TextAttributesKey PAREN = TextAttributesKey.createTextAttributesKey(PAREN_ID);
    public static final TextAttributesKey BRACE = TextAttributesKey.createTextAttributesKey(BRACES_ID);
    public static final TextAttributesKey BRACKET = TextAttributesKey.createTextAttributesKey(BRACKETS_ID);
    public static final TextAttributesKey STRING2 = TextAttributesKey.createTextAttributesKey(STRING_ID);
    public static final TextAttributesKey NUMBER = TextAttributesKey.createTextAttributesKey(NUMBER_ID);
    public static final TextAttributesKey INTERNAL_COMMAND = TextAttributesKey.createTextAttributesKey(INTERNAL_COMMAND_ID);
    public static final TextAttributesKey VAR_USE = TextAttributesKey.createTextAttributesKey(VAR_USE_ID);
    public static final TextAttributesKey BAD_CHARACTER = TextAttributesKey.createTextAttributesKey(BAD_CHARACTER_ID);

    //psi highlighting
    public static final TextAttributesKey FUNCTION_CALL = TextAttributesKey.createTextAttributesKey(FUNCTION_CALL_ID);
    public static final TextAttributesKey VAR_DEF = TextAttributesKey.createTextAttributesKey(VAR_DEF_ID);
    public static final TextAttributesKey VAR_USE_BUILTIN = TextAttributesKey.createTextAttributesKey(VAR_USE_BUILTIN_ID);

    //
    public static final TextAttributesKey NONE = HighlighterColors.TEXT;

    private static final Map<IElementType, TextAttributesKey> attributes;

    static {
        //setup default attribute formatting
        SHEBANG_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);

        VAR_USE_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);
        VAR_USE_ATTRIB.setForegroundColor(new Color(0, 0, 204)); //dark blue

        VAR_USE_INTERNAL_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);
        VAR_USE_INTERNAL_ATTRIB.setForegroundColor(new Color(0, 0, 204)); //dark blue

        VAR_USE_COMPOSED_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);
        VAR_USE_COMPOSED_ATTRIB.setForegroundColor(new Color(0, 0, 204)); //dark blue

        VAR_DEF_ATTRIB.setForegroundColor(new Color(51, 51, 255)); //lighter blue

        HERE_DOC_ATTRIB.setBackgroundColor(new Color(204, 255, 204)); //light green background
        HERE_DOC_START_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);
        HERE_DOC_START_ATTRIB.setForegroundColor(new Color(0, 153, 0)); //dark green
        HERE_DOC_END_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);
        HERE_DOC_END_ATTRIB.setForegroundColor(new Color(0, 153, 0)); // dark greem

        FUNCTION_CALL_ATTRIB.setFontType(SimpleTextAttributes.STYLE_ITALIC);

        EXTERNAL_COMMAND_ATTRIB.setFontType(SimpleTextAttributes.STYLE_ITALIC);

        BACKQUOTE_ATTRIB.setFontType(SimpleTextAttributes.STYLE_ITALIC);

        INTERNAL_COMMAND_ATTRIB.setFontType(SimpleTextAttributes.STYLE_ITALIC | SimpleTextAttributes.STYLE_BOLD);

        SUBSHELL_COMMAND_ATTRIB.setFontType(SimpleTextAttributes.STYLE_ITALIC);

        attributes = new HashMap<IElementType, TextAttributesKey>();
        fillMap(attributes, lineCommentSet, LINE_COMMENT);
        fillMap(attributes, keywords, KEYWORD);
        fillMap(attributes, parenthesisSet, PAREN);
        fillMap(attributes, curlySet, BRACE);
        fillMap(attributes, bracketSet, BRACKET);
        fillMap(attributes, string2Set, STRING2);
        fillMap(attributes, numberSet, NUMBER);
        fillMap(attributes, internalCommandSet, INTERNAL_COMMAND);
        fillMap(attributes, varUseSet, VAR_USE);
        fillMap(attributes, badCharacterSet, BAD_CHARACTER);
    }

    @NotNull
    public Lexer getHighlightingLexer() {
        return new RLexer();
    }

    @NotNull
    public TextAttributesKey[] getTokenHighlights(final IElementType tokenType) {
        return pack(attributes.get(tokenType));
    }
}


