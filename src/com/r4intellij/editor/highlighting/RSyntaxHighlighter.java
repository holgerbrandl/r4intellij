/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.editor.highlighting;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.ui.SimpleTextAttributes;
import com.r4intellij.lang.lexer.RLexer;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;
import java.util.Map;

import static com.r4intellij.psi.RTypes.*;


/**
 * Defines R token highlighting and formatting.
 *
 * @author Holger Brandl
 */
public class RSyntaxHighlighter extends SyntaxHighlighterBase {

    private static final TokenSet lineCommentSet = TokenSet.create(R_COMMENT);
    private static final TokenSet parenthesisSet = TokenSet.create(R_LEFT_PAREN, R_RIGHT_PAREN);
    private static final TokenSet curlySet = TokenSet.create(R_LEFT_BRACE, R_RIGHT_BRACE);
    private static final TokenSet bracketSet = TokenSet.create(R_LEFT_BRACKET, R_RIGHT_BRACKET);
    private static final TokenSet string2Set = TokenSet.create(R_STR_CONST);
    private static final TokenSet numberSet = TokenSet.create(R_NUM_CONST);


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

    // http://confluence.jetbrains.com/pages/viewpage.action?pageId=49463468

    //custom high-lighting definitions
    private static final TextAttributes SHEBANG_ATTRIB = DefaultLanguageHighlighterColors.LINE_COMMENT.getDefaultAttributes().clone();

    public static final TextAttributesKey LINE_COMMENT = TextAttributesKey.createTextAttributesKey(LINE_COMMENT_ID);
    public static final TextAttributesKey KEYWORD = TextAttributesKey.createTextAttributesKey(KEYWORD_ID);
    public static final TextAttributesKey PAREN = TextAttributesKey.createTextAttributesKey(PAREN_ID);
    public static final TextAttributesKey BRACE = TextAttributesKey.createTextAttributesKey(BRACES_ID);
    public static final TextAttributesKey BRACKET = TextAttributesKey.createTextAttributesKey(BRACKETS_ID);
    public static final TextAttributesKey STRING2 = TextAttributesKey.createTextAttributesKey(STRING_ID);
    public static final TextAttributesKey NUMBER = TextAttributesKey.createTextAttributesKey(NUMBER_ID);

    private static final Map<IElementType, TextAttributesKey> attributes;

    static TokenSet keywords = TokenSet.create(R_ELSE, R_FOR, R_FUNCTION, R_IF, R_WHILE);


    static {
        //setup default attribute formatting
        SHEBANG_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);

        attributes = new HashMap<IElementType, TextAttributesKey>();
        fillMap(attributes, lineCommentSet, LINE_COMMENT);
        fillMap(attributes, keywords, KEYWORD);
        fillMap(attributes, parenthesisSet, PAREN);
        fillMap(attributes, curlySet, BRACE);
        fillMap(attributes, bracketSet, BRACKET);
        fillMap(attributes, string2Set, STRING2);
        fillMap(attributes, numberSet, NUMBER);
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


