/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.highlighting;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.ui.SimpleTextAttributes;
import com.r4intellij.lexer.RLexer;
import com.r4intellij.parsing.RParserDefinition;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;

import static com.r4intellij.parsing.RElementTypes.*;


/**
 * Defines R token highlighting and formatting.
 * <p>
 * See https://confluence.jetbrains.com/display/IntelliJIDEA/Syntax+Highlighter+and+Color+Settings+Page
 *
 * @author Holger Brandl
 * @author HongKee Moon
 */
public class RSyntaxHighlighter extends SyntaxHighlighterBase {

    private static final TokenSet lineCommentSet = TokenSet.create(RParserDefinition.END_OF_LINE_COMMENT);
    private static final TokenSet parenthesisSet = TokenSet.create(R_LPAR, R_RPAR);
    private static final TokenSet curlySet = TokenSet.create(R_LBRACE, R_RBRACE);
    private static final TokenSet bracketSet = TokenSet.create(R_LBRACKET, R_RBRACKET);
    private static final TokenSet string2Set = TokenSet.create(R_STRING);
    private static final TokenSet numberSet = TokenSet.create(R_NUMERIC);

    //custom high-lighting definitions
    private static final TextAttributes SHEBANG_ATTRIB = DefaultLanguageHighlighterColors.LINE_COMMENT.getDefaultAttributes().clone();

    private static final Map<IElementType, TextAttributesKey> attributes;

    static final TokenSet keywords = TokenSet.create(R_ELSE, R_FOR, R_FUNCTION, R_IF, R_WHILE);

    static {
        //setup default attribute formatting
        SHEBANG_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);

        attributes = new HashMap<IElementType, TextAttributesKey>();

        fillMap(attributes, lineCommentSet, RSyntaxHighlighterColors.COMMA);
        fillMap(attributes, keywords, RSyntaxHighlighterColors.KEYWORD);
        fillMap(attributes, parenthesisSet, RSyntaxHighlighterColors.PARENTHS);
        fillMap(attributes, curlySet, RSyntaxHighlighterColors.BRACES);
        fillMap(attributes, bracketSet, RSyntaxHighlighterColors.BRACKETS);
        fillMap(attributes, string2Set, RSyntaxHighlighterColors.STRING);
        fillMap(attributes, numberSet, RSyntaxHighlighterColors.NUMBER);
    }


    private final Project project;
    private final VirtualFile virtualFile;


    public RSyntaxHighlighter(@Nullable Project project, @Nullable VirtualFile virtualFile) {
        this.project = project;
        this.virtualFile = virtualFile;
    }


    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new RLexer();
    }


    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(final IElementType tokenType) {
        return pack(attributes.get(tokenType));
    }
}


