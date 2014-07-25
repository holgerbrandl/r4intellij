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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.ui.SimpleTextAttributes;
import com.r4intellij.lang.lexer.RLexer;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.HashMap;
import java.util.Map;

import static com.r4intellij.psi.RTypes.*;


/**
 * Defines R token highlighting and formatting.
 *
 * @author Holger Brandl
 * @author HongKee Moon
 */
public class RSyntaxHighlighter extends SyntaxHighlighterBase {

    private static final TokenSet lineCommentSet = TokenSet.create(R_COMMENT);
    private static final TokenSet parenthesisSet = TokenSet.create(R_LEFT_PAREN, R_RIGHT_PAREN);
    private static final TokenSet curlySet = TokenSet.create(R_LEFT_BRACE, R_RIGHT_BRACE);
    private static final TokenSet bracketSet = TokenSet.create(R_LEFT_BRACKET, R_RIGHT_BRACKET);
    private static final TokenSet string2Set = TokenSet.create(R_STR_CONST);
    private static final TokenSet numberSet = TokenSet.create(R_NUM_CONST);

    //custom high-lighting definitions
    private static final TextAttributes SHEBANG_ATTRIB = DefaultLanguageHighlighterColors.LINE_COMMENT.getDefaultAttributes().clone();

    private static final Map<IElementType, TextAttributesKey> attributes;

    static TokenSet keywords = TokenSet.create(R_ELSE, R_FOR, R_FUNCTION, R_IF, R_WHILE, R_IN, R_BREAK, R_LEFT_ASSIGN, R_EQ_ASSIGN);

	static TokenSet variable = TokenSet.create(R_SYMBOL);

	static TokenSet funcall = TokenSet.create(R_FUNCALL);

    static {
        //setup default attribute formatting
        SHEBANG_ATTRIB.setFontType(SimpleTextAttributes.STYLE_BOLD);

        attributes = new HashMap<IElementType, TextAttributesKey>();
        fillMap(attributes, lineCommentSet, RHighlighterColors.COMMENT_ATTR_KEY);
        fillMap(attributes, keywords, RHighlighterColors.KEYWORD_ATTR_KEY);
        fillMap(attributes, parenthesisSet, RHighlighterColors.PAREN_ATTR_KEY);
        fillMap(attributes, curlySet, RHighlighterColors.BRACES_ATTR_KEY);
        fillMap(attributes, bracketSet, RHighlighterColors.BRACKETS_ATTR_KEY);
        fillMap(attributes, string2Set, RHighlighterColors.STRING_ATTR_KEY);
        fillMap(attributes, numberSet, RHighlighterColors.NUMBER_ATTR_KEY);
		fillMap(attributes, variable, RHighlighterColors.VARIABLE_ATTR_KEY);
		fillMap(attributes, funcall, RHighlighterColors.FUNCALL_ATTR_KEY);
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
		//System.out.println(tokenType.getClass().toString() + tokenType.toString());
        return pack(attributes.get(tokenType));
    }
}


