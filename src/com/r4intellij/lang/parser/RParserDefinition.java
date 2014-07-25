/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;
import com.r4intellij.lang.RLanguage;
import com.r4intellij.lang.lexer.RLexer;
import com.r4intellij.psi.RFile;
import com.r4intellij.psi.RTypes;
import org.jetbrains.annotations.NotNull;


/**
 * DOCUMENT ME!
 *
 * @author Holger Brandl
 */
public class RParserDefinition implements ParserDefinition, RTypes {

	public static final TokenSet WHITE_SPACES = TokenSet.create(TokenType.WHITE_SPACE);
	public static final TokenSet COMMENTS = TokenSet.create(R_COMMENT);
	public static final TokenSet STRING_LITERAL = TokenSet.create(R_COMMENT);

	public static final IFileElementType FILE = new IFileElementType(Language.<RLanguage>findInstance(RLanguage.class));

    @NotNull
	@Override
    public Lexer createLexer(Project project) {
        return new RLexer();
    }

	@NotNull
	public TokenSet getWhitespaceTokens() {
		return WHITE_SPACES;
	}

	@NotNull
	public TokenSet getCommentTokens() {
		return COMMENTS;
	}

	@NotNull
	public TokenSet getStringLiteralElements() {
		return STRING_LITERAL;
	}

    public PsiParser createParser(Project project) {
        return new RLangParser();
    }


    public IFileElementType getFileNodeType() {

        return FILE;
    }

    public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode leftAst, ASTNode rightAst) {
        final IElementType left = leftAst.getElementType();
        final IElementType right = rightAst.getElementType();

        if (left == R_NUM_CONST && right == R_SYMBOL) return SpaceRequirements.MUST;

        if (left == R_LEFT_PAREN
                || right == R_RIGHT_PAREN
                || left == R_RIGHT_PAREN
                || right == R_LEFT_PAREN

                || left == R_LEFT_BRACE
                || right == R_LEFT_BRACE
                || left == R_RIGHT_BRACE
                || right == R_RIGHT_BRACE

                || left == R_LEFT_BRACKET
                || right == R_RIGHT_BRACKET
                || left == R_RIGHT_BRACKET
                || right == R_LEFT_BRACKET

                || left == R_SYMBOL
                || right == R_SYMBOL) {

            return SpaceRequirements.MAY;
        }

        return SpaceRequirements.MUST;
    }


    @NotNull
    public PsiElement createElement(ASTNode node) {
        return RTypes.Factory.createElement(node);
    }


    public PsiFile createFile(FileViewProvider viewProvider) {
        return new RFile(viewProvider);
    }
}
