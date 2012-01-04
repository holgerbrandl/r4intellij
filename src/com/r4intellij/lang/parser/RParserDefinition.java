/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang.parser;

import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
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
import com.r4intellij.file.RFileType;
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

    @NotNull
    public Lexer createLexer(Project project) {
        return new RLexer();
    }

    public PsiParser createParser(Project project) {
        return new RDummyParser();
    }


    IFileElementType FILE = new IFileElementType(RFileType.R_LANGUAGE);

    public IFileElementType getFileNodeType() {

        return FILE;
    }

    @NotNull
    public TokenSet getWhitespaceTokens() {
        return TokenSet.create(TokenType.WHITE_SPACE);
    }

    @NotNull
    public TokenSet getCommentTokens() {
        return TokenSet.create(R_COMMENT);
    }

    @NotNull
    public TokenSet getStringLiteralElements() {
        return TokenSet.create(R_STR_CONST);
    }

    public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode leftAst, ASTNode rightAst) {
        final IElementType left = leftAst.getElementType();
        final IElementType right = rightAst.getElementType();

        if (left == R_LEFT_PAREN
                || right == R_RIGHT_PAREN
                || left == R_RIGHT_PAREN
                || right == R_LEFT_PAREN

                || left == R_LEFT_BRACE
                || right == R_LEFT_BRACE
                || left == R_RIGHT_BRACE
                || right == R_RIGHT_BRACE

//                || (left == WORD && right == PARAM_EXPANSION_OP)

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
//        final IElementType type = node.getElementType();
//        if (type == FILE) {
//            return new Root(node);
//        } else if (type == FUNCTION_DEFINITION) {
//            return new Def(node);
//        } else if (type == ANONYMOUS_FUNCTION_DEFINITION) {
//            return new Fn(node);
//        } else if (type == SINGLE_ARG_ANONYMOUS_FUNCTION_DEFINITION) {
//            return new SingleArgFn(node);
//        } else if (type == MACRO_DEFINITION) {
//            return new Mac(node);
//        } else if (type == EXPRESSION) {
//            return new Expression(node);
//        } else if (type == VARIABLE_ASSIGNMENT) {
//            return new VariableAssignment(node);
//        } else if (type == OPTIONAL_PARAMETER) {
//            return new OptionalParameter(node);
//        } else if (type == REST_PARAMETER) {
//            return new RestParameter(node);
//        } else if (type == PARAMETER) {
//            return new Parameter(node);
//        } else if (type == VARIABLE_DEFINITION) {
//            return new VariableDefinition(node);
//        } else if (type == VARIABLE_REFERENCE) {
//            return new VariableReference(node);
//        } else if (type == IF_BLOCK) {
//            return new If(node);
//        } else if (type == LET_BLOCK) {
//            return new Let(node);
//        } else if (type == WITH_BLOCK) {
//            return new With(node);
//        } else if (type == PARAMETER_LIST) {
//            return new ParameterList(node);
//        } else if (type == DOCSTRING) {
//            return new Docstring(node);
//        } else if (type == BACKQUOTED_EXPRESSION) {
//            return new BackquotedExpression(node);
//        } else if (type == QUOTED_EXPRESSION) {
//            return new QuotedExpression(node);
//        } else if (type == COMMA_AT_EXPRESSION) {
//            return new CommaAtExpression(node);
//        } else if (type == COMMA_EXPRESSION) {
//            return new CommaExpression(node);
//        } else if (type == LITERAL) {
//            return new Literal(node);
//        }

        return new ASTWrapperPsiElement(node);
    }

    public PsiFile createFile(FileViewProvider viewProvider) {
        return new RFile(viewProvider);
    }
}
