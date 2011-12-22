/*
 * Copyright 2011 Holger Brandl
 *
 * This code is licensed under BSD. For details see
 * http://www.opensource.org/licenses/bsd-license.php
 */

package com.r4intellij.lang.parser;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.Stack;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.lang.lexer.RTokenTypes.STRING_LITERAL;


/**
 * Walk through the token stream of a Arc source file and generate the appropriate PSI tree.
 *
 * @author Holger Brandl
 */
public class RDummyParser implements PsiParser {

    private Stack<PsiBuilder.Marker> markers = new Stack<PsiBuilder.Marker>();
    private PsiBuilder builder;

    @NotNull
    @Override
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        this.builder = builder;

        // TODO - Delete me!
        builder.setDebugMode(true);


        final PsiBuilder.Marker rootMarker = builder.mark();

        try {
            while (!builder.eof()) {
                markAndAdvance(STRING_LITERAL);
//                parseNext();
            }
        } catch (EofException e) {
            while (!markers.empty()) {
                drop();
            }
        }

        rootMarker.done(root);
        return builder.getTreeBuilt();
    }

//    private void parseNext() {
//        if (isAt(LEFT_PAREN)) {
//            parseExpression();
//        } else if (isAt(LEFT_SQUARE)) {
//            parseSingleArgFn();
//        } else if (isAt(SYMBOL)) {
//            markAndAdvance(VARIABLE_REFERENCE);
//        } else if (isAtMacroTemplateToken()) {
//            advance();
//        } else {
//            // TODO - We can actually have error conditions at this point - for example, when parseNext gets called, we're not expecting a right paren, but we could get one!
//            markAndAdvance(LITERAL);
//        }
//    }
//
//    private void parseSingleArgFn() {
//        markAndAdvance();
//        parseBody(RIGHT_SQUARE);
//        done(SINGLE_ARG_ANONYMOUS_FUNCTION_DEFINITION);
//    }
//
//    private void parseExpression() {
//        markAndAdvance();
//        IElementType type = getExpressionType();
//        if (isAt(DEF) || isAt(MAC)) {
//            advance(); // Advance past def/mac token
//            parseName();
//            parseParameters();
//            parseDocstring();
//        } else if (isAt(EQ)) {
//            advance(); // Advance past = token
//            if (isAt (SYMBOL)) {
//                markAndAdvance(VARIABLE_DEFINITION);
//            } else {
//                type = EXPRESSION;
//            }
//        } else if (isAt(IF)) {
//            advance(); // Advance past if token
//        } else if (isAt(LET)) {
//            advance(); // Advance past let token
//            parseBinding();
//        } else if (isAt(WITH)) {
//            advance(); // Advance past with token
//            if (isAt(LEFT_PAREN)) {
//                advance();
//                while (!isAt(RIGHT_PAREN)) {
//                    parseBinding();
//                }
//                advance();
//            }
//        }
//        parseBody(RIGHT_PAREN);
//        done(type);
//    }
//
//    private boolean isAtMacroTemplateToken() {
//        return isAt(BACKQUOTE) || isAt(QUOTE) || isAt(COMMA) || isAt(COMMA_AT);
//    }
//
//    private void parseBinding() {
//        if (isAtMacroTemplateToken()) {
//            advance();
//        }
//
//        if (isAt(LEFT_PAREN)) {
//            parseExpression();
//        } else {
//            parseName();
//        }
//
//        parseNext();
//    }
//
//    private void parseBody(IElementType teminator) {
//        while (!isAt(teminator)) {
//            parseNext();
//        }
//        advance();
//    }
//
//    private void parseName() {
//        if (isAtMacroTemplateToken()) {
//            advance();
//        }
//        if (isAt (SYMBOL) || isIn(KEYWORDS) || isIn(SPECIAL_CHARACTERS)) { // Arc will blissfully let us redefine... anything!
//            markAndAdvance(VARIABLE_DEFINITION);
//        } else {
//            builder.error("Expected identifier"); // TODO - Prop-ify me!!
//        }
//    }
//
//    private void parseParameters() {
//        mark();
//        if (isAt(SYMBOL)) {
//            markAndAdvance(REST_PARAMETER);
//        } else if (isAt(LEFT_PAREN)) {
//            advance();
//            while (!isAt(RIGHT_PAREN)) {
//                parseParameter();
//            }
//            advance();
//        } else {
//            advance();
//            builder.error("Expected parameter");
//        }
//        done(PARAMETER_LIST);
//    }
//
//    private void parseParameter() {
//        if (isAtMacroTemplateToken()) {
//            advance();
//        }
//
//        if (isAt(SYMBOL)) {
//            markAndAdvance(PARAMETER);
//        } else if (isAt(LEFT_PAREN)) {
//            parseOptionalParameter();
//        } else if (isAt(DOT)) {
//            advance();
//            markAndAdvance(REST_PARAMETER);
//        } else {
//            advance();
//            builder.error("Expected parameter");
//        }
//    }
//
//    private void parseOptionalParameter() {
//        markAndAdvance(); // Advance past left paren
//        advance(); // Advance past 'o' token
//        parseName();
//        parseBody(RIGHT_PAREN);
//        done(OPTIONAL_PARAMETER);
//    }
//
//    private void parseDocstring() {
//        if (isAt(STRING_LITERAL)) {
//            markAndAdvance();
//
//            // If the string is the *entire* body of a def/mac/fn, then it is *not* a docstring...
//            if (isAt(RIGHT_PAREN)) {
//                drop();
//            } else {
//                done(DOCSTRING);
//            }
//        }
//    }

    private void done(IElementType type) {
        try {
            markers.pop().done(type);
        } catch (RuntimeException e) {
            throw new RuntimeException(builder.getTokenText(), e);
        }
    }

    private void drop() {
        try {
            markers.pop().drop();
        } catch (RuntimeException e) {
            throw new RuntimeException(builder.getTokenText(), e);
        }
    }

    private void mark() {
        markers.push(builder.mark());
    }

    private void advance() {
        if (builder.eof()) {
            throw new EofException();
        }
        builder.advanceLexer();
    }

    private void markAndAdvance(IElementType type) {
        markAndAdvance();
        done(type);
    }

    private void markAndAdvance() {
        mark();
        advance();
    }
//
//    private boolean isAt(IElementType token) {
//        return builder.getTokenType() == token;
//    }
//
//    private boolean isIn(TokenSet set) {
//        return set.contains(builder.getTokenType());
//    }
//
//    private IElementType getExpressionType() {
//        if (isAt(DEF)) {
//            return FUNCTION_DEFINITION;
//        } else if (isAt(MAC)) {
//            return MACRO_DEFINITION;
//        } else if (isAt(FN)) {
//            return ANONYMOUS_FUNCTION_DEFINITION;
//        } else if (isAt(EQ)) {
//            return VARIABLE_ASSIGNMENT;
//        } else if (isAt(IF)) {
//            return IF_BLOCK;
//        } else if (isAt(LET)) {
//            return LET_BLOCK;
//        } else if (isAt(WITH)) {
//            return WITH_BLOCK;
//        }
//        return EXPRESSION;
//    }

    private class EofException extends RuntimeException {

    }

}