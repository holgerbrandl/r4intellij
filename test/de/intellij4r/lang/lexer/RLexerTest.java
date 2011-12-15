package de.intellij4r.lang.lexer;

/*
 * Copyright (c) Kurt Christensen, 2009
 *
 *  Licensed under the Artistic License, Version 2.0 (the "License"); you may not use this
 *  file except in compliance with the License. You may obtain a copy of the License at:
 *
 *  http://www.opensource.org/licenses/artistic-license-2.0.php
 *
 *  Unless required by applicable law or agreed to in writing, software distributed under
 *  the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS
 *  OF ANY KIND, either express or implied. See the License for the specific language
 *  governing permissions and limitations under the License..
 */

import com.intellij.psi.tree.IElementType;
import com.r4intellij.Utils;
import com.r4intellij.lang.lexer.RLexer;
import org.junit.Assert;
import org.junit.Test;

import static com.r4intellij.lang.lexer.RTokenTypes.*;


public class RLexerTest {

    private static void printTokenization(String code) {
        RLexer lexer = new RLexer();
        lexer.start(new StringBuffer(code));

        // just print all tokens
        while (lexer.getTokenType() != null) {
            IElementType tokenType = lexer.getTokenType();
            System.out.println(tokenType);
            lexer.advance();
        }

    }

    private void testTokenization(String code, IElementType[] expectedTokens) {
        printTokenization(code);

        RLexer lexer = new RLexer();
        lexer.start(new StringBuffer(code));

        // do the comparison
        lexer.start(new StringBuffer(code));
        for (IElementType expectedToken : expectedTokens) {
            IElementType tokenType = lexer.getTokenType();
            Assert.assertEquals(expectedToken, tokenType);
            lexer.advance();
        }

        Assert.assertNull("surplus tokens in input:" + lexer.getTokenType(), lexer.getTokenType());
    }

    @Test
    public void testSimplePrint() {
        testTokenization("-(wait, \"foobar23\") # uhuh",
                new IElementType[]{
                        ARITH_MINUS,
                        LEFT_PAREN,
                        INTERNAL_COMMAND,
                        COMMA,
                        STRING_LITERAL,
                        RIGHT_PAREN,
                        COMMENT
                });
    }

    @Test
    public void testCtrlCharacterString() {
        testTokenization("print(\"\\t\")",
                new IElementType[]{
                        INTERNAL_COMMAND,
                        LEFT_PAREN,
                        STRING_LITERAL,
                        RIGHT_PAREN,
                });
    }

    @Test
    public void testMultiCharacterString() {
        testTokenization("install.packages(\"plyr\",\"ggplot2\",type=\"source\")",
                new IElementType[]{
                        IDENTIFIER,
                        LEFT_PAREN,
                        STRING_LITERAL,
                        COMMA,
                        STRING_LITERAL,
                        COMMA,
                        IDENTIFIER,
                        ASSIGNMENT,
                        STRING_LITERAL,
                        RIGHT_PAREN
                });
    }


    @Test
    public void testSimpleFunctionCall() {
        testTokenization("foo <- bar(tt, par=23); # ddff",
                new IElementType[]{
                        IDENTIFIER,
                        ASSIGNMENT,
                        IDENTIFIER,
                        LEFT_PAREN,
                        IDENTIFIER,
                        COMMA,
                        IDENTIFIER,
                        ASSIGNMENT,
                        NUMBER,
                        RIGHT_PAREN,
                        SEMICOLON,
                        COMMENT
                });
    }


    @Test
    public void testMultiLineBlockCommentTokenization() {
        testTokenization("# This \r\n # a long \r\n # comment (+ 1 2)\r\n 1+1;",
                new IElementType[]{
                        COMMENT,
                        COMMENT,
                        COMMENT,
                        NUMBER,
                        ARITH_PLUS,
                        NUMBER,
                        SEMICOLON
                });
    }

    @Test
    public void testComplexTokenization() {
        String testData = Utils.readFileAsString("/Users/brandl/find_prion_domains.R");
//        String testData = Utils.readFileAsString("misc/normality tests.R");
        printTokenization(testData);
    }


}
