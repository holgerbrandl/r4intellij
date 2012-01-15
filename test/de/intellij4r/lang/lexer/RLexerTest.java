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

import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.r4intellij.Utils;
import com.r4intellij.lang.lexer.RLexer;
import org.junit.Assert;
import org.junit.Test;

import static com.r4intellij.psi.RTypes.*;


public class RLexerTest {

    public static void main(String[] args) {
//        printTokenization("bxCxNames <- paste(\".boxcox\", sep = \"\");", true);
        String code = "\n" +
                "createPyTable <- function(cmd,...){\n" +
                "\tresult <- read.table(tFile, ...)\n" +
                "\tunlink(tFile)\n" +
                "\treturn(result)\n" +
                "}";
        code = Utils.readFileAsString("misc/Test.R");

        printTokenization(code, true);
    }

    private static void printTokenization(String code, boolean consumeWhiteSpaces) {
        RLexer lexer = new RLexer();
        lexer.start(new StringBuffer(code));
        consumeWhiteSpaces(consumeWhiteSpaces, lexer);

        // just print all tokens
        while (lexer.getTokenType() != null) {
            IElementType tokenType = lexer.getTokenType();
            System.out.println(tokenType);
            lexer.advance();
            consumeWhiteSpaces(consumeWhiteSpaces, lexer);

        }

    }

    private void testTokenization(String code, IElementType[] expectedTokens, boolean consumeWhiteSpaces) {
        printTokenization(code, true);

        RLexer lexer = new RLexer();
        lexer.start(new StringBuffer(code));

        consumeWhiteSpaces(consumeWhiteSpaces, lexer);

        // do the comparison
        lexer.start(new StringBuffer(code));
        for (IElementType expectedToken : expectedTokens) {
            IElementType tokenType = lexer.getTokenType();
            Assert.assertEquals(expectedToken, tokenType);
            lexer.advance();

            consumeWhiteSpaces(consumeWhiteSpaces, lexer);
        }

        Assert.assertNull("surplus tokens in input:" + lexer.getTokenType(), lexer.getTokenType());
    }

    private static void consumeWhiteSpaces(boolean consumeWhiteSpaces, RLexer lexer) {
        if (consumeWhiteSpaces) {
            while (lexer.getTokenType() != null && lexer.getTokenType() == TokenType.WHITE_SPACE) {
                lexer.advance();
            }
        }
    }

    @Test
    public void testSimplePrint() {
        testTokenization("-(wait, \"foobar23\") # uhuh",
                new IElementType[]{
                        R_ARITH_MINUS,
                        R_LEFT_PAREN,
                        R_SYMBOL,
                        R_COMMA,
                        R_STR_CONST,
                        R_RIGHT_PAREN,
                        R_COMMENT
                }, true);
    }

    @Test
    public void testCtrlCharacterString() {
        testTokenization("print(\"\\t\")",
                new IElementType[]{
                        R_SYMBOL,
                        R_LEFT_PAREN,
                        R_STR_CONST,
                        R_RIGHT_PAREN,
                }, true);
    }

    @Test
    public void testFunDef() {
        testTokenization("tt <- function(a=3,...);",
                new IElementType[]{
                        R_SYMBOL,
                        R_LEFT_ASSIGN,
                        R_FUNCTION,
                        R_LEFT_PAREN,
                        R_SYMBOL,
                        R_EQ_ASSIGN,
                        R_NUM_CONST,
                        R_COMMA,
                        R_SYMBOL_FORMALS,
                        R_RIGHT_PAREN,
                        R_SEMICOLON
                }, true);
    }

    @Test
    public void testMultiCharacterString() {
        testTokenization("install.packages(\"plyr\",\"ggplot2\",type=\"source\")",
                new IElementType[]{
                        R_SYMBOL,
                        R_LEFT_PAREN,
                        R_STR_CONST,
                        R_COMMA,
                        R_STR_CONST,
                        R_COMMA,
                        R_SYMBOL,
                        R_EQ_ASSIGN,
                        R_STR_CONST,
                        R_RIGHT_PAREN
                }, true);
    }


    @Test
    public void testSimpleFunctionCall() {
        testTokenization("foo <- bar(tt, par=23); # ddff",
                new IElementType[]{
                        R_SYMBOL,
                        R_LEFT_ASSIGN,
                        R_SYMBOL,
                        R_LEFT_PAREN,
                        R_SYMBOL,
                        R_COMMA,
                        R_SYMBOL,
                        R_EQ_ASSIGN,
                        R_NUM_CONST,
                        R_RIGHT_PAREN,
                        R_SEMICOLON,
                        R_COMMENT
                }, true);
    }


    @Test
    public void testMultiLineBlockCommentTokenization() {
        testTokenization("# This \r\n # a long \r\n # comment (+ 1 2)\r\n 1+1;",
                new IElementType[]{
                        R_COMMENT,
                        R_EOL,
                        R_COMMENT,
                        R_EOL,
                        R_COMMENT,
                        R_EOL,
                        R_NUM_CONST,
                        R_ARITH_PLUS,
                        R_NUM_CONST,
                        R_SEMICOLON
                }, true);
    }

    @Test
    public void testComplexTokenization() {
        String testData = Utils.readFileAsString("misc/complex_script.R");
//        String testData = Utils.readFileAsString("misc/normality tests.R");
        printTokenization(testData, true);
    }


}
