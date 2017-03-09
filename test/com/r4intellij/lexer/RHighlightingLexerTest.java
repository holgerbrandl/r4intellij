package com.r4intellij.lexer;

import com.intellij.lexer.Lexer;
import junit.framework.TestCase;

public class RHighlightingLexerTest extends TestCase {

    public void testLogicTrue() {
        doTest("TRUE", "TRUE");
    }


    public void testLogicFalse() {
        doTest("FALSE", "FALSE");
    }


    public void testNumeric1() {
        doTest("1", "NUMERIC");
    }


    public void testZeroNumeric() {
        doTest("01234", "NUMERIC");
    }


    public void testNumeric10() {
        doTest("10", "NUMERIC");
    }


    public void testNumericFloat() {
        doTest("0.1", "NUMERIC");
    }


    public void testNumericFloat2() {
        doTest(".2", "NUMERIC");
    }


    public void testNumericExponent() {
        doTest("1e-7", "NUMERIC");
    }


    public void testNumericFloatExponent() {
        doTest("1.2e+7", "NUMERIC");
    }


    public void testNumericHexExponent() {
        doTest("0x1.1p-2", "NUMERIC");
    }


    public void testNumericBinaryExponent() {
        doTest("0x123p456", "NUMERIC");
    }


    public void testNumericHex() {
        doTest("0x1", "NUMERIC");
    }


    public void testInteger1() {
        doTest("1L", "INTEGER");
    }


    public void testIntegerHex() {
        doTest("0x10L", "INTEGER");
    }


    public void testIntegerLong() {
        doTest("1000000L", "INTEGER");
    }


    public void testIntegerExponent() {
        doTest("1e6L", "INTEGER");
    }


    public void testNumericWithWarn() {         // TODO: inspection. Actually, it's numeric one
        doTest("1.1L", "INTEGER");
    }


    public void testNumericWithWarnExp() {      // TODO: inspection. Actually, it's numeric one
        doTest("1e-3L", "INTEGER");
    }


    public void testSyntaxError() {
        doTest("12iL", "COMPLEX", "identifier");
    }


    public void testUnnecessaryDecimalPoint() {  // TODO: inspection. Unnecessary Decimal Point warning runtime
        doTest("1.L", "INTEGER");
    }


    public void testComplex() {
        doTest("1i", "COMPLEX");
    }


    public void testFloatComplex() {
        doTest("4.1i", "COMPLEX");
    }


    public void testExponentComplex() {
        doTest("1e-2i", "COMPLEX");
    }


    public void testHexLong() {
        doTest("0xFL", "INTEGER");
    }


    public void testSingleQuotedString() {
        doTest("'qwerty'", "STRING");
    }


    public void testDoubleQuotedString() {
        doTest("\"qwerty\"", "STRING");
    }


    public void testEscapeStringDouble() {
        doTest("\"\\\"\"", "STRING");
    }


    public void testEscapeStringSingle() {
        doTest("'\\\''", "STRING");
    }


    public void testEscapeString() {
        doTest("'\\r\\n\\t\\b\\a\\f\\v'", "STRING");
    }


    public void testEscapeOctString() {
        doTest("'\\123'", "STRING");
    }


    public void testEscapeHexString() {
        doTest("'\\x1'", "STRING");
    }


    public void testEscapeUnicodeString() {
        doTest("'\\u1234'", "STRING");
    }


    public void testEscapeBigUnicodeString() {
        doTest("'\\u12345678'", "STRING");
    }


    public void testErrorInString() {             //TODO: inspection. string errors
        doTest("'\\0'", "STRING");
    }


    public void testIdentifier() {
        doTest("a1", "identifier");
    }


    public void testIdentifierDot() {
        doTest("a.1", "identifier");
    }


    public void testIdentifierUnderscore() {
        doTest("a_1", "identifier");
    }


    public void testIdentifierDotDot() {
        doTest("..", "identifier");
    }


    public void testIdentifierDotUnderscore() {
        doTest("._", "identifier");
    }


    public void testIdentifierDotLetter() {
        doTest(".x", "identifier");
    }


    public void testIdentifierDotDigit() {
        doTest(".1", "NUMERIC");
    }


    public void testAssignment() {
        doTest("a <- 42\n", "identifier", "SPACE", "<-", "SPACE", "NUMERIC", "nl");
    }


    public void testAssignmentComment() {
        doTest("A <- a * 2  # R is case sensitive\n", "identifier", "SPACE", "<-", "SPACE", "identifier", "SPACE", "*", "SPACE", "NUMERIC",
                "SPACE", "END_OF_LINE_COMMENT", "nl");
    }


    public void testPrintFunction() {
        doTest("print(a)\n", "identifier", "(", "identifier", ")", "nl");
    }


    public void testCat() {
        doTest("cat(A, \"\\n\") # \"84\" is concatenated with \"\\n\"\n", "identifier", "(", "identifier", ",", "SPACE", "STRING", ")", "SPACE",
                "END_OF_LINE_COMMENT", "nl");
    }


    public void testDoubleBrackets() {
        doTest("profile[[pnames[pm]]]", "identifier", "[[", "identifier", "[", "identifier", "]", "]]");
    }


    public void testDoubleBracketsSeparated() {
        doTest("return(invisible(dll_list[[ seq_along(dll_list)[ind] ]]))", "identifier", "(", "identifier", "(", "identifier", "[[", "SPACE",
                "identifier", "(", "identifier", ")", "[", "identifier", "]", "SPACE", "]]", ")", ")");
    }


    public void testIf() {
        doTest("if(A>a) # true, 84 > 42\n" +
                        "{\n" +
                        "  cat(A, \">\", a, \"\\n\")\n" +
                        "} ", "if", "(", "identifier", ">", "identifier", ")", "SPACE", "END_OF_LINE_COMMENT", "nl", "{", "nl", "SPACE", "identifier",
                "(", "identifier", ",", "SPACE", "STRING", ",", "SPACE", "identifier", ",", "SPACE", "STRING", ")", "nl", "}", "SPACE");
    }


    private static void doTest(String text, String... expectedTokens) {
        doLexerTest(text, new RLexer(), expectedTokens);
    }


    public static void doLexerTest(String text, Lexer lexer, String... expectedTokens) {
        doLexerTest(text, lexer, false, expectedTokens);
    }


    public static void doLexerTest(String text,
                                   Lexer lexer,
                                   boolean checkTokenText,
                                   String... expectedTokens) {

        lexer.start(text);
        int idx = 0;
        int tokenPos = 0;
        while (lexer.getTokenType() != null) {
            if (idx >= expectedTokens.length) {
                StringBuilder remainingTokens = new StringBuilder("\"" + lexer.getTokenType().toString() + "\"");
                lexer.advance();
                while (lexer.getTokenType() != null) {
                    remainingTokens.append(",");
                    remainingTokens.append(" \"").append(checkTokenText ? lexer.getTokenText() : lexer.getTokenType().toString()).append("\"");
                    lexer.advance();
                }
                fail("Too many tokens. Following tokens: " + remainingTokens.toString());
            }
            assertEquals("Token offset mismatch at position " + idx, tokenPos, lexer.getTokenStart());
            String tokenName = checkTokenText ? lexer.getTokenText() : lexer.getTokenType().toString();
            assertEquals("Token mismatch at position " + idx, expectedTokens[idx], tokenName);
            idx++;
            tokenPos = lexer.getTokenEnd();
            lexer.advance();
        }

        if (idx < expectedTokens.length) fail("Not enough tokens");
    }
}
