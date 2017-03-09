package com.r4intellij.parser;

import com.intellij.testFramework.ParsingTestCase;
import com.intellij.testFramework.TestDataPath;
import com.r4intellij.parsing.RParserDefinition;

@TestDataPath("/testData/psi/")
public class RParsingTest extends ParsingTestCase {
    private static final String DATA_PATH = System.getProperty("user.dir") + "/testData/psi/";


    public RParsingTest() {
        super("", "r", new RParserDefinition());
    }


    @Override
    protected String getTestDataPath() {
        return DATA_PATH;
    }


    public void testSlice() {
        doTest();
    }


    public void testAssignment() {
        doTest();
    }


    public void testBinary() {
        doTest();
    }


    public void testBinarySlice() {
        doTest();
    }


    public void testBooleanExpressions() {
        doTest();
    }


    public void testFunctionCall() {
        doTest();
    }


    public void testFunctionDefinition() {

        doTest();
    }


    /**
     * note: it may look wrong to parse the tailing () into a call on the block, but R does so as well.
     * Actually even if this defines syntactically correct R code, R can not evaluate it, and errors with an
     * <code>Error in foo() : attempt to apply non-function</code>
     * To call an anonymously defined function in place additional brackets are requires:
     * <code>(function(){1})()</code>
     */
    public void testInPlaceFunctionDefCall() {
        doTest();
    }


    public void testIfStatement() {
        doTest();
    }


    public void testForStatement() {
        doTest();
    }


    public void testRepeatStatement() {
        doTest();
    }


    public void testRepeatBlockStatement() {
        doTest();
    }


    public void testWhileStatement() {
        doTest();
    }


    public void testHelpStatement() {
        doTest();
    }


    public void testSubscription() {
        doTest();
    }


    public void testStatementBreak() {
        doTest();
    }


    public void testStatementBreakAssignment() {
        doTest();
    }


    public void testHelpOnKeyword() {
        doTest();
    }


    public void testBreak() {
        doTest();
    }


    public void testOperator() {
        doTest();
    }


    public void testParenthesized() {
        doTest();
    }


    public void testSemicolon() {
        doTest();
    }


    public void testPrecedence() {
        doTest();
    }


    public void testStringKeywordArg() {
        doTest();
    }


    public void testKeywordArg() {
        doTest();
    }


    public void testBlockAsArgument() {
        doTest();
    }


    public void testFormulae() {
        doTest();
    }


    public void testFunctionBodyAsExpression() {
        doTest();
    }


    public void testFunctionAsCallArgument() {
        doTest();
    }


    public void testIfShortForm() {
        doTest();
    }


    public void testAssignmentInSubscription() {
        doTest();
    }


    public void testEmptyKeywordArgument() {
        doTest();
    }


    public void testIfInKeywordArgument() {
        doTest();
    }


    public void testIfStatementAsArgument() {
        doTest();
    }


    public void testReferenceWithAt() {
        doTest();
    }


    public void testDotAsReference() {
        doTest();
    }


    public void testReprAsKeywordArgument() {
        doTest();
    }


    public void testEmptyExpressionInSubscription() {
        doTest();
    }


    public void testReprInFunctionDef() {
        doTest();
    }


    public void testStatementAsDefaultValue() {
        doTest();
    }


    public void testDoubleDoubleBracket() {
        doTest();
    }


    public void testDotAsFunctionParameter() {
        doTest();
    }


    public void testExpressionAsBinaryExpressionPart() {
        doTest();
    }


    public void testDotsInSublist() {
        doTest();
    }


    public void testNewLineAfterKeywordArgument() {
        doTest();
    }


    public void testDotAsKeywordArgument() {
        doTest();
    }


    public void testReprInSublist() {
        doTest();
    }


    public void testBinaryExpressionNewLine() {
        doTest();
    }


    public void testMemberAccess() {
        doTest();
    }


    public void doTest() {
        doTest(true);
    }
}
