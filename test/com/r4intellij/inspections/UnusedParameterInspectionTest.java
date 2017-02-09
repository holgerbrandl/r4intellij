package com.r4intellij.inspections;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import org.intellij.lang.annotations.Language;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class UnusedParameterInspectionTest extends RInspectionTest {

    // TODO test annotation options to whitelist symobls and functions

    public void testUnusedParameterInspection() {
        assertUnused(readTestDataFile());
    }


    private void assertUnused(@Language("R") String expr) {
        CodeInsightTestFixture fixture = doExprTest(expr);
        List<HighlightInfo> highlightInfo = fixture.doHighlighting();

        // make sure that they show up as unused
        assertNotEmpty(highlightInfo);
        assertEquals(highlightInfo.get(0).type.getAttributesKey(), CodeInsightColors.NOT_USED_ELEMENT_ATTRIBUTES);
    }


    private void assertAllUsed(@Language("R") String expr) {
        // todo needed? doExpr will fail if there's a warning!!

        CodeInsightTestFixture fixture = doExprTest(expr);
        List<HighlightInfo> highlightInfo = fixture.doHighlighting();

        assertEmpty(highlightInfo);
    }

    // False negative tests: unused annotation should be present

    // naming convention: False negative test start with Unused.


    public void testUnusedSymbol() {
        doExprTest("a = 3");
    }


    public void testUnusedVariableInFunExpr() {
        doExprTest(readTestDataFile());
    }


    public void testUnusedAnonymousFunExpr() {
        doExprTest("function(x)x");
    }


    public void testUnusedFunction() {
        doExprTest("myfun = function(x)x");
    }


    // False positive tests: Unused annotation might be present (or was by regression) but should not


    // this should if all be optional
//    public void dontFlagLastExprInFile () {
//        doExprTest("a = 3");
//    }


    public void testOutsideBlockUsage() {
        doExprTest("{ a = 3; }; a"); // since (in contrary to java) is legal in R; scoping works different somehow
    }


    public void testUsageOutsideIfEls() {
        doExprTest("{ a = 3; }; a"); // since (in contrary to java) is legal in R; scoping works different somehow
    }


    /**
     * We should not flag symbols as unused if they are used for named arguments
     */
    public void testDontFlagNamedFunctionParameter() {
        // if this happens, it rather means that the reference resolver is broken (again)
        assertAllUsed("myFun = function(arg) head(x=arg)");
    }


    /**
     * Last element of fun should be flagged because its return value as side effect
     */
    public void testDontFlagLastFunExprStatement() {
        assertAllUsed("function(){ a = 3 }()");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnusedParameterInspection.class;
    }
}
