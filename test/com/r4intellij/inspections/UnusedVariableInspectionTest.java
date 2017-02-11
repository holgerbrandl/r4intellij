package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class UnusedVariableInspectionTest extends RInspectionTest {

    // TODO test annotation options to whitelist symobls and functions

    // False negative tests: unused annotation should be present


    public void testUnusedVariable() {
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


    public void testOutsideBlockUsage() {
        // since (in contrary to java) is legal in R; scoping works different somehow
        doExprTest("{ a = 3; }; a");
    }


    public void testUsageOutsideIfEls() {
        // since (in contrary to java) is legal in R; scoping works different somehow
        doExprTest("{ a = 3; }; a");
    }


    /**
     * Last expression of function expression should be flagged because its return value as side effect
     */
    public void testDontFlagLastFunExprStatement() {
        assertAllUsed("function(){ a = 3 }()");
    }


    public void testDontFlagFunctionArgUsedAsUnnamedArg() {
        assertAllUsed("function(usedArg) head(usedArg)");
    }


    public void testDontFlagFunctionArgUsedAsNamedArg() {
        // todo this will fail because it's the last statment in the file --> Disable for some unit-tests
        assertAllUsed("function(usedArg) head(x=usedArg)");
    }





    // this should if all be optional
//    public void dontFlagLastExprInFile () {
//        doExprTest("a = 3");
//    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnusedVariableInspection.class;
    }
}
