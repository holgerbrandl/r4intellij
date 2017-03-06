package com.r4intellij.inspections;

import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import org.jetbrains.annotations.NotNull;

public class UnusedVariableInspectionTest extends RInspectionTest {

    // TODO test annotation options to whitelist symobls and functions

    // False negative tests: unused annotation should be present


    public void testUnusedVariable() {
        // no downstream assignment no side-effect --> flag unused
        doExprTest("<warning descr=\"Variable 'a' is never used\">a</warning> = 3");
    }


    public void testUnusedVariableInFunExpr() {
        doExprTest(readTestDataFile());
    }


    public void testUnusedAnonymousFunExpr() {
        doExprTest("function(x)x");
    }


    public void testUnusedFunction() {
        doExprTest("<warning descr=\"Variable 'myfun' is never used\">myfun</warning> = function(x)x");
    }


    public void testColumnDeleteByNullAssign() {
        // this affects both resolvability of foo and the inspection\
        // to detect that `foo` is of interest and not `foo$bar`
        doExprTest("foo = data.frame() ; foo$bar <- NULL; head(foo)");
    }


    public void testFlagUnusedMemberAccess() {
        // this affects both resolvability of foo and the inspection\
        // to detect that `foo` is of interest and not `foo$bar`
        doExprTest("foo = data.frame() ; " + warnUnused("foo$bar") + " <- 3");
    }


    @NotNull
    private static String warnUnused(@NotNull String varName) {
        return "<warning descr=\"Variable '" + varName + "' is never used\">" + varName + "</warning>";
    }


    /**
     * Make sure to not mistake overridden functions symbols.
     */
    //TODO v1.1 not critical and also hard. e.g c = if(T) mean else max; c(1:3)
    public void _testOverrideFunWithSymbol() {
        // c should b be tagged as unused
        // c() should not resolve to c but to base::c
        CodeInsightTestFixture fixture = doExprTest("<warning descr=\"Variable 'c' is never used\">c</warning> = 1; c('foo', 'bar')");

        //todo
//        cAssign = fixture.getFile().
//        funCall = ...
//        funcCall.resolve() != cAssign
    }


    // False positive tests: Unused annotation might be present (or was by regression) but should not


    public void testOutsideBlockUsage() {
        // since (in contrary to java) is legal in R; scoping works different somehow
        doExprTest("{ a = 3; }; a");
    }


    /**
     * array modifications are regular usage, so a should be flagged as used; 1+a has printing as side effect.
     */
    public void testArrayModification() {
        doExprTest("a = 1:5; a[4] = 3; 1 + a");
    }


    /**
     * This is weired r magic. Make sure that don't tag <code>a</code>  nor <code>rownames(a)</code> as unused.
     * See https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Attributes
     * <p>
     * TODO also ensure that rownames resolves to the correct <code>`rownames<-`</code>. TBD: Where would it matter?
     */
    public void testDedicatedAccessorFunction() {
        createSkeletonLibrary("base");

        doExprTest("a = data.frame(col1=1:3, col2=NA); rownames(a) = c('foo', 'bar');  a");

        // todo maybe we could/should ensure that foo is tagged as unused in
        // foo = iris; names(foo) <- 1:3
    }


    public void testUsageOutsideIfElse() {
        // since (in contrary to java) is legal in R; scoping works different somehow
        doExprTest("if(T){ a = 3; }else{ b = 2; }; a ; b");
    }


    public void testDonFlagReturn() {
        assertAllUsed("function(){ if(T){ head(iris); return(1) };  return(2); { return(3) }; return(4) }()");
    }


    /**
     * Last expression of function expression should be flagged because its return value as side effect
     */
    public void testDontFlagLastFunExprStatement() {
        assertAllUsed("myfun = function(){ a = 3 }; myfun()");
    }


    /**
     * The last statement of a block is its return value in R.
     */
    public void testDontFlagLastBlockExprStatement() {
        assertAllUsed("{ foo= 3; foo }");
    }

    // todo finish this --> NoSideEffectsInspection
//    public void testFlagNoSideEffectExprInFunction(){
//        assertAllUsed("myFun = function(){ <warning descr=\"Expression '1+1' has no side effects\">1+1<\warning>; 3 }; myFun()");
//    }
//
//    public void testFlagNoSideEffectExprInBlock(){
//        assertAllUsed("{ <warning descr=\"Expression '1+1' has no side effects\">1+1<\warning>; 3 }");
//    }


    public void testFlagLastBlockIfNotAssignedOrReturn() {
        // a bit more artificial but still valid
        assertAllUsed("myfun = function(){ head(iris); { a = 3} }; myfun()");

        // bar printing is usage side-effect -> all used
        // TODO reenable and fix (but does not seem very common usecase)
//        assertAllUsed("bar = { foo = 3}; bar");
    }


    public void testDontFlagExprInTerminalIfElse() { // this already not really realistic, but for sake of completeness
        assertAllUsed("myfun = function(){ head(iris); if(T){ a = 3} }; myfun()");
    }


    public void testDontFlagFunctionArgUsedAsUnnamedArg() {
        assertAllUsed("function(usedArg) head(usedArg)");
    }


    public void testDontFlagFunctionArgUsedAsNamedArg() {
        // todo this will fail because it's the last statment in the file --> Disable for some unit-tests
        assertAllUsed("function(usedArg) head(x=usedArg)");
    }


    public void testQuoteAgnosticOperatorsDefs() {
        assertAllUsed(
                "`%foo%` <- function(a,b) 3\n" +
                        "'%bar%' <- function(a,b) 3;\n" +
                        "1 %foo% 3;  2 %bar% 3");

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
