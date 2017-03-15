package com.r4intellij.inspections;

import com.google.common.collect.Iterables;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.r4intellij.psi.api.RAssignmentStatement;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertThat;

public class UnresolvedReferenceInspectionTest extends RInspectionTest {

    // positive tests: symbols that should be resolvable. These test actually test the resolver itself and not so
    // much the inspection code


    public void testIrisReassignment() {
        doExprTest("iris = iris");
    }


    public void testNoWarningForOverriddenMethod() {
        doTest(getTestName(true) + ".R");
    }


    public void testOutsideBlockUsage() {
        // outside a should be resolvable
        doExprTest("{ a = 3; }; a");
    }


    public void testUsageOutsideIfElse() {
        // outside a and b should be resolvable because of r scoping rules
        doExprTest("if(TRUE)\n{ a = 3; }else{\n b = 2; }; a ; b");
    }


    // negative tests: symbols that should not be resolvable


    public void testUnresovableSymbolInScope() {
        doTest(getTestName(true) + ".R");
    }


    public void testUnresolvableFunction() {
        doTest(getTestName(true) + ".R");
    }


    public void testPackageNameInLibraryCall() {
        doTest(getTestName(true) + ".R");
    }


    public void testForwardSelfAssignment() {
        doExprTest("sdf = { " + forwardRef("sdf") + " }");
    }


    @NotNull
    public static String forwardRef(@NotNull String varName) {
        return "<error descr=\"Forward reference\">" + varName + "</error>";
    }


    public void testForwardReference() {
//        doExprTest("foo = <warning descr=\"Unresolved reference\">bar</warning>; bar = 1");
        doExprTest("foo = { <error descr=\"Forward reference\">bar</error> } ; bar = 1");
    }


    public void testFindFirstForwardReference() {
//        doExprTest("foo = <warning descr=\"Unresolved reference\">bar</warning>; bar = 1");
        CodeInsightTestFixture fixture = doExprTest("foo = { <error descr=\"Forward reference\">bar</error> } ; bar = 1");
        PsiElement psiElement = fixture.getFile().getChildren()[0];
        // todo finish test implementation
    }


    public void testRedefinedReferenceLookup() {
        // no warning is expected here but do we correctly reveal the second assignment as reference for a?
        CodeInsightTestFixture fixture = doExprTest("a = 2; a = 3; b = a");

        Collection<RAssignmentStatement> assignments = PsiTreeUtil.findChildrenOfType(fixture.getFile(), RAssignmentStatement.class);
        assertSize(3, assignments);

        PsiElement aResolved = Iterables.getLast(assignments).getAssignedValue().getReference().resolve();

        assertNotNull(aResolved);
        assertThat(aResolved, instanceOf(RAssignmentStatement.class));
        assertEquals(((RAssignmentStatement) aResolved).getAssignedValue().getText(), "3");
    }


    public void testUnamedCallArgumentInFunctionBody() {
        doExprTest("function() head(<warning descr=\"Unresolved reference\">sdf</warning>)");
    }


    public void testNamedCallArgumentInFunctionBody() {
        doExprTest("function() head(x=<warning descr=\"Unresolved reference\">sdf</warning>)");
    }


    public void testDoubleQuotedOpDef() {
        doExprTest("\"%foo%\" <- function(a,b) 3; 1 %foo% 3");
    }


    public void testBackTickOpDef() {
        doExprTest("`%foo%` <- function(a,b) 3; 1 %foo% 3");
    }


    public void testOperatorReDefinition() {
        // todo
    }


    @NotNull
    private static String unresolved(@NotNull String varName) {
        return "<error descr=\"Unresolved reference\">" + varName + "</error>";
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnresolvedReferenceInspection.class;
    }
}
