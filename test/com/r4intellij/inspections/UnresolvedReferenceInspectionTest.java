package com.r4intellij.inspections;

import com.google.common.collect.Iterables;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.intellij.util.ArrayUtil;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RAssignmentStatement;
import org.jetbrains.annotations.NotNull;

import java.util.Collection;

import static com.r4intellij.packages.LocalRUtil.DEFAULT_PACKAGES;
import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertThat;

public class UnresolvedReferenceInspectionTest extends RInspectionTest {


    @Override
    public void setUp() throws Exception {
        super.setUp();

        // inject stub index here for more reproducible testing
        RPackageService.getTestInstance();
//        RPackageService.getInstance().refreshIndex();

        // add base packages for testing
        createSkeletonLibrary(ArrayUtil.toStringArray(DEFAULT_PACKAGES));
    }


    // positive tests: symbols that should be resolvable. These test actually test the resolver itself and not so
    // much the inspection code




    public void testIrisReassignment() {
        doExprTest("iris = iris");
    }


    public void testNoWarningForOverriddenMethod() {
        doTest(getTestName(true) + ".R");
    }


    public void testResolveNamespaceCall() {
//        addPckgsToSkeletonLibrary(myFixture, ArrayUtil.toStringArray(DEFAULT_PACKAGES));
//        createLibraryFromPckgNames("dplyr", "datasets");
        addPckgsToSkeletonLibrary("dplyr");
//        doExprTest("foo = dplyr::group_by(iris)");
        doExprTest("dplyr::group_by(iris)");
    }


    /**
     * since it is associated with base the package should be resolvable. This should come from  stub-index
     */
    public void testIris() {
        // myFixture.addFileToProject("base.R", readFileAsString(getSkeletonPath("utils").toPath()));

        // rather use actual library here to see if stub-index is working correctly
        createSkeletonLibrary("datasets");
        doExprTest("iris");
    }


    public void testNasaWithoutDplyr() {
        doExprTest("<warning descr=\"Unresolved reference\">nasa</warning>");
    }


    public void testNasaWithDplyr() {
        createSkeletonLibrary("dplyr");
        doExprTest("require(dplyr); nasa");
    }


    public void testTransitiveDependencies() {
//        addPckgsToSkeletonLibrary("datasets");
        doExprTest("require(caret); ggplot(iris)");
    }


    public void testForwardImportAfterUsage() {
        doExprTest(forwardRef("glimpse") + "(iris) ; require(dplyr)");
    }


    // see https://github.com/tidyverse/tidyverse/issues/40
    public void testTransitiveTidyverseDependencies() {
        createSkeletonLibrary("dplyr", "datasets");

        doExprTest("require(tidyverse); group_by(iris)");
    }


    public void testOutsideBlockUsage() {
        // outside a should be resolvable
        doExprTest("{ a = 3; }; a");
    }


    public void testUsageOutsideIfElse() {
        // outside a and b should be resolvable because of r scoping rules
        doExprTest("if(T)\n{ a = 3; }else{\n b = 2; }; a ; b");
    }


    public void testUnquotedVarUsageInTidyverse() {
        doExprTest("require(tidyverse); count(iris, Species)");
    }


    // negative tests: symbols that should not be resolvable


    public void testTidyrImportMissing() {
        doTest(getTestName(true) + ".R");
    }


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
    private static String forwardRef(@NotNull String varName) {
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


    public void testOperatorReDef() {

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
