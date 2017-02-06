package com.r4intellij.inspections;

import com.google.common.collect.Iterables;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RAssignmentStatement;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.Collection;

import static org.hamcrest.CoreMatchers.instanceOf;
import static org.junit.Assert.assertThat;

public class UnresolvedReferenceInspectionTest extends RInspectionTest {


    @Override
    public void setUp() throws Exception {
        super.setUp();

        // inject stub index here for more reproducible testing
        RPackageService.getTestInstance(new File(TEST_DATA_PATH, "libindex.dat"));
//        RPackageService.getInstance().refreshIndex();
    }


    // positive tests: symbols that should be resolvable


    public void testSimpleAssignment() {
        doExprTest("foo = 23; 16 + foo");
    }


    public void testLocalFunctionCall() {
        doExprTest("foo = function(x) x; foo(3)");
    }


    public void testNoWarningForOverriddenMethod() {
        doTest(getTestName(true) + ".R");
    }


    // data-set resolve (which should come via stub index


    /**
     * since it is asscoiated with base the package should be resolvable
     */
    public void testIris() {
        doExprTest("iris");
    }


    public void testNasaWithoutDplyr() {
        doExprTest("<warning descr=\"Unresolved reference\">nasa</warning>");
    }


    public void testNasaWithDplyr() {
        doExprTest("require(dplyr); nasa");
    }


    public void testTransitiveDependencies() {
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


    public void testSelfAssignment() {
        doExprTest("sdf = ls(<warning descr=\"Unresolved reference\">sdf</warning>)");
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


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnresolvedReferenceInspection.class;
    }
}
