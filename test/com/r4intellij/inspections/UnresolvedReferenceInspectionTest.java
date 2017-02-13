package com.r4intellij.inspections;

import com.google.common.collect.Iterables;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.impl.libraries.ProjectLibraryTable;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.intellij.util.ArrayUtil;
import com.r4intellij.packages.RPackageService;
import com.r4intellij.psi.api.RAssignmentStatement;
import com.r4intellij.settings.LibraryUtil;
import org.jetbrains.annotations.NotNull;

import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static com.r4intellij.inspections.TypeCheckerInspectionTest.getSkeletonPath;
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
        createLibraryFromPckgNames(myFixture, ArrayUtil.toStringArray(DEFAULT_PACKAGES));
    }


    // positive tests: symbols that should be resolvable. These test actually test the resolver itself and not so
    // much the inspection code


    public void testSimpleAssignment() {
        doExprTest("foo = 23; 16 + foo");
    }


    public void testLocalFunctionCall() {
        doExprTest("foo = function(x) x; foo(3)");
    }


    public void testNoWarningForOverriddenMethod() {
        doTest(getTestName(true) + ".R");
    }


    public void testResolveNamespaceCall() {
//        addPckgsToSkeletonLibrary(myFixture, ArrayUtil.toStringArray(DEFAULT_PACKAGES));
//        createLibraryFromPckgNames(myFixture, "dplyr", "datasets");
        addPckgsToSkeletonLibrary(myFixture, "dplyr");
//        doExprTest("foo = dplyr::group_by(iris)");
        doExprTest("dplyr::group_by(iris)");
    }


    /**
     * since it is associated with base the package should be resolvable. This should come from  stub-index
     */
    public void testIris() {
        // myFixture.addFileToProject("base.R", readFileAsString(getSkeletonPath("utils").toPath()));

        // rather use actual library here to see if stub-index is working correctly
        createLibraryFromPckgNames(myFixture, "datasets");
        doExprTest("iris");
    }


    public static void addPckgsToSkeletonLibrary(CodeInsightTestFixture myFixture, String... packageNames) {
//        fail("not yet ready because we can not fetch the existing library");

        LibraryTable libraryTable = ProjectLibraryTable.getInstance(myFixture.getModule().getProject());
        Library libraryByName = libraryTable.getLibraryByName(LibraryUtil.R_SKELETONS);

        if (libraryByName != null) {
            Stream<String> existingLibFiles = Arrays.stream(libraryByName.getFiles(OrderRootType.CLASSES)).
                    map(f -> f.getName().replaceFirst("[.]r", ""));
            packageNames = Stream.concat(existingLibFiles, Arrays.stream(packageNames)).toArray(String[]::new);

        }

        createLibraryFromPckgNames(myFixture, packageNames);
    }


    public static void createLibraryFromPckgNames(CodeInsightTestFixture myFixture, String... packageNames) {
        Module myModule = myFixture.getModule();

        LocalFileSystem fileSystem = LocalFileSystem.getInstance();

        List<VirtualFile> skeletons = Arrays.stream(packageNames).map(pckgName -> {
            Path skeletonPath = getSkeletonPath(pckgName).toPath();
            return fileSystem.findFileByPath(skeletonPath.toAbsolutePath().toString());
        }).collect(Collectors.toList());


        PsiTestUtil.addProjectLibrary(myModule,
                LibraryUtil.R_SKELETONS,
                ArrayUtil.toObjectArray(skeletons, VirtualFile.class));
    }


    public void testNasaWithoutDplyr() {
        doExprTest("<warning descr=\"Unresolved reference\">nasa</warning>");
    }


    public void testNasaWithDplyr() {
        createLibraryFromPckgNames(myFixture, "dplyr");
        doExprTest("require(dplyr); nasa");
    }


    public void testTransitiveDependencies() {
//        addPckgsToSkeletonLibrary("datasets");
        doExprTest("require(caret); ggplot(iris)");
    }


    // see https://github.com/tidyverse/tidyverse/issues/40
    public void testTransitiveTidyverseDependencies() {
        createLibraryFromPckgNames(myFixture, "dplyr", "datasets");

        doExprTest("require(tidyverse); group_by(iris)");
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


    public void testSelfAssignment() {
//        doExprTest("sdf = ls(<warning descr=\"Unresolved reference\">sdf</warning>)");
        doExprTest("sdf = { <warning descr=\"Forward reference\">sdf</warning> }");
//        doExprTest("sdf = <warning descr=\"Unresolved reference\">foo</warning>");
    }


    public void testForwardReference() {
//        doExprTest("foo = <warning descr=\"Unresolved reference\">bar</warning>; bar = 1");
        doExprTest("foo = { <warning descr=\"Forward reference\">bar</warning> } ; bar = 1");
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


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnresolvedReferenceInspection.class;
    }
}
