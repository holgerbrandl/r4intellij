package com.r4intellij.inspections;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.impl.libraries.ProjectLibraryTable;
import com.intellij.openapi.roots.libraries.Library;
import com.intellij.openapi.roots.libraries.LibraryTable;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.testFramework.PsiTestUtil;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.intellij.util.ArrayUtil;
import com.r4intellij.RTestCase;
import com.r4intellij.settings.LibraryUtil;
import org.intellij.lang.annotations.Language;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public abstract class RInspectionTest extends RTestCase {

    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/inspections/" + getClass().getSimpleName().replace("Test", "");
    }


    protected CodeInsightTestFixture doTest() {
        return doTest(getTestName(true) + ".R");
    }


    protected CodeInsightTestFixture doTest(@NotNull String filename) {
        myFixture.configureByFile(filename);
        myFixture.enableInspections(getInspection());
        myFixture.testHighlighting(true, false, false, filename);

        return myFixture;
    }


    protected CodeInsightTestFixture doExprTest(@NotNull String expressionList) {
        myFixture.configureByText("a.R", expressionList);
        myFixture.enableInspections(getInspection());
        myFixture.testHighlighting(true, false, false);

        return myFixture;
    }


    protected String readTestDataFile() {
        Path testDataPath = Paths.get(getTestDataPath(), getTestName(true) + ".R");
        return readFileAsString(testDataPath);

    }


    @NotNull
    protected String readFileAsString(Path testDataPath) {
        try {
            return new String(Files.readAllBytes(testDataPath));
        } catch (IOException e) {
            throw new IllegalArgumentException("could not read test resource file", e);
        }
    }


    protected void assertUnused(@Language("R") String expr) {
        CodeInsightTestFixture fixture = doExprTest(expr);
        List<HighlightInfo> highlightInfo = fixture.doHighlighting();

        // make sure that they show up as unused
        assertNotEmpty(highlightInfo);
        assertEquals(highlightInfo.get(0).type.getAttributesKey(), CodeInsightColors.NOT_USED_ELEMENT_ATTRIBUTES);
    }


    protected void assertAllUsed(String expr) {
        // todo needed? doExpr will fail if there's a warning!!

        CodeInsightTestFixture fixture = doExprTest(expr);
        List<HighlightInfo> highlightInfo = fixture.doHighlighting();

        assertEmpty(highlightInfo);
    }


    protected static void addPckgsToSkeletonLibrary(CodeInsightTestFixture myFixture, String... packageNames) {
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


    protected static void createLibraryFromPckgNames(CodeInsightTestFixture myFixture, String... packageNames) {
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


    @NotNull
    // todo make generic or add test-skeletons to test-data for better portability
    private static File getSkeletonPath(final String pckgName) {
        return new File("/Users/brandl/Library/Caches/IntelliJIdea2016.3/r_skeletons/1842261700/" + pckgName + ".r");
    }


    @NotNull
    abstract Class<? extends RInspection> getInspection();
}
