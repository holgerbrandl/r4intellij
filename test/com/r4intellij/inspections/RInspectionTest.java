package com.r4intellij.inspections;

import com.intellij.codeInsight.daemon.impl.HighlightInfo;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.r4intellij.RTestCase;
import org.intellij.lang.annotations.Language;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.List;

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


    @NotNull
    abstract Class<? extends RInspection> getInspection();
}
