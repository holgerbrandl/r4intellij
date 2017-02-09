package com.r4intellij.inspections;

import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.r4intellij.RTestCase;
import org.intellij.lang.annotations.Language;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

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


    protected CodeInsightTestFixture doExprTest(@NotNull @Language("R") String expressionList) {
        myFixture.configureByText("a.R", expressionList);
        myFixture.enableInspections(getInspection());
        myFixture.testHighlighting(true, false, false);

        return myFixture;
    }


    protected String readTestDataFile() {
        Path testDataPath = Paths.get(getTestDataPath(), getTestName(true) + ".R");
        try {
            return new String(Files.readAllBytes(testDataPath));
        } catch (IOException e) {
            throw new IllegalArgumentException("could not read test resource file", e);
        }
    }


    @NotNull
    abstract Class<? extends RInspection> getInspection();
}
