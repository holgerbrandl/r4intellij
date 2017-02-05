package com.r4intellij.inspections;

import com.intellij.testFramework.fixtures.CodeInsightTestFixture;
import com.r4intellij.RTestCase;
import org.jetbrains.annotations.NotNull;

public abstract class RInspectionTest extends RTestCase {

    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/inspections/" + getClass().getSimpleName().replace("Test", "");
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


    @NotNull
    abstract Class<? extends RInspection> getInspection();
}
