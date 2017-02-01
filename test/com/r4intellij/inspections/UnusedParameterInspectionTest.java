package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class UnusedParameterInspectionTest extends RInspectionTest {

    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/inspections";
    }


    public void testUnusedParameterInspection() {
        doTest("unusedParameterInspection.R");
    }


    @NotNull
    @Override
    Class<? extends RLocalInspection> getInspection() {
        return UnusedParameterInspection.class;
    }
}
