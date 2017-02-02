package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class UnusedParameterInspectionTest extends RInspectionTest {


    public void testUnusedParameterInspection() {
        doTest(getTestName(true) + ".R");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnusedParameterInspection.class;
    }
}
