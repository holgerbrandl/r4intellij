package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class UnresolvedReferenceInspectionTest extends RInspectionTest {
    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/inspections";
    }


    public void testUnresolvedReferenceInspection() {
        doTest(getTestName(true) + ".R");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnresolvedReferenceInspection.class;
    }
}
