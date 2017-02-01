package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class RUnresolvedReferenceInspectionTest extends RInspectionTest {
    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/inspections";
    }


    public void testUnresolvedReferenceInspection() {
        doTest("unresolvedReferenceInspection.R");
    }


    @NotNull
    @Override
    Class<? extends RLocalInspection> getInspection() {
        return RUnresolvedReferenceInspection.class;
    }
}
