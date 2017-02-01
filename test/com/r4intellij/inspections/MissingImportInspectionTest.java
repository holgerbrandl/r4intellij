package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

/**
 * @author Holger Brandl
 */
public class MissingImportInspectionTest extends RInspectionTest {

    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/inspections/MissingImport";
    }


    public void testTidyrMissing() {
        doTest(getTestName(true) + ".R");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return MissingImportInspection.class;
    }
}
