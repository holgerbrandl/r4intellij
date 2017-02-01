package com.r4intellij.inspections;

import com.r4intellij.packages.RPackageService;
import org.jetbrains.annotations.NotNull;

import java.io.File;

/**
 * @author Holger Brandl
 */
public class MissingImportInspectionTest extends RInspectionTest {

    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/inspections/MissingImport";
    }


    public void testTidyrImportMissing() {
        // inject stub index here for more reproducible testing
        RPackageService.getTestInstance(new File(super.getTestDataPath(), "libindex.dat"));
//        RPackageService.getInstance().refreshIndex();

        doTest(getTestName(true) + ".R");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return MissingImportInspection.class;
    }
}
