package com.r4intellij.inspections;

import com.r4intellij.packages.RPackageService;
import org.jetbrains.annotations.NotNull;

import java.io.File;

/**
 * @author Holger Brandl
 */
public class MissingPackageInspectionTest extends RInspectionTest {


    @Override
    public void setUp() throws Exception {
        super.setUp();

        // inject stub index here for more reproducible testing
        RPackageService.getTestInstance(new File(TEST_DATA_PATH, "libindex.dat"));
//        RPackageService.getInstance().refreshIndex();
    }


    public void testMissingFoobarPackage() {
        doTest(getTestName(true) + ".R");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return MissingPackageInspection.class;
    }
}
