package com.r4intellij.inspections;

import com.r4intellij.packages.RPackageService;
import org.jetbrains.annotations.NotNull;

import java.io.File;

public class UnresolvedReferenceInspectionTest extends RInspectionTest {


    @Override
    public void setUp() throws Exception {
        super.setUp();

        // inject stub index here for more reproducible testing
        RPackageService.getTestInstance(new File(TEST_DATA_PATH, "libindex.dat"));
//        RPackageService.getInstance().refreshIndex();
    }


    public void testTidyrImportMissing() {
        doTest(getTestName(true) + ".R");
    }


    public void testNoWarningForOverriddenMethod() {
        doTest(getTestName(true) + ".R");
    }


    public void testUnresovableSymbolInScope() {
        doTest(getTestName(true) + ".R");
    }


    public void testUnresolvableFunction() {
        doTest(getTestName(true) + ".R");
    }


    public void testPackageNameInLibraryCall() {
        doTest(getTestName(true) + ".R");
    }


    public void testPackageData() {
        doTest(getTestName(true) + ".R");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnresolvedReferenceInspection.class;
    }
}
