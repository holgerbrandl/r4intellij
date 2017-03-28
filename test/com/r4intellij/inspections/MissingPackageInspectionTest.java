package com.r4intellij.inspections;

import com.r4intellij.packages.RIndexCache;
import org.jetbrains.annotations.NotNull;

/**
 * @author Holger Brandl
 */
public class MissingPackageInspectionTest extends RInspectionTest {


    @Override
    public void setUp() throws Exception {
        super.setUp();

        // inject stub index here for more reproducible testing
        RIndexCache.getTestInstance();
//        RIndexCache.getInstance().refreshIndex();
    }


    public void testMissingFoobarPackage() {
        doTest(getTestName(true) + ".R");
    }


    public void testQuotedPackageName() {
        doTest(getTestName(true) + ".R");
    }


    public void testFunCallRequireArg() {
        doExprTest("require(getPckgName('foo'))");
    }


    public void testMissingPckgInNamespaceCall() {
        doExprTest("<error descr=\"'foobar' is not yet installed\">foobar</error>::myFun()");
    }


    public void testMissingPckgInNonExportedNamespaceCall() {
        doExprTest("<error descr=\"'foobar' is not yet installed\">foobar</error>:::myFun()");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return MissingPackageInspection.class;
    }
}
