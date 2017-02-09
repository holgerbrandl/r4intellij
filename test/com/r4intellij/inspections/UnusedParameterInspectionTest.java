package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class UnusedParameterInspectionTest extends RInspectionTest {


    // False negative tests: unused annotation should be present


    public void testUnusedParameterInspection() {
        assertUnused(readTestDataFile());
    }


    // False positive tests: Unused annotation might be present (or was by regression) but should not

    /**
     * We should not flag symbols as unused if they are used for named arguments
     */
    public void testDontFlagNamedFunctionParameter() {
        // if this happens, it rather means that the reference resolver is broken (again)
        assertAllUsed("myFun = function(arg) head(x=arg)");
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return UnusedParameterInspection.class;
    }
}
