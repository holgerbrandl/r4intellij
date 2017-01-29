package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class RTypeCheckerInspectionTest extends RInspectionTest {

    public void testNoWarnings() {
        doTest("test.R");
    }


    public void testWrongTypeParameter() {
        doTest("test1.R");
    }


    public void testArgumentsMatching() {
        doTest("test2.R");
    }


    public void testTripleDot() {
        doTest("test3.R");
    }


    public void testRule() {
        doTest("test4.R");
    }


    public void testGuessReturnFromBody() {
        doTest("test5.r");
    }


    public void testIfElseType() {
        doTest("test6.r");
    }


    public void testOptional() {
        doTest("test-optional.r");
    }


    public void testList() {
        doTest("list.r");
    }


    public void testBinary() {
        doTest("binary.r");
    }


    public void testSlice() {
        doTest("slice.r");
    }


    public void testVector() {
        doTest("vector.r");
    }


    public void testDefaultValue() {
        doTest("default-value.r");
    }


    @Override
    protected String getTestDataPath() {
        return super.getTestDataPath() + "/typing/";
    }


    @NotNull
    @Override
    Class<? extends RLocalInspection> getInspection() {
        return RTypeCheckerInspection.class;
    }
}
