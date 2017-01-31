package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class RTypeCheckerInspectionTest extends RInspectionTest {

    public void testNoWarnings() {
        doTest("NoWarnings.R");
    }


    public void testWrongTypeParameter() {
        doTest("WrongTypeParameter.R");
    }


    public void testArgumentsMatching() {
        doTest("ArgumentsMatching.R");
    }


    public void testTripleDot() {
        doTest("TripleDot.R");
    }


    public void testRule() {
        doTest("Rule.R");
    }


    public void testGuessReturnFromBody() {
        doTest("GuessReturnFromBody.R");
    }


    public void testIfElseType() {
        doTest("IfElseType.R");
    }


    public void testOptional() {
        doTest("TestOptional.R");
    }


    public void testList() {
        doTest("List.R");
    }


    public void testBinary() {
        doTest("Binary.R");
    }


    public void testSlice() {
        doTest("Slice.R");
    }


    public void testVector() {
        doTest("Vector.R");
    }


    public void testDefaultValue() {
        doTest("DefaultValue.R");
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
