package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class TypeCheckerInspectionTest extends RInspectionTest {


    public void testNoWarnings() {
        doTest("NoWarnings.R");
    }


    public void testMultipleFormalArgMatches() {
        doTest("MultipleFormalArgMatches.R");
    }


    public void testUnusedTripleDotArgument() {
        doTest("UnusedTripleDotArgument.R");
    }


    public void testOptional() {
        doTest("TestOptional.R");
    }

    // tests below require type system to be active, disabled for now until types are brought back


    public void _testWrongTypeParameter() {
        doTest("WrongTypeParameter.R");
    }


    public void _testRule() {
        doTest("Rule.R");
    }


    public void _testGuessReturnFromBody() {
        doTest("GuessReturnFromBody.R");
    }


    public void _testIfElseType() {
        doTest("IfElseType.R");
    }



    public void _testList() {
        doTest("List.R");
    }


    public void _testBinary() {
        doTest("Binary.R");
    }


    public void _testSlice() {
        doTest("Slice.R");
    }


    public void _testVector() {
        doTest("Vector.R");
    }


    public void _testExpectedCharacterFoundNumeric() {
        doTest();
    }


    @NotNull
    @Override
    Class<? extends RInspection> getInspection() {
        return TypeCheckerInspection.class;
    }
}
