package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class RUnusedParameterInspectionTest extends RInspectionTest {

  @Override
  protected String getTestDataPath() {
    return super.getTestDataPath() + "/inspections/unusedParameterInspection";
  }

  public void test() {
    doTest("main.R");
  }

  @NotNull
  @Override
  Class<? extends RLocalInspection> getInspection() {
    return RUnusedParameterInspection.class;
  }
}
