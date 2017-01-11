package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class RUnusedInspectionTest extends RInspectionTest {

  @Override
  protected String getTestDataPath() {
    return super.getTestDataPath() + "/inspections/unusedInspection";
  }

  public void test() {
    doTest("main.R");
  }

  @NotNull
  @Override
  Class<? extends RLocalInspection> getInspection() {
    return RUnusedInspection.class;
  }
}
