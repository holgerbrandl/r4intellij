package com.r4intellij.inspections;

import org.jetbrains.annotations.NotNull;

public class RUnresolvedReferenceInspectionTest extends RInspectionTest {
  @Override
  protected String getTestDataPath() {
    return super.getTestDataPath() + "/inspections/unresolvedReferenceInspection";
  }

  public void test() {
    doTest("main.R");
  }

  @NotNull
  @Override
  Class<? extends RLocalInspection> getInspection() {
    return RUnresolvedReferenceInspection.class;
  }
}
