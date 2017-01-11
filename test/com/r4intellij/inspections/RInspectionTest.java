package com.r4intellij.inspections;

import com.r4intellij.RTestCase;
import org.jetbrains.annotations.NotNull;

public abstract class RInspectionTest extends RTestCase {

  protected void doTest(@NotNull String filename) {
    myFixture.configureByFile(filename);
    myFixture.enableInspections(getInspection());
    myFixture.testHighlighting(true, false, false, filename);
  }

  @NotNull
  abstract Class<? extends RLocalInspection> getInspection();
}
