package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

public abstract class RLocalInspection extends LocalInspectionTool {

  @Nls
  @NotNull
  @Override
  public String getGroupDisplayName() {
    return "R inspections";
  }

  protected void registerProblem(ProblemsHolder holder, PsiElement element, String message, ProblemHighlightType highlightType) {
    if (holder != null) {
      holder.registerProblem(element, message, highlightType);
    }
  }
}
