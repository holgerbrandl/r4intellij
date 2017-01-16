package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.RPsiUtils;
import com.r4intellij.parsing.RElementTypes;
import com.r4intellij.psi.api.*;
import com.r4intellij.psi.references.RReferenceImpl;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.List;

public class RUnresolvedReferenceInspection extends RLocalInspection {
  @Nls
  @NotNull
  @Override
  public String getDisplayName() {
    return "Unresolved reference";
  }

  @NotNull
  @Override
  public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly, @NotNull LocalInspectionToolSession session) {
    return new Visitor(holder);
  }

  private class Visitor extends RVisitor {

    private final ProblemsHolder myProblemHolder;

    public Visitor(ProblemsHolder holder) {
      myProblemHolder = holder;
    }

    @Override
    public void visitReferenceExpression(@NotNull RReferenceExpression element) {
      PsiElement sibling = element.getNextSibling();
        if (sibling != null && sibling.getNode().getElementType() == RElementTypes.R_DOUBLECOLON) {
        return;
      }

      if (RPsiUtils.isNamedArgument(element)) {
        return;
      }

      RCallExpression callExpression = PsiTreeUtil.getParentOfType(element, RCallExpression.class);
      if (callExpression != null) {
        RFunctionExpression function = RPsiUtils.getFunction(callExpression);
        if (function != null) {
          List<RParameter> list = function.getParameterList().getParameterList();
          if (RPsiUtils.containsTripleDot(list)) {
            return;
          }
        }
      }

      RReferenceImpl reference = element.getReference();
      if (reference != null) {
        PsiElement resolve = reference.resolve();
        if (resolve == null) {
          registerProblem(myProblemHolder, element, "Unresolved reference", ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
        }
      }
    }
  }
}
