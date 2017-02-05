package com.r4intellij.inspections;

import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Query;
import com.r4intellij.psi.api.RParameter;
import com.r4intellij.psi.api.RVisitor;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

public class UnusedParameterInspection extends RInspection {


    @Override
    public boolean isEnabledByDefault() {
        return true;
    }



    @NotNull
    @Override
    public HighlightDisplayLevel getDefaultLevel() {
        return HighlightDisplayLevel.WEAK_WARNING;
    }


    @Nls
    @NotNull
    @Override
    public String getDisplayName() {
        return "Unused Function Parameter";
    }


    @NotNull
    @Override
    public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly, @NotNull LocalInspectionToolSession session) {
        return new Visitor(holder);
    }



    //TODO: check not only parameters
    private class Visitor extends RVisitor {

        private final ProblemsHolder myProblemHolder;


        public Visitor(@NotNull final ProblemsHolder holder) {
            myProblemHolder = holder;
        }


        @Override
        public void visitParameter(@NotNull RParameter o) {
            Query<PsiReference> search = ReferencesSearch.search(o);
            PsiReference first = search.findFirst();
            if (first == null) {
                myProblemHolder.registerProblem(o, "Unused parameter " + o.getText(), ProblemHighlightType.LIKE_UNUSED_SYMBOL);
            }
        }
    }
}
