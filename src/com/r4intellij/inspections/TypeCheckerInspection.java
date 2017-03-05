package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.ArgumentMatcher;
import com.r4intellij.typing.MatchingException;
import com.r4intellij.typing.RTypeContext;
import com.r4intellij.typing.RTypeProvider;
import com.r4intellij.typing.types.RErrorType;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class TypeCheckerInspection extends RInspection {

    @Nls
    @NotNull
    @Override
    public String getDisplayName() {
        return "Validate Call Arguments";
    }


    @NotNull
    @Override
    public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly, @NotNull LocalInspectionToolSession session) {
        return new Visitor(holder);
    }


    public static boolean isPipeContext(RCallExpression callExpression) {
        // see https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html

        boolean hasDotArg = callExpression
                .getArgumentList().getExpressionList()
                .stream()
                .anyMatch(ex -> Objects.equals(ex.getText(), "."));

        if (hasDotArg) return false;

        // TODO convert to settings item
        List<String> pipeOps = Arrays.asList("%>%", "%<>%", "%T>", "%$");

        if (callExpression.getParent() instanceof ROperatorExpression) {
            String opText = ((ROperatorExpression) callExpression.getParent()).getOperator().getText();
            return pipeOps.contains(opText);
        }

        return false;
    }


    private class Visitor extends RVisitor {

        private final ProblemsHolder myProblemHolder;


        public Visitor(@NotNull ProblemsHolder holder) {
            myProblemHolder = holder;
        }


        @Override
        public void visitCallExpression(@NotNull RCallExpression callExpression) {
            List<RExpression> arguments = callExpression.getArgumentList().getExpressionList();

            try {
                ArgumentMatcher argumentMatcher = new ArgumentMatcher(callExpression);

                if (isPipeContext(callExpression)) {
                    argumentMatcher.setFirstArgInjected(true);
                }

                argumentMatcher.checkArguments(callExpression.getArgumentList());
            } catch (MatchingException e) {
                myProblemHolder.registerProblem(callExpression, e.getMessage(), ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
            }

            visitExpression(callExpression);

            // unclear need (but seems to have side-effects side effects)
            RTypeProvider.getType(callExpression);
        }



        @Override
        public void visitOperatorExpression(@NotNull ROperatorExpression operatorExpression) {
            //NOTE Visiting ops would just make sense once the type system is reenabled.
            // At the moment it's just eating CPU time

            ROperator operator = PsiTreeUtil.getChildOfType(operatorExpression, ROperator.class);
            if (operator == null) {
                return;
            }

            // ignore binary ops (for now). E.g. `-` is not resolved to an unary function and it's not clear why
            if (!operatorExpression.isBinary()) return;

            PsiReference referenceToFunction = operator.getReference();

            try {
                new ArgumentMatcher(referenceToFunction).checkArguments(operatorExpression);
            } catch (MatchingException e) {
                myProblemHolder.registerProblem(operatorExpression, e.getMessage(), ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
            }

            RTypeProvider.getType(operatorExpression);
        }


        // todo why do we need to visit those elements? test-coverage?

        @Override
        public void visitAtExpression(@NotNull RAtExpression o) {
            RTypeProvider.getType(o);
        }


        @Override
        public void visitMemberExpression(@NotNull RMemberExpression o) {
            RTypeProvider.getType(o);
        }


        @Override
        public void visitSubscriptionExpression(@NotNull RSubscriptionExpression o) {
            RTypeProvider.getType(o);
        }
    }


    @Override
    public void inspectionFinished(@NotNull LocalInspectionToolSession session, @NotNull ProblemsHolder problemsHolder) {
        Map<RPsiElement, RErrorType> errors = RTypeContext.getExpressionsWithError(problemsHolder.getProject());
        for (Map.Entry<RPsiElement, RErrorType> error : errors.entrySet()) {
            problemsHolder.registerProblem(error.getKey(), error.getValue().getErrorMessage(), ProblemHighlightType.GENERIC_ERROR);
        }
    }
}
