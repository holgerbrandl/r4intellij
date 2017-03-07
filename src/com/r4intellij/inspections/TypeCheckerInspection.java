package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.*;
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


    public static boolean isPipeContext(@NotNull RCallExpression callExpression) {
        // see https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html

        boolean hasDotArg = callExpression
                .getArgumentList().getExpressionList()
                .stream()
                .anyMatch(ex -> Objects.equals(ex.getText(), "."));

        if (hasDotArg) return false;

        // TODO convert to settings item
        List<String> pipeOps = Arrays.asList("%>%", "%<>%", "%T>", "%$");

        PsiElement callExParent = callExpression.getParent();
        if (callExParent != null && callExParent instanceof ROperatorExpression) {
            ROperator operator = ((ROperatorExpression) callExParent).getOperator();
            if (operator == null) return false;

            String opText = operator.getText();
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
            try {
                ArgumentMatcher argumentMatcher = new ArgumentMatcher(callExpression);

                argumentMatcher.matchArgs(callExpression.getArgumentList());
            } catch (MatchingException e) {
                myProblemHolder.registerProblem(callExpression, e.getMessage(), ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
            } catch (UnknownTypeException e) {
                //stop visiting if we can resolve the function (i.e. its type is Unknown)
                return;
            }

            visitExpression(callExpression);

            // unclear need (but seems to have side-effects side effects)
//            RTypeProvider.getType(callExpression);
        }



        @Override
        public void visitOperatorExpression(@NotNull ROperatorExpression operatorExpression) {
            //NOTE Visiting ops would just make real sense only if the type system would be re-enabled.
            // At the moment it's just eating CPU time, but we keep it for easy "renablility"

            ROperator operator = PsiTreeUtil.getChildOfType(operatorExpression, ROperator.class);
            if (operator == null) {
                return;
            }

            // ignore binary ops (for now). E.g. `-` is not resolved to an unary function and it's not clear why
            if (!operatorExpression.isBinary()) return;

            PsiReference referenceToFunction = operator.getReference();

            try {
                new ArgumentMatcher(referenceToFunction).matchArgs(operatorExpression);
            } catch (MatchingException e) {
                myProblemHolder.registerProblem(operatorExpression, e.getMessage(), ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
            } catch (UnknownTypeException e) {
                return;
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
