package com.r4intellij.inspections;

import com.intellij.codeInspection.LocalInspectionToolSession;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import com.r4intellij.psi.api.*;
import com.r4intellij.typing.MatchingException;
import com.r4intellij.typing.RTypeChecker;
import com.r4intellij.typing.RTypeContext;
import com.r4intellij.typing.RTypeProvider;
import com.r4intellij.typing.types.RErrorType;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Map;

public class TypeCheckerInspection extends RInspection {

    @Nls
    @NotNull
    @Override
    public String getDisplayName() {
        return "R Type Checker";
    }


    @Override
    public boolean isEnabledByDefault() {
        return false;
    }


    @NotNull
    @Override
    public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly, @NotNull LocalInspectionToolSession session) {
        return new Visitor(holder);
    }


    private class Visitor extends RVisitor {
        private final ProblemsHolder myProblemHolder;


        public Visitor(@NotNull ProblemsHolder holder) {
            myProblemHolder = holder;
        }


        @Override
        public void visitCallExpression(@NotNull RCallExpression callExpression) {
            PsiReference referenceToFunction = callExpression.getExpression().getReference();
            List<RExpression> arguments = callExpression.getArgumentList().getExpressionList();
            checkFunctionCall(callExpression, referenceToFunction, arguments);
            visitExpression(callExpression);

            // all disabled because unclear need (sideeffects) and creates dead-lock
            RTypeProvider.getType(callExpression);
        }


        @Override
        public void visitOperatorExpression(@NotNull ROperatorExpression operatorExpression) {
            ROperator operator = PsiTreeUtil.getChildOfType(operatorExpression, ROperator.class);
            if (operator == null) {
                return;
            }
            PsiReference referenceToFunction = operator.getReference();
            List<RExpression> arguments = PsiTreeUtil.getChildrenOfTypeAsList(operatorExpression, RExpression.class);
            checkFunctionCall(operatorExpression, referenceToFunction, arguments);
            RTypeProvider.getType(operatorExpression);
        }


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


        private void checkFunctionCall(PsiElement callSite, PsiReference referenceToFunction, List<RExpression> arguments) {
            if (referenceToFunction != null) {
                PsiElement assignmentStatement = referenceToFunction.resolve();
                if (assignmentStatement != null && assignmentStatement instanceof RAssignmentStatement) {
                    RAssignmentStatement assignment = (RAssignmentStatement) assignmentStatement;
                    RPsiElement assignedValue = assignment.getAssignedValue();
                    if (assignedValue != null && assignedValue instanceof RFunctionExpression) {
                        RFunctionExpression function = (RFunctionExpression) assignedValue;
                        try {
                            RTypeChecker.checkTypes(arguments, function);
                        } catch (MatchingException e) {
                            myProblemHolder.registerProblem(callSite, e.getMessage(), ProblemHighlightType.GENERIC_ERROR_OR_WARNING);
                        }
                    }
                }
            }
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
