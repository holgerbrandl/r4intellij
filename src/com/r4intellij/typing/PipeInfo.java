package com.r4intellij.typing;

import com.intellij.psi.PsiElement;
import com.r4intellij.psi.api.RCallExpression;
import com.r4intellij.psi.api.ROperator;
import com.r4intellij.psi.api.ROperatorExpression;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

/**
 * @author Holger Brandl
 */
public class PipeInfo {

    public static final PipeInfo NONE = new PipeInfo(false, false);

    public final boolean firstArgInjected;
    public final boolean isPipeTarget;


    public PipeInfo(boolean firstArgInjected, boolean isPipeTarget) {
        this.firstArgInjected = firstArgInjected;
        this.isPipeTarget = isPipeTarget;
    }


    public static PipeInfo fromCallExpression(RCallExpression callExpression) {
        boolean isPipeTarget = isPipeTarget(callExpression);
        boolean firstArgInjected = isPipeTarget && hasDotArgs(callExpression);

        return new PipeInfo(isPipeTarget, firstArgInjected);

    }


    private static boolean hasDotArgs(@NotNull RCallExpression callExpression) {
        return callExpression
                .getArgumentList().getExpressionList()
                .stream()
                .anyMatch(ex -> Objects.equals(ex.getText(), "."));
    }


    private static boolean isPipeTarget(@NotNull RCallExpression callExpression) {
        // TODO convert to settings item
        List<String> pipeOps = Arrays.asList("%>%", "%<>%", "%T>");

        PsiElement callExParent = callExpression.getParent();
        if (callExParent != null && callExParent instanceof ROperatorExpression) {
            ROperator operator = ((ROperatorExpression) callExParent).getOperator();
            if (operator == null) return false;

            String opText = operator.getText();
            return pipeOps.contains(opText);
        }

        return false;
    }
}
