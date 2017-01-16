package com.r4intellij.run.debug.stack;

import com.intellij.icons.AllIcons;
import com.intellij.xdebugger.frame.XFullValueEvaluator;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import com.r4intellij.debugger.RDebuggerStringUtils;
import com.r4intellij.debugger.frame.RVar;
import org.jetbrains.annotations.NotNull;

final class RXPresentationUtils {

    public static void computePresentation(@NotNull final RVar var, @NotNull final XValueNode node) {
        if (isOneLine(var.getValue())) {
            setVarPresentation(node, var.getType(), var.getValue());
        } else {
            computeMultilineVarPresentation(var, node);
        }
    }


    public static void computePresentation(@NotNull final String value, @NotNull final XValueNode node) {
        if (isOneLine(value)) {
            setPresentation(node, value);
        } else {
            computeMultilinePresentation(value, node);
        }
    }


    private static boolean isOneLine(@NotNull final String value) {
        return RDebuggerStringUtils.findNextLineBegin(value, 0) == value.length();
    }


    private static void setVarPresentation(@NotNull final XValueNode node, @NotNull final String type, @NotNull final String presentation) {
        node.setPresentation(
                AllIcons.Debugger.Value,
                type,
                presentation,
                false
        );
    }


    private static void computeMultilineVarPresentation(@NotNull final RVar var, @NotNull final XValueNode node) {
        final String value = var.getValue();

        setVarPresentation(node, var.getType(), calculatePreview(value));
        setFullValueEvaluator(node, value);
    }


    private static void setPresentation(@NotNull final XValueNode node, @NotNull final String value) {
        final XValuePresentation presentation = new XValuePresentation() {
            @Override
            public void renderValue(@NotNull final XValueTextRenderer renderer) {
                renderer.renderValue(value);
            }
        };

        node.setPresentation(
                AllIcons.Debugger.Value,
                presentation,
                false
        );
    }


    private static void computeMultilinePresentation(@NotNull final String value, @NotNull final XValueNode node) {
        setPresentation(node, calculatePreview(value));
        setFullValueEvaluator(node, value);
    }


    @NotNull
    private static String calculatePreview(@NotNull final String value) {
        return value.substring(
                0,
                RDebuggerStringUtils.findCurrentLineEnd(value, 0)
        ).trim().replaceAll("\\s{2,}", " ");
    }


    private static void setFullValueEvaluator(@NotNull final XValueNode node, @NotNull final String value) {
        final XFullValueEvaluator evaluator = new XFullValueEvaluator() {
            @Override
            public void startEvaluation(@NotNull final XFullValueEvaluationCallback callback) {
                callback.evaluated(value);
            }
        };

        node.setFullValueEvaluator(evaluator);
    }
}
