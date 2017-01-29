package com.r4intellij.run.debug.mock;

import com.intellij.icons.AllIcons;
import com.intellij.xdebugger.frame.XFullValueEvaluator;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class MockXValueNode extends IllegalXValueNode {

    @NotNull
    private final String myShortValue;

    @Nullable
    private final String myFullValue;

    private int myPres;
    private int myEval;


    public MockXValueNode(@NotNull final String shortValue, @Nullable final String fullValue) {
        myShortValue = shortValue;
        myFullValue = fullValue;
    }


    @Override
    public void setPresentation(@Nullable final Icon icon, @NotNull final XValuePresentation presentation, final boolean hasChildren) {
        myPres++;

        assertEquals(AllIcons.Debugger.Value, icon);
        assertFalse(hasChildren);

        final MockXValueTextRenderer renderer = new MockXValueTextRenderer(myShortValue);
        presentation.renderValue(renderer);

        assertEquals(1, renderer.getCounter());
    }


    @Override
    public void setFullValueEvaluator(@NotNull final XFullValueEvaluator fullValueEvaluator) {
        if (myFullValue == null) {
            throw new IllegalStateException("SetFullValueEvaluator shouldn't be called");
        }

        myEval++;

        final MockXFullValueEvaluationCallback callback = new MockXFullValueEvaluationCallback(myFullValue);
        fullValueEvaluator.startEvaluation(callback);

        assertEquals(1, callback.getCounter());
    }


    public int getPres() {
        return myPres;
    }


    public int getEval() {
        return myEval;
    }
}
