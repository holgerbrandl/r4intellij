package com.r4intellij.run.debug.mock;

import com.intellij.icons.AllIcons;
import com.intellij.xdebugger.frame.XFullValueEvaluator;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;

public class MockXVarNode extends IllegalXValueNode {

    @NotNull
    private final String myType;

    @NotNull
    private final String myShortValue;

    @Nullable
    private final String myFullValue;

    private int myPres;
    private int myEval;


    public MockXVarNode(@NotNull final String type, @NotNull final String shortValue, @Nullable final String fullValue) {
        myType = type;
        myShortValue = shortValue;
        myFullValue = fullValue;

        myPres = 0;
        myEval = 0;
    }


    @Override
    public void setPresentation(@Nullable final Icon icon,
                                @Nullable final String type,
                                @NotNull final String value,
                                final boolean hasChildren) {
        myPres++;

        assertEquals(AllIcons.Debugger.Value, icon);
        assertEquals(myType, type);
        assertEquals(myShortValue, value);
        assertFalse(hasChildren);
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
