package com.r4intellij.run.debug.mock;

import com.intellij.xdebugger.frame.XFullValueEvaluator;
import com.intellij.xdebugger.frame.XValueNode;
import com.intellij.xdebugger.frame.presentation.XValuePresentation;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class IllegalXValueNode implements XValueNode {

    @Override
    public void setPresentation(@Nullable final Icon icon,
                                @Nullable final String type,
                                @NotNull final String value,
                                final boolean hasChildren) {
        throw new IllegalStateException("SetPresentation shouldn't be called");
    }


    @Override
    public void setPresentation(@Nullable final Icon icon, @NotNull final XValuePresentation presentation, final boolean hasChildren) {
        throw new IllegalStateException("SetPresentation shouldn't be called");
    }


    @Override
    public void setPresentation(@Nullable final Icon icon,
                                @Nullable final String type,
                                @NotNull final String separator,
                                @Nullable final String value,
                                final boolean hasChildren) {
        throw new IllegalStateException("SetPresentation shouldn't be called");
    }


    @Override
    public void setFullValueEvaluator(@NotNull final XFullValueEvaluator fullValueEvaluator) {
        throw new IllegalStateException("SetFullValueEvaluator shouldn't be called");
    }


    @Override
    public boolean isObsolete() {
        throw new IllegalStateException("IsObsolete shouldn't be called");
    }
}
