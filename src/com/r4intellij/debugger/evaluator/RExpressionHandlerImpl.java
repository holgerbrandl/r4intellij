package com.r4intellij.debugger.evaluator;

import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.debugger.RDebuggerUtils;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.data.RCommands.expressionOnFrameCommand;

public class RExpressionHandlerImpl implements RExpressionHandler {

    private int myLastFrameNumber = 0;


    @NotNull
    @Override
    public String handle(final int frameNumber, @NotNull final String expression) {
        if (StringUtil.isJavaIdentifier(expression)) {
            return RDebuggerUtils.calculateValueCommand(frameNumber, expression);
        }

        if (frameNumber == myLastFrameNumber) {
            return expression;
        } else {
            return expressionOnFrameCommand(frameNumber, expression);
        }
    }


    @Override
    public void setLastFrameNumber(final int lastFrameNumber) {
        myLastFrameNumber = lastFrameNumber;
    }
}
