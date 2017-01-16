package com.r4intellij.run.run;

import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutionResultCalculator;
import com.r4intellij.debugger.executor.RExecutionResultType;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.RDebuggerStringUtils.*;
import static com.r4intellij.debugger.data.RResponseConstants.PROMPT;

public class RRunExecutionResultCalculator implements RExecutionResultCalculator {

    @Override
    public boolean isComplete(@NotNull final CharSequence output) {
        final int promptIndex = output.length() - PROMPT.length();

        return StringUtil.endsWith(output, PROMPT) &&
                promptIndex > 0 &&
                StringUtil.isLineBreak(output.charAt(promptIndex - 1));
    }


    @NotNull
    @Override
    public RExecutionResult calculate(@NotNull final CharSequence output, @NotNull final String error) {
        final String result = calculateResult(output);

        return new RExecutionResult(
                result,
                RExecutionResultType.RESPONSE,
                TextRange.allOf(result),
                error
        );
    }


    @NotNull
    private String calculateResult(@NotNull final CharSequence output) {
        final int leftBound = findNextLineBegin(output, 0);
        final int rightBound = findLastButOneLineEnd(output, findLastLineBegin(output));

        if (leftBound >= rightBound) {
            return "";
        } else {
            return output.subSequence(leftBound, rightBound).toString();
        }
    }
}
