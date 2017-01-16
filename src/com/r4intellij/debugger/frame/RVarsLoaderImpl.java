package com.r4intellij.debugger.frame;

import com.intellij.openapi.util.text.StringUtil;
import com.r4intellij.debugger.RDebuggerUtils;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.exception.RUnexpectedExecutionResultTypeException;
import com.r4intellij.debugger.executor.RExecutionResult;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

import static com.r4intellij.debugger.RDebuggerUtils.calculateRepresentation;
import static com.r4intellij.debugger.RDebuggerUtils.calculateValueCommand;
import static com.r4intellij.debugger.data.RCommands.*;
import static com.r4intellij.debugger.data.RLanguageConstants.FUNCTION_TYPE;
import static com.r4intellij.debugger.executor.RExecutionResultType.DEBUG_AT;
import static com.r4intellij.debugger.executor.RExecutionResultType.RESPONSE;
import static com.r4intellij.debugger.executor.RExecutorUtils.execute;

// TODO [dbg][upd_test]
class RVarsLoaderImpl implements RVarsLoader {

    @NotNull
    private final RExecutor myExecutor;

    @NotNull
    private final ROutputReceiver myReceiver;

    @NotNull
    private final RValueModifier myModifier;

    private final int myFrameNumber;


    public RVarsLoaderImpl(@NotNull final RExecutor executor,
                           @NotNull final ROutputReceiver receiver,
                           @NotNull final RValueModifier modifier,
                           final int frameNumber) {
        myExecutor = executor;
        myReceiver = receiver;
        myModifier = modifier;
        myFrameNumber = frameNumber;
    }


    @NotNull
    @Override
    public List<RVar> load() throws RDebuggerException {
        final String text = execute(
                myExecutor,
                lsCommand(myFrameNumber),
                RESPONSE,
                myReceiver
        );

        final List<RVar> vars = new ArrayList<RVar>();

        for (final String variableName : calculateVariableNames(text)) {
            final RVar var = loadVar(variableName);

            if (var != null) {
                vars.add(var);
            }
        }

        return vars;
    }


    @NotNull
    private List<String> calculateVariableNames(@NotNull final String text) {
        final List<String> result = new ArrayList<String>();

        for (final String line : StringUtil.splitByLines(text)) {
            for (final String token : StringUtil.tokenize(new StringTokenizer(line))) {
                final String var = getVariableName(token);

                if (var != null) {
                    result.add(var);
                }
            }
        }

        return result;
    }


    @Nullable
    private RVar loadVar(@NotNull final String var) throws RDebuggerException {
        final String type = execute(
                myExecutor,
                typeOfCommand(expressionOnFrameCommand(myFrameNumber, var)),
                RESPONSE,
                myReceiver
        );

        if (type.equals(FUNCTION_TYPE) && RDebuggerUtils.isServiceName(var)) {
            return null;
        }

        return new RVar(
                var,
                type,
                loadValue(var, type),
                myModifier
        );
    }


    @Nullable
    private String getVariableName(@NotNull final String token) {
        final boolean isNotEmptyQuotedString = StringUtil.isQuotedString(token) && token.length() > 2;

        if (isNotEmptyQuotedString) {
            return token.substring(1, token.length() - 1);
        } else {
            return null;
        }
    }


    @NotNull
    private String loadValue(@NotNull final String var,
                             @NotNull final String type) throws RDebuggerException {
        final RExecutionResult result = execute(myExecutor, calculateValueCommand(myFrameNumber, var), myReceiver);

        switch (result.getType()) {
            case RESPONSE:
                return calculateRepresentation(
                        type,
                        result.getOutput()
                );
            case DEBUG_AT:
                return calculateRepresentation(
                        type,
                        execute(
                                myExecutor,
                                EXECUTE_AND_STEP_COMMAND,
                                RESPONSE,
                                myReceiver
                        )
                );
            default:
                throw new RUnexpectedExecutionResultTypeException(
                        "Actual type is not the same as expected: " +
                                "[" +
                                "actual: " + result.getType() + ", " +
                                "expected: " +
                                "[" + RESPONSE + ", " + DEBUG_AT + "]" +
                                "]"
                );
        }
    }
}
