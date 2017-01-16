package com.r4intellij.debugger;

import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutor;
import com.r4intellij.debugger.function.RFunctionDebuggerFactory;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.RDebuggerStringUtils.findLastButOneLineEnd;
import static com.r4intellij.debugger.RDebuggerStringUtils.findLastLineBegin;
import static com.r4intellij.debugger.data.RCommands.*;
import static com.r4intellij.debugger.data.RFunctionConstants.SERVICE_FUNCTION_PREFIX;
import static com.r4intellij.debugger.data.RLanguageConstants.CLOSURE;
import static com.r4intellij.debugger.data.RLanguageConstants.FUNCTION_TYPE;
import static com.r4intellij.debugger.data.RResponseConstants.ENVIRONMENT_PREFIX;

public final class RDebuggerUtils {

    @NotNull
    public static String forciblyEvaluateFunction(@NotNull final RExecutor executor,
                                                  @NotNull final RFunctionDebuggerFactory factory,
                                                  @NotNull final ROutputReceiver receiver) throws RDebuggerException {
        final RForcedFunctionDebuggerHandler handler = new RForcedFunctionDebuggerHandler(executor, factory, receiver);

        //noinspection StatementWithEmptyBody
        while (handler.advance()) {
        }

        return handler.getResult();
    }


    @NotNull
    public static String calculateRepresentation(@NotNull final String value) {
        final int lastLineBegin = findLastLineBegin(value);

        if (value.startsWith(ENVIRONMENT_PREFIX, lastLineBegin)) {
            return value.substring(
                    0,
                    findLastButOneLineEnd(value, lastLineBegin)
            );
        } else {
            return value;
        }
    }


    @NotNull
    public static String calculateRepresentation(@NotNull final String type, @NotNull final String value) {
        if (type.equals(FUNCTION_TYPE)) {
            return calculateRepresentation(value);
        } else {
            return value;
        }
    }


    @NotNull
    public static String calculateValueCommand(final int frameNumber, @NotNull final String var) {
        final String globalVar = expressionOnFrameCommand(frameNumber, var);

        final String isFunction = typeOfCommand(globalVar) + " == \"" + CLOSURE + "\"";
        final String isDebugged = isDebuggedCommand(globalVar);

        return "if (" + isFunction + " && " + isDebugged + ") " + attrCommand(globalVar, "original") + " else " + globalVar;
    }


    public static boolean isServiceName(@NotNull final String name) {
        return name.startsWith(SERVICE_FUNCTION_PREFIX);
    }
}
