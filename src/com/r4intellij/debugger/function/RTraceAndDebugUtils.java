package com.r4intellij.debugger.function;

import com.r4intellij.debugger.RDebuggerUtils;
import com.r4intellij.debugger.ROutputReceiver;
import com.r4intellij.debugger.exception.RDebuggerException;
import com.r4intellij.debugger.executor.RExecutor;
import org.jetbrains.annotations.NotNull;

import static com.r4intellij.debugger.RDebuggerStringUtils.findCurrentLineEnd;
import static com.r4intellij.debugger.RDebuggerStringUtils.findNextLineBegin;
import static com.r4intellij.debugger.data.RCommands.*;
import static com.r4intellij.debugger.data.RFunctionConstants.*;
import static com.r4intellij.debugger.data.RLanguageConstants.CLOSURE;
import static com.r4intellij.debugger.executor.RExecutionResultType.EMPTY;
import static com.r4intellij.debugger.executor.RExecutionResultType.RESPONSE;
import static com.r4intellij.debugger.executor.RExecutorUtils.execute;

public final class RTraceAndDebugUtils {

    @NotNull
    private static final String LS_FUNCTIONS_COMMAND = filterCommand(
            "function(x) x == \"" + CLOSURE + "\"",
            eapplyCommand(ENVIRONMENT_COMMAND, TYPEOF_FUNCTION)
    );


    public static void traceAndDebugFunctions(@NotNull final RExecutor executor, @NotNull final ROutputReceiver receiver)
            throws RDebuggerException {
        final String output = execute(
                executor,
                LS_FUNCTIONS_COMMAND,
                RESPONSE,
                receiver
        );

        int index = 0;

        while (output.startsWith("$", index)) {
            final int currentLineEnd = findCurrentLineEnd(output, index + 2);

            traceAndDebugFunction(
                    executor,
                    receiver,
                    output.substring(
                            index + 1,
                            currentLineEnd
                    )
            );

            index = findNextLineBegin(output, findNextLineBegin(output, index));
        }
    }


    private static void traceAndDebugFunction(@NotNull final RExecutor executor,
                                              @NotNull final ROutputReceiver receiver,
                                              @NotNull final String functionName) throws RDebuggerException {
        if (RDebuggerUtils.isServiceName(functionName) && !functionName.equals(MAIN_FUNCTION_NAME)) {
            return;
        }

        execute(executor, enterFunction(functionName), EMPTY, receiver);
        execute(executor, traceCommand(functionName, enterFunctionName(functionName)), RESPONSE);
        execute(executor, debugCommand(functionName), EMPTY, receiver);
    }


    @NotNull
    private static String enterFunction(@NotNull final String functionName) {
        return enterFunctionName(functionName) + " <- function() { print(\"" + functionName + "\") }";
    }


    @NotNull
    private static String enterFunctionName(@NotNull final String functionName) {
        return SERVICE_FUNCTION_PREFIX + functionName + SERVICE_ENTER_FUNCTION_SUFFIX;
    }
}
